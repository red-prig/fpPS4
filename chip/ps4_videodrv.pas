unit ps4_videodrv;

{$mode objfpc}{$H+}

{/$define ww}
{/$define null_rt}

interface

uses
  Classes, SysUtils,
  bittype,

  ps4_libSceVideoOut,
  ps4_pssl,
  ps4_shader,
  pm4defs,

  //ps4_Tiling,

  vulkan,
  vDevice,
  vMemory,
  vShader,
  vPipeline,
  vImage,
  vRender,

  si_ci_vi_merged_offset,
  si_ci_vi_merged_enum,
  si_ci_vi_merged_registers

  ;

procedure vSubmitCommandBuffers(
          count:DWORD;
          dcbGpuAddrs:PPointer;
          dcbSizesInBytes:PDWORD;
          ccbGpuAddrs:PPointer;
          ccbSizesInBytes:PDWORD;
          Flip:PqcFlipInfo);

procedure vSubmitDone;

implementation

Uses
 ps4_libSceGnmDriver,
 ps4_gpu_regs,
 shader_dump;

Var
 GPU_REGS:TGPU_REGS;

procedure onPrepareFlip();
begin
 //
end;

procedure onPrepareFlipLabel(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4PrepareFlip);
var
 adr:PDWORD;
begin
 QWORD(adr):=QWORD(Body^.ADDRES_LO) or (QWORD(Body^.ADDRES_HI) shl $20);
 {$ifdef ww}Writeln('adr:',HexStr(adr),' data:',Body^.DATA);{$endif}
 adr^:=Body^.DATA;
end;

procedure onPrepareFlipWithEopInterrupt(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4PrepareFlipWithEopInterrupt);
begin
 {$ifdef ww}writeln;{$endif}
end;

procedure onPrepareFlipWithEopInterruptLabel(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4PrepareFlipWithEopInterrupt);
var
 adr:PDWORD;
begin
 QWORD(adr):=QWORD(Body^.ADDRES_LO) or (QWORD(Body^.ADDRES_HI) shl $20);
 {$ifdef ww}Writeln('adr:',HexStr(adr),' data:',Body^.DATA);{$endif}
 adr^:=Body^.DATA;
end;

const
 kEventWriteSource32BitsImmediate     =$1; ///< Source is a 32-bit constant value provided as a separate function argument.
 kEventWriteSource64BitsImmediate     =$2; ///< Source is a 64-bit constant value provided as a separate function argument.
 kEventWriteSourceGlobalClockCounter  =$3; ///< Source is a 64-bit timestamp from the system’s 100Mhz global clock.
 kEventWriteSourceGpuCoreClockCounter =$4; ///< Source is a 64-bit timestamp from the GPU’s 800Mhz clock.

 // EVENT_WRITE_EOP packet definitions
 EVENTWRITEEOP_DATA_SEL_DISCARD            =0;
 EVENTWRITEEOP_DATA_SEL_SEND_DATA32        =1;
 EVENTWRITEEOP_DATA_SEL_SEND_DATA64        =2;
 EVENTWRITEEOP_DATA_SEL_SEND_GPU_CLOCK     =3;

 EVENTWRITEEOP_INT_SEL_NONE                =0;
 EVENTWRITEEOP_INT_SEL_SEND_INT            =1;
 EVENTWRITEEOP_INT_SEL_SEND_INT_ON_CONFIRM =2;
 EVENTWRITEEOP_INT_SEL_SEND_DATA_ON_CONFIRM=3;

procedure onEventWriteEop(pm4Hdr:PM4_TYPE_3_HEADER;Body:PEVENTWRITEEOP);
var
 adr:Pointer;
begin
 QWORD(adr):=QWORD(Body^.ADDRESS_LO) or (QWORD(Body^.DATA_CNTL.ADDRESS_HI) shl $20);
 Case Body^.DATA_CNTL.DATA_SEL of
  EVENTWRITEEOP_DATA_SEL_DISCARD:;//nop
  kEventWriteSource32BitsImmediate    :PDWORD(adr)^:=Body^.DATA_LO;
  kEventWriteSource64BitsImmediate    :PQWORD(adr)^:=PQWORD(@Body^.DATA_LO)^;
  kEventWriteSourceGlobalClockCounter ,
  kEventWriteSourceGpuCoreClockCounter:PQWORD(adr)^:=GetTickCount64*1000;
  else
   Assert(False);
 end;

 post_event_eop;
end;

const
 //kEosCsDone = $0000002f; ///< Causes the SQ to generate a signal to indicate that all CS work prior to this point has completed.
 //kEosPsDone = $00000030; ///< Causes the SQ to generate a signal to indicate that all PS work prior to this point has completed.

 EVENT_WRITE_EOS_INDEX_CSDONE_PSDONE=6;

 EVENT_WRITE_EOS_CMD_STORE_APPEND_COUNT_TO_MEMORY=0;
 EVENT_WRITE_EOS_CMD_STORE_GDS_DATA_TO_MEMORY    =1;
 EVENT_WRITE_EOS_CMD_STORE_32BIT_DATA_TO_MEMORY  =2;

procedure onEventWriteEos(pm4Hdr:PM4_TYPE_3_HEADER;Body:PTPM4CMDEVENTWRITEEOS);
var
 adr:PDWORD;
begin
 Case Body^.eventIndex of
  EVENT_WRITE_EOS_INDEX_CSDONE_PSDONE:
  begin
   Case Body^.eventType of
    CS_DONE:{$ifdef ww}Writeln('kEosCsDone'){$endif};
    PS_DONE:{$ifdef ww}Writeln('kEosPsDone'){$endif};
    else
     Assert(False);
   end;
   Case Body^.command of
    //EVENT_WRITE_EOS_CMD_STORE_APPEND_COUNT_TO_MEMORY:;
    //EVENT_WRITE_EOS_CMD_STORE_GDS_DATA_TO_MEMORY    :;
    EVENT_WRITE_EOS_CMD_STORE_32BIT_DATA_TO_MEMORY  :
    begin
     QWORD(adr):=QWORD(Body^.addressLo) or (QWORD(Body^.addressHi) shl $20);
     {$ifdef ww}Writeln('adr:',HexStr(adr),' data:',Body^.DATA){$endif};
     adr^:=Body^.DATA;
    end;
    else
     Assert(False);
   end;
  end;
  else
   Assert(False);
 end;
 //writeln;
end;

procedure onEventWrite(pm4Hdr:PM4_TYPE_3_HEADER;Body:PTPM4CMDEVENTWRITE);
begin
 Case Body^.eventType of
  THREAD_TRACE_MARKER:;
  FLUSH_AND_INV_CB_META:
   begin
    Case Body^.EVENTINDEX of
     %0000:{$ifdef ww}Writeln('Any non-Time Stamp/non-Fence/non-Trap EVENT_TYPE not listed.'){$endif};
     %0001:{$ifdef ww}Writeln('ZPASS_DONE'){$endif};
     %0010:{$ifdef ww}Writeln('SAMPLE_PIPELINESTATS'){$endif};
     %0011:{$ifdef ww}Writeln('SAMPLE_STREAMOUTSTAT[S|S1|S2|S3]'){$endif};
     %0100:{$ifdef ww}Writeln('[CS|VS|PS]_PARTIAL_FLUSH'){$endif};
     %0101:{$ifdef ww}Writeln('Reserved for EVENT_WRITE_EOP time stamp/fence event types'){$endif};
     %0110:{$ifdef ww}Writeln('Reserved for EVENT_WRITE_EOS packet'){$endif};
     %0111:{$ifdef ww}Writeln('CACHE_FLUSH, CACHE_FLUSH_AND_INV_EVENT'){$endif};
     else
      Assert(False);
    end;
   end;
  else
   Assert(False);
 end;
end;

const
//DmaDataSrc
 kDmaDataSrcMemory	        = $0; ///< Source is a GPU-visible memory address.
 kDmaDataSrcGds	                = $1; ///< Source is an offset into Global Data Store (GDS).
 kDmaDataSrcData	        = $2; ///< Source is a 32-bit data constant.
 kDmaDataSrcMemoryUsingL2       = $3; ///< Source is a GPU-visible memory address, but should be read directly from the L2 cache.
 kDmaDataSrcRegister	        = $4; ///< Source is a GPU register offset (auto-increment enabled for multi-register DMAs).
 kDmaDataSrcRegisterNoIncrement = $C; ///< Source is a GPU register offset (auto-increment disabled for multi-register DMAs).

const
//DmaDataDst
 kDmaDataDstMemory	        = $0; ///< Destination is a GPU-visible memory address.
 kDmaDataDstGds	                = $1; ///< Destination is an offset into Global Data Store (GDS).
 kDmaDataDstRegister	        = $4; ///< Destination is a GPU register offset (auto-increment enabled for multi-register DMAs).
 kDmaDataDstRegisterNoIncrement = $C; ///< Destination is a GPU register offset (auto-increment disabled for multi-register DMAs).

{
2 = 0010
3 = 0011
4 = 0100
8 = 1000
C = 1100
}

procedure onDMAData(pm4Hdr:PM4_TYPE_3_HEADER;Body:PTPM4DMADATA);
var
 adrSrc,adrDst:PDWORD;
 srcSel,dstSel:DWORD;
begin
 srcSel:=((PDWORD(Body)[0] shr $1d) and 3) or ((PDWORD(Body)[5] shr $19) and 8) or ((PDWORD(Body)[5] shr $18) and 4);
 dstSel:=((PDWORD(Body)[0] shr $14) and 1) or ((PDWORD(Body)[5] shr $1a) and 8) or ((PDWORD(Body)[5] shr $19) and 4);

 QWORD(adrSrc):=QWORD(Body^.srcAddrLo) or (QWORD(Body^.srcAddrHi) shl $20);
 QWORD(adrDst):=QWORD(Body^.dstAddrLo) or (QWORD(Body^.dstAddrHi) shl $20);

 case srcSel of
  kDmaDataSrcMemory,
  kDmaDataSrcMemoryUsingL2:
   begin
    case dstSel of
     kDmaDataDstMemory:
      begin
       Move(adrSrc^,adrDst^,Body^.Flags2.byteCount);
      end;
     kDmaDataDstRegister,
     kDmaDataDstRegisterNoIncrement:
     begin
      if (Body^.dstAddrLo=$3022C) then
      begin
       {$ifdef ww}Writeln('prefetchIntoL2:',HexStr(adrSrc),' count(DW):',Body^.Flags2.byteCount div 4){$endif};
      end else
      begin
       {$ifdef ww}Writeln('SetRegister:',HexStr(Body^.dstAddrLo shr 2,4),' count(DW):',Body^.Flags2.byteCount div 4){$endif};
      end;
     end;
     else
      Assert(False);
    end;
   end;
  kDmaDataSrcData:
   begin
    case dstSel of
     kDmaDataDstMemory:
      begin
       FillDWORD(adrDst^,Body^.Flags2.byteCount div 4,Body^.srcAddrLo);
      end;
     kDmaDataDstRegister,
     kDmaDataDstRegisterNoIncrement:
      {$ifdef ww}Writeln('SetRegister:',HexStr(Body^.dstAddrLo shr 2,4),' count(DW):1'){$endif};
     else
      Assert(False);
    end;
   end;
  else
   Assert(False);
 end;
end;

// WRITE_DATA DST_SEL and ENGINE definitions
const
 WRITE_DATA_DST_SEL_REGISTER    =0;
 WRITE_DATA_DST_SEL_MEMORY_SYNC =1;
 WRITE_DATA_DST_SEL_TCL2        =2;
 WRITE_DATA_DST_SEL_GDS         =3;
 WRITE_DATA_DST_SEL_MEMORY_ASYNC=5;

 WRITE_DATA_CACHE_POLICY_LRU    =0;
 WRITE_DATA_CACHE_POLICY_STREAM =1;
 WRITE_DATA_CACHE_POLICY_BYPASS =2;

 WRITE_DATA_ENGINE_ME           =0;
 WRITE_DATA_ENGINE_PFP          =1;
 WRITE_DATA_ENGINE_CE           =2;

procedure onWriteData(pm4Hdr:PM4_TYPE_3_HEADER;Body:PTPM4CMDWRITEDATA);
var
 adr:PDWORD;
 i,count:Word;
begin

 Assert(Body^.CONTROL.wrOneAddr=0);

 Case Body^.CONTROL.engineSel of
  WRITE_DATA_ENGINE_ME:;
  else
   Assert(False);
 end;

 Case Body^.CONTROL.dstSel of
  WRITE_DATA_DST_SEL_MEMORY_SYNC,
  WRITE_DATA_DST_SEL_MEMORY_ASYNC:
    begin
     count:=pm4Hdr.count;
     if (count>=3) then
     begin
      count:=count-2;
      QWORD(adr):=QWORD(Body^.dstAddrLo) or (QWORD(Body^.dstAddrHi) shl $20);
      {$ifdef ww}Writeln('adr:',HexStr(adr),' data:',PDWORD(@Body^.DATA)^){$endif};
      Move(Body^.DATA,adr^,count*SizeOf(DWORD));
     end;
    end;
  else
   Assert(False);
 end;

end;

procedure onAcquireMem(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4ACQUIREMEM);
begin
 {$ifdef ww}writeln;{$endif}
end;

procedure onPm40(pm4Hdr:PM4_TYPE_0_HEADER;Body:PDWORD);
begin
 {$ifdef ww}Writeln('PM4_TYPE_0:Reg:',HexStr(pm4Hdr.baseIndex,4),' count(DW):',pm4Hdr.count+1);{$endif}
end;

procedure onNop(pm4Hdr:PM4_TYPE_3_HEADER;Body:PDWORD);
begin
 case Body^ of

  {$ifdef ww}
  OP_HINT_1920_1080                                :Writeln('\HINT_1920_1080                                ');
  OP_HINT_1860_1080                                :Writeln('\HINT_1860_1080                                ');
  OP_HINT_320_240                                  :Writeln('\HINT_320_240                                  ');

  OP_HINT_WRITE_GPU_PREFETCH_INTO_L2               :Writeln('\HINT_WRITE_GPU_PREFETCH_INTO_L2               ');
  OP_HINT_BASE_ALLOCATE_FROM_COMMAND_BUFFER        :Writeln('\HINT_BASE_ALLOCATE_FROM_COMMAND_BUFFER        ');
  OP_HINT_PUSH_MARKER                              :Writeln('\HINT_PUSH_MARKER                              ');
  OP_HINT_POP_MARKER                               :Writeln('\HINT_POP_MARKER                               ');
  OP_HINT_SET_VSHARP_IN_USER_DATA                  :Writeln('\HINT_SET_VSHARP_IN_USER_DATA                  ');
  OP_HINT_SET_TSHARP_IN_USER_DATA                  :Writeln('\HINT_SET_TSHARP_IN_USER_DATA                  ');
  OP_HINT_SET_SSHARP_IN_USER_DATA                  :Writeln('\HINT_SET_SSHARP_IN_USER_DATA                  ');
  OP_HINT_SET_USER_DATA_REGION                     :Writeln('\HINT_SET_USER_DATA_REGION                     ');
  OP_HINT_BASE_MARK_DISPATCH_DRAW_ACB_ADDRESS      :Writeln('\HINT_BASE_MARK_DISPATCH_DRAW_ACB_ADDRESS      ');
  OP_HINT_INLINE_DATA1                             :Writeln('\HINT_INLINE_DATA1                             ');
  OP_HINT_INLINE_DATA2                             :Writeln('\HINT_INLINE_DATA2                             ');

  OP_HINT_SET_DB_RENDER_CONTROL                    :Writeln('\HINT_SET_DB_RENDER_CONTROL                    ');
  OP_HINT_SET_DB_COUNT_CONTROL                     :Writeln('\HINT_SET_DB_COUNT_CONTROL                     ');
  OP_HINT_SET_RENDER_OVERRIDE_CONTROL              :Writeln('\HINT_SET_RENDER_OVERRIDE_CONTROL              ');
  OP_HINT_SET_RENDER_OVERRIDE2CONTROL              :Writeln('\HINT_SET_RENDER_OVERRIDE2CONTROL              ');
  OP_HINT_SET_PS_SHADER_SAMPLE_EXCLUSION_MASK      :Writeln('\HINT_SET_PS_SHADER_SAMPLE_EXCLUSION_MASK      ');
  OP_HINT_SET_DEPTH_BOUNDS_RANGE                   :Writeln('\HINT_SET_DEPTH_BOUNDS_RANGE                   ');
  OP_HINT_SET_STENCIL_CLEAR_VALUE                  :Writeln('\HINT_SET_STENCIL_CLEAR_VALUE                  ');
  OP_HINT_SET_DEPTH_CLEAR_VALUE                    :Writeln('\HINT_SET_DEPTH_CLEAR_VALUE                    ');
  OP_HINT_SET_SCREEN_SCISSOR                       :Writeln('\HINT_SET_SCREEN_SCISSOR                       ');
  OP_HINT_SET_DEPTH_RENDER_TARGET                  :Writeln('\HINT_SET_DEPTH_RENDER_TARGET                  ');
  OP_HINT_SET_BORDER_COLOR_TABLE_ADDR              :Writeln('\HINT_SET_BORDER_COLOR_TABLE_ADDR              ');
  OP_HINT_SET_WINDOW_OFFSET                        :Writeln('\HINT_SET_WINDOW_OFFSET                        ');
  OP_HINT_SET_WINDOW_SCISSOR                       :Writeln('\HINT_SET_WINDOW_SCISSOR                       ');
  OP_HINT_SET_CLIP_RECTANGLE_RULE                  :Writeln('\HINT_SET_CLIP_RECTANGLE_RULE                  ');
  OP_HINT_SET_HARDWARE_SCREEN_OFFSET               :Writeln('\HINT_SET_HARDWARE_SCREEN_OFFSET               ');
  OP_HINT_SET_RENDER_TARGET_MASK                   :Writeln('\HINT_SET_RENDER_TARGET_MASK                   ');
  OP_HINT_SET_GENERIC_SCISSOR                      :Writeln('\HINT_SET_GENERIC_SCISSOR                      ');
  OP_HINT_SET_PERFMON_ENABLE                       :Writeln('\HINT_SET_PERFMON_ENABLE                       ');
  OP_HINT_SET_SCALED_RESOLUTION_GRID               :Writeln('\HINT_SET_SCALED_RESOLUTION_GRID               ');
  OP_HINT_SET_FOVEATED_WINDOW                      :Writeln('\HINT_SET_FOVEATED_WINDOW                      ');
  OP_HINT_SET_INDEX_OFFSET                         :Writeln('\HINT_SET_INDEX_OFFSET                         ');
  OP_HINT_SET_PRIMITIVE_RESET_INDEX                :Writeln('\HINT_SET_PRIMITIVE_RESET_INDEX                ');
  OP_HINT_SET_STENCIL_OP_CONTROL                   :Writeln('\HINT_SET_STENCIL_OP_CONTROL                   ');
  OP_HINT_SET_STENCIL                              :Writeln('\HINT_SET_STENCIL                              ');
  OP_HINT_SET_PS_SHADER_USAGE                      :Writeln('\HINT_SET_PS_SHADER_USAGE                      ');
  OP_HINT_SET_GRAPHICS_SCRATCH_SIZE                :Writeln('\HINT_SET_GRAPHICS_SCRATCH_SIZE                ');
  OP_HINT_SET_DEPTH_STENCIL_CONTROL                :Writeln('\HINT_SET_DEPTH_STENCIL_CONTROL                ');
  OP_HINT_SET_DEPTH_EQAA_CONTROL                   :Writeln('\HINT_SET_DEPTH_EQAA_CONTROL                   ');
  OP_HINT_SET_CB_CONTROL                           :Writeln('\HINT_SET_CB_CONTROL                           ');
  OP_HINT_SET_CLIP_CONTROL                         :Writeln('\HINT_SET_CLIP_CONTROL                         ');
  OP_HINT_SET_PRIMITIVE_SETUP                      :Writeln('\HINT_SET_PRIMITIVE_SETUP                      ');
  OP_HINT_SET_VIEWPORT_TRANSFORM_CONTROL           :Writeln('\HINT_SET_VIEWPORT_TRANSFORM_CONTROL           ');
  OP_HINT_SET_OBJECT_ID_MODE                       :Writeln('\HINT_SET_OBJECT_ID_MODE                       ');
  OP_HINT_SET_COMPUTE_SHADER_CONTROL               :Writeln('\HINT_SET_COMPUTE_SHADER_CONTROL               ');
  OP_HINT_SET_COMPUTE_SCRATCH_SIZE                 :Writeln('\HINT_SET_COMPUTE_SCRATCH_SIZE                 ');
  OP_HINT_SET_PRIMITIVE_TYPE_BASE                  :Writeln('\HINT_SET_PRIMITIVE_TYPE_BASE                  ');
  OP_HINT_SET_POINT_SIZE                           :Writeln('\HINT_SET_POINT_SIZE                           ');
  OP_HINT_SET_POINT_MIN_MAX                        :Writeln('\HINT_SET_POINT_MIN_MAX                        ');
  OP_HINT_SET_LINE_WIDTH                           :Writeln('\HINT_SET_LINE_WIDTH                           ');
  OP_HINT_SET_GS_MODE                              :Writeln('\HINT_SET_GS_MODE                              ');
  OP_HINT_SET_GS_ON_CHIP_CONTROL                   :Writeln('\HINT_SET_GS_ON_CHIP_CONTROL                   ');
  OP_HINT_SET_SCAN_MODE_CONTROL                    :Writeln('\HINT_SET_SCAN_MODE_CONTROL                    ');
  OP_HINT_SET_PS_SHADER_RATE                       :Writeln('\HINT_SET_PS_SHADER_RATE                       ');
  OP_HINT_SET_PRIMITIVE_ID_ENABLE                  :Writeln('\HINT_SET_PRIMITIVE_ID_ENABLE                  ');
  OP_HINT_SET_PRIMITIVE_RESET_INDEX_ENABLE         :Writeln('\HINT_SET_PRIMITIVE_RESET_INDEX_ENABLE         ');
  OP_HINT_SET_DRAW_PAYLOAD_CONTROL                 :Writeln('\HINT_SET_DRAW_PAYLOAD_CONTROL                 ');
  OP_HINT_SET_INSTANCE_STEP_RATE                   :Writeln('\HINT_SET_INSTANCE_STEP_RATE                   ');
  OP_HINT_SETUP_ES_GS_RING_REGISTERS               :Writeln('\HINT_SETUP_ES_GS_RING_REGISTERS               ');
  OP_HINT_SET_VERTEX_REUSE_ENABLE                  :Writeln('\HINT_SET_VERTEX_REUSE_ENABLE                  ');
  OP_HINT_SET_HTILE_STENCIL0                       :Writeln('\HINT_SET_HTILE_STENCIL0                       ');
  OP_HINT_SET_HTILE_STENCIL1                       :Writeln('\HINT_SET_HTILE_STENCIL1                       ');
  OP_HINT_SETUP_DRAW_OPAQUE_PARAMETERS_1           :Writeln('\HINT_SETUP_DRAW_OPAQUE_PARAMETERS_1           ');
  OP_HINT_SETUP_DRAW_OPAQUE_PARAMETERS_0           :Writeln('\HINT_SETUP_DRAW_OPAQUE_PARAMETERS_0           ');
  OP_HINT_SET_TESSELLATION_DISTRIBUTION_THRESHOLDS :Writeln('\HINT_SET_TESSELLATION_DISTRIBUTION_THRESHOLDS ');
  OP_HINT_SET_ACTIVE_SHADER_STAGES                 :Writeln('\HINT_SET_ACTIVE_SHADER_STAGES                 ');
  OP_HINT_SETUP_GS_VS_RING_REGISTERS               :Writeln('\HINT_SETUP_GS_VS_RING_REGISTERS               ');
  OP_HINT_SET_ALPHA_TO_MASK_CONTROL                :Writeln('\HINT_SET_ALPHA_TO_MASK_CONTROL                ');
  OP_HINT_SET_DISPATCH_DRAW_INDEX_DEALLOCATION_MASK:Writeln('\HINT_SET_DISPATCH_DRAW_INDEX_DEALLOCATION_MASK');
  OP_HINT_SET_POLYGON_OFFSET_Z_FORMAT              :Writeln('\HINT_SET_POLYGON_OFFSET_Z_FORMAT              ');
  OP_HINT_SET_POLYGON_OFFSET_CLAMP                 :Writeln('\HINT_SET_POLYGON_OFFSET_CLAMP                 ');
  OP_HINT_SET_POLYGON_OFFSET_FRONT                 :Writeln('\HINT_SET_POLYGON_OFFSET_FRONT                 ');
  OP_HINT_SET_POLYGON_OFFSET_BACK                  :Writeln('\HINT_SET_POLYGON_OFFSET_BACK                  ');
  OP_HINT_SET_GS_MODE_DISABLE                      :Writeln('\HINT_SET_GS_MODE_DISABLE                      ');
  OP_HINT_SET_STREAMOUT_MAPPING                    :Writeln('\HINT_SET_STREAMOUT_MAPPING                    ');
  OP_HINT_SET_AA_SAMPLE_COUNT                      :Writeln('\HINT_SET_AA_SAMPLE_COUNT                      ');
  OP_HINT_SET_VERTEX_QUANTIZATION                  :Writeln('\HINT_SET_VERTEX_QUANTIZATION                  ');
  OP_HINT_SET_GUARD_BANDS                          :Writeln('\HINT_SET_GUARD_BANDS                          ');
  OP_HINT_SET_AA_SAMPLE_MASK1                      :Writeln('\HINT_SET_AA_SAMPLE_MASK1                      ');
  OP_HINT_SET_AA_SAMPLE_MASK2                      :Writeln('\HINT_SET_AA_SAMPLE_MASK2                      ');
  OP_HINT_SET_TEXTURE_GRADIENT_FACTORS             :Writeln('\HINT_SET_TEXTURE_GRADIENT_FACTORS             ');
  OP_HINT_SET_PERF_COUNTER_CONTROL_PA              :Writeln('\HINT_SET_PERF_COUNTER_CONTROL_PA              ');
  OP_HINT_SET_PRIMITIVE_TYPE_NEO                   :Writeln('\HINT_SET_PRIMITIVE_TYPE_NEO                   ');
  {$endif}

  OP_HINT_PREPARE_FLIP_VOID:
  begin
   onPrepareFlip();
   {$ifdef ww}Writeln('\HINT_PREPARE_FLIP_VOID');{$endif}
  end;
  OP_HINT_PREPARE_FLIP_LABEL:
  begin
   {$ifdef ww}Writeln('\HINT_PREPARE_FLIP_LABEL');{$endif}
   onPrepareFlipLabel(pm4Hdr,@Body[1]);
  end;
  OP_HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_VOID:
  begin
   {$ifdef ww}Writeln('\HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_VOID');{$endif}
   onPrepareFlipWithEopInterrupt(pm4Hdr,@Body[1]);
  end;
  OP_HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_LABEL:
  begin
   {$ifdef ww}Writeln('\HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_LABEL');{$endif}
   onPrepareFlipWithEopInterruptLabel(pm4Hdr,@Body[1]);
  end;
  {$ifdef ww}else
    Writeln('\Hint:',HexStr(Body^,8));{$endif}
 end;
end;

procedure onContextControl(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDCONTEXTCONTROL);
begin
 writeln;
end;

//The purpose of the Clear_State packet is to reduce command buffer preamble setup time for all driver versions of
//both DX and OpenGL and to specifically support DX11’s Display Lists requirements. The definition of Clear State
//is essentially everything off, resources all NULL, other values set to a defined default state.

procedure onClearState(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDCLEARSTATE);
begin
 GPU_REGS.Clear;
end;

const
 CONTEXT_REG_BASE = $A000;
 CONTEXT_SPACE_START=$0000a000;

procedure onSetContextReg(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 c:=pm4Hdr.count;
 if c<>0 then
 For i:=0 to c-1 do
 begin
  r:=CONTEXT_REG_BASE+Body^.REG_OFFSET+i;
  v:=PDWORD(@Body^.REG_DATA)[i];

  //{$ifdef ww}Writeln('SetContextReg:',getRegName(r),'=',HexStr(v,8));{$endif}
  //Continue;

  Case r of
   mmCB_COLOR0_BASE..mmCB_COLOR7_DCC_BASE:
   begin
    PDWORD(@GPU_REGS.RENDER_TARGET)[r-mmCB_COLOR0_BASE]:=v;
   end;
   mmCB_TARGET_MASK  :DWORD(GPU_REGS.TARGET_MASK)     :=v;
   mmCB_COLOR_CONTROL:DWORD(GPU_REGS.CB_COLOR_CONTROL):=v;

   mmCB_BLEND0_CONTROL..mmCB_BLEND7_CONTROL:
   begin
    PDWORD(@GPU_REGS.CB_BLEND_CONTROL)[r-mmCB_BLEND0_CONTROL]:=v;
   end;

   mmCB_SHADER_MASK  :DWORD(GPU_REGS.SPI.PS.SHADER_MASK):=v;

   mmPA_SC_MODE_CNTL_0:DWORD(GPU_REGS.SC_MODE_CNTL_0) :=v;
   mmPA_SC_MODE_CNTL_1:DWORD(GPU_REGS.SC_MODE_CNTL_1) :=v;

   mmPA_SC_VPORT_SCISSOR_0_TL..mmPA_SC_VPORT_SCISSOR_15_BR:
   begin
    PDWORD(@GPU_REGS.VPORT_SCISSOR)[r-mmPA_SC_VPORT_SCISSOR_0_TL]:=v;
   end;

   mmPA_SC_VPORT_ZMIN_0..mmPA_SC_VPORT_ZMAX_15:
   begin
    PDWORD(@GPU_REGS.VPORT_ZMIN_MAX)[r-mmPA_SC_VPORT_ZMIN_0]:=v;
   end;

   mmPA_CL_VPORT_XSCALE..mmPA_CL_VPORT_ZOFFSET_15:
   begin
    PDWORD(@GPU_REGS.VPORT_SCALE_OFFSET)[r-mmPA_CL_VPORT_XSCALE]:=v;
   end;

   mmPA_CL_VTE_CNTL:DWORD(GPU_REGS.VTE_CNTL):=v;

   mmPA_SC_SCREEN_SCISSOR_TL:DWORD(GPU_REGS.SCREEN_SCISSOR_TL):=v;
   mmPA_SC_SCREEN_SCISSOR_BR:DWORD(GPU_REGS.SCREEN_SCISSOR_BR):=v;

   mmPA_SC_AA_MASK_X0Y0_X1Y0:DWORD(GPU_REGS.SC_AA_MASK_X0Y0_X1Y0):=v;
   mmPA_SC_AA_MASK_X0Y1_X1Y1:DWORD(GPU_REGS.SC_AA_MASK_X0Y1_X1Y1):=v;
   mmPA_SC_AA_CONFIG        :DWORD(GPU_REGS.SC_AA_CONFIG):=v;

   mmPA_SU_HARDWARE_SCREEN_OFFSET:DWORD(GPU_REGS.HARDWARE_SCREEN_OFFSET):=v;

   mmPA_SU_VTX_CNTL:DWORD(GPU_REGS.VTX_CNTL):=v;

   mmPA_SU_LINE_CNTL:DWORD(GPU_REGS.SU_LINE_CNTL)      :=v;
   mmPA_SU_POINT_SIZE:DWORD(GPU_REGS.SU_POINT_SIZE)    :=v;
   mmPA_SU_POINT_MINMAX:DWORD(GPU_REGS.SU_POINT_MINMAX):=v;

   mmPA_CL_CLIP_CNTL:DWORD(GPU_REGS.CL_CLIP_CNTL)        :=v;
   mmPA_SC_CLIPRECT_RULE:DWORD(GPU_REGS.SC_CLIPRECT_RULE):=v;

   mmPA_CL_GB_VERT_CLIP_ADJ:PDWORD(@GPU_REGS.GB_CLIP.VERT_CLIP_ADJ)^:=v;
   mmPA_CL_GB_VERT_DISC_ADJ:PDWORD(@GPU_REGS.GB_CLIP.VERT_DISC_ADJ)^:=v;
   mmPA_CL_GB_HORZ_CLIP_ADJ:PDWORD(@GPU_REGS.GB_CLIP.HORZ_CLIP_ADJ)^:=v;
   mmPA_CL_GB_HORZ_DISC_ADJ:PDWORD(@GPU_REGS.GB_CLIP.HORZ_DISC_ADJ)^:=v;

   mmSPI_VS_OUT_CONFIG    :DWORD(GPU_REGS.SPI.VS.OUT_CONFIG):=v;
   mmPA_CL_VS_OUT_CNTL    :DWORD(GPU_REGS.SPI.VS.OUT_CNTL):=v;

   mmSPI_SHADER_POS_FORMAT:DWORD(GPU_REGS.SPI.VS.POS_FORMAT):=v;
   mmSPI_SHADER_Z_FORMAT  :DWORD(GPU_REGS.SPI.PS.Z_FORMAT)  :=v;
   mmSPI_SHADER_COL_FORMAT:DWORD(GPU_REGS.SPI.PS.COL_FORMAT):=v;
   mmSPI_BARYC_CNTL       :DWORD(GPU_REGS.SPI.PS.BARYC_CNTL):=v;

   mmSPI_PS_INPUT_ENA     :DWORD(GPU_REGS.SPI.PS.INPUT_ENA) :=v;
   mmSPI_PS_INPUT_ADDR    :DWORD(GPU_REGS.SPI.PS.INPUT_ADDR):=v;
   mmSPI_PS_IN_CONTROL    :DWORD(GPU_REGS.SPI.PS.IN_CONTROL):=v;

   mmSPI_PS_INPUT_CNTL_0  :DWORD(GPU_REGS.SPI.PS.INPUT_CNTL_0):=v;
   mmSPI_PS_INPUT_CNTL_1  :DWORD(GPU_REGS.SPI.PS.INPUT_CNTL_1):=v;

   mmDB_SHADER_CONTROL    :DWORD(GPU_REGS.SPI.PS.SHADER_CONTROL):=v;

   mmDB_RENDER_CONTROL    :DWORD(GPU_REGS.DEPTH.RENDER_CONTROL):=v;
   mmDB_DEPTH_CONTROL     :DWORD(GPU_REGS.DEPTH.DEPTH_CONTROL):=v;

   mmDB_DEPTH_VIEW        :DWORD(GPU_REGS.DEPTH.DEPTH_VIEW        ):=v;
   mmDB_HTILE_DATA_BASE   :DWORD(GPU_REGS.DEPTH.HTILE_DATA_BASE   ):=v;
   mmDB_DEPTH_BOUNDS_MIN  :DWORD(GPU_REGS.DEPTH.DEPTH_BOUNDS_MIN  ):=v;
   mmDB_DEPTH_BOUNDS_MAX  :DWORD(GPU_REGS.DEPTH.DEPTH_BOUNDS_MAX  ):=v;
   mmDB_STENCIL_CLEAR     :DWORD(GPU_REGS.DEPTH.STENCIL_CLEAR     ):=v;
   mmDB_DEPTH_CLEAR       :DWORD(GPU_REGS.DEPTH.DEPTH_CLEAR       ):=v;

   mmDB_DEPTH_INFO        :DWORD(GPU_REGS.DEPTH.DEPTH_INFO        ):=v;
   mmDB_Z_INFO            :DWORD(GPU_REGS.DEPTH.Z_INFO            ):=v;
   mmDB_STENCIL_INFO      :DWORD(GPU_REGS.DEPTH.STENCIL_INFO      ):=v;
   mmDB_Z_READ_BASE       :DWORD(GPU_REGS.DEPTH.Z_READ_BASE       ):=v;
   mmDB_STENCIL_READ_BASE :DWORD(GPU_REGS.DEPTH.STENCIL_READ_BASE ):=v;
   mmDB_Z_WRITE_BASE      :DWORD(GPU_REGS.DEPTH.Z_WRITE_BASE      ):=v;
   mmDB_STENCIL_WRITE_BASE:DWORD(GPU_REGS.DEPTH.STENCIL_WRITE_BASE):=v;
   mmDB_DEPTH_SIZE        :DWORD(GPU_REGS.DEPTH.DEPTH_SIZE        ):=v;
   mmDB_DEPTH_SLICE       :DWORD(GPU_REGS.DEPTH.DEPTH_SLICE       ):=v;

   mmDB_HTILE_SURFACE     :DWORD(GPU_REGS.DEPTH.HTILE_SURFACE     ):=v;

   mmVGT_SHADER_STAGES_EN :DWORD(GPU_REGS.VGT_SHADER_STAGES_EN) :=v;
   mmVGT_OUT_DEALLOC_CNTL :DWORD(GPU_REGS.VGT_OUT_DEALLOC_CNTL) :=v;

   mmVGT_VTX_CNT_EN       :DWORD(GPU_REGS.VGT_VTX_INDX.CNT_EN):=v;

   mmVGT_MIN_VTX_INDX     :DWORD(GPU_REGS.VGT_VTX_INDX.MIN_INDX):=v;
   mmVGT_MAX_VTX_INDX     :DWORD(GPU_REGS.VGT_VTX_INDX.MAX_INDX):=v;

   mmVGT_INDX_OFFSET      :DWORD(GPU_REGS.VGT_VTX_INDX.INDX_OFFSET):=v;

   mmVGT_MULTI_PRIM_IB_RESET_INDX:DWORD(GPU_REGS.VGT_MULTI_PRIM_IB_RESET_INDX):=v;

   mmVGT_OUTPUT_PATH_CNTL:DWORD(GPU_REGS.VGT_OUTPUT_PATH_CNTL):=v;

   //mmVGT_GS_MODE:v:=v;

   mmPA_SU_POLY_OFFSET_DB_FMT_CNTL:DWORD(GPU_REGS.PA_SU_POLY_OFFSET_DB_FMT_CNTL):=v;

   {$ifdef ww}else
     Writeln('SetContextReg:',getRegName(r),'=',HexStr(v,8));{$endif}
  end;

 end;
end;

const
 PERSISTENT_SPACE_START=$00002c00;

procedure onSetShReg(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 c:=pm4Hdr.count;
 if c<>0 then
 For i:=0 to c-1 do
 begin
  r:=PERSISTENT_SPACE_START+Body^.REG_OFFSET+i;
  v:=PDWORD(@Body^.REG_DATA)[i];

  //{$ifdef ww}Writeln('SetShReg:',getRegName(r),'=',HexStr(v,8));{$endif}
  //Continue;

  Case r of

   mmSPI_SHADER_PGM_LO_PS   :GPU_REGS.SPI.PS.LO:=v;
   mmSPI_SHADER_PGM_HI_PS   :GPU_REGS.SPI.PS.HI:=v;
   mmSPI_SHADER_PGM_RSRC1_PS:DWORD(GPU_REGS.SPI.PS.RSRC1):=v;
   mmSPI_SHADER_PGM_RSRC2_PS:DWORD(GPU_REGS.SPI.PS.RSRC2):=v;
   mmSPI_SHADER_PGM_RSRC3_PS:DWORD(GPU_REGS.SPI.PS.RSRC3):=v;

   mmSPI_SHADER_USER_DATA_PS_0..mmSPI_SHADER_USER_DATA_PS_15:
    PDWORD(@GPU_REGS.SPI.PS.USER_DATA)[r-mmSPI_SHADER_USER_DATA_PS_0]:=v;

   mmSPI_SHADER_PGM_LO_VS   :GPU_REGS.SPI.VS.LO:=v;
   mmSPI_SHADER_PGM_HI_VS   :GPU_REGS.SPI.VS.HI:=v;
   mmSPI_SHADER_PGM_RSRC1_VS:DWORD(GPU_REGS.SPI.VS.RSRC1):=v;
   mmSPI_SHADER_PGM_RSRC2_VS:DWORD(GPU_REGS.SPI.VS.RSRC2):=v;
   mmSPI_SHADER_PGM_RSRC3_VS:DWORD(GPU_REGS.SPI.VS.RSRC3):=v;

   mmSPI_SHADER_USER_DATA_VS_0..mmSPI_SHADER_USER_DATA_VS_15:
    PDWORD(@GPU_REGS.SPI.VS.USER_DATA)[r-mmSPI_SHADER_USER_DATA_VS_0]:=v;

   mmSPI_SHADER_LATE_ALLOC_VS:DWORD(GPU_REGS.SPI.VS.LATE_ALLOC):=v;

   //mmSPI_SHADER_PGM_RSRC3_GS:v:=v;
   //mmSPI_SHADER_PGM_RSRC3_ES:v:=v;
   //mmSPI_SHADER_PGM_RSRC3_HS:v:=v;
   //mmSPI_SHADER_PGM_RSRC3_LS:v:=v;

   mmCOMPUTE_PGM_LO         :GPU_REGS.SPI.CS.LO:=v;
   mmCOMPUTE_PGM_HI         :GPU_REGS.SPI.CS.HI:=v;
   mmCOMPUTE_PGM_RSRC1      :DWORD(GPU_REGS.SPI.CS.RSRC1):=v;
   mmCOMPUTE_PGM_RSRC2      :DWORD(GPU_REGS.SPI.CS.RSRC2):=v;

   mmCOMPUTE_NUM_THREAD_X   :DWORD(GPU_REGS.SPI.CS.NUM_THREAD_X):=v;
   mmCOMPUTE_NUM_THREAD_Y   :DWORD(GPU_REGS.SPI.CS.NUM_THREAD_Y):=v;
   mmCOMPUTE_NUM_THREAD_Z   :DWORD(GPU_REGS.SPI.CS.NUM_THREAD_Z):=v;

   mmCOMPUTE_USER_DATA_0..mmCOMPUTE_USER_DATA_15:
    PDWORD(@GPU_REGS.SPI.CS.USER_DATA)[r-mmCOMPUTE_USER_DATA_0]:=v;

   mmCOMPUTE_STATIC_THREAD_MGMT_SE0:DWORD(GPU_REGS.SPI.CS.STATIC_THREAD_MGMT_SE0):=v;
   mmCOMPUTE_STATIC_THREAD_MGMT_SE1:DWORD(GPU_REGS.SPI.CS.STATIC_THREAD_MGMT_SE1):=v;
   mmCOMPUTE_RESOURCE_LIMITS       :DWORD(GPU_REGS.SPI.CS.RESOURCE_LIMITS):=v;

   {$ifdef ww}else
    Writeln('SetShReg:',getRegName(r),'=',HexStr(v,8));{$endif}
  end;

 end;
end;

type
 PVGT_PRIMITIVE_TYPE=^TVGT_PRIMITIVE_TYPE;
 PGRBM_GFX_INDEX=^TGRBM_GFX_INDEX;

Const
 UCONFIG_SPACE_START=$0000c000;

procedure onSetUConfigReg(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;

begin
 //r:=Body^.REG_OFFSET+$C000;

 //mmVGT_PRIMITIVE_TYPE__CI__VI                     = 0xC242;
 //mmVGT_INDEX_TYPE__CI__VI                         = 0xC243;
 //mmVGT_NUM_INSTANCES__CI__VI                      = 0xC24D;

 c:=pm4Hdr.count;
 if c<>0 then
 For i:=0 to c-1 do
 begin
  r:=UCONFIG_SPACE_START{ $C000}+Body^.REG_OFFSET+i;
  v:=PDWORD(@Body^.REG_DATA)[i];

  {$ifdef ww}Writeln('SetUConfigReg:',getRegName(r),'=',HexStr(v,8));{$endif}

  Case r of
   mmVGT_PRIMITIVE_TYPE:DWORD(GPU_REGS.VGT_PRIMITIVE_TYPE):=v;
   mmVGT_INDEX_TYPE    :DWORD(GPU_REGS.VGT_INDEX_TYPE    ):=v;
   mmVGT_NUM_INSTANCES :DWORD(GPU_REGS.VGT_NUM_INSTANCES ):=v;
   mmGRBM_GFX_INDEX:{$ifdef ww}Writeln('INSTANCE_INDEX:',PGRBM_GFX_INDEX(@v)^.INSTANCE_INDEX){$endif};
   {$ifdef ww}else
    Writeln('SetUConfigReg:',getRegName(r),'=',HexStr(v,8));{$endif}
  end;

 end;


end;

type
 PVGT_DMA_INDEX_TYPE=^TVGT_DMA_INDEX_TYPE;

procedure onIndexType(pm4Hdr:PM4_TYPE_3_HEADER;Body:PVGT_DMA_INDEX_TYPE);
begin
 GPU_REGS.VGT_DMA.INDEX_TYPE:=Body^;
 {$ifdef ww}
 Case Body^.INDEX_TYPE of
  VGT_INDEX_16:Write('VGT_INDEX_16');
  VGT_INDEX_32:Write('VGT_INDEX_32');
  VGT_INDEX_8 :Write('VGT_INDEX_8');
  else         Write('VGT_INDEX_UNKNOW');
 end;
 Writeln;
 {$endif}
end;

//SLICE.TILE_MAX number of tiles in a slice (equal to Pitch * Height / 64),

//PITCH.TILE_MAX = 159,   //(PITCH.TILE_MAX+1)*8=1280
//SLICE.TILE_MAX = 15359, //(SLICE.TILE_MAX+1)/(PITCH.TILE_MAX+1)*8=768

function getCodeAddress(lo,hi:DWORD):Pointer;
begin
 Result:=Pointer(((QWORD(hi) shl 40) or (QWORD(lo) shl 8)));
end;

function getFetchAddress(lo,hi:DWORD):Pointer;
begin
 Result:=Pointer(((QWORD(hi) shl 32) or (QWORD(lo) and (not 3))));
end;

function getIndexAddress(lo,hi:DWORD):Pointer;
begin
 Result:=Pointer(((Word(hi) shl 32) or (QWORD(lo) and (not 1))));
end;

var
 FCmdPool:TCmdPool;

 FCmdBuffer:TvCmdBuffer;

 FVSShader:TvShader;
 FPSShader:TvShader;

procedure UpdateGpuRegsInfo;


var
 i:DWORD;

 FRenderCmd:TvRenderTargets;

 RT_INFO:TRT_INFO;
 DB_INFO:TDB_INFO;
 ri:TUnionResourceImage;

begin

 {$ifdef null_rt}Exit;{$endif}

 DumpPS(GPU_REGS);
 DumpVS(GPU_REGS);

 {
 Writeln('paddedWidth[0]:',(GPU_REGS.RENDER_TARGET[0].PITCH.TILE_MAX+1)*8);
 Writeln('paddedHeigh[0]:',(GPU_REGS.RENDER_TARGET[0].SLICE.TILE_MAX+1)*8 div (GPU_REGS.RENDER_TARGET[0].PITCH.TILE_MAX+1));


 Writeln('ZMIN_ZMAX[0]:',GPU_REGS.VPORT_ZMIN_MAX[0].ZMIN:0:3,' ',GPU_REGS.VPORT_ZMIN_MAX[0].ZMAX:0:3);

 Writeln('VPORT_OFFSET[0]:',GPU_REGS.VPORT_SCALE_OFFSET[0].XOFFSET:0:3,':', //x=XOFFSET-XSCALE
                            GPU_REGS.VPORT_SCALE_OFFSET[0].YOFFSET:0:3,':', //y=YOFFSET-YSCALE
                            GPU_REGS.VPORT_SCALE_OFFSET[0].ZOFFSET:0:3);    //minDepth=ZOFFSET

 Writeln('VPORT_SCALE[0]:' ,GPU_REGS.VPORT_SCALE_OFFSET[0].XSCALE:0:3,':',  //width =XSCALE*2
                            GPU_REGS.VPORT_SCALE_OFFSET[0].YSCALE:0:3,':',  //height=YSCALE*2
                            GPU_REGS.VPORT_SCALE_OFFSET[0].ZSCALE:0:3);     //maxDepth=ZOFFSET+ZSCALE

 Writeln(
  GPU_REGS.VPORT_SCALE_OFFSET[0].XOFFSET-GPU_REGS.VPORT_SCALE_OFFSET[0].XSCALE:0:3,' ',
  GPU_REGS.VPORT_SCALE_OFFSET[0].YOFFSET-GPU_REGS.VPORT_SCALE_OFFSET[0].YSCALE:0:3,' ',
  GPU_REGS.VPORT_SCALE_OFFSET[0].XSCALE*2:0:3,' ',
  GPU_REGS.VPORT_SCALE_OFFSET[0].YSCALE*2:0:3,' ',
  GPU_REGS.VPORT_SCALE_OFFSET[0].ZOFFSET:0:3,' ',
  GPU_REGS.VPORT_SCALE_OFFSET[0].ZOFFSET+GPU_REGS.VPORT_SCALE_OFFSET[0].ZSCALE:0:3
 );
 }


 //Writeln((GPU_REGS.DEPTH.DEPTH_SIZE.PITCH_TILE_MAX+1)*8,'x',(GPU_REGS.DEPTH.DEPTH_SIZE.HEIGHT_TILE_MAX+1)*8);

 if not GPU_REGS.COMP_ENABLE then Exit;

 InitVulkan;
 if (FCmdPool=nil) then
 begin
  FCmdPool:=TCmdPool.Create(VulkanApp.FGFamily);
 end;

 if (FCmdBuffer=nil) then
 begin
  FCmdBuffer:=TvCmdBuffer.Create;
  FCmdBuffer.cmdbuf:=FCmdPool.Alloc;

  //FCmdBuffer.FWaitSemaphore:=TvSemaphore.Create;
  //FCmdBuffer.FSignSemaphore:=TvSemaphore.Create;
  FCmdBuffer.FSignFence:=TvFence.Create(true);
 end;

  FRenderCmd:=TvRenderTargets.Create;

  FRenderCmd.FRenderPass:=TvRenderPass.Create;
  FRenderCmd.FPipeline  :=TvGraphicsPipeline.Create;
  FRenderCmd.FPipeline.FLayout:=TvPipelineLayout.Create;
  FRenderCmd.FPipeline.FRenderPass:=FRenderCmd.FRenderPass;

  FRenderCmd.FFramebuffer:=TvFramebuffer.Create;
  FRenderCmd.FFramebuffer.SetRenderPass(FRenderCmd.FRenderPass);

 FRenderCmd.FFramebuffer.FreeImageViews;
 FRenderCmd.FRenderPass.Clear;
 FRenderCmd.FPipeline.Clear;

 FRenderCmd.FFramebuffer.SetSize(GPU_REGS.GET_SCREEN_SIZE);

 FRenderCmd.FPipeline.SetPrimType(GPU_REGS.GET_PRIM_TYPE);

 FRenderCmd.FRenderArea:=GPU_REGS.GET_SCREEN;

 For i:=0 to 15 do
  if GPU_REGS.VP_ENABLE(i) then
  begin
   FRenderCmd.FPipeline.AddVPort(GPU_REGS.GET_VPORT(i),GPU_REGS.GET_SCISSOR(i));
  end;

 FCmdBuffer.BeginCmdBuffer;

 if GPU_REGS.COMP_ENABLE then
 For i:=0 to 7 do
 if GPU_REGS.RT_ENABLE(i) then
  begin
   RT_INFO:=GPU_REGS.GET_RT_INFO(i);

   ri:=FetchUnionImage2D(RT_INFO.Addr,
                         RT_INFO.cformat,
                         RT_INFO.extend,
                         ord(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or
                         ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
                         ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT)
                         );

   FRenderCmd.FFramebuffer.AddImageView(ri.FImage.NewView);

   FRenderCmd.FRenderPass.AddColorRef(FRenderCmd.FRenderPass.subpass.colorAttachmentCount);
   FRenderCmd.FRenderPass.AddColorAt(RT_INFO.cformat,RT_INFO.FAST_CLEAR,True);
   FRenderCmd.FPipeline.AddBlend(RT_INFO.blend);

   if RT_INFO.FAST_CLEAR then
   begin
    FRenderCmd.AddClearColor(TVkClearValue(RT_INFO.CLEAR_COLOR));
   end;

  end;

 if false{GPU_REGS.DB_ENABLE} then
 begin
  DB_INFO:=GPU_REGS.GET_DB_INFO;

  ri:=FetchUnionImage2D(DB_INFO.Z_READ_ADDR,
                        DB_INFO.dformat,
                        DB_INFO.extend,
                        ord(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT)
                        );

  FRenderCmd.FFramebuffer.AddImageView(ri.FImage.NewView);

  FRenderCmd.FRenderPass.SetDepthStencilRef(FRenderCmd.FRenderPass.subpass.colorAttachmentCount);

  FRenderCmd.FRenderPass.AddDepthAt(
   DB_INFO.dformat,
   DB_INFO.DEPTH_CLEAR,
   not DB_INFO.Z_READ_ONLY,
   DB_INFO.STENCIL_CLEAR,
   not DB_INFO.STENCIL_READ_ONLY);

  FRenderCmd.FRenderPass.SetZorderStage(DB_INFO.zorder_stage);

  if DB_INFO.DEPTH_CLEAR or DB_INFO.STENCIL_CLEAR then
  begin
   FRenderCmd.AddClearColor(DB_INFO.CLEAR_VALUE);
  end;

  FRenderCmd.FPipeline.DepthStencil.depthTestEnable      :=DB_INFO.depthTestEnable      ;
  FRenderCmd.FPipeline.DepthStencil.depthWriteEnable     :=DB_INFO.depthWriteEnable     ;
  FRenderCmd.FPipeline.DepthStencil.depthCompareOp       :=DB_INFO.depthCompareOp       ;
  FRenderCmd.FPipeline.DepthStencil.depthBoundsTestEnable:=DB_INFO.depthBoundsTestEnable;
  FRenderCmd.FPipeline.DepthStencil.stencilTestEnable    :=DB_INFO.stencilTestEnable    ;
  FRenderCmd.FPipeline.DepthStencil.front                :=DB_INFO.front                ;
  FRenderCmd.FPipeline.DepthStencil.back                 :=DB_INFO.back                 ;
  FRenderCmd.FPipeline.DepthStencil.minDepthBounds       :=DB_INFO.minDepthBounds       ;
  FRenderCmd.FPipeline.DepthStencil.maxDepthBounds       :=DB_INFO.maxDepthBounds       ;

 end;

 if (FVSShader=nil) then
 begin
  FVSShader:=TvShader.Create;
  FVSShader.LoadFromFile('spirv\vs_78EF9008.spv');
 end;

 if (FPSShader=nil) then
 begin
  FPSShader:=TvShader.Create;
  FPSShader.LoadFromFile('spirv\ps_FBCA196D.spv');
 end;

 FRenderCmd.FPipeline.SetVSShader(FVSShader);
 FRenderCmd.FPipeline.SetPSShader(FPSShader);

 if not FCmdBuffer.BeginRenderPass(FRenderCmd) then
  Writeln('!BeginRenderPass');

end;

procedure UpdateGpuRegsInfoCompute;
begin

 {$ifdef null_rt}Exit;{$endif}

 DumpCS(GPU_REGS);

 //
end;

procedure onDrawIndex2(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDDRAWINDEX2);
var
 Addr:Pointer;

begin
 GPU_REGS.VGT_DMA.MAX_SIZE:=Body^.maxSize;
 GPU_REGS.VGT_DMA.BASE_LO :=Body^.indexBaseLo;
 GPU_REGS.VGT_DMA.BASE_HI :=Body^.indexBaseHi;
 GPU_REGS.VGT_DMA.SIZE    :=Body^.indexCount;
 GPU_REGS.VGT_DMA.INDICES :=Body^.indexCount;

  //drawInitiator:TVGT_DRAW_INITIATOR;

 UpdateGpuRegsInfo;

 Addr:=getIndexAddress(GPU_REGS.VGT_DMA.BASE_LO,GPU_REGS.VGT_DMA.BASE_HI);

 FCmdBuffer.DrawIndex2(Addr,GPU_REGS.VGT_DMA.INDICES,GPU_REGS.GET_INDEX_TYPE);

 {$ifdef ww}Writeln('DrawIndex:',Body^.indexCount);{$endif}
end;

procedure onDrawIndexAuto(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDDRAWINDEXAUTO);
begin
 GPU_REGS.VGT_DMA.INDICES:=Body^.indexCount;

 UpdateGpuRegsInfo;

 FCmdBuffer.DrawIndexAuto(GPU_REGS.VGT_DMA.INDICES,GPU_REGS.GET_INDEX_TYPE);

 {$ifdef ww}Writeln('onDrawIndexAuto:',Body^.indexCount);{$endif}
end;

procedure onDispatchDirect(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDDISPATCHDIRECT);
begin

 UpdateGpuRegsInfoCompute;

 {$ifdef ww}Writeln('onDispatchDirect:',Body^.dimX,':',Body^.dimY,':',Body^.dimZ);{$endif}
end;

type
 PVGT_DMA_NUM_INSTANCES=^TVGT_DMA_NUM_INSTANCES;

procedure onNumInstances(pm4Hdr:PM4_TYPE_3_HEADER;Body:PVGT_DMA_NUM_INSTANCES);
begin
 GPU_REGS.VGT_DMA.NUM_INSTANCES:=Body^;
 {$ifdef ww}Writeln('onNumInstances:',Body^);{$endif}
end;

procedure vSubmitDone;
begin
 GPU_REGS.ClearDMA;
end;

procedure vSubmitCommandBuffers(
          count:DWORD;
          dcbGpuAddrs:PPointer;
          dcbSizesInBytes:PDWORD;
          ccbGpuAddrs:PPointer;
          ccbSizesInBytes:PDWORD;
          Flip:PqcFlipInfo);
var
 n,i,s:DWORD;
 token:DWORD;
 P:PByte;

begin
 Assert((ccbSizesInBytes=nil) or ((ccbSizesInBytes<>nil) and (ccbSizesInBytes^=0)));
 n:=0;
 While (n<count) do
 begin
  i:=0;
  s:=dcbSizesInBytes[n];
  P:=PByte(dcbGpuAddrs[n]);
  While (i<s) do
  begin
   token:=PDWORD(P)^;

   case PM4_TYPE(token) of
    0:onPm40(PM4_TYPE_0_HEADER(token),@PDWORD(P)[1]);
    3:case PM4_TYPE_3_HEADER(token).opcode of
       IT_NOP:onNop(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       IT_EVENT_WRITE_EOP              :
       begin
        {$ifdef ww}Writeln('IT_EVENT_WRITE_EOP');{$endif}
        onEventWriteEop(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;
       IT_EVENT_WRITE_EOS              :
       begin
        {$ifdef ww}Writeln('IT_EVENT_WRITE_EOS');{$endif}
        onEventWriteEos(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;
       IT_DMA_DATA                     :
       begin
        {$ifdef ww}Writeln('IT_DMA_DATA');{$endif}
        onDMAData(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;
       IT_ACQUIRE_MEM:
       begin
        {$ifdef ww}Writeln('IT_ACQUIRE_MEM');{$endif}
        onAcquireMem(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;
       IT_CONTEXT_CONTROL:
       begin
        {$ifdef ww}Writeln('IT_CONTEXT_CONTROL');{$endif}
        onContextControl(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;
       IT_CLEAR_STATE:
       begin
        {$ifdef ww}Writeln('IT_CLEAR_STATE');{$endif}
        onClearState(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;
       IT_SET_CONTEXT_REG:
       begin
        {$ifdef ww}Writeln('IT_SET_CONTEXT_REG');{$endif}
        onSetContextReg(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;
       IT_SET_SH_REG:
       begin
        {$ifdef ww}Writeln('IT_SET_SH_REG');{$endif}
        onSetShReg(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;
       IT_SET_UCONFIG_REG:
       begin
        {$ifdef ww}Writeln('IT_SET_UCONFIG_REG');{$endif}
        onSetUConfigReg(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;
       IT_INDEX_TYPE:
       begin
        {$ifdef ww}Writeln('IT_INDEX_TYPE');{$endif}
        onIndexType(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;
       IT_DRAW_INDEX_2:
       begin
        {$ifdef ww}Writeln('IT_DRAW_INDEX_2');{$endif}
        onDrawIndex2(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;
       IT_DRAW_INDEX_AUTO:
       begin
        {$ifdef ww}Writeln('IT_DRAW_INDEX_AUTO');{$endif}
        onDrawIndexAuto(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;
       IT_DISPATCH_DIRECT:
       begin
        {$ifdef ww}Writeln('IT_DISPATCH_DIRECT');{$endif}
        onDispatchDirect(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;

       IT_NUM_INSTANCES:
       begin
        {$ifdef ww}Writeln('IT_NUM_INSTANCES');{$endif}
        onNumInstances(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;

       IT_WAIT_REG_MEM:
       begin
        {$ifdef ww}Writeln('IT_WAIT_REG_MEM');{$endif}
        //(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;

       IT_WRITE_DATA:
       begin
        {$ifdef ww}Writeln('IT_WRITE_DATA');{$endif}
        onWriteData(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        //(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;

       IT_EVENT_WRITE:
       begin
        {$ifdef ww}Writeln('IT_EVENT_WRITE'){$endif};
        onEventWrite(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
       end;

       {$ifdef ww}else
        Writeln('PM4_TYPE_3.opcode:',HexStr(PM4_TYPE_3_HEADER(token).opcode,2));{$endif}
      end;

    else
     Assert(False);
    {
    {$ifdef ww}else
     Writeln('PM4_TYPE_',PM4_TYPE(token));{$endif}
     }
   end;


   P:=P+PM4_LENGTH_DW(token)*sizeof(DWORD);
   i:=i+PM4_LENGTH_DW(token)*sizeof(DWORD);
  end;
  Inc(n);
 end;

 FCmdBuffer.EndRenderPass;

 FCmdBuffer.BeginCmdBuffer;



 //need to moved submit_done
 FCmdBuffer.QueueSubmit;
 //FCmdBuffer.FSignFence.Wait(High(uint64));
 FCmdBuffer.ClearRenderList;

 vkQueueWaitIdle(RenderQueue);

 _qc_sceVideoOutSubmitFlip(Flip);
end;

initialization
 GPU_REGS.Clear;

end.

