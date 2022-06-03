unit ps4_videodrv;

{$mode objfpc}{$H+}

{/$define ww}
{/$define null_rt}

interface

uses
  Classes,
  SysUtils,
  bittype,

  sys_types,
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
  vCmdBuffer,

  vShaderExt,
  vShaderManager,
  vPipelineLayoutManager,
  vSetsPoolManager,
  vHostBufferManager,
  vImageManager,
  vSampler,
  vSamplerManager,

  si_ci_vi_merged_offset,
  si_ci_vi_merged_enum,
  si_ci_vi_merged_registers,

  trace_manager

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

 FCmdPool:TvCmdPool;

 FCmdBuffer:TvCmdBuffer;

 FLastSetReg:WORD;

 //FSubmitFlip:TqcFlipInfo;
 //PSubmitFlip:PqcFlipInfo;

 FFlipLabel:PDWORD;
 FFlipLData:DWORD;

procedure onPrepareFlip();
begin
 //

 vSubmitDone;

end;

procedure onPrepareFlipLabel(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4PrepareFlip);
var
 adr:PDWORD;
begin
 QWORD(adr):=QWORD(Body^.ADDRES_LO) or (QWORD(Body^.ADDRES_HI) shl $20);
 {$ifdef ww}Writeln('adr:',HexStr(adr),' data:',Body^.DATA);{$endif}
 //adr^:=Body^.DATA;

 Assert(FFlipLabel=nil);
 FFlipLabel:=adr;
 FFlipLData:=Body^.DATA;

 vSubmitDone;
end;

procedure onPrepareFlipWithEopInterrupt(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4PrepareFlipWithEopInterrupt);
begin
 {$ifdef ww}writeln;{$endif}

 vSubmitDone;

 post_event_eop;
end;

procedure onPrepareFlipWithEopInterruptLabel(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4PrepareFlipWithEopInterrupt);
var
 adr:PDWORD;
begin
 QWORD(adr):=QWORD(Body^.ADDRES_LO) or (QWORD(Body^.ADDRES_HI) shl $20);
 {$ifdef ww}Writeln('adr:',HexStr(adr),' data:',Body^.DATA);{$endif}
 //adr^:=Body^.DATA;

 Assert(FFlipLabel=nil);
 FFlipLabel:=adr;
 FFlipLData:=Body^.DATA;

 vSubmitDone;

 post_event_eop;
end;

procedure onEventWriteEop(pm4Hdr:PM4_TYPE_3_HEADER;Body:PEVENTWRITEEOP);
var
 adr:Pointer;
begin
 Assert(Body^.EVENT_CNTL.EVENT_INDEX=EVENT_WRITE_INDEX_ANY_EOP_TIMESTAMP);

 {$ifdef ww}
 Case Body^.EVENT_CNTL.EVENT_TYPE of
  kEopFlushCbDbCaches             :Writeln('kEopFlushCbDbCaches');
  kEopFlushAndInvalidateCbDbCaches:Writeln('kEopFlushAndInvalidateCbDbCaches');
  kEopCbDbReadsDone               :Writeln('kEopCbDbReadsDone');
  else
   Assert(false);
 end;
 {$endif}

 if ((Body^.DATA_CNTL.reserved6 and 1)<>0) then
 begin
  {$ifdef ww}Writeln('kEventWriteDestTcL2');{$endif}
 end else
 begin

  QWORD(adr):=QWORD(Body^.ADDRESS_LO) or (QWORD(Body^.DATA_CNTL.addressHi) shl $20);

  {$ifdef ww}
  Case Body^.DATA_CNTL.dataSel of
   kEventWriteSource32BitsImmediate    :Writeln('kEventWriteSource32BitsImmediate     adr:',HexStr(adr),' data:',Body^.DATA_LO);
   kEventWriteSource64BitsImmediate    :Writeln('kEventWriteSource64BitsImmediate     adr:',HexStr(adr),' data:',PQWORD(@Body^.DATA_LO)^);
   kEventWriteSourceGlobalClockCounter :Writeln('kEventWriteSourceGlobalClockCounter  adr:',HexStr(adr),' data:',GetTickCount64*1000);
   kEventWriteSourceGpuCoreClockCounter:Writeln('kEventWriteSourceGpuCoreClockCounter adr:',HexStr(adr),' data:',GetTickCount64*1000);
   else;
  end;
  {$endif}

  if (Body^.DATA_CNTL.intSel<>0) then
  begin
   {$ifdef ww}Writeln('Interrupt');{$endif}

   vSubmitDone;

   post_event_eop;
  end;


  Case Body^.DATA_CNTL.dataSel of
   EVENTWRITEEOP_DATA_SEL_DISCARD:;//nop
   kEventWriteSource32BitsImmediate    :PDWORD(adr)^:=Body^.DATA_LO;
   kEventWriteSource64BitsImmediate    :PQWORD(adr)^:=PQWORD(@Body^.DATA_LO)^;
   kEventWriteSourceGlobalClockCounter ,
   kEventWriteSourceGpuCoreClockCounter:PQWORD(adr)^:=GetTickCount64*1000;
   else
    Assert(False);
  end;

 end;

end;

procedure onEventWriteEos(pm4Hdr:PM4_TYPE_3_HEADER;Body:PTPM4CMDEVENTWRITEEOS);
var
 adr:PDWORD;
begin
 Assert(Body^.eventType=47);

 Case Body^.eventIndex of
  EVENT_WRITE_EOS_INDEX_CSDONE_PSDONE:
  begin
   {Case Body^.eventType of
    CS_DONE:{$ifdef ww}Writeln('kEosCsDone'){$endif};
    PS_DONE:{$ifdef ww}Writeln('kEosPsDone'){$endif};
    else
     Assert(False);
   end;}
   Case Body^.command of
    //EVENT_WRITE_EOS_CMD_STORE_APPEND_COUNT_TO_MEMORY:;
    //EVENT_WRITE_EOS_CMD_STORE_GDS_DATA_TO_MEMORY    :;
    EVENT_WRITE_EOS_CMD_STORE_32BIT_DATA_TO_MEMORY  :
    begin
     QWORD(adr):=QWORD(Body^.addressLo) or (QWORD(Body^.addressHi) shl $20);
     {$ifdef ww}Writeln('adr:',HexStr(adr),' data:',Body^.DATA){$endif};
     //adr^:=Body^.DATA;
     FCmdBuffer.writeAtEndOfShader(Body^.eventType,adr,Body^.DATA);
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
 {$ifdef ww}
 Case Body^.eventType of
  THREAD_TRACE_MARKER        :Writeln('THREAD_TRACE_MARKER');
  FLUSH_AND_INV_CB_PIXEL_DATA:Writeln('FLUSH_AND_INV_CB_PIXEL_DATA');
  FLUSH_AND_INV_CB_META      :Writeln('FLUSH_AND_INV_CB_META');
  else
   Assert(False,IntToStr(Body^.eventType));
 end;

 Case Body^.EVENTINDEX of
  EVENT_WRITE_INDEX_ANY_NON_TIMESTAMP    :Writeln('ANY_NON_TIMESTAMP');
  EVENT_WRITE_INDEX_ZPASS_DONE           :Writeln('ZPASS_DONE');
  EVENT_WRITE_INDEX_SAMPLE_PIPELINESTAT  :Writeln('SAMPLE_PIPELINESTATS');
  EVENT_WRITE_INDEX_SAMPLE_STREAMOUTSTATS:Writeln('SAMPLE_STREAMOUTSTAT[S|S1|S2|S3]');
  EVENT_WRITE_INDEX_VS_PS_PARTIAL_FLUSH  :Writeln('[CS|VS|PS]_PARTIAL_FLUSH');
  EVENT_WRITE_INDEX_ANY_EOP_TIMESTAMP    :Writeln('ANY_EOP_TIMESTAMP');
  EVENT_WRITE_INDEX_ANY_EOS_TIMESTAMP    :Writeln('ANY_EOS_TIMESTAMP');
  EVENT_WRITE_INDEX_CACHE_FLUSH_EVENT    :Writeln('CACHE_FLUSH, CACHE_FLUSH_AND_INV_EVENT');
  else
   Assert(False);
 end;
 {$endif}
end;

procedure onDMAData(pm4Hdr:PM4_TYPE_3_HEADER;Body:PTPM4DMADATA);
var
 adrSrc,adrDst:PDWORD;
 srcSel,dstSel:DWORD;
begin
 srcSel:=((PDWORD(Body)[0] shr $1d) and 3) or ((PDWORD(Body)[5] shr $19) and 8) or ((PDWORD(Body)[5] shr $18) and 4);
 dstSel:=((PDWORD(Body)[0] shr $14) and 1) or ((PDWORD(Body)[5] shr $1a) and 8) or ((PDWORD(Body)[5] shr $19) and 4);

 QWORD(adrSrc):=QWORD(Body^.srcAddrLo) or (QWORD(Body^.srcAddrHi) shl $20);
 QWORD(adrDst):=QWORD(Body^.dstAddrLo) or (QWORD(Body^.dstAddrHi) shl $20);

 //Flags1.cpSync  isBlocking

 case srcSel of
  kDmaDataSrcMemory,
  kDmaDataSrcMemoryUsingL2:
   begin
    case dstSel of
     kDmaDataDstMemory:
      begin

       Case Body^.Flags1.engine of
        CP_DMA_ENGINE_ME :FCmdBuffer.dmaData(adrSrc,adrDst,Body^.Flags2.byteCount,Boolean(Body^.Flags1.cpSync));
        CP_DMA_ENGINE_PFP:Move(adrSrc^,adrDst^,Body^.Flags2.byteCount);
       end;

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
       Assert(false,'TODO');
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

       Case Body^.Flags1.engine of
        CP_DMA_ENGINE_ME :FCmdBuffer.dmaData(Body^.srcAddrLo,adrDst,Body^.Flags2.byteCount,Boolean(Body^.Flags1.cpSync));
        CP_DMA_ENGINE_PFP:FillDWORD(adrDst^,Body^.Flags2.byteCount div 4,Body^.srcAddrLo);
       end;

      end;
     kDmaDataDstRegister,
     kDmaDataDstRegisterNoIncrement:
      {$ifdef ww}Writeln('SetRegister:',HexStr(Body^.dstAddrLo shr 2,4),' count(DW):1'){$endif};
     kDmaDataDstGds:
      {$ifdef ww}Writeln('SetGds'){$endif};
     else
      Assert(False);
    end;
   end;
  else
   Assert(False);
 end;
end;

procedure onWriteData(pm4Hdr:PM4_TYPE_3_HEADER;Body:PTPM4CMDWRITEDATA);
var
 adr:PDWORD;
 count:Word;
begin

 Assert(Body^.CONTROL.wrOneAddr=0);

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

      Case Body^.CONTROL.engineSel of
       WRITE_DATA_ENGINE_ME:
        begin
         FCmdBuffer.dmaData(@Body^.DATA,adr,count*SizeOf(DWORD),Boolean(Body^.CONTROL.wrConfirm));
        end;
       WRITE_DATA_ENGINE_PFP:
        begin
         Move(Body^.DATA,adr^,count*SizeOf(DWORD));
        end;
       else
        Assert(False);
      end;

     end;
    end;
  else
   Assert(False);
 end;

end;

//vkFlushMappedMemoryRanges analog
procedure onAcquireMem(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4ACQUIREMEM);
begin
 {$ifdef ww}writeln;{$endif}
end;

procedure onWaitRegMem(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDWAITREGMEM);
begin
  {$ifdef ww}
 Case Body^.engine of
  0:
    Case Body^.memSpace of
     0:Writeln('waitOnRegister');
     1:Writeln('waitOnAddress');
     else
      Assert(false);
    end;
  1:
    Case Body^.memSpace of
     0:Writeln('waitOnRegisterAndStall');
     1:Writeln('waitOnAddressAndStall');
     else
      Assert(false);
    end;
  else
   Assert(false);
 end;
 {$endif}
end;

procedure onPm40(pm4Hdr:PM4_TYPE_0_HEADER;Body:PDWORD);
begin
 {$ifdef ww}Writeln('PM4_TYPE_0:Reg:',HexStr(pm4Hdr.baseIndex,4),' count(DW):',pm4Hdr.count+1);{$endif}
end;

procedure onPushMarker(Body:PChar);
begin
 {$ifdef ww}Writeln('\HINT_PUSH_MARKER:',Body);{$endif}
end;

procedure onWidthHeight(Body:PWORD);
begin
 {$ifdef ww}Writeln('\HINT_',Body[0],'_',Body[1]);{$endif}
end;

procedure onNop(pm4Hdr:PM4_TYPE_3_HEADER;Body:PDWORD);
begin

 Case FLastSetReg of
  mmCB_COLOR0_FMASK_SLICE,
  mmCB_COLOR1_FMASK_SLICE,
  mmCB_COLOR2_FMASK_SLICE,
  mmCB_COLOR3_FMASK_SLICE,
  mmCB_COLOR4_FMASK_SLICE,
  mmCB_COLOR5_FMASK_SLICE,
  mmCB_COLOR6_FMASK_SLICE,
  mmCB_COLOR7_FMASK_SLICE,

  mmCB_COLOR0_DCC_BASE,
  mmCB_COLOR1_DCC_BASE,
  mmCB_COLOR2_DCC_BASE,
  mmCB_COLOR3_DCC_BASE,
  mmCB_COLOR4_DCC_BASE,
  mmCB_COLOR5_DCC_BASE,
  mmCB_COLOR6_DCC_BASE,
  mmCB_COLOR7_DCC_BASE,

  mmDB_HTILE_SURFACE:
   begin
    onWidthHeight(PWORD(Body));
    Exit;
   end;
 end;

 case Body^ of

  {$ifdef ww}

  OP_HINT_UPDATE_PS_DB_CONTROL                     :Writeln('\HINT_UPDATE_PS_DB_CONTROL');
  OP_HINT_UPDATE_VS_OUT_CNTL                       :Writeln('\HINT_UPDATE_VS_OUT_CNTL');
  OP_HINT_UPDATE_PS_FORMAT                         :Writeln('\HINT_UPDATE_PS_FORMAT');
  OP_HINT_UPDATE_PS_INPUT                          :Writeln('\HINT_UPDATE_PS_INPUT');
  OP_HINT_UPDATE_PS_IN_CONTROL                     :Writeln('\HINT_UPDATE_PS_IN_CONTROL');
  OP_HINT_UPDATE_VS_OUT_CONFIG                     :Writeln('\HINT_UPDATE_VS_OUT_CONFIG');
  OP_HINT_UPDATE_PS_RSRC                           :Writeln('\HINT_UPDATE_PS_RSRC');
  OP_HINT_UPDATE_PS_BARY_CNTL                      :Writeln('\HINT_UPDATE_PS_BARY_CNTL');
  OP_HINT_UPDATE_VS_RSRC                           :Writeln('\HINT_UPDATE_VS_RSRC');
  OP_HINT_UPDATE_VS_POS_FORMAT                     :Writeln('\HINT_UPDATE_VS_POS_FORMAT');

  OP_HINT_WRITE_GPU_PREFETCH_INTO_L2               :Writeln('\HINT_WRITE_GPU_PREFETCH_INTO_L2');
  OP_HINT_BASE_ALLOCATE_FROM_COMMAND_BUFFER        :Writeln('\HINT_BASE_ALLOCATE_FROM_COMMAND_BUFFER');
  OP_HINT_PUSH_MARKER                              ://Writeln('\HINT_PUSH_MARKER');
   onPushMarker(@Body[1]);

  OP_HINT_POP_MARKER                               :Writeln('\HINT_POP_MARKER');
  OP_HINT_SET_VSHARP_IN_USER_DATA                  :Writeln('\HINT_SET_VSHARP_IN_USER_DATA');
  OP_HINT_SET_TSHARP_IN_USER_DATA                  :Writeln('\HINT_SET_TSHARP_IN_USER_DATA');
  OP_HINT_SET_SSHARP_IN_USER_DATA                  :Writeln('\HINT_SET_SSHARP_IN_USER_DATA');
  OP_HINT_SET_USER_DATA_REGION                     :Writeln('\HINT_SET_USER_DATA_REGION');
  OP_HINT_BASE_MARK_DISPATCH_DRAW_ACB_ADDRESS      :Writeln('\HINT_BASE_MARK_DISPATCH_DRAW_ACB_ADDRESS');
  OP_HINT_INLINE_DATA1                             :Writeln('\HINT_INLINE_DATA1');
  OP_HINT_INLINE_DATA2                             :Writeln('\HINT_INLINE_DATA2');

  OP_HINT_SET_DB_RENDER_CONTROL                    :Writeln('\HINT_SET_DB_RENDER_CONTROL');
  OP_HINT_SET_DB_COUNT_CONTROL                     :Writeln('\HINT_SET_DB_COUNT_CONTROL');
  OP_HINT_SET_RENDER_OVERRIDE_CONTROL              :Writeln('\HINT_SET_RENDER_OVERRIDE_CONTROL');
  OP_HINT_SET_RENDER_OVERRIDE2CONTROL              :Writeln('\HINT_SET_RENDER_OVERRIDE2CONTROL');
  OP_HINT_SET_PS_SHADER_SAMPLE_EXCLUSION_MASK      :Writeln('\HINT_SET_PS_SHADER_SAMPLE_EXCLUSION_MASK');
  OP_HINT_SET_DEPTH_BOUNDS_RANGE                   :Writeln('\HINT_SET_DEPTH_BOUNDS_RANGE');
  OP_HINT_SET_STENCIL_CLEAR_VALUE                  :Writeln('\HINT_SET_STENCIL_CLEAR_VALUE');
  OP_HINT_SET_DEPTH_CLEAR_VALUE                    :Writeln('\HINT_SET_DEPTH_CLEAR_VALUE');
  OP_HINT_SET_SCREEN_SCISSOR                       :Writeln('\HINT_SET_SCREEN_SCISSOR');
  OP_HINT_SET_DEPTH_RENDER_TARGET                  :Writeln('\HINT_SET_DEPTH_RENDER_TARGET');
  OP_HINT_SET_BORDER_COLOR_TABLE_ADDR              :Writeln('\HINT_SET_BORDER_COLOR_TABLE_ADDR');
  OP_HINT_SET_WINDOW_OFFSET                        :Writeln('\HINT_SET_WINDOW_OFFSET');
  OP_HINT_SET_WINDOW_SCISSOR                       :Writeln('\HINT_SET_WINDOW_SCISSOR');
  OP_HINT_SET_CLIP_RECTANGLE_RULE                  :Writeln('\HINT_SET_CLIP_RECTANGLE_RULE');
  OP_HINT_SET_HARDWARE_SCREEN_OFFSET               :Writeln('\HINT_SET_HARDWARE_SCREEN_OFFSET');
  OP_HINT_SET_RENDER_TARGET_MASK                   :Writeln('\HINT_SET_RENDER_TARGET_MASK');
  OP_HINT_SET_GENERIC_SCISSOR                      :Writeln('\HINT_SET_GENERIC_SCISSOR');
  OP_HINT_SET_PERFMON_ENABLE                       :Writeln('\HINT_SET_PERFMON_ENABLE');
  OP_HINT_SET_SCALED_RESOLUTION_GRID               :Writeln('\HINT_SET_SCALED_RESOLUTION_GRID');
  OP_HINT_SET_FOVEATED_WINDOW                      :Writeln('\HINT_SET_FOVEATED_WINDOW');
  OP_HINT_SET_INDEX_OFFSET                         :Writeln('\HINT_SET_INDEX_OFFSET');
  OP_HINT_SET_PRIMITIVE_RESET_INDEX                :Writeln('\HINT_SET_PRIMITIVE_RESET_INDEX');
  OP_HINT_SET_STENCIL_OP_CONTROL                   :Writeln('\HINT_SET_STENCIL_OP_CONTROL');
  OP_HINT_SET_STENCIL                              :Writeln('\HINT_SET_STENCIL');
  OP_HINT_SET_PS_SHADER_USAGE                      :Writeln('\HINT_SET_PS_SHADER_USAGE');
  OP_HINT_SET_GRAPHICS_SCRATCH_SIZE                :Writeln('\HINT_SET_GRAPHICS_SCRATCH_SIZE');
  OP_HINT_SET_DEPTH_STENCIL_CONTROL                :Writeln('\HINT_SET_DEPTH_STENCIL_CONTROL');
  OP_HINT_SET_DEPTH_EQAA_CONTROL                   :Writeln('\HINT_SET_DEPTH_EQAA_CONTROL');
  OP_HINT_SET_CB_CONTROL                           :Writeln('\HINT_SET_CB_CONTROL');
  OP_HINT_SET_CLIP_CONTROL                         :Writeln('\HINT_SET_CLIP_CONTROL');
  OP_HINT_SET_PRIMITIVE_SETUP                      :Writeln('\HINT_SET_PRIMITIVE_SETUP');
  OP_HINT_SET_VIEWPORT_TRANSFORM_CONTROL           :Writeln('\HINT_SET_VIEWPORT_TRANSFORM_CONTROL');
  OP_HINT_SET_OBJECT_ID_MODE                       :Writeln('\HINT_SET_OBJECT_ID_MODE');
  OP_HINT_SET_COMPUTE_SHADER_CONTROL               :Writeln('\HINT_SET_COMPUTE_SHADER_CONTROL');
  OP_HINT_SET_COMPUTE_SCRATCH_SIZE                 :Writeln('\HINT_SET_COMPUTE_SCRATCH_SIZE');
  OP_HINT_SET_PRIMITIVE_TYPE_BASE                  :Writeln('\HINT_SET_PRIMITIVE_TYPE_BASE');
  OP_HINT_SET_POINT_SIZE                           :Writeln('\HINT_SET_POINT_SIZE');
  OP_HINT_SET_POINT_MIN_MAX                        :Writeln('\HINT_SET_POINT_MIN_MAX');
  OP_HINT_SET_LINE_WIDTH                           :Writeln('\HINT_SET_LINE_WIDTH');
  OP_HINT_SET_GS_MODE                              :Writeln('\HINT_SET_GS_MODE');
  OP_HINT_SET_GS_ON_CHIP_CONTROL                   :Writeln('\HINT_SET_GS_ON_CHIP_CONTROL');
  OP_HINT_SET_SCAN_MODE_CONTROL                    :Writeln('\HINT_SET_SCAN_MODE_CONTROL');
  OP_HINT_SET_PS_SHADER_RATE                       :Writeln('\HINT_SET_PS_SHADER_RATE');
  OP_HINT_SET_PRIMITIVE_ID_ENABLE                  :Writeln('\HINT_SET_PRIMITIVE_ID_ENABLE');
  OP_HINT_SET_PRIMITIVE_RESET_INDEX_ENABLE         :Writeln('\HINT_SET_PRIMITIVE_RESET_INDEX_ENABLE');
  OP_HINT_SET_DRAW_PAYLOAD_CONTROL                 :Writeln('\HINT_SET_DRAW_PAYLOAD_CONTROL');
  OP_HINT_SET_INSTANCE_STEP_RATE                   :Writeln('\HINT_SET_INSTANCE_STEP_RATE');
  OP_HINT_SETUP_ES_GS_RING_REGISTERS               :Writeln('\HINT_SETUP_ES_GS_RING_REGISTERS');
  OP_HINT_SET_VERTEX_REUSE_ENABLE                  :Writeln('\HINT_SET_VERTEX_REUSE_ENABLE');
  OP_HINT_SET_HTILE_STENCIL0                       :Writeln('\HINT_SET_HTILE_STENCIL0');
  OP_HINT_SET_HTILE_STENCIL1                       :Writeln('\HINT_SET_HTILE_STENCIL1');
  OP_HINT_SETUP_DRAW_OPAQUE_PARAMETERS_1           :Writeln('\HINT_SETUP_DRAW_OPAQUE_PARAMETERS_1');
  OP_HINT_SETUP_DRAW_OPAQUE_PARAMETERS_0           :Writeln('\HINT_SETUP_DRAW_OPAQUE_PARAMETERS_0');
  OP_HINT_SET_TESSELLATION_DISTRIBUTION_THRESHOLDS :Writeln('\HINT_SET_TESSELLATION_DISTRIBUTION_THRESHOLDS');
  OP_HINT_SET_ACTIVE_SHADER_STAGES                 :Writeln('\HINT_SET_ACTIVE_SHADER_STAGES');
  OP_HINT_SETUP_GS_VS_RING_REGISTERS               :Writeln('\HINT_SETUP_GS_VS_RING_REGISTERS');
  OP_HINT_SET_ALPHA_TO_MASK_CONTROL                :Writeln('\HINT_SET_ALPHA_TO_MASK_CONTROL');
  OP_HINT_SET_DISPATCH_DRAW_INDEX_DEALLOCATION_MASK:Writeln('\HINT_SET_DISPATCH_DRAW_INDEX_DEALLOCATION_MASK');
  OP_HINT_SET_POLYGON_OFFSET_Z_FORMAT              :Writeln('\HINT_SET_POLYGON_OFFSET_Z_FORMAT');
  OP_HINT_SET_POLYGON_OFFSET_CLAMP                 :Writeln('\HINT_SET_POLYGON_OFFSET_CLAMP');
  OP_HINT_SET_POLYGON_OFFSET_FRONT                 :Writeln('\HINT_SET_POLYGON_OFFSET_FRONT');
  OP_HINT_SET_POLYGON_OFFSET_BACK                  :Writeln('\HINT_SET_POLYGON_OFFSET_BACK');
  OP_HINT_SET_GS_MODE_DISABLE                      :Writeln('\HINT_SET_GS_MODE_DISABLE');
  OP_HINT_SET_STREAMOUT_MAPPING                    :Writeln('\HINT_SET_STREAMOUT_MAPPING');
  OP_HINT_SET_AA_SAMPLE_COUNT                      :Writeln('\HINT_SET_AA_SAMPLE_COUNT');
  OP_HINT_SET_VERTEX_QUANTIZATION                  :Writeln('\HINT_SET_VERTEX_QUANTIZATION');
  OP_HINT_SET_GUARD_BANDS                          :Writeln('\HINT_SET_GUARD_BANDS');
  OP_HINT_SET_AA_SAMPLE_MASK1                      :Writeln('\HINT_SET_AA_SAMPLE_MASK1');
  OP_HINT_SET_AA_SAMPLE_MASK2                      :Writeln('\HINT_SET_AA_SAMPLE_MASK2');
  OP_HINT_SET_TEXTURE_GRADIENT_FACTORS             :Writeln('\HINT_SET_TEXTURE_GRADIENT_FACTORS');
  OP_HINT_SET_PERF_COUNTER_CONTROL_PA              :Writeln('\HINT_SET_PERF_COUNTER_CONTROL_PA');
  OP_HINT_SET_PRIMITIVE_TYPE_NEO                   :Writeln('\HINT_SET_PRIMITIVE_TYPE_NEO');
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
 {$ifdef ww}writeln;{$endif}
end;

//The purpose of the Clear_State packet is to reduce command buffer preamble setup time for all driver versions of
//both DX and OpenGL and to specifically support DX11’s Display Lists requirements. The definition of Clear State
//is essentially everything off, resources all NULL, other values set to a defined default state.

procedure onClearState(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDCLEARSTATE);
begin
 GPU_REGS.Clear;
end;

procedure onSetCommonReg(reg:WORD;value:DWORD);
begin
 FLastSetReg:=reg;

 Case reg of

  //onSetContextReg

  mmCB_COLOR0_BASE..mmCB_COLOR7_DCC_BASE:
  begin
   PDWORD(@GPU_REGS.RENDER_TARGET)[reg-mmCB_COLOR0_BASE]:=value;
  end;

  mmCB_TARGET_MASK  :DWORD(GPU_REGS.TARGET_MASK)     :=value;
  mmCB_COLOR_CONTROL:DWORD(GPU_REGS.CB_COLOR_CONTROL):=value;


  mmCB_BLEND_RED..mmCB_BLEND_ALPHA:
  begin
   PDWORD(@GPU_REGS.CB_BLEND_RGBA)[reg-mmCB_BLEND_RED]:=value;
  end;

  mmCB_BLEND0_CONTROL..mmCB_BLEND7_CONTROL:
  begin
   PDWORD(@GPU_REGS.CB_BLEND_CONTROL)[reg-mmCB_BLEND0_CONTROL]:=value;
  end;

  mmCB_SHADER_MASK  :DWORD(GPU_REGS.SPI.PS.SHADER_MASK):=value;

  mmPA_SC_MODE_CNTL_0:DWORD(GPU_REGS.SC_MODE_CNTL_0) :=value;
  mmPA_SC_MODE_CNTL_1:DWORD(GPU_REGS.SC_MODE_CNTL_1) :=value;

  mmPA_SC_GENERIC_SCISSOR_TL:DWORD(GPU_REGS.GENERIC_SCISSOR.TL) :=value;
  mmPA_SC_GENERIC_SCISSOR_BR:DWORD(GPU_REGS.GENERIC_SCISSOR.BR) :=value;

  mmPA_SC_VPORT_SCISSOR_0_TL..mmPA_SC_VPORT_SCISSOR_15_BR:
  begin
   PDWORD(@GPU_REGS.VPORT_SCISSOR)[reg-mmPA_SC_VPORT_SCISSOR_0_TL]:=value;
  end;

  mmPA_SC_VPORT_ZMIN_0..mmPA_SC_VPORT_ZMAX_15:
  begin
   PDWORD(@GPU_REGS.VPORT_ZMIN_MAX)[reg-mmPA_SC_VPORT_ZMIN_0]:=value;
  end;

  mmPA_CL_VPORT_XSCALE..mmPA_CL_VPORT_ZOFFSET_15:
  begin
   PDWORD(@GPU_REGS.VPORT_SCALE_OFFSET)[reg-mmPA_CL_VPORT_XSCALE]:=value;
  end;

  mmPA_CL_VTE_CNTL:DWORD(GPU_REGS.VTE_CNTL):=value;

  mmPA_SC_SCREEN_SCISSOR_TL:DWORD(GPU_REGS.SCREEN_SCISSOR_TL):=value;
  mmPA_SC_SCREEN_SCISSOR_BR:DWORD(GPU_REGS.SCREEN_SCISSOR_BR):=value;

  mmPA_SC_AA_MASK_X0Y0_X1Y0:DWORD(GPU_REGS.SC_AA_MASK_X0Y0_X1Y0):=value;
  mmPA_SC_AA_MASK_X0Y1_X1Y1:DWORD(GPU_REGS.SC_AA_MASK_X0Y1_X1Y1):=value;
  mmPA_SC_AA_CONFIG        :DWORD(GPU_REGS.SC_AA_CONFIG):=value;

  mmPA_SU_HARDWARE_SCREEN_OFFSET:DWORD(GPU_REGS.HARDWARE_SCREEN_OFFSET):=value;

  mmPA_SU_VTX_CNTL:DWORD(GPU_REGS.VTX_CNTL):=value;

  mmPA_SU_LINE_CNTL:DWORD(GPU_REGS.SU_LINE_CNTL)      :=value;
  mmPA_SU_POINT_SIZE:DWORD(GPU_REGS.SU_POINT_SIZE)    :=value;
  mmPA_SU_POINT_MINMAX:DWORD(GPU_REGS.SU_POINT_MINMAX):=value;

  mmPA_CL_CLIP_CNTL:DWORD(GPU_REGS.CL_CLIP_CNTL)        :=value;
  mmPA_SC_CLIPRECT_RULE:DWORD(GPU_REGS.SC_CLIPRECT_RULE):=value;

  mmPA_CL_GB_VERT_CLIP_ADJ:PDWORD(@GPU_REGS.GB_CLIP.VERT_CLIP_ADJ)^:=value;
  mmPA_CL_GB_VERT_DISC_ADJ:PDWORD(@GPU_REGS.GB_CLIP.VERT_DISC_ADJ)^:=value;
  mmPA_CL_GB_HORZ_CLIP_ADJ:PDWORD(@GPU_REGS.GB_CLIP.HORZ_CLIP_ADJ)^:=value;
  mmPA_CL_GB_HORZ_DISC_ADJ:PDWORD(@GPU_REGS.GB_CLIP.HORZ_DISC_ADJ)^:=value;

  mmSPI_VS_OUT_CONFIG    :DWORD(GPU_REGS.SPI.VS.OUT_CONFIG):=value;
  mmPA_CL_VS_OUT_CNTL    :DWORD(GPU_REGS.SPI.VS.OUT_CNTL):=value;

  mmSPI_SHADER_POS_FORMAT:DWORD(GPU_REGS.SPI.VS.POS_FORMAT):=value;
  mmSPI_SHADER_Z_FORMAT  :DWORD(GPU_REGS.SPI.PS.Z_FORMAT)  :=value;
  mmSPI_SHADER_COL_FORMAT:DWORD(GPU_REGS.SPI.PS.COL_FORMAT):=value;
  mmSPI_BARYC_CNTL       :DWORD(GPU_REGS.SPI.PS.BARYC_CNTL):=value;

  mmSPI_PS_INPUT_ENA     :DWORD(GPU_REGS.SPI.PS.INPUT_ENA) :=value;
  mmSPI_PS_INPUT_ADDR    :DWORD(GPU_REGS.SPI.PS.INPUT_ADDR):=value;
  mmSPI_PS_IN_CONTROL    :DWORD(GPU_REGS.SPI.PS.IN_CONTROL):=value;

  mmSPI_PS_INPUT_CNTL_0..mmSPI_PS_INPUT_CNTL_31:
  begin
   PDWORD(@GPU_REGS.SPI.PS.INPUT_CNTL)[reg-mmSPI_PS_INPUT_CNTL_0]:=value;
  end;

  mmDB_SHADER_CONTROL    :DWORD(GPU_REGS.SPI.PS.SHADER_CONTROL):=value;

  mmDB_RENDER_CONTROL    :DWORD(GPU_REGS.DEPTH.RENDER_CONTROL):=value;
  mmDB_DEPTH_CONTROL     :DWORD(GPU_REGS.DEPTH.DEPTH_CONTROL):=value;

  mmDB_DEPTH_VIEW        :DWORD(GPU_REGS.DEPTH.DEPTH_VIEW        ):=value;
  mmDB_HTILE_DATA_BASE   :DWORD(GPU_REGS.DEPTH.HTILE_DATA_BASE   ):=value;
  mmDB_DEPTH_BOUNDS_MIN  :DWORD(GPU_REGS.DEPTH.DEPTH_BOUNDS_MIN  ):=value;
  mmDB_DEPTH_BOUNDS_MAX  :DWORD(GPU_REGS.DEPTH.DEPTH_BOUNDS_MAX  ):=value;
  mmDB_STENCIL_CLEAR     :DWORD(GPU_REGS.DEPTH.STENCIL_CLEAR     ):=value;
  mmDB_DEPTH_CLEAR       :DWORD(GPU_REGS.DEPTH.DEPTH_CLEAR       ):=value;

  mmDB_DEPTH_INFO        :DWORD(GPU_REGS.DEPTH.DEPTH_INFO        ):=value;
  mmDB_Z_INFO            :DWORD(GPU_REGS.DEPTH.Z_INFO            ):=value;
  mmDB_STENCIL_INFO      :DWORD(GPU_REGS.DEPTH.STENCIL_INFO      ):=value;
  mmDB_Z_READ_BASE       :DWORD(GPU_REGS.DEPTH.Z_READ_BASE       ):=value;
  mmDB_STENCIL_READ_BASE :DWORD(GPU_REGS.DEPTH.STENCIL_READ_BASE ):=value;
  mmDB_Z_WRITE_BASE      :DWORD(GPU_REGS.DEPTH.Z_WRITE_BASE      ):=value;
  mmDB_STENCIL_WRITE_BASE:DWORD(GPU_REGS.DEPTH.STENCIL_WRITE_BASE):=value;
  mmDB_DEPTH_SIZE        :DWORD(GPU_REGS.DEPTH.DEPTH_SIZE        ):=value;
  mmDB_DEPTH_SLICE       :DWORD(GPU_REGS.DEPTH.DEPTH_SLICE       ):=value;

  mmDB_HTILE_SURFACE     :DWORD(GPU_REGS.DEPTH.HTILE_SURFACE     ):=value;

  mmVGT_SHADER_STAGES_EN :DWORD(GPU_REGS.VGT_SHADER_STAGES_EN) :=value;
  mmVGT_OUT_DEALLOC_CNTL :DWORD(GPU_REGS.VGT_OUT_DEALLOC_CNTL) :=value;

  mmVGT_VTX_CNT_EN       :DWORD(GPU_REGS.VGT_VTX_INDX.CNT_EN):=value;

  mmVGT_MIN_VTX_INDX     :DWORD(GPU_REGS.VGT_VTX_INDX.MIN_INDX):=value;
  mmVGT_MAX_VTX_INDX     :DWORD(GPU_REGS.VGT_VTX_INDX.MAX_INDX):=value;

  mmVGT_INDX_OFFSET      :DWORD(GPU_REGS.VGT_VTX_INDX.INDX_OFFSET):=value;

  mmVGT_MULTI_PRIM_IB_RESET_INDX:DWORD(GPU_REGS.VGT_MULTI_PRIM_IB_RESET_INDX):=value;

  mmVGT_OUTPUT_PATH_CNTL:DWORD(GPU_REGS.VGT_OUTPUT_PATH_CNTL):=value;

  //mmVGT_GS_MODE:value:=value;

  mmPA_SU_POLY_OFFSET_DB_FMT_CNTL:DWORD(GPU_REGS.PA_SU_POLY_OFFSET_DB_FMT_CNTL):=value;


  //SetShReg

  mmSPI_SHADER_PGM_LO_PS   :GPU_REGS.SPI.PS.LO:=value;
  mmSPI_SHADER_PGM_HI_PS   :GPU_REGS.SPI.PS.HI:=value;
  mmSPI_SHADER_PGM_RSRC1_PS:DWORD(GPU_REGS.SPI.PS.RSRC1):=value;
  mmSPI_SHADER_PGM_RSRC2_PS:DWORD(GPU_REGS.SPI.PS.RSRC2):=value;
  mmSPI_SHADER_PGM_RSRC3_PS:DWORD(GPU_REGS.SPI.PS.RSRC3):=value;

  mmSPI_SHADER_USER_DATA_PS_0..mmSPI_SHADER_USER_DATA_PS_15:
   PDWORD(@GPU_REGS.SPI.PS.USER_DATA)[reg-mmSPI_SHADER_USER_DATA_PS_0]:=value;

  mmSPI_SHADER_PGM_LO_VS   :GPU_REGS.SPI.VS.LO:=value;
  mmSPI_SHADER_PGM_HI_VS   :GPU_REGS.SPI.VS.HI:=value;
  mmSPI_SHADER_PGM_RSRC1_VS:DWORD(GPU_REGS.SPI.VS.RSRC1):=value;
  mmSPI_SHADER_PGM_RSRC2_VS:DWORD(GPU_REGS.SPI.VS.RSRC2):=value;
  mmSPI_SHADER_PGM_RSRC3_VS:DWORD(GPU_REGS.SPI.VS.RSRC3):=value;

  mmSPI_SHADER_USER_DATA_VS_0..mmSPI_SHADER_USER_DATA_VS_15:
   PDWORD(@GPU_REGS.SPI.VS.USER_DATA)[reg-mmSPI_SHADER_USER_DATA_VS_0]:=value;

  mmSPI_SHADER_LATE_ALLOC_VS:DWORD(GPU_REGS.SPI.VS.LATE_ALLOC):=value;

  //mmSPI_SHADER_PGM_RSRC3_GS:value:=value;
  //mmSPI_SHADER_PGM_RSRC3_ES:value:=value;
  //mmSPI_SHADER_PGM_RSRC3_HS:value:=value;
  //mmSPI_SHADER_PGM_RSRC3_LS:value:=value;

  mmCOMPUTE_PGM_LO         :GPU_REGS.SPI.CS.LO:=value;
  mmCOMPUTE_PGM_HI         :GPU_REGS.SPI.CS.HI:=value;
  mmCOMPUTE_PGM_RSRC1      :DWORD(GPU_REGS.SPI.CS.RSRC1):=value;
  mmCOMPUTE_PGM_RSRC2      :DWORD(GPU_REGS.SPI.CS.RSRC2):=value;

  mmCOMPUTE_NUM_THREAD_X   :DWORD(GPU_REGS.SPI.CS.NUM_THREAD_X):=value;
  mmCOMPUTE_NUM_THREAD_Y   :DWORD(GPU_REGS.SPI.CS.NUM_THREAD_Y):=value;
  mmCOMPUTE_NUM_THREAD_Z   :DWORD(GPU_REGS.SPI.CS.NUM_THREAD_Z):=value;

  mmCOMPUTE_USER_DATA_0..mmCOMPUTE_USER_DATA_15:
   PDWORD(@GPU_REGS.SPI.CS.USER_DATA)[reg-mmCOMPUTE_USER_DATA_0]:=value;

  mmCOMPUTE_STATIC_THREAD_MGMT_SE0:DWORD(GPU_REGS.SPI.CS.STATIC_THREAD_MGMT_SE0):=value;
  mmCOMPUTE_STATIC_THREAD_MGMT_SE1:DWORD(GPU_REGS.SPI.CS.STATIC_THREAD_MGMT_SE1):=value;
  mmCOMPUTE_RESOURCE_LIMITS       :DWORD(GPU_REGS.SPI.CS.RESOURCE_LIMITS):=value;

  //SetUConfigReg

  mmVGT_PRIMITIVE_TYPE:DWORD(GPU_REGS.VGT_PRIMITIVE_TYPE):=value;
  mmVGT_INDEX_TYPE    :DWORD(GPU_REGS.VGT_INDEX_TYPE    ):=value;
  mmVGT_NUM_INSTANCES :DWORD(GPU_REGS.VGT_NUM_INSTANCES ):=value;
  mmGRBM_GFX_INDEX    :DWORD(GPU_REGS.GRBM_GFX_INDEX    ):=value;

  {$ifdef ww}else
   Writeln('onSetCommonReg:',getRegName(reg),'=',HexStr(value,8));{$endif}
 end;
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

  onSetCommonReg(r,v);

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

  onSetCommonReg(r,v);

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

  //{$ifdef ww}Writeln('SetUConfigReg:',getRegName(r),'=',HexStr(v,8));{$endif}

  onSetCommonReg(r,v);

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

var
 //FCmdPool:TCmdPool;

 //FCmdBuffer:TvCmdBuffer;

 //FVSShader:TvShaderExt;
 //FPSShader:TvShaderExt;

 FAttrBuilder:TvAttrBuilder;
 FUniformBuilder:TvUniformBuilder;

 FShaderGroup:TvShaderGroup;

Procedure vkCmdBindVertexBuffer(commandBuffer:TVkCommandBuffer;
                                Binding:TVkUInt32;
                                Buffer:TVkBuffer;
                                Offset:TVkDeviceSize);
begin
 vkCmdBindVertexBuffers(commandBuffer,Binding,1,@Buffer,@Offset);
end;

Procedure vkCmdBindDescriptorBuffer(commandBuffer:TVkCommandBuffer;
                                    Binding:TVkUInt32;
                                    Buffer:TVkBuffer;
                                    Offset:TVkDeviceSize);
var
 info:TVkWriteDescriptorSet;
begin
 info:=Default(TVkWriteDescriptorSet);
 info.sType:=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;

end;

Procedure vkCmdBindSB(cmd:TVkCommandBuffer;
                      point:TVkPipelineBindPoint;
                      layout:TVkPipelineLayout;
                      aSet,aBind,aElem:TVkUInt32;
                      buffer:TVkBuffer;
                      offset,range:TVkDeviceSize);
var
 dwrite:TVkWriteDescriptorSet;
 buf:TVkDescriptorBufferInfo;
begin
 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
 dwrite.descriptorCount:=1;
 dwrite.pBufferInfo    :=@buf;

 buf:=Default(TVkDescriptorBufferInfo);
 buf.buffer:=buffer;
 buf.offset:=offset;
 buf.range :=range ;

 if (vkCmdPushDescriptorSetKHR=nil) then
 begin
  TPFN_vkVoidFunction(vkCmdPushDescriptorSetKHR):=vkGetInstanceProcAddr(VulkanApp.FInstance,'vkCmdPushDescriptorSetKHR');
 end;

 Assert(vkCmdPushDescriptorSetKHR<>nil);

 vkCmdPushDescriptorSetKHR(cmd,point,layout,aSet,1,@dwrite);
end;

procedure vkCmdWaitEvent(commandBuffer:TVkCommandBuffer;
                         event:TVkEvent;
                         srcStageMask:TVkPipelineStageFlags;
                         dstStageMask:TVkPipelineStageFlags);
begin
 vkCmdWaitEvents(commandBuffer,
  1,
  @event,
  srcStageMask,
  dstStageMask,
  0,
  nil,
  0,
  nil,
  0,
  nil);
end;

type
 TvEvent2=class(TvEvent)
  Procedure Release(Sender:TObject);
 end;

Procedure TvEvent2.Release(Sender:TObject);
begin
 Free;
end;

//var
// FRenderCmd:TvRenderTargets;

procedure UpdateGpuRegsInfo;


var
 i,o,a:DWORD;
 pData:Pointer;

 FRenderCmd:TvRenderTargets;

 RT_INFO:TRT_INFO;
 DB_INFO:TDB_INFO;
 //ri:TURDevcImage2D;
 ri:TvImage2;
 iv:TvImageView2;

 sm:TvSampler;

 range:TVkImageSubresourceRange;
 clr:TVkClearColorValue;

 clr2:TVkClearDepthStencilValue;

 BufferImageCopy:TVkBufferImageCopy;

 fdump_ps,fdump_vs:RawByteString;

 //PB:PDWORD;
 //PV:PVSharpResource4;

 //buf:TURDevcImage1D;

 buf:TvHostBuffer;

 FDescriptorGroup:TvDescriptorGroup;

 FVSShader:TvShaderExt;
 FPSShader:TvShaderExt;

 //FShaderGroup:TvShaderGroup;

 Event:TvEvent2;

begin

 {$ifdef null_rt}Exit;{$endif}

 fdump_ps:=DumpPS(GPU_REGS);
 fdump_vs:=DumpVS(GPU_REGS);

 {$ifdef ww}Writeln(fdump_vs);{$endif}
 {$ifdef ww}Writeln(fdump_ps);{$endif}

 //if fdump_ps='shader_dump\WeAreDoomed_ps4_ps_BA60EAD7.dump' then Exit;

 //if fdump_ps='shader_dump\WeAreDoomed_ps4_ps_B9680888.dump' then
 //begin
 // exit;
 //end;

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

 //if not GPU_REGS.COMP_ENABLE then Exit;

 if not (GPU_REGS.COMP_ENABLE or GPU_REGS.DB_ENABLE) then Exit;

 InitVulkan;
 if (FCmdPool=nil) then
 begin
  FCmdPool:=TvCmdPool.Create(VulkanApp.FGFamily);
 end;

 if (FCmdBuffer=nil) then
 begin
  FCmdBuffer:=TvCmdBuffer.Create(FCmdPool,RenderQueue);

  //FCmdBuffer.FWaitSemaphore:=TvSemaphore.Create;
  //FCmdBuffer.FSignSemaphore:=TvSemaphore.Create;
 end;


 //if (FRenderCmd=nil) then
 //begin
 // FRenderCmd:=TvRenderTargets.Create;
 // FRenderCmd.FRenderPass:=TvRenderPass.Create;
 // FRenderCmd.FPipeline  :=TvGraphicsPipeline.Create;
 // FRenderCmd.FPipeline.FRenderPass:=FRenderCmd.FRenderPass;
 //
 // FRenderCmd.FFramebuffer:=TvFramebuffer.Create;
 // FRenderCmd.FFramebuffer.SetRenderPass(FRenderCmd.FRenderPass);
 //end;

 ///////////////////

 FRenderCmd:=TvRenderTargets.Create;

 FRenderCmd.FRenderPass:=TvRenderPass.Create;
 FRenderCmd.FPipeline  :=TvGraphicsPipeline.Create;
 /////FRenderCmd.FPipeline.FLayout:=TvPipelineLayout.Create;
 FRenderCmd.FPipeline.FRenderPass:=FRenderCmd.FRenderPass;

 FRenderCmd.FFramebuffer:=TvFramebuffer.Create;
 FRenderCmd.FFramebuffer.SetRenderPass(FRenderCmd.FRenderPass);

 //////////////////


 FRenderCmd.FFramebuffer.FreeImageViews;
 FRenderCmd.FRenderPass.Clear;
 FRenderCmd.FPipeline.Clear;

 FRenderCmd.FFramebuffer.SetSize(GPU_REGS.GET_SCREEN_SIZE);

 FRenderCmd.FPipeline.SetPrimType(GPU_REGS.GET_PRIM_TYPE);
 FRenderCmd.FPipeline.SetBlendColors(@GPU_REGS.CB_BLEND_RGBA);

 FRenderCmd.FRenderArea:=GPU_REGS.GET_SCREEN;


 For i:=0 to 15 do
  if GPU_REGS.VP_ENABLE(i) then
  begin
   FRenderCmd.FPipeline.AddVPort(GPU_REGS.GET_VPORT(i),GPU_REGS.GET_SCISSOR(i));
  end;

 FCmdBuffer.EndRenderPass;
 FCmdBuffer.BeginCmdBuffer;


 //vkMemoryBarrier(FCmdBuffer.cmdbuf,
 //          {ord(VK_ACCESS_TRANSFER_WRITE_BIT) or} {ord(VK_ACCESS_HOST_WRITE_BIT) or} ord(VK_ACCESS_MEMORY_WRITE_BIT),
 //          {ord(VK_ACCESS_INDEX_READ_BIT) or} ord(VK_ACCESS_MEMORY_READ_BIT),
 //          ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
 //          ord(VK_PIPELINE_STAGE_TRANSFER_BIT)
 //          );

 //vkCmdPipelineBarrier(FCmdBuffer.cmdbuf,
 // ord(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
 // ord(VK_PIPELINE_STAGE_TRANSFER_BIT),
 // 0,
 // 0,
 // nil,
 // 0,
 // nil,
 // 0,
 // nil);


 Event:=nil;
 //Event:=TvEvent2.Create;
 //FCmdBuffer.AddDependence(@Event.Release);


 if (Event<>nil) then
 vkCmdSetEvent(FCmdBuffer.cmdbuf,
               Event.FHandle,
               ord(VK_PIPELINE_STAGE_TRANSFER_BIT));


 //Writeln(Event.Status);

 if GPU_REGS.COMP_ENABLE then
 For i:=0 to 7 do
 if GPU_REGS.RT_ENABLE(i) then
  begin
   RT_INFO:=GPU_REGS.GET_RT_INFO(i);

   {$ifdef ww}Writeln('RT:',i,' ',HexStr(RT_INFO.FImageInfo.Addr));{$endif}

   //RT_INFO.IMAGE_USAGE:=RT_INFO.IMAGE_USAGE or TM_CLEAR;
   //RT_INFO.IMAGE_USAGE:=RT_INFO.IMAGE_USAGE and (not TM_READ);

   ri:=FetchImage(FCmdBuffer,
                  RT_INFO.FImageInfo,
                  ord(VK_IMAGE_USAGE_SAMPLED_BIT) or
                  ord(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                  RT_INFO.IMAGE_USAGE
                  );

   //ri.data_usage:=ri.data_usage and (not TM_READ); //reset read

   iv:=ri.FetchView(FCmdBuffer,RT_INFO.FImageView);

   //

   {$ifdef ww}
   Writeln('TM_READ :',RT_INFO.IMAGE_USAGE and TM_READ <>0);
   Writeln('TM_WRITE:',RT_INFO.IMAGE_USAGE and TM_WRITE<>0);
   Writeln('TM_CLEAR:',RT_INFO.IMAGE_USAGE and TM_CLEAR<>0);
   {$endif}

   //Writeln(hexstr(PDWORD(RT_INFO.FImageInfo.Addr)[0],8));
   //writeln;

   //RT_INFO.IMAGE_USAGE:={TM_CLEAR or }TM_READ{ or TM_WRITE};

   //RT_INFO.IMAGE_USAGE:=RT_INFO.IMAGE_USAGE and (not TM_CLEAR);


   {
   if (RT_INFO.IMAGE_USAGE and TM_CLEAR=0) then
   begin

    FCmdBuffer.PushImageBarrier(ri.FHandle,
                                iv.GetSubresRange,
                                ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

    buf:=FetchHostBuffer(FCmdBuffer,
                         ri.key.Addr,
                         ri.key.params.extend.width*ri.key.params.extend.height*4,
                         ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT));

    vkBufferMemoryBarrier(FCmdBuffer.cmdbuf,
                          buf.FHandle,
                          ord(VK_ACCESS_SHADER_WRITE_BIT),
                          ord(VK_ACCESS_MEMORY_READ_BIT),
                          0,ri.key.params.extend.width*ri.key.params.extend.height*4,
                          ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                          ord(VK_PIPELINE_STAGE_TRANSFER_BIT)
                          );

    BufferImageCopy:=Default(TVkBufferImageCopy);

    BufferImageCopy.bufferOffset:=buf.Foffset;
    BufferImageCopy.bufferRowLength:=0;
    BufferImageCopy.bufferImageHeight:=0;
    BufferImageCopy.imageSubresource:=ri.GetSubresLayer;
    //BufferImageCopy.imageOffset:TVkOffset3D; //0
    BufferImageCopy.imageExtent.Create(ri.key.params.extend.width,
                                       ri.key.params.extend.height,
                                       ri.key.params.extend.depth);

    vkCmdCopyBufferToImage(FCmdBuffer.cmdbuf,
                           buf.FHandle,
                           ri.FHandle,
                           VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                           1,
                           @BufferImageCopy);
   end;
   }

   //

   //VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
   //VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT

   FCmdBuffer.PushImageBarrier(ri.FHandle,
                               iv.GetSubresRange,
                               GetColorAccessMask(RT_INFO.IMAGE_USAGE),
                               VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                               ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                               ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) );

   FRenderCmd.FFramebuffer.AddImageView(iv);

   //Writeln('colorAttachmentCount:',FRenderCmd.FRenderPass.subpass.colorAttachmentCount);
   //Writeln('AtCount:',FRenderCmd.FRenderPass.AtCount);

   FRenderCmd.FRenderPass.AddColorRef(FRenderCmd.FRenderPass.subpass.colorAttachmentCount,RT_INFO.IMAGE_USAGE);

   FRenderCmd.FRenderPass.AddColorAt(RT_INFO.FImageInfo.cformat,
                                     RT_INFO.IMAGE_USAGE,
                                     TVkSampleCountFlagBits(RT_INFO.FImageInfo.params.samples));


   //RT_INFO.blend.blendEnable:=0;
   FRenderCmd.FPipeline.AddBlend(RT_INFO.blend);

   //if RT_INFO.FAST_CLEAR then
   begin
    FRenderCmd.AddClearColor(TVkClearValue(RT_INFO.CLEAR_COLOR));
   end;

  end;

 if GPU_REGS.DB_ENABLE then
 begin
  DB_INFO:=GPU_REGS.GET_DB_INFO;

  {$ifdef ww}
  Writeln('DB');
  Writeln('TM_READ :',DB_INFO.DEPTH_USAGE and TM_READ <>0);
  Writeln('TM_WRITE:',DB_INFO.DEPTH_USAGE and TM_WRITE<>0);
  Writeln('TM_CLEAR:',DB_INFO.DEPTH_USAGE and TM_CLEAR<>0);
  {$endif}

  //DB_INFO.DEPTH_USAGE:={TM_CLEAR or} TM_READ or TM_WRITE;


  ri:=FetchImage(FCmdBuffer,
                 DB_INFO.FImageInfo,
                 ord(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or
                 ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
                 ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                 {DB_INFO.DEPTH_USAGE}0
                 );

  //ri.data_usage:=ri.data_usage and (not TM_READ); //reset read

  iv:=ri.FetchView(FCmdBuffer);


  if not GPU_REGS.COMP_ENABLE then
  begin
   FCmdBuffer.PushImageBarrier(ri.FHandle,
                               iv.GetSubresRange,
                               ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

   range:=iv.GetSubresRange;
   clr2:=DB_INFO.CLEAR_VALUE.depthStencil;

   vkCmdClearDepthStencilImage(FCmdBuffer.cmdbuf,
                               ri.FHandle,
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               @clr2,
                               1,@range);

   Exit;
  end;

  //DB_INFO.DEPTH_CLEAR:=false;

  {if not (DB_INFO.DEPTH_CLEAR or DB_INFO.STENCIL_CLEAR) then
  begin

   FCmdBuffer.PushImageBarrier(ri.FHandle,
                               iv.GetSubresRange,
                               ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

   buf:=FetchHostBuffer(FCmdBuffer,
                        ri.key.Addr,
                        ri.key.params.extend.width*ri.key.params.extend.height*4,
                        ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT));

   BufferImageCopy:=Default(TVkBufferImageCopy);

   BufferImageCopy.bufferOffset:=buf.Foffset;
   BufferImageCopy.bufferRowLength:=0;
   BufferImageCopy.bufferImageHeight:=0;
   BufferImageCopy.imageSubresource:=ri.GetSubresLayer;
   BufferImageCopy.imageSubresource.aspectMask:=ord(VK_IMAGE_ASPECT_DEPTH_BIT);
   //BufferImageCopy.imageOffset:TVkOffset3D; //0
   BufferImageCopy.imageExtent.Create(ri.key.params.extend.width,
                                      ri.key.params.extend.height,
                                      ri.key.params.extend.depth);

   vkCmdCopyBufferToImage(FCmdBuffer.cmdbuf,
                          buf.FHandle,
                          ri.FHandle,
                          VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                          1,
                          @BufferImageCopy);

  end;}

  FCmdBuffer.PushImageBarrier(ri.FHandle,
                              iv.GetSubresRange,
                              GetDepthStencilAccessMask(DB_INFO.DEPTH_USAGE,DB_INFO.STENCIL_USAGE),
                              GetDepthStencilLayout    (DB_INFO.DEPTH_USAGE,DB_INFO.STENCIL_USAGE),
                              DB_INFO.zorder_stage );

  FRenderCmd.FFramebuffer.AddImageView(iv);

  //Writeln('colorAttachmentCount:',FRenderCmd.FRenderPass.subpass.colorAttachmentCount);
  //Writeln('AtCount:',FRenderCmd.FRenderPass.AtCount);

  FRenderCmd.FRenderPass.SetDepthStencilRef(FRenderCmd.FRenderPass.subpass.colorAttachmentCount,DB_INFO.DEPTH_USAGE,DB_INFO.STENCIL_USAGE);

  //if not GPU_REGS.COMP_ENABLE then
  //begin
  // DB_INFO.DEPTH_CLEAR:=True;
  //end;

  {FRenderCmd.FRenderPass.AddDepthAt(
   DB_INFO.FImageInfo.cformat,
   DB_INFO.DEPTH_CLEAR,
   not DB_INFO.Z_READ_ONLY,
   DB_INFO.STENCIL_CLEAR,
   not DB_INFO.STENCIL_READ_ONLY);}

  FRenderCmd.FRenderPass.AddDepthAt(
   DB_INFO.FImageInfo.cformat,
   DB_INFO.DEPTH_USAGE,
   DB_INFO.STENCIL_USAGE);

  FRenderCmd.FRenderPass.SetZorderStage(DB_INFO.zorder_stage);

  //if DB_INFO.DEPTH_CLEAR or DB_INFO.STENCIL_CLEAR then
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

 {$ifdef ww}Writeln('[FVSShader]');{$endif}
 FVSShader:=FetchShader(vShaderStageVs,0,GPU_REGS);
 if (FVSShader=nil) then Exit;

 FAttrBuilder:=Default(TvAttrBuilder);
 FVSShader.EnumVertLayout(@FAttrBuilder.AddAttr,FVSShader.FDescSetId,@GPU_REGS.SPI.VS.USER_DATA);

 //if (FVSShader=nil) then
 //begin
 // FVSShader:=TvShaderExt.Create;
 // FVSShader.FDescSetId:=0;
 // FVSShader.LoadFromFile(ChangeFileExt(fdump_vs,'.spv'));
 //
 //
 // FAttrBuilder:=Default(TvAttrBuilder);
 //
 // FVSShader.EnumVertLayout(@FAttrBuilder.AddAttr,FVSShader.FDescSetId,@GPU_REGS.SPI.VS.USER_DATA);
 //
 //end;

 if (Length(FAttrBuilder.FBindDescs)<>0) then
 begin
  With FRenderCmd.FPipeline.vertexInputInfo do
  begin
   vertexBindingDescriptionCount  :=Length(FAttrBuilder.FBindDescs);
   pVertexBindingDescriptions     :=@FAttrBuilder.FBindDescs[0];
   vertexAttributeDescriptionCount:=Length(FAttrBuilder.FAttrDescs);
   pVertexAttributeDescriptions   :=@FAttrBuilder.FAttrDescs[0];
  end;
 end;

 {$ifdef ww}Writeln('[FPSShader]');{$endif}
 FPSShader:=FetchShader(vShaderStagePs,1,GPU_REGS);
 if (FPSShader=nil) then Exit;

 //if (FPSShader=nil) then
 //begin
 // FPSShader:=TvShaderExt.Create;
 // FPSShader.FDescSetId:=1;
 // FPSShader.LoadFromFile(ChangeFileExt(fdump_ps,'.spv'));
 //end;

 if (FShaderGroup=nil) then
 begin
  FShaderGroup:=TvShaderGroup.Create;
 end;

  FShaderGroup.Clear;

  FShaderGroup.SetVSShader(FVSShader);
  FShaderGroup.SetPSShader(FPSShader);

  FShaderGroup.Compile;
 //end;

 FRenderCmd.FPipeline.FShaderGroup:=FShaderGroup;

  FDescriptorGroup:=FetchDescriptorGroup(FCmdBuffer,FShaderGroup.FLayout);


  FUniformBuilder:=Default(TvUniformBuilder);

  FVSShader.EnumUnifLayout(@FUniformBuilder.AddAttr,FVSShader.FDescSetId,@GPU_REGS.SPI.VS.USER_DATA);

  if (FPSShader<>nil) then
   FPSShader.EnumUnifLayout(@FUniformBuilder.AddAttr,FPSShader.FDescSetId,@GPU_REGS.SPI.PS.USER_DATA);

 if (Length(FUniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FImages) do
  With FUniformBuilder.FImages[i] do
  begin

   ri:=FetchImage(FCmdBuffer,
                  FImage,
                  ord(VK_IMAGE_USAGE_SAMPLED_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                  TM_READ
                  );

   iv:=ri.FetchView(FCmdBuffer,FView);

   {

   FCmdBuffer.PushImageBarrier(ri.FHandle,
                               iv.GetSubresRange,
                               ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

   {//
   range:=iv.GetSubresRange;
   clr.float32[0]:=1;
   clr.float32[1]:=1;
   clr.float32[2]:=1;
   clr.float32[3]:=1;

   vkCmdClearColorImage(FCmdBuffer.cmdbuf,
                        ri.FHandle,
                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                        @clr,
                        1,@range);
   //}


   //

   buf:=FetchHostBuffer(FCmdBuffer,
                        ri.key.Addr,
                        ri.key.params.extend.width*ri.key.params.extend.height*4,
                        ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT));

   BufferImageCopy:=Default(TVkBufferImageCopy);

   BufferImageCopy.bufferOffset:=buf.Foffset;
   BufferImageCopy.bufferRowLength:=0;
   BufferImageCopy.bufferImageHeight:=0;
   BufferImageCopy.imageSubresource:=ri.GetSubresLayer;
   //BufferImageCopy.imageOffset:TVkOffset3D; //0
   BufferImageCopy.imageExtent.Create(ri.key.params.extend.width,
                                      ri.key.params.extend.height,
                                      ri.key.params.extend.depth);

   vkCmdCopyBufferToImage(FCmdBuffer.cmdbuf,
                          buf.FHandle,
                          ri.FHandle,
                          VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                          1,
                          @BufferImageCopy);

   //

   }

   FCmdBuffer.PushImageBarrier(ri.FHandle,
                               iv.GetSubresRange,
                               ord(VK_ACCESS_SHADER_READ_BIT),
                               VK_IMAGE_LAYOUT_GENERAL,
                               ord(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                               ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) );

  end;
 end;

 if (Event<>nil) then
 vkCmdWaitEvent(FCmdBuffer.cmdbuf,Event.FHandle,
                 ord(VK_PIPELINE_STAGE_TRANSFER_BIT),
                 ord(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT)
                );

 if not FCmdBuffer.BeginRenderPass(FRenderCmd) then
  Writeln('!BeginRenderPass');

 if (FVSShader.FPushConst.size<>0) then
 begin
  pData:=FVSShader.GetPushConstData(@GPU_REGS.SPI.VS.USER_DATA);

  Assert(pData<>nil);

  FCmdBuffer.PushConstant(VK_PIPELINE_BIND_POINT_GRAPHICS,
                          ord(VK_SHADER_STAGE_VERTEX_BIT),
                          0,FVSShader.FPushConst.size,
                          pData);

 end;

 if (FPSShader<>nil) then
 if (FPSShader.FPushConst.size<>0) then
 begin
  pData:=FPSShader.GetPushConstData(@GPU_REGS.SPI.PS.USER_DATA);

  //Assert(pData<>nil);

  if (pData<>nil) then
  FCmdBuffer.PushConstant(VK_PIPELINE_BIND_POINT_GRAPHICS,
                          ord(VK_SHADER_STAGE_FRAGMENT_BIT),
                          0,FPSShader.FPushConst.size,
                          pData);
 end;

 if (Length(FAttrBuilder.FBindExt)<>0) then
 begin
  For i:=0 to High(FAttrBuilder.FBindExt) do
  With FAttrBuilder.FBindExt[i] do
  begin
   buf:=FetchHostBuffer(FCmdBuffer,min_addr,stride*count,ord(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT));

   vkCmdBindVertexBuffer(FCmdBuffer.cmdbuf,
                         binding,
                         buf.FHandle,
                         buf.Foffset);

  end;
 end;

 //

 if (Length(FUniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FImages) do
  With FUniformBuilder.FImages[i] do
  begin

   ri:=FetchImage(FCmdBuffer,
                  FImage,
                  ord(VK_IMAGE_USAGE_SAMPLED_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                  TM_READ
                  );

   iv:=ri.FetchView(FCmdBuffer,FView);

   ri.data_usage:=ri.data_usage and (not TM_READ); ////////

   FDescriptorGroup.FSets[fset].BindImg(bind,0,
                                        iv.FHandle,
                                        VK_IMAGE_LAYOUT_GENERAL);


  end;
 end;
 //

 if (Length(FUniformBuilder.FSamplers)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FSamplers) do
  With FUniformBuilder.FSamplers[i] do
  begin
   sm:=FetchSampler(FCmdBuffer,PS);

   FDescriptorGroup.FSets[fset].BindSmp(bind,0,sm.FHandle);

  end;
 end;

 //
 if (Length(FUniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FBuffers) do
  With FUniformBuilder.FBuffers[i] do
  begin

   buf:=FetchHostBuffer(FCmdBuffer,addr,size,ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT));

   //Writeln('AlignDw:',addr-AlignDw(addr,256));

   //pData:=AlignDw(addr,256)+AlignDw(buf.Foffset,$10)+4;

   //For o:=0 to 3 do
   //begin
   // PSingle(pData)[o*8+0]:=PSingle(pData)[o*8+0];
   // Write(PSingle(pData)[o*8+0]:0:4,' ');
   // Write(PSingle(pData)[o*8+1]:0:4,' ');
   // Write(PSingle(pData)[o*8+2]:0:4,' ');
   // Write(PSingle(pData)[o*8+3]:0:4,' ');
   // writeln;
   //end;

   //For o:=0 to 5 do
   //begin
   // Writeln(PSingle(addr)[o]);
   //end;

   o:=buf.Foffset;

   a:=o-AlignDw(o,limits.minStorageBufferOffsetAlignment);
   //Writeln('align_offset=',a);
   if (a<>offset) then Assert(false);

   o:=AlignDw(o,limits.minStorageBufferOffsetAlignment{ $10});    //minStorageBufferOffsetAlignment
   //o:=o+$10;


   FDescriptorGroup.FSets[fset].BindBuf(bind,0,
                                        VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                        buf.FHandle,
                                        o,
                                        VK_WHOLE_SIZE);

   {
   vkCmdBindSB(FCmdBuffer.cmdbuf,
               VK_PIPELINE_BIND_POINT_GRAPHICS,
               FRenderCmd.FPipeline.FLayout.FHandle,
               fset,bind,0,
               buf.FHostBuf.FHandle,
               o,
               size);
   }

  end;
 end;

 FCmdBuffer.BindSets(VK_PIPELINE_BIND_POINT_GRAPHICS,FDescriptorGroup);

 {
 FCmdBuffer.EndRenderPass;

 if GPU_REGS.COMP_ENABLE then
 For i:=0 to 7 do
 if GPU_REGS.RT_ENABLE(i) then
  begin
   RT_INFO:=GPU_REGS.GET_RT_INFO(i);

   ri:=FetchImage(FCmdBuffer,
                  RT_INFO.FImageInfo,
                  ord(VK_IMAGE_USAGE_SAMPLED_BIT) or
                  ord(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT)
                  );

   iv:=ri.FetchView(FCmdBuffer,RT_INFO.FImageView);

   FCmdBuffer.PushImageBarrier(ri.FHandle,
                               iv.GetSubresRange,
                               ord(VK_ACCESS_TRANSFER_READ_BIT),
                               VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

   buf:=FetchHostBuffer(FCmdBuffer,
                        ri.key.Addr,
                        ri.key.params.extend.width*ri.key.params.extend.height*4,
                        ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT));

   BufferImageCopy:=Default(TVkBufferImageCopy);

   BufferImageCopy.bufferOffset:=buf.Foffset;
   BufferImageCopy.bufferRowLength:=0;
   BufferImageCopy.bufferImageHeight:=0;
   BufferImageCopy.imageSubresource:=ri.GetSubresLayer;
   //BufferImageCopy.imageOffset:TVkOffset3D; //0
   BufferImageCopy.imageExtent.Create(ri.key.params.extend.width,
                                      ri.key.params.extend.height,
                                      ri.key.params.extend.depth);

   vkCmdCopyImageToBuffer(FCmdBuffer.cmdbuf,
                          ri.FHandle,
                          VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                          buf.FHandle,
                          1,
                          @BufferImageCopy);

   FCmdBuffer.PushImageBarrier(ri.FHandle,
                               iv.GetSubresRange,
                               ord(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                               VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                               ord(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                               ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) );

  end;

 }

 //writeln;
end;

//var
 //FCSShader:TvShaderExt;

 //FComputePipeline:TvComputePipeline;

 //FShaderGroup2:TvShaderGroup;

procedure UpdateGpuRegsInfoCompute;
var
 fdump_cs:RawByteString;

 i,o,a:Integer;

 pData:Pointer;

 buf:TvHostBuffer;

 FDescriptorGroup:TvDescriptorGroup;

 FCSShader:TvShaderExt;

 FComputePipeline:TvComputePipeline;
begin

 {$ifdef null_rt}Exit;{$endif}

 fdump_cs:=DumpCS(GPU_REGS);

 InitVulkan;
 if (FCmdPool=nil) then
 begin
  FCmdPool:=TvCmdPool.Create(VulkanApp.FGFamily);
 end;

 if (FCmdBuffer=nil) then
 begin
  FCmdBuffer:=TvCmdBuffer.Create(FCmdPool,RenderQueue);
 end;

 FCmdBuffer.EndRenderPass;
 FCmdBuffer.BeginCmdBuffer;

 //if (FCSShader=nil) then
 //begin
 // FCSShader:=TvShaderExt.Create;
 // FCSShader.FDescSetId:=0;
 // FCSShader.LoadFromFile(ChangeFileExt(fdump_cs,'.spv'));
 //end;

 FCSShader:=FetchShader(vShaderStageCs,0,GPU_REGS);
 if (FCSShader=nil) then Exit;

 if (FShaderGroup=nil) then
 begin
  FShaderGroup:=TvShaderGroup.Create;
 end;

  FShaderGroup.Clear;
  FShaderGroup.SetCSShader(FCSShader);
  FShaderGroup.Compile;


  FComputePipeline:=TvComputePipeline.Create;
  FComputePipeline.SetLayout(FShaderGroup.FLayout);
  FComputePipeline.SetShader(FCSShader);
  FComputePipeline.Compile;

 FCmdBuffer.BindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,FComputePipeline.FHandle);
 FCmdBuffer.BindLayout(VK_PIPELINE_BIND_POINT_COMPUTE,FShaderGroup.FLayout);

 if (FCSShader.FPushConst.size<>0) then
 begin
  pData:=FCSShader.GetPushConstData(@GPU_REGS.SPI.CS.USER_DATA);

  //PDWORD(pData)[1]:=$707F7070;

  //Writeln(HexStr(PQWORD(pData)^,16));

  FCmdBuffer.PushConstant(VK_PIPELINE_BIND_POINT_COMPUTE,
                          ord(VK_SHADER_STAGE_COMPUTE_BIT),
                          0,FCSShader.FPushConst.size,
                          pData);
 end;

 FUniformBuilder:=Default(TvUniformBuilder);
 FCSShader.EnumUnifLayout(@FUniformBuilder.AddAttr,FCSShader.FDescSetId,@GPU_REGS.SPI.CS.USER_DATA);

 FDescriptorGroup:=FetchDescriptorGroup(FCmdBuffer,FShaderGroup.FLayout);

 if (Length(FUniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FBuffers) do
  With FUniformBuilder.FBuffers[i] do
  begin

   buf:=FetchHostBuffer(FCmdBuffer,addr,size,ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT));

   {
   if (bind=0) then
   begin
    Writeln(HexStr(PDWORD(addr)^,8));
   end;
   }

   {
   if (bind=1) then
   begin
    //FillDWORD(addr^,size div 4,$FFBBBBBB);
    Writeln('CLEAR RT:',HexStr(addr));


    vkBufferMemoryBarrier(FCmdBuffer.cmdbuf,
                          buf.FHandle,
                          ord(VK_ACCESS_NONE_KHR),
                          ord(VK_ACCESS_SHADER_WRITE_BIT),
                          0,size,
                          ord(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                          ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT)
                          );

   end;
   }

   //Writeln(PInteger(addr)[0]);
   //Writeln(PByte(@PInteger(addr)[1])[0]);
   //Writeln(PByte(@PInteger(addr)[1])[1]);
   //Writeln(PByte(@PInteger(addr)[1])[2]);
   //Writeln(PByte(@PInteger(addr)[1])[3]);

   o:=buf.Foffset;

   a:=o-AlignDw(o,limits.minStorageBufferOffsetAlignment);
   //Writeln('align_offset=',a);
   if (a<>offset) then Assert(false);

   o:=AlignDw(o,limits.minStorageBufferOffsetAlignment { $10});    //minStorageBufferOffsetAlignment


   FDescriptorGroup.FSets[fset].BindBuf(bind,0,
                                        VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                        buf.FHandle,
                                        o,
                                        VK_WHOLE_SIZE);

  end;
 end;

 FCmdBuffer.BindSets(VK_PIPELINE_BIND_POINT_COMPUTE,FDescriptorGroup);

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

 FCmdBuffer.DrawIndexAuto(GPU_REGS.VGT_DMA.INDICES);

 {$ifdef ww}Writeln('onDrawIndexAuto:',Body^.indexCount);{$endif}
end;

procedure onDispatchDirect(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDDISPATCHDIRECT);
begin

 UpdateGpuRegsInfoCompute;

 FCmdBuffer.DispatchDirect(Body^.dimX,Body^.dimY,Body^.dimZ);

 {$ifdef ww}Writeln('onDispatchDirect:',Body^.dimX,':',Body^.dimY,':',Body^.dimZ);{$endif}
end;

type
 PVGT_DMA_NUM_INSTANCES=^TVGT_DMA_NUM_INSTANCES;

procedure onNumInstances(pm4Hdr:PM4_TYPE_3_HEADER;Body:PVGT_DMA_NUM_INSTANCES);
begin
 GPU_REGS.VGT_DMA.NUM_INSTANCES:=Body^;
 {$ifdef ww}Writeln('onNumInstances:',Body^);{$endif}
end;

var
 FSubmitFlip:TqcFlipInfo;
 PSubmitFlip:PqcFlipInfo;

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

 //_set_trace_local_print(true);

 if (Flip<>nil) then
 begin
  Assert(PSubmitFlip=nil);
  FSubmitFlip:=Flip^;
  PSubmitFlip:=@FSubmitFlip;
 end;

 n:=0;
 While (n<count) do
 begin

  if (ccbGpuAddrs<>nil) and (ccbSizesInBytes<>nil) then
  begin
   Assert(ccbSizesInBytes[n]=0,'TODO CCB');
  end;

  i:=0;
  s:=dcbSizesInBytes[n];
  P:=PByte(dcbGpuAddrs[n]);
  While (i<s) do
  begin
   token:=PDWORD(P)^;

   case PM4_TYPE(token) of
    0:begin
       onPm40(PM4_TYPE_0_HEADER(token),@PDWORD(P)[1]);
      end;
    3:begin
       case PM4_TYPE_3_HEADER(token).opcode of
        IT_NOP:
        begin
         onNop(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_EVENT_WRITE_EOP:
        begin
         {$ifdef ww}Writeln('IT_EVENT_WRITE_EOP');{$endif}
         onEventWriteEop(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_EVENT_WRITE_EOS:
        begin
         {$ifdef ww}Writeln('IT_EVENT_WRITE_EOS');{$endif}
         onEventWriteEos(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_DMA_DATA:
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
         onWaitRegMem(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
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

       case PM4_TYPE_3_HEADER(token).opcode of
        IT_SET_CONTEXT_REG:;
        IT_SET_SH_REG     :;
        IT_SET_UCONFIG_REG:;
        else
         FLastSetReg:=0;
       end;

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

end;

procedure vSubmitDone;
begin

 FCmdBuffer.EndRenderPass;

 FCmdBuffer.BeginCmdBuffer;

 if (FCmdBuffer<>nil) then
 begin
  FCmdBuffer.Fence.Reset;
  //FCmdBuffer.QueueSubmit(ord(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT));
  FCmdBuffer.QueueSubmit({ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT)});
  FCmdBuffer.Fence.Wait(High(uint64));
  FCmdBuffer.ReleaseResource;
 end;

 //RenderQueue.QueueWaitIdle;

 if (FFlipLabel<>nil) then
 begin
  FFlipLabel^:=FFlipLData;
  FFlipLabel:=nil;
 end;

 _qc_sceVideoOutSubmitFlip(PSubmitFlip);
 PSubmitFlip:=nil;

 GPU_REGS.ClearDMA;
end;

initialization
 GPU_REGS.Clear;

end.

