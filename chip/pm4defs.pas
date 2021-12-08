unit pm4defs;

{$mode objfpc}{$H+}

interface

uses
  bittype,
  si_ci_vi_merged_registers;

const
 IT_NOP                                = $00000010;
 IT_SET_BASE                           = $00000011;
 IT_CLEAR_STATE                        = $00000012;
 IT_INDEX_BUFFER_SIZE                  = $00000013;
 IT_DISPATCH_DIRECT                    = $00000015;
 IT_DISPATCH_INDIRECT                  = $00000016;
 IT_INDIRECT_BUFFER_END                = $00000017;
 IT_INDIRECT_BUFFER_CNST_END           = $00000019;
 IT_ATOMIC_GDS                         = $0000001d;
 IT_ATOMIC_MEM                         = $0000001e;
 IT_OCCLUSION_QUERY                    = $0000001f;
 IT_SET_PREDICATION                    = $00000020;
 IT_REG_RMW                            = $00000021;
 IT_COND_EXEC                          = $00000022;
 IT_PRED_EXEC                          = $00000023;
 IT_DRAW_INDIRECT                      = $00000024;
 IT_DRAW_INDEX_INDIRECT                = $00000025;
 IT_INDEX_BASE                         = $00000026;
 IT_DRAW_INDEX_2                       = $00000027;
 IT_CONTEXT_CONTROL                    = $00000028;
 IT_INDEX_TYPE                         = $0000002a;
 IT_DRAW_INDIRECT_MULTI                = $0000002c;
 IT_DRAW_INDEX_AUTO                    = $0000002d;
 IT_NUM_INSTANCES                      = $0000002f;
 IT_DRAW_INDEX_MULTI_AUTO              = $00000030;
 IT_INDIRECT_BUFFER_PRIV               = $00000032;
 IT_INDIRECT_BUFFER_CNST               = $00000033;
 IT_COND_INDIRECT_BUFFER_CNST          = $00000033;
 IT_STRMOUT_BUFFER_UPDATE              = $00000034;
 IT_DRAW_INDEX_OFFSET_2                = $00000035;
 IT_DRAW_PREAMBLE                      = $00000036;
 IT_WRITE_DATA                         = $00000037;
 IT_DRAW_INDEX_INDIRECT_MULTI          = $00000038;
 IT_MEM_SEMAPHORE                      = $00000039;
 IT_DRAW_INDEX_MULTI_INST              = $0000003a;
 IT_COPY_DW                            = $0000003b;
 IT_WAIT_REG_MEM                       = $0000003c;
 IT_INDIRECT_BUFFER                    = $0000003f;
 IT_COND_INDIRECT_BUFFER               = $0000003f;
 IT_COPY_DATA                          = $00000040;
 IT_CP_DMA                             = $00000041;
 IT_PFP_SYNC_ME                        = $00000042;
 IT_SURFACE_SYNC                       = $00000043;
 IT_ME_INITIALIZE                      = $00000044;
 IT_COND_WRITE                         = $00000045;
 IT_EVENT_WRITE                        = $00000046;
 IT_EVENT_WRITE_EOP                    = $00000047;
 IT_EVENT_WRITE_EOS                    = $00000048;
 IT_RELEASE_MEM                        = $00000049;
 IT_PREAMBLE_CNTL                      = $0000004a;
 IT_DRAW_RESERVED0                     = $0000004c;
 IT_DRAW_RESERVED1                     = $0000004d;
 IT_DRAW_RESERVED2                     = $0000004e;
 IT_DRAW_RESERVED3                     = $0000004f;
 IT_DMA_DATA                           = $00000050;
 IT_CONTEXT_REG_RMW                    = $00000051;
 IT_GFX_CNTX_UPDATE                    = $00000052;
 IT_BLK_CNTX_UPDATE                    = $00000053;
 IT_INCR_UPDT_STATE                    = $00000055;
 IT_ACQUIRE_MEM                        = $00000058;
 IT_REWIND                             = $00000059;
 IT_INTERRUPT                          = $0000005a;
 IT_GEN_PDEPTE                         = $0000005b;
 IT_INDIRECT_BUFFER_PASID              = $0000005c;
 IT_PRIME_UTCL2                        = $0000005d;
 IT_LOAD_UCONFIG_REG                   = $0000005e;
 IT_LOAD_SH_REG                        = $0000005f;
 IT_LOAD_CONFIG_REG                    = $00000060;
 IT_LOAD_CONTEXT_REG                   = $00000061;
 IT_LOAD_COMPUTE_STATE                 = $00000062;
 IT_LOAD_SH_REG_INDEX                  = $00000063;
 IT_SET_CONFIG_REG                     = $00000068;
 IT_SET_CONTEXT_REG                    = $00000069;
 IT_SET_CONTEXT_REG_INDEX              = $0000006a;
 IT_SET_VGPR_REG_DI_MULTI              = $00000071;
 IT_SET_SH_REG_DI                      = $00000072;
 IT_SET_CONTEXT_REG_INDIRECT           = $00000073;
 IT_SET_SH_REG_DI_MULTI                = $00000074;
 IT_GFX_PIPE_LOCK                      = $00000075;
 IT_SET_SH_REG                         = $00000076;
 IT_SET_SH_REG_OFFSET                  = $00000077;
 IT_SET_QUEUE_REG                      = $00000078;
 IT_SET_UCONFIG_REG                    = $00000079;
 IT_SET_UCONFIG_REG_INDEX              = $0000007a;
 IT_FORWARD_HEADER                     = $0000007c;
 IT_SCRATCH_RAM_WRITE                  = $0000007d;
 IT_SCRATCH_RAM_READ                   = $0000007e;
 IT_LOAD_CONST_RAM                     = $00000080;
 IT_WRITE_CONST_RAM                    = $00000081;
 IT_DUMP_CONST_RAM                     = $00000083;
 IT_INCREMENT_CE_COUNTER               = $00000084;
 IT_INCREMENT_DE_COUNTER               = $00000085;
 IT_WAIT_ON_CE_COUNTER                 = $00000086;
 IT_WAIT_ON_DE_COUNTER_DIFF            = $00000088;
 IT_SWITCH_BUFFER                      = $0000008b;
 IT_FRAME_CONTROL                      = $00000090;
 IT_INDEX_ATTRIBUTES_INDIRECT          = $00000091;
 IT_WAIT_REG_MEM64                     = $00000093;
 IT_COND_PREEMPT                       = $00000094;
 IT_HDP_FLUSH                          = $00000095;
 IT_INVALIDATE_TLBS                    = $00000098;
 IT_DMA_DATA_FILL_MULTI                = $0000009a;
 IT_SET_SH_REG_INDEX                   = $0000009b;
 IT_DRAW_INDIRECT_COUNT_MULTI          = $0000009c;
 IT_DRAW_INDEX_INDIRECT_COUNT_MULTI    = $0000009d;
 IT_DUMP_CONST_RAM_OFFSET              = $0000009e;
 IT_LOAD_CONTEXT_REG_INDEX             = $0000009f;
 IT_SET_RESOURCES                      = $000000a0;
 IT_MAP_PROCESS                        = $000000a1;
 IT_MAP_QUEUES                         = $000000a2;
 IT_UNMAP_QUEUES                       = $000000a3;
 IT_QUERY_STATUS                       = $000000a4;
 IT_RUN_LIST                           = $000000a5;
 IT_MAP_PROCESS_VM                     = $000000a6;

 //OP_HINT_NOP=0;

 OP_HINT_1920_1080=$04380780;
 OP_HINT_1860_1080=$04380744;
 OP_HINT_320_240  =$00F00140;

 OP_HINT_WRITE_GPU_PREFETCH_INTO_L2               =$60000000;
 OP_HINT_BASE_ALLOCATE_FROM_COMMAND_BUFFER        =$68750000;
 OP_HINT_PUSH_MARKER                              =$68750001;
 OP_HINT_POP_MARKER                               =$68750002;
 OP_HINT_SET_VSHARP_IN_USER_DATA                  =$68750004;
 OP_HINT_SET_TSHARP_IN_USER_DATA                  =$68750005;
 OP_HINT_SET_SSHARP_IN_USER_DATA                  =$68750006;
 OP_HINT_SET_USER_DATA_REGION                     =$6875000D;
 OP_HINT_BASE_MARK_DISPATCH_DRAW_ACB_ADDRESS      =$68750012;
 OP_HINT_PREPARE_FLIP_VOID                        =$68750777;
 OP_HINT_PREPARE_FLIP_LABEL                       =$68750778;
 OP_HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_VOID     =$68750780;
 OP_HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_LABEL    =$68750781;
 OP_HINT_INLINE_DATA1                             =$68752000;
 OP_HINT_INLINE_DATA2                             =$68753000;

 OP_HINT_SET_DB_RENDER_CONTROL                    =$00000000;
 OP_HINT_SET_DB_COUNT_CONTROL                     =$00000001;
 OP_HINT_SET_RENDER_OVERRIDE_CONTROL              =$00000003;
 OP_HINT_SET_RENDER_OVERRIDE2CONTROL              =$00000004;
 OP_HINT_SET_PS_SHADER_SAMPLE_EXCLUSION_MASK      =$00000006;
 OP_HINT_SET_DEPTH_BOUNDS_RANGE                   =$00000008;
 OP_HINT_SET_STENCIL_CLEAR_VALUE                  =$0000000A;
 OP_HINT_SET_DEPTH_CLEAR_VALUE                    =$0000000B;
 OP_HINT_SET_SCREEN_SCISSOR                       =$0000000C;
 OP_HINT_SET_DEPTH_RENDER_TARGET                  =$00000010;
 OP_HINT_SET_BORDER_COLOR_TABLE_ADDR              =$00000020;
 OP_HINT_SET_WINDOW_OFFSET                        =$00000080;
 OP_HINT_SET_WINDOW_SCISSOR                       =$00000081;
 OP_HINT_SET_CLIP_RECTANGLE_RULE                  =$00000083;
 OP_HINT_SET_HARDWARE_SCREEN_OFFSET               =$0000008D;
 OP_HINT_SET_RENDER_TARGET_MASK                   =$0000008E;
 OP_HINT_SET_GENERIC_SCISSOR                      =$00000090;
 OP_HINT_SET_PERFMON_ENABLE                       =$000000D8;
 OP_HINT_SET_SCALED_RESOLUTION_GRID               =$000000E8;
 OP_HINT_SET_FOVEATED_WINDOW                      =$000000EB;
 OP_HINT_SET_RESET_FOVEATED_WINDOW                =$000000EB;
 OP_HINT_SET_INDEX_OFFSET                         =$00000102;
 OP_HINT_SET_PRIMITIVE_RESET_INDEX                =$00000103;
 OP_HINT_SET_STENCIL_OP_CONTROL                   =$0000010B;
 OP_HINT_SET_STENCIL                              =$0000010C;
 OP_HINT_SET_STENCIL_SEPARATE                     =$0000010C;
 OP_HINT_SET_PS_SHADER_USAGE                      =$00000191;
 OP_HINT_SET_GRAPHICS_SCRATCH_SIZE                =$000001BA;
 OP_HINT_SET_DEPTH_STENCIL_CONTROL                =$00000200;
 OP_HINT_SET_DEPTH_STENCIL_DISABLE                =$00000200;
 OP_HINT_SET_DEPTH_EQAA_CONTROL                   =$00000201;
 OP_HINT_SET_CB_CONTROL                           =$00000202;
 OP_HINT_SET_CLIP_CONTROL                         =$00000204;
 OP_HINT_SET_PRIMITIVE_SETUP                      =$00000205;
 OP_HINT_SET_VIEWPORT_TRANSFORM_CONTROL           =$00000206;
 OP_HINT_SET_OBJECT_ID_MODE                       =$0000020D;
 OP_HINT_SET_COMPUTE_SHADER_CONTROL               =$00000215;
 OP_HINT_SET_COMPUTE_SCRATCH_SIZE                 =$00000218;
 OP_HINT_SET_PRIMITIVE_TYPE_BASE                  =$00000242;
 OP_HINT_SET_POINT_SIZE                           =$00000280;
 OP_HINT_SET_POINT_MIN_MAX                        =$00000281;
 OP_HINT_SET_LINE_WIDTH                           =$00000282;
 OP_HINT_GS_MODE_ENABLE                           =$00000290;
 OP_HINT_SET_GS_MODE                              =$00000290;
 OP_HINT_GS_MODE_ENABLE_ON_CHIP                   =$00000291;
 OP_HINT_SET_GS_ON_CHIP_CONTROL                   =$00000291;
 OP_HINT_SET_SCAN_MODE_CONTROL                    =$00000292;
 OP_HINT_SET_PS_SHADER_RATE                       =$00000293;
 OP_HINT_SET_PRIMITIVE_ID_ENABLE                  =$000002A1;
 OP_HINT_SET_PRIMITIVE_RESET_INDEX_ENABLE         =$000002A5;
 OP_HINT_SET_DRAW_PAYLOAD_CONTROL                 =$000002A6;
 OP_HINT_SET_INSTANCE_STEP_RATE                   =$000002A8;
 OP_HINT_SETUP_ES_GS_RING_REGISTERS               =$000002AB;
 OP_HINT_SET_VERTEX_REUSE_ENABLE                  =$000002AD;
 OP_HINT_SET_HTILE_STENCIL0                       =$000002B0;
 OP_HINT_SET_HTILE_STENCIL1                       =$000002B1;
 OP_HINT_SETUP_DRAW_OPAQUE_PARAMETERS_1           =$000002CA;
 OP_HINT_SETUP_DRAW_OPAQUE_PARAMETERS_0           =$000002CC;
 OP_HINT_SET_TESSELLATION_DISTRIBUTION_THRESHOLDS =$000002D4;
 OP_HINT_SET_ACTIVE_SHADER_STAGES                 =$000002D5;
 OP_HINT_SETUP_GS_VS_RING_REGISTERS               =$000002D7;
 OP_HINT_SET_ALPHA_TO_MASK_CONTROL                =$000002DC;
 OP_HINT_SET_DISPATCH_DRAW_INDEX_DEALLOCATION_MASK=$000002DD;
 OP_HINT_SET_POLYGON_OFFSET_Z_FORMAT              =$000002DE;
 OP_HINT_SET_POLYGON_OFFSET_CLAMP                 =$000002DF;
 OP_HINT_SET_POLYGON_OFFSET_FRONT                 =$000002E0;
 OP_HINT_SET_POLYGON_OFFSET_BACK                  =$000002E2;
 OP_HINT_GS_MODE_DISABLE                          =$000002E5;
 OP_HINT_SET_GS_MODE_DISABLE                      =$000002E5;
 OP_HINT_SET_VS_SHADER_STREAMOUT_ENABLE           =$000002E5;
 OP_HINT_SET_STREAMOUT_MAPPING                    =$000002E6;
 OP_HINT_SET_AA_SAMPLE_COUNT                      =$000002F8;
 OP_HINT_SET_VERTEX_QUANTIZATION                  =$000002F9;
 OP_HINT_SET_GUARD_BANDS                          =$000002FA;
 OP_HINT_SET_AA_SAMPLE_MASK1                      =$0000030E;
 OP_HINT_SET_AA_SAMPLE_MASK2                      =$0000030F;
 OP_HINT_SET_TEXTURE_GRADIENT_FACTORS             =$00000382;
 OP_HINT_SET_PERF_COUNTER_CONTROL_PA              =$00001808;
 OP_HINT_SET_PRIMITIVE_TYPE_NEO                   =$10000242;


type
 PPM4_HEADER=^PM4_HEADER;
 PM4_HEADER=bitpacked record
  reserved:Word;  //16
  count:bit14;    //14
  _type:bit2;     //2
 end;

 PPM4_TYPE_0_HEADER=^PM4_TYPE_0_HEADER;
 PM4_TYPE_0_HEADER=bitpacked record
  baseIndex:Word; //16
  count:bit14;    //14
  _type:bit2;     //2
 end;

 PPM4_TYPE_3_HEADER=^PM4_TYPE_3_HEADER;
 PM4_TYPE_3_HEADER=bitpacked record
  predicate:bit1;  //1
  shaderType:bit1; //1
  reserved:bit6;   //6
  opcode:Byte;     //8
  count:bit14;     //14
  _type:bit2;      //2
 end;

 PPM4PrepareFlip=^TPM4PrepareFlip;
 TPM4PrepareFlip=packed record
  ADDRES_LO:DWORD;
  ADDRES_HI:DWORD;
  DATA:DWORD;
 end;

 PPM4PrepareFlipWithEopInterrupt=^TPM4PrepareFlipWithEopInterrupt;
 TPM4PrepareFlipWithEopInterrupt=packed record
  ADDRES_LO:DWORD;
  ADDRES_HI:DWORD;
  DATA:DWORD;
  eventType:DWORD;
  cacheAction:DWORD;
 end;

 PTPM4CMDWRITEDATA=^TPM4CMDWRITEDATA;
 TPM4CMDWRITEDATA=packed record
  CONTROL:bitpacked record
   reserved1  :bit8;
   dstSel     :bit4;  ///< destination select
   reserved2  :bit4;
   wrOneAddr  :bit1;  ///< Increment or not increment address
   reserved3  :bit3;
   wrConfirm  :bit1;  ///< Wait or not wait for confirmation
   reserved4  :bit3;
   atc        :bit1;
   cachePolicy:bit2;  ///< Cache olicy settings for write requests to the TCL2
   volatile   :bit1;  ///< Volatile setting for write requests to the TCL2
   reserved5  :bit2;
   engineSel  :bit2;  ///< engine select
  end;
  dstAddrLo:DWORD;
  dstAddrHi:DWORD;
  data:packed record end;
 end;

 PEVENTWRITEEOP=^TEVENTWRITEEOP;
 TEVENTWRITEEOP=packed record
  EVENT_CNTL:bitpacked record
   EVENT_TYPE:bit6;    //6  ///< event type written to VGT_EVENT_INITIATOR
   Reserved1:bit2;     //2
   EVENT_INDEX:bit4;   //4  ///< event index

   tcl1VolActionEna__CI:bit1; //1
   tcVolActionEna__CI  :bit1; //1
   reserved2:bit1;            //1
   tcWbActionEna__CI:bit1;    //1
   tcl1ActionEna__CI:bit1;    //1
   tcActionEna__CI:bit1;      //1

   reserved3:bit2;            //2
   invalidateL2__SI:bit1;     //1
   reserved4:bit3;            //3
   atc__CI:bit1;              //1
   cachePolicy__CI:bit2;      //2 ///< Cache Policy setting used for writing fences and timestamps to the TCL2
   volatile__CI:bit1;         //1 ///< Volatile setting used for writing fences and timestamps to the TCL2.
   reserved5:bit4;            //4
  end;
  ADDRESS_LO:DWORD;  ///< low bits of address
  DATA_CNTL:bitpacked record
   ADDRESS_HI:bit24;//24  ///< high bits of address
   INT_SEL:bit2;    //2   ///< selects interrupt action for end-of-pipe
   Reserved:bit3;   //3   ///< reserved
   DATA_SEL:bit3;   //3   ///< selects source of data
  end;
  DATA_LO:DWORD;   ///< value that will be written to memory when event occurs
  DATA_HI:DWORD;   ///< value that will be written to memory when event occurs
 end;

 PTPM4CMDEVENTWRITEEOS=^TPM4CMDEVENTWRITEEOS;
 TPM4CMDEVENTWRITEEOS=bitpacked record
  eventType  :bit6;    ///< event type written to VGT_EVENT_INITIATOR
  reserved1  :bit2;    ///< reserved
  eventIndex :bit4;    ///< event index
  reserved2  :bit20;   ///< reserved
  addressLo:DWORD;     ///< low bits of address, must be 4 byte aligned
  addressHi  :bit29;   ///< high bits of address
  command    :bit3;    ///< command
  Case byte of
   0:(
      gdsIndex:Word;   ///< indexed offset into GDS partition
      size    :Word;   ///< number of DWs to read from the GDS
   );
   1:(
      data:DWORD;      ///< fence value that will be written to memory when event occurs
   );
 end;

 PTPM4CMDEVENTWRITE=^TPM4CMDEVENTWRITE;
 TPM4CMDEVENTWRITE=bitpacked record
  eventType        :bit6;    ///< event type written to VGT_EVENT_INITIATOR
  reserved1        :bit2;    ///< reserved
  eventIndex       :bit4;    ///< event index
                             ///<      0000: Any non-Time Stamp/non-Fence/non-Trap EVENT_TYPE not listed.
                             ///<      0001: ZPASS_DONE
                             ///<      0010: SAMPLE_PIPELINESTATS
                             ///<      0011: SAMPLE_STREAMOUTSTAT[S|S1|S2|S3]
                             ///<      0100: [CS|VS|PS]_PARTIAL_FLUSH
                             ///<      0101: Reserved for EVENT_WRITE_EOP time stamp/fence event types
                             ///<      0110: Reserved for EVENT_WRITE_EOS packet
                             ///<      0111: CACHE_FLUSH, CACHE_FLUSH_AND_INV_EVENT
                             ///<      1000 - 1111: Reserved for future use.
  reserved2        :bit8;    ///< reserved
  invalidateL2     :bit1;    ///< Send WBINVL2 op to the TC L2 cache when eventIndex = 0111.
  reserved3        :bit3;
  ATC              :bit1;    ///< needed by Sample_PipelineStats (compute engine)
  reserved4        :bit6;    ///< reserved
  offload_enable   :bit1;    ///< Offload queue until EOP queue goes empty, only works for MEC.                                                ///< Setting this bit on graphics/ME will do nothing/be masked out.
 end;

 PTPM4DMADATA=^TPM4DMADATA;
 TPM4DMADATA=packed record

  Flags1:bitpacked record
   engine         :bit1;
   reserved1      :bit11;
   srcATC         :bit1;
   srcCachePolicy :bit2;
   srcVolatile    :bit1;
   reserved2      :bit4;
   dstSel         :bit2;
   reserved3      :bit2;
   dstATC         :bit1;
   dstCachePolicy :bit2;
   dstVolatile    :bit1;
   reserved4      :bit1;
   srcSel         :bit2;
   cpSync         :bit1;
  end;

  srcAddrLo:DWORD;
  srcAddrHi:DWORD;
  dstAddrLo:DWORD;
  dstAddrHi:DWORD;

  Flags2:bitpacked record
   byteCount :bit21;
   disWC     :bit1;
   srcSwap   :bit2;
   dstSwap   :bit2;
   sas       :bit1;
   das       :bit1;
   saic      :bit1;
   daic      :bit1;
   rawWait   :bit1;
   reserved5 :bit1;
  end;

 end;

 PPM4ACQUIREMEM=^TPM4ACQUIREMEM;
 TPM4ACQUIREMEM=bitpacked record
  coherCntl      :bit31;
  engine         :bit1;
  coherSize      :DWORD;
  coherSizeHi    :bit8;
  coherSizeHiRsvd:bit16;
  reserved1      :bit8;
  coherBaseLo    :DWORD;
  coherBaseHi    :bit24;
  reserved2      :bit8;
  pollInterval   :bit16;
  reserved3      :bit16;
 end;

 TCONTEXTCONTROLENABLE=bitpacked record
  enableSingleCntxConfigReg:bit1;   ///< single context config reg
  enableMultiCntxRenderReg :bit1;   ///< multi context render state reg
  reserved1                :bit13;  ///< reserved
  enableUserConfigReg      :bit1;   ///< User Config Reg on CI(reserved for SI)
  enableGfxSHReg           :bit1;   ///< Gfx SH Registers
  reserved2                :bit7;   ///< reserved
  enableCSSHReg            :bit1;   ///< CS SH Registers
  reserved3                :bit6;   ///< reserved
  enableDw                 :bit1;   ///< DW enable
 end;

 PPM4CMDCONTEXTCONTROL=^TPM4CMDCONTEXTCONTROL;
 TPM4CMDCONTEXTCONTROL=bitpacked record
  loadControl :TCONTEXTCONTROLENABLE; ///< enable bits for loading
  shadowEnable:TCONTEXTCONTROLENABLE; ///< enable bits for shadowing
 end;

 PPM4CMDCLEARSTATE=^DWORD;

 PPM4CMDSETDATA=^TPM4CMDSETDATA;
 TPM4CMDSETDATA=packed record
  REG_OFFSET:WORD;
  RESERVED:WORD;
  REG_DATA:packed record end;
 end;

 PPM4CMDDRAWINDEX2=^TPM4CMDDRAWINDEX2;
 TPM4CMDDRAWINDEX2=packed record
  maxSize:DWORD;      // VGT_DMA_MAX_SIZE
  indexBaseLo:DWORD;  // VGT_DMA_BASE
  indexBaseHi:DWORD;  // VGT_DMA_BASE_HI
  indexCount:DWORD;   // VGT_DMA_SIZE ,VGT_NUM_INDICES
  drawInitiator:TVGT_DRAW_INITIATOR;
 end;

 PPM4CMDDRAWINDEXAUTO=^TPM4CMDDRAWINDEXAUTO;
 TPM4CMDDRAWINDEXAUTO=packed record
  indexCount:DWORD;  ///< max index count
  drawInitiator:TVGT_DRAW_INITIATOR;
 end;

 PPM4CMDDISPATCHDIRECT=^TPM4CMDDISPATCHDIRECT;
 TPM4CMDDISPATCHDIRECT=packed record
  dimX:DWORD; ///< X dimensions of the array of thread groups to be dispatched
  dimY:DWORD; ///< Y dimensions of the array of thread groups to be dispatched
  dimZ:DWORD; ///< Z dimensions of the array of thread groups to be dispatched
  dispatchInitiator:TCOMPUTE_DISPATCH_INITIATOR; ///< Dispatch Initiator Register
 end;

function PM4_HEADER_BUILD(lenDw:WORD;op,priv:Byte):DWORD; inline;
function PM4_PRIV(token:DWORD):Byte; inline;
function PM4_TYPE(token:DWORD):Byte; inline;
function PM4_LENGTH_DW(token:DWORD):WORD; inline;

implementation

function PM4_HEADER_BUILD(lenDw:WORD;op,priv:Byte):DWORD; inline;
begin
 Result:=((lenDw shl 16)+$3FFE0000) or $C0000000 or
          (op shl 8) or priv;

end;

function PM4_PRIV(token:DWORD):Byte; inline;
begin
 Result:=Byte(token);
end;

function PM4_TYPE(token:DWORD):Byte; inline;
begin
 Result:=(token shr 30) and 3;
end;

function PM4_LENGTH_DW(token:DWORD):WORD; inline;
begin
 Result:=((token shr 16) and $3FFF) + 2;
end;

end.

