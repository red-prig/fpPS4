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

 OP_HINT_UPDATE_PS_DB_CONTROL                     =$c01e008f;
 OP_HINT_UPDATE_VS_OUT_CNTL                       =$c01e01b1;
 OP_HINT_UPDATE_PS_FORMAT                         =$c01e01b3;
 OP_HINT_UPDATE_PS_INPUT                          =$c01e01b6;
 OP_HINT_UPDATE_PS_IN_CONTROL                     =$c01e01b8;
 OP_HINT_UPDATE_VS_OUT_CONFIG                     =$c01e01c3;
 OP_HINT_UPDATE_PS_RSRC                           =$c01e01c4;
 OP_HINT_UPDATE_PS_BARY_CNTL                      =$c01e0203;
 OP_HINT_UPDATE_VS_RSRC                           =$c01e0207;
 OP_HINT_UPDATE_VS_POS_FORMAT                     =$c00a1000;

 OP_HINT_WRITE_GPU_PREFETCH_INTO_L2               =$60000000;
 OP_HINT_BASE_ALLOCATE_FROM_COMMAND_BUFFER        =$68750000;
 OP_HINT_PUSH_MARKER                              =$68750001;
 OP_HINT_POP_MARKER                               =$68750002;
 OP_HINT_SET_MARKER                               =$68750003;
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

 //per ring
 SH_REG_BASE = $2C00;
 SH_REG_END  = $3000;

 //8 context
 CONTEXT_REG_BASE = $A000;
 CONTEXT_REG_END  = $A400;

 //1 context
 CONFIG_SPACE_START=$2000;
 CONFIG_SPACE_END  =$BFFF;

 //1 context
 USERCONFIG_REG_BASE = $C000;
 USERCONFIG_REG_END  = $FFFF;

type
 PPM4_TYPE_0_HEADER=^PM4_TYPE_0_HEADER;
 PM4_TYPE_0_HEADER=bitpacked record
  baseIndex:Word;  //16
  count    :bit14; //14
  _type    :bit2;  //2
 end;

 PPM4_TYPE_2_HEADER=^PM4_TYPE_2_HEADER;
 PM4_TYPE_2_HEADER=bitpacked record
  reserved:bit30; //30
  _type   :bit2;  //2
 end;

 PPM4_TYPE_3_HEADER=^PM4_TYPE_3_HEADER;
 PM4_TYPE_3_HEADER=bitpacked record
  predicate :bit1;  //1
  shaderType:bit1;  //1 < 0: Graphics, 1: Compute Shader
  reserved  :bit6;  //6
  opcode    :Byte;  //8
  count     :bit14; //14
  _type     :bit2;  //2
 end;

 PPM4PrepareFlip=^TPM4PrepareFlip;
 TPM4PrepareFlip=packed record
  ADDRES:QWORD;
  DATA  :DWORD;
 end;

 PPM4PrepareFlipWithEopInterrupt=^TPM4PrepareFlipWithEopInterrupt;
 TPM4PrepareFlipWithEopInterrupt=packed record
  ADDRES     :QWORD;
  DATA       :DWORD;
  eventType  :DWORD;
  cacheAction:DWORD;
 end;

 PPM4CMDCONSTRAMLOAD=^PM4CMDCONSTRAMLOAD;
 PM4CMDCONSTRAMLOAD=bitpacked record
  header   :PM4_TYPE_3_HEADER;
  addr     :QWORD;
  numDwords:bit15; // < number of DWords to load (bits 2-0 must be 0)
  reserved1:bit17;
  offset   :bit16; // < Byte offset in the RAM, must be 32 byte aligned
  reserved2:bit16;
 end;

const
 // Valid values for PM4CMDDRAWSETBASE::baseIndex
 BASE_INDEX_DISPLAY_LIST     =$0000;
 BASE_INDEX_DRAW_INDIRECT    =$0001;
 BASE_INDEX_DISPATCH_INDIRECT=$0001;
 BASE_INDEX_LOAD_REG         =$0004; // Used by LOAD_SH/CONTEXT_REG_INDEX
 BASE_INDEX_INDIRECT_DATA    =$0005; // Used by SET_SH_REG_OFFSET index = 1

 // Valid values for PM4CMDDRAWSETBASE::baseIndex constant engine packet
 BASE_INDEX_CE_DST_BASE_ADDR =$0002;

type
 PPM4CMDDRAWSETBASE=^TPM4CMDDRAWSETBASE;
 TPM4CMDDRAWSETBASE=bitpacked record
  header   :PM4_TYPE_3_HEADER;
  baseIndex:bit4;  // < base index selector
  reserved1:bit28;
  address  :bit48; // < base address of buffer, must be 8 byte aligned
 end;

const
 SET_PRED_CLEAR    =0;
 SET_PRED_ZPASS    =1;
 SET_PRED_PRIMCOUNT=2;
 SET_PRED_MEM      =3;

type
 PPM4CMDSETPREDICATION=^TPM4CMDSETPREDICATION;
 TPM4CMDSETPREDICATION=bitpacked record
  header            :PM4_TYPE_3_HEADER;
  startAddress      :bit40; // < start address
  predicationBoolean:bit1;  // < predication boolean
  reserved1         :bit3;
  hint              :bit1;  // < hint
                            // < (only valid for Zpass/Occlusion Predicate)
  reserved2         :bit3;
  predOp            :bit3;  // < predicate operation
  reserved3         :bit12;
  continueBit       :bit1;  // < continue set predication
 end;

 PPM4CMDDRAWPREAMBLE=^TPM4CMDDRAWPREAMBLE;
 TPM4CMDDRAWPREAMBLE=bitpacked record
  header  :PM4_TYPE_3_HEADER;
  control1:TVGT_PRIMITIVE_TYPE; //< writes to VGT_PRIMITIVE_TYPE reg
  control2:TIA_MULTI_VGT_PARAM; //< writes to IA_MULTI_VGT_PARAM reg
  control3:TVGT_LS_HS_CONFIG;   //< writes to VGT_LS_HS_CONFIG   reg
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

type
 PPM4CMDWRITEDATA=^PM4CMDWRITEDATA;
 PM4CMDWRITEDATA=packed record
  header :PM4_TYPE_3_HEADER;
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
  dstAddr:QWORD;
  data:packed record end;
 end;

const
 // EVENT_WRITE_EOP packet definitions
 EVENTWRITEEOP_DATA_SEL_DISCARD            =0;
 EVENTWRITEEOP_DATA_SEL_SEND_DATA32        =1;
 EVENTWRITEEOP_DATA_SEL_SEND_DATA64        =2;
 EVENTWRITEEOP_DATA_SEL_SEND_GPU_CLOCK     =3; //system 100Mhz global clock.
 EVENTWRITEEOP_DATA_SEL_SEND_CP_PERFCOUNTER=4; //GPU 800Mhz clock.

 EVENTWRITEEOP_INT_SEL_NONE                =0;
 EVENTWRITEEOP_INT_SEL_SEND_INT            =1;
 EVENTWRITEEOP_INT_SEL_SEND_INT_ON_CONFIRM =2;
 EVENTWRITEEOP_INT_SEL_SEND_DATA_ON_CONFIRM=3;

 //event type
 kEopFlushCbDbCaches              = $00000004;  //end of read CB/DB, wait fence, label .....EOP
 kEopFlushAndInvalidateCbDbCaches = $00000014;
 kEopCbDbReadsDone                = $00000028;  //end read CB/DB, label .....EOP
 kEopCsDone                       = $00000028;  //wait cs shader, label .....EOP

type
 PPM4CMDEVENTWRITEEOP=^TPM4CMDEVENTWRITEEOP;
 TPM4CMDEVENTWRITEEOP=bitpacked record
  header          :PM4_TYPE_3_HEADER;

  eventType       :bit6;  //00 // < event type written to VGT_EVENT_INITIATOR
  Reserved1       :bit2;  //06
  eventIndex      :bit4;  //08 // < event index [0x5]

  tcL1VolActionEna:bit1;  //12 //(cacheAction & 0x3f) [0x00,0x10,0x33,0x38,0x3B]
  tcVolActionEna  :bit1;  //13 //(cacheAction & 0x3f)
  reserved2       :bit1;  //14 //(cacheAction & 0x3f)
  tcWbActionEna   :bit1;  //15 //(cacheAction & 0x3f)
  tcL1ActionEna   :bit1;  //16 //(cacheAction & 0x3f)
  tcActionEna     :bit1;  //17 //(cacheAction & 0x3f)

  reserved3       :bit2;  //18
  invalidateL2    :bit1;  //20
  reserved4       :bit3;  //21
  atc             :bit1;  //24

  cachePolicy     :bit2;  //25 // < Cache Policy setting used for writing fences and timestamps to the TCL2
                               //   (cachePolicy & 3)
  volatile        :bit1;  //27 // < Volatile setting used for writing fences and timestamps to the TCL2.
                               //   (dstSelector & 0x10) [kEventWriteDestTcL2Volatile]
  reserved5       :bit4;  //28

  address         :bit48; // < bits of address

  destTcL2        :bit1;  //16  // < (dstSelector & 1) [kEventWriteDestTcL2,kEventWriteDestTcL2Volatile]
  reserved6       :bit7;  //17  // < reserved
  intSel          :bit2;  //24  // < selects interrupt action for end-of-pipe (25 bit is eop) [INT_SEL_SEND_INT_ON_CONFIRM]
  reserved7       :bit3;  //26  // < reserved
  dataSel         :bit3;  //29  // < selects source of data (srcSelector)

  DATA            :QWORD; // < value that will be written to memory when event occurs
 end;

const
 EVENT_WRITE_EOS_CMD_STORE_APPEND_COUNT_TO_MEMORY=0;
 EVENT_WRITE_EOS_CMD_STORE_GDS_DATA_TO_MEMORY    =1;
 EVENT_WRITE_EOS_CMD_STORE_32BIT_DATA_TO_MEMORY  =2;

type
 PPM4CMDEVENTWRITEEOS=^PM4CMDEVENTWRITEEOS;
 PM4CMDEVENTWRITEEOS=bitpacked record
  header     :PM4_TYPE_3_HEADER;
  eventType  :bit6;    ///< event type written to VGT_EVENT_INITIATOR (CS_DONE, PS_DONE)
  reserved1  :bit2;    ///< reserved
  eventIndex :bit4;    ///< event index (EVENT_WRITE_INDEX_ANY_EOS_TIMESTAMP)
  reserved2  :bit20;   ///< reserved
  address    :bit61;   ///< bits of address, must be 4 byte aligned
  command    :bit3;    ///< command (EVENT_WRITE_EOS_CMD_*)
  Case byte of
   0:(
      gdsIndex:Word;   ///< indexed offset into GDS partition
      size    :Word;   ///< number of DWs to read from the GDS
   );
   1:(
      data:DWORD;      ///< fence value that will be written to memory when event occurs
   );
 end;

const
 EVENT_WRITE_INDEX_ANY_NON_TIMESTAMP     = 0;
 EVENT_WRITE_INDEX_ZPASS_DONE            = 1;
 EVENT_WRITE_INDEX_SAMPLE_PIPELINESTAT   = 2;
 EVENT_WRITE_INDEX_SAMPLE_STREAMOUTSTATS = 3;
 EVENT_WRITE_INDEX_VS_PS_PARTIAL_FLUSH   = 4;
 EVENT_WRITE_INDEX_ANY_EOP_TIMESTAMP     = 5;
 EVENT_WRITE_INDEX_ANY_EOS_TIMESTAMP     = 6;
 EVENT_WRITE_INDEX_CACHE_FLUSH_EVENT     = 7;

type
 PTPM4CMDEVENTWRITE=^TPM4CMDEVENTWRITE;
 TPM4CMDEVENTWRITE=bitpacked record
  header           :PM4_TYPE_3_HEADER;
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
 kDmaDataDstMemoryUsingL2       = $3;
 kDmaDataDstRegister	        = $4; ///< Destination is a GPU register offset (auto-increment enabled for multi-register DMAs).
 kDmaDataDstRegisterNoIncrement = $C; ///< Destination is a GPU register offset (auto-increment disabled for multi-register DMAs).

const
 CP_DMA_ENGINE_ME  = 0;
 CP_DMA_ENGINE_PFP = 1;

 CPDMA_ADDR_SPACE_MEM = 0;
 CPDMA_ADDR_SPACE_REG = 1;

 //CPDMA_SRC_SEL
 CPDMA_SRC_SEL_SRC_ADDR          = 0;
 CPDMA_SRC_SEL_GDS               = 1;
 CPDMA_SRC_SEL_DATA              = 2;
 CPDMA_SRC_SEL_SRC_ADDR_USING_L2 = 3;

 //CPDMA_DST_SEL
 CPDMA_DST_SEL_DST_ADDR          = 0;
 CPDMA_DST_SEL_GDS               = 1;
 CPDMA_DST_SEL_DST_ADDR_USING_L2 = 3;

type
 PPM4DMADATA=^PM4DMADATA;
 PM4DMADATA=packed record
  header:PM4_TYPE_3_HEADER;

  Flags1:bitpacked record
   engine         :bit1;   //CP_DMA_ENGINE_PFP, CP_DMA_ENGINE_ME
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
   cpSync         :bit1;   //Synchronize the transfer (isBlocking)
  end;

  srcAddr:QWORD;
  dstAddr:QWORD;

  Flags2:bitpacked record
   byteCount :bit21;      //Number of bytes to copy
   disWC     :bit1;       //disable write-confirm
   srcSwap   :bit2;
   dstSwap   :bit2;
   sas       :bit1;       //CPDMA_ADDR_SPACE_MEM, CPDMA_ADDR_SPACE_REG
   das       :bit1;       //CPDMA_ADDR_SPACE_MEM, CPDMA_ADDR_SPACE_REG
   saic      :bit1;
   daic      :bit1;
   rawWait   :bit1;
   reserved5 :bit1;
  end;

 end;

 PPM4ACQUIREMEM=^TPM4ACQUIREMEM;
 TPM4ACQUIREMEM=bitpacked record
  header         :PM4_TYPE_3_HEADER;
  coherCntl      :bit31;
  engine         :bit1;
  coherSizeLo    :DWORD;
  coherSizeHi    :bit8;
  coherSizeHiRsvd:bit16;
  reserved1      :bit8;
  coherBaseLo    :DWORD;
  coherBaseHi    :bit24;
  reserved2      :bit8;
  pollInterval   :bit16;
  reserved3      :bit16;
 end;

const
 // WAIT_REG_MEM space and function definitions
 WAIT_REG_MEM_SPACE_REGISTER    =0;
 WAIT_REG_MEM_SPACE_MEMORY      =1;
 WAIT_REG_MEM_SPACE_TCL2__CI    =2;

 WAIT_REG_MEM_FUNC_ALWAYS       =0;
 WAIT_REG_MEM_FUNC_LESS         =1;
 WAIT_REG_MEM_FUNC_LESS_EQUAL   =2;
 WAIT_REG_MEM_FUNC_EQUAL        =3;
 WAIT_REG_MEM_FUNC_NOT_EQUAL    =4;
 WAIT_REG_MEM_FUNC_GREATER_EQUAL=5;
 WAIT_REG_MEM_FUNC_GREATER      =6;

 WAIT_REG_MEM_ENGINE_ME         =0;
 WAIT_REG_MEM_ENGINE_PFP        =1;
 WAIT_REG_MEM_ENGINE_CE         =2;

{
 StallCommandBufferParser:
  e=1   (PFP)
  op=00
  ms=00  (0=reg 1=mem)
  r=0
  f=101 (WAIT_REG_MEM_FUNC_GREATER_EQUAL)
}

type
 PPM4CMDWAITREGMEM=^PM4CMDWAITREGMEM;
 PM4CMDWAITREGMEM=bitpacked record
  header          :PM4_TYPE_3_HEADER;
  compareFunc     :bit3;  ///< function. WAIT_REG_MEM_FUNC_XXXX
  reserved1       :bit1;  ///< reserved
  memSpace        :bit2;  ///< memory space (0 = register, 1 = memory, 2=TC/L2, 3 = reserved)
  operation       :bit2;  ///< operation:
                          ///<    00: WAIT_REG_MEM - Wait on Masked Register/Memory value to equal reference value.
                          ///<    01: WR_WAIT_WR_REG (PFP only)
                          ///<            Writes REFERENCE value to POLL_ADDRESS_LO
                          ///<            Waits for REFERENCE = POLL_ADDRESS_HI
                          ///<            Write REFERENCE to POLL_ADDRESS_HI.
  engine          :bit2;  ///< 0 = ME, 1 = PFP, 2 = CE
  uncached        :bit1;  ///< When set the memory read will always use MTYPE 3 (uncached)
                          ///  Only applies when executed on MEC (ACE).
                          ///  WAIT_REG_MEM on PFP or ME are always uncached.
  reserved2       :bit13; ///< reserved
  atc             :bit1;  ///< ATC steting for MC read transactions
  cachePolicy     :bit2;  ///< Reserved for future use of CACHE_POLICY
  volatile        :bit1;  ///< Reserved for future use of VOLATILE
  reserved3       :bit4;  ///< reserved

  pollAddress     :QWORD; ///< Address to poll or register offset
  reference       :DWORD; ///< reference value
  mask            :DWORD; ///< mask for comparison
  pollInterval    :DWORD; ///< interval to wait when issuing new poll requests
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
  header      :PM4_TYPE_3_HEADER;
  loadControl :TCONTEXTCONTROLENABLE; ///< enable bits for loading
  shadowEnable:TCONTEXTCONTROLENABLE; ///< enable bits for shadowing
 end;

 PPM4CMDCLEARSTATE=^DWORD;

 PPM4CMDSETDATA=^TPM4CMDSETDATA;
 TPM4CMDSETDATA=packed record
  header    :PM4_TYPE_3_HEADER;
  REG_OFFSET:WORD;
  RESERVED  :WORD;
  REG_DATA  :packed record end;
 end;

 PPM4CMDDRAWINDEXBUFFERSIZE=^TPM4CMDDRAWINDEXBUFFERSIZE;
 TPM4CMDDRAWINDEXBUFFERSIZE=packed record
  header    :PM4_TYPE_3_HEADER;
  numIndices:DWORD;
 end;

 PPM4CMDDRAWINDEXTYPE=^TPM4CMDDRAWINDEXTYPE;
 TPM4CMDDRAWINDEXTYPE=packed record
  header   :PM4_TYPE_3_HEADER;
  indexType:bit2;  // < select 16 Vs 32bit index
  swapMode :bit2;  // < DMA swap mode
  reserved :bit28;
 end;

 PPM4CMDDRAWINDEX2=^TPM4CMDDRAWINDEX2;
 TPM4CMDDRAWINDEX2=packed record
  header       :PM4_TYPE_3_HEADER;
  maxSize      :DWORD;  // VGT_DMA_MAX_SIZE
  indexBaseLo  :DWORD;  // VGT_DMA_BASE
  indexBaseHi  :DWORD;  // VGT_DMA_BASE_HI
  indexCount   :DWORD;  // VGT_DMA_SIZE ,VGT_NUM_INDICES
  drawInitiator:TVGT_DRAW_INITIATOR;
 end;

 PPM4CMDDRAWINDEXAUTO=^TPM4CMDDRAWINDEXAUTO;
 TPM4CMDDRAWINDEXAUTO=packed record
  header       :PM4_TYPE_3_HEADER;
  indexCount   :DWORD;  ///< max index count
  drawInitiator:TVGT_DRAW_INITIATOR;
 end;

 PPM4CMDDRAWINDEXBASE=^TPM4CMDDRAWINDEXBASE;
 TPM4CMDDRAWINDEXBASE=bitpacked record
  header     :PM4_TYPE_3_HEADER;
  indexBaseLo:DWORD; ///< Base Address Lo of index buffer, must be 2 byte aligned
  indexBaseHi:Word;  ///< Base Address Hi of index buffer
  reserved1  :bit14;
  baseSelect :bit2;  ///< Base Address select mode
 end;

 PPM4CMDDRAWNUMINSTANCES=^TPM4CMDDRAWNUMINSTANCES;
 TPM4CMDDRAWNUMINSTANCES=packed record
  header      :PM4_TYPE_3_HEADER;
  numInstances:DWORD;
 end;

 PPM4CMDDRAWINDEXOFFSET2=^TPM4CMDDRAWINDEXOFFSET2;
 TPM4CMDDRAWINDEXOFFSET2=packed record
  header       :PM4_TYPE_3_HEADER;
  maxSize      :DWORD; ///< maximum number of indices
  indexOffset  :DWORD; ///< zero based starting index number
  indexCount   :DWORD; ///< number of indices in the Index Buffer
  drawInitiator:TVGT_DRAW_INITIATOR;
 end;

 PPM4CMDDISPATCHDIRECT=^TPM4CMDDISPATCHDIRECT;
 TPM4CMDDISPATCHDIRECT=packed record
  header           :PM4_TYPE_3_HEADER;
  dimX             :DWORD;                       ///< X dimensions of the array of thread groups to be dispatched
  dimY             :DWORD;                       ///< Y dimensions of the array of thread groups to be dispatched
  dimZ             :DWORD;                       ///< Z dimensions of the array of thread groups to be dispatched
  dispatchInitiator:TCOMPUTE_DISPATCH_INITIATOR; ///< Dispatch Initiator Register
 end;

 TUSERCONFIG_REG_SHORT=packed record
  CP_COHER_BASE_HI  :TCP_COHER_BASE_HI;   // 0xC079
  CP_COHER_CNTL     :TCP_COHER_CNTL;      // 0xC07C
  CP_COHER_SIZE     :TCP_COHER_SIZE;      // 0xC07D
  CP_COHER_BASE     :TCP_COHER_BASE;      // 0xC07E
  CP_COHER_SIZE_HI  :TCP_COHER_SIZE_HI;   // 0xC08C
  GRBM_GFX_INDEX    :TGRBM_GFX_INDEX;     // 0xC200
  VGT_ESGS_RING_SIZE:TVGT_ESGS_RING_SIZE; // 0xC240
  VGT_GSVS_RING_SIZE:TVGT_GSVS_RING_SIZE; // 0xC241
  VGT_PRIMITIVE_TYPE:TVGT_PRIMITIVE_TYPE; // 0xC242
  VGT_INDEX_TYPE    :TVGT_INDEX_TYPE;     // 0xC243
  VGT_NUM_INDICES   :TVGT_NUM_INDICES;    // 0xC24C
  VGT_NUM_INSTANCES :TVGT_NUM_INSTANCES;  // 0xC24D
 end;

function get_op_name(op:Byte):RawByteString;
function get_hint_name(op:DWORD):RawByteString;

function PM4_HEADER_BUILD(lenDw:WORD;op,priv:Byte):DWORD; inline;
function PM4_PRIV(token:DWORD):Byte; inline;
function PM4_TYPE(token:DWORD):Byte; inline;
function PM4_LENGTH_DW(token:DWORD):WORD; inline;
function PM4_LENGTH(token:DWORD):DWORD; inline;

implementation

function get_op_name(op:Byte):RawByteString;
begin
 case op of
  IT_NOP                            :Result:='NOP';
  IT_SET_BASE                       :Result:='SET_BASE';
  IT_CLEAR_STATE                    :Result:='CLEAR_STATE';
  IT_INDEX_BUFFER_SIZE              :Result:='INDEX_BUFFER_SIZE';
  IT_DISPATCH_DIRECT                :Result:='DISPATCH_DIRECT';
  IT_DISPATCH_INDIRECT              :Result:='DISPATCH_INDIRECT';
  IT_INDIRECT_BUFFER_END            :Result:='INDIRECT_BUFFER_END';
  IT_INDIRECT_BUFFER_CNST_END       :Result:='INDIRECT_BUFFER_CNST_END';
  IT_ATOMIC_GDS                     :Result:='ATOMIC_GDS';
  IT_ATOMIC_MEM                     :Result:='ATOMIC_MEM';
  IT_OCCLUSION_QUERY                :Result:='OCCLUSION_QUERY';
  IT_SET_PREDICATION                :Result:='SET_PREDICATION';
  IT_REG_RMW                        :Result:='REG_RMW';
  IT_COND_EXEC                      :Result:='COND_EXEC';
  IT_PRED_EXEC                      :Result:='PRED_EXEC';
  IT_DRAW_INDIRECT                  :Result:='DRAW_INDIRECT';
  IT_DRAW_INDEX_INDIRECT            :Result:='DRAW_INDEX_INDIRECT';
  IT_INDEX_BASE                     :Result:='INDEX_BASE';
  IT_DRAW_INDEX_2                   :Result:='DRAW_INDEX_2';
  IT_CONTEXT_CONTROL                :Result:='CONTEXT_CONTROL';
  IT_INDEX_TYPE                     :Result:='INDEX_TYPE';
  IT_DRAW_INDIRECT_MULTI            :Result:='DRAW_INDIRECT_MULTI';
  IT_DRAW_INDEX_AUTO                :Result:='DRAW_INDEX_AUTO';
  IT_NUM_INSTANCES                  :Result:='NUM_INSTANCES';
  IT_DRAW_INDEX_MULTI_AUTO          :Result:='DRAW_INDEX_MULTI_AUTO';
  IT_INDIRECT_BUFFER_PRIV           :Result:='INDIRECT_BUFFER_PRIV';
  IT_INDIRECT_BUFFER_CNST           :Result:='INDIRECT_BUFFER_CNST';
  IT_STRMOUT_BUFFER_UPDATE          :Result:='STRMOUT_BUFFER_UPDATE';
  IT_DRAW_INDEX_OFFSET_2            :Result:='DRAW_INDEX_OFFSET_2';
  IT_DRAW_PREAMBLE                  :Result:='DRAW_PREAMBLE';
  IT_WRITE_DATA                     :Result:='WRITE_DATA';
  IT_DRAW_INDEX_INDIRECT_MULTI      :Result:='DRAW_INDEX_INDIRECT_MULTI';
  IT_MEM_SEMAPHORE                  :Result:='MEM_SEMAPHORE';
  IT_DRAW_INDEX_MULTI_INST          :Result:='DRAW_INDEX_MULTI_INST';
  IT_COPY_DW                        :Result:='COPY_DW';
  IT_WAIT_REG_MEM                   :Result:='WAIT_REG_MEM';
  IT_INDIRECT_BUFFER                :Result:='INDIRECT_BUFFER';
  IT_COPY_DATA                      :Result:='COPY_DATA';
  IT_CP_DMA                         :Result:='CP_DMA';
  IT_PFP_SYNC_ME                    :Result:='PFP_SYNC_ME';
  IT_SURFACE_SYNC                   :Result:='SURFACE_SYNC';
  IT_ME_INITIALIZE                  :Result:='ME_INITIALIZE';
  IT_COND_WRITE                     :Result:='COND_WRITE';
  IT_EVENT_WRITE                    :Result:='EVENT_WRITE';
  IT_EVENT_WRITE_EOP                :Result:='EVENT_WRITE_EOP';
  IT_EVENT_WRITE_EOS                :Result:='EVENT_WRITE_EOS';
  IT_RELEASE_MEM                    :Result:='RELEASE_MEM';
  IT_PREAMBLE_CNTL                  :Result:='PREAMBLE_CNTL';
  IT_DRAW_RESERVED0                 :Result:='DRAW_RESERVED0';
  IT_DRAW_RESERVED1                 :Result:='DRAW_RESERVED1';
  IT_DRAW_RESERVED2                 :Result:='DRAW_RESERVED2';
  IT_DRAW_RESERVED3                 :Result:='DRAW_RESERVED3';
  IT_DMA_DATA                       :Result:='DMA_DATA';
  IT_CONTEXT_REG_RMW                :Result:='CONTEXT_REG_RMW';
  IT_GFX_CNTX_UPDATE                :Result:='GFX_CNTX_UPDATE';
  IT_BLK_CNTX_UPDATE                :Result:='BLK_CNTX_UPDATE';
  IT_INCR_UPDT_STATE                :Result:='INCR_UPDT_STATE';
  IT_ACQUIRE_MEM                    :Result:='ACQUIRE_MEM';
  IT_REWIND                         :Result:='REWIND';
  IT_INTERRUPT                      :Result:='INTERRUPT';
  IT_GEN_PDEPTE                     :Result:='GEN_PDEPTE';
  IT_INDIRECT_BUFFER_PASID          :Result:='INDIRECT_BUFFER_PASID';
  IT_PRIME_UTCL2                    :Result:='PRIME_UTCL2';
  IT_LOAD_UCONFIG_REG               :Result:='LOAD_UCONFIG_REG';
  IT_LOAD_SH_REG                    :Result:='LOAD_SH_REG';
  IT_LOAD_CONFIG_REG                :Result:='LOAD_CONFIG_REG';
  IT_LOAD_CONTEXT_REG               :Result:='LOAD_CONTEXT_REG';
  IT_LOAD_COMPUTE_STATE             :Result:='LOAD_COMPUTE_STATE';
  IT_LOAD_SH_REG_INDEX              :Result:='LOAD_SH_REG_INDEX';
  IT_SET_CONFIG_REG                 :Result:='SET_CONFIG_REG';
  IT_SET_CONTEXT_REG                :Result:='SET_CONTEXT_REG';
  IT_SET_CONTEXT_REG_INDEX          :Result:='SET_CONTEXT_REG_INDEX';
  IT_SET_VGPR_REG_DI_MULTI          :Result:='SET_VGPR_REG_DI_MULTI';
  IT_SET_SH_REG_DI                  :Result:='SET_SH_REG_DI';
  IT_SET_CONTEXT_REG_INDIRECT       :Result:='SET_CONTEXT_REG_INDIRECT';
  IT_SET_SH_REG_DI_MULTI            :Result:='SET_SH_REG_DI_MULTI';
  IT_GFX_PIPE_LOCK                  :Result:='GFX_PIPE_LOCK';
  IT_SET_SH_REG                     :Result:='SET_SH_REG';
  IT_SET_SH_REG_OFFSET              :Result:='SET_SH_REG_OFFSET';
  IT_SET_QUEUE_REG                  :Result:='SET_QUEUE_REG';
  IT_SET_UCONFIG_REG                :Result:='SET_UCONFIG_REG';
  IT_SET_UCONFIG_REG_INDEX          :Result:='SET_UCONFIG_REG_INDEX';
  IT_FORWARD_HEADER                 :Result:='FORWARD_HEADER';
  IT_SCRATCH_RAM_WRITE              :Result:='SCRATCH_RAM_WRITE';
  IT_SCRATCH_RAM_READ               :Result:='SCRATCH_RAM_READ';
  IT_LOAD_CONST_RAM                 :Result:='LOAD_CONST_RAM';
  IT_WRITE_CONST_RAM                :Result:='WRITE_CONST_RAM';
  IT_DUMP_CONST_RAM                 :Result:='DUMP_CONST_RAM';
  IT_INCREMENT_CE_COUNTER           :Result:='INCREMENT_CE_COUNTER';
  IT_INCREMENT_DE_COUNTER           :Result:='INCREMENT_DE_COUNTER';
  IT_WAIT_ON_CE_COUNTER             :Result:='WAIT_ON_CE_COUNTER';
  IT_WAIT_ON_DE_COUNTER_DIFF        :Result:='WAIT_ON_DE_COUNTER_DIFF';
  IT_SWITCH_BUFFER                  :Result:='SWITCH_BUFFER';
  IT_FRAME_CONTROL                  :Result:='FRAME_CONTROL';
  IT_INDEX_ATTRIBUTES_INDIRECT      :Result:='INDEX_ATTRIBUTES_INDIRECT';
  IT_WAIT_REG_MEM64                 :Result:='WAIT_REG_MEM64';
  IT_COND_PREEMPT                   :Result:='COND_PREEMPT';
  IT_HDP_FLUSH                      :Result:='HDP_FLUSH';
  IT_INVALIDATE_TLBS                :Result:='INVALIDATE_TLBS';
  IT_DMA_DATA_FILL_MULTI            :Result:='DMA_DATA_FILL_MULTI';
  IT_SET_SH_REG_INDEX               :Result:='SET_SH_REG_INDEX';
  IT_DRAW_INDIRECT_COUNT_MULTI      :Result:='DRAW_INDIRECT_COUNT_MULTI';
  IT_DRAW_INDEX_INDIRECT_COUNT_MULTI:Result:='DRAW_INDEX_INDIRECT_COUNT_MULTI';
  IT_DUMP_CONST_RAM_OFFSET          :Result:='DUMP_CONST_RAM_OFFSET';
  IT_LOAD_CONTEXT_REG_INDEX         :Result:='LOAD_CONTEXT_REG_INDEX';
  IT_SET_RESOURCES                  :Result:='SET_RESOURCES';
  IT_MAP_PROCESS                    :Result:='MAP_PROCESS';
  IT_MAP_QUEUES                     :Result:='MAP_QUEUES';
  IT_UNMAP_QUEUES                   :Result:='UNMAP_QUEUES';
  IT_QUERY_STATUS                   :Result:='QUERY_STATUS';
  IT_RUN_LIST                       :Result:='RUN_LIST';
  IT_MAP_PROCESS_VM                 :Result:='MAP_PROCESS_VM';
  else
   Result:='0x'+HexStr(op,2);
 end;
end;


function get_hint_name(op:DWORD):RawByteString;
begin
 case op of
  OP_HINT_UPDATE_PS_DB_CONTROL                     :Result:='UPDATE_PS_DB_CONTROL';
  OP_HINT_UPDATE_VS_OUT_CNTL                       :Result:='UPDATE_VS_OUT_CNTL';
  OP_HINT_UPDATE_PS_FORMAT                         :Result:='UPDATE_PS_FORMAT';
  OP_HINT_UPDATE_PS_INPUT                          :Result:='UPDATE_PS_INPUT';
  OP_HINT_UPDATE_PS_IN_CONTROL                     :Result:='UPDATE_PS_IN_CONTROL';
  OP_HINT_UPDATE_VS_OUT_CONFIG                     :Result:='UPDATE_VS_OUT_CONFIG';
  OP_HINT_UPDATE_PS_RSRC                           :Result:='UPDATE_PS_RSRC';
  OP_HINT_UPDATE_PS_BARY_CNTL                      :Result:='UPDATE_PS_BARY_CNTL';
  OP_HINT_UPDATE_VS_RSRC                           :Result:='UPDATE_VS_RSRC';
  OP_HINT_UPDATE_VS_POS_FORMAT                     :Result:='UPDATE_VS_POS_FORMAT';
  OP_HINT_WRITE_GPU_PREFETCH_INTO_L2               :Result:='WRITE_GPU_PREFETCH_INTO_L2';
  OP_HINT_BASE_ALLOCATE_FROM_COMMAND_BUFFER        :Result:='BASE_ALLOCATE_FROM_COMMAND_BUFFER';
  OP_HINT_PUSH_MARKER                              :Result:='PUSH_MARKER';
  OP_HINT_POP_MARKER                               :Result:='POP_MARKER';
  OP_HINT_SET_MARKER                               :Result:='SET_MARKER';
  OP_HINT_SET_VSHARP_IN_USER_DATA                  :Result:='SET_VSHARP_IN_USER_DATA';
  OP_HINT_SET_TSHARP_IN_USER_DATA                  :Result:='SET_TSHARP_IN_USER_DATA';
  OP_HINT_SET_SSHARP_IN_USER_DATA                  :Result:='SET_SSHARP_IN_USER_DATA';
  OP_HINT_SET_USER_DATA_REGION                     :Result:='SET_USER_DATA_REGION';
  OP_HINT_BASE_MARK_DISPATCH_DRAW_ACB_ADDRESS      :Result:='BASE_MARK_DISPATCH_DRAW_ACB_ADDRESS';
  OP_HINT_PREPARE_FLIP_VOID                        :Result:='PREPARE_FLIP_VOID';
  OP_HINT_PREPARE_FLIP_LABEL                       :Result:='PREPARE_FLIP_LABEL';
  OP_HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_VOID     :Result:='PREPARE_FLIP_WITH_EOP_INTERRUPT_VOID';
  OP_HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_LABEL    :Result:='PREPARE_FLIP_WITH_EOP_INTERRUPT_LABEL';
  OP_HINT_INLINE_DATA1                             :Result:='INLINE_DATA1';
  OP_HINT_INLINE_DATA2                             :Result:='INLINE_DATA2';
  OP_HINT_SET_DB_RENDER_CONTROL                    :Result:='SET_DB_RENDER_CONTROL';
  OP_HINT_SET_DB_COUNT_CONTROL                     :Result:='SET_DB_COUNT_CONTROL';
  OP_HINT_SET_RENDER_OVERRIDE_CONTROL              :Result:='SET_RENDER_OVERRIDE_CONTROL';
  OP_HINT_SET_RENDER_OVERRIDE2CONTROL              :Result:='SET_RENDER_OVERRIDE2CONTROL';
  OP_HINT_SET_PS_SHADER_SAMPLE_EXCLUSION_MASK      :Result:='SET_PS_SHADER_SAMPLE_EXCLUSION_MASK';
  OP_HINT_SET_DEPTH_BOUNDS_RANGE                   :Result:='SET_DEPTH_BOUNDS_RANGE';
  OP_HINT_SET_STENCIL_CLEAR_VALUE                  :Result:='SET_STENCIL_CLEAR_VALUE';
  OP_HINT_SET_DEPTH_CLEAR_VALUE                    :Result:='SET_DEPTH_CLEAR_VALUE';
  OP_HINT_SET_SCREEN_SCISSOR                       :Result:='SET_SCREEN_SCISSOR';
  OP_HINT_SET_DEPTH_RENDER_TARGET                  :Result:='SET_DEPTH_RENDER_TARGET';
  OP_HINT_SET_BORDER_COLOR_TABLE_ADDR              :Result:='SET_BORDER_COLOR_TABLE_ADDR';
  OP_HINT_SET_WINDOW_OFFSET                        :Result:='SET_WINDOW_OFFSET';
  OP_HINT_SET_WINDOW_SCISSOR                       :Result:='SET_WINDOW_SCISSOR';
  OP_HINT_SET_CLIP_RECTANGLE_RULE                  :Result:='SET_CLIP_RECTANGLE_RULE';
  OP_HINT_SET_HARDWARE_SCREEN_OFFSET               :Result:='SET_HARDWARE_SCREEN_OFFSET';
  OP_HINT_SET_RENDER_TARGET_MASK                   :Result:='SET_RENDER_TARGET_MASK';
  OP_HINT_SET_GENERIC_SCISSOR                      :Result:='SET_GENERIC_SCISSOR';
  OP_HINT_SET_PERFMON_ENABLE                       :Result:='SET_PERFMON_ENABLE';
  OP_HINT_SET_SCALED_RESOLUTION_GRID               :Result:='SET_SCALED_RESOLUTION_GRID';
  OP_HINT_SET_FOVEATED_WINDOW                      :Result:='SET_FOVEATED_WINDOW';
  OP_HINT_SET_INDEX_OFFSET                         :Result:='SET_INDEX_OFFSET';
  OP_HINT_SET_PRIMITIVE_RESET_INDEX                :Result:='SET_PRIMITIVE_RESET_INDEX';
  OP_HINT_SET_STENCIL_OP_CONTROL                   :Result:='SET_STENCIL_OP_CONTROL';
  OP_HINT_SET_STENCIL                              :Result:='SET_STENCIL';
  OP_HINT_SET_PS_SHADER_USAGE                      :Result:='SET_PS_SHADER_USAGE';
  OP_HINT_SET_GRAPHICS_SCRATCH_SIZE                :Result:='SET_GRAPHICS_SCRATCH_SIZE';
  OP_HINT_SET_DEPTH_STENCIL_CONTROL                :Result:='SET_DEPTH_STENCIL_CONTROL';
  OP_HINT_SET_DEPTH_EQAA_CONTROL                   :Result:='SET_DEPTH_EQAA_CONTROL';
  OP_HINT_SET_CB_CONTROL                           :Result:='SET_CB_CONTROL';
  OP_HINT_SET_CLIP_CONTROL                         :Result:='SET_CLIP_CONTROL';
  OP_HINT_SET_PRIMITIVE_SETUP                      :Result:='SET_PRIMITIVE_SETUP';
  OP_HINT_SET_VIEWPORT_TRANSFORM_CONTROL           :Result:='SET_VIEWPORT_TRANSFORM_CONTROL';
  OP_HINT_SET_OBJECT_ID_MODE                       :Result:='SET_OBJECT_ID_MODE';
  OP_HINT_SET_COMPUTE_SHADER_CONTROL               :Result:='SET_COMPUTE_SHADER_CONTROL';
  OP_HINT_SET_COMPUTE_SCRATCH_SIZE                 :Result:='SET_COMPUTE_SCRATCH_SIZE';
  OP_HINT_SET_PRIMITIVE_TYPE_BASE                  :Result:='SET_PRIMITIVE_TYPE_BASE';
  OP_HINT_SET_POINT_SIZE                           :Result:='SET_POINT_SIZE';
  OP_HINT_SET_POINT_MIN_MAX                        :Result:='SET_POINT_MIN_MAX';
  OP_HINT_SET_LINE_WIDTH                           :Result:='SET_LINE_WIDTH';
  OP_HINT_SET_GS_MODE                              :Result:='SET_GS_MODE';
  OP_HINT_SET_GS_ON_CHIP_CONTROL                   :Result:='SET_GS_ON_CHIP_CONTROL';
  OP_HINT_SET_SCAN_MODE_CONTROL                    :Result:='SET_SCAN_MODE_CONTROL';
  OP_HINT_SET_PS_SHADER_RATE                       :Result:='SET_PS_SHADER_RATE';
  OP_HINT_SET_PRIMITIVE_ID_ENABLE                  :Result:='SET_PRIMITIVE_ID_ENABLE';
  OP_HINT_SET_PRIMITIVE_RESET_INDEX_ENABLE         :Result:='SET_PRIMITIVE_RESET_INDEX_ENABLE';
  OP_HINT_SET_DRAW_PAYLOAD_CONTROL                 :Result:='SET_DRAW_PAYLOAD_CONTROL';
  OP_HINT_SET_INSTANCE_STEP_RATE                   :Result:='SET_INSTANCE_STEP_RATE';
  OP_HINT_SETUP_ES_GS_RING_REGISTERS               :Result:='SETUP_ES_GS_RING_REGISTERS';
  OP_HINT_SET_VERTEX_REUSE_ENABLE                  :Result:='SET_VERTEX_REUSE_ENABLE';
  OP_HINT_SET_HTILE_STENCIL0                       :Result:='SET_HTILE_STENCIL0';
  OP_HINT_SET_HTILE_STENCIL1                       :Result:='SET_HTILE_STENCIL1';
  OP_HINT_SETUP_DRAW_OPAQUE_PARAMETERS_1           :Result:='SETUP_DRAW_OPAQUE_PARAMETERS_1';
  OP_HINT_SETUP_DRAW_OPAQUE_PARAMETERS_0           :Result:='SETUP_DRAW_OPAQUE_PARAMETERS_0';
  OP_HINT_SET_TESSELLATION_DISTRIBUTION_THRESHOLDS :Result:='SET_TESSELLATION_DISTRIBUTION_THRESHOLDS';
  OP_HINT_SET_ACTIVE_SHADER_STAGES                 :Result:='SET_ACTIVE_SHADER_STAGES';
  OP_HINT_SETUP_GS_VS_RING_REGISTERS               :Result:='SETUP_GS_VS_RING_REGISTERS';
  OP_HINT_SET_ALPHA_TO_MASK_CONTROL                :Result:='SET_ALPHA_TO_MASK_CONTROL';
  OP_HINT_SET_DISPATCH_DRAW_INDEX_DEALLOCATION_MASK:Result:='SET_DISPATCH_DRAW_INDEX_DEALLOCATION_MASK';
  OP_HINT_SET_POLYGON_OFFSET_Z_FORMAT              :Result:='SET_POLYGON_OFFSET_Z_FORMAT';
  OP_HINT_SET_POLYGON_OFFSET_CLAMP                 :Result:='SET_POLYGON_OFFSET_CLAMP';
  OP_HINT_SET_POLYGON_OFFSET_FRONT                 :Result:='SET_POLYGON_OFFSET_FRONT';
  OP_HINT_SET_POLYGON_OFFSET_BACK                  :Result:='SET_POLYGON_OFFSET_BACK';
  OP_HINT_SET_GS_MODE_DISABLE                      :Result:='SET_GS_MODE_DISABLE';
  OP_HINT_SET_STREAMOUT_MAPPING                    :Result:='SET_STREAMOUT_MAPPING';
  OP_HINT_SET_AA_SAMPLE_COUNT                      :Result:='SET_AA_SAMPLE_COUNT';
  OP_HINT_SET_VERTEX_QUANTIZATION                  :Result:='SET_VERTEX_QUANTIZATION';
  OP_HINT_SET_GUARD_BANDS                          :Result:='SET_GUARD_BANDS';
  OP_HINT_SET_AA_SAMPLE_MASK1                      :Result:='SET_AA_SAMPLE_MASK1';
  OP_HINT_SET_AA_SAMPLE_MASK2                      :Result:='SET_AA_SAMPLE_MASK2';
  OP_HINT_SET_TEXTURE_GRADIENT_FACTORS             :Result:='SET_TEXTURE_GRADIENT_FACTORS';
  OP_HINT_SET_PERF_COUNTER_CONTROL_PA              :Result:='SET_PERF_COUNTER_CONTROL_PA';
  OP_HINT_SET_PRIMITIVE_TYPE_NEO                   :Result:='SET_PRIMITIVE_TYPE_NEO';
  else
   Result:='0x'+HexStr(op,8);
 end;
end;

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

function PM4_LENGTH(token:DWORD):DWORD; inline;
begin
 Result:=((token shr 14) and $FFFC) + 8;
end;

end.

