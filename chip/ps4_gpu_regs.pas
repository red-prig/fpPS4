unit ps4_gpu_regs;

{$mode objfpc}{$H+}

interface

uses
 Classes,
 SysUtils,
 vulkan,
 vImage,
 bittype,
 pm4defs,
 ps4_shader,
 si_ci_vi_merged_offset,
 si_ci_vi_merged_enum,
 si_ci_vi_merged_registers;

type
 TRENDER_TARGET=packed record
  BASE       :TCB_COLOR0_BASE       ; //mmCB_COLOR0_BASE_DEFAULT
  PITCH      :TCB_COLOR0_PITCH      ; //mmCB_COLOR0_PITCH_DEFAULT
  SLICE      :TCB_COLOR0_SLICE      ; //mmCB_COLOR0_SLICE_DEFAULT
  VIEW       :TCB_COLOR0_VIEW       ; //mmCB_COLOR0_VIEW_DEFAULT
  INFO       :TCB_COLOR0_INFO       ; //mmCB_COLOR0_INFO_DEFAULT
  ATTRIB     :TCB_COLOR0_ATTRIB     ; //mmCB_COLOR0_ATTRIB_DEFAULT
  DCC_CONTROL:TCB_COLOR0_DCC_CONTROL; //mmCB_COLOR0_DCC_CONTROL_DEFAULT
  CMASK      :TCB_COLOR0_CMASK      ; //mmCB_COLOR0_CMASK_DEFAULT
  CMASK_SLICE:TCB_COLOR0_CMASK_SLICE; //mmCB_COLOR0_CMASK_SLICE_DEFAULT
  FMASK      :TCB_COLOR0_FMASK      ; //mmCB_COLOR0_FMASK_DEFAULT
  FMASK_SLICE:TCB_COLOR0_FMASK_SLICE; //mmCB_COLOR0_FMASK_SLICE_DEFAULT
  CLEAR_WORD :QWORD;                  //mmCB_COLOR0_CLEAR_WORD0_DEFAULT
                                      //mmCB_COLOR0_CLEAR_WORD1_DEFAULT
  DCC_BASE   :TCB_COLOR0_DCC_BASE   ; //mmCB_COLOR0_DCC_BASE_DEFAULT
  align:DWORD;
 end;

 TGB_CLIP=packed record
  VERT_CLIP_ADJ:Single;
  VERT_DISC_ADJ:Single;
  HORZ_CLIP_ADJ:Single;
  HORZ_DISC_ADJ:Single;
 end;

 TVPORT_SCISSOR=packed record
  TL:TPA_SC_VPORT_SCISSOR_0_TL;
  BR:TPA_SC_VPORT_SCISSOR_0_BR;
 end;

 TVPORT_ZMIN_MAX=packed record
  ZMIN:Single;
  ZMAX:Single;
 end;

 TVPORT_SCALE_OFFSET=packed record
  XSCALE :Single;
  XOFFSET:Single;
  YSCALE :Single;
  YOFFSET:Single;
  ZSCALE :Single;
  ZOFFSET:Single;
 end;

 TSPI_USER_DATA=array[0..15] of DWORD;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_XY=bitpacked record
   S0_X:bit4;
   S0_Y:bit4;
   S1_X:bit4;
   S1_Y:bit4;
   S2_X:bit4;
   S2_Y:bit4;
   S3_X:bit4;
   S3_Y:bit4;
   S4_X:bit4;
   S4_Y:bit4;
   S5_X:bit4;
   S5_Y:bit4;
   S6_X:bit4;
   S6_Y:bit4;
   S7_X:bit4;
   S7_Y:bit4;
   S8_X:bit4;
   S8_Y:bit4;
   S9_X:bit4;
   S9_Y:bit4;
  S10_X:bit4;
  S10_Y:bit4;
  S11_X:bit4;
  S11_Y:bit4;
  S12_X:bit4;
  S12_Y:bit4;
  S13_X:bit4;
  S13_Y:bit4;
  S14_X:bit4;
  S14_Y:bit4;
  S15_X:bit4;
  S15_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL=bitpacked array[0..1,0..15] of bit4;

 TPA_SC_CENTROID_PRIORITY=bitpacked record
   DISTANCE_0:bit4;
   DISTANCE_1:bit4;
   DISTANCE_2:bit4;
   DISTANCE_3:bit4;
   DISTANCE_4:bit4;
   DISTANCE_5:bit4;
   DISTANCE_6:bit4;
   DISTANCE_7:bit4;
   DISTANCE_8:bit4;
   DISTANCE_9:bit4;
  DISTANCE_10:bit4;
  DISTANCE_11:bit4;
  DISTANCE_12:bit4;
  DISTANCE_13:bit4;
  DISTANCE_14:bit4;
  DISTANCE_15:bit4;
 end;

 TRT_INFO=record
  //Addr:Pointer;

  //extend:TVkExtent2D;
  padded:TVkExtent2D;

  //cformat:TVkFormat;
  //TILE_MODE_INDEX:DWORD;

  FImageInfo:TvImageKey;
  FImageView:TvImageViewKey;

  COMP_SWAP :Byte;
  //FAST_CLEAR:Boolean;

  IMAGE_USAGE:Byte;

  CLEAR_COLOR:TVkClearColorValue;

  blend:TVkPipelineColorBlendAttachmentState;
 end;

 TDB_INFO=record

  Z_READ_ADDR:Pointer;
  Z_WRITE_ADDR:Pointer;

  STENCIL_READ_ADDR:Pointer;
  STENCIL_WRITE_ADDR:Pointer;

  //extend:TVkExtent2D;
  padded:TVkExtent2D;

  //DEPTH_CLEAR   :Boolean;
  //STENCIL_CLEAR :Boolean;

  //Z_READ_ONLY      :Boolean;
  //STENCIL_READ_ONLY:Boolean;

  CLEAR_VALUE:TVkClearValue;

  depthTestEnable      :TVkBool32;
  depthWriteEnable     :TVkBool32;
  depthCompareOp       :TVkCompareOp;
  depthBoundsTestEnable:TVkBool32;
  stencilTestEnable    :TVkBool32;

  front:TVkStencilOpState;
  back:TVkStencilOpState;

  minDepthBounds:TVkFloat;
  maxDepthBounds:TVkFloat;

  DEPTH_USAGE:Byte;
  STENCIL_USAGE:Byte;

  FImageInfo:TvImageKey;

  //dformat:TVkFormat;

  zorder_stage:TVkPipelineStageFlags;

 end;

 PGPU_REGS=^TGPU_REGS;
 TGPU_REGS=packed object
  RENDER_TARGET:array[0..7] of TRENDER_TARGET;
  TARGET_MASK:TCB_TARGET_MASK;
  VTE_CNTL:TPA_CL_VTE_CNTL;

  SC_MODE_CNTL_0:TPA_SC_MODE_CNTL_0;
  SC_MODE_CNTL_1:TPA_SC_MODE_CNTL_1;

  GENERIC_SCISSOR:TVPORT_SCISSOR;

  VPORT_SCISSOR:array[0..15] of TVPORT_SCISSOR;

  VPORT_ZMIN_MAX:array[0..15] of TVPORT_ZMIN_MAX;

  VPORT_SCALE_OFFSET:array[0..15] of TVPORT_SCALE_OFFSET;

  SCREEN_SCISSOR_BR:TPA_SC_SCREEN_SCISSOR_BR;
  SCREEN_SCISSOR_TL:TPA_SC_SCREEN_SCISSOR_TL;

  SC_AA_CONFIG:TPA_SC_AA_CONFIG;
  SC_AA_MASK_X0Y0_X1Y0:TPA_SC_AA_MASK_X0Y0_X1Y0;
  SC_AA_MASK_X0Y1_X1Y1:TPA_SC_AA_MASK_X0Y1_X1Y1;

  SC_AA_SAMPLE_LOCS_PIXEL:array[0..1,0..1] of TPA_SC_AA_SAMPLE_LOCS_PIXEL_XY;

  SC_CENTROID_PRIORITY:TPA_SC_CENTROID_PRIORITY;

  HARDWARE_SCREEN_OFFSET:TPA_SU_HARDWARE_SCREEN_OFFSET;
  SU_LINE_CNTL:TPA_SU_LINE_CNTL;
  SU_POINT_SIZE:TPA_SU_POINT_SIZE;
  SU_POINT_MINMAX:TPA_SU_POINT_MINMAX;

  VTX_CNTL:TPA_SU_VTX_CNTL;

  GB_CLIP:TGB_CLIP;
  CL_CLIP_CNTL:TPA_CL_CLIP_CNTL;
  SC_CLIPRECT_RULE:TPA_SC_CLIPRECT_RULE;

  SC_MODE_CNTL:TPA_SU_SC_MODE_CNTL;

  VGT_SHADER_STAGES_EN:TVGT_SHADER_STAGES_EN;
  VGT_OUT_DEALLOC_CNTL:TVGT_OUT_DEALLOC_CNTL;

  VGT_VTX_INDX:packed record
   CNT_EN:TVGT_VTX_CNT_EN;
   INDX_OFFSET:DWORD;
   MIN_INDX:DWORD;
   MAX_INDX:DWORD;
  end;

  VGT_MULTI_PRIM_IB_RESET_INDX:TVGT_MULTI_PRIM_IB_RESET_INDX;

  VGT_OUTPUT_PATH_CNTL:TVGT_OUTPUT_PATH_CNTL;

  VGT_GS_MODE:TVGT_GS_MODE;

  VGT_GS_PER_ES:TVGT_GS_PER_ES;
  VGT_ES_PER_GS:TVGT_ES_PER_GS;
  VGT_GS_PER_VS:TVGT_GS_PER_VS;

  VGT_PRIMITIVE_TYPE:TVGT_PRIMITIVE_TYPE;
  VGT_INDEX_TYPE    :TVGT_INDEX_TYPE    ;
  VGT_NUM_INSTANCES :TVGT_NUM_INSTANCES ;
  GRBM_GFX_INDEX    :TGRBM_GFX_INDEX;

  IA_MULTI_VGT_PARAM:TIA_MULTI_VGT_PARAM;

  VGT_DMA:packed record
   INDEX_TYPE:TVGT_DMA_INDEX_TYPE;
   NUM_INSTANCES:TVGT_DMA_NUM_INSTANCES;

   MAX_SIZE:DWORD;

   BASE_LO:DWORD;
   BASE_HI:DWORD;

   SIZE:DWORD;
   INDICES:DWORD;

  end;

  SPI:packed record

   PS:packed record
    INPUT_CNTL:array[0..31] of TSPI_PS_INPUT_CNTL_0;

    LO,HI:DWORD;
    RSRC1:TSPI_SHADER_PGM_RSRC1_PS;
    RSRC2:TSPI_SHADER_PGM_RSRC2_PS;
    RSRC3:TSPI_SHADER_PGM_RSRC3_PS;

    Z_FORMAT  :TSPI_SHADER_Z_FORMAT;
    COL_FORMAT:TSPI_SHADER_COL_FORMAT;

    INPUT_ENA :TSPI_PS_INPUT_ENA;
    INPUT_ADDR:TSPI_PS_INPUT_ADDR;
    IN_CONTROL:TSPI_PS_IN_CONTROL;
    BARYC_CNTL:TSPI_BARYC_CNTL;

    SHADER_CONTROL:TDB_SHADER_CONTROL;
    SHADER_MASK:TCB_SHADER_MASK;

    USER_DATA:TSPI_USER_DATA;
   end;

   VS:packed record
    LO,HI:DWORD;
    RSRC1:TSPI_SHADER_PGM_RSRC1_VS;
    RSRC2:TSPI_SHADER_PGM_RSRC2_VS;
    RSRC3:TSPI_SHADER_PGM_RSRC3_VS;

    OUT_CONFIG:TSPI_VS_OUT_CONFIG;
    POS_FORMAT:TSPI_SHADER_POS_FORMAT;
    OUT_CNTL  :TPA_CL_VS_OUT_CNTL;

    USER_DATA:TSPI_USER_DATA;

    LATE_ALLOC:TSPI_SHADER_LATE_ALLOC_VS;
   end;

   CS:packed record
    LO,HI:DWORD;
    RSRC1:TCOMPUTE_PGM_RSRC1;
    RSRC2:TCOMPUTE_PGM_RSRC2;

    STATIC_THREAD_MGMT_SE0:TCOMPUTE_STATIC_THREAD_MGMT_SE0;
    STATIC_THREAD_MGMT_SE1:TCOMPUTE_STATIC_THREAD_MGMT_SE1;
    RESOURCE_LIMITS:TCOMPUTE_RESOURCE_LIMITS;

    NUM_THREAD_X:TCOMPUTE_NUM_THREAD_X;
    NUM_THREAD_Y:TCOMPUTE_NUM_THREAD_Y;
    NUM_THREAD_Z:TCOMPUTE_NUM_THREAD_Z;

    USER_DATA:TSPI_USER_DATA;
   end;

  end;

  DEPTH:packed record
   RENDER_CONTROL    :TDB_RENDER_CONTROL;
   DEPTH_CONTROL     :TDB_DEPTH_CONTROL;

   DEPTH_VIEW        :TDB_DEPTH_VIEW        ;
   HTILE_DATA_BASE   :TDB_HTILE_DATA_BASE   ;
   DEPTH_BOUNDS_MIN  :Single;
   DEPTH_BOUNDS_MAX  :Single;
   STENCIL_CLEAR     :TDB_STENCIL_CLEAR     ;
   DEPTH_CLEAR       :Single;

   DEPTH_INFO        :TDB_DEPTH_INFO        ;
   Z_INFO            :TDB_Z_INFO            ;
   STENCIL_INFO      :TDB_STENCIL_INFO      ;
   Z_READ_BASE       :TDB_Z_READ_BASE       ;
   STENCIL_READ_BASE :TDB_STENCIL_READ_BASE ;
   Z_WRITE_BASE      :TDB_Z_WRITE_BASE      ;
   STENCIL_WRITE_BASE:TDB_STENCIL_WRITE_BASE;
   DEPTH_SIZE        :TDB_DEPTH_SIZE        ;
   DEPTH_SLICE       :TDB_DEPTH_SLICE       ;

   HTILE_SURFACE     :TDB_HTILE_SURFACE     ;

   EQAA              :TDB_EQAA;

   COUNT_CONTROL     :TDB_COUNT_CONTROL;
  end;


  CB_COLOR_CONTROL:TCB_COLOR_CONTROL;

  CB_BLEND_RGBA:array[0..7] of Single;

  CB_BLEND_CONTROL:array[0..7] of TCB_BLEND0_CONTROL;

  PA_SU_POLY_OFFSET_DB_FMT_CNTL:TPA_SU_POLY_OFFSET_DB_FMT_CNTL;

  Function  _SHADER_MASK(i:Byte):Byte; inline;  //0..7
  Function  _TARGET_MASK(i:Byte):Byte; inline;  //0..7
  Function  _COMP_MASK(i:Byte):Byte;   inline;  //0..7
  Function  COMP_ENABLE:Boolean; inline;
  Function  RT_ENABLE(i:Byte):Boolean; //0..7
  Function  VP_ENABLE(i:Byte):Boolean; //0..15
  Function  GET_VPORT(i:Byte):TVkViewport; //0..15
  Function  GET_SCISSOR(i:Byte):TVkRect2D; //0..15
  Function  GET_SCREEN:TVkRect2D;
  Function  GET_SCREEN_SIZE:TVkExtent2D;
  Function  GET_RT_BLEND(i:Byte):TVkPipelineColorBlendAttachmentState; //0..7
  Function  GET_RT_INFO(i:Byte):TRT_INFO; //0..7
  Function  DB_ENABLE:Boolean;
  Function  GET_DB_INFO:TDB_INFO;
  function  GET_PRIM_TYPE:TVkPrimitiveTopology;
  function  GET_INDEX_TYPE:TVkIndexType;
  function  GET_INDEX_TYPE_SIZE:Byte;

  Procedure Clear;
  Procedure InitDefault;
  Procedure ClearDMA;
 end;

const
 // Provided by VK_EXT_image_view_min_lod
 VK_STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT = 1000391001;

type
 PVkImageViewMinLodCreateInfoEXT=^TVkImageViewMinLodCreateInfoEXT;
 TVkImageViewMinLodCreateInfoEXT=record
  sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT
  pNext:PVkVoid;
  minLod:TVkFloat;
 end;

function _get_vsharp_cformat(PV:PVSharpResource4):TVkFormat;
function _get_tsharp4_cformat(PT:PTSharpResource4):TVkFormat;

function _get_tsharp4_min_lod(PT:PTSharpResource4):TVkImageViewMinLodCreateInfoEXT;

function _get_tsharp4_image_info(PT:PTSharpResource4):TvImageKey;
function _get_tsharp4_image_view(PT:PTSharpResource4):TvImageViewKey;

function _get_ssharp_info(PS:PSSharpResource4):TVkSamplerCreateInfo;

implementation

Function TGPU_REGS._SHADER_MASK(i:Byte):Byte; inline; //0..7
begin
 Result:=(DWORD(SPI.PS.SHADER_MASK) shr (i shl 2)) and 15;
end;

Function TGPU_REGS._TARGET_MASK(i:Byte):Byte; inline; //0..7
begin
 Result:=(DWORD(TARGET_MASK) shr (i shl 2)) and 15;
end;

Function TGPU_REGS._COMP_MASK(i:Byte):Byte; inline;  //0..7
begin
 Result:=((DWORD(SPI.PS.SHADER_MASK) and DWORD(TARGET_MASK)) shr (i shl 2)) and 15;
end;

Function TGPU_REGS.COMP_ENABLE:Boolean; inline;
begin
 Result:=(DWORD(SPI.PS.SHADER_MASK) and DWORD(TARGET_MASK))<>0;
end;

Function TGPU_REGS.RT_ENABLE(i:Byte):Boolean; //0..7
begin
 Result:=(RENDER_TARGET[i].BASE<>0) and
         (RENDER_TARGET[i].INFO.FORMAT<>0) and
         (_COMP_MASK(i)<>0);
end;

Function TGPU_REGS.VP_ENABLE(i:Byte):Boolean; //0..15
begin
 Result:=(PQWORD(@VPORT_SCALE_OFFSET[i])[0]<>0) or
         (PQWORD(@VPORT_SCALE_OFFSET[i])[1]<>0) or
         (PQWORD(@VPORT_SCALE_OFFSET[i])[2]<>0);
end;

Function TGPU_REGS.GET_VPORT(i:Byte):TVkViewport; //0..15
var
 V:TVPORT_SCALE_OFFSET;
begin
 Result:=Default(TVkViewport);
 V:=VPORT_SCALE_OFFSET[i];

 if (VTE_CNTL.VPORT_X_SCALE_ENA =0) then V.XSCALE :=1;
 if (VTE_CNTL.VPORT_X_OFFSET_ENA=0) then V.XOFFSET:=0;
 if (VTE_CNTL.VPORT_Y_SCALE_ENA =0) then V.YSCALE :=1;
 if (VTE_CNTL.VPORT_Y_OFFSET_ENA=0) then V.YOFFSET:=0;
 if (VTE_CNTL.VPORT_Z_SCALE_ENA =0) then V.ZSCALE :=1;
 if (VTE_CNTL.VPORT_Z_OFFSET_ENA=0) then V.ZOFFSET:=0;

 Assert(VTE_CNTL.VTX_XY_FMT=0);
 Assert(VTE_CNTL.VTX_Z_FMT =0);
 Assert(VTE_CNTL.VTX_W0_FMT=1);

 Result.x       :=V.XOFFSET-V.XSCALE;
 Result.y       :=V.YOFFSET-V.YSCALE;
 Result.width   :=V.XSCALE*2;
 Result.height  :=V.YSCALE*2;
 Result.minDepth:=V.ZOFFSET;
 Result.maxDepth:=V.ZOFFSET+V.ZSCALE;
end;

Function _fix_scissor_range(i:Word):Word;
begin
 Result:=i;
 if SmallInt(Result)<0 then Result:=0;
 if SmallInt(Result)>16384 then Result:=16384;
end;

Function TGPU_REGS.GET_SCISSOR(i:Byte):TVkRect2D; //0..15
begin
 if (SC_MODE_CNTL_0.VPORT_SCISSOR_ENABLE<>0) and
    ((DWORD(VPORT_SCISSOR[i].TL)<>0) or
     (DWORD(VPORT_SCISSOR[i].BR)<>0)) then
 begin
  Result.offset.x     :=_fix_scissor_range(VPORT_SCISSOR[i].TL.TL_X);
  Result.offset.y     :=_fix_scissor_range(VPORT_SCISSOR[i].TL.TL_Y);
  Result.extent.width :=_fix_scissor_range(VPORT_SCISSOR[i].BR.BR_X);
  Result.extent.height:=_fix_scissor_range(VPORT_SCISSOR[i].BR.BR_Y);
 end else
 begin
  Result.offset.x     :=_fix_scissor_range(SCREEN_SCISSOR_TL.TL_X);
  Result.offset.y     :=_fix_scissor_range(SCREEN_SCISSOR_TL.TL_Y);
  Result.extent.width :=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_X);
  Result.extent.height:=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_Y);
 end;

 Result.extent.width :=Result.extent.width -Result.offset.x;
 Result.extent.height:=Result.extent.height-Result.offset.y;
end;

Function TGPU_REGS.GET_SCREEN:TVkRect2D;
{var
 i:Byte;
 x,y:Word;}
begin
 Result.offset.x     :=_fix_scissor_range(SCREEN_SCISSOR_TL.TL_X);
 Result.offset.y     :=_fix_scissor_range(SCREEN_SCISSOR_TL.TL_Y);
 Result.extent.width :=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_X);
 Result.extent.height:=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_Y);
 Result.extent.width :=Result.extent.width -Result.offset.x;
 Result.extent.height:=Result.extent.height-Result.offset.y;
 {if (SC_MODE_CNTL_0.VPORT_SCISSOR_ENABLE=1) then
 begin
  Result:=Default(TPA_SC_SCREEN_SCISSOR_BR);
  For i:=0 to 15 do //SCISSOR WINDOW TODO
  begin
   x:=_fix_scissor_range(VPORT_SCISSOR[i].BR.BR_X);
   y:=_fix_scissor_range(VPORT_SCISSOR[i].BR.BR_Y);
   if (Result.BR_X<x) then Result.BR_X:=x;
   if (Result.BR_Y<y) then Result.BR_Y:=y;
  end;
 end else
 begin
  Result.BR_X:=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_X);
  Result.BR_Y:=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_Y);
 end;}
end;

Function TGPU_REGS.GET_SCREEN_SIZE:TVkExtent2D;
begin
 Result.width :=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_X);
 Result.height:=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_Y);
end;

Function GetBlendFactor(i:Byte):TVkBlendFactor;
begin
 Case i of
  BLEND_ZERO                    :Result:=VK_BLEND_FACTOR_ZERO;
  BLEND_ONE                     :Result:=VK_BLEND_FACTOR_ONE;
  BLEND_SRC_COLOR               :Result:=VK_BLEND_FACTOR_SRC_COLOR;
  BLEND_ONE_MINUS_SRC_COLOR     :Result:=VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR;
  BLEND_SRC_ALPHA               :Result:=VK_BLEND_FACTOR_SRC_ALPHA;
  BLEND_ONE_MINUS_SRC_ALPHA     :Result:=VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
  BLEND_DST_ALPHA               :Result:=VK_BLEND_FACTOR_DST_ALPHA;
  BLEND_ONE_MINUS_DST_ALPHA     :Result:=VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA;
  BLEND_DST_COLOR               :Result:=VK_BLEND_FACTOR_DST_COLOR;
  BLEND_ONE_MINUS_DST_COLOR     :Result:=VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR;
  BLEND_SRC_ALPHA_SATURATE      :Result:=VK_BLEND_FACTOR_SRC_ALPHA_SATURATE;
  BLEND_BOTH_SRC_ALPHA          :Result:=VK_BLEND_FACTOR_SRC1_ALPHA;
  BLEND_BOTH_INV_SRC_ALPHA      :Result:=VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA;
  BLEND_CONSTANT_COLOR          :Result:=VK_BLEND_FACTOR_CONSTANT_COLOR;
  BLEND_ONE_MINUS_CONSTANT_COLOR:Result:=VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR;
  BLEND_SRC1_COLOR              :Result:=VK_BLEND_FACTOR_SRC1_COLOR;
  BLEND_INV_SRC1_COLOR          :Result:=VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR;
  BLEND_SRC1_ALPHA              :Result:=VK_BLEND_FACTOR_SRC1_ALPHA;
  BLEND_INV_SRC1_ALPHA          :Result:=VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA;
  BLEND_CONSTANT_ALPHA          :Result:=VK_BLEND_FACTOR_CONSTANT_ALPHA;
  BLEND_ONE_MINUS_CONSTANT_ALPHA:Result:=VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA;
  else
   Assert(false);
 end;
end;

Function GetBlendOp(i:Byte):TVkBlendOp;
begin
 Case i of
  COMB_DST_PLUS_SRC :Result:=VK_BLEND_OP_ADD;
  COMB_SRC_MINUS_DST:Result:=VK_BLEND_OP_SUBTRACT;
  COMB_MIN_DST_SRC  :Result:=VK_BLEND_OP_MIN;
  COMB_MAX_DST_SRC  :Result:=VK_BLEND_OP_MAX;
  COMB_DST_MINUS_SRC:Result:=VK_BLEND_OP_REVERSE_SUBTRACT;
  else
   Assert(false);
 end;
end;

function GetRTCompCount(FORMAT:Byte):Byte;
begin
 Result:=0;
 Case FORMAT of
  COLOR_8             :Result:=1;
  COLOR_16            :Result:=1;
  COLOR_8_8           :Result:=2;
  COLOR_32            :Result:=1;
  COLOR_16_16         :Result:=2;
  COLOR_10_11_11      :Result:=3;
  COLOR_11_11_10      :Result:=3;
  COLOR_10_10_10_2    :Result:=4;
  COLOR_2_10_10_10    :Result:=4;
  COLOR_8_8_8_8       :Result:=4;
  COLOR_32_32         :Result:=2;
  COLOR_16_16_16_16   :Result:=4;
  COLOR_RESERVED_13   :Result:=3; //32_32_32
  COLOR_32_32_32_32   :Result:=4;
  COLOR_5_6_5         :Result:=3;
  COLOR_1_5_5_5       :Result:=4;
  COLOR_5_5_5_1       :Result:=4;
  COLOR_4_4_4_4       :Result:=4;
  COLOR_8_24          :Result:=2;
  COLOR_24_8          :Result:=2;
  COLOR_X24_8_32_FLOAT:Result:=3;
 end;
end;

type
 TCOMP_MAP=array[0..3] of Byte;

function GetCompMap(COMP_SWAP,COUNT:Byte):TCOMP_MAP;
begin
 Result:=Default(TCOMP_MAP);

 Case COUNT of
  1:
    Case COMP_SWAP of
     SWAP_STD    :Result[0]:=ord(VK_COLOR_COMPONENT_R_BIT);
     SWAP_ALT    :Result[0]:=ord(VK_COLOR_COMPONENT_G_BIT);
     SWAP_STD_REV:Result[0]:=ord(VK_COLOR_COMPONENT_B_BIT);
     SWAP_ALT_REV:Result[0]:=ord(VK_COLOR_COMPONENT_A_BIT);
    end;
  2:
    Case COMP_SWAP of
     SWAP_STD    :begin
                   Result[0]:=ord(VK_COLOR_COMPONENT_R_BIT);
                   Result[1]:=ord(VK_COLOR_COMPONENT_G_BIT);
                  end;
     SWAP_ALT    :begin
                   Result[0]:=ord(VK_COLOR_COMPONENT_R_BIT);
                   Result[1]:=ord(VK_COLOR_COMPONENT_A_BIT);
                  end;
     SWAP_STD_REV:begin
                   Result[0]:=ord(VK_COLOR_COMPONENT_A_BIT);
                   Result[1]:=ord(VK_COLOR_COMPONENT_B_BIT);
                  end;
     SWAP_ALT_REV:begin
                   Result[0]:=ord(VK_COLOR_COMPONENT_A_BIT);
                   Result[1]:=ord(VK_COLOR_COMPONENT_R_BIT);
                  end;
    end;
  3:
    Case COMP_SWAP of
     SWAP_STD    :begin
                   Result[0]:=ord(VK_COLOR_COMPONENT_R_BIT);
                   Result[1]:=ord(VK_COLOR_COMPONENT_G_BIT);
                   Result[2]:=ord(VK_COLOR_COMPONENT_B_BIT);
                  end;
     SWAP_ALT    :begin
                   Result[0]:=ord(VK_COLOR_COMPONENT_R_BIT);
                   Result[1]:=ord(VK_COLOR_COMPONENT_G_BIT);
                   Result[2]:=ord(VK_COLOR_COMPONENT_A_BIT);
                  end;
     SWAP_STD_REV:begin
                   Result[0]:=ord(VK_COLOR_COMPONENT_A_BIT);
                   Result[1]:=ord(VK_COLOR_COMPONENT_B_BIT);
                   Result[2]:=ord(VK_COLOR_COMPONENT_G_BIT);
                  end;
     SWAP_ALT_REV:begin
                   Result[0]:=ord(VK_COLOR_COMPONENT_A_BIT);
                   Result[1]:=ord(VK_COLOR_COMPONENT_R_BIT);
                   Result[2]:=ord(VK_COLOR_COMPONENT_G_BIT);
                  end;
    end;
  4:
    Case COMP_SWAP of
     SWAP_STD    :begin
                   Result[0]:=ord(VK_COLOR_COMPONENT_R_BIT);
                   Result[1]:=ord(VK_COLOR_COMPONENT_G_BIT);
                   Result[2]:=ord(VK_COLOR_COMPONENT_B_BIT);
                   Result[3]:=ord(VK_COLOR_COMPONENT_A_BIT);
                  end;
     SWAP_ALT    :begin
                   Result[0]:=ord(VK_COLOR_COMPONENT_B_BIT);
                   Result[1]:=ord(VK_COLOR_COMPONENT_G_BIT);
                   Result[2]:=ord(VK_COLOR_COMPONENT_R_BIT);
                   Result[3]:=ord(VK_COLOR_COMPONENT_A_BIT);
                  end;
     SWAP_STD_REV:begin
                   Result[0]:=ord(VK_COLOR_COMPONENT_A_BIT);
                   Result[1]:=ord(VK_COLOR_COMPONENT_B_BIT);
                   Result[2]:=ord(VK_COLOR_COMPONENT_G_BIT);
                   Result[3]:=ord(VK_COLOR_COMPONENT_R_BIT);
                  end;
     SWAP_ALT_REV:begin
                   Result[0]:=ord(VK_COLOR_COMPONENT_A_BIT);
                   Result[1]:=ord(VK_COLOR_COMPONENT_R_BIT);
                   Result[2]:=ord(VK_COLOR_COMPONENT_G_BIT);
                   Result[3]:=ord(VK_COLOR_COMPONENT_B_BIT);
                  end;
    end;

 end;

 //SWAP_STD      (R=>0)
 //SWAP_ALT      (G=>0)
 //SWAP_STD_REV  (B=>0)
 //SWAP_ALT_REV  (A=>0)

 //SWAP_ALT      (R=>0, A=>1)
 //SWAP_ALT      (R=>0, G=>1, A=>2)

 //SWAP_STD      (R=>0, G=>1, B=>2, A=>3)
 //SWAP_ALT      (B=>0, G=>1, R=>2, A=>3).
 //SWAP_STD_REV  (A=>0, B=>1, G=>2, R=>3)
 //SWAP_ALT_REV  (A=>0, R=>1, G=>2, B=>3)
end;

Function TGPU_REGS.GET_RT_BLEND(i:Byte):TVkPipelineColorBlendAttachmentState; //0..7
var
 m:Byte;
 COMP_MAP:TCOMP_MAP;
begin
 Result:=Default(TVkPipelineColorBlendAttachmentState);

 m:=GetRTCompCount(RENDER_TARGET[i].INFO.FORMAT);
 COMP_MAP:=GetCompMap(RENDER_TARGET[i].INFO.COMP_SWAP,m);

 //COMP_SWAP depend  (B=>0, G=>1, R=>2, A=>3)
 m:=_COMP_MASK(i);
 if m.TestBit(0) then Result.colorWriteMask:=Result.colorWriteMask or COMP_MAP[0];
 if m.TestBit(1) then Result.colorWriteMask:=Result.colorWriteMask or COMP_MAP[1];
 if m.TestBit(2) then Result.colorWriteMask:=Result.colorWriteMask or COMP_MAP[2];
 if m.TestBit(3) then Result.colorWriteMask:=Result.colorWriteMask or COMP_MAP[3];

 //BLEND_CLAMP

 if (RENDER_TARGET[i].INFO.BLEND_BYPASS<>0) then
 begin
  Result.blendEnable:=VK_FALSE;
 end else
 if (CB_BLEND_CONTROL[i].ENABLE=0) then
 begin
  Result.blendEnable:=VK_FALSE;
 end else
 begin
  Result.blendEnable:=VK_TRUE;

  Result.srcColorBlendFactor:=GetBlendFactor(CB_BLEND_CONTROL[i].COLOR_SRCBLEND);
  Result.dstColorBlendFactor:=GetBlendFactor(CB_BLEND_CONTROL[i].COLOR_DESTBLEND);
  Result.colorBlendOp       :=GetBlendOp(CB_BLEND_CONTROL[i].COLOR_COMB_FCN);

  if (CB_BLEND_CONTROL[i].SEPARATE_ALPHA_BLEND=0) then
  begin
   Result.srcAlphaBlendFactor:=Result.srcColorBlendFactor;
   Result.dstAlphaBlendFactor:=Result.dstColorBlendFactor;
   Result.alphaBlendOp       :=Result.colorBlendOp;
  end else //VkPhysicalDeviceFeatures.independentBlend
  begin
   Result.srcAlphaBlendFactor:=GetBlendFactor(CB_BLEND_CONTROL[i].ALPHA_SRCBLEND);
   Result.dstAlphaBlendFactor:=GetBlendFactor(CB_BLEND_CONTROL[i].ALPHA_DESTBLEND);
   Result.alphaBlendOp       :=GetBlendOp(CB_BLEND_CONTROL[i].ALPHA_COMB_FCN);
  end;


  Assert(CB_BLEND_CONTROL[i].DISABLE_ROP3=0);
 end;

 //Result.blendEnable:=VK_TRUE;
 //Result.SRCCOLORBLENDFACTOR:=VK_BLEND_FACTOR_SRC_ALPHA;
 //Result.DSTCOLORBLENDFACTOR:=VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
 //Result.COLORBLENDOP       :=VK_BLEND_OP_ADD;
 //Result.SRCALPHABLENDFACTOR:=VK_BLEND_FACTOR_SRC_ALPHA;
 //Result.DSTALPHABLENDFACTOR:=VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
 //Result.ALPHABLENDOP       :=VK_BLEND_OP_ADD;
 //Result.COLORWRITEMASK     :=15;

 //CB_COLOR_CONTROL.MODE   //CB_DISABLE
 //Assert(CB_COLOR_CONTROL.ROP3 = 204); //CB_DISABLE

{
 POSSIBLE VALUES:
00 - 0x00: BLACKNESS
05 - 0x05
10 - 0x0A
15 - 0x0F
17 - 0x11: NOTSRCERASE
34 - 0x22
51 - 0x33: NOTSRCCOPY
68 - 0x44: SRCERASE
80 - 0x50
85 - 0x55: DSTINVERT
90 - 0x5A: PATINVERT
95 - 0x5F
102 - 0x66: SRCINVERT
119 - 0x77
136 - 0x88: SRCAND
153 - 0x99
160 - 0xA0
165 - 0xA5
170 - 0xAA
175 - 0xAF
187 - 0xBB: MERGEPAINT
204 - 0xCC: SRCCOPY
221 - 0xDD
238 - 0xEE: SRCPAINT
240 - 0xF0: PATCOPY
245 - 0xF5
250 - 0xFA
255 - 0xFF: WHITENESS
}

end;

const
 // Depth modes (for depth buffers)
 kTileModeDepth_2dThin_64                   = $00000000; ///< Recommended for depth targets with one fragment per pixel.
 kTileModeDepth_2dThin_128                  = $00000001; ///< Recommended for depth targets with two or four fragments per pixel, or texture-readable.
 kTileModeDepth_2dThin_256                  = $00000002; ///< Recommended for depth targets with eight fragments per pixel.
 kTileModeDepth_2dThin_512                  = $00000003; ///< Recommended for depth targets with 512-byte tiles.
 kTileModeDepth_2dThin_1K                   = $00000004; ///< Recommended for depth targets with 1024-byte tiled.
 kTileModeDepth_1dThin                      = $00000005; ///< Not used; included only for completeness.
 kTileModeDepth_2dThinPrt_256               = $00000006; ///< Recommended for partially-resident depth surfaces. Does not support aliasing multiple virtual texture pages to the same physical page.
 kTileModeDepth_2dThinPrt_1K                = $00000007; ///< Not used; included only for completeness.
 // Display modes
 kTileModeDisplay_LinearAligned             = $00000008; ///< Recommended for any surface to be easily accessed on the CPU.
 kTileModeDisplay_1dThin                    = $00000009; ///< Not used; included only for completeness.
 kTileModeDisplay_2dThin                    = $0000000A; ///< Recommended mode for displayable render targets.
 kTileModeDisplay_ThinPrt                   = $0000000B; ///< Supports aliasing multiple virtual texture pages to the same physical page.
 kTileModeDisplay_2dThinPrt                 = $0000000C; ///< Does not support aliasing multiple virtual texture pages to the same physical page.
 // Thin modes (for non-displayable 1D/2D/3D surfaces)
 kTileModeThin_1dThin                       = $0000000D; ///< Recommended for read-only non-volume textures.
 kTileModeThin_2dThin                       = $0000000E; ///< Recommended for non-displayable intermediate render targets and read/write non-volume textures.
 kTileModeThin_3dThin                       = $0000000F; ///< Not used; included only for completeness.
 kTileModeThin_ThinPrt                      = $00000010; ///< Recommended for partially-resident textures (PRTs). Supports aliasing multiple virtual texture pages to the same physical page.
 kTileModeThin_2dThinPrt                    = $00000011; ///< Does not support aliasing multiple virtual texture pages to the same physical page.
 kTileModeThin_3dThinPrt                    = $00000012; ///< Does not support aliasing multiple virtual texture pages to the same physical page.
 // Thick modes (for 3D textures)
 kTileModeThick_1dThick                     = $00000013; ///< Recommended for read-only volume textures.
 kTileModeThick_2dThick                     = $00000014; ///< Recommended for volume textures to which pixel shaders will write.
 kTileModeThick_3dThick                     = $00000015; ///< Not used; included only for completeness.
 kTileModeThick_ThickPrt                    = $00000016; ///< Supports aliasing multiple virtual texture pages to the same physical page.
 kTileModeThick_2dThickPrt                  = $00000017; ///< Does not support aliasing multiple virtual texture pages to the same physical page.
 kTileModeThick_3dThickPrt                  = $00000018; ///< Does not support aliasing multiple virtual texture pages to the same physical page.
 kTileModeThick_2dXThick                    = $00000019; ///< Recommended for volume textures to which pixel shaders will write.
 kTileModeThick_3dXThick                    = $0000001A; ///< Not used; included only for completeness.
 // Hugely inefficient linear display mode -- do not use!
 kTileModeDisplay_LinearGeneral             = $0000001F; ///< Unsupported; do not use!

Function TGPU_REGS.GET_RT_INFO(i:Byte):TRT_INFO; //0..7
var
 COMP_MAP:TCOMP_MAP;
 W:QWORD;
begin
 Result:=Default(TRT_INFO);

 Result.FImageInfo.Addr:=Pointer(QWORD(RENDER_TARGET[i].BASE) shl 8);
 if (RENDER_TARGET[i].INFO.LINEAR_GENERAL<>0) then
 begin
  Result.FImageInfo.Addr:=Pointer(QWORD(Result.FImageInfo.Addr) or Byte(RENDER_TARGET[i].VIEW.SLICE_START));
 end;

 Result.FImageInfo.params.extend.width :=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_X);
 Result.FImageInfo.params.extend.height:=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_Y);
 Result.FImageInfo.params.extend.depth :=1;

 Result.padded.Width :=(RENDER_TARGET[i].PITCH.TILE_MAX+1)*8;
 Result.padded.Height:=(RENDER_TARGET[i].SLICE.TILE_MAX+1)*8 div (RENDER_TARGET[i].PITCH.TILE_MAX+1);

 Assert(RENDER_TARGET[i].INFO.ENDIAN=ENDIAN_NONE);
 //Assert(RENDER_TARGET[i].INFO.COMPRESSION=0);  //FMASK and MSAA

 Case RENDER_TARGET[i].INFO.FORMAT of
  COLOR_8_8_8_8:
   Case RENDER_TARGET[i].INFO.NUMBER_TYPE of
    NUMBER_UNORM:Result.FImageInfo.cformat:=VK_FORMAT_R8G8B8A8_UNORM;
    NUMBER_SRGB :Result.FImageInfo.cformat:=VK_FORMAT_R8G8B8A8_SRGB;
    else
     Assert(false,'TODO');
   end;
  else
   Assert(false,'TODO');
 end;

 if (RENDER_TARGET[i].INFO.LINEAR_GENERAL<>0) then
  Result.FImageInfo.params.tiling_idx:=kTileModeDisplay_LinearGeneral
 else
  Result.FImageInfo.params.tiling_idx:=RENDER_TARGET[i].ATTRIB.TILE_MODE_INDEX;

 Result.FImageInfo.params.itype      :=ord(VK_IMAGE_TYPE_2D);
 Result.FImageInfo.params.samples    :=1 shl (RENDER_TARGET[i].ATTRIB.NUM_SAMPLES and 3);
 Result.FImageInfo.params.mipLevels  :=1;
 Result.FImageInfo.params.arrayLayers:=1;

 Result.FImageView.cformat   :=Result.FImageInfo.cformat;
 Result.FImageView.vtype     :=ord(VK_IMAGE_VIEW_TYPE_2D);

 //Result.FImageView.dstSel.r:=ord(VK_COMPONENT_SWIZZLE_R);
 //Result.FImageView.dstSel.g:=ord(VK_COMPONENT_SWIZZLE_G);
 //Result.FImageView.dstSel.b:=ord(VK_COMPONENT_SWIZZLE_B);
 //Result.FImageView.dstSel.a:=ord(VK_COMPONENT_SWIZZLE_A);

 //Result.FImageView.dstSel:TvDstSel; TODO

 //Result.FImageView.base_level:Byte;  //first mip level (0..15)
 //Result.FImageView.last_level:Byte;  //last mip level (0..15)
 //Result.FImageView.base_array:Word;  //first array index (0..16383)
 //Result.FImageView.last_array:Word;  //texture height (0..16383)

 Result.blend:=GET_RT_BLEND(i);

 Result.COMP_SWAP:=RENDER_TARGET[i].INFO.COMP_SWAP;

 if (RENDER_TARGET[i].INFO.FAST_CLEAR<>0) then
 begin
  Result.IMAGE_USAGE:=TM_CLEAR or TM_WRITE;
 end else
 begin
  Result.IMAGE_USAGE:=TM_READ  or TM_WRITE;
 end;

 //if (Result.blend.blendEnable<>0) then
 //begin
 // Result.IMAGE_USAGE:=Result.IMAGE_USAGE or TM_READ;
 //end;

  Case RENDER_TARGET[i].INFO.FORMAT of
   COLOR_8_8_8_8:
    Case RENDER_TARGET[i].INFO.NUMBER_TYPE of
     NUMBER_UNORM,
     NUMBER_SRGB :
      begin
       COMP_MAP:=GetCompMap(RENDER_TARGET[i].INFO.COMP_SWAP,4);

       //VK_COLOR_COMPONENT_R_BIT=$00000001, 0001
       //VK_COLOR_COMPONENT_G_BIT=$00000002, 0010
       //VK_COLOR_COMPONENT_B_BIT=$00000004, 0100
       //VK_COLOR_COMPONENT_A_BIT=$00000008  1000

       W:=RENDER_TARGET[i].CLEAR_WORD;

       //Writeln((W shr (BsrDWord(COMP_MAP[0]) shl 3)) and 255);
       //Writeln((W shr (BsrDWord(COMP_MAP[1]) shl 3)) and 255);
       //Writeln((W shr (BsrDWord(COMP_MAP[2]) shl 3)) and 255);
       //Writeln((W shr (BsrDWord(COMP_MAP[3]) shl 3)) and 255);

       Result.CLEAR_COLOR.float32[0]:=((W shr (BsrDWord(COMP_MAP[0]) shl 3)) and 255)/255;
       Result.CLEAR_COLOR.float32[1]:=((W shr (BsrDWord(COMP_MAP[1]) shl 3)) and 255)/255;
       Result.CLEAR_COLOR.float32[2]:=((W shr (BsrDWord(COMP_MAP[2]) shl 3)) and 255)/255;
       Result.CLEAR_COLOR.float32[3]:=((W shr (BsrDWord(COMP_MAP[3]) shl 3)) and 255)/255;
      end;
     else
      Assert(false);
    end;
   else
    Assert(false);
  end;

 //end;

end;

Function TGPU_REGS.DB_ENABLE:Boolean;
begin
 Result:=(
          (DEPTH.DEPTH_CONTROL.STENCIL_ENABLE<>0) and
          (DEPTH.STENCIL_INFO.FORMAT<>0)
          ) or
         (
          (DEPTH.DEPTH_CONTROL.Z_ENABLE<>0) and
          (DEPTH.Z_INFO.FORMAT<>0)
         );
end;

Function TGPU_REGS.GET_DB_INFO:TDB_INFO;
begin
 Result:=Default(TDB_INFO);

 //Result.extend:=GET_SCREEN_SIZE;

 Result.padded.width :=(DEPTH.DEPTH_SIZE.PITCH_TILE_MAX +1)*8;
 Result.padded.height:=(DEPTH.DEPTH_SIZE.HEIGHT_TILE_MAX+1)*8;

 Result.DEPTH_USAGE  :=((TM_WRITE or TM_CLEAR)*DEPTH.RENDER_CONTROL.DEPTH_CLEAR_ENABLE);
 Result.STENCIL_USAGE:=((TM_WRITE or TM_CLEAR)*DEPTH.RENDER_CONTROL.STENCIL_CLEAR_ENABLE);

 if (Result.DEPTH_USAGE=0) then
 begin
  Result.DEPTH_USAGE:=Result.DEPTH_USAGE or TM_READ;
 end;

 if (DEPTH.DEPTH_VIEW.Z_READ_ONLY=0) then
 begin
  Result.DEPTH_USAGE:=Result.DEPTH_USAGE or TM_WRITE;
 end;

 if (DEPTH.DEPTH_VIEW.STENCIL_READ_ONLY=0) then
 begin
  Result.STENCIL_USAGE:=Result.STENCIL_USAGE or TM_WRITE;
 end;

 //Result.DEPTH_CLEAR  :=DEPTH.RENDER_CONTROL.DEPTH_CLEAR_ENABLE<>0;
 //Result.STENCIL_CLEAR:=DEPTH.RENDER_CONTROL.STENCIL_CLEAR_ENABLE<>0;

 //Result.Z_READ_ONLY      :=DEPTH.DEPTH_VIEW.Z_READ_ONLY<>0;
 //Result.STENCIL_READ_ONLY:=DEPTH.DEPTH_VIEW.STENCIL_READ_ONLY<>0;

 Assert(DEPTH.RENDER_CONTROL.DEPTH_COPY=0);
 Assert(DEPTH.RENDER_CONTROL.STENCIL_COPY=0);
 Assert(DEPTH.RENDER_CONTROL.COPY_CENTROID=0);
 Assert(DEPTH.RENDER_CONTROL.COPY_SAMPLE=0);

 Result.CLEAR_VALUE.depthStencil.depth  :=DEPTH.DEPTH_CLEAR;
 Result.CLEAR_VALUE.depthStencil.stencil:=DEPTH.STENCIL_CLEAR.CLEAR;

 /////

 Result.depthTestEnable      :=DEPTH.DEPTH_CONTROL.Z_ENABLE;            //1:1
 Result.depthWriteEnable     :=DEPTH.DEPTH_CONTROL.Z_WRITE_ENABLE;      //1:1
 Result.depthBoundsTestEnable:=DEPTH.DEPTH_CONTROL.DEPTH_BOUNDS_ENABLE; //1:1
 Result.stencilTestEnable    :=DEPTH.DEPTH_CONTROL.STENCIL_ENABLE;      //1:1

 Result.depthCompareOp:=TVkCompareOp(DEPTH.DEPTH_CONTROL.ZFUNC); //1:1

 Result.minDepthBounds:=DEPTH.DEPTH_BOUNDS_MIN;
 Result.maxDepthBounds:=DEPTH.DEPTH_BOUNDS_MAX;

 //compareMask:TVkUInt32; //DB_STENCILREFMASK DB_STENCILREFMASK_BF
 //writeMask:TVkUInt32;   //DB_STENCILREFMASK DB_STENCILREFMASK_BF
 //reference:TVkUInt32;   //DB_STENCILREFMASK DB_STENCILREFMASK_BF

 if (DEPTH.DEPTH_CONTROL.DISABLE_COLOR_WRITES_ON_DEPTH_PASS<>0) then
 begin
  Result.front.failOp:=VK_STENCIL_OP_REPLACE;
  Result.front.depthFailOp:=VK_STENCIL_OP_REPLACE;
  //Result.front.reference:=;  ???
 end;

 if (DEPTH.DEPTH_CONTROL.ENABLE_COLOR_WRITES_ON_DEPTH_FAIL<>0) then
 begin
  Result.front.passOp:=VK_STENCIL_OP_REPLACE;
  //Result.front.reference:=;  ???
 end;

 Result.front.compareOp:=TVkCompareOp(DEPTH.DEPTH_CONTROL.STENCILFUNC); //1:1

 if (DEPTH.DEPTH_CONTROL.BACKFACE_ENABLE<>0) then
 begin
  Result.back:=Result.front;

  Result.back.compareOp:=TVkCompareOp(DEPTH.DEPTH_CONTROL.STENCILFUNC_BF); //1:1
 end;

 ////

 Assert(DEPTH.DEPTH_VIEW.SLICE_START=0);

 Case DEPTH.Z_INFO.FORMAT of
  Z_INVALID :
   if (DEPTH.STENCIL_INFO.FORMAT=STENCIL_8) then
   begin
    Result.FImageInfo.cformat:=VK_FORMAT_S8_UINT;
   end;
  Z_16      :
   if (DEPTH.STENCIL_INFO.FORMAT=STENCIL_8) then
   begin
    Result.FImageInfo.cformat:=VK_FORMAT_D16_UNORM_S8_UINT;
   end else
   begin
    Result.FImageInfo.cformat:=VK_FORMAT_D16_UNORM;
   end;
  Z_24      :
   if (DEPTH.STENCIL_INFO.FORMAT=STENCIL_8) then
   begin
    Result.FImageInfo.cformat:=VK_FORMAT_D24_UNORM_S8_UINT;
   end else
   begin
    Result.FImageInfo.cformat:=VK_FORMAT_X8_D24_UNORM_PACK32;
   end;
  Z_32_FLOAT:
   if (DEPTH.STENCIL_INFO.FORMAT=STENCIL_8) then
   begin
    Result.FImageInfo.cformat:=VK_FORMAT_D32_SFLOAT_S8_UINT;
   end else
   begin
    Result.FImageInfo.cformat:=VK_FORMAT_D32_SFLOAT;
   end;
 end;

 Result.Z_READ_ADDR :=Pointer(QWORD(DEPTH.Z_READ_BASE) shl 8);
 Result.Z_WRITE_ADDR:=Pointer(QWORD(DEPTH.Z_WRITE_BASE) shl 8);

 Result.STENCIL_READ_ADDR :=Pointer(QWORD(DEPTH.STENCIL_READ_BASE) shl 8);
 Result.STENCIL_WRITE_ADDR:=Pointer(QWORD(DEPTH.STENCIL_WRITE_BASE) shl 8);

 Assert(SPI.PS.SHADER_CONTROL.Z_EXPORT_ENABLE=0);
 Assert(SPI.PS.SHADER_CONTROL.STENCIL_TEST_VAL_EXPORT_ENABLE=0);

 Case SPI.PS.SHADER_CONTROL.Z_ORDER of
  LATE_Z,
  RE_Z               :Result.zorder_stage:=ord(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT);

  EARLY_Z_THEN_LATE_Z,
  EARLY_Z_THEN_RE_Z  :Result.zorder_stage:=ord(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or
                                           ord(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT);
 end;

 Result.FImageInfo.Addr:=Result.Z_READ_ADDR;

 Result.FImageInfo.params.extend.width :=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_X);
 Result.FImageInfo.params.extend.height:=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_Y);
 Result.FImageInfo.params.extend.depth :=1;

 Result.FImageInfo.params.tiling_idx:=DEPTH.Z_INFO.TILE_MODE_INDEX;

 Result.FImageInfo.params.itype      :=ord(VK_IMAGE_TYPE_2D);
 Result.FImageInfo.params.samples    :=1 shl (DEPTH.Z_INFO.NUM_SAMPLES and 3);
 Result.FImageInfo.params.mipLevels  :=1;
 Result.FImageInfo.params.arrayLayers:=1;
end;

function TGPU_REGS.GET_PRIM_TYPE:TVkPrimitiveTopology;
begin
 case VGT_PRIMITIVE_TYPE.PRIM_TYPE of
  DI_PT_POINTLIST    :Result:=VK_PRIMITIVE_TOPOLOGY_POINT_LIST                   ;
  DI_PT_LINELIST     :Result:=VK_PRIMITIVE_TOPOLOGY_LINE_LIST                    ;
  DI_PT_LINESTRIP    :Result:=VK_PRIMITIVE_TOPOLOGY_LINE_STRIP                   ;
  DI_PT_TRILIST      :Result:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST                ;
  DI_PT_TRIFAN       :Result:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN                 ;
  DI_PT_TRISTRIP     :Result:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP               ;
  DI_PT_PATCH        :Result:=VK_PRIMITIVE_TOPOLOGY_PATCH_LIST                   ;
  DI_PT_LINELIST_ADJ :Result:=VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY     ;
  DI_PT_LINESTRIP_ADJ:Result:=VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY    ;
  DI_PT_TRILIST_ADJ  :Result:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY ;
  DI_PT_TRISTRIP_ADJ :Result:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY;

  DI_PT_RECTLIST     ,
  DI_PT_LINELOOP     ,
  DI_PT_QUADLIST     ,
  DI_PT_QUADSTRIP    ,
  DI_PT_POLYGON      :Result:=TVkPrimitiveTopology(VGT_PRIMITIVE_TYPE.PRIM_TYPE); //need to emulate

  else
   Assert(False);
 end;
end;

// VGT_DI_PRIM_TYPE
//DI_PT_NONE          | kPrimitiveTypeNone               |
//DI_PT_POINTLIST     | kPrimitiveTypePointList          | VK_PRIMITIVE_TOPOLOGY_POINT_LIST
//DI_PT_LINELIST      | kPrimitiveTypeLineList           | VK_PRIMITIVE_TOPOLOGY_LINE_LIST
//DI_PT_LINESTRIP     | kPrimitiveTypeLineStrip          | VK_PRIMITIVE_TOPOLOGY_LINE_STRIP
//DI_PT_TRILIST       | kPrimitiveTypeTriList            | VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
//DI_PT_TRIFAN        | kPrimitiveTypeTriFan             | VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
//DI_PT_TRISTRIP      | kPrimitiveTypeTriStrip           | VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
//DI_PT_PATCH         | kPrimitiveTypePatch              | VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
//DI_PT_LINELIST_ADJ  | kPrimitiveTypeLineListAdjacency  | VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
//DI_PT_LINESTRIP_ADJ | kPrimitiveTypeLineStripAdjacency | VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
//DI_PT_TRILIST_ADJ   | kPrimitiveTypeTriListAdjacency   | VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
//DI_PT_TRISTRIP_ADJ  | kPrimitiveTypeTriStripAdjacency  | VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
//DI_PT_RECTLIST      | kPrimitiveTypeRectList           |
//DI_PT_LINELOOP      | kPrimitiveTypeLineLoop           |
//DI_PT_QUADLIST      | kPrimitiveTypeQuadList           |
//DI_PT_QUADSTRIP     | kPrimitiveTypeQuadStrip          |
//DI_PT_POLYGON       | kPrimitiveTypePolygon            |

function TGPU_REGS.GET_INDEX_TYPE:TVkIndexType;
begin
 Case VGT_DMA.INDEX_TYPE.INDEX_TYPE of
  VGT_INDEX_16:Result:=VK_INDEX_TYPE_UINT16;
  VGT_INDEX_32:Result:=VK_INDEX_TYPE_UINT32;
  VGT_INDEX_8 :Result:=VK_INDEX_TYPE_UINT8_EXT;
  else         Result:=VK_INDEX_TYPE_NONE_KHR;
 end;
end;

function TGPU_REGS.GET_INDEX_TYPE_SIZE:Byte;
begin
 Case VGT_DMA.INDEX_TYPE.INDEX_TYPE of
  VGT_INDEX_16:Result:=16;
  VGT_INDEX_32:Result:=32;
  VGT_INDEX_8 :Result:=8;
  else         Result:=0;
 end;
end;



Procedure TGPU_REGS.Clear;
begin
 FillChar(Self,SizeOf(Self),0);
end;

Procedure TGPU_REGS.InitDefault;
begin
 Clear;

 DWORD(SPI.CS.STATIC_THREAD_MGMT_SE0):=$FFFFFFFF;
 DWORD(SPI.CS.STATIC_THREAD_MGMT_SE1):=$FFFFFFFF;
 DWORD(SPI.CS.RESOURCE_LIMITS)       :=$00000170;

 DWORD(SPI.PS.RSRC3)                 :=$001701FF;
 DWORD(SPI.VS.RSRC3)                 :=$001701FD;
 //mmSPI_SHADER_PGM_RSRC3_GS=001701FF
 //mmSPI_SHADER_PGM_RSRC3_ES=001701FD
 //mmSPI_SHADER_PGM_RSRC3_HS=00000017
 //mmSPI_SHADER_PGM_RSRC3_LS=001701FD
 DWORD(SPI.VS.LATE_ALLOC)            :=$0000001C;
 DWORD(SPI.VS.OUT_CONFIG)            :=$00000002;

 DWORD(VTX_CNTL)                     :=$0000002D;
 DWORD(SU_LINE_CNTL)                 :=$00000008;
 DWORD(SU_POINT_SIZE)                :=$00080008;
 DWORD(SU_POINT_MINMAX)              :=$FFFF0000;
 DWORD(VTE_CNTL)                     :=$0000043F;
 DWORD(SC_CLIPRECT_RULE)             :=$0000FFFF;
 DWORD(VGT_OUT_DEALLOC_CNTL)         :=$00000010;

 PDWORD(@GB_CLIP.VERT_CLIP_ADJ)^     :=$3F800000;
 PDWORD(@GB_CLIP.HORZ_CLIP_ADJ)^     :=$3F800000;
 PDWORD(@GB_CLIP.VERT_DISC_ADJ)^     :=$3F800000;
 PDWORD(@GB_CLIP.HORZ_DISC_ADJ)^     :=$3F800000;

 DWORD(CB_COLOR_CONTROL)             :=$00CC0010;

 DWORD(SC_AA_MASK_X0Y0_X1Y0)         :=$FFFFFFFF;
 DWORD(SC_AA_MASK_X0Y1_X1Y1)         :=$FFFFFFFF;

 DWORD(VGT_VTX_INDX.MAX_INDX)        :=$FFFFFFFF;

 DWORD(SC_MODE_CNTL_1)               :=$06020000;

 DWORD(PA_SU_POLY_OFFSET_DB_FMT_CNTL):=$000001E9;

 DWORD(VGT_GS_PER_ES)                :=$00000100;
 DWORD(VGT_ES_PER_GS)                :=$00000100;
 DWORD(VGT_GS_PER_VS)                :=$00000004;

 DWORD(GRBM_GFX_INDEX    )           :=$E0000000;
 DWORD(IA_MULTI_VGT_PARAM)           :=$000000FF;

 VGT_DMA.NUM_INSTANCES:=1;
end;

Procedure TGPU_REGS.ClearDMA;
begin
 FillChar(VGT_DMA,SizeOf(VGT_DMA),0);
end;

function _get_vsharp_cformat(PV:PVSharpResource4):TVkFormat;
begin
 Result:=Default(TVkFormat);
 if (PV=nil) then Exit;

 Case PV^.nfmt of

  BUF_NUM_FORMAT_UNORM:
   case PV^.dfmt of
    BUF_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_UNORM;
    BUF_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_UNORM;
    BUF_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_UNORM;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16_UNORM;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_UNORM;
    else
     Assert(false,_get_buf_dfmt_str(PV^.dfmt));
   end;

  BUF_NUM_FORMAT_SNORM:
   case PV^.dfmt of
    BUF_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SNORM;
    BUF_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SNORM;
    BUF_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SNORM;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16_SNORM;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SNORM;
    else
     Assert(false,_get_buf_dfmt_str(PV^.dfmt));
   end;

  BUF_NUM_FORMAT_USCALED:
   case PV^.dfmt of
    BUF_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_USCALED;
    BUF_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_USCALED;
    BUF_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_USCALED;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16_USCALED;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_USCALED;
    else
     Assert(false,_get_buf_dfmt_str(PV^.dfmt));
   end;

  BUF_NUM_FORMAT_SSCALED:
   case PV^.dfmt of
    BUF_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SSCALED;
    BUF_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SSCALED;
    BUF_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SSCALED;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16_SSCALED;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SSCALED;
    else
     Assert(false,_get_buf_dfmt_str(PV^.dfmt));
   end;

  BUF_NUM_FORMAT_UINT:
   case PV^.dfmt of
    BUF_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_UINT;
    BUF_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_UINT;
    BUF_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_UINT;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16_UINT;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_UINT;
    BUF_DATA_FORMAT_32         :Result:=VK_FORMAT_R32_UINT;
    BUF_DATA_FORMAT_32_32      :Result:=VK_FORMAT_R32G32_UINT;
    BUF_DATA_FORMAT_32_32_32   :Result:=VK_FORMAT_R32G32B32_UINT;
    BUF_DATA_FORMAT_32_32_32_32:Result:=VK_FORMAT_R32G32B32A32_UINT;
    else
     Assert(false,_get_buf_dfmt_str(PV^.dfmt));
   end;

  BUF_NUM_FORMAT_SINT:
   case PV^.dfmt of
    BUF_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SINT;
    BUF_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SINT;
    BUF_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SINT;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16_SINT;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SINT;
    BUF_DATA_FORMAT_32         :Result:=VK_FORMAT_R32_SINT;
    BUF_DATA_FORMAT_32_32      :Result:=VK_FORMAT_R32G32_SINT;
    BUF_DATA_FORMAT_32_32_32   :Result:=VK_FORMAT_R32G32B32_SINT;
    BUF_DATA_FORMAT_32_32_32_32:Result:=VK_FORMAT_R32G32B32A32_SINT;
    else
     Assert(false,_get_buf_dfmt_str(PV^.dfmt));
   end;

  BUF_NUM_FORMAT_FLOAT:
   case PV^.dfmt of
    BUF_DATA_FORMAT_32         :Result:=VK_FORMAT_R32_SFLOAT;
    BUF_DATA_FORMAT_32_32      :Result:=VK_FORMAT_R32G32_SFLOAT;
    BUF_DATA_FORMAT_32_32_32   :Result:=VK_FORMAT_R32G32B32_SFLOAT;
    BUF_DATA_FORMAT_32_32_32_32:Result:=VK_FORMAT_R32G32B32A32_SFLOAT;
    else
     Assert(false,_get_buf_dfmt_str(PV^.dfmt));
   end;

  else
   Assert(false,_get_buf_nfmt_str(PV^.nfmt));
 end;
end;

function _img_is_msaa(b:Byte):Boolean; inline;
begin
 Case b of
  SQ_RSRC_IMG_2D_MSAA      ,
  SQ_RSRC_IMG_2D_MSAA_ARRAY:Result:=True;
  else
                            Result:=False;
 end;
end;

function _get_tsharp4_cformat(PT:PTSharpResource4):TVkFormat;
begin
 Result:=Default(TVkFormat);
 if (PT=nil) then Exit;

 Case PT^.nfmt of
  IMG_NUM_FORMAT_UNORM  :
    case PT^.dfmt of
     IMG_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_UNORM;
     IMG_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_UNORM;
     IMG_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_UNORM;
     IMG_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16_UNORM;
     IMG_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_UNORM;
     IMG_DATA_FORMAT_5_6_5      :Result:=VK_FORMAT_R5G6B5_UNORM_PACK16;
     IMG_DATA_FORMAT_BC1        :Result:=VK_FORMAT_BC1_RGBA_UNORM_BLOCK;
     IMG_DATA_FORMAT_BC3        :Result:=VK_FORMAT_BC3_UNORM_BLOCK
     else
      Assert(false,_get_tex_dfmt_str(PT^.dfmt));
    end;

  IMG_NUM_FORMAT_SRGB  :
    case PT^.dfmt of
     IMG_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SRGB;
     IMG_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SRGB;
     IMG_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SRGB;
     IMG_DATA_FORMAT_BC1        :Result:=VK_FORMAT_BC1_RGBA_SRGB_BLOCK;
     IMG_DATA_FORMAT_BC3        :Result:=VK_FORMAT_BC3_SRGB_BLOCK
     else
      Assert(false,_get_tex_dfmt_str(PT^.dfmt));
    end;

  IMG_NUM_FORMAT_SNORM  :
    case PT^.dfmt of
     IMG_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SNORM;
     IMG_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SNORM;
     IMG_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SNORM;
     IMG_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16_SNORM;
     IMG_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SNORM;
     else
      Assert(false,_get_tex_dfmt_str(PT^.dfmt));
    end;

  IMG_NUM_FORMAT_USCALED:
    case PT^.dfmt of
     IMG_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_USCALED;
     IMG_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_USCALED;
     IMG_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_USCALED;
     IMG_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16_USCALED;
     IMG_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_USCALED;
     else
      Assert(false,_get_tex_dfmt_str(PT^.dfmt));
    end;


  IMG_NUM_FORMAT_SSCALED:
    case PT^.dfmt of
     IMG_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SSCALED;
     IMG_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SSCALED;
     IMG_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SSCALED;
     IMG_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16_SSCALED;
     IMG_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SSCALED;
     else
      Assert(false,_get_tex_dfmt_str(PT^.dfmt));
    end;

  IMG_NUM_FORMAT_UINT   :
   case PT^.dfmt of
    IMG_DATA_FORMAT_8           :Result:=VK_FORMAT_R8_UINT;
    IMG_DATA_FORMAT_8_8         :Result:=VK_FORMAT_R8G8_UINT;
    IMG_DATA_FORMAT_8_8_8_8     :Result:=VK_FORMAT_R8G8B8A8_UINT;
    IMG_DATA_FORMAT_16_16       :Result:=VK_FORMAT_R16_UINT;
    IMG_DATA_FORMAT_16_16_16_16 :Result:=VK_FORMAT_R16G16B16A16_UINT;
    IMG_DATA_FORMAT_32          :Result:=VK_FORMAT_R32_UINT;
    IMG_DATA_FORMAT_32_32       :Result:=VK_FORMAT_R32G32_UINT;
    IMG_DATA_FORMAT_32_32_32    :Result:=VK_FORMAT_R32G32B32_UINT;
    IMG_DATA_FORMAT_32_32_32_32 :Result:=VK_FORMAT_R32G32B32A32_UINT;
    IMG_DATA_FORMAT_FMASK8_S2_F1:Result:=VK_FORMAT_R8_UINT;
    IMG_DATA_FORMAT_FMASK8_S4_F1:Result:=VK_FORMAT_R8_UINT;
    IMG_DATA_FORMAT_FMASK8_S8_F1:Result:=VK_FORMAT_R8_UINT;
    IMG_DATA_FORMAT_FMASK8_S2_F2:Result:=VK_FORMAT_R8_UINT;
    IMG_DATA_FORMAT_FMASK8_S4_F2:Result:=VK_FORMAT_R8_UINT;
    IMG_DATA_FORMAT_FMASK8_S4_F4:Result:=VK_FORMAT_R8_UINT;
    else
     Assert(false,_get_tex_dfmt_str(PT^.dfmt));
   end;

  IMG_NUM_FORMAT_SINT   :
   case PT^.dfmt of
    IMG_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SINT;
    IMG_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SINT;
    IMG_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SINT;
    IMG_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16_SINT;
    IMG_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SINT;
    IMG_DATA_FORMAT_32         :Result:=VK_FORMAT_R32_SINT;
    IMG_DATA_FORMAT_32_32      :Result:=VK_FORMAT_R32G32_SINT;
    IMG_DATA_FORMAT_32_32_32   :Result:=VK_FORMAT_R32G32B32_SINT;
    IMG_DATA_FORMAT_32_32_32_32:Result:=VK_FORMAT_R32G32B32A32_SINT;
    else
     Assert(false,_get_tex_dfmt_str(PT^.dfmt));
   end;

  IMG_NUM_FORMAT_FLOAT  :
   case PT^.dfmt of
    IMG_DATA_FORMAT_32         :Result:=VK_FORMAT_R32_SFLOAT;
    IMG_DATA_FORMAT_32_32      :Result:=VK_FORMAT_R32G32_SFLOAT;
    IMG_DATA_FORMAT_32_32_32   :Result:=VK_FORMAT_R32G32B32_SFLOAT;
    IMG_DATA_FORMAT_32_32_32_32:Result:=VK_FORMAT_R32G32B32A32_SFLOAT;
    else
     Assert(false,_get_tex_dfmt_str(PT^.dfmt));
   end;

  else
   Assert(false,_get_tex_nfmt_str(PT^.nfmt));
 end;

end;

function _get_tsharp4_image_info(PT:PTSharpResource4):TvImageKey;
begin
 Result:=Default(TvImageKey);
 if (PT=nil) then Exit;

 Result.Addr:=Pointer(PT^.base shl 8);
 Result.cformat:=_get_tsharp4_cformat(PT);

 Case PT^._type of
  SQ_RSRC_IMG_1D           :Result.params.itype:=ord(VK_IMAGE_TYPE_1D);
  SQ_RSRC_IMG_2D           :Result.params.itype:=ord(VK_IMAGE_TYPE_2D);
  SQ_RSRC_IMG_3D           :Result.params.itype:=ord(VK_IMAGE_TYPE_3D);
  SQ_RSRC_IMG_CUBE         :Result.params.itype:=ord(VK_IMAGE_TYPE_2D);
  SQ_RSRC_IMG_1D_ARRAY     :Result.params.itype:=ord(VK_IMAGE_TYPE_1D);
  SQ_RSRC_IMG_2D_ARRAY     :Result.params.itype:=ord(VK_IMAGE_TYPE_2D);
  SQ_RSRC_IMG_2D_MSAA      :Result.params.itype:=ord(VK_IMAGE_TYPE_2D);
  SQ_RSRC_IMG_2D_MSAA_ARRAY:Result.params.itype:=ord(VK_IMAGE_TYPE_2D);
  else;
   Assert(false);
 end;

 Result.params.tiling_idx   :=PT^.tiling_idx;
 Result.params.extend.width :=PT^.width+1;
 Result.params.extend.height:=PT^.height+1;
 Result.params.extend.depth :=1;

 if _img_is_msaa(PT^._type) then
 begin
  Result.params.samples  :=PT^.last_level;
  Result.params.mipLevels:=1;
 end else
 begin
  Result.params.samples  :=1;
  Result.params.mipLevels:=PT^.last_level-PT^.base_level+1;
 end;

 //Assert(Result.params.mipLevels=1,'TODO');
 Result.params.mipLevels:=1; /////

 Result.params.arrayLayers:=1;
end;

function _get_dst_sel_swizzle(b:Byte):Byte;
begin
 Case b of
  0:Result:=ord(VK_COMPONENT_SWIZZLE_ZERO);
  1:Result:=ord(VK_COMPONENT_SWIZZLE_ONE);
  4:Result:=ord(VK_COMPONENT_SWIZZLE_R);
  5:Result:=ord(VK_COMPONENT_SWIZZLE_G);
  6:Result:=ord(VK_COMPONENT_SWIZZLE_B);
  7:Result:=ord(VK_COMPONENT_SWIZZLE_A);
  else
    Result:=ord(VK_COMPONENT_SWIZZLE_IDENTITY);
 end;
end;

function _get_lod(w:Word):TVkFloat; forward;

function _get_tsharp4_min_lod(PT:PTSharpResource4):TVkImageViewMinLodCreateInfoEXT;
begin
 Result:=Default(TVkImageViewMinLodCreateInfoEXT);
 if (PT=nil) then Exit;

 ord(Result.sType):=VK_STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT;
 Result.minLod:=_get_lod(PT^.min_lod);
end;

//perf_mod:bit3;  //0=0/16, 1=2/16, 2=5/16, 3=7/16, 4=9/16, 5=11/16, 6=14/16, 7=16/16
//interlaced:bit1;  //texture is interlaced
//tiling_idx:bit5;  //index into lookup table of surface tiling settings
//pow2pad:bit1;  //memory footprint is padded to power of 2 dimensions

function _get_tsharp4_image_view(PT:PTSharpResource4):TvImageViewKey;
var
 t:Byte;
begin
 Result:=Default(TvImageViewKey);
 if (PT=nil) then Exit;

 Result.cformat:=_get_tsharp4_cformat(PT);

 Case PT^._type of
  SQ_RSRC_IMG_1D           :Result.vtype:=ord(VK_IMAGE_VIEW_TYPE_1D);
  SQ_RSRC_IMG_2D           :Result.vtype:=ord(VK_IMAGE_VIEW_TYPE_2D);
  SQ_RSRC_IMG_3D           :Result.vtype:=ord(VK_IMAGE_VIEW_TYPE_3D);
  SQ_RSRC_IMG_CUBE         :Result.vtype:=ord(VK_IMAGE_VIEW_TYPE_CUBE);
  SQ_RSRC_IMG_1D_ARRAY     :Result.vtype:=ord(VK_IMAGE_VIEW_TYPE_1D_ARRAY);
  SQ_RSRC_IMG_2D_ARRAY     :Result.vtype:=ord(VK_IMAGE_VIEW_TYPE_2D_ARRAY);
  SQ_RSRC_IMG_2D_MSAA      :Result.vtype:=ord(VK_IMAGE_VIEW_TYPE_2D);
  SQ_RSRC_IMG_2D_MSAA_ARRAY:Result.vtype:=ord(VK_IMAGE_VIEW_TYPE_2D_ARRAY);
  else;
   Assert(false);
 end;

 Result.dstSel.r:=_get_dst_sel_swizzle(PT^.dst_sel_x);
 Result.dstSel.g:=_get_dst_sel_swizzle(PT^.dst_sel_y);
 Result.dstSel.b:=_get_dst_sel_swizzle(PT^.dst_sel_z);
 Result.dstSel.a:=_get_dst_sel_swizzle(PT^.dst_sel_w);

 Case Result.cformat of
  VK_FORMAT_R5G6B5_UNORM_PACK16:
   begin
    t:=Result.dstSel.r;
    Result.dstSel.r:=Result.dstSel.b;
    Result.dstSel.b:=t;
   end;
  else;
 end;


 if not _img_is_msaa(PT^._type) then
 begin
  Result.base_level:=PT^.base_level;
  Result.last_level:=PT^.last_level;
 end;

 Result.base_level:=0; /////
 Result.last_level:=0; /////

end;


function _get_xy_filter(b:Byte):TVkFilter;
begin
 Case b of
  TEX_XYFilter_Point      :Result:=VK_FILTER_NEAREST;
  TEX_XYFilter_Linear     :Result:=VK_FILTER_LINEAR;
  TEX_XYFilter_AnisoPoint :Result:=VK_FILTER_NEAREST;
  TEX_XYFilter_AnisoLinear:Result:=VK_FILTER_LINEAR;
  else
    Result:=VK_FILTER_NEAREST;
 end;
end;

function _get_mip_filter(b:Byte):TVkSamplerMipmapMode;
begin
 Case b of
  TEX_MipFilter_None           :Result:=VK_SAMPLER_MIPMAP_MODE_NEAREST;
  TEX_MipFilter_Point          :Result:=VK_SAMPLER_MIPMAP_MODE_NEAREST;
  TEX_MipFilter_Linear         :Result:=VK_SAMPLER_MIPMAP_MODE_LINEAR;
  TEX_MipFilter_Point_Aniso_Adj:Result:=VK_SAMPLER_MIPMAP_MODE_NEAREST;
  else
    Result:=VK_SAMPLER_MIPMAP_MODE_NEAREST;
 end;
end;

function _get_clamp(b:Byte):TVkSamplerAddressMode;
begin
 Case b of
  SQ_TEX_WRAP                   :Result:=VK_SAMPLER_ADDRESS_MODE_REPEAT;
  SQ_TEX_MIRROR                 :Result:=VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT;
  SQ_TEX_CLAMP_LAST_TEXEL       :Result:=VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
  SQ_TEX_MIRROR_ONCE_LAST_TEXEL :Result:=VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE;
  SQ_TEX_CLAMP_HALF_BORDER      :Result:=VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
  SQ_TEX_MIRROR_ONCE_HALF_BORDER:Result:=VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE;
  SQ_TEX_CLAMP_BORDER           :Result:=VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
  SQ_TEX_MIRROR_ONCE_BORDER     :Result:=VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE;
  else
    Result:=VK_SAMPLER_ADDRESS_MODE_REPEAT;
 end;
end;

function _get_lod_bias(bias:word;sec:Byte):TVkFloat;
var
 b,s:TVkFloat;
begin
 b:=(-1*Tlod_bias_bits(bias).sign)+Tlod_bias_bits(bias).int+(Tlod_bias_bits(bias).frac/256);
 s:=(-1*Tlod_bias_sec_bits(sec).sign)+Tlod_bias_sec_bits(sec).int+(Tlod_bias_sec_bits(sec).frac/16);
 Result:=b+s;
end;

function _is_aniso_enabled(mag_filter,min_filter:Byte):TVkBool32;
begin
 Result:=ord((mag_filter=TEX_XYFilter_AnisoPoint) or
             (mag_filter=TEX_XYFilter_AnisoLinear) or
             (min_filter=TEX_XYFilter_AnisoPoint) or
             (min_filter=TEX_XYFilter_AnisoLinear));
end;

function _get_aniso_ratio(max_aniso_ratio:Byte):TVkFloat;
begin
 Case max_aniso_ratio of
  SQ_TEX_ANISO_RATIO_1 :Result:=1;
  SQ_TEX_ANISO_RATIO_2 :Result:=2;
  SQ_TEX_ANISO_RATIO_4 :Result:=4;
  SQ_TEX_ANISO_RATIO_8 :Result:=8;
  SQ_TEX_ANISO_RATIO_16:Result:=16;
  else
    Result:=0;
 end;
end;

function _get_lod(w:Word):TVkFloat;
begin
 Result:=Tlod_bits(w).int+(Tlod_bits(w).frac/256);
end;


//aniso_threshold:bit3;   //Threshold before sampling anisotropically (enum)
//mc_coord_trunc:bit1;
//force_degamma:bit1;   //Force de-gamma after filtering regardless of format
//aniso_bias:bit6;   //Anisotropy bias factor; unsigned fixed point 1.5
//trunc_coord:bit1;
//disable_cube_wrap:bit1;   //Disable sampling/filtering across face boundaries
//filter_mode:bit2;   //LERP, min, or max filter; default: LERP

//perf_mip:bit4;   //Bri-linear factor
//perf_z:bit4;

//z_filter:bit2;   //Filter in Z coordinate direction for volume textures

//border_color_ptr:bit12;  //Offset into global border color buffer

//border_color_type:bit2;   //Opaque-black, transparent-black, white, or color ptr

//VkSamplerCustomBorderColorCreateInfoEXT
function _get_border_color(color_type:Byte):TVkBorderColor;
begin
 Case color_type of
  TEX_BorderColor_TransparentBlack:Result:=VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK; //VK_BORDER_COLOR_INT_TRANSPARENT_BLACK
  TEX_BorderColor_OpaqueBlack     :Result:=VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK;      //VK_BORDER_COLOR_INT_OPAQUE_BLACK
  TEX_BorderColor_OpaqueWhite     :Result:=VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE;      //VK_BORDER_COLOR_INT_OPAQUE_WHITE
  TEX_BorderColor_Register        :Result:=VK_BORDER_COLOR_FLOAT_CUSTOM_EXT;        //VK_BORDER_COLOR_INT_CUSTOM_EXT
  else
                                   Result:=VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK;
 end;
end;

function _get_ssharp_info(PS:PSSharpResource4):TVkSamplerCreateInfo;
begin
 Result:=Default(TVkSamplerCreateInfo);
 if (PS=nil) then Exit;

 Result.sType:=VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;

 Result.magFilter:=_get_xy_filter(PS^.xy_mag_filter);
 Result.minFilter:=_get_xy_filter(PS^.xy_min_filter);

 Result.mipmapMode:=_get_mip_filter(PS^.mip_filter);

 Result.addressModeU:=_get_clamp(PS^.clamp_x);
 Result.addressModeV:=_get_clamp(PS^.clamp_y);
 Result.addressModeW:=_get_clamp(PS^.clamp_z);

 Result.mipLodBias:=_get_lod_bias(PS^.lod_bias,PS^.lod_bias_sec);

 Result.anisotropyEnable:=_is_aniso_enabled(PS^.xy_mag_filter,PS^.xy_min_filter);

 Result.maxAnisotropy:=_get_aniso_ratio(PS^.max_aniso_ratio);

 Result.compareEnable:=ord(PS^.depth_compare_func<>SQ_TEX_DEPTH_COMPARE_NEVER);
 Result.compareOp    :=TVkCompareOp(PS^.depth_compare_func); //1:1

 Result.minLod:=_get_lod(PS^.min_lod);
 Result.maxLod:=_get_lod(PS^.max_lod);

 Result.borderColor:=_get_border_color(PS^.border_color_type);

 Result.unnormalizedCoordinates:=PS^.force_unorm_coords;
end;

end.




