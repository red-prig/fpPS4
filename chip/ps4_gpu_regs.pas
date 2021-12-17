unit ps4_gpu_regs;

{$mode objfpc}{$H+}

interface

uses
 Classes,
 SysUtils,
 vulkan,
 bittype,
 pm4defs,
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

 TRT_INFO=record
  Addr:Pointer;

  extend:TVkExtent2D;
  padded:TVkExtent2D;

  cformat:TVkFormat;
  TILE_MODE_INDEX:DWORD;

  COMP_SWAP :Byte;
  FAST_CLEAR:Boolean;

  CLEAR_COLOR:TVkClearColorValue;

  blend:TVkPipelineColorBlendAttachmentState;
 end;

 TDB_INFO=record

  Z_READ_ADDR:Pointer;
  Z_WRITE_ADDR:Pointer;

  STENCIL_READ_ADDR:Pointer;
  STENCIL_WRITE_ADDR:Pointer;

  extend:TVkExtent2D;
  padded:TVkExtent2D;

  DEPTH_CLEAR   :Boolean;
  STENCIL_CLEAR :Boolean;

  Z_READ_ONLY      :Boolean;
  STENCIL_READ_ONLY:Boolean;

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

  dformat:TVkFormat;

  zorder_stage:TVkPipelineStageFlagBits;

 end;

 TGPU_REGS=packed object
  RENDER_TARGET:array[0..7] of TRENDER_TARGET;
  TARGET_MASK:TCB_TARGET_MASK;
  VTE_CNTL:TPA_CL_VTE_CNTL;

  SC_MODE_CNTL_0:TPA_SC_MODE_CNTL_0;
  SC_MODE_CNTL_1:TPA_SC_MODE_CNTL_1;

  VPORT_SCISSOR:array[0..15] of TVPORT_SCISSOR;

  VPORT_ZMIN_MAX:array[0..15] of TVPORT_ZMIN_MAX;

  VPORT_SCALE_OFFSET:array[0..15] of TVPORT_SCALE_OFFSET;

  SCREEN_SCISSOR_BR:TPA_SC_SCREEN_SCISSOR_BR;
  SCREEN_SCISSOR_TL:TPA_SC_SCREEN_SCISSOR_TL;

  SC_AA_CONFIG:TPA_SC_AA_CONFIG;
  SC_AA_MASK_X0Y0_X1Y0:TPA_SC_AA_MASK_X0Y0_X1Y0;
  SC_AA_MASK_X0Y1_X1Y1:TPA_SC_AA_MASK_X0Y1_X1Y1;

  HARDWARE_SCREEN_OFFSET:TPA_SU_HARDWARE_SCREEN_OFFSET;
  SU_LINE_CNTL:TPA_SU_LINE_CNTL;
  SU_POINT_SIZE:TPA_SU_POINT_SIZE;
  SU_POINT_MINMAX:TPA_SU_POINT_MINMAX;

  VTX_CNTL:TPA_SU_VTX_CNTL;

  GB_CLIP:TGB_CLIP;
  CL_CLIP_CNTL:TPA_CL_CLIP_CNTL;
  SC_CLIPRECT_RULE:TPA_SC_CLIPRECT_RULE;

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

  VGT_PRIMITIVE_TYPE:TVGT_PRIMITIVE_TYPE;
  VGT_INDEX_TYPE    :TVGT_INDEX_TYPE    ;
  VGT_NUM_INSTANCES :TVGT_NUM_INSTANCES ;

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
    INPUT_CNTL_0:TSPI_PS_INPUT_CNTL_0;
    INPUT_CNTL_1:TSPI_PS_INPUT_CNTL_1;

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

  end;


  CB_COLOR_CONTROL:TCB_COLOR_CONTROL;

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
  Procedure ClearDMA;
 end;

implementation

Function TGPU_REGS._SHADER_MASK(i:Byte):Byte; inline; //0..7
begin
 Result:=(DWORD(SPI.PS.SHADER_MASK) shr i) and 15;
end;

Function TGPU_REGS._TARGET_MASK(i:Byte):Byte; inline; //0..7
begin
 Result:=(DWORD(TARGET_MASK) shr i) and 15;
end;

Function TGPU_REGS._COMP_MASK(i:Byte):Byte; inline;  //0..7
begin
 Result:=((DWORD(SPI.PS.SHADER_MASK) and DWORD(TARGET_MASK)) shr i) and 15;
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
 if (SC_MODE_CNTL_0.VPORT_SCISSOR_ENABLE=1) then
 begin
  Result.offset.x     :=_fix_scissor_range(VPORT_SCISSOR[i].TL.TL_X);
  Result.offset.y     :=_fix_scissor_range(VPORT_SCISSOR[i].TL.TL_Y);
  Result.extent.width :=_fix_scissor_range(VPORT_SCISSOR[i].BR.BR_X);
  Result.extent.height:=_fix_scissor_range(VPORT_SCISSOR[i].BR.BR_Y);
  Result.extent.width :=Result.extent.width -Result.offset.x;
  Result.extent.height:=Result.extent.height-Result.offset.y;
 end else
 begin
  Result.offset.x     :=_fix_scissor_range(SCREEN_SCISSOR_TL.TL_X);
  Result.offset.y     :=_fix_scissor_range(SCREEN_SCISSOR_TL.TL_Y);
  Result.extent.width :=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_X);
  Result.extent.height:=_fix_scissor_range(SCREEN_SCISSOR_BR.BR_Y);
  Result.extent.width :=Result.extent.width -Result.offset.x;
  Result.extent.height:=Result.extent.height-Result.offset.y;
 end;
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

 if (RENDER_TARGET[i].INFO.BLEND_BYPASS=1) then
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
  Result.srcAlphaBlendFactor:=GetBlendFactor(CB_BLEND_CONTROL[i].ALPHA_SRCBLEND);
  Result.dstAlphaBlendFactor:=GetBlendFactor(CB_BLEND_CONTROL[i].ALPHA_DESTBLEND);

  Result.colorBlendOp:=GetBlendOp(CB_BLEND_CONTROL[i].COLOR_COMB_FCN);
  Result.alphaBlendOp:=GetBlendOp(CB_BLEND_CONTROL[i].ALPHA_COMB_FCN);

  Assert(CB_BLEND_CONTROL[i].SEPARATE_ALPHA_BLEND=0);
  Assert(CB_BLEND_CONTROL[i].DISABLE_ROP3        =0);
 end;

 //Assert(CB_COLOR_CONTROL.ROP3 = 204);

end;

Function TGPU_REGS.GET_RT_INFO(i:Byte):TRT_INFO; //0..7
var
 COMP_MAP:TCOMP_MAP;
 W:QWORD;
begin
 Result:=Default(TRT_INFO);

 Result.Addr:=Pointer(QWORD(RENDER_TARGET[i].BASE) shl 8);
 if (RENDER_TARGET[i].INFO.LINEAR_GENERAL=1) then
 begin
  Result.Addr:=Pointer(QWORD(Result.Addr) or Byte(RENDER_TARGET[i].VIEW.SLICE_START));
 end;

 Result.extend:=GET_SCREEN_SIZE;

 Result.padded.Width :=(RENDER_TARGET[i].PITCH.TILE_MAX+1)*8;
 Result.padded.Height:=(RENDER_TARGET[i].SLICE.TILE_MAX+1)*8 div (RENDER_TARGET[i].PITCH.TILE_MAX+1);

 Assert(RENDER_TARGET[i].INFO.ENDIAN=ENDIAN_NONE);
 //Assert(RENDER_TARGET[i].INFO.COMPRESSION=0);  //FMASK and MSAA

 Case RENDER_TARGET[i].INFO.FORMAT of
  COLOR_8_8_8_8:
   Case RENDER_TARGET[i].INFO.NUMBER_TYPE of
    NUMBER_UNORM:Result.cformat:=VK_FORMAT_R8G8B8A8_UNORM;
    NUMBER_SRGB :Result.cformat:=VK_FORMAT_R8G8B8A8_SRGB;
    else
     Assert(false);
   end;
  else
   Assert(false);
 end;

 Result.TILE_MODE_INDEX:=RENDER_TARGET[i].ATTRIB.TILE_MODE_INDEX;
 if (RENDER_TARGET[i].INFO.LINEAR_GENERAL=1) then Result.TILE_MODE_INDEX:=8;

 Result.COMP_SWAP:=RENDER_TARGET[i].INFO.COMP_SWAP;

 if (RENDER_TARGET[i].INFO.FAST_CLEAR=1) then
 begin
  Result.FAST_CLEAR:=True;

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

 end;

 Result.blend:=GET_RT_BLEND(i);

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

 Result.extend:=GET_SCREEN_SIZE;

 Result.padded.width :=(DEPTH.DEPTH_SIZE.PITCH_TILE_MAX +1)*8;
 Result.padded.height:=(DEPTH.DEPTH_SIZE.HEIGHT_TILE_MAX+1)*8;


 Result.DEPTH_CLEAR  :=DEPTH.RENDER_CONTROL.DEPTH_CLEAR_ENABLE<>0;
 Result.STENCIL_CLEAR:=DEPTH.RENDER_CONTROL.STENCIL_CLEAR_ENABLE<>0;

 Result.Z_READ_ONLY      :=DEPTH.DEPTH_VIEW.Z_READ_ONLY<>0;
 Result.STENCIL_READ_ONLY:=DEPTH.DEPTH_VIEW.STENCIL_READ_ONLY<>0;

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
    Result.dformat:=VK_FORMAT_S8_UINT;
   end;
  Z_16      :
   if (DEPTH.STENCIL_INFO.FORMAT=STENCIL_8) then
   begin
    Result.dformat:=VK_FORMAT_D16_UNORM_S8_UINT;
   end else
   begin
    Result.dformat:=VK_FORMAT_D16_UNORM;
   end;
  Z_24      :
   if (DEPTH.STENCIL_INFO.FORMAT=STENCIL_8) then
   begin
    Result.dformat:=VK_FORMAT_D24_UNORM_S8_UINT;
   end else
   begin
    Result.dformat:=VK_FORMAT_X8_D24_UNORM_PACK32;
   end;
  Z_32_FLOAT:
   if (DEPTH.STENCIL_INFO.FORMAT=STENCIL_8) then
   begin
    Result.dformat:=VK_FORMAT_D32_SFLOAT_S8_UINT;
   end else
   begin
    Result.dformat:=VK_FORMAT_D32_SFLOAT;
   end;
 end;

 Result.Z_READ_ADDR :=Pointer(QWORD(DEPTH.Z_READ_BASE) shl 8);
 Result.Z_WRITE_ADDR:=Pointer(QWORD(DEPTH.Z_WRITE_BASE) shl 8);

 Result.STENCIL_READ_ADDR :=Pointer(QWORD(DEPTH.STENCIL_READ_BASE) shl 8);
 Result.STENCIL_WRITE_ADDR:=Pointer(QWORD(DEPTH.STENCIL_WRITE_BASE) shl 8);

 Assert(SPI.PS.SHADER_CONTROL.Z_EXPORT_ENABLE=0);
 Assert(SPI.PS.SHADER_CONTROL.STENCIL_TEST_VAL_EXPORT_ENABLE=0);

 Case SPI.PS.SHADER_CONTROL.Z_ORDER of
  LATE_Z             :Result.zorder_stage:=VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT;
  EARLY_Z_THEN_LATE_Z:Result.zorder_stage:=TVkPipelineStageFlagBits(
                                            ord(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or
                                            ord(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT));
  RE_Z               :Result.zorder_stage:=VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
  EARLY_Z_THEN_RE_Z  :Result.zorder_stage:=VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
 end;

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

 DWORD(SPI.CS.STATIC_THREAD_MGMT_SE0):=$FFFFFFFF;
 DWORD(SPI.CS.STATIC_THREAD_MGMT_SE1):=$FFFFFFFF;

end;

Procedure TGPU_REGS.ClearDMA;
begin
 FillChar(VGT_DMA,SizeOf(VGT_DMA),0);
end;

end.




