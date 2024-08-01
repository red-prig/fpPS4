unit vRegs2Vulkan;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 bittype,
 half16,
 sys_bootparam,
 Vulkan,
 vImage,
 vShader,
 ps4_shader,
 pm4defs,
 si_ci_vi_merged_offset,
 si_ci_vi_merged_enum,
 si_ci_vi_merged_registers,
 si_ci_vi_merged_groups;

type
 TRT_INFO=record

  attachment:TVkUInt32;

  FImageInfo:TvImageKey;
  FImageView:TvImageViewKey;

  COMP_SWAP :Byte;

  IMAGE_USAGE:Byte;

  CLEAR_COLOR:TVkClearColorValue;

  blend:TVkPipelineColorBlendAttachmentState;
 end;

 THTILE_INFO=record
  KEY                :TvImageKey;
  SIZE               :Ptruint;
  LINEAR             :Byte;
  TC_COMPATIBLE      :Byte;
  TILE_SURFACE_ENABLE:Byte;
  TILE_STENCIL_ENABLE:Byte;
 end;

 TDB_INFO=record

  Z_READ_ADDR :Pointer;
  Z_WRITE_ADDR:Pointer;

  STENCIL_READ_ADDR :Pointer;
  STENCIL_WRITE_ADDR:Pointer;

  CLEAR_VALUE:TVkClearValue;

  ds_state:TVkPipelineDepthStencilStateCreateInfo;

  DEPTH_USAGE  :Byte;
  STENCIL_USAGE:Byte;

  FImageInfo:TvImageKey;

  zorder_stage:TVkPipelineStageFlags;

  HTILE_INFO:THTILE_INFO;
 end;

 TBLEND_INFO=packed record
  logicOp       :TVkLogicOp;
  blendConstants:array[0..3] of TVkFloat;
 end;

 PSH_REG_GFX_GROUP    =^TSH_REG_GFX_GROUP;     // 0x2C00
 PSH_REG_COMPUTE_GROUP=^TSH_REG_COMPUTE_GROUP; // 0x2E00
 PCONTEXT_REG_GROUP   =^TCONTEXT_REG_GROUP;    // 0xA000
 PUSERCONFIG_REG_SHORT=^TUSERCONFIG_REG_SHORT;

 PGPU_USERDATA=^TGPU_USERDATA;

 PGPU_REGS=^TGPU_REGS;
 TGPU_REGS=packed object
  SG_REG:PSH_REG_GFX_GROUP;     // 0x2C00
  SC_REG:PSH_REG_COMPUTE_GROUP; // 0x2E00
  CX_REG:PCONTEXT_REG_GROUP;    // 0xA000
  UC_REG:PUSERCONFIG_REG_SHORT; // 0xC000

  Function  _SHADER_MASK(i:Byte):Byte; inline;  //0..7
  Function  _TARGET_MASK(i:Byte):Byte; inline;  //0..7
  Function  _COMP_MASK(i:Byte):Byte;   inline;  //0..7
  Function  COMP_ENABLE:Boolean; inline;
  Function  RT_ENABLE(i:Byte):Boolean; //0..7
  Function  GET_HI_RT:Byte;            //0..7
  Function  VP_ENABLE(i:Byte):Boolean; //0..15
  Function  GET_VPORT(i:Byte):TVkViewport; //0..15
  Function  GET_SCISSOR(i:Byte):TVkRect2D; //0..15
  Function  GET_SCREEN:TVkRect2D;
  Function  GET_SCREEN_SIZE:TVkExtent2D;
  Function  GET_RT_BLEND(i:Byte):TVkPipelineColorBlendAttachmentState; //0..7
  Function  GET_BLEND_INFO:TBLEND_INFO;
  Function  GET_RT_INFO(i:Byte):TRT_INFO; //0..7
  Function  DB_ENABLE:Boolean;
  Function  GET_DB_INFO:TDB_INFO;

  Function  GET_RASTERIZATION:TVkPipelineRasterizationStateCreateInfo;
  Function  GET_PROVOKING:TVkProvokingVertexModeEXT;
  Function  GET_MULTISAMPLE:TVkPipelineMultisampleStateCreateInfo;

  function  GET_PRIM_RESET:TVkBool32;
  function  GET_PRIM_TYPE :TVkPrimitiveTopology;
  function  GET_INDEX_TYPE:TVkIndexType;

  function  get_reg(i:word):DWORD;

  Function  get_code_addr(FStage:TvShaderStage):Pointer;
  Function  get_user_data(FStage:TvShaderStage):Pointer;
  procedure export_user_data_rt(dst:PGPU_USERDATA);
  procedure export_user_data_cs(dst:PGPU_USERDATA);
 end;

 TGPU_USERDATA=packed object
  A:array[TvShaderStage] of TSPI_USER_DATA;
  Function get_user_data(FStage:TvShaderStage):Pointer;
 end;

function GET_INDEX_TYPE_SIZE(i:TVkIndexType):Byte;

//

function _get_vsharp_cformat(PV:PVSharpResource4):TVkFormat;
function _get_tsharp4_cformat(PT:PTSharpResource4):TVkFormat;

function _get_tsharp4_image_info(PT:PTSharpResource4):TvImageKey;
function _get_tsharp8_image_info(PT:PTSharpResource8):TvImageKey;

function _get_tsharp4_image_view(PT:PTSharpResource4):TvImageViewKey;
function _get_tsharp8_image_view(PT:PTSharpResource8):TvImageViewKey;

function _get_ssharp_info(PS:PSSharpResource4):TVkSamplerCreateInfo;

implementation

uses
 ps4_Tiling;

Function TGPU_REGS._SHADER_MASK(i:Byte):Byte; inline; //0..7
begin
 Result:=(DWORD(CX_REG^.CB_SHADER_MASK) shr (i shl 2)) and 15;
end;

Function TGPU_REGS._TARGET_MASK(i:Byte):Byte; inline; //0..7
begin
 Result:=(DWORD(CX_REG^.CB_TARGET_MASK) shr (i shl 2)) and 15;
end;

Function TGPU_REGS._COMP_MASK(i:Byte):Byte; inline;  //0..7
begin
 Result:=((DWORD(CX_REG^.CB_SHADER_MASK) and DWORD(CX_REG^.CB_TARGET_MASK)) shr (i shl 2)) and 15;
end;

Function TGPU_REGS.COMP_ENABLE:Boolean; inline;
begin
 Result:=(DWORD(CX_REG^.CB_SHADER_MASK) and DWORD(CX_REG^.CB_TARGET_MASK))<>0;
end;

Function TGPU_REGS.RT_ENABLE(i:Byte):Boolean; //0..7
begin
 Result:=(CX_REG^.RENDER_TARGET[i].BASE<>0) and
         (CX_REG^.RENDER_TARGET[i].INFO.FORMAT<>0) and
         (_COMP_MASK(i)<>0);
end;

Function TGPU_REGS.GET_HI_RT:Byte; //0..7
var
 i:Byte;
begin
 Result:=0;
 For i:=1 to 7 do
 begin
  if RT_ENABLE(i) then
  begin
   Result:=i;
  end;
 end;
end;

Function TGPU_REGS.VP_ENABLE(i:Byte):Boolean; //0..15
begin
 Result:=(PQWORD(@CX_REG^.PA_CL_VPORT_SCALE_OFFSET[i])[0]<>0) or
         (PQWORD(@CX_REG^.PA_CL_VPORT_SCALE_OFFSET[i])[1]<>0) or
         (PQWORD(@CX_REG^.PA_CL_VPORT_SCALE_OFFSET[i])[2]<>0);
end;

Function TGPU_REGS.GET_VPORT(i:Byte):TVkViewport; //0..15
var
 V:TVPORT_SCALE_OFFSET;
 VTE_CNTL:TPA_CL_VTE_CNTL;
begin
 Result:=Default(TVkViewport);
 V:=CX_REG^.PA_CL_VPORT_SCALE_OFFSET[i];

 VTE_CNTL:=CX_REG^.PA_CL_VTE_CNTL;

 if (VTE_CNTL.VPORT_X_SCALE_ENA =0) then V.XSCALE :=1;
 if (VTE_CNTL.VPORT_X_OFFSET_ENA=0) then V.XOFFSET:=0;
 if (VTE_CNTL.VPORT_Y_SCALE_ENA =0) then V.YSCALE :=1;
 if (VTE_CNTL.VPORT_Y_OFFSET_ENA=0) then V.YOFFSET:=0;
 if (VTE_CNTL.VPORT_Z_SCALE_ENA =0) then V.ZSCALE :=1;
 if (VTE_CNTL.VPORT_Z_OFFSET_ENA=0) then V.ZOFFSET:=0;

 Assert(VTE_CNTL.VTX_XY_FMT=0,'VTE_CNTL.VTX_XY_FMT');
 Assert(VTE_CNTL.VTX_Z_FMT =0,'VTE_CNTL.VTX_Z_FMT' );
 Assert(VTE_CNTL.VTX_W0_FMT=1,'VTE_CNTL.VTX_W0_FMT');

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

 if (CX_REG^.PA_SC_MODE_CNTL_0.VPORT_SCISSOR_ENABLE<>0) and
    ((DWORD(CX_REG^.PA_SC_VPORT_SCISSOR[i].TL)<>0) or
     (DWORD(CX_REG^.PA_SC_VPORT_SCISSOR[i].BR)<>0)) then
 begin
  Result.offset.x     :=_fix_scissor_range(CX_REG^.PA_SC_VPORT_SCISSOR[i].TL.TL_X);
  Result.offset.y     :=_fix_scissor_range(CX_REG^.PA_SC_VPORT_SCISSOR[i].TL.TL_Y);
  Result.extent.width :=_fix_scissor_range(CX_REG^.PA_SC_VPORT_SCISSOR[i].BR.BR_X);
  Result.extent.height:=_fix_scissor_range(CX_REG^.PA_SC_VPORT_SCISSOR[i].BR.BR_Y);
 end else
 begin
  Result.offset.x     :=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_TL.TL_X);
  Result.offset.y     :=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_TL.TL_Y);
  Result.extent.width :=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_BR.BR_X);
  Result.extent.height:=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_BR.BR_Y);
 end;

 Result.extent.width :=Result.extent.width -Result.offset.x;
 Result.extent.height:=Result.extent.height-Result.offset.y;
end;

Function TGPU_REGS.GET_SCREEN:TVkRect2D;
begin
 Result.offset.x     :=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_TL.TL_X);
 Result.offset.y     :=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_TL.TL_Y);
 Result.extent.width :=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_BR.BR_X);
 Result.extent.height:=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_BR.BR_Y);
 Result.extent.width :=Result.extent.width -Result.offset.x;
 Result.extent.height:=Result.extent.height-Result.offset.y;
end;

Function TGPU_REGS.GET_SCREEN_SIZE:TVkExtent2D;
begin
 Result.width :=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_BR.BR_X);
 Result.height:=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_BR.BR_Y);
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
   Assert(false,'Unknow blend factor:0x'+HexStr(i,1));
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
   Assert(false,'Unknow blend op:0x'+HexStr(i,1));
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

 {
  Match the physical representation of the final pixel (RGBA)
   to the output component number in shader export     (0123)
 }

 //SWAP_STD      (R=>0)
 //SWAP_ALT      (G=>0)
 //SWAP_STD_REV  (B=>0)
 //SWAP_ALT_REV  (A=>0)

 //SWAP_STD      (R=>0, G=>1)
 //SWAP_ALT      (R=>0, A=>1)
 //SWAP_STD_REV  (G=>0, R=>1)
 //SWAP_ALT_REV  (A=>0, R=>1)

 //SWAP_STD      (R=>0, G=>1, B=>2)
 //SWAP_ALT      (R=>0, G=>1, A=>2)
 //SWAP_STD_REV  (B=>0, G=>1, R=>2)
 //SWAP_ALT_REV  (A=>0, G=>1, R=>2)

 //SWAP_STD      (R=>0, G=>1, B=>2, A=>3)
 //SWAP_ALT      (B=>0, G=>1, R=>2, A=>3)
 //SWAP_STD_REV  (A=>0, B=>1, G=>2, R=>3)
 //SWAP_ALT_REV  (A=>0, R=>1, G=>2, B=>3)
end;

const
 VK_SWIZZLE_I=ord(VK_COMPONENT_SWIZZLE_IDENTITY);
 VK_SWIZZLE_Z=ord(VK_COMPONENT_SWIZZLE_ZERO    );
 VK_SWIZZLE_O=ord(VK_COMPONENT_SWIZZLE_ONE     );
 VK_SWIZZLE_R=ord(VK_COMPONENT_SWIZZLE_R       );
 VK_SWIZZLE_G=ord(VK_COMPONENT_SWIZZLE_G       );
 VK_SWIZZLE_B=ord(VK_COMPONENT_SWIZZLE_B       );
 VK_SWIZZLE_A=ord(VK_COMPONENT_SWIZZLE_A       );

 shader_swizzle_map:array[1..4,SWAP_STD..SWAP_ALT_REV] of TvDstSel=(
  (
   (r:VK_SWIZZLE_R;g:VK_SWIZZLE_O;b:VK_SWIZZLE_O;a:VK_SWIZZLE_O),
   (r:VK_SWIZZLE_G;g:VK_SWIZZLE_O;b:VK_SWIZZLE_O;a:VK_SWIZZLE_O),
   (r:VK_SWIZZLE_B;g:VK_SWIZZLE_O;b:VK_SWIZZLE_O;a:VK_SWIZZLE_O),
   (r:VK_SWIZZLE_A;g:VK_SWIZZLE_O;b:VK_SWIZZLE_O;a:VK_SWIZZLE_O)
  ),(
   (r:VK_SWIZZLE_R;g:VK_SWIZZLE_G;b:VK_SWIZZLE_O;a:VK_SWIZZLE_O),
   (r:VK_SWIZZLE_R;g:VK_SWIZZLE_A;b:VK_SWIZZLE_O;a:VK_SWIZZLE_O),
   (r:VK_SWIZZLE_G;g:VK_SWIZZLE_R;b:VK_SWIZZLE_O;a:VK_SWIZZLE_O),
   (r:VK_SWIZZLE_A;g:VK_SWIZZLE_R;b:VK_SWIZZLE_O;a:VK_SWIZZLE_O)
  ),(
   (r:VK_SWIZZLE_R;g:VK_SWIZZLE_G;b:VK_SWIZZLE_B;a:VK_SWIZZLE_O),
   (r:VK_SWIZZLE_R;g:VK_SWIZZLE_G;b:VK_SWIZZLE_A;a:VK_SWIZZLE_O),
   (r:VK_SWIZZLE_B;g:VK_SWIZZLE_G;b:VK_SWIZZLE_R;a:VK_SWIZZLE_O),
   (r:VK_SWIZZLE_A;g:VK_SWIZZLE_G;b:VK_SWIZZLE_R;a:VK_SWIZZLE_O)
  ),(
   (r:VK_SWIZZLE_R;g:VK_SWIZZLE_G;b:VK_SWIZZLE_B;a:VK_SWIZZLE_A),
   (r:VK_SWIZZLE_B;g:VK_SWIZZLE_G;b:VK_SWIZZLE_R;a:VK_SWIZZLE_A),
   (r:VK_SWIZZLE_A;g:VK_SWIZZLE_B;b:VK_SWIZZLE_G;a:VK_SWIZZLE_R),
   (r:VK_SWIZZLE_A;g:VK_SWIZZLE_R;b:VK_SWIZZLE_G;a:VK_SWIZZLE_B)
  )
 );

Function TGPU_REGS.GET_RT_BLEND(i:Byte):TVkPipelineColorBlendAttachmentState; //0..7
var
 RENDER_TARGET:TRENDER_TARGET;
 BLEND_CONTROL:TCB_BLEND0_CONTROL;
 COMP_MAP:TCOMP_MAP;
 m:Byte;
begin
 Result:=Default(TVkPipelineColorBlendAttachmentState);

 RENDER_TARGET:=CX_REG^.RENDER_TARGET[i];
 BLEND_CONTROL:=CX_REG^.CB_BLEND_CONTROL[i];

 m:=GetRTCompCount(RENDER_TARGET.INFO.FORMAT);
 COMP_MAP:=GetCompMap(RENDER_TARGET.INFO.COMP_SWAP,m);

 //COMP_SWAP depend  (B=>0, G=>1, R=>2, A=>3)
 m:=_COMP_MASK(i);
 if m.TestBit(0) then Result.colorWriteMask:=Result.colorWriteMask or COMP_MAP[0];
 if m.TestBit(1) then Result.colorWriteMask:=Result.colorWriteMask or COMP_MAP[1];
 if m.TestBit(2) then Result.colorWriteMask:=Result.colorWriteMask or COMP_MAP[2];
 if m.TestBit(3) then Result.colorWriteMask:=Result.colorWriteMask or COMP_MAP[3];

 //BLEND_CLAMP

 if (RENDER_TARGET.INFO.BLEND_BYPASS<>0) then
 begin
  Result.blendEnable:=VK_FALSE;
 end else
 if (CX_REG^.CB_BLEND_CONTROL[i].ENABLE=0) then
 begin
  Result.blendEnable:=VK_FALSE;
 end else
 begin
  Result.blendEnable:=VK_TRUE;

  Result.srcColorBlendFactor:=GetBlendFactor(BLEND_CONTROL.COLOR_SRCBLEND);
  Result.dstColorBlendFactor:=GetBlendFactor(BLEND_CONTROL.COLOR_DESTBLEND);
  Result.colorBlendOp       :=GetBlendOp    (BLEND_CONTROL.COLOR_COMB_FCN);

  if (BLEND_CONTROL.SEPARATE_ALPHA_BLEND=0) then
  begin
   Result.srcAlphaBlendFactor:=Result.srcColorBlendFactor;
   Result.dstAlphaBlendFactor:=Result.dstColorBlendFactor;
   Result.alphaBlendOp       :=Result.colorBlendOp;
  end else //VkPhysicalDeviceFeatures.independentBlend
  begin
   Result.srcAlphaBlendFactor:=GetBlendFactor(BLEND_CONTROL.ALPHA_SRCBLEND);
   Result.dstAlphaBlendFactor:=GetBlendFactor(BLEND_CONTROL.ALPHA_DESTBLEND);
   Result.alphaBlendOp       :=GetBlendOp    (BLEND_CONTROL.ALPHA_COMB_FCN);
  end;

 end;

end;

function get_logic_op(ROP3:Byte):TVkLogicOp;
begin
 Result:=VK_LOGIC_OP_COPY;
 case ROP3 of
  $00:Result:=VK_LOGIC_OP_CLEAR;
  $88:Result:=VK_LOGIC_OP_AND;
  $44:Result:=VK_LOGIC_OP_AND_REVERSE;
  $CC:Result:=VK_LOGIC_OP_COPY;
  $22:Result:=VK_LOGIC_OP_AND_INVERTED;
  $AA:Result:=VK_LOGIC_OP_NO_OP;
  $66:Result:=VK_LOGIC_OP_XOR;
  $EE:Result:=VK_LOGIC_OP_OR;
  $11:Result:=VK_LOGIC_OP_NOR;
  $99:Result:=VK_LOGIC_OP_EQUIVALENT;
  $55:Result:=VK_LOGIC_OP_INVERT;
  $DD:Result:=VK_LOGIC_OP_OR_REVERSE;
  $33:Result:=VK_LOGIC_OP_COPY_INVERTED;
  $BB:Result:=VK_LOGIC_OP_OR_INVERTED;
  $77:Result:=VK_LOGIC_OP_NAND;
  $FF:Result:=VK_LOGIC_OP_SET;
  else
      begin
       Writeln(stderr,'unknow logic op:0x',HexStr(ROP3,2));
      end;
 end;
end;

Function TGPU_REGS.GET_BLEND_INFO:TBLEND_INFO;
begin
 Result.logicOp:=get_logic_op(CX_REG^.CB_COLOR_CONTROL.ROP3);

 Result.blendConstants[0]:=CX_REG^.CB_BLEND_RGBA[0];
 Result.blendConstants[1]:=CX_REG^.CB_BLEND_RGBA[1];
 Result.blendConstants[2]:=CX_REG^.CB_BLEND_RGBA[2];
 Result.blendConstants[3]:=CX_REG^.CB_BLEND_RGBA[3];
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

function _conv_clr_to_float(num:Byte;value,mask:qword):DWORD;
var
 s:Single;
 i:Integer;
begin
 Result:=0;
 Case num of
  NUMBER_UNORM,
  NUMBER_SRGB:
   begin
    s:=(value and mask)/mask;
    Result:=PDWORD(@s)^;
   end;
  NUMBER_SNORM:
   begin
    i:=0;

    Case mask of
         $FF:i:=PShortInt(@value)^;
       $FFFF:i:=PSmallInt(@value)^;
     $FFFFFF:i:=PInteger (@value)^;
    end;

    s:=i/(mask div 2);
    Result:=PDWORD(@s)^;
   end;
  NUMBER_UINT,
  NUMBER_USCALED:
   begin
    Result:=(value and mask);
   end;
  NUMBER_SINT,
  NUMBER_SSCALED:
   begin
    i:=0;

    Case mask of
         $FF:i:=PShortInt(@value)^;
       $FFFF:i:=PSmallInt(@value)^;
     $FFFFFF:i:=PInteger (@value)^;
     else;
    end;

    Result:=PDWORD(@i)^;
   end;
  NUMBER_FLOAT:
   begin

    Case mask of
       $FFFF:
      begin
       s:=PHalf16(@value)^;
       Result:=PDWORD(@s)^;
      end;
     $FFFFFF:Result:=value;
     else;
    end;

   end;

  else;
 end;
end;

type
 TCLR_2_10_10_10=bitpacked record
  A:bit2;
  R:bit10;
  G:bit10;
  B:bit10;
 end;

procedure _conv_clr_2_10_10_10(num:Byte;value:dword;var clr:TVkClearColorValue);
var
 i:array[0..3] of Integer;
begin
 i[0]:=TCLR_2_10_10_10(value).R;
 i[1]:=TCLR_2_10_10_10(value).G;
 i[2]:=TCLR_2_10_10_10(value).B;
 i[3]:=TCLR_2_10_10_10(value).A;

 if (num=NUMBER_SNORM) then
 begin
  Assert(false,'TODO NUMBER_SNORM');
 end;

 clr.float32[0]:=i[0];
 clr.float32[1]:=i[1];
 clr.float32[2]:=i[2];
 clr.float32[3]:=i[3];
end;

function GetRenderTargetFormat(FORMAT,NUMBER_TYPE,TILING_ID:Byte):TVkFormat;
begin
 Result:=VK_FORMAT_UNDEFINED;

 Case FORMAT of
  COLOR_8:
   Case NUMBER_TYPE of
    NUMBER_UNORM:Result:=VK_FORMAT_R8_UNORM;
    NUMBER_SNORM:Result:=VK_FORMAT_R8_SNORM;
    NUMBER_UINT :Result:=VK_FORMAT_R8_UINT;
    NUMBER_SINT :Result:=VK_FORMAT_R8_SINT;
    else;
   end;
  COLOR_8_8:
   Case NUMBER_TYPE of
    NUMBER_UNORM:Result:=VK_FORMAT_R8G8_UNORM;
    NUMBER_SNORM:Result:=VK_FORMAT_R8G8_SNORM;
    NUMBER_UINT :Result:=VK_FORMAT_R8G8_UINT;
    NUMBER_SINT :Result:=VK_FORMAT_R8G8_SINT;
    else;
   end;
  COLOR_8_8_8_8:
   Case NUMBER_TYPE of
    NUMBER_UNORM:Result:=VK_FORMAT_R8G8B8A8_UNORM;
    NUMBER_SRGB :Result:=VK_FORMAT_R8G8B8A8_SRGB;
    NUMBER_SNORM:Result:=VK_FORMAT_R8G8B8A8_SNORM;
    NUMBER_UINT :Result:=VK_FORMAT_R8G8B8A8_UINT;
    NUMBER_SINT :Result:=VK_FORMAT_R8G8B8A8_SINT;
    else;
   end;

  COLOR_16:
   Case NUMBER_TYPE of
    NUMBER_UNORM  :Result:=VK_FORMAT_R16_UNORM;
    NUMBER_SNORM  :Result:=VK_FORMAT_R16_SNORM;
    NUMBER_UINT   :Result:=VK_FORMAT_R16_UINT;
    NUMBER_SINT   :Result:=VK_FORMAT_R16_SINT;
    NUMBER_FLOAT  :Result:=VK_FORMAT_R16_SFLOAT;
    else;
   end;
  COLOR_16_16:
   Case NUMBER_TYPE of
    NUMBER_UNORM  :Result:=VK_FORMAT_R16G16_UNORM;
    NUMBER_SNORM  :Result:=VK_FORMAT_R16G16_SNORM;
    NUMBER_UINT   :Result:=VK_FORMAT_R16G16_UINT;
    NUMBER_SINT   :Result:=VK_FORMAT_R16G16_SINT;
    NUMBER_FLOAT  :Result:=VK_FORMAT_R16G16_SFLOAT;
    else;
   end;
  COLOR_16_16_16_16:
   Case NUMBER_TYPE of
    NUMBER_UNORM  :Result:=VK_FORMAT_R16G16B16A16_UNORM;
    NUMBER_SNORM  :Result:=VK_FORMAT_R16G16B16A16_SNORM;
    NUMBER_UINT   :Result:=VK_FORMAT_R16G16B16A16_UINT;
    NUMBER_SINT   :Result:=VK_FORMAT_R16G16B16A16_SINT;
    NUMBER_FLOAT  :Result:=VK_FORMAT_R16G16B16A16_SFLOAT;
    else;
   end;

  COLOR_32:
   if IsTileModeDepth(TILING_ID) then
   begin
    Result:=VK_FORMAT_D32_SFLOAT;
   end else
   Case NUMBER_TYPE of
    NUMBER_UINT   :Result:=VK_FORMAT_R32_UINT;
    NUMBER_SINT   :Result:=VK_FORMAT_R32_SINT;
    NUMBER_FLOAT  :Result:=VK_FORMAT_R32_SFLOAT;
    else;
   end;
  COLOR_32_32:
   Case NUMBER_TYPE of
    NUMBER_UINT   :Result:=VK_FORMAT_R32G32_UINT;
    NUMBER_SINT   :Result:=VK_FORMAT_R32G32_SINT;
    NUMBER_FLOAT  :Result:=VK_FORMAT_R32G32_SFLOAT;
    else;
   end;
  COLOR_32_32_32_32:
   Case NUMBER_TYPE of
    NUMBER_UINT   :Result:=VK_FORMAT_R32G32B32A32_UINT;
    NUMBER_SINT   :Result:=VK_FORMAT_R32G32B32A32_SINT;
    NUMBER_FLOAT  :Result:=VK_FORMAT_R32G32B32A32_SFLOAT;
    else;
   end;

  COLOR_2_10_10_10:
   Case NUMBER_TYPE of
    NUMBER_UNORM  :Result:=VK_FORMAT_A2R10G10B10_UNORM_PACK32;
    else;
   end;

  COLOR_10_11_11: //R:11 G:11 B:10
   Case NUMBER_TYPE of
    NUMBER_FLOAT  :Result:=VK_FORMAT_B10G11R11_UFLOAT_PACK32;
    else;
   end;

  COLOR_11_11_10: //R:10 G:11 B:11
   Case NUMBER_TYPE of
    NUMBER_FLOAT  :Result:=VK_FORMAT_R10G11B11_UFLOAT_FAKE32; //Not directly handled to a vulkan
    else;
   end;

  else;
 end;

 if (Result=VK_FORMAT_UNDEFINED) then
 begin
  Assert(false,'Unknown Render target format:'+IntTostr(FORMAT)+':'+IntTostr(NUMBER_TYPE));
 end;

end;

Function TGPU_REGS.GET_RT_INFO(i:Byte):TRT_INFO; //0..7
var
 RENDER_TARGET:TRENDER_TARGET;
 COMP_MAP:TCOMP_MAP;
 W:QWORD;
 FORMAT:Byte;
 NUMBER_TYPE:Byte;
 tmp:Word;
begin
 Result:=Default(TRT_INFO);

 if not RT_ENABLE(i) then
 begin
  Result.attachment:=VK_ATTACHMENT_UNUSED;

  Result.FImageInfo.cformat           :=VK_FORMAT_R8G8B8A8_UNORM;
  Result.FImageInfo.params.itype      :=ord(VK_IMAGE_TYPE_2D);
  Result.FImageInfo.params.width      :=1;
  Result.FImageInfo.params.height     :=1;
  Result.FImageInfo.params.depth      :=1;
  Result.FImageInfo.params.samples    :=1;
  Result.FImageInfo.params.mipLevels  :=1;
  Result.FImageInfo.params.arrayLayers:=1;

  Result.FImageView.cformat:=VK_FORMAT_R8G8B8A8_UNORM;
  Result.FImageView.vtype  :=ord(VK_IMAGE_VIEW_TYPE_2D);

  Exit;
 end;

 Result.attachment:=i;

 RENDER_TARGET:=CX_REG^.RENDER_TARGET[i];

 Result.FImageInfo.Addr:=Pointer(QWORD(RENDER_TARGET.BASE) shl 8);
 if (RENDER_TARGET.INFO.LINEAR_GENERAL<>0) then
 begin
  Result.FImageInfo.Addr:=Pointer(QWORD(Result.FImageInfo.Addr) or Byte(RENDER_TARGET.VIEW.SLICE_START));
 end;

 Result.FImageInfo.params.width :=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_BR.BR_X);
 Result.FImageInfo.params.height:=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_BR.BR_Y);
 Result.FImageInfo.params.depth :=1;

 tmp:=(RENDER_TARGET.PITCH.TILE_MAX+1);
 Result.FImageInfo.params.pad_width :=tmp*8;
 Result.FImageInfo.params.pad_height:=(RENDER_TARGET.SLICE.TILE_MAX+1)*8 div tmp;

 Assert(RENDER_TARGET.INFO.ENDIAN=ENDIAN_NONE,'ENDIAN:'+IntToStr(RENDER_TARGET.INFO.ENDIAN));
 //Assert(RENDER_TARGET[i].INFO.COMPRESSION=0);  //FMASK and MSAA

 FORMAT     :=RENDER_TARGET.INFO.FORMAT;
 NUMBER_TYPE:=RENDER_TARGET.INFO.NUMBER_TYPE;

 if (RENDER_TARGET.INFO.LINEAR_GENERAL<>0) then
 begin
  Result.FImageInfo.params.tiling.idx:=kTileModeDisplay_LinearGeneral;
 end else
 begin
  Result.FImageInfo.params.tiling.idx:=RENDER_TARGET.ATTRIB.TILE_MODE_INDEX;
 end;

 Result.FImageInfo.cformat:=GetRenderTargetFormat(FORMAT,
                                                  NUMBER_TYPE,
                                                  Result.FImageInfo.params.tiling.idx);

 if (p_neomode<>0) then
 begin
  Result.FImageInfo.params.tiling.alt:=RENDER_TARGET.INFO.ALT_TILE_MODE;
 end;

 Result.FImageInfo.params.itype      :=ord(VK_IMAGE_TYPE_2D);
 Result.FImageInfo.params.samples    :=1 shl (RENDER_TARGET.ATTRIB.NUM_SAMPLES and 3);
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

 Result.COMP_SWAP:=RENDER_TARGET.INFO.COMP_SWAP;

 if (RENDER_TARGET.INFO.FAST_CLEAR<>0) then
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

  Case FORMAT of
   COLOR_8,
   COLOR_8_8,
   COLOR_8_8_8_8:
      begin
       COMP_MAP:=GetCompMap(RENDER_TARGET.INFO.COMP_SWAP,4);

       //VK_COLOR_COMPONENT_R_BIT=$00000001, 0001
       //VK_COLOR_COMPONENT_G_BIT=$00000002, 0010
       //VK_COLOR_COMPONENT_B_BIT=$00000004, 0100
       //VK_COLOR_COMPONENT_A_BIT=$00000008  1000

       W:=RENDER_TARGET.CLEAR_WORD;

       //Writeln((W shr (BsrDWord(COMP_MAP[0]) shl 3)) and 255);
       //Writeln((W shr (BsrDWord(COMP_MAP[1]) shl 3)) and 255);
       //Writeln((W shr (BsrDWord(COMP_MAP[2]) shl 3)) and 255);
       //Writeln((W shr (BsrDWord(COMP_MAP[3]) shl 3)) and 255);

       Result.CLEAR_COLOR.uint32[0]:=_conv_clr_to_float(NUMBER_TYPE,W shr (BsrDWord(COMP_MAP[0]) shl 3),$FF);
       Result.CLEAR_COLOR.uint32[1]:=_conv_clr_to_float(NUMBER_TYPE,W shr (BsrDWord(COMP_MAP[1]) shl 3),$FF);
       Result.CLEAR_COLOR.uint32[2]:=_conv_clr_to_float(NUMBER_TYPE,W shr (BsrDWord(COMP_MAP[2]) shl 3),$FF);
       Result.CLEAR_COLOR.uint32[3]:=_conv_clr_to_float(NUMBER_TYPE,W shr (BsrDWord(COMP_MAP[3]) shl 3),$FF);
      end;

   COLOR_16,
   COLOR_16_16,
   COLOR_16_16_16_16:
      begin
       COMP_MAP:=GetCompMap(RENDER_TARGET.INFO.COMP_SWAP,4);

       W:=RENDER_TARGET.CLEAR_WORD;

       Result.CLEAR_COLOR.uint32[0]:=_conv_clr_to_float(NUMBER_TYPE,W shr (BsrDWord(COMP_MAP[0]) shl 4),$FFFF);
       Result.CLEAR_COLOR.uint32[1]:=_conv_clr_to_float(NUMBER_TYPE,W shr (BsrDWord(COMP_MAP[1]) shl 4),$FFFF);
       Result.CLEAR_COLOR.uint32[2]:=_conv_clr_to_float(NUMBER_TYPE,W shr (BsrDWord(COMP_MAP[2]) shl 4),$FFFF);
       Result.CLEAR_COLOR.uint32[3]:=_conv_clr_to_float(NUMBER_TYPE,W shr (BsrDWord(COMP_MAP[3]) shl 4),$FFFF);
      end;

   COLOR_32,
   COLOR_32_32:
      begin
       COMP_MAP:=GetCompMap(RENDER_TARGET.INFO.COMP_SWAP,4);

       W:=RENDER_TARGET.CLEAR_WORD;

       Result.CLEAR_COLOR.uint32[0]:=_conv_clr_to_float(NUMBER_TYPE,W shr (BsrDWord(COMP_MAP[0]) shl 5),$FFFFFFFF);
       Result.CLEAR_COLOR.uint32[1]:=_conv_clr_to_float(NUMBER_TYPE,W shr (BsrDWord(COMP_MAP[1]) shl 5),$FFFFFFFF);
       Result.CLEAR_COLOR.uint32[2]:=_conv_clr_to_float(NUMBER_TYPE,W shr (BsrDWord(COMP_MAP[2]) shl 5),$FFFFFFFF);
       Result.CLEAR_COLOR.uint32[3]:=_conv_clr_to_float(NUMBER_TYPE,W shr (BsrDWord(COMP_MAP[3]) shl 5),$FFFFFFFF);
      end;

   COLOR_32_32_32_32:; //128bit ignore

   COLOR_2_10_10_10:
    begin
     W:=RENDER_TARGET.CLEAR_WORD;

     _conv_clr_2_10_10_10(NUMBER_TYPE,W,Result.CLEAR_COLOR);
    end

   else
    {Assert(false)}; //TODO
  end;

 if (RENDER_TARGET.ATTRIB.FORCE_DST_ALPHA_1<>0) then
 begin
  Result.FImageView.dstSel.a:=ord(VK_COMPONENT_SWIZZLE_ONE);
 end;

end;

Function TGPU_REGS.DB_ENABLE:Boolean;
begin
 Result:=(
          (CX_REG^.DB_DEPTH_CONTROL.STENCIL_ENABLE<>0) and
          (CX_REG^.DB_STENCIL_INFO.FORMAT<>0)
          ) or
         (
          (CX_REG^.DB_DEPTH_CONTROL.Z_ENABLE<>0) and
          (CX_REG^.DB_Z_INFO.FORMAT<>0)
         );
end;

Function GetStencilOp(b:Byte):TVkStencilOp;
begin
 case b of
  STENCIL_KEEP        :Result:=VK_STENCIL_OP_KEEP;
  STENCIL_ZERO        :Result:=VK_STENCIL_OP_ZERO;
  STENCIL_ONES        :Result:=VK_STENCIL_OP_REPLACE;             //reference=1
  STENCIL_REPLACE_TEST:Result:=VK_STENCIL_OP_REPLACE;             //reference=STENCILREFMASK__.STENCILTESTVAL
  STENCIL_REPLACE_OP  :Result:=VK_STENCIL_OP_REPLACE;             //reference=STENCILREFMASK__.STENCILOPVAL
  STENCIL_ADD_CLAMP   :Result:=VK_STENCIL_OP_INCREMENT_AND_CLAMP; //+1? STENCILOPVAL
  STENCIL_SUB_CLAMP   :Result:=VK_STENCIL_OP_DECREMENT_AND_CLAMP; //-1? STENCILOPVAL
  STENCIL_INVERT      :Result:=VK_STENCIL_OP_INVERT;
  STENCIL_ADD_WRAP    :Result:=VK_STENCIL_OP_INCREMENT_AND_WRAP;  //+1? STENCILOPVAL
  STENCIL_SUB_WRAP    :Result:=VK_STENCIL_OP_DECREMENT_AND_WRAP;  //-1? STENCILOPVAL
  STENCIL_AND         ,                                           //    STENCILOPVAL
  STENCIL_OR          ,                                           //    STENCILOPVAL
  STENCIL_XOR         ,                                           //    STENCILOPVAL
  STENCIL_NAND        ,                                           //    STENCILOPVAL
  STENCIL_NOR         ,                                           //    STENCILOPVAL
  STENCIL_XNOR        :Assert(false,'Unknow stencil op:STENCIL_XNOR'); //    STENCILOPVAL
  else
                       Assert(false,'Unknow stencil op:0x'+HexStr(b,1));
 end;
end;

Function _GetStencilRef(b:Byte;RF:TDB_STENCILREFMASK):Integer;
begin
 Result:=-1; //no change
 case b of
  STENCIL_ONES        :Result:=1;
  STENCIL_REPLACE_TEST:Result:=RF.STENCILTESTVAL;
  STENCIL_REPLACE_OP  :Result:=RF.STENCILOPVAL;
  STENCIL_ADD_CLAMP   :Result:=RF.STENCILOPVAL;
  STENCIL_SUB_CLAMP   :Result:=RF.STENCILOPVAL;
  STENCIL_ADD_WRAP    :Result:=RF.STENCILOPVAL;
  STENCIL_SUB_WRAP    :Result:=RF.STENCILOPVAL;
  else;
 end;
end;

procedure _set_st_ref(var cur:Integer;i:Integer);
begin
 if (i<>-1) then
 begin
  Assert((cur=-1) or (cur=i),'multi param');
  cur:=i;
 end;
end;

Function GetStencilRef_FF(SC:TDB_STENCIL_CONTROL;RF:TDB_STENCILREFMASK):Byte;
var
 cur:Integer;
begin
 Result:=0;
 cur:=-1;
 _set_st_ref(cur,_GetStencilRef(SC.STENCILFAIL ,RF));
 _set_st_ref(cur,_GetStencilRef(SC.STENCILZPASS,RF));
 _set_st_ref(cur,_GetStencilRef(SC.STENCILZFAIL,RF));
 if (cur<>-1) then
 begin
  Result:=cur;
 end;
end;

Function GetStencilRef_BF(SC:TDB_STENCIL_CONTROL;RF:TDB_STENCILREFMASK):Byte;
var
 cur:Integer;
begin
 Result:=0;
 cur:=-1;
 _set_st_ref(cur,_GetStencilRef(SC.STENCILFAIL_BF ,RF));
 _set_st_ref(cur,_GetStencilRef(SC.STENCILZPASS_BF,RF));
 _set_st_ref(cur,_GetStencilRef(SC.STENCILZFAIL_BF,RF));
 if (cur<>-1) then
 begin
  Result:=cur;
 end;
end;

const
 DB_Z_FORMATS:array[0..3,0..1] of TVkFormat=
 (
  //STENCIL_INVALID             ,STENCIL_8
  (VK_FORMAT_UNDEFINED          ,VK_FORMAT_S8_UINT           ), //Z_INVALID
  (VK_FORMAT_D16_UNORM          ,VK_FORMAT_D16_UNORM_S8_UINT ), //Z_16
  (VK_FORMAT_X8_D24_UNORM_PACK32,VK_FORMAT_D24_UNORM_S8_UINT ), //Z_24
  (VK_FORMAT_D32_SFLOAT         ,VK_FORMAT_D32_SFLOAT_S8_UINT)  //Z_32_FLOAT
 );

 DB_Z_ORDER:array[0..3] of TVkPipelineStageFlags=(
  ord(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),    //LATE_Z

  ord(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or //EARLY_Z_THEN_LATE_Z
  ord(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),

  ord(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),    //RE_Z

  ord(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or //EARLY_Z_THEN_RE_Z
  ord(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT)
 );

Function TGPU_REGS.GET_DB_INFO:TDB_INFO;
var
 RENDER_CONTROL  :TDB_RENDER_CONTROL;
 DEPTH_CONTROL   :TDB_DEPTH_CONTROL;
 STENCIL_CONTROL :TDB_STENCIL_CONTROL;
 SHADER_CONTROL  :TDB_SHADER_CONTROL;
 DB_Z_INFO       :TDB_Z_INFO;
 DB_DEPTH_VIEW   :TDB_DEPTH_VIEW;
 DB_STENCIL_INFO :TDB_STENCIL_INFO;
 DB_HTILE_SURFACE:TDB_HTILE_SURFACE;
begin
 Result:=Default(TDB_INFO);

 RENDER_CONTROL  :=CX_REG^.DB_RENDER_CONTROL;
 DEPTH_CONTROL   :=CX_REG^.DB_DEPTH_CONTROL;
 STENCIL_CONTROL :=CX_REG^.DB_STENCIL_CONTROL;
 SHADER_CONTROL  :=CX_REG^.DB_SHADER_CONTROL;
 DB_Z_INFO       :=CX_REG^.DB_Z_INFO;
 DB_DEPTH_VIEW   :=CX_REG^.DB_DEPTH_VIEW;
 DB_STENCIL_INFO :=CX_REG^.DB_STENCIL_INFO;
 DB_HTILE_SURFACE:=CX_REG^.DB_HTILE_SURFACE;

 //

 if (DEPTH_CONTROL.Z_ENABLE<>0) then
 begin
  if (DB_DEPTH_VIEW.Z_READ_ONLY<>0) then
  begin
   //readonly
   Result.DEPTH_USAGE:=TM_READ;
  end else
  if (RENDER_CONTROL.DEPTH_CLEAR_ENABLE<>0) then
  begin
    //clear
   Result.DEPTH_USAGE:=TM_WRITE or TM_CLEAR;
  end else
  begin
   //read before
   Result.DEPTH_USAGE:=TM_READ or TM_WRITE;
  end;
 end;

 if (DEPTH_CONTROL.STENCIL_ENABLE<>0) then
 begin
  if (DB_DEPTH_VIEW.STENCIL_READ_ONLY<>0) then
  begin
   //readonly
   Result.STENCIL_USAGE:=TM_READ;
  end else
  if (RENDER_CONTROL.STENCIL_CLEAR_ENABLE<>0) then
  begin
    //clear
   Result.STENCIL_USAGE:=TM_WRITE or TM_CLEAR;
  end else
  begin
   //read before
   Result.STENCIL_USAGE:=TM_READ or TM_WRITE;
  end;
 end;

 Assert(RENDER_CONTROL.DEPTH_COPY   =0,'RENDER_CONTROL.DEPTH_COPY'   );
 Assert(RENDER_CONTROL.STENCIL_COPY =0,'RENDER_CONTROL.STENCIL_COPY' );
 Assert(RENDER_CONTROL.COPY_CENTROID=0,'RENDER_CONTROL.COPY_CENTROID');
 Assert(RENDER_CONTROL.COPY_SAMPLE  =0,'RENDER_CONTROL.COPY_SAMPLE'  );

 Result.CLEAR_VALUE.depthStencil.depth  :=PSingle(@CX_REG^.DB_DEPTH_CLEAR)^;
 Result.CLEAR_VALUE.depthStencil.stencil:=CX_REG^.DB_STENCIL_CLEAR.CLEAR;

 /////
 Result.ds_state.sType:=VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;

 Result.ds_state.depthTestEnable      :=DEPTH_CONTROL.Z_ENABLE;            //1:1
 Result.ds_state.depthWriteEnable     :=DEPTH_CONTROL.Z_WRITE_ENABLE;      //1:1
 Result.ds_state.depthBoundsTestEnable:=DEPTH_CONTROL.DEPTH_BOUNDS_ENABLE; //1:1
 Result.ds_state.stencilTestEnable    :=DEPTH_CONTROL.STENCIL_ENABLE;      //1:1

 if ((Result.DEPTH_USAGE and TM_CLEAR)=0) then
 begin
  Result.ds_state.depthCompareOp:=TVkCompareOp(DEPTH_CONTROL.ZFUNC); //1:1
 end else
 begin
  //force clear all
  Result.ds_state.depthCompareOp:=VK_COMPARE_OP_NEVER;
 end;

 Result.ds_state.minDepthBounds:=PSingle(@CX_REG^.DB_DEPTH_BOUNDS_MIN)^;
 Result.ds_state.maxDepthBounds:=PSingle(@CX_REG^.DB_DEPTH_BOUNDS_MAX)^;

 Assert(DEPTH_CONTROL.ENABLE_COLOR_WRITES_ON_DEPTH_FAIL =0,'ENABLE_COLOR_WRITES_ON_DEPTH_FAIL' );
 Assert(DEPTH_CONTROL.DISABLE_COLOR_WRITES_ON_DEPTH_PASS=0,'DISABLE_COLOR_WRITES_ON_DEPTH_PASS');

 //CX_REG^.PA_CL_CLIP_CNTL

 if ((Result.STENCIL_USAGE and TM_CLEAR)<>0) then
 begin
  //force clear all
  //VK_COMPARE_OP_NEVER|VK_STENCIL_OP_KEEP
 end else
 begin

  Result.ds_state.front.failOp     :=GetStencilOp(STENCIL_CONTROL.STENCILFAIL);
  Result.ds_state.front.passOp     :=GetStencilOp(STENCIL_CONTROL.STENCILZPASS);
  Result.ds_state.front.depthFailOp:=GetStencilOp(STENCIL_CONTROL.STENCILZFAIL);
  Result.ds_state.front.compareOp  :=TVkCompareOp(DEPTH_CONTROL.STENCILFUNC);   //1:1
  Result.ds_state.front.compareMask:=CX_REG^.DB_STENCILREFMASK.STENCILMASK;
  Result.ds_state.front.writeMask  :=CX_REG^.DB_STENCILREFMASK.STENCILWRITEMASK;
  Result.ds_state.front.reference  :=GetStencilRef_FF(STENCIL_CONTROL,CX_REG^.DB_STENCILREFMASK);

  if (DEPTH_CONTROL.BACKFACE_ENABLE<>0) then
  begin
   Result.ds_state.back:=Result.ds_state.front;
  end else
  begin
   Result.ds_state.back.failOp     :=GetStencilOp(STENCIL_CONTROL.STENCILFAIL_BF);
   Result.ds_state.back.passOp     :=GetStencilOp(STENCIL_CONTROL.STENCILZPASS_BF);
   Result.ds_state.back.depthFailOp:=GetStencilOp(STENCIL_CONTROL.STENCILZFAIL_BF);
   Result.ds_state.back.compareOp  :=TVkCompareOp(DEPTH_CONTROL.STENCILFUNC_BF);   //1:1
   Result.ds_state.back.compareMask:=CX_REG^.DB_STENCILREFMASK_BF.STENCILMASK_BF;
   Result.ds_state.back.writeMask  :=CX_REG^.DB_STENCILREFMASK_BF.STENCILWRITEMASK_BF;
   Result.ds_state.back.reference  :=GetStencilRef_BF(STENCIL_CONTROL,TDB_STENCILREFMASK(CX_REG^.DB_STENCILREFMASK_BF));
  end;

 end;

 ////

 Assert(DB_DEPTH_VIEW.SLICE_START=0,'DB_DEPTH_VIEW.SLICE_START');

 Result.Z_READ_ADDR :=Pointer(QWORD(CX_REG^.DB_Z_READ_BASE ) shl 8);
 Result.Z_WRITE_ADDR:=Pointer(QWORD(CX_REG^.DB_Z_WRITE_BASE) shl 8);

 Assert(Result.Z_READ_ADDR=Result.Z_WRITE_ADDR,'Z_READ_ADDR<>Z_WRITE_ADDR');

 Result.STENCIL_READ_ADDR :=Pointer(QWORD(CX_REG^.DB_STENCIL_READ_BASE ) shl 8);
 Result.STENCIL_WRITE_ADDR:=Pointer(QWORD(CX_REG^.DB_STENCIL_WRITE_BASE) shl 8);

 Assert(Result.STENCIL_READ_ADDR=Result.STENCIL_WRITE_ADDR,'STENCIL_READ_ADDR<>STENCIL_WRITE_ADDR');

 Assert(SHADER_CONTROL.Z_EXPORT_ENABLE=0               ,'Z_EXPORT_ENABLE');
 Assert(SHADER_CONTROL.STENCIL_TEST_VAL_EXPORT_ENABLE=0,'STENCIL_TEST_VAL_EXPORT_ENABLE');

 //VK_EXT_depth_range_unrestricted
 Assert(CX_REG^.DB_RENDER_OVERRIDE.DISABLE_VIEWPORT_CLAMP=0,'DISABLE_VIEWPORT_CLAMP');

 //SHADER_CONTROL.CONSERVATIVE_Z_EXPORT -> SPIRV DepthGreater/DepthLess
 //CX_REG^.PA_SU_VTX_CNTL.PIX_CENTER    -> SPIRV PixelCenterInteger
 //SHADER_CONTROL.DEPTH_BEFORE_SHADER   -> SPIRV EarlyFragmentTests

 //CX_REG^.CB_COLOR_CONTROL
 //CX_REG^.DB_RENDER_OVERRIDE.FORCE_SHADER_Z_ORDER

 Result.zorder_stage:=DB_Z_ORDER[SHADER_CONTROL.Z_ORDER];

 if (SHADER_CONTROL.DEPTH_BEFORE_SHADER<>0) then
 begin
  Result.zorder_stage:=Result.zorder_stage or ord(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT);
 end;

 //

 if (DB_STENCIL_INFO.FORMAT<>0) then
 begin
  Assert(DB_Z_INFO.TILE_MODE_INDEX=DB_STENCIL_INFO.TILE_MODE_INDEX,'DB_Z_INFO.TILE_MODE_INDEX<>DB_STENCIL_INFO.TILE_MODE_INDEX');
 end;

 //

 Result.FImageInfo.cformat:=DB_Z_FORMATS[DB_Z_INFO.FORMAT,DB_STENCIL_INFO.FORMAT];

 Result.FImageInfo.Addr   :=Result.Z_READ_ADDR;
 Result.FImageInfo.Addr2  :=Result.STENCIL_READ_ADDR;

 Result.FImageInfo.params.width :=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_BR.BR_X);
 Result.FImageInfo.params.height:=_fix_scissor_range(CX_REG^.PA_SC_SCREEN_SCISSOR_BR.BR_Y);
 Result.FImageInfo.params.depth :=1;

 Result.FImageInfo.params.tiling.idx:=DB_Z_INFO.TILE_MODE_INDEX;

 Result.FImageInfo.params.itype      :=ord(VK_IMAGE_TYPE_2D);
 Result.FImageInfo.params.samples    :=1 shl (DB_Z_INFO.NUM_SAMPLES and 3);
 Result.FImageInfo.params.mipLevels  :=1;
 Result.FImageInfo.params.arrayLayers:=1;

 Result.FImageInfo.params.pad_width :=(CX_REG^.DB_DEPTH_SIZE.PITCH_TILE_MAX +1)*8;
 Result.FImageInfo.params.pad_height:=(CX_REG^.DB_DEPTH_SIZE.HEIGHT_TILE_MAX+1)*8;

 if (DB_Z_INFO.TILE_SURFACE_ENABLE<>0) and
    (CX_REG^.DB_HTILE_DATA_BASE<>0) then
 begin
  Result.HTILE_INFO.KEY.ADDR:=Pointer(QWORD(CX_REG^.DB_HTILE_DATA_BASE) shl 8);

  Result.HTILE_INFO.LINEAR:=DB_HTILE_SURFACE.LINEAR;

  if (p_neomode<>0) then
  begin
   Result.HTILE_INFO.TC_COMPATIBLE:=DB_HTILE_SURFACE.TC_COMPATIBLE;
  end;

  Result.HTILE_INFO.TILE_SURFACE_ENABLE:=1;
  Result.HTILE_INFO.TILE_STENCIL_ENABLE:=ord(DB_STENCIL_INFO.TILE_STENCIL_DISABLE=0);

  if (Result.HTILE_INFO.TILE_STENCIL_ENABLE<>0) then
  begin
   Result.HTILE_INFO.KEY.Addr2:=Result.HTILE_INFO.KEY.ADDR;
  end;

  computeHtileInfo(@Result.HTILE_INFO.SIZE,
                   nil,
                   @Result.HTILE_INFO.KEY.params.pad_width,
                   @Result.HTILE_INFO.KEY.params.pad_height,
                   //
                   Result.FImageInfo.params.pad_width,
                   Result.FImageInfo.params.pad_height,
                   DB_DEPTH_VIEW.SLICE_MAX,
                   //
                   Boolean(Result.HTILE_INFO.LINEAR),
                   Boolean(Result.HTILE_INFO.TC_COMPATIBLE),
                   DB_Z_INFO.TILE_MODE_INDEX
                  );

  Result.HTILE_INFO.KEY.params.pad_width:=Result.HTILE_INFO.KEY.params.pad_width div 16;

  Result.HTILE_INFO.KEY.cformat:=VK_FORMAT_R32_UINT;
  Result.HTILE_INFO.KEY.params.itype      :=ord(VK_IMAGE_TYPE_2D);

  if (Result.HTILE_INFO.LINEAR<>0) then
  begin
   Result.HTILE_INFO.KEY.params.tiling.idx:=kTileModeDisplay_LinearAligned;
   Result.HTILE_INFO.KEY.params.tiling.alt:=Result.HTILE_INFO.TC_COMPATIBLE;
  end else
  begin
   Result.HTILE_INFO.KEY.params.tiling.idx:=DB_Z_INFO.TILE_MODE_INDEX;
   Result.HTILE_INFO.KEY.params.tiling.alt:=Result.HTILE_INFO.TC_COMPATIBLE;
  end;

  Result.HTILE_INFO.KEY.params.samples    :=1;
  Result.HTILE_INFO.KEY.params.mipLevels  :=1;
  Result.HTILE_INFO.KEY.params.width      :=Result.HTILE_INFO.KEY.params.pad_width;
  Result.HTILE_INFO.KEY.params.height     :=Result.HTILE_INFO.KEY.params.pad_height;
  Result.HTILE_INFO.KEY.params.depth      :=1;
  Result.HTILE_INFO.KEY.params.arrayLayers:=1;

 end;

end;

function get_polygon_mode(SU_SC_MODE_CNTL:TPA_SU_SC_MODE_CNTL):TVkPolygonMode;
var
 t:Byte;
begin
 if (SU_SC_MODE_CNTL.POLY_MODE=0) then
 begin
  Exit(VK_POLYGON_MODE_FILL);
 end;

 //fillModeNonSolid

 if (SU_SC_MODE_CNTL.CULL_FRONT=0) then
 begin
  t:=SU_SC_MODE_CNTL.POLYMODE_FRONT_PTYPE;
 end else
 if (SU_SC_MODE_CNTL.CULL_BACK=0) then
 begin
  t:=SU_SC_MODE_CNTL.POLYMODE_BACK_PTYPE;
 end else
 begin
  t:=2;
 end;

 case t of
  0:Result:=VK_POLYGON_MODE_POINT;
  1:Result:=VK_POLYGON_MODE_LINE;
  2:Result:=VK_POLYGON_MODE_FILL;
  else
    Result:=VK_POLYGON_MODE_FILL;
 end;
end;

function get_cull_mode(SU_SC_MODE_CNTL:TPA_SU_SC_MODE_CNTL):TVkCullModeFlags;
begin
 Result:=TVkCullModeFlags(SU_SC_MODE_CNTL.CULL_FRONT or (SU_SC_MODE_CNTL.CULL_BACK shl 1));
end;

Function TGPU_REGS.GET_RASTERIZATION:TVkPipelineRasterizationStateCreateInfo;
var
 SU_SC_MODE_CNTL:TPA_SU_SC_MODE_CNTL;
 PA_CL_CLIP_CNTL:TPA_CL_CLIP_CNTL;
begin
 SU_SC_MODE_CNTL:=CX_REG^.PA_SU_SC_MODE_CNTL;
 PA_CL_CLIP_CNTL:=CX_REG^.PA_CL_CLIP_CNTL;

 Result:=Default(TVkPipelineRasterizationStateCreateInfo);
 Result.sType:=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;

 if (SG_REG^.SPI_SHADER_PGM_LO_PS<>0) or
    (SG_REG^.SPI_SHADER_PGM_HI_PS.MEM_BASE<>0) then
 if (CX_REG^.DB_RENDER_CONTROL.DEPTH_CLEAR_ENABLE=0) and
    (CX_REG^.DB_RENDER_CONTROL.STENCIL_CLEAR_ENABLE=0) then
 begin
  Result.rasterizerDiscardEnable:=PA_CL_CLIP_CNTL.DX_RASTERIZATION_KILL;
 end;

 //VkPhysicalDeviceDepthClampZeroOneFeaturesEXT::depthClampZeroOne
 Result.depthClampEnable       :=PA_CL_CLIP_CNTL.ZCLIP_NEAR_DISABLE or PA_CL_CLIP_CNTL.ZCLIP_FAR_DISABLE;
 Result.polygonMode            :=get_polygon_mode(SU_SC_MODE_CNTL);
 Result.cullMode               :=get_cull_mode   (SU_SC_MODE_CNTL);
 Result.frontFace              :=TVkFrontFace    (SU_SC_MODE_CNTL.FACE); //1:1
 Result.lineWidth              :=(CX_REG^.PA_SU_LINE_CNTL.WIDTH/8);

 if (DWORD(CX_REG^.PA_SU_POLY_OFFSET_DB_FMT_CNTL)<>0) then
 begin
  Result.depthBiasClamp:=PSingle(@CX_REG^.PA_SU_POLY_OFFSET_CLAMP)^;

  if (SU_SC_MODE_CNTL.CULL_FRONT=0) then
  begin
   Result.depthBiasEnable        :=SU_SC_MODE_CNTL.POLY_OFFSET_FRONT_ENABLE;
   Result.depthBiasConstantFactor:=PSingle(@CX_REG^.PA_SU_POLY_OFFSET_FRONT_OFFSET)^;
   Result.depthBiasSlopeFactor   :=(PSingle(@CX_REG^.PA_SU_POLY_OFFSET_FRONT_SCALE)^/16);
  end else
  if (SU_SC_MODE_CNTL.CULL_BACK=0) then
  begin
   Result.depthBiasEnable        :=SU_SC_MODE_CNTL.POLY_OFFSET_BACK_ENABLE;
   Result.depthBiasConstantFactor:=PSingle(@CX_REG^.PA_SU_POLY_OFFSET_BACK_OFFSET)^;
   Result.depthBiasSlopeFactor   :=(PSingle(@CX_REG^.PA_SU_POLY_OFFSET_BACK_SCALE)^/16);
  end;

 end;

end;

Function TGPU_REGS.GET_PROVOKING:TVkProvokingVertexModeEXT;
begin
 Result:=TVkProvokingVertexModeEXT(CX_REG^.PA_SU_SC_MODE_CNTL.PROVOKING_VTX_LAST);
end;

Function TGPU_REGS.GET_MULTISAMPLE:TVkPipelineMultisampleStateCreateInfo;
var
 ps_iter_samples,num_samples:Integer;
begin
 Result:=Default(TVkPipelineMultisampleStateCreateInfo);
 Result.sType:=VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;

 if (CX_REG^.DB_EQAA.PS_ITER_SAMPLES<>0) or
    (CX_REG^.PA_SC_AA_CONFIG.MSAA_NUM_SAMPLES<>0) then
 begin
  ps_iter_samples:=1 shl CX_REG^.DB_EQAA.PS_ITER_SAMPLES;
  num_samples    :=1 shl CX_REG^.PA_SC_AA_CONFIG.MSAA_NUM_SAMPLES;

  Result.sampleShadingEnable  :=VK_TRUE;
  Result.rasterizationSamples :=TVkSampleCountFlagBits(num_samples);
  Result.minSampleShading     :=ps_iter_samples/num_samples;
  Result.pSampleMask          :=nil; //TODO
  Result.alphaToCoverageEnable:=CX_REG^.DB_ALPHA_TO_MASK.ALPHA_TO_MASK_ENABLE;
  Result.alphaToOneEnable     :=VK_FALSE;
 end else
 begin
  Result.rasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
 end;
end;

function TGPU_REGS.GET_PRIM_RESET:TVkBool32;
begin
 Result:=CX_REG^.VGT_MULTI_PRIM_IB_RESET_EN.RESET_EN;

 if (Result<>0) then
 begin
  Case UC_REG^.VGT_INDEX_TYPE.INDEX_TYPE of
   VGT_INDEX_16:Assert(CX_REG^.VGT_MULTI_PRIM_IB_RESET_INDX=$0000FFFF,'unsupport reset index:0x'+HexStr(CX_REG^.VGT_MULTI_PRIM_IB_RESET_INDX,8));
   VGT_INDEX_32:Assert(CX_REG^.VGT_MULTI_PRIM_IB_RESET_INDX=$FFFFFFFF,'unsupport reset index:0x'+HexStr(CX_REG^.VGT_MULTI_PRIM_IB_RESET_INDX,8));
   VGT_INDEX_8 :Assert(CX_REG^.VGT_MULTI_PRIM_IB_RESET_INDX=$000000FF,'unsupport reset index:0x'+HexStr(CX_REG^.VGT_MULTI_PRIM_IB_RESET_INDX,8));
   else;
  end;

 end;
end;

function TGPU_REGS.GET_PRIM_TYPE:TVkPrimitiveTopology;
begin
 case UC_REG^.VGT_PRIMITIVE_TYPE.PRIM_TYPE of
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
  DI_PT_POLYGON      :Result:=TVkPrimitiveTopology(UC_REG^.VGT_PRIMITIVE_TYPE.PRIM_TYPE); //need to emulate

  else
   Assert(False,'unknow prim type:0x'+HexStr(UC_REG^.VGT_PRIMITIVE_TYPE.PRIM_TYPE,1));
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
 if (CX_REG^.VGT_DMA_INDEX_TYPE.SWAP_MODE<>0) then
 begin
  Assert(false,'swapMode:'+IntToStr(CX_REG^.VGT_DMA_INDEX_TYPE.SWAP_MODE));
 end;

 Case UC_REG^.VGT_INDEX_TYPE.INDEX_TYPE of
  VGT_INDEX_16:Result:=VK_INDEX_TYPE_UINT16;
  VGT_INDEX_32:Result:=VK_INDEX_TYPE_UINT32;
  VGT_INDEX_8 :Result:=VK_INDEX_TYPE_UINT8_EXT;
  else         Result:=VK_INDEX_TYPE_NONE_KHR;
 end;
end;

function GET_INDEX_TYPE_SIZE(i:TVkIndexType):Byte;
begin
 Case i of
  VK_INDEX_TYPE_UINT16   :Result:=16;
  VK_INDEX_TYPE_UINT32   :Result:=32;
  VK_INDEX_TYPE_UINT8_EXT:Result:=8;
  else                    Result:=0;
 end;
end;

function TGPU_REGS.get_reg(i:word):DWORD;
begin
 case i of
  $2C00..$2D8C:Result:=PDWORD(SG_REG)[i-$2C00];
  $2E00..$2E7F:Result:=PDWORD(SC_REG)[i-$2E00];
  $A000..$A38F:Result:=PDWORD(CX_REG)[i-$A000];
  $C079:Result:=PDWORD(@UC_REG^.CP_COHER_BASE_HI  )^;
  $C07C:Result:=PDWORD(@UC_REG^.CP_COHER_CNTL     )^;
  $C07D:Result:=PDWORD(@UC_REG^.CP_COHER_SIZE     )^;
  $C07E:Result:=PDWORD(@UC_REG^.CP_COHER_BASE     )^;
  $C08C:Result:=PDWORD(@UC_REG^.CP_COHER_SIZE_HI  )^;
  $C200:Result:=PDWORD(@UC_REG^.GRBM_GFX_INDEX    )^;
  $C240:Result:=PDWORD(@UC_REG^.VGT_ESGS_RING_SIZE)^;
  $C241:Result:=PDWORD(@UC_REG^.VGT_GSVS_RING_SIZE)^;
  $C242:Result:=PDWORD(@UC_REG^.VGT_PRIMITIVE_TYPE)^;
  $C243:Result:=PDWORD(@UC_REG^.VGT_INDEX_TYPE    )^;
  $C24C:Result:=PDWORD(@UC_REG^.VGT_NUM_INDICES   )^;
  $C24D:Result:=PDWORD(@UC_REG^.VGT_NUM_INSTANCES )^;
  else
        Result:=0;
 end;
end;

Function TGPU_REGS.get_code_addr(FStage:TvShaderStage):Pointer;
begin
 Result:=nil;
 case FStage of
  vShaderStageLs:Result:=getCodeAddress(SG_REG^.SPI_SHADER_PGM_LO_LS,SG_REG^.SPI_SHADER_PGM_HI_LS.MEM_BASE);
  vShaderStageHs:Result:=getCodeAddress(SG_REG^.SPI_SHADER_PGM_LO_HS,SG_REG^.SPI_SHADER_PGM_HI_HS.MEM_BASE);
  vShaderStageEs:Result:=getCodeAddress(SG_REG^.SPI_SHADER_PGM_LO_ES,SG_REG^.SPI_SHADER_PGM_HI_ES.MEM_BASE);
  vShaderStageGs:Result:=getCodeAddress(SG_REG^.SPI_SHADER_PGM_LO_GS,SG_REG^.SPI_SHADER_PGM_HI_GS.MEM_BASE);
  vShaderStageVs:Result:=getCodeAddress(SG_REG^.SPI_SHADER_PGM_LO_VS,SG_REG^.SPI_SHADER_PGM_HI_VS.MEM_BASE);
  vShaderStagePs:Result:=getCodeAddress(SG_REG^.SPI_SHADER_PGM_LO_PS,SG_REG^.SPI_SHADER_PGM_HI_PS.MEM_BASE);
  vShaderStageCs:Result:=getCodeAddress(SC_REG^.COMPUTE_PGM_LO      ,SC_REG^.COMPUTE_PGM_HI.DATA);
 end;
end;

Function TGPU_REGS.get_user_data(FStage:TvShaderStage):Pointer;
begin
 Result:=nil;
 case FStage of
  vShaderStageLs:Result:=@SG_REG^.SPI_SHADER_USER_DATA_LS;
  vShaderStageHs:Result:=@SG_REG^.SPI_SHADER_USER_DATA_HS;
  vShaderStageEs:Result:=@SG_REG^.SPI_SHADER_USER_DATA_ES;
  vShaderStageGs:Result:=@SG_REG^.SPI_SHADER_USER_DATA_GS;
  vShaderStageVs:Result:=@SG_REG^.SPI_SHADER_USER_DATA_VS;
  vShaderStagePs:Result:=@SG_REG^.SPI_SHADER_USER_DATA_PS;
  vShaderStageCs:Result:=@SC_REG^.COMPUTE_USER_DATA;
 end;
end;

procedure TGPU_REGS.export_user_data_rt(dst:PGPU_USERDATA);
begin
 dst^.A[vShaderStageLs]:=SG_REG^.SPI_SHADER_USER_DATA_LS;
 dst^.A[vShaderStageHs]:=SG_REG^.SPI_SHADER_USER_DATA_HS;
 dst^.A[vShaderStageEs]:=SG_REG^.SPI_SHADER_USER_DATA_ES;
 dst^.A[vShaderStageGs]:=SG_REG^.SPI_SHADER_USER_DATA_GS;
 dst^.A[vShaderStageVs]:=SG_REG^.SPI_SHADER_USER_DATA_VS;
 dst^.A[vShaderStagePs]:=SG_REG^.SPI_SHADER_USER_DATA_PS;
end;

procedure TGPU_REGS.export_user_data_cs(dst:PGPU_USERDATA);
begin
 dst^.A[vShaderStageCs]:=SC_REG^.COMPUTE_USER_DATA;
end;

Function TGPU_USERDATA.get_user_data(FStage:TvShaderStage):Pointer;
begin
 Result:=@A[FStage];
end;

///

function _get_vsharp_cformat(PV:PVSharpResource4):TVkFormat;
begin
 Result:=VK_FORMAT_UNDEFINED;
 if (PV=nil) then Exit;

 Case PV^.nfmt of

  BUF_NUM_FORMAT_UNORM:
   case PV^.dfmt of
    BUF_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_UNORM;
    BUF_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_UNORM;
    BUF_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_UNORM;
    BUF_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_UNORM;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_UNORM;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_UNORM;
    BUF_DATA_FORMAT_2_10_10_10 :Result:=VK_FORMAT_A2R10G10B10_UNORM_PACK32;
    else;
   end;

  BUF_NUM_FORMAT_SNORM:
   case PV^.dfmt of
    BUF_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SNORM;
    BUF_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SNORM;
    BUF_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SNORM;
    BUF_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_SNORM;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_SNORM;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SNORM;
    else;
   end;

  BUF_NUM_FORMAT_USCALED:
   case PV^.dfmt of
    BUF_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_USCALED;
    BUF_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_USCALED;
    BUF_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_USCALED;
    BUF_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_USCALED;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_USCALED;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_USCALED;
    else;
   end;

  BUF_NUM_FORMAT_SSCALED:
   case PV^.dfmt of
    BUF_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SSCALED;
    BUF_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SSCALED;
    BUF_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SSCALED;
    BUF_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_SSCALED;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_SSCALED;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SSCALED;
    else;
   end;

  BUF_NUM_FORMAT_UINT:
   case PV^.dfmt of
    BUF_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_UINT;
    BUF_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_UINT;
    BUF_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_UINT;
    BUF_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_UINT;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_UINT;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_UINT;
    BUF_DATA_FORMAT_32         :Result:=VK_FORMAT_R32_UINT;
    BUF_DATA_FORMAT_32_32      :Result:=VK_FORMAT_R32G32_UINT;
    BUF_DATA_FORMAT_32_32_32   :Result:=VK_FORMAT_R32G32B32_UINT;
    BUF_DATA_FORMAT_32_32_32_32:Result:=VK_FORMAT_R32G32B32A32_UINT;
    else;
   end;

  BUF_NUM_FORMAT_SINT:
   case PV^.dfmt of
    BUF_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SINT;
    BUF_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SINT;
    BUF_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SINT;
    BUF_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_SINT;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_SINT;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SINT;
    BUF_DATA_FORMAT_32         :Result:=VK_FORMAT_R32_SINT;
    BUF_DATA_FORMAT_32_32      :Result:=VK_FORMAT_R32G32_SINT;
    BUF_DATA_FORMAT_32_32_32   :Result:=VK_FORMAT_R32G32B32_SINT;
    BUF_DATA_FORMAT_32_32_32_32:Result:=VK_FORMAT_R32G32B32A32_SINT;
    else;
   end;

  BUF_NUM_FORMAT_FLOAT:
   case PV^.dfmt of
    BUF_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_SFLOAT;
    BUF_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_SFLOAT;
    BUF_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SFLOAT;
    BUF_DATA_FORMAT_32         :Result:=VK_FORMAT_R32_SFLOAT;
    BUF_DATA_FORMAT_32_32      :Result:=VK_FORMAT_R32G32_SFLOAT;
    BUF_DATA_FORMAT_32_32_32   :Result:=VK_FORMAT_R32G32B32_SFLOAT;
    BUF_DATA_FORMAT_32_32_32_32:Result:=VK_FORMAT_R32G32B32A32_SFLOAT;
    else;
   end;

  else;
 end;

 Assert(Result<>VK_FORMAT_UNDEFINED,'[_get_vsharp_cformat] dfmt:'+_get_buf_dfmt_str(PV^.dfmt)+' nfmt:'+_get_buf_nfmt_str(PV^.nfmt));
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
 Result:=VK_FORMAT_UNDEFINED;
 if (PT=nil) then Exit;

 Case PT^.nfmt of
  IMG_NUM_FORMAT_UNORM  :
    case PT^.dfmt of
     IMG_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_UNORM;
     IMG_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_UNORM;
     IMG_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_UNORM;
     IMG_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_UNORM;
     IMG_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_UNORM;
     IMG_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_UNORM;
     IMG_DATA_FORMAT_5_6_5      :Result:=VK_FORMAT_R5G6B5_UNORM_PACK16;
     IMG_DATA_FORMAT_4_4_4_4    :Result:=VK_FORMAT_R4G4B4A4_UNORM_PACK16;
     IMG_DATA_FORMAT_BC1        :Result:=VK_FORMAT_BC1_RGBA_UNORM_BLOCK;
     IMG_DATA_FORMAT_BC2        :Result:=VK_FORMAT_BC2_UNORM_BLOCK;
     IMG_DATA_FORMAT_BC3        :Result:=VK_FORMAT_BC3_UNORM_BLOCK;
     IMG_DATA_FORMAT_BC4        :Result:=VK_FORMAT_BC4_UNORM_BLOCK;
     IMG_DATA_FORMAT_BC5        :Result:=VK_FORMAT_BC5_UNORM_BLOCK;
     IMG_DATA_FORMAT_BC6        :Result:=VK_FORMAT_BC6H_UFLOAT_BLOCK;
     IMG_DATA_FORMAT_BC7        :Result:=VK_FORMAT_BC7_UNORM_BLOCK;

     IMG_DATA_FORMAT_2_10_10_10 :Result:=VK_FORMAT_A2R10G10B10_UNORM_PACK32;
     else;
    end;

  IMG_NUM_FORMAT_SRGB  :
    case PT^.dfmt of
     IMG_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SRGB;
     IMG_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SRGB;
     IMG_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SRGB;
     IMG_DATA_FORMAT_BC1        :Result:=VK_FORMAT_BC1_RGBA_SRGB_BLOCK;
     IMG_DATA_FORMAT_BC2        :Result:=VK_FORMAT_BC2_SRGB_BLOCK;
     IMG_DATA_FORMAT_BC3        :Result:=VK_FORMAT_BC3_SRGB_BLOCK;
     IMG_DATA_FORMAT_BC4        :Result:=VK_FORMAT_BC4_UNORM_BLOCK;
     IMG_DATA_FORMAT_BC5        :Result:=VK_FORMAT_BC5_UNORM_BLOCK;
     IMG_DATA_FORMAT_BC6        :Result:=VK_FORMAT_BC6H_UFLOAT_BLOCK;
     IMG_DATA_FORMAT_BC7        :Result:=VK_FORMAT_BC7_SRGB_BLOCK;

     IMG_DATA_FORMAT_2_10_10_10 :Result:=VK_FORMAT_A2R10G10B10_UNORM_PACK32;
     else;
    end;

  IMG_NUM_FORMAT_SNORM  :
    case PT^.dfmt of
     IMG_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SNORM;
     IMG_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SNORM;
     IMG_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SNORM;
     IMG_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_SNORM;
     IMG_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_SNORM;
     IMG_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SNORM;
     else;
    end;

  IMG_NUM_FORMAT_USCALED:
    case PT^.dfmt of
     IMG_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_USCALED;
     IMG_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_USCALED;
     IMG_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_USCALED;
     IMG_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_USCALED;
     IMG_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_USCALED;
     IMG_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_USCALED;
     else;
    end;


  IMG_NUM_FORMAT_SSCALED:
    case PT^.dfmt of
     IMG_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SSCALED;
     IMG_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SSCALED;
     IMG_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SSCALED;
     IMG_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_SSCALED;
     IMG_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_SSCALED;
     IMG_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SSCALED;
     else;
    end;

  IMG_NUM_FORMAT_UINT   :
   case PT^.dfmt of
    IMG_DATA_FORMAT_8           :Result:=VK_FORMAT_R8_UINT;
    IMG_DATA_FORMAT_8_8         :Result:=VK_FORMAT_R8G8_UINT;
    IMG_DATA_FORMAT_8_8_8_8     :Result:=VK_FORMAT_R8G8B8A8_UINT;
    IMG_DATA_FORMAT_16          :Result:=VK_FORMAT_R16_UINT;
    IMG_DATA_FORMAT_16_16       :Result:=VK_FORMAT_R16G16_UINT;
    IMG_DATA_FORMAT_16_16_16_16 :Result:=VK_FORMAT_R16G16B16A16_UINT;

    IMG_DATA_FORMAT_32          :
     if IsTileModeDepth(PT^.tiling_idx) then
     begin
      Result:=VK_FORMAT_D32_SFLOAT;
     end else
     begin
      Result:=VK_FORMAT_R32_UINT;
     end;

    IMG_DATA_FORMAT_32_32       :Result:=VK_FORMAT_R32G32_UINT;
    IMG_DATA_FORMAT_32_32_32    :Result:=VK_FORMAT_R32G32B32_UINT;
    IMG_DATA_FORMAT_32_32_32_32 :Result:=VK_FORMAT_R32G32B32A32_UINT;
    IMG_DATA_FORMAT_FMASK8_S2_F1:Result:=VK_FORMAT_R8_UINT;
    IMG_DATA_FORMAT_FMASK8_S4_F1:Result:=VK_FORMAT_R8_UINT;
    IMG_DATA_FORMAT_FMASK8_S8_F1:Result:=VK_FORMAT_R8_UINT;
    IMG_DATA_FORMAT_FMASK8_S2_F2:Result:=VK_FORMAT_R8_UINT;
    IMG_DATA_FORMAT_FMASK8_S4_F2:Result:=VK_FORMAT_R8_UINT;
    IMG_DATA_FORMAT_FMASK8_S4_F4:Result:=VK_FORMAT_R8_UINT;
    else;
   end;

  IMG_NUM_FORMAT_SINT   :
   case PT^.dfmt of
    IMG_DATA_FORMAT_8          :Result:=VK_FORMAT_R8_SINT;
    IMG_DATA_FORMAT_8_8        :Result:=VK_FORMAT_R8G8_SINT;
    IMG_DATA_FORMAT_8_8_8_8    :Result:=VK_FORMAT_R8G8B8A8_SINT;
    IMG_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_SINT;
    IMG_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_SINT;
    IMG_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SINT;

    IMG_DATA_FORMAT_32         :
     if IsTileModeDepth(PT^.tiling_idx) then
     begin
      Result:=VK_FORMAT_D32_SFLOAT;
     end else
     begin
      Result:=VK_FORMAT_R32_SINT;
     end;

    IMG_DATA_FORMAT_32_32      :Result:=VK_FORMAT_R32G32_SINT;
    IMG_DATA_FORMAT_32_32_32   :Result:=VK_FORMAT_R32G32B32_SINT;
    IMG_DATA_FORMAT_32_32_32_32:Result:=VK_FORMAT_R32G32B32A32_SINT;
    else;
   end;

  IMG_NUM_FORMAT_FLOAT  :
   case PT^.dfmt of
    IMG_DATA_FORMAT_16         :Result:=VK_FORMAT_R16_SFLOAT;
    IMG_DATA_FORMAT_16_16      :Result:=VK_FORMAT_R16G16_SFLOAT;
    IMG_DATA_FORMAT_16_16_16_16:Result:=VK_FORMAT_R16G16B16A16_SFLOAT;

    IMG_DATA_FORMAT_32         :
     if IsTileModeDepth(PT^.tiling_idx) then
     begin
      Result:=VK_FORMAT_D32_SFLOAT;
     end else
     begin
      Result:=VK_FORMAT_R32_SFLOAT;
     end;

    IMG_DATA_FORMAT_32_32      :Result:=VK_FORMAT_R32G32_SFLOAT;
    IMG_DATA_FORMAT_32_32_32   :Result:=VK_FORMAT_R32G32B32_SFLOAT;
    IMG_DATA_FORMAT_32_32_32_32:Result:=VK_FORMAT_R32G32B32A32_SFLOAT;

    IMG_DATA_FORMAT_5_9_9_9    :Result:=VK_FORMAT_E5B9G9R9_UFLOAT_PACK32;
    IMG_DATA_FORMAT_10_11_11   :Result:=VK_FORMAT_B10G11R11_UFLOAT_PACK32;
    IMG_DATA_FORMAT_11_11_10   :Result:=VK_FORMAT_R10G11B11_UFLOAT_FAKE32;
    else;
   end;

  else;
 end;

 Assert(Result<>VK_FORMAT_UNDEFINED,'[_get_tsharp4_cformat] dfmt:'+_get_tex_dfmt_str(PT^.dfmt)+' nfmt:'+_get_tex_nfmt_str(PT^.nfmt));
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
  SQ_RSRC_IMG_CUBE         :
   begin
    Result.params.itype:=ord(VK_IMAGE_TYPE_2D);
    Result.params.cube :=1;
   end;
  SQ_RSRC_IMG_1D_ARRAY     :Result.params.itype:=ord(VK_IMAGE_TYPE_1D);
  SQ_RSRC_IMG_2D_ARRAY     :Result.params.itype:=ord(VK_IMAGE_TYPE_2D);
  SQ_RSRC_IMG_2D_MSAA      :Result.params.itype:=ord(VK_IMAGE_TYPE_2D);
  SQ_RSRC_IMG_2D_MSAA_ARRAY:Result.params.itype:=ord(VK_IMAGE_TYPE_2D);
  else;
   Assert(false,'Unknow tsharp4 type:0x'+HexStr(PT^._type,1));
 end;

 Result.params.tiling.idx:=PT^.tiling_idx;
 Result.params.tiling.alt:=0;
 Result.params.width     :=PT^.width +1;
 Result.params.height    :=PT^.height+1;
 Result.params.depth     :=1;

 if _img_is_msaa(PT^._type) then
 begin
  Result.params.samples  :=PT^.last_level+1;
  Result.params.mipLevels:=1;
 end else
 begin
  Result.params.samples  :=1;
  Result.params.mipLevels:=PT^.last_level+1;
 end;

 //Assert(Result.params.mipLevels=1,'TODO');
 //Result.params.mipLevels:=1; /////

 Result.params.arrayLayers:=1;

 //TODO: Calculate padding by tilling mode
 Result.params.pad_width :=Result.params.width;
 Result.params.pad_height:=Result.params.height;
end;

function _get_tsharp8_image_info(PT:PTSharpResource8):TvImageKey;
begin
 Result:=_get_tsharp4_image_info(PTSharpResource4(PT));
 //
 if (p_neomode<>0) then
 begin
  Result.params.tiling.alt:=PT^.alt_tile_mode;
 end;
 //
 Case PT^._type of
  SQ_RSRC_IMG_3D:
   begin
    Result.params.depth:=PT^.depth+1;
   end;
  else;
 end;
 //
 Case PT^._type of
  SQ_RSRC_IMG_CUBE,
  SQ_RSRC_IMG_1D_ARRAY,
  SQ_RSRC_IMG_2D_ARRAY,
  SQ_RSRC_IMG_2D_MSAA_ARRAY:
   begin
    Result.params.arrayLayers:=PT^.last_array+1;
   end
  else;
 end;
end;

function _get_dst_sel_swizzle(b:Byte):Byte;
begin
 Case b of
  0:Result:=VK_SWIZZLE_Z;
  1:Result:=VK_SWIZZLE_O;
  4:Result:=VK_SWIZZLE_R;
  5:Result:=VK_SWIZZLE_G;
  6:Result:=VK_SWIZZLE_B;
  7:Result:=VK_SWIZZLE_A;
  else
    Result:=VK_SWIZZLE_I;
 end;
end;

//perf_mod:bit3;  //0=0/16, 1=2/16, 2=5/16, 3=7/16, 4=9/16, 5=11/16, 6=14/16, 7=16/16
//interlaced:bit1;  //texture is interlaced
//tiling_idx:bit5;  //index into lookup table of surface tiling settings
//pow2pad:bit1;  //memory footprint is padded to power of 2 dimensions

function _get_lod(w:Word):TVkFloat; forward;

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
   Assert(false,'Unknow tsharp4 type:0x'+HexStr(PT^._type,1));
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

 Result.minLod:=_get_lod(PT^.min_lod);

 //Result.base_level:=0; /////
 //Result.last_level:=0; /////
end;

function _get_tsharp8_image_view(PT:PTSharpResource8):TvImageViewKey;
begin
 Result:=_get_tsharp4_image_view(PTSharpResource4(PT));
 //
 Case PT^._type of
  SQ_RSRC_IMG_CUBE,
  SQ_RSRC_IMG_1D_ARRAY     ,
  SQ_RSRC_IMG_2D_ARRAY     ,
  SQ_RSRC_IMG_2D_MSAA_ARRAY:
   begin
    Result.base_array:=PT^.base_array;
    Result.last_array:=PT^.last_array;
   end;
  else;
 end;
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
 with Tlod_bias_bits(bias) do
 begin
  b:=(-1*sign)+int+(frac/256);
 end;

 with Tlod_bias_sec_bits(sec) do
 begin
  s:=(-1*sign)+int+(frac/16);
 end;

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
 with Tlod_bits(w) do
 begin
  Result:=int+(frac/256);
 end;
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

