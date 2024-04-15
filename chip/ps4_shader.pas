unit ps4_shader;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  bittype,
  si_ci_vi_merged_enum,
  ps4_pssl;

const
 SHADER_BINARY_INFO_SIG:array[0..6] of Char='OrbShdr';
 SHADER_END_SEARCH_MAX_DW=256+7;

type
 PShaderBinaryInfo=^TShaderBinaryInfo;
 TShaderBinaryInfo=bitpacked record
  signature:array[0..6] of AnsiChar;  // 'OrbShdr'
  version:Byte;                       // ShaderBinaryInfoVersion

  pssl_or_cg   :bit1;                 // 1 = PSSL / Cg, 0 = IL / shtb
  cached       :bit1;                 // 1 = when compile, debugging source was cached.  May only make sense for PSSL=1
  m_type       :bit4;                 // See enum ShaderBinaryType
  source_type  :bit2;                 // See enum ShaderSourceType
  length       :bit24;                // Binary code length (does not include this structure or any of its preceding associated tables)

  chunkUsageBaseOffsetInDW:Byte;     // in DW, which starts at ((uint32_t*)&ShaderBinaryInfo) - m_chunkUsageBaseOffsetInDW; max is currently 7 dwords (128 T# + 32 V# + 20 CB V# + 16 UAV T#/V#)
  numInputUsageSlots:Byte;           // Up to 16 user data reg slots + 128 extended user data dwords supported by CUE; up to 16 user data reg slots + 240 extended user data dwords supported by Gnm::InputUsageSlot
  isSrt              :bit1;          // 1 if this shader uses shader resource tables and has an SrtDef table embedded below the input usage table and any extended usage info
  isSrtUsedInfoValid :bit1;          // 1 if SrtDef::m_isUsed=0 indicates an element is definitely unused; 0 if SrtDef::m_isUsed=0 indicates only that the element is not known to be used (m_isUsed=1 always indicates a resource is known to be used)
  isExtendedUsageInfo:bit1;          // 1 if this shader has extended usage info for the InputUsage table embedded below the input usage table
  reserved2:bit5; //1;
  reserved3:Byte; //5

  shaderHash0:DWORD;                 // Association hash first 4 bytes
  shaderHash1:DWORD;                 // Association hash second 4 bytes
  crc32      :DWORD;                 // crc32 of shader + this struct, just up till this field
 end;

 PInputUsageSlot=^TInputUsageSlot;
 TInputUsageSlot=packed record
  m_usageType    :Byte; ///< From Gnm::ShaderInputUsageType.
  m_apiSlot      :Byte; ///< API slot or chunk ID.
  m_startRegister:Byte; ///< User data slot.
  Case byte of
   0:(
      b:bitpacked record
       m_registerCount:bit1; ///< If 0, count is 4DW; if 1, count is 8DW. Other sizes are defined by the usage type.
        m_resourceType:bit1; ///< If 0, resource type <c>V#</c>; if 1, resource type <c>T#</c>, in case of a Gnm::kShaderInputUsageImmResource.
            m_reserved:bit2; ///< Unused; must be set to zero.
           m_chunkMask:bit4; ///< Internal usage data.
      end;
     );
   1:(m_srtSizeInDWordMinusOne:Byte); ///< Size of the SRT data; used for Gnm::kShaderInputUsageImmShaderResourceTable.
 end;

const
 //ShaderBinaryType
 kShaderTypePs   = 0;
 kShaderTypeVsVs = 1;
 kShaderTypeVsEs = 2;
 kShaderTypeVsLs = 3;
 kShaderTypeCs   = 4;
 kShaderTypeGs   = 5;
 kShaderTypeHs   = 7;
 kShaderTypeDsVs = 8;

//ShaderType
 kShaderTypeGraphics = $00000000; ///< Configures command buffer for graphics commands.
 kShaderTypeCompute  = $00000001; ///< Configures command buffer for compute commands.

 //ShaderStage
 kShaderStageCs = $00000000;	///< Compute shader stage.
 kShaderStagePs = $00000001;	///< Pixel shader stage.
 kShaderStageVs = $00000002;	///< Vertex shader stage.
 kShaderStageGs = $00000003;	///< Geometry shader stage.
 kShaderStageEs = $00000004;	///< Export shader stage.
 kShaderStageHs = $00000005;	///< Hull shader stage.
 kShaderStageLs = $00000006;	///< LDS shader stage.

 kShaderStageCount = 7; //< The number of shader stages.

 //Specifies which stages should be activated in the graphics shader pipeline.
 //ActiveShaderStages
 kActiveShaderStagesVsPs                = $00000000; ///< VS/PS only.
 kActiveShaderStagesEsGsVsPs            = $000000B0; ///< Geometry shader followed by VS/PS.
 kActiveShaderStagesLsHsVsPs            = $00000045; ///< Tessellation followed by VS/PS.
 kActiveShaderStagesOffChipLsHsVsPs	= $00000145; ///< Off-chip tessellation followed by VS/PS
 kActiveShaderStagesLsHsEsGsVsPs        = $000000AD; ///< Tessellation followed by the geometry shader followed by VS/PS.
 kActiveShaderStagesOffChipLsHsEsGsVsPs	= $000001AD; ///< Off-chip tessellation followed by the geometry shader followed by VS/PS.
 kActiveShaderStagesDispatchDrawVsPs	= $00000200; ///< Dispatch Draw VS/PS only.

 //Describes a data resource expected by a shader.
 //Each input must be bound by the application before the shader runs.
 //ShaderInputUsageType
 kShaderInputUsageImmResource                = $00; ///< Immediate read-only buffer/texture descriptor.
 kShaderInputUsageImmSampler	             = $01; ///< Immediate sampler descriptor.
 kShaderInputUsageImmConstBuffer             = $02; ///< Immediate constant buffer descriptor.
 kShaderInputUsageImmVertexBuffer            = $03; ///< Immediate vertex buffer descriptor.
 kShaderInputUsageImmRwResource		     = $04; ///< Immediate read/write buffer/texture descriptor.
 kShaderInputUsageImmAluFloatConst	     = $05; ///< Immediate float const (scalar or vector).
 kShaderInputUsageImmAluBool32Const	     = $06; ///< 32 immediate Booleans packed into one UINT.
 kShaderInputUsageImmGdsCounterRange	     = $07; ///< Immediate UINT with GDS address range for counters (used for append/consume buffers).
 kShaderInputUsageImmGdsMemoryRange	     = $08; ///< Immediate UINT with GDS address range for storage.
 kShaderInputUsageImmGwsBase                 = $09; ///< Immediate UINT with GWS resource base offset.
 kShaderInputUsageImmShaderResourceTable     = $0A; ///< Pointer to read/write resource indirection table.
 kShaderInputUsageImmLdsEsGsSize             = $0D; ///< Immediate LDS ESGS size used in on-chip GS
 // Skipped several items here...
 kShaderInputUsageSubPtrFetchShader	     = $12; ///< Immediate fetch shader subroutine pointer.
 kShaderInputUsagePtrResourceTable           = $13; ///< Flat resource table pointer.
 kShaderInputUsagePtrInternalResourceTable   = $14; ///< Flat internal resource table pointer.
 kShaderInputUsagePtrSamplerTable	     = $15; ///< Flat sampler table pointer.
 kShaderInputUsagePtrConstBufferTable	     = $16; ///< Flat const buffer table pointer.
 kShaderInputUsagePtrVertexBufferTable       = $17; ///< Flat vertex buffer table pointer.
 kShaderInputUsagePtrSoBufferTable	     = $18; ///< Flat stream-out buffer table pointer.
 kShaderInputUsagePtrRwResourceTable	     = $19; ///< Flat read/write resource table pointer.
 kShaderInputUsagePtrInternalGlobalTable     = $1A; ///< Internal driver table pointer.
 kShaderInputUsagePtrExtendedUserData        = $1B; ///< Extended user data pointer.
 kShaderInputUsagePtrIndirectResourceTable   = $1C; ///< Pointer to resource indirection table.
 kShaderInputUsagePtrIndirectInternalResourceTable = $1D; ///< Pointer to internal resource indirection table.
 kShaderInputUsagePtrIndirectRwResourceTable = $1E; ///< Pointer to read/write resource indirection table.
 // Skipped several items here...
 kShaderInputUsageImmGdsKickRingBufferOffse  = $22;	///< Immediate UINT offset into GDS kick ring buffer for DispatchDraw. This must not be in extended user data.
 kShaderInputUsageImmVertexRingBufferOffse   = $23;	///< Immediate UINT offset into vertex ring buffer for DispatchDraw. This must not be in extended user data.
 kShaderInputUsagePtrDispatchDraw	     = $24;	///< Pointer to DispatchDraw data. This must not be in extended user data.
 kShaderInputUsageImmDispatchDrawInstances   = $25;	///< Immediate UINT ((firstInstance<<16)|(numInstances-1)). This must not be in extended user data.

 //ShaderProgramType
 kShaderProgramTypeLs  = 1 shl 0;
 kShaderProgramTypeHs  = 1 shl 1;
 kShaderProgramTypeEs  = 1 shl 2;
 kShaderProgramTypeGs  = 1 shl 3;
 kShaderProgramTypeVs  = 1 shl 4;
 kShaderProgramTypePs  = 1 shl 5;

 kShaderProgramTypeCsG = 1 shl 10;

 kShaderProgramTypeCs0 = 1 shl 15;
 kShaderProgramTypeCs1 = 1 shl 16;
 kShaderProgramTypeCs2 = 1 shl 17;
 kShaderProgramTypeCs3 = 1 shl 18;
 kShaderProgramTypeCs4 = 1 shl 19;
 kShaderProgramTypeCs5 = 1 shl 20;
 kShaderProgramTypeCs6 = 1 shl 21;
 kShaderProgramTypeCs7 = 1 shl 22;

type
 TShaderType=(
  kInvalidShader,		///< Invalid or unrecognized shader.
  kVertexShader,		///< VS stage shader
  kPixelShader,			///< PS stage shader.
  kGeometryShader,		///< GS stage shader.
  kComputeShader,		///< CS stage shader.
  kExportShader,		///< ES stage shader.
  kLocalShader,			///< LS stage shader.
  kHullShader,			///< HS stage shader.
  kComputeVertexShader);        ///< VS stage shader with embedded CS stage frontend shader.


 TPsslShaderType=(
  kShaderTypeVsShader,
  kShaderTypeFsShader,
  kShaderTypeCsShader,
  kShaderTypeGsShader,
  kShaderTypeHsShader,
  kShaderTypeDsShader,
  kShaderTypeShaderTypeLast
 );

 TPsslCodeType=(
  kCodeTypeIl,
  kCodeTypeIsa,
  kCodeTypeScu,
  kCodeTypeCodeTypeLast
 );

const
                              ///< Memory Type     | L2$ Mode       | Memory Bus         | L1$ Default Behavior | K$ Default Behavior
                              ///< ----------------+----------------+--------------------+----------------------+--------------------
 kResourceMemoryTypePV = $60; ///< Private         | Non Volatile   | Garlic Recommended | LRU                  | Bypass
 kResourceMemoryTypeGC = $6D; ///< GPU Coherent    | Non Volatile   | Garlic Recommended | Bypass               | Bypass
 kResourceMemoryTypeSC = $6E; ///< System Coherent | Volatile       | Onion Recommended  | Bypass               | Bypass
 kResourceMemoryTypeUC = $6F; ///< Uncached        | Not Applicable | Onion Only         | Bypass               | Bypass
 kResourceMemoryTypeRO = $10; ///< Read Only       | Non Volatile   | Garlic Recommended | LRU                  | LRU

type
 Tlod_bits=bitpacked record
   int:bit4;
  frac:bit8;
 end;

 Taniso_bias_bits=bitpacked record
   int:bit1;
  frac:bit5;
 end;

 Tlod_bias_bits=bitpacked record
  sign:bit1;
   int:bit5;
  frac:bit8;
 end;

 Tlod_bias_sec_bits=bitpacked record
  sign:bit1;
   int:bit1;
  frac:bit4;
 end;

 PVBufResource2=^TVBufResource2;
 TVBufResource2=Pointer;

// |Instruction           |Data Format|Num Format |DST SEL |
// +----------------------+-----------+-----------+--------+
// |TBUFFER_LOAD_FORMAT_* |instruction|instruction|identity|
// |TBUFFER_STORE_FORMAT_*|instruction|instruction|identity|
// |BUFFER_LOAD_<type>    |derived    |derived    |identity|
// |BUFFER_STORE_<type>   |derived    |derived    |identity|
// |BUFFER_LOAD_FORMAT_*  |resource   |resource   |resource|
// |BUFFER_STORE_FORMAT_* |resource   |resource   |resource|
// |BUFFER_ATOMIC_*       |derived    |derived    |identity|

 PVSharpResource4=^TVSharpResource4;
 TVSharpResource4=bitpacked record
           base:bit44;
      mtype_L1s:bit2;
       mtype_L2:bit2;
         stride:bit14; //bytes: 0..16383
  cache_swizzle:bit1;
     swizzle_en:bit1;  //swizzle AOS according to stride, index_stride, and element_size, else linear (stride * index + offset)
           //64
    num_records:bit32; //n units of 'stride'
           //32
      dst_sel_x:bit3;  //Destination channel select:
      dst_sel_y:bit3;  //0=0, 1=1, 4=R, 5=G, 6=B, 7=A
      dst_sel_z:bit3;
      dst_sel_w:bit3;
           nfmt:bit3;  //numeric data type (float, int, ...)
           dfmt:bit4;  //data format
   element_size:bit2;  //2, 4, 8, or 16  bytes. Used for swizzled buffer addressing
   index_stride:bit2;  //8, 16, 32, or 64. Used for swizzled buffer addressing
      addtid_en:bit1;  //addtid_en add thread id to the index for addr calc
      reserved1:bit1;
        hash_en:bit1;
      reserved2:bit1;
          mtype:bit3;
          _type:bit2; //value == 0 for buf. Overlaps upper 2 bits of 4-bit TYPE field in 128-bit T# resource
 end;

 PTSharpResource4=^TTSharpResource4;
 TTSharpResource4=bitpacked record
        base:bit38;
    mtype_L2:bit2;
     min_lod:bit12; //fixed point 4.8 minimum LOD (0.0..15.0)
        dfmt:bit6;  //texture data format; num components, num bits
        nfmt:bit4;  //texture numeric format; value conversion
   mtype_L1L:bit2;
        //64
       width:bit14; //texture width (0..16383)
      height:bit14; //texture height (0..16383)
    perf_mod:bit3;  //0=0/16, 1=2/16, 2=5/16, 3=7/16, 4=9/16, 5=11/16, 6=14/16, 7=16/16
  interlaced:bit1;  //texture is interlaced
        //32
   dst_sel_x:bit3;  //Destination channel select:
   dst_sel_y:bit3;  //0=0, 1=1, 4=R, 5=G, 6=B, 7=A
   dst_sel_z:bit3;
   dst_sel_w:bit3;
  base_level:bit4;  //first mip level (0..15)
  last_level:bit4;  //last mip level (0..15); for msaa, number of samples
  tiling_idx:bit5;  //index into lookup table of surface tiling settings
     pow2pad:bit1;  //memory footprint is padded to power of 2 dimensions
   mtype_L1M:bit1;
    reserved:bit1;
       _type:bit4; //values [8..15] are 1D, 2D, 3D, Cube, 1D array, 2D array, 2D MSAA, 2D MSAA array; 0 is V#, 1-7 reserved
 end;

 PTSharpResource8=^TTSharpResource8;
 TTSharpResource8=bitpacked record
        base:bit38;
    mtype_L2:bit2;
     min_lod:bit12; //fixed point 4.8 minimum LOD (0.0..15.0)
        dfmt:bit6;  //texture data format; num components, num bits
        nfmt:bit4;  //texture numeric format; value conversion
   mtype_L1L:bit2;
        //64
       width:bit14; //texture width (0..16383)
      height:bit14; //texture height (0..16383)
    perf_mod:bit3;  //0=0/16, 1=2/16, 2=5/16, 3=7/16, 4=9/16, 5=11/16, 6=14/16, 7=16/16
  interlaced:bit1;  //texture is interlaced
        //32
   dst_sel_x:bit3;  //Destination channel select:
   dst_sel_y:bit3;  //0=0, 1=1, 4=R, 5=G, 6=B, 7=A
   dst_sel_z:bit3;
   dst_sel_w:bit3;
  base_level:bit4;  //first mip level (0..15)
  last_level:bit4;  //last mip level (0..15); for msaa, number of samples
  tiling_idx:bit5;  //index into lookup table of surface tiling settings
     pow2pad:bit1;  //memory footprint is padded to power of 2 dimensions
   mtype_L1M:bit1;
    reserved:bit1;
       _type:bit4;  //values [8..15] are 1D, 2D, 3D, Cube, 1D array, 2D array, 2D MSAA, 2D MSAA array; 0 is V#, 1-7 reserved
        //32
            depth:bit13; //3D texture depth (0..8192)
            pitch:bit14; //texture pitch in texels (0..16383); defaults to width
        reserved2:bit5;
        //32
       base_array:bit13; //first array index (0..16383)
       last_array:bit13; //texture height (0..16383)
        reserved3:bit6;
        //64
     min_lod_warn:bit12; //min mip level to trigger LWE (LOD warn enable); unsigned fixed point 4.8
  counter_bank_id:bit8;  //PRT counter ID
   LOD_hdw_cnt_en:bit1;  //PRT hardware counter enable
        reserved4:bit43;
 end;

 PSSharpResource4=^TSSharpResource4;
 TSSharpResource4=bitpacked record
             clamp_x:bit3;   //Clamp/wrap mode for out of range X coordinate
             clamp_y:bit3;   //Clamp/wrap mode for out of range Y coordinate
             clamp_z:bit3;   //Clamp/wrap mode for out of range Z coordinate
     max_aniso_ratio:bit3;   //Maximum anisotropy ratio (enum)
  depth_compare_func:bit3;   //Depth compare function
  force_unorm_coords:bit1;   //Force unnormalized (texel) address coordinates
     aniso_threshold:bit3;   //Threshold before sampling anisotropically (enum)
      mc_coord_trunc:bit1;
       force_degamma:bit1;   //Force de-gamma after filtering regardless of format
          aniso_bias:bit6;   //Anisotropy bias factor; unsigned fixed point 1.5
         trunc_coord:bit1;
   disable_cube_wrap:bit1;   //Disable sampling/filtering across face boundaries
         filter_mode:bit2;   //LERP, min, or max filter; default: LERP
            reserved:bit1;
             //32
             min_lod:bit12;  //Minimum LOD to allow; unsigned fixed point 4.8
             max_lod:bit12;  //Maximum LOD to allow; unsigned fixed point 4.8
            perf_mip:bit4;   //Bri-linear factor
              perf_z:bit4;
             //32
            lod_bias:bit14;  //Bias to calculated LOD value; signed fixed point 5.8
        lod_bias_sec:bit6;   //Secondary fractional LOD bias; signed fixed point 1.4
       xy_mag_filter:bit2;   //Magnification filter in X,Y coordinate directions
       xy_min_filter:bit2;   //Minification filter in X,Y coordinate directions
            z_filter:bit2;   //Filter in Z coordinate direction for volume textures
          mip_filter:bit2;   //Filter in LOD coordinate direction for mipped textures
           reserved2:bit4;
             //32
    border_color_ptr:bit12;  //Offset into global border color buffer
           reserved3:bit18;
   border_color_type:bit2;   //Opaque-black, transparent-black, white, or color ptr
 end;

function  getCodeAddress(lo,hi:DWORD):Pointer;
function  getFetchAddress(lo,hi:DWORD):Pointer;
function  getBufferAddress(lo,hi:DWORD):Pointer;
function  getIndexAddress(lo,hi:DWORD):Pointer;

function  _calc_shader_size(base:Pointer;size_dw:DWORD=0;setpc:Boolean=false):DWORD;
function  _calc_shader_info_fast(base:Pointer):PShaderBinaryInfo;
function  _calc_shader_info(base:Pointer;size_dw:DWORD=0;setpc:Boolean=false):PShaderBinaryInfo;
function  _calc_shader_slot(info:PShaderBinaryInfo):PInputUsageSlot;

/////////

function  _get_dst_sel_ch(b:Byte):Char;
function  _get_buf_nfmt_str(b:Byte):RawByteString;
function  _get_buf_dfmt_str(b:Byte):RawByteString;
function  _get_element_size_str(b:Byte):RawByteString;
function  _get_index_stride_str(b:Byte):RawByteString;
function  _get_tex_type_str(b:Byte):RawByteString;
function  _get_tex_dfmt_str(b:Byte):RawByteString;
function  _get_tex_nfmt_str(b:Byte):RawByteString;
function  _get_perf_mod_str(b:Byte):RawByteString;
function  _get_lod_str(w:Word):RawByteString;
function  _get_clamp_str(b:Byte):RawByteString;
function  _get_aniso_ratio_str(b:Byte):RawByteString;
function  _get_depth_compare_func_str(b:Byte):RawByteString;
function  _get_aniso_bias_str(b:byte):RawByteString;
function  _get_lod_bias_str(w:word):RawByteString;
function  _get_lod_bias_sec_str(b:byte):RawByteString;
function  _get_filter_mode_str(b:Byte):RawByteString;
function  _get_xy_filter_str(b:Byte):RawByteString;
function  _get_z_filter_str(b:Byte):RawByteString;
function  _get_mip_filter_str(b:Byte):RawByteString;
function  _get_border_color_type_str(b:Byte):RawByteString;

Function  getResourceMemoryType_vsharp(PV:PVSharpResource4):Byte;
Function  getResourceMemoryType_tsharp4(PT:PTSharpResource4):Byte;
Function  getResourceMemoryType_tsharp8(PT:PTSharpResource8):Byte;
Function  getMemoryType_str(b:Byte):RawByteString;
function  get_tiling_idx_str(b:Byte):RawByteString;

procedure print_vsharp(PV:PVSharpResource4);
procedure print_tsharp4(PT:PTSharpResource4);
procedure print_tsharp8(PT:PTSharpResource8);
procedure print_ssharp4(PS:PSSharpResource4);

function  GetElemCount(PT:PTSharpResource4):Byte;
function  GetArrayedType(PT:PTSharpResource4):Byte;
function  GetMsType(PT:PTSharpResource4):Byte;

implementation

function getCodeAddress(lo,hi:DWORD):Pointer;
begin
 Result:={%H-}Pointer((QWORD(hi) shl 40) or (QWORD(lo) shl 8));
end;

function getFetchAddress(lo,hi:DWORD):Pointer;
begin
 Result:={%H-}Pointer((QWORD(hi) shl 32) or (QWORD(lo) and (not 3)));
end;

function getBufferAddress(lo,hi:DWORD):Pointer;
begin
 Result:={%H-}Pointer((QWORD(hi) shl 32) or (QWORD(lo) and (not 3)));
end;

function getIndexAddress(lo,hi:DWORD):Pointer;
begin
 Result:={%H-}Pointer((QWORD(hi) shl 32) or (QWORD(lo) and (not 1)));
end;

function _calc_shader_size(base:Pointer;size_dw:DWORD=0;setpc:Boolean=false):DWORD;
var
 i:Integer;
 _end:Pointer;
begin
 if (base=nil) then Exit(0);
 _end:=ps4_pssl._parse_size(base,size_dw,setpc);
 Result:=(_end-base);
 i:=System.IndexDword(_end^,SHADER_END_SEARCH_MAX_DW,PDWORD(@SHADER_BINARY_INFO_SIG)^);
 If (i<>-1) then
 begin
  Result:=Result+i*SizeOf(DWORD)+SizeOf(TShaderBinaryInfo);
 end;
end;

function _calc_shader_info_fast(base:Pointer):PShaderBinaryInfo;
var
 i:DWORD;
begin
 Result:=nil;
 if (base=nil) then Exit;
 if PDWORD(base)[0]=$BEEB03FF then
 begin
  i:=PDWORD(base)[1];
  i:=(i+1) shl 3;
  base:=base+i;
  if PDWORD(base)[0]=PDWORD(@SHADER_BINARY_INFO_SIG)^ then Exit(base);
  base:=base+4;
  if PDWORD(base)[0]=PDWORD(@SHADER_BINARY_INFO_SIG)^ then Exit(base);
 end;
end;

function _calc_shader_info(base:Pointer;size_dw:DWORD=0;setpc:Boolean=false):PShaderBinaryInfo;
var
 i:Integer;
 _end:Pointer;
begin
 Result:=_calc_shader_info_fast(base);
 if (Result<>nil) or (base=nil) then Exit;
 _end:=ps4_pssl._parse_size(base,size_dw,setpc);
 i:=System.IndexDword(_end^,SHADER_END_SEARCH_MAX_DW,PDWORD(@SHADER_BINARY_INFO_SIG)^);
 If (i<>-1) then
 begin
  Result:=_end+i*SizeOf(DWORD);
 end;
end;

function _calc_shader_slot(info:PShaderBinaryInfo):PInputUsageSlot;
var
 usageMasks:Pointer;
begin
 Result:=nil;
 if (info=nil) then Exit;
 if (info^.numInputUsageSlots<>0) then
 begin
  usageMasks:=Pointer(info)-(info^.chunkUsageBaseOffsetInDW*4);
  Result:=PInputUsageSlot(usageMasks)-info^.numInputUsageSlots;
 end;
end;

/////////

function _get_dst_sel_ch(b:Byte):Char;
begin
 Case b of
  0:Result:='0';
  1:Result:='1';
  4:Result:='R';
  5:Result:='G';
  6:Result:='B';
  7:Result:='A';
  else
    Result:=#0;
 end;
end;

function _get_buf_nfmt_str(b:Byte):RawByteString;
begin
 Case b of
  BUF_NUM_FORMAT_UNORM  :Result:='UNORM';
  BUF_NUM_FORMAT_SNORM  :Result:='SNORM';
  BUF_NUM_FORMAT_USCALED:Result:='USCALED';
  BUF_NUM_FORMAT_SSCALED:Result:='SSCALED';
  BUF_NUM_FORMAT_UINT   :Result:='UINT';
  BUF_NUM_FORMAT_SINT   :Result:='SINT';
  BUF_NUM_FORMAT_FLOAT  :Result:='FLOAT';
  else
    Result:='';
 end;
end;

function _get_buf_dfmt_str(b:Byte):RawByteString;
begin
 Case b of
  BUF_DATA_FORMAT_8          :Result:='8';
  BUF_DATA_FORMAT_16         :Result:='16';
  BUF_DATA_FORMAT_8_8        :Result:='8_8';
  BUF_DATA_FORMAT_32         :Result:='32';
  BUF_DATA_FORMAT_16_16      :Result:='16_16';
  BUF_DATA_FORMAT_10_11_11   :Result:='10_11_11';
  BUF_DATA_FORMAT_11_11_10   :Result:='11_11_10';
  BUF_DATA_FORMAT_10_10_10_2 :Result:='10_10_10_2';
  BUF_DATA_FORMAT_2_10_10_10 :Result:='2_10_10_10';
  BUF_DATA_FORMAT_8_8_8_8    :Result:='8_8_8_8';
  BUF_DATA_FORMAT_32_32      :Result:='32_32';
  BUF_DATA_FORMAT_16_16_16_16:Result:='16_16_16_16';
  BUF_DATA_FORMAT_32_32_32   :Result:='32_32_32';
  BUF_DATA_FORMAT_32_32_32_32:Result:='32_32_32_32';
  else
    Result:='';
 end;
end;

function _get_element_size_str(b:Byte):RawByteString;
begin
 Case b of
  0:Result:='2';
  1:Result:='4';
  2:Result:='8';
  3:Result:='16';
  else
    Result:='';
 end;
end;

function _get_index_stride_str(b:Byte):RawByteString;
begin
 Case b of
  0:Result:='8';
  1:Result:='16';
  2:Result:='32';
  3:Result:='64';
  else
    Result:='';
 end;
end;

function _get_tex_type_str(b:Byte):RawByteString;
begin
 Case b of
  SQ_RSRC_IMG_1D           :Result:='1D';
  SQ_RSRC_IMG_2D           :Result:='2D';
  SQ_RSRC_IMG_3D           :Result:='3D';
  SQ_RSRC_IMG_CUBE         :Result:='CUBE';
  SQ_RSRC_IMG_1D_ARRAY     :Result:='1D_ARRAY';
  SQ_RSRC_IMG_2D_ARRAY     :Result:='2D_ARRAY';
  SQ_RSRC_IMG_2D_MSAA      :Result:='2D_MSAA';
  SQ_RSRC_IMG_2D_MSAA_ARRAY:Result:='2D_MSAA_ARRAY';
  else
    Result:='';
 end;
end;

function _get_tex_dfmt_str(b:Byte):RawByteString;
begin
 Case b of
  IMG_DATA_FORMAT_8                :Result:='8';
  IMG_DATA_FORMAT_16               :Result:='16';
  IMG_DATA_FORMAT_8_8              :Result:='8_8';
  IMG_DATA_FORMAT_32               :Result:='32';
  IMG_DATA_FORMAT_16_16            :Result:='16_16';
  IMG_DATA_FORMAT_10_11_11         :Result:='10_11_11';
  IMG_DATA_FORMAT_11_11_10         :Result:='11_11_10';
  IMG_DATA_FORMAT_10_10_10_2       :Result:='10_10_10_2';
  IMG_DATA_FORMAT_2_10_10_10       :Result:='2_10_10_10';
  IMG_DATA_FORMAT_8_8_8_8          :Result:='8_8_8_8';
  IMG_DATA_FORMAT_32_32            :Result:='32_32';
  IMG_DATA_FORMAT_16_16_16_16      :Result:='16_16_16_16';
  IMG_DATA_FORMAT_32_32_32         :Result:='32_32_32';
  IMG_DATA_FORMAT_32_32_32_32      :Result:='32_32_32_32';
  IMG_DATA_FORMAT_5_6_5            :Result:='5_6_5';
  IMG_DATA_FORMAT_1_5_5_5          :Result:='1_5_5_5';
  IMG_DATA_FORMAT_5_5_5_1          :Result:='5_5_5_1';
  IMG_DATA_FORMAT_4_4_4_4          :Result:='4_4_4_4';
  IMG_DATA_FORMAT_8_24             :Result:='8_24';
  IMG_DATA_FORMAT_24_8             :Result:='24_8';
  IMG_DATA_FORMAT_X24_8_32         :Result:='X24_8_32';
  IMG_DATA_FORMAT_ETC2_RGB         :Result:='ETC2_RGB';
  IMG_DATA_FORMAT_ETC2_RGBA        :Result:='ETC2_RGBA';
  IMG_DATA_FORMAT_ETC2_R           :Result:='ETC2_R';
  IMG_DATA_FORMAT_ETC2_RG          :Result:='ETC2_RG';
  IMG_DATA_FORMAT_ETC2_RGBA1       :Result:='ETC2_RGBA1';
  IMG_DATA_FORMAT_GB_GR            :Result:='GB_GR';
  IMG_DATA_FORMAT_BG_RG            :Result:='BG_RG';
  IMG_DATA_FORMAT_5_9_9_9          :Result:='5_9_9_9';
  IMG_DATA_FORMAT_BC1              :Result:='BC1';
  IMG_DATA_FORMAT_BC2              :Result:='BC2';
  IMG_DATA_FORMAT_BC3              :Result:='BC3';
  IMG_DATA_FORMAT_BC4              :Result:='BC4';
  IMG_DATA_FORMAT_BC5              :Result:='BC5';
  IMG_DATA_FORMAT_BC6              :Result:='BC6';
  IMG_DATA_FORMAT_BC7              :Result:='BC7';
  IMG_DATA_FORMAT_FMASK8_S2_F1     :Result:='FMASK8_S2_F1';
  IMG_DATA_FORMAT_FMASK8_S4_F1     :Result:='FMASK8_S4_F1';
  IMG_DATA_FORMAT_FMASK8_S8_F1     :Result:='FMASK8_S8_F1';
  IMG_DATA_FORMAT_FMASK8_S2_F2     :Result:='FMASK8_S2_F2';
  IMG_DATA_FORMAT_FMASK8_S4_F2     :Result:='FMASK8_S4_F2';
  IMG_DATA_FORMAT_FMASK8_S4_F4     :Result:='FMASK8_S4_F4';
  IMG_DATA_FORMAT_FMASK16_S16_F1   :Result:='FMASK16_S16_F1';
  IMG_DATA_FORMAT_FMASK16_S8_F2    :Result:='FMASK16_S8_F2';
  IMG_DATA_FORMAT_FMASK32_S16_F2   :Result:='FMASK32_S16_F2';
  IMG_DATA_FORMAT_FMASK32_S8_F4    :Result:='FMASK32_S8_F4';
  IMG_DATA_FORMAT_FMASK32_S8_F8    :Result:='FMASK32_S8_F8';
  IMG_DATA_FORMAT_FMASK64_S16_F4   :Result:='FMASK64_S16_F4';
  IMG_DATA_FORMAT_FMASK64_S16_F8   :Result:='FMASK64_S16_F8';
  IMG_DATA_FORMAT_4_4              :Result:='4_4';
  IMG_DATA_FORMAT_6_5_5            :Result:='6_5_5';
  IMG_DATA_FORMAT_1                :Result:='1';
  IMG_DATA_FORMAT_32_AS_8          :Result:='32_AS_8';
  IMG_DATA_FORMAT_32_AS_8_8        :Result:='32_AS_8_8';
  IMG_DATA_FORMAT_32_AS_32_32_32_32:Result:='32_AS_32_32_32_32';
  else
    Result:='';
 end;
end;

function _get_tex_nfmt_str(b:Byte):RawByteString;
begin
 Case b of
  IMG_NUM_FORMAT_UNORM  :Result:='UNORM';
  IMG_NUM_FORMAT_SNORM  :Result:='SNORM';
  IMG_NUM_FORMAT_USCALED:Result:='USCALED';
  IMG_NUM_FORMAT_SSCALED:Result:='SSCALED';
  IMG_NUM_FORMAT_UINT   :Result:='UINT';
  IMG_NUM_FORMAT_SINT   :Result:='SINT';
  IMG_NUM_FORMAT_FLOAT  :Result:='FLOAT';
  IMG_NUM_FORMAT_SRGB   :Result:='SRGB';
  else
    Result:='';
 end;
end;

function _get_perf_mod_str(b:Byte):RawByteString;
begin
 Case b of
  0:Result:='0/16';
  1:Result:='2/16';
  2:Result:='5/16';
  3:Result:='7/16';
  4:Result:='9/16';
  5:Result:='11/16';
  6:Result:='14/16';
  7:Result:='16/16';
  else
    Result:='';
 end;
end;

function _get_lod_str(w:Word):RawByteString;
begin
 Result:=IntToStr(Tlod_bits(w).int)+'.('+IntToStr(Tlod_bits(w).frac)+'/256)';
end;

function _get_clamp_str(b:Byte):RawByteString;
begin
 Case b of
  SQ_TEX_WRAP                   :Result:='WRAP';
  SQ_TEX_MIRROR                 :Result:='MIRROR';
  SQ_TEX_CLAMP_LAST_TEXEL       :Result:='CLAMP_LAST_TEXEL';
  SQ_TEX_MIRROR_ONCE_LAST_TEXEL :Result:='MIRROR_ONCE_LAST_TEXEL ';
  SQ_TEX_CLAMP_HALF_BORDER      :Result:='CLAMP_HALF_BORDER';
  SQ_TEX_MIRROR_ONCE_HALF_BORDER:Result:='MIRROR_ONCE_HALF_BORDER';
  SQ_TEX_CLAMP_BORDER           :Result:='CLAMP_BORDER';
  SQ_TEX_MIRROR_ONCE_BORDER     :Result:='MIRROR_ONCE_BORDER';
  else
    Result:='';
 end;
end;

function _get_aniso_ratio_str(b:Byte):RawByteString;
begin
 Case b of
  SQ_TEX_ANISO_RATIO_1 :Result:='1';
  SQ_TEX_ANISO_RATIO_2 :Result:='2';
  SQ_TEX_ANISO_RATIO_4 :Result:='4';
  SQ_TEX_ANISO_RATIO_8 :Result:='8';
  SQ_TEX_ANISO_RATIO_16:Result:='16';
  else
    Result:='';
 end;
end;

function _get_depth_compare_func_str(b:Byte):RawByteString;
begin
 Case b of
  SQ_TEX_DEPTH_COMPARE_NEVER       :Result:='NEVER';
  SQ_TEX_DEPTH_COMPARE_LESS        :Result:='LESS';
  SQ_TEX_DEPTH_COMPARE_EQUAL       :Result:='EQUAL';
  SQ_TEX_DEPTH_COMPARE_LESSEQUAL   :Result:='LESSEQUAL';
  SQ_TEX_DEPTH_COMPARE_GREATER     :Result:='GREATER';
  SQ_TEX_DEPTH_COMPARE_NOTEQUAL    :Result:='NOTEQUAL';
  SQ_TEX_DEPTH_COMPARE_GREATEREQUAL:Result:='GREATEREQUAL';
  SQ_TEX_DEPTH_COMPARE_ALWAYS      :Result:='ALWAYS';
  else
    Result:='';
 end;
end;

function _get_aniso_bias_str(b:byte):RawByteString;
begin
 with Taniso_bias_bits(b) do
 begin
  Result:=IntToStr(int)+'.('+IntToStr(frac)+'/32)';
 end;
end;

function _get_lod_bias_str(w:word):RawByteString;
begin
 with Tlod_bias_bits(w) do
 begin
  Result:=IntToStr(int)+'.('+IntToStr(frac)+'/256)';
  if (sign<>0) then
  begin
   Result:='-'+Result;
  end;
 end;
end;

function _get_lod_bias_sec_str(b:byte):RawByteString;
begin
 with Tlod_bias_sec_bits(b) do
 begin
  Result:=IntToStr(int)+'.('+IntToStr(frac)+'/16)';
  if (sign<>0) then
  begin
   Result:='-'+Result;
  end;
 end;
end;

function _get_filter_mode_str(b:Byte):RawByteString;
begin
 Case b of
  SQ_IMG_FILTER_MODE_BLEND:Result:='BLEND';
  SQ_IMG_FILTER_MODE_MIN  :Result:='MIN';
  SQ_IMG_FILTER_MODE_MAX  :Result:='MAX';
  else
    Result:='';
 end;
end;

function _get_xy_filter_str(b:Byte):RawByteString;
begin
 Case b of
  TEX_XYFilter_Point      :Result:='Point';
  TEX_XYFilter_Linear     :Result:='Linear';
  TEX_XYFilter_AnisoPoint :Result:='AnisoPoint';
  TEX_XYFilter_AnisoLinear:Result:='AnisoLinear';
  else
    Result:='';
 end;
end;

function _get_z_filter_str(b:Byte):RawByteString;
begin
 Case b of
  TEX_ZFilter_None  :Result:='None';
  TEX_ZFilter_Point :Result:='Point';
  TEX_ZFilter_Linear:Result:='Linear';
  else
    Result:='';
 end;
end;

function _get_mip_filter_str(b:Byte):RawByteString;
begin
 Case b of
  TEX_MipFilter_None           :Result:='None';
  TEX_MipFilter_Point          :Result:='Point';
  TEX_MipFilter_Linear         :Result:='Linear';
  TEX_MipFilter_Point_Aniso_Adj:Result:='Point_Aniso_Adj';
  else
    Result:='';
 end;
end;

function _get_border_color_type_str(b:Byte):RawByteString;
begin
 Case b of
  TEX_BorderColor_TransparentBlack:Result:='TransparentBlack';
  TEX_BorderColor_OpaqueBlack     :Result:='OpaqueBlack';
  TEX_BorderColor_OpaqueWhite     :Result:='OpaqueWhite';
  TEX_BorderColor_Register        :Result:='Register';
  else
    Result:='';
 end;
end;

Function getResourceMemoryType_vsharp(PV:PVSharpResource4):Byte;
begin
 Result:=(PV^.mtype_L1s shl 5) or
         (PV^.mtype_L2) or
         (PV^.mtype shl 2);
end;

Function getResourceMemoryType_tsharp4(PT:PTSharpResource4):Byte;
begin
 if (PT^.mtype_L1M<>0) then
 begin
  Result:=$10;
 end else
 begin
  Result:=$60;
 end;
 Result:=Result or
         (PT^.mtype_L2) or
         (PT^.mtype_L1L shl 2);
end;

Function getResourceMemoryType_tsharp8(PT:PTSharpResource8):Byte;
begin
 if (PT^.mtype_L1M<>0) then
 begin
  Result:=$10;
 end else
 begin
  Result:=$60;
 end;
 Result:=Result or
         (PT^.mtype_L2) or
         (PT^.mtype_L1L shl 2);
end;

Function getMemoryType_str(b:Byte):RawByteString;
begin
 Case b of
  kResourceMemoryTypePV:Result:='Private';
  kResourceMemoryTypeGC:Result:='GPU Coherent';
  kResourceMemoryTypeSC:Result:='System Coherent';
  kResourceMemoryTypeUC:Result:='Uncached';
  kResourceMemoryTypeRO:Result:='Read Only';
  else
    Result:='0x'+HexStr(b,2);
 end;
end;

function get_tiling_idx_str(b:Byte):RawByteString;
begin
 Case b of
  $00:Result:='Depth_2dThin_64';
  $01:Result:='Depth_2dThin_128';
  $02:Result:='Depth_2dThin_256';
  $03:Result:='Depth_2dThin_512';
  $04:Result:='Depth_2dThin_1K';
  $05:Result:='Depth_1dThin';
  $06:Result:='Depth_2dThinPrt_256';
  $07:Result:='Depth_2dThinPrt_1K';
  $08:Result:='Display_LinearAligned';
  $09:Result:='Display_1dThin';
  $0A:Result:='Display_2dThin';
  $0B:Result:='Display_ThinPrt';
  $0C:Result:='Display_2dThinPrt';
  $0D:Result:='Thin_1dThin';
  $0E:Result:='Thin_2dThin';
  $0F:Result:='Thin_3dThin';
  $10:Result:='Thin_ThinPrt';
  $11:Result:='Thin_2dThinPrt';
  $12:Result:='Thin_3dThinPrt';
  $13:Result:='Thick_1dThick';
  $14:Result:='Thick_2dThick';
  $15:Result:='Thick_3dThick';
  $16:Result:='Thick_ThickPrt';
  $17:Result:='Thick_2dThickPrt';
  $18:Result:='Thick_3dThickPrt';
  $19:Result:='Thick_2dXThick';
  $1A:Result:='Thick_3dXThick';
  $1F:Result:='Display_LinearGeneral';
  else
    Result:='';
 end;
end;

procedure print_vsharp(PV:PVSharpResource4);
begin
 if (PV=nil) then Exit;
 Writeln('base=',HexStr(PV^.base,10));
 Writeln('stride=',PV^.stride);
 Writeln('cache_swizzle=',PV^.cache_swizzle);
 Writeln('swizzle_en=',PV^.swizzle_en);
 Writeln('num_records=',PV^.num_records);
 Writeln('dst_sel=',
  _get_dst_sel_ch(PV^.dst_sel_x)+
  _get_dst_sel_ch(PV^.dst_sel_y)+
  _get_dst_sel_ch(PV^.dst_sel_z)+
  _get_dst_sel_ch(PV^.dst_sel_w));

 Writeln('nfmt=',_get_buf_nfmt_str(PV^.nfmt)); //numeric data type (float, int, ...)
 Writeln('dfmt=',_get_buf_dfmt_str(PV^.dfmt)); //data format

 Writeln('element_size=',_get_element_size_str(PV^.element_size));
 Writeln('index_stride=',_get_index_stride_str(PV^.index_stride));

 Writeln('addtid_en=',PV^.addtid_en);
 Writeln('hash_en=',PV^.hash_en);

 Writeln('MemoryType=',getMemoryType_str(getResourceMemoryType_vsharp(PV)));

 Writeln('type=',PV^._type);
end;

procedure print_tsharp4(PT:PTSharpResource4);
begin
 if (PT=nil) then Exit;
 Writeln('base=',HexStr(PT^.base shl 8,10));
 Writeln('min_lod=',_get_lod_str(PT^.min_lod));

 Writeln('dfmt=',_get_tex_dfmt_str(PT^.dfmt));
 Writeln('nfmt=',_get_tex_nfmt_str(PT^.nfmt));

 Writeln('width=',PT^.width+1);
 Writeln('height=',PT^.height+1);

 Writeln('perf_mod=',_get_perf_mod_str(PT^.perf_mod));
 Writeln('interlaced=',PT^.interlaced);

 Writeln('dst_sel=',
  _get_dst_sel_ch(PT^.dst_sel_x)+
  _get_dst_sel_ch(PT^.dst_sel_y)+
  _get_dst_sel_ch(PT^.dst_sel_z)+
  _get_dst_sel_ch(PT^.dst_sel_w));

 Writeln('base_level=',PT^.base_level);
 Writeln('last_level=',PT^.last_level);
 Writeln('tiling_idx=',get_tiling_idx_str(PT^.tiling_idx));
 Writeln('pow2pad=',PT^.pow2pad);

 Writeln('MemoryType=',getMemoryType_str(getResourceMemoryType_tsharp4(PT)));

 Writeln('type=',_get_tex_type_str(PT^._type));
end;

procedure print_tsharp8(PT:PTSharpResource8);
begin
 if (PT=nil) then Exit;
 print_tsharp4(Pointer(PT));

 Writeln('depth=',PT^.depth+1);
 Writeln('pitch=',PT^.pitch+1);

 Writeln('base_array=',PT^.base_array);
 Writeln('last_array=',PT^.last_array);

 Writeln('min_lod_warn=',_get_lod_str(PT^.min_lod_warn));

 Writeln('counter_bank_id=',PT^.counter_bank_id);
 Writeln('LOD_hdw_cnt_en=',PT^.LOD_hdw_cnt_en);
end;

procedure print_ssharp4(PS:PSSharpResource4);
begin
 if (PS=nil) then Exit;

 Writeln('clamp_x=',_get_clamp_str(PS^.clamp_x));
 Writeln('clamp_y=',_get_clamp_str(PS^.clamp_y));
 Writeln('clamp_z=',_get_clamp_str(PS^.clamp_z));

 Writeln('max_aniso_ratio=',_get_aniso_ratio_str(PS^.max_aniso_ratio));
 Writeln('depth_compare_func=',_get_depth_compare_func_str(PS^.depth_compare_func));

 Writeln('force_unorm_coords=',PS^.force_unorm_coords);

 Writeln('aniso_threshold=',PS^.aniso_threshold,'/8');

 Writeln('mc_coord_trunc=',PS^.mc_coord_trunc);
 Writeln('force_degamma=',PS^.force_degamma);

 Writeln('aniso_bias=',_get_aniso_bias_str(PS^.aniso_bias));

 Writeln('trunc_coord=',PS^.trunc_coord);
 Writeln('disable_cube_wrap=',PS^.disable_cube_wrap);

 Writeln('filter_mode=',_get_filter_mode_str(PS^.filter_mode));

 Writeln('min_lod=',_get_lod_str(PS^.min_lod));
 Writeln('max_lod=',_get_lod_str(PS^.max_lod));

 Writeln('perf_mip=',PS^.perf_mip);
 Writeln('perf_z=',PS^.perf_z);

 Writeln('lod_bias=',_get_lod_bias_str(PS^.lod_bias));
 Writeln('lod_bias_sec=',_get_lod_bias_sec_str(PS^.lod_bias_sec));

 Writeln('xy_mag_filter=',_get_xy_filter_str(PS^.xy_mag_filter));
 Writeln('xy_min_filter=',_get_xy_filter_str(PS^.xy_min_filter));
 Writeln('z_filter=',_get_z_filter_str(PS^.z_filter));
 Writeln('mip_filter=',_get_mip_filter_str(PS^.mip_filter));

 Writeln('border_color_ptr=',PS^.border_color_ptr);

 Writeln('border_color_type=',_get_border_color_type_str(PS^.border_color_type));
end;

function GetElemCount(PT:PTSharpResource4):Byte;
begin
 Result:=1;
 if (PT=nil) then Exit;
 Case PT^.dfmt of

  IMG_DATA_FORMAT_8                :Result:=1;
  IMG_DATA_FORMAT_ETC2_R           :Result:=1;
  IMG_DATA_FORMAT_BC4              :Result:=1;
  IMG_DATA_FORMAT_1                :Result:=1;
  IMG_DATA_FORMAT_32_AS_8          :Result:=1;

  IMG_DATA_FORMAT_FMASK8_S2_F1     :Result:=1;
  IMG_DATA_FORMAT_FMASK8_S4_F1     :Result:=1;
  IMG_DATA_FORMAT_FMASK8_S8_F1     :Result:=1;
  IMG_DATA_FORMAT_FMASK8_S2_F2     :Result:=1;
  IMG_DATA_FORMAT_FMASK8_S4_F2     :Result:=1;
  IMG_DATA_FORMAT_FMASK8_S4_F4     :Result:=1;
  IMG_DATA_FORMAT_FMASK16_S16_F1   :Result:=1;
  IMG_DATA_FORMAT_FMASK16_S8_F2    :Result:=1;
  IMG_DATA_FORMAT_FMASK32_S16_F2   :Result:=1;
  IMG_DATA_FORMAT_FMASK32_S8_F4    :Result:=1;
  IMG_DATA_FORMAT_FMASK32_S8_F8    :Result:=1;
  IMG_DATA_FORMAT_FMASK64_S16_F4   :Result:=1;
  IMG_DATA_FORMAT_FMASK64_S16_F8   :Result:=1;

  IMG_DATA_FORMAT_8_8              :Result:=2;
  IMG_DATA_FORMAT_16_16            :Result:=2;
  IMG_DATA_FORMAT_32_32            :Result:=2;
  IMG_DATA_FORMAT_8_24             :Result:=2;
  IMG_DATA_FORMAT_24_8             :Result:=2;
  IMG_DATA_FORMAT_X24_8_32         :Result:=2;
  IMG_DATA_FORMAT_ETC2_RG          :Result:=2;
  IMG_DATA_FORMAT_BC5              :Result:=2;
  IMG_DATA_FORMAT_4_4              :Result:=2;
  IMG_DATA_FORMAT_32_AS_8_8        :Result:=2;

  IMG_DATA_FORMAT_10_11_11         :Result:=3;
  IMG_DATA_FORMAT_11_11_10         :Result:=3;
  IMG_DATA_FORMAT_32_32_32         :Result:=3;
  IMG_DATA_FORMAT_5_6_5            :Result:=3;
  IMG_DATA_FORMAT_ETC2_RGB         :Result:=3;
  IMG_DATA_FORMAT_GB_GR            :Result:=3;
  IMG_DATA_FORMAT_BG_RG            :Result:=3;
  IMG_DATA_FORMAT_BC1              :Result:=3;
  IMG_DATA_FORMAT_BC6              :Result:=3;
  IMG_DATA_FORMAT_6_5_5            :Result:=3;

  IMG_DATA_FORMAT_10_10_10_2       :Result:=4;
  IMG_DATA_FORMAT_2_10_10_10       :Result:=4;
  IMG_DATA_FORMAT_8_8_8_8          :Result:=4;
  IMG_DATA_FORMAT_16_16_16_16      :Result:=4;
  IMG_DATA_FORMAT_32_32_32_32      :Result:=4;
  IMG_DATA_FORMAT_1_5_5_5          :Result:=4;
  IMG_DATA_FORMAT_5_5_5_1          :Result:=4;
  IMG_DATA_FORMAT_4_4_4_4          :Result:=4;
  IMG_DATA_FORMAT_ETC2_RGBA        :Result:=4;
  IMG_DATA_FORMAT_ETC2_RGBA1       :Result:=4;
  IMG_DATA_FORMAT_5_9_9_9          :Result:=4;
  IMG_DATA_FORMAT_BC2              :Result:=4;
  IMG_DATA_FORMAT_BC3              :Result:=4;
  IMG_DATA_FORMAT_BC7              :Result:=4;
  IMG_DATA_FORMAT_32_AS_32_32_32_32:Result:=4;
  else;
 end;
end;

function GetArrayedType(PT:PTSharpResource4):Byte;
begin
 Result:=0;
 if (PT=nil) then Exit;
 Case PT^._type of
  SQ_RSRC_IMG_1D_ARRAY,
  SQ_RSRC_IMG_2D_ARRAY,
  SQ_RSRC_IMG_2D_MSAA_ARRAY:Result:=1;
  else;
 end;
end;

function GetMsType(PT:PTSharpResource4):Byte;
begin
 Result:=0;
 if (PT=nil) then Exit;
 Case PT^._type of
  SQ_RSRC_IMG_2D_MSAA,
  SQ_RSRC_IMG_2D_MSAA_ARRAY:Result:=1;
  else;
 end;
end;

end.

