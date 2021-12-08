unit ps4_shader;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,
  bittype,
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

type
 PVSharpResource=^TVSharpResource;
 TVSharpResource=bitpacked record
           base:bit44;
      mtype_L1s:bit2;
       mtype_L2:bit2;
         stride:bit14;
  cache_swizzle:bit1;
     swizzle_en:bit1;  //swizzle AOS according to stride, index_stride, and element_size, else linear (stride * index + offset)
           //64
    num_records:bit32; //n units of 'stride'
      dst_sel_x:bit3;  //Destination channel select:
      dst_sel_y:bit3;  //0=0, 1=1, 4=R, 5=G, 6=B, 7=A
      dst_sel_z:bit3;
      dst_sel_w:bit3;
           nfmt:bit3;  //numeric data type (float, int, ...)
           dfmt:bit4;  //data format
   element_size:bit2;  //2, 4, 8, or 16  bytes. Used for swizzled buffer addressing
   index_stride:bit2;  //8, 16, 32, or 64. Used for swizzled buffer addressing
      addtid_en:bit1;  //addtid_en add thread id to the index for addr calc
       reserved:bit1;
        hash_en:bit1;
          mtype:bit3;
          _type:bit2; //value == 0 for buf. Overlaps upper 2 bits of 4-bit TYPE field in 128-bit T# resource
 end;

 PTSharpResource=^TTSharpResource;
 TTSharpResource=bitpacked record
        base:bit38;
    mtype_L2:bit2;
     min_lod:bit12;
        dfmt:bit6;
        nfmt:bit4;
   mtype_L1L:bit2;
        //64
       width:bit14;
      height:bit14;
    perf_mod:bit3;
  interlaced:bit1;
   dst_sel_x:bit3;
   dst_sel_y:bit3;
   dst_sel_z:bit3;
   dst_sel_w:bit3;
  base_level:bit4;
  last_level:bit4;
  tiling_idx:bit5;
     pow2pad:bit1;
   mtype_L1M:bit1;
    reserved:bit1;
       _type:bit4;
        //
 end;

 PVBufResource=^TVBufResource;
 TVBufResource=Pointer;

function  _calc_shader_size(base:Pointer;size_dw:DWORD=0;setpc:Boolean=false):DWORD;
function  _calc_shader_info(base:Pointer;size_dw:DWORD=0;setpc:Boolean=false):PShaderBinaryInfo;
function  _calc_shader_slot(info:PShaderBinaryInfo):PInputUsageSlot;

implementation

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

function _calc_shader_info(base:Pointer;size_dw:DWORD=0;setpc:Boolean=false):PShaderBinaryInfo;
var
 i:Integer;
 _end:Pointer;
begin
 Result:=nil;
 if (base=nil) then Exit;
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

end.

