unit emit_MIMG;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  spirv,
  si_ci_vi_merged_enum,
  ps4_shader,
  ps4_pssl,
  srNode,
  srType,
  srReg,
  srLayout,
  srOp,
  emit_fetch;

type
 TsrImageMods=Set Of (imMinLod,imGrad,imLod,imBiasLod,imZeroLod,imDref,imOffset);

 TImgSampleParam=object
  roffset:DWORD;
  mods:TsrImageMods;
  img_op:Integer;

  coord,pcf,bias,lod,min_lod,offset:PsrRegNode;
 end;

 TEmit_MIMG=class(TEmitFetch)
  procedure emit_MIMG;
  procedure DistribDmask(DMASK:Byte;dst:PsrRegNode;info:PsrImageInfo);
  function  GatherDmask(telem:TsrDataType):PsrRegNode;
  Function  GatherCoord_f(var offset:DWORD;info:PsrImageInfo):PsrRegNode;
  Function  GatherCoord_u(var offset:DWORD;info:PsrImageInfo):PsrRegNode;
  Function  Gather_value(var offset:DWORD;rtype:TsrDataType):PsrRegNode;
  Function  Gather_packed_offset(var offset:DWORD;dim:Byte):PsrRegNode;
  procedure Gather_sample_param(var p:TImgSampleParam;info:PsrImageInfo);
  procedure add_sample_op(var p:TImgSampleParam;node:PSpirvOp);
  procedure emit_image_sample(Tgrp:PsrNode;info:PsrImageInfo);
  procedure emit_image_sample_gather(Tgrp:PsrNode;info:PsrImageInfo);
  procedure emit_image_load(Tgrp:PsrNode;info:PsrImageInfo);
  procedure emit_image_store(Tgrp:PsrNode;info:PsrImageInfo);
  procedure emit_image_get_resinfo(Tgrp:PsrNode;info:PsrImageInfo);
  procedure emit_image_get_lod(Tgrp:PsrNode;info:PsrImageInfo);
 end;

implementation

function GetImageFormat(PT:PTSharpResource4):Byte;
begin
 Result:=ImageFormat.Unknown;
 if (PT=nil) then Exit;
 Case PT^.nfmt of
  IMG_NUM_FORMAT_UNORM,
  IMG_NUM_FORMAT_SRGB:
    begin //R8, Rg8,Rg16, Rgba8,Rgba16,Rgb10A2

     Case PT^.dfmt of

      IMG_DATA_FORMAT_8                :Result:=ImageFormat.R8;
      IMG_DATA_FORMAT_ETC2_R           :Result:=ImageFormat.R8;
      IMG_DATA_FORMAT_BC4              :Result:=ImageFormat.R8;
      IMG_DATA_FORMAT_1                :Result:=ImageFormat.R8;
      IMG_DATA_FORMAT_32_AS_8          :Result:=ImageFormat.R8;

      IMG_DATA_FORMAT_8_8              :Result:=ImageFormat.Rg8;
      IMG_DATA_FORMAT_16_16            :Result:=ImageFormat.Rg16;
      IMG_DATA_FORMAT_ETC2_RG          :Result:=ImageFormat.Rg8;
      IMG_DATA_FORMAT_BC5              :Result:=ImageFormat.Rg8;
      IMG_DATA_FORMAT_4_4              :Result:=ImageFormat.Rg8;
      IMG_DATA_FORMAT_32_AS_8_8        :Result:=ImageFormat.Rg8;

      IMG_DATA_FORMAT_10_11_11         :Result:=ImageFormat.Rgb10A2;
      IMG_DATA_FORMAT_11_11_10         :Result:=ImageFormat.Rgb10A2;
      IMG_DATA_FORMAT_5_6_5            :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_ETC2_RGB         :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_GB_GR            :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_BG_RG            :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_BC1              :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_BC6              :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_6_5_5            :Result:=ImageFormat.Rgba8;

      IMG_DATA_FORMAT_10_10_10_2       :Result:=ImageFormat.Rgb10A2;
      IMG_DATA_FORMAT_2_10_10_10       :Result:=ImageFormat.Rgb10A2;
      IMG_DATA_FORMAT_8_8_8_8          :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_1_5_5_5          :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_5_5_5_1          :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_4_4_4_4          :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_ETC2_RGBA        :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_ETC2_RGBA1       :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_5_9_9_9          :Result:=ImageFormat.Rgba16;
      IMG_DATA_FORMAT_BC2              :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_BC3              :Result:=ImageFormat.Rgba8;
      IMG_DATA_FORMAT_BC7              :Result:=ImageFormat.Rgba8;

      else;
     end;

    end;

  IMG_NUM_FORMAT_SNORM:
    begin //R8Snorm, Rg8Snorm,Rg16Snorm, Rgba8Snorm,Rgba16Snorm

     Case PT^.dfmt of

      IMG_DATA_FORMAT_8                :Result:=ImageFormat.R8Snorm;
      IMG_DATA_FORMAT_ETC2_R           :Result:=ImageFormat.R8Snorm;
      IMG_DATA_FORMAT_BC4              :Result:=ImageFormat.R8Snorm;
      IMG_DATA_FORMAT_1                :Result:=ImageFormat.R8Snorm;
      IMG_DATA_FORMAT_32_AS_8          :Result:=ImageFormat.R8Snorm;

      IMG_DATA_FORMAT_8_8              :Result:=ImageFormat.Rg8Snorm;
      IMG_DATA_FORMAT_16_16            :Result:=ImageFormat.Rg16Snorm;
      IMG_DATA_FORMAT_ETC2_RG          :Result:=ImageFormat.Rg8Snorm;
      IMG_DATA_FORMAT_BC5              :Result:=ImageFormat.Rg8Snorm;
      IMG_DATA_FORMAT_4_4              :Result:=ImageFormat.Rg8Snorm;
      IMG_DATA_FORMAT_32_AS_8_8        :Result:=ImageFormat.Rg8Snorm;

      IMG_DATA_FORMAT_10_11_11         :Result:=ImageFormat.Rgba16Snorm;
      IMG_DATA_FORMAT_11_11_10         :Result:=ImageFormat.Rgba16Snorm;
      IMG_DATA_FORMAT_5_6_5            :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_ETC2_RGB         :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_GB_GR            :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_BG_RG            :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_BC1              :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_BC6              :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_6_5_5            :Result:=ImageFormat.Rgba8Snorm;

      IMG_DATA_FORMAT_10_10_10_2       :Result:=ImageFormat.Rgba16Snorm;
      IMG_DATA_FORMAT_2_10_10_10       :Result:=ImageFormat.Rgba16Snorm;
      IMG_DATA_FORMAT_8_8_8_8          :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_1_5_5_5          :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_5_5_5_1          :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_4_4_4_4          :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_ETC2_RGBA        :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_ETC2_RGBA1       :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_5_9_9_9          :Result:=ImageFormat.Rgba16Snorm;
      IMG_DATA_FORMAT_BC2              :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_BC3              :Result:=ImageFormat.Rgba8Snorm;
      IMG_DATA_FORMAT_BC7              :Result:=ImageFormat.Rgba8Snorm;

      else;
     end;

    end;

  IMG_NUM_FORMAT_UINT:
    begin //R8ui,R16ui,R32ui, Rg8ui,Rg16ui,Rg32ui, Rgba8ui,Rgba16ui,Rgba32ui,Rgb10a2ui

     Case PT^.dfmt of

      IMG_DATA_FORMAT_8                :Result:=ImageFormat.R8ui;
      IMG_DATA_FORMAT_16               :Result:=ImageFormat.R16ui;
      IMG_DATA_FORMAT_32               :Result:=ImageFormat.R32ui;
      IMG_DATA_FORMAT_ETC2_R           :Result:=ImageFormat.R8ui;
      IMG_DATA_FORMAT_BC4              :Result:=ImageFormat.R8ui;
      IMG_DATA_FORMAT_1                :Result:=ImageFormat.R8ui;
      IMG_DATA_FORMAT_32_AS_8          :Result:=ImageFormat.R8ui;

      IMG_DATA_FORMAT_8_8              :Result:=ImageFormat.Rg8ui;
      IMG_DATA_FORMAT_16_16            :Result:=ImageFormat.Rg16ui;
      IMG_DATA_FORMAT_32_32            :Result:=ImageFormat.Rg32ui;
      IMG_DATA_FORMAT_8_24             :Result:=ImageFormat.Rg32ui;
      IMG_DATA_FORMAT_24_8             :Result:=ImageFormat.Rg32ui;
      IMG_DATA_FORMAT_X24_8_32         :Result:=ImageFormat.Rg32ui;
      IMG_DATA_FORMAT_ETC2_RG          :Result:=ImageFormat.Rg8ui;
      IMG_DATA_FORMAT_BC5              :Result:=ImageFormat.Rg8ui;
      IMG_DATA_FORMAT_4_4              :Result:=ImageFormat.Rg8ui;
      IMG_DATA_FORMAT_32_AS_8_8        :Result:=ImageFormat.Rg8ui;

      IMG_DATA_FORMAT_10_11_11         :Result:=ImageFormat.Rgb10a2ui;
      IMG_DATA_FORMAT_11_11_10         :Result:=ImageFormat.Rgb10a2ui;
      IMG_DATA_FORMAT_32_32_32         :Result:=ImageFormat.Rgba32ui;
      IMG_DATA_FORMAT_5_6_5            :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_ETC2_RGB         :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_GB_GR            :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_BG_RG            :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_BC1              :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_BC6              :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_6_5_5            :Result:=ImageFormat.Rgba8ui;

      IMG_DATA_FORMAT_10_10_10_2       :Result:=ImageFormat.Rgb10a2ui;
      IMG_DATA_FORMAT_2_10_10_10       :Result:=ImageFormat.Rgb10a2ui;
      IMG_DATA_FORMAT_8_8_8_8          :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_16_16_16_16      :Result:=ImageFormat.Rgba16ui;
      IMG_DATA_FORMAT_32_32_32_32      :Result:=ImageFormat.Rgba32ui;
      IMG_DATA_FORMAT_1_5_5_5          :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_5_5_5_1          :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_4_4_4_4          :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_ETC2_RGBA        :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_ETC2_RGBA1       :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_5_9_9_9          :Result:=ImageFormat.Rgba16ui;
      IMG_DATA_FORMAT_BC2              :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_BC3              :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_BC7              :Result:=ImageFormat.Rgba8ui;
      IMG_DATA_FORMAT_32_AS_32_32_32_32:Result:=ImageFormat.Rgba32ui;

      IMG_DATA_FORMAT_FMASK8_S2_F1     :Result:=ImageFormat.R8ui;
      IMG_DATA_FORMAT_FMASK8_S4_F1     :Result:=ImageFormat.R8ui;
      IMG_DATA_FORMAT_FMASK8_S8_F1     :Result:=ImageFormat.R8ui;
      IMG_DATA_FORMAT_FMASK8_S2_F2     :Result:=ImageFormat.R8ui;
      IMG_DATA_FORMAT_FMASK8_S4_F2     :Result:=ImageFormat.R8ui;
      IMG_DATA_FORMAT_FMASK8_S4_F4     :Result:=ImageFormat.R8ui;

      IMG_DATA_FORMAT_FMASK16_S16_F1   :Result:=ImageFormat.R16ui;
      IMG_DATA_FORMAT_FMASK16_S8_F2    :Result:=ImageFormat.R16ui;

      IMG_DATA_FORMAT_FMASK32_S16_F2   :Result:=ImageFormat.R32ui;
      IMG_DATA_FORMAT_FMASK32_S8_F4    :Result:=ImageFormat.R32ui;
      IMG_DATA_FORMAT_FMASK32_S8_F8    :Result:=ImageFormat.R32ui;

      IMG_DATA_FORMAT_FMASK64_S16_F4   :Result:=ImageFormat.R64ui;
      IMG_DATA_FORMAT_FMASK64_S16_F8   :Result:=ImageFormat.R64ui;

      else;
     end;

    end;

  IMG_NUM_FORMAT_SINT:
    begin //R8i,R16i,R32i, Rg8i,Rg16i,Rg32i, Rgba8i,Rgba16i,Rgba32i

     Case PT^.dfmt of

      IMG_DATA_FORMAT_8                :Result:=ImageFormat.R8i;
      IMG_DATA_FORMAT_16               :Result:=ImageFormat.R16i;
      IMG_DATA_FORMAT_32               :Result:=ImageFormat.R32i;
      IMG_DATA_FORMAT_ETC2_R           :Result:=ImageFormat.R8i;
      IMG_DATA_FORMAT_BC4              :Result:=ImageFormat.R8i;
      IMG_DATA_FORMAT_1                :Result:=ImageFormat.R8i;
      IMG_DATA_FORMAT_32_AS_8          :Result:=ImageFormat.R8i;

      IMG_DATA_FORMAT_8_8              :Result:=ImageFormat.Rg8i;
      IMG_DATA_FORMAT_16_16            :Result:=ImageFormat.Rg16i;
      IMG_DATA_FORMAT_32_32            :Result:=ImageFormat.Rg32i;
      IMG_DATA_FORMAT_8_24             :Result:=ImageFormat.Rg32i;
      IMG_DATA_FORMAT_24_8             :Result:=ImageFormat.Rg32i;
      IMG_DATA_FORMAT_X24_8_32         :Result:=ImageFormat.Rg32i;
      IMG_DATA_FORMAT_ETC2_RG          :Result:=ImageFormat.Rg8i;
      IMG_DATA_FORMAT_BC5              :Result:=ImageFormat.Rg8i;
      IMG_DATA_FORMAT_4_4              :Result:=ImageFormat.Rg8i;
      IMG_DATA_FORMAT_32_AS_8_8        :Result:=ImageFormat.Rg8i;

      IMG_DATA_FORMAT_10_11_11         :Result:=ImageFormat.Rgba16i;
      IMG_DATA_FORMAT_11_11_10         :Result:=ImageFormat.Rgba16i;
      IMG_DATA_FORMAT_32_32_32         :Result:=ImageFormat.Rgba32i;
      IMG_DATA_FORMAT_5_6_5            :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_ETC2_RGB         :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_GB_GR            :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_BG_RG            :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_BC1              :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_BC6              :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_6_5_5            :Result:=ImageFormat.Rgba8i;

      IMG_DATA_FORMAT_10_10_10_2       :Result:=ImageFormat.Rgba16i;
      IMG_DATA_FORMAT_2_10_10_10       :Result:=ImageFormat.Rgba16i;
      IMG_DATA_FORMAT_8_8_8_8          :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_16_16_16_16      :Result:=ImageFormat.Rgba16i;
      IMG_DATA_FORMAT_32_32_32_32      :Result:=ImageFormat.Rgba32i;
      IMG_DATA_FORMAT_1_5_5_5          :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_5_5_5_1          :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_4_4_4_4          :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_ETC2_RGBA        :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_ETC2_RGBA1       :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_5_9_9_9          :Result:=ImageFormat.Rgba16i;
      IMG_DATA_FORMAT_BC2              :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_BC3              :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_BC7              :Result:=ImageFormat.Rgba8i;
      IMG_DATA_FORMAT_32_AS_32_32_32_32:Result:=ImageFormat.Rgba32i;

      IMG_DATA_FORMAT_FMASK8_S2_F1     :Result:=ImageFormat.R8i;
      IMG_DATA_FORMAT_FMASK8_S4_F1     :Result:=ImageFormat.R8i;
      IMG_DATA_FORMAT_FMASK8_S8_F1     :Result:=ImageFormat.R8i;
      IMG_DATA_FORMAT_FMASK8_S2_F2     :Result:=ImageFormat.R8i;
      IMG_DATA_FORMAT_FMASK8_S4_F2     :Result:=ImageFormat.R8i;
      IMG_DATA_FORMAT_FMASK8_S4_F4     :Result:=ImageFormat.R8i;

      IMG_DATA_FORMAT_FMASK16_S16_F1   :Result:=ImageFormat.R16i;
      IMG_DATA_FORMAT_FMASK16_S8_F2    :Result:=ImageFormat.R16i;

      IMG_DATA_FORMAT_FMASK32_S16_F2   :Result:=ImageFormat.R32i;
      IMG_DATA_FORMAT_FMASK32_S8_F4    :Result:=ImageFormat.R32i;
      IMG_DATA_FORMAT_FMASK32_S8_F8    :Result:=ImageFormat.R32i;

      IMG_DATA_FORMAT_FMASK64_S16_F4   :Result:=ImageFormat.R64i;
      IMG_DATA_FORMAT_FMASK64_S16_F8   :Result:=ImageFormat.R64i;

      else;
     end;

    end;

  IMG_NUM_FORMAT_USCALED,
  IMG_NUM_FORMAT_SSCALED,
  IMG_NUM_FORMAT_FLOAT:
    begin //R16f,R32f, Rg16f,Rg32f, R11fG11fB10f, Rgba16f,Rgba32f

     Case PT^.dfmt of

      IMG_DATA_FORMAT_16               :Result:=ImageFormat.R16f;
      IMG_DATA_FORMAT_32               :Result:=ImageFormat.R32f;
      IMG_DATA_FORMAT_ETC2_R           :Result:=ImageFormat.R16f;
      IMG_DATA_FORMAT_BC4              :Result:=ImageFormat.R16f;

      IMG_DATA_FORMAT_16_16            :Result:=ImageFormat.Rg16f;
      IMG_DATA_FORMAT_32_32            :Result:=ImageFormat.Rg32f;
      IMG_DATA_FORMAT_8_24             :Result:=ImageFormat.Rg32f;
      IMG_DATA_FORMAT_24_8             :Result:=ImageFormat.Rg32f;
      IMG_DATA_FORMAT_X24_8_32         :Result:=ImageFormat.Rg32f;
      IMG_DATA_FORMAT_ETC2_RG          :Result:=ImageFormat.Rg16f;
      IMG_DATA_FORMAT_BC5              :Result:=ImageFormat.Rg16f;

      IMG_DATA_FORMAT_10_11_11         :Result:=ImageFormat.R11fG11fB10f;
      IMG_DATA_FORMAT_11_11_10         :Result:=ImageFormat.R11fG11fB10f;
      IMG_DATA_FORMAT_32_32_32         :Result:=ImageFormat.Rgba32f;
      IMG_DATA_FORMAT_ETC2_RGB         :Result:=ImageFormat.Rgba16f;
      IMG_DATA_FORMAT_BC1              :Result:=ImageFormat.Rgba16f;
      IMG_DATA_FORMAT_BC6              :Result:=ImageFormat.Rgba16f;

      IMG_DATA_FORMAT_10_10_10_2       :Result:=ImageFormat.Rgba16f;
      IMG_DATA_FORMAT_2_10_10_10       :Result:=ImageFormat.Rgba16f;
      IMG_DATA_FORMAT_16_16_16_16      :Result:=ImageFormat.Rgba16f;
      IMG_DATA_FORMAT_32_32_32_32      :Result:=ImageFormat.Rgba32f;
      IMG_DATA_FORMAT_ETC2_RGBA        :Result:=ImageFormat.Rgba16f;
      IMG_DATA_FORMAT_ETC2_RGBA1       :Result:=ImageFormat.Rgba16f;
      IMG_DATA_FORMAT_BC2              :Result:=ImageFormat.Rgba16f;
      IMG_DATA_FORMAT_BC3              :Result:=ImageFormat.Rgba16f;
      IMG_DATA_FORMAT_BC7              :Result:=ImageFormat.Rgba16f;
      IMG_DATA_FORMAT_32_AS_32_32_32_32:Result:=ImageFormat.Rgba32f;

      else;
     end;

    end;

  else;
 end;
end;

function GetDimType(PT:PTSharpResource4):Byte;
begin
 Result:=0;
 if (PT=nil) then Exit;
 Case PT^._type of
  SQ_RSRC_IMG_1D,
  SQ_RSRC_IMG_1D_ARRAY     :Result:=Dim.Dim1D;
  SQ_RSRC_IMG_2D,
  SQ_RSRC_IMG_2D_ARRAY,
  SQ_RSRC_IMG_2D_MSAA,
  SQ_RSRC_IMG_2D_MSAA_ARRAY:Result:=Dim.Dim2D;
  SQ_RSRC_IMG_3D           :Result:=Dim.Dim3D;
  SQ_RSRC_IMG_CUBE         :Result:=Dim.Cube;
  else;
 end;
end;

function GetElemType(PT:PTSharpResource4):TsrDataType;
begin
 Result:=dtFloat32;
 if (PT=nil) then Exit;
 Case PT^.nfmt of
  IMG_NUM_FORMAT_UINT:Result:=dtUint32;
  IMG_NUM_FORMAT_SINT:Result:=dtInt32;
  else;
 end;
end;

function GetDimCount(b:Byte):Byte;
begin
 Result:=1;
 Case b of
  Dim.Dim2D:Result:=2;
  Dim.Dim3D:Result:=3;
  Dim.Cube :Result:=3;
  else;
 end;
end;

function GetImageInfo(PT:PTSharpResource4):TsrImageInfo;
begin
 Result:=Default(TsrImageInfo);
 Result.dtype:=GetElemType(PT);

 Result.tinfo.Dim    :=GetDimType(PT);
 Result.tinfo.Depth  :=2;
 Result.tinfo.Arrayed:=GetArrayedType(PT);
 Result.tinfo.MS     :=GetMsType(PT);
 Result.tinfo.Format :=GetImageFormat(PT);
end;

function GetImageMods(OP:Byte):TsrImageMods;
begin
 Case OP of
  IMAGE_SAMPLE_CL       :Result:=[imMinLod];
  IMAGE_SAMPLE_D        :Result:=[imGrad];
  IMAGE_SAMPLE_D_CL     :Result:=[imGrad,imMinLod];
  IMAGE_SAMPLE_L        :Result:=[imLod];
  IMAGE_SAMPLE_B        :Result:=[imBiasLod];
  IMAGE_SAMPLE_B_CL     :Result:=[imBiasLod,imMinLod];
  IMAGE_SAMPLE_LZ       :Result:=[imZeroLod];
  IMAGE_SAMPLE_C        :Result:=[imDref];
  IMAGE_SAMPLE_C_CL     :Result:=[imDref,imMinLod];
  IMAGE_SAMPLE_C_D      :Result:=[imDref,imGrad];
  IMAGE_SAMPLE_C_D_CL   :Result:=[imDref,imGrad,imMinLod];
  IMAGE_SAMPLE_C_L      :Result:=[imDref,imLod];
  IMAGE_SAMPLE_C_B      :Result:=[imDref,imBiasLod];
  IMAGE_SAMPLE_C_B_CL   :Result:=[imDref,imBiasLod,imMinLod];
  IMAGE_SAMPLE_C_LZ     :Result:=[imDref,imZeroLod];
  IMAGE_SAMPLE_O        :Result:=[imOffset];
  IMAGE_SAMPLE_CL_O     :Result:=[imMinLod,imOffset];
  IMAGE_SAMPLE_D_O      :Result:=[imGrad,imOffset];
  IMAGE_SAMPLE_D_CL_O   :Result:=[imGrad,imMinLod,imOffset];
  IMAGE_SAMPLE_L_O      :Result:=[imLod,imOffset];
  IMAGE_SAMPLE_B_O      :Result:=[imBiasLod,imOffset];
  IMAGE_SAMPLE_B_CL_O   :Result:=[imBiasLod,imMinLod,imOffset];
  IMAGE_SAMPLE_LZ_O     :Result:=[imZeroLod,imOffset];
  IMAGE_SAMPLE_C_O      :Result:=[imDref,imOffset];
  IMAGE_SAMPLE_C_CL_O   :Result:=[imDref,imMinLod,imOffset];
  IMAGE_SAMPLE_C_D_O    :Result:=[imDref,imGrad,imOffset];
  IMAGE_SAMPLE_C_D_CL_O :Result:=[imDref,imGrad,imMinLod,imOffset];
  IMAGE_SAMPLE_C_L_O    :Result:=[imDref,imLod,imOffset];
  IMAGE_SAMPLE_C_B_O    :Result:=[imDref,imBiasLod,imOffset];
  IMAGE_SAMPLE_C_B_CL_O :Result:=[imDref,imBiasLod,imMinLod,imOffset];
  IMAGE_SAMPLE_C_LZ_O   :Result:=[imDref,imZeroLod,imOffset];
  //
  IMAGE_GATHER4_CL      :Result:=[imMinLod];
  IMAGE_GATHER4_L       :Result:=[imLod];
  IMAGE_GATHER4_B       :Result:=[imBiasLod];
  IMAGE_GATHER4_B_CL    :Result:=[imBiasLod,imMinLod];
  IMAGE_GATHER4_LZ      :Result:=[imZeroLod];
  IMAGE_GATHER4_C       :Result:=[imDref];
  IMAGE_GATHER4_C_CL    :Result:=[imDref,imMinLod];
  IMAGE_GATHER4_C_L     :Result:=[imDref,imLod];
  IMAGE_GATHER4_C_B     :Result:=[imDref,imBiasLod];
  IMAGE_GATHER4_C_B_CL  :Result:=[imDref,imBiasLod,imMinLod];
  IMAGE_GATHER4_C_LZ    :Result:=[imDref,imMinLod];
  IMAGE_GATHER4_O       :Result:=[imOffset];
  IMAGE_GATHER4_CL_O    :Result:=[imMinLod,imOffset];
  IMAGE_GATHER4_L_O     :Result:=[imLod,imOffset];
  IMAGE_GATHER4_B_O     :Result:=[imBiasLod,imOffset];
  IMAGE_GATHER4_B_CL_O  :Result:=[imBiasLod,imMinLod,imOffset];
  IMAGE_GATHER4_LZ_O    :Result:=[imZeroLod,imOffset];
  IMAGE_GATHER4_C_O     :Result:=[imDref,imOffset];
  IMAGE_GATHER4_C_CL_O  :Result:=[imDref,imMinLod,imOffset];
  IMAGE_GATHER4_C_L_O   :Result:=[imDref,imLod,imOffset];
  IMAGE_GATHER4_C_B_O   :Result:=[imDref,imBiasLod,imOffset];
  IMAGE_GATHER4_C_B_CL_O:Result:=[imDref,imBiasLod,imMinLod,imOffset];
  IMAGE_GATHER4_C_LZ_O  :Result:=[imDref,imZeroLod,imOffset];
  //
  IMAGE_SAMPLE_CD       :Result:=[imGrad];
  IMAGE_SAMPLE_CD_CL    :Result:=[imGrad,imMinLod];
  IMAGE_SAMPLE_C_CD     :Result:=[imDref,imGrad];
  IMAGE_SAMPLE_C_CD_CL  :Result:=[imDref,imGrad,imMinLod];
  IMAGE_SAMPLE_CD_O     :Result:=[imGrad,imOffset];
  IMAGE_SAMPLE_CD_CL_O  :Result:=[imGrad,imMinLod,imOffset];
  IMAGE_SAMPLE_C_CD_O   :Result:=[imDref,imGrad,imOffset];
  IMAGE_SAMPLE_C_CD_CL_O:Result:=[imDref,imGrad,imMinLod,imOffset];
  else
    Result:=[];
 end;
end;

function GetImgOpStr(i:Integer):RawByteString;

 procedure t(o:Integer;const v:RawByteString); inline;
 begin
  if ((o and i)<>0) then
  begin
   if (Result<>'') then
   begin
    Result:=Result+'|'+v;
   end else
   begin
    Result:=v;
   end;
  end;
 end;

begin
 Result:='';
 t(ImageOperands.Bias        ,'Bias');
 t(ImageOperands.Lod         ,'Lod');
 t(ImageOperands.Grad        ,'Grad');
 t(ImageOperands.ConstOffset ,'ConstOffset');
 t(ImageOperands.Sample      ,'Sample');
 t(ImageOperands.MinLod      ,'MinLod');
end;

procedure TEmit_MIMG.DistribDmask(DMASK:Byte;dst:PsrRegNode;info:PsrImageInfo); //result
var
 pSlot:PsrRegSlot;
 dtype:TsrDataType;
 i,d,max:Byte;
begin
 dtype:=dst^.dtype.Child;
 max  :=dst^.dtype.Count;

 d:=0;
 For i:=0 to 3 do
  if DMASK.TestBit(i) then
  begin
   pSlot:=get_vdst8(FSPI.MIMG.VDATA+d);
   Inc(d);
   Assert(pSlot<>nil);

   if (dst^.dtype.isScalar) then
   begin
    if (i=0) then
    begin
     MakeCopy(pSlot,dst);
    end else
    begin
     SetConst_i(pSlot,dtype,0);
    end;
   end else
   begin
    if (i<max) then
    begin
     OpExtract(line,pSlot^.New(line,dtype),dst,i);
    end else
    begin
     SetConst_i(pSlot,dtype,0);
    end;
   end;

  end;
end;

function TEmit_MIMG.GatherDmask(telem:TsrDataType):PsrRegNode;
var
 src:array[0..3] of PsrRegNode;
 i,d,m:Byte;
begin
 d:=0;
 For i:=0 to 3 do
 if Byte(FSPI.MIMG.DMASK).TestBit(i) then
 begin
  src[i]:=fetch_vsrc8(FSPI.MIMG.VDATA+d,telem);
  Inc(d);
  m:=i;
 end else
 begin
  src[i]:=nil;
 end;

 //Result:=telem.AsVector(4);
 For i:=0 to m do
 begin
  Assert(src[i]<>nil,'TODO: zero or prev value?');
 end;

 if (m=0) then
 begin
  Result:=src[0];
 end else
 begin
  Result:=OpMakeVec(line,telem.AsVector(m+1),@src);
 end;
end;

Function TEmit_MIMG.GatherCoord_f(var offset:DWORD;info:PsrImageInfo):PsrRegNode; //src
var
 src:array[0..3] of PsrRegNode;
 i,count:Byte;
begin
 Result:=nil;

 count:=GetDimCount(info^.tinfo.Dim);
 if (FSPI.MIMG.DA<>0) then Inc(count); //slice

 if (info^.tinfo.Dim=Dim.Cube) then
 begin
  //x,y,slice,(face_id+slice*8)

  if (FSPI.MIMG.DA<>0) then //slice
  begin
   src[0]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+0,dtFloat32); //x
   src[1]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+1,dtFloat32); //y
   src[2]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+3,dtFloat32); //face TODO: face-slice*8
   src[3]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+2,dtFloat32); //slice
  end else
  begin
   src[0]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+0,dtFloat32); //x
   src[1]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+1,dtFloat32); //y
   src[2]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+2,dtFloat32); //face

   if (info^.tinfo.Arrayed<>0) then //is array
   begin
    Inc(count);
    src[3]:=NewReg_s(dtFloat32,0);
   end;
  end;

  Result:=OpMakeCub(line,TsrDataType(dtFloat32).AsVector(count),@src);

 end else
 begin

  For i:=0 to count-1 do
  begin
   src[i]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+i,dtFloat32);
  end;

  if (info^.tinfo.Arrayed<>0) and //is array
     (FSPI.MIMG.DA=0) then        //not slice
  begin
   i:=count;
   Inc(count);

   src[i]:=NewReg_s(dtFloat32,0);
  end;

  if (count=1) then
  begin
   Result:=src[0];
  end else
  begin
   Result:=OpMakeVec(line,TsrDataType(dtFloat32).AsVector(count),@src);
  end;

 end;

 offset:=offset+count;
end;

Function TEmit_MIMG.GatherCoord_u(var offset:DWORD;info:PsrImageInfo):PsrRegNode; //src
var
 src:array[0..3] of PsrRegNode;
 i,count:Byte;
begin
 Result:=nil;

 count:=GetDimCount(info^.tinfo.Dim);
 if (FSPI.MIMG.DA<>0) then Inc(count); //slice

 if (info^.tinfo.Dim=Dim.Cube) then
 begin
  //x,y,slice,(face_id+slice*8)

  if (FSPI.MIMG.DA<>0) then //slice
  begin
   src[0]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+0,dtInt32); //x
   src[1]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+1,dtInt32); //y
   src[2]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+3,dtInt32); //face TODO: face-slice*8
   src[3]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+2,dtInt32); //slice
  end else
  begin
   src[0]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+0,dtInt32); //x
   src[1]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+1,dtInt32); //y
   src[2]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+2,dtInt32); //face

   if (info^.tinfo.Arrayed<>0) then //is array
   begin
    Inc(count);
    src[3]:=NewReg_i(dtInt32,0);
   end;
  end;

  Result:=OpMakeCub(line,TsrDataType(dtInt32).AsVector(count),@src);

 end else
 begin

  For i:=0 to count-1 do
  begin
   src[i]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+i,dtInt32);
  end;

  if (info^.tinfo.Arrayed<>0) and //is array
     (FSPI.MIMG.DA=0) then        //not slice
  begin
   i:=count;
   Inc(count);

   src[i]:=NewReg_i(dtInt32,0);
  end;

  if (count=1) then
  begin
   Result:=src[0];
  end else
  begin
   Result:=OpMakeVec(line,TsrDataType(dtInt32).AsVector(count),@src);
  end;

 end;

 offset:=offset+count;
end;

Function TEmit_MIMG.Gather_value(var offset:DWORD;rtype:TsrDataType):PsrRegNode;
begin
 Result:=fetch_vsrc8(FSPI.MIMG.VADDR+offset,rtype);
 Inc(offset);
end;

Function TEmit_MIMG.Gather_packed_offset(var offset:DWORD;dim:Byte):PsrRegNode;
var
 src:PsrRegNode;
begin
 Result:=nil;

 src:=fetch_vsrc8(FSPI.MIMG.VADDR+offset,dtUint32);
 Inc(offset);

 Case dim of
  1:Result:=OpPackOfs(line,dtInt32,dim,src);
  2:Result:=OpPackOfs(line,dtVec2i,dim,src);
  3:Result:=OpPackOfs(line,dtVec3i,dim,src);
  else
   Assert(False);
 end;

end;

procedure TEmit_MIMG.Gather_sample_param(var p:TImgSampleParam;info:PsrImageInfo);
begin
 p.roffset:=0;
 p.mods:=GetImageMods(FSPI.MIMG.OP);
 p.img_op:=0;

 //gather
 if (imOffset in p.mods) then
 begin
  p.offset:=Gather_packed_offset(p.roffset,GetDimCount(info^.tinfo.Dim));
  p.img_op:=p.img_op or ImageOperands.ConstOffset;
 end;

 if (imBiasLod in p.mods) then
 begin
  p.bias:=Gather_value(p.roffset,dtFloat32);
  p.img_op:=p.img_op or ImageOperands.Bias;
 end;

 if (imDref in p.mods) then
 begin
  p.pcf:=Gather_value(p.roffset,dtFloat32);
 end;

 if (imGrad in p.mods) then
 begin
  Assert(false,'TODO imGrad');
 end;

 p.coord:=GatherCoord_f(p.roffset,info);

 if (imLod in p.mods) then
 begin
  p.lod:=Gather_value(p.roffset,dtFloat32);
  p.img_op:=p.img_op or ImageOperands.Lod;
 end else
 if (imZeroLod in p.mods) then
 begin
  p.lod:=NewReg_s(dtFloat32,0);
  p.img_op:=p.img_op or ImageOperands.Lod;
 end;

 if (imMinLod in p.mods) then
 begin
  p.min_lod:=Gather_value(p.roffset,dtFloat32);
  p.img_op:=p.img_op or ImageOperands.MinLod;
 end;
 //gather
end;

procedure TEmit_MIMG.add_sample_op(var p:TImgSampleParam;node:PSpirvOp);
begin
 //ImageOperands
 if (p.img_op<>0) then
 begin
  node^.AddLiteral(p.img_op,GetImgOpStr(p.img_op));

  if ((p.img_op and ImageOperands.Bias)<>0) then
  begin
   node^.AddParam(p.bias);
  end;

  if ((p.img_op and ImageOperands.Lod)<>0) then
  begin
   node^.AddParam(p.lod);
  end;

  if ((p.img_op and ImageOperands.Grad)<>0) then
  begin
   Assert(false,'TODO imGrad');
  end;

  if ((p.img_op and ImageOperands.ConstOffset)<>0) then
  begin
   node^.AddParam(p.offset);
  end;

  if ((p.img_op and ImageOperands.MinLod)<>0) then
  begin
   node^.AddParam(p.min_lod);
   AddCapability(Capability.MinLod);
  end;

 end;
 //ImageOperands
end;

procedure TEmit_MIMG.emit_image_sample(Tgrp:PsrNode;info:PsrImageInfo);
var
 src:array[0..3] of PsrRegSlot;

 pLayout:PsrDataLayout;
 Sgrp:PsrNode;

 dst,cmb:PsrRegNode;

 param:TImgSampleParam;

 node:PSpirvOp;
begin
 if not get_srsrc(FSPI.MIMG.SSAMP,4,@src) then Assert(false);

 pLayout:=GroupingSharp(src,rtSSharp4);
 Sgrp:=FetchSampler(pLayout);

 cmb:=OpSampledImage(line,Tgrp,Sgrp,info^.dtype,info^.tinfo);

 //gather
 param:=Default(TImgSampleParam);
 Gather_sample_param(param,info);
 //gather

 if (imDref in param.mods) then
 begin
  dst:=NewReg(info^.dtype);
 end else
 begin
  dst:=NewReg(info^.dtype.AsVector(4));
 end;

 //OpImage
 if (imDref in param.mods) then
 begin
  if (FExecutionModel=ExecutionModel.Fragment) and
     ((param.img_op and ImageOperands.Lod)=0) then
  begin
   //scalar
   node:=OpImageSampleDrefImplicitLod(line,cmb,dst,param.coord,param.pcf);
  end else
  begin
   //scalar
   node:=OpImageSampleDrefExplicitLod(line,cmb,dst,param.coord,param.pcf);
  end;
 end else
 begin
  if (FExecutionModel=ExecutionModel.Fragment) and
     ((param.img_op and ImageOperands.Lod)=0) then
  begin
   //vector
   node:=OpImageSampleImplicitLod(line,cmb,dst,param.coord);
  end else
  begin
   //vector
   node:=OpImageSampleExplicitLod(line,cmb,dst,param.coord);
  end;
 end;
 //OpImage

 //ImageOperands
 add_sample_op(param,node);
 //ImageOperands

 DistribDmask(FSPI.MIMG.DMASK,dst,info);
end;

procedure TEmit_MIMG.emit_image_sample_gather(Tgrp:PsrNode;info:PsrImageInfo);
var
 src:array[0..3] of PsrRegSlot;

 pLayout:PsrDataLayout;
 Sgrp:PsrNode;

 dst,cmb:PsrRegNode;

 param:TImgSampleParam;
 id:Byte;

 node:PSpirvOp;
begin
 if not get_srsrc(FSPI.MIMG.SSAMP,4,@src) then Assert(false);

 pLayout:=GroupingSharp(src,rtSSharp4);
 Sgrp:=FetchSampler(pLayout);

 cmb:=OpSampledImage(line,Tgrp,Sgrp,info^.dtype,info^.tinfo);

 dst:=NewReg(info^.dtype.AsVector(4));

 //gather
 param:=Default(TImgSampleParam);
 Gather_sample_param(param,info);
 //gather

 id:=BsrByte(FSPI.MIMG.DMASK);
 //OpImage
 if (imDref in param.mods) then
 begin
  Assert(id=0,'gather pcf with non red component');
  //vector
  node:=OpImageDrefGather(line,cmb,dst,param.coord,param.pcf);
 end else
 begin
  //vector
  node:=OpImageGather(line,cmb,dst,param.coord,id);
 end;
 //OpImage

 //OpExtension "SPV_AMD_texture_gather_bias_lod"
 //Capability.ImageGatherBiasLodAMD
 //VK_AMD_texture_gather_bias_lod
 begin
  param.img_op:=param.img_op and (not (ImageOperands.Bias or ImageOperands.Lod))
 end;

 //ImageOperands
 add_sample_op(param,node);
 //ImageOperands

 DistribDmask(15,dst,info);
end;

procedure TEmit_MIMG.emit_image_load(Tgrp:PsrNode;info:PsrImageInfo);
var
 dst,coord,lod,smp:PsrRegNode;

 roffset:DWORD;

 node:PSpirvOp;
begin

 dst:=NewReg(info^.dtype.AsVector(4));

 roffset:=0;

 Case FSPI.MIMG.OP of
  IMAGE_LOAD:
    begin
     coord:=GatherCoord_u(roffset,info);

     //vector
     node:=OpImageFetch(line,Tgrp,dst,coord);

     if (info^.tinfo.MS<>0) then //fragid T# 2D MSAA
     begin
      smp:=Gather_value(roffset,dtUint32);

      node^.AddLiteral(ImageOperands.Sample,'Sample');
      node^.AddParam(smp);
     end;
    end;
  IMAGE_LOAD_MIP: //All except MSAA
    begin
     coord:=GatherCoord_u(roffset,info);

     //vector
     node:=OpImageFetch(line,Tgrp,dst,coord);

     lod:=Gather_value(roffset,dtUint32);

     node^.AddLiteral(ImageOperands.Lod,'Lod');
     node^.AddParam(lod);
    end;
  else
    Assert(false,'MIMG?'+IntToStr(FSPI.MIMG.OP));
 end;

 DistribDmask(FSPI.MIMG.DMASK,dst,info);
end;

procedure TEmit_MIMG.emit_image_store(Tgrp:PsrNode;info:PsrImageInfo);
var
 dst,coord,lod,smp:PsrRegNode;

 roffset:DWORD;

 node:PSpirvOp;
begin
 dst:=GatherDmask(info^.dtype);

 roffset:=0;

 Case FSPI.MIMG.OP of
  IMAGE_STORE:
    begin
     coord:=GatherCoord_u(roffset,info);

     //scalar or vector
     node:=OpImageWrite(line,Tgrp,coord,dst);

     if (info^.tinfo.MS<>0) then //fragid T# 2D MSAA
     begin
      smp:=Gather_value(roffset,dtUint32);

      node^.AddLiteral(ImageOperands.Sample,'Sample');
      node^.AddParam(smp);
     end;
    end;
  IMAGE_STORE_MIP: //All except MSAA
    begin
     coord:=GatherCoord_u(roffset,info);

     //scalar or vector
     node:=OpImageWrite(line,Tgrp,coord,dst);

     lod:=Gather_value(roffset,dtUint32);

     node^.AddLiteral(ImageOperands.Lod,'Lod');
     node^.AddParam(lod);
    end;
  else
    Assert(false,'MIMG?'+IntToStr(FSPI.MIMG.OP));
 end;

end;

procedure TEmit_MIMG.emit_image_get_resinfo(Tgrp:PsrNode;info:PsrImageInfo);
var
 offset:DWORD;
 dst,lod:PsrRegNode;

 dvec:TsrDataType;
 count:Byte;
begin
 offset:=0;
 lod:=Gather_value(offset,dtUint32);

 count:=1;
 Case info^.tinfo.Dim of
  Dim.Dim2D:count:=2;
  Dim.Cube :count:=2;
  Dim.Dim3D:count:=3;
  else;
 end;
 if (info^.tinfo.Arrayed<>0) then Inc(count);

 dvec:=TsrDataType(dtUint32).AsVector(count);

 dst:=NewReg(dvec);

 _Op2(line,Op.OpImageQuerySizeLod,dst,PsrRegNode(Tgrp),lod);

 DistribDmask(FSPI.MIMG.DMASK,dst,info);

 AddCapability(Capability.ImageQuery);
end;

procedure TEmit_MIMG.emit_image_get_lod(Tgrp:PsrNode;info:PsrImageInfo);
var
 src:array[0..3] of PsrRegSlot;

 pLayout:PsrDataLayout;
 Sgrp:PsrNode;

 dst,cmb:PsrRegNode;

 param:TImgSampleParam;
begin
 if not get_srsrc(FSPI.MIMG.SSAMP,4,@src) then Assert(false);

 pLayout:=GroupingSharp(src,rtSSharp4);
 Sgrp:=FetchSampler(pLayout);

 cmb:=OpSampledImage(line,Tgrp,Sgrp,info^.dtype,info^.tinfo);

 //DMASK: {lod, lod_unclamped, 0, 0}

 //gather
 param:=Default(TImgSampleParam);
 param.coord:=GatherCoord_f(param.roffset,info);
 //gather

 dst:=NewReg(dtVec2f);

 _Op2(line,Op.OpImageQueryLod,dst,PsrRegNode(cmb),param.coord);

 DistribDmask(FSPI.MIMG.DMASK,dst,info);

 AddCapability(Capability.ImageQuery);
end;

procedure TEmit_MIMG.emit_MIMG;
var
 src:array[0..7] of PsrRegSlot;

 pLayout:PsrDataLayout;
 info:TsrImageInfo;

 Tgrp:PsrNode;

begin
 pLayout:=nil;

 Case FSPI.MIMG.R128 of
  0: //256=8 *4*8
    begin
     if not get_srsrc(FSPI.MIMG.SRSRC,8,@src) then Assert(false);
     pLayout:=GroupingSharp(src,rtTSharp8);
    end;
  1: //128=4 *4*8
    begin
     if not get_srsrc(FSPI.MIMG.SRSRC,4,@src) then Assert(false);
     pLayout:=GroupingSharp(src,rtTSharp4);
    end;
 end;

 info:=GetImageInfo(pLayout^.pData);

 if (imDref in GetImageMods(FSPI.MIMG.OP)) then
 begin
  info.tinfo.Depth:=1;
 end;

 Case FSPI.MIMG.OP of
  IMAGE_SAMPLE..IMAGE_SAMPLE_C_LZ_O:  //sampled
    begin
     Assert(FSPI.MIMG.UNRM=0,'FSPI.MIMG.UNRM');

     info.tinfo.Sampled:=1;
     Tgrp:=FetchImage(pLayout,info.dtype,info.tinfo);

     emit_image_sample(Tgrp,@info);
    end;

  IMAGE_GATHER4..IMAGE_GATHER4_C_LZ_O: //sampled gather
    begin
     Assert(FSPI.MIMG.UNRM=0,'FSPI.MIMG.UNRM');

     info.tinfo.Sampled:=1;
     Tgrp:=FetchImage(pLayout,info.dtype,info.tinfo);

     emit_image_sample_gather(Tgrp,@info);
    end;

  IMAGE_LOAD..IMAGE_LOAD_MIP_PCK_SGN: //loaded
    begin
     info.tinfo.Sampled:=1;
     Tgrp:=FetchImage(pLayout,info.dtype,info.tinfo);

     emit_image_load(Tgrp,@info);
    end;

  IMAGE_STORE..IMAGE_STORE_MIP_PCK: //stored
    begin
     info.tinfo.Sampled:=2;
     Tgrp:=FetchImage(pLayout,info.dtype,info.tinfo);

     emit_image_store(Tgrp,@info);
    end;

  IMAGE_GET_RESINFO: //get info by mip
    begin
     info.tinfo.Sampled:=1;
     Tgrp:=FetchImage(pLayout,info.dtype,info.tinfo);

     emit_image_get_resinfo(Tgrp,@info);
    end;

  IMAGE_GET_LOD:
    begin
     info.tinfo.Sampled:=1;
     Tgrp:=FetchImage(pLayout,info.dtype,info.tinfo);

     emit_image_get_lod(Tgrp,@info);
    end;

  else
    Assert(false,'MIMG?'+IntToStr(FSPI.MIMG.OP));
 end;

 //Writeln('DMASK=',FSPI.MIMG.DMASK);
 //Writeln('VADDR=',FSPI.MIMG.VADDR);
 //Writeln('VDATA=',FSPI.MIMG.VDATA);
 //Writeln('SRSRC=',FSPI.MIMG.SRSRC); //T#
 //Writeln('SSAMP=',FSPI.MIMG.SSAMP); //S#
 //writeln;
end;

end.




