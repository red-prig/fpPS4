unit emit_MIMG;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  spirv,
  si_ci_vi_merged_enum,
  ps4_shader,
  ps4_pssl,
  srNodes,
  srTypes,
  srConst,
  srReg,
  srLayout,
  srOp,
  SprvEmit,
  emit_op;

type
 TEmit_MIMG=object(TEmitOp)
  procedure _emit_MIMG;
  procedure _DistribDmask(dst:PsrRegNode;telem:TsrDataType);
  Function  _GatherCoord_f(var offset:DWORD;dim_id:Byte):PsrRegNode;
  Function  _GatherCoord_u(var offset:DWORD;dim_id:Byte):PsrRegNode;
  Function  _Gather_packed_offset(var offset:DWORD;dim:Byte):PsrRegNode;
  procedure _emit_image_sample(Tgrp:PsrRegNode;info:PsrImageInfo);
  procedure _emit_image_load(Tgrp:PsrRegNode;info:PsrImageInfo);
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

procedure TEmit_MIMG._DistribDmask(dst:PsrRegNode;telem:TsrDataType); //result
var
 pSlot:PsrRegSlot;
 i,d:Byte;
begin
 d:=0;
 For i:=0 to 3 do
  if Byte(FSPI.MIMG.DMASK).TestBit(i) then
  begin
   pSlot:=FRegsStory.get_vdst8(FSPI.MIMG.VDATA+d); Inc(d);
   Assert(pSlot<>nil);
   dst^.mark_read;
   emit_OpCompExtract(line,pSlot^.New(line,telem),dst,i);
  end;
end;

Function TEmit_MIMG._GatherCoord_f(var offset:DWORD;dim_id:Byte):PsrRegNode; //src
var
 src:array[0..3] of PsrRegNode;
 i,count:Byte;
begin
 Result:=nil;

 count:=GetDimCount(dim_id);
 if (FSPI.MIMG.DA<>0) then Inc(count); //slice

 if (dim_id=Dim.Cube) then
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
  end;

  Result:=emit_OpMakeCube(line,GetVecType(dtFloat32,count),count,@src);
  Result^.mark_read;

 end else
 begin

  For i:=0 to count-1 do
  begin
   src[i]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+i,dtFloat32);
  end;

  if (count=1) then
  begin
   Result:=src[0];
  end else
  begin
   Result:=emit_OpMakeVec(line,GetVecType(dtFloat32,count),count,@src);
   Result^.mark_read;
  end;

 end;

 offset:=offset+count;
end;

Function TEmit_MIMG._GatherCoord_u(var offset:DWORD;dim_id:Byte):PsrRegNode; //src
var
 src:array[0..3] of PsrRegNode;
 i,count:Byte;
begin
 Result:=nil;

 count:=GetDimCount(dim_id);
 if (FSPI.MIMG.DA<>0) then Inc(count); //slice

 if (dim_id=Dim.Cube) then
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
  end;

  Result:=emit_OpMakeCube(line,GetVecType(dtInt32,count),count,@src);
  Result^.mark_read;

 end else
 begin

  For i:=0 to count-1 do
  begin
   src[i]:=fetch_vsrc8(FSPI.MIMG.VADDR+offset+i,dtInt32);
  end;

  if (count=1) then
  begin
   Result:=src[0];
  end else
  begin
   Result:=emit_OpMakeVec(line,GetVecType(dtInt32,count),count,@src);
   Result^.mark_read;
  end;

 end;

 offset:=offset+count;
end;

Function TEmit_MIMG._Gather_packed_offset(var offset:DWORD;dim:Byte):PsrRegNode;
var
 src:PsrRegNode;
begin
 Result:=nil;

 src:=fetch_vsrc8(FSPI.MIMG.VADDR+offset,dtUint32);
 Inc(offset);

 Case dim of
  1:Result:=emit_OpPackOfs(line,dtInt32,dim,src);
  2:Result:=emit_OpPackOfs(line,dtVec2i,dim,src);
  3:Result:=emit_OpPackOfs(line,dtVec3i,dim,src);
  else
   Assert(False);
 end;

 Result^.mark_read;
end;

procedure TEmit_MIMG._emit_image_sample(Tgrp:PsrRegNode;info:PsrImageInfo);
var
 src:array[0..3] of PsrRegSlot;

 pLayout:PsrDataLayout;
 Sgrp:PsrRegNode;

 dst,cmb,coord,lod,offset:PsrRegNode;

 roffset:DWORD;

 node:PSpirvOp;

begin
 if not FRegsStory.get_srsrc(FSPI.MIMG.SSAMP,4,@src) then Assert(false);

 pLayout:=GroupingSharp(src,rtSSharp4);
 Sgrp:=FetchSampler(pLayout);

 cmb:=emit_OpSampledImage(line,Tgrp,Sgrp,info^.dtype,info^.tinfo);
 cmb^.mark_read;

 dst:=NewReg(GetVecType(info^.dtype,4));

 roffset:=0;

 Case FSPI.MIMG.OP of
  IMAGE_SAMPLE:
    begin
     coord:=_GatherCoord_f(roffset,info^.tinfo.Dim);

     if (FExecutionModel=ExecutionModel.Fragment) then
     begin
      node:=emit_OpImageSampleImplicitLod(line,dst,cmb,coord);
     end else
     begin
      node:=emit_OpImageSampleExplicitLod(line,dst,cmb,coord);
     end;

    end;

  IMAGE_SAMPLE_LZ:
    begin
     coord:=_GatherCoord_f(roffset,info^.tinfo.Dim);

     node:=emit_OpImageSampleExplicitLod(line,dst,cmb,coord);

     node^.AddLiteral(ImageOperands.Lod,'Lod');

     //0
     lod:=FetchReg(FConsts.Fetchf(dtFloat32,0));
     node^.AddParam(ntReg,lod);
    end;

  IMAGE_SAMPLE_LZ_O:
    begin
     offset:=_Gather_packed_offset(roffset,GetDimCount(info^.tinfo.Dim));

     coord:=_GatherCoord_f(roffset,info^.tinfo.Dim);

     node:=emit_OpImageSampleExplicitLod(line,dst,cmb,coord);

     node^.AddLiteral(ImageOperands.Lod or ImageOperands.ConstOffset,'Lod|ConstOffset');

     //0
     lod:=FetchReg(FConsts.Fetchf(dtFloat32,0));
     node^.AddParam(ntReg,lod);

     //1
     node^.AddParam(ntReg,offset);
    end;

  else
    Assert(false);
 end;

 _DistribDmask(dst,info^.dtype);
end;

procedure TEmit_MIMG._emit_image_load(Tgrp:PsrRegNode;info:PsrImageInfo);
var
 dst,coord,lod,smp:PsrRegNode;

 roffset:DWORD;

 node:PSpirvOp;
begin

 dst:=NewReg(GetVecType(info^.dtype,4));

 roffset:=0;

 Case FSPI.MIMG.OP of
  IMAGE_LOAD:
    begin
     coord:=_GatherCoord_u(roffset,info^.tinfo.Dim);
     node:=emit_OpImageFetch(line,Tgrp,dst,coord);

     if (info^.tinfo.MS<>0) then //fragid T# 2D MSAA
     begin
      smp:=fetch_vsrc8(FSPI.MIMG.VADDR+roffset,dtUint32);
      Inc(roffset);

      node^.AddLiteral(ImageOperands.Sample,'Sample');
      node^.AddParam(ntReg,smp);
     end;
    end;
  IMAGE_LOAD_MIP: //All except MSAA
    begin
     coord:=_GatherCoord_u(roffset,info^.tinfo.Dim);
     node:=emit_OpImageFetch(line,Tgrp,dst,coord);

     lod:=fetch_vsrc8(FSPI.MIMG.VADDR+roffset,dtUint32);
     Inc(roffset);

     node^.AddLiteral(ImageOperands.Lod,'Lod');
     node^.AddParam(ntReg,lod);
    end;
  else
    Assert(false);
 end;

 _DistribDmask(dst,info^.dtype);
end;

procedure TEmit_MIMG._emit_MIMG;
var
 src:array[0..7] of PsrRegSlot;

 pLayout:PsrDataLayout;
 info:TsrImageInfo;

 Tgrp:PsrRegNode;

begin

 Assert(FSPI.MIMG.UNRM=0,'FSPI.MIMG.UNRM');

 pLayout:=nil;

 Case FSPI.MIMG.R128 of
  0:
    begin
     if not FRegsStory.get_srsrc(FSPI.MIMG.SRSRC,8,@src) then Assert(false);
     pLayout:=GroupingSharp(src,rtTSharp4);
     //print_tsharp4(pLayout^.pData);
     //Tgrp:=GroupingSharp(@src,rtTSharp8);
    end;
  1:
    begin
     if not FRegsStory.get_srsrc(FSPI.MIMG.SRSRC,4,@src) then Assert(false);
     pLayout:=GroupingSharp(src,rtTSharp8);
     //print_tsharp8(pLayout^.pData);
     //Tgrp:=GroupingSharp(@src,rtTSharp4);
    end;
 end;

 info:=GetImageInfo(pLayout^.pData);

 Case FSPI.MIMG.OP of
  IMAGE_SAMPLE..IMAGE_SAMPLE_C_LZ_O:  //sampled
    begin
     info.tinfo.Sampled:=1;
     Tgrp:=FetchImage(pLayout,info.dtype,info.tinfo);

     _emit_image_sample(Tgrp,@info);
    end;

  IMAGE_LOAD..IMAGE_LOAD_MIP_PCK_SGN: //loaded
    begin
     info.tinfo.Sampled:=1;
     Tgrp:=FetchImage(pLayout,info.dtype,info.tinfo);

     _emit_image_load(Tgrp,@info);
    end

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




