unit srVBufInfo;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_pssl,
  srType,
  srLayout,
  spirv;

type
 //Destination channel select:
 //0=0, 1=1, 4=R, 5=G, 6=B, 7=A
 Tdst_sel=array[0..3] of Byte;

 TBuf_info=packed object
  grp:PsrDataLayout;
  dsel:Tdst_sel;
  DFMT:Byte;  //BUF_DATA_FORMAT_*
  NFMT:Byte;  //BUF_NUM_FORMAT_*
  count:Byte; //count reg dst
  function GetResultType:TsrDataType;
  function GetElemType:TsrDataType;
  function GetElemCount:Byte; inline;
  function GetElemSize:Byte; inline;
  function GetSizeFormat:Byte; inline;
  function GetAlignSize:Byte; inline;
  function IsComp:Boolean;
  function IsExtFormat:Boolean;
  function GetImageFormat:Byte;
  function GetImageFormatElement:Byte;
  function GetImageInfo:TsrImageInfo;
  function GetImageInfoElement:TsrImageInfo;
 end;

const
 dst_sel_identity:Tdst_sel=(4,5,6,7);

function Buf_info(grp:PsrDataLayout;dsel:Tdst_sel;DFMT,NFMT,count:Byte):TBuf_info; inline;
function dst_sel(r,g,b,a:Byte):Tdst_sel; inline;
function get_reverse_dst_sel(dst:Tdst_sel):Tdst_sel;
function GetElemCount(DFMT:Byte):Byte; inline;

implementation

const
 DFMT_ELEM_COUNT:array[0..15] of Byte=(
  0,  //BUF_DATA_FORMAT_INVALID
  1,  //BUF_DATA_FORMAT_8
  1,  //BUF_DATA_FORMAT_16
  2,  //BUF_DATA_FORMAT_8_8
  1,  //BUF_DATA_FORMAT_32
  2,  //BUF_DATA_FORMAT_16_16
  3,  //BUF_DATA_FORMAT_10_11_11
  3,  //BUF_DATA_FORMAT_11_11_10
  4,  //BUF_DATA_FORMAT_10_10_10_2
  4,  //BUF_DATA_FORMAT_2_10_10_10
  4,  //BUF_DATA_FORMAT_8_8_8_8
  2,  //BUF_DATA_FORMAT_32_32
  4,  //BUF_DATA_FORMAT_16_16_16_16
  3,  //BUF_DATA_FORMAT_32_32_32
  4,  //BUF_DATA_FORMAT_32_32_32_32
  0); //BUF_DATA_FORMAT_RESERVED

 DFMT_ELEM_SIZE:array[0..15] of Byte=(
  0,  //BUF_DATA_FORMAT_INVALID
  1,  //BUF_DATA_FORMAT_8
  2,  //BUF_DATA_FORMAT_16
  1,  //BUF_DATA_FORMAT_8_8
  4,  //BUF_DATA_FORMAT_32
  2,  //BUF_DATA_FORMAT_16_16
  4,  //BUF_DATA_FORMAT_10_11_11
  4,  //BUF_DATA_FORMAT_11_11_10
  4,  //BUF_DATA_FORMAT_10_10_10_2
  4,  //BUF_DATA_FORMAT_2_10_10_10
  1,  //BUF_DATA_FORMAT_8_8_8_8
  4,  //BUF_DATA_FORMAT_32_32
  2,  //BUF_DATA_FORMAT_16_16_16_16
  4,  //BUF_DATA_FORMAT_32_32_32
  4,  //BUF_DATA_FORMAT_32_32_32_32
  0); //BUF_DATA_FORMAT_RESERVED

 DFMT_SIZE:array[0..15] of Byte=(
  0,  //BUF_DATA_FORMAT_INVALID
  1,  //BUF_DATA_FORMAT_8            //shr 2
  2,  //BUF_DATA_FORMAT_16           //shr 2
  2,  //BUF_DATA_FORMAT_8_8          //shr 2
  4,  //BUF_DATA_FORMAT_32           //shr 2
  4,  //BUF_DATA_FORMAT_16_16        //shr 2
  4,  //BUF_DATA_FORMAT_10_11_11     //shr 2
  4,  //BUF_DATA_FORMAT_11_11_10     //shr 2
  4,  //BUF_DATA_FORMAT_10_10_10_2   //shr 2
  4,  //BUF_DATA_FORMAT_2_10_10_10   //shr 2
  4,  //BUF_DATA_FORMAT_8_8_8_8      //shr 2
  8,  //BUF_DATA_FORMAT_32_32        //shr 3
  8,  //BUF_DATA_FORMAT_16_16_16_16  //shr 3
  12, //BUF_DATA_FORMAT_32_32_32     //div 12
  16, //BUF_DATA_FORMAT_32_32_32_32  //shr 4
  0); //BUF_DATA_FORMAT_RESERVED

function Buf_info(grp:PsrDataLayout;dsel:Tdst_sel;DFMT,NFMT,count:Byte):TBuf_info; inline;
begin
 Result.grp  :=grp;
 Result.dsel :=dsel;
 Result.DFMT :=DFMT;
 Result.NFMT :=NFMT;
 Result.count:=count;
end;

function dst_sel(r,g,b,a:Byte):Tdst_sel; inline;
begin
 Result[0]:=r;
 Result[1]:=g;
 Result[2]:=b;
 Result[3]:=a;
end;

function get_reverse_dst_sel(dst:Tdst_sel):Tdst_sel;
var
 i,f,d:Byte;
begin
 Result:=Default(Tdst_sel);
 For i:=0 to 3 do
  For f:=0 to 3 do
  begin
   d:=dst[f];
   Case d of
    4..7:
     begin
      d:=d-4;
      if (i=d) then
      begin
       Result[i]:=f+4;
       Break;
      end;
     end;
    else;
   end;
  end;
end;

function TBuf_info.GetResultType:TsrDataType;
begin
 Result:=dtFloat32;
 Case NFMT of
  BUF_NUM_FORMAT_UINT:Result:=dtUint32;
  BUF_NUM_FORMAT_SINT:Result:=dtInt32;
  else;
 end;
end;

function TBuf_info.GetElemType:TsrDataType;
begin
 Result:=dtUnknow;
 Case DFMT_ELEM_SIZE[DFMT] of
  1:Case NFMT of
     BUF_NUM_FORMAT_SNORM   ,
     BUF_NUM_FORMAT_SSCALED ,
     BUF_NUM_FORMAT_SINT    ,
     BUF_NUM_FORMAT_SNORM_NZ:Result:=dtInt8;
     else                    Result:=dtUint8;
    end;
  2:Case NFMT of
     BUF_NUM_FORMAT_SNORM   ,
     BUF_NUM_FORMAT_SSCALED ,
     BUF_NUM_FORMAT_SINT    ,
     BUF_NUM_FORMAT_SNORM_NZ:Result:=dtInt16;
     BUF_NUM_FORMAT_FLOAT   :Result:=dtHalf16;
     else                    Result:=dtUint16;
    end;
  4:Case NFMT of
     BUF_NUM_FORMAT_SNORM   ,
     BUF_NUM_FORMAT_SSCALED ,
     BUF_NUM_FORMAT_SINT    ,
     BUF_NUM_FORMAT_SNORM_NZ:Result:=dtInt32;
     BUF_NUM_FORMAT_FLOAT   :Result:=dtFloat32;
     else                    Result:=dtUint32;
    end;
  else;
 end;
end;

function GetElemCount(DFMT:Byte):Byte; inline;
begin
 Result:=DFMT_ELEM_COUNT[DFMT];
end;

function TBuf_info.GetElemCount:Byte; inline;
begin
 Result:=DFMT_ELEM_COUNT[DFMT];
end;

function TBuf_info.GetElemSize:Byte; inline;
begin
 Result:=DFMT_ELEM_SIZE[DFMT];
end;

function TBuf_info.GetSizeFormat:Byte; inline;
begin
 Result:=DFMT_SIZE[DFMT];
end;

function Min(a,b:PtrInt):PtrInt; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

function TBuf_info.GetAlignSize:Byte; inline;
begin
 Result:=Min(4,GetSizeFormat);
end;

function TBuf_info.IsComp:Boolean;
begin
 Case DFMT of
  BUF_DATA_FORMAT_8          ,
  BUF_DATA_FORMAT_16         ,
  BUF_DATA_FORMAT_8_8        ,
  BUF_DATA_FORMAT_16_16      ,
  BUF_DATA_FORMAT_10_11_11   ,
  BUF_DATA_FORMAT_11_11_10   ,
  BUF_DATA_FORMAT_10_10_10_2 ,
  BUF_DATA_FORMAT_2_10_10_10 ,
  BUF_DATA_FORMAT_8_8_8_8    ,
  BUF_DATA_FORMAT_16_16_16_16:Result:=True;
  else
                              Result:=False;
 end;
end;

function TBuf_info.IsExtFormat:Boolean;
begin
 Case DFMT of
  BUF_DATA_FORMAT_10_11_11   ,
  BUF_DATA_FORMAT_11_11_10   ,
  BUF_DATA_FORMAT_10_10_10_2 ,
  BUF_DATA_FORMAT_2_10_10_10 :Result:=True;
  else
                              Result:=False;
 end;
end;

function TBuf_info.GetImageFormat:Byte;
begin
 Result:=ImageFormat.Unknown;
 Case DFMT of
  BUF_DATA_FORMAT_8          : //R8 R8Snorm R8ui R8i
   Case NFMT of
     BUF_NUM_FORMAT_UNORM   :Result:=ImageFormat.R8;
     BUF_NUM_FORMAT_SNORM   :Result:=ImageFormat.R8Snorm;
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.R8ui;
     BUF_NUM_FORMAT_SINT    :Result:=ImageFormat.R8i;
     BUF_NUM_FORMAT_SNORM_NZ:Result:=ImageFormat.R8i;
    else;
   end;
  BUF_DATA_FORMAT_16         : //R16 R16Snorm R16ui R16i R16f
   Case NFMT of
     BUF_NUM_FORMAT_UNORM   :Result:=ImageFormat.R16;
     BUF_NUM_FORMAT_SNORM   :Result:=ImageFormat.R16Snorm;
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.R16ui;
     BUF_NUM_FORMAT_SINT    :Result:=ImageFormat.R16i;
     BUF_NUM_FORMAT_SNORM_NZ:Result:=ImageFormat.R16i;
     BUF_NUM_FORMAT_USCALED ,
     BUF_NUM_FORMAT_SSCALED ,
     BUF_NUM_FORMAT_FLOAT   :Result:=ImageFormat.R16f;
   end;
  BUF_DATA_FORMAT_8_8        : //Rg8 Rg8Snorm Rg8ui Rg8i
   Case NFMT of
     BUF_NUM_FORMAT_UNORM   :Result:=ImageFormat.Rg8;
     BUF_NUM_FORMAT_SNORM   :Result:=ImageFormat.Rg8Snorm;
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.Rg8ui;
     BUF_NUM_FORMAT_SINT    :Result:=ImageFormat.Rg8i;
     BUF_NUM_FORMAT_SNORM_NZ:Result:=ImageFormat.Rg8i;
    else;
   end;
  BUF_DATA_FORMAT_32         : //R32ui R32i R32f
   Case NFMT of
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.R32ui;
     BUF_NUM_FORMAT_SINT    :Result:=ImageFormat.R32i;
     BUF_NUM_FORMAT_SNORM_NZ:Result:=ImageFormat.R32i;
     BUF_NUM_FORMAT_USCALED ,
     BUF_NUM_FORMAT_SSCALED ,
     BUF_NUM_FORMAT_FLOAT   :Result:=ImageFormat.R32f;
    else;
   end;
  BUF_DATA_FORMAT_16_16      : //Rg16 Rg16Snorm Rg16ui Rg16i Rg16f
   Case NFMT of
     BUF_NUM_FORMAT_UNORM   :Result:=ImageFormat.Rg16;
     BUF_NUM_FORMAT_SNORM   :Result:=ImageFormat.Rg16Snorm;
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.Rg16ui;
     BUF_NUM_FORMAT_SINT    :Result:=ImageFormat.Rg16i;
     BUF_NUM_FORMAT_SNORM_NZ:Result:=ImageFormat.Rg16i;
     BUF_NUM_FORMAT_USCALED ,
     BUF_NUM_FORMAT_SSCALED ,
     BUF_NUM_FORMAT_FLOAT   :Result:=ImageFormat.Rg16f;
   end;
  BUF_DATA_FORMAT_10_11_11   ,
  BUF_DATA_FORMAT_11_11_10   : //R11fG11fB10f
   Case NFMT of
     BUF_NUM_FORMAT_FLOAT   :Result:=ImageFormat.R11fG11fB10f;
    else;
   end;
  BUF_DATA_FORMAT_10_10_10_2 ,
  BUF_DATA_FORMAT_2_10_10_10 : //Rgb10A2 Rgb10a2ui
   Case NFMT of
     BUF_NUM_FORMAT_UNORM   :Result:=ImageFormat.Rgb10A2;
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.Rgb10a2ui;
    else;
   end;
  BUF_DATA_FORMAT_8_8_8_8    : //Rgba8 Rgba8Snorm Rgba8ui Rgba8i
   Case NFMT of
     BUF_NUM_FORMAT_UNORM   :Result:=ImageFormat.Rgba8;
     BUF_NUM_FORMAT_SNORM   :Result:=ImageFormat.Rgba8Snorm;
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.Rgba8ui;
     BUF_NUM_FORMAT_SINT    :Result:=ImageFormat.Rgba8i;
     BUF_NUM_FORMAT_SNORM_NZ:Result:=ImageFormat.Rgba8i;
    else;
   end;
  BUF_DATA_FORMAT_32_32      : //Rg32ui Rg32i Rg32f
   Case NFMT of
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.Rg32ui;
     BUF_NUM_FORMAT_SINT    :Result:=ImageFormat.Rg32i;
     BUF_NUM_FORMAT_SNORM_NZ:Result:=ImageFormat.Rg32i;
     BUF_NUM_FORMAT_USCALED ,
     BUF_NUM_FORMAT_SSCALED ,
     BUF_NUM_FORMAT_FLOAT   :Result:=ImageFormat.Rg32f;
    else;
   end;
  BUF_DATA_FORMAT_16_16_16_16: //Rgba16 Rgba16Snorm Rgba16ui Rgba16i Rgba16f
   Case NFMT of
     BUF_NUM_FORMAT_UNORM   :Result:=ImageFormat.Rgba16;
     BUF_NUM_FORMAT_SNORM   :Result:=ImageFormat.Rgba16Snorm;
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.Rgba16ui;
     BUF_NUM_FORMAT_SINT    :Result:=ImageFormat.Rgba16i;
     BUF_NUM_FORMAT_SNORM_NZ:Result:=ImageFormat.Rgba16i;
     BUF_NUM_FORMAT_USCALED ,
     BUF_NUM_FORMAT_SSCALED ,
     BUF_NUM_FORMAT_FLOAT   :Result:=ImageFormat.Rgba16f;
   end;
  BUF_DATA_FORMAT_32_32_32   ,
  BUF_DATA_FORMAT_32_32_32_32: //Rgba32ui Rgba32i Rgba32f
   Case NFMT of
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.Rgba32ui;
     BUF_NUM_FORMAT_SINT    :Result:=ImageFormat.Rgba32i;
     BUF_NUM_FORMAT_SNORM_NZ:Result:=ImageFormat.Rgba32i;
     BUF_NUM_FORMAT_USCALED ,
     BUF_NUM_FORMAT_SSCALED ,
     BUF_NUM_FORMAT_FLOAT   :Result:=ImageFormat.Rgba32f;
    else;
   end;
  else;
 end;
end;

//

function TBuf_info.GetImageFormatElement:Byte;
begin
 Result:=ImageFormat.Unknown;
 Case DFMT of
  BUF_DATA_FORMAT_8          ,
  BUF_DATA_FORMAT_8_8        ,
  BUF_DATA_FORMAT_8_8_8_8    : //R8 R8Snorm R8ui R8i
   Case NFMT of
     BUF_NUM_FORMAT_UNORM   :Result:=ImageFormat.R8;
     BUF_NUM_FORMAT_SNORM   :Result:=ImageFormat.R8Snorm;
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.R8ui;
     BUF_NUM_FORMAT_SINT    :Result:=ImageFormat.R8i;
     BUF_NUM_FORMAT_SNORM_NZ:Result:=ImageFormat.R8i;
    else;
   end;
  BUF_DATA_FORMAT_16         ,
  BUF_DATA_FORMAT_16_16      ,
  BUF_DATA_FORMAT_16_16_16_16: //R16 R16Snorm R16ui R16i R16f
   Case NFMT of
     BUF_NUM_FORMAT_UNORM   :Result:=ImageFormat.R16;
     BUF_NUM_FORMAT_SNORM   :Result:=ImageFormat.R16Snorm;
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.R16ui;
     BUF_NUM_FORMAT_SINT    :Result:=ImageFormat.R16i;
     BUF_NUM_FORMAT_SNORM_NZ:Result:=ImageFormat.R16i;
     BUF_NUM_FORMAT_USCALED ,
     BUF_NUM_FORMAT_SSCALED ,
     BUF_NUM_FORMAT_FLOAT   :Result:=ImageFormat.R16f;
   end;
  BUF_DATA_FORMAT_32         , //R32ui R32i R32f
  BUF_DATA_FORMAT_32_32      ,
  BUF_DATA_FORMAT_32_32_32   ,
  BUF_DATA_FORMAT_32_32_32_32:
   Case NFMT of
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.R32ui;
     BUF_NUM_FORMAT_SINT    :Result:=ImageFormat.R32i;
     BUF_NUM_FORMAT_SNORM_NZ:Result:=ImageFormat.R32i;
     BUF_NUM_FORMAT_USCALED ,
     BUF_NUM_FORMAT_SSCALED ,
     BUF_NUM_FORMAT_FLOAT   :Result:=ImageFormat.R32f;
    else;
   end;
  BUF_DATA_FORMAT_10_11_11   ,
  BUF_DATA_FORMAT_11_11_10   : //R11fG11fB10f
   Case NFMT of
     BUF_NUM_FORMAT_FLOAT   :Result:=ImageFormat.R11fG11fB10f;
    else;
   end;
  BUF_DATA_FORMAT_10_10_10_2 ,
  BUF_DATA_FORMAT_2_10_10_10 : //Rgb10A2 Rgb10a2ui
   Case NFMT of
     BUF_NUM_FORMAT_UNORM   :Result:=ImageFormat.Rgb10A2;
     BUF_NUM_FORMAT_UINT    :Result:=ImageFormat.Rgb10a2ui;
    else;
   end;
  else;
 end;
end;

//

function TBuf_info.GetImageInfo:TsrImageInfo;
begin
 Result:=Default(TsrImageInfo);
 Result.dtype:=GetResultType;

 Result.tinfo.Dim    :=Dim.Buffer;
 Result.tinfo.Depth  :=2;
 Result.tinfo.Arrayed:=0;
 Result.tinfo.MS     :=0;
 Result.tinfo.Sampled:=2;
 Result.tinfo.Format :=GetImageFormat;
end;

function TBuf_info.GetImageInfoElement:TsrImageInfo;
begin
 Result:=Default(TsrImageInfo);
 Result.dtype:=GetResultType;

 Result.tinfo.Dim    :=Dim.Buffer;
 Result.tinfo.Depth  :=2;
 Result.tinfo.Arrayed:=0;
 Result.tinfo.MS     :=0;
 Result.tinfo.Sampled:=2;
 Result.tinfo.Format :=GetImageFormatElement;
end;

end.

