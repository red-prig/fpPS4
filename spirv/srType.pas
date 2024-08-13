unit srType;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
 bittype,
 Half16;

type
 TsrDataType=(
  //Real types
  dtUnknow,
  dtBool,

  dtHalf16,
  dtFloat32,
  dtFloat64,

  dtInt8,
  dtUint8,

  dtInt16,
  dtUint16,

  dtInt32,
  dtUint32,

  dtInt64,
  dtUint64,

  //Composite types
  dtVec2b,
  dtVec3b,
  dtVec4b,

  dtStruct2i,
  dtStruct2u,

  dtVec2u8,
  dtVec4u8,

  dtVec2i8,
  dtVec4i8,

  dtVec2u16,
  dtVec4u16,

  dtVec2i16,
  dtVec4i16,

  dtVec2u,
  dtVec3u,
  dtVec4u,

  dtVec2i,
  dtVec3i,
  dtVec4i,

  dtVec2h,
  dtVec4h,

  dtVec2f,
  dtVec3f,
  dtVec4f,

  //Spirv types
  dtTypeVoid,
  dtTypeFunction,
  dtTypePointer,
  dtTypeStruct,
  dtTypeArray,
  dtTypeRuntimeArray,
  dtTypeImage,
  dtTypeSampler,
  dtTypeSampledImage,

  dtString,
  dtConstant
 );

 TsrDataTypeHelper = type helper for TsrDataType
  function isInt:Boolean;
  function isFloat:Boolean;
  function isBool:Boolean;
  function isVector:Boolean;
  function isScalar:Boolean;
  function Child:TsrDataType;
  function Count:Byte;
  function Sign:Byte;
  function BitSize:Byte;
  function High:QWORD;
  function AsVector(_count:Byte):TsrDataType;
  function AsStruct2:TsrDataType;
 end;

 Pvec2f=^Tvec2f;
 Tvec2f=array[0..1] of Single;

 Pvec3f=^Tvec3f;
 Tvec3f=array[0..2] of Single;

 Pvec4f=^Tvec4f;
 Tvec4f=array[0..3] of Single;

 Pvec2h=^Tvec2h;
 Tvec2h=array[0..1] of THalf16;

 Pvec4h=^Tvec4h;
 Tvec4h=array[0..3] of THalf16;

 //child - result type
 TsrTypeImageInfo=bitpacked record
  Dim    :bit3;  //Dim.*
  Depth  :bit2;  //0:no,1:yes,2:any
  Arrayed:bit1;  //0:no,1:yes
  MS     :bit1;  //0:no,1:yes
  Sampled:bit2;  //0:runtime,1:sampling,2:read/write
  Format :bit23; //ImageFormat.*
 end;

 PsrImageInfo=^TsrImageInfo;
 TsrImageInfo=packed record
  dtype:TsrDataType;
  tinfo:TsrTypeImageInfo;
 end;

function type_get_base_name1(dtype:TsrDataType):RawByteString;
function LazyType2(t1,t2:TsrDataType):TsrDataType;
function LazyType3(t1,t2,t3:TsrDataType):TsrDataType;
function StoreType(t:TsrDataType):TsrDataType;
function CompareType(rtype1,rtype2:TsrDataType):Boolean;
function TryBitcastType(rtype1,rtype2:TsrDataType):Boolean;
function is_unprep_type(old,new:TsrDataType;weak:Boolean):Boolean;

implementation

function type_get_base_name1(dtype:TsrDataType):RawByteString;
begin
 Result:='';
 case dtype of

  dtBool       :Result:='bool';

  dtHalf16     :Result:='half';
  dtFloat32    :Result:='float';
  dtFloat64    :Result:='double';

  dtInt8       :Result:='int8';
  dtUint8      :Result:='uint8';

  dtInt16      :Result:='int16';
  dtUint16     :Result:='uint16';

  dtInt32      :Result:='int';
  dtUint32     :Result:='uint';

  dtInt64      :Result:='int64';
  dtUint64     :Result:='uint64';

  //Composite types
  dtVec2b      :Result:='bvec2';
  dtVec3b      :Result:='bvec3';
  dtVec4b      :Result:='bvec4';

  dtStruct2i   :Result:='rec2i';
  dtStruct2u   :Result:='rec2u';

  dtVec2u8     :Result:='u8vec2';
  dtVec4u8     :Result:='u8vec4';

  dtVec2i8     :Result:='i8vec2';
  dtVec4i8     :Result:='i8vec4';

  dtVec2u16    :Result:='u16vec2';
  dtVec4u16    :Result:='u16vec4';

  dtVec2i16    :Result:='i16vec2';
  dtVec4i16    :Result:='i16vec4';

  dtVec2u      :Result:='uvec2';
  dtVec3u      :Result:='uvec3';
  dtVec4u      :Result:='uvec4';

  dtVec2i      :Result:='ivec2';
  dtVec3i      :Result:='ivec3';
  dtVec4i      :Result:='ivec4';

  dtVec2h      :Result:='hvec2';
  dtVec4h      :Result:='hvec4';

  dtVec2f      :Result:='vec2';
  dtVec3f      :Result:='vec3';
  dtVec4f      :Result:='vec4';

  dtTypeVoid   :Result:='void';
  dtTypeSampler:Result:='samp';

  else;
 end;
end;

function LazyType2(t1,t2:TsrDataType):TsrDataType;
begin
 if (t1<>dtUnknow) then Result:=t1 else Result:=t2;
end;

function LazyType3(t1,t2,t3:TsrDataType):TsrDataType;
begin
 if (t1<>dtUnknow) then Result:=t1 else Result:=t2;
 if (Result=dtUnknow) then Result:=t3;
end;

function StoreType(t:TsrDataType):TsrDataType;
begin
 if (t=dtBool) then
  Result:=dtUint32
 else
  Result:=t;
end;

function TsrDataTypeHelper.isInt:Boolean;
begin
 Case Self of
  dtInt8,
  dtUint8,

  dtInt16,
  dtUint16,

  dtInt32,
  dtUint32,

  dtInt64,
  dtUint64,

  dtStruct2i,
  dtStruct2u,

  dtVec2u8,
  dtVec4u8,

  dtVec2i8,
  dtVec4i8,

  dtVec2u16,
  dtVec4u16,

  dtVec2i16,
  dtVec4i16,

  dtVec2u,
  dtVec3u,
  dtVec4u,

  dtVec2i,
  dtVec3i,
  dtVec4i,

  dtVec2f,
  dtVec3f,
  dtVec4f:
   Result:=True;

  else
   Result:=False;
 end;
end;

function TsrDataTypeHelper.isFloat:Boolean;
begin
 Case Self of
  dtHalf16,
  dtFloat32,
  dtFloat64,

  dtVec2h,
  dtVec4h,

  dtVec2f,
  dtVec3f,
  dtVec4f:
   Result:=True;

  else
   Result:=False;
 end;
end;

function TsrDataTypeHelper.isBool:Boolean;
begin
 Case Self of
  dtBool,

  dtVec2b,
  dtVec3b,
  dtVec4b:Result:=True;
  else
   Result:=False;
 end;
end;

function TsrDataTypeHelper.isVector:Boolean;
begin
 Case Self of
  dtVec2b,
  dtVec3b,
  dtVec4b,

  dtStruct2i,
  dtStruct2u,

  dtVec2u8,
  dtVec4u8,

  dtVec2i8,
  dtVec4i8,

  dtVec2u16,
  dtVec4u16,

  dtVec2i16,
  dtVec4i16,

  dtVec2u,
  dtVec3u,
  dtVec4u,

  dtVec2i,
  dtVec3i,
  dtVec4i,

  dtVec2h,
  dtVec4h,

  dtVec2f,
  dtVec3f,
  dtVec4f:
   Result:=True;

  else
   Result:=False;
 end;
end;

function TsrDataTypeHelper.isScalar:Boolean;
begin
 Result:=not isVector;
end;

function TsrDataTypeHelper.Child:TsrDataType;
begin
 Case Self of
  dtVec2b,
  dtVec3b,
  dtVec4b:Result:=dtBool;

  dtVec2u8,
  dtVec4u8:Result:=dtUint8;

  dtVec2i8,
  dtVec4i8:Result:=dtInt8;

  dtVec2u16,
  dtVec4u16:Result:=dtUint16;

  dtVec2i16,
  dtVec4i16:Result:=dtInt16;

  dtStruct2u,
  dtVec2u,
  dtVec3u,
  dtVec4u:Result:=dtUint32;

  dtStruct2i,
  dtVec2i,
  dtVec3i,
  dtVec4i:Result:=dtInt32;

  dtVec2h,
  dtVec4h:Result:=dtHalf16;

  dtVec2f,
  dtVec3f,
  dtVec4f:Result:=dtFloat32;
  else
           Result:=dtUnknow;
 end;
end;

function TsrDataTypeHelper.Count:Byte;
begin
 Case Self of
  dtVec2b,
  dtVec2u8,
  dtVec2i8,
  dtVec2u16,
  dtVec2i16,
  dtVec2u,
  dtVec2i,
  dtVec2h,
  dtVec2f,
  dtStruct2i,
  dtStruct2u:Result:=2;

  dtVec3b,
  dtVec3u,
  dtVec3i,
  dtVec3f:Result:=3;

  dtVec4b,
  dtVec4u8,
  dtVec4i8,
  dtVec4u16,
  dtVec4i16,
  dtVec4u,
  dtVec4i,
  dtVec4f,
  dtVec4h:Result:=4;
  else
          Result:=0;
 end;
end;

function TsrDataTypeHelper.Sign:Byte;
begin
 Case Self of
  dtInt8,
  dtInt16,
  dtInt32,
  dtInt64,

  dtStruct2i,

  dtHalf16,
  dtFloat32,
  dtFloat64,
  dtVec2h,
  dtVec4h,
  dtVec2f,
  dtVec3f,
  dtVec4f:Result:=1;

  else
   Result:=0;
 end;
end;

function TsrDataTypeHelper.BitSize:Byte;
begin
 Case Self of
  dtInt8,
  dtUint8:Result:=8;

  dtHalf16,
  dtInt16,
  dtUint16,
  dtVec2u8,
  dtVec2i8:Result:=16;

  dtUnknow,
  dtBool,    //for typecast
  dtFloat32,
  dtInt32,
  dtUint32,
  dtVec4u8,
  dtVec4i8,
  dtVec2u16,
  dtVec2i16,
  dtVec2h:Result:=32;

  dtFloat64,
  dtInt64,
  dtUint64,
  dtVec4u16,
  dtVec4i16,
  dtVec2u,
  dtVec2i,
  dtVec2f,
  dtVec4h,
  dtStruct2i,
  dtStruct2u:Result:=64;

  dtVec3u,
  dtVec3i,
  dtVec3f:Result:=96;

  dtVec4u,
  dtVec4i,
  dtVec4f:Result:=128;

  else
   Result:=0;
 end;
end;

function TsrDataTypeHelper.High:QWORD;
var
 s:Byte;
begin
 Result:=0;
 if (Self=dtBool) then Exit(1);
 s:=BitSize;
 Case s of
   8:Result:=System.High(Byte);
  16:Result:=System.High(Word);
  32:Result:=System.High(DWord);
  64:Result:=System.High(QWord);
  else
     Assert(false);
 end;
end;

function TsrDataTypeHelper.AsVector(_count:Byte):TsrDataType;
begin
 Result:=dtUnknow;
 if (_count<=1) then Exit(Self);
 Case Self of
  dtBool:
    Case _count of
     2:Result:=dtVec2b;
     3:Result:=dtVec3b;
     4:Result:=dtVec4b;
    end;
  dtUint8:
    Case _count of
     2:Result:=dtVec2u8;
     4:Result:=dtVec4u8;
    end;
  dtInt8:
    Case _count of
     2:Result:=dtVec2i8;
     4:Result:=dtVec4i8;
    end;
  dtUint16:
    Case _count of
     2:Result:=dtVec2u16;
     4:Result:=dtVec4u16;
    end;
  dtInt16:
    Case _count of
     2:Result:=dtVec2i16;
     4:Result:=dtVec4i16;
    end;
  dtHalf16:
    Case _count of
     2:Result:=dtVec2h;
     4:Result:=dtVec4h;
    end;
  dtFloat32:
    Case _count of
     2:Result:=dtVec2f;
     3:Result:=dtVec3f;
     4:Result:=dtVec4f;
    end;
  dtInt32:
    Case _count of
     2:Result:=dtVec2i;
     3:Result:=dtVec3i;
     4:Result:=dtVec4i;
    end;
  dtUint32:
    Case _count of
     2:Result:=dtVec2u;
     3:Result:=dtVec3u;
     4:Result:=dtVec4u;
    end;
  else;
 end;
end;

function TsrDataTypeHelper.AsStruct2:TsrDataType;
begin
 Result:=dtUnknow;
 Case Self of
  dtInt32 :Result:=dtStruct2i;
  dtUint32:Result:=dtStruct2u;
  else;
 end;
end;

function CompareType(rtype1,rtype2:TsrDataType):Boolean;
begin
 Case rtype1 of
  dtInt8,
  dtUint8:Result:=(rtype2=dtInt8) or (rtype2=dtUint8);

  dtInt16,
  dtUint16:Result:=(rtype2=dtInt16) or (rtype2=dtUint16);

  dtInt32,
  dtUint32:Result:=(rtype2=dtInt32) or (rtype2=dtUint32);

  dtVec2u8,
  dtVec2i8:Result:=(rtype2=dtVec2u8) or (rtype2=dtVec2i8);

  dtVec4u8,
  dtVec4i8:Result:=(rtype2=dtVec4u8) or (rtype2=dtVec4i8);

  dtVec2u16,
  dtVec2i16:Result:=(rtype2=dtVec2u16) or (rtype2=dtVec2i16);

  dtVec4u16,
  dtVec4i16:Result:=(rtype2=dtVec4u16) or (rtype2=dtVec4i16);

  dtVec2u,
  dtVec2i:Result:=(rtype2=dtVec2u) or (rtype2=dtVec2i);

  dtVec3u,
  dtVec3i:Result:=(rtype2=dtVec3u) or (rtype2=dtVec3i);

  dtVec4u,
  dtVec4i:Result:=(rtype2=dtVec4u) or (rtype2=dtVec4i);

 else
          Result:=(rtype1=rtype2);
 end;
end;

function TryBitcastType(rtype1,rtype2:TsrDataType):Boolean;
var
 s,d:Byte;
begin
 s:=rtype1.BitSize;
 d:=rtype2.BitSize;
 Result:=(s<>0) and (d<>0) and (s=d);
end;

function is_unprep_type(old,new:TsrDataType;weak:Boolean):Boolean;
begin
 Result:=False;
 if (new<>dtUnknow) and (old<>new) then
 begin
  Case old of
    dtUnknow:Result:=True;
    dtBool  :Result:=weak;
   else;
  end;
 end;
end;

end.

