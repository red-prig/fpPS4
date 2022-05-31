unit srTypes;

{$mode objfpc}{$H+}

interface

uses
  bittype,
  Half16,
  spirv,
  srNodes,
  srRefId;

type
 TsrDataType=(
  //Real types
  dtUnknow,
  dtBool,

  dtFloat32,
  dtHalf16,

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
  dtVec4f,

  dtVec2h,
  dtVec4h,

  //Spirv types
  dtTypeVoid,
  dtTypeFunction,
  dtTypePointer,
  dtTypeStruct,
  dtTypeArray,
  dtTypeRuntimeArray,
  dtTypeImage,
  dtTypeSampler,
  dtTypeSampledImage
 );

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

 PPsrType=^PsrType;
 PsrType=^TsrType;

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

 TsrType=object
  pPrev,pNext,pLeft,pRight:PsrType;
  //----
  ID:TsrRefId; //post id
  read_count:DWORD;
  dtype:TsrDataType;
  key:packed record
   OpId:DWORD;
   count:DWORD;
   ext:packed record
    case byte of
     0:(int_size,int_sign:DWORD);
     1:(float_size:DWORD);
     2:(storage_class:DWORD);
     3:(pField:Pointer);
     4:(array_count,array_stride:DWORD);
     5:(vector_count:DWORD);
     6:(image:TsrTypeImageInfo);
   end;
  end;
  Data:record end;
  function  c(n1,n2:PsrType):Integer; static;
  Procedure mark_read;
  Procedure mark_unread;
  function  GetCompItem(i:DWORD):PsrType; inline;
  Procedure SetCompItem(i:DWORD;p:PsrType); inline;
  Procedure _mark_read_child;
  Procedure _mark_unread_child;
  Procedure Clear;
 end;

 TsrTypeList=object
  type
   TNodeList=specialize TNodeList<PsrType>;
   TNodeFetch=specialize TNodeFetch<PsrType,TsrType>;
  var
   Alloc:TfnAlloc;
   FList:TNodeList;
   FNTree:TNodeFetch;
  function _New(count:DWORD):PsrType;
  function _Insert(node:PsrType):PsrType;
  function _Fetch(node:PsrType):PsrType;
  function _FetchVector(dtype,ctype:TsrDataType;vector_count:DWORD):PsrType;
  function Fetch(dtype:TsrDataType):PsrType;
  function FetchPointer(child:PsrType;storage_class:DWORD):PsrType;
  function FetchFunction(ret:PsrType):PsrType;
  function FetchFunction1(ret,param:PsrType):PsrType;
  function FetchStruct(child:PsrType;pField:Pointer):PsrType;
  function FetchStructNode(node:PsrType;count:DWORD;pField:Pointer):PsrType;
  function FetchArrayNode(node,child:PsrType;array_count,array_stride:DWORD):PsrType;
  function FetchArray(child:PsrType;array_count,array_stride:DWORD):PsrType;
  function FetchRuntimeArray(child:PsrType;array_stride:DWORD):PsrType;
  function FetchImage(child:PsrType;image_info:TsrTypeImageInfo):PsrType;
  function FetchSampledImage(child:PsrType):PsrType;
 end;

const
 ExtImgBuf:TsrTypeImageInfo=(
  Dim    :0; //Dim1D
  Depth  :0;
  Arrayed:0;
  MS     :0;
  Sampled:2;
  Format :0; //Unknown
 );

 ExtImage2D:TsrTypeImageInfo=(
  Dim    :1; //Dim2D
  Depth  :0;
  Arrayed:0;
  MS     :0;
  Sampled:1;
  Format :0; //Unknown
 );

function _GetNodeSize(count:DWORD):ptruint; inline;
function LazyType2(t1,t2:TsrDataType):TsrDataType;
function LazyType3(t1,t2,t3:TsrDataType):TsrDataType;
function isIntVector(rtype:TsrDataType):Boolean; inline;
function isIntScalar(rtype:TsrDataType):Boolean; inline;
function GetVecChild(rtype:TsrDataType):TsrDataType;
function GetVecCount(rtype:TsrDataType):Byte;
Function GetVecType(elem:TsrDataType;count:Byte):TsrDataType;
function isVector(rtype:TsrDataType):Boolean; inline;
function GetTypeHigh(rtype:TsrDataType):QWORD;
function CompareType(rtype1,rtype2:TsrDataType):Boolean;
function SignType(rtype:TsrDataType):Byte;
function BitSizeType(rtype:TsrDataType):Byte;
function TryBitcastType(rtype1,rtype2:TsrDataType):Boolean;

implementation

function TsrType.c(n1,n2:PsrType):Integer;
begin
 Result:=CompareByte(n1^.key,n2^.key,SizeOf(TsrType.key));
 if (Result<>0) then Exit;
 Result:=ComparePtruint(@n1^.Data,@n2^.Data,n1^.key.count);
end;

Procedure TsrType.mark_read;
begin
 Inc(read_count);
end;

Procedure TsrType.mark_unread;
begin
 if (read_count<>0) then Dec(read_count);
end;


function TsrType.GetCompItem(i:DWORD):PsrType; inline;
begin
 Result:=PPsrType(@Data)[i];
end;

Procedure TsrType.SetCompItem(i:DWORD;p:PsrType); inline;
begin
 PPsrType(@Data)[i]:=p;
end;

Procedure TsrType._mark_read_child;
var
 i:DWORD;
begin
 if (key.count<>0) then
  For i:=0 to key.count-1 do
  begin
   GetCompItem(i)^.mark_read;
  end;
end;

Procedure TsrType._mark_unread_child;
var
 i:DWORD;
begin
 if (key.count<>0) then
  For i:=0 to key.count-1 do
  begin
   GetCompItem(i)^.mark_unread;
  end;
end;

Procedure TsrType.Clear;
var
 i:DWORD;
begin
 if (key.count<>0) then
 begin
  For i:=0 to key.count-1 do
  begin
   GetCompItem(i)^.mark_unread;
   SetCompItem(i,nil);
  end;
  key.count:=0;
 end;
end;

function _GetNodeSize(count:DWORD):ptruint; inline;
begin
 Result:=SizeOf(TsrType)+SizeOf(Pointer)*count;
end;

function TsrTypeList._New(count:DWORD):PsrType;
begin
 Result:=Alloc(_GetNodeSize(count));
end;

function TsrTypeList._Insert(node:PsrType):PsrType;
begin
 Result:=FNTree.Find(node);
 if (Result=nil) then
 begin
  FNTree.Insert(node);
  FList.Push_tail(node);
  Result:=node;
 end else
 begin
  node^._mark_unread_child;
 end;
end;

function TsrTypeList._Fetch(node:PsrType):PsrType;
var
 size:ptruint;
begin
 Result:=FNTree.Find(node);
 if (Result=nil) then
 begin
  size:=_GetNodeSize(node^.key.count);
  Result:=Alloc(size);
  Move(node^,Result^,Size);
  FNTree.Insert(Result);
  FList.Push_tail(Result);
 end else
 begin
  node^._mark_unread_child;
 end;
end;

function TsrTypeList._FetchVector(dtype,ctype:TsrDataType;vector_count:DWORD):PsrType;
var
 rec:record
  node:TsrType;
  align:Pointer;
 end;
 child:PsrType;
begin
 Result:=nil;
 child:=Fetch(ctype);
 rec.node:=Default(TsrType);
 rec.node.dtype:=dtype;
 rec.node.key.OpId:=Op.OpTypeVector;
 rec.node.key.ext.vector_count:=vector_count;
 rec.node.key.count:=1;
 rec.node.SetCompItem(0,child);
 Result:=_Fetch(@rec.node);
 Result^.mark_read;
end;

function TsrTypeList.Fetch(dtype:TsrDataType):PsrType;
var
 rec:record
  node:TsrType;
  align:array[0..1] of Pointer;
 end;
 child:PsrType;
begin
 Result:=nil;
 rec.node:=Default(TsrType);
 Case dtype of
  dtUnknow:;

  dtBool:
    begin
     rec.node.dtype:=dtype;
     rec.node.key.OpId:=Op.OpTypeBool;
     Result:=_Fetch(@rec.node);
     Result^.mark_read;
    end;

  dtInt8,
  dtUint8,
  dtInt16,
  dtUint16,
  dtInt32,
  dtUint32,
  dtInt64,
  dtUint64:
    begin
     rec.node.dtype:=dtype;
     rec.node.key.OpId:=Op.OpTypeInt;
     rec.node.key.ext.int_sign:=SignType(dtype);
     rec.node.key.ext.int_size:=BitSizeType(dtype);
     Result:=_Fetch(@rec.node);
     Result^.mark_read;
    end;

  dtFloat32,
  dtHalf16:
    begin
     rec.node.dtype:=dtype;
     rec.node.key.OpId:=Op.OpTypeFloat;
     rec.node.key.ext.float_size:=BitSizeType(dtype);
     Result:=_Fetch(@rec.node);
     Result^.mark_read;
    end;

  dtTypeVoid:
    begin
     rec.node.dtype:=dtype;
     rec.node.key.OpId:=Op.OpTypeVoid;
     Result:=_Fetch(@rec.node);
     Result^.mark_read;
    end;

  dtTypeSampler:
    begin
     rec.node.dtype:=dtype;
     rec.node.key.OpId:=Op.OpTypeSampler;
     Result:=_Fetch(@rec.node);
     Result^.mark_read;
    end;

  //

  dtVec2b,
  dtVec3b,
  dtVec4b,

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
  dtVec4f,

  dtVec2h,
  dtVec4h:Result:=_FetchVector(dtype,GetVecChild(dtype),GetVecCount(dtype));

  //

  dtStruct2u:
    begin
     child:=Fetch(dtUint32);
     child^.mark_read;
     rec.node.dtype:=dtStruct2u;
     rec.node.key.OpId:=Op.OpTypeStruct;
     rec.node.key.count:=2;
     rec.node.SetCompItem(0,child);
     rec.node.SetCompItem(1,child);
     Result:=_Fetch(@rec.node);
     Result^.mark_read;
    end;

  //

  else
   Assert(false);
 end;

end;

function TsrTypeList.FetchPointer(child:PsrType;storage_class:DWORD):PsrType;
var
 rec:record
  node:TsrType;
  align:Pointer;
 end;
begin
 Assert(child<>nil);
 rec.node:=Default(TsrType);
 rec.node.dtype:=dtTypePointer;
 rec.node.key.OpId:=Op.OpTypePointer;
 rec.node.key.ext.storage_class:=storage_class;
 rec.node.key.count:=1;
 rec.node.SetCompItem(0,child);
 Result:=_Fetch(@rec.node);
 Result^.mark_read;
end;

function TsrTypeList.FetchFunction(ret:PsrType):PsrType;
var
 rec:record
  node:TsrType;
  align:Pointer;
 end;
begin
 Assert(ret<>nil);
 rec.node:=Default(TsrType);
 rec.node.dtype:=dtTypeFunction;
 rec.node.key.OpId:=Op.OpTypeFunction;
 rec.node.key.count:=1;
 rec.node.SetCompItem(0,ret);
 Result:=_Fetch(@rec.node);
 Result^.mark_read;
end;

function TsrTypeList.FetchFunction1(ret,param:PsrType):PsrType;
var
 rec:record
  node:TsrType;
  align:array[0..1] of Pointer;
 end;
begin
 Assert(ret<>nil);
 rec.node:=Default(TsrType);
 rec.node.dtype:=dtTypeFunction;
 rec.node.key.OpId:=Op.OpTypeFunction;
 rec.node.key.count:=2;
 rec.node.SetCompItem(0,ret);
 rec.node.SetCompItem(1,param);
 Result:=_Fetch(@rec.node);
 Result^.mark_read;
end;

function TsrTypeList.FetchStruct(child:PsrType;pField:Pointer):PsrType;
var
 rec:record
  node:TsrType;
  align:Pointer;
 end;
begin
 Assert(child<>nil);
 rec.node:=Default(TsrType);
 rec.node.dtype:=dtTypeStruct;
 rec.node.key.OpId:=Op.OpTypeStruct;
 rec.node.key.ext.pField:=pField;
 rec.node.key.count:=1;
 rec.node.SetCompItem(0,child);
 Result:=_Fetch(@rec.node);
 Result^.mark_read;
end;

function TsrTypeList.FetchStructNode(node:PsrType;count:DWORD;pField:Pointer):PsrType;
begin
 Assert(node<>nil);
 node^.dtype:=dtTypeStruct;
 node^.key.OpId:=Op.OpTypeStruct;
 node^.key.ext.pField:=pField;
 node^.key.count:=count;
 Result:=_Insert(node);
 Result^.mark_read;
end;

function TsrTypeList.FetchArrayNode(node,child:PsrType;array_count,array_stride:DWORD):PsrType;
var
 aUint:PsrType;
begin
 Assert(node<>nil);
 Assert(child<>nil);
 node^:=Default(TsrType);
 node^.dtype:=dtTypeArray;
 node^.key.OpId:=Op.OpTypeArray;
 node^.key.ext.array_count:=array_count;
 node^.key.ext.array_stride:=array_stride;
 node^.key.count:=2;
 node^.SetCompItem(0,child);
 aUint:=Fetch(dtUInt32);
 node^.SetCompItem(1,aUint);
 FList.Push_tail(node);
 Result:=node;
 Result^.mark_read;
end;

function TsrTypeList.FetchArray(child:PsrType;array_count,array_stride:DWORD):PsrType;
var
 rec:record
  node:TsrType;
  align:array[0..1] of Pointer;
 end;
 aUint:PsrType;
begin
 Assert(child<>nil);
 rec.node:=Default(TsrType);
 rec.node.dtype:=dtTypeArray;
 rec.node.key.OpId:=Op.OpTypeArray;
 rec.node.key.ext.array_count:=array_count;
 rec.node.key.ext.array_stride:=array_stride;
 rec.node.key.count:=2;
 rec.node.SetCompItem(0,child);
 aUint:=Fetch(dtUInt32);
 rec.node.SetCompItem(1,aUint);
 Result:=_Fetch(@rec.node);
 Result^.mark_read;
end;

function TsrTypeList.FetchRuntimeArray(child:PsrType;array_stride:DWORD):PsrType;
var
 rec:record
  node:TsrType;
  align:Pointer;
 end;
begin
 Assert(child<>nil);
 rec.node:=Default(TsrType);
 rec.node.dtype:=dtTypeRuntimeArray;
 rec.node.key.OpId:=Op.OpTypeRuntimeArray;
 rec.node.key.ext.array_stride:=array_stride;
 rec.node.key.count:=1;
 rec.node.SetCompItem(0,child);
 Result:=_Fetch(@rec.node);
 Result^.mark_read;
end;

function TsrTypeList.FetchImage(child:PsrType;image_info:TsrTypeImageInfo):PsrType;
var
 rec:record
  node:TsrType;
  align:Pointer;
 end;
begin
 Assert(child<>nil);
 rec.node:=Default(TsrType);
 rec.node.dtype:=dtTypeImage;
 rec.node.key.OpId:=Op.OpTypeImage;
 rec.node.key.ext.image:=image_info;
 rec.node.key.count:=1;
 rec.node.SetCompItem(0,child);
 Result:=_Fetch(@rec.node);
 Result^.mark_read;
end;

function TsrTypeList.FetchSampledImage(child:PsrType):PsrType;
var
 rec:record
  node:TsrType;
  align:Pointer;
 end;
begin
 Assert(child<>nil);
 rec.node:=Default(TsrType);
 rec.node.dtype:=dtTypeSampledImage;
 rec.node.key.OpId:=Op.OpTypeSampledImage;
 rec.node.key.count:=1;
 rec.node.SetCompItem(0,child);
 Result:=_Fetch(@rec.node);
 Result^.mark_read;
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

function isIntVector(rtype:TsrDataType):Boolean; inline;
begin
 Case rtype of
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
  dtVec4i:Result:=True;
  else
           Result:=False;
 end;
end;

function isIntScalar(rtype:TsrDataType):Boolean; inline;
begin
 Case rtype of
  dtInt8,
  dtUint8,

  dtInt16,
  dtUint16,

  dtInt32 ,
  dtUint32,

  dtInt64 ,
  dtUint64:Result:=True;
  else
           Result:=False;
 end;
end;

function GetVecChild(rtype:TsrDataType):TsrDataType;
begin
 Case rtype of
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

  dtVec2i,
  dtVec3i,
  dtVec4i:Result:=dtInt32;

  dtVec2f,
  dtVec3f,
  dtVec4f:Result:=dtFloat32;

  dtVec2h,
  dtVec4h:Result:=dtHalf16;
  else
           Result:=dtUnknow;
 end;
end;

function GetVecCount(rtype:TsrDataType):Byte;
begin
 Case rtype of
  dtVec2b,
  dtVec2u8,
  dtVec2i8,
  dtVec2u16,
  dtVec2i16,
  dtVec2u,
  dtVec2i,
  dtVec2f,
  dtVec2h,
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

Function GetVecType(elem:TsrDataType;count:Byte):TsrDataType;
begin
 Result:=dtUnknow;
 if (count<=1) then Exit(elem);
 Case elem of
  dtBool:
    Case count of
     2:Result:=dtVec2b;
     3:Result:=dtVec3b;
     4:Result:=dtVec4b;
    end;
  dtUint8:
    Case count of
     2:Result:=dtVec2u8;
     4:Result:=dtVec4u8;
    end;
  dtInt8:
    Case count of
     2:Result:=dtVec2i8;
     4:Result:=dtVec4i8;
    end;
  dtUint16:
    Case count of
     2:Result:=dtVec2u16;
     4:Result:=dtVec4u16;
    end;
  dtInt16:
    Case count of
     2:Result:=dtVec2i16;
     4:Result:=dtVec4i16;
    end;
  dtHalf16:
    Case count of
     2:Result:=dtVec2h;
     4:Result:=dtVec4h;
    end;
  dtFloat32:
    Case count of
     2:Result:=dtVec2f;
     3:Result:=dtVec3f;
     4:Result:=dtVec4f;
    end;
  dtInt32:
    Case count of
     2:Result:=dtVec2i;
     3:Result:=dtVec3i;
     4:Result:=dtVec4i;
    end;
  dtUint32:
    Case count of
     2:Result:=dtVec2u;
     3:Result:=dtVec3u;
     4:Result:=dtVec4u;
    end;
  else;
 end;
end;

function isVector(rtype:TsrDataType):Boolean; inline;
begin
 Result:=GetVecChild(rtype)<>dtUnknow;
end;

function GetTypeHigh(rtype:TsrDataType):QWORD;
var
 s:Byte;
begin
 Result:=0;
 if (rtype=dtBool) then Exit(1);
 s:=BitSizeType(rtype);
 Case s of
   8:Result:=High(Byte);
  16:Result:=High(Word);
  32:Result:=High(DWord);
  64:Result:=High(QWord);
  else
     Assert(false);
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

function SignType(rtype:TsrDataType):Byte;
begin
 Result:=0;
 Case rtype of

  dtInt8,
  dtInt16,
  dtInt32,
  dtInt64:Result:=1;

  dtHalf16,
  dtFloat32,
  dtVec2h,
  dtVec4h,
  dtVec2f,
  dtVec3f,
  dtVec4f:Result:=1;

  else;
 end;
end;

function BitSizeType(rtype:TsrDataType):Byte;
begin
 Result:=0;
 Case rtype of

  dtInt8,
  dtUint8:Result:=8;

  dtInt16,
  dtUint16,
  dtVec2u8,
  dtVec2i8,
  dtHalf16:Result:=16;

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

  dtInt64,
  dtUint64,
  dtVec4u16,
  dtVec4i16,
  dtVec2f,
  dtVec4h,
  dtStruct2u:Result:=64;

  dtVec3u,
  dtVec3f:Result:=96;
  dtVec4f:Result:=128;

  else;
 end;
end;

function TryBitcastType(rtype1,rtype2:TsrDataType):Boolean;
var
 s,d:Byte;
begin
 s:=BitSizeType(rtype1);
 d:=BitSizeType(rtype2);
 Result:=(s<>0) and (d<>0) and (s=d);
end;


end.

