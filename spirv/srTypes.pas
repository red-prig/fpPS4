unit srTypes;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 spirv,
 srType,
 ginodes,
 srNode,
 srLiteral,
 srRefId;

type
 ntType=class(TsrNodeVmt)
  class Procedure zero_read   (node:PsrNode);               override;
  class Procedure zero_unread (node:PsrNode);               override;
  class function  Next        (node:PsrNode):Pointer;       override;
  class function  Prev        (node:PsrNode):Pointer;       override;
  class function  GetPrintName(node:PsrNode):RawByteString; override;
  class function  GetRef      (node:PsrNode):Pointer;       override;
 end;

 PPsrType=^PsrType;
 PsrType=^TsrType;
 TsrType=packed object(TsrNode)
  private
   pPrev,pNext,pLeft,pRight:PsrType;
   //
   ID:TsrRefId; //post id
   fdtype:TsrDataType;
   fsize:DWORD;
   fOpId:WORD;
   fcount:WORD;
   pData:PPsrNode;
   function  c(n1,n2:PsrType):Integer; static;
  public
   property  size:DWORD        read fsize;
   property  OpId:WORD         read fOpId;
   property  ItemCount:WORD    read fcount;
   Procedure Init; inline;
   function  dtype:TsrDataType;
   function  GetItem(i:Word):PsrNode;
   function  GetDWORD(i:Word):DWORD;
   function  array_stride:DWORD;
   function  array_count:DWORD;
   function  storage_class:DWORD;
   function  image_info:TsrTypeImageInfo;
   function  GetPrintName:RawByteString;
 end;

 PsrTypeList=^TsrTypeList;
 TsrTypeList=object
  type
   TNodeList=specialize TNodeList<PsrType>;
   TNodeFetch=specialize TNodeFetch<PsrType,TsrType>;
  var
   FEmit:TCustomEmit;
   FList:TNodeList;
   FNTree:TNodeFetch;
  Procedure Init(Emit:TCustomEmit);
  function  _Insert(node:PsrType;copy:Boolean):PsrType;
  function  _Fetch(node:PsrType;copy:Boolean):PsrType;
  function  _Fetch0(dtype:TsrDataType;OpId:DWORD):PsrType;
  function  _FetchVector(dtype:TsrDataType):PsrType;
  function  _FetchInt(dtype:TsrDataType):PsrType;
  function  _FetchFloat(dtype:TsrDataType):PsrType;
  function  _FetchStruct2(dtype:TsrDataType):PsrType;
  function  _FetchConst(dtype:TsrDataType;Value:QWORD):PsrType;
  function  Fetch(dtype:TsrDataType):PsrType;
  function  FetchPointer(child:PsrType;storage_class:DWORD):PsrType;
  function  FetchFunction(ret:PsrType):PsrType;
  function  FetchFunction(copy:Boolean;count:Byte;pData:PPsrType):PsrType;
  function  FetchStruct (count:Word;pData:PPsrType;copy:Boolean;_size:DWORD):PsrType;
  function  InsertStruct(count:Word;pData:PPsrType;copy:Boolean;_size:DWORD):PsrType;
  function  FetchArray(child:PsrType;array_count:DWORD):PsrType;
  function  FetchRuntimeArray(child:PsrType):PsrType;
  function  FetchImage(child:PsrType;image_info:TsrTypeImageInfo):PsrType;
  function  FetchSampledImage(child:PsrType):PsrType;
  function  First:PsrType; inline;
 end;

implementation

//

class Procedure ntType.zero_read(node:PsrNode);
var
 i:DWORD;
begin
 With PsrType(node)^ do
 begin
  if (fcount<>0) then
   For i:=0 to fcount-1 do
   begin
    GetItem(i)^.mark_read(node);
   end;
 end;
end;

class Procedure ntType.zero_unread(node:PsrNode);
var
 i:DWORD;
begin
 With PsrType(node)^ do
 begin
  if (fcount<>0) then
   For i:=0 to fcount-1 do
   begin
    GetItem(i)^.mark_unread(node);
   end;
 end;
end;

class function ntType.Next(node:PsrNode):Pointer;
begin
 Result:=PsrType(node)^.pNext;
end;

class function ntType.Prev(node:PsrNode):Pointer;
begin
 Result:=PsrType(node)^.pPrev;
end;

class function ntType.GetPrintName(node:PsrNode):RawByteString;
begin
 Result:=PsrType(node)^.GetPrintName;
end;

class function ntType.GetRef(node:PsrNode):Pointer;
begin
 Result:=@PsrType(node)^.ID;
end;

//

Procedure TsrType.Init; inline;
begin
 fntype:=ntType;
end;

function TsrType.c(n1,n2:PsrType):Integer;
begin
 //first OpId
 Result:=Integer(n1^.fOpId>n2^.fOpId)-Integer(n1^.fOpId<n2^.fOpId);
 if (Result<>0) then Exit;
 //second fCount
 Result:=Integer(n1^.fCount>n2^.fCount)-Integer(n1^.fCount<n2^.fCount);
 if (Result<>0) then Exit;
 //third pData
 Result:=ComparePtruint(PPtruint(n1^.pData),PPtruint(n2^.pData),n1^.fCount);
end;

function TsrType.dtype:TsrDataType;
begin
 Result:=dtUnknow;
 if (@Self=nil) then Exit;
 Result:=fdtype;
end;

function TsrType.GetItem(i:Word):PsrNode;
begin
 if (i>fCount) then Exit(nil);
 Result:=pData[i];
end;

function TsrType.GetDWORD(i:Word):DWORD;
var
 pCount:PsrLiteral;
begin
 Result:=0;
 if (i>fCount) then Exit;
 pCount:=pData[i]^.AsType(ntLiteral);
 if (pCount=nil) then Exit;
 Result:=pCount^.Value;
end;

function TsrType.array_stride:DWORD;
var
 child:PsrType;
begin
 Result:=0;

 Case fdtype of
   dtTypeArray:;
   dtTypeRuntimeArray:;
  else
   Exit;
 end;

 child:=GetItem(0)^.AsType(ntType);
 if (child=nil) then Exit;

 Result:=child^.fsize;
end;

function TsrType.array_count:DWORD;
var
 pConst:PsrType;
begin
 Result:=0;
 if (fdtype<>dtTypeArray) then Exit;
 pConst:=GetItem(1)^.AsType(ntType);
 if (pConst=nil) then Exit;
 Result:=pConst^.GetDWORD(1);
end;

function TsrType.storage_class:DWORD;
begin
 Result:=0;
 if (fdtype<>dtTypePointer) then Exit;
 Result:=GetDWORD(0);
end;

function TsrType.image_info:TsrTypeImageInfo;

begin
 Result:=Default(TsrTypeImageInfo);
 if (fdtype<>dtTypeImage) then Exit;
 if (fCount<>7) then Exit;
 //
 Result.Dim    :=GetDWORD(1);
 Result.Depth  :=GetDWORD(2);
 Result.Arrayed:=GetDWORD(3);
 Result.MS     :=GetDWORD(4);
 Result.Sampled:=GetDWORD(5);
 Result.Format :=GetDWORD(6);
end;

function type_get_base_name2(node:PsrType):RawByteString;
var
 R:PsrRefId;
 child:PsrType;
begin
 Result:='';
 case node^.dtype of
  dtTypeImage:
    begin
     R:=node^.GetRef;
     Assert(R<>nil  ,'type_get_base_name2$1');
     Assert(R^.Alloc,'type_get_base_name2$2');
     Result:='ti'+IntToStr(R^.ID);
    end;
  dtTypeSampledImage:
    begin
     child:=node^.GetItem(0)^.AsType(ntType);
     if (child=nil) then Exit;
     Result:=type_get_base_name2(child);
     if (Result='') then Exit;
     Result:='tm'+Result;
    end;
  dtTypeArray:
    begin
     child:=node^.GetItem(0)^.AsType(ntType);
     if (child=nil) then Exit;
     Result:=type_get_base_name2(child);
     if (Result='') then Exit;
     Result:='ta'+Result+IntToStr(node^.array_count);
    end;
  dtTypeRuntimeArray:
    begin
     child:=node^.GetItem(0)^.AsType(ntType);
     if (child=nil) then Exit;
     Result:=type_get_base_name2(child);
     if (Result='') then Exit;
     Result:='tr'+Result;
    end;
  dtTypeStruct:
    begin
     Assert(node^.ID.Alloc);
     Result:='ts'+IntToStr(node^.ID.ID);
    end;
  dtTypeFunction:
    begin
     child:=node^.GetItem(0)^.AsType(ntType);
     if (child=nil) then Exit;
     Result:=type_get_base_name2(child);
     if (Result='') then Exit;
     Result:='tf'+Result;
    end;
  dtConstant:
    begin
     Result:='tc'+IntToStr(node^.GetDWORD(1));
    end;
  else
    Result:=type_get_base_name1(node^.dtype);
 end;
end;

function type_get_base_name3(node:PsrType):RawByteString;
var
 child:PsrType;
begin
 case node^.dtype of
  dtTypePointer:
    begin
     child:=node^.GetItem(1)^.AsType(ntType);
     if (child=nil) then Exit;
     Result:=type_get_base_name2(child);
     if (Result='') then Exit;
     Case node^.storage_class of
      StorageClass.UniformConstant :Result:='p'+Result+'_uc';
      StorageClass.Input           :Result:='p'+Result+'_in';
      StorageClass.Uniform         :Result:='p'+Result+'_uf';
      StorageClass.Output          :Result:='p'+Result+'_ot';
      StorageClass.Workgroup       :Result:='p'+Result+'_wg';
      StorageClass.CrossWorkgroup  :Result:='p'+Result+'_cw';
      StorageClass.Private_        :Result:='p'+Result+'_pv';
      StorageClass.Function_       :Result:='p'+Result+'_fc';
      StorageClass.PushConstant    :Result:='p'+Result+'_pc';
      StorageClass.Image           :Result:='p'+Result+'_im';
      StorageClass.StorageBuffer   :Result:='p'+Result+'_sb';
      else
       Exit('');
     end;
    end;
  else
   begin
    Result:=type_get_base_name2(node);
   end;
 end;
end;

function TsrType.GetPrintName:RawByteString;
begin
 Result:=type_get_base_name3(@Self);
 if (Result='') then
 begin
  Assert(ID.Alloc);
  Result:='t'+IntToStr(ID.ID);
 end;
end;

//

Procedure TsrTypeList.Init(Emit:TCustomEmit);
begin
 FEmit:=Emit;
end;

function TsrTypeList._Insert(node:PsrType;copy:Boolean):PsrType;
var
 size:ptruint;
begin
 Result:=FEmit.Alloc(SizeOf(TsrType));
 Move(node^,Result^,SizeOf(TsrType));

 if copy and (node^.fCount<>0) then
 begin
  size:=SizeOf(Pointer)*node^.fCount;
  Result^.pData:=FEmit.Alloc(size);
  Move(node^.pData^,Result^.pData^,Size);
 end;

 FList.Push_tail(Result);
end;

function TsrTypeList._Fetch(node:PsrType;copy:Boolean):PsrType;
begin
 Result:=FNTree.Find(node);
 if (Result=nil) then
 begin
  Result:=_Insert(node,copy);
  FNTree.Insert(Result);
 end;
end;

function TsrTypeList._Fetch0(dtype:TsrDataType;OpId:DWORD):PsrType;
var
 node:TsrType;
begin
 Result:=nil;
 //
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtype;
 node.fsize :=dtype.BitSize div 8;
 node.fOpId :=OpId;
 Result:=_Fetch(@node,True);
end;

function TsrTypeList._FetchVector(dtype:TsrDataType):PsrType;
var
 pLiteralList:PsrLiteralList;
 node:TsrType;
 item:array[0..1] of PsrNode;
begin
 Result:=nil;
 pLiteralList:=FEmit.GetLiteralList;
 //
 item[0]:=Fetch(dtype.Child);
 item[1]:=pLiteralList^.FetchLiteral(dtype.Count,nil);
 //
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtype;
 node.fsize :=dtype.BitSize div 8;
 node.fOpId :=Op.OpTypeVector;
 node.fCount:=2;
 node.pData :=@item;
 Result:=_Fetch(@node,True);
end;

function TsrTypeList._FetchInt(dtype:TsrDataType):PsrType;
var
 pLiteralList:PsrLiteralList;
 node:TsrType;
 item:array[0..1] of PsrNode;
begin
 Result:=nil;
 pLiteralList:=FEmit.GetLiteralList;
 //
 item[0]:=pLiteralList^.FetchLiteral(dtype.BitSize,nil);
 item[1]:=pLiteralList^.FetchLiteral(dtype.Sign   ,nil);
 //
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtype;
 node.fsize :=dtype.BitSize div 8;
 node.fOpId :=Op.OpTypeInt;
 node.fCount:=2;
 node.pData :=@item;
 Result:=_Fetch(@node,True);
end;

function TsrTypeList._FetchFloat(dtype:TsrDataType):PsrType;
var
 pLiteralList:PsrLiteralList;
 node:TsrType;
 item:array[0..0] of PsrNode;
begin
 Result:=nil;
 pLiteralList:=FEmit.GetLiteralList;
 //
 item[0]:=pLiteralList^.FetchLiteral(dtype.BitSize,nil);
 //
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtype;
 node.fsize :=dtype.BitSize div 8;
 node.fOpId :=Op.OpTypeFloat;
 node.fCount:=1;
 node.pData :=@item;
 Result:=_Fetch(@node,True);
end;

function TsrTypeList._FetchStruct2(dtype:TsrDataType):PsrType;
var
 node:TsrType;
 item:array[0..1] of PsrNode;
begin
 Result:=nil;
 item[0]:=Fetch(dtype.Child);
 item[1]:=item[0];
 //
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtype;
 node.fsize :=dtype.BitSize div 8;
 node.fOpId :=Op.OpTypeStruct;
 node.fCount:=2;
 node.pData :=@item;
 Result:=_Fetch(@node,True);
end;

function TsrTypeList._FetchConst(dtype:TsrDataType;Value:QWORD):PsrType;
var
 pLiteralList:PsrLiteralList;
 node:TsrType;
 item:array[0..1] of PsrNode;
begin
 Result:=nil;
 pLiteralList:=FEmit.GetLiteralList;
 //
 item[0]:=Fetch(dtype);
 item[1]:=pLiteralList^.FetchConst(dtype,Value);
 //
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtConstant;
 node.fsize :=dtype.BitSize div 8;
 node.fOpId :=Op.OpConstant;
 node.fCount:=2;
 node.pData :=@item;
 Result:=_Fetch(@node,True);
end;

function TsrTypeList.Fetch(dtype:TsrDataType):PsrType;
begin
 Result:=nil;

 Case dtype of
  dtUnknow:;

  dtBool:
    begin
     Result:=_Fetch0(dtype,Op.OpTypeBool);
    end;

  //

  dtInt8,
  dtUint8,
  dtInt16,
  dtUint16,
  dtInt32,
  dtUint32,
  dtInt64,
  dtUint64:
    begin
     Result:=_FetchInt(dtype);
    end;

  dtHalf16,
  dtFloat32,
  dtFloat64:
    begin
     Result:=_FetchFloat(dtype);
    end;

  dtTypeVoid:
    begin
     Result:=_Fetch0(dtype,Op.OpTypeVoid);
    end;

  dtTypeSampler:
    begin
     Result:=_Fetch0(dtype,Op.OpTypeSampler);
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

  dtVec2h,
  dtVec4h,

  dtVec2f,
  dtVec3f,
  dtVec4f:
   begin
    Result:=_FetchVector(dtype);
   end;

  //

  dtStruct2u:
    begin
     Result:=_FetchStruct2(dtype);
    end;

  //

  else
   Assert(false);
 end;

end;

function TsrTypeList.FetchPointer(child:PsrType;storage_class:DWORD):PsrType;
var
 pLiteralList:PsrLiteralList;
 node:TsrType;
 item:array[0..1] of PsrNode;
begin
 Assert(child<>nil);
 pLiteralList:=FEmit.GetLiteralList;
 //
 item[0]:=pLiteralList^.FetchLiteral(storage_class,PChar(StorageClass.GetStr(storage_class)));
 item[1]:=child;
 //
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtTypePointer;
 node.fOpId :=Op.OpTypePointer;
 node.fCount:=2;
 node.pData :=@item;
 Result:=_Fetch(@node,True);
end;

function TsrTypeList.FetchFunction(ret:PsrType):PsrType;
var
 node:TsrType;
begin
 Assert(ret<>nil);
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtTypeFunction;
 node.fOpId :=Op.OpTypeFunction;
 node.fCount:=1;
 node.pData :=@ret;
 Result:=_Fetch(@node,True);
end;

function TsrTypeList.FetchFunction(copy:Boolean;count:Byte;pData:PPsrType):PsrType;
var
 node:TsrType;
begin
 Assert(count<>0);
 Assert(pData<>nil);
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtTypeFunction;
 node.fOpId :=Op.OpTypeFunction;
 node.fCount:=count;
 node.pData :=Pointer(pData);
 Result:=_Fetch(@node,copy);
end;

function TsrTypeList.FetchStruct(count:Word;pData:PPsrType;copy:Boolean;_size:DWORD):PsrType;
var
 node:TsrType;
begin
 Assert(count<>0);
 Assert(pData<>nil);
 Assert(_size<>0);
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtTypeStruct;
 node.fsize :=_size;
 node.fOpId :=Op.OpTypeStruct;
 node.fCount:=count;
 node.pData :=Pointer(pData);
 Result:=_Fetch(@node,copy);
end;

function TsrTypeList.InsertStruct(count:Word;pData:PPsrType;copy:Boolean;_size:DWORD):PsrType;
var
 node:TsrType;
begin
 Assert(count<>0);
 Assert(pData<>nil);
 Assert(_size<>0);
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtTypeStruct;
 node.fsize :=_size;
 node.fOpId :=Op.OpTypeStruct;
 node.fCount:=count;
 node.pData :=Pointer(pData);
 Result:=_Insert(@node,copy);
end;

function TsrTypeList.FetchArray(child:PsrType;array_count:DWORD):PsrType;
var
 node:TsrType;
 item:array[0..1] of PsrType;
begin
 Assert(child<>nil);
 //
 item[0]:=child;
 item[1]:=_FetchConst(dtUInt32,array_count);
 //
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtTypeArray;
 node.fsize :=child^.fsize*array_count;
 node.fOpId :=Op.OpTypeArray;
 node.fCount:=2;
 node.pData :=@item;
 Result:=_Fetch(@node,True);
end;

function TsrTypeList.FetchRuntimeArray(child:PsrType):PsrType;
var
 node:TsrType;
begin
 Assert(child<>nil);
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtTypeRuntimeArray;
 node.fsize :=DWORD(-1);
 node.fOpId :=Op.OpTypeRuntimeArray;
 node.fCount:=1;
 node.pData :=@child;
 Result:=_Fetch(@node,True);
end;

function Dim_GetStr(w:Word):PChar;
begin
 Result:='';
 Case w of
  Dim.Dim1D      :Result:='1D';
  Dim.Dim2D      :Result:='2D';
  Dim.Dim3D      :Result:='3D';
  Dim.Cube       :Result:='Cube';
  Dim.Rect       :Result:='Rect';
  Dim.Buffer     :Result:='Buffer';
  Dim.SubpassData:Result:='SubpassData';
  else;
 end;
end;

function TsrTypeList.FetchImage(child:PsrType;image_info:TsrTypeImageInfo):PsrType;
var
 pLiteralList:PsrLiteralList;
 node:TsrType;
 item:array[0..6] of PsrNode;
begin
 Assert(child<>nil);
 pLiteralList:=FEmit.GetLiteralList;
 //
 item[0]:=child;
 item[1]:=pLiteralList^.FetchLiteral(image_info.Dim    ,Dim_GetStr(image_info.Dim));
 item[2]:=pLiteralList^.FetchLiteral(image_info.Depth  ,nil);
 item[3]:=pLiteralList^.FetchLiteral(image_info.Arrayed,nil);
 item[4]:=pLiteralList^.FetchLiteral(image_info.MS     ,nil);
 item[5]:=pLiteralList^.FetchLiteral(image_info.Sampled,nil);
 item[6]:=pLiteralList^.FetchLiteral(image_info.Format ,PChar(ImageFormat.GetStr(image_info.Format)));
 //
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtTypeImage;
 node.fOpId :=Op.OpTypeImage;
 node.fCount:=7;
 node.pData :=@item;
 Result:=_Fetch(@node,True);
end;

function TsrTypeList.FetchSampledImage(child:PsrType):PsrType;
var
 node:TsrType;
begin
 Assert(child<>nil);
 node:=Default(TsrType);
 node.Init;
 node.fdtype:=dtTypeSampledImage;
 node.fOpId :=Op.OpTypeSampledImage;
 node.fCount:=1;
 node.pData :=@child;
 Result:=_Fetch(@node,True);
end;

function TsrTypeList.First:PsrType; inline;
begin
 Result:=FList.pHead;
end;

end.

