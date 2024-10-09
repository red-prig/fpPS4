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
 PsrTypeKey=^TsrTypeKey;
 TsrTypeKey=packed record
  fdtype:TsrDataType;
  fsize :DWORD;
  fOpId :WORD;
  fcount:WORD;
  pData :PPsrNode;
 end;

 PPsrType=^TsrType;
 TsrType=class(TsrNode)
  public
   pPrev,pNext,pLeft,pRight:TsrType;
   class function c(n1,n2:PsrTypeKey):Integer; static;
  private
   ID:TsrRefId; //post id
   key:TsrTypeKey;
  public
   //
   Procedure _zero_read   ;               override;
   Procedure _zero_unread ;               override;
   function  _Next        :TsrNode;       override;
   function  _Prev        :TsrNode;       override;
   function  _GetPrintName:RawByteString; override;
   function  _GetRef      :Pointer;       override;
   //
   property  size:DWORD        read key.fsize;
   property  OpId:WORD         read key.fOpId;
   property  count:WORD        read key.fcount;
   property  ItemCount:WORD    read key.fcount;
   function  dtype:TsrDataType;
   function  GetItem(i:Word):TsrNode;
   function  GetDWORD(i:Word):DWORD;
   function  array_stride:DWORD;
   function  is_array_image:Boolean;
   function  array_image_info:TsrTypeImageInfo;
   function  array_count:DWORD;
   function  storage_class:DWORD;
   function  is_image:Boolean;
   function  image_info:TsrTypeImageInfo;
   function  GetPrintName:RawByteString;
 end;

 ntType=TsrType;

 PsrTypeList=^TsrTypeList;
 TsrTypeList=object
  type
   TNodeList=specialize TNodeListClass<TsrType>;
   TNodeTree=specialize TNodeTreeClass<TsrType>;
  var
   FEmit:TCustomEmit;
   FList:TNodeList;
   FTree:TNodeTree;
  Procedure Init(Emit:TCustomEmit);
  function  _Insert(key:PsrTypeKey;copy:Boolean):TsrType;
  function  _Fetch(key:PsrTypeKey;copy:Boolean):TsrType;
  function  _Fetch0(dtype:TsrDataType;OpId:DWORD):TsrType;
  function  _FetchVector(dtype:TsrDataType):TsrType;
  function  _FetchInt(dtype:TsrDataType):TsrType;
  function  _FetchFloat(dtype:TsrDataType):TsrType;
  function  _FetchStruct2(dtype:TsrDataType):TsrType;
  function  _FetchConst(dtype:TsrDataType;Value:QWORD):TsrType;
  function  Fetch(dtype:TsrDataType):TsrType;
  function  FetchPointer(child:TsrType;storage_class:DWORD):TsrType;
  function  FetchFunction(ret:TsrType):TsrType;
  function  FetchFunction(copy:Boolean;count:Byte;pData:PPsrType):TsrType;
  function  FetchStruct (count:Word;pData:PPsrType;copy:Boolean;_size:DWORD):TsrType;
  function  InsertStruct(count:Word;pData:PPsrType;copy:Boolean;_size:DWORD):TsrType;
  function  FetchArray(child:TsrType;array_count:DWORD):TsrType;
  function  FetchRuntimeArray(child:TsrType):TsrType;
  function  FetchImage(child:TsrType;image_info:TsrTypeImageInfo):TsrType;
  function  FetchSampledImage(child:TsrType):TsrType;
  function  First:TsrType; inline;
 end;

operator := (i:TsrNode):TsrType; inline;

implementation

operator := (i:TsrNode):TsrType; inline;
begin
 Result:=TsrType(Pointer(i)); //typecast hack
end;

//

Procedure TsrType._zero_read;
var
 i:DWORD;
begin
 if (count<>0) then
  For i:=0 to count-1 do
  begin
   GetItem(i).mark_read(Self);
  end;
end;

Procedure TsrType._zero_unread;
var
 i:DWORD;
begin
 if (count<>0) then
  For i:=0 to count-1 do
  begin
   GetItem(i).mark_unread(Self);
  end;
end;

function TsrType._Next:TsrNode;
begin
 Result:=pNext;
end;

function TsrType._Prev:TsrNode;
begin
 Result:=pPrev;
end;

function TsrType._GetPrintName:RawByteString;
begin
 Result:=GetPrintName;
end;

function TsrType._GetRef:Pointer;
begin
 Result:=@ID;
end;

//

class function TsrType.c(n1,n2:PsrTypeKey):Integer;
begin
 //first OpId
 Result:=ord(n1^.fOpId>n2^.fOpId)-ord(n1^.fOpId<n2^.fOpId);
 if (Result<>0) then Exit;
 //second fCount
 Result:=ord(n1^.fCount>n2^.fCount)-ord(n1^.fCount<n2^.fCount);
 if (Result<>0) then Exit;
 //third pData (order sort)
 Result:=CompareNodes(n1^.pData,n2^.pData,n1^.fCount);
end;

function TsrType.dtype:TsrDataType;
begin
 Result:=dtUnknow;
 if (Self=nil) then Exit;
 Result:=key.fdtype;
end;

function TsrType.GetItem(i:Word):TsrNode;
begin
 if (i>Count) then Exit(nil);
 Result:=key.pData[i];
end;

function TsrType.GetDWORD(i:Word):DWORD;
var
 pCount:TsrLiteral;
begin
 Result:=0;
 if (i>Count) then Exit;
 pCount:=key.pData[i].specialize AsType<ntLiteral>;
 if (pCount=nil) then Exit;
 Result:=pCount.Value;
end;

function TsrType.array_stride:DWORD;
var
 child:TsrType;
begin
 Result:=0;

 if not (dtype in [dtTypeArray,dtTypeRuntimeArray]) then Exit;

 child:=GetItem(0).specialize AsType<ntType>;
 if (child=nil) then Exit;

 Result:=child.size;
end;

function TsrType.is_array_image:Boolean;
var
 child:TsrType;
begin
 Result:=False;

 if not (dtype in [dtTypeArray,dtTypeRuntimeArray]) then Exit;

 child:=GetItem(0).specialize AsType<ntType>;
 if (child=nil) then Exit;

 Result:=child.is_image;
end;

function TsrType.array_image_info:TsrTypeImageInfo;
var
 child:TsrType;
begin
 Result:=Default(TsrTypeImageInfo);

 if not (dtype in [dtTypeArray,dtTypeRuntimeArray]) then Exit;

 child:=GetItem(0).specialize AsType<ntType>;
 if (child=nil) then Exit;

 Result:=child.image_info;
end;

function TsrType.array_count:DWORD;
var
 pConst:TsrType;
begin
 Result:=0;
 if (dtype<>dtTypeArray) then Exit;
 pConst:=GetItem(1).specialize AsType<ntType>;
 if (pConst=nil) then Exit;
 Result:=pConst.GetDWORD(1);
end;

function TsrType.storage_class:DWORD;
begin
 Result:=0;
 if (dtype<>dtTypePointer) then Exit;
 Result:=GetDWORD(0);
end;

function TsrType.is_image:Boolean;
begin
 Result:=(dtype=dtTypeImage);
end;

function TsrType.image_info:TsrTypeImageInfo;
begin
 Result:=Default(TsrTypeImageInfo);
 if (dtype<>dtTypeImage) then Exit;
 if (Count<>7) then Exit;
 //
 Result.Dim    :=GetDWORD(1);
 Result.Depth  :=GetDWORD(2);
 Result.Arrayed:=GetDWORD(3);
 Result.MS     :=GetDWORD(4);
 Result.Sampled:=GetDWORD(5);
 Result.Format :=GetDWORD(6);
end;

function type_get_base_name2(node:TsrType):RawByteString;
var
 R:PsrRefId;
 child:TsrType;
begin
 Result:='';
 case node.dtype of
  dtTypeImage:
    begin
     R:=node.GetRef;
     Assert(R<>nil  ,'type_get_base_name2$1');
     Assert(R^.Alloc,'type_get_base_name2$2');
     Result:='ti'+IntToStr(R^.ID);
    end;
  dtTypeSampledImage:
    begin
     child:=node.GetItem(0).specialize AsType<ntType>;
     if (child=nil) then Exit;
     Result:=type_get_base_name2(child);
     if (Result='') then Exit;
     Result:='tm'+Result;
    end;
  dtTypeArray:
    begin
     child:=node.GetItem(0).specialize AsType<ntType>;
     if (child=nil) then Exit;
     Result:=type_get_base_name2(child);
     if (Result='') then Exit;
     Result:='ta'+Result+IntToStr(node.array_count);
    end;
  dtTypeRuntimeArray:
    begin
     child:=node.GetItem(0).specialize AsType<ntType>;
     if (child=nil) then Exit;
     Result:=type_get_base_name2(child);
     if (Result='') then Exit;
     Result:='tr'+Result;
    end;
  dtTypeStruct:
    begin
     Assert(node.ID.Alloc);
     Result:='ts'+IntToStr(node.ID.ID);
    end;
  dtTypeFunction:
    begin
     child:=node.GetItem(0).specialize AsType<ntType>;
     if (child=nil) then Exit;
     Result:=type_get_base_name2(child);
     if (Result='') then Exit;
     Result:='tf'+Result;
    end;
  dtConstant:
    begin
     Result:='tc'+IntToStr(node.GetDWORD(1));
    end;
  else
    Result:=type_get_base_name1(node.dtype);
 end;
end;

function type_get_base_name3(node:TsrType):RawByteString;
var
 child:TsrType;
begin
 case node.dtype of
  dtTypePointer:
    begin
     child:=node.GetItem(1).specialize AsType<ntType>;
     if (child=nil) then Exit;
     Result:=type_get_base_name2(child);
     if (Result='') then Exit;
     Case node.storage_class of
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
 Result:=type_get_base_name3(Self);
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

function TsrTypeList._Insert(key:PsrTypeKey;copy:Boolean):TsrType;
var
 size:ptruint;
begin
 Result:=FEmit.specialize New<TsrType>;
 Result.key:=key^;

 if copy and (key^.fCount<>0) then
 begin
  size:=SizeOf(Pointer)*key^.fCount;
  Result.key.pData:=FEmit.Alloc(size);
  Move(key^.pData^,Result.key.pData^,Size);
 end;

 FList.Push_tail(Result);
end;

function TsrTypeList._Fetch(key:PsrTypeKey;copy:Boolean):TsrType;
begin
 Result:=FTree.Find(key);
 if (Result=nil) then
 begin
  Result:=_Insert(key,copy);
  FTree.Insert(Result);
 end;
end;

function TsrTypeList._Fetch0(dtype:TsrDataType;OpId:DWORD):TsrType;
var
 key:TsrTypeKey;
begin
 Result:=nil;
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtype;
 key.fsize :=dtype.BitSize div 8;
 key.fOpId :=OpId;
 //
 Result:=_Fetch(@key,True);
end;

function TsrTypeList._FetchVector(dtype:TsrDataType):TsrType;
var
 pLiteralList:PsrLiteralList;
 key:TsrTypeKey;
 item:array[0..1] of TsrNode;
begin
 Result:=nil;
 pLiteralList:=FEmit.GetLiteralList;
 //
 item[0]:=Fetch(dtype.Child);
 item[1]:=pLiteralList^.FetchLiteral(dtype.Count,nil);
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtype;
 key.fsize :=dtype.BitSize div 8;
 key.fOpId :=Op.OpTypeVector;
 key.fCount:=2;
 key.pData :=@item;
 //
 Result:=_Fetch(@key,True);
end;

function TsrTypeList._FetchInt(dtype:TsrDataType):TsrType;
var
 pLiteralList:PsrLiteralList;
 key:TsrTypeKey;
 item:array[0..1] of TsrNode;
begin
 Result:=nil;
 pLiteralList:=FEmit.GetLiteralList;
 //
 item[0]:=pLiteralList^.FetchLiteral(dtype.BitSize,nil);
 item[1]:=pLiteralList^.FetchLiteral(dtype.Sign   ,nil);
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtype;
 key.fsize :=dtype.BitSize div 8;
 key.fOpId :=Op.OpTypeInt;
 key.fCount:=2;
 key.pData :=@item;
 //
 Result:=_Fetch(@key,True);
end;

function TsrTypeList._FetchFloat(dtype:TsrDataType):TsrType;
var
 pLiteralList:PsrLiteralList;
 key:TsrTypeKey;
 item:array[0..0] of TsrNode;
begin
 Result:=nil;
 pLiteralList:=FEmit.GetLiteralList;
 //
 item[0]:=pLiteralList^.FetchLiteral(dtype.BitSize,nil);
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtype;
 key.fsize :=dtype.BitSize div 8;
 key.fOpId :=Op.OpTypeFloat;
 key.fCount:=1;
 key.pData :=@item;
 //
 Result:=_Fetch(@key,True);
end;

function TsrTypeList._FetchStruct2(dtype:TsrDataType):TsrType;
var
 key:TsrTypeKey;
 item:array[0..1] of TsrNode;
begin
 Result:=nil;
 item[0]:=Fetch(dtype.Child);
 item[1]:=item[0];
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtype;
 key.fsize :=dtype.BitSize div 8;
 key.fOpId :=Op.OpTypeStruct;
 key.fCount:=2;
 key.pData :=@item;
 //
 Result:=_Fetch(@key,True);
end;

function TsrTypeList._FetchConst(dtype:TsrDataType;Value:QWORD):TsrType;
var
 pLiteralList:PsrLiteralList;
 key:TsrTypeKey;
 item:array[0..1] of TsrNode;
begin
 Result:=nil;
 pLiteralList:=FEmit.GetLiteralList;
 //
 item[0]:=Fetch(dtype);
 item[1]:=pLiteralList^.FetchConst(dtype,Value);
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtConstant;
 key.fsize :=dtype.BitSize div 8;
 key.fOpId :=Op.OpConstant;
 key.fCount:=2;
 key.pData :=@item;
 //
 Result:=_Fetch(@key,True);
end;

function TsrTypeList.Fetch(dtype:TsrDataType):TsrType;
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

  dtStruct2i,
  dtStruct2u,
  dtStruct2i64,
  dtStruct2u64:
    begin
     Result:=_FetchStruct2(dtype);
    end;
  //

  else
   Assert(false);
 end;

end;

function TsrTypeList.FetchPointer(child:TsrType;storage_class:DWORD):TsrType;
var
 pLiteralList:PsrLiteralList;
 key:TsrTypeKey;
 item:array[0..1] of TsrNode;
begin
 Assert(child<>nil);
 pLiteralList:=FEmit.GetLiteralList;
 //
 item[0]:=pLiteralList^.FetchLiteral(storage_class,PChar(StorageClass.GetStr(storage_class)));
 item[1]:=child;
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtTypePointer;
 key.fOpId :=Op.OpTypePointer;
 key.fCount:=2;
 key.pData :=@item;
 //
 Result:=_Fetch(@key,True);
end;

function TsrTypeList.FetchFunction(ret:TsrType):TsrType;
var
 key:TsrTypeKey;
begin
 Assert(ret<>nil);
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtTypeFunction;
 key.fOpId :=Op.OpTypeFunction;
 key.fCount:=1;
 key.pData :=@ret;
 //
 Result:=_Fetch(@key,True);
end;

function TsrTypeList.FetchFunction(copy:Boolean;count:Byte;pData:PPsrType):TsrType;
var
 key:TsrTypeKey;
begin
 Assert(count<>0);
 Assert(pData<>nil);
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtTypeFunction;
 key.fOpId :=Op.OpTypeFunction;
 key.fCount:=count;
 key.pData :=Pointer(pData);
 //
 Result:=_Fetch(@key,copy);
end;

function TsrTypeList.FetchStruct(count:Word;pData:PPsrType;copy:Boolean;_size:DWORD):TsrType;
var
 key:TsrTypeKey;
begin
 Assert(count<>0);
 Assert(pData<>nil);
 Assert(_size<>0);
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtTypeStruct;
 key.fsize :=_size;
 key.fOpId :=Op.OpTypeStruct;
 key.fCount:=count;
 key.pData :=Pointer(pData);
 //
 Result:=_Fetch(@key,copy);
end;

function TsrTypeList.InsertStruct(count:Word;pData:PPsrType;copy:Boolean;_size:DWORD):TsrType;
var
 key:TsrTypeKey;
begin
 Assert(count<>0);
 Assert(pData<>nil);
 Assert(_size<>0);
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtTypeStruct;
 key.fsize :=_size;
 key.fOpId :=Op.OpTypeStruct;
 key.fCount:=count;
 key.pData :=Pointer(pData);
 //
 Result:=_Insert(@key,copy);
end;

function TsrTypeList.FetchArray(child:TsrType;array_count:DWORD):TsrType;
var
 key:TsrTypeKey;
 item:array[0..1] of TsrType;
begin
 Assert(child<>nil);
 //
 item[0]:=child;
 item[1]:=_FetchConst(dtUInt32,array_count);
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtTypeArray;
 key.fsize :=child.size*array_count;
 key.fOpId :=Op.OpTypeArray;
 key.fCount:=2;
 key.pData :=@item;
 //
 Result:=_Fetch(@key,True);
end;

function TsrTypeList.FetchRuntimeArray(child:TsrType):TsrType;
var
 key:TsrTypeKey;
begin
 Assert(child<>nil);
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtTypeRuntimeArray;
 key.fsize :=DWORD(-1);
 key.fOpId :=Op.OpTypeRuntimeArray;
 key.fCount:=1;
 key.pData :=@child;
 //
 Result:=_Fetch(@key,True);
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

function TsrTypeList.FetchImage(child:TsrType;image_info:TsrTypeImageInfo):TsrType;
var
 pLiteralList:PsrLiteralList;
 key:TsrTypeKey;
 item:array[0..6] of TsrNode;
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
 key:=Default(TsrTypeKey);
 key.fdtype:=dtTypeImage;
 key.fOpId :=Op.OpTypeImage;
 key.fCount:=7;
 key.pData :=@item;
 //
 Result:=_Fetch(@key,True);
end;

function TsrTypeList.FetchSampledImage(child:TsrType):TsrType;
var
 key:TsrTypeKey;
begin
 Assert(child<>nil);
 //
 key:=Default(TsrTypeKey);
 key.fdtype:=dtTypeSampledImage;
 key.fOpId :=Op.OpTypeSampledImage;
 key.fCount:=1;
 key.pData :=@child;
 //
 Result:=_Fetch(@key,True);
end;

function TsrTypeList.First:TsrType; inline;
begin
 Result:=FList.pHead;
end;

end.

