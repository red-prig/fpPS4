unit srBuffer;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 spirv,
 ginodes,
 srNode,
 srType,
 srTypes,
 srVariable,
 srLayout,
 srDecorate,
 srConfig;

type
 TsrBuffer=class;

 TsrField=class;

 TsrFieldValue=(frNotFit,frIdent,frVectorAsValue,frValueInVector,frValueInArray);

 TFieldFetchValue=record
  fValue:TsrFieldValue;
  pField:TsrField;
 end;

 TFieldEnumCb=procedure(pField:TsrField) of object;
 TsrField=class
  type
   TFieldTree=specialize TNodeTreeClass<TsrField>;
  var
   pLeft,pRight:TsrField;
   //----

   pBuffer:TsrBuffer;
   pParent:TsrField;
   //
   key:PtrUint;  //offset
   //
   FFSize :PtrUint; //full size of struct/array (cached)
   stride :PtrUint; //value size/stride in array
   count  :PtrUint; //count in array

   FCount:PtrUint;     //field count
   Fdtype:TsrDataType; //field type

   sType,vType:TsrType;

   FID:Integer; //alloc late

   //

   FList:TFieldTree;

  class function c(n1,n2:PPtrUint):Integer; static;
  property  offset:PtrUint read key write key;
  function  Cross(o,s:PtrUint):Boolean;
  function  Find_be(o:PtrUint):TsrField;
  function  Find_le(o:PtrUint):TsrField;
  function  Find_ls(o:PtrUint):TsrField;
  function  First:TsrField;
  function  Last:TsrField;
  function  Next(p:TsrField):TsrField;
  function  Prev(p:TsrField):TsrField;
  function  Fetch(o:PtrUint):TsrField;
  Procedure ClearSize;
  function  FindIntersect(_offset,_size:PtrUint):TsrField;
  function  FetchValue(_offset,_size:PtrUint;_dtype:TsrDataType;_weak:Boolean):TFieldFetchValue;
  function  FetchArray(_offset,_size,_stride:PtrUint):TFieldFetchValue;
  function  FetchRuntimeArray(_offset,_stride:PtrUint):TFieldFetchValue;
  function  IsStructUsedRuntimeArray:Boolean;
  function  IsStructNotUsed:Boolean;
  function  IsTop:Boolean;
  function  GetStructDecorate:DWORD;
  procedure UpdateSize;
  function  Size:PtrUint;
  procedure FillNode(o,s:PtrUint);
  function  FillSpace:Integer;
 end;

 TsrBufferType=(btStorageBuffer,btUniformBuffer,btPushConstant,btWorkgroup,btPrivate);

 PsrBufferKey=^TsrBufferKey;
 TsrBufferKey=packed record
  pLayout:TsrDataLayout;
  AliasId:PtrInt;
 end;

 TsrBuffer=class(TsrDescriptor)
  pLeft,pRight:TsrBuffer;
  //----
  //
  align_offset:DWORD;

  FDList:TDependenceNodeList;

  bType:TsrBufferType;

  key:TsrBufferKey;

  FTop:TsrField;

  pNextAlias:TsrBuffer; // alias cache

  tRef:TsrNode; //bitcast cache

  //
  Procedure add_read(src:TsrNode);         override;
  Procedure rem_read(src:TsrNode);         override;
  //
  function  _GetStorageName:RawByteString; override;
  //
  class function c(n1,n2:PsrBufferKey):Integer; static;
  //
  property  pLayout:TsrDataLayout read key.pLayout;
  property  AliasId:PtrInt        read key.AliasId;
  //
  Function  is_export_used:Boolean;
  Procedure Init(); inline;
  Procedure AddDep(t:TsrNode);
  Procedure RemDep(t:TsrNode);
  function  _chain_read:DWORD;
  function  chain_read :DWORD;
  function  _chain_write:DWORD;
  function  chain_write:DWORD;
  function  GetStorageName:RawByteString;
  function  GetTypeChar:Char;
  function  GetRw:Char;
  function  GetString:RawByteString;
  function  GetStructName:RawByteString;
  function  GetSize:PtrUint;
  procedure EnumAllField(cb:TFieldEnumCb);
  procedure ShiftOffset(Offset:PtrUint);
 end;

 ntBuffer=TsrBuffer;

 PsrBufferList=^TsrBufferList;
 TsrBufferList=object
  type
   TNodeTree=specialize TNodeTreeClass<TsrBuffer>;
  var
   FEmit:TCustomEmit;
   FTree:TNodeTree;
   FPushConstant:TsrBuffer;
  procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(s:TsrDataLayout;n:PtrInt;GLC,SLC:Boolean):TsrBuffer;
  function  NextAlias(buf:TsrBuffer;GLC,SLC:Boolean):TsrBuffer;
  Function  First:TsrBuffer;
  Function  Next(node:TsrBuffer):TsrBuffer;
  procedure EnumAllField(cb:TFieldEnumCb);
  procedure OnFillSpace(node:TsrField);
  procedure FillSpace;
  procedure OnAllocID(pField:TsrField);
  procedure AllocID;
  procedure AllocBinding(Var FBinding:Integer);
  procedure AllocSourceExtension;
  function  FindUserDataBuf:TsrBuffer;
  procedure ApplyBufferType;
  procedure AlignOffset(node:TsrBuffer;offset:PtrUint);
  procedure AlignOffset;
  procedure OnAllocTypeBinding(pField:TsrField);
  procedure AllocTypeBinding;
  procedure AllocName;
 end;

operator := (i:TObject):TsrField; inline;

implementation

operator := (i:TObject):TsrField; inline;
begin
 Result:=TsrField(Pointer(i)); //typecast hack
end;

Procedure TsrBuffer.add_read(src:TsrNode);
begin
 inherited;
 if src.IsType(ntChain) then
 begin
  AddDep(src);
 end;
end;

Procedure TsrBuffer.rem_read(src:TsrNode);
begin
 inherited;
 if src.IsType(ntChain) then
 begin
  RemDep(src);
 end;
end;

function TsrBuffer._GetStorageName:RawByteString;
begin
 Result:=GetStorageName;
end;

//---

class function TsrField.c(n1,n2:PPtrUint):Integer;
begin
 Result:=ord(n1^>n2^)-ord(n1^<n2^);
end;

function TsrField.Cross(o,s:PtrUint):Boolean;
begin
 Result:=((o>=offset) and (o<(offset+size))) or
         ((offset>=o) and (offset<(o+s)));
end;

function TsrField.Find_be(o:PtrUint):TsrField;
begin
 Result:=FList.Find_be(@o);
end;

function TsrField.Find_le(o:PtrUint):TsrField;
begin
 Result:=FList.Find_le(@o);
end;

function TsrField.Find_ls(o:PtrUint):TsrField;
begin
 Result:=FList.Find_ls(@o);
end;

function TsrField.First:TsrField;
begin
 Result:=FList.Min;
end;

function TsrField.Last:TsrField;
begin
 Result:=FList.Max;
end;

function TsrField.Next(p:TsrField):TsrField;
begin
 Result:=FList.Next(p);
end;

function TsrField.Prev(p:TsrField):TsrField;
begin
 Result:=FList.Prev(p);
end;

function TsrField.Fetch(o:PtrUint):TsrField;
begin
 Result:=FList.Find(@o);
 if (Result=nil) then
 begin
  Assert(pBuffer.Emit<>nil);
  Result:=pBuffer.Emit.specialize New<TsrField>;
  Result.pBuffer:=pBuffer;
  Result.pParent:=Self;
  Result.offset :=o;
  Result.FID    :=-1;
  FList.Insert(Result);
  //inc field count
  Inc(FCount);
  //clear parent size
  ClearSize;
 end;
end;

Procedure TsrField.ClearSize;
var
 node:TsrField;
begin
 node:=Self;
 while (node<>nil) do
 begin
  //update only struct
  if (node.Fdtype<>dtTypeStruct) then Exit;
  //dont clear max size
  if (node.FFSize=High(PtrUint)) then Exit;
  //
  node.FFSize:=0;
  //
  node:=node.pParent;
 end;
end;

function TsrField.FindIntersect(_offset,_size:PtrUint):TsrField;
var
 node:TsrField;
begin
 Result:=nil;
 node:=Find_ls(_offset+_size);
 //
 while (node<>nil) do
 begin
  if node.Cross(_offset,_size) then
  begin
   Exit(node);
  end;
  //
  node:=Prev(node);
 end;
end;

function TsrField.FetchValue(_offset,_size:PtrUint;_dtype:TsrDataType;_weak:Boolean):TFieldFetchValue;
var
 node:TsrField;
 _stride:PtrUint;
begin
 Result:=Default(TFieldFetchValue);

 if _dtype.isVector then
 begin
  _stride:=_dtype.Child.BitSize div 8;
 end else
 begin
  _stride:=_dtype.BitSize div 8;
 end;

 Assert(_size=(_dtype.BitSize div 8));

 //find intersec
 node:=FindIntersect(_offset,_size);

 if (node=nil) then
 begin
  //new
  node:=Fetch(_offset);
  node.FFSize:=_size;   //fixed size
  node.stride:=_stride;
  node.count :=1;
  node.Fdtype:=_dtype;
  //
  Result.fValue:=frIdent;
  Result.pField:=node;
 end else
 Case node.Fdtype of
  dtTypeArray:
   begin
    if (node.offset>_offset) then Exit; //ident or big than
    if ((node.offset+node.size)<(_offset+_size)) then Exit;
    if (node.stride<_size) then Exit;   //ident or min stride
    //
    Result.fValue:=frValueInArray;
    Result.pField:=node;
   end;
  dtTypeRuntimeArray:
   begin
    if (node.offset>_offset) then Exit; //ident or big than
    if (node.stride<_size) then Exit;   //ident or min stride
    //
    Result.fValue:=frValueInArray;
    Result.pField:=node;
   end;
  else
   begin
    if node.Fdtype.isVector then
    begin //ftVector
     if _dtype.isVector then
     begin
      if (node.offset=_offset) and
         (node.size  =_size)   and
         (node.stride=_stride) then
      begin
       Result.fValue:=frIdent; //ident
       Result.pField:=node;
      end;
     end else
     begin
      if (node.offset>_offset) then Exit; //ident or big than
      _offset:=_offset-node.offset;

      if (node.stride=_size) and
         (_offset mod node.stride=0) then
      begin
       Result.fValue:=frValueInVector; //down to vector
       Result.pField:=node;
      end;
     end;
    end else
    begin //ftValue
     if _dtype.isVector then
     begin
      if (node.offset=_offset) and
         (node.size  =_size)   then
      begin
       Result.fValue:=frVectorAsValue; //vector as value?
       Result.pField:=node;
      end;
     end else
     begin
      if (node.offset=_offset) and
         (node.size  =_size)   then
      begin
       Result.fValue:=frIdent; //ident
       Result.pField:=node;
      end;
     end;
    end;
   end;
 end;

 if not _weak then
 begin
  case Result.fValue of
   frIdent:
     begin
      if (node.Fdtype<>_dtype) then
      begin
       Result:=Default(TFieldFetchValue);
      end;
     end;
   frValueInVector:
     begin
      if (node.Fdtype.Child<>_dtype) then
      begin
       Result:=Default(TFieldFetchValue);
      end;
     end;
   else;
  end;
 end;
 //
end;

function TsrField.FetchArray(_offset,_size,_stride:PtrUint):TFieldFetchValue;
var
 node:TsrField;
 _count:PtrUint;

 max_old:PtrUint;
 max_new:PtrUint;
begin
 Result:=Default(TFieldFetchValue);

 _count:=_size div _stride;
 _size :=_count*_stride; //align

 //find intersec
 node:=FindIntersect(_offset,_size);

 if (node=nil) then
 begin
  //new
  node:=Fetch(_offset);
  node.FFSize:=_size; //fixed size
  node.stride:=_stride;
  node.count :=_count;
  node.Fdtype:=dtTypeArray;
  //
  Result.fValue:=frValueInArray;
  Result.pField:=node;
 end else
 Case node.Fdtype of
  dtTypeArray:
   begin
    if (node.offset>_offset) then Exit; //ident or big than
    if (node.stride<>_stride) then Exit; //ident stride
    //
    max_old:=node.offset+node.size;
    max_new:=_offset+_size;
    //
    if (max_old<max_new) then
    begin
     //expand?
     if FindIntersect(max_old,max_new-max_old)<>nil then
     begin
      //dont expand
      //not fit
      Exit;
     end;
     //
     _size:=max_new-node.offset;
     _size:=Align(_size,_stride); //align up
     _count:=_size div _stride;
     //
     node.FFSize:=_size; //fixed size
     node.count :=_count;
    end;
    //
    Result.fValue:=frValueInArray;
    Result.pField:=node;
   end;
  dtTypeRuntimeArray:
   begin
    if (node.offset>_offset) then Exit; //ident or big than
    if (node.stride<>_stride) then Exit; //ident stride
    //
    Result.fValue:=frValueInArray;
    Result.pField:=node;
   end;
  else;
 end;
end;

function TsrField.FetchRuntimeArray(_offset,_stride:PtrUint):TFieldFetchValue;
var
 node:TsrField;
begin
 Result:=Default(TFieldFetchValue);

 node:=Find_be(_offset); //RA only last item
 if (node<>nil) then
 begin
  if (node.offset>_offset) or
     (node.Fdtype<>dtTypeRuntimeArray) or
     (node.stride<>_stride) then
  begin
   //not fit
   Exit;
  end;
 end;

 //find intersec
 node:=FindIntersect(_offset,High(PtrUint)-offset);

 if (node=nil) then
 begin
  //new
  node:=Fetch(_offset);
  node.FFSize:=High(PtrUint); //fixed size
  node.stride:=_stride;
  node.count :=node.FFSize div _stride;
  node.Fdtype :=dtTypeRuntimeArray;
  //
  Result.fValue:=frValueInArray;
  Result.pField:=node;
 end else
 if (node.stride=_stride) and   //ident stride
    (node.Fdtype=dtTypeRuntimeArray) and
    (node.offset<=_offset) then //ident or big than
 begin
  Result.fValue:=frValueInArray;
  Result.pField:=node;
 end;
end;

function TsrField.IsStructUsedRuntimeArray:Boolean;
var
 node:TsrField;
begin
 Result:=False;
 if (Fdtype=dtTypeStruct) then
 begin
  node:=FList.Max;
  if (node<>nil) then
  begin
   Result:=(node.Fdtype=dtTypeRuntimeArray);
  end;
 end;
end;

function TsrField.IsStructNotUsed:Boolean;
var
 child:TsrField;
begin
 Result:=False;

 if IsTop then Exit;
 if (FCount>1) then Exit;

 child:=First;
 if (child=nil) then Exit;

 if (child.offset<>0) then Exit;
 if (child.size<>stride) then Exit;

 Result:=True;
end;

function TsrField.IsTop:Boolean;
begin
 Result:=(pParent=nil);
end;

function TsrField.GetStructDecorate:DWORD;
begin
 Result:=DWORD(-1); //dont use
 if IsTop then
 if (Fdtype=dtTypeStruct) then  //is struct
 begin

  if (pBuffer.FStorage<>StorageClass.StorageBuffer) and
     (pBuffer.bType=btStorageBuffer) then
  begin
   Result:=Decoration.BufferBlock;
  end else
  begin
   Result:=Decoration.Block;
  end;

 end;
end;

procedure TsrField.UpdateSize;
var
 node:TsrField;
begin
 if (FFSize<>0) then Exit;
 //
 if Fdtype.isVector then
 begin
  FFSize:=Fdtype.BitSize div 8
 end else
 if (Fdtype=dtTypeArray) then
 begin
  //array
  FFSize:=count*stride;
 end else
 if (Fdtype=dtTypeRuntimeArray) then
 begin
  //runtame array
  FFSize:=High(Ptruint);
 end else
 begin
  //struct
  node:=FList.Max;
  if (node<>nil) then
  begin
   FFSize:=node.size;
   //check max
   if (FFSize=High(PtrUint)) then Exit;
   FFSize:=node.offset+FFSize;
  end;
 end;
end;

function TsrField.Size:PtrUint;
begin
 UpdateSize;
 Result:=FFSize;
end;

procedure TsrField.FillNode(o,s:PtrUint);

 procedure _Pad(p,v:PtrUint;_dtype:TsrDataType); //inline;
 var
  node:TsrField;
 begin
  if (o mod p<>0) and (s>=v) then
  begin
   node:=Fetch(o);
   Assert(node.Fdtype=dtUnknow,'WTF');
   node.FFSize:=v; //fixed size
   node.stride:=v;
   if _dtype.isVector then
   begin
    node.stride:=_dtype.Child.BitSize div 8;
   end;
   node.Fdtype:=_dtype;
   o:=o+v;
   s:=s-v;
  end;
 end;

 procedure _Fill(v:PtrUint;_dtype:TsrDataType); //inline;
 var
  count:PtrUint;
  node:TsrField;
 begin
  count:=s div v;
  While (count<>0) do
  begin
   node:=Fetch(o);
   Assert(node.Fdtype=dtUnknow,'WTF');
   node.FFSize:=v; //fixed size
   node.stride:=v;
   if _dtype.isVector then
   begin
    node.stride:=_dtype.Child.BitSize div 8;
   end;
   node.Fdtype:=_dtype;
   o:=o+v;
   s:=s-v;
   Dec(count);
  end;
 end;

begin
 if (s=0) then Exit;

 _Pad ( 2,1,dtUint8);
 _Pad ( 4,2,dtHalf16);
 _Pad ( 8,4,dtFloat32);
 _Pad (16,8,dtVec2f);

 _Fill(16,dtVec4f);
 _Fill(8 ,dtVec2f);
 _Fill(4 ,dtFloat32);
 _Fill(2 ,dtHalf16);
 _Fill(1 ,dtUint8);
end;

function TsrField.FillSpace:Integer;
var
 pNode:TsrField;
 Foffset,Fsize:PtrUint;
begin
 Result:=0;
 pNode:=First;
 if (pNode=nil) then Exit;

 Foffset:=0;
 While (pNode<>nil) do
 begin

  if (pNode.Fdtype=dtUnknow) then
  begin
   Case pNode.size of
     1:pNode.Fdtype:=dtUint8;
     2:pNode.Fdtype:=dtHalf16;
     4:pNode.Fdtype:=dtFloat32;
     8:pNode.Fdtype:=dtVec2f;
    16:pNode.Fdtype:=dtVec4f;
    else
     Assert(false);
   end;
  end;

  if (Foffset<pNode.offset) then
  begin
   Fsize:=pNode.offset-Foffset;
   FillNode(Foffset,Fsize);
   Inc(Result);
  end;
  Foffset:=pNode.offset+pNode.size;

  pNode:=Next(pNode);
 end;

 if (stride<>0) and (Fdtype in [dtTypeArray,dtTypeRuntimeArray]) then
 begin
  pNode:=FList.Max;

  if (pNode<>nil) then
  begin
   Foffset:=pNode.offset+pNode.size;
   Assert(Foffset<=stride);
   if (Foffset<stride) then
   begin
    Fsize:=stride-Foffset;
    FillNode(Foffset,Fsize);
    Inc(Result);
   end;
  end;
 end;
end;

//--

class function TsrBuffer.c(n1,n2:PsrBufferKey):Integer;
begin
 //first pLayout
 Result:=ord(n1^.pLayout.Order>n2^.pLayout.Order)-ord(n1^.pLayout.Order<n2^.pLayout.Order);
 if (Result<>0) then Exit;
 //second AliasId
 Result:=ord(n1^.AliasId>n2^.AliasId)-ord(n1^.AliasId<n2^.AliasId);
end;

Function TsrBuffer.is_export_used:Boolean;
begin
 Result:=IsUsed;
 if Result then
 begin
  if (pVar=nil) then Exit(false);
  Result:=pVar.IsUsed;
 end;
end;

Procedure TsrBuffer.Init(); inline;
begin
 bType   :=btStorageBuffer;
 FStorage:=StorageClass.Uniform;
 FBinding:=-1;

 FTop:=Emit.specialize New<TsrField>;

 FTop.FID    :=-1;
 FTop.Fdtype :=dtTypeStruct;
 FTop.pBuffer:=Self;
end;

Procedure TsrBuffer.AddDep(t:TsrNode);
var
 node:TDependenceNode;
begin
 if (t=nil) or (Self=nil) then Exit;

 node:=NewDependence;
 node.pNode:=t;

 FDList.Push_head(node);
end;

Procedure TsrBuffer.RemDep(t:TsrNode);
var
 node:TDependenceNode;
begin
 if (t=nil) or (Self=nil) then Exit;
 node:=FDList.pHead;
 While (node<>nil) do
 begin
  if (node.pNode=t) then
  begin
   FDList.Remove(node);

   FreeDependence(node);

   Exit;
  end;
  node:=node.pNext;
 end;
 Assert(false,'not found!');
end;

function TsrBuffer._chain_read:DWORD;
var
 node:TDependenceNode;
begin
 Result:=0;
 node:=FDList.pHead;
 While (node<>nil) do
 begin
  if node.pNode.IsType(ntChain) then
  begin
   Result:=Result+node.pNode.read_count;
  end;
  node:=node.pNext;
 end;
end;

function TsrBuffer.chain_read:DWORD;
var
 node:TsrBuffer;
begin
 if Flags.Bitcast then
 begin
  node:=Self;
  while (node<>nil) do
  begin
   Result:=Result+node._chain_read;
   node:=node.pNextAlias;
  end;
 end else
 begin
  Result:=_chain_read;
 end;
end;

function TsrBuffer._chain_write:DWORD;
var
 node:TDependenceNode;
begin
 Result:=0;
 node:=FDList.pHead;
 While (node<>nil) do
 begin
  if node.pNode.IsType(ntChain) then
  begin
   Result:=Result+node.pNode.write_count;
  end;
  node:=node.pNext;
 end;
end;

function TsrBuffer.chain_write:DWORD;
var
 node:TsrBuffer;
begin
 if Flags.Bitcast then
 begin
  node:=Self;
  while (node<>nil) do
  begin
   Result:=Result+node._chain_write;
   node:=node.pNextAlias;
  end;
 end else
 begin
  Result:=_chain_write;
 end;
end;

function TsrBuffer.GetStorageName:RawByteString;
begin
 Result:='';

 if (pLayout<>nil) then
 begin
  case pLayout.key.rtype of
   rtLDS:
    begin
     Result:='sLds'+IntToStr(FBinding);
     Exit;
    end;
   rtGDS:
    begin
     Result:='sGds'+IntToStr(FBinding);
     Exit;
    end;
   else;
  end;
 end;

 Case bType of
  btStorageBuffer:Result:='sBuf'+IntToStr(FBinding);
  btUniformBuffer:Result:='uBuf'+IntToStr(FBinding);
  btPushConstant :Result:='cBuf';
 end;
end;

function TsrBuffer.GetTypeChar:Char;
begin
 Result:=#0;
 Case bType of
  btStorageBuffer:Result:='S';
  btUniformBuffer:Result:='U';
  btPushConstant :Result:='P';
 end;
end;

function TsrBuffer.GetRw:Char;
begin
 Result:='0';
 if (chain_read<>0) then
 begin
  Result:='1';
 end;
 if (chain_write<>0) then
 begin
  Result:=Char(ord(Result) or ord('2'));
 end;
end;

function TsrBuffer.GetString:RawByteString;
var
 PID:DWORD;
begin
 PID:=0;
 if (pLayout<>nil) then
 begin
  PID:=pLayout.FID;
 end;
 Result:='B'+GetTypeChar+
         ';PID='+HexStr(PID,8)+
         ';BND='+HexStr(FBinding,8)+
         ';LEN='+HexStr(GetSize,8)+
         ';OFS='+HexStr(align_offset,8)+
         ';MRW='+GetRw;
end;

function TsrBuffer.GetStructName:RawByteString;
begin
 Result:='TD'+HexStr(FBinding,8);
end;

function TsrBuffer.GetSize:PtrUint;
begin
 Result:=FTop.size;
end;

procedure TsrBuffer.EnumAllField(cb:TFieldEnumCb);
var
 curr,node:TsrField;
begin
 if (cb=nil) then Exit;
 curr:=FTop;
 node:=curr.First;
 repeat
  While (node<>nil) do
  begin
   if (node.First<>nil) then //child exist
   begin
    curr:=node;
    node:=curr.First;        //down
   end else
   begin
    cb(node);
    node:=curr.Next(node);
   end;
  end;
  cb(curr);
  node:=curr;
  curr:=curr.pParent; //up
  if (curr=nil) then Break;
  node:=curr.Next(node);
 until false;
end;

procedure TsrBuffer.ShiftOffset(Offset:PtrUint);
var
 node:TsrField;
begin
 if (Offset=0) then Exit;
 node:=FTop.Last;
 While (node<>nil) do
 begin
  node.offset:=node.offset+Offset;
  node:=FTop.Prev(node);
 end;
 //
 FTop.ClearSize;
 FTop.UpdateSize;
end;

procedure TsrBufferList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrBufferList.Fetch(s:TsrDataLayout;n:PtrInt;GLC,SLC:Boolean):TsrBuffer;
var
 key:TsrBufferKey;
begin
 key:=Default(TsrBufferKey);
 key.pLayout:=s;
 key.AliasId:=n;
 //
 Result:=FTree.Find(@key);
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrBuffer>;
  Result.Init();
  Result.key:=key;
  //
  Result.InitVar();
  //
  FTree.Insert(Result);
 end;
 //
 if GLC then
 begin
  Result.Flags.Coherent:=True;
 end;
 //
 if SLC then
 begin
  Result.Flags.Volatile:=True;
 end;
end;

function TsrBufferList.NextAlias(buf:TsrBuffer;GLC,SLC:Boolean):TsrBuffer;
begin
 Result:=nil;
 if (buf=nil) then Exit;

 Result:=buf.pNextAlias;
 if (Result=nil) then
 begin
  Result:=Fetch(buf.pLayout,buf.AliasId+1,GLC,SLC);
  //cache
  buf.pNextAlias:=Result;
 end;

 //mark
    buf.Flags.Aliased:=True;
 Result.Flags.Aliased:=True;
end;

Function TsrBufferList.First:TsrBuffer;
begin
 Result:=FTree.Min;
end;

Function TsrBufferList.Next(node:TsrBuffer):TsrBuffer;
begin
 Result:=FTree.Next(node);
end;

procedure TsrBufferList.EnumAllField(cb:TFieldEnumCb);
var
 node:TsrBuffer;
begin
 if (cb=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  if node.IsUsed then
  begin
   node.EnumAllField(cb);
  end;
  node:=Next(node);
 end;
end;

procedure TsrBufferList.OnFillSpace(node:TsrField);
begin
 node.FillSpace;
end;

procedure TsrBufferList.FillSpace;
begin
 EnumAllField(@OnFillSpace);
end;

procedure TsrBufferList.OnAllocID(pField:TsrField);
var
 node:TsrField;
 ID:Integer;
begin
 ID:=0;
 node:=pField.First;
 While (node<>nil) do
 begin
  if pField.Fdtype.IsVector then
  begin
   ID:=node.offset div pField.stride;
   node.FID:=ID;
  end else
  begin
   node.FID:=ID;
   Inc(ID);
  end;
  node:=pField.Next(node);
 end;
end;

procedure TsrBufferList.AllocID;
begin
 EnumAllField(@OnAllocID);
end;

procedure TsrBufferList.AllocBinding(Var FBinding:Integer);
var
 pConfig:PsrConfig;
 pDecorateList:TsrDecorateList;
 node:TsrBuffer;
 FHide:Integer;
begin
 pConfig:=FEmit.GetConfig;
 pDecorateList:=FEmit.GetDecorateList;
 //
 node:=First;
 While (node<>nil) do
 begin
  if node.is_export_used then
  if not (node.bType in [btPushConstant]) then
  if (node.FBinding=-1) then //alloc
  begin
   //
   if not (node.bType in [btWorkgroup,btPrivate]) then
   begin
    pDecorateList.OpDecorate(node.pVar,Decoration.Binding      ,FBinding);
    pDecorateList.OpDecorate(node.pVar,Decoration.DescriptorSet,pConfig^.DescriptorSet);
    //
    if (node.Flags.Coherent) then
    begin
     pDecorateList.OpDecorate(node.pVar,Decoration.Coherent,0);
    end;
    //
    if (node.Flags.Volatile) then
    begin
     pDecorateList.OpDecorate(node.pVar,Decoration.Volatile,0);
    end;
    //next bind id
    node.FBinding:=FBinding;
    Inc(FBinding);
   end; //
   //Aliased need for uniform/storage/workgroup
   if (node.Flags.Aliased) and (not node.Flags.Bitcast) then
   begin
    pDecorateList.OpDecorate(node.pVar,Decoration.Aliased,0);
   end;
  end;
  //
  node:=Next(node);
 end;
 //Alloc hide id
 FHide:=FBinding;
 node:=First;
 While (node<>nil) do
 begin
  if (node.FBinding=-1) then //alloc
  begin
   node.FBinding:=FHide;
   Inc(FHide);
  end;
  node:=Next(node);
 end;
end;

procedure TsrBufferList.AllocSourceExtension;
var
 pDebugInfoList:TsrDebugInfoList;
 node:TsrBuffer;
begin
 pDebugInfoList:=FEmit.GetDebugInfoList;
 node:=First;
 While (node<>nil) do
 begin
  if node.is_export_used then
  if not (node.bType in [btWorkgroup,btPrivate]) then
  begin
   pDebugInfoList.OpSource(node.GetString);
  end;
  //
  node:=Next(node);
 end;
end;

function TsrBufferList.FindUserDataBuf:TsrBuffer;
var
 node:TsrBuffer;
begin
 Result:=nil;
 node:=First;
 While (node<>nil) do
 begin
  if node.IsUsed then
  begin
   if node.pLayout.IsUserData then
   begin
    Exit(node);
   end;
  end;
  node:=Next(node);
 end;
end;

procedure TsrBufferList.ApplyBufferType;
label
 _storage;
var
 pConfig:PsrConfig;
 node:TsrBuffer;
 fchain_write:DWORD;
begin
 pConfig:=FEmit.GetConfig;

 node:=FindUserDataBuf;
 if (node<>nil) and
    (FPushConstant=nil) then
 if (node.bType=btStorageBuffer) and
    (node.chain_write=0) and
    (node.GetSize<=pConfig^.maxPushConstantsSize) then
 begin
  node.bType   :=btPushConstant;
  node.FStorage:=StorageClass.PushConstant;
  FPushConstant :=node;
 end;

 node:=First;
 While (node<>nil) do
 begin
  if node.IsUsed and (node.bType=btStorageBuffer) then
  begin

   fchain_write:=node.chain_write;

   if node.pLayout.IsGlobalDataShare then
   begin
    //global buffer
    goto _storage;
    //
   end else
   if node.pLayout.IsLocalDataShare then
   begin
    //Workgroup? (btWorkgroup) (btPrivate)
    if (FEmit.GetExecutionModel=ExecutionModel.GLCompute) then
    begin
     node.bType   :=btWorkgroup;
     node.FStorage:=StorageClass.Workgroup;
    end else
    begin
     node.bType   :=btPrivate;
     node.FStorage:=StorageClass.Private_;
    end;
    //
   end else
   if (not pConfig^.UseOnlyUserdataPushConst) and
      (FPushConstant=nil) and
      (fchain_write=0) and
      (node.GetSize<=pConfig^.maxPushConstantsSize) then
   begin
    node.bType   :=btPushConstant;
    node.FStorage:=StorageClass.PushConstant;
    FPushConstant :=node;
   end else
   if (fchain_write=0) and
      (node.GetSize<=pConfig^.maxUniformBufferRange) then
   begin
    node.bType   :=btUniformBuffer;
    node.FStorage:=StorageClass.Uniform;
   end else
   begin
    _storage: //
    if pConfig^.CanUseStorageBufferClass then
    begin
     node.FStorage:=StorageClass.StorageBuffer;
    end else
    begin
     node.FStorage:=StorageClass.Uniform;
    end;
   end;

  end;
  node:=Next(node);
 end;
end;

function AlignShift(addr:Pointer;alignment:PtrUInt):PtrUInt; inline;
begin
 if (alignment>1) then
 begin
  Result:=(PtrUInt(addr) mod alignment);
 end else
 begin
  Result:=0;
 end;
end;

procedure TsrBufferList.AlignOffset(node:TsrBuffer;offset:PtrUint);
var
 P:Pointer;
begin
 P:=node.pLayout.GetData;
 offset:=AlignShift(P,offset);
 node.align_offset:=offset;
 node.ShiftOffset(offset);
end;

procedure TsrBufferList.AlignOffset;
var
 pConfig:PsrConfig;
 node:TsrBuffer;
begin
 pConfig:=FEmit.GetConfig;
 node:=First;
 While (node<>nil) do
 begin
  if node.IsUsed then //Dont use is_export_used in this stage
  if not node.pLayout.IsLocalDataShare then
  if not node.pLayout.IsGlobalDataShare then
  begin
   Case node.bType of
    btStorageBuffer:
      begin
       AlignOffset(node,pConfig^.minStorageBufferOffsetAlignment);
      end;
    btUniformBuffer:
      begin
       AlignOffset(node,pConfig^.minUniformBufferOffsetAlignment);
      end;
    btPushConstant:
      begin
       node.align_offset:=pConfig^.PushConstantsOffset;
       node.ShiftOffset(pConfig^.PushConstantsOffset);
      end;
   end;
  end;
  node:=Next(node);
 end;
end;

procedure TsrBufferList.OnAllocTypeBinding(pField:TsrField);
var
 pDecorateList:TsrDecorateList;
 node:TsrField;
 SD:DWORD;
begin
 if (pField.sType=nil) then Exit;
 if (pField.sType.dtype<>dtTypeStruct) then Exit;

 pDecorateList:=FEmit.GetDecorateList;
 SD:=pField.GetStructDecorate;
 if (SD<>DWORD(-1)) then
 begin
  pDecorateList.OpDecorate(pField.sType,SD,0);
 end;
 node:=pField.First;
 While (node<>nil) do
 begin
  pDecorateList.OpMember(pField.sType,node.FID,node.offset);
  node:=pField.Next(node);
 end;
end;

procedure TsrBufferList.AllocTypeBinding;
var
 pDecorateList:TsrDecorateList;
 pHeaderList  :TsrHeaderList;
 Config       :PsrConfig;
 node:TsrBuffer;
begin
 EnumAllField(@OnAllocTypeBinding);
 //
 pDecorateList:=FEmit.GetDecorateList;
 pHeaderList  :=FEmit.GetHeaderList;
 Config       :=FEmit.GetConfig;
 //
 node:=First;
 While (node<>nil) do
 begin
  if node.is_export_used then
  begin
   if (node.bType=btStorageBuffer) then
   begin
    if (node.chain_read=0) then
    begin
     pDecorateList.OpDecorate(node.pVar,Decoration.NonReadable,0);
    end;
    if (node.chain_write=0) then
    begin
     pDecorateList.OpDecorate(node.pVar,Decoration.NonWritable,0);
    end;
   end;
   //
   if (node.bType=btWorkgroup) then
   begin
    if Config^.IsSpv14 then
    begin
     pHeaderList.SPV_KHR_workgroup_memory_explicit_layout;
    end;
   end;
  end; //is_export_used
  //
  node:=Next(node);
 end;
end;

procedure TsrBufferList.AllocName;
var
 FDebugInfo:TsrDebugInfoList;
 node:TsrBuffer;
begin
 FDebugInfo:=FEmit.GetDebugInfoList;
 node:=First;
 While (node<>nil) do
 begin
  if node.IsUsed and (node.FTop.vType<>nil) then
  begin
   FDebugInfo.OpName(node.FTop.vType,node.GetStructName);
  end;
  node:=Next(node);
 end;
end;

end.

