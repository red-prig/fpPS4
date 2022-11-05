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
 srReg,
 srVariable,
 srLayout,
 srDecorate,
 srConfig;

type
 ntBuffer=class(ntDescriptor)
  class Procedure add_read      (node,src:PsrNode);           override;
  class Procedure rem_read      (node,src:PsrNode);           override;
  //
  class Function  pwrite_count  (node:PsrNode):PDWORD;        override;
  class function  GetStorageName(node:PsrNode):RawByteString; override;
 end;

 PsrBuffer=^TsrBuffer;

 PsrField=^TsrField;

 TsrFieldValue=(frNotFit,frIdent,frVectorAsValue,frValueInVector,frValueInArray);

 TFieldFetchValue=record
  fValue:TsrFieldValue;
  pField:PsrField;
 end;

 TFieldEnumCb=procedure(pField:PsrField) of object;
 TsrField=object
  type
   TFieldFetch=specialize TNodeFetch<PsrField,TsrField>;
  var
   pLeft,pRight:PsrField;
   //----

   pBuffer:PsrBuffer;
   pParent:PsrField;
   offset:PtrUint;
   size:PtrUint;
   stride:PtrUint;

   FCount:PtrUint;
   dtype:TsrDataType;
   pType:PsrType;
   FID:Integer; //alloc late

   //

   FList:TFieldFetch;

  function  c(n1,n2:PsrField):Integer; static;
  function  Cross(o,s:PtrUint):Boolean;
  function  Find_be(o:PtrUint):PsrField;
  function  Find_le(o:PtrUint):PsrField;
  function  First:PsrField;
  function  Last:PsrField;
  function  Next(p:PsrField):PsrField;
  function  Prev(p:PsrField):PsrField;
  function  Fetch(o:PtrUint):PsrField;
  function  FetchValue(_offset,_size:PtrUint;_dtype:TsrDataType):TFieldFetchValue;
  function  FetchRuntimeArray(_offset,_stride:PtrUint):TFieldFetchValue;
  function  IsStructUsedRuntimeArray:Boolean;
  function  IsStructNotUsed:Boolean; inline;
  function  IsTop:Boolean; inline;
  function  GetStructDecorate:DWORD;
  procedure UpdateSize;
  function  GetSize:PtrUint;
  procedure FillNode(o,s:PtrUint);
  function  FillSpace:Integer;
 end;

 TsrBufferType=(btStorageBuffer,btUniformBuffer,btPushConstant);

 TsrBuffer=packed object(TsrDescriptor)
  pLeft,pRight:PsrBuffer;
  //----
  fwrite_count:DWORD;
  //
  align_offset:DWORD;

  FEmit:TCustomEmit;
  FDList:TRegDNodeList;

  bType:TsrBufferType;

  key:packed record
   pLayout:PsrDataLayout;
   AliasId:PtrInt;
  end;

  FTop:TsrField;

  function  c(n1,n2:PsrBuffer):Integer; static;
  Procedure Init(Emit:TCustomEmit); inline;
  Procedure AddDep(t:PsrNode);
  Procedure RemDep(t:PsrNode);
  function  chain_read :DWORD;
  function  chain_write:DWORD;
  function  GetStorageName:RawByteString;
  function  GetTypeChar:Char;
  function  GetString:RawByteString;
  function  GetStructName:RawByteString;
  function  GetSize:PtrUint;
  procedure EnumAllField(cb:TFieldEnumCb);
  procedure ShiftOffset(Offset:PtrUint);
 end;

 PsrBufferList=^TsrBufferList;
 TsrBufferList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrBuffer,TsrBuffer>;
  var
   FEmit:TCustomEmit;
   FNTree:TNodeFetch;
   FPushConstant:PsrBuffer;
  procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(s:PsrDataLayout;n:PtrInt):PsrBuffer;
  function  NextAlias(buf:PsrBuffer):PsrBuffer;
  Function  First:PsrBuffer;
  Function  Next(node:PsrBuffer):PsrBuffer;
  procedure EnumAllField(cb:TFieldEnumCb);
  procedure OnFillSpace(node:PsrField);
  procedure FillSpace;
  procedure OnAllocID(pField:PsrField);
  procedure AllocID;
  procedure AllocBinding(Var FBinding:Integer);
  procedure AllocSourceExtension;
  function  FindUserDataBuf:PsrBuffer;
  procedure ApplyBufferType;
  procedure AlignOffset(node:PsrBuffer;offset:PtrUint);
  procedure AlignOffset;
  procedure OnAllocTypeBinding(pField:PsrField);
  procedure AllocTypeBinding;
  procedure AllocName;
 end;

implementation

class Procedure ntBuffer.add_read(node,src:PsrNode);
begin
 inherited;
 if src^.IsType(ntChain) then
 begin
  PsrBuffer(node)^.AddDep(src);
 end;
end;

class Procedure ntBuffer.rem_read(node,src:PsrNode);
begin
 inherited;
 if src^.IsType(ntChain) then
 begin
  PsrBuffer(node)^.RemDep(src);
 end;
end;

//

class Function ntBuffer.pwrite_count(node:PsrNode):PDWORD;
begin
 Result:=@PsrBuffer(node)^.fwrite_count;
end;

class function ntBuffer.GetStorageName(node:PsrNode):RawByteString;
begin
 Result:=PsrBuffer(node)^.GetStorageName;
end;

//---

function TsrField.c(n1,n2:PsrField):Integer;
begin
 Result:=Integer(n1^.offset>n2^.offset)-Integer(n1^.offset<n2^.offset);
end;

function TsrField.Cross(o,s:PtrUint):Boolean;
begin
 Result:=((o>=offset) and (o<offset+size)) or
         ((offset>=o) and (offset<o+s));
end;

function TsrField.Find_be(o:PtrUint):PsrField;
var
 node:TsrField;
begin
 node:=Default(TsrField);
 node.offset:=o;
 Result:=FList.Find_be(@node);
end;

function TsrField.Find_le(o:PtrUint):PsrField;
var
 node:TsrField;
begin
 node:=Default(TsrField);
 node.offset:=o;
 Result:=FList.Find_le(@node);
end;

function TsrField.First:PsrField;
begin
 Result:=FList.Min;
end;

function TsrField.Last:PsrField;
begin
 Result:=FList.Max;
end;

function TsrField.Next(p:PsrField):PsrField;
begin
 Result:=FList.Next(p);
end;

function TsrField.Prev(p:PsrField):PsrField;
begin
 Result:=FList.Prev(p);
end;

function TsrField.Fetch(o:PtrUint):PsrField;
var
 node:TsrField;
begin
 node:=Default(TsrField);
 node.offset:=o;
 Result:=FList.Find(@node);
 if (Result=nil) then
 begin
  Assert(pBuffer^.FEmit<>nil);
  Result:=pBuffer^.FEmit.Alloc(SizeOf(TsrField));
  Result^.pBuffer:=pBuffer;
  Result^.pParent:=@Self;
  Result^.offset :=o;
  Result^.FID    :=-1;
  FList.Insert(Result);
  Inc(FCount);
 end;
end;

function TsrField.FetchValue(_offset,_size:PtrUint;_dtype:TsrDataType):TFieldFetchValue;
var
 node:PsrField;
 _stride:PtrUint;
begin
 Result:=Default(TFieldFetchValue);

 _stride:=0;
 if _dtype.isVector then
 begin
  _stride:=_dtype.Child.BitSize div 8;
 end;

 node:=Find_le(_offset);
 if (node<>nil) then
 begin
  if not node^.Cross(_offset,_size) then
  begin
   node:=Next(node);
   if (node<>nil) then
   begin
    if not node^.Cross(_offset,_size) then
    begin
     node:=nil;
    end;
   end;
  end;
 end;

 if (node=nil) then
 begin
  //new
  node:=Fetch(_offset);
  node^.size  :=_size;
  node^.stride:=_stride;
  node^.dtype :=_dtype;
  Result.fValue:=frIdent;
  Result.pField:=node;
 end else
 Case node^.dtype of
  dtTypeArray:
   begin
    if (node^.offset>_offset) then Exit; //ident or big than
    if (node^.offset+node^.size<_offset+_size) then Exit;
    if (node^.stride<_size) then Exit;   //ident or min stride
    Result.fValue:=frValueInArray;
    Result.pField:=node;
   end;
  dtTypeRuntimeArray:
   begin
    if (node^.offset>_offset) then Exit; //ident or big than
    if (node^.stride<_size) then Exit;   //ident or min stride
    Result.fValue:=frValueInArray;
    Result.pField:=node;

    Result.fValue:=frNotFit;
   end;
  else
   begin
    if node^.dtype.isVector then
    begin //ftVector
     if _dtype.isVector then
     begin
      if (node^.offset=_offset) and
         (node^.size  =_size)   and
         (node^.stride=_stride) then
      begin
       Result.fValue:=frIdent; //ident
       Result.pField:=node;
      end;
     end else
     begin
      if (node^.offset>_offset) then Exit; //ident or big than
      _offset:=_offset-node^.offset;

      if (node^.stride=_size) and
         (_offset mod node^.stride=0) then
      begin
       Result.fValue:=frValueInVector; //down to vector
       Result.pField:=node;
      end;
     end;
    end else
    begin //ftValue
     if _dtype.isVector then
     begin
      if (node^.offset=_offset) and
         (node^.size  =_size)   then
      begin
       Result.fValue:=frVectorAsValue; //vector as value?
       Result.pField:=node;
      end;
     end else
     begin
      if (node^.offset=_offset) and
         (node^.size  =_size)   then
      begin
       Result.fValue:=frIdent; //ident
       Result.pField:=node;
      end;
     end;
    end;
   end;
 end;
end;

function TsrField.FetchRuntimeArray(_offset,_stride:PtrUint):TFieldFetchValue;
var
 node:PsrField;
begin
 Result:=Default(TFieldFetchValue);

 node:=Find_be(_offset); //RA only last item
 if (node<>nil) then
 begin
  if (node^.offset>_offset) or
     (node^.dtype<>dtTypeRuntimeArray) or
     (node^.stride<>_stride) then
  begin
   Result.fValue:=frNotFit;
   Result.pField:=nil;
   Exit;
  end;
 end;

 node:=Find_le(_offset);
 if (node=nil) then
 begin
  //new
  node:=Fetch(_offset);
  node^.size   :=High(PtrUint)-_offset;
  node^.stride :=_stride;
  node^.dtype  :=dtTypeRuntimeArray;
  Result.fValue:=frValueInArray;
  Result.pField:=node;
 end else
 if (node^.stride=_stride) and
    (node^.dtype =dtTypeRuntimeArray) and
    (node^.offset<=_offset) then //ident or big than
 begin
  Result.fValue:=frValueInArray;
  Result.pField:=node;
 end;
end;

function TsrField.IsStructUsedRuntimeArray:Boolean;
var
 node:PsrField;
begin
 Result:=False;
 if (dtype=dtTypeStruct) then
 begin
  node:=FList.Max;
  if (node<>nil) then
  begin
   size:=node^.offset+node^.size;
  end;
  Result:=(size=High(PtrUint));
 end;
end;

function TsrField.IsStructNotUsed:Boolean; inline;
begin
 Result:=(FCount<=1) and (pParent<>nil);
end;

function TsrField.IsTop:Boolean; inline;
begin
 Result:=(pParent=nil);
end;

function TsrField.GetStructDecorate:DWORD;
begin
 Result:=DWORD(-1); //dont use
 if IsTop then
 if (dtype=dtTypeStruct) then  //is struct
 begin

  if (pBuffer^.FStorage<>StorageClass.StorageBuffer) and
     (pBuffer^.bType=btStorageBuffer) then
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
 node:PsrField;
begin
 node:=FList.Max;
 if (node<>nil) then
 begin
  size:=node^.offset+node^.size;
 end;
end;

function TsrField.GetSize:PtrUint;
begin
 UpdateSize;
 Result:=size;
end;

procedure TsrField.FillNode(o,s:PtrUint);

 procedure _Pad(p,v:PtrUint;_dtype:TsrDataType); //inline;
 var
  node:PsrField;
 begin
  if (o mod p<>0) and (s>=v) then
  begin
   node:=Fetch(o);
   Assert(node^.dtype=dtUnknow,'WTF');
   node^.size:=v;
   node^.dtype:=_dtype;
   o:=o+v;
   s:=s-v;
  end;
 end;

 procedure _Fill(v:PtrUint;_dtype:TsrDataType); //inline;
 var
  count:PtrUint;
  node:PsrField;
 begin
  count:=s div v;
  While (count<>0) do
  begin
   node:=Fetch(o);
   Assert(node^.dtype=dtUnknow,'WTF');
   node^.size:=v;
   node^.dtype:=_dtype;
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
 pNode:PsrField;
 Foffset,Fsize:PtrUint;
begin
 Result:=0;
 pNode:=First;
 if (pNode=nil) then Exit;

 Foffset:=0;
 While (pNode<>nil) do
 begin

  if (pNode^.dtype=dtUnknow) then
  begin
   Case pNode^.size of
     1:pNode^.dtype:=dtUint8;
     2:pNode^.dtype:=dtHalf16;
     4:pNode^.dtype:=dtFloat32;
     8:pNode^.dtype:=dtVec2f;
    16:pNode^.dtype:=dtVec4f;
    else
     Assert(false);
   end;
  end;

  if (Foffset<pNode^.offset) then
  begin
   Fsize:=pNode^.offset-Foffset;
   FillNode(Foffset,Fsize);
   Inc(Result);
  end;
  Foffset:=pNode^.offset+pNode^.size;
  pNode:=Next(pNode);
 end;

 if (stride<>0) and (dtype in [dtTypeArray,dtTypeRuntimeArray]) then
 begin
  pNode:=FList.Max;
  if (pNode<>nil) then
  begin
   Foffset:=pNode^.offset+pNode^.size;
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

function TsrBuffer.c(n1,n2:PsrBuffer):Integer;
begin
 //first pLayout
 Result:=Integer(n1^.key.pLayout>n2^.key.pLayout)-Integer(n1^.key.pLayout<n2^.key.pLayout);
 if (Result<>0) then Exit;
 //second CastNum
 Result:=Integer(n1^.key.AliasId>n2^.key.AliasId)-Integer(n1^.key.AliasId<n2^.key.AliasId);
end;

Procedure TsrBuffer.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;

 fntype  :=ntBuffer;
 bType   :=btStorageBuffer;
 FStorage:=StorageClass.Uniform;
 FBinding:=-1;

 FTop.FID:=-1;
 FTop.dtype:=dtTypeStruct;
end;

Procedure TsrBuffer.AddDep(t:PsrNode);
var
 pRegsStory:PsrRegsStory;
 node:PRegDNode;
begin
 if (t=nil) or (@Self=nil) then Exit;

 pRegsStory:=FEmit.GetRegsStory;
 node:=pRegsStory^.AllocDep;

 node^.pNode:=t;
 FDList.Push_head(node);
end;

Procedure TsrBuffer.RemDep(t:PsrNode);
var
 pRegsStory:PsrRegsStory;
 node,_prev:PRegDNode;
begin
 if (t=nil) or (@Self=nil) then Exit;
 node:=FDList.pHead;
 _prev:=nil;
 While (node<>nil) do
 begin
  if (node^.pNode=t) then
  begin
   if (_prev=nil) then
   begin
    FDList.pHead:=node^.pNext;
   end else
   begin
    _prev^.pNext:=node^.pNext;
   end;

   pRegsStory:=FEmit.GetRegsStory;
   pRegsStory^.FreeDep(node);

   Exit;
  end;
  _prev:=node;
  node:=node^.pNext;
 end;
 Assert(false,'not found!');
end;

function TsrBuffer.chain_read:DWORD;
var
 node:PRegDNode;
begin
 Result:=0;
 node:=FDList.pHead;
 While (node<>nil) do
 begin
  if node^.pNode^.IsType(ntChain) then
  begin
   Result:=Result+PsrChain(node^.pNode)^.read_count;
  end;
  node:=node^.pNext;
 end;
end;

function TsrBuffer.chain_write:DWORD;
var
 node:PRegDNode;
begin
 Result:=0;
 node:=FDList.pHead;
 While (node<>nil) do
 begin
  if node^.pNode^.IsType(ntChain) then
  begin
   Result:=Result+PsrChain(node^.pNode)^.write_count;
  end;
  node:=node^.pNext;
 end;
end;

function TsrBuffer.GetStorageName:RawByteString;
begin
 Result:='';
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

function TsrBuffer.GetString:RawByteString;
var
 PID:DWORD;
begin
 PID:=0;
 if (key.pLayout<>nil) then
 begin
  PID:=key.pLayout^.FID;
 end;
 Result:='B'+GetTypeChar+
         ';PID='+HexStr(PID,8)+
         ';BND='+HexStr(FBinding,8)+
         ';LEN='+HexStr(GetSize,8)+
         ';OFS='+HexStr(align_offset,8);
end;

function TsrBuffer.GetStructName:RawByteString;
begin
 Result:='TD'+HexStr(FBinding,8);
end;

function TsrBuffer.GetSize:PtrUint;
begin
 Result:=FTop.GetSize;
end;

procedure TsrBuffer.EnumAllField(cb:TFieldEnumCb);
var
 curr,node:PsrField;
begin
 if (cb=nil) then Exit;
 curr:=@FTop;
 node:=curr^.First;
 repeat
  While (node<>nil) do
  begin
   if (node^.First<>nil) then //child exist
   begin
    curr:=node;
    node:=curr^.First;        //down
   end else
   begin
    cb(node);
    node:=curr^.Next(node);
   end;
  end;
  cb(curr);
  node:=curr;
  curr:=curr^.pParent; //up
  if (curr=nil) then Break;
  node:=curr^.Next(node);
 until false;
end;

procedure TsrBuffer.ShiftOffset(Offset:PtrUint);
var
 node:PsrField;
begin
 if (Offset=0) then Exit;
 node:=FTop.Last;
 While (node<>nil) do
 begin
  if (node^.offset+node^.size=High(PtrUint)) then
  begin
   node^.size:=node^.size-Offset;
  end;
  node^.offset:=node^.offset+Offset;
  node:=FTop.Prev(node);
 end;
end;

procedure TsrBufferList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrBufferList.Fetch(s:PsrDataLayout;n:PtrInt):PsrBuffer;
var
 node:TsrBuffer;
begin
 node:=Default(TsrBuffer);
 node.Init(FEmit);
 node.key.pLayout:=s;
 node.key.AliasId:=n;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrBuffer));
  Move(node,Result^,SizeOf(TsrBuffer));
  Result^.FTop.pBuffer:=Result;
  //
  Result^.InitVar(FEmit);
  //
  FNTree.Insert(Result);
 end;
end;

function TsrBufferList.NextAlias(buf:PsrBuffer):PsrBuffer;
begin
 Result:=nil;
 if (buf=nil) then Exit;
 Result:=Fetch(buf^.key.pLayout,buf^.key.AliasId+1);
end;

Function TsrBufferList.First:PsrBuffer;
begin
 Result:=FNTree.Min;
end;

Function TsrBufferList.Next(node:PsrBuffer):PsrBuffer;
begin
 Result:=FNTree.Next(node);
end;

procedure TsrBufferList.EnumAllField(cb:TFieldEnumCb);
var
 node:PsrBuffer;
begin
 if (cb=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  if node^.IsUsed then
  begin
   node^.EnumAllField(cb);
  end;
  node:=Next(node);
 end;
end;

procedure TsrBufferList.OnFillSpace(node:PsrField);
begin
 node^.FillSpace;
end;

procedure TsrBufferList.FillSpace;
begin
 EnumAllField(@OnFillSpace);
end;

procedure TsrBufferList.OnAllocID(pField:PsrField);
var
 node:PsrField;
 ID:Integer;
begin
 ID:=0;
 node:=pField^.First;
 While (node<>nil) do
 begin
  if pField^.dtype.IsVector then
  begin
   ID:=node^.offset div pField^.stride;
   node^.FID:=ID;
  end else
  begin
   node^.FID:=ID;
   Inc(ID);
  end;
  node:=pField^.Next(node);
 end;
end;

procedure TsrBufferList.AllocID;
begin
 EnumAllField(@OnAllocID);
end;

procedure TsrBufferList.AllocBinding(Var FBinding:Integer);
var
 pConfig:PsrConfig;
 pDecorateList:PsrDecorateList;
 node:PsrBuffer;
 pVar:PsrVariable;
begin
 pConfig:=FEmit.GetConfig;
 pDecorateList:=FEmit.GetDecorateList;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) and node^.IsUsed then
  if (node^.bType<>btPushConstant) then
  if (node^.FBinding=-1) then //alloc
  begin
   pDecorateList^.OpDecorate(pVar,Decoration.Binding,FBinding);
   pDecorateList^.OpDecorate(pVar,Decoration.DescriptorSet,pConfig^.DescriptorSet);
   node^.FBinding:=FBinding;
   Inc(FBinding);
  end;
  node:=Next(node);
 end;
end;

procedure TsrBufferList.AllocSourceExtension;
var
 pDebugInfoList:PsrDebugInfoList;
 node:PsrBuffer;
 pVar:PsrVariable;
begin
 pDebugInfoList:=FEmit.GetDebugInfoList;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) and node^.IsUsed then
  begin
   pDebugInfoList^.OpSourceExtension(node^.GetString);
  end;
  node:=Next(node);
 end;
end;

function TsrBufferList.FindUserDataBuf:PsrBuffer;
var
 node:PsrBuffer;
begin
 Result:=nil;
 node:=First;
 While (node<>nil) do
 begin
  if node^.IsUsed then
  begin
   if node^.key.pLayout^.IsUserData then
   begin
    Exit(node);
   end;
  end;
  node:=Next(node);
 end;
end;

procedure TsrBufferList.ApplyBufferType;
var
 pConfig:PsrConfig;
 node:PsrBuffer;
 fchain_write:DWORD;
begin
 pConfig:=FEmit.GetConfig;

 node:=FindUserDataBuf;
 if (node<>nil) and (FPushConstant=nil) then
 if (node^.write_count=0) and
    (node^.GetSize<=pConfig^.maxPushConstantsSize) then
 begin
  node^.bType   :=btPushConstant;
  node^.FStorage:=StorageClass.PushConstant;
  FPushConstant :=node;
 end;

 node:=First;
 While (node<>nil) do
 begin
  if node^.IsUsed and (node^.bType=btStorageBuffer) then
  begin

   fchain_write:=node^.chain_write;

   if (FPushConstant=nil) and
      (fchain_write=0) and
      (node^.GetSize<=pConfig^.maxPushConstantsSize) then
   begin
    node^.bType   :=btPushConstant;
    node^.FStorage:=StorageClass.PushConstant;
    FPushConstant :=node;
   end else
   if (fchain_write=0) and
      (node^.GetSize<=pConfig^.maxUniformBufferRange) then
   begin
    node^.bType   :=btUniformBuffer;
    node^.FStorage:=StorageClass.Uniform;
   end else
   if pConfig^.CanUseStorageBufferClass then
   begin
    node^.FStorage:=StorageClass.StorageBuffer;
   end else
   begin
    node^.FStorage:=StorageClass.Uniform;
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

procedure TsrBufferList.AlignOffset(node:PsrBuffer;offset:PtrUint);
var
 P:Pointer;
begin
 P:=node^.key.pLayout^.GetData;
 offset:=AlignShift(P,offset);
 node^.align_offset:=offset;
 node^.ShiftOffset(offset);
end;

procedure TsrBufferList.AlignOffset;
var
 pConfig:PsrConfig;
 node:PsrBuffer;
begin
 pConfig:=FEmit.GetConfig;
 node:=First;
 While (node<>nil) do
 begin
  if node^.IsUsed then
  begin
   Case node^.bType of
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
       node^.align_offset:=pConfig^.PushConstantsOffset;
       node^.ShiftOffset(pConfig^.PushConstantsOffset);
      end;
   end;
  end;
  node:=Next(node);
 end;
end;

procedure TsrBufferList.OnAllocTypeBinding(pField:PsrField);
var
 pDecorateList:PsrDecorateList;
 node:PsrField;
 SD:DWORD;
begin
 if (pField^.dtype<>dtTypeStruct) then Exit;
 if (pField^.pType=nil) then Exit;
 pDecorateList:=FEmit.GetDecorateList;
 SD:=pField^.GetStructDecorate;
 if (SD<>DWORD(-1)) then
 begin
  pDecorateList^.OpDecorate(pField^.pType,SD,0);
 end;
 node:=pField^.First;
 While (node<>nil) do
 begin
  pDecorateList^.OpMemberDecorate(pField^.pType,node^.FID,node^.offset);
  node:=pField^.Next(node);
 end;
end;

procedure TsrBufferList.AllocTypeBinding;
var
 pDecorateList:PsrDecorateList;
 node:PsrBuffer;
begin
 EnumAllField(@OnAllocTypeBinding);
 //
 pDecorateList:=FEmit.GetDecorateList;
 //
 node:=First;
 While (node<>nil) do
 begin
  if node^.IsUsed and (node^.pVar<>nil) then
  if (node^.bType=btStorageBuffer) then
  begin
   if (node^.chain_read=0) then
   begin
    pDecorateList^.OpDecorate(node^.pVar,Decoration.NonReadable,0);
   end;
   if (node^.chain_write=0) then
   begin
    pDecorateList^.OpDecorate(node^.pVar,Decoration.NonWritable,0);
   end;
  end;
  node:=Next(node);
 end;
end;

procedure TsrBufferList.AllocName;
var
 FDebugInfo:PsrDebugInfoList;
 node:PsrBuffer;
begin
 FDebugInfo:=FEmit.GetDebugInfoList;
 node:=First;
 While (node<>nil) do
 begin
  if node^.IsUsed and (node^.FTop.pType<>nil) then
  begin
   FDebugInfo^.OpName(node^.FTop.pType,node^.GetStructName);
  end;
  node:=Next(node);
 end;
end;

end.

