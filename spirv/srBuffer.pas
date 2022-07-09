unit srBuffer;

{$mode ObjFPC}{$H+}

interface

uses
  spirv,
  srNodes,
  srTypes,
  srVariable,
  srLayout,
  srDecorate;

type
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
   parent:PsrField;
   offset:PtrUint;
   size:PtrUint;
   stride:PtrUint;

   FCount:PtrUint;
   dtype:TsrDataType;
   pType:PsrType;
   FID:Integer; //alloc late

   //

   Alloc:TfnAlloc;
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
  function  IsStructNotUsed:Boolean;
  function  GetStructDecorate:DWORD;
  procedure FillNode(o,s:PtrUint);
  function  FillSpace:Integer;
  procedure AllocID;
  procedure AllocBinding(aType:PsrType;Decorates:PsrDecorateList);
 end;

 TsrBufferType=(btStorageBuffer,btUniformBuffer,btPushConstant);

 TsrBuffer=object(TsrDescriptor)
  pLeft,pRight:PsrBuffer;
  //----

  key:packed record
   pLayout:PsrDataLayout;
   CastNum:PtrInt;
  end;

  bType:TsrBufferType;

  write_count:DWORD;
  align_offset:DWORD;

  FTop:TsrField;
  function  c(n1,n2:PsrBuffer):Integer; static;
  function  GetTypeChar:Char;
  function  GetString:RawByteString;
  function  GetStructName:RawByteString;
  procedure UpdateSize;
  function  GetSize:PtrUint;
  procedure EnumAllField(cb:TFieldEnumCb);
  procedure ShiftOffset(Offset:PtrUint);
 end;

 TsrBufferCfg=object
  SpvVersion:PtrUint;
  maxUniformBufferRange:PtrUint;           // $FFFF
  PushConstantsOffset:PtrUint;             // 0
  maxPushConstantsSize:PtrUint;            // 128
  minStorageBufferOffsetAlignment:PtrUint; // $10
  minUniformBufferOffsetAlignment:PtrUint; // $100
  Procedure Init;
  Function  CanUseStorageBufferClass:Boolean;
 end;

 TsrBufferList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrBuffer,TsrBuffer>;
  var
   Alloc:TfnAlloc;
   cfg:TsrBufferCfg;
   FNTree:TNodeFetch;
   FPushConstant:PsrBuffer;
  procedure Init(cb:TfnAlloc);
  function  Fetch(s:PsrDataLayout;n:PtrInt):PsrBuffer;
  function  NextCast(buf:PsrBuffer):PsrBuffer;
  Function  First:PsrBuffer;
  Function  Next(node:PsrBuffer):PsrBuffer;
  procedure EnumAllField(cb:TFieldEnumCb);
  procedure OnFillSpace(node:PsrField);
  procedure FillSpace;
  procedure OnAllocID(node:PsrField);
  procedure AllocID;
  procedure AllocBinding(Var FBinding:Integer;Decorates:PsrDecorateList);
  procedure AllocSourceExtension(FDebugInfo:PsrDebugInfoList);
  procedure UpdateStorage(node:PsrChain;pBuffer:PsrBuffer);
  procedure ApplyBufferType;
  procedure AlignOffset(node:PsrBuffer;offset:PtrUint);
  procedure AlignOffset;
 end;

implementation

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
  Result:=Alloc(SizeOf(TsrField));
  Result^.Alloc:=Alloc;
  Result^.pBuffer:=pBuffer;
  Result^.parent:=@Self;
  Result^.offset:=o;
  Result^.FID:=-1;
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
 if isVector(_dtype) then
 begin
  _stride:=BitSizeType(GetVecChild(_dtype)) div 8;
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
   end;
  else
   begin
    if isVector(node^.dtype) then
    begin //ftVector
     if isVector(_dtype) then
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
     if isVector(_dtype) then
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

function TsrField.IsStructNotUsed:Boolean;
begin
 Result:=(FCount<=1) and (parent<>nil);
end;

function TsrField.GetStructDecorate:DWORD;
begin
 Result:=DWORD(-1); //dont use
 if (parent=nil) then //is top
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

procedure TsrField.AllocID;
var
 node:PsrField;
 ID:Integer;
begin
 ID:=0;
 node:=First;
 While (node<>nil) do
 begin
  if IsVector(dtype) then
  begin
   ID:=node^.offset div stride;
   node^.FID:=ID;
  end else
  begin
   node^.FID:=ID;
   Inc(ID);
  end;
  node:=Next(node);
 end;
end;

procedure TsrField.AllocBinding(aType:PsrType;Decorates:PsrDecorateList);
var
 node:PsrField;
begin
 if (aType=nil) then Exit;
 if (Decorates=nil) then Exit;
 if isVector(dtype) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  Decorates^.emit_member_decorate(ntType,aType,node^.FID,node^.offset);
  node:=Next(node);
 end;
end;

//--

function TsrBuffer.c(n1,n2:PsrBuffer):Integer;
begin
 //first pLayout
 Result:=Integer(n1^.key.pLayout>n2^.key.pLayout)-Integer(n1^.key.pLayout<n2^.key.pLayout);
 if (Result<>0) then Exit;
 //second CastNum
 Result:=Integer(n1^.key.CastNum>n2^.key.CastNum)-Integer(n1^.key.CastNum<n2^.key.CastNum);
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

procedure TsrBuffer.UpdateSize;
var
 node:PsrField;
begin
 node:=FTop.FList.Max;
 if (node<>nil) then
 begin
  FTop.size:=node^.offset+node^.size;
 end;
end;

function TsrBuffer.GetSize:PtrUint;
begin
 UpdateSize;
 Result:=FTop.size;
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
   if (node^.FList.pRoot<>nil) then //child exist
   begin
    curr^.FList._Splay(node); //Move to root
    curr:=node;
    node:=curr^.First;        //down
   end else
   begin
    cb(node);
    node:=curr^.Next(node);
   end;
  end;
  cb(curr);
  curr:=curr^.parent; //up
  if (curr=nil) then Break;
  node:=curr^.FList.pRoot; //last find
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

Procedure TsrBufferCfg.Init;
begin
 SpvVersion:=$10100;
 maxUniformBufferRange:=$FFFF;
 maxPushConstantsSize:=128;
 minStorageBufferOffsetAlignment:=$10;
 minUniformBufferOffsetAlignment:=$100;
end;

Function TsrBufferCfg.CanUseStorageBufferClass:Boolean;
begin
 Result:=(SpvVersion>=$10300);
end;

procedure TsrBufferList.Init(cb:TfnAlloc);
begin
 Alloc:=cb;
 cfg.Init;
end;

function TsrBufferList.Fetch(s:PsrDataLayout;n:PtrInt):PsrBuffer;
var
 node:TsrBuffer;
begin
 node:=Default(TsrBuffer);
 node.key.pLayout:=s;
 node.key.CastNum:=n;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=Alloc(SizeOf(TsrBuffer));
  Result^.key.pLayout:=s;
  Result^.key.CastNum:=n;
  Result^.bType   :=btStorageBuffer;
  Result^.FStorage:=StorageClass.Uniform;
  Result^.FBinding:=-1;
  Result^.FTop.Alloc:=Alloc;
  Result^.FTop.pBuffer:=Result;
  Result^.FTop.FID:=-1;
  Result^.FTop.dtype:=dtTypeStruct;
  FNTree.Insert(Result);
 end;
end;

function TsrBufferList.NextCast(buf:PsrBuffer):PsrBuffer;
begin
 Result:=nil;
 if (buf=nil) then Exit;
 Result:=Fetch(buf^.key.pLayout,buf^.key.CastNum+1);
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
  node^.EnumAllField(cb);
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

procedure TsrBufferList.OnAllocID(node:PsrField);
begin
 node^.AllocID;
end;

procedure TsrBufferList.AllocID;
begin
 EnumAllField(@OnAllocID);
end;

procedure TsrBufferList.AllocBinding(Var FBinding:Integer;Decorates:PsrDecorateList);
var
 node:PsrBuffer;
 pVar:PsrVariable;
begin
 if (Decorates=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) then
  begin
   if (node^.bType<>btPushConstant) then
   if (node^.FBinding=-1) then //alloc
   begin
    Decorates^.emit_decorate(ntVar,pVar,Decoration.Binding,FBinding);
    Decorates^.emit_decorate(ntVar,pVar,Decoration.DescriptorSet,Decorates^.FDescriptorSet);
    node^.FBinding:=FBinding;
    Inc(FBinding);
   end;
  end;
  node:=Next(node);
 end;
end;

procedure TsrBufferList.AllocSourceExtension(FDebugInfo:PsrDebugInfoList);
var
 node:PsrBuffer;
 pVar:PsrVariable;
begin
 if (FDebugInfo=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) then
  begin
   FDebugInfo^.emit_source_extension(node^.GetString);
  end;
  node:=Next(node);
 end;
end;

procedure TsrBufferList.UpdateStorage(node:PsrChain;pBuffer:PsrBuffer);
begin
 if (node=nil) or (pBuffer=nil) then Exit;
 pBuffer^.write_count:=pBuffer^.write_count+node^.write_count;
end;

procedure TsrBufferList.ApplyBufferType;
var
 node:PsrBuffer;
begin
 node:=First;
 While (node<>nil) do
 begin
  if (node^.bType=btStorageBuffer) then
  begin

   if (FPushConstant=nil) and
      (node^.write_count=0) and
      (node^.GetSize<=cfg.maxPushConstantsSize) then
   begin
    node^.bType   :=btPushConstant;
    node^.FStorage:=StorageClass.PushConstant;
    FPushConstant:=node;
   end else
   if (node^.write_count=0) and
      (node^.GetSize<=cfg.maxUniformBufferRange) then
   begin
    node^.bType   :=btUniformBuffer;
    node^.FStorage:=StorageClass.Uniform;
   end else
   if cfg.CanUseStorageBufferClass then
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
 node:PsrBuffer;
begin
 node:=First;
 While (node<>nil) do
 begin
  Case node^.bType of
   btStorageBuffer:
     begin
      AlignOffset(node,cfg.minStorageBufferOffsetAlignment);
     end;
   btUniformBuffer:
     begin
      AlignOffset(node,cfg.minUniformBufferOffsetAlignment);
     end;
   btPushConstant:
     begin
      node^.align_offset:=cfg.PushConstantsOffset;
      node^.ShiftOffset(cfg.PushConstantsOffset);
     end;
  end;
  node:=Next(node);
 end;
end;

end.

