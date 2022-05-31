unit srLayout;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_shader,
  srNodes,
  srParser,
  srCFG,
  srTypes,
  srReg,
  srVariable,
  srRefId,
  srDecorate;

type
 TsrResourceType=(
  rtBufPtr2,
  rtFunPtr2,
  rtVSharp4,
  rtSSharp4,
  rtTSharp4,
  rtTSharp8
 );

type
 PsrDataLayout=^TsrDataLayout;

 PsrChainExt=^TsrChainExt;
 TsrChainExt=object
  pIndex:PsrRegNode;
  stride:PtrUint;
  function c(n1,n2:PsrChainExt):Integer; static;
 end;

 PsrChain=^TsrChain;
 TsrChain=object
  pLeft,pRight:PsrChain;
  //----
  read_count:DWORD;
  write_count:DWORD;
  parent:PsrDataLayout;
  key:packed record
   ext:TsrChainExt;
   size,offset:PtrUint;
  end;
  rSlot:TsrRegSlot;
  pField:Pointer;
  pLine:Pointer;
  ID:TsrRefId; //post id
  function  c(n1,n2:PsrChain):Integer; static;
  Procedure mark_read;
  Procedure mark_unread;
  Procedure mark_write;
  Procedure SetRegType(rtype:TsrDataType);
  function  GetRegType:TsrDataType;
  function  IsUsed:Boolean;
 end;

 TsrChains=array[0..7] of PsrChain;

 //----

 TsrDataLayout=object
  type
   TChainFetch=specialize TNodeFetch<PsrChain,TsrChain>;
  var
   pLeft,pRight:PsrDataLayout;
   //----
   key:packed record
    parent:PsrDataLayout;
    offset:PtrUint;
    rtype:TsrResourceType;
   end;
   pData:Pointer;
   FID:Integer;
   Alloc:TfnAlloc;
   FList:TChainFetch;
  function c(n1,n2:PsrDataLayout):Integer; static;
  function Fetch(o,s:PtrUint;ext:PsrChainExt):PsrChain;
  Function First:PsrChain;
  Function Last:PsrChain;
  Function Next(node:PsrChain):PsrChain;
  Function Prev(node:PsrChain):PsrChain;
  function GetData:Pointer;
  function GetStride:PtrUint;
  function GetTypeChar:Char;
  function GetString:RawByteString;
  function GetFuncString(LEN:DWORD):RawByteString;
 end;

 TsrDataLayoutList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrDataLayout,TsrDataLayout>;
  var
   FTop:TsrDataLayout;
   FNTree:TNodeFetch;
  procedure Init(cb:TfnAlloc);
  procedure SetUserData(pData:Pointer);
  function  Fetch(p:PsrDataLayout;o:PtrUint;t:TsrResourceType):PsrDataLayout;
  Function  First:PsrDataLayout;
  Function  Next(node:PsrDataLayout):PsrDataLayout;
  function  Grouping(const chain:TsrChains;rtype:TsrResourceType):PsrDataLayout;
  Procedure AllocID;
  procedure AllocSourceExtension(FDebugInfo:PsrDebugInfoList);
  procedure AllocFuncExt(FDebugInfo:PsrDebugInfoList;var Heap:TsrCodeHeap);
 end;

//----

 PsrDescriptor=^TsrDescriptor;
 TsrDescriptor=object
  pVar:PsrVariable;
  FStorage:DWORD;
  FBinding:Integer;
 end;

function is_consistents(const chains:TsrChains;count:Byte):Boolean;
function is_no_index_chains(const chains:TsrChains;count:Byte):Boolean;
function is_userdata_chains(const chains:TsrChains;count:Byte):Boolean;
function GetResourceSizeDw(r:TsrResourceType):Byte;

implementation

function TsrDataLayout.c(n1,n2:PsrDataLayout):Integer;
begin
 //first parent
 Result:=Integer(n1^.key.parent>n2^.key.parent)-Integer(n1^.key.parent<n2^.key.parent);
 if (Result<>0) then Exit;
 //second offset
 Result:=Integer(n1^.key.offset>n2^.key.offset)-Integer(n1^.key.offset<n2^.key.offset);
 if (Result<>0) then Exit;
 //third rtype
 Result:=Integer(n1^.key.rtype>n2^.key.rtype)-Integer(n1^.key.rtype<n2^.key.rtype);
end;

function TsrDataLayout.Fetch(o,s:PtrUint;ext:PsrChainExt):PsrChain;
var
 node:TsrChain;
begin
 node:=Default(TsrChain);
 node.key.offset:=o;
 node.key.size  :=s;
 if (ext<>nil) then
 begin
  node.key.ext:=ext^;
 end;
 Result:=FList.Find(@node);
 if (Result=nil) then
 begin
  Result:=Alloc(SizeOf(TsrChain));
  Result^.parent:=@Self;
  Result^.key.offset:=o;
  Result^.key.size  :=s;
  if (ext<>nil) then
  begin
   Result^.key.ext:=ext^;
  end;
  Result^.rSlot.Init(Alloc,'CHAIN');
  FList.Insert(Result);
 end else
 begin
  if (ext<>nil) then
  if (ext^.pIndex<>nil) then
  begin
   ext^.pIndex^.mark_unread;
  end;
 end;
end;

Function TsrDataLayout.First:PsrChain;
begin
 Result:=FList.Min;
end;

Function TsrDataLayout.Last:PsrChain;
begin
 Result:=FList.Max;
end;

Function TsrDataLayout.Next(node:PsrChain):PsrChain;
begin
 Result:=FList.Next(node);
end;

Function TsrDataLayout.Prev(node:PsrChain):PsrChain;
begin
 Result:=FList.Prev(node);
end;

function TsrDataLayout.GetData:Pointer;
begin
 Result:=nil;
 if (pData<>nil) then
  Case key.rtype of
   rtBufPtr2,
   rtFunPtr2:Result:=pData;
   rtVSharp4:Result:={%H-}Pointer(PVSharpResource4(pData)^.base);
   rtTSharp4,
   rtTSharp8:Result:={%H-}Pointer(PTSharpResource4(pData)^.base shl 8);
   else;
  end;
end;

function TsrDataLayout.GetStride:PtrUint;
begin
 Result:=0;
 if (pData<>nil) then
  Case key.rtype of
   rtBufPtr2:Result:=4;
   rtVSharp4:Result:=PVSharpResource4(pData)^.stride;
   else;
  end;
end;

function TsrDataLayout.GetTypeChar:Char;
begin
 Result:=#0;
 case key.rtype of
  rtBufPtr2:Result:='B';
  rtFunPtr2:Result:='F';
  rtVSharp4:Result:='V';
  rtSSharp4:Result:='S';
  rtTSharp4:Result:='t';
  rtTSharp8:Result:='T';
 end;
end;

function TsrDataLayout.GetString:RawByteString;
var
 PID:DWORD;
begin
 PID:=0;
 if (key.parent<>nil) then
 begin
  PID:=key.parent^.FID;
 end;
 Result:='#'+GetTypeChar+
         ';PID='+HexStr(PID,8)+
         ';OFS='+HexStr(key.offset,8);
end;

function TsrDataLayout.GetFuncString(LEN:DWORD):RawByteString;
begin
 Result:='FF'+
         ';PID='+HexStr(FID,8)+
         ';LEN='+HexStr(LEN,8);
end;

procedure TsrDataLayoutList.Init(cb:TfnAlloc);
begin
 FTop.Alloc:=cb;
 FNTree.Insert(@FTop);
end;

procedure TsrDataLayoutList.SetUserData(pData:Pointer);
begin
 FTop.pData:=pData;
end;

function TsrDataLayoutList.Fetch(p:PsrDataLayout;o:PtrUint;t:TsrResourceType):PsrDataLayout;
var
 node:TsrDataLayout;
 pData:Pointer;
begin
 node:=Default(TsrDataLayout);
 node.Alloc:=FTop.Alloc;
 node.key.parent:=p;
 node.key.offset:=o;
 node.key.rtype :=t;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=FTop.Alloc(SizeOf(TsrDataLayout));
  Move(node,Result^,SizeOf(TsrDataLayout));
  FNTree.Insert(Result);

  Result^.FID:=-1;

  if (p<>nil) then
  begin
   pData:=p^.GetData;
   if (pData<>nil) then
    case t of
     rtBufPtr2,
     rtFunPtr2:Result^.pData:={%H-}Pointer(PPtrUint(pData+o)^ and (not 3));
     rtVSharp4,
     rtSSharp4,
     rtTSharp4,
     rtTSharp8:Result^.pData:=pData+o;
    end;
  end;

 end;
end;

Function TsrDataLayoutList.First:PsrDataLayout;
begin
 Result:=FNTree.Min;
end;

Function TsrDataLayoutList.Next(node:PsrDataLayout):PsrDataLayout;
begin
 Result:=FNTree.Next(node);
end;

function GetResourceSizeDw(r:TsrResourceType):Byte;
begin
 Result:=0;
 Case r of
  rtBufPtr2:Result:=2;
  rtFunPtr2:Result:=2;
  rtVSharp4:Result:=4;
  rtSSharp4:Result:=4;
  rtTSharp4:Result:=4;
  rtTSharp8:Result:=8;
 end;
end;

function TsrDataLayoutList.Grouping(const chain:TsrChains;rtype:TsrResourceType):PsrDataLayout;
begin
 Result:=nil;

 if not is_consistents(chain,GetResourceSizeDw(rtype)) then
 begin
  Assert(False,'inconsistent resources not supported');
 end;

 if not is_no_index_chains(chain,GetResourceSizeDw(rtype)) then
 begin
  Assert(False,'indexed chain not support');
 end;

 Result:=Fetch(chain[0]^.parent,chain[0]^.key.offset,rtype);
end;

Procedure TsrDataLayoutList.AllocID;
var
 node:PsrDataLayout;
 FID:Integer;
begin
 FID:=1;
 node:=First;
 While (node<>nil) do
 begin
  if (node^.FID=-1) then
  begin
   node^.FID:=FID;
   Inc(FID);
  end;
  node:=Next(node);
 end;
end;

procedure TsrDataLayoutList.AllocSourceExtension(FDebugInfo:PsrDebugInfoList);
var
 node:PsrDataLayout;
begin
 if (FDebugInfo=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  FDebugInfo^.emit_source_extension(node^.GetString);
  node:=Next(node);
 end;
end;

procedure TsrDataLayoutList.AllocFuncExt(FDebugInfo:PsrDebugInfoList;var Heap:TsrCodeHeap);
var
 node:PsrDataLayout;
 block:PsrCodeBlock;
begin
 node:=First;
 While (node<>nil) do
 begin
  if (node^.key.rtype=rtFunPtr2) then
  begin
   block:=Heap.FindByPtr(node^.pData);
   if (block<>nil) then
   begin
    FDebugInfo^.emit_source_extension(node^.GetFuncString(block^.Size));
   end;
  end;
  node:=Next(node);
 end;
end;

function TsrChainExt.c(n1,n2:PsrChainExt):Integer;
begin
 //first pIndex forward
 Result:=Integer(n1^.pIndex>n2^.pIndex)-Integer(n1^.pIndex<n2^.pIndex);
 if (Result<>0) then Exit;

 //second stride forward
 Result:=Integer(n1^.stride>n2^.stride)-Integer(n1^.stride<n2^.stride);
end;

function TsrChain.c(n1,n2:PsrChain):Integer;
begin
 //first ext
 Result:=TsrChainExt.c(@n1^.key.ext,@n2^.key.ext);
 if (Result<>0) then Exit;

 //second size backward
 Result:=Integer(n1^.key.size<n2^.key.size)-Integer(n1^.key.size>n2^.key.size);
 if (Result<>0) then Exit;

 //third offset forward
 Result:=Integer(n1^.key.offset>n2^.key.offset)-Integer(n1^.key.offset<n2^.key.offset);
end;

Procedure TsrChain.mark_read;
begin
 Inc(read_count);
end;

Procedure TsrChain.mark_unread;
begin
 if (read_count<>0) then Dec(read_count);
end;

Procedure TsrChain.mark_write;
begin
 Inc(write_count);
end;

Procedure TsrChain.SetRegType(rtype:TsrDataType);
var
 node:PsrRegNode;
begin
 node:=rSlot.pStory.pHead;
 While (node<>nil) do
 begin
  node^.dtype:=rtype;
  node:=node^.pNext;
 end;
end;

function TsrChain.GetRegType:TsrDataType;
var
 pReg:PsrRegNode;
begin
 Result:=dtUnknow;
 pReg:=rSlot.current;
 if (pReg=nil) then Exit;
 Result:=pReg^.dtype;
end;

function TsrChain.IsUsed:Boolean;
begin
 Result:=((read_count<>0) or
         (write_count<>0)) and
         (rSlot.current<>nil);
end;

function is_consistents(const chains:TsrChains;count:Byte):Boolean;
var
 parent:PsrDataLayout;
 offset,t:PtrUint;
 i:Byte;
begin
 offset:=0;
 t:=0;
 if (count<2) then Exit(True);
 Result:=False;
 if (chains[0]=nil) then Exit;
 parent:=chains[0]^.parent;
 offset:=chains[0]^.key.offset;
 For i:=1 to count-1 do
 begin
  t:=chains[i-1]^.key.size;
  offset:=offset+t;
  if (chains[i]=nil) then Exit;
  if (chains[i]^.parent<>parent) then Exit;
  t:=chains[i]^.key.offset;
  if (offset<>t) then Exit;
 end;
 Result:=True;
end;

function is_no_index_chains(const chains:TsrChains;count:Byte):Boolean;
var
 i:Byte;
begin
 Result:=False;
 if (count=0) then Exit;
 For i:=0 to count-1 do
 begin
  if (chains[i]=nil) then Exit;
  if (chains[i]^.key.ext.pIndex<>nil) then Exit;
 end;
 Result:=True;
end;

function is_userdata_chains(const chains:TsrChains;count:Byte):Boolean;
var
 parent:PsrDataLayout;
 i:Byte;
begin
 Result:=False;
 if (count=0) then Exit;
 For i:=0 to count-1 do
 begin
  if (chains[i]=nil) then Exit;
  parent:=chains[i]^.parent;
  if (parent=nil) then Exit;
  if (parent^.key.parent<>nil) then Exit;
 end;
 Result:=True;
end;

end.

