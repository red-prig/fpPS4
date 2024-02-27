unit srLayout;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 ps4_shader,
 spirv,
 ginodes,
 srNode,
 srCFGParser,
 srCFGCursor,
 srType,
 srTypes,
 srReg,
 srOp,
 srVariable,
 srBitcast,
 srRefId,
 srDecorate;

type
 TsrResourceType=(
  rtRoot,
  rtBufPtr2,
  rtFunPtr2,
  rtVSharp4,
  rtSSharp4,
  rtTSharp4,
  rtTSharp8
 );

type
 ntChain=class(TsrNodeVmt)
  class Procedure zero_read   (node:PsrNode);               override;
  class Procedure zero_unread (node:PsrNode);               override;
  class Function  pwrite_count(node:PsrNode):PDWORD;        override;
  class Procedure SetWriter   (node,w,line:PsrNode);        override;
  class Procedure ResetWriter (node,w:PsrNode);             override;
  class function  Down        (node:PsrNode):Pointer;       override;
  class function  Next        (node:PsrNode):Pointer;       override;
  class function  Prev        (node:PsrNode):Pointer;       override;
  class function  Parent      (node:PsrNode):Pointer;       override;
  class Procedure PrepType    (node:PPrepTypeNode);         override;
  class function  GetPrintName(node:PsrNode):RawByteString; override;
  class function  GetRef      (node:PsrNode):Pointer;       override;
 end;

 PsrDataLayout=^TsrDataLayout;

 PsrChainLvl_1=^TsrChainLvl_1;
 TsrChainLvl_1=object
  pIndex:PsrRegNode;
  stride:PtrUint;
  function c(n1,n2:PsrChainLvl_1):Integer; static;
 end;

 PsrChainLvl_0=^TsrChainLvl_0;
 TsrChainLvl_0=object
  size  :PtrUint;
  offset:PtrUint;
  function c(n1,n2:PsrChainLvl_0):Integer; static;
 end;

 PsrChain=^TsrChain;
 TsrChain=object(TsrNode)
  type
   PVNode=^TVNode;
   TVNode=record
    pPrev,pNext:PVNode;
    pLine:PspirvOp;
   end;
   TNodeList=specialize TNodeList<PVNode>;
  public
   pLeft,pRight:PsrChain;
   function  c(n1,n2:PsrChain):Integer; static;
  private
   fwrite_count:DWORD;
   //--
   ID:TsrRefId; //post id
   FParent:PsrDataLayout;
   key:packed record
    lvl_1:TsrChainLvl_1;
    lvl_0:TsrChainLvl_0;
   end;
   FBuffer:PsrNode;
   FWriter:PsrNode;
   Fdtype:TsrDataType;
   FList:TNodeList;
   Procedure SetWriter(t:PsrNode);
   Function  GetWriter:PsrNode;
   Procedure SetBuffer(t:PsrNode);
   Function  GetBuffer:PsrNode;
   Procedure SetRegType(rtype:TsrDataType);
  public
   pField:Pointer;
   property  Parent:PsrDataLayout read FParent;
   property  pIndex:PsrRegNode    read key.lvl_1.pIndex;
   property  stride:PtrUint       read key.lvl_1.stride;
   property  size  :PtrUint       read key.lvl_0.size;
   property  offset:PtrUint       read key.lvl_0.offset;
   property  dtype:TsrDataType    read Fdtype write SetRegType;
   property  pWriter:PsrNode      read GetWriter write SetWriter;
   property  pBuffer:PsrNode      read GetBuffer write SetBuffer;
   Procedure Init(L:PsrDataLayout);
   function  Emit:TCustomEmit;
   Procedure UpdateRegType;
   Procedure PrepType(new:TsrDataType);
   procedure AddLine(pLine:PspirvOp);
   function  FirstLine:PspirvOp;
   procedure FetchLoad(pLine:PspirvOp;dst:PsrRegNode);
   Procedure FetchStore(pLine:PspirvOp;src:PsrRegNode);
   function  GetPrintName:RawByteString;
 end;

 TsrChains=array[0..7] of PsrChain;

 TChainCb=function(node:PsrChain):Integer of object;

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
   FEmit:TCustomEmit;
   FList:TChainFetch;
  function c(n1,n2:PsrDataLayout):Integer; static;
  function Fetch(lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1):PsrChain;
  Function First:PsrChain;
  Function Last:PsrChain;
  Function NextChain(node:PsrChain):PsrChain;
  Function PrevChain(node:PsrChain):PsrChain;
  function EnumChain(cb:TChainCb):Integer;
  function GetData:Pointer;
  function IsUserData:Boolean; inline;
  function GetStride:PtrUint;
  function GetTypeChar:Char;
  function GetString:RawByteString;
  function GetFuncString(LEN:DWORD):RawByteString;
 end;

 PsrDataLayoutList=^TsrDataLayoutList;
 TsrDataLayoutList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrDataLayout,TsrDataLayout>;
  var
   FTop:TsrDataLayout;
   FNTree:TNodeFetch;
  procedure Init(Emit:TCustomEmit);
  procedure SetUserData(pData:Pointer);
  function  pRoot:PsrDataLayout;
  function  Fetch(p:PsrDataLayout;o:PtrUint;t:TsrResourceType):PsrDataLayout;
  Function  First:PsrDataLayout;
  Function  Next(node:PsrDataLayout):PsrDataLayout;
  function  Grouping(const chain:TsrChains;rtype:TsrResourceType):PsrDataLayout;
  function  EnumChain(cb:TChainCb):Integer;
  Procedure AllocID;
  procedure AllocSourceExtension;
  procedure AllocFuncExt;
 end;

//----

 ntDescriptor=class(TsrNodeVmt)
  class Procedure zero_read      (node:PsrNode);         override;
  class Procedure zero_unread    (node:PsrNode);         override;
  class Function  GetPtype       (node:PsrNode):PsrNode; override;
  class Function  GetStorageClass(node:PsrNode):DWORD;   override;
 end;

 PsrDescriptor=^TsrDescriptor;
 TsrDescriptor=packed object(TsrNode)
  protected
   FVar:PsrVariable;
   FType:PsrType;
   FStorage:DWORD;
   FBinding:Integer;
   procedure InitVar(Emit:TCustomEmit);
   procedure InitType(rtype:TsrDataType;Emit:TCustomEmit);
   procedure SetType(t:PsrType);
  public
   property  pVar:PsrVariable read FVar;
   property  pType:PsrType    read FType write SetType;
 end;

function is_consistents(const chains:TsrChains;count:Byte):Boolean;
function is_no_index_chains(const chains:TsrChains;count:Byte):Boolean;
function is_userdata_chains(const chains:TsrChains;count:Byte):Boolean;
function GetResourceSizeDw(r:TsrResourceType):Byte;

implementation

class Procedure ntChain.zero_read(node:PsrNode);
begin
 With PsrChain(node)^ do
 begin
  key.lvl_1.pIndex^.mark_read(node);
  FBuffer^.mark_read(node);
 end;
end;

class Procedure ntChain.zero_unread(node:PsrNode);
begin
 With PsrChain(node)^ do
 begin
  key.lvl_1.pIndex^.mark_unread(node);
  FBuffer^.mark_unread(node);
 end;
end;

class Function ntChain.pwrite_count(node:PsrNode):PDWORD;
begin
 Result:=@PsrChain(node)^.fwrite_count;
end;

class Procedure ntChain.SetWriter(node,w,line:PsrNode);
begin
 With PsrChain(node)^ do
 begin
  SetWriter(w);
 end;
end;

class Procedure ntChain.ResetWriter(node,w:PsrNode);
begin
 With PsrChain(node)^ do
 if (FWriter=w) then
 begin
  SetWriter(nil);
 end;
end;

class function ntChain.Down(node:PsrNode):Pointer;
begin
 Result:=PsrChain(node)^.FWriter;
end;

class function ntChain.Next(node:PsrNode):Pointer;
begin
 Result:=PsrChain(node)^.FParent^.NextChain(PsrChain(node));
end;

class function ntChain.Prev(node:PsrNode):Pointer;
begin
 Result:=PsrChain(node)^.FParent^.PrevChain(PsrChain(node));
end;

class function ntChain.Parent(node:PsrNode):Pointer;
begin
 Result:=PsrChain(node)^.FParent;
end;

class Procedure ntChain.PrepType(node:PPrepTypeNode);
begin
 PsrChain(node^.dnode)^.PrepType(TsrDataType(node^.rtype));
 node^.dnode:=nil;
end;

class function ntChain.GetPrintName(node:PsrNode):RawByteString;
begin
 Result:=PsrChain(node)^.GetPrintName;
end;

class function ntChain.GetRef(node:PsrNode):Pointer;
begin
 Result:=@PsrChain(node)^.ID;
end;

//

class Procedure ntDescriptor.zero_read(node:PsrNode);
begin
 With PsrDescriptor(node)^ do
 begin
  pType^.mark_read(node);
 end;
end;

class Procedure ntDescriptor.zero_unread(node:PsrNode);
begin
 With PsrDescriptor(node)^ do
 begin
  pType^.mark_unread(node);
 end;
end;

class Function ntDescriptor.GetPtype(node:PsrNode):PsrNode;
begin
 Result:=PsrDescriptor(node)^.FType;
end;

class Function ntDescriptor.GetStorageClass(node:PsrNode):DWORD;
begin
 Result:=PsrDescriptor(node)^.FStorage;
end;

//

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

function TsrDataLayout.Fetch(lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1):PsrChain;
var
 node:TsrChain;
begin
 node:=Default(TsrChain);
 if (lvl_0<>nil) then
 begin
  node.key.lvl_0:=lvl_0^;
 end;
 if (lvl_1<>nil) then
 begin
  node.key.lvl_1:=lvl_1^;
 end;

 Result:=FList.Find(@node);
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrChain));
  Move(node,Result^,SizeOf(TsrChain));

  Result^.Init(@Self);

  FList.Insert(Result);
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

Function TsrDataLayout.NextChain(node:PsrChain):PsrChain;
begin
 Result:=FList.Next(node);
end;

Function TsrDataLayout.PrevChain(node:PsrChain):PsrChain;
begin
 Result:=FList.Prev(node);
end;

function TsrDataLayout.EnumChain(cb:TChainCb):Integer;
var
 node:PsrChain;
begin
 Result:=0;
 node:=First;
 While (node<>nil) do
 begin
  if node^.IsUsed then
  begin
   Result:=Result+cb(node);
  end;
  node:=node^.Next;
 end;
end;

function TsrDataLayout.GetData:Pointer;
begin
 Result:=nil;
 if (pData<>nil) then
  Case key.rtype of
   rtRoot,
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
   rtRoot,
   rtBufPtr2:Result:=4;
   rtVSharp4:Result:=PVSharpResource4(pData)^.stride;
   else;
  end;
end;

function TsrDataLayout.IsUserData:Boolean; inline;
begin
 Result:=(key.rtype=rtRoot);
end;

function TsrDataLayout.GetTypeChar:Char;
begin
 Result:=#0;
 case key.rtype of
  rtRoot   :Result:='R';
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

procedure TsrDataLayoutList.Init(Emit:TCustomEmit);
begin
 FTop.FEmit:=Emit;
 FNTree.Insert(@FTop);
end;

procedure TsrDataLayoutList.SetUserData(pData:Pointer);
begin
 FTop.pData:=pData;
end;

function TsrDataLayoutList.pRoot:PsrDataLayout;
begin
 Result:=@FTop;
end;

function TsrDataLayoutList.Fetch(p:PsrDataLayout;o:PtrUint;t:TsrResourceType):PsrDataLayout;
var
 node:TsrDataLayout;
 pData:Pointer;
begin
 Assert(p<>nil);
 node:=Default(TsrDataLayout);
 node.FEmit:=FTop.FEmit;
 node.key.parent:=p;
 node.key.offset:=o;
 node.key.rtype :=t;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=FTop.FEmit.Alloc(SizeOf(TsrDataLayout));
  Move(node,Result^,SizeOf(TsrDataLayout));
  FNTree.Insert(Result);

  Result^.FID:=-1;

  pData:=p^.GetData;
  if (pData<>nil) then
   case t of
    rtRoot,
    rtBufPtr2,
    rtFunPtr2:Result^.pData:={%H-}Pointer(PPtrUint(pData+o)^ and (not 3));
    rtVSharp4,
    rtSSharp4,
    rtTSharp4,
    rtTSharp8:Result^.pData:=pData+o;
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
  rtRoot   :Result:=2;
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

 Result:=Fetch(chain[0]^.Parent,chain[0]^.offset,rtype);
end;

function TsrDataLayoutList.EnumChain(cb:TChainCb):Integer;
var
 node:PsrDataLayout;
begin
 Result:=0;
 if (cb=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  Result:=Result+node^.EnumChain(cb);
  node:=Next(node);
 end;
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

procedure TsrDataLayoutList.AllocSourceExtension;
var
 pDebugInfoList:PsrDebugInfoList;
 node:PsrDataLayout;
begin
 pDebugInfoList:=FTop.FEmit.GetDebugInfoList;
 node:=First;
 While (node<>nil) do
 begin
  pDebugInfoList^.OpSource(node^.GetString);
  node:=Next(node);
 end;
end;

procedure TsrDataLayoutList.AllocFuncExt;
var
 pDebugInfoList:PsrDebugInfoList;
 pHeap:PsrCodeHeap;
 node:PsrDataLayout;
 block:PsrCodeBlock;
begin
 pDebugInfoList:=FTop.FEmit.GetDebugInfoList;
 pHeap:=FTop.FEmit.GetCodeHeap;
 node:=First;
 While (node<>nil) do
 begin
  if (node^.key.rtype=rtFunPtr2) then
  begin
   block:=pHeap^.FindByPtr(node^.pData);
   if (block<>nil) then
   begin
    pDebugInfoList^.OpSource(node^.GetFuncString(block^.Size));
   end;
  end;
  node:=Next(node);
 end;
end;

//

Procedure TsrChain.Init(L:PsrDataLayout);
begin
 fntype:=ntChain;
 FParent:=L;
end;

function TsrChainLvl_1.c(n1,n2:PsrChainLvl_1):Integer;
begin
 //first pIndex backward
 Result:=Integer(n1^.pIndex<n2^.pIndex)-Integer(n1^.pIndex>n2^.pIndex);
 if (Result<>0) then Exit;

 //second stride forward
 Result:=Integer(n1^.stride>n2^.stride)-Integer(n1^.stride<n2^.stride);
end;

function TsrChainLvl_0.c(n1,n2:PsrChainLvl_0):Integer;
begin
 //first size backward
 Result:=Integer(n1^.size<n2^.size)-Integer(n1^.size>n2^.size);
 if (Result<>0) then Exit;

 //second offset forward
 Result:=Integer(n1^.offset>n2^.offset)-Integer(n1^.offset<n2^.offset);
end;

function TsrChain.c(n1,n2:PsrChain):Integer;
begin
 //first lvl_1
 Result:=TsrChainLvl_1.c(@n1^.key.lvl_1,@n2^.key.lvl_1);
 if (Result<>0) then Exit;

 //second lvl_0
 Result:=TsrChainLvl_0.c(@n1^.key.lvl_0,@n2^.key.lvl_0);
end;

function TsrChain.Emit:TCustomEmit;
begin
 Result:=FParent^.FEmit;
end;

Procedure TsrChain.SetWriter(t:PsrNode);
begin
 if (@Self=nil) then Exit;
 if (FWriter=t) then Exit;

 if isUsed then
 begin
        t^.mark_read  (@Self);
  FWriter^.mark_unread(@Self);
 end;
 FWriter:=t;
end;

Function TsrChain.GetWriter:PsrNode;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 Result:=FWriter;
end;

Procedure TsrChain.SetBuffer(t:PsrNode);
begin
 if (@Self=nil) then Exit;
 if (FBuffer=t) then Exit;

 if isUsed then
 begin
        t^.mark_read  (@Self);
  FBuffer^.mark_unread(@Self);
 end;
 FBuffer:=t;
end;

Function TsrChain.GetBuffer:PsrNode;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 Result:=FBuffer;
end;

Procedure TsrChain.SetRegType(rtype:TsrDataType);
var
 pTypeList:PsrTypeList;
 FType:PsrType;
 node:PVNode;
 pLine:PspirvOp;
begin
 Assert(rtype.BitSize div 8=size);

 Fdtype:=rtype;

 pTypeList:=Emit.GetTypeList;
 FType:=pTypeList^.Fetch(rtype);

 node:=FList.pHead;
 While (node<>nil) do
 begin
  pLine:=node^.pLine;

  Case pLine^.OpId of

   Op.OpLoad:
    begin
     pLine^.pDst^.PrepType(ord(rtype));
     pLine^.pType:=Ftype;
    end;

   Op.OpStore:
    begin
     pLine^.ParamFirst^.Value^.PrepType(ord(rtype));
    end;

   else;
  end;

  node:=node^.pNext;
 end;
end;

Procedure TsrChain.UpdateRegType;
var
 pTypeList:PsrTypeList;
 pBitcastList:PsrBitcastList;
 FType:PsrType;
 node:PVNode;
 pLine:PspirvOp;
 dst:PsrRegNode;
 old,rtype:TsrDataType;
begin
 rtype:=Fdtype;

 pTypeList:=Emit.GetTypeList;
 FType:=pTypeList^.Fetch(rtype);

 pBitcastList:=Emit.GetBitcastList;

 node:=FList.pHead;
 While (node<>nil) do
 begin
  pLine:=node^.pLine;

  Case pLine^.OpId of

   Op.OpLoad:
    begin
     pLine^.pDst^.PrepType(ord(rtype));
     pLine^.pType:=Ftype;

     dst:=pLine^.pDst^.AsType(ntReg);
     if (dst<>nil) then
     begin
      old:=dst^.dtype;
      if (old<>dtUnknow) and (not CompareType(rtype,old)) then
      begin
       //OpLoad -> new -> dst
       pBitcastList:=Emit.GetBitcastList;
       dst:=pBitcastList^.FetchDstr(rtype,dst);
       pLine^.pDst:=dst;
      end;
     end;
    end;

   Op.OpStore:
    begin
     pLine^.ParamFirst^.Value^.PrepType(ord(rtype));

     dst:=pLine^.ParamFirst^.Value^.AsType(ntReg);
     if (dst<>nil) then
     begin
      old:=dst^.dtype;
      if (old<>dtUnknow) and (rtype<>old) then
      begin
       //OpStore <- new <- dst
       pBitcastList:=Emit.GetBitcastList;
       dst:=pBitcastList^.FetchRead(rtype,dst);
       pLine^.ParamFirst^.Value:=dst;
      end;
     end;
    end;

   else;
  end;

  node:=node^.pNext;
 end;
end;

Procedure TsrChain.PrepType(new:TsrDataType);
var
 old:TsrDataType;
begin
 if (new=dtUnknow) then Exit;
 old:=Fdtype;
 if is_unprep_type(old,new,True) then
 begin
  old:=StoreType(new);
  SetRegType(old);
 end;
end;

procedure TsrChain.AddLine(pLine:PspirvOp);
var
 node:PVNode;
begin
 node:=Emit.Alloc(SizeOf(TVNode));
 node^.pLine:=pLine;
 FList.Push_tail(node);
end;

function TsrChain.FirstLine:PspirvOp;
var
 node:PVNode;
begin
 Result:=nil;
 node:=FList.pHead;
 if (node<>nil) then
 begin
  Result:=node^.pLine;
 end;
end;

procedure TsrChain.FetchLoad(pLine:PspirvOp;dst:PsrRegNode);
var
 pTypeList:PsrTypeList;
begin
 Assert(dst<>nil);

 PrepType(dst^.dtype);

 pTypeList:=Emit.GetTypeList;
 pLine:=PspirvOp(Emit.OpLoad(pLine,pTypeList^.Fetch(Fdtype),dst,@Self));

 AddLine(pLine);
end;

Procedure TsrChain.FetchStore(pLine:PspirvOp;src:PsrRegNode);
begin
 if (src=nil) then Exit;

 PrepType(src^.dtype);

 pLine:=PspirvOp(Emit.OpStore(pLine,@Self,src));

 AddLine(pLine);
end;

function TsrChain.GetPrintName:RawByteString;
begin
 Assert(ID.Alloc);
 Result:='ac'+IntToStr(ID.ID);
end;

//

procedure TsrDescriptor.InitVar(Emit:TCustomEmit);
var
 pVariableList:PsrVariableList;
begin
 if (Emit=nil) then Exit;
 if (FVar<>nil) then Exit;
 //
 pVariableList:=Emit.GetVariableList;
 //
 FVar:=pVariableList^.Fetch;
 FVar^.pSource:=@Self;
end;

procedure TsrDescriptor.InitType(rtype:TsrDataType;Emit:TCustomEmit);
var
 pTypeList:PsrTypeList;
begin
 if (Emit=nil) then Exit;
 if (FType<>nil) then Exit;
 //
 pTypeList:=Emit.GetTypeList;
 //
 SetType(pTypeList^.Fetch(rtype));
end;

procedure TsrDescriptor.SetType(t:PsrType);
begin
 if (FType=t) then Exit;

 if isUsed then
 begin
      t^.mark_read  (@Self);
  FType^.mark_unread(@Self);
 end;
 FType:=t;
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
 offset:=chains[0]^.offset;
 For i:=1 to count-1 do
 begin
  t:=chains[i-1]^.size;
  offset:=offset+t;
  if (chains[i]=nil) then Exit;
  if (chains[i]^.parent<>parent) then Exit;
  t:=chains[i]^.offset;
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
  if (chains[i]^.key.lvl_1.pIndex<>nil) then Exit;
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

