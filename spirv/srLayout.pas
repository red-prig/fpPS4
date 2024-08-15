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
 srDecorate,
 srConfig;

type
 TsrResourceType=(
  rtRoot,
  rtImmData,
  rtBufPtr2,
  rtFunPtr2,
  rtVSharp4,
  rtSSharp4,
  rtTSharp4,
  rtTSharp8,
  rtLDS,
  rtGDS
 );

type
 TsrDataLayout=class;

 PsrChainLvl_1=^TsrChainLvl_1;
 TsrChainLvl_1=object
  pIndex:TsrRegNode;
  stride:PtrUint;
  function c(n1,n2:PsrChainLvl_1):Integer; static;
 end;

 PsrChainLvl_0=^TsrChainLvl_0;
 TsrChainLvl_0=object
  size  :PtrUint;
  offset:PtrUint;
  function c(n1,n2:PsrChainLvl_0):Integer; static;
 end;

 TsrChainFlags=bitpacked record
  dtype:TsrDataType; //dtUnknow=weak type
  GLC  :Boolean;     //Coherent
  SLC  :Boolean;     //Volatile
 end;

type
 PsrChainKey=^TsrChainKey;
 TsrChainKey=packed record
  lvl_1:TsrChainLvl_1;
  lvl_0:TsrChainLvl_0;
  Flags:TsrChainFlags;
 end;

 TsrChain=class(TsrNode)
  public
   pPrev,pNext :TsrChain;
   pLeft,pRight:TsrChain;
   class function c(n1,n2:PsrChainKey):Integer; static;
  private
   //--
   ID:TsrRefId; //post id
   FParent:TsrDataLayout;
   key:TsrChainKey;
   FBuffer:TsrNode;
   FWriter:TsrNode;
   Fdtype:TsrDataType;
   FList:TDependenceNodeList;
   Procedure SetWriter(t:TsrNode);
   Function  GetWriter:TsrNode;
   Procedure SetBuffer(t:TsrNode);
   Function  GetBuffer:TsrNode;
   Procedure SetRegType(rtype:TsrDataType);
   Procedure SetIndex(t:TsrRegNode);
   Procedure SetOffset(t:PtrUint);
  public
   pField:TObject;
   //
   FUndoIndex :TsrNode;
   FUndoOffset:PtrUint;
   //
   Procedure _zero_read   ;                     override;
   Procedure _zero_unread ;                     override;
   Procedure _SetWriter   (w,line:TsrNode);     override;
   Procedure _ResetWriter (w:TsrNode);          override;
   function  _Down        :TsrNode;             override;
   function  _Next        :TsrNode;             override;
   function  _Prev        :TsrNode;             override;
   function  _Parent      :TsrNode;             override;
   Function  _GetStorageClass:DWORD;            override;
   Procedure _PrepType    (node:PPrepTypeNode); override;
   function  _GetPrintName:RawByteString;       override;
   function  _GetRef      :Pointer;             override;
   //
   property  Parent:TsrDataLayout read FParent;
   property  pIndex:TsrRegNode    read key.lvl_1.pIndex write SetIndex;
   property  stride:PtrUint       read key.lvl_1.stride;
   property  size  :PtrUint       read key.lvl_0.size;
   property  offset:PtrUint       read key.lvl_0.offset write SetOffset;
   property  Flags:TsrChainFlags  read key.Flags;
   property  dtype :TsrDataType   read Fdtype    write SetRegType;
   property  pWriter:TsrNode      read GetWriter write SetWriter;
   property  pBuffer:TsrNode      read GetBuffer write SetBuffer;
   function  dweak:Boolean;
   Procedure Init(L:TsrDataLayout);
   Procedure UpdateRegType;
   Procedure PrepType(new:TsrDataType);
   procedure AddLine(pLine:TSpirvOp);
   function  FirstLine:TSpirvOp;
   procedure FetchLoad (pLine:TSpirvOp;dst:TsrRegNode);
   Procedure FetchStore(pLine:TSpirvOp;src:TsrRegNode);
   function  GetPrintName:RawByteString;
 end;

 ntChain=TsrChain;

 TsrChains=array[0..7] of TsrChain;

 TChainCb=function(node:TsrChain):Integer of object;

 //----

 PsrDataLayoutKey=^TsrDataLayoutKey;
 TsrDataLayoutKey=packed record
  parent:TsrDataLayout;
  offset:PtrUint;
  rtype :TsrResourceType;
 end;

 TsrDataLayout=class
  type
   TChainList=specialize TNodeListClass<TsrChain>;
   TChainTree=specialize TNodeTreeClass<TsrChain>;
  var
   pLeft,pRight:TsrDataLayout;
   //----
   key:TsrDataLayoutKey;
   pData :Pointer;
   FID   :Integer;
   FOrder:Integer;
   FSetid:Integer;
   FCache:Integer;
   FEmit :TCustomEmit;
   FList :TChainList;
   FTree :TChainTree;
  class function c(n1,n2:PsrDataLayoutKey):Integer; static;
  function  Order:Integer;
  function  Fetch(lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1;cflags:Byte=0):TsrChain;
  Procedure UpdateCache;
  Function  First:TsrChain;
  Function  Last :TsrChain;
  function  EnumChain(cb:TChainCb):Integer;
  function  GetData:Pointer;
  function  IsUserData:Boolean; inline;
  function  IsLocalDataShare:Boolean; inline;
  function  IsGlobalDataShare:Boolean; inline;
  function  UseBitcast:Boolean;
  function  GetStride:PtrUint;
  function  GetTypeChar:Char;
  function  GetString:RawByteString;
  function  GetFuncString(LEN:DWORD):RawByteString;
 end;

 PsrDataImmKey=^TsrDataImmKey;
 TsrDataImmKey=record
  FImmSize:PtrUint;
  pData   :PDWORD;
 end;

 TsrDataImm=class
  var
   pLeft,pRight:TsrDataImm;
   //----
   key:TsrDataImmKey;
   FImmOffset:PtrUint;
  class function c(a,b:PsrDataImmKey):Integer; static;
  function GetStringDword(i:PtrUint):RawByteString;
 end;

 PsrDataLayoutList=^TsrDataLayoutList;
 TsrDataLayoutList=object
  type
   TNodeTree=specialize TNodeTreeClass<TsrDataLayout>;
   TDataImmTree=specialize TNodeTreeClass<TsrDataImm>;
  var
   FTop      :TsrDataLayout;
   FTree     :TNodeTree;
   FOrder    :Integer;
   FImmOffset:DWORD;
   FImmData  :TDataImmTree;
  procedure Init(Emit:TCustomEmit);
  procedure SetUserData(pData:Pointer);
  function  pRoot:TsrDataLayout;
  function  Fetch(p:TsrDataLayout;o:PtrUint;t:TsrResourceType;pData:Pointer):TsrDataLayout;
  Function  First:TsrDataLayout;
  Function  Next(node:TsrDataLayout):TsrDataLayout;
  function  Grouping(const chain:TsrChains;rtype:TsrResourceType):TsrDataLayout;
  function  FetchImmData(size:Integer;pData:Pointer):TsrDataImm;
  function  FetchImm(pData:PDWORD;rtype:TsrResourceType):TsrDataLayout;
  function  FetchLDS():TsrDataLayout;
  function  FetchGDS():TsrDataLayout;
  function  EnumChain(cb:TChainCb):Integer;
  Procedure AllocID;
  procedure AllocSourceExtension;
  procedure AllocFuncExt;
  procedure AllocImmExt;
 end;

//----

 TsrDescriptor=class(TsrNode)
  protected
   FVar:TsrVariable;
   FType:TsrType;
   FStorage:DWORD;
   FBinding:Integer;
   procedure InitVar();
   procedure InitType(rtype:TsrDataType);
   procedure SetType(t:TsrType);
  public
   Flags:bitpacked record
    Coherent:Boolean;
    Volatile:Boolean;
    Aliased :Boolean;
    Bitcast :Boolean;
   end;
   //
   Procedure _zero_read      ;         override;
   Procedure _zero_unread    ;         override;
   Function  _GetPtype       :TsrNode; override;
   Function  _GetStorageClass:DWORD;   override;
   //
   property  pVar:TsrVariable read FVar;
   property  pType:TsrType    read FType write SetType;
 end;

 ntDescriptor=TsrDescriptor;

function is_consistents(const chains:TsrChains;count:Byte):Boolean;
function is_no_index_chains(const chains:TsrChains;count:Byte):Boolean;
function is_userdata_chains(const chains:TsrChains;count:Byte):Boolean;
function GetResourceSizeDw(r:TsrResourceType):Byte;

operator := (i:TsrNode):TsrChain; inline;

function cflags(dtype:TsrDataType;GLC:Byte=0;SLC:Byte=0):Byte;

implementation

operator := (i:TsrNode):TsrChain; inline;
begin
 Result:=TsrChain(Pointer(i)); //typecast hack
end;

function cflags(dtype:TsrDataType;GLC:Byte=0;SLC:Byte=0):Byte;
begin
 TsrChainFlags(Result).dtype:=dtype;
 TsrChainFlags(Result).GLC  :=(GLC<>0);
 TsrChainFlags(Result).SLC  :=(SLC<>0);
end;

Procedure TsrChain._zero_read;
begin
 key.lvl_1.pIndex.mark_read(Self);
 FBuffer.mark_read(Self);
end;

Procedure TsrChain._zero_unread;
begin
 key.lvl_1.pIndex.mark_unread(Self);
 FBuffer.mark_unread(Self);
end;

Procedure TsrChain._SetWriter(w,line:TsrNode);
begin
 SetWriter(w);
end;

Procedure TsrChain._ResetWriter(w:TsrNode);
begin
 if (FWriter=w) then
 begin
  SetWriter(nil);
 end;
end;

function TsrChain._Down:TsrNode;
begin
 Result:=FWriter;
end;

function TsrChain._Next:TsrNode;
begin
 Result:=pNext;
end;

function TsrChain._Prev:TsrNode;
begin
 Result:=pPrev;
end;

function TsrChain._Parent:TsrNode;
begin
 Result:=TsrNode(FParent);
end;

Procedure TsrChain._PrepType(node:PPrepTypeNode);
begin
 TsrChain(node^.dnode).PrepType(TsrDataType(node^.rtype));
 node^.dnode:=nil;
end;

Function TsrChain._GetStorageClass:DWORD;
begin
 Result:=FBuffer.GetStorageClass;
end;

function TsrChain._GetPrintName:RawByteString;
begin
 Result:=GetPrintName;
end;

function TsrChain._GetRef:Pointer;
begin
 Result:=@ID;
end;

//

Procedure TsrDescriptor._zero_read;
begin
 pType.mark_read(Self);
end;

Procedure TsrDescriptor._zero_unread;
begin
 pType.mark_unread(Self);
end;

Function TsrDescriptor._GetPtype:TsrNode;
begin
 Result:=FType;
end;

Function TsrDescriptor._GetStorageClass:DWORD;
begin
 Result:=FStorage;
end;

//

class function TsrDataLayout.c(n1,n2:PsrDataLayoutKey):Integer;
begin
 //first parent
 Result:=ord(n1^.parent.Order>n2^.parent.Order)-ord(n1^.parent.Order<n2^.parent.Order);
 if (Result<>0) then Exit;
 //second offset
 Result:=ord(n1^.offset>n2^.offset)-ord(n1^.offset<n2^.offset);
 if (Result<>0) then Exit;
 //third rtype
 Result:=ord(n1^.rtype>n2^.rtype)-ord(n1^.rtype<n2^.rtype);
end;

function TsrDataLayout.Order:Integer;
begin
 Result:=0;
 if (Self<>nil) then
 begin
  Result:=FOrder;
 end;
end;

function TsrDataLayout.Fetch(lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1;cflags:Byte=0):TsrChain;
var
 _key:TsrChainKey;
begin
 _key:=Default(TsrChainKey);
 //
 if (lvl_0<>nil) then
 begin
  _key.lvl_0:=lvl_0^;
 end;
 //
 if (lvl_1<>nil) then
 begin
  _key.lvl_1:=lvl_1^;
 end;
 //
 _key.Flags:=TsrChainFlags(cflags);
 //
 Result:=FTree.Find(@_key);
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrChain>;
  Result.Init(Self);
  Result.key   :=_key;
  Result.Fdtype:=_key.Flags.dtype;
  FTree.Insert(Result);
  //
  Inc(FSetid);
 end;
end;

Procedure TsrDataLayout.UpdateCache;
var
 node:TsrChain;
begin
 if (FSetid<>FCache) then
 begin
  FCache:=FSetid;
  //Clear
  repeat
   node:=FList.Pop_tail;
  until (node=nil);
  //Load
  node:=FTree.Min;
  while (node<>nil) do
  begin
   FList.Push_tail(node);
   //
   node:=FTree.Next(node);
  end;
 end;
end;

Function TsrDataLayout.First:TsrChain;
begin
 UpdateCache;
 Result:=FList.pHead;
end;

Function TsrDataLayout.Last:TsrChain;
begin
 UpdateCache;
 Result:=FList.pTail;
end;

function TsrDataLayout.EnumChain(cb:TChainCb):Integer;
var
 node:TsrChain;
begin
 Result:=0;
 node:=First;
 While (node<>nil) do
 begin
  if node.IsUsed then
  begin
   Result:=Result+cb(node);
  end;
  node:=node.Next;
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
   rtImmData:Result:=TsrDataImm(pData).key.pData;
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

function TsrDataLayout.IsLocalDataShare:Boolean; inline;
begin
 Result:=(key.rtype=rtLDS);
end;

function TsrDataLayout.IsGlobalDataShare:Boolean; inline;
begin
 Result:=(key.rtype=rtGDS);
end;

function TsrDataLayout.UseBitcast:Boolean;
var
 pConfig:PsrConfig;
begin
 pConfig:=FEmit.GetConfig;

 if IsLocalDataShare then
 begin
  if (FEmit.GetExecutionModel=ExecutionModel.GLCompute) then
  begin
   Result:=pConfig^.BitcastPointer.Workgroup;
  end else
  begin
   //private
   Result:=true;
  end;
 end else
 begin
  Result:=pConfig^.BitcastPointer.Storage;
 end;
end;

function TsrDataLayout.GetTypeChar:Char;
begin
 Result:=#0;
 case key.rtype of
  rtRoot   :Result:='R';
  rtImmData:Result:='D';
  rtBufPtr2:Result:='B';
  rtFunPtr2:Result:='F';
  rtVSharp4:Result:='V';
  rtSSharp4:Result:='S';
  rtTSharp4:Result:='t';
  rtTSharp8:Result:='T';
  rtLDS    :Result:='L';
  rtGDS    :Result:='G';
 end;
end;

function TsrDataLayout.GetString:RawByteString;
var
 PID:DWORD;
begin
 PID:=0;
 if (key.parent<>nil) then
 begin
  PID:=key.parent.FID;
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

class function TsrDataImm.c(a,b:PsrDataImmKey):Integer;
begin
 //first size
 Result:=ord(a^.FImmSize>b^.FImmSize)-ord(a^.FImmSize<b^.FImmSize);
 if (Result<>0) then Exit;
 //second data
 Result:=CompareByte(a^.pData^,b^.pData^,a^.FImmSize);
end;

function TsrDataImm.GetStringDword(i:PtrUint):RawByteString;
begin
 Result:='!D;'+HexStr(key.pData[i],8);
end;

procedure TsrDataLayoutList.Init(Emit:TCustomEmit);
begin
 FTop:=Emit.specialize New<TsrDataLayout>;
 FTop.FEmit:=Emit;
 FTree.Insert(FTop);
end;

procedure TsrDataLayoutList.SetUserData(pData:Pointer);
begin
 FTop.pData:=pData;
end;

function TsrDataLayoutList.pRoot:TsrDataLayout;
begin
 Result:=FTop;
end;

function TsrDataLayoutList.Fetch(p:TsrDataLayout;o:PtrUint;t:TsrResourceType;pData:Pointer):TsrDataLayout;
var
 key:TsrDataLayoutKey;
begin
 Assert(p<>nil);
 key:=Default(TsrDataLayoutKey);
 key.parent:=p;
 key.offset:=o;
 key.rtype :=t;
 //
 Result:=FTree.Find(@key);
 if (Result=nil) then
 begin
  Inc(FOrder);

  Result:=FTop.FEmit.specialize New<TsrDataLayout>;
  Result.FID   :=-1;
  Result.FOrder:=FOrder;
  Result.FEmit :=FTop.FEmit;
  Result.key   :=key;

  FTree.Insert(Result);

  if (pData<>nil) then
   case t of
    rtRoot,
    rtBufPtr2,
    rtFunPtr2:Result.pData:={%H-}Pointer(PPtrUint(pData+o)^ and (not 3));
    rtVSharp4,
    rtSSharp4,
    rtTSharp4,
    rtTSharp8:Result.pData:=pData+o;
    rtImmData:Result.pData:=pData;
   end;

 end;
end;

Function TsrDataLayoutList.First:TsrDataLayout;
begin
 Result:=FTree.Min;
end;

Function TsrDataLayoutList.Next(node:TsrDataLayout):TsrDataLayout;
begin
 Result:=FTree.Next(node);
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

function TsrDataLayoutList.Grouping(const chain:TsrChains;rtype:TsrResourceType):TsrDataLayout;
var
 parent:TsrDataLayout;
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

 parent:=chain[0].Parent;

 Result:=Fetch(parent,chain[0].offset,rtype,parent.GetData);
end;

function TsrDataLayoutList.FetchImmData(size:Integer;pData:Pointer):TsrDataImm;
var
 key:TsrDataImmKey;
 dst:TsrDataImm;
begin
 key:=Default(TsrDataImmKey);
 key.FImmSize:=size;
 key.pData   :=pData;

 dst:=FImmData.Find(@key);
 if (dst=nil) then
 begin
  dst:=FTop.FEmit.specialize New<TsrDataImm>;

  dst.key:=key;
  dst.FImmOffset:=FImmOffset;
  dst.key.pData :=FTop.FEmit.Alloc(size);

  Move(pData^,dst.key.pData^,size);

  FImmData.Insert(dst);

  FImmOffset:=FImmOffset+size;
 end;

 Result:=dst;
end;

function TsrDataLayoutList.FetchImm(pData:PDWORD;rtype:TsrResourceType):TsrDataLayout;
var
 parent:TsrDataLayout;
 dst :TsrDataImm;
 size:Integer;
begin
 Result:=nil;
 size:=GetResourceSizeDw(rtype)*SizeOf(DWORD);

 dst:=FetchImmData(size,pData);

 parent:=Fetch(pRoot,dst.FImmOffset,rtImmData,dst);

 Result:=Fetch(parent,0,rtype,parent.GetData);
end;

function TsrDataLayoutList.FetchLDS():TsrDataLayout;
begin
 Result:=Fetch(pRoot,0,rtLDS,nil);
end;

function TsrDataLayoutList.FetchGDS():TsrDataLayout;
begin
 Result:=Fetch(pRoot,0,rtGDS,nil);
end;

function TsrDataLayoutList.EnumChain(cb:TChainCb):Integer;
var
 node:TsrDataLayout;
begin
 Result:=0;
 if (cb=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  Result:=Result+node.EnumChain(cb);
  node:=Next(node);
 end;
end;

Procedure TsrDataLayoutList.AllocID;
var
 node:TsrDataLayout;
 FID:Integer;
begin
 FID:=1;
 node:=First;
 While (node<>nil) do
 begin
  if (node.FID=-1) then
  begin
   node.FID:=FID;
   Inc(FID);
  end;
  node:=Next(node);
 end;
end;

procedure TsrDataLayoutList.AllocSourceExtension;
var
 pDebugInfoList:PsrDebugInfoList;
 node:TsrDataLayout;
begin
 pDebugInfoList:=FTop.FEmit.GetDebugInfoList;
 node:=First;
 While (node<>nil) do
 begin
  pDebugInfoList^.OpSource(node.GetString);
  node:=Next(node);
 end;
 //
 AllocFuncExt;
 AllocImmExt;
end;

procedure TsrDataLayoutList.AllocFuncExt;
var
 pDebugInfoList:PsrDebugInfoList;
 pHeap:PsrCodeHeap;
 node:TsrDataLayout;
 block:TsrCodeBlock;
begin
 pDebugInfoList:=FTop.FEmit.GetDebugInfoList;
 pHeap:=FTop.FEmit.GetCodeHeap;
 node:=First;
 While (node<>nil) do
 begin
  if (node.key.rtype=rtFunPtr2) then
  begin
   block:=pHeap^.FindByPtr(node.pData);
   if (block<>nil) then
   begin
    pDebugInfoList^.OpSource(node.GetFuncString(block.Size));
   end;
  end;
  node:=Next(node);
 end;
end;

procedure TsrDataLayoutList.AllocImmExt;
var
 pDebugInfoList:PsrDebugInfoList;
 node:TsrDataLayout;
 imm:TsrDataImm;
 i,c:PtrUint;
begin
 pDebugInfoList:=FTop.FEmit.GetDebugInfoList;
 node:=First;
 While (node<>nil) do
 begin
  if (node.key.rtype=rtImmData) then
  begin
   imm:=TsrDataImm(node.pData);

   c:=imm.key.FImmSize div SizeOf(DWORD);

   if (c<>0) then
   For i:=0 to c-1 do
   begin
    pDebugInfoList^.OpSource(imm.GetStringDword(i));
   end;

  end;
  node:=Next(node);
 end;
end;

//

function TsrChain.dweak:Boolean;
begin
 Result:=(key.Flags.dtype=dtUnknow);
end;

Procedure TsrChain.Init(L:TsrDataLayout);
begin
 FParent:=L;
end;

function TsrChainLvl_1.c(n1,n2:PsrChainLvl_1):Integer;
begin
 //1 pIndex backward (order sort)
 Result:=ord(n1^.pIndex.Order<n2^.pIndex.Order)-ord(n1^.pIndex.Order>n2^.pIndex.Order);
 if (Result<>0) then Exit;

 //2 stride forward
 Result:=ord(n1^.stride>n2^.stride)-ord(n1^.stride<n2^.stride);
end;

function TsrChainLvl_0.c(n1,n2:PsrChainLvl_0):Integer;
begin
 //1 size backward
 Result:=ord(n1^.size<n2^.size)-ord(n1^.size>n2^.size);
 if (Result<>0) then Exit;

 //2 offset forward
 Result:=ord(n1^.offset>n2^.offset)-ord(n1^.offset<n2^.offset);
end;

class function TsrChain.c(n1,n2:PsrChainKey):Integer;
begin

 //1 lvl_0
 Result:=TsrChainLvl_0.c(@n1^.lvl_0,@n2^.lvl_0);
 if (Result<>0) then Exit;

 //2 lvl_1
 Result:=TsrChainLvl_1.c(@n1^.lvl_1,@n2^.lvl_1);
 if (Result<>0) then Exit;

 //3 flags
 Result:=ord(Byte(n1^.Flags)>Byte(n2^.Flags))-ord(Byte(n1^.Flags)<Byte(n2^.Flags));
end;

Procedure TsrChain.SetWriter(t:TsrNode);
begin
 if (Self=nil) then Exit;
 if (FWriter=t) then Exit;

 if isUsed then
 begin
        t.mark_read  (Self);
  FWriter.mark_unread(Self);
 end;
 FWriter:=t;
end;

Function TsrChain.GetWriter:TsrNode;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 Result:=FWriter;
end;

Procedure TsrChain.SetBuffer(t:TsrNode);
begin
 if (Self=nil) then Exit;
 if (FBuffer=t) then Exit;

 if isUsed then
 begin
        t.mark_read  (Self);
  FBuffer.mark_unread(Self);
 end;
 FBuffer:=t;
end;

Function TsrChain.GetBuffer:TsrNode;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 Result:=FBuffer;
end;

Procedure TsrChain.SetRegType(rtype:TsrDataType);
var
 pTypeList:PsrTypeList;
 FType:TsrType;
 node:TDependenceNode;
 pLine:TSpirvOp;
begin
 if (Fdtype=rtype) then Exit;

 Assert(rtype.BitSize div 8=size);

 Fdtype:=rtype;

 pTypeList:=Emit.GetTypeList;
 FType:=pTypeList^.Fetch(rtype);

 node:=FList.pHead;
 While (node<>nil) do
 begin
  pLine:=node.pNode;

  Case pLine.OpId of

   Op.OpLoad:
    begin
     pLine.pDst.PrepType(ord(rtype));
     pLine.pType:=Ftype;
    end;

   Op.OpStore,
   Op.OpAtomicStore,
   Op.OpAtomicExchange,
   Op.OpAtomicCompareExchange,
   Op.OpAtomicCompareExchangeWeak,
   Op.OpAtomicIIncrement,
   Op.OpAtomicIDecrement,
   Op.OpAtomicIAdd,
   Op.OpAtomicISub,
   Op.OpAtomicSMin,
   Op.OpAtomicUMin,
   Op.OpAtomicSMax,
   Op.OpAtomicUMax,
   Op.OpAtomicAnd,
   Op.OpAtomicOr,
   Op.OpAtomicXor:
    begin
     pLine.ParamNode(1).Value.PrepType(ord(rtype));
    end;

   else;
  end;

  node:=node.pNext;
 end;
end;

Procedure TsrChain.SetIndex(t:TsrRegNode);
begin
 if (Self=nil) then Exit;
 if (key.lvl_1.pIndex=t) then Exit;

 //update count
 if isUsed then
 begin
                 t.mark_read  (Self);
  key.lvl_1.pIndex.mark_unread(Self);
 end;

 //unlink
 FParent.FTree.Delete(Self);
 //set
 key.lvl_1.pIndex:=t;
 //link
 FParent.FTree.Insert(Self);
end;

Procedure TsrChain.SetOffset(t:PtrUint);
begin
 if (Self=nil) then Exit;
 if (key.lvl_0.offset=t) then Exit;

 //unlink
 FParent.FTree.Delete(Self);
 //set
 key.lvl_0.offset:=t;
 //link
 FParent.FTree.Insert(Self);
end;

Procedure TsrChain.UpdateRegType;
var
 pTypeList:PsrTypeList;
 pBitcastList:PsrBitcastList;
 FType:TsrType;
 node:TDependenceNode;
 pLine:TSpirvOp;
 dst:TsrRegNode;
 old,rtype:TsrDataType;
begin
 rtype:=Fdtype;

 pTypeList:=Emit.GetTypeList;
 FType:=pTypeList^.Fetch(rtype);

 pBitcastList:=Emit.GetBitcastList;

 node:=FList.pHead;
 While (node<>nil) do
 begin
  pLine:=node.pNode;

  Case pLine.OpId of

   Op.OpLoad:
    begin
     pLine.pDst.PrepType(ord(rtype));
     pLine.pType:=Ftype;

     dst:=pLine.pDst.specialize AsType<ntReg>;
     if (dst<>nil) then
     begin
      old:=dst.dtype;
      if (old<>dtUnknow) and (not CompareType(rtype,old)) then
      begin
       //OpLoad -> new -> dst
       dst:=pBitcastList^.FetchDstr(rtype,dst);
       pLine.pDst:=dst;
      end;
     end;
    end;

   Op.OpStore,
   Op.OpAtomicStore,
   Op.OpAtomicExchange,
   Op.OpAtomicCompareExchange,
   Op.OpAtomicCompareExchangeWeak,
   Op.OpAtomicIIncrement,
   Op.OpAtomicIDecrement,
   Op.OpAtomicIAdd,
   Op.OpAtomicISub,
   Op.OpAtomicSMin,
   Op.OpAtomicUMin,
   Op.OpAtomicSMax,
   Op.OpAtomicUMax,
   Op.OpAtomicAnd,
   Op.OpAtomicOr,
   Op.OpAtomicXor:
    begin
     pLine.ParamNode(1).Value.PrepType(ord(rtype));

     dst:=pLine.ParamNode(1).Value.specialize AsType<ntReg>;
     if (dst<>nil) then
     begin
      old:=dst.dtype;
      if (old<>dtUnknow) and (rtype<>old) then
      begin
       //OpStore <- new <- dst
       dst:=pBitcastList^.FetchRead(rtype,dst);
       pLine.ParamNode(1).Value:=dst;
      end;
     end;
    end;

   else;
  end;

  node:=node.pNext;
 end;
end;

Procedure TsrChain.PrepType(new:TsrDataType);
var
 old:TsrDataType;
begin
 if (new=dtUnknow) then Exit;
 old:=Fdtype;
 if is_unprep_type(old,new,dweak) then
 begin
  old:=StoreType(new);
  SetRegType(old);
 end;
end;

procedure TsrChain.AddLine(pLine:TSpirvOp);
var
 node:TDependenceNode;
begin
 node:=NewDependence;
 node.pNode:=pLine;
 FList.Push_tail(node);
end;

function TsrChain.FirstLine:TSpirvOp;
var
 node:TDependenceNode;
begin
 Result:=nil;
 node:=FList.pHead;
 if (node<>nil) then
 begin
  Result:=node.pNode;
 end;
end;

procedure TsrChain.FetchLoad(pLine:TSpirvOp;dst:TsrRegNode);
var
 pTypeList:PsrTypeList;
begin
 Assert(dst<>nil);

 PrepType(dst.dtype);

 pTypeList:=Emit.GetTypeList;
 pLine:=Emit.OpLoad(pLine,pTypeList^.Fetch(dtype),dst,Self);

 AddLine(pLine);
end;

Procedure TsrChain.FetchStore(pLine:TSpirvOp;src:TsrRegNode);
begin
 if (src=nil) then Exit;

 PrepType(src.dtype);

 pLine:=Emit.OpStore(pLine,Self,src);

 AddLine(pLine);
end;

function TsrChain.GetPrintName:RawByteString;
begin
 Assert(ID.Alloc);
 Result:='ac'+IntToStr(ID.ID);
end;

//

procedure TsrDescriptor.InitVar();
var
 pVariableList:PsrVariableList;
begin
 if (FVar<>nil) then Exit;
 //
 pVariableList:=Emit.GetVariableList;
 //
 FVar:=pVariableList^.Fetch;
 FVar.pSource:=Self;
end;

procedure TsrDescriptor.InitType(rtype:TsrDataType);
var
 pTypeList:PsrTypeList;
begin
 if (FType<>nil) then Exit;
 //
 pTypeList:=Emit.GetTypeList;
 //
 SetType(pTypeList^.Fetch(rtype));
end;

procedure TsrDescriptor.SetType(t:TsrType);
begin
 if (FType=t) then Exit;

 if isUsed then
 begin
      t.mark_read  (Self);
  FType.mark_unread(Self);
 end;
 FType:=t;
end;

function is_consistents(const chains:TsrChains;count:Byte):Boolean;
var
 parent:TsrDataLayout;
 offset,t:PtrUint;
 i:Byte;
begin
 offset:=0;
 t:=0;
 if (count<2) then Exit(True);
 Result:=False;
 if (chains[0]=nil) then Exit;
 parent:=chains[0].parent;
 offset:=chains[0].offset;
 For i:=1 to count-1 do
 begin
  t:=chains[i-1].size;
  offset:=offset+t;
  if (chains[i]=nil) then Exit;
  if (chains[i].parent<>parent) then Exit;
  t:=chains[i].offset;
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
  if (chains[i].key.lvl_1.pIndex<>nil) then Exit;
 end;
 Result:=True;
end;

function is_userdata_chains(const chains:TsrChains;count:Byte):Boolean;
var
 parent:TsrDataLayout;
 i:Byte;
begin
 Result:=False;
 if (count=0) then Exit;
 For i:=0 to count-1 do
 begin
  if (chains[i]=nil) then Exit;
  parent:=chains[i].parent;
  if (parent=nil) then Exit;
  if (parent.key.parent<>nil) then Exit;
 end;
 Result:=True;
end;

end.

