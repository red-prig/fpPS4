unit srLayout;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 si_ci_vi_merged_enum,
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
  rtVSharp2,
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
   property  stride:PtrUint       read key.lvl_1.stride write key.lvl_1.stride;
   property  size  :PtrUint       read key.lvl_0.size;
   property  offset:PtrUint       read key.lvl_0.offset write SetOffset;
   property  Flags :TsrChainFlags read key.Flags;
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
  offset:PtrUint;
  rtype :TsrResourceType;
 end;

 TsrDescriptor=class;

 TsrDataLayout=class
  type
   TDataTree =specialize TNodeTreeClass<TsrDataLayout>;
   TDescList =specialize TNodeListClass<TsrDescriptor>;
   TChainList=specialize TNodeListClass<TsrChain>;
   TChainTree=specialize TNodeTreeClass<TsrChain>;
   TInplaceData  =array[0..7] of DWORD;
  var
   pPrev,pNext :TsrDataLayout;
   pLeft,pRight:TsrDataLayout;
   //----
   key       :TsrDataLayoutKey;
   FData     :TInplaceData;
   FID       :Integer;
   FOrder    :Integer;
   FSetid    :Integer;
   FCache    :Integer;
   FEmit     :TCustomEmit;
   FParent   :TsrDataLayout;
   FDataTree :TDataTree;
   FDescList :TDescList;
   FChainList:TChainList;
   FChainTree:TChainTree;
   //
   RINF:Boolean; //Resource data precompiled (dst_sel,nfmt,dfmt)
   //
  class function c(n1,n2:PsrDataLayoutKey):Integer; static;
  function  Order:Integer;
  function  Fetch(lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1;cflags:Byte=0):TsrChain;
  Procedure UpdateCache;
  Function  First:TsrChain;
  Function  Last :TsrChain;
  function  EnumChain(cb:TChainCb):Integer;
  function  GetData:Pointer;
  function  GetSharp:Pointer;
  function  IsUserData:Boolean; inline;
  function  IsLocalDataShare:Boolean; inline;
  function  IsGlobalDataShare:Boolean; inline;
  function  UseBitcast:Boolean;
  function  GetStride:PtrUint;
  function  GetTypeChar:Char;
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
   TDataList   =specialize TNodeListClass<TsrDataLayout>;
   TDataImmTree=specialize TNodeTreeClass<TsrDataImm>;
  var
   FTop      :TsrDataLayout;
   FDataList :TDataList;
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
  procedure AllocSourceExtension2;
 end;

 TseWriter=object
  pList:TsrDebugInfoList;
  node :TsrDataLayout;
  deep :Integer;
  function  Next:Boolean;
  Procedure Header(const name:RawByteString);
  Procedure StrOpt(const name,Value:RawByteString);
  Procedure IntOpt(const name:RawByteString;Value:QWORD);
  Procedure HexOpt(const name:RawByteString;Value:QWORD);
  Procedure ImmOpt(const name:RawByteString;P:Pointer;len:qword);
 end;

//----

 TsrDescriptor=class(TsrNode)
  private
   pPrev,pNext:TsrDescriptor;
  protected
   FVar    :TsrVariable;
   FType   :TsrType;
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
   procedure AllocSourceExtension2(var Writer:TseWriter); virtual;
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
 PrepType(TsrDataType(node^.rtype));
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

procedure TsrDescriptor.AllocSourceExtension2(var Writer:TseWriter);
begin
 //
end;

//

class function TsrDataLayout.c(n1,n2:PsrDataLayoutKey):Integer;
begin
 //first offset
 Result:=ord(n1^.offset>n2^.offset)-ord(n1^.offset<n2^.offset);
 if (Result<>0) then Exit;
 //second rtype
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
 if (_key.lvl_1.pIndex<>nil) then
 begin
  Assert((_key.lvl_1.stride<>0),'stride=0');
 end;
 //
 _key.Flags:=TsrChainFlags(cflags);
 //
 Result:=FChainTree.Find(@_key);
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrChain>;
  Result.Init(Self);
  Result.key   :=_key;
  Result.Fdtype:=_key.Flags.dtype;
  FChainTree.Insert(Result);
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
   node:=FChainList.Pop_tail;
  until (node=nil);
  //Load
  node:=FChainTree.Min;
  while (node<>nil) do
  begin
   FChainList.Push_tail(node);
   //
   node:=FChainTree.Next(node);
  end;
 end;
end;

Function TsrDataLayout.First:TsrChain;
begin
 UpdateCache;
 Result:=FChainList.pHead;
end;

Function TsrDataLayout.Last:TsrChain;
begin
 UpdateCache;
 Result:=FChainList.pTail;
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
 Case key.rtype of
  rtRoot,
  rtBufPtr2,
  rtFunPtr2:Result:=PPointer(@FData)^;
  rtVSharp2,
  rtVSharp4:Result:=Pointer(PVSharpResource4(@FData)^.base and (not 3));
  rtTSharp4,
  rtTSharp8:Result:=Pointer(QWORD(PTSharpResource4(@FData)^.base) shl 8);
  rtImmData:Result:=TsrDataImm(PPointer(@FData)^).key.pData;
  else;
 end;
end;

function TsrDataLayout.GetSharp:Pointer;
begin
 Result:=nil;
 Case key.rtype of
  rtRoot,
  rtBufPtr2,
  rtFunPtr2:Result:=PPointer(@FData)^;
  rtVSharp2,
  rtVSharp4,
  rtSSharp4,
  rtTSharp4,
  rtTSharp8:Result:=@FData;
  rtImmData:Result:=TsrDataImm(PPointer(@FData)^);
  else;
 end;
end;

function TsrDataLayout.GetStride:PtrUint;
begin
 Result:=0;
 Case key.rtype of
  rtRoot,
  rtBufPtr2:Result:=4;
  rtVSharp2,
  rtVSharp4:Result:=PVSharpResource4(@FData)^.stride;
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
  rtVSharp2:Result:='v';
  rtVSharp4:Result:='V';
  rtSSharp4:Result:='S';
  rtTSharp4:Result:='t';
  rtTSharp8:Result:='T';
  rtLDS    :Result:='L';
  rtGDS    :Result:='G';
 end;
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
 FDataList.Push_tail(FTop);
end;

procedure TsrDataLayoutList.SetUserData(pData:Pointer);
begin
 PPointer(@FTop.FData)^:=pData;
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
 key.offset:=o;
 key.rtype :=t;
 //
 Result:=p.FDataTree.Find(@key);
 if (Result=nil) then
 begin
  Inc(FOrder);

  Result:=FTop.FEmit.specialize New<TsrDataLayout>;
  Result.FID    :=-1;
  Result.FOrder :=FOrder;
  Result.FEmit  :=FTop.FEmit;
  Result.key    :=key;
  Result.FParent:=p;

  p.FDataTree.Insert(Result);
  FDataList.Push_tail(Result);

  if (pData<>nil) then
  begin
   Result.FData:=Default(TsrDataLayout.TInplaceData);

   case t of
    rtRoot   :PPointer(@Result.FData)^:=pData;
    rtFunPtr2:PPointer(@Result.FData)^:=Pointer(PPtrUint(pData+o)^);
    rtBufPtr2:PPointer(@Result.FData)^:=Pointer(PPtrUint(pData+o)^ and (not 3));
    rtImmData:PPointer(@Result.FData)^:=pData;
    rtVSharp2,
    rtVSharp4,
    rtSSharp4,
    rtTSharp4,
    rtTSharp8:Move(Pointer(pData+o)^,Result.FData,GetResourceSizeDw(t)*SizeOf(DWORD));
   end;

  end;

 end;
end;

Function TsrDataLayoutList.First:TsrDataLayout;
begin
 Result:=FDataList.pHead;
end;

Function TsrDataLayoutList.Next(node:TsrDataLayout):TsrDataLayout;
begin
 Result:=node.pNext;
end;

function GetResourceSizeDw(r:TsrResourceType):Byte;
begin
 Result:=0;
 Case r of
  rtRoot   :Result:=2;
  rtBufPtr2:Result:=2;
  rtFunPtr2:Result:=2;
  rtVSharp2:Result:=2;
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

function TseWriter.Next:Boolean;
var
 newv:TsrDataLayout;
 oldv:TsrDataLayout;
begin
 oldv:=node;
 //
 newv:=oldv.FDataTree.Min; //down
 Inc(deep);
 if (newv=nil) then
 begin
  repeat //up
   if (oldv.FParent=nil) then
   begin
    newv:=nil;
    oldv:=nil;
   end else
   begin
    newv:=oldv.FParent.FDataTree.Next(oldv);
    oldv:=oldv.FParent;
   end;
   Dec(deep);
  until (oldv=nil) or (newv<>nil);
 end;
 //
 node:=newv;
 //
 Result:=(node<>nil);
end;

Function HexStr2(Val:qword):shortstring;
var
 count:Byte;
begin
 if (Val<=9) then
 begin
  Result:=AnsiChar(Byte(Val)+ord('0'));
 end else
 begin
  count:=BsrQWord(Val);
  if (count=$FF) then count:=0;
  count:=(count+4) div 4;
  Result:='0x'+HexStr(Val,count);
 end;
end;

const
 HexTbl:array[0..15] of char='0123456789ABCDEF';

Function HexLen(P:PByte;len:qword):RawByteString;
var
 i:qword;
begin
 Result:='';
 SetLength(Result,len*2);
 For i:=0 to len-1 do
 begin
  Result[i*2+1]:=hextbl[P[i] and $f];
  Result[i*2+2]:=hextbl[P[i] shr  4];
 end;
end;

Procedure TseWriter.Header(const name:RawByteString);
begin
 pList.OpSource(Space(deep)+name);
end;

Procedure TseWriter.StrOpt(const name,Value:RawByteString);
begin
 pList.OpSource(Space(deep+1)+name+':'+Value);
end;

Procedure TseWriter.IntOpt(const name:RawByteString;Value:QWORD);
begin
 pList.OpSource(Space(deep+1)+name+':'+IntToStr(Value));
end;

Procedure TseWriter.HexOpt(const name:RawByteString;Value:QWORD);
begin
 pList.OpSource(Space(deep+1)+name+':'+HexStr2(Value));
end;

Procedure TseWriter.ImmOpt(const name:RawByteString;P:Pointer;len:qword);
var
 i,d,m:qword;
begin
 d:=len div SizeOf(DWORD);
 m:=len mod SizeOf(DWORD);

 if (d<>0) then
 For i:=0 to d-1 do
 begin
  pList.OpSource(Space(deep+1)+name+':0x'+HexStr(PDWORD(P)[i],8));
 end;

 if (m<>0) then
 begin
  i:=0;
  Move(PDWORD(P)[d],i,m);
  pList.OpSource(Space(deep+1)+name+':0x'+HexStr(i,m*2));
 end;

 //pList.OpSource(Space(deep+1)+name+':'+HexLen(P,len));
end;

function IsInvalidVSharp(dfmt,num_records:DWORD):Boolean; inline;
begin
 Result:=(dfmt=0) or (num_records=0);
end;

procedure TsrDataLayoutList.AllocSourceExtension2;
var
 Writer:TseWriter;
 pHeap :PsrCodeHeap;
 desc  :TsrDescriptor;
 pCode :TsrCodeRegion;
 imm   :TsrDataImm;
 PV    :PVSharpResource4;
 PS    :PSSharpResource4 absolute PV;
 PT    :PTSharpResource4 absolute PV;
begin
 pHeap:=FTop.FEmit.GetCodeHeap;

 Writer:=Default(TseWriter);
 Writer.pList:=FTop.FEmit.GetDebugInfoList;
 Writer.node:=pRoot;

 repeat

  //start block
  Writer.Header('#'+Writer.node.GetTypeChar);

  case Writer.node.key.rtype of
   rtFunPtr2,
   rtBufPtr2,
   rtVSharp2,
   rtVSharp4,
   rtSSharp4,
   rtTSharp4,
   rtTSharp8:
    begin
     //offset
     if (Writer.node.key.offset<>0) then
     begin
      Writer.HexOpt('OFS',Writer.node.key.offset);
     end;
    end;
   else;
  end;

  case Writer.node.key.rtype of
   rtVSharp2,
   rtVSharp4:
    begin
     //Resource data precompiled

     PV:=Writer.node.GetSharp;

     with PV^ do
     begin
      if (Writer.node.RINF) then
      begin
       Writer.IntOpt('RINF',1);
       if IsInvalidVSharp(dfmt,num_records) then
       begin
        Writer.IntOpt('INVL',1);
       end;
       Writer.IntOpt('DFMT',dfmt);
       Writer.IntOpt('NFMT',nfmt);
       Writer.IntOpt('STRD',stride);
       Writer.StrOpt('DSEL',_get_dst_sel_str(dst_sel_x,dst_sel_y,dst_sel_z,dst_sel_w));
      end else
      begin
       if IsInvalidVSharp(dfmt,num_records) then
       begin
        Writer.IntOpt('INVL',1);
       end;
      end;
     end;

    end;

   rtSSharp4:
    begin
     //Resource data precompiled

     PS:=Writer.node.GetSharp;

     with PS^ do
     begin
      if (force_degamma<>0) then
      begin
       Writer.IntOpt('FDGM',force_degamma);
      end;
     end;

    end;

   rtTSharp4,
   rtTSharp8:
    begin
     //Resource data precompiled

     PT:=Writer.node.GetSharp;

     with PT^ do
     begin
      Writer.IntOpt('TYPE',_type);
      if (Writer.node.RINF) then
      begin
       Writer.StrOpt('RINF','1');
       Writer.IntOpt('DFMT',dfmt);
       Writer.IntOpt('NFMT',nfmt);
       Writer.StrOpt('DSEL',_get_dst_sel_str(dst_sel_x,dst_sel_y,dst_sel_z,dst_sel_w));
      end else
      begin
       Writer.IntOpt('DFMT',dfmt); //0->invalid
       Writer.IntOpt('NFMT',nfmt);
      end;
     end;

    end;
   else;
  end;

  case Writer.node.key.rtype of
   rtImmData:
    begin
     //imm data
     imm:=TsrDataImm(Writer.node.GetSharp);
     Assert(imm<>nil);
     //
     Writer.HexOpt('LEN',imm.key.FImmSize);
     Writer.ImmOpt('IMM',imm.key.pData,imm.key.FImmSize);
    end;
   rtFunPtr2:
    begin
     //func
     pCode:=pHeap^.FindByPtr(Writer.node.GetData);
     Assert(pCode<>nil);
     //
     Writer.HexOpt('LEN',pCode.Size);
     Writer.ImmOpt('IMM',pCode.DMem,pCode.Size);
    end;
   else;
  end;

  Inc(Writer.deep);
  //
  desc:=Writer.node.FDescList.pHead;
  while (desc<>nil) do
  begin
   desc.AllocSourceExtension2(Writer);
   //
   desc:=desc.pNext;
  end;
  //
  Dec(Writer.deep);

  Writer.Next;

 until (Writer.node=nil);

 //
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
begin
 if (Fdtype=rtype) then Exit;

 Assert(rtype.BitSize div 8=size);

 Fdtype:=rtype;

 UpdateRegType;
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
 FParent.FChainTree.Delete(Self);
 //set
 key.lvl_1.pIndex:=t;
 //link
 FParent.FChainTree.Insert(Self);
end;

Procedure TsrChain.SetOffset(t:PtrUint);
begin
 if (Self=nil) then Exit;
 if (key.lvl_0.offset=t) then Exit;

 //unlink
 FParent.FChainTree.Delete(Self);
 //set
 key.lvl_0.offset:=t;
 //link
 FParent.FChainTree.Insert(Self);
end;

Procedure TsrChain.UpdateRegType;
var
 pTypeList:PsrTypeList;
 pBitcastList:PsrBitcastList;
 FType:TsrType;
 node:TDependenceNode;
 pLine:TSpirvOp;
 Value:TsrNode;
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
     Value:=pLine.pDst;
     Value.PrepType(ord(rtype));

     pLine.pType:=Ftype;

     dst:=Value.specialize AsType<ntReg>;
     if (dst<>nil) then
     begin
      old:=dst.dtype;
      if (old<>dtUnknow) and (rtype<>old) then
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
     Value:=pLine.ParamNode(1).Value;
     Value.PrepType(ord(rtype));

     dst:=Value.specialize AsType<ntReg>;
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
 //dont update with allocated field
 if (pField<>nil) then Exit;
 //
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
 pIndex:TsrRegNode;
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
 pIndex:=chains[0].pIndex;
 For i:=1 to count-1 do
 begin
  if (chains[i]=nil) then Exit;
  if (chains[i].parent<>parent) then Exit;
  if (chains[i].pIndex<>pIndex) then Exit;
  //
  t:=chains[i-1].size;
  offset:=offset+t;
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
  if (parent.FParent<>nil) then Exit;
 end;
 Result:=True;
end;

end.

