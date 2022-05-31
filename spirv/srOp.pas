unit srOp;

{$mode objfpc}{$H+}

interface

uses
  srNodes,
  srLabel,
  srCFG,
  srParser,
  srTypes,
  srRefId,
  srReg;

type
 POpParamNode=^TOpParamNode;
 TOpParamNode=packed record
  pNext:POpParamNode;
  //----
  ntype:TsrNodeType;
  Case Byte of
   0:(pData:Pointer);
   1:(Value:DWORD;name:record end);
 end;

 TOpParamQueue=specialize TNodeQueue<POpParamNode>;

 PsrOpBlock=^TsrOpBlock;

 PspirvOp=^TspirvOp;
 TspirvOp=object
  pPrev,pNext:PspirvOp;
  //----
  pParent:PsrOpBlock;
  pParam:TOpParamQueue;
  Adr:TSrcAdr;
  dst_type:PsrType;
  dst:TOpParamSingle;
  OpId:DWORD;
  procedure InsertAfter(new:PspirvOp);
  procedure InsertBefore(new:PspirvOp);
  procedure Remove;
  function  ParamNode(i:Byte):POpParamNode;
  procedure AddParam(const s:TOpParamSingle);
  procedure AddParam(ntype:TsrNodeType;Data:Pointer);
  procedure AddParamAfter(prev:POpParamNode;ntype:TsrNodeType;Data:Pointer);
  procedure AddLiteral(Value:PtrUint;const name:RawByteString='');
  procedure AddString(const name:RawByteString);
  Procedure SetDstReg(r:Pointer);
 end;

 TsrVolMark=(vmNone,vmEnd,vmBreak,vmCont);

 TsrBlockInfo=packed record
  b_adr,e_adr:TSrcAdr;
  bType:TsrBlockType;
 end;

 TsrOpList=specialize TNodeList<PspirvOp>;

 TsrOpBlockSimple=object(TsrOpList)
  Alloc:TfnAlloc;
  procedure Init(cb:TfnAlloc);
  function  NewSpirvOp(OpId:DWORD):PSpirvOp;
  function  AddSpirvOp(node:PSpirvOp):PSpirvOp;
  function  AddSpirvOp(OpId:DWORD):PSpirvOp;
 end;

 TsrOpBlock=object(TsrOpBlockSimple)
  pParent:PsrOpBlock;
  pUpLine:PSpirvOp; //from post

  Block:TsrBlockInfo;

  Labels:record
   pBegOp:PspirvOp;
   pEndOp:PspirvOp;
   pMrgOp:PspirvOp;
  end;

  FCursor:TsrCursor;

  Regs:record
   pSnap:PsrRegsSnapshot;
   FVolMark:TsrVolMark;
  end;

  Cond:record
   pReg:PsrRegNode;
   FVal:Boolean;
   FUseCont:Boolean;
  end;

  FLevel:DWORD;

  dummy:TspirvOp;

  function  line:PSpirvOp;
  procedure SetCFGBlock(pLBlock:PsrCFGBlock);
  procedure SetInfo(const b:TsrBlockInfo);
  procedure SetInfo(bType:TsrBlockType;b_adr,e_adr:TSrcAdr);
  procedure SetLabels(pBegOp,pEndOp,pMrgOp:PspirvOp);
  procedure SetCond(pReg:PsrRegNode;FVal:Boolean);
  function  IsEndOf(Adr:TSrcAdr):Boolean;
  function  FindUpLoop:PsrOpBlock;
  function  FindUpCond(pReg:PsrRegNode):PsrOpBlock;
 end;

 PSpirvFunc=^TSpirvFunc;
 TSpirvFunc=object
  pPrev,pNext,pLeft,pRight:PSpirvFunc;
  //----
  read_count:DWORD;
  FTop:TsrOpBlock;
  pBlock:PsrOpBlock;
  ID:TsrRefId; //post id
  name:RawByteString;
  function  c(n1,n2:PSpirvFunc):Integer; static;
  Procedure PushBlock(New:PsrOpBlock);
  function  PopBlock:Boolean;
  function  line:PSpirvOp;
  Procedure Init(const fname:RawByteString;Alloc:TfnAlloc);
  Procedure mark_read;
  Procedure mark_unread;
  function  NewSpirvOp(OpId:DWORD):PSpirvOp;
  function  AddSpirvOp(node:PSpirvOp):PSpirvOp;
  function  AddSpirvOp(OpId:DWORD):PSpirvOp;
 end;

 TsrFuncList=object
  type
   TNodeList=specialize TNodeList<PSpirvFunc>;
   TNodeFetch=specialize TNodeFetch<PSpirvFunc,TSpirvFunc>;
  var
   FList:TNodeList;
   FNTree:TNodeFetch;
  function  Search(const name:RawByteString):PSpirvFunc;
  procedure Insert(new:PSpirvFunc);
 end;

 PsrCacheOp=^TsrCacheOp;
 TsrCacheOp=object
  pLeft,pRight:PsrCacheOp;
  //----
  pReg:PsrRegNode;
  key:packed record
   place:PsrOpBlock;
   OpId:DWORD;
   dtype:TsrDataType;
   count:DWORD;
  end;
  Data:record end;
  function  c(n1,n2:PsrCacheOp):Integer; static;
  function  GetCompItem(i:Byte):PsrRegNode; inline;
  Procedure SetCompItem(i:Byte;p:PsrRegNode); inline;
 end;

 TsrCacheOpList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrCacheOp,TsrCacheOp>;
  var
   Alloc:TfnAlloc;
   FNTree:TNodeFetch;
  function Fetch(place:PsrOpBlock;OpId:DWORD;rtype:TsrDataType;count:Byte;src:PPsrRegNode):PsrCacheOp;
 end;

implementation

uses
 SprvEmit;

procedure TspirvOp.InsertAfter(new:PspirvOp);
begin
 Assert(new<>nil);
 Assert(pParent<>nil);
 pParent^.InsertAfter(@Self,new);
 new^.pParent:=pParent;
end;

procedure TspirvOp.InsertBefore(new:PspirvOp);
begin
 Assert(new<>nil);
 Assert(pParent<>nil);
 pParent^.InsertAfter(@Self,new);
 new^.pParent:=pParent;
end;

procedure TspirvOp.Remove;
begin
 Assert(pParent<>nil);
 pParent^.Remove(@Self);
 pParent:=nil;
end;

function TspirvOp.ParamNode(i:Byte):POpParamNode;
var
 node:POpParamNode;
begin
 Result:=nil;
 node:=pParam.pHead;
 While (node<>nil) do
 begin
  if (i=0) then Exit(node);
  Dec(i);
  node:=node^.pNext;
 end;
end;

procedure TspirvOp.AddParam(const s:TOpParamSingle);
var
 node:POpParamNode;
begin
 node:=pParent^.Alloc(SizeOf(TOpParamNode));
 node^.ntype:=s.ntype;
 node^.pData:=s.pData;
 pParam.Push_tail(node);
end;

procedure TspirvOp.AddParam(ntype:TsrNodeType;Data:Pointer);
var
 node:POpParamNode;
begin
 node:=pParent^.Alloc(SizeOf(TOpParamNode));
 node^.ntype:=ntype;
 node^.pData:=Data;
 pParam.Push_tail(node);
end;

procedure TspirvOp.AddParamAfter(prev:POpParamNode;ntype:TsrNodeType;Data:Pointer);
var
 node:POpParamNode;
begin
 node:=pParent^.Alloc(SizeOf(TOpParamNode));
 node^.ntype:=ntype;
 node^.pData:=Data;
 pParam.InsertAfter(prev,node);
end;

procedure TspirvOp.AddLiteral(Value:PtrUint;const name:RawByteString='');
var
 node:POpParamNode;
 l:ptruint;
begin
 l:={%H-}ptruint(@TOpParamNode(nil^).name)+Length(name)+1;
 node:=pParent^.Alloc(l);
 node^.ntype:=ntLiteral;
 node^.Value:=Value;
 Move(PChar(name)^,node^.name,Length(name)+1);
 pParam.Push_tail(node);
end;

procedure TspirvOp.AddString(const name:RawByteString);
var
 node:POpParamNode;
 l:ptruint;
begin
 l:={%H-}ptruint(@TOpParamNode(nil^).name)+Length(name)+1;
 l:=Align(l,SizeOf(DWORD));
 node:=pParent^.Alloc(l);
 node^.ntype:=ntString;
 node^.Value:=0;
 Move(PChar(name)^,node^.name,Length(name)+1);
 pParam.Push_tail(node);
end;

Procedure TspirvOp.SetDstReg(r:Pointer);
begin
 Assert(r<>nil);
 dst.SetParam(ntReg,r);
 PsrRegNode(r)^.pWriter.SetParam(ntOp,@Self);
 PsrRegNode(r)^.pLine:=@Self; //update line [bitcast order]
end;

function TsrOpBlock.line:PSpirvOp;
begin
 Result:=nil;
 if (@Self<>nil) then
 begin
  Result:=pTail;
 end;
end;

procedure TsrOpBlock.SetCFGBlock(pLBlock:PsrCFGBlock);
begin
 dummy.Adr  :=pLBlock^.pBLabel^.Adr;
 Block.b_adr:=pLBlock^.pBLabel^.Adr;
 Block.e_adr:=pLBlock^.pELabel^.Adr;
 Block.bType:=pLBlock^.bType;
end;

procedure TsrOpBlock.SetInfo(const b:TsrBlockInfo);
begin
 dummy.Adr  :=b.b_adr;
 Block.b_adr:=b.b_adr;
 Block.e_adr:=b.e_adr;
 Block.bType:=b.bType;
end;

procedure TsrOpBlock.SetInfo(bType:TsrBlockType;b_adr,e_adr:TSrcAdr);
begin
 dummy.Adr  :=b_adr;
 Block.b_adr:=b_adr;
 Block.e_adr:=e_adr;
 Block.bType:=bType;
end;

procedure TsrOpBlock.SetLabels(pBegOp,pEndOp,pMrgOp:PspirvOp);
begin
 Labels.pBegOp:=pBegOp;
 Labels.pEndOp:=pEndOp;
 Labels.pMrgOp:=pMrgOp;
end;

procedure TsrOpBlock.SetCond(pReg:PsrRegNode;FVal:Boolean);
begin
 Cond.pReg:=pReg;
 Cond.FVal:=FVal;
end;

function TsrOpBlock.IsEndOf(Adr:TSrcAdr):Boolean;
begin
 Result:=(Block.e_adr.get_pc<=Adr.get_pc);
end;

function TsrOpBlock.FindUpLoop:PsrOpBlock;
var
 node:PsrOpBlock;
begin
 Result:=nil;
 node:=@Self;
 While (node<>nil) do
 begin
  if (node^.Block.bType=btLoop) then Exit(node);
  node:=node^.pParent;
 end;
end;

function TsrOpBlock.FindUpCond(pReg:PsrRegNode):PsrOpBlock;
var
 node:PsrOpBlock;
begin
 Result:=nil;
 if (pReg=nil) then Exit;
 pReg:=RegDown(pReg);
 if pReg^.is_const then Exit;
 node:=@Self;
 While (node<>nil) do
 begin
  if (node^.Block.bType=btCond) and (pReg=RegDown(node^.Cond.pReg)) then Exit(node);
  node:=node^.pParent;
 end;
end;

function TSpirvFunc.c(n1,n2:PSpirvFunc):Integer;
var
 count1,count2:sizeint;
begin
 Count1:=Length(n1^.name);
 Count2:=Length(n2^.name);
 Result:=Integer(Count1>Count2)-Integer(Count1<Count2);
 if (Result=0) then
 begin
  Result:=CompareByte(PChar(n1^.name)^,PChar(n2^.name)^,Count1);
 end;
end;

Procedure TSpirvFunc.PushBlock(New:PsrOpBlock);
begin
 if (New=nil) then Exit;
 New^.pParent:=pBlock;
 New^.FLevel :=pBlock^.FLevel+1;
 pBlock:=New;
end;

function TSpirvFunc.PopBlock:Boolean;
begin
 Result:=False;
 if (pBlock=nil) then Exit;
 if (pBlock^.pParent=nil) then Exit;
 pBlock:=pBlock^.pParent;
 Result:=True;
end;

function TSpirvFunc.line:PSpirvOp;
begin
 Result:=nil;
 if (pBlock<>nil) then
 begin
  Result:=pBlock^.pTail;
 end;
end;

Procedure TSpirvFunc.Init(const fname:RawByteString;Alloc:TfnAlloc);
begin
 name:=fname;
 pBlock:=@FTop;
 pBlock^.Alloc:=Alloc;
 pBlock^.dummy.pParent:=pBlock;
 pBlock^.Push_head(@pBlock^.dummy);
end;

Procedure TSpirvFunc.mark_read;
begin
 Inc(read_count);
end;

Procedure TSpirvFunc.mark_unread;
begin
 if (read_count<>0) then Dec(read_count);
end;

function TSpirvFunc.NewSpirvOp(OpId:DWORD):PSpirvOp;
begin
 Result:=FTop.Alloc(SizeOf(TSpirvOp));
 Result^.OpId:=OpId;
end;

function TSpirvFunc.AddSpirvOp(node:PSpirvOp):PSpirvOp;
begin
 Result:=node;
 if (node=nil) then Exit;
 pBlock^.Push_tail(node);
 node^.pParent:=pBlock;
end;

function TSpirvFunc.AddSpirvOp(OpId:DWORD):PSpirvOp;
begin
 Result:=AddSpirvOp(NewSpirvOp(OpId));
end;

//

procedure TsrOpBlockSimple.Init(cb:TfnAlloc);
begin
 Alloc:=cb;
end;

function TsrOpBlockSimple.NewSpirvOp(OpId:DWORD):PSpirvOp;
begin
 Result:=Alloc(SizeOf(TSpirvOp));
 Result^.OpId:=OpId;
end;

function TsrOpBlockSimple.AddSpirvOp(node:PSpirvOp):PSpirvOp;
begin
 Result:=node;
 if (node=nil) then Exit;
 Push_tail(node);
 node^.pParent:=@Self;
end;

function TsrOpBlockSimple.AddSpirvOp(OpId:DWORD):PSpirvOp;
begin
 Result:=AddSpirvOp(NewSpirvOp(OpId));
end;

//

function TsrFuncList.Search(const name:RawByteString):PSpirvFunc;
var
 node:TSpirvFunc;
begin
 node:=Default(TSpirvFunc);
 node.name:=name;
 Result:=FNTree.Find(@node);
end;

procedure TsrFuncList.Insert(new:PSpirvFunc);
begin
 FNTree.Insert(new);
 FList.Push_head(new);
end;

//--

function TsrCacheOp.c(n1,n2:PsrCacheOp):Integer;
var
 count:DWORD;
begin
 Result:=CompareByte(n1^.key,n2^.key,SizeOf(TsrCacheOp.key));
 if (Result=0) then
 begin
  count:=n1^.key.count;
  if (count<>0) then
  begin
   Result:=CompareByte(n1^.Data,n2^.Data,count*SizeOf(Pointer));
  end;
 end;
end;

function TsrCacheOp.GetCompItem(i:Byte):PsrRegNode; inline;
begin
 Result:=PPsrRegNode(@Data)[i];
end;

Procedure TsrCacheOp.SetCompItem(i:Byte;p:PsrRegNode); inline;
begin
 PPsrRegNode(@Data)[i]:=p;
end;

function TsrCacheOpList.Fetch(place:PsrOpBlock;OpId:DWORD;rtype:TsrDataType;count:Byte;src:PPsrRegNode):PsrCacheOp;
var
 i:Byte;
 size:ptruint;
 rec:record
  node:TsrCacheOp;
  align:array[0..3] of Pointer;
 end;
begin
 Assert(place<>nil);
 Assert((count>0) and (count<=4));
 Result:=nil;
 rec.node:=Default(TsrCacheOp);
 rec.node.key.place:=place;
 rec.node.key.OpId :=OpId;
 rec.node.key.dtype:=rtype;
 rec.node.key.count:=count;
 For i:=0 to count-1 do
 begin
  rec.node.SetCompItem(i,src[i]);
 end;
 Result:=FNTree.Find(@rec.node);
 if (Result=nil) then
 begin
  size:=SizeOf(TsrCacheOp)+SizeOf(Pointer)*count;
  Result:=Alloc(size);
  Move(rec.node,Result^,Size);
  FNTree.Insert(Result);
 end;
end;

end.

