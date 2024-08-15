unit srOp;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 spirv,
 ginodes,
 srNode,
 srCFGLabel,
 srCFGParser,
 srCFGCursor,
 srLiteral,
 srType,
 srTypes,
 srRefId,
 srReg;

type
 TspirvOp=class;

 PPspirvOp=^TspirvOp;

 TOpParamNode=class
  public
   pNext:TOpParamNode;
  private
   pParent:TspirvOp;
   pValue:TsrNode;
   procedure SetValue(v:TsrNode);
  public
   property Next:TOpParamNode read pNext;
   property Parent:TspirvOp   read pParent;
   property Value:TsrNode     read pValue write SetValue;
   function AsReg:TsrRegNode;
   function TryGetValue(var V:PtrUint):Boolean;
 end;
 POpParamNode=TOpParamNode;

 TOpParamQueue=specialize TNodeQueueClass<POpParamNode>;

 TsrOpCustom=class(TsrNode)
  public
   pPrev,pNext:TsrOpCustom;
  private
   pParent:TsrOpCustom; //TsrOpBlock;
  public
   function  _Next  :TsrNode; override;
   function  _Prev  :TsrNode; override;
   function  _Parent:TsrNode; override;
 end;

 ntOpCustom=TsrOpCustom;

 TsrOpList=specialize TNodeListClass<TsrOpCustom>;

 TsoFlags=(soClear,soNotUsed);
 TsoSetFlags=Set of TsoFlags;

 TspirvOp=class(TsrOpCustom)
  private
   pParam:TOpParamQueue;
   FType :TsrType;
   Fdst  :TsrNode;
   flags :TsoSetFlags;
   procedure SetType(t:TsrType);
   Procedure SetDst(r:TsrNode);
   Procedure UnClear;
  public
   Adr :TSrcAdr;
   OpId:DWORD;
   //
   Procedure _zero_read;                    override;
   Procedure _PrepType(node:PPrepTypeNode); override;
   Function  _GetPtype:TsrNode;             override;
   function  _GetIndexCount:DWORD;          override;
   //
   property  pType:TsrType read FType write SetType;
   property  pDst:TsrNode  read Fdst  write SetDst;
   procedure Init(_OpId:DWORD); inline;
   procedure InsertAfter(new:TspirvOp);
   procedure InsertBefore(new:TspirvOp);
   procedure Remove;
   function  ParamFirst:POpParamNode;
   function  ParamLast:POpParamNode;
   function  ParamNode(i:Byte):POpParamNode;
   procedure AddParam(p:TsrNode);
   procedure AddParamAfter(__prev:POpParamNode;p:TsrNode);
   procedure AddLiteral(Value:PtrUint;const name:RawByteString='');
   procedure AddString(const name:RawByteString);
   function  is_cleared:Boolean;
   function  Clear:Boolean;
   procedure mark_not_used;
   function  can_clear:Boolean;
 end;

 ntOp=TspirvOp;

 TsrVolMark=(vmNone,vmEnd,vmBreak,vmCont);

 TsrBlockInfo=packed record
  b_adr,e_adr:TSrcAdr;
  bType:TsrBlockType;
 end;

 TsrOpBlockCustom=class(TsrOpCustom)
  private
   FList :TsrOpList;
   FIndex:DWORD;
   FLevel:DWORD;
   procedure AddIndex(d:DWORD);
   procedure SubIndex(d:DWORD);
  public
   //
   function  _First        :TsrNode; override;
   function  _Last         :TsrNode; override;
   function  _GetIndexCount:DWORD;   override;
   //
   property  Level:DWORD read FLevel;
   procedure InsertAfter(node,new:TsrOpCustom);
   procedure InsertBefore(node,new:TsrOpCustom);
   procedure Remove(node:TsrOpCustom);
   procedure UpdateLevel;
   function  NewSpirvOp(OpId:DWORD):TspirvOp;
   function  AddSpirvOp(node:TspirvOp):TspirvOp;
   function  AddSpirvOp(OpId:DWORD):TspirvOp;
   function  line:TsrNode;
 end;

 ntOpBlock=TsrOpBlockCustom;

 TsrOpBlock=packed class(TsrOpBlockCustom)
  public

   Block:TsrBlockInfo;

   Labels:record
    pBegOp:TspirvOp;
    pEndOp:TspirvOp;
    pMrgOp:TspirvOp;
   end;

   FCursor:TsrCursor;

   Regs:record
    pSnap_org:PsrRegsSnapshot;
    pSnap_cur:PsrRegsSnapshot;
    FVolMark:TsrVolMark;
   end;

   Cond:record
    pReg:TsrRegNode;
    FVal:Boolean;
    FUseCont:Boolean;
   end;

   dummy:TspirvOp;

   procedure Init;
   procedure SetCFGBlock(pLBlock:TsrCFGBlock);
   procedure SetInfo(const b:TsrBlockInfo);
   procedure SetInfo(bType:TsrBlockType;b_adr,e_adr:TSrcAdr);
   procedure SetLabels(pBegOp,pEndOp,pMrgOp:TspirvOp);
   procedure SetCond(pReg:TsrRegNode;FVal:Boolean);
   function  IsEndOf(Adr:TSrcAdr):Boolean;
   function  FindUpLoop:TsrOpBlock;
   function  FindUpCond(pReg:TsrRegNode):TsrOpBlock;
 end;

 TSpirvFunc=class(TsrNode)
  public
   pPrev,pNext,pLeft,pRight:TSpirvFunc;
   class function  c(n1,n2:PRawByteString):Integer; static;
  private
   key:RawByteString;
   FTop:TsrOpBlock;
   FBlock:TsrOpBlock;
   ID:TsrRefId; //post id
  public
   //
   function  _First       :TsrNode;       override;
   function  _Last        :TsrNode;       override;
   function  _GetRef      :Pointer;       override;
   function  _GetPrintName:RawByteString; override;
   //
   property  Name:RawByteString read key;
   property  pBlock:TsrOpBlock  read FBlock;
   Function  pTop:TsrOpBlock;
   Procedure PushBlock(New:TsrOpBlock);
   function  PopBlock:Boolean;
   function  line:TspirvOp;
   Procedure Init(const _name:RawByteString);
   function  NewSpirvOp(OpId:DWORD):TspirvOp;
   function  AddSpirvOp(node:TspirvOp):TspirvOp;
   function  AddSpirvOp(OpId:DWORD):TspirvOp;
   function  GetPrintName:RawByteString;
 end;

 ntFunc=TSpirvFunc;

 PsrFuncList=^TsrFuncList;
 TsrFuncList=object
  type
   TNodeList=specialize TNodeListClass<TSpirvFunc>;
   TNodeTree=specialize TNodeTreeClass<TSpirvFunc>;
  var
   FList:TNodeList;
   FTree:TNodeTree;
  function  Search(const name:RawByteString):TSpirvFunc;
  procedure Insert(new:TSpirvFunc);
  function  First:TSpirvFunc; inline;
 end;

Function classif_rw_op(OpId:DWORD):Byte;

operator := (i:TsrNode):TspirvOp;   inline;
operator := (i:TsrNode):TsrOpBlock; inline;
operator := (i:TsrNode):TSpirvFunc; inline;

implementation

operator := (i:TsrNode):TspirvOp; inline;
begin
 Result:=TspirvOp(Pointer(i)); //typecast hack
end;

operator := (i:TsrNode):TsrOpBlock; inline;
begin
 Result:=TsrOpBlock(Pointer(i)); //typecast hack
end;

operator := (i:TsrNode):TSpirvFunc; inline;
begin
 Result:=TSpirvFunc(Pointer(i)); //typecast hack
end;

//

function TsrOpCustom._Next:TsrNode;
begin
 Result:=pNext;
end;

function TsrOpCustom._Prev:TsrNode;
begin
 Result:=pPrev;
end;

function TsrOpCustom._Parent:TsrNode;
begin
 Result:=pParent;
end;

//

Procedure TspirvOp._zero_read;
begin
 UnClear;
end;

Function TspirvOp._GetPtype:TsrNode;
begin
 Result:=FType;
end;

function TspirvOp._GetIndexCount:DWORD;
begin
 Result:=1;
end;

Procedure TspirvOp._PrepType(node:PPrepTypeNode);
var
 new:TsrDataType;
 pNode:POpParamNode;
 pTypeList:PsrTypeList;
begin
 With TspirvOp(node^.dnode) do
 begin
  new:=TsrDataType(node^.rtype);
  if (new=dtUnknow) then Exit;

  Case OpId of
    Op.OpLoad:
     begin
      pNode:=ParamNode(0);
      if (pNode<>nil) then
      begin
       //change?
       pTypeList:=pParent.Emit.GetTypeList;
       pType:=pTypeList^.Fetch(new);
       //next
       node^.dnode:=pNode.pValue;
       Exit;
      end;
     end;
   else;
  end;

 end;

 node^.dnode:=nil;
end;

//

function TsrOpBlockCustom._First:TsrNode;
begin
 Result:=FList.pHead;
end;

function TsrOpBlockCustom._Last:TsrNode;
begin
 Result:=FList.pTail;
end;

function TsrOpBlockCustom._GetIndexCount:DWORD;
begin
 Result:=FIndex;
end;

//

function TSpirvFunc._First:TsrNode;
begin
 Result:=FTop.FList.pHead;
end;

function TSpirvFunc._Last:TsrNode;
begin
 Result:=FTop.FList.pTail;
end;

function TSpirvFunc._GetPrintName:RawByteString;
begin
 Result:=GetPrintName;
end;

function TSpirvFunc._GetRef:Pointer;
begin
 Result:=@ID;
end;

//

procedure TOpParamNode.SetValue(v:TsrNode);
var
 b:Byte;
begin
 if (pValue=v) then Exit;
 //
 Assert(pParent<>nil);
 //
 if not pParent.is_cleared then
 begin
  if (pParent.ParamFirst=Self) then
  begin
   b:=classif_rw_op(pParent.OpId);
  end else
  begin
   b:=1;
  end;
  //
  if (b and 2)<>0 then
  begin
        v.mark_write  (pParent);
   pValue.mark_unwrite(pParent);
  end;
  //
  if (b and 1)<>0 then
  begin
        v.mark_read  (pParent);
   pValue.mark_unread(pParent);
  end;
 end;
 //
 pValue:=v;
end;

function TOpParamNode.AsReg:TsrRegNode;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 Result:=pValue.specialize AsType<ntReg>;
end;

function TOpParamNode.TryGetValue(var V:PtrUint):Boolean;
begin
 Result:=False;
 if (Self=nil) then Exit;
 if pValue.IsType(ntLiteral) then
 begin
  V:=TsrLiteral(pValue).Value;
  Result:=True;
 end;
end;


//

procedure TspirvOp.Init(_OpId:DWORD); inline;
begin
 OpId:=_OpId;
end;

procedure TspirvOp.InsertAfter(new:TspirvOp);
begin
 Assert(new<>nil);
 Assert(pParent<>nil);
 TsrOpBlock(pParent).InsertAfter(Self,new);
end;

procedure TspirvOp.InsertBefore(new:TspirvOp);
begin
 Assert(new<>nil);
 Assert(pParent<>nil);
 TsrOpBlock(pParent).InsertBefore(Self,new);
end;

procedure TspirvOp.Remove;
begin
 Assert(pParent<>nil);
 TsrOpBlock(pParent).Remove(Self);
end;

function TspirvOp.ParamFirst:POpParamNode;
begin
 Result:=pParam.pHead;
end;

function TspirvOp.ParamLast:POpParamNode;
begin
 Result:=pParam.pTail;
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
  node:=node.pNext;
 end;
end;

procedure TspirvOp.AddParam(p:TsrNode);
var
 node:POpParamNode;
begin
 node:=Emit.specialize New<TOpParamNode>;
 node.pParent:=Self;
 //
 pParam.Push_tail(node);
 //
 node.Value:=p; //mark_read/mark_write
end;

procedure TspirvOp.AddParamAfter(__prev:POpParamNode;p:TsrNode);
var
 node:POpParamNode;
begin
 node:=Emit.specialize New<TOpParamNode>;
 node.pParent:=Self;
 //
 pParam.InsertAfter(__prev,node);
 //
 node.Value:=p; //mark_read/mark_write
end;

procedure TspirvOp.AddLiteral(Value:PtrUint;const name:RawByteString='');
var
 pLiterals:PsrLiteralList;
 Literal:TsrLiteral;
begin
 pLiterals:=Emit.GetLiteralList;
 Literal:=pLiterals^.FetchLiteral(Value,Pchar(name));
 AddParam(Literal);
end;

procedure TspirvOp.AddString(const name:RawByteString);
var
 pLiterals:PsrLiteralList;
 Literal:TsrLiteralString;
begin
 pLiterals:=Emit.GetLiteralList;
 Literal:=pLiterals^.FetchString(Pchar(name));
 AddParam(Literal);
end;

procedure TspirvOp.SetType(t:TsrType);
begin
 if (FType=t) then Exit;
 if not is_cleared then
 begin
      t.mark_read  (Self);
  FType.mark_unread(Self);
 end;
 FType:=t;
end;

Procedure TspirvOp.SetDst(r:TsrNode);
begin
 if (Fdst=r) then Exit;

 Fdst.ResetWriter(Self);
 Fdst:=r;
 Fdst.SetWriter(Self,Self);
end;

function TspirvOp.is_cleared:Boolean;
begin
 Result:=(soClear in flags);
end;

function TspirvOp.Clear:Boolean;
var
 node:POpParamNode;
 b:Byte;
begin
 Result:=False;
 if not can_clear then Exit;

 Assert(read_count=0,Op.GetStr(OpId));

 FType.mark_unread(Self);
 node:=pParam.pHead;
 While (node<>nil) do
 begin
  //
  if (pParam.pHead=node) then
  begin
   b:=classif_rw_op(OpId);
  end else
  begin
   b:=1;
  end;
  //
  if (b and 2)<>0 then
  begin
   node.pValue.mark_unwrite(Self);
  end;
  //
  if (b and 1)<>0 then
  begin
   node.pValue.mark_unread(Self);
  end;
  //
  node:=node.pNext;
 end;

 flags:=flags-[soNotUsed];
 flags:=flags+[soClear];
 Result:=True;
end;

Procedure TspirvOp.UnClear;
var
 node:POpParamNode;
 b:Byte;
begin
 if not is_cleared then Exit;

 FType.mark_read(Self);
 node:=pParam.pHead;
 While (node<>nil) do
 begin
  //
  if (pParam.pHead=node) then
  begin
   b:=classif_rw_op(OpId);
  end else
  begin
   b:=1;
  end;
  //
  if (b and 2)<>0 then
  begin
   node.pValue.mark_write(Self);
  end;
  //
  if (b and 1)<>0 then
  begin
   node.pValue.mark_read(Self);
  end;
  //
  node:=node.pNext;
 end;

 flags:=flags-[soClear];
end;

procedure TspirvOp.mark_not_used;
begin
 flags:=flags+[soNotUsed];
end;

function TspirvOp.can_clear:Boolean;
begin
 if (soClear in flags)   then Exit(False);
 if (soNotUsed in flags) then Exit(True);
 if (OpId=Op.OpNop)      then Exit(True);
 if (Fdst=nil)           then Exit(False);
 Result:=not Fdst.IsUsed;
end;

//

procedure TsrOpBlock.Init;
begin
 dummy:=Emit.specialize New<TspirvOp>;
 dummy.Init(Op.OpNop);
 AddSpirvOp(dummy);
end;

procedure TsrOpBlock.SetCFGBlock(pLBlock:TsrCFGBlock);
begin
 dummy.Adr  :=pLBlock.pBLabel.Adr;
 Block.b_adr:=pLBlock.pBLabel.Adr;
 Block.e_adr:=pLBlock.pELabel.Adr;
 Block.bType:=pLBlock.bType;
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

procedure TsrOpBlock.SetLabels(pBegOp,pEndOp,pMrgOp:TspirvOp);
begin
 Labels.pBegOp:=pBegOp;
 Labels.pEndOp:=pEndOp;
 Labels.pMrgOp:=pMrgOp;
end;

procedure TsrOpBlock.SetCond(pReg:TsrRegNode;FVal:Boolean);
begin
 Cond.pReg:=pReg;
 Cond.FVal:=FVal;
end;

function TsrOpBlock.IsEndOf(Adr:TSrcAdr):Boolean;
begin
 Result:=(Block.e_adr.get_code_ptr<=Adr.get_code_ptr);
end;

function TsrOpBlock.FindUpLoop:TsrOpBlock;
var
 node:TsrOpBlock;
begin
 Result:=nil;
 node:=Self;
 While (node<>nil) do
 begin
  if (node.Block.bType=btLoop) then Exit(node);
  node:=node.pParent;
 end;
end;

function TsrOpBlock.FindUpCond(pReg:TsrRegNode):TsrOpBlock;
var
 node:TsrOpBlock;
begin
 Result:=nil;
 if (pReg=nil) then Exit;
 pReg:=RegDown(pReg);
 if pReg.is_const then Exit;
 node:=Self;
 While (node<>nil) do
 begin
  if (node.Block.bType=btCond) and (pReg=RegDown(node.Cond.pReg)) then Exit(node);
  node:=node.pParent;
 end;
end;

class function TSpirvFunc.c(n1,n2:PRawByteString):Integer;
var
 count1,count2:sizeint;
begin
 Count1:=Length(n1^);
 Count2:=Length(n2^);
 Result:=ord(Count1>Count2)-ord(Count1<Count2);
 if (Result=0) then
 begin
  Result:=CompareByte(PChar(n1^)^,PChar(n2^)^,Count1);
 end;
end;

Function TSpirvFunc.pTop:TsrOpBlock;
begin
 Result:=FTop;
end;

Procedure TSpirvFunc.PushBlock(New:TsrOpBlock);
begin
 if (New=nil) then Exit;
 New.pParent:=FBlock;
 New.FLevel :=FBlock.FLevel+1;
 FBlock:=New;
end;

function TSpirvFunc.PopBlock:Boolean;
begin
 Result:=False;
 if (FBlock=nil) then Exit;
 if (FBlock.pParent=nil) then Exit;
 FBlock:=FBlock.pParent;
 Result:=True;
end;

function TSpirvFunc.line:TspirvOp;
begin
 Result:=nil;
 if (FBlock<>nil) then
 begin
  Result:=FBlock.FList.pTail;
 end;
end;

Procedure TSpirvFunc.Init(const _name:RawByteString);
begin
 key:=_name;
 FTop:=Emit.specialize New<TsrOpBlock>;
 FTop.Init;
 FBlock:=FTop;
 FBlock.Init();
end;

function TSpirvFunc.NewSpirvOp(OpId:DWORD):TspirvOp;
begin
 Result:=Emit.specialize New<TSpirvOp>;
 Result.Init(OpId);
end;

function TSpirvFunc.AddSpirvOp(node:TspirvOp):TspirvOp;
begin
 Result:=node;
 if (node=nil) then Exit;
 FBlock.AddSpirvOp(node);
end;

function TSpirvFunc.AddSpirvOp(OpId:DWORD):TspirvOp;
begin
 Result:=AddSpirvOp(NewSpirvOp(OpId));
end;

function TSpirvFunc.GetPrintName:RawByteString;
begin
 if (name<>'') then
 begin
  Result:=name;
 end else
 begin
  Assert(ID.Alloc);
  Result:='f'+IntToStr(ID.ID);
 end;
end;

//

procedure TsrOpBlockCustom.AddIndex(d:DWORD);
var
 node:TsrOpBlock;
begin
 node:=TsrOpBlock(Self);
 While (node<>nil) do
 begin
  node.FIndex:=node.FIndex+d;
  node:=node.pParent;
 end;
end;

procedure TsrOpBlockCustom.SubIndex(d:DWORD);
var
 node:TsrOpBlock;
begin
 node:=TsrOpBlock(Self);
 While (node<>nil) do
 begin
  Assert(node.FIndex>=d);
  node.FIndex:=node.FIndex-d;
  node:=node.pParent;
 end;
end;

procedure TsrOpBlockCustom.InsertAfter(node,new:TsrOpCustom);
begin
 FList.InsertAfter(node,new);
 new.pParent:=Self;
 AddIndex(new.GetIndexCount);
 mark_read(new);
end;

procedure TsrOpBlockCustom.InsertBefore(node,new:TsrOpCustom);
begin
 FList.InsertBefore(node,new);
 new.pParent:=Self;
 AddIndex(new.GetIndexCount);
 mark_read(new);
end;

procedure TsrOpBlockCustom.Remove(node:TsrOpCustom);
begin
 Assert(node.pParent=Self);
 FList.Remove(node);
 node.pParent:=nil;
 SubIndex(node.GetIndexCount);
 mark_unread(node);
end;

procedure TsrOpBlockCustom.UpdateLevel;
begin
 if (pParent=nil) then
 begin
  FLevel:=0;
 end else
 begin
  FLevel:=TsrOpBlockCustom(pParent).FLevel+1;
 end;
end;

function TsrOpBlockCustom.NewSpirvOp(OpId:DWORD):TspirvOp;
begin
 Result:=Emit.specialize New<TSpirvOp>;
 Result.Init(OpId);
end;

function TsrOpBlockCustom.AddSpirvOp(node:TspirvOp):TspirvOp;
begin
 Result:=node;
 if (node=nil) then Exit;
 FList.Push_tail(node);
 node.pParent:=Self;
 AddIndex(node.GetIndexCount);
 mark_read(node);
end;

function TsrOpBlockCustom.AddSpirvOp(OpId:DWORD):TspirvOp;
begin
 Result:=AddSpirvOp(NewSpirvOp(OpId));
end;

function TsrOpBlockCustom.line:TsrNode;
begin
 Result:=nil;
 if (Self<>nil) then
 begin
  Result:=FList.pTail;
 end;
end;

//

function TsrFuncList.Search(const name:RawByteString):TSpirvFunc;
begin
 Result:=FTree.Find(@name);
end;

procedure TsrFuncList.Insert(new:TSpirvFunc);
begin
 FTree.Insert(new);
 FList.Push_head(new);
end;

function TsrFuncList.First:TSpirvFunc; inline;
begin
 Result:=FList.pHead;
end;

//--

Function classif_rw_op(OpId:DWORD):Byte;
begin
 Case OpId of
  Op.OpStore,
  Op.OpImageWrite,
  Op.OpAtomicStore:
   Result:=2; //w
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
   Result:=3; //rw
  else
   Result:=1;
 end;
end;

end.

