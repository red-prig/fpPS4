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
 ntOpCustom=class(TsrNodeVmt)
  class function  Next  (node:PsrNode):Pointer;             override;
  class function  Prev  (node:PsrNode):Pointer;             override;
  class function  Parent(node:PsrNode):Pointer;             override;
 end;

 ntOp=class(ntOpCustom)
  class Procedure zero_read(node:PsrNode);                  override;
  class Procedure PrepType(node:PPrepTypeNode);             override;
  class Function  GetPtype (node:PsrNode):PsrNode;          override;
  class function  GetIndexCount(node:PsrNode):DWORD;        override;
 end;

 ntOpBlock=class(ntOpCustom)
  class function  First        (node:PsrNode):Pointer;      override;
  class function  Last         (node:PsrNode):Pointer;      override;
  class function  GetIndexCount(node:PsrNode):DWORD;        override;
 end;

 ntFunc=class(TsrNodeVmt)
  class function  First       (node:PsrNode):Pointer;       override;
  class function  Last        (node:PsrNode):Pointer;       override;
  class function  GetRef      (node:PsrNode):Pointer;       override;
  class function  GetPrintName(node:PsrNode):RawByteString; override;
 end;

 PPspirvOp=^PspirvOp;
 PspirvOp=^TspirvOp;

 POpParamNode=^TOpParamNode;
 TOpParamNode=packed object
  private
   pNext:POpParamNode;
   pParent:PspirvOp;
   pValue:PsrNode;
   procedure SetValue(v:PsrNode);
  public
   property Next:POpParamNode read pNext;
   property Parent:PspirvOp   read pParent;
   property Value:PsrNode     read pValue write SetValue;
   function AsReg:Pointer;
   function TryGetValue(var V:PtrUint):Boolean;
 end;

 TOpParamQueue=specialize TNodeQueue<POpParamNode>;

 PsrOpBlock=^TsrOpBlock;

 PsrOpCustom=^TsrOpCustom;
 TsrOpCustom=object(TsrNode)
  private
   pPrev,pNext:PsrOpCustom;
   pParent:PsrOpBlock;
 end;

 TsrOpList=specialize TNodeList<PsrOpCustom>;

 TsoFlags=(soClear,soNotUsed);
 TsoSetFlags=Set of TsoFlags;

 TspirvOp=object(TsrOpCustom)
  private
   pParam:TOpParamQueue;
   FType:PsrType;
   Fdst:PsrNode;
   flags:TsoSetFlags;
   procedure SetType(t:PsrType);
   Procedure SetDst(r:PsrNode);
   Procedure UnClear;
  public
   Adr:TSrcAdr;
   OpId:DWORD;
   property  pType:PsrType read FType write SetType;
   property  pDst:PsrNode  read Fdst  write SetDst;
   procedure Init(_OpId:DWORD); inline;
   procedure InsertAfter(new:PspirvOp);
   procedure InsertBefore(new:PspirvOp);
   procedure Remove;
   Function  Alloc(Size:ptruint):Pointer;
   function  ParamFirst:POpParamNode;
   function  ParamLast:POpParamNode;
   function  ParamNode(i:Byte):POpParamNode;
   procedure AddParam(p:Pointer);
   procedure AddParamAfter(_prev:POpParamNode;p:Pointer);
   procedure AddLiteral(Value:PtrUint;const name:RawByteString='');
   procedure AddString(const name:RawByteString);
   function  is_cleared:Boolean;
   function  Clear:Boolean;
   procedure mark_not_used;
   function  can_clear:Boolean;
 end;

 TsrVolMark=(vmNone,vmEnd,vmBreak,vmCont);

 TsrBlockInfo=packed record
  b_adr,e_adr:TSrcAdr;
  bType:TsrBlockType;
 end;

 TsrOpBlockCustom=object(TsrOpCustom)
  private
   FEmit:TCustomEmit;
   FList:TsrOpList;
   FIndex:DWORD;
   FLevel:DWORD;
   procedure AddIndex(d:DWORD);
   procedure SubIndex(d:DWORD);
  public
   property  Emit:TCustomEmit read FEmit;
   property  Level:DWORD      read FLevel;
   procedure Init(_Emit:TCustomEmit);
   procedure InsertAfter(node,new:PsrOpCustom);
   procedure InsertBefore(node,new:PsrOpCustom);
   procedure Remove(node:PsrOpCustom);
   procedure UpdateLevel;
   Function  Alloc(Size:ptruint):Pointer;
   function  NewSpirvOp(OpId:DWORD):PSpirvOp;
   function  AddSpirvOp(node:PSpirvOp):PSpirvOp;
   function  AddSpirvOp(OpId:DWORD):PSpirvOp;
   function  line:Pointer;
 end;

 TsrOpBlock=packed object(TsrOpBlockCustom)
  public

   Block:TsrBlockInfo;

   Labels:record
    pBegOp:PspirvOp;
    pEndOp:PspirvOp;
    pMrgOp:PspirvOp;
   end;

   FCursor:TsrCursor;

   Regs:record
    pSnap_org:PsrRegsSnapshot;
    pSnap_cur:PsrRegsSnapshot;
    FVolMark:TsrVolMark;
   end;

   Cond:record
    pReg:PsrRegNode;
    FVal:Boolean;
    FUseCont:Boolean;
   end;

   dummy:TspirvOp;

   procedure Init(_Emit:TCustomEmit);
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
 TSpirvFunc=object(TsrNode)
  private
   pPrev,pNext,pLeft,pRight:PSpirvFunc;
   //----
   FName:RawByteString;
   FTop:TsrOpBlock;
   FBlock:PsrOpBlock;
   ID:TsrRefId; //post id
   function  c(n1,n2:PSpirvFunc):Integer; static;
  public
   property  Name:RawByteString read FName;
   property  pBlock:PsrOpBlock  read FBlock;
   Function  pTop:PsrOpBlock;
   Procedure PushBlock(New:PsrOpBlock);
   function  PopBlock:Boolean;
   function  line:Pointer;
   Procedure Init(const _name:RawByteString;Emit:TCustomEmit);
   function  NewSpirvOp(OpId:DWORD):PSpirvOp;
   function  AddSpirvOp(node:PSpirvOp):PSpirvOp;
   function  AddSpirvOp(OpId:DWORD):PSpirvOp;
   function  GetPrintName:RawByteString;
 end;

 PsrFuncList=^TsrFuncList;
 TsrFuncList=object
  type
   TNodeList=specialize TNodeList<PSpirvFunc>;
   TNodeFetch=specialize TNodeFetch<PSpirvFunc,TSpirvFunc>;
  var
   FList:TNodeList;
   FNTree:TNodeFetch;
  function  Search(const name:RawByteString):PSpirvFunc;
  procedure Insert(new:PSpirvFunc);
  function  First:PSpirvFunc; inline;
 end;

Function is_write_op(OpId:DWORD):Boolean;

implementation

class function ntOpCustom.Next(node:PsrNode):Pointer;
begin
 Result:=PsrOpCustom(node)^.pNext;
end;

class function ntOpCustom.Prev(node:PsrNode):Pointer;
begin
 Result:=PsrOpCustom(node)^.pPrev;
end;

class function ntOpCustom.Parent(node:PsrNode):Pointer;
begin
 Result:=PsrOpCustom(node)^.pParent;
end;

//

class Procedure ntOp.zero_read(node:PsrNode);
begin
 PspirvOp(node)^.UnClear;
end;

class Function ntOp.GetPtype(node:PsrNode):PsrNode;
begin
 Result:=PspirvOp(node)^.FType;
end;

class function ntOp.GetIndexCount(node:PsrNode):DWORD;
begin
 Result:=1;
end;

class Procedure ntOp.PrepType(node:PPrepTypeNode);
var
 new:TsrDataType;
 pNode:POpParamNode;
 pTypeList:PsrTypeList;
begin
 With PspirvOp(node^.dnode)^ do
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
       pTypeList:=pParent^.FEmit.GetTypeList;
       pType:=pTypeList^.Fetch(new);
       //next
       node^.dnode:=pNode^.pValue;
       Exit;
      end;
     end;
   else;
  end;

 end;

 node^.dnode:=nil;
end;

//

class function ntOpBlock.First(node:PsrNode):Pointer;
begin
 Result:=PsrOpBlock(node)^.FList.pHead;
end;

class function ntOpBlock.Last(node:PsrNode):Pointer;
begin
 Result:=PsrOpBlock(node)^.FList.pTail;
end;

class function ntOpBlock.GetIndexCount(node:PsrNode):DWORD;
begin
 Result:=PsrOpBlock(node)^.FIndex;
end;

//

class function ntFunc.First(node:PsrNode):Pointer;
begin
 Result:=PSpirvFunc(node)^.FTop.FList.pHead;
end;

class function ntFunc.Last(node:PsrNode):Pointer;
begin
 Result:=PSpirvFunc(node)^.FTop.FList.pTail;
end;

class function ntFunc.GetPrintName(node:PsrNode):RawByteString;
begin
 Result:=PSpirvFunc(node)^.GetPrintName;
end;

class function ntFunc.GetRef(node:PsrNode):Pointer;
begin
 Result:=@PSpirvFunc(node)^.ID;
end;

//

procedure TOpParamNode.SetValue(v:PsrNode);
begin
 if (pValue=v) then Exit;
 Assert(pParent<>nil);
 if not pParent^.is_cleared then
 begin
       v^.mark_read  (pParent);
  pValue^.mark_unread(pParent);
 end;
 pValue:=v;
end;

function TOpParamNode.AsReg:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 Result:=pValue^.AsType(ntReg);
end;

function TOpParamNode.TryGetValue(var V:PtrUint):Boolean;
begin
 Result:=False;
 if (@Self=nil) then Exit;
 if pValue^.IsType(ntLiteral) then
 begin
  V:=PsrLiteral(pValue)^.Value;
  Result:=True;
 end;
end;


//

procedure TspirvOp.Init(_OpId:DWORD); inline;
begin
 fntype:=ntOp;
 OpId:=_OpId;
end;

procedure TspirvOp.InsertAfter(new:PspirvOp);
begin
 Assert(new<>nil);
 Assert(pParent<>nil);
 pParent^.InsertAfter(@Self,new);
end;

procedure TspirvOp.InsertBefore(new:PspirvOp);
begin
 Assert(new<>nil);
 Assert(pParent<>nil);
 pParent^.InsertBefore(@Self,new);
end;

procedure TspirvOp.Remove;
begin
 Assert(pParent<>nil);
 pParent^.Remove(@Self);
end;

Function TspirvOp.Alloc(Size:ptruint):Pointer;
begin
 Result:=pParent^.Alloc(Size);
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
  node:=node^.pNext;
 end;
end;

procedure TspirvOp.AddParam(p:Pointer);
var
 node:POpParamNode;
begin
 node:=Alloc(SizeOf(TOpParamNode));
 node^.pParent:=@Self;
 node^.pValue :=p;
 pParam.Push_tail(node);

 if not is_cleared then
 begin
  PsrNode(p)^.mark_read(@Self);
 end;
end;

procedure TspirvOp.AddParamAfter(_prev:POpParamNode;p:Pointer);
var
 node:POpParamNode;
begin
 node:=Alloc(SizeOf(TOpParamNode));
 node^.pParent:=@Self;
 node^.pValue:=p;
 pParam.InsertAfter(_prev,node);

 if not is_cleared then
 begin
  PsrNode(p)^.mark_read(@Self);
 end;
end;

procedure TspirvOp.AddLiteral(Value:PtrUint;const name:RawByteString='');
var
 pLiterals:PsrLiteralList;
 Literal:PsrLiteral;
begin
 pLiterals:=pParent^.FEmit.GetLiteralList;
 Literal:=pLiterals^.FetchLiteral(Value,Pchar(name));
 AddParam(Literal);
end;

procedure TspirvOp.AddString(const name:RawByteString);
var
 pLiterals:PsrLiteralList;
 Literal:PsrLiteralString;
begin
 pLiterals:=pParent^.FEmit.GetLiteralList;
 Literal:=pLiterals^.FetchString(Pchar(name));
 AddParam(Literal);
end;

procedure TspirvOp.SetType(t:PsrType);
begin
 if (FType=t) then Exit;
 if not is_cleared then
 begin
      t^.mark_read  (@Self);
  FType^.mark_unread(@Self);
 end;
 FType:=t;
end;

Procedure TspirvOp.SetDst(r:PsrNode);
begin
 if (Fdst=r) then Exit;

 if is_write_op(OpId) then
 begin
  if not is_cleared then
  begin
      r^.mark_write  (@Self);
   Fdst^.mark_unwrite(@Self);
  end;
  Fdst:=r;
 end else
 begin
  Fdst^.ResetWriter(@Self);
  Fdst:=r;
  Fdst^.SetWriter(@Self,@Self);
 end;

end;

function TspirvOp.is_cleared:Boolean;
begin
 Result:=(soClear in flags);
end;

function TspirvOp.Clear:Boolean;
var
 node:POpParamNode;
begin
 Result:=False;
 if not can_clear then Exit;

 Assert(read_count=0);

 if is_write_op(OpId) then
 begin
  Fdst^.mark_unwrite(@Self);
 end;

 FType^.mark_unread(@Self);
 node:=pParam.pHead;
 While (node<>nil) do
 begin
  node^.pValue^.mark_unread(@Self);
  node:=node^.pNext;
 end;

 flags:=flags-[soNotUsed];
 flags:=flags+[soClear];
 Result:=True;
end;

Procedure TspirvOp.UnClear;
var
 node:POpParamNode;
begin
 if not is_cleared then Exit;

 if is_write_op(OpId) then
 begin
  Fdst^.mark_write(@Self);
 end;

 FType^.mark_read(@Self);
 node:=pParam.pHead;
 While (node<>nil) do
 begin
  node^.pValue^.mark_read(@Self);
  node:=node^.pNext;
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
 Result:=not Fdst^.IsUsed;
end;

//

procedure TsrOpBlock.Init(_Emit:TCustomEmit);
begin
 fntype:=ntOpBlock;
 FEmit :=_Emit;
 dummy.Init(Op.OpNop);
 AddSpirvOp(@dummy);
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
 Count1:=Length(n1^.FName);
 Count2:=Length(n2^.FName);
 Result:=Integer(Count1>Count2)-Integer(Count1<Count2);
 if (Result=0) then
 begin
  Result:=CompareByte(PChar(n1^.FName)^,PChar(n2^.FName)^,Count1);
 end;
end;

Function TSpirvFunc.pTop:PsrOpBlock;
begin
 Result:=@FTop;
end;

Procedure TSpirvFunc.PushBlock(New:PsrOpBlock);
begin
 if (New=nil) then Exit;
 New^.pParent:=FBlock;
 New^.FLevel :=FBlock^.FLevel+1;
 FBlock:=New;
end;

function TSpirvFunc.PopBlock:Boolean;
begin
 Result:=False;
 if (FBlock=nil) then Exit;
 if (FBlock^.pParent=nil) then Exit;
 FBlock:=FBlock^.pParent;
 Result:=True;
end;

function TSpirvFunc.line:Pointer;
begin
 Result:=nil;
 if (FBlock<>nil) then
 begin
  Result:=FBlock^.FList.pTail;
 end;
end;

Procedure TSpirvFunc.Init(const _name:RawByteString;Emit:TCustomEmit);
begin
 fntype:=ntFunc;
 FName:=_name;
 FBlock:=@FTop;
 FBlock^.Init(Emit);
end;

function TSpirvFunc.NewSpirvOp(OpId:DWORD):PSpirvOp;
begin
 Result:=FTop.Alloc(SizeOf(TSpirvOp));
 Result^.Init(OpId);
end;

function TSpirvFunc.AddSpirvOp(node:PSpirvOp):PSpirvOp;
begin
 Result:=node;
 if (node=nil) then Exit;
 FBlock^.AddSpirvOp(node);
end;

function TSpirvFunc.AddSpirvOp(OpId:DWORD):PSpirvOp;
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

procedure TsrOpBlockCustom.Init(_Emit:TCustomEmit);
begin
 fntype:=ntOpBlock;
 FEmit :=_Emit;
end;

procedure TsrOpBlockCustom.AddIndex(d:DWORD);
var
 node:PsrOpBlock;
begin
 node:=@Self;
 While (node<>nil) do
 begin
  node^.FIndex:=node^.FIndex+d;
  node:=node^.pParent;
 end;
end;

procedure TsrOpBlockCustom.SubIndex(d:DWORD);
var
 node:PsrOpBlock;
begin
 node:=@Self;
 While (node<>nil) do
 begin
  Assert(node^.FIndex>=d);
  node^.FIndex:=node^.FIndex-d;
  node:=node^.pParent;
 end;
end;

procedure TsrOpBlockCustom.InsertAfter(node,new:PsrOpCustom);
begin
 FList.InsertAfter(node,new);
 new^.pParent:=@Self;
 AddIndex(new^.GetIndexCount);
 mark_read(@Self);
end;

procedure TsrOpBlockCustom.InsertBefore(node,new:PsrOpCustom);
begin
 FList.InsertBefore(node,new);
 new^.pParent:=@Self;
 AddIndex(new^.GetIndexCount);
 mark_read(@Self);
end;

procedure TsrOpBlockCustom.Remove(node:PsrOpCustom);
begin
 Assert(node^.pParent=@Self);
 FList.Remove(node);
 node^.pParent:=nil;
 SubIndex(node^.GetIndexCount);
 mark_unread(@Self);
end;

procedure TsrOpBlockCustom.UpdateLevel;
begin
 if (pParent=nil) then
 begin
  FLevel:=0;
 end else
 begin
  FLevel:=pParent^.FLevel+1;
 end;
end;

Function TsrOpBlockCustom.Alloc(Size:ptruint):Pointer;
begin
 Result:=FEmit.Alloc(Size);
end;

function TsrOpBlockCustom.NewSpirvOp(OpId:DWORD):PSpirvOp;
begin
 Result:=Alloc(SizeOf(TSpirvOp));
 Result^.Init(OpId);
end;

function TsrOpBlockCustom.AddSpirvOp(node:PSpirvOp):PSpirvOp;
begin
 Result:=node;
 if (node=nil) then Exit;
 FList.Push_tail(node);
 node^.pParent:=@Self;
 AddIndex(node^.GetIndexCount);
 mark_read(@Self);
end;

function TsrOpBlockCustom.AddSpirvOp(OpId:DWORD):PSpirvOp;
begin
 Result:=AddSpirvOp(NewSpirvOp(OpId));
end;

function TsrOpBlockCustom.line:Pointer;
begin
 Result:=nil;
 if (@Self<>nil) then
 begin
  Result:=FList.pTail;
 end;
end;

//

function TsrFuncList.Search(const name:RawByteString):PSpirvFunc;
var
 node:TSpirvFunc;
begin
 node:=Default(TSpirvFunc);
 node.FName:=name;
 Result:=FNTree.Find(@node);
end;

procedure TsrFuncList.Insert(new:PSpirvFunc);
begin
 FNTree.Insert(new);
 FList.Push_head(new);
end;

function TsrFuncList.First:PSpirvFunc; inline;
begin
 Result:=FList.pHead;
end;

//--

Function is_write_op(OpId:DWORD):Boolean;
begin
 Case OpId of
  Op.OpStore,
  Op.OpImageWrite,
  Op.OpAtomicStore,
  Op.OpAtomicExchange,
  Op.OpAtomicCompareExchange,
  Op.OpAtomicCompareExchangeWeak:Result:=True;
  else
   Result:=False;
 end;
end;

end.

