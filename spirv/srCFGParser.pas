unit srCFGParser;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 ps4_pssl,
 ginodes,
 srNode,
 srConfig;

type
 TsrCodeRegion=class;

 PSrcAdr=^TSrcAdr;
 TSrcAdr=object
  pCode :TsrCodeRegion;
  Offset:PtrUInt;
  function get_code_ptr:PDWORD;
  function get_dmem_ptr:PDWORD;
 end;

 TsrSourceNode=class;

 TsrSourceNodeType=class of TsrSourceNode;

 TsrSourceBlock=class;

 TsrSourceNode=class
  pParent:TsrSourceBlock;
  pPrev,pNext:TsrSourceNode;
  //
  order:PtrUint;
  //
  function ntype:TsrSourceNodeType; inline;
  function First:TsrSourceNode; virtual;
  function Last :TsrSourceNode; virtual;
  function get_level:DWORD;
 end;

 TsrSourceNodeList=specialize TNodeListClass<TsrSourceNode>;

 //

 TsrSourceAdr=class(TsrSourceNode)
  key  :TSrcAdr; //b_adr
  e_adr:TSrcAdr;
  property b_adr:TSrcAdr read key write key;
  class function c(n1,n2:PSrcAdr):Integer; static;
 end;

 TsrSourceLabel=class(TsrSourceAdr)
  pLeft,pRight:TsrSourceLabel;
  links_count:PtrUint;
 end;

 TsrSourceLabelTree=specialize TNodeTreeClass<TsrSourceLabel>;

 TsrSourceInstruction=class(TsrSourceAdr)
  pLeft,pRight:TsrSourceInstruction;
  //
  FSPI:TSPI;
  //
  used:Boolean;
 end;

 TsrSourceInstructionTree=specialize TNodeTreeClass<TsrSourceInstruction>;

 //

 TsrCondition=(
  cNone,
  cFalse,
  cTrue,
  cScc0,
  cScc1,
  cVccz,
  cVccnz,
  cExecz,
  cExecnz,
  cTidz,
  cTidnz
 );

 TsrStatementType=(
  sCond,
  sCopy,
  sVar,
  sStore,
  sBreak,
  sNot,
  sOr,
  sAnd
 );

 //sCond  TsrCondition
 //sCopy  [self] [false]:sCond/sCopy/sNot/sOr/sAnd

 //sVar   id

 //sStore [var]:sVar [false]:sCond/sCopy/sNot/sOr/sAnd

 //sBreak

 //sNot   [cond]:sCond/sCopy/sNot/sOr

 //sOr    [cond]:sCond/sCopy/sNot/sOr/sAnd [cond]:sCond/sLoad/sNot/sOr/sAnd
 //sAnd   [cond]:sCond/sCopy/sNot/sOr/sAnd [cond]:sCond/sLoad/sNot/sOr/sAnd

 TsrStatement=class(TsrSourceNode)
  sType:TsrStatementType;
  pSrc :TsrStatement;
  pDst :TsrStatement;
  //
  pCache:TObject;
  u:record
   Case Byte of
    0:(id  :PtrUint);
    1:(cond:TsrCondition);
  end;
 end;

 TsrSourceGoto=class(TsrSourceNode)
  FGoto:record
   pPrev,pNext:TsrSourceGoto;
  end;
  //
  pCond :TsrStatement;
  pLabel:TsrSourceLabel;
 end;

 TsrGotoList=object
  pHead,pTail:TsrSourceGoto;
  //pMiddle:TsrSourceGoto;
  procedure Push_head(Node:TsrSourceGoto);
  procedure Push_tail(Node:TsrSourceGoto);
  procedure InsertAfter (node,new:TsrSourceGoto);
  procedure InsertBefore(node,new:TsrSourceGoto);
  function  Pop_tail:TsrSourceGoto;
  procedure Remove(node:TsrSourceGoto);
 end;

 //

 TsrBlockType=(btMain,btSetpc,btCond,btElse,btLoop,btMerg,btExec,btInline,btOther);

 TsrSourceBlock=class(TsrSourceNode)
  FList:TsrSourceNodeList;
  FInit:TsrSourceNodeList;
  pCond:TsrStatement;
  bType:TsrBlockType;
  //
  pIf  :TsrSourceBlock;
  pElse:TsrSourceBlock;
  //
  function  pReal:TsrSourceBlock;
  function  First:TsrSourceNode; override;
  function  Last :TsrSourceNode; override;
  procedure Push_head(Node:TsrSourceNode);
  procedure Push_tail(Node:TsrSourceNode);
  function  Pop_head:TsrSourceNode;
  function  Pop_tail:TsrSourceNode;
  procedure InsertAfter(node,new:TsrSourceNode);
  procedure InsertBefore(node,new:TsrSourceNode);
  procedure Remove(node:TsrSourceNode);
  Procedure splice(node_first,node_last:TsrSourceNode);
 end;

 //

 TsrCodeRegion=class
  pNext :TsrCodeRegion;
  //
  FEmit :TCustomEmit;
  Body  :Pointer;
  DMem  :Pointer;
  Size  :PtrUint;
  //
  FTop  :TsrSourceBlock;
  //
  function FindByPtr(base:Pointer):TsrSourceNode;
 end;

function get_branch_offset(var FSPI:TSPI):ptrint;
function IsReal(b:TsrBlockType):Boolean;
function parse_code_cfg2(var pCode:TsrCodeRegion;bType:TsrBlockType;Body,Dmem:Pointer;FEmit:TCustomEmit):Integer;

implementation

type
 TsrCFGParser2=object
  FEmit:TCustomEmit;
  pCode:TsrCodeRegion;
  pCurr:TsrSourceNode;
  order:PtrUint;
  VarId:Ptruint;
  //
  FLabelTree:TsrSourceLabelTree;
  FInstructionTree:TsrSourceInstructionTree;
  FGotoList:TsrGotoList;
  FGotoFree:TsrGotoList;
  //
  Function  NewInstruction(b_adr,e_adr:TSrcAdr;var FSPI:TSPI):TsrSourceInstruction;
  Function  NewLabel      (Adr:TSrcAdr):TsrSourceLabel;
  Function  NewGoto       (Cond:TsrCondition;Adr:TSrcAdr):TsrSourceGoto;
  Function  NewGoto       (pCond:TsrStatement;pLabel:TsrSourceLabel):TsrSourceGoto;
  procedure FreeGoto      (node:TsrSourceGoto);
  Function  NewBlock      (bType:TsrBlockType):TsrSourceBlock;
  //
  Function  NewCond (cond:TsrCondition):TsrStatement;
  Function  NewCopy (pCond:TsrStatement):TsrStatement;
  Function  NewVar  :TsrStatement;
  Function  NewStore(pVar,pCond:TsrStatement):TsrStatement;
  Function  NewBreak:TsrStatement;
  Function  NewNot  (pCond:TsrStatement):TsrStatement;
  Function  NewOr   (pCond1,pCond2:TsrStatement):TsrStatement;
  Function  NewAnd  (pCond1,pCond2:TsrStatement):TsrStatement;
  //
  procedure EmitGoto(instr:TsrSourceInstruction;Adr:TSrcAdr);
  function  Parse:Integer;
  procedure EmitLabels;
  procedure GotoPass;
  procedure InitMovePass;
  procedure RemoveGoto(goto_stmt:TsrSourceGoto;simple_pass:Boolean);
  procedure EliminateAsConditional(goto_stmt:TsrSourceGoto);
  procedure EliminateAsLoop       (goto_stmt:TsrSourceGoto);
  function  MoveOutward           (goto_stmt:TsrSourceGoto):TsrSourceGoto;
  function  MoveOutwardIf         (goto_stmt:TsrSourceGoto):TsrSourceGoto;
  function  MoveOutwardElse       (goto_stmt:TsrSourceGoto):TsrSourceGoto;
  function  MoveOutwardLoop       (goto_stmt:TsrSourceGoto):TsrSourceGoto;
  function  MoveOutwardSwitch     (goto_stmt:TsrSourceGoto):TsrSourceGoto;
  function  MoveInward            (goto_stmt:TsrSourceGoto):TsrSourceGoto;
  function  Lift                  (goto_stmt:TsrSourceGoto):TsrSourceGoto;
  procedure ExecPass;
  procedure Print;
 end;

function get_branch_offset(var FSPI:TSPI):ptrint;
begin
 Result:=FSPI.OFFSET_DW+Smallint(FSPI.SOPP.SIMM)+1;
end;

function IsReal(b:TsrBlockType):Boolean;
begin
 case b of
  btMain,
  btSetpc,
  btCond,
  btElse,
  btLoop:Result:=True;
  else
         Result:=False;
 end;
end;

function TsrSourceNode.ntype:TsrSourceNodeType; inline;
begin
 Result:=TsrSourceNodeType(ClassType);
end;

function TsrSourceNode.First:TsrSourceNode;
begin
 Result:=nil;
end;

function TsrSourceNode.Last:TsrSourceNode;
begin
 Result:=nil;
end;

function TsrSourceNode.get_level:DWORD;
var
 node:TsrSourceBlock;
begin
 node:=pParent;
 Result:=0;
 While (node<>nil) do
 begin
  if isReal(node.bType) then
  begin
   Inc(Result);
  end;
  node:=node.pParent;
 end;
end;

///

class function TsrSourceAdr.c(n1,n2:PSrcAdr):Integer;
var
 ptr1,ptr2:Pointer;
begin
 ptr1:=n1^.get_code_ptr;
 ptr2:=n2^.get_code_ptr;
 Result:=ord(ptr1>ptr2)-ord(ptr1<ptr2);
end;

///

procedure TsrGotoList.Push_head(Node:TsrSourceGoto);
begin
 if (pHead=nil) then
 begin
  pTail:=node;
  node.FGoto.pNext:=nil;
 end else
 begin
  pHead.FGoto.pPrev:=node;
  node.FGoto.pNext:=pHead;
 end;
 node.FGoto.pPrev:=nil;
 pHead:=node;
end;

procedure TsrGotoList.Push_tail(Node:TsrSourceGoto);
begin
 if (pTail=nil) then
 begin
  pHead:=node;
  node.FGoto.pPrev:=nil;
 end else
 begin
  pTail.FGoto.pNext:=node;
  node.FGoto.pPrev:=pTail;
 end;
 node.FGoto.pNext:=nil;
 pTail:=node;
end;

procedure TsrGotoList.InsertAfter(node,new:TsrSourceGoto);
begin
 new.FGoto.pPrev:=node;
 if (node.FGoto.pNext=nil) then
 begin
  new.FGoto.pNext:=nil;
  pTail:=new;
 end else
 begin
  Assert(node.FGoto.pNext.FGoto.pPrev=node);
  new.FGoto.pNext:=node.FGoto.pNext;
  node.FGoto.pNext.FGoto.pPrev:=new;
 end;
 node.FGoto.pNext:=new;
end;

procedure TsrGotoList.InsertBefore(node,new:TsrSourceGoto);
begin
 new.FGoto.pNext:=node;
 if (node.FGoto.pPrev=nil) then
 begin
  new.FGoto.pPrev:=nil;
  pHead:=new;
 end else
 begin
  Assert(node.FGoto.pPrev.FGoto.pNext=node);
  new.FGoto.pPrev:=node.FGoto.pPrev;
  node.FGoto.pPrev.FGoto.pNext:=new;
 end;
 node.FGoto.pPrev:=new;
end;

function TsrGotoList.Pop_tail:TsrSourceGoto;
begin
 if (pTail=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=pTail;
  pTail:=pTail.FGoto.pPrev;
  if (pTail=nil) then
  begin
   pHead:=nil;
  end else
  begin
   pTail.FGoto.pNext:=nil;
  end;
  Result.FGoto.pPrev:=nil;
  Result.FGoto.pNext:=nil;
 end;
end;

procedure TsrGotoList.Remove(node:TsrSourceGoto);
begin
 if (node.FGoto.pPrev=nil) then
 begin
  if (pHead=node) then
  begin
   pHead:=node.FGoto.pNext;
  end;
 end else
 begin
  Assert(node.FGoto.pPrev.FGoto.pNext=node);
  node.FGoto.pPrev.FGoto.pNext:=node.FGoto.pNext;
 end;
 if (node.FGoto.pNext=nil) then
 begin
  if (pTail=node) then
  begin
   pTail:=node.FGoto.pPrev;
  end;
 end else
 begin
  Assert(node.FGoto.pNext.FGoto.pPrev=node);
  node.FGoto.pNext.FGoto.pPrev:=node.FGoto.pPrev;
 end;
 node.FGoto.pPrev:=nil;
 node.FGoto.pNext:=nil;
end;

///

function TsrSourceBlock.pReal:TsrSourceBlock;
begin
 if (Self=nil) then Exit(nil);
 Result:=Self;
 while (Result<>nil) do
 begin
  if IsReal(Result.bType) then Break;
  Result:=Result.pParent;
 end;
end;

function TsrSourceBlock.First:TsrSourceNode;
begin
 Result:=FList.pHead;
end;

function TsrSourceBlock.Last:TsrSourceNode;
begin
 Result:=FList.pTail;
end;

procedure TsrSourceBlock.Push_head(Node:TsrSourceNode);
begin
 Assert(Node.pParent=nil);
 FList.Push_head(Node);
 Node.pParent:=Self;
end;

procedure TsrSourceBlock.Push_tail(Node:TsrSourceNode);
begin
 Assert(Node.pParent=nil);
 FList.Push_tail(Node);
 Node.pParent:=Self;
end;

function  TsrSourceBlock.Pop_head:TsrSourceNode;
begin
 Result:=FList.Pop_head;
 if (Result<>nil) then
 begin
  Result.pParent:=nil;
 end;
end;

function  TsrSourceBlock.Pop_tail:TsrSourceNode;
begin
 Result:=FList.Pop_tail;
 if (Result<>nil) then
 begin
  Result.pParent:=nil;
 end;
end;

procedure TsrSourceBlock.InsertAfter(node,new:TsrSourceNode);
begin
 Assert(node<>nil);
 Assert(node.pParent=Self);
 Assert(new.pParent=nil);
 //
 FList.InsertAfter(Node,new);
 new.pParent:=Self;
end;

procedure TsrSourceBlock.InsertBefore(node,new:TsrSourceNode);
begin
 Assert(node<>nil);
 Assert(node.pParent=Self);
 Assert(new.pParent=nil);
 //
 FList.InsertBefore(Node,new);
 new.pParent:=Self;
end;

procedure TsrSourceBlock.Remove(node:TsrSourceNode);
begin
 Assert(node<>nil);
 Assert(node.pParent=Self);
 //
 FList.Remove(node);
 node.pParent:=nil;
 node.pPrev  :=nil;
 node.pNext  :=nil;
end;

Procedure TsrSourceBlock.splice(node_first,node_last:TsrSourceNode); //[node_first..node_last)
var
 node:TsrSourceNode;
 next:TsrSourceNode;
 parent:TsrSourceBlock;
begin
 Assert(node_first<>nil);

 if (node_last<>nil) then
 begin
  Assert(node_first.pParent=node_last.pParent);
 end;

 parent:=node_first.pParent;
 node:=node_first;
 While (node<>nil) and (node<>node_last) do
 begin
  Assert(parent=node.pParent);
  next:=node.pNext;
  parent.Remove(node);
  Push_tail(node);
  node:=next;
 end;
end;

///

Function TsrCFGParser2.NewInstruction(b_adr,e_adr:TSrcAdr;var FSPI:TSPI):TsrSourceInstruction;
begin
 Result:=FEmit.specialize New<TsrSourceInstruction>;
 Inc(order);
 Result.order:=order;
 Result.b_adr:=b_adr;
 Result.e_adr:=e_adr;
 Result.FSPI :=FSPI;
 Result.used :=True;
end;

Function TsrCFGParser2.NewLabel(Adr:TSrcAdr):TsrSourceLabel;
begin
 Result:=FLabelTree.Find(@Adr);
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrSourceLabel>;
  Inc(order);
  Result.order:=order;
  Result.b_adr:=Adr;
  Result.e_adr:=Adr;
  //
  FLabelTree.Insert(Result);
 end;
 Inc(Result.links_count);
end;

Function TsrCFGParser2.NewGoto(Cond:TsrCondition;Adr:TSrcAdr):TsrSourceGoto;
begin
 Result:=FGotoFree.Pop_tail;
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrSourceGoto>;
 end;
 Inc(order);
 Result.order :=order;
 Result.pCond :=NewCond(Cond);
 Result.pLabel:=NewLabel(Adr);
end;

Function TsrCFGParser2.NewGoto(pCond:TsrStatement;pLabel:TsrSourceLabel):TsrSourceGoto;
begin
 Result:=FGotoFree.Pop_tail;
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrSourceGoto>;
 end;
 Inc(order);
 Result.order :=order;
 Result.pCond :=pCond;
 Result.pLabel:=pLabel;
end;

procedure TsrCFGParser2.FreeGoto(node:TsrSourceGoto);
begin
 FGotoList.Remove(node);
 if (node.pParent<>nil) then
 begin
  node.pParent.Remove(node);
 end;
 FGotoFree.Push_tail(node);
end;

Function TsrCFGParser2.NewBlock(bType:TsrBlockType):TsrSourceBlock;
begin
 Result:=FEmit.specialize New<TsrSourceBlock>;
 Inc(order);
 Result.order:=order;
 Result.bType:=bType;
end;

Function TsrCFGParser2.NewCond(cond:TsrCondition):TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Inc(order);
 Result.order :=order;
 Result.sType :=sCond;
 Result.u.cond:=cond;
end;

Function TsrCFGParser2.NewCopy(pCond:TsrStatement):TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Inc(order);
 Result.order:=order;
 Result.sType:=sCopy;
 Result.pDst :=Result;
 Result.pSrc :=pCond;
 Result.u.id :=VarId;
 Inc(VarId);
end;

Function TsrCFGParser2.NewVar:TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Inc(order);
 Result.order:=order;
 Result.sType:=sVar;
 Result.u.id :=VarId;
 Inc(VarId);
end;

Function TsrCFGParser2.NewStore(pVar,pCond:TsrStatement):TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Inc(order);
 Result.order:=order;
 Result.sType:=sStore;
 Result.pDst :=pVar;
 Result.pSrc :=pCond;
end;

Function TsrCFGParser2.NewBreak:TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Inc(order);
 Result.order:=order;
 Result.sType:=sBreak;
end;

const
 InvertCond:array[TsrCondition] of TsrCondition=(
  cNone,   //cNone,
  cTrue,   //cFalse,
  cFalse,  //cTrue,
  cScc1,   //cScc0,
  cScc0,   //cScc1,
  cVccnz,  //cVccz,
  cVccz,   //cVccnz,
  cExecnz, //cExecz,
  cExecz,  //cExecnz,
  cTidnz,  //cTidz,
  cTidz    //cTidnz
 );

Function TsrCFGParser2.NewNot(pCond:TsrStatement):TsrStatement;
begin
 case pCond.sType of
  sCond:Result:=NewCond(InvertCond[pCond.u.cond]);
  else
   begin
    Result:=FEmit.specialize New<TsrStatement>;
    Inc(order);
    Result.order:=order;
    Result.sType:=sNot;
    Result.pSrc :=pCond;
   end;
 end;
end;

Function TsrCFGParser2.NewOr(pCond1,pCond2:TsrStatement):TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Inc(order);
 Result.order:=order;
 Result.sType:=sOr;
 Result.pSrc :=pCond1;
 Result.pDst :=pCond2;
end;

Function TsrCFGParser2.NewAnd(pCond1,pCond2:TsrStatement):TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Inc(order);
 Result.order:=order;
 Result.sType:=sAnd;
 Result.pSrc :=pCond1;
 Result.pDst :=pCond2;
end;

///

function TSrcAdr.get_code_ptr:PDWORD;
begin
 if (pCode=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=pCode.Body;
 end;
 //
 Result:=Pointer(Result)+Offset;
end;

function TSrcAdr.get_dmem_ptr:PDWORD;
begin
 if (pCode=nil) then
 begin
  Result:=nil
 end else
 begin
  Result:=pCode.DMem;
 end;
 //
 Result:=Pointer(Result)+Offset;
end;

///

function ToAdr(pCode:TsrCodeRegion;Offset:PtrUInt):TSrcAdr; inline;
begin
 Result.pCode :=pCode ;
 Result.Offset:=Offset;
end;

procedure TsrCFGParser2.EmitGoto(instr:TsrSourceInstruction;Adr:TSrcAdr);
var
 node:TsrSourceGoto;
begin
 instr.used:=False;
 //
 Adr.Offset:=Adr.Offset+(Smallint(instr.FSPI.SOPP.SIMM)*SizeOf(DWORD));
 //
 Case instr.FSPI.SOPP.OP of
  S_CBRANCH_SCC0  :node:=NewGoto(cScc0  ,Adr);
  S_CBRANCH_SCC1  :node:=NewGoto(cScc1  ,Adr);
  S_CBRANCH_VCCZ  :node:=NewGoto(cVccz  ,Adr);
  S_CBRANCH_VCCNZ :node:=NewGoto(cVccnz ,Adr);
  S_CBRANCH_EXECZ :node:=NewGoto(cExecz ,Adr);
  S_CBRANCH_EXECNZ:node:=NewGoto(cExecnz,Adr);
  S_BRANCH        :node:=NewGoto(cTrue  ,Adr);
  else
   Assert(false);
 end;
 //
 pCurr.pParent.Push_tail(node);
 pCurr:=node;
 //
 if (Smallint(instr.FSPI.SOPP.SIMM)<0) then
 begin
  //prior to cycles in revers
  FGotoList.Push_head(node);
  //

  {
  if (FGotoList.pMiddle=nil) then
  begin
   FGotoList.pMiddle:=node;
  end;
  }

 end else
 if (node.pCond.u.cond=cTrue) then
 begin
  FGotoList.Push_tail(node);

  {
  //middle unconditional in revers
  if (FGotoList.pMiddle=nil) then
  begin
   FGotoList.pMiddle:=node;
   //
   FGotoList.Push_head(node);
  end else
  begin
   //FGotoList.InsertBefore(FGotoList.pMiddle,node);
   FGotoList.InsertAfter(FGotoList.pMiddle,node);
  end;
  }

 end else
 begin
  FGotoList.Push_tail(node);
 end;
end;

function TsrCFGParser2.Parse:Integer;
var
 FCursor:TShaderParser;
 b_adr,e_adr:TSrcAdr;
 FSPI:TSPI;
 node:TsrSourceInstruction;
begin
 if (pCode=nil) then Exit(4);

 //init first label
 pCurr:=NewLabel(ToAdr(pCode,0));
 pCode.FTop.Push_tail(pCurr);

 FCursor:=Default(TShaderParser);
 FCursor.Body:=pCode.Dmem;

 FSPI:=Default(TSPI);

 repeat
  b_adr:=ToAdr(pCode,FCursor.OFFSET_DW*SizeOf(DWORD));
  //
  Result:=FCursor.Next(FSPI);
  if (Result=-1) then Break;
  //
  e_adr:=ToAdr(pCode,FCursor.OFFSET_DW*SizeOf(DWORD));
  //
  node:=NewInstruction(b_adr,e_adr,FSPI);
  //
  pCurr.pParent.Push_tail(node);
  pCurr:=node;
  //
  FInstructionTree.Insert(node);
  //
  Case FSPI.CMD.EN of
   W_SOP1:
     Case FSPI.SOP1.OP of
      S_SETPC_B64:
       if (pCurr.pParent.bType=btSetpc) then
       begin
        Result:=1;
        Break;
       end;
     end;
   W_SOPP:
     Case FSPI.SOPP.OP of
      S_CBRANCH_SCC0  :EmitGoto(node,e_adr);
      S_CBRANCH_SCC1  :EmitGoto(node,e_adr);
      S_CBRANCH_VCCZ  :EmitGoto(node,e_adr);
      S_CBRANCH_VCCNZ :EmitGoto(node,e_adr);
      S_CBRANCH_EXECZ :EmitGoto(node,e_adr);
      S_CBRANCH_EXECNZ:EmitGoto(node,e_adr);
      S_BRANCH        :EmitGoto(node,e_adr);
      else;
     end;
  end;
  //
  if (Result<>0) then Break;
  //
 until false;
 //
 pCode.Size:=FCursor.OFFSET_DW*SizeOf(DWORD);
end;

function flow_down_next_up(node:TsrSourceNode):TsrSourceNode;
begin
 Result:=node.First; //down
 if (Result=nil) then
 begin
  repeat //up
   Result:=node.pNext;
   node:=node.pParent;
  until (node=nil) or (Result<>nil);
 end;
end;

function flow_next_up(node:TsrSourceNode):TsrSourceNode;
begin
 repeat //up
  Result:=node.pNext;
  node:=node.pParent;
 until (node=nil) or (Result<>nil);
end;

function flow_down_next(node:TsrSourceNode):TsrSourceNode;
begin
 Result:=node.First; //down
 if (Result=nil) then
 begin
  //next
  Result:=node.pNext;
 end;
end;

procedure TsrCFGParser2.EmitLabels;
var
 node:TsrSourceGoto;
 inst:TsrSourceInstruction;
begin
 node:=FGotoList.pHead;
 //
 while (node<>nil) do
 begin
  inst:=FInstructionTree.Find(@node.pLabel.key);
  Assert(inst<>nil);
  //
  if (node.pLabel.pParent=nil) then //if not inserted
  begin
   inst.pParent.InsertBefore(inst,node.pLabel);
  end;
  //
  node:=node.FGoto.pNext;
 end;
end;

function Level(stmt:TsrSourceNode):DWORD; inline;
begin
 Result:=stmt.get_level;
end;

function IsDirectlyRelated(goto_stmt,label_stmt:TsrSourceNode):Boolean;
var
 goto_level,label_level:DWORD;
 min_level,max_level:DWORD;
 min,max:TsrSourceNode;
begin
 goto_level :=Level(goto_stmt);
 label_level:=Level(label_stmt);

 if (label_level < goto_level) then
 begin
  min_level:=label_level;
  max_level:=goto_level;
  min:=label_stmt;
  max:=goto_stmt;
 end else
 begin // goto_level < label_level
  min_level:=goto_level;
  max_level:=label_level;
  min:=goto_stmt;
  max:=label_stmt;
 end;

 while (max_level > min_level) do
 begin
  Dec(max_level);
  max:=max.pParent.pReal;
 end;

 Result:=(min.pParent.pReal=max.pParent.pReal);
end;

function IsIndirectlyRelated(goto_stmt,label_stmt:TsrSourceNode):Boolean;
begin
 Result:=(goto_stmt.pParent.pReal <> label_stmt.pParent.pReal) and
         (not IsDirectlyRelated(goto_stmt, label_stmt));
end;

function AreSiblings(goto_stmt,label_stmt:TsrSourceNode):Boolean; inline;
begin
 Result:=(goto_stmt.pParent.pReal=label_stmt.pParent.pReal);
end;

function SiblingFromNephew(uncle,nephew:TsrSourceNode;up:Boolean):TsrSourceNode;
var
 parent,it,it_up:TsrSourceNode;
begin
 parent:=uncle.pParent.pReal;
 it    :=nephew;
 it_up :=it.pParent.pReal;

 while (it_up <> nil) and (it_up <> parent) do
 begin
  it   :=it_up;
  it_up:=it.pParent.pReal;
 end;

 if up then
 if (it.pParent<>nil) then
 if (it.pParent.bType=btMerg) then
 begin
  it:=it.pParent;
 end;

 Result:=it;
end;

function AreOrdered(left_sibling,right_sibling:TsrSourceNode):Boolean;
var
 it:TsrSourceNode;
begin
 if AreSiblings(left_sibling,right_sibling) then
 begin
  it:=right_sibling;
  while (it<>nil) do
  begin
   if (it=left_sibling) then Exit(False);
   //
   it:=it.pNext;
  end;
  Result:=True;
 end else
 begin
  Result:=False;
 end;
end;

function NeedsLift(goto_stmt,label_stmt:TsrSourceNode):Boolean;
var
 sibling:TsrSourceNode;
begin
 sibling:=SiblingFromNephew(goto_stmt,label_stmt,True);
 Result:=AreOrdered(sibling,goto_stmt);
end;

Function IsUnconditional(cond:TsrStatement):Boolean; inline;
begin
 Result:=(cond.sType=sCond) and (cond.u.cond=cTrue)
end;

Function IsUnreachable(cond:TsrStatement):Boolean; inline;
begin
 Result:=(cond.sType=sCond) and (cond.u.cond=cFalse)
end;

procedure TsrCFGParser2.GotoPass;
var
 node,next:TsrSourceGoto;
begin
 node:=FGotoList.pHead;
 //
 while (node<>nil) do
 begin
  next:=node.FGoto.pNext;
  //
  RemoveGoto(node,True);
  //
  node:=next;
 end;
 //
 node:=FGotoList.pHead;
 //
 while (node<>nil) do
 begin
  next:=node.FGoto.pNext;
  //
  RemoveGoto(node,False);
  //
  node:=next;
 end;
 //
end;

procedure MoveListBefore(List:TsrSourceNodeList;before:TsrSourceNode);
var
 node:TsrSourceNode;
begin
 while (List.pHead<>nil) do
 begin
  node:=List.Pop_head;
  before.pParent.InsertBefore(before,node);
 end;
end;

procedure TsrCFGParser2.InitMovePass;
var
 node,next:TsrSourceNode;
begin
 node:=pCode.FTop.First;

 while (node<>nil) do
 begin
  next:=flow_down_next_up(node);

  if (node.ntype=TsrSourceBlock) then
  begin
   MoveListBefore(TsrSourceBlock(node).FInit,node);
  end;

  node:=next;
 end;
end;

Function NextUsed(node:TsrSourceNode):TsrSourceNode;
begin
 Result:=node.pNext;
 while (Result<>nil) do
 begin
  if (Result.ntype=TsrSourceInstruction) then
  begin
   if not TsrSourceInstruction(Result).used then
   begin
    Result:=Result.pNext;
    Continue;
   end;
  end;
  Break;
 end;
end;

Function NextUsedFlowUp(node:TsrSourceNode):TsrSourceNode;
begin
 Result:=flow_next_up(node);
 while (Result<>nil) do
 begin
  if (Result.ntype=TsrSourceInstruction) then
  begin
   if not TsrSourceInstruction(Result).used then
   begin
    Result:=flow_next_up(Result);
    Continue;
   end;
  end;
  Break;
 end;
end;

Function NextUsedFlowDown(node:TsrSourceNode):TsrSourceNode;
begin
 Result:=flow_down_next(node);
 while (Result<>nil) do
 begin
  if (Result.ntype=TsrSourceInstruction) then
  begin
   if not TsrSourceInstruction(Result).used then
   begin
    Result:=flow_down_next(Result);
    Continue;
   end;
  end;
  Break;
 end;
end;

procedure TsrCFGParser2.RemoveGoto(goto_stmt:TsrSourceGoto;simple_pass:Boolean);
var
 label_stmt:TsrSourceLabel;
 label_level,goto_level:DWORD;
begin
 // Force goto_stmt and label_stmt to be directly related
 label_stmt:=goto_stmt.pLabel;
 if (IsIndirectlyRelated(goto_stmt, label_stmt)) then
 begin
  if simple_pass then Exit;
  // Move goto_stmt out using outward-movement transformation until it becomes
  // directly related to label_stmt
  while (not IsDirectlyRelated(goto_stmt, label_stmt)) do
  begin
   goto_stmt:=MoveOutward(goto_stmt);
  end
 end;
 // Force goto_stmt and label_stmt to be siblings
 if (IsDirectlyRelated(goto_stmt, label_stmt)) then
 begin
  label_level:=Level(label_stmt);
  goto_level :=Level(goto_stmt);
  if (goto_level > label_level) then
  begin
   if simple_pass then Exit;
   // Move goto_stmt out of its level using outward-movement transformations
   while (goto_level > label_level) do
   begin
    goto_stmt:=MoveOutward(goto_stmt);
    //Dec(goto_level);
    goto_level:=Level(goto_stmt);
   end;
  end else
  if (goto_level < label_level) then
  begin
   if simple_pass then Exit;
   //
   if (NeedsLift(goto_stmt, label_stmt)) then
   begin
    // Lift goto_stmt to above stmt containing label_stmt using goto-lifting
    // transformations
    goto_stmt:=Lift(goto_stmt);
   end;
   // Move goto_stmt into label_stmt's level using inward-movement transformation
   while (goto_level < label_level) do
   begin
    goto_stmt:=MoveInward(goto_stmt);
    Inc(goto_level);
   end;
  end;
 end;
 //
 if (not AreSiblings(goto_stmt, label_stmt)) then
 begin
  Assert(false,'Goto is not a sibling with the label');
 end;
 // goto_stmt and label_stmt are guaranteed to be siblings, eliminate
 if (NextUsed(goto_stmt) = label_stmt) then
 begin
  // Simply eliminate the goto if the label is next to it
  FreeGoto(goto_stmt);
 end else
 if (AreOrdered(goto_stmt, label_stmt)) then
 begin
  // Eliminate goto_stmt with a conditional
  EliminateAsConditional(goto_stmt);
 end else
 begin
  // Eliminate goto_stmt with a loop
  EliminateAsLoop(goto_stmt);
 end;
end;

procedure InsertAfter(node,new:TsrSourceNode);
var
 parent:TsrSourceBlock;
begin
 parent:=node.pParent;
 if (parent.bType=btMerg) then
 begin
  node:=parent;
  parent:=node.pParent;
 end;
 parent.InsertAfter(node,new);
end;

procedure InsertBefore(node,new:TsrSourceNode);
var
 parent:TsrSourceBlock;
begin
 parent:=node.pParent;
 if (parent.bType=btMerg) then
 begin
  node:=parent;
  parent:=node.pParent;
 end;
 parent.InsertBefore(node,new);
end;

procedure TsrCFGParser2.EliminateAsConditional(goto_stmt:TsrSourceGoto);
var
 label_stmt:TsrSourceLabel;
 cond      :TsrStatement;
 neg_cond  :TsrStatement;
 if_stmt   :TsrSourceBlock;
 if_merg   :TsrSourceBlock;
begin
 label_stmt:=goto_stmt.pLabel;

 if_merg:=NewBlock(btMerg);
 if_stmt:=NewBlock(btCond);
 if_merg.Push_tail(if_stmt);

 if_stmt.splice(goto_stmt.pNext,label_stmt);

 InsertAfter(goto_stmt,if_merg);

 cond:=goto_stmt.pCond;

 neg_cond:=NewNot(cond);
 if_merg.FInit.Push_tail(neg_cond);

 if_stmt.pCond:=neg_cond;

 //
 FreeGoto(goto_stmt);
end;

procedure TsrCFGParser2.EliminateAsLoop(goto_stmt:TsrSourceGoto);
var
 label_stmt:TsrSourceLabel;
 cond      :TsrStatement;
 loop_stmt :TsrSourceBlock;
begin
 label_stmt:=goto_stmt.pLabel;

 loop_stmt:=NewBlock(btLoop);
 loop_stmt.splice(label_stmt,goto_stmt);

 InsertBefore(goto_stmt,loop_stmt);

 cond:=goto_stmt.pCond;

 loop_stmt.pCond:=cond;

 //
 FreeGoto(goto_stmt);
end;

function FindUpLoop(node:TsrSourceBlock):TsrSourceBlock;
begin
 Result:=nil;
 While (node<>nil) do
 begin
  if (node.bType=btLoop) then Exit(node);
  node:=node.pParent;
 end;
end;

function IsBreakLoop(goto_stmt:TsrSourceGoto):Boolean;
var
 loop:TsrSourceBlock;
 node:TsrSourceNode;
begin
 Result:=False;
 loop:=FindUpLoop(goto_stmt.pParent);
 if (loop<>nil) then
 begin
  node:=goto_stmt.pLabel;
  while (node<>nil) do
  begin
   if (node=loop) then
   begin
    //label inside loop
    Exit(False);
   end;
   //
   node:=node.pParent;
  end;
  //
  Result:=True;
 end;
end;

{
 if (eval)
 {
  break-------\
 }            |
 ...          |
 <------------/
}

function IsBreakElse(goto_stmt:TsrSourceGoto):Boolean;
var
 parent:TsrSourceBlock;
begin
 Result:=False;
 if IsUnconditional(goto_stmt.pCond) then
 if (NextUsed(goto_stmt)=nil) then
 if (goto_stmt.pParent<>nil) then
 if (goto_stmt.pParent.bType=btCond) then
 begin
  parent:=goto_stmt.pParent;
  parent:=parent.pParent;
  if (parent<>nil) then
  if (parent.bType=btMerg) then
  if (NextUsed(parent)<>goto_stmt.pLabel) then
  begin
   Result:=AreOrdered(parent,goto_stmt.pLabel);
  end;
 end;
end;

function IsBreakSwitch(goto_stmt:TsrSourceGoto):Boolean;
var
 label_stmt:TsrSourceLabel;
 parent:TsrSourceBlock;
begin
 Result:=False;

 label_stmt:=goto_stmt.pLabel;

 //if IsUnconditional(goto_stmt.pCond) then
 //if (NextUsed(goto_stmt)=nil) then
 if (goto_stmt.pParent<>nil) then
 if (goto_stmt.pParent.bType=btCond) then
 if (label_stmt.links_count>1) then
 begin
  parent:=goto_stmt.pParent;
  parent:=parent.pParent;

  if (NextUsedFlowUp(parent)<>label_stmt) then
  while (parent<>nil) do
  begin

   if (parent.bType=btLoop) then
   begin
    Exit(False);
   end else
   if AreSiblings(parent,label_stmt) then
   begin
    Exit(True);
   end;

   parent:=parent.pParent;
  end;

 end;
end;

function TsrCFGParser2.MoveOutward(goto_stmt:TsrSourceGoto):TsrSourceGoto;
var
 parent:TsrSourceBlock;
begin
 if (NextUsed(goto_stmt)=goto_stmt.pLabel) then
 begin
  Exit(goto_stmt);
 end;

 if IsBreakElse(goto_stmt) then
 begin
  Exit(MoveOutwardElse(goto_stmt));
 end;

 if IsBreakSwitch(goto_stmt) then
 begin
  Exit(MoveOutwardSwitch(goto_stmt));
 end;

 if IsBreakLoop(goto_stmt) then
 begin
  Exit(MoveOutwardLoop(goto_stmt));
 end;

 parent:=goto_stmt.pParent.pReal;

 case parent.bType of
  btCond:Result:=MoveOutwardIf  (goto_stmt);
  btElse:Result:=MoveOutwardIf  (goto_stmt);
  btLoop:Result:=MoveOutwardLoop(goto_stmt);
  else
    begin
     Writeln(stderr,'Invalid outward movement:',parent.bType);
     Assert(false,'Invalid outward movement');
    end;
 end;

end;

{
 ....
 if (expr)
 {
  stmt_1;
  ....
  if (cond) goto L1;
  ....
  stmt_i;
 }
 ....
 L1:
 stmt_n;
}
//-->
{
 ....
 goto_L1 = false;
 if (expr)
 {
  stmt_1;
  ....
  goto_L1 = cond;
  if (!cond)
  {
   stmt_2;
   ....
   stmt_i;
  }
 }
 if (goto_L1) goto L1;
 ....
 L1:
 stmt_n;
}
//-->
{
 ....
 goto_L1 = false;
 if (expr)
 {
  stmt_1;
  ....
  goto_L1 = cond;
 }
 if (goto_L1) goto L1;
 ....
 L1:
 stmt_n;
}

function TsrCFGParser2.MoveOutwardIf(goto_stmt:TsrSourceGoto):TsrSourceGoto;
var
 label_stmt:TsrSourceLabel;
 parent    :TsrSourceBlock;
 pmerge    :TsrSourceBlock;
 new_var   :TsrStatement;
 set_var   :TsrStatement;
 cond      :TsrStatement;
 neg_cond  :TsrStatement;
 new_goto  :TsrSourceGoto;
 if_stmt   :TsrSourceBlock;
 if_merg   :TsrSourceBlock;
begin
 label_stmt:=goto_stmt.pLabel;

 parent:=goto_stmt.pParent;
 pmerge:=parent.pParent;
 Assert(pmerge<>nil);
 Assert(pmerge.bType=btMerg);

 cond:=goto_stmt.pCond;

 if (NextUsedFlowUp(pmerge)<>label_stmt) then
 begin
  new_var:=NewVar;
 end else
 begin
  //simplification
  new_var:=nil;
 end;

 if (new_var<>nil) then
 begin
  //goto_L1 = false;
  set_var:=NewStore(new_var,NewCond(cFalse));
  pmerge.FInit.Push_tail(set_var);
 end;

 if (NextUsed(goto_stmt)=nil) then
 begin
  //empty body

  if (new_var<>nil) then
  begin
   //goto_L1 = cond;
   set_var:=NewStore(new_var,cond);
   parent.Push_tail(set_var);
  end;

 end else
 begin
  if_merg:=NewBlock(btMerg);
  if_stmt:=NewBlock(btCond);
  if_merg.Push_tail(if_stmt);

  if_stmt.splice(goto_stmt.pNext,nil);

  InsertAfter(goto_stmt,if_merg);

  if (new_var<>nil) then
  begin
   //goto_L1 = cond;
   set_var:=NewStore(new_var,cond);
   if_merg.FInit.Push_tail(set_var);
  end;

  neg_cond:=NewNot(cond);
  if_merg.FInit.Push_tail(neg_cond);

  if_stmt.pCond:=neg_cond;

  //
 end;

 //
 FreeGoto(goto_stmt);
 //

 if (new_var=nil) then
 begin
  if IsUnconditional(Cond) then
  begin
   new_var:=Cond;
  end else
  begin
   new_var:=NewCond(cTrue);
  end;
 end;

 new_goto:=NewGoto(new_var,label_stmt);
 InsertAfter(pmerge,new_goto);

 Result:=new_goto;
end;

{
 ....
 if (expr)
 {
  stmt_1;
  ....
  goto L1;
 }
 stmt_2;
 {
  ....
  L1:
  ....
 }
 stmt_n;
}
//-->
{
 ....
 if (expr)
 {
  stmt_1;
  ....
 } else {
  stmt_2;
  goto L1;
 }

 {
  ....
  L1:
  ....
 }
 stmt_n;
}

function TsrCFGParser2.MoveOutwardElse(goto_stmt:TsrSourceGoto):TsrSourceGoto;
var
 label_stmt:TsrSourceLabel;
 pmerge    :TsrSourceBlock;
 cond      :TsrStatement;
 new_goto  :TsrSourceGoto;
 if_stmt   :TsrSourceBlock;
 if_else   :TsrSourceBlock;
begin
 label_stmt:=goto_stmt.pLabel;

 if_stmt:=goto_stmt.pParent;
 pmerge:=if_stmt.pParent;
 Assert(pmerge<>nil);
 Assert(pmerge.bType=btMerg);

 cond:=goto_stmt.pCond;
 Assert(IsUnconditional(cond));

 if_else:=NewBlock(btElse);
 if_else.pIf  :=if_stmt;
 if_stmt.pElse:=if_else;
 pmerge.InsertAfter(if_stmt,if_else);

 if_else.splice(pmerge.pNext,label_stmt);

 //
 FreeGoto(goto_stmt);
 //

 goto_stmt:=TsrSourceGoto(if_else.Last);

 if (goto_stmt<>nil) then
 if (goto_stmt.ntype=TsrSourceGoto) then
 if IsUnconditional(goto_stmt.pCond) then
 begin
  Exit(goto_stmt);
 end;

 new_goto:=NewGoto(cond,label_stmt);
 if_else.Push_tail(new_goto);

 Result:=new_goto;
end;

{
 ....
 do {
  stmt_1;
  ....
  if (expr) {
   if (cond) goto L1;
  }
  ....
  stmt_i;
 } while (expr)
 ....
 L1:
 stmt_n;
}
//-->
{
 ....
 goto_L1 = false;
 do {
  stmt_1;
  ....
  if (expr) {
   if (cond) {
    goto_L1 = true;
    break;
   }
  }
  ....
  stmt_i;
 } while (expr)
 if (goto_L1) goto L1;
 ....
 L1:
 stmt_n;
}

function TsrCFGParser2.MoveOutwardLoop(goto_stmt:TsrSourceGoto):TsrSourceGoto;
var
 label_stmt :TsrSourceLabel;
 loop_parent:TsrSourceBlock;
 cond       :TsrStatement;
 new_var    :TsrStatement;
 set_var    :TsrStatement;
 new_goto   :TsrSourceGoto;
 if_stmt    :TsrSourceBlock;
 if_merg    :TsrSourceBlock;
begin
 label_stmt:=goto_stmt.pLabel;

 loop_parent:=FindUpLoop(goto_stmt.pParent);

 cond:=goto_stmt.pCond;

 if (NextUsedFlowUp(loop_parent)<>label_stmt) then
 begin
  new_var:=NewVar;

  //goto_L1 = false;
  set_var:=NewStore(new_var,NewCond(cFalse));
  loop_parent.FInit.Push_tail(set_var);
 end else
 begin
  //simplification
  new_var:=nil;
 end;

 //Unconditional
 if IsUnconditional(cond) then
 begin
  if (new_var<>nil) then
  begin
   //goto_L1 = true;
   set_var:=NewStore(new_var,NewCond(cTrue));
   InsertBefore(goto_stmt,set_var);
  end;

  //break;
  InsertBefore(goto_stmt,NewBreak);
 end else
 begin
  //if (cond) {
  if_merg:=NewBlock(btMerg);
  if_stmt:=NewBlock(btCond);
  if_merg.Push_tail(if_stmt);
  InsertBefore(goto_stmt,if_merg);

  if_stmt.pCond:=cond;

  if (new_var<>nil) then
  begin
   //goto_L1 = true;
   set_var:=NewStore(new_var,NewCond(cTrue));
   if_stmt.Push_tail(set_var);
  end;

  //break;
  if_stmt.Push_tail(NewBreak);
 end;

 //
 FreeGoto(goto_stmt);
 //

 if (new_var=nil) then
 begin
  if IsUnconditional(Cond) then
  begin
   new_var:=Cond;
  end else
  begin
   new_var:=NewCond(cTrue);
  end;
 end;

 new_goto:=NewGoto(new_var,label_stmt);
 InsertAfter(loop_parent,new_goto);

 Result:=new_goto;
end;

function SanitizeNoBreaks(node_first,node_last:TsrSourceNode):Boolean;
var
 node,next:TsrSourceNode;
begin
 Result:=True;
 node:=node_first;

 while (node<>nil) and (node<>node_last) do
 begin

  if (node.ntype=TsrStatement) then
  begin
   if (TsrStatement(node).sType=sBreak) then
   begin
    Exit(False);
   end;
  end;

  next:=node.First; //down

  if (next<>nil) then
  if (TsrSourceBlock(node).bType=btLoop) then
  begin
   next:=nil;
  end;

  if (next=nil) then
  begin
   repeat //up
    next:=node.pNext;
    node:=node.pParent;
   until (node=nil) or (next<>nil) or (node=node_last);
  end;

  node:=next;
 end;
end;

{
 if (expr) {
  if (cond) goto L1;
 }
 ...
 L1:
 ...
}

function TsrCFGParser2.MoveOutwardSwitch(goto_stmt:TsrSourceGoto):TsrSourceGoto;
var
 label_stmt :TsrSourceLabel;
 nested_stmt:TsrSourceNode;
 loop_stmt  :TsrSourceBlock;
begin
 label_stmt :=goto_stmt.pLabel;

 nested_stmt:=SiblingFromNephew(label_stmt,goto_stmt,True);

 if not SanitizeNoBreaks(nested_stmt, label_stmt) then
 begin
  Assert(false,'SanitizeNoBreaks:MoveOutwardSwitch');
 end;

 loop_stmt:=NewBlock(btLoop);
 loop_stmt.pCond:=NewCond(cFalse);

 loop_stmt.splice(nested_stmt, label_stmt);

 InsertBefore(label_stmt,loop_stmt);

 Result:=MoveOutwardLoop(goto_stmt);
end;

{ ...
 if (cond) goto L1;
 stmt_1;
 ...
 stmt_i;

 if (expr) {
  stmt_k;
  L1:
  ...
  stmt_n;
 }

}

//--> [Cond]
{
 ...
 goto_L1 = cond;
 if (!cond) {
  stmt_1;
  ...
  stmt_i;
 }
 if (goto_L1 || expr) {
  if (goto_L1) goto L1;
  stmt_k;
  L1:
  ...
  stmt_n;
 }
}

//--> [Else]
{ ...
 goto_L1 = cond;
 if (!cond) {
  stmt_1;
  ...
  stmt_i;
 }
 if (!goto_L1 && expr) {
  ...
  stmt_j;
  ...
 }
 else {
  if (goto_L1) goto L1;
  stmt_k;
  L1:
  ...
  stmt_n;
 }
}

//--> [Loop]
{
 ...
 goto_L1 = cond;
 if (!cond) {
  stmt_1;
  ...
  stmt_i;
 }

 do {
  if (goto_L1) goto L1;
  stmt_j;
  ...
  L1:
  ...
  stmt_n;
 } while (expr)
}

function TsrCFGParser2.MoveInward(goto_stmt:TsrSourceGoto):TsrSourceGoto;
var
 label_stmt :TsrSourceLabel;
 nested_stmt:TsrSourceBlock;
 nested_real:TsrSourceBlock;
 new_copy   :TsrStatement;
 cond       :TsrStatement;
 neg_cond   :TsrStatement;
 new_op     :TsrStatement;
 new_goto   :TsrSourceGoto;
 if_stmt    :TsrSourceBlock;
 if_merg    :TsrSourceBlock;
begin
 label_stmt:=goto_stmt.pLabel;

 nested_stmt:=TsrSourceBlock(SiblingFromNephew(goto_stmt,label_stmt,False));
 Assert(nested_stmt.ntype=TsrSourceBlock);

 nested_real:=nested_stmt;
 if (nested_stmt.pParent<>nil) then
 if (nested_stmt.pParent.bType=btMerg) then
 begin
  nested_real:=nested_stmt.pParent;
 end;

 cond:=goto_stmt.pCond;

 if (NextUsed(goto_stmt)=nested_real) then
 begin
  //empty body

  //goto_L1 = cond;
  if (cond.sType<>sCond) then
  begin
   new_copy:=cond;
  end else
  begin
   new_copy:=NewCopy(cond);
   nested_real.FInit.Push_tail(new_copy);
  end;

 end else
 begin
  if_merg:=NewBlock(btMerg);
  if_stmt:=NewBlock(btCond);
  if_merg.Push_tail(if_stmt);

  if_stmt.splice(goto_stmt.pNext,nested_real);

  InsertAfter(goto_stmt,if_merg);

  //goto_L1 = cond;
  if (cond.sType<>sCond) then
  begin
   new_copy:=cond;
  end else
  begin
   new_copy:=NewCopy(cond);
   if_merg.FInit.Push_tail(new_copy);
  end;

  neg_cond:=NewNot(cond);
  if_merg.FInit.Push_tail(neg_cond);

  if_stmt.pCond:=neg_cond;

  //
 end;

 case TsrSourceBlock(nested_stmt).bType of
  btCond:
    begin
     Assert(nested_stmt.pCond<>nil);
     //load before

     if IsUnreachable(nested_stmt.pCond) then
     begin
      new_op:=new_copy;
     end else
     begin
      //(goto_L1 || expr)
      new_op:=NewOr(new_copy,nested_stmt.pCond);
      //
      nested_real.FInit.Push_tail(new_op);
     end;

     // Update nested if condition
     nested_stmt.pCond:=new_op;
    end;
  btElse:
    begin
     if_stmt:=nested_stmt.pIf;
     Assert(if_stmt<>nil);
     Assert(nested_stmt.pCond<>nil);
     //load before

     if IsUnreachable(nested_stmt.pCond) then
     begin
      new_op:=nested_stmt.pCond;
     end else
     begin
      //!goto_L1
      neg_cond:=NewNot(new_copy);
      nested_real.FInit.Push_tail(neg_cond);
      //(!goto_L1 && expr)
      new_op:=NewAnd(neg_cond,nested_stmt.pCond);
      //
      nested_real.FInit.Push_tail(new_op);
     end;

     // Update nested if condition
     nested_stmt.pCond:=new_op;
    end;
  btLoop:
    begin
     //
    end;
  else
   begin
    Writeln(stderr,'Invalid inward movement:',nested_stmt.bType);
    Assert(false,'Invalid inward movement');
   end;
 end;

 //
 FreeGoto(goto_stmt);
 //

 new_goto:=NewGoto(new_copy,label_stmt);
 nested_stmt.Push_head(new_goto);

 Result:=new_goto;
end;

{
 ....
 stmt_1;
 stmt_2;
 ....
 stmt_lab; /*contains L1*/
 ....
 if (cond) goto L1;
 ....
 stmt_n;
}
//-->
{
 ....
 stmt_1;
 stmt_2;
 ....
 goto_L1 = false;
 do {
  if (goto_L1) goto L1;
  stmt_lab; /*contains L1*/
  ....
  goto_L1 = cond;
 } while (cond);
 ....
 stmt_n;
}

function TsrCFGParser2.Lift(goto_stmt:TsrSourceGoto):TsrSourceGoto;
var
 label_stmt :TsrSourceLabel;
 nested_stmt:TsrSourceNode;
 loop_stmt  :TsrSourceBlock;
 cond       :TsrStatement;
 new_var    :TsrStatement;
 set_var    :TsrStatement;
 new_goto   :TsrSourceGoto;
begin
 label_stmt:=goto_stmt.pLabel;

 nested_stmt:=SiblingFromNephew(goto_stmt,label_stmt,True);

 if not SanitizeNoBreaks(nested_stmt, goto_stmt) then
 begin
  Assert(false,'SanitizeNoBreaks:Lift');
 end;

 loop_stmt:=NewBlock(btLoop);

 loop_stmt.splice(nested_stmt, goto_stmt);

 InsertBefore(goto_stmt,loop_stmt);

 new_var:=NewVar;
 cond   :=goto_stmt.pCond;

 loop_stmt.pCond:=cond;

 // goto_L1 = cond;
 set_var:=NewStore(new_var,cond);
 loop_stmt.Push_tail(set_var);

 //goto_L1 = false;
 set_var:=NewStore(new_var,NewCond(cFalse));
 loop_stmt.FInit.Push_tail(set_var);

 //
 FreeGoto(goto_stmt);
 //

 new_goto:=NewGoto(new_var,label_stmt);
 loop_stmt.Push_head(new_goto);

 Result:=new_goto;
end;

///

type
 TsrExecFlow=(
  efNone,
  efFalse,
  efTrue
 );

function is_cmp_x(i:Integer):Boolean; inline;
begin
 case i of
  V_CMPX_F_F32 ..V_CMPX_T_F32,
  V_CMPX_F_F64 ..V_CMPX_T_F64,
  V_CMPSX_F_F32..V_CMPSX_T_F32,
  V_CMPSX_F_F64..V_CMPSX_T_F64,
  V_CMPX_F_I32 ..V_CMPX_T_I32,
  V_CMPX_F_I64 ..V_CMPX_T_I64,
  V_CMPX_F_U32 ..V_CMPX_T_U32,
  V_CMPX_F_U64 ..V_CMPX_T_U64,

  V_CMPX_CLASS_F32,
  V_CMPX_CLASS_F64:
   Result:=True;

  else
   Result:=False;
 end;
end;

function GetExecFlow(var FSPI:TSPI):TsrExecFlow;
begin
 Case FSPI.CMD.EN of
  W_VOP1  :
    Case FSPI.VOP1.OP of
     V_NOP              :Result:=efNone;
     V_READFIRSTLANE_B32:Result:=efFalse;
     else
                         Result:=efTrue;
    end;
  //-> The result is given in a scalar register, so it needs to be processed separately
  W_VOPC  :Result:=efFalse;
  W_VOP3  :
    Case FSPI.VOP3a.OP of
     //-> The result is given in a scalar register, so it needs to be processed separately
     0..255                 :Result:=efFalse; //VOP3c
     384+V_NOP              :Result:=efNone;
     256+V_READLANE_B32     :Result:=efFalse;
     256+V_WRITELANE_B32    :Result:=efFalse;
     384+V_READFIRSTLANE_B32:Result:=efFalse;
     else
                             Result:=efTrue;
    end;
  W_DS    :
    Case FSPI.DS.OP of
     DS_NOP:Result:=efNone;
     else
            Result:=efTrue;
    end;
  W_MUBUF :Result:=efTrue;
  W_MTBUF :Result:=efTrue;
  W_EXP   :Result:=efFalse; //-> processed separately
  W_MIMG  :Result:=efTrue;
  W_VOP2  :
    Case FSPI.VOP2.OP of
     V_READLANE_B32 :Result:=efFalse;
     V_WRITELANE_B32:Result:=efFalse;
     else
                     Result:=efTrue;
    end;
  W_SOPP  :
    Case FSPI.SOPP.OP of
     S_NOP    :Result:=efNone;
     S_WAITCNT:Result:=efNone;
     else
               Result:=efFalse;
    end;
  else
    Result:=efFalse;
 end;
end;

function GetExecFlow(node:TsrSourceNode):TsrExecFlow;
begin
 if (node.ntype=TsrSourceInstruction) then
 begin
  Result:=GetExecFlow(TsrSourceInstruction(node).FSPI);
 end else
 if (node.ntype=TsrSourceNode) then
 begin
  Result:=efNone;
 end else
 if (node.ntype=TsrSourceLabel) then
 begin
  Result:=efNone;
 end else
 begin
  Result:=efFalse;
 end;
end;

procedure TsrCFGParser2.ExecPass;
var
 node   :TsrSourceNode;
 next   :TsrSourceNode;
 parent :TsrSourceNode;
 exec   :TsrSourceBlock;
 if_stmt:TsrSourceBlock;
 if_merg:TsrSourceBlock;
begin
 node:=pCode.FTop.First;
 exec:=nil;

 while (node<>nil) do
 begin
  parent:=node.pParent;
  next:=flow_down_next_up(node);

  case GetExecFlow(node) of
   efNone:
    begin
     if (exec<>nil) then
     begin
      exec.splice(node,node.pNext);
     end;
    end;
   efFalse:
    begin
     exec:=nil;
    end;
   efTrue:
    begin
     if (exec=nil) then
     begin
      if_merg:=NewBlock(btMerg);
      if_stmt:=NewBlock(btCond);
      if_stmt.pCond:=NewCond(cTidnz);
      if_merg.Push_tail(if_stmt);
      InsertBefore(node,if_merg);
      exec:=if_stmt;
     end;
     exec.splice(node,node.pNext);
    end;
  end;

  if (next<>nil) then
  if (parent<>next.pParent) then
  begin
   exec:=nil;
  end;

  node:=next;
 end;

end;

///

function Vertical(node:TsrSourceNode):RawByteString;
var
 i:Integer;
 tmp:TsrSourceBlock;
begin
 i:=node.get_level;
 //
 Result:='';
 SetLength(Result,i);

 tmp:=node.pParent;
 While (tmp<>nil) do
 begin
  if isReal(tmp.bType) then
  begin
   if (tmp.bType=btLoop) then
   begin
    Result[i]:='!';
   end else
   begin
    Result[i]:='|';
   end;
   Dec(i);
  end;
  tmp:=tmp.pParent;
 end;

 //
 if (i<>0) and (node.ntype=TsrSourceBlock) then
 begin
  Result[i]:='+';
 end;
end;

function get_str_prefix(count:Byte;node:TsrSourceNode):RawByteString;
begin
 if (node.ntype=TsrSourceInstruction) then
 begin
  Result:=' '+HexStr(TsrSourceInstruction(node).b_adr.Offset,count)+Vertical(node);
 end else
 if (node.ntype=TsrSourceLabel) then
 begin
  Result:=' '+Space(count)+Space(node.get_level);
 end else
 begin
  Result:=' '+Space(count)+Vertical(node);
 end;
end;

function get_str_label(count:Byte;node:TsrSourceLabel):RawByteString;
begin
 Result:='_label_'+HexStr(node.b_adr.Offset,count);
end;

const
 CondStr:array[TsrCondition] of RawByteString=(
  'None',
  'False',
  'True',
  'Scc0',
  'Scc1',
  'Vccz',
  'Vccnz',
  'Execz',
  'Execnz',
  'Tidz',
  'Tidnz'
 );

function GetCondStr(node:TsrStatement):RawByteString;
begin
 Result:='';
 while (node<>nil) do
 begin
  case node.sType of
   sCond :begin Result:=Result+CondStr[node.u.cond]; Break; end;
   sCopy :begin Result:=Result+'C'+IntToStr(node.u.id); Break; end;
   sVar  :begin Result:=Result+'U'+IntToStr(node.u.id); Break; end;
   //sStore:;
   //sBreak:;
   sNot  :begin Result:='!'+Result; node:=node.pSrc; end;
   sOr   :begin Result:=Result+'('+GetCondStr(node.pSrc)+' || '+GetCondStr(node.pDst)+')'; Break; end;
   sAnd  :begin Result:=Result+'('+GetCondStr(node.pSrc)+' && '+GetCondStr(node.pDst)+')'; Break; end;
   else   Break;
  end;
 end;
end;

function GetStatmentStr(node:TsrStatement):RawByteString;
begin
 Result:='';
 case node.sType of
  //sCond :'
  sCopy :Result:=GetCondStr(node.pDst)+' = '+GetCondStr(node.pSrc)+';';
  //sVar  :'
  sStore:Result:=GetCondStr(node.pDst)+' = '+GetCondStr(node.pSrc)+';';
  sBreak:Result:='break;';
  //sNot  :;
  //sOr   :;
  //sAnd  :;
  else;
 end;
end;

procedure PrintStatmentList(const prefix:RawByteString;List:TsrSourceNodeList);
var
 node:TsrStatement;
 s:RawByteString;
begin
 node:=TsrStatement(List.pHead);
 while (node<>nil) do
 begin
  case node.sType of
   sCond:; //skip
   sNot :; //skip
   sOr  :; //skip
   sAnd :; //skip
   else
    begin
     s:=GetStatmentStr(node);
     if (s<>'') then
     begin
      Writeln(prefix+s);
     end;
    end;
  end;
  //
  node:=TsrStatement(node.pNext);
 end;
end;

function get_str_goto(count:Byte;node:TsrSourceGoto):RawByteString;
begin
 Result:='if ('+GetCondStr(node.pCond)+') goto '+get_str_label(count,node.pLabel)+'; //#'+IntToStr(node.order);
end;

function get_down_str(node:TsrSourceBlock):RawByteString;
begin
 Result:='';
 case node.bType of
  btCond:Result:='if ('+GetCondStr(node.pCond)+') { //#'+IntToStr(node.order);
  btLoop:Result:='do { //#'+IntToStr(node.order);
  else;
 end;
end;

function get_up_str(node:TsrSourceBlock):RawByteString;
begin
 Result:='';
 case node.bType of
  btCond:begin
          if (node.pElse=nil) then
          begin
           Result:='}; //if #'+IntToStr(node.order);;
          end else
          begin
           Result:='} else { //#'+IntToStr(node.order);
          end;
         end;
  btElse:Result:='}; //else #'+IntToStr(node.order);
  btLoop:begin
          Result:='} while ('+GetCondStr(node.pCond)+'); //#'+IntToStr(node.order);
         end;
  else;
 end;
end;

procedure TsrCFGParser2.Print;
var
 count:Byte;
 prefix,s:RawByteString;
 node,next:TsrSourceNode;

begin
 count:=BsrQWord(pCode.Size);
 if (count=$FF) then count:=0;
 count:=(count+4) div 4;

 node:=pCode.FTop.First;

 while (node<>nil) do
 begin

  prefix:=get_str_prefix(count,node);

  if (node.ntype=TsrSourceInstruction) then
  begin
   Writeln(prefix+get_str_spi(TsrSourceInstruction(node).FSPI));
  end else
  if (node.ntype=TsrSourceLabel) then
  begin
   Writeln(prefix+get_str_label(count,TsrSourceLabel(node))+': //#'+IntToStr(node.order));
  end else
  if (node.ntype=TsrSourceGoto) then
  begin
   Writeln(prefix+get_str_goto(count,TsrSourceGoto(node)));
  end else
  if (node.ntype=TsrSourceBlock) then
  begin
   //
   PrintStatmentList(prefix,TsrSourceBlock(node).FInit);
   //down
   s:=get_down_str(TsrSourceBlock(node));
   if (s<>'') then
   begin
    Writeln(prefix+s);
   end;
   //up
   if (node.First=nil) then
   begin
    s:=get_up_str(TsrSourceBlock(node));
    if (s<>'') then
    begin
     Writeln(prefix+s);
    end;
   end;
   //
  end else
  if (node.ntype=TsrStatement) then
  begin
   s:=GetStatmentStr(TsrStatement(node));
   if (s<>'') then
   begin
    Writeln(prefix+s);
   end;
  end else
  begin
   //Writeln(prefix+node.ntype.ClassName);
  end;

  next:=node.First; //down
  if (next=nil) then
  begin
   repeat //up
    next:=node.pNext;
    node:=node.pParent;
    //
    if (node<>nil) and (next=nil) then
    begin
     s:=get_up_str(TsrSourceBlock(node));
     if (s<>'') then
     begin
      prefix:=get_str_prefix(count,node);
      Writeln(prefix+s);
     end;
    end else
    begin
     Break;
    end;
    //
   until false;
  end;

  node:=next;
 end;

end;

///

function parse_code_cfg2(var pCode:TsrCodeRegion;bType:TsrBlockType;Body,Dmem:Pointer;FEmit:TCustomEmit):Integer;
var
 parser:TsrCFGParser2;
begin
 pCode:=FEmit.specialize New<TsrCodeRegion>;
 //
 parser:=Default(TsrCFGParser2);
 parser.FEmit:=FEmit;
 parser.pCode:=pCode;
 //
 pCode.FEmit:=FEmit;
 pCode.Body :=Body;
 pCode.Dmem :=Dmem;
 pCode.FTop :=parser.NewBlock(bType);

 Result:=parser.Parse;
 if (Result>1) then Exit;

 parser.EmitLabels;
 parser.GotoPass;
 parser.InitMovePass;
 parser.ExecPass;

 if PsrConfig(pCode.FEmit.GetConfig)^.PrintCfg then
 begin
  parser.Print;
 end;

 Result:=0;
end;

//

function TsrCodeRegion.FindByPtr(base:Pointer):TsrSourceNode;
var
 node:TsrSourceNode;
begin
 Result:=nil;
 node:=FTop.First;

 while (node<>nil) do
 begin

  if node.InheritsFrom(TsrSourceAdr) then
  with TsrSourceAdr(node) do
  begin
   if (b_adr.get_code_ptr=base) then
   begin
    Exit(node);
   end else
   if (e_adr.get_code_ptr=base) then
   begin
    Exit(node);
   end;
  end;

  node:=flow_down_next_up(node);
 end;

end;

///

end.

