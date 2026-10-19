unit srFlow;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 ps4_pssl,
 spirv,
 srNode,
 srCFGParser,
 srCFGCursor,
 srPrivate,
 srType,
 srConst,
 srReg,
 srOp,
 srOpUtils,
 srRefId,
 srConfig,
 emit_op;

type
 TConvertResult=record
  pNode:TsrNode;
  pLine:TspirvOp;
 end;

 TEmitFlow=class(TEmitOp)
  //
  Procedure InitFlow;
  procedure mark_end_of(mark:TsrVolMark);
  Procedure PushBlockOp(pLine:TspirvOp;pChild:TsrOpBlock;iCursor:TsrCursor);
  function  PopBlockOp:Boolean;
  function  ConvertCond(cond:TsrCondition;pLine:TspirvOp):TConvertResult;
  function  ConvertStatment(node:TsrStatement;pLine:TspirvOp):TConvertResult;
  function  LoadStatment(C:TConvertResult):TConvertResult;
  procedure emit_break(pCurr:TsrOpBlock);
  procedure EmitStatment(node:TsrStatement);
  function  NewMerge(pLine:TspirvOp;iCursor:TsrCursor):TsrOpBlock;
  function  NewIf   (pOpMerge:TsrOpBlock;iCursor:TsrCursor;src:TsrRegNode;ForceElse:Boolean):TsrOpBlock;
  function  NewElse (pOpMerge:TsrOpBlock;iCursor:TsrCursor):TsrOpBlock;
  function  NewLoop (iCursor:TsrCursor):TsrOpBlock;
  function  BlockBeg:Boolean;
  function  BlockEnd:Boolean;
  //
  function  get_code_ptr:Pointer;
  procedure set_code_ptr(base:Pointer;bType:TsrBlockType);
  function  fetch_cursor_ptr(base:Pointer;bType:TsrBlockType):TsrCursor;
  //function  IsFinalize:Boolean;
  //
  procedure Finalize;
  //
  function  ParseStage(base:Pointer):Integer;
 end;  

implementation

Procedure TEmitFlow.InitFlow;
begin
 CodeHeap.Init(Self);
 //
 InitBlock:=AllocBlockOp;
 InitBlock.SetInfo(btOther);
 PushBlockOp(line,InitBlock,Default(TsrCursor));
 Main.PopBlock;
end;

procedure TEmitFlow.mark_end_of(mark:TsrVolMark);
var
 node:TsrOpBlock;
begin
 node:=Main.pBlock;
 While (node<>nil) do
 begin

  if (node.FVolMark<>vmNone) then
  begin
   //already marked, exit
   Exit;
  end;

  node.FVolMark:=mark;

  //exit if real block
  if IsReal(node.bType) then Exit;

  node:=node.Parent;
 end;
end;

function FindPredecessor(pLine:TspirvOp):TspirvOp;
var
 node :TspirvOp;
 block:TsrOpBlock;
begin
 repeat

  node:=pLine;
  repeat
   if node.IsType(TsrOpBlock) then
   begin
    block:=node;
    if (block.Labels.pMrgOp<>nil) then
    begin
     Exit(block.Labels.pMrgOp);
    end;
   end;

   node:=node.Prev;
  until (node=nil);

  repeat
   block:=pLine.Parent;

   if (block.Labels.pRepOp<>nil) then
   begin
    Exit(block.Labels.pRepOp);
   end else
   if (block.Labels.pBegOp<>nil) then
   begin
    Exit(block.Labels.pBegOp);
   end;

   pLine:=block;
  until (pLine.Prev<>nil);
  pLine:=pLine.Prev;

 until false;
end;

procedure SetupPredecessor(pChild:TsrOpBlock);

 procedure SetupLabel(pLine:TspirvOp);
 begin
  if (pLine<>nil) then
  if (TsrOpLabel(pLine.pDst).pred=nil) then
  begin
   TsrOpLabel(pLine.pDst).pred:=pChild.vctx.pPrdLabel;
  end;
 end;

begin
 pChild.Labels.pPrdOp:=FindPredecessor(pChild.Prev);

 pChild.vctx.pPrdLabel:=pChild.Labels.pPrdOp.pDst;

 if (pChild.Labels.pPrdOp<>nil) then
 begin
  SetupLabel(pChild.Labels.pBegOp);
  SetupLabel(pChild.Labels.pMrgOp);
  SetupLabel(pChild.Labels.pRepOp);
 end;
end;

Procedure TEmitFlow.PushBlockOp(pLine:TspirvOp;pChild:TsrOpBlock;iCursor:TsrCursor);
var
 node,last:TspirvOp;
begin
 pChild.FCursor:=iCursor; //prev
 //pChild.FLBlock:=pLBlock;

 pChild.Labels.pPrdOp:=FindPredecessor(pLine);

 pLine:=InsSpirvOp(pLine,pChild);

 Main.PushBlock(pChild);

 SetupPredecessor(pChild);

 node:=pChild.First;
 last:=pChild.Next;

 while (node<>nil) and (node<>last) do
 begin

  if node.IsType(ntOpBlock) then
  begin
   SetupPredecessor(node);
  end;

  node:=flow_down_next_up(node);
 end;

 {
 if (pLBlock<>nil) then
 begin
  Cursor.pBlock:=pLBlock; //push
 end;
 }
end;

Function IsUnconditional(cond:TsrStatement):Boolean; inline;
begin
 Result:=(cond=nil) or ((cond.sType=sCond) and (cond.u.cond=cTrue));
end;

Function IsUnreachable(cond:TsrStatement):Boolean; inline;
begin
 Result:=(cond.sType=sCond) and (cond.u.cond=cFalse);
end;

function TEmitFlow.PopBlockOp:Boolean;
var
 pOpBlock:TsrOpBlock;
 pBegOp,pEndOp,pMrgOp:TspirvOp;

 procedure pop_free(pOpBlock:TsrOpBlock);
 begin
  RegsStory.FreeSnapshot(pOpBlock.Regs.orig);
  RegsStory.FreeSnapshot(pOpBlock.Regs.prev);
  RegsStory.FreeSnapshot(pOpBlock.Regs.next);
  //
  pOpBlock.Regs.orig:=nil;
  pOpBlock.Regs.prev:=nil;
  pOpBlock.Regs.next:=nil;
  //
  pOpBlock.vctx.FBreaks.Clear;
 end;

 procedure pop_merge(pOpBlock:TsrOpBlock);
 begin
  //
 end;

 procedure pop_merge_after(pOpBlock:TsrOpBlock);
 begin
  pop_free(pOpBlock.pIf);
 end;

 procedure pop_cond(pOpBlock:TsrOpBlock);
 begin
  //
 end;

 procedure pop_cond_after(pOpBlock:TsrOpBlock);
 begin
  if (pOpBlock.pElse<>nil) then //have else
  begin
   //save current to prev
   RegsStory.FillSnapshot(pOpBlock.Regs.prev);
   //restore original by start "else"
   PrivateList.build_volatile_reset(pOpBlock.Regs.orig);
  end else
  begin
   //save current to prev
   RegsStory.FillSnapshot(pOpBlock.Regs.prev);
   //
   if (pOpBlock.FVolMark<>vmNone) then
   begin
    //restore original if inside endpgm/break/continue
    PrivateList.build_volatile_reset(pOpBlock.Regs.orig);
   end else
   begin

    pOpBlock.vctx.pPrvLabel:=nil;
    pOpBlock.vctx.pCurLabel:=FindPredecessor(pOpBlock.pBody.line).pDst;

    //calc next volatile state
    PrivateList.build_volatile_endif(pOpBlock.vctx,pOpBlock.Regs.orig,pOpBlock.Regs.prev,pOpBlock.Regs.next);
    //set next volatile state
    PrivateList.build_volatile_reset(pOpBlock.Regs.next);
   end;
  end;
 end;

 procedure pop_else(pOpBlock:TsrOpBlock);
 begin
  //
 end;

 procedure pop_else_after(pOpBlock:TsrOpBlock);
 var
  pIf:TsrOpBlock;
 begin
  pIf:=pOpBlock.pIf;
  if (pOpBlock.FVolMark<>vmNone) and  //pElse
     (pIf.FVolMark<>vmNone) then      //pIf
  begin
   //restore original if both blocks inside endpgm/break/continue
   PrivateList.build_volatile_reset(pOpBlock.Regs.orig);
   //mark up
   if (pOpBlock.FVolMark=pIf.FVolMark) then
   begin
    mark_end_of(pOpBlock.FVolMark);
   end else
   begin
    mark_end_of(vmMixed);
   end;
  end else
  begin
   pOpBlock.vctx.pPrvLabel:=FindPredecessor(pIf.pBody.line).pDst;
   pOpBlock.vctx.pCurLabel:=FindPredecessor(pOpBlock.pBody.line).pDst;

   //calc next volatile state
   PrivateList.build_volatile_endif(pOpBlock.vctx,pOpBlock.Regs.orig,pOpBlock.Regs.prev,pOpBlock.Regs.next);
   //set next volatile state
   PrivateList.build_volatile_reset(pOpBlock.Regs.next);
  end;
 end;

 procedure pop_loop(pOpBlock:TsrOpBlock);
 var
  pLine:TspirvOp;
  parent:TsrSourceBlock;
  src:TsrRegNode;
 begin
  //add OpLoopMerge continue

  Assert(pBegOp<>nil);
  Assert(pEndOp<>nil);
  Assert(pMrgOp<>nil);

  parent:=pOpBlock.FCursor.AsBlock;
  Assert(parent<>nil);

  pLine:=pOpBlock.pBody.line; //before close

  if IsUnconditional(parent.pCond) then
  begin
   //not post conditions
   OpBranch(pOpBlock.pPCond.line,pBegOp); //continue
  end else
  begin
   //have post conditions
   src:=LoadStatment(ConvertStatment(parent.pCond,pLine)).pNode;
   Assert(src<>nil);
   OpBranchCond(pOpBlock.pPCond.line,pBegOp,pMrgOp,src); //True|False
  end;

 end;

 procedure pop_loop_after(pOpBlock:TsrOpBlock);
 var
  parent:TsrSourceBlock;
 begin

  pOpBlock.vctx.pAftOp:=pEndOp;

  if (pOpBlock.FVolMark<>vmNone) then
  begin
   //set next volatile state
   PrivateList.build_volatile_reset(pOpBlock.Regs.next);
  end else
  begin
   parent:=pOpBlock.FCursor.AsBlock;

   if IsUnconditional(parent.pCond) then
   begin
    //continue
    PrivateList.build_volatile_conti(pOpBlock.vctx,pOpBlock.Regs.orig,pOpBlock.Regs.prev,pOpBlock.Regs.next);
   end else
   if IsUnreachable(parent.pCond) then
   begin
    //break
    PrivateList.build_volatile_break(pOpBlock.vctx,pOpBlock.Regs.orig,pOpBlock.Regs.prev,pOpBlock.Regs.next);
   end else
   begin
    //break
    PrivateList.build_volatile_break(pOpBlock.vctx,pOpBlock.Regs.orig,pOpBlock.Regs.prev,pOpBlock.Regs.next);
    //continue
    PrivateList.build_volatile_conti(pOpBlock.vctx,pOpBlock.Regs.orig,pOpBlock.Regs.prev,pOpBlock.Regs.next);
   end;

   //break
   PrivateList.build_volatile_bloop(pOpBlock.vctx,pOpBlock.Regs.orig,pOpBlock.Regs.prev,pOpBlock.Regs.next);

   //set next volatile state
   PrivateList.build_volatile_reset(pOpBlock.Regs.next);
  end;

  pop_free(pOpBlock);
 end;

 procedure pop_other(pOpBlock:TsrOpBlock);
 begin
  //pMrgOp???
  if (pEndOp<>nil) then
  begin
   OpBranch  (line,pEndOp);
   AddSpirvOp(line,pEndOp);
  end;
 end;

begin
 Result:=False;
 if (Main=nil) then Exit;

 pOpBlock:=Main.pBlock;
 if (pOpBlock=nil) then Exit;

 pBegOp:=pOpBlock.Labels.pBegOp;
 pEndOp:=pOpBlock.Labels.pEndOp;
 pMrgOp:=pOpBlock.Labels.pMrgOp;

 Case pOpBlock.bType of
  btMerg:
   begin
    pop_merge(pOpBlock);
   end;
  btCond:
   begin
    pop_cond(pOpBlock);
   end;
  btElse:
   begin
    pop_else(pOpBlock);
   end;
  btLoop:
   begin
    pop_loop(pOpBlock);
   end;
  else
    pop_other(pOpBlock);
 end;

 //restore

 Case pOpBlock.bType of
  btInline:
   begin
    Assert(pOpBlock.FCursor.pCode<>nil);
    Assert(pOpBlock.FCursor.pNode<>nil);
    Cursor:=pOpBlock.FCursor;
   end;
  btOther:
   begin
    if (pOpBlock.FCursor.pCode<>nil) then
    if (pOpBlock.FCursor.pNode<>nil) then
    begin
     Cursor:=pOpBlock.FCursor;
    end;
   end;
  else
   begin
    if (pOpBlock.FCursor.pCode<>nil) then
    if (pOpBlock.FCursor.pNode<>nil) then
    begin
     Cursor.PopBlock;
    end;
   end;
 end;

 Result:=Main.PopBlock;

 Case pOpBlock.bType of
  btMerg:
   begin
    pop_merge_after(pOpBlock);
   end;
  btCond:
   begin
    pop_cond_after(pOpBlock);
   end;
  btElse:
   begin
    pop_else_after(pOpBlock);
   end;
  btLoop:
   begin
    pop_loop_after(pOpBlock);
   end;
  else
   begin
   end;
 end;

end;

function Base64(b:Byte):Char;
begin
 case (b and 63) of
   0..25:Result:=Char(b+Byte('A')-0);
  26..51:Result:=Char(b+Byte('a')-26);
  52..61:Result:=Char(b+Byte('0')-52);
      62:Result:='+';
      63:Result:='-';
 end;
end;


function TEmitFlow.ConvertCond(cond:TsrCondition;pLine:TspirvOp):TConvertResult;
begin
 case cond of
  cFalse :Result.pNode:=NewImm_b(False,pLine);
  cTrue  :Result.pNode:=NewImm_b(True ,pLine);
  cScc0  :Result.pNode:=fetch_scc;                 //It means that (scc == 0)
  cScc1  :Result.pNode:=fetch_scc;                 //It means that (scc == 1)
  cVccz  :Result.pNode:=fetch_vccnz     (@pLine);  //It means that (vcc0  == 0) && (vcc1  == 0)
  cVccnz :Result.pNode:=fetch_vccnz     (@pLine);  //It means that (vcc0  != 0) || (vcc1  != 0)
  cExecz :Result.pNode:=fetch_execnz    (@pLine);  //It means that (exec0 == 0) && (exec1 == 0)
  cExecnz:Result.pNode:=fetch_execnz    (@pLine);  //It means that (exec0 != 0) || (exec1 != 0)
  cTidz  :Result.pNode:=fetch_execnz_tid(@pLine);  //It means that (exec[thread_id:] == 0)
  cTidnz :Result.pNode:=fetch_execnz_tid(@pLine);  //It means that (exec[thread_id:] != 0)
  else
   Assert(false,'ConvertCond');
 end;
 //Since the main condition is this condition equal to zero, then we need to invert it again!
 case cond of
  cScc0,
  cVccz,
  cExecz,
  cTidz:
   begin
    //invert
    if TsrRegNode(Result.pNode).is_const then
    begin
     //early optimization
     Result.pNode:=NewImm_b(not TsrRegNode(Result.pNode).AsConst.AsBool,pLine);
    end else
    begin
     Result.pNode:=OpLogicalNotTo(Result.pNode,@pLine);
    end;
    //
   end;
  else;
 end;
 //
 Result.pLine:=pLine;
end;

function TEmitFlow.ConvertStatment(node:TsrStatement;pLine:TspirvOp):TConvertResult;
begin
 Result.pNode:=nil;
 Result.pLine:=pLine;
 //
 case node.sType of
  sCond :begin
          Result:=ConvertCond(node.u.cond,pLine);
         end;
  sCopy :begin
          Result.pNode:=TsrNode(node.pCache);
          Assert(Result.pNode<>nil);
         end;
  sVar  :begin
          if (node.pCache=nil) then
          begin
           node.pCache:=TsrNode(RegsStory.alloc_cond);
           Assert(node.pCache<>nil,'alloc_cond');
          end;

          Result.pNode:=PsrRegSlot(node.pCache)^.current;

          if (Result.pNode=nil) then
          begin
           Result.pNode:=NewImm_b(False,pLine);
           PsrRegSlot(node.pCache)^.current:=Result.pNode;
          end;
         end;
  sNot  :begin
          Result.pNode:=TsrNode(node.pCache);
          Assert(Result.pNode<>nil);
         end;
  sOr   :begin
          Result.pNode:=TsrNode(node.pCache);
          Assert(Result.pNode<>nil);
         end;
  sAnd  :begin
          Result.pNode:=TsrNode(node.pCache);
          Assert(Result.pNode<>nil);
         end;
  else
   Assert(false,'ConvertStatment');
 end;
end;

function TEmitFlow.LoadStatment(C:TConvertResult):TConvertResult;
begin
 Result:=C;
end;

procedure TEmitFlow.emit_break(pCurr:TsrOpBlock);
var
 pMrgOp  :TspirvOp;
 pOpBreak:TspirvOp;
 pOpLabel:TspirvOp;

 parent:TsrSourceBlock;

 pLoop:TsrOpBlock;
 pIf  :TsrOpBlock;

 bnew:Boolean;

 node:TDependenceNode;
begin
 pLoop:=Main.pBlock.FindUpLoop;
 Assert(pLoop<>nil,'Break to Unknow');

 pMrgOp:=pLoop.Labels.pMrgOp;
 Assert(pMrgOp<>nil);

 Assert(pLoop.FCursor.pNode<>nil);
 parent:=pLoop.FCursor.AsBlock;
 Assert(parent<>nil);

 bnew:=true;
 //if pCurr.IsEndOf(Cursor.Adr) then //is last
 begin
  if IsReal(pCurr.bType) then
  begin
   bnew:=false;
  end;
 end;

 pLoop.vctx.pAftOp:=FindPredecessor(line);

 //calc volatile
 PrivateList.build_volatile_break(pLoop.vctx,pLoop.Regs.orig,pLoop.Regs.prev,pLoop.Regs.next);

 //mark hints
 mark_end_of(vmBreak);

 pOpBreak:=OpBranch(pCurr.line,pMrgOp);
 if bnew then
 begin
  pOpLabel:=NewLabelOp(True);
  AddSpirvOp(pCurr.line,pOpLabel);

  pIf:=pCurr;
  while (not IsReal(pIf.bType)) do
  begin
   pIf:=pIf.Parent;
  end;

  pMrgOp:=pIf.Labels.pMrgOp;

  //push to list
  TsrOpLabel(pOpLabel.pDst).pred:=TsrOpLabel(pMrgOp.pDst).pred;
  TsrOpLabel(pMrgOp.pDst).pred:=pOpLabel.pDst;
 end;

 pLoop.vctx.FBreaks.Push_tail(pOpBreak);
end;

procedure TEmitFlow.EmitStatment(node:TsrStatement);
Var
 R:TsrRegNode;
 D:TsrRegNode;
 C:TConvertResult;
begin
 case node.sType of
  sCond:; //skip
  sCopy:
   begin
    C:=ConvertStatment(node.pSrc,line);
    C:=LoadStatment(C);
    R:=C.pNode.specialize AsType<TsrRegNode>;
    Assert(R<>nil);

    node.pCache:=R;
   end;
  sStore:
   begin
    C:=ConvertStatment(node.pSrc,line);
    C:=LoadStatment(C);
    D:=C.pNode.specialize AsType<TsrRegNode>;
    Assert(D<>nil);

    if (node.pDst.pCache=nil) then
    begin
     node.pDst.pCache:=TsrNode(RegsStory.alloc_cond);
     Assert(node.pDst.pCache<>nil,'alloc_cond');
    end;

    //Writeln('sStore to:',node.pDst.u.id,' from ',node.pSrc.u.id);

    PsrRegSlot(node.pDst.pCache)^.current:=D;
   end;
  sBreak:
   begin
    emit_break(Main.pBlock);
   end;
  sNot:
   begin
    C:=ConvertStatment(node.pSrc,line);
    C:=LoadStatment(C);
    R:=C.pNode.specialize AsType<TsrRegNode>;
    Assert(R<>nil);

    R:=OpLogicalNotTo(R);

    node.pCache:=R;
   end;
  sOr:
   begin
    C:=ConvertStatment(node.pSrc,line);
    C:=LoadStatment(C);
    R:=C.pNode.specialize AsType<TsrRegNode>;
    Assert(R<>nil);

    C:=ConvertStatment(node.pDst,line);
    C:=LoadStatment(C);
    D:=C.pNode.specialize AsType<TsrRegNode>;
    Assert(D<>nil);

    R:=OpOrTo(R,D);

    node.pCache:=R;
   end;
  sAnd:
   begin
    C:=ConvertStatment(node.pSrc,line);
    C:=LoadStatment(C);
    R:=C.pNode.specialize AsType<TsrRegNode>;
    Assert(R<>nil);

    C:=ConvertStatment(node.pDst,line);
    C:=LoadStatment(C);
    D:=C.pNode.specialize AsType<TsrRegNode>;
    Assert(D<>nil);

    R:=OpAndTo(R,D);

    node.pCache:=R;
   end;
  else
   Assert(false,'EmitStatment');
 end;
end;

function TEmitFlow.NewMerge(pLine:TspirvOp;iCursor:TsrCursor):TsrOpBlock;
var
 pMrgOp:TspirvOp;
 pNop  :TspirvOp;
begin
 pMrgOp:=NewLabelOp(False); //merge

 pNop:=nil;
 if pLine.IsType(ntOp) then
 if (pLine.OpId=Op.OpNop) then
 begin
  pNop:=pLine;
  pNop.mark([soNotUsed,soPost]);
 end;

 if (pNop=nil) then
 begin
  pNop:=AddSpirvOp(pLine,Op.OpNop);
  pNop.mark([soNotUsed,soPost]);
  //
  pLine:=pNop;
 end;

 Result:=AllocBlockOp;
 Result.SetInfo(btMerg);
 Result.SetLabels(nil,nil,pMrgOp);

 Result.vctx.pMrgOp:=pMrgOp;

 //save nop before
 Result.vctx.Befor:=pNop;

 //add by push point
 PushBlockOp(pLine,Result,iCursor);

 //Deferred instruction
 //OpCondMerge(line,pMrgOp);
end;

function _IsConstTrue(pReg:TsrRegNode):Boolean;
var
 pConst:TsrConst;
begin
 Result:=False;
 if (pReg=nil) then Exit;
 pConst:=pReg.AsConst;
 if (pConst=nil) then Exit;
 Result:=pConst.AsBool;
end;

function TEmitFlow.NewIf(pOpMerge:TsrOpBlock;iCursor:TsrCursor;src:TsrRegNode;ForceElse:Boolean):TsrOpBlock;
var
 orig:TsrRegsSnapshot;
 pLBlock:TsrSourceBlock;
 pLElse:TsrSourceBlock;
 pBegOp,pEndOp,pMrgOp,pBefor:TspirvOp;
 pLine:TspirvOp;
 pOpElse:TsrOpBlock;
 pOpBody:TsrOpBlock;

 function _IsNestedTrue(src:TsrRegNode):Boolean;
 var
  pCond :TsrOpBlock;
  Invert:Boolean;
 begin
  Invert:=false;
  pCond:=Main.pBlock.FindUpCondByReg(src,False,Invert);
  Result:=(pCond<>nil) and (Invert=False);
 end;

begin
 pMrgOp:=pOpMerge.Labels.pMrgOp;
 pBefor:=pOpMerge.vctx.Befor;

 pLBlock:=iCursor.AsBlock;

 if (src=nil) then
 if (pLBlock<>nil) then
 if (pLBlock.pElse=nil) then  //no else
 if (pLBlock.pCond<>nil) then //have cond
 begin
  src:=LoadStatment(ConvertStatment(pLBlock.pCond,pBefor)).pNode;
  //
  if _IsConstTrue(src) or
     _IsNestedTrue(src) then
  begin
   //early optimization

   pOpMerge.bType:=btOther;
   pOpMerge.SetLabels(nil,nil,nil);

   //down body group
   pOpBody:=AllocBlockOp;
   pOpBody.SetInfo(btOther);
   PushBlockOp(pOpMerge.line,pOpBody,Default(TsrCursor));

   Exit(pOpBody);
  end;
 end;

 //Add deferred instruction
 OpCondMerge(pOpMerge.line,pMrgOp);

 if (pLBlock<>nil) then
 begin
  pLElse:=pLBlock.pElse;
 end else
 begin
  pLElse:=nil;
 end;

 orig:=RegsStory.get_snapshot;

 pBegOp:=NewLabelOp(False); //begin

 if (pLElse<>nil) or ForceElse then //have else
 begin
  pEndOp:=NewLabelOp(False); //endif/begelse
 end else
 begin
  pEndOp:=pMrgOp; //endif
 end;

 Result:=NewBlockOp(orig);
 Result.SetLabels(pBegOp,pEndOp,pMrgOp);
 Result.SetInfo(btCond);

 Result.vctx.Befor:=pBefor;

 Result.vctx.pMrgOp:=pMrgOp;

 //add if to merge
 PushBlockOp(pOpMerge.line,Result,iCursor);

 pLine:=pOpMerge.line;

 pLine:=OpBranch  (pLine,pMrgOp);
 pLine:=AddSpirvOp(pLine,pMrgOp);

 Result.vctx.pBegLabel:=pBegOp.pDst;

 pOpMerge.pBody:=Result; //Merge->if

 if (pLBlock<>nil) then
 begin
  if (pLBlock.pCond<>nil) then
  begin
   if (src=nil) then
   begin
    src:=LoadStatment(ConvertStatment(pLBlock.pCond,pBefor)).pNode;
   end;
   Assert(src<>nil);
   //
   PrepTypeNode(src,dtBool,False);
   //
   Result.Labels.pBcnOp:=OpBranchCond(line,pBegOp,pEndOp,src); //True|False
   //
   Result.SetCond(src,true);
  end else
  begin
   //deffered
   Result.Labels.pBcnOp:=AddSpirvOp(line,Op.OpBranchConditional);
  end;
 end else
 begin
  if (src<>nil) then
  begin
   PrepTypeNode(src,dtBool,False);
   //
   Result.Labels.pBcnOp:=OpBranchCond(line,pBegOp,pEndOp,src); //True|False
   //
   Result.SetCond(src,true);
  end else
  begin
   //deffered
   Result.Labels.pBcnOp:=AddSpirvOp(line,Op.OpBranchConditional);
  end;
 end;

 AddSpirvOp(line,pBegOp);

 if (pLElse<>nil) or ForceElse then //have else
 begin

  //create else block
  pOpElse:=AllocBlockOp;
  pOpElse.SetLabels(pEndOp,pMrgOp,pMrgOp);
  pOpElse.SetInfo(btElse);

  pOpElse.vctx.pMrgOp:=pMrgOp;
  pOpElse.vctx.pPrvOp:=pEndOp;

  pOpElse.vctx.pPrdLabel:=Result.Labels.pPrdOp.pDst;
  pOpElse.vctx.pBegLabel:=pEndOp.pDst;

  //save snap links
  pOpElse.Regs.orig:=Result.Regs.orig;
  pOpElse.Regs.prev:=Result.Regs.prev;
  pOpElse.Regs.next:=Result.Regs.next;

  Result .pElse:=pOpElse; //if->else
  pOpElse.pIf  :=Result;  //else->if

  pOpMerge.pElse:=pOpElse;
 end;

 pOpMerge.pIf:=Result;

 //down body group
 pOpBody:=AllocBlockOp;
 pOpBody.SetInfo(btOther);
 Result.pBody:=pOpBody; //save body link
 PushBlockOp(line,pOpBody,Default(TsrCursor));
end;

function TEmitFlow.NewElse(pOpMerge:TsrOpBlock;iCursor:TsrCursor):TsrOpBlock;
var
 pBegOp,pMrgOp:TspirvOp;
 pOpBody:TsrOpBlock;
begin
 Result:=pOpMerge.pElse;
 Assert(Result<>nil);

 {
  merge {

   if {
    ......
   }

   else {
    OpBranch
    OpLine
    ......
   }

   OpBranch
   OpLine
   OpPhi
  }
 }

 //down else block
 PushBlockOp(pOpMerge.pIf,Result,iCursor);

 pBegOp:=Result.Labels.pBegOp;
 pMrgOp:=Result.Labels.pMrgOp;

 OpBranch  (line,pMrgOp); //goto end
 AddSpirvOp(line,pBegOp); //start else

 //down body group
 pOpBody:=AllocBlockOp;
 pOpBody.SetInfo(btOther);
 Result.pBody:=pOpBody; //save body link
 PushBlockOp(line,pOpBody,Default(TsrCursor));
end;

function TEmitFlow.NewLoop(iCursor:TsrCursor):TsrOpBlock;
var
 orig:TsrRegsSnapshot;
 pLine:TspirvOp;
 pBegOp,pMrgOp,pEndOp,pRepOp:TspirvOp;
 pNop:TspirvOp;
 pOpBody:TsrOpBlock;

 procedure add_after(pLoop:TsrOpBlock);
 var
  //pLine:TspirvOp;
  pOpBreak:TspirvOp;
  parent:TsrSourceBlock;
  pOpChild:TsrOpBlock;
  node:TDependenceNode;
 begin
  //add OpLoopMerge continue

  parent:=pLoop.FCursor.AsBlock;
  Assert(parent<>nil);

  //pLine:=line; //before close

  if IsUnconditional(parent.pCond) then
  begin
   //not post conditions
   pOpBreak:=OpBranch(line,pMrgOp); //break

   pLoop.vctx.FBreaks.Push_tail(pOpBreak);
  end else
  begin
   //have post conditions
   OpBranch(line,pEndOp); //LoopMerge
  end;

  AddSpirvOp(line,pEndOp); //OpLoopMerge end

    pOpChild:=AllocBlockOp;
    pOpChild.SetInfo(btOther);
    PushBlockOp(line,pOpChild,Default(TsrCursor));

    pLoop.pPCond:=pOpChild;

  Main.PopBlock;

   pOpChild:=AllocBlockOp;
   pOpChild.SetInfo(btOther);
   PushBlockOp(line,pOpChild,Default(TsrCursor));

   AddSpirvOp(line,pMrgOp); //end

  Main.PopBlock;
 end;

begin
 orig:=RegsStory.get_snapshot;
 PrivateList.make_copy_all;

 //get before
 pLine:=line;

 pNop:=nil;
 if pLine.IsType(ntOp) then
 if (pLine.OpId=Op.OpNop) then
 begin
  pNop:=pLine;
  pNop.mark([soNotUsed,soPost]);
 end;

 if (pNop=nil) then
 begin
  pNop:=AddSpirvOp(pLine,Op.OpNop);
  pNop.mark([soNotUsed,soPost]);
  //
  pLine:=pNop;
 end;

 Assert(pLine.IsType(ntOp) ,'WTF');
 Assert(pLine.OpId=Op.OpNop,'WTF');

 pBegOp:=NewLabelOp(False); //continue
 pMrgOp:=NewLabelOp(False); //end
 pEndOp:=NewLabelOp(False); //cond
 pRepOp:=NewLabelOp(False); //start

 Result:=NewBlockOp(RegsStory.get_snapshot,orig);
 Result.SetLabels(pBegOp,pEndOp,pMrgOp);
 Result.Labels.pRepOp:=pRepOp;
 Result.SetInfo(btLoop);

 //save nop before
 Result.vctx.Befor:=pNop;

 Result.vctx.pMrgOp:=pMrgOp;
 Result.vctx.pPrvOp:=pBegOp;
 Result.vctx.pAftOp:=pEndOp;

 //save push point
 pLine:=line;

 PushBlockOp(pLine,Result,iCursor);

 OpBranch  (line,pBegOp);
 AddSpirvOp(line,pBegOp);    //continue loop

 OpLoopMerge(line,pMrgOp,pEndOp);
 OpBranch   (line,pRepOp);
 AddSpirvOp (line,pRepOp);

 //down group
 pOpBody:=AllocBlockOp;
 pOpBody.SetInfo(btOther);
 Result.pBody:=pOpBody; //save body link

 //save push point
 pLine:=line;

 add_after(Result);

 PushBlockOp(pLine,pOpBody,Default(TsrCursor));
end;

function TEmitFlow.BlockBeg:Boolean;
var
 parent:TsrSourceBlock;
 pOpMerge:TsrOpBlock;
begin
 Result:=False;

 //is marked of end
 //if IsFinalize then Exit;

 parent:=Cursor.AsBlock;

 Case parent.bType of
  btMerg:
   begin
    pOpMerge:=NewMerge(line,Cursor);

    Result:=True;
   end;
  btCond:
   begin
    pOpMerge:=line.Parent;
    Assert(pOpMerge<>nil);
    Assert(pOpMerge.bType=btMerg);

    NewIf(pOpMerge,Cursor,nil,False);

    Result:=True;
   end;
  btElse:
   begin
    pOpMerge:=line.Parent;
    Assert(pOpMerge<>nil);
    Assert(pOpMerge.bType=btMerg);

    NewElse(pOpMerge,Cursor);

    Result:=True;
   end;
  btLoop:
   begin
    NewLoop(Cursor);

    Result:=True;
   end;
  btInline: //skip
   begin
    //
   end;
  else
   begin
    Assert(false);
   end;
 end;

end;

function TEmitFlow.BlockEnd:Boolean;
begin
 Result:=False;
 if (Main=nil) then Exit;
 if (Main.pBlock=nil) then Exit;

 if (Main.pBlock.Parent<>nil) then
 begin
  Result:=PopBlockOp;
 end;
end;

//

function TEmitFlow.get_code_ptr:Pointer;
begin
 Result:=Cursor.e_adr.get_code_ptr;
end;

procedure TEmitFlow.set_code_ptr(base:Pointer;bType:TsrBlockType);
begin
 if (Cursor.b_adr.get_code_ptr=base) then Exit;
 Cursor:=CodeHeap.FetchByPtr(base,bType);
end;

function TEmitFlow.fetch_cursor_ptr(base:Pointer;bType:TsrBlockType):TsrCursor;
begin
 Result:=CodeHeap.FetchByPtr(base,bType);
end;

{
function TEmitFlow.IsFinalize:Boolean;
begin
 Result:=False;
 if (Main.pBlock.Parent=nil) then
 if Cursor.pBlock.IsEndOf(Cursor.Adr) or
    (Main.pBlock.FVolMark=vmEndpg) then //marked of end
 begin
  Result:=True;
 end;
end;
}

procedure TEmitFlow.Finalize;
begin
 if (Main=nil) then Exit;

 if (Main.pBlock<>nil) then
  While (Main.pBlock.Parent<>nil) do
  begin
   PopBlockOp;
  end;

 AddSpirvOp(Op.OpReturn);
 AddSpirvOp(Op.OpFunctionEnd);
end;

//

function TEmitFlow.ParseStage(base:Pointer):Integer;
label
 _skip;
var
 next:TsrSourceNode;
 FLevel:DWORD;
begin
 Result:=0;
 set_code_ptr(base,btMain);

 while (Cursor.pNode<>nil) do
 begin

  if Cursor.fnext then
  begin
   //skip intruction
   Cursor.fnext:=False;
   goto _skip;
  end;

  //down
  while (Cursor.pNode.ntype=TsrSourceBlock) do
  begin
   next:=Cursor.pNode.First;
   if (next=nil) then
   begin
    //up
    goto _skip;
   end;
   //
   BlockBeg;
   //
   Cursor.pNode:=next;
   Cursor.UpdateAdr;
  end;

  if (Cursor.pNode.ntype=TsrSourceNode) then
  begin
   //skip
  end else
  if (Cursor.pNode.ntype=TsrSourceLabel) then
  begin
   //skip
  end else
  if (Cursor.pNode.ntype=TsrSourceInstruction) then
  begin
   //
   FSPI:=TsrSourceInstruction(Cursor.pNode).FSPI;
   //
   if Config.PrintAsm then
   begin

    FLevel:=0;
    if (Main<>nil) then
    if (Main.pBlock<>nil) then
    begin
     FLevel:=Main.pBlock.Level;
    end;

    Writeln(HexStr(Cursor.b_adr.Offset,4),Space(FLevel+1),get_str_spi(FSPI));
   end;
   //
   next:=Cursor.pNode;

   emit_spi;

   if (Cursor.pNode<>next) then
   begin
    //node is change
    Continue;
   end;
   //
  end else
  if (Cursor.pNode.ntype=TsrStatement) then
  begin
   //
   EmitStatment(TsrStatement(Cursor.pNode));
   //
  end else
  begin
   Assert(false,'Unhandled node:'+Cursor.pNode.ntype.ClassName);
  end;

  _skip:

  next:=Cursor.pNode.pNext;
  while (next=nil) and
        (Cursor.pNode.pParent<>nil) and
        (Cursor.pNode.pParent<>Cursor.pCode.FTop) do
  begin
   //up
   BlockEnd; //"Cursor.pNode:=Cursor.pNode.pParent" in "PopBlockOp"
   //
   next:=Cursor.pNode.pNext;
  end;
  //
  Cursor.pNode:=next;
  Cursor.UpdateAdr;

 end; //while

 Finalize;
end;

end.

