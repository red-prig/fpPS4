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
  function  NewMerge(iCursor:TsrCursor):TsrOpBlock;
  function  NewIf   (pOpMerge:TsrOpBlock;iCursor:TsrCursor;src:TsrRegNode):TsrOpBlock;
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

Procedure TEmitFlow.PushBlockOp(pLine:TspirvOp;pChild:TsrOpBlock;iCursor:TsrCursor);
begin
 pChild.FCursor:=iCursor; //prev
 //pChild.FLBlock:=pLBlock;

 InsSpirvOp(pLine,pChild);

 Main.PushBlock(pChild);

 {
 if (pLBlock<>nil) then
 begin
  Cursor.pBlock:=pLBlock; //push
 end;
 }
end;

function TEmitFlow.PopBlockOp:Boolean;
var
 pOpBlock:TsrOpBlock;
 pOpChild:TsrOpBlock;
 pBegOp,pEndOp,pMrgOp:TspirvOp;

 procedure pop_merge(pOpBlock:TsrOpBlock);
 begin
  Assert(pMrgOp<>nil);

  if not is_term_op(line) then
  begin
   OpBranch(line,pMrgOp);
  end;

  AddSpirvOp(line,pMrgOp); //end
 end;

 procedure pop_merge_after(pOpBlock:TsrOpBlock);
 begin
  //
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
   pOpBlock.Regs.prev^:=RegsStory.get_snapshot;
   //restore original by start "else"
   PrivateList.build_volatile_reset(pOpBlock.Regs.orig);
  end else
  begin
   //save current to prev
   pOpBlock.Regs.prev^:=RegsStory.get_snapshot;
   //
   if (pOpBlock.FVolMark<>vmNone) then
   begin
    //restore original if inside endpgm/break/continue
    PrivateList.build_volatile_reset(pOpBlock.Regs.orig);
   end else
   begin
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

  pLine:=line; //before close

  //if pOpBlock.Cond.FUseCont then //use continue

  if (parent.pCond<>nil) then
  begin
   //have post conditions
   if not is_term_op(line) then
   begin
    OpBranch(line,pMrgOp); //LoopMerge
   end;
  end else
  begin
   //not post conditions
   if not is_term_op(line) then
   begin
    OpBranch(line,pEndOp); //break
   end;
  end;

  AddSpirvOp(line,pMrgOp); //OpLoopMerge end

    pOpChild:=AllocBlockOp;
    pOpChild.SetInfo(btOther);
    PushBlockOp(line,pOpChild,Default(TsrCursor));

    if (parent.pCond<>nil) then
    begin
     //have post conditions
     src:=LoadStatment(ConvertStatment(parent.pCond,pLine)).pNode;
     Assert(src<>nil);
     //
     OpBranchCond(line,pBegOp,pEndOp,src); //True|False
    end else
    begin
     //not post conditions
     OpBranch(line,pBegOp); //continue
    end;

  Main.PopBlock;

  AddSpirvOp(line,pEndOp); //end

 end;

 Function IsUnconditional(cond:TsrStatement):Boolean; inline;
 begin
  Result:=(cond.sType=sCond) and (cond.u.cond=cTrue)
 end;

 Function IsUnreachable(cond:TsrStatement):Boolean; inline;
 begin
  Result:=(cond.sType=sCond) and (cond.u.cond=cFalse)
 end;

 procedure pop_loop_after(pOpBlock:TsrOpBlock);
 var
  parent:TsrSourceBlock;
 begin
  if (pOpBlock.FVolMark<>vmNone) then
  begin
   //set next volatile state
   PrivateList.build_volatile_reset(pOpBlock.Regs.next);
  end else
  begin
   parent:=pOpBlock.FCursor.AsBlock;

   if (parent.pCond=nil) then
   begin
    //continue
    PrivateList.build_volatile_conti(pOpBlock.vctx,pOpBlock.Regs.orig,pOpBlock.Regs.prev,pOpBlock.Regs.next);
   end else
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

   //set next volatile state
   PrivateList.build_volatile_reset(pOpBlock.Regs.next);
  end;
 end;

 procedure pop_other(pOpBlock:TsrOpBlock);
 begin
  //pMrgOp???
  if (pEndOp<>nil) then
  begin
   if not is_term_op(line) then
   begin
    OpBranch(line,pEndOp);
   end;
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
    Assert(pOpBlock.FCursor.pCode<>nil);
    Assert(pOpBlock.FCursor.pNode<>nil);
    Cursor.PopBlock;
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
var
 V:TsrVolatile;
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
          if (node.pCache<>nil) then
          begin
           Result.pNode:=TsrNode(node.pCache);
          end else
          begin
           V:=PrivateList.NewVolatile(@RegsStory.FUnattach);
           V.ForceBool:=True;
           //
           Result.pNode:=V;
           node.pCache:=Result.pNode;
          end;
          //load

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
var
 V:TsrVolatile;
 R:TsrRegNode;
begin
 Result:=C;

 if C.pNode.IsType(ntVolatile) then
 begin
  V:=C.pNode.specialize AsType<TsrVolatile>;
  Assert(V<>nil);

  R:=NewReg(dtBool);
  R.pWriter:=V;
  R.CustomLine:=C.pLine;

  Result.pNode:=R;
 end;

end;

procedure TEmitFlow.emit_break(pCurr:TsrOpBlock);
var
 pOpLabel:TspirvOp;

 parent:TsrSourceBlock;

 pLoop:TsrOpBlock;

 bnew:Boolean;
begin
 pLoop:=Main.pBlock.FindUpLoop;
 Assert(pLoop<>nil,'Break to Unknow');

 pOpLabel:=nil;

 {
 parent:=pLoop.FCursor.AsBlock;
 Assert(parent<>nil);

 if (parent.Last=Cursor.pNode) then //is break?
 begin
  pOpLabel:=pLoop.Labels.pEndOp;
 end else
 begin
  Assert(false,'break');
 end;
 }

 pOpLabel:=pLoop.Labels.pEndOp;
 Assert(pOpLabel<>nil);

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

 //calc volatile
 PrivateList.build_volatile_break(pLoop.vctx,pLoop.Regs.orig,pLoop.Regs.prev,pLoop.Regs.next);

 //mark hints
 mark_end_of(vmBreak);

 OpBranch(pCurr.line,pOpLabel);
 if bnew then
 begin
  AddSpirvOp(pCurr.line,NewLabelOp(True));
 end;
end;

procedure TEmitFlow.EmitStatment(node:TsrStatement);
Var
 V:TsrVolatile;
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
    C:=ConvertStatment(node.pDst,line);
    V:=C.pNode.specialize AsType<TsrVolatile>;
    Assert(V<>nil);

    C:=ConvertStatment(node.pSrc,C.pLine);
    C:=LoadStatment(C);
    D:=C.pNode.specialize AsType<TsrRegNode>;
    Assert(D<>nil);

    //Writeln('sStore to:',node.pDst.u.id,' from ',node.pSrc.u.id);

    V.AddStore(D);
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

function TEmitFlow.NewMerge(iCursor:TsrCursor):TsrOpBlock;
var
 pMrgOp:TspirvOp;
 pLine:TspirvOp;
 pNop :TspirvOp;
begin
 pMrgOp:=NewLabelOp(False); //merge

 //save push point
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

 Result:=AllocBlockOp;
 Result.SetInfo(btMerg);
 Result.SetLabels(nil,nil,pMrgOp);
 //Result.Cond.FExcMerg:=pLBlock.ExcMerg;

 //save nop before
 Result.vctx.Befor:=pNop;

 //add nop aka PostLink
 pNop:=AddSpirvOp(pLine,Op.OpNop);
 pNop.mark([soNotUsed,soPost]);

 Result.vctx.After:=pNop;

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

function TEmitFlow.NewIf(pOpMerge:TsrOpBlock;iCursor:TsrCursor;src:TsrRegNode):TsrOpBlock;
var
 orig:TsrRegsSnapshot;
 pLBlock:TsrSourceBlock;
 pLElse:TsrSourceBlock;
 pBegOp,pEndOp,pMrgOp,pAfter,pBefor:TspirvOp;
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
 pAfter:=pOpMerge.vctx.After;
 pBefor:=pOpMerge.vctx.Befor;

 pLBlock:=iCursor.AsBlock;

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

   //down body group
   pOpBody:=AllocBlockOp;
   pOpBody.SetInfo(btOther);
   PushBlockOp(line,pOpBody,Default(TsrCursor));

   Exit(pOpBody);
  end;
 end;

 //Add deferred instruction
 OpCondMerge(line,pMrgOp);

 if (pLBlock<>nil) then
 begin
  pLElse:=pLBlock.pElse;
 end else
 begin
  pLElse:=nil;
 end;

 orig:=RegsStory.get_snapshot;

 pBegOp:=NewLabelOp(False); //begin

 if (pLElse<>nil) then //have else
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
 Result.vctx.After:=pAfter; //move nop link

 PushBlockOp(line,Result,iCursor);

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

 if (pLElse<>nil) then //have else
 begin

  //create else block
  pOpElse:=AllocBlockOp;
  pOpElse.SetLabels(pEndOp,pMrgOp,pMrgOp);
  pOpElse.SetInfo(btElse);
  pOpElse.vctx.After:=pAfter; //move nop link

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

 //down else block
 PushBlockOp(line,Result,iCursor);

 pBegOp:=Result.Labels.pBegOp;
 pMrgOp:=Result.Labels.pMrgOp;

 if not is_term_op(line) then
 begin
  OpBranch (line,pMrgOp); //goto end
 end;
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
 pBegOp,pEndOp,pMrgOp,pRepOp:TspirvOp;
 pNop:TspirvOp;
 pOpBody:TsrOpBlock;
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
 pEndOp:=NewLabelOp(False); //end
 pMrgOp:=NewLabelOp(False); //cond
 pRepOp:=NewLabelOp(False); //start

 Result:=NewBlockOp(RegsStory.get_snapshot,orig);
 Result.SetLabels(pBegOp,pEndOp,pMrgOp);
 Result.SetInfo(btLoop);

 //save nop before
 Result.vctx.Befor:=pNop;

 //save push point
 pLine:=line;

 //add nop aka PostLink
 pNop:=AddSpirvOp(pLine,Op.OpNop);
 pNop.mark([soNotUsed,soPost]);
 //
 Result.vctx.After:=pNop;

 PushBlockOp(pLine,Result,iCursor);

 OpBranch  (line,pBegOp);
 AddSpirvOp(line,pBegOp);    //continue loop

 OpLoopMerge(line,pEndOp,pMrgOp);
 OpBranch   (line,pRepOp);
 AddSpirvOp (line,pRepOp);

 //down group
 pOpBody:=AllocBlockOp;
 pOpBody.SetInfo(btOther);
 Result.pBody:=pOpBody; //save body link

 PushBlockOp(line,pOpBody,Default(TsrCursor));
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
    pOpMerge:=NewMerge(Cursor);

    Result:=True;
   end;
  btCond:
   begin
    pOpMerge:=line.Parent;
    Assert(pOpMerge<>nil);
    Assert(pOpMerge.bType=btMerg);

    NewIf(pOpMerge,Cursor,nil);

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

