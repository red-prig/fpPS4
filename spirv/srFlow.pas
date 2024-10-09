unit srFlow;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 ps4_pssl,
 spirv,
 srNode,
 srCFGLabel,
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
 TEmitFlow=class(TEmitOp)
  //
  Procedure InitFlow;
  procedure mark_end_of(mark:TsrVolMark);
  Procedure PushBlockOp(pLine:TspirvOp;pChild:TsrOpBlock;pLBlock:TsrCFGBlock);
  function  PopBlockOp:Boolean;
  function  ConvertCond(cond:TsrCondition;pLine:TspirvOp):TsrRegNode;
  function  ConvertStatment(node:TsrStatement;pLine:TspirvOp):TsrNode;
  procedure emit_break(b_adr:TSrcAdr;pCurr:TsrOpBlock);
  procedure EmitStatment(node:TsrStatement);
  procedure EmitStatmentList(List:TsrStatementList);
  function  NewMerge(pLBlock:TsrCFGBlock):TsrOpBlock;
  function  NewIf   (pOpMerge:TsrOpBlock;pLBlock:TsrCFGBlock;src:TsrRegNode):TsrOpBlock;
  function  NewElse (pOpMerge:TsrOpBlock;pLBlock:TsrCFGBlock):TsrOpBlock;
  function  NewLoop (pLBlock:TsrCFGBlock):TsrOpBlock;
  function  CheckBlockBeg:Boolean;
  function  CheckBlockEnd:Boolean;
  //
  function  get_code_ptr:Pointer;
  procedure set_code_ptr(base:Pointer;bType:TsrBlockType);
  function  fetch_cursor_ptr(base:Pointer;bType:TsrBlockType):TsrCursor;
  function  IsFinalize:Boolean;
  function  FindLabel(Adr:TSrcAdr):TsrLabel;
  //
  procedure Finalize;
  //
  function  NextParse:Byte;
  function  ParseStage(base:Pointer):Integer;
 end;  

implementation

Procedure TEmitFlow.InitFlow;
begin
 CodeHeap.Init(Self);
 //
 InitBlock:=AllocBlockOp;
 InitBlock.SetInfo(btOther,Cursor.Adr,Cursor.Adr);
 PushBlockOp(line,InitBlock,nil);
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
  if IsReal(node.Block.bType) then Exit;

  node:=node.Parent;
 end;
end;

Procedure TEmitFlow.PushBlockOp(pLine:TspirvOp;pChild:TsrOpBlock;pLBlock:TsrCFGBlock);
begin
 pChild.FCursor:=Cursor; //prev
 pChild.FLBlock:=pLBlock;

 InsSpirvOp(pLine,pChild);

 Main.PushBlock(pChild);

 if (pLBlock<>nil) then
 begin
  Cursor.pBlock:=pLBlock; //push
 end;
end;

function TEmitFlow.PopBlockOp:Boolean;
var
 pOpBlock:TsrOpBlock;
 pOpChild:TsrOpBlock;
 pBegOp,pEndOp,pMrgOp:TspirvOp;

 procedure pop_merge;
 begin
  Assert(pMrgOp<>nil);

  if not is_term_op(line) then
  begin
   OpBranch(line,pMrgOp);
  end;

  AddSpirvOp(line,pMrgOp); //end
 end;

 procedure pop_merge_after;
 begin
  //
 end;

 procedure pop_cond;
 begin
  //
 end;

 procedure pop_cond_after;
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

 procedure pop_else;
 begin
  //
 end;

 procedure pop_else_after;
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

 procedure pop_loop;
 var
  pLine:TspirvOp;
  pLBlock:TsrCFGBlock;
  src:TsrRegNode;
 begin
  //add OpLoopMerge continue

  Assert(pBegOp<>nil);
  Assert(pEndOp<>nil);
  Assert(pMrgOp<>nil);

  pLBlock:=pOpBlock.FLBlock;
  Assert(pLBlock<>nil);

  pLine:=line; //before close

  //if pOpBlock.Cond.FUseCont then //use continue

   if (pLBlock.pCond<>nil) then
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
     pOpChild.SetInfo(btOther,Cursor.Adr,Cursor.Adr);
     PushBlockOp(line,pOpChild,nil);

     if (pLBlock.pCond<>nil) then
     begin
      //have post conditions
      src:=ConvertStatment(pLBlock.pCond,pLine);
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

  {
  end else //dont used continue
  begin

   {
   if not is_term_op(line) then
   begin
    AddSpirvOp(line,NewLabelOp(True)); //devide
   end;
   }

   AddSpirvOp(line,pMrgOp); //OpLoopMerge end

     pOpChild:=AllocBlockOp;
     pOpChild.SetInfo(btOther,Cursor.Adr,Cursor.Adr);
     PushBlockOp(line,pOpChild,nil);
     OpBranch(line,pEndOp); //break
   Main.PopBlock;

   AddSpirvOp(line,pEndOp); //end
  }


 end;

 procedure pop_loop_after;
  var
  pLBlock:TsrCFGBlock;
 begin
  if (pOpBlock.FVolMark<>vmNone) then
  begin
   //restore original if inside endpgm/break/continue
   PrivateList.build_volatile_reset(pOpBlock.Regs.orig);
  end else
  begin
   //calc break volatile state
   PrivateList.build_volatile_break(pOpBlock.vctx,pOpBlock.Regs.orig,pOpBlock.Regs.prev,pOpBlock.Regs.next);

   pLBlock:=pOpBlock.FLBlock;
   if (pLBlock.pCond<>nil) then
   begin
    //have post conditions
    PrivateList.build_volatile_conti(pOpBlock.vctx,pOpBlock.Regs.orig,pOpBlock.Regs.prev,pOpBlock.Regs.next);
   end;

   //set next volatile state
   PrivateList.build_volatile_reset(pOpBlock.Regs.next);
  end;
 end;

 procedure pop_other;
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

 if (pOpBlock.FLBlock<>nil) then
 begin
  EmitStatmentList(pOpBlock.FLBlock.FEnded);
 end;

 pBegOp:=pOpBlock.Labels.pBegOp;
 pEndOp:=pOpBlock.Labels.pEndOp;
 pMrgOp:=pOpBlock.Labels.pMrgOp;

 Case pOpBlock.Block.bType of
  btMerg:
   begin
    pop_merge;
   end;
  btCond:
   begin
    pop_cond;
   end;
  btElse:
   begin
    pop_else;
   end;
  btLoop:
   begin
    pop_loop;
   end;
  else
    pop_other;
 end;

 //restore

 Case pOpBlock.Block.bType of
  btInline:
   begin
    Assert(pOpBlock.FCursor.pBlock<>nil);
    Cursor:=pOpBlock.FCursor;
   end;
  btOther:
   begin
    if (pOpBlock.FLBlock<>nil) then
    begin
     Cursor.PopBlock;
    end;
   end;
  else
   begin
    Assert(pOpBlock.FCursor.pBlock<>nil);
    //Cursor.pCode:=pOpBlock.FCursor.pCode;
    Cursor.PopBlock;
   end;
 end;

 Result:=Main.PopBlock;

 Case pOpBlock.Block.bType of
  btMerg:
   begin
    pop_merge_after;
   end;
  btCond:
   begin
    pop_cond_after;
    //PrivateList.build_volatile_test;
   end;
  btElse:
   begin
    pop_else_after;
   end;
  btLoop:
   begin
    pop_loop_after;
    //PrivateList.build_volatile_test;
   end;
  else
   begin
    //PrivateList.build_volatile_test;
   end;
 end;

 if (pOpBlock.FLBlock<>nil) then
 begin
  EmitStatmentList(pOpBlock.FLBlock.FAfter);
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

function TEmitFlow.ConvertCond(cond:TsrCondition;pLine:TspirvOp):TsrRegNode;
begin
 case cond of
  cFalse :Result:=NewReg_b(False);
  cTrue  :Result:=NewReg_b(True);
  cScc0  :Result:=fetch_scc;
  cScc1  :Result:=fetch_scc;
  cVccz  :Result:=fetch_vccz ; //It means that lane_id=0
  cVccnz :Result:=fetch_vccz ; //It means that lane_id=0
  cExecz :Result:=fetch_execz; //It means that lane_id=0
  cExecnz:Result:=fetch_execz; //It means that lane_id=0
  else
   Assert(false,'ConvertCond');
 end;
 //Since the main condition is this condition equal to zero, then we need to invert it again!
 case cond of
  cScc0,
  cVccz,
  cExecz:
   begin
    //invert
    if Result.is_const then
    begin
     //early optimization
     Result:=NewReg_b(not Result.AsConst.AsBool,@pLine);
    end else
    begin
     Result:=OpLogicalNotTo(Result,@pLine);
    end;
    //
   end;
  else;
 end;
 //
end;

function TEmitFlow.ConvertStatment(node:TsrStatement;pLine:TspirvOp):TsrNode;
begin
 Result:=nil;
 case node.sType of
  sCond :begin
          Result:=ConvertCond(node.u.cond,pLine);
         end;
  sVar  :begin
          if (node.pCache<>nil) then
          begin
           Result:=TsrNode(node.pCache);
          end else
          begin
           Result:=PrivateList.FetchCustom(dtBool).NewVolatile;
           node.pCache:=Result;
          end;
         end;
  sLoad :begin
          Result:=TsrNode(node.pCache);
          Assert(Result<>nil);
         end;
  sNot  :begin
          Result:=TsrNode(node.pCache);
          Assert(Result<>nil);
         end;
  sOr   :begin
          Result:=TsrNode(node.pCache);
          Assert(Result<>nil);
         end;
  sAnd  :begin
          Result:=TsrNode(node.pCache);
          Assert(Result<>nil);
         end;
  else
   Assert(false);
 end;
end;

procedure TEmitFlow.emit_break(b_adr:TSrcAdr;pCurr:TsrOpBlock);
var
 pOpLabel:TspirvOp;

 pLoop:TsrOpBlock;

 bnew:Boolean;
begin
 pLoop:=Main.pBlock.FindUpLoop;
 Assert(pLoop<>nil,'Break to Unknow');

 pOpLabel:=nil;

 if (pLoop.Block.e_adr.get_code_ptr=b_adr.get_code_ptr) then //is break?
 begin
  pOpLabel:=pLoop.Labels.pEndOp;
 end else
 begin
  Assert(false,'break');
 end;

 Assert(pOpLabel<>nil);

 bnew:=true;
 if pCurr.IsEndOf(Cursor.Adr) then //is last
 begin
  Case pCurr.Block.bType of
   btSetpc:;
   else
     begin
      bnew:=false;
     end;
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
begin
 case node.sType of
  sCond:; //skip
  sStore:
   begin
    V:=ConvertStatment(node.pDst,line).specialize AsType<TsrVolatile>;
    Assert(V<>nil);

    R:=ConvertStatment(node.pSrc,line).specialize AsType<TsrRegNode>;
    Assert(R<>nil);

    V.AddStore(R);
   end;
  sLoad:
   begin
    V:=ConvertStatment(node.pSrc,line).specialize AsType<TsrVolatile>;
    Assert(V<>nil);

    R:=NewReg(dtBool);
    R.pWriter:=V;

    node.pCache:=R;
   end;
  sBreak:
   begin
    Assert(node.sLabel<>nil);

    emit_break(node.sLabel.Adr,Main.pBlock);
   end;
  sNot:
   begin
    R:=ConvertStatment(node.pSrc,line).specialize AsType<TsrRegNode>;
    Assert(R<>nil);

    R:=OpLogicalNotTo(R);

    node.pCache:=R;
   end;
  sOr:
   begin
    R:=ConvertStatment(node.pSrc,line).specialize AsType<TsrRegNode>;
    Assert(R<>nil);

    D:=ConvertStatment(node.pDst,line).specialize AsType<TsrRegNode>;
    Assert(D<>nil);

    R:=OpOrTo(R,D);

    node.pCache:=R;
   end;
  sAnd:
   begin
    R:=ConvertStatment(node.pSrc,line).specialize AsType<TsrRegNode>;
    Assert(R<>nil);

    D:=ConvertStatment(node.pDst,line).specialize AsType<TsrRegNode>;
    Assert(D<>nil);

    R:=OpAndTo(R,D);

    node.pCache:=R;
   end;
  else
   Assert(false);
 end;
end;

procedure TEmitFlow.EmitStatmentList(List:TsrStatementList);
var
 node:TsrStatement;
begin
 node:=List.pHead;
 while (node<>nil) do
 begin
  EmitStatment(node);
  //
  node:=node.pNext;
 end;
end;

function TEmitFlow.NewMerge(pLBlock:TsrCFGBlock):TsrOpBlock;
var
 Info:TsrBlockInfo;
 pMrgOp:TspirvOp;
 pLine:TspirvOp;
 pNop :TspirvOp;
begin
 Info:=Default(TsrBlockInfo);
 Info.bType:=btMerg;

 if (pLBlock<>nil) then
 begin
  Info.b_adr:=pLBlock.pBLabel.Adr;
  Info.e_adr:=pLBlock.pELabel.Adr;
 end else
 begin
  Info.b_adr:=Cursor.prev_adr;
  Info.e_adr:=Cursor.Adr;
 end;

 pMrgOp:=NewLabelOp(False); //merge
 pMrgOp.Adr:=Info.e_adr;

 //save push point
 pLine:=line;

 if pLine.IsType(ntOp) then
 if (pLine.OpId=Op.OpNop) then
 begin
  pNop:=pLine;
 end else
 begin
  pNop:=AddSpirvOp(pLine,Op.OpNop);
  pNop.mark_not_used;
  //
  pLine:=pNop;
 end;

 Result:=AllocBlockOp;
 Result.SetInfo(Info);
 Result.SetLabels(nil,nil,pMrgOp);

 //save nop before
 Result.vctx.Befor:=pNop;

 //add nop aka PostLink
 pNop:=AddSpirvOp(pLine,Op.OpNop);
 pNop.mark_not_used;

 Result.vctx.After:=pNop;

 //add by push point
 PushBlockOp(pLine,Result,pLBlock);

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

function TEmitFlow.NewIf(pOpMerge:TsrOpBlock;pLBlock:TsrCFGBlock;src:TsrRegNode):TsrOpBlock;
var
 orig:TsrRegsSnapshot;
 pLElse:TsrCFGBlock;
 pBegOp,pEndOp,pMrgOp,pAfter,pBefor:TspirvOp;
 pOpElse:TsrOpBlock;
 pOpBody:TsrOpBlock;
 Info:TsrBlockInfo;

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

 src:=nil;

 if (pLBlock<>nil) then
 if (pLBlock.pElse=nil) then  //no else
 if (pLBlock.pCond<>nil) then //have cond
 begin
  src:=ConvertStatment(pLBlock.pCond,pBefor);
  //
  if _IsConstTrue(src) or
     _IsNestedTrue(src) then
  begin
   //early optimization

   pOpMerge.Block.bType:=btOther;

   //down body group
   Info:=Default(TsrBlockInfo);
   Info.bType:=btOther;
   Info.b_adr:=pLBlock.pBLabel.Adr;
   Info.e_adr:=pLBlock.pELabel.Adr;

   pOpBody:=AllocBlockOp;
   pOpBody.SetInfo(Info);
   PushBlockOp(line,pOpBody,pLBlock);

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

 Info:=Default(TsrBlockInfo);
 Info.bType:=btCond;

 if (pLBlock<>nil) then
 begin
  Info.b_adr:=pLBlock.pBLabel.Adr;
  Info.e_adr:=pLBlock.pELabel.Adr;
 end else
 begin
  Info.b_adr:=Cursor.prev_adr;
  Info.e_adr:=Cursor.Adr;
 end;

 pBegOp:=NewLabelOp(False); //begin

 if (pLElse<>nil) then //have else
 begin
  pEndOp:=NewLabelOp(False); //endif/begelse
 end else
 begin
  pEndOp:=pMrgOp; //endif
 end;

 pBegOp.Adr:=Info.b_adr;
 pEndOp.Adr:=Info.e_adr;

 Result:=NewBlockOp(orig);
 Result.SetLabels(pBegOp,pEndOp,pMrgOp);
 Result.SetInfo(Info);

 Result.vctx.Befor:=pBefor;
 Result.vctx.After:=pAfter; //move nop link

 PushBlockOp(line,Result,pLBlock);

 pOpMerge.pBody:=Result; //Merge->if

 if (pLBlock<>nil) then
 begin
  if (pLBlock.pCond<>nil) then
  begin
   if (src=nil) then
   begin
    src:=ConvertStatment(pLBlock.pCond,pBefor);
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
  Info.bType:=btElse;
  Info.b_adr:=pLElse.pBLabel.Adr;
  Info.e_adr:=pLElse.pELabel.Adr;

  //create else block
  pOpElse:=AllocBlockOp;
  pOpElse.SetLabels(pEndOp,pMrgOp,pMrgOp);
  pOpElse.SetInfo(Info);
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
 Info.bType:=btOther;

 if (pLBlock<>nil) then
 begin
  Info.b_adr:=pLBlock.pBLabel.Adr;
  Info.e_adr:=pLBlock.pELabel.Adr;
 end else
 begin
  Info.b_adr:=Cursor.prev_adr;
  Info.e_adr:=Cursor.Adr;
 end;

 pOpBody:=AllocBlockOp;
 pOpBody.SetInfo(Info);
 Result.pBody:=pOpBody; //save body link
 PushBlockOp(line,pOpBody,nil);
end;

function TEmitFlow.NewElse(pOpMerge:TsrOpBlock;pLBlock:TsrCFGBlock):TsrOpBlock;
var
 pBegOp,pMrgOp:TspirvOp;
 pOpBody:TsrOpBlock;
 Info:TsrBlockInfo;
begin
 Result:=pOpMerge.pElse;
 Assert(Result<>nil);

 //down else block
 PushBlockOp(line,Result,pLBlock);

 pBegOp:=Result.Labels.pBegOp;
 pMrgOp:=Result.Labels.pMrgOp;

 if not is_term_op(line) then
 begin
  OpBranch (line,pMrgOp); //goto end
 end;
 AddSpirvOp(line,pBegOp); //start else

 //down body group
 Info.bType:=btOther;
 Info.b_adr:=pLBlock.pBLabel.Adr;
 Info.e_adr:=pLBlock.pELabel.Adr;

 pOpBody:=AllocBlockOp;
 pOpBody.SetInfo(Info);
 Result.pBody:=pOpBody; //save body link
 PushBlockOp(line,pOpBody,nil);
end;

function TEmitFlow.NewLoop(pLBlock:TsrCFGBlock):TsrOpBlock;
var
 orig:TsrRegsSnapshot;
 pLine:TspirvOp;
 pBegOp,pEndOp,pMrgOp,pRepOp:TspirvOp;
 pNop:TspirvOp;
 pOpBody:TsrOpBlock;
 Info:TsrBlockInfo;
begin
 orig:=RegsStory.get_snapshot;
 PrivateList.make_copy_all;

 //get before
 pLine:=line;

 if pLine.IsType(ntOp) then
 if (pLine.OpId=Op.OpNop) then
 begin
  pNop:=pLine;
 end else
 begin
  pNop:=AddSpirvOp(pLine,Op.OpNop);
  pNop.mark_not_used;
  //
  pLine:=pNop;
 end;

 Assert(pLine.IsType(ntOp) ,'WTF');
 Assert(pLine.OpId=Op.OpNop,'WTF');

 Info:=Default(TsrBlockInfo);
 Info.b_adr:=pLBlock.pBLabel.Adr;
 Info.e_adr:=pLBlock.pELabel.Adr;
 Info.bType:=btLoop;

 pBegOp:=NewLabelOp(False); //continue
 pEndOp:=NewLabelOp(False); //end
 pMrgOp:=NewLabelOp(False); //cond
 pRepOp:=NewLabelOp(False); //start

 pBegOp.Adr:=Info.b_adr;
 pEndOp.Adr:=Info.e_adr;
 pMrgOp.Adr:=Info.e_adr;
 pRepOp.Adr:=Info.b_adr;

 Result:=NewBlockOp(RegsStory.get_snapshot,orig);
 Result.SetLabels(pBegOp,pEndOp,pMrgOp);
 Result.SetInfo(Info);

 //save nop before
 Result.vctx.Befor:=pNop;

 //save push point
 pLine:=line;

 //add nop aka PostLink
 pNop:=AddSpirvOp(pLine,Op.OpNop);
 pNop.mark_not_used;
 //
 Result.vctx.After:=pNop;

 PushBlockOp(pLine,Result,pLBlock);

 OpBranch  (line,pBegOp);
 AddSpirvOp(line,pBegOp);    //continue loop

 OpLoopMerge(line,pEndOp,pMrgOp);
 OpBranch   (line,pRepOp);
 AddSpirvOp (line,pRepOp);

 //down group
 Info.bType:=btOther;
 pOpBody:=AllocBlockOp;
 pOpBody.SetInfo(Info);
 Result.pBody:=pOpBody; //save body link

 PushBlockOp(line,pOpBody,nil);
end;

function TEmitFlow.CheckBlockBeg:Boolean;
var
 pLBlock:TsrCFGBlock;
 pOpMerge:TsrOpBlock;
 adr:TSrcAdr;
begin
 Result:=False;

 //is marked of end
 if IsFinalize then Exit;

 adr:=Cursor.Adr;

 if (FindLabel(adr)=nil) then Exit;

 pLBlock:=Cursor.pBlock.FindBlock(adr);

 if (pLBlock<>nil) then
 begin
  EmitStatmentList(pLBlock.FBefore);

  Case pLBlock.bType of
   btMerg:
    begin
     pOpMerge:=NewMerge(pLBlock);

     Result:=True;
    end;
   btCond:
    begin
     pOpMerge:=line.Parent;
     Assert(pOpMerge<>nil);
     Assert(pOpMerge.Block.bType=btMerg);

     NewIf(pOpMerge,pLBlock,nil);

     Result:=True;
    end;
   btElse:
    begin
     pOpMerge:=line.Parent;
     Assert(pOpMerge<>nil);
     Assert(pOpMerge.Block.bType=btMerg);

     NewElse(pOpMerge,pLBlock);

     Result:=True;
    end;
   btLoop:
    begin
     NewLoop(pLBlock);

     Result:=True;
    end;
   btInline: //skip
    begin
     adr:=pLBlock.pELabel.Adr;
     Cursor.Adr:=adr;
    end;
   else
    begin
     Assert(false);
    end;
  end;

  EmitStatmentList(pLBlock.FStart);
 end;

end;

function TEmitFlow.CheckBlockEnd:Boolean;
begin
 Result:=False;
 if (Main=nil) then Exit;
 if (Main.pBlock=nil) then Exit;

 if (Main.pBlock.Parent<>nil) then
 if Main.pBlock.IsEndOf(Cursor.Adr) then
 begin
  Result:=PopBlockOp;
 end;
end;

//

function TEmitFlow.get_code_ptr:Pointer;
begin
 Result:=Cursor.Adr.get_code_ptr;
end;

procedure TEmitFlow.set_code_ptr(base:Pointer;bType:TsrBlockType);
begin
 if (Cursor.Adr.get_code_ptr=base) then Exit;
 Cursor:=CodeHeap.FetchByPtr(base,bType);
end;

function TEmitFlow.fetch_cursor_ptr(base:Pointer;bType:TsrBlockType):TsrCursor;
begin
 Result:=CodeHeap.FetchByPtr(base,bType);
end;

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

function TEmitFlow.FindLabel(Adr:TSrcAdr):TsrLabel;
begin
 Result:=nil;
 if (Cursor.pCode=nil) then Exit;
 Result:=Cursor.pCode.FindLabel(Adr);
end;

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

function TEmitFlow.NextParse:Byte;
label
 _open_d;
var
 FLevel:DWORD;
 pBase:TsrOpBlock;
begin
 if (Cursor.pCode=nil)  then Exit(2);
 if (Cursor.pBlock=nil) then Exit(3);
 if (Main=nil)          then Exit(4);
 if (Main.pBlock=nil)   then Exit(5);

 if Config.PrintAsm then
 begin
  Write(HexStr(Cursor.OFFSET_DW*4,4));

  //Write('(',GetGlobalIndex(line),')');

  FLevel:=0;
  if (Main<>nil) then
  if (Main.pBlock<>nil) then
  begin
   FLevel:=Main.pBlock.Level;
  end;
  Write(Space(FLevel+1));
 end;

 Result:=Cursor.Next(FSPI);
 if (Result>1) then Exit;

 if Config.PrintAsm then
 begin
  print_spi(FSPI);
 end;

 emit_spi;

 While (CheckBlockBeg) do;
 While (CheckBlockEnd) do;
 While (CheckBlockBeg) do;

 Result:=0;
 if IsFinalize then
 begin
  Finalize;
  Result:=1;
 end;

end;

function TEmitFlow.ParseStage(base:Pointer):Integer;
begin
 Result:=0;
 set_code_ptr(base,btMain);
 Main.pTop.SetCFGBlock(Cursor.pBlock);
 While (CheckBlockBeg) do;
 repeat
  Result:=NextParse;
  Case Result of
   0:;
   1:Break;
   else
     Break;
  end;
 until false;
end;

end.

