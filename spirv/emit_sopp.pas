unit emit_SOPP;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srType,
  srCFGParser,
  srCFGLabel,
  srCFGCursor,
  srFlow,
  srReg,
  srOp,
  srOpUtils,
  spirv,
  emit_fetch;

type
 TEmit_SOPP=class(TEmitFetch)
  procedure emit_SOPP;
  procedure emit_S_BRANCH_COND(cond:TsrCondition;invert:Boolean);
  procedure emit_S_BRANCH;
  procedure emit_loop_branch(b_adr:TSrcAdr;pCurr:TsrOpBlock);
  function  FetchCond(Adr:TSrcAdr):TsrOpBlock;
  function  FetchElse(Adr:TSrcAdr):Boolean;
  function  IsInLoop(Adr:TSrcAdr):Boolean;
  function  IsUnknow(Adr:TSrcAdr):Boolean;
  function  get_inline_end_addr(adr:TSrcAdr):TSrcAdr;
  procedure emit_block_unknow(adr:TSrcAdr);
 end;

implementation

function TEmit_SOPP.get_inline_end_addr(adr:TSrcAdr):TSrcAdr;
var
 cInline:TsrCursor;
begin
 cInline:=fetch_cursor_ptr(adr.get_code_ptr,btInline);
 Result :=cInline.pCode.FTop.pELabel.Adr; //get end of code
end;

function _up_to_real(t:TsrOpBlock):TsrOpBlock;
begin
 repeat
  if not t.IsType(ntOpBlock) then Break;
  if IsReal(t.Block.bType) then Break;
  t:=t.Parent;
 until false;
 Result:=t;
end;

function TEmit_SOPP.FetchCond(Adr:TSrcAdr):TsrOpBlock;
var
 pOpBlock:TsrOpBlock;
begin
 Result:=nil;

 pOpBlock:=Main.pBlock.FindUpCond;
 if (pOpBlock=nil) then Exit;

 if (pOpBlock.Block.b_adr.get_code_ptr=Adr.get_code_ptr) then
 begin
  Result:=pOpBlock;
 end else
 begin
  //special case
  if (pOpBlock.Block.b_adr.get_code_ptr=Cursor.prev_adr.get_code_ptr) then
  if (pOpBlock.Block.e_adr.get_code_ptr=Adr.get_code_ptr) then
  begin
   Result:=pOpBlock;
  end;
 end;

end;

function TEmit_SOPP.FetchElse(Adr:TSrcAdr):Boolean;
var
 pOpBlock:TsrOpBlock;
 pElse   :TsrOpBlock;
begin
 Result:=False;

 pOpBlock:=Main.pBlock.FindUpCond;
 if (pOpBlock=nil) then Exit;

 pElse:=pOpBlock.pElse;
 if (pElse<>nil) then
 begin
  if (pElse.Block.e_adr.get_code_ptr=Adr.get_code_ptr) then
  begin
   Result:=True;
  end;
 end;
end;

function TEmit_SOPP.IsInLoop(Adr:TSrcAdr):Boolean;
var
 pOpBlock:TsrOpBlock;
begin
 Result:=false;

 pOpBlock:=Main.pBlock.FindUpLoop;
 if (pOpBlock=nil) then Exit(False);

 if (pOpBlock.Block.b_adr.get_code_ptr=adr.get_code_ptr) then //is continue?
 begin
  Result:=True;
 end else
 if (pOpBlock.Block.e_adr.get_code_ptr=adr.get_code_ptr) then //is break?
 begin
  Result:=True;
 end else
 begin
  Result:=False;
 end;
end;

function TEmit_SOPP.IsUnknow(Adr:TSrcAdr):Boolean;
var
 pLabel:TsrLabel;
begin
 pLabel:=FindLabel(Adr);
 Assert(pLabel<>nil);

 Result:=pLabel.IsType(ltUnknow);
end;

procedure TEmit_SOPP.emit_S_BRANCH_COND(cond:TsrCondition;invert:Boolean);
var
 c_adr,b_adr:TSrcAdr;
 pLabel:TsrLabel;

 pCond:TsrOpBlock;

 pBegOp,pEndOp:TspirvOp;

 src:TsrRegNode;
begin
 pLabel:=FindLabel(Cursor.prev_adr);
 if (pLabel<>nil) then
 begin
  if (ltGoto in pLabel.lType) then Exit;
 end;

 While (CheckBlockBeg) do;

 c_adr:=Cursor.Adr;
 b_adr:=c_adr;
 b_adr.Offdw:=get_branch_offset(FSPI);

 pLabel:=FindLabel(b_adr);
 Assert(pLabel<>nil);

 pCond:=FetchCond(c_adr);
 Assert(pCond<>nil,'Goto Unknow');

 src:=ConvertCond(cond,pCond.vctx.Befor);

 pBegOp:=pCond.Labels.pBegOp;
 pEndOp:=pCond.Labels.pEndOp;

 pCond.SetCond(src,invert);

 //OpBranchConditional

 //The instruction specifies which block to skip, so need to invert the condition!
 //Since the main condition is this condition equal to zero, then we need to invert it again!
 Case invert of
  True: //invert of invert of invert!
   begin
    pCond.Labels.pBcnOp.AddParam(src);
    pCond.Labels.pBcnOp.AddParam(pEndOp.pDst); //True
    pCond.Labels.pBcnOp.AddParam(pBegOp.pDst); //False
   end;
  False: //invert of invert!
   begin
    pCond.Labels.pBcnOp.AddParam(src);
    pCond.Labels.pBcnOp.AddParam(pBegOp.pDst); //True
    pCond.Labels.pBcnOp.AddParam(pEndOp.pDst); //False
   end;
 end;

 if (pCond.Block.e_adr.get_code_ptr=b_adr.get_code_ptr) then
 begin
  //if (eval) {}
 end else
 begin
  emit_loop_branch(b_adr,pCond.pBody);
 end;
end;

procedure TEmit_SOPP.emit_block_unknow(adr:TSrcAdr);
var
 pOpChild:TsrOpBlock;
 Info:TsrBlockInfo;
begin
 Info:=Default(TsrBlockInfo);

 Info.b_adr:=adr;
 Info.e_adr:=get_inline_end_addr(adr);
 Info.bType:=btInline;

 //down group
 pOpChild:=AllocBlockOp;
 pOpChild.SetInfo(Info);
 PushBlockOp(line,pOpChild,nil);

 set_code_ptr(adr.get_code_ptr,btInline);
end;

procedure TEmit_SOPP.emit_loop_branch(b_adr:TSrcAdr;pCurr:TsrOpBlock);
var
 pOpLabel:TspirvOp;

 pLoop:TsrOpBlock;

 FVolMark:TsrVolMark;

 bnew:Boolean;
begin
 pLoop:=Main.pBlock.FindUpLoop;
 Assert(pLoop<>nil,'Goto Unknow');
 //break/continue

 pOpLabel:=nil;

 FVolMark:=vmNone;
 if (pLoop.Block.b_adr.get_code_ptr=b_adr.get_code_ptr) then //is continue?
 begin
  pOpLabel:=pLoop.Labels.pMrgOp; //-> OpLoopMerge end -> OpLoopMerge before
  pLoop.Cond.FUseCont:=True;
  FVolMark:=vmConti;
 end else
 if (pLoop.Block.e_adr.get_code_ptr=b_adr.get_code_ptr) then //is break?
 begin
  pOpLabel:=pLoop.Labels.pEndOp;
  FVolMark:=vmBreak;
 end else
 begin
  Assert(false,'break/continue');
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
 case FVolMark of
  vmBreak:PrivateList.build_volatile_break(pLoop.vctx,pLoop.Regs.orig,pLoop.Regs.prev,pLoop.Regs.next);
  vmConti:PrivateList.build_volatile_conti(pLoop.vctx,pLoop.Regs.orig,pLoop.Regs.prev,pLoop.Regs.next);
 end;

 //mark hints
 mark_end_of(FVolMark);

 OpBranch(pCurr.line,pOpLabel);
 if bnew then
 begin
  AddSpirvOp(pCurr.line,NewLabelOp(True));
 end;
end;

procedure TEmit_SOPP.emit_S_BRANCH;
var
 pLabel:TsrLabel;
 c_adr,b_adr:TSrcAdr;
begin
 pLabel:=FindLabel(Cursor.prev_adr);
 if (pLabel<>nil) then
 begin
  if (ltGoto in pLabel.lType) then Exit;
 end;

 While (CheckBlockBeg) do;

 c_adr:=Cursor.Adr;
 b_adr:=c_adr;
 b_adr.Offdw:=get_branch_offset(FSPI);

 if FetchElse(b_adr) then
 begin
  //{} else {}
 end else
 begin
  emit_loop_branch(b_adr,Main.pBlock);
 end;
end;

procedure TEmit_SOPP.emit_SOPP;
begin
 Case FSPI.SOPP.OP of
  S_NOP,
  S_WAITCNT:;

  S_TTRACEDATA:; //write_thread_trace_data(M0[31:0])

  S_ENDPGM:
   begin
    if not is_term_op(line) then
    begin
     AddSpirvOp(Op.OpReturn);
    end;
    //mark hints
    mark_end_of(vmEndpg);
   end;

  S_CBRANCH_SCC0  :emit_S_BRANCH_COND(cScc0 ,false);
  S_CBRANCH_SCC1  :emit_S_BRANCH_COND(cScc0 ,true);
  S_CBRANCH_VCCZ  :emit_S_BRANCH_COND(cVccz ,false); //It means that lane_id=0
  S_CBRANCH_VCCNZ :emit_S_BRANCH_COND(cVccz ,true);  //It means that lane_id=0
  S_CBRANCH_EXECZ :emit_S_BRANCH_COND(cExecz,false); //It means that lane_id=0
  S_CBRANCH_EXECNZ:emit_S_BRANCH_COND(cExecz,true);  //It means that lane_id=0

  S_BRANCH        :emit_S_BRANCH;

  else
   Assert(false,'SOPP?'+IntToStr(FSPI.SOPP.OP));
 end;
end;

end.

