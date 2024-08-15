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
  procedure emit_S_BRANCH_COND(pSlot:PsrRegSlot;n:Boolean);
  procedure emit_S_BRANCH;
  procedure mark_end_of;
  function  IsBegLoop(Adr:TSrcAdr):Boolean;
  function  IsEndLoop(Adr:TSrcAdr):Boolean;
  function  IsUnknow(Adr:TSrcAdr):Boolean;
  procedure emit_cond_block(pSlot:PsrRegSlot;n:Boolean;adr:TSrcAdr);
  procedure emit_block_unknow(adr:TSrcAdr);
  procedure UpBuildVol(last:PsrOpBlock);
  procedure emit_loop(adr:TSrcAdr);
  procedure emit_loop_cond(pSlot:PsrRegSlot;n:Boolean;adr:TSrcAdr);
 end;

implementation

procedure TEmit_SOPP.emit_cond_block(pSlot:PsrRegSlot;n:Boolean;adr:TSrcAdr);
var
 src:PsrRegNode;
 pOpBlock:PsrOpBlock;
 pOpChild:PsrOpBlock;
 pOpLabel:array[0..1] of PspirvOp;
 pLBlock:PsrCFGBlock;
 Info:array[0..1] of TsrBlockInfo;
begin
 src:=MakeRead(pSlot,dtBool); //get before OpBranchConditional

 pOpLabel[0]:=NewLabelOp(False);
 pOpLabel[1]:=NewLabelOp(False);

 pLBlock:=Cursor.pCode^.FTop.DownBlock(adr);
 Assert(pLBlock<>@Cursor.pCode^.FTop,'not found');

 Info[0]:=Default(TsrBlockInfo);
 Info[1]:=Default(TsrBlockInfo);

 Case pLBlock^.bType of
  btAdr: //set new adr
   begin
    Info[0].b_adr:=Cursor.Adr;
    Info[0].e_adr:=Cursor.Adr;
    Info[0].bType:=btCond;
    //
    Info[1].b_adr:=pLBlock^.pBLabel^.Adr;
    Info[1].e_adr:=pLBlock^.pELabel^.Adr;
    Info[1].bType:=btAdr;
   end;
  btCond: //normal cond
   begin
    Info[0].b_adr:=pLBlock^.pBLabel^.Adr;
    Info[0].e_adr:=pLBlock^.pELabel^.Adr;
    Info[0].bType:=btCond;
    //
    Info[1].b_adr:=Info[0].b_adr;
    Info[1].e_adr:=Info[0].e_adr;
    Info[1].bType:=btOther;
   end;
  else
   Assert(false,'emit_cond_block');
 end;

 pOpLabel[0]^.Adr:=Info[0].b_adr;
 pOpLabel[1]^.Adr:=Info[0].e_adr;

 pOpBlock:=NewBlockOp(get_snapshot);
 pOpBlock^.SetLabels(pOpLabel[0],pOpLabel[1],nil);
 pOpBlock^.SetInfo(Info[0]);
 pOpBlock^.SetCond(src,not n);

 PushBlockOp(line,pOpBlock,pLBlock);

 OpCondMerge(line,pOpLabel[1]);

 Case n of
  True :OpBranchCond(line,pOpLabel[1],pOpLabel[0],src);
  False:OpBranchCond(line,pOpLabel[0],pOpLabel[1],src);
 end;

 AddSpirvOp(line,pOpLabel[0]);

 //down group
 pOpChild:=AllocBlockOp;
 pOpChild^.SetInfo(Info[1]);
 PushBlockOp(line,pOpChild,nil);

 if (pLBlock^.bType=btAdr) then //set new adr
 begin
  set_code_ptr(adr.get_code_ptr,btAdr);
 end;
end;

procedure TEmit_SOPP.UpBuildVol(last:PsrOpBlock);
var
 node:PsrOpBlock;
begin
 node:=Main^.pBlock;
 While (node<>nil) do
 begin

  Case node^.Block.bType of
   btCond:PrivateList.build_volatile_cur(node^.Regs.pSnap_cur);
   btLoop:PrivateList.build_volatile_brk(node^.Regs.pSnap_cur);
   else;
  end;

  if (node=last) then Break;
  node:=node^.Parent;
 end;
end;

procedure TEmit_SOPP.emit_loop(adr:TSrcAdr);
var
 node,pOpBlock:PsrOpBlock;
 pOpLabel:PspirvOp;
 FVolMark:TsrVolMark;
 bnew:Boolean;
begin
 node:=Main^.pBlock;
 pOpBlock:=node^.FindUpLoop;
 Assert(pOpBlock<>nil,'not found');

 pOpLabel:=nil;

 FVolMark:=vmNone;
 if (pOpBlock^.Block.b_adr.get_code_ptr=adr.get_code_ptr) then //is continue?
 begin
  pOpLabel:=pOpBlock^.Labels.pMrgOp; //-> OpLoopMerge end -> OpLoopMerge before
  pOpBlock^.Cond.FUseCont:=True;
  FVolMark:=vmCont;
 end else
 if (pOpBlock^.Block.b_adr.get_code_ptr=adr.get_code_ptr) then //is break?
 begin
  pOpLabel:=pOpBlock^.Labels.pEndOp;
  FVolMark:=vmBreak;
 end else
 begin
  Assert(false,'emit_loop');
 end;

 Assert(pOpLabel<>nil);

 bnew:=true;
 if Cursor.pBlock^.IsEndOf(Cursor.Adr) then //is last
 begin
  //Assert(node^.Block.e_adr.get_pc=Cursor.Adr.get_pc);
  Case node^.Block.bType of
   btSetpc:;
   else
     begin
      bnew:=false;
     end;
  end;
 end;

 UpBuildVol(pOpBlock);
 node^.Regs.FVolMark:=FVolMark; //mark end of

 OpBranch(line,pOpLabel);
 if bnew then
 begin
  AddSpirvOp(line,NewLabelOp(True));
 end;
end;

procedure TEmit_SOPP.emit_loop_cond(pSlot:PsrRegSlot;n:Boolean;adr:TSrcAdr);
var
 src:PsrRegNode;
 node,pOpBlock:PsrOpBlock;
 pOpLabel:array[0..1] of PspirvOp;
 FVolMark:TsrVolMark;
begin
 src:=MakeRead(pSlot,dtBool);

 node:=Main^.pBlock;
 pOpBlock:=node^.FindUpLoop;
 Assert(pOpBlock<>nil,'not found');

 pOpLabel[0]:=nil;

 FVolMark:=vmNone;
 if (pOpBlock^.Block.b_adr.get_code_ptr=adr.get_code_ptr) then //is continue?
 begin
  pOpLabel[0]:=pOpBlock^.Labels.pMrgOp; //-> OpLoopMerge end -> OpLoopMerge before
  pOpBlock^.Cond.FUseCont:=True;
  FVolMark:=vmCont;
 end else
 if (pOpBlock^.Block.b_adr.get_code_ptr=adr.get_code_ptr) then //is break?
 begin
  pOpLabel[0]:=pOpBlock^.Labels.pEndOp;
  FVolMark:=vmBreak;
 end else
 begin
  Assert(false,'emit_loop');
 end;

 Assert(pOpLabel[0]<>nil);
 pOpLabel[1]:=NewLabelOp(False);

 UpBuildVol(pOpBlock);
 node^.Regs.FVolMark:=FVolMark; //mark end of

 OpCondMerge(line,pOpLabel[1]);

 Case n of
  True :OpBranchCond(line,pOpLabel[0],pOpLabel[1],src);
  False:OpBranchCond(line,pOpLabel[1],pOpLabel[0],src);
 end;

 AddSpirvOp(line,pOpLabel[1]);
end;

function TEmit_SOPP.IsBegLoop(Adr:TSrcAdr):Boolean;
var
 node:PsrCFGBlock;
begin
 Result:=false;
 node:=Cursor.pBlock^.FindUpLoop;
 if (node<>nil) then
 begin
  Result:=node^.pBLabel^.Adr.get_code_ptr=Adr.get_code_ptr;
 end;
end;

function TEmit_SOPP.IsEndLoop(Adr:TSrcAdr):Boolean;
var
 node:PsrCFGBlock;
begin
 Result:=false;
 node:=Cursor.pBlock^.FindUpLoop;
 if (node<>nil) then
 begin
  Result:=node^.pELabel^.Adr.get_code_ptr=Adr.get_code_ptr;
 end;
end;

function TEmit_SOPP.IsUnknow(Adr:TSrcAdr):Boolean;
var
 pLabel:PsrLabel;
begin
 pLabel:=FindLabel(Adr);
 Assert(pLabel<>nil);

 Result:=pLabel^.IsType(ltUnknow);
end;

procedure TEmit_SOPP.emit_S_BRANCH_COND(pSlot:PsrRegSlot;n:Boolean);
var
 c_adr,b_adr:TSrcAdr;
 pLabel:PsrLabel;
begin
 c_adr:=Cursor.Adr;
 b_adr:=c_adr;
 b_adr.Offdw:=get_branch_offset(FSPI);

 pLabel:=FindLabel(b_adr);
 Assert(pLabel<>nil);
 //Assert(not pLabel^.IsType(ltUnknow));

 if pLabel^.IsType(ltBegAdr) then //adr
 begin
  emit_cond_block(pSlot,n,b_adr);
 end else
 if (SmallInt(FSPI.SOPP.SIMM)<0) then //up
 begin //continue?
  if not IsBegLoop(b_adr) then Assert(false,'Unknow');
  emit_loop_cond(pSlot,n,b_adr);
 end else
 begin //down
  if Cursor.pBlock^.IsBigOf(b_adr) then
  begin //break?
   if not IsEndLoop(b_adr) then Assert(false,'Unknow');
   emit_loop_cond(pSlot,n,b_adr);
  end else
  begin //cond
   emit_cond_block(pSlot,n,c_adr);
  end;
 end;

end;

procedure TEmit_SOPP.emit_block_unknow(adr:TSrcAdr);
var
 c_adr:TSrcAdr;
 e_adr:TSrcAdr;
 pOpChild:PsrOpBlock;
 Info:TsrBlockInfo;
begin
 Info:=Default(TsrBlockInfo);

 c_adr:=Cursor.Adr;                          //get current
 set_code_ptr(adr.get_code_ptr,btAdrBranch); //set new
 e_adr:=Cursor.pCode^.FTop.pELabel^.Adr;     //get end of code
 set_code_ptr(c_adr.get_code_ptr,btMain);    //ret current

 Info.b_adr:=adr;
 Info.e_adr:=e_adr;
 Info.bType:=btAdrBranch;

 //down group
 pOpChild:=AllocBlockOp;
 pOpChild^.SetInfo(Info);
 PushBlockOp(line,pOpChild,nil);

 set_code_ptr(adr.get_code_ptr,btAdrBranch);
end;

procedure TEmit_SOPP.emit_S_BRANCH;
var
 c_adr,b_adr:TSrcAdr;

begin
 c_adr:=Cursor.Adr;
 b_adr:=c_adr;
 b_adr.Offdw:=get_branch_offset(FSPI);

 if IsUnknow(b_adr) then
 begin
  emit_block_unknow(b_adr);
 end else
 if (SmallInt(FSPI.SOPP.SIMM)<0) then //up
 begin  //continue?
  if not IsBegLoop(b_adr) then Assert(false,'Unknow');
  emit_loop(b_adr);
 end else //down
 begin //break?
  if IsEndLoop(b_adr) then
  begin
   emit_loop(b_adr);
  end else
  begin
   emit_block_unknow(b_adr);
  end;
 end;

end;

procedure TEmit_SOPP.mark_end_of;
begin
 Main^.pBlock^.Regs.FVolMark:=vmEnd; //mark end of
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
    mark_end_of;
   end;

  S_CBRANCH_SCC0  :emit_S_BRANCH_COND(get_scc  ,false);
  S_CBRANCH_SCC1  :emit_S_BRANCH_COND(get_scc  ,true);
  S_CBRANCH_VCCZ  :emit_S_BRANCH_COND(get_vcc0 ,false);
  S_CBRANCH_VCCNZ :emit_S_BRANCH_COND(get_vcc0 ,true);
  S_CBRANCH_EXECZ :emit_S_BRANCH_COND(get_exec0,false);
  S_CBRANCH_EXECNZ:emit_S_BRANCH_COND(get_exec0,true);

  S_BRANCH        :emit_S_BRANCH;

  else
   Assert(false,'SOPP?'+IntToStr(FSPI.SOPP.OP));
 end;
end;

end.

