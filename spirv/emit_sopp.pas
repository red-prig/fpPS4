unit emit_SOPP;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srTypes,
  srParser,
  srLabel,
  srCFG,
  srReg,
  srOp,
  srOpUtils,
  spirv,
  SprvEmit,
  emit_op;

type
 TEmit_SOPP=object(TEmitOp)
  procedure _emit_SOPP;
  procedure _emit_S_BRANCH_COND(pSlot:PsrRegSlot;n:Boolean);
  procedure _emit_S_BRANCH;
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

uses
 srVolatile;

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

 pOpLabel[0]:=NewLabelOp;
 pOpLabel[1]:=NewLabelOp;

 pLBlock:=FCursor.pCode^.FTop.DownBlock(adr);
 Assert(pLBlock<>@FCursor.pCode^.FTop,'not found');

 Info[0]:=Default(TsrBlockInfo);
 Info[1]:=Default(TsrBlockInfo);

 Case pLBlock^.bType of
  btAdr: //set new adr
   begin
    Info[0].b_adr:=FCursor.Adr;
    Info[0].e_adr:=FCursor.Adr;
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

 pOpBlock:=NewBlockOp(FRegsStory.get_snapshot);
 pOpBlock^.SetLabels(pOpLabel[0],pOpLabel[1],nil);
 pOpBlock^.SetInfo(Info[0]);
 pOpBlock^.SetCond(src,not n);

 PushBlockOp(line,pOpBlock,pLBlock);

 emit_OpCondMerge(line,pOpLabel[1]);

 Case n of
  True :emit_OpBranchCond(line,pOpLabel[1],pOpLabel[0],src);
  False:emit_OpBranchCond(line,pOpLabel[0],pOpLabel[1],src);
 end;

 AddSpirvOp(line,pOpLabel[0]);

 //down group
 pOpChild:=AllocBlockOp;
 pOpChild^.SetInfo(Info[1]);
 PushBlockOp(line,pOpChild,nil);

 if (pLBlock^.bType=btAdr) then //set new adr
 begin
  SetPtr(adr.get_pc,btAdr);
 end;
end;

procedure TEmit_SOPP.UpBuildVol(last:PsrOpBlock);
var
 node:PsrOpBlock;
begin
 node:=FMain^.pBlock;
 While (node<>nil) do
 begin

  Case node^.Block.bType of
   btCond:TEmitVolatile(Self).build_volatile_cur(node^.Regs.pSnap);
   btLoop:TEmitVolatile(Self).build_volatile_old(node^.Regs.pSnap);
   else;
  end;

  if (node=last) then Break;
  node:=node^.pParent;
 end;
end;

procedure TEmit_SOPP.emit_loop(adr:TSrcAdr);
var
 node,pOpBlock:PsrOpBlock;
 pOpLabel:array[0..1] of PspirvOp;
 FVolMark:TsrVolMark;
 bnew:Boolean;
begin

 node:=FMain^.pBlock;
 pOpBlock:=node^.FindUpLoop;
 Assert(pOpBlock<>nil,'not found');

 pOpLabel[0]:=nil;

 FVolMark:=vmNone;
 if (pOpBlock^.Block.b_adr.get_pc=adr.get_pc) then //is continue?
 begin
  pOpLabel[0]:=pOpBlock^.Labels.pMrgOp; //-> OpLoopMerge end -> OpLoopMerge before
  pOpBlock^.Cond.FUseCont:=True;
  FVolMark:=vmCont;
 end else
 if (pOpBlock^.Block.b_adr.get_pc=adr.get_pc) then //is break?
 begin
  pOpLabel[0]:=pOpBlock^.Labels.pEndOp;
  FVolMark:=vmBreak;
 end else
 begin
  Assert(false,'emit_loop');
 end;

 Assert(pOpLabel[0]<>nil);

 bnew:=true;
 if FCursor.pBlock^.IsEndOf(FCursor.Adr) then //is last
 begin
  Assert(node^.Block.e_adr.get_pc=FCursor.Adr.get_pc);
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

 emit_OpBranch(line,pOpLabel[0]);
 if bnew then
 begin
  pOpLabel[1]:=NewLabelOp;
  AddSpirvOp(line,pOpLabel[1]);
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

 node:=FMain^.pBlock;
 pOpBlock:=node^.FindUpLoop;
 Assert(pOpBlock<>nil,'not found');

 pOpLabel[0]:=nil;

 FVolMark:=vmNone;
 if (pOpBlock^.Block.b_adr.get_pc=adr.get_pc) then //is continue?
 begin
  pOpLabel[0]:=pOpBlock^.Labels.pMrgOp; //-> OpLoopMerge end -> OpLoopMerge before
  pOpBlock^.Cond.FUseCont:=True;
  FVolMark:=vmCont;
 end else
 if (pOpBlock^.Block.b_adr.get_pc=adr.get_pc) then //is break?
 begin
  pOpLabel[0]:=pOpBlock^.Labels.pEndOp;
  FVolMark:=vmBreak;
 end else
 begin
  Assert(false,'emit_loop');
 end;

 Assert(pOpLabel[0]<>nil);
 pOpLabel[1]:=NewLabelOp;

 UpBuildVol(pOpBlock);
 node^.Regs.FVolMark:=FVolMark; //mark end of

 emit_OpCondMerge(line,pOpLabel[1]);

 Case n of
  True :emit_OpBranchCond(line,pOpLabel[0],pOpLabel[1],src);
  False:emit_OpBranchCond(line,pOpLabel[1],pOpLabel[0],src);
 end;

 AddSpirvOp(line,pOpLabel[1]);
end;

function TEmit_SOPP.IsBegLoop(Adr:TSrcAdr):Boolean;
var
 node:PsrCFGBlock;
begin
 Result:=false;
 node:=FCursor.pBlock^.FindUpLoop;
 if (node<>nil) then
 begin
  Result:=node^.pBLabel^.Adr.get_pc=Adr.get_pc;
 end;
end;

function TEmit_SOPP.IsEndLoop(Adr:TSrcAdr):Boolean;
var
 node:PsrCFGBlock;
begin
 Result:=false;
 node:=FCursor.pBlock^.FindUpLoop;
 if (node<>nil) then
 begin
  Result:=node^.pELabel^.Adr.get_pc=Adr.get_pc;
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

procedure TEmit_SOPP._emit_S_BRANCH_COND(pSlot:PsrRegSlot;n:Boolean);
var
 c_adr,b_adr:TSrcAdr;
 pLabel:PsrLabel;

begin
 c_adr:=FCursor.Adr;
 b_adr:=c_adr;
 b_adr.Offdw:=get_branch_offset(FSPI);

 pLabel:=FindLabel(b_adr);
 Assert(pLabel<>nil);
 Assert(not pLabel^.IsType(ltUnknow));

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
  if FCursor.pBlock^.IsBigOf(b_adr) then
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

 c_adr:=FCursor.Adr;                      //get current
 SetPtr(adr.get_pc,btAdr);                //set new
 e_adr:=FCursor.pCode^.FTop.pELabel^.Adr; //get end of code
 SetPtr(c_adr.get_pc,btMain);             //ret current

 Info.b_adr:=adr;
 Info.e_adr:=e_adr;
 Info.bType:=btAdr;

 //down group
 pOpChild:=AllocBlockOp;
 pOpChild^.SetInfo(Info);
 PushBlockOp(line,pOpChild,nil);

 SetPtr(adr.get_pc,btAdr);
end;

procedure TEmit_SOPP._emit_S_BRANCH;
var
 c_adr,b_adr:TSrcAdr;

begin
 c_adr:=FCursor.Adr;
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
  if not IsEndLoop(b_adr) then Assert(false,'Unknow');
  emit_loop(b_adr);
 end;

end;

procedure TEmit_SOPP._emit_SOPP;
begin
 Case FSPI.SOPP.OP of
  S_NOP,
  S_WAITCNT:;

  S_ENDPGM:
   begin
    if not is_term_op(line) then
    begin
     AddSpirvOp(Op.OpReturn);
    end;
    FMain^.pBlock^.Regs.FVolMark:=vmEnd; //mark end of
   end;

  S_CBRANCH_SCC0  :_emit_S_BRANCH_COND(@FRegsStory.SCC,false);
  S_CBRANCH_SCC1  :_emit_S_BRANCH_COND(@FRegsStory.SCC,true);
  S_CBRANCH_VCCZ  :_emit_S_BRANCH_COND(@FRegsStory.VCC[0],false);
  S_CBRANCH_VCCNZ :_emit_S_BRANCH_COND(@FRegsStory.VCC[0],true);
  S_CBRANCH_EXECZ :_emit_S_BRANCH_COND(@FRegsStory.EXEC[0],false);
  S_CBRANCH_EXECNZ:_emit_S_BRANCH_COND(@FRegsStory.EXEC[0],true);

  S_BRANCH        :_emit_S_BRANCH;

  else
   Assert(false,'SOPP?'+IntToStr(FSPI.SOPP.OP));
 end;
end;

end.

