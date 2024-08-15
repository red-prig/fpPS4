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
 srReg,
 srOp,
 srOpUtils,
 srConfig,
 emit_op;

type
 TEmitFlow=class(TEmitOp)
  //
  Procedure InitFlow;
  Procedure PushBlockOp(pLine:TspirvOp;pChild:TsrOpBlock;pLBlock:TsrCFGBlock);
  function  PopBlockOp:Boolean;
  function  CheckBlockBeg:Boolean;
  function  CheckBlockEnd:Boolean;
  //
  function  get_code_ptr:Pointer;
  procedure set_code_ptr(base:Pointer;bType:TsrBlockType);
  function  IsFinalize:Boolean;
  function  FindLabel(Adr:TSrcAdr):TsrLabel;
  //
  procedure Finalize;
  //
  function  NextParse:Byte;
  function  ParseStage(base:Pointer):Byte;
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

Procedure TEmitFlow.PushBlockOp(pLine:TspirvOp;pChild:TsrOpBlock;pLBlock:TsrCFGBlock);
begin
 pChild.FCursor:=Cursor; //prev

 InsSpirvOp(pLine,pChild);

 Main.PushBlock(pChild);

 if (pLBlock<>nil) then
 begin
  Cursor.pBlock:=pLBlock; //push
 end;
end;

procedure UpdateVolMark(pBlock:TsrOpBlock);
var
 pChild:TsrOpBlock;
begin
 if (not pBlock.IsType(ntOpBlock)) then Exit;
 if (pBlock.Regs.FVolMark<>vmNone) then Exit;

 pChild:=pBlock.line; //last
 if (not pChild.IsType(ntOpBlock)) then Exit;

 Case pChild.Block.bType of
  btCond,
  btLoop:Exit;
  else;
 end;

 pBlock.Regs.FVolMark:=pChild.Regs.FVolMark;
end;

function TEmitFlow.PopBlockOp:Boolean;
var
 pOpBlock:TsrOpBlock;
 pOpChild:TsrOpBlock;
 pOpLabel:array[0..2] of TspirvOp;

 branch_up:Boolean;

 procedure pop_cond;
 begin
  Assert(pOpLabel[1]<>nil);

  if not is_term_op(line) then
  begin
   OpBranch(line,pOpLabel[1]);
  end;

  AddSpirvOp(line,pOpLabel[1]); //end
 end;

 procedure pop_cond_after;
 begin
  if (pOpBlock.Regs.FVolMark<>vmEnd) then
  begin
   PrivateList.build_volatile_cur(pOpBlock.Regs.pSnap_org);
  end else
  begin
   PrivateList.build_volatile_dis(pOpBlock.Regs.pSnap_org);
  end;
 end;

 procedure pop_loop;
 begin
  //add OpLoopMerge continue

  Assert(pOpLabel[0]<>nil);
  Assert(pOpLabel[1]<>nil);
  Assert(pOpLabel[2]<>nil);

  if pOpBlock.Cond.FUseCont then //use continue
  begin

   if not is_term_op(line) then
   begin
    OpBranch(line,pOpLabel[1]); //break
   end;

   AddSpirvOp(line,pOpLabel[2]); //OpLoopMerge end

     pOpChild:=AllocBlockOp;
     pOpChild.SetInfo(btOther,Cursor.Adr,Cursor.Adr);
     PushBlockOp(line,pOpChild,nil);
     OpBranch(line,pOpLabel[0]); //continue
   Main.PopBlock;

   AddSpirvOp(line,pOpLabel[1]); //end

  end else //dont used continue
  begin

   if not is_term_op(line) then
   begin
    AddSpirvOp(line,NewLabelOp(True)); //devide
   end;

   AddSpirvOp(line,pOpLabel[2]); //OpLoopMerge end

     pOpChild:=AllocBlockOp;
     pOpChild.SetInfo(btOther,Cursor.Adr,Cursor.Adr);
     PushBlockOp(line,pOpChild,nil);
     OpBranch(line,pOpLabel[1]); //break
   Main.PopBlock;

   AddSpirvOp(line,pOpLabel[1]); //end

  end;

 end;

 procedure pop_loop_after;
 begin
  PrivateList.build_volatile_old(pOpBlock.Regs.pSnap_org);
 end;

 procedure pop_else;
 begin
  if (pOpLabel[1]<>nil) then
  begin
   if not is_term_op(line) then
   begin
    OpBranch(line,pOpLabel[1]);
   end;
   AddSpirvOp(line,pOpLabel[1]);
  end;
 end;

begin
 Result:=False;
 if (Main=nil) then Exit;

 branch_up:=False;

 repeat

  pOpBlock:=Main.pBlock;
  if (pOpBlock=nil) then Exit;

  UpdateVolMark(pOpBlock);

  pOpLabel[0]:=pOpBlock.Labels.pBegOp;
  pOpLabel[1]:=pOpBlock.Labels.pEndOp;
  pOpLabel[2]:=pOpBlock.Labels.pMrgOp;

  Case pOpBlock.Block.bType of
   btCond:
    begin
     pop_cond;
     branch_up:=False;
    end;
   btLoop:
    begin
     pop_loop;
     branch_up:=False;
    end;
   else
     pop_else;
  end;

  Case pOpBlock.Block.bType of
   btAdr:
    begin
     Cursor:=pOpBlock.FCursor;
    end;
   btAdrBranch:
    begin
     Cursor:=pOpBlock.FCursor;
     branch_up:=True;
    end;
   btOther:; //nop
   else
    begin
     Cursor.PopBlock;
    end;
  end;

  Result:=Main.PopBlock;

  Case pOpBlock.Block.bType of
   btCond:
    begin
     pop_cond_after;
     //PrivateList.build_volatile_test;
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

 until (not branch_up) or (not Result);

end;

function TEmitFlow.CheckBlockBeg:Boolean;
var
 pLBlock:TsrCFGBlock;
 pOpLabel:array[0..3] of TspirvOp;
 pOpBlock:TsrOpBlock;
 pOpChild:TsrOpBlock;
 adr:TSrcAdr;
 Info:TsrBlockInfo;
begin
 Result:=False;
 adr:=Cursor.Adr;
 if (FindLabel(adr)=nil) then Exit;

 pLBlock:=Cursor.pBlock.FindBlock(adr);
 if (pLBlock<>nil) then
  Case pLBlock.bType of
   btLoop:
    begin
     PrivateList.make_copy_all;

     Info:=Default(TsrBlockInfo);
     Info.b_adr:=pLBlock.pBLabel.Adr;
     Info.e_adr:=pLBlock.pELabel.Adr;
     Info.bType:=btLoop;

     pOpLabel[0]:=NewLabelOp(False); //continue
     pOpLabel[1]:=NewLabelOp(False); //end
     pOpLabel[2]:=NewLabelOp(False); //cond
     pOpLabel[3]:=NewLabelOp(False); //start

     pOpLabel[0].Adr:=Info.b_adr;
     pOpLabel[1].Adr:=Info.e_adr;
     pOpLabel[2].Adr:=Info.e_adr;
     pOpLabel[3].Adr:=Info.b_adr;

     pOpBlock:=NewBlockOp(RegsStory.get_snapshot);
     pOpBlock.SetLabels(pOpLabel[0],pOpLabel[1],pOpLabel[2]);
     pOpBlock.SetInfo(Info);

     PushBlockOp(line,pOpBlock,pLBlock);

     OpBranch(line,pOpLabel[0]);
     AddSpirvOp(line,pOpLabel[0]);    //continue loop

     OpLoopMerge(line,pOpLabel[1],pOpLabel[2]);
     OpBranch(line,pOpLabel[3]);
     AddSpirvOp(line,pOpLabel[3]);

     //down group
     Info.bType:=btOther;
     pOpChild:=AllocBlockOp;
     pOpChild.SetInfo(Info);
     PushBlockOp(line,pOpChild,nil);
    end;
   btAdr,
   btAdrBranch: //skip
    begin
     adr:=pLBlock.pELabel.Adr;
     Cursor.Adr:=adr;
    end;
   else;
  end;

end;

function TEmitFlow.CheckBlockEnd:Boolean;
begin
 Result:=False;
 if (Main=nil) then Exit;
 if (Main.pBlock=nil) then Exit;

 if (Main.pBlock.Parent<>nil) and
    Main.pBlock.IsEndOf(Cursor.Adr) then
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

function TEmitFlow.IsFinalize:Boolean;
begin
 Result:=False;
 if (Main.pBlock.Parent=nil) then
  if Cursor.pBlock.IsEndOf(Cursor.Adr) then
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
var
 FLevel:DWORD;
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

 Result:=0;
 if IsFinalize then
 begin
  Finalize;
  Result:=1;
 end;

end;

function TEmitFlow.ParseStage(base:Pointer):Byte;
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

