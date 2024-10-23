unit srCFGParser;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 ps4_pssl,
 ginodes,
 srNode,
 srCFGLabel;

type
 TsrCFGBlock=class;

 TsrCFGBlockList=specialize TNodeListClass<TsrCFGBlock>;

 TsrStatementList=specialize TNodeListClass<TsrStatement>;

 TsrCFGBlock=class
  pParent,pPrev,pNext:TsrCFGBlock;

  FList:TsrCFGBlockList;

  pIf  :TsrCFGBlock;
  pElse:TsrCFGBlock;

  pBLabel:TsrLabel;
  pELabel:TsrLabel;

  pCond:TsrStatement;

  FBefore:TsrStatementList;
  FAfter :TsrStatementList;

  FStart :TsrStatementList;
  FEnded :TsrStatementList;

  EndFixed:Boolean;

  bType:TsrBlockType;

  order:Integer;

  function  beg_adr:TSrcAdr;
  function  end_adr:TSrcAdr;
  function  First:TsrCFGBlock;
  function  Last :TsrCFGBlock;
  function  pReal:TsrCFGBlock;
  function  IsEndOf  (Adr:TSrcAdr):Boolean;
  function  IsBigOf  (Adr:TSrcAdr):Boolean;
  function  IsContain(Adr:TSrcAdr):Boolean;
  function  IsInside (Adr:TSrcAdr):Boolean;
  function  FindBlock(Adr:TSrcAdr):TsrCFGBlock;
  function  DownBlock(Adr:TSrcAdr):TsrCFGBlock;
  function  WorkBlock(Adr:TSrcAdr):TsrCFGBlock;
  function  FindNext (Adr:TSrcAdr):TsrCFGBlock;
  function  FindPrev (Adr:TSrcAdr):TsrCFGBlock;
  function  UpBlock  (Adr:TSrcAdr):TsrCFGBlock;
  function  FindUpLoop:TsrCFGBlock;
  Procedure Push_front  (new:TsrCFGBlock;_first:TsrCFGBlock=nil;_last:TsrCFGBlock=nil);
  Procedure Push_back   (new:TsrCFGBlock;_first:TsrCFGBlock=nil;_last:TsrCFGBlock=nil);
  Procedure Insert_after(node,new:TsrCFGBlock;_first:TsrCFGBlock=nil;_last:TsrCFGBlock=nil);
  Procedure MoveTo(_parent,_first,_last:TsrCFGBlock);
  Procedure InsertAfterTo(_parent,_after,_first,_last:TsrCFGBlock);
  Procedure MoveFromByRange(_parent:TsrCFGBlock;b_adr,e_adr:TSrcAdr);
  function  ExistByRange(b_adr,e_adr:TSrcAdr):Boolean;
  function  get_level:DWORD;
 end;

 TsrCodeBlock=class(TsrLabelBlock)
  pNext:TsrCodeBlock;
  //----
  FTop:TsrCFGBlock;
 end;

 TsrUnknowGoto=class
  pPrev,pNext:TsrUnknowGoto;
  goto_stmt :TsrStatement;
  label_stmt:TsrLabel;
 end;

 TsrUnknowGotoList=specialize TNodeListClass<TsrUnknowGoto>;

 TsrCFGParser=object
  FCursor:TsrLCursor;
  pCode  :TsrCodeBlock;
  pBlock :TsrCFGBlock;
  FUnknowGotoList:TsrUnknowGotoList;
  FSPI:TSPI;
  order:Integer;
  procedure AddUnknowGoto(goto_stmt,next_stmt,label_stmt:TsrLabel;cond:TsrCondition);
  procedure _print_label(Adr:TSrcAdr);
  function  get_str_prefix(node:TsrCFGBlock;adr:TSrcAdr):RawByteString;
  procedure Print();
  function  Parse():Integer;
  function  FindUpCond(cond:TsrCondition;Adr:TSrcAdr):TsrCFGBlock;
  procedure OpenExec;
  procedure CloseExec(Before:Boolean);
  function  NextParse:Integer;
  Procedure Finalize;
  function  CheckBlockEnd:Boolean;
  function  emit_SOP1:Boolean;
  function  emit_SOPP:Boolean;
  Function  NewBlock:TsrCFGBlock;
  Procedure PushBlock(New:TsrCFGBlock);
  function  PopBlock:Boolean;
  function  FindUpperLoop(branch_adr:TSrcAdr;var r_child,r_parent:TsrCFGBlock):Integer;
  function  FindLowerLoop(branch_adr:TSrcAdr;var r_child,r_parent:TsrCFGBlock):Integer;
  procedure emit_S_BRANCH(cond:TsrCondition);
  procedure ExecPass;
  procedure ExecConvert(node:TsrCFGBlock);
  procedure ExecDivide(adr:TSrcAdr);
  procedure GotoPass;
  function  BlockOf(stmt:TsrLabel):TsrCFGBlock;
  function  LevelOf(stmt:TsrLabel):DWORD;
  function  IsDirectlyRelated(goto_stmt,label_stmt:TsrLabel):Boolean;
  function  IsIndirectlyRelated(goto_stmt,label_stmt:TsrLabel):Boolean;
  function  AreSiblings(stmt:TsrCFGBlock;label_stmt:TsrLabel):Boolean;
  function  AreSiblings(stmt,label_stmt:TsrLabel):Boolean;
  function  SiblingFromNephew(uncle,nephew:TsrLabel):TsrCFGBlock;
  function  AreOrdered(left_sibling:TsrCFGBlock;right_sibling:TsrLabel):Boolean;
  function  AreOrdered(left_sibling,right_sibling:TsrLabel):Boolean;
  function  NeedsLift(goto_stmt,label_stmt:TsrLabel):Boolean;
  procedure RemoveGoto(goto_stmt:TsrStatement;label_stmt:TsrLabel);
  procedure EliminateAsConditional(goto_stmt:TsrStatement;label_stmt:TsrLabel);
  procedure EliminateAsLoop(goto_stmt:TsrStatement;label_stmt:TsrLabel);
  function  MoveOutward(goto_stmt:TsrStatement;label_stmt:TsrLabel):TsrStatement;
  function  MoveOutwardIf(goto_stmt:TsrStatement;label_stmt:TsrLabel):TsrStatement;
  function  MoveOutwardLoop(goto_stmt:TsrStatement;label_stmt:TsrLabel):TsrStatement;
  function  MoveInward(goto_stmt:TsrStatement;label_stmt:TsrLabel):TsrStatement;
  function  Lift(goto_stmt:TsrStatement;label_stmt:TsrLabel):TsrStatement;
 end;

function last_block:TsrCFGBlock;
function is_valid(v:TsrCFGBlock):Boolean;

function parse_code_cfg(bType:TsrBlockType;pCode:TsrCodeBlock):Integer;

implementation

uses
 srConfig;

function parse_code_cfg(bType:TsrBlockType;pCode:TsrCodeBlock):Integer;
var
 LParser:TsrCFGParser;
begin
 pCode.FTop.bType:=bType;

 LParser:=Default(TsrCFGParser);
 LParser.pCode:=pCode;
 Result:=LParser.Parse();

 if PsrConfig(pCode.FEmit.GetConfig)^.PrintCfg then
 begin
  LParser.Print();
 end;
end;

function TsrCFGBlock.beg_adr:TSrcAdr;
var
 node:TsrCFGBlock;
begin
 node:=Self;
 Result:=Default(TSrcAdr);
 //
 while (node<>nil) do
 begin
  if (node.pBLabel=nil) then
  begin
   if (node.pPrev=nil) then
   begin
    node:=node.pParent;
   end else
   begin
    node:=node.pPrev;
   end;
  end else
  begin
   Exit(node.pBLabel.Adr);
  end;
 end;
end;

function TsrCFGBlock.end_adr:TSrcAdr;
var
 node:TsrCFGBlock;
begin
 node:=Self;
 Result:=Default(TSrcAdr);
 Result.Offdw:=High(DWORD);
 //
 while (node<>nil) do
 begin
  if (node.pELabel=nil) then
  begin
   if (node.pNext=nil) then
   begin
    node:=node.pParent;
   end else
   begin
    node:=node.pNext;
   end;
  end else
  begin
   Exit(node.pELabel.Adr);
  end;
 end;
end;

function TsrCFGBlock.First:TsrCFGBlock;
begin
 Result:=FList.pHead;
end;

function TsrCFGBlock.Last:TsrCFGBlock;
begin
 Result:=FList.pTail;
end;

function TsrCFGBlock.pReal:TsrCFGBlock;
begin
 if (Self=nil) then Exit(nil);
 Result:=Self;
 while (Result<>nil) do
 begin
  if IsReal(Result.bType) then Break;
  Result:=Result.pParent;
 end;
end;

function TsrCFGBlock.IsEndOf(Adr:TSrcAdr):Boolean;
begin
 Result:=False;
 if (pELabel<>nil) then
 begin
  Result:=(pELabel.Adr.get_code_ptr<=Adr.get_code_ptr);
 end;
end;

function TsrCFGBlock.IsBigOf(Adr:TSrcAdr):Boolean;
begin
 Result:=False;
 if (pELabel<>nil) then
 begin
  Result:=(pELabel.Adr.get_code_ptr<Adr.get_code_ptr);
 end;
end;

function TsrCFGBlock.IsContain(Adr:TSrcAdr):Boolean;
var
 b,e:Boolean;
begin
 b:=true;
 if (pBLabel<>nil) then
 begin
  b:=(pBLabel.Adr.get_code_ptr<=Adr.get_code_ptr);
 end;
 e:=true;
 if (pELabel<>nil) then
 begin
  e:=(pELabel.Adr.get_code_ptr>Adr.get_code_ptr);
 end;
 Result:=b and e;
end;

function TsrCFGBlock.IsInside(Adr:TSrcAdr):Boolean;
var
 b,e:Boolean;
begin
 b:=true;
 if (pBLabel<>nil) then
 begin
  b:=(pBLabel.Adr.get_code_ptr<Adr.get_code_ptr);
 end;
 e:=true;
 if (pELabel<>nil) then
 begin
  e:=(pELabel.Adr.get_code_ptr>Adr.get_code_ptr);
 end;
 Result:=b and e;
end;

function TsrCFGBlock.FindBlock(Adr:TSrcAdr):TsrCFGBlock;
var
 node:TsrCFGBlock;
begin
 Result:=nil;
 node:=First;
 While (node<>nil) do
 begin
  if node.IsContain(Adr) then Exit(node);
  node:=node.pNext;
 end;
end;

function TsrCFGBlock.DownBlock(Adr:TSrcAdr):TsrCFGBlock;
var
 next:TsrCFGBlock;
begin
 Result:=Self;
 repeat
  next:=Result.FindBlock(Adr);
  if (next=nil) then Exit;
  Result:=next;
 until false;
end;

function TsrCFGBlock.WorkBlock(Adr:TSrcAdr):TsrCFGBlock;
var
 next:TsrCFGBlock;
begin
 Result:=Self;
 repeat
  next:=Result.FindBlock(Adr);
  if (next=nil) then Exit;
  if (next.beg_adr.get_code_ptr=Adr.get_code_ptr) then Exit;
  Result:=next;
 until false;
end;

function last_block:TsrCFGBlock;
begin
 Result:=TsrCFGBlock(Pointer(High(ptruint)));
end;

function is_valid(v:TsrCFGBlock):Boolean;
begin
 Result:=(v<>nil) and (v<>last_block);
end;

function TsrCFGBlock.FindNext(Adr:TSrcAdr):TsrCFGBlock;
var
 node:TsrCFGBlock;
begin
 Result:=nil;
 node:=First;
 While (node<>nil) do
 begin
  if (node.beg_adr.get_code_ptr>Adr.get_code_ptr) then
  begin
   Exit(node);
  end;
  node:=node.pNext;
 end;
 Result:=last_block;
end;

function TsrCFGBlock.FindPrev(Adr:TSrcAdr):TsrCFGBlock;
var
 node:TsrCFGBlock;
begin
 Result:=last_block;
 node:=Last;
 While (node<>nil) do
 begin
  if (node.end_adr.get_code_ptr<=Adr.get_code_ptr) then
  begin
   Exit(node);
  end;
  node:=node.pPrev;
 end;
 Result:=nil;
end;

function TsrCFGBlock.UpBlock(Adr:TSrcAdr):TsrCFGBlock;
var
 next:TsrCFGBlock;
begin
 Result:=Self;
 While (Result.IsEndOf(Adr)) do
 begin
  next:=Result.pParent;
  if (next=nil) then Exit;
  Result:=next;
 end;
end;

function TsrCFGBlock.FindUpLoop:TsrCFGBlock;
var
 node:TsrCFGBlock;
begin
 Result:=nil;
 node:=Self;
 While (node<>nil) do
 begin
  if (node.bType=btLoop) then Exit(node);
  node:=node.pParent;
 end;
end;

Procedure TsrCFGBlock.Push_front(new:TsrCFGBlock;_first:TsrCFGBlock=nil;_last:TsrCFGBlock=nil);
begin
 if (New=nil) then Exit;
 new.pParent:=Self;
 MoveTo(new,_first,_last);
 FList.Push_head(new);
end;

Procedure TsrCFGBlock.Push_back(new:TsrCFGBlock;_first:TsrCFGBlock=nil;_last:TsrCFGBlock=nil);
begin
 if (New=nil) then Exit;
 new.pParent:=Self;
 MoveTo(new,_first,_last);
 FList.Push_tail(new);
end;

Procedure TsrCFGBlock.Insert_after(node,new:TsrCFGBlock;_first:TsrCFGBlock=nil;_last:TsrCFGBlock=nil);
begin
 if (New=nil) then Exit;
 new.pParent:=Self;
 MoveTo(new,_first,_last);
 //
 if (node=nil) then
 begin
  FList.Push_head(new);
 end else
 if (node=last_block) then
 begin
  FList.Push_tail(new);
 end else
 begin
  FList.InsertAfter(node,new);
 end;
end;

Procedure TsrCFGBlock.MoveTo(_parent,_first,_last:TsrCFGBlock);
var
 next:TsrCFGBlock;
begin
 While (_first<>nil) do
 begin
  Assert(_first.pParent=Self);
  next:=_first.pNext;
  FList.Remove(_first);
  _parent.FList.Push_tail(_first);
  _first.pParent:=_parent;
  if (_first=_last) then Break;
  _first:=next;
 end;
end;

Procedure TsrCFGBlock.InsertAfterTo(_parent,_after,_first,_last:TsrCFGBlock);
var
 next:TsrCFGBlock;
begin
 While (_first<>nil) do
 begin
  Assert(_first.pParent=Self);
  next:=_first.pNext;
  FList.Remove(_first);
  _parent.FList.InsertAfter(_after,_first);
  _after:=_first;
  _first.pParent:=_parent;
  if (_first=_last) then Break;
  _first:=next;
 end;
end;

Procedure TsrCFGBlock.MoveFromByRange(_parent:TsrCFGBlock;b_adr,e_adr:TSrcAdr);
var
 node:TsrCFGBlock;
 next:TsrCFGBlock;
begin
 node:=_parent.First;
 While (node<>nil) do
 begin
  next:=node.pNext;
  if (node.beg_adr.get_code_ptr>=b_adr.get_code_ptr) then
  if (node.end_adr.get_code_ptr<=e_adr.get_code_ptr) then
  begin
   _parent.FList.Remove(node);
   Self.FList.Push_tail(node);
   node.pParent:=Self;
  end;
  node:=next;
 end;
end;

function TsrCFGBlock.ExistByRange(b_adr,e_adr:TSrcAdr):Boolean;
var
 node:TsrCFGBlock;
begin
 Result:=False;
 node:=First;
 While (node<>nil) do
 begin
  if (node.beg_adr.get_code_ptr>=b_adr.get_code_ptr) then
  if (node.end_adr.get_code_ptr<=e_adr.get_code_ptr) then
  begin
   Exit(True);
  end;
  node:=node.pNext;
 end;
end;

function TsrCFGBlock.get_level:DWORD;
var
 node:TsrCFGBlock;
begin
 node:=Self;
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

procedure TsrCFGParser.AddUnknowGoto(goto_stmt,next_stmt,label_stmt:TsrLabel;cond:TsrCondition);
var
 pCond:TsrStatement;
 pGoto:TsrStatement;

 G:TsrUnknowGoto;
begin
 goto_stmt.AddType(ltGoto);

 pCond:=pCode.NewCond(cond);
 pGoto:=pCode.NewGoto(goto_stmt,next_stmt,pCond);

 G:=pCode.FEmit.specialize New<TsrUnknowGoto>;
 G.goto_stmt :=pGoto;
 G.label_stmt:=label_stmt;

 FUnknowGotoList.Push_tail(G);
end;

procedure TsrCFGParser._print_label(Adr:TSrcAdr);
var
 pLabel:TsrLabel;
begin
 pLabel:=pCode.FindLabel(Adr);
 if (pLabel<>nil) then
 begin
  Write('label_',HexStr(Adr.Offdw*4,4),': ;');

  if (ltUnknow    in pLabel.lType) then Write('ltUnknow ');
  if (ltGoto      in pLabel.lType) then Write('ltGoto ');
  if (ltContinue  in pLabel.lType) then Write('ltContinue ');
  if (ltBreak     in pLabel.lType) then Write('ltBreak ');

  writeln;
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

Function RevOrder(pBlock:TsrCFGBlock):RawByteString;
begin
 Result:='';
 while (pBlock<>nil) do
 begin
  if isReal(pBlock.bType) then
  begin
   Result:=Base64(pBlock.order)+Result;
  end;
  pBlock:=pBlock.pParent;
 end;
 Result:=' '+Result+' ';
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
  'Execnz'
 );

function GetCondStr(node:TsrStatement):RawByteString;
begin
 Result:='';
 while (node<>nil) do
 begin
  case node.sType of
   sCond :begin Result:=Result+CondStr[node.u.cond]; Break; end;
   //sGoto :;
   sVar  :begin Result:=Result+'V'+IntToStr(node.u.id); Break; end;
   //sStore:;
   sLoad :begin Result:=Result+'L'+IntToStr(node.u.id); Break; end;
   //sBreak:;
   sNot  :begin Result:='!'+Result; node:=node.pSrc; end;
   sOr   :begin Result:=Result+'('+GetCondStr(node.pSrc)+' || '+GetCondStr(node.pDst)+')'; Break; end;
   sAnd  :begin Result:=Result+'('+GetCondStr(node.pSrc)+' && '+GetCondStr(node.pDst)+')'; Break; end;
   else   Break;
  end;
 end;
end;

function GetStatment(node:TsrStatement):RawByteString;
begin
 Result:='';
 case node.sType of
  //sCond :'
  //sGoto :;
  //sVar  :'
  sStore:Result:=GetCondStr(node.pDst)+' = '+GetCondStr(node.pSrc);
  sLoad :Result:=GetCondStr(node)     +' = '+GetCondStr(node.pSrc);
  sBreak:Result:='break';
  //sNot:;
  //sOr:;
  //sAnd:;
  else;
 end;
end;

procedure PrintStatmentList(const prefix:RawByteString;List:TsrStatementList);
var
 node:TsrStatement;
begin
 node:=List.pHead;
 while (node<>nil) do
 begin
  case node.sType of
   sCond:; //skip
   sNot :; //skip
   sOr  :; //skip
   sAnd :; //skip
   else
    Writeln(prefix,GetStatment(node)+';');
  end;
  //
  node:=node.pNext;
 end;
end;

procedure PrintDown(const prefix:RawByteString;node:TsrCFGBlock);
begin
 case node.bType of
  btCond:Writeln(prefix,'if (',GetCondStr(node.pCond),') {');
  btLoop:Writeln(prefix,'do {');
  else;
 end;
end;

procedure PrintUp(const prefix:RawByteString;node:TsrCFGBlock);
begin
 case node.bType of
  btCond:begin
          if (node.pElse=nil) then
          begin
           Writeln(prefix,'};');
          end else
          begin
           Writeln(prefix,'} else {');
          end;
         end;
  btElse:Writeln(prefix,'};');
  btLoop:begin
          if (node.pCond=nil) then
          begin
           Writeln(prefix,' break;');
           Writeln(prefix,'} while (true);');
          end else
          begin
           Writeln(prefix,'} while (',GetCondStr(node.pCond),');');
          end;
         end;
  else;
 end;
end;

function TsrCFGParser.get_str_prefix(node:TsrCFGBlock;adr:TSrcAdr):RawByteString;
const
 print_block_id=False;
begin
 if print_block_id then
 begin
  Result:=' '+HexStr(adr.Offdw*4,3)+RevOrder(node);
 end else
 begin
  Result:=' '+HexStr(adr.Offdw*4,3)+Space(node.get_level);
 end;
end;

procedure TsrCFGParser.Print();
const
 print_block_id=False;
var
 prefix:RawByteString;
 node,next:TsrCFGBlock;
 Adr:TSrcAdr;
 i:Integer;
begin
 FCursor.Init(TsrLabelBlock(pCode));
 pBlock:=pCode.FTop;

 prefix:=get_str_prefix(pBlock,FCursor.Adr);

 PrintStatmentList(prefix,pBlock.FBefore);
 PrintStatmentList(prefix,pBlock.FStart);

 repeat
  Adr:=FCursor.Adr;

  //pBlock:=pBlock.DownBlock(Adr);

  node:=pBlock;
  repeat
   next:=node.FindBlock(Adr);
   if (next=nil) then Break;
   //
   prefix:=get_str_prefix(node,Adr);

   PrintStatmentList(prefix,next.FBefore);
   PrintDown(prefix,next);
   //
   prefix:=get_str_prefix(next,Adr);

   PrintStatmentList(prefix,next.FStart);
   //
   node:=next;
  until false;
  pBlock:=node;

  _print_label(Adr);

  prefix:=get_str_prefix(pBlock,Adr);
  Write(prefix);

  FSPI:=Default(TSPI);
  i:=FCursor.Next(FSPI);
  Case i of
   0,1:begin

        Case FSPI.CMD.EN of
         W_SOP1:if emit_SOP1 then i:=1;
        end;

        print_spi(FSPI);

        Adr:=FCursor.Adr;

        //pBlock:=pBlock.UpBlock(Adr);

        node:=pBlock;
        While (node.IsEndOf(Adr)) do
        begin
         next:=node.pParent;
         if (next=nil) then Break;
         //
         prefix:=get_str_prefix(node,Adr);

         PrintStatmentList(prefix,node.FEnded);
         //
         prefix:=get_str_prefix(next,Adr);

         PrintUp(prefix,node);
         PrintStatmentList(prefix,node.FAfter);
         //
         node:=next;
        end;
        pBlock:=node;

       end;
  end;

 until (i<>0);
 _print_label(Adr);
end;

function TsrCFGParser.Parse():Integer;
begin
 if (pCode=nil) then Exit(4);
 FCursor.Init(TsrLabelBlock(pCode));
 pBlock:=pCode.FTop;
 pCode.FTop.pBLabel:=pCode.FetchLabel(FCursor.Adr);
 repeat
  Result:=NextParse;
  Case Result of
   0:;
   1:Break;
   else
     Break;
  end;
 until false;
 //
 ExecPass;
 //
 GotoPass;
end;

function GetExecFlow(var FSPI:TSPI):TsrCondition;
begin
 Case FSPI.CMD.EN of
  W_VOP1  :
    Case FSPI.VOP1.OP of
     V_NOP              :Result:=cNone;
     V_READFIRSTLANE_B32:Result:=cFalse;
     else
                         Result:=cTrue;
    end;
  W_VOPC  :Result:=cTrue;
  W_VOP3  :
    Case FSPI.VOP3a.OP of
     384+V_NOP:Result:=cNone;
     else
               Result:=cTrue;
    end;
  W_DS    :
    Case FSPI.DS.OP of
     DS_NOP:Result:=cNone;
     else
            Result:=cTrue;
    end;
  W_MUBUF :Result:=cTrue;
  W_MTBUF :Result:=cTrue;
  //W_EXP  exp used separately
  W_MIMG  :Result:=cTrue;
  W_VOP2  :
    Case FSPI.VOP2.OP of
     V_READLANE_B32 :Result:=cFalse;
     V_WRITELANE_B32:Result:=cFalse;
     else
                     Result:=cTrue;
    end;
  W_SOPP  :
    Case FSPI.SOPP.OP of
     S_NOP    :Result:=cNone;
     S_WAITCNT:Result:=cNone;
     else
               Result:=cFalse;
    end;
  else
    Result:=cFalse;
 end;
end;

function TsrCFGParser.FindUpCond(cond:TsrCondition;Adr:TSrcAdr):TsrCFGBlock;
var
 node:TsrCFGBlock;
begin
 Result:=nil;
 node:=pBlock;
 While (node<>nil) do
 begin
  if (node.beg_adr.get_code_ptr<>Adr.get_code_ptr) then
  begin
   //early exit
   Exit(nil);
  end;
  //
  if (node.bType=btCond) then
  if (node.pCond<>nil) then
  if (node.pCond.sType=sCond) then
  if (node.pCond.u.cond=cond) then
  begin
   Exit(node);
  end;
  //
  node:=node.pParent;
 end;
end;

procedure TsrCFGParser.OpenExec;
var
 node:TsrCFGBlock;
begin
 if (pBlock.bType=btExec) then
 begin
  //is opened
  Exit;
 end;

 node:=NewBlock;
 node.pBLabel:=pCode.FetchLabel(FCursor.prev_adr);
 node.bType:=btExec;
 PushBlock(node);

 if (FindUpCond(cExecnz,FCursor.prev_adr)<>nil) then
 begin
  //nested cond
  node.pCond:=nil;
 end else
 begin
  node.pCond:=pCode.NewCond(cExecnz);
 end;
end;

procedure TsrCFGParser.CloseExec(Before:Boolean);
var
 node:TsrCFGBlock;
begin
 if (pBlock.bType<>btExec) then
 begin
  //is not opened
  Exit;
 end;

 node:=pBlock;

 if Before then
 begin
  node.pELabel:=pCode.FetchLabel(FCursor.prev_adr);
 end else
 begin
  node.pELabel:=pCode.FetchLabel(FCursor.Adr);
 end;

 PopBlock;
end;

procedure TsrCFGParser.ExecConvert(node:TsrCFGBlock);
var
 parent :TsrCFGBlock;
 if_stmt:TsrCFGBlock;
 if_merg:TsrCFGBlock;
begin
 parent:=node.pParent;
 if_stmt:=node;

 if (if_stmt.pCond=nil) then
 begin
  //nested cond
  if_stmt.InsertAfterTo(parent,if_stmt,if_stmt.First,nil);
  parent.FList.Remove(if_stmt);
  Exit;
 end;

 if_stmt.bType:=btCond;

 if_merg:=NewBlock;
 if_merg.pBLabel:=if_stmt.pBLabel;
 if_merg.pELabel:=if_stmt.pELabel;
 if_merg.bType:=btMerg;

 parent.Insert_after(if_stmt,if_merg);
 parent.MoveTo(if_merg,if_stmt,if_stmt);
end;

procedure TsrCFGParser.ExecDivide(Adr:TSrcAdr);
var
 parent:TsrCFGBlock;
 node  :TsrCFGBlock;
 new   :TsrCFGBlock;
 pLabel:TsrLabel;
begin
 node:=pCode.FTop.DownBlock(adr);
 if (node<>nil) then
 if (node.bType=btExec) then
 if (node.beg_adr.get_code_ptr<>Adr.get_code_ptr) and
    (node.end_adr.get_code_ptr<>Adr.get_code_ptr) then
 begin
  parent:=node.pParent;
  pLabel:=pCode.FetchLabel(Adr);

  //[B..C]
  node.pELabel:=pLabel;

  //[C..E]
  new:=NewBlock;
  new.pBLabel:=pLabel;
  new.pELabel:=node.pELabel;
  new.bType:=btExec;

  if (node.pCond<>nil) then
  begin
   new.pCond:=pCode.NewCond(cExecnz);
  end;

  parent.Insert_after(node,new);

  if (pBlock=node) then
  begin
   //change current
   pBlock:=new;
  end;

 end;
end;

function flow_down_next_up(node:TsrCFGBlock):TsrCFGBlock;
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

procedure TsrCFGParser.ExecPass;
var
 node,prev:TsrCFGBlock;
begin
 if (pCode.FTop=nil) then Exit;
 node:=pCode.FTop.First;
 While (node<>nil) do
 begin
  //test
  Assert(node.beg_adr.get_code_ptr<=node.end_adr.get_code_ptr,'WTF');

  Assert(node.pBLabel<>nil,'WTF');
  Assert(node.pELabel<>nil,'WTF');

  if (node.bType=btExec) then
  begin
   ExecConvert(node);
  end;

  prev:=node;
  node:=flow_down_next_up(node);

  //test
  if (node<>nil) then
  begin
   if (node.pParent<>prev) then
   begin
    Assert(prev.end_adr.get_code_ptr<=node.beg_adr.get_code_ptr,'WTF');
   end;
  end;

 end;
end;

function TsrCFGParser.NextParse:Integer;
begin
 FSPI:=Default(TSPI);
 //
 Result:=FCursor.Next(FSPI);
 //
 Case Result of
  0,1:;
  else Exit;
 end;
 //
 case GetExecFlow(FSPI) of
  cFalse:CloseExec(True);
  cTrue :OpenExec;
  else;
 end;
 //
 Case FSPI.CMD.EN of
  W_SOP1:if emit_SOP1 then Result:=1;
  W_SOPP:if emit_SOPP then Result:=1;
 end;
 //
 While (CheckBlockEnd) do;
 //
 if (Result=1) then
 begin
  Finalize;
 end;
end;

Procedure TsrCFGParser.Finalize;
begin
 pCode.FTop.pELabel:=pCode.FetchLabel(FCursor.Adr);
 pCode.Size:=FCursor.OFFSET_DW*4;
end;

function TsrCFGParser.CheckBlockEnd:Boolean;
var
 node:TsrCFGBlock;
begin
 Result:=False;
 if (pBlock=nil) then Exit;
 if (pBlock.pParent=nil) then Exit;

 node:=pBlock;

 if (node.bType=btExec) then
 begin
  node:=node.pParent;
 end;

 Result:=node.IsEndOf(FCursor.Adr);

 if Result then
 begin
  CloseExec(False);
  Assert(node.bType<>btExec);

  PopBlock;
 end;
end;

function TsrCFGParser.emit_SOP1:Boolean;
begin
 Result:=False;
 Case FSPI.SOP1.OP of
  S_SETPC_B64:
   if (pBlock.bType=btSetpc) then
   begin
    Result:=True;
   end;
 end;
end;

function TsrCFGParser.emit_SOPP:Boolean;
begin
 Result:=False;
 Case FSPI.SOPP.OP of

  S_ENDPGM:;

  S_CBRANCH_SCC0  :emit_S_BRANCH(cScc0);
  S_CBRANCH_SCC1  :emit_S_BRANCH(cScc1);
  S_CBRANCH_VCCZ  :emit_S_BRANCH(cVccz);
  S_CBRANCH_VCCNZ :emit_S_BRANCH(cVccnz);
  S_CBRANCH_EXECZ :emit_S_BRANCH(cExecz);
  S_CBRANCH_EXECNZ:emit_S_BRANCH(cExecnz);
  S_BRANCH        :emit_S_BRANCH(cTrue);

  else;
 end;
end;

Function TsrCFGParser.NewBlock:TsrCFGBlock;
begin
 Inc(order);
 Result:=pCode.FEmit.specialize New<TsrCFGBlock>;
 Result.order:=order;
end;

Procedure TsrCFGParser.PushBlock(New:TsrCFGBlock);
begin
 if (New=nil) then Exit;

 Assert(pBlock.bType<>btExec);

 pBlock.FList.Push_tail(New);
 New.pParent:=pBlock;
 pBlock:=New;
end;

function TsrCFGParser.PopBlock:Boolean;
begin
 Result:=False;
 if (pBlock=nil) then Exit;
 if (pBlock.pParent=nil) then Exit;
 pBlock:=pBlock.pParent;
 Result:=True;
end;

function TsrCFGParser.FindUpperLoop(branch_adr:TSrcAdr;var r_child,r_parent:TsrCFGBlock):Integer;
var
 child,parent,next:TsrCFGBlock;
begin
 Result:=0;
 r_child :=nil;
 r_parent:=nil;

 child :=pBlock.FList.pTail;
 parent:=pBlock;

 repeat

  if parent.IsInside(branch_adr) then
  begin
   next:=child;
   while (child<>nil) do
   begin
    if (parent.bType=btLoop) then
    if (not child.EndFixed) then
    if (child.pCond=nil) then //no post conditions
    begin
     //is exist loop
     if (child.beg_adr.get_code_ptr=branch_adr.get_code_ptr) then
     begin
      //case 3
      r_child :=child;
      r_parent:=parent;
      Exit(3);
     end;
    end;

    if (child.end_adr.get_code_ptr<=branch_adr.get_code_ptr) then
    begin
     //restore
     child:=next;
     Break;
    end;

    //
    next:=child;
    child:=child.pPrev;
   end;
   //restore
   child:=next;

   if (child<>nil) then
   begin
    if child.IsInside(branch_adr) then
    begin
     Exit(0);
    end;

    if (child.end_adr.get_code_ptr<=branch_adr.get_code_ptr) then
    begin
     //no child
     child:=nil;
    end;
   end;

   //case 2
   r_child :=child;
   r_parent:=parent;
   Exit(2);
  end else //IsInside
  if (parent.bType=btLoop) then
  begin
   //is exist loop
   if (parent.beg_adr.get_code_ptr=branch_adr.get_code_ptr) then
   begin
    //case 1
    r_parent:=parent;
    Exit(1);
   end else
   begin
    Exit(0);
   end;
  end;

  if (parent.beg_adr.get_code_ptr<=branch_adr.get_code_ptr) then Break;

  if (parent.pParent=nil) then Break;
  child :=parent;
  parent:=parent.pParent;
 until false;
end;

function TsrCFGParser.FindLowerLoop(branch_adr:TSrcAdr;var r_child,r_parent:TsrCFGBlock):Integer;
var
 child,parent:TsrCFGBlock;
begin
 Result:=0;
 r_child :=nil;
 r_parent:=nil;

 child :=nil;
 parent:=pBlock;

 repeat

  if parent.IsInside(branch_adr) or
     (parent.end_adr.get_code_ptr=branch_adr.get_code_ptr) then
  begin
   //case 2
   r_child :=child;
   r_parent:=parent;
   Exit(2);
  end else
  if (parent.bType=btLoop) then
  begin
   //is exist loop
   if (parent.end_adr.get_code_ptr=branch_adr.get_code_ptr) then
   begin
    //case 1
    r_parent:=parent;
    Exit(1);
   end else
   begin
    Exit(0);
   end;
  end;

  if (parent.end_adr.get_code_ptr>branch_adr.get_code_ptr) then Break;

  if (parent.pParent=nil) then Break;
  child :=parent;
  parent:=parent.pParent;
 until false;
end;

procedure UpdateUp(node:TsrCFGBlock);
var
 child:TsrCFGBlock;
 updateB,updateE:Boolean;
begin
 while (node<>nil) do
 begin

  case node.bType of
   btCond:;
   btElse:;
   btLoop:;
   btMerg:;
   else
    Break;
  end;

  updateB:=False;
  updateE:=False;

  //update begin label
  child:=node.First;
  if (child<>nil) then
  begin
   if (node.pBLabel=nil) then
   begin
    updateB:=True;
   end else
   if (node.pBLabel.Adr.get_code_ptr>child.pBLabel.Adr.get_code_ptr) then
   begin
    updateB:=True;
   end;
   //
   if updateB then
   begin
    node.pBLabel:=child.pBLabel;
   end;
  end;

  //update end label
  child:=node.FList.pTail;
  if (child<>nil) then
  begin
   if (node.pELabel=nil) then
   begin
    updateE:=True;
   end else
   if (node.pELabel.Adr.get_code_ptr<child.pELabel.Adr.get_code_ptr) then
   begin
    updateE:=True;
   end;
   //
   if updateE then
   begin
    node.pELabel:=child.pELabel;
   end;
  end;

  if (updateB=False) and
     (updateE=False) then
  begin
   Break;
  end;

  //
  node:=node.pParent;
 end;
end;

procedure TsrCFGParser.emit_S_BRANCH(cond:TsrCondition);
var
 pPrev,pNext,pBranch:TsrLabel;
 c_adr,b_adr:TSrcAdr;
 node,child,parent:TsrCFGBlock;
 prevLabel:TsrLabelType;
begin

 c_adr:=FCursor.Adr;
 b_adr:=c_adr;
 b_adr.Offdw:=get_branch_offset(FSPI);

 pNext  :=pCode.FetchLabel(c_adr);
 pBranch:=pCode.FetchLabel(b_adr);
 //pPrev:=pCode.FetchLabel(FCursor.prev_adr);

 ExecDivide(b_adr);

 {
 //test
 begin
  pPrev:=pCode.FetchLabel(FCursor.prev_adr);

  AddUnknowGoto(pPrev,pNext,pBranch,cond);
  Exit;
 end;
 }

 prevLabel:=ltUnknow;

 if (SmallInt(FSPI.SOPP.SIMM)<0) then //up
 begin

{

   [case:1]
   while () do<-\
   {            |
    if (eval)   |
    {           |
     continue---/
    }
   }

   [case:2]
   <------------\
                |
   if (eval)    |
   {            |
   //do         |
   }            |
                |
   if (eval)    |
   {            |
    continue----/
   }

   [case:3]
   while () do<-\
   {            |
   //do         |
   }            |
                |
   if (eval)    |
   {            |
   //do         |
   }            |
                |
   if (eval)    |
   {            |
    continue----/
   }

}


  parent:=nil;
  child :=nil;
  case FindUpperLoop(b_adr,child,parent) of
   1:
     begin
      prevLabel:=ltContinue;
     end;
   2:
     begin
      //new loop block

      prevLabel:=ltContinue;

      pNext.RemType(ltUnknow);

      node:=NewBlock;
      node.pBLabel:=pBranch;
      node.pELabel:=pNext;
      node.bType:=btLoop;

      parent.Push_back(node,child);

      //update end label
      UpdateUp(node);

      //update block
      pBlock:=pCode.FTop.DownBlock(FCursor.prev_adr);

     end;
   3:
     begin
      //move to loop block

      prevLabel:=ltContinue;

      node:=child; //loop
      child:=node.pNext;

      parent.MoveTo(node,child,nil);

      //update end label
      UpdateUp(node);

      //update block
      pBlock:=pCode.FTop.DownBlock(FCursor.prev_adr);

     end;
   else
     begin
      //Assert(False,'AddUnknowGoto');

      pPrev:=pCode.FetchLabel(FCursor.prev_adr);

      AddUnknowGoto(pPrev,pNext,pBranch,cond);
      Exit;
     end;
  end;

 end else //down
 begin

{
  [case:0]
  if (eval)
  {
  //do
  }

  [case:1]
  while () do
  {
   if (eval)
   {
    break------\
   }           |
  }<-----------/

  [case:2]
  if (eval)
  {
   break-------\
  }            |
               |
  <------------/

}

  if (pBlock.end_adr.get_code_ptr<b_adr.get_code_ptr) then
  begin
   //loop end

   parent:=nil;
   child :=nil;
   case FindLowerLoop(b_adr,child,parent) of
    1:
      begin
       prevLabel:=ltBreak;

       parent.EndFixed:=True;
      end;
    2:
      begin

       {
        if (eval)
        {
         ......
         branch------\
        } else {     |
         ......      |
        }<-----------/
       }

       if (cond=cTrue)  and          //only unconditional branch
          (child.bType=btMerg) and   //is merge block
          (child=pBlock.pParent) and //is current merge block
          (pBlock.end_adr.get_code_ptr=c_adr.get_code_ptr) then //is ended this
       begin
        //new else block

        //save block
        child :=pBlock;
        parent:=pBlock.pParent;

        //{} else {}

        //add else block to merge block
        node:=NewBlock;
        node.pBLabel:=pNext;
        node.pELabel:=pBranch;
        node.bType:=btElse;
        parent.Push_back(node);

        //update end label
        UpdateUp(parent);

        //set current block to else
        pBlock:=node;

        //set links
        child.pElse:=node;  //if->else
        node .pIf  :=child; //else->if

       end else
       begin
        //new loop block

        prevLabel:=ltBreak;

        node:=NewBlock;
        node.pBLabel:=pNext;
        node.pELabel:=pBranch;
        node.bType:=btLoop;
        node.EndFixed:=True;

        parent.Push_back(node,child);

        //update begin label
        UpdateUp(node);

        //update block
        pBlock:=pCode.FTop.DownBlock(FCursor.prev_adr);
       end;

      end;
    else
      begin
       //Assert(False,'AddUnknowGoto');

       pPrev:=pCode.FetchLabel(FCursor.prev_adr);

       AddUnknowGoto(pPrev,pNext,pBranch,cond);
       Exit;
      end;
   end;

  end else
  begin
   //add merge block
   node:=NewBlock;
   node.pBLabel:=pNext;
   node.pELabel:=pBranch;
   node.bType:=btMerg;
   PushBlock(node);

   //if (eval) {}

   node:=NewBlock;
   node.pBLabel:=pNext;
   node.pELabel:=pBranch;
   node.bType:=btCond;
   PushBlock(node);

   //set cond to block
   if (cond<>cTrue) then
   begin
    //The instruction specifies which block to skip, so need to invert the condition!
    node.pCond:=pCode.NewCond(InvertCond[cond]);

    //mark used
    pPrev:=pCode.FetchLabel(FCursor.prev_adr);
    pPrev.AddType(ltGoto);
   end;

  end;

 end; //if (SmallInt(FSPI.SOPP.SIMM)<0) then

 if (prevLabel<>ltUnknow) then
 begin

  pPrev:=pCode.FetchLabel(FCursor.prev_adr);
  pPrev.AddType(prevLabel);

  //add extra block
  if (cond<>cTrue) then
  begin
   //add merge block
   node:=NewBlock;
   node.pBLabel:=pPrev; //prev_adr
   node.pELabel:=pNext; //c_adr
   node.bType:=btMerg;
   PushBlock(node);

   //if (eval) {}

   node:=NewBlock;
   node.pBLabel:=pPrev;
   node.pELabel:=pNext;
   node.bType:=btCond;
   PushBlock(node);
  end;
 end;

end;

procedure TsrCFGParser.GotoPass;
var
 node:TsrUnknowGoto;
begin
 node:=FUnknowGotoList.pHead;
 while (node<>nil) do
 begin
  //Print();
  //
  RemoveGoto(node.goto_stmt,node.label_stmt);
  //
  node:=node.pNext;
 end;
end;

function TsrCFGParser.BlockOf(stmt:TsrLabel):TsrCFGBlock;
begin
 Result:=pCode.FTop.DownBlock(stmt.Adr);
end;

function TsrCFGParser.LevelOf(stmt:TsrLabel):DWORD;
begin
 Result:=BlockOf(stmt).get_level;
end;

function TsrCFGParser.IsDirectlyRelated(goto_stmt,label_stmt:TsrLabel):Boolean;
var
 min_level  :DWORD;
 max_level  :DWORD;
 l          :DWORD;
 min_up:TsrCFGBlock;
 max_up:TsrCFGBlock;
 m     :TsrCFGBlock;
begin
 min_up:=BlockOf(goto_stmt);
 max_up:=BlockOf(label_stmt);

 min_level:=min_up.get_level;
 max_level:=max_up.get_level;

 if (max_level < min_level) then
 begin
  m     :=min_up;
  min_up:=max_up;
  max_up:=m;
  //
  l        :=min_level;
  min_level:=max_level;
  max_level:=l;
 end;

 while (max_up<>nil) do
 begin
  if (min_up=max_up) then
  begin
   Exit(True);
  end;
  max_up:=max_up.pParent.pReal;
 end;

 Result:=False;
end;

function TsrCFGParser.IsIndirectlyRelated(goto_stmt,label_stmt:TsrLabel):Boolean;
begin
 Result:=(BlockOf(goto_stmt) <> BlockOf(label_stmt)) and
         (not IsDirectlyRelated(goto_stmt,label_stmt));
end;

function TsrCFGParser.AreSiblings(stmt:TsrCFGBlock;label_stmt:TsrLabel):Boolean;
var
 pParent1:TsrCFGBlock;
 pParent2:TsrCFGBlock;
begin
 pParent1:=stmt.pParent.pReal;
 pParent2:=BlockOf(label_stmt);
 //
 Result:=(pParent1=pParent2);
end;

function TsrCFGParser.AreSiblings(stmt,label_stmt:TsrLabel):Boolean;
begin
 Result:=BlockOf(stmt)=BlockOf(label_stmt);
end;

function TsrCFGParser.SiblingFromNephew(uncle,nephew:TsrLabel):TsrCFGBlock;
var
 parent:TsrCFGBlock;
 it_up :TsrCFGBlock;
 it    :TsrCFGBlock;
begin
 parent:=BlockOf(uncle);
 it_up :=BlockOf(nephew);
 it    :=it_up;

 while (it_up<>parent) do
 begin
  it   :=it_up;
  it_up:=it.pParent.pReal;
 end;

 Result:=it;
end;

function TsrCFGParser.AreOrdered(left_sibling:TsrCFGBlock;right_sibling:TsrLabel):Boolean;
begin
 if AreSiblings(left_sibling,right_sibling) then
 begin
  Result:=right_sibling.Adr.get_code_ptr>left_sibling.beg_adr.get_code_ptr;
 end else
 begin
  Result:=False;
 end;
end;

function TsrCFGParser.AreOrdered(left_sibling,right_sibling:TsrLabel):Boolean;
begin
 if AreSiblings(left_sibling,right_sibling) then
 begin
  Result:=right_sibling.Adr.get_code_ptr>left_sibling.Adr.get_code_ptr;
 end else
 begin
  Result:=False;
 end;
end;

function TsrCFGParser.NeedsLift(goto_stmt,label_stmt:TsrLabel):Boolean;
var
 sibling:TsrCFGBlock;
begin
 sibling:=SiblingFromNephew(goto_stmt,label_stmt);
 Result:=AreOrdered(sibling,goto_stmt);
end;

procedure TsrCFGParser.RemoveGoto(goto_stmt:TsrStatement;label_stmt:TsrLabel);
label
 _rep;

var
 label_level:DWORD;
 goto_level :DWORD;
begin
 {
 Writeln('---');
 _print_label(goto_stmt.sLabel.Adr);
 _print_label(label_stmt.Adr);
 Writeln('---');
 }

 _rep:

 // Force goto_stmt and label_stmt to be directly related
 if IsIndirectlyRelated(goto_stmt.sLabel,label_stmt) then
 begin
  // Move goto_stmt out using outward-movement transformation until it becomes
  // directly related to label_stmt
  while (not IsDirectlyRelated(goto_stmt.sLabel, label_stmt)) do
  begin
   goto_stmt:=MoveOutward(goto_stmt,label_stmt);
  end;
 end;

 // Force goto_stmt and label_stmt to be siblings
 if IsDirectlyRelated(goto_stmt.sLabel, label_stmt) then
 begin
  label_level:=LevelOf(label_stmt);
  goto_level :=LevelOf(goto_stmt.sLabel);
  if (goto_level > label_level) then
  begin
   // Move goto_stmt out of its level using outward-movement transformations
   while (goto_level > label_level) do
   begin
    goto_stmt:=MoveOutward(goto_stmt,label_stmt);
    //Dec(goto_level);
    goto_level:=LevelOf(goto_stmt.sLabel);
   end;
  end else // Level(goto_stmt) < Level(label_stmt)
  begin
   if (NeedsLift(goto_stmt.sLabel, label_stmt)) then
   begin
    // Lift goto_stmt to above stmt containing label_stmt using goto-lifting
    // transformations
    goto_stmt:=Lift(goto_stmt,label_stmt);
    //update
    label_level:=LevelOf(label_stmt);
    goto_level :=LevelOf(goto_stmt.sLabel);
   end;
   // Move goto_stmt into label_stmt's level using inward-movement transformation
   while (goto_level < label_level) do
   begin
    goto_stmt:=MoveInward(goto_stmt,label_stmt);
    //Inc(goto_level);
    goto_level:=LevelOf(goto_stmt.sLabel);
   end;
  end;
 end;

 if (not AreSiblings(goto_stmt.sLabel, label_stmt)) then
 begin
  //Goto is not a sibling with the label
  goto _rep;
 end;

 // goto_stmt and label_stmt are guaranteed to be siblings, eliminate
 if (goto_stmt.sNext.Adr.get_code_ptr=label_stmt.Adr.get_code_ptr) then
 begin
  // Simply eliminate the goto if the label is next to it
 end else
 if AreOrdered(goto_stmt.sLabel, label_stmt) then
 begin
  // Eliminate goto_stmt with a conditional
  EliminateAsConditional(goto_stmt, label_stmt);
 end else
 begin
  // Eliminate goto_stmt with a loop
  EliminateAsLoop(goto_stmt, label_stmt);
 end;

end;

procedure TsrCFGParser.EliminateAsConditional(goto_stmt:TsrStatement;label_stmt:TsrLabel);
var
 parent  :TsrCFGBlock;
 cond    :TsrStatement;
 neg_cond:TsrStatement;
 if_stmt :TsrCFGBlock;
 if_merg :TsrCFGBlock;
 prev    :TsrCFGBlock;
begin
 parent:=BlockOf(goto_stmt.sLabel);

 cond    :=goto_stmt.pSrc;
 neg_cond:=pCode.NewNot(cond);

 if_stmt:=NewBlock;
 if_stmt.pBLabel:=goto_stmt.sNext;
 if_stmt.pELabel:=label_stmt;
 if_stmt.pCond:=neg_cond;
 if_stmt.bType:=btCond;
 if_stmt.MoveFromByRange(parent,if_stmt.beg_adr,if_stmt.end_adr);

 if_merg:=NewBlock;
 if_merg.pBLabel:=if_stmt.pBLabel;
 if_merg.pELabel:=if_stmt.pELabel;
 if_merg.bType:=btMerg;
 if_merg.Push_back(if_stmt);

 if_merg.FBefore.Push_tail(neg_cond);

 prev:=parent.FindPrev(if_merg.beg_adr);
 parent.Insert_after(prev,if_merg);

 //update labels
 UpdateUp(if_stmt);
end;

procedure TsrCFGParser.EliminateAsLoop(goto_stmt:TsrStatement;label_stmt:TsrLabel);
var
 parent   :TsrCFGBlock;
 cond     :TsrStatement;
 loop_stmt:TsrCFGBlock;
 prev     :TsrCFGBlock;
begin
 parent:=BlockOf(goto_stmt.sLabel);

 cond:=goto_stmt.pSrc;

 loop_stmt:=NewBlock;
 loop_stmt.pBLabel:=label_stmt;
 loop_stmt.pELabel:=goto_stmt.sLabel;
 loop_stmt.pCond:=cond;
 loop_stmt.bType:=btLoop;
 loop_stmt.MoveFromByRange(parent,loop_stmt.beg_adr,loop_stmt.end_adr);

 prev:=parent.FindPrev(loop_stmt.beg_adr);
 parent.Insert_after(prev,loop_stmt);

 //update labels
 UpdateUp(loop_stmt);
end;

function TsrCFGParser.MoveOutward(goto_stmt:TsrStatement;label_stmt:TsrLabel):TsrStatement;
var
 parent:TsrCFGBlock;
begin
 if (goto_stmt.sNext.Adr.get_code_ptr=label_stmt.Adr.get_code_ptr) then
 begin
  Exit(goto_stmt);
 end;

 parent:=BlockOf(goto_stmt.sLabel);

 case parent.bType of
  btCond:Result:=MoveOutwardIf  (goto_stmt,label_stmt);
  btElse:Result:=MoveOutwardIf  (goto_stmt,label_stmt);
  btLoop:Result:=MoveOutwardLoop(goto_stmt,label_stmt);
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

function TsrCFGParser.MoveOutwardIf(goto_stmt:TsrStatement;label_stmt:TsrLabel):TsrStatement;
var
 parent  :TsrCFGBlock;
 pmerge  :TsrCFGBlock;
 prev    :TsrCFGBlock;
 new_var :TsrStatement;
 set_var :TsrStatement;
 cond    :TsrStatement;
 neg_cond:TsrStatement;
 new_cond:TsrStatement;
 new_goto:TsrStatement;
 if_stmt :TsrCFGBlock;
 if_merg :TsrCFGBlock;
begin
 parent:=BlockOf(goto_stmt.sLabel);
 pmerge:=parent.pParent;
 Assert(pmerge<>nil);
 Assert(pmerge.bType=btMerg);

 cond:=goto_stmt.pSrc;

 new_var:=pCode.NewVar;

 //goto_L1 = false;
 set_var:=pCode.NewStore(new_var,pCode.NewCond(cFalse));
 pmerge.FBefore.Push_tail(set_var);

 //goto_L1 = cond;
 set_var:=pCode.NewStore(new_var,cond);

 if (goto_stmt.sNext.Adr.get_code_ptr=parent.pELabel.Adr.get_code_ptr) then
 begin
  //empty body

  //goto_L1 = cond;
  parent.FEnded.Push_tail(set_var);
 end else
 begin
  neg_cond:=pCode.NewNot(cond);

  if_stmt:=NewBlock;
  if_stmt.pBLabel:=goto_stmt.sNext;
  if_stmt.pELabel:=parent.pELabel;
  if_stmt.pCond:=neg_cond;
  if_stmt.bType:=btCond;
  if_stmt.MoveFromByRange(parent,if_stmt.beg_adr,if_stmt.end_adr);

  if_merg:=NewBlock;
  if_merg.pBLabel:=if_stmt.pBLabel;
  if_merg.pELabel:=if_stmt.pELabel;
  if_merg.bType:=btMerg;
  if_merg.Push_back(if_stmt);

  if_merg.FBefore.Push_tail(neg_cond);

  //goto_L1 = cond;
  if_merg.FBefore.Push_tail(set_var);

  prev:=parent.FindPrev(if_merg.beg_adr);
  parent.Insert_after(prev,if_merg);

  //update labels
  UpdateUp(if_stmt);
 end;

 new_cond:=pCode.NewLoad(new_var);
 pmerge.FAfter.Push_tail(new_cond);

 new_goto:=pCode.NewGoto(parent.pELabel,parent.pELabel,new_cond);

 Result:=new_goto;
end;

{
 ....
 do {
  stmt_1;
  ....
  if (cond) goto L1;
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
  goto_L1 = cond;
  if (!cond)
  {
   stmt_2;
   ....
   stmt_i;
   ....
   break;
  }
 } while (expr)
 if (goto_L1) goto L1;
 ....
 L1:
 stmt_n;
}

function TsrCFGParser.MoveOutwardLoop(goto_stmt:TsrStatement;label_stmt:TsrLabel):TsrStatement;
var
 parent   :TsrCFGBlock;
 prev     :TsrCFGBlock;
 new_var  :TsrStatement;
 set_var  :TsrStatement;
 cond     :TsrStatement;
 neg_cond :TsrStatement;
 new_cond :TsrStatement;
 new_break:TsrStatement;
 new_goto :TsrStatement;
 if_stmt  :TsrCFGBlock;
 if_merg  :TsrCFGBlock;
begin
 parent:=BlockOf(goto_stmt.sLabel);

 cond:=goto_stmt.pSrc;

 new_var:=pCode.NewVar;

 //goto_L1 = false;
 set_var:=pCode.NewStore(new_var,pCode.NewCond(cFalse));
 parent.FBefore.Push_tail(set_var);

 //goto_L1 = cond;
 set_var:=pCode.NewStore(new_var,cond);

 if (goto_stmt.sNext.Adr.get_code_ptr=parent.pELabel.Adr.get_code_ptr) then
 begin
  //empty body

  //goto_L1 = cond;
  parent.FEnded.Push_tail(set_var);
 end else
 begin
  neg_cond:=pCode.NewNot(cond);

  if_stmt:=NewBlock;
  if_stmt.pBLabel:=goto_stmt.sNext;
  if_stmt.pELabel:=parent.pELabel;
  if_stmt.pCond:=neg_cond;
  if_stmt.bType:=btCond;
  if_stmt.MoveFromByRange(parent,if_stmt.beg_adr,if_stmt.end_adr);

  if_merg:=NewBlock;
  if_merg.pBLabel:=if_stmt.pBLabel;
  if_merg.pELabel:=if_stmt.pELabel;
  if_merg.bType:=btMerg;
  if_merg.Push_back(if_stmt);

  if_merg.FBefore.Push_tail(neg_cond);

  //goto_L1 = cond;
  if_merg.FBefore.Push_tail(set_var);

  prev:=parent.FindPrev(if_merg.beg_adr);
  parent.Insert_after(prev,if_merg);

  //update labels
  UpdateUp(if_stmt);
 end;

 //break;
 new_break:=pCode.NewBreak(parent.pELabel);
 parent.FEnded.Push_tail(new_break);

 new_cond:=pCode.NewLoad(new_var);
 parent.FAfter.Push_tail(new_cond);

 new_goto:=pCode.NewGoto(parent.pELabel,parent.pELabel,new_cond);

 Result:=new_goto;
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
 if (!cond && expr) {
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
  goto_L2 = goto_L1;
  goto_L1 = false;
  if (goto_L2) goto L1;
  stmt_j;
  ...
  L1:
  ...
  stmt_n;
 } while (expr)
}

function TsrCFGParser.MoveInward(goto_stmt:TsrStatement;label_stmt:TsrLabel):TsrStatement;
var
 parent     :TsrCFGBlock;
 prev       :TsrCFGBlock;
 nested_stmt:TsrCFGBlock;
 nested_real:TsrCFGBlock;
 new_var    :TsrStatement;
 set_var    :TsrStatement;
 cond       :TsrStatement;
 neg_cond   :TsrStatement;
 new_op     :TsrStatement;
 new_goto   :TsrStatement;
 if_stmt    :TsrCFGBlock;
 if_merg    :TsrCFGBlock;
begin
 parent:=BlockOf(goto_stmt.sLabel);

 nested_stmt:=SiblingFromNephew(goto_stmt.sLabel,label_stmt);
 nested_real:=nested_stmt.pReal;

 new_var:=pCode.NewVar;
 cond   :=goto_stmt.pSrc;

 //goto_L1 = cond;
 set_var:=pCode.NewStore(new_var,cond);

 if (goto_stmt.sNext.Adr.get_code_ptr=parent.pELabel.Adr.get_code_ptr) then
 begin
  //empty body

  nested_real.FBefore.Push_tail(set_var);
 end else
 begin
  neg_cond:=pCode.NewNot(cond);

  if_stmt:=NewBlock;
  if_stmt.pBLabel:=goto_stmt.sNext;
  if_stmt.pELabel:=nested_stmt.pBLabel;
  if_stmt.pCond:=neg_cond;
  if_stmt.bType:=btCond;
  if_stmt.MoveFromByRange(parent,if_stmt.beg_adr,if_stmt.end_adr);

  if_merg:=NewBlock;
  if_merg.pBLabel:=if_stmt.pBLabel;
  if_merg.pELabel:=if_stmt.pELabel;
  if_merg.bType:=btMerg;
  if_merg.Push_back(if_stmt);

  if_merg.FBefore.Push_tail(set_var);
  if_merg.FBefore.Push_tail(neg_cond);

  prev:=parent.FindPrev(if_merg.beg_adr);
  parent.Insert_after(prev,if_merg);

  //update labels
  UpdateUp(if_stmt);
 end;

 case nested_stmt.bType of
  btCond:
    begin
     //load before
     cond:=pCode.NewLoad(new_var);
     //(goto_L1 || expr)
     Assert(nested_stmt.pCond<>nil);
     new_op:=pCode.NewOr(nested_stmt.pCond,cond);
     //
     nested_real.FBefore.Push_tail(cond);
     nested_real.FBefore.Push_tail(new_op);
     // Update nested if condition
     nested_stmt.pCond:=new_op;
    end;
  btElse:
    begin
     if_stmt:=nested_stmt.pIf;
     Assert(if_stmt<>nil);
     //load before
     cond:=pCode.NewLoad(new_var);
     //!cond
     neg_cond:=pCode.NewNot(cond);
     //(!cond && expr)
     Assert(nested_stmt.pCond<>nil);
     new_op:=pCode.NewAnd(nested_stmt.pCond,cond);
     //
     nested_real.FBefore.Push_tail(cond);
     nested_real.FBefore.Push_tail(neg_cond);
     nested_real.FBefore.Push_tail(new_op);
     // Update nested if condition
     nested_stmt.pCond:=new_op;
    end;
  btLoop:
    begin
     //goto_L2 = goto_L1;
     cond:=pCode.NewLoad(new_var);
     nested_stmt.FStart.Push_tail(cond);
     //goto_L1 = false;
     set_var:=pCode.NewStore(new_var,pCode.NewCond(cFalse));
     nested_stmt.FStart.Push_tail(set_var);
    end;
  else
   begin
    Writeln(stderr,'Invalid inward movement:',nested_stmt.bType);
    Assert(false,'Invalid inward movement');
   end;
 end;

 new_goto:=pCode.NewGoto(nested_stmt.pBLabel,nested_stmt.pBLabel,cond);

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

function TsrCFGParser.Lift(goto_stmt:TsrStatement;label_stmt:TsrLabel):TsrStatement;
var
 parent     :TsrCFGBlock;
 prev       :TsrCFGBlock;
 nested_stmt:TsrCFGBlock;
 new_var    :TsrStatement;
 cond       :TsrStatement;
 new_goto   :TsrStatement;
 set_var    :TsrStatement;
 loop_stmt  :TsrCFGBlock;
begin
 parent:=BlockOf(goto_stmt.sLabel);

 nested_stmt:=SiblingFromNephew(goto_stmt.sLabel,label_stmt);

 new_var:=pCode.NewVar;
 cond   :=goto_stmt.pSrc;

 loop_stmt:=NewBlock;
 loop_stmt.pBLabel:=nested_stmt.pBLabel;
 loop_stmt.pELabel:=goto_stmt.sLabel;
 loop_stmt.pCond:=cond;
 loop_stmt.bType:=btLoop;
 loop_stmt.MoveFromByRange(parent,loop_stmt.beg_adr,loop_stmt.end_adr);
 //TODO: SanitizeNoBreaks(loop_stmt);

 prev:=parent.FindPrev(loop_stmt.beg_adr);
 parent.Insert_after(prev,loop_stmt);

 //update labels
 UpdateUp(loop_stmt);

 cond    :=pCode.NewLoad(new_var);
 loop_stmt.FStart.Push_tail(cond);

 new_goto:=pCode.NewGoto(label_stmt,label_stmt,cond);

 // goto_L1 = cond;
 cond   :=goto_stmt.pSrc;
 set_var:=pCode.NewStore(new_var,cond);
 loop_stmt.FEnded.Push_tail(set_var);

 //goto_L1 = false;
 set_var:=pCode.NewStore(new_var,pCode.NewCond(cFalse));
 loop_stmt.FBefore.Push_tail(set_var);

 Result:=new_goto;
end;


end.

