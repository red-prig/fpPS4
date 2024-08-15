unit srCFGParser;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_pssl,
 ginodes,
 srNode,
 srCFGLabel;

type
 TsrCFGBlock=class;

 TsrCFGBlockList=specialize TNodeListClass<TsrCFGBlock>;

 TsrCFGBlock=class
  pParent,pPrev,pNext:TsrCFGBlock;

  FList:TsrCFGBlockList;

  pBLabel:TsrLabel;
  pELabel:TsrLabel;

  bType:TsrBlockType;

  term_id:Integer;

  function  IsEndOf(Adr:TSrcAdr):Boolean;
  function  IsBigOf(Adr:TSrcAdr):Boolean;
  function  IsContain(Adr:TSrcAdr):Boolean;
  function  FindBlock(Adr:TSrcAdr):TsrCFGBlock;
  function  DownBlock(Adr:TSrcAdr):TsrCFGBlock;
  function  UpBlock(Adr:TSrcAdr):TsrCFGBlock;
  function  FindUpLoop:TsrCFGBlock;
  Procedure InsertBlockList(New,child:TsrCFGBlock);
  function  get_level:DWORD;
 end;

 TsrCodeBlock=class(TsrLabelBlock)
  pNext:TsrCodeBlock;
  //----
  FTop:TsrCFGBlock;
 end;

 TsrCFGParser=object
  FCursor:TsrLCursor;
  pCode  :TsrCodeBlock;
  pBlock :TsrCFGBlock;
  FSPI:TSPI;
  procedure _print_label(Adr:TSrcAdr);
  procedure Print();
  function  Parse():Byte;
  function  NextParse:Byte;
  Procedure Finalize;
  function  CheckBlock:Boolean;
  Procedure CheckLabel;
  Procedure CheckTerm;
  function  emit_SOP1:Boolean;
  function  emit_SOPP:Boolean;
  Function  NewBlock:TsrCFGBlock;
  Procedure PushBlock(New:TsrCFGBlock);
  function  PopBlock:Boolean;
  procedure emit_S_BRANCH;
 end;

function parse_code_cfg(bType:TsrBlockType;pCode:TsrCodeBlock):Byte;

implementation

function parse_code_cfg(bType:TsrBlockType;pCode:TsrCodeBlock):Byte;
var
 LParser:TsrCFGParser;
begin
 pCode.FTop.bType:=bType;

 LParser:=Default(TsrCFGParser);
 LParser.pCode:=pCode;
 Result:=LParser.Parse();

 //LParser.Print();
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

function TsrCFGBlock.FindBlock(Adr:TSrcAdr):TsrCFGBlock;
var
 node:TsrCFGBlock;
begin
 Result:=nil;
 node:=FList.pHead;
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

Procedure TsrCFGBlock.InsertBlockList(New,child:TsrCFGBlock);
var
 next:TsrCFGBlock;
begin
 if (New=nil) then Exit;
 New.pParent:=Self;
 While (child<>nil) do
 begin
  Assert(child.pParent=Self);
  next:=child.pNext;
  FList.Remove(child);
  New.FList.Push_tail(child);
  child.pParent:=New;
  child:=next;
 end;
 FList.Push_tail(New);
end;

function TsrCFGBlock.get_level:DWORD;
var
 node:TsrCFGBlock;
begin
 node:=Self;
 Result:=0;
 While (node<>nil) do
 begin
  Inc(Result);
  node:=node.pParent;
 end;
end;

procedure TsrCFGParser._print_label(Adr:TSrcAdr);
var
 pLabel:TsrLabel;
begin
 pLabel:=pCode.FindLabel(Adr);
 if (pLabel<>nil) then
 begin
  Write('label_',HexStr(Adr.Offdw*4,4),': ;');

  if (ltBranch    in pLabel.lType) then Write('ltBranch ');
  if (ltUnknow    in pLabel.lType) then Write('ltUnknow ');
  if (ltBegAdr    in pLabel.lType) then Write('ltBegAdr ');
  if (ltEndAdr    in pLabel.lType) then Write('ltEndAdr ');
  if (ltBegCond   in pLabel.lType) then Write('ltBegCond ');
  if (ltEndCond   in pLabel.lType) then Write('ltEndCond ');
  if (ltBegLoop   in pLabel.lType) then Write('ltBegLoop ');
  if (ltEndLoop   in pLabel.lType) then Write('ltEndLoop ');

  writeln;
 end;
end;

procedure TsrCFGParser.Print();
var
 Adr:TSrcAdr;
 level:DWORD;
 i:Byte;
begin
 FCursor.Init(TsrLabelBlock(pCode));
 pBlock:=pCode.FTop;
 repeat
  Adr:=FCursor.Adr;

  pBlock:=pBlock.DownBlock(Adr);

  level:=pBlock.get_level;

  _print_label(Adr);

  Write(' ',HexStr(FCursor.OFFSET_DW*4,4));
  Write(Space(level));

  FSPI:=Default(TSPI);
  i:=FCursor.Next(FSPI);
  Case i of
   0,1:begin

        print_spi(FSPI);

        Adr:=FCursor.Adr;
        pBlock:=pBlock.UpBlock(Adr);

       end;
  end;

 until (i<>0);
 _print_label(Adr);
end;

function TsrCFGParser.Parse():Byte;
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
end;

function TsrCFGParser.NextParse:Byte;
begin
 FSPI:=Default(TSPI);
 Result:=FCursor.Next(FSPI);
 Case Result of
  0,1:begin
       Case FSPI.CMD.EN of
        W_SOP1:if emit_SOP1 then Result:=1;
        W_SOPP:if emit_SOPP then Result:=1;
       end;
       While (CheckBlock) do;
       if (Result=0) then
       begin
        CheckTerm;
       end;
       CheckLabel;
       if (Result=1) then
       begin
        Finalize;
       end;
      end;
 end;
end;

Procedure TsrCFGParser.Finalize;
begin
 pCode.FTop.pELabel:=pCode.FetchLabel(FCursor.Adr);
 pCode.Size:=FCursor.OFFSET_DW*4;
end;

function TsrCFGParser.CheckBlock:Boolean;
var
 pLabel:TsrLabel;
begin
 Result:=False;
 if (pBlock=nil) then Exit;
 Result:=pBlock.IsEndOf(FCursor.Adr);
 if Result then
 begin

  Case pBlock.bType of
   btAdr:
    begin
     pLabel:=pBlock.pELabel;
     pLabel.AddType(ltEndAdr);
    end;
   btLoop:
    begin
     pLabel:=pBlock.pELabel;
     pLabel.AddType(ltEndLoop);
    end;
   else;
  end;

  PopBlock;
 end;
end;

Procedure TsrCFGParser.CheckLabel;
var
 c_adr:TSrcAdr;
 pLabel:array[0..1] of TsrLabel;
 node,prev:TsrCFGBlock;
begin
 c_adr:=FCursor.Adr;
 pLabel[0]:=pCode.FindLabel(c_adr);
 if (pLabel[0]=nil) then Exit;

 if not (pLabel[0].IsType(ltUnknow)) then Exit;

 prev:=pBlock.FList.pTail;
 While (prev<>nil) do
 begin
  if prev.IsContain(c_adr) then Break;
  prev:=prev.pPrev;
 end;
 if (prev=nil) then Exit;

 if not (prev.IsContain(c_adr)) then Exit;

 pLabel[1]:=prev.pBLabel;

 pLabel[0].RemType(ltUnknow); //ltEndLoop later
 pLabel[1].AddType(ltBegLoop);

 node:=NewBlock;
 node.pBLabel:=pLabel[1];
 node.pELabel:=pLabel[0];
 node.bType:=btLoop;

 pBlock.InsertBlockList(node,prev);
end;

Procedure TsrCFGParser.CheckTerm;
var
 c_adr:TSrcAdr;
 pLabel:array[0..1] of TsrLabel;
 node,prev:TsrCFGBlock;
begin
 With pBlock do
 begin
  if (term_id>=0) then Exit;
  term_id:=Abs(term_id);
 end;

 c_adr:=FCursor.Adr;

 pLabel[0]:=pCode.FetchLabel(c_adr);
 pLabel[1]:=nil;
 pLabel[0].RemType(ltUnknow);
 pLabel[0].AddType(ltBegAdr);

 prev:=pBlock.pParent;
 if (prev<>nil) then
 begin
  pLabel[1]:=prev.pELabel;
 end;

 node:=NewBlock;
 node.pBLabel:=pLabel[0];
 node.pELabel:=pLabel[1];
 node.bType:=btAdr;
 PushBlock(node);
end;

function TsrCFGParser.emit_SOP1:Boolean;
begin
 Result:=False;
 Case FSPI.SOP1.OP of
  S_SETPC_B64:if (pBlock.bType=btSetpc) then Result:=True;
 end;
end;

function TsrCFGParser.emit_SOPP:Boolean;
var
 c_adr:TSrcAdr;
 pLabel:TsrLabel;
begin
 Result:=False;
 Case FSPI.SOPP.OP of

  S_ENDPGM:
   begin

    if (pBlock.bType=btAdr) then
    begin
     c_adr:=FCursor.Adr;
     pLabel:=pCode.FetchLabel(c_adr);
     pLabel.AddType(ltEndAdr);
     pBlock.pELabel:=pLabel;
     PopBlock;
    end;

    With pBlock do
    begin
     term_id:=-(Abs(term_id)+1);
    end;

   end;

  S_CBRANCH_SCC0  ,
  S_CBRANCH_SCC1  ,
  S_CBRANCH_VCCZ  ,
  S_CBRANCH_VCCNZ ,
  S_CBRANCH_EXECZ ,
  S_CBRANCH_EXECNZ,
  S_BRANCH        :emit_S_BRANCH;

  else;
 end;
end;

Function TsrCFGParser.NewBlock:TsrCFGBlock;
begin
 Result:=pCode.FEmit.specialize New<TsrCFGBlock>;
end;

Procedure TsrCFGParser.PushBlock(New:TsrCFGBlock);
begin
 if (New=nil) then Exit;
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

function GetBeginAdr(node:TsrCFGBlock):TSrcAdr;
begin
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

function GetEndAdr(node:TsrCFGBlock):TSrcAdr;
begin
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

procedure TsrCFGParser.emit_S_BRANCH;
var
 pLabel:array[0..1] of TsrLabel;
 c_adr,b_adr,t_adr:TSrcAdr;
 node,child,prev,parent:TsrCFGBlock;
begin
 c_adr:=FCursor.Adr;
 b_adr:=c_adr;
 b_adr.Offdw:=get_branch_offset(FSPI);

 pLabel[0]:=pCode.FetchLabel(c_adr);
 pLabel[1]:=pCode.FetchLabel(b_adr);
 pLabel[0].AddType(ltBranch);

 if (SmallInt(FSPI.SOPP.SIMM)<0) then //up
 begin

  child :=pBlock.FList.pTail;
  parent:=pBlock;

  repeat

   t_adr:=GetBeginAdr(parent);

   if (parent.bType=btLoop) and
      (t_adr.get_code_ptr=b_adr.get_code_ptr) then //is exist loop
   begin
    pLabel[0].RemType(ltUnknow); //ltEndLoop later
    if (parent.pELabel=nil) then
    begin
     parent.pELabel:=pLabel[0]; //set end
    end else
    begin
     t_adr:=parent.pELabel.Adr;
     if (t_adr.get_code_ptr<c_adr.get_code_ptr) then
     begin
      parent.pELabel:=pLabel[0]; //update end of loop
     end;
    end;
    Exit;
   end;

   if (t_adr.get_code_ptr<=b_adr.get_code_ptr) then Break;
   if (parent.pParent=nil) then Break;

   child :=parent;
   parent:=parent.pParent;
  until false;

  //up child for move
  if (child<>nil) then
  begin
   t_adr:=GetEndAdr(child);

   if (t_adr.get_code_ptr<=b_adr.get_code_ptr) then
   begin
    child:=nil;
   end else
   repeat
    prev:=child.pPrev;
    if (prev=nil) then Break;

    t_adr:=GetEndAdr(prev);

    if (t_adr.get_code_ptr<=b_adr.get_code_ptr) then Break;

    child:=prev;
   until false;

   if (child<>nil) then
   begin
    if (GetBeginAdr(child).get_code_ptr<b_adr.get_code_ptr) and
       (GetEndAdr  (child).get_code_ptr>b_adr.get_code_ptr) then
    begin
     Assert(false,'branch betwen condition');
    end;
   end;

  end;

  //new loop block

  pLabel[0].RemType(ltUnknow); //ltEndLoop later
  pLabel[1].AddType(ltBegLoop);

  node:=NewBlock;
  node.pBLabel:=pLabel[1];
  node.pELabel:=pLabel[0];
  node.bType:=btLoop;

  parent.InsertBlockList(node,child);

 end else //down
 begin

  if (GetEndAdr(pBlock).get_code_ptr<b_adr.get_code_ptr) then
  begin
   //push adr or loop end
   if not pLabel[1].IsType(ltBegAdr) then
   begin
    pLabel[1].AddType(ltUnknow);
   end;
   Exit;
  end;

  pLabel[0].AddType(ltBegCond);
  pLabel[1].AddType(ltEndCond);

  node:=NewBlock;
  node.pBLabel:=pLabel[0];
  node.pELabel:=pLabel[1];
  node.bType:=btCond;
  PushBlock(node);

 end;

end;

end.

