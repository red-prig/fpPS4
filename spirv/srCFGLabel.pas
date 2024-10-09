unit srCFGLabel;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_pssl,
 ginodes,
 srNode;

type
 TsrLabelBlock=class;

 PSrcAdr=^TSrcAdr;
 TSrcAdr=object
  pCode:TsrLabelBlock;
  Offdw:ptrint;
  function get_code_ptr:PDWORD;
  function get_dmem_ptr:PDWORD;
 end;

 TsrLCursor=object(TShaderParser)
  private
   pCode:TsrLabelBlock;
   function  get_src_adr:TSrcAdr;
   Procedure set_src_adr(src:TSrcAdr);
  public
   prev_adr:TSrcAdr;
   procedure Init(Code:TsrLabelBlock);
   property  Adr:TSrcAdr read get_src_adr write set_src_adr;
   Function  Next(Var SPI:TSPI):Integer;
 end;

 TsrLabelType=(ltUnknow,
               ltGoto,
               ltContinue,
               ltBreak);

 TsrSetLabelType=Set of TsrLabelType;

 TsrBlockType=(btMain,btSetpc,btCond,btElse,btLoop,btMerg,btExec,btInline,btOther);

 TsrCondition=(
  cNone,
  cFalse,
  cTrue,
  cScc0,
  cScc1,
  cVccz,
  cVccnz,
  cExecz,
  cExecnz
 );

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
  cExecz   //cExecnz
 );

type
 TsrLabel=class
  public
   pLeft,pRight:TsrLabel;
  private
   key:TSrcAdr;
  public
   lType:TsrSetLabelType;
   class function c(n1,n2:PSrcAdr):Integer; static;
   property  Adr:TSrcAdr read key;
   Procedure AddType(t:TsrLabelType);
   Procedure RemType(t:TsrLabelType);
   function  IsType (t:TsrLabelType):Boolean;
 end;

 TsrStatementType=(
  sCond,
  sGoto,
  sVar,
  sStore,
  sLoad,
  sBreak,
  sNot,
  sOr,
  sAnd
 );

 //sCond  TsrCondition
 //sGoto  [sLabel]:TsrLabel [sNext]:TsrLabel [cond]:sCond/sLoad/sNot/sOr/sAnd

 //sVar   id

 //sStore [var]:sVar [false]:sCond/sLoad/sNot/sOr/sAnd
 //sLoad  [var]:sVar

 //sBreak

 //sNot   [cond]:sCond/sLoad/sNot/sOr

 //sOr    [cond]:sCond/sLoad/sNot/sOr/sAnd [cond]:sCond/sLoad/sNot/sOr/sAnd
 //sAnd   [cond]:sCond/sLoad/sNot/sOr/sAnd [cond]:sCond/sLoad/sNot/sOr/sAnd

 TsrStatement=class
  pPrev :TsrStatement;
  pNext :TsrStatement;
  //
  sType :TsrStatementType;
  sLabel:TsrLabel;
  sNext :TsrLabel;
  pSrc  :TsrStatement;
  pDst  :TsrStatement;
  //
  pCache:TObject;
  u:record
   Case Byte of
    0:(id  :PtrUint);
    1:(cond:TsrCondition);
  end;
 end;

 TsrLabels=specialize TNodeTreeClass<TsrLabel>;

 TsrLabelBlock=class
  FEmit:TCustomEmit;
  Body:Pointer;
  DMem:Pointer;
  Size:ptruint;
  FLabels:TsrLabels;
  FVarId:Ptruint;
  Function  FindLabel (Adr:TSrcAdr):TsrLabel;
  Function  FetchLabel(Adr:TSrcAdr):TsrLabel;
  Function  IsContain (P:Pointer):Boolean;
  //
  Function  NewCond(cond:TsrCondition):TsrStatement;
  Function  NewGoto(sLabel,sNext:TsrLabel;pCond:TsrStatement):TsrStatement;
  Function  NewVar:TsrStatement;
  Function  NewStore(pVar,pCond:TsrStatement):TsrStatement;
  Function  NewLoad (pVar:TsrStatement):TsrStatement;
  Function  NewBreak(sLabel:TsrLabel):TsrStatement;
  Function  NewNot  (pCond:TsrStatement):TsrStatement;
  Function  NewOr   (pCond1,pCond2:TsrStatement):TsrStatement;
  Function  NewAnd  (pCond1,pCond2:TsrStatement):TsrStatement;
 end;

function get_branch_offset(var FSPI:TSPI):ptrint;

function IsReal(b:TsrBlockType):Boolean;

implementation

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

function TSrcAdr.get_code_ptr:PDWORD;
begin
 if (pCode=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=PDWORD(pCode.Body);
 end;
 //
 Result:=PDWORD(Result)+Offdw;
end;

function TSrcAdr.get_dmem_ptr:PDWORD;
begin
 if (pCode=nil) then
 begin
  Result:=nil
 end else
 begin
  Result:=PDWORD(pCode.DMem);
 end;
 //
 Result:=PDWORD(Result)+Offdw;
end;


procedure TsrLCursor.Init(Code:TsrLabelBlock);
begin
 pCode    :=Code;
 Body     :=Code.DMem;
 OFFSET_DW:=0;
 prev_adr :=Adr;
end;

Function TsrLCursor.Next(Var SPI:TSPI):Integer;
begin
 prev_adr:=Adr;
 Result:=inherited Next(SPI);
end;

function TsrLCursor.get_src_adr:TSrcAdr;
begin
 Result:=Default(TSrcAdr);
 Result.pCode:=pCode;
 Result.Offdw:=OFFSET_DW;
end;

Procedure TsrLCursor.set_src_adr(src:TSrcAdr);
begin
 pCode    :=src.pCode;
 Body     :=pCode.DMem;
 OFFSET_DW:=src.Offdw;
end;

class function TsrLabel.c(n1,n2:PSrcAdr):Integer;
var
 p1,p2:Pointer;
begin
 p1:=n1^.get_code_ptr;
 p2:=n2^.get_code_ptr;
 Result:=ord(p1>p2)-ord(p1<p2);
end;

Procedure TsrLabel.AddType(t:TsrLabelType);
begin
 lType:=lType+[t];
end;

Procedure TsrLabel.RemType(t:TsrLabelType);
begin
 lType:=lType-[t];
end;

function TsrLabel.IsType(t:TsrLabelType):Boolean;
begin
 Result:=t in lType;
end;

function get_branch_offset(var FSPI:TSPI):ptrint;
begin
 Result:=FSPI.OFFSET_DW+Smallint(FSPI.SOPP.SIMM)+1;
end;

Function TsrLabelBlock.FindLabel(Adr:TSrcAdr):TsrLabel;
begin
 Assert(Adr.pCode=self);
 Result:=FLabels.Find(@Adr);
end;

Function TsrLabelBlock.FetchLabel(Adr:TSrcAdr):TsrLabel;
begin
 Assert(Adr.pCode=self);
 Result:=nil;
 Result:=FLabels.Find(@Adr);
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrLabel>;
  Result.key:=Adr;
  FLabels.Insert(Result);
 end;
end;

Function TsrLabelBlock.IsContain(P:Pointer):Boolean;
begin
 Result:=(Body<=P) and ((Body+Size)>P);
end;

Function TsrLabelBlock.NewCond(cond:TsrCondition):TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Result.sType :=sCond;
 Result.u.cond:=cond;
end;

Function TsrLabelBlock.NewGoto(sLabel,sNext:TsrLabel;pCond:TsrStatement):TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Result.sType :=sGoto;
 Result.sLabel:=sLabel;
 Result.sNext :=sNext;
 Result.pSrc  :=pCond;
end;

Function TsrLabelBlock.NewVar:TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Result.sType:=sVar;
 Result.u.id :=FVarId;
 Inc(FVarId);
end;

Function TsrLabelBlock.NewStore(pVar,pCond:TsrStatement):TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Result.sType:=sStore;
 Result.pDst :=pVar;
 Result.pSrc :=pCond;
end;

Function TsrLabelBlock.NewLoad(pVar:TsrStatement):TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Result.sType:=sLoad;
 Result.pSrc :=pVar;
 Result.u.id :=FVarId;
 Inc(FVarId);
end;

Function TsrLabelBlock.NewBreak(sLabel:TsrLabel):TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Result.sType :=sBreak;
 Result.sLabel:=sLabel;
end;

Function TsrLabelBlock.NewNot(pCond:TsrStatement):TsrStatement;
begin
 case pCond.sType of
  sCond:Result:=NewCond(InvertCond[pCond.u.cond]);
  else
   begin
    Result:=FEmit.specialize New<TsrStatement>;
    Result.sType:=sNot;
    Result.pSrc :=pCond;
   end;
 end;
end;

Function TsrLabelBlock.NewOr(pCond1,pCond2:TsrStatement):TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Result.sType:=sOr;
 Result.pSrc :=pCond1;
 Result.pDst :=pCond2;
end;

Function TsrLabelBlock.NewAnd(pCond1,pCond2:TsrStatement):TsrStatement;
begin
 Result:=FEmit.specialize New<TsrStatement>;
 Result.sType:=sAnd;
 Result.pSrc :=pCond1;
 Result.pDst :=pCond2;
end;

end.

