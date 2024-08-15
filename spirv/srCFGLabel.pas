unit srCFGLabel;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_pssl,
 ginodes,
 srNode;

type
 PsrLabelBlock=^TsrLabelBlock;

 TSrcAdr=object
  pCode:PsrLabelBlock;
  Offdw:ptrint;
  function get_code_ptr:PDWORD;
  function get_dmem_ptr:PDWORD;
 end;

 TsrLCursor=object(TShaderParser)
  private
   pCode:PsrLabelBlock;
   function  get_src_adr:TSrcAdr;
   Procedure set_src_adr(src:TSrcAdr);
  public
   procedure Init(Code:PsrLabelBlock);
   property  Adr:TSrcAdr read get_src_adr write set_src_adr;
 end;

 TsrLabelType=(ltBranch,ltUnknow,ltBegAdr,ltEndAdr,ltBegCond,ltEndCond,ltBegLoop,ltEndLoop);

 TsrSetLabelType=Set of TsrLabelType;

 TsrBlockType=(btMain,btAdr,btAdrBranch,btSetpc,btCond,btLoop,btOther);

 PsrLabel=^TsrLabel;
 TsrLabel=object
  pLeft,pRight:PsrLabel;
  //----
  Adr:TSrcAdr;
  lType:TsrSetLabelType;
  function  c(n1,n2:PsrLabel):Integer; static;
  Procedure AddType(t:TsrLabelType);
  Procedure RemType(t:TsrLabelType);
  function  IsType(t:TsrLabelType):Boolean;
 end;

 TsrLabels=specialize TNodeFetch<PsrLabel,TsrLabel>;

 TsrLabelBlock=object
  FEmit:TCustomEmit;
  Body:Pointer;
  DMem:Pointer;
  Size:ptruint;
  FLabels:TsrLabels;
  Function  FindLabel (Adr:TSrcAdr):PsrLabel;
  Function  FetchLabel(Adr:TSrcAdr):PsrLabel;
  Function  IsContain (P:Pointer):Boolean;
 end;

function get_branch_offset(var FSPI:TSPI):ptrint;

implementation

function TSrcAdr.get_code_ptr:PDWORD;
begin
 if (pCode=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=PDWORD(pCode^.Body);
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
  Result:=PDWORD(pCode^.DMem);
 end;
 //
 Result:=PDWORD(Result)+Offdw;
end;


procedure TsrLCursor.Init(Code:PsrLabelBlock);
begin
 pCode    :=Code;
 Body     :=Code^.DMem;
 OFFSET_DW:=0;
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
 Body     :=pCode^.DMem;
 OFFSET_DW:=src.Offdw;
end;

function TsrLabel.c(n1,n2:PsrLabel):Integer;
var
 p1,p2:Pointer;
begin
 p1:=n1^.Adr.get_code_ptr;
 p2:=n2^.Adr.get_code_ptr;
 Result:=Integer(p1>p2)-Integer(p1<p2);
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

Function TsrLabelBlock.FindLabel(Adr:TSrcAdr):PsrLabel;
var
 node:TsrLabel;
begin
 Assert(Adr.pCode=@self);
 Result:=nil;
 node:=Default(TsrLabel);
 node.Adr:=Adr;
 Result:=FLabels.Find(@node);
end;

Function TsrLabelBlock.FetchLabel(Adr:TSrcAdr):PsrLabel;
var
 node:TsrLabel;
begin
 Assert(Adr.pCode=@self);
 Result:=nil;
 node:=Default(TsrLabel);
 node.Adr:=Adr;
 Result:=FLabels.Find(@node);
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrLabel));
  Result^.Adr:=Adr;
  FLabels.Insert(Result);
 end;
end;

Function TsrLabelBlock.IsContain(P:Pointer):Boolean;
begin
 Result:=(Body<=P) and ((Body+Size)>P);
end;

end.

