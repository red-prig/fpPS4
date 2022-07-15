unit srLabel;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 ps4_pssl,
 srNodes;

type
 TSrcAdr=object
  pBody:PDWORD;
  Offdw:ptrint;
  function get_pc:PDWORD;
 end;

 TsrLCursor=object(TShaderParser)
  private
   function  get_src_adr:TSrcAdr;
   Procedure set_src_adr(src:TSrcAdr);
  public
   procedure Init(base:Pointer);
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

 TsrLabels=object
  type
   TNodeFetch=specialize TNodeFetch<PsrLabel,TsrLabel>;
  var
   FNTree:TNodeFetch;
 end;

function get_branch_offset(var FSPI:TSPI):ptrint;

implementation

function TSrcAdr.get_pc:PDWORD;
begin
 Result:=pBody+Offdw;
end;

procedure TsrLCursor.Init(base:Pointer);
begin
 Body     :=base;
 OFFSET_DW:=0;
end;

function TsrLCursor.get_src_adr:TSrcAdr;
begin
 Result:=Default(TSrcAdr);
 Result.pBody:=Body;
 Result.Offdw:=OFFSET_DW;
end;

Procedure TsrLCursor.set_src_adr(src:TSrcAdr);
begin
 Body     :=src.pBody;
 OFFSET_DW:=src.Offdw;
end;

function TsrLabel.c(n1,n2:PsrLabel):Integer;
var
 p1,p2:Pointer;
begin
 p1:=n1^.Adr.get_pc;
 p2:=n2^.Adr.get_pc;
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

end.

