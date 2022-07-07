unit emit_SOPK;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srTypes,
  srConst,
  srReg,
  SprvEmit,
  emit_op;

type
 TEmit_SOPK=object(TEmitOp)
  procedure _emit_SOPK;
  procedure _emit_S_MOVK_I32;
 end;

implementation

procedure TEmit_SOPK._emit_S_MOVK_I32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_sdst7(FSPI.SOPK.SDST);
 src:=FetchReg(FConsts.Fetchi(dtInt32,SmallInt(FSPI.SOPK.SIMM)));
 _MakeCopy(dst,src);
end;

procedure TEmit_SOPK._emit_SOPK;
begin

 Case FSPI.SOPK.OP of

  S_MOVK_I32:
   begin
    _emit_S_MOVK_I32;
   end;

  else
   Assert(false,'SOPK?'+IntToStr(FSPI.SOPK.OP));
 end;

end;


end.


