unit emit_SOPK;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srType,
  srReg,
  emit_fetch;

type
 TEmit_SOPK=class(TEmitFetch)
  procedure emit_SOPK;
  procedure emit_S_MOVK_I32;
 end;

implementation

procedure TEmit_SOPK.emit_S_MOVK_I32;
Var
 dst:PsrRegSlot;
begin
 dst:=get_sdst7(FSPI.SOPK.SDST);
 SetConst_i(dst,dtInt32,SmallInt(FSPI.SOPK.SIMM));
end;

procedure TEmit_SOPK.emit_SOPK;
begin

 Case FSPI.SOPK.OP of

  S_MOVK_I32: emit_S_MOVK_I32;

  else
   Assert(false,'SOPK?'+IntToStr(FSPI.SOPK.OP));
 end;

end;


end.


