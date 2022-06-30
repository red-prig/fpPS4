unit emit_DS;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srTypes,
  srReg,
  spirv,
  SprvEmit,
  emit_op;

type
 TEmit_DS=object(TEmitOp)
  procedure _emit_DS;
  procedure _emit_DS_SWIZZLE_B32;
 end;

implementation

procedure TEmit_DS._emit_DS_SWIZZLE_B32; //TODO
Var
 dst:PsrRegSlot;
begin
 dst:=FRegsStory.get_vdst8(FSPI.DS.VDST);
 SetConst(dst,dtFloat32,0);
end;

procedure TEmit_DS._emit_DS;
begin

 Case FSPI.DS.OP of

  DS_NOP:;

  //DS_SWIZZLE_B32 v4 v3 v0 v0 OFFSET:0x80AA GDS:0

  DS_SWIZZLE_B32: //TODO
   begin;
     _emit_DS_SWIZZLE_B32;
   end;

  else
   Assert(false,'DS?');
 end;

end;

{
OFFSET0:Byte;
OFFSET1:Byte;
GDS:bit1;
ADDR:Byte;  (vbindex)
DATA0:Byte; (vsrc0)
DATA1:Byte; (vsrc1)
VDST:Byte;
}

end.


