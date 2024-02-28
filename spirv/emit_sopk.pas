unit emit_SOPK;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 spirv,
 ps4_pssl,
 srType,
 srReg,
 emit_fetch;

type
 TEmit_SOPK=class(TEmitFetch)
  procedure emit_SOPK;
  procedure emit_S_MOVK_I32;
  procedure emit_S_ADDK_I32;
  procedure emit_S_CMPK_I32(OpId:DWORD);
  procedure emit_S_CMPK_U32(OpId:DWORD);
 end;

implementation

function SignExtend16(W:Word):Integer; inline;
const
 shift=BitSizeOf(Integer)-BitSizeOf(Word);
begin
 Result:=SarLongint((Integer(W) shl shift),shift);
end;

procedure TEmit_SOPK.emit_S_MOVK_I32; //sdst.s = signExtend(imm16)
Var
 dst:PsrRegSlot;
begin
 dst:=get_sdst7(FSPI.SOPK.SDST);
 SetConst_i(dst,dtInt32,SignExtend16(FSPI.SOPK.SIMM));
end;

procedure TEmit_SOPK.emit_S_ADDK_I32; //sdst.s = (sdst.s + signExtend(imm16)); SCC = overflow
Var
 dst:PsrRegSlot;
 car:PsrRegSlot;
 src:PsrRegNode;
 imm:PsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOPK.SDST);
 car:=get_scc;

 src:=fetch_ssrc8(FSPI.SOPK.SDST,dtInt32);
 imm:=NewReg_i(dtInt32,SignExtend16(FSPI.SOPK.SIMM));

 OpIAddExt(dst,car,src,imm);
end;

procedure TEmit_SOPK.emit_S_CMPK_I32(OpId:DWORD); //SCC = compareOp(sdst.s, signExtend(imm16.s))
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
 imm:PsrRegNode;
begin
 dst:=get_scc;

 src:=fetch_ssrc8(FSPI.SOPK.SDST,dtInt32);
 imm:=NewReg_i(dtInt32,SignExtend16(FSPI.SOPK.SIMM));

 OpCmpS(OpId,dst,src,imm);
end;

procedure TEmit_SOPK.emit_S_CMPK_U32(OpId:DWORD); //SCC = compareOp(sdst.u, imm16.u)
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
 imm:PsrRegNode;
begin
 dst:=get_scc;

 src:=fetch_ssrc8(FSPI.SOPK.SDST,dtUint32);
 imm:=NewReg_i(dtUint32,FSPI.SOPK.SIMM);

 OpCmpS(OpId,dst,src,imm);
end;

procedure TEmit_SOPK.emit_SOPK;
begin

 Case FSPI.SOPK.OP of

  S_CMPK_EQ_I32:emit_S_CMPK_I32(Op.OpIEqual);
  S_CMPK_LG_I32:emit_S_CMPK_I32(Op.OpINotEqual);
  S_CMPK_GT_I32:emit_S_CMPK_I32(Op.OpSGreaterThan);
  S_CMPK_GE_I32:emit_S_CMPK_I32(Op.OpSGreaterThanEqual);
  S_CMPK_LT_I32:emit_S_CMPK_I32(Op.OpSLessThan);
  S_CMPK_LE_I32:emit_S_CMPK_I32(Op.OpSLessThanEqual);

  S_CMPK_EQ_U32:emit_S_CMPK_U32(Op.OpIEqual);
  S_CMPK_LG_U32:emit_S_CMPK_U32(Op.OpINotEqual);
  S_CMPK_GT_U32:emit_S_CMPK_U32(Op.OpSGreaterThan);
  S_CMPK_GE_U32:emit_S_CMPK_U32(Op.OpSGreaterThanEqual);
  S_CMPK_LT_U32:emit_S_CMPK_U32(Op.OpSLessThan);
  S_CMPK_LE_U32:emit_S_CMPK_U32(Op.OpSLessThanEqual);

  S_MOVK_I32: emit_S_MOVK_I32;

  S_ADDK_I32: emit_S_ADDK_I32;

  else
   Assert(false,'SOPK?'+IntToStr(FSPI.SOPK.OP));
 end;

end;


end.


