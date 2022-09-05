unit emit_VOP2;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  spirv,
  srType,
  srReg,
  emit_fetch;

type
 TEmit_VOP2=class(TEmitFetch)
  procedure emit_VOP2;
  procedure emit_V_CNDMASK_B32;
  procedure emit_V_AND_B32;
  procedure emit_V_OR_B32;
  procedure emit_V_XOR_B32;
  procedure emit_V_SH(OpId:DWORD;rtype:TsrDataType);
  procedure emit_V_SHREV(OpId:DWORD;rtype:TsrDataType);
  procedure emit_V_ADD_I32;
  procedure emit_V_SUB_I32;
  procedure emit_V_SUBREV_I32;
  procedure emit_V2_F32(OpId:DWORD);
  procedure emit_V_SUBREV_F32;
  procedure emit_V_CVT_PKRTZ_F16_F32;
  procedure emit_V_MUL_I32_I24;
  procedure emit_V_MUL_U32_U24;
  procedure emit_V_MAC_F32;
  procedure emit_V_MADAK_F32;
  procedure emit_V_MADMK_F32;
  procedure emit_V_MMX(OpId:DWORD;rtype:TsrDataType);
 end;

implementation

procedure TEmit_VOP2.emit_V_CNDMASK_B32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUnknow);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUnknow);
 src[2]:=MakeRead(get_vcc0,dtBool);

 OpSelect(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP2.emit_V_AND_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUnknow);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUnknow);

 OpBitwiseAnd(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_OR_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUnknow);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUnknow);

 OpBitwiseOr(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_XOR_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUInt32);

 OpBitwiseXor(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_SH(OpId:DWORD;rtype:TsrDataType);
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,rtype);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 src[1]:=OpBitwiseAndTo(src[1],31);
 src[1]^.PrepType(ord(dtUInt32));

 Op2(OpId,src[0]^.dtype,dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_SHREV(OpId:DWORD;rtype:TsrDataType);
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,rtype);

 src[0]:=OpBitwiseAndTo(src[0],31);
 src[0]^.PrepType(ord(dtUInt32));

 Op2(OpId,src[1]^.dtype,dst,src[1],src[0]);
end;

procedure TEmit_VOP2.emit_V_ADD_I32; //vdst = vsrc0.s + vsrc1.s; sdst[thread_id:] = carry_out & EXEC
Var
 dst,car:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 exc:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);
 car:=get_vcc0;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 OpIAddExt(dst,car,src[0],src[1]);

 exc:=MakeRead(get_exec0,dtUnknow);
 OpBitwiseAnd(car,car^.current,exc); //carry_out & EXEC
end;

procedure TEmit_VOP2.emit_V_SUB_I32; //vdst = vsrc0.u - vsub.u; sdst[thread_id:] = borrow_out & EXEC
Var
 dst,bor:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 exc:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);
 bor:=get_vcc0;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 OpISubExt(dst,bor,src[0],src[1]);

 exc:=MakeRead(get_exec0,dtUnknow);
 OpBitwiseAnd(bor,bor^.current,exc); //borrow_out & EXEC
end;

procedure TEmit_VOP2.emit_V_SUBREV_I32; //vdst = vsrc1.u - vsub.u; sdst[thread_id:] = borrow_out & EXEC
Var
 dst,bor:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 exc:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);
 bor:=get_vcc0;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 OpISubExt(dst,bor,src[1],src[0]);

 exc:=MakeRead(get_exec0,dtUnknow);
 OpBitwiseAnd(bor,bor^.current,exc); //borrow_out & EXEC
end;

procedure TEmit_VOP2.emit_V2_F32(OpId:DWORD);
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 Op2(OpId,dtFloat32,dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_SUBREV_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 Op2(Op.OpFSub,dtFloat32,dst,src[1],src[0]);
end;

procedure TEmit_VOP2.emit_V_CVT_PKRTZ_F16_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 OpConvFloatToHalf2(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_MUL_I32_I24;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 bit24:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtInt32);

 bit24:=NewReg_q(dtUInt32,$FFFFFF);

 src[0]:=OpBitwiseAndTo(src[0],bit24);
 src[0]^.PrepType(ord(dtInt32));

 src[1]:=OpBitwiseAndTo(src[1],bit24);
 src[1]^.PrepType(ord(dtInt32));

 OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_MUL_U32_U24;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 bit24:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUInt32);

 bit24:=NewReg_q(dtUInt32,$FFFFFF);

 src[0]:=OpBitwiseAndTo(src[0],bit24);
 src[0]^.PrepType(ord(dtUInt32));

 src[1]:=OpBitwiseAndTo(src[1],bit24);
 src[1]^.PrepType(ord(dtUInt32));

 OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_MAC_F32; //vdst = vsrc0.f * vsrc1.f + vdst.f  -> fma
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);
 src[2]:=MakeRead(dst,dtFloat32);

 OpFmaF32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP2.emit_V_MADAK_F32; //vdst = vsrc0.f * vsrc1.f + kadd.f
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);
 src[2]:=NewReg_q(dtFloat32,FSPI.INLINE32);

 OpFmaF32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP2.emit_V_MADMK_F32; //vdst = vsrc0.f * kmul.f + vadd.f
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=NewReg_q(dtFloat32,FSPI.INLINE32);
 src[2]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 OpFmaF32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP2.emit_V_MMX(OpId:DWORD;rtype:TsrDataType);
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,rtype);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,rtype);

 OpGlsl2(OpId,rtype,dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_VOP2;
begin

 Case FSPI.VOP2.OP of

  V_CNDMASK_B32: emit_V_CNDMASK_B32;

  V_AND_B32    : emit_V_AND_B32;
  V_OR_B32     : emit_V_OR_B32;
  V_XOR_B32    : emit_V_XOR_B32;

  V_LSHL_B32   : emit_V_SH(Op.OpShiftLeftLogical,dtUint32);
  V_LSHLREV_B32: emit_V_SHREV(Op.OpShiftLeftLogical,dtUint32);
  V_LSHR_B32   : emit_V_SH(Op.OpShiftRightLogical,dtUint32);
  V_LSHRREV_B32: emit_V_SHREV(Op.OpShiftRightLogical,dtUint32);
  V_ASHR_I32   : emit_V_SH(Op.OpShiftRightLogical,dtInt32);
  V_ASHRREV_I32: emit_V_SHREV(Op.OpShiftRightLogical,dtInt32);

  V_ADD_I32    : emit_V_ADD_I32;
  V_SUB_I32    : emit_V_SUB_I32;
  V_SUBREV_I32 : emit_V_SUBREV_I32;

  V_ADD_F32    : emit_V2_F32(Op.OpFAdd);
  V_SUB_F32    : emit_V2_F32(Op.OpFSub);
  V_SUBREV_F32 : emit_V_SUBREV_F32;

  V_MUL_F32    :emit_V2_F32(Op.OpFMul);

  V_CVT_PKRTZ_F16_F32: emit_V_CVT_PKRTZ_F16_F32;

  V_MUL_I32_I24: emit_V_MUL_I32_I24;
  V_MUL_U32_U24: emit_V_MUL_U32_U24;

  V_MAC_F32  : emit_V_MAC_F32;
  V_MADAK_F32: emit_V_MADAK_F32;
  V_MADMK_F32: emit_V_MADMK_F32;

  V_MIN_LEGACY_F32: emit_V_MMX(GlslOp.NMin,dtFloat32);
  V_MAX_LEGACY_F32: emit_V_MMX(GlslOp.NMax,dtFloat32);

  V_MIN_F32: emit_V_MMX(GlslOp.FMin,dtFloat32);
  V_MAX_F32: emit_V_MMX(GlslOp.FMax,dtFloat32);

  V_MIN_I32: emit_V_MMX(GlslOp.SMin,dtInt32);
  V_MAX_I32: emit_V_MMX(GlslOp.SMax,dtInt32);

  V_MIN_U32: emit_V_MMX(GlslOp.UMin,dtUint32);
  V_MAX_U32: emit_V_MMX(GlslOp.UMax,dtUint32);

  else
   Assert(false,'VOP2?'+IntToStr(FSPI.VOP2.OP));
 end;

end;

end.

