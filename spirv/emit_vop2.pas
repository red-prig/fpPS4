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
  function  get_legacy_cmp(src0,src1,zero:TsrRegNode):TsrRegNode;
  procedure emit_V_CNDMASK_B32;
  procedure emit_V_AND_B32;
  procedure emit_V_OR_B32;
  procedure emit_V_XOR_B32;
  procedure emit_V_SH_NRM(OpId:DWORD;rtype:TsrDataType);
  procedure emit_V_SH_REV(OpId:DWORD;rtype:TsrDataType);
  procedure emit_V_ADD_I32;
  procedure emit_V_SUB_I32;
  procedure emit_V_SUBREV_I32;
  procedure emit_V2_F32(OpId:DWORD);
  procedure emit_V_MUL_LEGACY_F32;
  procedure emit_V_SUBREV_F32;
  procedure emit_V_CVT_PKRTZ_F16_F32;
  procedure emit_V_MUL_I32_I24;
  procedure emit_V_MUL_U32_U24;
  procedure emit_V_MAC_F32;
  procedure emit_V_MAC_LEGACY_F32;
  procedure emit_V_MADAK_F32;
  procedure emit_V_MADMK_F32;
  procedure emit_V_BCNT_U32_B32;
  procedure emit_V_MMX(OpId:DWORD;rtype:TsrDataType);
  procedure emit_V_LDEXP_F32;
  procedure emit_V_ADDC_U32;
  procedure emit_V_MBCNT_LO_U32_B32;
  procedure emit_V_MBCNT_HI_U32_B32;
 end;

implementation

function TEmit_VOP2.get_legacy_cmp(src0,src1,zero:TsrRegNode):TsrRegNode;
var
 eql:array[0..1] of TsrRegNode;
begin
 if CompareReg(src0,src1) then
 begin
  Result:=NewReg(dtBool);
  _Op2(line,Op.OpFOrdEqual,Result,src0,zero);
 end else
 begin
  eql[0]:=NewReg(dtBool);
  eql[1]:=NewReg(dtBool);

  _Op2(line,Op.OpFOrdEqual,eql[0],src0,zero);
  _Op2(line,Op.OpFOrdEqual,eql[1],src1,zero);

  Result:=NewReg(dtBool);
  _Op2(line,Op.OpLogicalOr,Result,eql[0],eql[1]);
 end;
end;

procedure TEmit_VOP2.emit_V_CNDMASK_B32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
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
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUnknow);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUnknow);

 OpBitwiseAnd(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_OR_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUnknow);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUnknow);

 OpBitwiseOr(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_XOR_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUInt32);

 OpBitwiseXor(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_SH_NRM(OpId:DWORD;rtype:TsrDataType);
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,rtype);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUInt32);

 src[1]:=OpAndTo(src[1],31);
 src[1].PrepType(ord(dtUInt32));

 Op2(OpId,src[0].dtype,dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_SH_REV(OpId:DWORD;rtype:TsrDataType);
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,rtype);

 src[0]:=OpAndTo(src[0],31);
 src[0].PrepType(ord(dtUInt32));

 Op2(OpId,src[1].dtype,dst,src[1],src[0]);
end;

procedure TEmit_VOP2.emit_V_ADD_I32; //vdst = vsrc0.s + vsrc1.s; sdst[thread_id:] = carry_out & EXEC
Var
 dst,car:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 exc:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);
 car:=get_vcc0;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 OpIAddExt(dst,car,src[0],src[1],dtUint32);

 exc:=MakeRead(get_exec0,dtUnknow);
 OpBitwiseAnd(car,car^.current,exc); //carry_out & EXEC
end;

procedure TEmit_VOP2.emit_V_SUB_I32; //vdst = vsrc0.u - vsub.u; sdst[thread_id:] = borrow_out & EXEC
Var
 dst,bor:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 exc:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);
 bor:=get_vcc0;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 OpISubExt(dst,bor,src[0],src[1],dtUint32);

 exc:=MakeRead(get_exec0,dtUnknow);
 OpBitwiseAnd(bor,bor^.current,exc); //borrow_out & EXEC
end;

procedure TEmit_VOP2.emit_V_SUBREV_I32; //vdst = vsrc1.u - vsub.u; sdst[thread_id:] = borrow_out & EXEC
Var
 dst,bor:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 exc:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);
 bor:=get_vcc0;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 OpISubExt(dst,bor,src[1],src[0],dtUint32);

 exc:=MakeRead(get_exec0,dtUnknow);
 OpBitwiseAnd(bor,bor^.current,exc); //borrow_out & EXEC
end;

procedure TEmit_VOP2.emit_V2_F32(OpId:DWORD);
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 Op2(OpId,dtFloat32,dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_MUL_LEGACY_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 zero:TsrRegNode;
 cmp:TsrRegNode;
 mul:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 zero:=NewReg_s(dtFloat32,0);
 cmp:=get_legacy_cmp(src[0],src[1],zero);

 //
 mul:=NewReg(dtFloat32);
 _Op2(line,Op.OpFMul,mul,src[0],src[1]);

 OpSelect(dst,mul,zero,cmp); //false,true,cond
end;

procedure TEmit_VOP2.emit_V_SUBREV_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 Op2(Op.OpFSub,dtFloat32,dst,src[1],src[0]);
end;

procedure TEmit_VOP2.emit_V_CVT_PKRTZ_F16_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 OpConvFloatToHalf2(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_MUL_I32_I24;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 bit24:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtInt32);

 bit24:=NewReg_q(dtUInt32,$FFFFFF);

 src[0]:=OpAndTo(src[0],bit24);
 src[0].PrepType(ord(dtInt32));

 src[1]:=OpAndTo(src[1],bit24);
 src[1].PrepType(ord(dtInt32));

 OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_MUL_U32_U24;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 bit24:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUInt32);

 bit24:=NewReg_q(dtUInt32,$FFFFFF);

 src[0]:=OpAndTo(src[0],bit24);
 src[0].PrepType(ord(dtUInt32));

 src[1]:=OpAndTo(src[1],bit24);
 src[1].PrepType(ord(dtUInt32));

 OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_MAC_F32; //vdst = vsrc0.f * vsrc1.f + vdst.f  -> fma
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);
 src[2]:=MakeRead(dst,dtFloat32);

 OpFmaF32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP2.emit_V_MAC_LEGACY_F32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 zero:TsrRegNode;
 cmp:TsrRegNode;
 mul:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);
 src[2]:=MakeRead(dst,dtFloat32);

 zero:=NewReg_s(dtFloat32,0);
 cmp:=get_legacy_cmp(src[0],src[1],zero);

 //
 OpFmaF32(dst,src[0],src[1],src[2]);
 //

 mul:=MakeRead(dst,dtFloat32);

 OpSelect(dst,mul,zero,cmp); //false,true,cond
end;

procedure TEmit_VOP2.emit_V_MADAK_F32; //vdst = vsrc0.f * vsrc1.f + kadd.f
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
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
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=NewReg_q(dtFloat32,FSPI.INLINE32);
 src[2]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 OpFmaF32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP2.emit_V_BCNT_U32_B32; //vdst = bit_count(vsrc0) + vsrc1.u
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 src[0]:=OpBitCountTo(src[0]);

 Op2(Op.OpIAdd,dtUint32,dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_MMX(OpId:DWORD;rtype:TsrDataType);
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,rtype);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,rtype);

 OpGlsl2(OpId,rtype,dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_LDEXP_F32; //vdst.f = vsrc0.f * pow(2.0, vsrc1.s)
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 two:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtInt32);

 two:=NewReg_s(dtFloat32,2);
 src[1]:=OpSToF(src[1],dtFloat32);

 src[1]:=OpPowTo(two,src[1]);

 Op2(Op.OpFMul,dtFloat32,dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_ADDC_U32;
Var
 dst,car:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 exc:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);
 car:=get_vcc0;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);
 src[2]:=MakeRead(get_vcc0,dtUInt32);

 src[2]:=OpAndTo(src[2],1);
 src[2].PrepType(ord(dtUInt32));

 OpIAddExt(dst,car,src[0],src[1],dtUint32); //src0+src1

 src[0]:=MakeRead(dst,dtUInt32);
 src[1]:=MakeRead(car,dtUInt32);   //save car1

 OpIAddExt(dst,car,src[0],src[2],dtUint32); //(src0+src1)+src2

 src[0]:=MakeRead(car,dtUInt32);

 OpBitwiseOr(car,src[1],src[0]);   //car1 or car2

 src[0]:=MakeRead(car,dtUInt32);

 exc:=MakeRead(get_exec0,dtUnknow);
 OpBitwiseAnd(car,src[0],exc);     //carry_out & EXEC
end;

//V_MBCNT_LO_U32_B32 v1, -1, v1

procedure TEmit_VOP2.emit_V_MBCNT_LO_U32_B32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 //V_MBCNT_LO_U32_B32 vdst, vsrc, vaccum
 //mask_lo_threads_before= (thread_id>32) ? 0xffffffff : (1<<thread_id)-1
 //vdst = vaccum.u + bit_count(vsrc & mask_lo_threads_before)

 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 src[0]:=OpAndTo(src[0],1); //mean mask_lo_threads_before=1
 src[0]:=OpBitCountTo(src[0]);

 OpIAdd(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_MBCNT_HI_U32_B32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 //V_MBCNT_HI_U32_B3 vdst, vsrc, vaccum
 //mask_hi_threads_before= (thread_id>32) ? (1<<(thread_id-32))-1 : 0
 //vdst = vaccum.u + bit_count(vsrc & mask_hi_threads_before)

 dst:=get_vdst8(FSPI.VOP2.VDST);

 //src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 //only lower thread_id mean
 MakeCopy(dst,src[1]);
end;

procedure TEmit_VOP2.emit_VOP2;
begin

 Case FSPI.VOP2.OP of

  V_CNDMASK_B32: emit_V_CNDMASK_B32;

  V_AND_B32    : emit_V_AND_B32;
  V_OR_B32     : emit_V_OR_B32;
  V_XOR_B32    : emit_V_XOR_B32;

  V_LSHL_B32   : emit_V_SH_NRM(Op.OpShiftLeftLogical    ,dtUint32);
  V_LSHLREV_B32: emit_V_SH_REV(Op.OpShiftLeftLogical    ,dtUint32);
  V_LSHR_B32   : emit_V_SH_NRM(Op.OpShiftRightLogical   ,dtUint32);
  V_LSHRREV_B32: emit_V_SH_REV(Op.OpShiftRightLogical   ,dtUint32);
  V_ASHR_I32   : emit_V_SH_NRM(Op.OpShiftRightArithmetic,dtInt32);
  V_ASHRREV_I32: emit_V_SH_REV(Op.OpShiftRightArithmetic,dtInt32);

  V_ADD_I32    : emit_V_ADD_I32;
  V_SUB_I32    : emit_V_SUB_I32;
  V_SUBREV_I32 : emit_V_SUBREV_I32;

  V_ADD_F32    : emit_V2_F32(Op.OpFAdd);
  V_SUB_F32    : emit_V2_F32(Op.OpFSub);
  V_SUBREV_F32 : emit_V_SUBREV_F32;

  V_MUL_F32    : emit_V2_F32(Op.OpFMul);
  V_MUL_LEGACY_F32: emit_V_MUL_LEGACY_F32;

  V_CVT_PKRTZ_F16_F32: emit_V_CVT_PKRTZ_F16_F32;

  V_MUL_I32_I24: emit_V_MUL_I32_I24;
  V_MUL_U32_U24: emit_V_MUL_U32_U24;

  V_MAC_F32  : emit_V_MAC_F32;
  V_MAC_LEGACY_F32: emit_V_MAC_LEGACY_F32;

  V_MADAK_F32: emit_V_MADAK_F32;
  V_MADMK_F32: emit_V_MADMK_F32;

  V_BCNT_U32_B32: emit_V_BCNT_U32_B32;

  V_MIN_LEGACY_F32: emit_V_MMX(GlslOp.NMin,dtFloat32);
  V_MAX_LEGACY_F32: emit_V_MMX(GlslOp.NMax,dtFloat32);

  V_MIN_F32: emit_V_MMX(GlslOp.FMin,dtFloat32);
  V_MAX_F32: emit_V_MMX(GlslOp.FMax,dtFloat32);

  V_MIN_I32: emit_V_MMX(GlslOp.SMin,dtInt32);
  V_MAX_I32: emit_V_MMX(GlslOp.SMax,dtInt32);

  V_MIN_U32: emit_V_MMX(GlslOp.UMin,dtUint32);
  V_MAX_U32: emit_V_MMX(GlslOp.UMax,dtUint32);

  V_LDEXP_F32: emit_V_LDEXP_F32;

  V_ADDC_U32: emit_V_ADDC_U32;

  V_MBCNT_LO_U32_B32: emit_V_MBCNT_LO_U32_B32;
  V_MBCNT_HI_U32_B32: emit_V_MBCNT_HI_U32_B32;

  else
   Assert(false,'VOP2?'+IntToStr(FSPI.VOP2.OP)+' '+get_str_spi(FSPI));
 end;

end;

end.

