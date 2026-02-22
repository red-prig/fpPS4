unit emit_VOP2;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  spirv,
  srType,
  srReg,
  srConst,
  emit_fetch;

type
 TEmit_VOP2=class(TEmitFetch)
  procedure emit_VOP2;
  function  get_legacy_cmp(src0,src1,zero:TsrRegNode):TsrRegNode;
  procedure emit_V_CNDMASK_B32;
  procedure emit_V_AND_B32;
  procedure emit_V_OR_B32;
  procedure emit_V_XOR_B32;
  procedure emit_V_SH(OpId:DWORD;rtype:TsrDataType;rev:Boolean);
  procedure emit_V_ADD_I32;
  procedure emit_V_SUB_I32(rev:Boolean);
  procedure emit_V_ADDC_U32;
  procedure emit_V_SUBB_U32(rev:Boolean);
  procedure emit_V2_F32(OpId:DWORD;rev:Boolean);
  procedure emit_V_MUL_LEGACY_F32;
  procedure emit_V_CVT_PKRTZ_F16_F32;
  procedure emit_V_CVT_PKNORM_I16_F32;
  procedure emit_V_CVT_PKNORM_U16_F32;
  procedure emit_V_CVT_PK_U16_U32;
  procedure emit_V_CVT_PK_I16_I32;

  procedure emit_V_MUL_I32_I24;
  procedure emit_V_MUL_U32_U24;
  procedure emit_V_MAC_F32;
  procedure emit_V_MAC_LEGACY_F32;
  procedure emit_V_MADAK_F32;
  procedure emit_V_MADMK_F32;
  procedure emit_V_BCNT_U32_B32;
  procedure emit_V_MMX(OpId:DWORD;rtype:TsrDataType);
  procedure emit_V_LDEXP_F32;
  procedure emit_V_MBCNT_LO_U32_B32;
  procedure emit_V_MBCNT_HI_U32_B32;
  procedure emit_V_WRITELANE_B32;
  procedure emit_V_READLANE_SUBGROUP;
  procedure emit_V_READLANE_B32;
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

procedure TEmit_VOP2.emit_V_CNDMASK_B32; //vdst = smask[thread_id:] ? vsrc1 : vsrc0
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9 (FSPI.VOP2.SRC0 ,dtUnknow);
 src[1]:=fetch_vsrc8 (FSPI.VOP2.VSRC1,dtUnknow);
 src[2]:=GetThreadBit(get_vcc0,get_vcc1,dtBool);

 //dst,cond,src_true,src_false
 OpSelect(dst,src[2],src[1],src[0]);
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

procedure TEmit_VOP2.emit_V_SH(OpId:DWORD;rtype:TsrDataType;rev:Boolean);
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 case rev of
  False:
   begin
    src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,rtype);
    src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUInt32);
   end;
  True:
   begin
    src[1]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUInt32);
    src[0]:=fetch_vsrc8(FSPI.VOP2.VSRC1,rtype);
   end;
 end;

 src[1]:=OpAndTo(src[1],31);
 src[1].PrepType(ord(dtUInt32));

 Op2(OpId,rtype,dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_ADD_I32; //vdst = vsrc0.s + vsrc1.s; sdst[thread_id:] = carry_out & EXEC
Var
 dst,car:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 //exc:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);
 car:=get_vcc0;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 OpIAddExt(dst,car,src[0],src[1],dtUint32);

 {
  TODO:
  if (EXEC[i]) {
    V_ADD_I32
    VCC[i] = car;
  }
  else {
    VCC[i] = 0;
  }
 }

 //exc:=MakeRead(get_exec0,dtUnknow);
 //OpBitwiseAnd(car,car^.current,exc); //carry_out & EXEC
end;

procedure TEmit_VOP2.emit_V_SUB_I32(rev:Boolean); //vdst = vsrc0.u - vsub.u; sdst[thread_id:] = borrow_out & EXEC
Var
 dst,bor:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 //exc:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);
 bor:=get_vcc0;

 case rev of
  False:
   begin
    src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
    src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);
   end;
  True:
   begin
    src[1]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
    src[0]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);
   end;
 end;

 OpISubExt(dst,bor,src[0],src[1],dtUint32);

 {
  TODO:
  if (EXEC[i]) {
    V_SUB_I32
    VCC[i] = bor;
  }
  else {
    VCC[i] = 0;
  }
 }

 //exc:=MakeRead(get_exec0,dtUnknow);
 //OpBitwiseAnd(bor,bor^.current,exc); //borrow_out & EXEC
end;

procedure TEmit_VOP2.emit_V_ADDC_U32;
Var
 dst,car:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 //exc:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);
 car:=get_vcc0;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);
 src[2]:=GetThreadBit(get_vcc0,get_vcc1,dtUInt32);

 src[2]:=OpAndTo(src[2],1);
 src[2].PrepType(ord(dtUInt32));

 OpIAddExt(dst,car,src[0],src[1],dtUint32); //src0+src1

 src[0]:=MakeRead(dst,dtUInt32);
 src[1]:=MakeRead(car,dtUInt32);   //save car1

 OpIAddExt(dst,car,src[0],src[2],dtUint32); //(src0+src1)+src2

 src[0]:=MakeRead(car,dtUInt32);

 OpBitwiseOr(car,src[1],src[0]);   //car1 or car2

 src[0]:=MakeRead(car,dtUInt32);

 {
  TODO:
  if (EXEC[i]) {
    V_ADDC_U32
    VCC[i] = car;
  }
  else {
    VCC[i] = 0;
  }
 }

 //exc:=MakeRead(get_exec0,dtUnknow);
 //OpBitwiseAnd(car,src[0],exc);     //carry_out & EXEC
end;

//v_subbrev_u32
//vdst = vsrc1.u - vsub.u - sborrow[thread_id:]; sdst[thread_id:] = borrow_out & EXEC

procedure TEmit_VOP2.emit_V_SUBB_U32(rev:Boolean); //vdst = vsrc0.u - vsub.u - sborrow[thread_id:]; sdst[thread_id:] = borrow_out & EXEC
Var
 dst,bor:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 //exc:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);
 bor:=get_vcc0;

 case rev of
  False:
   begin
    src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
    src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);
   end;
  True:
   begin
    src[1]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
    src[0]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);
   end;
 end;

 src[2]:=GetThreadBit(get_vcc0,get_vcc1,dtUInt32);

 src[2]:=OpAndTo(src[2],1);
 src[2].PrepType(ord(dtUInt32));

 OpISubExt(dst,bor,src[0],src[1],dtUInt32); //src0-src1

 src[0]:=MakeRead(dst,dtUInt32);
 src[1]:=MakeRead(bor,dtUInt32);   //save car1

 OpISubExt(dst,bor,src[0],src[2],dtUInt32); //(src0-src1)-src2

 src[0]:=MakeRead(bor,dtUInt32);

 //Or??? And???
 OpBitwiseOr(bor,src[1],src[0]);   //car1 or car2

 {
  TODO:
  if (EXEC[i]) {
    V_SUBB_U32
    SDST[i] = bor;
  }
  else {
    SDST[i] = 0;
  }
 }

 //src[0]:=MakeRead(bor,dtUInt32);

 //exc:=MakeRead(get_exec0,dtUnknow);
 //OpBitwiseAnd(bor,src[0],exc);     //borrow_out & EXEC
end;

procedure TEmit_VOP2.emit_V2_F32(OpId:DWORD;rev:Boolean);
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 case rev of
  False:
   begin
    src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
    src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);
   end;
  True:
   begin
    src[1]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
    src[0]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);
   end;
 end;

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

 zero:=NewImm_s(dtFloat32,0);
 cmp:=get_legacy_cmp(src[0],src[1],zero);

 //
 mul:=NewReg(dtFloat32);
 _Op2(line,Op.OpFMul,mul,src[0],src[1]);

 //dst,cond,src_true,src_false
 OpSelect(dst,cmp,zero,mul);
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

procedure TEmit_VOP2.emit_V_CVT_PKNORM_I16_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 OpGlsl1(GlslOp.packSnorm2x16,dtInt32,dst,OpVectorTo(line,dtVec2f,@src));
end;

procedure TEmit_VOP2.emit_V_CVT_PKNORM_U16_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 OpGlsl1(GlslOp.PackUnorm2x16,dtUint32,dst,OpVectorTo(line,dtVec2f,@src));
end;

//vdst[15:0]  = Min(vsrc0.u,0xffff);
//vdst[31:16] = Min(vsrc1.u,0xffff);
procedure TEmit_VOP2.emit_V_CVT_PK_U16_U32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 max:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 max:=NewImm_i(dtUint32,$ffff);

 src[0]:=OpUMinTo(src[0],max);
 src[1]:=OpUMinTo(src[1],max);

 src[0]:=OpUToU(src[0],dtUint16);
 src[1]:=OpUToU(src[1],dtUint16);

 MakeCopy(dst,OpVectorTo(line,dtVec2u16,@src));
end;

//vdst[15:0]  = Clamp(vsrc0.s,-0x8000, 0x7fff);
//vdst[31:16] = Clamp(vsrc1.s,-0x8000, 0x7fff);
procedure TEmit_VOP2.emit_V_CVT_PK_I16_I32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 min,max:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtInt32);

 min:=NewImm_i(dtInt32,-$8000);
 max:=NewImm_i(dtInt32, $7fff);

 src[0]:=OpSClampTo(src[0],min,max);
 src[1]:=OpSClampTo(src[1],min,max);

 src[0]:=OpSToS(src[0],dtInt16);
 src[1]:=OpSToS(src[1],dtInt16);

 MakeCopy(dst,OpVectorTo(line,dtVec2i16,@src));
end;

procedure TEmit_VOP2.emit_V_MUL_I32_I24; //vdst = (vsrc0[23:0].s * vsrc1[23:0].s)
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtInt32);

 src[0]:=OpBFSETo(src[0],NewImm_i(dtInt32,0),NewImm_i(dtInt32,24));
 src[1]:=OpBFSETo(src[1],NewImm_i(dtInt32,0),NewImm_i(dtInt32,24));

 OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_MUL_U32_U24;  //vdst = (vsrc0[23:0].u * vsrc1[23:0].u)
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 bit24:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUInt32);

 bit24:=NewImm_q(dtUInt32,$FFFFFF);

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

 zero:=NewImm_s(dtFloat32,0);
 cmp:=get_legacy_cmp(src[0],src[1],zero);

 //
 OpFmaF32(dst,src[0],src[1],src[2]);
 //

 mul:=MakeRead(dst,dtFloat32);

 //dst,cond,src_true,src_false
 OpSelect(dst,cmp,src[2],mul);
end;

procedure TEmit_VOP2.emit_V_MADAK_F32; //vdst = vsrc0.f * vsrc1.f + kadd.f
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);
 src[2]:=NewImm_q(dtFloat32,FSPI.INLINE32);

 OpFmaF32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP2.emit_V_MADMK_F32; //vdst = vsrc0.f * kmul.f + vadd.f
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=NewImm_q(dtFloat32,FSPI.INLINE32);
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

 two:=NewImm_s(dtFloat32,2);
 src[1]:=OpSToF(src[1],dtFloat32);

 src[1]:=OpPowTo(two,src[1]);

 Op2(Op.OpFMul,dtFloat32,dst,src[0],src[1]);
end;

//V_MBCNT_LO_U32_B32 v1, -1, v1

function _IsConstBitMask(pReg:TsrRegNode):Boolean;
var
 pConst:TsrConst;
begin
 Result:=False;
 pReg:=RegDown(pReg);
 if (pReg=nil) then Exit;
 pConst:=pReg.AsConst;
 if (pConst=nil) then Exit;
 Result:=(pConst.AsInt32=-1);
end;

function _IsInitialWave(pReg:TsrRegNode):Boolean;
var
 pConst:TsrConst;
begin
 Result:=False;
 pReg:=RegDown(pReg);
 if (pReg=nil) then Exit;
 pConst:=pReg.AsConst;
 if (pConst=nil) then Exit;
 Result:=is_dwave(pReg) and (pConst.AsInt32=1);
end;

procedure TEmit_VOP2.emit_V_MBCNT_LO_U32_B32;
Var
 dst:PsrRegSlot;
 src:array[0..3] of TsrRegNode;
 dwave:Boolean;
begin
 //V_MBCNT_LO_U32_B32 vdst, vsrc, vaccum
 //mask_lo_threads_before= (thread_id>32) ? 0xffffffff : (1<<thread_id)-1
 //vdst = vaccum.u + bit_count(vsrc & mask_lo_threads_before)

 dst:=get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);
 src[2]:=MakeRead(get_exec0,dtUnknow);

 dwave:=is_dwave(src[0]) or is_dwave(src[2]);

 if is_dwave(src[0]) then
 begin
  src[0]:=FetchSubgroup(src[0]);
 end;

 if is_dwave(src[2]) then
 begin
  src[2]:=FetchSubgroup(src[2]);
 end;

 src[0]:=OpLogicalAndTo(src[0],src[2]);

 if dwave then
 begin
  src[0].dsbgr:=True;

  src[0]:=OpSubgroupBallotTo(src[0]);
  src[0]:=OpSubgroupBallotBitCountTo(src[0]);
 end else
 begin
  src[0]:=OpBitCountTo(src[0]);
 end;

 OpIAdd(dst,src[0],src[1]);
end;

procedure TEmit_VOP2.emit_V_MBCNT_HI_U32_B32;
Var
 dst:PsrRegSlot;
 src:array[0..3] of TsrRegNode;
begin
 //V_MBCNT_HI_U32_B3 vdst, vsrc, vaccum
 //mask_hi_threads_before= (thread_id>32) ? (1<<(thread_id-32))-1 : 0
 //vdst = vaccum.u + bit_count(vsrc & mask_hi_threads_before)

 //TODO: check ThreadBit

 dst:=get_vdst8(FSPI.VOP2.VDST);

 //src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 //only lower thread_id mean
 MakeCopy(dst,src[1]);
end;

procedure TEmit_VOP2.emit_V_WRITELANE_B32;
Var
 dst  :PsrRegSlot;
 src  :TsrRegNode;
 slane:TsrRegNode;
 slane_id:Byte;
 lane :PsrRegSlot;
begin
 dst  :=get_vdst8  (FSPI.VOP2.VDST);
 src  :=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUnknow);
 slane:=fetch_ssrc8(FSPI.VOP2.VSRC1,dtUnknow);

 Assert(slane.is_const,'TODO: indexed V_WRITELANE_B32');

 slane_id:=(slane.AsConst.AsUint8 and 63);

 if not dst^.ConvertToVectorArray then
 begin
  Assert(false,'ConvertToVectorArray');
 end;

 lane:=dst^.Lanes(slane_id);

 MakeCopy(lane,src);
end;

procedure TEmit_VOP2.emit_V_READLANE_SUBGROUP;
Var
 dst:PsrRegSlot;
 src:TsrRegNode;
 slane:TsrRegNode;
begin
 dst  :=get_ssrc8  (FSPI.VOP2.VDST);
 src  :=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUnknow);
 slane:=fetch_ssrc8(FSPI.VOP2.VSRC1,dtUint32);

 Assert(slane.is_const,'TODO: indexed V_READLANE_B32');

 case src.dtype of
  dtFloat32:; //allow
  dtInt32  :; //allow
  dtUint32 :; //allow
  else
   begin
    //retype
    src:=fetch_ssrc9(FSPI.VOP2.SRC0,dtUint32);
   end;
 end;

 Op3(Op.OpGroupNonUniformBroadcast,src.dtype,dst,NewImm_i(dtUint32,Scope.Subgroup),src,slane);
end;

procedure TEmit_VOP2.emit_V_READLANE_B32;
Var
 dst  :PsrRegSlot;
 src  :PsrRegSlot;
 slane:TsrRegNode;
 slane_id:Byte;
 lane :PsrRegSlot;
 sdata:TsrRegNode;
begin
 dst  :=get_ssrc8  (FSPI.VOP2.VDST);
 src  :=get_ssrc9  (FSPI.VOP2.SRC0);
 slane:=fetch_ssrc8(FSPI.VOP2.VSRC1,dtUnknow);

 if (src^.Category=cVector) then
 begin
  emit_V_READLANE_SUBGROUP;
  Exit;
 end;

 Assert(src^.Category=cVectorArray,'TODO: subgroup V_READLANE_B32');

 Assert(slane.is_const,'TODO: indexed V_READLANE_B32');

 slane_id:=(slane.AsConst.AsUint8 and 63);

 lane:=src^.Lanes(slane_id);

 sdata:=MakeRead(lane,dtUnknow);

 MakeCopy(dst,sdata);
end;

procedure TEmit_VOP2.emit_VOP2;
begin

 Case FSPI.VOP2.OP of

  V_CNDMASK_B32: emit_V_CNDMASK_B32;

  V_AND_B32    : emit_V_AND_B32;
  V_OR_B32     : emit_V_OR_B32;
  V_XOR_B32    : emit_V_XOR_B32;

  V_LSHL_B32   : emit_V_SH(Op.OpShiftLeftLogical    ,dtUint32,False);
  V_LSHLREV_B32: emit_V_SH(Op.OpShiftLeftLogical    ,dtUint32,True);
  V_LSHR_B32   : emit_V_SH(Op.OpShiftRightLogical   ,dtUint32,False);
  V_LSHRREV_B32: emit_V_SH(Op.OpShiftRightLogical   ,dtUint32,True);
  V_ASHR_I32   : emit_V_SH(Op.OpShiftRightArithmetic,dtInt32 ,False);
  V_ASHRREV_I32: emit_V_SH(Op.OpShiftRightArithmetic,dtInt32 ,True);

  V_ADD_I32    : emit_V_ADD_I32;
  V_SUB_I32    : emit_V_SUB_I32(False);
  V_SUBREV_I32 : emit_V_SUB_I32(True );

  V_ADD_F32    : emit_V2_F32(Op.OpFAdd,False);
  V_SUB_F32    : emit_V2_F32(Op.OpFSub,False);
  V_SUBREV_F32 : emit_V2_F32(Op.OpFSub,True );

  V_ADDC_U32: emit_V_ADDC_U32;

  V_SUBB_U32   :emit_V_SUBB_U32(False);
  V_SUBBREV_U32:emit_V_SUBB_U32(True);

  V_MUL_F32    : emit_V2_F32(Op.OpFMul,False);
  V_MUL_LEGACY_F32: emit_V_MUL_LEGACY_F32;

  V_CVT_PKNORM_U16_F32: emit_V_CVT_PKNORM_U16_F32;
  V_CVT_PKNORM_I16_F32: emit_V_CVT_PKNORM_I16_F32;
  V_CVT_PKRTZ_F16_F32 : emit_V_CVT_PKRTZ_F16_F32;

  V_CVT_PK_U16_U32: emit_V_CVT_PK_U16_U32;
  V_CVT_PK_I16_I32: emit_V_CVT_PK_I16_I32;

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

  V_MBCNT_LO_U32_B32: emit_V_MBCNT_LO_U32_B32;
  V_MBCNT_HI_U32_B32: emit_V_MBCNT_HI_U32_B32;

  V_WRITELANE_B32:emit_V_WRITELANE_B32;
  V_READLANE_B32 :emit_V_READLANE_B32;

  else
   Assert(false,'VOP2?'+IntToStr(FSPI.VOP2.OP)+' '+get_str_spi(FSPI));
 end;

end;

end.

