unit emit_VOP3;

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
 TEmit_VOP3=class(TEmitFetch)
  procedure emit_VOP3c;
  procedure emit_VOP3b;
  procedure emit_VOP3a;
  procedure emit_V_CMP  (OpId:DWORD;rtype:TsrDataType;x:Boolean);
  procedure emit_V_CMP_C(r,x:Boolean);
  procedure emit_V_CMP_CLASS_32(x:Boolean);

  procedure emit_src_neg_bit(src:PPsrRegNode;count:Byte;rtype:TsrDataType);
  procedure emit_src_abs_bit(src:PPsrRegNode;count:Byte;rtype:TsrDataType);
  procedure emit_dst_omod__f(dst:PsrRegSlot;rtype:TsrDataType);
  procedure emit_dst_clamp_f(dst:PsrRegSlot;rtype:TsrDataType);
  function  get_legacy_cmp(src0,src1,zero:TsrRegNode):TsrRegNode;

  procedure emit_V_ADDC_U32;
  procedure emit_V_SUBB_U32;

  procedure emit_V_ADD_I32;
  procedure emit_V_SUB_I32;

  procedure emit_V_CNDMASK_B32;
  procedure emit_V_MUL_LEGACY_F32;
  procedure emit_V2_F32(OpId:DWORD;rev:Boolean);

  procedure emit_V_CVT_PKNORM_I16_F32;
  procedure emit_V_CVT_PKNORM_U16_F32;
  procedure emit_V_CVT_PKRTZ_F16_F32;

  procedure emit_V_MMX(OpId:DWORD;rtype:TsrDataType);
  procedure emit_V_MMX3(OpId:DWORD;rtype:TsrDataType);
  procedure emit_V_SH(OpId:DWORD;rtype:TsrDataType;rev:Boolean);
  procedure emit_V_MUL_LO(rtype:TsrDataType);
  function  fetch_i24(src:TsrRegNode):TsrRegNode;
  procedure emit_V_MUL_I32_I24;
  function  fetch_u24(src:TsrRegNode):TsrRegNode;
  procedure emit_V_MUL_U32_U24;
  procedure emit_V_MUL_HI(rtype:TsrDataType);
  procedure emit_V_MAC_F32;
  procedure emit_V_LDEXP_F32;
  procedure emit_V_BCNT_U32_B32;
  procedure emit_V_MBCNT_LO_U32_B32;
  procedure emit_V_MBCNT_HI_U32_B32;

  procedure emit_V_BFE_U32;
  procedure emit_V_BFE_I32;
  procedure emit_V_BFI_B32;
  procedure emit_V_BFM_B32;
  procedure emit_V_MAD_F32;
  procedure emit_V_MAD_LEGACY_F32;
  procedure emit_V_MAD_I32_I24;
  procedure emit_V_MAD_U32_U24;
  procedure emit_V_MAD_U64_U32;
  procedure emit_V_SAD_U32;
  procedure emit_V_MED3_F32;
  procedure emit_V_MED3_I32;
  procedure emit_V_MED3_U32;
  procedure emit_V_FMA_F32;
  procedure emit_V_MMX64(OpId:DWORD);
  function  SelectCubeResult(x,y,z,x_res,y_res,z_res:TsrRegNode):TsrRegNode;
  procedure emit_V_CUBE(OpId:Word);
  procedure emit_V_MOV_B32;
  procedure emit_V_CVT(OpId:DWORD;dst_type,src_type:TsrDataType);
  procedure emit_V_CVT_F16_F32;
  procedure emit_V_CVT_F32_F16;
  procedure emit_V_EXT_F32(OpId:DWORD);
  procedure emit_V_SIN_COS(OpId:DWORD);
  procedure emit_V_RCP_F32;
 end;

implementation

procedure TEmit_VOP3.emit_V_CMP(OpId:DWORD;rtype:TsrDataType;x:Boolean);
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 if not get_sdst7_pair(FSPI.VOP3a.VDST,@dst) then Exit;

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 if (rtype.BitSize=64) then
 begin
  src[0]:=fetch_ssrc9_64(FSPI.VOP3a.SRC0,rtype);
  src[1]:=fetch_ssrc9_64(FSPI.VOP3a.SRC1,rtype);
 end else
 begin
  src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,rtype);
  src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,rtype);
 end;

 emit_src_abs_bit(@src,2,rtype);
 emit_src_neg_bit(@src,2,rtype);

 OpCmpV(OpId,dst[0],dst[1],src[0],src[1]);

 if x then
 begin
  MakeCopy(get_exec0,dst[0]^.current);
  MakeCopy(get_exec1,dst[1]^.current);
 end;
end;

procedure TEmit_VOP3.emit_V_CMP_C(r,x:Boolean);
Var
 dst:array[0..1] of PsrRegSlot;
begin
 if not get_sdst7_pair(FSPI.VOP3a.VDST,@dst) then Exit;

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 SetThreadBit(dst[0],dst[1],NewImm_b(r));

 if x then
 begin
  MakeCopy(get_exec0,dst[0]^.current);
  MakeCopy(get_exec1,dst[1]^.current);
 end;
end;

procedure TEmit_VOP3.emit_V_CMP_CLASS_32(x:Boolean);
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 if not get_sdst7_pair(FSPI.VOP3a.VDST,@dst) then Exit;

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUInt32);

 emit_src_abs_bit(@src,1,dtFloat32);
 emit_src_neg_bit(@src,1,dtFloat32);

 OpCmpClass(dst[0],dst[1],src[0],src[1]);

 if x then
 begin
  MakeCopy(get_exec0,dst[0]^.current);
  MakeCopy(get_exec1,dst[1]^.current);
 end;
end;

procedure TEmit_VOP3.emit_src_neg_bit(src:PPsrRegNode;count:Byte;rtype:TsrDataType);
var
 i:Byte;
begin
 if (FSPI.VOP3a.NEG=0) then Exit;
 if not rtype.isFloat then Exit;

 For i:=0 to count-1 do
  if Byte(FSPI.VOP3a.NEG).TestBit(i) then
  begin
   Assert(src[i].dtype=rtype);
   src[i]:=OpFNegateTo(src[i]);
  end;
end;

procedure TEmit_VOP3.emit_src_abs_bit(src:PPsrRegNode;count:Byte;rtype:TsrDataType);
var
 i:Byte;
begin
 if (FSPI.VOP3a.ABS=0) then Exit;
 if not rtype.isFloat then Exit;

 For i:=0 to count-1 do
  if Byte(FSPI.VOP3a.ABS).TestBit(i) then
  begin
   Assert(src[i].dtype=rtype);
   src[i]:=OpFAbsTo(src[i]);
  end;
end;

procedure TEmit_VOP3.emit_dst_omod__f(dst:PsrRegSlot;rtype:TsrDataType);
var
 src,tmp:TsrRegNode;
begin
 if not rtype.isFloat then Exit;

 src:=dst^.current;

 Case FSPI.VOP3a.OMOD of
  0:;
  1: // *2
    begin
     tmp:=NewImm_s(rtype,2);
     Op2(Op.OpFMul,rtype,dst,src,tmp);
    end;
  2: // *4
    begin
     tmp:=NewImm_s(rtype,4);
     Op2(Op.OpFMul,rtype,dst,src,tmp);
    end;
  3: // /2
    begin
     tmp:=NewImm_s(rtype,2);
     Op2(Op.OpFDiv,rtype,dst,src,tmp);
    end;
 end;

end;

procedure TEmit_VOP3.emit_dst_clamp_f(dst:PsrRegSlot;rtype:TsrDataType);
var
 src,min,max:TsrRegNode;
begin
 if (FSPI.VOP3a.CLAMP=0) then Exit;
 if not rtype.isFloat then Exit;

 src:=dst^.current;
 min:=NewImm_s(rtype,0);
 max:=NewImm_s(rtype,1);

 OpGlsl3(GlslOp.FClamp,rtype,dst,src,min,max);
end;

function TEmit_VOP3.get_legacy_cmp(src0,src1,zero:TsrRegNode):TsrRegNode;
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

procedure TEmit_VOP3.emit_V_CNDMASK_B32; //vdst = smask[thread_id:] ? vsrc1 : vsrc0
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 rtype:TsrDataType;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 if (Byte(FSPI.VOP3a.ABS).TestBit(0)) or
    (Byte(FSPI.VOP3a.NEG).TestBit(0)) or
    (Byte(FSPI.VOP3a.ABS).TestBit(1)) or
    (Byte(FSPI.VOP3a.NEG).TestBit(1)) then
 begin
  rtype:=dtFloat32;
 end else
 begin
  rtype:=dtUnknow;
 end;

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,rtype);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,rtype);
 src[2]:=GetThreadBit(get_ssrc9(FSPI.VOP3a.SRC2),get_ssrc9(FSPI.VOP3a.SRC2+1),dtBool);

 emit_src_abs_bit(@src,2,rtype);
 emit_src_neg_bit(@src,2,rtype);

 //dst,cond,src_true,src_false
 OpSelect(dst,src[2],src[1],src[0]);
end;

procedure TEmit_VOP3.emit_V_MUL_LEGACY_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 zero:TsrRegNode;
 cmp:TsrRegNode;
 mul:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 emit_src_abs_bit(@src,2,dtFloat32);
 emit_src_neg_bit(@src,2,dtFloat32);

 zero:=NewImm_s(dtFloat32,0);
 cmp:=get_legacy_cmp(src[0],src[1],zero);

 //
 mul:=NewReg(dtFloat32);
 _Op2(line,Op.OpFMul,mul,src[0],src[1]);

 //dst,cond,src_true,src_false
 OpSelect(dst,cmp,zero,mul);

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);
end;

procedure TEmit_VOP3.emit_V2_F32(OpId:DWORD;rev:Boolean);
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 emit_src_abs_bit(@src,2,dtFloat32);
 emit_src_neg_bit(@src,2,dtFloat32);

 case rev of
  False:Op2(OpId,dtFloat32,dst,src[0],src[1]);
  True :Op2(OpId,dtFloat32,dst,src[1],src[0]);
 end;

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);
end;

procedure TEmit_VOP3.emit_V_CVT_PKNORM_I16_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 vec:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 emit_src_abs_bit(@src,2,dtFloat32);
 emit_src_neg_bit(@src,2,dtFloat32);

 vec:=OpVectorTo(line,dtVec2f,@src);

 OpGlsl1(GlslOp.packSnorm2x16,dtInt32,dst,vec);
end;

procedure TEmit_VOP3.emit_V_CVT_PKNORM_U16_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 vec:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 emit_src_abs_bit(@src,2,dtFloat32);
 emit_src_neg_bit(@src,2,dtFloat32);

 vec:=OpVectorTo(line,dtVec2f,@src);

 OpGlsl1(GlslOp.PackUnorm2x16,dtUint32,dst,vec);
end;

procedure TEmit_VOP3.emit_V_CVT_PKRTZ_F16_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 emit_src_abs_bit(@src,2,dtFloat32);
 emit_src_neg_bit(@src,2,dtFloat32);

 OpConvFloatToHalf2(dst,src[0],src[1]);
end;

procedure TEmit_VOP3.emit_V_MMX(OpId:DWORD;rtype:TsrDataType);
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,rtype);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,rtype);

 emit_src_abs_bit(@src,2,rtype);
 emit_src_neg_bit(@src,2,rtype);

 OpGlsl2(OpId,rtype,dst,src[0],src[1]);

 emit_dst_omod__f(dst,rtype);
 emit_dst_clamp_f(dst,rtype);
end;

procedure TEmit_VOP3.emit_V_MMX3(OpId:DWORD;rtype:TsrDataType);
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 mid:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,rtype);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,rtype);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,rtype);

 emit_src_abs_bit(@src,3,rtype);
 emit_src_neg_bit(@src,3,rtype);

 OpGlsl2(OpId,rtype,dst,src[0],src[1]);

 mid:=MakeRead(dst,rtype);

 OpGlsl2(OpId,rtype,dst,mid,src[2]);

 emit_dst_omod__f(dst,rtype);
 emit_dst_clamp_f(dst,rtype);
end;

procedure TEmit_VOP3.emit_V_SH(OpId:DWORD;rtype:TsrDataType;rev:Boolean);
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 case rev of
  False:
    begin
     src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,rtype);
     src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUInt32);
    end;
  True:
    begin
     src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUInt32);
     src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC1,rtype);
    end;
 end;

 src[1]:=OpAndTo(src[1],31);
 src[1].PrepType(ord(dtUInt32));

 Op2(OpId,rtype,dst,src[0],src[1]);
end;

procedure TEmit_VOP3.emit_V_MUL_LO(rtype:TsrDataType);
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,rtype);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,rtype);

 OpIMul(dst,src[0],src[1]);
end;

function int24(b:Integer):Integer; inline;
const
 shift=BitSizeOf(Integer)-24;
begin
 Result:=SarLongint((b shl shift),shift);
end;

function TEmit_VOP3.fetch_i24(src:TsrRegNode):TsrRegNode;
var
 pImm:TsrConst;
begin
 //early optimization
 pImm:=src.pWriter.specialize AsType<ntConst>;
 if (pImm<>nil) then
 begin
  Result:=NewImm_i(dtInt32,int24(pImm.AsInt32));
 end else
 begin
  Result:=OpBFSETo(src,NewImm_i(dtInt32,0),NewImm_i(dtInt32,24));
 end;
end;

procedure TEmit_VOP3.emit_V_MUL_I32_I24; //vdst = (vsrc0[23:0].s * vsrc1[23:0].s)
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtInt32);

 src[0]:=fetch_i24(src[0]);
 src[1]:=fetch_i24(src[1]);

 OpIMul(dst,src[0],src[1]);
end;

function TEmit_VOP3.fetch_u24(src:TsrRegNode):TsrRegNode;
var
 pImm:TsrConst;
begin
 //early optimization
 pImm:=src.pWriter.specialize AsType<ntConst>;
 if (pImm<>nil) then
 begin
  Result:=NewImm_i(dtUInt32,(pImm.AsInt32 and $FFFFFF));
 end else
 begin
  Result:=OpAndTo(src,NewImm_q(dtUInt32,$FFFFFF));
  Result.PrepType(ord(dtUInt32));
 end;
end;

procedure TEmit_VOP3.emit_V_MUL_U32_U24;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUInt32);

 src[0]:=fetch_u24(src[0]);
 src[1]:=fetch_u24(src[1]);

 OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP3.emit_V_MUL_HI(rtype:TsrDataType);
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 tmp_r,dst_r:TsrRegNode;
 tst:TsrDataType;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,rtype);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,rtype);

 tst:=rtype.AsStruct2;
 Assert(tst<>dtUnknow);

 tmp_r:=NewReg(tst);

 if (rtype.Sign=0) then
 begin
  _Op2(line,Op.OpUMulExtended,tmp_r,src[0],src[1]);
 end else
 begin
  _Op2(line,Op.OpSMulExtended,tmp_r,src[0],src[1]);
 end;

 dst_r:=dst^.New(rtype);

 OpExtract(line,dst_r,tmp_r,1);
end;

procedure TEmit_VOP3.emit_V_MAC_F32; //vdst = vsrc0.f * vsrc1.f + vdst.f  -> fma
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=MakeRead(dst,dtFloat32);

 emit_src_abs_bit(@src,3,dtFloat32);
 emit_src_neg_bit(@src,3,dtFloat32);

 OpFmaF32(dst,src[0],src[1],src[2]);

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);
end;

procedure TEmit_VOP3.emit_V_LDEXP_F32; //vdst.f = vsrc0.f * pow(2.0, vsrc1.s)
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 two:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtInt32);

 emit_src_abs_bit(@src,1,dtFloat32);
 emit_src_neg_bit(@src,1,dtFloat32);

 two:=NewImm_s(dtFloat32,2);
 src[1]:=OpSToF(src[1],dtFloat32);

 src[1]:=OpPowTo(two,src[1]);

 Op2(Op.OpFMul,dtFloat32,dst,src[0],src[1]);

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);
end;

procedure TEmit_VOP3.emit_V_BCNT_U32_B32; //vdst = bit_count(vsrc0) + vsrc1.u
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUint32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUint32);

 src[0]:=OpBitCountTo(src[0]);

 Op2(Op.OpIAdd,dtUint32,dst,src[0],src[1]);
end;

procedure TEmit_VOP3.emit_V_MBCNT_LO_U32_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 //V_MBCNT_LO_U32_B32 vdst, vsrc, vaccum
 //mask_lo_threads_before= (thread_id>32) ? 0xffffffff : (1<<thread_id)-1
 //vdst = vaccum.u + bit_count(vsrc & mask_lo_threads_before)

 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUint32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUint32);

 src[0]:=OpAndTo(src[0],1); //mean mask_lo_threads_before=1
 src[0]:=OpBitCountTo(src[0]);

 OpIAdd(dst,src[0],src[1]);
end;

procedure TEmit_VOP3.emit_V_MBCNT_HI_U32_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 //V_MBCNT_HI_U32_B3 vdst, vsrc, vaccum
 //mask_hi_threads_before= (thread_id>32) ? (1<<(thread_id-32))-1 : 0
 //vdst = vaccum.u + bit_count(vsrc & mask_hi_threads_before)

 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 //src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUint32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUint32);

 //only lower thread_id mean
 MakeCopy(dst,src[1]);
end;

procedure TEmit_VOP3.emit_V_BFE_U32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUint32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUint32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUint32);

 OpBFE_32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3.emit_V_BFE_I32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUint32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUint32);

 OpBFE_32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3.emit_V_BFI_B32;
Var
 dst:PsrRegSlot;
 bitmsk:TsrRegNode;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 //ignore
 //Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 //Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 //Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 //Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 bitmsk:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUint32);
 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUint32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUint32);

 OpBFIB32(dst,bitmsk,src[0],src[1]);
end;

procedure TEmit_VOP3.emit_V_BFM_B32; //vdst = ((1<<vsize[4:0]) - 1) << voffset[4:0]
Var
 dst:PsrRegSlot;
 vsize  :TsrRegNode;
 voffset:TsrRegNode;
 src    :TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 vsize  :=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUint32);
 voffset:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUint32);

 vsize  :=OpAndTo(vsize  ,15); //[0:4]
 voffset:=OpAndTo(voffset,15); //[0:4]

 vsize  .PrepType(ord(dtUint32));
 voffset.PrepType(ord(dtUint32));

 src:=OpShlTo (NewImm_q(dtUint32,1),vsize);
 src:=OpISubTo(vsize,1);

 src:=OpShlTo(src,voffset);

 MakeCopy(dst,src);
end;

procedure TEmit_VOP3.emit_V_MAD_F32; //vdst = vsrc0.f * vsrc1.f + vadd.f
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 emit_src_abs_bit(@src,3,dtFloat32);
 emit_src_neg_bit(@src,3,dtFloat32);

 OpFmaF32(dst,src[0],src[1],src[2]);

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);
end;

procedure TEmit_VOP3.emit_V_MAD_LEGACY_F32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 zero:TsrRegNode;
 cmp:TsrRegNode;
 mul:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 zero:=NewImm_s(dtFloat32,0);
 cmp:=get_legacy_cmp(src[0],src[1],zero);

 //
 emit_src_abs_bit(@src,3,dtFloat32);
 emit_src_neg_bit(@src,3,dtFloat32);

 OpFmaF32(dst,src[0],src[1],src[2]);

 mul:=MakeRead(dst,dtFloat32);

 //dst,cond,src_true,src_false
 OpSelect(dst,cmp,src[2],mul);

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);
 //

end;

procedure TEmit_VOP3.emit_V_MAD_I32_I24; //vdst.i = vsrc0[23:0].i * vsrc1[23:0].i + vsrc2.i
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 pImm:TsrConst;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 //FSPI.VOP3a.ABS   ignored
 //FSPI.VOP3a.CLAMP ignored
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtInt32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtInt32);

 src[0]:=fetch_i24(src[0]);
 src[1]:=fetch_i24(src[1]);

 OpFmaI32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3.emit_V_MAD_U32_U24; //vdst.u = vsrc0[23:0].u * vsrc1[23:0].u + vsrc2.u
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 //FSPI.VOP3a.ABS   ignored
 //FSPI.VOP3a.CLAMP ignored
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUInt32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUInt32);

 src[0]:=fetch_u24(src[0]);
 src[1]:=fetch_u24(src[1]);

 OpFmaU32(dst,src[0],src[1],src[2]);
end;

//vdst.du = vsrc0.u * vsrc1.u + vsrc2.du; VCC = carry & EXEC
procedure TEmit_VOP3.emit_V_MAD_U64_U32;
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 mul,sum,car:TsrRegNode;
 //exc:TsrRegNode;
begin
 dst[0]:=get_vdst8(FSPI.VOP3a.VDST+0);
 dst[1]:=get_vdst8(FSPI.VOP3a.VDST+1);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 //FSPI.VOP3a.ABS   ignored
 //FSPI.VOP3a.CLAMP ignored
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9   (FSPI.VOP3a.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9   (FSPI.VOP3a.SRC1,dtUInt32);
 src[2]:=fetch_ssrc9_64(FSPI.VOP3a.SRC2,dtUint64);

 src[0]:=OpUToU(src[0],dtUint64);
 src[1]:=OpUToU(src[1],dtUint64);

 mul:=NewReg(dtUint64);
 sum:=NewReg(dtUint64);
 car:=NewReg(dtUint64);

 _Op2(line,Op.OpIMul,mul,src[0],src[1]); //vsrc0.u * vsrc1.u
 OpIAddExt(sum,car,mul,src[2]);          //mul + vsrc2.du | carry
 car:=OpIntToBoolTo(car);

 MakeCopy64(dst[0],dst[1],sum);

 {
  TODO:
  if (EXEC[i]) {
    V_MAD_U64_U32
    VCC[i] = car;
  }
  else {
    VCC[i] = 0;
  }
 }

 //VCC = carry & EXEC
 //exc:=MakeRead(get_exec0,dtUnknow);
 //OpBitwiseAnd(get_vcc0,car,exc);
end;

procedure TEmit_VOP3.emit_V_SAD_U32; //dst.u = abs(vsrc0.u - vsrc1.u) + vaccum.u[15:0]
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 rdif,rvac:TsrRegNode;
 bit16:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUInt32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUInt32);

 rdif:=NewReg(dtUInt32);
 OpAbsDiff(line,rdif,src[0],src[1]);

 bit16:=NewImm_q(dtUInt32,$FFFF);
 rvac:=OpAndTo(src[2],bit16);
 rvac.PrepType(ord(dtUInt32));

 OpIAdd(dst,rdif,rvac);
end;

procedure TEmit_VOP3.emit_V_MED3_F32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 emit_src_abs_bit(@src,3,dtFloat32);
 emit_src_neg_bit(@src,3,dtFloat32);

 OpMED3F(dst,src[0],src[1],src[2]);

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);
end;

procedure TEmit_VOP3.emit_V_MED3_I32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtInt32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtInt32);

 OpMED3I(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3.emit_V_MED3_U32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUint32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUint32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUint32);

 OpMED3U(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3.emit_V_FMA_F32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 emit_src_abs_bit(@src,3,dtFloat32);
 emit_src_neg_bit(@src,3,dtFloat32);

 OpFmaF32(dst,src[0],src[1],src[2]);

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);
end;

procedure TEmit_VOP3.emit_V_MMX64(OpId:DWORD);
Var
 dst:array[0..1] of PsrRegSlot;
 mmx:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst[0]:=get_vdst8(FSPI.VOP3a.VDST+0);
 dst[1]:=get_vdst8(FSPI.VOP3a.VDST+1);

 mmx:=@RegsStory.FUnattach;

 src[0]:=fetch_ssrc9_64(FSPI.VOP3a.SRC0,dtFloat64);
 src[1]:=fetch_ssrc9_64(FSPI.VOP3a.SRC1,dtFloat64);

 emit_src_abs_bit(@src,2,dtFloat64);
 emit_src_neg_bit(@src,2,dtFloat64);

 OpGlsl2(OpId,dtFloat64,mmx,src[0],src[1]);

 emit_dst_omod__f(mmx,dtFloat64);
 emit_dst_clamp_f(mmx,dtFloat64);

 MakeCopy64(dst[0],dst[1],mmx^.current);
end;

{
float3 __GetCubemapUv(float3 uv)
{
 float  faceId = __v_cubeid_f32(uv.x, uv.y, uv.z);
 float  sc     = __v_cubesc_f32(uv.x, uv.y, uv.z);
 float  tc     = __v_cubetc_f32(uv.x, uv.y, uv.z);
 float  ima    = 1.f / abs( __v_cubema_f32(uv.x, uv.y, uv.z) );
 return float3(sc * ima + 1.5f, tc * ima + 1.5f, faceId);
}                            x                y       z

float4 __GetCubemapArrayUv(float4 uv)
{
 float3 cuv = __GetCubemapUv(uv.xyz);
 cuv.z      = cuv.z + (uv.w * 8.f); //face_id + slice*8
 return float4(cuv, uv.w);
}
}

function TEmit_VOP3.SelectCubeResult(x,y,z,x_res,y_res,z_res:TsrRegNode):TsrRegNode;
var
 abs_x:TsrRegNode;
 abs_y:TsrRegNode;
 abs_z:TsrRegNode;

 cmp_zx:TsrRegNode;
 cmp_zy:TsrRegNode;

 z_face_cond:TsrRegNode;
 y_face_cond:TsrRegNode;
begin
 abs_x:=OpFAbsTo(x);
 abs_y:=OpFAbsTo(y);
 abs_z:=OpFAbsTo(z);

 cmp_zx:=NewReg(dtBool);
 _Op2(line,Op.OpFOrdGreaterThanEqual,cmp_zx,abs_z,abs_x);

 cmp_zy:=NewReg(dtBool);
 _Op2(line,Op.OpFOrdGreaterThanEqual,cmp_zy,abs_z,abs_y);

 z_face_cond:=OpLogicalAndTo(cmp_zx,cmp_zy);

 y_face_cond:=NewReg(dtBool);
 _Op2(line,Op.OpFOrdGreaterThanEqual,y_face_cond,abs_y,abs_x);

 Result:=OpSelectTo(z_face_cond,z_res,OpSelectTo(y_face_cond,y_res,x_res));
end;

procedure TEmit_VOP3.emit_V_CUBE(OpId:Word);
Var
 dst:PsrRegSlot;
 src:array[0..2] of TsrRegNode;

 zero:TsrRegNode;
 two :TsrRegNode;

 x_neg_cond:TsrRegNode;
 y_neg_cond:TsrRegNode;
 z_neg_cond:TsrRegNode;

 x_face:TsrRegNode;
 y_face:TsrRegNode;
 z_face:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 emit_src_abs_bit(@src,3,dtFloat32);
 emit_src_neg_bit(@src,3,dtFloat32);

 case OpId of
  V_CUBEID_F32:
    begin
     zero:=NewImm_s(dtFloat32,0.0);

     x_neg_cond:=NewReg(dtBool);
     _Op2(line,Op.OpFOrdLessThan,x_neg_cond,src[0],zero);

     y_neg_cond:=NewReg(dtBool);
     _Op2(line,Op.OpFOrdLessThan,y_neg_cond,src[1],zero);

     z_neg_cond:=NewReg(dtBool);
     _Op2(line,Op.OpFOrdLessThan,z_neg_cond,src[2],zero);

     x_face:=OpSelectTo(x_neg_cond,NewImm_s(dtFloat32,1.0),NewImm_s(dtFloat32,0.0));
     y_face:=OpSelectTo(y_neg_cond,NewImm_s(dtFloat32,3.0),NewImm_s(dtFloat32,2.0));
     z_face:=OpSelectTo(z_neg_cond,NewImm_s(dtFloat32,5.0),NewImm_s(dtFloat32,4.0));

     MakeCopy(dst,SelectCubeResult(src[0],src[1],src[2],x_face,y_face,z_face));
    end;
  V_CUBESC_F32:
    begin
     zero:=NewImm_s(dtFloat32,0.0);

     x_neg_cond:=NewReg(dtBool);
     _Op2(line,Op.OpFOrdLessThan,x_neg_cond,src[0],zero);

     z_neg_cond:=NewReg(dtBool);
     _Op2(line,Op.OpFOrdLessThan,z_neg_cond,src[2],zero);

     x_face:=OpSelectTo(x_neg_cond,src[2],OpFNegateTo(src[2]));
     y_face:=src[0];
     z_face:=OpSelectTo(z_neg_cond,OpFNegateTo(src[0]),src[0]);

     MakeCopy(dst,SelectCubeResult(src[0],src[1],src[2],x_face,y_face,z_face));
    end;
  V_CUBETC_F32:
    begin
     zero:=NewImm_s(dtFloat32,0.0);

     y_neg_cond:=NewReg(dtBool);
     _Op2(line,Op.OpFOrdLessThan,y_neg_cond,src[1],zero);

     x_face:=OpFNegateTo(src[1]);
     y_face:=OpSelectTo(y_neg_cond,OpFNegateTo(src[2]),src[2]);
     z_face:=x_face;

     MakeCopy(dst,SelectCubeResult(src[0],src[1],src[2],x_face,y_face,z_face));
    end;
  V_CUBEMA_F32:
    begin
     two:=NewImm_s(dtFloat32,2.0);

     x_face:=OpFMulTo(src[0],two);
     y_face:=OpFMulTo(src[1],two);
     z_face:=OpFMulTo(src[2],two);

     MakeCopy(dst,SelectCubeResult(src[0],src[1],src[2],x_face,y_face,z_face));
    end;
  else
   Assert(false);
 end;

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);

end;

procedure TEmit_VOP3.emit_V_MOV_B32;
Var
 dst:PsrRegSlot;
 src:TsrRegNode;
 rtype:TsrDataType;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 if (Byte(FSPI.VOP3a.ABS).TestBit(0)) or
    (Byte(FSPI.VOP3a.NEG).TestBit(0)) then
 begin
  rtype:=dtFloat32;
 end else
 begin
  rtype:=dtUnknow;
 end;

 src:=fetch_ssrc9(FSPI.VOP3a.SRC0,rtype);

 emit_src_abs_bit(@src,1,rtype);
 emit_src_neg_bit(@src,1,rtype);

 MakeCopy(dst,src);
end;

procedure TEmit_VOP3.emit_V_CVT(OpId:DWORD;dst_type,src_type:TsrDataType);
Var
 dst:PsrRegSlot;
 src:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);
 src:=fetch_ssrc9(FSPI.VOP3a.SRC0,src_type);

 if (not src_type.isFloat) then
 begin
  Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
  Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');
 end;

 emit_src_abs_bit(@src,1,src_type);
 emit_src_neg_bit(@src,1,src_type);

 Op1(OpId,dst_type,dst,src);

 if (not dst_type.isFloat) then
 begin
  Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
  Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 end;

 emit_dst_omod__f(dst,dst_type);
 emit_dst_clamp_f(dst,dst_type);
end;

procedure TEmit_VOP3.emit_V_CVT_F16_F32; //vdst[15:0].hf = ConvertFloatToHalfFloat(vsrc.f)
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 dstv:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);
 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);

 emit_src_abs_bit(@src[0],1,dtFloat32);
 emit_src_neg_bit(@src[0],1,dtFloat32);

 MakeCopy(dst,src[0]);

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);

 src[0]:=MakeRead(dst,dtFloat32);

 src[0]:=OpFToF(src[0],dtHalf16);
 src[1]:=NewImm_s(dtHalf16,0);

 dstv:=OpVectorTo(line,dtVec2h,@src);

 dst^.New(dtVec2h).pWriter:=dstv;
end;

procedure TEmit_VOP3.emit_V_CVT_F32_F16; //vdst.f = ConvertHalfFloatToFloat(vsrc[15:0].hf)
Var
 dst:PsrRegSlot;
 src:TsrRegNode;
 dst0:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);
 src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtVec2h{dtUnknow});

 //src:=OpBitwiseAndTo(src,$FFFF);
 //src^.PrepType(ord(dtHalf16));

 dst0:=NewReg(dtHalf16);
 OpExtract(line,dst0,src,0);

 src:=OpFToF({src}dst0,dtFloat32);

 emit_src_abs_bit(@src,1,dtFloat32);
 emit_src_neg_bit(@src,1,dtFloat32);

 MakeCopy(dst,src);

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);
end;

procedure TEmit_VOP3.emit_V_EXT_F32(OpId:DWORD);
Var
 dst:PsrRegSlot;
 src:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);
 src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);

 emit_src_abs_bit(@src,1,dtFloat32);
 emit_src_neg_bit(@src,1,dtFloat32);

 OpGlsl1(OpId,dtFloat32,dst,src);

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);
end;

procedure TEmit_VOP3.emit_V_SIN_COS(OpId:DWORD);
const
 PI2:Single=2*PI;
Var
 dst:PsrRegSlot;
 src:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);
 src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);

 src:=OpFMulToS(src,PI2);

 emit_src_abs_bit(@src,1,dtFloat32);
 emit_src_neg_bit(@src,1,dtFloat32);

 OpGlsl1(OpId,dtFloat32,dst,src);

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);
end;

procedure TEmit_VOP3.emit_V_RCP_F32;
Var
 dst:PsrRegSlot;
 src:TsrRegNode;
 one:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);
 src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);

 emit_src_abs_bit(@src,1,dtFloat32);
 emit_src_neg_bit(@src,1,dtFloat32);

 one:=NewImm_s(dtFloat32,1);

 Op2(Op.OpFDiv,dtFloat32,dst,one,src);

 emit_dst_omod__f(dst,dtFloat32);
 emit_dst_clamp_f(dst,dtFloat32);
end;

procedure TEmit_VOP3.emit_VOP3c;
begin
 Case FSPI.VOP3a.OP of

  V_CMP_F_F32,
  V_CMP_F_F64,
  V_CMP_F_I32,
  V_CMP_F_I64,
  V_CMP_F_U32,
  V_CMP_F_U64,
  V_CMPS_F_F32,
  V_CMPS_F_F64:emit_V_CMP_C(false,false);

  V_CMP_T_F32,
  V_CMP_T_F64,
  V_CMP_T_I32,
  V_CMP_T_I64,
  V_CMP_T_U32,
  V_CMP_T_U64,
  V_CMPS_T_F32,
  V_CMPS_T_F64:emit_V_CMP_C(true,false);

  V_CMPX_F_F32,
  V_CMPX_F_F64,
  V_CMPX_F_I32,
  V_CMPX_F_I64,
  V_CMPX_F_U32,
  V_CMPX_F_U64,
  V_CMPSX_F_F32,
  V_CMPSX_F_F64:emit_V_CMP_C(false,true);

  V_CMPX_T_F32,
  V_CMPX_T_F64,
  V_CMPX_T_I32,
  V_CMPX_T_I64,
  V_CMPX_T_U32,
  V_CMPX_T_U64,
  V_CMPSX_T_F32,
  V_CMPSX_T_F64:emit_V_CMP_C(true,true);

  //

  V_CMP_LT_F32    :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat32,false);
  V_CMP_EQ_F32    :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat32,false);
  V_CMP_LE_F32    :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat32,false);
  V_CMP_GT_F32    :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat32,false);
  V_CMP_LG_F32    :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat32,false);
  V_CMP_GE_F32    :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat32,false);
  V_CMP_O_F32     :emit_V_CMP(Op.OpOrdered               ,dtFloat32,false);
  V_CMP_U_F32     :emit_V_CMP(Op.OpUnordered             ,dtFloat32,false);
  V_CMP_NGE_F32   :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat32,false);
  V_CMP_NLG_F32   :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat32,false);
  V_CMP_NGT_F32   :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat32,false);
  V_CMP_NLE_F32   :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat32,false);
  V_CMP_NEQ_F32   :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat32,false);
  V_CMP_NLT_F32   :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat32,false);

  V_CMPX_LT_F32   :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat32,true);
  V_CMPX_EQ_F32   :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat32,true);
  V_CMPX_LE_F32   :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat32,true);
  V_CMPX_GT_F32   :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat32,true);
  V_CMPX_LG_F32   :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat32,true);
  V_CMPX_GE_F32   :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat32,true);
  V_CMPX_O_F32    :emit_V_CMP(Op.OpOrdered               ,dtFloat32,true);
  V_CMPX_U_F32    :emit_V_CMP(Op.OpUnordered             ,dtFloat32,true);
  V_CMPX_NGE_F32  :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat32,true);
  V_CMPX_NLG_F32  :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat32,true);
  V_CMPX_NGT_F32  :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat32,true);
  V_CMPX_NLE_F32  :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat32,true);
  V_CMPX_NEQ_F32  :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat32,true);
  V_CMPX_NLT_F32  :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat32,true);

  //

  V_CMPS_LT_F32   :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat32,false);
  V_CMPS_EQ_F32   :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat32,false);
  V_CMPS_LE_F32   :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat32,false);
  V_CMPS_GT_F32   :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat32,false);
  V_CMPS_LG_F32   :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat32,false);
  V_CMPS_GE_F32   :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat32,false);
  V_CMPS_O_F32    :emit_V_CMP(Op.OpOrdered               ,dtFloat32,false);
  V_CMPS_U_F32    :emit_V_CMP(Op.OpUnordered             ,dtFloat32,false);
  V_CMPS_NGE_F32  :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat32,false);
  V_CMPS_NLG_F32  :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat32,false);
  V_CMPS_NGT_F32  :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat32,false);
  V_CMPS_NLE_F32  :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat32,false);
  V_CMPS_NEQ_F32  :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat32,false);
  V_CMPS_NLT_F32  :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat32,false);

  V_CMPSX_LT_F32  :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat32,true);
  V_CMPSX_EQ_F32  :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat32,true);
  V_CMPSX_LE_F32  :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat32,true);
  V_CMPSX_GT_F32  :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat32,true);
  V_CMPSX_LG_F32  :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat32,true);
  V_CMPSX_GE_F32  :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat32,true);
  V_CMPSX_O_F32   :emit_V_CMP(Op.OpOrdered               ,dtFloat32,true);
  V_CMPSX_U_F32   :emit_V_CMP(Op.OpUnordered             ,dtFloat32,true);
  V_CMPSX_NGE_F32 :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat32,true);
  V_CMPSX_NLG_F32 :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat32,true);
  V_CMPSX_NGT_F32 :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat32,true);
  V_CMPSX_NLE_F32 :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat32,true);
  V_CMPSX_NEQ_F32 :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat32,true);
  V_CMPSX_NLT_F32 :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat32,true);

  //

  V_CMP_LT_I32    :emit_V_CMP(Op.OpSLessThan             ,dtInt32,false);
  V_CMP_EQ_I32    :emit_V_CMP(Op.OpIEqual                ,dtInt32,false);
  V_CMP_LE_I32    :emit_V_CMP(Op.OpSLessThanEqual        ,dtInt32,false);
  V_CMP_GT_I32    :emit_V_CMP(Op.OpSGreaterThan          ,dtInt32,false);
  V_CMP_LG_I32    :emit_V_CMP(Op.OpINotEqual             ,dtInt32,false);
  V_CMP_GE_I32    :emit_V_CMP(Op.OpSGreaterThanEqual     ,dtInt32,false);

  V_CMPX_LT_I32   :emit_V_CMP(Op.OpSLessThan             ,dtInt32,true);
  V_CMPX_EQ_I32   :emit_V_CMP(Op.OpIEqual                ,dtInt32,true);
  V_CMPX_LE_I32   :emit_V_CMP(Op.OpSLessThanEqual        ,dtInt32,true);
  V_CMPX_GT_I32   :emit_V_CMP(Op.OpSGreaterThan          ,dtInt32,true);
  V_CMPX_LG_I32   :emit_V_CMP(Op.OpINotEqual             ,dtInt32,true);
  V_CMPX_GE_I32   :emit_V_CMP(Op.OpSGreaterThanEqual     ,dtInt32,true);

  V_CMP_LT_U32    :emit_V_CMP(Op.OpULessThan             ,dtUint32,false);
  V_CMP_EQ_U32    :emit_V_CMP(Op.OpIEqual                ,dtUint32,false);
  V_CMP_LE_U32    :emit_V_CMP(Op.OpULessThanEqual        ,dtUint32,false);
  V_CMP_GT_U32    :emit_V_CMP(Op.OpUGreaterThan          ,dtUint32,false);
  V_CMP_LG_U32    :emit_V_CMP(Op.OpINotEqual             ,dtUint32,false);
  V_CMP_GE_U32    :emit_V_CMP(Op.OpUGreaterThanEqual     ,dtUint32,false);

  V_CMPX_LT_U32   :emit_V_CMP(Op.OpULessThan             ,dtUint32,true);
  V_CMPX_EQ_U32   :emit_V_CMP(Op.OpIEqual                ,dtUint32,true);
  V_CMPX_LE_U32   :emit_V_CMP(Op.OpULessThanEqual        ,dtUint32,true);
  V_CMPX_GT_U32   :emit_V_CMP(Op.OpUGreaterThan          ,dtUint32,true);
  V_CMPX_LG_U32   :emit_V_CMP(Op.OpINotEqual             ,dtUint32,true);
  V_CMPX_GE_U32   :emit_V_CMP(Op.OpUGreaterThanEqual     ,dtUint32,true);

  //

  V_CMP_LT_F64    :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat64,false);
  V_CMP_EQ_F64    :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat64,false);
  V_CMP_LE_F64    :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat64,false);
  V_CMP_GT_F64    :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat64,false);
  V_CMP_LG_F64    :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat64,false);
  V_CMP_GE_F64    :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat64,false);
  V_CMP_O_F64     :emit_V_CMP(Op.OpOrdered               ,dtFloat64,false);
  V_CMP_U_F64     :emit_V_CMP(Op.OpUnordered             ,dtFloat64,false);
  V_CMP_NGE_F64   :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat64,false);
  V_CMP_NLG_F64   :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat64,false);
  V_CMP_NGT_F64   :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat64,false);
  V_CMP_NLE_F64   :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat64,false);
  V_CMP_NEQ_F64   :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat64,false);
  V_CMP_NLT_F64   :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat64,false);

  V_CMPX_LT_F64   :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat64,true);
  V_CMPX_EQ_F64   :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat64,true);
  V_CMPX_LE_F64   :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat64,true);
  V_CMPX_GT_F64   :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat64,true);
  V_CMPX_LG_F64   :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat64,true);
  V_CMPX_GE_F64   :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat64,true);
  V_CMPX_O_F64    :emit_V_CMP(Op.OpOrdered               ,dtFloat64,true);
  V_CMPX_U_F64    :emit_V_CMP(Op.OpUnordered             ,dtFloat64,true);
  V_CMPX_NGE_F64  :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat64,true);
  V_CMPX_NLG_F64  :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat64,true);
  V_CMPX_NGT_F64  :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat64,true);
  V_CMPX_NLE_F64  :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat64,true);
  V_CMPX_NEQ_F64  :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat64,true);
  V_CMPX_NLT_F64  :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat64,true);

  //

  V_CMPS_LT_F64   :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat64,false);
  V_CMPS_EQ_F64   :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat64,false);
  V_CMPS_LE_F64   :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat64,false);
  V_CMPS_GT_F64   :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat64,false);
  V_CMPS_LG_F64   :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat64,false);
  V_CMPS_GE_F64   :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat64,false);
  V_CMPS_O_F64    :emit_V_CMP(Op.OpOrdered               ,dtFloat64,false);
  V_CMPS_U_F64    :emit_V_CMP(Op.OpUnordered             ,dtFloat64,false);
  V_CMPS_NGE_F64  :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat64,false);
  V_CMPS_NLG_F64  :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat64,false);
  V_CMPS_NGT_F64  :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat64,false);
  V_CMPS_NLE_F64  :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat64,false);
  V_CMPS_NEQ_F64  :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat64,false);
  V_CMPS_NLT_F64  :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat64,false);

  V_CMPSX_LT_F64  :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat64,true);
  V_CMPSX_EQ_F64  :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat64,true);
  V_CMPSX_LE_F64  :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat64,true);
  V_CMPSX_GT_F64  :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat64,true);
  V_CMPSX_LG_F64  :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat64,true);
  V_CMPSX_GE_F64  :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat64,true);
  V_CMPSX_O_F64   :emit_V_CMP(Op.OpOrdered               ,dtFloat64,true);
  V_CMPSX_U_F64   :emit_V_CMP(Op.OpUnordered             ,dtFloat64,true);
  V_CMPSX_NGE_F64 :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat64,true);
  V_CMPSX_NLG_F64 :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat64,true);
  V_CMPSX_NGT_F64 :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat64,true);
  V_CMPSX_NLE_F64 :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat64,true);
  V_CMPSX_NEQ_F64 :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat64,true);
  V_CMPSX_NLT_F64 :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat64,true);

  //

  V_CMP_LT_I64    :emit_V_CMP(Op.OpSLessThan             ,dtInt64,false);
  V_CMP_EQ_I64    :emit_V_CMP(Op.OpIEqual                ,dtInt64,false);
  V_CMP_LE_I64    :emit_V_CMP(Op.OpSLessThanEqual        ,dtInt64,false);
  V_CMP_GT_I64    :emit_V_CMP(Op.OpSGreaterThan          ,dtInt64,false);
  V_CMP_LG_I64    :emit_V_CMP(Op.OpINotEqual             ,dtInt64,false);
  V_CMP_GE_I64    :emit_V_CMP(Op.OpSGreaterThanEqual     ,dtInt64,false);

  V_CMPX_LT_I64   :emit_V_CMP(Op.OpSLessThan             ,dtInt64,true);
  V_CMPX_EQ_I64   :emit_V_CMP(Op.OpIEqual                ,dtInt64,true);
  V_CMPX_LE_I64   :emit_V_CMP(Op.OpSLessThanEqual        ,dtInt64,true);
  V_CMPX_GT_I64   :emit_V_CMP(Op.OpSGreaterThan          ,dtInt64,true);
  V_CMPX_LG_I64   :emit_V_CMP(Op.OpINotEqual             ,dtInt64,true);
  V_CMPX_GE_I64   :emit_V_CMP(Op.OpSGreaterThanEqual     ,dtInt64,true);

  V_CMP_LT_U64    :emit_V_CMP(Op.OpULessThan             ,dtUint64,false);
  V_CMP_EQ_U64    :emit_V_CMP(Op.OpIEqual                ,dtUint64,false);
  V_CMP_LE_U64    :emit_V_CMP(Op.OpULessThanEqual        ,dtUint64,false);
  V_CMP_GT_U64    :emit_V_CMP(Op.OpUGreaterThan          ,dtUint64,false);
  V_CMP_LG_U64    :emit_V_CMP(Op.OpINotEqual             ,dtUint64,false);
  V_CMP_GE_U64    :emit_V_CMP(Op.OpUGreaterThanEqual     ,dtUint64,false);

  V_CMPX_LT_U64   :emit_V_CMP(Op.OpULessThan             ,dtUint64,true);
  V_CMPX_EQ_U64   :emit_V_CMP(Op.OpIEqual                ,dtUint64,true);
  V_CMPX_LE_U64   :emit_V_CMP(Op.OpULessThanEqual        ,dtUint64,true);
  V_CMPX_GT_U64   :emit_V_CMP(Op.OpUGreaterThan          ,dtUint64,true);
  V_CMPX_LG_U64   :emit_V_CMP(Op.OpINotEqual             ,dtUint64,true);
  V_CMPX_GE_U64   :emit_V_CMP(Op.OpUGreaterThanEqual     ,dtUint64,true);

  //

  V_CMP_CLASS_F32 :emit_V_CMP_CLASS_32(false);
  V_CMPX_CLASS_F32:emit_V_CMP_CLASS_32(true );

  //

  else
   Assert(false,'VOP3c?'+IntToStr(FSPI.VOP3a.OP)+' '+get_str_spi(FSPI));
 end;

end;

procedure TEmit_VOP3.emit_V_ADDC_U32;
Var
 dst,car:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 //exc:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3b.VDST);
 car:=get_sdst7(FSPI.VOP3b.SDST);

 Assert(FSPI.VOP3b.OMOD=0,'FSPI.VOP3b.OMOD');
 Assert(FSPI.VOP3b.NEG =0,'FSPI.VOP3b.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3b.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3b.SRC1,dtUInt32);
 src[2]:=fetch_ssrc9(FSPI.VOP3b.SRC2,dtUInt32);

 src[2]:=OpAndTo(src[2],1);
 src[2].PrepType(ord(dtUInt32));

 OpIAddExt(dst,car,src[0],src[1],dtUInt32); //src0+src1

 src[0]:=MakeRead(dst,dtUInt32);
 src[1]:=MakeRead(car,dtUInt32);   //save car1

 OpIAddExt(dst,car,src[0],src[2],dtUInt32); //(src0+src1)+src2

 src[0]:=MakeRead(car,dtUInt32);

 OpBitwiseOr(car,src[1],src[0]);   //car1 or car2

 {
  TODO:
  if (EXEC[i]) {
    V_ADDC_U32
    SDST[i] = bor;
  }
  else {
    SDST[i] = 0;
  }
 }

 //src[0]:=MakeRead(car,dtUInt32);

 //exc:=MakeRead(get_exec0,dtUnknow);
 //OpBitwiseAnd(car,src[0],exc);     //carry_out & EXEC
end;

procedure TEmit_VOP3.emit_V_SUBB_U32;
Var
 dst,bor:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
 //exc:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3b.VDST);
 bor:=get_sdst7(FSPI.VOP3b.SDST);

 Assert(FSPI.VOP3b.OMOD=0,'FSPI.VOP3b.OMOD');
 Assert(FSPI.VOP3b.NEG =0,'FSPI.VOP3b.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3b.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3b.SRC1,dtUInt32);
 src[2]:=fetch_ssrc9(FSPI.VOP3b.SRC2,dtUInt32);

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

procedure TEmit_VOP3.emit_V_ADD_I32;
Var
 dst,car:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 //exc:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3b.VDST);
 car:=get_sdst7(FSPI.VOP3b.SDST);

 Assert(FSPI.VOP3b.OMOD=0,'FSPI.VOP3b.OMOD');
 Assert(FSPI.VOP3b.NEG =0,'FSPI.VOP3b.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3b.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3b.SRC1,dtUInt32);

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

procedure TEmit_VOP3.emit_V_SUB_I32;
Var
 dst,bor:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 //exc:TsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3b.VDST);
 bor:=get_sdst7(FSPI.VOP3b.SDST);

 //ignored
 //Assert(FSPI.VOP3b.OMOD=0,'FSPI.VOP3b.OMOD');
 //Assert(FSPI.VOP3b.NEG =0,'FSPI.VOP3b.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3b.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3b.SRC1,dtUInt32);

 OpISubExt(dst,bor,src[0],src[1],dtUInt32);

 {
  TODO:
  if (EXEC[i]) {
    V_SUB_I32
    SDST[i] = bor;
  }
  else {
    SDST[i] = 0;
  }
 }

 //exc:=MakeRead(get_exec0,dtUnknow);
 //OpBitwiseAnd(bor,bor^.current,exc); //borrow_out & EXEC
end;

procedure TEmit_VOP3.emit_VOP3b;
begin
 Case FSPI.VOP3b.OP of
  256+V_ADDC_U32: emit_V_ADDC_U32;
  256+V_SUBB_U32: emit_V_SUBB_U32;

  256+V_ADD_I32 : emit_V_ADD_I32;
  256+V_SUB_I32 : emit_V_SUB_I32;

  else
   Assert(false,'VOP3b?'+IntToStr(FSPI.VOP3b.OP)+' '+get_str_spi(FSPI));
 end;
end;

procedure TEmit_VOP3.emit_VOP3a;
begin
 Case FSPI.VOP3a.OP of

  //VOP2 analog

  256+V_CNDMASK_B32: emit_V_CNDMASK_B32;

  256+V_ADD_F32    : emit_V2_F32(Op.OpFAdd,False);
  256+V_SUB_F32    : emit_V2_F32(Op.OpFSub,False);
  256+V_SUBREV_F32 : emit_V2_F32(Op.OpFSub,True );

  256+V_LSHL_B32   : emit_V_SH(Op.OpShiftLeftLogical    ,dtUint32,False);
  256+V_LSHLREV_B32: emit_V_SH(Op.OpShiftLeftLogical    ,dtUint32,True );
  256+V_LSHR_B32   : emit_V_SH(Op.OpShiftRightLogical   ,dtUint32,False);
  256+V_LSHRREV_B32: emit_V_SH(Op.OpShiftRightLogical   ,dtUint32,True );
  256+V_ASHR_I32   : emit_V_SH(Op.OpShiftRightArithmetic,dtInt32 ,False);
  256+V_ASHRREV_I32: emit_V_SH(Op.OpShiftRightArithmetic,dtInt32 ,True );

  256+V_CVT_PKNORM_I16_F32:emit_V_CVT_PKNORM_I16_F32;
  256+V_CVT_PKNORM_U16_F32:emit_V_CVT_PKNORM_U16_F32;
  256+V_CVT_PKRTZ_F16_F32: emit_V_CVT_PKRTZ_F16_F32;

  256+V_MIN_LEGACY_F32:emit_V_MMX(GlslOp.NMin,dtFloat32);
  256+V_MAX_LEGACY_F32:emit_V_MMX(GlslOp.NMax,dtFloat32);

  256+V_MIN_F32:emit_V_MMX(GlslOp.FMin,dtFloat32);
  256+V_MAX_F32:emit_V_MMX(GlslOp.FMax,dtFloat32);

  256+V_MIN_I32:emit_V_MMX(GlslOp.SMin,dtInt32);
  256+V_MAX_I32:emit_V_MMX(GlslOp.SMax,dtInt32);

  256+V_MIN_U32:emit_V_MMX(GlslOp.UMin,dtUint32);
  256+V_MAX_U32:emit_V_MMX(GlslOp.UMax,dtUint32);

  256+V_MUL_LEGACY_F32: emit_V_MUL_LEGACY_F32;

  256+V_MUL_F32: emit_V2_F32(Op.OpFMul,False);

  256+V_MUL_I32_I24: emit_V_MUL_I32_I24;
  256+V_MUL_U32_U24: emit_V_MUL_U32_U24;

  256+V_MAC_F32: emit_V_MAC_F32;

  256+V_LDEXP_F32: emit_V_LDEXP_F32;

  256+V_BCNT_U32_B32: emit_V_BCNT_U32_B32;

  256+V_MBCNT_LO_U32_B32: emit_V_MBCNT_LO_U32_B32;
  256+V_MBCNT_HI_U32_B32: emit_V_MBCNT_HI_U32_B32;

  256+V_BFM_B32: emit_V_BFM_B32;

  //VOP3 only

  V_MUL_LO_U32: emit_V_MUL_LO(dtUint32);
  V_MUL_HI_U32: emit_V_MUL_HI(dtUint32);

  V_MUL_LO_I32: emit_V_MUL_LO(dtInt32);
  V_MUL_HI_I32: emit_V_MUL_HI(dtInt32);

  V_BFE_U32: emit_V_BFE_U32;
  V_BFE_I32: emit_V_BFE_I32;
  V_BFI_B32: emit_V_BFI_B32;

  V_MAD_F32: emit_V_MAD_F32;
  V_MAD_LEGACY_F32: emit_V_MAD_LEGACY_F32;

  V_MAD_I32_I24: emit_V_MAD_I32_I24;
  V_MAD_U32_U24: emit_V_MAD_U32_U24;
  V_MAD_U64_U32: emit_V_MAD_U64_U32;

  V_SAD_U32 : emit_V_SAD_U32;

  V_MIN3_F32: emit_V_MMX3(GlslOp.FMin,dtFloat32);
  V_MAX3_F32: emit_V_MMX3(GlslOp.FMax,dtFloat32);

  V_MIN3_I32: emit_V_MMX3(GlslOp.SMin,dtInt32);
  V_MAX3_I32: emit_V_MMX3(GlslOp.SMax,dtInt32);

  V_MIN3_U32: emit_V_MMX3(GlslOp.UMin,dtUint32);
  V_MAX3_U32: emit_V_MMX3(GlslOp.UMax,dtUint32);

  V_MED3_F32: emit_V_MED3_F32;
  V_MED3_I32: emit_V_MED3_I32;
  V_MED3_U32: emit_V_MED3_U32;
  V_FMA_F32 : emit_V_FMA_F32;

  V_MIN_F64:emit_V_MMX64(GlslOp.FMin);
  V_MAX_F64:emit_V_MMX64(GlslOp.FMax);

  V_CUBEID_F32:emit_V_CUBE(V_CUBEID_F32);
  V_CUBESC_F32:emit_V_CUBE(V_CUBESC_F32);
  V_CUBETC_F32:emit_V_CUBE(V_CUBETC_F32);
  V_CUBEMA_F32:emit_V_CUBE(V_CUBEMA_F32);

  //VOP1 analog

  384+V_NOP:;

  384+V_CVT_F32_I32: emit_V_CVT(Op.OpConvertSToF,dtFloat32,dtInt32);
  384+V_CVT_F32_U32: emit_V_CVT(Op.OpConvertUToF,dtFloat32,dtUInt32);
  384+V_CVT_U32_F32: emit_V_CVT(Op.OpConvertFToU,dtUInt32 ,dtFloat32);
  384+V_CVT_I32_F32: emit_V_CVT(Op.OpConvertFToS,dtInt32  ,dtFloat32);

  384+V_CVT_F16_F32: emit_V_CVT_F16_F32;
  384+V_CVT_F32_F16: emit_V_CVT_F32_F16;

  384+V_MOV_B32  : emit_V_MOV_B32;

  384+V_FRACT_F32: emit_V_EXT_F32(GlslOp.Fract);
  384+V_TRUNC_F32: emit_V_EXT_F32(GlslOp.Trunc);
  384+V_CEIL_F32 : emit_V_EXT_F32(GlslOp.Ceil);

  384+V_RNDNE_F32: emit_V_EXT_F32(GlslOp.RoundEven);
  384+V_FLOOR_F32: emit_V_EXT_F32(GlslOp.Floor);
  384+V_EXP_F32  : emit_V_EXT_F32(GlslOp.Exp2);
  384+V_LOG_F32  : emit_V_EXT_F32(GlslOp.Log2);

  384+V_RSQ_F32  : emit_V_EXT_F32(GlslOp.InverseSqrt);

  384+V_SQRT_F32 : emit_V_EXT_F32(GlslOp.Sqrt);

  384+V_SIN_F32  : emit_V_SIN_COS(GlslOp.Sin);
  384+V_COS_F32  : emit_V_SIN_COS(GlslOp.Cos);

  384+V_RCP_F32  : emit_V_RCP_F32;
  384+V_RCP_IFLAG_F32: emit_V_RCP_F32;

  else
   Assert(false,'VOP3a?'+IntToStr(FSPI.VOP3a.OP)+' '+get_str_spi(FSPI));
 end;

end;

end.

