unit emit_VOP3;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  spirv,
  srType,
  srReg,
  srOpUtils,
  emit_fetch;

type
 TEmit_VOP3=class(TEmitFetch)
  procedure emit_VOP3c;
  procedure emit_VOP3b;
  procedure emit_VOP3a;
  procedure emit_V_CMP_32(OpId:DWORD;rtype:TsrDataType;x:Boolean);
  procedure emit_V_CMP_C(r,x:Boolean);

  procedure emit_src_neg_bit(src:PPsrRegNode;count:Byte);
  procedure emit_src_abs(src:PPsrRegNode);
  procedure emit_src_abs_bit(src:PPsrRegNode;count:Byte);
  procedure emit_dst_omod_f(dst:PsrRegSlot);
  procedure emit_dst_clamp_f(dst:PsrRegSlot);

  procedure emit_V_CNDMASK_B32;
  procedure emit_V2_F32(OpId:DWORD);
  procedure emit_V_CVT_PKRTZ_F16_F32;
  procedure emit_V_MMX_F32(OpId:DWORD);
  procedure emit_V_MUL_LO_I32;
  procedure emit_V_MUL_I32_I24;
  procedure emit_V_MUL_U32_U24;
  procedure emit_V_MUL_HI_U32;
  procedure emit_V_MAC_F32;

  procedure emit_V_BFE_U32;
  procedure emit_V_BFI_B32;
  procedure emit_V_MAD_F32;
  procedure emit_V_MAD_I32_I24;
  procedure emit_V_MAD_U32_U24;
  procedure emit_V_SAD_U32;
  procedure emit_V_MAX3_F32;
  procedure emit_V_MIN3_F32;
  procedure emit_V_MED3_F32;
  procedure emit_V_FMA_F32;
  procedure emit_V_CUBE(OpId:DWORD);
  procedure emit_V_MOV_B32;
  procedure emit_V_EXT_F32(OpId:DWORD);
  procedure emit_V_SIN_COS(OpId:DWORD);
  procedure emit_V_RCP_F32;
 end;

implementation

procedure TEmit_VOP3.emit_V_CMP_32(OpId:DWORD;rtype:TsrDataType;x:Boolean);
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 if not get_sdst7_pair(FSPI.VOP3a.VDST,@dst) then Exit;

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,rtype);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,rtype);

 emit_src_abs_bit(@src,2);
 emit_src_neg_bit(@src,2);

 OpCmpV(OpId,dst[0],src[0],src[1]);

 SetConst_q(dst[1],dtUnknow,0); //set zero

 if x then
 begin
  MakeCopy(get_exec0,dst[0]^.current);
  SetConst_q(get_exec1,dtUnknow,0);     //set zero
 end;
end;

procedure TEmit_VOP3.emit_V_CMP_C(r,x:Boolean);
Var
 dst:array[0..1] of PsrRegSlot;
begin
 if not get_sdst7_pair(FSPI.VOP3a.VDST,@dst) then Exit;

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 SetConst_b(dst[0],r);
 SetConst_q(dst[1],dtUnknow,0); //set zero

 if x then
 begin
  MakeCopy(get_exec0,dst[0]^.current);
  SetConst_q(get_exec1,dtUnknow,0);     //set zero
 end;
end;

procedure TEmit_VOP3.emit_src_neg_bit(src:PPsrRegNode;count:Byte);
var
 i:Byte;
 dst:PsrRegNode;
begin
 if (FSPI.VOP3a.NEG=0) then Exit;

 For i:=0 to count-1 do
  if Byte(FSPI.VOP3a.NEG).TestBit(i) then
  begin
   dst:=NewReg(dtFloat32);
   _Op1(line,Op.OpFNegate,dst,src[i]);
   src[i]:=dst;
  end;
end;

procedure TEmit_VOP3.emit_src_abs(src:PPsrRegNode);
var
 dst:PsrRegNode;
begin
 dst:=NewReg(dtFloat32);
 _OpGlsl1(line,GlslOp.FAbs,dst,src^);
 src^:=dst;
end;

procedure TEmit_VOP3.emit_src_abs_bit(src:PPsrRegNode;count:Byte);
var
 i:Byte;
begin
 if (FSPI.VOP3a.ABS=0) then Exit;

 For i:=0 to count-1 do
  if Byte(FSPI.VOP3a.ABS).TestBit(i) then
  begin
   emit_src_abs(@src[i]);
  end;
end;

procedure TEmit_VOP3.emit_dst_omod_f(dst:PsrRegSlot);
var
 src,tmp:PsrRegNode;
begin
 src:=dst^.current;

 Case FSPI.VOP3a.OMOD of
  0:;
  1: // *2
    begin
     tmp:=NewReg_s(dtFloat32,2);
     Op2(Op.OpFMul,dtFloat32,dst,src,tmp);
    end;
  2: // *4
    begin
     tmp:=NewReg_s(dtFloat32,4);
     Op2(Op.OpFMul,dtFloat32,dst,src,tmp);
    end;
  3: // /2
    begin
     tmp:=NewReg_s(dtFloat32,2);
     Op2(Op.OpFDiv,dtFloat32,dst,src,tmp);
    end;
 end;

end;

procedure TEmit_VOP3.emit_dst_clamp_f(dst:PsrRegSlot);
var
 src,min,max:PsrRegNode;
begin
 if (FSPI.VOP3a.CLAMP=0) then Exit;

 src:=dst^.current;
 min:=NewReg_s(dtFloat32,0);
 max:=NewReg_s(dtFloat32,1);

 OpGlsl3(GlslOp.FClamp,dtFloat32,dst,src,min,max);
end;

procedure TEmit_VOP3.emit_V_CNDMASK_B32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUnknow);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUnknow);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtBool);

 emit_src_abs_bit(@src,2);
 emit_src_neg_bit(@src,2);

 OpSelect(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3.emit_V2_F32(OpId:DWORD);
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 emit_src_abs_bit(@src,2);
 emit_src_neg_bit(@src,2);

 Op2(OpId,dtFloat32,dst,src[0],src[1]);

 emit_dst_omod_f(dst);
 emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3.emit_V_CVT_PKRTZ_F16_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 emit_src_abs_bit(@src,2);
 emit_src_neg_bit(@src,2);

 OpConvFloatToHalf2(dst,src[0],src[1]);
end;

procedure TEmit_VOP3.emit_V_MMX_F32(OpId:DWORD);
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 emit_src_abs_bit(@src,2);
 emit_src_neg_bit(@src,2);

 OpGlsl2(OpId,dtFloat32,dst,src[0],src[1]);

 emit_dst_omod_f(dst);
 emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3.emit_V_MUL_LO_I32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtInt32);

 OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP3.emit_V_MUL_I32_I24;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 bit24:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtInt32);

 bit24:=NewReg_q(dtUInt32,$FFFFFF);

 src[0]:=OpBitwiseAndTo(src[0],bit24);
 src[0]^.PrepType(ord(dtInt32));

 src[1]:=OpBitwiseAndTo(src[1],bit24);
 src[1]^.PrepType(ord(dtInt32));

 OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP3.emit_V_MUL_U32_U24;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 bit24:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUInt32);

 bit24:=NewReg_q(dtUInt32,$FFFFFF);

 src[0]:=OpBitwiseAndTo(src[0],bit24);
 src[0]^.PrepType(ord(dtUInt32));

 src[1]:=OpBitwiseAndTo(src[1],bit24);
 src[1]^.PrepType(ord(dtUInt32));

 OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP3.emit_V_MUL_HI_U32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 tmp_r,dst_r:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUInt32);

 tmp_r:=NewReg(dtStruct2u);
 _Op2(line,Op.OpUMulExtended,tmp_r,src[0],src[1]);

 dst_r:=dst^.New(line,dtUInt32);

 OpExtract(line,dst_r,tmp_r,1);
end;

procedure TEmit_VOP3.emit_V_MAC_F32; //vdst = vsrc0.f * vsrc1.f + vdst.f  -> fma
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=MakeRead(dst,dtFloat32);

 emit_src_abs_bit(@src,3);
 emit_src_neg_bit(@src,3);

 OpFmaF32(dst,src[0],src[1],src[2]);

 emit_dst_omod_f(dst);
 emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3.emit_V_BFE_U32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUint32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUint32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUint32);

 OpBFEU32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3.emit_V_BFI_B32;
Var
 dst:PsrRegSlot;
 bitmsk:PsrRegNode;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 bitmsk:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUint32);
 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUint32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUint32);

 OpBFIB32(dst,bitmsk,src[0],src[1]);
end;

procedure TEmit_VOP3.emit_V_MAD_F32; //vdst = vsrc0.f * vsrc1.f + vadd.f
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 emit_src_abs_bit(@src,3);
 emit_src_neg_bit(@src,3);

 OpFmaF32(dst,src[0],src[1],src[2]);

 emit_dst_omod_f(dst);
 emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3.emit_V_MAD_I32_I24;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
 bit24:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtInt32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtInt32);

 bit24:=NewReg_q(dtUInt32,$FFFFFF);

 src[0]:=OpBitwiseAndTo(src[0],bit24);
 src[0]^.PrepType(ord(dtInt32));

 src[1]:=OpBitwiseAndTo(src[1],bit24);
 src[1]^.PrepType(ord(dtInt32));

 OpFmaI32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3.emit_V_MAD_U32_U24;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
 bit24:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUInt32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUInt32);

 bit24:=NewReg_q(dtUInt32,$FFFFFF);

 src[0]:=OpBitwiseAndTo(src[0],bit24);
 src[0]^.PrepType(ord(dtUInt32));

 src[1]:=OpBitwiseAndTo(src[1],bit24);
 src[1]^.PrepType(ord(dtUInt32));

 OpFmaU32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3.emit_V_SAD_U32; //dst.u = abs(vsrc0.u - vsrc1.u) + vaccum.u
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
 rdif,rvac:PsrRegNode;
 //msk:PsrRegNode;
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

 //msk:=NewReg(pConsts^.Fetch(dtUInt32,$FFFF)); mask need?
 //OpBitwiseAnd(tmp,src[2],msk);
 //rvac:=tmp^.current;
 rvac:=src[2];

 OpIAdd(dst,rdif,rvac);
end;

procedure TEmit_VOP3.emit_V_MAX3_F32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 emit_src_abs_bit(@src,3);
 emit_src_neg_bit(@src,3);

 OpGlsl2(GlslOp.FMax,dtFloat32,dst,OpFMaxTo(src[0],src[1]),src[2]);

 emit_dst_omod_f(dst);
 emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3.emit_V_MIN3_F32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 emit_src_abs_bit(@src,3);
 emit_src_neg_bit(@src,3);

 OpGlsl2(GlslOp.FMin,dtFloat32,dst,OpFMinTo(src[0],src[1]),src[2]);

 emit_dst_omod_f(dst);
 emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3.emit_V_MED3_F32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 emit_src_abs_bit(@src,3);
 emit_src_neg_bit(@src,3);

 OpMED3F(dst,src[0],src[1],src[2]);

 emit_dst_omod_f(dst);
 emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3.emit_V_FMA_F32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 emit_src_abs_bit(@src,3);
 emit_src_neg_bit(@src,3);

 OpFmaF32(dst,src[0],src[1],src[2]);

 emit_dst_omod_f(dst);
 emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3.emit_V_CUBE(OpId:DWORD);
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 Op3(OpId,dtFloat32,dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3.emit_V_MOV_B32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 if (Byte(FSPI.VOP3a.ABS).TestBit(0)) or
    (Byte(FSPI.VOP3a.NEG).TestBit(0)) then
 begin
  src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);

  emit_src_abs_bit(@src,1);
  emit_src_neg_bit(@src,1);

  MakeCopy(dst,src);
 end else
 begin
  src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUnknow);

  MakeCopy(dst,src);
 end;

end;

procedure TEmit_VOP3.emit_V_EXT_F32(OpId:DWORD);
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);

 emit_src_abs_bit(@src,1);
 emit_src_neg_bit(@src,1);

 OpGlsl1(OpId,dtFloat32,dst,src);

 emit_dst_omod_f(dst);
 emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3.emit_V_SIN_COS(OpId:DWORD);
const
 PI2:Single=2*PI;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);

 src:=OpFMulToS(src,PI2);

 emit_src_abs_bit(@src,1);
 emit_src_neg_bit(@src,1);

 OpGlsl1(OpId,dtFloat32,dst,src);

 emit_dst_omod_f(dst);
 emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3.emit_V_RCP_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
 one:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP3a.VDST);

 src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);

 emit_src_abs_bit(@src,1);
 emit_src_neg_bit(@src,1);

 one:=NewReg_s(dtFloat32,1);

 Op2(Op.OpFDiv,dtFloat32,dst,one,src);

 emit_dst_omod_f(dst);
 emit_dst_clamp_f(dst);
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

  V_CMP_LT_F32    :emit_V_CMP_32(Op.OpFOrdLessThan          ,dtFloat32,false);
  V_CMP_EQ_F32    :emit_V_CMP_32(Op.OpFOrdEqual             ,dtFloat32,false);
  V_CMP_LE_F32    :emit_V_CMP_32(Op.OpFOrdLessThanEqual     ,dtFloat32,false);
  V_CMP_GT_F32    :emit_V_CMP_32(Op.OpFOrdGreaterThan       ,dtFloat32,false);
  V_CMP_LG_F32    :emit_V_CMP_32(Op.OpFOrdNotEqual          ,dtFloat32,false);
  V_CMP_GE_F32    :emit_V_CMP_32(Op.OpFOrdGreaterThanEqual  ,dtFloat32,false);
  V_CMP_O_F32     :emit_V_CMP_32(Op.OpOrdered               ,dtFloat32,false);
  V_CMP_U_F32     :emit_V_CMP_32(Op.OpUnordered             ,dtFloat32,false);
  V_CMP_NGE_F32   :emit_V_CMP_32(Op.OpFUnordLessThan        ,dtFloat32,false);
  V_CMP_NLG_F32   :emit_V_CMP_32(Op.OpFUnordEqual           ,dtFloat32,false);
  V_CMP_NGT_F32   :emit_V_CMP_32(Op.OpFUnordLessThanEqual   ,dtFloat32,false);
  V_CMP_NLE_F32   :emit_V_CMP_32(Op.OpFUnordGreaterThan     ,dtFloat32,false);
  V_CMP_NEQ_F32   :emit_V_CMP_32(Op.OpFUnordNotEqual        ,dtFloat32,false);
  V_CMP_NLT_F32   :emit_V_CMP_32(Op.OpFUnordGreaterThanEqual,dtFloat32,false);

  V_CMPX_LT_F32   :emit_V_CMP_32(Op.OpFOrdLessThan          ,dtFloat32,true);
  V_CMPX_EQ_F32   :emit_V_CMP_32(Op.OpFOrdEqual             ,dtFloat32,true);
  V_CMPX_LE_F32   :emit_V_CMP_32(Op.OpFOrdLessThanEqual     ,dtFloat32,true);
  V_CMPX_GT_F32   :emit_V_CMP_32(Op.OpFOrdGreaterThan       ,dtFloat32,true);
  V_CMPX_LG_F32   :emit_V_CMP_32(Op.OpFOrdNotEqual          ,dtFloat32,true);
  V_CMPX_GE_F32   :emit_V_CMP_32(Op.OpFOrdGreaterThanEqual  ,dtFloat32,true);
  V_CMPX_O_F32    :emit_V_CMP_32(Op.OpOrdered               ,dtFloat32,true);
  V_CMPX_U_F32    :emit_V_CMP_32(Op.OpUnordered             ,dtFloat32,true);
  V_CMPX_NGE_F32  :emit_V_CMP_32(Op.OpFUnordLessThan        ,dtFloat32,true);
  V_CMPX_NLG_F32  :emit_V_CMP_32(Op.OpFUnordEqual           ,dtFloat32,true);
  V_CMPX_NGT_F32  :emit_V_CMP_32(Op.OpFUnordLessThanEqual   ,dtFloat32,true);
  V_CMPX_NLE_F32  :emit_V_CMP_32(Op.OpFUnordGreaterThan     ,dtFloat32,true);
  V_CMPX_NEQ_F32  :emit_V_CMP_32(Op.OpFUnordNotEqual        ,dtFloat32,true);
  V_CMPX_NLT_F32  :emit_V_CMP_32(Op.OpFUnordGreaterThanEqual,dtFloat32,true);

  //

  V_CMPS_LT_F32   :emit_V_CMP_32(Op.OpFOrdLessThan          ,dtFloat32,false);
  V_CMPS_EQ_F32   :emit_V_CMP_32(Op.OpFOrdEqual             ,dtFloat32,false);
  V_CMPS_LE_F32   :emit_V_CMP_32(Op.OpFOrdLessThanEqual     ,dtFloat32,false);
  V_CMPS_GT_F32   :emit_V_CMP_32(Op.OpFOrdGreaterThan       ,dtFloat32,false);
  V_CMPS_LG_F32   :emit_V_CMP_32(Op.OpFOrdNotEqual          ,dtFloat32,false);
  V_CMPS_GE_F32   :emit_V_CMP_32(Op.OpFOrdGreaterThanEqual  ,dtFloat32,false);
  V_CMPS_O_F32    :emit_V_CMP_32(Op.OpOrdered               ,dtFloat32,false);
  V_CMPS_U_F32    :emit_V_CMP_32(Op.OpUnordered             ,dtFloat32,false);
  V_CMPS_NGE_F32  :emit_V_CMP_32(Op.OpFUnordLessThan        ,dtFloat32,false);
  V_CMPS_NLG_F32  :emit_V_CMP_32(Op.OpFUnordEqual           ,dtFloat32,false);
  V_CMPS_NGT_F32  :emit_V_CMP_32(Op.OpFUnordLessThanEqual   ,dtFloat32,false);
  V_CMPS_NLE_F32  :emit_V_CMP_32(Op.OpFUnordGreaterThan     ,dtFloat32,false);
  V_CMPS_NEQ_F32  :emit_V_CMP_32(Op.OpFUnordNotEqual        ,dtFloat32,false);
  V_CMPS_NLT_F32  :emit_V_CMP_32(Op.OpFUnordGreaterThanEqual,dtFloat32,false);

  V_CMPSX_LT_F32  :emit_V_CMP_32(Op.OpFOrdLessThan          ,dtFloat32,true);
  V_CMPSX_EQ_F32  :emit_V_CMP_32(Op.OpFOrdEqual             ,dtFloat32,true);
  V_CMPSX_LE_F32  :emit_V_CMP_32(Op.OpFOrdLessThanEqual     ,dtFloat32,true);
  V_CMPSX_GT_F32  :emit_V_CMP_32(Op.OpFOrdGreaterThan       ,dtFloat32,true);
  V_CMPSX_LG_F32  :emit_V_CMP_32(Op.OpFOrdNotEqual          ,dtFloat32,true);
  V_CMPSX_GE_F32  :emit_V_CMP_32(Op.OpFOrdGreaterThanEqual  ,dtFloat32,true);
  V_CMPSX_O_F32   :emit_V_CMP_32(Op.OpOrdered               ,dtFloat32,true);
  V_CMPSX_U_F32   :emit_V_CMP_32(Op.OpUnordered             ,dtFloat32,true);
  V_CMPSX_NGE_F32 :emit_V_CMP_32(Op.OpFUnordLessThan        ,dtFloat32,true);
  V_CMPSX_NLG_F32 :emit_V_CMP_32(Op.OpFUnordEqual           ,dtFloat32,true);
  V_CMPSX_NGT_F32 :emit_V_CMP_32(Op.OpFUnordLessThanEqual   ,dtFloat32,true);
  V_CMPSX_NLE_F32 :emit_V_CMP_32(Op.OpFUnordGreaterThan     ,dtFloat32,true);
  V_CMPSX_NEQ_F32 :emit_V_CMP_32(Op.OpFUnordNotEqual        ,dtFloat32,true);
  V_CMPSX_NLT_F32 :emit_V_CMP_32(Op.OpFUnordGreaterThanEqual,dtFloat32,true);

  //

  V_CMP_LT_I32    :emit_V_CMP_32(Op.OpSLessThan             ,dtInt32,false);
  V_CMP_EQ_I32    :emit_V_CMP_32(Op.OpIEqual                ,dtInt32,false);
  V_CMP_LE_I32    :emit_V_CMP_32(Op.OpSLessThanEqual        ,dtInt32,false);
  V_CMP_GT_I32    :emit_V_CMP_32(Op.OpSGreaterThan          ,dtInt32,false);
  V_CMP_LG_I32    :emit_V_CMP_32(Op.OpINotEqual             ,dtInt32,false);
  V_CMP_GE_I32    :emit_V_CMP_32(Op.OpSGreaterThanEqual     ,dtInt32,false);

  V_CMPX_LT_I32   :emit_V_CMP_32(Op.OpSLessThan             ,dtInt32,true);
  V_CMPX_EQ_I32   :emit_V_CMP_32(Op.OpIEqual                ,dtInt32,true);
  V_CMPX_LE_I32   :emit_V_CMP_32(Op.OpSLessThanEqual        ,dtInt32,true);
  V_CMPX_GT_I32   :emit_V_CMP_32(Op.OpSGreaterThan          ,dtInt32,true);
  V_CMPX_LG_I32   :emit_V_CMP_32(Op.OpINotEqual             ,dtInt32,true);
  V_CMPX_GE_I32   :emit_V_CMP_32(Op.OpSGreaterThanEqual     ,dtInt32,true);

  V_CMP_LT_U32    :emit_V_CMP_32(Op.OpULessThan             ,dtUint32,false);
  V_CMP_EQ_U32    :emit_V_CMP_32(Op.OpIEqual                ,dtUint32,false);
  V_CMP_LE_U32    :emit_V_CMP_32(Op.OpULessThanEqual        ,dtUint32,false);
  V_CMP_GT_U32    :emit_V_CMP_32(Op.OpUGreaterThan          ,dtUint32,false);
  V_CMP_LG_U32    :emit_V_CMP_32(Op.OpINotEqual             ,dtUint32,false);
  V_CMP_GE_U32    :emit_V_CMP_32(Op.OpUGreaterThanEqual     ,dtUint32,false);

  V_CMPX_LT_U32   :emit_V_CMP_32(Op.OpULessThan             ,dtUint32,true);
  V_CMPX_EQ_U32   :emit_V_CMP_32(Op.OpIEqual                ,dtUint32,true);
  V_CMPX_LE_U32   :emit_V_CMP_32(Op.OpULessThanEqual        ,dtUint32,true);
  V_CMPX_GT_U32   :emit_V_CMP_32(Op.OpUGreaterThan          ,dtUint32,true);
  V_CMPX_LG_U32   :emit_V_CMP_32(Op.OpINotEqual             ,dtUint32,true);
  V_CMPX_GE_U32   :emit_V_CMP_32(Op.OpUGreaterThanEqual     ,dtUint32,true);

  else
   Assert(false,'VOP3c?'+IntToStr(FSPI.VOP3a.OP));
 end;

end;

procedure TEmit_VOP3.emit_VOP3b;
begin
 Assert(false,'VOP3b?'+IntToStr(FSPI.VOP3b.OP));
end;

procedure TEmit_VOP3.emit_VOP3a;
begin
 Case FSPI.VOP3a.OP of

  //VOP2 analog

  256+V_CNDMASK_B32: emit_V_CNDMASK_B32;

  256+V_ADD_F32: emit_V2_F32(Op.OpFAdd);
  256+V_SUB_F32: emit_V2_F32(Op.OpFSub);

  256+V_CVT_PKRTZ_F16_F32: emit_V_CVT_PKRTZ_F16_F32;

  256+V_MIN_LEGACY_F32:emit_V_MMX_F32(GlslOp.NMin);
  256+V_MAX_LEGACY_F32:emit_V_MMX_F32(GlslOp.NMax);

  256+V_MIN_F32:emit_V_MMX_F32(GlslOp.FMin);
  256+V_MAX_F32:emit_V_MMX_F32(GlslOp.FMax);

  256+V_MUL_F32: emit_V2_F32(Op.OpFMul);

  256+V_MUL_I32_I24: emit_V_MUL_I32_I24;
  256+V_MUL_U32_U24: emit_V_MUL_U32_U24;

  256+V_MAC_F32: emit_V_MAC_F32;

  //VOP3 only

  V_MUL_LO_I32: emit_V_MUL_LO_I32;
  V_MUL_HI_U32: emit_V_MUL_HI_U32;

  V_BFE_U32: emit_V_BFE_U32;
  V_BFI_B32: emit_V_BFI_B32;
  V_MAD_F32: emit_V_MAD_F32;

  V_MAD_I32_I24: emit_V_MAD_I32_I24;
  V_MAD_U32_U24: emit_V_MAD_U32_U24;

  V_SAD_U32 : emit_V_SAD_U32;
  V_MAX3_F32: emit_V_MAX3_F32;
  V_MIN3_F32: emit_V_MIN3_F32;
  V_MED3_F32: emit_V_MED3_F32;
  V_FMA_F32 : emit_V_FMA_F32;

  V_CUBEID_F32:emit_V_CUBE(OpCUBEID);
  V_CUBESC_F32:emit_V_CUBE(OpCUBESC);
  V_CUBETC_F32:emit_V_CUBE(OpCUBETC);
  V_CUBEMA_F32:emit_V_CUBE(OpCUBEMA);

  //VOP1 analog

  384+V_NOP:;

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

  else
   Assert(false,'VOP3a?'+IntToStr(FSPI.VOP3a.OP));
 end;

end;

end.

