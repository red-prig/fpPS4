unit emit_VOP1;

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
 TEmit_VOP1=class(TEmitFetch)
  procedure emit_VOP1;
  procedure emit_V_MOV_B32;
  procedure emit_V_CVT(OpId:DWORD;dst_type,src_type:TsrDataType);
  procedure emit_V_CVT_F16_F32;
  procedure emit_V_CVT_F32_F16;
  procedure emit_V_CVT_OFF_F32_I4;
  procedure emit_V_CVT_FLR_I32_F32;
  procedure emit_V_CVT_RPI_I32_F32;
  procedure emit_V_CVT_F32_UBYTE0;
  procedure emit_V_EXT_F32(OpId:DWORD);
  procedure emit_V_RSQ_CLAMP_F32;
  procedure emit_V_SIN_COS(OpId:DWORD);
  procedure emit_V_RCP_F32;
  procedure emit_V_FFBL_B32;
  procedure emit_V_BFREV_B32;
 end;

implementation

procedure TEmit_VOP1.emit_V_MOV_B32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtUnknow);
 MakeCopy(dst,src);
end;

procedure TEmit_VOP1.emit_V_CVT(OpId:DWORD;dst_type,src_type:TsrDataType);
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,src_type);
 Op1(OpId,dst_type,dst,src);
end;

procedure TEmit_VOP1.emit_V_CVT_F16_F32; //vdst[15:0].hf = ConvertFloatToHalfFloat(vsrc.f)
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 dstv:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src[0]:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);

 src[0]:=OpFToF(src[0],dtHalf16);
 src[1]:=NewReg_s(dtHalf16,0);

 dstv:=OpMakeVec(line,dtVec2h,@src);

 dst^.New(line,dtVec2h)^.pWriter:=dstv;
end;

procedure TEmit_VOP1.emit_V_CVT_F32_F16; //vdst.f = ConvertHalfFloatToFloat(vsrc[15:0].hf)
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
 dst0:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtVec2h{dtUnknow});

 //src:=OpBitwiseAndTo(src,$FFFF);
 //src^.PrepType(ord(dtHalf16));

 dst0:=NewReg(dtHalf16);
 OpExtract(line,dst0,src,0);

 Op1(Op.OpFConvert,dtFloat32,dst,{src}dst0);
end;

//V_CVT_OFF_F32_I4
//([0..3]-8)/16
procedure TEmit_VOP1.emit_V_CVT_OFF_F32_I4;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
 num_16:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtUInt32);

 src:=OpAndTo(src,15);
 src^.PrepType(ord(dtInt32));

 src:=OpISubTo(src,8);

 src:=OpSToF(src,dtFloat32);

 num_16:=NewReg_s(dtFloat32,16);
 Op2(Op.OpFDiv,dtFloat32,dst,src,num_16);
end;

procedure TEmit_VOP1.emit_V_CVT_FLR_I32_F32; //ConvertFloatToSignedInt(floor(vsrc.f))
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);

 src:=OpFloorTo(src);

 Op1(Op.OpConvertFToS,dtInt32,dst,src);
end;

procedure TEmit_VOP1.emit_V_CVT_RPI_I32_F32; //ConvertFloatToSignedInt(floor(vsrc.f+0.5))
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);

 src:=OpFAddToS(src,0.5);
 src:=OpFloorTo(src);

 Op1(Op.OpConvertFToS,dtInt32,dst,src);
end;

procedure TEmit_VOP1.emit_V_CVT_F32_UBYTE0;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtUInt32);

 src:=OpAndTo(src,$FF);
 src^.PrepType(ord(dtUInt32));

 Op1(Op.OpConvertUToF,dtFloat32,dst,src);
end;

procedure TEmit_VOP1.emit_V_EXT_F32(OpId:DWORD);
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);
 OpGlsl1(OpId,dtFloat32,dst,src);
end;

procedure TEmit_VOP1.emit_V_RSQ_CLAMP_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
 flt:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);
 OpGlsl1(GlslOp.InverseSqrt,dtFloat32,dst,src);

 src:=MakeRead(dst,dtFloat32);
 flt:=NewReg_s(dtFloat32,FLT_MAX);

 OpGlsl2(GlslOp.NMin,dtFloat32,dst,src,flt);
end;

procedure TEmit_VOP1.emit_V_SIN_COS(OpId:DWORD);
const
 PI2:Single=2*PI;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);

 src:=OpFMulToS(src,PI2);

 OpGlsl1(OpId,dtFloat32,dst,src);
end;

procedure TEmit_VOP1.emit_V_RCP_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
 one:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);

 one:=NewReg_s(dtFloat32,1);

 Op2(Op.OpFDiv,dtFloat32,dst,one,src);
end;

procedure TEmit_VOP1.emit_V_FFBL_B32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtInt32);

 OpGlsl1(GlslOp.FindILsb,dtInt32,dst,src);
end;

procedure TEmit_VOP1.emit_V_BFREV_B32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
 one:PsrRegNode;
begin
 dst:=get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtUInt32);

 Op1(Op.OpBitReverse,dtUInt32,dst,src);
end;

procedure TEmit_VOP1.emit_VOP1;
begin

 Case FSPI.VOP1.OP of

  V_NOP:;

  V_MOV_B32: emit_V_MOV_B32;

  V_CVT_F32_I32: emit_V_CVT(Op.OpConvertSToF,dtFloat32,dtInt32);
  V_CVT_F32_U32: emit_V_CVT(Op.OpConvertUToF,dtFloat32,dtUInt32);
  V_CVT_U32_F32: emit_V_CVT(Op.OpConvertFToU,dtUInt32 ,dtFloat32);
  V_CVT_I32_F32: emit_V_CVT(Op.OpConvertFToS,dtInt32  ,dtFloat32);

  V_CVT_F16_F32: emit_V_CVT_F16_F32;
  V_CVT_F32_F16: emit_V_CVT_F32_F16;

  V_CVT_OFF_F32_I4: emit_V_CVT_OFF_F32_I4;

  V_CVT_FLR_I32_F32: emit_V_CVT_FLR_I32_F32;

  V_CVT_RPI_I32_F32: emit_V_CVT_RPI_I32_F32;

  V_CVT_F32_UBYTE0: emit_V_CVT_F32_UBYTE0;


  V_FRACT_F32: emit_V_EXT_F32(GlslOp.Fract);
  V_TRUNC_F32: emit_V_EXT_F32(GlslOp.Trunc);
  V_CEIL_F32 : emit_V_EXT_F32(GlslOp.Ceil);

  V_RNDNE_F32: emit_V_EXT_F32(GlslOp.RoundEven);
  V_FLOOR_F32: emit_V_EXT_F32(GlslOp.Floor);
  V_EXP_F32  : emit_V_EXT_F32(GlslOp.Exp2);
  V_LOG_F32  : emit_V_EXT_F32(GlslOp.Log2);

  V_RSQ_F32  : emit_V_EXT_F32(GlslOp.InverseSqrt);
  V_RSQ_CLAMP_F32: emit_V_RSQ_CLAMP_F32;

  V_SQRT_F32 : emit_V_EXT_F32(GlslOp.Sqrt);

  V_SIN_F32  : emit_V_SIN_COS(GlslOp.Sin);
  V_COS_F32  : emit_V_SIN_COS(GlslOp.Cos);

  V_RCP_F32  : emit_V_RCP_F32;
  V_RCP_IFLAG_F32: emit_V_RCP_F32;

  V_FFBL_B32 : emit_V_FFBL_B32;

  V_BFREV_B32: emit_V_BFREV_B32;

  else
   Assert(false,'VOP1?'+IntToStr(FSPI.VOP1.OP));
 end;

end;

end.

