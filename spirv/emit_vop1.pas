unit emit_VOP1;

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
 TEmit_VOP1=object(TEmitOp)
  procedure _emit_VOP1;
  procedure _emit_V_MOV_B32;
  procedure _emit_V_CVT(OpId:DWORD;dst_type,src_type:TsrDataType);
  procedure _emit_V_CVT_OFF_F32_I4;
  procedure _emit_V_EXT_F32(OpId:DWORD);
  procedure _emit_V_RCP_F32;
 end;

implementation

procedure TEmit_VOP1._emit_V_MOV_B32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtUnknow);
 _MakeCopy(dst,src);
end;

procedure TEmit_VOP1._emit_V_CVT(OpId:DWORD;dst_type,src_type:TsrDataType);
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,src_type);
 emit_Op1(OpId,dst_type,dst,src);
end;

//V_CVT_OFF_F32_I4
//([0..3]-8)/16
procedure TEmit_VOP1._emit_V_CVT_OFF_F32_I4;
Var
 dst,tmp:PsrRegSlot;
 src:PsrRegNode;
 num_8:PsrRegNode;
 num_15:PsrRegNode;
 num_16:PsrRegNode;
 subi,subf:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtUInt32);

 tmp:=@FRegsStory.FUnattach;
 num_15:=FetchReg(FConsts.Fetch(dtUInt32,15));

 emit_OpBitwiseAnd(tmp,src,num_15);
 src:=MakeRead(tmp,dtUInt32);

 num_8:=FetchReg(FConsts.Fetch(dtInt32,8));
 subi:=dst^.New(line,dtInt32);

 _emit_OpISub(line,subi,src,num_8);

 subi^.mark_read;
 subf:=dst^.New(line,dtFloat32);

 _emit_Op1(line,Op.OpConvertSToF,subf,subi);

 subf^.mark_read;
 num_16:=FetchReg(FConsts.Fetchf(dtFloat32,16));

 emit_OpFDiv(dst,subf,num_16);
end;

procedure TEmit_VOP1._emit_V_EXT_F32(OpId:DWORD);
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);
 emit_OpExt1(OpId,dtFloat32,dst,src);
end;

procedure TEmit_VOP1._emit_V_RCP_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
 one:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);

 one:=FetchReg(FConsts.Fetchf(dtFloat32,1));

 emit_OpFDiv(dst,one,src);
end;

procedure TEmit_VOP1._emit_VOP1;
begin

 Case FSPI.VOP1.OP of

  V_NOP:;

  V_MOV_B32:
    begin
     _emit_V_MOV_B32;
    end;

  V_CVT_F32_I32: _emit_V_CVT(Op.OpConvertSToF,dtFloat32,dtInt32);
  V_CVT_F32_U32: _emit_V_CVT(Op.OpConvertUToF,dtFloat32,dtUInt32);
  V_CVT_U32_F32: _emit_V_CVT(Op.OpConvertFToU,dtUInt32 ,dtFloat32);
  V_CVT_I32_F32: _emit_V_CVT(Op.OpConvertFToS,dtInt32  ,dtFloat32);

  V_CVT_OFF_F32_I4:
    begin
     _emit_V_CVT_OFF_F32_I4;
    end;

  V_FRACT_F32: _emit_V_EXT_F32(GlslOp.Fract);
  V_TRUNC_F32: _emit_V_EXT_F32(GlslOp.Trunc);
  V_CEIL_F32 : _emit_V_EXT_F32(GlslOp.Ceil);

  V_FLOOR_F32: _emit_V_EXT_F32(GlslOp.Floor);
  V_EXP_F32  : _emit_V_EXT_F32(GlslOp.Exp2);
  V_LOG_F32  : _emit_V_EXT_F32(GlslOp.Log2);

  V_RSQ_F32  : _emit_V_EXT_F32(GlslOp.InverseSqrt);

  V_SQRT_F32 : _emit_V_EXT_F32(GlslOp.Sqrt);

  V_SIN_F32  : _emit_V_EXT_F32(GlslOp.Sin);
  V_COS_F32  : _emit_V_EXT_F32(GlslOp.Cos);

  V_RCP_F32:
    begin
     _emit_V_RCP_F32;
    end;

  else
   Assert(false,'VOP1?'+IntToStr(FSPI.VOP1.OP));
 end;

end;

end.

