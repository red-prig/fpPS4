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
  procedure _emit_V_CVT_F32_I32;
  procedure _emit_V_CVT_U32_F32;
  procedure _emit_V_CVT_I32_F32;
  procedure _emit_V_CVT_F32_U32;
  procedure _emit_V_CVT_OFF_F32_I4;
  procedure _emit_V_SQRT_F32;
  procedure _emit_V_RSQ_F32;
  procedure _emit_V_FLOOR_F32;
  procedure _emit_V_FRACT_F32;
  procedure _emit_V_EXP_F32;
  procedure _emit_V_RCP_F32;
  procedure _emit_V_SIN_F32;
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

procedure TEmit_VOP1._emit_V_CVT_F32_I32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtInt32);
 emit_Op1(Op.OpConvertSToF,dtFloat32,dst,src);
end;

procedure TEmit_VOP1._emit_V_CVT_I32_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);
 emit_Op1(Op.OpConvertFToS,dtInt32,dst,src);
end;

procedure TEmit_VOP1._emit_V_CVT_U32_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);
 emit_Op1(Op.OpConvertFToU,dtUInt32,dst,src);
end;

procedure TEmit_VOP1._emit_V_CVT_F32_U32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtUInt32);
 emit_Op1(Op.OpConvertUToF,dtFloat32,dst,src);
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

procedure TEmit_VOP1._emit_V_SQRT_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);
 emit_OpSqrt(dst,src);
end;

procedure TEmit_VOP1._emit_V_RSQ_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);
 emit_OpInverseSqrt(dst,src);
end;

procedure TEmit_VOP1._emit_V_FLOOR_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);
 emit_OpFloor(dst,src);
end;

procedure TEmit_VOP1._emit_V_FRACT_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);
 emit_OpFract(dst,src);
end;

procedure TEmit_VOP1._emit_V_EXP_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);
 emit_OpExp2(dst,src);
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

procedure TEmit_VOP1._emit_V_SIN_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP1.VDST);
 src:=fetch_ssrc9(FSPI.VOP1.SRC0,dtFloat32);
 emit_OpSin(dst,src);
end;

procedure TEmit_VOP1._emit_VOP1;
begin

 Case FSPI.VOP1.OP of

  V_MOV_B32:
    begin
     _emit_V_MOV_B32;
    end;

  V_CVT_F32_I32:
    begin
     _emit_V_CVT_F32_I32;
    end;

  V_CVT_I32_F32:
    begin
     _emit_V_CVT_I32_F32;
    end;

  V_CVT_U32_F32:
    begin
     _emit_V_CVT_U32_F32;
    end;

  V_CVT_F32_U32:
    begin
     _emit_V_CVT_F32_U32;
    end;

  V_CVT_OFF_F32_I4:
    begin
     _emit_V_CVT_OFF_F32_I4;
    end;

  V_SQRT_F32:
    begin
     _emit_V_SQRT_F32;
    end;

  V_RSQ_F32:
    begin
     _emit_V_RSQ_F32;
    end;

  V_FLOOR_F32:
    begin
     _emit_V_FLOOR_F32;
    end;

  V_FRACT_F32:
    begin
     _emit_V_FRACT_F32;
    end;

  V_EXP_F32:
    begin
     _emit_V_EXP_F32;
    end;

  V_RCP_F32:
    begin
     _emit_V_RCP_F32;
    end;

  V_SIN_F32:
    begin
     _emit_V_SIN_F32;
    end;

  else
   Assert(false,'VOP1?'+IntToStr(FSPI.VOP1.OP));
 end;

end;

end.

