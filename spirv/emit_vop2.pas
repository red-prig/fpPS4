unit emit_VOP2;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srTypes,
  srConst,
  srReg,
  SprvEmit,
  emit_op;

type
 TEmit_VOP2=object(TEmitOp)
  procedure _emit_VOP2;
  procedure _emit_V_CNDMASK_B32;
  procedure _emit_V_AND_B32;
  procedure _emit_V_OR_B32;
  procedure _emit_V_XOR_B32;
  procedure _emit_V_LSHL_B32;
  procedure _emit_V_LSHLREV_B32;
  procedure _emit_V_LSHR_B32;
  procedure _emit_V_LSHRREV_B32;
  procedure _emit_V_ASHR_I32;
  procedure _emit_V_ASHRREV_I32;
  procedure _emit_V_ADD_I32;
  procedure _emit_V_SUB_I32;
  procedure _emit_V_SUBREV_I32;
  procedure _emit_V_ADD_F32;
  procedure _emit_V_SUB_F32;
  procedure _emit_V_SUBREV_F32;
  procedure _emit_V_MUL_F32;
  procedure _emit_V_CVT_PKRTZ_F16_F32;
  procedure _emit_V_MAC_F32;
  procedure _emit_V_MADAK_F32;
  procedure _emit_V_MADMK_F32;
  procedure _emit_V_MIN_F32;
  procedure _emit_V_MAX_F32;
 end;

implementation

procedure TEmit_VOP2._emit_V_CNDMASK_B32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUnknow);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUnknow);
 src[2]:=MakeRead(@FRegsStory.VCC[0],dtBool);

 emit_OpSelect(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP2._emit_V_AND_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUnknow);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUnknow);

 emit_OpBitwiseAnd(dst,src[0],src[1]);
end;

procedure TEmit_VOP2._emit_V_OR_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUnknow);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUnknow);

 emit_OpBitwiseOr(dst,src[0],src[1]);
end;

procedure TEmit_VOP2._emit_V_XOR_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,{dtUnknow}dtUInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,{dtUnknow}dtUInt32);

 emit_OpBitwiseXor(dst,src[0],src[1]);
end;

procedure TEmit_VOP2._emit_V_LSHL_B32;
Var
 dst,tmp:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);
 tmp:=@FRegsStory.FUnattach;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 src[2]:=FetchReg(FConsts.Fetch(dtUInt32,31));
 emit_OpBitwiseAnd(tmp,src[1],src[2]);
 src[1]:=MakeRead(tmp,dtUInt32);

 emit_OpShl(dst,src[0],src[1]);
end;

procedure TEmit_VOP2._emit_V_LSHLREV_B32;
Var
 dst,tmp:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);
 tmp:=@FRegsStory.FUnattach;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 src[2]:=FetchReg(FConsts.Fetch(dtUInt32,31));
 emit_OpBitwiseAnd(tmp,src[0],src[2]);
 src[0]:=MakeRead(tmp,dtUInt32);

 emit_OpShl(dst,src[1],src[0]);
end;

procedure TEmit_VOP2._emit_V_LSHR_B32;
Var
 dst,tmp:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);
 tmp:=@FRegsStory.FUnattach;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 src[2]:=FetchReg(FConsts.Fetch(dtUInt32,31));
 emit_OpBitwiseAnd(tmp,src[1],src[2]);
 src[1]:=MakeRead(tmp,dtUInt32);

 emit_OpShr(dst,src[0],src[1]);
end;

procedure TEmit_VOP2._emit_V_LSHRREV_B32;
Var
 dst,tmp:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);
 tmp:=@FRegsStory.FUnattach;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 src[2]:=FetchReg(FConsts.Fetch(dtUInt32,31));
 emit_OpBitwiseAnd(tmp,src[0],src[2]);
 src[0]:=MakeRead(tmp,dtUInt32);

 emit_OpShr(dst,src[1],src[0]);
end;

procedure TEmit_VOP2._emit_V_ASHR_I32;
Var
 dst,tmp:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);
 tmp:=@FRegsStory.FUnattach;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtInt32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 src[2]:=FetchReg(FConsts.Fetch(dtInt32,31));
 emit_OpBitwiseAnd(tmp,src[1],src[2]);
 src[1]:=MakeRead(tmp,dtInt32);

 emit_OpShrA(dst,src[0],src[1]);
end;

procedure TEmit_VOP2._emit_V_ASHRREV_I32;
Var
 dst,tmp:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);
 tmp:=@FRegsStory.FUnattach;

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtInt32);

 src[2]:=FetchReg(FConsts.Fetch(dtInt32,31));
 emit_OpBitwiseAnd(tmp,src[0],src[2]);
 src[0]:=MakeRead(tmp,dtInt32);

 emit_OpShrA(dst,src[1],src[0]);
end;

procedure TEmit_VOP2._emit_V_ADD_I32; //vdst = vsrc0.s + vsrc1.s; sdst[thread_id:] = carry_out & EXEC
Var
 dst,car:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 exc,tmp:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);
 car:=@FRegsStory.VCC[0];

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 emit_OpIAddExt(dst,@FRegsStory.FUnattach,src[0],src[1]);

 tmp:=FRegsStory.FUnattach.current;
 tmp^.mark_read;
 exc:=MakeRead(@FRegsStory.EXEC[0],dtUnknow);
 emit_OpBitwiseAnd(car,tmp,exc);
end;

procedure TEmit_VOP2._emit_V_SUB_I32; //vdst = vsrc0.u - vsub.u; sdst[thread_id:] = borrow_out & EXEC
Var
 dst,car:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 exc,tmp:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);
 car:=@FRegsStory.VCC[0];

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 emit_OpISubExt(dst,@FRegsStory.FUnattach,src[0],src[1]);

 tmp:=FRegsStory.FUnattach.current;
 tmp^.mark_read;
 exc:=MakeRead(@FRegsStory.EXEC[0],dtUnknow);
 emit_OpBitwiseAnd(car,tmp,exc);
end;

procedure TEmit_VOP2._emit_V_SUBREV_I32; //vdst = vsrc1.u - vsub.u; sdst[thread_id:] = borrow_out & EXEC
Var
 dst,car:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 exc,tmp:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);
 car:=@FRegsStory.VCC[0];

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtUint32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtUint32);

 emit_OpISubExt(dst,@FRegsStory.FUnattach,src[1],src[0]);

 tmp:=FRegsStory.FUnattach.current;
 tmp^.mark_read;
 exc:=MakeRead(@FRegsStory.EXEC[0],dtUnknow);
 emit_OpBitwiseAnd(car,tmp,exc);
end;

procedure TEmit_VOP2._emit_V_ADD_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 emit_OpFAdd(dst,src[0],src[1]);
end;

procedure TEmit_VOP2._emit_V_SUB_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 emit_OpFSub(dst,src[0],src[1]);
end;

procedure TEmit_VOP2._emit_V_SUBREV_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 emit_OpFSub(dst,src[1],src[0]);
end;

procedure TEmit_VOP2._emit_V_MUL_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 emit_OpFMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP2._emit_V_CVT_PKRTZ_F16_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);
 _emit_ConvFloatToHalf(dst,src[0],src[1]);
end;

procedure TEmit_VOP2._emit_V_MAC_F32; //vdst = vsrc0.f * vsrc1.f + vdst.f  -> fma
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);
 src[2]:=MakeRead(dst,dtFloat32);

 emit_OpFmaF32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP2._emit_V_MADAK_F32; //vdst = vsrc0.f * vsrc1.f + kadd.f
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);
 src[2]:=FetchReg(FConsts.Fetch(dtFloat32,FSPI.INLINE32));

 emit_OpFmaF32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP2._emit_V_MADMK_F32; //vdst = vsrc0.f * kmul.f + vadd.f
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=FetchReg(FConsts.Fetch(dtFloat32,FSPI.INLINE32));
 src[2]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 emit_OpFmaF32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP2._emit_V_MIN_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 emit_OpFMin(dst,src[0],src[1]);
end;

procedure TEmit_VOP2._emit_V_MAX_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP2.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP2.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOP2.VSRC1,dtFloat32);

 emit_OpFMax(dst,src[0],src[1]);
end;

procedure TEmit_VOP2._emit_VOP2;
begin

 Case FSPI.VOP2.OP of

  V_CNDMASK_B32:
    begin
     _emit_V_CNDMASK_B32;
    end;

  V_AND_B32:
    begin
     _emit_V_AND_B32;
    end;

  V_OR_B32:
    begin
     _emit_V_OR_B32;
    end;

  V_XOR_B32:
    begin
     _emit_V_XOR_B32;
    end;

  V_LSHL_B32:
    begin
     _emit_V_LSHL_B32;
    end;

  V_LSHLREV_B32:
    begin
     _emit_V_LSHLREV_B32;
    end;

  V_LSHR_B32:
    begin
     _emit_V_LSHR_B32;
    end;

  V_LSHRREV_B32:
    begin
     _emit_V_LSHRREV_B32
    end;

  V_ASHR_I32:
    begin
     _emit_V_ASHR_I32;
    end;

  V_ASHRREV_I32:
    begin
     _emit_V_ASHRREV_I32
    end;

  V_ADD_I32: //vdst = vsrc0.s + vsrc1.s; sdst[thread_id:] = carry_out & EXEC
    begin
     // Vector ALU (except v_readlane, v_readfirstlane, v_writelane),
     // Vector Memory,
     // Local and Global Data Share
     // and Export.
     _emit_V_ADD_I32;
    end;

  V_SUB_I32:
    begin
     _emit_V_SUB_I32;
    end;

  V_SUBREV_I32:
    begin
     _emit_V_SUBREV_I32;
    end;

  V_ADD_F32:
    begin
     _emit_V_ADD_F32;
    end;

  V_SUB_F32:
    begin
     _emit_V_SUB_F32;
    end;

  V_SUBREV_F32:
    begin
     _emit_V_SUBREV_F32;
    end;

  V_MUL_F32:
    begin
     _emit_V_MUL_F32;
    end;

  V_CVT_PKRTZ_F16_F32:
    begin
     _emit_V_CVT_PKRTZ_F16_F32;
    end;

  V_MAC_F32: //vdst = vsrc0.f * vsrc1.f + vdst.f  -> fma
    begin
     _emit_V_MAC_F32;
    end;

  V_MADAK_F32:
    begin
     _emit_V_MADAK_F32;
    end;

  V_MADMK_F32:
    begin
     _emit_V_MADMK_F32;
    end;

  V_MIN_F32:
    begin
     _emit_V_MIN_F32;
    end;

  V_MAX_F32:
    begin
     _emit_V_MAX_F32;
    end;

  else
   Assert(false,'VOP2?'+IntToStr(FSPI.VOP2.OP));
 end;

end;

end.

