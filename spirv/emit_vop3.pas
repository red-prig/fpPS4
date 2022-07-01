unit emit_VOP3;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  spirv,
  srTypes,
  srReg,
  srOpUtils,
  SprvEmit,
  emit_op;

type
 TEmit_VOP3=object(TEmitOp)
  procedure _emit_VOP3c;
  procedure _emit_VOP3b;
  procedure _emit_VOP3a;
  procedure _emit_V_CMP_32(OpId:DWORD;rtype:TsrDataType;x:Boolean);
  procedure _emit_V_CMP_C(r,x:Boolean);

  procedure _emit_src_neg_bit(src:PPsrRegNode;count:Byte);
  procedure _emit_src_abs(src:PPsrRegNode);
  procedure _emit_src_abs_bit(src:PPsrRegNode;count:Byte);
  procedure _emit_dst_omod_f(dst:PsrRegSlot);
  procedure _emit_dst_clamp_f(dst:PsrRegSlot);

  procedure _emit_V_CNDMASK_B32;
  procedure _emit_V_ADD_F32;
  procedure _emit_V_SUB_F32;
  procedure _emit_V_CVT_PKRTZ_F16_F32;
  procedure _emit_V_MIN_F32;
  procedure _emit_V_MAX_F32;
  procedure _emit_V_MUL_LO_I32;
  procedure _emit_V_MUL_F32;
  procedure _emit_V_MUL_I32_I24;
  procedure _emit_V_MUL_U32_U24;
  procedure _emit_V_MAC_F32;

  procedure _emit_V_BFE_U32;
  procedure _emit_V_BFI_B32;
  procedure _emit_V_MAD_F32;
  procedure _emit_V_MAD_I32_I24;
  procedure _emit_V_MAD_U32_U24;
  procedure _emit_V_SAD_U32;
  procedure _emit_V_MAX3_F32;
  procedure _emit_V_MIN3_F32;
  procedure _emit_V_MED3_F32;
  procedure _emit_V_FMA_F32;
  procedure _emit_V_CUBE(OpId:DWORD);
  procedure _emit_V_MOV_B32;
  procedure _emit_V_EXT_F32(OpId:DWORD);
  procedure _emit_V_RCP_F32;
 end;

implementation

procedure TEmit_VOP3._emit_V_CMP_32(OpId:DWORD;rtype:TsrDataType;x:Boolean);
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst[0]:=FRegsStory.get_sdst7(FSPI.VOP3a.VDST+0);
 dst[1]:=FRegsStory.get_sdst7(FSPI.VOP3a.VDST+1);

 Assert(dst[0]<>nil);
 Assert(dst[1]<>nil);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,rtype);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,rtype);

 _emit_src_abs_bit(@src,2);
 _emit_src_neg_bit(@src,2);

 emit_OpCmpV(OpId,dst[0],src[0],src[1]);

 SetConst(dst[1],dtUnknow,0); //set zero

 if x then
 begin
  MakeCopy(@FRegsStory.EXEC[0],dst[0]^.current);
  SetConst(@FRegsStory.EXEC[1],dtUnknow,0);     //set zero
 end;
end;

procedure TEmit_VOP3._emit_V_CMP_C(r,x:Boolean);
Var
 dst:array[0..1] of PsrRegSlot;
begin
 dst[0]:=FRegsStory.get_sdst7(FSPI.VOP3a.VDST+0);
 dst[1]:=FRegsStory.get_sdst7(FSPI.VOP3a.VDST+1);

 Assert(dst[0]<>nil);
 Assert(dst[1]<>nil);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 SetConst(dst[0],dtBool,QWORD(r));
 SetConst(dst[1],dtUnknow,0); //set zero

 if x then
 begin
  MakeCopy(@FRegsStory.EXEC[0],dst[0]^.current);
  SetConst(@FRegsStory.EXEC[1],dtUnknow,0);     //set zero
 end;
end;

procedure TEmit_VOP3._emit_src_neg_bit(src:PPsrRegNode;count:Byte);
var
 i:Byte;
 dst:PsrRegNode;
begin
 if (FSPI.VOP3a.NEG=0) then Exit;

 For i:=0 to count-1 do
  if Byte(FSPI.VOP3a.NEG).TestBit(i) then
  begin
   dst:=NewReg(dtFloat32);
   dst^.mark_read;
   _emit_OpFNegate(line,dst,src[i]);
   src[i]:=dst;
  end;
end;

procedure TEmit_VOP3._emit_src_abs(src:PPsrRegNode);
var
 dst:PsrRegNode;
begin
 dst:=NewReg(dtFloat32);
 dst^.mark_read;
 _emit_OpFAbs(line,dst,src^);
 src^:=dst;
end;

procedure TEmit_VOP3._emit_src_abs_bit(src:PPsrRegNode;count:Byte);
var
 i:Byte;
begin
 if (FSPI.VOP3a.ABS=0) then Exit;

 For i:=0 to count-1 do
  if Byte(FSPI.VOP3a.ABS).TestBit(i) then
  begin
   _emit_src_abs(@src[i]);
  end;
end;

procedure TEmit_VOP3._emit_dst_omod_f(dst:PsrRegSlot);
var
 src,tmp:PsrRegNode;
begin

 Case FSPI.VOP3a.OMOD of
  0:;
  1: // *2
    begin
     src:=dst^.current;
     src^.mark_read;
     tmp:=FetchReg(FConsts.Fetchf(dtFloat32,2));
     emit_OpFMul(dst,src,tmp);
    end;
  2: // *4
    begin
     src:=dst^.current;
     src^.mark_read;
     tmp:=FetchReg(FConsts.Fetchf(dtFloat32,4));
     emit_OpFMul(dst,src,tmp);
    end;
  3: // /2
    begin
     src:=dst^.current;
     src^.mark_read;
     tmp:=FetchReg(FConsts.Fetchf(dtFloat32,2));
     emit_OpFDiv(dst,src,tmp);
    end;
 end;

end;

procedure TEmit_VOP3._emit_dst_clamp_f(dst:PsrRegSlot);
var
 src,min,max:PsrRegNode;
begin
 if (FSPI.VOP3a.CLAMP=0) then Exit;

 src:=dst^.current;
 src^.mark_read;
 min:=FetchReg(FConsts.Fetchf(dtFloat32,0));
 max:=FetchReg(FConsts.Fetchf(dtFloat32,1));

 emit_OpFClamp(dst,src,min,max);
end;

procedure TEmit_VOP3._emit_V_CNDMASK_B32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUnknow);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUnknow);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtBool);

 _emit_src_abs_bit(@src,2);
 _emit_src_neg_bit(@src,2);

 emit_OpSelect(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3._emit_V_ADD_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 _emit_src_abs_bit(@src,2);
 _emit_src_neg_bit(@src,2);

 emit_OpFAdd(dst,src[0],src[1]);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3._emit_V_SUB_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 _emit_src_abs_bit(@src,2);
 _emit_src_neg_bit(@src,2);

 emit_OpFSub(dst,src[0],src[1]);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3._emit_V_CVT_PKRTZ_F16_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 _emit_src_abs_bit(@src,2);
 _emit_src_neg_bit(@src,2);

 _emit_ConvFloatToHalf(dst,src[0],src[1]);
end;

procedure TEmit_VOP3._emit_V_MIN_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 _emit_src_abs_bit(@src,2);
 _emit_src_neg_bit(@src,2);

 emit_OpFMin(dst,src[0],src[1]);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3._emit_V_MAX_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 _emit_src_abs_bit(@src,2);
 _emit_src_neg_bit(@src,2);

 emit_OpFMax(dst,src[0],src[1]);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3._emit_V_MUL_LO_I32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtInt32);

 emit_OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP3._emit_V_MUL_F32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);

 _emit_src_abs_bit(@src,2);
 _emit_src_neg_bit(@src,2);

 emit_OpFMul(dst,src[0],src[1]);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3._emit_V_MUL_I32_I24;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtInt32);

 //24bit mask TODO
 emit_OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP3._emit_V_MUL_U32_U24;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUInt32);

 //24bit mask TODO
 emit_OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_VOP3._emit_V_MAC_F32; //vdst = vsrc0.f * vsrc1.f + vdst.f  -> fma
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 _emit_src_abs_bit(@src,3);
 _emit_src_neg_bit(@src,3);

 emit_OpFmaF32(dst,src[0],src[1],src[2]);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3._emit_V_BFE_U32;
Var
 dst,tmp:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
 num_31:PsrRegNode;
 offset,count:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);
 tmp:=@FRegsStory.FUnattach;

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUint32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUint32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUint32);

 num_31:=FetchReg(FConsts.Fetch(dtUInt32,31));

 emit_OpBitwiseAnd(tmp,src[1],num_31);
 offset:=MakeRead(tmp,dtUInt32);

 num_31^.mark_read;
 emit_OpBitwiseAnd(tmp,src[2],num_31);
 count:=MakeRead(tmp,dtUInt32);

 emit_OpBfeU(dst,src[0],offset,count);
end;

procedure TEmit_VOP3._emit_V_BFI_B32;
Var
 dst,tmp:PsrRegSlot;
 bitmsk:PsrRegNode;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);
 tmp:=@FRegsStory.FUnattach;

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 bitmsk:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUint32);
 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUint32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUint32);

 emit_OpBitwiseAnd(tmp,src[0],bitmsk);
 src[0]:=MakeRead(tmp,dtUInt32);

 emit_OpNot(tmp,bitmsk);
 bitmsk:=MakeRead(tmp,dtUInt32);

 emit_OpBitwiseAnd(tmp,src[1],bitmsk);
 src[1]:=MakeRead(tmp,dtUInt32);

 emit_OpBitwiseOr(dst,src[0],src[1]);
end;

procedure TEmit_VOP3._emit_V_MAD_F32; //vdst = vsrc0.f * vsrc1.f + vadd.f
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 _emit_src_abs_bit(@src,3);
 _emit_src_neg_bit(@src,3);

 emit_OpFmaF32(dst,src[0],src[1],src[2]);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3._emit_V_MAD_I32_I24;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtInt32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtInt32);

 emit_OpFmaI32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3._emit_V_MAD_U32_U24;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUInt32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUInt32);

 emit_OpFmaU32(dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3._emit_V_SAD_U32; //dst.u = abs(vsrc0.u - vsrc1.u) + vaccum.u
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
 rdif,rvac:PsrRegNode;
 //msk:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtUInt32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtUInt32);

 rdif:=NewReg(dtInt32);
 _emit_OpAbsDiff(line,rdif,src[0],src[1]);

 //msk:=FetchReg(FConsts.Fetch(dtUInt32,$FFFF)); mask need?
 //emit_OpBitwiseAnd(tmp,src[2],msk);
 //rvac:=tmp^.current;
 rvac:=src[2];

 rdif^.mark_read;
 rvac^.mark_read;
 emit_OpIAdd(dst,rdif,rvac);
end;

procedure TEmit_VOP3._emit_V_MAX3_F32;
Var
 dst,tmp:PsrRegSlot;
 src:array[0..3] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);
 tmp:=@FRegsStory.FUnattach;

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 _emit_src_abs_bit(@src,3);
 _emit_src_neg_bit(@src,3);

 emit_OpFMax(tmp,src[0],src[1]);
 src[3]:=MakeRead(tmp,dtFloat32);

 emit_OpFMax(dst,src[3],src[2]);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3._emit_V_MIN3_F32;
Var
 dst,tmp:PsrRegSlot;
 src:array[0..3] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);
 tmp:=@FRegsStory.FUnattach;

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 _emit_src_abs_bit(@src,3);
 _emit_src_neg_bit(@src,3);

 emit_OpFMin(tmp,src[0],src[1]);
 src[3]:=MakeRead(tmp,dtFloat32);

 emit_OpFMin(dst,src[3],src[2]);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3._emit_V_MED3_F32;
Var
 dst:PsrRegSlot;
 src:array[0..3] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 _emit_src_abs_bit(@src,3);
 _emit_src_neg_bit(@src,3);

 emit_MED3F(dst,src[0],src[1],src[2]);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3._emit_V_FMA_F32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 _emit_src_abs_bit(@src,3);
 _emit_src_neg_bit(@src,3);

 emit_OpFmaF32(dst,src[0],src[1],src[2]);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

{
procedure TEmit_VOP3._emit_V_CUBEMA_F32;
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
 max:array[0..1] of PsrRegNode;
 num_2:PsrRegNode;
begin
 //max(max(abs(x),abs(y)),abs(z))*2

 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 _emit_src_abs(@src[0]);
 _emit_src_abs(@src[1]);
 _emit_src_abs(@src[2]);

 max[0]:=NewReg(dtFloat32);
 max[0]^.mark_read;
 _emit_OpFMax(line,max[0],src[0],src[1]);

 max[1]:=NewReg(dtFloat32);
 max[1]^.mark_read;
 _emit_OpFMax(line,max[1],max[0],src[2]);

 num_2:=FetchReg(FConsts.Fetchf(dtFloat32,2));

 emit_OpFMul(dst,max[1],num_2);
end;
}

procedure TEmit_VOP3._emit_V_CUBE(OpId:DWORD);
Var
 dst:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.ABS  =0,'FSPI.VOP3a.ABS');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');
 Assert(FSPI.VOP3a.NEG  =0,'FSPI.VOP3a.NEG');

 src[0]:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);
 src[1]:=fetch_ssrc9(FSPI.VOP3a.SRC1,dtFloat32);
 src[2]:=fetch_ssrc9(FSPI.VOP3a.SRC2,dtFloat32);

 emit_Op3(OpId,dtFloat32,dst,src[0],src[1],src[2]);
end;

procedure TEmit_VOP3._emit_V_MOV_B32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 Assert(FSPI.VOP3a.OMOD =0,'FSPI.VOP3a.OMOD');
 Assert(FSPI.VOP3a.CLAMP=0,'FSPI.VOP3a.CLAMP');

 if (Byte(FSPI.VOP3a.ABS).TestBit(0)) or
    (Byte(FSPI.VOP3a.NEG).TestBit(0)) then
 begin
  src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);

  _emit_src_abs_bit(@src,1);
  _emit_src_neg_bit(@src,1);

  _MakeCopy(dst,src);
 end else
 begin
  src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtUnknow);

  _MakeCopy(dst,src);
 end;

end;

procedure TEmit_VOP3._emit_V_EXT_F32(OpId:DWORD);
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);

 _emit_src_abs_bit(@src,1);
 _emit_src_neg_bit(@src,1);

 emit_OpExt1(OpId,dtFloat32,dst,src);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3._emit_V_RCP_F32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
 one:PsrRegNode;
begin
 dst:=FRegsStory.get_vdst8(FSPI.VOP3a.VDST);

 src:=fetch_ssrc9(FSPI.VOP3a.SRC0,dtFloat32);

 _emit_src_abs_bit(@src,1);
 _emit_src_neg_bit(@src,1);

 one:=FetchReg(FConsts.Fetchf(dtFloat32,1));

 emit_OpFDiv(dst,one,src);

 _emit_dst_omod_f(dst);
 _emit_dst_clamp_f(dst);
end;

procedure TEmit_VOP3._emit_VOP3c;
begin
 Case FSPI.VOP3a.OP of

  V_CMP_F_F32,
  V_CMP_F_F64,
  V_CMP_F_I32,
  V_CMP_F_I64,
  V_CMP_F_U32,
  V_CMP_F_U64,
  V_CMPS_F_F32,
  V_CMPS_F_F64:_emit_V_CMP_C(false,false);

  V_CMP_T_F32,
  V_CMP_T_F64,
  V_CMP_T_I32,
  V_CMP_T_I64,
  V_CMP_T_U32,
  V_CMP_T_U64,
  V_CMPS_T_F32,
  V_CMPS_T_F64:_emit_V_CMP_C(true,false);

  V_CMPX_F_F32,
  V_CMPX_F_F64,
  V_CMPX_F_I32,
  V_CMPX_F_I64,
  V_CMPX_F_U32,
  V_CMPX_F_U64,
  V_CMPSX_F_F32,
  V_CMPSX_F_F64:_emit_V_CMP_C(false,true);

  V_CMPX_T_F32,
  V_CMPX_T_F64,
  V_CMPX_T_I32,
  V_CMPX_T_I64,
  V_CMPX_T_U32,
  V_CMPX_T_U64,
  V_CMPSX_T_F32,
  V_CMPSX_T_F64:_emit_V_CMP_C(true,true);

  //

  V_CMP_LT_F32    :_emit_V_CMP_32(Op.OpFOrdLessThan          ,dtFloat32,false);
  V_CMP_EQ_F32    :_emit_V_CMP_32(Op.OpFOrdEqual             ,dtFloat32,false);
  V_CMP_LE_F32    :_emit_V_CMP_32(Op.OpFOrdLessThanEqual     ,dtFloat32,false);
  V_CMP_GT_F32    :_emit_V_CMP_32(Op.OpFOrdGreaterThan       ,dtFloat32,false);
  V_CMP_LG_F32    :_emit_V_CMP_32(Op.OpFOrdNotEqual          ,dtFloat32,false);
  V_CMP_GE_F32    :_emit_V_CMP_32(Op.OpFOrdGreaterThanEqual  ,dtFloat32,false);
  V_CMP_O_F32     :_emit_V_CMP_32(Op.OpOrdered               ,dtFloat32,false);
  V_CMP_U_F32     :_emit_V_CMP_32(Op.OpUnordered             ,dtFloat32,false);
  V_CMP_NGE_F32   :_emit_V_CMP_32(Op.OpFUnordLessThan        ,dtFloat32,false);
  V_CMP_NLG_F32   :_emit_V_CMP_32(Op.OpFUnordEqual           ,dtFloat32,false);
  V_CMP_NGT_F32   :_emit_V_CMP_32(Op.OpFUnordLessThanEqual   ,dtFloat32,false);
  V_CMP_NLE_F32   :_emit_V_CMP_32(Op.OpFUnordGreaterThan     ,dtFloat32,false);
  V_CMP_NEQ_F32   :_emit_V_CMP_32(Op.OpFUnordNotEqual        ,dtFloat32,false);
  V_CMP_NLT_F32   :_emit_V_CMP_32(Op.OpFUnordGreaterThanEqual,dtFloat32,false);

  V_CMPX_LT_F32   :_emit_V_CMP_32(Op.OpFOrdLessThan          ,dtFloat32,true);
  V_CMPX_EQ_F32   :_emit_V_CMP_32(Op.OpFOrdEqual             ,dtFloat32,true);
  V_CMPX_LE_F32   :_emit_V_CMP_32(Op.OpFOrdLessThanEqual     ,dtFloat32,true);
  V_CMPX_GT_F32   :_emit_V_CMP_32(Op.OpFOrdGreaterThan       ,dtFloat32,true);
  V_CMPX_LG_F32   :_emit_V_CMP_32(Op.OpFOrdNotEqual          ,dtFloat32,true);
  V_CMPX_GE_F32   :_emit_V_CMP_32(Op.OpFOrdGreaterThanEqual  ,dtFloat32,true);
  V_CMPX_O_F32    :_emit_V_CMP_32(Op.OpOrdered               ,dtFloat32,true);
  V_CMPX_U_F32    :_emit_V_CMP_32(Op.OpUnordered             ,dtFloat32,true);
  V_CMPX_NGE_F32  :_emit_V_CMP_32(Op.OpFUnordLessThan        ,dtFloat32,true);
  V_CMPX_NLG_F32  :_emit_V_CMP_32(Op.OpFUnordEqual           ,dtFloat32,true);
  V_CMPX_NGT_F32  :_emit_V_CMP_32(Op.OpFUnordLessThanEqual   ,dtFloat32,true);
  V_CMPX_NLE_F32  :_emit_V_CMP_32(Op.OpFUnordGreaterThan     ,dtFloat32,true);
  V_CMPX_NEQ_F32  :_emit_V_CMP_32(Op.OpFUnordNotEqual        ,dtFloat32,true);
  V_CMPX_NLT_F32  :_emit_V_CMP_32(Op.OpFUnordGreaterThanEqual,dtFloat32,true);

  //

  V_CMPS_LT_F32   :_emit_V_CMP_32(Op.OpFOrdLessThan          ,dtFloat32,false);
  V_CMPS_EQ_F32   :_emit_V_CMP_32(Op.OpFOrdEqual             ,dtFloat32,false);
  V_CMPS_LE_F32   :_emit_V_CMP_32(Op.OpFOrdLessThanEqual     ,dtFloat32,false);
  V_CMPS_GT_F32   :_emit_V_CMP_32(Op.OpFOrdGreaterThan       ,dtFloat32,false);
  V_CMPS_LG_F32   :_emit_V_CMP_32(Op.OpFOrdNotEqual          ,dtFloat32,false);
  V_CMPS_GE_F32   :_emit_V_CMP_32(Op.OpFOrdGreaterThanEqual  ,dtFloat32,false);
  V_CMPS_O_F32    :_emit_V_CMP_32(Op.OpOrdered               ,dtFloat32,false);
  V_CMPS_U_F32    :_emit_V_CMP_32(Op.OpUnordered             ,dtFloat32,false);
  V_CMPS_NGE_F32  :_emit_V_CMP_32(Op.OpFUnordLessThan        ,dtFloat32,false);
  V_CMPS_NLG_F32  :_emit_V_CMP_32(Op.OpFUnordEqual           ,dtFloat32,false);
  V_CMPS_NGT_F32  :_emit_V_CMP_32(Op.OpFUnordLessThanEqual   ,dtFloat32,false);
  V_CMPS_NLE_F32  :_emit_V_CMP_32(Op.OpFUnordGreaterThan     ,dtFloat32,false);
  V_CMPS_NEQ_F32  :_emit_V_CMP_32(Op.OpFUnordNotEqual        ,dtFloat32,false);
  V_CMPS_NLT_F32  :_emit_V_CMP_32(Op.OpFUnordGreaterThanEqual,dtFloat32,false);

  V_CMPSX_LT_F32  :_emit_V_CMP_32(Op.OpFOrdLessThan          ,dtFloat32,true);
  V_CMPSX_EQ_F32  :_emit_V_CMP_32(Op.OpFOrdEqual             ,dtFloat32,true);
  V_CMPSX_LE_F32  :_emit_V_CMP_32(Op.OpFOrdLessThanEqual     ,dtFloat32,true);
  V_CMPSX_GT_F32  :_emit_V_CMP_32(Op.OpFOrdGreaterThan       ,dtFloat32,true);
  V_CMPSX_LG_F32  :_emit_V_CMP_32(Op.OpFOrdNotEqual          ,dtFloat32,true);
  V_CMPSX_GE_F32  :_emit_V_CMP_32(Op.OpFOrdGreaterThanEqual  ,dtFloat32,true);
  V_CMPSX_O_F32   :_emit_V_CMP_32(Op.OpOrdered               ,dtFloat32,true);
  V_CMPSX_U_F32   :_emit_V_CMP_32(Op.OpUnordered             ,dtFloat32,true);
  V_CMPSX_NGE_F32 :_emit_V_CMP_32(Op.OpFUnordLessThan        ,dtFloat32,true);
  V_CMPSX_NLG_F32 :_emit_V_CMP_32(Op.OpFUnordEqual           ,dtFloat32,true);
  V_CMPSX_NGT_F32 :_emit_V_CMP_32(Op.OpFUnordLessThanEqual   ,dtFloat32,true);
  V_CMPSX_NLE_F32 :_emit_V_CMP_32(Op.OpFUnordGreaterThan     ,dtFloat32,true);
  V_CMPSX_NEQ_F32 :_emit_V_CMP_32(Op.OpFUnordNotEqual        ,dtFloat32,true);
  V_CMPSX_NLT_F32 :_emit_V_CMP_32(Op.OpFUnordGreaterThanEqual,dtFloat32,true);

  //

  V_CMP_LT_I32    :_emit_V_CMP_32(Op.OpSLessThan             ,dtInt32,false);
  V_CMP_EQ_I32    :_emit_V_CMP_32(Op.OpIEqual                ,dtInt32,false);
  V_CMP_LE_I32    :_emit_V_CMP_32(Op.OpSLessThanEqual        ,dtInt32,false);
  V_CMP_GT_I32    :_emit_V_CMP_32(Op.OpSGreaterThan          ,dtInt32,false);
  V_CMP_LG_I32    :_emit_V_CMP_32(Op.OpINotEqual             ,dtInt32,false);
  V_CMP_GE_I32    :_emit_V_CMP_32(Op.OpSGreaterThanEqual     ,dtInt32,false);

  V_CMPX_LT_I32   :_emit_V_CMP_32(Op.OpSLessThan             ,dtInt32,true);
  V_CMPX_EQ_I32   :_emit_V_CMP_32(Op.OpIEqual                ,dtInt32,true);
  V_CMPX_LE_I32   :_emit_V_CMP_32(Op.OpSLessThanEqual        ,dtInt32,true);
  V_CMPX_GT_I32   :_emit_V_CMP_32(Op.OpSGreaterThan          ,dtInt32,true);
  V_CMPX_LG_I32   :_emit_V_CMP_32(Op.OpINotEqual             ,dtInt32,true);
  V_CMPX_GE_I32   :_emit_V_CMP_32(Op.OpSGreaterThanEqual     ,dtInt32,true);

  V_CMP_LT_U32    :_emit_V_CMP_32(Op.OpULessThan             ,dtUint32,false);
  V_CMP_EQ_U32    :_emit_V_CMP_32(Op.OpIEqual                ,dtUint32,false);
  V_CMP_LE_U32    :_emit_V_CMP_32(Op.OpULessThanEqual        ,dtUint32,false);
  V_CMP_GT_U32    :_emit_V_CMP_32(Op.OpUGreaterThan          ,dtUint32,false);
  V_CMP_LG_U32    :_emit_V_CMP_32(Op.OpINotEqual             ,dtUint32,false);
  V_CMP_GE_U32    :_emit_V_CMP_32(Op.OpUGreaterThanEqual     ,dtUint32,false);

  V_CMPX_LT_U32   :_emit_V_CMP_32(Op.OpULessThan             ,dtUint32,true);
  V_CMPX_EQ_U32   :_emit_V_CMP_32(Op.OpIEqual                ,dtUint32,true);
  V_CMPX_LE_U32   :_emit_V_CMP_32(Op.OpULessThanEqual        ,dtUint32,true);
  V_CMPX_GT_U32   :_emit_V_CMP_32(Op.OpUGreaterThan          ,dtUint32,true);
  V_CMPX_LG_U32   :_emit_V_CMP_32(Op.OpINotEqual             ,dtUint32,true);
  V_CMPX_GE_U32   :_emit_V_CMP_32(Op.OpUGreaterThanEqual     ,dtUint32,true);

  else
   Assert(false,'VOP3c?'+IntToStr(FSPI.VOP3a.OP));
 end;

end;

procedure TEmit_VOP3._emit_VOP3b;
begin
 Assert(false,'VOP3b?'+IntToStr(FSPI.VOP3b.OP));
end;

procedure TEmit_VOP3._emit_VOP3a;
begin

 //VOP2 analog

 Case FSPI.VOP3a.OP of

  256+V_CNDMASK_B32:
   begin
    _emit_V_CNDMASK_B32;
   end;

  256+V_ADD_F32:
   begin
    _emit_V_ADD_F32;
   end;

  256+V_SUB_F32:
   begin
    _emit_V_SUB_F32;
   end;

  256+V_CVT_PKRTZ_F16_F32:
   begin
    _emit_V_CVT_PKRTZ_F16_F32;
   end;

  256+V_MIN_F32:
   begin
    _emit_V_MIN_F32;
   end;

  256+V_MAX_F32:
   begin
    _emit_V_MAX_F32;
   end;

  256+V_MUL_F32:
    begin
     _emit_V_MUL_F32;
    end;

  256+V_MUL_I32_I24:
    begin
     _emit_V_MUL_I32_I24;
    end;

  256+V_MUL_U32_U24:
    begin
     _emit_V_MUL_U32_U24;
    end;

  256+V_MAC_F32:
    begin
     _emit_V_MAC_F32;
    end;

  //VOP3 only

  V_MUL_LO_I32:
    begin
     _emit_V_MUL_LO_I32;
    end;

  V_BFE_U32:
   begin
    _emit_V_BFE_U32;
   end;

  V_BFI_B32:
   begin
    _emit_V_BFI_B32;
   end;

  V_MAD_F32:
   begin
    _emit_V_MAD_F32;
   end;

  V_MAD_I32_I24:
   begin
    _emit_V_MAD_I32_I24;
   end;

  V_MAD_U32_U24:
   begin
    _emit_V_MAD_U32_U24;
   end;

  V_SAD_U32:
   begin
    _emit_V_SAD_U32;
   end;

  V_MAX3_F32:
   begin
    _emit_V_MAX3_F32;
   end;

  V_MIN3_F32:
   begin
    _emit_V_MIN3_F32;
   end;

  V_MED3_F32:
   begin
    _emit_V_MED3_F32;
   end;

  V_FMA_F32:
   begin
    _emit_V_FMA_F32;
   end;

  V_CUBEID_F32:_emit_V_CUBE(OpCUBEID);
  V_CUBESC_F32:_emit_V_CUBE(OpCUBESC);
  V_CUBETC_F32:_emit_V_CUBE(OpCUBETC);
  V_CUBEMA_F32:_emit_V_CUBE(OpCUBEMA);

  //VOP1 analog

  384+V_NOP:;

  384+V_MOV_B32:
   begin
    _emit_V_MOV_B32;
   end;

  384+V_FRACT_F32: _emit_V_EXT_F32(GlslOp.Fract);
  384+V_TRUNC_F32: _emit_V_EXT_F32(GlslOp.Trunc);
  384+V_CEIL_F32 : _emit_V_EXT_F32(GlslOp.Ceil);

  384+V_FLOOR_F32: _emit_V_EXT_F32(GlslOp.Floor);
  384+V_EXP_F32  : _emit_V_EXT_F32(GlslOp.Exp2);
  384+V_LOG_F32  : _emit_V_EXT_F32(GlslOp.Log2);

  384+V_RSQ_F32  : _emit_V_EXT_F32(GlslOp.InverseSqrt);

  384+V_SQRT_F32 : _emit_V_EXT_F32(GlslOp.Sqrt);

  384+V_SIN_F32  : _emit_V_EXT_F32(GlslOp.Sin);
  384+V_COS_F32  : _emit_V_EXT_F32(GlslOp.Cos);

  384+V_RCP_F32:
   begin
    _emit_V_RCP_F32;
   end;

  else
   Assert(false,'VOP3a?'+IntToStr(FSPI.VOP3a.OP));
 end;

end;

end.

