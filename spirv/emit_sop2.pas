unit emit_SOP2;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  spirv,
  ps4_pssl,
  srType,
  srReg,
  emit_fetch;


type
 TEmit_SOP2=class(TEmitFetch)
  procedure emit_SOP2;
  procedure emit_S_ADD_U32;
  procedure emit_S_ADD_I32;
  procedure emit_S_ADDC_U32;
  procedure emit_S_MUL_I32;
  procedure OpISccNotZero(src:PsrRegNode);
  procedure emit_S_LSHL_B32;
  procedure emit_S_LSHR_B32;
  procedure emit_S_AND_B32;
  procedure emit_S_AND_B64;
  procedure emit_S_ANDN2_B64;
  procedure emit_S_OR_B64;
  procedure emit_S_ORN2_B64;
  procedure emit_S_NOR_B64;
  procedure emit_S_CSELECT_B32;
  procedure emit_S_CSELECT_B64;
  procedure emit_S_BFE_U32;
 end;

implementation

procedure TEmit_SOP2.emit_S_ADD_U32;
Var
 dst,car:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);
 car:=get_scc;

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 OpIAddExt(dst,car,src[0],src[1]);
end;

procedure TEmit_SOP2.emit_S_ADD_I32;
Var
 dst,car:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);
 car:=get_scc;

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtInt32);

 OpIAddExt(dst,car,src[0],src[1]);
end;

procedure TEmit_SOP2.emit_S_ADDC_U32;
Var
 dst,car:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);
 car:=get_scc;

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);
 src[2]:=MakeRead(car,dtUInt32);

 OpIAddExt(dst,car,src[0],src[1]); //src0+src1

 src[0]:=MakeRead(dst,dtUInt32);
 src[1]:=MakeRead(car,dtUInt32);   //save car1

 OpIAddExt(dst,car,src[0],src[2]); //(src0+src1)+SCC

 src[0]:=MakeRead(car,dtUInt32);

 OpBitwiseOr(car,src[1],src[0]);   //SCC1 or SCC2
end;

procedure TEmit_SOP2.emit_S_MUL_I32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtInt32);

 OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_SOP2.OpISccNotZero(src:PsrRegNode); //SCC = (sdst.u != 0)
begin
 MakeCopy(get_scc,src);
 get_scc^.current^.dtype:=dtBool; //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_LSHL_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 src[1]:=OpBitwiseAndTo(src[1],31);
 src[1]^.PrepType(ord(dtUInt32));

 Op2(Op.OpShiftLeftLogical,src[0]^.dtype,dst,src[0],src[1]);

 OpISccNotZero(dst^.current); //SCC = (sdst.u != 0)
end;

procedure TEmit_SOP2.emit_S_LSHR_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 src[1]:=OpBitwiseAndTo(src[1],31);
 src[1]^.PrepType(ord(dtUInt32));

 Op2(Op.OpShiftRightLogical,src[0]^.dtype,dst,src[0],src[1]);

 OpISccNotZero(dst^.current); //SCC = (sdst.u != 0)
end;

procedure TEmit_SOP2.emit_S_AND_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 OpBitwiseAnd(dst,src[0],src[1]);

 OpISccNotZero(dst^.current); //SCC = (sdst.u != 0)
end;

procedure TEmit_SOP2.emit_S_AND_B64; //SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of PsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC0,@src0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC1,@src1,dtUInt32) then Assert(False);

 OpBitwiseAnd(dst[0],src0[0],src1[0]);
 OpBitwiseAnd(dst[1],src0[1],src1[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 OpLogicalOr(get_scc,src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_ANDN2_B64; //SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of PsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC0,@src0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC1,@src1,dtUInt32) then Assert(False);

 src1[0]:=OpNotTo(src1[0]);
 src1[1]:=OpNotTo(src1[1]);

 OpBitwiseAnd(dst[0],src0[0],src1[0]);
 OpBitwiseAnd(dst[1],src0[1],src1[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 OpLogicalOr(get_scc,src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_OR_B64; //SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of PsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC0,@src0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC1,@src1,dtUInt32) then Assert(False);

 OpBitwiseOr(dst[0],src0[0],src1[0]);
 OpBitwiseOr(dst[1],src0[1],src1[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 OpLogicalOr(get_scc,src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_ORN2_B64; //SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of PsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC0,@src0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC1,@src1,dtUInt32) then Assert(False);

 src1[0]:=OpNotTo(src1[0]);
 src1[1]:=OpNotTo(src1[1]);

 OpBitwiseOr(dst[0],src0[0],src1[0]);
 OpBitwiseOr(dst[1],src0[1],src1[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 OpLogicalOr(get_scc,src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_NOR_B64; //SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of PsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC0,@src0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC1,@src1,dtUInt32) then Assert(False);

 src2[0]:=OpBitwiseOrTo(src0[0],src1[0]);
 src2[1]:=OpBitwiseOrTo(src0[1],src1[1]);

 OpNot(dst[0],src2[0]);
 OpNot(dst[1],src2[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 OpLogicalOr(get_scc,src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_CSELECT_B32; //sdst = SCC ? ssrc0 : ssrc1
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 scc:PsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUnknow);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUnknow);
 scc:=MakeRead(get_scc,dtBool);

 OpSelect(dst,src[0],src[1],scc);
end;

procedure TEmit_SOP2.emit_S_CSELECT_B64; //sdst[2] = SCC ? ssrc0[2] : ssrc1[2]
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1:array[0..1] of PsrRegNode;
 scc:PsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC0,@src0,dtUnknow) then Assert(False);
 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC1,@src1,dtUnknow) then Assert(False);

 scc:=MakeRead(get_scc,dtBool);

 OpSelect(dst[0],src0[0],src1[0],scc);
 OpSelect(dst[1],src0[1],src1[1],scc);
end;

//offset = ssrc1[4:0].u   and 31
//width = ssrc1[22:16].u  shr 16
procedure TEmit_SOP2.emit_S_BFE_U32; //sdst.u = bitFieldExtract(ssrc0); SCC = (sdst.u != 0)
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 offset,count:PsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUint32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUint32);

 offset:=OpBitwiseAndTo(src[1],31);
 count :=OpShrTo(src[1],16);

 Op3(Op.OpBitFieldUExtract,dtUInt32,dst,src[0],offset,count);

 OpISccNotZero(dst^.current); //SCC = (sdst.u != 0)
end;

procedure TEmit_SOP2.emit_SOP2;
begin

 Case FSPI.SOP2.OP of

  S_ADD_U32: emit_S_ADD_U32;
  S_ADD_I32: emit_S_ADD_I32;

  S_ADDC_U32: emit_S_ADDC_U32;

  S_MUL_I32: emit_S_MUL_I32;

  S_LSHL_B32: emit_S_LSHL_B32;
  S_LSHR_B32: emit_S_LSHR_B32;

  S_AND_B32: emit_S_AND_B32;
  S_AND_B64: emit_S_AND_B64;

  S_ANDN2_B64: emit_S_ANDN2_B64;

  S_OR_B64: emit_S_OR_B64;

  S_ORN2_B64: emit_S_ORN2_B64;

  S_NOR_B64: emit_S_NOR_B64;

  S_CSELECT_B32: emit_S_CSELECT_B32;
  S_CSELECT_B64: emit_S_CSELECT_B64;

  S_BFE_U32: emit_S_BFE_U32;

  else
   Assert(False,'SOP2?'+IntToStr(FSPI.SOP2.OP));
 end;

end;

end.

