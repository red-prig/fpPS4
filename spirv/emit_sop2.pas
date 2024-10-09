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
  procedure emit_S_ADD_I32;
  procedure emit_S_ADD_U32;
  procedure emit_S_SUB_I32;
  procedure emit_S_SUB_U32;
  procedure emit_S_ADDC_U32;
  procedure emit_S_MUL_I32;
  procedure OpISccNotZero(src:TsrRegNode);
  procedure OpISccNotZero2(src0,src1:TsrRegNode);
  procedure emit_S_SH(OpId:DWORD;rtype:TsrDataType);
  procedure emit_S_AND_B32;
  procedure emit_S_AND_B64;
  procedure emit_S_ANDN2_B64;
  procedure emit_S_OR_B32;
  procedure emit_S_OR_B64;
  procedure emit_S_XOR_B32;
  procedure emit_S_XOR_B64;
  procedure emit_S_ORN2_B64;
  procedure emit_S_NAND_B64;
  procedure emit_S_NOR_B64;
  procedure emit_S_CSELECT_B32;
  procedure emit_S_CSELECT_B64;
  procedure emit_S_BFE_U32;
  procedure emit_S_BFM_B32;
 end;

implementation

procedure TEmit_SOP2.emit_S_ADD_I32;
Var
 dst,car:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 x,y,d:TsrRegNode;
 a,b:TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);
 car:=get_scc;

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtInt32);

 //Force type
 src[0]:=BitcastList.FetchRead(dtInt32,src[0]);
 src[1]:=BitcastList.FetchRead(dtInt32,src[1]);

 OpIAdd(dst,src[0],src[1]);

 //scc = sign(x) == sign(y) && sign(d) != sign(x);

 x:=OpIsSSignTo(src[0]);
 y:=OpIsSSignTo(src[1]);
 d:=OpIsSSignTo(dst^.current);

 a:=OpEqualTo(x,y);
 b:=OpNotEqualTo(d,x);

 OpBitwiseAnd(car,a,b);
end;

procedure TEmit_SOP2.emit_S_ADD_U32;
Var
 dst,car:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);
 car:=get_scc;

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 OpIAddExt(dst,car,src[0],src[1],dtUInt32);
end;

procedure TEmit_SOP2.emit_S_SUB_I32;
Var
 dst,bor:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 x,y,d:TsrRegNode;
 a,b:TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);
 bor:=get_scc;

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtInt32);

 //Force type
 src[0]:=BitcastList.FetchRead(dtInt32,src[0]);
 src[1]:=BitcastList.FetchRead(dtInt32,src[1]);

 OpISub(dst,src[0],src[1]);

 //scc = sign(x) != sign(y) && sign(d) != sign(x);

 x:=OpIsSSignTo(src[0]);
 y:=OpIsSSignTo(src[1]);
 d:=OpIsSSignTo(dst^.current);

 a:=OpNotEqualTo(x,y);
 b:=OpNotEqualTo(d,x);

 OpBitwiseAnd(bor,a,b);
end;

procedure TEmit_SOP2.emit_S_SUB_U32;
Var
 dst,bor:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);
 bor:=get_scc;

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 OpISubExt(dst,bor,src[0],src[1],dtUInt32);
end;

procedure TEmit_SOP2.emit_S_ADDC_U32;
Var
 dst,car:PsrRegSlot;
 src:array[0..2] of TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);
 car:=get_scc;

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);
 src[2]:=MakeRead(car,dtUInt32);

 OpIAddExt(dst,car,src[0],src[1],dtUInt32); //src0+src1

 src[0]:=MakeRead(dst,dtUInt32);
 src[1]:=MakeRead(car,dtUInt32);   //save car1

 OpIAddExt(dst,car,src[0],src[2],dtUInt32); //(src0+src1)+SCC

 src[0]:=MakeRead(car,dtUInt32);

 OpBitwiseOr(car,src[1],src[0]);   //SCC1 or SCC2
end;

procedure TEmit_SOP2.emit_S_MUL_I32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtInt32);

 OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_SOP2.OpISccNotZero(src:TsrRegNode); //SCC = (sdst.u != 0)
begin
 if src.is_const then
 begin
  //early optimization
  SetConst_b(get_scc,src.AsConst.AsBool);
 end else
 begin
  MakeCopy(get_scc,src);
  get_scc^.current.dtype:=dtBool; //implict cast (int != 0)
 end;
end;

procedure TEmit_SOP2.OpISccNotZero2(src0,src1:TsrRegNode);
begin
 if src0.is_const and
    src1.is_const then
 begin
  //early optimization
  SetConst_b(get_scc,src0.AsConst.AsBool or src1.AsConst.AsBool);
 end else
 begin
  OpLogicalOr(get_scc,src0,src1); //implict cast (int != 0)
 end;
end;

procedure TEmit_SOP2.emit_S_SH(OpId:DWORD;rtype:TsrDataType);
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,rtype);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 src[1]:=OpAndTo(src[1],31);
 src[1].PrepType(ord(dtUInt32));

 Op2(OpId,src[0].dtype,dst,src[0],src[1]);

 OpISccNotZero(dst^.current); //SCC = (sdst.u != 0)
end;

procedure TEmit_SOP2.emit_S_AND_B32; //sdst = (ssrc0 & ssrc1); SCC = (sdst != 0)
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 OpBitwiseAnd(dst,src[0],src[1]);

 OpISccNotZero(dst^.current); //SCC = (sdst.u != 0)
end;

procedure TEmit_SOP2.emit_S_AND_B64; //sdst[2] = (ssrc0[2] & ssrc1[2]); SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of TsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC0,@src0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC1,@src1,dtUInt32) then Assert(False);

 OpBitwiseAnd(dst[0],src0[0],src1[0]);
 OpBitwiseAnd(dst[1],src0[1],src1[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 OpISccNotZero2(src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_ANDN2_B64; //sdst[2] = (ssrc0[2] & ~ssrc1[2]); SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of TsrRegNode;
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

 OpISccNotZero2(src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_OR_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 OpBitwiseOr(dst,src[0],src[1]);

 OpISccNotZero(dst^.current); //SCC = (sdst.u != 0)
end;

procedure TEmit_SOP2.emit_S_OR_B64; //sdst[2] = (ssrc0[2] | ssrc1[2]); SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of TsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC0,@src0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC1,@src1,dtUInt32) then Assert(False);

 OpBitwiseOr(dst[0],src0[0],src1[0]);
 OpBitwiseOr(dst[1],src0[1],src1[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 OpISccNotZero2(src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_XOR_B32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 OpBitwiseXor(dst,src[0],src[1]);

 OpISccNotZero(dst^.current); //SCC = (sdst.u != 0)
end;

procedure TEmit_SOP2.emit_S_XOR_B64; //sdst[2] = (ssrc0[2] ^ ssrc1[2]); SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of TsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC0,@src0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC1,@src1,dtUInt32) then Assert(False);

 OpBitwiseXor(dst[0],src0[0],src1[0]);
 OpBitwiseXor(dst[1],src0[1],src1[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 OpISccNotZero2(src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_ORN2_B64; //sdst[2] = (ssrc0[2] | ~ssrc1[2]); SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of TsrRegNode;
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

 OpISccNotZero2(src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_NAND_B64; //sdst[2] = ~(ssrc0[2] & ssrc1[2]); SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of TsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC0,@src0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC1,@src1,dtUInt32) then Assert(False);

 src2[0]:=OpAndTo(src0[0],src1[0]);
 src2[1]:=OpAndTo(src0[1],src1[1]);

 OpNot(dst[0],src2[0]);
 OpNot(dst[1],src2[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 OpISccNotZero2(src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_NOR_B64; //sdst[2] = ~(ssrc0[2] | ssrc1[2]); SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of TsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC0,@src0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(FSPI.SOP2.SSRC1,@src1,dtUInt32) then Assert(False);

 src2[0]:=OpOrTo(src0[0],src1[0]);
 src2[1]:=OpOrTo(src0[1],src1[1]);

 OpNot(dst[0],src2[0]);
 OpNot(dst[1],src2[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 OpISccNotZero2(src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2.emit_S_CSELECT_B32; //sdst = SCC ? ssrc0 : ssrc1
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 scc:TsrRegNode;
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
 src0,src1:array[0..1] of TsrRegNode;
 scc:TsrRegNode;
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
 src:array[0..1] of TsrRegNode;
 offset,count:TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 offset:=OpAndTo(src[1],31);
 count :=OpShrTo(src[1],16);

 Op3(Op.OpBitFieldUExtract,dtUInt32,dst,src[0],offset,count);

 OpISccNotZero(dst^.current); //SCC = (sdst.u != 0)
end;

procedure TEmit_SOP2.emit_S_BFM_B32; //sdst.u = ((1 << ssrc0.u[4:0])-1) << ssrc1[4:0]
Var
 dst:PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 one:TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 src[0]:=OpAndTo(src[0],31);
 src[1]:=OpAndTo(src[1],31);

 src[0].PrepType(ord(dtUInt32));
 src[1].PrepType(ord(dtUInt32));

 one:=NewReg_q(dtUInt32,1);

 src[0]:=OpShrTo(one,src[0]); //(1 << src0)
 src[0]:=OpISubTo(src[0],1);  //-1

 Op2(Op.OpShiftRightLogical,dtUInt32,dst,src[0],src[1]);
end;

procedure TEmit_SOP2.emit_SOP2;
begin

 Case FSPI.SOP2.OP of

  S_ADD_U32: emit_S_ADD_U32;
  S_ADD_I32: emit_S_ADD_I32;

  S_SUB_U32: emit_S_SUB_U32;
  S_SUB_I32: emit_S_SUB_I32;

  S_ADDC_U32: emit_S_ADDC_U32;

  S_MUL_I32: emit_S_MUL_I32;

  S_LSHL_B32: emit_S_SH(Op.OpShiftLeftLogical    ,dtUInt32);
  S_LSHR_B32: emit_S_SH(Op.OpShiftRightLogical   ,dtUInt32);
  S_ASHR_I32: emit_S_SH(Op.OpShiftRightArithmetic,dtInt32);

  S_AND_B32: emit_S_AND_B32;
  S_AND_B64: emit_S_AND_B64;

  S_ANDN2_B64: emit_S_ANDN2_B64;

  S_OR_B32: emit_S_OR_B32;
  S_OR_B64: emit_S_OR_B64;

  S_XOR_B32: emit_S_XOR_B32;
  S_XOR_B64: emit_S_XOR_B64;

  S_ORN2_B64: emit_S_ORN2_B64;

  S_NAND_B64: emit_S_NAND_B64;

  S_NOR_B64: emit_S_NOR_B64;

  S_CSELECT_B32: emit_S_CSELECT_B32;
  S_CSELECT_B64: emit_S_CSELECT_B64;

  S_BFE_U32: emit_S_BFE_U32;
  S_BFM_B32: emit_S_BFM_B32;

  else
   Assert(False,'SOP2?'+IntToStr(FSPI.SOP2.OP)+' '+get_str_spi(FSPI));
 end;

end;

end.

