unit emit_SOP2;

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
 TEmit_SOP2=object(TEmitOp)
  procedure _emit_SOP2;
  procedure _emit_S_ADD_I32;
  procedure _emit_S_MUL_I32;
  procedure _emit_S_LSHL_B32;
  procedure _emit_S_LSHR_B32;
  procedure _emit_S_AND_B32;
  procedure _emit_S_AND_B64;
  procedure _emit_S_ANDN2_B64;
  procedure _emit_S_OR_B64;
  procedure _emit_S_NOR_B64;
  procedure _emit_S_CSELECT_B32;
  procedure _emit_S_CSELECT_B64;
  procedure _emit_S_BFE_U32;
 end;

implementation

procedure TEmit_SOP2._emit_S_ADD_I32;
Var
 dst,car:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_sdst7(FSPI.SOP2.SDST);
 car:=@FRegsStory.SCC;

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtInt32);

 emit_OpIAddExt(dst,car,src[0],src[1]);
end;

procedure TEmit_SOP2._emit_S_MUL_I32;
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtInt32);

 emit_OpIMul(dst,src[0],src[1]);
end;

procedure TEmit_SOP2._emit_S_LSHL_B32; //SCC = (sdst.u != 0)
Var
 dst,tmp:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_sdst7(FSPI.SOP2.SDST);
 tmp:=@FRegsStory.FUnattach;

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 src[2]:=FetchReg(FConsts.Fetch(dtUInt32,31));
 emit_OpBitwiseAnd(tmp,src[1],src[2]);
 src[1]:=MakeRead(tmp,dtUInt32);

 emit_OpShl(dst,src[0],src[1]);

 //dst = dtUint32

 tmp:=@FRegsStory.SCC;
 MakeCopy(tmp,dst^.current);
 tmp^.current^.dtype:=dtBool;

 //dst^.current^.mark_read;
 //emit_IntToBool(line,FRegsStory.SCC.New(dtBool),dst^.current);
end;

procedure TEmit_SOP2._emit_S_LSHR_B32; //SCC = (sdst.u != 0)
Var
 dst,tmp:PsrRegSlot;
 src:array[0..2] of PsrRegNode;
begin
 dst:=FRegsStory.get_sdst7(FSPI.SOP2.SDST);
 tmp:=@FRegsStory.FUnattach;

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 src[2]:=FetchReg(FConsts.Fetch(dtUInt32,31));
 emit_OpBitwiseAnd(tmp,src[1],src[2]);
 src[1]:=MakeRead(tmp,dtUInt32);

 emit_OpShr(dst,src[0],src[1]);

 tmp:=@FRegsStory.SCC;
 MakeCopy(tmp,dst^.current);
 tmp^.current^.dtype:=dtBool;
end;

procedure TEmit_SOP2._emit_S_AND_B32; //SCC = (sdst.u != 0)
Var
 dst,tmp:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=FRegsStory.get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUInt32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUInt32);

 emit_OpBitwiseAnd(dst,src[0],src[1]);

 tmp:=@FRegsStory.SCC;
 MakeCopy(tmp,dst^.current);
 tmp^.current^.dtype:=dtBool;
end;

procedure TEmit_SOP2._emit_S_AND_B64; //SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of PsrRegNode;
begin
 if not FRegsStory.get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(@src0,FSPI.SOP2.SSRC0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(@src1,FSPI.SOP2.SSRC1,dtUInt32) then Assert(False);

 emit_OpBitwiseAnd(dst[0],src0[0],src1[0]);
 emit_OpBitwiseAnd(dst[1],src0[1],src1[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 src2[0]^.mark_read;
 src2[1]^.mark_read;
 emit_OpLogicalOr(@FRegsStory.SCC,src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2._emit_S_ANDN2_B64; //SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 tmp:PsrRegSlot;
 src0,src1,src2:array[0..1] of PsrRegNode;
begin
 if not FRegsStory.get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(@src0,FSPI.SOP2.SSRC0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(@src1,FSPI.SOP2.SSRC1,dtUInt32) then Assert(False);

 tmp:=@FRegsStory.FUnattach;

 emit_OpNot(tmp,src1[0]);
 src1[0]:=MakeRead(tmp,dtUnknow);

 emit_OpNot(tmp,src1[1]);
 src1[1]:=MakeRead(tmp,dtUnknow);

 emit_OpBitwiseAnd(dst[0],src0[0],src1[0]);
 emit_OpBitwiseAnd(dst[1],src0[1],src1[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 src2[0]^.mark_read;
 src2[1]^.mark_read;
 emit_OpLogicalOr(@FRegsStory.SCC,src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2._emit_S_OR_B64; //SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of PsrRegNode;
begin
 if not FRegsStory.get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(@src0,FSPI.SOP2.SSRC0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(@src1,FSPI.SOP2.SSRC1,dtUInt32) then Assert(False);

 emit_OpBitwiseOr(dst[0],src0[0],src1[0]);
 emit_OpBitwiseOr(dst[1],src0[1],src1[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 src2[0]^.mark_read;
 src2[1]^.mark_read;
 emit_OpLogicalOr(@FRegsStory.SCC,src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2._emit_S_NOR_B64; //SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1,src2:array[0..1] of PsrRegNode;
begin
 if not FRegsStory.get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(@src0,FSPI.SOP2.SSRC0,dtUInt32) then Assert(False);
 if not fetch_ssrc9_pair(@src1,FSPI.SOP2.SSRC1,dtUInt32) then Assert(False);

 emit_OpBitwiseOr(dst[0],src0[0],src1[0]);
 emit_OpBitwiseOr(dst[1],src0[1],src1[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 src2[0]^.mark_read;
 src2[1]^.mark_read;

 emit_OpNot(dst[0],src2[0]);
 emit_OpNot(dst[1],src2[1]);

 src2[0]:=dst[0]^.current;
 src2[1]:=dst[1]^.current;

 src2[0]^.mark_read;
 src2[1]^.mark_read;

 emit_OpLogicalOr(@FRegsStory.SCC,src2[0],src2[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP2._emit_S_CSELECT_B32; //sdst = SCC ? ssrc0 : ssrc1
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 scc:PsrRegNode;
begin
 dst:=FRegsStory.get_sdst7(FSPI.SOP2.SDST);

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUnknow);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUnknow);
 scc:=MakeRead(@FRegsStory.SCC,dtBool);

 emit_OpSelect(dst,src[0],src[1],scc);
end;

procedure TEmit_SOP2._emit_S_CSELECT_B64; //sdst[2] = SCC ? ssrc0[2] : ssrc1[2]
Var
 dst:array[0..1] of PsrRegSlot;
 src0,src1:array[0..1] of PsrRegNode;
 scc:PsrRegNode;
begin
 if not FRegsStory.get_sdst7_pair(FSPI.SOP2.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(@src0,FSPI.SOP2.SSRC0,dtUnknow) then Assert(False);
 if not fetch_ssrc9_pair(@src1,FSPI.SOP2.SSRC1,dtUnknow) then Assert(False);

 scc:=MakeRead(@FRegsStory.SCC,dtBool);
 scc^.mark_read;

 emit_OpSelect(dst[0],src0[0],src1[0],scc);
 emit_OpSelect(dst[1],src0[1],src1[1],scc);
end;

//offset = ssrc1[4:0].u   and 31
//width = ssrc1[22:16].u  shr 16
procedure TEmit_SOP2._emit_S_BFE_U32; //sdst.u = bitFieldExtract(ssrc0); SCC = (sdst.u != 0)
Var
 dst,tmp:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 num_31,num_16:PsrRegNode;
 offset,count:PsrRegNode;
begin
 dst:=FRegsStory.get_sdst7(FSPI.SOP2.SDST);
 tmp:=@FRegsStory.FUnattach;

 src[0]:=fetch_ssrc9(FSPI.SOP2.SSRC0,dtUint32);
 src[1]:=fetch_ssrc9(FSPI.SOP2.SSRC1,dtUint32);

 num_31:=FetchReg(FConsts.Fetch(dtUInt32,31));

 emit_OpBitwiseAnd(tmp,src[1],num_31);
 offset:=MakeRead(tmp,dtUInt32);

 num_16:=FetchReg(FConsts.Fetch(dtUInt32,16));

 src[1]^.mark_read;
 emit_OpShr(tmp,src[1],num_16);
 count:=MakeRead(tmp,dtUInt32);

 emit_OpBfeU(dst,src[0],offset,count);

 tmp:=@FRegsStory.SCC;
 MakeCopy(tmp,dst^.current);
 tmp^.current^.dtype:=dtBool;
end;

procedure TEmit_SOP2._emit_SOP2;
begin

 Case FSPI.SOP2.OP of

  S_ADD_I32:
   begin
    _emit_S_ADD_I32;
   end;

  S_MUL_I32: //& 0xFFFFFFFF
    begin
     _emit_S_MUL_I32;
    end;

  S_LSHL_B32: //SCC = (sdst.u != 0)
    begin
     _emit_S_LSHL_B32;
    end;

  S_LSHR_B32:
    begin
     _emit_S_LSHR_B32;
    end;

  S_AND_B32:
    begin
     _emit_S_AND_B32;
    end;

  S_AND_B64:
    begin
     _emit_S_AND_B64;
    end;

  S_ANDN2_B64:
    begin
     _emit_S_ANDN2_B64;
    end;

  S_OR_B64:
    begin
     _emit_S_OR_B64;
    end;

  S_NOR_B64:
    begin
     _emit_S_NOR_B64;
    end;

  S_CSELECT_B32:
    begin
     _emit_S_CSELECT_B32;
    end;

  S_CSELECT_B64:
    begin
     _emit_S_CSELECT_B64;
    end;

  S_BFE_U32:
    begin
     _emit_S_BFE_U32;
    end;

  else
   Assert(False,'SOP2?'+IntToStr(FSPI.SOP2.OP));
 end;

end;

end.

