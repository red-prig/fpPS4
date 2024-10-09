unit emit_SOP1;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  spirv,
  srCFGLabel,
  srFlow,
  srType,
  srConst,
  srReg,
  srLayout,
  srOpUtils,
  emit_fetch;

type
 TEmit_SOP1=class(TEmitFetch)
  procedure emit_SOP1;
  Function  GetFuncPtr(src:PPsrRegNode):Pointer;
  procedure emit_S_MOV_B32;
  procedure emit_S_MOV_B64;
  procedure OpISccNotZero(src:TsrRegNode);
  procedure OpISccNotZero2(src0,src1:TsrRegNode);
  procedure emit_S_NOT_B32;
  procedure emit_S_NOT_B64;
  procedure emit_S_GETPC_B64;
  procedure emit_S_SETPC_B64;
  procedure emit_S_SWAPPC_B64;
  procedure emit_S_AND_SAVEEXEC_B64;
  procedure _OpWQM32(dst:PsrRegSlot;src:TsrRegNode);
  procedure emit_S_WQM_B32;
  procedure emit_S_WQM_B64;
  procedure emit_S_BREV_B32;
  procedure emit_S_BREV_B64;
 end;

implementation

Function TEmit_SOP1.GetFuncPtr(src:PPsrRegNode):Pointer;
var
 chain:TsrChains;
 pConst:array[0..1] of TsrConst;
 pLayout:TsrDataLayout;
begin
 Result:=nil;

 src[0]:=RegDown(src[0]);
 src[1]:=RegDown(src[1]);

 Assert(src[0]<>nil);
 Assert(src[1]<>nil);

 if (src[0].is_const) and (src[1].is_const) then
 begin
  pConst[0]:=src[0].AsConst;
  pConst[1]:=src[1].AsConst;

  QWORD(Result):=QWORD(pConst[0].AsUint32) or (QWORD(pConst[1].AsUint32) shl 32);
 end else
 begin
  chain:=Default(TsrChains);
  chain[0]:=GetChainRegNode(src[0]);
  chain[1]:=GetChainRegNode(src[1]);

  pLayout:=DataLayoutList.Grouping(chain,rtFunPtr2);
  Result:=pLayout.pData;
 end;

 Assert(Result<>nil);
end;

procedure TEmit_SOP1.emit_S_MOV_B32;
Var
 dst:PsrRegSlot;
 src:TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP1.SDST);
 src:=fetch_ssrc9(FSPI.SOP1.SSRC,dtUnknow);

 MakeCopy(dst,src);
end;

procedure TEmit_SOP1.emit_S_MOV_B64;
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP1.SDST,@dst) then Assert(false);

 if not fetch_ssrc9_pair(FSPI.SOP1.SSRC,@src,dtUnknow) then Assert(false);

 MakeCopy(dst[0],src[0]);
 MakeCopy(dst[1],src[1]);
end;

procedure TEmit_SOP1.OpISccNotZero(src:TsrRegNode); //SCC = (sdst.u != 0)
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

procedure TEmit_SOP1.OpISccNotZero2(src0,src1:TsrRegNode);
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

procedure TEmit_SOP1.emit_S_NOT_B32; //sdst = ~ssrc; SCC = (sdst != 0)
Var
 dst:PsrRegSlot;
 src:TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP1.SDST);
 src:=fetch_ssrc9(FSPI.SOP1.SSRC,dtUnknow);

 OpNot(dst,src);

 src:=MakeRead(dst,dtUnknow);

 OpISccNotZero(src);
end;

procedure TEmit_SOP1.emit_S_NOT_B64; //sdst[2] = ~ssrc0[2]; SCC = (sdst[2] != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP1.SDST,@dst) then Assert(false);

 if not fetch_ssrc9_pair(FSPI.SOP1.SSRC,@src,dtUnknow) then Assert(false);

 OpNot(dst[0],src[0]);
 OpNot(dst[1],src[1]);

 src[0]:=dst[0]^.current;
 src[1]:=dst[1]^.current;

 OpISccNotZero2(src[0],src[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP1.emit_S_GETPC_B64;
Var
 dst:array[0..1] of PsrRegSlot;

 oldptr:Pointer;
begin
 if not get_sdst7_pair(FSPI.SOP1.SDST,@dst) then Assert(false);

 oldptr:=get_code_ptr;

 SetConst_q(dst[0],dtUint32,QWORD(oldptr));
 SetConst_q(dst[1],dtUint32,QWORD(oldptr) shr 32);
end;

procedure TEmit_SOP1.emit_S_SETPC_B64;
Var
 src:array[0..1] of TsrRegNode;

 newptr:Pointer;
begin
 While (CheckBlockEnd) do;

 //ret
 if not fetch_ssrc9_pair(FSPI.SOP1.SSRC,@src,dtUnknow) then Assert(false);

 newptr:=GetFuncPtr(@src);

 set_code_ptr(newptr,btMain);

 While (CheckBlockBeg) do;
end;

procedure TEmit_SOP1.emit_S_SWAPPC_B64;
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of TsrRegNode;

 oldptr,newptr:Pointer;
begin
 //call
 if not get_sdst7_pair(FSPI.SOP1.SDST,@dst) then Assert(false);

 if not fetch_ssrc9_pair(FSPI.SOP1.SSRC,@src,dtUnknow) then Assert(false);

 newptr:=GetFuncPtr(@src);

 oldptr:=get_code_ptr;

 SetConst_q(dst[0],dtUint32,QWORD(oldptr));
 SetConst_q(dst[1],dtUint32,QWORD(oldptr) shr 32);

 set_code_ptr(newptr,btSetpc);
end;

procedure TEmit_SOP1.emit_S_AND_SAVEEXEC_B64; //sdst.du = EXEC;| EXEC = (ssrc.du & EXEC);| SCC = (sdst != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of TsrRegNode;
 exc:array[0..1] of TsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP1.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP1.SSRC,@src,dtUnknow) then Assert(False); //ssrc8

 exc[0]:=MakeRead(get_exec0,dtUnknow);
 exc[1]:=MakeRead(get_exec1,dtUnknow);

 MakeCopy(dst[0],exc[0]);
 MakeCopy(dst[1],exc[1]);

 OpBitwiseAnd(get_exec0,src[0],exc[0]);
 OpBitwiseAnd(get_exec1,src[1],exc[1]);

 //SCC = ((exc[0] != 0) or ((exc[1] != 0))

 OpISccNotZero2(exc[0],exc[1]); //implict cast (int != 0)

 //SCC = (sdst != 0)    SCC = ((exc[0] != 0) or ((exc[1] != 0))
end;

function F_WQM_32(D:DWORD):DWORD;
var
 i:Byte;
begin
 Result:=0;
 if (D=0) then Exit;
 For i:=0 to 7 do
 begin
  if (((D shr (i*4)) and 15)<>0) then
  begin
   Result:=Result or ($F shl (i*4));
  end;
 end;
end;

procedure TEmit_SOP1._OpWQM32(dst:PsrRegSlot;src:TsrRegNode);
begin
 if src.is_const then
 begin
  //early optimization
  SetConst_q(dst,src.dtype,F_WQM_32(src.AsConst.GetData));
 end else
 begin
  OpWQM32(dst,src);
 end;
end;

procedure TEmit_SOP1.emit_S_WQM_B32;
Var
 dst:PsrRegSlot;
 src:TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP1.SDST);
 src:=fetch_ssrc9(FSPI.SOP1.SSRC,dtUnknow);

 _OpWQM32(dst,src);

 src:=MakeRead(dst,dtUnknow);
 OpISccNotZero(src);
end;

//TODO: VK_KHR_shader_quad_control:RequireFullQuadsKHR
procedure TEmit_SOP1.emit_S_WQM_B64;
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of TsrRegNode;

begin
 //S_WQM_B64 EXEC_LO, EXEC_LO   //sdst.du = wholeQuadMode(ssrc.du); SCC = (sdst.du != 0)
 //dst[q*4+3:q*4].du = (ssrc[4*q+3:4*q].du != 0) ? 0xF : 0
 //dst[3:0].du = (ssrc[3:0].du != 0) ? 0xF : 0
 //dst[63:60].du = (ssrc[63:60].du != 0) ? 0xF : 0
 //if (ssrc[3:0].du != 0) then dst[63:60].du=0xF else dst[63:60].du=0

 if not get_sdst7_pair(FSPI.SOP1.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP1.SSRC,@src,dtUnknow) then Assert(False); //ssrc8

 _OpWQM32(dst[0],src[0]);
 _OpWQM32(dst[1],src[1]);

 src[0]:=dst[0]^.current;
 src[1]:=dst[1]^.current;

 OpISccNotZero2(src[0],src[1]); //implict cast (int != 0)
end;

procedure TEmit_SOP1.emit_S_BREV_B32; //sdst[31:0] = ssrc[0:31]
Var
 dst:PsrRegSlot;
 src:TsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP1.SDST);
 src:=fetch_ssrc9(FSPI.SOP1.SSRC,dtUInt32);

 Op1(Op.OpBitReverse,dtUInt32,dst,src);
end;

procedure TEmit_SOP1.emit_S_BREV_B64; //sdst[63:0] = ssrc[0:63]
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 if not get_sdst7_pair(FSPI.SOP1.SDST,@dst) then Assert(false);

 if not fetch_ssrc9_pair(FSPI.SOP1.SSRC,@src,dtUnknow) then Assert(false);

 Op1(Op.OpBitReverse,dtUInt32,dst[0],src[1]); //0 -> 1
 Op1(Op.OpBitReverse,dtUInt32,dst[1],src[0]); //1 -> 0
end;

procedure TEmit_SOP1.emit_SOP1;
begin

 Case FSPI.SOP1.OP of
  S_MOV_B32         : emit_S_MOV_B32;
  S_MOV_B64         : emit_S_MOV_B64;

  S_NOT_B32         : emit_S_NOT_B32;
  S_NOT_B64         : emit_S_NOT_B64;

  S_WQM_B32         : emit_S_WQM_B32;
  S_WQM_B64         : emit_S_WQM_B64;

  S_BREV_B32        : emit_S_BREV_B32;
  S_BREV_B64        : emit_S_BREV_B64;

  S_GETPC_B64       : emit_S_GETPC_B64;
  S_SETPC_B64       : emit_S_SETPC_B64;
  S_SWAPPC_B64      : emit_S_SWAPPC_B64;
  S_AND_SAVEEXEC_B64: emit_S_AND_SAVEEXEC_B64;
  else
   Assert(false,'SOP1?'+IntToStr(FSPI.SOP1.OP));
 end;

end;


end.





