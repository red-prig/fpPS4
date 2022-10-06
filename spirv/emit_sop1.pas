unit emit_SOP1;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
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
  procedure emit_S_GETPC_B64;
  procedure emit_S_SETPC_B64;
  procedure emit_S_SWAPPC_B64;
  procedure emit_S_AND_SAVEEXEC_B64;
  procedure emit_S_WQM_B64;
 end;

implementation

Function TEmit_SOP1.GetFuncPtr(src:PPsrRegNode):Pointer;
var
 chain:TsrChains;
 pConst:array[0..1] of PsrConst;
 pLayout:PsrDataLayout;
begin
 Result:=nil;

 src[0]:=RegDown(src[0]);
 src[1]:=RegDown(src[1]);

 Assert(src[0]<>nil);
 Assert(src[1]<>nil);

 if (src[0]^.is_const) and (src[1]^.is_const) then
 begin
  pConst[0]:=src[0]^.AsConst;
  pConst[1]:=src[1]^.AsConst;

  QWORD(Result):=QWORD(pConst[0]^.AsUint32) or (QWORD(pConst[1]^.AsUint32) shl 32);
 end else
 begin
  chain:=Default(TsrChains);
  chain[0]:=GetChainRegNode(src[0]);
  chain[1]:=GetChainRegNode(src[1]);

  pLayout:=DataLayoutList.Grouping(chain,rtFunPtr2);
  Result:=pLayout^.pData;
 end;

 Assert(Result<>nil);
end;

procedure TEmit_SOP1.emit_S_MOV_B32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=get_sdst7(FSPI.SOP1.SDST);
 src:=fetch_ssrc9(FSPI.SOP1.SSRC,dtUnknow);
 MakeCopy(dst,src);
end;

procedure TEmit_SOP1.emit_S_MOV_B64;
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst[0]:=get_sdst7(FSPI.SOP1.SDST+0);
 dst[1]:=get_sdst7(FSPI.SOP1.SDST+1);

 src[0]:=fetch_ssrc9(FSPI.SOP1.SSRC+0,dtUnknow);
 src[1]:=fetch_ssrc9(FSPI.SOP1.SSRC+1,dtUnknow);

 MakeCopy(dst[0],src[0]);
 MakeCopy(dst[1],src[1]);
end;

procedure TEmit_SOP1.emit_S_GETPC_B64;
Var
 dst:array[0..1] of PsrRegSlot;

 oldptr:Pointer;
begin
 if not get_sdst7_pair(FSPI.SOP1.SDST,@dst) then Assert(false);

 oldptr:=GetPtr;

 SetConst_q(dst[0],dtUint32,QWORD(oldptr));
 SetConst_q(dst[1],dtUint32,QWORD(oldptr) shr 32);
end;

procedure TEmit_SOP1.emit_S_SETPC_B64;
Var
 src:array[0..1] of PsrRegNode;

 newptr:Pointer;
begin
 if not fetch_ssrc9_pair(FSPI.SOP1.SSRC,@src,dtUnknow) then Assert(false);

 newptr:=GetFuncPtr(@src);

 SetPtr(newptr,btMain);
end;

procedure TEmit_SOP1.emit_S_SWAPPC_B64;
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of PsrRegNode;

 oldptr,newptr:Pointer;
begin
 if not get_sdst7_pair(FSPI.SOP1.SDST,@dst) then Assert(false);

 if not fetch_ssrc9_pair(FSPI.SOP1.SSRC,@src,dtUnknow) then Assert(false);

 newptr:=GetFuncPtr(@src);

 oldptr:=GetPtr;

 SetConst_q(dst[0],dtUint32,QWORD(oldptr));
 SetConst_q(dst[1],dtUint32,QWORD(oldptr) shr 32);

 SetPtr(newptr,btSetpc);
end;

procedure TEmit_SOP1.emit_S_AND_SAVEEXEC_B64; //sdst.du = EXEC;| EXEC = (ssrc.du & EXEC);| SCC = (sdst != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 exc:array[0..1] of PsrRegNode;
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

 OpLogicalOr(get_scc,exc[0],exc[1]); //implict cast (int != 0)

 //SCC = (sdst != 0)    SCC = ((exc[0] != 0) or ((exc[1] != 0))
end;

procedure TEmit_SOP1.emit_S_WQM_B64;
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of PsrRegNode;

begin
 //S_WQM_B64 EXEC_LO, EXEC_LO   //sdst.du = wholeQuadMode(ssrc.du); SCC = (sdst.du != 0)
 //dst[q*4+3:q*4].du = (ssrc[4*q+3:4*q].du != 0) ? 0xF : 0
 //dst[3:0].du = (ssrc[3:0].du != 0) ? 0xF : 0
 //dst[63:60].du = (ssrc[63:60].du != 0) ? 0xF : 0
 //if (ssrc[3:0].du != 0) then dst[63:60].du=0xF else dst[63:60].du=0

 if not get_sdst7_pair(FSPI.SOP1.SDST,@dst) then Assert(False);

 if not fetch_ssrc9_pair(FSPI.SOP1.SSRC,@src,dtUnknow) then Assert(False); //ssrc8

 OpWQM32(dst[0],src[0]);
 OpWQM32(dst[1],src[1]);
end;

procedure TEmit_SOP1.emit_SOP1;
begin

 Case FSPI.SOP1.OP of
  S_MOV_B32         : emit_S_MOV_B32;
  S_MOV_B64         : emit_S_MOV_B64;
  S_WQM_B64         : emit_S_WQM_B64;
  S_GETPC_B64       : emit_S_GETPC_B64;
  S_SETPC_B64       : emit_S_SETPC_B64;
  S_SWAPPC_B64      : emit_S_SWAPPC_B64;
  S_AND_SAVEEXEC_B64: emit_S_AND_SAVEEXEC_B64;
  else
   Assert(false,'SOP1?'+IntToStr(FSPI.SOP1.OP));
 end;

end;


end.





