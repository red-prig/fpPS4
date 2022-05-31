unit emit_SOP1;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srLabel,
  srTypes,
  srConst,
  srReg,
  srLayout,
  SprvEmit,
  emit_op;

type
 TEmit_SOP1=object(TEmitOp)
  procedure _emit_SOP1;
  Function  _GetFuncPtr(src:PPsrRegSlot):Pointer;
  procedure _emit_S_MOV_B32;
  procedure _emit_S_MOV_B64;
  procedure _emit_S_SWAPPC_B64;
  procedure _emit_S_SETPC_B64;
  procedure _emit_S_AND_SAVEEXEC_B64;
  procedure _emit_S_WQM_B64;
 end;

implementation

Function TEmit_SOP1._GetFuncPtr(src:PPsrRegSlot):Pointer;
var
 chain:TsrChains;
 pLayout:PsrDataLayout;

begin
 Result:=nil;

 chain:=Default(TsrChains);
 chain[0]:=GetChainRegNode(src[0]^.current);
 chain[1]:=GetChainRegNode(src[1]^.current);

 pLayout:=FDataLayouts.Grouping(chain,rtFunPtr2);
 Result:=pLayout^.pData;

 Assert(Result<>nil);
end;

procedure TEmit_SOP1._emit_S_MOV_B32;
Var
 dst:PsrRegSlot;
 src:PsrRegNode;
begin
 dst:=FRegsStory.get_sdst7(FSPI.SOP1.SDST);
 src:=fetch_ssrc9(FSPI.SOP1.SSRC,dtUnknow);
 _MakeCopy(dst,src);
end;

procedure TEmit_SOP1._emit_S_MOV_B64;
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst[0]:=FRegsStory.get_sdst7(FSPI.SOP1.SDST+0);
 dst[1]:=FRegsStory.get_sdst7(FSPI.SOP1.SDST+1);

 src[0]:=fetch_ssrc9(FSPI.SOP1.SSRC+0,dtUnknow);
 src[1]:=fetch_ssrc9(FSPI.SOP1.SSRC+1,dtUnknow);

 _MakeCopy(dst[0],src[0]);
 _MakeCopy(dst[1],src[1]);
end;

procedure TEmit_SOP1._emit_S_SWAPPC_B64;
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of PsrRegSlot;

 oldptr,newptr:Pointer;

begin
 if not FRegsStory.get_sdst7_pair(FSPI.SOP1.SDST,@dst) then Assert(false);

 Assert(not is_const_ssrc9(FSPI.SOP1.SSRC));

 if not FRegsStory.get_ssrc9_pair(FSPI.SOP1.SSRC,@src) then Assert(false);

 newptr:=_GetFuncPtr(@src);
 Assert(newptr<>nil);

 oldptr:=FCursor.Adr.get_pc;

 SetConst(dst[0],dtUint32,{%H-}QWORD(oldptr));
 SetConst(dst[1],dtUint32,{%H-}QWORD(oldptr) shr 32);

 SetPtr(newptr,btSetpc);
end;

procedure TEmit_SOP1._emit_S_SETPC_B64;
Var
 src:array[0..1] of PsrRegSlot;
 node:array[0..1] of PsrRegNode;
 pConst:array[0..1] of PsrConst;

 newptr:Pointer;

begin
 Assert(not is_const_ssrc9(FSPI.SOP1.SSRC));
 if not FRegsStory.get_ssrc9_pair(FSPI.SOP1.SSRC,@src) then Assert(false);

 node[0]:=RegDown(src[0]^.current);
 node[1]:=RegDown(src[1]^.current);

 Assert(node[0]<>nil);
 Assert(node[1]<>nil);

 if (node[0]^.is_const) and (node[1]^.is_const) then
 begin
  //ret of func
  pConst[0]:=node[0]^.AsConst;
  pConst[1]:=node[1]^.AsConst;

  {%H-}QWORD(newptr):=QWORD(pConst[0]^.AsUint) or (QWORD(pConst[1]^.AsUint) shl 32);

  SetPtr(newptr,btMain);
 end else
 begin
  Assert(false);
 end;

end;

procedure TEmit_SOP1._emit_S_AND_SAVEEXEC_B64; //sdst.du = EXEC;| EXEC = (ssrc.du & EXEC);| SCC = (sdst != 0)
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of PsrRegNode;
 exc:array[0..1] of PsrRegNode;

begin

 dst[0]:=FRegsStory.get_sdst7(FSPI.SOP1.SDST+0);
 dst[1]:=FRegsStory.get_sdst7(FSPI.SOP1.SDST+1);

 Assert(dst[0]<>nil);
 Assert(dst[1]<>nil);

 if not fetch_ssrc9_pair(@src,FSPI.SOP1.SSRC,dtUnknow) then Assert(False); //ssrc8

 PrepTypeSlot(@FRegsStory.EXEC[0],dtUnknow);
 PrepTypeSlot(@FRegsStory.EXEC[1],dtUnknow);

 exc[0]:=FRegsStory.EXEC[0].current;
 exc[1]:=FRegsStory.EXEC[1].current;

 MakeCopy(dst[0],exc[0]);
 MakeCopy(dst[1],exc[1]);

 exc[0]^.mark_read;
 exc[1]^.mark_read;

 emit_OpBitwiseAnd(@FRegsStory.EXEC[0],src[0],exc[0]);
 emit_OpBitwiseAnd(@FRegsStory.EXEC[1],src[1],exc[1]);

 //SCC = ((exc[0] != 0) or ((exc[1] != 0))

 exc[0]^.mark_read;
 exc[1]^.mark_read;

 emit_OpLogicalOr(@FRegsStory.SCC,exc[0],exc[1]); //implict cast (int != 0)

 //SCC = (sdst != 0)    SCC = ((exc[0] != 0) or ((exc[1] != 0))
end;

procedure TEmit_SOP1._emit_S_WQM_B64;
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of PsrRegNode;

begin
 //S_WQM_B64 EXEC_LO, EXEC_LO   //sdst.du = wholeQuadMode(ssrc.du); SCC = (sdst.du != 0)
 //dst[q*4+3:q*4].du = (ssrc[4*q+3:4*q].du != 0) ? 0xF : 0
 //dst[3:0].du = (ssrc[3:0].du != 0) ? 0xF : 0
 //dst[63:60].du = (ssrc[63:60].du != 0) ? 0xF : 0
 //if (ssrc[3:0].du != 0) then dst[63:60].du=0xF else dst[63:60].du=0

 dst[0]:=FRegsStory.get_sdst7(FSPI.SOP1.SDST+0);
 dst[1]:=FRegsStory.get_sdst7(FSPI.SOP1.SDST+1);

 Assert(dst[0]<>nil);
 Assert(dst[1]<>nil);

 if not fetch_ssrc9_pair(@src,FSPI.SOP1.SSRC,dtUnknow) then Assert(False); //ssrc8

 emit_WQM_32(dst[0],src[0]);
 emit_WQM_32(dst[1],src[1]);
end;

procedure TEmit_SOP1._emit_SOP1;
begin

 Case FSPI.SOP1.OP of
  S_MOV_B32:
    begin
     _emit_S_MOV_B32;
    end;

  S_MOV_B64:
    begin
     _emit_S_MOV_B64;
    end;

  S_WQM_B64:
    begin
     _emit_S_WQM_B64;
    end;

  S_SWAPPC_B64:
    begin
     _emit_S_SWAPPC_B64;
    end;

  S_SETPC_B64:
    begin
     _emit_S_SETPC_B64;
    end;

  S_AND_SAVEEXEC_B64:
    begin
     _emit_S_AND_SAVEEXEC_B64;
    end;

  else
   Assert(false,'SOP1?'+IntToStr(FSPI.SOP1.OP));
 end;

end;


end.





