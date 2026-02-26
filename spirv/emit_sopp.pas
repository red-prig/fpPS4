unit emit_SOPP;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srType,
  srConst,
  srOp,
  srOpUtils,
  srOpInternal,
  spirv,
  emit_fetch;

type
 TEmit_SOPP=class(TEmitFetch)
  procedure emit_SOPP;
  procedure emit_S_WAITCNT;
  procedure emit_S_BARRIER;
 end;

implementation

procedure TEmit_SOPP.emit_S_WAITCNT;
Var
 node:TspirvOp;
begin
 if (Twaitcnt_simm(FSPI.SOPP.SIMM).lgkmcnt<>15) or
    (Twaitcnt_simm(FSPI.SOPP.SIMM).expcnt <>7 ) or
    (Twaitcnt_simm(FSPI.SOPP.SIMM).vmcnt  <>15) then
 begin
  node:=AddSpirvOp(line,srOpInternal.OpWaitCnt); //need first
  node.AddLiteral(Twaitcnt_simm(FSPI.SOPP.SIMM).lgkmcnt);
  node.AddLiteral(Twaitcnt_simm(FSPI.SOPP.SIMM).expcnt );
  node.AddLiteral(Twaitcnt_simm(FSPI.SOPP.SIMM).vmcnt  );
 end;

end;

procedure TEmit_SOPP.emit_S_BARRIER;
Var
 node:TspirvOp;
 execution,memory,memory_semantics:TsrConst;
begin
 //upgrade version to 1.3
 Config.UpgradeVersion13;

 node:=AddSpirvOp(line,Op.OpControlBarrier); //need first

 execution       :=ConstList.Fetch(dtUint32,Scope.Workgroup);
 memory          :=ConstList.Fetch(dtUint32,Scope.Workgroup);
 memory_semantics:=ConstList.Fetch(dtUint32,MemorySemantics.AcquireRelease or MemorySemantics.WorkgroupMemory);

 node.AddParam(execution);
 node.AddParam(memory);
 node.AddParam(memory_semantics);
end;

procedure TEmit_SOPP.emit_SOPP;
begin
 Case FSPI.SOPP.OP of
  S_NOP,
  S_WAITCNT:emit_S_WAITCNT;

  S_TTRACEDATA:; //write_thread_trace_data(M0[31:0])
  S_SETPRIO   :; //USER_PRIO[1:0] = imm16[1:0].u

  S_ENDPGM:
   begin
    AddSpirvOp(Op.OpReturn);
    //mark hints
    mark_end_of(vmEndpg);
   end;

  S_CBRANCH_SCC0  :; //It means that (scc == 0)
  S_CBRANCH_SCC1  :; //It means that (scc == 1)
  S_CBRANCH_VCCZ  :; //It means that (vcc0  == 0) && (vcc1  == 0)
  S_CBRANCH_VCCNZ :; //It means that (vcc0  != 0) || (vcc1  != 0)
  S_CBRANCH_EXECZ :; //It means that (exec0 == 0) && (exec1 == 0)
  S_CBRANCH_EXECNZ:; //It means that (exec0 != 0) || (exec1 != 0)

  S_BRANCH        :;

  S_BARRIER       :emit_S_BARRIER;

  else
   Assert(false,'SOPP?'+IntToStr(FSPI.SOPP.OP)+' '+get_str_spi(FSPI));
 end;
end;

end.

