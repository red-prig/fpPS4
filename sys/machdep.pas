unit machdep;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 signal,
 signalvar,
 ucontext,
 kern_thread;

procedure sendsig(catcher:sig_t;ksi:p_ksiginfo;mask:p_sigset_t);
function  sys_sigreturn(sigcntxp:p_ucontext_t):Integer;

implementation

uses
 systm,
 md_psl,
 kern_sig,
 trap;

procedure sendsig(catcher:sig_t;ksi:p_ksiginfo;mask:p_sigset_t);
var
 td:p_kthread;
 sf:sigframe;
 sfp:p_sigframe;
 regs:p_trapframe;
 sp:QWORD;
 sig,oonstack:Integer;
begin
 //char *xfpusave;
 //size_t xfpusave_len;

 td:=curkthread;

 sig:=ksi^.ksi_info.si_signo;

 //psp:=p_sigacts

 regs:=td^.td_frame;
 oonstack:=sigonstack(regs^.tf_rsp);

 //if (cpu_max_ext_state_size > sizeof(struct savefpu) && use_xsave) {
 // xfpusave_len = cpu_max_ext_state_size - sizeof(struct savefpu);
 // xfpusave = __builtin_alloca(xfpusave_len);
 //} else {
 // xfpusave_len = 0;
 // xfpusave = NULL;
 //}

 // Save user context.
 sf:=Default(sigframe);

 sf.sf_uc.uc_sigmask:=mask^;
 sf.sf_uc.uc_stack  :=td^.td_sigstk;

 if ((td^.td_pflags and TDP_ALTSTACK)<>0) then
 begin
  if (oonstack<>0) then
  begin
   sf.sf_uc.uc_stack.ss_flags:=SS_ONSTACK;
  end else
  begin
   sf.sf_uc.uc_stack.ss_flags:=0;
  end;
 end else
 begin
  sf.sf_uc.uc_stack.ss_flags:=SS_DISABLE;
 end;

 sf.sf_uc.uc_mcontext.mc_onstack:=oonstack;

 Move(regs^,sf.sf_uc.uc_mcontext.mc_rdi,SizeOf(trapframe));

 sf.sf_uc.uc_mcontext.mc_len:=sizeof(mcontext_t);

 //get_fpcontext(td, &sf.sf_uc.uc_mcontext, xfpusave, xfpusave_len);
 //fpstate_drop(td);

 //sf.sf_uc.uc_mcontext.mc_fsbase = pcb->pcb_fsbase;
 //sf.sf_uc.uc_mcontext.mc_gsbase = pcb->pcb_gsbase;

 if ((td^.td_pflags and TDP_ALTSTACK)<>0) and
    (oonstack=0) and
    SIGISMEMBER(@p_sigacts.ps_sigonstack,sig) then
 begin
  sp:=QWORD(td^.td_sigstk.ss_sp)+td^.td_sigstk.ss_size;
 end else
 begin
  sp:=regs^.tf_rsp-128;
 end;

 //if (xfpusave != NULL) {
 // sp -= xfpusave_len;
 // sp = (char *)((unsigned long)sp & ~0x3Ful);
 // sf.sf_uc.uc_mcontext.mc_xfpustate = (register_t)sp;
 //}

 sp:=sp-sizeof(sigframe);

 sfp:=p_sigframe(sp and (not $F));

 regs^.tf_rdi:=sig;
 regs^.tf_rdx:=QWORD(@sfp^.sf_uc);

 sf.sf_si:=Default(siginfo_t);

 if (SIGISMEMBER(@p_sigacts.ps_siginfo,sig)) then
 begin
  regs^.tf_rsi:=QWORD(@sfp^.sf_si);
  sf.sf_ahu:=Pointer(catcher);

  sf.sf_si:=ksi^.ksi_info;
  sf.sf_si.si_signo:=sig;

  regs^.tf_rcx:=QWORD(ksi^.ksi_info.si_addr);
 end else
 begin
  regs^.tf_rsi:=ksi^.ksi_info.si_code;
  regs^.tf_rcx:=QWORD(ksi^.ksi_info.si_addr);
  sf.sf_ahu:=Pointer(catcher);
 end;

 ps_mtx_unlock;
 PROC_UNLOCK;

 if (copyout(@sf,sfp,sizeof(sigframe))<>0) then
 begin
  PROC_LOCK;
  sigexit(td,SIGILL);
 end;

 //if  (xfpusave != NULL && copyout(xfpusave,
 //    (void *)sf.sf_uc.uc_mcontext.mc_xfpustate, xfpusave_len)
 //    != 0)) {
 // PROC_LOCK(p);
 // sigexit(td, SIGILL);
 //}

 regs^.tf_rsp:=QWORD(sfp);
 regs^.tf_rip:=QWORD(@sigcode);
 regs^.tf_rflags:=regs^.tf_rflags and (not (PSL_T or PSL_D));

 set_pcb_flags(td,PCB_FULL_IRET);
 PROC_LOCK;
 ps_mtx_lock;
end;

function sys_sigreturn(sigcntxp:p_ucontext_t):Integer;
begin
 //TODO
 writeln;
end;


end.

