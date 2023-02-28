unit machdep;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 signal,
 signalvar,
 ucontext,
 kern_thread;

const
 _ucodesel=(8 shl 3) or 3;
 _udatasel=(7 shl 3) or 3;
 _ufssel  =(2 shl 3) or 3;
 _ugssel  =(3 shl 3) or 3;

procedure sendsig(catcher:sig_t;ksi:p_ksiginfo;mask:p_sigset_t);
function  sys_sigreturn(sigcntxp:p_ucontext_t):Integer;

implementation

uses
 errno,
 systm,
 md_psl,
 kern_sig,
 trap;

function get_fpcontext(td:p_kthread;mcp:p_mcontext_t;xstate:Pointer):Integer;
begin
 Result:=0;

 mcp^.mc_flags   :=mcp^.mc_flags or _MC_HASFPXSTATE;
 mcp^.mc_fpformat:=_MC_FPFMT_XMM;
 mcp^.mc_ownedfp :=_MC_FPOWNED_FPU;

 Move(td^.td_fpstate,xstate^,SizeOf(mcp^.mc_fpstate));
end;

function set_fpcontext(td:p_kthread;mcp:p_mcontext_t;xstate:Pointer):Integer;
begin
 Result:=0;

 if (mcp^.mc_fpformat=_MC_FPFMT_NODEV) then Exit(0);

 if (mcp^.mc_fpformat<>_MC_FPFMT_XMM) then Exit(EINVAL);

 if (mcp^.mc_ownedfp=_MC_FPOWNED_NONE) then Exit(0);

 if (mcp^.mc_ownedfp=_MC_FPOWNED_FPU) or
    (mcp^.mc_ownedfp=_MC_FPOWNED_PCB) then
 begin
  Move(xstate^,td^.td_fpstate,SizeOf(mcp^.mc_fpstate));
  Exit(0);
 end;

 Exit(EINVAL);
end;

procedure sendsig(catcher:sig_t;ksi:p_ksiginfo;mask:p_sigset_t);
var
 td:p_kthread;
 sf:sigframe;
 sfp:p_sigframe;
 regs:p_trapframe;
 sp:QWORD;
 sig,oonstack:Integer;
begin
 td:=curkthread;

 sig:=ksi^.ksi_info.si_signo;

 regs:=td^.td_frame;
 oonstack:=sigonstack(regs^.tf_rsp);

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

 //set segs
 regs^.tf_cs:=_ucodesel;
 regs^.tf_ds:=_udatasel;
 regs^.tf_ss:=_udatasel;
 regs^.tf_es:=_udatasel;
 regs^.tf_fs:=_ufssel;
 regs^.tf_gs:=_ugssel;
 regs^.tf_flags:=regs^.tf_flags or TF_HASSEGS;
 //set segs

 Move(regs^,sf.sf_uc.uc_mcontext.mc_rdi,SizeOf(trapframe));

 sf.sf_uc.uc_mcontext.mc_len:=sizeof(mcontext_t);

 //xmm,ymm
 if ((regs^.tf_flags and TF_HASFPXSTATE)<>0) then
 begin
  get_fpcontext(td,@sf.sf_uc.uc_mcontext,@sf.sf_uc.uc_mcontext.mc_fpstate);

  regs^.tf_flags:=regs^.tf_flags and (not TF_HASFPXSTATE);
 end;
 //xmm,ymm

 sf.sf_uc.uc_mcontext.mc_flags :=sf.sf_uc.uc_mcontext.mc_flags or _MC_HASBASES;
 sf.sf_uc.uc_mcontext.mc_fsbase:=QWORD(td^.pcb_fsbase);
 sf.sf_uc.uc_mcontext.mc_gsbase:=QWORD(td^.pcb_gsbase);

 if ((td^.td_pflags and TDP_ALTSTACK)<>0) and
    (oonstack=0) and
    SIGISMEMBER(@p_sigacts.ps_sigonstack,sig) then
 begin
  sp:=QWORD(td^.td_sigstk.ss_sp)+td^.td_sigstk.ss_size;
 end else
 begin
  sp:=regs^.tf_rsp-128;
 end;

 sp:=sp-sizeof(sigframe);

 sfp:=p_sigframe(sp and (not $1F));

 regs^.tf_rdi:=sig;
 regs^.tf_rdx:=QWORD(@sfp^.sf_uc);

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

 regs^.tf_rsp:=QWORD(sfp);
 regs^.tf_rip:=QWORD(@sigcode);
 regs^.tf_rflags:=regs^.tf_rflags and (not (PSL_T or PSL_D));

 set_pcb_flags(td,PCB_FULL_IRET);
 PROC_LOCK;
 ps_mtx_lock;
end;

function sys_sigreturn(sigcntxp:p_ucontext_t):Integer;
var
 td:p_kthread;
 uc:ucontext_t;
 regs:p_trapframe;
 ucp:p_ucontext_t;
begin
 td:=curkthread;

 Result:=copyin(sigcntxp,@uc,sizeof(ucontext_t));
 if (Result<>0) then Exit;

 ucp:=@uc;
 if ((ucp^.uc_mcontext.mc_flags and (not _MC_FLAG_MASK))<>0) then
 begin
  Exit(EINVAL);
 end;

 regs:=td^.td_frame;

 //xmm,ymm
 if ((uc.uc_mcontext.mc_flags and _MC_HASFPXSTATE)<>0) then
 begin
  Result:=set_fpcontext(td,@ucp^.uc_mcontext,@ucp^.uc_mcontext.mc_fpstate);
  if (Result<>0) then Exit;

  regs^.tf_flags:=regs^.tf_flags or TF_HASFPXSTATE;
 end;
 //xmm,ymm

 Move(ucp^.uc_mcontext.mc_rdi,regs^,sizeof(trapframe));

 //pcb^.pcb_fsbase := ucp^.uc_mcontext.mc_fsbase;
 //pcb^.pcb_gsbase := ucp^.uc_mcontext.mc_gsbase;

 kern_sigprocmask(td,SIG_SETMASK,@ucp^.uc_sigmask,nil,0);

 set_pcb_flags(td,PCB_FULL_IRET);
 Result:=EJUSTRETURN;
end;


end.

