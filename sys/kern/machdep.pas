unit machdep;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 signal,
 signalvar,
 ucontext,
 kern_thr;

const
 _ucodesel=(8 shl 3) or 3;
 _udatasel=(7 shl 3) or 3;
 _ufssel  =(2 shl 3) or 3;
 _ugssel  =(3 shl 3) or 3;

procedure bzero(ptr:Pointer;size:ptrint);
Procedure bmove(src,dst:Pointer;size:ptrint);

function  cpu_getstack(td:p_kthread):QWORD;
procedure cpu_set_user_tls(td:p_kthread;base:Pointer);
procedure cpu_set_syscall_retval(td:p_kthread;error:Integer);
procedure cpu_set_upcall_kse(td:p_kthread;entry,arg:Pointer;stack:p_stack_t);

function  get_mcontext(td:p_kthread;mcp:p_mcontext_t;flags:Integer):Integer;
function  get_mcontext2(td:p_kthread;mcp:p_mcontext_t;flags:Integer):Integer;
function  set_mcontext(td:p_kthread;mcp:p_mcontext_t):Integer;

procedure sendsig(catcher:sig_t;ksi:p_ksiginfo;mask:p_sigset_t);
function  sys_sigreturn(sigcntxp:p_ucontext_t):Integer;

implementation

uses
 errno,
 systm,
 kern_psl,
 kern_sig,
 trap,
 md_context;

//clearing memory without AVX optimizations
procedure bzero(ptr:Pointer;size:ptrint);
begin
 while (size>0) do
 begin
  PByte(ptr)^:=0;
  Inc(ptr);
  Dec(size);
 end;
end;

//move memory without AVX optimizations
Procedure bmove(src,dst:Pointer;size:ptrint);
begin
 while (size>0) do
 begin
  PByte(dst)^:=PByte(src)^;
  Inc(src);
  Inc(dst);
  Dec(size);
 end;
end;

function cpu_getstack(td:p_kthread):QWORD;
begin
 Result:=td^.td_frame^.tf_rsp;
end;

procedure cpu_set_user_tls(td:p_kthread;base:Pointer);
begin
 td^.pcb_fsbase:=base;
 td^.td_teb^.tcb:=base;
 set_pcb_flags(td,PCB_FULL_IRET);
end;

procedure cpu_set_syscall_retval(td:p_kthread;error:Integer);
begin
 Case error of
  0:With td^.td_frame^ do
    begin
     tf_rax:=td^.td_retval[0];
     tf_rdx:=td^.td_retval[1];
     tf_rflags:=tf_rflags and (not PSL_C);
    end;
  ERESTART:
    With td^.td_frame^ do
    begin
     //tf_err = size of syscall cmd
     tf_rip:=tf_rip-td^.td_frame^.tf_err;
     tf_r10:=tf_rcx;
     set_pcb_flags(td,PCB_FULL_IRET);
    end;
  EJUSTRETURN:; //nothing
  else
    With td^.td_frame^ do
    begin
     tf_rax:=error;
     tf_rflags:=tf_rflags or PSL_C;
    end;
 end;
end;

procedure cpu_set_upcall_kse(td:p_kthread;entry,arg:Pointer;stack:p_stack_t);
begin
 {
  * Set the trap frame to point at the beginning of the uts
  * function.
  }
 td^.td_frame^.tf_rbp:=0;
 td^.td_frame^.tf_rsp:=(ptruint(stack^.ss_sp) + stack^.ss_size) and (not $F);
 Dec(td^.td_frame^.tf_rsp,8);
 td^.td_frame^.tf_rip:=ptruint(entry);
 td^.td_frame^.tf_ds:=_udatasel;
 td^.td_frame^.tf_es:=_udatasel;
 td^.td_frame^.tf_fs:=_ufssel;
 td^.td_frame^.tf_gs:=_ugssel;
 td^.td_frame^.tf_flags:=TF_HASSEGS;

 {
  * Pass the address of the mailbox for this kse to the uts
  * function as a parameter on the stack.
  }
 td^.td_frame^.tf_rdi:=ptruint(arg);
end;

function get_fpcontext(td:p_kthread;mcp:p_mcontext_t;xstate:Pointer):Integer;
begin
 Result:=0;

 mcp^.mc_flags   :=mcp^.mc_flags or _MC_HASFPXSTATE;
 mcp^.mc_fpformat:=_MC_FPFMT_XMM;
 mcp^.mc_ownedfp :=_MC_FPOWNED_FPU;

 bmove(@td^.td_fpstate,xstate,SizeOf(mcp^.mc_fpstate));
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
  bmove(xstate,@td^.td_fpstate,SizeOf(mcp^.mc_fpstate));
  td^.td_frame^.tf_flags:=td^.td_frame^.tf_flags or TF_HASFPXSTATE;
  Exit(0);
 end;

 Exit(EINVAL);
end;

function get_mcontext(td:p_kthread;mcp:p_mcontext_t;flags:Integer):Integer;
var
 tp:p_trapframe;
begin
 tp:=td^.td_frame;

 PROC_LOCK();
 mcp^.mc_onstack:=sigonstack(tp^.tf_rsp);
 PROC_UNLOCK();

 mcp^.mc_r15   :=tp^.tf_r15;
 mcp^.mc_r14   :=tp^.tf_r14;
 mcp^.mc_r13   :=tp^.tf_r13;
 mcp^.mc_r12   :=tp^.tf_r12;
 mcp^.mc_r11   :=tp^.tf_r11;
 mcp^.mc_r10   :=tp^.tf_r10;
 mcp^.mc_r9    :=tp^.tf_r9;
 mcp^.mc_r8    :=tp^.tf_r8;
 mcp^.mc_rdi   :=tp^.tf_rdi;
 mcp^.mc_rsi   :=tp^.tf_rsi;
 mcp^.mc_rbp   :=tp^.tf_rbp;
 mcp^.mc_rbx   :=tp^.tf_rbx;
 mcp^.mc_rcx   :=tp^.tf_rcx;
 mcp^.mc_rflags:=tp^.tf_rflags;

 if ((flags and GET_MC_CLEAR_RET)<>0) then
 begin
  mcp^.mc_rax:=0;
  mcp^.mc_rdx:=0;
  mcp^.mc_rflags:=tp^.tf_rflags and (not PSL_C);
 end else
 begin
  mcp^.mc_rax:=tp^.tf_rax;
  mcp^.mc_rdx:=tp^.tf_rdx;
 end;

 mcp^.mc_rip  :=tp^.tf_rip;
 mcp^.mc_cs   :=tp^.tf_cs;
 mcp^.mc_rsp  :=tp^.tf_rsp;
 mcp^.mc_ss   :=tp^.tf_ss;
 mcp^.mc_ds   :=tp^.tf_ds;
 mcp^.mc_es   :=tp^.tf_es;
 mcp^.mc_fs   :=tp^.tf_fs;
 mcp^.mc_gs   :=tp^.tf_gs;
 mcp^.mc_flags:=tp^.tf_flags;
 mcp^.mc_len  :=sizeof(mcontext_t);

 //xmm,ymm
 if ((tp^.tf_flags and TF_HASFPXSTATE)<>0) then
 begin
  Result:=get_fpcontext(td,mcp,@mcp^.mc_fpstate);
 end else
 begin
  Result:=md_get_fpcontext(td,mcp,@mcp^.mc_fpstate);
 end;
 if (Result<>0) then Exit;
 //xmm,ymm

 mcp^.mc_fsbase:=ptruint(td^.pcb_fsbase);
 mcp^.mc_gsbase:=ptruint(td^.pcb_gsbase);

 bzero(@mcp^.mc_spare,sizeof(mcp^.mc_spare));

 Result:=0;
end;

//sce ext

function get_mcontext2(td:p_kthread;mcp:p_mcontext_t;flags:Integer):Integer;
var
 tp:p_trapframe;
begin
 tp:=td^.td_frame;

 mcp^.mc_onstack:=sigonstack(tp^.tf_rsp);

 mcp^.mc_r15   :=tp^.tf_r15;
 mcp^.mc_r14   :=tp^.tf_r14;
 mcp^.mc_r13   :=tp^.tf_r13;
 mcp^.mc_r12   :=tp^.tf_r12;
 mcp^.mc_r11   :=tp^.tf_r11;
 mcp^.mc_r10   :=tp^.tf_r10;
 mcp^.mc_r9    :=tp^.tf_r9;
 mcp^.mc_r8    :=tp^.tf_r8;
 mcp^.mc_rdi   :=tp^.tf_rdi;
 mcp^.mc_rsi   :=tp^.tf_rsi;
 mcp^.mc_rbp   :=tp^.tf_rbp;
 mcp^.mc_rbx   :=tp^.tf_rbx;
 mcp^.mc_rcx   :=tp^.tf_rcx;
 mcp^.mc_rflags:=tp^.tf_rflags;
 mcp^.mc_rax   :=tp^.tf_rax;
 mcp^.mc_rdx   :=tp^.tf_rdx;
 mcp^.mc_rip   :=tp^.tf_rip;
 mcp^.mc_cs    :=tp^.tf_cs;
 mcp^.mc_rsp   :=tp^.tf_rsp;
 mcp^.mc_ss    :=tp^.tf_ss;
 mcp^.mc_ds    :=tp^.tf_ds;
 mcp^.mc_es    :=tp^.tf_es;
 mcp^.mc_fs    :=tp^.tf_fs;
 mcp^.mc_gs    :=tp^.tf_gs;
 mcp^.mc_flags :=tp^.tf_flags;
 mcp^.mc_len   :=sizeof(mcontext_t);

 //xmm,ymm
 get_fpcontext(td,mcp,@mcp^.mc_fpstate);
 //xmm,ymm

 mcp^.mc_fsbase:=ptruint(td^.pcb_fsbase);
 mcp^.mc_gsbase:=ptruint(td^.pcb_gsbase);

 bzero(@mcp^.mc_spare,sizeof(mcp^.mc_spare));

 Result:=0;
end;

//sce ext

function set_mcontext(td:p_kthread;mcp:p_mcontext_t):Integer;
var
 tp:p_trapframe;
 rflags:QWORD;
begin
 tp:=td^.td_frame;

 if (mcp^.mc_len<>sizeof(mcontext_t)) or
    ((mcp^.mc_flags and (not _MC_FLAG_MASK))<>0) then
 begin
  Exit(EINVAL);
 end;

 tp:=td^.td_frame;

 //xmm,ymm
 if ((mcp^.mc_rflags and _MC_HASFPXSTATE)<>0) then
 begin
  Result:=set_fpcontext(td,mcp,@mcp^.mc_fpstate);
  if (Result<>0) then Exit;
 end;
 //xmm,ymm

 rflags:=(mcp^.mc_rflags and PSL_USERCHANGE) or
         (tp^.tf_rflags and (not PSL_USERCHANGE));

 tp^.tf_r15   :=mcp^.mc_r15;
 tp^.tf_r14   :=mcp^.mc_r14;
 tp^.tf_r13   :=mcp^.mc_r13;
 tp^.tf_r12   :=mcp^.mc_r12;
 tp^.tf_r11   :=mcp^.mc_r11;
 tp^.tf_r10   :=mcp^.mc_r10;
 tp^.tf_r9    :=mcp^.mc_r9;
 tp^.tf_r8    :=mcp^.mc_r8;
 tp^.tf_rdi   :=mcp^.mc_rdi;
 tp^.tf_rsi   :=mcp^.mc_rsi;
 tp^.tf_rbp   :=mcp^.mc_rbp;
 tp^.tf_rbx   :=mcp^.mc_rbx;
 tp^.tf_rdx   :=mcp^.mc_rdx;
 tp^.tf_rcx   :=mcp^.mc_rcx;
 tp^.tf_rax   :=mcp^.mc_rax;
 tp^.tf_rip   :=mcp^.mc_rip;
 tp^.tf_rflags:=rflags;
 tp^.tf_rsp   :=mcp^.mc_rsp;
 tp^.tf_ss    :=mcp^.mc_ss;
 tp^.tf_flags :=mcp^.mc_flags;

 if ((tp^.tf_flags and TF_HASSEGS)<>0) then
 begin
  tp^.tf_ds:=mcp^.mc_ds;
  tp^.tf_es:=mcp^.mc_es;
  tp^.tf_fs:=mcp^.mc_fs;
  tp^.tf_gs:=mcp^.mc_gs;
 end;

 if ((mcp^.mc_flags and _MC_HASBASES)<>0) then
 begin
  td^.pcb_fsbase:=Pointer(mcp^.mc_fsbase);
  td^.pcb_gsbase:=Pointer(mcp^.mc_gsbase);
 end;

 set_pcb_flags(td,PCB_FULL_IRET);
 Result:=0;
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

 bmove(regs,@sf.sf_uc.uc_mcontext.mc_rdi,SizeOf(trapframe));

 sf.sf_uc.uc_mcontext.mc_len:=sizeof(mcontext_t);

 //xmm,ymm
 if ((regs^.tf_flags and TF_HASFPXSTATE)<>0) then
 begin
  get_fpcontext(td,@sf.sf_uc.uc_mcontext,@sf.sf_uc.uc_mcontext.mc_fpstate);

  //reset fpcontext usage
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
 end;
 //xmm,ymm

 bmove(@ucp^.uc_mcontext.mc_rdi,regs,sizeof(trapframe));

 if ((ucp^.uc_mcontext.mc_flags and _MC_HASBASES)<>0) then
 begin
  td^.pcb_fsbase:=Pointer(ucp^.uc_mcontext.mc_fsbase);
  td^.pcb_gsbase:=Pointer(ucp^.uc_mcontext.mc_gsbase);
 end;

 kern_sigprocmask(td,SIG_SETMASK,@ucp^.uc_sigmask,nil,0);

 set_pcb_flags(td,PCB_FULL_IRET);
 Result:=EJUSTRETURN;
end;


end.

