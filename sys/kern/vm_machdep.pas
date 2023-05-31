unit vm_machdep;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 windows,
 md_psl,
 ucontext,
 trap,
 kern_thr;

procedure ipi_sigreturn;
function  ipi_send_cpu(td:p_kthread):Integer;

function  _umtxq_alloc:THandle; inline;
procedure _umtxq_free(h:THandle); inline;
function  msleep_umtxq(h:THandle;timo:Int64):Integer; inline;
function  wakeup_umtxq(h:THandle):Integer; inline;

function  msleep_td(timo:Int64):Integer; inline;
function  wakeup_td(td:p_kthread):Integer; inline;
procedure md_yield; inline;

implementation

uses
 errno,
 systm,
 machdep,
 md_context,
 signal,
 kern_sig,
 md_thread;

function ntw2px(n:Integer):Integer; inline;
begin
 Case DWORD(n) of
  STATUS_SUCCESS         :Result:=0;
  STATUS_ABANDONED       :Result:=EPERM;
  STATUS_ALERTED         :Result:=EINTR;
  STATUS_USER_APC        :Result:=EINTR;
  STATUS_TIMEOUT         :Result:=ETIMEDOUT;
  STATUS_ACCESS_VIOLATION:Result:=EFAULT;
  else
                          Result:=EINVAL;
 end;
end;

function ntd2px(n:Integer):Integer; inline;
begin
 Case DWORD(n) of
  STATUS_SUCCESS         :Result:=ETIMEDOUT;
  STATUS_ABANDONED       :Result:=EPERM;
  STATUS_ALERTED         :Result:=EINTR;
  STATUS_USER_APC        :Result:=EINTR;
  STATUS_ACCESS_VIOLATION:Result:=EFAULT;
  else
                          Result:=EINVAL;
 end;
end;

function _umtxq_alloc:THandle; inline;
var
 n:Integer;
begin
 Result:=0;
 n:=NtCreateEvent(@Result,EVENT_ALL_ACCESS,nil,SynchronizationEvent,False);
 Assert(n=0);
end;

procedure _umtxq_free(h:THandle); inline;
begin
 NtClose(h);
end;

function msleep_umtxq(h:THandle;timo:Int64):Integer; inline;
begin
 if (timo=0) then
 begin
  timo:=NT_INFINITE;
 end else
 begin
  timo:=-timo;
 end;
 sig_sta;
 Result:=ntw2px(NtWaitForSingleObject(h,True,@timo));
 sig_cla;
end;

function wakeup_umtxq(h:THandle):Integer; inline;
begin
 Result:=ntw2px(NtSetEvent(h,nil));
end;

function msleep_td(timo:Int64):Integer; inline;
begin
 if (timo=0) then
 begin
  timo:=NT_INFINITE;
 end else
 begin
  timo:=-timo;
 end;
 sig_sta;
 Result:=ntd2px(NtDelayExecution(True,@timo));
 sig_cla;
end;

procedure _apc_null(dwParam:PTRUINT); stdcall;
begin
end;

function wakeup_td(td:p_kthread):Integer; inline;
begin
 Result:=ntw2px(NtQueueApcThread(td^.td_handle,@_apc_null,nil,nil,0));
end;

procedure md_yield; inline;
begin
 NtYieldExecution;
end;

function cpu_get_iflag(td:p_kthread):PInteger; inline;
begin
 Result:=@td^.td_teb^.iflag;
end;

function IS_SYSTEM_STACK(td:p_kthread;rsp:qword):Boolean; inline;
begin
 Result:=(rsp<=QWORD(td^.td_kstack)) and (rsp>(QWORD(td^.td_kstack)-SYS_STACK_SIZE));
end;

function IS_SYSCALL(rip:qword):Boolean;
var
 w:Word;
begin
 Result:=False;
 if (rip<>0) then
 begin
  w:=0;
  NtReadVirtualMemory(NtCurrentProcess,@PWord(Rip)[-1],@w,SizeOf(Word),nil);
  Result:=(w=$050F);
 end;
end;

function alloca(size:qword):Pointer; sysv_abi_default; assembler; nostackframe;
asm
 movqq       %rsp,%rax
 subq        %rdi,%rax
 lea     -8(%rax),%rax
 andq        $-32,%rax
 movqq     (%rsp),%rdi
 movqq       %rax,%rsp
 lea    -32(%rsp),%rsp
 jmp    %rdi
end;

procedure ipi_sigreturn;
var
 td:p_kthread;
 Context:PCONTEXT;
 regs:p_trapframe;
begin
 td:=curkthread;
 regs:=td^.td_frame;

 if ((regs^.tf_flags and TF_HASFPXSTATE)<>0) then
 begin
  //xmm,ymm
  Context:=alloca(GetContextSize(CONTEXT_ALLX));
  Context:=InitializeContextExtended(Context,CONTEXT_ALLX);
 end else
 begin
  //simple
  Context:=alloca(SizeOf(TCONTEXT)+15);
  Context^:=Default(TCONTEXT);
  Context^.ContextFlags:=CONTEXT_INTEGER or CONTEXT_CONTROL;
 end;

 Context^.Rdi:=regs^.tf_rdi;
 Context^.Rsi:=regs^.tf_rsi;
 Context^.Rdx:=regs^.tf_rdx;
 Context^.Rcx:=regs^.tf_rcx;
 Context^.R8 :=regs^.tf_r8 ;
 Context^.R9 :=regs^.tf_r9 ;
 Context^.Rax:=regs^.tf_rax;
 Context^.Rbx:=regs^.tf_rbx;
 Context^.Rbp:=regs^.tf_rbp;
 Context^.R10:=regs^.tf_r10;
 Context^.R11:=regs^.tf_r11;
 Context^.R12:=regs^.tf_r12;
 Context^.R13:=regs^.tf_r13;
 Context^.R14:=regs^.tf_r14;
 Context^.R15:=regs^.tf_r15;

 Context^.Rip   :=regs^.tf_rip;
 Context^.EFlags:=regs^.tf_rflags;
 Context^.Rsp   :=regs^.tf_rsp;

 Context^.SegGs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegEs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegDs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegCs:=KGDT64_R3_CODE  or RPL_MASK;
 Context^.SegSs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegFs:=KGDT64_R3_CMTEB or RPL_MASK;

 //xmm,ymm
 if ((regs^.tf_flags and TF_HASFPXSTATE)<>0) then
 begin
  _set_fpcontext(Context,@td^.td_fpstate);

  regs^.tf_flags:=regs^.tf_flags and (not TF_HASFPXSTATE);
 end;
 //xmm,ymm

 NtContinue(Context,False);
end;

function ipi_send_cpu(td:p_kthread):Integer;
label
 resume,
 tryagain;
var
 td_handle:THandle;
 iflag    :PInteger;
 Context  :PCONTEXT;
 w:LARGE_INTEGER;
 sf:sigframe;
 sfp:p_sigframe;
 regs:p_trapframe;
 sp:QWORD;
 oonstack:Integer;
begin
 Result   :=0;
 td_handle:=td^.td_handle;
 iflag    :=cpu_get_iflag(td);

 PROC_LOCK;

 tryagain:

 if (NtSuspendThread(td_handle,nil)<>STATUS_SUCCESS) then
 begin
  PROC_UNLOCK;
  Exit(ESRCH);
 end;

 w.QuadPart:=0;
 if (NtWaitForSingleObject(td_handle,False,@w)<>STATUS_TIMEOUT) then
 begin
  Result:=ESRCH;
  goto resume;
 end;

 if ((iflag^ and SIG_ALTERABLE)<>0) then //alterable?
 begin
  NtQueueApcThread(td_handle,@_apc_null,nil,nil,0);
  Result:=0;
  goto resume;
 end else
 if (iflag^<>0) then //locked?
 begin
  Result:=0;
  goto resume;
 end;

 Context:=alloca(GetContextSize(CONTEXT_ALLX));

 Context:=InitializeContextExtended(Context,CONTEXT_ALLX);

 if (NtGetContextThread(td_handle,Context)<>STATUS_SUCCESS) then
 begin
  Result:=ESRCH;
  goto resume;
 end;

 if IS_SYSTEM_STACK(td,Context^.Rsp) then //system?
 begin
  Result:=0;
  goto resume;
 end;

 if IS_SYSCALL(Context^.Rip) then //system call in code without blocking
 begin
  NtResumeThread(td_handle,nil);
  w.QuadPart:=-10000;
  NtDelayExecution(False,@w); //100ms
  goto tryagain;
 end;

 regs:=td^.td_frame;
 oonstack:=sigonstack(Context^.Rsp);

 // Save user context.
 sf:=Default(sigframe);

 sf.sf_uc.uc_sigmask:=td^.td_sigmask;
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

 //copy frame
 regs^.tf_rdi:=Context^.Rdi;
 regs^.tf_rsi:=Context^.Rsi;
 regs^.tf_rdx:=Context^.Rdx;
 regs^.tf_rcx:=Context^.Rcx;
 regs^.tf_r8 :=Context^.R8 ;
 regs^.tf_r9 :=Context^.R9 ;
 regs^.tf_rax:=Context^.Rax;
 regs^.tf_rbx:=Context^.Rbx;
 regs^.tf_rbp:=Context^.Rbp;
 regs^.tf_r10:=Context^.R10;
 regs^.tf_r11:=Context^.R11;
 regs^.tf_r12:=Context^.R12;
 regs^.tf_r13:=Context^.R13;
 regs^.tf_r14:=Context^.R14;
 regs^.tf_r15:=Context^.R15;

 regs^.tf_rip   :=Context^.Rip;
 regs^.tf_rflags:=Context^.EFlags;
 regs^.tf_rsp   :=Context^.Rsp;

 regs^.tf_cs:=_ucodesel;
 regs^.tf_ds:=_udatasel;
 regs^.tf_ss:=_udatasel;
 regs^.tf_es:=_udatasel;
 regs^.tf_fs:=_ufssel;
 regs^.tf_gs:=_ugssel;
 regs^.tf_flags:=TF_HASSEGS;
 //copy frame

 sf.sf_uc.uc_mcontext.mc_onstack:=oonstack;

 Move(regs^,sf.sf_uc.uc_mcontext.mc_rdi,SizeOf(trapframe));

 sf.sf_uc.uc_mcontext.mc_len:=sizeof(mcontext_t);

 sf.sf_uc.uc_mcontext.mc_fsbase:=QWORD(td^.pcb_fsbase);
 sf.sf_uc.uc_mcontext.mc_gsbase:=QWORD(td^.pcb_gsbase);

 sf.sf_uc.uc_mcontext.mc_flags:=_MC_HASSEGS or _MC_HASBASES or _MC_HASFPXSTATE;

 //xmm,ymm
 _get_fpcontext(Context,@sf.sf_uc.uc_mcontext.mc_fpstate);

 sf.sf_uc.uc_mcontext.mc_fpformat:=_MC_FPFMT_XMM;
 sf.sf_uc.uc_mcontext.mc_ownedfp :=_MC_FPOWNED_FPU;
 //xmm,ymm

 sp:=QWORD(td^.td_kstack);

 sp:=sp-sizeof(sigframe);

 sfp:=p_sigframe(sp and (not $1F));

 if (copyout(@sf,sfp,sizeof(sigframe))<>0) then
 begin
  Result:=EFAULT;
  goto resume;
 end;

 Context^.Rsp:=QWORD(sfp);
 Context^.Rip:=QWORD(@sigipi);
 Context^.EFlags:=Context^.EFlags and (not (PSL_T or PSL_D));

 set_pcb_flags(td,PCB_FULL_IRET);

 if (NtSetContextThread(td_handle,Context)<>STATUS_SUCCESS) then
 begin
  Result:=ESRCH;
  goto resume;
 end;

 resume:
  NtResumeThread(td_handle,nil);
  PROC_UNLOCK;
end;

end.



