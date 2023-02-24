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
 sys_kernel,
 kern_thread;

function  cpu_thread_alloc(td:p_kthread):Integer;
function  cpu_thread_free(td:p_kthread):Integer;
procedure cpu_set_syscall_retval(td:p_kthread;error:Integer);
function  cpuset_setaffinity(td:p_kthread;new:Ptruint):Integer;
procedure cpu_set_user_tls(td:p_kthread;base:Pointer);
function  cpu_set_priority(td:p_kthread;prio:Integer):Integer;
function  cpu_getstack(td:p_kthread):QWORD;

procedure ipi_sigreturn;
function  ipi_send_cpu(td:p_kthread):Integer;

implementation

uses
 systm,
 machdep,
 signal,
 kern_sig;

Const
 SYS_STACK_SIZE=16*1024;

function cpu_thread_alloc(td:p_kthread):Integer;
var
 data:Pointer;
 size:ULONG_PTR;
begin
 data:=nil;
 size:=SYS_STACK_SIZE;

 Result:=NtAllocateVirtualMemory(
           NtCurrentProcess,
           @data,
           0,
           @size,
           MEM_COMMIT or MEM_RESERVE,
           PAGE_READWRITE
          );

 data:=data+SYS_STACK_SIZE;
 td^.td_kstack:=data;
end;

function cpu_thread_free(td:p_kthread):Integer;
var
 data:Pointer;
 size:ULONG_PTR;
begin
 data:=td^.td_kstack;
 data:=data-SYS_STACK_SIZE;
 size:=0;

 Result:=NtFreeVirtualMemory(
           NtCurrentProcess,
           @data,
           @size,
           MEM_RELEASE
          );
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

function cpuset_setaffinity(td:p_kthread;new:Ptruint):Integer;
begin
 td^.td_cpuset:=new;
 Result:=NtSetInformationThread(td^.td_handle,ThreadAffinityMask,@new,SizeOf(Ptruint));
end;

procedure cpu_set_user_tls(td:p_kthread;base:Pointer); inline;
var
 ptls:PPointer;
begin
 td^.pcb_fsbase:=base;

 ptls:=td^.td_teb+$708;

 ptls^:=base;
end;

function cpu_get_iflag(td:p_kthread):PInteger; inline;
begin
 Result:=td^.td_teb+$710;
end;

function cpu_set_priority(td:p_kthread;prio:Integer):Integer;
begin
 td^.td_priority:=prio;

 Case prio of
    0..255:prio:= 16;
  256..496:prio:= 2;
  497..526:prio:= 1;
  527..556:prio:= 0;
  557..586:prio:=-1;
  587..767:prio:=-2;
  else
           prio:=-16;
 end;

 Result:=NtSetInformationThread(td^.td_handle,ThreadBasePriority,@prio,SizeOf(Integer));
end;

function cpu_getstack(td:p_kthread):QWORD; inline;
begin
 Result:=td^.td_frame^.tf_rsp;
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

procedure _apc_null(dwParam:PTRUINT); stdcall;
begin
end;

procedure ipi_sigreturn;
var
 _Context:array[0..SizeOf(TCONTEXT)+14] of Byte;
 Context :PCONTEXT;
 regs:p_trapframe;
begin
 regs:=curkthread^.td_frame;
 Context:=Align(@_Context,16);

 Context^:=Default(TCONTEXT);
 Context^.ContextFlags:=CONTEXT_INTEGER or CONTEXT_CONTROL;

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

 NtContinue(Context,False);
end;

function ipi_send_cpu(td:p_kthread):Integer;
label
 resume,
 tryagain;
var
 td_handle:THandle;
 iflag    :PInteger;
 _Context :array[0..SizeOf(TCONTEXT)+14] of Byte;
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
 Context  :=Align(@_Context,16);

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

 Context^:=Default(TCONTEXT);
 Context^.ContextFlags:=CONTEXT_INTEGER or CONTEXT_CONTROL;

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
  SwDelayExecution(False,@w); //100ms
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



