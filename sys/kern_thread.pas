unit kern_thread;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 windows,
 sys_kernel,
 ucontext,
 signal,
 signalvar,
 hamt;

const
 TDS_INACTIVE=0;
 TDS_RUNNING =1;

 TDF_BORROWING  =$00000001; // Thread is borrowing pri from another.
 TDF_INPANIC    =$00000002; // Caused a panic, let it drive crashdump.
 TDF_INMEM      =$00000004; // Thread's stack is in memory.
 TDF_SINTR      =$00000008; // Sleep is interruptible.
 TDF_TIMEOUT    =$00000010; // Timing out during sleep.
 TDF_IDLETD     =$00000020; // This is a per-CPU idle thread.
 TDF_CANSWAP    =$00000040; // Thread can be swapped.
 TDF_SLEEPABORT =$00000080; // sleepq_abort was called.
 TDF_KTH_SUSP   =$00000100; // kthread is suspended
 TDF_UNUSED09   =$00000200; // --available--
 TDF_BOUNDARY   =$00000400; // Thread suspended at user boundary
 TDF_ASTPENDING =$00000800; // Thread has some asynchronous events.
 TDF_TIMOFAIL   =$00001000; // Timeout from sleep after we were awake.
 TDF_SBDRY      =$00002000; // Stop only on usermode boundary.
 TDF_UPIBLOCKED =$00004000; // Thread blocked on user PI mutex.
 TDF_NEEDSUSPCHK=$00008000; // Thread may need to suspend.
 TDF_NEEDRESCHED=$00010000; // Thread needs to yield.
 TDF_NEEDSIGCHK =$00020000; // Thread may need signal delivery.
 TDF_NOLOAD     =$00040000; // Ignore during load avg calculations.
 TDF_UNUSED19   =$00080000; // --available--
 TDF_THRWAKEUP  =$00100000; // Libthr thread must not suspend itself.
 TDF_UNUSED21   =$00200000; // --available--
 TDF_SWAPINREQ  =$00400000; // Swapin request due to wakeup.
 TDF_UNUSED23   =$00800000; // --available--
 TDF_SCHED0     =$01000000; // Reserved for scheduler private use
 TDF_SCHED1     =$02000000; // Reserved for scheduler private use
 TDF_SCHED2     =$04000000; // Reserved for scheduler private use
 TDF_SCHED3     =$08000000; // Reserved for scheduler private use
 TDF_ALRMPEND   =$10000000; // Pending SIGVTALRM needs to be posted.
 TDF_PROFPEND   =$20000000; // Pending SIGPROF needs to be posted.
 TDF_MACPEND    =$40000000; // AST-based MAC event pending.

 //

 TDP_OLDMASK      =$00000001; // Need to restore mask after suspend.
 TDP_INKTR        =$00000002; // Thread is currently in KTR code.
 TDP_INKTRACE     =$00000004; // Thread is currently in KTRACE code.
 TDP_BUFNEED      =$00000008; // Do not recurse into the buf flush
 TDP_COWINPROGRESS=$00000010; // Snapshot copy-on-write in progress.
 TDP_ALTSTACK     =$00000020; // Have alternate signal stack.
 TDP_DEADLKTREAT  =$00000040; // Lock aquisition - deadlock treatment.
 TDP_NOFAULTING   =$00000080; // Do not handle page faults.
 TDP_NOSLEEPING   =$00000100; // Thread is not allowed to sleep on a sq.
 TDP_OWEUPC       =$00000200; // Call addupc() at next AST.
 TDP_ITHREAD      =$00000400; // Thread is an interrupt thread.
 TDP_SYNCIO       =$00000800; // Local override, disable async i/o.
 TDP_SCHED1       =$00001000; // Reserved for scheduler private use
 TDP_SCHED2       =$00002000; // Reserved for scheduler private use
 TDP_SCHED3       =$00004000; // Reserved for scheduler private use
 TDP_SCHED4       =$00008000; // Reserved for scheduler private use
 TDP_GEOM         =$00010000; // Settle GEOM before finishing syscall
 TDP_SOFTDEP      =$00020000; // Stuck processing softdep worklist
 TDP_NORUNNINGBUF =$00040000; // Ignore runningbufspace check
 TDP_WAKEUP       =$00080000; // Don't sleep in umtx cond_wait
 TDP_INBDFLUSH    =$00100000; // Already in BO_BDFLUSH, do not recurse
 TDP_KTHREAD      =$00200000; // This is an official kernel thread
 TDP_CALLCHAIN    =$00400000; // Capture thread's callchain
 TDP_IGNSUSP      =$00800000; // Permission to ignore the MNTK_SUSPEND*
 TDP_AUDITREC     =$01000000; // Audit record pending on thread
 TDP_RFPPWAIT     =$02000000; // Handle RFPPWAIT on syscall exit
 TDP_RESETSPUR    =$04000000; // Reset spurious page fault history.
 TDP_NERRNO       =$08000000; // Last errno is already in td_errno
 TDP_UIOHELD      =$10000000; // Current uio has pages held in td_ma
 TDP_DEVMEMIO     =$20000000; // Accessing memory for /dev/mem
 TDP_EXECVMSPC    =$40000000; // Execve destroyed old vmspace

 //

 PRI_ITHD     =1; // Interrupt thread.
 PRI_REALTIME =2; // Real time process.
 PRI_TIMESHARE=3; // Time sharing process.
 PRI_IDLE     =4; // Idle process.
 PRI_FIFO     =10;

 PRI_MIN=0;
 PRI_MAX=960;

 PRI_MIN_TIMESHARE=256;
 PRI_MAX_TIMESHARE=767;

 PUSER=PRI_MIN_TIMESHARE;

 RTP_PRIO_REALTIME =PRI_REALTIME;
 RTP_PRIO_NORMAL   =PRI_TIMESHARE;
 RTP_PRIO_IDLE     =PRI_IDLE;
 RTP_PRIO_FIFO     =PRI_FIFO;

 THR_SUSPENDED=$0001;

type
 p_kthread=^kthread;
 kthread=packed record
  td_umtxq        :Pointer; //p_umtx_q
  td_handle       :THandle; //nt thread
  td_teb          :Pointer;
  td_lock         :Pointer;
  td_tid          :QWORD;
  td_sigstk       :stack_t;
  td_state        :Integer;
  td_pflags       :Integer;
  td_flags        :Integer;
  td_errno        :Integer;
  pcb_flags       :Integer;
  td_ref          :Integer;
  td_priority     :Word;
  td_pri_class    :Word;
  td_base_pri     :Word;
  td_base_user_pri:Word;
  td_lend_user_pri:Word;
  td_user_pri     :Word;
  td_name         :array[0..31] of AnsiChar;
  //
  td_cpuset       :Ptruint;
  td_sigmask      :sigset_t;
  td_oldsigmask   :sigset_t;
  td_sigqueue     :sigqueue_t;
  td_frame        :p_trapframe;
  td_retval       :array[0..1] of QWORD;
  td_kstack       :Pointer;
 end;

 p_rtprio=^rtprio;
 rtprio=packed record
  _type:Word;
  _prio:Word;
 end;

 p_thr_param=^thr_param;
 thr_param=packed record
  start_func:Pointer;
  arg       :Pointer;
  stack_base:Pointer;
  stack_size:Ptruint;
  tls_base  :Pointer;
  tls_size  :Ptruint;
  child_tid :PQWORD;
  parent_tid:PQWORD;
  flags     :Integer;
  align     :Integer;
  rtp       :p_rtprio;
  name      :PChar;
  spare     :array[0..1] of Pointer;
 end;

procedure sched_fork_thread(td,childtd:p_kthread);
procedure sched_class(td:p_kthread;_class:Integer);
function  sched_priority(td:p_kthread;prio:Integer):Integer;
procedure sched_prio(td:p_kthread;prio:Integer);
procedure sched_user_prio(td:p_kthread;prio:Integer);
procedure sched_lend_user_prio(td:p_kthread;prio:Integer);

function  rtp_to_pri(rtp:p_rtprio;td:p_kthread):Integer;
procedure pri_to_rtp(td:p_kthread;rtp:p_rtprio);

function  thread_alloc:p_kthread;
procedure thread_free(td:p_kthread);

function  sys_thr_new(_param:p_thr_param;_size:Integer):Integer;
function  sys_thr_self(id:PQWORD):Integer;
procedure sys_thr_exit(state:PQWORD);
function  sys_thr_kill(id:QWORD;sig:Integer):Integer;

procedure thread_inc_ref(td:p_kthread);
procedure thread_dec_ref(td:p_kthread);
procedure thread_lock(td:p_kthread);
procedure thread_unlock(td:p_kthread);
function  tdfind(tid:DWORD):p_kthread;
procedure FOREACH_THREAD_IN_PROC(cb,userdata:Pointer);

function  curkthread:p_kthread;
procedure set_curkthread(td:p_kthread);

function  SIGPENDING(td:p_kthread):Boolean;
function  TD_IS_RUNNING(td:p_kthread):Boolean; inline;

procedure PROC_LOCK;
procedure PROC_UNLOCK;

implementation

{
64 48 A1 [0000000000000000] mov rax,fs:[$0000000000000000] -> 65 48 A1 [0807000000000000] mov rax,gs:[$0000000000000708]
64 48 8B 04 25 [00000000]   mov rax,fs:[$00000000]         -> 65 48 8B 04 25 [08070000]   mov rax,gs:[$00000708]
64 48 8B 0C 25 [00000000]   mov rcx,fs:[$00000000]         -> 65 48 8B 0C 25 [08070000]   mov rcx,gs:[$00000708]
64 48 8B 14 25 [00000000]   mov rdx,fs:[$00000000]         -> 65 48 8B 14 25 [08070000]   mov rdx,gs:[$00000708]
64 48 8B 1C 25 [00000000]   mov rbx,fs:[$00000000]         -> 65 48 8B 1C 25 [08070000]   mov rbx,gs:[$00000708]
64 48 8B 24 25 [00000000]   mov rsp,fs:[$00000000]         -> 65 48 8B 24 25 [08070000]   mov rsp,gs:[$00000708]
64 48 8B 2C 25 [00000000]   mov rbp,fs:[$00000000]         -> 65 48 8B 2C 25 [08070000]   mov rbp,gs:[$00000708]
64 48 8B 34 25 [00000000]   mov rsi,fs:[$00000000]         -> 65 48 8B 34 25 [08070000]   mov rsi,gs:[$00000708]
64 48 8B 3C 25 [00000000]   mov rdi,fs:[$00000000]         -> 65 48 8B 3C 25 [08070000]   mov rdi,gs:[$00000708]
64 4C 8B 04 25 [00000000]   mov r8 ,fs:[$00000000]         -> 65 4C 8B 04 25 [08070000]   mov r8 ,gs:[$00000708]
64 4C 8B 0C 25 [00000000]   mov r9 ,fs:[$00000000]         -> 65 4C 8B 0C 25 [08070000]   mov r9 ,gs:[$00000708]
64 4C 8B 14 25 [00000000]   mov r10,fs:[$00000000]         -> 65 4C 8B 14 25 [08070000]   mov r10,gs:[$00000708]
64 4C 8B 1C 25 [00000000]   mov r11,fs:[$00000000]         -> 65 4C 8B 1C 25 [08070000]   mov r11,gs:[$00000708]
64 4C 8B 24 25 [00000000]   mov r12,fs:[$00000000]         -> 65 4C 8B 24 25 [08070000]   mov r12,gs:[$00000708]
64 4C 8B 2C 25 [00000000]   mov r13,fs:[$00000000]         -> 65 4C 8B 2C 25 [08070000]   mov r13,gs:[$00000708]
64 4C 8B 34 25 [00000000]   mov r14,fs:[$00000000]         -> 65 4C 8B 34 25 [08070000]   mov r14,gs:[$00000708]
64 4C 8B 3C 25 [00000000]   mov r15,fs:[$00000000]         -> 65 4C 8B 3C 25 [08070000]   mov r15,gs:[$00000708]
}

uses
 gtailq,
 systm,
 vm_machdep,
 kern_rwlock,
 kern_mtx,
 kern_umtx,
 kern_sig;

var
 p_mtx:Pointer=nil;

 tidhashtbl:TSTUB_HAMT32;
 tidhash_lock:Pointer=nil;

 p_numthreads:Integer=0;

 const
  max_threads_per_proc=1500;

function curkthread:p_kthread; assembler; nostackframe;
asm
 movqq %gs:(0x700),Result
end;

procedure set_curkthread(td:p_kthread); assembler; nostackframe;
asm
 movqq td,%gs:(0x700)
end;

function SIGPENDING(td:p_kthread):Boolean;
begin
 Result:=SIGNOTEMPTY(@td^.td_sigqueue.sq_signals) and
         sigsetmasked(@td^.td_sigqueue.sq_signals,@td^.td_sigmask);
end;

function TD_IS_RUNNING(td:p_kthread):Boolean; inline;
begin
 Result:=td^.td_state=TDS_RUNNING
end;

procedure PROC_LOCK;
begin
 mtx_lock(@p_mtx);
end;

procedure PROC_UNLOCK;
begin
 mtx_lock(@p_mtx);
end;

procedure threadinit; inline;
begin
 mtx_init(@p_mtx);
 FillChar(tidhashtbl,SizeOf(tidhashtbl),0);
end;

function thread_alloc:p_kthread;
var
 data:Pointer;
begin
 data:=AllocMem(SizeOf(kthread)+SizeOf(trapframe));

 Result:=data;

 data:=data+SizeOf(kthread);
 Result^.td_frame:=data;

 cpu_thread_alloc(Result);

 Result^.td_state:=TDS_INACTIVE;
 Result^.td_lend_user_pri:=PRI_MAX;

 //
 umtx_thread_init(Result);
end;

procedure thread_free(td:p_kthread);

begin
 umtx_thread_fini(td);
 //
 cpu_thread_free(td);
 //
 FreeMem(td);
end;

procedure thread_inc_ref(td:p_kthread);
begin
 System.InterlockedIncrement(td^.td_ref);
end;

procedure thread_dec_ref(td:p_kthread);
begin
 if (System.InterlockedDecrement(td^.td_ref)=0) then
 begin
  thread_free(td);
 end;
end;

procedure thread_lock(td:p_kthread);
begin
 rw_wlock(td^.td_lock);
end;

procedure thread_unlock(td:p_kthread);
begin
 rw_wunlock(td^.td_lock);
end;

procedure thread_link(td:p_kthread);
begin
 td^.td_state:=TDS_INACTIVE;
 td^.td_flags:=TDF_INMEM;

 sigqueue_init(@td^.td_sigqueue);

 System.InterlockedIncrement(p_numthreads);
end;

procedure thread_unlink(td:p_kthread);
begin
 System.InterlockedDecrement(p_numthreads)
end;

function tdfind(tid:DWORD):p_kthread;
Var
 data:PPointer;
begin
 Result:=nil;
 rw_rlock(tidhash_lock);

 data:=HAMT_search32(@tidhashtbl,tid);

 if (data<>nil) then
 begin
  Result:=data^;
 end;

 if (Result<>nil) then
 begin
  thread_inc_ref(Result);
 end;

 rw_runlock(tidhash_lock);
end;

procedure tidhash_add(td:p_kthread);
var
 data:PPointer;
begin
 rw_wlock(tidhash_lock);

 data:=HAMT_insert32(@tidhashtbl,td^.td_tid,td);

 if (data<>nil) then
 begin
  if (data^=td) then
  begin
   thread_inc_ref(td);
  end;
 end;

 rw_wunlock(tidhash_lock);
end;

procedure tidhash_remove(td:p_kthread);
var
 data:Pointer;
begin
 rw_wlock(tidhash_lock);

 data:=HAMT_delete32(@tidhashtbl,td^.td_tid);

 rw_wunlock(tidhash_lock);

 if (data=td) then
 begin
  thread_dec_ref(td);
 end;
end;

procedure FOREACH_THREAD_IN_PROC(cb,userdata:Pointer);
begin
 rw_wlock(tidhash_lock);

 HAMT_traverse32(@tidhashtbl,Tfree_data_cb(cb),userdata);

 rw_wunlock(tidhash_lock);
end;

function BaseQueryInfo(td:p_kthread):Integer;
var
 TBI:THREAD_BASIC_INFORMATION;
 pcur:PPointer;
begin
 TBI:=Default(THREAD_BASIC_INFORMATION);

 Result:=NtQueryInformationThread(
           td^.td_handle,
           ThreadBasicInformation,
           @TBI,
           SizeOf(THREAD_BASIC_INFORMATION),
           nil);
 if (Result<>0) then Exit;

 td^.td_teb   :=TBI.TebBaseAddress;
 td^.td_cpuset:=TBI.AffinityMask;

 pcur:=td^.td_teb+$700; //self

 pcur^:=td;
end;

procedure BaseInitializeStack(InitialTeb  :PINITIAL_TEB;
                              StackAddress:Pointer;
                              StackSize   :Ptruint); inline;
begin
 InitialTeb^.PreviousStackBase :=nil;
 InitialTeb^.PreviousStackLimit:=nil;
 InitialTeb^.StackBase         :=StackAddress+StackSize;  //start addr
 InitialTeb^.StackLimit        :=StackAddress;            //lo addr
 InitialTeb^.AllocatedStackBase:=StackAddress;            //lo addr
end;

procedure BaseInitializeContext(Context     :PCONTEXT;
                                Parameter   :Pointer;
                                StartAddress:Pointer;
                                StackAddress:Pointer); inline;
begin
 Context^:=Default(TCONTEXT);

 Context^.Rsp:=ptruint(StackAddress);
 Context^.Rbp:=ptruint(StackAddress);
 Context^.Rdi:=ptruint(Parameter);
 Context^.Rip:=ptruint(StartAddress);

 Context^.SegGs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegEs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegDs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegCs:=KGDT64_R3_CODE  or RPL_MASK;
 Context^.SegSs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegFs:=KGDT64_R3_CMTEB or RPL_MASK;

 Context^.EFlags:=$3000 or EFLAGS_INTERRUPT_MASK;

 Context^.MxCsr:=INITIAL_MXCSR;

 Context^.ContextFlags:=CONTEXT_THREAD;
end;

function create_thread(td        :p_kthread; //calling thread
                       ctx       :Pointer;
                       start_func:Pointer;
                       arg       :Pointer;
                       stack_base:Pointer;
                       stack_size:QWORD;
                       tls_base  :Pointer;
                       child_tid :PQWORD;
                       parent_tid:PQWORD;
                       rtp       :p_rtprio;
                       name      :PChar
                      ):Integer;
label
 _term;
var
 newtd:p_kthread;

 _ClientId  :array[0..SizeOf(TCLIENT_ID  )+14] of Byte;
 _InitialTeb:array[0..SizeOf(TINITIAL_TEB)+14] of Byte;
 _Context   :array[0..SizeOf(TCONTEXT    )+14] of Byte;

 ClientId  :PCLIENT_ID;
 InitialTeb:PINITIAL_TEB;
 Context   :PCONTEXT;

 Stack:Pointer;

 n:Integer;
begin

 if (p_numthreads>=max_threads_per_proc) then
 begin
  Exit(EPROCLIM);
 end;

 if (rtp<>nil) then
 begin
  Case (rtp^._type and $fff7) of //RTP_PRIO_BASE
   RTP_PRIO_IDLE:
     begin
      if (rtp^._prio<>960) then Exit(EINVAL);
     end;
   RTP_PRIO_NORMAL:
     begin
      if (rtp^._prio>959) then Exit(EINVAL);
     end;
   PRI_REALTIME:
     begin
      if (rtp^._prio>767) then Exit(EINVAL);
     end;
   else
     Exit(EINVAL)
  end;
 end;

 if (ctx<>nil) then Exit(EINVAL);

 if (ptruint(stack_base)<$1000) or (stack_size<$1000) then Exit(EINVAL);

 newtd:=thread_alloc;
 if (newtd=nil) then Exit(ENOMEM);

 ClientId  :=Align(@_ClientId  ,16);
 InitialTeb:=Align(@_InitialTeb,16);
 Context   :=Align(@_Context   ,16);

 ClientId^.UniqueProcess:=NtCurrentProcess;
 ClientId^.UniqueThread :=NtCurrentThread;

 BaseInitializeStack(InitialTeb,stack_base,stack_size);

 Stack:=InitialTeb^.StackBase;
 Stack:=Pointer((ptruint(Stack) and (not $F)){-Sizeof(Pointer)});

 BaseInitializeContext(Context,
                       arg,
                       start_func,
                       Stack);

 n:=NtCreateThread(
           @newtd^.td_handle,
           THREAD_ALL_ACCESS,
           nil,
           NtCurrentProcess,
           ClientId,
           Context,
           InitialTeb,
           True);

 if (n<>0) then
 begin
  thread_free(newtd);
  Exit(EINVAL);
 end;

 newtd^.td_tid:=DWORD(ClientId^.UniqueThread);

 if (BaseQueryInfo(newtd)<>0) then
 begin
  _term:
  NtTerminateThread(newtd^.td_handle,n);
  NtClose(newtd^.td_handle);

  thread_free(newtd);
  Exit(EFAULT);
 end;

 cpu_set_user_tls(newtd,tls_base);

 if (child_tid<>nil) then
 begin
  n:=suword64(child_tid^,newtd^.td_tid);
  if (n<>0) then Goto _term;
 end;

 if (parent_tid<>nil) then
 begin
  n:=suword64(parent_tid^,newtd^.td_tid);
  if (n<>0) then Goto _term;
 end;

 if (td<>nil) then
 begin
  newtd^.td_sigmask:=td^.td_sigmask;
 end;

 thread_link(newtd);

 if (name<>nil) then
 begin
  Move(name^,newtd^.td_name,SizeOf(newtd^.td_name));
 end;
 SetThreadDebugName(newtd^.td_handle,'ps4:'+newtd^.td_name);

 sched_fork_thread(td,newtd);

 tidhash_add(newtd);

 if (rtp<>nil) then
 begin
  if (td=nil) then
  begin
   rtp_to_pri(rtp,newtd);
   sched_prio(newtd,newtd^.td_user_pri);
  end else
  if (td^.td_pri_class<>PRI_TIMESHARE) then
  begin
   rtp_to_pri(rtp,newtd);
   sched_prio(newtd,newtd^.td_user_pri);
  end;
 end;

 newtd^.td_state:=TDS_RUNNING;
 NtResumeThread(newtd^.td_handle,nil);
end;

function kern_thr_new(td:p_kthread;param:p_thr_param):Integer;
var
 rtp:rtprio;
 rtpp:p_rtprio;
 name:array[0..31] of AnsiChar;
begin
 Result:=0;
 rtpp:=nil;

 if (param^.rtp<>nil) then
 begin
  Result:=copyin(param^.rtp,@rtp,Sizeof(rtprio));
  if (Result<>0) then Exit(EFAULT);
  rtpp:=@rtp;
 end;

 name[0]:=#0;

 if (param^.name<>nil) then
 begin
  Result:=copyinstr(param^.name,@name,32,nil);
  if (Result<>0) then Exit(EFAULT);
 end;

 Result:=create_thread(td,
                       nil,
                       param^.start_func,
                       param^.arg,
                       param^.stack_base,
                       param^.stack_size,
                       param^.tls_base,
                       param^.child_tid,
                       param^.parent_tid,
                       rtpp,
                       @name);
end;

function sys_thr_new(_param:p_thr_param;_size:Integer):Integer;
var
 param:thr_param;
begin
 if (_size<0) or (_size>Sizeof(thr_param)) then Exit(EINVAL);

 param:=Default(thr_param);

 Result:=copyin(_param,@param,_size);
 if (Result<>0) then Exit(EFAULT);

 Result:=kern_thr_new(curkthread,@param);
end;

procedure thread_exit;
var
 td:p_kthread;
 rsp:QWORD;
begin
 td:=curkthread;
 if (td=nil) then Exit;

 ASSERT(TAILQ_EMPTY(@td^.td_sigqueue.sq_list),'signal pending');

 td^.td_state:=TDS_INACTIVE;

 thread_inc_ref(td);
 tidhash_remove(td);
 thread_unlink(td);

 NtClose(td^.td_handle);

 umtx_thread_exit(td);

 //switch to userstack
 rsp:=td^.td_frame^.tf_rsp;
 if (rsp<>0) then
 asm
  mov rsp,%rsp
 end;

 //free
 thread_dec_ref(td);

 RtlExitUserThread(0);
end;

function sys_thr_self(id:PQWORD):Integer;
var
 td:p_kthread;
begin
 if (id=nil) then Exit(EINVAL);

 td:=curkthread;
 if (td=nil) then Exit(EFAULT);

 Result:=suword64(id^,td^.td_tid);
 if (Result<>0) then Exit(EFAULT);

 Result:=0;
end;

procedure sys_thr_exit(state:PQWORD);
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit;

 if (state<>nil) then
 begin
  kern_umtx_wake(td,Pointer(state),High(Integer),0);
 end;

 tdsigcleanup(td);
 //thread_stopped(p);
 thread_exit();
 // NOTREACHED
end;

type
 p_t_stk=^_t_stk;
 _t_stk=record
   error:Integer;
   sig:Integer;
   td:p_kthread;
   ksi:ksiginfo_t;
 end;

procedure _for_stk(td:p_kthread;data:p_t_stk); register; //Tfree_data_cb
begin
 if (td<>data^.td) then
 begin
  data^.error:=0;
  if (data^.sig=0) then Exit;
  tdksignal(td,data^.sig,@data^.ksi);
 end;
end;

function sys_thr_kill(id:QWORD;sig:Integer):Integer;
var
 data:_t_stk;
begin
 data.td:=curkthread;

 ksiginfo_init(@data.ksi);
 data.ksi.ksi_info.si_signo:=sig;
 data.ksi.ksi_info.si_code :=SI_LWP;

 if (int64(id)=-1) then
 begin
  if (sig<>0) and (not _SIG_VALID(sig)) then
  begin
   Result:=EINVAL;
  end else
  begin
   data.error:=ESRCH;
   data.sig:=0;
   PROC_LOCK;
   FOREACH_THREAD_IN_PROC(@_for_stk,@data);
   PROC_UNLOCK;
   Result:=data.error;
  end;
 end else
 begin
  Result:=0;

  data.td:=tdfind(DWORD(id));
  if (data.td=nil) then Exit(ESRCH);

  if (sig=0) then
  begin
   //
  end else
  if (not _SIG_VALID(sig)) then
   Result:=EINVAL
  else
   tdksignal(data.td,sig,@data.ksi);

  thread_dec_ref(data.td);

  PROC_UNLOCK;
 end;
end;


function rtp_to_pri(rtp:p_rtprio;td:p_kthread):Integer;
var
 newpri:Integer;
begin

 Case (rtp^._type and $fff7) of //RTP_PRIO_BASE
  RTP_PRIO_IDLE:
    begin
     newpri:=960;
     if (rtp^._prio<>960) then Exit(EINVAL);
    end;
  RTP_PRIO_NORMAL:
    begin
     newpri:=rtp^._prio;
     if (newpri>959) then Exit(EINVAL);
    end;
  PRI_REALTIME:
    begin
     newpri:=rtp^._prio;
     if (newpri>767) then Exit(EINVAL);
    end;
  else
    Exit(EINVAL)
 end;

 thread_lock(td);
 sched_class(td,rtp^._type);
 sched_user_prio(td, newpri);

 if (td=curkthread) then
 begin
  sched_prio(td,td^.td_user_pri);
 end;

 thread_unlock(td);
end;

procedure pri_to_rtp(td:p_kthread;rtp:p_rtprio);
begin
 thread_lock(td);

 case (td^.td_pri_class and $fff7) of //PRI_BASE
  PRI_REALTIME,
  RTP_PRIO_NORMAL,
  PRI_IDLE:
    begin
     rtp^._prio:=td^.td_base_user_pri;
    end;
  else;
 end;
 rtp^._type:=td^.td_pri_class;

 thread_unlock(td);
end;

procedure sched_fork_thread(td,childtd:p_kthread);
begin
 if (td<>nil) then
 begin
  cpuset_setaffinity(childtd,td^.td_cpuset);
  sched_priority(td,td^.td_base_pri);
 end;
end;

procedure sched_class(td:p_kthread;_class:Integer); inline;
begin
 td^.td_pri_class:=_class;
end;

function sched_priority(td:p_kthread;prio:Integer):Integer; inline;
begin
 Result:=cpu_set_priority(td,prio);
end;

procedure sched_prio(td:p_kthread;prio:Integer);
begin
 td^.td_base_pri:=prio;
 sched_priority(td, prio);
end;

procedure sched_user_prio(td:p_kthread;prio:Integer); inline;
begin
 td^.td_base_user_pri:=prio;
 if (td^.td_lend_user_pri<=prio) then Exit;
 td^.td_user_pri:=prio;
end;

function min(a,b:Integer):Integer; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

procedure sched_lend_user_prio(td:p_kthread;prio:Integer);
begin
 td^.td_lend_user_pri:=prio;
 td^.td_user_pri:=min(prio,td^.td_base_user_pri);
 if (td^.td_priority>td^.td_user_pri) then
 begin
  sched_prio(td,td^.td_user_pri);
 end;
end;






initialization
 threadinit;

end.

