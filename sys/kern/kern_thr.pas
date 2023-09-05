unit kern_thr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 ucontext,
 signal,
 signalvar;

const
 TDS_INACTIVE =0;
 TDS_INHIBITED=1;
 TDS_CAN_RUN  =2;
 TDS_RUNQ     =3;
 TDS_RUNNING  =4;

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

 TDF_SLICEEND   =TDF_SCHED2; // Thread time slice is over.

 TDF_SUSP_CTX   =$80000000;  // Sony extension

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

 TDI_SUSPENDED    =$0001; // On suspension queue.
 TDI_SLEEPING     =$0002; // Actually asleep! (tricky).
 TDI_SWAPPED      =$0004; // Stack not in mem.  Bad juju if run.
 TDI_LOCK         =$0008; // Stopped on a lock.
 TDI_IWAIT        =$0010; // Awaiting interrupt.

 TDI_SUSP_CTX     =$0020; // Sony extension

 THR_SUSPENDED    =$0001;

 // These flags are kept in p_flag.
 P_ADVLOCK        =$00001; // Process may hold a POSIX advisory lock.
 P_CONTROLT       =$00002; // Has a controlling terminal.
 P_WEXIT          =$02000; // Working on exiting.
 P_EXEC           =$04000; // Process called exec.
 P_INEXEC       =$4000000; // Process is in execve().

 // Types and flags for mi_switch().
 SW_TYPE_MASK     =$ff; // First 8 bits are switch type
 SWT_NONE          = 0; // Unspecified switch.
 SWT_PREEMPT       = 1; // Switching due to preemption.
 SWT_OWEPREEMPT    = 2; // Switching due to opepreempt.
 SWT_TURNSTILE     = 3; // Turnstile contention.
 SWT_SLEEPQ        = 4; // Sleepq wait.
 SWT_SLEEPQTIMO    = 5; // Sleepq timeout wait.
 SWT_RELINQUISH    = 6; // yield call.
 SWT_NEEDRESCHED   = 7; // NEEDRESCHED was set.
 SWT_IDLE          = 8; // Switching from the idle thread.
 SWT_IWAIT         = 9; // Waiting for interrupts.
 SWT_SUSPEND       =10; // Thread suspended.
 SWT_REMOTEPREEMPT =11; // Remote processor preempted.
 SWT_REMOTEWAKEIDLE=12; // Remote processor preempted idle.
 SWT_COUNT         =13; // Number of switch types.

 // Flags
 SW_VOL    =$0100; // Voluntary switch.
 SW_INVOL  =$0200; // Involuntary switch.
 SW_PREEMPT=$0400; // The invol switch is a preemption

type
 p_teb=^teb;
 teb=packed record
  SEH    :Pointer;
  stack  :Pointer;
  sttop  :Pointer;
  _align1:array[0..20] of QWORD;
  jitcall:Pointer;                 //0x0C0
  _align2:array[0..30] of QWORD;
  gsbase :Pointer;                 //0x1C0
  jit_rsp:Pointer;                 //0x1C8
  jit_rax:Pointer;                 //0x1D0
  _align3:array[0..164] of QWORD;
  thread :Pointer;                 //0x700
  tcb    :Pointer;                 //0x708
  iflag  :Integer;                 //0x710
 end;

const
 teb_jitcall=ptruint(@teb(nil^).jitcall);
 teb_gsbase =ptruint(@teb(nil^).gsbase );
 teb_thread =ptruint(@teb(nil^).thread );
 teb_tcb    =ptruint(@teb(nil^).tcb    );
 teb_iflag  =ptruint(@teb(nil^).iflag  );

 {$IF teb_jitcall<>$0C0}{$STOP teb_jitcall<>$0C0}{$ENDIF}
 {$IF teb_gsbase <>$1C0}{$STOP teb_gsbase <>$1C0}{$ENDIF}
 {$IF teb_thread <>$700}{$STOP teb_thread <>$700}{$ENDIF}
 {$IF teb_tcb    <>$708}{$STOP teb_tcb    <>$708}{$ENDIF}
 {$IF teb_iflag  <>$710}{$STOP teb_iflag  <>$710}{$ENDIF}

type
 t_td_name=array[0..31] of AnsiChar;

 t_td_stack=packed record
  stack:Pointer;
  sttop:Pointer;
 end;

 pp_kthread=^p_kthread;
 p_kthread=^kthread;
 kthread=record
  td_umtxq        :Pointer; //p_umtx_q
  td_handle       :THandle; //nt thread
  td_teb          :p_teb;
  td_lock         :Pointer;
  td_tid          :QWORD;
  td_sigstk       :stack_t;
  td_state        :Integer;
  td_pflags       :Integer;
  td_flags        :Integer;
  td_errno        :Integer;
  pcb_flags       :Integer;
  td_ref          :Integer;
  //
  td_priority     :Word;
  td_pri_class    :Word;
  td_base_pri     :Word;
  td_base_user_pri:Word;
  td_lend_user_pri:Word;
  td_user_pri     :Word;
  td_name         :t_td_name;
  //
  td_cpuset       :Ptruint;
  td_sigmask      :sigset_t;
  td_oldsigmask   :sigset_t;
  td_sigqueue     :sigqueue_t;
  td_frame        :trapframe;
  td_fpstate      :array[0..103] of QWORD;
  pcb_fsbase      :Pointer;
  pcb_gsbase      :Pointer;
  td_retval       :array[0..1] of QWORD;
  td_ustack       :t_td_stack;
  td_kstack       :t_td_stack;
  //
  td_sleepqueue   :Pointer;
  td_slpq         :TAILQ_ENTRY;
  td_wchan        :Pointer;
  td_sqqueue      :Integer;
  td_intrval      :Integer;
  td_inhibitors   :Integer;
  td_dupfd        :Integer;
  td_ru           :packed record
   ru_inblock     :Int64;
   ru_oublock     :Int64;
   ru_nsignals    :Int64;
   ru_nvcsw       :Int64;
   ru_nivcsw      :Int64;
  end;
  td_slptick      :Int64;
  //
  td_fpop         :Pointer;
  td_map_def_user :Pointer;
  td_dmap_def_user:Pointer;
  td_rmap_def_user:Pointer;
  td_sel          :Pointer;
  td_vp_reserv    :Int64;
  pcb_onfault     :Pointer;
 end;

 p_thr_param=^thr_param;
 thr_param=packed record
  start_func:Pointer;
  arg       :Pointer;
  stack_base:Pointer;
  stack_size:Ptruint;
  tls_base  :Pointer;
  tls_size  :Ptruint;
  child_tid :PDWORD;
  parent_tid:PDWORD;
  flags     :Integer;
  align     :Integer;
  rtp       :Pointer;
  name      :PChar;
  spare     :array[0..1] of Pointer;
 end;
 {$IF sizeof(thr_param)<>104}{$STOP sizeof(thr_param)<>104}{$ENDIF}

function  curkthread:p_kthread;
procedure set_curkthread(td:p_kthread);

function  TD_IS_SLEEPING(td:p_kthread):Boolean;
function  TD_ON_SLEEPQ(td:p_kthread):Boolean;
function  TD_IS_SUSPENDED(td:p_kthread):Boolean;
function  TD_IS_SWAPPED(td:p_kthread):Boolean;
function  TD_ON_LOCK(td:p_kthread):Boolean;
function  TD_AWAITING_INTR(td:p_kthread):Boolean;
function  TD_IS_RUNNING(td:p_kthread):Boolean;
function  TD_ON_RUNQ(td:p_kthread):Boolean;
function  TD_CAN_RUN(td:p_kthread):Boolean;
function  TD_IS_INHIBITED(td:p_kthread):Boolean;
function  TD_ON_UPILOCK(td:p_kthread):Boolean;
function  TD_IS_IDLETHREAD(td:p_kthread):Boolean;

procedure TD_SET_SLEEPING(td:p_kthread);
procedure TD_SET_SWAPPED(td:p_kthread);
procedure TD_SET_LOCK(td:p_kthread);
procedure TD_SET_SUSPENDED(td:p_kthread);
procedure TD_SET_IWAIT(td:p_kthread);

procedure TD_CLR_SLEEPING(td:p_kthread);
procedure TD_CLR_SWAPPED(td:p_kthread);
procedure TD_CLR_LOCK(td:p_kthread);
procedure TD_CLR_SUSPENDED(td:p_kthread);
procedure TD_CLR_IWAIT(td:p_kthread);

procedure TD_SET_RUNNING(td:p_kthread);
procedure TD_SET_RUNQ(td:p_kthread);
procedure TD_SET_CAN_RUN(td:p_kthread);

procedure THREAD_NO_SLEEPING();
procedure THREAD_SLEEPING_OK();

function  curthread_pflags_set(flags:Integer):Integer;
procedure curthread_pflags_restore(save:Integer);
procedure curthread_set_pcb_onfault(v:Pointer);

implementation

function curkthread:p_kthread; assembler; nostackframe;
asm
 movqq %gs:teb.thread,Result
end;

procedure set_curkthread(td:p_kthread); assembler; nostackframe;
asm
 movqq td,%gs:teb.thread
end;

function TD_IS_SLEEPING(td:p_kthread):Boolean;
begin
 Result:=(td^.td_inhibitors and TDI_SLEEPING)<>0;
end;

function TD_ON_SLEEPQ(td:p_kthread):Boolean;
begin
 Result:=(td^.td_wchan<>nil);
end;

function TD_IS_SUSPENDED(td:p_kthread):Boolean;
begin
 Result:=(td^.td_inhibitors and TDI_SUSPENDED)<>0;
end;

function TD_IS_SWAPPED(td:p_kthread):Boolean;
begin
 Result:=(td^.td_inhibitors and TDI_SWAPPED)<>0;
end;

function TD_ON_LOCK(td:p_kthread):Boolean;
begin
 Result:=(td^.td_inhibitors and TDI_LOCK)<>0;
end;

function TD_AWAITING_INTR(td:p_kthread):Boolean;
begin
 Result:=(td^.td_inhibitors and TDI_IWAIT)<>0;
end;

function TD_IS_RUNNING(td:p_kthread):Boolean;
begin
 Result:=(td^.td_state=TDS_RUNNING);
end;

function TD_ON_RUNQ(td:p_kthread):Boolean;
begin
 Result:=(td^.td_state=TDS_RUNQ);
end;

function TD_CAN_RUN(td:p_kthread):Boolean;
begin
 Result:=(td^.td_state=TDS_CAN_RUN);
end;

function TD_IS_INHIBITED(td:p_kthread):Boolean;
begin
 Result:=(td^.td_state=TDS_INHIBITED);
end;

function TD_ON_UPILOCK(td:p_kthread):Boolean;
begin
 Result:=(td^.td_flags and TDF_UPIBLOCKED)<>0;
end;

function TD_IS_IDLETHREAD(td:p_kthread):Boolean;
begin
 Result:=(td^.td_flags and TDF_IDLETD)<>0;
end;

procedure TD_SET_INHIB(td:p_kthread;inhib:Integer); inline;
begin
 td^.td_state:=TDS_INHIBITED;
 td^.td_inhibitors:=td^.td_inhibitors or inhib;
end;

procedure TD_CLR_INHIB(td:p_kthread;inhib:Integer); inline;
begin
 if ((td^.td_inhibitors and inhib)<>0) then
 begin
  inhib:=td^.td_inhibitors and (not inhib);
  td^.td_inhibitors:=inhib;
  if (inhib=0) then
  begin
   td^.td_state:=TDS_CAN_RUN;
  end;
 end;
end;

procedure TD_SET_SLEEPING(td:p_kthread);
begin
 TD_SET_INHIB(td,TDI_SLEEPING);
end;

procedure TD_SET_SWAPPED(td:p_kthread);
begin
 TD_SET_INHIB(td,TDI_SWAPPED);
end;

procedure TD_SET_LOCK(td:p_kthread);
begin
 TD_SET_INHIB(td,TDI_LOCK);
end;

procedure TD_SET_SUSPENDED(td:p_kthread);
begin
 TD_SET_INHIB(td,TDI_SUSPENDED);
end;

procedure TD_SET_IWAIT(td:p_kthread);
begin
 TD_SET_INHIB(td,TDI_IWAIT);
end;


procedure TD_CLR_SLEEPING(td:p_kthread);
begin
 TD_CLR_INHIB(td,TDI_SLEEPING);
end;

procedure TD_CLR_SWAPPED(td:p_kthread);
begin
 TD_CLR_INHIB(td,TDI_SWAPPED);
end;

procedure TD_CLR_LOCK(td:p_kthread);
begin
 TD_CLR_INHIB(td,TDI_LOCK);
end;

procedure TD_CLR_SUSPENDED(td:p_kthread);
begin
 TD_CLR_INHIB(td,TDI_SUSPENDED);
end;

procedure TD_CLR_IWAIT(td:p_kthread);
begin
 TD_CLR_INHIB(td,TDI_IWAIT);
end;

procedure TD_SET_RUNNING(td:p_kthread);
begin
 td^.td_state:=TDS_RUNNING;
end;

procedure TD_SET_RUNQ(td:p_kthread);
begin
 td^.td_state:=TDS_RUNQ;
end;

procedure TD_SET_CAN_RUN(td:p_kthread);
begin
 td^.td_state:=TDS_CAN_RUN;
end;

procedure THREAD_NO_SLEEPING();
var
 td:p_kthread;
begin
 td:=curkthread;
 Assert((td^.td_pflags and TDP_NOSLEEPING)=0,'nested no sleeping');
 td^.td_pflags:=td^.td_pflags or TDP_NOSLEEPING;
end;

procedure THREAD_SLEEPING_OK();
var
 td:p_kthread;
begin
 td:=curkthread;
 Assert((td^.td_pflags and TDP_NOSLEEPING)<>0,'nested sleeping ok');
 td^.td_pflags:=td^.td_pflags and (not TDP_NOSLEEPING);
end;

//

function curthread_pflags_set(flags:Integer):Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(0);
 Result:=(not flags) or (td^.td_pflags and flags);
 td^.td_pflags:=td^.td_pflags or flags;
end;

procedure curthread_pflags_restore(save:Integer);
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit;
 td^.td_pflags:=td^.td_pflags and save;
end;

procedure curthread_set_pcb_onfault(v:Pointer);
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit;
 td^.pcb_onfault:=v;
end;


end.

