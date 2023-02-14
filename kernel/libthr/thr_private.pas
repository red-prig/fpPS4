unit thr_private;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 signal,
 _umtx;

const
 THR_MUTEX_INITIALIZER         =nil;
 THR_ADAPTIVE_MUTEX_INITIALIZER=Pointer(1);
 THR_MUTEX_DESTROYED           =Pointer(2);
 THR_COND_INITIALIZER          =nil;
 THR_COND_DESTROYED            =Pointer(1);
 THR_RWLOCK_INITIALIZER        =nil;
 THR_RWLOCK_DESTROYED          =Pointer(1);

 PMUTEX_FLAG_TYPE_MASK=$0ff;
 PMUTEX_FLAG_PRIVATE  =$100;
 PMUTEX_FLAG_DEFERED  =$200;

 MAX_DEFER_WAITERS    =50;

 //Flags for condition variables.
 COND_FLAGS_PRIVATE=$01;
 COND_FLAGS_INITED =$02;
 COND_FLAGS_BUSY   =$04;

 THR_STACK_USER=$100; // 0xFF reserved for <pthread.h>

 //Thread creation state attributes.
 THR_CREATE_RUNNING  =0;
 THR_CREATE_SUSPENDED=1;

 //Miscellaneous definitions.
 THR_STACK_DEFAULT=(2 * 1024 * 1024);
 THR_STACK_INITIAL=(THR_STACK_DEFAULT * 2);

type
 p_pthread=^pthread;

 p_pthread_mutex=^pthread_mutex;

 pthread_mutex_entry=packed record
  pPrev,pNext:p_pthread_mutex;
 end;

 pthread_mutex=packed record
  //Lock for accesses to this structure.
  m_lock      :umutex;
  m_flags     :Integer;
  magic2      :DWORD;
  m_owner     :p_pthread;
  m_count     :Integer;
  m_spinloops :Integer;
  m_yieldloops:Integer;
  magic1      :DWORD;
  //Link for all mutexes a thread currently owns.
  m_qe        :pthread_mutex_entry;
  //
 end;

 pthread_mutex_attr=packed record
  m_type    :Integer;
  m_protocol:Integer;
  m_ceiling :Integer;
 end;

 pthread_cond=packed record
  __has_user_waiters:DWORD;
  __has_kern_waiters:DWORD;
  __flags           :DWORD;
  __clock_id        :DWORD;
  magic             :DWORD;
  u1,u2,u3          :DWORD;
 end;

 pthread_cond_attr=packed record
  c_pshared:Integer;
  c_clockid:Integer;
 end;

 pthread_barrier=packed record
  b_lock   :umutex;
  b_cv     :ucond;
  b_cycle  :int64;
  b_count  :Integer;
  b_waiters:Integer;
 end;

 pthread_barrierattr=packed record
  pshared:Integer;
 end;

 pthread_spinlock=packed record
  s_lock:umutex;
 end;

 t_routine_proc=procedure(data:Pointer); SysV_ABI_CDecl;

 p_pthread_cleanup=^pthread_cleanup;
 pthread_cleanup=packed record
  prev:p_pthread_cleanup;
  routine:t_routine_proc;
  routine_arg:Pointer;
  onheap:Integer;
 end;

 p_pthread_atfork=^pthread_atfork;
 pthread_atfork=packed record
  qe_prev:p_pthread_atfork;
  qe_next:p_pthread_atfork;
  prepare:TProcedure;
  parent :TProcedure;
  child  :TProcedure;
 end;

 pthread_attr=packed record
  sched_policy  :Integer;
  sched_inherit :Integer;
  prio          :Integer;
  suspend       :Integer;
  flags         :Integer;
  _align        :Integer;
  stackaddr_attr:Pointer;
  stacksize_attr:ptruint;
  guardsize_attr:ptruint;
  cpuset        :ptruint;
  cpusetsize    :ptruint;
 end;

 p_wake_addr=^wake_addr;
 wake_addr=packed record
  link :p_wake_addr;
  value:DWORD;
  pad  :array[0..11] of Byte;
 end;

 p_sleepqueue=^sleepqueue;

 sleepqueue_entry=packed record
  pPrev,pNext:p_sleepqueue;
 end;

 sleepqueue=packed record
  sq_blocked:sleepqueue_entry;
  sq_freeq  :p_sleepqueue;
  sq_hash   :sleepqueue_entry;
  sq_flink  :p_sleepqueue;
  sq_wchan  :Pointer;
  sq_type   :Integer;
 end;

 pthread_prio=packed record
  pri_min    :Integer;
  pri_max    :Integer;
  pri_default:Integer;
 end;

 pthread_rwlockattr=packed record
  pshared:Integer;
  type_np:Integer;
 end;

 pthread_rwlock=packed record
  lock:urwlock;
  owner:Pointer; //pthread*
  magic:DWORD;
  align:DWORD;
 end;

 _Unwind_Exception=packed record
  exception_class  :Int64;
  exception_cleanup:Pointer;
  private_1        :QWORD;
  private_2        :QWORD;
 end;

 td_event_msg_t=array[0..5] of DWORD;

 p_pthread_specific_elem=^pthread_specific_elem;
 pthread_specific_elem=packed record
  data :Pointer;
  seqno:Integer;
  align:Integer;
 end;

 p_pthread_key=^pthread_key;
 pthread_key=packed record
  allocated:Integer;
  seqno:Integer;
  _destructor:Pointer;
 end;

 pthreadlist_entry=packed record
  pPrev,pNext:p_pthread;
 end;

 pthread=packed record
  tid                :Integer;
  _align1            :Integer;
  lock               :umutex;                //Lock for accesses to this thread structure.
  cycle              :Integer;               //Internal condition variable cycle number.
  locklevel          :Integer;               //How many low level locks the thread held.
  critical_count     :Integer;               //Set to non-zero when this thread has entered a critical region.
  sigblock           :Integer;               //Signal blocked counter.
  tle                :pthreadlist_entry;     //link for all threads in process
  gcle               :pthreadlist_entry;     //Queue entry for GC lists.
  hle                :pthreadlist_entry;     //Hash queue entry.
  wle                :pthreadlist_entry;     //Sleep queue entry
  refcount           :Integer;               //Threads reference count.
  _align2            :Integer;
  start_routine      :Pointer;
  arg                :Pointer;
  attr               :pthread_attr;
  cancel_enable      :Integer;               //Cancellation is enabled
  cancel_pending     :Integer;               //Cancellation request is pending
  cancel_point       :Integer;               //Thread is at cancellation point
  no_cancel          :Integer;               //Cancellation is temporarily disabled
  cancel_async       :Integer;               //Asynchronouse cancellation is enabled
  cancelling         :Integer;               //Cancellation is in progress
  sigmask            :sigset_t;              //Thread temporary signal mask.
  unblock_sigcancel  :Integer;               //Thread should unblock SIGCANCEL.
  in_sigsuspend      :Integer;               //In sigsuspend state
  deferred_siginfo   :siginfo_t;             //deferred signal info
  deferred_sigmask   :sigset_t;              //signal mask to restore.
  deferred_sigact    :sigaction_t;           //the sigaction should be used for deferred signal.
  deferred_run       :Integer;               //deferred signal delivery is performed, do not reenter.
  force_exit         :Integer;               //Force new thread to exit.
  state              :Integer;               //Thread state
  error              :Integer;               //Error variable used instead of errno.
  _align4            :Integer;
  joiner             :Pointer;               //The joiner is the thread that is joining to this thread.
  flags              :Integer;               //Miscellaneous flags; only set with scheduling lock held.
  tlflags            :Integer;               //Thread list flags; only set with thread list lock held.
  mutexq             :pthread_mutex_entry;   //Queue of currently owned NORMAL or PRIO_INHERIT type mutexes.
  pp_mutexq          :pthread_mutex_entry;   //Queue of all owned PRIO_PROTECT mutexes.
  ret                :Pointer;
  specific           :p_pthread_specific_elem;
  specific_data_count:Integer;
  rdlock_count       :Integer;               //Number rwlocks rdlocks held.
  rtld_bits          :Integer;               //Current locks bitmap for rtld.
  _align5            :Integer;
  tcb                :Pointer;               //Thread control block
  cleanup            :p_pthread_cleanup;     //Cleanup handlers Link List
  ex                 :_Unwind_Exception;
  unwind_stackend    :Pointer;
  unwind_disabled    :Integer;
  magic              :DWORD;                 //Magic value to help recognize a valid thread structure from an invalid one
  report_events      :Integer;               //Enable event reporting
  event_mask         :Integer;
  event_buf          :td_event_msg_t;        //Event
  wchan              :Pointer;               //Wait channel
  mutex_obj          :p_pthread_mutex;       //Referenced mutex
  will_sleep         :Integer;               //Thread will sleep
  nwaiter_defer      :Integer;               //Number of threads deferred
  defer_waiters      :array[0..49] of QWORD; //Deferred threads from pthread_cond_signal
  spec_flag          :Integer;               //bit 1 specific is alloc
  _align6            :Integer;
  wake_addr          :p_wake_addr;
  sleepqueue         :p_sleepqueue;          //Sleep queue
 end;

const
 //pthread_state
 PS_RUNNING=0;
 PS_DEAD   =1;

 // Miscellaneous flags; only set with scheduling lock held.
 THR_FLAGS_PRIVATE     =$0001;
 THR_FLAGS_NEED_SUSPEND=$0002; // thread should be suspended
 THR_FLAGS_SUSPENDED   =$0004; // thread is suspended
 THR_FLAGS_DETACHED    =$0008; // thread is detached

 // Thread list flags; only set with thread list lock held.
 TLFLAGS_GC_SAFE  =$0001; // thread safe for cleaning
 TLFLAGS_IN_TDLIST=$0002; // thread in all thread list
 TLFLAGS_IN_GCLIST=$0004; // thread in gc list

 THR_MAGIC=$d09ba115;

function SHOULD_CANCEL(thr:p_pthread):Boolean; inline;
function THR_SHOULD_GC(thr:p_pthread):Boolean; inline;
function THR_IN_CRITICAL(thr:p_pthread):Boolean; inline;

implementation

function SHOULD_CANCEL(thr:p_pthread):Boolean; inline;
begin
 Result:=(thr^.cancel_pending<>0) and
         (thr^.cancel_enable<>0) and
         (thr^.no_cancel=0);
end;

function THR_SHOULD_GC(thr:p_pthread):Boolean; inline;
begin
 Result:=(thr^.refcount=0) and
         (thr^.state=PS_DEAD) and
         ((thr^.flags and THR_FLAGS_DETACHED)<>0);
end;

function THR_IN_CRITICAL(thr:p_pthread):Boolean; inline;
begin
 Result:=(thr^.locklevel>0) or
         (thr^.critical_count>0);
end;

end.

