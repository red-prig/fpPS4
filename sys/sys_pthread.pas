unit sys_pthread;

{$mode ObjFPC}{$H+}

interface

uses
 sys_kernel,
 sys_signal;

const
//Run-time invariant values:
 PTHREAD_STACK_MIN=16384;

 PTHREAD_EXPLICIT_SCHED=0;

 PTHREAD_DETACHED     =$1;
 PTHREAD_SCOPE_SYSTEM =$2;
 PTHREAD_INHERIT_SCHED=$4;
 PTHREAD_NOFLOAT      =$8;

 PTHREAD_CREATE_DETACHED=PTHREAD_DETACHED;
 PTHREAD_CREATE_JOINABLE=0;

 PTHREAD_CANCEL_ENABLE      =0;
 PTHREAD_CANCEL_DISABLE     =1;

 PTHREAD_CANCEL_DEFERRED    =0;
 PTHREAD_CANCEL_ASYNCHRONOUS=2;
 PTHREAD_CANCELED           =Pointer(1);

 SCE_PTHREAD_DESTRUCTOR_ITERATIONS =4;
 SCE_PTHREAD_KEYS_MAX              =256;
 SCE_PTHREAD_STACK_MIN             =PTHREAD_STACK_MIN;
 SCE_PTHREAD_THREADS_MAX           =High(DWORD);
 SCE_PTHREAD_BARRIER_SERIAL_THREAD =-1;

//Flags for threads and thread attributes.
 SCE_PTHREAD_DETACHED              =$1;
 SCE_PTHREAD_INHERIT_SCHED         =$4;
 SCE_PTHREAD_NOFLOAT               =$8;

 SCE_PTHREAD_CREATE_DETACHED       =SCE_PTHREAD_DETACHED;
 SCE_PTHREAD_CREATE_JOINABLE       =0;
 SCE_PTHREAD_EXPLICIT_SCHED        =0;

//Flags for read/write lock attributes
 SCE_PTHREAD_PROCESS_PRIVATE       =0;
 SCE_PTHREAD_PROCESS_SHARED        =1;

//POSIX scheduling policies
 SCHED_FIFO      =1;
 SCHED_OTHER     =2;
 SCHED_RR        =3;

// for sceKernelMsync()
 SCE_KERNEL_MS_SYNC       =$0;
 SCE_KERNEL_MS_ASYNC      =$1;
 SCE_KERNEL_MS_INVALIDATE =$2;

// for sceKernelSchedGetPriorityMax()/Min()
 SCE_KERNEL_SCHED_FIFO           =SCHED_FIFO;
 SCE_KERNEL_SCHED_RR             =SCHED_RR;
 SCE_KERNEL_PRIO_FIFO_DEFAULT    =700;
 SCE_KERNEL_PRIO_FIFO_HIGHEST    =256;
 SCE_KERNEL_PRIO_FIFO_LOWEST     =767;

// for SceKernelCpumask
 SCE_KERNEL_CPUMASK_6CPU_ALL   =$3f;
 SCE_KERNEL_CPUMASK_7CPU_ALL   =$7f;
 SCE_KERNEL_CPUMASK_USER_ALL   =$3f; // obsolete

 //Thread creation state attributes.
 THR_CREATE_RUNNING  =0;
 THR_CREATE_SUSPENDED=1;

 //Miscellaneous definitions.
 THR_STACK_DEFAULT=(2 * 1024 * 1024);
 THR_STACK_INITIAL=(THR_STACK_DEFAULT * 2);

 THR_STACK_USER=$100; // 0xFF reserved for <pthread.h>

type
 p_pthread_attr_t=^pthread_attr_t;
 pthread_attr_t=^pthread_attr;
 pthread_attr=packed record
  sched_policy :Integer;
  sched_inherit:Integer;
  prio         :Integer;
  suspend      :Integer;
  flags        :Integer; //((*attr)->flags & PTHREAD_DETACHED)
  _align       :Integer;
  stackaddr_attr:Pointer;
  stacksize_attr:QWORD;
  guardsize_attr:QWORD;
  cpuset    :QWORD; //cpuset_t *cpuset;
  cpusetsize:QWORD;
 end;

 p_pthread_once_t=^pthread_once_t;
 pthread_once_t=packed record
  state:Integer;
  _align:Integer;
  mutex:Pointer; //pthread_mutex
 end;

const
 //Flags for once initialization.
 PTHREAD_NEEDS_INIT=0;
 PTHREAD_DONE_INIT =1;

 //Static once initialization values.
 PTHREAD_ONCE_INIT:pthread_once_t=(state:PTHREAD_NEEDS_INIT;_align:0;mutex:nil);

type
 p_pthread_key_data=^_pthread_key_data;
 _pthread_key_data=packed record
  version_:ptruint;
  data_:Pointer;
 end;

 _pthread_keys=array[0..SCE_PTHREAD_KEYS_MAX-1] of _pthread_key_data;

 t_init_routine_proc=procedure; SysV_ABI_CDecl;
 t_cb_proc=procedure(data:Pointer); SysV_ABI_CDecl;

 Ppthread_key_t=^pthread_key_t;
 pthread_key_t=DWORD;

 p_pthread_cleanup=^pthread_cleanup;
 pthread_cleanup=packed record
  prev:p_pthread_cleanup;
  routine:t_cb_proc;
  routine_arg:Pointer;
  onheap:Integer;
 end;

 p_pthread=^pthread;
 pthread=^pthread_t;
 pthread_t=record
  entry:Pointer;
  arg:Pointer;
  handle:TThreadID;
  ThreadId:TThreadID;
  detachstate:Integer;
  Attr:pthread_attr;
  name:array[0..31] of AnsiChar;
  //
  errno:QWORD;
  net_errno:QWORD;
  //
  cleanup:p_pthread_cleanup;
  //
  specific:p_pthread_key_data;
  //
  sig:sigqueue_t;
 end;

 PSceKernelSchedParam=^SceKernelSchedParam;
 SceKernelSchedParam=packed record
  sched_priority:Integer;
 end;

threadvar
 tcb_thread:pthread;

var
 sceKernelThreadDtors:TProcedure;

function _get_curthread:pthread; inline;

function sysv_wrapper(arg,proc:Pointer):Pointer; SysV_ABI_CDecl;

function SysBeginThread(sa:Pointer;
                        stacksize:ptruint;
                        ThreadMain:Pointer; //function ThreadMain(param : pointer) : Longint; stdcall;
                        p:pointer;
                        creationFlags:dword;
                        var ThreadId:TThreadID):TThreadID;

function sys_get_thread_prior(handle:TThreadID):Integer;
function sys_set_thread_prior(handle:TThreadID;prio:Integer):Integer;

implementation

uses
 atomic,
 spinlock,
 Windows;

function _get_curthread:pthread; inline;
begin
 Result:=tcb_thread;
end;

var
 _lazy_init:Integer=0;
 _lazy_wait:Integer=0;

function _thread_null(parameter:pointer):ptrint;
begin
 Result:=0;
end;

procedure init_threads;
begin
 if XCHG(_lazy_init,1)=0 then
 begin
  BeginThread(@_thread_null);
  fetch_add(_lazy_wait,1);
 end else
 begin
  wait_until_equal(_lazy_wait,0);
 end;
end;

//rdi,rsi:rax
function sysv_wrapper(arg,proc:Pointer):Pointer; SysV_ABI_CDecl; assembler; nostackframe;
asm
    subq   $40, %rsp
.seh_stackalloc 40
.seh_endprologue
    call   %rsi
    nop                     { this nop is critical for exception handling }
    addq   $40, %rsp
.seh_handler __FPC_default_handler,@except,@unwind
end;

function SysBeginThread(sa:Pointer;
                        stacksize:ptruint;
                        ThreadMain:Pointer; //function ThreadMain(param : pointer) : Longint; stdcall;
                        p:pointer;
                        creationFlags:dword;
                        var ThreadId:TThreadID):TThreadID;
var
 _threadid:dword;
begin
 _sig_lock;

 init_threads;

 _threadid:=0;
 Result:=CreateThread(sa,stacksize,ThreadMain,p,creationflags,_threadid);

 _sig_unlock;

 ThreadID:=_threadid;
end;

//ThreadGetPriority = -15 and 15. :0..30
//scePthreadGetprio = 767 and 256 :0..511

function sys_get_thread_prior(handle:TThreadID):Integer;
begin
 _sig_lock;
  Result:=System.ThreadGetPriority(handle);
 _sig_unlock;
 Result:=767-(((Result+15)*511) div 30);
end;

const
 PRIORITY_TABLE:array[0..30] of SmallInt=(
  { 0}  THREAD_PRIORITY_IDLE         , //-15
  { 1}  THREAD_PRIORITY_IDLE         , //-15
  { 2}  THREAD_PRIORITY_IDLE         , //-15
  { 3}  THREAD_PRIORITY_LOWEST       , // -2
  { 4}  THREAD_PRIORITY_LOWEST       , // -2
  { 5}  THREAD_PRIORITY_LOWEST       , // -2
  { 6}  THREAD_PRIORITY_LOWEST       , // -2
  { 7}  THREAD_PRIORITY_LOWEST       , // -2
  { 8}  THREAD_PRIORITY_BELOW_NORMAL , // -1
  { 9}  THREAD_PRIORITY_BELOW_NORMAL , // -1
  {10}  THREAD_PRIORITY_BELOW_NORMAL , // -1
  {11}  THREAD_PRIORITY_BELOW_NORMAL , // -1
  {12}  THREAD_PRIORITY_BELOW_NORMAL , // -1
  {13}  THREAD_PRIORITY_NORMAL       , //  0
  {14}  THREAD_PRIORITY_NORMAL       , //  0
  {15}  THREAD_PRIORITY_NORMAL       , //  0
  {16}  THREAD_PRIORITY_NORMAL       , //  0
  {17}  THREAD_PRIORITY_NORMAL       , //  0
  {18}  THREAD_PRIORITY_ABOVE_NORMAL , //  1
  {19}  THREAD_PRIORITY_ABOVE_NORMAL , //  1
  {20}  THREAD_PRIORITY_ABOVE_NORMAL , //  1
  {21}  THREAD_PRIORITY_ABOVE_NORMAL , //  1
  {22}  THREAD_PRIORITY_ABOVE_NORMAL , //  1
  {23}  THREAD_PRIORITY_ABOVE_NORMAL , //  2
  {24}  THREAD_PRIORITY_ABOVE_NORMAL , //  2
  {25}  THREAD_PRIORITY_ABOVE_NORMAL , //  2
  {26}  THREAD_PRIORITY_ABOVE_NORMAL , //  2
  {27}  THREAD_PRIORITY_ABOVE_NORMAL , //  2
  {28}  THREAD_PRIORITY_ABOVE_NORMAL , // 15
  {29}  THREAD_PRIORITY_ABOVE_NORMAL , // 15
  {30}  THREAD_PRIORITY_ABOVE_NORMAL   // 15
 );

function sys_set_thread_prior(handle:TThreadID;prio:Integer):Integer;
begin
 if (prio>767) or (prio<256) then Exit(EINVAL);
 prio:=(((767-prio)*30) div 511);
 prio:=PRIORITY_TABLE[prio];
 _sig_lock;
  if System.ThreadSetPriority(handle,prio) then
  begin
   Result:=0;
  end else
  begin
   Result:=ESRCH;
  end;
 _sig_unlock;
end;



end.

