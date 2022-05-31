unit ps4_pthread;

{$mode objfpc}{$H+}

interface

uses
 LFQueue,
 windows,
 sys_pthread,
 sys_signal;

procedure ps4_pthread_cxa_finalize(P:Pointer); SysV_ABI_CDecl;

function  ps4_scePthreadAttrInit(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrDestroy(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_pthread_attr_init(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_pthread_attr_destroy(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetstacksize(pAttr:p_pthread_attr_t;size:size_t):Integer; SysV_ABI_CDecl;
function  ps4_pthread_attr_setstacksize(pAttr:p_pthread_attr_t;size:size_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetdetachstate(pAttr:p_pthread_attr_t;detachstate:Integer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetschedpolicy(pAttr:p_pthread_attr_t;policy:Integer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetaffinity(pAttr:p_pthread_attr_t;mask:QWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetaffinity(pAttr:p_pthread_attr_t;mask:PQWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetinheritsched(pAttr:p_pthread_attr_t;inheritSched:Integer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetstackaddr(pAttr:p_pthread_attr_t;stackAddr:PPointer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetstacksize(pAttr:p_pthread_attr_t;stackSize:PQWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetstack(pAttr:p_pthread_attr_t;stackAddr:PPointer;stackSize:PQWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetdetachstate(pAttr:p_pthread_attr_t;detachstate:Pinteger):Integer; SysV_ABI_CDecl;
function  ps4_pthread_attr_getdetachstate(pAttr:p_pthread_attr_t;detachstate:Pinteger):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadAttrGet(pid:pthread;pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadCreate(pthread:p_pthread;pAttr:p_pthread_attr_t;entry:Pointer;arg:Pointer;name:Pchar):Integer; SysV_ABI_CDecl;
function  ps4_pthread_create(pthread:p_pthread;pAttr:p_pthread_attr_t;entry:Pointer;arg:Pointer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadDetach(_pthread:pthread):Integer; SysV_ABI_CDecl;
function  ps4_pthread_detach(_pthread:pthread):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadJoin(_pthread:pthread;value:PPointer):Integer; SysV_ABI_CDecl;

function  ps4_pthread_once(once_control:p_pthread_once_t;init_routine:t_init_routine_proc):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadOnce(once_control:p_pthread_once_t;init_routine:t_init_routine_proc):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadEqual(t1,t2:pthread):Integer; SysV_ABI_CDecl;
function  ps4_pthread_equal(t1,t2:pthread):Integer; SysV_ABI_CDecl;

procedure ps4_scePthreadExit(value_ptr:Pointer); SysV_ABI_CDecl;
procedure ps4_pthread_exit(value_ptr:Pointer); SysV_ABI_CDecl;

function  ps4_pthread_self():pthread; SysV_ABI_CDecl;
function  ps4_scePthreadSelf():pthread; SysV_ABI_CDecl;

function  ps4_getpid():Integer; SysV_ABI_CDecl;

function  ps4_scePthreadGetname(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadRename(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadSetaffinity(_pthread:pthread;mask:QWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadGetaffinity(_pthread:pthread;mask:PQWORD):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadGetprio(_pthread:pthread;prio:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadSetprio(_pthread:pthread;prio:Integer):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadGetschedparam(_pthread:pthread;policy:PInteger;param:PSceKernelSchedParam):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadSetschedparam(_pthread:pthread;policy:Integer;param:PSceKernelSchedParam):Integer; SysV_ABI_CDecl;

function  ps4_sched_get_priority_max(policy:Integer):Integer; SysV_ABI_CDecl;
function  ps4_sched_get_priority_min(policy:Integer):Integer; SysV_ABI_CDecl;

procedure ps4_scePthreadYield(); SysV_ABI_CDecl;
procedure ps4_pthread_yield(); SysV_ABI_CDecl;

procedure ps4_pthread_cleanup_push(routine:t_cb_proc;arg:Pointer); SysV_ABI_CDecl;
procedure ps4_pthread_cleanup_pop(execute:Integer); SysV_ABI_CDecl;
procedure ps4___pthread_cleanup_push_imp(cleanup_routine:t_cb_proc;
                                         cleanup_arg:Pointer;
                                         cleanup_info:p_pthread_cleanup_info); SysV_ABI_CDecl;
procedure ps4___pthread_cleanup_pop_imp(execute:Integer); SysV_ABI_CDecl;

function  ps4_pthread_key_create(pKey:Ppthread_key_t;dest:t_cb_proc):Integer; SysV_ABI_CDecl;
function  ps4_pthread_key_delete(Key:pthread_key_t):Integer; SysV_ABI_CDecl;
function  ps4_pthread_getspecific(Key:pthread_key_t):Pointer; SysV_ABI_CDecl;
function  ps4_pthread_setspecific(Key:pthread_key_t;value:Pointer):Integer; SysV_ABI_CDecl;

function  _pthread_run_entry(pthread:p_pthread):Integer;

implementation

uses
 atomic,
 spinlock,
 sys_kernel,
 ps4_mutex,
 ps4_map_mm,
 ps4_program,
 ps4_elf;

type
 p_pthread_key_node=^_pthread_key_node;
 _pthread_key_node=packed record
  next_:p_pthread_key_node;
  version_:ptruint;
  dest_:t_cb_proc;
 end;

var
 _pthread_key_nodes:array[0..SCE_PTHREAD_KEYS_MAX-1] of _pthread_key_node;
 _pthread_key_queue:TIntrusiveMPSCQueue;
 _pthread_key_queue_lock:Pointer;

procedure _pthread_keys_cleanup_dest; forward;

//struct dl_phdr_info
procedure ps4_pthread_cxa_finalize(P:Pointer); SysV_ABI_CDecl;
begin
 Writeln('__pthread_cxa_finalize');
end;

//struct pthread_attr _pthread_attr_default = {
//        .sched_policy = SCHED_OTHER,
//        .sched_inherit = PTHREAD_INHERIT_SCHED,
//        .prio = 0,
//        .suspend = THR_CREATE_RUNNING,
//        .flags = PTHREAD_SCOPE_SYSTEM,
//        .stackaddr_attr = NULL,
//        .stacksize_attr = THR_STACK_DEFAULT,
//        .guardsize_attr = 0,
//        .cpusetsize = 0,
//        .cpuset = NULL
//};

function ps4_scePthreadAttrInit(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('scePthreadAttrInit');
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 pAttr^:=SwAllocMem(SizeOf(tthread_attr_t));
 if (pAttr^=nil) then Exit(SCE_KERNEL_ERROR_ENOMEM);
 pAttr^^.stacksize_attr:=PTHREAD_STACK_MIN;
 Result:=0;
end;

function ps4_scePthreadAttrDestroy(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('scePthreadAttrDestroy');
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 SwFreeMem(XCHG(pAttr^,nil));
 Result:=0;
end;

function ps4_pthread_attr_init(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('pthread_attr_init');
 if (pAttr=nil) then Exit(EINVAL);
 pAttr^:=SwAllocMem(SizeOf(tthread_attr_t));
 if (pAttr^=nil) then Exit(ENOMEM);
 pAttr^^.stacksize_attr:=PTHREAD_STACK_MIN;
 Result:=0;
end;

function ps4_pthread_attr_destroy(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('pthread_attr_destroy');
 Result:=EINVAL;
 if (pAttr=nil) then Exit;
 SwFreeMem(XCHG(pAttr^,nil));
 Result:=0;
end;

function ps4_scePthreadAttrSetstacksize(pAttr:p_pthread_attr_t;size:size_t):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if (pAttr^=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if (size<PTHREAD_STACK_MIN) then Exit(SCE_KERNEL_ERROR_EINVAL);
 pAttr^^.stacksize_attr:=size;
 Result:=0;
end;

function ps4_pthread_attr_setstacksize(pAttr:p_pthread_attr_t;size:size_t):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 if (pAttr^=nil) then Exit(EINVAL);
 if (size<PTHREAD_STACK_MIN) then Exit(EINVAL);
 pAttr^^.stacksize_attr:=size;
 Result:=0;
end;

function ps4_scePthreadAttrSetdetachstate(pAttr:p_pthread_attr_t;detachstate:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 if (pAttr^=nil) then Exit;

 Case detachstate of
  PTHREAD_CREATE_JOINABLE:;
  PTHREAD_CREATE_DETACHED:;
  else
   Exit(SCE_KERNEL_ERROR_EINVAL);
 end;

 pAttr^^.detachstate:=detachstate;
 Result:=0;
end;

function ps4_scePthreadAttrSetschedpolicy(pAttr:p_pthread_attr_t;policy:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 if (pAttr^=nil) then Exit;
 pAttr^^.policy:=policy;
 Result:=0;
end;

function ps4_scePthreadAttrSetschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) or (param=nil) then Exit;
 if (pAttr^=nil) then Exit;
 pAttr^^.sched_priority:=param^;
 Result:=0;
end;

function ps4_scePthreadAttrSetaffinity(pAttr:p_pthread_attr_t;mask:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 if (pAttr^=nil) then Exit;
 pAttr^^.cpuset:=mask;
 Result:=0;
end;

function ps4_scePthreadAttrGetaffinity(pAttr:p_pthread_attr_t;mask:PQWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) or (mask=nil) then Exit;
 if (pAttr^=nil) then Exit;
 mask^:=pAttr^^.cpuset;
 Result:=0;
end;

function ps4_scePthreadAttrSetinheritsched(pAttr:p_pthread_attr_t;inheritSched:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_scePthreadAttrGetstackaddr(pAttr:p_pthread_attr_t;stackAddr:PPointer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (stackAddr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if (pAttr^=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 stackAddr^:=pAttr^^.stackaddr_attr;
 Result:=0;
end;

function ps4_scePthreadAttrGetstacksize(pAttr:p_pthread_attr_t;stackSize:PQWORD):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (stackSize=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if (pAttr^=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 stackSize^:=pAttr^^.stacksize_attr;
 Result:=0;
end;

function ps4_scePthreadAttrGetstack(pAttr:p_pthread_attr_t;stackAddr:PPointer;stackSize:PQWORD):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (stackAddr=nil) or (stackSize=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if (pAttr^=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 stackAddr^:=pAttr^^.stackaddr_attr;
 stackSize^:=pAttr^^.stacksize_attr;
 Result:=0;
end;

function ps4_scePthreadAttrGetdetachstate(pAttr:p_pthread_attr_t;detachstate:Pinteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (detachstate=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if (pAttr^=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 detachstate^:=pAttr^^.detachstate;
 Result:=0;
end;

function ps4_pthread_attr_getdetachstate(pAttr:p_pthread_attr_t;detachstate:Pinteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (detachstate=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if (pAttr^=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 detachstate^:=pAttr^^.detachstate;
 Result:=0;
end;

procedure _free_pthread(data:pthread);
begin
 _sig_lock;
 System.CloseThread(data^.handle);
 FreeMem(data);
 _sig_unlock;
end;

procedure _thread_init;
begin
 _sig_lock;
 RegistredStack;
 ps4_app.InitThread(0);
 _sig_unlock;
end;

procedure _thread_cleanup;
begin
 _sig_lock;
 _pthread_keys_cleanup_dest;
 ps4_app.FreeThread;
 UnRegistredStack;
 _sig_unlock;
end;

function on_ps4_run_entry(arg:Pointer):Pointer; SysV_ABI_CDecl;
begin
 Result:=nil;
 ps4_app.InitCode;
 Telf_file(ps4_app.prog).mapCodeEntry;
end;

const
 default_name:Pchar='main';

function _pthread_run_entry(pthread:p_pthread):Integer;
var
 attr:pthread_attr_t;
begin
 ps4_pthread_attr_init(@attr);
 ps4_pthread_attr_setstacksize(@attr,DefaultStackSize);
 Result:=ps4_scePthreadCreate(pthread,@attr,@on_ps4_run_entry,nil,default_name);
 ps4_pthread_attr_destroy(@attr);
end;

const
 _PREPARE_FREE=2;
 _PREPARE_JOIN=3;

function on_ps4_run_thread(data:pthread):Longint; stdcall;
type
 Tps4entry=function(arg:Pointer):Pointer; SysV_ABI_CDecl;
begin
 Result:=0;

 StackLength:=data^.Attr.stacksize_attr;
 StackBottom:=Sptr-StackLength;

 ReadBarrier;
 if (data<>nil) and (data^.entry<>nil) then
 begin
  if (data^.Attr.stackaddr_attr=nil) then
  begin
   data^.Attr.stackaddr_attr:=StackBottom;
  end;

  writeln('BeginThread:',data^.name,':',HexStr(data^.entry));
  tcb_thread:=data;
  _thread_init;

  wait_until_equal(data^.handle,0);

  //data^.arg:=Tps4entry(data^.entry)(data^.arg);
  data^.arg:=sysv_wrapper(data^.arg,data^.entry);
  ReadWriteBarrier;

  _thread_cleanup;
  writeln('EndThread:',data^.name);

  if CAS(data^.detachstate,PTHREAD_CREATE_DETACHED,_PREPARE_FREE) then
  begin
   _free_pthread(data);
  end else
  begin
   CAS(data^.detachstate,PTHREAD_CREATE_JOINABLE,_PREPARE_JOIN);
  end;

 end;

 DoneThread;
end;

function ps4_scePthreadAttrGet(pid:pthread;pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
begin
 if (pid=nil) or (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if (pAttr^=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 pAttr^^:=pid^.Attr;
end;

//typedef pthread_t ScePthread;

function ps4_scePthreadCreate(pthread:p_pthread;pAttr:p_pthread_attr_t;entry:Pointer;arg:Pointer;name:Pchar):Integer; SysV_ABI_CDecl;
Var
 data:pthread;
 Handle,ThreadId:TThreadID;
 sa:Pointer;
 ss:SizeUInt;
 creationFlags:dword;
begin
 Writeln('scePthreadCreate:',HexStr(entry),' ',name);

 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pthread=nil) then Exit;

 //if {false} name='AudioOutThread' then
 //if (name='streamThread') or (name='AudioOutThread') then
 //if false then
 begin
  data:=SwAllocMem(SizeOf(pthread_t));
  if (data=nil) then Exit(SCE_KERNEL_ERROR_ENOMEM);

  sigqueue_init(@data^.sig);

  data^.entry:=entry;
  data^.arg:=arg;
  if (name<>nil) then MoveChar0(name^,data^.name,32);

  ReadWriteBarrier;

  if (pAttr<>nil) and (pAttr^<>nil) then
  begin
   data^.Attr:=pAttr^^;
   data^.detachstate:=pAttr^^.detachstate;
   ReadWriteBarrier;

   creationFlags:=0;
   sa:=pAttr^^.stackaddr_attr;
   ss:=pAttr^^.stacksize_attr;

   if (ss<PTHREAD_STACK_MIN) then
   begin
    ss:=PTHREAD_STACK_MIN;
    data^.Attr.stacksize_attr:=ss;
   end;

   ThreadId:=0;
   _sig_lock;

   Handle:=SysBeginThread(sa,ss,@on_ps4_run_thread,data,creationFlags,ThreadId);

   _sig_unlock;
   if (Handle=0) then
   begin
    SwFreeMem(data);
    Exit(SCE_KERNEL_ERROR_EAGAIN);
   end;

   if (pAttr^^.cpuset<>0) then
   begin
    _sig_lock;
    SetThreadAffinityMask(Handle,pAttr^^.cpuset);
    _sig_unlock;
   end;

  end else
  begin
   ThreadId:=0;
   _sig_lock;

   ss:=PTHREAD_STACK_MIN;
   data^.Attr.stacksize_attr:=ss;

   Handle:=SysBeginThread(nil,ss,@on_ps4_run_thread,data,0,ThreadId);

   _sig_unlock;
   if (Handle=0) then
   begin
    SwFreeMem(data);
    Exit(SCE_KERNEL_ERROR_EAGAIN);
   end;
  end;

  XCHG(data^.ThreadId,ThreadId);
  XCHG(data^.handle,Handle);

  pthread^:=data;
 end;

 Result:=0;
end;

function ps4_pthread_create(pthread:p_pthread;pAttr:p_pthread_attr_t;entry:Pointer;arg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=sce2px(ps4_scePthreadCreate(pthread,pAttr,entry,arg,nil));
end;

function ps4_scePthreadDetach(_pthread:pthread):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Writeln('scePthreadDetach:',_pthread^.name);
 if CAS(_pthread^.detachstate,PTHREAD_CREATE_JOINABLE,PTHREAD_CREATE_DETACHED) then
 begin
  _pthread^.Attr.detachstate:=PTHREAD_CREATE_DETACHED;
  Result:=0
 end else
 if CAS(_pthread^.detachstate,_PREPARE_JOIN,_PREPARE_FREE) then
 begin
  _free_pthread(_pthread);
  Result:=0
 end else
 begin
  Result:=SCE_KERNEL_ERROR_EINVAL;
 end;
end;

function ps4_pthread_detach(_pthread:pthread):Integer; SysV_ABI_CDecl;
begin
 Result:=sce2px(ps4_scePthreadDetach(_pthread));
end;

function ps4_scePthreadJoin(_pthread:pthread;value:PPointer):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Writeln('scePthreadJoin:',_pthread^.name);

 if CAS(_pthread^.detachstate,PTHREAD_CREATE_JOINABLE,_PREPARE_FREE) then
 begin
  SwWaitFor(_pthread^.handle,nil);
  if (value<>nil) then value^:=_pthread^.arg;
  _free_pthread(_pthread);
  Result:=0;
 end else
 if CAS(_pthread^.detachstate,_PREPARE_JOIN,_PREPARE_FREE) then
 begin
  if (value<>nil) then value^:=_pthread^.arg;
  _free_pthread(_pthread);
  Result:=0;
 end else
 begin
  Result:=SCE_KERNEL_ERROR_EINVAL;
 end;
end;

function ps4_pthread_once(once_control:p_pthread_once_t;init_routine:t_init_routine_proc):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 if (once_control=nil) or (init_routine=nil) then Exit(EINVAL);

 if (once_control^.state<>PTHREAD_NEEDS_INIT) then Exit(0);

 Result:=ps4_pthread_mutex_lock(@once_control^.mutex);
 if (Result<>0) then Exit;

 if (once_control^.state=PTHREAD_NEEDS_INIT) then
 begin
  //pthread_cleanup_push(_pthread_once_cleanup, co);

  init_routine();

  //pthread_cleanup_pop(0);

  once_control^.state:=PTHREAD_DONE_INIT;
 end;

 ps4_pthread_mutex_unlock(@once_control^.mutex);
end;

function ps4_scePthreadOnce(once_control:p_pthread_once_t;init_routine:t_init_routine_proc):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_once(once_control,init_routine));
end;

function ps4_scePthreadEqual(t1,t2:pthread):Integer; SysV_ABI_CDecl;
begin
 if (t1=t2) then
  Result:=1
 else
  Result:=0;
end;

function ps4_pthread_equal(t1,t2:pthread):Integer; SysV_ABI_CDecl;
begin
 if (t1=t2) then
  Result:=1
 else
  Result:=0;
end;

procedure ps4_scePthreadExit(value_ptr:Pointer); SysV_ABI_CDecl;
var
 data:pthread;
begin
 data:=tcb_thread;
 if (data=nil) then Exit;
 Writeln('ExitThread:',data^.name);
 data^.arg:=value_ptr;

 ReadWriteBarrier;

 _thread_cleanup;

 if CAS(data^.detachstate,PTHREAD_CREATE_DETACHED,_PREPARE_FREE) then
 begin
  _free_pthread(data);
 end else
 begin
  CAS(data^.detachstate,PTHREAD_CREATE_JOINABLE,_PREPARE_JOIN);
 end;

 _sig_lock;
 System.EndThread(0);
end;

procedure ps4_pthread_exit(value_ptr:Pointer); SysV_ABI_CDecl;
begin
 ps4_scePthreadExit(value_ptr);
end;

function ps4_pthread_self():pthread; SysV_ABI_CDecl;
begin
 Result:=tcb_thread;
end;

function ps4_scePthreadSelf():pthread; SysV_ABI_CDecl;
begin
 Result:=tcb_thread;
end;

function ps4_getpid():Integer; SysV_ABI_CDecl;
begin
 Result:=tcb_thread^.ThreadId;
end;

function ps4_scePthreadGetname(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) or (name=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 MoveChar0(_pthread^.name,name^,32);
end;

function ps4_scePthreadRename(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) or (name=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 MoveChar0(name^,_pthread^.name,32);
end;

function ps4_scePthreadSetaffinity(_pthread:pthread;mask:QWORD):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) then Exit(SCE_KERNEL_ERROR_ESRCH);
 Result:=0;
 _pthread^.Attr.cpuset:=mask;
 _sig_lock;
 SetThreadAffinityMask(_pthread^.handle,mask);
 _sig_unlock;
end;

function ps4_scePthreadGetaffinity(_pthread:pthread;mask:PQWORD):Integer; SysV_ABI_CDecl;
var
 tmp:QWORD;
begin
 if (_pthread=nil) then Exit(SCE_KERNEL_ERROR_ESRCH);
 if (mask=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Result:=0;
 if (_pthread^.Attr.cpuset=0) then
 begin
  _sig_lock;
  GetProcessAffinityMask(GetCurrentProcess,@_pthread^.Attr.cpuset,@tmp);
  _sig_unlock;
 end;
 mask^:=_pthread^.Attr.cpuset;
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
  {23}  THREAD_PRIORITY_HIGHEST      , //  2
  {24}  THREAD_PRIORITY_HIGHEST      , //  2
  {25}  THREAD_PRIORITY_HIGHEST      , //  2
  {26}  THREAD_PRIORITY_HIGHEST      , //  2
  {27}  THREAD_PRIORITY_HIGHEST      , //  2
  {28}  THREAD_PRIORITY_TIME_CRITICAL, // 15
  {29}  THREAD_PRIORITY_TIME_CRITICAL, // 15
  {30}  THREAD_PRIORITY_TIME_CRITICAL  // 15
 );

//ThreadGetPriority = -15 and 15. :0..30
//scePthreadGetprio = 767 and 256 :0..511
function ps4_scePthreadGetprio(_pthread:pthread;prio:PInteger):Integer; SysV_ABI_CDecl;
Var
 r:Integer;
begin
 if (_pthread=nil) or (prio=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 _sig_lock;
 r:=System.ThreadGetPriority(_pthread^.handle);
 _sig_unlock;
 prio^:=767-(((r+15)*511) div 30);
 Result:=0;
end;

function ps4_scePthreadSetprio(_pthread:pthread;prio:Integer):Integer; SysV_ABI_CDecl;
Var
 r:Integer;
begin
 if (_pthread=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);

 if (prio>767) then prio:=767;
 if (prio<256) then prio:=256;

 //Writeln('scePthreadSetprio:',prio);
 r:=(((767-prio)*30) div 511);
 r:=PRIORITY_TABLE[r];

 Result:=0;
 _sig_lock;
 if not System.ThreadSetPriority(_pthread^.handle,r) then
 begin
  Result:=SCE_KERNEL_ERROR_ESRCH;
 end;
 _sig_unlock;
end;

//ThreadGetPriority = -15 and 15. :0..30
function ps4_scePthreadGetschedparam(_pthread:pthread;policy:PInteger;param:PSceKernelSchedParam):Integer; SysV_ABI_CDecl;
Var
 r:Integer;
begin
 if (_pthread=nil) or (policy=nil) or (param=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);

 policy^:=SCE_KERNEL_SCHED_RR;

 _sig_lock;
 r:=System.ThreadGetPriority(_pthread^.handle);
 _sig_unlock;
 param^.sched_priority:=(r+15);
 Result:=0;
end;

function ps4_scePthreadSetschedparam(_pthread:pthread;policy:Integer;param:PSceKernelSchedParam):Integer; SysV_ABI_CDecl;
Var
 r:Integer;
begin
 if (_pthread=nil) or (param=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);

 r:=param^.sched_priority;

 if (r>30) then r:=30;
 if (r<0)  then r:=0;

 r:=PRIORITY_TABLE[r];

 Result:=0;
 _sig_lock;
 if not System.ThreadSetPriority(_pthread^.handle,r) then
 begin
  Result:=SCE_KERNEL_ERROR_ESRCH;
 end;
 _sig_unlock;
end;


function ps4_sched_get_priority_max(policy:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=30;
end;

function ps4_sched_get_priority_min(policy:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

procedure ps4_scePthreadYield(); SysV_ABI_CDecl;
begin
 SwYieldExecution;
end;

procedure ps4_pthread_yield(); SysV_ABI_CDecl;
begin
 SwYieldExecution;
end;

procedure ps4_pthread_cleanup_push(routine:t_cb_proc;arg:Pointer); SysV_ABI_CDecl;
begin
 Writeln('pthread_cleanup_push');
end;

procedure ps4_pthread_cleanup_pop(execute:Integer); SysV_ABI_CDecl;
begin
 Writeln('pthread_cleanup_pop');
end;

procedure ps4___pthread_cleanup_push_imp(cleanup_routine:t_cb_proc;
                                         cleanup_arg:Pointer;
                                         cleanup_info:p_pthread_cleanup_info); SysV_ABI_CDecl;
begin
 Writeln('__pthread_cleanup_push_imp');
end;

procedure ps4___pthread_cleanup_pop_imp(execute:Integer); SysV_ABI_CDecl;
begin
 Writeln('__pthread_cleanup_pop_imp');
end;

procedure _pthread_keys_init;
var
 i:Integer;
begin
 _pthread_key_queue.Create;
 For i:=Low(_pthread_key_nodes) to High(_pthread_key_nodes) do
 begin
  _pthread_key_nodes[i]:=Default(_pthread_key_node);
  _pthread_key_queue.Push(@_pthread_key_nodes[i]);
 end;
end;

procedure _pthread_keys_cleanup_dest;
var
 i:Integer;
 local:p_pthread_key_data;

 node:p_pthread_key_node;
 version:ptruint;
 dest:t_cb_proc;
begin
 local:=@tcb_thread^.keys[0];

 For i:=Low(_pthread_key_nodes) to High(_pthread_key_nodes) do
 begin
  node:=@_pthread_key_nodes[i];

  version:=load_consume(node^.version_);
  dest:=t_cb_proc(load_consume(Pointer(node^.dest_)));

  if (ptruint(dest)>1) and
     (local[i].version_=version) and
     (local[i].data_<>nil) then
  begin
   dest(local[i].data_);
  end;

 end;

end;

function ps4_pthread_key_create(pKey:Ppthread_key_t;dest:t_cb_proc):Integer; SysV_ABI_CDecl;
var
 node:p_pthread_key_node;
begin
 if (pKey=nil) then Exit(EINVAL);
 Writeln('pthread_key_create',' ',ps4_pthread_self^.sig._lock);

 if (dest=nil) then dest:=t_cb_proc(1);

 node:=nil;
 spin_lock(_pthread_key_queue_lock);
 _pthread_key_queue.Pop(node);
 spin_unlock(_pthread_key_queue_lock);

 if (node=nil) then Exit(EAGAIN);

 System.InterlockedIncrement(Pointer(node^.version_));
 XCHG(Pointer(node^.dest_),Pointer(dest));

 pKey^:=(node-p_pthread_key_node(@_pthread_key_nodes));
 Result:=0;
end;

function ps4_pthread_key_delete(Key:pthread_key_t):Integer; SysV_ABI_CDecl;
var
 node:p_pthread_key_node;
begin
 if (DWORD(Key)>=SCE_PTHREAD_KEYS_MAX) then Exit(EINVAL);
 Writeln('pthread_key_delete');

 node:=@_pthread_key_nodes[Key];

 if (XCHG(Pointer(node^.dest_),nil)=nil) then Exit(EINVAL);

 System.InterlockedIncrement(Pointer(node^.version_));

 _pthread_key_queue.Push(node);
end;

function ps4_pthread_getspecific(Key:pthread_key_t):Pointer; SysV_ABI_CDecl;
var
 node:p_pthread_key_node;
 version:ptruint;
 local:p_pthread_key_data;
begin
 if (DWORD(Key)>=SCE_PTHREAD_KEYS_MAX) then Exit(nil);

 node:=@_pthread_key_nodes[Key];

 version:=load_consume(node^.version_);
 if (load_consume(Pointer(node^.dest_))=nil) then Exit(nil);

 local:=@tcb_thread^.keys[Key];

 if (local^.version_<>version) then Exit(nil);

 Result:=local^.data_;
end;

function ps4_pthread_setspecific(Key:pthread_key_t;value:Pointer):Integer; SysV_ABI_CDecl;
var
 node:p_pthread_key_node;
 version:ptruint;
 local:p_pthread_key_data;
begin
 if (DWORD(Key)>=SCE_PTHREAD_KEYS_MAX) then Exit(EINVAL);

 node:=@_pthread_key_nodes[Key];

 version:=load_consume(node^.version_);
 if (load_consume(Pointer(node^.dest_))=nil) then Exit(EINVAL);

 local:=@tcb_thread^.keys[Key];

 local^.version_:=version;
 local^.data_   :=value;

 Result:=0;
end;

initialization
 _pthread_keys_init;

end.


