unit ps4_pthread;

{$mode objfpc}{$H+}

interface

uses
 windows,
 sys_crt,
 sys_pthread,
 sys_signal;

function  ps4_scePthreadAttrGet(pid:pthread;pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;

function  ps4_pthread_create_name_np(pthread:p_pthread;pAttr:p_pthread_attr_t;entry:Pointer;arg:Pointer;name:Pchar):Integer; SysV_ABI_CDecl;
function  ps4_pthread_create(pthread:p_pthread;pAttr:p_pthread_attr_t;entry:Pointer;arg:Pointer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadCreate(pthread:p_pthread;pAttr:p_pthread_attr_t;entry:Pointer;arg:Pointer;name:Pchar):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadDetach(_pthread:pthread):Integer; SysV_ABI_CDecl;
function  ps4_pthread_detach(_pthread:pthread):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadJoin(_pthread:pthread;value:PPointer):Integer; SysV_ABI_CDecl;
function  ps4_pthread_join(_pthread:pthread;value:PPointer):Integer; SysV_ABI_CDecl;

function  ps4_pthread_once(once_control:p_pthread_once_t;init_routine:t_init_routine_proc):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadOnce(once_control:p_pthread_once_t;init_routine:t_init_routine_proc):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadEqual(t1,t2:pthread):Integer; SysV_ABI_CDecl;
function  ps4_pthread_equal(t1,t2:pthread):Integer; SysV_ABI_CDecl;

procedure ps4_scePthreadExit(value_ptr:Pointer); SysV_ABI_CDecl;
procedure ps4_pthread_exit(value_ptr:Pointer); SysV_ABI_CDecl;

function  ps4_scePthreadCancel(_pthread:pthread):Integer; SysV_ABI_CDecl;

function  ps4_pthread_setcancelstate(state:Integer;oldstate:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_pthread_setcanceltype (_type:Integer;oldtype:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadSetcancelstate(state:Integer;oldState:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadSetcanceltype(_type:Integer;oldType:PInteger):Integer; SysV_ABI_CDecl;

function  ps4_pthread_self():pthread; SysV_ABI_CDecl;
function  ps4_scePthreadSelf():pthread; SysV_ABI_CDecl;

function  ps4_getpid():Integer; SysV_ABI_CDecl;
function  ps4_scePthreadGetthreadid():Integer; SysV_ABI_CDecl;

function  ps4_pthread_getname_np(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadGetname(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;

function  ps4_pthread_rename_np(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadRename(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;

procedure ps4_pthread_set_name_np(_pthread:pthread;name:Pchar); SysV_ABI_CDecl;
procedure ps4_scePthreadSetName(_pthread:pthread;name:Pchar); SysV_ABI_CDecl;

function  ps4_scePthreadSetaffinity(_pthread:pthread;mask:QWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadGetaffinity(_pthread:pthread;mask:PQWORD):Integer; SysV_ABI_CDecl;

function  ps4_sceKernelGetCurrentCpu():Integer; SysV_ABI_CDecl;

function  ps4_pthread_getprio(_pthread:pthread):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadGetprio(_pthread:pthread;prio:PInteger):Integer; SysV_ABI_CDecl;

function  ps4_pthread_setprio(_pthread:pthread;prio:Integer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadSetprio(_pthread:pthread;prio:Integer):Integer; SysV_ABI_CDecl;

function  ps4_pthread_getschedparam(_pthread:pthread;policy:PInteger;param:PSceKernelSchedParam):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadGetschedparam(_pthread:pthread;policy:PInteger;param:PSceKernelSchedParam):Integer; SysV_ABI_CDecl;

function  ps4_pthread_setschedparam(_pthread:pthread;policy:Integer;param:PSceKernelSchedParam):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadSetschedparam(_pthread:pthread;policy:Integer;param:PSceKernelSchedParam):Integer; SysV_ABI_CDecl;

function  ps4_sched_get_priority_max(policy:Integer):Integer; SysV_ABI_CDecl;
function  ps4_sched_get_priority_min(policy:Integer):Integer; SysV_ABI_CDecl;

procedure ps4_scePthreadYield; SysV_ABI_CDecl;
function  ps4_pthread_yield:Integer; SysV_ABI_CDecl;
function  ps4_sched_yield:Integer; SysV_ABI_CDecl;

procedure ps4_pthread_cleanup_push(routine:t_cb_proc;arg:Pointer); SysV_ABI_CDecl;
procedure ps4_pthread_cleanup_pop(execute:Integer); SysV_ABI_CDecl;
procedure ps4___pthread_cleanup_push_imp(routine:t_cb_proc;
                                         arg:Pointer;
                                         info:p_pthread_cleanup); SysV_ABI_CDecl;
procedure ps4___pthread_cleanup_pop_imp(execute:Integer); SysV_ABI_CDecl;

function  _pthread_run_entry(pthread:p_pthread;name:Pchar;stack:PDWORD):Integer;

implementation

uses
 atomic,
 spinlock,
 sys_kernel,
 ps4_pthread_key,
 ps4_mutex,
 ps4_map_mm,
 ps4_program,
 ps4_elf,
 ps4_pthread_attr;

procedure _free_pthread(data:pthread);
begin
 _sig_lock;
 if (data^.handle<>0) then
 begin
  System.CloseThread(data^.handle);
 end;
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

procedure _pthread_cleanup_pop; inline;
var
 curthread:pthread;
begin
 curthread:=_get_curthread;
 if (curthread=nil) then Exit;

 While (curthread^.cleanup<>nil) do ps4___pthread_cleanup_pop_imp(1);
end;

procedure _thread_cleanup;
begin
 _pthread_cleanup_pop;

 if (sceKernelThreadDtors<>nil) then
 begin
  sceKernelThreadDtors();
 end;

 _sig_lock;
 _thread_cleanupspecific;
 ps4_app.FreeThread;
 UnRegistredStack;
 _sig_unlock;
end;

function on_ps4_run_entry(arg:Pointer):Pointer; SysV_ABI_CDecl;
begin
 Result:=nil;
 ps4_app.InitThread(1);
 ps4_app.InitCode;
 Telf_file(ps4_app.prog).mapCodeEntry;
 writeln('--[END]--');
end;

function _pthread_run_entry(pthread:p_pthread;name:Pchar;stack:PDWORD):Integer;
var
 init:Integer;
 attr:pthread_attr_t;
begin
 if (name=nil) then
 begin
  name:=default_name;
 end;
 init:=THR_STACK_INITIAL;
 if (stack=nil) then
 begin
  stack:=@init;
 end else
 if (stack^<THR_STACK_INITIAL) then
 begin
  stack:=@init;
 end;

 ps4_pthread_attr_init(@attr);
 ps4_pthread_attr_setstacksize(@attr,stack^);
 Result:=ps4_scePthreadCreate(pthread,@attr,@on_ps4_run_entry,nil,name);
 ps4_pthread_attr_destroy(@attr);
end;

const
 _PREPARE_FREE=2;
 _PREPARE_JOIN=3;

function on_ps4_run_thread(data:pthread):Longint; stdcall;
type
 Tps4entry=function(arg:Pointer):Pointer; SysV_ABI_CDecl;

var
 base:Pointer;
begin
 Result:=0;

 sys_crt_init;

 //StackTop    - Max
 //StackBottom - Min

 StackLength:=data^.Attr.stacksize_attr;
 Assert(StackLength<>0);

 StackBottom:=StackTop-StackLength;

 //Writeln('StackTop   :',HexStr(StackTop));
 //Writeln('Sptr       :',HexStr(Sptr));
 //
 //Writeln('StackBottom:',HexStr(StackBottom));
 //Writeln('StackLength:',HexStr(StackLength,16));

 ReadBarrier;
 if (data<>nil) and (data^.entry<>nil) then
 begin
  //if (data^.Attr.stackaddr_attr=nil) then
  //begin
   data^.Attr.stackaddr_attr:=StackBottom;
   data^.Attr.stacksize_attr:=StackLength;
  //end;

  tcb_thread:=data;
  SetThreadDebugName(data^.ThreadId,'ps4:'+data^.name);
  WriteLn(SysLogPrefix, 'BeginThread:',HexStr(data^.entry));
  _thread_init;

  wait_until_equal(data^.handle,0);

  //init static tls
  if (Telf_file(ps4_program.ps4_app.prog).pTls.full_size<>0) then
  begin
   base:=Telf_file(ps4_program.ps4_app.prog)._get_tls;
   SetTlsBase(base);
   Assert(GetTlsBase=base);
  end;
  //init static tls

  //data^.arg:=Tps4entry(data^.entry)(data^.arg);
  data^.arg:=sysv_wrapper(data^.arg,data^.entry);
  ReadWriteBarrier;

  _thread_cleanup;
  writeln(SysLogPrefix,'EndThread:',data^.name);

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

function ps4_pthread_create_name_np(pthread:p_pthread;pAttr:p_pthread_attr_t;entry:Pointer;arg:Pointer;name:Pchar):Integer; SysV_ABI_CDecl;
Var
 attr:pthread_attr_t;
 data:pthread;
 Handle,ThreadId:TThreadID;
 sa:Pointer;
 ss:SizeUInt;
 creationFlags:dword;
begin
 Writeln(SysLogPrefix, 'pthread_create:',HexStr(entry),' ',name);

 Result:=EINVAL;
 if (pthread=nil) then Exit;

 begin
  data:=SwAllocMem(SizeOf(pthread_t));
  if (data=nil) then Exit(ENOMEM);

  sigqueue_init(@data^.sig);

  data^.entry:=entry;
  data^.arg:=arg;
  if (name<>nil) then MoveChar0(name^,data^.name,32);

  ReadWriteBarrier;

  attr:=nil;
  if (pAttr<>nil) then
  begin
   attr:=pAttr^;
  end;
  if (attr=nil) then
  begin
   attr:=@_pthread_attr_default;
  end;

  data^.Attr:=attr^;
  data^.detachstate:=attr^.flags;
  ReadWriteBarrier;

  creationFlags:=0;
  sa:=attr^.stackaddr_attr;
  ss:=attr^.stacksize_attr;

  if (ss<PTHREAD_STACK_MIN) then
  begin
   ss:=PTHREAD_STACK_MIN;
   data^.Attr.stacksize_attr:=ss;
  end;

  if (attr^.prio>767) or (attr^.prio<256) then //if not valid set default
  begin
   data^.Attr.prio:=SCE_KERNEL_PRIO_FIFO_DEFAULT;
  end;

  ThreadId:=0;

  _sig_lock;
   Handle:=SysBeginThread(sa,ss,@on_ps4_run_thread,data,creationFlags,ThreadId);
  _sig_unlock;

  if (Handle=0) then
  begin
   SwFreeMem(data);
   Exit(EAGAIN);
  end;

   if (data^.Attr.cpuset<>0) then
   begin
    _sig_lock;
     SetThreadAffinityMask(Handle,attr^.cpuset);
    _sig_unlock;
   end;
   sys_set_thread_prior(Handle,data^.Attr.prio);

 end;

 XCHG(data^.ThreadId,ThreadId);
 XCHG(data^.handle,Handle);

 pthread^:=data;

 Result:=0;
end;

function ps4_pthread_create(pthread:p_pthread;pAttr:p_pthread_attr_t;entry:Pointer;arg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_pthread_create_name_np(pthread,pAttr,entry,arg,nil);
end;

function ps4_scePthreadCreate(pthread:p_pthread;pAttr:p_pthread_attr_t;entry:Pointer;arg:Pointer;name:Pchar):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_create_name_np(pthread,pAttr,entry,arg,name));
end;

function ps4_scePthreadDetach(_pthread:pthread):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Writeln('scePthreadDetach:',_pthread^.name);
 if CAS(_pthread^.detachstate,PTHREAD_CREATE_JOINABLE,PTHREAD_CREATE_DETACHED) then
 begin
  _pthread^.Attr.flags:=PTHREAD_CREATE_DETACHED;
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
 Writeln(SysLogPrefix, 'scePthreadJoin:',_pthread^.name);

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

function ps4_pthread_join(_pthread:pthread;value:PPointer):Integer; SysV_ABI_CDecl;
begin
 Result:=sce2px(ps4_scePthreadJoin(_pthread,value));
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
 Result:=px2sce(ps4_pthread_equal(t1,t2));
end;

function ps4_pthread_equal(t1,t2:pthread):Integer; SysV_ABI_CDecl;
begin
 Result:=ord(t1=t2);
end;

function ps4_scePthreadCancel(_pthread:pthread):Integer; SysV_ABI_CDecl;
begin
 if _pthread=nil then
  Exit(SCE_KERNEL_ERROR_ESRCH);
 Writeln(SysLogPrefix, 'scePthreadCancel');
 //Dirty thread termination
 if CAS(_pthread^.detachstate,PTHREAD_CREATE_DETACHED,_PREPARE_FREE) then
 begin
  _free_pthread(_pthread);
 end else
 begin
  CAS(_pthread^.detachstate,PTHREAD_CREATE_JOINABLE,_PREPARE_JOIN);
  _sig_lock;
  Windows.TerminateThread(_pthread^.handle,0);
  _pthread^.handle:=0;
  _sig_unlock;
  //_free_pthread(_pthread);
 end;
 Result:=0;
end;

procedure ps4_scePthreadExit(value_ptr:Pointer); SysV_ABI_CDecl;
var
 data:pthread;
begin
 data:=tcb_thread;
 if (data=nil) then Exit;
 Writeln(SysLogPrefix, 'ExitThread');
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

function ps4_pthread_setcancelstate(state:Integer;oldstate:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 Case state of
  PTHREAD_CANCEL_DISABLE:{Writeln('PTHREAD_CANCEL_DISABLE')};
  PTHREAD_CANCEL_ENABLE :{Writeln('PTHREAD_CANCEL_ENABLE')};
  else
   Exit(EINVAL);
 end;
end;

function ps4_pthread_setcanceltype(_type:Integer;oldtype:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 Case _type of
  PTHREAD_CANCEL_DEFERRED    :;
  PTHREAD_CANCEL_ASYNCHRONOUS:;
  else
   Exit(EINVAL);
 end;
end;

function ps4_scePthreadSetcancelstate(state:Integer;oldState:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_setcancelstate(state,oldState));
end;

function ps4_scePthreadSetcanceltype(_type:Integer;oldType:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_setcanceltype(_type,oldType));
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

function ps4_scePthreadGetthreadid():Integer; SysV_ABI_CDecl;
begin
 Result:=tcb_thread^.ThreadId;
end;

function ps4_pthread_getname_np(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) or (name=nil) then Exit(EINVAL);
 MoveChar0(_pthread^.name,name^,32);
 Result:=0;
end;

function ps4_scePthreadGetname(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_getname_np(_pthread,name));
end;

function ps4_pthread_rename_np(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) then Exit(EINVAL);
 FillChar(_pthread^.name,32,0);
 if (name<>nil) then
 begin
  MoveChar0(name^,_pthread^.name,32);
 end;
 SetThreadDebugName(_pthread^.ThreadId,'ps4:'+_pthread^.name);
 Result:=0;
end;

function ps4_scePthreadRename(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_rename_np(_pthread,name));
end;

procedure ps4_pthread_set_name_np(_pthread:pthread;name:Pchar); SysV_ABI_CDecl;
begin
 if (_pthread=nil) then Exit;
 FillChar(_pthread^.name,32,0);
 if (name<>nil) then
 begin
  MoveChar0(name^,_pthread^.name,32);
 end;
 SetThreadDebugName(_pthread^.ThreadId,'ps4:'+_pthread^.name);
end;

procedure ps4_scePthreadSetName(_pthread:pthread;name:Pchar); SysV_ABI_CDecl;
begin
 ps4_pthread_set_name_np(_pthread,name);
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

function GetCurrentProcessorNumber():DWORD; stdcall external 'kernel32';

function ps4_sceKernelGetCurrentCpu():Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=GetCurrentProcessorNumber;
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

function ps4_pthread_getprio(_pthread:pthread):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) then Exit(-1);
 Result:=_pthread^.Attr.prio;
end;

function ps4_scePthreadGetprio(_pthread:pthread;prio:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) or (prio=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 prio^:=_pthread^.Attr.prio;
 Result:=0;
end;

function ps4_pthread_setprio(_pthread:pthread;prio:Integer):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) then Exit(EINVAL);

 if (_pthread^.Attr.sched_policy=SCHED_OTHER) or
    (_pthread^.Attr.prio=prio) then
 begin
  _pthread^.Attr.prio:=prio;
  Exit(0);
 end;

 Result:=sys_set_thread_prior(_pthread^.handle,prio);
 if (Result<>0) then
 begin
  _pthread^.Attr.prio:=prio;
 end;
end;

function ps4_scePthreadSetprio(_pthread:pthread;prio:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_setprio(_pthread,prio));
end;

function ps4_pthread_getschedparam(_pthread:pthread;policy:PInteger;param:PSceKernelSchedParam):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) or (policy=nil) or (param=nil) then Exit(EINVAL);

 policy^:=_pthread^.Attr.sched_policy;
 param^.sched_priority:=_pthread^.Attr.prio;
 Result:=0;
end;

function ps4_scePthreadGetschedparam(_pthread:pthread;policy:PInteger;param:PSceKernelSchedParam):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_getschedparam(_pthread,policy,param));
end;

function ps4_pthread_setschedparam(_pthread:pthread;policy:Integer;param:PSceKernelSchedParam):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) or (param=nil) then Exit(EINVAL);

 if (_pthread^.Attr.sched_policy=policy) and
    ((policy=SCHED_OTHER) or (_pthread^.Attr.prio=param^.sched_priority)) then
 begin
  _pthread^.Attr.prio:=param^.sched_priority;
  Exit(0);
 end;

 Result:=sys_set_thread_prior(_pthread^.handle,param^.sched_priority);
 if (Result<>0) then
 begin
  _pthread^.Attr.sched_policy:=policy;
  _pthread^.Attr.prio:=param^.sched_priority;
 end;
end;

function ps4_scePthreadSetschedparam(_pthread:pthread;policy:Integer;param:PSceKernelSchedParam):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_setschedparam(_pthread,policy,param));
end;

function ps4_sched_get_priority_max(policy:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_PRIO_FIFO_HIGHEST;
end;

function ps4_sched_get_priority_min(policy:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_PRIO_FIFO_LOWEST;
end;

procedure ps4_scePthreadYield; SysV_ABI_CDecl;
begin
 SwYieldExecution;
end;

function ps4_pthread_yield:Integer; SysV_ABI_CDecl;
begin
 SwYieldExecution;
 Result:=0;
end;

function ps4_sched_yield:Integer; SysV_ABI_CDecl;
begin
 SwYieldExecution;
 Result:=0;
end;

procedure ps4_pthread_cleanup_push(routine:t_cb_proc;arg:Pointer); SysV_ABI_CDecl;
var
 curthread:pthread;
 newbuf:p_pthread_cleanup;
begin
 Writeln('pthread_cleanup_push');

 curthread:=_get_curthread;
 if (curthread=nil) then Exit;

 newbuf:=AllocMem(SizeOf(pthread_cleanup));
 if (newbuf=nil) then Exit;

 newbuf^.routine    :=routine;
 newbuf^.routine_arg:=arg;
 newbuf^.onheap     :=1;
 newbuf^.prev       :=curthread^.cleanup;

 curthread^.cleanup:=newbuf;
end;

procedure ps4_pthread_cleanup_pop(execute:Integer); SysV_ABI_CDecl;
begin
 ps4___pthread_cleanup_pop_imp(execute);
end;

procedure ps4___pthread_cleanup_push_imp(routine:t_cb_proc;
                                         arg:Pointer;
                                         info:p_pthread_cleanup); SysV_ABI_CDecl;
var
 curthread:pthread;
begin
 //Writeln('__pthread_cleanup_push_imp');

 curthread:=_get_curthread;
 if (curthread=nil) then Exit;

 info^.routine    :=routine;
 info^.routine_arg:=arg;
 info^.onheap     :=0;
 info^.prev       :=curthread^.cleanup;

 curthread^.cleanup:=info;
end;

procedure ps4___pthread_cleanup_pop_imp(execute:Integer); SysV_ABI_CDecl;
var
 curthread:pthread;
 old:p_pthread_cleanup;
begin
 //Writeln('__pthread_cleanup_pop_imp');

 curthread:=_get_curthread;
 if (curthread=nil) then Exit;

 old:=curthread^.cleanup;
 if (old<>nil) then
 begin
  curthread^.cleanup:=old^.prev;
  if (execute<>0) then
  begin
   old^.routine(old^.routine_arg);
  end;
  if (old^.onheap<>0) then
  begin
   FreeMem(old);
  end;
 end;
end;


end.


