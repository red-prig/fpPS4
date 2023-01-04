unit ps4_pthread;

{$mode objfpc}{$H+}

interface

uses
 windows,
 sys_crt,
 sys_pthread,
 sys_signal;

function  ps4_pthread_set_defaultstacksize_np(size:QWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadSetDefaultstacksize(size:QWORD):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadAttrInit(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrDestroy(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_pthread_attr_init(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_pthread_attr_destroy(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetstacksize(pAttr:p_pthread_attr_t;size:size_t):Integer; SysV_ABI_CDecl;
function  ps4_pthread_attr_setstacksize(pAttr:p_pthread_attr_t;size:size_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetdetachstate(pAttr:p_pthread_attr_t;detachstate:Integer):Integer; SysV_ABI_CDecl;
function  ps4_pthread_attr_setdetachstate(pAttr:p_pthread_attr_t;detachstate:Integer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetschedpolicy(pAttr:p_pthread_attr_t;policy:Integer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_pthread_attr_setschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_pthread_attr_getschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetaffinity(pAttr:p_pthread_attr_t;mask:QWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetaffinity(pAttr:p_pthread_attr_t;mask:PQWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetinheritsched(pAttr:p_pthread_attr_t;inheritSched:Integer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetguardsize(pAttr:p_pthread_attr_t;guardSize:PQWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetstackaddr(pAttr:p_pthread_attr_t;stackAddr:PPointer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetstacksize(pAttr:p_pthread_attr_t;stackSize:PQWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetstack(pAttr:p_pthread_attr_t;stackAddr:PPointer;stackSize:PQWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetdetachstate(pAttr:p_pthread_attr_t;detachstate:Pinteger):Integer; SysV_ABI_CDecl;
function  ps4_pthread_attr_getdetachstate(pAttr:p_pthread_attr_t;detachstate:Pinteger):Integer; SysV_ABI_CDecl;

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

function  ps4_pthread_setcancelstate(state:Integer;oldstate:PInteger):Integer; SysV_ABI_CDecl;

function  ps4_pthread_self():pthread; SysV_ABI_CDecl;
function  ps4_scePthreadSelf():pthread; SysV_ABI_CDecl;

function  ps4_getpid():Integer; SysV_ABI_CDecl;
function  ps4_scePthreadGetthreadid():Integer; SysV_ABI_CDecl;

function  ps4_scePthreadGetname(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadRename(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;

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
 ps4_elf;


const
 default_name:Pchar='main';

var
 default_main_stack:Integer=DefaultStackSize;
 default_stack_size:QWORD=$10000;

function ps4_pthread_set_defaultstacksize_np(size:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=EINVAL;
 if (size>PTHREAD_STACK_MIN) then
 begin
  default_stack_size:=size;
  Result:=0;
 end;
end;

function ps4_scePthreadSetDefaultstacksize(size:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_set_defaultstacksize_np(size));
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
 Writeln(SysLogPrefix, 'scePthreadAttrInit');
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 pAttr^:=SwAllocMem(SizeOf(tthread_attr_t));
 if (pAttr^=nil) then Exit(SCE_KERNEL_ERROR_ENOMEM);
 pAttr^^.stacksize_attr:=default_stack_size;
 Result:=0;
end;

function ps4_scePthreadAttrDestroy(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix, 'scePthreadAttrDestroy');
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 SwFreeMem(XCHG(pAttr^,nil));
 Result:=0;
end;

function ps4_pthread_attr_init(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix, 'pthread_attr_init');
 if (pAttr=nil) then Exit(EINVAL);
 pAttr^:=SwAllocMem(SizeOf(tthread_attr_t));
 if (pAttr^=nil) then Exit(ENOMEM);
 pAttr^^.stacksize_attr:=default_stack_size;
 Result:=0;
end;

function ps4_pthread_attr_destroy(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
begin                    
 Writeln(SysLogPrefix, 'pthread_attr_destroy');
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

 pAttr^^.flags:=detachstate;
 Result:=0;
end;

function ps4_pthread_attr_setdetachstate(pAttr:p_pthread_attr_t;detachstate:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=EINVAL;
 if (pAttr=nil) then Exit;
 if (pAttr^=nil) then Exit;

 Case detachstate of
  PTHREAD_CREATE_JOINABLE:;
  PTHREAD_CREATE_DETACHED:;
  else
   Exit(EINVAL);
 end;

 pAttr^^.flags:=detachstate;
 Result:=0;
end;

function ps4_scePthreadAttrSetschedpolicy(pAttr:p_pthread_attr_t;policy:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 if (pAttr^=nil) then Exit;
 pAttr^^.sched_policy:=policy;
 Result:=0;
end;

function ps4_scePthreadAttrSetschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) or (param=nil) then Exit;
 if (pAttr^=nil) then Exit;
 pAttr^^.prio:=param^;
 Result:=0;
end;

function ps4_scePthreadAttrGetschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) or (param=nil) then Exit;
 if (pAttr^=nil) then Exit;
 param^:=pAttr^^.prio;
 Result:=0;
end;

function ps4_pthread_attr_setschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=EINVAL;
 if (pAttr=nil) or (param=nil) then Exit;
 if (pAttr^=nil) then Exit;
 pAttr^^.prio:=param^;
 Result:=0;
end;

function ps4_pthread_attr_getschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=EINVAL;
 if (pAttr=nil) or (param=nil) then Exit;
 if (pAttr^=nil) then Exit;
 param^:=pAttr^^.prio;
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
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 if (pAttr^=nil) then Exit;
 pAttr^^.sched_inherit:=inheritSched;
 Result:=0;
end;

function ps4_scePthreadAttrGetguardsize(pAttr:p_pthread_attr_t;guardSize:PQWORD):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (guardSize=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if (pAttr^=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 guardSize^:=pAttr^^.guardsize_attr;
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
 detachstate^:=pAttr^^.flags;
 Result:=0;
end;

function ps4_pthread_attr_getdetachstate(pAttr:p_pthread_attr_t;detachstate:Pinteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (detachstate=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if (pAttr^=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 detachstate^:=pAttr^^.flags;
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
 attr:pthread_attr_t;
begin
 if (name=nil) then
 begin
  name:=default_name;
 end;
 if (stack=nil) then
 begin
  stack:=@default_main_stack;
 end else
 if (stack^<default_main_stack) then
 begin
  stack:=@default_main_stack;
 end;

 ps4_pthread_attr_init(@attr);
 ps4_pthread_attr_setstacksize(@attr,stack^);
 Result:=ps4_scePthreadCreate(pthread,@attr,@on_ps4_run_entry,nil,name);
 ps4_pthread_attr_destroy(@attr);
end;

function sys_get_prior(handle:TThreadID):Integer;
begin
 Result:=System.ThreadGetPriority(handle);
 Result:=767-(((Result+15)*511) div 30);
end;

const
 _PREPARE_FREE=2;
 _PREPARE_JOIN=3;

procedure SetStackTop(p:Pointer); assembler; nostackframe;
asm
 movq %rax,%gs:(8)
end;

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

  data^.Attr.prio:=sys_get_prior(data^.handle);

  //init static tls in stack top
  if (Telf_file(ps4_program.ps4_app.prog).pTls.full_size<>0) then
  begin
   base:=StackTop-SizeOf(Pointer);
   SetStackTop(base);
   PPointer(base)^:=Telf_file(ps4_program.ps4_app.prog)._get_tls;
  end;
  //init static tls in stack top

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

  if (pAttr<>nil) and (pAttr^<>nil) then
  begin
   data^.Attr:=pAttr^^;
   data^.detachstate:=pAttr^^.flags;
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
    Exit(EAGAIN);
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

function ps4_scePthreadGetname(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) or (name=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 MoveChar0(_pthread^.name,name^,32);
 Result:=0;
end;

function ps4_scePthreadRename(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) or (name=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 MoveChar0(name^,_pthread^.name,32);
 Result:=0;
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

//ThreadGetPriority = -15 and 15. :0..30
//scePthreadGetprio = 767 and 256 :0..511

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
var
 r:Integer;
begin
 if (_pthread=nil) then Exit(EINVAL);

 if (prio>767) then Exit(EINVAL);
 if (prio<256) then Exit(EINVAL);

 r:=(((767-prio)*30) div 511);
 r:=PRIORITY_TABLE[r];

 Result:=0;
 _sig_lock;
 if System.ThreadSetPriority(_pthread^.handle,r) then
 begin
  _pthread^.Attr.prio:=r;
 end else
 begin
  Result:=ESRCH;
 end;
 _sig_unlock;
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

 Result:=ps4_pthread_setprio(_pthread,param^.sched_priority);
 if (Result<>0) then Exit;

 _pthread^.Attr.sched_policy:=policy;
 Result:=0;
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


