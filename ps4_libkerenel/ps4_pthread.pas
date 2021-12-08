unit ps4_pthread;

{$mode objfpc}{$H+}

interface

uses
 windows,
 Classes, SysUtils;

type
 Ppthread_attr_t=^pthread_attr_t;
 pthread_attr_t=^tthread_attr_t;
 tthread_attr_t=packed record
  policy:Integer;
  sched_priority:Integer;
  //prio          :Integer;
  suspend       :Integer;
  flags         :Integer;
  stackaddr_attr:Pointer;
  stacksize_attr:size_t;
  cpuset:QWORD;
  //guardsize_attr:size_t;
  //cpuset        :Pointer;//cpuset_t
  //cpusetsize    :size_t;
  detachstate:Integer;
 end;

 Ppthread=^pthread;
 pthread=^pthread_t;
 pthread_t=record
  entry:Pointer;
  arg:Pointer;
  handle:TThreadID;
  detachstate:Integer;
  name:array[0..31] of AnsiChar;
 end;

const
//Run-time invariant values:
 PTHREAD_STACK_MIN=4*1024;

 PTHREAD_DETACHED     =$1;
 PTHREAD_SCOPE_SYSTEM =$2;
 PTHREAD_INHERIT_SCHED=$4;
 PTHREAD_NOFLOAT      =$8;

 PTHREAD_CREATE_DETACHED=PTHREAD_DETACHED;
 PTHREAD_CREATE_JOINABLE=0;

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

const
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

procedure ps4_pthread_cxa_finalize(P:Pointer); SysV_ABI_CDecl;

function  ps4_scePthreadAttrInit(pAttr:Ppthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrDestroy(pAttr:Ppthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetstacksize(pAttr:Ppthread_attr_t;size:size_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetdetachstate(pAttr:Ppthread_attr_t;detachstate:Integer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetschedpolicy(pAttr:Ppthread_attr_t;policy:Integer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetschedparam(pAttr:Ppthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetaffinity(pAttr:Ppthread_attr_t;mask:QWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetinheritsched(pAttr:Ppthread_attr_t;inheritSched:Integer):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadCreate(pthread:Ppthread;pAttr:Ppthread_attr_t;entry:Pointer;arg:Pointer;name:Pchar):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadDetach(_pthread:pthread):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadJoin(_pthread:pthread;value:PPointer):Integer; SysV_ABI_CDecl;
procedure ps4_scePthreadExit(value_ptr:Pointer); SysV_ABI_CDecl;
function  ps4_pthread_self():pthread; SysV_ABI_CDecl;
function  ps4_scePthreadSelf():pthread; SysV_ABI_CDecl;

function  ps4_scePthreadGetname(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadRename(_pthread:pthread;name:Pchar):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadSetaffinity(_pthread:pthread;mask:QWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadGetprio(_pthread:pthread;prio:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadSetprio(_pthread:pthread;prio:Integer):Integer; SysV_ABI_CDecl;
procedure ps4_scePthreadYield(); SysV_ABI_CDecl;

procedure _pthread_run_entry(pthread:Ppthread);

implementation

uses
 ps4_map_mm,
 ps4_program,
 ps4_elf,
 ps4_libkernel;

//struct dl_phdr_info
procedure ps4_pthread_cxa_finalize(P:Pointer); SysV_ABI_CDecl;
begin
 Writeln('__pthread_cxa_finalize');
end;

function ps4_scePthreadAttrInit(pAttr:Ppthread_attr_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('scePthreadAttrInit');
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 pAttr^:=AllocMem(SizeOf(tthread_attr_t));
 Result:=0;
end;

function ps4_scePthreadAttrDestroy(pAttr:Ppthread_attr_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('scePthreadAttrDestroy');
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 FreeMem(System.InterlockedExchange(pAttr^,nil));
 Result:=0;
end;

function ps4_scePthreadAttrSetstacksize(pAttr:Ppthread_attr_t;size:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 if (pAttr^=nil) then Exit;
 pAttr^^.stacksize_attr:=size;
 Result:=0;
end;

function ps4_scePthreadAttrSetdetachstate(pAttr:Ppthread_attr_t;detachstate:Integer):Integer; SysV_ABI_CDecl;
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

function ps4_scePthreadAttrSetschedpolicy(pAttr:Ppthread_attr_t;policy:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 if (pAttr^=nil) then Exit;
 pAttr^^.policy:=policy;
 Result:=0;
end;

function ps4_scePthreadAttrSetschedparam(pAttr:Ppthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) or (param=nil) then Exit;
 if (pAttr^=nil) then Exit;
 pAttr^^.sched_priority:=param^;
 Result:=0;
end;

function ps4_scePthreadAttrSetaffinity(pAttr:Ppthread_attr_t;mask:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 if (pAttr^=nil) then Exit;
 pAttr^^.cpuset:=mask;
 Result:=0;
end;

function ps4_scePthreadAttrSetinheritsched(pAttr:Ppthread_attr_t;inheritSched:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function CAS(Var addr:Integer;Comp,New:Integer):Boolean; inline;
begin
 Result:=System.InterlockedCompareExchange(addr,New,Comp)=Comp;
end;

procedure _free_pthread(data:pthread);
begin
 System.CloseThread(data^.handle);
 FreeMem(data);
end;

threadvar
 _pthread_self:pthread;

function on_ps4_run_entry(data:pthread):ptrint;
type
 Tps4entry=function(arg:Pointer):Pointer; SysV_ABI_CDecl;
begin
 Result:=0;
 RegistredStack;
 ps4_app.InitThread;

 While (System.InterLockedExchangeAdd(data^.handle,0)=0) do System.ThreadSwitch;
 _pthread_self:=data;

 ps4_app.InitCode;
 Telf_file(ps4_app.prog).mapCodeEntry;

 ps4_app.FreeThread;
 UnRegistredStack;
end;

procedure _pthread_run_entry(pthread:Ppthread);
Var
 data:pthread;
 Handle,ThreadId:TThreadID;
begin
 if (pthread=nil) then Exit;

 data:=AllocMem(SizeOf(pthread_t));
 if (data=nil) then Exit;

 ThreadId:=0;
 Handle:=BeginThread(TThreadFunc(@on_ps4_run_entry),data,ThreadId);
 if (Handle=0) then
 begin
  FreeMem(data);
  Exit;
 end;

 System.InterlockedExchange(data^.handle,Handle);

 pthread^:=data;
end;

const
 _PREPARE_FREE=2;
 _PREPARE_JOIN=3;

function on_ps4_run_thread(data:pthread):ptrint;
type
 Tps4entry=function(arg:Pointer):Pointer; SysV_ABI_CDecl;
begin
 Result:=0;
 ReadBarrier;
 if (data<>nil) and (data^.entry<>nil) then
 begin
  writeln('BeginThread:',data^.name,':',HexStr(data^.entry));
  RegistredStack;
  ps4_app.InitThread;

  While (System.InterLockedExchangeAdd(data^.handle,0)=0) do System.ThreadSwitch;
  _pthread_self:=data;

  data^.arg:=Tps4entry(data^.entry)(data^.arg);
  ReadWriteBarrier;

  ps4_app.FreeThread;
  UnRegistredStack;
  writeln('EndThread:',data^.name);

  if CAS(data^.detachstate,PTHREAD_CREATE_DETACHED,_PREPARE_FREE) then
  begin
   _free_pthread(data);
  end else
  begin
   CAS(data^.detachstate,PTHREAD_CREATE_JOINABLE,_PREPARE_JOIN);
  end;

 end;
end;

//typedef pthread_t ScePthread;

function ps4_scePthreadCreate(pthread:Ppthread;pAttr:Ppthread_attr_t;entry:Pointer;arg:Pointer;name:Pchar):Integer; SysV_ABI_CDecl;
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
  data:=AllocMem(SizeOf(pthread_t));
  if (data=nil) then Exit(SCE_KERNEL_ERROR_ENOMEM);
  data^.entry:=entry;
  data^.arg:=arg;
  if (name<>nil) then MoveChar0(name^,data^.name,32);

  ReadWriteBarrier;

  if (pAttr<>nil) and (pAttr^<>nil) then
  begin
   data^.detachstate:=pAttr^^.detachstate;
   ReadWriteBarrier;

   creationFlags:=0;
   sa:=pAttr^^.stackaddr_attr;
   ss:=pAttr^^.stacksize_attr;

   if (ss<DefaultStackSize) then ss:=DefaultStackSize;

   ThreadId:=0;
   Handle:=BeginThread(sa,ss,TThreadFunc(@on_ps4_run_thread),data,creationFlags,ThreadId);
   if (Handle=0) then
   begin
    FreeMem(data);
    Exit(SCE_KERNEL_ERROR_EAGAIN);
   end;

   if (pAttr^^.cpuset<>0) then
   begin
    SetThreadAffinityMask(Handle,pAttr^^.cpuset);
   end;

  end else
  begin
   ThreadId:=0;
   Handle:=BeginThread(TThreadFunc(@on_ps4_run_thread),data,ThreadId);
   if (Handle=0) then
   begin
    FreeMem(data);
    Exit(SCE_KERNEL_ERROR_EAGAIN);
   end;
  end;

  System.InterlockedExchange(data^.handle,Handle);

  pthread^:=data;
 end;

 Result:=0;
end;

function ps4_scePthreadDetach(_pthread:pthread):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Writeln('scePthreadDetach:',_pthread^.name);
 if CAS(_pthread^.detachstate,PTHREAD_CREATE_JOINABLE,PTHREAD_CREATE_DETACHED) then
 begin
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

function ps4_scePthreadJoin(_pthread:pthread;value:PPointer):Integer; SysV_ABI_CDecl;
begin
 if (_pthread=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Writeln('scePthreadJoin:',_pthread^.name);

 if CAS(_pthread^.detachstate,PTHREAD_CREATE_JOINABLE,_PREPARE_FREE) then
 begin
  System.WaitForThreadTerminate(_pthread^.handle,INFINITE);
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

procedure ps4_scePthreadExit(value_ptr:Pointer); SysV_ABI_CDecl;
var
 data:pthread;
begin
 data:=_pthread_self;
 if (data=nil) then Exit;
 Writeln('ExitThread:',data^.name);
 data^.arg:=value_ptr;
 ReadWriteBarrier;
 ps4_app.FreeThread;
 UnRegistredStack;

 if CAS(data^.detachstate,PTHREAD_CREATE_DETACHED,_PREPARE_FREE) then
 begin
  _free_pthread(data);
 end else
 begin
  CAS(data^.detachstate,PTHREAD_CREATE_JOINABLE,_PREPARE_JOIN);
 end;

 System.EndThread(0);
end;

function ps4_pthread_self():pthread; SysV_ABI_CDecl;
begin
 Result:=_pthread_self;
end;

function ps4_scePthreadSelf():pthread; SysV_ABI_CDecl;
begin
 Result:=_pthread_self;
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
 if (_pthread=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Result:=0;
 SetThreadAffinityMask(_pthread^.handle,mask);
end;

//ThreadGetPriority = -15 and 15. :0..30
//scePthreadGetprio = 767 and 256 :0..511
function ps4_scePthreadGetprio(_pthread:pthread;prio:PInteger):Integer; SysV_ABI_CDecl;
Var
 r:Integer;
begin
 if (_pthread=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if (prio=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 r:=System.ThreadGetPriority(_pthread^.handle);
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
 r:=(((767-prio)*30) div 511)-15;

 Result:=0;
 if not System.ThreadSetPriority(_pthread^.handle,r) then Result:=SCE_KERNEL_ERROR_ESRCH;
end;

procedure ps4_scePthreadYield(); SysV_ABI_CDecl;
begin
 System.ThreadSwitch;
end;

end.

