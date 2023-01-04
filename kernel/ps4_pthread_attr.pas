unit ps4_pthread_attr;

{$mode ObjFPC}{$H+}

interface

uses
 atomic,
 sys_kernel,
 sys_pthread;

const
 default_name:Pchar='main';

var
 _pthread_attr_default:pthread_attr=(
  sched_policy  :SCHED_OTHER;
  sched_inherit :PTHREAD_INHERIT_SCHED;
  prio          :0;
  suspend       :THR_CREATE_RUNNING;
  flags         :PTHREAD_SCOPE_SYSTEM;
  stackaddr_attr:nil;
  stacksize_attr:$10000;
  guardsize_attr:0;
  cpuset        :0
  //cpusetsize = 0,
  //cpuset = NULL
 );

function  ps4_pthread_set_defaultstacksize_np(size:QWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadSetDefaultstacksize(size:QWORD):Integer; SysV_ABI_CDecl;

function  ps4_pthread_attr_init(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_pthread_attr_destroy(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadAttrInit(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrDestroy(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;

function  ps4_pthread_attr_setstacksize(pAttr:p_pthread_attr_t;size:size_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetstacksize(pAttr:p_pthread_attr_t;size:size_t):Integer; SysV_ABI_CDecl;

function  ps4_pthread_attr_setdetachstate(pAttr:p_pthread_attr_t;detachstate:Integer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetdetachstate(pAttr:p_pthread_attr_t;detachstate:Integer):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadAttrSetschedpolicy(pAttr:p_pthread_attr_t;policy:Integer):Integer; SysV_ABI_CDecl;

function  ps4_pthread_attr_setschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrSetschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;

function  ps4_pthread_attr_getschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadAttrSetaffinity(pAttr:p_pthread_attr_t;mask:QWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetaffinity(pAttr:p_pthread_attr_t;mask:PQWORD):Integer; SysV_ABI_CDecl;

function  ps4_scePthreadAttrSetinheritsched(pAttr:p_pthread_attr_t;inheritSched:Integer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetguardsize(pAttr:p_pthread_attr_t;guardSize:PQWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetstackaddr(pAttr:p_pthread_attr_t;stackAddr:PPointer):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetstacksize(pAttr:p_pthread_attr_t;stackSize:PQWORD):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetstack(pAttr:p_pthread_attr_t;stackAddr:PPointer;stackSize:PQWORD):Integer; SysV_ABI_CDecl;

function  ps4_pthread_attr_getdetachstate(pAttr:p_pthread_attr_t;detachstate:Pinteger):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadAttrGetdetachstate(pAttr:p_pthread_attr_t;detachstate:Pinteger):Integer; SysV_ABI_CDecl;

implementation

function ps4_pthread_set_defaultstacksize_np(size:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=EINVAL;
 if (size>PTHREAD_STACK_MIN) then
 begin
  _pthread_attr_default.stacksize_attr:=size;
  Result:=0;
 end;
end;

function ps4_scePthreadSetDefaultstacksize(size:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_set_defaultstacksize_np(size));
end;

function ps4_pthread_attr_init(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix, 'pthread_attr_init');
 if (pAttr=nil) then Exit(EINVAL);
 pAttr^:=SwAllocMem(SizeOf(pthread_attr));
 if (pAttr^=nil) then Exit(ENOMEM);
 pAttr^^:=_pthread_attr_default;
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

function ps4_scePthreadAttrInit(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_attr_init(pAttr));
end;

function ps4_scePthreadAttrDestroy(pAttr:p_pthread_attr_t):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_attr_destroy(pAttr));
end;

function ps4_pthread_attr_setstacksize(pAttr:p_pthread_attr_t;size:size_t):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 if (pAttr^=nil) then Exit(EINVAL);
 if (size<PTHREAD_STACK_MIN) then Exit(EINVAL);
 pAttr^^.stacksize_attr:=size;
 Result:=0;
end;

function ps4_scePthreadAttrSetstacksize(pAttr:p_pthread_attr_t;size:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_attr_setstacksize(pAttr,size));
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

function ps4_scePthreadAttrSetdetachstate(pAttr:p_pthread_attr_t;detachstate:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_attr_setdetachstate(pAttr,detachstate));
end;

function ps4_scePthreadAttrSetschedpolicy(pAttr:p_pthread_attr_t;policy:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (pAttr=nil) then Exit;
 if (pAttr^=nil) then Exit;
 pAttr^^.sched_policy:=policy;
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

function ps4_scePthreadAttrSetschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_attr_setschedparam(pAttr,param));
end;

function ps4_pthread_attr_getschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=EINVAL;
 if (pAttr=nil) or (param=nil) then Exit;
 if (pAttr^=nil) then Exit;
 param^:=pAttr^^.prio;
 Result:=0;
end;

function ps4_scePthreadAttrGetschedparam(pAttr:p_pthread_attr_t;param:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_attr_getschedparam(pAttr,param));
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

function ps4_pthread_attr_getdetachstate(pAttr:p_pthread_attr_t;detachstate:Pinteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (detachstate=nil) then Exit(EINVAL);
 if (pAttr^=nil) then Exit(EINVAL);
 detachstate^:=pAttr^^.flags;
 Result:=0;
end;

function ps4_scePthreadAttrGetdetachstate(pAttr:p_pthread_attr_t;detachstate:Pinteger):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_attr_getdetachstate(pAttr,detachstate));
end;

end.
