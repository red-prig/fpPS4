unit ps4_sema;

{$mode objfpc}{$H+}

interface

uses
 windows,
 sys_types,
 spinlock;

const
 SCE_KERNEL_SEMA_ATTR_TH_FIFO=$01;
 SCE_KERNEL_SEMA_ATTR_TH_PRIO=$02;
 SCE_KERNEL_SEMA_ID_INVALID:Int64=-1;

 SEM_FAILED	=nil;
 SEM_VALUE_MAX	=High(Integer);

type
 pwsem_node=^wsem_node;
 wsem_node=record
  pNext,pPrev:pwsem_node;
  //
  thread:THandle;
  Count:Integer;
  ret:Integer;
 end;

 wsem_list=object
  pHead,pTail:pwsem_node;
  procedure Insert(Node:pwsem_node);
  procedure Remove(node:pwsem_node);
 end;

 PSceKernelSemaOptParam=^TSceKernelSemaOptParam;
 TSceKernelSemaOptParam=packed record
  size:QWORD;
 end;

 PSceKernelSema=^SceKernelSema;
 SceKernelSema=^_sem_t;
 _sem_t=record
  valid:DWORD;
  init :Integer;
  max  :Integer;
  num  :Integer;
  value:Integer;
  refs :DWORD;
  vlock:Pointer;
  list :wsem_list;
  name :array[0..31] of AnsiChar;
 end;


function ps4_sem_init(sem:PSceKernelSema;value:Integer):Integer; SysV_ABI_CDecl;
function ps4_sem_destroy(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;
function ps4_sem_getvalue(sem:PSceKernelSema;sval:PInteger):Integer; SysV_ABI_CDecl;
function ps4_sem_post(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;
function ps4_sem_timedwait(sem:PSceKernelSema;ts:Ptimespec):Integer; SysV_ABI_CDecl;
function ps4_sem_trywait(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;
function ps4_sem_wait(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;

function ps4_sceKernelCreateSema(
          sem:PSceKernelSema;
          name:Pchar;
          attr:DWORD;
          init,max:Integer;
          opt:PSceKernelSemaOptParam):Integer; SysV_ABI_CDecl;
function ps4_sceKernelDeleteSema(sem:SceKernelSema):Integer; SysV_ABI_CDecl;
function ps4_sceKernelWaitSema(sem:SceKernelSema;Count:Integer;pTimeout:PDWORD):Integer; SysV_ABI_CDecl;
function ps4_sceKernelSignalSema(sem:SceKernelSema;Count:Integer):Integer; SysV_ABI_CDecl;
function ps4_sceKernelPollSema(sem:SceKernelSema;Count:Integer):Integer; SysV_ABI_CDecl;
function ps4_sceKernelCancelSema(sem:SceKernelSema;setCount:Integer;threads:PInteger):Integer; SysV_ABI_CDecl;

function do_sema_b_wait(sema:THandle;pTimeout:PQWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;
function do_sema_b_wait_intern(sema:THandle;pTimeout:PQWORD):Integer; inline;

function do_sema_b_release(sema:THandle;count:DWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;

procedure SwEnterCriticalSection(var cs:TRTLCriticalSection);

implementation

//int	 sem_unlink(const char *);

uses
 ntapi,
 atomic,
 sys_kernel,
 sys_pthread,
 sys_signal,
 sys_time,
 ps4_time;

const
 LIFE_SEM=$BAB1F00D;
 DEAD_SEM=$DEADBEEF;

function SwTryEnterCriticalSection(var cs:TRTLCriticalSection):longint;
begin
 _sig_lock;
 Result:=System.TryEnterCriticalSection(cs);
 _sig_unlock;
end;

procedure SwEnterCriticalSection(var cs:TRTLCriticalSection);
var
 ft:TLargeInteger;
begin
 ft:=-10000;
 While (SwTryEnterCriticalSection(cs)=0) do
 begin
  SwDelayExecution(True,@ft);
 end;
end;

function do_sema_b_wait(sema:THandle;pTimeout:PQWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;
var
 v:Integer;
begin
 _sig_lock;
 SwEnterCriticalSection(cs);
 System.InterlockedDecrement(val);
 v:=val;
 System.LeaveCriticalSection(cs);
 if (v>=0) then
 begin
  _sig_unlock;
  Exit(0);
 end;
 Result:=SwWaitFor(sema,pTimeout);
 SwEnterCriticalSection(cs);
 if (Result<>0) then
 begin
  System.InterlockedIncrement(val);
 end;
 System.LeaveCriticalSection(cs);
 _sig_unlock;
end;

function do_sema_b_wait_intern(sema:THandle;pTimeout:PQWORD):Integer; inline;
begin
 Result:=SwWaitFor(sema,pTimeout);
end;

function _rel_wait_count(waiters_count,count:Integer):Integer; inline;
begin
 if (waiters_count<count) then
  Result:=waiters_count
 else
  Result:=count;
end;

function __do_sema_b_release(sema:THandle;count:DWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;
var
 waiters_count:Integer;
begin
 SwEnterCriticalSection(cs);
 if (Int64(val)+Int64(count))>$7fffffff then
 begin
  System.LeaveCriticalSection(cs);
  Exit(EINVAL);
 end;
 waiters_count:=-val;
 System.InterlockedExchangeAdd(val,count);

 if (waiters_count<=0) then
 begin
  LeaveCriticalSection(cs);
  Exit(0);
 end;

 if ReleaseSemaphore(sema,_rel_wait_count(waiters_count,count),nil) then
 begin
  LeaveCriticalSection(cs);
  Exit(0);
 end;

 System.InterlockedExchangeAdd(val, -count);
 System.LeaveCriticalSection(cs);
 Exit(EINVAL);
end;

function do_sema_b_release(sema:THandle;count:DWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;
begin
 _sig_lock;
 Result:=__do_sema_b_release(sema,count,cs,val);
 _sig_unlock;
end;


/////

procedure wsem_list.Insert(Node:pwsem_node);
begin
 if (pTail=nil) then
 begin
  pHead:=node;
  node^.pPrev:=nil;
 end else
 begin
  pTail^.pNext:=node;
  node^.pPrev:=pTail;
 end;
 node^.pNext:=nil;
 pTail:=node;
end;

procedure wsem_list.Remove(node:pwsem_node);
begin
 if (node^.pPrev=nil) then
 begin
  if (pHead=node) then
  begin
   pHead:=node^.pNext;
  end;
 end else
 begin
  node^.pPrev^.pNext:=node^.pNext;
 end;
 if (node^.pNext=nil) then
 begin
  if (pTail=node) then
  begin
   pTail:=node^.pPrev;
  end;
 end else
 begin
  node^.pNext^.pPrev:=node^.pPrev;
 end;
end;

function sem_impl_init(m,mi:PSceKernelSema;max,value:Integer):Integer;
var
 new_mi:SceKernelSema;
begin
 new_mi:=AllocMem(SizeOf(_sem_t));
 if (new_mi=nil) then Exit(ENOMEM);

 new_mi^.init :=value;
 new_mi^.max  :=max;
 new_mi^.value:=value;
 new_mi^.valid:=LIFE_SEM;
 new_mi^.refs :=1;

 if CAS(m^,mi^,new_mi) then
 begin
  mi^:=new_mi;
 end else
 begin
  FreeMem(new_mi);
  mi^:=m^;
 end;

 Result:=0;
end;

function _sem_init(sem:PSceKernelSema;value:Integer):Integer;
var
 sv:SceKernelSema;
begin
 if (sem=nil) or (value<0) then Exit(EINVAL);
 sv:=sem^;
 _sig_lock;
 Result:=sem_impl_init(sem,@sv,SEM_VALUE_MAX,value);
 _sig_unlock;
end;

function sem_enter(sem,svp:PSceKernelSema):Integer;
var
 sv:SceKernelSema;
begin
 if (sem=nil) then Exit(EINVAL);
 sv:=sem^;
 if (sv=nil) then Exit(EINVAL);
 if not safe_test(sv^.valid,LIFE_SEM) then Exit(ESRCH);

 spin_lock(sv^.vlock);
 System.InterlockedIncrement(sv^.refs);

 svp^:=sv;
 Result:=0;
end;

procedure sem_leave(sv:SceKernelSema);
begin
 if (System.InterlockedDecrement(sv^.refs)=0) then
 begin
  SwFreeMem(sv);
 end;
end;

procedure _apc_null(dwParam:PTRUINT); stdcall;
begin
end;

function _sem_destroy(sem:PSceKernelSema):Integer;
var
 sv:SceKernelSema;
 node:pwsem_node;
begin
 Result:=sem_enter(sem,@sv);
 if (Result<>0) then Exit;

 sv^.value:=SEM_VALUE_MAX;

 if not CAS(sv^.valid,LIFE_SEM,DEAD_SEM) then
 begin
  spin_unlock(sv^.vlock);
  sem_leave(sv);
  Exit(EINVAL);
 end;

 //cancel all
 sv^.value:=sv^.max;

 node:=sv^.list.pHead;
 While (node<>nil) do
 begin
  if (node^.ret=1) then
   begin
    node^.Count:=0;
    node^.ret:=0;
    NtQueueApcThread(node^.thread,@_apc_null,0,nil,0);
   end;
  node:=node^.pNext;
 end;

 spin_unlock(sv^.vlock);
 System.InterlockedDecrement(sv^.refs);
 sem_leave(sv);
 Result:=0;
end;


function _sem_trywait(sem:PSceKernelSema;count:Integer):Integer;
var
 sv:SceKernelSema;
begin
 Result:=sem_enter(sem,@sv);
 if (Result<>0) then Exit;

 if (count>sv^.max) then
 begin
  spin_unlock(sv^.vlock);
  sem_leave(sv);
  Exit(EINVAL);
 end;

 if (sv^.value>=Count) then
 begin
  Dec(sv^.value,Count);
  Result:=0;
 end else
 begin
  Result:=EBUSY;
 end;

 spin_unlock(sv^.vlock);
 sem_leave(sv);
end;

function _sem_wait(sem:PSceKernelSema;count:Integer;pTimeout:PQWORD):Integer;
var
 t:pthread;
 sv:SceKernelSema;
 timeout:Int64;
 passed :Int64;
 START:QWORD;
 QTIME:QWORD;
 node:wsem_node;
begin
 if (count<=0) then Exit(EINVAL);

 t:=_get_curthread;
 if (t=nil) then Exit(ESRCH);

 Result:=sem_enter(sem,@sv);
 if (Result<>0) then Exit;

 //Writeln(GetCurrentThreadId,':>sem_wait:',sv^.name,' count:',count,' value:',sv^.value);

 if (count>sv^.max) then
 begin
  spin_unlock(sv^.vlock);
  sem_leave(sv);
  Exit(EINVAL);
 end;

 if (sv^.value>=count) then
 begin
  Dec(sv^.value,count);
  spin_unlock(sv^.vlock);
  sem_leave(sv);
  Exit(0);
 end;

 node:=Default(wsem_node);
 node.thread:=t^.handle;
 node.count :=count;
 node.ret   :=1;
 sv^.list.Insert(@node);

 spin_unlock(sv^.vlock);

 if (pTimeout<>nil) then
 begin
  timeout:=(pTimeout^ div 100);
  SwSaveTime(START);
 end else
 begin
  timeout:=NT_INFINITE;
 end;

 repeat

  if (node.ret<>1) then //is signaled
  begin
   Result:=node.ret;
   Break;
  end;

  if (pTimeout<>nil) then
  begin
   if (timeout=0) then
   begin
    Result:=ETIMEDOUT;
    Break;
   end;

   SwSaveTime(QTIME);

   timeout:=-timeout;
   SwDelayExecution(True,@timeout);
   timeout:=-timeout;

   passed:=SwTimePassedUnits(QTIME);

   if (passed>=timeout) then
   begin
    Result:=ETIMEDOUT;
    Break;
   end else
   begin
    timeout:=timeout-passed;
   end;

  end else
  begin
   SwDelayExecution(True,@timeout);
  end;

 until false;

 if (pTimeout<>nil) then
 begin
  if (Result=ETIMEDOUT) then
  begin
   pTimeout^:=0;
  end else
  begin
   passed:=SwTimePassedUnits(QTIME);
   pTimeout^:=passed*100;
  end;
 end;

 spin_lock(sv^.vlock);
   sv^.list.Remove(@node);
 spin_unlock(sv^.vlock);

 //Writeln(GetCurrentThreadId,':<sem_wait:',sv^.name,' count:',count,' value:',sv^.value);
 sem_leave(sv);
end;

function _sem_timedwait(sem:PSceKernelSema;ts:Ptimespec):Integer;
var
 t:QWORD;
begin
 if (ts=nil) then
 begin
  _sig_lock;
  Result:=_sem_wait(sem,1,nil);
  _sig_unlock;
 end else
 begin
  t:=_pthread_rel_time_in_ns(ts^);
  _sig_lock;
  Result:=_sem_wait(sem,1,@t);
  _sig_unlock;
 end;
end;

function _sem_post(sem:PSceKernelSema;count:Integer):Integer;
var
 sv:SceKernelSema;
 node:pwsem_node;
 value:Integer;
begin
 if (count<=0) then Exit(EINVAL);
 Result:=sem_enter(sem,@sv);
 if (Result<>0) then Exit;

 //Writeln(GetCurrentThreadId,':>sem_post:',sv^.name,' count:',count,' value:',sv^.value);

 if (count>sv^.max) then
 begin
  spin_unlock(sv^.vlock);
  sem_leave(sv);
  Exit(EINVAL);
 end;

 value:=sv^.value+count;

 node:=sv^.list.pHead;
 While (node<>nil) do
 begin
  if (node^.ret=1) then
   begin
    if (node^.Count>value) then
    begin
     Dec(node^.Count,value);
     value:=0;
     Break;
    end else
    if (node^.Count<=value) then
    begin
     Dec(value,node^.Count);
     node^.Count:=0;
     node^.ret:=0;
     NtQueueApcThread(node^.thread,@_apc_null,0,nil,0);
     if (value=0) then Break;
    end;
   end;
  node:=node^.pNext;
 end;

 if (value>sv^.max) then
 begin
  sv^.value:=sv^.max;
  Result:=EOVERFLOW;
 end else
 begin
  sv^.value:=value;
 end;

 spin_unlock(sv^.vlock);

 //Writeln(GetCurrentThreadId,'<sem_post:',sv^.name,' count:',count,' value:',sv^.value);

 sem_leave(sv);
end;

function _sem_getvalue(sem:PSceKernelSema;sval:PInteger):Integer;
var
 sv:SceKernelSema;
begin
 if (sval=nil) then Exit(EINVAL);
 Result:=sem_enter(sem,@sv);
 if (Result<>0) then Exit;
 sval^:=sv^.value;
 spin_unlock(sv^.vlock);
 sem_leave(sv);
 Result:=0;
end;

//

function ps4_sem_init(sem:PSceKernelSema;value:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=_set_errno(_sem_init(sem,value));
end;

function ps4_sem_destroy(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sem_destroy(sem));
 _sig_unlock;
end;

function ps4_sem_getvalue(sem:PSceKernelSema;sval:PInteger):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sem_getvalue(sem,sval));
 _sig_unlock;
end;

function ps4_sem_post(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sem_post(sem,1));
 _sig_unlock;
end;

function ps4_sem_timedwait(sem:PSceKernelSema;ts:Ptimespec):Integer; SysV_ABI_CDecl;
begin
 Result:=_set_errno(_sem_timedwait(sem,ts));
end;

function ps4_sem_trywait(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sem_trywait(sem,1);
 _sig_unlock;
 if (Result=EBUSY) then Result:=EAGAIN;
 Result:=_set_errno(Result);
end;

function ps4_sem_wait(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sem_wait(sem,1,nil));
 _sig_unlock;
end;

////

function ps4_sceKernelCreateSema(
          sem:PSceKernelSema;
          name:Pchar;
          attr:DWORD;
          init,max:Integer;
          opt:PSceKernelSemaOptParam):Integer; SysV_ABI_CDecl;
var
 sv:SceKernelSema;
begin
 if (sem=nil) or (max<=0) or (init<0) then Exit(SCE_KERNEL_ERROR_EINVAL);
 sv:=sem^;
 _sig_lock;
 Result:=px2sce(sem_impl_init(sem,@sv,max,init));
 _sig_unlock;
 if (Result<>0) then Exit;
 if (name<>nil) then MoveChar0(name^,sv^.name,32);
end;

function ps4_sceKernelDeleteSema(sem:SceKernelSema):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=px2sce(_sem_destroy(@sem));
 _sig_unlock;
end;

//typedef unsigned int SceKernelUseconds;
function ps4_sceKernelWaitSema(sem:SceKernelSema;Count:Integer;pTimeout:PDWORD):Integer; SysV_ABI_CDecl;
var
 t:QWORD;
begin
 if (pTimeout=nil) then
 begin
  _sig_lock;
  Result:=px2sce(_sem_wait(@sem,Count,nil));
  _sig_unlock;
 end else
 begin
  t:=_usec2nsec(pTimeout^);
  _sig_lock;
  Result:=px2sce(_sem_wait(@sem,Count,@t));
  _sig_unlock;
  pTimeout^:=dwMilliSecs(_nsec2usec(t));
 end;
end;

function ps4_sceKernelSignalSema(sem:SceKernelSema;Count:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sem_post(@sem,Count);
 _sig_unlock;
 if (Result=EOVERFLOW) then Result:=EINVAL;
 Result:=px2sce(Result);
end;

function ps4_sceKernelPollSema(sem:SceKernelSema;Count:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=px2sce(_sem_trywait(@sem,count));
 _sig_unlock;
end;

function ps4_sceKernelCancelSema(sem:SceKernelSema;setCount:Integer;threads:PInteger):Integer; SysV_ABI_CDecl;
var
 sv:SceKernelSema;
 node:pwsem_node;
 reset:Integer;
begin
 if (setCount<=0) then Exit(EINVAL);
 Result:=sem_enter(@sem,@sv);
 if (Result<>0) then Exit;

 if (setCount>sv^.max) then
 begin
  spin_unlock(sv^.vlock);
  sem_leave(sv);
  Exit(SCE_KERNEL_ERROR_EINVAL);
 end;

 sv^.value:=setCount;

 reset:=0;
 node:=sv^.list.pHead;
 While (node<>nil) do
 begin
  if (node^.ret=1) then
   begin
    Inc(reset);
    node^.Count:=0;
    node^.ret:=0;
    NtQueueApcThread(node^.thread,@_apc_null,0,nil,0);
   end;
  node:=node^.pNext;
 end;

 if (threads<>nil) then
 begin
  threads^:=reset;
 end;

 spin_unlock(sv^.vlock);
 sem_leave(sv);
end;


end.

