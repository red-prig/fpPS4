unit ps4_sema;

{$mode objfpc}{$H+}

interface

uses
 windows,
 sys_types;

const
 SCE_KERNEL_SEMA_ATTR_TH_FIFO=$01;
 SCE_KERNEL_SEMA_ATTR_TH_PRIO=$02;
 SCE_KERNEL_SEMA_ID_INVALID:Int64=-1;

 SEM_FAILED	=nil;
 SEM_VALUE_MAX	=High(Integer);

type
 PSceKernelSemaOptParam=^TSceKernelSemaOptParam;
 TSceKernelSemaOptParam=packed record
  size:QWORD;
 end;

 PSceKernelSema=^SceKernelSema;
 SceKernelSema=^_sem_t;
 _sem_t=record
  valid:DWORD;
  s:THandle;
  init,max:Integer;
  num:Integer;
  value:Integer;
  //vlock:pthread_mutex_t;
  lock:Pointer;
  name:array[0..31] of AnsiChar;
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
function ps4_sceKernelCancelSema(sem:SceKernelSema;count:Integer;threads:PInteger):Integer; SysV_ABI_CDecl;

//function do_sema_b_wait(sema:THandle;timeout:DWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;
//function do_sema_b_wait_intern(sema:THandle;timeout:DWORD):Integer;

function do_sema_b_wait(sema:THandle;pTimeout:PQWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;
function do_sema_b_wait_intern(sema:THandle;pTimeout:PQWORD):Integer; inline;

function do_sema_b_release(sema:THandle;count:DWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;

procedure SwEnterCriticalSection(var cs:TRTLCriticalSection);

implementation

//int	 sem_unlink(const char *);

uses
 atomic,
 spinlock,
 sys_kernel,
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

{
function do_sema_b_wait(sema:THandle;timeout:DWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;
var
 r:Integer;
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
 r:=do_sema_b_wait_intern(sema,timeout);
 SwEnterCriticalSection(cs);
 if (r<>0) then
 begin
  System.InterlockedIncrement(val);
 end;
 System.LeaveCriticalSection(cs);
 Result:=r;
 _sig_unlock;
end;
}

{
function do_sema_b_wait_intern(sema:THandle;timeout:DWORD):Integer;
var
 r:Integer;
 res:DWORD;
 QTIME:DWORD;
begin

 if (timeout<>INFINITE) then
 begin
  _sig_lock;
  QTIME:=Windows.GetTickCount;
  _sig_unlock;
 end;

 repeat

  _sig_lock(True);
  res:=WaitForSingleObjectEx(sema,timeout,True);
  _sig_unlock;

  case res of
   WAIT_IO_COMPLETION:
    begin

     if (timeout<>INFINITE) then
     begin
      _sig_lock;
      QTIME:=Windows.GetTickCount-QTIME;
      _sig_unlock;

      if (QTIME>timeout) then
       timeout:=0
      else
       timeout:=timeout-QTIME;

      if (timeout=0) then
      begin
       r:=0;
       Break;
      end;
     end;

    end;
   WAIT_TIMEOUT:
    begin
     r:=ETIMEDOUT;
     Break;
    end;
   WAIT_ABANDONED:
    begin
     r:=EPERM;
     Break;
    end;
   WAIT_OBJECT_0:
    begin
     r:=0;
     Break;
    end;
   else
    begin
     r:=EINVAL;
     Break;
    end;
  end;

 until false;

 //if (r<>0) and (r<>EINVAL) and (WaitForSingleObject(sema,0)=WAIT_OBJECT_0) then
 // r:=0;
 Result:=r;
end;
}

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

function sem_impl_init(m,mi:PSceKernelSema;max,value:Integer):Integer;
var
 new_mi:SceKernelSema;
begin
 new_mi:=AllocMem(SizeOf(_sem_t));
 if (new_mi=nil) then Exit(ENOMEM);

 new_mi^.init:=value;
 new_mi^.max:=max;
 new_mi^.value:=value;

 new_mi^.s:=CreateSemaphore(nil,0,SEM_VALUE_MAX,nil);

 if (new_mi^.s=0) then
 begin
  FreeMem(new_mi);
  Exit(ENOSPC);
 end;

 new_mi^.valid:=LIFE_SEM;

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

function _sem_destroy(sem:PSceKernelSema):Integer;
var
 sv:SceKernelSema;
 bkoff:backoff_exp;
begin
 if (sem=nil) then Exit(EINVAL);
 sv:=XCHG(sem^,nil);
 if (sv=nil) then Exit(EINVAL);

 if not safe_test(sv^.valid,LIFE_SEM) then Exit(EINVAL);
 spin_lock(sv^.lock);

 if not CloseHandle(sv^.s) then
 begin
  spin_unlock(sv^.lock);
  Exit(EINVAL);
 end;

 sv^.value:=SEM_VALUE_MAX;

 if CAS(sv^.valid,LIFE_SEM,DEAD_SEM) then
 begin
  spin_unlock(sv^.lock);
  bkoff.Reset;
  While (sv^.num<>0) do bkoff.Wait;
  FreeMem(sv);
  Result:=0;
 end else
 begin
  spin_unlock(sv^.lock);
  Result:=EINVAL;
 end;
end;

function sem_std_enter(sem,svp:PSceKernelSema):Integer;
var
 sv:SceKernelSema;
begin
 if (sem=nil) then Exit(EINVAL);
 sv:=sem^;
 if (sv=nil) then Exit(EINVAL);
 if not safe_test(sv^.valid,LIFE_SEM) then Exit(EINVAL);

 spin_lock(sv^.lock);

 if (sem^=nil) then
 begin
  spin_unlock(sv^.lock);
  Exit(EINVAL);
 end;

 svp^:=sv;
 Result:=0;
end;

function _sem_trywait(sem:PSceKernelSema):Integer;
var
 sv:SceKernelSema;
begin
 Result:=sem_std_enter(sem,@sv);
 if (Result<>0) then Exit;

 if (sv^.value<=0) then
 begin
  spin_unlock(sv^.lock);
  Exit(EAGAIN);
 end;

 Dec(sv^.value);
 spin_unlock(sv^.lock);

 Result:=0;
end;

function _sem_wait(sem:PSceKernelSema;count:Integer;pTimeout:PQWORD):Integer;
var
 sv:SceKernelSema;
 cur_v:Integer;
 semh:THandle;
begin
 if (count<=0) then Exit(EINVAL);
 Result:=sem_std_enter(sem,@sv);
 if (Result<>0) then Exit;

 //if (sv^.name='SuspendSemaphore') or
 //   (sv^.name='ResumeSemaphore') then
 // Writeln('>sem_wait:',sv^.name,' count:',count,' value:',sv^.value);

 if (count>sv^.max) then
 begin
  spin_unlock(sv^.lock);
  Exit(EINVAL);
 end;

 Dec(sv^.value,count);
 cur_v:=sv^.value;
 semh :=sv^.s;
 spin_unlock(sv^.lock);

 if (cur_v>=0) then Exit(0);

 //pthread_cleanup_push (clean_wait_sem, (void *) &arg);

 System.InterlockedIncrement(sv^.num);
 Result:=do_sema_b_wait_intern(semh,pTimeout);
 System.InterlockedDecrement(sv^.num);

 //if (sv^.name='SuspendSemaphore') or
 //   (sv^.name='ResumeSemaphore') then
 // Writeln('<sem_wait:',sv^.name,' count:',count,' value:',sv^.value);

 //pthread_cleanup_pop (ret);
 if (Result=EINVAL) then Result:=0;
end;

function _sem_timedwait(sem:PSceKernelSema;ts:Ptimespec):Integer;
var
 t:QWORD;
begin
 if (ts=nil) then
 begin
  Result:=_sem_wait(sem,1,nil);
 end else
 begin
  t:=_pthread_rel_time_in_ns(ts^);
  Result:=_sem_wait(sem,1,@t);
 end;
end;

function _sem_post(sem:PSceKernelSema;count:Integer):Integer;
var
 sv:SceKernelSema;
 waiters_count:Integer;
begin
 if (count<=0) then Exit(EINVAL);
 Result:=sem_std_enter(sem,@sv);
 if (Result<>0) then Exit;

 //if (sv^.name='SuspendSemaphore') or
 //   (sv^.name='ResumeSemaphore') then
 // Writeln('>sem_post:',sv^.name,' count:',count,' value:',sv^.value);

 if (count>sv^.max) or (sv^.value>(sv^.max-count)) then
 begin
  spin_unlock(sv^.lock);
  Exit(EINVAL);
 end;

 waiters_count:=-sv^.value;
 Inc(sv^.value,count);

 if (waiters_count<=0) then
 begin
  spin_unlock(sv^.lock);
  Exit(0);
 end;

 if ReleaseSemaphore(sv^.s,_rel_wait_count(waiters_count,count),nil) then
 begin
  spin_unlock(sv^.lock);
  Exit(0);
 end;

 Dec(sv^.value,count);
 spin_unlock(sv^.lock);
 Result:=EINVAL;
end;

function _sem_getvalue(sem:PSceKernelSema;sval:PInteger):Integer;
var
 sv:SceKernelSema;
begin
 if (sval=nil) then Exit(EINVAL);
 Result:=sem_std_enter(sem,@sv);
 if (Result<>0) then Exit;
 sval^:=sv^.value;
 spin_unlock(sv^.lock);
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
 Result:=_set_errno(_sem_getvalue(sem,sval));
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
 Result:=_set_errno(_sem_trywait(sem));
end;

function ps4_sem_wait(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;
begin
 Result:=_set_errno(_sem_wait(sem,1,nil));
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
 q:QWORD;
 t:QWORD;
begin
 if (pTimeout=nil) then
 begin
  Result:=px2sce(_sem_wait(@sem,Count,nil));
 end else
 begin
  t:=_usec2nsec(pTimeout^);
  q:=_pthread_time_in_ns;
  Result:=px2sce(_sem_wait(@sem,Count,@t));
 end;

 if (pTimeout<>nil) then
 begin
  if (Result=SCE_KERNEL_ERROR_ETIMEDOUT) then
  begin
   pTimeout^:=0;
  end else
  begin
   t:=_pthread_time_in_ns;
   if (t>q) then
   begin
    q:=t-q;
   end else
   begin
    q:=0;
   end;
   pTimeout^:=dwMilliSecs(_nsec2usec(q));
  end;
 end;
end;

function ps4_sceKernelSignalSema(sem:SceKernelSema;Count:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=px2sce(_sem_post(@sem,Count));
 _sig_unlock;
end;

function ps4_sceKernelPollSema(sem:SceKernelSema;Count:Integer):Integer; SysV_ABI_CDecl;
var
 sv:SceKernelSema;
begin
 if (Count<=0) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Result:=px2sce(sem_std_enter(@sem,@sv));
 if (Result<>0) then Exit;

 if (Count>sv^.max) then
 begin
  spin_unlock(sv^.lock);
  Exit(SCE_KERNEL_ERROR_EINVAL);
 end;

 if (sv^.value>=Count) then
 begin
  Dec(sv^.value,Count);
  Result:=0;
 end else
 begin
  Result:=SCE_KERNEL_ERROR_EBUSY;
 end;
 spin_unlock(sv^.lock);
end;

function ps4_sceKernelCancelSema(sem:SceKernelSema;count:Integer;threads:PInteger):Integer; SysV_ABI_CDecl;
var
 sv:SceKernelSema;
 waiters_count:Integer;
begin
 Result:=px2sce(sem_std_enter(@sem,@sv));
 if (Result<>0) then Exit;

 if (count>sv^.max) then
 begin
  spin_unlock(sv^.lock);
  Exit(SCE_KERNEL_ERROR_EINVAL);
 end;

 if (threads<>nil) then threads^:=sv^.num;

 waiters_count:=-sv^.value;

 if (waiters_count>0) then
 begin
  _sig_lock;
  ReleaseSemaphore(sv^.s,waiters_count,nil);
  _sig_unlock;
 end;

 if (count<0) then
  sv^.value:=sv^.init
 else
  sv^.value:=count;

 spin_unlock(sv^.lock);
 Result:=0;
end;


end.

