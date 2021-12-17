unit ps4_sema;

{$mode objfpc}{$H+}

interface

uses
 windows,
 ps4_types;

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

function do_sema_b_wait(sema:THandle;timeout:DWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;
function do_sema_b_wait_intern(sema:THandle;timeout:DWORD):Integer;
function do_sema_b_release(sema:THandle;count:DWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;


implementation

//int	 sem_unlink(const char *);

uses
 spinlock,
 ps4_time,
 ps4_libkernel;

const
 LIFE_SEM=$BAB1F00D;
 DEAD_SEM=$DEADBEEF;

function do_sema_b_wait(sema:THandle;timeout:DWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;
var
 r:Integer;
 v:Integer;
begin
 System.EnterCriticalSection(cs);
 System.InterlockedDecrement(val);
 v:=val;
 System.LeaveCriticalSection(cs);
 if (v>=0) then Exit(0);
 r:=do_sema_b_wait_intern(sema,timeout);
 System.EnterCriticalSection(cs);
 if (r<>0) then
 begin
  System.InterlockedIncrement(val);
 end;
 System.LeaveCriticalSection(cs);
 Result:=r;
end;

function do_sema_b_wait_intern(sema:THandle;timeout:DWORD):Integer;
var
 r:Integer;
 res:DWORD;
begin
 res:=WaitForSingleObject(sema,timeout);
 case res of
  WAIT_TIMEOUT  :r:=ETIMEDOUT;
  WAIT_ABANDONED:r:=EPERM;
  WAIT_OBJECT_0 :r:=0;
  else
                 r:=EINVAL;
 end;
 if (r<>0) and (r<>EINVAL) and (WaitForSingleObject(sema,0)=WAIT_OBJECT_0) then
  r:=0;
 Result:=r;
end;

function do_sema_b_release(sema:THandle;count:DWORD;var cs:TRTLCriticalSection;var val:Integer):Integer;
var
 wc,s:Integer;
begin
 System.EnterCriticalSection(cs);
 if (Int64(val)+Int64(count))>$7fffffff then
 begin
  System.LeaveCriticalSection(cs);
  Exit(EINVAL);
 end;
 wc:=-val;
 //if (wc=0) then wc:=1;
 System.InterlockedExchangeAdd(val,count);
 if (wc<count) then s:=wc else s:=count;
 if ((wc<=0) or ReleaseSemaphore(sema,s,nil)) then
 begin
  LeaveCriticalSection(cs);
  Exit(0);
 end;
 System.InterlockedExchangeAdd(val, -count);
 System.LeaveCriticalSection(cs);
 Exit(EINVAL);
end;

/////

function CAS(Var addr:Pointer;Comp,New:Pointer):Boolean; inline;
begin
 Result:=System.InterlockedCompareExchange(addr,New,Comp)=Comp;
end;

function CAS(Var addr:DWORD;Comp,New:DWORD):Boolean; inline;
begin
 Result:=System.InterlockedCompareExchange(addr,New,Comp)=Comp;
end;

function XCHG(Var addr:Pointer;New:Pointer):Pointer; inline;
begin
 Result:=System.InterLockedExchange(addr,New);
end;

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
end;

function _sem_init(sem:PSceKernelSema;value:Integer):Integer;
var
 sv:SceKernelSema;
begin
 if (sem=nil) or (value<0) then Exit(EINVAL);
 sv:=sem^;
 Result:=sem_impl_init(sem,@sv,SEM_VALUE_MAX,value);
end;

function _sem_destroy(sem:PSceKernelSema):Integer;
var
 sv:SceKernelSema;
 bkoff:backoff_exp;
begin
 if (sem=nil) then Exit(EINVAL);
 sv:=XCHG(sem^,nil);
 if (sv=nil) then Exit(EINVAL);

 spin_lock(sv^.lock);
 if (sv^.valid<>LIFE_SEM) then Exit(EINVAL);

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
 if (sv^.valid<>LIFE_SEM) then Exit(EINVAL);

 spin_lock(sv^.lock);
 if (sv^.valid<>LIFE_SEM) then Exit(EINVAL);

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

function _sem_wait(sem:PSceKernelSema;count:Integer;t:DWORD):Integer;
var
 sv:SceKernelSema;
 cur_v:Integer;
 semh:THandle;
begin
 if (count<=0) then Exit(EINVAL);
 Result:=sem_std_enter(sem,@sv);
 if (Result<>0) then Exit;

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
 Result:=do_sema_b_wait_intern(semh,t);
 System.InterlockedDecrement(sv^.num);

 //pthread_cleanup_pop (ret);
 if (Result=EINVAL) then Result:=0;
end;

function _sem_timedwait(sem:PSceKernelSema;ts:Ptimespec):Integer;
var
 t:DWORD;
begin
 if (ts=nil) then
 begin
  t:=INFINITE;
 end else
 begin
  t:=dwMilliSecs(_pthread_rel_time_in_ms(ts^));
 end;
 Result:=_sem_wait(sem,1,t);
end;

function _sem_post(sem:PSceKernelSema;count:Integer):Integer;
var
 sv:SceKernelSema;
 waiters_count,w:Integer;
begin
 if (count<=0) then Exit(EINVAL);
 Result:=sem_std_enter(sem,@sv);
 if (Result<>0) then Exit;

 if (count>sv^.max) or (sv^.value>(sv^.max-count)) then
 begin
  spin_unlock(sv^.lock);
  Exit(EINVAL);
 end;

 waiters_count:=-sv^.value;
 Inc(sv^.value,count);

 if (waiters_count<count) then w:=waiters_count else w:=count;

 if (waiters_count<=0) or ReleaseSemaphore(sv^.s,w,nil) then
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
 Result:=lc_set_errno(_sem_init(sem,value));
end;

function ps4_sem_destroy(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;
begin
 Result:=lc_set_errno(_sem_destroy(sem));
end;

function ps4_sem_getvalue(sem:PSceKernelSema;sval:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=lc_set_errno(_sem_getvalue(sem,sval));
end;

function ps4_sem_post(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;
begin
 Result:=lc_set_errno(_sem_post(sem,1));
end;

function ps4_sem_timedwait(sem:PSceKernelSema;ts:Ptimespec):Integer; SysV_ABI_CDecl;
begin
 Result:=lc_set_errno(_sem_timedwait(sem,ts));
end;

function ps4_sem_trywait(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;
begin
 Result:=lc_set_errno(_sem_trywait(sem));
end;

function ps4_sem_wait(sem:PSceKernelSema):Integer; SysV_ABI_CDecl;
begin
 Result:=lc_set_errno(_sem_wait(sem,1,INFINITE));
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
 Result:=px2sce(sem_impl_init(sem,@sv,max,init));
 if (Result<>0) then Exit;
 if (name<>nil) then MoveChar0(name^,sv^.name,32);
end;

function ps4_sceKernelDeleteSema(sem:SceKernelSema):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(_sem_destroy(@sem));
end;

//typedef unsigned int SceKernelUseconds;
function ps4_sceKernelWaitSema(sem:SceKernelSema;Count:Integer;pTimeout:PDWORD):Integer; SysV_ABI_CDecl;
var
 q:QWORD;
 t:DWORD;
begin
 if (pTimeout=nil) then
 begin
  t:=INFINITE;
 end else
 begin
  t:=_usec2msec(pTimeout^);
  q:=_pthread_time_in_ms;
 end;
 Result:=px2sce(_sem_wait(@sem,Count,t));
 if (pTimeout<>nil) then
 begin
  if (Result=SCE_KERNEL_ERROR_ETIMEDOUT) then
  begin
   pTimeout^:=0;
  end else
  begin
   q:=_pthread_time_in_ms-q;
   q:=q*1000;
   pTimeout^:=dwMilliSecs(q);
  end;
 end;
end;

function ps4_sceKernelSignalSema(sem:SceKernelSema;Count:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(_sem_post(@sem,Count));
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
  ReleaseSemaphore(sv^.s,waiters_count,nil);
 end;

 if (count<0) then
  sv^.value:=sv^.init
 else
  sv^.value:=count;

 spin_unlock(sv^.lock);
 Result:=0;
end;


end.

