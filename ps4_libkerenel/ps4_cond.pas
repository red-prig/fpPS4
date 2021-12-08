unit ps4_cond;

{$mode objfpc}{$H+}

interface

uses
 Windows,
 ps4_mutex,
 ps4_types;

type
 Ppthread_condattr=^pthread_condattr_t;
 pthread_condattr_t=bitpacked record
  _shared:0..1;         //1
  _clock:0..31;         //5
  _align:0..67108863;   //26
  _align2:Integer;      //32
 end;

 Ppthread_cond=^pthread_cond;
 pthread_cond=^pthread_cond_t;
 pthread_cond_t=record
  valid:DWORD;
  busy :DWORD;
  waiters_count_:DWORD;
  waiters_count_unblock_:DWORD;
  waiters_count_gone_:DWORD;
  value_q:Integer;
  value_b:Integer;
  waiters_count_lock_:TRTLCriticalSection;
  waiters_q_lock_:TRTLCriticalSection;
  waiters_b_lock_:TRTLCriticalSection;
  sema_q:THandle;
  sema_b:THandle;
  name:array[0..31] of AnsiChar;
 end;

 PScePthreadCond=Ppthread_cond;

Const
 PTHREAD_COND_INITIALIZER=nil;

function ps4_pthread_condattr_init(pAttr:Ppthread_condattr):Integer; SysV_ABI_CDecl;
function ps4_pthread_condattr_destroy(pAttr:Ppthread_condattr):Integer; SysV_ABI_CDecl;
function ps4_pthread_condattr_getclock(pAttr:Ppthread_condattr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_pthread_condattr_setclock(pAttr:Ppthread_condattr;t:Integer):Integer; SysV_ABI_CDecl;
function ps4_pthread_condattr_getpshared(pAttr:Ppthread_condattr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_pthread_condattr_setpshared(pAttr:Ppthread_condattr;t:Integer):Integer; SysV_ABI_CDecl;

function ps4_pthread_cond_init(pCond:Ppthread_cond;pAttr:Ppthread_condattr):Integer; SysV_ABI_CDecl;
function ps4_pthread_cond_destroy(pCond:Ppthread_cond):Integer; SysV_ABI_CDecl;

function ps4_pthread_cond_signal(pCond:Ppthread_cond):Integer; SysV_ABI_CDecl;
function ps4_pthread_cond_broadcast(pCond:Ppthread_cond):Integer; SysV_ABI_CDecl;

function ps4_pthread_cond_wait(pCond:Ppthread_cond;pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
function ps4_pthread_cond_timedwait(pCond:Ppthread_cond;pMutex:Ppthread_mutex;ptime:Ptimespec):Integer; SysV_ABI_CDecl;

function ps4_scePthreadCondattrInit(pAttr:Ppthread_condattr):Integer; SysV_ABI_CDecl;
function ps4_scePthreadCondattrDestroy(pAttr:Ppthread_condattr):Integer; SysV_ABI_CDecl;

function ps4_scePthreadCondInit(pCond:PScePthreadCond;pAttr:Ppthread_condattr;name:Pchar):Integer; SysV_ABI_CDecl;
function ps4_scePthreadCondDestroy(pCond:PScePthreadCond):Integer; SysV_ABI_CDecl;

function ps4_scePthreadCondSignal(pCond:PScePthreadCond):Integer; SysV_ABI_CDecl;
function ps4_scePthreadCondWait(pCond:PScePthreadCond;pMutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;
function ps4_scePthreadCondTimedwait(pCond:PScePthreadCond;pMutex:PScePthreadMutex;usec:DWORD):Integer; SysV_ABI_CDecl;
function ps4_scePthreadCondBroadcast(pCond:PScePthreadCond):Integer; SysV_ABI_CDecl;

implementation

Uses
 spinlock,
 ps4_sema,
 ps4_libkernel,
 ps4_time;

function ps4_pthread_condattr_init(pAttr:Ppthread_condattr):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 pAttr^:=Default(pthread_condattr_t);
 Result:=0;
end;

function ps4_pthread_condattr_destroy(pAttr:Ppthread_condattr):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 pAttr^:=Default(pthread_condattr_t);
 Result:=0;
end;

function ps4_pthread_condattr_getclock(pAttr:Ppthread_condattr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(EINVAL);
 t^:=pAttr^._clock;
 Result:=0;
end;

function ps4_pthread_condattr_setclock(pAttr:Ppthread_condattr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 Case t of
  CLOCK_REALTIME         :;
  CLOCK_VIRTUAL          :;
  CLOCK_PROF             :;
  CLOCK_MONOTONIC        :;
  CLOCK_UPTIME           :;
  CLOCK_UPTIME_PRECISE   :;
  CLOCK_UPTIME_FAST      :;
  CLOCK_REALTIME_PRECISE :;
  CLOCK_REALTIME_FAST    :;
  CLOCK_MONOTONIC_PRECISE:;
  CLOCK_MONOTONIC_FAST   :;
  CLOCK_SECOND           :;
  CLOCK_THREAD_CPUTIME_ID:;
  CLOCK_PROCTIME         :;
  CLOCK_EXT_NETWORK      :;
  CLOCK_EXT_DEBUG_NETWORK:;
  CLOCK_EXT_AD_NETWORK   :;
  CLOCK_EXT_RAW_NETWORK  :;
  else
   Exit(EINVAL);
 end;
 pAttr^._clock:=t;
 Result:=0;
end;

function ps4_pthread_condattr_getpshared(pAttr:Ppthread_condattr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(EINVAL);
 t^:=pAttr^._shared;
 Result:=0;
end;

function ps4_pthread_condattr_setpshared(pAttr:Ppthread_condattr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 Case t of
  PTHREAD_PROCESS_PRIVATE:;
  PTHREAD_PROCESS_SHARED :;
  else
   Exit(EINVAL);
 end;
 pAttr^._shared:=t;
 Result:=0;
end;

Const
 LIFE_COND=$C0BAB1FD;
 DEAD_COND=$C0DEADBF;

{type
 sCondWaitHelper=record
  c:pthread_cond;
  external_mutex:pthread_mutex;
  r:Pinteger;
 end;}

var
 cond_locked:Pointer=nil;

function STATIC_COND_INITIALIZER(x:Ppthread_cond):Boolean; inline;
begin
 Result:=(x^=PTHREAD_COND_INITIALIZER);
end;

function pthread_cond_init(c:Ppthread_cond;a:Ppthread_condattr;str:PChar):Integer;
var
 _c:pthread_cond;
begin
 if (c=nil) then Exit(EINVAL);

 _c:=AllocMem(SizeOf(pthread_cond_t));
 if (_c=nil) then Exit(ENOMEM);

 _c^.valid:=DEAD_COND;

 _c^.sema_q:=CreateSemaphore(nil,0,$7fffffff,nil);
 if (_c^.sema_q=0) then
 begin
  FreeMem(_c);
  c^:=nil;
  Exit(EAGAIN);
 end;
 _c^.sema_b:=CreateSemaphore(nil,0,$7fffffff,nil);
 if (_c^.sema_b=0) then
 begin
  CloseHandle(_c^.sema_q);
  FreeMem(_c);
  c^:=nil;
  Exit(EAGAIN);
 end;

 System.InitCriticalSection(_c^.waiters_count_lock_);
 System.InitCriticalSection(_c^.waiters_b_lock_);
 System.InitCriticalSection(_c^.waiters_q_lock_);
 _c^.value_q:=0;
 _c^.value_b:=1;
 _c^.valid:=LIFE_COND;

 if (str<>nil) then MoveChar0(str^,_c^.name,32);

 c^:=_c;
 Result:=0;
end;

function cond_static_init(c:Ppthread_cond):Integer;
var
 r:Integer;
begin
 r:=0;
 spin_lock(cond_locked);
 if (c=nil) then Exit(EINVAL);

 if STATIC_COND_INITIALIZER(c) then
 begin
  r:=pthread_cond_init(c,nil,nil);
 end;

 spin_unlock(cond_locked);
 Result:=r;
end;

function ps4_pthread_cond_init(pCond:Ppthread_cond;pAttr:Ppthread_condattr):Integer; SysV_ABI_CDecl;
begin
 Result:=pthread_cond_init(pCond,pAttr,nil);
end;

function ps4_pthread_cond_destroy(pCond:Ppthread_cond):Integer; SysV_ABI_CDecl;
var
 r:Integer;
 _c:pthread_cond;
begin
 if (pCond=nil) then Exit(EINVAL);

 if STATIC_COND_INITIALIZER(pCond) then
 begin
  spin_lock(cond_locked);
  if STATIC_COND_INITIALIZER(pCond) then
  begin
   r:=0;
  end else
  begin
   r:=EBUSY;
  end;
  spin_unlock(cond_locked);
  Exit(r);
 end;

 _c:=pCond^;
 r:=do_sema_b_wait(_c^.sema_b,INFINITE,_c^.waiters_b_lock_,_c^.value_b);
 if (r<>0) then Exit(r);

 if (System.TryEnterCriticalSection(_c^.waiters_count_lock_)=0) then
 begin
  do_sema_b_release(_c^.sema_b,1,_c^.waiters_b_lock_,_c^.value_b);
  Exit(EBUSY);
 end;

 if (_c^.waiters_count_ > _c^.waiters_count_gone_) then
 begin
  r:=do_sema_b_release(_c^.sema_b, 1,_c^.waiters_b_lock_,_c^.value_b);
  if (r=0) then r:=EBUSY;
  System.LeaveCriticalSection(_c^.waiters_count_lock_);
  Exit(r);
 end;

 pCond^:=nil;

 do_sema_b_release(_c^.sema_b,1,_c^.waiters_b_lock_,_c^.value_b);

 if (not CloseHandle(_c^.sema_q)) and (r=0) then r:=EINVAL;
 if (not CloseHandle(_c^.sema_b)) and (r=0) then r:=EINVAL;

 System.LeaveCriticalSection (_c^.waiters_count_lock_);
 System.DoneCriticalSection(_c^.waiters_count_lock_);
 System.DoneCriticalSection(_c^.waiters_b_lock_);
 System.DoneCriticalSection(_c^.waiters_q_lock_);
 _c^.valid:=DEAD_COND;
 FreeMem(_c);
 Result:=0;
end;

function ps4_pthread_cond_signal(pCond:Ppthread_cond):Integer; SysV_ABI_CDecl;
var
 r:Integer;
 _c:pthread_cond;
begin
 if (pCond=nil) then Exit(EINVAL);

 _c:=pCond^;
 if (_c=PTHREAD_COND_INITIALIZER) then
  Exit(0)
 else
 if (_c^.valid<>LIFE_COND) then
  Exit(EINVAL);

 System.EnterCriticalSection(_c^.waiters_count_lock_);

 //mingw implement is wrong
 if true {(_c^.waiters_count_unblock_<>0)} then
 begin
  if (_c^.waiters_count_=0) then
  begin
   System.LeaveCriticalSection(_c^.waiters_count_lock_);
   Exit(0);
  end;
  Dec(_c^.waiters_count_);
  Inc(_c^.waiters_count_unblock_);
 end else
 if false {(_c^.waiters_count_>_c^.waiters_count_gone_)} then
 begin

  r:=do_sema_b_wait(_c^.sema_b,INFINITE,_c^.waiters_b_lock_,_c^.value_b);
  if (r<>0) then
  begin
        //r:=do_sema_b_release (_c^.sema_b,1,_c^.waiters_b_lock_,_c^.value_b);
   System.LeaveCriticalSection (_c^.waiters_count_lock_);
   Exit(r);
  end;

  if (_c^.waiters_count_gone_<>0) then
  begin
   Dec(_c^.waiters_count_,_c^.waiters_count_gone_);
   _c^.waiters_count_gone_:=0;
  end;
  Dec(_c^.waiters_count_);
  _c^.waiters_count_unblock_:=1;

  //System.LeaveCriticalSection(_c^.waiters_count_lock_);
        //r:=do_sema_b_release (_c^.sema_b,1,_c^.waiters_b_lock_,_c^.value_b);

 end else
 begin
  System.LeaveCriticalSection(_c^.waiters_count_lock_);
  Exit(0);
 end;

 System.LeaveCriticalSection(_c^.waiters_count_lock_);
 Result:=do_sema_b_release(_c^.sema_q,1,_c^.waiters_q_lock_,_c^.value_q);
end;

function ps4_pthread_cond_broadcast(pCond:Ppthread_cond):Integer; SysV_ABI_CDecl;
var
 r,relCnt:Integer;
 _c:pthread_cond;
begin
 if (pCond=nil) then Exit(EINVAL);
 relCnt:=0;

 _c:=pCond^;
 if (_c=PTHREAD_COND_INITIALIZER) then
  Exit(0)
 else
 if (_c^.valid<>LIFE_COND) then
  Exit(EINVAL);

 System.EnterCriticalSection(_c^.waiters_count_lock_);

 //mingw implement is wrong
 if true {(_c^.waiters_count_unblock_<>0)} then
 begin
  if (_c^.waiters_count_=0) then
  begin
   System.LeaveCriticalSection (_c^.waiters_count_lock_);
   Exit(0);
  end;
  relCnt:=_c^.waiters_count_;
  _c^.waiters_count_:=0;
  Inc(_c^.waiters_count_unblock_,relCnt);
 end else
 if false {(_c^.waiters_count_>_c^.waiters_count_gone_)} then
 begin
  r:=do_sema_b_wait(_c^.sema_b,INFINITE,_c^.waiters_b_lock_,_c^.value_b);
  if (r<>0) then
  begin
       //r:=do_sema_b_release (_c^.sema_b,1,_c^.waiters_b_lock_,_c^.value_b);
   System.LeaveCriticalSection(_c^.waiters_count_lock_);
   Exit(r);
  end;
  if (_c^.waiters_count_gone_<>0) then
  begin
   Dec(_c^.waiters_count_,_c^.waiters_count_gone_);
   _c^.waiters_count_gone_:=0;
  end;
  relCnt:=_c^.waiters_count_;
  _c^.waiters_count_:=0;
  _c^.waiters_count_unblock_:=relCnt;

  //System.LeaveCriticalSection(_c^.waiters_count_lock_);
        //r:=do_sema_b_release (_c^.sema_b,1,_c^.waiters_b_lock_,_c^.value_b);

 end else
 begin
  System.LeaveCriticalSection(_c^.waiters_count_lock_);
  Exit(0);
 end;

 LeaveCriticalSection(_c^.waiters_count_lock_);
 Result:=do_sema_b_release(_c^.sema_q,relCnt,_c^.waiters_q_lock_,_c^.value_q);
end;

function ps4_pthread_cond_wait(pCond:Ppthread_cond;pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
var
 //ch:sCondWaitHelper;
 r:Integer;
 _c:pthread_cond;
Label
 tryagain;
begin
 if (pCond=nil) then Exit(EINVAL);

 _c:=pCond^;
 if (_c=PTHREAD_COND_INITIALIZER) then
 begin
  r:=cond_static_init(pCond);
  if (r<>0) and (r<>EBUSY) then Exit(r);
  _c:=pCond^;
 end else
 if (_c^.valid<>LIFE_COND) then
  Exit(EINVAL);

 tryagain:
 r:=do_sema_b_wait(_c^.sema_b,INFINITE,_c^.waiters_b_lock_,_c^.value_b);
 if (r<>0) then Exit(r);

 if (System.TryEnterCriticalSection(_c^.waiters_count_lock_)=0) then
 begin
  r:=do_sema_b_release(_c^.sema_b,1,_c^.waiters_b_lock_,_c^.value_b);
  if (r<>0) then Exit(r);
  System.ThreadSwitch;
  goto tryagain;
 end;

 Inc(_c^.waiters_count_);

 LeaveCriticalSection(_c^.waiters_count_lock_);
 r:=do_sema_b_release (_c^.sema_b,1,_c^.waiters_b_lock_,_c^.value_b);
 if (r<>0) then Exit(r);

 //ch.c = _c;
 //ch.r = &r;
 //ch.external_mutex = external_mutex;
 //pthread_cleanup_push(cleanup_wait, (void *) &ch);

 r:=ps4_pthread_mutex_unlock(pMutex);
 //Writeln('ps4_pthread_mutex_unlock:',HexStr(pMutex),':',HexStr(r,8));
 if (r=0) then
 begin
  r:=do_sema_b_wait(_c^.sema_q,INFINITE,_c^.waiters_q_lock_,_c^.value_q);
 end;

 ps4_pthread_mutex_lock(pMutex); //WHY IT NO IN MINGW

 //pthread_cleanup_pop(1);
 Result:=r;
end;

function pthread_cond_timedwait_impl(c:Ppthread_cond;m:Ppthread_mutex;t:DWORD):Integer;
var
 //ch:sCondWaitHelper;
 r:Integer;
 _c:pthread_cond;
Label
 tryagain;
begin
 if (c=nil) then Exit(EINVAL);

 _c:=c^;
 if (_c=PTHREAD_COND_INITIALIZER) then
 begin
  r:=cond_static_init(c);
  if (r<>0) and (r<>EBUSY) then Exit(r);
  _c:=c^;
 end else
 if (_c^.valid<>LIFE_COND) then
  Exit(EINVAL);

 tryagain:
 r:=do_sema_b_wait(_c^.sema_b,INFINITE,_c^.waiters_b_lock_,_c^.value_b);
 if (r<>0) then Exit(r);

 if (System.TryEnterCriticalSection(_c^.waiters_count_lock_)=0) then
 begin
  r:=do_sema_b_release(_c^.sema_b,1,_c^.waiters_b_lock_,_c^.value_b);
  if (r<>0) then Exit(r);
  System.ThreadSwitch;
  goto tryagain;
 end;

 Inc(_c^.waiters_count_);
 System.LeaveCriticalSection(_c^.waiters_count_lock_);
 r:=do_sema_b_release(_c^.sema_b,1,_c^.waiters_b_lock_,_c^.value_b);
 if (r<>0) then Exit(r);

 //ch.c = _c;
 //ch.r = &r;
 //ch.external_mutex = external_mutex;
 //pthread_cleanup_push(cleanup_wait, (void *) &ch);

 r:=ps4_pthread_mutex_unlock(m);
 //Writeln('ps4_pthread_mutex_unlock:',HexStr(m),':',HexStr(r,8));
 if (r=0) then
 begin
  r:=do_sema_b_wait(_c^.sema_q,t,_c^.waiters_q_lock_,_c^.value_q);
 end;

 ps4_pthread_mutex_lock(m); //WHY IT NO IN MINGW

 //pthread_cleanup_pop(1);

 Result:=r;
end;


function ps4_pthread_cond_timedwait(pCond:Ppthread_cond;pMutex:Ppthread_mutex;ptime:Ptimespec):Integer; SysV_ABI_CDecl;
var
 t:DWORD;
begin
 if (ptime=nil) then
 begin
  Result:=ps4_pthread_cond_wait(pCond,pMutex);
 end else
 begin
  t:=dwMilliSecs(_pthread_rel_time_in_ms(ptime^));
  Result:=pthread_cond_timedwait_impl(pCond,pMutex,t);
 end;
end;

///////////////////////

function ps4_scePthreadCondattrInit(pAttr:Ppthread_condattr):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 pAttr^:=Default(pthread_condattr_t);
 Result:=0;
end;

function ps4_scePthreadCondattrDestroy(pAttr:Ppthread_condattr):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 pAttr^:=Default(pthread_condattr_t);
 Result:=0;
end;

function ps4_scePthreadCondInit(pCond:PScePthreadCond;pAttr:Ppthread_condattr;name:Pchar):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(pthread_cond_init(pCond,pAttr,name));
end;

function ps4_scePthreadCondDestroy(pCond:PScePthreadCond):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_cond_destroy(pCond));
end;

function ps4_scePthreadCondSignal(pCond:PScePthreadCond):Integer; SysV_ABI_CDecl;
begin
 //Writeln('>scePthreadCondSignal:',HexStr(pCond));
 Result:=px2sce(ps4_pthread_cond_signal(pCond));
 //Writeln('<scePthreadCondSignal:',HexStr(pCond),':',HexStr(Result,8));
end;

function ps4_scePthreadCondWait(pCond:PScePthreadCond;pMutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;
begin
 //Writeln('>scePthreadCondWait:',HexStr(pCond),':',HexStr(pMutex));
 Result:=px2sce(ps4_pthread_cond_wait(pCond,pMutex));
 //Writeln('<scePthreadCondWait:',HexStr(pCond),':',HexStr(pMutex),':',HexStr(Result,8));
 //Result:=0;
end;

//Time to wait (microseconds)
function ps4_scePthreadCondTimedwait(pCond:PScePthreadCond;pMutex:PScePthreadMutex;usec:DWORD):Integer; SysV_ABI_CDecl;
begin
 //Writeln('>scePthreadCondTimedwait:',HexStr(pCond),':',HexStr(pMutex));
 Result:=px2sce(pthread_cond_timedwait_impl(pCond,pMutex,_usec2msec(usec)));
 //if Result=SCE_KERNEL_ERROR_ETIMEDOUT then Writeln('SCE_KERNEL_ERROR_ETIMEDOUT');
end;

function ps4_scePthreadCondBroadcast(pCond:PScePthreadCond):Integer; SysV_ABI_CDecl;
begin
 //Writeln('>scePthreadCondBroadcast:',HexStr(pCond));
 Result:=px2sce(ps4_pthread_cond_broadcast(pCond));
 //Writeln('<scePthreadCondBroadcast:',HexStr(pCond),':',HexStr(Result,8));
end;

end.

