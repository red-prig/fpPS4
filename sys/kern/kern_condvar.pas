unit kern_condvar;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 time,
 kern_thr,
 sys_sleepqueue;

type
 p_cv=^t_cv;
 t_cv=packed record
  cv_description:Pchar;
  cv_waiters    :Integer;
 end;

procedure cv_init(cvp:p_cv;desc:Pchar);
procedure cv_destroy(cvp:p_cv);
procedure _cv_wait(cvp:p_cv;lock:Pointer);
procedure _cv_wait_unlock(cvp:p_cv;lock:Pointer);
function  _cv_wait_sig(cvp:p_cv;lock:Pointer):Integer;
function  _cv_timedwait(cvp:p_cv;lock:Pointer;timo:Int64):Integer;
function  _cv_timedwait_sig(cvp:p_cv;lock:Pointer;timo:Int64):Integer;
function  _cv_timedwait_sig_proctime(cvp:p_cv;lock:Pointer;timeout:p_timespec):Integer;
procedure cv_signal(cvp:p_cv);
procedure cv_signalto(cvp:p_cv;td:p_kthread);
procedure cv_broadcastpri(cvp:p_cv;pri:Integer);

implementation

uses
 errno,
 kern_mtx,
 md_time;

function IS_SLEEPABLE(lock:Pointer):Boolean; inline;
begin
 Result:=False;
end;

procedure lc_lock(lock:Pointer); inline;
begin
 mtx_lock(p_mtx(lock)^);
end;

procedure lc_unlock(lock:Pointer); inline;
begin
 mtx_unlock(p_mtx(lock)^);
end;

procedure cv_init(cvp:p_cv;desc:Pchar);
begin
 cvp^.cv_description:=desc;
 cvp^.cv_waiters    :=0;
end;

procedure cv_destroy(cvp:p_cv);
begin
 //
end;

procedure CV_ASSERT(cvp:p_cv;lock,td:Pointer);
begin
 Assert(td<>nil,'td nil');
 Assert(TD_IS_RUNNING(td),'not TDS_RUNNING');
 Assert(cvp<>nil,'cvp nil');
 Assert(lock<>nil,'lock nil');
end;

procedure _cv_wait(cvp:p_cv;lock:Pointer);
var
 td:p_kthread;
begin
 td:=curkthread;

 CV_ASSERT(cvp,lock,td);

 sleepq_lock(cvp);

 Inc(cvp^.cv_waiters);

 sleepq_add(cvp,lock,cvp^.cv_description,SLEEPQ_CONDVAR,0);

 if IS_SLEEPABLE(lock) then
 begin
  sleepq_release(cvp);
 end;

 lc_unlock(lock);
 if IS_SLEEPABLE(lock) then
 begin
  sleepq_lock(cvp);
 end;

 sleepq_wait(cvp,0);

 lc_lock(lock);
end;

procedure _cv_wait_unlock(cvp:p_cv;lock:Pointer);
var
 td:p_kthread;
begin
 td:=curkthread;

 CV_ASSERT(cvp,lock,td);

 sleepq_lock(cvp);

 Inc(cvp^.cv_waiters);

 sleepq_add(cvp,lock,cvp^.cv_description,SLEEPQ_CONDVAR,0);
 if IS_SLEEPABLE(lock) then
 begin
  sleepq_release(cvp);
 end;
 lc_unlock(lock);
 if IS_SLEEPABLE(lock) then
 begin
  sleepq_lock(cvp);
 end;
 sleepq_wait(cvp,0);
end;

function _cv_wait_sig(cvp:p_cv;lock:Pointer):Integer;
var
 td:p_kthread;
begin
 td:=curkthread;

 CV_ASSERT(cvp,lock,td);

 sleepq_lock(cvp);

 Inc(cvp^.cv_waiters);

 sleepq_add(cvp,lock,cvp^.cv_description,SLEEPQ_CONDVAR or SLEEPQ_INTERRUPTIBLE,0);

 if IS_SLEEPABLE(lock) then
 begin
  sleepq_release(cvp);
 end;

 lc_unlock(lock);
 if IS_SLEEPABLE(lock) then
 begin
  sleepq_lock(cvp);
 end;

 Result:=sleepq_wait_sig(cvp,0);

 lc_lock(lock);
end;

function _cv_timedwait(cvp:p_cv;lock:Pointer;timo:Int64):Integer;
var
 td:p_kthread;
begin
 td:=curkthread;

 CV_ASSERT(cvp,lock,td);

 sleepq_lock(cvp);

 Inc(cvp^.cv_waiters);

 sleepq_add(cvp,lock,cvp^.cv_description,SLEEPQ_CONDVAR,0);
 sleepq_set_timeout(cvp,timo);

 if IS_SLEEPABLE(lock) then
 begin
  sleepq_release(cvp);
 end;

 lc_unlock(lock);
 if IS_SLEEPABLE(lock) then
 begin
  sleepq_lock(cvp);
 end;

 Result:=sleepq_timedwait(cvp,0);

 lc_lock(lock);
end;

function _cv_timedwait_sig(cvp:p_cv;lock:Pointer;timo:Int64):Integer;
var
 td:p_kthread;
begin
 td:=curkthread;

 CV_ASSERT(cvp,lock,td);

 sleepq_lock(cvp);

 Inc(cvp^.cv_waiters);

 sleepq_add(cvp,lock,cvp^.cv_description,SLEEPQ_CONDVAR or SLEEPQ_INTERRUPTIBLE,0);
 sleepq_set_timeout(cvp,timo);

 if IS_SLEEPABLE(lock) then
 begin
  sleepq_release(cvp);
 end;

 lc_unlock(lock);
 if IS_SLEEPABLE(lock) then
 begin
  sleepq_lock(cvp);
 end;

 Result:=sleepq_timedwait_sig(cvp,0);

 lc_lock(lock);
end;

function _cv_timedwait_sig_proctime(cvp:p_cv;lock:Pointer;timeout:p_timespec):Integer;
var
 tv,ts,ts2:Int64;
begin
 tv:=TIMESPEC_TO_UNIT(timeout);
 ts:=get_unit_uptime;
 ts:=ts+tv;

 repeat
  Result:=_cv_timedwait_sig(cvp,lock,tvtohz(tv));
  if (Result<>EWOULDBLOCK) then Break;

  ts2:=get_unit_uptime;
  if (ts2>=ts) then
  begin
   Result:=EWOULDBLOCK;
   Break;
  end;

  tv:=ts-ts2;
 until false;
end;

procedure cv_signal(cvp:p_cv);
begin
 sleepq_lock(cvp);
 if (cvp^.cv_waiters>0) then
 begin
  Dec(cvp^.cv_waiters);
  sleepq_signal(cvp,SLEEPQ_CONDVAR,0,0);
 end;
 sleepq_release(cvp);
end;

procedure cv_signalto(cvp:p_cv;td:p_kthread);
begin
 sleepq_lock(cvp);
 if (cvp^.cv_waiters>0) then
 begin
  if (sleepq_signalto(cvp,SLEEPQ_CONDVAR,0,0,td)<>-1) then
  begin
   Dec(cvp^.cv_waiters);
  end;
 end;
 sleepq_release(cvp);
end;

procedure cv_broadcastpri(cvp:p_cv;pri:Integer);
begin
 if (pri=-1) then pri:=0;
 sleepq_lock(cvp);
 if (cvp^.cv_waiters>0) then
 begin
  cvp^.cv_waiters:=0;
  sleepq_broadcast(cvp,SLEEPQ_CONDVAR,pri,0);
 end;
 sleepq_release(cvp);
end;




end.

