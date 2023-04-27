unit subr_sleepqueue;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 hamt,
 kern_mtx,
 kern_thr,
 rtprio;

const
 SLEEPQ_TYPE         =$ff;  // Mask of sleep queue types.
 SLEEPQ_SLEEP        =$00;  // Used by sleep/wakeup.
 SLEEPQ_CONDVAR      =$01;  // Used for a cv.
 SLEEPQ_PAUSE        =$02;  // Used by pause.
 SLEEPQ_SX           =$03;  // Used by an sx lock.
 SLEEPQ_LK           =$04;  // Used by a lockmgr.
 SLEEPQ_INTERRUPTIBLE=$100; // Sleep is interruptible.
 SLEEPQ_STOP_ON_BDRY =$200; // Stop sleeping thread

 SC_TABLESIZE=128;
 SC_MASK     =(SC_TABLESIZE-1);
 SC_SHIFT    =8;

 NR_SLEEPQS=2;

type
 p_sleepqueue=^sleepqueue;
 sleepqueue=packed record
  sq_blocked   :array[0..NR_SLEEPQS-1] of TAILQ_HEAD;
  sq_blockedcnt:array[0..NR_SLEEPQS-1] of DWORD;
  sq_hash      :LIST_ENTRY;
  sq_free      :p_sleepqueue;
  sq_wchan     :Pointer;
  sq_type      :Integer;
 end;

 p_sleepqueue_chain=^sleepqueue_chain;
 sleepqueue_chain=packed record
  sc_hamt:TSTUB_HAMT64;
  sc_lock:mtx;
 end;

function  sleepq_alloc:p_sleepqueue;
procedure sleepq_free(sq:p_sleepqueue); inline;
procedure sleepq_lock(wchan:Pointer);
procedure sleepq_release(wchan:Pointer);
procedure sleepq_add(wchan,lock,wmesg:Pointer;flags,queue:Integer);
procedure sleepq_set_timeout(wchan:Pointer;time:Int64);
function  sleepq_sleepcnt(wchan,lock:Pointer;flags,queue:Integer):DWORD;
procedure sleepq_wait(wchan:Pointer;pri:Integer);
function  sleepq_wait_sig(wchan:Pointer;pri:Integer):Integer;
function  sleepq_timedwait(wchan:Pointer;pri:Integer):Integer;
function  sleepq_timedwait_sig(wchan:Pointer;pri:Integer):Integer;
function  sleepq_get_type(wchan:Pointer;pri:Integer):Integer;
function  sleepq_signal(wchan:Pointer;flags,pri,queue:Integer):Integer;
function  sleepq_signalto(wchan:Pointer;flags,pri,queue:Integer;std:p_kthread):Integer;
function  sleepq_broadcast(wchan:Pointer;flags,pri,queue:Integer):Integer;
procedure sleepq_remove(td:p_kthread;wchan:Pointer);
function  sleepq_abort(td:p_kthread;intrval:Integer):Integer;

procedure init_sleepqueues; //SYSINIT

implementation

uses
 errno,
 signalvar,
 kern_thread,
 kern_sig,
 sched_ule;

var
 sleepq_chains:array[0..SC_MASK] of sleepqueue_chain;

procedure init_sleepqueues;
var
 i:Integer;
begin
 For i:=0 to SC_MASK do
 begin
  mtx_init(sleepq_chains[i].sc_lock,'sleepq chain');
 end;
end;

procedure sleepq_switch(wchan:Pointer;pri:Integer); forward;
function  sleepq_resume_thread(sq:p_sleepqueue;td:p_kthread;pri:Integer):Integer; forward;
procedure sleepq_timeout(arg:Pointer); forward;

function sleepq_alloc:p_sleepqueue;
var
 i:Integer;
begin
 Result:=AllocMem(SizeOf(sleepqueue));
 For i:=0 to NR_SLEEPQS-1 do
 begin
  TAILQ_INIT(@Result^.sq_blocked[i]);
 end;
 LIST_INIT(@Result^.sq_free);
end;

procedure sleepq_free(sq:p_sleepqueue); inline;
begin
 FreeMem(sq);
end;

function SC_HASH(wc:Pointer):DWORD; inline;
begin
 Result:=(ptruint(wc) shr SC_SHIFT) and SC_MASK;
end;

function SC_LOOKUP(wc:Pointer):p_sleepqueue_chain; inline;
begin
 Result:=@sleepq_chains[SC_HASH(wc)];
end;

procedure sleepq_lock(wchan:Pointer);
var
 sc:p_sleepqueue_chain;
begin
 sc:=SC_LOOKUP(wchan);
 mtx_lock(sc^.sc_lock);
end;

procedure sleepq_release(wchan:Pointer);
var
 sc:p_sleepqueue_chain;
begin
 sc:=SC_LOOKUP(wchan);
 mtx_unlock(sc^.sc_lock);
end;

function sleepq_lookup(wchan:Pointer):p_sleepqueue;
var
 sc:p_sleepqueue_chain;
 data:PPointer;
begin
 Result:=nil;
 sc:=SC_LOOKUP(wchan);
 data:=HAMT_search64(@sc^.sc_hamt,QWORD(wchan));
 if (data<>nil) then Result:=data^;
end;

procedure HAMT_INSERT(wchan:Pointer;sq:p_sleepqueue); inline;
var
 sc:p_sleepqueue_chain;
begin
 sc:=SC_LOOKUP(wchan);
 HAMT_insert64(@sc^.sc_hamt,QWORD(wchan),sq);
end;

procedure HAMT_REMOVE(wchan:Pointer); inline;
var
 sc:p_sleepqueue_chain;
begin
 sc:=SC_LOOKUP(wchan);
 HAMT_delete64(@sc^.sc_hamt,QWORD(wchan));
end;

procedure sleepq_add(wchan,lock,wmesg:Pointer;flags,queue:Integer);
var
 td:p_kthread;
 sq:p_sleepqueue;
begin
 td:=curkthread;

 Assert(td^.td_sleepqueue<>nil);
 Assert(wchan<>nil,'invalid nil wait channel');
 Assert((queue>=0) and (queue<NR_SLEEPQS));

 Assert((td^.td_pflags and TDP_NOSLEEPING)=0,'Trying sleep, but thread marked as sleeping prohibited');

 sq:=sleepq_lookup(wchan);

 if (sq=nil) then
 begin
  sq:=td^.td_sleepqueue;
  sq^.sq_wchan:=wchan;
  sq^.sq_type :=flags and SLEEPQ_TYPE;
  HAMT_INSERT(wchan,sq);
 end else
 begin
  Assert(wchan=sq^.sq_wchan);
  Assert((flags and SLEEPQ_TYPE)=sq^.sq_type);
  sq:=td^.td_sleepqueue;
  LIST_INSERT_HEAD(@sq^.sq_free,sq,@sq^.sq_hash);
 end;

 thread_lock(td);
 TAILQ_INSERT_TAIL(@sq^.sq_blocked[queue],td,@td^.td_slpq);
 Inc(sq^.sq_blockedcnt[queue]);
 td^.td_sleepqueue:=nil;
 td^.td_sqqueue   :=queue;
 td^.td_wchan     :=wchan;
 if ((flags and SLEEPQ_INTERRUPTIBLE)<>0) then
 begin
  td^.td_flags:=td^.td_flags or TDF_SINTR;
  td^.td_flags:=td^.td_flags and (not TDF_SLEEPABORT);
 end;
 thread_unlock(td);
end;

procedure sleepq_set_timeout(wchan:Pointer;time:Int64);
var
 td:p_kthread;
begin
 td:=curkthread;

 Assert(TD_ON_SLEEPQ(td));
 Assert(td^.td_sleepqueue=nil);
 Assert(wchan<>nil);

 td^.td_timeo:=time;
end;

function sleepq_sleepcnt(wchan,lock:Pointer;flags,queue:Integer):DWORD;
var
 sq:p_sleepqueue;
begin
 Result:=0;
 Assert(wchan<>nil,'invalid nil wait channel');
 Assert((queue>=0) and (queue<NR_SLEEPQS));
 sq:=sleepq_lookup(wchan);
 if (sq=nil) then Exit;
 Result:=sq^.sq_blockedcnt[queue];
end;

function sleepq_catch_signals(wchan:Pointer;pri:Integer):Integer;
label
 _out;
var
 td:p_kthread;
 sc:p_sleepqueue_chain;
 sq:p_sleepqueue;
 sig,stop_allowed:Integer;
begin
 td:=curkthread;

 sc:=SC_LOOKUP(wchan);

 Assert(wchan<>nil);
 if ((td^.td_pflags and TDP_WAKEUP)<>0) then
 begin
  td^.td_pflags:=td^.td_pflags and (not TDP_WAKEUP);
  Result:=EINTR;
  thread_lock(td);
  goto _out;
 end;

 thread_lock(td);
 if ((td^.td_flags and (TDF_NEEDSIGCHK or TDF_NEEDSUSPCHK))=0) then
 begin
  sleepq_switch(wchan,pri);
  Exit(0);
 end;

 if ((td^.td_flags and TDF_SBDRY)<>0) then
 begin
  stop_allowed:=SIG_STOP_NOT_ALLOWED;
 end else
 begin
  stop_allowed:=SIG_STOP_ALLOWED;
 end;

 thread_unlock(td);

 mtx_unlock(sc^.sc_lock);

 PROC_LOCK;

 ps_mtx_lock;

 sig:=cursig(td,stop_allowed);
 if (sig=0) then
 begin
  ps_mtx_unlock;
  Result:=0;
 end else
 begin
  if (SIGISMEMBER(@p_sigacts.ps_sigintr,sig)) then
  begin
   Result:=EINTR;
  end else
  begin
   Result:=ERESTART;
  end;
  ps_mtx_unlock;
 end;

 mtx_lock(sc^.sc_lock);

 thread_lock(td);
 PROC_UNLOCK;

 if (Result=0) then
 begin
  sleepq_switch(wchan,pri);
  Exit(0);
 end;

 _out:
  if TD_ON_SLEEPQ(td) then
  begin
   sq:=sleepq_lookup(wchan);
   sleepq_resume_thread(sq,td,0);
  end;

  mtx_unlock(sc^.sc_lock);

  Assert(td^.td_lock<>@sc^.sc_lock);
end;

procedure sleepq_switch(wchan:Pointer;pri:Integer);
var
 td:p_kthread;
 sc:p_sleepqueue_chain;
 sq:p_sleepqueue;
 r:Integer;
begin
 td:=curkthread;
 sc:=SC_LOOKUP(wchan);

 if (td^.td_sleepqueue<>nil) then
 begin
  mtx_unlock(sc^.sc_lock);
  Exit;
 end;

 if ((td^.td_flags and TDF_TIMEOUT)<>0) then
 begin
  Assert(TD_ON_SLEEPQ(td));
  sq:=sleepq_lookup(wchan);
  sleepq_resume_thread(sq,td,0);
  mtx_unlock(sc^.sc_lock);
  Exit;
 end;

 Assert(td^.td_sleepqueue=nil);
 sched_sleep(td,pri);
 TD_SET_SLEEPING(td);

 thread_unlock(td);       //
 mtx_unlock(sc^.sc_lock); //
 r:=sched_switch(td);
 mtx_lock(sc^.sc_lock);   //
 thread_lock(td);         //

 if (r=ETIMEDOUT) then
 begin
  sleepq_timeout(td);
 end;

 Assert(TD_IS_RUNNING(td),'running but not TDS_RUNNING');
end;

function sleepq_check_timeout():Integer;
var
 td:p_kthread;
begin
 Result:=0;
 td:=curkthread;

 if ((td^.td_flags and TDF_TIMEOUT)<>0) then
 begin
  td^.td_flags:=td^.td_flags and (not TDF_TIMEOUT);
  Exit(EWOULDBLOCK);
 end;

 if ((td^.td_flags and TDF_TIMOFAIL)<>0) then
 begin
  td^.td_flags:=td^.td_flags and (not TDF_TIMOFAIL);
 end;
end;

function sleepq_check_signals():Integer;
var
 td:p_kthread;
begin
 td:=curkthread;

 if ((td^.td_flags and TDF_SINTR)<>0) then
 begin
  td^.td_flags:=td^.td_flags and (not TDF_SINTR);
 end;

 if ((td^.td_flags and TDF_SLEEPABORT)<>0) then
 begin
  td^.td_flags:=td^.td_flags and (not TDF_SLEEPABORT);
  Exit(td^.td_intrval);
 end;

 Result:=0;
end;

procedure sleepq_wait(wchan:Pointer;pri:Integer);
var
 td:p_kthread;
begin
 td:=curkthread;

 Assert((td^.td_flags and TDF_SINTR)=0);
 thread_lock(td);
 sleepq_switch(wchan,pri);
 thread_unlock(td);
end;

function sleepq_wait_sig(wchan:Pointer;pri:Integer):Integer;
var
 rcatch:Integer;
begin
 rcatch:=sleepq_catch_signals(wchan,pri);
 Result:=sleepq_check_signals();
 thread_unlock(curkthread);
 if (rcatch<>0) then
 begin
  Exit(rcatch);
 end;
end;

function sleepq_timedwait(wchan:Pointer;pri:Integer):Integer;
var
 td:p_kthread;
begin
 td:=curkthread;

 Assert((td^.td_flags and TDF_SINTR)=0);
 thread_lock(td);
 sleepq_switch(wchan,pri);
 Result:=sleepq_check_timeout();
 thread_unlock(td);
end;

function sleepq_timedwait_sig(wchan:Pointer;pri:Integer):Integer;
var
 rcatch,rvalt,rvals:Integer;
begin
 rcatch:=sleepq_catch_signals(wchan,pri);
 rvalt :=sleepq_check_timeout();
 rvals :=sleepq_check_signals();
 thread_unlock(curkthread);
 if (rcatch<>0) then
 begin
  Exit(rcatch);
 end;
 if (rvals<>0) then
 begin
  Exit(rvals);
 end;
 Exit(rvalt);
end;

function sleepq_get_type(wchan:Pointer;pri:Integer):Integer;
var
 sq:p_sleepqueue;
begin
 Assert(wchan<>nil);

 sleepq_lock(wchan);
 sq:=sleepq_lookup(wchan);
 if (sq=nil) then
 begin
  sleepq_release(wchan);
  Exit(-1);
 end;
 Result:=sq^.sq_type;
 sleepq_release(wchan);
end;

function sleepq_resume_thread(sq:p_sleepqueue;td:p_kthread;pri:Integer):Integer;
begin
 Result:=0;

 Assert(td<>nil);
 Assert(sq^.sq_wchan<>nil);
 Assert(td^.td_wchan=sq^.sq_wchan);
 Assert((td^.td_sqqueue<NR_SLEEPQS) and (td^.td_sqqueue>=0));

 Dec(sq^.sq_blockedcnt[td^.td_sqqueue]);
 TAILQ_REMOVE(@sq^.sq_blocked[td^.td_sqqueue],td,@td^.td_slpq);

 if (LIST_EMPTY(@sq^.sq_free)) then
 begin
  td^.td_sleepqueue:=sq;
 end else
 begin
  td^.td_sleepqueue:=LIST_FIRST(@sq^.sq_free);
 end;

 sq:=td^.td_sleepqueue;

 if (sq^.sq_hash.le_next=nil) then
 begin
  HAMT_REMOVE(sq^.sq_wchan);
 end else
 begin
  LIST_REMOVE(sq,@sq^.sq_hash);
 end;

 td^.td_wchan:=nil;
 td^.td_flags:=td^.td_flags and (not TDF_SINTR);

 Assert((pri=0) or ((pri>=PRI_MIN) and (pri<=PRI_MAX)));
 if (pri<>0) and
    (td^.td_priority>pri) and
    (PRI_BASE(td^.td_pri_class)=PRI_TIMESHARE) then
 begin
  sched_prio(td,pri);
 end;

 if TD_IS_SLEEPING(td) then
 begin
  TD_CLR_SLEEPING(td);
  Exit(setrunnable(td));
 end;
end;

function sleepq_signal(wchan:Pointer;flags,pri,queue:Integer):Integer;
var
 td,besttd:p_kthread;
 sq:p_sleepqueue;
begin
 Result:=0;
 Assert(wchan<>nil,'invalid nil wait channel');
 Assert((queue>=0) and (queue<NR_SLEEPQS));

 sq:=sleepq_lookup(wchan);
 if (sq=nil) then Exit;

 Assert(sq^.sq_type=(flags and SLEEPQ_TYPE),'mismatch between sleep/wakeup and cv_*');

 besttd:=nil;
 td:=TAILQ_FIRST(@sq^.sq_blocked[queue]);
 While (td<>nil) do
 begin
  if (besttd=nil) or (td^.td_priority<besttd^.td_priority) then
  begin
   besttd:=td;
  end;
  td:=TAILQ_NEXT(td,@td^.td_slpq);
 end;

 Assert(besttd<>nil);
 thread_lock(besttd);
 Result:=sleepq_resume_thread(sq,besttd,pri);
 thread_unlock(besttd);
end;

function sleepq_signalto(wchan:Pointer;flags,pri,queue:Integer;std:p_kthread):Integer;
var
 td:p_kthread;
 sq:p_sleepqueue;
begin
 Result:=-1;

 Assert(wchan<>nil,'invalid nil wait channel');
 Assert((queue>=0) and (queue<NR_SLEEPQS));

 sq:=sleepq_lookup(wchan);
 if (sq=nil) then Exit;

 Assert(sq^.sq_type=(flags and SLEEPQ_TYPE),'mismatch between sleep/wakeup and cv_*');

 td:=TAILQ_FIRST(@sq^.sq_blocked[queue]);
 While (td<>nil) and (td<>std) do
 begin
  td:=TAILQ_NEXT(td,@td^.td_slpq);
 end;

 if (td=std) then
 begin
  thread_lock(std);
  Result:=sleepq_resume_thread(sq,std,pri);
  thread_unlock(std);
 end;
end;

function sleepq_broadcast(wchan:Pointer;flags,pri,queue:Integer):Integer;
var
 td:p_kthread;
 sq:p_sleepqueue;
begin
 Result:=0;

 Assert(wchan<>nil,'invalid nil wait channel');
 Assert((queue>=0) and (queue<NR_SLEEPQS));

 sq:=sleepq_lookup(wchan);
 if (sq=nil) then Exit;

 Assert(sq^.sq_type=(flags and SLEEPQ_TYPE),'mismatch between sleep/wakeup and cv_*');

 td:=TAILQ_FIRST(@sq^.sq_blocked[queue]);
 While (td<>nil) do
 begin
  thread_lock(td);
  if (sleepq_resume_thread(sq,td,pri)<>0) then
  begin
   Result:=1;
  end;
  thread_unlock(td);
  td:=TAILQ_NEXT(td,@td^.td_slpq);
 end;
end;

procedure sleepq_timeout(arg:Pointer);
var
 td:p_kthread;
 sq:p_sleepqueue;
 wchan:Pointer;
begin
 td:=arg;

 //thread_lock(td);
 if (TD_IS_SLEEPING(td) and TD_ON_SLEEPQ(td)) then
 begin
  wchan:=td^.td_wchan;
  sq:=sleepq_lookup(wchan);
  Assert(sq<>nil);
  td^.td_flags:=td^.td_flags or TDF_TIMEOUT;
  sleepq_resume_thread(sq,td,0);
  thread_unlock(td);
  Exit;
 end;

 if TD_ON_SLEEPQ(td) then
 begin
  td^.td_flags:=td^.td_flags or TDF_TIMEOUT;
  thread_unlock(td);
  Exit;
 end;

 if ((td^.td_flags and TDF_TIMEOUT)<>0) then
 begin
  Assert(TD_IS_SLEEPING(td));
  td^.td_flags:=td^.td_flags and (not TDF_TIMEOUT);
  TD_CLR_SLEEPING(td);
  setrunnable(td);
 end else
 begin
  td^.td_flags:=td^.td_flags or TDF_TIMOFAIL;
 end;

 //thread_unlock(td);
end;

procedure sleepq_remove(td:p_kthread;wchan:Pointer);
var
 sq:p_sleepqueue;
begin

 Assert(wchan<>nil);
 sleepq_lock(wchan);
 sq:=sleepq_lookup(wchan);

 if (not TD_ON_SLEEPQ(td)) or (td^.td_wchan<>wchan) then
 begin
  sleepq_release(wchan);
  Exit;
 end;

 thread_lock(td);
 Assert(sq<>nil);
 Assert(td^.td_wchan=wchan);
 sleepq_resume_thread(sq,td,0);
 thread_unlock(td);
 sleepq_release(wchan);
end;

function sleepq_abort(td:p_kthread;intrval:Integer):Integer;
var
 sq:p_sleepqueue;
 wchan:Pointer;
begin
 Assert(TD_ON_SLEEPQ(td));
 Assert((td^.td_flags and TDF_SINTR)<>0);
 Assert((intrval=EINTR) or (intrval=ERESTART));

 if ((td^.td_flags and TDF_TIMEOUT)<>0) then
 begin
  Exit(0);
 end;

 td^.td_intrval:=intrval;
 td^.td_flags:=td^.td_flags or TDF_SLEEPABORT;

 if (not TD_IS_SLEEPING(td)) then
 begin
  Exit(0);
 end;

 wchan:=td^.td_wchan;
 Assert(wchan<>nil);
 sq:=sleepq_lookup(wchan);
 Assert(sq<>nil);

 Result:=sleepq_resume_thread(sq,td,0);
end;

end.

