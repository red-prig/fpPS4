unit kern_timeout;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_synch,
 subr_sleepqueue,
 kern_thr,
 kern_mtx,
 kern_rwlock,
 kern_callout;

procedure kern_timeout_init();

procedure CC_LOCK(cc:p_callout_cpu);
procedure CC_UNLOCK(cc:p_callout_cpu);
procedure CC_LOCK_ASSERT(cc:p_callout_cpu);
function  callout_lock():p_callout_cpu;
procedure callout_cc_del(c:p_callout;cc:p_callout_cpu);

procedure callout_init (c:p_callout;mpsafe:Integer);
procedure _callout_init_lock(c:p_callout;lock:Pointer;flags:Integer);

procedure softclock_call_cc(c:p_callout;cc:p_callout_cpu);

function  _callout_stop_safe(c:p_callout;safe:Integer):Integer;
function  callout_reset_on(c:p_callout;to_ticks:Int64;ftn:t_callout_func;arg:Pointer):Integer;

function  callout_drain(c:p_callout):Integer;
function  callout_stop(c:p_callout):Integer;
procedure callout_init_mtx(c:p_callout;var mtx:mtx;flags:Integer);
procedure callout_init_rw(c:p_callout;var rw:Pointer;flags:Integer);
function  callout_reset(c:p_callout;on_tick:Int64;fn,arg:Pointer):Integer;
function  callout_reset_curcpu(c:p_callout;on_tick:Int64;fn,arg:Pointer):Integer;

implementation

uses
 md_timeout;

var
 timeout_cpu:t_callout_cpu;

 rw_giant:Pointer=nil;

procedure callout_cpu_init(cc:p_callout_cpu);
begin
 mtx_init(cc^.cc_lock,'callout');
 TAILQ_INIT(@cc^.cc_calllist);
 TAILQ_INIT(@cc^.cc_callfree);
end;

procedure kern_timeout_init();
begin
 callout_cpu_init(@timeout_cpu);
 md_start_softclock();
end;

procedure callout_init(c:p_callout;mpsafe:Integer);
begin
 c^:=Default(t_callout);
 if (mpsafe<>0) then
 begin
  c^.c_flags:=CALLOUT_RETURNUNLOCKED;
 end else
 begin
  c^.c_lock :=@rw_giant;
  c^.c_flags:=CALLOUT_RWLOCK;
 end;
end;

procedure _callout_init_lock(c:p_callout;lock:Pointer;flags:Integer);
begin
 c^:=Default(t_callout);
 c^.c_lock:=lock;
 Assert((flags and (not (CALLOUT_RETURNUNLOCKED or CALLOUT_SHAREDLOCK)))=0,'callout_init_lock: bad flags');
 Assert((lock<>nil) or ((flags and CALLOUT_RETURNUNLOCKED)=0),'callout_init_lock: CALLOUT_RETURNUNLOCKED with no lock');
 Assert(lock=nil,'invalid lock');
 c^.c_flags:=flags and (CALLOUT_RETURNUNLOCKED or CALLOUT_SHAREDLOCK);
end;

procedure CC_LOCK(cc:p_callout_cpu); inline;
begin
 mtx_lock(cc^.cc_lock);
end;

procedure CC_UNLOCK(cc:p_callout_cpu); inline;
begin
 mtx_unlock(cc^.cc_lock);
end;

procedure CC_LOCK_ASSERT(cc:p_callout_cpu); inline;
begin
 mtx_assert(cc^.cc_lock);
end;

function callout_lock():p_callout_cpu; inline;
begin
 Result:=@timeout_cpu;
 CC_LOCK(Result);
end;

procedure callout_cc_add(c:p_callout;cc:p_callout_cpu;to_ticks:Int64;func:t_callout_func;arg:Pointer);
begin
 CC_LOCK_ASSERT(cc);

 if (to_ticks <= 0) then
 begin
  to_ticks:=1;
 end;

 c^.c_arg:=arg;
 c^.c_flags:=c^.c_flags or (CALLOUT_ACTIVE or CALLOUT_PENDING);
 c^.c_func:=func;
 c^.c_time:=to_ticks;

 TAILQ_INSERT_TAIL(@cc^.cc_calllist,c,@c^.c_links);

 md_callout_new_inserted(c,cc);
end;

procedure callout_cc_del(c:p_callout;cc:p_callout_cpu);
begin
 md_callout_done(c);
 if ((c^.c_flags and CALLOUT_LOCAL_ALLOC)=0) then Exit;
 c^.c_func:=nil;
 TAILQ_INSERT_TAIL(@cc^.cc_callfree,c,@c^.c_links);
end;

procedure lc_lock(c:p_callout);
var
 c_lock :Pointer;
 c_flags:QWORD;
begin
 c_lock :=c^.c_lock;
 c_flags:=c^.c_flags;

 if (c_lock=nil) then Exit;

 if ((c_flags and CALLOUT_RWLOCK)<>0) then
 begin
  if ((c_flags and CALLOUT_SHAREDLOCK)<>0) then
  begin
   rw_rlock(PPointer(c_lock)^);
  end else
  begin
   rw_wlock(PPointer(c_lock)^);
  end;
 end else
 begin
  mtx_lock(p_mtx(c_lock)^);
 end;
end;

procedure lc_unlock(c:p_callout);
var
 c_lock :Pointer;
 c_flags:QWORD;
begin
 c_lock :=c^.c_lock;
 c_flags:=c^.c_flags;

 if (c_lock=nil) then Exit;

 if ((c_flags and CALLOUT_RWLOCK)<>0) then
 begin
  if ((c_flags and CALLOUT_SHAREDLOCK)<>0) then
  begin
   rw_runlock(PPointer(c_lock)^);
  end else
  begin
   rw_wunlock(PPointer(c_lock)^);
  end;
 end else
 begin
  mtx_unlock(p_mtx(c_lock)^);
 end;
end;

procedure softclock_call_cc(c:p_callout;cc:p_callout_cpu);
label
 skip;
var
 c_arg  :Pointer;
 c_func :t_callout_func;
 c_lock :Pointer;
 c_flags:QWORD;
begin
 Assert((c^.c_flags and (CALLOUT_PENDING or CALLOUT_ACTIVE))=(CALLOUT_PENDING or CALLOUT_ACTIVE),'softclock_call_cc');

 c_lock :=c^.c_lock;
 c_func :=c^.c_func;
 c_arg  :=c^.c_arg;
 c_flags:=c^.c_flags;

 if ((c^.c_flags and CALLOUT_LOCAL_ALLOC)<>0) then
  c^.c_flags:=CALLOUT_LOCAL_ALLOC
 else
  c^.c_flags:=c^.c_flags and (not CALLOUT_PENDING);

 cc^.cc_curr  :=c;
 cc^.cc_cancel:=0;
 CC_UNLOCK(cc);

 if (c_lock<>nil) then
 begin
  lc_lock(c);

  if (cc^.cc_cancel<>0) then
  begin
   lc_unlock(c);
   goto skip;
  end;

  cc^.cc_cancel:=1;
 end;

 THREAD_NO_SLEEPING();

 c_func(c_arg);

 THREAD_SLEEPING_OK();

 if ((c_flags and CALLOUT_RETURNUNLOCKED)=0) then
 begin
  lc_unlock(c);
 end;

 skip:
 CC_LOCK(cc);

 Assert(cc^.cc_curr=c,'mishandled cc_curr');

 cc^.cc_curr:=nil;

 if (cc^.cc_waiting<>0) then
 begin
  cc^.cc_waiting:=0;
  CC_UNLOCK(cc);
  wakeup(@cc^.cc_waiting);
  CC_LOCK(cc);
 end;

 Assert(((c_flags and CALLOUT_LOCAL_ALLOC)=0) or (c^.c_flags=CALLOUT_LOCAL_ALLOC),'corrupted callout');

 if ((c_flags and CALLOUT_LOCAL_ALLOC)<>0) then
 begin
  callout_cc_del(c, cc);
 end;
end;

{
 * New interface; clients allocate their own callout structures.
 *
 * callout_reset() - establish or change a timeout
 * callout_stop() - disestablish a timeout
 * callout_init() - initialize a callout structure so that it can
 * safely be passed to callout_reset() and callout_stop()
 *
 * defines three convenience macros:
 *
 * callout_active() - returns truth if callout has not been stopped,
 * drained, or deactivated since the last time the callout was
 * reset.
 * callout_pending() - returns truth if callout is still waiting for timeout
 * callout_deactivate() - marks the callout as having been serviced
 }
function callout_reset_on(c:p_callout;to_ticks:Int64;ftn:t_callout_func;arg:Pointer):Integer;
var
 cc:p_callout_cpu;
 cancelled:Integer;
begin
 cancelled:=0;

 cc:=callout_lock();

 md_callout_reset(c);

 if (cc^.cc_curr=c) then
 begin
  {
   * We're being asked to reschedule a callout which is
   * currently in progress.  If there is a lock then we
   * can cancel the callout if it has not really started.
   }
  if (c^.c_lock<>nil) and (cc^.cc_cancel=0) then
  begin
   cancelled:=1;
   cc^.cc_cancel:=1;
  end;

  if (cc^.cc_waiting<>0) then
  begin
   {
    * Someone has called callout_drain to kill this
    * callout.  Don't reschedule.
    }
   CC_UNLOCK(cc);
   Exit(cancelled);
  end;
 end;

 if ((c^.c_flags and CALLOUT_PENDING)<>0) then
 begin
  TAILQ_REMOVE(@cc^.cc_calllist,c,@c^.c_links);

  cancelled:=1;
  c^.c_flags:=c^.c_flags and (not (CALLOUT_ACTIVE or CALLOUT_PENDING));
 end;

 callout_cc_add(c, cc, to_ticks, ftn, arg);

 CC_UNLOCK(cc);

 Exit(cancelled);
end;

function _callout_stop_safe(c:p_callout;safe:Integer):Integer;
label
 again;
var
 cc:p_callout_cpu;
 use_lock :Integer;
 sq_locked:Integer;
begin
 sq_locked:=0;

 use_lock:=ord((safe=0) and (c^.c_lock<>nil));

 again:
 cc:=callout_lock();

 md_callout_stop(c);

 if ((c^.c_flags and CALLOUT_PENDING)=0) then
 begin
  c^.c_flags:=c^.c_flags and (not CALLOUT_ACTIVE);

  if (cc^.cc_curr<>c) then
  begin
   CC_UNLOCK(cc);
   if (sq_locked<>0) then
   begin
    sleepq_release(@cc^.cc_waiting);
   end;
   Exit(0);
  end;

  if (safe<>0) then
  begin
   while (cc^.cc_curr=c) do
   begin

    if (sq_locked=0) then
    begin
     CC_UNLOCK(cc);
     sleepq_lock(@cc^.cc_waiting);
     sq_locked:=1;
     goto again;
    end;

    cc^.cc_waiting:=1;

    CC_UNLOCK(cc);
    sleepq_add(@cc^.cc_waiting,nil,PChar('codrain'),SLEEPQ_SLEEP,0);
    sleepq_wait(@cc^.cc_waiting,0);
    sq_locked:=0;

    CC_LOCK(cc);
   end;
  end else
  if (use_lock<>0) and (cc^.cc_cancel=0) then
  begin
   cc^.cc_cancel:=1;

   CC_UNLOCK(cc);
   Assert(sq_locked=0,'sleepqueue chain locked');
   Exit(1);
  end;

  CC_UNLOCK(cc);
  Assert(sq_locked=0,'sleepqueue chain still locked');
  Exit(0);
 end;

 if (sq_locked<>0) then
 begin
  sleepq_release(@cc^.cc_waiting);
 end;

 c^.c_flags:=c^.c_flags and (not (CALLOUT_ACTIVE or CALLOUT_PENDING));

 TAILQ_REMOVE(@cc^.cc_calllist,c,@c^.c_links);

 callout_cc_del(c, cc);

 CC_UNLOCK(cc);
 Exit(1);
end;

//

function callout_drain(c:p_callout):Integer;
begin
 Result:=_callout_stop_safe(c,1);
end;

function callout_stop(c:p_callout):Integer;
begin
 Result:=_callout_stop_safe(c,0);
end;

procedure callout_init_mtx(c:p_callout;var mtx:mtx;flags:Integer);
begin
 _callout_init_lock(c,@mtx,flags);
end;

procedure callout_init_rw(c:p_callout;var rw:Pointer;flags:Integer);
begin
 _callout_init_lock(c,@rw,flags or CALLOUT_RWLOCK);
end;

function callout_reset(c:p_callout;on_tick:Int64;fn,arg:Pointer):Integer;
begin
 Result:=callout_reset_on(c,on_tick,t_callout_func(fn),arg);
end;

function callout_reset_curcpu(c:p_callout;on_tick:Int64;fn,arg:Pointer):Integer;
begin
 Result:=callout_reset_on(c,on_tick,t_callout_func(fn),arg);
end;


end.

