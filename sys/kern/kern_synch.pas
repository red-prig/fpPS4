unit kern_synch;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_sleepqueue,
 kern_param,
 kern_mtx,
 kern_thr;

function  msleep(ident   :Pointer;
                 lock    :p_mtx;
                 priority:Integer;
                 wmesg   :PChar;
                 timo    :Int64):Integer;

function  tsleep(ident   :Pointer;
                 priority:Integer;
                 wmesg   :PChar;
                 timo    :Int64):Integer;

function  pause(wmesg:PChar;timo:Int64):Integer;

procedure wakeup(ident:Pointer);
procedure wakeup_one(ident:Pointer);
function  mi_switch(flags:Integer):Integer;

procedure maybe_yield();
procedure kern_yield(prio:Integer);
function  sys_yield():Integer;
function  sys_sched_yield():Integer;
function  sys_cpumode_yield():Integer;

implementation

uses
 kern_proc,
 kern_thread,
 sched_ule,
 md_sleep,
 rtprio;

var
 pause_wchan:Integer=0;

function msleep(ident   :Pointer;
                lock    :p_mtx;
                priority:Integer;
                wmesg   :PChar;
                timo    :Int64):Integer; public;
var
 td:p_kthread;
 catch,flags,pri:Integer;
begin
 td:=curkthread;

 catch:=priority and PCATCH;
 pri  :=priority and PRIMASK;

 if (TD_ON_SLEEPQ(td)) then
 begin
  sleepq_remove(td,td^.td_wchan);
 end;

 if (ident=@pause_wchan) then
  flags:=SLEEPQ_PAUSE
 else
  flags:=SLEEPQ_SLEEP;

 if (catch<>0) then
 begin
  flags:=flags or SLEEPQ_INTERRUPTIBLE;
 end;

 if ((priority and PBDRY)<>0) then
 begin
  flags:=flags or SLEEPQ_STOP_ON_BDRY;
 end;

 sleepq_lock(ident);

 if (lock<>nil) then //not LC_SLEEPABLE
 begin
  mtx_unlock(lock^);
 end;

 {
  * We put ourselves on the sleep queue and start our timeout
  * before calling thread_suspend_check, as we could stop there,
  * and a wakeup or a SIGCONT (or both) could occur while we were
  * stopped without resuming us.  Thus, we must be ready for sleep
  * when cursig() is called.  If the wakeup happens while we're
  * stopped, then td will no longer be on a sleep queue upon
  * return from cursig().
 }

 sleepq_add(ident,lock,wmesg,flags,0);

 if (timo<>0) then
 begin
  sleepq_set_timeout(ident,timo);
 end;

 if (timo<>catch) then
  Result:=sleepq_timedwait_sig(ident,pri)
 else if (timo<>0) then
  Result:=sleepq_timedwait(ident,pri)
 else if (catch<>0) then
  Result:=sleepq_wait_sig(ident,pri)
 else
 begin
  sleepq_wait(ident,pri);
  Result:=0;
 end;

 //sleepq_release in sleepq_switch

 if (lock<>nil) and ((priority and PDROP)=0) then
 begin
  mtx_lock(lock^);
 end;
end;

function tsleep(ident   :Pointer;
                priority:Integer;
                wmesg   :PChar;
                timo    :Int64):Integer;
begin
 Result:=msleep(ident,nil,priority,wmesg,timo);
end;

function pause(wmesg:PChar;timo:Int64):Integer;
begin
 // silently convert invalid timeouts
 if (timo < 1) then timo:=1;

 Result:=(tsleep(@pause_wchan, 0, wmesg, timo));
end;

procedure wakeup(ident:Pointer); public;
begin
 sleepq_lock(ident);
 sleepq_broadcast(ident,SLEEPQ_SLEEP,0,0);
 sleepq_release(ident);
end;

procedure wakeup_one(ident:Pointer);
begin
 sleepq_lock(ident);
 sleepq_signal(ident,SLEEPQ_SLEEP,0,0);
 sleepq_release(ident);
end;

function mi_switch(flags:Integer):Integer; public;
var
 td:p_kthread;
begin
 Result:=0;
 td:=curkthread;

 if (td<>nil) then
 begin
  if (flags and SW_VOL)<>0 then
  begin
   System.InterlockedIncrement64(td^.td_ru.ru_nvcsw);
   System.InterlockedIncrement64(p_proc.p_nvcsw);
  end else
  begin
   System.InterlockedIncrement64(td^.td_ru.ru_nivcsw);
   System.InterlockedIncrement64(p_proc.p_nivcsw);
  end;
 end;

 case (flags and SW_TYPE_MASK) of
  SWT_RELINQUISH,
  SWT_NEEDRESCHED:
   begin
    md_yield;
   end;
  SWT_SLEEPQ,
  SWT_SLEEPQTIMO:
   begin
    if (td=nil) then Exit(-1);
    Result:=sched_switch(td);
   end;
  SWT_SUSPEND:
   begin
    if (td=nil) then Exit(-1);
    td^.td_slptick:=0; //infinite
    Result:=sched_switch(td);
   end;
 end;

end;

procedure maybe_yield();
begin
 kern_yield(PRI_USER);
end;

procedure kern_yield(prio:Integer);
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td<>nil) then
 begin
  thread_lock(td);
  if (prio=PRI_USER) then
   prio:=td^.td_user_pri;
  if (prio>=0) then
   sched_prio(td, prio);
  thread_unlock(td);
 end;
 mi_switch(SW_VOL or SWT_RELINQUISH);
end;

function sys_yield():Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td<>nil) then
 begin
  thread_lock(td);
  if (PRI_BASE(td^.td_pri_class)=PRI_TIMESHARE) then
   sched_prio(td, PRI_MAX_TIMESHARE);
  thread_unlock(td);
  td^.td_retval[0]:=0;
 end;
 mi_switch(SW_VOL or SWT_RELINQUISH);
 Exit(0);
end;

function sys_sched_yield():Integer;
begin
 Result:=sys_yield;
end;

function sys_cpumode_yield():Integer;
begin
 Result:=sys_yield;
end;

end.

