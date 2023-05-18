unit kern_synch;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_sleepqueue,
 kern_mtx,
 kern_thr;

const
 PUSER=700;
 PRI_MIN_KERN=64;

 PSWP  =(PRI_MIN_KERN+ 0);
 PVM   =(PRI_MIN_KERN+ 4);
 PINOD =(PRI_MIN_KERN+ 8);
 PRIBIO=(PRI_MIN_KERN+12);
 PVFS  =(PRI_MIN_KERN+16);
 PZERO =(PRI_MIN_KERN+20);
 PSOCK =(PRI_MIN_KERN+24);
 PWAIT =(PRI_MIN_KERN+28);
 PLOCK =(PRI_MIN_KERN+32);
 PPAUSE=(PRI_MIN_KERN+36);

 PRIMASK=$0fff;
 PCATCH =$1000;
 PDROP  =$2000;
 PBDRY  =$4000;

function  msleep(ident   :Pointer;
                 lock    :p_mtx;
                 priority:Integer;
                 wmesg   :PChar;
                 timo    :Int64):Integer;

function  tsleep(ident   :Pointer;
                 priority:Integer;
                 wmesg   :PChar;
                 timo    :Int64):Integer; inline;

procedure wakeup(ident:Pointer);
procedure wakeup_one(ident:Pointer);

procedure maybe_yield();
procedure kern_yield(prio:Integer);
function  sys_yield():Integer;

implementation

uses
 kern_thread,
 sched_ule,
 vm_machdep,
 rtprio;

function msleep(ident   :Pointer;
                lock    :p_mtx;
                priority:Integer;
                wmesg   :PChar;
                timo    :Int64):Integer;
var
 td:p_kthread;
 catch,flags,pri:Integer;
begin
 td:=curkthread;

 catch:=priority and PCATCH;
 pri  :=priority and PRIMASK;

 if (TD_ON_SLEEPQ(td)) then
  sleepq_remove(td,td^.td_wchan);

 flags:=SLEEPQ_SLEEP;

 if (catch<>0) then
  flags:=flags or SLEEPQ_INTERRUPTIBLE;

 if (priority and PBDRY)<>0 then
  flags:=flags or SLEEPQ_STOP_ON_BDRY;

 sleepq_lock(ident);

 mtx_unlock(lock^);

 sleepq_add(ident,lock,wmesg,flags,0);

 if (timo<>0) then
  sleepq_set_timeout(ident,timo);

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

 if (lock<>nil) and ((priority and PDROP)=0) then
 begin
  mtx_lock(lock^);
 end;
end;

function tsleep(ident   :Pointer;
                priority:Integer;
                wmesg   :PChar;
                timo    :Int64):Integer; inline;
begin
 Result:=msleep(ident,nil,priority,wmesg,timo);
end;

procedure wakeup(ident:Pointer);
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
 mi_switch();
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
 mi_switch();
 Exit(0);
end;

end.

