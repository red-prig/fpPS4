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

procedure wakeup(ident:Pointer);
procedure wakeup_one(ident:Pointer);

implementation

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



end.

