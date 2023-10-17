unit kern_ksched;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 rtprio,
 time;

const
 //POSIX scheduling policies
 SCHED_FIFO =1;
 SCHED_OTHER=2;
 SCHED_RR   =3;

type
 p_sched_param=^t_sched_param;
 t_sched_param=packed record
  sched_priority:Integer;
 end;

 p_ksched=^t_ksched;
 t_ksched=packed record
  rr_interval:timespec;
 end;

function sys_sched_setparam(pid:Integer;param:Pointer):Integer;
function sys_sched_getparam(pid:Integer;param:Pointer):Integer;
function sys_sched_setscheduler(pid,policy:Integer;param:Pointer):Integer;
function sys_sched_getscheduler(pid:Integer):Integer;
function sys_sched_get_priority_max(policy:Integer):Integer;
function sys_sched_get_priority_min(policy:Integer):Integer;
function sys_sched_rr_get_interval(pid:Integer;interval:Pointer):Integer;

implementation

uses
 errno,
 systm,
 kern_thr,
 kern_proc,
 md_proc;

const
 sched_rr_interval=4;

var
 //Configured in kernel version:
 ksched:t_ksched=(rr_interval:(tv_sec:0;tv_nsec:1000000000 div sched_rr_interval));

function ksched_get_priority_max(ksched:p_ksched;policy:DWORD;prio:PInteger):Integer;
begin
 if ((policy or 2)=3) then //SCHED_FIFO,SCHED_RR
 begin
  prio^:=PRI_MAX_TIMESHARE;
  Exit(0);
 end;
 Exit(EINVAL);
end;

function ksched_get_priority_min(ksched:p_ksched;policy:DWORD;prio:PInteger):Integer;
begin
 if ((policy or 2)=3) then //SCHED_FIFO,SCHED_RR
 begin
  prio^:=PRI_MIN_TIMESHARE;
  Exit(0);
 end;
 Exit(EINVAL);
end;

function ksched_getparam(ksched:p_ksched;td:p_kthread;param:p_sched_param):Integer;
var
 rtp:t_rtprio;
begin
 pri_to_rtp(td,@rtp);
 param^.sched_priority:=rtp._prio;
 Exit(0);
end;

function ksched_getscheduler(ksched:p_ksched;td:p_kthread;policy:PDWORD):Integer;
var
 rtp:t_rtprio;
 p:DWORD;
begin
 pri_to_rtp(td,@rtp);
 p:=SCHED_FIFO;
 if (rtp._type<>PRI_FIFO) then
 begin
  p:=ord(rtp._type=PRI_REALTIME) or 2; //SCHED_OTHER,SCHED_RR
 end;
 policy^:=p;
 Exit(0);
end;

function ksched_rr_get_interval(ksched:p_ksched;td:p_kthread;time:p_timespec):Integer;
begin
 time^:=ksched^.rr_interval;
 Exit(0);
end;

function ksched_setparam(ksched:p_ksched;td:p_kthread;param:p_sched_param):Integer;
var
 rtp:t_rtprio;
 policy:DWORD;
begin
 Result:=EPERM;
 policy:=SCHED_FIFO;
 pri_to_rtp(td,@rtp);
 if (rtp._type<>PRI_FIFO) then
 begin
  if (rtp._type<>PRI_REALTIME) then
  begin
   Exit(EINVAL);
  end;
  policy:=SCHED_RR;
 end;
 if ((param^.sched_priority - PRI_MIN_TIMESHARE) < 512) then
 begin
  rtp._prio:=param^.sched_priority;
  rtp._type:=ord(policy=SCHED_FIFO) * 8 + 2; //PRI_REALTIME,PRI_FIFO
  rtp_to_pri(@rtp,td);
  Result:=0;
 end;
end;

function ksched_setscheduler(ksched:p_ksched;td:p_kthread;policy:DWORD;param:p_sched_param):Integer;
var
 rtp:t_rtprio;
begin
 if ((policy or 2)=3) then //SCHED_FIFO,SCHED_RR
 begin
  Result:=EPERM;
  if ((param^.sched_priority - PRI_MIN_TIMESHARE) < 512) then
  begin
   rtp._prio:=param^.sched_priority;
   rtp._type:=ord(policy=SCHED_FIFO) * 8 + 2; //PRI_REALTIME,PRI_FIFO
   rtp_to_pri(@rtp,td);
   Result:=0;
  end;
 end else
 begin
  Result:=EINVAL;
 end;
end;

/////////

function sys_sched_setparam(pid:Integer;param:Pointer):Integer;
var
 td:p_kthread;
 e:Integer;
 sched_param:t_sched_param;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 e:=copyin(param, @sched_param, sizeof(sched_param));
 if (e<>0) then Exit(e);

 if (pid=0) or (pid=g_pid) then
 begin
  //
 end else
 begin
  Exit(ESRCH);
 end;

 PROC_LOCK();
 e:=ksched_setparam(@ksched,td,@sched_param);
 PROC_UNLOCK();

 Exit(e);
end;

function sys_sched_getparam(pid:Integer;param:Pointer):Integer;
var
 td:p_kthread;
 e:Integer;
 sched_param:t_sched_param;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (pid=0) or (pid=g_pid) then
 begin
  //
 end else
 begin
  Exit(ESRCH);
 end;

 PROC_LOCK();
 e:=ksched_getparam(@ksched, td, @sched_param);
 PROC_UNLOCK();

 if (e=0) then
  e:=copyout(@sched_param, param, sizeof(sched_param));

 Exit(e);
end;

function sys_sched_setscheduler(pid,policy:Integer;param:Pointer):Integer;
var
 td:p_kthread;
 e:Integer;
 sched_param:t_sched_param;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 { Don't allow non root user to set a scheduler policy. }
 //e:=priv_check(td, PRIV_SCHED_SET);
 //if (e<>0) then Exit(e);

 e:=copyin(param, @sched_param, sizeof(sched_param));
 if (e<>0) then Exit(e);

 if (pid=0) or (pid=g_pid) then
 begin
  //
 end else
 begin
  Exit(ESRCH);
 end;

 PROC_LOCK();
 e:=ksched_setscheduler(@ksched, td, policy, @sched_param);
 PROC_UNLOCK();

 Exit(e);
end;

function sys_sched_getscheduler(pid:Integer):Integer;
var
 td:p_kthread;
 e,policy:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (pid=0) or (pid=g_pid) then
 begin
  //
 end else
 begin
  Exit(ESRCH);
 end;

 PROC_LOCK();
 e:=ksched_getscheduler(@ksched, td, @policy);
 td^.td_retval[0]:=policy;
 PROC_UNLOCK();

 Exit(e);
end;

function sys_sched_get_priority_max(policy:Integer):Integer;
var
 td:p_kthread;
 error,prio:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 error:=ksched_get_priority_max(@ksched, policy, @prio);

 td^.td_retval[0]:=prio;
 Exit(error);
end;

function sys_sched_get_priority_min(policy:Integer):Integer;
var
 td:p_kthread;
 error,prio:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 error:=ksched_get_priority_min(@ksched, policy, @prio);

 td^.td_retval[0]:=prio;
 Exit(error);
end;

function kern_sched_rr_get_interval(td:p_kthread;pid:Integer;ts:p_timespec):Integer;
var
 e:Integer;
begin

 if (pid=0) or (pid=g_pid) then
 begin
  //
 end else
 begin
  Exit(ESRCH);
 end;

 PROC_LOCK();
 e:=ksched_rr_get_interval(@ksched, td, ts);
 PROC_UNLOCK();

 Exit(e);
end;

function sys_sched_rr_get_interval(pid:Integer;interval:Pointer):Integer;
var
 td:p_kthread;
 time:timespec;
 error:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 error:=kern_sched_rr_get_interval(td, pid, @time);

 if (error=0) then
  error:=copyout(@time, interval, sizeof(time));

 Exit(error);
end;



end.

