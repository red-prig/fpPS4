unit kern_rtprio;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 rtprio,
 kern_thr;

function  rtp_to_pri(rtp:p_rtprio;td:p_kthread):Integer;
procedure pri_to_rtp(td:p_kthread;rtp:p_rtprio);

function  sys_rtprio_thread(func,tid:Integer;rtp:Pointer):Integer;
function  sys_rtprio(func,pid:Integer;rtp:Pointer):Integer;

implementation

uses
 systm,
 errno,
 kern_proc,
 kern_thread,
 sched_ule,
 md_proc;

function rtp_to_pri(rtp:p_rtprio;td:p_kthread):Integer; public;
var
 newpri:Integer;
begin
 Result:=0;

 Case RTP_PRIO_BASE(rtp^._type) of
  RTP_PRIO_IDLE:
    begin
     newpri:=960;
     if (rtp^._prio<>960) then Exit(EINVAL);
    end;
  RTP_PRIO_NORMAL:
    begin
     newpri:=rtp^._prio;
     if (newpri>959) then Exit(EINVAL);
    end;
  PRI_REALTIME:
    begin
     newpri:=rtp^._prio;
     if (newpri>767) then Exit(EINVAL);
    end;
  else
    Exit(EINVAL)
 end;

 thread_lock(td);
 sched_class(td,rtp^._type);
 sched_user_prio(td, newpri);

 if (td=curkthread) then
 begin
  sched_prio(td,td^.td_user_pri);
 end;

 thread_unlock(td);
end;

procedure pri_to_rtp(td:p_kthread;rtp:p_rtprio); public;
begin
 thread_lock(td);

 case PRI_BASE(td^.td_pri_class) of
  RTP_PRIO_REALTIME,
  RTP_PRIO_NORMAL,
  RTP_PRIO_IDLE:
    begin
     rtp^._prio:=td^.td_base_user_pri;
    end;
  else;
 end;
 rtp^._type:=td^.td_pri_class;

 thread_unlock(td);
end;

function sys_rtprio_thread(func,tid:Integer;rtp:Pointer):Integer;
var
 td,td1:p_kthread;
 _rtp:t_rtprio;
begin
 td:=curkthread;

 if (func=RTP_SET) then
 begin
  Result:=copyin(rtp,@_rtp,sizeof(t_rtprio))
 end else
 begin
  Result:=0;
  _rtp:=Default(t_rtprio);
 end;

 if (tid=0) or (tid=td^.td_tid) then
 begin
  td1:=td;
  thread_inc_ref(td1);
 end else
 begin
  td1:=tdfind(tid);
  if (td1=nil) then Exit(ESRCH);
 end;

 case func of
  RTP_LOOKUP:
   begin
    pri_to_rtp(td1,@_rtp);
    Result:=copyout(@_rtp,rtp,sizeof(t_rtprio));
   end;
  RTP_SET:
   begin
    if (Result<>0) then
    begin
     thread_dec_ref(td1);
     Exit;
    end;
    Result:=rtp_to_pri(@_rtp,td1);
   end;
  else
   Result:=EINVAL;
 end;

 thread_dec_ref(td1);
end;

function sys_rtprio(func,pid:Integer;rtp:Pointer):Integer;
var
 td,tdp:p_kthread;
 rtp1,rtp2:t_rtprio;
begin
 if (pid<>0) and (pid<>p_proc.p_pid) then Exit(ESRCH);

 td:=curkthread;

 if (func=RTP_SET) then
 begin
  Result:=copyin(rtp,@rtp1,sizeof(t_rtprio))
 end else
 begin
  Result:=0;
  rtp1:=Default(t_rtprio);
 end;

 case func of
  RTP_LOOKUP:
   begin

    PROC_LOCK;

    if (pid=0) then
    begin
     pri_to_rtp(td,@rtp1);
    end else
    begin
     rtp1._type:=RTP_PRIO_IDLE;
     rtp1._prio:=RTP_PRIO_MAX;

     threads_lock;

       tdp:=TAILQ_FIRST(@p_threads);
       while (tdp<>nil) do
       begin

        pri_to_rtp(tdp,@rtp2);

        if (rtp2._type<rtp1._type) or
           ((rtp2._type=rtp1._type) and
            (rtp2._prio<rtp1._prio)) then
        begin
         rtp1:=rtp2;
        end;

        tdp:=TAILQ_NEXT(tdp,@tdp^.td_plist)
       end;

     threads_unlock;
    end;

    PROC_UNLOCK;

    Result:=copyout(@rtp1,rtp,sizeof(t_rtprio));
   end;
  RTP_SET:
   begin
    if (Result<>0) then Exit;

    PROC_LOCK;

    if (pid=0) then
    begin
     Result:=rtp_to_pri(@rtp1,td);
    end else
    begin
     threads_lock;

       tdp:=TAILQ_FIRST(@p_threads);
       while (tdp<>nil) do
       begin

        Result:=rtp_to_pri(@rtp1,tdp);
        if (Result<>0) then Break;

        tdp:=TAILQ_NEXT(tdp,@tdp^.td_plist)
       end;

     threads_unlock;
    end;

    PROC_UNLOCK;
   end;
  else
   Result:=EINVAL;
 end;

end;


end.


