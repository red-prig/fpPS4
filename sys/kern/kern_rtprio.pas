unit kern_rtprio;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 rtprio,
 kern_thr;

function  rtp_to_pri(rtp:p_rtprio;td:p_kthread):Integer;
procedure pri_to_rtp(td:p_kthread;rtp:p_rtprio);

function  sys_rtprio_thread(func,tid:Integer;rtp:p_rtprio):Integer;
function  sys_rtprio(func,pid:Integer;rtp:p_rtprio):Integer;

implementation

uses
 systm,
 errno,
 kern_thread,
 sched_ule,
 md_proc;

function rtp_to_pri(rtp:p_rtprio;td:p_kthread):Integer;
var
 newpri:Integer;
begin

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

procedure pri_to_rtp(td:p_kthread;rtp:p_rtprio);
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

function sys_rtprio_thread(func,tid:Integer;rtp:p_rtprio):Integer;
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

procedure _for_lookup_rtprio(td:p_kthread;rtp:p_rtprio); register; //Tfree_data_cb
var
 _rtp2:t_rtprio;
begin
 if (td=nil) or (rtp=nil) then Exit;

 pri_to_rtp(td,@_rtp2);
 if (_rtp2._type<rtp^._type) or
    ((_rtp2._type=rtp^._type) and
     (_rtp2._prio<rtp^._prio)) then
 begin
  rtp^:=_rtp2;
 end;
end;

procedure _for_set_rtprio(td:p_kthread;rtp:p_rtprio); register; //Tfree_data_cb
var
 err:Integer;
begin
 if (td=nil) or (rtp=nil) then Exit;

 if (rtp^._type<>$FFFF) then
 begin
  err:=rtp_to_pri(rtp,td);
  if (err<>0) then
  begin
   rtp^._type:=$FFFF;
   rtp^._prio:=err;
  end;
 end;
end;

function sys_rtprio(func,pid:Integer;rtp:p_rtprio):Integer;
var
 td:p_kthread;
 _rtp:t_rtprio;
begin
 if (pid<>0) and (pid<>g_pid) then Exit(ESRCH);

 td:=curkthread;

 if (func=RTP_SET) then
 begin
  Result:=copyin(rtp,@_rtp,sizeof(t_rtprio))
 end else
 begin
  Result:=0;
  _rtp:=Default(t_rtprio);
 end;

 case func of
  RTP_LOOKUP:
   begin
    if (pid=0) then
    begin
     pri_to_rtp(td,@_rtp);
    end else
    begin
     _rtp._type:=RTP_PRIO_IDLE;
     _rtp._prio:=RTP_PRIO_MAX;
     FOREACH_THREAD_IN_PROC(@_for_lookup_rtprio,@_rtp);
    end;
    Result:=copyout(@_rtp,rtp,sizeof(t_rtprio));
   end;
  RTP_SET:
   begin
    if (Result<>0) then Exit;
    if (pid=0) then
    begin
     Result:=rtp_to_pri(@_rtp,td);
    end else
    begin
     FOREACH_THREAD_IN_PROC(@_for_set_rtprio,@_rtp);
     if (_rtp._type=$FFFF) then
     begin
      Result:=_rtp._prio;
     end;
    end;
   end;
  else
   Result:=EINVAL;
 end;

end;


end.


