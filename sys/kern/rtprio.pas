unit rtprio;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 errno,
 kern_thr;

const
 PRI_ITHD     =1; // Interrupt thread.
 PRI_REALTIME =2; // Real time process.
 PRI_TIMESHARE=3; // Time sharing process.
 PRI_IDLE     =4; // Idle process.
 PRI_FIFO_BIT =8;
 PRI_FIFO     =10;

 PRI_MIN=0;
 PRI_MAX=960;

 PRI_MIN_TIMESHARE=256;
 PRI_MAX_TIMESHARE=767;

 PUSER=PRI_MIN_TIMESHARE;

 RTP_PRIO_REALTIME =PRI_REALTIME;
 RTP_PRIO_NORMAL   =PRI_TIMESHARE;
 RTP_PRIO_IDLE     =PRI_IDLE;
 RTP_PRIO_FIFO     =PRI_FIFO;

type
 p_rtprio=^t_rtprio;
 t_rtprio=packed record
  _type:Word;
  _prio:Word;
 end;

function  PRI_BASE(P:Integer):Integer;

function  rtp_to_pri(rtp:p_rtprio;td:p_kthread):Integer;
procedure pri_to_rtp(td:p_kthread;rtp:p_rtprio);

implementation

uses
 kern_thread,
 sched_ule;

function PRI_BASE(P:Integer):Integer;
begin
 Result:=P and (not PRI_FIFO_BIT);
end;

function rtp_to_pri(rtp:p_rtprio;td:p_kthread):Integer;
var
 newpri:Integer;
begin

 Case (rtp^._type and $fff7) of //RTP_PRIO_BASE
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

 case (td^.td_pri_class and $fff7) of //PRI_BASE
  PRI_REALTIME,
  RTP_PRIO_NORMAL,
  PRI_IDLE:
    begin
     rtp^._prio:=td^.td_base_user_pri;
    end;
  else;
 end;
 rtp^._type:=td^.td_pri_class;

 thread_unlock(td);
end;

end.

