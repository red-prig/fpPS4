unit kern_time;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 time;

Procedure timeinit; //SYSINIT

procedure getmicrouptime(tvp:ptimeval);
procedure getnanotime(tp:Ptimespec);

function  sys_clock_gettime(clock_id:Integer;tp:Ptimespec):Integer;
function  sys_clock_settime(clock_id:Integer;tp:Ptimespec):Integer;
function  sys_clock_getres(clock_id:Integer;tp:Ptimespec):Integer;
function  sys_nanosleep(rqtp,rmtp:ptimespec):Integer;
function  sys_gettimeofday(tp:ptimeval;tzp:ptimezone):Integer;
function  sys_settimeofday(tv:ptimeval;tzp:ptimezone):Integer;

implementation

uses
 errno,
 systm,
 md_time,
 kern_synch;

Procedure timeinit;
begin
 md_timeinit;
 getmicrouptime(@boottime);
end;

procedure getmicrouptime(tvp:ptimeval);
var
 time:Int64;
begin
 time:=get_unit_uptime;
 tvp^.tv_sec :=(time div UNIT_PER_SEC);
 tvp^.tv_usec:=(time mod UNIT_PER_SEC) div 10;
end;

procedure getnanotime(tp:Ptimespec);
var
 time:Int64;
begin
 unittime(@time);
 time:=time-DELTA_EPOCH_IN_UNIT;
 tp^.tv_sec :=(time div UNIT_PER_SEC);
 tp^.tv_nsec:=(time mod UNIT_PER_SEC)*100;
end;

function sys_clock_gettime(clock_id:Integer;tp:Ptimespec):Integer;
var
 ats:timespec;
begin
 Result:=kern_clock_gettime(clock_id,@ats);
 if (Result=0) then
 begin
  Result:=copyout(@ats,tp,sizeof(ats));
 end;
end;

function sys_clock_settime(clock_id:Integer;tp:Ptimespec):Integer;
var
 ats:timespec;
begin
 Result:=copyin(tp,@ats,sizeof(ats));
 if (Result<>0) then Exit;

 //error:=priv_check(td, PRIV_CLOCK_SETTIME)
 Exit(EPERM);
end;

function sys_clock_getres(clock_id:Integer;tp:Ptimespec):Integer;
var
 ats:timespec;
begin
 Result:=kern_clock_getres(clock_id,@ats);
 if (Result=0) then
 begin
  Result:=copyout(@ats,tp,sizeof(ats));
 end;
end;

var
 nanowait:Integer=0;

function kern_nanosleep(rqt,rmt:ptimespec):Integer;
var
 ts,ts2,tv:Int64;
 error:Integer;
begin
 Result:=0;

 if (rqt^.tv_nsec < 0) or (rqt^.tv_nsec >= 1000000000) then
  Exit(EINVAL);

 if (rqt^.tv_sec < 0) or ((rqt^.tv_sec=0) and (rqt^.tv_nsec=0)) then
  Exit(0);

 ts:=get_unit_uptime;
 tv:=TIMESPEC_TO_UNIT(rqt);
 ts:=ts+tv;
 repeat
  error:=tsleep(@nanowait, PWAIT or PCATCH, 'nanslp', tvtohz(tv));
  ts2:=get_unit_uptime;
  if (error<>EWOULDBLOCK) then
  begin
   if (error=ERESTART) then error:=EINTR;
   if (rmt<>nil) then
   begin
    ts:=ts-ts2;
    if (ts<0) then
    begin
     ts:=0;
    end;
    UNIT_TO_TIMESPEC(rmt,ts);
   end;
   Exit(error);
  end;
  if (ts2>=ts) then Exit(0);
  tv:=ts-ts2;
 until false;
end;

function sys_nanosleep(rqtp,rmtp:ptimespec):Integer;
var
 rmt,rqt:timespec;
 error,error2:Integer;
begin
 error:=copyin(rqtp, @rqt, sizeof(timespec));
 if (error<>0) then Exit(error);

 if (rmtp<>nil) then
 begin
  rmt:=Default(timespec);
  error:=copyout(@rmt, rmtp, sizeof(timespec));
  if (error<>0) then Exit(error);
 end;

 error:=kern_nanosleep(@rqt, @rmt);

 if (error<>0) and (rmtp<>nil) then
 begin
  error2:=copyout(@rmt, rmtp, sizeof(timespec));
  if (error2<>0) then
   error:=error2;
 end;
 Exit(error);
end;

function sys_gettimeofday(tp:ptimeval;tzp:ptimezone):Integer;
var
 atv:timeval;
 rtz:timezone;
 error:Integer;
begin
 error:=0;

 if (tp<>nil) then
 begin
  getmicrouptime(@atv);
  error:=copyout(@atv, tp, sizeof (timeval));
 end;
 if (error=0) and (tzp<>nil) then
 begin
  gettimezone(@rtz);
  error:=copyout(@rtz, tzp, sizeof (rtz));
 end;
 Exit(error);
end;

function sys_settimeofday(tv:ptimeval;tzp:ptimezone):Integer;
var
 atv:timeval;
 atz:timezone;
 error:Integer;
begin
 if (tv<>nil) then
 begin
  error:=copyin(tv, @atv, sizeof(timeval));
  if (error<>0) then Exit(error);
 end;

 if (tzp<>nil) then
 begin
  error:=copyin(tzp, @atz, sizeof(timeval));
  if (error<>0) then Exit(error);
 end;

 //error:=priv_check(td, PRIV_SETTIMEOFDAY);
 Exit(EPERM);
end;

end.


