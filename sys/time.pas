unit time;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 {
  * Names of the interval timers, and structure
  * defining a timer setting.
  }
 ITIMER_REAL   =0;
 ITIMER_VIRTUAL=1;
 ITIMER_PROF   =2;

 CLOCK_REALTIME         =0;
 CLOCK_VIRTUAL          =1;
 CLOCK_PROF             =2;
 CLOCK_MONOTONIC        =4;
 CLOCK_UPTIME           =5;  // FreeBSD-specific.
 CLOCK_UPTIME_PRECISE   =7;  // FreeBSD-specific.
 CLOCK_UPTIME_FAST      =8;  // FreeBSD-specific.
 CLOCK_REALTIME_PRECISE =9;  // FreeBSD-specific.
 CLOCK_REALTIME_FAST    =10; // FreeBSD-specific.
 CLOCK_MONOTONIC_PRECISE=11; // FreeBSD-specific.
 CLOCK_MONOTONIC_FAST   =12; // FreeBSD-specific.
 CLOCK_SECOND           =13; // FreeBSD-specific.
 CLOCK_THREAD_CPUTIME_ID=14;
 CLOCK_PROCTIME         =15; // ORBIS only
 CLOCK_EXT_NETWORK      =16; // ORBIS only
 CLOCK_EXT_DEBUG_NETWORK=17; // ORBIS only
 CLOCK_EXT_AD_NETWORK   =18; // ORBIS only
 CLOCK_EXT_RAW_NETWORK  =19; // ORBIS only

type
 Ptimespec=^timespec;
 timespec=packed record
  tv_sec :Int64;       /// seconds
  tv_nsec:Int64;       /// nanoseconds
 end;
 {$IF sizeof(timespec)<>16}{$STOP sizeof(timespec)<>16}{$ENDIF}

 Ptimeval=^timeval;
 timeval=packed record
  tv_sec :Int64;
  tv_usec:Int64;   //microsecond
 end;
 {$IF sizeof(timeval)<>16}{$STOP sizeof(timeval)<>16}{$ENDIF}

 Pitimerval=^itimerval;
 itimerval=packed record
  it_interval:timeval; { timer interval }
  it_value   :timeval; { current value }
 end;
 {$IF sizeof(itimerval)<>32}{$STOP sizeof(itimerval)<>32}{$ENDIF}

 Ptimezone=^timezone;
 timezone=packed record
  tz_minuteswest:Integer;
  tz_dsttime    :Integer;
 end;
 {$IF sizeof(timezone)<>8}{$STOP sizeof(timezone)<>8}{$ENDIF}

 ptime_t=^time_t;
 time_t=QWORD;
 {$IF sizeof(time_t)<>8}{$STOP sizeof(time_t)<>8}{$ENDIF}

 ptimesec=^timesec;
 timesec=packed record
  tz_time   :time_t;
  tz_secwest:DWORD;
  tz_dstsec :DWORD;
 end;
 {$IF sizeof(timesec)<>16}{$STOP sizeof(timesec)<>16}{$ENDIF}

const
 tick=100;

 UNIT_PER_SEC         =10000000;
 NSEC_PER_UNIT        =100;
 UNIT_PER_USEC        =10;

 DELTA_EPOCH_IN_UNIT  =116444736000000000;
 POW10_9              =1000000000;

 hz=UNIT_PER_SEC;

function _usec2msec(usec:QWORD):QWORD;  //Microsecond to Milisecond
function _msec2usec(msec:QWORD):QWORD;  //Milisecond  to Microsecond
function _usec2nsec(usec:QWORD):QWORD;  //Microsecond to Nanosecond
function _nsec2usec(nsec:QWORD):QWORD;  //Nanosecond  to Microsecond
function _msec2nsec(msec:QWORD):QWORD;  //Milisecond  to Nanosecond
function _nsec2msec(nsec:QWORD):QWORD;  //Nanosecond  to Milisecond

procedure timevalfix(t1:ptimeval);
procedure timevaladd(t1,t2:ptimeval);
procedure timevalsub(t1,t2:ptimeval);

function  timespeccmp_lt(tvp,uvp:ptimespec):Integer;

procedure TIMEVAL_TO_TIMESPEC(tv:ptimeval;ts:ptimespec);
procedure TIMESPEC_TO_TIMEVAL(tv:ptimeval;ts:ptimespec);

function  TIMESPEC_TO_UNIT(ts:ptimespec):Int64;   //Unit
procedure UNIT_TO_TIMESPEC(ts:ptimespec;u:Int64); //Unit
function  TIMEVAL_TO_UNIT (tv:ptimeval ):Int64;   //Unit
procedure UNIT_TO_TIMEVAL (tv:ptimeval;u:Int64);  //Unit
function  USEC_TO_UNIT    (usec:QWORD  ):Int64;   //Unit

function  cputick2usec(time:QWORD):QWORD; inline;
function  tvtohz(time:Int64):Int64;
procedure usec2timespec(ts:ptimespec;timeo:DWORD);

procedure TIMESPEC_ADD(dst,src,val:ptimespec);
procedure TIMESPEC_SUB(dst,src,val:ptimespec);

function  itimerfix(tv:ptimeval):Integer;

function  clock_gettime(clock_id:Integer;tp:Ptimespec):Integer;
function  clock_getres(clock_id:Integer;tp:Ptimespec):Integer;

var
 boottime:timeval;

implementation

uses
 errno,
 trap,
 thr_error,
 kern_time;

function _usec2msec(usec:QWORD):QWORD;  //Microsecond to Milisecond
begin
 Result:=(usec+999) div 1000;
end;

function _msec2usec(msec:QWORD):QWORD;  //Milisecond to Microsecond
begin
 Result:=msec*1000;
end;

function _usec2nsec(usec:QWORD):QWORD;  //Microsecond to Nanosecond
begin
 Result:=usec*1000;
end;

function _nsec2usec(nsec:QWORD):QWORD;  //Nanosecond to Microsecond
begin
 Result:=(nsec+999) div 1000;
end;

function _msec2nsec(msec:QWORD):QWORD;  //Milisecond to Nanosecond
begin
 Result:=msec*1000000;
end;

function _nsec2msec(nsec:QWORD):QWORD;  //Nanosecond to Milisecond
begin
 Result:=(nsec+999999) div 1000000;
end;

procedure timevalfix(t1:ptimeval);
begin
 if (t1^.tv_usec < 0) then
 begin
  Dec(t1^.tv_sec);
  Inc(t1^.tv_usec,1000000);
 end;
 if (t1^.tv_usec >= 1000000) then
 begin
  Inc(t1^.tv_sec);
  Dec(t1^.tv_usec,1000000);
 end;
end;

procedure timevaladd(t1,t2:ptimeval);
begin
 Inc(t1^.tv_sec ,t2^.tv_sec);
 Inc(t1^.tv_usec,t2^.tv_usec);
 timevalfix(t1);
end;

procedure timevalsub(t1,t2:ptimeval);
begin
 Dec(t1^.tv_sec ,t2^.tv_sec);
 Dec(t1^.tv_usec,t2^.tv_usec);
 timevalfix(t1);
end;

function timespeccmp_lt(tvp,uvp:ptimespec):Integer;
begin
 if (tvp^.tv_sec=uvp^.tv_sec) then
 begin
  Result:=ord(tvp^.tv_nsec < uvp^.tv_nsec);
 end else
 begin
  Result:=ord(tvp^.tv_sec < uvp^.tv_sec);
 end;
end;

procedure TIMEVAL_TO_TIMESPEC(tv:ptimeval;ts:ptimespec);
begin
 ts^.tv_sec :=tv^.tv_sec;
 ts^.tv_nsec:=tv^.tv_usec * 1000;
end;

procedure TIMESPEC_TO_TIMEVAL(tv:ptimeval;ts:ptimespec);
begin
 tv^.tv_sec :=ts^.tv_sec;
 tv^.tv_usec:=ts^.tv_nsec div 1000;
end;

function TIMESPEC_TO_UNIT(ts:ptimespec):Int64; //Unit
begin
 Result:=(QWORD(ts^.tv_sec)*UNIT_PER_SEC)+(QWORD(ts^.tv_nsec) div NSEC_PER_UNIT);
end;

procedure UNIT_TO_TIMESPEC(ts:ptimespec;u:Int64); //Unit
begin
 ts^.tv_sec :=(u div UNIT_PER_SEC);
 ts^.tv_nsec:=(u mod UNIT_PER_SEC)*NSEC_PER_UNIT;
end;

function TIMEVAL_TO_UNIT(tv:ptimeval):Int64; //Unit
begin
 Result:=(QWORD(tv^.tv_sec)*UNIT_PER_SEC)+(QWORD(tv^.tv_usec)*UNIT_PER_USEC);
end;

procedure UNIT_TO_TIMEVAL(tv:ptimeval;u:Int64); //Unit
begin
 tv^.tv_sec :=(u div UNIT_PER_SEC);
 tv^.tv_usec:=(u mod UNIT_PER_SEC) div UNIT_PER_USEC;
end;

function USEC_TO_UNIT(usec:QWORD):Int64; //Unit
begin
 Result:=(usec*UNIT_PER_USEC);
end;

function cputick2usec(time:QWORD):QWORD; inline;
begin
 Result:=time div UNIT_PER_USEC;
end;

function tvtohz(time:Int64):Int64;
begin
 Result:=time;
end;

procedure usec2timespec(ts:ptimespec;timeo:DWORD);
begin
 ts^.tv_sec :=(timeo div 1000000);
 ts^.tv_nsec:=(timeo mod 1000000)*1000;
end;

procedure TIMESPEC_ADD(dst,src,val:ptimespec);
begin
 dst^.tv_sec :=src^.tv_sec +val^.tv_sec;
 dst^.tv_nsec:=src^.tv_nsec+val^.tv_nsec;
 if (dst^.tv_nsec>=1000000000) then
 begin
  Inc(dst^.tv_sec);
  Dec(dst^.tv_nsec,1000000000);
 end;
end;

procedure TIMESPEC_SUB(dst,src,val:ptimespec);
begin
 dst^.tv_sec :=src^.tv_sec -val^.tv_sec;
 dst^.tv_nsec:=src^.tv_nsec-val^.tv_nsec;
 if (dst^.tv_nsec<0) then
 begin
  Dec(dst^.tv_sec);
  Inc(dst^.tv_nsec,1000000000);
 end;
end;

{
 * Check that a proposed value to load into the .it_value or
 * .it_interval part of an interval timer is acceptable, and
 * fix it to have at least minimal value (i.e. if it is less
 * than the resolution of the clock, round it up.)
 }
function itimerfix(tv:ptimeval):Integer;
begin
 if (tv^.tv_sec < 0) or (tv^.tv_usec < 0) or (tv^.tv_usec >= 1000000) then
  Exit(EINVAL);
 if (tv^.tv_sec=0) and (tv^.tv_usec<>0) and (tv^.tv_usec < tick) then
  tv^.tv_usec:=tick;
 Exit(0);
end;

function clock_gettime(clock_id:Integer;tp:Ptimespec):Integer; assembler; nostackframe;
asm
 movq  sys_clock_gettime,%rax
 call  fast_syscall
 jmp   cerror
end;

function clock_getres(clock_id:Integer;tp:Ptimespec):Integer; assembler; nostackframe;
asm
 movq  sys_clock_getres,%rax
 call  fast_syscall
 jmp   cerror
end;

end.

