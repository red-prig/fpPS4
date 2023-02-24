unit time;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
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

 Ptimeval=^timeval;
 timeval=packed record
  tv_sec :Int64;
  tv_usec:Int64;   //microsecond
 end;

 Ptimezone=^timezone;
 timezone=packed record
  tz_minuteswest:Integer;
  tz_dsttime    :Integer;
 end;

 time_t=QWORD;
 ptime_t=^time_t;

 ptimesec=^timesec;
 timesec=packed record
  tz_time   :time_t;
  tz_secwest:DWORD;
  tz_dstsec :DWORD;
 end;

function TIMESPEC_TO_UNIT(ts:ptimespec):Int64; inline; //Unit
function tvtohz(time:Int64):Int64; inline;

implementation

function TIMESPEC_TO_UNIT(ts:ptimespec):Int64; inline; //Unit
begin
 Result:=(QWORD(ts^.tv_sec)*10000000)+(QWORD(ts^.tv_nsec) div 100);
end;

function tvtohz(time:Int64):Int64; inline;
begin
 Result:=-time;
end;

end.

