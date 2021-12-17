unit ps4_time;

{$mode objfpc}{$H+}

interface

uses
  spinlock,
  windows,
  ps4_types,
  Classes, SysUtils;

const
 CLOCK_REALTIME         =0;
 CLOCK_VIRTUAL          =1;
 CLOCK_PROF             =2;
 CLOCK_MONOTONIC        =4;
 CLOCK_UPTIME           =5;	// FreeBSD-specific.
 CLOCK_UPTIME_PRECISE   =7;	// FreeBSD-specific.
 CLOCK_UPTIME_FAST      =8;	// FreeBSD-specific.
 CLOCK_REALTIME_PRECISE =9;	// FreeBSD-specific.
 CLOCK_REALTIME_FAST    =10;	// FreeBSD-specific.
 CLOCK_MONOTONIC_PRECISE=11;	// FreeBSD-specific.
 CLOCK_MONOTONIC_FAST   =12;	// FreeBSD-specific.
 CLOCK_SECOND           =13;	// FreeBSD-specific.
 CLOCK_THREAD_CPUTIME_ID=14;
 CLOCK_PROCTIME         =15;    // ORBIS only
 CLOCK_EXT_NETWORK      =16;    // ORBIS only
 CLOCK_EXT_DEBUG_NETWORK=17;    // ORBIS only
 CLOCK_EXT_AD_NETWORK   =18;    // ORBIS only
 CLOCK_EXT_RAW_NETWORK  =19;    // ORBIS only

function _usec2msec(usec:DWORD):DWORD;
function _pthread_time_in_ms_from_timespec(const ts:timespec):QWORD; inline;
function _pthread_time_in_ms:QWORD; inline;
function _pthread_rel_time_in_ms(const ts:timespec):QWORD;
function dwMilliSecs(ms:QWORD):DWORD; inline;

function ps4_gettimeofday(tv:Ptimeval;tz:Ptimezone):Integer; SysV_ABI_CDecl;
function ps4_clock_gettime(clock_id:Integer;tp:Ptimespec):Integer; SysV_ABI_CDecl;

function ps4_sceKernelGetTscFrequency():QWORD; SysV_ABI_CDecl;
function ps4_sceKernelReadTsc():QWORD; SysV_ABI_CDecl;
function ps4_sceKernelClockGettime(clockId:Integer;tp:Ptimespec):Integer; SysV_ABI_CDecl;
function ps4_sceKernelGetProcessTime:QWORD; SysV_ABI_CDecl; //microseconds

function ps4_nanosleep(req,rem:Ptimespec):Integer; SysV_ABI_CDecl;
function ps4_usleep(usec:Integer):Integer; SysV_ABI_CDecl;
function ps4_sceKernelUsleep(usec:DWORD):Integer; SysV_ABI_CDecl;

Const
 FILETIME_1970        =116444736000000000;
 HECTONANOSEC_PER_SEC =10000000;
 DELTA_EPOCH_IN_100NS =116444736000000000;
 POW10_7              =10000000;
 POW10_9              =1000000000;

implementation

Uses
 ps4_libkernel;

function _usec2msec(usec:DWORD):DWORD;
begin
 Result:=(usec+999) div 1000;
end;

function _pthread_time_in_ms_from_timespec(const ts:timespec):QWORD; inline;
begin
 Result:=QWORD(ts.tv_sec)*1000+QWORD(ts.tv_nsec+999999) div 1000000;
end;

function _pthread_time_in_ms:QWORD; inline;
var
 ts:timespec;
begin
 ts:=Default(timespec);
 ps4_clock_gettime(CLOCK_REALTIME,@ts);
 Result:=_pthread_time_in_ms_from_timespec(ts);
end;

function _pthread_rel_time_in_ms(const ts:timespec):QWORD;
var
 t1,t2:QWORD;
begin
 t1:=_pthread_time_in_ms_from_timespec(ts);
 t2:=_pthread_time_in_ms;
 if (t1<t2) then
  Result:=0
 else
  Result:=t1-t2;
end;

function dwMilliSecs(ms:QWORD):DWORD; inline;
begin
 if (ms>=$ffffffff) then
  Result:=$ffffffff
 else
  Result:=DWORD(ms);
end;

type
 TGetSystemTimeAsFileTime=procedure(var lpSystemTimeAsFileTime:TFILETIME); stdcall;

var
 _GetSystemTimeAsFileTime:TGetSystemTimeAsFileTime;

procedure GetSystemTimeAsFileTime(var lpSystemTimeAsFileTime:TFILETIME);
var
 h:HMODULE;
begin
 if (_GetSystemTimeAsFileTime=nil) then
 begin
  h:=GetModuleHandle('kernel32.dll');
  Pointer(_GetSystemTimeAsFileTime):=GetProcAddress(h,'GetSystemTimePreciseAsFileTime');
  if (_GetSystemTimeAsFileTime=nil) then
  begin
   Pointer(_GetSystemTimeAsFileTime):=GetProcAddress(h,'GetSystemTimeAsFileTime');
  end;
 end;
 _GetSystemTimeAsFileTime(lpSystemTimeAsFileTime);
end;

procedure gettimezone(z:Ptimezone);
var
 TZInfo:TTimeZoneInformation;
 tzi:DWORD;
begin
 if (z<>nil) then
 begin
  tzi:=GetTimeZoneInformation(@TZInfo);
  if (tzi<>TIME_ZONE_ID_INVALID) then
  begin
   z^.tz_minuteswest:=TZInfo.Bias;
   if (tzi=TIME_ZONE_ID_DAYLIGHT) then
    z^.tz_dsttime:=1
   else
    z^.tz_dsttime:=0;
  end else
  begin
   z^.tz_minuteswest:=0;
   z^.tz_dsttime    :=0;
  end;
 end;
end;

function getntptimeofday(tp:Ptimespec;z:Ptimezone):Integer;
var
 _now:TFILETIME;
begin
 gettimezone(z);
 if (tp<>nil) then
 begin
  GetSystemTimeAsFileTime(_now);
  QWORD(_now):=QWORD(_now)-FILETIME_1970;
  tp^.tv_sec :=QWORD(_now) div HECTONANOSEC_PER_SEC;
  tp^.tv_nsec:=(QWORD(_now) mod HECTONANOSEC_PER_SEC)*100;
 end;
 Result:=0;
end;

function ps4_gettimeofday(tv:Ptimeval;tz:Ptimezone):Integer; SysV_ABI_CDecl;
Var
 tp:timespec;
begin
 Result:=getntptimeofday(@tp,tz);
 if (tv<>nil) then
 begin
  tv^.tv_sec :=tp.tv_sec;
  tv^.tv_usec:=(tp.tv_nsec div 1000);
 end;
end;

var
 FPerformanceFrequency:TLargeInteger=1;

function ps4_clock_gettime(clock_id:Integer;tp:Ptimespec):Integer; SysV_ABI_CDecl;
var
 ct,et,kt,ut:TFILETIME;
 pf,pc,tc:TLargeInteger;
begin

 if (tp=nil) then Exit(-1);
 Result:=0;

 case clock_id of
  CLOCK_SECOND:
  begin
   GetSystemTimeAsFileTime(ct);
   QWORD(ct):=QWORD(ct)-DELTA_EPOCH_IN_100NS;
   tp^.tv_sec :=QWORD(ct) div POW10_7;
   //tp^.tv_nsec:=(QWORD(ct) mod POW10_7)*100;
   tp^.tv_nsec:=0;
  end;

  CLOCK_REALTIME,
  CLOCK_REALTIME_PRECISE,
  CLOCK_REALTIME_FAST:
   begin
    GetSystemTimeAsFileTime(ct);
    QWORD(ct):=QWORD(ct)-DELTA_EPOCH_IN_100NS;
    tp^.tv_sec :=QWORD(ct) div POW10_7;
    tp^.tv_nsec:=(QWORD(ct) mod POW10_7)*100;
   end;

  CLOCK_MONOTONIC,
  CLOCK_MONOTONIC_PRECISE,
  CLOCK_MONOTONIC_FAST:
   begin

    System.ThreadSwitch; //this stabilize timers, why? idk
    System.ThreadSwitch;

    pf:=FPerformanceFrequency;
    pc:=0;
    QueryPerformanceCounter(pc);

    tp^.tv_sec :=pc div pf;
    tp^.tv_nsec:=((pc mod pf)*POW10_9+(pf shr 1)) div pf;

    if (tp^.tv_nsec>=POW10_9) then
    begin
     Inc(tp^.tv_sec);
     Dec(tp^.tv_nsec,POW10_9);
    end;

   end;

  CLOCK_PROCTIME:
   begin
    if not GetProcessTimes(GetCurrentProcess,ct,et,kt,ut) then Exit(lc_set_errno(EINVAL));
    QWORD(ct)  :=QWORD(kt)+QWORD(ut);
    tp^.tv_sec :=QWORD(ct) div POW10_7;
    tp^.tv_nsec:=(QWORD(ct) mod POW10_7)*100;
   end;

  CLOCK_THREAD_CPUTIME_ID:
   begin
    if not GetThreadTimes(GetCurrentProcess,ct,et,kt,ut) then Exit(lc_set_errno(EINVAL));
    QWORD(ct)  :=QWORD(kt)+QWORD(ut);
    tp^.tv_sec :=QWORD(ct) div POW10_7;
    tp^.tv_nsec:=(QWORD(ct) mod POW10_7)*100;
   end

  else
   Result:=lc_set_errno(EINVAL);
 end;

end;

function ps4_sceKernelGetTscFrequency():QWORD; SysV_ABI_CDecl;
begin
 Result:=FPerformanceFrequency;
end;

function ps4_sceKernelReadTsc():QWORD; SysV_ABI_CDecl;
begin
 System.ThreadSwitch; //this stabilize timers, why? idk
 System.ThreadSwitch;
 Result:=0;
 QueryPerformanceCounter(TLargeInteger(Result));
end;

function ps4_sceKernelClockGettime(clockId:Integer;tp:Ptimespec):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_clock_gettime(clockId,tp);
end;

function ps4_sceKernelGetProcessTime:QWORD; SysV_ABI_CDecl; //microseconds
var
 ct,et,kt,ut:TFileTime;
begin
 if GetProcessTimes(GetCurrentProcess,ct,et,kt,ut) then
 begin
  Result:=(QWORD(kt)+QWORD(ut)) div 10;
 end else
 begin
  lc_set_errno(EINVAL);
  Result:=0;
 end;
end;

//1sec=10 000 000

//lpUserTime/ 10 000 000 *1 000 000
//lpUserTime/ 10   *1

function ps4_nanosleep(req,rem:Ptimespec):Integer; SysV_ABI_CDecl;
var
 timer:THandle;
 ft:TLargeInteger;
begin
 if (req=nil) then Exit(EINVAL);
 ft:=req^.tv_nsec+req^.tv_sec*1000000000;
 ft:=-(ft div 100);
 timer:=CreateWaitableTimer(nil,True,nil);
 SetWaitableTimer(timer,ft,0,nil,nil,False);
 WaitForSingleObject(timer,INFINITE);
 CloseHandle(timer);
 if (rem<>nil) then
 begin
  rem^:=Default(timespec);
 end;
 Result:=0;
end;

function ps4_usleep(usec:Integer):Integer; SysV_ABI_CDecl;
var
 timer:THandle;
 ft:TLargeInteger;
begin
 ft:=-(10*usec);
 timer:=CreateWaitableTimer(nil,True,nil);
 SetWaitableTimer(timer,ft,0,nil,nil,False);
 WaitForSingleObject(timer,INFINITE);
 CloseHandle(timer);
 Result:=0;
end;

function ps4_sceKernelUsleep(usec:DWORD):Integer; SysV_ABI_CDecl;
var
 timer:THandle;
 ft:TLargeInteger;
begin
 ft:=-(10*usec);
 timer:=CreateWaitableTimer(nil,True,nil);
 SetWaitableTimer(timer,ft,0,nil,nil,False);
 WaitForSingleObject(timer,INFINITE);
 CloseHandle(timer);
 Result:=0;
end;

initialization
 QueryPerformanceFrequency(FPerformanceFrequency);

end.

