unit sys_time;

{$mode ObjFPC}{$H+}

interface

uses
 windows,
 ntapi,
 sys_types;

function  _usec2msec(usec:QWORD):QWORD;  //Microsecond to Milisecond
function  _msec2usec(msec:QWORD):QWORD;  //Milisecond  to Microsecond
function  _usec2nsec(usec:QWORD):QWORD;  //Microsecond to Nanosecond
function  _nsec2usec(nsec:QWORD):QWORD;  //Nanosecond  to Microsecond
function  _msec2nsec(msec:QWORD):QWORD;  //Milisecond  to Nanosecond
function  _nsec2msec(nsec:QWORD):QWORD;  //Nanosecond  to Milisecond

function  _time_in_ms_from_timespec(const ts:timespec):QWORD; inline; //Milisecond
function  _time_in_ns_from_timespec(const ts:timespec):QWORD; inline; //Nanosecond
function  dwMilliSecs(ms:QWORD):DWORD;

function  filetime_to_hnsec(ft:TFILETIME):QWORD; inline;
function  filetime_to_timespec(ft:TFILETIME):timespec;

procedure SwQueryPerformanceCounter(var pc,pf:QWORD);
procedure SwSaveTime(var pc:QWORD);
function  SwTimePassedUnits(ot:QWORD):QWORD;
function  SwGetTimeUnits:Int64;

function  SwGetProcessTime(var ut:QWORD):Boolean;
function  SwGetThreadTime(var ut:QWORD):Boolean;

procedure SwGetSystemTimeAsFileTime(var lpSystemTimeAsFileTime:TFILETIME);
procedure Swgettimezone(z:Ptimezone);
function  Swgetntptimeofday(tp:Ptimespec;z:Ptimezone):Integer;
function  SwGetTimeUsec:QWORD;

Const
 FILETIME_1970        =116444736000000000;
 HECTONANOSEC_PER_SEC =10000000;
 DELTA_EPOCH_IN_100NS =116444736000000000;
 POW10_7              =10000000;
 POW10_9              =1000000000;
 //POW10_11             =100000000000;

implementation

uses
 sys_signal;

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

function _time_in_ms_from_timespec(const ts:timespec):QWORD; inline; //Milisecond
begin
 Result:=QWORD(ts.tv_sec)*1000+QWORD(ts.tv_nsec+999999) div 1000000;
end;

function _time_in_ns_from_timespec(const ts:timespec):QWORD; inline; //Nanosecond
begin
 Result:=QWORD(ts.tv_sec)*POW10_9+QWORD(ts.tv_nsec);
end;

function dwMilliSecs(ms:QWORD):DWORD;
begin
 if (ms>=$ffffffff) then
  Result:=$ffffffff
 else
  Result:=DWORD(ms);
end;

function filetime_to_hnsec(ft:TFILETIME):QWORD; inline;
begin
 Result:=QWORD(ft)-FILETIME_1970;
end;

function filetime_to_timespec(ft:TFILETIME):timespec;
begin
 QWORD(ft):=filetime_to_hnsec(ft);
 Result.tv_sec :=QWORD(ft) div HECTONANOSEC_PER_SEC;
 Result.tv_nsec:=(QWORD(ft) mod HECTONANOSEC_PER_SEC)*100;
end;

procedure SwQueryPerformanceCounter(var pc,pf:QWORD);
begin
 pc:=0;
 pf:=1;
 _sig_lock;
 NtQueryPerformanceCounter(@pc,@pf);
 _sig_unlock;
end;

procedure SwSaveTime(var pc:QWORD);
var
 pf:QWORD;
begin
 pc:=0;
 pf:=1;
 _sig_lock;
 NtQueryPerformanceCounter(@pc,@pf);
 _sig_unlock;
end;

function SwTimePassedUnits(ot:QWORD):QWORD;
var
 pc:QWORD;
 pf:QWORD;

 //sec:QWORD;
 //uec:QWORD;

 DW0,DW1:QWORD;
begin
 pc:=0;
 pf:=1;
 _sig_lock;
 NtQueryPerformanceCounter(@pc,@pf);
 _sig_unlock;

 if (pc>ot) then
  pc:=pc-ot
 else
  pc:=(ot+High(QWORD))+pc;

 //DW0*POW10_7/pf + SHL_32* DW1*POW10_7/pf

 DW0:=(DWORD(pc shr 00)*POW10_7) div pf;
 DW1:=(DWORD(pc shr 32)*POW10_7) div pf;

 Result:=DW0+(DW1 shl 32);

 //sec:=pc div pf;
 //uec:=((pc mod pf)*POW10_7{POW10_11}+(pf shr 1)) div pf;
 //Result:=sec*POW10_7{POW10_11}+uec;
end;

function SwGetTimeUnits:Int64;
var
 pc:QWORD;
 pf:QWORD;

 //sec:QWORD;
 //uec:QWORD;

 DW0,DW1:QWORD;
begin
 pc:=0;
 pf:=1;
 _sig_lock;
 NtQueryPerformanceCounter(@pc,@pf);
 _sig_unlock;

 //DW0*POW10_7/pf + SHL_32* DW1*POW10_7/pf

 DW0:=(DWORD(pc shr 00)*POW10_7) div pf;
 DW1:=(DWORD(pc shr 32)*POW10_7) div pf;

 Result:=DW0+(DW1 shl 32);

 //sec:=pc div pf;
 //uec:=((pc mod pf)*POW10_7{POW10_11}+(pf shr 1)) div pf;
 //Result:=sec*POW10_7{POW10_11}+uec;
end;

function SwGetProcessTime(var ut:QWORD):Boolean;
var
 ct,et,kt:TFileTime;
begin
 QWORD(ct):=0;
 QWORD(et):=0;
 QWORD(kt):=0;
 ut:=0;
 _sig_lock;
 Result:=GetProcessTimes(GetCurrentProcess,ct,et,kt,TFileTime(ut));
 _sig_unlock;
end;

function SwGetThreadTime(var ut:QWORD):Boolean;
var
 ct,et,kt:TFileTime;
begin
 QWORD(ct):=0;
 QWORD(et):=0;
 QWORD(kt):=0;
 ut:=0;
 _sig_lock;
 Result:=GetThreadTimes(GetCurrentProcess,ct,et,kt,TFileTime(ut));
 _sig_unlock;
end;

type
 TGetSystemTimeAsFileTime=procedure(var lpSystemTimeAsFileTime:TFILETIME); stdcall;

var
 _GetSystemTimeAsFileTime:TGetSystemTimeAsFileTime;

procedure SwGetSystemTimeAsFileTime(var lpSystemTimeAsFileTime:TFILETIME);
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
 _sig_lock;
 _GetSystemTimeAsFileTime(lpSystemTimeAsFileTime);
 _sig_unlock;
end;

procedure Swgettimezone(z:Ptimezone);
var
 TZInfo:TTimeZoneInformation;
 tzi:DWORD;
begin
 if (z<>nil) then
 begin
  _sig_lock;
  tzi:=GetTimeZoneInformation(@TZInfo);
  _sig_unlock;
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

function Swgetntptimeofday(tp:Ptimespec;z:Ptimezone):Integer;
var
 _now:TFILETIME;
begin
 Swgettimezone(z);
 if (tp<>nil) then
 begin
  SwGetSystemTimeAsFileTime(_now);
  QWORD(_now):=filetime_to_hnsec(_now);
  tp^.tv_sec :=QWORD(_now) div HECTONANOSEC_PER_SEC;
  tp^.tv_nsec:=(QWORD(_now) mod HECTONANOSEC_PER_SEC)*100;
 end;
 Result:=0;
end;

function SwGetTimeUsec:QWORD;
var
 pc,pf:QWORD;

 DW0,DW1:QWORD;
begin
 pc:=0;
 pf:=1;
 _sig_lock;
 NtQueryPerformanceCounter(@pc,@pf);
 _sig_unlock;

 //DW0*1000000/pf + SHL_32* DW1*1000000/pf

 DW0:=(DWORD(pc shr 00)*1000000) div pf;
 DW1:=(DWORD(pc shr 32)*1000000) div pf;

 Result:=DW0+(DW1 shl 32);
end;

end.

