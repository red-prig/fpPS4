unit md_time;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 windows,
 ntapi,
 time;

Procedure md_timeinit;

function  rdtsc:QWORD; assembler;
function  get_rdtsc_freq:QWORD;

function  get_proc_time:Int64;
function  get_proc_time_freq:Int64;

function  get_unit_uptime:Int64;
procedure unittime(time:PInt64);
procedure calcru_proc(user,syst:PInt64);
procedure get_process_cputime(time:PInt64);
procedure calcru_thread(user,syst:PInt64);
procedure get_thread_cputime(time:PInt64);
procedure gettimezone(z:p_timezone);
procedure getadjtime(tv:p_timeval);

function  kern_clock_gettime_unit(clock_id:Integer;time:PInt64):Integer;
function  kern_clock_gettime(clock_id:Integer;tp:p_timespec):Integer;
function  kern_clock_getres(clock_id:Integer;tp:p_timespec):Integer;

implementation

uses
 errno;

Procedure md_timeinit;
var
 min,max,cur:ULONG;
begin
 NtQueryTimerResolution(@min,@max,@cur);
 NtSetTimerResolution(max,True,@cur);
end;

function rdtsc:QWORD; assembler; nostackframe;
asm
 lfence
 rdtsc
 lfence
 shl  $32,%rdx
 or  %rdx,%rax
end;

function _get_rdtsc_freq:QWORD;
var
 shared_page:PQWORD;
 size:DWORD;
 R:DWORD;
begin
 Result:=0;
 shared_page:=nil;
 size:=0;

 R:=NtQuerySystemInformation(SystemHypervisorSharedPageInformation,
                             @shared_page,SizeOf(Pointer),@size);
 if (R<>0) then Exit;
 if (size<>SizeOf(Pointer)) then Exit;

 Result:=(UNIT_PER_SEC shl 32) div (shared_page[1] shr 32);
end;

function tsc_calibrate:QWORD;
const
 samples=80;
var
 i:Integer;

 tsc_freq :QWORD;
 qpc_begin:QWORD;
 tsc_begin:QWORD;
 qpc_end  :QWORD;
 tsc_end  :QWORD;
 qpc_freq :QWORD;
begin
 tsc_freq:=0;

 For i:=0 to samples-1 do
 begin
  qpc_freq :=get_proc_time_freq;
  qpc_begin:=get_proc_time;
  tsc_begin:=rdtsc;

  Sleep(2);

  qpc_end:=get_proc_time;
  tsc_end:=rdtsc;

  if (qpc_end<>qpc_begin) then
  begin
   tsc_freq:=tsc_freq + (tsc_end - tsc_begin) * qpc_freq div (qpc_end - qpc_begin);
  end;
 end;

 tsc_freq:=tsc_freq div samples;

 Result:=tsc_freq;
end;

function get_rdtsc_freq:QWORD;
begin
 Result:=_get_rdtsc_freq;

 if (Result=0) then
 begin
  Result:=tsc_calibrate;
 end;
end;

function get_proc_time:Int64;
var
 pc:QWORD;
 pf:QWORD;
begin
 pc:=0;
 pf:=1;
 NtQueryPerformanceCounter(@pc,@pf);

 Result:=pc;
end;

function get_proc_time_freq:Int64;
var
 pc:QWORD;
 pf:QWORD;
begin
 pc:=0;
 pf:=1;
 NtQueryPerformanceCounter(@pc,@pf);

 Result:=pf;
end;

function mul_div_u64(m,d,v:QWORD):QWORD; sysv_abi_default; assembler; nostackframe;
asm
 movq v,%rax
 mulq m
 divq d
end;

function get_unit_uptime:Int64;
var
 pc:QWORD;
 pf:QWORD;
begin
 pc:=0;
 pf:=1;
 NtQueryPerformanceCounter(@pc,@pf);

 if (pf=UNIT_PER_SEC) then
 begin
  Result:=pc;
 end else
 begin
  Result:=mul_div_u64(UNIT_PER_SEC,pf,pc);
 end;
end;

type
 tunittime=procedure(time:PInt64); stdcall;

var
 _unittime:tunittime;

procedure unittime(time:PInt64);
var
 h:HMODULE;
begin
 if (_unittime=nil) then
 begin
  h:=GetModuleHandle('kernel32.dll');
  Pointer(_unittime):=GetProcAddress(h,'GetSystemTimePreciseAsFileTime');
  if (_unittime=nil) then
  begin
   Pointer(_unittime):=GetProcAddress(h,'GetSystemTimeAsFileTime');
  end;
 end;
 _unittime(time);
end;

procedure calcru_proc(user,syst:PInt64);
var
 k:KERNEL_USER_TIMES;
begin
 k:=Default(KERNEL_USER_TIMES);
 NtQueryInformationProcess(NtCurrentProcess,
                           ProcessTimes,
                           @k,
                           SizeOf(KERNEL_USER_TIMES),
                           nil);
 user^:=k.UserTime.QuadPart;
 syst^:=k.KernelTime.QuadPart;
end;

procedure get_process_cputime(time:PInt64);
var
 k:KERNEL_USER_TIMES;
begin
 k:=Default(KERNEL_USER_TIMES);
 NtQueryInformationProcess(NtCurrentProcess,
                           ProcessTimes,
                           @k,
                           SizeOf(KERNEL_USER_TIMES),
                           nil);

 unittime(@k.ExitTime.QuadPart);
 time^:=k.ExitTime.QuadPart-k.CreateTime.QuadPart;
end;

procedure calcru_thread(user,syst:PInt64);
var
 k:KERNEL_USER_TIMES;
begin
 k:=Default(KERNEL_USER_TIMES);
 NtQueryInformationThread(NtCurrentThread,
                          ThreadTimes,
                          @k,
                          SizeOf(KERNEL_USER_TIMES),
                          nil);
 unittime(@k.ExitTime.QuadPart);
 user^:=k.ExitTime.QuadPart-k.UserTime.QuadPart;
 syst^:=k.ExitTime.QuadPart-k.KernelTime.QuadPart;
end;

procedure get_thread_cputime(time:PInt64);
var
 k:KERNEL_USER_TIMES;
begin
 k:=Default(KERNEL_USER_TIMES);
 NtQueryInformationThread(NtCurrentThread,
                          ThreadTimes,
                          @k,
                          SizeOf(KERNEL_USER_TIMES),
                          nil);
 unittime(@k.ExitTime.QuadPart);
 time^:=k.ExitTime.QuadPart-k.CreateTime.QuadPart;
end;

procedure gettimezone(z:p_timezone);
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
   z^.tz_dsttime    :=ord(tzi=TIME_ZONE_ID_DAYLIGHT);
  end else
  begin
   z^.tz_minuteswest:=0;
   z^.tz_dsttime    :=0;
  end;
 end;
end;

procedure getadjtime(tv:p_timeval);
var
 STA:SYSTEM_QUERY_TIME_ADJUST_INFORMATION;
 R:DWORD;
begin
 tv^:=Default(timeval);
 STA:=Default(SYSTEM_QUERY_TIME_ADJUST_INFORMATION);

 R:=NtQuerySystemInformation(SystemTimeAdjustmentInformation,@STA,SizeOf(STA),nil);
 if (R<>0) then Exit;

 if not Boolean(STA.Enable) then
 begin
  tv^.tv_sec :=(STA.TimeAdjustment div UNIT_PER_SEC);
  tv^.tv_usec:=(STA.TimeAdjustment mod UNIT_PER_SEC) div UNIT_PER_USEC;
 end;
end;

function kern_clock_gettime_unit(clock_id:Integer;time:PInt64):Integer;
var
 user,syst:Int64;
begin
 Result:=0;

 case clock_id of
  CLOCK_REALTIME,
  CLOCK_REALTIME_PRECISE,
  CLOCK_REALTIME_FAST: //getnanotime
   begin
    //nanotime
    unittime(@user);
    user:=user-DELTA_EPOCH_IN_UNIT;
    time^:=user;
   end;

  CLOCK_VIRTUAL:
   begin
    calcru_proc(@user,@syst);
    time^:=user;
   end;

  CLOCK_PROF:
   begin
    calcru_proc(@user,@syst);
    time^:=user+syst;
   end;

  CLOCK_MONOTONIC,
  CLOCK_MONOTONIC_PRECISE,
  CLOCK_MONOTONIC_FAST,
  CLOCK_UPTIME,
  CLOCK_UPTIME_PRECISE,
  CLOCK_UPTIME_FAST,
  CLOCK_EXT_NETWORK,
  CLOCK_EXT_DEBUG_NETWORK,
  CLOCK_EXT_AD_NETWORK,
  CLOCK_EXT_RAW_NETWORK:
   begin
    //nanouptime + time
    time^:=get_unit_uptime;
   end;

  CLOCK_SECOND:
  begin
   unittime(@user);
   user:=user-DELTA_EPOCH_IN_UNIT;
   user:=user-(user mod UNIT_PER_SEC);
   time^:=user;
  end;

  CLOCK_PROCTIME:
   begin
    //nanouptime - resume_time
    get_process_cputime(time);
   end;

  CLOCK_THREAD_CPUTIME_ID:
   begin
    get_thread_cputime(time);
   end

  else
   Result:=EINVAL;
 end;
end;

function kern_clock_gettime(clock_id:Integer;tp:p_timespec):Integer;
var
 time:Int64;
begin
 time:=0;
 Result:=kern_clock_gettime_unit(clock_id,@time);
 if (Result=0) then
 begin
  tp^.tv_sec :=(time div UNIT_PER_SEC);
  tp^.tv_nsec:=(time mod UNIT_PER_SEC)*NSEC_PER_UNIT;
 end;
end;

function kern_clock_getres(clock_id:Integer;tp:p_timespec):Integer;
begin
 Result:=0;

 case clock_id of
  CLOCK_REALTIME,
  CLOCK_VIRTUAL,
  CLOCK_PROF,
  CLOCK_MONOTONIC,
  CLOCK_UPTIME,
  CLOCK_UPTIME_PRECISE,
  CLOCK_UPTIME_FAST,
  CLOCK_REALTIME_PRECISE,
  CLOCK_REALTIME_FAST,
  CLOCK_MONOTONIC_PRECISE,
  CLOCK_MONOTONIC_FAST,
  CLOCK_THREAD_CPUTIME_ID,
  CLOCK_PROCTIME,
  CLOCK_EXT_NETWORK,
  CLOCK_EXT_DEBUG_NETWORK,
  CLOCK_EXT_AD_NETWORK,
  CLOCK_EXT_RAW_NETWORK:
   begin
    tp^.tv_sec :=0;
    tp^.tv_nsec:=80;
   end;

  CLOCK_SECOND:
  begin
   tp^.tv_sec :=1;
   tp^.tv_nsec:=0;
  end;

  else
   Result:=EINVAL;
 end;
end;


end.

