unit kern_time;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 windows,
 ntapi,
 time;

function  cputick2usec(time:QWORD):QWORD; inline;
function  get_unit_uptime:Int64;
procedure getnanotime(tp:Ptimespec);

function  kern_clock_gettime_unit(clock_id:Integer;time:PInt64):Integer;
function  kern_clock_gettime(clock_id:Integer;tp:Ptimespec):Integer;
function  kern_clock_getres(clock_id:Integer;tp:Ptimespec):Integer;

function  sys_clock_gettime(clock_id:Integer;tp:Ptimespec):Integer;
function  sys_clock_getres(clock_id:Integer;tp:Ptimespec):Integer;

implementation

uses
 errno,
 systm;

Const
 UNIT_PER_SEC         =10000000;
 DELTA_EPOCH_IN_UNIT  =116444736000000000;
 POW10_7              =10000000;
 POW10_9              =1000000000;

function cputick2usec(time:QWORD):QWORD; inline;
begin
 Result:=time div 10;
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

procedure calcru(user,syst:PInt64);
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

procedure getnanotime(tp:Ptimespec);
var
 time:Int64;
begin
 unittime(@time);
 time:=time-DELTA_EPOCH_IN_UNIT;
 tp^.tv_sec :=(time div POW10_7);
 tp^.tv_nsec:=(time mod POW10_7)*100;
end;

function kern_clock_gettime_unit(clock_id:Integer;time:PInt64):Integer;
var
 user,syst:Int64;
begin
 Result:=0;

 case clock_id of
  CLOCK_REALTIME,
  CLOCK_REALTIME_PRECISE,
  CLOCK_REALTIME_FAST:
   begin
    unittime(@user);
    user:=user-DELTA_EPOCH_IN_UNIT;
    time^:=user;
   end;

  CLOCK_VIRTUAL:
   begin
    calcru(@user,@syst);
    time^:=user;
   end;

  CLOCK_PROF:
   begin
    calcru(@user,@syst);
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
    time^:=get_unit_uptime;
   end;

  CLOCK_SECOND:
  begin
   unittime(@user);
   user:=user-DELTA_EPOCH_IN_UNIT;
   user:=user-(user mod POW10_7);
   time^:=user;
  end;

  CLOCK_PROCTIME:
   begin
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

function kern_clock_gettime(clock_id:Integer;tp:Ptimespec):Integer;
var
 time:Int64;
begin
 time:=0;
 Result:=kern_clock_gettime_unit(clock_id,@time);
 if (Result=0) then
 begin
  tp^.tv_sec :=(time div POW10_7);
  tp^.tv_nsec:=(time mod POW10_7)*100;
 end;
end;

function kern_clock_getres(clock_id:Integer;tp:Ptimespec):Integer;
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
    tp^.tv_nsec:=100;
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

Procedure Init;
var
 min,max,cur:ULONG;
begin
 NtQueryTimerResolution(@min,@max,@cur);
 NtSetTimerResolution(max,True,@cur);
end;

initialization
 Init;

end.


