unit ps4_time;

{$mode objfpc}{$H+}

interface

uses
  sys_types;

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

function _pthread_time_in_ms:QWORD; //Milisecond
function _pthread_rel_time_in_ms(const ts:timespec):QWORD; //Milisecond
function _pthread_time_in_ns:QWORD; //Nanosecond
function _pthread_rel_time_in_ns(const ts:timespec):QWORD; //Nanosecond

function ps4_gettimeofday(tv:Ptimeval;tz:Ptimezone):Integer; SysV_ABI_CDecl;
function ps4_clock_getres(clock_id:Integer;tp:Ptimespec):Integer; SysV_ABI_CDecl;
function ps4_clock_gettime(clock_id:Integer;tp:Ptimespec):Integer; SysV_ABI_CDecl;

function ps4_sceKernelGettimeofday(tv:Ptimeval):Integer; SysV_ABI_CDecl;
function ps4_sceKernelGettimezone(tz:Ptimezone):Integer; SysV_ABI_CDecl;

function ps4_clock_settime(clock_id:Integer;tp:Ptimespec):Integer; SysV_ABI_CDecl;
function ps4_settimeofday(tv:Ptimeval;tz:Ptimezone):Integer; SysV_ABI_CDecl;
function ps4_sceKernelSettimeofday(tv:Ptimeval;tz:Ptimezone):Integer; SysV_ABI_CDecl;

function ps4_sceKernelGetTscFrequency():QWORD; SysV_ABI_CDecl;
function ps4_sceKernelReadTsc():QWORD; SysV_ABI_CDecl;
function ps4_sceKernelClockGettime(clockId:Integer;tp:Ptimespec):Integer; SysV_ABI_CDecl;
function ps4_sceKernelGetProcessTime:QWORD; SysV_ABI_CDecl; //microseconds
function ps4_sceKernelGetProcessTimeCounterFrequency:QWORD; SysV_ABI_CDecl; //microseconds*10
function ps4_sceKernelGetProcessTimeCounter:QWORD; SysV_ABI_CDecl; //microseconds*10

function ps4_nanosleep(req,rem:Ptimespec):Integer; SysV_ABI_CDecl;          //nanoseconds
function ps4_sceKernelNanosleep(req,rem:Ptimespec):Integer; SysV_ABI_CDecl; //nanoseconds

function ps4_usleep(usec:DWORD):Integer; SysV_ABI_CDecl;          //microseconds
function ps4_sceKernelUsleep(usec:DWORD):Integer; SysV_ABI_CDecl; //microseconds

function ps4_sleep(sec:DWORD):DWORD; SysV_ABI_CDecl;          //second
function ps4_sceKernelSleep(sec:DWORD):DWORD; SysV_ABI_CDecl; //second

type
 ptimesec=^timesec;
 timesec=packed record
  tz_time   :time_t;
  tz_secwest:DWORD;
  tz_dstsec :DWORD;
 end;

function ps4_sceKernelConvertUtcToLocaltime(_time:time_t;
                                            local_time:ptime_t;
                                            _tsec:ptimesec;
                                            _dstsec:PDWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelConvertLocaltimeToUtc(_time:time_t;
                                            unknow:qword;
                                            utc_time:ptime_t;
                                            _tsec:ptimesec;
                                            _dstsec:PDWORD):Integer; SysV_ABI_CDecl;

implementation

Uses
 Windows,
 ntapi,
 sys_kernel,
 sys_signal,
 sys_time;

function _pthread_time_in_ms:QWORD; //Milisecond
var
 ts:timespec;
begin
 ts:=Default(timespec);
 ps4_clock_gettime(CLOCK_REALTIME,@ts);
 Result:=_time_in_ms_from_timespec(ts);
end;

function _pthread_rel_time_in_ms(const ts:timespec):QWORD; //Milisecond
var
 t1,t2:QWORD;
begin
 t1:=_time_in_ms_from_timespec(ts);
 t2:=_pthread_time_in_ms;
 if (t1<t2) then
  Result:=0
 else
  Result:=t1-t2;
end;

function _pthread_time_in_ns:QWORD; //Nanosecond
var
 ts:timespec;
begin
 ts:=Default(timespec);
 ps4_clock_gettime(CLOCK_REALTIME,@ts);
 Result:=_time_in_ms_from_timespec(ts);
end;

function _pthread_rel_time_in_ns(const ts:timespec):QWORD; //Nanosecond
var
 t1,t2:QWORD;
begin
 t1:=_time_in_ns_from_timespec(ts);
 t2:=_pthread_time_in_ns;
 if (t1<t2) then
  Result:=0
 else
  Result:=t1-t2;
end;

function ps4_gettimeofday(tv:Ptimeval;tz:Ptimezone):Integer; SysV_ABI_CDecl;
Var
 tp:timespec;
begin
 Result:=_set_errno(Swgetntptimeofday(@tp,tz));
 if (Result=0) and (tv<>nil) then
 begin
  tv^.tv_sec :=tp.tv_sec;
  tv^.tv_usec:=(tp.tv_nsec div 1000);
 end;
end;

function ps4_sceKernelGettimeofday(tv:Ptimeval):Integer; SysV_ABI_CDecl;
Var
 tp:timespec;
begin
 Result:=Swgetntptimeofday(@tp,nil);
 _set_errno(Result);
 Result:=px2sce(Result);
 if (Result=0) and (tv<>nil) then
 begin
  tv^.tv_sec :=tp.tv_sec;
  tv^.tv_usec:=(tp.tv_nsec div 1000);
 end;
end;

function ps4_sceKernelGettimezone(tz:Ptimezone):Integer; SysV_ABI_CDecl;
begin
 Swgettimezone(tz);
 Result:=_set_errno(0);
end;

function ps4_clock_settime(clock_id:Integer;tp:Ptimespec):Integer; SysV_ABI_CDecl;
begin
 Result:=_set_errno(EPERM);
end;

function ps4_settimeofday(tv:Ptimeval;tz:Ptimezone):Integer; SysV_ABI_CDecl;
begin
 Result:=_set_errno(EPERM);
end;

function ps4_sceKernelSettimeofday(tv:Ptimeval;tz:Ptimezone):Integer; SysV_ABI_CDecl;
begin
 Result:=_set_errno(px2sce(EPERM));
end;

function ps4_clock_getres(clock_id:Integer;tp:Ptimespec):Integer; SysV_ABI_CDecl;
begin
 if (tp=nil) then Exit(_set_errno(EINVAL));

 Result:=_set_errno(0);

 case clock_id of
  CLOCK_SECOND:
  begin
   tp^.tv_sec :=1;
   tp^.tv_nsec:=0;
  end;

  CLOCK_PROCTIME,
  CLOCK_THREAD_CPUTIME_ID,

  CLOCK_REALTIME,
  CLOCK_REALTIME_PRECISE,
  CLOCK_REALTIME_FAST:
   begin
    tp^.tv_sec :=0;
    tp^.tv_nsec:=100;
   end;

  CLOCK_MONOTONIC,
  CLOCK_MONOTONIC_PRECISE,
  CLOCK_MONOTONIC_FAST:
   begin
    tp^.tv_sec :=0;
    tp^.tv_nsec:=100;
   end;

  else
   Result:=_set_errno(EINVAL);
 end;

end;

function mul_div_u64(m,d,v:QWORD):QWORD; sysv_abi_default; assembler; nostackframe;
asm
 movq v,%rax
 mulq m
 divq d
end;

function ps4_clock_gettime(clock_id:Integer;tp:Ptimespec):Integer; SysV_ABI_CDecl;
var
 pc,pf:QWORD;
begin
 if (tp=nil) then Exit(_set_errno(EINVAL));

 Result:=_set_errno(0);

 case clock_id of
  CLOCK_SECOND:
  begin
   SwGetSystemTimeAsFileTime(TFILETIME(pc));
   pc:=pc-DELTA_EPOCH_IN_100NS;
   tp^.tv_sec :=pc div POW10_7;
   tp^.tv_nsec:=0;
  end;

  CLOCK_REALTIME,
  CLOCK_REALTIME_PRECISE,
  CLOCK_REALTIME_FAST:
   begin
    SwGetSystemTimeAsFileTime(TFILETIME(pc));
    pc:=pc-DELTA_EPOCH_IN_100NS;
    tp^.tv_sec :=(pc div POW10_7);
    tp^.tv_nsec:=(pc mod POW10_7)*100;
   end;

  CLOCK_EXT_NETWORK      ,
  CLOCK_EXT_DEBUG_NETWORK,
  CLOCK_EXT_AD_NETWORK   ,
  CLOCK_EXT_RAW_NETWORK  ,

  CLOCK_MONOTONIC,
  CLOCK_MONOTONIC_PRECISE,
  CLOCK_MONOTONIC_FAST:
   begin
    SwQueryPerformanceCounter(pc,pf);

    pc:=mul_div_u64(POW10_7,pf,pc);

    tp^.tv_sec :=(pc div POW10_7);
    tp^.tv_nsec:=(pc mod POW10_7)*100;
   end;

  CLOCK_PROCTIME:
   begin
    if SwGetProcessTime(pc) then
    begin
     tp^.tv_sec :=(pc div POW10_7);
     tp^.tv_nsec:=(pc mod POW10_7)*100;
    end else
    begin
     Result:=_set_errno(EINVAL);
    end;
   end;

  CLOCK_THREAD_CPUTIME_ID:
   begin
    if SwGetThreadTime(pc) then
    begin
     tp^.tv_sec :=(pc div POW10_7);
     tp^.tv_nsec:=(pc mod POW10_7)*100;
    end else
    begin
     Result:=_set_errno(EINVAL);
    end;
   end

  else
   Result:=_set_errno(EINVAL);
 end;

end;

function ps4_sceKernelGetTscFrequency():QWORD; SysV_ABI_CDecl;
begin
 Result:=POW10_7;
end;

function ps4_sceKernelReadTsc():QWORD; SysV_ABI_CDecl;
var
 pc,pf:QWORD;
begin
 SwQueryPerformanceCounter(pc,pf);

 Result:=mul_div_u64(POW10_7,pf,pc);
end;

function ps4_sceKernelClockGettime(clockId:Integer;tp:Ptimespec):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_clock_gettime(clockId,tp);
end;

function ps4_sceKernelGetProcessTime:QWORD; SysV_ABI_CDecl; //microseconds
begin
 if SwGetProcessTime(Result) then
 begin
  Result:=Result div 10;
 end else
 begin
  _set_errno(EINVAL);
  Result:=0;
 end;
end;

function ps4_sceKernelGetProcessTimeCounterFrequency:QWORD; SysV_ABI_CDecl; //microseconds*10
begin
 Result:=HECTONANOSEC_PER_SEC;
end;

function ps4_sceKernelGetProcessTimeCounter:QWORD; SysV_ABI_CDecl; //microseconds*10
begin
 if SwGetProcessTime(Result) then
 begin
  //
 end else
 begin
  _set_errno(EINVAL);
  Result:=0;
 end;
end;

//1sec=10 000 000

//lpUserTime/ 10 000 000 *1 000 000
//lpUserTime/ 10   *1

function _nanosleep(req,rem:Ptimespec):Integer;
var
 timeout:Int64;
 passed :Int64;
 START:QWORD;
begin
 //test_cancel
 if (req=nil) then Exit(EINVAL);

 timeout:=_time_in_ns_from_timespec(req^);

 //
 //timeout:=((timeout+99999999) div 100000000)*100000000;
 //

 timeout:=-((timeout+99) div 100); //in 100ns

 START:=0;
 if (rem<>nil) then
 begin
  SwSaveTime(START);
 end;

 Case SwDelayExecution(True,@timeout) of
  STATUS_USER_APC,
  STATUS_KERNEL_APC,
  STATUS_ALERTED: //signal interrupt
   begin
    if (rem<>nil) then
    begin
     timeout:=-timeout;
     passed:=SwTimePassedUnits(START);

     if (passed>=timeout) then
     begin
      rem^:=Default(timespec);
     end else
     begin
      timeout:=timeout-passed;
      timeout:=timeout*100;  //100ns to ns
      rem^.tv_sec :=timeout div POW10_9;
      rem^.tv_nsec:=timeout mod POW10_9;
     end;

    end;
    Result:=EINTR;
   end;
  else
   begin
    if (rem<>nil) then
    begin
     rem^:=Default(timespec);
    end;
    Result:=0;
   end;
 end;

 //test_cancel
end;

function ps4_nanosleep(req,rem:Ptimespec):Integer; SysV_ABI_CDecl; //nanoseconds
begin
 Result:=_set_errno(_nanosleep(req,rem));
end;

function ps4_sceKernelNanosleep(req,rem:Ptimespec):Integer; SysV_ABI_CDecl; //nanoseconds
var
 tmp:timespec;
begin
 tmp:=Default(timespec);

 repeat
  Result:=_nanosleep(req,@tmp);
  _set_errno(Result);

  if (Result=0) then
  begin
   if (rem<>nil) then
   begin
    rem^:=tmp;
   end;
   Exit;
  end;

  req:=@tmp;
 until (Result<>EINTR);

 Result:=px2sce(Result);
end;

function ps4_usleep(usec:DWORD):Integer; SysV_ABI_CDecl; //microseconds
var
 time:timespec;
begin
 //test_cancel
 time.tv_sec :=usec div 1000000;
 time.tv_nsec:=((usec mod 1000000)*1000);
 Result:=_set_errno(_nanosleep(@time,nil));
 //test_cancel
end;

function ps4_sceKernelUsleep(usec:DWORD):Integer; SysV_ABI_CDecl; //microseconds
var
 req,rem:timespec;
begin
 rem:=Default(timespec);

 req.tv_sec :=usec div 1000000;
 req.tv_nsec:=((usec mod 1000000)*1000);

 repeat
  Result:=_nanosleep(@req,@rem);
  _set_errno(Result);

  if (Result=0) then Exit;

  req:=rem;
 until (Result<>EINTR);

 Result:=px2sce(Result);
end;

function _sleep(sec:DWORD):DWORD;
var
 req,rem:timespec;
 ret,rsec:DWORD;
begin
 rem:=Default(timespec);

 if (Integer(sec)<0) then
 begin
  req.tv_sec :=$7fffffff;
  req.tv_nsec:=0;

  ret:=_nanosleep(@req,@rem);
  _set_errno(ret);

  rsec:=0;
  if (ret<>0) then
  begin
   if (ret=EINTR) then
   begin
    rsec:=(Integer(rem.tv_sec)+1)-DWORD(rem.tv_nsec=0);
   end;
  end;

  Result:=sec+$80000001+rsec;
 end else
 begin
  req.tv_sec :=sec;
  req.tv_nsec:=0;

  ret:=_nanosleep(@req,@rem);
  _set_errno(ret);

  Result:=0;
  if (ret<>0) then
  begin
   Result:=sec;
   if (ret=EINTR) then
   begin
    Result:=(Integer(rem.tv_sec)+1)-DWORD(rem.tv_nsec=0);
   end;
  end;

 end;

end;

function ps4_sleep(sec:DWORD):DWORD; SysV_ABI_CDecl; //second
begin
 //test_cancel
 Result:=_sleep(sec);
 //test_cancel
end;

function ps4_sceKernelSleep(sec:DWORD):DWORD; SysV_ABI_CDecl; //second
begin
 repeat
  //test_cancel
  sec:=_sleep(sec);
  //test_cancel
 until (sec=0);
 Result:=0;
end;

//

function ps4_sceKernelConvertUtcToLocaltime(_time:time_t;
                                            local_time:ptime_t;
                                            _tsec:ptimesec;
                                            _dstsec:PDWORD):Integer; SysV_ABI_CDecl;
var
 tmz:timezone;
 perror:pinteger;
begin
 Result:=ps4_gettimeofday(nil,@tmz);
 if (Result<0) then
 begin
  perror:=_error;
  if (perror<>nil) then
  begin
   Result:=px2sce(perror^);
  end;
  Exit;
 end;

 if (local_time<>nil) then
 begin
  local_time^:=(tmz.tz_minuteswest+tmz.tz_dsttime)*60+_time;
 end;

 if (_tsec<>nil) then
 begin
  _tsec^.tz_time   :=_time;
  _tsec^.tz_secwest:=tmz.tz_minuteswest*60;
  _tsec^.tz_dstsec :=tmz.tz_dsttime*60;
 end;

 if (_dstsec<>nil) then
 begin
  _dstsec^:=tmz.tz_dsttime*60;
 end;
end;

function ps4_sceKernelConvertLocaltimeToUtc(_time:time_t;
                                            unknow:qword;
                                            utc_time:ptime_t;
                                            _tsec:ptimesec;
                                            _dstsec:PDWORD):Integer; SysV_ABI_CDecl;
var
 tmz:timezone;
 rtime:time_t;
 perror:pinteger;
begin
 Result:=ps4_gettimeofday(nil,@tmz);
 if (Result<0) then
 begin
  perror:=_error;
  if (perror<>nil) then
  begin
   Result:=px2sce(perror^);
  end;
  Exit;
 end;

 rtime:=_time-((tmz.tz_minuteswest+tmz.tz_dsttime)*60);

 if (utc_time<>nil) then
 begin
  utc_time^:=rtime;
 end;

 if (_tsec<>nil) then
 begin
  _tsec^.tz_time   :=rtime;
  _tsec^.tz_secwest:=tmz.tz_minuteswest*60;
  _tsec^.tz_dstsec :=tmz.tz_dsttime*60;
 end;

 if (_dstsec<>nil) then
 begin
  _dstsec^:=tmz.tz_dsttime*60;
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

