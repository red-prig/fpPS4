unit ps4_libSceRtc;

{$mode ObjFPC}{$H+}

interface

uses
  sys_types,
  ps4_program,
  ps4_libkernel,
  ps4_time,
  Classes,
  SysUtils;

const
 SCE_RTC_ERROR_NOT_INITIALIZED    =$80B50001;
 SCE_RTC_ERROR_INVALID_POINTER    =$80B50002;
 SCE_RTC_ERROR_INVALID_VALUE      =$80B50003;
 SCE_RTC_ERROR_INVALID_ARG        =$80B50004;
 SCE_RTC_ERROR_NOT_SUPPORTED      =$80B50005;
 SCE_RTC_ERROR_NO_CLOCK           =$80B50006;
 SCE_RTC_ERROR_BAD_PARSE          =$80B50007;
 SCE_RTC_ERROR_INVALID_YEAR       =$80B50008;
 SCE_RTC_ERROR_INVALID_MONTH      =$80B50009;
 SCE_RTC_ERROR_INVALID_DAY        =$80B5000A;
 SCE_RTC_ERROR_INVALID_HOUR       =$80B5000B;
 SCE_RTC_ERROR_INVALID_MINUTE     =$80B5000C;
 SCE_RTC_ERROR_INVALID_SECOND     =$80B5000D;
 SCE_RTC_ERROR_INVALID_MICROSECOND=$80B5000E;

 SCE_RTC_DAYOFWEEK_SUNDAY   =0;
 SCE_RTC_DAYOFWEEK_MONDAY   =1;
 SCE_RTC_DAYOFWEEK_TUESDAY  =2;
 SCE_RTC_DAYOFWEEK_WEDNESDAY=3;
 SCE_RTC_DAYOFWEEK_THURSDAY =4;
 SCE_RTC_DAYOFWEEK_FRIDAY   =5;
 SCE_RTC_DAYOFWEEK_SATURDAY =6;

type
 pSceRtcDateTime=^SceRtcDateTime;
 SceRtcDateTime=packed record
  year  :Word;
  month :Word;
  day   :Word;
  hour  :Word;
  minute:Word;
  second:Word;
  microsecond:DWORD;
 end;

implementation

uses
 sys_kernel;

function SDK_VERSION:DWORD;
begin
 Result:=sys_kernel.SDK_VERSION;
 if (Result=0) then Result:=$5050031;
end;

function ps4_module_start(args:QWORD;argp:Pointer):Integer; SysV_ABI_CDecl; //BaOKcng8g88
begin
 Result:=0;
end;

function ps4_module_stop(args:QWORD;argp:Pointer):Integer; SysV_ABI_CDecl; //KpDMrPHvt3Q
begin
 Result:=0;
end;

function ps4_sceRtcInit():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceRtcEnd():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

//

function _set_tz(tz_minuteswest,tz_dsttime:Integer):Integer;
var
 tz:timezone;
begin
 tz.tz_minuteswest:=tz_minuteswest;
 tz.tz_dsttime    :=tz_dsttime;
 Result:=ps4_sceKernelSettimeofday(nil,@tz);
end;

function _sceRtcTickSubMicroseconds(pTick0,pTick1:PQWORD;lSub:Int64):Integer;
var
 t1:QWORD;
begin
 if (pTick0=nil) or (pTick1=nil) then Exit(-$7f4afffe);

 if (lSub=0) then
 begin
  pTick0^:=pTick1^;
  Exit(0);
 end;

 t1:=pTick1^;

 if (lSub<0) then
 begin
  if (t1 < QWORD(-lSub)) then Exit(-$7f4afffd);
 end else
 begin
  if ((not t1) < QWORD(lSub)) then Exit(-$7f4afffd);
 end;

 t1:=t1+lSub;
 pTick0^:=t1;
 Result:=0;
end;

function _sceRtcTickAddMinutes(pTick0,pTick1:PQWORD;lAdd:Int64):Integer;
var
 ladd_mul:QWORD;
 t1:QWORD;
begin
 if (pTick0=nil) or (pTick1=nil) then Exit(-$7f4afffe);

 if (lAdd=0) then
 begin
  pTick0^:=pTick1^;
  Exit(0);
 end;

 ladd_mul:=lAdd*60000000;

 t1:=pTick1^;

 if (lAdd < 0) then
 begin
  if (t1 < QWORD(-ladd_mul)) then Exit(-$7f4afffd);
 end else
 begin
  if ((not t1) < QWORD(ladd_mul)) then Exit(-$7f4afffd);
 end;

 pTick0^:=t1+ladd_mul;
 Result:=0;
end;


function leap_year(year:Word):Boolean; inline;
begin
 if (year=(((year shr 4) div $19)*400)) then
 begin
  Result:=True;
 end else
 if (year=(((year shr 2) div $19)*100)) then
 begin
  Result:=False;
 end else
 begin
  Result:=(year and 3)=0;
 end;
end;

function _sceRtcCheckValid(pTime:pSceRtcDateTime):Integer; inline;
var
 year:WORD;
 leap:Boolean;
begin
 if (pTime=nil) then Exit(-$7f4afffe);

 year:=pTime^.year;

 if (year=0) or (year>9999)                    then Exit(-$7f4afff8);
 if (pTime^.month=0) or (pTime^.month>12)      then Exit(-$7f4afff7);
 if (pTime^.day=0)                             then Exit(-$7f4afff6);

 leap:=leap_year(year);
 if (pTime^.day>MonthDays[leap][pTime^.month]) then Exit(-$7f4afff6);
 if (pTime^.hour>=24)                          then Exit(-$7f4afff5);
 if (pTime^.minute>=60)                        then Exit(-$7f4afff4);
 if (pTime^.second>=60)                        then Exit(-$7f4afff3);
 if (pTime^.microsecond>=1000000)              then Exit(-$7f4afff2);
 Result:=0;
end;

//

function ps4_sceRtcTickAddTicks(pTick0,pTick1:PQWORD;lAdd:Int64):Integer; SysV_ABI_CDecl;
begin
 if (pTick0=nil) or (pTick1=nil) then Exit(-$7f4afffe);

 pTick0^:=lAdd+pTick1^;

 Result:=0;
end;

function ps4_sceRtcTickAddMicroseconds(pTick0,pTick1:PQWORD;lAdd:Int64):Integer; SysV_ABI_CDecl;
begin
 if (pTick0=nil) or (pTick1=nil) then Exit(-$7f4afffe);

 pTick0^:=lAdd+pTick1^;

 Result:=0;
end;

function ps4_sceRtcTickAddSeconds(pTick0,pTick1:PQWORD;lAdd:Int64):Integer; SysV_ABI_CDecl;
begin
 if (pTick0=nil) or (pTick1=nil) then Exit(-$7f4afffe);

 pTick0^:=(lAdd*1000000)+pTick1^;

 Result:=0;
end;

function ps4_sceRtcTickAddMinutes(pTick0,pTick1:PQWORD;lAdd:Int64):Integer; SysV_ABI_CDecl;
begin
 if (pTick0=nil) or (pTick1=nil) then Exit(-$7f4afffe);

 pTick0^:=(lAdd*60000000)+pTick1^;

 Result:=0;
end;

function ps4_sceRtcTickAddHours(pTick0,pTick1:PQWORD;lAdd:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pTick0=nil) or (pTick1=nil) then Exit(-$7f4afffe);

 pTick0^:=(Int64(lAdd)*3600000000)+pTick1^;

 Result:=0;
end;

function ps4_sceRtcTickAddDays(pTick0,pTick1:PQWORD;lAdd:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pTick0=nil) or (pTick1=nil) then Exit(-$7f4afffe);

 pTick0^:=(Int64(lAdd)*86400000000)+pTick1^;

 Result:=0;
end;


function ps4_sceRtcTickAddWeeks(pTick0,pTick1:PQWORD;lAdd:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pTick0=nil) or (pTick1=nil) then Exit(-$7f4afffe);

 pTick0^:=(Int64(lAdd)*$8cd0e3a000)+pTick1^;

 Result:=0;
end;

//

function ps4_sceRtcSetConf(param_1,param_2:QWORD;tz_minuteswest,tz_dsttime:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=_set_tz(tz_minuteswest,tz_dsttime);
end;

function ps4_sceRtcSetCurrentTick(pTick:PQWORD):Integer; SysV_ABI_CDecl;
var
 tick:QWORD;
 time:timeval;
begin
 if (pTick=nil) then Exit(-$7f4afffe);
 if (pTick^<=$dcbffeff2bbfff) then Exit(-$7f4afffd);

 tick:=pTick^+$ff23400100d44000;

 time.tv_sec :=tick div 1000000;
 time.tv_usec:=tick mod 1000000;
 Result:=ps4_sceKernelSettimeofday(@time,nil);
end;

function ps4_sceRtcGetCurrentTick(pTick:PQWORD):Integer; SysV_ABI_CDecl;
var
 time:timespec;
begin
 if (pTick=nil) then Exit(-$7f4afffe);

 Result:=ps4_sceKernelClockGettime(0,@time);

 if (Result>=0) then
 begin
  pTick^:=(time.tv_nsec div 1000) + (time.tv_sec*1000000) + $dcbffeff2bc000;
 end
end;

function ps4_sceRtcSetTick(pTime:pSceRtcDateTime;pTick:PQWORD):Integer; SysV_ABI_CDecl;
var
 ly,ld,lm,j:cardinal;
 days:qword;
 msec:qword;
begin
 if (pTime=nil) or (pTick=nil) then Exit(-$7f4afffe);

 days:=pTick^ div (3600000*1000*24);
 msec:=pTick^ mod (3600000*1000*24);

 days:=days+307;

 j := pred(days SHL 2);
 ly:= j DIV 146097;
 j:= j - 146097 * cardinal(ly);
 ld := j SHR 2;
 j:=(ld SHL 2 + 3) DIV 1461;
 ld:= (cardinal(ld) SHL 2 + 7 - 1461*j) SHR 2;
 lm:=(5 * ld-3) DIV 153;
 ld:= (5 * ld +2 - 153*lm) DIV 5;
 ly:= 100 * cardinal(ly) + j;
 if lm < 10 then
 begin
  inc(lm,3);
 end else
 begin
  dec(lm,9);
  inc(ly);
 end;

 pTime^.year :=ly;
 pTime^.month:=lm;
 pTime^.day  :=ld;

 pTime^.Hour   := msec div (3600000*1000);
 msec := msec mod (3600000*1000);
 pTime^.Minute := msec div (60000*1000);
 msec := msec mod (60000*1000);
 pTime^.Second := msec div (1000*1000);
 msec := msec mod (1000*1000);
 pTime^.microsecond := msec;
end;

function _sceRtcGetTick(pTime:pSceRtcDateTime;pTick:PQWORD):Integer;
var
 c,ya:cardinal;
 days:qword;
 msec:qword;
begin
 Result:=0;
 if (pTime^.month>2) then
 begin
  Dec(pTime^.month,3);
 end else
 begin
  Inc(pTime^.month,9);
  Dec(pTime^.Year);
 end;
 c:= pTime^.Year DIV 100;
 ya:=pTime^.Year - 100*c;

 days:=(146097*c) SHR 2 + (1461*ya) SHR 2 + (153*cardinal(pTime^.Month)+2) DIV 5 + cardinal(pTime^.Day);
 days:=days-307;
 days:=days*(3600000*1000*24);

 msec:=cardinal(pTime^.Hour)*(3600000*1000)+
       cardinal(pTime^.minute)*(60000*1000)+
       cardinal(pTime^.second)*(1000*1000)+
       pTime^.microsecond;

 pTick^:=days+msec;
end;

function ps4_sceRtcGetTick(pTime:pSceRtcDateTime;pTick:PQWORD):Integer; SysV_ABI_CDecl;
begin
 if (pTick=nil) then Exit(-$7f4afffe);
 Result:=_sceRtcCheckValid(pTime);
 if (Result<>0) then Exit;
 Result:=_sceRtcGetTick(pTime,pTick);
end;

function ps4_sceRtcGetCurrentClock(pTime:pSceRtcDateTime;iTimeZone:Integer):Integer; SysV_ABI_CDecl;
var
 tick:QWORD;
 time:timespec;
begin
 if (pTime=nil) then Exit(-$7f4afffe);

 Result:=ps4_sceKernelClockGettime(0,@time);

 if (Result>=0) then
 begin
  tick:=(time.tv_nsec div 1000) + (time.tv_sec * 1000000) + $dcbffeff2bc000;
  ps4_sceRtcTickAddMinutes(@tick,@tick,iTimeZone);
  ps4_sceRtcSetTick(pTime,@tick);
 end;
end;

function ps4_sceRtcGetCurrentClockLocalTime(pTime:pSceRtcDateTime):Integer; SysV_ABI_CDecl;
var
 local_time:time_t;
 _tick,tick:QWORD;
 time:timespec;
 tsec:timesec;
begin
 if (pTime=nil) then Exit(-$7f4afffe);

 Result:=ps4_sceKernelClockGettime(0,@time);

 if (Result>=0) then
 begin
  _tick:= (time.tv_nsec div 1000) + (time.tv_sec * 1000000);
  tick := _tick + $dcbffeff2bc000;

  Result:=ps4_sceKernelConvertUtcToLocaltime(_tick div 1000000,@local_time,@tsec,nil);

  if (Result>=0) then
  begin
   ps4_sceRtcTickAddMinutes(@tick,@tick,(tsec.tz_dstsec + tsec.tz_secwest) div $3c);
   ps4_sceRtcSetTick(pTime,@tick);
  end;

 end;
end;

function ps4_sceRtcConvertUtcToLocalTime(pUtc,pLocalTime:PQWORD):Integer; SysV_ABI_CDecl;
var
 tsec:timesec;
 local_time:time_t;
begin
 if (pUtc=nil) then Exit(-$7f4afffe);

 Result:=ps4_sceKernelConvertUtcToLocaltime((pUtc^ + $ff23400100d44000) div 1000000,@local_time,@tsec,nil);

 if (Result>=0) then
 begin
  Result:=_sceRtcTickSubMicroseconds(pLocalTime,pUtc,((tsec.tz_dstsec + tsec.tz_secwest) div $3c) * 60000000);
 end;
end;

function ps4_sceRtcConvertLocalTimeToUtc(pLocalTime,pUtc:PQWORD):Integer; SysV_ABI_CDecl;
var
 tsec:timesec;
 utc_time:time_t;
begin
 if (pLocalTime=nil) then Exit(-$7f4afffe);

 Result:=ps4_sceKernelConvertLocaltimeToUtc((pLocalTime^ + $ff23400100d44000) div 1000000,$ffffffff,@utc_time,@tsec,nil);

 if (Result>=0) then
 begin
  Result:=ps4_sceRtcTickAddMinutes(pUtc,pLocalTime,-((tsec.tz_dstsec + tsec.tz_secwest) div $3c));
 end;
end;


function ps4_sceRtcGetCurrentNetworkTick(pTick:PQWORD):Integer; SysV_ABI_CDecl;
var
 perror:Pinteger;
 time:timespec;
begin
 if (pTick=nil) then Exit(-$7f4afffe);

 Result:=ps4_clock_gettime($10,@time);
 if (Result=0) then
 begin
  pTick^:=(time.tv_nsec div 1000) + (time.tv_sec * 1000000) + $dcbffeff2bc000;
 end else
 begin
  perror:=ps4___error;
  Result:=-$7f4affff;
  if (perror^<>5) then
  begin
   Result:=ps4_sceKernelError(perror^);
  end;
 end;
end;

function ps4_sceRtcGetCurrentRawNetworkTick(pTick:PQWORD):Integer; SysV_ABI_CDecl;
var
 perror:Pinteger;
 time:timespec;
begin
 if (pTick=nil) then Exit(-$7f4afffe);

 Result:=ps4_clock_gettime($13,@time);
 if (Result=0) then
 begin
  pTick^:=(time.tv_nsec div 1000) + (time.tv_sec * 1000000) + $dcbffeff2bc000;
 end else
 begin
  perror:=ps4___error;
  Result:=-$7f4affff;
  if (perror^<>5) then
  begin
   Result:=ps4_sceKernelError(perror^);
  end;
 end;
end;

function ps4_sceRtcGetCurrentDebugNetworkTick(pTick:PQWORD):Integer; SysV_ABI_CDecl;
var
 perror:Pinteger;
 time:timespec;
begin
 if (pTick=nil) then Exit(-$7f4afffe);

 Result:=ps4_clock_gettime($11,@time);
 if (Result<>0) then
 begin
  perror:=ps4___error;
  Result:=-$7f4affff;

  if (perror^<>5) then
  begin
   Result:=ps4_sceKernelError(perror^);
   if (Result<>-$7f4affff) then Exit;
  end;
  Result:=ps4_clock_gettime($10,@time);

  Result:=-$7f4affff;
  if (perror^<>5) then
  begin
   Result:=ps4_sceKernelError(perror^);
  end;

  Exit;
 end;

 pTick^:=(time.tv_nsec div 1000) + (time.tv_sec * 1000000) + $dcbffeff2bc000;
 Result:=0;
end;

function ps4_sceRtcGetCurrentAdNetworkTick(pTick:PQWORD):Integer; SysV_ABI_CDecl;
var
 perror:Pinteger;
 time:timespec;
begin
 if (pTick=nil) then Exit(-$7f4afffe);

 Result:=ps4_clock_gettime($12,@time);
 if (Result<>0) then
 begin
  perror:=ps4___error;
  Result:=-$7f4affff;

  if (perror^<>5) then
  begin
   Result:=ps4_sceKernelError(perror^);
   if (Result<>-$7f4affff) then Exit;
  end;
  Result:=ps4_clock_gettime($10,@time);

  Result:=-$7f4affff;
  if (perror^<>5) then
  begin
   Result:=ps4_sceKernelError(perror^);
  end;

  Exit;
 end;

 pTick^:=(time.tv_nsec div 1000) + (time.tv_sec * 1000000) + $dcbffeff2bc000;
 Result:=0;
end;

function ps4_sceRtcSetCurrentNetworkTick(pTick:PQWORD):Integer; SysV_ABI_CDecl;
var
 perror:Pinteger;
 tick:QWORD;
 ptime:ptimespec;
 time:timespec;
begin
 if (pTick=nil) then
 begin
  ptime:=nil;
 end else
 begin
  if (pTick^<$dcbffeff2bc000) then Exit(-$7f4afffd);
  ptime:=@time;
  tick:=pTick^+$ff23400100d44000;
  time.tv_sec :=(tick div 1000000);
  time.tv_nsec:=(tick mod 1000000)*1000;
 end;

 Result:=ps4_clock_settime($10,ptime);
 if (Result<>0) then
 begin
  perror:=ps4___error;
  Result:=ps4_sceKernelError(perror^);
 end;
end;

function ps4_sceRtcSetCurrentDebugNetworkTick(pTick:PQWORD):Integer; SysV_ABI_CDecl;
var
 perror:Pinteger;
 tick:QWORD;
 ptime:ptimespec;
 time:timespec;
begin
 if (pTick=nil) then
 begin
  ptime:=nil;
 end else
 begin
  if (pTick^<$dcbffeff2bc000) then Exit(-$7f4afffd);
  ptime:=@time;
  tick:=pTick^+$ff23400100d44000;
  time.tv_sec :=(tick div 1000000);
  time.tv_nsec:=(tick mod 1000000)*1000;
 end;

 Result:=ps4_clock_settime($11,ptime);
 if (Result<>0) then
 begin
  perror:=ps4___error;
  Result:=ps4_sceKernelError(perror^);
 end;
end;

function ps4_sceRtcSetCurrentAdNetworkTick(pTick:PQWORD):Integer; SysV_ABI_CDecl;
var
 perror:Pinteger;
 tick:QWORD;
 ptime:ptimespec;
 time:timespec;
begin
 if (pTick=nil) then
 begin
  ptime:=nil;
 end else
 begin
  if (pTick^<$dcbffeff2bc000) then Exit(-$7f4afffd);
  ptime:=@time;
  tick:=pTick^+$ff23400100d44000;
  time.tv_sec :=(tick div 1000000);
  time.tv_nsec:=(tick mod 1000000)*1000;
 end;

 Result:=ps4_clock_settime($12,ptime);
 if (Result<>0) then
 begin
  perror:=ps4___error;
  Result:=ps4_sceKernelError(perror^);
 end;
end;

function ps4_sceRtcGetTickResolution:Integer; SysV_ABI_CDecl;
begin
 Result:=1000000;
end;

function ps4_sceRtcIsLeapYear(year:Integer):Integer; SysV_ABI_CDecl;
begin
 if (year<1) then
 begin
  Exit(-$7f4afff8);
 end;
 if (year<>(year div 400)*400) then
 begin
  if (year<>(year div 100)*100) then
  begin
   Exit(Integer((year and 3)=0));
  end;
  Result:=0;
 end;
 Result:=1;
end;

function ps4_sceRtcGetDaysInMonth(year,month:Integer):Integer; SysV_ABI_CDecl;
var
 leap:Boolean;
begin
 if (year<=0)  then Exit(-$7f4afff8);
 if (month<=0) then Exit(-$7f4afff7);
 if (month>12) then Exit(-$7f4afff7);

 leap:=leap_year(year);
 Result:=MonthDays[leap][month];
end;

function int64_mul_high(a,b:Int64):Int64; assembler; nostackframe;
asm
 mov  a,%rax
 imul b
 mov  %rdx,Result
end;

function ps4_sceRtcGetDayOfWeek(year,month,day:Integer):Integer; SysV_ABI_CDecl;
var
 month_m1:DWORD;

 v10,v11:Int64;

 days:Byte;
 leap:Boolean;
begin

 if (SDK_VERSION < $3000000) then
 begin
  if (year<1)    then Exit(-$7f4afff8);
  if (month<=0)  then Exit(-$7f4afff7);
  if (month>12)  then Exit(-$7f4afff7);
 end else
 begin
  if (month<=0)  then Exit(-$7f4afff7);
  if (month>12)  then Exit(-$7f4afff7);
  if (year<1)    then Exit(-$7f4afff8);
  if (year>9999) then Exit(-$7f4afff8);
 end;

 month_m1:=month-1;

 leap:=leap_year(year);
 days:=MonthDays[leap][month];

 if ((day <= 0) or (day > days)) then Exit(-$7f4afff6);

 if (month_m1<2) then
 begin
  month:=month+12;
 end;

 v10 := Integer(year - int32(month_m1 < 2));

 v11 := int64_mul_high($5C28F5C28F5C28F5 , v10) - v10;

 v11 := (v11 shr $3F) + SarInt64(v11 , 6);

 v10 := v10 + (v10 div 4) + (v10 div 400);

 Result := Integer( (13 * month + 8) div 5
                     + v10
                     + v11
                     + day)
                    mod 7;

end;

function ps4_sceRtcCheckValid(pTime:pSceRtcDateTime):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceRtcCheckValid(pTime);
end;

function ps4_sceRtcSetDosTime(pTime:pSceRtcDateTime;uiDosTime:DWORD):Integer; SysV_ABI_CDecl;
var
 days:Word;
begin
 if (pTime=nil) then Exit(-$7f4afffe);

 pTime^.microsecond:= 0;
 pTime^.second     := (uiDosTime shl 1) and $3e;
 pTime^.minute     := (uiDosTime shr 5) and $3f;
 pTime^.hour       := (uiDosTime and $f800) shr $b;

 days := uiDosTime shr $10;

 pTime^.day        := (days and $1f);
 pTime^.month      := (days shr 5) and $f;
 pTime^.year       := (days shr 9) + $7bc;

 Result:=0
end;

function ps4_sceRtcGetDosTime(pTime:pSceRtcDateTime;puiDosTime:PDWORD):Integer; SysV_ABI_CDecl;
var
 days:Word;
 year:Word;
 month:Word;
begin
 if (puiDosTime=nil) then Exit(-$7f4afffe);
 Result:=_sceRtcCheckValid(pTime);
 if (Result<>0) then Exit;

 year := pTime^.year;

 month := pTime^.month;

 days := pTime^.day;

 if (year < 1980) then
 begin
  puiDosTime^ := 0;
 end else
 begin
  if (year < 2108) then
  begin
   puiDosTime^ := ((pTime^.second shr 1) and $1f) or
                  ((pTime^.minute and $3f) shl 5) or
                  ((pTime^.hour and $1f) shl $b) or
                  (((month and $f) * $20 + $8800 + (year * $200) or (days and $1f)) shl $10);
   Exit(0);
  end;
  puiDosTime^ := $ff9fbf7d;
 end;
 Result := -$7f4afff8;

end;

function ps4_sceRtcSetWin32FileTime(pTime:pSceRtcDateTime;ulWin32Time:QWORD):Integer; SysV_ABI_CDecl;
var
 tick:QWORD;
begin
 if (pTime=nil) then Exit(-$7f4afffe);

 tick:=(ulWin32Time div 10) + $b36168b6a58000;
 ps4_sceRtcSetTick(pTime,@tick);

 Result:=0;
end;

function ps4_sceRtcGetWin32FileTime(pTime:pSceRtcDateTime;pulWin32Time:PQWORD):Integer; SysV_ABI_CDecl;
var
 tick:qword;
begin
 if (pulWin32Time=nil) then Exit(-$7f4afffe);
 Result:=_sceRtcCheckValid(pTime);
 if (Result<>0) then Exit;

 Result:=_sceRtcGetTick(pTime,@tick);

 if (tick < $b36168b6a58000) then
 begin
  pulWin32Time^:=0;
  Result:=(-Integer(pTime^.year<1601)*5)+(-$7f4afffd);
 end else
 begin
  pulWin32Time^:=tick*10+-$701ce1722770000;
 end;
end;

function ps4_sceRtcSetTime_t(pTime:pSceRtcDateTime;iTime:Int64):Integer; SysV_ABI_CDecl;
var
 tick:QWORD;
begin
 if (SDK_VERSION<$3000000) then
 begin
  iTime:=iTime and $ffffffff;
 end else
 if (iTime<0) then
 begin
  Exit(-$7f4afffd);
 end;

 if (pTime=nil) then
 begin
  Exit(-$7f4afffe);
 end else
 begin
  tick:=iTime*1000000+$dcbffeff2bc000;
  ps4_sceRtcSetTick(pTime,@tick);
  Result:=0;
 end;
end;

function ps4_sceRtcGetTime_t(pTime:pSceRtcDateTime;piTime:PInt64):Integer; SysV_ABI_CDecl;
var
 tick:QWORD;
begin
 if (piTime=nil) then Exit(-$7f4afffe);
 Result:=_sceRtcCheckValid(pTime);
 if (Result<>0) then Exit;

 Result:=_sceRtcGetTick(pTime,@tick);

 if (tick < $dcbffeff2bc000) then
 begin
  piTime^:=0;
  Result :=(-Integer(pTime^.year<1970)*5)+(-$7f4afffd);
 end else
 begin
  piTime^:=(tick+$ff23400100d44000) div 1000000;
 end

end;

function ps4_sceRtcCompareTick(pTick0,pTick1:PQWORD):Integer; SysV_ABI_CDecl;
begin
 if (pTick0=nil) or (pTick1=nil) then Exit(-$7f4afffe);
 Result:=-1;
 if (pTick1^<=pTick0^) then
 begin
  Result:=(-Integer(pTick1^<pTick0^)) and 1;
 end;
end;

function ps4_sceRtcTickAddMonths(pTick0,pTick1:PQWORD;iAdd:Integer):Integer; SysV_ABI_CDecl;
var
 Time:SceRtcDateTime;
 TempMonth,S:Integer;
begin
 if (pTick0=nil) or (pTick1=nil) then Exit(-$7f4afffe);

 if (iAdd=0) then
 begin
  pTick0^:=pTick1^;
  Exit(0);
 end;

 ps4_sceRtcSetTick(@Time,pTick1);

 If (iAdd>=0) then
 begin
  s:=1
 end else
 begin
  s:=-1;
 end;

 inc(Time.Year,(iAdd div 12));
 TempMonth:=Time.Month+(iAdd mod 12)-1;

 if (TempMonth>11) or
    (TempMonth<0) then
 begin
  Dec(TempMonth,S*12);
  Inc(Time.Year,S);
 end;

 Time.Month:=TempMonth+1;

 If (Time.Day>MonthDays[leap_year(Time.Year)][Time.Month]) then
 begin
  Time.Day:=MonthDays[leap_year(Time.Year)][Time.Month];
 end;

 Result:=_sceRtcCheckValid(@Time);
 if (Result<>0) then Exit;

 Result:=_sceRtcGetTick(@Time,pTick0);
end;

function ps4_sceRtcTickAddYears(pTick0,pTick1:PQWORD;iAdd:Integer):Integer; SysV_ABI_CDecl;
var
 Time:SceRtcDateTime;
begin
 if (pTick0=nil) or (pTick1=nil) then Exit(-$7f4afffe);

 if (iAdd=0) then
 begin
  pTick0^:=pTick1^;
  Exit(0);
 end;

 ps4_sceRtcSetTick(@Time,pTick1);

 Inc(Time.Year,iAdd);

 Result:=_sceRtcCheckValid(@Time);
 if (Result<>0) then Exit;

 Result:=_sceRtcGetTick(@Time,pTick0);
end;

function Load_libSceRtc(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceRtc');

 lib^.set_proc($05A38A72783C83CF,@ps4_module_start);
 lib^.set_proc($2A90CCACF1EFB774,@ps4_module_stop);
 lib^.set_proc($2E5A1D08C0DB937A,@ps4_sceRtcInit);
 lib^.set_proc($F12963431EA90CFF,@ps4_sceRtcEnd);
 lib^.set_proc($02A54CB2CAF9D917,@ps4_sceRtcTickAddTicks);
 lib^.set_proc($5CF222C39F02F863,@ps4_sceRtcTickAddMicroseconds);
 lib^.set_proc($D3B3B9DB91E0202B,@ps4_sceRtcTickAddSeconds);
 lib^.set_proc($9A7FED7F84221739,@ps4_sceRtcTickAddMinutes);
 lib^.set_proc($30373971DF077C20,@ps4_sceRtcTickAddHours);
 lib^.set_proc($351D49D0DECBDB16,@ps4_sceRtcTickAddDays);
 lib^.set_proc($808E2DD7DE1CD96F,@ps4_sceRtcTickAddWeeks);
 lib^.set_proc($7C52E098D5290A18,@ps4_sceRtcSetConf);
 lib^.set_proc($7787C72C21A663CD,@ps4_sceRtcSetCurrentTick);
 lib^.set_proc($D7C076352D72F545,@ps4_sceRtcGetCurrentTick);
 lib^.set_proc($B9E7A06BABF7194C,@ps4_sceRtcSetTick);
 lib^.set_proc($F30FC7D7D8A9E3C2,@ps4_sceRtcGetTick);
 lib^.set_proc($F257EF9D132AC043,@ps4_sceRtcGetCurrentClock);
 lib^.set_proc($64F0F560E288F8AC,@ps4_sceRtcGetCurrentClockLocalTime);
 lib^.set_proc($3354EF16CB7F8EB3,@ps4_sceRtcConvertUtcToLocalTime);
 lib^.set_proc($F18AF5E37C849D1A,@ps4_sceRtcConvertLocalTimeToUtc);
 lib^.set_proc($CCEF542F7A8820D4,@ps4_sceRtcGetCurrentNetworkTick);
 lib^.set_proc($1D6C4739D6CCFCF8,@ps4_sceRtcGetCurrentRawNetworkTick);
 lib^.set_proc($3ADD431378227FCE,@ps4_sceRtcGetCurrentDebugNetworkTick);
 lib^.set_proc($2CDDD971BEF64347,@ps4_sceRtcGetCurrentAdNetworkTick);
 lib^.set_proc($AA10C1B48A3E6AEC,@ps4_sceRtcSetCurrentNetworkTick);
 lib^.set_proc($54B0D43CA9B0E4BF,@ps4_sceRtcSetCurrentDebugNetworkTick);
 lib^.set_proc($B15DAD2BEC8E8415,@ps4_sceRtcSetCurrentAdNetworkTick);
 lib^.set_proc($8CC370A98AF847F9,@ps4_sceRtcGetTickResolution);
 lib^.set_proc($520F290B042F8747,@ps4_sceRtcIsLeapYear);
 lib^.set_proc($DCEECB9FC02A275A,@ps4_sceRtcGetDaysInMonth);
 lib^.set_proc($0B220AFE2E177604,@ps4_sceRtcGetDayOfWeek);
 lib^.set_proc($94F10161D557D174,@ps4_sceRtcCheckValid);
 lib^.set_proc($6983C27757028728,@ps4_sceRtcSetDosTime);
 lib^.set_proc($13B011E28ECDCBB1,@ps4_sceRtcGetDosTime);
 lib^.set_proc($9F92620095EC6DCB,@ps4_sceRtcSetWin32FileTime);
 lib^.set_proc($8DF44ED2E4E3B730,@ps4_sceRtcGetWin32FileTime);
 lib^.set_proc($6C311554FE1B4E34,@ps4_sceRtcSetTime_t);
 lib^.set_proc($06DAA6A534571E09,@ps4_sceRtcGetTime_t);
 lib^.set_proc($7CD699E036F31C01,@ps4_sceRtcCompareTick);
 lib^.set_proc($08BEB2F6AFD76EE4,@ps4_sceRtcTickAddMonths);
 lib^.set_proc($FF9CB6B89EB6A92F,@ps4_sceRtcTickAddYears);

 ps4_module_start(0,nil);
end;

//TODO sceRtcParseDateTime
//TODO sceRtcParseRFC3339
//TODO sceRtcFormatRFC2822LocalTime
//TODO sceRtcFormatRFC2822
//TODO sceRtcFormatRFC3339LocalTime
//TODO sceRtcFormatRFC3339

initialization
 ps4_app.RegistredPreLoad('libSceRtc.prx',@Load_libSceRtc);

end.

