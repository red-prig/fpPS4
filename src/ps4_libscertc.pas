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

implementation

type
 pSceRtcDateTime=^SceRtcDateTime;
 SceRtcDateTime=packed record
  year  :Word;
  month :Word;
  day   :Word;
  hour  :Word;
  minute:Word;
  second:Word;
  microsecond:QWORD;
 end;

var
 SDK_VERSION:DWORD;

function ps4_module_start(args:QWORD;argp:Pointer):Integer; SysV_ABI_CDecl; //BaOKcng8g88
begin
 if (ps4_sceKernelGetCompiledSdkVersion(@SDK_VERSION)<0) then
 begin
  SDK_VERSION:=$5050031;
 end;
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

//TODO sceRtcTickAddMonths
//TODO sceRtcTickAddYears

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
label
 _skip,_next1,_next2;
var
 iVar1:Integer;
 uVar2:DWORD;
 year:DWORD;
 days:DWORD;
 ms1:QWORD;
 iVar3:Integer;
 sec:QWORD;
 min:Word;
 ms2:QWORD;
 leap:Boolean;
begin
 if (pTime=nil) or (pTick=nil) then Exit(-$7f4afffe);

 sec := pTick^ div 86400000000;
 ms1 := pTick^ mod 86400000000;
 ms2 := ms1 div 1000000;
 pTime^.microsecond := Integer(ms1) + (Integer(ms2) * -1000000);
 min := Word(ms2 div $3c);
 pTime^.second := Word(ms1 div 1000000) + (min * -$3c);
 year := 1;
 pTime^.minute := min + Word((ms2 div $3c) div $3c) * -$3c;
 pTime^.hour := Word(ms2 div $e10) -
               (Word((ms2 div $e10) div $18) * 8 +
               (Word((ms2 div $e10) * $aaaaaaab shr $20) and $fff0));
 if (DWORD(sec) > $23ab0) then
 begin
  iVar1 := Integer(sec div $23ab1);
  year  := iVar1 * 400 or 1;
  sec   := QWORD(iVar1 * -$23ab1 + DWORD(sec));
 end;
 uVar2 := DWORD(sec);
 if (uVar2 > $8eab) then
 begin
  iVar1 := Integer((sec shr 2) div $23ab);
  iVar3 := iVar1 * 100;
  uVar2 := iVar1 * -$8eac + uVar2;
  if (uVar2 = 0) then
  begin
   year := iVar3 + -1 + year;
   uVar2 := $16d;
   goto _skip;
  end;
  year := iVar3 + year;
 end;
 if ($5b4 < uVar2) then
 begin
  year := year + (uVar2 div $5b5) * 4;
  uVar2 := uVar2 mod $5b5;
 end;

_skip:

 repeat
   days := year mod 400;
   if (((Integer(year) < 1) or (days = 0)) or (((year <> (year div 100) * 100) and ((year and 3) = 0)))) then
   begin
    iVar1 := -$16e;
    if (uVar2 < $16e) then
    begin
     pTime^.year := Word(year);
     if (Integer(year) > 0) then
     begin
      leap:=True;
      if (days <> 0) then
      begin
_next1:
       leap:=leap_year(Word(year));
      end;
     end;
_next2:
     year := 0;
     repeat
      days := uVar2;
      year := year + 1;
      uVar2 := days - MonthDays[leap][ms1];
     until not (MonthDays[leap][ms1] <= days);
     pTime^.month := Word(year);
     pTime^.day := Word(days) + 1;
     Exit(0);
    end;
   end else
   begin
    iVar1 := -$16d;
    if (uVar2 < $16d) then
    begin
     pTime^.year := Word(year);
     if (days <> 0) then goto _next1;
     leap := True;
     goto _next2;
    end;
   end;
   uVar2 := uVar2 + iVar1;
   year := year + 1;
 until false;

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

//TODO sceRtcParseDateTime
//TODO sceRtcParseRFC3339
//TODO sceRtcFormatRFC2822LocalTime
//TODO sceRtcFormatRFC2822
//TODO sceRtcFormatRFC3339LocalTime
//TODO sceRtcFormatRFC3339

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
 if (month>12) then Exit(-$7f4afff7);

 leap:=leap_year(year);
 Result:=MonthDays[leap][month-1];
end;

function ps4_sceRtcGetDayOfWeek(year,month,day:Integer):Integer; SysV_ABI_CDecl;
var
 month_m1:DWORD;
 lVar2:QWORD;
 lVar3:QWORD;
 lVar4:QWORD;
 days:Byte;
 leap:Boolean;
begin

 if (SDK_VERSION < $3000000) then
 begin
  if (year<1)    then Exit(-$7f4afff8);
  if (month>12)  then Exit(-$7f4afff7);
 end else
 begin
  if (month>12)  then Exit(-$7f4afff7);
  if (year<1)    then Exit(-$7f4afff8);
  if (year>9999) then Exit(-$7f4afff8);
 end;

 month_m1:=month-1;

 leap:=leap_year(year);
 days:=MonthDays[leap][month_m1];

 if ((day <= 0) or (day > days)) then Exit(-$7f4afff6);

 if (month_m1<2) then
 begin
  month:=month+$c;
 end;


 lVar3 := (year - Integer(month_m1 < 2));

 //
end;

{
ecx: = rsi + 0xc; //month+$c

r9_7  := int64(edi);

rdi_6 := (r9 * 0x5c28f5c28f5c28f5) - r9;

rdx_5 := (r9 * 0xa3d70a3d70a3d70b) + r9;

rax_4 := int64((esi * 0xd) + 8) * 0x66666667;

rcx_3 := int64(r8d) + r9 +
         ( ( (( r9 >> 0x3f ) >> 0x3e) + r9) >> 2) +
         ( (rdi_6 >> 6) + (rdi_6 >> 0x3f) ) +
         (rdx_5 >> 8) +
         (rdx_5 >> 0x3f) +
         int64(Int32( int64(Int32(rax_4 >> 0x21)) +
         int64(Int32( rax_4 >> 0x3f )) ));

rdx_2 := rcx_3 * (0x4924924924924925);

rdx_1 := rdx_2 >> 1;

ret   := Int32(rcx_3) - (Int32(rdx_1*8) - ( Int32(rdx_1) + Int32( rdx_2 >> 0x3f )));
}


function ps4_sceRtcCheckValid(pTime:pSceRtcDateTime):Integer; SysV_ABI_CDecl;
var
 year:WORD;
 leap:Boolean;
begin
 if (pTime=nil) then  Exit(-$7f4afffe);
 year:=pTime^.year;
 leap:=leap_year(year);
 if (year>9999)                                then Exit(-$7f4afff8);
 if (pTime^.month>12)                          then Exit(-$7f4afff7);
 if (pTime^.day>MonthDays[leap][pTime^.month]) then Exit(-$7f4afff6);
 if (pTime^.hour>=24)                          then Exit(-$7f4afff5);
 if (pTime^.minute>=60)                        then Exit(-$7f4afff4);
 if (pTime^.second>=60)                        then Exit(-$7f4afff3);
 if (pTime^.microsecond>=1000000)              then Exit(-$7f4afff2);
 Result:=0;
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

//TODO

//nop nid:libSceRtc:CCEF542F7A8820D4:sceRtcGetCurrentNetworkTick
//nop nid:libSceRtc:D7C076352D72F545:sceRtcGetCurrentTick

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


 ps4_module_start(0,nil);
end;

initialization
 ps4_sceRtcGetDayOfWeek(2022,6,1);
 //ps4_app.RegistredPreLoad('libSceRtc.prx',@Load_libSceRtc);

end.

