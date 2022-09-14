unit ps4_libSceRtc;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  ps4_libkernel,
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
 //ps4_app.RegistredPreLoad('libSceRtc.prx',@Load_libSceRtc);

end.

