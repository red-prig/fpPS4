unit ps4_libSceAvSetting;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}
{$WARN 4110 off}

interface

uses
 subr_dynlib;

implementation

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

function ps4_sceAvSettingInit:Integer;
begin
 Result:=0;
end;

type
 pSceVideoOutMode=^SceVideoOutMode;
 SceVideoOutMode=packed record
  size          :DWORD; // sizeof(SceVideoOutMode)
  signalEncoding:Byte;  // SceVideoOutSignalEncoding
  signalRange   :Byte;  // SceVideoOutSignalRange
  colorimetry   :Byte;  // SceVideoOutColorimetry
  depth         :Byte;  // SceVideoOutColorDepth
  refreshRate   :QWORD; // SceVideoOutRefreshRate
  resolution    :QWORD; // SceVideoOutResolution
  contentType   :Byte;  // SceVideoOutContentType
  _reserved0    :array[0..2] of Byte;
  _reserved     :DWORD;
 end;

 SceVideoOutVrViewCropAdjustment=packed record
  verticalOffset:Word;
  reserved0     :Word;
  reserved1     :array[0..2] of DWORD;
 end;

 pSceVideoOutConfigureOptions=^SceVideoOutConfigureOptions;
 SceVideoOutConfigureOptions=packed record
  vrViewCropAdjustment:SceVideoOutVrViewCropAdjustment;
 end;

function ps4_sceAvSettingChangeOutputMode3(
          param_1:Integer; //0x700
          param_2:Pointer; //NULL
          param_3:Pointer; //NULL
          mode   :pSceVideoOutMode;
          param_5:Pointer; //NULL
          options:pSceVideoOutConfigureOptions;
          submit :QWORD
         ):Integer;
begin
 LOG_INFO('sceAvSettingChangeOutputMode3');
 Result:=0;
end;

type
 pAvDeviceInfo=^TAvDeviceInfo;
 TAvDeviceInfo=packed record
  size       :DWORD;
  unknow_0x04:DWORD;
  unknow_0x08:QWORD;
  unknow_0x10:Byte;
  _align1    :array[0..2] of Byte;
  unknow_0x14:Single;
  unknow_0x18:QWORD;
  capability :QWORD;
 end;
 {$IF sizeof(TAvDeviceInfo)<>40}{$STOP sizeof(TAvDeviceInfo)<>40}{$ENDIF}


function ps4_sceAvSettingGetDeviceInfo(
          op   :QWORD; //0x700
          pInfo:pAvDeviceInfo
         ):Integer;
begin
 if (pInfo=nil) then
 begin
  Exit(Integer($809a0002));
 end;

 if (op<>$7000) then
 begin
  Exit(Integer($809a0003));
 end;

 pInfo^:=Default(TAvDeviceInfo);
 pInfo^.size       :=40;
 pInfo^.unknow_0x04:=1;

 Result:=0;
end;

type
 pHdmiMonitorInfo=^THdmiMonitorInfo;
 THdmiMonitorInfo=packed record
  data:array[0..323] of Byte;
 end;

function ps4_sceAvSettingGetHdmiMonitorInfo(op   :QWORD;
                                            pInfo:pHdmiMonitorInfo):Integer;
begin
 if (pInfo=nil) then
 begin
  Exit(Integer($809a0001));
 end;

 case op of
  $7000:;
  $7102:;
  else
   Exit(Integer($809a0003));
 end;

 pInfo^:=Default(THdmiMonitorInfo);

 Result:=0;
end;

type
 PVideoOutModeHdmi=^TVideoOutModeHdmi;
 TVideoOutModeHdmi=packed record
  size    :DWORD;
  unknow2 :Byte;
  unknow3 :Byte;
  unknow4 :Byte;
  unknow5 :Byte;
  unknow6 :QWORD;
  unknow7 :QWORD;
  unknow8 :Byte;
  unknow9 :Byte;
  unknow10:Byte;
  unknow11:Byte;
  unknow12:DWORD;
 end;
 {$IF sizeof(TVideoOutModeHdmi)<>32}{$STOP sizeof(TVideoOutModeHdmi)<>32}{$ENDIF}

function ps4_sceAvSettingIsSupportedVideoOutModeByHdmiMonitorInfo(pInfo:pHdmiMonitorInfo;
                                                                  pMode:PVideoOutModeHdmi):Integer;
begin
 if (pInfo=nil) or
    (pMode=nil) then
 begin
  Exit(Integer($809a0001));
 end;

 if (pMode^.size<>32) or
    (pMode^.unknow9<>$ff) or
    (pMode^.unknow10<>$ff) or
    (pMode^.unknow11<>$ff) or
    (pMode^.unknow12<>$ffffffff) then
 begin
  Exit(Integer($809a0002));
 end;

 Exit(0);
end;

//

function Load_libSceAvSetting(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceAvSetting');

 lib:=Result^.add_lib('libSceAvSetting');
 lib.set_proc($10865D5934FB65DC,@ps4_sceAvSettingInit);
 lib.set_proc($65F3078150D8CF36,@ps4_sceAvSettingChangeOutputMode3);
 lib.set_proc($BE1DDA43E254A525,@ps4_sceAvSettingGetDeviceInfo);
 lib.set_proc($FCCBA2EBB4D9778B,@ps4_sceAvSettingGetHdmiMonitorInfo);
 lib.set_proc($123F5888498E5951,@ps4_sceAvSettingIsSupportedVideoOutModeByHdmiMonitorInfo);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceAvSetting.prx',@Load_libSceAvSetting);

end.

