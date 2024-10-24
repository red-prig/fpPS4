unit ps4_libSceAvSetting;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}
{$WARN 4110 off}

interface

uses
 subr_dynlib;

implementation

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
 Writeln('sceAvSettingChangeOutputMode3');
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
          param_1:QWORD; //0x700
          pInfo  :pAvDeviceInfo
         ):Integer;
begin
 if (pInfo=nil) then
 begin
  Exit(Integer($809a0002));
 end;

 if (param_1<>$7000) then
 begin
  Exit(Integer($809a0003));
 end;

 pInfo^:=Default(TAvDeviceInfo);
 pInfo^.size       :=40;
 pInfo^.unknow_0x04:=1;

 Result:=0;
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
end;

var
 stub:t_int_file;

initialization
 reg_int_file(stub,'libSceAvSetting.prx',@Load_libSceAvSetting);

end.

