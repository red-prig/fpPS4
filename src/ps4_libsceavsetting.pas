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

//

function Load_libSceAvSetting(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceAvSetting');

 lib:=Result^.add_lib('libSceAvSetting');
 lib.set_proc($10865D5934FB65DC,@ps4_sceAvSettingInit);
 lib.set_proc($65F3078150D8CF36,@ps4_sceAvSettingChangeOutputMode3);
end;

var
 stub:t_int_file;

initialization
 reg_int_file(stub,'libSceAvSetting.prx',@Load_libSceAvSetting);

end.

