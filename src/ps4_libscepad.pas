unit ps4_libScePad;

{$mode objfpc}{$H+}

interface

uses
  windows,
  ps4_program,
  sys_signal,
  Classes,
  SysUtils,

  sce_pad_interface,
  sdl2_pad_interface,
  xinput_pad_interface,
  kbm_pad_interface;

Procedure select_pad_interface(const name:RawByteString);
Procedure select_led_color    (const name:RawByteString);

implementation

uses
 sce_pad_types,
 ps4_libSceVideoOut,
 sys_kernel;

Const
 DefaultPadInterface:TAbstractScePadInterface=TXInputPadInterface;

var
 ScePadInterface:TAbstractScePadInterface=nil;

 DefaultPadLightBar:ScePadLightBarParam;

Procedure select_pad_interface(const name:RawByteString);
begin
 case lowercase(name) of
  'xinput'  :ScePadInterface:=TXInputPadInterface;
  'sdl2'    :ScePadInterface:=TSdl2PadInterface;
  'keyboard':ScePadInterface:=TKbmPadInterface;
  else
             ScePadInterface:=DefaultPadInterface; //default
 end;

 if not ScePadInterface.Load then
 begin
  ScePadInterface:=TKbmPadInterface; //reset to kbm
 end;
end;

Procedure select_led_color(const name:RawByteString);
var
 clr:DWord;
begin
 if TryStrToDWord(name,clr) then
 begin
  DefaultPadLightBar.r:=(clr shr 16) and $FF;
  DefaultPadLightBar.g:=(clr shr  8) and $FF;
  DefaultPadLightBar.b:=(clr shr  0) and $FF;
 end;
end;

function ps4_scePadInit():Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'scePadInit');

 if (ScePadInterface=nil) then
 begin
  ScePadInterface:=DefaultPadInterface; //default
  if not ScePadInterface.Load then
  begin
   ScePadInterface:=TKbmPadInterface; //reset to kbm
  end;
 end;

 Result:=ScePadInterface.Init;
end;

function ps4_scePadOpen(userID,_type,index:Integer;param:Pointer):Integer; SysV_ABI_CDecl;
var
 sce_handle:TScePadHandle;
 key:Integer;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);

 case _type of
  SCE_PAD_PORT_TYPE_STANDARD      :;
  SCE_PAD_PORT_TYPE_SPECIAL       :;
  SCE_PAD_PORT_TYPE_REMOTE_CONTROL:;
  else
   begin
    //Writeln('scePadOpen:SCE_PAD_ERROR_INVALID_ARG:1');
    Exit(SCE_PAD_ERROR_INVALID_ARG);
   end;
 end;

 if (_type=SCE_PAD_PORT_TYPE_REMOTE_CONTROL) and
    (userID<>$FF) then {SCE_USER_SERVICE_USER_ID_SYSTEM}
 begin
  //Writeln('scePadOpen:SCE_PAD_ERROR_INVALID_ARG:2');
  Exit(SCE_PAD_ERROR_INVALID_ARG);
 end;

 //Writeln('scePadOpen:[userID=',userID,' type=',_type,' index=',index,']');

 _sig_lock;

 sce_handle:=FindPadByParam(userID,_type,index);
 if (sce_handle<>nil) then
 begin
  //Writeln('scePadOpen:SCE_PAD_ERROR_ALREADY_OPENED');
  sce_handle.Release;
  _sig_unlock;
  Exit(SCE_PAD_ERROR_ALREADY_OPENED);
 end;

 sce_handle:=nil;
 Result:=ScePadInterface.Open(sce_handle);
 if (Result<>0) then
 begin
  //Writeln('scePadOpen:',HexStr(Result,8));
  _sig_unlock;
  Exit;
 end;

 sce_handle.SetLightBar(@DefaultPadLightBar);

 key:=0;
 if pad_handles.New(sce_handle,key) then
 begin
  sce_handle.userID:=userID;
  sce_handle._type :=_type;
  sce_handle.index :=index;
  sce_handle.handle:=key;
  //
  SavePadHandle(sce_handle);
  //
  sce_handle.Release;
  //
  Result:=key;
 end else
 begin
  Result:=SCE_PAD_ERROR_FATAL;
 end;

 //Writeln('scePadOpen:',Result);

 _sig_unlock;
end;

function ps4_scePadGetHandle(userID,_type,index:Integer):Integer; SysV_ABI_CDecl;
var
 sce_handle:TScePadHandle;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);

 sce_handle:=FindPadByParam(userID,_type,index);

 if (sce_handle=nil) then Exit(SCE_PAD_ERROR_NO_HANDLE);

 Result:=sce_handle.handle;

 sce_handle.Release;
end;

function ps4_scePadClose(handle:Integer):Integer; SysV_ABI_CDecl;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);

 if not pad_handles.Delete(handle) then Exit(SCE_PAD_ERROR_INVALID_HANDLE);

 Result:=0;
end;


function ps4_scePadReadState(handle:Integer;data:PScePadData):Integer; SysV_ABI_CDecl;
var
 sce_handle:TScePadHandle;
begin
 Result:=0;
 if (data=nil) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);

 data^:=Default(ScePadData);

 _sig_lock;

 sce_handle:=TScePadHandle(pad_handles.Acqure(handle));

 if (sce_handle=nil) then
 begin
  _sig_unlock;
  Exit(SCE_PAD_ERROR_INVALID_HANDLE);
 end;

 //connect data

 data^.timestamp:=Sysutils.GetTickCount64;

 data^.connected     :=True;
 data^.connectedCount:=1;

 data^.leftStick.x   :=$80;
 data^.leftStick.y   :=$80;
 data^.rightStick.x  :=$80;
 data^.rightStick.y  :=$80;
 data^.orientation.w :=1;
 data^.acceleration.y:=1;

 //only in active windows

 if (MainWindows<>GetForegroundWindow) then
 begin
  sce_handle.Release;
  _sig_unlock;
  Result:=0;
  Exit;
 end;

 sce_handle.ReadState(data);

 //data^.connected     :=True;

 sce_handle.Release;
 _sig_unlock;
 Result:=0;
end;

function ps4_scePadRead(handle:Integer;data:PScePadData;num:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 if (data=nil) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);

 if (num<>0) then
 begin
  ps4_scePadReadState(handle,data);
  Result:=1;
 end;
end;

function ps4_scePadSetVibration(handle:Integer;pParam:PScePadVibrationParam):Integer; SysV_ABI_CDecl;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);
 Result:=0;
end;

function ps4_scePadSetLightBar(handle:Integer;pParam:PScePadLightBarParam):Integer; SysV_ABI_CDecl;
var
 sce_handle:TScePadHandle;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);

 if (pParam=nil) then Exit(SCE_PAD_ERROR_INVALID_ARG);

 _sig_lock;

 sce_handle:=TScePadHandle(pad_handles.Acqure(handle));

 if (sce_handle=nil) then
 begin
  _sig_unlock;
  Exit(SCE_PAD_ERROR_INVALID_HANDLE);
 end;

 Result:=sce_handle.SetLightBar(pParam);

 sce_handle.Release;
 _sig_unlock;
end;

function ps4_scePadResetLightBar(handle:Integer):Integer; SysV_ABI_CDecl;
var
 sce_handle:TScePadHandle;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);

 _sig_lock;

 sce_handle:=TScePadHandle(pad_handles.Acqure(handle));

 if (sce_handle=nil) then
 begin
  _sig_unlock;
  Exit(SCE_PAD_ERROR_INVALID_HANDLE);
 end;

 Result:=sce_handle.ResetLightBar();

 sce_handle.Release;
 _sig_unlock;
end;

function ps4_scePadGetControllerInformation(handle:Integer;
                                            pInfo:PScePadControllerInformation
                                            ):Integer; SysV_ABI_CDecl;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);
 if (pInfo=nil) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 pInfo^:=Default(ScePadControllerInformation);
 pInfo^.touchPadInfo.pixelDensity := 1;
 pInfo^.touchPadInfo.resolution.x := 1920;
 pInfo^.touchPadInfo.resolution.y := 950;
 pInfo^.stickInfo.deadZoneLeft    := 2;
 pInfo^.stickInfo.deadZoneRight   := 2;
 pInfo^.connectionType := SCE_PAD_CONNECTION_TYPE_LOCAL;
 pInfo^.connectedCount := 1;
 pInfo^.connected      := True;
 pInfo^.deviceClass    := SCE_PAD_DEVICE_CLASS_STANDARD;
 Result:=0;
end;

function ps4_scePadDeviceClassGetExtendedInformation(handle:Integer;
                                                     pExtInfo:pScePadDeviceClassExtendedInformation
                                                     ):Integer; SysV_ABI_CDecl;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);
 if (pExtInfo=nil) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 pExtInfo^:=Default(ScePadDeviceClassExtendedInformation);
 pExtInfo^.deviceClass:=SCE_PAD_DEVICE_CLASS_STANDARD;
 Result:=0;
end;

function ps4_scePadDeviceClassParseData(handle:Integer;
                                        pData:pScePadData;
                                        pDeviceClassData:pScePadDeviceClassData
                                        ):Integer; SysV_ABI_CDecl;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);
 if (pData=nil) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 if (pDeviceClassData=nil) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 pDeviceClassData^:=Default(ScePadDeviceClassData);
 pDeviceClassData^.deviceClass:=SCE_PAD_DEVICE_CLASS_STANDARD;
 pDeviceClassData^.bDataValid:=True;
 Result:=0;
end;

function ps4_scePadSetMotionSensorState(handle:Integer;bEnable:Boolean):Integer; SysV_ABI_CDecl;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);
 Result:=0;
end;

function ps4_scePadResetOrientation(handle:Integer):Integer; SysV_ABI_CDecl;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);
 Result:=0;
end;

function ps4_scePadSetTiltCorrectionState(handle:Integer;bEnable:BOOL):Integer; SysV_ABI_CDecl;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);
 Result:=0;
end;

function ps4_scePadSetAngularVelocityDeadbandState(handle:Integer;bEnable:BOOL):Integer; SysV_ABI_CDecl;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);
 Result:=0;
end;

type
 p_pad_ext_param=^t_pad_ext_param;
 t_pad_ext_param=packed record
  param_0:Word;
  param_1:Word;
  param_2:Word;
  param_3:Byte;
 end;

function ps4_scePadOpenExt(userID,_type,index:Integer;param:p_pad_ext_param):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_scePadOpen(userID,_type,index,param);
end;

function ps4_scePadGetExtControllerInformation(handle:Integer;
                                               pInfo:pScePadExtControllerInformation):Integer; SysV_ABI_CDecl;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);

 pInfo^:=Default(ScePadExtControllerInformation);

 Result:=ps4_scePadGetControllerInformation(handle,@pInfo^.base);
end;

function ps4_scePadOutputReport(param_1:Integer;param_2:Byte;param_3:Pointer;param_4:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libScePad(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libScePad');

 lib^.set_proc($86FD65BA226BA903,@ps4_scePadInit);
 lib^.set_proc($EA77207B9FA5E50B,@ps4_scePadClose);
 lib^.set_proc($C64D0071AACFDD5E,@ps4_scePadOpen);
 lib^.set_proc($BB51911E9FA85A86,@ps4_scePadGetHandle);
 lib^.set_proc($6277605EA41557B7,@ps4_scePadReadState);
 lib^.set_proc($AB570735F1B270B2,@ps4_scePadRead);
 lib^.set_proc($C8556739D1B1BD96,@ps4_scePadSetVibration);
 lib^.set_proc($451E27A2F50410D6,@ps4_scePadSetLightBar);
 lib^.set_proc($0EC703D62F475F5C,@ps4_scePadResetLightBar);
 lib^.set_proc($8233FDFCA433A149,@ps4_scePadGetControllerInformation);
 lib^.set_proc($01CB25A4DD631D1F,@ps4_scePadDeviceClassGetExtendedInformation);
 lib^.set_proc($2073EA71B734CC20,@ps4_scePadDeviceClassParseData);
 lib^.set_proc($72556F2F86439EDC,@ps4_scePadSetMotionSensorState);
 lib^.set_proc($AC866747A792A6F9,@ps4_scePadResetOrientation);
 lib^.set_proc($BC32CCA092DD7BC2,@ps4_scePadSetTiltCorrectionState);
 lib^.set_proc($AF8E260317521BE5,@ps4_scePadSetAngularVelocityDeadbandState);
 lib^.set_proc($58522249F5C652AF,@ps4_scePadOpenExt);
 lib^.set_proc($8466DFD904C19AA7,@ps4_scePadGetExtControllerInformation);
 lib^.set_proc($0EB52EF1C3EB8DEF,@ps4_scePadOutputReport);
end;

initialization
 ps4_app.RegistredPreLoad('libScePad.prx',@Load_libScePad);

end.

