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

 if (_type<>SCE_PAD_PORT_TYPE_STANDARD) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 if (index<0) or (index>15) then Exit(SCE_PAD_ERROR_INVALID_ARG);

 sce_handle:=nil;
 Result:=ScePadInterface.Open(index,sce_handle);
 if (Result<>0) then Exit;

 sce_handle.SetLightBar(@DefaultPadLightBar);

 key:=0;
 if pad_handles.New(sce_handle,key) then
 begin
  sce_handle.handle:=key;
  sce_handle.Release;
  Result:=key;
 end else
 begin
  Result:=SCE_PAD_ERROR_FATAL;
 end;
end;

function ps4_scePadGetHandle(userID,_type,index:Integer):Integer; SysV_ABI_CDecl;
begin
 if (ScePadInterface=nil) then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);

 if (_type<>SCE_PAD_PORT_TYPE_STANDARD) then Exit(SCE_PAD_ERROR_INVALID_ARG);

 if (index<0) or (index>15) then Exit(SCE_PAD_ERROR_INVALID_ARG);

 if (pad_opened[index]=nil) then Exit(SCE_PAD_ERROR_NO_HANDLE);

 Result:=pad_opened[index].handle;
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
end;

initialization
 ps4_app.RegistredPreLoad('libScePad.prx',@Load_libScePad);

end.

