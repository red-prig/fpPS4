unit ps4_libScePad;

{$mode objfpc}{$H+}

interface

uses
  windows,
  ps4_program,
  sys_signal,
  Classes,
  SysUtils;

implementation

const
 SCE_PAD_ERROR_INVALID_ARG             =-2137915391; // 0x80920001
 SCE_PAD_ERROR_INVALID_PORT            =-2137915390; // 0x80920002
 SCE_PAD_ERROR_INVALID_HANDLE          =-2137915389; // 0x80920003
 SCE_PAD_ERROR_ALREADY_OPENED          =-2137915388; // 0x80920004
 SCE_PAD_ERROR_NOT_INITIALIZED         =-2137915387; // 0x80920005
 SCE_PAD_ERROR_INVALID_LIGHTBAR_SETTING=-2137915386; // 0x80920006
 SCE_PAD_ERROR_DEVICE_NOT_CONNECTED    =-2137915385; // 0x80920007
 SCE_PAD_ERROR_NO_HANDLE               =-2137915384; // 0x80920008
 SCE_PAD_ERROR_FATAL                   =-2137915137; // 0x809200FF

function ps4_scePadInit():Integer; SysV_ABI_CDecl;
begin
 Writeln('scePadInit');
 Result:=0;
end;

function ps4_scePadOpen(userID,_type,index:Integer;param:Pointer):Integer; SysV_ABI_CDecl;
begin
 //Writeln('scePadOpen:',userID);
 Result:=222;
end;

function ps4_scePadClose(handle:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

const
 ORBIS_PAD_PORT_TYPE_STANDARD=0;

 SCE_PAD_BUTTON_L3          = $00000002;
 SCE_PAD_BUTTON_R3          = $00000004;
 SCE_PAD_BUTTON_OPTIONS     = $00000008;
 SCE_PAD_BUTTON_UP          = $00000010;
 SCE_PAD_BUTTON_RIGHT       = $00000020;
 SCE_PAD_BUTTON_DOWN        = $00000040;
 SCE_PAD_BUTTON_LEFT        = $00000080;
 SCE_PAD_BUTTON_L2          = $00000100;
 SCE_PAD_BUTTON_R2          = $00000200;
 SCE_PAD_BUTTON_L1          = $00000400;
 SCE_PAD_BUTTON_R1          = $00000800;
 SCE_PAD_BUTTON_TRIANGLE    = $00001000;
 SCE_PAD_BUTTON_CIRCLE      = $00002000;
 SCE_PAD_BUTTON_CROSS       = $00004000;
 SCE_PAD_BUTTON_SQUARE      = $00008000;
 SCE_PAD_BUTTON_TOUCH_PAD   = $00100000;
 SCE_PAD_BUTTON_INTERCEPTED = $80000000;

 SCE_PAD_MAX_TOUCH_NUM=2;
 SCE_PAD_MAX_DEVICE_UNIQUE_DATA_SIZE=12;

type
 Tvec_float3=packed record
  x,y,z:Single;
 end;
 Tvec_float4=packed record
  x,y,z,w:Single;
 end;
 ScePadAnalogStick=packed record
  x,y:Byte;
 end;
 ScePadAnalogButtons=packed record
  l2,r2:Byte;
  padding:Word;
 end;

 ScePadTouch=packed record
  x:Word;
  y:Word;
  id:Byte;
  reserve:array[0..2] of Byte;
 end;

 ScePadTouchData=packed record
  touchNum:Byte;
  reserve:array[0..2] of Byte;
  reserve1:DWORD;
  touch:array[0..SCE_PAD_MAX_TOUCH_NUM-1] of ScePadTouch;
 end;

 ScePadExtensionUnitData=packed record
  extensionUnitId:DWORD;
  reserve:Byte;
  dataLength:Byte;
  data:array[0..9] of Byte;
 end;

 PScePadData=^ScePadData;
 ScePadData=packed record
  buttons          :DWORD;
  leftStick        :ScePadAnalogStick;
  rightStick       :ScePadAnalogStick;
  analogButtons    :ScePadAnalogButtons;
  orientation      :Tvec_float4;
  acceleration     :Tvec_float3;
  angularVelocity  :Tvec_float3;
  touchData        :ScePadTouchData;
  connected        :Boolean;
  timestamp        :QWORD;
  extensionUnitData:ScePadExtensionUnitData;
  connectedCount   :Byte;
  reserve:array[0..1] of Byte;
  deviceUniqueDataLen:Byte;
  deviceUniqueData:array[0..SCE_PAD_MAX_DEVICE_UNIQUE_DATA_SIZE-1] of Byte;
 end;

 TPadColor=packed record
  r,g,b,a:Byte;
 end;

 PScePadVibrationParam=^ScePadVibrationParam;
 ScePadVibrationParam=packed record
  largeMotor:Byte;
  smallMotor:Byte;
 end;

 ScePadColor=packed record
  r:Byte;
  g:Byte;
  b:Byte;
  reserve:Byte;
 end;
 ScePadLightBarParam=ScePadColor;
 PScePadLightBarParam=^ScePadLightBarParam;

function ps4_scePadReadState(handle:Integer;data:PScePadData):Integer; SysV_ABI_CDecl;
var
 mPoint:TPoint;
begin
 Result:=SCE_PAD_ERROR_INVALID_ARG;
 if (data=nil) then Exit;
 //Writeln(SizeOf(TPadData));  //184
 data^:=Default(ScePadData);

 _sig_lock;

 //FillChar(data^,SizeOf(ScePadData),1);
 //FillChar(data^.touchData,SizeOf(ScePadData.touchData),1);

 data^.touchData.touchNum:=1;
 data^.touchData.touch[0].id:=0;

 GetCursorPos(mPoint);
 data^.touchData.touch[0].x:=mPoint.X;
 data^.touchData.touch[0].y:=mPoint.Y;

 data^.connected:=True;
 data^.timestamp:=Sysutils.GetTickCount64;
 data^.connectedCount:=1;

 data^.leftStick.x:=$80;
 data^.leftStick.y:=$80;
 data^.rightStick.x:=$80;
 data^.rightStick.y:=$80;

 //

 if GetAsyncKeyState(VK_W)<>0 then
  data^.leftStick.y:=0;

 if GetAsyncKeyState(VK_S)<>0 then
  data^.leftStick.y:=$FF;

 if GetAsyncKeyState(VK_A)<>0 then
  data^.leftStick.x:=0;

 if GetAsyncKeyState(VK_D)<>0 then
  data^.leftStick.x:=$FF;

 //

 if GetAsyncKeyState(VK_I)<>0 then
  data^.rightStick.y:=0;

 if GetAsyncKeyState(VK_K)<>0 then
  data^.rightStick.y:=$FF;

 if GetAsyncKeyState(VK_J)<>0 then
  data^.rightStick.x:=0;

 if GetAsyncKeyState(VK_L)<>0 then
  data^.rightStick.x:=$FF;

 //

 if GetAsyncKeyState(VK_LBUTTON)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_TOUCH_PAD;

 if GetAsyncKeyState(VK_RETURN)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_OPTIONS;

 if GetAsyncKeyState(VK_UP)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_UP;

 if GetAsyncKeyState(VK_RIGHT)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_RIGHT;

 if GetAsyncKeyState(VK_DOWN)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_DOWN;

 if GetAsyncKeyState(VK_LEFT)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_LEFT;

 if GetAsyncKeyState(VK_NUMPAD1)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_TRIANGLE;

 if GetAsyncKeyState(VK_NUMPAD2)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_CIRCLE;

 if GetAsyncKeyState(VK_NUMPAD4)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_CROSS;

 if GetAsyncKeyState(VK_NUMPAD5)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_SQUARE;

 if GetAsyncKeyState(VK_Q)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L1;

 if GetAsyncKeyState(VK_E)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L2;

 if GetAsyncKeyState(VK_NUMPAD6)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L3;


 if GetAsyncKeyState(VK_U)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R1;

 if GetAsyncKeyState(VK_O)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R2;

 if GetAsyncKeyState(VK_NUMPAD3)<>0 then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R3;


 _sig_unlock;
 Result:=0;
end;

function ps4_scePadRead(handle:Integer;data:PScePadData;num:Integer):Integer; SysV_ABI_CDecl;
begin
 ps4_scePadReadState(handle,data);
 Result:=1;
end;

function ps4_scePadSetVibration(handle:Integer;pParam:PScePadVibrationParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_scePadResetLightBar(handle:Integer):Integer; assembler; nostackframe;
asm
 xor %rax,%rax
end;

type
 ScePadTouchPadInformation=packed record
  pixelDensity:Single;
  resolution:packed record
   x,y:Word;
  end;
 end;

 ScePadStickInformation=packed record
  deadZoneLeft:Byte;
  deadZoneRight:Byte;
 end;

 PScePadControllerInformation=^ScePadControllerInformation;
 ScePadControllerInformation=packed record
  touchPadInfo:ScePadTouchPadInformation;
  stickInfo:ScePadStickInformation;
  connectionType:Byte;
  connectedCount:Byte;
  connected:Boolean;
  deviceClass:DWORD;
  reserve:array[0..7] of Byte;
 end;

const
 SCE_PAD_CONNECTION_TYPE_LOCAL=0;
 SCE_PAD_CONNECTION_TYPE_REMOTE=1;
 SCE_PAD_CONNECTION_TYPE_REMOTE_VITA=SCE_PAD_CONNECTION_TYPE_REMOTE;
 SCE_PAD_CONNECTION_TYPE_REMOTE_DUALSHOCK4=2;

 SCE_PAD_DEVICE_CLASS_INVALID        = -1;
 SCE_PAD_DEVICE_CLASS_STANDARD       = 0;
 SCE_PAD_DEVICE_CLASS_GUITAR         = 1;
 SCE_PAD_DEVICE_CLASS_DRUM           = 2;
 SCE_PAD_DEVICE_CLASS_DJ_TURNTABLE   = 3;
 SCE_PAD_DEVICE_CLASS_DANCEMAT       = 4;
 SCE_PAD_DEVICE_CLASS_NAVIGATION     = 5;
 SCE_PAD_DEVICE_CLASS_STEERING_WHEEL = 6;
 SCE_PAD_DEVICE_CLASS_STICK          = 7;
 SCE_PAD_DEVICE_CLASS_FLIGHT_STICK   = 8;
 SCE_PAD_DEVICE_CLASS_GUN            = 9;


function ps4_scePadGetControllerInformation(handle:Integer;pInfo:PScePadControllerInformation):Integer; SysV_ABI_CDecl;
begin
 //FillChar(pInfo^,SizeOf(ScePadControllerInformation),1);
 //Exit(0);
 if (pInfo=nil) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 pInfo^:=Default(ScePadControllerInformation);
 pInfo^.touchPadInfo.pixelDensity := 1;
 pInfo^.touchPadInfo.resolution.x := 256;
 pInfo^.touchPadInfo.resolution.y := 256;
 pInfo^.stickInfo.deadZoneLeft    := 2;
 pInfo^.stickInfo.deadZoneRight   := 2;
 pInfo^.connectionType := SCE_PAD_CONNECTION_TYPE_LOCAL;
 pInfo^.connectedCount := 1;
 pInfo^.connected      := True;
 pInfo^.deviceClass    := SCE_PAD_DEVICE_CLASS_STANDARD;
 Result:=0;
end;

function ps4_scePadSetMotionSensorState(handle:Integer;bEnable:Boolean):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_scePadSetLightBar(handle:Integer;pParam:PScePadLightBarParam):Integer; SysV_ABI_CDecl;
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
 lib^.set_proc($6277605EA41557B7,@ps4_scePadReadState);
 lib^.set_proc($AB570735F1B270B2,@ps4_scePadRead);
 lib^.set_proc($C8556739D1B1BD96,@ps4_scePadSetVibration);
 lib^.set_proc($0EC703D62F475F5C,@ps4_scePadResetLightBar);
 lib^.set_proc($8233FDFCA433A149,@ps4_scePadGetControllerInformation);
 lib^.set_proc($72556F2F86439EDC,@ps4_scePadSetMotionSensorState);
 lib^.set_proc($451E27A2F50410D6,@ps4_scePadSetLightBar);
end;

initialization
 ps4_app.RegistredPreLoad('libScePad.prx',@Load_libScePad);

end.

