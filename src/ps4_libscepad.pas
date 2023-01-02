unit ps4_libScePad;

{$mode objfpc}{$H+}

interface

uses
  windows,
  ps4_program,
  spinlock,
  sys_signal,
  Classes,
  SysUtils,
  xinput,
  formController;

implementation

uses
 ps4_libSceVideoOut, uMappableInputs, sys_kernel;

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

 SCE_PAD_CONNECTION_TYPE_LOCAL=0;
 SCE_PAD_CONNECTION_TYPE_REMOTE=1;
 SCE_PAD_CONNECTION_TYPE_REMOTE_VITA=SCE_PAD_CONNECTION_TYPE_REMOTE;
 SCE_PAD_CONNECTION_TYPE_REMOTE_DUALSHOCK4=2;

 //ScePadDeviceClass
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
  _align:array[0..2] of Byte;
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
  _align:array[0..2] of Byte;
  deviceClass:DWORD; //ScePadDeviceClass
  reserve:array[0..7] of Byte;
 end;

 pScePadDeviceClassExtendedInformation=^ScePadDeviceClassExtendedInformation;
 ScePadDeviceClassExtendedInformation=packed record
  deviceClass:DWORD; //ScePadDeviceClass
  reserved:DWORD;
  classData:packed record
   Case Byte of

    0:(steeringWheel:packed record
        capability:Byte;
        reserved1:Byte;
        maxPhysicalWheelAngle:Word;
        reserved2:QWORD;
       end);

    1:(guitar:packed record
        capability:Byte;
        quantityOfSelectorSwitch:Byte;
        reserved1:Word;
        reserved2:QWORD;
       end);

    2:(drum:packed record
        capability:Byte;
        reserved1:Byte;
        reserved2:Word;
        reserved3:QWORD;
       end);

    3:(flightStick:packed record
        capability:Byte;
        reserved1:Byte;
        reserved2:Word;
        reserved3:QWORD;
       end);

    4:(data:array[0..11] of Byte);

  end;
 end;

 pScePadDeviceClassData=^ScePadDeviceClassData;
 ScePadDeviceClassData=packed record
  deviceClass:DWORD; //ScePadDeviceClass
  bDataValid :Boolean;
  _align:array[0..2] of Byte;
  classData:packed record
   Case Byte of

    0:(steeringWheel:packed record
        steeringWheelAngle:Single;
        steeringWheel     :Word;
        acceleratorPedal  :Word;
        brakePedal        :Word;
        clutchPedal       :Word;
        handBlake         :Word;
        gear              :Byte;
        reserved          :Byte;
       end); //SCE_PAD_DEVICE_CLASS_STEERING_WHEEL

    1:(guitar:packed record
        toneNumber:Byte;
        whammyBar :Byte;
        tilt      :Byte;
        fret      :Byte;
        fretSolo  :Byte;
        reserved:array[0..10] of Byte;
       end); //SCE_PAD_DEVICE_CLASS_GUITAR

    2:(drum:packed record
        snare      :Byte;
        tom1       :Byte;
        tom2       :Byte;
        floorTom   :Byte;
        hihatCymbal:Byte;
        rideCymbal :Byte;
        crashCymbal:Byte;
        reserved:array[0..8] of Byte;
       end); //SCE_PAD_DEVICE_CLASS_DRUM

    3:(flightStick:packed record
        stickAxisX     :Word;
        stickAxisY     :Word;
        stickTwist     :Byte;
        throttle       :Byte;
        trigger        :Byte;
        rudderPedal    :Byte;
        brakePedalLeft :Byte;
        brakePedalRight:Byte;
        antennaKnob    :Byte;
        rangeKnob      :Byte;
        reserved:array[0..3] of Byte;
       end); //SCE_PAD_DEVICE_CLASS_FLIGHT_STICK

    4:(others:packed record
        dataLen:Byte;
        reserved:array[0..2] of Byte;
        data:array[0..SCE_PAD_MAX_DEVICE_UNIQUE_DATA_SIZE-1] of Byte;
       end); //Not Supported device

  end;
 end;

var
 last_mouse_lock:Pointer;
 last_mouse_point:TPoint;
 last_mouse_init:Integer=0;
 xinput_last_poll:Long=0;
 xinput_controllers_connected:Array[0..XUSER_MAX_COUNT-1] of Boolean;

function ps4_scePadInit():Integer; SysV_ABI_CDecl;
var
 controllerIndex:Integer;
begin
 Writeln(SysLogPrefix,'scePadInit');

 // init xinput
 for controllerIndex := 0 to XUSER_MAX_COUNT - 1 do
   xinput_controllers_connected[controllerIndex] := false;

 Result:=0;
end;

function ps4_scePadOpen(userID,_type,index:Integer;param:Pointer):Integer; SysV_ABI_CDecl;
begin
 //Writeln('scePadOpen:',userID);
 Result:=222;
end;

function ps4_scePadGetHandle(userID,_type,index:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=222;
end;

function ps4_scePadClose(handle:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;


function ps4_scePadReadState(handle:Integer;data:PScePadData):Integer; SysV_ABI_CDecl;
var
 mPoint,delta:TPoint;
 cs:TXInputState;
 controllerIndex,stateResult:DWORD;

 function GetAsyncKeyState(vKey:longint):Boolean; inline;
 begin
  Result:=(Windows.GetKeyState(vKey) and $8000)<>0;
 end;

begin
 Result:=SCE_PAD_ERROR_INVALID_ARG;
 if (data=nil) then Exit;
 //Writeln(SizeOf(TPadData));  //184
 data^:=Default(ScePadData);

 _sig_lock;

 //connect data

 data^.timestamp:=Sysutils.GetTickCount64;

 data^.connected:=True;
 data^.connectedCount:=1;

 data^.leftStick.x :=$80;
 data^.leftStick.y :=$80;
 data^.rightStick.x:=$80;
 data^.rightStick.y:=$80;

 //only in active windows

 if (MainWindows<>GetForegroundWindow) then
 begin
  _sig_unlock;
  Result:=0;
  Exit;
 end;

 // xinput - Check connected controllers every couple of seconds
 if GetTickCount64 > xinput_last_poll + 10000 then
 begin
   for controllerIndex := 0 to XUSER_MAX_COUNT - 1 do
     xinput_controllers_connected[controllerIndex] := XInputGetState(controllerIndex, cs) <> ERROR_DEVICE_NOT_CONNECTED;

   xinput_last_poll := GetTickCount64;
 end;

 // xinput for controllers
 for controllerIndex := 0 to XUSER_MAX_COUNT - 1 do
 begin
  if not MappableInputs.XInputEnabled then break;
  if not xinput_controllers_connected[controllerIndex] then
     continue;
  ZeroMemory(@cs, SizeOf(cs));
  stateResult := XInputGetState(controllerIndex, cs);

  if stateResult = ERROR_SUCCESS then
  begin
   if MappableInputs.PS4IsPressed(miCross, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_CROSS;
   if MappableInputs.PS4IsPressed(miCircle, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_CIRCLE;
   if MappableInputs.PS4IsPressed(miSquare, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_SQUARE;
   if MappableInputs.PS4IsPressed(miTriangle, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_TRIANGLE;

   if MappableInputs.PS4IsPressed(miOptions, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_OPTIONS;
   if MappableInputs.PS4IsPressed(miTouchPad, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_TOUCH_PAD;

   if MappableInputs.PS4IsPressed(miL1, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L1;
   if MappableInputs.PS4IsPressed(miR1, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R1;
   if MappableInputs.PS4IsPressed(miL3, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L3;
   if MappableInputs.PS4IsPressed(miR3, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R3;

   if MappableInputs.PS4IsPressed(miDPadUp, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_UP;
   if MappableInputs.PS4IsPressed(miDPadDown, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_DOWN;
   if MappableInputs.PS4IsPressed(miDPadLeft, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_LEFT;  
   if MappableInputs.PS4IsPressed(miDPadRight, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_RIGHT;

   data^.leftStick.x:=Trunc(128+(MappableInputs.GetAnalog(miLJoyRight, cs)-MappableInputs.GetAnalog(miLJoyLeft, cs))*127);
   data^.leftStick.y:=Trunc(128+(MappableInputs.GetAnalog(miLJoyDown, cs)-MappableInputs.GetAnalog(miLJoyUp, cs))*127);

   data^.rightStick.x:=Trunc(128+(MappableInputs.GetAnalog(miRJoyRight, cs)-MappableInputs.GetAnalog(miRJoyLeft, cs))*127);
   data^.rightStick.y:=Trunc(128+(MappableInputs.GetAnalog(miRJoyDown, cs)-MappableInputs.GetAnalog(miRJoyUp, cs))*127);

   data^.analogButtons.l2:=Trunc(MappableInputs.GetAnalog(miL2, cs)*255);
   data^.analogButtons.r2:=Trunc(MappableInputs.GetAnalog(miR2, cs)*255);

   if MappableInputs.PS4IsPressed(miL2, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L2;    
   if MappableInputs.PS4IsPressed(miR2, cs) then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R2;
  end;
 end;

 //mouse as touch pad

 spin_lock(last_mouse_lock);

 GetCursorPos(mPoint);

  if (last_mouse_init=0) then
  begin
   last_mouse_init :=1;
   last_mouse_point:=mPoint;
  end else
  if QWORD(mPoint)<>QWORD(last_mouse_point) then
  begin
   data^.touchData.touchNum:=1;
   data^.touchData.touch[0].id:=0;

   delta:=mPoint;

   if (delta.X<0) then delta.X:=0;
   if (delta.Y<0) then delta.Y:=0;

   if (delta.X>1919) then delta.X:=1919;
   if (delta.Y>941)  then delta.Y:=941;

   data^.touchData.touch[0].x:=delta.X;
   data^.touchData.touch[0].y:=delta.Y;

   last_mouse_point:=mPoint;
  end;

 spin_unlock(last_mouse_lock);

 //keymapping

 if GetAsyncKeyState(VK_W) then
  data^.leftStick.y:=0;

 if GetAsyncKeyState(VK_S) then
  data^.leftStick.y:=$FF;

 if GetAsyncKeyState(VK_A) then
  data^.leftStick.x:=0;

 if GetAsyncKeyState(VK_D) then
  data^.leftStick.x:=$FF;

 //

 if GetAsyncKeyState(VK_I) then
  data^.rightStick.y:=0;

 if GetAsyncKeyState(VK_K) then
  data^.rightStick.y:=$FF;

 if GetAsyncKeyState(VK_J) then
  data^.rightStick.x:=0;

 if GetAsyncKeyState(VK_L) then
  data^.rightStick.x:=$FF;

 //

 if GetAsyncKeyState(VK_LBUTTON) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_TOUCH_PAD;

 if GetAsyncKeyState(VK_RETURN) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_OPTIONS;

 if GetAsyncKeyState(VK_UP) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_UP;

 if GetAsyncKeyState(VK_RIGHT) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_RIGHT;

 if GetAsyncKeyState(VK_DOWN) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_DOWN;

 if GetAsyncKeyState(VK_LEFT) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_LEFT;

 if GetAsyncKeyState(VK_NUMPAD8) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_TRIANGLE;

 if GetAsyncKeyState(VK_NUMPAD6) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_CIRCLE;

 if GetAsyncKeyState(VK_NUMPAD2) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_CROSS;

 if GetAsyncKeyState(VK_NUMPAD4) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_SQUARE;

 if GetAsyncKeyState(VK_Q) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L1;

 if GetAsyncKeyState(VK_1) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L2;

 if GetAsyncKeyState(VK_Z) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L3;


 if GetAsyncKeyState(VK_E) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R1;

 if GetAsyncKeyState(VK_4) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R2;

 if GetAsyncKeyState(VK_C) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R3;

 _sig_unlock;
 Result:=0;
end;

function ps4_scePadRead(handle:Integer;data:PScePadData;num:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 if (num<>0) then
 begin
  ps4_scePadReadState(handle,data);
  Result:=1;
 end;
end;

function ps4_scePadSetVibration(handle:Integer;pParam:PScePadVibrationParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_scePadResetLightBar(handle:Integer):Integer; assembler; nostackframe;
asm
 xor %rax,%rax
end;

function ps4_scePadGetControllerInformation(handle:Integer;
                                            pInfo:PScePadControllerInformation
                                            ):Integer; SysV_ABI_CDecl;
begin
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

function ps4_scePadDeviceClassGetExtendedInformation(handle:Integer;
                                                     pExtInfo:pScePadDeviceClassExtendedInformation
                                                     ):Integer; SysV_ABI_CDecl;
begin
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
 if (pData=nil) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 if (pDeviceClassData=nil) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 pDeviceClassData^:=Default(ScePadDeviceClassData);
 pDeviceClassData^.deviceClass:=SCE_PAD_DEVICE_CLASS_STANDARD;
 pDeviceClassData^.bDataValid:=True;
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

function ps4_scePadResetOrientation(handle:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_scePadSetTiltCorrectionState(handle:Integer;bEnable:BOOL):Integer; SysV_ABI_CDecl;
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
 lib^.set_proc($0EC703D62F475F5C,@ps4_scePadResetLightBar);
 lib^.set_proc($8233FDFCA433A149,@ps4_scePadGetControllerInformation);
 lib^.set_proc($01CB25A4DD631D1F,@ps4_scePadDeviceClassGetExtendedInformation);
 lib^.set_proc($2073EA71B734CC20,@ps4_scePadDeviceClassParseData);
 lib^.set_proc($72556F2F86439EDC,@ps4_scePadSetMotionSensorState);
 lib^.set_proc($451E27A2F50410D6,@ps4_scePadSetLightBar);
 lib^.set_proc($AC866747A792A6F9,@ps4_scePadResetOrientation);
 lib^.set_proc($BC32CCA092DD7BC2,@ps4_scePadSetTiltCorrectionState);
end;

initialization
 ps4_app.RegistredPreLoad('libScePad.prx',@Load_libScePad);

end.

