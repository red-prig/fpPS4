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
  formController,
  SDL2;

implementation

uses
 sce_pad_types,
 ps4_libSceVideoOut,
 uMappableInputs,
 sys_kernel;

var
 last_mouse_lock:Pointer;
 last_mouse_point:TPoint;
 last_mouse_init:Integer=0;
 xinput_last_poll:Long=0;
 xinput_controllers_connected:Array[0..XUSER_MAX_COUNT-1] of Boolean;
 game_controller:PSDL_GameController;

function ps4_scePadInit():Integer; SysV_ABI_CDecl;
var
 controllerIndex:Integer;
begin
 Writeln(SysLogPrefix,'scePadInit');


 //init SDL2 Joystick
 if SDL_Numjoysticks() < 1 then
 begin
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('SDL2 Error: No Joysticks Connected!');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
 end else
 begin
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('Number of Joysticks connected: ', SDL_Numjoysticks());

  game_controller:=SDL_GameControllerOpen(0);
  //Manipulate the below RGB values to change LED color
  SDL_GameControllerSetLED(game_controller, 255, 0, 120);

  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('SDL2: Game Controller loaded!');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('Controller Name: ', SDL_GameControllerName(game_controller));
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('USB Vendor ID: ', SDL_GameControllerGetVendor(game_controller));
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('USB Product ID: ', SDL_GameControllerGetProduct(game_controller));
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('Product Version: ', SDL_GameControllerGetProductVersion(game_controller));
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('Firmware Version: ', SDL_GameControllerGetFirmwareVersion(game_controller));
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('Serial Number: ', SDL_GameControllerGetSerial(game_controller));
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('Controller Path: ', SDL_GameControllerPath(game_controller));
  //Writeln('----------------------------------------------------------------------------------------');
  //Writeln('----------------------------------------------------------------------------------------');
  //Writeln('Player Index: ', SDL_GameControllerGetPlayerIndex(game_controller));
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('Modifiable LED: ', SDL_GameControllerHasLED(game_controller));
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('Modifiable Rumble: ', SDL_GameControllerHasRumble(game_controller));
  Writeln('----------------------------------------------------------------------------------------');
  Writeln('----------------------------------------------------------------------------------------');
 end;

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
 //event:TSDL_Event;

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

 //for controllerIndex := 0 to SDL_Numjoysticks() - 1 do
 // while SDL_PollEvent(@event) <> 0 do
 // begin
 //  if event.type_ = SDL_CONTROLLERBUTTONDOWN then
 //  Writeln(event.cbutton.button,' pressed');
 //  if event.type_ = SDL_CONTROLLERAXISMOTION then
 //  Writeln(event.caxis.axis,' moved');
 // end;

 begin
  //Options and Touchpad
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_BACK) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_OPTIONS;
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_START) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_TOUCH_PAD;

  //Hats(D-PAD)
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_DPAD_UP) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_UP;
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_DPAD_DOWN) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_DOWN;
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_DPAD_LEFT) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_LEFT;
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_DPAD_RIGHT) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_RIGHT;

  //Cross, Circle, Square and Triangle
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_A) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_CROSS;
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_B) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_CIRCLE;
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_X) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_SQUARE;
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_Y) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_TRIANGLE;

  //L1, R1 and L3, R3
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_LEFTSHOULDER) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L1;
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_RIGHTSHOULDER) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R1;

  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_LEFTSTICK) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L3;
  if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_RIGHTSTICK) = 1 then
    data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R3;

  //Left and Right Axes
  data^.leftStick.x:=Trunc(SDL_GameControllerGetAxis(game_controller, SDL_CONTROLLER_AXIS_LEFTX));
  data^.leftStick.y:=Trunc(SDL_GameControllerGetAxis(game_controller, SDL_CONTROLLER_AXIS_LEFTY));

  data^.rightStick.x:=Trunc(SDL_GameControllerGetAxis(game_controller, SDL_CONTROLLER_AXIS_RIGHTX));
  data^.rightStick.y:=Trunc(SDL_GameControllerGetAxis(game_controller, SDL_CONTROLLER_AXIS_RIGHTY));

  //Left and Right Triggers (L2 and R2)
  //SDL has no support to define L2 and R2 as buttons, so keep that in mind for future
  data^.analogButtons.l2:=Trunc(SDL_GameControllerGetAxis(game_controller, SDL_CONTROLLER_AXIS_TRIGGERLEFT)*255);
  data^.analogButtons.r2:=Trunc(SDL_GameControllerGetAxis(game_controller, SDL_CONTROLLER_AXIS_TRIGGERRIGHT)*255);
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

function ps4_scePadSetAngularVelocityDeadbandState(handle:Integer;bEnable:BOOL):Integer; SysV_ABI_CDecl;
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
 lib^.set_proc($AF8E260317521BE5,@ps4_scePadSetAngularVelocityDeadbandState);
end;

initialization
 ps4_app.RegistredPreLoad('libScePad.prx',@Load_libScePad);

end.

