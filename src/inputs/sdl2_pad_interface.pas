unit sdl2_pad_interface;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 sdl2,
 sce_pad_types,
 sce_pad_interface;

type
 TSdl2PadHandle=class(TScePadHandle)
  index:Integer;
  connectedCount:Integer;
  game_controller:PSDL_GameController;
  function   ReadState(data:PScePadData):Integer; override;
  destructor Destroy; override;
 end;

 TSdl2PadInterface=class(TScePadInterface)
  class var
   sdl2_init:Boolean;
   sdl2_open:array[0..15] of TSdl2PadHandle;
  class procedure pre_init;
  class function  Init:Integer; override;
  class function  Done:Integer; override;
  class function  Open(index:Integer;var handle:TScePadHandle):Integer; override;
  class function  GetHandle(index:Integer):Integer; override;
  class function  FindOpened(device_index:Integer;prev:PSDL_GameController):Boolean;
  class function  FindDevice(prev:PSDL_GameController):PSDL_GameController;
 end;

implementation

class procedure TSdl2PadInterface.pre_init;
var
 i:Integer;
begin
 i:=SDL_InitSubSystem(SDL_INIT_JOYSTICK or SDL_INIT_GAMECONTROLLER);
 if (i<>0) then Exit;
 Writeln('SDL2 Game-Controller subsystem initialized!');
 sdl2_init:=True;
end;

class function TSdl2PadInterface.Init:Integer;
begin
 if (sdl2_init) then
 begin
  Result:=0;
 end else
 begin
  Result:=SCE_PAD_ERROR_NOT_INITIALIZED;
 end;
end;

class function TSdl2PadInterface.Done:Integer;
begin
 SDL_QuitSubSystem(SDL_INIT_JOYSTICK or SDL_INIT_GAMECONTROLLER);
 Writeln('SDL2 Game-Controller subsystem exited!');
 Result:=0;
end;

function Compare(g1,g2:TSDL_JoystickGUID):Boolean; inline;
begin
 Result:=CompareByte(g1,g2,SizeOf(TSDL_JoystickGUID))=0;
end;

function Compare(g1,g2:PSDL_GameController):Boolean;
begin
 Result:=Compare(SDL_JoystickGetGUID(SDL_GameControllerGetJoystick(g1)),
                 SDL_JoystickGetGUID(SDL_GameControllerGetJoystick(g2))
                );
end;

function Compare(device_index:Integer;g2:PSDL_GameController):Boolean;
begin
 Result:=Compare(SDL_JoystickGetDeviceGUID(device_index),
                 SDL_JoystickGetGUID(SDL_GameControllerGetJoystick(g2))
                );
end;

class function TSdl2PadInterface.FindOpened(device_index:Integer;prev:PSDL_GameController):Boolean;
var
 i:Integer;
begin
 Result:=False;
 For i:=0 to 15 do
  if (sdl2_open[i]<>nil) then
  if (sdl2_open[i].game_controller<>nil) then
  if (sdl2_open[i].game_controller<>prev) then
  begin
   Result:=Compare(device_index,sdl2_open[i].game_controller);
   if Result then Break;
  end;
end;

class function TSdl2PadInterface.FindDevice(prev:PSDL_GameController):PSDL_GameController;
var
 i,c:Integer;
 first,compared:Integer;
begin
 Result:=nil;
 first:=-1;
 compared:=-1;
 SDL_LockJoysticks;
  c:=SDL_NumJoysticks();
  if (c<>0) then
  For i:=0 to c-1 do
   if SDL_IsGameController(i) then
   if not FindOpened(i,prev) then
   begin
    if (first=-1) then first:=i;
    if (prev=nil) then
    begin
     compared:=i;
     Break;
    end else
    if Compare(i,prev) then
    begin
     compared:=i;
     Break;
    end;
   end;
  //
  if (compared=-1) then compared:=first;
  if (compared<>-1) then
  begin
   Result:=SDL_GameControllerOpen(compared);
  end;
 SDL_UnlockJoysticks;
end;

class function TSdl2PadInterface.Open(index:Integer;var handle:TScePadHandle):Integer;
var
 game_controller:PSDL_GameController;
begin
 Result:=0;
 if not sdl2_init then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);
 if (index<0) or (index>15) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 if (sdl2_open[index]<>nil) then Exit(SCE_PAD_ERROR_ALREADY_OPENED);

 game_controller:=FindDevice(nil);

 handle:=TSdl2PadHandle.Create;
 TSdl2PadHandle(handle).index:=index;
 TSdl2PadHandle(handle).game_controller:=game_controller;

 sdl2_open[index]:=TSdl2PadHandle(handle);

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
 Writeln('----------------------------------------------------------------------------------------');
 Writeln('----------------------------------------------------------------------------------------');


 Writeln('Controller GUID: ', GUIDToString(TGUID(SDL_JoystickGetGUID(SDL_GameControllerGetJoystick(game_controller)))));
 Writeln('----------------------------------------------------------------------------------------');
 Writeln('----------------------------------------------------------------------------------------');


 Writeln('Modifiable LED: ', SDL_GameControllerHasLED(game_controller));
 Writeln('----------------------------------------------------------------------------------------');
 Writeln('----------------------------------------------------------------------------------------');
 Writeln('Modifiable Rumble: ', SDL_GameControllerHasRumble(game_controller));
 Writeln('----------------------------------------------------------------------------------------');
 Writeln('----------------------------------------------------------------------------------------');
end;

class function TSdl2PadInterface.GetHandle(index:Integer):Integer;
begin
 if not sdl2_init then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);
 if (index<0) or (index>15) then Exit(SCE_PAD_ERROR_INVALID_ARG);

 if (sdl2_open[index]=nil) then Exit(SCE_PAD_ERROR_NO_HANDLE);

 Result:=sdl2_open[index].handle;
end;

function AxisToAnalog(i:Integer):Byte;
begin
 if (i>0) then
 begin
  i:=(i*127) div 32767;
 end else
 begin
  i:=(i*128) div 32768;
 end;
 Result:=Byte(i+128);
end;

function AxisToTrigger(i:Integer):Byte;
begin
 Result:=Byte((abs(i)*255) div 32767);
end;

function TSdl2PadHandle.ReadState(data:PScePadData):Integer;
var
 new:PSDL_GameController;
begin
 Result:=0;

 if not SDL_GameControllerGetAttached(game_controller) then
 begin
  new:=TSdl2PadInterface.FindDevice(game_controller);
  if (new=nil) then
  begin
   data^.connected:=False;
   data^.connectedCount:=Byte(connectedCount);
   Exit(0);
  end else
  begin
   Writeln('Reconnect:',index);
   SDL_GameControllerClose(game_controller);
   game_controller:=new;
   Inc(connectedCount);
  end;
 end;

 data^.connected:=True;
 data^.connectedCount:=Byte(connectedCount);

 //Options and Touchpad
 if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_START) = 1 then
   data^.buttons:=data^.buttons or SCE_PAD_BUTTON_OPTIONS;

 if SDL_GameControllerGetButton(game_controller, SDL_CONTROLLER_BUTTON_TOUCHPAD) = 1 then
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
 data^.leftStick.x:=AxisToAnalog(SDL_GameControllerGetAxis(game_controller, SDL_CONTROLLER_AXIS_LEFTX));
 data^.leftStick.y:=AxisToAnalog(SDL_GameControllerGetAxis(game_controller, SDL_CONTROLLER_AXIS_LEFTY));

 data^.rightStick.x:=AxisToAnalog(SDL_GameControllerGetAxis(game_controller, SDL_CONTROLLER_AXIS_RIGHTX));
 data^.rightStick.y:=AxisToAnalog(SDL_GameControllerGetAxis(game_controller, SDL_CONTROLLER_AXIS_RIGHTY));

 //Left and Right Triggers (L2 and R2)
 data^.analogButtons.l2:=AxisToTrigger(SDL_GameControllerGetAxis(game_controller, SDL_CONTROLLER_AXIS_TRIGGERLEFT ));
 data^.analogButtons.r2:=AxisToTrigger(SDL_GameControllerGetAxis(game_controller, SDL_CONTROLLER_AXIS_TRIGGERRIGHT));

 //Approx L2
 if (data^.analogButtons.l2>1) then
 begin
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L2;
 end;

 //Approx R2
 if (data^.analogButtons.r2>1) then
 begin
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R2;
 end;
end;

destructor TSdl2PadHandle.Destroy;
begin
 TSdl2PadInterface.sdl2_open[index]:=nil;
 inherited;
end;

initialization
 //sdl2 needs to be initialized on the main thread, otherwise it just doesn't work. :(
 TSdl2PadInterface.pre_init;

finalization
 TSdl2PadInterface.Done;

end.

