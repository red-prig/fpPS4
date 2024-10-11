unit sdl2_pad_interface;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 spinlock,
 sdl2,
 sce_pad_types,
 sce_pad_interface,
 kbm_pad_interface;

type
 TSdl2PadHandle=class(TScePadHandle)
  var
   connectedCount:Integer;
   game_controller:PSDL_GameController;
   LED:ScePadLightBarParam;
  function   SetLightBar(data:pScePadLightBarParam):Integer; override;
  function   ResetLightBar():Integer;                        override;
  function   ReadState(data:PScePadData):Integer;            override;
 end;

 TSdl2PadInterface=class(TScePadInterface)
  class var
   sdl2_init:Boolean;
  class function  Load:Boolean; override;
  class procedure Unload;       override;
  class function  Init:Integer; override;
  class function  Done:Integer; override;
  class function  Open(var handle:TScePadHandle):Integer; override;
  class function  FindOpened(device_index:Integer;prev:PSDL_GameController):Boolean;
  class function  FindDevice(prev:PSDL_GameController):PSDL_GameController;
 end;

implementation

class function TSdl2PadInterface.Load:Boolean;
var
 i:Integer;
begin
 if sdl2_init then Exit(True);

 i:=SDL_InitSubSystem(SDL_INIT_JOYSTICK or SDL_INIT_GAMECONTROLLER);
 sdl2_init:=(i=0);

 if sdl2_init then
 begin
  Writeln('SDL2 Game-Controller subsystem initialized!');
 end else
 begin
  Writeln('SDL2 Game-Controller not initialized!');
 end;

 Result:=sdl2_init;
end;

class procedure TSdl2PadInterface.Unload;
begin
 if not sdl2_init then Exit;
 SDL_QuitSubSystem(SDL_INIT_JOYSTICK or SDL_INIT_GAMECONTROLLER);
 sdl2_init:=False;
 Writeln('SDL2 Game-Controller subsystem quit!');
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
 h:TSdl2PadHandle;
begin
 Result:=False;
 spin_lock(pad_lock);
 For i:=0 to 15 do
  if (pad_opened[i]<>nil) then
  if (pad_opened[i].InheritsFrom(TSdl2PadHandle)) then
  begin
   h:=TSdl2PadHandle(pad_opened[i]);
   if (h.game_controller<>nil) then
   if (h.game_controller<>prev) then
   begin
    Result:=Compare(device_index,h.game_controller);
    if Result then Break;
   end;
  end;
 spin_unlock(pad_lock);
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

class function TSdl2PadInterface.Open(var handle:TScePadHandle):Integer;
var
 game_controller:PSDL_GameController;
begin
 Result:=0;
 if not sdl2_init then Exit(SCE_PAD_ERROR_NOT_INITIALIZED);

 game_controller:=FindDevice(nil);

 handle:=TSdl2PadHandle.Create;
 TSdl2PadHandle(handle).game_controller:=game_controller;

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

function TSdl2PadHandle.SetLightBar(data:pScePadLightBarParam):Integer;
begin
 LED:=data^;
 if (game_controller<>nil) then
 begin
  SDL_GameControllerSetLED(game_controller, LED.r, LED.g, LED.b);
 end;
end;

function TSdl2PadHandle.ResetLightBar():Integer;
begin
 LED:=Default(ScePadLightBarParam);
 if (game_controller<>nil) then
 begin
  SDL_GameControllerSetLED(game_controller, LED.r, LED.g, LED.b);
 end;
end;

function TSdl2PadHandle.ReadState(data:PScePadData):Integer;
var
 i,f,t,n:Integer;
 x,y,pressure:Single;
 state:Byte;
 new:PSDL_GameController;
 event:TSDL_Event;
begin
 Result:=0;

 //loop events
 while (SDL_PollEvent(@event)<>0) do;

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
   //
   SDL_GameControllerSetLED(game_controller, LED.r, LED.g, LED.b);
  end;
 end;

 data^.connected:=True;
 data^.connectedCount:=Byte(connectedCount);

 t:=SDL_GameControllerGetNumTouchpads(game_controller);

 if (t=0) then
 begin
  //no touchpad? use mouse
  TMouseAsTouchpad.ReadState(data);
 end else
 begin
  //use ony first touchpad
  f:=SDL_GameControllerGetNumTouchpadFingers(game_controller,0);

  n:=0;
  data^.touchData.touchNum:=0;

  if (f<>0) then
   For i:=0 to f-1 do
   begin

    if SDL_GameControllerGetTouchpadFinger(
        game_controller,
        0,i,
        @state,
        @x,@y,@pressure)=0 then
    begin

     if (x>1919) then x:=1919;
     if (y>941)  then y:=941;

     data^.touchData.touch[n].id:=n;
     data^.touchData.touch[n].x :=Word(Trunc(x));
     data^.touchData.touch[n].y :=Word(Trunc(y));

     Inc(n);
     data^.touchData.touchNum:=n;

     if (n=SCE_PAD_MAX_TOUCH_NUM) then Break;
    end;
   end;
 end;


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

finalization
 TSdl2PadInterface.Unload;

end.

