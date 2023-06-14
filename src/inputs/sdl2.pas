unit sdl2;

{
 Clipped headers from "SDL2-for-Pascal"
}

{$mode objfpc}{$H+}

interface

  {$IFDEF WINDOWS}
    uses
      dynlibs,
      ctypes,
      Windows;
  {$ENDIF}

  {$IF DEFINED(UNIX) AND NOT DEFINED(ANDROID)}
    uses
      {$IFDEF FPC}
      ctypes,
      UnixType,
      {$ENDIF}
      {$IFDEF DARWIN}
      CocoaAll;
      {$ELSE}
      X,
      XLib;
      {$ENDIF}
  {$ENDIF}

const

  {$IFDEF WINDOWS}
    SDL_LibName = 'SDL2.dll';
  {$ENDIF}

  {$IFDEF UNIX}
    {$IFDEF DARWIN}
      SDL_LibName = 'libSDL2.dylib';
    {$ELSE}
      SDL_LibName = 'libSDL2.so';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MACOS}
    SDL_LibName = 'SDL2';
  {$ENDIF}

type
  PPSDL_Init = ^PSDL_Init;
  PSDL_Init = ^TSDL_Init;
  TSDL_Init = type cuint32;

const
  SDL_INIT_JOYSTICK       = TSDL_Init($00000200);   // SDL_INIT_JOYSTICK implies SDL_INIT_EVENTS
  SDL_INIT_GAMECONTROLLER = TSDL_Init($00002000);   //turn on game controller also implicitly does JOYSTICK

type
  PPSDL_GameController = ^PSDL_GameController;
  PSDL_GameController = ^TSDL_GameController;
  TSDL_GameController = record end;

  PPSDL_GameControllerType = ^PSDL_GameControllerType;
  PSDL_GameControllerType = ^TSDL_GameControllerType;
  TSDL_GameControllerType = type cint;

const
  SDL_CONTROLLER_TYPE_UNKNOWN                      = TSDL_GameControllerType(0);
  SDL_CONTROLLER_TYPE_XBOX360                      = TSDL_GameControllerType(1);
  SDL_CONTROLLER_TYPE_XBOXONE                      = TSDL_GameControllerType(2);
  SDL_CONTROLLER_TYPE_PS3                          = TSDL_GameControllerType(3);
  SDL_CONTROLLER_TYPE_PS4                          = TSDL_GameControllerType(4);
  SDL_CONTROLLER_TYPE_NINTENDO_SWITCH_PRO          = TSDL_GameControllerType(5);
  SDL_CONTROLLER_TYPE_VIRTUAL                      = TSDL_GameControllerType(6);
  SDL_CONTROLLER_TYPE_PS5                          = TSDL_GameControllerType(7);
  SDL_CONTROLLER_TYPE_AMAZON_LUNA                  = TSDL_GameControllerType(8);
  SDL_CONTROLLER_TYPE_GOOGLE_STADIA                = TSDL_GameControllerType(9);
  SDL_CONTROLLER_TYPE_NVIDIA_SHIELD                = TSDL_GameControllerType(10);
  SDL_CONTROLLER_TYPE_NINTENDO_SWITCH_JOYCON_LEFT  = TSDL_GameControllerType(11);
  SDL_CONTROLLER_TYPE_NINTENDO_SWITCH_JOYCON_RIGHT = TSDL_GameControllerType(12);
  SDL_CONTROLLER_TYPE_NINTENDO_SWITCH_JOYCON_PAIR  = TSDL_GameControllerType(13);

type
  PPSDL_GameControllerBindType = ^PSDL_GameControllerBindType;
  PSDL_GameControllerBindType = ^TSDL_GameControllerBindType;
  TSDL_GameControllerBindType = type cint;

const
  SDL_CONTROLLER_BINDTYPE_NONE   = TSDL_GameControllerBindType(0);
  SDL_CONTROLLER_BINDTYPE_BUTTON = TSDL_GameControllerBindType(1);
  SDL_CONTROLLER_BINDTYPE_AXIS   = TSDL_GameControllerBindType(2);
  SDL_CONTROLLER_BINDTYPE_HAT    = TSDL_GameControllerBindType(3);

type
  THat = record
    hat: cint;
    hat_mask: cint;
  end;

  PPSDL_GameControllerButtonBind = ^PSDL_GameControllerButtonBind;
  PSDL_GameControllerButtonBind = ^TSDL_GameControllerButtonBind;
  TSDL_GameControllerButtonBind = record
    bindType: TSDL_GameControllerBindType;
    case cint of
      0: ( button: cint; );
      1: ( axis: cint; );
      2: ( hat: THat; );
  end;

type
  PPSDL_Joystick = ^PSDL_Joystick;
  PSDL_Joystick = ^TSDL_Joystick;
  TSDL_Joystick = record end;

  PPSDL_JoystickGUID = ^PSDL_JoystickGUID;
  PSDL_JoystickGUID = ^TSDL_JoystickGUID;
  TSDL_JoystickGUID = type TGUID;

  PPSDL_JoystickID = ^PSDL_JoystickID;
  PSDL_JoystickID = ^TSDL_JoystickID;
  TSDL_JoystickID = type cint32;

type
  PPSDL_JoystickType = ^PSDL_JoystickType;
  PSDL_JoystickType = ^TSDL_JoystickType;
  TSDL_JoystickType = type Integer;

const
  SDL_JOYSTICK_TYPE_UNKNOWN        = TSDL_JoystickType(0);
  SDL_JOYSTICK_TYPE_GAMECONTROLLER = TSDL_JoystickType(1);
  SDL_JOYSTICK_TYPE_WHEEL          = TSDL_JoystickType(2);
  SDL_JOYSTICK_TYPE_ARCADE_STICK   = TSDL_JoystickType(3);
  SDL_JOYSTICK_TYPE_FLIGHT_STICK   = TSDL_JoystickType(4);
  SDL_JOYSTICK_TYPE_DANCE_PAD      = TSDL_JoystickType(5);
  SDL_JOYSTICK_TYPE_GUITAR         = TSDL_JoystickType(6);
  SDL_JOYSTICK_TYPE_DRUM_KIT       = TSDL_JoystickType(7);
  SDL_JOYSTICK_TYPE_ARCADE_PAD     = TSDL_JoystickType(8);
  SDL_JOYSTICK_TYPE_THROTTLE       = TSDL_JoystickType(9);

type
  PPSDL_JoystickPowerLevel = ^PSDL_JoystickPowerLevel;
  PSDL_JoystickPowerLevel = ^TSDL_JoystickPowerLevel;
  TSDL_JoystickPowerLevel = type Integer;

const
  SDL_JOYSTICK_POWER_UNKNOWN = TSDL_JoystickPowerLevel(-1);
  SDL_JOYSTICK_POWER_EMPTY   = TSDL_JoystickPowerLevel(0);  {* <= 5% *}
  SDL_JOYSTICK_POWER_LOW     = TSDL_JoystickPowerLevel(1);  {* <= 20% *}
  SDL_JOYSTICK_POWER_MEDIUM  = TSDL_JoystickPowerLevel(2);  {* <= 70% *}
  SDL_JOYSTICK_POWER_FULL    = TSDL_JoystickPowerLevel(3);  {* <= 100% *}
  SDL_JOYSTICK_POWER_WIRED   = TSDL_JoystickPowerLevel(4);
  SDL_JOYSTICK_POWER_MAX     = TSDL_JoystickPowerLevel(5);

const
  SDL_IPHONE_MAX_GFORCE = 5.0;

type
  PPSDL_GameControllerButton = ^PSDL_GameControllerButton;
  PSDL_GameControllerButton = ^TSDL_GameControllerButton;
  TSDL_GameControllerButton = type cint;

const
  SDL_CONTROLLER_BUTTON_INVALID       = TSDL_GameControllerButton(-1);
  SDL_CONTROLLER_BUTTON_A             = TSDL_GameControllerButton(0);
  SDL_CONTROLLER_BUTTON_B             = TSDL_GameControllerButton(1);
  SDL_CONTROLLER_BUTTON_X             = TSDL_GameControllerButton(2);
  SDL_CONTROLLER_BUTTON_Y             = TSDL_GameControllerButton(3);
  SDL_CONTROLLER_BUTTON_BACK          = TSDL_GameControllerButton(4);
  SDL_CONTROLLER_BUTTON_GUIDE         = TSDL_GameControllerButton(5);
  SDL_CONTROLLER_BUTTON_START         = TSDL_GameControllerButton(6);
  SDL_CONTROLLER_BUTTON_LEFTSTICK     = TSDL_GameControllerButton(7);
  SDL_CONTROLLER_BUTTON_RIGHTSTICK    = TSDL_GameControllerButton(8);
  SDL_CONTROLLER_BUTTON_LEFTSHOULDER  = TSDL_GameControllerButton(9);
  SDL_CONTROLLER_BUTTON_RIGHTSHOULDER = TSDL_GameControllerButton(10);
  SDL_CONTROLLER_BUTTON_DPAD_UP       = TSDL_GameControllerButton(11);
  SDL_CONTROLLER_BUTTON_DPAD_DOWN     = TSDL_GameControllerButton(12);
  SDL_CONTROLLER_BUTTON_DPAD_LEFT     = TSDL_GameControllerButton(13);
  SDL_CONTROLLER_BUTTON_DPAD_RIGHT    = TSDL_GameControllerButton(14);
  SDL_CONTROLLER_BUTTON_MISC1         = TSDL_GameControllerButton(15); {**< Xbox Series X share button, PS5 microphone button, Nintendo Switch Pro capture button, Amazon Luna microphone button *}
  SDL_CONTROLLER_BUTTON_PADDLE1       = TSDL_GameControllerButton(16); {**< Xbox Elite paddle P1 *}
  SDL_CONTROLLER_BUTTON_PADDLE2       = TSDL_GameControllerButton(17); {**< Xbox Elite paddle P3 *}
  SDL_CONTROLLER_BUTTON_PADDLE3       = TSDL_GameControllerButton(18); {**< Xbox Elite paddle P2 *}
  SDL_CONTROLLER_BUTTON_PADDLE4       = TSDL_GameControllerButton(19); {**< Xbox Elite paddle P4 *}
  SDL_CONTROLLER_BUTTON_TOUCHPAD      = TSDL_GameControllerButton(20); {**< PS4/PS5 touchpad button *}
  SDL_CONTROLLER_BUTTON_MAX           = TSDL_GameControllerButton(21);

type
  PPSDL_GameControllerAxis = ^PSDL_GameControllerAxis;
  PSDL_GameControllerAxis = ^TSDL_GameControllerAxis;
  TSDL_GameControllerAxis = type cint;

const
  SDL_CONTROLLER_AXIS_INVALID = TSDL_GameControllerAxis(-1);
  SDL_CONTROLLER_AXIS_LEFTX = TSDL_GameControllerAxis(0);
  SDL_CONTROLLER_AXIS_LEFTY = TSDL_GameControllerAxis(1);
  SDL_CONTROLLER_AXIS_RIGHTX = TSDL_GameControllerAxis(2);
  SDL_CONTROLLER_AXIS_RIGHTY = TSDL_GameControllerAxis(3);
  SDL_CONTROLLER_AXIS_TRIGGERLEFT = TSDL_GameControllerAxis(4);
  SDL_CONTROLLER_AXIS_TRIGGERRIGHT = TSDL_GameControllerAxis(5);
  SDL_CONTROLLER_AXIS_MAX = TSDL_GameControllerAxis(6);

function  SDL_InitSubSystem(flags: TSDL_Init): cint; cdecl;
procedure SDL_QuitSubSystem(flags: TSDL_Init); cdecl;

var
 SDL_LockJoysticks  :procedure(); cdecl;
 SDL_UnlockJoysticks:procedure(); cdecl;
 SDL_NumJoysticks   :function(): cint; cdecl;

////

 SDL_GameControllerOpen:function(joystick_index: cint): PSDL_GameController cdecl;
 SDL_GameControllerClose:procedure(gamecontroller: PSDL_GameController) cdecl;

//

 SDL_GameControllerNumMappings:function():cint; cdecl;
 SDL_GameControllerName:function(gamecontroller: PSDL_GameController): PAnsiChar cdecl;
 SDL_GameControllerGetVendor:function(gamecontroller: PSDL_GameController): cuint16; cdecl;
 SDL_GameControllerGetProduct:function(gamecontroller: PSDL_GameController): cuint16; cdecl;
 SDL_GameControllerGetProductVersion:function(gamecontroller: PSDL_GameController): cuint16; cdecl;
 SDL_GameControllerGetFirmwareVersion:function(gamecontroller: PSDL_GameController): cuint16; cdecl;
 SDL_GameControllerGetSerial:function(gamecontroller: PSDL_GameController): PAnsiChar; cdecl;
 SDL_GameControllerPath:function(gamecontroller: PSDL_GameController): PAnsiChar; cdecl;
 SDL_GameControllerHasRumble:function(gamecontroller: PSDL_GameController): Boolean; cdecl;
 SDL_GameControllerGetButton:function(gamecontroller: PSDL_GameController; button: TSDL_GameControllerButton): cuint8 cdecl;
 SDL_GameControllerGetNumTouchpads:function(gamecontroller: PSDL_GameController): cint; cdecl;
 SDL_GameControllerGetNumTouchpadFingers:function(gamecontroller: PSDL_GameController; touchpad: cint): cint; cdecl;

 SDL_GameControllerGetTouchpadFinger:function(
  gamecontroller: PSDL_GameController;
  touchpad, finger: cint;
  state: pcuint8;
  x, y, pressure: pcfloat
 ): cint; cdecl;

 SDL_GameControllerGetAxis:function(gamecontroller: PSDL_GameController; axis: TSDL_GameControllerAxis): cint16 cdecl;

 SDL_GameControllerRumble:function(
  gamecontroller: PSDL_GameController;
  low_frequency_rumble, high_frequency_rumble: cuint16;
  duration_ms: cuint32
 ): cint; cdecl;

 SDL_GameControllerHasRumbleTriggers:function(gamecontroller: PSDL_GameController): Boolean; cdecl;

 SDL_GameControllerRumbleTriggers:function(
  gamecontroller: PSDL_GameController;
  left_rumble, right_rumble: cuint16;
  duration_ms: cuint32
 ): cint; cdecl;

 SDL_GameControllerHasLED:function(gamecontroller: PSDL_GameController): Boolean; cdecl;
 SDL_GameControllerSetLED:function(gamecontroller: PSDL_GameController; red, green, blue: cuint8): cint; cdecl;

implementation

var
 init_flags:TSDL_Init=0;
 lib_handle:TLibHandle=NilHandle;

 ///
 _SDL_InitSubSystem:function (flags: TSDL_Init): cint; cdecl;
 _SDL_QuitSubSystem:procedure(flags: TSDL_Init); cdecl;
 ///

function SDL_InitSubSystem(flags: TSDL_Init): cint; cdecl;
begin
 if (lib_handle=NilHandle) then
 begin
  lib_handle:=SafeLoadLibrary(SDL_LibName);
  if (lib_handle=NilHandle) then Exit(-1);
 end;

 Pointer(_SDL_InitSubSystem):=GetProcedureAddress(lib_handle,'SDL_InitSubSystem');
 Pointer(_SDL_QuitSubSystem):=GetProcedureAddress(lib_handle,'SDL_QuitSubSystem');

 if (_SDL_InitSubSystem=nil) then
 begin
  UnloadLibrary(lib_handle);
  lib_handle:=NilHandle;
  init_flags:=0;
 end;

 Result:=_SDL_InitSubSystem(flags);

 if (Result=0) then
 begin
  init_flags:=init_flags or flags;
 end;

 if ((flags and SDL_INIT_JOYSTICK)<>0) then
 begin
  Pointer(SDL_LockJoysticks  ):=GetProcedureAddress(lib_handle,'SDL_LockJoysticks');
  Pointer(SDL_UnlockJoysticks):=GetProcedureAddress(lib_handle,'SDL_UnlockJoysticks');
  Pointer(SDL_NumJoysticks   ):=GetProcedureAddress(lib_handle,'SDL_NumJoysticks');
 end;

 if ((flags and SDL_INIT_GAMECONTROLLER)<>0) then
 begin
  Pointer(SDL_GameControllerOpen                 ):=GetProcedureAddress(lib_handle,'SDL_GameControllerOpen');
  Pointer(SDL_GameControllerClose                ):=GetProcedureAddress(lib_handle,'SDL_GameControllerClose');
  Pointer(SDL_GameControllerNumMappings          ):=GetProcedureAddress(lib_handle,'SDL_GameControllerNumMappings');
  Pointer(SDL_GameControllerName                 ):=GetProcedureAddress(lib_handle,'SDL_GameControllerName');
  Pointer(SDL_GameControllerGetVendor            ):=GetProcedureAddress(lib_handle,'SDL_GameControllerGetVendor');
  Pointer(SDL_GameControllerGetProduct           ):=GetProcedureAddress(lib_handle,'SDL_GameControllerGetProduct');
  Pointer(SDL_GameControllerGetProductVersion    ):=GetProcedureAddress(lib_handle,'SDL_GameControllerGetProductVersion');
  Pointer(SDL_GameControllerGetFirmwareVersion   ):=GetProcedureAddress(lib_handle,'SDL_GameControllerGetFirmwareVersion');
  Pointer(SDL_GameControllerGetSerial            ):=GetProcedureAddress(lib_handle,'SDL_GameControllerGetSerial');
  Pointer(SDL_GameControllerPath                 ):=GetProcedureAddress(lib_handle,'SDL_GameControllerPath');
  Pointer(SDL_GameControllerHasRumble            ):=GetProcedureAddress(lib_handle,'SDL_GameControllerHasRumble');
  Pointer(SDL_GameControllerGetButton            ):=GetProcedureAddress(lib_handle,'SDL_GameControllerGetButton');
  Pointer(SDL_GameControllerGetNumTouchpads      ):=GetProcedureAddress(lib_handle,'SDL_GameControllerGetNumTouchpads');
  Pointer(SDL_GameControllerGetNumTouchpadFingers):=GetProcedureAddress(lib_handle,'SDL_GameControllerGetNumTouchpadFingers');
  Pointer(SDL_GameControllerGetTouchpadFinger    ):=GetProcedureAddress(lib_handle,'SDL_GameControllerGetTouchpadFinger');
  Pointer(SDL_GameControllerGetAxis              ):=GetProcedureAddress(lib_handle,'SDL_GameControllerGetAxis');
  Pointer(SDL_GameControllerRumble               ):=GetProcedureAddress(lib_handle,'SDL_GameControllerRumble');
  Pointer(SDL_GameControllerHasRumbleTriggers    ):=GetProcedureAddress(lib_handle,'SDL_GameControllerHasRumbleTriggers');
  Pointer(SDL_GameControllerRumbleTriggers       ):=GetProcedureAddress(lib_handle,'SDL_GameControllerRumbleTriggers');
  Pointer(SDL_GameControllerHasLED               ):=GetProcedureAddress(lib_handle,'SDL_GameControllerHasLED');
  Pointer(SDL_GameControllerSetLED               ):=GetProcedureAddress(lib_handle,'SDL_GameControllerSetLED');
 end;
end;

procedure SDL_QuitSubSystem(flags: TSDL_Init); cdecl;
begin
 init_flags:=init_flags and (not flags);

 if (_SDL_QuitSubSystem<>nil) then
 begin
  _SDL_QuitSubSystem(flags);
 end;

 if (init_flags=0) then
 begin
  UnloadLibrary(lib_handle);
  lib_handle:=NilHandle;
 end;

 if ((flags and SDL_INIT_JOYSTICK)<>0) then
 begin
  Pointer(SDL_LockJoysticks  ):=nil;
  Pointer(SDL_UnlockJoysticks):=nil;
  Pointer(SDL_NumJoysticks   ):=nil;
 end;

 if ((flags and SDL_INIT_GAMECONTROLLER)<>0) then
 begin
  Pointer(SDL_GameControllerOpen                 ):=nil;
  Pointer(SDL_GameControllerClose                ):=nil;
  Pointer(SDL_GameControllerNumMappings          ):=nil;
  Pointer(SDL_GameControllerName                 ):=nil;
  Pointer(SDL_GameControllerGetVendor            ):=nil;
  Pointer(SDL_GameControllerGetProduct           ):=nil;
  Pointer(SDL_GameControllerGetProductVersion    ):=nil;
  Pointer(SDL_GameControllerGetFirmwareVersion   ):=nil;
  Pointer(SDL_GameControllerGetSerial            ):=nil;
  Pointer(SDL_GameControllerPath                 ):=nil;
  Pointer(SDL_GameControllerHasRumble            ):=nil;
  Pointer(SDL_GameControllerGetButton            ):=nil;
  Pointer(SDL_GameControllerGetNumTouchpads      ):=nil;
  Pointer(SDL_GameControllerGetNumTouchpadFingers):=nil;
  Pointer(SDL_GameControllerGetTouchpadFinger    ):=nil;
  Pointer(SDL_GameControllerGetAxis              ):=nil;
  Pointer(SDL_GameControllerRumble               ):=nil;
  Pointer(SDL_GameControllerHasRumbleTriggers    ):=nil;
  Pointer(SDL_GameControllerRumbleTriggers       ):=nil;
  Pointer(SDL_GameControllerHasLED               ):=nil;
  Pointer(SDL_GameControllerSetLED               ):=nil;
 end;
end;



end.

