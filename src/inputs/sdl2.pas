unit sdl2;

{
 Clipped headers from "SDL2-for-Pascal"
}

interface

  {$IFDEF WINDOWS}
    uses
      dynlibs,
      {$IFDEF FPC}
      ctypes,
      {$ENDIF}
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

  {$IF DEFINED(UNIX) AND DEFINED(ANDROID) AND DEFINED(FPC)}
    uses
      ctypes,
      UnixType;
  {$ENDIF}

const

  {$IFDEF WINDOWS}
    SDL_LibName = 'SDL2.dll';
  {$ENDIF}

  {$IFDEF UNIX}
    {$IFDEF DARWIN}
      SDL_LibName = 'libSDL2.dylib';
      {$IFDEF FPC}
        {$LINKLIB libSDL2}
      {$ENDIF}
    {$ELSE}
      {$IFDEF FPC}
        SDL_LibName = 'libSDL2.so';
      {$ELSE}
        SDL_LibName = 'libSDL2.so.0';
      {$ENDIF}
      {$MESSAGE HINT 'Known MESA bug may generate float-point exception in software graphics mode! See https://github.com/PascalGameDevelopment/SDL2-for-Pascal/issues/56 for reference.'}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MACOS}
    SDL_LibName = 'SDL2';
    {$IFDEF FPC}
      {$linklib libSDL2}
    {$ENDIF}
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

////

function SDL_InitSubSystem(flags: TSDL_Init): cint; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_InitSubSystem' {$ENDIF} {$ENDIF};


procedure SDL_QuitSubSystem(flags: TSDL_Init); cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_QuitSubSystem' {$ENDIF} {$ENDIF};

//////

procedure SDL_LockJoysticks(); cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LockJoysticks' {$ENDIF} {$ENDIF};

procedure SDL_UnlockJoysticks(); cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_UnlockJoysticks' {$ENDIF} {$ENDIF};

function SDL_NumJoysticks(): cint; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_NumJoysticks' {$ENDIF} {$ENDIF};

////

function SDL_GameControllerOpen(joystick_index: cint): PSDL_GameController cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerOpen' {$ENDIF} {$ENDIF};

procedure SDL_GameControllerClose(gamecontroller: PSDL_GameController) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerClose' {$ENDIF} {$ENDIF};

//

function SDL_GameControllerNumMappings():cint; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerNumMappings' {$ENDIF} {$ENDIF};

function SDL_GameControllerName(gamecontroller: PSDL_GameController): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerName' {$ENDIF} {$ENDIF};

function SDL_GameControllerGetVendor(gamecontroller: PSDL_GameController): cuint16; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetVendor' {$ENDIF} {$ENDIF};

function SDL_GameControllerGetProduct(gamecontroller: PSDL_GameController): cuint16; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetProduct' {$ENDIF} {$ENDIF};

function SDL_GameControllerGetProductVersion(gamecontroller: PSDL_GameController): cuint16; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetProductVersion' {$ENDIF} {$ENDIF};

function SDL_GameControllerGetFirmwareVersion(gamecontroller: PSDL_GameController): cuint16; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetFirmwareVersion' {$ENDIF} {$ENDIF};

function SDL_GameControllerGetSerial(gamecontroller: PSDL_GameController): PAnsiChar; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetSerial' {$ENDIF} {$ENDIF};

function SDL_GameControllerPath(gamecontroller: PSDL_GameController): PAnsiChar; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerPath' {$ENDIF} {$ENDIF};

function SDL_GameControllerHasRumble(gamecontroller: PSDL_GameController): Boolean; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerHasRumble' {$ENDIF} {$ENDIF};

function SDL_GameControllerGetButton(gamecontroller: PSDL_GameController; button: TSDL_GameControllerButton): cuint8 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetButton' {$ENDIF} {$ENDIF};


function SDL_GameControllerGetNumTouchpads(gamecontroller: PSDL_GameController): cint; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetNumTouchpads' {$ENDIF} {$ENDIF};

function SDL_GameControllerGetNumTouchpadFingers(gamecontroller: PSDL_GameController; touchpad: cint): cint; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetNumTouchpadFingers' {$ENDIF} {$ENDIF};

function SDL_GameControllerGetTouchpadFinger(
  gamecontroller: PSDL_GameController;
  touchpad, finger: cint;
  state: pcuint8;
  x, y, pressure: pcfloat
): cint; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetTouchpadFinger' {$ENDIF} {$ENDIF};

function SDL_GameControllerGetAxis(gamecontroller: PSDL_GameController; axis: TSDL_GameControllerAxis): cint16 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerGetAxis' {$ENDIF} {$ENDIF};

function SDL_GameControllerRumble(
  gamecontroller: PSDL_GameController;
  low_frequency_rumble, high_frequency_rumble: cuint16;
  duration_ms: cuint32
): cint; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerRumble' {$ENDIF} {$ENDIF};

function SDL_GameControllerHasRumbleTriggers(gamecontroller: PSDL_GameController): Boolean; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerHasRumbleTriggers' {$ENDIF} {$ENDIF};

function SDL_GameControllerRumbleTriggers(
  gamecontroller: PSDL_GameController;
  left_rumble, right_rumble: cuint16;
  duration_ms: cuint32
): cint; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerRumbleTriggers' {$ENDIF} {$ENDIF};



function SDL_GameControllerHasLED(gamecontroller: PSDL_GameController): Boolean; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerHasLED' {$ENDIF} {$ENDIF};

function SDL_GameControllerSetLED(gamecontroller: PSDL_GameController; red, green, blue: cuint8): cint; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GameControllerSetLED' {$ENDIF} {$ENDIF};


implementation


end.

