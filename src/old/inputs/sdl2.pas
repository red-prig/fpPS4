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
  PSDL_ScanCode = ^TSDL_ScanCode;
  TSDL_ScanCode = type cint;

  PSDL_KeyCode = ^TSDL_KeyCode;
  TSDL_KeyCode = type cint32;

  PPSDL_Keysym = ^PSDL_Keysym;
  PSDL_Keysym = ^TSDL_Keysym;
  TSDL_Keysym = record
    scancode: TSDL_ScanCode;       // SDL physical key code - see SDL_Scancode for details
    sym: TSDL_KeyCode;             // SDL virtual key code - see SDL_Keycode for details
    mod_: cuint16;                 // current key modifiers
    unicode: cuint32;              // (deprecated) use SDL_TextInputEvent instead
  end;

  PSDL_TouchID  = ^TSDL_TouchID;
  TSDL_TouchID  = type cint64;

  PSDL_FingerID = ^TSDL_FingerID;
  TSDL_FingerID = type cint64;

  PSDL_GestureID = ^TSDL_GestureID;
  TSDL_GestureID = type cint64;

  PSDL_EventType = ^TSDL_EventType;
  TSDL_EventType = type cuint32;

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

///

const

  { General keyboard/mouse state definitions }
  SDL_RELEASED         = 0;
  SDL_PRESSED          = 1;

  SDL_FIRSTEVENT       = TSDL_EventType(0);     // Unused (do not remove) (needed in pascal?)

  SDL_COMMONEVENT      = TSDL_EventType(1);     //added for pascal-compatibility

  { Application events }
  SDL_QUITEV           = TSDL_EventType($100);  // User-requested quit (originally SDL_QUIT, but changed, cause theres a method called SDL_QUIT)


  { These application events have special meaning on iOS, see README.iOS for details *}

  {* The application is being terminated by the OS. *
   * Called on iOS in applicationWillTerminate()    *
   * Called on Android in onDestroy()               *}
  SDL_APP_TERMINATING  = TSDL_EventType($101);

  {* The application is low on memory, free memory if possible. *
   * Called on iOS in applicationDidReceiveMemoryWarning()      *
   * Called on Android in onLowMemory()                         *}
  SDL_APP_LOWMEMORY    = TSDL_EventType($102);

  {* The application is about to enter the background. *
   * Called on iOS in applicationWillResignActive()    *
   * Called on Android in onPause()                    *}
  SDL_APP_WILLENTERBACKGROUND = TSDL_EventType($103);

  {* The application did enter the background and may not get CPU for some time. *
   * Called on iOS in applicationDidEnterBackground()                            *
   * Called on Android in onPause()                                              *}
  SDL_APP_DIDENTERBACKGROUND = TSDL_EventType($104);

  {* The application is about to enter the foreground. *
   * Called on iOS in applicationWillEnterForeground() *
   * Called on Android in onResume()                   *}
  SDL_APP_WILLENTERFOREGROUND = TSDL_EventType($105);

  {* The application is now interactive.           *
   * Called on iOS in applicationDidBecomeActive() *
   * Called on Android in onResume()               *}
  SDL_APP_DIDENTERFOREGROUND = TSDL_EventType($106);

  {* The user's locale preferences have changed. *}
  SDL_LOCALECHANGED    = TSDL_EventType($107);

  { Display events }
  SDL_DISPLAYEVENT     = TSDL_EventType($150);  // Display state change

  { Window events }
  SDL_WINDOWEVENT      = TSDL_EventType($200);  // Window state change
  SDL_SYSWMEVENT       = TSDL_EventType($201);  // System specific event

  { Keyboard events }
  SDL_KEYDOWN          = TSDL_EventType($300);  // Key pressed
  SDL_KEYUP            = TSDL_EventType($301);  // Key released
  SDL_TEXTEDITING      = TSDL_EventType($302);  // Keyboard text editing (composition)
  SDL_TEXTINPUT        = TSDL_EventType($303);  // Keyboard text input
  SDL_KEYMAPCHANGED    = TSDL_EventType($304);  // Keymap changed due to a system event such as an input language or keyboard layout change.
  SDL_TEXTEDITING_EXT  = TSDL_EventType($305);  // Extended keyboard text editing (composition)

  { Mouse events }
  SDL_MOUSEMOTION      = TSDL_EventType($400);  // Mouse moved
  SDL_MOUSEBUTTONDOWN  = TSDL_EventType($401);  // Mouse button pressed
  SDL_MOUSEBUTTONUP    = TSDL_EventType($402);  // Mouse button released
  SDL_MOUSEWHEEL       = TSDL_EventType($403);  // Mouse wheel motion

  { Joystick events }
  SDL_JOYAXISMOTION     = TSDL_EventType($600);  // Joystick axis motion
  SDL_JOYBALLMOTION     = TSDL_EventType($601);  // Joystick trackball motion
  SDL_JOYHATMOTION      = TSDL_EventType($602);  // Joystick hat position change
  SDL_JOYBUTTONDOWN     = TSDL_EventType($603);  // Joystick button pressed
  SDL_JOYBUTTONUP       = TSDL_EventType($604);  // Joystick button released
  SDL_JOYDEVICEADDED    = TSDL_EventType($605);  // A new joystick has been inserted into the system
  SDL_JOYDEVICEREMOVED  = TSDL_EventType($606);  // An opened joystick has been removed
  SDL_JOYBATTERYUPDATED = TSDL_EventType($607);  // Joystick battery level change

  { Game controller events }
  SDL_CONTROLLERAXISMOTION     = TSDL_EventType($650);  // Game controller axis motion
  SDL_CONTROLLERBUTTONDOWN     = TSDL_EventType($651);  // Game controller button pressed
  SDL_CONTROLLERBUTTONUP       = TSDL_EventType($652);  // Game controller button released
  SDL_CONTROLLERDEVICEADDED    = TSDL_EventType($653);  // A new Game controller has been inserted into the system
  SDL_CONTROLLERDEVICEREMOVED  = TSDL_EventType($654);  // An opened Game controller has been removed
  SDL_CONTROLLERDEVICEREMAPPED = TSDL_EventType($655);  // The controller mapping was updated
  SDL_CONTROLLERTOUCHPADDOWN   = TSDL_EventType($666);  // Game controller touchpad was touched
  SDL_CONTROLLERTOUCHPADMOTION = TSDL_EventType($667);  // Game controller touchpad finger was moved
  SDL_CONTROLLERTOUCHPADUP     = TSDL_EventType($668);  // Game controller touchpad finger was lifted
  SDL_CONTROLLERSENSORUPDATE   = TSDL_EventType($669);  // Game controller sensor was updated

  { Touch events }
  SDL_FINGERDOWN      = TSDL_EventType($700);
  SDL_FINGERUP        = TSDL_EventType($701);
  SDL_FINGERMOTION    = TSDL_EventType($702);

  { Gesture events }
  SDL_DOLLARGESTURE   = TSDL_EventType($800);
  SDL_DOLLARRECORD    = TSDL_EventType($801);
  SDL_MULTIGESTURE    = TSDL_EventType($802);

  { Clipboard events }
  SDL_CLIPBOARDUPDATE = TSDL_EventType($900); // The clipboard changed

  { Drag and drop events }
  SDL_DROPFILE        = TSDL_EventType($1000); // The system requests a file open
  SDL_DROPTEXT        = TSDL_EventType($1001); // text/plain drag-and-drop event
  SDL_DROPBEGIN       = TSDL_EventType($1002); // A new set of drops is beginning (NULL filename)
  SDL_DROPCOMPLETE    = TSDL_EventType($1003); // Current set of drops is now complete (NULL filename)

  { Audio hotplug events }
  SDL_AUDIODEVICEADDED     = TSDL_EventType($1100); // A new audio device is available
  SDL_AUDIODEVICEREMOVED   = TSDL_EventType($1101); // An audio device has been removed.

  { Sensor events }
  SDL_SENSORUPDATED = TSDL_EventType($1200); // A sensor was updated

  { Render events }
  SDL_RENDER_TARGETS_RESET = TSDL_EventType($2000); // The render targets have been reset
  SDL_RENDER_DEVICE_RESET  = TSDL_EventType($2001); // The device has been reset and all textures need to be recreated

  { Internal events }
  SDL_POLLSENTINEL = TSDL_EventType($7F00); // Signals the end of an event poll cycle

  {** Events SDL_USEREVENT through SDL_LASTEVENT are for your use,
   *  and should be allocated with SDL_RegisterEvents()
   *}
  SDL_USEREVENT    = TSDL_EventType($8000);

  {**
   *  This last event is only for bounding internal arrays (needed in pascal ??)
   *}
  SDL_LASTEVENT    = TSDL_EventType($FFFF);

type
  {**
   *  Fields shared by every event
   *}
  PPSDL_CommonEvent = ^PSDL_CommonEvent;
  PSDL_CommonEvent = ^TSDL_CommonEvent;
  TSDL_CommonEvent = record
    type_: cuint32;
    timestamp: cuint32;
  end;

  {**
   * Display state change event data (event.display.*)
   *}
  PPSDL_DisplayEvent = ^PSDL_DisplayEvent;
  PSDL_DisplayEvent = ^TSDL_DisplayEvent;
  TSDL_DisplayEvent = record
    type_: cuint32;     // SDL_DISPLAYEVENT
    timestamp: cuint32; // In milliseconds, populated using SDL_GetTicks()
    display: cuint32;   // The associated display index
    event: cuint8;      // SDL_DisplayEventID
    padding1: cuint8;
    padding2: cuint8;
    padding3: cuint8;
    data1: cint32;      // event dependent data
  end;

  {**
   *  Window state change event data (event.window.*)
   *}
  PPSDL_WindowEvent = ^PSDL_WindowEvent;
  PSDL_WindowEvent = ^TSDL_WindowEvent;
  TSDL_WindowEvent = record
    type_: cuint32;       // SDL_WINDOWEVENT
    timestamp: cuint32;
    windowID: cuint32;    // The associated window
    event: cuint8;        // SDL_WindowEventID
    padding1: cuint8;
    padding2: cuint8;
    padding3: cuint8;
    data1: cint32;       // event dependent data
    data2: cint32;       // event dependent data
  end;

  {**
   *  Keyboard button event structure (event.key.*)
   *}
  PPSDL_KeyboardEvent = ^PSDL_KeyboardEvent;
  PSDL_KeyboardEvent = ^TSDL_KeyboardEvent;
  TSDL_KeyboardEvent = record
    type_: cuint32;        // SDL_KEYDOWN or SDL_KEYUP
    timestamp: cuint32;
    windowID: cuint32;     // The window with keyboard focus, if any
    state: cuint8;         // SDL_PRESSED or SDL_RELEASED
    repeat_: cuint8;       // Non-zero if this is a key repeat
    padding2: cuint8;
    padding3: cuint8;
    keysym: TSDL_KeySym;  // The key that was pressed or released
  end;

const
  SDL_TEXTEDITINGEVENT_TEXT_SIZE = 32;

type
  {**
   *  Keyboard text editing event structure (event.edit.*)
   *}
  PPSDL_TextEditingEvent = ^PSDL_TextEditingEvent;
  PSDL_TextEditingEvent = ^TSDL_TextEditingEvent;
  TSDL_TextEditingEvent = record
    type_: cuint32;                               // SDL_TEXTEDITING
    timestamp: cuint32;
    windowID: cuint32;                            // The window with keyboard focus, if any
    text: array[0..SDL_TEXTEDITINGEVENT_TEXT_SIZE] of Char;  // The editing text
    start: cint32;                               // The start cursor of selected editing text
    length: cint32;                              // The length of selected editing text
  end;

  {**
   *  Extended keyboard text editing event structure (event.editExt.*) when text would be
   *  truncated if stored in the text buffer SDL_TextEditingEvent
   *}
  PPSDL_TextEditingExtEvent = ^PSDL_TextEditingExtEvent;
  PSDL_TextEditingExtEvent = ^TSDL_TextEditingExtEvent;
  TSDL_TextEditingExtEvent = record
    type_: cuint32;     // SDL_TEXTEDITING_EXT
    timestamp: cuint32; // In milliseconds, populated using SDL_GetTicks()
    windowID: cuint32;  // The window with keyboard focus, if any
    text: PAnsiChar;    // The editing text, which should be freed with SDL_free(), and will not be NIL
    start: cint32;      // The start cursor of selected editing text
    length: cint32;     // The length of selected editing text
  end;

const
  SDL_TEXTINPUTEVENT_TEXT_SIZE = 32;

type

  {**
   *  Keyboard text input event structure (event.text.*)
   *}
  PPSDL_TextInputEvent = ^PSDL_TextInputEvent;
  PSDL_TextInputEvent = ^TSDL_TextInputEvent;
  TSDL_TextInputEvent = record
    type_: cuint32;                                          // SDL_TEXTINPUT
    timestamp: cuint32;
    windowID: cuint32;                                       // The window with keyboard focus, if any
    text: array[0..SDL_TEXTINPUTEVENT_TEXT_SIZE] of Char;   // The input text
  end;

  {**
   *  Mouse motion event structure (event.motion.*)
   *}
  PPSDL_MouseMotionEvent = ^PSDL_MouseMotionEvent;
  PSDL_MouseMotionEvent = ^TSDL_MouseMotionEvent;
  TSDL_MouseMotionEvent = record
    type_: cuint32;      // SDL_MOUSEMOTION
    timestamp: cuint32;  // In milliseconds, populated using SDL_GetTicks()
    windowID: cuint32;   // The window with mouse focus, if any
    which: cuint32;      // The mouse instance id, or SDL_TOUCH_MOUSEID
    state: cuint32;      // The current button state
    x: cint32;           // X coordinate, relative to window
    y: cint32;           // Y coordinate, relative to window
    xrel: cint32;        // The relative motion in the X direction
    yrel: cint32;        // The relative motion in the Y direction
  end;

  {**
   *  Mouse button event structure (event.button.*)
   *}
  PPSDL_MouseButtonEvent = ^PSDL_MouseButtonEvent;
  PSDL_MouseButtonEvent = ^TSDL_MouseButtonEvent;
  TSDL_MouseButtonEvent = record
    type_: cuint32;       // SDL_MOUSEBUTTONDOWN or SDL_MOUSEBUTTONUP
    timestamp: cuint32;
    windowID: cuint32;    // The window with mouse focus, if any
    which: cuint32;       // The mouse instance id, or SDL_TOUCH_MOUSEID
    button: cuint8;       // The mouse button index
    state: cuint8;        // SDL_PRESSED or SDL_RELEASED
    clicks: cuint8;       // 1 for single-click, 2 for double-click, etc.
    padding1: cuint8;
    x: cint32;           // X coordinate, relative to window
    y: cint32;           // Y coordinate, relative to window
  end;

  {**
   *  Mouse wheel event structure (event.wheel.*)
   *}
  PPSDL_MouseWheelEvent = ^PSDL_MouseWheelEvent;
  PSDL_MouseWheelEvent = ^TSDL_MouseWheelEvent;
  TSDL_MouseWheelEvent = record
    type_: cuint32;        // SDL_MOUSEWHEEL
    timestamp: cuint32;
    windowID: cuint32;     // The window with mouse focus, if any
    which: cuint32;        // The mouse instance id, or SDL_TOUCH_MOUSEID
    x: cint32;             // The amount scrolled horizontally
    y: cint32;             // The amount scrolled vertically
    direction: cuint32;    // Set to one of the SDL_MOUSEWHEEL_* defines. When FLIPPED the values in X and Y will be opposite. Multiply by -1 to change them back
    preciseX: cfloat;      // The amount scrolled horizontally, positive to the right and negative to the left, with float precision (added in 2.0.18)
    preciseY: cfloat;      // The amount scrolled vertically, positive away from the user and negative toward the user, with float precision (added in 2.0.18)
    mouseX: cint32;        // X coordinate, relative to window (added in 2.26.0)
    mouseY: cint32;        // Y coordinate, relative to window (added in 2.26.0)
  end;

  {**
   *  Joystick axis motion event structure (event.jaxis.*)
   *}
  PPSDL_JoyAxisEvent = ^PSDL_JoyAxisEvent;
  PSDL_JoyAxisEvent = ^TSDL_JoyAxisEvent;
  TSDL_JoyAxisEvent = record
    type_: cuint32;         // SDL_JOYAXISMOTION
    timestamp: cuint32;
    which: TSDL_JoystickID; // The joystick instance id
    axis: cuint8;           // The joystick axis index
    padding1: cuint8;
    padding2: cuint8;
    padding3: cuint8;
    value: cint16;         // The axis value (range: -32768 to 32767)
    padding4: cuint16;
  end;

  {**
   *  Joystick trackball motion event structure (event.jball.*)
   *}
  PPSDL_JoyBallEvent = ^PSDL_JoyBallEvent;
  PSDL_JoyBallEvent = ^TSDL_JoyBallEvent;
  TSDL_JoyBallEvent = record
    type_: cuint32;         // SDL_JOYBALLMOTION
    timestamp: cuint32;
    which: TSDL_JoystickID; // The joystick instance id
    ball: cuint8;           // The joystick trackball index
    padding1: cuint8;
    padding2: cuint8;
    padding3: cuint8;
    xrel: cint16;          // The relative motion in the X direction
    yrel: cint16;          // The relative motion in the Y direction
  end;

  {**
   *  Joystick hat position change event structure (event.jhat.*)
   *}
  PPSDL_JoyHatEvent = ^PSDL_JoyHatEvent;
  PSDL_JoyHatEvent = ^TSDL_JoyHatEvent;
  TSDL_JoyHatEvent = record
    type_: cuint32;         // SDL_JOYHATMOTION
    timestamp: cuint32;
    which: TSDL_JoystickID; // The joystick instance id
    hat: cuint8;            // The joystick hat index
    value: cuint8;         {*  The hat position value.
                           *  SDL_HAT_LEFTUP   SDL_HAT_UP       SDL_HAT_RIGHTUP
                           *  SDL_HAT_LEFT     SDL_HAT_CENTERED SDL_HAT_RIGHT
                           *  SDL_HAT_LEFTDOWN SDL_HAT_DOWN     SDL_HAT_RIGHTDOWN
                           *
                           *  Note that zero means the POV is centered.
                           *}
    padding1: cuint8;
    padding2: cuint8;
  end;

  {**
   *  Joystick button event structure (event.jbutton.*)
   *}
  PPSDL_JoyButtonEvent = ^PSDL_JoyButtonEvent;
  PSDL_JoyButtonEvent = ^TSDL_JoyButtonEvent;
  TSDL_JoyButtonEvent = record
    type_: cuint32;        // SDL_JOYBUTTONDOWN or SDL_JOYBUTTONUP
    timestamp: cuint32;
    which: TSDL_JoystickID; // The joystick instance id
    button: cuint8;         // The joystick button index
    state: cuint8;          // SDL_PRESSED or SDL_RELEASED
    padding1: cuint8;
    padding2: cuint8;
  end;

  {**
   *  Joystick device event structure (event.jdevice.*)
   *}
  PPSDL_JoyDeviceEvent = ^PSDL_JoyDeviceEvent;
  PSDL_JoyDeviceEvent = ^TSDL_JoyDeviceEvent;
  TSDL_JoyDeviceEvent = record
    type_: cuint32;      // SDL_JOYDEVICEADDED or SDL_JOYDEVICEREMOVED
    timestamp: cuint32;
    which: cint32;      // The joystick device index for the ADDED event, instance id for the REMOVED event
  end;

  {**
   *  Joysick battery level change event structure (event.jbattery.*)
   *}
  PPSDL_JoyBatteryEvent = ^PSDL_JoyBatteryEvent;
  PSDL_JoyBatteryEvent = ^TSDL_JoyBatteryEvent;
  TSDL_JoyBatteryEvent = record
    type_: cuint32;                 // SDL_JOYBATTERYUPDATED
    timestamp: cuint32;             // In milliseconds, populated using SDL_GetTicks()
    which: TSDL_JoystickID;         // The joystick instance id
    level: TSDL_JoystickPowerLevel; // The joystick battery level
  end;

  {**
   *  Game controller axis motion event structure (event.caxis.*)
   *}
  PPSDL_ControllerAxisEvent = ^PSDL_ControllerAxisEvent;
  PSDL_ControllerAxisEvent = ^TSDL_ControllerAxisEvent;
  TSDL_ControllerAxisEvent = record
    type_: cuint32;         // SDL_CONTROLLERAXISMOTION
    timestamp: cuint32;
    which: TSDL_JoystickID; // The joystick instance id
    axis: cuint8;           // The controller axis (SDL_GameControllerAxis)
    padding1: cuint8;
    padding2: cuint8;
    padding3: cuint8;
    value: cint16;         // The axis value (range: -32768 to 32767)
    padding4: cuint16;
  end;

  {**
   *  Game controller button event structure (event.cbutton.*)
   *}
  PPSDL_ControllerButtonEvent = ^PSDL_ControllerButtonEvent;
  PSDL_ControllerButtonEvent = ^TSDL_ControllerButtonEvent;
  TSDL_ControllerButtonEvent = record
    type_: cuint32;         // SDL_CONTROLLERBUTTONDOWN or SDL_CONTROLLERBUTTONUP
    timestamp: cuint32;
    which: TSDL_JoystickID; // The joystick instance id
    button: cuint8;         // The controller button (SDL_GameControllerButton)
    state: cuint8;          // SDL_PRESSED or SDL_RELEASED
    padding1: cuint8;
    padding2: cuint8;
  end;


  {**
   *  Controller device event structure (event.cdevice.*)
   *}
  PPSDL_ControllerDeviceEvent = ^PSDL_ControllerDeviceEvent;
  PSDL_ControllerDeviceEvent = ^TSDL_ControllerDeviceEvent;
  TSDL_ControllerDeviceEvent = record
    type_: cuint32;       // SDL_CONTROLLERDEVICEADDED, SDL_CONTROLLERDEVICEREMOVED, or SDL_CONTROLLERDEVICEREMAPPED
    timestamp: cuint32;
    which: cint32;       // The joystick device index for the ADDED event, instance id for the REMOVED or REMAPPED event
  end;

  {**
   *  Game controller touchpad event structure (event.ctouchpad.*)
   *}
  PPSDL_ControllerTouchpadEvent = ^PSDL_ControllerTouchpadEvent;
  PSDL_ControllerTouchpadEvent = ^TSDL_ControllerTouchpadEvent;
  TSDL_ControllerTouchpadEvent = record
    type_: cuint32;         // SDL_CONTROLLERTOUCHPADDOWN or SDL_CONTROLLERTOUCHPADMOTION or SDL_CONTROLLERTOUCHPADUP
    timestamp: cuint32;     // In milliseconds, populated using SDL_GetTicks()
    which: TSDL_JoystickID; // The joystick instance id
    touchpad: cint32;       // The index of the touchpad
    finger: cint32;         // The index of the finger on the touchpad
    x: cfloat;              // Normalized in the range 0...1 with 0 being on the left
    y: cfloat;              // Normalized in the range 0...1 with 0 being at the top
    pressure: cfloat;       // Normalized in the range 0...1
  end;

  {**
   *  Game controller sensor event structure (event.csensor.*)
   *}
  PPSDL_ControllerSensorEvent = ^PSDL_ControllerSensorEvent;
  PSDL_ControllerSensorEvent = ^TSDL_ControllerSensorEvent;
  TSDL_ControllerSensorEvent = record
    type_: cuint32;              // SDL_CONTROLLERSENSORUPDATE
    timestamp: cuint32;          // In milliseconds, populated using SDL_GetTicks()
    which: TSDL_JoystickID;      // The joystick instance id
    sensor: cint32;              // The type of the sensor, one of the values of SDL_SensorType
    data: array[0..2] of cfloat; // Up to 3 values from the sensor, as defined in SDL_sensor.h
  end;

  {**
   *  Audio device event structure (event.adevice.*)
   *}
  PPSDL_AudioDeviceEvent = ^PSDL_AudioDeviceEvent;
  PSDL_AudioDeviceEvent = ^TSDL_AudioDeviceEvent;
  TSDL_AudioDeviceEvent = record
    type_: cuint32;        // ::SDL_AUDIODEVICEADDED, or ::SDL_AUDIODEVICEREMOVED
    timestamp: cuint32;
    which: cuint32;        // The audio device index for the ADDED event (valid until next SDL_GetNumAudioDevices() call), SDL_AudioDeviceID for the REMOVED event
    iscapture: cuint8;     // zero if an output device, non-zero if a capture device.
    padding1: cuint8;
    padding2: cuint8;
    padding3: cuint8;
  end;


  {**
   *  Touch finger event structure (event.tfinger.*)
   *}
  PPSDL_TouchFingerEvent = ^PSDL_TouchFingerEvent;
  PSDL_TouchFingerEvent = ^TSDL_TouchFingerEvent;
  TSDL_TouchFingerEvent = record
    type_: cuint32;         // SDL_FINGERMOTION or SDL_FINGERDOWN or SDL_FINGERUP
    timestamp: cuint32;
    touchId: TSDL_TouchID;  // The touch device id
    fingerId: TSDL_FingerID;
    x: cfloat;              // Normalized in the range 0...1
    y: cfloat;              // Normalized in the range 0...1
    dx: cfloat;             // Normalized in the range 0...1
    dy: cfloat;             // Normalized in the range 0...1
    pressure: cfloat;       // Normalized in the range 0...1
    window: cuint32;        // The window underneath the finger, if any
  end;

  {**
   *  Multiple Finger Gesture Event (event.mgesture.*)
   *}
  PPSDL_MultiGestureEvent = ^PSDL_MultiGestureEvent;
  PSDL_MultiGestureEvent = ^TSDL_MultiGestureEvent;
  TSDL_MultiGestureEvent = record
    type_: cuint32;        // SDL_MULTIGESTURE
    timestamp: cuint32;
    touchId: TSDL_TouchID; // The touch device index
    dTheta: cfloat;
    dDist: cfloat;
    x: cfloat;
    y: cfloat;
    numFingers: cuint16;
    padding: cuint16;
  end;


  {* (event.dgesture.*) *}
  PPSDL_DollarGestureEvent = ^PSDL_DollarGestureEvent;
  PSDL_DollarGestureEvent = ^TSDL_DollarGestureEvent;
  TSDL_DollarGestureEvent = record
    type_: cuint32;         // SDL_DOLLARGESTURE
    timestamp: cuint32;
    touchId: TSDL_TouchID;  // The touch device id
    gestureId: TSDL_GestureID;
    numFingers: cuint32;
    error: cfloat;
    x: cfloat;              // Normalized center of gesture
    y: cfloat;              // Normalized center of gesture
  end;


  {**
   *  An event used to request a file open by the system (event.drop.*)
   *  This event is disabled by default, you can enable it with SDL_EventState()
   *  If you enable this event, you must free the filename in the event.
   *}
  PPSDL_DropEvent = ^PSDL_DropEvent;
  PSDL_DropEvent = ^TSDL_DropEvent;
  TSDL_DropEvent = record
    type_: cuint32;      // SDL_DROPBEGIN or SDL_DROPFILE or SDL_DROPTEXT or SDL_DROPCOMPLETE
    timestamp: cuint32;
    file_: PAnsiChar;   // The file name, which should be freed with SDL_free(), is NIL on begin/complete
    windowID: cuint32;  // The window that was dropped on, if any
  end;

  {**
   *  Sensor event structure (event.sensor.*)
   *}
  PPSDL_SensorEvent = ^PSDL_SensorEvent;
  PSDL_SensorEvent = ^TSDL_SensorEvent;
  TSDL_SensorEvent = record
    type_: cuint32;              // SDL_SENSORUPDATED
    timestamp: cuint32;          // In milliseconds, populated using SDL_GetTicks()
    which: cint32;               // The instance ID of the sensor
    data: array[0..5] of cfloat; // Up to 6 values from the sensor - additional values can be queried using SDL_SensorGetData()
  end;

  {**
   *  The "quit requested" event
   *}
  PPSDL_QuitEvent = ^PSDL_QuitEvent;
  PSDL_QuitEvent = ^TSDL_QuitEvent;
  TSDL_QuitEvent = record
    type_: cuint32;        // SDL_QUIT
    timestamp: cuint32;
  end;

  {**
   *  A user-defined event type (event.user.*)
   *}
  PPSDL_UserEvent = ^PSDL_UserEvent;
  PSDL_UserEvent = ^TSDL_UserEvent;
  TSDL_UserEvent = record
    type_: cuint32;       // SDL_USEREVENT through SDL_NUMEVENTS-1
    timestamp: cuint32;
    windowID: cuint32;    // The associated window if any
    code: cint32;        // User defined event code
    data1: Pointer;      // User defined data pointer
    data2: Pointer;      // User defined data pointer
  end;

  PSDL_SysWMmsg=Pointer;

  {**
   *  A video driver dependent system event (event.syswm.*)
   *  This event is disabled by default, you can enable it with SDL_EventState()
   *
   *  If you want to use this event, you should include SDL_syswm.h.
   *}
  PPSDL_SysWMEvent = ^PSDL_SysWMEvent;
  PSDL_SysWMEvent = ^TSDL_SysWMEvent;
  TSDL_SysWMEvent = record
    type_: cuint32;       // SDL_SYSWMEVENT
    timestamp: cuint32;
    msg: PSDL_SysWMmsg;  // driver dependent data (defined in SDL_syswm.h)
  end;

  {**
   *  General event structure
   *}
  PPSDL_Event = ^PSDL_Event;
  PSDL_Event = ^TSDL_Event;
  TSDL_Event = record
    case cint of
      0:  (type_: cuint32);

      SDL_COMMONEVENT:  (common: TSDL_CommonEvent);
      SDL_DISPLAYEVENT: (display: TSDL_DisplayEvent);
      SDL_WINDOWEVENT:  (window: TSDL_WindowEvent);

      SDL_KEYUP,
      SDL_KEYDOWN:  (key: TSDL_KeyboardEvent);
      SDL_TEXTEDITING:  (edit: TSDL_TextEditingEvent);
      SDL_TEXTEDITING_EXT: (exitExt: TSDL_TextEditingExtEvent);
      SDL_TEXTINPUT:  (text: TSDL_TextInputEvent);

      SDL_MOUSEMOTION:  (motion: TSDL_MouseMotionEvent);
      SDL_MOUSEBUTTONUP,
      SDL_MOUSEBUTTONDOWN:  (button: TSDL_MouseButtonEvent);
      SDL_MOUSEWHEEL:  (wheel: TSDL_MouseWheelEvent);

      SDL_JOYAXISMOTION:  (jaxis: TSDL_JoyAxisEvent);
      SDL_JOYBALLMOTION: (jball: TSDL_JoyBallEvent);
      SDL_JOYHATMOTION: (jhat: TSDL_JoyHatEvent);
      SDL_JOYBUTTONDOWN,
      SDL_JOYBUTTONUP: (jbutton: TSDL_JoyButtonEvent);
      SDL_JOYDEVICEADDED,
      SDL_JOYDEVICEREMOVED: (jdevice: TSDL_JoyDeviceEvent);
      SDL_JOYBATTERYUPDATED: (jbattery: TSDL_JoyBatteryEvent);

      SDL_CONTROLLERAXISMOTION: (caxis: TSDL_ControllerAxisEvent);
      SDL_CONTROLLERBUTTONUP,
      SDL_CONTROLLERBUTTONDOWN: (cbutton: TSDL_ControllerButtonEvent);
      SDL_CONTROLLERDEVICEADDED,
      SDL_CONTROLLERDEVICEREMOVED,
      SDL_CONTROLLERDEVICEREMAPPED: (cdevice: TSDL_ControllerDeviceEvent);
      SDL_CONTROLLERTOUCHPADDOWN,
      SDL_CONTROLLERTOUCHPADMOTION,
      SDL_CONTROLLERTOUCHPADUP: (ctouchpad: TSDL_ControllerTouchpadEvent);
      SDL_CONTROLLERSENSORUPDATE: (csensor: TSDL_ControllerSensorEvent);

      SDL_AUDIODEVICEADDED,
      SDL_AUDIODEVICEREMOVED: (adevice: TSDL_AudioDeviceEvent);

      SDL_SENSORUPDATED: (sensor: TSDL_SensorEvent);

      SDL_QUITEV: (quit: TSDL_QuitEvent);

      SDL_USEREVENT: (user: TSDL_UserEvent);
      SDL_SYSWMEVENT: (syswm: TSDL_SysWMEvent);

      SDL_FINGERDOWN,
      SDL_FINGERUP,
      SDL_FINGERMOTION: (tfinger: TSDL_TouchFingerEvent);
      SDL_MULTIGESTURE: (mgesture: TSDL_MultiGestureEvent);
      SDL_DOLLARGESTURE,SDL_DOLLARRECORD: (dgesture: TSDL_DollarGestureEvent);

      SDL_DROPFILE: (drop: TSDL_DropEvent);
  end;

///

function  SDL_InitSubSystem(flags: TSDL_Init): cint; cdecl;
procedure SDL_QuitSubSystem(flags: TSDL_Init); cdecl;

var
 SDL_PollEvent:function(event: PSDL_Event): cint32 cdecl;

 SDL_LockJoysticks  :procedure(); cdecl;
 SDL_UnlockJoysticks:procedure(); cdecl;
 SDL_NumJoysticks   :function(): cint; cdecl;

 SDL_JoystickGetDeviceGUID:function(device_index: cint): TSDL_JoystickGUID; cdecl;
 SDL_JoystickGetGUID      :function(joystick: PSDL_Joystick): TSDL_JoystickGUID; cdecl;

////
 SDL_IsGameController   :function(joystick_index: cint): Boolean cdecl;
 SDL_GameControllerOpen :function(joystick_index: cint): PSDL_GameController cdecl;
 SDL_GameControllerClose:procedure(gamecontroller: PSDL_GameController) cdecl;

 SDL_GameControllerGetJoystick:function(gamecontroller: PSDL_GameController): PSDL_Joystick cdecl;
 SDL_GameControllerGetAttached:function(gamecontroller: PSDL_GameController): Boolean cdecl;
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
 Pointer(SDL_PollEvent)     :=GetProcedureAddress(lib_handle,'SDL_PollEvent');

 if (_SDL_InitSubSystem=nil) then
 begin
  UnloadLibrary(lib_handle);
  lib_handle:=NilHandle;
  init_flags:=0;
  Exit(-1);
 end;

 Result:=_SDL_InitSubSystem(flags);
 if (Result<>0) then Exit;

 init_flags:=init_flags or flags;

 if ((flags and SDL_INIT_JOYSTICK)<>0) then
 begin
  Pointer(SDL_LockJoysticks        ):=GetProcedureAddress(lib_handle,'SDL_LockJoysticks');
  Pointer(SDL_UnlockJoysticks      ):=GetProcedureAddress(lib_handle,'SDL_UnlockJoysticks');
  Pointer(SDL_NumJoysticks         ):=GetProcedureAddress(lib_handle,'SDL_NumJoysticks');
  Pointer(SDL_JoystickGetDeviceGUID):=GetProcedureAddress(lib_handle,'SDL_JoystickGetDeviceGUID');
  Pointer(SDL_JoystickGetGUID      ):=GetProcedureAddress(lib_handle,'SDL_JoystickGetGUID');
 end;

 if ((flags and SDL_INIT_GAMECONTROLLER)<>0) then
 begin
  Pointer(SDL_IsGameController                   ):=GetProcedureAddress(lib_handle,'SDL_IsGameController');
  Pointer(SDL_GameControllerOpen                 ):=GetProcedureAddress(lib_handle,'SDL_GameControllerOpen');
  Pointer(SDL_GameControllerClose                ):=GetProcedureAddress(lib_handle,'SDL_GameControllerClose');
  Pointer(SDL_GameControllerGetJoystick          ):=GetProcedureAddress(lib_handle,'SDL_GameControllerGetJoystick');
  Pointer(SDL_GameControllerGetAttached          ):=GetProcedureAddress(lib_handle,'SDL_GameControllerGetAttached');
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
 if (lib_handle=NilHandle) then Exit;

 init_flags:=init_flags and (not flags);

 if (_SDL_QuitSubSystem<>nil) then
 begin
  _SDL_QuitSubSystem(flags);
 end;

 if (init_flags=0) then
 begin
  UnloadLibrary(lib_handle);
  lib_handle:=NilHandle;

  Pointer(_SDL_InitSubSystem):=nil;
  Pointer(_SDL_QuitSubSystem):=nil;
  Pointer(SDL_PollEvent)     :=nil;
 end;

 if ((flags and SDL_INIT_JOYSTICK)<>0) then
 begin
  Pointer(SDL_LockJoysticks        ):=nil;
  Pointer(SDL_UnlockJoysticks      ):=nil;
  Pointer(SDL_NumJoysticks         ):=nil;
  Pointer(SDL_JoystickGetDeviceGUID):=nil;
  Pointer(SDL_JoystickGetGUID      ):=nil;
 end;

 if ((flags and SDL_INIT_GAMECONTROLLER)<>0) then
 begin
  Pointer(SDL_IsGameController                   ):=nil;
  Pointer(SDL_GameControllerOpen                 ):=nil;
  Pointer(SDL_GameControllerClose                ):=nil;
  Pointer(SDL_GameControllerGetJoystick          ):=nil;
  Pointer(SDL_GameControllerGetAttached          ):=nil;
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

