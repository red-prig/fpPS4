unit XInput;

interface

uses
 dynlibs;

type
  BOOL = longbool;

const
  XINPUT_DLL:array[0..2] of PChar=(
   'XInput1_4.dll',
   'XInput1_3.dll',
   'XInput9_1_0.dll'
  );

  // Device types available in XINPUT_CAPABILITIES
  XINPUT_DEVTYPE_GAMEPAD          = $01;
  XINPUT_DEVSUBTYPE_WHEEL         = $02;
  XINPUT_DEVSUBTYPE_ARCADE_STICK  = $03;
  XINPUT_DEVSUBTYPE_FLIGHT_SICK   = $04;
  XINPUT_DEVSUBTYPE_DANCE_PAD     = $05;

  // Device subtypes available in XINPUT_CAPABILITIES
  XINPUT_DEVSUBTYPE_GAMEPAD       = $01;

  // Flags for XINPUT_CAPABILITIES
  XINPUT_CAPS_VOICE_SUPPORTED     = $0004;

  // Constants for gamepad buttons
  XINPUT_GAMEPAD_DPAD_UP          = $0001;
  XINPUT_GAMEPAD_DPAD_DOWN        = $0002;
  XINPUT_GAMEPAD_DPAD_LEFT        = $0004;
  XINPUT_GAMEPAD_DPAD_RIGHT       = $0008;
  XINPUT_GAMEPAD_START            = $0010;
  XINPUT_GAMEPAD_BACK             = $0020;
  XINPUT_GAMEPAD_LEFT_THUMB       = $0040;
  XINPUT_GAMEPAD_RIGHT_THUMB      = $0080;
  XINPUT_GAMEPAD_LEFT_SHOULDER    = $0100;
  XINPUT_GAMEPAD_RIGHT_SHOULDER   = $0200;
  XINPUT_GAMEPAD_GUIDE            = $0400; //Undocumented
  XINPUT_GAMEPAD_A                = $1000;
  XINPUT_GAMEPAD_B                = $2000;
  XINPUT_GAMEPAD_X                = $4000;
  XINPUT_GAMEPAD_Y                = $8000;

  // Gamepad thresholds
  XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE  = 7849;
  XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE = 8689;
  XINPUT_GAMEPAD_TRIGGER_THRESHOLD    = 30;

  // Flags to pass to XInputGetCapabilities
  XINPUT_FLAG_GAMEPAD             = $00000001;

  // Devices that support batteries
  BATTERY_DEVTYPE_GAMEPAD         = $00;
  BATTERY_DEVTYPE_HEADSET         = $01;

  // Flags for battery status level
  BATTERY_TYPE_DISCONNECTED       = $00;    // This device is not connected
  BATTERY_TYPE_WIRED              = $01;    // Wired device, no battery
  BATTERY_TYPE_ALKALINE           = $02;    // Alkaline battery source
  BATTERY_TYPE_NIMH               = $03;    // Nickel Metal Hydride battery source
  BATTERY_TYPE_UNKNOWN            = $FF;    // Cannot determine the battery type

  // These are only valid for wireless, connected devices, with known battery types
  // The amount of use time remaining depends on the type of device.
  BATTERY_LEVEL_EMPTY             = $00;
  BATTERY_LEVEL_LOW               = $01;
  BATTERY_LEVEL_MEDIUM            = $02;
  BATTERY_LEVEL_FULL              = $03;

  // User index definitions
  XUSER_MAX_COUNT = 4;
  XUSER_INDEX_ANY = $FF;

  // Codes returned for the gamepad keystroke
  VK_PAD_A                        = $5800;
  VK_PAD_B                        = $5801;
  VK_PAD_X                        = $5802;
  VK_PAD_Y                        = $5803;
  VK_PAD_RSHOULDER                = $5804;
  VK_PAD_LSHOULDER                = $5805;
  VK_PAD_LTRIGGER                 = $5806;
  VK_PAD_RTRIGGER                 = $5807;

  VK_PAD_DPAD_UP                  = $5810;
  VK_PAD_DPAD_DOWN                = $5811;
  VK_PAD_DPAD_LEFT                = $5812;
  VK_PAD_DPAD_RIGHT               = $5813;
  VK_PAD_START                    = $5814;
  VK_PAD_BACK                     = $5815;
  VK_PAD_LTHUMB_PRESS             = $5816;
  VK_PAD_RTHUMB_PRESS             = $5817;

  VK_PAD_LTHUMB_UP                = $5820;
  VK_PAD_LTHUMB_DOWN              = $5821;
  VK_PAD_LTHUMB_RIGHT             = $5822;
  VK_PAD_LTHUMB_LEFT              = $5823;
  VK_PAD_LTHUMB_UPLEFT            = $5824;
  VK_PAD_LTHUMB_UPRIGHT           = $5825;
  VK_PAD_LTHUMB_DOWNRIGHT         = $5826;
  VK_PAD_LTHUMB_DOWNLEFT          = $5827;

  VK_PAD_RTHUMB_UP                = $5830;
  VK_PAD_RTHUMB_DOWN              = $5831;
  VK_PAD_RTHUMB_RIGHT             = $5832;
  VK_PAD_RTHUMB_LEFT              = $5833;
  VK_PAD_RTHUMB_UPLEFT            = $5834;
  VK_PAD_RTHUMB_UPRIGHT           = $5835;
  VK_PAD_RTHUMB_DOWNRIGHT         = $5836;
  VK_PAD_RTHUMB_DOWNLEFT          = $5837;

  // Flags used in XINPUT_KEYSTROKE
  XINPUT_KEYSTROKE_KEYDOWN        = $0001;
  XINPUT_KEYSTROKE_KEYUP          = $0002;
  XINPUT_KEYSTROKE_REPEAT         = $0004;

  ERROR_EMPTY                = 4306;
  ERROR_DEVICE_NOT_CONNECTED = 1167;

type
  PXInputGamepad = ^TXInputGamepad;

  TXInputGamepad = record
    wButtons: word;
    bLeftTrigger: byte;
    bRightTrigger: byte;
    sThumbLX: smallint;
    sThumbLY: smallint;
    sThumbRX: smallint;
    sThumbRY: smallint;
  end;

  PXInputState = ^TXInputState;
  TXInputState = record
    dwPacketNumber: DWORD;
    Gamepad: TXInputGamepad;
  end;

  PXInputVibration = ^TXInputVibration;
  TXInputVibration = record
    wLeftMotorSpeed: word;
    wRightMotorSpeed: word;
  end;

  PXInputCapabilities = ^TXInputCapabilities;
  TXInputCapabilities = record
    _Type:            Byte;
    SubType:          Byte;
    Flags:            Word;
    Gamepad:          TXInputGamepad;
    Vibration:        TXInputVibration;
  end;

  PXInputBatteryInformation = ^TXInputBatteryInformation;
  TXInputBatteryInformation = record
    BatteryType: Byte;
    BatteryLevel: Byte;
  end;

  PXInputKeystroke = ^TXInputKeystroke;
  TXInputKeystroke = record
    VirtualKey: Word;
    Unicode: WideChar;
    Flags: Word;
    UserIndex: Byte;
    HidCode: Byte;
  end;

function  XInput_Init: Integer; stdcall;
procedure XInput_Quit; stdcall;

var
 XInputGetState:function(
    dwUserIndex: DWORD;      // [in] Index of the gamer associated with the device
    out pState: TXInputState // [out] Receives the current state
 ): DWORD; stdcall;

 XInputSetState:function(
    dwUserIndex: DWORD;                 // [in] Index of the gamer associated with the device
    const pVibration: TXInputVibration  // [in, out] The vibration information to send to the controller
 ): DWORD; stdcall;

 XInputGetCapabilities:function(
    dwUserIndex: DWORD;                     // [in] Index of the gamer associated with the device
    dwFlags: DWORD;                         // [in] Input flags that identify the device type
    out pCapabilities: TXInputCapabilities  // [out] Receives the capabilities
 ): DWORD; stdcall;

 XInputEnable:procedure(
    enable: BOOL     // [in] Indicates whether xinput is enabled or disabled.
 ); stdcall;

 XInputGetDSoundAudioDeviceGuids:function(
    dwUserIndex: DWORD;           // [in] Index of the gamer associated with the device
    out pDSoundRenderGuid: TGUID; // [out] DSound device ID for render
    out pDSoundCaptureGuid: TGUID // [out] DSound device ID for capture
 ): DWORD; stdcall;

 XInputGetBatteryInformation:function(
    dwUserIndex: DWORD;          // [in]  Index of the gamer associated with the device
    devType: Byte;               // [in]  Which device on this user index
    out pBatteryInformation: TXInputBatteryInformation // [out] Contains the level and types of batteries
 ): DWORD; stdcall;

 XInputGetKeystroke:function(
    dwUserIndex: DWORD;               // [in]  Index of the gamer associated with the device
    dwReserved: DWORD;                // [in]  Reserved for future use
    var pKeystroke: TXInputKeystroke  // [out] Pointer to an XINPUT_KEYSTROKE structure that receives an input event.
 ): DWORD; stdcall;

implementation

var
 lib_handle:TLibHandle=NilHandle;

function XInput_Init:Integer; stdcall;
var
 i:Integer;
begin
 Result:=0;

 if (lib_handle<>NilHandle) then Exit;

 For i:=0 to High(XINPUT_DLL) do
 begin
  lib_handle:=SafeLoadLibrary(XINPUT_DLL[i]);
  if (lib_handle<>NilHandle) then Break;
 end;

 if (lib_handle=NilHandle) then Exit(-1);

 Pointer(XInputGetState):=GetProcedureAddress(lib_handle,100);

 if (XInputGetState=nil) then
 begin
  Pointer(XInputGetState):=GetProcedureAddress(lib_handle,'XInputGetState');
 end;

 if (XInputGetState=nil) then
 begin
  UnloadLibrary(lib_handle);
  lib_handle:=NilHandle;
  Exit(-1);
 end;

 Pointer(XInputSetState                 ):=GetProcedureAddress(lib_handle,'XInputSetState');
 Pointer(XInputGetCapabilities          ):=GetProcedureAddress(lib_handle,'XInputGetCapabilities');
 Pointer(XInputEnable                   ):=GetProcedureAddress(lib_handle,'XInputEnable');
 Pointer(XInputGetDSoundAudioDeviceGuids):=GetProcedureAddress(lib_handle,'XInputGetDSoundAudioDeviceGuids');
 Pointer(XInputGetBatteryInformation    ):=GetProcedureAddress(lib_handle,'XInputGetBatteryInformation');
 Pointer(XInputGetKeystroke             ):=GetProcedureAddress(lib_handle,'XInputGetKeystroke');
end;

procedure XInput_Quit; stdcall;
begin
 if (lib_handle=NilHandle) then Exit;

 UnloadLibrary(lib_handle);
 lib_handle:=NilHandle;

 Pointer(XInputGetState                 ):=nil;
 Pointer(XInputSetState                 ):=nil;
 Pointer(XInputGetCapabilities          ):=nil;
 Pointer(XInputEnable                   ):=nil;
 Pointer(XInputGetDSoundAudioDeviceGuids):=nil;
 Pointer(XInputGetBatteryInformation    ):=nil;
 Pointer(XInputGetKeystroke             ):=nil;
end;


end.


