unit SDL3;

{$mode ObjFPC}{$H+}

interface

const
  SDL_LibName = 'SDL3.dll';

const
  SDL_INIT_AUDIO    = $00000010; { `SDL_INIT_AUDIO` implies `SDL_INIT_EVENTS`  }
  SDL_INIT_VIDEO    = $00000020; { `SDL_INIT_VIDEO` implies `SDL_INIT_EVENTS`, should be initialized on the main thread  }
  SDL_INIT_JOYSTICK = $00000200; { `SDL_INIT_JOYSTICK` implies `SDL_INIT_EVENTS`  }
  SDL_INIT_HAPTIC   = $00001000;
  SDL_INIT_GAMEPAD  = $00002000; { `SDL_INIT_GAMEPAD` implies `SDL_INIT_JOYSTICK`  }
  SDL_INIT_EVENTS   = $00004000;
  SDL_INIT_SENSOR   = $00008000; { `SDL_INIT_SENSOR` implies `SDL_INIT_EVENTS`  }
  SDL_INIT_CAMERA   = $00010000; { `SDL_INIT_CAMERA` implies `SDL_INIT_EVENTS`  }

const
  SDL_APP_CONTINUE = 0; {*< Value that requests that the app continue from the main callbacks.  }
  SDL_APP_SUCCESS  = 1; {*< Value that requests termination with success from the main callbacks.  }
  SDL_APP_FAILURE  = 2; {*< Value that requests termination with error from the main callbacks.  }

const
  SDL_HINT_APP_NAME     = 'SDL_APP_NAME';
  SDL_HINT_AUDIO_DRIVER = 'SDL_AUDIO_DRIVER';

const
  { Audio hotplug events  }
  SDL_EVENT_AUDIO_DEVICE_ADDED          = ($1100); {*< A new audio device is available  }
  SDL_EVENT_AUDIO_DEVICE_REMOVED        = ($1101); {*< An audio device has been removed.  }
  SDL_EVENT_AUDIO_DEVICE_FORMAT_CHANGED = ($1102); {*< An audio device's format has been changed by the system.  }

  SDL_EVENT_USER                        = ($8000);

type
  PPSDL_Event = ^PSDL_Event;
  PSDL_Event = ^TSDL_Event;
  TSDL_Event = record
      case Integer of
         0: (type_: Integer);
           //....
        37: (padding: array[0..127] of Byte);
  end;

function  SDL_WasInit      (flags: Integer): Integer;
function  SDL_InitSubSystem(flags: Integer): Boolean;
procedure SDL_QuitSubSystem(flags: Integer);
procedure SDL_free         (mem: Pointer);
function  SDL_PollEvent    (event: PSDL_Event): Boolean;
function  SDL_WaitEvent    (event: PSDL_Event): Boolean;

const
  SDL_AUDIO_MASK_BITSIZE    = $FF;
  SDL_AUDIO_MASK_FLOAT      = 1 shl 8;
  SDL_AUDIO_MASK_BIG_ENDIAN = 1 shl 12;
  SDL_AUDIO_MASK_SIGNED     = 1 shl 15;

const
  SDL_AUDIO_UNKNOWN  = $0000; {*< Unspecified audio format  }
  SDL_AUDIO_U8       = $0008; {*< Unsigned 8-bit samples  }
                              { SDL_DEFINE_AUDIO_FORMAT(0, 0, 0, 8),  }
  SDL_AUDIO_S8       = $8008; {*< Signed 8-bit samples  }
                              { SDL_DEFINE_AUDIO_FORMAT(1, 0, 0, 8),  }
  SDL_AUDIO_S16LE    = $8010; {*< Signed 16-bit samples  }
                              { SDL_DEFINE_AUDIO_FORMAT(1, 0, 0, 16),  }
  SDL_AUDIO_S16BE    = $9010; {*< As above, but big-endian byte order  }
                              { SDL_DEFINE_AUDIO_FORMAT(1, 1, 0, 16),  }
  SDL_AUDIO_S32LE    = $8020; {*< 32-bit integer samples  }
                              { SDL_DEFINE_AUDIO_FORMAT(1, 0, 0, 32),  }
  SDL_AUDIO_S32BE    = $9020; {*< As above, but big-endian byte order  }
                              { SDL_DEFINE_AUDIO_FORMAT(1, 1, 0, 32),  }
  SDL_AUDIO_F32LE    = $8120; {*< 32-bit floating point samples  }
                              { SDL_DEFINE_AUDIO_FORMAT(1, 0, 1, 32),  }
  SDL_AUDIO_F32BE    = $9120; {*< As above, but big-endian byte order  }
                              { SDL_DEFINE_AUDIO_FORMAT(1, 1, 1, 32),  }

type
 PPSDL_AudioStream = ^PSDL_AudioStream;
 PSDL_AudioStream  = type Pointer;

type
 PSDL_AudioDeviceID = ^TSDL_AudioDeviceID;
 TSDL_AudioDeviceID = type DWORD;

const
  SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK  = TSDL_AudioDeviceID($FFFFFFFF);
  SDL_AUDIO_DEVICE_DEFAULT_RECORDING = TSDL_AudioDeviceID($FFFFFFFE);

type
  PPSDL_AudioSpec = ^PSDL_AudioSpec;
  PSDL_AudioSpec = ^TSDL_AudioSpec;
  TSDL_AudioSpec = record
   format  : Integer; {*< Audio data format  }
   channels: Integer; {*< Number of channels: 1 mono, 2 stereo, etc  }
   freq    : Integer; {*< sample rate: sample frames per second  }
  end;

type
 TSDL3Audio=class
  Destructor Destroy; override;
  var
   SDL_GetNumAudioDrivers           :function  ():Integer; cdecl;
   SDL_GetAudioDriver               :function  (index: Integer): PAnsiChar; cdecl;
   SDL_GetCurrentAudioDriver        :function  ():PAnsiChar; cdecl;

   SDL_GetAudioPlaybackDevices      :function  (count: pInteger): PSDL_AudioDeviceID; cdecl;
   SDL_GetAudioRecordingDevices     :function  (count: pInteger): PSDL_AudioDeviceID; cdecl;
   SDL_GetAudioDeviceName           :function  (devid: TSDL_AudioDeviceID): PAnsiChar; cdecl;

   SDL_OpenAudioDevice              :function  (devid: TSDL_AudioDeviceID; spec: PSDL_AudioSpec): TSDL_AudioDeviceID; cdecl;
   SDL_CloseAudioDevice             :procedure (devid: TSDL_AudioDeviceID); cdecl;
   SDL_PauseAudioDevice             :function  (devid: TSDL_AudioDeviceID): Boolean; cdecl;
   SDL_ResumeAudioDevice            :function  (devid: TSDL_AudioDeviceID): Boolean; cdecl;
   SDL_AudioDevicePaused            :function  (devid: TSDL_AudioDeviceID): Boolean; cdecl;
   SDL_GetAudioDeviceFormat         :function  (devid: TSDL_AudioDeviceID; spec: PSDL_AudioSpec; sample_frames: pInteger): Boolean; cdecl;
   SDL_GetAudioDeviceGain           :function  (devid: TSDL_AudioDeviceID): Single; cdecl;
   SDL_SetAudioDeviceGain           :function  (devid: TSDL_AudioDeviceID; gain: Single): Boolean; cdecl;
   SDL_GetAudioStreamDevice         :function  (stream: PSDL_AudioStream): TSDL_AudioDeviceID; cdecl;

   SDL_BindAudioStream              :function  (devid: TSDL_AudioDeviceID; stream: PSDL_AudioStream): Boolean; cdecl;
   SDL_UnbindAudioStream            :procedure (stream: PSDL_AudioStream); cdecl;
   SDL_CreateAudioStream            :function  (src_spec: PSDL_AudioSpec; dst_spec: PSDL_AudioSpec): PSDL_AudioStream; cdecl;
   SDL_DestroyAudioStream           :procedure (stream: PSDL_AudioStream); cdecl;
   SDL_SetAudioStreamInputChannelMap:function  (stream: PSDL_AudioStream; chmap: pInteger; count: Integer): Boolean; cdecl;
   SDL_PutAudioStreamData           :function  (stream: PSDL_AudioStream; buf: Pointer; len: Integer): Boolean; cdecl;
   SDL_GetAudioStreamQueued         :function  (stream: PSDL_AudioStream): Integer; cdecl;
 end;

function _SDL_InitAudio():TSDL3Audio;

implementation

var
 lib_handle:TLibHandle=NilHandle;

 //
 //function SDL_SetHint(name: PAnsiChar; value: PAnsiChar): Boolean; cdecl;
 //function SDL_SetHintWithPriority(name: PAnsiChar; value: PAnsiChar; priority: TSDL_HintPriority): Boolean; cdecl;
 _SDL_Init         :function (flags: Integer): Boolean; cdecl;
 _SDL_InitSubSystem:function (flags: Integer): Boolean; cdecl;
 _SDL_QuitSubSystem:procedure(flags: Integer); cdecl;
 _SDL_WasInit      :function (flags: Integer): Integer; cdecl;
 _SDL_free         :procedure(mem: Pointer); cdecl;
 _SDL_PollEvent    :function (event: PSDL_Event): Boolean; cdecl;
 _SDL_WaitEvent    :function (event: PSDL_Event): Boolean; cdecl;
 //

function SDL_WasInit(flags: Integer): Integer;
begin
 Result:=0;
 if (_SDL_WasInit<>nil) then
 begin
  Result:=_SDL_WasInit(flags);
 end;
end;

procedure SDL_free(mem: Pointer);
begin
 if (_SDL_free<>nil) then
 begin
  _SDL_free(mem);
 end;
end;

function SDL_PollEvent(event: PSDL_Event): Boolean;
begin
 Result:=False;
 if (_SDL_PollEvent<>nil) then
 begin
  Result:=_SDL_PollEvent(event);
 end;
end;

function SDL_WaitEvent(event: PSDL_Event): Boolean;
begin
 Result:=False;
 if (_SDL_WaitEvent<>nil) then
 begin
  Result:=_SDL_WaitEvent(event);
 end;
end;

function SDL_InitSubSystem(flags: Integer): Boolean;
var
 first_init:Boolean;
begin
 first_init:=False;
 if (lib_handle=NilHandle) then
 begin
  lib_handle:=SafeLoadLibrary(SDL_LibName);
  if (lib_handle=NilHandle) then Exit(False);
  first_init:=True;
 end;

 if first_init then
 begin
  Pointer(_SDL_Init)         :=GetProcedureAddress(lib_handle,'SDL_Init');
  Pointer(_SDL_InitSubSystem):=GetProcedureAddress(lib_handle,'SDL_InitSubSystem');
  Pointer(_SDL_QuitSubSystem):=GetProcedureAddress(lib_handle,'SDL_QuitSubSystem');
  Pointer(_SDL_WasInit)      :=GetProcedureAddress(lib_handle,'SDL_WasInit');
  Pointer(_SDL_free)         :=GetProcedureAddress(lib_handle,'SDL_free');
  Pointer(_SDL_PollEvent)    :=GetProcedureAddress(lib_handle,'SDL_PollEvent');
  Pointer(_SDL_WaitEvent)    :=GetProcedureAddress(lib_handle,'SDL_WaitEvent');
 end;

 Result:=False;

 if first_init then
 begin
  if (_SDL_Init<>nil) then
  begin
   Result:=_SDL_Init(flags);
  end;
 end else
 begin
  if (_SDL_InitSubSystem<>nil) then
  begin
   Result:=_SDL_InitSubSystem(flags);
  end;
 end;

 if not Result then
 begin
  UnloadLibrary(lib_handle);
  lib_handle:=NilHandle;
  Exit;
 end;

 //.....
end;

procedure SDL_QuitSubSystem(flags: Integer);
var
 i:Integer;
begin
 if (lib_handle=NilHandle) then Exit;

 if (_SDL_QuitSubSystem<>nil) then
 begin
  _SDL_QuitSubSystem(flags);
 end;

 i:=SDL_WasInit(SDL_INIT_AUDIO    or
                SDL_INIT_VIDEO    or
                SDL_INIT_JOYSTICK or
                SDL_INIT_HAPTIC   or
                SDL_INIT_GAMEPAD  or
                SDL_INIT_EVENTS   or
                SDL_INIT_SENSOR   or
                SDL_INIT_CAMERA
               );

 if (i=0) then
 begin
  UnloadLibrary(lib_handle);
  lib_handle:=NilHandle;

  Pointer(_SDL_Init)         :=nil;
  Pointer(_SDL_InitSubSystem):=nil;
  Pointer(_SDL_QuitSubSystem):=nil;
  Pointer(_SDL_WasInit)      :=nil;
  Pointer(_SDL_free)         :=nil;
  Pointer(_SDL_PollEvent)    :=nil;
  Pointer(_SDL_WaitEvent)    :=nil;
 end;

 //.....
end;

//

Destructor TSDL3Audio.Destroy;
begin
 SDL_QuitSubSystem(SDL_INIT_AUDIO);
 inherited;
end;

function _SDL_InitAudio():TSDL3Audio;
begin
 Result:=nil;
 if SDL_InitSubSystem(SDL_INIT_AUDIO) then
 begin
  Result:=TSDL3Audio.Create;

  Pointer(Result.SDL_GetNumAudioDrivers           ):=GetProcedureAddress(lib_handle,'SDL_GetNumAudioDrivers');
  Pointer(Result.SDL_GetAudioDriver               ):=GetProcedureAddress(lib_handle,'SDL_GetAudioDriver');
  Pointer(Result.SDL_GetCurrentAudioDriver        ):=GetProcedureAddress(lib_handle,'SDL_GetCurrentAudioDriver');

  Pointer(Result.SDL_GetAudioPlaybackDevices      ):=GetProcedureAddress(lib_handle,'SDL_GetAudioPlaybackDevices');
  Pointer(Result.SDL_GetAudioRecordingDevices     ):=GetProcedureAddress(lib_handle,'SDL_GetAudioRecordingDevices');
  Pointer(Result.SDL_GetAudioDeviceName           ):=GetProcedureAddress(lib_handle,'SDL_GetAudioDeviceName');

  Pointer(Result.SDL_OpenAudioDevice              ):=GetProcedureAddress(lib_handle,'SDL_OpenAudioDevice');
  Pointer(Result.SDL_CloseAudioDevice             ):=GetProcedureAddress(lib_handle,'SDL_CloseAudioDevice');
  Pointer(Result.SDL_PauseAudioDevice             ):=GetProcedureAddress(lib_handle,'SDL_PauseAudioDevice');
  Pointer(Result.SDL_ResumeAudioDevice            ):=GetProcedureAddress(lib_handle,'SDL_ResumeAudioDevice');
  Pointer(Result.SDL_AudioDevicePaused            ):=GetProcedureAddress(lib_handle,'SDL_AudioDevicePaused');
  Pointer(Result.SDL_GetAudioDeviceFormat         ):=GetProcedureAddress(lib_handle,'SDL_GetAudioDeviceFormat');
  Pointer(Result.SDL_GetAudioDeviceGain           ):=GetProcedureAddress(lib_handle,'SDL_GetAudioDeviceGain');
  Pointer(Result.SDL_SetAudioDeviceGain           ):=GetProcedureAddress(lib_handle,'SDL_SetAudioDeviceGain');
  Pointer(Result.SDL_GetAudioStreamDevice         ):=GetProcedureAddress(lib_handle,'SDL_GetAudioStreamDevice');

  Pointer(Result.SDL_BindAudioStream              ):=GetProcedureAddress(lib_handle,'SDL_BindAudioStream');
  Pointer(Result.SDL_UnbindAudioStream            ):=GetProcedureAddress(lib_handle,'SDL_UnbindAudioStream');
  Pointer(Result.SDL_CreateAudioStream            ):=GetProcedureAddress(lib_handle,'SDL_CreateAudioStream');
  Pointer(Result.SDL_DestroyAudioStream           ):=GetProcedureAddress(lib_handle,'SDL_DestroyAudioStream');
  Pointer(Result.SDL_SetAudioStreamInputChannelMap):=GetProcedureAddress(lib_handle,'SDL_SetAudioStreamInputChannelMap');
  Pointer(Result.SDL_PutAudioStreamData           ):=GetProcedureAddress(lib_handle,'SDL_PutAudioStreamData');
  Pointer(Result.SDL_GetAudioStreamQueued         ):=GetProcedureAddress(lib_handle,'SDL_GetAudioStreamQueued');
 end;
end;


end.

