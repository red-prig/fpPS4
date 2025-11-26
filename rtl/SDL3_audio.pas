unit SDL3_audio;

{$mode ObjFPC}{$H+}

interface

uses
 SDL3,
 SDL3_main;

const
  { Audio hotplug events  }
  SDL_EVENT_AUDIO_DEVICE_ADDED          = ($1100); {*< A new audio device is available  }
  SDL_EVENT_AUDIO_DEVICE_REMOVED        = ($1101); {*< An audio device has been removed.  }
  SDL_EVENT_AUDIO_DEVICE_FORMAT_CHANGED = ($1102); {*< An audio device's format has been changed by the system.  }

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

function SDL_InitAudio():TSDL3Audio;

implementation

Destructor TSDL3Audio.Destroy;
begin
 SDL_QuitSubSystem(SDL_INIT_AUDIO);
 inherited;
end;

function _SDL_InitAudio(Data:Pointer):TSDL3Audio; register;
begin
 Result:=nil;
 if SDL_InitSubSystem(SDL_INIT_AUDIO) then
 begin
  Result:=TSDL3Audio.Create;

  Pointer(Result.SDL_GetNumAudioDrivers           ):=GetProcedureAddress(sdl3_lib_handle,'SDL_GetNumAudioDrivers');
  Pointer(Result.SDL_GetAudioDriver               ):=GetProcedureAddress(sdl3_lib_handle,'SDL_GetAudioDriver');
  Pointer(Result.SDL_GetCurrentAudioDriver        ):=GetProcedureAddress(sdl3_lib_handle,'SDL_GetCurrentAudioDriver');

  Pointer(Result.SDL_GetAudioPlaybackDevices      ):=GetProcedureAddress(sdl3_lib_handle,'SDL_GetAudioPlaybackDevices');
  Pointer(Result.SDL_GetAudioRecordingDevices     ):=GetProcedureAddress(sdl3_lib_handle,'SDL_GetAudioRecordingDevices');
  Pointer(Result.SDL_GetAudioDeviceName           ):=GetProcedureAddress(sdl3_lib_handle,'SDL_GetAudioDeviceName');

  Pointer(Result.SDL_OpenAudioDevice              ):=GetProcedureAddress(sdl3_lib_handle,'SDL_OpenAudioDevice');
  Pointer(Result.SDL_CloseAudioDevice             ):=GetProcedureAddress(sdl3_lib_handle,'SDL_CloseAudioDevice');
  Pointer(Result.SDL_PauseAudioDevice             ):=GetProcedureAddress(sdl3_lib_handle,'SDL_PauseAudioDevice');
  Pointer(Result.SDL_ResumeAudioDevice            ):=GetProcedureAddress(sdl3_lib_handle,'SDL_ResumeAudioDevice');
  Pointer(Result.SDL_AudioDevicePaused            ):=GetProcedureAddress(sdl3_lib_handle,'SDL_AudioDevicePaused');
  Pointer(Result.SDL_GetAudioDeviceFormat         ):=GetProcedureAddress(sdl3_lib_handle,'SDL_GetAudioDeviceFormat');
  Pointer(Result.SDL_GetAudioDeviceGain           ):=GetProcedureAddress(sdl3_lib_handle,'SDL_GetAudioDeviceGain');
  Pointer(Result.SDL_SetAudioDeviceGain           ):=GetProcedureAddress(sdl3_lib_handle,'SDL_SetAudioDeviceGain');
  Pointer(Result.SDL_GetAudioStreamDevice         ):=GetProcedureAddress(sdl3_lib_handle,'SDL_GetAudioStreamDevice');

  Pointer(Result.SDL_BindAudioStream              ):=GetProcedureAddress(sdl3_lib_handle,'SDL_BindAudioStream');
  Pointer(Result.SDL_UnbindAudioStream            ):=GetProcedureAddress(sdl3_lib_handle,'SDL_UnbindAudioStream');
  Pointer(Result.SDL_CreateAudioStream            ):=GetProcedureAddress(sdl3_lib_handle,'SDL_CreateAudioStream');
  Pointer(Result.SDL_DestroyAudioStream           ):=GetProcedureAddress(sdl3_lib_handle,'SDL_DestroyAudioStream');
  Pointer(Result.SDL_SetAudioStreamInputChannelMap):=GetProcedureAddress(sdl3_lib_handle,'SDL_SetAudioStreamInputChannelMap');
  Pointer(Result.SDL_PutAudioStreamData           ):=GetProcedureAddress(sdl3_lib_handle,'SDL_PutAudioStreamData');
  Pointer(Result.SDL_GetAudioStreamQueued         ):=GetProcedureAddress(sdl3_lib_handle,'SDL_GetAudioStreamQueued');
 end;
end;

function SDL_InitAudio():TSDL3Audio;
begin
 Result:=TSDL3Audio(SDL3_SendSync(TFunc(@_SDL_InitAudio),nil));
end;

end.



