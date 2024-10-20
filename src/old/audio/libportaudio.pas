{ Header for libportaudio

  Copyright (C) 2021 Red_prig

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.
}

unit libportaudio;

{$mode objfpc}{$H+}
{$PACKRECORDS C}

interface

uses
 CTypes;

{$ifdef USE_STATIC_PORTAUDIO}
 {$Linklib libportaudio.a, static}
 {$IFDEF unix}
  {$Linklib libasound}
  {$Linklib libpthread}
  {$Linklib libm}
  {$Linklib libc}
 {$ENDIF}
 {$IFDEF windows}
  {$Linklib libkernel32}
  {$Linklib libwinmm}
  {$Linklib libmsvcrt}
 {$ENDIF}
{$endif}

Const
{$IFDEF unix}
 {$IFDEF darwin}
  libname='libportaudio.2.dylib';
 {$ELSE}
  libname='libportaudio.so.2';
 {$ENDIF}
 {$ELSE}
  libname='libportaudio-2.dll';
{$ENDIF}

type
  PaError = CInt32;
  PaErrorCode =(
    paNotInitialized := -10000,
    paUnanticipatedHostError,
    paInvalidChannelCount,
    paInvalidSampleRate,
    paInvalidDevice,
    paInvalidFlag,
    paSampleFormatNotSupported,
    paBadIODeviceCombination,
    paInsufficientMemory,
    paBufferTooBig,
    paBufferTooSmall,
    paNullCallback,
    paBadStreamPtr,
    paTimedOut,
    paInternalError,
    paDeviceUnavailable,
    paIncompatibleHostApiSpecificStreamInfo,
    paStreamIsStopped,
    paStreamIsNotStopped,
    paInputOverflowed,
    paOutputUnderflowed,
    paHostApiNotFound,
    paInvalidHostApi,
    paCanNotReadFromACallbackStream,
    paCanNotWriteToACallbackStream,
    paCanNotReadFromAnOutputOnlyStream,
    paCanNotWriteToAnInputOnlyStream,
    paIncompatibleStreamHostApi,
    paBadBufferPtr,
    paNoError := 0
  );

  PaDeviceIndex = CInt32;

  PaHostApiIndex = CInt32;

  PaHostApiTypeId =(paInDevelopment := 0,
    paDirectSound := 1,
    paMME := 2,
    paASIO := 3,
    paSoundManager := 4,
    paCoreAudio := 5,
    paOSS := 7,
    paALSA := 8,
    paAL := 9,
    paBeOS := 10,
    paWDMKS := 11,
    paJACK := 12,
    paWASAPI := 13,
    paAudioScienceHPI := 14
  );

  PaHostApiInfo = record
    structVersion : CInt32;
    _type : PaHostApiTypeId ;
    _name : Pchar;
    deviceCount : CInt32;
    defaultInputDevice : PaDeviceIndex;
    defaultOutputDevice : PaDeviceIndex;
  end;
  PPaHostApiInfo = ^PaHostApiInfo;

  PaHostErrorInfo = record
    hostApiType : PaHostApiTypeId;
    errorCode : CLong;
    errorText : PChar;
  end;
  PPaHostErrorInfo = ^PaHostErrorInfo;

  PaTime = CDouble;

  PaSampleFormat = pCULongLong;

  PaDeviceInfo = record
    structVersion : CInt32;
    _name : PChar;
    hostApi : PaHostApiIndex;
    maxInputChannels : CInt32;
    maxOutputChannels : CInt32;
    defaultLowInputLatency : PaTime;
    defaultLowOutputLatency : PaTime;
    defaultHighInputLatency : PaTime;
    defaultHighOutputLatency : PaTime;
    defaultSampleRate : CDouble;
  end;
  PPaDeviceInfo = ^PaDeviceInfo;

  PaStreamParameters = record
    device : PaDeviceIndex;
    channelCount : CInt32;
    sampleFormat : PaSampleFormat;
    suggestedLatency : PaTime;
    hostApiSpecificStreamInfo : Pointer;
  end;
  PPaStreamParameters = ^PaStreamParameters;

  PaStream = Pointer;
  PPaStream = ^PaStream;
  PPPaStream = ^PPaStream;

  PaStreamFlags = CULong;

  PaStreamCallbackTimeInfo = record
    inputBufferAdcTime : PaTime;
    currentTime : PaTime;
    outputBufferDacTime : PaTime;
  end;

  PaStreamCallbackFlags = CULong;

  PaStreamCallbackResult =(
    paContinue := 0,
    paComplete := 1,
    paAbort := 2);

  PaStreamCallback = function(
    input : Pointer;
    output : Pointer;
    frameCount : CULong;
    timeInfo : PaStreamCallbackTimeInfo;
    statusFlags : PaStreamCallbackFlags;
    userData : Pointer) : CInt32; cdecl;

  PaStreamFinishedCallback = procedure(userData : Pointer); cdecl;

  PaStreamInfo = record
    structVersion : CInt32;
    inputLatency : PaTime;
    outputLatency : PaTime;
    sampleRate : CDouble;
  end;
  PPaStreamInfo = ^PaStreamInfo;

 const
  paFormatIsSupported = 0;
  paFramesPerBufferUnspecified = 0;
  paNoDevice = PaDeviceIndex(-1);
  paUseHostApiSpecificDeviceSpecification = PaDeviceIndex(-2);
  paFloat32 = PaSampleFormat($00000001);
  paInt32 = PaSampleFormat($00000002);
  paInt24 = PaSampleFormat($00000004);
  paInt16 = PaSampleFormat($00000008);
  paInt8 = PaSampleFormat($00000010);
  paUInt8 = PaSampleFormat($00000020);
  paCustomFormat = PaSampleFormat($00010000);
  paNonInterleaved = PaSampleFormat($80000000);
  paNoFlag = PaStreamFlags(0);
  paClipOff = PaStreamFlags($00000001);
  paDitherOff = PaStreamFlags($00000002);
  paNeverDropInput = PaStreamFlags($00000004);
  paPrimeOutputBuffersUsingStreamCallback = PaStreamFlags($00000008);
  paPlatformSpecificFlags = PaStreamFlags($FFFF0000);
  paInputUnderflow = PaStreamCallbackFlags($00000001);
  paInputOverflow = PaStreamCallbackFlags($00000002);
  paOutputUnderflow = PaStreamCallbackFlags($00000004);
  paOutputOverflow = PaStreamCallbackFlags($00000008);
  paPrimingOutput = PaStreamCallbackFlags($00000010);

function Pa_GetVersion():CInt32 ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetVersionText():PChar ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetErrorText(errorCode : PaError):PChar ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_Initialize():PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_Terminate():PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetHostApiCount():PaHostApiIndex ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetDefaultHostApi():PaHostApiIndex ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetHostApiInfo(hostApi : PaHostApiIndex):PPaHostApiInfo ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_HostApiTypeIdToHostApiIndex(_type : PaHostApiTypeId):PaHostApiIndex ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_HostApiDeviceIndexToDeviceIndex(hostApi : PaHostApiIndex;hostApiDeviceIndex : CInt32):PaDeviceIndex ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetLastHostErrorInfo():PPaHostErrorInfo ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetDeviceCount:PaDeviceIndex ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetDefaultInputDevice:PaDeviceIndex ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetDefaultOutputDevice:PaDeviceIndex ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetDeviceInfo(device : PaDeviceIndex):PPaDeviceInfo ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_IsFormatSupported(inputParameters,outputParameters : PPaStreamParameters; sampleRate : CDouble):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};

function Pa_OpenStream(stream : PPPaStream;
                       inputParameters : PPaStreamParameters;
                       outputParameters : PPaStreamParameters;
                       sampleRate : CDouble;
                       framesPerBuffer : CULong;
                       streamFlags : PaStreamFlags;
                       streamCallback : PaStreamCallback;
                       userData : Pointer):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};

function Pa_OpenDefaultStream(stream : PPPaStream;
                              numInputChannels : CInt32;
                              numOutputChannels : CInt32;
                              sampleFormat : PaSampleFormat;
                              sampleRate : CDouble;
                              framesPerBuffer : CULong;
                              streamCallback : PaStreamCallback;
                              userData : Pointer):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};

function Pa_CloseStream(stream : PPaStream):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_SetStreamFinishedCallback(stream : PPaStream;
          streamFinishedCallback : PaStreamFinishedCallback):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_StartStream(stream : PPaStream):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_StopStream(stream : PPaStream):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_AbortStream(stream : PPaStream):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_IsStreamStopped(stream : PPaStream):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_IsStreamActive(stream : PPaStream):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetStreamInfo(stream : PPaStream):PPaStreamInfo ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetStreamTime(stream : PPaStream):Patime ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetStreamCpuLoad(stream : PPaStream):CDouble ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_ReadStream(stream : PPaStream; buffer : pcfloat ;frames : CULong):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_WriteStream(stream : PPaStream; buffer : pcfloat ;frames : CULong):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetStreamReadAvailable(stream : PPaStream):CSLong ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetStreamWriteAvailable(stream : PPaStream):CSLong ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_GetSampleSize(format : PaSampleFormat):PaError ; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};
function Pa_Sleep(msec : CLong) : integer; cdecl; external {$ifndef USE_STATIC_PORTAUDIO} libname {$endif};

implementation

{$IFNDEF PA_NOT_chkstk_ms}
procedure ___chkstk_ms; cdecl; export;
begin
end;
{$ENDIF}

{$IFNDEF PA_NOT_assert}
procedure _assert(__assertion,__file,__line:PChar); cdecl; export;
Var
 lineno:longint;
 Error:word;
begin
 if Assigned(AssertErrorProc) then
 begin
  lineno:=0;
  Error:=0;
  Val(__line,lineno,Error);
  AssertErrorProc(__assertion,__file,lineno,get_caller_addr(get_frame));
 end;
end;
{$ENDIF}

end.

