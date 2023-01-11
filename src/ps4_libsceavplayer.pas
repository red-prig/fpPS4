unit ps4_libSceAvPlayer;

{$mode ObjFPC}{$H+}

interface

uses
  windows,
  ps4_program,
  sys_time,
  sys_pthread,
  Classes,
  SysUtils,
  Math;

implementation

uses
  sys_kernel;

const
 LANGUAGE_CODE_ENG:array[0..3] of Char=('E', 'N', 'G', #0);
 DIRECTORY_AVPLAYER_DUMP = 'avplayer_dump';

type
 SceAvPlayerAllocate=function(argP:Pointer;argAlignment:DWord;argSize:DWord):Pointer; SysV_ABI_CDecl;
 SceAvPlayerDeallocate=procedure(argP:Pointer;argMemory:Pointer); SysV_ABI_CDecl;
 SceAvPlayerAllocateTexture=function(argP:Pointer;argAlignment:DWord;argSize:DWord):Pointer; SysV_ABI_CDecl;
 SceAvPlayerDeallocateTexture=procedure(argP:Pointer;argMemory:Pointer); SysV_ABI_CDecl;

 SceAvPlayerOpenFile=function(argP:Pointer;argFilename:PChar):Integer; SysV_ABI_CDecl;
 SceAvPlayerCloseFile=function(argP:Pointer):Integer; SysV_ABI_CDecl;
 SceAvPlayerReadOffsetFile=function(argP:Pointer;argBuffer:PByte;argPosition:QWord;argLength:DWord):Integer; SysV_ABI_CDecl;
 SceAvPlayerSizeFile=function(argP:Pointer):QWord; SysV_ABI_CDecl;

 SceAvPlayerEventCallback=procedure(p:Pointer;argEventId:Integer;argSourceId:Integer;argEventData:Pointer); SysV_ABI_CDecl;

 SceAvPlayerMemAllocator=packed record
  objectPointer    :Pointer;
  allocate         :SceAvPlayerAllocate;
  deallocate       :SceAvPlayerDeallocate;
  allocateTexture  :SceAvPlayerAllocateTexture;
  deallocateTexture:SceAvPlayerDeallocateTexture;
 end;

 SceAvPlayerFileReplacement=packed record
  objectPointer:Pointer;
  open         :SceAvPlayerOpenFile;
  close        :SceAvPlayerCloseFile;
  readOffset   :SceAvPlayerReadOffsetFile;
  size         :SceAvPlayerSizeFile;
 end;

 SceAvPlayerEventReplacement=packed record
  objectPointer:Pointer;
  eventCallback:SceAvPlayerEventCallback;
 end;

 SceAvPlayerInitData=packed record
  memoryReplacement         :SceAvPlayerMemAllocator;
  fileReplacement           :SceAvPlayerFileReplacement;
  eventReplacement          :SceAvPlayerEventReplacement;
  debugLevel                :DWord;
  basePriority              :DWord;
  numOutputVideoFrameBuffers:Integer;
  autoStart                 :Boolean;
  reserved                  :array[0..2] of Byte;
  defaultLanguage           :PChar;
 end;
 PSceAvPlayerInitData=^SceAvPlayerInitData;

 SceAvPlayerAudio=packed record
  channelCount:Word;
  reserved    :array[0..1] of Byte;
  sampleRate  :DWord;
  size        :DWord;
  languageCode:array[0..3] of Char;
 end;

 SceAvPlayerVideo=packed record
  width       :DWord;
  height      :DWord;
  aspectRatio :Single;
  languageCode:array[0..3] of Char;
 end;

 SceAvPlayerTextPosition=packed record
  top   :Word;
  left  :Word;
  bottom:Word;
  right :Word;
 end;

 SceAvPlayerTimedText=packed record
  languageCode:array[0..3] of Char;
  textSize    :Word;
  fontSize    :Word;
  position    :SceAvPlayerTextPosition;
 end;

 SceAvPlayerStreamDetails=packed record
  reserved:array[0..15] of Byte;
  audio   :SceAvPlayerAudio;
  video   :SceAvPlayerVideo;
  subs    :SceAvPlayerTimedText;
 end;

 SceAvPlayerFrameInfo=packed record
  pData    :PByte;
  reserved :array[0..3] of Byte;
  timeStamp:QWord;
  details  :SceAvPlayerStreamDetails;
 end;
 PSceAvPlayerFrameInfo=^SceAvPlayerFrameInfo;

 SceAvPlayerAudioEx=packed record
  channelCount:Word;
  reserved    :array[0..1] of Byte;
  sampleRate  :DWord;
  size        :DWord;
  languageCode:array[0..3] of Char;
  reserved1   :array[0..63] of Byte;
 end;

 SceAvPlayerVideoEx=packed record
  width             :DWord;
  height            :DWord;
  aspectRatio       :Single;
  languageCode      :array[0..3] of Char;
  framerate         :DWord;
  cropLeftOffset    :DWord;
  cropRightOffset   :DWord;
  cropTopOffset     :DWord;
  cropBottomOffset  :DWord;
  pitch             :DWord;
  lumaBitDepth      :Byte;
  chromaBitDepth    :Byte;
  videoFullRangeFlag:Boolean;
  reserved          :array[0..36] of Byte;
 end;

 SceAvPlayerTimedTextEx=packed record
  languageCode:array[0..3] of Char;
  reserved    :array[0..75] of Byte;
 end;

 SceAvPlayerStreamDetailsEx=packed record
  audio   :SceAvPlayerAudioEx;
  video   :SceAvPlayerVideoEx;
  subs    :SceAvPlayerTimedTextEx;
  reserved:array[0..79] of Byte;
 end;

 SceAvPlayerFrameInfoEx=packed record
  pData    :PByte;
  reserved :array[0..3] of Byte;
  timeStamp:QWord;
  details  :SceAvPlayerStreamDetailsEx;
 end;
 PSceAvPlayerFrameInfoEx=^SceAvPlayerFrameInfoEx;

 TAvPlayerInfo=record
  isLooped         :Boolean;
  isPaused         :Boolean;
  lastFrameTime    :QWord;
  lastTimeStamp    :QWord;
  memoryReplacement:SceAvPlayerMemAllocator;
  fileReplacement  :SceAvPlayerFileReplacement;
  eventReplacement :SceAvPlayerEventReplacement;
  audioBuffer      :PByte;
  videoBuffer      :PByte;
  source           :RawByteString;
 end;
 PAvPlayerInfo=^TAvPlayerInfo;
 // TODO: For now AvPlayer handle is pointer that points directly to player struct
 SceAvPlayerHandle=PAvPlayerInfo;

function GetTimeInMs:QWord; inline;
begin
 Result:=GetTickCount64;
end;

function ps4_sceAvPlayerInit(pInit:PSceAvPlayerInitData):SceAvPlayerHandle; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceAvPlayerInit');
 New(Result);
 Result^.lastTimeStamp:=0;
 Result^.lastFrameTime:=GetTimeInMs;
 Result^.memoryReplacement:=pInit^.memoryReplacement;
 Result^.eventReplacement:=pInit^.eventReplacement;
 Result^.fileReplacement:=pInit^.fileReplacement;
 // TODO: Dummy values
 Result^.videoBuffer:=AllocMem(640*360*4);
 Result^.audioBuffer:=AllocMem(44100);
end;

function ps4_sceAvPlayerAddSource(handle:SceAvPlayerHandle;argFilename:PChar):Integer; SysV_ABI_CDecl;
const
 BUF_SIZE = 512*1024;
var
 fileSize,
 bytesRemaining,
 offset,
 bytesRead,
 actualBufSize:DWord;
 buf          :array[0..BUF_SIZE-1] of Byte;
 p            :Pointer;
 f            :THandle;
begin
 Writeln(SysLogPrefix,'sceAvPlayerAddSource');
 if (handle<>nil) and (handle^.fileReplacement.open<>nil) and (handle^.fileReplacement.close<>nil)
   and (handle^.fileReplacement.readOffset<>nil) and (handle^.fileReplacement.size<>nil) then
 begin
  p:=handle^.fileReplacement.objectPointer;
  if handle^.fileReplacement.open(p, argFilename)<0 then
   Exit(-1);
  fileSize:=handle^.fileReplacement.size(p);
  if fileSize<0 then
   Exit(-1);
  // Read data and write to dump directory
  CreateDir(DIRECTORY_AVPLAYER_DUMP);
  //
  handle^.source:=DIRECTORY_AVPLAYER_DUMP+'/'+ExtractFileName(argFilename);
  f:=FileCreate(handle^.source,fmOpenWrite);
  //
  bytesRemaining:=fileSize;
  offset:=0;
  while bytesRemaining>0 do
  begin
   actualBufSize:=Min(BUF_SIZE,bytesRemaining);
   bytesRead:=handle^.fileReplacement.readOffset(p,@buf[0],offset,actualBufSize);
   if bytesRead<0 then
   begin
    handle^.fileReplacement.close(p);
    Exit(-1);
   end;
   FileWrite(f,buf,actualBufSize);
   Dec(bytesRemaining,actualBufSize);
   Inc(offset,actualBufSize);
  end;
  FileClose(f);
  handle^.fileReplacement.close(p);
  // TODO: prepare for playback should be here
  Result:=0;
 end else
  Result:=-1;
end;

function ps4_sceAvPlayerIsActive(handle:SceAvPlayerHandle): Boolean SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceAvPlayerIsActive');
 if handle=nil then
  Exit(False);
 // TODO: Dummy calculation, we "stop" the video after 1s if isLooped is not set
 if (not handle^.isLooped) and (handle^.lastTimeStamp>=1000) then
 begin
  Result:=False;
 end else
  Result:=True;
end;

function ps4_sceAvPlayerSetLooping(handle:SceAvPlayerHandle;loopFlag:Boolean):DWord; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceAvPlayerSetLooping');
 Result:=0;
 handle^.isLooped:=loopFlag;
end;

function ps4_sceAvPlayerGetAudioData(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfo):Boolean; SysV_ABI_CDecl;
var
 currentTime:QWord;
begin
 Writeln(SysLogPrefix,'sceAvPlayerGetAudioData');
 // TODO: dummy data for now
 if (frameInfo<>nil) and (handle<>nil) then
 begin
  currentTime:=GetTimeInMs;
  frameInfo^.pData:=handle^.audioBuffer;
  frameInfo^.timeStamp:=frameInfo^.timeStamp + (currentTime - handle^.lastFrameTime);
  frameInfo^.details.audio.channelCount:=0;
  frameInfo^.details.audio.sampleRate:=44100;
  frameInfo^.details.audio.size:=16384;
  frameInfo^.details.audio.languageCode:=LANGUAGE_CODE_ENG;
  handle^.lastFrameTime:=currentTime;
  handle^.lastTimeStamp:=frameInfo^.timeStamp;
  Result:=True;
 end else
  Result:=False;
end;

function ps4_sceAvPlayerGetVideoDataEx(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfoEx):Boolean; SysV_ABI_CDecl;
var
 currentTime:QWord;
begin
 Writeln(SysLogPrefix,'sceAvPlayerGetVideoDataEx');
 // TODO: dummy data for now
 if (frameInfo<>nil) and (handle<>nil) then
 begin
  currentTime:=GetTimeInMs;
  frameInfo^.pData:=handle^.videoBuffer;
  frameInfo^.timeStamp:=frameInfo^.timeStamp + (currentTime - handle^.lastFrameTime);
  frameInfo^.details.video.width:=640;
  frameInfo^.details.video.height:=360;
  frameInfo^.details.video.aspectRatio:=1;
  frameInfo^.details.video.framerate:=25;
  frameInfo^.details.video.languageCode:=LANGUAGE_CODE_ENG;
  handle^.lastFrameTime:=currentTime;
  handle^.lastTimeStamp:=frameInfo^.timeStamp;
  Result:=False; // TODO: For testing purpose
 end else
  Result:=False;
end;

function ps4_sceAvPlayerGetVideoData(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfo):Boolean; SysV_ABI_CDecl;
begin
  Writeln(SysLogPrefix,'sceAvPlayerGetVideoData');
  // TODO: Rely on ps4_sceAvPlayerGetVideoDataEx to get the frame
  Result:=False;
end;

function ps4_sceAvPlayerClose(handle:SceAvPlayerHandle):Integer; SysV_ABI_CDecl;
begin
 if handle<>nil then
 begin
  if handle^.audioBuffer<>nil then
   FreeMem(handle^.audioBuffer);
  if handle^.videoBuffer<>nil then
   FreeMem(handle^.videoBuffer);
  Dispose(handle);
  Result:=0;
 end else
  Result:=-1;
end;

function Load_libSceAvPlayer(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceAvPlayer');

 lib^.set_proc($692EBA448D201A0A,@ps4_sceAvPlayerInit);
 lib^.set_proc($28C7046BEAC7B08A,@ps4_sceAvPlayerAddSource);
 lib^.set_proc($51B42861AC0EB1F6,@ps4_sceAvPlayerIsActive);
 lib^.set_proc($395B61B34C467E1A,@ps4_sceAvPlayerSetLooping);
 lib^.set_proc($5A7A7539572B6609,@ps4_sceAvPlayerGetAudioData);
 lib^.set_proc($25D92C42EF2935D4,@ps4_sceAvPlayerGetVideoDataEx);
 lib^.set_proc($3642700F32A6225C,@ps4_sceAvPlayerClose);
end;

initialization
 ps4_app.RegistredPreLoad('libSceAvPlayer.prx',@Load_libSceAvPlayer);

end.
