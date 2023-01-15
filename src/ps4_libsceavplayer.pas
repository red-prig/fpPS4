unit ps4_libSceAvPlayer;

{$mode ObjFPC}{$H+}

interface

uses
  libavcodec,
  libavdevice,
  libavformat,
  libavutil,
  libswscale,
  libswresample,
  windows,
  ps4_program,
  spinlock,
  sys_time,
  ps4_time,
  sys_pthread,
  Classes,
  SysUtils,
  Generics.Collections,
  Math;

implementation

uses
  sys_kernel;

const
 LANGUAGE_CODE_ENG:array[0..3] of Char=('E', 'N', 'G', #0);
 DIRECTORY_AVPLAYER_DUMP='avplayer_dump';
 BUFFER_COUNT=2;

type
 TAVPacketQueue = specialize TQueue<PAVPacket>;

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

 SceAvPlayerInitDataEx=packed record
  size                      :DWord;
  memoryReplacement         :SceAvPlayerMemAllocator;
  fileReplacement           :SceAvPlayerFileReplacement;
  eventReplacement          :SceAvPlayerEventReplacement;
  defaultLanguage           :PChar;
  padding                   :array[0..11] of DWord;
  numOutputVideoFrameBuffers:Integer;
  autoStart                 :Boolean;
  reserved                  :array[0..2] of Byte;
 end;
 PSceAvPlayerInitDataEx=^SceAvPlayerInitDataEx;

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

 TAvPlayerState=class
  formatContext       :PAVFormatContext;
  audioCodecContext   :PAVCodecContext;
  videoCodecContext   :PAVCodecContext;
  audioPackets        :TAVPacketQueue;
  videoPackets        :TAVPacketQueue;
  lastTimeStamp       :QWord;
  audioBuffer         :array[0..BUFFER_COUNT-1] of PSmallInt;
  videoBuffer         :array[0..BUFFER_COUNT-1] of PByte;
  videoStreamId       :Integer;
  audioStreamId       :Integer;
  channelCount,
  sampleCount,
  sampleRate          :Integer;
  source              :RawByteString;
  constructor Create;
  destructor  Destroy; override;
  procedure   CreateMedia(const aSource: RawByteString);
  procedure   FreeMedia;
  function    NextPacket(const id:Integer):Boolean;
  function    ReceiveAudio:PSmallInt;
  function    ReceiveVideo:PByte;
  function    GetFramerate:QWord;
  function    Buffer(const aType:DWord;const data:Pointer):Pointer;
 end;

 TAvPlayerInfo=record
  playerState      :TAvPlayerState;
  //
  isLooped         :Boolean;
  isPaused         :Boolean;
  lastFrameTime    :QWord;
  memoryReplacement:SceAvPlayerMemAllocator;
  fileReplacement  :SceAvPlayerFileReplacement;
  eventReplacement :SceAvPlayerEventReplacement;
 end;
 PAvPlayerInfo=^TAvPlayerInfo;
 // TODO: For now AvPlayer handle is pointer that points directly to player struct
 SceAvPlayerHandle=PAvPlayerInfo;

var
 lock:Pointer;

function GetTimeInUs:QWord; inline;
begin
 Result:=GetTickCount64*1000; // TODO: Not accurate
end;

constructor TAvPlayerState.Create;
begin
 inherited Create;
 videoStreamId:=-1;
 audioStreamId:=-1;
end;

destructor TAvPlayerState.Destroy;
begin
 FreeMedia;
 inherited;
end;

procedure TAvPlayerState.CreateMedia(const aSource: RawByteString);
var
 videoCodec :PAVCodec;
 audioCodec :PAVCodec;
 videoStream:PAVStream;
 audioStream:PAVStream;
 p          :Pointer;
begin
 FreeMedia;
 source:=aSource;
 formatContext:=avformat_alloc_context;

 avformat_open_input(formatContext,PChar(source),nil,ppAVDictionary(nil));
 Writeln(SysLogPrefix,Format('Format: %s, duration: %dms',[formatContext^.iformat^.long_name,formatContext^.duration div 1000]));
 // Print some useful information about media

 videoStreamId:=av_find_best_stream(formatContext,AVMEDIA_TYPE_VIDEO,-1,-1,p,0);
 audioStreamId:=av_find_best_stream(formatContext,AVMEDIA_TYPE_AUDIO,-1,-1,p,0);
 if videoStreamId>=0 then
 begin
  videoStream:=formatContext^.streams[videoStreamId];
  videoCodec:=avcodec_find_decoder(videoStream^.codecpar^.codec_id);
  videoCodecContext:=avcodec_alloc_context3(videoCodec);
  avcodec_parameters_to_context(videoCodecContext,videoStream^.codecpar);
  avcodec_open2(videoCodecContext,videoCodec,nil);
  Writeln(SysLogPrefix,Format('%d) Video codec: %s, resolution: %d x %d',[videoStreamId,videoCodec^.name,videoStream^.codecpar^.width,videoStream^.codecpar^.height]));
 end;
 if audioStreamId>=0 then
 begin
  audioStream:=formatContext^.streams[audioStreamId];
  audioCodec:=avcodec_find_decoder(audioStream^.codecpar^.codec_id);
  audioCodecContext:=avcodec_alloc_context3(audioCodec);
  avcodec_parameters_to_context(audioCodecContext,audioStream^.codecpar);
  avcodec_open2(audioCodecContext,audioCodec,nil);
  Writeln(SysLogPrefix,Format('%d) Audio codec: %s, channels: %d, sample rate: %d',[audioStreamId,audioCodec^.name,audioStream^.codecpar^.channels,audioStream^.codecpar^.sample_rate]));
 end;

 audioPackets:=TAVPacketQueue.Create;
 videoPackets:=TAVPacketQueue.Create;
end;

procedure TAvPlayerState.FreeMedia;
var
 packet:PAVPacket;
 I     :Integer;
begin
 if formatContext=nil then
  Exit;

 while audioPackets.Count>0 do
 begin
  packet:=audioPackets.Dequeue;
  av_packet_free(packet);
 end;
 while videoPackets.Count>0 do
 begin
  packet:=videoPackets.Dequeue;
  av_packet_free(packet);
 end;
 audioPackets.Free;
 videoPackets.Free;

 if videoCodecContext<>nil then
 begin
  avcodec_close(audioCodecContext);
  avcodec_free_context(audioCodecContext);
 end;
 if audioCodecContext<>nil then
 begin
  avcodec_close(videoCodecContext);
  avcodec_free_context(videoCodecContext);
 end;

 avformat_close_input(formatContext);
 for I:=0 to BUFFER_COUNT-1 do
 begin
  if audioBuffer[I]<>nil then
   FreeMem(audioBuffer[I]);
  if videoBuffer[I]<>nil then
   FreeMem(videoBuffer[I]);
 end;
end;

function TAvPlayerState.NextPacket(const id:Integer):Boolean;
var
 thisQueue,
 thatQueue:TAvPacketQueue;
 packet   :PAVPacket;
 err      :Integer;
begin
 if id=videoStreamId then
 begin
  thisQueue:=videoPackets;
  thatQueue:=audioPackets;
 end else
 begin
  thisQueue:=audioPackets;
  thatQueue:=videoPackets;
 end;
 while True do
 begin
  if thisQueue.Count>0 then
  begin
   packet:=thisQueue.Dequeue;
   if id=videoStreamId then
   begin
    err:=avcodec_send_packet(videoCodecContext,packet);
    assert(err=0);
   end else
   begin
    err:=avcodec_send_packet(audioCodecContext,packet);
    assert(err=0);
   end;
   av_packet_free(packet);
   Exit(True);
  end;
  packet:=av_packet_alloc;
  if av_read_frame(formatContext,packet)<>0 then
  begin
   Exit(False);
  end;
  if id=packet^.stream_index then
   thisQueue.Enqueue(packet)
  else
   thatQueue.Enqueue(packet)
 end;
end;

function TAvPlayerState.ReceiveAudio:PSmallInt;
var
 err      :Integer;
 frame    :PAVFrame;
 i, j     :Integer;
 fdata    :PSingle;
 pcmSample:SmallInt;
begin
 if (audioStreamId<0) or (source='') then
  Exit(nil);
 frame:=av_frame_alloc;
 Result:=nil;
 while True do
 begin
  err:=avcodec_receive_frame(audioCodecContext,frame);
  if (err=AVERROR_EAGAIN) and (NextPacket(audioStreamId)) then
   continue;
  if err<>0 then
  begin
   source:='';
   break;
  end;
  //
  if frame^.format<>Integer(AV_SAMPLE_FMT_FLTP) then
   Writeln('Unknown audio format: ',frame^.format);
  channelCount:=frame^.channels;
  sampleCount:=frame^.nb_samples;
  sampleRate:=frame^.sample_rate;
  GetMem(Result,sampleCount*channelCount*SizeOf(SmallInt));
  for i:=0 to sampleCount-1 do
   for j:=0 to channelCount-1 do
   begin
    fdata:=PSingle(frame^.data[j]);
    pcmSample:=Floor(fdata[i]*High(SmallInt));
    Result[i*channelCount+j]:=pcmSample;
   end;
  break;
 end;
 av_frame_free(frame);
end;

function TAvPlayerState.ReceiveVideo:PByte;
var
 size      :Integer;
 err       :Integer;
 frame     :PAVFrame;
 i, j      :Integer;
 fdata     :PSingle;
 sample    :Single;
 pcmSamplex:Word;
begin
 if (audioStreamId<0) or (source='') then
  Exit(nil);
 frame:=av_frame_alloc;
 while True do
 begin
  err:=avcodec_receive_frame(audioCodecContext,frame);
  if (err=EAGAIN) and (NextPacket(audioStreamId)) then
   continue;
  if err<>0 then
  begin
   source:='';
  end;
  //
  lastTimeStamp:=frame^.best_effort_timestamp;
  size:=videoCodecContext^.width*videoCodecContext^.height*SizeOf(DWord);
  GetMem(Result,size);
  Move(frame^.data[0],Result[0],size);
  // TODO: yuv to rgb
  break;
 end;
 av_frame_free(frame);
end;

function TAvPlayerState.GetFramerate:QWord;
var
 rational:AVRational;
begin
 rational:=formatContext^.streams[videoStreamId]^.avg_frame_rate;
 Result:=Round(rational.den/rational.num * 1000000);
end;

function TAvPlayerState.Buffer(const aType:DWord;const data:Pointer):Pointer;
begin
 if aType=0 then
 begin
  if data<>nil then
  begin
   if audioBuffer[0]<>nil then
    FreeMem(audioBuffer[0]);
   audioBuffer[0]:=data;
  end;
  Exit(audioBuffer[0]);
 end else
 begin
  if data<>nil then
  begin
   if videoBuffer[0]<>nil then
    FreeMem(videoBuffer[0]);
   videoBuffer[0]:=data;
  end;
  Exit(videoBuffer[0]);
 end;
end;

function ps4_sceAvPlayerInit(pInit:PSceAvPlayerInitData):SceAvPlayerHandle; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceAvPlayerInit');
 New(Result);
 Result^.playerState:=TAvPlayerState.Create;
 Result^.memoryReplacement:=pInit^.memoryReplacement;
 Result^.eventReplacement:=pInit^.eventReplacement;
 Result^.fileReplacement:=pInit^.fileReplacement;
 Result^.lastFrameTime:=GetTimeInUs;
end;

function ps4_sceAvPlayerInitEx(pInit:PSceAvPlayerInitDataEx):SceAvPlayerHandle; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceAvPlayerInitEx');
 New(Result);
 Result^.playerState:=TAvPlayerState.Create;
 Result^.memoryReplacement:=pInit^.memoryReplacement;
 Result^.eventReplacement:=pInit^.eventReplacement;
 Result^.fileReplacement:=pInit^.fileReplacement;
end;

function ps4_sceAvPlayerAddSource(handle:SceAvPlayerHandle;argFilename:PChar):Integer; SysV_ABI_CDecl;
const
 BUF_SIZE = 512*1024;
var
 fileSize,
 bytesRemaining,
 offset,
 bytesRead,
 actualBufSize  :DWord;
 buf            :array[0..BUF_SIZE-1] of Byte;
 p              :Pointer;
 f              :THandle;
 source         :RawByteString;
begin
 Writeln(SysLogPrefix,'sceAvPlayerAddSource');
 spin_lock(lock);
 if (handle<>nil) and (handle^.fileReplacement.open<>nil) and (handle^.fileReplacement.close<>nil)
   and (handle^.fileReplacement.readOffset<>nil) and (handle^.fileReplacement.size<>nil) then
 begin
  p:=handle^.fileReplacement.objectPointer;
  if handle^.fileReplacement.open(p, argFilename)<0 then
  begin
   Exit(-1);
   spin_unlock(lock);
  end;
  fileSize:=handle^.fileReplacement.size(p);
  if fileSize<0 then
  begin
   Exit(-1);
   spin_unlock(lock);
  end;
  // Read data and write to dump directory
  CreateDir(DIRECTORY_AVPLAYER_DUMP);
  //
  source:=DIRECTORY_AVPLAYER_DUMP+'/'+ExtractFileName(argFilename);
  f:=FileCreate(source,fmOpenWrite);
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
    spin_unlock(lock);
   end;
   FileWrite(f,buf,actualBufSize);
   Dec(bytesRemaining,actualBufSize);
   Inc(offset,actualBufSize);
  end;
  FileClose(f);
  handle^.fileReplacement.close(p);
  // Init player
  handle^.playerState.CreateMedia(source);
  Result:=0;
 end else
  Result:=-1;
 spin_unlock(lock);
end;

function ps4_sceAvPlayerIsActive(handle:SceAvPlayerHandle): Boolean SysV_ABI_CDecl;
begin
 //Writeln(SysLogPrefix,'sceAvPlayerIsActive');
 if (handle=nil) or (handle^.playerState.source='') then
  Exit(False);
 Exit(True);
end;

function ps4_sceAvPlayerSetLooping(handle:SceAvPlayerHandle;loopFlag:Boolean):DWord; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceAvPlayerSetLooping');
 Result:=0;
 handle^.isLooped:=loopFlag;
end;

function ps4_sceAvPlayerGetAudioData(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfo):Boolean; SysV_ABI_CDecl;
var
 audioData:PSmallInt=nil;
begin
 //Writeln(SysLogPrefix,'sceAvPlayerGetAudioData');
 if (frameInfo<>nil) and (handle<>nil) and (handle^.playerState.source<>'') and (not handle^.isPaused) then
 begin
  spin_lock(lock);
  audioData:=handle^.playerState.ReceiveAudio;
  if audioData=nil then
  begin
   spin_unlock(lock);
   Exit(False);
  end;
  frameInfo^.timeStamp:=handle^.playerState.lastTimeStamp;
  frameInfo^.details.audio.channelCount:=handle^.playerState.channelCount;
  frameInfo^.details.audio.sampleRate:=handle^.playerState.sampleRate;
  frameInfo^.details.audio.size:=handle^.playerState.channelCount*handle^.playerState.sampleCount*SizeOf(SmallInt);
  frameInfo^.pData:=handle^.playerState.Buffer(0,audioData);
  spin_unlock(lock);
  Result:=True;
 end else
  Result:=False;
end;

function ps4_sceAvPlayerGetVideoDataEx(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfoEx):Boolean; SysV_ABI_CDecl;
var
 currentTime:QWord;
 videoData  :PByte;
begin
 //Writeln(SysLogPrefix,'sceAvPlayerGetVideoDataEx');
 if (frameInfo<>nil) and (handle<>nil) and (handle^.playerState.source<>'') then
 begin
  spin_lock(lock);
  {if handle^.lastFrameTime+handle^.playerState.GetFramerate<GetTimeInUs then
  begin
   handle^.lastFrameTime:=GetTimeInUs;
   videoData:=handle^.playerState.ReceiveVideo;
  end;}
  frameInfo^.timeStamp:=handle^.playerState.lastTimeStamp;
  frameInfo^.details.video.width:=handle^.playerState.videoCodecContext^.width;
  frameInfo^.details.video.height:=handle^.playerState.videoCodecContext^.height;
  frameInfo^.details.video.aspectRatio:=handle^.playerState.videoCodecContext^.width/handle^.playerState.videoCodecContext^.height;
  frameInfo^.details.video.framerate:=0;
  frameInfo^.details.video.languageCode:=LANGUAGE_CODE_ENG;
  spin_unlock(lock);
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
  if handle^.playerState<>nil then
   handle^.playerState.Free;
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
 lib^.set_proc($A3D79646448BF8CE,@ps4_sceAvPlayerInitEx);
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
