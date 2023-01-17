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
  sys_signal,
  sys_path,
  sys_time,
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
 TAVPacketQueue=specialize TQueue<PAVPacket>;

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
  thisSize                  :QWORD;
  memoryReplacement         :SceAvPlayerMemAllocator;
  fileReplacement           :SceAvPlayerFileReplacement;
  eventReplacement          :SceAvPlayerEventReplacement;
  defaultLanguage           :PChar;
  debugLevel                :DWORD; //SceAvPlayerDebuglevels
  audioDecoderPriority      :DWORD;
  audioDecoderAffinity      :DWORD;
  videoDecoderPriority      :DWORD;
  videoDecoderAffinity      :DWORD;
  demuxerPriority           :DWORD;
  demuxerAffinity           :DWORD;
  controllerPriority        :DWORD;
  controllerAffinity        :DWORD;
  httpStreamingPriority     :DWORD;
  httpStreamingAffinity     :DWORD;
  fileStreamingPriority     :DWORD;
  fileStreamingAffinity     :DWORD;
  numOutputVideoFrameBuffers:Integer;
  autoStart                 :Boolean;
  reserved                  :array[0..2] of Byte;
  _align                    :DWORD;
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
  case byte of //union
   0:(reserved:array[0..15] of Byte);
   1:(audio   :SceAvPlayerAudio    );
   2:(video   :SceAvPlayerVideo    );
   3:(subs    :SceAvPlayerTimedText);
 end;

 SceAvPlayerFrameInfo=packed record
  pData    :PByte;
  reserved :DWORD;
  _align   :DWORD;
  timeStamp:QWord; //The timestamp in ms
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
  Case Byte of //union
   0:(audio   :SceAvPlayerAudioEx    );
   1:(video   :SceAvPlayerVideoEx    );
   2:(subs    :SceAvPlayerTimedTextEx);
   3:(reserved:array[0..79] of Byte  );
 end;

 SceAvPlayerFrameInfoEx=packed record
  pData    :PByte;
  reserved :DWORD;
  _align   :DWORD;
  timeStamp:QWord; //The timestamp in ms
  details  :SceAvPlayerStreamDetailsEx;
 end;
 PSceAvPlayerFrameInfoEx=^SceAvPlayerFrameInfoEx;

 PSceAvPlayerPostInitData = Pointer;

 TMemChunk=packed record
  pData:Pointer;
  fSize:Ptruint;
 end;

 TAvPlayerState=class
  formatContext       :PAVFormatContext;
  audioCodecContext   :PAVCodecContext;
  videoCodecContext   :PAVCodecContext;
  audioPackets        :TAVPacketQueue;
  videoPackets        :TAVPacketQueue;
  lastVideoTimeStamp  :QWord;
  lastAudioTimeStamp  :QWord;
  audioBuffer         :array[0..BUFFER_COUNT-1] of PSmallInt;
  videoBuffer         :array[0..BUFFER_COUNT-1] of PByte;
  videoStreamId       :Integer;
  audioStreamId       :Integer;
  channelCount,
  sampleCount,
  sampleRate          :Integer;
  source              :RawByteString; // TODO: "sceAvPlayerAddSource" indicates there may be more than 1 source per instance
  info                :Pointer; // Pointer to TAvPlayerInfo
  constructor Create;
  destructor  Destroy; override;
  procedure   CreateMedia(const aSource: RawByteString);
  procedure   FreeMedia;
  function    NextPacket(const id:Integer):Boolean;
  function    ReceiveAudio:TMemChunk;
  function    ReceiveVideo:TMemChunk;
  function    GetFramerate:QWord;
  function    IsPlaying:Boolean;
  function    Buffer(const aType:DWord;const chunk:TMemChunk):Pointer;
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
 // For now AvPlayer handle is pointer that points directly to player struct
 SceAvPlayerHandle=PAvPlayerInfo;
 PSceAvPlayerHandle=^SceAvPlayerHandle;

var
 lock:Pointer;

function GetTimeInUs:QWord; inline;
begin
 Result:=SwGetTimeUsec;
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
 Writeln(SysLogPrefix,source);
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
  Writeln(SysLogPrefix,Format('%d) Video codec: %s, resolution: %d x %d',[videoStreamId,videoCodec^.long_name,videoStream^.codecpar^.width,videoStream^.codecpar^.height]));
 end;
 if audioStreamId>=0 then
 begin
  audioStream:=formatContext^.streams[audioStreamId];
  audioCodec:=avcodec_find_decoder(audioStream^.codecpar^.codec_id);
  audioCodecContext:=avcodec_alloc_context3(audioCodec);
  avcodec_parameters_to_context(audioCodecContext,audioStream^.codecpar);
  avcodec_open2(audioCodecContext,audioCodec,nil);
  Writeln(SysLogPrefix,Format('%d) Audio codec: %s, channels: %d, sample rate: %d',[audioStreamId,audioCodec^.long_name,audioStream^.codecpar^.channels,audioStream^.codecpar^.sample_rate]));
 end;

 audioPackets:=TAVPacketQueue.Create;
 videoPackets:=TAVPacketQueue.Create;
end;

procedure TAvPlayerState.FreeMedia;
var
 packet    :PAVPacket;
 I         :Integer;
 playerInfo:PAvPlayerInfo;
begin
 if formatContext=nil then
  Exit;

 playerInfo:=info;
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
  begin
   FreeMem(audioBuffer[I]);
  end;
  if videoBuffer[I]<>nil then
   playerInfo^.memoryReplacement.deallocateTexture(playerInfo^.memoryReplacement.objectPointer,videoBuffer[I]);
 end;
 source:='';
 formatContext:=nil;
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

function TAvPlayerState.ReceiveAudio:TMemChunk;
var
 err      :Integer;
 frame    :PAVFrame;
 i, j     :Integer;
 fdata    :PSingle;
 pcmSample:SmallInt;
begin
 Result:=Default(TMemChunk);
 if (audioStreamId<0) or (not IsPlaying) then Exit;
 frame:=av_frame_alloc;
 Result.pData:=nil;
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
  lastAudioTimeStamp:=av_rescale_q(frame^.best_effort_timestamp,formatContext^.streams[audioStreamId]^.time_base,AV_TIME_BASE_Q);
  channelCount:=frame^.channels;
  sampleCount:=frame^.nb_samples;
  sampleRate:=frame^.sample_rate;
  Result.fSize:=sampleCount*channelCount*SizeOf(SmallInt);
  GetMem(Result.pData,Result.fSize);
  for i:=0 to sampleCount-1 do
   for j:=0 to channelCount-1 do
   begin
    fdata:=PSingle(frame^.data[j]);
    pcmSample:=Floor(fdata[i]*High(SmallInt));
    PSmallInt(Result.pData)[i*channelCount+j]:=pcmSample;
   end;
  break;
 end;
 av_frame_free(frame);
end;

function TAvPlayerState.ReceiveVideo:TMemChunk;
var
 err       :Integer;
 frame     :PAVFrame;
 j         :Integer;
 i         :Integer;
 fdata     :PSingle;
 sample    :Single;
 pcmSamplex:Word;
 p         :PByte;
begin
 Result:=Default(TMemChunk);
 if (videoStreamId<0) or (not IsPlaying) then Exit;
 frame:=av_frame_alloc;
 Result.pData:=nil;
 while True do
 begin
  err:=avcodec_receive_frame(videoCodecContext,frame);
  if (err=AVERROR_EAGAIN) and (NextPacket(videoStreamId)) then
   continue;
  if err<>0 then
  begin
   source:='';
  end;
  //
  lastVideoTimeStamp:=av_rescale_q(frame^.best_effort_timestamp,formatContext^.streams[videoStreamId]^.time_base,AV_TIME_BASE_Q);
  Result.fSize:=videoCodecContext^.width*videoCodecContext^.height*3 div 2;
  GetMem(Result.pData,Result.fSize);

  p:=Result.pData;
  for i:=0 to frame^.height-1 do
  begin
   Move(frame^.data[0][frame^.linesize[0]*i],p[0],frame^.width);
   p:=p+frame^.width;
  end;
  for i:=0 to frame^.height div 2-1 do
  begin
   for j:=0 to frame^.width div 2-1 do
   begin
    p[0]:=frame^.data[1][frame^.linesize[1]*i+j];
    p[1]:=frame^.data[2][frame^.linesize[2]*i+j];
    p:=p+2;
   end;
  end;
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

function TAvPlayerState.IsPlaying:Boolean;
begin
 Result:=source<>'';
end;

function TAvPlayerState.Buffer(const aType:DWord;const chunk:TMemChunk):Pointer;
var
 playerInfo:PAvPlayerInfo;
begin
 playerInfo:=info;
 if aType=0 then
 begin
  if (chunk.pData<>nil) then
  begin
   if (audioBuffer[0]<>nil) then
   begin
    FreeMem(audioBuffer[0]);
   end;
   audioBuffer[0]:=chunk.pData;
  end;
  Exit(audioBuffer[0]);
 end else
 begin
  if (chunk.pData<>nil) then
  begin
   if videoBuffer[0]=nil then
   begin
    videoBuffer[0]:=playerInfo^.memoryReplacement.allocateTexture(playerInfo^.memoryReplacement.objectPointer,0,chunk.fSize);
   end;
   Move(chunk.pData^,videoBuffer[0]^,chunk.fSize);
   FreeMem(chunk.pData);
  end;
  Exit(videoBuffer[0]);
 end;
end;

function _test_mem_alloc(var m:SceAvPlayerMemAllocator):Boolean; inline;
begin
 Result:=False;
 if (m.allocate         =nil) then Exit;
 if (m.deallocate       =nil) then Exit;
 if (m.allocateTexture  =nil) then Exit;
 if (m.deallocateTexture=nil) then Exit;
 Result:=True;
end;

function _sceAvPlayerInit(pInit:PSceAvPlayerInitData):SceAvPlayerHandle;
begin
 Result:=nil;
 if (pInit=nil) then Exit;

 if not _test_mem_alloc(pInit^.memoryReplacement) then
 begin
  Writeln(SysLogPrefix,'All allocators are required for AVPlayer Initialisation.');
  Exit;
 end;

 Writeln(SysLogPrefix,'sceAvPlayerInit');

 New(Result);
 Result^.playerState:=TAvPlayerState.Create;
 Result^.playerState.info :=Result;

 Result^.memoryReplacement:=pInit^.memoryReplacement;
 Result^.eventReplacement :=pInit^.eventReplacement;
 Result^.fileReplacement  :=pInit^.fileReplacement;

 Result^.lastFrameTime    :=GetTimeInUs;
end;

function ps4_sceAvPlayerInit(pInit:PSceAvPlayerInitData):SceAvPlayerHandle; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceAvPlayerInit(pInit);
 _sig_unlock;
end;

function _sceAvPlayerInitEx(pInit:PSceAvPlayerInitDataEx;pHandle:PSceAvPlayerHandle):Integer;
var
 handle:SceAvPlayerHandle;
begin
 Result:=-1;
 if (pInit=nil) or (pHandle=nil) then Exit;

 if not _test_mem_alloc(pInit^.memoryReplacement) then
 begin
  Writeln(SysLogPrefix,'All allocators are required for AVPlayer Initialisation.');
  Exit;
 end;

 Writeln(SysLogPrefix,'sceAvPlayerInitEx');

 New(handle);
 handle^.playerState:=TAvPlayerState.Create;
 handle^.playerState.info :=handle;

 handle^.memoryReplacement:=pInit^.memoryReplacement;
 handle^.eventReplacement :=pInit^.eventReplacement;
 handle^.fileReplacement  :=pInit^.fileReplacement;

 handle^.lastFrameTime    :=GetTimeInUs;

 pHandle^:=handle;
 Result:=0;
end;

function ps4_sceAvPlayerInitEx(pInit:PSceAvPlayerInitDataEx;pHandle:PSceAvPlayerHandle):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceAvPlayerInitEx(pInit,pHandle);
 _sig_unlock;
end;

function ps4_sceAvPlayerPostInit(handle:SceAvPlayerHandle;pPostInit:PSceAvPlayerPostInitData):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if (handle=nil) or (pPostInit=nil) then Exit;
 Writeln(SysLogPrefix,'sceAvPlayerPostInit');
 Result:=0;
end;

function _sceAvPlayerAddSource(handle:SceAvPlayerHandle;argFilename:PChar):Integer;
const
 BUF_SIZE=512*1024;
var
 fileSize,
 bytesRemaining,
 offset         :QWord;
 bytesRead      :Integer;
 actualBufSize  :QWord;
 buf            :array[0..BUF_SIZE-1] of Byte;
 p              :Pointer;
 f              :THandle;
 source         :RawByteString;
begin
 Writeln(SysLogPrefix,'sceAvPlayerAddSource:',argFilename);
 spin_lock(lock);
 // With file functions provided by client
 if (handle<>nil) and (handle^.fileReplacement.open<>nil) and (handle^.fileReplacement.close<>nil)
   and (handle^.fileReplacement.readOffset<>nil) and (handle^.fileReplacement.size<>nil) then
 begin
  p:=handle^.fileReplacement.objectPointer;
  if handle^.fileReplacement.open(p,argFilename)<0 then
  begin
   spin_unlock(lock);
   Exit(-1);
  end;
  fileSize:=handle^.fileReplacement.size(p);
  if (fileSize=0) then //result is uint64
  begin
   spin_unlock(lock);
   Exit(-1);
  end;
  // Read data and write to dump directory
  // TODO: Should cache the file so it can be reused later
  CreateDir(DIRECTORY_AVPLAYER_DUMP);
  //
  source:=DIRECTORY_AVPLAYER_DUMP+'/'+ExtractFileName(argFilename);
  f:=FileCreate(source,fmOpenWrite);
  //
  bytesRemaining:=fileSize;
  offset:=0;
  while bytesRemaining>0 do
  begin
   actualBufSize:=Min(QWORD(BUF_SIZE),bytesRemaining);
   bytesRead:=handle^.fileReplacement.readOffset(p,@buf[0],offset,actualBufSize);
   if bytesRead<0 then
   begin
    handle^.fileReplacement.close(p);
    spin_unlock(lock);
    Exit(-1);
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
 // Without client-side file functions
 begin
  source:='';
  Result:=parse_filename(argFilename,source);
  if (Result=PT_FILE) then //only real files
  begin
   handle^.playerState.CreateMedia(source);
   Result:=0;
  end else
  begin
   Result:=-1;
  end;
 end;
 spin_unlock(lock);
end;

function ps4_sceAvPlayerAddSource(handle:SceAvPlayerHandle;argFilename:PChar):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceAvPlayerAddSource(handle,argFilename);
 _sig_unlock;
end;

function ps4_sceAvPlayerIsActive(handle:SceAvPlayerHandle): Boolean; SysV_ABI_CDecl;
begin
 //Writeln(SysLogPrefix,'sceAvPlayerIsActive');
 if (handle=nil) or (not handle^.playerState.IsPlaying) then
  Exit(False);
 Exit(True);
end;

function ps4_sceAvPlayerSetLooping(handle:SceAvPlayerHandle;loopFlag:Boolean):DWord; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceAvPlayerSetLooping');
 Result:=0;
 handle^.isLooped:=loopFlag;
end;

function _sceAvPlayerGetAudioData(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfo):Boolean;
var
 audioData:TMemChunk;
begin
 //Writeln(SysLogPrefix,'sceAvPlayerGetAudioData');
 Result:=False;
 if (frameInfo<>nil) and (handle<>nil) and (handle^.playerState.IsPlaying) and (not handle^.isPaused) then
 begin
  audioData:=Default(TMemChunk);
  spin_lock(lock);
  audioData:=handle^.playerState.ReceiveAudio;
  if (audioData.pData=nil) then
  begin
   spin_unlock(lock);
   Exit(False);
  end;
  frameInfo^.timeStamp:=_usec2msec(handle^.playerState.lastAudioTimeStamp);
  frameInfo^.details.audio.channelCount:=handle^.playerState.channelCount;
  frameInfo^.details.audio.sampleRate:=handle^.playerState.sampleRate;
  frameInfo^.details.audio.size:=handle^.playerState.channelCount*handle^.playerState.sampleCount*SizeOf(SmallInt);
  frameInfo^.pData:=handle^.playerState.Buffer(0,audioData);
  spin_unlock(lock);
  Result:=True;
 end;
end;

function ps4_sceAvPlayerGetAudioData(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfo):Boolean; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceAvPlayerGetAudioData(handle,frameInfo);
 _sig_unlock;
end;

function _sceAvPlayerGetVideoDataEx(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfoEx):Boolean;
var
 videoData:TMemChunk;
begin
 //Writeln(SysLogPrefix,'sceAvPlayerGetVideoDataEx');
 Result:=False;
 if (frameInfo<>nil) and (handle<>nil) and (handle^.playerState.IsPlaying) then
 begin
  videoData:=Default(TMemChunk);
  spin_lock(lock);
  if handle^.playerState.lastVideoTimeStamp<handle^.playerState.lastAudioTimestamp then
   repeat
    handle^.lastFrameTime:=GetTimeInUs;
    videoData:=handle^.playerState.ReceiveVideo;
   until handle^.playerState.lastVideoTimeStamp>=handle^.playerState.lastAudioTimeStamp; // Check to see if video catch up with current timestamp
  if (videoData.pData=nil) then
  begin
   spin_unlock(lock);
   Exit(False);
  end;
  frameInfo^.timeStamp:=_usec2msec(handle^.playerState.lastVideoTimeStamp);
  frameInfo^.details.video.width:=handle^.playerState.videoCodecContext^.width;
  frameInfo^.details.video.height:=handle^.playerState.videoCodecContext^.height;
  frameInfo^.details.video.aspectRatio:=handle^.playerState.videoCodecContext^.width/handle^.playerState.videoCodecContext^.height;
  frameInfo^.details.video.framerate:=0;
  frameInfo^.details.video.languageCode:=LANGUAGE_CODE_ENG;
  frameInfo^.pData:=handle^.playerState.Buffer(1,videoData);
  spin_unlock(lock);
  Result:=True;
 end;
end;

function ps4_sceAvPlayerGetVideoDataEx(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfoEx):Boolean; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceAvPlayerGetVideoDataEx(handle,frameInfo);
 _sig_unlock;
end;

function ps4_sceAvPlayerGetVideoData(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfo):Boolean; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceAvPlayerGetVideoData');
 // TODO: Rely on ps4_sceAvPlayerGetVideoDataEx to get the frame
 Result:=False;
end;

function _sceAvPlayerStop(handle:SceAvPlayerHandle):Integer;
begin
 Result:=-1;
 if (handle=nil) then Exit;

 Writeln(SysLogPrefix,'sceAvPlayerStop');

 handle^.playerState.FreeMedia;
 Result:=0;
end;

function ps4_sceAvPlayerStop(handle:SceAvPlayerHandle):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceAvPlayerStop(handle);
 _sig_unlock;
end;

function _sceAvPlayerClose(handle:SceAvPlayerHandle):Integer;
begin
 Result:=-1;
 if (handle=nil) then Exit;

 if (handle^.playerState<>nil) then
 begin
  handle^.playerState.Free;
 end;
 Dispose(handle);

 Result:=0;
end;

function ps4_sceAvPlayerClose(handle:SceAvPlayerHandle):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceAvPlayerClose(handle);
 _sig_unlock;
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
 lib^.set_proc($1C3D58295536EBF3,@ps4_sceAvPlayerPostInit);
 lib^.set_proc($28C7046BEAC7B08A,@ps4_sceAvPlayerAddSource);
 lib^.set_proc($51B42861AC0EB1F6,@ps4_sceAvPlayerIsActive);
 lib^.set_proc($395B61B34C467E1A,@ps4_sceAvPlayerSetLooping);
 lib^.set_proc($5A7A7539572B6609,@ps4_sceAvPlayerGetAudioData);
 lib^.set_proc($25D92C42EF2935D4,@ps4_sceAvPlayerGetVideoDataEx);
 lib^.set_proc($642D7BC37BC1E4BA,@ps4_sceAvPlayerStop);
 lib^.set_proc($3642700F32A6225C,@ps4_sceAvPlayerClose);
end;

initialization
 ps4_app.RegistredPreLoad('libSceAvPlayer.prx',@Load_libSceAvPlayer);

end.
