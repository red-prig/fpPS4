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
  hamt,
  ps4_program,
  spinlock,
  sys_types,
  sys_crt,
  sys_signal,
  sys_path,
  sys_time,
  Classes,
  SysUtils,
  Generics.Collections,
  Math;

var
  DISABLE_FMV_HACK:Boolean=False;

implementation

uses
  param_sfo,
  sys_kernel;

const
 LANGUAGE_CODE_ENG:array[0..3] of Char=('E', 'N', 'G', #0);
 DIRECTORY_AVPLAYER_DUMP='avplayer_dump';
 BUFFER_COUNT=2;

 SCE_AVPLAYER_STATE_STOP         =$01;
 SCE_AVPLAYER_STATE_READY        =$02;
 SCE_AVPLAYER_STATE_PLAY         =$03;
 SCE_AVPLAYER_STATE_PAUSE        =$04;
 SCE_AVPLAYER_STATE_BUFFERING    =$05;
 SCE_AVPLAYER_TIMED_TEXT_DELIVERY=$10;
 SCE_AVPLAYER_WARNING_ID         =$20;

 SCE_AVPLAYER_VIDEO    =0;
 SCE_AVPLAYER_AUDIO    =1;
 SCE_AVPLAYER_TIMEDTEXT=2;
 SCE_AVPLAYER_UNKNOWN  =3;

type
 TAVPacketQueue=specialize TQueue<PAVPacket>;

 SceAvPlayerAllocate         =function(argP:Pointer;argAlignment:DWord;argSize:DWord):Pointer; SysV_ABI_CDecl;
 SceAvPlayerDeallocate       =procedure(argP:Pointer;argMemory:Pointer); SysV_ABI_CDecl;
 SceAvPlayerAllocateTexture  =function(argP:Pointer;argAlignment:DWord;argSize:DWord):Pointer; SysV_ABI_CDecl;
 SceAvPlayerDeallocateTexture=procedure(argP:Pointer;argMemory:Pointer); SysV_ABI_CDecl;

 SceAvPlayerOpenFile      =function(argP:Pointer;argFilename:PChar):Integer; SysV_ABI_CDecl;
 SceAvPlayerCloseFile     =function(argP:Pointer):Integer; SysV_ABI_CDecl;
 SceAvPlayerReadOffsetFile=function(argP:Pointer;argBuffer:PByte;argPosition:QWord;argLength:DWord):Integer; SysV_ABI_CDecl;
 SceAvPlayerSizeFile      =function(argP:Pointer):QWord; SysV_ABI_CDecl;

 SceAvPlayerEventCallback=procedure(p:Pointer;argEventId:Integer;argSourceId:Integer;argEventData:Pointer); SysV_ABI_CDecl;
 SceAvPlayerLogCallback=Pointer;

 SceAvPlayerUriType=Integer; // enum
 SceAvPlayerSourceType=Integer; // enum
 SceAvPlayerAvSyncMode=Integer; // enum

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
  audioDecoderStackSize     :DWord;
  videoDecoderStackSize     :DWord;
  demuxerStackSize          :DWord;
  controllerStackSize       :DWord;
  httpStreamingStackSize    :DWord;
  fileStreamingStackSize    :DWord;
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

 PSceAvPlayerPostInitData=Pointer;

 SceAvPlayerStreamInfo=packed record
  type_    :DWord;
  align_   :Dword;
  details  :SceAvPlayerStreamDetails;
  duration :QWord;
  startTime:QWord;
 end;
 PSceAvPlayerStreamInfo=^SceAvPlayerStreamInfo;

 SceAvPlayerUri=packed record
  name  :PChar;
  length:DWord;
  _align:DWord;
 end;

 SceAvPlayerSourceDetails=packed record
  uri       :SceAvPlayerUri;
  reserved  :array[0..63] of Byte;
  sourceType:SceAvPlayerSourceType;
  reserved2 :array[0..43] of Byte;
 end;
 PSceAvPlayerSourceDetails=^SceAvPlayerSourceDetails;

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
  audioChunk,
  videoChunk          :TMemChunk;
  durationInMs        :QWord;
  streamCount         :DWord;
  videoStreamId       :Integer;
  audioStreamId       :Integer;
  channelCount,
  sampleCount,
  sampleRate          :Integer;
  source              :RawByteString;
  info                :Pointer; // Pointer to TAvPlayerInfo
  constructor Create;
  destructor  Destroy; override;
  procedure   CreateMedia(const aSource: RawByteString);
  procedure   FreeMedia;
  function    NextPacket(const id:Integer):Boolean;
  function    ReceiveAudio(const ignoreIsPlaying:Boolean=False):TMemChunk;
  function    ReceiveVideo:TMemChunk;
  function    GetFramerate:QWord;
  function    IsPlaying:Boolean;
  function    IsMediaAvailable:Boolean;
  function    Buffer(const aType:DWord;const chunk:TMemChunk):Pointer;
 end;

 SceAvPlayerHandle=QWord;
 PSceAvPlayerHandle=^SceAvPlayerHandle;

 TAvPlayerInfo=class
  handle           :SceAvPlayerHandle;
  refs             :DWORD;
  Flock            :TRTLCriticalSection;
  //
  playerState      :TAvPlayerState;
  //
  isLooped         :Boolean;
  isPaused         :Boolean;
  lastFrameTime    :QWord;
  memoryReplacement:SceAvPlayerMemAllocator;
  fileReplacement  :SceAvPlayerFileReplacement;
  eventReplacement :SceAvPlayerEventReplacement;
  //
  constructor Create;
  destructor  Destroy; override;
  //
  procedure inc_ref;
  procedure dec_ref;
  procedure lock;
  procedure unlock;
  //
  function  Allocate         (argAlignment:DWord;argSize:DWord):Pointer;
  procedure Deallocate       (argMemory:Pointer);
  function  AllocateTexture  (argAlignment:DWord;argSize:DWord):Pointer;
  procedure DeallocateTexture(argMemory:Pointer);
  //
  procedure EventCallback    (argEventId:Integer;argSourceId:Integer;argEventData:Pointer);
  //
  Function  FileReplacementProvided:Boolean;
  function  open             (argFilename:PChar):Integer;
  function  close            ():Integer;
  function  readOffset       (argBuffer:PByte;argPosition:QWord;argLength:DWord):Integer;
  function  size             ():QWord;
 end;

var
 hamt_lock   :Pointer;
 handleCount :QWord=0;
 AvHandleHamt:TSTUB_HAMT64;

function _GetTimeInUs:QWord; inline;
begin
 Result:=SwGetTimeUsec;
end;

//GetPlayer increment refs
function _GetPlayer(const handle:SceAvPlayerHandle):TAvPlayerInfo;
var
 data:PPointer;
begin
 Result:=nil;
 spin_lock(hamt_lock);
  data:=HAMT_search64(@AvHandleHamt,handle);
  if (data<>nil) then
  begin
   Result:=TAvPlayerInfo(data^);
   Result.inc_ref;
  end;
 spin_unlock(hamt_lock);
end;

function _AddPlayer(const player:TAvPlayerInfo):SceAvPlayerHandle;
var
 data:PPointer;
begin
 spin_lock(hamt_lock);
  Inc(handleCount);
  data:=HAMT_insert64(@AvHandleHamt,handleCount,player);
  Assert(data<>nil,'unexpected err');
  //force reuse
  data^:=player;
  //save handle
  player.handle:=handleCount;
  //inc refs
  player.inc_ref;
  Result:=handleCount;
 spin_unlock(hamt_lock);
end;

procedure _DeletePlayer(const handle:SceAvPlayerHandle);
var
 data:TAvPlayerInfo;
begin
 spin_lock(hamt_lock);
  HAMT_delete64(@AvHandleHamt,handle,@data);
 spin_unlock(hamt_lock);

 if (data<>nil) then
 begin
  data.dec_ref;
 end;
end;

procedure _FreeChunk(var chunk:TMemChunk);
begin
 if chunk.pAddr<>nil then
 begin
  FreeMem(chunk.pAddr);
  chunk.pAddr:=nil;
 end;
end;

procedure _CompareAndAllocChunk(var chunk:TMemChunk;const nSize:QWord);
begin
 if nSize<>chunk.nSize then
 begin
  if chunk.pAddr<>nil then
  begin
   FreeMem(chunk.pAddr);
  end;
  chunk.pAddr:=AllocMem(nSize);
  chunk.nSize:=nSize;
 end;
end;

procedure AvHandleClean(data,userdata:Pointer);
var
 handle:SceAvPlayerHandle;
 player:TAvPlayerInfo;
begin
 if (data=nil) then Exit;
 player:=TAvPlayerInfo(data);
 handle:=player.handle;
 Writeln(StdWrn,'WARNING: Leftover AvPlayer handle, let me clean it up: ', handle);
 player.dec_ref;
end;

constructor TAvPlayerInfo.Create;
begin
 InitCriticalSection(Flock);
 playerState:=TAvPlayerState.Create;
 playerState.info:=Self;
end;

destructor TAvPlayerInfo.Destroy;
begin
 DoneCriticalSection(Flock);
 FreeAndNil(playerState);
end;

procedure TAvPlayerInfo.inc_ref;
begin
 if (Self=nil) then Exit; //nil safe
 System.InterlockedIncrement(refs);
end;

procedure TAvPlayerInfo.dec_ref;
begin
 if (Self=nil) then Exit; //nil safe
 if (System.InterlockedDecrement(refs)=0) then
 begin
  //auto free
  Free;
 end;
end;

//recrusive lock
procedure TAvPlayerInfo.lock;
begin
 if (Self=nil) then Exit; //nil safe
 EnterCriticalSection(Flock);
end;

procedure TAvPlayerInfo.unlock;
begin
 if (Self=nil) then Exit; //nil safe
 LeaveCriticalSection(Flock);
end;

function TAvPlayerInfo.Allocate(argAlignment:DWord;argSize:DWord):Pointer;
begin
 Result:=memoryReplacement.allocate(memoryReplacement.objectPointer,argAlignment,argSize);
end;

procedure TAvPlayerInfo.Deallocate(argMemory:Pointer);
begin
 memoryReplacement.Deallocate(memoryReplacement.objectPointer,argMemory);
end;

function  TAvPlayerInfo.AllocateTexture(argAlignment:DWord;argSize:DWord):Pointer;
begin
 Result:=memoryReplacement.AllocateTexture(memoryReplacement.objectPointer,argAlignment,argSize);
end;

procedure TAvPlayerInfo.DeallocateTexture(argMemory:Pointer);
begin
 memoryReplacement.DeallocateTexture(memoryReplacement.objectPointer,argMemory);
end;

//

procedure TAvPlayerInfo.EventCallback(argEventId:Integer;argSourceId:Integer;argEventData:Pointer);
begin
 if (eventReplacement.eventCallback<>nil) then
 begin
  Writeln(SysLogPrefix,'AvPlayerEventCallback,event=',argEventId);
  eventReplacement.eventCallback(eventReplacement.objectPointer,argEventId,argSourceId,argEventData);
 end;
end;

//

function _test_file_cbs(var m:SceAvPlayerFileReplacement):Boolean; inline;
begin
 Result:=False;
 if (m.open      =nil) then Exit;
 if (m.close     =nil) then Exit;
 if (m.readOffset=nil) then Exit;
 if (m.size      =nil) then Exit;
 Result:=True;
end;

Function TAvPlayerInfo.FileReplacementProvided:Boolean;
begin
 Result:=_test_file_cbs(fileReplacement);
end;

function TAvPlayerInfo.open(argFilename:PChar):Integer;
begin
 Result:=0;
 if (fileReplacement.open<>nil) then
 begin
  Result:=fileReplacement.open(fileReplacement.objectPointer,argFilename);
 end;
end;

function TAvPlayerInfo.close():Integer;
begin
 Result:=0;
 if (fileReplacement.close<>nil) then
 begin
  Result:=fileReplacement.close(fileReplacement.objectPointer);
 end;
end;

function TAvPlayerInfo.readOffset(argBuffer:PByte;argPosition:QWord;argLength:DWord):Integer;
begin
 Result:=0;
 if (fileReplacement.readOffset<>nil) then
 begin
  Result:=fileReplacement.readOffset(fileReplacement.objectPointer,argBuffer,argPosition,argLength);
 end;
end;

function TAvPlayerInfo.size():QWord;
begin
 Result:=0;
 if (fileReplacement.size<>nil) then
 begin
  Result:=fileReplacement.size(fileReplacement.objectPointer);
 end;
end;

procedure _AvPlayerEventCallback(const handle:SceAvPlayerHandle;const event:Integer;const eventData:Pointer);
var
 player:TAvPlayerInfo;
begin
 player:=_GetPlayer(handle);
 if (player<>nil) then
 begin
  player.EventCallback(event,0,eventData);
  player.dec_ref;
 end;
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
 if source<>'' then
  Writeln('TODO: 1 instance can have multiple media sources');
 FreeMedia;
 source:=aSource;
 formatContext:=avformat_alloc_context;

 avformat_open_input(formatContext,PChar(source),nil,ppAVDictionary(nil));
 durationInMs:=_usec2msec(formatContext^.duration);
 Writeln(SysLogPrefix,source);
 Writeln(SysLogPrefix,Format('Format: %s, duration: %dms',[formatContext^.iformat^.long_name,durationInMs]));
 // Print some useful information about media

 streamCount  :=formatContext^.nb_streams;
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
 audioChunk:=Default(TMemChunk);
 videoChunk:=Default(TMemChunk);
end;

procedure TAvPlayerState.FreeMedia;
var
 packet    :PAVPacket;
 I         :Integer;
 playerInfo:TAvPlayerInfo;
begin
 if formatContext=nil then
  Exit;

 playerInfo:=TAvPlayerInfo(info);
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
 _FreeChunk(audioChunk);
 _FreeChunk(videoChunk);

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
   playerInfo.Deallocate(audioBuffer[I]);
  if videoBuffer[I]<>nil then
   playerInfo.DeallocateTexture(videoBuffer[I]);
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
   end else
   begin
    err:=avcodec_send_packet(audioCodecContext,packet);
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

function TAvPlayerState.ReceiveAudio(const ignoreIsPlaying:Boolean=False):TMemChunk;
var
 err      :Integer;
 frame    :PAVFrame;
 i, j     :Integer;
 fdata    :PSingle;
 nsize    :QWord;
 pcmSample:SmallInt;
begin
 Result:=Default(TMemChunk);
 if (audioStreamId<0) or ((not ignoreIsPlaying) and (not IsPlaying)) then Exit;
 frame:=av_frame_alloc;
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
   Writeln(StdErr,'ERROR: Unknown audio format: ',frame^.format);
  lastAudioTimeStamp:=av_rescale_q(frame^.best_effort_timestamp,formatContext^.streams[audioStreamId]^.time_base,AV_TIME_BASE_Q);
  channelCount:=frame^.channels;
  sampleCount:=frame^.nb_samples;
  sampleRate:=frame^.sample_rate;
  nsize:=sampleCount*channelCount*SizeOf(SmallInt);
  _CompareAndAllocChunk(audioChunk,nsize);
  for i:=0 to sampleCount-1 do
   for j:=0 to channelCount-1 do
   begin
    fdata:=PSingle(frame^.data[j]);
    pcmSample:=Floor(fdata[i]*High(SmallInt));
    PSmallInt(audioChunk.pAddr)[i*channelCount+j]:=pcmSample;
   end;
  break;
 end;
 av_frame_free(frame);
 Result:=audioChunk;
end;

function TAvPlayerState.ReceiveVideo:TMemChunk;
var
 err       :Integer;
 frame     :PAVFrame;
 j         :Integer;
 i         :Integer;
 fdata     :PSingle;
 nsize     :QWord;
 sample    :Single;
 pcmSamplex:Word;
 p         :PByte;
begin
 Result:=Default(TMemChunk);
 if (videoStreamId<0) or (not IsPlaying) then Exit;
 frame:=av_frame_alloc;
 while True do
 begin
  err:=avcodec_receive_frame(videoCodecContext,frame);
  if (err=AVERROR_EAGAIN) and (NextPacket(videoStreamId)) then
   continue;
  if err<>0 then
  begin
   source:='';
   break;
  end;
  //
  if frame^.format<>Integer(AV_PIX_FMT_YUV420P) then
   Writeln(StdErr,'ERROR: Unknown video format: ',frame^.format);
  lastVideoTimeStamp:=av_rescale_q(frame^.best_effort_timestamp,formatContext^.streams[videoStreamId]^.time_base,AV_TIME_BASE_Q);
  nsize:=videoCodecContext^.width*videoCodecContext^.height*3 div 2;
  _CompareAndAllocChunk(videoChunk,nsize);
  if lastVideoTimeStamp>=lastAudioTimeStamp then // Only copy frame when video is synced with audio
  begin
   p:=videoChunk.pAddr;
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
  end;
  break;
 end;
 av_frame_free(frame);
 Result:=videoChunk;
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
 Result:=IsMediaAvailable and (not TAvPlayerInfo(info).isPaused);
end;

function TAvPlayerState.IsMediaAvailable:Boolean;
begin
 Result:=source<>'';
end;

function TAvPlayerState.Buffer(const aType:DWord;const chunk:TMemChunk):Pointer;
var
 playerInfo:TAvPlayerInfo;
begin
 playerInfo:=TAvPlayerInfo(info);
 if aType=0 then
 begin
  if (chunk.pAddr<>nil) then
  begin
   if audioBuffer[0]=nil then
   begin
    audioBuffer[0]:=playerInfo.Allocate(32,chunk.nSize);
   end;
   Move(chunk.pAddr^,audioBuffer[0]^,chunk.nSize);
  end;
  Exit(audioBuffer[0]);
 end else
 begin
  if (chunk.pAddr<>nil) then
  begin
   if videoBuffer[0]=nil then
   begin
    videoBuffer[0]:=playerInfo.AllocateTexture(32,chunk.nSize);
   end;
   Move(chunk.pAddr^,videoBuffer[0]^,chunk.nSize);
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
var
 player:TAvPlayerInfo;
begin
 Result:=0;
 if (pInit=nil) then Exit;

 if not _test_mem_alloc(pInit^.memoryReplacement) then
 begin
  Writeln(StdErr,SysLogPrefix,'ERROR: All allocators are required for AVPlayer Initialisation.');
  Exit;
 end;

 Writeln(SysLogPrefix,'sceAvPlayerInit,autoStart=',pInit^.autoStart);

 player:=TAvPlayerInfo.Create;

 player.memoryReplacement:=pInit^.memoryReplacement;
 player.eventReplacement :=pInit^.eventReplacement;
 player.fileReplacement  :=pInit^.fileReplacement;

 player.isPaused         :=not pInit^.autoStart;

 player.lastFrameTime    :=_GetTimeInUs;

 Result:=_AddPlayer(player);
end;

function ps4_sceAvPlayerInit(pInit:PSceAvPlayerInitData):SceAvPlayerHandle; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceAvPlayerInit(pInit);
 _sig_unlock;
end;

function _sceAvPlayerInitEx(pInit:PSceAvPlayerInitDataEx;pHandle:PSceAvPlayerHandle):Integer;
var
 player:TAvPlayerInfo;
begin
 Result:=-1;
 if (pInit=nil) or (pHandle=nil) then Exit;

 if not _test_mem_alloc(pInit^.memoryReplacement) then
 begin
  Writeln(StdErr,SysLogPrefix,'ERROR: All allocators are required for AVPlayer Initialisation.');
  Exit;
 end;

 Writeln(SysLogPrefix,'sceAvPlayerInitEx,autoStart=',pInit^.autoStart);

 player:=TAvPlayerInfo.Create;

 player.memoryReplacement:=pInit^.memoryReplacement;
 player.eventReplacement :=pInit^.eventReplacement;
 player.fileReplacement  :=pInit^.fileReplacement;

 player.isPaused         :=not pInit^.autoStart;

 player.lastFrameTime    :=_GetTimeInUs;

 pHandle^:=_AddPlayer(player);
 Result:=0;
end;

function ps4_sceAvPlayerInitEx(pInit:PSceAvPlayerInitDataEx;pHandle:PSceAvPlayerHandle):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceAvPlayerInitEx(pInit,pHandle);
 _sig_unlock;
end;

function ps4_sceAvPlayerPostInit(handle:SceAvPlayerHandle;pPostInit:PSceAvPlayerPostInitData):Integer; SysV_ABI_CDecl;
var
 player:TAvPlayerInfo;
begin
 player:=_GetPlayer(handle);
 Result:=-1;
 if (player=nil) or (pPostInit=nil) then Exit;
 Writeln(SysLogPrefix,'sceAvPlayerPostInit');
 player.dec_ref;
 Result:=0;
end;

function _sceAvPlayerAddSource(handle:SceAvPlayerHandle;argFilename:PChar):Integer;
const
 BUF_SIZE=512*1024;
var
 player         :TAvPlayerInfo;
 fileSize,
 bytesRemaining,
 offset         :QWord;
 bytesRead      :Integer;
 actualBufSize  :QWord;
 buf            :array[0..BUF_SIZE-1] of Byte;
 f              :THandle;
 path,
 source         :RawByteString;

 function getFileSize(aFileName:RawByteString):Int64;
 var
  info:TSearchRec;
 begin
  if FindFirst(aFileName,0,info)=0 then
   Result:=Info.Size
  else
   Result:=-1;
  FindClose(info);
 end;

begin
 if DISABLE_FMV_HACK then
  Exit(-1);
 player:=_GetPlayer(handle);
 player.lock;

 // With file functions provided by client
 if (player<>nil) then
 begin
  if player.FileReplacementProvided then
  begin
   if player.open(argFilename)<0 then
   begin
    player.unlock;
    player.dec_ref;
    Exit(-1);
   end;
   fileSize:=player.size();
   if (fileSize=0) then //result is uint64
   begin
    player.unlock;
    player.dec_ref;
    Exit(-1);
   end;
   // Read data and write to dump directory
   path:=DIRECTORY_AVPLAYER_DUMP+'/'+param_sfo.ParamSfoGetString('TITLE_ID');
   CreateDir(path);
   //
   source:=path+'/'+IntToStr(fileSize)+'_'+ExtractFileName(argFilename);
   if fileSize<>getFileSize(source) then
   begin
    f:=FileCreate(source,fmOpenWrite);
    //
    bytesRemaining:=fileSize;
    offset:=0;
    while bytesRemaining>0 do
    begin
     actualBufSize:=Min(QWORD(BUF_SIZE),bytesRemaining);
     bytesRead:=player.readOffset(@buf[0],offset,actualBufSize);
     if bytesRead<0 then
     begin
      player.close();
      player.unlock;
      player.dec_ref;
      Exit(-1);
     end;
     FileWrite(f,buf,actualBufSize);
     Dec(bytesRemaining,actualBufSize);
     Inc(offset,actualBufSize);
    end;
    FileClose(f);
    player.close();
   end;
   // Init player
   player.playerState.CreateMedia(source);
   Result:=0;
  end else
  // Without client-side file functions
  begin
   source:='';
   Result:=parse_filename(argFilename,source);
   if (Result=PT_FILE) then //only real files
   begin
    player.playerState.CreateMedia(source);
    Result:=0;
   end else
   begin
    Result:=-1;
   end;
  end;

  player.EventCallback(SCE_AVPLAYER_STATE_BUFFERING,0,nil);
  player.EventCallback(SCE_AVPLAYER_STATE_READY,0,nil);

  player.unlock;
  player.dec_ref;
 end;
end;

function ps4_sceAvPlayerAddSource(handle:SceAvPlayerHandle;argFilename:PChar):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Writeln(SysLogPrefix,'sceAvPlayerAddSource:',argFilename);
 Result:=_sceAvPlayerAddSource(handle,argFilename);
 _sig_unlock;
end;

function ps4_sceAvPlayerAddSourceEx(handle:SceAvPlayerHandle;uriType:SceAvPlayerUriType;sourceDetails:PSceAvPlayerSourceDetails):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 if sourceDetails<>nil then
 begin
  Writeln(SysLogPrefix,'sceAvPlayerAddSourceEx:',sourceDetails^.uri.name);
  if sourceDetails^.sourceType=1 then
   Writeln('Source type: MP4')
  else
   Writeln(StdErr,'ERROR: Source type: Unknown ',sourceDetails^.sourceType);
  Result:=_sceAvPlayerAddSource(handle,sourceDetails^.uri.name)
 end else
 begin
  Writeln(SysLogPrefix,'sceAvPlayerAddSourceEx:nil');
  Result:=-1;
 end;
 _sig_unlock;
end;

function ps4_sceAvPlayerIsActive(handle:SceAvPlayerHandle):Boolean; SysV_ABI_CDecl;
var
 player:TAvPlayerInfo;
begin
 if DISABLE_FMV_HACK then
  Exit(False);
 player:=_GetPlayer(handle);
 Result:=False;
 if (player<>nil) then
 begin
  Result:=player.playerState.IsPlaying;
 end;
 //Writeln(SysLogPrefix,'sceAvPlayerIsActive=',Result);
 player.dec_ref;
end;

function ps4_sceAvPlayerSetLooping(handle:SceAvPlayerHandle;loopFlag:Boolean):Integer; SysV_ABI_CDecl;
var
 player:TAvPlayerInfo;
begin
 if DISABLE_FMV_HACK then
  Exit(0);
 player:=_GetPlayer(handle);
 Writeln(SysLogPrefix,'sceAvPlayerSetLooping,loopFlag=',loopFlag);
 if (player<>nil) then
 begin
  player.isLooped:=loopFlag;
  player.dec_ref;
  Result:=0;
 end else
 begin
  Result:=-1;
 end;
end;

function _sceAvPlayerGetAudioData(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfo):Boolean;
var
 audioData:TMemChunk;
 player   :TAvPlayerInfo;
begin
 if DISABLE_FMV_HACK then
  Exit(False);
 player:=_GetPlayer(handle);
 //Writeln(SysLogPrefix,'sceAvPlayerGetAudioData');
 Result:=False;
 if player<>nil then
 begin
  player.lock;
  if (frameInfo<>nil) and (player.playerState.IsPlaying) then
  begin
   audioData:=Default(TMemChunk);
   audioData:=player.playerState.ReceiveAudio;
   if (audioData.pAddr=nil) then
   begin
    player.unlock;
    player.dec_ref;
    Exit(False);
   end;
   frameInfo^.timeStamp                 :=_usec2msec(player.playerState.lastAudioTimeStamp);
   frameInfo^.details.audio.channelCount:=player.playerState.channelCount;
   frameInfo^.details.audio.sampleRate  :=player.playerState.sampleRate;
   frameInfo^.details.audio.size        :=player.playerState.channelCount*player.playerState.sampleCount*SizeOf(SmallInt);
   frameInfo^.pData                     :=player.playerState.Buffer(0,audioData);
   Result:=True;
  end;
  player.unlock;
  player.dec_ref;
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
 player   :TAvPlayerInfo;
begin
 if DISABLE_FMV_HACK then
  Exit(False);
 //Writeln(SysLogPrefix,'sceAvPlayerGetVideoDataEx');
 player:=_GetPlayer(handle);
 Result:=False;
 if (player<>nil) then
 begin
  player.lock;
  if (frameInfo<>nil) and (player.playerState.IsPlaying) then
  begin
   videoData:=Default(TMemChunk);
   if player.playerState.lastVideoTimeStamp<=player.playerState.lastAudioTimestamp then
    repeat
     player.lastFrameTime:=_GetTimeInUs;
     videoData:=player.playerState.ReceiveVideo;
    until (videoData.pAddr=nil) or (player.playerState.lastVideoTimeStamp>=player.playerState.lastAudioTimeStamp); // Check to see if video catch up with current timestamp
   if (videoData.pAddr=nil) then
   begin
    player.unlock;
    player.dec_ref;
    Exit(False);
   end;
   frameInfo^.timeStamp                   :=_usec2msec(player.playerState.lastVideoTimeStamp);
   frameInfo^.details.video.width         :=player.playerState.videoCodecContext^.width;
   frameInfo^.details.video.height        :=player.playerState.videoCodecContext^.height;
   frameInfo^.details.video.aspectRatio   :=player.playerState.videoCodecContext^.width/player.playerState.videoCodecContext^.height;
   frameInfo^.details.video.framerate     :=0;
   frameInfo^.details.video.languageCode  :=LANGUAGE_CODE_ENG;
   frameInfo^.details.video.pitch         :=frameInfo^.details.video.width;
   frameInfo^.details.video.lumaBitDepth  :=8;
   frameInfo^.details.video.chromaBitDepth:=8;
   frameInfo^.pData                       :=player.playerState.Buffer(1,videoData);
   Result:=True;
  end;
  player.unlock;
  player.dec_ref;
 end;
end;

function ps4_sceAvPlayerGetVideoDataEx(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfoEx):Boolean; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceAvPlayerGetVideoDataEx(handle,frameInfo);
 _sig_unlock;
end;

function ps4_sceAvPlayerGetVideoData(handle:SceAvPlayerHandle;frameInfo:PSceAvPlayerFrameInfo):Boolean; SysV_ABI_CDecl;
var
 frameInfoEx:SceAvPlayerFrameInfoEx;
begin
 _sig_lock;
 Result:=_sceAvPlayerGetVideoDataEx(handle,@frameInfoEx);
 if Result then
 begin
  frameInfo^.timeStamp                 :=frameInfoEx.timeStamp;
  frameInfo^.details.video.width       :=frameInfoEx.details.video.width;
  frameInfo^.details.video.height      :=frameInfoEx.details.video.height;
  frameInfo^.details.video.aspectRatio :=frameInfoEx.details.video.aspectRatio;
  frameInfo^.details.video.languageCode:=LANGUAGE_CODE_ENG;
  frameInfo^.pData                     :=frameInfoEx.pData;
 end;
 _sig_unlock;
end;

function ps4_sceAvPlayerSetAvSyncMode(handle:SceAvPlayerHandle;argSyncMode:SceAvPlayerAvSyncMode):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceAvPlayerSetAvSyncMode,argSyncMode=',argSyncMode);
 // Do nothing
 Result:=0;
end;

function ps4_sceAvPlayerCurrentTime(handle:SceAvPlayerHandle):QWord; SysV_ABI_CDecl;
var
 player:TAvPlayerInfo;
begin
 if DISABLE_FMV_HACK then
  Exit(0);
 player:=_GetPlayer(handle);
 if (player=nil) or (not player.playerState.IsPlaying) then
  Result:=0
 else
  Result:=_usec2msec(player.playerState.lastVideoTimeStamp);

 player.dec_ref;
end;

function _sceAvPlayerStop(handle:SceAvPlayerHandle):Integer;
var
 player:TAvPlayerInfo;
begin
 player:=_GetPlayer(handle);
 player.lock;
 Result:=-1;
 if (player<>nil) then
 begin
  player.playerState.FreeMedia;
  player.unlock;
  player.dec_ref;
  Result:=0;
 end;
 _AvPlayerEventCallback(handle,SCE_AVPLAYER_STATE_STOP,nil);
end;

function ps4_sceAvPlayerStop(handle:SceAvPlayerHandle):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;

 Writeln(SysLogPrefix,'sceAvPlayerStop');
 Result:=_sceAvPlayerStop(handle);

 _sig_unlock;
end;

function ps4_sceAvPlayerSetLogCallback(logCb:SceAvPlayerLogCallback;userData:Pointer):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceAvPlayerSetLogCallback');
 Result:=0;
end;

function ps4_sceAvPlayerStreamCount(handle:SceAvPlayerHandle):Integer; SysV_ABI_CDecl;
var
 player:TAvPlayerInfo;
begin
 if DISABLE_FMV_HACK then
  Exit(-1);
 player:=_GetPlayer(handle);
 if (player<>nil) then
 begin
  if player.playerState.IsMediaAvailable then
   Result:=player.playerState.streamCount
  else
   Result:=0;
  player.dec_ref;
 end else
  Result:=-1;
 Writeln(SysLogPrefix,'sceAvPlayerStreamCount=',Result);
end;

function ps4_sceAvPlayerJumpToTime(handle:SceAvPlayerHandle;timeInMs:QWord):Integer; SysV_ABI_CDecl;
var
 player:TAvPlayerInfo;
begin
 if DISABLE_FMV_HACK then
  Exit(-1);
 Writeln(SysLogPrefix,'sceAvPlayerJumpToTime');
 player:=_GetPlayer(handle);
 if (player=nil) or ((player<>nil) and (not player.playerState.IsMediaAvailable)) then
  Result:=-1
 else
 // TODO: Do nothing for now
  Result:=0;
 player.dec_ref;
end;

function ps4_sceAvPlayerGetStreamInfo(handle:SceAvPlayerHandle;streamId:DWord;argInfo:PSceAvPlayerStreamInfo):Integer; SysV_ABI_CDecl;
var
 player:TAvPlayerInfo;
begin
 if DISABLE_FMV_HACK then
  Exit(-1);
 Writeln(SysLogPrefix,'sceAvPlayerGetStreamInfo,streamId=',streamId);
 player:=_GetPlayer(handle);
 if (player<>nil) and (player.playerState.IsMediaAvailable) and (argInfo<>nil) then
 begin
  if streamId=player.playerState.videoStreamId then
  begin
   argInfo^.type_                     :=SCE_AVPLAYER_VIDEO;
   argInfo^.duration                  :=player.playerState.durationInMs;
   argInfo^.startTime                 :=0;
   argInfo^.details.video.width       :=player.playerState.videoCodecContext^.width;
   argInfo^.details.video.height      :=player.playerState.videoCodecContext^.height;
   argInfo^.details.video.aspectRatio :=player.playerState.videoCodecContext^.width/player.playerState.videoCodecContext^.height;
   argInfo^.details.video.languageCode:=LANGUAGE_CODE_ENG;
   Writeln('width: ',argInfo^.details.video.width);
   Writeln('height: ',argInfo^.details.video.height);
   Writeln('aspectRatio: ',argInfo^.details.video.aspectRatio:0:4);
  end else
  if streamId=player.playerState.audioStreamId then
  begin
   // We need to read audio packet to get sampleCount value. Since this function may be called before sceAvPlayerGetAudioData,
   // the value we currently have may not valid.
   // Therefore we need to manually call ReceiveAudio to receive audio information, in case sampleCount = 0
   if player.playerState.sampleCount=0 then
    player.playerState.ReceiveAudio(True);
   argInfo^.type_                     :=SCE_AVPLAYER_AUDIO;
   argInfo^.duration                  :=player.playerState.durationInMs;
   argInfo^.startTime                 :=0;
   argInfo^.details.audio.channelCount:=player.playerState.channelCount;
   argInfo^.details.audio.sampleRate  :=player.playerState.sampleRate;
   argInfo^.details.audio.size        :=player.playerState.channelCount*player.playerState.sampleCount*SizeOf(SmallInt);
   argInfo^.details.audio.languageCode:=LANGUAGE_CODE_ENG;
   Writeln('channelCount: ',argInfo^.details.audio.channelCount);
   Writeln('sampleRate: ',argInfo^.details.audio.sampleRate);
   Writeln('size: ',argInfo^.details.audio.size);
  end else
   argInfo^.type_:=SCE_AVPLAYER_UNKNOWN;
  Result:=0;
 end else
  Result:=-1;
 player.dec_ref;
end;

function ps4_sceAvPlayerEnableStream(handle:SceAvPlayerHandle;streamId:DWord):Integer; SysV_ABI_CDecl;
var
 player:TAvPlayerInfo;
begin
 if DISABLE_FMV_HACK then
  Exit(-1);
 Writeln(SysLogPrefix,'sceAvPlayerEnableStream,streamId=',streamId);
 player:=_GetPlayer(handle);
 if (player<>nil) and (player.playerState.IsMediaAvailable) then
  Result:=0 // Do nothing
 else
  Result:=-1;
 player.dec_ref;
end;

function ps4_sceAvPlayerDisableStream(handle:SceAvPlayerHandle;streamId:DWord):Integer; SysV_ABI_CDecl;
var
 player:TAvPlayerInfo;
begin
 if DISABLE_FMV_HACK then
  Exit(-1);
 Writeln(SysLogPrefix,'sceAvPlayerDisableStream,streamId=',streamId);
 player:=_GetPlayer(handle);
 if (player<>nil) and (player.playerState.IsMediaAvailable) then
  Result:=0 // Do nothing
 else
  Result:=-1;
 player.dec_ref;
end;

function ps4_sceAvPlayerStart(handle:SceAvPlayerHandle):Integer; SysV_ABI_CDecl;
var
 player:TAvPlayerInfo;
begin
 Writeln(SysLogPrefix,'sceAvPlayerStart');
 player:=_GetPlayer(handle);
 player.lock;
 if (player=nil) or ((player<>nil) and (not player.playerState.IsMediaAvailable)) then
  Result:=-1
 else
 begin
  player.isPaused:=False;
  player.EventCallback(SCE_AVPLAYER_STATE_PLAY,0,nil);
 end;
 player.unlock;
 player.dec_ref;
end;

function ps4_sceAvPlayerPause(handle:SceAvPlayerHandle):Integer; SysV_ABI_CDecl;
var
 player:TAvPlayerInfo;
begin
 Writeln(SysLogPrefix,'sceAvPlayerPause');
 player:=_GetPlayer(handle);
 player.lock;
 if (player=nil) or ((player<>nil) and (not player.playerState.IsMediaAvailable)) then
  Result:=-1
 else
 begin
  player.isPaused:=True;
  player.EventCallback(SCE_AVPLAYER_STATE_PAUSE,0,nil);
 end;
 player.unlock;
 player.dec_ref;
end;

function ps4_sceAvPlayerResume(handle:SceAvPlayerHandle):Integer; SysV_ABI_CDecl;
var
 player:TAvPlayerInfo;
begin
 Writeln(SysLogPrefix,'sceAvPlayerResume');
 player:=_GetPlayer(handle);
 player.lock;
 if (player=nil) or ((player<>nil) and (not player.playerState.IsMediaAvailable)) then
  Result:=-1
 else
 begin
  player.isPaused:=False;
  player.EventCallback(SCE_AVPLAYER_STATE_PLAY,0,nil);
 end;
 player.unlock;
 player.dec_ref;
end;

function _sceAvPlayerClose(handle:SceAvPlayerHandle):Integer;
begin
 Result:=-1;

 _sceAvPlayerStop(handle);
 _DeletePlayer(handle);

 Result:=0;
end;

function ps4_sceAvPlayerClose(handle:SceAvPlayerHandle):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Writeln(SysLogPrefix,'sceAvPlayerClose');
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
 lib^.set_proc($C7CBAFB8538F6615,@ps4_sceAvPlayerAddSourceEx);
 lib^.set_proc($51B42861AC0EB1F6,@ps4_sceAvPlayerIsActive);
 lib^.set_proc($395B61B34C467E1A,@ps4_sceAvPlayerSetLooping);
 lib^.set_proc($5A7A7539572B6609,@ps4_sceAvPlayerGetAudioData);
 lib^.set_proc($A37F915A71D58928,@ps4_sceAvPlayerGetVideoData);
 lib^.set_proc($25D92C42EF2935D4,@ps4_sceAvPlayerGetVideoDataEx);
 lib^.set_proc($93FABEC4EC5D7371,@ps4_sceAvPlayerSetAvSyncMode);
 lib^.set_proc($C3033DF608C57F56,@ps4_sceAvPlayerCurrentTime);
 lib^.set_proc($7814EB799F382456,@ps4_sceAvPlayerSetLogCallback);
 lib^.set_proc($85D4F247309741E4,@ps4_sceAvPlayerStreamCount);
 lib^.set_proc($5C2F7033EC542F3F,@ps4_sceAvPlayerJumpToTime);
 lib^.set_proc($77C15C6F37C0750C,@ps4_sceAvPlayerGetStreamInfo);
 lib^.set_proc($38324ADAC9FDC380,@ps4_sceAvPlayerEnableStream);
 lib^.set_proc($04E54A033466B934,@ps4_sceAvPlayerDisableStream);
 lib^.set_proc($113E06AFF52ED3BB,@ps4_sceAvPlayerStart);
 lib^.set_proc($F72E6FF9F18DE169,@ps4_sceAvPlayerPause);
 lib^.set_proc($C399A80013709D16,@ps4_sceAvPlayerResume);
 lib^.set_proc($642D7BC37BC1E4BA,@ps4_sceAvPlayerStop);
 lib^.set_proc($3642700F32A6225C,@ps4_sceAvPlayerClose);
end;

initialization
 FillChar(AvHandleHamt,SizeOf(AvHandleHamt),0);
 ps4_app.RegistredPreLoad('libSceAvPlayer.prx',@Load_libSceAvPlayer);

finalization
 // Cleanup leftover handle. This should not happen, unless the game quit halfway or get memory leak somehow
 HAMT_clear32(@AvHandleHamt,@AvHandleClean,nil);

end.
