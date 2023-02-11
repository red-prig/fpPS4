unit ps4_libSceAudiodec;

{$mode ObjFPC}{$H+}

interface

uses
 hamt,
 spinlock,
 ps4_program;

implementation

const
 SCE_AUDIODEC_TYPE_AT9  =1;
 SCE_AUDIODEC_TYPE_MP3  =2;
 SCE_AUDIODEC_TYPE_M4AAC=3;

 SCE_AUDIODEC_ERROR_API_FAIL=$807F0000;
 SCE_AUDIODEC_ERROR_INVALID_TYPE=$807F0001;
 SCE_AUDIODEC_ERROR_ARG=$807F0002;
 SCE_AUDIODEC_ERROR_INVALID_SIZE=$807F0003;
 SCE_AUDIODEC_ERROR_INVALID_PARAM_SIZE=$807F0004;
 SCE_AUDIODEC_ERROR_INVALID_BSI_INFO_SIZE=$807F0005;
 SCE_AUDIODEC_ERROR_INVALID_AU_INFO_SIZE=$807F0006;
 SCE_AUDIODEC_ERROR_INVALID_PCM_ITEM_SIZE=$807F0007;
 SCE_AUDIODEC_ERROR_INVALID_CTRL_POINTER=$807F0008;
 SCE_AUDIODEC_ERROR_INVALID_PARAM_POINTER=$807F0009;
 SCE_AUDIODEC_ERROR_INVALID_BSI_INFO_POINTER=$807F000A;
 SCE_AUDIODEC_ERROR_INVALID_AU_INFO_POINTER=$807F000B;
 SCE_AUDIODEC_ERROR_INVALID_PCM_ITEM_POINTER=$807F000C;
 SCE_AUDIODEC_ERROR_INVALID_AU_POINTER=$807F000D;
 SCE_AUDIODEC_ERROR_INVALID_PCM_POINTER=$807F000E;
 SCE_AUDIODEC_ERROR_INVALID_HANDLE=$807F000F;
 SCE_AUDIODEC_ERROR_INVALID_WORD_LENGTH=$807F0010;
 SCE_AUDIODEC_ERROR_INVALID_AU_SIZE=$807F0011;
 SCE_AUDIODEC_ERROR_INVALID_PCM_SIZE=$807F0012;
 SCE_AUDIODEC_ERROR_M4AAC_INVALID_SAMPLING_FREQ=$807F0300;
 SCE_AUDIODEC_ERROR_M4AAC_INVALID_ENABLE_HEAAC=$807F0302;
 SCE_AUDIODEC_ERROR_M4AAC_INVALID_CONFIG_NUMBER=$807F0303;
 SCE_AUDIODEC_ERROR_M4AAC_INVALID_MAX_CHANNELS=$807F0304;
 SCE_AUDIODEC_ERROR_M4AAC_INVALID_ENABLE_NONDELAY_OUTPUT=$807F0305;
 SCE_AUDIODEC_ERROR_M4AAC_INVALID_SURROUND_CHANNEL_INTERLEAVE_ORDER=$807F0306;
 SCE_AUDIODEC_ERROR_AT9_INVALID_CONFIG_DATA=$807F1000;

type
 SceAudiodecAuInfo=packed record
  uiSize  :DWord;
  _align1 :Dword;
  pAuAddr :Pointer;
  uiAuSize:DWord;
  _align2 :DWord;
 end;
 PSceAudiodecAuInfo=^SceAudiodecAuInfo;

 SceAudiodecPcmItem=packed record
  uiSize   :DWord;
  _align1  :Dword;
  pPcmAddr :Pointer;
  uiPcmSize:DWord;
  _align2  :DWord;
 end;
 PSceAudiodecPcmItem=^SceAudiodecPcmItem;

 SceAudiodecParam=packed record
  uiSize:DWord;
  // TODO: union
 end;
 PSceAudiodecParam=^SceAudiodecParam;

 SceAudiodecBsiInfo=packed record
  uiSize:DWord;
  // TODO: union
 end;
 PSceAudiodecBsiInfo=^SceAudiodecBsiInfo;

 SceAudiodecCtrl=packed record
  pParam  :PSceAudiodecParam;
  pBsiInfo:PSceAudiodecBsiInfo;
  pAuInfo :PSceAudiodecAuInfo;
  pPcmItem:PSceAudiodecPcmItem;
 end;
 PSceAudiodecCtrl=^SceAudiodecCtrl;

 SceAudiodecHandle =DWord;
 PSceAudiodecHandle=^SceAudiodecHandle;

 TAudioDecoderAT9=record
  channels          :DWord;
  sampleRate        :DWord;
  bitRate           :DWord;
  configData        :DWord;
  superFrameRate    :DWord;
  framesInSuperFrame:DWord;
 end;

 TAudioDecoderMP3=record
  channels:DWord;
 end;

 TAudioDecoderM4AAC=record
  channels  :DWord;
  sampleRate:DWord;
  isAdts    :DWord;
  isSbr     :DWord;
 end;

 TAudioDecoderInfo=record
  param    :SceAudiodecParam;
  bsiInfo  :SceAudiodecBsiInfo;
  codecType:DWord;
  case Byte of
   SCE_AUDIODEC_TYPE_AT9  :(at9:TAudioDecoderAT9);
   SCE_AUDIODEC_TYPE_MP3  :(mp3:TAudioDecoderMP3);
   SCE_AUDIODEC_TYPE_M4AAC:(aac:TAudioDecoderM4AAC);
 end;

 TAudioDecoder=class
  handle:SceAudiodecHandle;
  info  :TAudioDecoderInfo;
  constructor Create(const pCtrl:PSceAudiodecCtrl;const codecType:DWord);
  destructor  Destroy; override;
 end;

var
 hamt_lock   :Pointer;
 handleCount :QWord=0;
 AudiodecHamt:TSTUB_HAMT32;

function _GetDecoder(const handle:SceAudiodecHandle):TAudioDecoder;
var
 data:PPointer;
begin
 Result:=nil;
 spin_lock(hamt_lock);
  data:=HAMT_search32(@AudiodecHamt,handle);
  if (data<>nil) then
  begin
   Result:=TAudioDecoder(data^);
  end;
 spin_unlock(hamt_lock);
end;

function _AddDecoder(const decoder:TAudioDecoder):SceAudiodecHandle;
var
 data:PPointer;
begin
 spin_lock(hamt_lock);
  Inc(handleCount);
  data:=HAMT_insert32(@AudiodecHamt,handleCount,decoder);
  Assert(data<>nil,'unexpected err');
  data^:=decoder;
  decoder.handle:=handleCount;
  Result:=handleCount;
 spin_unlock(hamt_lock);
end;

procedure _DeleteDecoder(const handle:SceAudiodecHandle);
begin
 spin_lock(hamt_lock);
  HAMT_delete32(@AudiodecHamt,handle);
 spin_unlock(hamt_lock);
end;

constructor TAudioDecoder.Create(const pCtrl:PSceAudiodecCtrl;const codecType:DWord);
begin
 inherited Create;
 _AddDecoder(Self);
 info.codecType:=codecType;
 Move(pCtrl^.pParam^  ,info.param  ,pCtrl^.pParam^.uiSize);
 Move(pCtrl^.pBsiInfo^,info.bsiInfo,pCtrl^.pBsiInfo^.uiSize);
 case codecType of
  SCE_AUDIODEC_TYPE_AT9:
   begin
    // TODO:
   end;
  SCE_AUDIODEC_TYPE_MP3:
   begin
    // TODO:
   end;
  SCE_AUDIODEC_TYPE_M4AAC:
   begin
    // TODO:
   end;
 end;
end;

destructor TAudioDecoder.Destroy;
begin
 _DeleteDecoder(handle);
 inherited;
end;

function ps4_sceAudiodecInitLibrary(codecType:DWord):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceAudiodecInitLibrary,codecType=',codecType);
 if not (codecType in [SCE_AUDIODEC_TYPE_AT9..SCE_AUDIODEC_TYPE_M4AAC]) then
  Result:=SCE_AUDIODEC_ERROR_INVALID_TYPE
 else
  Result:=0;
end;

function ps4_sceAudiodecCreateDecoder(pCtrl:PSceAudiodecCtrl;uiCodecType:Dword):Integer; SysV_ABI_CDecl;
var
 audioDecoder:TAudioDecoder;
begin
 Writeln('sceAudiodecCreateDecoder,uiCodecType=',uiCodecType);
 if pCtrl=nil then
  Exit(SCE_AUDIODEC_ERROR_INVALID_CTRL_POINTER);
 if pCtrl^.pParam=nil then
  Exit(SCE_AUDIODEC_ERROR_INVALID_PARAM_POINTER);
 if pCtrl^.pBsiInfo=nil then
  Exit(SCE_AUDIODEC_ERROR_INVALID_BSI_INFO_POINTER);
 case uiCodecType of
  SCE_AUDIODEC_TYPE_AT9,
  SCE_AUDIODEC_TYPE_MP3,
  SCE_AUDIODEC_TYPE_M4AAC:
   begin
    audioDecoder:=TAudioDecoder.Create(pCtrl,uiCodecType);
    Result:=audioDecoder.handle;
   end;
  else
   begin
    Writeln(StdErr,'ERROR: Unsupported audio decoder');
    Result:=SCE_AUDIODEC_ERROR_INVALID_TYPE;
   end;
 end;
end;

function ps4_sceAudiodecDecode(handle:Integer;pCtrl:PSceAudiodecCtrl):Integer; SysV_ABI_CDecl;
begin
 if pCtrl=nil then
  Exit(-1);
 //Writeln('sceAudiodecDecode,handle=',handle,',uiAuSize=',pCtrl^.pAuInfo^.uiAuSize,',uiPcmSize=',pCtrl^.pPcmItem^.uiPcmSize);
 // TODO:
 Result:=0;
end;

function ps4_sceAudiodecDeleteDecoder(handle:Integer):Integer; SysV_ABI_CDecl;
var
 audioDecoder:TAudioDecoder;
begin
 Writeln('sceAudiodecDeleteDecoder,handle=',handle);
 audioDecoder:=_GetDecoder(handle);
 if audioDecoder<>nil then
  audioDecoder.Free;
 Result:=0;
end;

function ps4_sceAudiodecClearContext(handle:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceAudiodecClearContext,handle=',handle);
 Result:=0;
end;

function Load_libSceAudiodec(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceAudiodec');

 lib^.set_proc($56386C9B1A5C7B32,@ps4_sceAudiodecInitLibrary);
 lib^.set_proc($3B77F5B0B31646FB,@ps4_sceAudiodecCreateDecoder);
 lib^.set_proc($2875C73032E420BC,@ps4_sceAudiodecDecode);
 lib^.set_proc($4E9F99132EBD98B9,@ps4_sceAudiodecDeleteDecoder);
 lib^.set_proc($E957FD5932C3A2CB,@ps4_sceAudiodecClearContext);
end;

initialization
 FillChar(AudiodecHamt,SizeOf(AudiodecHamt),0);
 ps4_app.RegistredPreLoad('libSceAudiodec.prx' ,@Load_libSceAudiodec);

end.

