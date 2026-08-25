unit ps4_libSceAjm;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 time,
 md_sleep,
 subr_dynlib;

implementation

uses
 kern_id;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

{$I ajm_error.inc}

const
 SCE_AJM_CODEC_MP3_DEC  =0;
 SCE_AJM_CODEC_AT9_DEC  =1;
 SCE_AJM_CODEC_M4AAC_DEC=2;
 SCE_AJM_CODEC_CELP8_DEC=3;
 SCE_AJM_CODEC_CELP8_ENC=4;
 SCE_AJM_CODEC_CELP_DEC =12;
 SCE_AJM_CODEC_CELP_ENC =13;

 //_SCE_AJM_FLAG_SIZE_REVISION            =3;
 //_SCE_AJM_INSTANCE_FLAG_OFFSET_STD_FLAGS=3+7;
 //_SCE_AJM_FLAG_SIZE_CODEC               =8;

 SCE_AJM_INSTANCE_FLAG_PRIORITY_VIOLATION=(QWORD(1) shl 11);
 SCE_AJM_INSTANCE_FLAG_RESAMPLE          =(QWORD(1) shl 12);

 SCE_AJM_FLAG_SIDEBAND_STREAM            =(QWORD(1) shl 47); //SceAjmSidebandStream
 SCE_AJM_FLAG_SIDEBAND_FORMAT            =(QWORD(1) shl 46); //SceAjmSidebandFormat
 SCE_AJM_FLAG_SIDEBAND_GAPLESS_DECODE    =(QWORD(1) shl 45); //SceAjmSidebandGaplessDecode

 SCE_AJM_FLAG_STATISTICS_ENGINE          =(QWORD(1) shl 31); //SceAjmSidebandStatisticsEngine (output), SceAjmSidebandStatisticsEngineParameters (input)
 SCE_AJM_FLAG_STATISTICS_ENGINE_PER_CODEC=(QWORD(1) shl 30); //SceAjmSidebandStatisticsEnginePerCodec (output)
 SCE_AJM_FLAG_STATISTICS_MEMORY          =(QWORD(1) shl 15); //SceAjmSidebandStatisticsMemory (output)

 SCE_AJM_FLAG_CONTROL_RESAMPLE           =(QWORD(1) shl 15); //SceAjmSidebandResampleParameters (input)
 SCE_AJM_FLAG_CONTROL_INITIALIZE         =(QWORD(1) shl 14); //SceAjmDecAt9InitializeParameters,SceAjmDecM4aacInitializeParameters
 SCE_AJM_FLAG_CONTROL_RESET              =(QWORD(1) shl 13);

 SCE_AJM_FLAG_RUN_MULTIPLE_FRAMES        =(QWORD(1) shl 12); //SceAjmSidebandMFrame
 SCE_AJM_FLAG_RUN_GET_CODEC_INFO         =(QWORD(1) shl 11); //SceAjmSidebandDecAt9CodecInfo,SceAjmSidebandDecM4aacCodecInfo,SceAjmSidebandDecMp3CodecInfo

 //SCE_AJM_FLAG_SIDEBAND_CODEC(N) (1 << (3 + (N)))

 SCE_AJM_FLAG_DEC_CELP8_LOST_FRAME       =(QWORD(1) shl 3);
 SCE_AJM_FLAG_DEC_CELP_LOST_FRAME        =(QWORD(1) shl 3);

var
 FAjmMap:t_id_desc_table;

type
 PSceAjmContextId=^SceAjmContextId;
 SceAjmContextId=Integer;
 SceAjmCodecType=Integer;

 pSceAjmInstanceId=^SceAjmInstanceId;
 SceAjmInstanceId=DWORD;

 SceAjmResult=Integer;
 pSceAjmSidebandResult=^SceAjmSidebandResult;
 SceAjmSidebandResult=packed record
  iResult        :SceAjmResult;
  iInternalResult:SceAjmResult;
 end;

 //SCE_AJM_FLAG_SIDEBAND_STREAM
 SceAjmSidebandStream=packed record
  iSizeConsumed        :Integer;
  iSizeProduced        :Integer;
  uiTotalDecodedSamples:QWORD;
 end;

 //SCE_AJM_FLAG_SIDEBAND_FORMAT
 SceAjmSidebandFormat=packed record
  eChannelNumber     :Integer;
  uiChannelMask      :DWORD;
  uiSamplingFrequency:DWORD;
  eSampleEncoding    :Integer;
  uiBitrate          :DWORD;
  _reserved          :DWORD;
 end;

 //SCE_AJM_FLAG_SIDEBAND_GAPLESS_DECODE
 SceAjmSidebandGaplessDecode=packed record
  uiTotalSamples  :DWORD;
  uiSkipSamples   :WORD;
  uiSkippedSamples:WORD;
 end;

 //SCE_AJM_FLAG_STATISTICS_ENGINE (output)
 SceAjmSidebandStatisticsEngine=packed record
  fUsageBatch   :Single;
  fUsageInterval:array[0..2] of Single;
 end;

 //SCE_AJM_FLAG_STATISTICS_ENGINE (input)
 SceAjmSidebandStatisticsEngineParameters=packed record
  uiIntervalCount:DWORD;
  fInterval      :array[0..2] of Single;
 end;

 //SCE_AJM_FLAG_STATISTICS_ENGINE_PER_CODEC (output)
 SceAjmSidebandStatisticsEnginePerCodec=packed record
  iCodecCount     :Byte;
  iCodecId        :array[0..2] of Byte;
  fCodecPercentage:array[0..2] of Single;
 end;

 //SCE_AJM_FLAG_STATISTICS_MEMORY
 SceAjmSidebandStatisticsMemory=packed record
  uiInstanceFree:DWORD;
  uiBufferFree  :DWORD;
  uiBatchSize   :DWORD;
  uiInputSize   :DWORD;
  uiOutputSize  :DWORD;
  uiSmallSize   :DWORD;
 end;

 //SCE_AJM_FLAG_CONTROL_RESAMPLE (input)
 SceAjmSidebandResampleParameters=packed record
  fRatio :Single;
  uiFlags:DWORD;
 end;

 //SCE_AJM_FLAG_RUN_MULTIPLE_FRAMES
 SceAjmSidebandMFrame=packed record
  uiNumFrames:DWORD;
  _reserved  :DWORD;
 end;

//////////////

 //SCE_AJM_FLAG_CONTROL_INITIALIZE
 SceAjmDecAt9InitializeParameters=packed record
  uiConfigData:DWORD;
  _reserved   :DWORD;
 end;

 //SCE_AJM_FLAG_RUN_GET_CODEC_INFO
 SceAjmSidebandDecAt9CodecInfo=packed record
  uiSuperFrameSize    :DWORD;
  uiFramesInSuperFrame:DWORD;
  uiNextFrameSize     :DWORD;
  uiFrameSamples      :DWORD;
 end;

 pSceAjmDecAt9GetCodecInfoResult=^SceAjmDecAt9GetCodecInfoResult;
 SceAjmDecAt9GetCodecInfoResult=packed record
  sResult   :SceAjmSidebandResult;
  sCodecInfo:SceAjmSidebandDecAt9CodecInfo;
 end;

//////////////

 //SCE_AJM_FLAG_CONTROL_INITIALIZE
 SceAjmDecM4aacInitializeParameters=packed record
  uiConfigNumber     :DWORD;
  uiSamplingFreqIndex:DWORD;
 end;

 //SCE_AJM_FLAG_RUN_GET_CODEC_INFO
 SceAjmSidebandDecM4aacCodecInfo=packed record
  uiHeaac   :DWORD;
  uiReserved:DWORD;
 end;

//////////////

 //SCE_AJM_FLAG_RUN_GET_CODEC_INFO
 SceAjmSidebandDecMp3CodecInfo=packed record
  uiHeader       :DWORD;
  ucCrc          :Byte;
  ucMode         :Byte;
  ucModeExtension:Byte;
  ucCopyright    :Byte;
  ucOriginal     :Byte;
  ucEmphasis     :Byte;
  _reserved      :array[0..2] of Word;
 end;

//////////////

 pSceAjmBuffer=^SceAjmBuffer;
 SceAjmBuffer=packed record
  pAddress:Pointer;
  szSize  :qword;
 end;

type
 pSceAjmBatchId=^SceAjmBatchId;
 SceAjmBatchId=DWORD;

 pSceAjmBatchError=^SceAjmBatchError;
 SceAjmBatchError=packed record
  iErrorCode     :Integer; //Detailed error code
  align1         :Integer;
  pJobAddress    :Pointer; //For internal use only
  uiCommandOffset:Integer; //For internal use only
  align2         :Integer;
  pJobOriginRa   :Pointer; //For internal use only
 end;

 TAjmContext=class
  desc:t_id_desc;
  AJM_CODEC_MP3_DEC  :Pointer;
  AJM_CODEC_AT9_DEC  :Pointer;
  AJM_CODEC_M4AAC_DEC:Pointer;
  AJM_CODEC_CELP8_DEC:Pointer;
  AJM_CODEC_CELP8_ENC:Pointer;
  AJM_CODEC_CELP_DEC :Pointer;
  AJM_CODEC_CELP_ENC :Pointer;
 end;


function ps4_sceAjmInitialize(iReserved:QWORD;pContext:PSceAjmContextId):Integer;
Var
 H:TAjmContext;
begin
 if (pContext=nil) then Exit(SCE_AJM_ERROR_INVALID_PARAMETER);
 H:=TAjmContext.Create;
 pContext^:=-1;
 if id_new(@FAjmMap,@H.desc,pContext) then
 begin
  id_release(@H.desc); //<-id_new
 end;
 Result:=0;
end;

function id_ctx_get(Key:Integer):TAjmContext;
var
 desc:p_id_desc;
begin
 Result:=nil;
 desc:=id_get(@FAjmMap,Key,nil);
 if (desc<>nil) then
 begin
  Result:=TAjmContext(Pointer(desc)-Ptruint(@TAjmContext(nil).desc));
 end;
end;

function ps4_sceAjmModuleRegister(uiContext:SceAjmContextId;uiCodec:SceAjmCodecType;iReserved:QWORD):Integer;
Var
 H:TAjmContext;
begin
 Result:=0;

 H:=id_ctx_get(uiContext);
 if (H=nil) then Exit(SCE_AJM_ERROR_INVALID_CONTEXT);

 Case uiCodec of
  SCE_AJM_CODEC_MP3_DEC  :
    begin
     if (H.AJM_CODEC_MP3_DEC<>nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_ALREADY_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_MP3_DEC:=Pointer(1);
    end;
  SCE_AJM_CODEC_AT9_DEC  :
    begin
     if (H.AJM_CODEC_AT9_DEC<>nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_ALREADY_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_AT9_DEC:=Pointer(1);
    end;
  SCE_AJM_CODEC_M4AAC_DEC:
    begin
     if (H.AJM_CODEC_M4AAC_DEC<>nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_ALREADY_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_M4AAC_DEC:=Pointer(1);
    end;
  SCE_AJM_CODEC_CELP8_DEC:
    begin
     if (H.AJM_CODEC_CELP8_DEC<>nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_ALREADY_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_CELP8_DEC:=Pointer(1);
    end;
  SCE_AJM_CODEC_CELP8_ENC:
    begin
     if (H.AJM_CODEC_CELP8_ENC<>nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_ALREADY_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_CELP8_ENC:=Pointer(1);
    end;
  SCE_AJM_CODEC_CELP_DEC :
    begin
     if (H.AJM_CODEC_CELP_DEC<>nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_ALREADY_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_CELP_DEC:=Pointer(1);
    end;
  SCE_AJM_CODEC_CELP_ENC :
    begin
     if (H.AJM_CODEC_CELP_ENC<>nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_ALREADY_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_CELP_ENC:=Pointer(1);
    end;
  else
    begin
     Result:=SCE_AJM_ERROR_INVALID_PARAMETER;
     id_release(@H.desc);
     Exit;
    end;
 end;

 Case uiCodec of
  SCE_AJM_CODEC_MP3_DEC  :LOG_INFO('SCE_AJM_CODEC_MP3_DEC  ');
  SCE_AJM_CODEC_AT9_DEC  :LOG_INFO('SCE_AJM_CODEC_AT9_DEC  ');
  SCE_AJM_CODEC_M4AAC_DEC:LOG_INFO('SCE_AJM_CODEC_M4AAC_DEC');
  SCE_AJM_CODEC_CELP8_DEC:LOG_INFO('SCE_AJM_CODEC_CELP8_DEC');
  SCE_AJM_CODEC_CELP8_ENC:LOG_INFO('SCE_AJM_CODEC_CELP8_ENC');
  SCE_AJM_CODEC_CELP_DEC :LOG_INFO('SCE_AJM_CODEC_CELP_DEC ');
  SCE_AJM_CODEC_CELP_ENC :LOG_INFO('SCE_AJM_CODEC_CELP_ENC ');
 end;

 id_release(@H.desc); //<-id_ctx_get
end;

function ps4_sceAjmModuleUnregister(uiContext:SceAjmContextId;uiCodec:SceAjmCodecType):Integer;
Var
 H:TAjmContext;
begin
 Result:=0;

 H:=id_ctx_get(uiContext);
 if (H=nil) then Exit(SCE_AJM_ERROR_INVALID_CONTEXT);

 Case uiCodec of
  SCE_AJM_CODEC_MP3_DEC  :
    begin
     if (H.AJM_CODEC_MP3_DEC=nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_NOT_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_MP3_DEC:=nil;
    end;
  SCE_AJM_CODEC_AT9_DEC  :
    begin
     if (H.AJM_CODEC_AT9_DEC=nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_NOT_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_AT9_DEC:=nil;
    end;
  SCE_AJM_CODEC_M4AAC_DEC:
    begin
     if (H.AJM_CODEC_M4AAC_DEC=nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_NOT_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_M4AAC_DEC:=nil;
    end;
  SCE_AJM_CODEC_CELP8_DEC:
    begin
     if (H.AJM_CODEC_CELP8_DEC=nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_NOT_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_CELP8_DEC:=nil;
    end;
  SCE_AJM_CODEC_CELP8_ENC:
    begin
     if (H.AJM_CODEC_CELP8_ENC=nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_NOT_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_CELP8_ENC:=nil;
    end;
  SCE_AJM_CODEC_CELP_DEC :
    begin
     if (H.AJM_CODEC_CELP_DEC=nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_NOT_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_CELP_DEC:=nil;
    end;
  SCE_AJM_CODEC_CELP_ENC :
    begin
     if (H.AJM_CODEC_CELP_ENC=nil) then
     begin
      Result:=SCE_AJM_ERROR_CODEC_NOT_REGISTERED;
      id_release(@H.desc);
      Exit;
     end;
     H.AJM_CODEC_CELP_ENC:=nil;
    end;
  else
    begin
     Result:=SCE_AJM_ERROR_INVALID_PARAMETER;
     id_release(@H.desc);
     Exit;
    end;
 end;

 id_release(@H.desc);
end;

function ps4_sceAjmFinalize(uiContext:SceAjmContextId):Integer;
begin
 Result:=0;
 if not id_del(@FAjmMap,uiContext,nil) then Result:=SCE_AJM_ERROR_INVALID_CONTEXT;
end;

function ps4_sceAjmInstanceCodecType(uiInstance:SceAjmInstanceId):SceAjmCodecType;
begin
 Result:=uiInstance shr 14;
end; 

function ps4_sceAjmInstanceCreate(uiContext:SceAjmContextId;
                                  uiCodec:SceAjmCodecType;
                                  uiFlags:QWORD;
                                  pInstance:pSceAjmInstanceId):Integer;
Var
 H:TAjmContext;
begin
 Result:=0;

 H:=id_ctx_get(uiContext);
 if (H=nil) then Exit(SCE_AJM_ERROR_INVALID_CONTEXT);

 Case uiCodec of
  SCE_AJM_CODEC_MP3_DEC  ,
  SCE_AJM_CODEC_AT9_DEC  ,
  SCE_AJM_CODEC_M4AAC_DEC,
  SCE_AJM_CODEC_CELP8_DEC,
  SCE_AJM_CODEC_CELP8_ENC,
  SCE_AJM_CODEC_CELP_DEC ,
  SCE_AJM_CODEC_CELP_ENC :
   begin
    //fake instance
    pInstance^:=1 or (uiCodec shl 14); //sceAjmInstanceCodecType
   end;
  else
    begin
     Result:=SCE_AJM_ERROR_INVALID_PARAMETER;
     id_release(@H.desc);
     Exit;
    end;
 end;

 id_release(@H.desc);
end;

function ps4_sceAjmInstanceDestroy(uiContext:SceAjmContextId;
                                   uiInstance:SceAjmInstanceId):Integer;
Var
 H:TAjmContext;
begin
 Result:=0;

 H:=id_ctx_get(uiContext);
 if (H=nil) then Exit(SCE_AJM_ERROR_INVALID_CONTEXT);

 //

 id_release(@H.desc);
end;

type
 pSceAjmSidebandStreamResult=^SceAjmSidebandStreamResult;
 SceAjmSidebandStreamResult=packed record
  sResult:SceAjmSidebandResult;
  sStream:SceAjmSidebandStream;
 end;

procedure FixSidebandOutput(uiInstance:SceAjmInstanceId;uiFlags:qword;pSidebandOutput:Pointer;szSidebandOutputSize:qword);

 procedure commit(data:Pointer;size:QWORD);
 begin
  if (size>szSidebandOutputSize) then
  begin
   size:=szSidebandOutputSize;
  end;

  Move(data^,pSidebandOutput^,size);

  Inc(pSidebandOutput     ,size);
  Dec(szSidebandOutputSize,size);
 end;

var
 i:Byte;
 c:qword;

 u:record
  case Byte of
    0:(sResult         :SceAjmSidebandResult);
    1:(sStream         :SceAjmSidebandStream);
    2:(sFormat         :SceAjmSidebandFormat);
    3:(sGapless        :SceAjmSidebandGaplessDecode);
    4:(sEngine         :SceAjmSidebandStatisticsEngine);
    5:(sEnginePerCodec :SceAjmSidebandStatisticsEnginePerCodec);
    6:(sMemory         :SceAjmSidebandStatisticsMemory);
    7:(sInit           :SceAjmDecAt9InitializeParameters);
    8:(sMFrame         :SceAjmSidebandMFrame);
    9:(sAt9CodecInfo   :SceAjmSidebandDecAt9CodecInfo);
   10:(sM4aacCodecInfo :SceAjmSidebandDecM4aacCodecInfo);
   11:(sMp3CodecInfo   :SceAjmSidebandDecMp3CodecInfo);
 end;

begin
 //
 u.sResult.iResult        :=0;
 u.sResult.iInternalResult:=0;
 commit(@u.sResult,SizeOf(u.sResult));
 //

 For i:=63 downto 0 do
 begin

  if (szSidebandOutputSize=0) then Break;

  c:=QWORD(1) shl i;

  if ((uiFlags and c)<>0) then
  begin

   case c of

    SCE_AJM_FLAG_SIDEBAND_STREAM:
     begin
      LOG_TRACE('SCE_AJM_FLAG_SIDEBAND_STREAM');
      u.sStream.iSizeConsumed:=48000;
      u.sStream.iSizeProduced:=48000;
      u.sStream.uiTotalDecodedSamples:=48000; //loop or div to zero
      commit(@u.sStream,SizeOf(u.sStream));
     end;

    SCE_AJM_FLAG_SIDEBAND_FORMAT:
     begin
      LOG_TRACE('SCE_AJM_FLAG_SIDEBAND_FORMAT');
      u.sFormat.eChannelNumber     :=1;
      u.sFormat.uiChannelMask      :=1;
      u.sFormat.uiSamplingFrequency:=48000;
      u.sFormat.eSampleEncoding    :=0;
      u.sFormat.uiBitrate          :=1024;
      u.sFormat._reserved          :=0;
      commit(@u.sFormat,SizeOf(u.sFormat));
     end;

    SCE_AJM_FLAG_SIDEBAND_GAPLESS_DECODE:
     begin
      LOG_TRACE('SCE_AJM_FLAG_SIDEBAND_GAPLESS_DECODE');
      u.sGapless.uiTotalSamples  :=1;
      u.sGapless.uiSkipSamples   :=0;
      u.sGapless.uiSkippedSamples:=0;
      commit(@u.sGapless,SizeOf(u.sGapless));
     end;

    SCE_AJM_FLAG_STATISTICS_ENGINE:
     begin
      LOG_TRACE('SCE_AJM_FLAG_STATISTICS_ENGINE');
      u.sEngine.fUsageBatch      :=80;
      u.sEngine.fUsageInterval[0]:=80;
      u.sEngine.fUsageInterval[1]:=80;
      u.sEngine.fUsageInterval[2]:=80;
      commit(@u.sEngine,SizeOf(u.sEngine));
     end;

    SCE_AJM_FLAG_STATISTICS_ENGINE_PER_CODEC:
     begin
      LOG_TRACE('SCE_AJM_FLAG_STATISTICS_ENGINE_PER_CODEC');
      u.sEnginePerCodec.iCodecCount        :=0;
      u.sEnginePerCodec.iCodecId[0]        :=0;
      u.sEnginePerCodec.iCodecId[1]        :=0;
      u.sEnginePerCodec.iCodecId[2]        :=0;
      u.sEnginePerCodec.fCodecPercentage[0]:=80;
      u.sEnginePerCodec.fCodecPercentage[1]:=80;
      u.sEnginePerCodec.fCodecPercentage[2]:=80;
      commit(@u.sEnginePerCodec,SizeOf(u.sEnginePerCodec));
     end;

    SCE_AJM_FLAG_STATISTICS_MEMORY:
     begin
      LOG_TRACE('SCE_AJM_FLAG_STATISTICS_MEMORY');
      u.sMemory.uiInstanceFree:=1;
      u.sMemory.uiBufferFree  :=1;
      u.sMemory.uiBatchSize   :=1;
      u.sMemory.uiInputSize   :=1;
      u.sMemory.uiOutputSize  :=1;
      u.sMemory.uiSmallSize   :=1;
      commit(@u.sMemory,SizeOf(u.sMemory));
     end;

     SCE_AJM_FLAG_CONTROL_INITIALIZE:
      begin
       LOG_TRACE('SCE_AJM_FLAG_CONTROL_INITIALIZE');
       u.sInit.uiConfigData:=0;
       u.sInit._reserved   :=0;
       commit(@u.sInit,SizeOf(u.sInit));
      end;

     SCE_AJM_FLAG_CONTROL_RESET:
      begin
       LOG_TRACE('SCE_AJM_FLAG_CONTROL_RESET');
      end;

     SCE_AJM_FLAG_RUN_MULTIPLE_FRAMES:
      begin
       LOG_TRACE('SCE_AJM_FLAG_RUN_MULTIPLE_FRAMES');
       u.sMFrame.uiNumFrames:=1;
       u.sMFrame._reserved  :=0;
       commit(@u.sMFrame,SizeOf(u.sMFrame));
      end;

     SCE_AJM_FLAG_RUN_GET_CODEC_INFO:
      begin
       case ps4_sceAjmInstanceCodecType(uiInstance) of
        SCE_AJM_CODEC_MP3_DEC:
         begin
          LOG_TRACE('SCE_AJM_FLAG_RUN_GET_CODEC_INFO:SCE_AJM_CODEC_MP3_DEC');
          u.sMp3CodecInfo.uiHeader       :=$00474154;
          u.sMp3CodecInfo.ucCrc          :=1;
          u.sMp3CodecInfo.ucMode         :=0;
          u.sMp3CodecInfo.ucModeExtension:=0;
          u.sMp3CodecInfo.ucCopyright    :=0;
          u.sMp3CodecInfo.ucOriginal     :=0;
          u.sMp3CodecInfo.ucEmphasis     :=0;
          u.sMp3CodecInfo._reserved[0]   :=0;
          u.sMp3CodecInfo._reserved[1]   :=0;
          u.sMp3CodecInfo._reserved[2]   :=0;
          commit(@u.sMp3CodecInfo,SizeOf(u.sMp3CodecInfo));
         end;
        SCE_AJM_CODEC_AT9_DEC:
         begin
          LOG_TRACE('SCE_AJM_FLAG_RUN_GET_CODEC_INFO:SCE_AJM_CODEC_AT9_DEC');
          u.sAt9CodecInfo.uiSuperFrameSize    :=1;
          u.sAt9CodecInfo.uiFramesInSuperFrame:=1;
          u.sAt9CodecInfo.uiNextFrameSize     :=1;
          u.sAt9CodecInfo.uiFrameSamples      :=1;
          commit(@u.sAt9CodecInfo,SizeOf(u.sAt9CodecInfo));
         end;
        SCE_AJM_CODEC_M4AAC_DEC:
         begin
          LOG_TRACE('SCE_AJM_FLAG_RUN_GET_CODEC_INFO:SCE_AJM_CODEC_M4AAC_DEC');
          u.sM4aacCodecInfo.uiHeaac   :=0;
          u.sM4aacCodecInfo.uiReserved:=0;
          commit(@u.sM4aacCodecInfo,SizeOf(u.sM4aacCodecInfo));
         end;
        else
          begin
           LOG_ERROR(stderr,'SCE_AJM_FLAG_RUN_GET_CODEC_INFO:',ps4_sceAjmInstanceCodecType(uiInstance));
           Break;
          end;
       end;
      end;

    else
      begin
       LOG_ERROR(stderr,'Unknow Sideband Flag:1 << ',i);
       Break;
      end;
   end;

  end;
 end;
end;

function ps4_sceAjmBatchJobControlBufferRa(
          pBatchPosition:Pointer;
          uiInstance:SceAjmInstanceId;
          uiFlags:qword;
          pSidebandInput:Pointer;
          szSidebandInputSize:qword;
          pSidebandOutput:Pointer;
          szSidebandOutputSize:qword;
          pReturnAddress:PPointer):Pointer;
begin
 Result:=nil;
 if (pSidebandOutput<>nil) then
 begin
  FillChar(pSidebandOutput^,szSidebandOutputSize,0);
  FixSidebandOutput(uiInstance,uiFlags,pSidebandOutput,szSidebandOutputSize);
 end;

end;

function ps4_sceAjmBatchJobInlineBuffer(
          const pBatchPosition :Pointer;
          const pDataInput     :Pointer;
          const szDataInputSize:QWORD;
          const pBatchAddress  :PPointer):Pointer;
begin
 PDWORD(pBatchPosition)^    :=PDWORD(pBatchPosition)^ and $ffffffe0 or 7;
 PDWORD(pBatchPosition + 4)^:=(szDataInputSize + 7) and $fffffff8;
 Move(pDataInput^, Pointer(pBatchPosition + 8)^, szDataInputSize);
 pBatchAddress^:=(pBatchPosition + 8);
 Result:=pBatchPosition + 8 + ((szDataInputSize + 7) and $fffffffffffffff8);
end;

function ps4_sceAjmBatchJobRunBufferRa(
          pBatchPosition:Pointer;
          uiInstance:SceAjmInstanceId;
          uiFlags:qword;
          pDataInput:Pointer;
          szDataInputSize:qword;
          pDataOutput:Pointer;
          szDataOutputSize:qword;
          pSidebandOutput:Pointer;
          szSidebandOutputSize:qword;
          pReturnAddress:PPointer):Pointer;
begin
 Result:=nil;

 if (pSidebandOutput<>nil) then
 begin
  FillChar(pSidebandOutput^,szSidebandOutputSize,0);
  FixSidebandOutput(uiInstance,uiFlags,pSidebandOutput,szSidebandOutputSize);
 end;

 //FillChar(pDataOutput^,szDataOutputSize,0);

end;

function ps4_sceAjmBatchJobRunSplitBufferRa(
          pBatchPosition:Pointer;
          uiInstance:SceAjmInstanceId;
          uiFlags:qword;
          pDataInputBuffers:pSceAjmBuffer;
          szNumDataInputBuffers:qword;
          pDataOutputBuffers:pSceAjmBuffer;
          szNumDataOutputBuffers:qword;
          pSidebandOutput:Pointer;
          szSidebandOutputSize:qword;
          pReturnAddress:PPointer):Pointer;
var
 i:qword;
begin
 Result:=nil;
 if (pSidebandOutput<>nil) then
 begin
  FillChar(pSidebandOutput^,szSidebandOutputSize,0);
  FixSidebandOutput(uiInstance,uiFlags,pSidebandOutput,szSidebandOutputSize);
 end;

 if (pDataOutputBuffers<>nil) and (szNumDataOutputBuffers<>0) then
 begin
  For i:=0 to szNumDataOutputBuffers-1 do
  if (pDataOutputBuffers[i].pAddress<>nil) then
  begin
   FillChar(pDataOutputBuffers[i].pAddress^,pDataOutputBuffers[i].szSize,0);
  end;
 end;
end;

function ps4_sceAjmBatchStartBuffer(
          uiContext     :SceAjmContextId;
          pBatchCommands:Pointer;
          szBatchSize   :qword;
          iPriority     :Integer;
          pBatchError   :pSceAjmBatchError;
          pBatch        :pSceAjmBatchId):Integer;
begin

 if (pBatchError<>nil) then
 begin
  pBatchError^:=Default(SceAjmBatchError);
 end;

 Result:=0;
end;

function ps4_sceAjmBatchWait(
          uiContext  :SceAjmContextId;
          uiBatch    :SceAjmBatchId;
          uiTimeout  :DWORD;
          pBatchError:pSceAjmBatchError):Integer;
begin

 msleep_td(hz div 2);

 if (pBatchError<>nil) then
 begin
  pBatchError^:=Default(SceAjmBatchError);
 end;

 Result:=0;
end;

function ps4_sceAjmMemoryRegister(
          uiContext:SceAjmContextId;
          pRegion:Pointer;
          szNumPages:QWORD):Integer;
begin
 LOG_TRACE('sceAjmMemoryRegister(0x',HexStr(pRegion),',0x',HexStr(szNumPages,16),')');
 Result:=0;
end;

{$WARN 4110 off}
function Load_libSceAjm(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceAjm');

 lib:=Result^.add_lib('libSceAjm');
 lib.set_proc($765FB87874B352EE,@ps4_sceAjmInitialize);
 lib.set_proc($43777216EC069FAE,@ps4_sceAjmModuleRegister);
 lib.set_proc($5A2EC3B652D5F8A2,@ps4_sceAjmModuleUnregister);
 lib.set_proc($307BABEAA0AC52EB,@ps4_sceAjmFinalize);
 lib.set_proc($7625E340D88CBBFB,@ps4_sceAjmInstanceCodecType);
 lib.set_proc($031A03AC8369E09F,@ps4_sceAjmInstanceCreate);
 lib.set_proc($45B2DBB8ABFCCE1A,@ps4_sceAjmInstanceDestroy);
 lib.set_proc($7660F26CDFFF167F,@ps4_sceAjmBatchJobControlBufferRa);
 lib.set_proc($B2D96086789CDC97,@ps4_sceAjmBatchJobInlineBuffer);
 lib.set_proc($125B25382A4E227B,@ps4_sceAjmBatchJobRunBufferRa);
 lib.set_proc($EE37405CAFB67CCA,@ps4_sceAjmBatchJobRunSplitBufferRa);
 lib.set_proc($7C5164934C5F196B,@ps4_sceAjmBatchStartBuffer);
 lib.set_proc($FEA2EC7C3032C086,@ps4_sceAjmBatchWait);
 lib.set_proc($6E44471181BA9443,@ps4_sceAjmMemoryRegister);
end;

var
 stub:t_int_file;

initialization
 id_table_init(@FAjmMap,1);
 RegisteredInternalFile(stub,'libSceAjm.prx',@Load_libSceAjm);

end.

