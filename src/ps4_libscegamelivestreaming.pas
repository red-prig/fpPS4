unit ps4_libSceGameLiveStreaming;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib;

const
 SCE_GAME_LIVE_STREAMING_MAX_SOCIAL_FEEDBACK_PRESET_TEXT_LENGTH=32;
 SCE_GAME_LIVE_STREAMING_MAX_SPOILER_TAG_LENGTH=32;
 MB_LEN_MAX=2;

 SCE_GAME_LIVE_STREAMING_ERROR_UNKNOWN                   =-2136997887; //0x80A00001
 SCE_GAME_LIVE_STREAMING_ERROR_INVALID_PARAM             =-2136997886; //0x80A00002
 SCE_GAME_LIVE_STREAMING_ERROR_INVALID_STATUS            =-2136997885; //0x80A00003
 SCE_GAME_LIVE_STREAMING_ERROR_NOT_INITIALIZED           =-2136997884; //0x80A00004
 SCE_GAME_LIVE_STREAMING_ERROR_ENCODER                   =-2136997883; //0x80A00005
 SCE_GAME_LIVE_STREAMING_ERROR_OUT_OF_MEMORY             =-2136997882; //0x80A00006
 SCE_GAME_LIVE_STREAMING_ERROR_NOT_FOUND                 =-2136997881; //0x80A00007
 SCE_GAME_LIVE_STREAMING_ERROR_NOT_SUPPORTED             =-2136997880; //0x80A00008
 SCE_GAME_LIVE_STREAMING_ERROR_MESSAGE_FILTER            =-2136997879; //0x80A00009
 SCE_GAME_LIVE_STREAMING_ERROR_SCREEN_NOT_INITIALIZED    =-2136997878; //0x80A0000A
 SCE_GAME_LIVE_STREAMING_ERROR_SCREEN_RESOLUTION_MISMATCH=-2136997877; //0x80A0000B
 SCE_GAME_LIVE_STREAMING_ERROR_PARAMSFO_NOT_AUTHORIZED   =-2136997876; //0x80A0000C

 //SceGameLiveStreamingVideoResolution
 SCE_GAME_LIVE_STREAMING_VIDEO_RESOLUTION_640x360  =$00000001;
 SCE_GAME_LIVE_STREAMING_VIDEO_RESOLUTION_960x540  =$00000002;
 SCE_GAME_LIVE_STREAMING_VIDEO_RESOLUTION_1280x720 =$00000004;
 SCE_GAME_LIVE_STREAMING_VIDEO_RESOLUTION_1920x1080=$00000008;

 //SceGameLiveStreamingCameraFramePosition
 SCE_GAME_LIVE_STREAMING_CAMERA_FRAME_POSITION_TOP_LEFT    =$01;
 SCE_GAME_LIVE_STREAMING_CAMERA_FRAME_POSITION_TOP_RIGHT   =$02;
 SCE_GAME_LIVE_STREAMING_CAMERA_FRAME_POSITION_BOTTOM_LEFT =$03;
 SCE_GAME_LIVE_STREAMING_CAMERA_FRAME_POSITION_BOTTOM_RIGHT=$04;

 //SceGameLiveStreamingSocialFeedbackMessageType
 SCE_GAME_LIVE_STREAMING_SOCIAL_FEEDBACK_MESSAGE_TYPE_MESSAGE=$01;
 SCE_GAME_LIVE_STREAMING_SOCIAL_FEEDBACK_MESSAGE_TYPE_COMMAND=$02;
 SCE_GAME_LIVE_STREAMING_SOCIAL_FEEDBACK_MESSAGE_TYPE_TEXT   =$03;

type
 pSceGameLiveStreamingStatus=^SceGameLiveStreamingStatus;
 SceGameLiveStreamingStatus=packed record
  isOnAir        :Boolean;
  _align         :array[0..2] of Byte;
  spectatorCounts:DWORD;
  userId         :Integer;
  reserved       :array[0..59] of Byte;
 end;

 pSceGameLiveStreamingStatus2=^SceGameLiveStreamingStatus2;
 SceGameLiveStreamingStatus2=packed record
  userId                  :Integer;
  isOnAir                 :Boolean;
  _align                  :array[0..2] of Byte;
  spectatorCounts         :DWORD;
  textMessageCounts       :DWORD;
  commandMessageCounts    :DWORD;
  broadcastVideoResolution:Integer; //SceGameLiveStreamingVideoResolution
  reserved                :array[0..47] of Byte;
 end;

 pSceGameLiveStreamingCameraFrameSetting=^SceGameLiveStreamingCameraFrameSetting;
 SceGameLiveStreamingCameraFrameSetting=packed record
  position       :Integer; //SceGameLiveStreamingCameraFramePosition
  alpha          :Single;
  isDisableCamera:Boolean;
  reserved       :array[0..30] of Byte;
 end;

 pSceGameLiveStreamingPresetSocialFeedback=^SceGameLiveStreamingPresetSocialFeedback;
 SceGameLiveStreamingPresetSocialFeedback=packed record
  commandId  :DWORD;
  commandText:array[0..
                    (SCE_GAME_LIVE_STREAMING_MAX_SOCIAL_FEEDBACK_PRESET_TEXT_LENGTH*
                     MB_LEN_MAX+1)
                   -1] of char;
  reserved   :array[0..30] of Byte;
 end;

 pSceGameLiveStreamingSocialFeedbackMessageType=^SceGameLiveStreamingSocialFeedbackMessageType;
 SceGameLiveStreamingSocialFeedbackMessageType=Integer;

 pSceGameLiveStreamingSpoilerTag=^SceGameLiveStreamingSpoilerTag;
 SceGameLiveStreamingSpoilerTag=packed record
  tagText :array[0..
                (SCE_GAME_LIVE_STREAMING_MAX_SPOILER_TAG_LENGTH*
                 MB_LEN_MAX+1)
               -1] of char;
  reserved:array[0..30] of Byte;
 end; 

implementation

function ps4_sceGameLiveStreamingInitialize(heapSize:qword):Integer;
begin
 Writeln('sceGameLiveStreamingInitialize:',heapSize);
 Result:=0;
end;

function ps4_sceGameLiveStreamingPermitServerSideRecording(isPermit:Boolean):Integer;
begin
 Writeln('sceGameLiveStreamingPermitServerSideRecording,isPermit=',isPermit);
 Result:=0;
end;

function ps4_sceGameLiveStreamingEnableLiveStreaming(isEnable:Boolean):Integer;
begin
 Writeln('sceGameLiveStreamingEnableLiveStreaming,isEnable=',isEnable);
 Result:=0;
end;

function ps4_sceGameLiveStreamingPermitLiveStreaming(isPermit:Boolean):Integer;
begin
 Writeln('sceGameLiveStreamingPermitLiveStreaming,isPermit=',isPermit);
 Result:=0;
end;

function ps4_sceGameLiveStreamingEnableSocialFeedback(isEnableSocialFeedback:Boolean):Integer;
begin
 Writeln('sceGameLiveStreamingEnableSocialFeedback,isEnableSocialFeedback=',isEnableSocialFeedback);
 Result:=0;
end;

function ps4_sceGameLiveStreamingGetCurrentStatus(status:pSceGameLiveStreamingStatus):Integer;
begin
 if (status=nil) then Exit(SCE_GAME_LIVE_STREAMING_ERROR_INVALID_PARAM);

 status^:=Default(SceGameLiveStreamingStatus);
 status^.isOnAir:=False;

 Result:=0;
end;

function ps4_sceGameLiveStreamingGetCurrentStatus2(status:pSceGameLiveStreamingStatus2):Integer;
begin
 if (status=nil) then Exit(SCE_GAME_LIVE_STREAMING_ERROR_INVALID_PARAM);

 status^:=Default(SceGameLiveStreamingStatus2);
 status^.isOnAir:=False;

 Result:=0;
end;

function ps4_sceGameLiveStreamingSetCameraFrameSetting(setting:pSceGameLiveStreamingCameraFrameSetting):Integer;
begin
 if (setting=nil) then Exit(SCE_GAME_LIVE_STREAMING_ERROR_INVALID_PARAM);

 Result:=0;
end;

function ps4_sceGameLiveStreamingSetMaxBitrate(isSetMaxBitrate:DWORD):Integer;
begin
 Writeln('sceGameLiveStreamingSetMaxBitrate,isSetMaxBitrate=',isSetMaxBitrate);
 Result:=0;
end;
 
function ps4_sceGameLiveStreamingSetStandbyScreenResource(resource:PChar):Integer;
begin
 Writeln('sceGameLiveStreamingSetStandbyScreenResource:',resource);
 Result:=0;
end;

function ps4_sceGameLiveStreamingSetPresetSocialFeedbackCommands(const presetCounts:QWORD;
                                                                 preset:pSceGameLiveStreamingPresetSocialFeedback):Integer;
begin
 Result:=0;
end;

function ps4_sceGameLiveStreamingGetSocialFeedbackMessagesCount(const _type:SceGameLiveStreamingSocialFeedbackMessageType;
                                                                count:PInteger):Integer;
begin
 Result:=0;
end;

function ps4_sceGameLiveStreamingSetSpoilerTag(const spoilerTagCounts:QWORD;
                                               const spoilerTag:pSceGameLiveStreamingSpoilerTag):Integer;
begin
 Result:=0;
end;

function Load_libSceGameLiveStreaming(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceGameLiveStreaming');

 lib:=Result^.add_lib('libSceGameLiveStreaming');
 lib.set_proc($92F604C369419DD9,@ps4_sceGameLiveStreamingInitialize);
 lib.set_proc($FC41E753AF201315,@ps4_sceGameLiveStreamingPermitServerSideRecording);
 lib.set_proc($75633CD005F7F68E,@ps4_sceGameLiveStreamingEnableLiveStreaming);
 lib.set_proc($2B443111B0FBABE7,@ps4_sceGameLiveStreamingPermitLiveStreaming);
 lib.set_proc($C013905A36D631F5,@ps4_sceGameLiveStreamingEnableSocialFeedback);
 lib.set_proc($0A83CCC77EBD12A3,@ps4_sceGameLiveStreamingGetCurrentStatus);
 lib.set_proc($94AF1D2C1369F4E1,@ps4_sceGameLiveStreamingGetCurrentStatus2);
 lib.set_proc($DCF4A2C00CC52121,@ps4_sceGameLiveStreamingSetCameraFrameSetting);
 lib.set_proc($ABF931B9A17B5115,@ps4_sceGameLiveStreamingSetMaxBitrate);
 lib.set_proc($30BBD823CE85140A,@ps4_sceGameLiveStreamingSetStandbyScreenResource);
 lib.set_proc($C9CA1D88FD88D31A,@ps4_sceGameLiveStreamingSetPresetSocialFeedbackCommands);
 lib.set_proc($C9E40A8C71138B8D,@ps4_sceGameLiveStreamingGetSocialFeedbackMessagesCount);
 lib.set_proc($66E5FECF3CF60E40,@ps4_sceGameLiveStreamingSetSpoilerTag);
end;

var
 stub:t_int_file;

initialization
 reg_int_file(stub,'libSceGameLiveStreaming.prx',@Load_libSceGameLiveStreaming);

end.

