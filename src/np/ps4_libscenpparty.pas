unit ps4_libSceNpParty;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib,
 ps4_libSceNpCommon;

const
 SCE_NP_PARTY_MEMBER_NUM_MAX=8;

type
 pSceNpPartyInitializeParam=^SceNpPartyInitializeParam;
 SceNpPartyInitializeParam=packed record
  sdkVersion:DWORD;
 end;

 pSceNpPartyState=^SceNpPartyState;
 SceNpPartyState=Word;

 pSceNpPartyRoomMemberId=^SceNpPartyRoomMemberId;
 SceNpPartyRoomMemberId=Word;

 pSceNpPartyMemberVoiceState=^SceNpPartyMemberVoiceState;
 SceNpPartyMemberVoiceState=Byte;

 pSceNpPeerAddressA=^SceNpPeerAddressA;
 SceNpPeerAddressA=packed record
  accountId:SceNpAccountId;
  platform :SceNpPlatformType;
  padding  :Integer;
 end;

 pSceNpPartyMemberFlags=^SceNpPartyMemberFlags;
 SceNpPartyMemberFlags=Byte;

 pSceNpPartyMemberInfo=^SceNpPartyMemberInfo;
 SceNpPartyMemberInfo=packed record
  peerAddress:SceNpPeerAddressA;
  onlineId   :SceNpOnlineId;
  memberId   :SceNpPartyRoomMemberId;
  memberFlags:SceNpPartyMemberFlags;
  reserved   :Byte;
 end;

 pSceNpPartyMemberVoiceInfo=^SceNpPartyMemberVoiceInfo;
 SceNpPartyMemberVoiceInfo=packed record
  memberId           :SceNpPartyRoomMemberId;
  memberVoiceState   :SceNpPartyMemberVoiceState;
  reserved           :Byte;
  destinationMemberId:SceNpPartyRoomMemberId;
 end;

 pSceNpPartyMemberList=^SceNpPartyMemberList;
 SceNpPartyMemberList=packed record
  members     :array[0..SCE_NP_PARTY_MEMBER_NUM_MAX-1] of SceNpPartyMemberInfo;
  memberIds   :array[0..SCE_NP_PARTY_MEMBER_NUM_MAX-1] of SceNpPartyRoomMemberId;
  memberNum   :Byte;
  privateParty:Boolean;
  reserved    :Word;
 end;

 SceNpPartyBinaryMessageEventHandler=procedure(
                                        event:Word;
                                        const data:Pointer;
                                        userdata:Pointer);

 SceNpPartyRoomEventHandler=procedure(
                               eventType:Word;
                               const data:Pointer;
                               userdata:Pointer);

 SceNpPartyVoiceEventHandler=procedure(
                               const memberVoiceInfo:SceNpPartyMemberVoiceInfo;
                               userdata:Pointer);

 pSceNpPartyEventHandlers=^SceNpPartyEventHandlers;
 SceNpPartyEventHandlers=packed record
  sdkVersion               :DWORD;
  reserved                 :DWORD;
  roomEvenHandler          :SceNpPartyRoomEventHandler;
  voiceEventHandler        :SceNpPartyVoiceEventHandler;
  binaryMessageEventHandler:SceNpPartyBinaryMessageEventHandler;
  reserved2                :Pointer;
 end;

implementation

function ps4_sceNpPartyInitialize(const param:pSceNpPartyInitializeParam):Integer;
begin
 Result:=0;
end;

function ps4_sceNpPartyRegisterHandler(const handlers:pSceNpPartyEventHandlers;userdata:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpPartyRegisterHandlerA(const handlers:pSceNpPartyEventHandlers;userdata:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpPartyGetState(const state:pSceNpPartyState):Integer;
begin
 Result:=0;
end;

function ps4_sceNpPartyGetMembers(const memberList:pSceNpPartyMemberList):Integer;
begin
 Result:=0;
end;

function ps4_sceNpPartyGetMemberInfo(const memberId:SceNpPartyRoomMemberId;
                                     const memberInfo:pSceNpPartyMemberInfo):Integer;
begin
 Result:=0;
end;

function ps4_sceNpPartyCheckCallback():Integer;
begin
 Result:=0;
end;

{$WARN 4110 off}
function Load_libSceNpParty(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpParty');

 lib:=Result^.add_lib('libSceNpParty');
 lib.set_proc($9616024D098191DB,@ps4_sceNpPartyInitialize);
 lib.set_proc($900F3C81BBFBD5AA,@ps4_sceNpPartyRegisterHandler);
 lib.set_proc($FAFE1F5473301567,@ps4_sceNpPartyRegisterHandlerA);
 lib.set_proc($684CCA749CC04D9D,@ps4_sceNpPartyGetState);
 lib.set_proc($4F650E29FD3464DD,@ps4_sceNpPartyGetMembers);
 lib.set_proc($1753FEFF0A71428C,@ps4_sceNpPartyGetMemberInfo);
 lib.set_proc($DDEE24DA6CCB9267,@ps4_sceNpPartyCheckCallback);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpParty.prx',@Load_libSceNpParty);

end.

