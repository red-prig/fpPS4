unit ps4_libSceNpParty;

{$mode ObjFPC}{$H+}

interface

uses
  windows,
  ps4_program,
  np_error,
  ps4_libSceNpCommon;

type
  pSceNpPartyInitializeParam=^SceNpPartyInitializeParam;
  SceNpPartyInitializeParam=packed record
  sdkVersion:DWORD;
end;

  pSceNpPartyState=^SceNpPartyState;
  SceNpPartyState=packed record
end;

  pSceNpPartyRoomMemberId=^SceNpPartyRoomMemberId;
  SceNpPartyRoomMemberId=packed record
end;

  pSceNpPartyMemberVoiceState=^SceNpPartyMemberVoiceState;
  SceNpPartyMemberVoiceState=packed record
end;

  pSceNpPeerAddressA=^SceNpPeerAddressA;
  SceNpPeerAddressA=packed record
  accountId:SceNpAccountId;
  platform:SceNpPlatformType;
  padding:char;
end;

  pSceNpPartyMemberFlags=^SceNpPartyMemberFlags;
  SceNpPartyMemberFlags=packed record
end;

  pSceNpPartyMemberInfo=^SceNpPartyMemberInfo;
  SceNpPartyMemberInfo=packed record
  peerAddress:SceNpPeerAddressA;
  onlineId:   SceNpOnlineId;
  memberId:   SceNpPartyRoomMemberId;
  memberFlags:SceNpPartyMemberFlags;
  reserved:   WORD;
end;

  pSceNpPartyMemberVoiceInfo=^SceNpPartyMemberVoiceInfo;
  SceNpPartyMemberVoiceInfo=packed record
  memberId:           SceNpPartyRoomMemberId;
  memberVoiceState:   SceNpPartyMemberVoiceState;
  reserved:           Word;
  destinationMemberId:SceNpPartyRoomMemberId;
end;

  pSceNpPartyMemberList=^SceNpPartyMemberList;
  SceNpPartyMemberList=packed record
  members:SceNpPartyMemberInfo;
  memberNum:Byte;
  reserved2:bool;
  reserved:array[0..5] of Byte;
end;

  SceNpPartyBinaryMessageEventHandler=procedure(
                                         event:Word;
                                         const data:Pointer;
                                         userdata:Pointer); SysV_ABI_CDecl;

  SceNpPartyRoomEventHandler=procedure(
                                eventType:Word;
                                const data:Pointer;
                                userdata:Pointer); SysV_ABI_CDecl;

  SceNpPartyVoiceEventHandler=procedure(
                                const memberVoiceInfo:SceNpPartyMemberVoiceInfo;
                                userdata:Pointer); SysV_ABI_CDecl;

  pSceNpPartyEventHandlers=^SceNpPartyEventHandlers;
  SceNpPartyEventHandlers=packed record
  sdkVersion:               DWORD;
  reserved:                 DWORD;
  roomEvenHandler:          SceNpPartyRoomEventHandler;
  voiceEventHandler:        SceNpPartyVoiceEventHandler;
  binaryMessageEventHandler:SceNpPartyBinaryMessageEventHandler;
  reserved2:                Pointer;
end;

implementation

function ps4_sceNpPartyInitialize(const param:pSceNpPartyInitializeParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpPartyRegisterHandler(const handlers:pSceNpPartyEventHandlers;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpPartyGetState(const state:pSceNpPartyState):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpPartyGetMembers(const memberList:pSceNpPartyMemberList):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpPartyGetMemberInfo(const memberId:SceNpPartyRoomMemberId;
                                     const memberInfo:pSceNpPartyMemberInfo):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpPartyCheckCallback():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceNpParty(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpParty');
 lib^.set_proc($9616024D098191DB,@ps4_sceNpPartyInitialize);
 lib^.set_proc($900F3C81BBFBD5AA,@ps4_sceNpPartyRegisterHandler);
 lib^.set_proc($684CCA749CC04D9D,@ps4_sceNpPartyGetState);
 lib^.set_proc($4F650E29FD3464DD,@ps4_sceNpPartyGetMembers);
 lib^.set_proc($1753FEFF0A71428C,@ps4_sceNpPartyGetMemberInfo);
 lib^.set_proc($DDEE24DA6CCB9267,@ps4_sceNpPartyCheckCallback);

end;

initialization
 ps4_app.RegistredPreLoad('libSceNpParty.prx',@Load_libSceNpParty);

end.