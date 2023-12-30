unit ps4_libSceNpInGameMessage;

{$mode objfpc}{$H+}

interface

uses
  windows,
  ps4_libSceNpCommon,
  ps4_program,
  np_error;


const

  NP_IN_GAME_MESSAGE_POOL_SIZE=(16*1024);

  SCE_NP_IN_GAME_MESSAGE_DATA_SIZE_MAX=512;


type

 pSceNpInGameMessageData=^SceNpInGameMessageData;
 SceNpInGameMessageData=packed record
  data:array[0..SCE_NP_IN_GAME_MESSAGE_DATA_SIZE_MAX-1] of AnsiChar;
  dataSize:QWORD;
 end;

 pSceNpPeerAddressA=^SceNpPeerAddressA;
 SceNpPeerAddressA=packed record
  accountId:SceNpAccountId;
  platform:SceNpPlatformType;
  padding:array[0..3] of AnsiChar;
 end;

 ///Callbacks place
 SceUserServiceUserId=Integer;

 SceNpInGameMessageEventCallbackA=procedure(libCtxId,pTo:Integer;pToOnlineId:pSceNpOnlineId;pFrom:Integer;pMessage:pSceNpInGameMessageData;pUserArg:Pointer) SysV_ABI_CDecl;



implementation

///Functions


function ps4_SceNpInGameMessageInitialize(sizePool:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=2;
end;

function ps4_SceNpInGameMessageTerminate(libCtxId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpInGameMessageCreateHandle(libCtxId:Integer):Integer; SysV_ABI_CDecl;
begin
  Result:=3;
end;

function ps4_sceNpInGameMessageDeleteHandle(libCtxId,handleId:Integer):Integer; SysV_ABI_CDecl;
begin
  Result:=0;
end;

function ps4_sceNpInGameMessagePrepareA(libCtxId,handleId:Integer;pReserved:Pointer;cbFunc:SceNpInGameMessageEventCallbackA;pUserArg:Pointer):Integer; SysV_ABI_CDecl;
begin
  Result:=0;
end;

function ps4_sceNpInGameMessageSendDataA(libCtxId:Integer;pTo:pSceNpPeerAddressA;pFrom:pSceNpPeerAddressA;pMessage:pSceNpInGameMessageData):Integer; SysV_ABI_CDecl;
begin
  Result:=0;
end;

function Load_libSceNpInGameMessage(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpInGameMessage');
 lib^.set_proc($1858555294666C71,@ps4_SceNpInGameMessageInitialize);
 lib^.set_proc($6CC1B77159949AE9,@ps4_SceNpInGameMessageTerminate);
 lib^.set_proc($B385046B988125D7,@ps4_sceNpInGameMessageCreateHandle);
 lib^.set_proc($F9A9EE4B1D9ABC74,@ps4_sceNpInGameMessageDeleteHandle);
 lib^.set_proc($2242FAD85329229A,@ps4_sceNpInGameMessagePrepareA);
 lib^.set_proc($3D00C5C5C9EAC6DC,@ps4_sceNpInGameMessageSendDataA);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpInGameMessage.prx',@Load_libSceNpInGameMessage);

end.
