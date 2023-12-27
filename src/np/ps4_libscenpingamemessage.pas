unit ps4_libSceNpInGameMessage;

{$mode objfpc}{$H+}

interface

uses
  windows,
  ps4_libSceNpCommon,
  ps4_libSceNpManager,
  ps4_program,
  np_error;


const

  NP_IN_GAME_MESSAGE_POOL_SIZE=(16 * 1024);

  SCE_NP_IN_GAME_MESSAGE_DATA_SIZE_MAX=512;

var

  number:Integer=0;


type
 pSceNpInGameMessageData=^SceNpInGameMessageData;
 SceNpInGameMessageData=packed record
  data:array[0..SCE_NP_IN_GAME_MESSAGE_DATA_SIZE_MAX-1] of AnsiChar;
  dataSize:QWORD;
 end;

 ///Callbacks place
 SceUserServiceUserId=Integer;

 SceNpInGameMessageEventCallbackA=procedure(libCtxId,pTo:Integer;pToOnlineId:pSceNpOnlineId;pFrom:Integer;pMessage:pSceNpInGameMessageData;pUserArg:Pointer) SysV_ABI_CDecl;



 implementation
 ///Functions


function ps4_SceNpInGameMessageInitialize(poolSize:QWORD;pOption:Pointer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpInGameMessageInitialize:', poolSize);
 Result:=4;
end;


function ps4_SceNpInGameMessageTerminate(libCtxId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;


function ps4_sceNpInGameMessageCreateHandle(libCtxId:Integer):Integer; SysV_ABI_CDecl;

begin
  if (libCtxId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
  Result:=5;  ///Needs positive value to create handle
end;

function ps4_sceNpInGameMessageDeleteHandle(libCtxId,handleId:Integer):Integer; SysV_ABI_CDecl;
begin
  Result:=0;
end;

function ps4_sceNpInGameMessagePrepareA(libCtxId,handleId:Integer;pReserved:Pointer;cbFunc:pSceNpInGameMessageEventCallbackA;pUserArg:Pointer):Integer; SysV_ABI_CDecl;
begin
  Result:=0;
end;

function ps4_sceNpInGameMessageSendDataA(libCtxId:Integer;pTo:pSceNpPeerAddressA;pFrom:pSceNpPeerAddressA;pMessage:pSceNpInGameMessageData):Integer; SysV_ABI_CDecl;
begin
  if (libCtxId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
  if (pMessage=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
  Result:=0;
end;

function Load_libSceNpInGameMessage(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpInGameMessage');
 lib^.set_proc($1858555294666C71,@ps4_sceNpInGameMessageInitialize);
 ///lib^.set_proc($,@ps4_SceNpInGameMessageTerminate);
 ///lib^.set_proc($,@ps4_sceNpInGameMessageCreateHandle);
 ///lib^.set_proc($,@ps4_sceNpInGameMessageDeleteHandle);
 ///lib^.set_proc($,@ps4_sceNpInGameMessagePrepareA);
 ///lib^.set_proc($,@ps4_sceNpInGameMessageSendDataA);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpInGameMessage.prx',@Load_libSceNpInGameMessage);

end.
