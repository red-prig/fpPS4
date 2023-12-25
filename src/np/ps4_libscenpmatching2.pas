unit ps4_libSceNpMatching2;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  ps4_libSceNpCommon;

implementation

const
 SCE_NP_MATCHING2_ERROR_NOT_INITIALIZED=$80550c03;

type
 pSceNpMatching2InitializeParameter=^SceNpMatching2InitializeParameter;
 SceNpMatching2InitializeParameter=packed record
  poolSize:QWORD;         // 0 = default
  cpuAffinityMask:QWORD;  // 0 = default SceKernelCpumask
  threadPriority:Integer; // 0 = default
  padding:Integer;
  threadStackSize:QWORD;  // 0 = default
  size:QWORD;             // size of this structure
  sslPoolSize:QWORD;      // 0 = default
 end;

 SceNpMatching2ContextId=Word;

 pSceNpMatching2CreateContextParam=^SceNpMatching2CreateContextParam;
 SceNpMatching2CreateContextParam=packed record
  npId        :pSceNpId;
  commId      :pSceNpCommunicationId;
  passPhrase  :pSceNpCommunicationPassphrase;
  serviceLabel:SceNpServiceLabel;
  _align      :DWORD;
  size        :size_t;
 end;

 pSceNpMatching2CreateContextParamA=^SceNpMatching2CreateContextParamA;
 SceNpMatching2CreateContextParamA=packed record
  user        :Integer;
  serviceLabel:SceNpServiceLabel;
  size        :size_t;
 end;

 SceNpMatching2ContextCallback=procedure(
                                ctxId,event:Word;
                                eventCause:Byte;
                                errorCode:Integer;
                                arg:Pointer); SysV_ABI_CDecl;

 SceNpMatching2LobbyEventCallback=procedure(
                                   ctxId:Word;
                                   lobbyId:QWORD;
                                   event:Word;
                                   data:Pointer;
                                   arg:Pointer); SysV_ABI_CDecl;

 SceNpMatching2RoomEventCallback=procedure(
                                  ctxId:Word;
                                  roomId:QWORD;
                                  event:Word;
                                  data:Pointer;
                                  arg:Pointer); SysV_ABI_CDecl;

 SceNpMatching2SignalingCallback=procedure(
                                  ctxId:Word;
                                  roomId:QWORD;
                                  peerMemberId:Word;
                                  event:Word;
                                  errorCode:Integer;
                                  arg:Pointer); SysV_ABI_CDecl;

function ps4_sceNpMatching2Initialize(param:pSceNpMatching2InitializeParameter):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpMatching2RegisterContextCallback(cbFunc:SceNpMatching2ContextCallback;cbFuncArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpMatching2RegisterLobbyEventCallback(ctxId:SceNpMatching2ContextId;
                                                      cbFunc:SceNpMatching2LobbyEventCallback;cbFuncArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpMatching2RegisterRoomEventCallback(ctxId:SceNpMatching2ContextId;
                                                     cbFunc:SceNpMatching2RoomEventCallback;cbFuncArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpMatching2RegisterSignalingCallback(ctxId:SceNpMatching2ContextId;
                                                     cbFunc:SceNpMatching2SignalingCallback;cbFuncArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpMatching2ContextStart(ctxId:SceNpMatching2ContextId;timeout:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpMatching2CreateContext(param:pSceNpMatching2CreateContextParam;
                                         ctxId:PWord):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpMatching2CreateContext,serviceLabel=',param^.serviceLabel,',size=',param^.size);
 Result:=0;
end;

function ps4_sceNpMatching2CreateContextA(param:pSceNpMatching2CreateContextParamA;
                                          ctxId:PWord):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpMatching2CreateContextA,serviceLabel=',param^.serviceLabel,',size=',param^.size);
 Result:=0;
end;

function ps4_sceNpMatching2Terminate():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceNpMatching2(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpMatching2');
 lib^.set_proc($D74B777B9F893E75,@ps4_sceNpMatching2Initialize);
 lib^.set_proc($7D041F3FCEC8EE1B,@ps4_sceNpMatching2RegisterContextCallback);
 lib^.set_proc($E0D8FBBB9079C820,@ps4_sceNpMatching2RegisterLobbyEventCallback);
 lib^.set_proc($A7ED849F199A00C3,@ps4_sceNpMatching2RegisterRoomEventCallback);
 lib^.set_proc($D1431E5911A764A0,@ps4_sceNpMatching2RegisterSignalingCallback);
 lib^.set_proc($EEF8CD43A675A29D,@ps4_sceNpMatching2ContextStart);
 lib^.set_proc($61F9A95BBD7DACCA,@ps4_sceNpMatching2CreateContext);
 lib^.set_proc($6A3BF373C7B6BA9A,@ps4_sceNpMatching2CreateContextA);
 lib^.set_proc($32AA77949FAC8F2E,@ps4_sceNpMatching2Terminate);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpMatching2.prx',@Load_libSceNpMatching2);

end.

