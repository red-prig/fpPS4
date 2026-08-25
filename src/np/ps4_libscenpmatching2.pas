unit ps4_libSceNpMatching2;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib;

implementation

uses
 ps4_libSceUserService,
 ps4_libSceNpCommon;

const
 SCE_NP_MATCHING2_ERROR_NOT_INITIALIZED=$80550c03;

type
 pSceNpMatching2InitializeParameter=^SceNpMatching2InitializeParameter;
 SceNpMatching2InitializeParameter=packed record
  poolSize       :QWORD;   // 0 = default
  cpuAffinityMask:QWORD;   // 0 = default SceKernelCpumask
  threadPriority :Integer; // 0 = default
  padding:Integer;
  threadStackSize:QWORD;   // 0 = default
  size           :QWORD;   // size of this structure
  sslPoolSize    :QWORD;   // 0 = default
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
  user        :SceUserServiceUserId;
  serviceLabel:SceNpServiceLabel;
  size        :size_t;
 end;

 SceNpMatching2RequestCallback=procedure(
                                ctxId    :Word;
                                reqId    :DWORD;
                                event    :Word;
                                errorCode:Integer;
                                data     :Pointer;
                                arg      :Pointer);

 pSceNpMatching2RequestOptParam=^SceNpMatching2RequestOptParam;
 SceNpMatching2RequestOptParam=packed record
  cbFunc   :SceNpMatching2RequestCallback;
  cbFuncArg:Pointer;
  timeout  :DWORD;
  appReqId :Word;
  padding  :Word;
 end;

 SceNpMatching2ContextCallback=procedure(
                                ctxId     :Word;
                                event     :Word;
                                eventCause:Byte;
                                errorCode :Integer;
                                arg       :Pointer);

 SceNpMatching2LobbyEventCallback=procedure(
                                   ctxId  :Word;
                                   lobbyId:QWORD;
                                   event  :Word;
                                   data   :Pointer;
                                   arg    :Pointer);

 SceNpMatching2RoomEventCallback=procedure(
                                  ctxId :Word;
                                  roomId:QWORD;
                                  event :Word;
                                  data  :Pointer;
                                  arg   :Pointer);

 SceNpMatching2SignalingCallback=procedure(
                                  ctxId       :Word;
                                  roomId      :QWORD;
                                  peerMemberId:Word;
                                  event       :Word;
                                  errorCode   :Integer;
                                  arg         :Pointer);

  SceNpMatching2RoomMessageCallback=procedure(
                                     ctxId      :Word;
                                     roomId     :QWORD;
                                     srcMemberId:Word;
                                     event      :Word;
                                     data       :Pointer;
                                     arg        :Pointer);

function ps4_sceNpMatching2Initialize(param:pSceNpMatching2InitializeParameter):Integer;
begin
 Result:=0;
end;

function ps4_sceNpMatching2RegisterContextCallback(cbFunc   :SceNpMatching2ContextCallback;
                                                   cbFuncArg:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpMatching2RegisterLobbyEventCallback(ctxId    :SceNpMatching2ContextId;
                                                      cbFunc   :SceNpMatching2LobbyEventCallback;
                                                      cbFuncArg:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpMatching2RegisterRoomEventCallback(ctxId    :SceNpMatching2ContextId;
                                                     cbFunc   :SceNpMatching2RoomEventCallback;
                                                     cbFuncArg:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpMatching2RegisterSignalingCallback(ctxId    :SceNpMatching2ContextId;
                                                     cbFunc   :SceNpMatching2SignalingCallback;
                                                     cbFuncArg:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpMatching2RegisterRoomMessageCallback(ctxId    :SceNpMatching2ContextId;
                                                       cbFunc   :SceNpMatching2RoomMessageCallback;
                                                       cbFuncArg:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpMatching2ContextStart(ctxId  :SceNpMatching2ContextId;
                                        timeout:QWORD):Integer;
begin
 Result:=0;
end;

function ps4_sceNpMatching2CreateContext(param:pSceNpMatching2CreateContextParam;
                                         ctxId:PWord):Integer;
begin
 Result:=0;
end;

function ps4_sceNpMatching2CreateContextA(param:pSceNpMatching2CreateContextParamA;
                                          ctxId:PWord):Integer;
begin
 Result:=0;
end;

function ps4_sceNpMatching2SetDefaultRequestOptParam(ctxId   :SceNpMatching2ContextId;
                                                     optParam:pSceNpMatching2RequestOptParam):Integer;
begin
 Result:=0;
end;

function ps4_sceNpMatching2Terminate():Integer;
begin
 Result:=0;
end;

{$WARN 4110 off}
function Load_libSceNpMatching2(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpMatching2');

 lib:=Result^.add_lib('libSceNpMatching2');
 lib.set_proc($D74B777B9F893E75,@ps4_sceNpMatching2Initialize);
 lib.set_proc($7D041F3FCEC8EE1B,@ps4_sceNpMatching2RegisterContextCallback);
 lib.set_proc($E0D8FBBB9079C820,@ps4_sceNpMatching2RegisterLobbyEventCallback);
 lib.set_proc($A7ED849F199A00C3,@ps4_sceNpMatching2RegisterRoomEventCallback);
 lib.set_proc($B81112CF3E02430B,@ps4_sceNpMatching2RegisterRoomMessageCallback);
 lib.set_proc($D1431E5911A764A0,@ps4_sceNpMatching2RegisterSignalingCallback);
 lib.set_proc($EEF8CD43A675A29D,@ps4_sceNpMatching2ContextStart);
 lib.set_proc($61F9A95BBD7DACCA,@ps4_sceNpMatching2CreateContext);
 lib.set_proc($6A3BF373C7B6BA9A,@ps4_sceNpMatching2CreateContextA);
 lib.set_proc($FBC7BBC172E68DDB,@ps4_sceNpMatching2SetDefaultRequestOptParam);
 lib.set_proc($32AA77949FAC8F2E,@ps4_sceNpMatching2Terminate);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpMatching2.prx',@Load_libSceNpMatching2);

end.

