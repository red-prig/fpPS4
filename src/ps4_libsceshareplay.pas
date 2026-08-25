unit ps4_libSceSharePlay;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib,
 ps4_libSceNpCommon,
 ps4_libSceUserService;

implementation

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

const
 SCE_SHARE_PLAY_ERROR_INVALID_ARGS       =-2129788927; // 0x810E0001
 SCE_SHARE_PLAY_ERROR_OUT_OF_MEMORY      =-2129788926; // 0x810E0002
 SCE_SHARE_PLAY_ERROR_ALREADY_INITIALIZED=-2129788925; // 0x810E0003
 SCE_SHARE_PLAY_ERROR_NOT_INITIALIZED    =-2129788924; // 0x810E0004
 SCE_SHARE_PLAY_ERROR_FATAL              =-2129788923; // 0x810E0005

 SCE_SHARE_PLAY_HEAP_SIZE=6*1024;

 //SceSharePlayConnectionStatus
 SCE_SHARE_PLAY_CONNECTION_STATUS_DORMANT  =0;
 SCE_SHARE_PLAY_CONNECTION_STATUS_READ     =1;
 SCE_SHARE_PLAY_CONNECTION_STATUS_CONNECTED=2;

 //SceSharePlayControllerMode
 SCE_SHARE_PLAY_CONTROLLER_MODE_WATCHING_HOST_PLAY    =0;
 SCE_SHARE_PLAY_CONTROLLER_MODE_PLAYING_AS_HOST       =1;
 SCE_SHARE_PLAY_CONTROLLER_MODE_PLAYING_GAME_WITH_HOST=2;

 //SceSharePlayProhibitionMode
 SCE_SHARE_PLAY_PROHIBITION_MODE_OFF           =0;
 SCE_SHARE_PLAY_PROHIBITION_MODE_CONTROL_ONLY  =1;
 SCE_SHARE_PLAY_PROHIBITION_MODE_CONTROL_SCREEN=2;

type
 pSceSharePlayConnectionInfo=^SceSharePlayConnectionInfo;
 SceSharePlayConnectionInfo=packed record
  status         :Integer; //SceSharePlayConnectionStatus
  mode           :Integer; //SceSharePlayControllerMode
  hostOnlineId   :SceNpOnlineId;
  visitorOnlineId:SceNpOnlineId;
  hostUserId     :SceUserServiceUserId;
  visitorUserId  :SceUserServiceUserId;
 end;

 pSceSharePlayConnectionInfoA=^SceSharePlayConnectionInfoA;
 SceSharePlayConnectionInfoA=packed record
  status          :Integer; //SceSharePlayConnectionStatus
  mode            :Integer; //SceSharePlayControllerMode
  hostOnlineId    :SceNpOnlineId;
  visitorOnlineId :SceNpOnlineId;
  hostAccountId   :SceNpAccountId;
  visitorAccountId:SceNpAccountId;
  hostUserId      :SceUserServiceUserId;
  visitorUserId   :SceUserServiceUserId;
 end;

function ps4_sceSharePlayInitialize(pHeap:Pointer;heapSize:qword):Integer;
begin
 LOG_TRACE('sceSharePlayInitialize:',HexStr(pHeap),':',heapSize);
 Result:=0;
end;

function ps4_sceSharePlaySetProhibition(mode:Integer):Integer;
begin
 LOG_INFO('sceSharePlaySetProhibition,mode=',mode);
 Result:=0;
end;

function ps4_sceSharePlayGetCurrentConnectionInfo(pInfo:pSceSharePlayConnectionInfo):Integer;
begin
 if (pInfo=nil) then Exit(SCE_SHARE_PLAY_ERROR_INVALID_ARGS);

 pInfo^:=Default(SceSharePlayConnectionInfo);
 Result:=0;
end;

function ps4_sceSharePlayGetCurrentConnectionInfoA(pInfo:pSceSharePlayConnectionInfoA):Integer;
begin
 if (pInfo=nil) then Exit(SCE_SHARE_PLAY_ERROR_INVALID_ARGS);

 pInfo^:=Default(SceSharePlayConnectionInfoA);
 Result:=0;
end;

{$WARN 4110 off}
function Load_libSceSharePlay(name:pchar):p_lib_info;
var
  lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceSharePlay');

 lib:=Result^.add_lib('libSceSharePlay');
 lib.set_proc($8ACAEEAAD86961CC,@ps4_sceSharePlayInitialize);
 lib.set_proc($728D8D0A3FFFA677,@ps4_sceSharePlaySetProhibition);
 lib.set_proc($38EACB281D1B483B,@ps4_sceSharePlayGetCurrentConnectionInfo);
 lib.set_proc($F8C09726559D8BEB,@ps4_sceSharePlayGetCurrentConnectionInfoA);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceSharePlay.prx',@Load_libSceSharePlay);

end.

