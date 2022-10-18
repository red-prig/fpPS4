unit ps4_libSceUserService;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

const
 SCE_USER_SERVICE_MAX_LOGIN_USERS=4;
 SCE_USER_SERVICE_USER_ID_INVALID=Integer($FFFFFFFF);

type
 PUserServiceInitializeParams=^TUserServiceInitializeParams;
 TUserServiceInitializeParams=packed record
  priority:DWORD;
 end;

 PUserServiceLoginUserIdList=^TUserServiceLoginUserIdList;
 TUserServiceLoginUserIdList=packed record
  userId:array[0..SCE_USER_SERVICE_MAX_LOGIN_USERS-1] of Integer;
 end;

const
//SceUserServiceEventType
 SCE_USER_SERVICE_EVENT_TYPE_LOGIN  =0;
 SCE_USER_SERVICE_EVENT_TYPE_LOGOUT =1;

type
 pSceUserServiceEvent=^SceUserServiceEvent;
 SceUserServiceEvent=packed record
  eventType:Integer; //SceUserServiceEventType
  userId:Integer;    //SceUserServiceUserId
 end;

 TUserServiceEventCallback=procedure(event:pSceUserServiceEvent;arg:Pointer); SysV_ABI_CDecl;

implementation

function ps4_sceUserServiceInitialize(params:PUserServiceInitializeParams):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceUserServiceInitialize2(threadPriority:Integer;cpuAffinityMask:qword):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceUserServiceTerminate:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceUserServiceGetLoginUserIdList(List:PUserServiceLoginUserIdList):Integer; SysV_ABI_CDecl;
var
 i:Integer;
begin
 Result:=-1;
 if (List=nil) then Exit;
 List^.userId[0]:=1;
 For i:=1 to SCE_USER_SERVICE_MAX_LOGIN_USERS-1 do
  List^.userId[i]:=SCE_USER_SERVICE_USER_ID_INVALID;
 Result:=0;
end;

function ps4_sceUserServiceGetInitialUser(pUserId:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if pUserId=nil then Exit;
 pUserId^:=1;
 Result:=0;
end;

function ps4_sceUserServiceGetUserName(userId:Integer;userName:PChar;size:size_t):Integer; SysV_ABI_CDecl;
Const
 cuser:PChar='user';
begin
 Move(cuser^,userName^,Length(cuser)+1);
 Result:=0;
end;

function ps4_sceUserServiceRegisterEventCallback(func:TUserServiceEventCallback;arg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceUserServiceRegisterEventCallback:',HexStr(func));
 Result:=0;
end;

const
 SCE_USER_SERVICE_ERROR_NOT_INITIALIZED =-2137653246; //0x80960002
 SCE_USER_SERVICE_ERROR_INVALID_ARGUMENT=-2137653243; //0x80960005
 SCE_USER_SERVICE_ERROR_NO_EVENT        =-2137653241; //0x80960007

var
 login_event:Boolean=False;

function ps4_sceUserServiceGetEvent(event:pSceUserServiceEvent):Integer; SysV_ABI_CDecl;
begin
 if (event=nil) then Exit(SCE_USER_SERVICE_ERROR_INVALID_ARGUMENT);

 if login_event then
 begin
  Result:=SCE_USER_SERVICE_ERROR_NO_EVENT;
 end else
 begin
  login_event:=True;

  event^:=Default(SceUserServiceEvent);
  event^.eventType:=SCE_USER_SERVICE_EVENT_TYPE_LOGIN;
  event^.userId   :=1;

  Result:=0;
 end;
end;

const
 //SceUserServiceUserColor
 SCE_USER_SERVICE_USER_COLOR_BLUE =0;
 SCE_USER_SERVICE_USER_COLOR_RED  =1;
 SCE_USER_SERVICE_USER_COLOR_GREEN=2;
 SCE_USER_SERVICE_USER_COLOR_PINK =3;

function ps4_sceUserServiceGetUserColor(userId:Integer;
                                        color:pInteger  //SceUserServiceUserColor
                                       ):Integer; SysV_ABI_CDecl;
begin
 if (color=nil) then Exit(SCE_USER_SERVICE_ERROR_INVALID_ARGUMENT);
 color^:=SCE_USER_SERVICE_USER_COLOR_BLUE;
 Result:=0;
end;

function Load_libSceUserService(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceUserService');

 lib^.set_proc($8F760CBB531534DA,@ps4_sceUserServiceInitialize);
 lib^.set_proc($6B3FF447A7AF899D,@ps4_sceUserServiceInitialize2);
 lib^.set_proc($6F01634BE6D7F660,@ps4_sceUserServiceTerminate);
 lib^.set_proc($7CF87298A36F2BF0,@ps4_sceUserServiceGetLoginUserIdList);
 lib^.set_proc($09D5A9D281D61ABD,@ps4_sceUserServiceGetInitialUser);
 lib^.set_proc($D71C5C3221AED9FA,@ps4_sceUserServiceGetUserName);
 lib^.set_proc($C87D7B43A356B558,@ps4_sceUserServiceGetEvent);
 lib^.set_proc($954A2AC1342EE06A,@ps4_sceUserServiceGetUserColor);

 lib:=Result._add_lib('libSceUserServiceForNpToolkit');

 lib^.set_proc($C2E23B73B50D9340,@ps4_sceUserServiceRegisterEventCallback);
end;

initialization
 ps4_app.RegistredPreLoad('libSceUserService.prx',@Load_libSceUserService);

end.

