unit ps4_libSceUserService;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes, SysUtils;

const
 SCE_KERNEL_PRIO_FIFO_LOWEST =$2FF;
 SCE_KERNEL_PRIO_FIFO_NORMAL =$2BC;
 SCE_KERNEL_PRIO_FIFO_HIGHEST=$100;

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

implementation

function ps4_sceUserServiceInitialize(params:PUserServiceInitializeParams):Integer; assembler; nostackframe;
asm
 xor %rax,%rax
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

function Load_libSceUserService(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceUserService');

 lib^.set_proc($8F760CBB531534DA,@ps4_sceUserServiceInitialize);
 lib^.set_proc($7CF87298A36F2BF0,@ps4_sceUserServiceGetLoginUserIdList);
 lib^.set_proc($09D5A9D281D61ABD,@ps4_sceUserServiceGetInitialUser);
 lib^.set_proc($D71C5C3221AED9FA,@ps4_sceUserServiceGetUserName);
end;

initialization
 ps4_app.RegistredPreLoad('libSceUserService.prx',@Load_libSceUserService);

end.

