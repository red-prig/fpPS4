unit ps4_libSceNpAuth;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 np_error,
 sys_kernel;

const
 SCE_NP_CLIENT_ID_MAX_LEN=128;
 SCE_NP_AUTHORIZATION_CODE_MAX_LEN=128;

type
 pSceNpClientId=^SceNpClientId;
 SceNpClientId=packed record
  id:array[0..SCE_NP_CLIENT_ID_MAX_LEN+1] of char;
  padding:array[0..6] of Byte;
 end;

 pSceNpAuthCreateAsyncRequestParameter=^SceNpAuthCreateAsyncRequestParameter;
 SceNpAuthCreateAsyncRequestParameter=packed record
  size:QWORD;
  cpuAffinityMask:QWORD;
  threadPriority:Integer;
  padding:array[0..3] of Byte;
 end;

 pSceNpAuthGetAuthorizationCodeParameter=^SceNpAuthGetAuthorizationCodeParameter;
 SceNpAuthGetAuthorizationCodeParameter=packed record
  size:QWORD;
  userId,padding:array[0..3] of Byte;
  clientId:pSceNpClientId;
  scope:PChar;
 end;

 pSceNpAuthorizationCode=^SceNpAuthorizationCode;
 SceNpAuthorizationCode=packed record
  code:array[0..SCE_NP_AUTHORIZATION_CODE_MAX_LEN+1] of char;
  padding:array[0..6] of Byte;
 end;

implementation

function ps4_sceNpAuthCreateRequest():Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceNpAuthCreateRequest');
 Result:=-1;
end;

function ps4_sceNpAuthCreateAsyncRequest(const pParam:pSceNpAuthCreateAsyncRequestParameter):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpAuthGetAuthorizationCode(reqId:Integer;
                                           const param:pSceNpAuthGetAuthorizationCodeParameter;
                                           authCode:pSceNpAuthorizationCode;
                                           issuerId:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpAuthPollAsync(reqId:Integer;pResult:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpAuthDeleteRequest(reqId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceNpAuth(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpAuth');
 lib^.set_proc($E9BC05928B184508,@ps4_sceNpAuthCreateRequest);
 lib^.set_proc($37E9ABEC68D3BEBF,@ps4_sceNpAuthCreateAsyncRequest);
 lib^.set_proc($2B11A43AB4094EA6,@ps4_sceNpAuthGetAuthorizationCode);
 lib^.set_proc($8234B27F34AC0DC1,@ps4_sceNpAuthPollAsync);
 lib^.set_proc($1FCC06F4193F9CF7,@ps4_sceNpAuthDeleteRequest);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpAuth.prx' ,@Load_libSceNpAuth);

end.

