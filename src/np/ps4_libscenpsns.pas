unit ps4_libSceNpSns;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib;

implementation

uses
 ps4_libSceUserService;

const
 SCE_NP_SNS_FACEBOOK_ERROR_UNKNOWN           =-2141903359; // 0x80552601
 SCE_NP_SNS_FACEBOOK_ERROR_INVALID_ARGUMENT  =-2141903358; // 0x80552602
 SCE_NP_SNS_FACEBOOK_ERROR_OUT_OF_MEMORY     =-2141903357; // 0x80552603
 SCE_NP_SNS_FACEBOOK_ERROR_EXCEEDS_MAX       =-2141903356; // 0x80552604
 SCE_NP_SNS_FACEBOOK_ERROR_UGM_RESTRICTION   =-2141903355; // 0x80552605
 SCE_NP_SNS_FACEBOOK_ERROR_ABORTED           =-2141903354; // 0x80552606
 SCE_NP_SNS_FACEBOOK_ERROR_ACCOUNT_NOT_BOUND =-2141903353; // 0x80552607
 SCE_NP_SNS_FACEBOOK_ERROR_CANCELED_BY_SYSTEM=-2141903352; // 0x80552608
 SCE_NP_SNS_FACEBOOK_ERROR_SUB_ACCOUNT       =-2141903351; // 0x80552609

const
 SCE_NP_SNS_FACEBOOK_MAX_REQUEST_NUM        =16;
 SCE_NP_SNS_FACEBOOK_PERMISSIONS_LENGTH_MAX =1023;
 SCE_NP_SNS_FACEBOOK_ACCESS_TOKEN_LENGTH_MAX=4096;

type
 pSceNpSnsFacebookAccessTokenParam=^SceNpSnsFacebookAccessTokenParam;
 SceNpSnsFacebookAccessTokenParam=packed record
  size       :QWORD; //sizeof(SceNpSnsFacebookAccessTokenParam)
  userId     :SceUserServiceUserId;
  _align     :Integer;
  fbAppId    :QWORD;
  permissions:array[0..SCE_NP_SNS_FACEBOOK_PERMISSIONS_LENGTH_MAX] of AnsiChar;
  reserved   :array[0..31] of Byte;
 end;

 pSceNpSnsFacebookAccessTokenResult=^SceNpSnsFacebookAccessTokenResult;
 SceNpSnsFacebookAccessTokenResult=packed record
  expiration :QWORD; //(sec)
  accessToken:array[0..SCE_NP_SNS_FACEBOOK_ACCESS_TOKEN_LENGTH_MAX] of AnsiChar;
  reserved   :array[0..38] of Byte;
 end;

function ps4_sceNpSnsFacebookCreateRequest():Integer;
begin
 Result:=1;
end;

function ps4_sceNpSnsFacebookDeleteRequest(reqId:Integer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpSnsFacebookAbortRequest(reqId:Integer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpSnsFacebookGetAccessToken(
          reqId :Integer;
          param :pSceNpSnsFacebookAccessTokenParam;
          pres  :pSceNpSnsFacebookAccessTokenResult
         ):Integer;
begin
 if (param=nil) or (pres=nil) then
 begin
  Exit(SCE_NP_SNS_FACEBOOK_ERROR_INVALID_ARGUMENT);
 end;

 pres^:=Default(SceNpSnsFacebookAccessTokenResult);
 pres^.accessToken[0]:='0';

 Result:=0;
end;

{$WARN 4110 off}
function Load_libSceNpSns(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpSns');

 lib:=Result^.add_lib('libSceNpSns');

 lib.set_proc($3A84E37F197CFF02,@ps4_sceNpSnsFacebookCreateRequest);
 lib.set_proc($A53BC0295D624241,@ps4_sceNpSnsFacebookDeleteRequest);
 lib.set_proc($FCB25B17D6FF5A1A,@ps4_sceNpSnsFacebookAbortRequest);
 lib.set_proc($56DC92F172C1A8D1,@ps4_sceNpSnsFacebookGetAccessToken);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpSns.prx',@Load_libSceNpSns);

end.

