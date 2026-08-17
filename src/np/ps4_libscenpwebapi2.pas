unit ps4_libSceNpWebApi2;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib,
 ps4_libSceNpCommon,
 ps4_libSceNpManager;

const
 SCE_NP_WEBAPI2_ERROR_OUT_OF_MEMORY                =-2141899775; // 0x80553401
 SCE_NP_WEBAPI2_ERROR_INVALID_ARGUMENT             =-2141899774; // 0x80553402
 SCE_NP_WEBAPI2_ERROR_INVALID_LIB_CONTEXT_ID       =-2141899773; // 0x80553403
 SCE_NP_WEBAPI2_ERROR_LIB_CONTEXT_NOT_FOUND        =-2141899772; // 0x80553404
 SCE_NP_WEBAPI2_ERROR_USER_CONTEXT_NOT_FOUND       =-2141899771; // 0x80553405
 SCE_NP_WEBAPI2_ERROR_REQUEST_NOT_FOUND            =-2141899770; // 0x80553406
 SCE_NP_WEBAPI2_ERROR_NOT_SIGNED_IN                =-2141899769; // 0x80553407
 SCE_NP_WEBAPI2_ERROR_INVALID_CONTENT_PARAMETER    =-2141899768; // 0x80553408
 SCE_NP_WEBAPI2_ERROR_ABORTED                      =-2141899767; // 0x80553409
 SCE_NP_WEBAPI2_ERROR_USER_CONTEXT_ALREADY_EXIST   =-2141899766; // 0x8055340A
 SCE_NP_WEBAPI2_ERROR_PUSH_EVENT_FILTER_NOT_FOUND  =-2141899765; // 0x8055340B
 SCE_NP_WEBAPI2_ERROR_PUSH_EVENT_CALLBACK_NOT_FOUND=-2141899764; // 0x8055340C
 SCE_NP_WEBAPI2_ERROR_HANDLE_NOT_FOUND             =-2141899763; // 0x8055340D
 SCE_NP_WEBAPI2_ERROR_SIGNED_IN_USER_NOT_FOUND     =-2141899762; // 0x8055340E
 SCE_NP_WEBAPI2_ERROR_LIB_CONTEXT_BUSY             =-2141899761; // 0x8055340F
 SCE_NP_WEBAPI2_ERROR_USER_CONTEXT_BUSY            =-2141899760; // 0x80553410
 SCE_NP_WEBAPI2_ERROR_REQUEST_BUSY                 =-2141899759; // 0x80553411
 SCE_NP_WEBAPI2_ERROR_INVALID_HTTP_STATUS_CODE     =-2141899758; // 0x80553412
 SCE_NP_WEBAPI2_ERROR_PROHIBITED_HTTP_HEADER       =-2141899757; // 0x80553413
 SCE_NP_WEBAPI2_ERROR_PROHIBITED_FUNCTION_CALL     =-2141899756; // 0x80553414
 SCE_NP_WEBAPI2_ERROR_MULTIPART_PART_NOT_FOUND     =-2141899755; // 0x80553415
 SCE_NP_WEBAPI2_ERROR_PARAMETER_TOO_LONG           =-2141899754; // 0x80553416
 SCE_NP_WEBAPI2_ERROR_HANDLE_BUSY                  =-2141899753; // 0x80553417
 SCE_NP_WEBAPI2_ERROR_LIB_CONTEXT_MAX              =-2141899752; // 0x80553418
 SCE_NP_WEBAPI2_ERROR_USER_CONTEXT_MAX             =-2141899751; // 0x80553419
 SCE_NP_WEBAPI2_ERROR_AFTER_SEND                   =-2141899750; // 0x8055341A
 SCE_NP_WEBAPI2_ERROR_TIMEOUT                      =-2141899749; // 0x8055341B
 SCE_NP_WEBAPI2_ERROR_PUSH_CONTEXT_NOT_FOUND       =-2141899748; // 0x8055341C

implementation

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

const
 SCE_NP_WEBAPI_EXTD_PUSH_EVENT_EXTD_DATA_KEY_LEN_MAX=32;

type
 pSceNpWebApi2ContentParameter=^SceNpWebApi2ContentParameter;
 SceNpWebApi2ContentParameter=packed record
  contentLength:QWORD;
  pContentType :Pchar;
  reserved     :array[0..15] of Byte;
 end;

 pSceNpWebApi2ResponseInformationOption=^SceNpWebApi2ResponseInformationOption;
 SceNpWebApi2ResponseInformationOption=packed record
  httpStatus      :Integer;
  _align          :Integer;
  pErrorObject    :Pchar;
  errorObjectSize :QWORD;
  responseDataSize:QWORD;
 end;

 pSceNpWebApi2ExtdPushEventExtdDataKey=^SceNpWebApi2ExtdPushEventExtdDataKey;
 SceNpWebApi2ExtdPushEventExtdDataKey=packed record
  val:array[0..SCE_NP_WEBAPI_EXTD_PUSH_EVENT_EXTD_DATA_KEY_LEN_MAX] of AnsiChar;
 end;

 pSceNpWebApi2PushEventFilterParameter=^SceNpWebApi2PushEventFilterParameter;
 SceNpWebApi2PushEventFilterParameter=packed record
  dataType      :SceNpWebApi2ExtdPushEventExtdDataKey;
  pExtdDataKey  :Pointer;
  extdDataKeyNum:QWORD;
 end;

const
 SCE_NP_WEBAPI2_PUSH_EVENT_UUID_LENGTH=36;

type
 pSceNpWebApi2PushEventPushContextId=^SceNpWebApi2PushEventPushContextId;
 SceNpWebApi2PushEventPushContextId=packed record
  uuid:array[0..SCE_NP_WEBAPI2_PUSH_EVENT_UUID_LENGTH] of Char;
 end;

function ps4_sceNpWebApi2Initialize(libHttp2CtxId:Integer;
                                    poolSize:size_t):Integer;
begin
 LOG_INFO('sceNpWebApi2Initialize:',libHttp2CtxId,':',poolSize);
 Result:=4;
end;

function ps4_sceNpWebApi2CreateRequest(titleUserCtxId:Integer;
	                               pApiGroup:Pchar;
	                               pPath:Pchar;
	                               method:PChar; //SceNpWebApi2HttpMethod
	                               pContentParameter:pSceNpWebApi2ContentParameter;
	                               pRequestId:pInt64):Integer;
begin
 Result:=0;
end;

function ps4_sceNpWebApi2SendRequest(requestId:Int64;
                                     pData:Pointer;
                                     dataSize:QWORD;
                                     pRespInfoOption:pSceNpWebApi2ResponseInformationOption):Integer;
begin
 if (pRespInfoOption<>nil) then
 begin
  pRespInfoOption^.httpStatus:=404;
  pRespInfoOption^.responseDataSize:=0;
 end;
 Result:=SCE_NP_WEBAPI2_ERROR_REQUEST_NOT_FOUND;
end;

function ps4_sceNpWebApi2CreateUserContext(libCtxId,m_userId:Integer):Integer;
begin
 LOG_INFO('sceNpWebApi2CreateUserContext:',libCtxId,':',m_userId);
 Result:=5;
end;

function ps4_sceNpWebApi2PushEventDeletePushContext(userCtxId:Integer;
                                                    pPushCtxId:pSceNpWebApi2PushEventPushContextId):Integer;
begin
 Result:=0;
end;

function ps4_sceNpWebApi2AddHttpRequestHeader(requestId:Integer;
                                              const pFieldName:PChar;
                                              const pValue:PChar):Integer;
begin
 Result:=0;
end;

function ps4_sceNpWebApi2PushEventCreateHandle(libCtxId:Integer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpWebApi2PushEventCreateFilter(libCtxId:Integer;
                                               handleId:Integer;
                                               pNpServiceName:PChar;
                                               npServiceLabel:DWORD;
                                               pFilterParam:pSceNpWebApi2PushEventFilterParameter;
                                               filterParamNum:QWORD):Integer;
begin
 Result:=0;
end;

function ps4_sceNpWebApi2PushEventRegisterCallback(libCtxId:Integer;
                                                   cbFunc:Pointer;
                                                   pUserArg:Pointer):Integer;
begin
 Result:=0;
end;

function Load_libSceNpWebApi2(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpWebApi2');

 lib:=Result^.add_lib('libSceNpWebApi2');
 lib.set_proc($FA8F7CD7A61086A4,@ps4_sceNpWebApi2Initialize);
 lib.set_proc($DC423F39227AE577,@ps4_sceNpWebApi2CreateRequest);
 lib.set_proc($95038217CE25BF3C,@ps4_sceNpWebApi2SendRequest);
 lib.set_proc($B24E786E2E85B583,@ps4_sceNpWebApi2CreateUserContext);
 lib.set_proc($41A7F179933758AE,@ps4_sceNpWebApi2PushEventDeletePushContext);
 lib.set_proc($7A038EBEB9C5EA62,@ps4_sceNpWebApi2AddHttpRequestHeader);
 lib.set_proc($595D46C0CDF63606,@ps4_sceNpWebApi2PushEventCreateHandle);
 lib.set_proc($32C685851FA53C4E,@ps4_sceNpWebApi2PushEventCreateFilter);
 lib.set_proc($7D8DD0A9E36417C9,@ps4_sceNpWebApi2PushEventRegisterCallback);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpWebApi2.prx',@Load_libSceNpWebApi2);

end.

