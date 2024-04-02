unit ps4_libSceNpWebApi;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  ps4_libSceNpCommon,
  ps4_libSceNpManager;

implementation

function ps4_sceNpWebApiInitialize(libHttpCtxId:Integer;
                                   poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpWebApiInitialize:',libHttpCtxId,':',poolSize);
 Result:=4;
end;

function ps4_sceNpWebApiTerminate(libCtxId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpWebApiCreateContext(libCtxId:Integer;pOnlineId:pSceNpOnlineId):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpWebApiCreateContext:',libCtxId,':',pOnlineId^.data);
 Result:=0;
end;


function ps4_sceNpWebApiCreateContextA(libCtxId,userId:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpWebApiCreateContextA:',libCtxId,':',userId);
 //Result:=Integer($80552907);
 Result:=0;
end;

function ps4_sceNpWebApiCreateHandle(libCtxId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=5;
end;

function ps4_sceNpWebApiDeleteHandle(libCtxId,handleId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

const
 //SceNpWebApiHttpMethod
 SCE_NP_WEBAPI_HTTP_METHOD_GET   =0;
 SCE_NP_WEBAPI_HTTP_METHOD_POST  =1;
 SCE_NP_WEBAPI_HTTP_METHOD_PUT   =2;
 SCE_NP_WEBAPI_HTTP_METHOD_DELETE=3;

type
 pSceNpWebApiContentParameter=^SceNpWebApiContentParameter;
 SceNpWebApiContentParameter=packed record
  contentLength:QWORD;
  pContentType:Pchar;
  reserved:array[0..15] of Byte;
 end;

function ps4_sceNpWebApiCreateRequest(
	  titleUserCtxId:Integer;
	  pApiGroup:Pchar;
	  pPath:Pchar;
	  method:Integer; //SceNpWebApiHttpMethod
	  pContentParameter:pSceNpWebApiContentParameter;
	  pRequestId:pInt64):Integer; SysV_ABI_CDecl;
begin
 pRequestId^:=6;
 Result:=0;
end;

function ps4_sceNpWebApiDeleteRequest(requestId:Int64):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpWebApiSendRequest(requestId:Int64;
                                    pData:Pointer;
                                    dataSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

type
 pSceNpWebApiResponseInformationOption=^SceNpWebApiResponseInformationOption;
 SceNpWebApiResponseInformationOption=packed record
  httpStatus:Integer;      //out
  _align:Integer;
  pErrorObject:Pchar;      //in
  errorObjectSize:size_t;  //in
  responseDataSize:size_t; //out
 end;

function ps4_sceNpWebApiSendRequest2(requestId:Int64;
                                     pData:Pointer;
                                     dataSize:size_t;
                                     pRespInfoOption:pSceNpWebApiResponseInformationOption
                                     ):Integer; SysV_ABI_CDecl;
begin
 if (pRespInfoOption<>nil) then
 begin
  pRespInfoOption^.httpStatus:=404;
  pRespInfoOption^.responseDataSize:=0;
 end;
 Result:=0;
end;

function ps4_sceNpWebApiGetHttpStatusCode(requestId:Int64;
                                          pStatusCode:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pStatusCode<>nil) then
 begin
  pStatusCode^:=404;
 end;
 Result:=0;
end;

function ps4_sceNpWebApiGetHttpResponseHeaderValueLength(
                                          requestId:Int64;
                                          pFieldName:PChar;
                                          pValueLength:PQWORD):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpWebApiGetHttpResponseHeaderValueLength:',pFieldName);
 if (pValueLength<>nil) then
 begin
  pValueLength^:=0;
 end;
 Result:=0;
end;

function ps4_sceNpWebApiReadData(requestId:Int64;
                                 pData:Pointer;
                                 size:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

const
 SCE_NP_WEBAPI_PUSH_EVENT_DATA_TYPE_LEN_MAX=64;
 SCE_NP_WEBAPI_EXTD_PUSH_EVENT_EXTD_DATA_KEY_LEN_MAX=32;

type
 pSceNpWebApiPushEventDataType=^SceNpWebApiPushEventDataType;
 SceNpWebApiPushEventDataType=packed record
  val:array[0..SCE_NP_WEBAPI_PUSH_EVENT_DATA_TYPE_LEN_MAX] of AnsiChar;
 end;

 pSceNpWebApiExtdPushEventExtdDataKey=^SceNpWebApiExtdPushEventExtdDataKey;
 SceNpWebApiExtdPushEventExtdDataKey=packed record
  val:array[0..SCE_NP_WEBAPI_EXTD_PUSH_EVENT_EXTD_DATA_KEY_LEN_MAX] of AnsiChar;
 end;

 pSceNpWebApiExtdPushEventFilterParameter=^SceNpWebApiExtdPushEventFilterParameter;
 SceNpWebApiExtdPushEventFilterParameter=packed record
  dataType:pSceNpWebApiExtdPushEventExtdDataKey;
  pExtdDataKey:Pointer;
  extdDataKeyNum:size_t;
 end;

 pSceNpWebApiExtdPushEventExtdData=^SceNpWebApiExtdPushEventExtdData;
 SceNpWebApiExtdPushEventExtdData=packed record
  extdDataKey:SceNpWebApiExtdPushEventExtdDataKey;
  pData      :PChar;
  dataLen    :QWORD;
 end;

 SceNpWebApiExtdPushEventCallbackA=procedure(
  userCtxId     :Integer;
  callbackId    :Integer;
  pNpServiceName:PChar;
  npServiceLabel:SceNpServiceLabel;
  pTo           :pSceNpPeerAddressA;
  pToOnlineId   :pSceNpOnlineId;
  pFrom         :pSceNpPeerAddressA;
  pFromOnlineId :SceNpOnlineId;
  pDataType     :pSceNpWebApiPushEventDataType;
  pData         :PChar;
  dataLen       :QWORD;
  pExtdData     :pSceNpWebApiExtdPushEventExtdData;
  extdDataNum   :QWORD;
  pUserArg      :Pointer
 ); SysV_ABI_CDecl;

function ps4_sceNpWebApiCreatePushEventFilter(libCtxId:Integer;
                                              pDataType:pSceNpWebApiPushEventDataType;
                                              dataTypeNum:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=7;
end;

function ps4_sceNpWebApiCreateServicePushEventFilter(libCtxId:Integer;
                                                     handleId:Integer;
                                                     pNpServiceName:PChar;
                                                     npServiceLabel:DWORD; //SceNpServiceLabel
                                                     pDataType:pSceNpWebApiPushEventDataType;
                                                     dataTypeNum:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=8;
end;

function ps4_sceNpWebApiCreateExtdPushEventFilter(libCtxId,handleId:Integer;
                                                  pNpServiceName:PChar;
                                                  npServiceLabel:DWORD;
                                                  pFilterParam:pSceNpWebApiExtdPushEventFilterParameter;
                                                  filterParamNum:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=9;
end;


function ps4_sceNpWebApiRegisterPushEventCallback(userCtxId:Integer;
                                                  filterId:Integer;
                                                  cbFunc:Pointer; //SceNpWebApiPushEventCallback
                                                  pUserArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=1;
end;

function ps4_sceNpWebApiRegisterServicePushEventCallback(userCtxId:Integer;
                                                         filterId:Integer;
                                                         cbFunc:Pointer; //SceNpWebApiServicePushEventCallback
                                                         pUserArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=2;
end;

function ps4_sceNpWebApiRegisterExtdPushEventCallback(userCtxId,filterId:Integer;
                                                         cbFunc:Pointer; //SceNpWebApiServicePushEventCallback
                                                         pUserArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=3;
end;

function ps4_sceNpWebApiRegisterExtdPushEventCallbackA(userCtxId,filterId:Integer;
                                                       cbFunc:SceNpWebApiExtdPushEventCallbackA;
                                                       pUserArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=3;
end;

procedure ps4_sceNpWebApiCheckTimeout(); SysV_ABI_CDecl;
begin
 //
end;

function ps4_sceNpWebApiDeleteContext(userCtxId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

//NpWebApi2

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
  dataType:pSceNpWebApi2ExtdPushEventExtdDataKey;
  pExtdDataKey:Pointer;
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
                                    poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpWebApi2Initialize:',libHttp2CtxId,':',poolSize);
 Result:=4;
end;

function ps4_sceNpWebApi2CreateRequest(titleUserCtxId:Integer;
	                               pApiGroup:Pchar;
	                               pPath:Pchar;
	                               method:PChar; //SceNpWebApi2HttpMethod
	                               pContentParameter:pSceNpWebApi2ContentParameter;
	                               pRequestId:pInt64):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpWebApi2SendRequest(requestId:Int64;
                                     pData:Pointer;
                                     dataSize:QWORD;
                                     pRespInfoOption:pSceNpWebApi2ResponseInformationOption):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpWebApi2CreateUserContext(libCtxId,m_userId:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpWebApi2CreateUserContext:',libCtxId,':',m_userId);
 Result:=5;
end;

function ps4_sceNpWebApi2PushEventDeletePushContext(userCtxId:Integer;
                                                    pPushCtxId:pSceNpWebApi2PushEventPushContextId):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpWebApi2AddHttpRequestHeader(requestId:Integer;
                                              const pFieldName:PChar;
                                              const pValue:PChar):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpWebApi2PushEventCreateFilter(libCtxId:Integer;
                                               handleId:Integer;
                                               pNpServiceName:PChar;
                                               npServiceLabel:DWORD;
                                               pFilterParam:pSceNpWebApi2PushEventFilterParameter;
                                               filterParamNum:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;  

//NpWebApi2

function Load_libSceNpWebApi(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpWebApi');

 lib^.set_proc($1B70272CD7510631,@ps4_sceNpWebApiInitialize);
 lib^.set_proc($6ACCF74ED22A185F,@ps4_sceNpWebApiTerminate);
 lib^.set_proc($C7563BCA261293B7,@ps4_sceNpWebApiCreateContext);
 lib^.set_proc($CE4E9CEB9C68C8ED,@ps4_sceNpWebApiCreateContextA);
 lib^.set_proc($EFD33F26ABEF1A8D,@ps4_sceNpWebApiCreateHandle);
 lib^.set_proc($E4C9FB4D8C29977D,@ps4_sceNpWebApiDeleteHandle);
 lib^.set_proc($ADD82CE59D4CC85C,@ps4_sceNpWebApiCreateRequest);
 lib^.set_proc($9E842095EBBE28B1,@ps4_sceNpWebApiDeleteRequest);
 lib^.set_proc($9156CBE212F72BBC,@ps4_sceNpWebApiSendRequest);
 lib^.set_proc($2A335E67FDBDCAC4,@ps4_sceNpWebApiSendRequest2);
 lib^.set_proc($936D74A0A80FF346,@ps4_sceNpWebApiGetHttpStatusCode);
 lib^.set_proc($EF8DD9CC4073955F,@ps4_sceNpWebApiGetHttpResponseHeaderValueLength);
 lib^.set_proc($090B4F45217A0ECF,@ps4_sceNpWebApiReadData);
 lib^.set_proc($CB94DAE490B34076,@ps4_sceNpWebApiCreatePushEventFilter);
 lib^.set_proc($B08171EF7E3EC72B,@ps4_sceNpWebApiCreateServicePushEventFilter);
 lib^.set_proc($3DF4930C280D3207,@ps4_sceNpWebApiRegisterPushEventCallback);
 lib^.set_proc($909409134B8A9B9C,@ps4_sceNpWebApiRegisterServicePushEventCallback);
 lib^.set_proc($33605407E0CD1061,@ps4_sceNpWebApiCreateExtdPushEventFilter);
 lib^.set_proc($BEB334D80E46CB53,@ps4_sceNpWebApiRegisterExtdPushEventCallback);
 lib^.set_proc($8E15CA1902787A02,@ps4_sceNpWebApiRegisterExtdPushEventCallbackA);
 lib^.set_proc($81534DCB17FFD528,@ps4_sceNpWebApiCheckTimeout);
 lib^.set_proc($5D48DDB124D36775,@ps4_sceNpWebApiDeleteContext);
end;

function Load_libSceNpWebApi2(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpWebApi2');

 lib^.set_proc($FA8F7CD7A61086A4,@ps4_sceNpWebApi2Initialize);
 lib^.set_proc($DC423F39227AE577,@ps4_sceNpWebApi2CreateRequest);
 lib^.set_proc($95038217CE25BF3C,@ps4_sceNpWebApi2SendRequest); 
 lib^.set_proc($B24E786E2E85B583,@ps4_sceNpWebApi2CreateUserContext);
 lib^.set_proc($41A7F179933758AE,@ps4_sceNpWebApi2PushEventDeletePushContext);
 lib^.set_proc($7A038EBEB9C5EA62,@ps4_sceNpWebApi2AddHttpRequestHeader);
 lib^.set_proc($32C685851FA53C4E,@ps4_sceNpWebApi2PushEventCreateFilter); 
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpWebApi.prx',@Load_libSceNpWebApi);
 ps4_app.RegistredPreLoad('libSceNpWebApi2.prx',@Load_libSceNpWebApi2);

end.

