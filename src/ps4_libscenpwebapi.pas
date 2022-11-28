unit ps4_libSceNpWebApi;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  ps4_libSceNpManager,
  Classes,
  SysUtils;

implementation

function ps4_sceNpWebApiInitialize(libHttpCtxId:Integer;
                                   poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpWebApiInitialize:',libHttpCtxId,':',poolSize);
 Result:=4;
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

//

function ps4_sceNpWebApi2Initialize(libHttp2CtxId:Integer;
                                    poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpWebApi2Initialize:',libHttp2CtxId,':',poolSize);
 Result:=4;
end;

function ps4_sceNpWebApi2CreateUserContext(libCtxId,m_userId:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpWebApi2CreateUserContext:',libCtxId,':',m_userId);
 Result:=5;
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
  httpStatus:Integer;
  _align:Integer;
  pErrorObject:Pchar;
  errorObjectSize:size_t;
  responseDataSize:size_t;
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
 end;
 Result:=0;
end;

function ps4_sceNpWebApiGetHttpStatusCode(requestId:Int64;
                                          pStatusCode:PInteger):Integer; SysV_ABI_CDecl;
begin
 pStatusCode^:=404;
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

type
 pSceNpWebApiPushEventDataType=^SceNpWebApiPushEventDataType;
 SceNpWebApiPushEventDataType=packed record
  val:array[0..SCE_NP_WEBAPI_PUSH_EVENT_DATA_TYPE_LEN_MAX] of AnsiChar;
 end;

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


function Load_libSceNpWebApi(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpWebApi');

 lib^.set_proc($1B70272CD7510631,@ps4_sceNpWebApiInitialize );
 lib^.set_proc($C7563BCA261293B7,@ps4_sceNpWebApiCreateContext);
 lib^.set_proc($CE4E9CEB9C68C8ED,@ps4_sceNpWebApiCreateContextA);
 lib^.set_proc($EFD33F26ABEF1A8D,@ps4_sceNpWebApiCreateHandle);
 lib^.set_proc($ADD82CE59D4CC85C,@ps4_sceNpWebApiCreateRequest);
 lib^.set_proc($9E842095EBBE28B1,@ps4_sceNpWebApiDeleteRequest);
 lib^.set_proc($9156CBE212F72BBC,@ps4_sceNpWebApiSendRequest);
 lib^.set_proc($2A335E67FDBDCAC4,@ps4_sceNpWebApiSendRequest2);
 lib^.set_proc($936D74A0A80FF346,@ps4_sceNpWebApiGetHttpStatusCode);
 lib^.set_proc($090B4F45217A0ECF,@ps4_sceNpWebApiReadData);
 lib^.set_proc($CB94DAE490B34076,@ps4_sceNpWebApiCreatePushEventFilter);
 lib^.set_proc($B08171EF7E3EC72B,@ps4_sceNpWebApiCreateServicePushEventFilter);
 lib^.set_proc($3DF4930C280D3207,@ps4_sceNpWebApiRegisterPushEventCallback);
 lib^.set_proc($909409134B8A9B9C,@ps4_sceNpWebApiRegisterServicePushEventCallback);
end;

function Load_libSceNpWebApi2(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpWebApi2');

 lib^.set_proc($FA8F7CD7A61086A4,@ps4_sceNpWebApi2Initialize );
 lib^.set_proc($B24E786E2E85B583,@ps4_sceNpWebApi2CreateUserContext);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpWebApi.prx',@Load_libSceNpWebApi);
 ps4_app.RegistredPreLoad('libSceNpWebApi2.prx',@Load_libSceNpWebApi2);

end.

