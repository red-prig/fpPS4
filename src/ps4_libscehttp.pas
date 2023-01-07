unit ps4_libSceHttp;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

uses sys_kernel;

const
 SCE_HTTP_NB_EVENT_SOCK_ERR = 8;

function ps4_sceHttpInit(libnetMemId,libsslCtxId:Integer;poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceHttpInit poolSize=',poolSize);
 Result:=4; // libhttpCtxId
end;

function ps4_sceHttpTerm(libhttpCtxId:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceHttpTerm:',libhttpCtxId);
 Result:=0;
end;

function ps4_sceHttpCreateTemplate(
	  libhttpCtxId:Integer;
	  userAgent:PChar;
	  httpVer:Integer;
	  autoProxyConf:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceHttpCreateTemplate userAgent=',userAgent,' httpVer=',httpVer);
 Result:=1; // templateId
end;

function ps4_sceHttpDeleteTemplate(templateId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSetNonblock(id:Integer;enable:Boolean):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSetAutoRedirect(id:Integer;enable:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpsEnableOption(id:Integer;sslFlags:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpsDisableOption(id:Integer;sslFlags:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpsSetSslVersion(id:Integer;version:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

type
 PSceHttpEpollHandle=^SceHttpEpollHandle;
 SceHttpEpollHandle=Pointer;

function ps4_sceHttpCreateEpoll(libhttpCtxId:Integer;eh:PSceHttpEpollHandle):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 eh^:=Pointer($BADF);
end;

function ps4_sceHttpDestroyEpoll(libhttpCtxId:Integer;eh:SceHttpEpollHandle):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSetEpoll(id:Integer;eh:SceHttpEpollHandle;userArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix, 'sceHttpSetEpoll');
 Result:=0;
end;

function ps4_sceHttpUnsetEpoll(id:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix, 'sceHttpUnsetEpoll');
 Result:=0;
end;

type
 PSceHttpNBEvent=^SceHttpNBEvent;
 SceHttpNBEvent=packed record
  events:DWORD;
  eventDetail:DWORD;
  id:Integer;
  userArg:Pointer;
 end;

function ps4_sceHttpWaitRequest(eh:SceHttpEpollHandle;
                                nbev:PSceHttpNBEvent;
                                maxevents:Integer;
                                timeout_us:Integer):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpWaitRequest handle=', HexStr(eh),' event.id=', nbev^.id,' maxevents=',maxevents);

 nbev^.events:=SCE_HTTP_NB_EVENT_SOCK_ERR;
 nbev^.id:=3;

 Result:=1;
end;

function ps4_sceHttpAddRequestHeader(id:Integer;name:PChar;value:PChar;mode:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 WriteLn(SysLogPrefix, 'sceHttpAddRequestHeader ',name,'=',value,' mode=',mode);
end;

type
 SceHttpsCallback=function(
                   libsslCtxId:Integer;
                   verifyErr:Integer;
                   sslCert:Pointer; //SceSslCert
                   certNum:Integer;
                   userArg:Pointer):Integer; SysV_ABI_CDecl;

function ps4_sceHttpsSetSslCallback(id:Integer;cbfunc:SceHttpsCallback;userArg:Pointer):Integer; SysV_ABI_CDecl;
begin      
 WriteLn(SysLogPrefix, 'sceHttpsSetSslCallback id=',id,' callback=',HexStr(@cbfunc));
 Result:=0;
end;

function ps4_sceHttpCreateConnectionWithURL(tmplId:Integer;
                                            url:PChar;
                                            enableKeepalive:Boolean):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpCreateConnectionWithURL:' + url);
 Result:=2;
end;

function ps4_sceHttpDeleteConnection(connId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpCreateRequestWithURL2(connId:Integer;
                                          method:PChar;
                                          url:PChar;
                                          contentLength:QWORD):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpCreateRequestWithURL2 method=', method, 'url=',url);
 Result:=3;
end;

function ps4_sceHttpDeleteRequest(reqId:Integer):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpDeleteRequest');
 Result:=0;
end;

function ps4_sceHttpSendRequest(reqId:Integer;
                                postData:Pointer;
                                size:QWORD):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpSendRequest reqId=',reqId,' postData=',HexStr(postData));
 Result:=0;
end;

function ps4_sceHttpGetStatusCode(reqId:Integer;
                                  statusCode:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (statusCode<>nil) then
 begin
  statusCode^:=404;
 end;
 Result:=0;
 Result:=Integer($80431082);
end;

const
 nullp:Pchar='';

function ps4_sceHttpGetAllResponseHeaders(reqId:Integer;
                                          header:PPchar;
                                          headerSize:PQWORD):Integer; SysV_ABI_CDecl;
begin
 header^:=@nullp;
 headerSize^:=0;
 Result:=0;
end;

function ps4_sceHttpGetResponseContentLength(reqId:Integer;
                                             presult:PInteger;
                                             contentLength:PQWORD):Integer; SysV_ABI_CDecl;
begin
 presult^:=0;
 contentLength^:=0;
 Result:=0;
end;

function ps4_sceHttpReadData(reqId:Integer;
                             data:Pointer;
                             size:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceHttp(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceHttp');
 lib^.set_proc($03D715314B44A786,@ps4_sceHttpInit);
 lib^.set_proc($224FCAA4B4E57FB4,@ps4_sceHttpTerm);
 lib^.set_proc($D206233D347FE9C6,@ps4_sceHttpCreateTemplate);
 lib^.set_proc($E08F2F129B84859F,@ps4_sceHttpDeleteTemplate);
 lib^.set_proc($B36FCD3C8BF3FA20,@ps4_sceHttpSetNonblock);
 lib^.set_proc($B36FCD3C8BF3FA20,@ps4_sceHttpSetAutoRedirect);
 lib^.set_proc($7F8D8ADFB9A6E513,@ps4_sceHttpsEnableOption);
 lib^.set_proc($992402C73593C152,@ps4_sceHttpsDisableOption);
 lib^.set_proc($539131406CB2C7DB,@ps4_sceHttpsSetSslVersion);
 lib^.set_proc($EB7F3575617EC6C4,@ps4_sceHttpCreateEpoll);
 lib^.set_proc($C1885755F4B612DE,@ps4_sceHttpDestroyEpoll);
 lib^.set_proc($FF19BB91940DA472,@ps4_sceHttpSetEpoll);
 lib^.set_proc($E7DB4BD404016FC5,@ps4_sceHttpUnsetEpoll);
 lib^.set_proc($A884A30C7AF138D7,@ps4_sceHttpWaitRequest);
 lib^.set_proc($118DBC4F66E437B9,@ps4_sceHttpAddRequestHeader);
 lib^.set_proc($86DC813A859E4B9F,@ps4_sceHttpsSetSslCallback);
 lib^.set_proc($AA0C43063A2B531B,@ps4_sceHttpCreateConnectionWithURL);
 lib^.set_proc($3FA037CADA6C8987,@ps4_sceHttpDeleteConnection);
 lib^.set_proc($0A7A7BEE9A1D9025,@ps4_sceHttpCreateRequestWithURL2);
 lib^.set_proc($A9EEE867EBF83D60,@ps4_sceHttpDeleteRequest);
 lib^.set_proc($D5ED8137023F5F31,@ps4_sceHttpSendRequest);
 lib^.set_proc($D1AD9304D7C4DC15,@ps4_sceHttpGetStatusCode);
 lib^.set_proc($68260F31250868FF,@ps4_sceHttpGetAllResponseHeaders);
 lib^.set_proc($CAE3B61F652F9E8B,@ps4_sceHttpGetResponseContentLength);
 lib^.set_proc($3F9A5DA3290F6139,@ps4_sceHttpReadData);
end;

function ps4_sceHttp2Init(libnetMemId,libsslCtxId:Integer;
                          poolSize:size_t;
                          maxConcurrentRequest:Integer):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttp2Init poolSize=',poolSize);
 Result:=3;
end;

function Load_libSceHttp2(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceHttp2');
 lib^.set_proc($DC909EDE509B43C0,@ps4_sceHttp2Init);
end;

initialization
 ps4_app.RegistredPreLoad('libSceHttp.prx' ,@Load_libSceHttp);
 ps4_app.RegistredPreLoad('libSceHttp2.prx',@Load_libSceHttp2);

end.

