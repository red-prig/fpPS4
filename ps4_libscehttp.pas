unit ps4_libSceHttp;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes, SysUtils;

implementation

function ps4_sceHttpInit(libnetMemId,libsslCtxId:Integer;poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceHttpInit:',poolSize);
 Result:=4;
end;

function ps4_sceHttpCreateTemplate(
	  libhttpCtxId:Integer;
	  userAgent:PChar;
	  httpVer:Integer;
	  autoProxyConf:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('userAgent:',userAgent);
 Result:=0;
end;

function ps4_sceHttpSetNonblock(id:Integer;enable:Boolean):Integer; SysV_ABI_CDecl;
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

function ps4_sceHttpAddRequestHeader(id:Integer;name:PChar;value:PChar;mode:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 Writeln(name,': ',value);
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
 lib^.set_proc($D206233D347FE9C6,@ps4_sceHttpCreateTemplate);
 lib^.set_proc($B36FCD3C8BF3FA20,@ps4_sceHttpSetNonblock);
 lib^.set_proc($EB7F3575617EC6C4,@ps4_sceHttpCreateEpoll);
 lib^.set_proc($118DBC4F66E437B9,@ps4_sceHttpAddRequestHeader);
 lib^.set_proc($86DC813A859E4B9F,@ps4_sceHttpsSetSslCallback);
end;

initialization
 ps4_app.RegistredPreLoad('libSceHttp.prx',@Load_libSceHttp);

end.

