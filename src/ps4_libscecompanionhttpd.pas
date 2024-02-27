unit ps4_libSceCompanionHttpd;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 ps4_libSceNet,
 Classes,
 SysUtils;

const
 SCE_COMPANION_HTTPD_ERROR_NOT_INITIALIZED=-2132541434; //0x80E40006
 SCE_COMPANION_HTTPD_ERROR_INVALID_PARAM  =-2132541436; //0x80E40004

type
 pSceCompanionHttpdOptParam2=^SceCompanionHttpdOptParam2;
 SceCompanionHttpdOptParam2=packed record
  thisSize                    :QWORD;
  workMemory                  :Pointer;
  workMemorySize              :QWORD;
  workThreadPriority          :Integer;
  _align1                     :Integer;
  workThreadAffinity          :QWORD;
  workThreadStackSize         :QWORD;
  transceiverThreadPriority   :Integer;
  _align2                     :Integer;
  transceiverThreadAffinity   :QWORD;
  transceiverStackSize        :QWORD;
  transceiverThreadCount      :QWORD;
  port                        :SceNetInPort_t;
  _align3                     :word;
  screenOrientation           :Integer;
  workDirectory               :PChar;
  enableWebSocketProxy        :Boolean;
  _align4                     :Byte;
  webSocketServerPort         :SceNetInPort_t;
  webSocketProxySessionTimeout:DWORD;
 end;

 pSceCompanionHttpdHeader=^SceCompanionHttpdHeader;
 SceCompanionHttpdHeader=packed record
  key   :PChar;
  value :PChar;
  header:pSceCompanionHttpdHeader;
 end;

 pSceCompanionHttpdRequest=^SceCompanionHttpdRequest;
 SceCompanionHttpdRequest=packed record
  method  :Integer;
  _align  :Integer;
  url     :PChar;
  header  :pSceCompanionHttpdHeader;
  body    :PChar;
  bodySize:QWORD;
 end;

 pSceCompanionHttpdResponse=^SceCompanionHttpdResponse;
 SceCompanionHttpdResponse=packed record
  status  :Integer;
  _align  :Integer;
  header  :pSceCompanionHttpdHeader;
  body    :PChar;
  bodySize:QWORD;
 end;

 SceCompanionHttpdRequestCallback=function(
  userId      :Integer;
  httpRequest :pSceCompanionHttpdRequest;
  httpResponse:pSceCompanionHttpdResponse;
  param       :Pointer
 ):Integer;

 SceCompanionHttpdRequestBodyReceptionCallback=function(
  event      :Integer;
  userId     :Integer;
  httpRequest:pSceCompanionHttpdRequest;
  param      :Pointer
 ):Integer;

implementation

function ps4_sceCompanionHttpdInitialize2(const option:pSceCompanionHttpdOptParam2):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceCompanionHttpdStart():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceCompanionHttpdRegisterRequestCallback(_function:SceCompanionHttpdRequestCallback;
                                                      param:Pointer):Integer; SysV_ABI_CDecl;
begin
 if (param=nil) then Exit(SCE_COMPANION_HTTPD_ERROR_INVALID_PARAM);
 Result:=0;
end;

function ps4_sceCompanionHttpdRegisterRequestBodyReceptionCallback(_function:SceCompanionHttpdRequestBodyReceptionCallback;
                                                                   param:Pointer):Integer; SysV_ABI_CDecl;
begin
 if (param=nil) then Exit(SCE_COMPANION_HTTPD_ERROR_INVALID_PARAM);
 Result:=0;
end;

function Load_libSceCompanionHttpd(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceCompanionHttpd');
 lib^.set_proc($380E856CE45E7DBA,@ps4_sceCompanionHttpdInitialize2);
 lib^.set_proc($93B17415C0CCFD77,@ps4_sceCompanionHttpdStart);
 lib^.set_proc($39A5B0F8854475B2,@ps4_sceCompanionHttpdRegisterRequestCallback);
 lib^.set_proc($7C73668A3EE40143,@ps4_sceCompanionHttpdRegisterRequestBodyReceptionCallback);
end;

initialization
 ps4_app.RegistredPreLoad('libSceCompanionHttpd.prx',@Load_libSceCompanionHttpd);

end.

