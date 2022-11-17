unit ps4_libSceNpWebApi;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

function ps4_sceNpWebApiInitialize(libHttpCtxId:Integer;
                                   poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpWebApiInitialize:',libHttpCtxId,':',poolSize);
 Result:=4;
end;

function ps4_sceNpWebApiCreateContextA(libCtxId,userId:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpWebApiCreateContextA:',libCtxId,':',userId);
 //Result:=Integer($80552907);
 Result:=0;
end;

//nop nid:libSceNpWebApi:ADD82CE59D4CC85C:sceNpWebApiCreateRequest


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

function Load_libSceNpWebApi(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpWebApi');

 lib^.set_proc($1B70272CD7510631,@ps4_sceNpWebApiInitialize );
 lib^.set_proc($CE4E9CEB9C68C8ED,@ps4_sceNpWebApiCreateContextA);
 //nop nid:libSceNpWebApi:ADD82CE59D4CC85C:sceNpWebApiCreateRequest
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

