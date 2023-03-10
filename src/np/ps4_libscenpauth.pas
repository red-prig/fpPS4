unit ps4_libSceNpAuth;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 np_error,
 sys_kernel;

implementation

function ps4_sceNpAuthCreateRequest():Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceNpAuthCreateRequest');
 Result:=-1;
end;

function Load_libSceNpAuth(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpAuth');
 lib^.set_proc($E9BC05928B184508,@ps4_sceNpAuthCreateRequest);
 //
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpAuth.prx' ,@Load_libSceNpAuth);

end.

