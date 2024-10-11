unit ps4_libSceNpSessionSignaling;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 Classes,
 SysUtils;

implementation

function ps4_sceNpSessionSignalingInitialize():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceNpSessionSignaling(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceNpSessionSignaling');
 lib^.set_proc($CAC9B0E89FCFF009,@ps4_sceNpSessionSignalingInitialize);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpSessionSignaling.prx',@Load_libSceNpSessionSignaling);

end.

