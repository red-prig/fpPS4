unit ps4_libSceUlt;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program;

implementation

function ps4_sceUltInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceUltInitialize');
 Result:=0;
end;

function Load_libSceUlt(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceUlt');

 lib^.set_proc($859220D44586B073,@ps4_sceUltInitialize);
end;

initialization
 ps4_app.RegistredPreLoad('libSceUlt.prx',@Load_libSceUlt);

end.

