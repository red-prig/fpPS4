unit ps4_libSceNpProfileDialog;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 Classes,
 SysUtils;

implementation

function ps4_sceNpProfileDialogTerminate():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceNpProfileDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpProfileDialog');
 lib^.set_proc($D12A7DBC9701D7FC,@ps4_sceNpProfileDialogTerminate);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpProfileDialog.prx',@Load_libSceNpProfileDialog);

end.

