unit ps4_libSceInvitationDialog;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

function ps4_sceInvitationDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceInvitationDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceInvitationDialog');
 lib^.set_proc($F7E83D88EABEEE48,@ps4_sceInvitationDialogUpdateStatus);
end;

initialization
 ps4_app.RegistredPreLoad('libSceInvitationDialog.prx',@Load_libSceInvitationDialog);

end.

