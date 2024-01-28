unit ps4_libSceWebBrowserDialog;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

const
 //SceCommonDialogStatus
 SCE_COMMON_DIALOG_STATUS_NONE       =0;
 SCE_COMMON_DIALOG_STATUS_INITIALIZED=1;
 SCE_COMMON_DIALOG_STATUS_RUNNING    =2;
 SCE_COMMON_DIALOG_STATUS_FINISHED   =3;

implementation

function ps4_sceWebBrowserDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceWebBrowserDialogGetStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceWebBrowserDialogTerminate():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceWebBrowserDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceWebBrowserDialog');
 lib^.set_proc($875751FEDE484A08,@ps4_sceWebBrowserDialogUpdateStatus);
 lib^.set_proc($0854C6E9AF138CE5,@ps4_sceWebBrowserDialogGetStatus);
 lib^.set_proc($A1C1EDC81C077F2B,@ps4_sceWebBrowserDialogTerminate);
end;

initialization
 ps4_app.RegistredPreLoad('libSceWebBrowserDialog.prx',@Load_libSceWebBrowserDialog);

end.

