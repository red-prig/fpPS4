unit ps4_libSceDialogs;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes, SysUtils;

implementation

const
//SceCommonDialogStatus
 SCE_COMMON_DIALOG_STATUS_NONE        = 0;
 SCE_COMMON_DIALOG_STATUS_INITIALIZED = 1;
 SCE_COMMON_DIALOG_STATUS_RUNNING     = 2;
 SCE_COMMON_DIALOG_STATUS_FINISHED    = 3;

//SceCommonDialogResult {
 SCE_COMMON_DIALOG_RESULT_OK            = 0;
 SCE_COMMON_DIALOG_RESULT_USER_CANCELED = 1;


function ps4_sceCommonDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceCommonDialogInitialize');
 Result:=0;
end;

function ps4_sceErrorDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceErrorDialogInitialize');
 Result:=0;
end;

function ps4_sceNpProfileDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpProfileDialogInitialize');
 Result:=0;
end;

function ps4_sceSaveDataDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSaveDataDialogUpdateStatus');
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;
end;

function ps4_sceSaveDataDialogProgressBarSetValue(target:Integer;rate:DWORD):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSaveDataDialogProgressBarSetValue:',rate);
 Result:=0;
end;

function ps4_sceSaveDataDialogTerminate():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSaveDataDialogTerminate');
 Result:=0;
end;

function Load_libSceCommonDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceCommonDialog');
 lib^.set_proc($BA85292C6364CA09,@ps4_sceCommonDialogInitialize);
end;

//

function Load_libSceErrorDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceErrorDialog');
 lib^.set_proc($23CF0A0A19729D2B,@ps4_sceErrorDialogInitialize);
end;

//

function Load_libSceNpProfileDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceNpProfileDialog');
 lib^.set_proc($2E0F8D084EA94F04,@ps4_sceNpProfileDialogInitialize);
end;

//

function Load_libSceSaveDataDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceSaveDataDialog');
 lib^.set_proc($28ADC1760D5158AD,@ps4_sceSaveDataDialogUpdateStatus);
 lib^.set_proc($85ACB509F4E62F20,@ps4_sceSaveDataDialogProgressBarSetValue);
 lib^.set_proc($62E1F6140EDACEA4,@ps4_sceSaveDataDialogTerminate);
end;

initialization
 ps4_app.RegistredPreLoad('libSceCommonDialog.prx',@Load_libSceCommonDialog);
 ps4_app.RegistredPreLoad('libSceErrorDialog.prx',@Load_libSceErrorDialog);
 ps4_app.RegistredPreLoad('libSceNpProfileDialog.prx',@Load_libSceNpProfileDialog);
 ps4_app.RegistredPreLoad('libSceSaveDataDialog.prx',@Load_libSceSaveDataDialog);

end.

