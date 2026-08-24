unit ps4_libSceDialogs;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
  subr_dynlib,
  ps4_libSceCommonDialog,
  ps4_libSceMsgDialog,
  ps4_libSceSaveDataDialog,
  ps4_libSceErrorDialog,
  ps4_libSceNpCommerce,
  ps4_libSceHmdSetupDialog,
  ps4_libSigninDialog,
  ps4_libSceImeDialog;

implementation

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

//

var
 status_profile_dialog:Integer=SCE_COMMON_DIALOG_STATUS_NONE;

function ps4_sceNpProfileDialogInitialize():Integer;
begin
 LOG_INFO('sceNpProfileDialogInitialize');
 status_profile_dialog:=SCE_COMMON_DIALOG_STATUS_INITIALIZED;
 Result:=0;
end;

function ps4_sceNpProfileDialogUpdateStatus():Integer;
begin
 Result:=status_profile_dialog;
end;

function ps4_sceNpProfileDialogTerminate():Integer;
begin
 Result:=0;
end;

//

function ps4_scePlayerInvitationDialogTerminate():Integer;
begin
 LOG_INFO('scePlayerInvitationDialogTerminate');
 Result:=0;
end;

//

function ps4_sceLoginDialogInitialize():Integer;
begin
 Result:=0;
end;

function ps4_sceLoginDialogUpdateStatus():Integer;
begin
 Result:=0;
end;

//

function ps4_sceNpFriendListDialogUpdateStatus():Integer;
begin
 Result:=0;
end;

//

function ps4_sceInvitationDialogInitialize():Integer;
begin
 Result:=0;
end;

function ps4_sceInvitationDialogTerminate():Integer;
begin
 Result:=0;
end;

function ps4_sceInvitationDialogUpdateStatus():Integer;
begin
 Result:=0;
end;

//

function ps4_sceWebBrowserDialogUpdateStatus():Integer;
begin
 Result:=0;
end;

function ps4_sceWebBrowserDialogGetStatus():Integer;
begin
 Result:=0;
end;

function ps4_sceWebBrowserDialogTerminate():Integer;
begin
 Result:=0;
end;

//


{$WARN 4110 off}
//

function Load_libSceNpProfileDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpProfileDialog');

 lib:=Result^.add_lib('libSceNpProfileDialog');
 lib.set_proc($2E0F8D084EA94F04,@ps4_sceNpProfileDialogInitialize);
 lib.set_proc($85A55913D1602AA1,@ps4_sceNpProfileDialogUpdateStatus);
 lib.set_proc($D12A7DBC9701D7FC,@ps4_sceNpProfileDialogTerminate);
end;

//

function Load_libScePlayerInvitationDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libScePlayerInvitationDialog');

 lib:=Result^.add_lib('libScePlayerInvitationDialog');
 lib.set_proc($8039B96BA19213DE,@ps4_scePlayerInvitationDialogTerminate);
end;

function Load_libSceLoginDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceLoginDialog');

 lib:=Result^.add_lib('libSceLoginDialog');
 lib.set_proc($A8FFC4BD0465D877,@ps4_sceLoginDialogInitialize);
 lib.set_proc($DAB73E7A049F6F90,@ps4_sceLoginDialogUpdateStatus);
end;

function Load_libSceNpFriendListDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpFriendListDialog');

 lib:=Result^.add_lib('libSceNpFriendListDialog');
 lib.set_proc($7EBC33DDECAE03AC,@ps4_sceNpFriendListDialogUpdateStatus);
end;

function Load_libSceInvitationDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceInvitationDialog');

 lib:=Result^.add_lib('libSceInvitationDialog');
 lib.set_proc($5EF039292E7AC1CB,@ps4_sceInvitationDialogInitialize);
 lib.set_proc($07A1D526D0D8C441,@ps4_sceInvitationDialogTerminate);
 lib.set_proc($F7E83D88EABEEE48,@ps4_sceInvitationDialogUpdateStatus);
end;

function Load_libSceWebBrowserDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceWebBrowserDialog');

 lib:=Result^.add_lib('libSceWebBrowserDialog');
 lib.set_proc($875751FEDE484A08,@ps4_sceWebBrowserDialogUpdateStatus);
 lib.set_proc($0854C6E9AF138CE5,@ps4_sceWebBrowserDialogGetStatus);
 lib.set_proc($A1C1EDC81C077F2B,@ps4_sceWebBrowserDialogTerminate);
end;

var
 stub:array[0..13] of t_int_file;

initialization
 RegisteredInternalFile(stub[2] ,'libSceNpProfileDialog.prx'       ,@Load_libSceNpProfileDialog       );
 RegisteredInternalFile(stub[7] ,'libScePlayerInvitationDialog.prx',@Load_libScePlayerInvitationDialog);
 RegisteredInternalFile(stub[9] ,'libSceLoginDialog.prx'           ,@Load_libSceLoginDialog           );
 RegisteredInternalFile(stub[11],'libSceNpFriendListDialog.prx'    ,@Load_libSceNpFriendListDialog    );
 RegisteredInternalFile(stub[12],'libSceInvitationDialog.prx'      ,@Load_libSceInvitationDialog      );
 RegisteredInternalFile(stub[13],'libSceWebBrowserDialog.prx'      ,@Load_libSceWebBrowserDialog      );

end.

