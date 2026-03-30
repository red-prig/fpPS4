unit ps4_libSceDialogs;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}
{$WARN 4110 off}

interface

uses
  subr_dynlib,
  ps4_libSceCommonDialog,
  ps4_libSceMsgDialog,
  ps4_libSceSaveDataDialog,
  ps4_libSceErrorDialog,
  ps4_libSigninDialog,
  ps4_libSceImeDialog;

implementation

Const
 SCE_NP_COMMERCE_DIALOG_RESULT_PURCHASED=2;

//

var
 status_profile_dialog:Integer=SCE_COMMON_DIALOG_STATUS_NONE;

function ps4_sceNpProfileDialogInitialize():Integer;
begin
 Writeln('sceNpProfileDialogInitialize');
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

var
 status_commerce_dialog:Integer=SCE_COMMON_DIALOG_STATUS_NONE;

function ps4_sceNpCommerceDialogInitialize():Integer;
begin
 Writeln('sceNpCommerceDialogInitialize');
 status_commerce_dialog:=SCE_COMMON_DIALOG_STATUS_INITIALIZED;
 Result:=0;
end;

type
 pSceNpCommerceDialogParam=^SceNpCommerceDialogParam;
 SceNpCommerceDialogParam=packed record
  baseParam:SceCommonDialogBaseParam;
  size:Integer;
  userId:Integer;
  mode:Integer;       //SceNpCommerceDialogMode
  serviceLabel:DWORD; //SceNpServiceLabel
  targets:PPChar;
  numTargets:DWORD;
  align:Integer;
  features:QWORD;
  userData:Pointer;
  reserved:array[0..31] of Byte;
 end;

function ps4_sceNpCommerceDialogOpen(param:pSceNpCommerceDialogParam):Integer;
begin
 Writeln('sceNpCommerceDialogOpen');
 status_commerce_dialog:=SCE_COMMON_DIALOG_STATUS_FINISHED;
 Result:=0;
end;

function ps4_sceNpCommerceDialogUpdateStatus():Integer;
begin
 Result:=status_commerce_dialog;
end;

function ps4_sceNpCommerceDialogGetStatus():Integer;
begin
 Result:=status_commerce_dialog;
end;

type
 pSceNpCommerceDialogResult=^SceNpCommerceDialogResult;
 SceNpCommerceDialogResult=packed record
  result:Integer;
  authorized:Boolean;
  align1:Byte;
  align2:Word;
  userData:Pointer;
  reserved:array[0..31] of Byte;
 end;

function ps4_sceNpCommerceDialogGetResult(pResult:pSceNpCommerceDialogResult):Integer;
begin
 //Writeln('sceNpCommerceDialogGetResult');
 if (pResult<>nil) then
 begin
  pResult^.result:=SCE_NP_COMMERCE_DIALOG_RESULT_PURCHASED;
  pResult^.authorized:=false;
 end;
 Result:=0;
end;

function ps4_sceNpCommerceDialogTerminate():Integer;
begin
 Writeln('sceNpCommerceDialogTerminate');
 status_commerce_dialog:=SCE_COMMON_DIALOG_STATUS_NONE;
 Result:=0;
end;

const
 //SceNpCommercePsStoreIconPos
 SCE_NP_COMMERCE_PS_STORE_ICON_CENTER=0;
 SCE_NP_COMMERCE_PS_STORE_ICON_LEFT  =1;
 SCE_NP_COMMERCE_PS_STORE_ICON_RIGHT =2;

function ps4_sceNpCommerceShowPsStoreIcon(pos:Integer):Integer;
begin
 Writeln('sceNpCommerceShowPsStoreIcon:',pos);
 Result:=0;
end;

function ps4_sceNpCommerceHidePsStoreIcon():Integer;
begin
 Writeln('sceNpCommerceHidePsStoreIcon');
 Result:=0;
end;

//

function ps4_scePlayerInvitationDialogTerminate():Integer;
begin
 Writeln('scePlayerInvitationDialogTerminate');
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

function ps4_sceHmdSetupDialogInitialize():Integer;
begin
 Result:=0;
end;

function ps4_sceHmdSetupDialogOpen(param:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceHmdSetupDialogUpdateStatus():Integer;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_FINISHED;
end;

function ps4_sceHmdSetupDialogGetResult(pResult:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceHmdSetupDialogTerminate():Integer;
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



function Load_libSceNpCommerce(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpCommerce');

 lib:=Result^.add_lib('libSceNpCommerce');
 lib.set_proc($D1A4766969906A5E,@ps4_sceNpCommerceDialogInitialize);
 lib.set_proc($0DF4820D10371236,@ps4_sceNpCommerceDialogOpen);
 lib.set_proc($2D1E5CC0530C0951,@ps4_sceNpCommerceDialogUpdateStatus);
 lib.set_proc($0826C2FA5AAABC5D,@ps4_sceNpCommerceDialogGetStatus);
 lib.set_proc($AF8D9B59C41BB596,@ps4_sceNpCommerceDialogGetResult);
 lib.set_proc($9BF23DD806F9D16F,@ps4_sceNpCommerceDialogTerminate);
 lib.set_proc($0C79B0B1AE92F137,@ps4_sceNpCommerceShowPsStoreIcon);
 lib.set_proc($76CA8256C34CD198,@ps4_sceNpCommerceHidePsStoreIcon);
end;

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

function Load_libSceHmdSetupDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceHmdSetupDialog');

 lib:=Result^.add_lib('libSceHmdSetupDialog');
 lib.set_proc($341D58DA40368C26,@ps4_sceHmdSetupDialogInitialize);
 lib.set_proc($34D8225784FE6A45,@ps4_sceHmdSetupDialogOpen);
 lib.set_proc($51DEE3DFE4432018,@ps4_sceHmdSetupDialogUpdateStatus);
 lib.set_proc($EA55511CC5792D8D,@ps4_sceHmdSetupDialogGetResult);
 lib.set_proc($FB3E0E26616B7997,@ps4_sceHmdSetupDialogTerminate);
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
 RegisteredInternalFile(stub[5] ,'libSceNpCommerce.prx'            ,@Load_libSceNpCommerce            );
 RegisteredInternalFile(stub[7] ,'libScePlayerInvitationDialog.prx',@Load_libScePlayerInvitationDialog);
 RegisteredInternalFile(stub[9] ,'libSceLoginDialog.prx'           ,@Load_libSceLoginDialog           );
 RegisteredInternalFile(stub[10],'libSceHmdSetupDialog.prx'        ,@Load_libSceHmdSetupDialog        );
 RegisteredInternalFile(stub[11],'libSceNpFriendListDialog.prx'    ,@Load_libSceNpFriendListDialog    );
 RegisteredInternalFile(stub[12],'libSceInvitationDialog.prx'      ,@Load_libSceInvitationDialog      );
 RegisteredInternalFile(stub[13],'libSceWebBrowserDialog.prx'      ,@Load_libSceWebBrowserDialog      );

end.

