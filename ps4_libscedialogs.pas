unit ps4_libSceDialogs;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils,
  ps4_libSceSaveData;

implementation

Const
 //SceCommonDialogStatus
 SCE_COMMON_DIALOG_STATUS_NONE        = 0;
 SCE_COMMON_DIALOG_STATUS_INITIALIZED = 1;
 SCE_COMMON_DIALOG_STATUS_RUNNING     = 2;
 SCE_COMMON_DIALOG_STATUS_FINISHED    = 3;

 //SceCommonDialogResult
 SCE_COMMON_DIALOG_RESULT_OK           =0;
 SCE_COMMON_DIALOG_RESULT_USER_CANCELED=1;

 SCE_COMMON_DIALOG_MAGIC_NUMBER=$C0D1A109;

 //SceMsgDialogMode
 SCE_MSG_DIALOG_MODE_INVALID     =(0);
 SCE_MSG_DIALOG_MODE_USER_MSG    =(1);
 SCE_MSG_DIALOG_MODE_PROGRESS_BAR=(2);
 SCE_MSG_DIALOG_MODE_SYSTEM_MSG  =(3);

 //SceMsgDialogButtonType
 SCE_MSG_DIALOG_BUTTON_TYPE_OK                    =(0);
 SCE_MSG_DIALOG_BUTTON_TYPE_YESNO                 =(1);
 SCE_MSG_DIALOG_BUTTON_TYPE_NONE                  =(2);
 SCE_MSG_DIALOG_BUTTON_TYPE_OK_CANCEL             =(3);
 SCE_MSG_DIALOG_BUTTON_TYPE_WAIT                  =(5);
 SCE_MSG_DIALOG_BUTTON_TYPE_WAIT_CANCEL           =(6);
 SCE_MSG_DIALOG_BUTTON_TYPE_YESNO_FOCUS_NO        =(7);
 SCE_MSG_DIALOG_BUTTON_TYPE_OK_CANCEL_FOCUS_CANCEL=(8);
 SCE_MSG_DIALOG_BUTTON_TYPE_2BUTTONS              =(9);

 //SceMsgDialogProgressBarType
 SCE_MSG_DIALOG_PROGRESSBAR_TYPE_PERCENTAGE       =(0);
 SCE_MSG_DIALOG_PROGRESSBAR_TYPE_PERCENTAGE_CANCEL=(1);

 //SceMsgDialogSystemMessageType;
 SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_EMPTY_STORE         =(0);
 SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_PSN_CHAT_RESTRICTION=(1);
 SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_PSN_UGC_RESTRICTION =(2);

 //SceSaveDataDialogAnimation
 SCE_SAVE_DATA_DIALOG_ANIMATION_ON  =(0);
 SCE_SAVE_DATA_DIALOG_ANIMATION_OFF =(1);

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

function ps4_sceErrorDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=0; //SCE_ERROR_DIALOG_STATUS_NONE
end;

function ps4_sceNpProfileDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpProfileDialogInitialize');
 Result:=0;
end;

function ps4_sceSaveDataDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSaveDataDialogInitialize');
 Result:=0;
end;

function ps4_sceSaveDataDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 //Writeln('sceSaveDataDialogUpdateStatus');
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;
end;

function ps4_sceSaveDataDialogGetStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;
end;

type
 pSceSaveDataDialogResult=^SceSaveDataDialogResult;
 SceSaveDataDialogResult=packed record
  mode:Integer;//SceSaveDataDialogMode;         //Mode of function
  result:Integer;                               //Result of executing function
  buttonId:Integer;//SceSaveDataDialogButtonId; //Id of button user selected
  _align:Integer;
  dirName:pSceSaveDataDirName;        //savedata directory name
  param:pSceSaveDataParam;            //Buffer to receive savedata information ( can be set NULL if you don't need it)
  userData:Pointer;                   //Userdata specified at calling function
  reserved:array[0..31] of Byte;      //Reserved range (must be filled by zero)
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

const
 SCE_COMMON_DIALOG_ERROR_NOT_FINISHED=-2135425019;//0x80B80005

function ps4_sceSaveDataDialogGetResult(_result:pSceSaveDataDialogResult):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_NOT_FINISHED;
end;

type
 pSceSaveDataDialogCloseParam=^SceSaveDataDialogCloseParam;
 SceSaveDataDialogCloseParam=packed record
  anim:Integer;
  reserved:array[0..31] of Byte;
 end;

function ps4_sceSaveDataDialogClose(closeParam:pSceSaveDataDialogCloseParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

//

function ps4_sceMsgDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceMsgDialogInitialize');
 Result:=0;
end;

type
 SceCommonDialogBaseParam=packed record
  size:QWORD;
  reserved:array[0..31] of Byte;
  magic:DWORD;
  _align:Integer;
 end; //__attribute__ ((__aligned__(8)));

 pSceMsgDialogButtonsParam=^SceMsgDialogButtonsParam;
 SceMsgDialogButtonsParam=packed record
  msg1,msg2:Pchar;
  reserved:array[0..31] of Byte;
 end;

 pSceMsgDialogUserMessageParam=^SceMsgDialogUserMessageParam;
 SceMsgDialogUserMessageParam=packed record
  buttonType:Integer; //SceMsgDialogButtonType
  _align:Integer;
  msg:PChar;
  buttonsParam:pSceMsgDialogButtonsParam;
  reserved:array[0..23] of Byte;
 end;

 pSceMsgDialogProgressBarParam=^SceMsgDialogProgressBarParam;
 SceMsgDialogProgressBarParam=packed record
  barType:Integer; //SceMsgDialogProgressBarType
  _align:Integer;
  msg:PChar;
  reserved:array[0..63] of Byte;
 end;

 pSceMsgDialogSystemMessageParam=^SceMsgDialogSystemMessageParam;
 SceMsgDialogSystemMessageParam=packed record
  sysMsgType:Integer; //SceMsgDialogSystemMessageType
  reserved:array[0..31] of Byte;
 end;

 pSceMsgDialogParam=^SceMsgDialogParam;
 SceMsgDialogParam=packed record
  baseParam:SceCommonDialogBaseParam;
  size:QWORD;
  mode:Integer; //SceMsgDialogMode
  _align1:Integer;
  userMsgParam:pSceMsgDialogUserMessageParam;
  progBarParam:pSceMsgDialogProgressBarParam;
  sysMsgParam:pSceMsgDialogSystemMessageParam;
  userId:Integer; //SceUserServiceUserId
  reserved:array[0..39] of Byte;
  _align2:Integer;
 end;

const
 SCE_COMMON_DIALOG_ERROR_PARAM_INVALID=-2135425014; // 0x80B8000A
 SCE_COMMON_DIALOG_ERROR_ARG_NULL     =-2135425011; // 0x80B8000D

function ps4_sceMsgDialogOpen(param:pSceMsgDialogParam):Integer; SysV_ABI_CDecl;
begin
 if (param=nil) then Exit(SCE_COMMON_DIALOG_ERROR_ARG_NULL);

 Case param^.mode of
   SCE_MSG_DIALOG_MODE_USER_MSG:
    begin
     if (param^.userMsgParam=nil) then Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);

     Writeln(param^.userMsgParam^.msg);

     //TODO
    end;
  else
   Assert(false,'TODO');
 end;

 Result:=0;
end;

function ps4_sceMsgDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;
end;

function ps4_sceMsgDialogGetStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;
end;

//

function ps4_sceNpCommerceDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpCommerceDialogInitialize');
 Result:=0;
end;

function ps4_sceNpCommerceDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=0; //SCE_COMMON_DIALOG_STATUS_NONE
end;

const
 //SceNpCommercePsStoreIconPos
 SCE_NP_COMMERCE_PS_STORE_ICON_CENTER=0;
 SCE_NP_COMMERCE_PS_STORE_ICON_LEFT  =1;
 SCE_NP_COMMERCE_PS_STORE_ICON_RIGHT =2;

function ps4_sceNpCommerceShowPsStoreIcon(pos:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpCommerceShowPsStoreIcon:',pos);
 Result:=0;
end;

//

function ps4_sceSigninDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSigninDialogInitialize');
 Result:=0;
end;

function ps4_sceSigninDialogTerminate():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSigninDialogTerminate');
 Result:=0;
end;

type
 pSceSigninDialogParam=^SceSigninDialogParam;
 SceSigninDialogParam=packed record
  size:Integer;
  userId:Integer;
  reserved:array[0..1] of Integer;
 end;

const
 SCE_SIGNIN_DIALOG_STATUS_NONE       =0;
 SCE_SIGNIN_DIALOG_STATUS_INITIALIZED=1;
 SCE_SIGNIN_DIALOG_STATUS_RUNNING    =2;
 SCE_SIGNIN_DIALOG_STATUS_FINISHED   =3;

function ps4_sceSigninDialogOpen(param:pSceSigninDialogParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceSigninDialogUpdateStatus:Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_SIGNIN_DIALOG_STATUS_FINISHED;
end;

//

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
 lib^.set_proc($596886BA1F577E04,@ps4_sceErrorDialogUpdateStatus);
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
 lib^.set_proc($B3D7B7F98A519F3C,@ps4_sceSaveDataDialogInitialize);
 lib^.set_proc($28ADC1760D5158AD,@ps4_sceSaveDataDialogUpdateStatus);
 lib^.set_proc($1112B392C6AE0090,@ps4_sceSaveDataDialogGetStatus);
 lib^.set_proc($85ACB509F4E62F20,@ps4_sceSaveDataDialogProgressBarSetValue);
 lib^.set_proc($62E1F6140EDACEA4,@ps4_sceSaveDataDialogTerminate);
 lib^.set_proc($C84889FEAAABE828,@ps4_sceSaveDataDialogGetResult);
 lib^.set_proc($7C7E3A2DA83CF176,@ps4_sceSaveDataDialogClose);
end;

function Load_libSceMsgDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceMsgDialog');
 lib^.set_proc($943AB1698D546C4A,@ps4_sceMsgDialogInitialize);
 lib^.set_proc($6F4E878740CF11A1,@ps4_sceMsgDialogOpen);
 lib^.set_proc($E9F202DD72ADDA4D,@ps4_sceMsgDialogUpdateStatus);
 lib^.set_proc($096556EFC41CDDF2,@ps4_sceMsgDialogGetStatus);
end;

function Load_libSceNpCommerce(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceNpCommerce');
 lib^.set_proc($D1A4766969906A5E,@ps4_sceNpCommerceDialogInitialize);
 lib^.set_proc($2D1E5CC0530C0951,@ps4_sceNpCommerceDialogUpdateStatus);
 lib^.set_proc($0C79B0B1AE92F137,@ps4_sceNpCommerceShowPsStoreIcon);
end;

function Load_libSceSigninDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceSigninDialog');
 lib^.set_proc($9A56067E6A84DDF4,@ps4_sceSigninDialogInitialize);
 lib^.set_proc($265A49568456BFB5,@ps4_sceSigninDialogOpen);
 lib^.set_proc($070DF59624C54F70,@ps4_sceSigninDialogUpdateStatus);
 lib^.set_proc($2D79664BA3EF25D5,@ps4_sceSigninDialogTerminate);
end;

initialization
 ps4_app.RegistredPreLoad('libSceCommonDialog.prx'   ,@Load_libSceCommonDialog);
 ps4_app.RegistredPreLoad('libSceErrorDialog.prx'    ,@Load_libSceErrorDialog);
 ps4_app.RegistredPreLoad('libSceNpProfileDialog.prx',@Load_libSceNpProfileDialog);
 ps4_app.RegistredPreLoad('libSceSaveDataDialog.prx' ,@Load_libSceSaveDataDialog);
 ps4_app.RegistredPreLoad('libSceMsgDialog.prx'      ,@Load_libSceMsgDialog);
 ps4_app.RegistredPreLoad('libSceNpCommerce.prx'     ,@Load_libSceNpCommerce);
 ps4_app.RegistredPreLoad('libSceSigninDialog.prx'   ,@Load_libSceSigninDialog);

end.

