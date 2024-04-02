unit ps4_libSceDialogs;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils,
  ps4_libSceIme,
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
 SCE_NP_COMMERCE_DIALOG_RESULT_PURCHASED=2;

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

function ps4_sceCommonDialogIsUsed():Boolean; SysV_ABI_CDecl;
begin
 Result:=True;
end;

//

var
 status_err_dialog:Integer=0; //SCE_ERROR_DIALOG_STATUS_NONE

function ps4_sceErrorDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceErrorDialogInitialize');
 status_err_dialog:=1; //SCE_ERROR_DIALOG_STATUS_INITIALIZED
 Result:=0;
end;

type
 pSceErrorDialogParam=^SceErrorDialogParam;
 SceErrorDialogParam=packed record
  size:Integer;
  errorCode:Integer;
  userId:Integer;
  reserved:Integer;
 end;

const
 SCE_ERROR_DIALOG_ERROR_PARAM_INVALID=Integer($80ED0003);

function ps4_sceErrorDialogOpen(param:pSceErrorDialogParam):Integer; SysV_ABI_CDecl;
begin
 if (param=nil) then Exit(SCE_ERROR_DIALOG_ERROR_PARAM_INVALID);
 Writeln('sceErrorDialogOpen:',HexStr(param^.errorCode,4));
 status_err_dialog:=3; //SCE_ERROR_DIALOG_STATUS_FINISHED
 Result:=0;
end;

function ps4_sceErrorDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=status_err_dialog;
end;

function ps4_sceErrorDialogGetStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=status_err_dialog;
end;

function ps4_sceErrorDialogTerminate():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceErrorDialogTerminate');
 status_err_dialog:=0; //SCE_ERROR_DIALOG_STATUS_NONE
 Result:=0;
end;

//

var
 status_profile_dialog:Integer=SCE_COMMON_DIALOG_STATUS_NONE;

function ps4_sceNpProfileDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpProfileDialogInitialize');
 status_profile_dialog:=SCE_COMMON_DIALOG_STATUS_INITIALIZED;
 Result:=0;
end;

function ps4_sceNpProfileDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=status_profile_dialog;
end;

function ps4_sceNpProfileDialogTerminate():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

//

var
 status_save_dialog:Integer=SCE_COMMON_DIALOG_STATUS_NONE;

function ps4_sceSaveDataDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSaveDataDialogInitialize');
 status_save_dialog:=SCE_COMMON_DIALOG_STATUS_INITIALIZED;
 Result:=0;
end;

//SceSaveDataDialogParam
function ps4_sceSaveDataDialogOpen(param:Pointer):Integer; SysV_ABI_CDecl;
begin
 if (param=nil) then Exit(SCE_ERROR_DIALOG_ERROR_PARAM_INVALID);
 Writeln('sceSaveDataDialogOpen:');
 status_save_dialog:=SCE_COMMON_DIALOG_STATUS_FINISHED;
 Result:=0;
end;

function ps4_sceSaveDataDialogIsReadyToDisplay:Integer; SysV_ABI_CDecl;
begin
 Result:=1;
end;

function ps4_sceSaveDataDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=status_save_dialog;
end;

function ps4_sceSaveDataDialogGetStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=status_save_dialog;
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
 status_save_dialog:=SCE_COMMON_DIALOG_STATUS_NONE;
 Result:=0;
end;

const
 SCE_COMMON_DIALOG_ERROR_NOT_FINISHED=-2135425019;//0x80B80005

function ps4_sceSaveDataDialogGetResult(_result:pSceSaveDataDialogResult):Integer; SysV_ABI_CDecl;
begin
 //Writeln('sceSaveDataDialogGetResult');
 Result:=0;
end;

type
 pSceSaveDataDialogCloseParam=^SceSaveDataDialogCloseParam;
 SceSaveDataDialogCloseParam=packed record
  anim:Integer;
  reserved:array[0..31] of Byte;
 end;

function ps4_sceSaveDataDialogClose(closeParam:pSceSaveDataDialogCloseParam):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSaveDataDialogClose');
 status_save_dialog:=SCE_COMMON_DIALOG_STATUS_FINISHED;
 Result:=0;
end;

//

var
 status_msg_dialog:Integer=SCE_COMMON_DIALOG_STATUS_NONE;

function ps4_sceMsgDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceMsgDialogInitialize');
 status_msg_dialog:=SCE_COMMON_DIALOG_STATUS_INITIALIZED;
 Result:=0;
end;

type
 SceCommonDialogBaseParam=packed record
  size:QWORD;
  reserved:array[0..35] of Byte;
  magic:DWORD;
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

 Writeln('sceMsgDialogOpen');

 Case param^.mode of
   SCE_MSG_DIALOG_MODE_USER_MSG:
    begin
     if (param^.userMsgParam=nil) then Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);

     Writeln(param^.userMsgParam^.msg);

     //TODO
    end;
  //else
  // Assert(false,'TODO');
 end;

 status_msg_dialog:=SCE_COMMON_DIALOG_STATUS_FINISHED;

 Result:=0;
end;

function ps4_sceMsgDialogClose():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceMsgDialogClose');
 status_msg_dialog:=SCE_COMMON_DIALOG_STATUS_FINISHED;
 Result:=0;
end;

function ps4_sceMsgDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=status_msg_dialog;
end;

function ps4_sceMsgDialogGetStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=status_msg_dialog;
end;

type
 pSceMsgDialogResult=^SceMsgDialogResult;
 SceMsgDialogResult=packed record
  mode:Integer;     //SceMsgDialogMode
  result:Integer;
  buttonId:Integer; //SceMsgDialogButtonId
  reserved:array[0..31] of Byte;
 end;

function ps4_sceMsgDialogGetResult(pResult:pSceMsgDialogResult):Integer; SysV_ABI_CDecl;
begin
 //Writeln('sceMsgDialogGetResult');
 if (pResult<>nil) then
 begin
  pResult^.result:=0;
  pResult^.buttonId:=1;
 end;
 Result:=0;
end;

function ps4_sceMsgDialogTerminate():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceMsgDialogTerminate');
 status_msg_dialog:=SCE_COMMON_DIALOG_STATUS_NONE;
 Result:=0;
end;

//

var
 status_commerce_dialog:Integer=SCE_COMMON_DIALOG_STATUS_NONE;

function ps4_sceNpCommerceDialogInitialize():Integer; SysV_ABI_CDecl;
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

function ps4_sceNpCommerceDialogOpen(param:pSceNpCommerceDialogParam):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpCommerceDialogOpen');
 status_commerce_dialog:=SCE_COMMON_DIALOG_STATUS_FINISHED;
 Result:=0;
end;

function ps4_sceNpCommerceDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=status_commerce_dialog;
end;

function ps4_sceNpCommerceDialogGetStatus():Integer; SysV_ABI_CDecl;
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

function ps4_sceNpCommerceDialogGetResult(pResult:pSceNpCommerceDialogResult):Integer; SysV_ABI_CDecl;
begin
 //Writeln('sceNpCommerceDialogGetResult');
 if (pResult<>nil) then
 begin
  pResult^.result:=SCE_NP_COMMERCE_DIALOG_RESULT_PURCHASED;
  pResult^.authorized:=false;
 end;
 Result:=0;
end;

function ps4_sceNpCommerceDialogTerminate():Integer; SysV_ABI_CDecl;
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

function ps4_sceNpCommerceShowPsStoreIcon(pos:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpCommerceShowPsStoreIcon:',pos);
 Result:=0;
end;

function ps4_sceNpCommerceHidePsStoreIcon():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpCommerceHidePsStoreIcon');
 Result:=0;
end;

//

const
 SCE_SIGNIN_DIALOG_STATUS_NONE       =0;
 SCE_SIGNIN_DIALOG_STATUS_INITIALIZED=1;
 SCE_SIGNIN_DIALOG_STATUS_RUNNING    =2;
 SCE_SIGNIN_DIALOG_STATUS_FINISHED   =3;

var
 status_signin_dialog:Integer=SCE_SIGNIN_DIALOG_STATUS_NONE;

function ps4_sceSigninDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSigninDialogInitialize');
 status_signin_dialog:=SCE_SIGNIN_DIALOG_STATUS_INITIALIZED;
 Result:=0;
end;

function ps4_sceSigninDialogTerminate():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSigninDialogTerminate');
 status_signin_dialog:=SCE_SIGNIN_DIALOG_STATUS_NONE;
 Result:=0;
end;

type
 pSceSigninDialogParam=^SceSigninDialogParam;
 SceSigninDialogParam=packed record
  size:Integer;
  userId:Integer;
  reserved:array[0..1] of Integer;
 end;

function ps4_sceSigninDialogOpen(param:pSceSigninDialogParam):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSigninDialogOpen');
 status_signin_dialog:=SCE_SIGNIN_DIALOG_STATUS_FINISHED;
 Result:=0;
end;

function ps4_sceSigninDialogUpdateStatus:Integer; SysV_ABI_CDecl;
begin
 Result:=status_signin_dialog;
end;

function ps4_scePlayerInvitationDialogTerminate():Integer; SysV_ABI_CDecl;
begin
 Writeln('scePlayerInvitationDialogTerminate');
 Result:=0;
end;

//

const
 SCE_IME_DIALOG_STATUS_NONE    =0;
 SCE_IME_DIALOG_STATUS_RUNNING =1;
 SCE_IME_DIALOG_STATUS_FINISHED=2;

type
 pSceImeDialogParam=^SceImeDialogParam;
 SceImeDialogParam=packed record
  userId,_type       :SceImeType;
  supportedLanguages :QWORD;
  enterLabel         :SceImeEnterLabel;
  inputMethod        :SceImeInputMethod;
  filter             :SceImeTextFilter;
  option             :DWORD;
  maxTextLength      :DWORD;
  inputTextBuffer    :PWideChar;
  posx               :Single;
  posy               :Single;
  horizontalAlignment:SceImeHorizontalAlignment;
  verticalAlignment  :SceImeVerticalAlignment;
  placeholder        :PWideChar;
  title              :PWideChar;
  reserved           :array[0..15] of ShortInt;
 end;

 pSceImeParamExtended=^SceImeParamExtended;
 SceImeParamExtended=packed record
  option                  :DWORD;
  colorBase               :SceImeColor;
  colorLine               :SceImeColor;
  colorTextField          :SceImeColor;
  colorPreedit            :SceImeColor;
  colorButtonDefault      :SceImeColor;
  colorButtonFunction     :SceImeColor;
  colorButtonSymbol       :SceImeColor;
  colorText               :SceImeColor;
  colorSpecial            :SceImeColor;
  priority                :SceImePanelPriority;
  additionalDictionaryPath:PChar;
  extKeyboardFilter       :SceImeExtKeyboardFilter;
  disableDevice           :DWORD;
  extKeyboardMode         :DWORD;
  reserved                :array[0..59] of ShortInt;
 end;

var
 status_ime_dialog:Integer=SCE_IME_DIALOG_STATUS_NONE;

function ps4_sceImeDialogInit(const param:pSceImeDialogParam;
                              const extended:pSceImeParamExtended):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end; 

function ps4_sceImeDialogGetStatus:Integer; SysV_ABI_CDecl;
begin
 Result:=status_ime_dialog;
end;

//

function ps4_sceLoginDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

//

function ps4_sceHmdSetupDialogInitialize():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdSetupDialogOpen(param:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdSetupDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_FINISHED;
end;

function ps4_sceHmdSetupDialogGetResult(pResult:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdSetupDialogTerminate():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

//

function ps4_sceNpFriendListDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

//

function ps4_sceInvitationDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
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
 lib^.set_proc($050DED7B2D099903,@ps4_sceCommonDialogIsUsed);
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
 lib^.set_proc($336645FC294B8606,@ps4_sceErrorDialogOpen);
 lib^.set_proc($596886BA1F577E04,@ps4_sceErrorDialogUpdateStatus);
 lib^.set_proc($B7616F1D15F382A9,@ps4_sceErrorDialogGetStatus);
 lib^.set_proc($F570312B63CCC24F,@ps4_sceErrorDialogTerminate);
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
 lib^.set_proc($85A55913D1602AA1,@ps4_sceNpProfileDialogUpdateStatus);
 lib^.set_proc($D12A7DBC9701D7FC,@ps4_sceNpProfileDialogTerminate);
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
 lib^.set_proc($E2D3E1B0FE85A432,@ps4_sceSaveDataDialogOpen);
 lib^.set_proc($7A7EE03559E1F3BF,@ps4_sceSaveDataDialogIsReadyToDisplay);
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
 lib^.set_proc($1D3ADC0CA9452AE3,@ps4_sceMsgDialogClose);
 lib^.set_proc($E9F202DD72ADDA4D,@ps4_sceMsgDialogUpdateStatus);
 lib^.set_proc($096556EFC41CDDF2,@ps4_sceMsgDialogGetStatus);
 lib^.set_proc($2EBF28BC71FD97A0,@ps4_sceMsgDialogGetResult);
 lib^.set_proc($78FC3F92A6667A5A,@ps4_sceMsgDialogTerminate);
end;

function Load_libSceNpCommerce(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceNpCommerce');
 lib^.set_proc($D1A4766969906A5E,@ps4_sceNpCommerceDialogInitialize);
 lib^.set_proc($0DF4820D10371236,@ps4_sceNpCommerceDialogOpen);
 lib^.set_proc($2D1E5CC0530C0951,@ps4_sceNpCommerceDialogUpdateStatus);
 lib^.set_proc($0826C2FA5AAABC5D,@ps4_sceNpCommerceDialogGetStatus);
 lib^.set_proc($AF8D9B59C41BB596,@ps4_sceNpCommerceDialogGetResult);
 lib^.set_proc($9BF23DD806F9D16F,@ps4_sceNpCommerceDialogTerminate);
 lib^.set_proc($0C79B0B1AE92F137,@ps4_sceNpCommerceShowPsStoreIcon);
 lib^.set_proc($76CA8256C34CD198,@ps4_sceNpCommerceHidePsStoreIcon);
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

function Load_libScePlayerInvitationDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libScePlayerInvitationDialog');
 lib^.set_proc($8039B96BA19213DE,@ps4_scePlayerInvitationDialogTerminate);
end;

function Load_libSceImeDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceImeDialog');
 lib^.set_proc($354781ACDEE1CDFD,@ps4_sceImeDialogInit); 
 lib^.set_proc($2000E60F8B527016,@ps4_sceImeDialogGetStatus);
end;

function Load_libSceLoginDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceLoginDialog');
 lib^.set_proc($A8FFC4BD0465D877,@ps4_sceLoginDialogInitialize);
end;

function Load_libSceHmdSetupDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceHmdSetupDialog');
 lib^.set_proc($341D58DA40368C26,@ps4_sceHmdSetupDialogInitialize);
 lib^.set_proc($34D8225784FE6A45,@ps4_sceHmdSetupDialogOpen);
 lib^.set_proc($51DEE3DFE4432018,@ps4_sceHmdSetupDialogUpdateStatus);
 lib^.set_proc($EA55511CC5792D8D,@ps4_sceHmdSetupDialogGetResult);
 lib^.set_proc($FB3E0E26616B7997,@ps4_sceHmdSetupDialogTerminate);
end;

function Load_libSceNpFriendListDialog(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceNpFriendListDialog');
 lib^.set_proc($7EBC33DDECAE03AC,@ps4_sceNpFriendListDialogUpdateStatus);
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
 ps4_app.RegistredPreLoad('libSceCommonDialog.prx'          ,@Load_libSceCommonDialog);
 ps4_app.RegistredPreLoad('libSceErrorDialog.prx'           ,@Load_libSceErrorDialog);
 ps4_app.RegistredPreLoad('libSceNpProfileDialog.prx'       ,@Load_libSceNpProfileDialog);
 ps4_app.RegistredPreLoad('libSceSaveDataDialog.prx'        ,@Load_libSceSaveDataDialog);
 ps4_app.RegistredPreLoad('libSceMsgDialog.prx'             ,@Load_libSceMsgDialog);
 ps4_app.RegistredPreLoad('libSceNpCommerce.prx'            ,@Load_libSceNpCommerce);
 ps4_app.RegistredPreLoad('libSceSigninDialog.prx'          ,@Load_libSceSigninDialog);
 ps4_app.RegistredPreLoad('libScePlayerInvitationDialog.prx',@Load_libScePlayerInvitationDialog);
 ps4_app.RegistredPreLoad('libSceImeDialog.prx'             ,@Load_libSceImeDialog);
 ps4_app.RegistredPreLoad('libSceLoginDialog.prx'           ,@Load_libSceLoginDialog);
 ps4_app.RegistredPreLoad('libSceHmdSetupDialog.prx'        ,@Load_libSceHmdSetupDialog);
 ps4_app.RegistredPreLoad('libSceNpFriendListDialog.prx'    ,@Load_libSceNpFriendListDialog); 
 ps4_app.RegistredPreLoad('libSceInvitationDialog.prx'      ,@Load_libSceInvitationDialog); 

end.

