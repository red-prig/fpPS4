unit ps4_libSceSaveDataDialog;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_mtx,
 subr_dynlib,
 kern_proc,
 ps4_libSceSaveData,
 ps4_libSceCommonDialog;

const
 //SceSaveDataDialogAnimation
 SCE_SAVE_DATA_DIALOG_ANIMATION_ON =0;
 SCE_SAVE_DATA_DIALOG_ANIMATION_OFF=1;

 //SceSaveDataDialogOptionBack
 SCE_SAVE_DATA_DIALOG_OPTION_BACK_ENABLE =0;
 SCE_SAVE_DATA_DIALOG_OPTION_BACK_DISABLE=1;

 //SceSaveDataDialogMode
 SCE_SAVE_DATA_DIALOG_MODE_INVALID       =0;
 SCE_SAVE_DATA_DIALOG_MODE_LIST          =1;
 SCE_SAVE_DATA_DIALOG_MODE_USER_MSG      =2;
 SCE_SAVE_DATA_DIALOG_MODE_SYSTEM_MSG    =3;
 SCE_SAVE_DATA_DIALOG_MODE_ERROR_CODE    =4;
 SCE_SAVE_DATA_DIALOG_MODE_PROGRESS_BAR  =5;
 SCE_SAVE_DATA_DIALOG_MODE_WIZARD_LIST   =6;
 SCE_SAVE_DATA_DIALOG_MODE_WIZARD_CONFIRM=7;

 //SceSaveDataDialogType
 SCE_SAVE_DATA_DIALOG_TYPE_SAVE  =1;
 SCE_SAVE_DATA_DIALOG_TYPE_LOAD  =2;
 SCE_SAVE_DATA_DIALOG_TYPE_DELETE=3;

 //SceSaveDataDialogFocusPos
 SCE_SAVE_DATA_DIALOG_FOCUS_POS_LISTHEAD  =0;
 SCE_SAVE_DATA_DIALOG_FOCUS_POS_LISTTAIL  =1;
 SCE_SAVE_DATA_DIALOG_FOCUS_POS_DATAHEAD  =2;
 SCE_SAVE_DATA_DIALOG_FOCUS_POS_DATATAIL  =3;
 SCE_SAVE_DATA_DIALOG_FOCUS_POS_DATALATEST=4;
 SCE_SAVE_DATA_DIALOG_FOCUS_POS_DATAOLDEST=5;
 SCE_SAVE_DATA_DIALOG_FOCUS_POS_DIRNAME   =6;

 //SceSaveDataDialogItemStyle
 SCE_SAVE_DATA_DIALOG_ITEM_STYLE_TITLE_DATESIZE_SUBTITLE=0;
 SCE_SAVE_DATA_DIALOG_ITEM_STYLE_TITLE_SUBTITLE_DATESIZE=1;
 SCE_SAVE_DATA_DIALOG_ITEM_STYLE_TITLE_DATESIZE         =2;

 //SceSaveDataDialogButtonType
 SCE_SAVE_DATA_DIALOG_BUTTON_TYPE_OK      =0;
 SCE_SAVE_DATA_DIALOG_BUTTON_TYPE_YESNO   =1;
 SCE_SAVE_DATA_DIALOG_BUTTON_TYPE_NONE    =2;
 SCE_SAVE_DATA_DIALOG_BUTTON_TYPE_OKCANCEL=3;

 //SceSaveDataDialogUserMessageType
 SCE_SAVE_DATA_DIALOG_USERMSG_TYPE_NORMAL=0;
 SCE_SAVE_DATA_DIALOG_USERMSG_TYPE_ERROR =1;

 //SceSaveDataDialogSystemMessageType
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_INVALID              =0;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NODATA               =1;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CONFIRM              =2;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_OVERWRITE            =3;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE              =4;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_PROGRESS             =5;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_FILE_CORRUPTED       =6;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_FINISHED             =7;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE_CONTINUABLE  =8;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_RESTORE              =9;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CORRUPTED_AND_DELETE =10;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CORRUPTED_AND_CREATE =11;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CORRUPTED_AND_RESTORE=13;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_TOTAL_SIZE_EXCEEDED  =14;
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE_RESTORE      =15;

 //SceSaveDataDialogProgressBarType
 SCE_SAVE_DATA_DIALOG_PROGRESSBAR_TYPE_PERCENTAGE=0;

 //SceSaveDataDialogProgressSystemMessageType
 SCE_SAVE_DATA_DIALOG_PRGRESS_SYSMSG_TYPE_INVALID =0;
 SCE_SAVE_DATA_DIALOG_PRGRESS_SYSMSG_TYPE_PROGRESS=1;
 SCE_SAVE_DATA_DIALOG_PRGRESS_SYSMSG_TYPE_RESTORE =2;

 //SceSaveDataDialogOptionFlag
 SCE_SAVE_DATA_DIALOG_OPTION_FLAG_DEFAULT=0;

 //SceSaveDataDialogWizardOption
 SCE_SAVE_DATA_DIALOG_WIZARD_OPTION_RESTORE_OR_DELETE       =1;
 SCE_SAVE_DATA_DIALOG_WIZARD_OPTION_RESTORE_OR_RECREATE     =2;
 SCE_SAVE_DATA_DIALOG_WIZARD_OPTION_RESTORE_OR_CORRUPTED    =3;
 SCE_SAVE_DATA_DIALOG_WIZARD_OPTION_SELECT_WITH_CONFIRMATION=$10000;
 SCE_SAVE_DATA_DIALOG_WIZARD_OPTION_DISPLAY_NO_DATA         =$20000;

 //SceSaveDataDialogButtonId
 SCE_SAVE_DATA_DIALOG_BUTTON_ID_INVALID=0;
 SCE_SAVE_DATA_DIALOG_BUTTON_ID_OK     =1;
 SCE_SAVE_DATA_DIALOG_BUTTON_ID_NO     =2;

{$CALLING default}

type
 PSaveDialogOpen=^TSaveDialogOpen;
 TSaveDialogOpen=record
  //
 end;

 TSaveDialogResult=record
  //
 end;

 TSaveDialogClient=class(TCommonDialogClient)
  data:TSaveDialogOpen;
  rate:DWORD;
 end;

implementation

type
 pSceSaveDataDialogCloseParam=^SceSaveDataDialogCloseParam;
 SceSaveDataDialogCloseParam=packed record
  anim    :Integer; //SceSaveDataDialogAnimation
  reserved:array[0..31] of Byte;
 end;

 pSceSaveDataDialogNewItem=^SceSaveDataDialogNewItem;
 SceSaveDataDialogNewItem=packed record
  title   :pchar;
  iconBuf :Pointer;
  iconSize:qword;
  reserved:array[0..31] of Byte;
 end;

 pSceSaveDataDialogAnimationParam=^SceSaveDataDialogAnimationParam;
 SceSaveDataDialogAnimationParam=packed record
  userOK    :Integer; //SceSaveDataDialogAnimation
  userCancel:Integer; //SceSaveDataDialogAnimation
  reserved:array[0..31] of Byte;
 end;

 pSceSaveDataDialogSystemMessageParam=^SceSaveDataDialogSystemMessageParam;
 SceSaveDataDialogSystemMessageParam=packed record
  sysMsgType:Integer; //SceSaveDataDialogSystemMessageType
  _align    :Integer;
  value     :QWORD;
  reserved  :array[0..31] of Byte;
 end;

 pSceSaveDataDialogItems=^SceSaveDataDialogItems;
 SceSaveDataDialogItems=packed record
  userId         :Integer;
  _align1        :Integer;
  titleId        :pSceSaveDataTitleId;
  dirName        :pSceSaveDataDirName;
  dirNameNum     :DWORD;
  _align2        :Integer;
  newItem        :pSceSaveDataDialogNewItem;
  focusPos       :Integer; //SceSaveDataDialogFocusPos
  _align3        :Integer;
  focusPosDirName:pSceSaveDataDirName;
  itemStyle      :Integer; //SceSaveDataDialogItemStyle
  reserved       :array[0..35] of Byte;
 end;

 pSceSaveDataDialogUserMessageParam=^SceSaveDataDialogUserMessageParam;
 SceSaveDataDialogUserMessageParam=packed record
  buttonType:Integer; //SceSaveDataDialogButtonType
  msgType   :Integer; //SceSaveDataDialogUserMessageType
  msg       :pchar;
  reserved  :array[0..31] of Byte;
 end;

 pSceSaveDataDialogErrorCodeParam=^SceSaveDataDialogErrorCodeParam;
 SceSaveDataDialogErrorCodeParam=packed record
  errorCode:Integer;
  reserved :array[0..31] of Byte;
 end;

 pSceSaveDataDialogProgressBarParam=^SceSaveDataDialogProgressBarParam;
 SceSaveDataDialogProgressBarParam=packed record
  barType   :Integer; //SceSaveDataDialogProgressBarType
  _align    :Integer;
  msg       :pchar;
  sysMsgType:Integer; //SceSaveDataDialogProgressSystemMessageType
  reserved  :array[0..27] of Byte;
 end;

 pSceSaveDataDialogOptionParam=^SceSaveDataDialogOptionParam;
 SceSaveDataDialogOptionParam=packed record
  back:Integer; //SceSaveDataDialogOptionBack
  flag:Integer; //SceSaveDataDialogOptionFlag
  reserved  :array[0..27] of Byte;
 end;

 pSceSaveDataDialogWizardParam=^SceSaveDataDialogWizardParam;
 SceSaveDataDialogWizardParam=packed record
  option     :Integer; //SceSaveDataDialogWizardOption
  reserved1  :Integer;
  fingerprint:pSceSaveDataFingerprint;
  reserved   :array[0..31] of Byte;
 end;

 pSceSaveDataDialogParam=^SceSaveDataDialogParam;
 SceSaveDataDialogParam=packed record
  baseParam     :SceCommonDialogBaseParam;
  size          :Integer;
  mode          :Integer; //SceSaveDataDialogMode
  dispType      :Integer; //SceSaveDataDialogType
  _align        :Integer;
  animParam     :pSceSaveDataDialogAnimationParam;
  items         :pSceSaveDataDialogItems;
  userMsgParam  :pSceSaveDataDialogUserMessageParam;
  sysMsgParam   :pSceSaveDataDialogSystemMessageParam;
  errorCodeParam:pSceSaveDataDialogErrorCodeParam;
  progBarParam  :pSceSaveDataDialogProgressBarParam;
  userData      :Pointer;
  optionParam   :pSceSaveDataDialogOptionParam;
  wizardParam   :pSceSaveDataDialogWizardParam;
  reserved      :array[0..15] of Byte;
 end;

 pSceSaveDataDialogResult=^SceSaveDataDialogResult;
 SceSaveDataDialogResult=packed record
  mode    :Integer; //SceSaveDataDialogMode     //Mode of function
  result  :Integer;                             //Result of executing function
  buttonId:Integer; //SceSaveDataDialogButtonId //Id of button user selected
  _align  :Integer;
  dirName :pSceSaveDataDirName;       //savedata directory name
  param   :pSceSaveDataParam;         //Buffer to receive savedata information ( can be set NULL if you don't need it)
  userData:Pointer;                   //Userdata specified at calling function
  reserved:array[0..31] of Byte;      //Reserved range (must be filled by zero)
 end;

var
 g_SaveDialog_mtx:mtx;
 g_client        :TSaveDialogClient=nil;

{$CALLING SysV_ABI_CDecl}

//

var
 status_save_dialog:Integer=SCE_COMMON_DIALOG_STATUS_NONE;

function ps4_sceSaveDataDialogInitialize():Integer;
begin
 Writeln('sceSaveDataDialogInitialize');
 status_save_dialog:=SCE_COMMON_DIALOG_STATUS_INITIALIZED;
 Result:=0;
end;

//SceSaveDataDialogParam
function ps4_sceSaveDataDialogOpen(param:pSceSaveDataDialogParam):Integer;
begin
 if (param=nil) then Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 Writeln('sceSaveDataDialogOpen:');
 status_save_dialog:=SCE_COMMON_DIALOG_STATUS_FINISHED;
 Result:=0;
end;

function ps4_sceSaveDataDialogClose(closeParam:pSceSaveDataDialogCloseParam):Integer;
begin
 Writeln('sceSaveDataDialogClose');
 status_save_dialog:=SCE_COMMON_DIALOG_STATUS_FINISHED;
 Result:=0;
end;

function ps4_sceSaveDataDialogTerminate():Integer;
begin
 Writeln('sceSaveDataDialogTerminate');
 status_save_dialog:=SCE_COMMON_DIALOG_STATUS_NONE;
 Result:=0;
end;

function ps4_sceSaveDataDialogIsReadyToDisplay:Integer;
begin
 Result:=1;
end;

function ps4_sceSaveDataDialogUpdateStatus():Integer;
begin
 Result:=status_save_dialog;
end;

function ps4_sceSaveDataDialogGetStatus():Integer;
begin
 Result:=status_save_dialog;
end;

function ps4_sceSaveDataDialogGetResult(_result:pSceSaveDataDialogResult):Integer;
begin
 //Writeln('sceSaveDataDialogGetResult');
 Result:=0;
end;

function ps4_sceSaveDataDialogProgressBarSetValue(target:Integer;rate:DWORD):Integer;
begin
 Writeln('sceSaveDataDialogProgressBarSetValue:',rate);
 Result:=0;
end;

function Load_libSceSaveDataDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceSaveDataDialog');

 lib:=Result^.add_lib('libSceSaveDataDialog');
 lib.set_proc($B3D7B7F98A519F3C,@ps4_sceSaveDataDialogInitialize);
 lib.set_proc($E2D3E1B0FE85A432,@ps4_sceSaveDataDialogOpen);
 lib.set_proc($7C7E3A2DA83CF176,@ps4_sceSaveDataDialogClose);
 lib.set_proc($62E1F6140EDACEA4,@ps4_sceSaveDataDialogTerminate);
 lib.set_proc($7A7EE03559E1F3BF,@ps4_sceSaveDataDialogIsReadyToDisplay);
 lib.set_proc($28ADC1760D5158AD,@ps4_sceSaveDataDialogUpdateStatus);
 lib.set_proc($1112B392C6AE0090,@ps4_sceSaveDataDialogGetStatus);
 lib.set_proc($C84889FEAAABE828,@ps4_sceSaveDataDialogGetResult);
 lib.set_proc($85ACB509F4E62F20,@ps4_sceSaveDataDialogProgressBarSetValue);

 mtx_init(g_SaveDialog_mtx,'g_SaveDialog_mtx');
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceSaveDataDialog.prx',@Load_libSceSaveDataDialog);

end.

