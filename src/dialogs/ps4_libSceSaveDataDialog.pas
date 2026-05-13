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
 SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_12                   =12;
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
 TSaveDialogNewItem=record
  title   :array[0..127] of Char;
  iconBuf :array[0..116735] of Byte;
  iconSize:QWORD;
 end;

 TSaveDialogOpen=record
  //base
  mode           :Byte; //SceSaveDataDialogMode
  dispType       :Byte; //SceSaveDataDialogType
  userOK         :Byte; //SceSaveDataDialogAnimation
  userCancel     :Byte; //SceSaveDataDialogAnimation
  back           :Byte; //SceSaveDataDialogOptionBack
  focusPos       :Byte; //SceSaveDataDialogFocusPos
  itemStyle      :Byte; //SceSaveDataDialogItemStyle
  is_new         :Byte;
  bar_sysMsgType :Byte; //SceSaveDataDialogProgressSystemMessageType
  sys_sysMsgType :Byte; //SceSaveDataDialogSystemMessageType
  buttonType     :Byte; //SceSaveDataDialogButtonType
  msgType        :Byte; //SceSaveDataDialogUserMessageType
  userData       :Pointer;
  //SystemMessage_info
  sys_value      :QWORD;
  //dir info
  userId         :Integer;
  dirNameNum     :DWORD;
  titleId        :array[0..SCE_SAVE_DATA_TITLE_ID_DATA_SIZE-1] of Char;
  dirNames       :array[0..1023] of SceSaveDataDirName;
  new_item       :TSaveDialogNewItem;
  focusPosDirName:SceSaveDataDirName;
  //error info
  errorCode      :Integer;
  //WizardParam info
  option         :Integer;
  fingerprint    :array[0..SCE_SAVE_DATA_FINGERPRINT_DATA_SIZE-1] of Char;
  //ProgressBar info
  bar_msg        :array[0..255] of Char;
  //UserMessage info
  user_msg       :array[0..255] of Char;
 end;

 TSaveDialogResult=record
  resultId:Integer;
  buttonId:Integer; //SceSaveDataDialogButtonId
  dirName :SceSaveDataDirName;
  param   :SceSaveDataParam;
 end;

 TSaveDialogClient=class(TCommonDialogClient)
  data  :TSaveDialogOpen;
  rate  :DWORD;
  rzdata:TSaveDialogResult;
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
  reserved2  :array[0..31] of Byte;
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

function ps4_sceSaveDataDialogInitialize():Integer;
var
 client:TSaveDialogClient;
begin
 Result:=0;
 Writeln('sceSaveDataDialogInitialize');

 mtx_lock(g_SaveDialog_mtx);

  Result:=SCE_COMMON_DIALOG_ERROR_ALREADY_INITIALIZED;
  if (g_client=nil) then
  begin

   Result:=SCE_COMMON_DIALOG_ERROR_BUSY;
   if (not ps4_sceCommonDialogIsUsed) then
   begin
    client:=TSaveDialogClient.Create;

    Result:=client.launchCmnDialog();

    if (Result<>0) then
    begin
     client.Free;
    end else
    begin
     g_client:=client;
    end;

   end;

  end;

 mtx_unlock(g_SaveDialog_mtx);
end;

function CheckMode(mode:Integer):Integer; inline;
begin
 if (mode < 1) or (7 < mode) then
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 end else
 begin
  Result:=0;
 end;
end;

function CheckDispType(dispType:Integer):Integer; inline;
begin
 if (dispType < 1) or (3 < dispType) then
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 end else
 begin
  Result:=0;
 end;
end;

function CheckAnim(anim:Integer):Integer; inline;
begin
 if (anim < 0) or (1 < anim) then
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 end else
 begin
  Result:=0;
 end;
end;

function CheckBack(back:Integer):Integer; inline;
begin
 if (back < 0) or (1 < back) then
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 end else
 begin
  Result:=0;
 end;
end;

function CheckItemStyle(itemStyle:Integer):Integer; inline;
begin
 if (itemStyle < 0) or (2 < itemStyle) then
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 end else
 begin
  Result:=0;
 end;
end;

function CheckFocusPos(focusPos:Integer):Integer; inline;
begin
 if (focusPos < 0) or (6 < focusPos) then
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 end else
 begin
  Result:=0;
 end;
end;

function CheckBarType(barType:Integer):Integer; inline;
begin
 if (barType <> 0) then
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 end else
 begin
  Result:=0;
 end;
end;

function CheckSysMsgType(sysMsgType:Integer):Integer; inline;
begin
 if (sysMsgType < 0) or (2 < sysMsgType) then
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 end else
 begin
  Result:=0;
 end;
end;

function CheckButtonType(buttonType:Integer):Integer; inline;
begin
 if (buttonType < 0) or (3 < buttonType) then
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 end else
 begin
  Result:=0;
 end;
end;

function CheckAnimParam(animParam:pSceSaveDataDialogAnimationParam):Integer;
begin
 if (animParam=nil) then Exit(0);
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if CheckAnim(animParam^.userOK)=0 then
 if CheckAnim(animParam^.userCancel)=0 then
 if CheckReserved(animParam^.reserved,sizeof(animParam^.reserved))=0 then
 begin
  Result:=0;
 end;
end;

function CheckOptionParam(optionParam:pSceSaveDataDialogOptionParam):Integer;
begin
 if (optionParam=nil) then Exit(0);
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if CheckBack(optionParam^.back)=0 then
 if (optionParam^.flag=SCE_SAVE_DATA_DIALOG_OPTION_FLAG_DEFAULT) then
 if CheckReserved(optionParam^.reserved,sizeof(optionParam^.reserved))=0 then
 begin
  Result:=0;
 end;
end;

function CheckSaveDataParam(param:pSceSaveDataDialogParam):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (param<>nil) then
 if CheckMode     (param^.mode)=0 then
 if CheckDispType (param^.dispType)=0 then
 if CheckAnimParam(param^.animParam)=0 then
 if (param^.items<>nil) then
 begin

  case param^.mode of
   SCE_SAVE_DATA_DIALOG_MODE_LIST:
     begin
      if (param^.userMsgParam   <>nil) or
         (param^.sysMsgParam    <>nil) or
         (param^.errorCodeParam <>nil) or
         (param^.progBarParam   <>nil) or
         (param^.wizardParam    <>nil) then
      begin
       Exit;
      end;
     end;
   SCE_SAVE_DATA_DIALOG_MODE_USER_MSG:
     begin
      if (param^.userMsgParam   =nil) or
         (param^.sysMsgParam   <>nil) or
         (param^.errorCodeParam<>nil) or
         (param^.progBarParam  <>nil) or
         (param^.wizardParam   <>nil) then
      begin
       Exit;
      end;
     end;
   SCE_SAVE_DATA_DIALOG_MODE_SYSTEM_MSG:
     begin
      if (param^.sysMsgParam     =nil) or
         (param^.userMsgParam   <>nil) or
         (param^.errorCodeParam <>nil) or
         (param^.progBarParam   <>nil) or
         (param^.wizardParam    <>nil) then
      begin
       Exit;
      end;
     end;
   SCE_SAVE_DATA_DIALOG_MODE_ERROR_CODE:
     begin
      if (param^.errorCodeParam =nil) or
         (param^.userMsgParam  <>nil) or
         (param^.sysMsgParam   <>nil) or
         (param^.progBarParam  <>nil) or
         (param^.wizardParam   <>nil) then
      begin
       Exit;
      end;
     end;
   SCE_SAVE_DATA_DIALOG_MODE_PROGRESS_BAR:
     begin
      if (param^.progBarParam   =nil) or
         (param^.userMsgParam  <>nil) or
         (param^.sysMsgParam   <>nil) or
         (param^.errorCodeParam<>nil) or
         (param^.wizardParam   <>nil) then
      begin
       Exit;
      end;
     end;
   SCE_SAVE_DATA_DIALOG_MODE_WIZARD_LIST:
     begin
      if (param^.wizardParam    =nil) or
         (param^.userMsgParam  <>nil) or
         (param^.sysMsgParam   <>nil) or
         (param^.errorCodeParam<>nil) or
         (param^.progBarParam  <>nil) then
      begin
       Exit;
      end;
     end;
   SCE_SAVE_DATA_DIALOG_MODE_WIZARD_CONFIRM:
     begin
      if (param^.wizardParam    =nil) or
         (param^.userMsgParam  <>nil) or
         (param^.sysMsgParam   <>nil) or
         (param^.errorCodeParam<>nil) or
         (param^.progBarParam  <>nil) then
      begin
       Exit;
      end;
     end;
   else;
  end;

  if CheckOptionParam(param^.optionParam)=0 then
  if CheckReserved(param^.reserved,sizeof(param^.reserved))=0 then
  begin
   Result:=0;
  end;

 end;
end;

function IsLoggedIn(userId:Integer):Integer; inline;
begin
 //sceUserServiceIsLoggedIn
 Result:=0;
end;

function IsRegistered(userId:Integer):Integer; inline;
begin
 //sceUserServiceGetRegisteredUserIdList
 Result:=0;
end;

function _CheckTitleId(titleId:pchar):Integer;
var
 len,i:DWORD;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 len:=strnlen_s(titleId,10);
 if (len < 10) then
 begin
  for i:=0 to 3 do
   if (titleId[i] < 'A') or(titleId[i] > 'Z') then
   begin
    Exit;
   end;
  for i:=4 to 8 do
   if (titleId[i] < '0') or(titleId[i] > '9') then
   begin
    Exit;
   end;
  //
  Result:=0;
 end;
end;

function CheckTitleId(titleId:pSceSaveDataTitleId):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (titleId<>nil) then
 if _CheckTitleId(@titleId^.data)=0 then
 if CheckReserved(titleId^.padding,sizeof(titleId^.padding))=0 then
 begin
  Result:=0;
 end;
end;

function _CheckDirName(dirName:pchar):Integer;
var
 len,i:DWORD;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 len:=strnlen_s(dirName,32);
 if (len < 32) then
 begin
  i:=0;
  while (i < len) do
  begin
   if ((dirName[i] < 'a') or ('z' < dirName[i])) and
      ((dirName[i] < 'A') or ('Z' < dirName[i])) and
      ((dirName[i] < '0') or ('9' < dirName[i])) and
      (dirName[i] <> '-') and
      (dirName[i] <> '.') and
      (dirName[i] <> '@') then
   begin
    Exit;
   end;
   i:=i+1;
  end;
  //
  Result:=0;
 end;
end;

function CheckDirName(dirName:pSceSaveDataDirName):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (dirName<>nil) then
 if (dirName^.data[0]<>#0) then
 begin
  Result:=_CheckDirName(@dirName^.data);
 end;
end;

function CheckDirNamesDuplicates(dirName:pSceSaveDataDirName;dirNameNum:DWORD):Integer;
var
 len,i,s:DWORD;
begin
 Result:=0;
 if (dirNameNum<>0) then
 for i:=0 to dirNameNum-1 do
 begin
  if (dirName[i].data[0]=#0) or (CheckDirName(@dirName[i])<>0) then
  begin
   Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
  end;
  //
  len:=strnlen_s(@dirName[i].data,$20);
  //
  for s:=0 to dirNameNum-1 do
  if (i<>s) then
  begin
   if (strncmp(@dirName[i].data,@dirName[s].data,len+1)=0) then
   begin
    Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
   end;
  end;
 end;
end;

function CheckNewItem(newItem:pSceSaveDataDialogNewItem):Integer;
var
 len,i:DWORD;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (newItem^.title<>nil) then
 begin
  len:=strnlen_s(newItem^.title,$80);
  if (len >= $80) then
  begin
   Exit;
  end;
  //
  i:=0;
  repeat
   case newItem^.title[i] of
     #0:Break;
    #10:Exit;
    #13:Exit;
    else;
   end;
   Inc(i);
  until (i = len);
  //
  //CheckUtf8???
 end;
 //
 if (newItem^.iconBuf = nil) then
 begin
  if (newItem^.iconSize <> 0) then
  begin
   Exit;
  end;
 end else
 if (newItem^.iconSize = 0) or (newItem^.iconSize > $1c800) then
 begin
  Exit;
 end;
 //
 if CheckReserved(newItem^.reserved,sizeof(newItem^.reserved))=0 then
 begin
  Result:=0;
 end;
end;

function CheckNonListItems(dispType:Integer;items:pSceSaveDataDialogItems):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (IsLoggedIn(items^.userId)=0) then
 if (p_proc.p_sdk_version < $1700000) or
    (IsRegistered(items^.userId)=0) then
 if (items^.titleId = nil) or
    (CheckTitleId(items^.titleId)=0) then
 if (items^.dirNameNum <= $400) then
 if (items^.dirName = nil) or
    (items^.dirName^.data[0]=#0) or
    (CheckDirName(items^.dirName)=0) then
 if (dispType <> SCE_SAVE_DATA_DIALOG_TYPE_SAVE) or
    (items^.newItem = nil) or
    (CheckNewItem(items^.newItem)=0) then
 if (items^.focusPos=SCE_SAVE_DATA_DIALOG_FOCUS_POS_LISTHEAD) then
 if (items^.focusPosDirName = nil) then
 if (p_proc.p_sdk_version < $6000000) or
    (CheckItemStyle(items^.itemStyle)=0) then
 if (CheckReserved(items^.reserved,sizeof(items^.reserved))=0) then
 begin
  Result:=0;
 end;
end;

function CheckListItems_old(dispType:Integer;items:pSceSaveDataDialogItems):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (IsLoggedIn(items^.userId)=0) then
 if (p_proc.p_sdk_version < $1700000) or
    (IsRegistered(items^.userId)=0) then
 if (items^.titleId = nil) or
    (CheckTitleId(items^.titleId)=0) then
 begin

  if (dispType = SCE_SAVE_DATA_DIALOG_TYPE_LOAD  ) or
     (dispType = SCE_SAVE_DATA_DIALOG_TYPE_DELETE) then
  begin
   if (items^.dirNameNum = 0) or
      (items^.dirName = nil) then
   begin
    Exit;
   end;
  end;

  if (items^.dirNameNum > $400) then
  begin
   Exit;
  end;

  if (items^.dirName <> nil) then
  if (CheckDirNamesDuplicates(items^.dirName,items^.dirNameNum)<>0) then
  begin
   Exit;
  end;

  if (dispType<>SCE_SAVE_DATA_DIALOG_TYPE_SAVE) or
     (items^.newItem = nil) or
     (CheckNewItem(items^.newItem)=0) then
  if (CheckFocusPos(items^.focusPos)=0) then
  begin

   if (items^.focusPosDirName <> nil) then
   begin
    if (items^.focusPos <> SCE_SAVE_DATA_DIALOG_FOCUS_POS_DIRNAME) or
       (CheckDirName(items^.focusPosDirName)<>0) then
    begin
     Exit;
    end;
   end;

   if (CheckItemStyle(items^.itemStyle)=0) then
   if (CheckReserved(items^.reserved,sizeof(items^.reserved))=0) then
   begin
    Result:=0;
   end;

  end;

 end;
end;

function CheckListItems_new(mode,dispType:Integer;items:pSceSaveDataDialogItems):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (IsLoggedIn(items^.userId)=0) then
 if (IsRegistered(items^.userId)=0) then
 if (items^.titleId = nil) or
    (CheckTitleId(items^.titleId)=0) then
 begin

  if (mode = SCE_SAVE_DATA_DIALOG_MODE_LIST) and
     (
      (dispType = SCE_SAVE_DATA_DIALOG_TYPE_LOAD  ) or
      (dispType = SCE_SAVE_DATA_DIALOG_TYPE_DELETE)
     ) then
  begin
   if (items^.dirNameNum = 0) or
      (items^.dirName = nil) then
   begin
    Exit;
   end;
  end;

  if (items^.dirNameNum > $400) then
  begin
   Exit;
  end;

  if (items^.dirName <> nil) then
  if (CheckDirNamesDuplicates(items^.dirName,items^.dirNameNum)<>0) then
  begin
   Exit;
  end;

  if (dispType<>SCE_SAVE_DATA_DIALOG_TYPE_SAVE) or
     (items^.newItem = nil) or
     (CheckNewItem(items^.newItem)=0) then
  if (CheckFocusPos(items^.focusPos)=0) then
  begin

   if (items^.focusPosDirName <> nil) then
   begin
    if (items^.focusPos <> SCE_SAVE_DATA_DIALOG_FOCUS_POS_DIRNAME) or
       (CheckDirName(items^.focusPosDirName)<>0) then
    begin
     Exit;
    end;
   end;

   if (CheckItemStyle(items^.itemStyle)=0) then
   if (CheckReserved(items^.reserved,sizeof(items^.reserved))=0) then
   begin
    Result:=0;
   end;

  end;

 end;

end;

function CheckItems(param:pSceSaveDataDialogParam):Integer;
begin
 if (param^.mode<>SCE_SAVE_DATA_DIALOG_MODE_LIST) and
    (param^.mode<>SCE_SAVE_DATA_DIALOG_MODE_WIZARD_LIST) then
 begin
  Result:=CheckNonListItems(param^.dispType,param^.items);
 end else
 if (p_proc.p_sdk_version < $6000000) then
 begin
  Result:=CheckListItems_old(param^.dispType,param^.items);
 end else
 begin
  Result:=CheckListItems_new(param^.mode,param^.dispType,param^.items);
 end;
end;

//g_error_vtable
function CheckErrorCodeParam(errorCodeParam:pSceSaveDataDialogErrorCodeParam):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (errorCodeParam^.errorCode < 0) then
 if (CheckReserved(errorCodeParam^.reserved,sizeof(errorCodeParam^.reserved))=0) then
 begin
  Result:=0;
 end;
end;

function CheckMsg(msg:pchar):Integer; inline;
begin
 //CheckUtf8???
 Result:=0;
end;

function CheckProgBarParam(progBarParam:pSceSaveDataDialogProgressBarParam):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (CheckBarType(progBarParam^.barType)=0) then
 if (CheckSysMsgType(progBarParam^.sysMsgType)=0) then
 begin

  if (progBarParam^.msg = nil)  or
     (progBarParam^.msg[0] = #0) then
  begin
   if (p_proc.p_sdk_version >= $2000000) and
      (progBarParam^.sysMsgType = SCE_SAVE_DATA_DIALOG_PRGRESS_SYSMSG_TYPE_INVALID) then
   begin
    Exit;
   end;
  end else
  begin
   if (p_proc.p_sdk_version >= $2000000) and
      (progBarParam^.sysMsgType <> SCE_SAVE_DATA_DIALOG_PRGRESS_SYSMSG_TYPE_INVALID) then
   begin
    Exit;
   end;
   //
   if (CheckMsg(progBarParam^.msg)<>0) then
   begin
    Exit;
   end;
  end;

  if (CheckReserved(progBarParam^.reserved,sizeof(progBarParam^.reserved))=0) then
  begin
   Result:=0;
  end;

 end;
end;

//g_progress_vtable
function CheckProgBarMode(param:pSceSaveDataDialogParam):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (CheckProgBarParam(param^.progBarParam)=0) then
 begin

  if (
      (param^.dispType = SCE_SAVE_DATA_DIALOG_TYPE_LOAD  ) or
      (param^.dispType = SCE_SAVE_DATA_DIALOG_TYPE_DELETE)
     ) and
     (
      (param^.items = nil) or
      ( (param^.items <> nil) and (param^.items^.dirNameNum = 0) )
     ) then
  begin
   Exit;
  end;

  Result:=0;

 end;
end;

function CheckSysMsgType(dispType,sysMsgType:Integer):Integer;
begin
 if (sysMsgType < 1) or (15 < sysMsgType) then
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 end else
 if (dispType = SCE_SAVE_DATA_DIALOG_TYPE_SAVE) or
    (
     (sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_OVERWRITE) and
     (sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE) and
     (sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE_CONTINUABLE) and
     (sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_TOTAL_SIZE_EXCEEDED)
    ) then
 begin
   if (
       (sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_RESTORE) or
       (sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE_RESTORE)
      ) and
      (dispType <> SCE_SAVE_DATA_DIALOG_TYPE_SAVE) and
      (dispType <> SCE_SAVE_DATA_DIALOG_TYPE_LOAD) then
   begin
    Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
   end else
   if (
       (sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CORRUPTED_AND_DELETE) or
       (sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CORRUPTED_AND_CREATE) or
       (sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CORRUPTED_AND_RESTORE)
      ) and
      (dispType <> SCE_SAVE_DATA_DIALOG_TYPE_SAVE) and
      (dispType <> SCE_SAVE_DATA_DIALOG_TYPE_LOAD) then
   begin
    Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
   end else
   begin
    Result:=0;
   end;
 end else
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 end;
end;

function CheckSystemMessageParam(dispType:Integer;sysMsgParam:pSceSaveDataDialogSystemMessageParam):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (CheckSysMsgType(dispType,sysMsgParam^.sysMsgType)=0) then
 if (CheckReserved(sysMsgParam^.reserved,sizeof(sysMsgParam^.reserved))=0) then
 begin
  Result:=0;
 end;
end;

//g_system_vtable
function CheckSystemMode(param:pSceSaveDataDialogParam):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (CheckSystemMessageParam(param^.dispType,param^.sysMsgParam)=0) then
 begin

  if (p_proc.p_sdk_version < $2000000) then
  begin
   if (p_proc.p_sdk_version < $1700000) then
   begin
    if (param^.sysMsgParam^.sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NODATA) then
    begin
     if (param^.items <> nil) and (param^.items^.dirNameNum <> 0) then
     begin
      Exit;
     end;
    end else
    if (param^.items = nil) or
       ( (param^.items <> nil) and (param^.items^.dirNameNum = 0) ) then
    begin
     Exit;
    end;
   end else
   if (param^.sysMsgParam^.sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NODATA) and
      (param^.sysMsgParam^.sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE) and
      (param^.sysMsgParam^.sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE_CONTINUABLE) and
      (
       (param^.items = nil) or
       ( (param^.items <> nil) and (param^.items^.dirNameNum = 0) )
      ) then
   begin
    Exit;
   end;
  end else
  if (param^.sysMsgParam^.sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NODATA) and
     (param^.sysMsgParam^.sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE) and
     (param^.sysMsgParam^.sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE_CONTINUABLE) and
     (param^.sysMsgParam^.sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_TOTAL_SIZE_EXCEEDED) then
  begin
   if (param^.sysMsgParam^.sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_RESTORE) or
      (param^.sysMsgParam^.sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CORRUPTED_AND_RESTORE) then
   begin
    if (param^.items = nil) or
       (param^.items^.userId = -1) or
       (param^.items^.dirName = nil) or
       (param^.items^.dirName^.data[0] = #0) or
       (param^.items^.newItem <> nil) then
    begin
     Exit;
    end;
   end else
   if (param^.sysMsgParam^.sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE_RESTORE) then
   begin
     if (param^.items^.userId = -1) or
        (param^.items^.dirName = nil) or
        (param^.items^.dirNameNum = 0) or
        (param^.items^.newItem <> nil) then
     begin
      Exit;
     end;
   end else
   if (param^.items = nil) or
      ( (param^.items^.dirName <> nil) and (param^.items^.dirNameNum = 0) ) or
      ( (param^.items^.dirName = nil) and (param^.items^.dirNameNum <> 0) ) then
   begin
    Exit;
   end
  end;

  if (param^.sysMsgParam^.sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_12) then
  begin
   //sceLncUtilGetAppStatus
  end;

  Result:=0;
 end;

end;

function CheckUserMsgParam_old(userMsgParam:pSceSaveDataDialogUserMessageParam):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (CheckButtonType(userMsgParam^.buttonType)=0) then
 if (userMsgParam^.msg <> nil) then
 if (CheckMsg(userMsgParam^.msg)=0) then
 if (CheckReserved(userMsgParam^.reserved,sizeof(userMsgParam^.reserved))=0) then
 begin
  Result:=0;
 end;
end;

function CheckUserMsgParam_new(userMsgParam:pSceSaveDataDialogUserMessageParam):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (CheckButtonType(userMsgParam^.buttonType)=0) then
 if (userMsgParam^.msgType = SCE_SAVE_DATA_DIALOG_USERMSG_TYPE_NORMAL) or
    (userMsgParam^.msgType = SCE_SAVE_DATA_DIALOG_USERMSG_TYPE_ERROR ) then
 if (userMsgParam^.msg <> nil) then
 if (CheckMsg(userMsgParam^.msg)=0) then
 if (CheckReserved(userMsgParam^.reserved,sizeof(userMsgParam^.reserved))=0) then
 begin
  Result:=0;
 end;
end;

//g_user_vtable
function CheckUserMode(param:pSceSaveDataDialogParam):Integer;
begin
 if (p_proc.p_sdk_version < $1700000) then
 begin
  Result:=CheckUserMsgParam_old(param^.userMsgParam);
 end else
 begin
  Result:=CheckUserMsgParam_new(param^.userMsgParam);
 end;
end;

function _CheckFingerprint(fingerprint:pchar):Integer;
var
 len,i:DWORD;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 len:=strnlen_s(fingerprint,65);
 if (len < 65) then
 begin
  i:=0;
  while (i < len) do
  begin
   if ((fingerprint[i] < 'a') or ('f' < fingerprint[i])) and
      ((fingerprint[i] < '0') or ('9' < fingerprint[i])) then
   begin
    Exit;
   end;
   i:=i+1;
  end;
  //
  Result:=0;
 end;
end;

function CheckFingerprint(fingerprint:pSceSaveDataFingerprint):Integer;
begin
 if (fingerprint=nil) then Exit(0);
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (_CheckFingerprint(@fingerprint^.data)=0) then
 if (CheckReserved(fingerprint^.padding,sizeof(fingerprint^.padding))=0) then
 begin
  Result:=0;
 end;
end;

function CheckWizardParam(wizardParam:pSceSaveDataDialogWizardParam):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (wizardParam<>nil) then
 if (wizardParam^.option<>0) then
 if ((wizardParam^.option and (not $30003))<>0) then
 if ((wizardParam^.option and $ffff) in [0..3]) then
 if (wizardParam^.fingerprint = nil) or
    (CheckFingerprint(wizardParam^.fingerprint)=0) then
 if (wizardParam^.reserved1 = 0) then
 if (CheckReserved(wizardParam^.reserved2,sizeof(wizardParam^.reserved2))=0) then
 begin
  Result:=0;
 end;
end;

function CheckFingerprintItems(WizardParam:pSceSaveDataDialogWizardParam;Items:pSceSaveDataDialogItems):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (WizardParam^.fingerprint = nil) then
 begin
  if (Items <> nil) and (Items^.titleId <> nil) then
  begin
   Exit;
  end;
 end else
 begin
 if (Items = nil) or (Items^.titleId = nil) then
  begin
   Exit;
  end;
 end;
 Result:=0;
end;

//g_wizardconfirm_vtable
function CheckWizardConfirmMode(param:pSceSaveDataDialogParam):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (CheckWizardParam(param^.wizardParam)=0) then
 begin

  if (param^.dispType = SCE_SAVE_DATA_DIALOG_TYPE_DELETE) then
  begin
   if ((param^.wizardParam^.option and $ffff) <> 0) then
   begin
    Exit;
   end;
  end else
  begin
   if ((param^.wizardParam^.option and $ffff) = 0) then
   begin
    Exit;
   end;
  end;

  if ((param^.wizardParam^.option and SCE_SAVE_DATA_DIALOG_WIZARD_OPTION_DISPLAY_NO_DATA) = 0) or
     (param^.dispType = SCE_SAVE_DATA_DIALOG_TYPE_LOAD) or
     (param^.dispType = SCE_SAVE_DATA_DIALOG_TYPE_DELETE) then
  if (param^.optionParam = nil) then
  if (CheckFingerprintItems(param^.wizardParam,param^.items)=0) then
  begin

   if (param^.items = nil) or
      (param^.items^.dirName = nil) or
      (param^.items^.dirNameNum = 0) then
   begin
    Exit;
   end;

   Result:=0;
  end;

 end;

end;

//g_wizardlist_vtable
function CheckWizardListMode(param:pSceSaveDataDialogParam):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (CheckWizardParam(param^.wizardParam)=0) then
 begin

  if (param^.dispType = SCE_SAVE_DATA_DIALOG_TYPE_DELETE) then
  begin
   if (param^.wizardParam^.option <> SCE_SAVE_DATA_DIALOG_WIZARD_OPTION_SELECT_WITH_CONFIRMATION) then
   begin
    Exit;
   end;
  end else
  begin
   if ((param^.wizardParam^.option and $ffff) = 0) then
   begin
    Exit;
   end;
  end;

  if ((param^.wizardParam^.option and SCE_SAVE_DATA_DIALOG_WIZARD_OPTION_DISPLAY_NO_DATA) = 0) then
  if (param^.optionParam = nil) then
  if (CheckFingerprintItems(param^.wizardParam,param^.items)=0) then
  begin
   Result:=0;
  end;

 end;
end;

//////////////////////

procedure CopyBackMode(src:pSceSaveDataDialogParam;var back:Byte);
var
 sysMsgType:DWORD;
 buttonType:DWORD;
begin
  if (p_proc.p_sdk_version < $4500000) then
  begin
   if (src^.optionParam <> nil) then
   begin
    back := src^.optionParam^.back;
   end;
  end else
  begin
   back := SCE_SAVE_DATA_DIALOG_OPTION_BACK_ENABLE;
   case (src^.mode) of
    SCE_SAVE_DATA_DIALOG_MODE_LIST,
    SCE_SAVE_DATA_DIALOG_MODE_WIZARD_LIST:
      if (src^.optionParam <> nil) then
      begin
       back := src^.optionParam^.back;
      end;
    SCE_SAVE_DATA_DIALOG_MODE_USER_MSG :
      begin
       buttonType := src^.userMsgParam^.buttonType;
       if (SCE_SAVE_DATA_DIALOG_BUTTON_TYPE_YESNO < buttonType) then
       begin
        if (buttonType = SCE_SAVE_DATA_DIALOG_BUTTON_TYPE_NONE) then
        begin
         if (src^.optionParam = nil) then
         begin
          Exit;
         end;
         back := src^.optionParam^.back;
         Exit;
        end;
        if (buttonType <> SCE_SAVE_DATA_DIALOG_BUTTON_TYPE_OKCANCEL) then
        begin
         Exit;
        end;
       end;
       back := SCE_SAVE_DATA_DIALOG_OPTION_BACK_ENABLE;
      end;
    SCE_SAVE_DATA_DIALOG_MODE_SYSTEM_MSG:
      begin
       sysMsgType := src^.sysMsgParam^.sysMsgType;
       if (3 < DWORD(sysMsgType-1)) then
       begin
        if (sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_PROGRESS) then
        begin
         if (src^.optionParam <> nil) then
         begin
          back := src^.optionParam^.back;
          Exit;
         end;
         back := SCE_SAVE_DATA_DIALOG_OPTION_BACK_DISABLE;
         Exit;
        end;
        if (2 < DWORD(sysMsgType-6)) and
           (sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_12) and
           (sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_TOTAL_SIZE_EXCEEDED) and
           (sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE_RESTORE) then
        begin
         if (src^.optionParam = nil) then
         begin
          Exit;
         end;
         back := src^.optionParam^.back;
         Exit;
        end;
       end;
       back := SCE_SAVE_DATA_DIALOG_OPTION_BACK_ENABLE;
      end;
    SCE_SAVE_DATA_DIALOG_MODE_ERROR_CODE:
      back := SCE_SAVE_DATA_DIALOG_OPTION_BACK_ENABLE;
    SCE_SAVE_DATA_DIALOG_MODE_PROGRESS_BAR:
      back := SCE_SAVE_DATA_DIALOG_OPTION_BACK_DISABLE;
    SCE_SAVE_DATA_DIALOG_MODE_WIZARD_CONFIRM:
      back := SCE_SAVE_DATA_DIALOG_OPTION_BACK_ENABLE;
    end;
  end
end;

procedure CopyBase(src:pSceSaveDataDialogParam;var dst:TSaveDialogOpen);
begin
 dst.mode     := src^.mode;
 dst.dispType := src^.dispType;
 if (src^.animParam <> nil) then
 begin
  dst.userOK     := src^.animParam^.userOK;
  dst.userCancel := src^.animParam^.userCancel;
 end;
 dst.userData := src^.userData;
 CopyBackMode(src,dst.back);
end;

procedure CopyNewItem(src:pSceSaveDataDialogNewItem;var dst:TSaveDialogNewItem);
begin
 if (src <> nil) then
 begin
  if (src^.title <> nil) then
  begin
   strncpy_s(@dst.title,src^.title,127);
   dst.title[127]:=#0;
  end;
  if ((src^.iconBuf <> nil) and (src^.iconSize <> 0)) then
  begin
   Move(src^.iconBuf^,dst.iconBuf,src^.iconSize);
   dst.iconSize:=src^.iconSize;
  end;
 end;
end;

procedure CopyDirNames(src:pSceSaveDataDialogParam;var dst:TSaveDialogOpen);
var
 items:pSceSaveDataDialogItems;
begin
 FillChar(dst.titleId        ,sizeof(dst.titleId),0);
 FillChar(dst.dirNames       ,sizeof(dst.dirNames),0);
 FillChar(dst.new_item       ,sizeof(dst.new_item),0);
 FillChar(dst.focusPosDirName,sizeof(dst.focusPosDirName),0);
 //
 items:=src^.items;
 dst.userId:=items^.userId;
 //
 if (items^.titleId <> nil) then
 begin
  dst.titleId:=items^.titleId^.data;
 end;
 //
 if (items^.dirName <> nil) then
 begin
  if (src^.mode = SCE_SAVE_DATA_DIALOG_MODE_LIST) or
     (src^.mode = SCE_SAVE_DATA_DIALOG_MODE_WIZARD_LIST) then
  begin
   Move(items^.dirName^,dst.dirNames,items^.dirNameNum*sizeof(SceSaveDataDirName));
   dst.dirNameNum := items^.dirNameNum;
  end else
  if (src^.sysMsgParam = nil) or
     (
      (src^.sysMsgParam^.sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NODATA) and
      (src^.sysMsgParam^.sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE) and
      (src^.sysMsgParam^.sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE_CONTINUABLE) and
      (src^.sysMsgParam^.sysMsgType <> SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_TOTAL_SIZE_EXCEEDED)
     ) then
  begin
   dst.dirNames[0]:=items^.dirName^;
   dst.dirNameNum := 1;
  end else
  begin
   dst.dirNameNum := 0;
  end;
 end;
 //
 if (src^.dispType = SCE_SAVE_DATA_DIALOG_TYPE_SAVE) then
 begin
  if (src^.mode = SCE_SAVE_DATA_DIALOG_MODE_SYSTEM_MSG) then
  begin
   if (p_proc.p_sdk_version < $2000000) then
   begin
    dst.is_new:=0;
   end else
   if (items^.dirName = nil) and
      (items^.dirNameNum = 0) and
      (
       (src^.sysMsgParam^.sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CONFIRM) or
       (src^.sysMsgParam^.sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE) or
       (src^.sysMsgParam^.sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_PROGRESS) or
       (src^.sysMsgParam^.sysMsgType = SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE_CONTINUABLE)
      ) then
   begin
    dst.is_new:=1;
   end else
   begin
    dst.is_new:=0;
   end;
  end else
  if (items^.dirNameNum = 0) then
  begin
   dst.is_new:=1;
  end else
  if (items^.dirNameNum <> 0) then
  begin
   if (items^.newItem = nil) then
   begin
    dst.is_new:=0;
   end else
   begin
    dst.is_new:=1;
   end
  end;
  if ((dst.is_new and 1) = 1) then
  begin
   CopyNewItem(items^.newItem,dst.new_item);
  end;
 end else
 begin
  dst.is_new := 0;
 end;
 //
 dst.focusPos := items^.focusPos;
 if (items^.focusPosDirName <> nil) then
 begin
  dst.focusPosDirName:=items^.focusPosDirName^;
 end;
 dst.itemStyle := items^.itemStyle;
end;

procedure CopyProgBarParam(src:pSceSaveDataDialogProgressBarParam;var dst:TSaveDialogOpen);
begin
 if (p_proc.p_sdk_version >= $4500000) then
 begin
  FillChar(dst.bar_msg,sizeof(dst.bar_msg),0);
 end;
 if (src^.msg <> nil) then
 begin
  Move(src^.msg^,dst.bar_msg,255);
  dst.bar_msg[255] := #0;
 end;
 dst.bar_sysMsgType := src^.sysMsgType;
end;

procedure CopySystemMessage(src:pSceSaveDataDialogSystemMessageParam;var dst:TSaveDialogOpen);
begin
 dst.sys_sysMsgType:=src^.sysMsgType;
 dst.sys_value     :=src^.value;
end;

procedure CopyErrorCode(src:pSceSaveDataDialogErrorCodeParam;var dst:TSaveDialogOpen);
begin
 dst.errorCode:=src^.errorCode;
end;

procedure CopyUserMsgParam(src:pSceSaveDataDialogUserMessageParam;var dst:TSaveDialogOpen);
begin
 dst.buttonType := src^.buttonType;
 if (p_proc.p_sdk_version < $1700000) then
 begin
  dst.msgType := SCE_SAVE_DATA_DIALOG_USERMSG_TYPE_NORMAL;
 end else
 begin
  dst.msgType := src^.msgType;
 end;
 if (src^.msg <> nil) then
 begin
  Move(src^.msg^,dst.user_msg,255);
  dst.user_msg[255] := #0;
 end;
end;

procedure CopyWizardParam(src:pSceSaveDataDialogWizardParam;var dst:TSaveDialogOpen);
begin
 dst.option := src^.option;
 if (src^.fingerprint <> nil) then
 begin
  strncpy_s(@dst.fingerprint,@src^.fingerprint^.data,64);
  dst.fingerprint[64] := #0;
 end;
end;

//////////////////////

function ps4_sceSaveDataDialogOpen(param:pSceSaveDataDialogParam):Integer;
begin
 Result:=0;

 if (param=nil) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_ARG_NULL);
 end;

 if CheckBaseParam(@param^.baseParam)<>0 then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 if (param^.size<>$98) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 Writeln('sceSaveDataDialogOpen');

 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_SaveDialog_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
   if (CheckSaveDataParam(param)=0) then
   if (CheckItems(param)=0) then
   begin
    case param^.mode of
     SCE_SAVE_DATA_DIALOG_MODE_LIST          :Result:=0;
     SCE_SAVE_DATA_DIALOG_MODE_USER_MSG      :Result:=CheckUserMode(param);
     SCE_SAVE_DATA_DIALOG_MODE_SYSTEM_MSG    :Result:=CheckSystemMode(param);
     SCE_SAVE_DATA_DIALOG_MODE_ERROR_CODE    :Result:=CheckErrorCodeParam(param^.errorCodeParam);
     SCE_SAVE_DATA_DIALOG_MODE_PROGRESS_BAR  :Result:=CheckProgBarMode(param);
     SCE_SAVE_DATA_DIALOG_MODE_WIZARD_LIST   :Result:=CheckWizardListMode(param);
     SCE_SAVE_DATA_DIALOG_MODE_WIZARD_CONFIRM:Result:=CheckWizardConfirmMode(param);
     else
       Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
    end;
   end;

   //copy
   if (Result=0) then
   begin
    //
    if (p_proc.p_sdk_version >= $6000000) then
    begin
     g_client.rate:=0;
    end;
    //
    CopyBase    (param,g_client.data);
    CopyDirNames(param,g_client.data);

    case param^.mode of
     SCE_SAVE_DATA_DIALOG_MODE_USER_MSG:
        CopyUserMsgParam(param^.userMsgParam,g_client.data);
     SCE_SAVE_DATA_DIALOG_MODE_SYSTEM_MSG:
        CopySystemMessage(param^.sysMsgParam,g_client.data);
     SCE_SAVE_DATA_DIALOG_MODE_ERROR_CODE:
       CopyErrorCode(param^.errorCodeParam,g_client.data);
     SCE_SAVE_DATA_DIALOG_MODE_PROGRESS_BAR:
       CopyProgBarParam(param^.progBarParam,g_client.data);
     SCE_SAVE_DATA_DIALOG_MODE_WIZARD_LIST,
     SCE_SAVE_DATA_DIALOG_MODE_WIZARD_CONFIRM:
       CopyWizardParam(param^.wizardParam,g_client.data);
     else;
    end;

    //reTFEla4NQw
    Result:=g_client.Open('SAVE_DIALOG_OPEN',@g_client.data,SizeOf(g_client.data));
   end;

  end;

 mtx_unlock(g_SaveDialog_mtx);

 Result:=0;
end;

function CheckCloseParam(closeParam:pSceSaveDataDialogCloseParam):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
 if (closeParam<>nil) then
 if CheckAnim(closeParam^.anim)=0 then
 if CheckReserved(closeParam^.reserved,sizeof(closeParam^.reserved))=0 then
 begin
  Result:=0;
 end;
end;

function ps4_sceSaveDataDialogClose(closeParam:pSceSaveDataDialogCloseParam):Integer;
var
 anim:Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 Writeln('sceSaveDataDialogClose');

 mtx_lock(g_SaveDialog_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_ARG_NULL;
   if (closeParam<>nil) then
   begin
    Result:=SCE_COMMON_DIALOG_ERROR_NOT_RUNNING;
    if (not g_client.isInitializedStatus) then
    if (not g_client.isFinish) then
    begin
     Result:=CheckCloseParam(closeParam);
     if (Result=0) then
     begin
      anim:=closeParam^.anim;
      Result:=g_client.Close(@anim,sizeof(anim));
     end;
    end;
   end;
  end;

 mtx_unlock(g_SaveDialog_mtx);
 //
end;

function ps4_sceSaveDataDialogTerminate():Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 Writeln('sceSaveDataDialogTerminate');

 mtx_lock(g_SaveDialog_mtx);

  if (g_client<>nil) then
  begin
   g_client.Terminate;
   g_client:=nil;
   Result:=0;
  end;

 mtx_unlock(g_SaveDialog_mtx);
 //
end;

function ps4_sceSaveDataDialogIsReadyToDisplay:Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;

 mtx_lock(g_SaveDialog_mtx);

  if (g_client<>nil) then
  begin
   if g_client.isFinish then
   begin
    Result:=SCE_COMMON_DIALOG_ERROR_INVALID_STATE;
   end else
   begin
    Result:=1; //IsReadyToDisplay
   end;
  end;

 mtx_unlock(g_SaveDialog_mtx);
 //
end;

function ps4_sceSaveDataDialogUpdateStatus():Integer;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;

 mtx_lock(g_SaveDialog_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_STATUS_INITIALIZED;
   if (not g_client.isInitializedStatus) then
   begin
    Result:=SCE_COMMON_DIALOG_STATUS_FINISHED;
    if (not g_client.isFinish) then
    begin
     g_client.updateState;
     if (g_client.isFinish) then
     begin
      Result:=SCE_COMMON_DIALOG_STATUS_FINISHED;
     end else
     begin
      Result:=SCE_COMMON_DIALOG_STATUS_RUNNING;
     end;
    end;
   end;
  end;

 mtx_unlock(g_SaveDialog_mtx);
 //
end;

function ps4_sceSaveDataDialogGetStatus():Integer;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;

 mtx_lock(g_SaveDialog_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_STATUS_INITIALIZED;
   if (not g_client.isInitializedStatus) then
   begin
    if (g_client.isFinish) then
    begin
     Result:=SCE_COMMON_DIALOG_STATUS_FINISHED;
    end else
    begin
     Result:=SCE_COMMON_DIALOG_STATUS_RUNNING;
    end;
   end;
  end;

 mtx_unlock(g_SaveDialog_mtx);
 //
end;

procedure CopyResult(src:TSaveDialogClient;pResult:pSceSaveDataDialogResult);
begin
 pResult^.mode     := src.data.mode;
 pResult^.result   := src.rzdata.resultId;
 pResult^.buttonId := src.rzdata.buttonId;
 //
 if (p_proc.p_sdk_version >= $4500000) and
    ( (src.rzdata.resultId = 1) or (src.rzdata.resultId < 0) ) then
 begin
  pResult^.buttonId := SCE_SAVE_DATA_DIALOG_BUTTON_ID_INVALID;
 end;
 //
 if (pResult^.dirName <> nil) then
 begin
  pResult^.dirName^ := src.rzdata.dirName;
 end;
 //
 if (pResult^.param <> nil) then
 begin
  Move(src.rzdata.param,pResult^.param^,sizeof(SceSaveDataParam));
 end;
 //
 pResult^.userData := src.data.userData;
end;

function ps4_sceSaveDataDialogGetResult(pResult:pSceSaveDataDialogResult):Integer;
begin
 if (pResult=nil) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_ARG_NULL);
 end;

 if CheckReserved(pResult^.reserved,SizeOf(pResult^.reserved))<>0 then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_SaveDialog_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_FINISHED;
   if (g_client.isFinish) then
   begin
    g_client.rzdata:=Default(TSaveDialogResult);
    g_client.getFinishData(@g_client.rzdata,sizeof(g_client.rzdata));

    CopyResult(g_client,pResult);

    Result:=g_client.rzdata.resultId;
   end;
  end;

 mtx_unlock(g_SaveDialog_mtx);
 //
end;

function ps4_sceSaveDataDialogProgressBarSetValue(target:Integer;rate:DWORD):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_SaveDialog_mtx);

  if (g_client<>nil) then
  if (not g_client.isInitializedStatus) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_RUNNING;
   if (not g_client.isFinish) then
   begin
    //
    if (g_client.data.mode<>SCE_SAVE_DATA_DIALOG_MODE_PROGRESS_BAR) then
    begin
     Result:=SCE_COMMON_DIALOG_ERROR_NOT_SUPPORTED;
    end else
    if (target<>0) then
    begin
     Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
    end else
    begin
     g_client.rate:=rate;
     Result:=g_client.SetValue(@g_client.rate,SizeOf(g_client.rate));
    end;
    //
   end;
  end;

 mtx_unlock(g_SaveDialog_mtx);
 //
end;

function ps4_sceSaveDataDialogProgressBarInc(target:Integer;delta:DWORD):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_SaveDialog_mtx);

  if (g_client<>nil) then
  if (not g_client.isInitializedStatus) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_RUNNING;
   if (not g_client.isFinish) then
   begin
    //
    if (g_client.data.mode<>SCE_SAVE_DATA_DIALOG_MODE_PROGRESS_BAR) then
    begin
     Result:=SCE_COMMON_DIALOG_ERROR_NOT_SUPPORTED;
    end else
    if (target<>0) then
    begin
     Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
    end else
    begin
     g_client.rate:=g_client.rate + delta;
     Result:=g_client.SetValue(@g_client.rate,SizeOf(g_client.rate));
    end;
    //
   end;
  end;

 mtx_unlock(g_SaveDialog_mtx);
 //
end;

function dt_fini(args:QWORD;argp,addr:Pointer):Integer;
begin
 ps4_sceSaveDataDialogTerminate();
 Result:=0;
end;

function Load_libSceSaveDataDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceSaveDataDialog');

 Result^.fini_proc_addr.native:=@dt_fini;

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
 lib.set_proc($57FB847852804495,@ps4_sceSaveDataDialogProgressBarInc);

 mtx_init(g_SaveDialog_mtx,'g_SaveDialog_mtx');
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceSaveDataDialog.prx',@Load_libSceSaveDataDialog);

end.

