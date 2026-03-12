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
   if (ord(titleId[i]) < ord('A')) or(ord(titleId[i]) > ord('Z')) then
   begin
    Exit;
   end;
  for i:=4 to 8 do
   if (ord(titleId[i]) < ord('0')) or(ord(titleId[i]) > ord('9')) then
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
     SCE_SAVE_DATA_DIALOG_MODE_LIST          :Assert(False);
     SCE_SAVE_DATA_DIALOG_MODE_USER_MSG      :Assert(False);
     SCE_SAVE_DATA_DIALOG_MODE_SYSTEM_MSG    :Assert(False);
     SCE_SAVE_DATA_DIALOG_MODE_ERROR_CODE    :Assert(False);
     SCE_SAVE_DATA_DIALOG_MODE_PROGRESS_BAR  :Assert(False);
     SCE_SAVE_DATA_DIALOG_MODE_WIZARD_LIST   :Assert(False);
     SCE_SAVE_DATA_DIALOG_MODE_WIZARD_CONFIRM:Assert(False);
     else
       Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
    end;

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
 Result:=1;
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

function ps4_sceSaveDataDialogGetResult(pResult:pSceSaveDataDialogResult):Integer;
begin
 //Writeln('sceSaveDataDialogGetResult');
 Result:=0;
end;

function ps4_sceSaveDataDialogProgressBarSetValue(target:Integer;rate:DWORD):Integer;
begin
 Writeln('sceSaveDataDialogProgressBarSetValue:',rate);
 Result:=0;
end;

function ps4_sceSaveDataDialogProgressBarInc(target:Integer;delta:DWORD):Integer;
begin
 Writeln('sceSaveDataDialogProgressBarInc:',delta);
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
 lib.set_proc($57FB847852804495,@ps4_sceSaveDataDialogProgressBarInc);

 mtx_init(g_SaveDialog_mtx,'g_SaveDialog_mtx');
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceSaveDataDialog.prx',@Load_libSceSaveDataDialog);

end.

