unit ps4_libSceMsgDialog;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_mtx,
 subr_dynlib,
 ps4_libSceCommonDialog;

const
 //SceMsgDialogMode
 SCE_MSG_DIALOG_MODE_INVALID     =0;
 SCE_MSG_DIALOG_MODE_USER_MSG    =1;
 SCE_MSG_DIALOG_MODE_PROGRESS_BAR=2;
 SCE_MSG_DIALOG_MODE_SYSTEM_MSG  =3;

 //SceMsgDialogButtonType
 SCE_MSG_DIALOG_BUTTON_TYPE_OK                    =0;
 SCE_MSG_DIALOG_BUTTON_TYPE_YESNO                 =1;
 SCE_MSG_DIALOG_BUTTON_TYPE_NONE                  =2;
 SCE_MSG_DIALOG_BUTTON_TYPE_OK_CANCEL             =3;
 SCE_MSG_DIALOG_BUTTON_TYPE_WAIT                  =5;
 SCE_MSG_DIALOG_BUTTON_TYPE_WAIT_CANCEL           =6;
 SCE_MSG_DIALOG_BUTTON_TYPE_YESNO_FOCUS_NO        =7;
 SCE_MSG_DIALOG_BUTTON_TYPE_OK_CANCEL_FOCUS_CANCEL=8;
 SCE_MSG_DIALOG_BUTTON_TYPE_2BUTTONS              =9;

 //SceMsgDialogButtonId
 SCE_MSG_DIALOG_BUTTON_ID_INVALID=0;
 SCE_MSG_DIALOG_BUTTON_ID_OK     =1;
 SCE_MSG_DIALOG_BUTTON_ID_YES    =1;
 SCE_MSG_DIALOG_BUTTON_ID_NO     =2;
 SCE_MSG_DIALOG_BUTTON_ID_BUTTON1=1;
 SCE_MSG_DIALOG_BUTTON_ID_BUTTON2=2;

 //SceMsgDialogProgressBarType
 SCE_MSG_DIALOG_PROGRESSBAR_TYPE_PERCENTAGE       =0;
 SCE_MSG_DIALOG_PROGRESSBAR_TYPE_PERCENTAGE_CANCEL=1;

 SCE_MSG_DIALOG_PROGRESSBAR_TARGET_BAR_DEFAULT=0;

 //SceMsgDialogSystemMessageType
 SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_EMPTY_STORE                            =0;
 SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_PSN_CHAT_RESTRICTION                   =1;
 SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_PSN_UGC_RESTRICTION                    =2;
 SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_WARNING_SWITCH_TO_SIMULVIEW            =3;
 SCE_MSG_DIALOG_SYSMSG_TYPE_CAMERA_NOT_CONNECTED                       =4;
 SCE_MSG_DIALOG_SYSMSG_TYPE_WARNING_PROFILE_PICTURE_AND_NAME_NOT_SHARED=5;
 SCE_MSG_DIALOG_SYSMSG_TYPE_PSN_COMMUNICATION_RESTRICTION              =6;

 SCE_MSG_DIALOG_BUTTON_MSG_SIZE=64;

{$CALLING default}

type
 TMsgDialogClient=class(TCommonDialogClient)
  //
 end;

implementation

var
 g_msg_mtx:mtx;
 g_client :TMsgDialogClient=nil;

{$CALLING SysV_ABI_CDecl}

function ps4_sceMsgDialogInitialize():Integer;
var
 client:TMsgDialogClient;
begin
 Result:=0;
 Writeln('sceMsgDialogInitialize');

 mtx_lock(g_msg_mtx);

  Result:=SCE_COMMON_DIALOG_ERROR_ALREADY_INITIALIZED;
  if (g_client=nil) then
  begin

   Result:=SCE_COMMON_DIALOG_ERROR_BUSY;
   if (not ps4_sceCommonDialogIsUsed) then
   begin
    client:=TMsgDialogClient.Create;

    Result:=NewClient(client);

    if (Result=0) then
    begin
     Result:=client.launchCmnDialog();
    end;

    if (Result<>0) then
    begin
     client.Free;
    end else
    begin
     //
     g_client:=client;
     //
    end;

   end;

  end;

 mtx_unlock(g_msg_mtx);
end;

type
 pSceMsgDialogButtonsParam=^SceMsgDialogButtonsParam;
 SceMsgDialogButtonsParam=packed record
  msg1,msg2:Pchar;
  reserved:array[0..31] of Byte;
 end;

 pSceMsgDialogUserMessageParam=^SceMsgDialogUserMessageParam;
 SceMsgDialogUserMessageParam=packed record
  buttonType  :Integer; //SceMsgDialogButtonType
  _align      :Integer;
  msg         :PChar;
  buttonsParam:pSceMsgDialogButtonsParam;
  reserved    :array[0..23] of Byte;
 end;

 pSceMsgDialogProgressBarParam=^SceMsgDialogProgressBarParam;
 SceMsgDialogProgressBarParam=packed record
  barType :Integer; //SceMsgDialogProgressBarType
  _align  :Integer;
  msg     :PChar;
  reserved:array[0..63] of Byte;
 end;

 pSceMsgDialogSystemMessageParam=^SceMsgDialogSystemMessageParam;
 SceMsgDialogSystemMessageParam=packed record
  sysMsgType:Integer; //SceMsgDialogSystemMessageType
  reserved  :array[0..31] of Byte;
 end;

 pSceMsgDialogParam=^SceMsgDialogParam;
 SceMsgDialogParam=packed record
  baseParam   :SceCommonDialogBaseParam;
  size        :QWORD;
  mode        :Integer; //SceMsgDialogMode
  _align1     :Integer;
  userMsgParam:pSceMsgDialogUserMessageParam;
  progBarParam:pSceMsgDialogProgressBarParam;
  sysMsgParam :pSceMsgDialogSystemMessageParam;
  userId      :Integer; //SceUserServiceUserId
  reserved    :array[0..39] of Byte;
  _align2     :Integer;
 end;

function ps4_sceMsgDialogOpen(param:pSceMsgDialogParam):Integer;
begin
 if (param=nil) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_ARG_NULL);
 end;

 if CheckBaseParam(@param^.baseParam)<>0 then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 if (g_client=nil) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED);
 end;

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

 //status_msg_dialog:=SCE_COMMON_DIALOG_STATUS_FINISHED;

 Result:=0;
end;

function ps4_sceMsgDialogClose():Integer;
begin
 Writeln('sceMsgDialogClose');
 //status_msg_dialog:=SCE_COMMON_DIALOG_STATUS_FINISHED;
 Result:=0;
end;

function ps4_sceMsgDialogUpdateStatus():Integer;
begin
 //Result:=status_msg_dialog;
end;

function ps4_sceMsgDialogGetStatus():Integer;
begin
 //Result:=status_msg_dialog;
end;

type
 pSceMsgDialogResult=^SceMsgDialogResult;
 SceMsgDialogResult=packed record
  mode    :Integer; //SceMsgDialogMode
  result  :Integer;
  buttonId:Integer; //SceMsgDialogButtonId
  reserved:array[0..31] of Byte;
 end;

function ps4_sceMsgDialogGetResult(pResult:pSceMsgDialogResult):Integer;
begin
 //Writeln('sceMsgDialogGetResult');
 if (pResult<>nil) then
 begin
  pResult^.result:=0;
  pResult^.buttonId:=1;
 end;
 Result:=0;
end;

function ps4_sceMsgDialogTerminate():Integer;
begin
 Writeln('sceMsgDialogTerminate');
 //status_msg_dialog:=SCE_COMMON_DIALOG_STATUS_NONE;
 Result:=0;
end;

//

function Load_libSceMsgDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceMsgDialog');

 lib:=Result^.add_lib('libSceMsgDialog');
 lib.set_proc($943AB1698D546C4A,@ps4_sceMsgDialogInitialize);
 lib.set_proc($6F4E878740CF11A1,@ps4_sceMsgDialogOpen);
 lib.set_proc($1D3ADC0CA9452AE3,@ps4_sceMsgDialogClose);
 lib.set_proc($E9F202DD72ADDA4D,@ps4_sceMsgDialogUpdateStatus);
 lib.set_proc($096556EFC41CDDF2,@ps4_sceMsgDialogGetStatus);
 lib.set_proc($2EBF28BC71FD97A0,@ps4_sceMsgDialogGetResult);
 lib.set_proc($78FC3F92A6667A5A,@ps4_sceMsgDialogTerminate);

 mtx_init(g_msg_mtx,'g_msg_mtx');
end;

var
 stub:t_int_file;

initialization
RegisteredInternalFile(stub,'libSceMsgDialog.prx',@Load_libSceMsgDialog);

end.

