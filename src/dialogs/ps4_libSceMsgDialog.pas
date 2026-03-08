unit ps4_libSceMsgDialog;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 kern_mtx,
 subr_dynlib,
 kern_proc,
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
 TMsgDialogOpen=record
  mode      :Integer;                //SceMsgDialogMode
  buttonType:Integer;                //SceMsgDialogButtonType
  barType   :Integer;                //SceMsgDialogProgressBarType
  sysMsgType:Integer;                //SceMsgDialogSystemMessageType
  userId    :Integer;                //SceUserServiceUserId
  msg       :array[0..8191] of Char; //char[8192]
  msg1      :array[0..63]   of Char; //char[64]
  msg2      :array[0..63]   of Char; //char[64]
 end;

 {
 TMsgDialogOpen=class(TSerializeObject)
  Fmode      :Integer;       //SceMsgDialogMode
  FbuttonType:Integer;       //SceMsgDialogButtonType
  FbarType   :Integer;       //SceMsgDialogProgressBarType
  FsysMsgType:Integer;       //SceMsgDialogSystemMessageType
  FuserId    :Integer;       //SceUserServiceUserId
  Fmsg       :RawByteString; //char[8192]
  Fmsg1      :RawByteString; //char[64]
  Fmsg2      :RawByteString; //char[64]
 published
  property mode      :Integer       read Fmode       write Fmode      ;
  property buttonType:Integer       read FbuttonType write FbuttonType;
  property barType   :Integer       read FbarType    write FbarType   ;
  property sysMsgType:Integer       read FsysMsgType write FsysMsgType;
  property userId    :Integer       read FuserId     write FuserId    ;
  property msg       :RawByteString read Fmsg        write Fmsg       ;
  property msg1      :RawByteString read Fmsg1       write Fmsg1      ;
  property msg2      :RawByteString read Fmsg2       write Fmsg2      ;
 end;
 }

 TMsgDialogClient=class(TCommonDialogClient)
  data:TMsgDialogOpen;
 end;

implementation

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

 pSceMsgDialogResult=^SceMsgDialogResult;
 SceMsgDialogResult=packed record
  mode    :Integer; //SceMsgDialogMode
  result  :Integer;
  buttonId:Integer; //SceMsgDialogButtonId
  reserved:array[0..31] of Byte;
 end;

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

    Result:=client.launchCmnDialog();

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

function strnlen_s(s:PChar;maxlen:ptrint):ptrint;
var
 i:size_t;
begin
 if (s=nil) then Exit(0);
 i:=0;
 if (maxlen<>0) then
 begin
  repeat
   if (s[i]=#0) then Exit(i);
   Inc(i);
  until (maxlen = i);
 end;
 Exit(maxlen);
end;

function CheckButtonsParam(buttonsParam:pSceMsgDialogButtonsParam):Boolean;
var
 len,i:DWORD;
begin
 Result:=True;

 if (buttonsParam=nil) then
 begin
  Exit(False);
 end;

 if (buttonsParam^.msg1=nil) then
 begin
  Exit(False);
 end;

 len:=strnlen_s(buttonsParam^.msg1,64);

 if (len >= 64) then
 begin
  Exit(False);
 end;

 i:=0;
 repeat
  case buttonsParam^.msg1[i] of
    #0:Break;
   #10:Exit(False);
   #13:Exit(False);
   else;
  end;
  Inc(i);
 until (i = 64);

 if (buttonsParam^.msg2=nil) then
 begin
  Exit(False);
 end;

 len:=strnlen_s(buttonsParam^.msg2,64);

 if (len >= 64) then
 begin
  Exit(False);
 end;

 i:=0;
 repeat
  case buttonsParam^.msg2[i] of
    #0:Break;
   #10:Exit(False);
   #13:Exit(False);
   else;
  end;
  Inc(i);
 until (i = 64);
 //
end;

function CheckUserMsgParam(userMsgParam:pSceMsgDialogUserMessageParam):Boolean;
var
 maxlen,len,i:DWORD;
begin
 Result:=True;

 if (userMsgParam=nil) then
 begin
  Exit(False);
 end;

 if (DWORD(userMsgParam^.buttonType)>9) then
 begin
  Exit(False);
 end;

 if (p_proc.p_sdk_version < $1500000) then
 begin
  maxlen:=$200;
 end else
 begin
  maxlen:=$2000;
 end;

 len:=strnlen_s(userMsgParam^.msg,maxlen);

 if (len >= maxlen) then
 begin
  Exit(False);
 end;

 for i:=0 to High(SceMsgDialogUserMessageParam.reserved) do
 if (userMsgParam^.reserved[i]<>0) then
 begin
  Exit(False);
 end;

 if (userMsgParam^.buttonType=SCE_MSG_DIALOG_BUTTON_TYPE_2BUTTONS) then
 begin
  Result:=CheckButtonsParam(userMsgParam^.buttonsParam);
 end else
 begin
  Result:=(userMsgParam^.buttonsParam=nil);
 end;
 //
end;

function CheckProgBarParam(progBarParam:pSceMsgDialogProgressBarParam):Boolean;
var
 len,i:DWORD;
begin
 Result:=True;

 if (progBarParam=nil) then
 begin
  Exit(False);
 end;

 if (DWORD(progBarParam^.barType)>1) then
 begin
  Exit(False);
 end;

 if (p_proc.p_sdk_version >= $1500000) then
 begin
  len:=strnlen_s(progBarParam^.msg,$2000);
  if (len >= $2000) then
  begin
   Exit(False);
  end;
 end;

 for i:=0 to High(SceMsgDialogProgressBarParam.reserved) do
 if (progBarParam^.reserved[i]<>0) then
 begin
  Exit(False);
 end;
 //
end;

function CheckSystemMessageParam(sysMsgParam:pSceMsgDialogSystemMessageParam;userId:Integer):Boolean;
begin
 Result:=True;

 if (sysMsgParam=nil) then
 begin
  Exit(False);
 end;

 if (DWORD(sysMsgParam^.sysMsgType)>=6) then
 begin
  Exit(False);
 end;

 case sysMsgParam^.sysMsgType of
  SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_PSN_CHAT_RESTRICTION,
  SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_PSN_UGC_RESTRICTION,
  SCE_MSG_DIALOG_SYSMSG_TYPE_WARNING_PROFILE_PICTURE_AND_NAME_NOT_SHARED,
  SCE_MSG_DIALOG_SYSMSG_TYPE_PSN_COMMUNICATION_RESTRICTION:
   begin
    //sceUserServiceIsLoggedIn(param->userId)
   end;

  else;
 end;

 //
end;

function ps4_sceMsgDialogOpen(param:pSceMsgDialogParam):Integer;
var
 maxlen,i:DWORD;
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

 if (param^.size<>$88) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 case param^.mode of
   SCE_MSG_DIALOG_MODE_USER_MSG:
     begin
      //
      if (param^.sysMsgParam<>nil) or (param^.progBarParam<>nil) then
      begin
       Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
      end;

      if not CheckUserMsgParam(param^.userMsgParam) then
      begin
       Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
      end;
      //
     end;
   SCE_MSG_DIALOG_MODE_PROGRESS_BAR:
     begin
      //
      if (param^.userMsgParam<>nil) or (param^.sysMsgParam<>nil) then
      begin
       Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
      end;

      if not CheckProgBarParam(param^.progBarParam) then
      begin
       Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
      end;
      //
     end;
   SCE_MSG_DIALOG_MODE_SYSTEM_MSG:
     begin
      //
      if (param^.userMsgParam<>nil) or (param^.progBarParam<>nil) then
      begin
       Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
      end;

      if not CheckSystemMessageParam(param^.sysMsgParam,param^.userId) then
      begin
       Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
      end;
      //
     end;
   else
    Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 for i:=0 to High(SceMsgDialogParam.reserved) do
 if (param^.reserved[i]<>0) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 Writeln('sceMsgDialogOpen');

 mtx_lock(g_msg_mtx);

  if (g_client<>nil) then
  begin

   g_client.data.mode  :=param^.mode;
   g_client.data.userId:=param^.userId;

   case g_client.data.mode of
     SCE_MSG_DIALOG_MODE_USER_MSG:
       begin
        //
        g_client.data.buttonType:=param^.userMsgParam^.buttonType;
        //
        if (p_proc.p_sdk_version < $1500000) then
        begin
         maxlen:=$1ff;
        end else
        begin
         maxlen:=$1fff;
        end;
        StrLCopy(g_client.data.msg,param^.userMsgParam^.msg,maxlen);
        //
        if (g_client.data.buttonType=SCE_MSG_DIALOG_BUTTON_TYPE_2BUTTONS) then
        begin
         StrLCopy(g_client.data.msg1,param^.userMsgParam^.buttonsParam^.msg1,63);
         StrLCopy(g_client.data.msg2,param^.userMsgParam^.buttonsParam^.msg2,63);
        end;
        //
       end;
     SCE_MSG_DIALOG_MODE_PROGRESS_BAR:
       begin
        //
        g_client.data.barType:=param^.progBarParam^.barType;
        //
        if (p_proc.p_sdk_version < $1500000) then
        begin
         maxlen:=$1ff;
        end else
        begin
         maxlen:=$1fff;
        end;
        StrLCopy(g_client.data.msg,param^.progBarParam^.msg,maxlen);
        //
       end;
     SCE_MSG_DIALOG_MODE_SYSTEM_MSG:
       begin
        //
        g_client.data.sysMsgType:=param^.sysMsgParam^.sysMsgType;
        //
       end;
     else;
   end;

  end else
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
  end;

  //reTFEla4NQw
  Result:=g_client.Open('MSG_DIALOG_OPEN',@g_client.data,SizeOf(g_client.data));

 mtx_unlock(g_msg_mtx);
 //
end;

function ps4_sceMsgDialogClose():Integer;
begin
 Writeln('sceMsgDialogClose');
 //status_msg_dialog:=SCE_COMMON_DIALOG_STATUS_FINISHED;
 Result:=0;
end;

function ps4_sceMsgDialogUpdateStatus():Integer;
begin
 Result:=0;
 //Result:=status_msg_dialog;
end;

function ps4_sceMsgDialogGetStatus():Integer;
begin
 Result:=0;
 //Result:=status_msg_dialog;
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

