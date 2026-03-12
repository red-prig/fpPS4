unit ps4_libSceMsgDialog;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
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
 PMsgDialogOpen=^TMsgDialogOpen;
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

 TMsgDialogResult=record
  resultId:Integer;
  buttonId:Integer; //SceMsgDialogButtonId
 end;

 TMsgDialogClient=class(TCommonDialogClient)
  data:TMsgDialogOpen;
  rate:DWORD;
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
 g_MsgDialog_mtx:mtx;
 g_client       :TMsgDialogClient=nil;

{$CALLING SysV_ABI_CDecl}

function ps4_sceMsgDialogInitialize():Integer;
var
 client:TMsgDialogClient;
begin
 Result:=0;
 Writeln('sceMsgDialogInitialize');

 mtx_lock(g_MsgDialog_mtx);

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

 mtx_unlock(g_MsgDialog_mtx);
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
 maxlen,len:DWORD;
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

 if CheckReserved(userMsgParam^.reserved,SizeOf(userMsgParam^.reserved))<>0 then
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
 len:DWORD;
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

 if CheckReserved(progBarParam^.reserved,SizeOf(progBarParam^.reserved))<>0 then
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
 maxlen:DWORD;
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

 if CheckReserved(param^.reserved,SizeOf(param^.reserved))<>0 then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 Writeln('sceMsgDialogOpen');

 mtx_lock(g_MsgDialog_mtx);

  if (g_client<>nil) then
  begin

   g_client.data:=Default(TMsgDialogOpen);
   g_client.rate:=0;

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
        strncpy_s(g_client.data.msg,param^.userMsgParam^.msg,maxlen);
        //
        if (g_client.data.buttonType=SCE_MSG_DIALOG_BUTTON_TYPE_2BUTTONS) then
        begin
         strncpy_s(g_client.data.msg1,param^.userMsgParam^.buttonsParam^.msg1,63);
         strncpy_s(g_client.data.msg2,param^.userMsgParam^.buttonsParam^.msg2,63);
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
        strncpy_s(g_client.data.msg,param^.progBarParam^.msg,maxlen);
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

 mtx_unlock(g_MsgDialog_mtx);
 //
end;

function ps4_sceMsgDialogClose():Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 Writeln('sceMsgDialogClose');

 mtx_lock(g_MsgDialog_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_RUNNING;
   if (not g_client.isInitializedStatus) then
   if (not g_client.isFinish) then
   begin
    Result:=g_client.Close;
   end;
  end;

 mtx_unlock(g_MsgDialog_mtx);
 //
end;

function ps4_sceMsgDialogTerminate():Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 Writeln('sceMsgDialogTerminate');

 mtx_lock(g_MsgDialog_mtx);

  if (g_client<>nil) then
  begin
   g_client.Terminate;
   g_client:=nil;
   Result:=0;
  end;

 mtx_unlock(g_MsgDialog_mtx);
 //
end;

function ps4_sceMsgDialogUpdateStatus():Integer;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;

 mtx_lock(g_MsgDialog_mtx);

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

 mtx_unlock(g_MsgDialog_mtx);
 //
end;

function ps4_sceMsgDialogGetStatus():Integer;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;

 mtx_lock(g_MsgDialog_mtx);

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

 mtx_unlock(g_MsgDialog_mtx);
 //
end;

function ps4_sceMsgDialogGetResult(pResult:pSceMsgDialogResult):Integer;
var
 rzdata:TMsgDialogResult;
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
 mtx_lock(g_MsgDialog_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_FINISHED;
   if (g_client.isFinish) then
   begin
    rzdata:=Default(TMsgDialogResult);;
    g_client.getFinishData(@rzdata,sizeof(rzdata));

    //fixup
    if (p_proc.p_sdk_version >= $3000000) then
    if (rzdata.buttonId = 0) and (rzdata.resultId = 1) then
    begin
     case g_client.data.mode of
       SCE_MSG_DIALOG_MODE_USER_MSG:
         begin
          case g_client.data.buttonType of
           0:begin
              rzdata.resultId:=0;
              rzdata.buttonId:=1;
             end;
           1,7:
             begin
              rzdata.resultId:=0;
              rzdata.buttonId:=2;
             end;
           else
             begin
              rzdata.resultId:=1;
              rzdata.buttonId:=0;
             end;
          end;
         end;
       SCE_MSG_DIALOG_MODE_SYSTEM_MSG:
         case g_client.data.sysMsgType of
          3:; //skip
          5:; //skip
          else
            rzdata.resultId:=0;
            rzdata.buttonId:=1;
         end;
       else;
     end;
    end;

    pResult^.mode    :=0;
    pResult^.result  :=rzdata.resultId;
    pResult^.buttonId:=rzdata.buttonId;

    Result:=rzdata.resultId;
   end;
  end;

 mtx_unlock(g_MsgDialog_mtx);
 //
end;

function ps4_sceMsgDialogProgressBarSetValue(target:Integer;rate:DWORD):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_MsgDialog_mtx);

  if (g_client<>nil) then
  if (not g_client.isInitializedStatus) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_RUNNING;
   if (not g_client.isFinish) then
   begin
    //
    if (g_client.data.mode<>SCE_MSG_DIALOG_MODE_PROGRESS_BAR) then
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

 mtx_unlock(g_MsgDialog_mtx);
 //
end;

function ps4_sceMsgDialogProgressBarInc(target:Integer;delta:DWORD):Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_MsgDialog_mtx);

  if (g_client<>nil) then
  if (not g_client.isInitializedStatus) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_RUNNING;
   if (not g_client.isFinish) then
   begin
    //
    if (g_client.data.mode<>SCE_MSG_DIALOG_MODE_PROGRESS_BAR) then
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

 mtx_unlock(g_MsgDialog_mtx);
 //
end;

function ps4_sceMsgDialogProgressBarSetMsg(target:Integer;barMsg:pchar):Integer;
var
 maxlen,len:DWORD;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_MsgDialog_mtx);

  if (g_client<>nil) then
  if (not g_client.isInitializedStatus) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_RUNNING;
   if (not g_client.isFinish) then
   begin
    //
    if (g_client.data.mode<>SCE_MSG_DIALOG_MODE_PROGRESS_BAR) then
    begin
     Result:=SCE_COMMON_DIALOG_ERROR_NOT_SUPPORTED;
    end else
    if (target<>0) then
    begin
     Result:=SCE_COMMON_DIALOG_ERROR_PARAM_INVALID;
    end else
    begin
     Result:=0;
     if (p_proc.p_sdk_version >= $1700000) then
     begin
      //
      if (p_proc.p_sdk_version > $1500000) then
      begin
       len:=strnlen_s(barMsg,$2000);
       if (len>=$2000) then
       begin
        mtx_unlock(g_MsgDialog_mtx);
        Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID)
       end;
      end;
      //
      if (p_proc.p_sdk_version < $1500000) then
      begin
       maxlen:=$200;
      end else
      begin
       maxlen:=$2000;
      end;
      //
      len:=strnlen_s(barMsg,maxlen);
      //
      Result:=g_client.SetMsg(barMsg,len);
     end;
    end;
    //
   end;
  end;

 mtx_unlock(g_MsgDialog_mtx);
 //
end;

function dt_fini(args:QWORD;argp,addr:Pointer):Integer;
begin
 ps4_sceMsgDialogTerminate();
 Result:=0;
end;

//

function Load_libSceMsgDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceMsgDialog');

 Result^.fini_proc_addr.native:=@dt_fini;

 lib:=Result^.add_lib('libSceMsgDialog');
 lib.set_proc($943AB1698D546C4A,@ps4_sceMsgDialogInitialize);
 lib.set_proc($6F4E878740CF11A1,@ps4_sceMsgDialogOpen);
 lib.set_proc($1D3ADC0CA9452AE3,@ps4_sceMsgDialogClose);
 lib.set_proc($78FC3F92A6667A5A,@ps4_sceMsgDialogTerminate);
 lib.set_proc($E9F202DD72ADDA4D,@ps4_sceMsgDialogUpdateStatus);
 lib.set_proc($096556EFC41CDDF2,@ps4_sceMsgDialogGetStatus);
 lib.set_proc($2EBF28BC71FD97A0,@ps4_sceMsgDialogGetResult);
 lib.set_proc($C13A5F825926BF7E,@ps4_sceMsgDialogProgressBarSetValue);
 lib.set_proc($19CE64D6A70AE1FB,@ps4_sceMsgDialogProgressBarInc);
 lib.set_proc($E87FFBD4E76BA573,@ps4_sceMsgDialogProgressBarSetMsg);

 mtx_init(g_MsgDialog_mtx,'g_MsgDialog_mtx');
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceMsgDialog.prx',@Load_libSceMsgDialog);

end.

