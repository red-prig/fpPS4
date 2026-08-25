unit ps4_libSigninDialog;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_mtx,
 subr_dynlib,
 sys_bootparam,
 host_ipc;

{$CALLING default}

type
 TSigninDialogOpen=record
  userId:Integer;
 end;

implementation

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

var
 g_SigninDialog_mtx:mtx;
 g_state        :Integer=0;

{$CALLING SysV_ABI_CDecl}

const
 SCE_SIGNIN_DIALOG_ERROR_NOT_INITIALIZED    =-2127233023; // 0x81350001
 SCE_SIGNIN_DIALOG_ERROR_ALREADY_INITIALIZED=-2127233022; // 0x81350002
 SCE_SIGNIN_DIALOG_ERROR_PARAM_INVALID      =-2127233021; // 0x81350003
 SCE_SIGNIN_DIALOG_ERROR_UNEXPECTED_FATAL   =-2127233020; // 0x81350004
 SCE_SIGNIN_DIALOG_ERROR_INVALID_STATE      =-2127233019; // 0x81350005
 SCE_SIGNIN_DIALOG_ERROR_SERVICE_BUSY       =-2127233018; // 0x81350006
 SCE_SIGNIN_DIALOG_ERROR_INVALID_USER_ID    =-2127233017; // 0x81350007

 //SceSigninDialogStatus
 SCE_SIGNIN_DIALOG_STATUS_NONE       =0;
 SCE_SIGNIN_DIALOG_STATUS_INITIALIZED=1;
 SCE_SIGNIN_DIALOG_STATUS_RUNNING    =2;
 SCE_SIGNIN_DIALOG_STATUS_FINISHED   =3;

 //SceSigninDialogResultType
 SCE_SIGNIN_DIALOG_RESULT_OK           =0;
 SCE_SIGNIN_DIALOG_RESULT_USER_CANCELED=1;

type
 pSceSigninDialogParam=^SceSigninDialogParam;
 SceSigninDialogParam=packed record
  size    :Integer;
  userId  :Integer;
  reserved:array[0..1] of Integer;
 end;

 SceSigninDialogResultType=Integer;

 pSceSigninDialogResult=^SceSigninDialogResult;
 SceSigninDialogResult=packed record
  result  :SceSigninDialogResultType;
  reserved:array[0..2] of Integer;
 end;

//

function ps4_sceSigninDialogInitialize():Integer;
begin
 LOG_INFO('sceSigninDialogInitialize');

 Result:=SCE_SIGNIN_DIALOG_ERROR_ALREADY_INITIALIZED;
 mtx_lock(g_SigninDialog_mtx);

  if (g_state=SCE_SIGNIN_DIALOG_STATUS_NONE) then
  begin
   Result:=0;
   g_state:=SCE_SIGNIN_DIALOG_STATUS_INITIALIZED;
  end;

 mtx_unlock(g_SigninDialog_mtx);
end;

function InvokeSync2(const msg:RawByteString;buf:Pointer;len:DWORD):Integer;
begin
 Result:=p_host_ipc.InvokeSync2(msg,buf,len);
 if (Result=-1) then
 begin
  Result:=SCE_SIGNIN_DIALOG_ERROR_SERVICE_BUSY;
 end else
 if (Result=-2) then
 begin
  Result:=SCE_SIGNIN_DIALOG_ERROR_INVALID_USER_ID;
 end else
 if (Result<0) then
 begin
  Result:=SCE_SIGNIN_DIALOG_ERROR_UNEXPECTED_FATAL;
 end;
end;

function ps4_sceSigninDialogOpen(param:pSceSigninDialogParam):Integer;
var
 data:TSigninDialogOpen;
begin
 if (param=nil) then
 begin
  Exit(SCE_SIGNIN_DIALOG_ERROR_PARAM_INVALID);
 end;

 if (param^.size<>$10) or
    (param^.reserved[0]<>0) or
    (param^.reserved[1]<>0) then
 begin
  Exit(SCE_SIGNIN_DIALOG_ERROR_PARAM_INVALID);
 end;

 if (DWORD(param^.userId + $f0000000) > $2fffffff) then
 begin
  Exit(SCE_SIGNIN_DIALOG_ERROR_INVALID_USER_ID);
 end;

 LOG_INFO('sceSigninDialogOpen');

 mtx_lock(g_SigninDialog_mtx);

   case g_state of
    SCE_SIGNIN_DIALOG_STATUS_NONE:
      Result:=SCE_SIGNIN_DIALOG_ERROR_NOT_INITIALIZED;
    SCE_SIGNIN_DIALOG_STATUS_RUNNING:
      Result:=SCE_SIGNIN_DIALOG_STATUS_RUNNING;
    else
      begin
       data.userId:=param^.userId;
       Result:=InvokeSync2('SIGNIN_DIALOG_OPEN',@data,sizeof(data));
       if (Result=0) then
       begin
        g_state:=SCE_SIGNIN_DIALOG_STATUS_RUNNING;
       end;
      end;
   end;

 mtx_unlock(g_SigninDialog_mtx);
end;

function ps4_sceSigninDialogClose():Integer;
begin
 mtx_lock(g_SigninDialog_mtx);

  case g_state of
   SCE_SIGNIN_DIALOG_STATUS_NONE:
     Result:=SCE_SIGNIN_DIALOG_ERROR_NOT_INITIALIZED;
   SCE_SIGNIN_DIALOG_STATUS_RUNNING:
     begin
      Result:=InvokeSync2('SIGNIN_DIALOG_CLOSE',nil,0);
      if (Result=0) then
      begin
       g_state:=SCE_SIGNIN_DIALOG_STATUS_FINISHED;
      end;
     end;
   else
     Result:=0;
  end;

 mtx_unlock(g_SigninDialog_mtx);
end;

function ps4_sceSigninDialogTerminate():Integer;
begin
 mtx_lock(g_SigninDialog_mtx);

  case g_state of
   SCE_SIGNIN_DIALOG_STATUS_NONE:
     Result:=SCE_SIGNIN_DIALOG_ERROR_NOT_INITIALIZED;
   SCE_SIGNIN_DIALOG_STATUS_RUNNING:
     begin
      InvokeSync2('SIGNIN_DIALOG_TERM',nil,0);
      g_state:=SCE_SIGNIN_DIALOG_STATUS_NONE;
      Result:=0;
     end;
   else
     begin
      g_state:=SCE_SIGNIN_DIALOG_STATUS_NONE;
      Result:=0;
     end;
  end;

 mtx_unlock(g_SigninDialog_mtx);
end;

function ps4_sceSigninDialogGetStatus:Integer;
begin
 mtx_lock(g_SigninDialog_mtx);

  Result:=g_state;

 mtx_unlock(g_SigninDialog_mtx);
end;

function ps4_sceSigninDialogUpdateStatus:Integer;
begin
 mtx_lock(g_SigninDialog_mtx);

  if (g_state=SCE_SIGNIN_DIALOG_STATUS_RUNNING) then
  begin
   if (InvokeSync2('SIGNIN_DIALOG_UPDATE',nil,0)=1) then
   begin
    g_state:=SCE_SIGNIN_DIALOG_STATUS_FINISHED;
   end;
  end;

  Result:=g_state;

 mtx_unlock(g_SigninDialog_mtx);
end;

function ps4_sceSigninDialogGetResult(pResult:pSceSigninDialogResult):Integer;
begin
 if (pResult=nil) then
 begin
  Exit(SCE_SIGNIN_DIALOG_ERROR_PARAM_INVALID);
 end;

 mtx_lock(g_SigninDialog_mtx);

  case g_state of
   SCE_SIGNIN_DIALOG_STATUS_NONE:
     Result:=SCE_SIGNIN_DIALOG_ERROR_NOT_INITIALIZED;
   SCE_SIGNIN_DIALOG_STATUS_FINISHED:
     begin
      Result:=InvokeSync2('SIGNIN_DIALOG_RESULT',nil,0);
      if (Result>=0) then
      begin
       pResult^.result:=Result;
       Result:=0;
      end else
      begin
       pResult^.result:=SCE_SIGNIN_DIALOG_RESULT_USER_CANCELED;
      end;
     end;
   else
     begin
      pResult^.result:=SCE_SIGNIN_DIALOG_RESULT_USER_CANCELED;
      Result:=SCE_SIGNIN_DIALOG_ERROR_INVALID_STATE;
     end;
  end;

 mtx_unlock(g_SigninDialog_mtx);
end;

//

function dt_fini(args:QWORD;argp,addr:Pointer):Integer;
begin
 ps4_sceSigninDialogTerminate();
 Result:=0;
end;

//

{$WARN 4110 off}
function Load_libSceSigninDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceSigninDialog');

 Result^.fini_proc_addr.native:=@dt_fini;

 lib:=Result^.add_lib('libSceSigninDialog');
 lib.set_proc($9A56067E6A84DDF4,@ps4_sceSigninDialogInitialize);
 lib.set_proc($265A49568456BFB5,@ps4_sceSigninDialogOpen);
 lib.set_proc($3373A410D1DCCA25,@ps4_sceSigninDialogClose);
 lib.set_proc($2D79664BA3EF25D5,@ps4_sceSigninDialogTerminate);
 lib.set_proc($DA6D3BEDA782F8F0,@ps4_sceSigninDialogGetStatus);
 lib.set_proc($070DF59624C54F70,@ps4_sceSigninDialogUpdateStatus);
 lib.set_proc($9EA1BBAEA9D8C355,@ps4_sceSigninDialogGetResult);

 mtx_init(g_SigninDialog_mtx,'g_SigninDialog_mtx');
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceSigninDialog.prx',@Load_libSceSigninDialog);

end.

