unit ps4_libSceErrorDialog;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_mtx,
 subr_dynlib,
 kern_proc,
 sys_bootparam,
 host_ipc_interface;

{$CALLING default}

type
 PErrDialogOpen=^TErrDialogOpen;
 TErrDialogOpen=record
  errorCode:Integer;
  userId   :Integer;
 end;

implementation

var
 g_ErrDialog_mtx:mtx;
 g_state        :Integer=0;

{$CALLING SysV_ABI_CDecl}

const
 SCE_ERROR_DIALOG_ERROR_NOT_INITIALIZED    =-2131951615; // 0x80ED0001
 SCE_ERROR_DIALOG_ERROR_ALREADY_INITIALIZED=-2131951614; // 0x80ED0002
 SCE_ERROR_DIALOG_ERROR_PARAM_INVALID      =-2131951613; // 0x80ED0003
 SCE_ERROR_DIALOG_ERROR_UNEXPECTED_FATAL   =-2131951612; // 0x80ED0004
 SCE_ERROR_DIALOG_ERROR_INVALID_STATE      =-2131951611; // 0x80ED0005
 SCE_ERROR_DIALOG_ERROR_SERVICE_BUSY       =-2131951610; // 0x80ED0006
 SCE_ERROR_DIALOG_ERROR_INVALID_USER_ID    =-2131951609; // 0x80ED0007

 SCE_ERROR_DIALOG_STATUS_NONE       =0;
 SCE_ERROR_DIALOG_STATUS_INITIALIZED=1;
 SCE_ERROR_DIALOG_STATUS_RUNNING    =2;
 SCE_ERROR_DIALOG_STATUS_FINISHED   =3;

type
 pSceErrorDialogParam=^SceErrorDialogParam;
 SceErrorDialogParam=packed record
  size     :Integer;
  errorCode:Integer;
  userId   :Integer;
  reserved :Integer;
 end;

function ps4_sceErrorDialogInitialize():Integer;
begin
 Writeln('sceErrorDialogInitialize');
 Result:=SCE_ERROR_DIALOG_ERROR_ALREADY_INITIALIZED;
 mtx_lock(g_ErrDialog_mtx);

  if (g_state=SCE_ERROR_DIALOG_STATUS_NONE) then
  begin
   Result:=0;
   g_state:=SCE_ERROR_DIALOG_STATUS_INITIALIZED;
  end;

 mtx_unlock(g_ErrDialog_mtx);
end;

function SendSync(const msg:RawByteString;buf:Pointer;len:DWORD):Integer;
begin
 Result:=p_host_ipc.SendSync(HashIpcStr(msg),len,buf);
 if (Result=-1) then
 begin
  Result:=SCE_ERROR_DIALOG_ERROR_SERVICE_BUSY;
 end else
 if (Result<0) then
 begin
  Result:=SCE_ERROR_DIALOG_ERROR_UNEXPECTED_FATAL;
 end;
end;

function ps4_sceErrorDialogOpen(param:pSceErrorDialogParam):Integer;
var
 data:TErrDialogOpen;
begin

 if (param = nil) then
 begin
  Exit(SCE_ERROR_DIALOG_ERROR_PARAM_INVALID);
 end;

 if (param^.size <> 16) then
 begin
  Exit(SCE_ERROR_DIALOG_ERROR_PARAM_INVALID);
 end;

 if (param^.reserved <> 0) then
 begin
  Exit(SCE_ERROR_DIALOG_ERROR_PARAM_INVALID);
 end;

 data.userId   :=param^.userId;
 data.errorCode:=param^.errorCode;

 if (data.errorCode > -1) then
 begin
  Exit(SCE_ERROR_DIALOG_ERROR_PARAM_INVALID);
 end;

 if (data.userId = 0) then
 begin
  Exit(SCE_ERROR_DIALOG_ERROR_INVALID_USER_ID);
 end;

 if (DWORD(data.userId) > $ff) and (DWORD(data.userId + $f0000000) > $2fffffff) then
 begin
  Exit(SCE_ERROR_DIALOG_ERROR_INVALID_USER_ID);
 end;

 Writeln('sceErrorDialogOpen:',HexStr(param^.errorCode,4));

 mtx_lock(g_ErrDialog_mtx);

  case g_state of
   SCE_ERROR_DIALOG_STATUS_NONE:
     Result:=SCE_ERROR_DIALOG_ERROR_NOT_INITIALIZED;
   SCE_ERROR_DIALOG_STATUS_RUNNING:
     Result:=SCE_ERROR_DIALOG_ERROR_INVALID_STATE;
   else
     begin
      Result:=SendSync('ERR_DIALOG_OPEN',@data,sizeof(data));
      if (Result=0) then
      begin
       g_state:=SCE_ERROR_DIALOG_STATUS_RUNNING;
      end;
     end;
  end;

 mtx_unlock(g_ErrDialog_mtx);
end;

function ps4_sceErrorDialogClose():Integer;
begin
 mtx_lock(g_ErrDialog_mtx);

  case g_state of
   SCE_ERROR_DIALOG_STATUS_NONE:
     Result:=SCE_ERROR_DIALOG_ERROR_NOT_INITIALIZED;
   SCE_ERROR_DIALOG_STATUS_RUNNING:
     begin
      Result:=SendSync('ERR_DIALOG_CLOSE',nil,0);
      if (Result=0) then
      begin
       g_state:=SCE_ERROR_DIALOG_STATUS_FINISHED;
      end;
     end;
   else
     Result:=0;
  end;

 mtx_unlock(g_ErrDialog_mtx);
end;

function ps4_sceErrorDialogUpdateStatus():Integer;
begin
 mtx_lock(g_ErrDialog_mtx);

  if (g_state=SCE_ERROR_DIALOG_STATUS_RUNNING) then
  begin
   if (SendSync('ERR_DIALOG_UPDATE',nil,0)=1) then
   begin
    g_state:=SCE_ERROR_DIALOG_STATUS_FINISHED;
   end;
  end;

  Result:=g_state;

 mtx_unlock(g_ErrDialog_mtx);
end;

function ps4_sceErrorDialogGetStatus():Integer;
begin
 mtx_lock(g_ErrDialog_mtx);

  Result:=g_state;

 mtx_unlock(g_ErrDialog_mtx);
end;

function ps4_sceErrorDialogTerminate():Integer;
begin
 Writeln('sceErrorDialogTerminate');

 mtx_lock(g_ErrDialog_mtx);

  case g_state of
   SCE_ERROR_DIALOG_STATUS_NONE:
     Result:=SCE_ERROR_DIALOG_ERROR_NOT_INITIALIZED;
   SCE_ERROR_DIALOG_STATUS_RUNNING:
     begin
      SendSync('ERR_DIALOG_CLOSE',nil,0);
      g_state:=SCE_ERROR_DIALOG_STATUS_NONE;
      Result:=0;
     end;
   else
     begin
      g_state:=SCE_ERROR_DIALOG_STATUS_NONE;
      Result:=0;
     end;
  end;

 mtx_unlock(g_ErrDialog_mtx);
end;

function dt_fini(args:QWORD;argp,addr:Pointer):Integer;
begin
 ps4_sceErrorDialogTerminate();
 Result:=0;
end;

//

function Load_libSceErrorDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceErrorDialog');

 Result^.fini_proc_addr.native:=@dt_fini;

 lib:=Result^.add_lib('libSceErrorDialog');
 lib.set_proc($23CF0A0A19729D2B,@ps4_sceErrorDialogInitialize);
 lib.set_proc($336645FC294B8606,@ps4_sceErrorDialogOpen);
 lib.set_proc($7A45C76F5903065D,@ps4_sceErrorDialogClose);
 lib.set_proc($596886BA1F577E04,@ps4_sceErrorDialogUpdateStatus);
 lib.set_proc($B7616F1D15F382A9,@ps4_sceErrorDialogGetStatus);
 lib.set_proc($F570312B63CCC24F,@ps4_sceErrorDialogTerminate);

 mtx_init(g_ErrDialog_mtx,'g_ErrDialog_mtx');
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceErrorDialog.prx',@Load_libSceErrorDialog);

end.

