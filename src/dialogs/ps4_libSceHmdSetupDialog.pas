unit ps4_libSceHmdSetupDialog;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_mtx,
 subr_dynlib,
 ps4_libSceCommonDialog;

{$CALLING default}

type
 THmdSetupDialogOpen=record
  userId               :Integer;
  disableHandoverScreen:Boolean;
 end;

 THmdSetupDialogResult=record
  resultId:Integer;
 end;

 THmdSetupDialogClient=class(TCommonDialogClient)
  data:THmdSetupDialogOpen;
 end;

implementation

var
 g_HmdSetup_mtx:mtx;
 g_client      :THmdSetupDialogClient=nil;

{$CALLING SysV_ABI_CDecl}

type
 pSceHmdSetupDialogParam=^SceHmdSetupDialogParam;
 SceHmdSetupDialogParam=packed record
  baseParam            :SceCommonDialogBaseParam;
  size                 :QWORD;
  userId               :Integer;
  disableHandoverScreen:Boolean;
  reserved             :array[0..39] of Byte;
  padding              :array[0..2] of Byte;
 end;

 pSceHmdSetupDialogResult=^SceHmdSetupDialogResult;
 SceHmdSetupDialogResult=packed record
  result  :Integer;
  reserved:array[0..31] of Byte;
 end;

function ps4_sceHmdSetupDialogInitialize():Integer;
var
 client:THmdSetupDialogClient;
begin
 Writeln('sceHmdSetupDialogInitialize');

 mtx_lock(g_HmdSetup_mtx);

  Result:=SCE_COMMON_DIALOG_ERROR_ALREADY_INITIALIZED;
  if (g_client=nil) then
  begin

   Result:=SCE_COMMON_DIALOG_ERROR_BUSY;
   if (not ps4_sceCommonDialogIsUsed) then
   begin
    client:=THmdSetupDialogClient.Create;

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

 mtx_unlock(g_HmdSetup_mtx);
end;

function ps4_sceHmdSetupDialogOpen(param:pSceHmdSetupDialogParam):Integer;
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

 if (param^.size<>$68) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 if (DWORD(param^.userId + $f0000000) >= $30000000) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 if CheckReserved(param^.reserved,SizeOf(param^.reserved))<>0 then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 Writeln('sceHmdSetupDialogOpen');

 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_HmdSetup_mtx);

  if (g_client<>nil) then
  begin
   g_client.data.userId               :=param^.userId;
   g_client.data.disableHandoverScreen:=param^.disableHandoverScreen;

   Result:=g_client.Open('HMDSETUP_DIALOG_OPEN',@g_client.data,SizeOf(g_client.data));
  end;

 mtx_unlock(g_HmdSetup_mtx);
end;

function ps4_sceHmdSetupDialogClose():Integer;
begin
 Writeln('sceHmdSetupDialogClose');

 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_HmdSetup_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_RUNNING;
   if (not g_client.isInitializedStatus) then
   if (not g_client.isFinish) then
   begin
    Result:=g_client.Close(nil,0);
   end;
  end;

 mtx_unlock(g_HmdSetup_mtx);
 //
end;

function ps4_sceHmdSetupDialogTerminate():Integer;
begin
 Writeln('sceHmdSetupDialogTerminate');

 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_HmdSetup_mtx);

  if (g_client<>nil) then
  begin
   g_client.Terminate;
   g_client:=nil;
   Result:=0;
  end;

 mtx_unlock(g_HmdSetup_mtx);
 //
end;

//

function ps4_sceHmdSetupDialogUpdateStatus():Integer;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;

 mtx_lock(g_HmdSetup_mtx);

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

 mtx_unlock(g_HmdSetup_mtx);
 //
end;

function ps4_sceHmdSetupDialogGetStatus():Integer;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;

 mtx_lock(g_HmdSetup_mtx);

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

 mtx_unlock(g_HmdSetup_mtx);
 //
end;

function ps4_sceHmdSetupDialogGetResult(pResult:pSceHmdSetupDialogResult):Integer;
var
 rzdata:THmdSetupDialogResult;
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
 mtx_lock(g_HmdSetup_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_FINISHED;
   if (g_client.isFinish) then
   begin
    rzdata:=Default(THmdSetupDialogResult);
    g_client.getFinishData(@rzdata,sizeof(rzdata));

    pResult^.result:=rzdata.resultId;

    Result:=0;
   end;
  end;

 mtx_unlock(g_HmdSetup_mtx);
 //
end;

//

function dt_fini(args:QWORD;argp,addr:Pointer):Integer;
begin
 ps4_sceHmdSetupDialogTerminate();
 Result:=0;
end;

//

function Load_libSceHmdSetupDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceHmdSetupDialog');

 Result^.fini_proc_addr.native:=@dt_fini;

 lib:=Result^.add_lib('libSceHmdSetupDialog');
 lib.set_proc($341D58DA40368C26,@ps4_sceHmdSetupDialogInitialize);
 lib.set_proc($34D8225784FE6A45,@ps4_sceHmdSetupDialogOpen);
 lib.set_proc($9E61F35381A1D31B,@ps4_sceHmdSetupDialogClose);
 lib.set_proc($FB3E0E26616B7997,@ps4_sceHmdSetupDialogTerminate);
 lib.set_proc($51DEE3DFE4432018,@ps4_sceHmdSetupDialogUpdateStatus);
 lib.set_proc($27D781A56D6E765E,@ps4_sceHmdSetupDialogGetStatus);
 lib.set_proc($EA55511CC5792D8D,@ps4_sceHmdSetupDialogGetResult);

 mtx_init(g_HmdSetup_mtx,'g_HmdSetup_mtx');
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceHmdSetupDialog.prx',@Load_libSceHmdSetupDialog);

end.

