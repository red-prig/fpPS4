unit ps4_libSceCommonDialog;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 kern_mtx,
 sys_bootparam,
 host_ipc_interface,
 core_serialization,
 subr_dynlib;

Const
 //SceCommonDialogStatus
 SCE_COMMON_DIALOG_STATUS_NONE         =0;
 SCE_COMMON_DIALOG_STATUS_INITIALIZED  =1;
 SCE_COMMON_DIALOG_STATUS_RUNNING      =2;
 SCE_COMMON_DIALOG_STATUS_FINISHED     =3;

 //SceCommonDialogResult
 SCE_COMMON_DIALOG_RESULT_OK           =0;
 SCE_COMMON_DIALOG_RESULT_USER_CANCELED=1;

 SCE_COMMON_DIALOG_MAGIC_NUMBER=$C0D1A109;

type
 pSceCommonDialogBaseParam=^SceCommonDialogBaseParam;
 SceCommonDialogBaseParam=packed record
  size    :QWORD;
  reserved:array[0..35] of Byte;
  magic   :DWORD;
 end;

const
 SCE_COMMON_DIALOG_ERROR_NOT_SYSTEM_INITIALIZED    =-2135425023; // 0x80B80001
 SCE_COMMON_DIALOG_ERROR_ALREADY_SYSTEM_INITIALIZED=-2135425022; // 0x80B80002
 SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED           =-2135425021; // 0x80B80003
 SCE_COMMON_DIALOG_ERROR_ALREADY_INITIALIZED       =-2135425020; // 0x80B80004
 SCE_COMMON_DIALOG_ERROR_NOT_FINISHED              =-2135425019; // 0x80B80005
 SCE_COMMON_DIALOG_ERROR_INVALID_STATE             =-2135425018; // 0x80B80006
 SCE_COMMON_DIALOG_ERROR_RESULT_NONE               =-2135425017; // 0x80B80007
 SCE_COMMON_DIALOG_ERROR_BUSY                      =-2135425016; // 0x80B80008
 SCE_COMMON_DIALOG_ERROR_OUT_OF_MEMORY             =-2135425015; // 0x80B80009
 SCE_COMMON_DIALOG_ERROR_PARAM_INVALID             =-2135425014; // 0x80B8000A
 SCE_COMMON_DIALOG_ERROR_NOT_RUNNING               =-2135425013; // 0x80B8000B
 SCE_COMMON_DIALOG_ERROR_ALREADY_CLOSE             =-2135425012; // 0x80B8000C
 SCE_COMMON_DIALOG_ERROR_ARG_NULL                  =-2135425011; // 0x80B8000D
 SCE_COMMON_DIALOG_ERROR_UNEXPECTED_FATAL          =-2135425010; // 0x80B8000E
 SCE_COMMON_DIALOG_ERROR_NOT_SUPPORTED             =-2135425009; // 0x80B8000F
 SCE_COMMON_DIALOG_ERROR_INHIBIT_SHAREPLAY_CLIENT  =-2135425008; // 0x80B80010

function ps4_sceCommonDialogIsUsed():Boolean;

{$CALLING default}

type
 TCommonDialogClient=class  //(TSerializeObject)
  private
   status:Byte;
   finish:Byte;
   closed:Byte;
   rzdata:array of Byte;
   function   OnCdlgFinish(mlen:DWORD;buf:Pointer):Ptruint;
  public
   function   isInitializedStatus:Boolean;
   function   isFinish:Boolean;
   function   launchCmnDialog:Integer;
   Procedure  Send(const msg:RawByteString;buf:Pointer;len:DWORD);
   function   Open(const msg:RawByteString;buf:Pointer;len:DWORD):Integer;
   function   SetValue(buf:Pointer;len:DWORD):Integer;
   function   SetMsg  (buf:Pointer;len:DWORD):Integer;
   function   getFinishData(buf:Pointer;len:DWORD):Integer;
   function   updateState:Integer;                  virtual;
   function   Close(buf:Pointer;len:DWORD):Integer; virtual;
   procedure  Terminate;                            virtual;
   Destructor Destroy;                              override;
 end;

function CheckBaseParam(pBaseParam:pSceCommonDialogBaseParam):Integer;
function CheckReserved(var buf;len:DWORD):Integer;
function strnlen_s(s:PChar;maxlen:ptrint):ptrint;
function strncpy_s(dst,src:PChar;maxlen:ptrint):PChar;
function strncmp  (str1,str2:PChar;maxlen:ptrint):Integer;

implementation

var
 g_common_dialog_init:Byte=0;
 g_common_dialog_mtx :mtx;
 g_curr_client       :TCommonDialogClient;

function clientRegister(client:TCommonDialogClient):Boolean;
begin
 Result:=False;

 mtx_lock(g_common_dialog_mtx);

  if (g_curr_client=nil) then
  begin
   g_curr_client:=client;
   Result:=True;
  end;

 mtx_unlock(g_common_dialog_mtx);
end;

function clientDeregister(client:TCommonDialogClient):Boolean;
begin
 Result:=False;

 mtx_lock(g_common_dialog_mtx);

  if (g_curr_client=client) then
  begin
   g_curr_client:=nil;
   Result:=True;
  end;

 mtx_unlock(g_common_dialog_mtx);
end;

//afLdI6i0lQw
function CheckBaseParam(pBaseParam:pSceCommonDialogBaseParam):Integer;
var
 i:Integer;
begin
 if (pBaseParam^.magic<>DWORD(PtrUint(pBaseParam)+SCE_COMMON_DIALOG_MAGIC_NUMBER)) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 if (pBaseParam^.size<>$30) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 For i:=0 to High(SceCommonDialogBaseParam.reserved) do
 if (pBaseParam^.reserved[i]<>0) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 Result:=0;
end;

function CheckReserved(var buf;len:DWORD):Integer;
var
 i:DWORD;
begin
 for i:=0 to len-1 do
 if (PByte(@buf)[i]<>0) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;
 Result:=0;
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

function strncpy_s(dst,src:PChar;maxlen:ptrint):PChar;
begin
 if (dst=nil) or (src=nil) then Exit(nil);
 Result:=StrLCopy(dst,src,maxlen);
end;

function strncmp(str1,str2:PChar;maxlen:ptrint):Integer;
begin
 Result:=CompareChar0(str1^,str2^,maxlen);
end;

function TCommonDialogClient.isInitializedStatus:Boolean;
begin
 Result:=(status=SCE_COMMON_DIALOG_STATUS_INITIALIZED);
end;

function TCommonDialogClient.isFinish:Boolean;
begin
 Result:=(status=SCE_COMMON_DIALOG_STATUS_FINISHED);
end;

//QXFsLON5QWw
function TCommonDialogClient.launchCmnDialog:Integer;
begin
 Result:=SCE_COMMON_DIALOG_ERROR_UNEXPECTED_FATAL;
 if clientRegister(Self) then
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_INVALID_STATE;
  if (status=SCE_COMMON_DIALOG_STATUS_NONE) then
  begin
   Result:=0;
   status:=SCE_COMMON_DIALOG_STATUS_INITIALIZED;
  end;
 end;
end;

Procedure TCommonDialogClient.Send(const msg:RawByteString;buf:Pointer;len:DWORD);
begin
 p_host_ipc.SendAsyn(HashIpcStr(msg),len,buf);
end;

function TCommonDialogClient.Open(const msg:RawByteString;buf:Pointer;len:DWORD):Integer;
begin
 if (status<>SCE_COMMON_DIALOG_STATUS_INITIALIZED) and
    (status<>SCE_COMMON_DIALOG_STATUS_FINISHED)  then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_INVALID_STATE);
 end;
 //
 SetLength(rzdata,0);
 finish:=0;
 closed:=0;
 status:=SCE_COMMON_DIALOG_STATUS_RUNNING;
 //
 Send(msg,buf,len);
 //
 Result:=0;
end;

function TCommonDialogClient.SetValue(buf:Pointer;len:DWORD):Integer;
begin
 if (status<>SCE_COMMON_DIALOG_STATUS_RUNNING)  then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_INVALID_STATE);
 end;

 Send('CDLG_SET_VALUE',buf,len);

 Result:=0;
end;

function TCommonDialogClient.SetMsg(buf:Pointer;len:DWORD):Integer;
begin
 if (status<>SCE_COMMON_DIALOG_STATUS_RUNNING)  then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_INVALID_STATE);
 end;

 Send('CDLG_SET_MSG',buf,len);

 Result:=0;
end;

function TCommonDialogClient.getFinishData(buf:Pointer;len:DWORD):Integer;
begin
 if (status<>SCE_COMMON_DIALOG_STATUS_FINISHED)  then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_RESULT_NONE);
 end;

 if (len>Length(rzdata)) then len:=Length(rzdata);
 Move(rzdata[0],buf^,len);

 Result:=0;
end;

function TCommonDialogClient.updateState:Integer;
begin
 mtx_lock(g_common_dialog_mtx);

  if (finish<>0) and (status=SCE_COMMON_DIALOG_STATUS_RUNNING) then
  begin
   status:=SCE_COMMON_DIALOG_STATUS_FINISHED;
  end;

 mtx_unlock(g_common_dialog_mtx);
 //
 Result:=0;
end;

function TCommonDialogClient.Close(buf:Pointer;len:DWORD):Integer;
begin
 if (status<>SCE_COMMON_DIALOG_STATUS_RUNNING) or (finish<>0) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_INVALID_STATE);
 end;

 if (closed<>0) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_ALREADY_CLOSE);
 end;

 Send('CDLG_CLOSE',buf,len);

 closed:=1;
 Result:=0;
end;

procedure TCommonDialogClient.Terminate;
begin
 Close(nil,0);
 //
 Free;
end;

Destructor TCommonDialogClient.Destroy;
begin
 clientDeregister(Self);
 inherited;
end;

function TCommonDialogClient.OnCdlgFinish(mlen:DWORD;buf:Pointer):Ptruint;
begin
 Result:=0;

 mtx_lock(g_common_dialog_mtx);

  if (g_curr_client<>nil) then
  with g_curr_client do
   if (status=SCE_COMMON_DIALOG_STATUS_RUNNING) then
   if (finish=0) then
   begin
    SetLength(rzdata,mlen);
    Move(buf^,rzdata[0],mlen);
    //
    finish:=1;
   end;

 mtx_unlock(g_common_dialog_mtx);
end;

{$CALLING SysV_ABI_CDecl}
//

function ps4_sceCommonDialogInitialize():Integer;
begin
 Result:=0;
 Writeln('sceCommonDialogInitialize');

 if (g_common_dialog_init=0) then
 begin
  g_common_dialog_init:=1;
  mtx_lock(g_common_dialog_mtx);

   //DialogInitialize
   p_host_handler.AddCallback('CDLG_FINISH',@TCommonDialogClient(nil).OnCdlgFinish);

  mtx_unlock(g_common_dialog_mtx);
 end else
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_ALREADY_SYSTEM_INITIALIZED;
 end;

end;

function ps4_sceCommonDialogIsUsed():Boolean;
begin
 mtx_lock(g_common_dialog_mtx);

  Result:=(g_curr_client<>nil);

 mtx_unlock(g_common_dialog_mtx);
end;

function dt_fini(args:QWORD;argp,addr:Pointer):Integer;
begin
 mtx_lock(g_common_dialog_mtx);

  p_host_handler.DelCallback('CDLG_FINISH');

 mtx_unlock(g_common_dialog_mtx);
 Result:=0;
end;

//

function Load_libSceCommonDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceCommonDialog');

 Result^.fini_proc_addr.native:=@dt_fini;

 lib:=Result^.add_lib('libSceCommonDialog');
 lib.set_proc($BA85292C6364CA09,@ps4_sceCommonDialogInitialize);
 lib.set_proc($050DED7B2D099903,@ps4_sceCommonDialogIsUsed);

 mtx_init(g_common_dialog_mtx,'g_common_dialog_mtx');
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceCommonDialog.prx',@Load_libSceCommonDialog);

end.

