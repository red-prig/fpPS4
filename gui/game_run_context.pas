unit game_run_context;

{$mode ObjFPC}{$H+}

interface

uses
 classes,
 SysUtils,
 Forms,
 md_pipe,
 host_ipc,
 game_info,
 param_sfo_gui,
 playgo_chunk_gui,
 SaveDataBackend;

type
 TGameProcess=class
  g_ipc  :THostIpcConnect;
  g_proc :THandle;
  g_p_pid:Integer;
  g_refs :Integer;
  g_fork :Boolean;
  g_stop :Boolean;
  function    Acquire      :Boolean; virtual;
  function    Release      :Boolean; virtual;
  function    is_terminated:Boolean; virtual;
  function    is_stoped    :Boolean; virtual;
  function    exit_code    :DWORD;   virtual;
  procedure   suspend; virtual;
  procedure   resume;  virtual;
  procedure   stop;    virtual;
  Constructor Create;
  Destructor  Destroy; override;
 end;

//

procedure ReleaseAndNil(var obj:TGameProcess);

{$M+}

type
 TGameRunContext=class
  public
   FParent:TForm;
   //
   FOnRunned:TNotifyEvent;
   FOnStop  :TNotifyEvent;
   //
   FIpcDispatch:THostIpcDispatchGui;
   //
   hOutput:THandle;
   //
   FConfigInfo :TConfigInfo;
   FGameItem   :TGameItem;
   FGameProcess:TGameProcess;
   FParamSfo   :TParamSfoFile;
   FSaveData   :TSaveDataBackendConnect;
   FSdClient   :THandle;
   //
   Procedure Stop();
   procedure StopAndNil();
   Procedure CloseItem();
   //
   function  GameProcessForked:Boolean;
   //
   Procedure CloseSavdata();
   function  FetchSavdata:TSaveDataBackendConnect;
  published
   function  KEV_EVENT     (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  ERROR         (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  WARNING       (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  PARAM_SFO_INIT(Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  PLAYGO_INIT   (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  OpenSaveDataBackend(Client:THostIpc;Value:TIpcValue):TIpcValue;
 end;

{$M-}

function LoadParamSfoFile2(const game:RawByteString):TParamSfoFile;

implementation

uses
 Dialogs,
 MsgDlgExt,
 sys_event;

procedure ReleaseAndNil(var obj:TGameProcess);
begin
 if (obj<>nil) then
 begin
  obj.Release;
  obj:=nil;
 end;
end;

//

function TGameProcess.Acquire:Boolean;
begin
 System.InterlockedIncrement(g_refs);
 Result:=True;
end;

function TGameProcess.Release:Boolean;
begin
 if System.InterlockedDecrement(g_refs)=0 then
 begin
  Free;
 end;
 Result:=True;
end;

function TGameProcess.is_terminated:Boolean;
begin
 Result:=False;
end;

function TGameProcess.is_stoped:Boolean;
begin
 Result:=g_stop;
end;

function TGameProcess.exit_code:DWORD;
begin
 Result:=0;
end;

procedure TGameProcess.suspend;
begin
 //
end;

procedure TGameProcess.resume;
begin
 //
end;

procedure TGameProcess.stop;
begin
 g_stop:=True;
 if (g_ipc<>nil) then
 begin
  g_ipc.Disconnect();
 end;
end;

Constructor TGameProcess.Create;
begin
 g_refs:=1;
end;

Destructor TGameProcess.Destroy;
begin
 FreeAndNil(g_ipc);
 inherited;
end;

//

Procedure TGameRunContext.Stop();
begin
 if (FGameProcess<>nil) then
 begin
  FGameProcess.stop;
 end;
end;

procedure TGameRunContext.StopAndNil();
begin
 if (FGameProcess<>nil) then
 begin
  FGameProcess.stop;
  FGameProcess.Release;
  FGameProcess:=nil;
 end;
end;

Procedure TGameRunContext.CloseItem();
begin
 if (FGameItem<>nil) then
 begin
  FGameItem.FLock:=False;
  FGameItem:=nil;
 end;
end;

//

function TGameRunContext.KEV_EVENT(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 kev:p_kevent;
 count:Integer;

 i:Integer;
begin
 Result:=0;

 kev  :=Value.GetBuf;
 count:=Value.GetLen div sizeof(t_kevent);

 i:=0;
 while (i<>count) do
 begin
  case kev[i].filter of
   EVFILT_PROC:
     begin
      if ((kev[i].fflags and NOTE_EXIT)<>0) then
      begin
       //ShowMessage('NOTE_EXIT pid:'+IntToStr(kev[i].ident));
       ShowMessage('The process reported exit!');
       Stop();
      end;
      if ((kev[i].fflags and NOTE_EXEC)<>0) then
      begin
       //ShowMessage('NOTE_EXEC pid:'+IntToStr(kev[i].ident));
       FOnRunned(nil);
       //SetButtonsState(mdsRunned);
      end;
     end;

   else;
  end;

  Inc(i);
 end;

end;

//

function TGameRunContext.ERROR(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
 if (MessageDlgEx(Value.GetString,'Error',[mbOK,mbAbort],FParent)=mrAbort) then
 begin
  Stop();
 end;
end;

function TGameRunContext.WARNING(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 i:Integer;
begin
 i:=MessageDlgEx(Value.GetString,'Warning',[mbYes,mbNo,mbAbort],FParent);
 if (i=mrAbort) then
 begin
  Stop();
 end;
 if (i=mrYes) then
 begin
  i:=0;
 end;
 Result:=i;
end;

//

function LoadParamSfoFile2(const game:RawByteString):TParamSfoFile;
begin
 Result:=LoadParamSfoFile(ExcludeTrailingPathDelimiter(game)+
                          DirectorySeparator+
                          'sce_sys'+
                          DirectorySeparator+
                          'param.sfo');
end;

function TGameRunContext.PARAM_SFO_INIT(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 V:RawByteString;
begin
 Result:=0;

 if (FGameItem=nil) then Exit;

 if (FParamSfo=nil) then
 begin
  FParamSfo:=LoadParamSfoFile2(FGameItem.MountList.game);
 end;

 if (FParamSfo=nil) then
 begin
  V:='"{$GAME}/sce_sys/param.sfo" not found, continue?';

  if (MessageDlgEx(V,'Error',[mbOK,mbAbort],FParent)=mrOK) then
  begin
   Exit(0);
  end else
  begin
   FOnStop(nil);
   Exit(0);
  end;
 end;

 Result:=TIpcValue.&Object(FParamSfo);
end;

//

function TGameRunContext.PLAYGO_INIT(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 playgo_file:TPlaygoFile;
 V:RawByteString;
begin
 Result:=0;

 if (FGameItem=nil) then Exit;

 V:=FGameItem.MountList.game;

 playgo_file:=LoadPlaygoFile(ExcludeTrailingPathDelimiter(V)+
                             DirectorySeparator+
                             'sce_sys'+
                             DirectorySeparator+
                             'playgo-chunk.dat');

 if (playgo_file=nil) then
 begin
  V:='"{$GAME}/sce_sys/playgo-chunk.dat" not found, continue?';

  if (MessageDlgEx(V,'Error',[mbOK,mbAbort],FParent)=mrOK) then
  begin
   Exit(0);
  end else
  begin
   FOnStop(nil);
   Exit(0);
  end;
 end;

 Result:=TIpcValue.&Object(playgo_file);
 FreeAndNil(playgo_file);
end;

//

function TGameRunContext.GameProcessForked:Boolean;
begin
 Result:=False;
 if (FGameProcess<>nil) then
 begin
  Result:=FGameProcess.g_fork;
 end;
end;

//

Procedure TGameRunContext.CloseSavdata();
begin
 if (FSaveData<>nil) then
 begin
  FSaveData.ExitProcess;
 end;
 FreeAndNil(FSaveData);
 //
 md_pipe_close(FSdClient);
 FSdClient:=0;
end;

function TGameRunContext.FetchSavdata:TSaveDataBackendConnect;
begin
 if (FSaveData=nil) then
 begin
  FSaveData:=TSaveDataBackendConnect.CreateProcess(
               FIpcDispatch,
               StdInputHandle,
               hOutput,
               hOutput,
               FConfigInfo.LogInfo.LogFilter);
 end;
 Result:=FSaveData;
end;

function TGameRunContext.OpenSaveDataBackend(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 Backend:TSaveDataBackendConnect;
 data:TPipeSend;
begin
 Backend:=FetchSavdata;

 md_pipe_close(FSdClient);
 FSdClient:=0;

 data.parent_pid:=GetProcessID;
 data.pipe_fd   :=Backend.NewClient();

 FSdClient:=data.pipe_fd; //let's put it off

 Result:=TIpcValue.Static(@data,SizeOf(data));
end;

//

end.

