unit game_run_context;

{$mode ObjFPC}{$H+}

interface

uses
 classes,
 SysUtils,
 game_process,
 md_pipe,
 host_ipc,
 game_info,
 param_sfo_gui,
 playgo_chunk_gui,
 SaveDataBackend;

 {$M+}

 type
  TGameRunContext=class
   public
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
    //
    procedure DoGameRunned; virtual;
    procedure DoGameStop; virtual;
    procedure DoLoadExec(const data:TPS4LoadExec); virtual;
    function  DoShowError(const msg:RawByteString):Integer; virtual;
    function  DoShowWarning(const msg:RawByteString):Integer; virtual;
    procedure DoProcessExitMsg; virtual;
   published
    function  KEV_EVENT     (Client:THostIpc;Value:TIpcValue):TIpcValue;
    function  ERROR         (Client:THostIpc;Value:TIpcValue):TIpcValue;
    function  WARNING       (Client:THostIpc;Value:TIpcValue):TIpcValue;
    function  PARAM_SFO_INIT(Client:THostIpc;Value:TIpcValue):TIpcValue;
    function  PLAYGO_INIT   (Client:THostIpc;Value:TIpcValue):TIpcValue;
    function  OpenSaveDataBackend(Client:THostIpc;Value:TIpcValue):TIpcValue;
    function  LOAD_EXEC      (Client:THostIpc;Value:TIpcValue):TIpcValue;
   end;

 {$M-}

function LoadParamSfoFile2(const game:RawByteString):TParamSfoFile;

implementation

uses
 Controls,
 sys_event;

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
       DoProcessExitMsg;
       Stop();
      end;
       if ((kev[i].fflags and NOTE_EXEC)<>0) then
       begin
        DoGameRunned;
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
 if (DoShowError(Value.GetString)=mrAbort) then
 begin
  Stop();
 end;
end;

function TGameRunContext.WARNING(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 i:Integer;
begin
 i:=DoShowWarning(Value.GetString);
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

  if (DoShowError(V)=mrOK) then
  begin
   Exit(0);
  end else
  begin
   DoGameStop;
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

  if (DoShowError(V)=mrOK) then
  begin
   Exit(0);
  end else
  begin
   DoGameStop;
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

function TGameRunContext.LOAD_EXEC(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 data:TPS4LoadExec;
begin
 Result:=0;

 data:=TPS4LoadExec(Value.GetObject(TPS4LoadExec));
 if (data=nil) then Exit;

 if (FGameItem=nil) or
    (FGameProcess=nil) then
 begin
  FreeAndNil(data);
  Exit;
 end;

 if (UpperCase(data.Path)='EXIT') then
 begin
  FreeAndNil(data);
  Stop();
  Exit;
 end;

 if GameProcessForked then
 begin
  //terminate
  StopAndNil();

  DoLoadExec(data);
 end else
 begin
  DoShowError('LoadExec is not supported for the current process');
 end;

 FreeAndNil(data);
end;

//

procedure TGameRunContext.DoGameRunned;
begin
 //
end;

procedure TGameRunContext.DoGameStop;
begin
 //
end;

procedure TGameRunContext.DoLoadExec(const data:TPS4LoadExec);
begin
 //
end;

function TGameRunContext.DoShowError(const msg:RawByteString):Integer;
begin
 Result:=mrOK;
end;

function TGameRunContext.DoShowWarning(const msg:RawByteString):Integer;
begin
 Result:=mrNo;
end;

procedure TGameRunContext.DoProcessExitMsg;
begin
 //
end;

//

end.
