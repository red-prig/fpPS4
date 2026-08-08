unit game_run_context;

{$mode ObjFPC}{$H+}

interface

uses
 md_pipe,
 SysUtils,
 core_serialization,
 host_ipc,
 game_info,
 param_sfo_gui,
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
   FIpcDispatch:THostIpcDispatchGui;
   //
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
   Procedure CloseSavdata();
   function  FetchSavdata:TSaveDataBackendConnect;
  published
   function  OpenSaveDataBackend(Client:THostIpc;Value:TIpcValue):TIpcValue;
 end;

{$M-}

implementation

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
  FSaveData:=TSaveDataBackendConnect.CreateProcess(FIpcDispatch);
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

