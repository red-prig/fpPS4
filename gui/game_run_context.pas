unit game_run_context;

{$mode ObjFPC}{$H+}

interface

uses
 SysUtils,
 core_serialization,
 host_ipc,
 game_info,
 param_sfo_gui;

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

type
 PGameRunContext=^TGameRunContext;
 TGameRunContext=object
  FGameItem   :TGameItem;
  FGameProcess:TGameProcess;
  FParamSfo   :TParamSfoFile;
  //
  Procedure Stop();
  procedure StopAndNil();
  Procedure CloseItem();
  //
  procedure BindHandler(Handler:THostIpcHandler);
  function  InvokeSync(const msg:RawByteString;obj:TSerializeObject):Ptruint;
  procedure InvokeAsyn(const msg:RawByteString;obj:TSerializeObject);
  procedure InvokeAsyn(const msg:RawByteString;buf:Pointer;mlen:DWORD);
 end;

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

//

procedure TGameRunContext.BindHandler(Handler:THostIpcHandler);
begin
 if (FGameProcess=nil) or (Handler=nil) then Exit;
 if (FGameProcess.g_ipc<>nil) then
 begin
  FGameProcess.g_ipc.FHandler:=Handler;
 end;
end;

function TGameRunContext.InvokeSync(const msg:RawByteString;obj:TSerializeObject):Ptruint;
begin
 Result:=Ptruint(-1);
 if (FGameProcess<>nil) then
 if (FGameProcess.g_ipc<>nil) then
 begin
  Result:=FGameProcess.g_ipc.InvokeSync2(msg,TIpcValue.&Object(obj));
 end;
end;

procedure TGameRunContext.InvokeAsyn(const msg:RawByteString;obj:TSerializeObject);
begin
 if (FGameProcess<>nil) then
 if (FGameProcess.g_ipc<>nil) then
 begin
  FGameProcess.g_ipc.InvokeAsyn(msg,TIpcValue.&Object(obj));
 end;
end;

procedure TGameRunContext.InvokeAsyn(const msg:RawByteString;buf:Pointer;mlen:DWORD);
begin
 if (FGameProcess<>nil) then
 if (FGameProcess.g_ipc<>nil) then
 begin
  FGameProcess.g_ipc.InvokeAsyn(msg,buf,mlen);
 end;
end;

end.

