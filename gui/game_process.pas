unit game_process;

{$mode ObjFPC}{$H+}

interface

uses
 SysUtils,
 host_ipc;

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

procedure ReleaseAndNil(var obj:TGameProcess);

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

end.
