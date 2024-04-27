unit md_game_process;

{$mode ObjFPC}{$H+}

interface

uses
 windows,
 ntapi,
 game_info;

type
 TGameProcessPipe=class(TGameProcess)
  FChildpip:THandle;
  function   is_terminated:Boolean; override;
  function   exit_code:DWORD; override;
  procedure  suspend; override;
  procedure  resume;  override;
  procedure  stop;    override;
  Destructor Destroy; override;
 end;

implementation

function TGameProcessPipe.is_terminated:Boolean;
var
 R:DWORD;
 T:QWORD;
begin
 T:=0;
 R:=NtWaitForSingleObject(g_proc,False,@T);

 Result:=(R=STATUS_WAIT_0);
end;

function TGameProcessPipe.exit_code:DWORD;
var
 info:PROCESS_BASIC_INFORMATION;
begin
 info:=Default(PROCESS_BASIC_INFORMATION);

 NtQueryInformationProcess(g_proc,
                           ProcessBasicInformation,
                           @info,
                           SizeOf(info),
                           nil);

 Result:=info.ExitStatus;
end;

procedure TGameProcessPipe.suspend;
begin
 NtSuspendProcess(g_proc);
end;

procedure TGameProcessPipe.resume;
begin
 NtResumeProcess(g_proc);
end;

procedure TGameProcessPipe.stop;
begin
 NtTerminateProcess(g_proc,0);
end;

Destructor TGameProcessPipe.Destroy;
begin
 CloseHandle(g_proc);
 CloseHandle(FChildpip);
 inherited;
end;


end.

