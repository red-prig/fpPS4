unit md_game_process;

{$mode ObjFPC}{$H+}

interface

uses
 windows,
 ntapi,
 host_ipc;

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
 data:array[0..SizeOf(PROCESS_BASIC_INFORMATION)-1+7] of Byte;
 p_info:PPROCESS_BASIC_INFORMATION;
begin
 p_info:=Align(@data,8);
 p_info^:=Default(PROCESS_BASIC_INFORMATION);

 NtQueryInformationProcess(g_proc,
                           ProcessBasicInformation,
                           p_info,
                           SizeOf(PROCESS_BASIC_INFORMATION),
                           nil);

 Result:=p_info^.ExitStatus;
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
 inherited;
 NtTerminateProcess(g_proc,0);
end;

Destructor TGameProcessPipe.Destroy;
begin
 CloseHandle(g_proc);
 CloseHandle(FChildpip);
 inherited;
end;


end.

