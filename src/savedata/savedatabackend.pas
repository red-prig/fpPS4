unit SaveDataBackend;

{$mode objfpc}{$H+}

interface

uses
 sys_crt_gui,
 LFQueue,
 windows,
 md_event,
 host_ipc,
 md_host_ipc,
 md_pipe,
 md_systm,
 md_systm_fork,
 SceSaveData;

type
 TSaveDataBackendConnect=class
  kipc    :THostIpcPipeKERN;
  hProcess:THandle;
  fork_pid:Integer;
  Constructor Create;
  Destructor Destroy; override;
 end;

 TCustomCommand=class;

 PQNode=^TQNode;
 TQNode=object
  next_:PQNode;
  self_:TCustomCommand;
 end;

 TCustomCommand=class
  node:TQNode;
  Constructor Create;
  procedure   Run; virtual;
 end;

 TSaveDataBackendProcess=class
  ppid  :Integer;
  parent:THandle;
  kipc  :THostIpcPipeKERN;
  queue :TIntrusiveMPSCQueue;
  event :t_event;
  Constructor Create;
  procedure   SendCmd(cmd:TCustomCommand);
  function    OnExitProc(Value:TIpcValue):TIpcValue; //EXIT_PROC
 end;

implementation

var
 gSaveDataBackendProcess:TSaveDataBackendProcess=nil;

type
 PForkData=^TForkData;
 TForkData=record
  pipefd:THandle;
 end;

procedure savedata_process(data:Pointer;size:QWORD); SysV_ABI_CDecl; forward;

Constructor TSaveDataBackendConnect.Create;
var
 kern2svdt:array[0..1] of THandle;
 fork_info:t_fork_proc;
 data:TForkData;
 r:DWORD;
begin

 r:=md_pipe2(@kern2svdt,MD_PIPE_ASYNC0 or MD_PIPE_ASYNC1);
 if (r<>0) then
 begin
  Writeln('failed md_pipe2:0x',HexStr(r,8));
  Assert(false,'TSaveDataBackend');
 end;

 kipc:=THostIpcPipeKERN.Create;
 kipc.set_pipe(kern2svdt[0]);

 data.pipefd:=kern2svdt[1];

 fork_info.hInput :=GetStdHandle(STD_INPUT_HANDLE);
 fork_info.hOutput:=GetStdHandle(STD_OUTPUT_HANDLE);
 fork_info.hError :=GetStdHandle(STD_ERROR_HANDLE);

 fork_info.proc:=@savedata_process;
 fork_info.data:=@data;
 fork_info.size:=sizeof(data);

 r:=md_fork_process(fork_info,0);

 if (r<>0) then
 begin
  Writeln('failed md_fork_process:0x',HexStr(r,8));
  Assert(false,'TSaveDataBackend');
 end;

 hProcess:=fork_info.hProcess;
 fork_pid:=fork_info.fork_pid;
end;

Destructor TSaveDataBackendConnect.Destroy;
begin
 kipc.InvokeAsyn('EXIT_PROC');
 //
 md_waitpidfd(hProcess,nil);
 md_pidfd_close(hProcess);
 //
 kipc.Free;
 inherited;
end;

///

Constructor TCustomCommand.Create;
begin
 node.self_:=self;
end;

procedure TCustomCommand.Run;
begin
 //
end;

//

type
 TCmdExitProc=class(TCustomCommand)
  procedure   Run; override;
 end;

procedure TCmdExitProc.Run;
begin
 Writeln('savedata_process stopped pid:',md_getpid,' parent_pid:',gSaveDataBackendProcess.ppid);

 Halt;
end;

//

Constructor TSaveDataBackendProcess.Create;
begin
 queue.Create;
 ev_init(event,'event');
 //
 kipc:=THostIpcPipeKERN.Create;
 kipc.FHandler:=THostIpcHandler.Create;
 //
 kipc.FHandler.AddCallback('EXIT_PROC',@OnExitProc);
 //
 inherited;
end;

procedure TSaveDataBackendProcess.SendCmd(cmd:TCustomCommand);
begin
 if (cmd=nil) then Exit;

 queue.Push(@cmd.node);

 ev_signal(event);
end;

function TSaveDataBackendProcess.OnExitProc(Value:TIpcValue):TIpcValue; //EXIT_PROC
begin
 Result:=0;
 kipc.Disconnect();
 SendCmd(TCmdExitProc.Create);
end;

///

function wait_parent(parameter:pointer):ptrint;
begin
 sys_crt_gui.sys_crt_init;

 Result:=md_waitpidfd(gSaveDataBackendProcess.parent,nil);

 if (Result<>0) then
 begin
  Writeln('failed md_waitpidfd:0x',HexStr(Result,8));
  Assert(false,'savedata_process');
 end;

 gSaveDataBackendProcess.OnExitProc(Default(TIpcValue));
end;

procedure savedata_process(data:Pointer;size:QWORD); SysV_ABI_CDecl;
var
 ppid:Integer;

 pipefd:THandle;
 parent:THandle;

 node:PQNode;
 cmd:TCustomCommand;
begin
 //while not IsDebuggerPresent do sleep(100);

 pipefd:=PForkData(data)^.pipefd;

 //free shared
 FreeMem(data);

 ppid:=md_getppid;

 Writeln('savedata_process started pid:',md_getpid,' parent_pid:',ppid);

 parent:=md_pidfd_open(ppid);

 //dup
 pipefd:=md_pidfd_getfd(parent,pipefd);

 gSaveDataBackendProcess:=TSaveDataBackendProcess.Create;
 gSaveDataBackendProcess.kipc.set_pipe(pipefd);

 gSaveDataBackendProcess.ppid  :=ppid  ;
 gSaveDataBackendProcess.parent:=parent;

 //////////////

 BeginThread(@wait_parent,nil);

 repeat
  ev_wait(gSaveDataBackendProcess.event);

  node:=nil;
  while gSaveDataBackendProcess.queue.Pop(node) do
  begin
   cmd:=node^.self_;
   cmd.Run;
   cmd.Free;
  end;

 until false;

end;


end.

