unit SaveDataBackend;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 classes,
 mqueue,
 errno,
 LFQueue,
 //windows,
 md_event,
 kern_mtx,
 kern_proc,
 host_ipc,
 md_host_ipc,
 md_pipe,
 md_systm,
 md_systm_fork,
 game_mount,
 vfs_mountroot,
 ps4_libSceUserService,
 SceSaveData,
 SaveDataBackendSfo,
 SaveDataBackendUtils,
 SaveDataKeystone;

type
 TPipeSend=record
  parent_pid:Int64;
  pipe_fd   :THandle;
 end;

 TSaveDataBackendConnect=class
  kipc      :THostIpcPipe;
  //
  hProcess  :THandle;
  fork_pid  :Integer;
  //
  MountSlots:DWORD;
  //
  Constructor CreateProcess(_Dispatcher:THostIpcDispatcher);
  Constructor CreateClient (_Dispatcher:THostIpcDispatcher;_pipefd:THandle);
  Destructor  Destroy; override;
  procedure   SendMountConfig(Config:TGameMountConfigExport);
  function    NewClient():THandle;
  procedure   ExitClient;
  procedure   ExitProcess;
  procedure   UmountAllForce;
  function    DoDelete       (del:pSceSaveDataDelete):Integer;
  function    DoMount        (mount:pSceSaveDataMount;pResult:pSceSaveDataMountResult;Transfering,Internal:Boolean):Integer;
  function    DoMountSys     (mount:pSceSaveDataMount;var slot_id:Integer):Integer;
  function    DoUmount       (slot_id:Integer;backup:boolean):Integer;
  function    DoUmountSys    (slot_id:Integer):Integer;
  function    GetMountInfo   (slot_id:Integer;info:pSceSaveDataMountInfo):Integer;
  function    GetMountInfoSys(slot_id:Integer;info:pSceSaveDataMountInfo):Integer;
  function    DoBackup       (backup:pSceSaveDataBackup):Integer;
  function    CheckBackup    (check:pSceSaveDataCheckBackupData):Integer;
  function    RestoreBackup  (restore:pSceSaveDataRestoreBackupData):Integer;
  function    GetEventResult (event:pSceSaveDataEvent):Integer;
  function    GetProgress    (p_progress:PSingle):Integer;
  function    ClearProgress  ():Integer;
  function    SaveIcon       (slot_id:Integer;icon:pSceSaveDataIcon):Integer;
  function    LoadIcon       (slot_id:Integer;icon:pSceSaveDataIcon;internal:Boolean):Integer;
  function    SetParam       (slot_id     :Integer;
                              paramType   :SceSaveDataParamType;
                              paramBuf    :Pointer;
                              paramBufSize:QWORD):Integer;
  function    GetParam       (slot_id     :Integer;
                              paramType   :SceSaveDataParamType;
                              paramBuf    :Pointer;
                              paramBufSize:QWORD;
                              gotSize     :PQWORD):Integer;
  function    SetupMemory    (userId        :SceUserServiceUserId;
                              slotId        :Integer;
                              bufferNum     :Integer;
                              memorySize    :DWORD;
                              iconMemorySize:DWORD;
                              paramSize     :DWORD;
                              InitParams    :pSceSaveDataParam
                             ):Integer;
  function    ReadMemory     (slot_id:Integer;dataBuf:Pointer;dataSize:DWORD;p_existedMemorySize:PQWORD):Integer;
  procedure   WriteMemory    (userId,slotId,bufferId:DWORD;addr:Pointer;size:DWORD);
  function    SyncMemory     (syncParam:pSceSaveDataMemorySync):Integer;
  function    DirNameSearch  (cond    :pSceSaveDataDirNameSearchCond;
                              pResult :pSceSaveDataDirNameSearchResult;
                              internal:Boolean):Integer;
 end;

 TSaveDataClient=class;

 TCustomCommand=class
  type
   PQNode=^TQNode;
   TQNode=object
    next_:PQNode;
    self_:TCustomCommand;
   end;
  var
   node  :TQNode;
   Client:TSaveDataClient;
   rid   :THostIpcResult;
   defer :Boolean;
  Constructor Create(_Client:TSaveDataClient;_rid:THostIpcResult);
  Destructor  Destroy; override;
  function    Run:TIpcValue; virtual;
  procedure   Invoke(value:TIpcValue);
  function    GetProgress:Single; virtual;
 end;

 TIpcEventDispatch=class(THostIpcDispatchQueue)
  event:t_event;
  Constructor Create(_Handler:THostIpcHandler);
  Destructor  Destroy; override;
  procedure   QueuePush(node:PQNode); override;
 end;

 TProgressInfo=record
  mtx  :mtx;
  cmd  :TCustomCommand; //Mount, Delete, RestoreBackupData
  Value:Single;
 end;

 TSaveDataBackendProcess=class;

 TSaveDataClient=class(THostIpcPipe)
  //
  entry:TAILQ_ENTRY;
  //
  sdk_version:DWORD;
  systemLang :DWORD;
  //
  GameMountConfig:TGameMountConfig;
  //
  MountManager:TMountManager;
  //
  SetupMemoryManager:TSetupMemoryManager;
  pWriteSlot:PSetupMemoryNode;
  //
  EventQueue:TEventQueue;
  //
  Progress:TProgressInfo;
  //
  IconData   :Pointer;
  iconBufSize:Ptrint;
  //
  Keystone:p_keystone_file;
  //
  Constructor Create(_Dispatcher:THostIpcDispatcher);
  Destructor  Destroy; override;
  procedure   UmountAllForce(Backend:TSaveDataBackendProcess);
  function    GetProgress(var p:Single):Integer;
  procedure   ClearProgress();
  procedure   SetProgressJob(cmd:TCustomCommand);
  function    LoadPkgIcon    (var _iconData:Pointer):Ptrint;
  function    LoadPkgKeystone():p_keystone_file;
 end;

 TClientManager=object
  List:TAILQ_HEAD;
  mtx :mtx;
  //
  procedure Init;
  function  NewClient(Dispatcher:TIpcEventDispatch):TSaveDataClient;
  procedure FreeClient(Client:TSaveDataClient);
  procedure InvokeBrokenAll;
  procedure DisconnectAll;
  procedure UmountAllForce(Backend:TSaveDataBackendProcess);
 end;

 {$M+}

 TSaveDataBackendProcess=class
  public
   ppid      :Integer;
   parent    :THandle;
   //
   Dispatcher:TIpcEventDispatch;
   Clients   :TClientManager;
   //
   job_queue :TIntrusiveMPSCQueue;
   job_event :t_event;
   //
   LockDirManager:TLockDirManager;
   //
   Constructor Create;
   procedure   SendCmd         (cmd:TCustomCommand);
   function    RecvCmd         (var cmd:TCustomCommand):Boolean;
   procedure   DoExit          ();
   function    SendBackupJob   (Client     :TSaveDataClient;
                                userId     :SceUserServiceUserId;
                                titleId    :pchar;
                                dirName    :pchar;
                                fingerprint:pSceSaveDataFingerprint;
                                event_type :Byte):Integer;
   function    SendSyncJob     (Client:TSaveDataClient;userId,slotId:DWORD;is_async,is_event:Boolean):Integer;
  published
   //All functions available for the IPC
   function    Confirm       (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    ExitProcess   (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    NewClient     (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    ExitClient    (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    MountConfig   (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    Delete        (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    Mount         (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    IsActiveMount (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    Umount        (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    GetMountInfo  (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    Backup        (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    CheckBackup   (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    RestoreBackup (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    GetEventResult(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    GetProgress   (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    ClearProgress (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    SaveIcon      (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    LoadIcon      (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    SetParam      (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    GetParam      (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    SetupMemory   (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    ReadMemory    (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    SetWriteSlot  (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    WriteMemory   (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    SyncMemory    (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
   function    DirNameSearch (Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
 end;

 {$M-}

implementation

//

var
 gSaveDataBackend:TSaveDataBackendProcess=nil;

type
 PForkData=^TForkData;
 TForkData=record
  pipefd:THandle;
 end;

procedure savedata_process(data:Pointer;size:QWORD); SysV_ABI_CDecl; forward;

Constructor TSaveDataBackendConnect.CreateProcess(_Dispatcher:THostIpcDispatcher);
var
 kern2svdt:t_pipe_pair;
 fork_info:t_fork_proc;
 data:TForkData;
 r:DWORD;
begin
 inherited;

 r:=md_pipe2(kern2svdt,MD_PIPE_ASYNC0 or MD_PIPE_ASYNC1);
 if (r<>0) then
 begin
  Writeln('failed md_pipe2:0x',HexStr(r,8));
  Assert(false,'TSaveDataBackend');
 end;

 //THostIpcConnect(p_host_ipc).Dispatcher

 kipc:=THostIpcPipe.Create(_Dispatcher);
 kipc.set_pipe(kern2svdt[0]);

 data.pipefd:=kern2svdt[1];

 fork_info.hInput :=StdInputHandle ;
 fork_info.hOutput:=StdOutputHandle;
 fork_info.hError :=StdErrorHandle ;

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

 kipc.InvokeSync2('Confirm');

 //The handle has been copied by another process, close it
 md_pipe_close(data.pipefd);
end;

Constructor TSaveDataBackendConnect.CreateClient(_Dispatcher:THostIpcDispatcher;_pipefd:THandle);
begin
 inherited;
 kipc:=THostIpcPipe.Create(_Dispatcher);
 kipc.set_pipe(_pipefd);
end;

procedure TSaveDataBackendConnect.ExitClient;
begin
 kipc.InvokeAsyn('ExitClient');
end;

procedure TSaveDataBackendConnect.ExitProcess;
begin
 kipc.InvokeAsyn('ExitProcess');
 //
 md_waitpidfd(hProcess,nil);
 md_pidfd_close(hProcess);
end;

Destructor TSaveDataBackendConnect.Destroy;
begin
 //
 kipc.Free;
 //
 inherited;
end;

///

Constructor TIpcEventDispatch.Create(_Handler:THostIpcHandler);
begin
 inherited;
 ev_init(event,'THostIpcPipeSave');
end;

Destructor TIpcEventDispatch.Destroy;
begin
 ev_destroy(event);
 inherited;
end;

procedure TIpcEventDispatch.QueuePush(node:PQNode);
begin
 inherited;
 ev_signal(event);
end;

///

Constructor TCustomCommand.Create(_Client:TSaveDataClient;_rid:THostIpcResult);
begin
 node.self_:=self;
 Client:=_Client;
 rid:=_rid;
 if (Client<>nil) then
 begin
  Client.Acquire;
 end;
end;

Destructor TCustomCommand.Destroy;
begin
 FreeAndNil(rid);
 //
 if (Client<>nil) then
 begin
  Client.Release;
 end;
 //
 inherited;
end;

function TCustomCommand.Run:TIpcValue;
begin
 Result:=0;
end;

procedure TCustomCommand.Invoke(value:TIpcValue);
begin
 if (rid=nil) then
 begin
  value.Free;
 end else
 begin
  rid.InvokeResult(value);
  FreeAndNil(rid);
 end;
end;

function TCustomCommand.GetProgress:Single;
begin
 Result:=0;
end;

//

type
 TCmdExitProc=class(TCustomCommand)
  function Run:TIpcValue; override;
 end;

function TCmdExitProc.Run:TIpcValue;
begin
 Result:=0;
 Writeln('savedata_process stopped pid:',GetProcessID,' parent_pid:',gSaveDataBackend.ppid);

 Halt;
end;

////

function wait_parent(parameter:pointer):ptrint;
begin
 Result:=md_waitpidfd(gSaveDataBackend.parent,nil);

 if (Result<>0) then
 begin
  Writeln('failed md_waitpidfd:0x',HexStr(Result,8));
  Assert(false,'savedata_process');
 end;

 gSaveDataBackend.DoExit();
end;

function job_thread(parameter:pointer):ptrint;
var
 cmd:TCustomCommand;
 Value:TIpcValue;
begin
 Result:=0;
 repeat
  ev_wait(gSaveDataBackend.job_event);

  cmd:=nil;
  while gSaveDataBackend.RecvCmd(cmd) do
  begin
   Value:=cmd.Run;
   if cmd.defer then
   begin
    cmd.defer:=False;
    Value.Free;
    gSaveDataBackend.SendCmd(cmd);
   end else
   begin
    cmd.Invoke(Value);
    cmd.Free;
   end;
  end;

 until false;
end;

//

Constructor TSaveDataClient.Create(_Dispatcher:THostIpcDispatcher);
begin
 inherited;
 //
 GameMountConfig:=TGameMountConfig.Create;
 //
 SetupMemoryManager.Init;
 //
 EventQueue.Init;
 //
 mtx_init(Progress.mtx,'Progress');
 //
end;

Destructor TSaveDataClient.Destroy;
begin
 //
 SetupMemoryManager.Free;
 //
 FreeAndNil(GameMountConfig);
 //
 if (IconData<>nil) then FreeMem(IconData);
 //
 if (Keystone<>nil) then FreeMem(Keystone);
 //
 inherited;
end;

//


procedure TClientManager.Init;
begin
 TAILQ_INIT(@List);
 mtx_init(mtx,'TClientManager');
end;

function TClientManager.NewClient(Dispatcher:TIpcEventDispatch):TSaveDataClient;
begin
 Result:=TSaveDataClient.Create(Dispatcher);
 //
 mtx_lock(mtx);
  TAILQ_INSERT_TAIL(@List,Result,@Result.entry);
 mtx_unlock(mtx);
end;

procedure TClientManager.FreeClient(Client:TSaveDataClient);
begin
 mtx_lock(mtx);
  TAILQ_REMOVE(@List,Client,@Client.entry);
 mtx_unlock(mtx);

 Client.Release;
end;

procedure TClientManager.InvokeBrokenAll;
var
 node:TSaveDataClient;
begin
 mtx_lock(mtx);
  node:=TSaveDataClient(TAILQ_FIRST(@List));
  while (node<>nil) do
  begin
   node.InvokeBroken();
   //
   node:=TSaveDataClient(TAILQ_NEXT(node,@node.entry));
  end;
 mtx_unlock(mtx);
end;

procedure TClientManager.DisconnectAll;
var
 node,next:TSaveDataClient;
begin
 mtx_lock(mtx);
  node:=TSaveDataClient(TAILQ_FIRST(@List));
  while (node<>nil) do
  begin
   next:=TSaveDataClient(TAILQ_NEXT(node,@node.entry));
   //
   node.Disconnect;
   TAILQ_REMOVE(@List,node,@node.entry);
   node.Release;
   //
   node:=next;
  end;
 mtx_unlock(mtx);
end;

procedure TClientManager.UmountAllForce(Backend:TSaveDataBackendProcess);
var
 node,next:TSaveDataClient;
begin
 mtx_lock(mtx);
  node:=TSaveDataClient(TAILQ_FIRST(@List));
  while (node<>nil) do
  begin
   next:=TSaveDataClient(TAILQ_NEXT(node,@node.entry));
   //
   node.UmountAllForce(Backend);
   //
   node:=next;
  end;
 mtx_unlock(mtx);
end;

//

procedure OnExitProc;
begin
 gSaveDataBackend.Clients.InvokeBrokenAll;
end;

procedure savedata_process(data:Pointer;size:QWORD); SysV_ABI_CDecl;
var
 ppid:Integer;

 pipefd:THandle;
 parent:THandle;

 kipc:TSaveDataClient;
begin
 //while not IsDebuggerPresent do sleep(100);

 pipefd:=PForkData(data)^.pipefd;

 //free shared
 FreeMem(data);

 ppid:=md_getppid;

 Writeln('savedata_process started pid:',GetProcessID,' parent_pid:',ppid);

 parent:=md_pidfd_open(ppid);

 //dup
 pipefd:=md_pidfd_getfd(parent,pipefd);

 gSaveDataBackend:=TSaveDataBackendProcess.Create;

 kipc:=gSaveDataBackend.Clients.NewClient(gSaveDataBackend.Dispatcher);
 kipc.set_pipe(pipefd);

 gSaveDataBackend.ppid  :=ppid  ;
 gSaveDataBackend.parent:=parent;

 //////////////

 AddExitProc(@OnExitProc);

 BeginThread(@wait_parent,nil);
 BeginThread(@job_thread,nil);

 repeat
  ev_wait(gSaveDataBackend.Dispatcher.event);

  gSaveDataBackend.Dispatcher.Update();
 until false;

end;

////

Constructor TSaveDataBackendProcess.Create;
begin
 inherited;
 //
 Clients.Init;
 //
 job_queue.Create;
 ev_init(job_event,'job_event');
 //
 LockDirManager.Init;
 //
 Dispatcher:=TIpcEventDispatch.Create(THostIpcHandler.Create);
 Dispatcher.Acquire;
 //
 Dispatcher.FHandler.AddPublished(Self);
 //
end;

procedure TSaveDataBackendProcess.SendCmd(cmd:TCustomCommand);
begin
 if (cmd=nil) then Exit;
 job_queue.Push(@cmd.node);
 ev_signal(job_event);
end;

function TSaveDataBackendProcess.RecvCmd(var cmd:TCustomCommand):Boolean;
var
 node:TCustomCommand.PQNode;
begin
 node:=nil;
 Result:=job_queue.Pop(node);
 if Result then
 begin
  cmd:=node^.self_;
 end;
end;

function TSaveDataBackendProcess.Confirm(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
end;

procedure TSaveDataBackendProcess.DoExit();
begin
 Clients.DisconnectAll;
 Clients.UmountAllForce(Self);
 SendCmd(TCmdExitProc.Create(nil,nil));
end;

function TSaveDataBackendProcess.ExitProcess(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
 DoExit();
end;

function TSaveDataBackendConnect.NewClient():THandle;
var
 kern2svdt:t_pipe_pair;
 data:TPipeSend;
 r:DWORD;
begin
 r:=md_pipe2(kern2svdt,MD_PIPE_ASYNC0 or MD_PIPE_ASYNC1);
 if (r<>0) then
 begin
  Writeln('failed md_pipe2:0x',HexStr(r,8));
  Assert(false,'TSaveDataBackend');
 end;

 Result:=kern2svdt[0];

 data.parent_pid:=GetProcessID;
 data.pipe_fd   :=kern2svdt[1];

 kipc.InvokeSync2('NewClient',TIpcValue.Static(@data,SizeOf(data)));

 //The handle has been copied by another process, close it
 md_pipe_close(data.pipe_fd);
end;

function TSaveDataBackendProcess.NewClient(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 data  :TPipeSend;
 procfd:THandle;
 kipc  :TSaveDataClient;
begin
 Result:=0;

 data:=Default(TPipeSend);
 Value.MoveTo(@data,sizeof(data));

 Writeln('NewClient started pid:',GetProcessID,' parent_pid:',data.parent_pid);

 procfd:=md_pidfd_open(data.parent_pid);

 //dup
 data.pipe_fd:=md_pidfd_getfd(procfd,data.pipe_fd);

 md_pidfd_close(procfd);

 kipc:=Clients.NewClient(Dispatcher);
 kipc.set_pipe(data.pipe_fd);
end;

function TSaveDataBackendProcess.ExitClient(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
 Writeln('ExitClient stopped pid:',GetProcessID,' parent_pid:',ppid);

 Client.Disconnect();
 Client.UmountAllForce(Self);
 Clients.FreeClient(Client);
end;

procedure TSaveDataBackendConnect.SendMountConfig(Config:TGameMountConfigExport);
begin
 if (Config=nil) then Exit;

 kipc.InvokeAsyn('MountConfig',TIpcValue.&Object(Config));

 FreeAndNil(Config);
end;

procedure TSaveDataBackendConnect.UmountAllForce;
var
 slot_id:Integer;
begin
 if (MountSlots<>0) then
 For slot_id:=0 to TMountManager.max-1 do
 if (MountSlots and (DWORD(1) shl slot_id))<>0 then
 begin
  Writeln('Force umount ', mount_savedata_slot_name[slot_id]);
  vfs_mountroot.unmount_from_sandbox(pchar(mount_savedata_slot_name[slot_id]),MNT_FORCE);
 end;
end;

function TSaveDataBackendProcess.MountConfig(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 data:TGameMountConfigExport;
begin
 Result:=0;

 data:=TGameMountConfigExport(Value.GetObject(TGameMountConfigExport));
 if (data=nil) then Exit;

 Client.sdk_version:=data.sdk_version;
 Client.systemLang :=data.systemLang ;

 Client.GameMountConfig.ATTRIBUTE   :=data.ATTRIBUTE;
 Client.GameMountConfig.Game        :=data.Game;
 Client.GameMountConfig.LocalDir    :=data.LocalDir;
 Client.GameMountConfig.TransferList:=data.TransferList;
 Client.GameMountConfig.TitleId     :=data.TitleId;
 Client.GameMountConfig.InstallDir  :=data.InstallDir;

 Writeln('[MOUNT_CONFIG]');
 Writeln(' sdk_version =0x',HexStr(data.sdk_version,8));
 Writeln(' systemLang  =0x',HexStr(data.systemLang,8));
 Writeln(' ATTRIBUTE   =0x',HexStr(data.ATTRIBUTE,8));
 Writeln(' Game        =',data.Game);
 Writeln(' LocalDir    =',data.LocalDir);
 Writeln(' TransferList=',data.TransferList);
 Writeln(' TitleId     =',data.TitleId);
 Writeln(' InstallDir  =',data.InstallDir);

 FreeAndNil(data);
end;

///

type
 TDelete=packed record
  userId :SceUserServiceUserId;
  titleId:SceSaveDataTitleId;
  dirName:SceSaveDataDirName;
 end;

function TSaveDataBackendConnect.DoDelete(del:pSceSaveDataDelete):Integer;
var
 data:TDelete;
begin
 FillChar(data,SizeOf(data),0);
  data.userId :=del^.userId;
 if (del^.titleId<>nil) then
  data.titleId:=del^.titleId^;
 if (del^.dirName<>nil) then
  data.dirName:=del^.dirName^;

 Result:=kipc.InvokeSync2('Delete',@data,sizeof(data));
end;

type
 TCustomDirJob=class(TCustomCommand)
  //
  user_id:Integer;
  titleId:SceSaveDataTitleId;
  dirName:SceSaveDataDirName;
  //
  fs_src:RawByteString;
  fs_dst:RawByteString;
  fs_old:RawByteString;
  fs_new:RawByteString;
  //
  procedure Init(_user_id:Integer;_titleId,_dirName:pchar);
  procedure UnLock;
  procedure DoDelete(p_progress:PSingle;add:Single);
 end;

procedure TCustomDirJob.Init(_user_id:Integer;_titleId,_dirName:pchar);
begin
 user_id:=_user_id;
 strlcopy(@titleId.data,_titleId,SCE_SAVE_DATA_TITLE_ID_DATA_SIZE);
 strlcopy(@dirName.data,_dirName,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE);
 //
 fs_src:=Client.GameMountConfig.GetSaveDataFolder   (_user_id,_titleId,_dirName);
 fs_dst:=Client.GameMountConfig.GetSaveDataBackupDst(_user_id,_titleId,_dirName);
 fs_old:=Client.GameMountConfig.GetSaveDataBackupOld(_user_id,_titleId,_dirName);
 fs_new:=Client.GameMountConfig.GetSaveDataBackupNew(_user_id,_titleId,_dirName);
end;

procedure TCustomDirJob.UnLock;
begin
 gSaveDataBackend.LockDirManager.UnLockDir(fs_src);
end;

procedure TCustomDirJob.DoDelete(p_progress:PSingle;add:Single);
begin
 //dont check errors
 game_mount.DeleteDirectory(fs_dst,False); if (p_progress<>nil) then p_progress^:=p_progress^+add;
 game_mount.DeleteDirectory(fs_old,False); if (p_progress<>nil) then p_progress^:=p_progress^+add;
 game_mount.DeleteDirectory(fs_new,False); if (p_progress<>nil) then p_progress^:=p_progress^+add;
 game_mount.DeleteDirectory(fs_src,False); if (p_progress<>nil) then p_progress^:=p_progress^+add;
end;

///

type
 TDeleteJob=class(TCustomDirJob)
  Progress:Single;
  function  Run:TIpcValue; override;
  function  GetProgress:Single; override;
 end;

function TDeleteJob.Run:TIpcValue;
begin
 Result:=0;

 Progress:=1/5;

 DoDelete(@Progress,1/5);

 Client.SetProgressJob(nil);
 Unlock;
end;

function TDeleteJob.GetProgress:Single;
begin
 Result:=Progress;
end;

function TSaveDataBackendProcess.Delete(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 data:TDelete;

 titleId:pchar;
 dirName:pchar;
 fs_src :RawByteString;

 job:TDeleteJob;
begin
 Result:=0;
 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 titleId:=@data.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@Client.GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 if Client.MountManager.IsActiveMount(data.userId,titleId,dirName) then
 begin
  Result:=SCE_SAVE_DATA_ERROR_BUSY;
 end else
 begin

  fs_src:=Client.GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

  if LockDirManager.LockDir(fs_src) then
  begin
   job:=TDeleteJob.Create(Client,Client.HoldResult);
   job.Init(data.userId,titleId,dirName);

   Client.SetProgressJob(job);
   SendCmd(job);
  end else
  begin
   Result:=SCE_SAVE_DATA_ERROR_BACKUP_BUSY;
  end;

 end;

end;

type
 TMount=packed record
  userId     :SceUserServiceUserId;
  titleId    :SceSaveDataTitleId;
  dirName    :SceSaveDataDirName;
  fingerprint:SceSaveDataFingerprint;
  blocks     :SceSaveDataBlocks;
  mountMode  :WORD; //SceSaveDataMountMode
  Transfering:Boolean;
  Internal   :Boolean;
 end;

 TMountResult=packed record
  result        :Integer;
  mountStatus   :WORD;
  slot_id       :WORD;
  requiredBlocks:SceSaveDataBlocks;
 end;

function TSaveDataBackendConnect.DoMount(mount:pSceSaveDataMount;pResult:pSceSaveDataMountResult;Transfering,Internal:Boolean):Integer;
var
 data:TMount;
 Value:TIpcValue;
 output:TMountResult;

 titleId:pchar;
 fs_src :RawByteString;
begin
 FillChar(data,SizeOf(data),0);
  data.userId     :=mount^.userId;
 if (mount^.titleId<>nil) then
  data.titleId    :=mount^.titleId^;
 if (mount^.dirName<>nil) then
  data.dirName    :=mount^.dirName^;
 if (mount^.fingerprint<>nil) then
  data.fingerprint:=mount^.fingerprint^;
 data.blocks      :=mount^.blocks;
 data.mountMode   :=mount^.mountMode;
 data.Transfering :=Transfering;
 data.Internal    :=Internal;

 Value:=kipc.InvokeSync('Mount',TIpcValue.Static(@data,sizeof(data)));

 FillChar(output,SizeOf(output),0);
 Value.MoveTo(@output,SizeOf(output));

 Value.Free;

 Result:=output.result;

 if (Result=0) then
 begin

  titleId:=@data.titleId.data;
  if (titleId[0]=#0) then
  begin
   titleId:=@gGameMountConfig.InstallDir;
  end;

  fs_src:=gGameMountConfig.GetSaveDataFolder(data.userId,titleId,@data.dirName.data);

  Result:=vfs_mountroot.mount_into_sandbox('ufs',
                                           pchar(mount_savedata_slot_name[output.slot_id]),
                                           pchar(fs_src),
                                           nil,
                                           ord((data.mountMode and SDMM_RDONLY)<>0)*MNT_RDONLY or
                                           MNT_PFS_32K);
  if (Result=0) then
  begin
   MountSlots:=MountSlots or (DWORD(1) shl output.slot_id);
  end else
  begin
   Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
   //Umount
   kipc.InvokeSync2('Umount',@output.slot_id,sizeof(output.slot_id));
  end;

 end;

 if (Result=0) then
 begin
  //out
  pResult^.mountPoint    :=mount_savedata_slot_name[output.slot_id];
  pResult^.requiredBlocks:=output.requiredBlocks;

  if (p_proc.p_sdk_version < $3500000) then
  begin
   //
  end else
  begin
   pResult^.mountStatus:=output.mountStatus;
  end;
 end;

end;

function TSaveDataBackendConnect.DoMountSys(mount:pSceSaveDataMount;var slot_id:Integer):Integer;
var
 data:TMount;
 Value:TIpcValue;
 output:TMountResult;
begin
 FillChar(data,SizeOf(data),0);
  data.userId     :=mount^.userId;
 if (mount^.titleId<>nil) then
  data.titleId    :=mount^.titleId^;
 if (mount^.dirName<>nil) then
  data.dirName    :=mount^.dirName^;
 if (mount^.fingerprint<>nil) then
  data.fingerprint:=mount^.fingerprint^;
 data.blocks      :=mount^.blocks;
 data.mountMode   :=mount^.mountMode;
 data.Internal    :=True;

 Value:=kipc.InvokeSync('Mount',TIpcValue.Static(@data,sizeof(data)));

 FillChar(output,SizeOf(output),0);
 Value.MoveTo(@output,SizeOf(output));

 Value.Free;

 Result:=output.result;

 slot_id:=output.slot_id
end;

type
 TMountJob=class(TCustomCommand)
  //
  fs_src:RawByteString;
  data  :TMount;
  //
  param_sfo:t_savedata_sfo_values;
  //
  mtime:QWORD;
  Progress:Single;
  requiredBlocks:QWORD;
  //
  procedure Init(const _data:TMount);
  function  Lock():Boolean;
  procedure UnLock();
  function  CreateParamSfo():Boolean;
  function  OpenParamSfo():Integer;
  function  SaveParamSfo():Integer;
  function  MountParamSfo():Integer;
  function  CreateTmpFiles():Boolean;
  function  CreateKeystone():Boolean;
  function  OpenKeystone():Integer;
  function  CheckMountData(is_created:Boolean):Integer;
  function  CreateMount():Integer;
  function  OpenMount():Integer;
  //
  function  Run:TIpcValue; override;
  function  GetProgress:Single; override;
 end;

procedure TMountJob.Init(const _data:TMount);
begin
 data:=_data;
end;

function TMountJob.Lock():Boolean;
begin
 Result:=gSaveDataBackend.LockDirManager.LockDir(fs_src);
end;

procedure TMountJob.UnLock();
begin
 gSaveDataBackend.LockDirManager.UnLockDir(fs_src);
end;

function TMountJob.CreateParamSfo():Boolean;
var
 fname:RawByteString;
begin
 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys');
 Result:=ForceDirectories(fname);
 if not Result then Exit;

 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/param.sfo');

 param_sfo.New(Client.GameMountConfig,data.userId,@data.titleId.data,@data.dirName.data,data.blocks,Client.systemLang);

 if ((data.mountMode and SDMM_RDWR)<>0) then
 if ((data.mountMode and SDMM_DESTRUCT_OFF)=0) then
 begin
  param_sfo.PARAMS.corrupt_flag:=1;
 end;

 Result:=param_sfo.SaveToFile(fname);
end;

function TMountJob.OpenParamSfo():Integer;
var
 fname:RawByteString;
begin
 Result:=0;

 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/param.sfo');

 if not param_sfo.LoadFromFile(fname) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 if not param_sfo.Verif(data.userId,@data.dirName.data) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 //update blocks
 data.blocks:=param_sfo.SAVEDATA_BLOCKS;
end;

function TMountJob.SaveParamSfo():Integer;
var
 fname:RawByteString;
begin
 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/param.sfo');

 if param_sfo.SaveToFile(fname) then
 begin
  Exit(0);
 end else
 begin
  Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
 end;
end;

function TMountJob.MountParamSfo():Integer;
var
 titleId:PChar;
begin
 Result:=0;

 //update data to sfo
 if ((data.mountMode and SDMM_RDWR)<>0) then
 begin
  //
  titleId:=@data.titleId.data;
  if (titleId[0]=#0) then
  begin
   titleId:=@Client.GameMountConfig.TitleId;
  end;

  strlcopy(@param_sfo.PARAMS.title_id_2,titleId,SCE_SAVE_DATA_TITLE_ID_DATA_SIZE);
  Inc(param_sfo.PARAMS.RETAIL_counter1);

  //mark in-mount
  if ((data.mountMode and SDMM_DESTRUCT_OFF)=0) then
  begin
   param_sfo.PARAMS.corrupt_flag:=1;
  end;

  Result:=SaveParamSfo();
  //
 end;
end;

function TMountJob.CreateTmpFiles():Boolean;
var
 fname:RawByteString;
begin
 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys');
 Result:=ForceDirectories(fname);
 if not Result then Exit;

 if (Client.sdk_version<$4500000) then
 begin
  fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/sce_paramsfo1');
  Result:=TruncFile(fname,$8000);
  if not Result then Exit;
 end;

 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/sce_icon0png0');
 Result:=TruncFile(fname,$1c800);
 if not Result then Exit;

 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/sce_icon0png1');
 Result:=TruncFile(fname,$1c800);
end;

function TMountJob.CreateKeystone():Boolean;
var
 fname:RawByteString;
 app_keystone:p_keystone_file;
begin
 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys');
 Result:=ForceDirectories(fname);
 if not Result then Exit;

 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/keystone');

 app_keystone:=Client.LoadPkgKeystone();

 Result:=WriteToFile(fname,app_keystone,SizeOf(t_keystone_file))=SizeOf(t_keystone_file);
end;

function TMountJob.OpenKeystone():Integer;
var
 fname:RawByteString;
 keystone_app:p_keystone_file;
 keystone_msd:t_keystone_file;
begin
 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/keystone');

 keystone_msd:=Default(t_keystone_file);

 if (ReadFromFile(fname,@keystone_msd,SizeOf(t_keystone_file))<>sizeof(t_keystone_file)) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 if (data.fingerprint.data[0]=#0) then
 begin
  Result:=sceSblSsCheckKeystone(@keystone_msd);

  if (Result<>0) then
  begin
   Result:=SCE_SAVE_DATA_ERROR_BROKEN;
  end else
  begin
   keystone_app:=Client.LoadPkgKeystone();

   if (CompareByte(keystone_msd,keystone_app^,SizeOf(t_keystone_file))<>0) then
   begin
    Result:=SCE_SAVE_DATA_ERROR_BROKEN;
   end;
  end;

 end else
 begin
  Result:=sceSblSsVerifyKeystone(@keystone_msd,@data.fingerprint.data);

  if (Result<>0) then
  begin
   Result:=SCE_SAVE_DATA_ERROR_FINGERPRINT_MISMATCH;
  end;

 end;

end;

function TMountJob.CheckMountData(is_created:Boolean):Integer;
var
 titleId:pchar;
begin
 Result:=0;

 if (Client.sdk_version < $3000000) then
 begin
  //
 end else
 if (not is_created) or
    ((Client.GameMountConfig.ATTRIBUTE and $80000)<>0) or
    (data.blocks < 32769) then
 begin
  //
 end else
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 if data.Transfering then
 begin

  titleId:=@data.titleId.data;
  if (titleId[0]=#0) then
  begin
   titleId:=@Client.GameMountConfig.InstallDir;
  end;

  if not Client.GameMountConfig.InTransferList(titleId) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_PARAMSFO_TRANSFER_TITLE_ID_NOT_FOUND);
  end;

 end;

end;

function TSaveDataClient.LoadPkgIcon(var _iconData:Pointer):Ptrint;
var
 ficon:RawByteString;
begin
 if (IconData<>nil) then
 begin
  _iconData:=IconData;
  Exit(iconBufSize);
 end;

 ficon:=ExcludeTrailingPathDelimiter(GameMountConfig.Game)+unix_to_host('/sce_sys/save_data.png');

 iconData:=AllocMem($1C800);

 iconBufSize:=ReadFromFile(ficon,iconData,$1C800);

 _iconData:=IconData;
 Exit(iconBufSize);
end;

function TSaveDataClient.LoadPkgKeystone():p_keystone_file;
var
 fkeystone:RawByteString;
begin
 if (Keystone<>nil) then
 begin
  Exit(Keystone);
 end;

 fkeystone:=ExcludeTrailingPathDelimiter(GameMountConfig.Game)+unix_to_host('/sce_sys/keystone');

 Keystone:=AllocMem(SizeOf(t_keystone_file));

 if (ReadFromFile(fkeystone,Keystone,SizeOf(t_keystone_file))<>sizeof(t_keystone_file)) then
 begin
  Writeln('Warning: /app0/sce_sys/keystone not loaded -> fill to fake pkg keystone');
  Keystone^:=fake_pkg_keystone;
 end;

 Exit(Keystone);
end;

function TMountJob.CreateMount():Integer;
var
 free_size  :QWORD;
 iconData   :Pointer;
 iconBufSize:Ptrint;
begin
 Progress:=2/9;

 Result:=CheckMountData(True);
 if (Result<>0) then Exit;

 if not ForceDirectories(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
 end;

 Result:=GetFreeSpace(Client.GameMountConfig.LocalDir,free_size);
 if (Result<>0) then Exit;

 if (data.blocks*SCE_SAVE_DATA_BLOCK_SIZE)>free_size then
 begin
  requiredBlocks:=data.blocks - (free_size div SCE_SAVE_DATA_BLOCK_SIZE) + 1;
  Exit(SCE_SAVE_DATA_ERROR_NO_SPACE_FS);
 end;

 Progress:=3/9;

 if not CreateParamSfo then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 Progress:=4/9;

 if not CreateTmpFiles then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 Progress:=5/9;

 if not CreateKeystone then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 Progress:=6/9;

 if ((data.mountMode and SDMM_COPY_ICON)<>0) then
 begin
  iconBufSize:=Client.LoadPkgIcon(iconData);

  if (iconBufSize<=0) then
  begin
   //
  end else
  begin
   if CheckPng(iconData,iconBufSize)=0 then
   begin
    SaveIcon(fs_src,iconData,iconBufSize);
   end;
  end;

 end;

 Progress:=7/9;

 update_mtime(fs_src,mtime);
end;

function TMountJob.OpenMount():Integer;
begin
 Progress:=2/9;

 Result:=CheckMountData(False);
 if (Result<>0) then Exit;

 Progress:=3/9;

 Result:=OpenParamSfo();
 if (Result<>0) then Exit;

 Progress:=4/9;

 if (param_sfo.PARAMS.fake_owner=0) then
 begin
  Result:=OpenKeystone();
  if (Result<>0) then Exit;
 end;

 Progress:=6/9;

 Result:=MountParamSfo();
 if (Result<>0) then Exit;

 Progress:=7/9;

 load_mtime(fs_src,mtime);
end;

function TMountJob.Run:TIpcValue;
var
 titleId:pchar;
 dirName:pchar;

 slot_id:Integer;
 output:TMountResult;

 minfo:TMountSlot;

 is_locked:Boolean;
begin
 output:=Default(TMountResult);

 if (Client.sdk_version < $1700000) then
 begin
  data.mountMode:=data.mountMode and (not SDMM_COPY_ICON);
 end;

 if (Client.sdk_version < $4500000) then
 begin
  data.mountMode:=data.mountMode and (not SDMM_CREATE2);
 end;

 if (data.fingerprint.data[0]<>#0) and ((data.mountMode and SDMM_RDONLY)=0) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 titleId:=@data.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@Client.GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 is_locked:=False;
 slot_id:=0;

 output.result:=Client.MountManager.GetFreeSlotId(data.userId,
                                                  titleId,
                                                  dirName,
                                                  data.Internal,
                                                  slot_id);

 Progress:=1/9;

 if (output.result=0) then
 begin

  fs_src:=Client.GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

  is_locked:=Lock();

  if not is_locked then
  begin
   output.result:=SCE_SAVE_DATA_ERROR_BACKUP_BUSY;
  end else
  begin
   if SaveDataExists(fs_src) then
   begin
    //open or exists error

    if ((data.mountMode and SDMM_CREATE2)<>0) then
    begin
     //open
     output.result:=OpenMount();
    end else
    if ((data.mountMode and SDMM_CREATE)<>0) then
    begin
     //error
     output.result:=SCE_SAVE_DATA_ERROR_EXISTS;
    end else
    begin
     //open
     output.result:=OpenMount();
    end;

   end else
   begin
    //create or not found error

    if ((data.mountMode and (SDMM_CREATE2 or SDMM_CREATE))<>0) then
    begin
     //create
     output.result:=CreateMount();
    end else
    begin
     //error
     output.result:=SCE_SAVE_DATA_ERROR_NOT_FOUND;
    end;

    if (output.result=0) and ((data.mountMode and SDMM_CREATE2)<>0) then
    begin
     output.mountStatus:=SCE_SAVE_DATA_MOUNT_STATUS_CREATED;
    end;

    if (output.result=SCE_SAVE_DATA_ERROR_NO_SPACE_FS) then
    begin
     output.requiredBlocks:=requiredBlocks;
    end;

   end;

   Progress:=8/9;

   if (output.result=0) then
   begin
    //save info
    minfo:=Default(TMountSlot);

    minfo.active:=1;
    minfo.userId:=data.userId;

    strncpy_s(@minfo.titleId.data,titleId,SCE_SAVE_DATA_TITLE_ID_DATA_SIZE  );
    strncpy_s(@minfo.dirName.data,dirName,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE);

    minfo.fingerprint:=data.fingerprint;
    minfo.max_blocks :=data.blocks;
    minfo.mountMode  :=data.mountMode;

    minfo.param_sfo  :=param_sfo;
    minfo.mtime      :=mtime;

    Client.MountManager.SetMount(slot_id,minfo);

    //out
    output.slot_id:=slot_id;
   end;

  end;

 end;

 Progress:=9/9;

 Client.SetProgressJob(nil);

 if (output.result<>0) and is_locked then
 begin
  Unlock;
 end;

 if (output.result=0) then
 begin
  Result:=TIpcValue.New(@output,sizeof(output));
 end else
 begin
  Result:=output.result;
 end;

end;

function TMountJob.GetProgress:Single;
begin
 Result:=Progress;
end;

function TSaveDataBackendProcess.Mount(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 data:TMount;

 job:TMountJob;
begin
 Result:=0;
 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 job:=TMountJob.Create(Client,Client.HoldResult);
 job.Init(data);

 Client.SetProgressJob(job);
 SendCmd(job);
end;

type
 TUmount=record
  slot_id:Integer;
  backup :boolean;
 end;

function TSaveDataBackendConnect.DoUmount(slot_id:Integer;backup:boolean):Integer;
var
 data:TUmount;
begin
 if (DWORD(slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if (MountSlots and (DWORD(1) shl slot_id))=0 then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 data.slot_id:=slot_id;
 data.backup :=backup;

 Result:=kipc.InvokeSync2('IsActiveMount',@data.slot_id,sizeof(data.slot_id));

 if (Result=0) then
 begin

  Result:=vfs_mountroot.unmount_from_sandbox(pchar(mount_savedata_slot_name[slot_id]),0);
  if (Result<>0) then
  begin
   case Result of
    EBUSY:Result:=SCE_SAVE_DATA_ERROR_BUSY;
    else
          Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
   end;
  end else
  begin
   MountSlots:=MountSlots and (not (DWORD(1) shl slot_id));

   //free
   Result:=kipc.InvokeSync2('Umount',@data,sizeof(data));
  end;

 end;

end;

function TSaveDataBackendConnect.DoUmountSys(slot_id:Integer):Integer;
var
 data:TUmount;
begin
 if (DWORD(slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 data.slot_id:=slot_id;
 data.backup :=False;

 Result:=kipc.InvokeSync2('IsActiveMount',@data.slot_id,sizeof(data.slot_id));

 if (Result=0) then
 begin
  //free
  Result:=kipc.InvokeSync2('Umount',@data,sizeof(data));
 end;

end;

type
 TMountInfo=packed record
  result    :QWORD;
  blocks    :SceSaveDataBlocks;
  freeBlocks:SceSaveDataBlocks;
 end;

function TSaveDataBackendProcess.IsActiveMount(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 slot_id:Integer;
begin
 Result:=0;
 slot_id:=Value.GetDWORD;

 if (DWORD(slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not Client.MountManager.IsActiveMount(slot_id) then
 begin
  Result:=SCE_SAVE_DATA_ERROR_NOT_MOUNTED;
 end else
 begin
  Result:=0;
 end;

end;

type
 TUmountJob=class(TCustomCommand)
  //
  fs_src:RawByteString;
  //
  minfo:TMountSlot;
  data :TUmount;
  force:Boolean;
  //
  procedure UnLock;
  function  UmountParamSfo():Integer;
  function  Run:TIpcValue; override;
 end;

procedure TUmountJob.UnLock;
begin
 gSaveDataBackend.LockDirManager.UnLockDir(fs_src);
end;

function TUmountJob.UmountParamSfo():Integer;
var
 fname:RawByteString;
begin
 Result:=0;

 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/param.sfo');

 //update data to sfo
 if ((minfo.mountMode and SDMM_RDWR)<>0) then
 begin
  //mark in-free
  if ((minfo.mountMode and SDMM_DESTRUCT_OFF)=0) then
  begin
   minfo.param_sfo.PARAMS.corrupt_flag:=0;
  end;

  if not minfo.param_sfo.SaveToFile(fname) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
  end;
  //
 end;
end;

function TUmountJob.Run:TIpcValue;
var
 titleId:pchar;
 dirName:pchar;

 err:Integer;
begin

 if not Client.MountManager.IsActiveMount(data.slot_id) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 titleId:=@minfo.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@Client.GameMountConfig.InstallDir;
 end;

 dirName:=@minfo.dirName.data;

 fs_src:=Client.GameMountConfig.GetSaveDataFolder(minfo.userId,titleId,dirName);

 //
 err:=0;

 if not force then
 begin
  err:=UmountParamSfo();
 end;

 if (err=0) then
 begin
  //free
  Client.MountManager.FreeMount(data.slot_id);

  Unlock;
 end;

 if (not force) and ((minfo.mountMode and SDMM_RDWR)<>0) then
 begin
  update_mtime(fs_src,minfo.mtime);
 end;

 if (err=0) and data.backup and ((minfo.mountMode and SDMM_RDWR)<>0) then
 begin
  gSaveDataBackend.SendBackupJob(Client,
                                 minfo.userId,
                                 @minfo.titleId,
                                 @minfo.dirName,
                                 @minfo.fingerprint,
                                 SDET_UMOUNT_BACKUP_END);
 end;

 Result:=err;
end;

function TSaveDataBackendProcess.Umount(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 data:TUmount;
 job:TUmountJob;
begin
 Result:=0;
 data:=Default(TUmount);
 Value.MoveTo(@data,SizeOf(data));

 if (DWORD(data.slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not Client.MountManager.IsActiveMount(data.slot_id) then
 begin
  Result:=SCE_SAVE_DATA_ERROR_NOT_MOUNTED;
 end else
 begin
  Result:=0;

  job:=TUmountJob.Create(Client,Client.HoldResult);

  job.minfo:=Client.MountManager.GetMount(data.slot_id);
  job.data :=data;

  SendCmd(job);
 end;

end;

procedure TSaveDataClient.UmountAllForce(Backend:TSaveDataBackendProcess);
var
 i:Integer;
 job:TUmountJob;
begin

 For i:=0 to MountManager.max-1 do
 if MountManager.IsActiveMount(i) then
 begin
  job:=TUmountJob.Create(Self,nil);

  job.minfo:=MountManager.GetMount(i);
  job.data.slot_id:=i;
  job.force:=True;

  Backend.SendCmd(job);
 end;

end;

function TSaveDataBackendConnect.GetMountInfo(slot_id:Integer;info:pSceSaveDataMountInfo):Integer;
var
 Value:TIpcValue;
 data:TMountInfo;
begin
 if (DWORD(slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if (MountSlots and (DWORD(1) shl slot_id))=0 then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 Value:=kipc.InvokeSync('GetMountInfo',slot_id);

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 Value.Free;

 Result:=data.result;

 if (Result=0) then
 begin
  info^.blocks    :=data.blocks    ;
  info^.freeBlocks:=data.freeBlocks;
 end;
end;

function TSaveDataBackendConnect.GetMountInfoSys(slot_id:Integer;info:pSceSaveDataMountInfo):Integer;
var
 Value:TIpcValue;
 data:TMountInfo;
begin
 if (DWORD(slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 Value:=kipc.InvokeSync('GetMountInfo',slot_id);

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 Value.Free;

 Result:=data.result;

 if (Result=0) then
 begin
  info^.blocks    :=data.blocks    ;
  info^.freeBlocks:=data.freeBlocks;
 end;
end;

function TSaveDataBackendProcess.GetMountInfo(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 slot_id:Integer;
 minfo  :TMountSlot;
 fs_src :RawByteString;
 output :TMountInfo;
 blocks :Int64;
begin
 Result:=0;
 slot_id:=Value.GetDWORD;

 if (DWORD(slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not Client.MountManager.IsActiveMount(slot_id) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 minfo:=Client.MountManager.GetMount(slot_id);

 fs_src:=Client.GameMountConfig.GetSaveDataFolder(minfo.userId,@minfo.titleId,@minfo.dirName);

 blocks:=GetFreeBlocks(fs_src,minfo.max_blocks);

 output.result    :=0;
 output.blocks    :=minfo.max_blocks;
 output.freeBlocks:=blocks;

 Result:=TIpcValue.New(@output,sizeof(output));
end;

type
 TBackup=packed record
  userId     :SceUserServiceUserId;
  titleId    :SceSaveDataTitleId;
  dirName    :SceSaveDataDirName;
  fingerprint:SceSaveDataFingerprint;
 end;

function TSaveDataBackendConnect.DoBackup(backup:pSceSaveDataBackup):Integer;
var
 data:TBackup;
begin
 FillChar(data,SizeOf(data),0);
  data.userId     :=backup^.userId;
 if (backup^.titleId<>nil) then
  data.titleId    :=backup^.titleId^;
 if (backup^.dirName<>nil) then
  data.dirName    :=backup^.dirName^;
 if (backup^.fingerprint<>nil) then
  data.fingerprint:=backup^.fingerprint^;

 Result:=kipc.InvokeSync2('Backup',@data,sizeof(data));
end;

function TSaveDataBackendProcess.Backup(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 data:TBackup;
 titleId:pchar;
begin
 Result:=0;
 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 titleId:=@data.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@Client.GameMountConfig.InstallDir;
 end;

 Result:=SendBackupJob(Client,
                       data.userId,
                       titleId,
                       @data.dirName.data,
                       @data.fingerprint,
                       SDET_BACKUP_END);
end;

type
 TCustomBackupJob=class(TCustomDirJob)
  //
  param_sfo  :t_savedata_sfo_values;
  fingerprint:SceSaveDataFingerprint;
  Progress   :Single;
  //
  function OpenParamSfo(const fdir:RawByteString):Integer;
  function OpenKeystone(const fdir:RawByteString):Integer;
  function Prepare:Boolean;
  function CheckBackup(check_keystone:Boolean):Integer;
  function GetProgress:Single; override;
 end;

 TBackupJob=class(TCustomBackupJob)
  event_type:Byte;
  function Backup:Integer;
  function OpenBackup:Integer;
  function Run:TIpcValue; override;
 end;

 TRestoreJob=class(TCustomBackupJob)
  function Restore:Boolean;
  function Run:TIpcValue; override;
 end;

function TCustomBackupJob.OpenParamSfo(const fdir:RawByteString):Integer;
var
 fname:RawByteString;
begin
 Result:=0;

 fname:=ExcludeTrailingPathDelimiter(fdir)+unix_to_host('/sce_sys/param.sfo');

 if not param_sfo.LoadFromFile(fname) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 if not param_sfo.Verif(user_id,@dirName.data) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

end;

function TCustomBackupJob.OpenKeystone(const fdir:RawByteString):Integer;
var
 fname:RawByteString;
 keystone_app:p_keystone_file;
 keystone_msd:t_keystone_file;
begin
 fname:=ExcludeTrailingPathDelimiter(fdir)+unix_to_host('/sce_sys/keystone');

 keystone_msd:=Default(t_keystone_file);

 if (ReadFromFile(fname,@keystone_msd,SizeOf(t_keystone_file))<>sizeof(t_keystone_file)) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 if (fingerprint.data[0]=#0) then
 begin
  Result:=sceSblSsCheckKeystone(@keystone_msd);

  if (Result<>0) then
  begin
   Result:=SCE_SAVE_DATA_ERROR_BROKEN;
  end else
  begin
   keystone_app:=Client.LoadPkgKeystone();

   if (CompareByte(keystone_msd,keystone_app^,SizeOf(t_keystone_file))<>0) then
   begin
    Result:=SCE_SAVE_DATA_ERROR_BROKEN;
   end;
  end;

 end else
 begin
  Result:=sceSblSsVerifyKeystone(@keystone_msd,@fingerprint.data);

  if (Result<>0) then
  begin
   Result:=SCE_SAVE_DATA_ERROR_FINGERPRINT_MISMATCH;
  end;

 end;

end;

function TCustomBackupJob.Prepare:Boolean;
begin
 Result:=False;

 Progress:=1/12;

 if DirectoryExists(fs_old) and (not DirectoryExists(fs_dst)) then
 begin
  //rollback an unfinished transaction
  if RenameFile(fs_old,fs_dst) then
  begin
   Writeln('rollback an unfinished transaction:',{$INCLUDE %LINENUM%});
  end else
  begin
   Writeln('RenameFile failed:',{$INCLUDE %LINENUM%});
   Exit;
  end;
 end;

 Progress:=2/12;

 //clear old
 if DirectoryExists(fs_old) then
 begin
  if game_mount.DeleteDirectory(fs_old,False) then
  begin
   //
  end else
  begin
   Writeln('DeleteDirectory failed:',{$INCLUDE %LINENUM%});
   Exit;
  end;
 end;

 Progress:=3/12;

 //clear new
 if DirectoryExists(fs_new) then
 begin
  if game_mount.DeleteDirectory(fs_new,False) then
  begin
   //
  end else
  begin
   Writeln('DeleteDirectory failed:',{$INCLUDE %LINENUM%});
   Exit;
  end;
 end;

 Progress:=4/12;

 Result:=True;
end;

function TCustomBackupJob.CheckBackup(check_keystone:Boolean):Integer;
begin
 if not SaveDataExists(fs_dst) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_FOUND);
 end;

 Progress:=5/12;

 Result:=OpenParamSfo(fs_dst);
 if (Result<>0) then Exit;

 Progress:=6/12;

 if check_keystone then
 if (param_sfo.PARAMS.fake_owner=0) then
 begin
  Result:=OpenKeystone(fs_dst);
  if (Result<>0) then Result:=SCE_SAVE_DATA_ERROR_BROKEN;
 end;
end;

function TCustomBackupJob.GetProgress:Single;
begin
 Result:=Progress;
end;

function TBackupJob.Backup:Integer;
var
 free_size:QWORD;
begin
 Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
 if not Prepare then Exit;

 Result:=GetFreeSpace(Client.GameMountConfig.LocalDir,free_size);
 if (Result<>0) then Exit;

 if (param_sfo.SAVEDATA_BLOCKS*SCE_SAVE_DATA_BLOCK_SIZE)>free_size then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NO_SPACE_FS);
 end;

 Result:=SCE_SAVE_DATA_ERROR_INTERNAL;

 //copy src->new
 if game_mount.CopyDirectory(fs_src,fs_new) then
 begin
  //
 end else
 begin
  Writeln('CopyDirectory failed:',{$INCLUDE %LINENUM%});
  Prepare;
  Exit;
 end;

 //move dst->old
 if DirectoryExists(fs_dst) then
 begin
  if RenameFile(fs_dst,fs_old) then
  begin
   //
  end else
  begin
   Writeln('RenameFile failed:',{$INCLUDE %LINENUM%});
   Prepare;
   Exit;
  end;
 end;

 //move new->dst
 if RenameFile(fs_new,fs_dst) then
 begin
  //
 end else
 begin
  Writeln('RenameFile failed:',{$INCLUDE %LINENUM%});
  Prepare;
  Exit;
 end;

 //delete old
 if DirectoryExists(fs_old) then
 begin
  if game_mount.DeleteDirectory(fs_old,False) then
  begin
   //
  end else
  begin
   Writeln('DeleteDirectory failed:',{$INCLUDE %LINENUM%});
   Exit;
  end;
 end;

 Result:=0;
end;

function TBackupJob.OpenBackup:Integer;
begin
 Result:=OpenParamSfo(fs_src);
 if (Result<>0) then Exit;

 if (param_sfo.PARAMS.fake_owner=0) then
 begin
  Result:=OpenKeystone(fs_src);
 end;
end;

function TBackupJob.Run:TIpcValue;
var
 err:Integer;
begin
 Result:=0;

 err:=Backup;

 if (event_type<>0) then
 begin
  Client.EventQueue.Push(event_type,err,user_id,@titleId,@dirName);
 end;

 ///
 UnLock;
end;

function TRestoreJob.Restore:Boolean;
var
 fs_tmp:RawByteString;
begin
 Result:=False;

 fs_tmp:=fs_src+'_tmp_cp0';

 Progress:=7/12;

 //delete files in tmp
 if game_mount.DeleteDirectory(fs_tmp,True) then
 begin
  //
 end else
 begin
  Writeln('DeleteDirectory failed:',{$INCLUDE %LINENUM%});
  Prepare;
  Exit;
 end;

 Progress:=8/12;

 //copy dst->tmp
 if game_mount.CopyDirectory(fs_dst,fs_tmp) then
 begin
  //
 end else
 begin
  Writeln('CopyDirectory failed:',{$INCLUDE %LINENUM%});
  Prepare;
  Exit;
 end;

 Progress:=9/12;

 //move src->new
 if RenameFile(fs_src,fs_new) then
 begin
  //
 end else
 begin
  Writeln('RenameFile failed:',{$INCLUDE %LINENUM%});
  Prepare;
  Exit;
 end;

 Progress:=10/12;

 //move tmp->src
 if RenameFile(fs_tmp,fs_src) then
 begin
  //
 end else
 begin
  Writeln('RenameFile failed:',{$INCLUDE %LINENUM%});
  Prepare;
  Exit;
 end;

 Progress:=11/12;

 //delete files in new
 if game_mount.DeleteDirectory(fs_new,False) then
 begin
  //
 end else
 begin
  Writeln('DeleteDirectory failed:',{$INCLUDE %LINENUM%});
  Prepare;
  Exit;
 end;

 Progress:=12/12;

 Result:=True;
end;

function TRestoreJob.Run:TIpcValue;
var
 free_size:QWORD;
 err:Integer;
begin

 if Prepare then
 begin
  err:=CheckBackup(True);
 end else
 begin
  err:=SCE_SAVE_DATA_ERROR_INTERNAL;
 end;

 if (err=0) then
 begin

  err:=GetFreeSpace(Client.GameMountConfig.LocalDir,free_size);
  if (err<>0) then Exit;

  if (param_sfo.SAVEDATA_BLOCKS*SCE_SAVE_DATA_BLOCK_SIZE)>free_size then
  begin
   Exit(SCE_SAVE_DATA_ERROR_NO_SPACE_FS);
  end;

  if Restore then
  begin
   err:=0;
  end else
  begin
   err:=SCE_SAVE_DATA_ERROR_INTERNAL;
  end;

 end;

 Result:=err;

 Client.SetProgressJob(nil);
 UnLock;
end;

function TSaveDataBackendProcess.SendBackupJob(Client     :TSaveDataClient;
                                               userId     :SceUserServiceUserId;
                                               titleId    :pchar;
                                               dirName    :pchar;
                                               fingerprint:pSceSaveDataFingerprint;
                                               event_type :Byte):Integer;
var
 fs_src:RawByteString;
 job:TBackupJob;
begin
 Result:=0;

 if Client.MountManager.IsActiveMount(userId,titleId,dirName) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BUSY);
 end;

 fs_src:=Client.GameMountConfig.GetSaveDataFolder(userId,titleId,dirName);

 if not SaveDataExists(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_FOUND);
 end;

 if not LockDirManager.LockDir(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BACKUP_BUSY);
 end;

 job:=TBackupJob.Create(Client,nil); //async
 job.Init(userId,titleId,dirName);
 job.event_type:=event_type;

 if (fingerprint<>nil) then
 begin
  job.fingerprint:=fingerprint^;
 end;

 Result:=job.OpenBackup();
 if (Result<>0) then
 begin
  job.UnLock;
  job.Free;
  Exit;
 end;

 SendCmd(job);
end;

type
 TCheckBackup=packed record
  userId   :SceUserServiceUserId;
  titleId  :SceSaveDataTitleId;
  dirName  :SceSaveDataDirName;
  get_param:Boolean;
  get_icon :Boolean;
 end;

 PCheckBackupOutput=^TCheckBackupOutput;
 TCheckBackupOutput=packed record
  result   :DWORD;
  params   :SceSaveDataParam;
  icon_size:DWORD;
  icon_data:record end;
 end;

function TSaveDataBackendConnect.CheckBackup(check:pSceSaveDataCheckBackupData):Integer;
var
 data:TCheckBackup;
 icon_size:DWORD;
 Value:TIpcValue;
 output:PCheckBackupOutput;
begin
 FillChar(data,SizeOf(data),0);
  data.userId     :=check^.userId;
 if (check^.titleId<>nil) then
  data.titleId    :=check^.titleId^;
 if (check^.dirName<>nil) then
  data.dirName    :=check^.dirName^;

 data.get_param:=(check^.param<>nil);
 data.get_icon :=(check^.icon <>nil);

 Value:=kipc.InvokeSync('CheckBackup',TIpcValue.Static(@data,sizeof(data)));

 Result:=Value.GetDWORD;

 if (Result=0) then
 begin
  output:=Value.GetBuf;

  if (check^.param<>nil) then
  if (Value.GetLen>=sizeof(TCheckBackupOutput)) then
  begin
   check^.param^:=output^.params;
  end;

  if (check^.icon<>nil) then
  begin
   icon_size:=Value.GetLen;
   if (icon_size<sizeof(TCheckBackupOutput)) then
   begin
    icon_size:=0;
   end else
   begin
    icon_size:=icon_size-sizeof(TCheckBackupOutput);
    if (icon_size>output^.icon_size) then icon_size:=output^.icon_size;
   end;

   if (icon_size <= check^.icon^.bufSize) then
   begin
    Move(output^.icon_data,check^.icon^.buf^,icon_size);
    check^.icon^.dataSize:=icon_size;
   end;
  end;

 end;

 Value.Free;
end;

type
 TCheckJob=class(TCustomBackupJob)
  //
  get_param:Boolean;
  get_icon :Boolean;
  //
  function Run:TIpcValue; override;
 end;

function TCheckJob.Run:TIpcValue;
var
 err :Integer;
 size:Ptrint;
 data:PCheckBackupOutput;
 ficon:RawByteString;
 mtime:QWORD;
begin
 Result:=0;

 if Prepare then
 begin
  err:=CheckBackup(False);
  if (err<>0) then Exit(err);

  if (not get_param) and (not get_icon) then
  begin
   Result:=err;
  end else
  begin

   data:=AllocMem($1C800*ord(get_icon)+sizeof(TCheckBackupOutput));

   if get_param then
   begin
    load_mtime(fs_dst,mtime);
    param_sfo.GetParam(SCE_SAVE_DATA_PARAM_TYPE_ALL,@data^.params,@size,mtime);
   end;

   size:=0;
   if get_icon then
   begin
    ficon:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/icon0.png');

    size:=ReadFromFile(ficon,@data^.icon_data,$1C800);

    if (size<=0) then
    begin
     size:=0;
    end else
    begin
     data^.icon_size:=size;
    end;

    Result:=TIpcValue.Inplace(data,data,size+sizeof(TCheckBackupOutput));
   end;

  end;

 end else
 begin
  Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
 end;

 UnLock;
end;

function TSaveDataBackendProcess.CheckBackup(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 data:TCheckBackup;
 titleId:pchar;
 dirName:pchar;

 fs_src:RawByteString;
 job:TCheckJob;
begin
 Result:=0;
 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 titleId:=@data.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@Client.GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 fs_src:=Client.GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

 if not SaveDataExists(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_FOUND);
 end;

 if not LockDirManager.LockDir(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BACKUP_BUSY);
 end;

 job:=TCheckJob.Create(Client,Client.HoldResult);
 job.Init(data.userId,titleId,dirName);

 job.get_param:=data.get_param;
 job.get_icon :=data.get_icon ;

 SendCmd(job);
end;

function TSaveDataBackendConnect.RestoreBackup(restore:pSceSaveDataRestoreBackupData):Integer;
var
 data:TBackup;
begin
 FillChar(data,SizeOf(data),0);
  data.userId     :=restore^.userId;
 if (restore^.titleId<>nil) then
  data.titleId    :=restore^.titleId^;
 if (restore^.dirName<>nil) then
  data.dirName    :=restore^.dirName^;
 if (restore^.fingerprint<>nil) then
  data.fingerprint:=restore^.fingerprint^;

 Result:=kipc.InvokeSync2('RestoreBackup',@data,sizeof(data));
end;

function TSaveDataBackendProcess.RestoreBackup(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 data:TBackup;
 titleId:pchar;
 dirName:pchar;

 fs_src:RawByteString;
 job:TRestoreJob;
begin
 Result:=0;
 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 titleId:=@data.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@Client.GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 if Client.MountManager.IsActiveMount(data.userId,titleId,dirName) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BUSY);
 end;

 fs_src:=Client.GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

 if not SaveDataExists(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_FOUND);
 end;

 if not LockDirManager.LockDir(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BACKUP_BUSY);
 end;

 job:=TRestoreJob.Create(Client,Client.HoldResult);
 job.Init(data.userId,titleId,dirName);
 job.fingerprint:=data.fingerprint;

 Client.SetProgressJob(job);
 SendCmd(job);
end;

type
 TEventResult=record
  result:qword;
  event :SceSaveDataEvent
 end;

function TSaveDataBackendConnect.GetEventResult(event:pSceSaveDataEvent):Integer;
var
 Value:TIpcValue;
 data:TEventResult;
begin
 Value:=kipc.InvokeSync('GetEventResult');

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 Result:=data.result;

 if (Result=0) then
 begin
  event^:=data.event;
 end;
end;

function TSaveDataBackendProcess.GetEventResult(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 data:TEventResult;
begin
 data:=Default(TEventResult);

 if Client.EventQueue.Pop(data.event) then
 begin
  Result:=TIpcValue.New(@data,SizeOf(data));
 end else
 begin
  Result:=SCE_SAVE_DATA_ERROR_NOT_FOUND;
 end;

end;

type
 TGetProgress=packed record
  result  :Integer;
  progress:Single;
 end;

function TSaveDataBackendConnect.GetProgress(p_progress:PSingle):Integer;
var
 Value:TIpcValue;
 data:TGetProgress;
begin
 Value:=kipc.InvokeSync('GetProgress');

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 Result:=data.result;

 if (Result=0) then
 begin
  p_progress^:=data.progress;
 end;
end;

function TSaveDataBackendProcess.GetProgress(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 data:TGetProgress;
begin
 data.result:=Client.GetProgress(data.progress);

 Result:=TIpcValue.New(@data,SizeOf(data));
end;

function TSaveDataBackendConnect.ClearProgress():Integer;
begin
 Result:=kipc.InvokeSync2('ClearProgress');
end;

function TSaveDataBackendProcess.ClearProgress(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
 Client.ClearProgress;
end;

function TSaveDataClient.GetProgress(var p:Single):Integer;
begin
 Result:=0;
 p:=0;

 mtx_lock(Progress.mtx);

  if (Progress.cmd=nil) then
  begin
   //
  end else
  begin
   Progress.Value:=Progress.cmd.GetProgress;
  end;

  p:=Progress.Value;

 mtx_unlock(Progress.mtx);
end;

procedure TSaveDataClient.ClearProgress();
begin
 mtx_lock(Progress.mtx);

  Progress.Value:=0;

 mtx_unlock(Progress.mtx);
end;

procedure TSaveDataClient.SetProgressJob(cmd:TCustomCommand);
begin
 mtx_lock(Progress.mtx);

  if (Progress.cmd<>nil) then
  begin
   Progress.Value:=Progress.cmd.GetProgress;
  end;

  Progress.cmd:=cmd;
  if (cmd<>nil) then
  begin
   Progress.Value:=0;
  end;

 mtx_unlock(Progress.mtx);
end;

function TSaveDataBackendConnect.SaveIcon(slot_id:Integer;icon:pSceSaveDataIcon):Integer;
var
 size:DWORD;
 data:p_input_buf;
begin
 size:=icon^.dataSize;
 data:=AllocMem(size+sizeof(t_input_buf));
 data^.slot:=slot_id;
 data^.size:=size;
 Move(icon^.buf^,data^.data,size);

 Result:=kipc.InvokeSync2('SaveIcon',TIpcValue.Inplace(data,data,size+sizeof(t_input_buf)));
end;

type
 TSaveIconJob=class(TCustomCommand)
  //
  fs_src:RawByteString;
  //
  slot:DWORD;
  len :DWORD;
  data:array[0..116735] of Byte;
  //
  function  Run:TIpcValue; override;
 end;

function TSaveIconJob.Run:TIpcValue;
var
 err:Integer;
 mtime:QWORD;
begin
 err:=CheckPng(@data,len);

 if (err=0) then
 begin
  if not SaveIcon(fs_src,@data,len) then
  begin
   err:=SCE_SAVE_DATA_ERROR_INTERNAL;
  end;
 end;

 if (err=0) then
 begin
  update_mtime(fs_src,mtime);
  Client.MountManager.SetMtime(slot,mtime);
 end;

 Result:=err;
end;

function TSaveDataBackendProcess.SaveIcon(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 len:DWORD;
 data:p_input_buf;
 prev:TMountSlot;
 job:TSaveIconJob;
begin
 Result:=0;
 len :=Value.GetLen;
 data:=Value.GetBuf;
 if (len<sizeof(t_input_buf)) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
 len:=len-sizeof(t_input_buf);
 if (len>data^.size) then len:=data^.size;

 if (DWORD(data^.slot)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not Client.MountManager.IsActiveMount(data^.slot) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 if Client.MountManager.IsReadOnly(data^.slot) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BAD_MOUNTED);
 end;

 prev:=Client.MountManager.GetMount(data^.slot);

 job:=TSaveIconJob.Create(Client,Client.HoldResult);
 job.fs_src:=Client.GameMountConfig.GetSaveDataFolder(prev.userId,@prev.titleId.data,@prev.dirName.data);

 job.slot:=data^.slot;
 job.len :=len;
 Move(data^.data,job.data,len);

 SendCmd(job);
end;

function TSaveDataBackendConnect.LoadIcon(slot_id:Integer;icon:pSceSaveDataIcon;internal:Boolean):Integer;
label
 _memcpy;
var
 len:DWORD;
 Value:TIpcValue;
 data:p_output_buf;
begin
 Value:=kipc.InvokeSync('LoadIcon',slot_id);

 Result:=Value.GetDWORD;

 if (Result=0) then
 begin

  len :=Value.GetLen;
  data:=Value.GetBuf;
  if (len<sizeof(t_output_buf)) then
  begin
   len:=0;
  end else
  begin
   len:=len-sizeof(t_output_buf);
   if (len>data^.size) then len:=data^.size;
  end;

  if (p_proc.p_sdk_version < $4000000) then
  begin
   if (icon^.bufSize < len) then
   begin
    Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
   end;
   _memcpy:
    Move(data^.data,icon^.buf^,len);
  end else
  begin
   if (internal) then
   begin
    if (icon^.bufSize < len) then
    begin
     Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
    end;
    goto _memcpy;
   end;
   if (icon^.bufSize >= len) then
   begin
    goto _memcpy;
   end;
   icon^.buf:=nil;
  end;

  icon^.dataSize:=len;
 end;

 Value.Free;
end;

type
 TLoadIconJob=class(TCustomCommand)
  //
  fs_src:RawByteString;
  //
  function Run:TIpcValue; override;
 end;

function TLoadIconJob.Run:TIpcValue;
var
 ficon:RawByteString;
 data:p_output_buf;
 size:Ptrint;
begin
 Result:=0;

 ficon:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/icon0.png');

 if not FileExists(ficon) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_FILE_NOT_FOUND);
 end;

 data:=AllocMem($1C800+sizeof(t_output_buf));

 size:=ReadFromFile(ficon,@data^.data,$1C800);

 if (size<=0) then
 begin
  Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
 end else
 begin
  data^.size:=size;
  Result:=TIpcValue.Inplace(data,data,size+sizeof(t_output_buf));
 end;

end;

function TSaveDataBackendProcess.LoadIcon(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 slot_id:Integer;
 prev:TMountSlot;
 job:TLoadIconJob;
begin
 Result:=0;

 slot_id:=Value.GetDWORD;

 if (DWORD(slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not Client.MountManager.IsActiveMount(slot_id) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 prev:=Client.MountManager.GetMount(slot_id);

 job:=TLoadIconJob.Create(Client,Client.HoldResult);
 job.fs_src:=Client.GameMountConfig.GetSaveDataFolder(prev.userId,@prev.titleId.data,@prev.dirName.data);

 SendCmd(job);
end;

type
 PSetParam=^TSetParam;
 TSetParam=packed record
  slot :WORD;
  ptype:WORD;
  size :DWORD;
  data :record end;
 end;

function TSaveDataBackendConnect.SetParam(slot_id     :Integer;
                                          paramType   :SceSaveDataParamType;
                                          paramBuf    :Pointer;
                                          paramBufSize:QWORD):Integer;
var
 data:PSetParam;
begin
 data:=AllocMem(paramBufSize+sizeof(TSetParam));
 data^.slot :=slot_id;
 data^.ptype:=paramType;
 data^.size :=paramBufSize;
 Move(paramBuf^,data^.data,paramBufSize);

 Result:=kipc.InvokeSync2('SetParam',TIpcValue.Inplace(data,data,paramBufSize+sizeof(TSetParam)));
end;

type
 TSetParamJob=class(TCustomCommand)
  //
  fs_src:RawByteString;
  //
  slot:DWORD;
  //
  param_sfo:t_savedata_sfo_values;
  //
  function Run:TIpcValue; override;
 end;

function TSetParamJob.Run:TIpcValue;
var
 fname:RawByteString;
 err:Integer;
 mtime:QWORD;
begin
 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/param.sfo');

 if param_sfo.SaveToFile(fname) then
 begin
  err:=0;
 end else
 begin
  err:=SCE_SAVE_DATA_ERROR_INTERNAL;
 end;

 if (err=0) then
 begin
  update_mtime(fs_src,mtime);
  Client.MountManager.SetMtime(slot,mtime);
 end;

 Result:=err;
end;

function TSaveDataBackendProcess.SetParam(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 len:DWORD;
 data:PSetParam;
 minfo:TMountSlot;
 job:TSetParamJob;
begin
 Result:=0;
 len :=Value.GetLen;
 data:=Value.GetBuf;
 if (len<sizeof(TSetParam)) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
 len:=len-sizeof(TSetParam);
 if (len>data^.size) then len:=data^.size;

 if (DWORD(data^.slot)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not Client.MountManager.IsActiveMount(data^.slot) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 if Client.MountManager.IsReadOnly(data^.slot) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BAD_MOUNTED);
 end;

 Client.MountManager.SetParam(data^.slot,data^.ptype,@data^.data,len);

 minfo:=Client.MountManager.GetMount(data^.slot);

 job:=TSetParamJob.Create(Client,Client.HoldResult);
 job.fs_src   :=Client.GameMountConfig.GetSaveDataFolder(minfo.userId,@minfo.titleId.data,@minfo.dirName.data);
 job.slot     :=data^.slot;
 job.param_sfo:=minfo.param_sfo;

 SendCmd(job);
end;

type
 PGetParam=^TGetParam;
 TGetParam=packed record
  slot :WORD;
  ptype:WORD;
 end;

function TSaveDataBackendConnect.GetParam(slot_id     :Integer;
                                          paramType   :SceSaveDataParamType;
                                          paramBuf    :Pointer;
                                          paramBufSize:QWORD;
                                          gotSize     :PQWORD):Integer;
var
 input:TGetParam;
 len:DWORD;
 Value:TIpcValue;
 data:p_output_buf;
begin
 input.slot :=slot_id;
 input.ptype:=paramType;

 Value:=kipc.InvokeSync('GetParam',TIpcValue.Static(@input,sizeof(input)));

 Result:=Value.GetDWORD;

 if (Result=0) then
 begin

  len :=Value.GetLen;
  data:=Value.GetBuf;
  if (len<sizeof(t_output_buf)) then
  begin
   len:=0;
  end else
  begin
   len:=len-sizeof(t_output_buf);
   if (len>data^.size) then len:=data^.size;
  end;

  FillChar(paramBuf^,paramBufSize,0);

  if (len <= paramBufSize) then
  begin
   Move(data^.data,paramBuf^,len);
   if (gotSize<>nil) then
   begin
    gotSize^:=len;
   end;
   Result:=0;
  end else
  begin
   Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
  end;

 end;

 Value.Free;
end;

function TSaveDataBackendProcess.GetParam(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 input:TGetParam;
 data :p_output_buf;
begin
 Result:=0;

 input:=Default(TGetParam);
 Value.MoveTo(@input,sizeof(input));

 if (DWORD(input.slot)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not Client.MountManager.IsActiveMount(input.slot) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 data:=AllocMem(sizeof(t_output_buf)+$1c800);

 Client.MountManager.GetParam(input.slot,input.ptype,@data^.data,@data^.size);

 Result:=TIpcValue.Inplace(data,data,sizeof(t_output_buf)+$1c800);
end;

function TSaveDataBackendConnect.SetupMemory(userId        :SceUserServiceUserId;
                                             slotId        :Integer;
                                             bufferNum     :Integer;
                                             memorySize    :DWORD;
                                             iconMemorySize:DWORD;
                                             paramSize     :DWORD;
                                             InitParams    :pSceSaveDataParam
                                            ):Integer;
var
 data:TSetupMemory;
begin
 FillChar(data,SizeOf(data),0);
 data.userId        :=userId        ;
 data.slotId        :=slotId        ;
 data.bufferNum     :=bufferNum     ;
 data.paramSize     :=paramSize     ;
 data.memorySize    :=memorySize    ;
 data.iconMemorySize:=iconMemorySize;

 if (InitParams<>nil) then
 begin
  data.InitParams.title    :=InitParams^.title    ;
  data.InitParams.subTitle :=InitParams^.subTitle ;
  data.InitParams.detail   :=InitParams^.detail   ;
  data.InitParams.userParam:=InitParams^.userParam;
 end;

 Result:=kipc.InvokeSync2('SetupMemory',@data,sizeof(data));
end;

type
 TSetupMemoryJob=class(TMountJob)
  //
  nslot:PSetupMemoryNode;
  //
  function Run:TIpcValue; override;
 end;

function TSetupMemoryJob.Run:TIpcValue;
var
 titleId:pchar;
 dirName:pchar;
 blocks :SceSaveDataBlocks;

 err:Integer;
 is_locked:Boolean;
 is_change:Boolean;
 is_init  :Boolean;

 RestoreJob:TRestoreJob;
begin
 err:=0;

 titleId:=@data.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@Client.GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 blocks:=data.blocks;

 is_locked:=False;

 fs_src:=Client.GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

 is_locked:=Lock();

 if not is_locked then
 begin
  err:=SCE_SAVE_DATA_ERROR_BUSY;
 end else
 begin
  if SaveDataExists(fs_src) then
  begin
   //open
   err:=OpenMount();
   is_init:=False;
  end else
  begin
   //create
   err:=CreateMount();
   is_init:=True;
  end;

  ///
  if (err=SCE_SAVE_DATA_ERROR_BROKEN) then
  begin
   RestoreJob:=TRestoreJob.Create(Client,nil);
   RestoreJob.Init(data.userId,titleId,dirName);

   if RestoreJob.Prepare then
   begin
    err:=RestoreJob.CheckBackup(True);
   end else
   begin
    err:=SCE_SAVE_DATA_ERROR_INTERNAL;
   end;

   if (err=0) then
   begin

    if RestoreJob.Restore then
    begin
     err:=0;
    end else
    begin
     err:=SCE_SAVE_DATA_ERROR_INTERNAL;
    end;

   end else
   if (err=SCE_SAVE_DATA_ERROR_NOT_FOUND) then
   begin
    //delete
    RestoreJob.DoDelete(nil,0);
    //create
    err:=CreateMount();
   end;

   RestoreJob.Free;
  end;
  ///

  if (err=0) then
  begin
   is_change:=False;

   //AllocSpace
   if (param_sfo.SAVEDATA_BLOCKS<>blocks) then
   begin
    param_sfo.SAVEDATA_BLOCKS:=blocks;
    is_change:=True;
   end;

   //InitParams
   if is_init then
   begin
    if (nslot^.data.InitParams.title[0]<>#0) then
    begin
     param_sfo.MAINTITLE:=nslot^.data.InitParams.title;
    end;

    param_sfo.SUBTITLE           :=nslot^.data.InitParams.subTitle;
    param_sfo.DETAIL             :=nslot^.data.InitParams.detail;
    param_sfo.SAVEDATA_LIST_PARAM:=nslot^.data.InitParams.userParam;

    is_change:=True;
   end;

   if is_change then
   begin
    err:=SaveParamSfo();
   end;
  end;

 end;

 if is_locked then
 begin
  Unlock;
 end;

 ///deref
 nslot^.Release;

 Result:=err;
end;

function TSaveDataBackendProcess.SetupMemory(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 input:TSetupMemory;
 node :PSetupMemoryNode;
 minfo:TMount;
 job  :TSetupMemoryJob;
 err  :Integer;
begin
 Result:=0;

 input:=Default(TSetupMemory);
 Value.MoveTo(@input,sizeof(input));

 if (input.slotId>=4) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
 if (input.bufferNum<>1) and (input.bufferNum<>2) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 node:=Client.SetupMemoryManager.Setup(input);
 if (node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 err:=node^.CreateBuffers();
 if (err<>0) then Exit(err);

 minfo:=Default(TMount);
 minfo.userId      :=input.userId;
 minfo.dirName.data:=sdmemory_slot_name[input.slotId];
 minfo.blocks      :=GetBlocks(input.memorySize);
 minfo.mountMode   :=SDMM_RDONLY;

 job:=TSetupMemoryJob.Create(Client,Client.HoldResult);
 job.Init(minfo);

 node^.Acquire;
 job.nslot:=node;

 SendCmd(job);
end;

type
 TReadMemory=record
  slot_id :Integer;
  dataSize:DWORD;
 end;

function TSaveDataBackendConnect.ReadMemory(slot_id:Integer;dataBuf:Pointer;dataSize:DWORD;p_existedMemorySize:PQWORD):Integer;
var
 input:TReadMemory;
 len:DWORD;
 Value:TIpcValue;
 data:p_output_buf;
begin
 input.slot_id :=slot_id;
 input.dataSize:=dataSize;

 Value:=kipc.InvokeSync('ReadMemory',TIpcValue.Static(@input,SizeOf(input)));

 Result:=Value.GetDWORD;

 if (Result=0) then
 begin

  len :=Value.GetLen;
  data:=Value.GetBuf;
  if (len<sizeof(t_output_buf)) then
  begin
   len:=0;
   p_existedMemorySize^:=0;
  end else
  begin
   len:=len-sizeof(t_output_buf);
   if (len>dataSize) then len:=dataSize;

   Move(data^.data,dataBuf^,len);

   p_existedMemorySize^:=data^.size;
  end;

 end;

 Value.Free;
end;

type
 TReadMemoryJob=class(TCustomCommand)
  //
  fs_src:RawByteString;
  //
  dataSize:DWORD;
  //
  function Run:TIpcValue; override;
 end;

function TReadMemoryJob.Run:TIpcValue;
var
 fmemory:RawByteString;
 data:p_output_buf;
 read_size:Ptrint;
 existedMemorySize:QWORD;
begin
 Result:=0;

 if not SaveMemoryExists(fs_src) then
 begin
  Exit(0);
 end;

 fmemory:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/memory.dat');

 data:=AllocMem(dataSize+sizeof(t_output_buf));

 read_size:=ReadFromFile(fmemory,@data^.data,dataSize);

 if (read_size<=0) then
 begin
  Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
 end else
 begin
  get_file_size(fmemory,existedMemorySize);
  data^.size:=existedMemorySize;

  Result:=TIpcValue.Inplace(data,data,read_size+sizeof(t_output_buf));
 end;

end;

function TSaveDataBackendProcess.ReadMemory(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 input:TReadMemory;
 prev:TMountSlot;
 job:TReadMemoryJob;
begin
 Result:=0;

 input:=Default(TReadMemory);
 Value.MoveTo(@input,sizeof(input));

 if (DWORD(input.slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not Client.MountManager.IsActiveMount(input.slot_id) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 prev:=Client.MountManager.GetMount(input.slot_id);

 job:=TReadMemoryJob.Create(Client,Client.HoldResult);
 job.fs_src:=Client.GameMountConfig.GetSaveDataFolder(prev.userId,@prev.titleId.data,@prev.dirName.data);

 job.dataSize:=input.dataSize;

 SendCmd(job);
end;

type
 TSetWriteSlot=packed record
  userId  :DWORD;
  slotId  :WORD;
  bufferId:WORD;
 end;

procedure TSaveDataBackendConnect.WriteMemory(userId,slotId,bufferId:DWORD;addr:Pointer;size:DWORD);
var
 data:TSetWriteSlot;
begin
 data.userId  :=userId;
 data.slotId  :=slotId;
 data.bufferId:=bufferId;

 kipc.InvokeAsyn('SetWriteSlot',@data,Sizeof(data));

 kipc.InvokeAsyn('WriteMemory',addr,size);
end;

function TSaveDataBackendProcess.SetWriteSlot(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 data:TSetWriteSlot;
 node:PSetupMemoryNode;
begin
 Result:=0;
 Client.pWriteSlot:=nil;

 data:=Default(TSetWriteSlot);
 Value.MoveTo(@data,sizeof(data));

 node:=Client.SetupMemoryManager.Get(data.userId,data.slotId);
 if (node=nil) then Exit;

 if (node^.is_setup) then
 if (data.bufferId<node^.data.bufferNum) then
 begin
  node^.FbufferId:=data.bufferId;
  Client.pWriteSlot:=node;
 end;

end;

function TSaveDataBackendProcess.WriteMemory(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 src_addr:Pointer;
 src_size:QWORD;

 buf:PSdMemoryBuffer;
 is_writed:Boolean;
begin
 Result:=0;
 if (Client.pWriteSlot=nil) then Exit;

 buf:=@Client.pWriteSlot^.sd_buffers[Client.pWriteSlot^.FbufferId];

 src_addr:=Value.GetBuf;
 src_size:=Value.GetLen;

 if (src_size>buf^.Fsize) then src_size:=buf^.Fsize;

 is_writed:=CompareByte(src_addr^,buf^.Paddr^,src_size)<>0;

 if is_writed then
 begin
  mtx_lock(Client.pWriteSlot^.mtx);

   Move(src_addr^,buf^.Paddr^,src_size);

   Client.pWriteSlot^.is_writed:=True;

  mtx_unlock(Client.pWriteSlot^.mtx);

  if (Client.pWriteSlot^.FRefs<2) then
  begin
   SendSyncJob(Client,Client.pWriteSlot^.data.userId,Client.pWriteSlot^.data.slotId,True,False);
  end;
 end;
end;

type
 TSyncMemory=packed record
  userId:DWORD;
  slotId:WORD;
  option:WORD; //SceSaveDataMemorySyncOption
 end;

function TSaveDataBackendConnect.SyncMemory(syncParam:pSceSaveDataMemorySync):Integer;
var
 data:TSyncMemory;
begin
 data.userId:=syncParam^.userId;
 data.slotId:=syncParam^.slotId;
 data.option:=syncParam^.option;

 Result:=kipc.InvokeSync2('SyncMemory',@data,sizeof(data));
end;

type
 TSyncMemoryJob=class(TCustomCommand)
  //
  fs_src  :RawByteString;
  //
  nslot   :PSetupMemoryNode;
  titleId :SceSaveDataTitleId;
  dirName :SceSaveDataDirName;
  is_async:Boolean;
  is_event:Boolean;
  //
  function  Lock():Boolean;
  procedure UnLock;
  function  SetParams(params:pSceSaveDataParam):Integer;
  function  SyncIcon (iconData:Pointer;iconBufSize:Ptrint):Integer;
  function  Run:TIpcValue; override;
 end;

function TSyncMemoryJob.Lock():Boolean;
begin
 Result:=gSaveDataBackend.LockDirManager.LockDir(fs_src);
end;

procedure TSyncMemoryJob.UnLock;
begin
 gSaveDataBackend.LockDirManager.UnLockDir(fs_src);
end;

function TSyncMemoryJob.SetParams(params:pSceSaveDataParam):Integer;
var
 param_sfo:t_savedata_sfo_values;
 fname:RawByteString;
begin
 Result:=0;

 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/param.sfo');

 if not param_sfo.LoadFromFile(fname) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 if not param_sfo.Verif(nslot^.data.userId,@dirName.data) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 param_sfo.SetParam(SCE_SAVE_DATA_PARAM_TYPE_ALL,params,$530);

 if param_sfo.SaveToFile(fname) then
 begin
  Result:=0;
 end else
 begin
  Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
 end;
end;

function TSyncMemoryJob.SyncIcon(iconData:Pointer;iconBufSize:Ptrint):Integer;
begin
 Result:=0;

 if (iconBufSize=0) then
 begin
  //CopyIcon

  iconBufSize:=Client.LoadPkgIcon(iconData);

  if (iconBufSize<=0) then
  begin
   //
  end else
  begin
   Result:=CheckPng(iconData,iconBufSize);
   if (Result<>0) then Exit;

   if not SaveIcon(fs_src,iconData,iconBufSize) then
   begin
    Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
   end;
  end;

 end else
 begin
  Result:=CheckPng(iconData,iconBufSize);
  if (Result<>0) then Exit;

  if not SaveIcon(fs_src,iconData,iconBufSize) then
  begin
   Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
  end;
 end;

end;

function TSyncMemoryJob.Run:TIpcValue;
var
 buf:PSdMemoryBuffer;
 params:SceSaveDataParam;
 is_writed:Boolean;
 is_eventd:Boolean;
 err      :Integer;
begin
 Result:=0;

 err:=0;

 if is_async then
 begin

  if Client.MountManager.IsActiveMount(nslot^.data.userId,@titleId.data,@dirName.data) then
  begin
   defer:=True;
   Exit(0); //????
  end;

  if not Lock() then
  begin
   defer:=True;
   Exit(0);
  end;

 end;

 mtx_lock(nslot^.mtx);

  is_writed:=nslot^.is_writed;
  is_eventd:=nslot^.is_eventd;

  if is_writed then
  begin
   is_eventd       :=False;
   nslot^.is_eventd:=False;

   buf:=@nslot^.sd_buffers[nslot^.FbufferId];

   if (buf^.PParamData=nil) then
   begin
    //SyncInitParam
    params:=Default(SceSaveDataParam);

    params.title    :=nslot^.data.InitParams.title;
    params.subTitle :=nslot^.data.InitParams.subTitle;
    params.detail   :=nslot^.data.InitParams.detail;
    params.userParam:=nslot^.data.InitParams.userParam;

    if (params.title[0]=#0) then
    begin
     strlcopy(@params.title,GET_MAINTITLE_DEFAULT(Client.systemLang),SCE_SAVE_DATA_TITLE_MAXSIZE);
    end;

    params.title   [127] :=#0;
    params.subTitle[127] :=#0;
    params.detail  [1023]:=#0;

    err:=SetParams(@params);
   end else
   begin
    //SyncParamBuf
    err:=SetParams(buf^.PParamData);
   end;

   if (err=0) and (buf^.PmemoryData<>nil) then
   begin
    if not SaveMemory(fs_src,buf^.PmemoryData,buf^.FmemorySize) then
    begin
     Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
    end;
   end;

   if (err=0) then
   begin
    if (buf^.PiconMemorySize=nil) then
    begin
     err:=SyncIcon(nil,0);
    end else
    begin
     err:=SyncIcon(buf^.PiconData,buf^.PiconMemorySize^.cur);
    end;
   end;

   nslot^.is_writed:=False;
  end;

  if (err=0) then
  begin
   update_mtime(fs_src,params.mtime);
  end;

 mtx_unlock(nslot^.mtx);

 ///

 if is_async then
 begin
  if is_event and (not is_eventd) then
  begin

   mtx_lock(nslot^.mtx);

    nslot^.is_eventd:=True;

   mtx_unlock(nslot^.mtx);

   Client.EventQueue.Push(
    SDET_SAVE_DATA_MEMORY_SYNC_END,
    err,
    nslot^.data.userId,
    @titleId,
    @dirName);

  end;
 end else
 begin
  Result:=err;
 end;

 if is_writed then
 begin
  UnLock; //unlock first

  gSaveDataBackend.SendBackupJob(Client,
                                 nslot^.data.userId,
                                 @titleId.data,
                                 @dirName.data,
                                 nil,
                                 0);

  if (err=0) then
  begin
   Writeln('Sync savedata memory of user ',HexStr(nslot^.data.userId,8),' is done.');
  end else
  begin
   Writeln('Sync savedata memory of user ',HexStr(nslot^.data.userId,8),' is failed : ',HexStr(err,8));
  end;

 end else
 begin
  UnLock;
 end;

 ///deref
 nslot^.Release;
end;

function TSaveDataBackendProcess.SendSyncJob(Client:TSaveDataClient;userId,slotId:DWORD;is_async,is_event:Boolean):Integer;
var
 node    :PSetupMemoryNode;
 titleId :pchar;
 dirName :pchar;
 job     :TSyncMemoryJob;
 fs_src  :RawByteString;
begin
 Result:=0;

 node:=Client.SetupMemoryManager.Get(userId,slotId);
 if (node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 titleId:=@Client.GameMountConfig.InstallDir;
 dirName:=sdmemory_slot_name[slotId];

 fs_src  :=Client.GameMountConfig.GetSaveDataFolder(userId,titleId,dirName);

 if is_async then
 begin
  job:=TSyncMemoryJob.Create(Client,nil);
 end else
 begin

  if Client.MountManager.IsActiveMount(userId,titleId,dirName) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_BUSY);
  end;

  if not LockDirManager.LockDir(fs_src) then
  begin
   Exit(0);
   //Exit(SCE_SAVE_DATA_ERROR_BUSY_FOR_SAVING);
  end;

  if (Client<>nil) then
  begin
   job:=TSyncMemoryJob.Create(Client,Client.HoldResult);
  end else
  begin
   job:=TSyncMemoryJob.Create(Client,nil);
  end;

 end;

 //incref
 node^.Acquire;

 job.fs_src      :=fs_src;
 job.nslot       :=node;
 job.titleId.data:=titleId;
 job.dirName.data:=dirName;
 job.is_async    :=is_async;
 job.is_event    :=is_event and is_async;

 SendCmd(job);
end;

function TSaveDataBackendProcess.SyncMemory(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 input:TSyncMemory;
begin
 input:=Default(TSyncMemory);
 Value.MoveTo(@input,sizeof(input));

 Result:=SendSyncJob(Client,input.userId,input.slotId,((input.option and 1)=0),True);
end;

type
 TDirNameSearch=record
  userId    :DWORD;
  max       :Word; //<=1024
  key       :Byte;
  order     :Byte;
  has_params:Boolean;
  has_infos :Boolean;
  internal  :Boolean;
  titleId   :SceSaveDataTitleId;
  dirName   :SceSaveDataDirName;
 end;

 PDirNameSearchNode=^TDirNameSearchNode;
 TDirNameSearchNode=record
  dirName:SceSaveDataDirName;
  params :SceSaveDataParam;
  infos  :SceSaveDataSearchInfo;
 end;

function Min(x, y: integer): integer; inline;
begin
  if x < y then Result := x else Result := y;
end;

function TSaveDataBackendConnect.DirNameSearch(cond    :pSceSaveDataDirNameSearchCond;
                                               pResult :pSceSaveDataDirNameSearchResult;
                                               internal:Boolean):Integer;
var
 data    :TDirNameSearch;
 Value   :TIpcValue;
 output  :PDirNameSearchNode;
 dirNames:pSceSaveDataDirName;
 params  :pSceSaveDataParam;
 infos   :pSceSaveDataSearchInfo;
 i,count :Integer;
begin
 Result:=0;

 FillChar(data,SizeOf(data),0);
  data.userId    :=cond^.userId;
  data.max       :=Min(pResult^.dirNamesNum,1024);
  data.key       :=cond^.key;
  data.order     :=cond^.order;
  data.has_params:=(pResult^.params<>nil);
  data.has_infos :=(pResult^.infos <>nil);
  data.internal  :=internal;
 if (cond^.titleId<>nil) then
  data.titleId:=cond^.titleId^;
 if (cond^.dirName<>nil) then
  data.dirName:=cond^.dirName^;

 if (data.max=0) then
 begin
  count:=0;
 end else
 begin
  Value:=kipc.InvokeSync('DirNameSearch',TIpcValue.Static(@data,sizeof(data)));

  if (Value.GetLen<=8) then
  begin
   Result:=Value.GetDWORD;
   Value.Free;
   Exit;
  end;

  output:=Value.GetBuf;
  count :=Value.GetLen div SizeOf(TDirNameSearchNode);

  dirNames:=pResult^.dirNames;
  params  :=pResult^.params;
  infos   :=pResult^.infos;

  if (count<>0) then
  For i:=0 to count-1 do
  begin
   if (dirNames<>nil) then
   begin
    dirNames[i]:=output[i].dirName;
   end;
   //
   if (params<>nil) then
   begin
    params[i]:=output[i].params;
   end;
   //
   if (infos<>nil) then
   begin
    infos[i]:=output[i].infos;
   end;
  end;

  Value.Free;
 end;

 pResult^.hitNum:=count;
 if (p_proc.p_sdk_version < $1700000) then
 begin
  pResult^.hitNum:=count;
 end else
 begin
  pResult^.setNum:=count;
 end;

end;

type
 TDirNameSearchJob=class(TCustomCommand)
  //
  fs_src:RawByteString;
  //
  data:TDirNameSearch;
  //
  List     :TFPList;
  param_sfo:t_savedata_sfo_values;
  //
  procedure IterateDirectory();
  procedure Sort();
  function  Run:TIpcValue; override;
 end;

function convert_dir_name_search(P:PChar):RawByteString;
var
 i:Integer;
begin
 Result:=RawByteString(P); //copy
 if (Length(Result)=0) then
 begin
  Result:='*';
 end else
 For i:=1 to Length(Result) do
 begin
  Case Result[i] of
   '%':Result[i]:='*';
   '_':Result[i]:='?';
   else;
  end;
 end;
end;

function NeedSfoByKey(key:Byte):Boolean; inline;
begin
 Result:=False;
 case key of
  SDSK_USER_PARAM,
  SDSK_BLOCKS,
  SDSK_FREE_BLOCKS:Result:=True;
  else;
 end;
end;

procedure TDirNameSearchJob.IterateDirectory();
var
 FileInfo :TSearchRec;
 CurParent:RawByteString;
 CurDir   :RawByteString;
 fname    :RawByteString;
 dirName  :SceSaveDataDirName;
 dir_node :PDirNameSearchNode;
 mtime    :QWORD;
 size     :DWORD;
 load_sfo :Boolean;
begin
 List:=TFPList.Create;

 load_sfo:=data.has_params or data.has_infos or NeedSfoByKey(data.key);

 CurParent:=IncludeTrailingPathDelimiter(fs_src);

 //Writeln(CurParent);

 if SysUtils.FindFirst(CurParent+convert_dir_name_search(@data.dirName.data),faDirectory,FileInfo)=0 then
 begin
  repeat
    // check if special file
    if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
    begin
      continue;
    end;

    dirName.data:=copy(FileInfo.Name,1,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE);

    // filter
    if not data.internal then
    begin
     if (PDWORD(@dirName.data)^=$5F656373) or (dirName.data[0]='_') then //sce_* or _*
     begin
       continue;
     end;
    end;

    CurDir:=CurParent+FileInfo.Name;

    dir_node:=AllocMem(SizeOf(TDirNameSearchNode));
    dir_node^.dirName:=dirName;

    if load_sfo then
    begin
     fname:=ExcludeTrailingPathDelimiter(CurDir)+unix_to_host('/sce_sys/param.sfo');
     //
     if param_sfo.LoadFromFile(fname) then
     begin
      mtime:=0;
      if data.has_params or (data.key=SDSK_MTIME) then
      begin
       load_mtime(CurDir,mtime);
      end;
      //
      param_sfo.GetParam(SCE_SAVE_DATA_PARAM_TYPE_ALL,@dir_node^.params,@size,mtime);
      //
      dir_node^.infos.blocks:=param_sfo.SAVEDATA_BLOCKS;
      //
      if data.has_infos or (data.key=SDSK_FREE_BLOCKS) then
      begin
       dir_node^.infos.freeBlocks:=GetFreeBlocks(CurDir,param_sfo.SAVEDATA_BLOCKS);
      end;
     end;
    end;

    List.Add(dir_node);
    dir_node:=nil;

    //Writeln(CurDir);

  until SysUtils.FindNext(FileInfo)<>0;
  SysUtils.FindClose(FileInfo);
 end;

end;

function SDSK_DIRNAME_ASC(Item1,Item2:PDirNameSearchNode): Integer;
begin
 Result:=CompareByte(Item1^.dirName,Item2^.dirName,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE);
end;

function SDSK_DIRNAME_DES(Item1,Item2:PDirNameSearchNode): Integer;
begin
 Result:=CompareByte(Item2^.dirName,Item1^.dirName,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE);
end;

function SDSK_USER_PARAM_ASC(Item1,Item2:PDirNameSearchNode): Integer;
begin
 Result:=(Item1^.params.userParam-Item2^.params.userParam);
end;

function SDSK_USER_PARAM_DES(Item1,Item2:PDirNameSearchNode): Integer;
begin
 Result:=(Item2^.params.userParam-Item1^.params.userParam);
end;

function SDSK_BLOCKS_ASC(Item1,Item2:PDirNameSearchNode): Integer;
begin
 Result:=Integer(Item1^.infos.blocks>Item2^.infos.blocks)-Integer(Item1^.infos.blocks<Item2^.infos.blocks);
end;

function SDSK_BLOCKS_DES(Item1,Item2:PDirNameSearchNode): Integer;
begin
 Result:=Integer(Item1^.infos.blocks<Item2^.infos.blocks)-Integer(Item1^.infos.blocks>Item2^.infos.blocks);
end;

function SDSK_MTIME_ASC(Item1,Item2:PDirNameSearchNode): Integer;
begin
 Result:=Integer(Item1^.params.mtime>Item2^.params.mtime)-Integer(Item1^.params.mtime<Item2^.params.mtime);
end;

function SDSK_MTIME_DES(Item1,Item2:PDirNameSearchNode): Integer;
begin
 Result:=Integer(Item1^.params.mtime<Item2^.params.mtime)-Integer(Item1^.params.mtime>Item2^.params.mtime);
end;

function SDSK_FREE_BLOCKS_ASC(Item1,Item2:PDirNameSearchNode): Integer;
begin
 Result:=Integer(Item1^.infos.freeBlocks>Item2^.infos.freeBlocks)-Integer(Item1^.infos.freeBlocks<Item2^.infos.freeBlocks);
end;

function SDSK_FREE_BLOCKS_DES(Item1,Item2:PDirNameSearchNode): Integer;
begin
 Result:=Integer(Item1^.infos.freeBlocks<Item2^.infos.freeBlocks)-Integer(Item1^.infos.freeBlocks>Item2^.infos.freeBlocks);
end;

procedure TDirNameSearchJob.Sort();
var
 Compare:TListSortCompare;
begin
 if (List=nil) then Exit;

 Compare:=nil;

 if (data.order=SDSO_ASCENT) then
 begin
  case data.key of
   SDSK_DIRNAME    :Compare:=TListSortCompare(@SDSK_DIRNAME_ASC    );
   SDSK_USER_PARAM :Compare:=TListSortCompare(@SDSK_USER_PARAM_ASC );
   SDSK_BLOCKS     :Compare:=TListSortCompare(@SDSK_BLOCKS_ASC     );
   SDSK_MTIME      :Compare:=TListSortCompare(@SDSK_MTIME_ASC      );
   SDSK_FREE_BLOCKS:Compare:=TListSortCompare(@SDSK_FREE_BLOCKS_ASC);
   else;
  end;
 end else
 begin
  case data.key of
   SDSK_DIRNAME    :Compare:=TListSortCompare(@SDSK_DIRNAME_DES    );
   SDSK_USER_PARAM :Compare:=TListSortCompare(@SDSK_USER_PARAM_DES );
   SDSK_BLOCKS     :Compare:=TListSortCompare(@SDSK_BLOCKS_DES     );
   SDSK_MTIME      :Compare:=TListSortCompare(@SDSK_MTIME_DES      );
   SDSK_FREE_BLOCKS:Compare:=TListSortCompare(@SDSK_FREE_BLOCKS_DES);
   else;
  end;
 end;

 if (Compare<>nil) then
 begin
  List.Sort(Compare);
 end;
end;

procedure FreeDirNameSearchNode(data,arg:pointer);
begin
 FreeMem(data);
end;

function TDirNameSearchJob.Run:TIpcValue;
var
 i,Count:Integer;
 dir_node:PDirNameSearchNode;
 output  :PDirNameSearchNode;
begin
 Result:=TIpcValue.Static(nil,0);

 IterateDirectory();
 Sort();

 if (List=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 Count:=Min(List.Count,data.max);

 if (Count>0) then
 begin
  output:=AllocMem(Count*SizeOf(TDirNameSearchNode));
  //
  For i:=0 to Count-1 do
  begin
   dir_node:=List.Items[i];
   if (dir_node<>nil) then
   begin
    output[i]:=dir_node^;
   end;
  end;
  //
  Result:=TIpcValue.Inplace(output,output,Count*SizeOf(TDirNameSearchNode));
 end;

 List.ForEachCall(@FreeDirNameSearchNode,nil);
 List.Free;
end;

function TSaveDataBackendProcess.DirNameSearch(Client:TSaveDataClient;Value:TIpcValue):TIpcValue;
var
 input:TDirNameSearch;
 titleId:pchar;
 job:TDirNameSearchJob;
begin
 Result:=0;
 input:=Default(TDirNameSearch);
 Value.MoveTo(@input,sizeof(input));

 if (input.max>1024) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 case input.key of
  SDSK_DIRNAME,
  SDSK_USER_PARAM,
  SDSK_BLOCKS,
  SDSK_MTIME,
  SDSK_FREE_BLOCKS:;
  else
   Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
 end;

 case input.order of
  SDSO_ASCENT,
  SDSO_DESCENT:;
  else
   Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
 end;

 titleId:=@input.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@Client.GameMountConfig.InstallDir;
 end;

 job:=TDirNameSearchJob.Create(Client,Client.HoldResult);

 job.fs_src:=Client.GameMountConfig.GetSaveDataFolder(input.userId,titleId,'');
 job.data  :=input;

 SendCmd(job);
end;



end.



