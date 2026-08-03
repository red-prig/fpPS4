unit SaveDataBackend;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 classes,
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
 ps4_libSceSystemService,
 SceSaveData,
 SaveDataBackendSfo,
 SaveDataBackendUtils;

type
 TSaveDataBackendConnect=class
  kipc    :THostIpcPipeKERN;
  hProcess:THandle;
  fork_pid:Integer;
  //
  MountSlots:array[0..TMountManager.max-1] of Boolean;
  //
  Constructor Create;
  Destructor  Destroy; override;
  procedure   SendMountConfig();
  procedure   UmountAllForce;
  function    DoDelete      (del:pSceSaveDataDelete):Integer;
  function    DoMount       (mount:pSceSaveDataMount;pResult:pSceSaveDataMountResult;Transfering,Internal:Boolean):Integer;
  function    DoUmount      (slot_id:Integer;backup:boolean):Integer;
  function    GetMountInfo  (slot_id:Integer;info:pSceSaveDataMountInfo):Integer;
  function    DoBackup      (backup:pSceSaveDataBackup):Integer;
  function    CheckBackup   (check:pSceSaveDataCheckBackupData):Integer;
  function    RestoreBackup (restore:pSceSaveDataRestoreBackupData):Integer;
  function    GetEventResult(event:pSceSaveDataEvent):Integer;
  function    GetProgress   (p_progress:PSingle):Integer;
  function    ClearProgress ():Integer;
  function    SaveIcon      (slot_id:Integer;icon:pSceSaveDataIcon):Integer;
  function    LoadIcon      (slot_id:Integer;icon:pSceSaveDataIcon;internal:Boolean):Integer;
  function    SetParam      (slot_id     :Integer;
                             paramType   :SceSaveDataParamType;
                             paramBuf    :Pointer;
                             paramBufSize:QWORD):Integer;
  function    GetParam      (slot_id     :Integer;
                             paramType   :SceSaveDataParamType;
                             paramBuf    :Pointer;
                             paramBufSize:QWORD;
                             gotSize     :PQWORD):Integer;
  function    SetupMemory   (userId        :SceUserServiceUserId;
                             slotId        :Integer;
                             bufferNum     :Integer;
                             memorySize    :DWORD;
                             iconMemorySize:DWORD;
                             paramSize     :DWORD;
                             InitParams    :pSceSaveDataParam
                            ):Integer;
  function    ReadMemory    (slot_id:Integer;dataBuf:Pointer;dataSize:DWORD;p_existedMemorySize:PQWORD):Integer;
  procedure   WriteMemory   (userId,slotId,bufferId:DWORD;addr:Pointer;size:DWORD);
  function    SyncMemory    (syncParam:pSceSaveDataMemorySync):Integer;
  function    DirNameSearch (cond    :pSceSaveDataDirNameSearchCond;
                             pResult :pSceSaveDataDirNameSearchResult;
                             internal:Boolean):Integer;
 end;

 TCustomCommand=class;

 TCustomCommand=class
  type
   PQNode=^TQNode;
   TQNode=object
    next_:PQNode;
    self_:TCustomCommand;
   end;
  var
   node :TQNode;
   rid  :DWORD;
   defer:Boolean;
  Constructor Create(_rid:DWORD);
  function    Run:TIpcValue; virtual;
  procedure   Invoke(value:TIpcValue);
  function    GetProgress:Single; virtual;
 end;

 THostIpcPipeSave=class(THostIpcPipe)
  event:t_event;
  Constructor Create;
  procedure   Recv_pipe; override;
 end;

 TProgressInfo=record
  mtx  :mtx;
  cmd  :TCustomCommand; //Mount, Delete, RestoreBackupData
  Value:Single;
 end;

 TSaveDataBackendProcess=class
  systemLang:DWORD;
  ppid      :Integer;
  parent    :THandle;
  kipc      :THostIpcPipeSave;
  job_queue :TIntrusiveMPSCQueue;
  job_event :t_event;
  //
  MountManager:TMountManager;
  //
  LockDirManager:TLockDirManager;
  //
  SetupMemoryManager:TSetupMemoryManager;
  pWriteSlot:PSetupMemoryNode;
  //
  EventQueue:TEventQueue;
  //
  Progress:TProgressInfo;
  //
  Constructor Create;
  procedure   SendCmd         (cmd:TCustomCommand);
  function    RecvCmd         (var cmd:TCustomCommand):Boolean;
  procedure   DoExit          ();
  function    OnExitProc      (Value:TIpcValue):TIpcValue; //EXIT_PROC
  function    OnMountConfig   (Value:TIpcValue):TIpcValue; //MOUNT_CONFIG
  function    OnDelete        (Value:TIpcValue):TIpcValue; //Delete
  function    OnMount         (Value:TIpcValue):TIpcValue; //Mount
  function    OnIsActiveMount (Value:TIpcValue):TIpcValue; //IsActiveMount
  function    OnUmount        (Value:TIpcValue):TIpcValue; //Umount
  procedure   UmountAllForce  ();
  function    OnGetMountInfo  (Value:TIpcValue):TIpcValue; //GetMountInfo
  function    OnBackup        (Value:TIpcValue):TIpcValue; //Backup
  function    SendBackupJob   (userId     :SceUserServiceUserId;
                               titleId    :pchar;
                               dirName    :pchar;
                               fingerprint:pSceSaveDataFingerprint;
                               event_type :Byte):Integer;
  function    OnCheckBackup   (Value:TIpcValue):TIpcValue; //CheckBackup
  function    OnRestoreBackup (Value:TIpcValue):TIpcValue; //RestoreBackup
  function    OnGetEventResult(Value:TIpcValue):TIpcValue; //GetEventResult
  function    OnGetProgress   (Value:TIpcValue):TIpcValue; //GetProgress
  function    OnClearProgress (Value:TIpcValue):TIpcValue; //ClearProgress
  procedure   SetProgressJob  (cmd:TCustomCommand);
  function    OnSaveIcon      (Value:TIpcValue):TIpcValue; //SaveIcon
  function    OnLoadIcon      (Value:TIpcValue):TIpcValue; //LoadIcon
  function    OnSetParam      (Value:TIpcValue):TIpcValue; //SetParam
  function    OnGetParam      (Value:TIpcValue):TIpcValue; //GetParam
  function    OnSetupMemory   (Value:TIpcValue):TIpcValue; //SetupMemory
  function    OnReadMemory    (Value:TIpcValue):TIpcValue; //ReadMemory
  function    OnSetWriteSlot  (Value:TIpcValue):TIpcValue; //SetWriteSlot
  function    OnWriteMemory   (Value:TIpcValue):TIpcValue; //WriteMemory
  function    SendSyncJob     (userId,slotId,option:DWORD;hold:Boolean):Integer;
  function    OnSyncMemory    (Value:TIpcValue):TIpcValue; //SyncMemory
  function    OnDirNameSearch (Value:TIpcValue):TIpcValue; //DirNameSearch
 end;

implementation

//

var
 gSaveDataBackend:TSaveDataBackendProcess=nil;

type
 PForkData=^TForkData;
 TForkData=record
  pipefd     :THandle;
  sdk_version:DWORD;
  systemLang :DWORD;
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

 data.pipefd     :=kern2svdt[1];
 data.sdk_version:=p_proc.p_sdk_version;

 data.systemLang:=0;
 ps4_sceSystemServiceParamGetInt(SCE_SYSTEM_SERVICE_PARAM_ID_LANG,@data.systemLang);

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

 SendMountConfig();
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

Constructor THostIpcPipeSave.Create;
begin
 inherited;
 ev_init(event,'THostIpcPipeSave');
end;

procedure THostIpcPipeSave.Recv_pipe;
begin
 inherited;
 ev_signal(event);
end;

///

Constructor TCustomCommand.Create(_rid:DWORD);
begin
 node.self_:=self;
 rid:=_rid;
end;

function TCustomCommand.Run:TIpcValue;
begin
 Result:=0;
end;

procedure TCustomCommand.Invoke(value:TIpcValue);
begin
 if (rid=0) then
 begin
  value.Free;
 end else
 begin
  gSaveDataBackend.kipc.InvokeResult(rid,value);
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

procedure OnExitProc;
begin
 gSaveDataBackend.kipc.InvokeBroken();
end;

procedure savedata_process(data:Pointer;size:QWORD); SysV_ABI_CDecl;
var
 systemLang:DWORD;

 ppid:Integer;

 pipefd:THandle;
 parent:THandle;
begin
 //while not IsDebuggerPresent do sleep(100);

 pipefd              :=PForkData(data)^.pipefd;
 p_proc.p_sdk_version:=PForkData(data)^.sdk_version;
 systemLang          :=PForkData(data)^.systemLang;

 //free shared
 FreeMem(data);

 ppid:=md_getppid;

 Writeln('savedata_process started pid:',GetProcessID,' parent_pid:',ppid);

 parent:=md_pidfd_open(ppid);

 //dup
 pipefd:=md_pidfd_getfd(parent,pipefd);

 gSaveDataBackend:=TSaveDataBackendProcess.Create;
 gSaveDataBackend.kipc.set_pipe(pipefd);

 gSaveDataBackend.ppid  :=ppid  ;
 gSaveDataBackend.parent:=parent;

 gSaveDataBackend.systemLang:=systemLang;
 //////////////

 AddExitProc(@OnExitProc);

 BeginThread(@wait_parent,nil);
 BeginThread(@job_thread,nil);

 repeat
  ev_wait(gSaveDataBackend.kipc.event);

  gSaveDataBackend.kipc.Update();
 until false;

end;

////

Constructor TSaveDataBackendProcess.Create;
begin
 job_queue.Create;
 ev_init(job_event,'job_event');
 //
 LockDirManager.Init;
 //
 SetupMemoryManager.Init;
 //
 EventQueue.Init;
 //
 mtx_init(Progress.mtx,'Progress');
 //
 kipc:=THostIpcPipeSave.Create;
 kipc.FHandler:=THostIpcHandler.Create;
 //
 kipc.FHandler.AddCallback('EXIT_PROC'     ,@OnExitProc);
 kipc.FHandler.AddCallback('MOUNT_CONFIG'  ,@OnMountConfig);
 kipc.FHandler.AddCallback('Delete'        ,@OnDelete);
 kipc.FHandler.AddCallback('Mount'         ,@OnMount);
 kipc.FHandler.AddCallback('IsActiveMount' ,@OnIsActiveMount);
 kipc.FHandler.AddCallback('Umount'        ,@OnUmount);
 kipc.FHandler.AddCallback('GetMountInfo'  ,@OnGetMountInfo);
 kipc.FHandler.AddCallback('Backup'        ,@OnBackup);
 kipc.FHandler.AddCallback('CheckBackup'   ,@OnCheckBackup);
 kipc.FHandler.AddCallback('RestoreBackup' ,@OnRestoreBackup);
 kipc.FHandler.AddCallback('GetEventResult',@OnGetEventResult);
 kipc.FHandler.AddCallback('GetProgress'   ,@OnGetProgress);
 kipc.FHandler.AddCallback('ClearProgress' ,@OnClearProgress);
 kipc.FHandler.AddCallback('SaveIcon'      ,@OnSaveIcon);
 kipc.FHandler.AddCallback('LoadIcon'      ,@OnLoadIcon);
 kipc.FHandler.AddCallback('SetParam'      ,@OnSetParam);
 kipc.FHandler.AddCallback('GetParam'      ,@OnGetParam);
 kipc.FHandler.AddCallback('SetupMemory'   ,@OnSetupMemory);
 kipc.FHandler.AddCallback('ReadMemory'    ,@OnReadMemory);
 kipc.FHandler.AddCallback('SetWriteSlot'  ,@OnSetWriteSlot);
 kipc.FHandler.AddCallback('WriteMemory'   ,@OnWriteMemory);
 kipc.FHandler.AddCallback('SyncMemory'    ,@OnSyncMemory);
 kipc.FHandler.AddCallback('DirNameSearch' ,@OnDirNameSearch);
 //
 inherited;
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

procedure TSaveDataBackendProcess.DoExit();
begin
 kipc.Disconnect();
 UmountAllForce();
 SendCmd(TCmdExitProc.Create(0));
end;

function TSaveDataBackendProcess.OnExitProc(Value:TIpcValue):TIpcValue; //EXIT_PROC
begin
 Result:=0;
 DoExit();
end;

procedure TSaveDataBackendConnect.SendMountConfig();
var
 data:TGameMountConfigExport;
begin
 data:=GameMountConfigExport;

 kipc.InvokeSync('MOUNT_CONFIG',TIpcValue.&Object(data));

 FreeAndNil(data);
end;

procedure TSaveDataBackendConnect.UmountAllForce;
var
 slot_id:Integer;
begin
 For slot_id:=0 to High(MountSlots) do
 if (MountSlots[slot_id]) then
 begin
  Writeln('Force umount ', mount_savedata_slot_name[slot_id]);
  vfs_mountroot.unmount_from_sandbox(pchar(mount_savedata_slot_name[slot_id]),MNT_FORCE);
 end;
end;

function TSaveDataBackendProcess.OnMountConfig(Value:TIpcValue):TIpcValue; //MOUNT_CONFIG
var
 data:TGameMountConfigExport;
begin
 Result:=0;

 data:=TGameMountConfigExport(Value.GetObject(TGameMountConfigExport));
 if (data=nil) then Exit;

 GameMountConfigImport(data);

 Writeln('[MOUNT_CONFIG]');
 Writeln(' ATTRIBUTE =0x',HexStr(data.ATTRIBUTE,8));
 Writeln(' LocalDir  =',data.LocalDir  );
 Writeln(' TitleId   =',data.TitleId   );
 Writeln(' InstallDir=',data.InstallDir);

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
 fs_src:=GameMountConfig.GetSaveDataFolder   (_user_id,_titleId,_dirName);
 fs_dst:=GameMountConfig.GetSaveDataBackupDst(_user_id,_titleId,_dirName);
 fs_old:=GameMountConfig.GetSaveDataBackupOld(_user_id,_titleId,_dirName);
 fs_new:=GameMountConfig.GetSaveDataBackupNew(_user_id,_titleId,_dirName);
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

 if (strncasecmp(@GameMountConfig.InstallDir,
                 @titleId,
                 SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)<>0) then
 begin
  //trying to delete another game?
  //check FINGERPRINT?
 end;

 Progress:=1/5;

 DoDelete(@Progress,1/5);

 gSaveDataBackend.SetProgressJob(nil);
 Unlock;
end;

function TDeleteJob.GetProgress:Single;
begin
 Result:=Progress;
end;

function TSaveDataBackendProcess.OnDelete(Value:TIpcValue):TIpcValue; //Delete
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
  titleId:=@GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 if MountManager.IsActiveMount(data.userId,titleId,dirName) then
 begin
  Result:=SCE_SAVE_DATA_ERROR_BUSY;
 end else
 begin

  fs_src:=GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

  if LockDirManager.LockDir(fs_src) then
  begin
   job:=TDeleteJob.Create(kipc.HoldResult);
   job.Init(data.userId,titleId,dirName);

   SetProgressJob(job);
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
   titleId:=@GameMountConfig.InstallDir;
  end;

  fs_src:=GameMountConfig.GetSaveDataFolder(data.userId,titleId,@data.dirName.data);

  Result:=vfs_mountroot.mount_into_sandbox('ufs',
                                           pchar(mount_savedata_slot_name[output.slot_id]),
                                           pchar(fs_src),
                                           nil,
                                           ord((data.mountMode and SDMM_RDONLY)<>0)*MNT_RDONLY or
                                           MNT_PFS_32K);
  if (Result=0) then
  begin
   MountSlots[output.slot_id]:=True;
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
  //
  procedure Init(const _data:TMount);
  function  Lock():Boolean;
  procedure UnLock();
  function  CreateParamSfo():Boolean;
  function  OpenParamSfo():Integer;
  function  SaveParamSfo():Integer;
  function  MountParamSfo():Integer;
  function  CreateTmpFiles():Boolean;
  function  CheckMountData(is_created:Boolean):Integer;
  function  CreateMount(force:Boolean):Integer;
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

 param_sfo.New(data.userId,@data.titleId.data,@data.dirName.data,data.blocks,gSaveDataBackend.systemLang);

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
   titleId:=@GameMountConfig.TitleId;
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

 if (p_proc.p_sdk_version<$4500000) then
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

function TMountJob.CheckMountData(is_created:Boolean):Integer;
begin
 Result:=0;

 if (p_proc.p_sdk_version < $3000000) then
 begin
  //
 end else
 if (not is_created) or
    ((GameMountConfig.ATTRIBUTE and $80000)<>0) or
    (data.blocks < 32769) then
 begin
  //
 end else
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 if data.Transfering then
 begin
  //TODO: SAVE_DATA_TRANSFER_TITLE_ID_LIST
  //SCE_SAVE_DATA_ERROR_PARAMSFO_TRANSFER_TITLE_ID_NOT_FOUND
 end;

end;

function TMountJob.CreateMount(force:Boolean):Integer;
var
 ficon:RawByteString;
 icon_data:Pointer;
 icon_size:Ptrint;
begin
 Progress:=2/9;

 Result:=CheckMountData(True);
 if (Result<>0) then Exit;

 if force then
 begin
  if not DeleteDirectory(fs_src,True) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
  end;
 end;

 Progress:=3/9;

 if not ForceDirectories(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
 end;

 Progress:=4/9;

 if not CreateParamSfo then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 Progress:=5/9;

 if not CreateTmpFiles then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 Progress:=6/9;

 if ((data.mountMode and SDMM_COPY_ICON)<>0) then
 begin
  ficon:=ExcludeTrailingPathDelimiter(GameMountConfig.Game)+unix_to_host('/sce_sys/save_data.png');

  icon_data:=AllocMem($1C800);

  icon_size:=ReadFromFile(ficon,icon_data,$1C800);

  if (icon_size<=0) then
  begin
   //
  end else
  begin
   if CheckPng(icon_data,icon_size)=0 then
   begin
    SaveIcon(fs_src,icon_data,icon_size);
   end;
  end;

  FreeMem(icon_data);
 end;

 Progress:=7/9;

 update_mtime(fs_src,mtime);
end;

function TMountJob.OpenMount():Integer;
begin
 Progress:=2/9;

 Result:=CheckMountData(False);
 if (Result<>0) then Exit;

 Progress:=4/9;

 Result:=OpenParamSfo();
 if (Result<>0) then Exit;

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

 if (p_proc.p_sdk_version < $1700000) then
 begin
  data.mountMode:=data.mountMode and (not SDMM_COPY_ICON);
 end;

 if (p_proc.p_sdk_version < $4500000) then
 begin
  data.mountMode:=data.mountMode and (not SDMM_CREATE2);
 end;

 titleId:=@data.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 if ((data.mountMode and SDMM_RDWR)<>0) then
 if (strncasecmp(@GameMountConfig.InstallDir,
                 titleId,
                 SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)<>0) then
 begin
  //trying to mount another game with RW?
  //check FINGERPRINT?
 end;

 is_locked:=False;
 slot_id:=0;

 output.result:=gSaveDataBackend.MountManager.GetFreeSlotId(data.userId,
                                                            titleId,
                                                            dirName,
                                                            data.Internal,
                                                            slot_id);

 Progress:=1/9;

 if (output.result=0) then
 begin

  fs_src:=GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

  is_locked:=Lock();

  if not is_locked then
  begin
   output.result:=SCE_SAVE_DATA_ERROR_BACKUP_BUSY;
  end else
  begin
   if SaveDataExists(fs_src) then
   begin

    //if (output.result=0) then

    //replace or exists error
    if ((data.mountMode and SDMM_CREATE2)<>0) then
    begin
     //output.result:=OpenParamSfo(); //TODO: check

     //force
     output.result:=CreateMount(True);
     //
     output.mountStatus:=SCE_SAVE_DATA_MOUNT_STATUS_CREATED;
     //
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
     output.result:=CreateMount(False);
     //
     output.mountStatus:=SCE_SAVE_DATA_MOUNT_STATUS_CREATED;
     //
    end else
    begin
     //error
     output.result:=SCE_SAVE_DATA_ERROR_NOT_FOUND;
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

    gSaveDataBackend.MountManager.SetMount(slot_id,minfo);

    //out
    output.slot_id       :=slot_id;
    output.requiredBlocks:=0; //TODO
   end;

  end;

 end;

 Progress:=9/9;

 gSaveDataBackend.SetProgressJob(nil);

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

function TSaveDataBackendProcess.OnMount(Value:TIpcValue):TIpcValue; //Mount
var
 data:TMount;

 job:TMountJob;
begin
 Result:=0;
 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 job:=TMountJob.Create(kipc.HoldResult);
 job.Init(data);

 SetProgressJob(job);
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

 if (MountSlots[slot_id]=False) then
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
   MountSlots[slot_id]:=False;

   //free
   Result:=kipc.InvokeSync2('Umount',@data,sizeof(data));
  end;

 end;

end;

type
 TMountInfo=packed record
  result    :QWORD;
  blocks    :SceSaveDataBlocks;
  freeBlocks:SceSaveDataBlocks;
 end;

function TSaveDataBackendProcess.OnIsActiveMount(Value:TIpcValue):TIpcValue; //IsActiveMount
var
 slot_id:Integer;
begin
 Result:=0;
 slot_id:=Value.GetDWORD;

 if (DWORD(slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not MountManager.IsActiveMount(slot_id) then
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

 if not gSaveDataBackend.MountManager.IsActiveMount(data.slot_id) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 titleId:=@minfo.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@GameMountConfig.InstallDir;
 end;

 dirName:=@minfo.dirName.data;

 fs_src:=GameMountConfig.GetSaveDataFolder(minfo.userId,titleId,dirName);

 //
 err:=0;

 if not force then
 begin
  err:=UmountParamSfo();
 end;

 if (err=0) then
 begin
  //free
  gSaveDataBackend.MountManager.FreeMount(data.slot_id);

  Unlock;
 end;

 if not force then
 begin
  update_mtime(fs_src,minfo.mtime);
 end;

 if (err=0) and data.backup and ((minfo.mountMode and SDMM_RDWR)<>0) then
 begin
  gSaveDataBackend.SendBackupJob(minfo.userId,
                                @minfo.titleId,
                                @minfo.dirName,
                                @minfo.fingerprint,
                                SDET_UMOUNT_BACKUP_END);
 end;

 Result:=err;
end;

function TSaveDataBackendProcess.OnUmount(Value:TIpcValue):TIpcValue; //Umount
var
 data:TUmount;
 job:TUmountJob;
begin
 Result:=0;
 data:=Default(TUmount);
 Value.MoveTo(@data,SizeOf(data));

 if (DWORD(data.slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not MountManager.IsActiveMount(data.slot_id) then
 begin
  Result:=SCE_SAVE_DATA_ERROR_NOT_MOUNTED;
 end else
 begin
  Result:=0;

  job:=TUmountJob.Create(kipc.HoldResult);

  job.minfo:=MountManager.GetMount(data.slot_id);
  job.data :=data;

  SendCmd(job);
 end;

end;

procedure TSaveDataBackendProcess.UmountAllForce();
var
 i:Integer;
 job:TUmountJob;
begin

 For i:=0 to MountManager.max-1 do
 if MountManager.IsActiveMount(i) then
 begin
  job:=TUmountJob.Create(0);

  job.minfo:=MountManager.GetMount(i);
  job.data.slot_id:=i;
  job.force:=True;

  SendCmd(job);
 end;

end;

function TSaveDataBackendConnect.GetMountInfo(slot_id:Integer;info:pSceSaveDataMountInfo):Integer;
var
 Value:TIpcValue;
 data:TMountInfo;
begin
 if (DWORD(slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if (MountSlots[slot_id]=False) then
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

function TSaveDataBackendProcess.OnGetMountInfo(Value:TIpcValue):TIpcValue; //GetMountInfo
var
 slot_id:Integer;
 mount:TMountSlot;
 fs_src:RawByteString;
 output:TMountInfo;
 blocks:Int64;
begin
 Result:=0;
 slot_id:=Value.GetDWORD;

 if (DWORD(slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not MountManager.IsActiveMount(slot_id) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 mount:=MountManager.GetMount(slot_id);

 fs_src:=GameMountConfig.GetSaveDataFolder(mount.userId,@mount.titleId,@mount.dirName);

 blocks:=GetFreeBlocks(fs_src,mount.max_blocks);

 output.result    :=0;
 output.blocks    :=mount.max_blocks;
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

function TSaveDataBackendProcess.OnBackup(Value:TIpcValue):TIpcValue; //Backup
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
  titleId:=@GameMountConfig.InstallDir;
 end;

 Result:=SendBackupJob(data.userId,
                      titleId,
                      @data.dirName.data,
                      @data.fingerprint,
                      SDET_BACKUP_END);
end;

type
 TCustomBackupJob=class(TCustomDirJob)
  //
  param_sfo:t_savedata_sfo_values;
  Progress:Single;
  //
  function OpenParamSfo(const fdir:RawByteString):Integer;
  function SaveParamSfo(const fdir:RawByteString):Integer;
  function Prepare:Boolean;
  function CheckBackup:Integer;
  function GetProgress:Single; override;
 end;

 TBackupJob=class(TCustomBackupJob)
  event_type:Byte;
  function Backup:Boolean;
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

function TCustomBackupJob.SaveParamSfo(const fdir:RawByteString):Integer;
var
 fname:RawByteString;
begin
 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/param.sfo');

 if param_sfo.SaveToFile(fname) then
 begin
  Result:=0
 end else
 begin
  Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
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

function TCustomBackupJob.CheckBackup:Integer;
begin
 if not SaveDataExists(fs_dst) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_FOUND);
 end;

 Progress:=5/12;

 Result:=OpenParamSfo(fs_dst);

 Progress:=6/12;
end;

function TCustomBackupJob.GetProgress:Single;
begin
 Result:=Progress;
end;

function TBackupJob.Backup:Boolean;
begin
 Result:=False;

 if not Prepare then Exit;

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

 Result:=True;
end;

function TBackupJob.Run:TIpcValue;
var
 err:Integer;
 res:Boolean;
begin
 Result:=0;
 res:=Backup;

 //SCE_SAVE_DATA_ERROR_NO_SPACE_FS
 case res of
  True :err:=0;
  False:err:=SCE_SAVE_DATA_ERROR_INTERNAL;
 end;

 if (event_type<>0) then
 begin
  gSaveDataBackend.EventQueue.Push(event_type,err,user_id,@titleId,@dirName);
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
 err:Integer;
begin

 if Prepare then
 begin
  err:=CheckBackup;
 end else
 begin
  err:=SCE_SAVE_DATA_ERROR_INTERNAL;
 end;

 if (err=0) then
 begin

  if Restore then
  begin
   err:=0;
  end else
  begin
   err:=SCE_SAVE_DATA_ERROR_INTERNAL;
  end;

 end;

 Result:=err;

 gSaveDataBackend.SetProgressJob(nil);
 UnLock;
end;

function TSaveDataBackendProcess.SendBackupJob(userId     :SceUserServiceUserId;
                                               titleId    :pchar;
                                               dirName    :pchar;
                                               fingerprint:pSceSaveDataFingerprint;
                                               event_type :Byte):Integer;
var
 fs_src:RawByteString;
 job:TBackupJob;
begin
 Result:=0;

 if MountManager.IsActiveMount(userId,titleId,dirName) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BUSY);
 end;

 fs_src:=GameMountConfig.GetSaveDataFolder(userId,titleId,dirName);

 if not SaveDataExists(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_FOUND);
 end;

 if not LockDirManager.LockDir(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BACKUP_BUSY);
 end;

 job:=TBackupJob.Create(0); //async
 job.Init(userId,titleId,dirName);
 job.event_type:=event_type;

 Result:=job.OpenParamSfo(fs_src);
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
  err:=CheckBackup;
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

function TSaveDataBackendProcess.OnCheckBackup(Value:TIpcValue):TIpcValue; //CheckBackup
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
  titleId:=@GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 fs_src:=GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

 if not SaveDataExists(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_FOUND);
 end;

 if not LockDirManager.LockDir(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BACKUP_BUSY);
 end;

 job:=TCheckJob.Create(kipc.HoldResult);
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

 Result:=kipc.InvokeSync2('RestoreBackup',@data,sizeof(data));
end;

function TSaveDataBackendProcess.OnRestoreBackup(Value:TIpcValue):TIpcValue; //RestoreBackup
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
  titleId:=@GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 if MountManager.IsActiveMount(data.userId,titleId,dirName) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BUSY);
 end;

 fs_src:=GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

 if not SaveDataExists(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_FOUND);
 end;

 if not LockDirManager.LockDir(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BACKUP_BUSY);
 end;

 job:=TRestoreJob.Create(kipc.HoldResult);
 job.Init(data.userId,titleId,dirName);

 SetProgressJob(job);
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

function TSaveDataBackendProcess.OnGetEventResult(Value:TIpcValue):TIpcValue; //GetEventResult
var
 data:TEventResult;
begin
 data:=Default(TEventResult);

 if gSaveDataBackend.EventQueue.Pop(data.event) then
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

function TSaveDataBackendProcess.OnGetProgress(Value:TIpcValue):TIpcValue; //GetProgress
var
 data:TGetProgress;
begin
 mtx_lock(Progress.mtx);

  if (Progress.cmd=nil) then
  begin
   //
  end else
  begin
   Progress.Value:=Progress.cmd.GetProgress;
  end;

  data.result  :=0;
  data.progress:=Progress.Value;

  Result:=TIpcValue.New(@data,SizeOf(data));

 mtx_unlock(Progress.mtx);
end;

function TSaveDataBackendConnect.ClearProgress():Integer;
begin
 Result:=kipc.InvokeSync2('ClearProgress');
end;

function TSaveDataBackendProcess.OnClearProgress(Value:TIpcValue):TIpcValue; //ClearProgress
begin
 Result:=0;
 mtx_lock(Progress.mtx);

  Progress.Value:=0;

 mtx_unlock(Progress.mtx);
end;

procedure TSaveDataBackendProcess.SetProgressJob(cmd:TCustomCommand);
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
  gSaveDataBackend.MountManager.SetMtime(slot,mtime);
 end;

 Result:=err;
end;

function TSaveDataBackendProcess.OnSaveIcon(Value:TIpcValue):TIpcValue; //SaveIcon
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

 if not MountManager.IsActiveMount(data^.slot) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 if MountManager.IsReadOnly(data^.slot) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BAD_MOUNTED);
 end;

 prev:=MountManager.GetMount(data^.slot);

 job:=TSaveIconJob.Create(kipc.HoldResult);
 job.fs_src:=GameMountConfig.GetSaveDataFolder(prev.userId,@prev.titleId.data,@prev.dirName.data);

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

function TSaveDataBackendProcess.OnLoadIcon(Value:TIpcValue):TIpcValue; //LoadIcon
var
 slot_id:Integer;
 prev:TMountSlot;
 job:TLoadIconJob;
begin
 Result:=0;

 slot_id:=Value.GetDWORD;

 if (DWORD(slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not MountManager.IsActiveMount(slot_id) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 prev:=MountManager.GetMount(slot_id);

 job:=TLoadIconJob.Create(kipc.HoldResult);
 job.fs_src:=GameMountConfig.GetSaveDataFolder(prev.userId,@prev.titleId.data,@prev.dirName.data);

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
  gSaveDataBackend.MountManager.SetMtime(slot,mtime);
 end;

 Result:=err;
end;

function TSaveDataBackendProcess.OnSetParam(Value:TIpcValue):TIpcValue; //SetParam
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

 if not MountManager.IsActiveMount(data^.slot) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 if MountManager.IsReadOnly(data^.slot) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BAD_MOUNTED);
 end;

 MountManager.SetParam(data^.slot,data^.ptype,@data^.data,len);

 minfo:=MountManager.GetMount(data^.slot);

 job:=TSetParamJob.Create(kipc.HoldResult);
 job.fs_src   :=GameMountConfig.GetSaveDataFolder(minfo.userId,@minfo.titleId.data,@minfo.dirName.data);
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

function TSaveDataBackendProcess.OnGetParam(Value:TIpcValue):TIpcValue; //GetParam
var
 input:TGetParam;
 data :p_output_buf;
begin
 Result:=0;

 input:=Default(TGetParam);
 Value.MoveTo(@input,sizeof(input));

 if (DWORD(input.slot)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not MountManager.IsActiveMount(input.slot) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 data:=AllocMem(sizeof(t_output_buf)+$1c800);

 MountManager.GetParam(input.slot,input.ptype,@data^.data,@data^.size);

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

 RestoreJob:TRestoreJob;
begin
 err:=0;

 titleId:=@data.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 blocks:=data.blocks;

 is_locked:=False;

 fs_src:=GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

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
  end else
  begin
   //create
   err:=CreateMount(False);
  end;

  ///
  if (err=SCE_SAVE_DATA_ERROR_BROKEN) then
  begin
   RestoreJob:=TRestoreJob.Create(0);
   RestoreJob.Init(data.userId,titleId,dirName);

   if RestoreJob.Prepare then
   begin
    err:=RestoreJob.CheckBackup;
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
    err:=CreateMount(False);
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

 Result:=err;
end;

function TSaveDataBackendProcess.OnSetupMemory(Value:TIpcValue):TIpcValue; //SetupMemory
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

 node:=SetupMemoryManager.Setup(input);
 if (node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 err:=node^.CreateBuffers();
 if (err<>0) then Exit(err);

 minfo:=Default(TMount);
 minfo.userId      :=input.userId;
 minfo.dirName.data:=sdmemory_slot_name[input.slotId];
 minfo.blocks      :=GetBlocks(input.memorySize);
 minfo.mountMode   :=SDMM_RDONLY;

 job:=TSetupMemoryJob.Create(kipc.HoldResult);
 job.Init(minfo);

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

function TSaveDataBackendProcess.OnReadMemory(Value:TIpcValue):TIpcValue; //ReadMemory
var
 input:TReadMemory;
 prev:TMountSlot;
 job:TReadMemoryJob;
begin
 Result:=0;

 input:=Default(TReadMemory);
 Value.MoveTo(@input,sizeof(input));

 if (DWORD(input.slot_id)>=TMountManager.max) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not MountManager.IsActiveMount(input.slot_id) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 prev:=MountManager.GetMount(input.slot_id);

 job:=TReadMemoryJob.Create(kipc.HoldResult);
 job.fs_src:=GameMountConfig.GetSaveDataFolder(prev.userId,@prev.titleId.data,@prev.dirName.data);

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

function TSaveDataBackendProcess.OnSetWriteSlot(Value:TIpcValue):TIpcValue; //SetWriteSlot
var
 data:TSetWriteSlot;
 node:PSetupMemoryNode;
begin
 Result:=0;
 pWriteSlot:=nil;

 data:=Default(TSetWriteSlot);
 Value.MoveTo(@data,sizeof(data));

 node:=SetupMemoryManager.Get(data.userId,data.slotId);
 if (node=nil) then Exit;

 if (node^.is_setup) then
 if (data.bufferId<node^.data.bufferNum) then
 begin
  node^.FbufferId:=data.bufferId;
  pWriteSlot:=node;
 end;

end;

function TSaveDataBackendProcess.OnWriteMemory(Value:TIpcValue):TIpcValue; //WriteMemory
var
 src_addr:Pointer;
 src_size:QWORD;

 buf:PSdMemoryBuffer;
 is_writed:Boolean;
begin
 Result:=0;
 if (pWriteSlot=nil) then Exit;

 buf:=@pWriteSlot^.sd_buffers[pWriteSlot^.FbufferId];

 src_addr:=Value.GetBuf;
 src_size:=Value.GetLen;

 if (src_size>buf^.Fsize) then src_size:=buf^.Fsize;

 is_writed:=CompareByte(src_addr^,buf^.Paddr^,src_size)<>0;

 if is_writed then
 begin
  mtx_lock(pWriteSlot^.mtx);

   Move(src_addr^,buf^.Paddr^,src_size);

   pWriteSlot^.is_writed:=True;

  mtx_unlock(pWriteSlot^.mtx);

  if (pWriteSlot^.job_count<2) then
  begin
   SendSyncJob(pWriteSlot^.data.userId,pWriteSlot^.data.slotId,0,False);
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
var
 ficon:RawByteString;
begin
 Result:=0;

 if (iconBufSize=0) then
 begin
  //CopyIcon

  ficon:=ExcludeTrailingPathDelimiter(GameMountConfig.Game)+unix_to_host('/sce_sys/save_data.png');

  iconData:=AllocMem($1C800);

  iconBufSize:=ReadFromFile(ficon,iconData,$1C800);

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

  FreeMem(iconData);
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

  if gSaveDataBackend.MountManager.IsActiveMount(nslot^.data.userId,@titleId.data,@dirName.data) then
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
     strlcopy(@params.title,GET_MAINTITLE_DEFAULT(gSaveDataBackend.systemLang),SCE_SAVE_DATA_TITLE_MAXSIZE);
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

   gSaveDataBackend.EventQueue.Push(
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

  gSaveDataBackend.SendBackupJob(nslot^.data.userId,
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
 System.InterlockedDecrement(nslot^.job_count);
end;

function TSaveDataBackendProcess.SendSyncJob(userId,slotId,option:DWORD;hold:Boolean):Integer;
var
 node    :PSetupMemoryNode;
 titleId :pchar;
 dirName :pchar;
 job     :TSyncMemoryJob;
 fs_src  :RawByteString;
 is_async:Boolean;
begin
 Result:=0;

 node:=SetupMemoryManager.Get(userId,slotId);
 if (node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 titleId:=@GameMountConfig.InstallDir;
 dirName:=sdmemory_slot_name[slotId];

 fs_src  :=GameMountConfig.GetSaveDataFolder(userId,titleId,dirName);
 is_async:=(option and 1)=0;

 if is_async then
 begin
  job:=TSyncMemoryJob.Create(0);
 end else
 begin

  if MountManager.IsActiveMount(userId,titleId,dirName) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_BUSY);
  end;

  if not LockDirManager.LockDir(fs_src) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_BUSY_FOR_SAVING);
  end;

  if hold then
  begin
   job:=TSyncMemoryJob.Create(kipc.HoldResult);
  end else
  begin
   job:=TSyncMemoryJob.Create(0);
  end;

 end;

 System.InterlockedIncrement(node^.job_count);

 job.fs_src      :=fs_src;
 job.nslot       :=node;
 job.titleId.data:=titleId;
 job.dirName.data:=dirName;
 job.is_async    :=is_async;
 job.is_event    :=hold and is_async;

 SendCmd(job);
end;

function TSaveDataBackendProcess.OnSyncMemory(Value:TIpcValue):TIpcValue; //SyncMemory
var
 input:TSyncMemory;
begin
 input:=Default(TSyncMemory);
 Value.MoveTo(@input,sizeof(input));

 Result:=SendSyncJob(input.userId,input.slotId,input.option,True);
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

function TSaveDataBackendProcess.OnDirNameSearch(Value:TIpcValue):TIpcValue; //DirNameSearch
var
 input:TDirNameSearch;
 titleId:pchar;
 job:TDirNameSearchJob;
begin
 Result:=0;
 input:=Default(TDirNameSearch);
 Value.MoveTo(@input,sizeof(input));

 titleId:=@input.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@GameMountConfig.InstallDir;
 end;

 job:=TDirNameSearchJob.Create(kipc.HoldResult);

 job.fs_src:=GameMountConfig.GetSaveDataFolder(input.userId,titleId,'');
 job.data  :=input;

 SendCmd(job);
end;



end.



