unit SaveDataBackend;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
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
 TMount=packed record
  userId     :SceUserServiceUserId;
  titleId    :SceSaveDataTitleId;
  dirName    :SceSaveDataDirName;
  fingerprint:SceSaveDataFingerprint;
  blocks     :SceSaveDataBlocks;
  mountMode  :DWORD; //SceSaveDataMountMode
  Transfering:Boolean;
 end;

 TMountResult=packed record
  result        :Integer;
  mountStatus   :WORD;
  slot_id       :WORD;
  requiredBlocks:SceSaveDataBlocks;
 end;

 TSaveDataBackendConnect=class
  kipc    :THostIpcPipeKERN;
  hProcess:THandle;
  fork_pid:Integer;
  //
  MountSlots:array[0..15] of Boolean;
  //
  Constructor Create;
  Destructor  Destroy; override;
  procedure   SendMountConfig();
  procedure   UmountAllForce;
  function    DoDelete      (del:pSceSaveDataDelete):Integer;
  function    DoMount       (mount:pSceSaveDataMount;var pResult:TMountResult;Transfering:Boolean):Integer;
  function    DoUmount      (slot_id:Integer;backup:boolean):Integer;
  function    GetMountInfo  (slot_id:Integer;info:pSceSaveDataMountInfo):Integer;
  function    DoBackup      (backup:pSceSaveDataBackup):Integer;
  function    CheckBackup   (check:pSceSaveDataCheckBackupData):Integer;
  function    RestoreBackup (restore:pSceSaveDataRestoreBackupData):Integer;
  function    GetEventResult(event:pSceSaveDataEvent):Integer;
  function    SaveIcon      (slot_id:Integer;icon:pSceSaveDataIcon):Integer;
  function    LoadIcon      (slot_id:Integer;icon:pSceSaveDataIcon):Integer;
  function    SetParam      (slot_id     :Integer;
                             paramType   :SceSaveDataParamType;
                             paramBuf    :Pointer;
                             paramBufSize:QWORD):Integer;
  function    GetParam      (slot_id     :Integer;
                             paramType   :SceSaveDataParamType;
                             paramBuf    :Pointer;
                             paramBufSize:QWORD;
                             gotSize     :PQWORD):Integer;
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
   node:TQNode;
   rid :DWORD;
  Constructor Create(_rid:DWORD);
  function    Run:TIpcValue; virtual;
  procedure   Invoke(value:TIpcValue);
 end;

 THostIpcPipeSave=class(THostIpcPipe)
  event:t_event;
  Constructor Create;
  procedure   Recv_pipe; override;
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
  EventQueue:TEventQueue;
  //
  Constructor Create;
  procedure   SendCmd(cmd:TCustomCommand);
  function    RecvCmd(var cmd:TCustomCommand):Boolean;
  function    OnExitProc      (Value:TIpcValue):TIpcValue; //EXIT_PROC
  function    OnMountConfig   (Value:TIpcValue):TIpcValue; //MOUNT_CONFIG
  function    OnDelete        (Value:TIpcValue):TIpcValue; //Delete
  function    OnMount         (Value:TIpcValue):TIpcValue; //Mount
  function    OnIsActiveMount (Value:TIpcValue):TIpcValue; //IsActiveMount
  function    OnUmount        (Value:TIpcValue):TIpcValue; //Umount
  function    OnGetMountInfo  (Value:TIpcValue):TIpcValue; //GetMountInfo
  function    OnBackup        (Value:TIpcValue):TIpcValue; //Backup
  function    SendBackupJob   (userId     :SceUserServiceUserId;
                               titleId    :pchar;
                               dirName    :pchar;
                               fingerprint:pSceSaveDataFingerprint;
                               umount     :Boolean):Integer;
  function    OnCheckBackup   (Value:TIpcValue):TIpcValue; //CheckBackup
  function    OnRestoreBackup (Value:TIpcValue):TIpcValue; //RestoreBackup
  function    OnGetEventResult(Value:TIpcValue):TIpcValue; //GetEventResult
  function    OnSaveIcon      (Value:TIpcValue):TIpcValue; //SaveIcon
  function    OnLoadIcon      (Value:TIpcValue):TIpcValue; //LoadIcon
  function    OnSetParam      (Value:TIpcValue):TIpcValue; //SetParam
  function    OnGetParam      (Value:TIpcValue):TIpcValue; //GetParam
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

 gSaveDataBackend.OnExitProc(Default(TIpcValue));
end;

function job_thread(parameter:pointer):ptrint;
var
 cmd:TCustomCommand;
begin
 Result:=0;
 repeat
  ev_wait(gSaveDataBackend.job_event);

  cmd:=nil;
  while gSaveDataBackend.RecvCmd(cmd) do
  begin
   cmd.Invoke(cmd.Run);
   cmd.Free;
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
 EventQueue.Init;
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
 kipc.FHandler.AddCallback('SaveIcon'      ,@OnSaveIcon);
 kipc.FHandler.AddCallback('LoadIcon'      ,@OnLoadIcon);
 kipc.FHandler.AddCallback('SetParam'      ,@OnSetParam);
 kipc.FHandler.AddCallback('GetParam'      ,@OnGetParam);
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

function TSaveDataBackendProcess.OnExitProc(Value:TIpcValue):TIpcValue; //EXIT_PROC
begin
 Result:=0;
 kipc.Disconnect();
 SendCmd(TCmdExitProc.Create(0));
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

///

type
 TDeleteJob=class(TCustomDirJob)
  function Run:TIpcValue; override;
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

 //dont check errors
 game_mount.DeleteDirectory(fs_dst,False);
 game_mount.DeleteDirectory(fs_old,False);
 game_mount.DeleteDirectory(fs_new,False);
 game_mount.DeleteDirectory(fs_src,False);

 Unlock;
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

   SendCmd(job);
  end else
  begin
   Result:=SCE_SAVE_DATA_ERROR_BACKUP_BUSY;
  end;

 end;

end;

function TSaveDataBackendConnect.DoMount(mount:pSceSaveDataMount;var pResult:TMountResult;Transfering:Boolean):Integer;
var
 data:TMount;
 Value:TIpcValue;

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
  data.blocks     :=mount^.blocks;
  data.mountMode  :=mount^.mountMode;
  data.Transfering:=Transfering;

 Value:=kipc.InvokeSync('Mount',TIpcValue.Static(@data,sizeof(data)));

 FillChar(pResult,SizeOf(pResult),0);
 Value.MoveTo(@pResult,SizeOf(pResult));

 Value.Free;

 Result:=pResult.result;

 if (Result=0) then
 begin

  titleId:=@data.titleId.data;
  if (titleId[0]=#0) then
  begin
   titleId:=@GameMountConfig.InstallDir;
  end;

  fs_src:=GameMountConfig.GetSaveDataFolder(data.userId,titleId,@data.dirName.data);

  Result:=vfs_mountroot.mount_into_sandbox('ufs',
                                           pchar(mount_savedata_slot_name[pResult.slot_id]),
                                           pchar(fs_src),
                                           nil,
                                           ord((data.mountMode and SDM_RDONLY)<>0)*MNT_RDONLY or
                                           MNT_PFS_32K);
  if (Result=0) then
  begin
   MountSlots[pResult.slot_id]:=True;
  end else
  begin
   Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
   //Umount
   kipc.InvokeSync2('Umount',@pResult.slot_id,sizeof(pResult.slot_id));
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
  procedure Init(const _data:TMount);
  function  Lock():Boolean;
  procedure UnLock();
  function  CreateParamSfo():Boolean;
  function  OpenParamSfo():Integer;
  function  MountParamSfo():Integer;
  function  CreateTmpFiles():Boolean;
  function  CheckMountData(is_created:Boolean):Integer;
  function  CreateMount(force:Boolean):Integer;
  function  OpenMount():Integer;
  //
  function  Run:TIpcValue; override;
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

 if ((data.mountMode and SDM_DESTRUCT_OFF)=0) then
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

function TMountJob.MountParamSfo():Integer;
var
 fname:RawByteString;
 titleId:PChar;
begin
 Result:=0;

 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/param.sfo');

 //update data to sfo
 if ((data.mountMode and SDM_RDWR)<>0) then
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
  if ((data.mountMode and SDM_DESTRUCT_OFF)=0) then
  begin
   param_sfo.PARAMS.corrupt_flag:=1;
  end;

  if not param_sfo.SaveToFile(fname) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
  end;
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
begin
 Result:=CheckMountData(True);
 if (Result<>0) then Exit;

 if force then
 begin
  if not DeleteDirectory(fs_src,True) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
  end;
 end;

 if not ForceDirectories(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
 end;

 if not CreateParamSfo then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
 if not CreateTmpFiles then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if ((data.mountMode and SDM_COPY_ICON)<>0) then
 begin
  Writeln('TODO:COPY_ICON');
 end;

end;

function TMountJob.OpenMount():Integer;
begin
 Result:=CheckMountData(False);
 if (Result<>0) then Exit;

 Result:=OpenParamSfo();
 if (Result<>0) then Exit;

 Result:=MountParamSfo();
 if (Result<>0) then Exit;
end;

function SaveDataExists(const fs_src:RawByteString):Boolean; forward;

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
  data.mountMode:=data.mountMode and (not SDM_COPY_ICON);
 end;

 if (p_proc.p_sdk_version < $4500000) then
 begin
  data.mountMode:=data.mountMode and (not SDM_CREATE2);
 end;

 titleId:=@data.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 if ((data.mountMode and SDM_RDWR)<>0) then
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
                                                            slot_id);
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
    if ((data.mountMode and SDM_CREATE2)<>0) then
    begin
     //output.result:=OpenParamSfo(); //TODO: check

     //force
     output.result:=CreateMount(True);
     //
     output.mountStatus:=SCE_SAVE_DATA_MOUNT_STATUS_CREATED;
     //
    end else
    if ((data.mountMode and SDM_CREATE)<>0) then
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

    if ((data.mountMode and (SDM_CREATE2 or SDM_CREATE))<>0) then
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

    gSaveDataBackend.MountManager.SetMount(slot_id,minfo);

    //out
    output.slot_id       :=slot_id;
    output.requiredBlocks:=0; //TODO
   end;

  end;

 end;

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
 if (DWORD(slot_id)>15) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

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

 if (DWORD(slot_id)>15) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

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
 if ((minfo.mountMode and SDM_RDWR)<>0) then
 begin
  //mark in-free
  if ((minfo.mountMode and SDM_DESTRUCT_OFF)=0) then
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
 titleId:=@minfo.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@GameMountConfig.InstallDir;
 end;

 dirName:=@minfo.dirName.data;

 fs_src:=GameMountConfig.GetSaveDataFolder(minfo.userId,titleId,dirName);

 //

 err:=UmountParamSfo();

 if (err=0) then
 begin
  //free
  gSaveDataBackend.MountManager.FreeMount(data.slot_id);

  Unlock;
 end;

 if (err=0) and data.backup and ((minfo.mountMode and SDM_RDWR)<>0) then
 begin
  gSaveDataBackend.SendBackupJob(minfo.userId,
                                @minfo.titleId,
                                @minfo.dirName,
                                @minfo.fingerprint,
                                True);
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
 Value.MoveTo(@data,SizeOf(data));;

 if (DWORD(data.slot_id)>15) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

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

function TSaveDataBackendConnect.GetMountInfo(slot_id:Integer;info:pSceSaveDataMountInfo):Integer;
var
 Value:TIpcValue;
 data:TMountInfo;
begin
 if (DWORD(slot_id)>15) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

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

 if (DWORD(slot_id)>15) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not MountManager.IsActiveMount(slot_id) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 mount:=MountManager.GetMount(slot_id);

 fs_src:=GameMountConfig.GetSaveDataFolder(mount.userId,@mount.titleId,@mount.dirName);

 blocks:=GetDirectorySizeLikePFS(fs_src);
 blocks:=blocks+1024+4*1024+4*1024; //pulling

 blocks:=(blocks+(SCE_SAVE_DATA_BLOCK_SIZE-1)) div SCE_SAVE_DATA_BLOCK_SIZE;

 blocks:=mount.max_blocks-blocks-32;
 if (blocks<0) then blocks:=0;

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
                      False);
end;

type
 TCustomBackupJob=class(TCustomDirJob)
  param_sfo:t_savedata_sfo_values;
  //
  function OpenParamSfo(const fdir:RawByteString):Integer;
  function SaveParamSfo(const fdir:RawByteString):Integer;
  function Prepare:Boolean;
  function CheckBackup:Integer;
 end;

 TBackupJob=class(TCustomBackupJob)
  umount:Boolean;
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

function SaveDataExists(const fs_src:RawByteString):Boolean;
var
 fs_tmp:RawByteString;
begin
 fs_tmp:=fs_src+'_tmp_cp0';

 if DirectoryExists(fs_tmp) and (not DirectoryExists(fs_src)) then
 begin
  //try repair
  RenameFile(fs_tmp,fs_src);
 end;

 Result:=DirectoryExists(fs_src);
end;

function TCustomBackupJob.Prepare:Boolean;
begin
 Result:=False;

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

 Result:=True;
end;

function TCustomBackupJob.CheckBackup:Integer;
begin
 if not SaveDataExists(fs_dst) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_FOUND);
 end;

 Result:=OpenParamSfo(fs_dst);
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
 id,err:Integer;
 res:Boolean;
begin
 Result:=0;
 res:=Backup;

 case umount of
  True :id:=SCE_SAVE_DATA_EVENT_TYPE_UMOUNT_BACKUP_END;
  False:id:=SCE_SAVE_DATA_EVENT_TYPE_BACKUP_END;
 end;

 //SCE_SAVE_DATA_ERROR_NO_SPACE_FS
 case res of
  True :err:=0;
  False:err:=SCE_SAVE_DATA_ERROR_INTERNAL;
 end;

 gSaveDataBackend.EventQueue.Push(id,err,user_id,@titleId,@dirName);

 sleep(200);

 ///
 UnLock;
end;

function TRestoreJob.Restore:Boolean;
var
 fs_tmp:RawByteString;
begin
 Result:=False;

 fs_tmp:=fs_src+'_tmp_cp0';

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

 UnLock;
end;

function TSaveDataBackendProcess.SendBackupJob(userId     :SceUserServiceUserId;
                                               titleId    :pchar;
                                               dirName    :pchar;
                                               fingerprint:pSceSaveDataFingerprint;
                                               umount     :Boolean):Integer;
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
 job.umount:=umount;

 Result:=job.OpenParamSfo(fs_src);
 if (Result<>0) then
 begin
  job.UnLock;
  job.Free;
  Exit;
 end;

 SendCmd(job);
end;

function TSaveDataBackendConnect.CheckBackup(check:pSceSaveDataCheckBackupData):Integer;
var
 data:TBackup;
begin
 FillChar(data,SizeOf(data),0);
  data.userId     :=check^.userId;
 if (check^.titleId<>nil) then
  data.titleId    :=check^.titleId^;
 if (check^.dirName<>nil) then
  data.dirName    :=check^.dirName^;

 Result:=kipc.InvokeSync2('CheckBackup',@data,sizeof(data));

 //TODO result:
 //param      :pSceSaveDataParam;
 //icon       :pSceSaveDataIcon;
end;

type
 TCheckJob=class(TCustomBackupJob)
  function Run:TIpcValue; override;
 end;

function TCheckJob.Run:TIpcValue;
begin
 Result:=0;

 if Prepare then
 begin
  Result:=CheckBackup;
 end else
 begin
  Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
 end;

 //TODO result:
 //param      :pSceSaveDataParam;
 //icon       :pSceSaveDataIcon;

 UnLock;
end;

function TSaveDataBackendProcess.OnCheckBackup(Value:TIpcValue):TIpcValue; //CheckBackup
var
 data:TBackup;
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
  len :DWORD;
  data:array[0..116735] of Byte;
  //
  function  Run:TIpcValue; override;
 end;

function TSaveIconJob.Run:TIpcValue;
var
 err:Integer;
begin
 err:=CheckPng(@data,len);

 if (err=0) then
 begin
  if not SaveIcon(fs_src,@data,len) then
  begin
   err:=SCE_SAVE_DATA_ERROR_INTERNAL;
  end;
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

 if (DWORD(data^.slot)>15) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

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

 job.len:=len;
 Move(data^.data,job.data,len);

 SendCmd(job);
end;

function TSaveDataBackendConnect.LoadIcon(slot_id:Integer;icon:pSceSaveDataIcon):Integer;
label
 _memcpy;
const
 internal=False;
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
  function  Run:TIpcValue; override;
 end;

function TLoadIconJob.Run:TIpcValue;
var
 ficon:RawByteString;
 buf:t_load_icon_buf;
begin
 Result:=0;

 ficon:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/icon0.png');

 if not FileExists(ficon) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_FILE_NOT_FOUND);
 end;

 buf:=LoadIcon(ficon);

 if (buf.err=0) then
 begin
  Result:=TIpcValue.Inplace(buf.data,buf.data,buf.size);
 end else
 begin
  Result:=buf.err;
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

 if (DWORD(slot_id)>15) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

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
  param_sfo:t_savedata_sfo_values;
  //
  function Run:TIpcValue; override;
 end;

function TSetParamJob.Run:TIpcValue;
var
 fname:RawByteString;
begin
 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/param.sfo');

 if param_sfo.SaveToFile(fname) then
 begin
  Result:=0;
 end else
 begin
  Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
 end;
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

 if (DWORD(data^.slot)>15) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

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

 Value:=kipc.InvokeSync('GetParam',TIpcValue.New(@input,sizeof(input)));

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

 if (DWORD(input.slot)>15) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not MountManager.IsActiveMount(input.slot) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 data:=AllocMem(sizeof(t_output_buf)+$1c800);

 MountManager.GetParam(input.slot,input.ptype,@data^.data,@data^.size);

 Result:=TIpcValue.Inplace(data,data,sizeof(t_output_buf)+$1c800);
end;


end.



