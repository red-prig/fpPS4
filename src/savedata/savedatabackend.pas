unit SaveDataBackend;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 errno,
 LFQueue,
 windows,
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
 SceSaveData;

type
 TSaveDataMountResult=packed record
  result        :Integer;
  mountStatus   :DWORD;
  slot_id       :DWORD;
  requiredBlocks:SceSaveDataBlocks;
  fs_src        :array[0..260] of Char;
 end;

 TSaveDataBackendConnect=class
  kipc    :THostIpcPipeKERN;
  hProcess:THandle;
  fork_pid:Integer;
  Constructor Create;
  Destructor  Destroy; override;
  procedure   SendMountConfig();
  function    SaveDataDelete(del:pSceSaveDataDelete):Integer;
  function    SaveDataMount (mount:pSceSaveDataMount;var pResult:TSaveDataMountResult;Transfering:Boolean):Integer;
  function    SaveDataUmount(slot_id:Integer):Integer;
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

 TMountSlot=record
  active     :Integer;
  userId     :SceUserServiceUserId;
  titleId    :SceSaveDataTitleId;
  dirName    :SceSaveDataDirName;
  fingerprint:SceSaveDataFingerprint;
  max_blocks :SceSaveDataBlocks;
 end;

 THostIpcPipeSave=class(THostIpcPipe)
  event:p_event;
  procedure Recv_pipe; override;
 end;

 TSaveDataBackendProcess=class
  ppid  :Integer;
  parent:THandle;
  kipc  :THostIpcPipeSave;
  queue :TIntrusiveMPSCQueue;
  event :t_event;
  //
  MountSlots:array[0..15] of TMountSlot;
  //
  Constructor Create;
  procedure   SendCmd(cmd:TCustomCommand);
  function    OnExitProc      (Value:TIpcValue):TIpcValue; //EXIT_PROC
  function    OnMountConfig   (Value:TIpcValue):TIpcValue; //MOUNT_CONFIG
  function    OnSaveDataDelete(Value:TIpcValue):TIpcValue; //SaveDataDelete
  function    OnSaveDataMount (Value:TIpcValue):TIpcValue; //SaveDataMount
  function    OnIsActiveMount (Value:TIpcValue):TIpcValue; //IsActiveMount
  function    OnSaveDataUmount(Value:TIpcValue):TIpcValue; //SaveDataUmount
 end;

implementation

var
 gSaveDataBackend:TSaveDataBackendProcess=nil;

type
 PForkData=^TForkData;
 TForkData=record
  pipefd     :THandle;
  sdk_version:DWORD;
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

procedure THostIpcPipeSave.Recv_pipe;
begin
 inherited;
 ev_signal(event^);
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

procedure OnExitProc;
begin
 gSaveDataBackend.kipc.InvokeBroken();
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

 pipefd              :=PForkData(data)^.pipefd;
 p_proc.p_sdk_version:=PForkData(data)^.sdk_version;

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

 //////////////

 AddExitProc(@OnExitProc);

 BeginThread(@wait_parent,nil);

 repeat
  ev_wait(gSaveDataBackend.event);

  gSaveDataBackend.kipc.Update();

  node:=nil;
  while gSaveDataBackend.queue.Pop(node) do
  begin
   cmd:=node^.self_;
   cmd.Run;
   cmd.Free;
  end;

 until false;

end;

////

Constructor TSaveDataBackendProcess.Create;
begin
 queue.Create;
 ev_init(event,'event');
 //
 kipc:=THostIpcPipeSave.Create;
 kipc.FHandler:=THostIpcHandler.Create;
 kipc.event:=@event;
 //
 kipc.FHandler.AddCallback('EXIT_PROC'     ,@OnExitProc);
 kipc.FHandler.AddCallback('MOUNT_CONFIG'  ,@OnMountConfig);
 kipc.FHandler.AddCallback('SaveDataDelete',@OnSaveDataDelete);
 kipc.FHandler.AddCallback('SaveDataMount' ,@OnSaveDataMount);
 kipc.FHandler.AddCallback('IsActiveMount' ,@OnIsActiveMount);
 kipc.FHandler.AddCallback('SaveDataUmount',@OnSaveDataUmount);
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

procedure TSaveDataBackendConnect.SendMountConfig();
var
 data:TGameMountConfigExport;
begin
 data:=GameMountConfigExport;

 kipc.InvokeSync('MOUNT_CONFIG',TIpcValue.&Object(data));

 FreeAndNil(data);
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
 Writeln(' LocalDir  =',data.LocalDir );
 Writeln(' TitleId   =',data.TitleId   );
 Writeln(' InstallDir=',data.InstallDir);

 FreeAndNil(data);
end;

///

function GetMountSlotId(userId:Integer;dirName,titleId:pchar;var slot_id:Integer):Integer;
var
 i,first_id:Integer;
begin

 first_id:=-1;

 For i:=0 to High(gSaveDataBackend.MountSlots) do
 if (gSaveDataBackend.MountSlots[i].active<>0) then
 begin

  if (gSaveDataBackend.MountSlots[i].userId=userId) then
  if (strncasecmp(@gSaveDataBackend.MountSlots[i].titleId.data,
                  titleId,
                  SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)=0) then
  if (strncasecmp(@gSaveDataBackend.MountSlots[i].dirName.data,
                  dirName,
                  SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE)=0) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_BUSY);
  end;

 end else
 if (first_id=-1) then
 begin
  first_id:=i;
 end;

 if (first_id=-1) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_MOUNT_FULL);
 end;

 slot_id:=first_id;
 Result:=0;
end;

function IsActiveMount(userId:Integer;dirName,titleId:pchar):Boolean;
var
 i:Integer;
begin
 Result:=False;

 For i:=0 to High(gSaveDataBackend.MountSlots) do
 if (gSaveDataBackend.MountSlots[i].active<>0) then
 begin

  if (gSaveDataBackend.MountSlots[i].userId=userId) then
  if (strncasecmp(@gSaveDataBackend.MountSlots[i].titleId.data,
                  titleId,
                  SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)=0) then
  if (strncasecmp(@gSaveDataBackend.MountSlots[i].dirName.data,
                  dirName,
                  SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE)=0) then
  begin
   Exit(True);
  end;

 end;

end;

type
 TSaveDataDelete=packed record
  userId :SceUserServiceUserId;
  titleId:SceSaveDataTitleId;
  dirName:SceSaveDataDirName;
 end;

function TSaveDataBackendConnect.SaveDataDelete(del:pSceSaveDataDelete):Integer;
var
 data:TSaveDataDelete;
begin
 FillChar(data,SizeOf(data),0);
  data.userId :=del^.userId;
 if (del^.titleId<>nil) then
  data.titleId:=del^.titleId^;
 if (del^.dirName<>nil) then
  data.dirName:=del^.dirName^;

 Result:=kipc.InvokeSync2('SaveDataDelete',@data,sizeof(data));
end;

function TSaveDataBackendProcess.OnSaveDataDelete(Value:TIpcValue):TIpcValue; //SaveDataDelete
var
 data:TSaveDataDelete;

 titleId:pchar;
 dirName:pchar;
 fs_src :RawByteString;
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

 mtx_lock(GameMountConfig.mount_mtx);

  if IsActiveMount(data.userId,dirName,titleId) then
  begin
   Result:=SCE_SAVE_DATA_ERROR_BUSY;
  end else
  begin

   if (strncasecmp(@GameMountConfig.InstallDir,
                   titleId,
                   SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)<>0) then
   begin
    //trying to delete another game?
    //check FINGERPRINT?
   end;

   fs_src:=GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

   //dont check errors
   game_mount.DeleteDirectory(fs_src,False);
  end;

 mtx_unlock(GameMountConfig.mount_mtx);
end;

type
 TSaveDataMount=packed record
  userId     :SceUserServiceUserId;
  titleId    :SceSaveDataTitleId;
  dirName    :SceSaveDataDirName;
  fingerprint:SceSaveDataFingerprint;
  blocks     :SceSaveDataBlocks;
  mountMode  :DWORD; //SceSaveDataMountMode
  Transfering:Boolean;
 end;

function TSaveDataBackendConnect.SaveDataMount(mount:pSceSaveDataMount;var pResult:TSaveDataMountResult;Transfering:Boolean):Integer;
var
 data:TSaveDataMount;
 Value:TIpcValue;
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

 Value:=kipc.InvokeSync('SaveDataMount',TIpcValue.Static(@data,sizeof(data)));
 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@pResult,SizeOf(pResult));

 Value.Free;

 Result:=pResult.result;

 if (Result=0) then
 begin

  Result:=vfs_mountroot.mount_into_sandbox('ufs',
                                           pchar(mount_savedata_slot_name[pResult.slot_id]),
                                           pchar(pResult.fs_src),
                                           nil,
                                           ord((data.mountMode and SDM_RDONLY)<>0)*MNT_RDONLY or
                                           MNT_EMU_PFS);
  if (Result<>0) then
  begin
   Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
   //unmount
   kipc.InvokeSync2('SaveDataUmount',TIpcValue.Static(@pResult.slot_id,sizeof(pResult.slot_id)));
  end;

 end;

end;

function TSaveDataBackendProcess.OnSaveDataMount(Value:TIpcValue):TIpcValue; //SaveDataMount
var
 data:TSaveDataMount;
 output:TSaveDataMountResult;

 mountMode:DWORD;
 slot_id  :Integer;
 titleId  :pchar;
 dirName  :pchar;
 fs_src   :RawByteString;
begin
 Result:=0;
 FillChar(output,SizeOf(output),0);
 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 mountMode:=data.mountMode;
 if (p_proc.p_sdk_version < $4500000) then
 begin
  mountMode:=mountMode and (not SDM_CREATE2);
 end;

 titleId:=@data.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 if ((mountMode and SDM_RDWR)<>0) then
 if (strncasecmp(@GameMountConfig.InstallDir,
                 titleId,
                 SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)<>0) then
 begin
  //trying to mount another game with RW?
  //check FINGERPRINT?
 end;

 slot_id:=0;

 mtx_lock(GameMountConfig.mount_mtx);

  output.result:=GetMountSlotId(data.userId,
                                dirName,
                                titleId,
                                slot_id);
  if (output.result=0) then
  begin

   fs_src:=GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

   output.mountStatus:=0;

   if DirectoryExists(fs_src) then
   begin

    if ((mountMode and SDM_CREATE2)<>0) then
    begin
     //force
     FormatMount(fs_src);
    end else
    if ((mountMode and SDM_CREATE)<>0) then
    begin
     //error
     output.result:=SCE_SAVE_DATA_ERROR_EXISTS;
    end;

   end else
   begin

    if ((mountMode and (SDM_CREATE2 or SDM_CREATE))<>0) then
    begin
     //create
     if ForceDirectories(fs_src) then
     begin
      output.mountStatus:=SCE_SAVE_DATA_MOUNT_STATUS_CREATED;
     end else
     begin
      output.result:=SCE_SAVE_DATA_ERROR_INTERNAL;
     end;
    end else
    begin
     //error
     output.result:=SCE_SAVE_DATA_ERROR_NOT_FOUND;
    end;

   end;

   if (output.result=0) then
   begin

    //save info
    gSaveDataBackend.MountSlots[slot_id].active:=1;
    gSaveDataBackend.MountSlots[slot_id].userId:=data.userId;

    strncpy_s(@gSaveDataBackend.MountSlots[slot_id].titleId.data,titleId,SCE_SAVE_DATA_TITLE_ID_DATA_SIZE  );
    strncpy_s(@gSaveDataBackend.MountSlots[slot_id].dirName.data,dirName,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE);

    gSaveDataBackend.MountSlots[slot_id].fingerprint:=data.fingerprint;
    gSaveDataBackend.MountSlots[slot_id].max_blocks :=data.blocks;

    //out
    output.slot_id       :=slot_id;
    output.requiredBlocks:=0; //TODO
    output.fs_src        :=fs_src;
   end;

  end;

 mtx_unlock(GameMountConfig.mount_mtx);

 if (output.result=0) then
 begin
  Result:=TIpcValue.New(@output,sizeof(output));
 end else
 begin
  Result:=output.result;
 end;
end;

function TSaveDataBackendConnect.SaveDataUmount(slot_id:Integer):Integer;
begin
 Result:=kipc.InvokeSync2('IsActiveMount',TIpcValue.Static(@slot_id,sizeof(slot_id)));

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

   //free
   Result:=kipc.InvokeSync2('SaveDataUmount',TIpcValue.Static(@slot_id,sizeof(slot_id)));
  end;

 end;

end;

function TSaveDataBackendProcess.OnIsActiveMount(Value:TIpcValue):TIpcValue; //IsActiveMount
var
 slot_id:Integer;
begin
 Result:=0;
 slot_id:=0;
 Value.MoveTo(@slot_id,SizeOf(slot_id));

 mtx_lock(GameMountConfig.mount_mtx);

  if (gSaveDataBackend.MountSlots[slot_id].active=0) then
  begin
   Result:=SCE_SAVE_DATA_ERROR_NOT_MOUNTED;
  end else
  begin
   Result:=0;
  end;

 mtx_unlock(GameMountConfig.mount_mtx);
end;

function TSaveDataBackendProcess.OnSaveDataUmount(Value:TIpcValue):TIpcValue; //SaveDataUmount
var
 slot_id:Integer;
begin
 Result:=0;
 slot_id:=0;
 Value.MoveTo(@slot_id,SizeOf(slot_id));;

 mtx_lock(GameMountConfig.mount_mtx);

  if (gSaveDataBackend.MountSlots[slot_id].active=0) then
  begin
   Result:=SCE_SAVE_DATA_ERROR_NOT_MOUNTED;
  end else
  begin
   Result:=0;

   //free
   gSaveDataBackend.MountSlots[slot_id]:=Default(TMountSlot);
  end;


 mtx_unlock(GameMountConfig.mount_mtx);
end;

end.



