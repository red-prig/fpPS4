unit SaveDataBackend;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 g_node_splay,
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
 param_sfo,
 game_mount,
 vfs_mountroot,
 ps4_libSceUserService,
 ps4_libSceSystemService,
 SceSaveData;

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

 TSaveDataMountResult=packed record
  result        :Integer;
  mountStatus   :DWORD;
  slot_id       :DWORD;
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
  function    SaveDataDelete   (del:pSceSaveDataDelete):Integer;
  function    SaveDataMount    (mount:pSceSaveDataMount;var pResult:TSaveDataMountResult;Transfering:Boolean):Integer;
  function    SaveDataUmount   (slot_id:Integer;backup:boolean):Integer;
  function    GetMountInfo     (slot_id:Integer;info:pSceSaveDataMountInfo):Integer;
  function    SaveDataBackup   (backup:pSceSaveDataBackup):Integer;
  function    CheckBackupData  (check:pSceSaveDataCheckBackupData):Integer;
  function    RestoreBackupData(restore:pSceSaveDataRestoreBackupData):Integer;
  function    GetEventResult   (event:pSceSaveDataEvent):Integer;
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

 TMountSlot=record
  active     :Integer;
  userId     :SceUserServiceUserId;
  titleId    :SceSaveDataTitleId;
  dirName    :SceSaveDataDirName;
  fingerprint:SceSaveDataFingerprint;
  max_blocks :SceSaveDataBlocks;
  mountMode  :DWORD; //SceSaveDataMountMode
 end;

 THostIpcPipeSave=class(THostIpcPipe)
  event:t_event;
  Constructor Create;
  procedure   Recv_pipe; override;
 end;

 PLockDirNode=^TLockDirNode;
 TLockDirNode=object
  //
  pLeft :PLockDirNode;
  pRight:PLockDirNode;
  //
  fs_src:RawByteString;
  //
  function c(n1,n2:PLockDirNode):Integer; static;
 end;

 TLockDirSplay=specialize TNodeSplay<TLockDirNode>;

 TEventQueue=object
  mtx:mtx;
  rd_pos:Byte;
  wr_pos:Byte;
  data:array[0..19] of SceSaveDataEvent;
  procedure Init;
  procedure Push(const event:SceSaveDataEvent);
  procedure Push(_type,errorCode,userId:Integer;titleId:pSceSaveDataTitleId;dirName:pSceSaveDataDirName);
  function  Pop (var event:SceSaveDataEvent):Boolean;
 end;

 TSaveDataBackendProcess=class
  systemLang:DWORD;
  ppid      :Integer;
  parent    :THandle;
  kipc      :THostIpcPipeSave;
  job_queue :TIntrusiveMPSCQueue;
  job_event :t_event;
  //
  MountSlots:array[0..15] of TMountSlot;
  //
  LockDirMap:TLockDirSplay;
  LockDirMtx:mtx;
  //
  EventQueue:TEventQueue;
  //
  Constructor Create;
  procedure   SendCmd(cmd:TCustomCommand);
  function    RecvCmd(var cmd:TCustomCommand):Boolean;
  function    LockDir  (const fs_src:RawByteString):Boolean;
  function    UnLockDir(const fs_src:RawByteString):Boolean;
  function    OnExitProc      (Value:TIpcValue):TIpcValue; //EXIT_PROC
  function    OnMountConfig   (Value:TIpcValue):TIpcValue; //MOUNT_CONFIG
  function    GetMountSlotId  (userId:Integer;titleId,dirName:pchar;var slot_id:Integer):Integer;
  function    IsActiveMount   (userId:Integer;titleId,dirName:pchar):Boolean;
  function    OnSaveDataDelete(Value:TIpcValue):TIpcValue; //SaveDataDelete
  function    OnSaveDataMount (Value:TIpcValue):TIpcValue; //SaveDataMount
  function    OnIsActiveMount (Value:TIpcValue):TIpcValue; //IsActiveMount
  function    OnSaveDataUmount(Value:TIpcValue):TIpcValue; //SaveDataUmount
  function    OnGetMountInfo  (Value:TIpcValue):TIpcValue; //GetMountInfo
  function    OnSaveDataBackup(Value:TIpcValue):TIpcValue; //SaveDataBackup
  function    SendBackupJob   (userId     :SceUserServiceUserId;
                               titleId    :pchar;
                               dirName    :pchar;
                               fingerprint:pSceSaveDataFingerprint;
                               umount     :Boolean):Integer;
  function    OnCheckBackupData  (Value:TIpcValue):TIpcValue; //CheckBackupData
  function    OnRestoreBackupData(Value:TIpcValue):TIpcValue; //RestoreBackupData
  function    OnGetEventResult   (Value:TIpcValue):TIpcValue; //GetEventResult
 end;

implementation

//

function TLockDirNode.c(n1,n2:PLockDirNode):Integer;
begin
 Result:=CompareText(n1^.fs_src,n2^.fs_src);
end;

//

procedure TEventQueue.Init;
begin
 mtx_init(mtx,'TEventQueue');
 rd_pos:=0;
 wr_pos:=0;
end;

procedure TEventQueue.Push(const event:SceSaveDataEvent);
begin
 mtx_lock(mtx);

 data[wr_pos]:=event;

 wr_pos:=(wr_pos+1) mod Length(data);

 if (wr_pos=rd_pos) then
 begin
  rd_pos:=(rd_pos+1) mod Length(data);
 end;

 mtx_unlock(mtx);
end;

procedure TEventQueue.Push(_type,errorCode,userId:Integer;titleId:pSceSaveDataTitleId;dirName:pSceSaveDataDirName);
var
 event:SceSaveDataEvent;
begin
 event:=Default(SceSaveDataEvent);
 event._type    :=_type;
 event.errorCode:=errorCode;
 event.userId   :=userId;
 event.titleId  :=titleId^;
 event.dirName  :=dirName^;
 //
 Push(event);
end;

function TEventQueue.Pop(var event:SceSaveDataEvent):Boolean;
begin
 mtx_lock(mtx);

 if (wr_pos=rd_pos) then
 begin
  Result:=False;
 end else
 begin
  event:=data[rd_pos];

  rd_pos:=(rd_pos+1) mod Length(data);

  Result:=True;
 end;

 mtx_unlock(mtx);
end;

//

type
 t_sfo_param_params_s=packed record
  version          :DWORD;                // =0
  user_id          :DWORD;                //
  psid_hmac        :array[0..31] of Byte; //
  counter_id       :DWORD;                //  =1  2  3
  title_id_1       :array[0..15] of Char; //   |  |  |
  title_id_2       :array[0..15] of Char; //   |  |  |
  RETAIL_counter1  :DWORD;                // <-/  |  |
  DEX_TOOL_counter2:DWORD;                // <----/  |
  DEX_TOOL_counter3:DWORD;                // <-------/
  fake_owner       :DWORD;                // =0/1
  flags            :DWORD;                // =4
  archive_time1    :QWORD;
  archive_time2    :QWORD;
  corrupt_flag     :DWORD;                // =0/1
  padding          :array[0..907] of Byte;
 end;
 {$IF sizeof(t_sfo_param_params_s)<>$400}{$STOP sizeof(t_sfo_param_params_s)<>$400}{$ENDIF}

{
the flags parameter is cumulative

app0_dir_id                        flags
----------------------------------+------
unknow(error?)                    | 0x01
disc                       (0) -> | 0x08
PkgSpCore                  (1)    |
                PS_CLOUD:true  -> | 0x02
                PS_CLOUD:false -> | 0x04
debug                      (2) -> | 0x10
debug hostapp/app data/app (3) -> | 0x20
}

type
 p_savedata_sfo_values=^t_savedata_sfo_values;
 t_savedata_sfo_values=packed object
  CATEGORY           :array[0..3] of Char;
  FORMAT             :array[0..3] of Char;
  TITLE_ID           :array[0..11] of Char;
  ATTRIBUTE          :DWORD;
  SAVEDATA_BLOCKS    :QWORD;
  PARAMS             :t_sfo_param_params_s;
  MAINTITLE          :array[0..127] of Char;
  SUBTITLE           :array[0..127] of Char;
  DETAIL             :array[0..1023] of Char;
  SAVEDATA_LIST_PARAM:DWORD;
  SAVEDATA_DIRECTORY :array[0..31] of Char;
  ACCOUNT_ID         :QWORD;
  //
  Procedure New(const data:TSaveDataMount;systemLang:DWORD);
  function  SaveToFile(const fname:RawByteString):Boolean;
  function  LoadFromFile(const fname:RawByteString):Boolean;
 end;

Procedure t_savedata_sfo_values.New(const data:TSaveDataMount;systemLang:DWORD);
var
 titleId:PChar;
begin
 titleId:=@data.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@GameMountConfig.TitleId;
 end;

 Self:=Default(t_savedata_sfo_values);
 //
 CATEGORY         :='sd';
 FORMAT           :='obs';
 ACCOUNT_ID       :=$6F6C6C6F706122E7;
 SAVEDATA_BLOCKS  :=data.blocks;
 params.user_id   :=data.userId;
 params.counter_id:=1;
 params.flags     :=4;

 if (systemLang>High(MAINTITLE_DEFAULT)) then systemLang:=0;
 StrCopy(@MAINTITLE,MAINTITLE_DEFAULT[systemLang]);

 strlcopy(@TITLE_ID         ,titleId,SCE_SAVE_DATA_TITLE_ID_DATA_SIZE);
 strlcopy(@params.title_id_1,titleId,SCE_SAVE_DATA_TITLE_ID_DATA_SIZE);
 strlcopy(@params.title_id_2,titleId,SCE_SAVE_DATA_TITLE_ID_DATA_SIZE);

 strlcopy(@SAVEDATA_DIRECTORY,@data.dirName.data,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE);
end;

function t_savedata_sfo_values.SaveToFile(const fname:RawByteString):Boolean;
var
 F:TParamSfoFileLoader;
begin
 F.New(192,136,2380);

 F.AddNameValue('ACCOUNT_ID'         ,@ACCOUNT_ID         ,SFO_FORMAT_BLOB  ,sizeof(ACCOUNT_ID)                  ,sizeof(ACCOUNT_ID));
 F.AddNameValue('ATTRIBUTE'          ,@ATTRIBUTE          ,SFO_FORMAT_UINT32,sizeof(ATTRIBUTE)                   ,sizeof(ATTRIBUTE));
 F.AddNameValue('CATEGORY'           ,@CATEGORY           ,SFO_FORMAT_STRING,strlen(pchar(@CATEGORY ))+1         ,sizeof(CATEGORY));
 F.AddNameValue('DETAIL'             ,@DETAIL             ,SFO_FORMAT_STRING,strlen(pchar(@DETAIL   ))+1         ,sizeof(DETAIL   ));
 F.AddNameValue('FORMAT'             ,@FORMAT             ,SFO_FORMAT_STRING,strlen(pchar(@FORMAT   ))+1         ,sizeof(FORMAT   ));
 F.AddNameValue('MAINTITLE'          ,@MAINTITLE          ,SFO_FORMAT_STRING,strlen(pchar(@MAINTITLE))+1         ,sizeof(MAINTITLE));
 F.AddNameValue('PARAMS'             ,@PARAMS             ,SFO_FORMAT_BLOB  ,sizeof(PARAMS)                      ,sizeof(PARAMS));
 F.AddNameValue('SAVEDATA_BLOCKS'    ,@SAVEDATA_BLOCKS    ,SFO_FORMAT_BLOB  ,sizeof(SAVEDATA_BLOCKS)             ,sizeof(SAVEDATA_BLOCKS));
 F.AddNameValue('SAVEDATA_DIRECTORY' ,@SAVEDATA_DIRECTORY ,SFO_FORMAT_STRING,strlen(pchar(@SAVEDATA_DIRECTORY))+1,sizeof(SAVEDATA_DIRECTORY));
 F.AddNameValue('SAVEDATA_LIST_PARAM',@SAVEDATA_LIST_PARAM,SFO_FORMAT_UINT32,sizeof(SAVEDATA_LIST_PARAM)         ,sizeof(SAVEDATA_LIST_PARAM));
 F.AddNameValue('SUBTITLE'           ,@SUBTITLE           ,SFO_FORMAT_STRING,strlen(pchar(@SUBTITLE))+1          ,sizeof(SUBTITLE));
 F.AddNameValue('TITLE_ID'           ,@TITLE_ID           ,SFO_FORMAT_STRING,strlen(pchar(@TITLE_ID))+1          ,sizeof(TITLE_ID));

 Result:=F.save(fname);
 F.Free;
end;

procedure _on_load_sfo(userdata:Pointer;name,value:pchar;format:WORD;size,max_size,i:DWORD);

 procedure copy_value(dst:Pointer;field_format:WORD;max_field_size:DWORD); inline;
 begin
  if (field_format=format) then
  begin
   if (size>max_field_size) then size:=max_field_size;
   Move(value^,dst^,size);
  end;
 end;

begin
 with p_savedata_sfo_values(userdata)^ do
 begin
  case RawByteString(name) of
   'ACCOUNT_ID'         :copy_value(@ACCOUNT_ID         ,SFO_FORMAT_BLOB  ,sizeof(ACCOUNT_ID));
   'ATTRIBUTE'          :copy_value(@ATTRIBUTE          ,SFO_FORMAT_UINT32,sizeof(ATTRIBUTE));
   'CATEGORY'           :copy_value(@CATEGORY           ,SFO_FORMAT_STRING,sizeof(CATEGORY));
   'DETAIL'             :copy_value(@DETAIL             ,SFO_FORMAT_STRING,sizeof(DETAIL   ));
   'FORMAT'             :copy_value(@FORMAT             ,SFO_FORMAT_STRING,sizeof(FORMAT   ));
   'MAINTITLE'          :copy_value(@MAINTITLE          ,SFO_FORMAT_STRING,sizeof(MAINTITLE));
   'PARAMS'             :copy_value(@PARAMS             ,SFO_FORMAT_BLOB  ,sizeof(PARAMS));
   'SAVEDATA_BLOCKS'    :copy_value(@SAVEDATA_BLOCKS    ,SFO_FORMAT_BLOB  ,sizeof(SAVEDATA_BLOCKS));
   'SAVEDATA_DIRECTORY' :copy_value(@SAVEDATA_DIRECTORY ,SFO_FORMAT_STRING,sizeof(SAVEDATA_DIRECTORY));
   'SAVEDATA_LIST_PARAM':copy_value(@SAVEDATA_LIST_PARAM,SFO_FORMAT_UINT32,sizeof(SAVEDATA_LIST_PARAM));
   'SUBTITLE'           :copy_value(@SUBTITLE           ,SFO_FORMAT_STRING,sizeof(SUBTITLE));
   'TITLE_ID'           :copy_value(@TITLE_ID           ,SFO_FORMAT_STRING,sizeof(TITLE_ID));
   else;
  end;
 end;
end;

function t_savedata_sfo_values.LoadFromFile(const fname:RawByteString):Boolean;
var
 F:TParamSfoFileLoader;
begin
 Result:=False;

 if not F.open(fname) then
 begin
  Exit;
 end;

 if not F.parse() then
 begin
  F.Free;
  Exit;
 end;

 Self:=Default(t_savedata_sfo_values);
 F.ForAll(@_on_load_sfo,@Self);

 F.Free;

 Result:=True;
end;

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
 mtx_init(LockDirMtx,'LockDirMtx');
 //
 EventQueue.Init;
 //
 kipc:=THostIpcPipeSave.Create;
 kipc.FHandler:=THostIpcHandler.Create;
 //
 kipc.FHandler.AddCallback('EXIT_PROC'        ,@OnExitProc);
 kipc.FHandler.AddCallback('MOUNT_CONFIG'     ,@OnMountConfig);
 kipc.FHandler.AddCallback('SaveDataDelete'   ,@OnSaveDataDelete);
 kipc.FHandler.AddCallback('SaveDataMount'    ,@OnSaveDataMount);
 kipc.FHandler.AddCallback('IsActiveMount'    ,@OnIsActiveMount);
 kipc.FHandler.AddCallback('SaveDataUmount'   ,@OnSaveDataUmount);
 kipc.FHandler.AddCallback('GetMountInfo'     ,@OnGetMountInfo);
 kipc.FHandler.AddCallback('SaveDataBackup'   ,@OnSaveDataBackup);
 kipc.FHandler.AddCallback('CheckBackupData'  ,@OnCheckBackupData);
 kipc.FHandler.AddCallback('RestoreBackupData',@OnRestoreBackupData);
 kipc.FHandler.AddCallback('GetEventResult'   ,@OnGetEventResult);
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

function TSaveDataBackendProcess.LockDir(const fs_src:RawByteString):Boolean;
var
 node:PLockDirNode;
begin
 node:=AllocMem(sizeof(TLockDirNode));
 node^.fs_src:=fs_src;

 mtx_lock(LockDirMtx);

  Result:=LockDirMap.Insert(node);

 mtx_unlock(LockDirMtx);

 if Result then
 begin
  //
 end else
 begin
  Finalize(node^);
  FreeMem(node);
 end;
end;

function TSaveDataBackendProcess.UnLockDir(const fs_src:RawByteString):Boolean;
var
 data:TLockDirNode;
 node:PLockDirNode;
begin
 data.fs_src:=fs_src;

 mtx_lock(LockDirMtx);

  node:=LockDirMap.Find(@data);
  Result:=LockDirMap.Delete(node);

 mtx_unlock(LockDirMtx);

 if (node<>nil) then
 begin
  Finalize(node^);
  FreeMem(node);
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
 Writeln(' LocalDir  =',data.LocalDir );
 Writeln(' TitleId   =',data.TitleId   );
 Writeln(' InstallDir=',data.InstallDir);

 FreeAndNil(data);
end;

///

function TSaveDataBackendProcess.GetMountSlotId(userId:Integer;titleId,dirName:pchar;var slot_id:Integer):Integer;
var
 i,first_id:Integer;
begin

 first_id:=-1;

 For i:=0 to High(MountSlots) do
 if (MountSlots[i].active<>0) then
 begin

  if (MountSlots[i].userId=userId) then
  if (strncasecmp(@MountSlots[i].titleId.data,
                  titleId,
                  SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)=0) then
  if (strncasecmp(@MountSlots[i].dirName.data,
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

function TSaveDataBackendProcess.IsActiveMount(userId:Integer;titleId,dirName:pchar):Boolean;
var
 i:Integer;
begin
 Result:=False;

 For i:=0 to High(MountSlots) do
 if (MountSlots[i].active<>0) then
 begin

  if (MountSlots[i].userId=userId) then
  if (strncasecmp(@MountSlots[i].titleId.data,
                  titleId,
                  SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)=0) then
  if (strncasecmp(@MountSlots[i].dirName.data,
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
 gSaveDataBackend.UnLockDir(fs_src);
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

function TSaveDataBackendProcess.OnSaveDataDelete(Value:TIpcValue):TIpcValue; //SaveDataDelete
var
 data:TSaveDataDelete;

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

 if IsActiveMount(data.userId,titleId,dirName) then
 begin
  Result:=SCE_SAVE_DATA_ERROR_BUSY;
 end else
 begin

  fs_src:=GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

  if LockDir(fs_src) then
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

function TSaveDataBackendConnect.SaveDataMount(mount:pSceSaveDataMount;var pResult:TSaveDataMountResult;Transfering:Boolean):Integer;
var
 data:TSaveDataMount;
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

 Value:=kipc.InvokeSync('SaveDataMount',TIpcValue.Static(@data,sizeof(data)));

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
   kipc.InvokeSync2('SaveDataUmount',@pResult.slot_id,sizeof(pResult.slot_id));
  end;

 end;

end;

type
 TMountJob=class(TCustomCommand)
  //
  fs_src:RawByteString;
  data  :TSaveDataMount;
  //
  param_sfo:t_savedata_sfo_values;
  //
  procedure Init(const _data:TSaveDataMount);
  function  Lock():Boolean;
  procedure UnLock();
  function  CreateParamSfo():Boolean;
  function  OpenParamSfo(is_mount:Boolean):Integer;
  function  MountParamSfo():Integer;
  function  UmountParamSfo():Integer;
  function  CreateTmpFiles():Boolean;
  function  CheckMountData(is_created:Boolean):Integer;
  function  CreateMount(force:Boolean):Integer;
  function  OpenMount():Integer;
  //
  function  Run:TIpcValue; override;
 end;

procedure TMountJob.Init(const _data:TSaveDataMount);
begin
 data:=_data;
end;

function TMountJob.Lock():Boolean;
begin
 Result:=gSaveDataBackend.LockDir(fs_src);
end;

procedure TMountJob.UnLock();
begin
 gSaveDataBackend.UnLockDir(fs_src);
end;

function TMountJob.CreateParamSfo():Boolean;
var
 fname:RawByteString;
begin
 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys');
 Result:=ForceDirectories(fname);
 if not Result then Exit;

 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/param.sfo');

 param_sfo.New(data,gSaveDataBackend.systemLang);

 if ((data.mountMode and SDM_DESTRUCT_OFF)=0) then
 begin
  param_sfo.PARAMS.corrupt_flag:=1;
 end;

 Result:=param_sfo.SaveToFile(fname);
end;

function TMountJob.OpenParamSfo(is_mount:Boolean):Integer;
var
 fname:RawByteString;
begin
 Result:=0;

 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/param.sfo');

 if not param_sfo.LoadFromFile(fname) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 if (param_sfo.CATEGORY<>'sd') or
    (param_sfo.FORMAT<>'obs') or
    (param_sfo.SAVEDATA_BLOCKS<96) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 if CompareChar0(param_sfo.SAVEDATA_DIRECTORY,data.dirName.data,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE)<>0 then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 if (param_sfo.PARAMS.version<>0) or
    (param_sfo.PARAMS.counter_id<>1) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 if is_mount and (param_sfo.PARAMS.corrupt_flag<>0) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 //sfo.PARAMS.user_id

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

function TMountJob.UmountParamSfo():Integer;
var
 fname:RawByteString;
begin
 Result:=0;

 fname:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/param.sfo');

 //update data to sfo
 if ((data.mountMode and SDM_RDWR)<>0) then
 begin
  //mark in-free
  if ((data.mountMode and SDM_DESTRUCT_OFF)=0) then
  begin
   param_sfo.PARAMS.corrupt_flag:=0;
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

 Result:=OpenParamSfo(True);
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
 output:TSaveDataMountResult;

 minfo:TMountSlot;

 is_locked:Boolean;
begin
 output:=Default(TSaveDataMountResult);

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

 output.result:=gSaveDataBackend.GetMountSlotId(data.userId,
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

    gSaveDataBackend.MountSlots[slot_id]:=minfo;

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

function TSaveDataBackendProcess.OnSaveDataMount(Value:TIpcValue):TIpcValue; //SaveDataMount
var
 data:TSaveDataMount;

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
 TSaveDataUmount=record
  slot_id:Integer;
  backup :boolean;
 end;

function TSaveDataBackendConnect.SaveDataUmount(slot_id:Integer;backup:boolean):Integer;
var
 data:TSaveDataUmount;
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
   Result:=kipc.InvokeSync2('SaveDataUmount',@data,sizeof(data));
  end;

 end;

end;

type
 SaveDataMountInfo=packed record
  result    :QWORD;
  blocks    :SceSaveDataBlocks;
  freeBlocks:SceSaveDataBlocks;
 end;

function TSaveDataBackendProcess.OnIsActiveMount(Value:TIpcValue):TIpcValue; //IsActiveMount
var
 slot_id:Integer;
begin
 Result:=0;
 slot_id:=0;
 Value.MoveTo(@slot_id,SizeOf(slot_id));

 if (DWORD(slot_id)>15) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if (MountSlots[slot_id].active=0) then
 begin
  Result:=SCE_SAVE_DATA_ERROR_NOT_MOUNTED;
 end else
 begin
  Result:=0;
 end;

end;

type
 TUmountJob=class(TMountJob)
  data2:TSaveDataUmount;
  function Run:TIpcValue; override;
 end;

function TUmountJob.Run:TIpcValue;
var
 titleId:pchar;
 dirName:pchar;

 err:Integer;
begin
 titleId:=@data.titleId.data;
 if (titleId[0]=#0) then
 begin
  titleId:=@GameMountConfig.InstallDir;
 end;

 dirName:=@data.dirName.data;

 fs_src:=GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

 //

 err:=OpenParamSfo(False);
 if (err=0) then
 begin
  err:=UmountParamSfo();
 end;

 if (err=0) then
 begin
  //free
  gSaveDataBackend.MountSlots[data2.slot_id]:=Default(TMountSlot);

  Unlock;
 end;

 if (err=0) and data2.backup and ((data.mountMode and SDM_RDWR)<>0) then
 begin
  gSaveDataBackend.SendBackupJob(data.userId,
                                @data.titleId,
                                @data.dirName,
                                @data.fingerprint,
                                True);
 end;

 Result:=err;
end;

function TSaveDataBackendProcess.OnSaveDataUmount(Value:TIpcValue):TIpcValue; //SaveDataUmount
var
 data:TSaveDataUmount;
 prev:TMountSlot;
 job:TUmountJob;
begin
 Result:=0;
 data:=Default(TSaveDataUmount);
 Value.MoveTo(@data,SizeOf(data));;

 if (DWORD(data.slot_id)>15) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if (MountSlots[data.slot_id].active=0) then
 begin
  Result:=SCE_SAVE_DATA_ERROR_NOT_MOUNTED;
 end else
 begin
  Result:=0;

  prev:=MountSlots[data.slot_id];

  job:=TUmountJob.Create(kipc.HoldResult);

  job.data.userId     :=prev.userId;
  job.data.titleId    :=prev.titleId;
  job.data.dirName    :=prev.dirName;
  job.data.fingerprint:=prev.fingerprint;
  job.data.blocks     :=prev.max_blocks;
  job.data.mountMode  :=prev.mountMode;

  job.data2:=data;

  SendCmd(job);
 end;

end;

function TSaveDataBackendConnect.GetMountInfo(slot_id:Integer;info:pSceSaveDataMountInfo):Integer;
var
 Value:TIpcValue;
 data:SaveDataMountInfo;
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
 output:SaveDataMountInfo;
 blocks:Int64;
begin
 Result:=0;
 slot_id:=Value.GetDWORD;

 if (DWORD(slot_id)>15) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if (MountSlots[slot_id].active=0) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_MOUNTED);
 end;

 mount:=MountSlots[slot_id];

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
 TSaveDataBackup=packed record
  userId     :SceUserServiceUserId;
  titleId    :SceSaveDataTitleId;
  dirName    :SceSaveDataDirName;
  fingerprint:SceSaveDataFingerprint;
 end;

function TSaveDataBackendConnect.SaveDataBackup(backup:pSceSaveDataBackup):Integer;
var
 data:TSaveDataBackup;
begin
 FillChar(data,SizeOf(data),0);
  data.userId     :=backup^.userId;
 if (backup^.titleId<>nil) then
  data.titleId    :=backup^.titleId^;
 if (backup^.dirName<>nil) then
  data.dirName    :=backup^.dirName^;
 if (backup^.fingerprint<>nil) then
  data.fingerprint:=backup^.fingerprint^;

 Result:=kipc.InvokeSync2('SaveDataBackup',@data,sizeof(data));
end;

function TSaveDataBackendProcess.OnSaveDataBackup(Value:TIpcValue):TIpcValue; //SaveDataBackup
var
 data:TSaveDataBackup;
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

 if (param_sfo.CATEGORY<>'sd') or
    (param_sfo.FORMAT<>'obs') or
    (param_sfo.SAVEDATA_BLOCKS<96) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 if CompareChar0(param_sfo.SAVEDATA_DIRECTORY,dirName,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE)<>0 then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BROKEN);
 end;

 if (param_sfo.PARAMS.version<>0) or
    (param_sfo.PARAMS.counter_id<>1) or
    (param_sfo.PARAMS.corrupt_flag<>0) then
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

 if IsActiveMount(userId,titleId,dirName) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BUSY);
 end;

 fs_src:=GameMountConfig.GetSaveDataFolder(userId,titleId,dirName);

 if not SaveDataExists(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_FOUND);
 end;

 if not LockDir(fs_src) then
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

function TSaveDataBackendConnect.CheckBackupData(check:pSceSaveDataCheckBackupData):Integer;
var
 data:TSaveDataBackup;
begin
 FillChar(data,SizeOf(data),0);
  data.userId     :=check^.userId;
 if (check^.titleId<>nil) then
  data.titleId    :=check^.titleId^;
 if (check^.dirName<>nil) then
  data.dirName    :=check^.dirName^;

 Result:=kipc.InvokeSync2('CheckBackupData',@data,sizeof(data));

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

function TSaveDataBackendProcess.OnCheckBackupData(Value:TIpcValue):TIpcValue; //CheckBackupData
var
 data:TSaveDataBackup;
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

 if not LockDir(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BACKUP_BUSY);
 end;

 job:=TCheckJob.Create(kipc.HoldResult);
 job.Init(data.userId,titleId,dirName);

 SendCmd(job);
end;

function TSaveDataBackendConnect.RestoreBackupData(restore:pSceSaveDataRestoreBackupData):Integer;
var
 data:TSaveDataBackup;
begin
 FillChar(data,SizeOf(data),0);
  data.userId     :=restore^.userId;
 if (restore^.titleId<>nil) then
  data.titleId    :=restore^.titleId^;
 if (restore^.dirName<>nil) then
  data.dirName    :=restore^.dirName^;

 Result:=kipc.InvokeSync2('RestoreBackupData',@data,sizeof(data));
end;

function TSaveDataBackendProcess.OnRestoreBackupData(Value:TIpcValue):TIpcValue; //RestoreBackupData
var
 data:TSaveDataBackup;
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

 if IsActiveMount(data.userId,titleId,dirName) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BUSY);
 end;

 fs_src:=GameMountConfig.GetSaveDataFolder(data.userId,titleId,dirName);

 if not SaveDataExists(fs_src) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_FOUND);
 end;

 if not LockDir(fs_src) then
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

end.



