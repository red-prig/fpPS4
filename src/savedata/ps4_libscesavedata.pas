unit ps4_libSceSaveData;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 errno,
 SceSaveData,
 kern_thr,
 kern_proc,
 kern_ksched,
 kern_authinfo,
 kern_mtx,
 mpmc_queue,
 subr_dynlib,
 game_mount,
 vfs_mountroot,
 ps4_libSceUserService;

implementation

{
uses
 sys_path,
 sys_signal;
}

{
type
 t_backup_event_queue=specialize mpmc_bounded_queue<SceSaveDataEvent>;

var
 backup:record
  queue:t_backup_event_queue;
  cb:SceSaveDataEventCallbackFunc;
  userdata:Pointer;
 end;


Procedure push_event(event:pSceSaveDataEvent);
var
 tmp:SceSaveDataEvent;
begin

 if (backup.cb<>nil) then
 begin
  backup.cb(event,backup.userdata);
 end;

 while not backup.queue.enqueue(event^) do
 begin
  backup.queue.dequeue(tmp); //drop first
 end;
end;
}

///

type
 t_init_version=(VERSION_INIT_0,VERSION_INIT_2,VERSION_INIT_3,VERSION_INIT_CDLG);

 TMountSlot=record
  active     :Integer;
  userId     :SceUserServiceUserId;
  titleId    :SceSaveDataTitleId;
  dirName    :SceSaveDataDirName;
  fingerprint:SceSaveDataFingerprint;
  max_blocks :SceSaveDataBlocks;
 end;

 TSaveDataInstance=class
  version             :t_init_version;
  memory_timeout_10sec:Boolean;
  force_default_prio  :Boolean;
  not_prio_by_cusaname:Boolean;
  priority            :Integer;
  threadStackSize     :DWORD;
  cpuAffinityMask     :QWORD;
  job_thread          :Pointer;
  mtx                 :mtx;
  //
  cb_event   :SceSaveDataEventCallbackFunc;
  cb_userdata:Pointer;
  //
  MountSlots:array[0..15] of TMountSlot;
 end;

var
 g_instance:TSaveDataInstance;

function IsLoggedIn(userId:Integer):Integer; inline;
begin
 //sceUserServiceIsLoggedIn
 Result:=0;
end;

function CheckReserved(var buf;len:DWORD):Boolean;
var
 i:DWORD;
begin
 for i:=0 to len-1 do
 if (PByte(@buf)[i]<>0) then
 begin
  Exit(False);
 end;
 Result:=True;
end;

function CheckTitleId(titleId:pSceSaveDataTitleId):Integer;
var
 i:DWORD;
begin
 if (titleId=nil) then
 begin
  Exit(0);
 end;

 if CheckReserved(titleId^.data,sizeof(titleId^.data)) then
 begin
  Exit(0);
 end;

 for i:=0 to 3 do
  if (titleId^.data[i] < 'A') or (titleId^.data[i] > 'Z') then
  begin
   Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
  end;
 for i:=4 to 8 do
  if (titleId^.data[i] < '0') or (titleId^.data[i] > '9') then
  begin
   Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
  end;

 if not CheckReserved(titleId^.padding,sizeof(titleId^.padding)) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 Result:=0;
end;

function strnlen_s(s:PChar;maxlen:ptrint):ptrint;
var
 i:size_t;
begin
 if (s=nil) then Exit(0);
 i:=0;
 if (maxlen<>0) then
 begin
  repeat
   if (s[i]=#0) then Exit(i);
   Inc(i);
  until (maxlen = i);
 end;
 Exit(maxlen);
end;

function strncasecmp(str1,str2:PChar;maxlen:ptrint):Integer;
begin
 if (maxlen<>0) then
 begin
  repeat
   if (LowerCase(str1^)<>LowerCase(str2^)) then
   begin
    Exit(ord(LowerCase(str1^))-ord(LowerCase(str2^)));
   end;

   if (str1^=#0) then break;

   Inc(str1);
   Inc(str2);

   Dec(maxlen);
  until (maxlen=0);

 end;
 Result:=0;
end;

function strncpy_s(dst,src:PChar;maxlen:ptrint):PChar; inline;
begin
 if (dst=nil) or (src=nil) then Exit(nil);
 Result:=StrLCopy(dst,src,maxlen);
end;

function is_sdmemory(name:pchar):Boolean;
begin
 Result:=False;
 if (PQWORD(@name[0])^=QWORD($656D64735F656373)) then //sce_sdme
 if (PDWORD(@name[8])^=DWORD($79726F6D)) then         //mory
 begin
  case Byte(name[12]) of
   $00:Result:=True;
   $31,
   $32,
   $33:if (name[13]=#0) then Result:=True;
   else;
  end;
 end;
end;

function CheckDirName(dirName:pSceSaveDataDirName;allow_sdm:Boolean):Integer;
var
 len,i:DWORD;
begin
 if (dirName=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 if allow_sdm then
 if is_sdmemory(@dirName^.data) then
 begin
  Exit(0);
 end;

 len:=strnlen_s(@dirName^.data,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE);

 if (len=0) or (len=SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 if (len<>0) then
 for i:=0 to len-1 do
 begin
  case dirName^.data[i] of
   'a'..'z':;
   'A'..'Z':;
   '0'..'9':;
   '-',
   '.',
   '@':;
   else
    Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
  end;
 end;

 Result:=0;
end;

function GetMountSlotId(userId:Integer;dirName,titleId:pchar;var slot_id:Integer):Integer;
var
 i,first_id:Integer;
begin

 first_id:=-1;

 For i:=0 to High(g_instance.MountSlots) do
 if (g_instance.MountSlots[i].active<>0) then
 begin

  if (g_instance.MountSlots[i].userId=userId) then
  if (strncasecmp(@g_instance.MountSlots[i].titleId.data,
                  titleId,
                  SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)=0) then
  if (strncasecmp(@g_instance.MountSlots[i].dirName.data,
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

 For i:=0 to High(g_instance.MountSlots) do
 if (g_instance.MountSlots[i].active<>0) then
 begin

  if (g_instance.MountSlots[i].userId=userId) then
  if (strncasecmp(@g_instance.MountSlots[i].titleId.data,
                  titleId,
                  SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)=0) then
  if (strncasecmp(@g_instance.MountSlots[i].dirName.data,
                  dirName,
                  SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE)=0) then
  begin
   Exit(True);
  end;

 end;

end;

function CheckDataInitParams0(params:pSceSaveDataInitParams):Integer; inline;
begin
 if (params=nil) then Exit(0);
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (DWORD(params^.priority-256)<512) then
 if (Byte(params^.reserved[0])<2) then
 if CheckReserved(params^.reserved[1],sizeof(params^.reserved)-1) then
 begin
  Result:=0;
 end;
end;

function CheckDataInitParams1(params:pSceSaveDataInitParams):Integer; inline;
begin
 if (params=nil) then Exit(0);
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (DWORD(params^.priority-256)<512) then
 if CheckReserved(params^.reserved,sizeof(params^.reserved)) then
 begin
  Result:=0;
 end;
end;

function CheckDataInitParams2(params:pSceSaveDataInitParams2):Integer; inline;
begin
 if (params=nil) then Exit(0);
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (DWORD(params^.priority-256)<512) then
 if (DWORD(params^.threadStackSize-1)>$3ffe) then
 if (QWORD(params^.cpuAffinityMask)<64) then
 if CheckReserved(params^.reserved,sizeof(params^.reserved)) then
 begin
  Result:=0;
 end;
end;

function CheckDataInitParams3(params:Pointer):Integer; inline;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (params=nil) then
 begin
  Result:=0;
 end;
end;

function InitInstance(instance:TSaveDataInstance;params:Pointer;version:t_init_version):Integer;
begin
 mtx_init(instance.mtx,'SaveDataInstance');
 instance.version        :=version;
 instance.priority       :=700;
 instance.threadStackSize:=$4000;
 instance.cpuAffinityMask:=0;

 case version of
  VERSION_INIT_0:
   begin
    if (params=nil) then Exit(0);

    if (p_proc.p_sdk_version >= $2000000) then
    begin
     Result:=CheckDataInitParams1(params);
     if (Result=0) then
     begin
      instance.priority            :=pSceSaveDataInitParams(params)^.priority;
      instance.not_prio_by_cusaname:=true;
     end;
    end else
    begin
     Result:=CheckDataInitParams0(params);
     if (Result=0) then
     begin
      instance.priority            :=pSceSaveDataInitParams(params)^.priority;
      instance.force_default_prio  :=(pSceSaveDataInitParams(params)^.reserved[0]<>0);
      instance.not_prio_by_cusaname:=true;
     end;
    end;

   end;
  VERSION_INIT_2:
   begin
    Result:=CheckDataInitParams2(params);
    if (Result=0) then
    begin
     instance.priority            :=pSceSaveDataInitParams2(params)^.priority;
     instance.not_prio_by_cusaname:=true;
     instance.threadStackSize     :=pSceSaveDataInitParams2(params)^.threadStackSize;
     instance.cpuAffinityMask     :=pSceSaveDataInitParams2(params)^.cpuAffinityMask;
    end;
   end;
  VERSION_INIT_3:
   begin
    Result:=CheckDataInitParams3(params);
   end;
  VERSION_INIT_CDLG:
   Assert(False,'VERSION_INIT_CDLG');
 end;

end;

procedure Getprio_by_cusaname(instance:TSaveDataInstance);
var
 sched_param:t_sched_param;
begin
 if (p_proc.p_sdk_version < $2000000) and
    (instance.force_default_prio=false) then
 begin
  instance.priority:=700;
 end;

 if (instance.not_prio_by_cusaname=false) then
 begin

  case String(g_appinfo.CUSANAME) of
   'CUSA00503',
   'CUSA01425',
   'CUSA00220':
     begin
      //scePthreadGetprio(scePthreadSelf(),&instance->prio)

      sched_param:=Default(t_sched_param);

      PROC_LOCK();
      ksched_getparam(@ksched, curkthread, @sched_param);
      PROC_UNLOCK();

      if (sched_param.sched_priority<>0) then
      begin
       instance.priority:=sched_param.sched_priority;
      end;

     end;
   else;
  end;

 end;
end;

function ConnectInstance(instance:TSaveDataInstance):Integer;
begin
 Result:=0;

 if (instance.version=VERSION_INIT_3) then
 begin

  if (p_proc.p_sdk_version < $6500000) then
  begin

   if (
       g_appinfo.titleWorkaround.ids[0] and
       (QWORD(1) shl BUG180029_SAVE_DATA_MEMORY_TIMEOUT_10SEC)
      )<>0 then
   begin
    instance.memory_timeout_10sec:=True;
   end;

  end else
  begin
   instance.memory_timeout_10sec:=True;
  end;

 end else
 begin
  Getprio_by_cusaname(instance);

  //init_job_thread
 end;

end;

function CreateSaveDataInstance(params:Pointer;version:t_init_version):Integer;
var
 instance:TSaveDataInstance;
begin
 if (g_instance<>nil) then Exit(0);

 instance:=TSaveDataInstance.Create;
 Result:=InitInstance(instance,params,version);
 g_instance:=instance;

 if (Result<0) then
 begin
  g_instance.Free;
  g_instance:=nil;
  Exit;
 end;

 Result:=ConnectInstance(g_instance);
end;

function ps4_sceSaveDataInitialize(params:pSceSaveDataInitParams):Integer;
begin
 Result:=CreateSaveDataInstance(params,VERSION_INIT_0);
end;

function ps4_sceSaveDataInitialize2(params:pSceSaveDataInitParams2):Integer;
begin
 Result:=CreateSaveDataInstance(params,VERSION_INIT_2);
end;

function ps4_sceSaveDataInitialize3(params:pSceSaveDataInitParams3):Integer;
begin
 Result:=CreateSaveDataInstance(params,VERSION_INIT_3);
end;

function ps4_sceSaveDataTerminate:Integer;
begin
 Result:=0;
end;

function ps4_sceSaveDataSetupSaveDataMemory(
           userId:SceUserServiceUserId;
           memorySize:QWORD;
           param:PSceSaveDataParam):Integer;
begin
 Result:=0;
end;

function ps4_sceSaveDataSetupSaveDataMemory2(
           setupParam:PSceSaveDataMemorySetup2;
           _result:PSceSaveDataMemorySetupResult):Integer;
begin
 Result:=0;
end;

function ps4_sceSaveDataGetSaveDataMemory(
           userId:SceUserServiceUserId;
           buf:Pointer;
           bufSize:QWORD;
           offset:QWORD):Integer;
begin
 if (buf<>nil) then
 begin
  FillChar(buf^,bufSize,0);
 end;
 Result:=0;
end;

function ps4_sceSaveDataGetSaveDataMemory2(
           getParam:PSceSaveDataMemoryGet2):Integer;
begin
 if (getParam<>nil) then
 begin
  if (getParam^.data<>nil) then
  begin
   if (getParam^.data^.buf<>nil) then
   begin
    FillChar(getParam^.data^.buf^,getParam^.data^.bufSize,0);
   end;
  end;
 end;
 Result:=0;
end;

function ps4_sceSaveDataSetSaveDataMemory(
           userId:SceUserServiceUserId;
           buf:Pointer;
           bufSize:QWORD;
           offset:QWORD):Integer;
begin
 Result:=0;
end;

function ps4_sceSaveDataSetSaveDataMemory2(
           setParam:PSceSaveDataMemorySet2):Integer;
begin
 Result:=0;
end;

function ps4_sceSaveDataSyncSaveDataMemory(
           syncParam:PSceSaveDataMemorySync):Integer;
begin
 Result:=0;
end;

function CheckSaveDataDelete1(del:pSceSaveDataDelete):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (del=nil) then Exit;

 if IsLoggedIn(del^.userId)<>0 then
 begin
  Exit(SCE_SAVE_DATA_ERROR_INVALID_LOGIN_USER);
 end;

 if CheckTitleId(del^.titleId)=0 then
 if CheckDirName(del^.dirName,False)=0 then
 if CheckReserved(del^.reserved,sizeof(del^.reserved)) then
 begin
  Result:=0;
 end;
end;

function CheckSaveDataDelete2(del:pSceSaveDataDelete):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (del=nil) then Exit;

 if IsLoggedIn(del^.userId)<>0 then
 begin
  Exit(SCE_SAVE_DATA_ERROR_INVALID_LOGIN_USER);
 end;

 if CheckTitleId(del^.titleId)=0 then
 if CheckDirName(del^.dirName,False)=0 then
 if (del^.progress=0) then
 if CheckReserved(del^.reserved,sizeof(del^.reserved)) then
 begin
  Result:=0;
 end;
end;

function SaveDataDelete(del:pSceSaveDataDelete):Integer;
var
 titleId:pchar;
 dirName:pchar;
 fs_src :RawByteString;
begin
 if (p_proc.p_sdk_version < $3500000) then
 begin
  Result:=CheckSaveDataDelete1(del);
 end else
 begin
  Result:=CheckSaveDataDelete2(del);
 end;
 if (Result<>0) then Exit;

 titleId:=@del^.titleId^.data;
 if (titleId=nil) then
 begin
  titleId:=@GameMountConfig.SaveTitleId;
 end else
 if (titleId[0]=#0) then
 begin
  titleId:=@GameMountConfig.SaveTitleId;
 end;

 dirName:=@del^.dirName^.data;

 mtx_lock(g_instance.mtx);
 mtx_lock(GameMountConfig.mount_mtx);

  if IsActiveMount(del^.userId,dirName,titleId) then
  begin
   Result:=SCE_SAVE_DATA_ERROR_BUSY;
  end else
  begin

   if (strncasecmp(@GameMountConfig.SaveTitleId,
                   titleId,
                   SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)<>0) then
   begin
    //trying to delete another game?
    //check FINGERPRINT?
   end;

   fs_src:=GameMountConfig.GetSaveDataFolder(del^.userId,titleId,dirName);

   //dont check errors
   game_mount.DeleteDirectory(fs_src,False);
  end;

 mtx_unlock(GameMountConfig.mount_mtx);
 mtx_unlock(g_instance.mtx);
end;

function ps4_sceSaveDataDelete(del:pSceSaveDataDelete):Integer;
begin

 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (del=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 Result:=SaveDataDelete(del);
end;

function CheckFingerprint(ptr:pSceSaveDataFingerprint):Integer;
var
 len,i:DWORD;
begin
 if (ptr=nil) then
 begin
  Exit(0);
 end;

 len:=strnlen_s(@ptr^.data,SCE_SAVE_DATA_FINGERPRINT_DATA_SIZE);

 if (len=SCE_SAVE_DATA_FINGERPRINT_DATA_SIZE) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 for i:=0 to SCE_SAVE_DATA_FINGERPRINT_DATA_SIZE-1 do
 begin
  case ptr^.data[i] of
   'a'..'z':;
   '0'..'9':;
   else
    Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
  end;
 end;

 if not CheckReserved(ptr^.padding,sizeof(ptr^.padding)) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 Result:=0;
end;

function CheckMountMode(mountMode:DWORD;blocks:SceSaveDataBlocks):Boolean; inline;
const
 RDONLY_RDWR  =SDM_RDONLY or SDM_RDWR;
 RDONLY_CREATE=SDM_RDONLY or SDM_CREATE;
begin
 Result:=(
          ((mountMode and SDM_CREATE)=0) or
          (blocks>95)
         ) and (
          (mountMode and RDONLY_RDWR)<>RDONLY_RDWR
         ) and (
          mountMode<>0
         ) and
         (
          (mountMode and RDONLY_CREATE)<>RDONLY_CREATE
         );
end;

function CheckSceSaveDataMount1(mount:pSceSaveDataMount;allow_sdm:Boolean):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (mount=nil) then Exit;

 if IsLoggedIn(mount^.userId)<>0 then
 begin
  Exit(SCE_SAVE_DATA_ERROR_INVALID_LOGIN_USER);
 end;

 if CheckTitleId(mount^.titleId)=0 then
 if CheckDirName(mount^.dirName,allow_sdm)=0 then
 if CheckFingerprint(mount^.fingerprint)=0 then
 if CheckMountMode(mount^.mountMode,mount^.blocks) then
 if CheckReserved(mount^.reserved,sizeof(mount^.reserved)) then
 begin
  Result:=0;
 end;
end;

function CheckSceSaveDataMount2(mount:pSceSaveDataMount):Integer;
begin
 Result:=CheckSceSaveDataMount1(mount,False);
 if (Result=0) then
 begin

  if (mount^.titleId=nil) then
  begin
   if (mount^.fingerprint=nil) then
   begin
    Exit(0);
   end;
  end else
  if (mount^.fingerprint<>nil) then
  begin
   Exit(0);
  end;

  Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 end;
end;

function CheckSceSaveDataMount3(mount:pSceSaveDataMount):Integer;
const
 CREATE_COPY_ICON=SDM_CREATE or SDM_COPY_ICON;
begin
 Result:=CheckSceSaveDataMount1(mount,False);
 if (Result=0) then
 begin

  if (mount^.titleId=nil) then
  begin
   if (mount^.fingerprint<>nil) then
   begin
    Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
   end;
  end else
  if (mount^.fingerprint=nil) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
  end;

  if ((mount^.mountMode and CREATE_COPY_ICON)=SDM_COPY_ICON) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
  end;

  Result:=0;
 end;
end;

function CheckSceSaveDataTransferringMount(mount:pSceSaveDataMount):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;

 if (mount<>nil) then
 if (mount^.titleId<>nil) and
    (mount^.fingerprint<>nil) then
 begin
  Result:=0;
 end;
end;

function CheckSceSaveDataMount4(mount:pSceSaveDataMount):Integer;
const
 CREATE_COPY_ICON=SDM_CREATE or SDM_COPY_ICON;
begin
 Result:=CheckSceSaveDataMount1(mount,True);
 if (Result=0) then
 begin
  if is_sdmemory(@mount^.dirName^.data) and ((mount^.mountMode and SDM_RDONLY)=0) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
  end;

  if (mount^.titleId=nil) then
  begin
   if (mount^.fingerprint<>nil) then
   begin
    Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
   end;
  end else
  if (mount^.fingerprint=nil) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
  end;

  if ((mount^.mountMode and CREATE_COPY_ICON)=SDM_COPY_ICON) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
  end;

  Result:=0;
 end;
end;

function CheckMountMode2(mountMode:DWORD;blocks:SceSaveDataBlocks):Boolean; inline;
const
 CREATE_CREATE2          =SDM_CREATE or SDM_CREATE2;
 RDONLY_CREATE           =SDM_RDONLY or SDM_CREATE;
 RDONLY_CREATE2          =SDM_RDONLY or SDM_CREATE2;
 RDONLY_RDWR             =SDM_RDONLY or SDM_RDWR;
 CREATE_COPY_ICON_CREATE2=SDM_CREATE or SDM_COPY_ICON or SDM_CREATE2;
begin
 Result:=(
          ((mountMode and CREATE_CREATE2)=0) or
          (blocks>95)
         ) and (
          mountMode<>0
         ) and
         (
          (mountMode and RDONLY_CREATE)<>RDONLY_CREATE
         ) and
         (
          (mountMode and RDONLY_CREATE2)<>RDONLY_CREATE2
         ) and
         (
          (mountMode and CREATE_CREATE2)<>CREATE_CREATE2
         ) and (
          (mountMode and RDONLY_RDWR)<>RDONLY_RDWR
         ) and (
          (mountMode and CREATE_COPY_ICON_CREATE2)<>SDM_COPY_ICON
         );
end;

function CheckSceSaveDataMount5(mount:pSceSaveDataMount):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (mount=nil) then Exit;

 if IsLoggedIn(mount^.userId)<>0 then
 begin
  Exit(SCE_SAVE_DATA_ERROR_INVALID_LOGIN_USER);
 end;

 if CheckTitleId(mount^.titleId)=0 then
 if CheckDirName(mount^.dirName,True)=0 then
 if CheckFingerprint(mount^.fingerprint)=0 then
 if CheckMountMode2(mount^.mountMode,mount^.blocks) then
 if CheckReserved(mount^.reserved,sizeof(mount^.reserved)) then
 begin

  if is_sdmemory(@mount^.dirName^.data) and ((mount^.mountMode and SDM_RDONLY)=0) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
  end;

  if (mount^.titleId=nil) then
  begin
   if (mount^.fingerprint=nil) then
   begin
    Exit(0);
   end;
  end else
  if (mount^.fingerprint<>nil) then
  begin
   Exit(0);
  end;

  Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 end;
end;

function CheckOutputSceSaveDataMountPoint1(pResult:pSceSaveDataMountResult):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (pResult=nil) then Exit;

 if CheckReserved(pResult^.reserved,sizeof(pResult^.reserved)) then
 begin
  Result:=0;
 end;
end;

function CheckOutputSceSaveDataMountPoint2(pResult:pSceSaveDataMountResult):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (pResult=nil) then Exit;

 if (pResult^.progress=0) then
 if CheckReserved(pResult^.reserved,sizeof(pResult^.reserved)) then
 begin
  Result:=0;
 end;
end;

function CheckSaveDataMount(mount      :pSceSaveDataMount;
                            pResult    :pSceSaveDataMountResult;
                            Transfering:Boolean):Integer;
begin

 if (p_proc.p_sdk_version < $1500000) then
 begin
  Result:=CheckSceSaveDataMount1(mount,False);
 end else
 if (p_proc.p_sdk_version < $1700000) then
 begin
  Result:=CheckSceSaveDataMount2(mount);
 end else
 if (p_proc.p_sdk_version < $2500000) then
 begin
  Result:=CheckSceSaveDataMount3(mount);
 end else
 begin

  if (Transfering) then
  begin
   Result:=CheckSceSaveDataTransferringMount(mount);
   if (Result<>0) then Exit;
  end;

  if (p_proc.p_sdk_version < $4500000) then
  begin
   Result:=CheckSceSaveDataMount4(mount);
  end else
  begin
   Result:=CheckSceSaveDataMount5(mount);
  end;

 end;

 if (Result<>0) then Exit;

 if (p_proc.p_sdk_version < $3500000) then
 begin
  Result:=CheckOutputSceSaveDataMountPoint1(pResult);
 end else
 if (p_proc.p_sdk_version < $4500000) then
 begin
  Result:=CheckOutputSceSaveDataMountPoint2(pResult);
 end else
 begin
  Result:=CheckOutputSceSaveDataMountPoint2(pResult);
 end;

end;

const
 mount_savedata_slot_name:array[0..15] of SceSaveDataMountPoint=(
  '/savedata0',
  '/savedata1',
  '/savedata2',
  '/savedata3',
  '/savedata4',
  '/savedata5',
  '/savedata6',
  '/savedata7',
  '/savedata8',
  '/savedata9',
  '/savedata10',
  '/savedata11',
  '/savedata12',
  '/savedata13',
  '/savedata14',
  '/savedata15'
 );

function SaveDataMount(mount      :pSceSaveDataMount;
                       pResult    :pSceSaveDataMountResult;
                       Transfering:Boolean):Integer;
var
 mountMode  :DWORD;
 mountStatus:DWORD;
 slot_id    :Integer;
 titleId    :pchar;
 dirName    :pchar;
 fs_src     :RawByteString;
begin
 Result:=CheckSaveDataMount(mount,pResult,Transfering);
 if (Result<>0) then Exit;

 mountMode:=mount^.mountMode;
 if (p_proc.p_sdk_version < $4500000) then
 begin
  mountMode:=mountMode and (not SDM_CREATE2);
 end;

 titleId:=@mount^.titleId^.data;
 if (titleId=nil) then
 begin
  titleId:=@GameMountConfig.SaveTitleId;
 end else
 if (titleId[0]=#0) then
 begin
  titleId:=@GameMountConfig.SaveTitleId;
 end;

 dirName:=@mount^.dirName^.data;

 if ((mountMode and SDM_RDWR)<>0) then
 if (strncasecmp(@GameMountConfig.SaveTitleId,
                 titleId,
                 SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)<>0) then
 begin
  //trying to mount another game with RW?
  //check FINGERPRINT?
 end;

 slot_id:=0;

 mtx_lock(g_instance.mtx);
 mtx_lock(GameMountConfig.mount_mtx);

  Result:=GetMountSlotId(mount^.userId,
                         dirName,
                         titleId,
                         slot_id);
  if (Result=0) then
  begin

   fs_src:=GameMountConfig.GetSaveDataFolder(mount^.userId,titleId,dirName);

   mountStatus:=0;

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
     Result:=SCE_SAVE_DATA_ERROR_EXISTS;
    end;

   end else
   begin

    if ((mountMode and (SDM_CREATE2 or SDM_CREATE))<>0) then
    begin
     //create
     if ForceDirectories(fs_src) then
     begin
      mountStatus:=SCE_SAVE_DATA_MOUNT_STATUS_CREATED;
     end else
     begin
      Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
     end;
    end else
    begin
     //error
     Result:=SCE_SAVE_DATA_ERROR_NOT_FOUND;
    end;

   end;

   if (Result=0) then
   begin

    Result:=vfs_mountroot.mount_into_sandbox('ufs',
                                             mount_savedata_slot_name[slot_id],
                                             pchar(fs_src),
                                             nil,
                                             ord((mountMode and SDM_RDONLY)<>0)*MNT_RDONLY or
                                             MNT_EMU_PFS);
    if (Result<>0) then
    begin
     Result:=SCE_SAVE_DATA_ERROR_INTERNAL;
    end;

   end;

   if (Result=0) then
   begin

    //save info
    g_instance.MountSlots[slot_id].active:=1;
    g_instance.MountSlots[slot_id].userId:=mount^.userId;

    strncpy_s(@g_instance.MountSlots[slot_id].titleId.data,titleId,SCE_SAVE_DATA_TITLE_ID_DATA_SIZE  );
    strncpy_s(@g_instance.MountSlots[slot_id].dirName.data,dirName,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE);

    if (mount^.fingerprint=nil) then
    begin
     g_instance.MountSlots[slot_id].fingerprint:=Default(SceSaveDataFingerprint);
    end else
    begin
     g_instance.MountSlots[slot_id].fingerprint:=mount^.fingerprint^;
    end;

    g_instance.MountSlots[slot_id].max_blocks:=mount^.blocks;

    //out
    pResult^.mountPoint:=mount_savedata_slot_name[slot_id];

    if (p_proc.p_sdk_version < $3500000) then
    begin
     pResult^.progress:=100;
    end else
    begin
     pResult^.mountStatus:=mountStatus;
    end;

   end;

  end;

 mtx_unlock(GameMountConfig.mount_mtx);
 mtx_unlock(g_instance.mtx);
end;

function ps4_sceSaveDataMount(mount:pSceSaveDataMount;
                              mountResult:pSceSaveDataMountResult):Integer;
begin

 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (mount=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 Result:=SaveDataMount(mount,mountResult,False);
end;

function ps4_sceSaveDataMount2(mount:PSceSaveDataMount2;
                               mountResult:PSceSaveDataMountResult):Integer;
var
 tmp:SceSaveDataMount;
begin

 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (mount=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 if not CheckReserved(mount^.reserved,sizeof(mount^.reserved)) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 tmp:=Default(SceSaveDataMount);
 tmp.userId   :=mount^.userId   ;
 tmp.dirName  :=mount^.dirName  ;
 tmp.blocks   :=mount^.blocks   ;
 tmp.mountMode:=mount^.mountMode;

 Result:=SaveDataMount(@tmp,mountResult,False);
end;

function ps4_sceSaveDataTransferringMount(mount:pSceSaveDataTransferringMount;
                                          mountResult:PSceSaveDataMountResult):Integer;
var
 tmp:SceSaveDataMount;
begin

 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (mount=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 if not CheckReserved(mount^.reserved,sizeof(mount^.reserved)) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 tmp:=Default(SceSaveDataMount);
 tmp.userId     :=mount^.userId     ;
 tmp.titleId    :=mount^.titleId    ;
 tmp.dirName    :=mount^.dirName    ;
 tmp.fingerprint:=mount^.fingerprint;
 tmp.mountMode  :=SDM_RDONLY        ;

 Result:=SaveDataMount(@tmp,mountResult,True);
end;

function GetMountSlotId(name:pchar;var slot_id:Integer):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (name<>nil) then
 if (name[0]='/') then
 if (PQWORD(@name[1])^=QWORD($6174616465766173)) then // savedata
 begin
  if (name[10]=#0) then
  begin
   case name[9] of
    '0'..'9':
     begin
      slot_id:=ord(name[9])-ord('0');
      Result:=0;
     end;
    else;
   end;
  end else
  if (name[11]=#0) then
  begin
   Case PWORD(@name[9])^ of
    $3031, //10
    $3131, //11
    $3231, //12
    $3331, //13
    $3431, //14
    $3531: //15
      begin
       slot_id:=ord(name[10])-ord('0')+10;
       Result:=0;
      end;
   end;
  end;
 end;
end;

function SaveDataUmount(mountPoint:pSceSaveDataMountPoint):Integer;
var
 slot_id:Integer;
begin
 slot_id:=0;
 Result:=GetMountSlotId(pchar(mountPoint),slot_id);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);
 mtx_lock(GameMountConfig.mount_mtx);

  if (g_instance.MountSlots[slot_id].active=0) then
  begin
   Result:=SCE_SAVE_DATA_ERROR_NOT_MOUNTED;
  end else
  begin

   Result:=vfs_mountroot.unmount_from_sandbox(pchar(mountPoint),0);
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
    g_instance.MountSlots[slot_id]:=Default(TMountSlot);

   end;

  end;


 mtx_unlock(GameMountConfig.mount_mtx);
 mtx_unlock(g_instance.mtx);
end;

function ps4_sceSaveDataUmount(mountPoint:pSceSaveDataMountPoint):Integer;
begin
 Result:=0;

 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=SaveDataUmount(mountPoint);
end;

function ps4_sceSaveDataUmountWithBackup(mountPoint:pSceSaveDataMountPoint):Integer;
var
 event:SceSaveDataEvent;
begin
 Result:=0;

 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=SaveDataUmount(mountPoint);

 {
 //backup this
 //...

 //in another thread?
 event:=Default(SceSaveDataEvent);
 event._type:=SCE_SAVE_DATA_EVENT_TYPE_UMOUNT_BACKUP_END;
 event.userId:=1;
 //event.titleId:SceSaveDataTitleId;
 //event.dirName:SceSaveDataDirName;
 push_event(@event);

 _sig_unlock;
 }
end;

function ps4_sceSaveDataGetMountInfo(mountPoint:pSceSaveDataMountPoint;
                                     info:pSceSaveDataMountInfo):Integer;
begin
 Result:=0;
 if (info<>nil) then
 begin
  info^:=Default(SceSaveDataMountInfo);
  info^.blocks    :=100000;
  info^.freeBlocks:=100000;
 end;
end;

function _convert_dir_name_search(P:PChar):RawByteString;
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
   '_':Result[i]:='#';
   else;
  end;
 end;
end;

{
function StringListAscCompare(List:TStringList;Index1,Index2:Integer):Integer;
begin
 Result:=CompareStr(List[Index1],List[Index2]);
end;

function StringListDscCompare(List:TStringList;Index1,Index2:Integer):Integer;
begin
 Result:=CompareStr(List[Index2],List[Index1]);
end;
}

function ps4_sceSaveDataDirNameSearch(cond:pSceSaveDataDirNameSearchCond;
                                      sres:pSceSaveDataDirNameSearchResult):Integer;
{
var
 ROut:TRawByteSearchRec;
 S,F:RawByteString;
 List:TStringList;
 i,n:Integer;
 }
begin
 Result:=0;

 if (cond=nil) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 if (sres=nil) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);

 Case cond^.order of
  SDSO_ASCENT :;
  SDSO_DESCENT:;
  else
   Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 sres^.setNum:=0;

 //Assert(cond^.key  =SCE_SAVE_DATA_SORT_KEY_DIRNAME);

 {
 s:=IncludeTrailingPathDelimiter(ps4_app.save_path)+_convert_dir_name_search(Pchar(cond^.dirName));

 _sig_lock;

 ROut:=Default(TRawByteSearchRec);
 if (FindFirst(s,faDirectory,ROut)=0) then
 begin
  List:=TStringList.Create;
  repeat
   if (ROut.FindData.dwFileAttributes and faDirectory)=faDirectory then
   begin
    F:=UTF8Encode(WideString(ROut.FindData.cFileName));
    Case F of
     '.','..':;
     else
      List.Add(F);
    end;
   end;
  until (FindNext(ROut)<>0);
  FindClose(ROut);

  sres^.hitNum:=List.Count;
  if (List.Count<>0) and (sres^.dirNamesNum<>0) then
  begin
   Case cond^.order of
    SCE_SAVE_DATA_SORT_ORDER_ASCENT :List.CustomSort(@StringListAscCompare);
    SCE_SAVE_DATA_SORT_ORDER_DESCENT:List.CustomSort(@StringListDscCompare);
    else;
   end;

   n:=List.Count;
   if (n>sres^.dirNamesNum) then n:=sres^.dirNamesNum;

   sres^.setNum:=n;

   For i:=0 to n-1 do
   begin
    s:=List[i];

    if (sres^.dirNames<>nil) then
    begin
     sres^.dirNames[i]:=Default(SceSaveDataDirName);
     MoveChar0(PChar(s)^,sres^.dirNames[i],SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE);
    end;

    if (sres^.params<>nil) then
    begin
     sres^.params[i]:=Default(SceSaveDataParam);
    end;

    if (sres^.infos<>nil) then
    begin
     sres^.infos[i]:=Default(SceSaveDataSearchInfo);
     sres^.infos[i].blocks    :=100000;
     sres^.infos[i].freeBlocks:=100000;
    end;

   end;

  end else
  begin
   sres^.setNum:=0;
  end;

  FreeAndNil(List);

 end;

 _sig_unlock;
 }

end;

function ps4_sceSaveDataGetParam(mountPoint:pSceSaveDataMountPoint;
                                 paramType:SceSaveDataParamType;
                                 paramBuf:Pointer;
                                 paramBufSize:QWORD;
                                 gotSize:PQWORD
                                ):Integer;
begin
 if (gotSize<>nil) then
 begin
  gotSize^:=0;
 end;
 Result:=0;
end;

//Save icon
function ps4_sceSaveDataSetParam(mountPoint:pSceSaveDataMountPoint;
                                 paramType:SceSaveDataParamType;
                                 paramBuf:Pointer;
                                 paramBufSize:QWORD):Integer;
begin
 Result:=0;
end;

function ps4_sceSaveDataSaveIcon(mountPoint:pSceSaveDataMountPoint;
                                 param:pSceSaveDataIcon):Integer;
begin
 if (mountPoint=nil) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 if (param=nil)      then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 Result:=0;
end;

//Load icon
function ps4_sceSaveDataLoadIcon(mountPoint:pSceSaveDataMountPoint;
                                 param:pSceSaveDataIcon):Integer;
begin
 if (mountPoint=nil) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 if (param=nil)      then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 Result:=SCE_SAVE_DATA_ERROR_FILE_NOT_FOUND;
end;

function ps4_sceSaveDataRegisterEventCallback(cb:SceSaveDataEventCallbackFunc;userdata:Pointer):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (cb=nil) or (g_instance.version=VERSION_INIT_3) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 mtx_lock(g_instance.mtx);

  g_instance.cb_event   :=cb;
  g_instance.cb_userdata:=userdata;

 mtx_unlock(g_instance.mtx);

 Result:=0;
end;

function ps4_sceSaveDataUnregisterEventCallback(cb:SceSaveDataEventCallbackFunc):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (cb=nil) or (g_instance.version=VERSION_INIT_3) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 Result:=SCE_SAVE_DATA_ERROR_NOT_REGIST_CALLBACK;

 mtx_lock(g_instance.mtx);

   if (g_instance.cb_event=cb) then
   begin
    g_instance.cb_event   :=nil;
    g_instance.cb_userdata:=nil;
    Result:=0;
   end;

 mtx_unlock(g_instance.mtx);
end;

function ps4_sceSaveDataGetEventResult(param:pSceSaveDataEventParam;
                                       event:pSceSaveDataEvent):Integer;
begin
 if (event=nil) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);

 event^:=Default(SceSaveDataEvent);

 {
 if backup.queue.dequeue(event^) then
 begin
  Result:=0;
 end else
 begin
  Result:=SCE_SAVE_DATA_ERROR_NOT_FOUND;
 end;
 }
end;

function ps4_sceSaveDataClearProgress():Integer;
begin
 //Сlearing the progress value for:
 //sceSaveDataMount2()
 //sceSaveDataDelete()
 //sceSaveDataRestoreBackupData()
 //sceSaveDataGetProgress()
 Result:=0;
end;

function ps4_sceSaveDataBackup(backup:pSceSaveDataBackup):Integer;
begin
 Result:=0;
end;

function ps4_sceSaveDataCheckBackupData(check:pSceSaveDataCheckBackupData):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_NOT_FOUND;
end;

procedure init_save;
begin
 //backup.queue.Create(32);
end;

function Load_libSceSaveData(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceSaveData');

 lib:=Result^.add_lib('libSceSaveData');
 lib.set_proc($664661B2408F5C5C,@ps4_sceSaveDataInitialize);
 lib.set_proc($9753660DE0E93465,@ps4_sceSaveDataInitialize2);
 lib.set_proc($4F2C2B14A0A82C66,@ps4_sceSaveDataInitialize3);
 lib.set_proc($C8A0F2F12E722C0D,@ps4_sceSaveDataTerminate);
 lib.set_proc($BFB00000CA342F3E,@ps4_sceSaveDataSetupSaveDataMemory);
 lib.set_proc($A10C921147E05D10,@ps4_sceSaveDataSetupSaveDataMemory2);
 lib.set_proc($EC1B79A410BF01CA,@ps4_sceSaveDataGetSaveDataMemory);
 lib.set_proc($43038EEEF7A09D5F,@ps4_sceSaveDataGetSaveDataMemory2);
 lib.set_proc($8776144735C64954,@ps4_sceSaveDataSetSaveDataMemory);
 lib.set_proc($71DBB2F6FE18993E,@ps4_sceSaveDataSetSaveDataMemory2);
 lib.set_proc($C224FD8DE0BBC4FC,@ps4_sceSaveDataSyncSaveDataMemory);
 lib.set_proc($4B51A478F235EF34,@ps4_sceSaveDataDelete);
 lib.set_proc($DF61D0010770336A,@ps4_sceSaveDataMount);
 lib.set_proc($D33E393C81FE48D2,@ps4_sceSaveDataMount2);
 lib.set_proc($580CD64D99B51FE2,@ps4_sceSaveDataTransferringMount);
 lib.set_proc($04C47817F51E9371,@ps4_sceSaveDataUmount);
 lib.set_proc($57069DC0104127CD,@ps4_sceSaveDataUmountWithBackup);
 lib.set_proc($EB9547D1069ACFAB,@ps4_sceSaveDataGetMountInfo);
 lib.set_proc($7722219D7ABFD123,@ps4_sceSaveDataDirNameSearch);
 lib.set_proc($5E0BD2B88767325C,@ps4_sceSaveDataGetParam);
 lib.set_proc($F39CEE97FFDE197B,@ps4_sceSaveDataSetParam);
 lib.set_proc($73CF18CB9E0CC74C,@ps4_sceSaveDataSaveIcon);
 lib.set_proc($7068CEDF0337576F,@ps4_sceSaveDataLoadIcon);
 lib.set_proc($86C29DE5CDB5B107,@ps4_sceSaveDataRegisterEventCallback);
 lib.set_proc($BFF00AD40C50852D,@ps4_sceSaveDataUnregisterEventCallback);
 lib.set_proc($8FCC4AB62163D126,@ps4_sceSaveDataGetEventResult);
 lib.set_proc($5B3FF82597DE3BD8,@ps4_sceSaveDataClearProgress);
 lib.set_proc($CF5240F3F889B779,@ps4_sceSaveDataBackup);
 lib.set_proc($4503AA0DB9376D25,@ps4_sceSaveDataCheckBackupData);

 //init_save;
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceSaveData.prx',@Load_libSceSaveData);

end.

