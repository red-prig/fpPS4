unit ps4_libSceSaveData;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_thr,
 kern_proc,
 kern_ksched,
 kern_authinfo,
 kern_mtx,
 mpmc_queue,
 subr_dynlib,
 ps4_libSceUserService;

Const
 SCE_SAVE_DATA_ERROR_PARAMETER                           =-2137063424; // 0x809F0000
 SCE_SAVE_DATA_ERROR_NOT_INITIALIZED                     =-2137063423; // 0x809F0001
 SCE_SAVE_DATA_ERROR_OUT_OF_MEMORY                       =-2137063422; // 0x809F0002
 SCE_SAVE_DATA_ERROR_BUSY                                =-2137063421; // 0x809F0003
 SCE_SAVE_DATA_ERROR_NOT_MOUNTED                         =-2137063420; // 0x809F0004
 SCE_SAVE_DATA_ERROR_NO_PERMISSION                       =-2137063419; // 0x809F0005
 SCE_SAVE_DATA_ERROR_FINGERPRINT_MISMATCH                =-2137063418; // 0x809F0006
 SCE_SAVE_DATA_ERROR_EXISTS                              =-2137063417; // 0x809F0007
 SCE_SAVE_DATA_ERROR_NOT_FOUND                           =-2137063416; // 0x809F0008
 SCE_SAVE_DATA_ERROR_NO_SPACE_FS                         =-2137063414; // 0x809F000A
 SCE_SAVE_DATA_ERROR_INTERNAL                            =-2137063413; // 0x809F000B
 SCE_SAVE_DATA_ERROR_MOUNT_FULL                          =-2137063412; // 0x809F000C
 SCE_SAVE_DATA_ERROR_BAD_MOUNTED                         =-2137063411; // 0x809F000D
 SCE_SAVE_DATA_ERROR_FILE_NOT_FOUND                      =-2137063410; // 0x809F000E
 SCE_SAVE_DATA_ERROR_BROKEN                              =-2137063409; // 0x809F000F
 SCE_SAVE_DATA_ERROR_INVALID_LOGIN_USER                  =-2137063407; // 0x809F0011
 SCE_SAVE_DATA_ERROR_MEMORY_NOT_READY                    =-2137063406; // 0x809F0012
 SCE_SAVE_DATA_ERROR_BACKUP_BUSY                         =-2137063405; // 0x809F0013
 SCE_SAVE_DATA_ERROR_NOT_REGIST_CALLBACK                 =-2137063403; // 0x809F0015
 SCE_SAVE_DATA_ERROR_BUSY_FOR_SAVING                     =-2137063402; // 0x809F0016
 SCE_SAVE_DATA_ERROR_LIMITATION_OVER                     =-2137063401; // 0x809F0017
 SCE_SAVE_DATA_ERROR_EVENT_BUSY                          =-2137063400; // 0x809F0018
 SCE_SAVE_DATA_ERROR_PARAMSFO_TRANSFER_TITLE_ID_NOT_FOUND=-2137063399; // 0x809F0019

 SCE_SAVE_DATA_TITLE_ID_DATA_SIZE=10;
 SCE_SAVE_DATA_FINGERPRINT_DATA_SIZE=65;

 SCE_SAVE_DATA_TITLE_MAXSIZE   =128;
 SCE_SAVE_DATA_SUBTITLE_MAXSIZE=128;
 SCE_SAVE_DATA_DETAIL_MAXSIZE  =1024;

 SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE=32;
 SCE_SAVE_DATA_MOUNT_POINT_DATA_MAXSIZE=16;
 SCE_SAVE_DATA_MOUNT_STATUS_CREATED=$00000001;

 //SceSaveDataEventType
 SCE_SAVE_DATA_EVENT_TYPE_UMOUNT_BACKUP_END        =1;
 SCE_SAVE_DATA_EVENT_TYPE_BACKUP_END               =2;
 SCE_SAVE_DATA_EVENT_TYPE_SAVE_DATA_MEMORY_SYNC_END=3;

type
 pSceSaveDataInitParams=^SceSaveDataInitParams;
 SceSaveDataInitParams=packed record
  priority:Integer;
  reserved:array[0..31] of Byte;
 end;

 pSceSaveDataInitParams2=^SceSaveDataInitParams2;
 SceSaveDataInitParams2=packed record
  priority       :Integer;
  threadStackSize:DWORD;
  cpuAffinityMask:QWORD;
  reserved       :array[0..31] of Byte;
 end;

 pSceSaveDataInitParams3=Pointer;

 PSceSaveDataParam=^SceSaveDataParam;
 SceSaveDataParam=packed record
  title    :array[0..SCE_SAVE_DATA_TITLE_MAXSIZE-1] of AnsiChar;
  subTitle :array[0..SCE_SAVE_DATA_SUBTITLE_MAXSIZE-1] of AnsiChar;
  detail   :array[0..SCE_SAVE_DATA_DETAIL_MAXSIZE-1] of AnsiChar;
  userParam:DWORD;
  align    :DWORD;
  mtime    :QWORD;
  reserved :array[0..31] of Byte;
 end;

 PSceSaveDataIcon=^SceSaveDataIcon;
 SceSaveDataIcon=packed record
  buf     :Pointer;
  bufSize :QWORD;
  dataSize:QWORD;
  reserved:array[0..31] of Byte;
 end;

 PSceSaveDataMemorySetup2=^SceSaveDataMemorySetup2;
 SceSaveDataMemorySetup2=packed record
  option        :DWORD;
  userId        :SceUserServiceUserId;
  memorySize    :QWORD;
  iconMemorySize:QWORD;
  initParam     :PSceSaveDataParam;
  initIcon      :PSceSaveDataIcon;
  reserved      :array[0..23] of Byte;
 end;

 PSceSaveDataMemorySetupResult=^SceSaveDataMemorySetupResult;
 SceSaveDataMemorySetupResult=packed record
  existedMemorySize:QWORD;
  reserved         :array[0..15] of Byte;
 end;

 PSceSaveDataMemoryData=^SceSaveDataMemoryData;
 SceSaveDataMemoryData=packed record
  buf     :Pointer;
  bufSize :QWORD;
  offset  :QWORD;
  reserved:array[0..39] of Byte;
 end;

 PSceSaveDataMemoryGet2=^SceSaveDataMemoryGet2;
 SceSaveDataMemoryGet2=packed record
  userId  :SceUserServiceUserId;
  padding :array[0..3] of Byte;
  data    :PSceSaveDataMemoryData;
  param   :PSceSaveDataParam;
  icon    :PSceSaveDataIcon;
  slotId  :DWORD;
  reserved:array[0..27] of Byte;
 end;

 PSceSaveDataMemorySet2=^SceSaveDataMemorySet2;
 SceSaveDataMemorySet2=packed record
  userId  :SceUserServiceUserId;
  padding :array[0..3] of Byte;
  data    :PSceSaveDataMemoryData;
  param   :PSceSaveDataParam;
  icon    :PSceSaveDataIcon;
  dataNum :DWORD;
  slotId  :DWORD;
  reserved:array[0..23] of Byte;
 end;

 PSceSaveDataMemorySync=^SceSaveDataMemorySync;
 SceSaveDataMemorySync=packed record
  userId  :SceUserServiceUserId;
  slotId  :DWORD;
  option  :DWORD; //SceSaveDataMemorySyncOption
  reserved:array[0..27] of Byte;
 end;

 PSceSaveDataDirName=^SceSaveDataDirName;
 SceSaveDataDirName=packed record
  data:array[0..SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE-1] of Char;
 end;

 PSceSaveDataMountPoint=^SceSaveDataMountPoint;
 SceSaveDataMountPoint=array[0..SCE_SAVE_DATA_MOUNT_POINT_DATA_MAXSIZE-1] of Char;

 pSceSaveDataTitleId=^SceSaveDataTitleId;
 SceSaveDataTitleId=packed record
  data   :array[0..SCE_SAVE_DATA_TITLE_ID_DATA_SIZE-1] of Char;
  padding:array[0..5] of Byte;
 end;

 pSceSaveDataDelete=^SceSaveDataDelete;
 SceSaveDataDelete=packed record
  userId  :SceUserServiceUserId;
  align1  :Integer;
  titleId :pSceSaveDataTitleId;
  dirName :pSceSaveDataDirName;
  unused  :Integer;
  reserved:array[0..31] of Byte;
  align2  :Integer;
 end;

 pSceSaveDataFingerprint=^SceSaveDataFingerprint;
 SceSaveDataFingerprint=packed record
  data   :array[0..SCE_SAVE_DATA_FINGERPRINT_DATA_SIZE-1] of Char;
  padding:array[0..14] of Byte;
 end;

 SceSaveDataBlocks=QWORD;

const
 //SceSaveDataMountMode
 SDM_RDONLY      =1;  //Read-only
 SDM_RDWR        =2;  //Read/write-enabled
 SDM_CREATE      =4;  //Create new (error if save data directory already exists)
 SDM_DESTRUCT_OFF=8;  //Turn off corrupt flag (not recommended)
 SDM_COPY_ICON   =16; //Copy save_data.png in package as icon when newly creating save data
 SDM_CREATE2     =32; //Create new (mount save data directory if it already exists)

type
 pSceSaveDataMount=^SceSaveDataMount;
 SceSaveDataMount=packed record
  userId     :SceUserServiceUserId;
  align1     :Integer;
  titleId    :pSceSaveDataTitleId;
  dirName    :PSceSaveDataDirName;
  fingerprint:pSceSaveDataFingerprint;
  blocks     :SceSaveDataBlocks;
  mountMode  :DWORD; //SceSaveDataMountMode
  reserved   :array[0..31] of Byte;
 end;

 PSceSaveDataMount2=^SceSaveDataMount2;
 SceSaveDataMount2=packed record
  userId   :SceUserServiceUserId;
  align1   :Integer;
  dirName  :PSceSaveDataDirName;
  blocks   :SceSaveDataBlocks;
  mountMode:DWORD;
  reserved :array[0..31] of Byte;
  align2   :Integer;
 end;

 pSceSaveDataTransferringMount=^SceSaveDataTransferringMount;
 SceSaveDataTransferringMount=packed record
  userId     :SceUserServiceUserId;
  align1     :Integer;
  titleId    :pSceSaveDataTitleId;
  dirName    :PSceSaveDataDirName;
  fingerprint:pSceSaveDataFingerprint;
  reserved   :array[0..31] of Byte;
 end;

 PSceSaveDataMountResult=^SceSaveDataMountResult;
 SceSaveDataMountResult=packed record
  mountPoint    :SceSaveDataMountPoint;
  requiredBlocks:SceSaveDataBlocks;
  progress      :DWORD; //SDK_VERSION <  0x3500000
  mountStatus   :DWORD; //SDK_VERSION >= 0x3500000
  reserved      :array[0..27] of Byte;
  align1        :Integer;
 end;

 pSceSaveDataMountInfo=^SceSaveDataMountInfo;
 SceSaveDataMountInfo=packed record
  blocks    :SceSaveDataBlocks;
  freeBlocks:SceSaveDataBlocks;
  reserved  :array[0..31] of Byte;
 end;

 pSceSaveDataDirNameSearchCond=^SceSaveDataDirNameSearchCond;
 SceSaveDataDirNameSearchCond=packed record
  userId  :SceUserServiceUserId;
  _align  :Integer;
  titleId :pSceSaveDataTitleId;
  dirName :pSceSaveDataDirName;
  key     :DWORD; //SceSaveDataSortKey
  order   :DWORD; //SceSaveDataSortOrder
  reserved:array[0..31] of Byte;
 end;

 pSceSaveDataSearchInfo=^SceSaveDataSearchInfo;
 SceSaveDataSearchInfo=packed record
  blocks    :SceSaveDataBlocks;
  freeBlocks:SceSaveDataBlocks;
  reserved  :array[0..31] of Byte;
 end;

 pSceSaveDataDirNameSearchResult=^SceSaveDataDirNameSearchResult;
 SceSaveDataDirNameSearchResult=packed record
  hitNum     :DWORD;
  _align     :Integer;
  dirNames   :pSceSaveDataDirName;
  dirNamesNum:DWORD;
  setNum     :DWORD;
  params     :pSceSaveDataParam;
  infos      :pSceSaveDataSearchInfo;
  reserved   :array[0..11] of Byte;
  _align2    :Integer;
 end;

 SceSaveDataParamType=DWORD;

 pSceSaveDataEvent=^SceSaveDataEvent;
 SceSaveDataEvent=packed record
  _type    :DWORD; //SceSaveDataEventType;
  errorCode:Integer;
  userId   :SceUserServiceUserId;
  padding  :Integer;
  titleId  :SceSaveDataTitleId;
  dirName  :SceSaveDataDirName;
  reserved :array[0..39] of Byte;
 end;

 pSceSaveDataEventParam=Pointer;

 SceSaveDataEventCallbackFunc=procedure(event:pSceSaveDataEvent;userdata:Pointer);

 pSceSaveDataBackup=^SceSaveDataBackup;
 SceSaveDataBackup=packed record
  userId     :SceUserServiceUserId;
  _align     :Integer;
  titleId    :pSceSaveDataTitleId;
  dirName    :pSceSaveDataDirName;
  fingerprint:pSceSaveDataFingerprint;
  reserved   :array[0..31] of Byte;
 end;

 pSceSaveDataCheckBackupData=^SceSaveDataCheckBackupData;
 SceSaveDataCheckBackupData=packed record
  userId     :SceUserServiceUserId;
  _align     :Integer;
  titleId    :pSceSaveDataTitleId;
  dirName    :pSceSaveDataDirName;
  param      :pSceSaveDataParam;
  icon       :pSceSaveDataIcon;
  reserved   :array[0..31] of Byte;
 end;

implementation

{
uses
 sys_path,
 sys_signal;
}

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

///

type
 t_init_version=(VERSION_INIT_0,VERSION_INIT_2,VERSION_INIT_3,VERSION_INIT_CDLG);

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
 end;

var
 g_instance:TSaveDataInstance;

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

function ps4_sceSaveDataDelete(del:pSceSaveDataDelete):Integer;
begin
 Result:=0;
end;

function IsLoggedIn(userId:Integer):Integer; inline;
begin
 //sceUserServiceIsLoggedIn
 Result:=0;
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

function is_sdm(name:pchar):Boolean;
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
 if is_sdm(@dirName^.data) then
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
  if is_sdm(@mount^.dirName^.data) and ((mount^.mountMode and SDM_RDONLY)=0) then
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

  if is_sdm(@mount^.dirName^.data) and ((mount^.mountMode and SDM_RDONLY)=0) then
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

 Result:=CheckSaveDataMount(mount,mountResult,False);
 if (Result<>0) then Exit;

 mountResult^:=Default(SceSaveDataMountResult);

 mountResult^.mountPoint :='/savedata0';
 mountResult^.mountStatus:=SCE_SAVE_DATA_MOUNT_STATUS_CREATED;

 Result:=0;
 {
 _sig_lock;
 Result:=FetchSaveMount(PChar(mount^.dirName),@mountResult^.mountPoint,mount^.mountMode);
 _sig_unlock;
 }

 Writeln('sceSaveDataMount');

 if (Result=0) and
    ((mount^.mountMode and (SDM_CREATE or SDM_CREATE2))<>0) then
 begin
  mountResult^.mountStatus:=SCE_SAVE_DATA_MOUNT_STATUS_CREATED;
 end;

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

 Result:=CheckSaveDataMount(@tmp,mountResult,False);
 if (Result<>0) then Exit;

 mountResult^:=Default(SceSaveDataMountResult);

 mountResult^.mountPoint :='/savedata0';
 mountResult^.mountStatus:=SCE_SAVE_DATA_MOUNT_STATUS_CREATED;

 Result:=0;
 {
 _sig_lock;
 Result:=FetchSaveMount(PChar(mount^.dirName),@mountResult^.mountPoint,mount^.mountMode);
 _sig_unlock;
 }

 Writeln('sceSaveDataMount2');

 if (Result=0) and
    ((mount^.mountMode and (SDM_CREATE or SDM_CREATE2))<>0) then
 begin
  mountResult^.mountStatus:=SCE_SAVE_DATA_MOUNT_STATUS_CREATED;
 end;

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

 Result:=CheckSaveDataMount(@tmp,mountResult,True);
 if (Result<>0) then Exit;

 mountResult^:=Default(SceSaveDataMountResult);

 Result:=0;

 Writeln('sceSaveDataTransferringMount');

 {
 _sig_lock;
 Result:=FetchSaveMount(PChar(mount^.dirName),@mountResult^.mountPoint,SCE_SAVE_DATA_MOUNT_MODE_RDONLY);
 _sig_unlock;
 }

end;

function ps4_sceSaveDataUmount(mountPoint:PSceSaveDataMountPoint):Integer;
begin
 Result:=0;

 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Writeln('sceSaveDataUmount');

 {
 _sig_lock;
 Result:=UnMountSavePath(PChar(mountPoint));
 _sig_unlock;
 }
end;

function ps4_sceSaveDataUmountWithBackup(mountPoint:PSceSaveDataMountPoint):Integer;
var
 event:SceSaveDataEvent;
begin
 Result:=0;

 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Writeln('sceSaveDataUmountWithBackup');

 {
 _sig_lock;
 Result:=UnMountSavePath(PChar(mountPoint));
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

function ps4_sceSaveDataGetMountInfo(mountPoint:PSceSaveDataMountPoint;
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

const
 SCE_SAVE_DATA_SORT_KEY_DIRNAME    =0;
 SCE_SAVE_DATA_SORT_KEY_USER_PARAM =1;
 SCE_SAVE_DATA_SORT_KEY_BLOCKS     =2;
 SCE_SAVE_DATA_SORT_KEY_MTIME      =3;
 SCE_SAVE_DATA_SORT_KEY_FREE_BLOCKS=4;

 SCE_SAVE_DATA_SORT_ORDER_ASCENT =0;
 SCE_SAVE_DATA_SORT_ORDER_DESCENT=1;

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
  SCE_SAVE_DATA_SORT_ORDER_ASCENT :;
  SCE_SAVE_DATA_SORT_ORDER_DESCENT:;
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

function ps4_sceSaveDataGetParam(mountPoint:PSceSaveDataMountPoint;
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
function ps4_sceSaveDataSetParam(mountPoint:PSceSaveDataMountPoint;
                                 paramType:SceSaveDataParamType;
                                 paramBuf:Pointer;
                                 paramBufSize:QWORD):Integer;
begin
 Result:=0;
end;

function ps4_sceSaveDataSaveIcon(mountPoint:PSceSaveDataMountPoint;
                                 param:pSceSaveDataIcon):Integer;
begin
 if (mountPoint=nil) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 if (param=nil)      then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 Result:=0;
end;

//Load icon
function ps4_sceSaveDataLoadIcon(mountPoint:PSceSaveDataMountPoint;
                                 param:pSceSaveDataIcon):Integer;
begin
 if (mountPoint=nil) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 if (param=nil)      then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 Result:=SCE_SAVE_DATA_ERROR_FILE_NOT_FOUND;
end;

function ps4_sceSaveDataRegisterEventCallback(cb:SceSaveDataEventCallbackFunc;userdata:Pointer):Integer;
begin
 backup.cb:=cb;
 backup.userdata:=userdata;
 Result:=0;
end;

function ps4_sceSaveDataGetEventResult(param:pSceSaveDataEventParam;
                                       event:pSceSaveDataEvent):Integer;
begin
 if (event=nil) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);

 event^:=Default(SceSaveDataEvent);

 if backup.queue.dequeue(event^) then
 begin
  Result:=0;
 end else
 begin
  Result:=SCE_SAVE_DATA_ERROR_NOT_FOUND;
 end;
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
 backup.queue.Create(32);
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

