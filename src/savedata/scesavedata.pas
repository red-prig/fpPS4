unit SceSaveData;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
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
 SCE_SAVE_DATA_ERROR_MOUNT_INHIBIT                       =-2137063408; // 0x809f0010
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

 pSceSaveDataMountPoint=^SceSaveDataMountPoint;
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
  progress:Integer;  //SDK_VERSION < 0x3500000
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

const
 //SceSaveDataSortKey
 SDSK_DIRNAME    =0;
 SDSK_USER_PARAM =1;
 SDSK_BLOCKS     =2;
 SDSK_MTIME      =3;
 SDSK_FREE_BLOCKS=4;

 //SceSaveDataSortOrder
 SDSO_ASCENT =0;
 SDSO_DESCENT=1;

type
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

end.

