unit SceSaveData;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 kern_proc,
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

const
 SCE_SAVE_DATA_ICON_WIDTH       =228;
 SCE_SAVE_DATA_ICON_HEIGHT      =128;
 SCE_SAVE_DATA_ICON_FILE_MAXSIZE=SCE_SAVE_DATA_ICON_WIDTH*SCE_SAVE_DATA_ICON_HEIGHT*4;

type
 pSceSaveDataIcon=^SceSaveDataIcon;
 SceSaveDataIcon=packed record
  buf     :Pointer;
  bufSize :QWORD;
  dataSize:QWORD;
  reserved:array[0..31] of Byte;
 end;

const
 //SceSaveDataSaveDataMemoryOption
 SCE_SAVE_DATA_MEMORY_OPTION_NONE         =0;
 SCE_SAVE_DATA_MEMORY_OPTION_SET_PARAM    =1;
 SCE_SAVE_DATA_MEMORY_OPTION_DOUBLE_BUFFER=2;

type
 PSceSaveDataMemorySetup2=^SceSaveDataMemorySetup2;
 SceSaveDataMemorySetup2=packed record
  option        :DWORD; //SceSaveDataSaveDataMemoryOption
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
 SCE_SAVE_DATA_BLOCK_SIZE     =32768;
 SCE_SAVE_DATA_BLOCKS_MIN2    =96;
 SCE_SAVE_DATA_BLOCKS_MAX     =32768;
 SCE_SAVE_DATA_MEMORY_MAXSIZE3=32*1024*1024;

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

 pSceSaveDataRestoreBackupData=^SceSaveDataRestoreBackupData;
 SceSaveDataRestoreBackupData=packed record
  userId     :SceUserServiceUserId;
  _align1    :Integer;
  titleId    :pSceSaveDataTitleId;
  dirName    :pSceSaveDataDirName;
  fingerprint:pSceSaveDataFingerprint;
  progress   :DWORD; //SDK_VERSION <  0x3500000
  reserved   :array[0..31] of Byte;
  _align2    :Integer;
 end;

const
 MAINTITLE_DEFAULT:array[0..30] of PChar=(
  #227#130#187#227#131#188#227#131#150#227#131#135#227#131#188#227#130#191,
  #83#97#118#101#100#32#68#97#116#97,
  #68#111#110#110#195#169#101#115#32#115#97#117#118#101#103#97#114#100#195#169#101#115,
  #68#97#116#111#115#32#103#117#97#114#100#97#100#111#115,
  #71#101#115#112#101#105#99#104#101#114#116#101#32#68#97#116#101#110,
  #68#97#116#105#32#115#97#108#118#97#116#105,
  #79#112#103#101#115#108#97#103#101#110#32#100#97#116#97,
  #68#97#100#111#115#32#103#117#97#114#100#97#100#111#115,
  #208#161#208#190#209#133#209#128#208#176#208#189#208#181#208#189#208#189#209#139#208#181#32#208#180#208#176#208#189#208#189#209#139#208#181,
  #236#160#128#236#158#165#32#235#141#176#236#157#180#237#132#176,
  #228#191#157#229#173#152#232#179#135#230#150#153,
  #228#191#157#229#173#152#230#149#176#230#141#174,
  #84#97#108#108#101#110#110#101#116#117#116#32#116#105#101#100#111#116,
  #83#112#97#114#97#100#101#32#100#97#116#97,
  #71#101#109#116#101#32#100#97#116#97,
  #76#97#103#114#101#100#101#32#100#97#116#97,
  #90#97#112#105#115#97#110#101#32#100#97#110#101,
  #68#97#100#111#115#32#115#97#108#118#111#115,
  #83#97#118#101#100#32#68#97#116#97,
  #75#97#121#196#177#116#108#196#177#32#86#101#114#105#108#101#114,
  #68#97#116#111#115#32#103#117#97#114#100#97#100#111#115,
  #216#168#217#138#216#167#217#134#216#167#216#170#32#217#133#216#173#217#129#217#136#216#184#216#169,
  #68#111#110#110#195#169#101#115#32#115#97#117#118#101#103#97#114#100#195#169#101#115,
  #85#108#111#197#190#101#110#195#161#32#100#97#116#97,
  #77#101#110#116#101#116#116#32#97#100#97#116,
  #206#145#207#128#206#191#206#184#206#183#206#186#206#181#207#133#206#188#206#173#206#189#206#177#32#206#180#206#181#206#180#206#191#206#188#206#173#206#189#206#177,
  #68#97#116#101#32#115#97#108#118#97#116#101,
  #224#184#130#224#185#137#224#184#173#224#184#161#224#184#185#224#184#165#224#184#151#224#184#181#224#185#136#224#184#154#224#184#177#224#184#153#224#184#151#224#184#182#224#184#129#224#185#132#224#184#167#224#185#137,
  #68#225#187#175#32#108#105#225#187#135#117#32#196#145#195#163#32#108#198#176#117,
  #68#97#116#97#32#84#101#114#115#105#109#112#97#110,
  #208#151#208#177#208#181#209#128#208#181#208#182#208#181#208#189#209#150#32#208#180#208#176#208#189#209#150
 );

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

function strnlen_s  (s:PChar;maxlen:ptrint):ptrint;
function strncasecmp(str1,str2:PChar;maxlen:ptrint):Integer;
function strncpy_s  (dst,src:PChar;maxlen:ptrint):PChar; inline;

function is_sdmemory(name:pchar):Boolean;
function GetMountSlotIdByMountPoint(name:pchar;var slot_id:Integer):Integer;

function CheckReserved(var buf;len:DWORD):Boolean;

function CheckSaveDataDelete(del:pSceSaveDataDelete):Integer;

function CheckSaveDataMount(mount      :pSceSaveDataMount;
                            pResult    :pSceSaveDataMountResult;
                            Transfering:Boolean):Integer;

function CheckLoadSaveDataIcon (icon:pSceSaveDataIcon):Integer;
function CheckSaveSaveDataIcon (icon:pSceSaveDataIcon):Integer;

function CheckSaveDataBackup   (backup:pSceSaveDataBackup):Integer;
function CheckCheckBackupData  (check:pSceSaveDataCheckBackupData;internal:Boolean):Integer;
function CheckRestoreBackupData(restore:pSceSaveDataRestoreBackupData):Integer;
function CheckMountInfo        (info:pSceSaveDataMountInfo):Integer;

implementation

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

function GetMountSlotIdByMountPoint(name:pchar;var slot_id:Integer):Integer;
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

function CheckSaveDataDelete(del:pSceSaveDataDelete):Integer;
begin
 if (p_proc.p_sdk_version < $3500000) then
 begin
  Result:=CheckSaveDataDelete1(del);
 end else
 begin
  Result:=CheckSaveDataDelete2(del);
 end;
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

function CheckOutputSceSaveDataMountPoint1(pResult:pSceSaveDataMountResult):Integer; inline;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (pResult=nil) then Exit;

 if CheckReserved(pResult^.reserved,sizeof(pResult^.reserved)) then
 begin
  Result:=0;
 end;
end;

function CheckOutputSceSaveDataMountPoint2(pResult:pSceSaveDataMountResult):Integer; inline;
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

function CheckLoadSaveDataIcon(icon:pSceSaveDataIcon):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (icon=nil) then Exit;

 if (icon^.buf<>nil) then
 if (icon^.bufSize<>0) then
 if CheckReserved(icon^.reserved,sizeof(icon^.reserved)) then
 begin
  Result:=0;
 end;
end;

function CheckSaveSaveDataIcon(icon:pSceSaveDataIcon):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (icon=nil) then Exit;

 if (icon^.buf<>nil) then
 if (icon^.bufSize<>0) then
 if (QWORD(icon^.dataSize-1)<116736) then
 if (icon^.dataSize<=icon^.bufSize) then
 if CheckReserved(icon^.reserved,sizeof(icon^.reserved)) then
 begin
  Result:=0;
 end;
end;

function CheckSaveDataBackup(backup:pSceSaveDataBackup):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (backup=nil) then Exit;

 if IsLoggedIn(backup^.userId)<>0 then
 begin
  Exit(SCE_SAVE_DATA_ERROR_INVALID_LOGIN_USER);
 end;

 if CheckTitleId(backup^.titleId)=0 then
 if CheckDirName(backup^.dirName,false)=0 then
 begin
  Result:=0;
 end;
end;

function CheckCheckBackupData(check:pSceSaveDataCheckBackupData;internal:Boolean):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (check=nil) then Exit;

 if IsLoggedIn(check^.userId)<>0 then
 begin
  Exit(SCE_SAVE_DATA_ERROR_INVALID_LOGIN_USER);
 end;

 if CheckTitleId(check^.titleId)=0 then
 begin
  if (check^.dirName=nil) then Exit;
  if (check^.dirName^.data[0]=#0) then Exit;

  if CheckDirName(check^.dirName,internal)=0 then
  begin

   if (check^.param<>nil) then
   begin
    if not CheckReserved(check^.param^.reserved,sizeof(check^.param^.reserved)) then
    begin
     Exit;
    end;
   end;

   if (check^.icon<>nil) then
   begin
    if CheckLoadSaveDataIcon(check^.icon)<>0 then
    begin
     Exit;
    end;
   end;

   if CheckReserved(check^.reserved,sizeof(check^.reserved)) then
   begin
    Result:=0;
   end;
  end;
 end;
end;

function CheckRestoreBackupData1(restore:pSceSaveDataRestoreBackupData):Integer; inline;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (restore=nil) then Exit;

 if IsLoggedIn(restore^.userId)<>0 then
 begin
  Exit(SCE_SAVE_DATA_ERROR_INVALID_LOGIN_USER);
 end;

 if CheckTitleId(restore^.titleId)=0 then
 if CheckDirName(restore^.dirName,False)=0 then
 if CheckFingerprint(restore^.fingerprint)=0 then
 if CheckReserved(restore^.reserved,sizeof(restore^.reserved)) then
 begin

  if (restore^.titleId=nil) then
  begin
   if (restore^.fingerprint<>nil) then
   begin
    Exit;
   end;
  end else
  if (restore^.fingerprint=nil) then
  begin
   Exit;
  end;

  Result:=0;
 end;
end;

function CheckRestoreBackupData2(restore:pSceSaveDataRestoreBackupData):Integer; inline;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (restore=nil) then Exit;

 if IsLoggedIn(restore^.userId)<>0 then
 begin
  Exit(SCE_SAVE_DATA_ERROR_INVALID_LOGIN_USER);
 end;

 if CheckTitleId(restore^.titleId)=0 then
 if CheckDirName(restore^.dirName,False)=0 then
 if CheckFingerprint(restore^.fingerprint)=0 then
 if (restore^.progress=0) then
 if CheckReserved(restore^.reserved,sizeof(restore^.reserved)) then
 begin

  if (restore^.titleId=nil) then
  begin
   if (restore^.fingerprint<>nil) then
   begin
    Exit;
   end;
  end else
  if (restore^.fingerprint=nil) then
  begin
   Exit;
  end;

  Result:=0;
 end;
end;

function CheckRestoreBackupData(restore:pSceSaveDataRestoreBackupData):Integer;
begin
 if (p_proc.p_sdk_version < $3500000) then
 begin
  Result:=CheckRestoreBackupData1(restore);
 end else
 begin
  Result:=CheckRestoreBackupData2(restore);
 end;
end;

function CheckMountInfo(info:pSceSaveDataMountInfo):Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (info=nil) then Exit;

 if CheckReserved(info^.reserved,sizeof(info^.reserved)) then
 begin
  Result:=0;
 end;
end;


end.

