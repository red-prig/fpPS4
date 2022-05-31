unit ps4_libSceSaveData;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

Const
 SCE_SAVE_DATA_TITLE_MAXSIZE   =128;
 SCE_SAVE_DATA_SUBTITLE_MAXSIZE=128;
 SCE_SAVE_DATA_DETAIL_MAXSIZE  =1024;

 SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE=32;
 SCE_SAVE_DATA_MOUNT_POINT_DATA_MAXSIZE=16;
 SCE_SAVE_DATA_MOUNT_STATUS_CREATED=$00000001;

type
 PSceSaveDataParam=^SceSaveDataParam;
 SceSaveDataParam=packed record
  title:array[0..SCE_SAVE_DATA_TITLE_MAXSIZE-1] of AnsiChar;
  subTitle:array[0..SCE_SAVE_DATA_SUBTITLE_MAXSIZE-1] of AnsiChar;
  detail:array[0..SCE_SAVE_DATA_DETAIL_MAXSIZE-1] of AnsiChar;
  userParam:DWORD;
  align:DWORD;
  mtime:QWORD;
  reserved:array[0..31] of Byte;
 end;

 PSceSaveDataIcon=^SceSaveDataIcon;
 SceSaveDataIcon=packed record
  buf:Pointer;
  bufSize:size_t;
  dataSize:size_t;
  reserved:array[0..31] of Byte;
 end;

 PSceSaveDataMemorySetup2=^SceSaveDataMemorySetup2;
 SceSaveDataMemorySetup2=packed record
  option:DWORD;
  userId:Integer;
  memorySize:size_t;
  iconMemorySize:size_t;
  initParam:PSceSaveDataParam;
  initIcon:PSceSaveDataIcon;
  reserved:array[0..23] of Byte;
 end;

 PSceSaveDataMemorySetupResult=^SceSaveDataMemorySetupResult;
 SceSaveDataMemorySetupResult=packed record
  existedMemorySize:size_t;
  reserved:array[0..15] of Byte;
 end;

 PSceSaveDataDirName=^SceSaveDataDirName;
 SceSaveDataDirName=array[0..SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE-1] of Char;

 PSceSaveDataMountPoint=^SceSaveDataMountPoint;
 SceSaveDataMountPoint=array[0..SCE_SAVE_DATA_MOUNT_POINT_DATA_MAXSIZE-1] of Char;

 PSceSaveDataMount2=^SceSaveDataMount2;
 SceSaveDataMount2=packed record
  userId:Integer;
  align1:Integer;
  dirName:PSceSaveDataDirName;
  blocks:QWORD;
  mountMode:DWORD;
  reserved:array[0..31] of Byte;
  align2:Integer;
 end;

 PSceSaveDataMountResult=^SceSaveDataMountResult;
 SceSaveDataMountResult=packed record
  mountPoint:SceSaveDataMountPoint;
  requiredBlocks:QWORD;
  unused:DWORD;
  mountStatus:DWORD;
  reserved:array[0..27] of Byte;
  align1:Integer;
 end;

implementation

uses
 sys_signal;

function ps4_sceSaveDataInitialize(params:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceSaveDataInitialize3(params:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceSaveDataTerminate:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceSaveDataSetupSaveDataMemory(userId:Integer;
                                        memorySize:QWORD;
                                        param:PSceSaveDataParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function sceSaveDataSetupSaveDataMemory2(setupParam:PSceSaveDataMemorySetup2;
                                        _result:PSceSaveDataMemorySetupResult):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceSaveDataGetSaveDataMemory(userId:Integer;
                                      buf:Pointer;
                                      bufSize:size_t;
                                      offset:QWORD):Integer; SysV_ABI_CDecl;
begin
 FillChar(buf^,bufSize,0);
 Result:=0;
end;

function ps4_sceSaveDataSetSaveDataMemory(userId:Integer;
                                      buf:Pointer;
                                      bufSize:size_t;
                                      offset:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

const
 SCE_SAVE_DATA_ERROR_PARAMETER  =-2137063424; // 0x809F0000
 SCE_SAVE_DATA_ERROR_EXISTS     =-2137063417; // 0x809F0007
 SCE_SAVE_DATA_ERROR_MOUNT_FULL =-2137063412; // 0x809F000C
 SCE_SAVE_DATA_ERROR_NOT_MOUNTED=-2137063420; // 0x809F0004

function ps4_sceSaveDataMount2(mount:PSceSaveDataMount2;mountResult:PSceSaveDataMountResult):Integer; SysV_ABI_CDecl;
begin
 if (mount=nil) or (mountResult=nil) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 _sig_lock;
 Result:=FetchMount(PChar(mount^.dirName),@mountResult^.mountPoint);
 _sig_unlock;
end;

function ps4_sceSaveDataUmount(mountPoint:PSceSaveDataMountPoint):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=UnMountPath(PChar(mountPoint));
 _sig_unlock;
end;

type
 SceSaveDataParamType=DWORD;

function ps4_sceSaveDataSetParam(mountPoint:PSceSaveDataMountPoint;
                                 paramType:SceSaveDataParamType;
                                 paramBuf:Pointer;
                                 paramBufSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceSaveData(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceSaveData');

 lib^.set_proc($664661B2408F5C5C,@ps4_sceSaveDataInitialize);
 lib^.set_proc($4F2C2B14A0A82C66,@ps4_sceSaveDataInitialize3);
 lib^.set_proc($C8A0F2F12E722C0D,@ps4_sceSaveDataTerminate);
 lib^.set_proc($BFB00000CA342F3E,@ps4_sceSaveDataSetupSaveDataMemory);
 lib^.set_proc($EC1B79A410BF01CA,@ps4_sceSaveDataGetSaveDataMemory);
 lib^.set_proc($8776144735C64954,@ps4_sceSaveDataSetSaveDataMemory);
 lib^.set_proc($D33E393C81FE48D2,@ps4_sceSaveDataMount2);
 lib^.set_proc($04C47817F51E9371,@ps4_sceSaveDataUmount);
 lib^.set_proc($F39CEE97FFDE197B,@ps4_sceSaveDataSetParam);
end;

initialization
 ps4_app.RegistredPreLoad('libSceSaveData.prx',@Load_libSceSaveData);

end.

