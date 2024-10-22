unit ps4_libSceAppContent;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib;

implementation

uses
 param_sfo_ipc;

{
uses
 sys_path,
 sys_signal,
 param_sfo;
}

Const
 SCE_APP_CONTENT_APPPARAM_ID_SKU_FLAG            =0;
 SCE_APP_CONTENT_APPPARAM_ID_USER_DEFINED_PARAM_1=1;
 SCE_APP_CONTENT_APPPARAM_ID_USER_DEFINED_PARAM_2=2;
 SCE_APP_CONTENT_APPPARAM_ID_USER_DEFINED_PARAM_3=3;
 SCE_APP_CONTENT_APPPARAM_ID_USER_DEFINED_PARAM_4=4;

 SCE_APP_CONTENT_APPPARAM_SKU_FLAG_TRIAL=1;
 SCE_APP_CONTENT_APPPARAM_SKU_FLAG_FULL =2;

 SCE_NP_UNIFIED_ENTITLEMENT_LABEL_SIZE=17;

 SCE_APP_CONTENT_MOUNTPOINT_DATA_MAXSIZE=16;
 SCE_APP_CONTENT_ADDCONT_MOUNT_MAXNUM   =64;

 //E temporary data option
 SCE_APP_CONTENT_TEMPORARY_DATA_OPTION_NONE  =0;
 SCE_APP_CONTENT_TEMPORARY_DATA_OPTION_FORMAT=1;

 SCE_APP_CONTENT_ERROR_PARAMETER         =-2133262334; //0x80D90002
 SCE_APP_CONTENT_ERROR_DRM_NO_ENTITLEMENT=-2133262329; //0x80D90007

 //SceAppContentAddcontDownloadStatus
 SCE_APP_CONTENT_ADDCONT_DOWNLOAD_STATUS_NO_EXTRA_DATA     =0;
 SCE_APP_CONTENT_ADDCONT_DOWNLOAD_STATUS_NO_IN_QUEUE       =1;
 SCE_APP_CONTENT_ADDCONT_DOWNLOAD_STATUS_DOWNLOADING       =2;
 SCE_APP_CONTENT_ADDCONT_DOWNLOAD_STATUS_DOWNLOAD_SUSPENDED=3;
 SCE_APP_CONTENT_ADDCONT_DOWNLOAD_STATUS_INSTALLED         =4;

 SCE_APP_CONTENT_ENTITLEMENT_KEY_SIZE=16;

type
 SceNpServiceLabel=DWORD;

 PSceAppContentInitParam=^SceAppContentInitParam;
 SceAppContentInitParam=packed record
  reserved:array[0..31] of Byte;
 end;

 PSceAppContentBootParam=^SceAppContentBootParam;
 SceAppContentBootParam=packed record
  reserved1:array[0..3] of Byte;
  attr:DWORD;
  reserved2:array[0..31] of Byte;
 end;

 pSceNpUnifiedEntitlementLabel=^SceNpUnifiedEntitlementLabel;
 SceNpUnifiedEntitlementLabel=packed record
  data:array[0..SCE_NP_UNIFIED_ENTITLEMENT_LABEL_SIZE-1] of AnsiChar;
  padding:array[0..2] of Byte;
 end;

 pSceAppContentAddcontInfo=^SceAppContentAddcontInfo;
 SceAppContentAddcontInfo=packed record
  entitlementLabel:SceNpUnifiedEntitlementLabel;
  status:DWORD; //SceAppContentAddcontDownloadStatus
 end;

 pSceAppContentMountPoint=^SceAppContentMountPoint;
 SceAppContentMountPoint=array[0..SCE_APP_CONTENT_MOUNTPOINT_DATA_MAXSIZE-1] of AnsiChar;

 pSceAppContentEntitlementKey=^SceAppContentEntitlementKey;
 SceAppContentEntitlementKey=array[0..SCE_APP_CONTENT_ENTITLEMENT_KEY_SIZE-1] of AnsiChar;

function ps4_sceAppContentInitialize(initParam:PSceAppContentInitParam;bootParam:PSceAppContentBootParam):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceAppContentInitialize');

 param_sfo_ipc.init_param_sfo;

 Result:=0;
end;

function ps4_sceAppContentAppParamGetInt(paramId:DWORD;value:PInteger):Integer;
begin
 Result:=0;
 if (value=nil) then Exit(SCE_APP_CONTENT_ERROR_PARAMETER);
 Case SCE_APP_CONTENT_APPPARAM_ID_SKU_FLAG of
  SCE_APP_CONTENT_APPPARAM_ID_SKU_FLAG            :value^:=SCE_APP_CONTENT_APPPARAM_SKU_FLAG_FULL;
  SCE_APP_CONTENT_APPPARAM_ID_USER_DEFINED_PARAM_1:value^:=ParamSfoGetUInt('USER_DEFINED_PARAM_1');
  SCE_APP_CONTENT_APPPARAM_ID_USER_DEFINED_PARAM_2:value^:=ParamSfoGetUInt('USER_DEFINED_PARAM_2');
  SCE_APP_CONTENT_APPPARAM_ID_USER_DEFINED_PARAM_3:value^:=ParamSfoGetUInt('USER_DEFINED_PARAM_3');
  SCE_APP_CONTENT_APPPARAM_ID_USER_DEFINED_PARAM_4:value^:=ParamSfoGetUInt('USER_DEFINED_PARAM_4');
  else
   Result:=SCE_APP_CONTENT_ERROR_PARAMETER;
 end;
end;

function ps4_sceAppContentGetAddcontInfoList(serviceLabel:SceNpServiceLabel;
                                             list:pSceAppContentAddcontInfo;
                                             listNum:DWORD;
                                             hitNum:PDWORD):Integer;
begin
 Result:=0;
 Writeln('sceAppContentGetAddcontInfoList:0x',HexStr(serviceLabel,8));
 if (hitNum<>nil) then
 begin
  hitNum^:=0; //no DLC
 end;
end;

function ps4_sceAppContentGetAddcontInfo(serviceLabel:SceNpServiceLabel;
                                         entitlementLabel:pSceNpUnifiedEntitlementLabel;
                                         info:pSceAppContentAddcontInfo
                                        ):Integer;
begin
 if (entitlementLabel=nil) or (info=nil) then Exit(SCE_APP_CONTENT_ERROR_PARAMETER);

 Result:=SCE_APP_CONTENT_ERROR_DRM_NO_ENTITLEMENT;
end;

function ps4_sceAppContentTemporaryDataFormat(mountPoint:pSceAppContentMountPoint):Integer;
begin
 {
 _sig_lock;
 Result:=FormatTmpPath(PChar(mountPoint));
 _sig_unlock;
 }
 Result:=-1;
end;

function ps4_sceAppContentTemporaryDataMount(mountPoint:pSceAppContentMountPoint):Integer;
begin
 {
 _sig_lock;
 Result:=FetchTmpMount(PChar(mountPoint),SCE_APP_CONTENT_TEMPORARY_DATA_OPTION_FORMAT);
 _sig_unlock;
 }
 Result:=-1;
end;

function ps4_sceAppContentTemporaryDataMount2(option:DWORD;mountPoint:pSceAppContentMountPoint):Integer;
begin
 {
 _sig_lock;
 Result:=FetchTmpMount(PChar(mountPoint),option);
 _sig_unlock;
 }
 Result:=-1;
end;

function ps4_sceAppContentTemporaryDataUnmount(mountPoint:pSceAppContentMountPoint):Integer;
begin
 {
 _sig_lock;
 Result:=UnMountTmpPath(PChar(mountPoint));
 _sig_unlock;
 }
 Result:=-1;
end;

function ps4_sceAppContentTemporaryDataGetAvailableSpaceKb(mountPoint:pSceAppContentMountPoint;availableSpaceKb:PQWORD):Integer;
begin
 {
 _sig_lock;
 Result:=GetTmpPathAvailableSpaceKb(PChar(mountPoint),availableSpaceKb);
 _sig_unlock;
 }
 Result:=-1;
end;

function ps4_sceAppContentDownloadDataGetAvailableSpaceKb(mountPoint:pSceAppContentMountPoint;availableSpaceKb:PQWORD):Integer;
begin
 {
 _sig_lock;
 Result:=GetDownloadAvailableSpaceKb(PChar(mountPoint),availableSpaceKb);
 _sig_unlock;
 }
 Result:=-1;
end;

function ps4_sceAppContentGetEntitlementKey(serviceLabel:SceNpServiceLabel;
                                            entitlementLabel:pSceNpUnifiedEntitlementLabel;
                                            key:pSceAppContentEntitlementKey
                                           ):Integer;
begin
 if (entitlementLabel=nil) or (key=nil) then Exit(SCE_APP_CONTENT_ERROR_PARAMETER);

 Result:=0;
end;

function ps4_sceAppContentAddcontUnmount(mountPoint:pSceAppContentMountPoint):Integer;
begin
 Result:=0;
end;

function Load_libSceAppContent(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 //export module is libSceAppContentUtil
 Result:=obj_new_int('libSceAppContentUtil');

 //

 lib:=Result^.add_lib('libSceAppContent');
 lib.set_proc($47D940F363AB68DB,@ps4_sceAppContentInitialize);
 lib.set_proc($F7D6FCD88297A47E,@ps4_sceAppContentAppParamGetInt);
 lib.set_proc($C6777C049CC0C669,@ps4_sceAppContentGetAddcontInfoList);
 lib.set_proc($9B8EE3B8E987D151,@ps4_sceAppContentGetAddcontInfo);
 lib.set_proc($6B937B9401B4CB64,@ps4_sceAppContentTemporaryDataFormat);
 lib.set_proc($EDB38B5FAE88CFF5,@ps4_sceAppContentTemporaryDataMount);
 lib.set_proc($6EE61B78B3865A60,@ps4_sceAppContentTemporaryDataMount2);
 lib.set_proc($6DCA255CC9A9EAA4,@ps4_sceAppContentTemporaryDataUnmount);
 lib.set_proc($49A2A26F6520D322,@ps4_sceAppContentTemporaryDataGetAvailableSpaceKb);
 lib.set_proc($1A5EB0E62D09A246,@ps4_sceAppContentDownloadDataGetAvailableSpaceKb);
 lib.set_proc($5D3591D145EF720B,@ps4_sceAppContentGetEntitlementKey);
 lib.set_proc($DEB1D6695FF5282E,@ps4_sceAppContentAddcontUnmount);
end;

var
 stub:t_int_file;

initialization
 reg_int_file(stub,'libSceAppContent.prx',@Load_libSceAppContent);

end.

