unit ps4_libSceAppContent;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

type
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

function ps4_sceAppContentInitialize(initParam:PSceAppContentInitParam;bootParam:PSceAppContentBootParam):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceAppContentInitialize');
 Result:=0;
end;

Const
 SCE_APP_CONTENT_APPPARAM_ID_SKU_FLAG=0;
 SCE_APP_CONTENT_APPPARAM_ID_USER_DEFINED_PARAM_1=1;
 SCE_APP_CONTENT_APPPARAM_ID_USER_DEFINED_PARAM_4=4;

 SCE_APP_CONTENT_ERROR_PARAMETER=-2133262334;//0x80D90002

 SCE_APP_CONTENT_APPPARAM_SKU_FLAG_TRIAL=1;
 SCE_APP_CONTENT_APPPARAM_SKU_FLAG_FULL =2;

 SCE_APP_CONTENT_ADDCONT_DOWNLOAD_STATUS_INSTALLED=4;

function ps4_sceAppContentAppParamGetInt(paramId:DWORD;value:PInteger):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceAppContentAppParamGetInt:',paramId);
 Case SCE_APP_CONTENT_APPPARAM_ID_SKU_FLAG of
  SCE_APP_CONTENT_APPPARAM_ID_SKU_FLAG:Result:=SCE_APP_CONTENT_APPPARAM_SKU_FLAG_FULL;
  1..4:Result:=SCE_APP_CONTENT_ADDCONT_DOWNLOAD_STATUS_INSTALLED;
  else
   Result:=SCE_APP_CONTENT_ERROR_PARAMETER;
 end;
end;

function Load_libSceAppContent(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceAppContent');

 lib^.set_proc($47D940F363AB68DB,@ps4_sceAppContentInitialize);
 lib^.set_proc($F7D6FCD88297A47E,@ps4_sceAppContentAppParamGetInt);
end;


initialization
 ps4_app.RegistredPreLoad('libSceAppContent.prx',@Load_libSceAppContent);

end.

