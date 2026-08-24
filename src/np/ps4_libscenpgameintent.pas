unit ps4_libSceNpGameIntent;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib;

implementation

uses
 ps4_libSceUserService;

const
 SCE_NP_GAME_INTENT_TYPE_MAX_SIZE=(32+1);
 SCE_NP_GAME_INTENT_DATA_MAX_SIZE=(16*1024+1);

type
 pSceNpGameIntentInitParam=^SceNpGameIntentInitParam;
 SceNpGameIntentInitParam=packed record
  size    :QWORD;
  reserved:array[0..31] of Byte;
 end;

 pSceNpGameIntentData=^SceNpGameIntentData;
 SceNpGameIntentData=packed record
  data   :array[0..SCE_NP_GAME_INTENT_DATA_MAX_SIZE-1] of Byte;
  padding:array[0..6] of Byte;
 end;

 pSceNpGameIntentInfo=^SceNpGameIntentInfo;
 SceNpGameIntentInfo=packed record
  size      :QWORD;
  userId    :SceUserServiceUserId;
  intentType:array[0..SCE_NP_GAME_INTENT_TYPE_MAX_SIZE-1] of AnsiChar;
  padding   :array[0..6] of Byte;
  reserved  :array[0..255] of Byte;
  intentData:SceNpGameIntentData;
 end;

function ps4_sceNpGameIntentInitialize(initParam:pSceNpGameIntentInitParam):Integer;
begin
 Result:=6;
end;

function ps4_sceNpGameIntentReceiveIntent(intentInfo:pSceNpGameIntentInfo):Integer;
begin
 Result:=0;
end;

function ps4_sceNpGameIntentGetPropertyValueString(intentData:pSceNpGameIntentData;
                                                   key:Pchar;
                                                   valueBuf:Pchar;
                                                   bufSize:QWORD):Integer;
begin
 if (valueBuf<>nil) then
 begin
  FillChar(valueBuf^,bufSize,0);
 end;
 Result:=0;
end;

{$WARN 4110 off}
function Load_libSceNpGameIntent(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpGameIntent');

 lib:=Result^.add_lib('libSceNpGameIntent');
 lib.set_proc($9BCEC11F1B7F1FAD,@ps4_sceNpGameIntentInitialize);
 lib.set_proc($8C4217500AFD5C4F,@ps4_sceNpGameIntentReceiveIntent);
 lib.set_proc($ACF97420D35CFCCF,@ps4_sceNpGameIntentGetPropertyValueString);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpGameIntent.prx',@Load_libSceNpGameIntent);

end.

