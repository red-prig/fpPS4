unit ps4_libSceCompanionUtil;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib,
 ps4_libSceUserService,
 ps4_libnet;

const
 SCE_COMPANION_UTIL_ERROR_NO_EVENT=-2136145912; //0x80AD0008

type
 pSceCompanionUtilOptParam=^SceCompanionUtilOptParam;
 SceCompanionUtilOptParam=packed record
  thisSize      :QWORD;
  workMemory    :Pointer;
  workMemorySize:QWORD;
 end;

 pSceCompanionUtilDeviceInfo=^SceCompanionUtilDeviceInfo;
 SceCompanionUtilDeviceInfo=packed record
  userId  :SceUserServiceUserId;
  addr    :SceNetSockaddrIn;
  reserved:array[0..235] of char;
 end;

 pSceCompanionUtilEvent=^SceCompanionUtilEvent;
 SceCompanionUtilEvent=packed record
  event:Integer;
  union:packed record
   case Byte of
    0:(deviceInfo:SceCompanionUtilDeviceInfo);
    1:(userId    :SceUserServiceUserId);
    2:(reserved  :array[0..255] of char);
  end;
 end;

implementation

function ps4_sceCompanionUtilOptParamInitialize(pOptParam:SceCompanionUtilOptParam):Integer;
begin
 Result:=0;
end;

function ps4_sceCompanionUtilInitialize(const option:pSceCompanionUtilOptParam):Integer;
begin
 Result:=0;
end;

function ps4_sceCompanionUtilGetEvent(pEvent:pSceCompanionUtilEvent):Integer;
begin
 Result:=SCE_COMPANION_UTIL_ERROR_NO_EVENT;
end;

function Load_libSceCompanionUtil(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceCompanionUtil');

 lib:=Result^.add_lib('libSceCompanionUtil');
 lib.set_proc($20F37F1514AB69F9,@ps4_sceCompanionUtilOptParamInitialize);
 lib.set_proc($C5BD7194885FD106,@ps4_sceCompanionUtilInitialize);
 lib.set_proc($704E4CB32D755A15,@ps4_sceCompanionUtilGetEvent);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceCompanionUtil.prx',@Load_libSceCompanionUtil);

end.

