unit ps4_libSceCompanionUtil;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 ps4_libSceNet,
 Classes,
 SysUtils;

type
 pSceCompanionUtilOptParam=^SceCompanionUtilOptParam;
 SceCompanionUtilOptParam=packed record
  thisSize:QWORD;
  workMemory:Pointer;
  workMemorySize:QWORD;
 end;

 pSceNetInAddr=^SceNetInAddr;
 SceNetInAddr=packed record
  s_addr:SceNetInAddr_t;
 end;

 pSceNetSaFamily_t=^SceNetSaFamily_t;
 SceNetSaFamily_t=Byte;

 pSceNetInPort_t=^SceNetInPort_t;
 SceNetInPort_t=Word;

 pSceNetSockaddrIn=^SceNetSockaddrIn;
 SceNetSockaddrIn=packed record
  sin_len:Byte;
  sin_family:SceNetSaFamily_t;
  sin_port:SceNetInPort_t;
  sin_addr:SceNetInAddr;
  sin_vport:SceNetInPort_t;
  sin_zero:array[0..5] of char;
 end;

 pSceCompanionUtilDeviceInfo=^SceCompanionUtilDeviceInfo;
 SceCompanionUtilDeviceInfo=packed record
  userId,addr:SceNetSockaddrIn;
  reserved:array[0..235] of char;
 end;

 pSceCompanionUtilEvent=^SceCompanionUtilEvent;
 SceCompanionUtilEvent=packed record
  event:Integer;
  union:packed record
  Case Byte of
   0:(deviceInfo:SceCompanionUtilDeviceInfo);
   1:(userId,reserved:array[0..255] of char);
   end;
 end;

implementation

function ps4_sceCompanionUtilOptParamInitialize(pOptParam:SceCompanionUtilOptParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceCompanionUtilInitialize(const option:pSceCompanionUtilOptParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceCompanionUtilGetEvent(pEvent:pSceCompanionUtilEvent):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceCompanionUtil(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceCompanionUtil');
 lib^.set_proc($20F37F1514AB69F9,@ps4_sceCompanionUtilOptParamInitialize);
 lib^.set_proc($C5BD7194885FD106,@ps4_sceCompanionUtilInitialize);
 lib^.set_proc($704E4CB32D755A15,@ps4_sceCompanionUtilGetEvent);
end;

initialization
 ps4_app.RegistredPreLoad('libSceCompanionUtil.prx',@Load_libSceCompanionUtil);

end.

