unit ps4_libSceAutoMounterClient;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
  subr_dynlib;

type
 char64 =array[0.. 63] of AnsiChar;
 char128=array[0..217] of AnsiChar;
 char256=array[0..255] of AnsiChar;

 pchar64 =^char64;
 pchar128=^char128;

 pSceAutoMounterClientUsbDeviceInfo=^SceAutoMounterClientUsbDeviceInfo;
 SceAutoMounterClientUsbDeviceInfo=packed record
  deviceName        :char128;
  totalSpace        :QWORD;
  availableSpace    :QWORD;
  vendorId          :WORD;
  productId         :WORD;
  bcdDevice         :WORD;
  manufacturer      :char256;
  _align            :WORD;
  manufacturerLength:QWORD;
  product           :char256;
  productLength     :QWORD;
  serialNumber      :char256;
  serialNumberLength:QWORD;
 end;

 SceAutoMounterClientCallbackFunc=Procedure(deviceName:pchar128;userPtr:Pointer);

implementation

type
 pScePthreadAttr=Pointer;

function ps4_sceAutoMounterClientInit(attr:pScePthreadAttr):Integer;
begin
 Result:=0;
end;

function ps4_sceAutoMounterClientTerm():Integer;
begin
 Result:=0;
end;

// eventId -> [0..1]
function ps4_sceAutoMounterClientRegisterCallback(eventId:Integer;
                                                  callbackFunc:SceAutoMounterClientCallbackFunc;
                                                  userPtr:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceAutoMounterClientUnregisterCallback(eventId:Integer;
                                                    callbackFunc:SceAutoMounterClientCallbackFunc):Integer;
begin
 Result:=0;
end;

function ps4_sceAutoMounterClientGetUsbDeviceList(p_names:pchar64;p_len:PInteger):Integer;
begin
 if (p_names=nil) or (p_len=nil) then
 begin
  Exit(Integer($80f40002));
 end;

 p_len^:=0;
end;

function ps4_sceAutoMounterClientGetUsbDeviceInfo(deviceName:pchar128;p_info:pSceAutoMounterClientUsbDeviceInfo):Integer;
begin
 Exit(Integer($80f40002));
end;

function Load_libSceAutoMounterClient(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceAutoMounterClient');

 lib:=Result^.add_lib('libSceAutoMounterClient');

 lib.set_proc($8E0A5CFF0E07EC55,@ps4_sceAutoMounterClientInit);
 lib.set_proc($8845C81334E336D2,@ps4_sceAutoMounterClientTerm);
 lib.set_proc($6C6B65D3847DA226,@ps4_sceAutoMounterClientRegisterCallback);
 lib.set_proc($8A4D2419407B524C,@ps4_sceAutoMounterClientUnregisterCallback);
 lib.set_proc($61178D0F7130823D,@ps4_sceAutoMounterClientGetUsbDeviceList);
 lib.set_proc($F26AC178B5B4664D,@ps4_sceAutoMounterClientGetUsbDeviceInfo);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceAutoMounterClient.prx',@Load_libSceAutoMounterClient);

end.

