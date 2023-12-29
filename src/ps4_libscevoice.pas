unit ps4_libSceVoice;

{$mode ObjFPC}{$H+}

interface


uses
  ps4_program,
  Classes,
  SysUtils;

type
  pSceVoiceStartParam=^SceVoiceStartParam;
  SceVoiceStartParam=packed record
end;
  
  pSceVoicePortParam=^SceVoicePortParam;
  SceVoicePortParam=packed record
end;

implementation

function ps4_sceVoiceInit(
          pArg:Pointer;  //pSceVoiceInitParam
          version:DWORD //SceVoiceVersion
          ):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVoiceQoSInit(
          pMemBlock:Pointer;
          memSize:DWORD;
          appType:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVoiceStart(pArg:pSceVoiceStartParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVoiceCreatePort(portId:DWORD; const pArg:pSceVoicePortParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVoiceConnectIPortToOPort(ips,ops:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceVoice(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceVoice');
 lib^.set_proc($F53AE1B86CDB7AB4,@ps4_sceVoiceInit);
 lib^.set_proc($E78A613C7D8B665B,@ps4_sceVoiceStart);
 lib^.set_proc($9D7A637B9C8DA5A1,@ps4_sceVoiceCreatePort);
 lib^.set_proc($A15F4601D276DC6C,@ps4_sceVoiceConnectIPortToOPort);
end;

function Load_libSceVoiceQoS(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceVoiceQoS');
 lib^.set_proc($53C21F365EBF0ACB,@ps4_sceVoiceQoSInit);
end;

initialization
 ps4_app.RegistredPreLoad('libSceVoice.prx'   ,@Load_libSceVoice);
 ps4_app.RegistredPreLoad('libSceVoiceQoS.prx',@Load_libSceVoiceQoS);

end.

