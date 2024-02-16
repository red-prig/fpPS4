unit ps4_libSceVoice;

{$mode ObjFPC}{$H+}

interface


uses
  ps4_program,
  Classes,
  SysUtils;

const
 SCE_VOICE_ERROR_ARGUMENT_INVALID=-2142369787;

 //SceVoicePortType
 SCE_VOICE_PORTTYPE_NULL        =-1;
 SCE_VOICE_PORTTYPE_IN_DEVICE   =0;
 SCE_VOICE_PORTTYPE_IN_PCMAUDIO =1;
 SCE_VOICE_PORTTYPE_IN_VOICE    =2;
 SCE_VOICE_PORTTYPE_OUT_PCMAUDIO=3;
 SCE_VOICE_PORTTYPE_OUT_VOICE   =4;
 SCE_VOICE_PORTTYPE_OUT_DEVICE  =5;

type
 pSceVoiceInitParam=^SceVoiceInitParam;
 SceVoiceInitParam=packed record
  appType  :Integer;
  _align   :Integer;
  onEvent  :Pointer;
  pUserData:Pointer;
  reserved :array[0..11] of Byte;
 end;

 pSceVoiceStartParam=^SceVoiceStartParam;
 SceVoiceStartParam=packed record
  container:Pointer;
  memSize  :DWORD;
  reserved :array[0..19] of Byte;
 end;

 pSceVoicePortParam=^SceVoicePortParam;
 SceVoicePortParam=packed record
  //
 end;

 pSceVoiceBasePortInfo=^SceVoiceBasePortInfo;
 SceVoiceBasePortInfo=packed record
  portType :Integer; //SceVoicePortType
  state    :Integer; //SceVoicePortState
  pEdge    :PDWORD;
  numByte  :DWORD;
  frameSize:DWORD;
  numEdge  :WORD;
  reserved :WORD;
 end;

 pSceVoiceResourceInfo=^SceVoiceResourceInfo;
 SceVoiceResourceInfo=packed record
  maxInVoicePort  :Word;
  maxOutVoicePort :Word;
  maxInDevicePort :Word;
  maxOutDevicePort:Word;
 end;

implementation

function ps4_sceVoiceInit(pArg:pSceVoiceInitParam;
                          version:DWORD //SceVoiceVersion
                          ):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVoiceQoSInit(pMemBlock:Pointer;memSize:DWORD;appType:Integer):Integer; SysV_ABI_CDecl;
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

function ps4_sceVoiceDeletePort(portId:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVoiceConnectIPortToOPort(ips,ops:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVoiceDisconnectIPortFromOPort(ips,ops:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVoiceGetPortInfo(portId:DWORD;pInfo:pSceVoiceBasePortInfo):Integer; SysV_ABI_CDecl;
begin
 if (pInfo=nil) then Exit(SCE_VOICE_ERROR_ARGUMENT_INVALID);

 pInfo^.portType :=0;
 pInfo^.state    :=3;
 pInfo^.numByte  :=0;
 pInfo^.frameSize:=1;
 pInfo^.numEdge  :=0;
 pInfo^.reserved :=0;

 Result:=0;
end;

function ps4_sceVoiceGetResourceInfo(pInfo:pSceVoiceResourceInfo):Integer; SysV_ABI_CDecl;
begin
 if (pInfo=nil) then Exit(SCE_VOICE_ERROR_ARGUMENT_INVALID);

 pInfo^.maxInVoicePort  :=2;
 pInfo^.maxOutVoicePort :=4;
 pInfo^.maxInDevicePort :=0;
 pInfo^.maxOutDevicePort:=5;

 Result:=0;
end;

function ps4_sceVoiceReadFromOPort(ops:DWORD;data:Pointer;size:pDWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVoiceSetVolume(portId:DWORD;volume:Single):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVoiceSetMuteFlag(portId:DWORD;bMuted:Boolean):Integer; SysV_ABI_CDecl;
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
 lib^.set_proc($6FB90923E9F1DA18,@ps4_sceVoiceDeletePort);
 lib^.set_proc($A15F4601D276DC6C,@ps4_sceVoiceConnectIPortToOPort);
 lib^.set_proc($6A3563DD01B6BA6E,@ps4_sceVoiceDisconnectIPortFromOPort);
 lib^.set_proc($0AB2EA0F058BA173,@ps4_sceVoiceGetPortInfo);
 lib^.set_proc($67A415EA3EE282F1,@ps4_sceVoiceGetResourceInfo);
 lib^.set_proc($710E831AC4048D5E,@ps4_sceVoiceReadFromOPort);
 lib^.set_proc($4011680088C9A174,@ps4_sceVoiceSetVolume);
 lib^.set_proc($8305329E41203456,@ps4_sceVoiceSetMuteFlag);
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

