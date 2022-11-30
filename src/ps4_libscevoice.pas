unit ps4_libSceVoice;

{$mode ObjFPC}{$H+}

interface


uses
  ps4_program,
  Classes,
  SysUtils;

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

function Load_libSceVoice(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceVoice');
 lib^.set_proc($F53AE1B86CDB7AB4,@ps4_sceVoiceInit);
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

