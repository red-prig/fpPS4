unit ps4_libSceAudiodecCpu;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program;

implementation

type
 PSceAudiodecCpuCtrl=Pointer;
 PSceAudiodecCpuResource=Pointer;

function ps4_sceAudiodecCpuQueryMemSize(pCtrl      :PSceAudiodecCpuCtrl;
                                        pRes       :PSceAudiodecCpuResource;
                                        uiCodecType:DWord):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceAudiodecCpuQueryMemSize,uiCodecType=',uiCodecType);
 Result:=0;
end;

function Load_libSceAudiodecCpu(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceAudiodecCpu');

 lib^.set_proc($92D0F6C370F81B65,@ps4_sceAudiodecCpuQueryMemSize);
end;

initialization
 ps4_app.RegistredPreLoad('libSceAudiodecCpu.prx',@Load_libSceAudiodecCpu);

end.

