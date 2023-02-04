unit ps4_libSceAudiodec;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program;

implementation

const
 SCE_AUDIODEC_TYPE_AT9=1;
 SCE_AUDIODEC_TYPE_MP3=2;
 SCE_AUDIODEC_TYPE_M4AAC=3;

 SCE_AUDIODEC_ERROR_INVALID_TYPE=$807F0001;

function ps4_sceAudiodecInitLibrary(codecType:DWord):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceAudiodecInitLibrary,codecType=',codecType);
 if not (codecType in [SCE_AUDIODEC_TYPE_AT9..SCE_AUDIODEC_TYPE_M4AAC]) then
  Result:=SCE_AUDIODEC_ERROR_INVALID_TYPE
 else
  Result:=0;
end;

function Load_libSceAudiodec(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceAudiodec');

 lib^.set_proc($56386C9B1A5C7B32,@ps4_sceAudiodecInitLibrary);
end;

initialization
 ps4_app.RegistredPreLoad('libSceAudiodec.prx' ,@Load_libSceAudiodec);

end.

