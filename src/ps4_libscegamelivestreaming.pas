unit ps4_libSceGameLiveStreaming;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program;

implementation

function ps4_sceGameLiveStreamingInitialize(heapSize:qword):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceGameLiveStreamingInitialize:',heapSize);
 Result:=0;
end;

function ps4_sceGameLiveStreamingPermitServerSideRecording(isPermit:Boolean):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceGameLiveStreamingPermitServerSideRecording,isPermit=',isPermit);
 Result:=0;
end;

function ps4_sceGameLiveStreamingEnableLiveStreaming(isEnable:Boolean):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceGameLiveStreamingEnableLiveStreaming,isEnable=',isEnable);
 Result:=0;
end;

function ps4_sceGameLiveStreamingPermitLiveStreaming(isPermit:Boolean):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceGameLiveStreamingPermitLiveStreaming,isPermit=',isPermit);
 Result:=0;
end;

function Load_libSceGameLiveStreaming(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceGameLiveStreaming');

 lib^.set_proc($92F604C369419DD9,@ps4_sceGameLiveStreamingInitialize);
 lib^.set_proc($FC41E753AF201315,@ps4_sceGameLiveStreamingPermitServerSideRecording);
 lib^.set_proc($75633CD005F7F68E,@ps4_sceGameLiveStreamingEnableLiveStreaming);
 lib^.set_proc($2B443111B0FBABE7,@ps4_sceGameLiveStreamingPermitLiveStreaming);
end;

initialization
 ps4_app.RegistredPreLoad('libSceGameLiveStreaming.prx',@Load_libSceGameLiveStreaming);

end.

