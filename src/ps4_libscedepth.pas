unit ps4_libSceDepth;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program;

implementation

type
 PSceDepthInitializeParameter=Pointer;
 PSceDepthQueryMemoryResult=Pointer;

function ps4_sceDepthQueryMemory(initParam:PSceDepthInitializeParameter;queryMem:PSceDepthQueryMemoryResult):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceDepthQueryMemory');
 Result:=0;
end;

function ps4_sceDepthInitialize(initParam:PSceDepthInitializeParameter;queryMem:PSceDepthQueryMemoryResult):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceDepthInitialize');
 Result:=0;
end;

function ps4_sceDepthEnableExtendedMode(param1:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceDepth(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceDepth');

 lib^.set_proc($E4CC31224DFF80BA,@ps4_sceDepthQueryMemory);
 lib^.set_proc($3DA6BD7601E71E94,@ps4_sceDepthInitialize);
 lib^.set_proc($CF9493F422201DAD,@ps4_sceDepthEnableExtendedMode);
end;

initialization
 ps4_app.RegistredPreLoad('libSceDepth.prx',@Load_libSceDepth);

end.


