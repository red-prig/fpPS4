unit ps4_libSceVrTracker;

{$mode ObjFPC}{$H+}

interface


uses
  ps4_program,
  Classes,
  SysUtils;

implementation

function ps4_sceVrTrackerQueryMemory(param,pResult:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVrTrackerInit(param:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVrTrackerTerm():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceVrTracker(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceVrTracker');
 lib^.set_proc($2BBCA162BB0804F7,@ps4_sceVrTrackerQueryMemory);
 lib^.set_proc($424465EE90114FD3,@ps4_sceVrTrackerInit);
 lib^.set_proc($201BF83F7AB5A50D,@ps4_sceVrTrackerTerm);
end;

initialization
 ps4_app.RegistredPreLoad('libSceVrTracker.prx',@Load_libSceVrTracker);

end.

