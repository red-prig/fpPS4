unit ps4_libSceZlib;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

function ps4_sceZlibInitialize(const buffer:Pointer;length:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceZlib(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceZlib');
 lib^.set_proc($9B5604ADD2170A9E,@ps4_sceZlibInitialize);
end;

initialization
 ps4_app.RegistredPreLoad('libSceZlib.prx',@Load_libSceZlib);

end.

