unit ps4_libSceUsbStorage;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 Classes,
 SysUtils;

implementation

function ps4_sceUsbStorageInit():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceUsbStorage(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceUsbStorage');
 lib^.set_proc($0430D9C05E64B937,@ps4_sceUsbStorageInit);
end;

initialization
 ps4_app.RegistredPreLoad('libSceUsbStorage.prx' ,@Load_libSceUsbStorage);

end.

