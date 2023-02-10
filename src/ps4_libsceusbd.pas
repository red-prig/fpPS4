unit ps4_libSceUsbd;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program;

implementation

const
 SCE_USBD_ERROR_NO_DEVICE=$80240004;

function ps4_sceUsbdInit:Integer; SysV_ABI_CDecl;
begin
 Writeln('sceUsbdInit');
 Result:=SCE_USBD_ERROR_NO_DEVICE;
end;

function Load_libSceUsbd(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceUsbd');

 lib^.set_proc($4CE860ECFEA44C7E,@ps4_sceUsbdInit);
end;

initialization
 ps4_app.RegistredPreLoad('libSceUsbd.prx',@Load_libSceUsbd);

end.
