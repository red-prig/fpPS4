unit ps4_libSceSysCore;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program;

implementation

function ps4_sceApplicationInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceApplicationInitialize');
 Result:=0;
end;

function Load_libSceSysCore(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceSysCore');

 lib^.set_proc($5C5608B4EC52EABD,@ps4_sceApplicationInitialize);
end;

initialization
 ps4_app.RegistredPreLoad('libSceSysCore.prx',@Load_libSceSysCore);

end.

