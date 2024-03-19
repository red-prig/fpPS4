unit ps4_libSceLncUtil;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 Classes,
 SysUtils;

implementation

function ps4_sceLncUtilInitialize():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceLncUtil(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceLncUtil');
 lib^.set_proc($7FF43C35DDF71417,@ps4_sceLncUtilInitialize);
end;

initialization
 ps4_app.RegistredPreLoad('libSceLncUtil.prx',@Load_libSceLncUtil);

end.

