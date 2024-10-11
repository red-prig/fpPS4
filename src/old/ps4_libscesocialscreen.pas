unit ps4_libSceSocialScreen;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program;

implementation

function ps4_sceSocialScreenInitialize():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceSocialScreen(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceSocialScreen');
 lib^.set_proc($A48EE81523CFEB90,@ps4_sceSocialScreenInitialize);
end;

initialization
 ps4_app.RegistredPreLoad('libSceSocialScreen.prx',@Load_libSceSocialScreen);

end.

