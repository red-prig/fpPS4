unit ps4_libSceNpTus;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program;

implementation

function ps4_sceNpTssCreateNpTitleCtx(serviceLabel:DWord;id:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
end;

function Load_libSceNpTus(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpTus');
 lib^.set_proc($B1155BD827F41878,@ps4_sceNpTssCreateNpTitleCtx);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpTus.prx',@Load_libSceNpTus);

end.

