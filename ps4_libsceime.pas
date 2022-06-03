unit ps4_libSceIme;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

function ps4_sceImeKeyboardOpen(
          userId:Integer;
          param:Pointer //SceImeKeyboardParam
          ):Integer; SysV_ABI_CDecl;
begin
 Result:=Integer($80BC0004);
end;

function ps4_sceImeUpdate(
          handler:Pointer //SceImeEventHandler
          ):Integer; SysV_ABI_CDecl;
begin
 Result:=Integer($80BC0004);
end;

function Load_libSceIme(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceIme');
 lib^.set_proc($79A1578DF26FDF1B,@ps4_sceImeKeyboardOpen);
 lib^.set_proc($FF81827D874D175B,@ps4_sceImeUpdate);
end;

initialization
 ps4_app.RegistredPreLoad('libSceIme.prx',@Load_libSceIme);

end.

