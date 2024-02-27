unit ps4_libSceConvertKeycode;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  ps4_libSceIme,
  Classes,
  SysUtils;

implementation

function ps4_sceConvertKeycodeGetVirtualKeycode(keycode:Word;
                                                keyboardType:Integer;
                                                vkeycode:PWord):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceConvertKeycode(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceConvertKeycode');
 lib^.set_proc($BC8B2826C2EFBE53,@ps4_sceConvertKeycodeGetVirtualKeycode);
end;

initialization
 //low priority
 ps4_app.RegistredFinLoad('libSceConvertKeycode.prx',@Load_libSceConvertKeycode);

end.

