unit ps4_libSceRtc;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

//TODO

//nop nid:libSceRtc:CCEF542F7A8820D4:sceRtcGetCurrentNetworkTick
//nop nid:libSceRtc:D7C076352D72F545:sceRtcGetCurrentTick

function Load_libSceRtc(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceRtc');
 //lib^.set_proc($79A1578DF26FDF1B,@ps4_sceImeKeyboardOpen);

end;

initialization
 //ps4_app.RegistredPreLoad('libSceRtc.prx',@Load_libSceRtc);

end.

