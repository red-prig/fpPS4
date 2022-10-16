unit ps4_libSceIme;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

type
 pSceImeEvent=^SceImeEvent;
 SceImeEvent=packed record
  id:Integer; //SceImeEventId
  //param:SceImeEventParam;
 end;

 SceImeEventHandler=procedure(arg:Pointer;e:pSceImeEvent);

 pSceImeKeyboardParam=^SceImeKeyboardParam;
 SceImeKeyboardParam=packed record
  option:DWORD;
  reserved1:DWORD;
  arg:Pointer;
  handler:SceImeEventHandler;
  reserved2:QWORD;
 end;

function ps4_sceImeKeyboardOpen(
          userId:Integer;
          param:pSceImeKeyboardParam
          ):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceImeKeyboardOpen:',userId,' ',HexStr(param));
 Result:=Integer($80BC0004);
end;

function ps4_sceImeUpdate(
          handler:Pointer //SceImeEventHandler
          ):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceImeUpdate:',HexStr(handler));
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

