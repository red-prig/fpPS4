unit ps4_libSceMouse;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

function ps4_sceMouseInit:Integer; SysV_ABI_CDecl;
begin
 Writeln('sceMouseInit');
 Result:=0;
end;

type
 pSceMouseOpenParam=^SceMouseOpenParam;
 SceMouseOpenParam=packed record
  behaviorFlag:Byte;
  reserved:array[0..6] of Byte;
 end;

const
 SCE_MOUSE_ERROR_INVALID_HANDLE=$80DF0003;

function ps4_sceMouseOpen(userId:Integer;
                          _type:Integer;
                          index:Integer;
                          pParam:pSceMouseOpenParam):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceMouseOpen');
 Result:=Integer(SCE_MOUSE_ERROR_INVALID_HANDLE);
end;

function Load_libSceMouse(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceMouse');
 lib^.set_proc($42CD305AE96097B5,@ps4_sceMouseInit);
 lib^.set_proc($45AAB16487FA0EF1,@ps4_sceMouseOpen);;
end;

initialization
 ps4_app.RegistredPreLoad('libSceMouse.prx',@Load_libSceMouse);

end.

