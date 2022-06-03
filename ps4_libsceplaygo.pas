unit ps4_libScePlayGo;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

function ps4_scePlayGoInitialize(
          initParam:Pointer //ScePlayGoInitParams
          ):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_scePlayGoOpen(
          outHandle:PInteger;
          param:Pointer
          ):Integer; SysV_ABI_CDecl;
begin
 Result:=Integer($80B2000E);
end;

function Load_libScePlayGo(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libScePlayGo');
 lib^.set_proc($B6CE8695938A46B1,@ps4_scePlayGoInitialize);
 lib^.set_proc($3351A66B5A1CAC61,@ps4_scePlayGoOpen);
end;

initialization
 ps4_app.RegistredPreLoad('libScePlayGo.prx',@Load_libScePlayGo);

end.

