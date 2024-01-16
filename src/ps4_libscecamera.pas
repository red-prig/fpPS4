unit ps4_libSceCamera;

{$mode ObjFPC}{$H+}

interface


uses
  ps4_program,
  Classes,
  SysUtils;

implementation


function ps4_sceCameraIsAttached(index:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceCameraOpen(userId:Integer;_type,index:Integer;pParam:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceCamera(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceCamera');
 lib^.set_proc($A7A9F73698B7618E,@ps4_sceCameraIsAttached);
 lib^.set_proc($0479FCDF1AC5F761,@ps4_sceCameraOpen);
end;

initialization
 ps4_app.RegistredPreLoad('libSceCamera.prx',@Load_libSceCamera);

end.

