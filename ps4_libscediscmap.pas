unit ps4_libSceDiscMap;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

const
 SCE_DISC_MAP_ERROR_INVALID_ARGUMENT=-2129657855; //0x81100001

function ps4_sceDiscMapIsRequestOnHDD(param1:PChar;param2,param3:Int64;param4:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (param1=nil) or (param4=nil) then Exit(SCE_DISC_MAP_ERROR_INVALID_ARGUMENT);
 param4^:=1;
 Result:=0;
end;

function ps4_8A828CAEE7EDD5E9(param1:PChar;param2,param3:Int64;param4,param5,param6:PInt64):Integer; SysV_ABI_CDecl;
begin
 param4^:=0;
 param5^:=0;
 param6^:=0;
 Result:=0;
end;

function Load_libSceDiscMap(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceDiscMap');
 lib^.set_proc($95B40AAAC11186D1,@ps4_sceDiscMapIsRequestOnHDD);
 lib^.set_proc($8A828CAEE7EDD5E9,@ps4_8A828CAEE7EDD5E9);
end;

initialization
 ps4_app.RegistredPreLoad('libSceDiscMap.prx',@Load_libSceDiscMap);

end.

