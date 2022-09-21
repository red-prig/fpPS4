unit ps4_libSceDiscMap;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

const
 SCE_DISC_MAP_ERROR_INVALID_ARGUMENT   =-2129657855; //0x81100001
 SCE_DISC_MAP_ERROR_LOCATION_NOT_MAPPED=-2129657854; //0x81100002
 SCE_DISC_MAP_ERROR_FILE_NOT_FOUND     =-2129657853; //0x81100003
 SCE_DISC_MAP_ERROR_NO_BITMAP_INFO     =-2129657852; //0x81100004
 SCE_DISC_MAP_ERROR_FATAL              =-2129657601; //0x811000FF

 DM_PATCH_FLAG=$1;
 DM_APP1_FLAG =$100;

function ps4_sceDiscMapIsRequestOnHDD(path:PChar;param2,param3:Int64;pret:PInteger):Integer; SysV_ABI_CDecl;
begin
 //pret^:=1;
 Result:=SCE_DISC_MAP_ERROR_NO_BITMAP_INFO;
end;

function ps4_ioKMruft1ek(path:PChar;param2,param3:Int64;pflags,pret1,pret2:PInt64):Integer; SysV_ABI_CDecl;
begin
 //pflags^:=0;
 //pret1^:=0;
 //pret2^:=0;
 Result:=SCE_DISC_MAP_ERROR_NO_BITMAP_INFO;
end;

function ps4_fJgP_wqifno(path:PChar;param2,param3:Int64;pflags,pret1,pret2:PInt64):Integer; SysV_ABI_CDecl;
begin
 if (param3<=0) or
    (param2<=-2) or
    (path=nil) or
    (pflags=nil) or
    (pret1=nil) or
    (pret2=nil) then Exit(SCE_DISC_MAP_ERROR_INVALID_ARGUMENT);

 pflags^:=0;
 pret1^:=0;
 pret2^:=0;
 Result:=0;
end;

function ps4_sceDiscMapGetPackageSize(fflags:Int64;pret1,pret2:PInt64):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_DISC_MAP_ERROR_NO_BITMAP_INFO;
end;

function Load_libSceDiscMap(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceDiscMap');
 lib^.set_proc($95B40AAAC11186D1,@ps4_sceDiscMapIsRequestOnHDD);
 lib^.set_proc($8A828CAEE7EDD5E9,@ps4_ioKMruft1ek);
 lib^.set_proc($7C980FFB0AA27E7A,@ps4_fJgP_wqifno);
 lib^.set_proc($7E5D5EA039F0438B,@ps4_sceDiscMapGetPackageSize);
end;

initialization
 ps4_app.RegistredPreLoad('libSceDiscMap.prx',@Load_libSceDiscMap);

end.

