unit ps4_libSceNpEntitlementAccess;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib;

implementation

function ps4_sceNpEntitlementAccessInitialize(initParam,bootParam:Pointer):Integer;
begin
 Result:=0;
end;

function Load_libSceNpEntitlementAccess(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpEntitlementAccess');

 lib:=Result^.add_lib('libSceNpEntitlementAccess');
 lib.set_proc($8CEF0333CA327A0A,@ps4_sceNpEntitlementAccessInitialize);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpEntitlementAccess.prx',@Load_libSceNpEntitlementAccess);

end.

