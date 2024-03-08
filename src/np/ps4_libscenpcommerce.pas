unit ps4_libSceNpCommerce;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 Classes,
 SysUtils;

implementation

function ps4_sceNpCommerceDialogGetStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpCommerceDialogUpdateStatus():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceNpCommerce(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpCommerce');
 lib^.set_proc($0826C2FA5AAABC5D,@ps4_sceNpCommerceDialogGetStatus);
 lib^.set_proc($2D1E5CC0530C0951,@ps4_sceNpCommerceDialogUpdateStatus);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpCommerce.prx',@Load_libSceNpCommerce);

end.

