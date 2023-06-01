unit kern_gpo;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_get_gpo(pbits:PByte):Integer;
function sys_set_gpo(uiBits:DWORD):Integer;

implementation

uses
 errno;

function sys_get_gpo(pbits:PByte):Integer;
begin
 Exit(EPERM); //sceSblACMgrIsSystemUcred
end;

function sys_set_gpo(uiBits:DWORD):Integer;
begin
 Exit(0);
end;

end.

