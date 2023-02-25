unit kern_mtx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 windows,
 ntapi;

function mtx_init(m:PPointer):Integer;
function mtx_destroy(m:PPointer):Integer;
function mtx_lock(m:PPointer):Integer;
function mtx_unlock(m:PPointer):Integer;

implementation

function mtx_init(m:PPointer):Integer;
begin
 Result:=NtCreateMutant(
           PHandle(m),
           MUTANT_ALL_ACCESS,
           nil,
           False);
end;

function mtx_destroy(m:PPointer):Integer;
begin
 Result:=NtClose(THandle(m^));
end;

function mtx_lock(m:PPointer):Integer;
begin
 Result:=NtWaitForSingleObject(THandle(m^),False,nil);
end;

function mtx_unlock(m:PPointer):Integer;
begin
 Result:=NtReleaseMutant(THandle(m^),nil);
end;


end.

