unit md_systm;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi;

function md_copyin (udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
function md_copyout(kaddr,udaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;

implementation

uses
 errno;

function md_copyin(udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
var
 num:DWORD;
begin
 num:=0;
 if (NtReadVirtualMemory(NtCurrentProcess,udaddr,kaddr,len,@num)=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=EFAULT;
 end;
 if (lencopied<>nil) then
 begin
  lencopied^:=num;
 end;
end;

function md_copyout(kaddr,udaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
var
 num:DWORD;
begin
 num:=0;
 if (NtWriteVirtualMemory(NtCurrentProcess,udaddr,kaddr,len,@num)=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=EFAULT;
 end;
 if (lencopied<>nil) then
 begin
  lencopied^:=num;
 end;
end;

end.


