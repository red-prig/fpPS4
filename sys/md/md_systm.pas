unit md_systm;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 windows,
 ntapi;

function  md_copyin (udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint;hProcess:THandle=NtCurrentProcess):Integer;
function  md_copyout(kaddr,udaddr:Pointer;len:ptruint;lencopied:pptruint;hProcess:THandle=NtCurrentProcess):Integer;

function  md_fuword(var base:Pointer):Pointer;

function  md_pidfd_getfd(pidfd,targetfd:THandle):THandle;
function  md_dup_to_pidfd(pidfd,targetfd:THandle):THandle;
function  md_pidfd_open (pid:DWORD):THandle;

implementation

uses
 errno;

function md_copyin(udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint;hProcess:THandle=NtCurrentProcess):Integer;
var
 num:DWORD;
begin
 num:=0;
 if (NtReadVirtualMemory(hProcess,udaddr,kaddr,len,@num)=0) then
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

function md_copyout(kaddr,udaddr:Pointer;len:ptruint;lencopied:pptruint;hProcess:THandle=NtCurrentProcess):Integer;
var
 num:DWORD;
begin
 num:=0;
 if (NtWriteVirtualMemory(hProcess,udaddr,kaddr,len,@num)=0) then
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

function md_fuword(var base:Pointer):Pointer;
begin
 if (md_copyin(@base,@Result,SizeOf(base),nil)<>0) then
 begin
  Result:=Pointer(-1);
 end;
end;

///

function md_pidfd_getfd(pidfd,targetfd:THandle):THandle;
begin
 Result:=0;
 NtDuplicateObject(
  pidfd,
  targetfd,
  NtCurrentProcess,
  @Result,
  0,
  0,
  DUPLICATE_SAME_ACCESS
 );
end;

function md_dup_to_pidfd(pidfd,targetfd:THandle):THandle;
begin
 Result:=0;
 NtDuplicateObject(
  NtCurrentProcess,
  targetfd,
  pidfd,
  @Result,
  0,
  0,
  DUPLICATE_SAME_ACCESS
 );
end;

function md_pidfd_open(pid:DWORD):THandle;
var
 ClientId:TCLIENT_ID;
 OATTR:OBJECT_ATTRIBUTES;
begin
 Result:=0;

 ClientId.UniqueProcess:=pid;
 ClientId.UniqueThread :=0;

 OATTR:=Default(OBJECT_ATTRIBUTES);
 OATTR.Length:=SizeOf(OBJECT_ATTRIBUTES);

 NtOpenProcess(@Result,PROCESS_DUP_HANDLE,@OATTR,@ClientId);
end;

end.


