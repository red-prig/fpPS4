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

function  md_getppid:DWORD;
function  md_pidfd_getfd (pidfd,targetfd:THandle):THandle;
function  md_dup_to_pidfd(pidfd,targetfd:THandle):THandle;
function  md_pidfd_open  (pid:DWORD):THandle;
function  md_waitpidfd   (pidfd:THandle;status:PDWORD):Integer;
function  md_pidfd_close (pidfd:THandle):Integer;

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

function md_getppid:DWORD;
var
 data:array[0..SizeOf(PROCESS_BASIC_INFORMATION)-1+7] of Byte;
 p_info:PPROCESS_BASIC_INFORMATION;
 R:DWORD;
begin
 Result:=0;
 p_info:=Align(@data,8);

 R:=NtQueryInformationProcess(NtCurrentProcess,
                              ProcessBasicInformation,
                              p_info,
                              SizeOf(PROCESS_BASIC_INFORMATION),
                              nil);
 if (R=0) then
 begin
  Result:=p_info^.InheritedFromUPI;
 end;
end;

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

 NtOpenProcess(@Result,SYNCHRONIZE or PROCESS_DUP_HANDLE or PROCESS_QUERY_LIMITED_INFORMATION,@OATTR,@ClientId);
end;

function md_waitpidfd(pidfd:THandle;status:PDWORD):Integer;
begin
 Result:=NtWaitForSingleObject(pidfd,False,nil);

 if (Result=STATUS_WAIT_0) and (status<>nil) then
 begin
  GetExitCodeProcess(pidfd,status);
 end;
end;

function md_pidfd_close(pidfd:THandle):Integer;
begin
 Result:=NtClose(pidfd);
end;

end.


