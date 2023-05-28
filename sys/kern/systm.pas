unit systm;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi;

const
 IOSIZE_MAX      =High(Integer);
 DEVFS_IOSIZE_MAX=High(Integer);

function copystr(from,_to:Pointer;maxlen:ptruint;lencopied:pptruint):Integer;
function copyin(udaddr,kaddr:Pointer;len:ptruint):Integer; inline;
function copyinstr(udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
function copyout(kaddr,udaddr:Pointer;len:ptruint):Integer; inline;
function fubyte(var base:Byte):Byte; inline;
function fuword32(var base:DWORD):DWORD; inline;
function fuword64(var base:QWORD):QWORD; inline;
function casuword32(var base:DWORD;oldval,newval:DWORD):DWORD; inline;
function casuword64(var base:QWORD;oldval,newval:QWORD):QWORD; inline;
function suword32(var base:DWORD;word:DWORD):DWORD; inline;
function suword64(var base:QWORD;word:QWORD):DWORD; inline;

implementation

uses
 sysutils,
 errno;

function copystr(from,_to:Pointer;maxlen:ptruint;lencopied:pptruint):Integer;
begin
 strlcopy(_to,from,maxlen);
 if (lencopied<>nil) then
 begin
  lencopied^:=strlen(_to);
 end;
 Result:=0;
end;

function copyin(udaddr,kaddr:Pointer;len:ptruint):Integer; inline;
begin
 if (NtReadVirtualMemory(NtCurrentProcess,udaddr,kaddr,len,nil)=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=EFAULT;
 end;
end;

function copyinstr(udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
var
 ch:Char;
 i:ptruint;
begin
 Result:=0;
 i:=0;
 ch:=#0;
 while (len>0) do
 begin
  Result:=copyin(udaddr,@ch,1);
  if (Result<>0) then Break;
  PChar(kaddr)^:=ch;
  Inc(i);
  if (ch=#0) then Break;
  Inc(udaddr);
  Inc(kaddr);
  Dec(len);
 end;
 if (lencopied<>nil) then
 begin
  lencopied^:=i;
 end;
end;

function copyout(kaddr,udaddr:Pointer;len:ptruint):Integer; inline;
begin
 if (NtWriteVirtualMemory(NtCurrentProcess,udaddr,kaddr,len,nil)=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=EFAULT;
 end;
end;

function fubyte(var base:Byte):Byte; inline;
begin
 if (NtReadVirtualMemory(NtCurrentProcess,@base,@Result,SizeOf(Byte),nil)<>0) then
 begin
  Result:=BYTE(-1);
 end;
end;

function fuword32(var base:DWORD):DWORD; inline;
begin
 if (NtReadVirtualMemory(NtCurrentProcess,@base,@Result,SizeOf(DWORD),nil)<>0) then
 begin
  Result:=DWORD(-1);
 end;
end;

function fuword64(var base:QWORD):QWORD; inline;
begin
 if (NtReadVirtualMemory(NtCurrentProcess,@base,@Result,SizeOf(QWORD),nil)<>0) then
 begin
  Result:=QWORD(-1);
 end;
end;

function casuword32(var base:DWORD;oldval,newval:DWORD):DWORD; inline;
begin
 Result:=System.InterlockedCompareExchange(base,newval,oldval);
end;

function casuword64(var base:QWORD;oldval,newval:QWORD):QWORD; inline;
begin
 Result:=System.InterlockedCompareExchange64(base,newval,oldval);
end;

function suword32(var base:DWORD;word:DWORD):DWORD; inline;
begin
 if (NtWriteVirtualMemory(NtCurrentProcess,@base,@word,SizeOf(DWORD),nil)=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=DWORD(-1);
 end;
end;

function suword64(var base:QWORD;word:QWORD):DWORD; inline;
begin
 if (NtWriteVirtualMemory(NtCurrentProcess,@base,@word,SizeOf(QWORD),nil)=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=DWORD(-1);
 end;
end;

end.

