unit sys_kernel;

{$mode ObjFPC}{$H+}

interface

Uses
 SysUtils,
 Windows;

{$I sce_errno.inc}
{$I errno.inc}

var
 SDK_VERSION:DWORD=0;

type
 SceKernelModule=Integer;

 PSceKernelLoadModuleOpt=^SceKernelLoadModuleOpt;
 SceKernelLoadModuleOpt=packed record
  size:size_t;
 end;

 atexit_func=function(param:Pointer):Integer;SysV_ABI_CDecl;
 TKernelAtexitFuncCount=function(handle:Integer):Integer;SysV_ABI_CDecl;
 TKernelAtexitReportFunc=procedure(handle:Integer);

function  px2sce(e:Integer):Integer;
function  sce2px(e:Integer):Integer;

function  _set_errno(r:Integer):Integer;
function  _set_sce_errno(r:Integer):Integer;
function  _error:Pointer;

function  SwFreeMem(p:pointer):ptruint;
function  SwAllocMem(Size:ptruint):pointer;

Procedure SwYieldExecution; inline;
function  SwDelayExecution(Alertable:Boolean;DelayInterval:PQWORD):DWORD;

function  SwWaitForSingleObject(
           ObjectHandle:THandle;
           TimeOut:PQWORD;
           Alertable:LONGBOOL):DWORD;

function  SwWaitFor(Handle:THandle;pTimeout:PQWORD):Integer; //pTimeout in ns

function  SwGetFileAttributes(Const lpFileName:RawByteString;lpFileInformation:LPVOID):DWORD;
function  SwGetFileType(hFile:HANDLE):DWORD;
function  SwGetFileInformationByHandle(hFile:HANDLE;lpFileInformation:LPBY_HANDLE_FILE_INFORMATION):DWORD;
Function  SwCreateDir(Const NewDir:RawByteString):DWORD;

Function  safe_move(const src;var dst;count:QWORD):QWORD;
procedure safe_move_ptr(const src;var dst);
function  safe_test(var src:DWORD;value:DWORD):Boolean;
function  safe_str(P:PChar):shortstring;

function MapViewOfFileEx(hFileMappingObject:HANDLE;
                         dwDesiredAccess:DWORD;
                         dwFileOffsetHigh:DWORD;
                         dwFileOffsetLow:DWORD;
                         dwNumberOfBytesToMap:SIZE_T;
                         lpBaseAddress:LPVOID):LPVOID; stdcall; external 'kernel32' name 'MapViewOfFileEx';

implementation

uses
 ntapi,
 sys_pthread,
 sys_signal,
 sys_time;

function px2sce(e:Integer):Integer;
begin
 if (e=0) then
  Result:=0
 else
  Result:=e-$7ffe0000;
end;

function sce2px(e:Integer):Integer;
begin
 if (e=0) then
  Result:=0
 else
  Result:=e+$7ffe0000;
end;

function _set_errno(r:Integer):Integer;
var
 t:pthread;
begin
 Result:=0;

 t:=tcb_thread;
 if (t<>nil) then t^.errno:=r;

 if (r<>0) then
 begin
  Result:=-1;
 end;
end;

function _set_sce_errno(r:Integer):Integer;
var
 t:pthread;
begin
 t:=tcb_thread;
 if (t<>nil) then t^.errno:=sce2px(r);

 Result:=r;
end;

function _error:Pointer;
var
 t:pthread;
begin
 Result:=nil;
 t:=tcb_thread;
 if (t<>nil) then Result:=@t^.errno;
end;

function SwFreeMem(p:pointer):ptruint;
begin
 _sig_lock;
 Result:=FreeMem(p);
 _sig_unlock;
end;

function SwAllocMem(Size:ptruint):pointer;
begin
 _sig_lock;
 Result:=AllocMem(Size);
 _sig_unlock;
end;

Procedure SwYieldExecution; inline;
begin
 _sig_lock;
 NtYieldExecution;
 _sig_unlock;
end;

function SwDelayExecution(Alertable:Boolean;DelayInterval:PQWORD):DWORD;
begin
 _sig_lock(ord(Alertable));
 Result:=NtDelayExecution(Alertable,Pointer(DelayInterval));
 _sig_unlock;
end;


function SwWaitForSingleObject(
          ObjectHandle:THandle;
          TimeOut:PQWORD;
          Alertable:LONGBOOL):DWORD;
begin
 _sig_lock(ord(Alertable));
 Result:=NtWaitForSingleObject(ObjectHandle,Alertable,Pointer(TimeOut));
 _sig_unlock;
end;

function SwWaitFor(Handle:THandle;pTimeout:PQWORD):Integer;
var
 timeout:Int64;
 passed :Int64;
 START:QWORD;
 QTIME:QWORD;
 res:DWORD;
begin
 Result:=0;

 if (pTimeout<>nil) then
 begin
  timeout:=(pTimeout^ div 100);
  SwSaveTime(START);
 end else
 begin
  timeout:=NT_INFINITE;
 end;

 repeat

  if (pTimeout<>nil) then
  begin
   if (timeout=0) then
   begin
    pTimeout^:=0;
    Result:=ETIMEDOUT;
    Break;
   end;

   SwSaveTime(QTIME);

   timeout:=-timeout;
   _sig_lock(SL_ALERTABLE);
   res:=NtWaitForSingleObject(Handle,True,@timeout);
   _sig_unlock;
   timeout:=-timeout;

   passed:=SwTimePassedUnits(QTIME);

   if (passed>=timeout) then
   begin
    timeout:=0;
   end else
   begin
    timeout:=timeout-passed;
   end;

  end else
  begin
   _sig_lock(SL_ALERTABLE);
   res:=NtWaitForSingleObject(Handle,True,@timeout);
   _sig_unlock;
  end;

  case res of
   STATUS_ALERTED,
   STATUS_USER_APC:
    begin
     //continue
    end;
   STATUS_TIMEOUT:
    begin
     if (pTimeout<>nil) then
     begin
      pTimeout^:=0;
     end;
     Result:=ETIMEDOUT;
     Break;
    end;
   STATUS_ABANDONED:
    begin
     if (pTimeout<>nil) then
     begin
      pTimeout^:=timeout*100;
     end;
     Result:=EPERM;
     Break;
    end;
   STATUS_SUCCESS:
    begin
     if (pTimeout<>nil) then
     begin
      pTimeout^:=timeout*100;
     end;
     Result:=0;
     Break;
    end;
   else
    begin
     if (pTimeout<>nil) then
     begin
      pTimeout^:=timeout*100;
     end;
     Result:=EINVAL;
     Break;
    end;
  end;

 until false;

end;

function SwGetFileAttributes(Const lpFileName:RawByteString;lpFileInformation:LPVOID):DWORD;
var
 wp:WideString;
begin
 Result:=0;
 _sig_lock;
 wp:=UTF8Decode(lpFileName);
 if not GetFileAttributesExW(PWideChar(wp),GetFileExInfoStandard,lpFileInformation) then
 begin
  Result:=GetLastError;
 end;
 _sig_unlock;
end;

function SwGetFileType(hFile:HANDLE):DWORD;
begin
 _sig_lock;
 Result:=GetFileType(hFile);
 _sig_unlock;
end;

function SwGetFileInformationByHandle(hFile:HANDLE;lpFileInformation:LPBY_HANDLE_FILE_INFORMATION):DWORD;
begin
 Result:=0;
 _sig_lock;
 if not GetFileInformationByHandle(hFile,lpFileInformation) then
 begin
  Result:=GetLastError;
 end;
 _sig_unlock;
end;

Function SwCreateDir(Const NewDir:RawByteString):DWORD;
begin
 Result:=0;
 _sig_lock;
 if not CreateDir(NewDir) then
 begin
  Result:=GetLastError;
 end;
 _sig_unlock;
end;

Function safe_move(const src;var dst;count:QWORD):QWORD;
begin
 _sig_lock;
 if not ReadProcessMemory(GetCurrentProcess,@src,@dst,count,Result) then Result:=0;
 _sig_unlock;
end;

procedure safe_move_ptr(const src;var dst);
begin
 if safe_move(src,dst,SizeOf(Pointer))<>SizeOf(Pointer) then Pointer(dst):=nil;
end;

function safe_test(var src:DWORD;value:DWORD):Boolean;
var
 t:DWORD;
begin
 Result:=False;
 t:=0;
 if (safe_move(src,t,SizeOf(DWORD))=SizeOf(DWORD)) then
 begin
  Result:=(t=value);
 end;
end;

function safe_str(P:PChar):shortstring;
var
 ch:Char;
begin
 Result:='';
 repeat
  ch:=#0;
  safe_move(P^,ch,SizeOf(Char));
  if (ch=#0) then Exit;
  Result:=Result+ch;
  if (Result[0]=#255) then Exit;
  Inc(P);
 until false;
end;

end.

