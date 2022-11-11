unit sys_file;

{$mode ObjFPC}{$H+}

interface

uses
 Windows,
 Classes,
 SysUtils,
 RWLock,
 sys_kernel,
 sys_time,
 sys_fd;

function _sys_file_open(const path:RawByteString;flags,mode:Integer):Integer;

implementation

type
 TFile=class(TCustomFile)
  var
   lock:TRWLock;
  Constructor Create;
  Destructor  Destroy; override;
  function lseek (offset:Int64;whence:Integer):Int64;    override;
  function read  (data:Pointer;size:Int64):Int64;        override;
  function pread (data:Pointer;size,offset:Int64):Int64; override;
  function readv (vector:p_iovec;count:Integer):Int64;   override;
  function write (data:Pointer;size:Int64):Int64;        override;
  function pwrite(data:Pointer;size,offset:Int64):Int64; override;
  function fstat (stat:PSceKernelStat):Integer;          override;
 end;

Function get_DesiredAccess(flags:Integer):DWORD;
begin
 Result:=0;
 if (flags and SCE_KERNEL_O_RDWR)<>0 then
 begin
  Result:=GENERIC_READ or GENERIC_WRITE;
 end else
 if (flags and SCE_KERNEL_O_WRONLY)<>0 then
 begin
  Result:=GENERIC_WRITE;
 end else
 begin
  Result:=GENERIC_READ;
 end;

 if (flags and SCE_KERNEL_O_APPEND)<>0 then
 begin
  Result:=Result or FILE_APPEND_DATA;
 end;
end;

Function get_CreationDisposition(flags:Integer):DWORD;
const
 CREAT_EXCL=SCE_KERNEL_O_CREAT or SCE_KERNEL_O_EXCL;
begin
 Result:=0;
 if (flags and CREAT_EXCL)=CREAT_EXCL then
 begin
  Result:=CREATE_NEW;
 end else
 if (flags and SCE_KERNEL_O_CREAT)<>0 then
 begin
  Result:=CREATE_ALWAYS;
 end else
 if (flags and SCE_KERNEL_O_TRUNC)<>0 then
 begin
  Result:=TRUNCATE_EXISTING;
 end else
 begin
  Result:=OPEN_EXISTING;
 end;
end;

function _sys_file_open(const path:RawByteString;flags,mode:Integer):Integer;
var
 f:TFile;
 h:THandle;

 err:DWORD;
 dwDesiredAccess:DWORD;
 dwCreationDisposition:DWORD;

 wp:WideString;
begin
 Result:=0;

 wp:=UTF8Decode(path);

 dwDesiredAccess:=get_DesiredAccess(flags);
 dwCreationDisposition:=get_CreationDisposition(flags);

 h:=CreateFileW(
  PWideChar(wp),
  dwDesiredAccess,
  FILE_SHARE_READ,
  nil,
  dwCreationDisposition,
  FILE_ATTRIBUTE_NORMAL,
  0
 );

 if (h=INVALID_HANDLE_VALUE) then
 begin
  err:=GetLastError;
  //Writeln('GetLastError:',err{,' ',ps4_pthread_self^.sig._lock});
  Case err of
   ERROR_INVALID_DRIVE,
   ERROR_PATH_NOT_FOUND,
   ERROR_FILE_NOT_FOUND   :Exit(-ENOENT);
   ERROR_ACCESS_DENIED    :Exit(-EACCES);
   ERROR_BUFFER_OVERFLOW  :Exit(-ENAMETOOLONG);
   ERROR_NOT_ENOUGH_MEMORY:Exit(-ENOMEM);
   ERROR_ALREADY_EXISTS   :Exit(-EEXIST);
   ERROR_FILE_EXISTS      :Exit(-EEXIST);
   ERROR_DISK_FULL        :Exit(-ENOSPC);
   else
                           Exit(-EIO);
  end;
 end;

 f:=TFile.Create;
 f.Handle:=h;

 Result:=_sys_open_fd(f);

 if (Result<0) then
 begin
  f.Release;
 end else
 begin
  f.Destroy;
 end;
end;

//

Constructor TFile.Create;
begin
 rwlock_init(lock);
end;

Destructor TFile.Destroy;
begin
 rwlock_destroy(lock);
 CloseHandle(Handle);
end;

function SetFilePointerEx(hFile:HANDLE;
                          liDistanceToMove:LARGE_INTEGER;
                          lpNewFilePointer:PLARGE_INTEGER;
                          dwMoveMethod:DWORD):BOOL; external 'kernel32';

function TFile.lseek (offset:Int64;whence:Integer):Int64;
var
 err:DWORD;
 R:BOOL;
begin
 Result:=0;

 rwlock_wrlock(lock);
  R:=SetFilePointerEx(Handle,LARGE_INTEGER(offset),@Result,whence);
 rwlock_unlock(lock);

 if not R then
 begin
  err:=GetLastError;
  Case err of
   ERROR_HANDLE_EOF       :Exit(-EOVERFLOW);
   ERROR_INVALID_PARAMETER:Exit(-EINVAL);
   else
                           Exit(-EOVERFLOW);
  end;
 end;
end;

function TFile.read  (data:Pointer;size:Int64):Int64;
var
 N:DWORD;
 R:BOOL;
begin
 Assert(size<High(DWORD));

 N:=0;
 rwlock_wrlock(lock);
  R:=ReadFile(Handle,data^,size,N,nil);
 rwlock_unlock(lock);

 if R then
 begin
  Result:=N;
 end else
 begin
  Result:=-EIO;
 end;
end;

function _get_pos(h:THandle):Int64;
const
 zero:LARGE_INTEGER=(QuadPart:0);
begin
 Result:=-1;
 if not SetFilePointerEx(h,zero,@Result,FILE_CURRENT) then
 begin
  Result:=-1;
 end;
end;

procedure _set_pos(h:THandle;p:Int64);
begin
 SetFilePointerEx(h,LARGE_INTEGER(p),nil,FILE_BEGIN);
end;

function TFile.pread (data:Pointer;size,offset:Int64):Int64;
var
 N:DWORD;
 O:TOVERLAPPED;
 p:Int64;
 R:BOOL;
begin
 Assert(size<High(DWORD));

 O:=Default(TOVERLAPPED);
 PInt64(@O.Offset)^:=offset;
 N:=0;
 rwlock_wrlock(lock);
  //NOTE: pread and pwrite don't change the file position, but ReadFile/WriteFile do, damn it.
  p:=_get_pos(Handle);
  R:=ReadFile(Handle,data^,size,N,@O);
  _set_pos(Handle,p);
 rwlock_unlock(lock);

 if R then
 begin
  Result:=N;
 end else
 begin
  Result:=-EIO;
 end;
end;

function TFile.readv (vector:p_iovec;count:Integer):Int64;
var
 N:DWORD;
 R:BOOL;
 i:Integer;
begin
 Result:=0;

 For i:=0 to count-1 do
 begin
  Assert(vector[i].iov_len<High(DWORD));

  N:=0;
  rwlock_wrlock(lock);
   R:=ReadFile(Handle,vector[i].iov_base^,vector[i].iov_len,N,nil);
  rwlock_unlock(lock);

  if R then
  begin
   Result:=Result+N;
   if (N<vector[i].iov_len) then Exit;
  end else
  begin
   Exit(-EIO);
   Break;
  end;

 end;

end;

function TFile.write (data:Pointer;size:Int64):Int64;
var
 N:DWORD;
 R:BOOL;
begin
 Assert(size<High(DWORD));

 N:=0;
 rwlock_wrlock(lock);
  R:=WriteFile(Handle,data^,size,N,nil);
 rwlock_unlock(lock);

 if R then
 begin
  Result:=N;
 end else
 begin
  Result:=-EIO;
 end;
end;

function TFile.pwrite(data:Pointer;size,offset:Int64):Int64;
var
 N:DWORD;
 O:TOVERLAPPED;
 p:Int64;
 R:BOOL;
begin
 Assert(size<High(DWORD));

 O:=Default(TOVERLAPPED);
 PInt64(@O.Offset)^:=offset;
 N:=0;
 rwlock_wrlock(lock);
  //NOTE: pread and pwrite don't change the file position, but ReadFile/WriteFile do, damn it.
  p:=_get_pos(Handle);
  R:=WriteFile(Handle,data^,size,N,@O);
  _set_pos(Handle,p);
 rwlock_unlock(lock);

 if R then
 begin
  Result:=N;
 end else
 begin
  Result:=-EIO;
 end;
end;

function file_attr_to_st_mode(attr:DWORD):Word;
begin
 Result:=S_IRUSR;
 if ((attr and FILE_ATTRIBUTE_DIRECTORY)<>0) then
  Result:=Result or S_IFDIR
 else
  Result:=Result or S_IFREG;

 if ((attr and FILE_ATTRIBUTE_READONLY)=0) then
  Result:=Result or S_IWUSR;
end;

function TFile.fstat (stat:PSceKernelStat):Integer;
var
 hfi:TByHandleFileInformation;
 err:DWORD;
begin
 stat^:=Default(SceKernelStat);

 Case SwGetFileType(Handle) of
  FILE_TYPE_PIPE:
    begin
     stat^.st_dev  :=fd;
     stat^.st_rdev :=fd;
     stat^.st_mode :=S_IFIFO;
     stat^.st_nlink:=1;
    end;
  FILE_TYPE_CHAR:
    begin
     stat^.st_dev  :=fd;
     stat^.st_rdev :=fd;
     stat^.st_mode :=S_IFCHR;
     stat^.st_nlink:=1;
    end;
  FILE_TYPE_DISK:
    begin
     err:=SwGetFileInformationByHandle(Handle,@hfi);
     if (err<>0) then
     begin
      Case err of
       ERROR_ACCESS_DENIED,
       ERROR_SHARING_VIOLATION,
       ERROR_LOCK_VIOLATION,
       ERROR_SHARING_BUFFER_EXCEEDED:
         Exit(EACCES);

       ERROR_BUFFER_OVERFLOW:
         Exit(ENAMETOOLONG);

       ERROR_NOT_ENOUGH_MEMORY:
         Exit(ENOMEM);

       else
         Exit(ENOENT);
      end;
     end;

     stat^.st_dev     :=fd;
     stat^.st_rdev    :=fd;

     stat^.st_mode    :=file_attr_to_st_mode(hfi.dwFileAttributes);
     stat^.st_size    :=hfi.nFileSizeLow or (QWORD(hfi.nFileSizeHigh) shl 32);
     stat^.st_nlink   :=Word(hfi.nNumberOfLinks);
     stat^.st_gen     :=hfi.nFileIndexLow;

     stat^.st_atim    :=filetime_to_timespec(hfi.ftLastAccessTime);
     stat^.st_mtim    :=filetime_to_timespec(hfi.ftLastWriteTime);
     stat^.st_ctim    :=stat^.st_mtim;
     stat^.st_birthtim:=filetime_to_timespec(hfi.ftCreationTime);

     stat^.st_blocks  :=((stat^.st_size+511) div 512);
     stat^.st_blksize :=512;
    end;

  else
   Exit(EBADF);
 end;

 Result:=0;
end;

//

end.



