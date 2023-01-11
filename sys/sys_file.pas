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
function _sys_file_stat(Const path:RawByteString;stat:PSceKernelStat):Integer;

implementation

type
 TFile=class(TCustomFile)
  var
   lock:TRWLock;
  Constructor Create;
  Destructor  Destroy; override;
  function    lseek    (offset:Int64;whence:Integer):Int64;               override;
  function    read     (data:Pointer;size:Int64):Int64;                   override;
  function    pread    (data:Pointer;size,offset:Int64):Int64;            override;
  function    readv    (vector:p_iovec;count:Integer):Int64;              override;
  function    preadv   (vector:p_iovec;count:Integer;offset:Int64):Int64; override;
  function    write    (data:Pointer;size:Int64):Int64;                   override;
  function    pwrite   (data:Pointer;size,offset:Int64):Int64;            override;
  function    ftruncate(size:Int64):Integer;                              override;
  function    fstat    (stat:PSceKernelStat):Integer;                     override;
  function    fsync    ():Integer;                                        override;
  function    fcntl    (cmd:Integer;param1:ptruint):Integer;              override;
 end;

Function get_DesiredAccess(flags:Integer):DWORD;
begin
 Result:=0;
 if (flags and O_RDWR)<>0 then
 begin
  Result:=GENERIC_READ or GENERIC_WRITE;
 end else
 if (flags and O_WRONLY)<>0 then
 begin
  Result:=GENERIC_WRITE;
 end else
 begin
  Result:=GENERIC_READ;
 end;

 if (flags and O_APPEND)<>0 then
 begin
  Result:=Result and (not GENERIC_WRITE);
  Result:=Result or FILE_APPEND_DATA;
 end;
end;

Function get_CreationDisposition(flags:Integer):DWORD;
const
 CREAT_EXCL=O_CREAT or O_EXCL;
begin
 Result:=0;
 if (flags and CREAT_EXCL)=CREAT_EXCL then
 begin
  Result:=CREATE_NEW;
 end else
 if (flags and O_CREAT)<>0 then
 begin
  Result:=CREATE_ALWAYS;
 end else
 if (flags and O_TRUNC)<>0 then
 begin
  Result:=TRUNCATE_EXISTING;
 end else
 begin
  Result:=OPEN_EXISTING;
 end;
end;

function __sys_file_open(const path:RawByteString;flags,mode:Integer;var f:TFile):Integer;
var
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
  FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
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
end;

function _sys_file_open(const path:RawByteString;flags,mode:Integer):Integer;
var
 f:TFile;
begin
 f:=nil;
 Result:=__sys_file_open(path,flags,mode,f);
 if (Result<>0) then Exit;

 Result:=_sys_open_fd(f,flags);

 if (Result<0) then
 begin
  f.Destroy;
 end else
 begin
  f.Release;
 end;
end;

//

function _sys_file_stat(Const path:RawByteString;stat:PSceKernelStat):Integer;
var
 f:TFile;
begin
 f:=nil;
 Result:=__sys_file_open(path,O_RDONLY,0,f);
 if (Result<>0) then Exit(-Result);

 Result:=f.fstat(stat);

 f.Destroy;
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

function _set_pos(h:THandle;p:Int64):Boolean;
begin
 Result:=SetFilePointerEx(h,LARGE_INTEGER(p),nil,FILE_BEGIN);
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
  if not _set_pos(Handle,p) then Assert(False);
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
label
 _exit;
var
 N:DWORD;
 R:BOOL;
 i:Integer;
begin
 Result:=0;

 rwlock_wrlock(lock);

  For i:=0 to count-1 do
  if (vector[i].iov_base<>nil) and (vector[i].iov_len<>0) then
  begin
   Assert(vector[i].iov_len<High(DWORD));

   N:=0;
   R:=ReadFile(Handle,vector[i].iov_base^,vector[i].iov_len,N,nil);

   if R then
   begin
    Result:=Result+N;
    if (N<vector[i].iov_len) then
    begin
     Goto _exit;
    end;
   end else
   begin
    Result:=-EIO;
    Goto _exit;
   end;

  end;

 _exit:
  rwlock_unlock(lock);
end;

function TFile.preadv(vector:p_iovec;count:Integer;offset:Int64):Int64;
label
 _exit;
var
 N:DWORD;
 R:BOOL;
 p:Int64;
 i:Integer;
begin
 Result:=0;

 rwlock_wrlock(lock);

  p:=_get_pos(Handle);
  if not _set_pos(Handle,offset) then Assert(False);

  For i:=0 to count-1 do
  if (vector[i].iov_base<>nil) and (vector[i].iov_len<>0) then
  begin
   Assert(vector[i].iov_len<High(DWORD));

   N:=0;
   R:=ReadFile(Handle,vector[i].iov_base^,vector[i].iov_len,N,nil);

   if R then
   begin
    Result:=Result+N;
    if (N<vector[i].iov_len) then
    begin
     Goto _exit;
    end;
   end else
   begin
    Result:=-EIO;
    Goto _exit;
   end;

  end;

 _exit:
  if not _set_pos(Handle,p) then Assert(False);
  rwlock_unlock(lock);
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
  if not _set_pos(Handle,p) then Assert(False);
 rwlock_unlock(lock);

 if R then
 begin
  Result:=N;
 end else
 begin
  Result:=-EIO;
 end;
end;

function TFile.ftruncate(size:Int64):Integer;
var
 p:Int64;
begin
 Result:=0;
 rwlock_wrlock(lock);
  p:=_get_pos(Handle);

  if _set_pos(Handle,size) then
  begin
   if not SetEndOfFile(Handle) then
   begin
    Result:=EIO;
   end;
  end else
  begin
   Result:=EINVAL;
  end;

  if not _set_pos(Handle,p) then Assert(False);
 rwlock_unlock(lock);
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

function TFile.fsync:Integer;
var
 err:DWORD;
begin
 if FlushFileBuffers(Handle) then
 begin
  Result:=0;
 end else
 begin
  err:=GetLastError;
  Case err of
   ERROR_INVALID_HANDLE:
     Exit(EINVAL);
   else
     Exit(EIO);
  end;
 end;
end;

function TFile.fcntl(cmd:Integer;param1:ptruint):Integer;
begin
 Case cmd of
  F_SETFL:
   begin
    if (Integer(param1) and O_NONBLOCK)<>(status and O_NONBLOCK) then
    begin
     Writeln(StdErr,'fcntl:O_NONBLOCK:TODO');
    end;
    if (Integer(param1) and O_APPEND)<>(status and O_APPEND) then
    begin
     Writeln(StdErr,'fcntl:O_APPEND:TODO');
    end;
   end;
  else;
 end;
 Result:=inherited;
end;

//

end.



