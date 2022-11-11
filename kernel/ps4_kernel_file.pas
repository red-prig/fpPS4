unit ps4_kernel_file;

{$mode objfpc}{$H+}

interface

uses
  windows,
  sys_types,
  sys_path,
  sys_fd,
  sys_file,
  sys_dev,
  sys_dir,
  Classes,
  SysUtils;

function ps4_open(path:PChar;flags,mode:Integer):Integer; SysV_ABI_CDecl;
function ps4_sceKernelOpen(path:PChar;flags,mode:Integer):Integer; SysV_ABI_CDecl;

function ps4_close(fd:Integer):Integer; SysV_ABI_CDecl;
function ps4_sceKernelClose(fd:Integer):Integer; SysV_ABI_CDecl;

function ps4_lseek(fd:Integer;offset:Int64;whence:Integer):Int64; SysV_ABI_CDecl;
function ps4_sceKernelLseek(fd:Integer;offset:Int64;whence:Integer):Int64; SysV_ABI_CDecl;

function ps4_read(fd:Integer;data:Pointer;size:Int64):Int64; SysV_ABI_CDecl;
function ps4_sceKernelRead(fd:Integer;buf:Pointer;nbytes:Int64):Int64; SysV_ABI_CDecl;

function ps4_pread(fd:Integer;data:Pointer;size,offset:Int64):Int64;  SysV_ABI_CDecl;
function ps4_sceKernelPread(fd:Integer;buf:Pointer;nbytes,offset:Int64):Int64; SysV_ABI_CDecl;

function ps4_readv(fd:Integer;vector:p_iovec;count:Integer):Int64; SysV_ABI_CDecl;
function ps4_sceKernelReadv(fd:Integer;iov:p_iovec;iovcnt:Integer):Int64; SysV_ABI_CDecl;

function ps4_write(fd:Integer;data:Pointer;size:Int64):Int64; SysV_ABI_CDecl;
function ps4_sceKernelWrite(fd:Integer;buf:Pointer;nbytes:Int64):Int64; SysV_ABI_CDecl;

function ps4_pwrite(fd:Integer;data:Pointer;size,offset:Int64):Int64;  SysV_ABI_CDecl;
function ps4_sceKernelPwrite(fd:Integer;buf:Pointer;nbytes,offset:Int64):Int64; SysV_ABI_CDecl;

function ps4_fstat(fd:Integer;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;
function ps4_sceKernelFstat(fd:Integer;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;

function ps4_stat(path:PChar;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;
function ps4_sceKernelStat(path:PChar;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;

function ps4_mkdir(path:PChar):Integer; SysV_ABI_CDecl;
function ps4_sceKernelMkdir(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;

function ps4_sceKernelCheckReachability(path:PChar):Integer; SysV_ABI_CDecl;

implementation

uses
 sys_kernel,
 sys_signal,
 sys_time;

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

var
 dev_random_nm:array[0..1] of PChar=('/dev/random','/dev/urandom');
 dev_random_fd:Integer=-1;

function _sys_open(path:PChar;flags,mode:Integer):Integer;
const
 WR_RDWR=O_WRONLY or O_RDWR;
 O_OFS=O_RDONLY or O_WRONLY or O_RDWR or O_APPEND;

var
 h:THandle;

 err:DWORD;
 dwDesiredAccess:DWORD;
 dwCreationDisposition:DWORD;

 rp:RawByteString;
 wp:WideString;
begin
 Result:=0;
 if (path=nil) then Exit(-EINVAL);

 Writeln('open:',path,' ',flags,' (',OctStr(mode,3),')');

 Assert((flags and O_DIRECTORY)=0,'folder open TODO');

 if ((flags and WR_RDWR)=WR_RDWR) then
 begin
  Exit(-EINVAL);
 end;

 if (path[0]=#0) then
 begin
  Exit(-ENOENT);
 end;

 if (CompareChar0(path^,dev_random_nm[0]^,Length(dev_random_nm[0]))=0) or
    (CompareChar0(path^,dev_random_nm[1]^,Length(dev_random_nm[1]))=0) then
 begin
  if (dev_random_fd<>-1) then
  begin
   Exit(dev_random_fd);
  end else
  begin
   h:=_get_osfhandle(0);

   Result:=_open_osfhandle(h,flags and O_OFS);

   if (Result<0) then
   begin
    Exit(-EMFILE);
   end else
   begin
    dev_random_fd:=Result;
    Exit;
   end;
  end;
 end;

 rp:='';
 Result:=parse_filename(path,rp);

 Case Result of
  PT_ROOT:Exit(-EACCES); //TODO
  PT_FILE:;
  PT_DEV :Exit(-EACCES); //TODO
  else
          Exit(-EACCES);
 end;

 wp:=UTF8Decode(rp);

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

 Result:=_open_osfhandle(h,flags and O_OFS);

 if (Result<0) then
 begin
  CloseHandle(h);
  Exit(-EMFILE);
 end;

end;

function ps4_open(path:PChar;flags,mode:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_open(path,flags,mode);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=_set_errno(-Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function ps4_sceKernelOpen(path:PChar;flags,mode:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_open(path,flags,mode);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=-Result;
  _set_errno(Result);
  Result:=px2sce(Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function _sys_close(fd:Integer):Integer;
begin
 if (dev_random_fd<>-1) and (dev_random_fd=fd) then
 begin
  Exit(0);
 end;

 Result:=_close(fd);

 if (Result<>0) then
 begin
  Result:=EBADF;
 end;
end;

function ps4_close(fd:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_close(fd));
 _sig_unlock;
end;

function ps4_sceKernelClose(fd:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_close(fd);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

function SetFilePointerEx(hFile:HANDLE;
                          liDistanceToMove:LARGE_INTEGER;
                          lpNewFilePointer:PLARGE_INTEGER;
                          dwMoveMethod:DWORD):BOOL; external 'kernel32';

function _sys_lseek(fd:Integer;offset:Int64;whence:Integer):Int64;
var
 h:THandle;
 err:DWORD;
begin
 Result:=0;
 if (fd<0) then Exit(-EINVAL);
 if (dev_random_fd=fd) then Exit(-ESPIPE);

 h:=_get_osfhandle(fd);

 if (h=INVALID_HANDLE_VALUE) then
 begin
  Exit(-EBADF);
 end;

 case whence of
  SEEK_SET,
  SEEK_CUR,
  SEEK_END:
    begin
     if not SetFilePointerEx(h,LARGE_INTEGER(offset),@Result,whence) then
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
  else
    Exit(-EINVAL);
 end;
end;

function ps4_lseek(fd:Integer;offset:Int64;whence:Integer):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_lseek(fd,offset,whence);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=_set_errno(-Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function ps4_sceKernelLseek(fd:Integer;offset:Int64;whence:Integer):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_lseek(fd,offset,whence);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=-Result;
  _set_errno(Result);
  Result:=px2sce(Result);
 end else
 begin
  _set_errno(0);
 end;
end;

const
 BCRYPT_USE_SYSTEM_PREFERRED_RNG=2;

function BCryptGenRandom(hAlgorithm:Pointer;
                         pbBuffer:PByte;
                         cbBuffer:DWORD;
                         dwFlags:DWORD):DWORD; stdcall; external 'Bcrypt';

function _sys_read(fd:Integer;data:Pointer;size:Int64):Int64;
var
 h:THandle;
 N:DWORD;
begin
 if (data=nil) then Exit(-EFAULT);
 if (fd<0) then Exit(-EINVAL);
 if (size<=0) then Exit(-EINVAL);

 Assert(size<High(DWORD));

 if (dev_random_fd=fd) then
 begin
  BCryptGenRandom(nil,data,size,BCRYPT_USE_SYSTEM_PREFERRED_RNG);
  Exit(size);
 end;

 h:=_get_osfhandle(fd);

 if (h=INVALID_HANDLE_VALUE) then
 begin
  Exit(-EBADF);
 end;

 N:=0;
 if ReadFile(h,data^,size,N,nil) then
 begin
  Result:=N;
 end else
 begin
  Result:=-EIO;
 end;
end;

function ps4_read(fd:Integer;data:Pointer;size:Int64):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_read(fd,data,size);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=_set_errno(-Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function ps4_sceKernelRead(fd:Integer;buf:Pointer;nbytes:Int64):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_read(fd,buf,nbytes);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=-Result;
  _set_errno(Result);
  Result:=px2sce(Result);
 end else
 begin
  _set_errno(0);
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

function _sys_pread(fd:Integer;data:Pointer;size,offset:Int64):Int64;
var
 h:THandle;
 N:DWORD;
 O:TOVERLAPPED;
 p:Int64;
begin
 if (data=nil) then Exit(-EFAULT);
 if (fd<0) then Exit(-EINVAL);
 if (size<=0) then Exit(-EINVAL);
 if (offset<0) then Exit(-EINVAL);

 Assert(size<High(DWORD));

 if (dev_random_fd=fd) then
 begin
  BCryptGenRandom(nil,data,size,BCRYPT_USE_SYSTEM_PREFERRED_RNG);
  Exit(size);
 end;

 h:=_get_osfhandle(fd);

 if (h=INVALID_HANDLE_VALUE) then
 begin
  Exit(-EBADF);
 end;

 O:=Default(TOVERLAPPED);
 PInt64(@O.Offset)^:=offset;

 //NOTE: pread and pwrite don't change the file position, but ReadFile/WriteFile do, damn it.
 p:=_get_pos(h);

 N:=0;
 if ReadFile(h,data^,size,N,@O) then
 begin
  Result:=N;
 end else
 begin
  Result:=-EIO;
 end;

 _set_pos(h,p);
end;

function ps4_pread(fd:Integer;data:Pointer;size,offset:Int64):Int64;  SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_pread(fd,data,size,offset);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=_set_errno(-Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function ps4_sceKernelPread(fd:Integer;buf:Pointer;nbytes,offset:Int64):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_pread(fd,buf,nbytes,offset);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=-Result;
  _set_errno(Result);
  Result:=px2sce(Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function _sys_readv(fd:Integer;vector:p_iovec;count:Integer):Int64;
var
 h:THandle;
 N:DWORD;
 i:Integer;
begin
 if (vector=nil) then Exit(-EFAULT);
 if (fd<0) then Exit(-EINVAL);
 if (count<=0) then Exit(-EINVAL);

 For i:=0 to count-1 do
 begin
  if (vector[i].iov_base=nil) then Exit(-EFAULT);
  if (vector[i].iov_len<=0)   then Exit(-EINVAL);
 end;

 if (dev_random_fd=fd) then
 begin

  Result:=0;
  For i:=0 to count-1 do
  begin
   BCryptGenRandom(nil,vector[i].iov_base,vector[i].iov_len,BCRYPT_USE_SYSTEM_PREFERRED_RNG);
   Result:=Result+vector[i].iov_len;
  end;

  Exit;
 end;

 h:=_get_osfhandle(fd);

 if (h=INVALID_HANDLE_VALUE) then
 begin
  Exit(-EBADF);
 end;

 Result:=0;
 For i:=0 to count-1 do
 begin
  N:=0;
  if ReadFile(h,vector[i].iov_base^,vector[i].iov_len,N,nil) then
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

function ps4_readv(fd:Integer;vector:p_iovec;count:Integer):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_readv(fd,vector,count);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=_set_errno(-Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function ps4_sceKernelReadv(fd:Integer;iov:p_iovec;iovcnt:Integer):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_readv(fd,iov,iovcnt);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=-Result;
  _set_errno(Result);
  Result:=px2sce(Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function _sys_write(fd:Integer;data:Pointer;size:Int64):Int64;
var
 h:THandle;
 N:DWORD;
begin
 if (data=nil) then Exit(-EFAULT);
 if (fd<0) then Exit(-EINVAL);
 if (size<=0) then Exit(-EINVAL);

 Assert(size<High(DWORD));

 if (dev_random_fd=fd) then Exit(-EPIPE);

 h:=_get_osfhandle(fd);

 if (h=INVALID_HANDLE_VALUE) then
 begin
  Exit(-EBADF);
 end;

 N:=0;
 if WriteFile(h,data^,size,N,nil) then
 begin
  Result:=N;
 end else
 begin
  Result:=-EIO;
 end;
end;

function ps4_write(fd:Integer;data:Pointer;size:Int64):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_write(fd,data,size);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=_set_errno(-Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function ps4_sceKernelWrite(fd:Integer;buf:Pointer;nbytes:Int64):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_write(fd,buf,nbytes);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=-Result;
  _set_errno(Result);
  Result:=px2sce(Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function _sys_pwrite(fd:Integer;data:Pointer;size,offset:Int64):Int64;
var
 h:THandle;
 N:DWORD;
 O:TOVERLAPPED;
 p:Int64;
begin
 if (data=nil) then Exit(-EFAULT);
 if (fd<0) then Exit(-EINVAL);
 if (size<=0) then Exit(-EINVAL);
 if (offset<0) then Exit(-EINVAL);

 Assert(size<High(DWORD));

 if (dev_random_fd=fd) then Exit(-EPIPE);

 h:=_get_osfhandle(fd);

 if (h=INVALID_HANDLE_VALUE) then
 begin
  Exit(-EBADF);
 end;

 O:=Default(TOVERLAPPED);
 PInt64(@O.Offset)^:=offset;

 //NOTE: pread and pwrite don't change the file position, but ReadFile/WriteFile do, damn it.
 p:=_get_pos(h);

 N:=0;
 if WriteFile(h,data^,size,N,@O) then
 begin
  Result:=N;
 end else
 begin
  Result:=-EIO;
 end;

 _set_pos(h,p);
end;

function ps4_pwrite(fd:Integer;data:Pointer;size,offset:Int64):Int64;  SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_pwrite(fd,data,size,offset);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=_set_errno(-Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function ps4_sceKernelPwrite(fd:Integer;buf:Pointer;nbytes,offset:Int64):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_pwrite(fd,buf,nbytes,offset);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=-Result;
  _set_errno(Result);
  Result:=px2sce(Result);
 end else
 begin
  _set_errno(0);
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

function _sys_fstat(fd:Integer;stat:PSceKernelStat):Integer;
var
 h:THandle;
 hfi:TByHandleFileInformation;
 err:DWORD;
begin
 if (stat=nil) then Exit(EINVAL);

 stat^:=Default(SceKernelStat);

 h:=_get_osfhandle(fd);

 if (h=INVALID_HANDLE_VALUE) then
 begin
  Exit(EBADF);
 end;

 Case SwGetFileType(h) of
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
     err:=SwGetFileInformationByHandle(h,@hfi);
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

function ps4_fstat(fd:Integer;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_fstat(fd,stat));
 _sig_unlock;
end;

function ps4_sceKernelFstat(fd:Integer;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_fstat(fd,stat);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

function _sys_stat(path:PChar;stat:PSceKernelStat):Integer;
var
 rp:RawByteString;
 hfi:WIN32_FILE_ATTRIBUTE_DATA;
 err:DWORD;
begin
 if (path=nil) or (stat=nil) then Exit(EINVAL);

 if (path[0]=#0) then
 begin
  Exit(ENOENT);
 end;

 stat^:=Default(SceKernelStat);

 rp:='';
 Result:=parse_filename(path,rp);

 Case Result of
  PT_ROOT:Exit(-EACCES); //TODO
  PT_FILE:;
  PT_DEV :
    begin
     stat^.st_dev  :=1;
     stat^.st_rdev :=1;
     stat^.st_mode :=S_IFCHR;
     stat^.st_nlink:=1;
     Exit(0);
    end
  else
          Exit(-EACCES);
 end;

 hfi:=Default(WIN32_FILE_ATTRIBUTE_DATA);
 err:=SwGetFileAttributes(rp,@hfi);
 if (err<>0) then
 begin
  Case err of
   ERROR_ACCESS_DENIED,
   ERROR_SHARING_VIOLATION,
   ERROR_LOCK_VIOLATION,
   ERROR_SHARING_BUFFER_EXCEEDED:
     Exit(SCE_KERNEL_ERROR_EACCES);

   ERROR_BUFFER_OVERFLOW:
     Exit(ENAMETOOLONG);

   ERROR_NOT_ENOUGH_MEMORY:
     Exit(ENOMEM);

   else
     Exit(ENOENT);
  end;
 end;

 stat^.st_mode    :=file_attr_to_st_mode(hfi.dwFileAttributes);
 stat^.st_size    :=hfi.nFileSizeLow or (QWORD(hfi.nFileSizeHigh) shl 32);

 stat^.st_atim    :=filetime_to_timespec(hfi.ftLastAccessTime);
 stat^.st_mtim    :=filetime_to_timespec(hfi.ftLastWriteTime);
 stat^.st_ctim    :=stat^.st_mtim;
 stat^.st_birthtim:=filetime_to_timespec(hfi.ftCreationTime);

 stat^.st_blocks  :=((stat^.st_size+511) div 512);
 stat^.st_blksize :=512;

 Result:=0;
end;

function ps4_stat(path:PChar;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_stat(path,stat));
 _sig_unlock;
end;

function ps4_sceKernelStat(path:PChar;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_stat(path,stat);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

function _sys_mkdir(path:PChar):Integer;
var
 fn:RawByteString;
 err:DWORD;
begin
 Result:=0;

 if (path=nil) then Exit(EINVAL);

 if (path[0]=#0) then
 begin
  Exit(ENOENT);
 end;

 Writeln('mkdir:',path);

 fn:='';
 Result:=parse_filename(path,fn);

 Case Result of
  PT_ROOT:Exit(-EACCES); //TODO
  PT_FILE:;
  PT_DEV :Exit(-EACCES);
  else
          Exit(-EACCES);
 end;

 err:=SwCreateDir(fn);

 if (err<>0) then
 begin
  Case err of
   ERROR_INVALID_DRIVE,
   ERROR_PATH_NOT_FOUND,
   ERROR_FILE_NOT_FOUND:
     Exit(ENOENT);

   ERROR_ACCESS_DENIED,
   ERROR_SHARING_VIOLATION,
   ERROR_LOCK_VIOLATION,
   ERROR_SHARING_BUFFER_EXCEEDED:
     Exit(EACCES);

   ERROR_BUFFER_OVERFLOW:
     Exit(ENAMETOOLONG);

   ERROR_NOT_ENOUGH_MEMORY:
     Exit(ENOMEM);

   ERROR_ALREADY_EXISTS,
   ERROR_FILE_EXISTS:
     Exit(EEXIST);

   ERROR_DISK_FULL:
     Exit(ENOSPC);

   else
     Exit(EIO);
  end;
 end;

 Result:=0;
end;

function ps4_mkdir(path:PChar):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_mkdir(path));
 _sig_unlock;
end;

function ps4_sceKernelMkdir(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_mkdir(path);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

function ps4_sceKernelCheckReachability(path:PChar):Integer; SysV_ABI_CDecl;
var
 fn:RawByteString;
begin
 Result:=0;

 if (path=nil) then Exit(_set_sce_errno(SCE_KERNEL_ERROR_EINVAL));
 if (path[0]=#0) then
 begin
  Exit(_set_sce_errno(SCE_KERNEL_ERROR_ENOENT));
 end;

 Writeln('sceKernelCheckReachability:',path);

 fn:='';
 _sig_lock;
 Result:=parse_filename(path,fn);
 _sig_unlock;

 Case Result of
  PT_ROOT:Exit(_set_sce_errno(0));
  PT_FILE:;
  PT_DEV :Exit(_set_sce_errno(0));
  else
          Exit(_set_sce_errno(SCE_KERNEL_ERROR_EACCES));
 end;

 if FileExists(fn) or DirectoryExists(fn) then
 begin
  Result:=0;
  _set_errno(0);
 end else
 begin
  Result:=_set_sce_errno(SCE_KERNEL_ERROR_ENOENT);
 end;

end;

end.

