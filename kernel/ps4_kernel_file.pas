unit ps4_kernel_file;

{$mode objfpc}{$H+}

interface

uses
  windows,
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

function ps4_getdirentries(fd:Integer;buf:Pointer;nbytes:Int64;basep:PInt64):Int64; SysV_ABI_CDecl;
function ps4_getdents(fd:Integer;buf:Pointer;nbytes:Int64):Int64; SysV_ABI_CDecl;
function ps4_sceKernelGetdirentries(fd:Integer;buf:Pointer;nbytes:Int64;basep:PInt64):Int64; SysV_ABI_CDecl;
function ps4_sceKernelGetdents(fd:Integer;buf:Pointer;nbytes:Int64):Int64; SysV_ABI_CDecl;

function ps4_stat(path:PChar;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;
function ps4_sceKernelStat(path:PChar;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;

function ps4_mkdir(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;
function ps4_sceKernelMkdir(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;

function ps4_sceKernelCheckReachability(path:PChar):Integer; SysV_ABI_CDecl;

implementation

uses
 sys_kernel,
 sys_signal;

function _sys_open(path:PChar;flags,mode:Integer):Integer;
const
 WR_RDWR=O_WRONLY or O_RDWR;

var
 rp:RawByteString;
begin
 Result:=0;
 if (path=nil) then Exit(-EINVAL);

 Writeln('open:',path,' ',flags,' (',OctStr(mode,3),')');

 if ((flags and WR_RDWR)=WR_RDWR) then
 begin
  Exit(-EINVAL);
 end;

 if (path[0]=#0) then
 begin
  Exit(-ENOENT);
 end;

 rp:='';
 Result:=parse_filename(path,rp);

 Case Result of
  PT_ROOT:Exit(-EACCES); //TODO
  PT_FILE:
    begin
     if DirectoryExists(rp) then
     begin
      Result:=_sys_dir_open(rp,flags,mode);
     end else
     begin
      if (flags and O_DIRECTORY)<>0 then
      begin
       Exit(-ENOTDIR);
      end;
      Result:=_sys_file_open(rp,flags,mode);
     end;
    end;
  PT_DEV:
    begin
     Result:=_sys_dev_open(rp,flags,mode);
    end;
  else
          Exit(-EACCES);
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

function _sys_lseek(fd:Integer;offset:Int64;whence:Integer):Int64;
var
 f:TCustomFile;
begin
 Result:=0;
 if (fd<0) then Exit(-EINVAL);

 case whence of
  SEEK_SET,
  SEEK_CUR,
  SEEK_END:;
  else
   Exit(-EINVAL);
 end;

 f:=_sys_acqure_fd(fd);
 if (f=nil) then Exit(-EBADF);

 Result:=f.lseek(offset,whence);

 f.Release;
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

function _sys_read(fd:Integer;data:Pointer;size:Int64):Int64;
var
 f:TCustomFile;
begin
 if (data=nil) then Exit(-EFAULT);
 if (fd<0) then Exit(-EINVAL);
 if (size<=0) then Exit(-EINVAL);

 f:=_sys_acqure_fd(fd);
 if (f=nil) then Exit(-EBADF);

 Result:=f.read(data,size);

 f.Release;
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

function _sys_pread(fd:Integer;data:Pointer;size,offset:Int64):Int64;
var
 f:TCustomFile;
begin
 if (fd<0) then Exit(-EINVAL);
 if (data=nil) then Exit(-EFAULT);
 if (size<=0) then Exit(-EINVAL);
 if (offset<0) then Exit(-EINVAL);

 f:=_sys_acqure_fd(fd);
 if (f=nil) then Exit(-EBADF);

 Result:=f.pread(data,size,offset);

 f.Release;
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
 f:TCustomFile;
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

 f:=_sys_acqure_fd(fd);
 if (f=nil) then Exit(-EBADF);

 Result:=f.readv(vector,count);

 f.Release;
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
 f:TCustomFile;
begin
 if (data=nil) then Exit(-EFAULT);
 if (fd<0) then Exit(-EINVAL);
 if (size<=0) then Exit(-EINVAL);

 f:=_sys_acqure_fd(fd);
 if (f=nil) then Exit(-EBADF);

 Result:=f.write(data,size);

 f.Release;
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
 f:TCustomFile;
begin
 if (fd<0) then Exit(-EINVAL);
 if (data=nil) then Exit(-EFAULT);
 if (size<=0) then Exit(-EINVAL);
 if (offset<0) then Exit(-EINVAL);

 f:=_sys_acqure_fd(fd);
 if (f=nil) then Exit(-EBADF);

 Result:=f.pwrite(data,size,offset);

 f.Release;
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

function _sys_fstat(fd:Integer;stat:PSceKernelStat):Integer;
var
 f:TCustomFile;
begin
 if (fd<0) then Exit(EINVAL);
 if (stat=nil) then Exit(EINVAL);

 f:=_sys_acqure_fd(fd);
 if (f=nil) then Exit(-EBADF);

 Result:=f.fstat(stat);

 f.Release;
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

function _sys_getdirentries(fd:Integer;buf:Pointer;nbytes:Int64;basep:PInt64):Int64;
var
 f:TCustomFile;
begin
 if (fd<0) then Exit(-EINVAL);
 if (buf=nil) then Exit(-EFAULT);
 if (nbytes<=0) then Exit(-EINVAL);

 f:=_sys_acqure_fd(fd);
 if (f=nil) then Exit(-EBADF);

 Result:=f.getdirentries(buf,nbytes,basep);

 f.Release;
end;

function ps4_getdirentries(fd:Integer;buf:Pointer;nbytes:Int64;basep:PInt64):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_getdirentries(fd,buf,nbytes,basep);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=_set_errno(-Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function ps4_getdents(fd:Integer;buf:Pointer;nbytes:Int64):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_getdirentries(fd,buf,nbytes,nil);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=_set_errno(-Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function ps4_sceKernelGetdirentries(fd:Integer;buf:Pointer;nbytes:Int64;basep:PInt64):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_getdirentries(fd,buf,nbytes,basep);
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

function ps4_sceKernelGetdents(fd:Integer;buf:Pointer;nbytes:Int64):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_getdirentries(fd,buf,nbytes,nil);
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

function _sys_stat(path:PChar;stat:PSceKernelStat):Integer;
var
 rp:RawByteString;
begin
 if (path=nil) or (stat=nil) then Exit(EINVAL);

 if (path[0]=#0) then
 begin
  Exit(ENOENT);
 end;

 rp:='';
 Result:=parse_filename(path,rp);

 Case Result of
  PT_ROOT:Exit(EACCES); //TODO
  PT_FILE:
    begin
     if DirectoryExists(rp) then
     begin
      Result:=_sys_dir_stat(rp,stat);
     end else
     begin
      Result:=_sys_file_stat(rp,stat);
     end;
    end;
  PT_DEV:
    begin
     Result:=_sys_dev_stat(rp,stat);
    end;
  else
          Exit(EACCES);
 end;
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

function _sys_mkdir(path:PChar;mode:Integer):Integer;
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
  PT_ROOT:Exit(EEXIST); //TODO
  PT_FILE:;
  PT_DEV :Exit(EACCES);
  else
          Exit(EACCES);
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

function ps4_mkdir(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_mkdir(path,mode));
 _sig_unlock;
end;

function ps4_sceKernelMkdir(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_mkdir(path,mode);
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

