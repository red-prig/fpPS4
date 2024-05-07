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

const
 SCE_KERNEL_AIO_DISABLE_SPLIT=0;
 SCE_KERNEL_AIO_ENABLE_SPLIT =1;

 SCE_KERNEL_AIO_SCHED_WINDOW_MAX    =128;
 SCE_KERNEL_AIO_DELAYED_COUNT_MAX   =128;
 SCE_KERNEL_AIO_SPLIT_SIZE_MAX      =$1000000;
 SCE_KERNEL_AIO_SPLIT_CHUNK_SIZE_MAX=$1000000;

 SCE_KERNEL_AIO_SCHED_WINDOW_DEFAULT    =32;
 SCE_KERNEL_AIO_DELAYED_COUNT_DEFAULT   =32;
 SCE_KERNEL_AIO_SPLIT_SIZE_DEFAULT      =$100000;
 SCE_KERNEL_AIO_SPLIT_CHUNK_SIZE_DEFAULT=$100000;

type
 pSceKernelAioSchedulingParam=^SceKernelAioSchedulingParam;
 SceKernelAioSchedulingParam=packed record
  schedulingWindowSize:Integer;
  delayedCountLimit   :Integer;
  enableSplit         :DWORD;
  splitSize           :DWORD;
  splitChunkSize      :DWORD;
 end;

 pSceKernelAioParam=^SceKernelAioParam;
 SceKernelAioParam=packed record
  low :SceKernelAioSchedulingParam;
  mid :SceKernelAioSchedulingParam;
  high:SceKernelAioSchedulingParam;
 end;

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

function ps4_preadv(fd:Integer;vector:p_iovec;count:Integer;offset:Int64):Int64; SysV_ABI_CDecl;
function ps4_sceKernelPreadv(fd:Integer;iov:p_iovec;iovcnt:Integer;offset:Int64):Int64; SysV_ABI_CDecl;

function ps4_write(fd:Integer;data:Pointer;size:Int64):Int64; SysV_ABI_CDecl;
function ps4_sceKernelWrite(fd:Integer;buf:Pointer;nbytes:Int64):Int64; SysV_ABI_CDecl;

function ps4_pwrite(fd:Integer;data:Pointer;size,offset:Int64):Int64;  SysV_ABI_CDecl;
function ps4_sceKernelPwrite(fd:Integer;buf:Pointer;nbytes,offset:Int64):Int64; SysV_ABI_CDecl;

function ps4_writev(fd:Integer;vector:p_iovec;count:Integer):Int64; SysV_ABI_CDecl;
function ps4_sceKernelWritev(fd:Integer;iov:p_iovec;iovcnt:Integer):Int64; SysV_ABI_CDecl;

function ps4_ftruncate(fd:Integer;size:Int64):Integer; SysV_ABI_CDecl;
function ps4_sceKernelFtruncate(fd:Integer;size:Int64):Integer; SysV_ABI_CDecl;

function ps4_fstat(fd:Integer;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;
function ps4_sceKernelFstat(fd:Integer;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;

function ps4_getdirentries(fd:Integer;buf:Pointer;nbytes:Int64;basep:PInt64):Int64; SysV_ABI_CDecl;
function ps4_getdents(fd:Integer;buf:Pointer;nbytes:Int64):Int64; SysV_ABI_CDecl;
function ps4_sceKernelGetdirentries(fd:Integer;buf:Pointer;nbytes:Int64;basep:PInt64):Int64; SysV_ABI_CDecl;
function ps4_sceKernelGetdents(fd:Integer;buf:Pointer;nbytes:Int64):Int64; SysV_ABI_CDecl;

function ps4_fsync(fd:Integer):Integer; SysV_ABI_CDecl;
function ps4_sceKernelFsync(fd:Integer):Integer; SysV_ABI_CDecl;

function ps4_fcntl(fd,cmd:Integer;param1:ptruint):Integer; SysV_ABI_CDecl;
function ps4_sceKernelFcntl(fd,cmd:Integer;param1:ptruint):Integer; SysV_ABI_CDecl;

function ps4_ioctl(fd,cmd:Integer;param1:ptruint):Integer; SysV_ABI_CDecl;

function ps4_stat(path:PChar;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;
function ps4_sceKernelStat(path:PChar;stat:PSceKernelStat):Integer; SysV_ABI_CDecl;

function ps4_truncate(path:PChar;length:Int64):Integer; SysV_ABI_CDecl;
function ps4_sceKernelTruncate(path:PChar;length:Int64):Integer; SysV_ABI_CDecl;

function ps4_mkdir(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;
function ps4_sceKernelMkdir(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;

function ps4_unlink(path:PChar):Integer; SysV_ABI_CDecl;
function ps4_sceKernelUnlink(path:PChar):Integer; SysV_ABI_CDecl;

function ps4_rmdir(path:PChar):Integer; SysV_ABI_CDecl;
function ps4_sceKernelRmdir(path:PChar):Integer; SysV_ABI_CDecl;

function ps4_rename(from,pto:PChar):Integer; SysV_ABI_CDecl;
function ps4_sceKernelRename(from,pto:PChar):Integer; SysV_ABI_CDecl;

function ps4_chmod(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;
function ps4_sceKernelChmod(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;

function ps4_sceKernelCheckReachability(path:PChar):Integer; SysV_ABI_CDecl;

function ps4_access(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;

function ps4_getdtablesize:Integer; SysV_ABI_CDecl;
function ps4_sceKernelGetFsSandboxRandomWord:PChar; SysV_ABI_CDecl;

procedure ps4_sceKernelAioInitializeParam(param:pSceKernelAioParam); SysV_ABI_CDecl;   

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

 Writeln(SysLogPrefix, 'open:',path,' 0x',HexStr(flags,4),' (',OctStr(mode,3),')');

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
  PT_ROOT:
    begin
     Result:=_sys_root_open(rp,flags,mode);
    end;
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

function ps4_preadv(fd:Integer;vector:p_iovec;count:Integer;offset:Int64):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_preadv(fd,vector,count,offset);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=_set_errno(-Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function ps4_sceKernelPreadv(fd:Integer;iov:p_iovec;iovcnt:Integer;offset:Int64):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_preadv(fd,iov,iovcnt,offset);
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

function ps4_writev(fd:Integer;vector:p_iovec;count:Integer):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_writev(fd,vector,count);
 _sig_unlock;

 if (Result<0) then
 begin
  Result:=_set_errno(-Result);
 end else
 begin
  _set_errno(0);
 end;
end;

function ps4_sceKernelWritev(fd:Integer;iov:p_iovec;iovcnt:Integer):Int64; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_writev(fd,iov,iovcnt);
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

function ps4_ftruncate(fd:Integer;size:Int64):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_ftruncate(fd,size));
 _sig_unlock;
end;

function ps4_sceKernelFtruncate(fd:Integer;size:Int64):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_ftruncate(fd,size);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
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

function ps4_fsync(fd:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_fsync(fd));
 _sig_unlock;
end;

function ps4_sceKernelFsync(fd:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_fsync(fd);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

function ps4_fcntl(fd,cmd:Integer;param1:ptruint):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_fcntl(fd,cmd,param1));
 _sig_unlock;
end;

function ps4_sceKernelFcntl(fd,cmd:Integer;param1:ptruint):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_fcntl(fd,cmd,param1);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

function ps4_ioctl(fd,cmd:Integer;param1:ptruint):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_ioctl(fd,cmd,param1));
 _sig_unlock;
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

 //Writeln('stat:',path);

 rp:='';
 Result:=parse_filename(path,rp);

 stat^:=Default(SceKernelStat);

 Case Result of
  PT_ROOT:
   begin
    Result:=_sys_root_stat(rp,stat);
   end;
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

function _sys_truncate(path:PChar;length:Int64):Integer;
var
 fn:RawByteString;
begin
 Result:=0;

 if (path=nil) then Exit(EINVAL);
 if (length<=0) then Exit(EINVAL);

 if (path[0]=#0) then
 begin
  Exit(ENOENT);
 end;

 Writeln(SysLogPrefix,'truncate:',path,' ',length);

 fn:='';
 Result:=parse_filename(path,fn);

 Case Result of
  PT_ROOT:Exit(EACCES); //TODO
  PT_FILE:;
  PT_DEV :Exit(EACCES);
  else
          Exit(EACCES);
 end;

 if FileExists(fn) then
 begin
  Result:=_sys_file_trunc(fn,length);
 end else
 begin
  if DirectoryExists(fn) then
  begin
   Result:=EISDIR;
  end else
  begin
   Result:=ENOENT;
  end;
 end;
end;

function ps4_truncate(path:PChar;length:Int64):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_truncate(path,length));
 _sig_unlock;
end;

function ps4_sceKernelTruncate(path:PChar;length:Int64):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_truncate(path,length);
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

Function SwDeleteFile(Const path:RawByteString):DWORD;
var
 W:WideString;
begin
 Result:=0;
 _sig_lock;
 W:=UTF8Decode(path);
 if not DeleteFileW(PWideChar(W)) then
 begin
  Result:=GetLastError;
 end;
 _sig_unlock;
end;

function _sys_unlink(path:PChar):Integer;
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

 Writeln('unlink:',path);

 fn:='';
 Result:=parse_filename(path,fn);

 Case Result of
  PT_ROOT:Exit(EACCES); //TODO
  PT_FILE:;
  PT_DEV :Exit(EACCES);
  else
          Exit(EACCES);
 end;

 err:=SwDeleteFile(fn);

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

function ps4_unlink(path:PChar):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_unlink(path));
 _sig_unlock;
end;

function ps4_sceKernelUnlink(path:PChar):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_unlink(path);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

Function SwRemoveDirectory(Const path:RawByteString):DWORD;
var
 W:WideString;
begin
 Result:=0;
 _sig_lock;
 W:=UTF8Decode(path);
 if not RemoveDirectoryW(PWideChar(W)) then
 begin
  Result:=GetLastError;
 end;
 _sig_unlock;
end;

function _sys_rmdir(path:PChar):Integer;
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

 Writeln('rmdir:',path);

 fn:='';
 Result:=parse_filename(path,fn);

 Case Result of
  PT_ROOT:Exit(EACCES); //TODO
  PT_FILE:;
  PT_DEV :Exit(EACCES);
  else
          Exit(EACCES);
 end;

 err:=SwRemoveDirectory(fn);

 if (err<>0) then
 begin
  Case err of
   ERROR_DIRECTORY,
   ERROR_INVALID_DRIVE,
   ERROR_PATH_NOT_FOUND,
   ERROR_FILE_NOT_FOUND:
     Exit(ENOENT);

   ERROR_CURRENT_DIRECTORY,
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

   ERROR_DIR_NOT_EMPTY:
     Exit(ENOTEMPTY);

   else
     Exit(EIO);
  end;
 end;

 Result:=0;
end;

function ps4_rmdir(path:PChar):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_rmdir(path));
 _sig_unlock;
end;

function ps4_sceKernelRmdir(path:PChar):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_rmdir(path);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

Function SwMoveFileEx(Const f,t:RawByteString):DWORD;
var
 Wf,Wt:WideString;
begin
 Result:=0;
 _sig_lock;

 Wf:=UTF8Decode(f);
 Wt:=UTF8Decode(t);

 if not MoveFileExW(PWideChar(Wf),PWideChar(Wt),MOVEFILE_REPLACE_EXISTING or MOVEFILE_COPY_ALLOWED) then
 begin
  Result:=GetLastError;
 end;
 _sig_unlock;
end;

function _sys_rename(from,pto:PChar):Integer;
var
 ffrom,fto:RawByteString;
 err:DWORD;
begin
 Result:=0;

 if (from=nil) or (pto=nil) then Exit(EINVAL);

 if (from[0]=#0) or (pto[0]=#0) then
 begin
  Exit(ENOENT);
 end;

 Writeln('rename:',from,'->',pto);

 ffrom:='';
 Result:=parse_filename(from,ffrom);

 Case Result of
  PT_ROOT:Exit(EACCES); //TODO
  PT_FILE:;
  PT_DEV :Exit(EACCES);
  else
          Exit(EACCES);
 end;

 fto:='';
 Result:=parse_filename(pto,fto);

 Case Result of
  PT_ROOT:Exit(EACCES); //TODO
  PT_FILE:;
  PT_DEV :Exit(EACCES);
  else
          Exit(EACCES);
 end;

 err:=SwMoveFileEx(ffrom,fto);

 if (err<>0) then
 begin
  Case err of
   ERROR_DIRECTORY,
   ERROR_INVALID_DRIVE,
   ERROR_PATH_NOT_FOUND,
   ERROR_FILE_NOT_FOUND:
     Exit(ENOENT);

   ERROR_CURRENT_DIRECTORY,
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

function ps4_rename(from,pto:PChar):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_rename(from,pto));
 _sig_unlock;
end;

function ps4_sceKernelRename(from,pto:PChar):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_rename(from,pto);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

function _sys_chmod(path:PChar;mode:Integer):Integer;
var
 fn:RawByteString;
begin
 Result:=0;

 if (path=nil) then Exit(EINVAL);

 if (path[0]=#0) then
 begin
  Exit(ENOENT);
 end;

 Writeln(SysLogPrefix,'chmod:',path,' (',OctStr(mode,3),')');

 fn:='';
 Result:=parse_filename(path,fn);

 Case Result of
  PT_ROOT:Exit(EACCES); //TODO
  PT_FILE:;
  PT_DEV :Exit(EACCES);
  else
          Exit(EACCES);
 end;

 if FileExists(fn) or DirectoryExists(fn) then
 begin
  Result:=0;
 end else
 begin
  Result:=ENOENT;
 end;
end;

function ps4_chmod(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(_sys_chmod(path,mode));
 _sig_unlock;
end;

function ps4_sceKernelChmod(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sys_chmod(path,mode);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

function ps4_sceKernelCheckReachability(path:PChar):Integer; SysV_ABI_CDecl;
var
 fn:RawByteString;
 ex:Boolean;
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
  PT_ROOT:Exit(_set_sce_errno(0)); //TODO dir exists
  PT_FILE:;
  PT_DEV :Exit(_set_sce_errno(0));
  else
          Exit(_set_sce_errno(SCE_KERNEL_ERROR_EACCES));
 end;

 _sig_lock;
 ex:=FileExists(fn) or DirectoryExists(fn);
 _sig_unlock;

 if ex then
 begin
  Result:=0;
  _set_errno(0);
 end else
 begin
  Result:=_set_sce_errno(SCE_KERNEL_ERROR_ENOENT);
 end;

end;

//access function
const
 F_OK = $00; // test for existence of file
 X_OK = $01; // test for execute or search permission
 W_OK = $02; // test for write permission
 R_OK = $04; // test for read permission

 A_ALL=F_OK or X_OK or W_OK or R_OK;

function sys_access(path:PChar;mode:Integer):Integer;
var
 fn:RawByteString;
begin
 Result:=0;

 if (path=nil) then Exit(EFAULT);

 //Writeln('access:',path,' ',mode);

 if (path[0]=#0) then
 begin
  Exit(ENOENT);
 end;

 if ((mode and (not A_ALL))<>0) then
 begin
  Exit(EINVAL);
 end;

 fn:='';
 Result:=parse_filename(path,fn);

 Case Result of
  PT_ROOT:Exit(0); //TODO test dir exits
  PT_FILE:;
  PT_DEV :Exit(0);
  else
          Exit(EACCES);
 end;

 if FileExists(fn) or DirectoryExists(fn) then
 begin
  Result:=0;
 end else
 begin
  Result:=ENOENT;
 end;

end;

function ps4_access(path:PChar;mode:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_set_errno(sys_access(path,mode));
 _sig_unlock;
end;

function ps4_getdtablesize:Integer; SysV_ABI_CDecl;
begin
 Result:=High(Integer);
 _set_errno(0);
end;

const
 fs_word:Pchar='sys';

function ps4_sceKernelGetFsSandboxRandomWord:PChar; SysV_ABI_CDecl;
begin
 //__sys_randomized_path
 Result:=fs_word;
end;

procedure ps4_sceKernelAioInitializeParam(param:pSceKernelAioParam); SysV_ABI_CDecl;
begin
 param^.low.schedulingWindowSize :=$20;
 param^.low.delayedCountLimit    :=$20;
 param^.low.enableSplit          :=1;
 param^.low.splitSize            :=$100000;
 param^.low.splitChunkSize       :=$100000;
 param^.mid.schedulingWindowSize :=$20;
 param^.mid.delayedCountLimit    :=$20;
 param^.mid.enableSplit          :=1;
 param^.mid.splitSize            :=$100000;
 param^.mid.splitChunkSize       :=$100000;
 param^.high.schedulingWindowSize:=$20;
 param^.high.delayedCountLimit   :=$20;
 param^.high.enableSplit         :=0;
 param^.high.splitSize           :=0;
 param^.high.splitChunkSize      :=0;
end;   

end.

