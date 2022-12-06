unit sys_fd;

{$mode ObjFPC}{$H+}

interface

uses
 windows,
 Classes,
 SysUtils,
 ps4_handles,
 sys_types,
 sys_kernel;

const
 NAME_MAX  =255;  // max bytes in a file name
 PATH_MAX  =1024; // max bytes in pathname
 IOV_MAX   =1024; // max elements in i/o vector
 MAXNAMLEN =255;

 O_RDONLY  =$0000;  // open for reading only
 O_WRONLY  =$0001;  // open for writing only
 O_RDWR    =$0002;  // open for reading and writing
 O_ACCMODE =$0003;  // mask for above modes

 O_NONBLOCK =$0004;  // no delay
 O_APPEND   =$0008;  // set append mode
 O_SYNC     =$0080;  // POSIX synonym for O_FSYNC
 O_CREAT    =$0200;  // create if nonexistent
 O_TRUNC    =$0400;  // truncate to zero length
 O_EXCL     =$0800;  // error if already exists
 O_DSYNC    =$1000;  // synchronous data writes(omit inode writes)

 O_DIRECT   =$00010000;
 O_FSYNC    =$0080;  // synchronous writes

 O_DIRECTORY =$00020000; // Fail if not directory
 O_EXEC      =$00040000; // Open for execute only

 S_IRWXU  =&0000700;   // RWX mask for owner
 S_IRUSR  =&0000400;   // R for owner
 S_IWUSR  =&0000200;   // W for owner
 S_IXUSR  =&0000100;   // X for owner

 S_IRWXG  =&0000070;   // RWX mask for group
 S_IRGRP  =&0000040;   // R for group
 S_IWGRP  =&0000020;   // W for group
 S_IXGRP  =&0000010;   // X for group

 S_IRWXO  =&0000007;   // RWX mask for other
 S_IROTH  =&0000004;   // R for other
 S_IWOTH  =&0000002;   // W for other
 S_IXOTH  =&0000001;   // X for other

 S_IFMT   =&0170000; // type of file mask
 S_IFIFO  =&0010000; // named pipe (fifo)
 S_IFCHR  =&0020000; // character special
 S_IFDIR  =&0040000; // directory
 S_IFBLK  =&0060000; // block special
 S_IFREG  =&0100000; // regular
 S_IFLNK  =&0120000; // symbolic link
 S_IFSOCK =&0140000; // socket
 S_ISVTX  =&0001000; // save swapped text even after use

 F_GETFL  =3;  // get file status flags
 F_SETFL  =4;  // set file status flags

 SEEK_SET =0; // set file offset to offset
 SEEK_CUR =1; // set file offset to current plus offset
 SEEK_END =2; // set file offset to EOF plus offset

 MAP_SHARED  =$0001;  // share changes
 MAP_PRIVATE =$0002;  // changes are private

 MAP_FILE   =$0000; // map from file (default)
 MAP_ANON   =$1000; // allocated from memory, swap space
 MAP_SYSTEM =$2000;

 MAP_NOCORE  =$00020000; // dont include these pages in a coredump
 MAP_NOSYNC  =$0800; // page to but do not sync underlying file
 MAP_PREFAULT_READ =$00040000; // prefault mapping for reading

 DT_UNKNOWN =0;
 DT_DIR     =4;
 DT_REG     =8;

 SCE_KERNEL_NAME_MAX        =NAME_MAX;
 SCE_KERNEL_PATH_MAX        =PATH_MAX;
 SCE_KERNEL_IOV_MAX         =IOV_MAX;
 SCE_KERNEL_MAXNAMLEN       =MAXNAMLEN;

 SCE_KERNEL_O_RDONLY        =O_RDONLY;
 SCE_KERNEL_O_WRONLY        =O_WRONLY;
 SCE_KERNEL_O_RDWR          =O_RDWR ;
 SCE_KERNEL_O_NONBLOCK      =O_NONBLOCK;
 SCE_KERNEL_O_APPEND        =O_APPEND;
 SCE_KERNEL_O_CREAT         =O_CREAT;
 SCE_KERNEL_O_TRUNC         =O_TRUNC;
 SCE_KERNEL_O_EXCL          =O_EXCL;
 SCE_KERNEL_O_DIRECT        =O_DIRECT;
 SCE_KERNEL_O_FSYNC         =O_FSYNC;
 SCE_KERNEL_O_SYNC          =O_SYNC;
 SCE_KERNEL_O_DSYNC         =O_DSYNC;
 SCE_KERNEL_O_DIRECTORY     =O_DIRECTORY;

 SCE_KERNEL_S_IFMT          =S_IFMT;
 SCE_KERNEL_S_IFDIR         =S_IFDIR;
 SCE_KERNEL_S_IFREG         =S_IFREG;

 SCE_KERNEL_S_IRUSR         =(S_IRUSR or S_IRGRP or S_IROTH or S_IXUSR or S_IXGRP or S_IXOTH);
 SCE_KERNEL_S_IWUSR         =(S_IWUSR or S_IWGRP or S_IWOTH or S_IXUSR or S_IXGRP or S_IXOTH);
 SCE_KERNEL_S_IXUSR         =(S_IXUSR or S_IXGRP or S_IXOTH);
 SCE_KERNEL_S_IRWXU         =(SCE_KERNEL_S_IRUSR or SCE_KERNEL_S_IWUSR);

 SCE_KERNEL_S_IRWU          =(SCE_KERNEL_S_IRUSR or SCE_KERNEL_S_IWUSR);
// 00777, R/W
 SCE_KERNEL_S_IRU           =(SCE_KERNEL_S_IRUSR);
// 00555, R

 SCE_KERNEL_S_INONE         =&0000000;

 //SCE_KERNEL_S_ISDIR(m)      =S_ISDIR(m);
 //SCE_KERNEL_S_ISREG(m)      =S_ISREG(m);

// for sceKernelFcntl()
 SCE_KERNEL_F_GETFL         =F_GETFL;
 SCE_KERNEL_F_SETFL         =F_SETFL;

// for sceKernelLseek()
 SCE_KERNEL_SEEK_SET        =SEEK_SET;
 SCE_KERNEL_SEEK_CUR        =SEEK_CUR;
 SCE_KERNEL_SEEK_END        =SEEK_END;

// for sceKernelMmap()
 SCE_KERNEL_MAP_NOCORE      =MAP_NOCORE;
 SCE_KERNEL_MAP_NOSYNC      =MAP_NOSYNC;
 SCE_KERNEL_MAP_PREFAULT_READ=MAP_PREFAULT_READ;
 SCE_KERNEL_MAP_PRIVATE     =MAP_PRIVATE;
 SCE_KERNEL_MAP_SHARED      =MAP_SHARED;

// for SceKernelDirent
 SCE_KERNEL_DT_UNKNOWN      =DT_UNKNOWN;
 SCE_KERNEL_DT_DIR          =DT_DIR;
 SCE_KERNEL_DT_REG          =DT_REG;

// for sceKernelSetCompress
 SCE_KERNEL_COMPRESS_FILE_MAGIC =($43534650);
 SCE_KERNEL_SET_COMPRESS_FILE   =(1);
 SCE_KERNEL_SET_REGULAR_FILE    =(0);

// for sceKernelLwfsSetAttribute
 SCE_KERNEL_LWFS_DISABLE =(0);
 SCE_KERNEL_LWFS_ENABLE  =(1);

type
 p_iovec=^iovec;
 iovec=packed record
  iov_base:Pointer; //Base address.
  iov_len :Int64;   //Length.
 end;

 PSceKernelStat=^SceKernelStat;
 SceKernelStat=packed object
  type
   __dev_t  =DWORD;
   ino_t    =DWORD;
   mode_t   =Word;
   nlink_t  =Word;
   uid_t    =DWORD;
   gid_t    =DWORD;
   off_t    =Int64;
   blkcnt_t =Int64;
   blksize_t=DWORD;
   fflags_t =DWORD;
  var
   st_dev     :__dev_t   ;      // inode's device
   st_ino     :ino_t     ;      // inode's number
   st_mode    :mode_t    ;      // inode protection mode         S_IFMT.....
   st_nlink   :nlink_t   ;      // number of hard links
   st_uid     :uid_t     ;      // user ID of the file's owner   S_IRWXU....
   st_gid     :gid_t     ;      // group ID of the file's group  S_IRWXG....
   st_rdev    :__dev_t   ;      // device type
   st_atim    :timespec  ;      // time of last access
   st_mtim    :timespec  ;      // time of last data modification
   st_ctim    :timespec  ;      // time of last file status change
   st_size    :off_t     ;      // file size, in bytes
   st_blocks  :blkcnt_t  ;      // blocks allocated for file
   st_blksize :blksize_t ;      // optimal blocksize for I/O
   st_flags   :fflags_t  ;      // user defined flags for file
   st_gen     :DWORD     ;      // file generation number
   st_lspare  :DWORD     ;
   st_birthtim:timespec  ;      // time of file creation
 end;

 TCustomFile=class(TClassHandle)
  var
   fd:Integer;
   Handle:THandle;
  function lseek        (offset:Int64;whence:Integer):Int64;           virtual;
  function read         (data:Pointer;size:Int64):Int64;               virtual;
  function pread        (data:Pointer;size,offset:Int64):Int64;        virtual;
  function readv        (vector:p_iovec;count:Integer):Int64;          virtual;
  function write        (data:Pointer;size:Int64):Int64;               virtual;
  function pwrite       (data:Pointer;size,offset:Int64):Int64;        virtual;
  function ftruncate    (size:Int64):Integer;                          virtual;
  function fstat        (stat:PSceKernelStat):Integer;                 virtual;
  function fsync        ():Integer;                                    virtual;
  function getdirentries(buf:Pointer;nbytes:Int64;basep:PInt64):Int64; virtual;
 end;

function _sys_get_osfhandle(fd:Integer):THandle;
function _sys_open_fd(f:TCustomFile):Integer;
function _sys_acqure_fd(fd:Integer):TCustomFile;
function _sys_close(fd:Integer):Integer;

implementation

var
 FileHandles:TIntegerHandles;

//

function TCustomFile.lseek (offset:Int64;whence:Integer):Int64;
begin
 Result:=-ENOTSUP;
end;

function TCustomFile.read  (data:Pointer;size:Int64):Int64;
begin
 Result:=-ENOTSUP;
end;

function TCustomFile.pread (data:Pointer;size,offset:Int64):Int64;
begin
 Result:=-ENOTSUP;
end;

function TCustomFile.readv (vector:p_iovec;count:Integer):Int64;
begin
 Result:=-ENOTSUP;
end;

function TCustomFile.write (data:Pointer;size:Int64):Int64;
begin
 Result:=-ENOTSUP;
end;

function TCustomFile.pwrite(data:Pointer;size,offset:Int64):Int64;
begin
 Result:=-ENOTSUP;
end;

function TCustomFile.ftruncate(size:Int64):Integer;
begin
 Result:=ENOTSUP;
end;

function TCustomFile.fstat (stat:PSceKernelStat):Integer;
begin
 Result:=ENOTSUP;
end;

function TCustomFile.fsync:Integer;
begin
 Result:=EINVAL;
end;

function TCustomFile.getdirentries(buf:Pointer;nbytes:Int64;basep:PInt64):Int64;
begin
 Result:=-EINVAL;
end;

//

function _sys_get_osfhandle(fd:Integer):THandle;
var
 f:TCustomFile;
begin
 Result:=INVALID_HANDLE_VALUE;
 f:=TCustomFile(FileHandles.Acqure(fd));
 if (f<>nil) then
 begin
  Result:=f.Handle;
  f.Release;
 end;
end;

function _sys_open_fd(f:TCustomFile):Integer;
begin
 if (f=nil) then Exit(-EINVAL);
 if FileHandles.New(f,Result) then
 begin
  f.fd:=Result;
 end else
 begin
  Result:=-EMFILE;
 end;
end;

function _sys_acqure_fd(fd:Integer):TCustomFile;
begin
 Result:=TCustomFile(FileHandles.Acqure(fd));
end;

function _sys_close(fd:Integer):Integer;
begin
 Result:=0;
 if (fd<0) then Exit(EINVAL);
 if not FileHandles.Delete(fd) then Exit(EBADF);
end;

//

initialization
 FileHandles:=TIntegerHandles.Create(0);

end.

