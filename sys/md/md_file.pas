unit md_file;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 errno,
 ntapi,
 windows,
 time,
 vfcntl,
 vstat;

const
 O_RDONLY   =vfcntl.O_RDONLY   ;
 O_WRONLY   =vfcntl.O_WRONLY   ;
 O_RDWR     =vfcntl.O_RDWR     ;
 O_NONBLOCK =vfcntl.O_NONBLOCK ;
 O_APPEND   =vfcntl.O_APPEND   ;
 O_ASYNC    =vfcntl.O_ASYNC    ;
 O_FSYNC    =vfcntl.O_FSYNC    ;
 O_SYNC     =vfcntl.O_SYNC     ;
 O_NOFOLLOW =vfcntl.O_NOFOLLOW ;
 O_CREAT    =vfcntl.O_CREAT    ;
 O_TRUNC    =vfcntl.O_TRUNC    ;
 O_EXCL     =vfcntl.O_EXCL     ;
 O_DSYNC    =vfcntl.O_DSYNC    ;
 O_DIRECTORY=vfcntl.O_DIRECTORY;

 AT_FDCWD  =vfcntl.AT_FDCWD;

type
 p_timespec=time.p_timespec;
 timespec  =time.timespec;

 p_stat=vstat.p_stat;
 t_stat=vstat.t_stat;

function  md_openat(at_fd:THandle;const path:RawByteString;flags,mode:DWORD;Var fd:THandle):DWORD;
function  md_open  (const path:RawByteString;flags,mode:DWORD;Var fd:THandle):DWORD;
function  md_close (fd:THandle):DWORD;

Function  md_create_swap_file(const path:RawByteString;SIZE:QWORD;Var fd:THandle):DWORD;
Function  md_delete_file     (const path:RawByteString):DWORD;

function  md_fstat(fd:THandle;sb:p_stat):Integer;
function  md_stat (const path:RawByteString;sb:p_stat):Integer;

function  md_futimens(fd:THandle;ts:p_timespec;numtimes:Integer):Integer;
function  md_utimens (const path:RawByteString;ts:p_timespec;numtimes:Integer):Integer;

implementation

const
 FILE_SHARE_ALL=FILE_SHARE_READ or
                FILE_SHARE_WRITE or
                FILE_SHARE_DELETE;

type
 TOBJ_ATTR=packed record
  OATTR:OBJECT_ATTRIBUTES;
  UPATH:UNICODE_STRING;
 end;

function INIT_UNICODE(const FileName:WideString):UNICODE_STRING;
begin
 Result.Length       :=Length(FileName)*SizeOf(WideChar);
 Result.MaximumLength:=Result.Length+SizeOf(WideChar);
 Result.Buffer       :=PWideChar(FileName);
end;

procedure INIT_OBJ(var OBJ:TOBJ_ATTR;fd:THandle;attr:ULONG;const FileName:WideString);
begin
 OBJ.OATTR.Length:=SizeOf(OBJECT_ATTRIBUTES);

 OBJ.OATTR.RootDirectory:=fd;
 OBJ.OATTR.ObjectName   :=@OBJ.UPATH;
 OBJ.OATTR.Attributes   :=attr;

 OBJ.UPATH:=INIT_UNICODE(FileName);
end;

function ntf2px(n:Integer):Integer; inline;
begin
 Case DWORD(n) of
  STATUS_SUCCESS               :Result:=0;
  STATUS_PENDING               :Result:=EWOULDBLOCK;
  STATUS_NO_MORE_FILES         :Result:=0;
  STATUS_ACCESS_VIOLATION      :Result:=EFAULT;
  STATUS_INVALID_HANDLE        :Result:=EBADF;
  STATUS_NO_SUCH_FILE          :Result:=ENOENT;
  STATUS_END_OF_FILE           :Result:=0;
  STATUS_NO_MEMORY             :Result:=ENOMEM;
  STATUS_ACCESS_DENIED         :Result:=EACCES;
  STATUS_DISK_CORRUPT_ERROR    :Result:=EIO;
  STATUS_OBJECT_NAME_NOT_FOUND :Result:=ENOENT;
  STATUS_OBJECT_NAME_COLLISION :Result:=EEXIST;
  STATUS_OBJECT_PATH_NOT_FOUND :Result:=ENOENT;
  STATUS_OBJECT_PATH_SYNTAX_BAD:Result:=ENOTDIR;
  STATUS_SHARING_VIOLATION     :Result:=EACCES;
  STATUS_FILE_LOCK_CONFLICT    :Result:=EWOULDBLOCK;
  STATUS_LOCK_NOT_GRANTED      :Result:=EWOULDBLOCK;
  STATUS_RANGE_NOT_LOCKED      :Result:=ENOLCK;
  STATUS_DISK_FULL             :Result:=ENOSPC;
  STATUS_FILE_IS_A_DIRECTORY   :Result:=EISDIR;
  STATUS_NOT_SAME_DEVICE       :Result:=EXDEV;
  STATUS_INSUFFICIENT_RESOURCES:Result:=ENOMEM;
  STATUS_DIRECTORY_NOT_EMPTY   :Result:=ENOTEMPTY;
  STATUS_FILE_CORRUPT_ERROR    :Result:=EIO;
  STATUS_NOT_A_DIRECTORY       :Result:=ENOTDIR;
  STATUS_NAME_TOO_LONG         :Result:=ENAMETOOLONG;
  STATUS_IO_DEVICE_ERROR       :Result:=EIO;
  STATUS_TOO_MANY_LINKS        :Result:=EMLINK;
  STATUS_CANT_CROSS_RM_BOUNDARY:Result:=EXDEV;
  else
                                Result:=EINVAL;
 end;
end;

function get_unix_file_time(time:LARGE_INTEGER):timespec; inline;
begin
 Int64(time):=Int64(time)-DELTA_EPOCH_IN_UNIT;
 Result.tv_sec :=(Int64(time) div UNIT_PER_SEC);
 Result.tv_nsec:=(Int64(time) mod UNIT_PER_SEC)*NSEC_PER_UNIT;
end;

function get_win_file_time(time:timespec):LARGE_INTEGER; inline;
begin
 Int64(Result):=(time.tv_sec*UNIT_PER_SEC)+(time.tv_nsec div NSEC_PER_UNIT);
 Int64(Result):=Int64(Result)+DELTA_EPOCH_IN_UNIT;
end;

Function GetDesiredAccess(flags:Integer):DWORD; inline;
begin
 Result:=SYNCHRONIZE or
         FILE_READ_ATTRIBUTES or
         FILE_WRITE_ATTRIBUTES;

 if ((flags and O_RDWR)<>0) then
 begin
  Result:=Result or (ord((flags and O_APPEND)=0)*FILE_WRITE_DATA) or (FILE_READ_DATA or FILE_APPEND_DATA);
 end else
 if ((flags and O_WRONLY)<>0) then
 begin
  Result:=Result or (ord((flags and O_APPEND)=0)*FILE_WRITE_DATA) or FILE_APPEND_DATA;
 end else
 begin
  Result:=Result or FILE_READ_DATA;
 end;

 if ((flags and O_DIRECTORY)<>0) then
 begin
  Result:=Result or FILE_LIST_DIRECTORY;
 end;
end;

Function GetCreationDisposition(flags:Integer):DWORD; inline;
begin
 Result:=0;
 if ((flags and O_CREAT)<>0) then
 begin
  if ((flags and O_EXCL)<>0) then
  begin
   Result:=FILE_CREATE;
  end else
  if ((flags and O_TRUNC)<>0) then
  begin
   Result:=FILE_OVERWRITE_IF;
  end else
  begin
   Result:=FILE_OPEN_IF;
  end;
 end else
 if ((flags and O_TRUNC)<>0) then
 begin
  Result:=FILE_OVERWRITE;
 end else
 begin
  Result:=FILE_OPEN;
 end;
end;

Function GetFileAttrtibute(flags,mode:Integer):DWORD; inline;
begin
 Result:=FILE_ATTRIBUTE_NORMAL;
 if ((flags and O_CREAT)<>0) and
    ((mode and (S_IWUSR or S_IWGRP or S_IWOTH))=0) then
 begin
  Result:=Result or FILE_ATTRIBUTE_READONLY;
 end;
end;

Function GetCreateOptions(flags:Integer):DWORD; inline;
begin
 Result:=FILE_SYNCHRONOUS_IO_NONALERT;

 if ((flags and (O_FSYNC or O_DSYNC))<>0) then
 begin
  Result:=Result or FILE_WRITE_THROUGH;
 end;

 if ((flags and O_DIRECTORY)<>0) then
 begin
  Result:=Result or (FILE_DIRECTORY_FILE or FILE_OPEN_FOR_BACKUP_INTENT);
 end;
end;

function md_openat(at_fd:THandle;const path:RawByteString;flags,mode:DWORD;Var fd:THandle):DWORD;
var
 DA,FA,CD,CO:DWORD;

 w:WideString;
 OBJ:TOBJ_ATTR;
 BLK:IO_STATUS_BLOCK;
begin

 w:='';
 if (at_fd=THandle(AT_FDCWD)) then
 begin
  //current dir
  at_fd:=0;

  SetLength(w,GetCurrentDirectoryW(0,nil));
  GetCurrentDirectoryW(Length(w),@w[1]);
  SetLength(w,Length(w)-1);

  w:='\??\'+w+'\'+UTF8Decode(path);
 end else
 if (at_fd=0) or (at_fd=INVALID_HANDLE_VALUE) then
 begin
  //full path
  at_fd:=0;
  w:='\??\'+UTF8Decode(ExpandFileName(path));
 end else
 begin
  //relative opened
  w:=UTF8Decode(path);
 end;

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,at_fd,OBJ_CASE_INSENSITIVE,w);
 BLK:=Default(IO_STATUS_BLOCK);

 DA:=GetDesiredAccess(flags);
 FA:=GetFileAttrtibute(flags,mode);
 CD:=GetCreationDisposition(flags);
 CO:=GetCreateOptions(flags);

 Result:=ntf2px(NtCreateFile(@fd,
                             DA,
                             @OBJ,
                             @BLK,
                             nil,
                             FA,
                             FILE_SHARE_ALL,
                             CD,
                             CO,
                             nil,
                             0));

end;

function md_open(const path:RawByteString;flags,mode:DWORD;Var fd:THandle):DWORD;
begin
 Result:=md_openat(0,path,flags,mode,fd);
end;

function md_close(fd:THandle):DWORD;
begin
 Result:=0;
 if (fd<>0) and (fd<>INVALID_HANDLE_VALUE) then
 begin
  Result:=ntf2px(NtClose(fd));
 end;
end;

Function NtTruncate(FileHandle:THandle;IoStatusBlock:PIO_STATUS_BLOCK;SIZE:QWORD):DWORD; inline;
begin
 Result:=NtSetInformationFile(
          FileHandle,
          IoStatusBlock,
          @SIZE,
          SizeOf(Int64),
          FileEndOfFileInformation);

 if (Result<>0) then
 begin
  Result:=NtSetInformationFile(
           FileHandle,
           IoStatusBlock,
           @SIZE,
           SizeOf(Int64),
           FileAllocationInformation);
 end;
end;

Function NtMarkDelete(FileHandle:THandle;IoStatusBlock:PIO_STATUS_BLOCK):DWORD; inline;
var
 FBI:FILE_BASIC_INFORMATION;
 del_on_close:Boolean absolute FBI;
begin
 FBI:=Default(FILE_BASIC_INFORMATION);
 FBI.FileAttributes:=FILE_ATTRIBUTE_NORMAL;

 // reset read-only
 NtSetInformationFile(FileHandle,IoStatusBlock,@FBI,SizeOf(FBI),FileBasicInformation);

 //mark delete
 del_on_close:=True;
 Result:=NtSetInformationFile(FileHandle,IoStatusBlock,@del_on_close,1,FileDispositionInformation);
end;

Function md_create_swap_file(const path:RawByteString;SIZE:QWORD;Var fd:THandle):DWORD;
var
 W:WideString;

 OBJ:TOBJ_ATTR;
 BLK:IO_STATUS_BLOCK;

begin
 W:=UTF8Decode(path);
 W:='\??\'+W;

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,0,OBJ_CASE_INSENSITIVE,w);
 BLK:=Default(IO_STATUS_BLOCK);
 fd:=0;

 Result:=ntf2px(NtCreateFile(@fd,
                             FILE_READ_DATA or
                             FILE_WRITE_DATA or
                             FILE_APPEND_DATA or
                             FILE_READ_ATTRIBUTES or
                             FILE_WRITE_ATTRIBUTES or
                             FILE_CAN_DELETE or
                             SYNCHRONIZE,
                             @OBJ,
                             @BLK,
                             nil,
                             FILE_ATTRIBUTE_TEMPORARY,
                             0,
                             FILE_OVERWRITE_IF,
                             FILE_SYNCHRONOUS_IO_NONALERT or
                             FILE_OPEN_REPARSE_POINT or
                             FILE_NON_DIRECTORY_FILE or
                             FILE_DELETE_ON_CLOSE,
                             nil,
                             0));

 if (Result<>0) then Exit;

 Result:=ntf2px(NtTruncate(fd,@BLK,SIZE));

 if (Result<>0) then
 begin
  NtClose(fd);
  fd:=0;
 end;
end;

Function md_delete_file(const path:RawByteString):DWORD;
const
 FILE_SHARE_ALL=FILE_SHARE_READ or
                FILE_SHARE_WRITE or
                FILE_SHARE_DELETE;
var
 fd:THandle;

 W:WideString;

 OBJ:TOBJ_ATTR;
 BLK:IO_STATUS_BLOCK;
begin
 W:=UTF8Decode(path);
 W:='\??\'+W;

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,0,OBJ_CASE_INSENSITIVE,w);
 BLK:=Default(IO_STATUS_BLOCK);
 fd:=0;

 Result:=ntf2px(NtOpenFile(@fd,
                           SYNCHRONIZE or
                           FILE_CAN_DELETE or
                           FILE_READ_DATA or
                           FILE_READ_ATTRIBUTES or
                           FILE_WRITE_ATTRIBUTES,
                           @OBJ,
                           @BLK,
                           FILE_SHARE_ALL,
                           FILE_OPEN_FOR_BACKUP_INTENT or
                           FILE_SYNCHRONOUS_IO_NONALERT or
                           FILE_OPEN_REPARSE_POINT
 ));

 if (Result<>0) then Exit;

 Result:=ntf2px(NtMarkDelete(fd,@BLK));

 NtClose(fd); //<-actual delete
end;

function md_fstat(fd:THandle;sb:p_stat):Integer;
var
 FBI:FILE_BASIC_INFORMATION;
 FSI:FILE_STANDARD_INFORMATION;
 FII:FILE_INTERNAL_INFORMATION;
 BLK:IO_STATUS_BLOCK;
 R:DWORD;
begin
 if (fd=0) or (fd=INVALID_HANDLE_VALUE) or (sb=nil) then Exit(EINVAL);

 //load time and file type
 FBI:=Default(FILE_BASIC_INFORMATION);
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtQueryInformationFile(
     FD,
     @BLK,
     @FBI,
     SizeOf(FBI),
     FileBasicInformation
    );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 sb^.st_atim    :=get_unix_file_time(FBI.LastAccessTime);
 sb^.st_mtim    :=get_unix_file_time(FBI.LastWriteTime);
 sb^.st_ctim    :=get_unix_file_time(FBI.ChangeTime);
 sb^.st_birthtim:=get_unix_file_time(FBI.CreationTime);
 sb^.st_flags   :=FBI.FileAttributes;

 if ((FBI.FileAttributes and FILE_ATTRIBUTE_READONLY)<>0) then
 begin
  sb^.st_mode:=(&0777 and (not &0222));
 end else
 begin
  sb^.st_mode:=0777;
 end;

 //load size
 FSI:=Default(FILE_STANDARD_INFORMATION);
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtQueryInformationFile(
     FD,
     @BLK,
     @FSI,
     SizeOf(FSI),
     FileStandardInformation
    );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 sb^.st_nlink  :=FSI.NumberOfLinks;
 sb^.st_size   :=Int64(FSI.EndOfFile);
 sb^.st_blocks :=Int64(FSI.AllocationSize) div S_BLKSIZE;
 sb^.st_blksize:=S_BLKSIZE;

 //load inode
 FII:=Default(FILE_INTERNAL_INFORMATION);
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtQueryInformationFile(
     FD,
     @BLK,
     @FII,
     SizeOf(FII),
     FileInternalInformation
    );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 sb^.st_dev:=FII.IndexNumber.HighPart;
 sb^.st_ino:=FII.IndexNumber.LowPart;
end;

function md_stat(const path:RawByteString;sb:p_stat):Integer;
var
 fd:THandle;
begin
 if (sb=nil) then Exit(EINVAL);

 Result:=md_open(path,O_RDONLY,0,fd);
 if (Result<>0) then Exit;

 Result:=md_fstat(fd,sb);

 NtClose(fd);
end;

function md_futimens(fd:THandle;ts:p_timespec;numtimes:Integer):Integer;
var
 FBI:FILE_BASIC_INFORMATION;
 BLK:IO_STATUS_BLOCK;
 R:DWORD;
begin
 if (fd=0) or (fd=INVALID_HANDLE_VALUE) or (ts=nil) then Exit(EINVAL);

 //load time and file type
 FBI:=Default(FILE_BASIC_INFORMATION);
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtQueryInformationFile(
     FD,
     @BLK,
     @FBI,
     SizeOf(FBI),
     FileBasicInformation
    );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 FBI.LastAccessTime:=get_win_file_time(ts[0]);
 FBI.LastWriteTime :=get_win_file_time(ts[1]);

 if (numtimes < 3) and
    (QWORD(FBI.LastWriteTime) < QWORD(FBI.CreationTime)) then
 begin
  FBI.CreationTime:=FBI.LastWriteTime;
 end;

 if (numtimes > 2) then
 begin
  FBI.CreationTime:=get_win_file_time(ts[2]);
 end;

 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtSetInformationFile(
     FD,
     @BLK,
     @FBI,
     SizeOf(FBI),
     FileBasicInformation);

 Result:=ntf2px(R);
end;

function md_utimens(const path:RawByteString;ts:p_timespec;numtimes:Integer):Integer;
var
 fd:THandle;
begin
 if (ts=nil) then Exit(EINVAL);

 Result:=md_open(path,O_RDWR,0,fd);
 if (Result<>0) then Exit;

 Result:=md_futimens(fd,ts,numtimes);

 NtClose(fd);
end;



end.

