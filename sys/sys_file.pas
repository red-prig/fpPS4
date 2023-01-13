unit sys_file;

{$mode ObjFPC}{$H+}

interface

uses
 Windows,
 ntapi,
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
begin
 Result:=0;
 if (flags and O_CREAT)<>0 then
 begin
  if (flags and O_EXCL)<>0 then
  begin
   Result:=CREATE_NEW;
  end else
  if (flags and O_TRUNC)<>0 then
  begin
   Result:=CREATE_ALWAYS;
  end else
  begin
   Result:=OPEN_ALWAYS;
  end;
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

 if ((flags and O_NONBLOCK)<>0) then
 begin
  Writeln(StdErr,'__sys_file_open:O_NONBLOCK:TODO');
 end;

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

function TFile.lseek(offset:Int64;whence:Integer):Int64;
label
 _set_pos;
var
 s:IO_STATUS_BLOCK;
 i:FILE_STANDARD_INFORMATION;
 e:Integer;
begin
 s:=Default(IO_STATUS_BLOCK);

 rwlock_wrlock(lock);

  Case whence of
   0:begin //beg
      _set_pos:

      e:=NtSetInformationFile(Handle,@s,@offset,SizeOf(Int64),FilePositionInformation);
     end;
   1:begin //cur
      i.EndOfFile.QuadPart:=0;
      e:=NtQueryInformationFile(Handle,@s,@i.EndOfFile,SizeOf(Int64),FilePositionInformation);
      if (e>=0) then
      begin
       offset:=offset+i.EndOfFile.QuadPart;
       goto _set_pos;
      end;
     end;
   2:begin //end
      i.EndOfFile.QuadPart:=0;
      e:=NtQueryInformationFile(Handle,@s,@i,SizeOf(i),FileStandardInformation);
      if (e>=0) then
      begin
       offset:=offset+i.EndOfFile.QuadPart;
       goto _set_pos;
      end;
     end;
   else
     e:=Integer(STATUS_INVALID_PARAMETER);
  end;

 rwlock_unlock(lock);

 e:=-ntf2px(e);

 if (e<>0) then
 begin
  Result:=e;
 end else
 begin
  Result:=offset;
 end;
end;

function TFile.read(data:Pointer;size:Int64):Int64;
var
 s:IO_STATUS_BLOCK;
 e:Integer;
begin
 s:=Default(IO_STATUS_BLOCK);

 rwlock_wrlock(lock);

  e:=NtReadFile(Handle,0,nil,nil,@s,data,size,nil,nil);
  if (e=STATUS_PENDING) then e:=NtWaitForSingleObject(Handle,False,nil);

 rwlock_unlock(lock);

 e:=-ntf2px(e);

 if (e<>0) then
 begin
  Result:=e;
 end else
 begin
  Result:=Int64(s.Information);
 end;
end;

{
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
}

function TFile.pread(data:Pointer;size,offset:Int64):Int64;
var
 s:IO_STATUS_BLOCK;
 e:Integer;
begin
 s:=Default(IO_STATUS_BLOCK);

 rwlock_wrlock(lock);
  //---------------------
  ///p:=_get_pos(Handle);
  //---------------------

  e:=NtReadFile(Handle,0,nil,nil,@s,data,size,@offset,nil);
  if (e=STATUS_PENDING) then e:=NtWaitForSingleObject(Handle,False,nil);

  //---------------------------------------------
  //if not _set_pos(Handle,p) then Assert(False);
  //---------------------------------------------
 rwlock_unlock(lock);

 e:=-ntf2px(e);

 if (e<>0) then
 begin
  Result:=e;
 end else
 begin
  Result:=Int64(s.Information);
 end;
end;

function TFile.readv(vector:p_iovec;count:Integer):Int64;
label
 _exit;
var
 s:IO_STATUS_BLOCK;
 e:Integer;
 i:Integer;
 v:iovec;
begin
 Result:=0;

 rwlock_wrlock(lock);

  For i:=0 to count-1 do
  begin
   v:=vector[i];

   if (v.iov_base<>nil) and (v.iov_len<>0) then
   begin

    s:=Default(IO_STATUS_BLOCK);
    e:=NtReadFile(Handle,0,nil,nil,@s,v.iov_base,v.iov_len,nil,nil);
    if (e=STATUS_PENDING) then e:=NtWaitForSingleObject(Handle,False,nil);

    e:=-ntf2px(e);

    if (e<>0) then
    begin
     Result:=e;
     Goto _exit;
    end else
    begin
     Result:=Result+Int64(s.Information);
     if (Int64(s.Information)<v.iov_len) then
     begin
      Goto _exit;
     end;
    end;

   end;
  end;

 _exit:
  rwlock_unlock(lock);
end;

function TFile.preadv(vector:p_iovec;count:Integer;offset:Int64):Int64;
label
 _exit;
var
 s:IO_STATUS_BLOCK;
 e:Integer;
 i:Integer;
 v:iovec;
 poffset:PLARGE_INTEGER;
begin
 Result:=0;

 poffset:=@offset; //first move to offset

 rwlock_wrlock(lock);

  //--------------------------------------------------
  //p:=_get_pos(Handle);
  //if not _set_pos(Handle,offset) then Assert(False);
  //--------------------------------------------------

  For i:=0 to count-1 do
  begin
   v:=vector[i];

   if (v.iov_base<>nil) and (v.iov_len<>0) then
   begin

    s:=Default(IO_STATUS_BLOCK);
    e:=NtReadFile(Handle,0,nil,nil,@s,v.iov_base,v.iov_len,poffset,nil);
    if (e=STATUS_PENDING) then e:=NtWaitForSingleObject(Handle,False,nil);

    poffset:=nil; //reset

    e:=-ntf2px(e);

    if (e<>0) then
    begin
     Result:=e;
     Goto _exit;
    end else
    begin
     Result:=Result+Int64(s.Information);
     if (Int64(s.Information)<v.iov_len) then
     begin
      Goto _exit;
     end;
    end;

   end;
  end;

 _exit:
  //--------------------------------------------------
  //if not _set_pos(Handle,p) then Assert(False);
  //--------------------------------------------------
  rwlock_unlock(lock);
end;

function TFile.write(data:Pointer;size:Int64):Int64;
var
 s:IO_STATUS_BLOCK;
 e:Integer;
begin
 s:=Default(IO_STATUS_BLOCK);

 rwlock_wrlock(lock);

  e:=NtWriteFile(Handle,0,nil,nil,@s,data,size,nil,nil);
  if (e=STATUS_PENDING) then e:=NtWaitForSingleObject(Handle,False,nil);

 rwlock_unlock(lock);

 e:=-ntf2px(e);

 if (e<>0) then
 begin
  Result:=e;
 end else
 begin
  Result:=Int64(s.Information);
 end;
end;

function TFile.pwrite(data:Pointer;size,offset:Int64):Int64;
var
 s:IO_STATUS_BLOCK;
 e:Integer;
begin
 s:=Default(IO_STATUS_BLOCK);

 rwlock_wrlock(lock);
  //---------------------
  ///p:=_get_pos(Handle);
  //---------------------

  e:=NtWriteFile(Handle,0,nil,nil,@s,data,size,@offset,nil);
  if (e=STATUS_PENDING) then e:=NtWaitForSingleObject(Handle,False,nil);

  //---------------------------------------------
  //if not _set_pos(Handle,p) then Assert(False);
  //---------------------------------------------
 rwlock_unlock(lock);

 e:=-ntf2px(e);

 if (e<>0) then
 begin
  Result:=e;
 end else
 begin
  Result:=Int64(s.Information);
 end;
end;

function TFile.ftruncate(size:Int64):Integer;
var
 s:IO_STATUS_BLOCK;
 e:Integer;
begin
 s:=Default(IO_STATUS_BLOCK);

 rwlock_wrlock(lock);

  e:=NtSetInformationFile(Handle,@s,@size,SizeOf(Int64),FileEndOfFileInformation);
  if (e>=0) then
  begin
   e:=NtSetInformationFile(Handle,@s,@size,SizeOf(Int64),FileAllocationInformation);
  end;

 rwlock_unlock(lock);

 Result:=ntf2px(e);
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



