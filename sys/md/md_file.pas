unit md_file;

{$mode ObjFPC}{$H+}

interface

uses
 ntapi,
 windows,
 vfcntl,
 vstat;

function  md_openat(at_fd:THandle;const path:RawByteString;flags,mode:DWORD;Var fd:THandle):DWORD;
function  md_open  (const path:RawByteString;flags,mode:DWORD;Var fd:THandle):DWORD;
function  md_close (fd:THandle):DWORD;

Function  md_create_swap_file(const path:RawByteString;SIZE:QWORD;Var fd:THandle):DWORD;
Function  md_delete_file     (const path:RawByteString):DWORD;

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

Function GetDesiredAccess(flags:Integer):DWORD; inline;
begin
 Result:=SYNCHRONIZE or
         FILE_READ_ATTRIBUTES or
         FILE_WRITE_ATTRIBUTES;

 if ((flags and O_RDWR)<>0) then
 begin
  Result:=Result or (FILE_READ_DATA or FILE_WRITE_DATA or FILE_APPEND_DATA);
 end else
 if ((flags and O_WRONLY)<>0) then
 begin
  Result:=Result or (FILE_WRITE_DATA or FILE_APPEND_DATA);
 end else
 begin
  Result:=Result or FILE_READ_DATA;
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
 Result:=FILE_SYNCHRONOUS_IO_NONALERT or
         FILE_NON_DIRECTORY_FILE;
 if ((flags and (O_FSYNC or O_DSYNC))<>0) then
 begin
  Result:=Result or FILE_WRITE_THROUGH;
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
  at_fd:=0;

  SetLength(w,GetCurrentDirectoryW(0,nil));
  GetCurrentDirectoryW(Length(w),@w[1]);
  SetLength(w,Length(w)-1);

  w:='\??\'+w+'\';
 end else
 if (at_fd=0) or (at_fd=INVALID_HANDLE_VALUE) then
 begin
  at_fd:=0;
  w:='\??\';
 end;

 w:=w+UTF8Decode(path);

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,at_fd,OBJ_CASE_INSENSITIVE,w);
 BLK:=Default(IO_STATUS_BLOCK);

 DA:=GetDesiredAccess(flags);
 FA:=GetFileAttrtibute(flags,mode);
 CD:=GetCreationDisposition(flags);
 CO:=GetCreateOptions(flags);

 Result:=NtCreateFile(@fd,
                      DA,
                      @OBJ,
                      @BLK,
                      nil,
                      FA,
                      FILE_SHARE_ALL,
                      CD,
                      CO,
                      nil,
                      0);

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
  Result:=NtClose(fd);
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

 Result:=NtCreateFile(@fd,
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
                      0);

 if (Result<>0) then Exit;

 Result:=NtTruncate(fd,@BLK,SIZE);

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

 Result:=NtOpenFile(@fd,
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
 );

 if (Result<>0) then Exit;

 Result:=NtMarkDelete(fd,@BLK);

 NtClose(fd); //<-actual delete
end;

end.

