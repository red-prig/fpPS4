unit md_pipe;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 windows,
 ntapi;

Const
 MD_PIPE_ASYNC0=1;
 MD_PIPE_ASYNC1=2;

function md_pipe2(pipefd:PHandle;flags:Integer):Integer;

implementation

const
 FILE_SHARE_VALID_FLAGS=FILE_SHARE_READ or
                        FILE_SHARE_WRITE or
                        FILE_SHARE_DELETE;

Const
 NamedPipe:PWideChar='\Device\NamedPipe\';

function md_pipe2(pipefd:PHandle;flags:Integer):Integer;
var
 BLK:IO_STATUS_BLOCK;
 UNAME:UNICODE_STRING;
 OATTR:OBJECT_ATTRIBUTES;
 timeout:LARGE_INTEGER;

 hDirectory:THandle;
 hFd:array[0..1] of THandle;
begin
 Result:=0;

 BLK:=Default(IO_STATUS_BLOCK);

 UNAME:=Default(UNICODE_STRING);
 UNAME.Length       :=Length(NamedPipe)*SizeOf(WideChar);
 UNAME.MaximumLength:=(Length(NamedPipe)+1)*SizeOf(WideChar);
 UNAME.Buffer       :=NamedPipe;

 OATTR:=Default(OBJECT_ATTRIBUTES);
 OATTR.Length    :=SizeOf(OBJECT_ATTRIBUTES);
 OATTR.ObjectName:=@UNAME;
 OATTR.Attributes:=OBJ_CASE_INSENSITIVE;

 hDirectory:=0;
 Result:=NtOpenFile(@hDirectory,
                    SYNCHRONIZE,
                    @OATTR,
                    @BLK,
                    FILE_SHARE_VALID_FLAGS,
                    0);
 if (Result<>0) then Exit;

 timeout.QuadPart:=-500000;

 BLK:=Default(IO_STATUS_BLOCK);

 UNAME:=Default(UNICODE_STRING);

 OATTR.RootDirectory:=hDirectory;

 hFd[0]:=0;
 Result:=NtCreateNamedPipeFile(@hFd[0],
                               SYNCHRONIZE or
                               FILE_READ_ATTRIBUTES  or FILE_READ_DATA  or
                               FILE_WRITE_ATTRIBUTES or FILE_WRITE_DATA or
                               FILE_CREATE_PIPE_INSTANCE,
                               @OATTR,
                               @BLK,
                               FILE_SHARE_READ or FILE_SHARE_WRITE,
                               FILE_CREATE,
                               (ord((flags and MD_PIPE_ASYNC0)=0)*FILE_SYNCHRONOUS_IO_NONALERT),
                               FILE_PIPE_BYTE_STREAM_TYPE,
                               FILE_PIPE_BYTE_STREAM_MODE,
                               FILE_PIPE_QUEUE_OPERATION,
                               $ffffffff,
                               0,
                               0,
                               @timeout);
 if (Result<>0) then
 begin
  NtClose(hDirectory);
  Exit;
 end;

 BLK:=Default(IO_STATUS_BLOCK);

 OATTR.RootDirectory:=hFd[0];
 OATTR.Attributes:=OBJ_CASE_INSENSITIVE or OBJ_INHERIT;

 hFd[1]:=0;
 Result:=NtOpenFile(@hFd[1],
                    SYNCHRONIZE or
                    FILE_READ_ATTRIBUTES  or FILE_READ_DATA or
                    FILE_WRITE_ATTRIBUTES or FILE_WRITE_DATA,
                    @OATTR,
                    @BLK,
                    FILE_SHARE_VALID_FLAGS,
                    (ord((flags and MD_PIPE_ASYNC1)=0)*FILE_SYNCHRONOUS_IO_NONALERT)
                   );

 if (Result<>0) then
 begin
  NtClose(hFd[0]);
  NtClose(hDirectory);
  Exit;
 end;

 NtClose(hDirectory);

 pipefd[0]:=hFd[0];
 pipefd[1]:=hFd[1];
end;

end.

