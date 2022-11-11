unit sys_dev;

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

function _sys_dev_open(const path:RawByteString;flags,mode:Integer):Integer;

implementation

type
 TDevFile=class(TCustomFile)
  Constructor Create;
  function lseek (offset:Int64;whence:Integer):Int64;    override;
  function fstat (stat:PSceKernelStat):Integer;          override;
 end;

 TDevRandom=class(TDevFile)
  function read  (data:Pointer;size:Int64):Int64;        override;
  function pread (data:Pointer;size,offset:Int64):Int64; override;
  function readv (vector:p_iovec;count:Integer):Int64;   override;
  function write (data:Pointer;size:Int64):Int64;        override;
  function pwrite(data:Pointer;size,offset:Int64):Int64; override;
 end;

 TDevStd=class(TDevFile)
  Text:PText;
  cache:TMemoryStream;
  Constructor Create(t:PText);
  Destructor  Destroy; override;
  function read  (data:Pointer;size:Int64):Int64;        override;
  function pread (data:Pointer;size,offset:Int64):Int64; override;
  function readv (vector:p_iovec;count:Integer):Int64;   override;
  function write (data:Pointer;size:Int64):Int64;        override;
  function pwrite(data:Pointer;size,offset:Int64):Int64; override;
 end;

function _sys_dev_open(const path:RawByteString;flags,mode:Integer):Integer;
var
 f:TCustomFile;
begin

 Case path of
  'stdin' :f:=TDevStd.Create(@Input);
  'stdout':f:=TDevStd.Create(@StdOut);
  'stderr':f:=TDevStd.Create(@StdErr);

  'random',
  'urandom':
    begin
     f:=TDevRandom.Create;
    end;
  else
   Exit(-ENOENT);
 end;

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

Constructor TDevFile.Create;
begin
 Handle:=INVALID_HANDLE_VALUE;
end;

function TDevFile.lseek (offset:Int64;whence:Integer):Int64;
begin
 Result:=-ESPIPE;
end;

function TDevFile.fstat (stat:PSceKernelStat):Integer;
begin
 Result:=0;
 stat^:=Default(SceKernelStat);
 stat^.st_dev  :=fd;
 stat^.st_rdev :=fd;
 stat^.st_mode :=S_IFCHR;
 stat^.st_nlink:=1;
end;

//

const
 BCRYPT_USE_SYSTEM_PREFERRED_RNG=2;

function BCryptGenRandom(hAlgorithm:Pointer;
                         pbBuffer:PByte;
                         cbBuffer:DWORD;
                         dwFlags:DWORD):DWORD; stdcall; external 'Bcrypt';

function TDevRandom.read  (data:Pointer;size:Int64):Int64;
begin
 Assert(size<High(DWORD));

 BCryptGenRandom(nil,data,size,BCRYPT_USE_SYSTEM_PREFERRED_RNG);
 Result:=size;
end;

function TDevRandom.pread (data:Pointer;size,offset:Int64):Int64;
begin
 Assert(size<High(DWORD));

 BCryptGenRandom(nil,data,size,BCRYPT_USE_SYSTEM_PREFERRED_RNG);
 Result:=size;
end;

function TDevRandom.readv (vector:p_iovec;count:Integer):Int64;
var
 i:Integer;
begin
 Result:=0;

 For i:=0 to count-1 do
 begin
  Assert(vector[i].iov_len<High(DWORD));

  BCryptGenRandom(nil,vector[i].iov_base,vector[i].iov_len,BCRYPT_USE_SYSTEM_PREFERRED_RNG);
  Result:=Result+vector[i].iov_len;
 end;
end;

function TDevRandom.write (data:Pointer;size:Int64):Int64;
begin
 Result:=size;
end;

function TDevRandom.pwrite(data:Pointer;size,offset:Int64):Int64;
begin
 Result:=size;
end;

//

Constructor TDevStd.Create(t:PText);
type
 PTextRec=^TextRec;
begin
 Handle:=PTextRec(t)^.Handle;
 Text:=t;
 cache:=TMemoryStream.Create
end;

Destructor TDevStd.Destroy;
begin
 FreeAndNil(cache);
end;

function TDevStd.read  (data:Pointer;size:Int64):Int64;
begin
 //
end;

function TDevStd.pread (data:Pointer;size,offset:Int64):Int64;
begin
 //
end;

function TDevStd.readv (vector:p_iovec;count:Integer):Int64;
begin
 //
end;

function TDevStd.write (data:Pointer;size:Int64):Int64;
begin
 //
end;

function TDevStd.pwrite(data:Pointer;size,offset:Int64):Int64;
begin
 //
end;

end.

