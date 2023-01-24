unit sys_dev;

{$mode ObjFPC}{$H+}

interface

uses
 Windows,
 Classes,
 SysUtils,
 RWLock,
 sys_kernel,
 sys_fd;

procedure _sys_dev_init;
function  _sys_dev_open(const path:RawByteString;flags,mode:Integer):Integer;
function  _sys_dev_stat(Const path:RawByteString;stat:PSceKernelStat):Integer;

implementation

type
 TDevFile=class(TCustomFile)
  Constructor Create;
  function lseek    (offset:Int64;whence:Integer):Int64; override;
  function ftruncate(size:Int64):Integer;                override;
  function fstat    (stat:PSceKernelStat):Integer;       override;
 end;

 TDevRandom=class(TDevFile)
  function read  (data:Pointer;size:Int64):Int64;                   override;
  function pread (data:Pointer;size,offset:Int64):Int64;            override;
  function readv (vector:p_iovec;count:Integer):Int64;              override;
  function preadv(vector:p_iovec;count:Integer;offset:Int64):Int64; override;
  function write (data:Pointer;size:Int64):Int64;                   override;
  function pwrite(data:Pointer;size,offset:Int64):Int64;            override;
  function writev(vector:p_iovec;count:Integer):Int64;              override;
 end;

 TDevStd=class(TDevFile)
  var
   RText,WText:PText;
   lock:TRWLock;
   cache:RawByteString;
  Constructor Create(r,w:PText);
  Destructor  Destroy; override;
  function read  (data:Pointer;size:Int64):Int64;                   override;
  function pread (data:Pointer;size,offset:Int64):Int64;            override;
  function readv (vector:p_iovec;count:Integer):Int64;              override;
  function preadv(vector:p_iovec;count:Integer;offset:Int64):Int64; override;
  function write (data:Pointer;size:Int64):Int64;                   override;
  function pwrite(data:Pointer;size,offset:Int64):Int64;            override;
  function writev(vector:p_iovec;count:Integer):Int64;              override;
  function ioctl(cmd:Integer;param1:ptruint):Integer;               override;
 end;

procedure _sys_dev_init;
begin
 _sys_dev_open('stdin' ,O_RDWR,0); //0
 _sys_dev_open('stdout',O_RDWR,0); //1
 _sys_dev_open('stderr',O_RDWR,0); //2
end;

function _sys_dev_open(const path:RawByteString;flags,mode:Integer):Integer;
var
 f:TCustomFile;
begin

 Case path of
  'stdin' :f:=TDevStd.Create(@Input,@StdOut);
  'stdout':f:=TDevStd.Create(@Input,@StdOut);
  'stderr':f:=TDevStd.Create(@Input,@StdErr);

  'random',
  'urandom':
    begin
     f:=TDevRandom.Create;
    end;
  else
   Exit(-ENOENT);
 end;

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

function _sys_dev_stat(Const path:RawByteString;stat:PSceKernelStat):Integer;
begin
 Result:=0;
 stat^:=Default(SceKernelStat);
 stat^.st_mode :=S_IFCHR;
 stat^.st_nlink:=1;
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

function TDevFile.ftruncate(size:Int64):Integer;
begin
 Result:=EACCES;
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

function TDevRandom.read  (data:Pointer;size:Int64):Int64;
begin
 if (data=nil) or (size=0) then Exit(0);
 Assert(size<High(DWORD));

 BCryptGenRandom(nil,data,size,BCRYPT_USE_SYSTEM_PREFERRED_RNG);
 Result:=size;
end;

function TDevRandom.pread (data:Pointer;size,offset:Int64):Int64;
begin
 Result:=read(data,size);
end;

function TDevRandom.readv (vector:p_iovec;count:Integer):Int64;
var
 i:Integer;
begin
 Result:=0;

 For i:=0 to count-1 do
 begin
  Result:=Result+read(vector[i].iov_base,vector[i].iov_len);
 end;
end;

function TDevRandom.preadv(vector:p_iovec;count:Integer;offset:Int64):Int64;
begin
 Result:=readv(vector,count);
end;

function TDevRandom.write (data:Pointer;size:Int64):Int64;
begin
 Result:=size;
end;

function TDevRandom.pwrite(data:Pointer;size,offset:Int64):Int64;
begin
 Result:=size;
end;

function TDevRandom.writev(vector:p_iovec;count:Integer):Int64;
var
 i:Integer;
begin
 Result:=0;

 For i:=0 to count-1 do
 begin
  Result:=Result+vector[i].iov_len;
 end;
end;

//

Constructor TDevStd.Create(r,w:PText);
type
 PTextRec=^TextRec;
begin
 Handle:=PTextRec(w)^.Handle;
 RText:=r;
 WText:=w;
 rwlock_init(lock);
 cache:='';
end;

Destructor TDevStd.Destroy;
begin
 rwlock_destroy(lock);
 cache:='';
end;

function TDevStd.read  (data:Pointer;size:Int64):Int64;
var
 S:RawByteString;
begin
 if (data=nil) or (size=0) then Exit(0);
 rwlock_wrlock(lock);
  if (Length(cache)<>0) then
  begin
   if (size>Length(cache)) then
   begin
    size:=Length(cache);
   end;

   Move(PChar(cache)^,data^,size);
   Delete(cache,1,size);

   rwlock_unlock(lock);
   Exit(size);
  end;
 rwlock_unlock(lock);

 S:='';
 System.ReadLn(RText^,S);
 S:=S+#10;

 if (Length(S)>size) then
 begin
  Move(PChar(S)^,data^,size);
  Result:=size;

  Delete(S,1,size);

  rwlock_wrlock(lock);
   cache:=cache+S;
  rwlock_unlock(lock);
 end else
 begin
  size:=Length(S);
  Move(PChar(S)^,data^,size);
  Result:=size;
 end;
end;

function TDevStd.pread (data:Pointer;size,offset:Int64):Int64;
begin
 Result:=read(data,size);
end;

function TDevStd.readv (vector:p_iovec;count:Integer):Int64;
var
 i,n:Integer;
begin
 Result:=0;

 For i:=0 to count-1 do
 begin
  n:=read(vector[i].iov_base,vector[i].iov_len);

  if (n>=0) then
  begin
   Result:=Result+n;
   if (n<vector[i].iov_len) then Exit;
  end else
  begin
   Exit(-EIO);
   Break;
  end;

 end;
end;

function TDevStd.preadv(vector:p_iovec;count:Integer;offset:Int64):Int64;
begin
 Result:=readv(vector,count);
end;

function TDevStd.write (data:Pointer;size:Int64):Int64;
var
 S:RawByteString;
begin
 SetString(S,data,size);
 System.Write(WText^,S);
 Result:=size;
end;

function TDevStd.pwrite(data:Pointer;size,offset:Int64):Int64;
begin
 Result:=write(data,size);
end;

function TDevStd.writev(vector:p_iovec;count:Integer):Int64;
var
 i,n:Integer;
begin
 Result:=0;

 For i:=0 to count-1 do
 begin
  n:=write(vector[i].iov_base,vector[i].iov_len);

  if (n>=0) then
  begin
   Result:=Result+n;
   if (n<vector[i].iov_len) then Exit;
  end else
  begin
   Exit(-EIO);
   Break;
  end;

 end;
end;

function TDevStd.ioctl(cmd:Integer;param1:ptruint):Integer;
begin
 Assert(false,'TODO:ioctl:'+HexStr(cmd,8));
 Result:=0;
end;

//

end.

