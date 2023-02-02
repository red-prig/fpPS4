unit sys_dev;

{$mode ObjFPC}{$H+}

interface

uses
 Windows,
 Classes,
 SysUtils,
 RWLock,
 sys_kernel,
 sys_crt,
 sys_fd,
 sys_dir;

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

 TDirCharDev=class(TDirFile)
  function fstat(stat:PSceKernelStat):Integer; override;
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

 TDevRng=class(TDevRandom)
  function ioctl (cmd:Integer;param1:ptruint):Integer;              override;
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

function _sys_dev_dir_open(const path:RawByteString;flags,mode:Integer):Integer;
var
 f:TDirCharDev;

begin
 Result:=0;

 f:=TDirCharDev.Create;
 f.path:=path;

 SetLength(f.dirs,0);

 f.add_dir('.');
 f.add_dir('..');
 f.add_dir('ajm'              );
 f.add_dir('bpf'              );
 f.add_dir('bpf0'             );
 f.add_dir('camera'           );
 f.add_dir('console'          );
 f.add_dir('ctty'             );
 f.add_dir('da0x6x1.crypt'    );
 f.add_dir('da0x6x2.crypt'    );
 f.add_dir('dbggc'            );
 f.add_dir('dce'              );
 f.add_dir('deci_stderr'      );
 f.add_dir('deci_stdin'       );
 f.add_dir('deci_stdout'      );
 f.add_dir('deci_tty2'        );
 f.add_dir('deci_tty3'        );
 f.add_dir('deci_tty4'        );
 f.add_dir('deci_tty5'        );
 f.add_dir('deci_tty6'        );
 f.add_dir('deci_tty7'        );
 f.add_dir('deci_ttya0'       );
 f.add_dir('deci_ttyb0'       );
 f.add_dir('deci_ttyc0'       );
 f.add_dir('dipsw'            );
 f.add_dir('dmem0'            );
 f.add_dir('dmem1'            );
 f.add_dir('fd'               );
 f.add_dir('gc'               );
 f.add_dir('hid'              );
 f.add_dir('icc_configuration');
 f.add_dir('icc_device_power' );
 f.add_dir('icc_indicator'    );
 f.add_dir('icc_nvs'          );
 f.add_dir('icc_power'        );
 f.add_dir('notification0'    );
 f.add_dir('notification1'    );
 f.add_dir('null'             );
 f.add_dir('random'           );
 f.add_dir('rng'              );
 f.add_dir('sce_zlib'         );
 f.add_dir('srtc'             );
 f.add_dir('stderr'           );
 f.add_dir('stdin'            );
 f.add_dir('stdout'           );
 f.add_dir('ugen0.4'          );
 f.add_dir('urandom'          );
 f.add_dir('usb'              );
 f.add_dir('usbctl'           );
 f.add_dir('uvd'              );
 f.add_dir('vce'              );
 f.add_dir('zero'             );

 Result:=_sys_open_fd(f,flags);

 if (Result<0) then
 begin
  f.Destroy;
 end else
 begin
  f.Release;
 end;
end;

function _sys_dev_char_open(const path:RawByteString;flags,mode:Integer):Integer;
var
 f:TCustomFile;
begin
 if (flags and O_DIRECTORY)<>0 then
 begin
  Exit(-ENOTDIR);
 end;

 Case path of
  'stdin' :f:=TDevStd.Create(@Input,@StdOut);
  'stdout':f:=TDevStd.Create(@Input,@StdOut);
  'stderr':f:=TDevStd.Create(@Input,@StdErr);

  'random',
  'urandom':
    begin
     f:=TDevRandom.Create;
    end;

  'rng':
    begin
     f:=TDevRng.Create;
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

function _sys_dev_open(const path:RawByteString;flags,mode:Integer):Integer;
begin
 if (path='') then
 begin
  Result:=_sys_dev_dir_open(path,flags,mode);
 end else
 begin
  Result:=_sys_dev_char_open(path,flags,mode);
 end;
end;

//

function _sys_dev_dir_stat(Const path:RawByteString;stat:PSceKernelStat):Integer;
begin
 stat^:=Default(SceKernelStat);

 stat^.st_mode    :=S_IFDIR;
 stat^.st_size    :=1;
 stat^.st_nlink   :=1;

 stat^.st_atim.tv_sec    :=1;
 stat^.st_mtim.tv_sec    :=1;
 stat^.st_ctim.tv_sec    :=1;
 stat^.st_birthtim.tv_sec:=1;

 stat^.st_blocks  :=0;
 stat^.st_blksize :=SizeOf(dirent);

 Result:=0;
end;

function _sys_dev_char_stat(Const path:RawByteString;stat:PSceKernelStat):Integer;
begin
 stat^:=Default(SceKernelStat);

 stat^.st_mode    :=S_IFCHR;
 stat^.st_size    :=1;
 stat^.st_nlink   :=1;

 stat^.st_atim.tv_sec    :=1;
 stat^.st_mtim.tv_sec    :=1;
 stat^.st_ctim.tv_sec    :=1;
 stat^.st_birthtim.tv_sec:=1;

 stat^.st_blocks  :=0;
 stat^.st_blksize :=512;

 Result:=0;
end;

function _sys_dev_stat(Const path:RawByteString;stat:PSceKernelStat):Integer;
begin
 Case path of
  '':Result:=_sys_dev_dir_stat(path,stat);

  'ajm',
  'bpf',
  'bpf0',
  'camera',
  'console',
  'ctty',
  'da0x6x1.crypt',
  'da0x6x2.crypt',
  'dbggc',
  'dce',
  'deci_stderr',
  'deci_stdin',
  'deci_stdout',
  'deci_tty2',
  'deci_tty3',
  'deci_tty4',
  'deci_tty5',
  'deci_tty6',
  'deci_tty7',
  'deci_ttya0',
  'deci_ttyb0',
  'deci_ttyc0',
  'dipsw',
  'dmem0',
  'dmem1',
  'fd',
  'gc',
  'hid',
  'icc_configuration',
  'icc_device_power',
  'icc_indicator',
  'icc_nvs',
  'icc_power',
  'notification0',
  'notification1',
  'null',
  'random',
  'rng',
  'sce_zlib',
  'srtc',
  'ugen0.4',
  'urandom',
  'usb',
  'usbctl',
  'uvd',
  'vce',
  'zero':
   begin
    Result:=_sys_dev_char_stat(path,stat);
   end

  else
   Result:=ENOENT;

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

function TDevFile.ftruncate(size:Int64):Integer;
begin
 Result:=EACCES;
end;

function TDevFile.fstat(stat:PSceKernelStat):Integer;
begin
 Result:=_sys_dev_char_stat('',stat);
 if (Result=0) then
 begin
  stat^.st_dev :=fd;
  stat^.st_rdev:=fd;
 end;
end;

function TDirCharDev.fstat(stat:PSceKernelStat):Integer;
begin
 Result:=_sys_dev_dir_stat(path,stat);
 if (Result=0) then
 begin
  stat^.st_dev :=fd;
  stat^.st_rdev:=fd;
 end;
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

function TDevRng.ioctl(cmd:Integer;param1:ptruint):Integer;
begin
 case cmd of
  $40445301, //_get_genuine_random
  $40445302: //Fips186Prng
  begin
   if (param1<$1000) then Exit(EFAULT);
   Result:=BCryptGenRandom(nil,Pointer(param1),64,BCRYPT_USE_SYSTEM_PREFERRED_RNG);
   if (Result<>0) then Result:=EFAULT;
  end;
  else
   Result:=EINVAL;
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
begin
 CrtOutWriteDirect(WText,data,size);
 Result:=size;
end;

function TDevStd.pwrite(data:Pointer;size,offset:Int64):Int64;
begin
 CrtOutWriteDirect(WText,data,size);
 Result:=size;
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

