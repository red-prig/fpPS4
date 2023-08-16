unit vfcntl;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
{
 * File status flags: these are used by open(2), fcntl(2).
 * They are also used (indirectly) in the kernel file structure f_flags,
 * which is a superset of the open/fcntl flags.  Open flags and f_flags
 * are inter-convertible using OFLAGS(fflags) and FFLAGS(oflags).
 * Open/fcntl flags begin with O_; kernel-internal flags begin with F.
 }
{ open-only flags }
 O_RDONLY =$0000; { open for reading only }
 O_WRONLY =$0001; { open for writing only }
 O_RDWR   =$0002; { open for reading and writing }
 O_ACCMODE=$0003; { mask for above modes }

{
 * Kernel encoding of open mode; separate read and write bits that are
 * independently testable: 1 greater than the above.
 *
 * XXX
 * FREAD and FWRITE are excluded from the #ifdef _KERNEL so that TIOCFLUSH,
 * which was documented to use FREAD/FWRITE, continues to work.
 }
 FREAD  =$0001;
 FWRITE =$0002;

 O_NONBLOCK=$0004;  { no delay }
 O_APPEND  =$0008;  { set append mode }

 O_SHLOCK=$0010;  { open with shared file lock }
 O_EXLOCK=$0020;  { open with exclusive file lock }
 O_ASYNC =$0040;  { signal pgrp when data ready }
 O_FSYNC =$0080;  { synchronous writes }

 O_SYNC    =$0080;  { POSIX synonym for O_FSYNC }
 O_NOFOLLOW=$0100;  { don't follow symlinks }
 O_CREAT   =$0200;  { create if nonexistent }
 O_TRUNC   =$0400;  { truncate to zero length }
 O_EXCL    =$0800;  { error if already exists }
 O_DSYNC   =$1000;  { synchronous data writes(omit inode writes) }
 FHASLOCK  =$4000;  { descriptor holds advisory lock }

{ Defined by POSIX 1003.1; BSD default, but must be distinct from O_RDONLY. }
 O_NOCTTY=$8000;  { don't assign controlling terminal }

{ Attempt to bypass buffer cache }
 O_DIRECT=$00010000;

{ Defined by POSIX Extended API Set Part 2 }
 O_DIRECTORY=$00020000; { Fail if not directory }
 O_EXEC     =$00040000; { Open for execute only }

 FEXEC=O_EXEC;

 O_TTY_INIT=$00080000; { Restore default termios attributes }
 O_CLOEXEC =$00100000;

 {
  * The O_* flags used to have only F* names, which were used in the kernel
  * and by fcntl.  We retain the F* names for the kernel f_flag field
  * and for backward compatibility for fcntl.  These flags are deprecated.
  }
 FAPPEND  =O_APPEND;   { kernel/compat }
 FASYNC   =O_ASYNC;    { kernel/compat }
 FFSYNC   =O_FSYNC;    { kernel }
 FDSYNC   =O_DSYNC;    { kernel }
 FNONBLOCK=O_NONBLOCK; { kernel }
 FNDELAY  =O_NONBLOCK; { compat }
 O_NDELAY =O_NONBLOCK; { compat }

 FRDAHEAD =O_CREAT;

{ bits to save after open }
 FMASK=(FREAD or FWRITE or FAPPEND or FASYNC or FFSYNC or FDSYNC or FNONBLOCK or O_DIRECT or FEXEC);
{ bits settable by fcntl(F_SETFL, ...) }
 FCNTLFLAGS=(FAPPEND or FASYNC or FFSYNC or FDSYNC or FNONBLOCK or FRDAHEAD or O_DIRECT);

{ Defined by POSIX Extended API Set Part 2 }
{
 * Magic value that specify the use of the current working directory
 * to determine the target of relative file paths in the openat() and
 * similar syscalls.
 }
 AT_FDCWD=-100;

{
 * Miscellaneous flags for the *at() syscalls.
 }
 AT_EACCESS         =$100; { Check access using effective user and group ID }
 AT_SYMLINK_NOFOLLOW=$200; { Do not follow symbolic links }
 AT_SYMLINK_FOLLOW  =$400; { Follow symbolic link }
 AT_REMOVEDIR       =$800; { Remove directory instead of file }

{
 * Constants used for fcntl(2)
 }
{ command values }
 F_DUPFD  = 0; { duplicate file descriptor }
 F_GETFD  = 1; { get file descriptor flags }
 F_SETFD  = 2; { set file descriptor flags }

 F_GETFL  = 3; { get file status flags }
 F_SETFL  = 4; { set file status flags }

 F_GETOWN = 5; { get SIGIO/SIGURG proc/pgrp }
 F_SETOWN = 6; { set SIGIO/SIGURG proc/pgrp }

 F_OGETLK = 7; { get record locking information }
 F_OSETLK = 8; { set record locking information }
 F_OSETLKW= 9; { F_SETLK; wait if blocked }
 F_DUP2FD =10; { duplicate file descriptor to arg }

 F_GETLK  =11; { get record locking information }
 F_SETLK  =12; { set record locking information }
 F_SETLKW =13; { F_SETLK; wait if blocked }

 F_SETLK_REMOTE  =14; { debugging support for remote locks }
 F_READAHEAD     =15; { read ahead }
 F_RDAHEAD       =16; { Darwin compatible read ahead }
 F_DUPFD_CLOEXEC =17; { Like F_DUPFD, but FD_CLOEXEC is set }
 F_DUP2FD_CLOEXEC=18; { Like F_DUP2FD, but FD_CLOEXEC is set }

{ file descriptor flags (F_GETFD, F_SETFD) }
 FD_CLOEXEC=1;  { close-on-exec flag }

{ record locking flags (F_GETLK, F_SETLK, F_SETLKW) }
 F_RDLCK   =1; { shared or read lock }
 F_UNLCK   =2; { unlock }
 F_WRLCK   =3; { exclusive or write lock }
 F_UNLCKSYS=4; { purge locks for a given system ID }
 F_CANCEL  =5; { cancel an async lock request }

 F_WAIT  =$010; { Wait until lock is granted }
 F_FLOCK =$020; { Use flock(2) semantics for lock }
 F_POSIX =$040; { Use POSIX semantics for lock }
 F_REMOTE=$080; { Lock owner is remote NFS client }
 F_NOINTR=$100; { Ignore signals when waiting }

 // lock operations for flock(2)
 LOCK_SH =$01; // shared file lock
 LOCK_EX =$02; // exclusive file lock
 LOCK_NB =$04; // don't block when locking
 LOCK_UN =$08; // unlock file

 //Advice to posix_fadvise
 POSIX_FADV_NORMAL    =0; // no special treatment
 POSIX_FADV_RANDOM    =1; // expect random page references
 POSIX_FADV_SEQUENTIAL=2; // expect sequential page references
 POSIX_FADV_WILLNEED  =3; // will need these pages
 POSIX_FADV_DONTNEED  =4; // dont need these pages
 POSIX_FADV_NOREUSE   =5; // access data only once

 {
  * Advisory file segment locking data type -
  * information passed to system by user
 }
type
 p_flock=^t_flock;
 t_flock=packed record
  l_start :Int64  ; // starting offset
  l_len   :Int64  ; // len = 0 means until end of file
  l_pid   :DWORD  ; // lock owner
  l_type  :WORD   ; // lock type: read/write, etc.
  l_whence:WORD   ; // type of l_start
  l_sysid :Integer; // remote system id or zero for local
 end;

{
  * Old advisory file segment locking data type,
  * before adding l_sysid.
}
 __oflock=packed record
  l_start :Int64; // starting offset
  l_len   :Int64; // len = 0 means until end of file
  l_pid   :DWORD; // lock owner
  l_type  :WORD ; // lock type: read/write, etc.
  l_whence:WORD ; // type of l_start
 end;

{ convert from open() flags to/from fflags; convert O_RD/WR to FREAD/FWRITE }
function FFLAGS(oflags:Integer):Integer; inline;
function OFLAGS(fflags:Integer):Integer; inline;

implementation

function FFLAGS(oflags:Integer):Integer; inline;
begin
 Result:=oflags+1;
end;

function OFLAGS(fflags:Integer):Integer; inline;
begin
 Result:=fflags-1;
end;

end.

