unit vcapability;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 p_cap_rights_t=^cap_rights_t;
 cap_rights_t=QWORD;

const
 {
  * Possible rights on capabilities.
  *
  * Notes:
  * Some system calls don't require a capability in order to perform an
  * operation on an fd.  These include: close, dup, dup2.
  *
  * sendfile is authorized using CAP_READ on the file and CAP_WRITE on the
  * socket.
  *
  * mmap() and aio*() system calls will need special attention as they may
  * involve reads or writes depending a great deal on context.
  }

 { General file I/O. }
  CAP_READ     =$0000000000000001; { read/recv }
  CAP_WRITE    =$0000000000000002; { write/send }
  CAP_MMAP     =$0000000000000004; { mmap }
  CAP_MAPEXEC  =$0000000000000008; { mmap(2) as exec }
  CAP_FEXECVE  =$0000000000000010;
  CAP_FSYNC    =$0000000000000020;
  CAP_FTRUNCATE=$0000000000000040;
  CAP_SEEK     =$0000000000000080;

 { VFS methods. }
  CAP_FCHFLAGS  =$0000000000000100;
  CAP_FCHDIR    =$0000000000000200;
  CAP_FCHMOD    =$0000000000000400;
  CAP_FCHOWN    =$0000000000000800;
  CAP_FCNTL     =$0000000000001000;
  CAP_FPATHCONF =$0000000000002000;
  CAP_FLOCK     =$0000000000004000;
  CAP_FSCK      =$0000000000008000;
  CAP_FSTAT     =$0000000000010000;
  CAP_FSTATFS   =$0000000000020000;
  CAP_FUTIMES   =$0000000000040000;
  CAP_CREATE    =$0000000000080000;
  CAP_DELETE    =$0000000000100000;
  CAP_MKDIR     =$0000000000200000;
  CAP_RMDIR     =$0000000000400000;
  CAP_MKFIFO    =$0000000000800000;

 { Lookups - used to constrain *at() calls. }
  CAP_LOOKUP =$0000000001000000;

 { Extended attributes. }
  CAP_EXTATTR_DELETE=$0000000002000000;
  CAP_EXTATTR_GET   =$0000000004000000;
  CAP_EXTATTR_LIST  =$0000000008000000;
  CAP_EXTATTR_SET   =$0000000010000000;

 { Access Control Lists. }
  CAP_ACL_CHECK =$0000000020000000;
  CAP_ACL_DELETE=$0000000040000000;
  CAP_ACL_GET   =$0000000080000000;
  CAP_ACL_SET   =$0000000100000000;

 { Socket operations. }
  CAP_ACCEPT      =$0000000200000000;
  CAP_BIND        =$0000000400000000;
  CAP_CONNECT     =$0000000800000000;
  CAP_GETPEERNAME =$0000001000000000;
  CAP_GETSOCKNAME =$0000002000000000;
  CAP_GETSOCKOPT  =$0000004000000000;
  CAP_LISTEN      =$0000008000000000;
  CAP_PEELOFF     =$0000010000000000;
  CAP_SETSOCKOPT  =$0000020000000000;
  CAP_SHUTDOWN    =$0000040000000000;

  CAP_SOCK_ALL=
  (CAP_ACCEPT or CAP_BIND or CAP_CONNECT
   or CAP_GETPEERNAME or CAP_GETSOCKNAME or CAP_GETSOCKOPT
   or CAP_LISTEN or CAP_PEELOFF or CAP_SETSOCKOPT or CAP_SHUTDOWN);

 { Mandatory Access Control. }
  CAP_MAC_GET =$0000080000000000;
  CAP_MAC_SET =$0000100000000000;

 { Methods on semaphores. }
  CAP_SEM_GETVALUE=$0000200000000000;
  CAP_SEM_POST =$0000400000000000;
  CAP_SEM_WAIT =$0000800000000000;

 { kqueue events. }
  CAP_POLL_EVENT =$0001000000000000;
  CAP_POST_EVENT =$0002000000000000;

 { Strange and powerful rights that should not be given lightly. }
  CAP_IOCTL =$0004000000000000;
  CAP_TTYHOOK =$0008000000000000;

 { Process management via process descriptors. }
  CAP_PDGETPID =$0010000000000000;
  CAP_PDWAIT =$0020000000000000;
  CAP_PDKILL =$0040000000000000;

 { The mask of all valid method rights. }
  CAP_MASK_VALID =$007fffffffffffff;

implementation

end.

