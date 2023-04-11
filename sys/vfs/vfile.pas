unit vfile;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vstat,
 vuio,
 vfs_vnode,
 kern_thr;

const
 NAME_MAX  =255;  // max bytes in a file name
 PATH_MAX  =1024; // max bytes in pathname
 PIPE_BUF  =512;  // max bytes for atomic pipe writes
 IOV_MAX   =1024; // max elements in i/o vector
 MAXNAMLEN =255;

 LINK_MAX   =32767; // max file link count
 MAX_CANON  =255;   // max bytes in term canon input line
 MAX_INPUT  =255;   // max bytes in terminal inpu
 NGROUPS_MAX=1023;  // max supplemental group id's

 MAXPATHLEN =PATH_MAX;
 MAXSYMLINKS=32;

 // configurable pathname variables
 _PC_LINK_MAX        =1;
 _PC_MAX_CANON       =2;
 _PC_MAX_INPUT       =3;
 _PC_NAME_MAX        =4;
 _PC_PATH_MAX        =5;
 _PC_PIPE_BUF        =6;
 _PC_CHOWN_RESTRICTED=7;
 _PC_NO_TRUNC        =8;
 _PC_VDISABLE        =9;
 _POSIX_VDISABLE     =$ff;

 // access function
 F_OK=0; // test for existence of file
 X_OK=$01; // test for execute or search permission
 W_OK=$02; // test for write permission
 R_OK=$04; // test for read permission

 // whence values for lseek(2)
 SEEK_SET=0; // set file offset to offset
 SEEK_CUR=1; // set file offset to current plus offset
 SEEK_END=2; // set file offset to EOF plus offset

 DTYPE_VNODE     = 1; { file }
 DTYPE_SOCKET    = 2; { communications endpoint }
 DTYPE_PIPE      = 3; { pipe }
 DTYPE_FIFO      = 4; { fifo (named pipe) }
 DTYPE_KQUEUE    = 5; { event queue }
 DTYPE_CRYPTO    = 6; { crypto }
 DTYPE_MQUEUE    = 7; { posix message queue }
 DTYPE_SHM       = 8; { swap-backed shared memory }
 DTYPE_SEM       = 9; { posix semaphore }
 DTYPE_PTS       =10; { pseudo teletype master device }
 DTYPE_DEV       =11; { Device specific fd type }
 DTYPE_CAPABILITY=12; { capability }
 DTYPE_PROCDESC  =13; { process descriptor }

 FOF_OFFSET  =$01; { Use the offset in uio argument }
 FOF_NOLOCK  =$02; { Do not take FOFFSET_LOCK }
 FOF_NEXTOFF =$04; { Also update f_nextoff }
 FOF_NOUPDATE=$10; { Do not update f_offset }

 DFLAG_PASSABLE=$01; { may be passed via unix sockets. }
 DFLAG_SEEKABLE=$02; { seekable / nonsequential }

type
 p_knote=Pointer;

 mode_t=Integer;
 uid_t =Integer;
 gid_t =Integer;

 pp_file=^p_file;
 p_file=^t_file;

 fo_rdwr_t    =function(fp:p_file;uio:p_uio;flags:Integer):Integer;
 fo_truncate_t=function(fp:p_file;length:Int64):Integer;
 fo_ioctl_t   =function(fp:p_file;com:QWORD;data:Pointer):Integer;
 fo_poll_t    =function(fp:p_file;events:Integer):Integer;
 fo_kqfilter_t=function(fp:p_file;kn:p_knote):Integer;
 fo_stat_t    =function(fp:p_file;sb:p_stat):Integer;
 fo_close_t   =function(fp:p_file):Integer;
 fo_chmod_t   =function(fp:p_file;mode:mode_t):Integer;
 fo_chown_t   =function(fp:p_file;uid:uid_t;gid:gid_t):Integer;

 fo_flags_t=type Integer;

 p_fileops=^fileops;
 fileops=packed record
  fo_read    :fo_rdwr_t    ;
  fo_write   :fo_rdwr_t    ;
  fo_truncate:fo_truncate_t;
  fo_ioctl   :fo_ioctl_t   ;
  fo_poll    :fo_poll_t    ;
  fo_kqfilter:fo_kqfilter_t;
  fo_stat    :fo_stat_t    ;
  fo_close   :fo_close_t   ;
  fo_chmod   :fo_chmod_t   ;
  fo_chown   :fo_chown_t   ;
  fo_flags:fo_flags_t; { DFLAG_* below }
 end;

 fadvise_info=packed record
  fa_advice   :Integer; { (f) FADV_* type. }
  fa_start    :Int64  ; { (f) Region start. }
  fa_end      :Int64  ; { (f) Region end. }
  fa_prevstart:Int64  ; { (f) Previous NOREUSE start. }
  fa_prevend  :Int64  ; { (f) Previous NOREUSE end. }
 end;

 t_file=packed record
  f_data        :Pointer  ; { file descriptor specific data }
  f_ops         :p_fileops; { File operations }
  f_vnode       :p_vnode  ; { NULL or applicable vnode }
  f_type        :Word     ; { descriptor type }
  f_vnread_flags:Word     ; { (f) Sleep lock for f_offset }
  f_flag        :DWORD    ; { see fcntl.h }
  f_count       :DWORD    ; { reference count }
  {
   *  DTYPE_VNODE specific fields.
   }
  f_seqcount:Integer; { Count of sequential accesses. }
  f_nextoff :Int64;   { next expected read/write offset. }
  f_vnun    :Pointer;
  {
   *  DFLAG_SEEKABLE specific fields
   }
  f_offset:Int64;
  {
   * Mandatory Access control information.
   }
  f_label:Pointer; { Place-holder for MAC label. }
 end;

const
 FOFFSET_LOCKED      =$1;
 FOFFSET_LOCK_WAITING=$2;
 FDEVFS_VNODE        =$4;

//extern fileops vnops;
//extern fileops badfileops;
//extern fileops socketops;
//extern int maxfiles;  { kernel limit on number of open files }
//extern int maxfilesperproc; { per process limit on number of open files }
//extern volatile int openfiles; { actual number of open files }

implementation

//function foffset_get(fp:p_file):Int64; inline;
//begin
// Result:=(foffset_lock(fp, FOF_NOLOCK));
//end;

{
 fhold(fp) (refcount_acquire(&(fp)^.f_count))
 fdrop(fp, td) (refcount_release(&(fp)^.f_count) ? _fdrop((fp), (td)) : _fnoop())

static __inline int
fo_read(file *fp, p_uio uio, ucred *active_cred, int flags)
begin

 return ((*fp^.f_ops^.fo_read)(fp, uio, flags));
end;

static __inline int
fo_write(file *fp, p_uio uio, ucred *active_cred, int flags)
begin

 return ((*fp^.f_ops^.fo_write)(fp, uio, flags));
end;

static __inline int
fo_truncate(file *fp, off_t length)
begin

 return ((*fp^.f_ops^.fo_truncate)(fp, length));
end;

static __inline int
fo_ioctl(file *fp, u_long com, void *data)
begin

 return ((*fp^.f_ops^.fo_ioctl)(fp, com, data));
end;

static __inline int
fo_poll(file *fp, int events)
begin

 return ((*fp^.f_ops^.fo_poll)(fp, events));
end;

static __inline int
fo_stat(file *fp, stat *sb)
begin

 return ((*fp^.f_ops^.fo_stat)(fp, sb));
end;

static __inline int
fo_close(file *fp, thread *td)
begin

 return ((*fp^.f_ops^.fo_close)(fp, td));
end;

static __inline int
fo_kqfilter(file *fp, knote *kn)
begin

 return ((*fp^.f_ops^.fo_kqfilter)(fp, kn));
end;

static __inline int
fo_chmod(file *fp, mode_t mode)
begin

 return ((*fp^.f_ops^.fo_chmod)(fp, mode));
end;

static __inline int
fo_chown(file *fp, uid_t uid, gid_t gid)
begin

 return ((*fp^.f_ops^.fo_chown)(fp, uid, gid));
end;
}



end.

