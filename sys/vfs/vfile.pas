unit vfile;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vstat,
 vuio,
 vfs_vnode,
 kern_thr,
 kern_id;

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

 maxfilesperproc = 44236;

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

 _PC_ASYNC_IO        =53;
 _PC_PRIO_IO         =54;
 _PC_SYNC_IO         =55;

 _PC_ALLOC_SIZE_MIN    =10;
 _PC_FILESIZEBITS      =12;
 _PC_REC_INCR_XFER_SIZE=14;
 _PC_REC_MAX_XFER_SIZE =15;
 _PC_REC_MIN_XFER_SIZE =16;
 _PC_REC_XFER_ALIGN    =17;
 _PC_SYMLINK_MAX       =18;

 _PC_ACL_EXTENDED=59;
 _PC_ACL_PATH_MAX=60;
 _PC_CAP_PRESENT =61;
 _PC_INF_PRESENT =62;
 _PC_MAC_PRESENT =63;
 _PC_ACL_NFS4    =64;

 // access function
 F_OK=0; // test for existence of file
 X_OK=$01; // test for execute or search permission
 W_OK=$02; // test for write permission
 R_OK=$04; // test for read permission

 // whence values for lseek(2)
 SEEK_SET =0; // set file offset to offset
 SEEK_CUR =1; // set file offset to current plus offset
 SEEK_END =2; // set file offset to EOF plus offset
 SEEK_DATA=3; // set file offset to next data past offset
 SEEK_HOLE=4; // set file offset to next hole past offset

 // whence values for lseek(2); renamed by POSIX 1003.1
 L_SET =SEEK_SET;
 L_INCR=SEEK_CUR;
 L_XTND=SEEK_END;

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
  fo_flags   :fo_flags_t; { DFLAG_* below }
 end;

 fadvise_info=packed record
  fa_advice   :Integer; { (f) FADV_* type. }
  fa_start    :Int64  ; { (f) Region start. }
  fa_end      :Int64  ; { (f) Region end. }
  fa_prevstart:Int64  ; { (f) Previous NOREUSE start. }
  fa_prevend  :Int64  ; { (f) Previous NOREUSE end. }
 end;

 t_file=packed record
  desc          :t_id_desc;
  //
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
  f_exclose :Integer;
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

var
 openfiles:Integer=0; { actual number of open files }

 {
  * The module initialization routine for POSIX asynchronous I/O will
  * set this to the version of AIO that it implements.  (Zero means
  * that it is not implemented.)  This value is used here by pathconf()
  * and in kern_descrip.c by fpathconf().
  }
 async_io_version:Integer=0;

function fo_read(fp:p_file;uio:p_uio;flags:Integer):Integer;
function fo_write(fp:p_file;uio:p_uio;flags:Integer):Integer;
function fo_truncate(fp:p_file;length:Int64):Integer;
function fo_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
function fo_poll(fp:p_file;events:Integer):Integer;
function fo_stat(fp:p_file;sb:p_stat):Integer;
function fo_close(fp:p_file):Integer;
function fo_kqfilter(fp:p_file;kn:Pointer):Integer;
function fo_chmod(fp:p_file;mode:mode_t):Integer;
function fo_chown(fp:p_file;uid:uid_t;gid:gid_t):Integer;

implementation

{
 fhold(fp) (refcount_acquire(&(fp)^.f_count))
 fdrop(fp, td) (refcount_release(&(fp)^.f_count) ? _fdrop((fp), (td)) : _fnoop())
}

function fo_read(fp:p_file;uio:p_uio;flags:Integer):Integer;
begin
 Exit(fp^.f_ops^.fo_read(fp,uio,flags));
end;

function fo_write(fp:p_file;uio:p_uio;flags:Integer):Integer;
begin
 Exit(fp^.f_ops^.fo_write(fp,uio,flags));
end;

function fo_truncate(fp:p_file;length:Int64):Integer;
begin
 Exit(fp^.f_ops^.fo_truncate(fp,length));
end;

function fo_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
begin
 Exit(fp^.f_ops^.fo_ioctl(fp,com,data));
end;

function fo_poll(fp:p_file;events:Integer):Integer;
begin
 Exit(fp^.f_ops^.fo_poll(fp,events));
end;

function fo_stat(fp:p_file;sb:p_stat):Integer;
begin
 Exit(fp^.f_ops^.fo_stat(fp,sb));
end;

function fo_close(fp:p_file):Integer;
begin
 Exit(fp^.f_ops^.fo_close(fp));
end;

function fo_kqfilter(fp:p_file;kn:Pointer):Integer;
begin
 Exit(fp^.f_ops^.fo_kqfilter(fp,kn));
end;

function fo_chmod(fp:p_file;mode:mode_t):Integer;
begin
 Exit(fp^.f_ops^.fo_chmod(fp,mode));
end;

function fo_chown(fp:p_file;uid:uid_t;gid:gid_t):Integer;
begin
 Exit(fp^.f_ops^.fo_chown(fp,uid,gid));
end;




end.

