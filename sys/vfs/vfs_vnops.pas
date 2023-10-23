unit vfs_vnops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sys_event,
 kern_param,
 vmount,
 vnamei,
 vfile,
 vstat,
 vuio,
 vmparam,
 vfilio,
 vnode;

function  vn_lock(vp:p_vnode;flags:Integer):Integer;

function  vn_open(ndp:p_nameidata;
                  flagp:PInteger;
                  cmode:Integer;
                  fp:p_file):Integer;

function  vn_open_cred(ndp:p_nameidata;
                       flagp:PInteger;
                       cmode:Integer;
                       vn_open_flags:DWORD;
                       fp:p_file):Integer;

function  vn_writechk(vp:p_vnode):Integer;
function  vn_start_write(vp:p_vnode;mpp:pp_mount;flags:Integer):Integer;
procedure vn_finished_write(mp:p_mount);
function  vn_close(vp:p_vnode;flags:Integer):Integer;
function  vn_stat(vp:p_vnode;sb:p_stat):Integer;

function  vn_io_fault(fp:p_file;uio:p_uio;flags:Integer):Integer;
function  vn_truncate(fp:p_file;length:Int64):Integer;
function  vn_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
function  vn_poll(fp:p_file;events:Integer):Integer;
function  vn_statfile(fp:p_file;sb:p_stat):Integer;
function  vn_closefile(fp:p_file):Integer;
function  vn_chmod(fp:p_file;mode:mode_t):Integer;
function  vn_chown(fp:p_file;uid:uid_t;gid:gid_t):Integer;
function  vn_kqfilter(fp:p_file;kn:p_knote):Integer;

const
 vnops:fileops=(
  fo_read    :@vn_io_fault;
  fo_write   :@vn_io_fault;
  fo_truncate:@vn_truncate;
  fo_ioctl   :@vn_ioctl;
  fo_poll    :@vn_poll;
  fo_kqfilter:@vn_kqfilter;
  fo_stat    :@vn_statfile;
  fo_close   :@vn_closefile;
  fo_chmod   :@vn_chmod;
  fo_chown   :@vn_chown;
  fo_flags   :DFLAG_PASSABLE or DFLAG_SEEKABLE
 );

function  foffset_get(fp:p_file):Int64;
function  foffset_lock(fp:p_file;flags:Integer):Int64;
procedure foffset_unlock(fp:p_file;val:Int64;flags:Integer);
procedure foffset_lock_uio(fp:p_file;uio:p_uio;flags:Integer);
procedure foffset_unlock_uio(fp:p_file;uio:p_uio;flags:Integer);

//

procedure vref(vp:p_vnode); external;

//

implementation

uses
 errno,
 vnode_if,
 vfcntl,
 vfs_subr,
 vfs_syscalls,
 kern_thr,
 kern_synch,
 kern_mtx,
 kern_mtxpool,
 kern_descrip;

function vn_lock(vp:p_vnode;flags:Integer):Integer;
begin
 Assert((flags and LK_TYPE_MASK)<>0,'vn_lock called with no locktype.');

 repeat
  Result:=VOP_LOCK(vp,flags,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  flags:=flags and (not LK_INTERLOCK);

  Assert(((flags and LK_RETRY)=0) or (Result=0),'LK_RETRY set with incompatible flags (0x%x) or an Result occured (%d)');

  if (Result=0) and
     ((vp^.v_iflag and VI_DOOMED)<>0) and
     ((flags and LK_RETRY)=0) then
  begin
   VOP_UNLOCK(vp,0);
   Result:=ENOENT;
   break;
  end;
 until ((flags and LK_RETRY)=0) or (Result=0);
end;

function vn_open(ndp:p_nameidata;
                 flagp:PInteger;
                 cmode:Integer;
                 fp:p_file):Integer;
begin
 Result:=vn_open_cred(ndp, flagp, cmode, 0, fp);
end;

{
 * Common code for vnode open operations.
 * Check permissions, and call the VOP_OPEN or VOP_CREATE routine.
 *
 * Note that this does NOT free nameidata for the successful case,
 * due to the NDINIT being done elsewhere.
}
function vn_open_cred(ndp:p_nameidata;
                      flagp:PInteger;
                      cmode:Integer;
                      vn_open_flags:DWORD;
                      fp:p_file):Integer;
label
 restart,
 bad;
var
 vp:p_vnode;
 mp:p_mount;
 vat:t_vattr;
 vap:p_vattr;
 ofmode,fmode,error:Integer;
 accmode:accmode_t;
 mps:Integer;
 vfslocked:Integer;
begin
 vap:=@vat;
 mps:=ndp^.ni_cnd.cn_flags and MPSAFE;

restart:
 vfslocked:=0;
 ofmode:=flagp^;
 fmode:=ofmode;

 if ((fmode and O_CREAT)<>0) then
 begin
  ndp^.ni_cnd.cn_nameiop:=CREATE;
  ndp^.ni_cnd.cn_flags:=ISOPEN or LOCKPARENT or LOCKLEAF or MPSAFE;

  if ((fmode and O_EXCL)=0) and ((fmode and O_NOFOLLOW)=0) then
  begin
   ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags or FOLLOW;
  end;

  if ((vn_open_flags and VN_OPEN_NOAUDIT)=0) then
  begin
   ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags or AUDITVNODE1;
  end;

  //bwillwrite();
  error:=nd_namei(ndp);
  if (error<>0) then
  begin
   Exit(error);
  end;

  vfslocked:=NDHASGIANT(ndp);
  if (mps=0) then
  begin
   ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags and (not MPSAFE);
  end;

  if (ndp^.ni_vp=nil) then
  begin
   vattr_null(vap);
   vap^.va_type:=VREG;
   vap^.va_mode:=cmode;

   if ((fmode and O_EXCL)<>0) then
   begin
    vap^.va_vaflags:=vap^.va_vaflags or VA_EXCLUSIVE;
   end;

   if (vn_start_write(ndp^.ni_dvp, @mp, V_NOWAIT)<>0) then
   begin
    NDFREE(ndp, NDF_ONLY_PNBUF);
    vput(ndp^.ni_dvp);
    VFS_UNLOCK_GIANT(ord(vfslocked));
    error:=vn_start_write(nil, @mp, V_XSLEEP or PCATCH);
    if (error<>0) then Exit(error);

    goto restart;
   end;

   //error:=mac_vnode_check_create(cred, ndp^.ni_dvp, &ndp^.ni_cnd, vap);
   //if (error=0) then
    error:=VOP_CREATE(ndp^.ni_dvp, @ndp^.ni_vp, @ndp^.ni_cnd, vap);

   vput(ndp^.ni_dvp);
   vn_finished_write(mp);

   if (error<>0) then
   begin
    VFS_UNLOCK_GIANT(ord(vfslocked));
    NDFREE(ndp, NDF_ONLY_PNBUF);
    Exit(error);
   end;

   fmode:=fmode and (not O_TRUNC);
   vp:=ndp^.ni_vp;
  end else
  begin
   if (ndp^.ni_dvp=ndp^.ni_vp) then
    vrele(ndp^.ni_dvp)
   else
    vput(ndp^.ni_dvp);

   ndp^.ni_dvp:=nil;
   vp:=ndp^.ni_vp;

   if ((fmode and O_EXCL)<>0) then
   begin
    error:=EEXIST;
    goto bad;
   end;

   fmode:=fmode and (not O_CREAT);
  end;
 end else
 begin
  ndp^.ni_cnd.cn_nameiop:=LOOKUP;
  ndp^.ni_cnd.cn_flags:=ISOPEN or LOCKLEAF or MPSAFE;

  if ((fmode and O_NOFOLLOW)<>0) then
  begin
   ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags or NOFOLLOW;
  end else
  begin
   ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags or FOLLOW;
  end;

  if ((fmode and FWRITE)=0) then
  begin
   ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags or LOCKSHARED;
  end;

  if ((vn_open_flags and VN_OPEN_NOAUDIT)=0) then
  begin
   ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags or AUDITVNODE1;
  end;

  error:=nd_namei(ndp);
  if (error<>0) then
  begin
   Exit(error);
  end;

  if (mps=0) then
  begin
   ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags and (not MPSAFE);
  end;

  vfslocked:=NDHASGIANT(ndp);
  vp:=ndp^.ni_vp;
 end;

 case vp^.v_type of
  VLNK:
   begin
    error:=EMLINK;
    goto bad;
   end;
  VSOCK:
   begin
    error:=EOPNOTSUPP;
    goto bad;
   end;
  VDIR:
   if ((fmode and (FWRITE or O_TRUNC))<>0) then
   begin
    error:=EISDIR;
    goto bad;
   end;
  else
   if ((fmode and O_DIRECTORY)<>0) then
   begin
    error:=ENOTDIR;
    goto bad;
   end;
 end;

 accmode:=0;
 if ((fmode and (FWRITE or O_TRUNC))<>0) then
 begin
  accmode:=accmode or VWRITE;
 end;

 if ((fmode and FREAD)<>0) then
 begin
  accmode:=accmode or VREAD;
 end;

 if ((fmode and FEXEC)<>0) then
 begin
  accmode:=accmode or VEXEC;
 end;

 if ((fmode and O_APPEND)<>0) and ((fmode and FWRITE)<>0) then
 begin
  accmode:=accmode or VAPPEND;
 end;

 //error:=mac_vnode_check_open(cred, vp, accmode);
 //if (error) then
 // goto bad;

 if ((fmode and O_CREAT)=0) then
 begin
  if ((accmode and VWRITE)<>0) then
  begin
   error:=vn_writechk(vp);
   if (error<>0) then goto bad;
  end;
  if (accmode<>0) then
  begin
   error:=VOP_ACCESS(vp, accmode);
   if (error<>0) then goto bad;
  end;
 end;

 if (vp^.v_type=VFIFO) and (VOP_ISLOCKED(vp)<>LK_EXCLUSIVE) then
 begin
  vn_lock(vp, LK_UPGRADE or LK_RETRY);
 end;

 error:=VOP_OPEN(vp, ofmode, fp);
 if (error<>0) then goto bad;

 if ((fmode and FWRITE)<>0) then
 begin
  VOP_ADD_WRITECOUNT(vp, 1);
 end;

 flagp^:=fmode;
 ASSERT_VOP_LOCKED(vp, 'vn_open_cred');

 if (mps=0) then
 begin
  VFS_UNLOCK_GIANT(ord(vfslocked));
 end;

 Exit(0);
bad:
 NDFREE(ndp, NDF_ONLY_PNBUF);
 vput(vp);
 VFS_UNLOCK_GIANT(ord(vfslocked));
 flagp^:=fmode;
 ndp^.ni_vp:=nil;
 Exit(error);
end;

function vn_writechk(vp:p_vnode):Integer;
begin
 ASSERT_VOP_LOCKED(vp, 'vn_writechk');

 Exit(0);
end;

function vn_start_write_locked(mp:p_mount;flags:Integer):Integer;
label
 unlock;
var
 td:p_kthread;
 error:Integer;
begin
 mtx_assert(MNT_MTX(mp)^);
 error:=0;

 td:=curkthread;
 {
  * Check on status of suspension.
  }
 if (td<>nil) then
 if ((td^.td_pflags and TDP_IGNSUSP)=0) or
    (mp^.mnt_susp_owner<>td) then
 begin
  while ((mp^.mnt_kern_flag and MNTK_SUSPEND)<>0) do
  begin
   if ((flags and V_NOWAIT)<>0) then
   begin
    error:=EWOULDBLOCK;
    goto unlock;
   end;
   error:=msleep(@mp^.mnt_flag, MNT_MTX(mp), (PUSER - 1) or (flags and PCATCH), 'suspfs', 0);
   if (error<>0) then
   begin
    goto unlock;
   end;
  end;
 end;

 if ((flags and V_XSLEEP)<>0) then
 begin
  goto unlock;
 end;

 Inc(mp^.mnt_writeopcount);
unlock:
 if (error<>0) or ((flags and V_XSLEEP)<>0) then
 begin
  MNT_REL(mp);
 end;

 MNT_IUNLOCK(mp);
 Exit(error);
end;

function vn_start_write(vp:p_vnode;mpp:pp_mount;flags:Integer):Integer;
var
 mp:p_mount;
 error:Integer;
begin
 error:=0;
 {
  * If a vnode is provided, get and Exit the mount point that
  * to which it will write.
  }
 if (vp<>nil) then
 begin
  error:=VOP_GETWRITEMOUNT(vp, mpp);
  if (error<>0) then
  begin
   mpp^:=nil;
   if (error<>EOPNOTSUPP) then
   begin
    Exit(error);
   end;
   Exit(0);
  end;
 end;
 mp:=mpp^;
 if (mp=nil) then
 begin
  Exit(0);
 end;

 {
  * VOP_GETWRITEMOUNT() Exits with the mp refcount held through
  * a vfs_ref().
  * As long as a vnode is not provided we need to acquire a
  * refcount for the provided mountpoint too, in order to
  * emulate a vfs_ref().
  }
 MNT_ILOCK(mp);
 if (vp=nil) then
 begin
  MNT_REF(mp);
 end;

 Exit(vn_start_write_locked(mp, flags));
end;


procedure vn_finished_write(mp:p_mount);
begin
 if (mp=nil) then Exit;
 MNT_ILOCK(mp);
 MNT_REL(mp);

 Dec(mp^.mnt_writeopcount);
 if (mp^.mnt_writeopcount < 0) then
 begin
  Assert(false,'vn_finished_write: neg cnt');
 end;

 if ((mp^.mnt_kern_flag and MNTK_SUSPEND)<>0) and (mp^.mnt_writeopcount<=0) then
 begin
  wakeup(@mp^.mnt_writeopcount);
 end;

 MNT_IUNLOCK(mp);
end;

function vn_close(vp:p_vnode;flags:Integer):Integer;
var
 mp:p_mount;
 error, lock_flags:Integer;
begin
 lock_flags:=LK_EXCLUSIVE;

 if (vp^.v_type<>VFIFO) and
    ((flags and FWRITE)=0) and
    (vp^.v_mount<>nil) then
 if ((p_mount(vp^.v_mount)^.mnt_kern_flag and MNTK_EXTENDED_SHARED)<>0) then
 begin
  lock_flags:=LK_SHARED;
 end;

 VFS_ASSERT_GIANT(vp^.v_mount);

 vn_start_write(vp, @mp, V_WAIT);
 vn_lock(vp, lock_flags or LK_RETRY);
 if ((flags and FWRITE)<>0) then
 begin
  Assert(vp^.v_writecount > 0,'vn_close: negative writecount');
  VOP_ADD_WRITECOUNT(vp, -1);
 end;
 error:=VOP_CLOSE(vp, flags);
 vput(vp);
 vn_finished_write(mp);
 Exit(error);
end;

function vn_stat(vp:p_vnode;sb:p_stat):Integer;
var
 vattr:t_vattr;
 vap:p_vattr;
 error:Integer;
 mode:WORD;
begin
 //error:=mac_vnode_check_stat(active_cred, file_cred, vp);
 //if (error<>0) then
 // Exit(error);

 vap:=@vattr;

 {
  * Initialize defaults for new and unusual fields, so that file
  * systems which don't support these fields don't need to know
  * about them.
  }
 vap^.va_birthtime.tv_sec :=-1;
 vap^.va_birthtime.tv_nsec:=0;

 vap^.va_fsid:=VNOVAL;
 vap^.va_rdev:=NODEV;

 error:=VOP_GETATTR(vp, vap);
 if (error<>0) then
 begin
  Exit(error);
 end;

 {
  * Zero the spare stat fields
  }
 sb^:=Default(t_stat);

 {
  * Copy from vattr table
  }
 if (vap^.va_fsid<>VNOVAL) then
  sb^.st_dev:=vap^.va_fsid
 else
  sb^.st_dev:=p_mount(vp^.v_mount)^.mnt_stat.f_fsid.val[0];

 sb^.st_ino:=vap^.va_fileid;

 mode:=vap^.va_mode;
 case vap^.va_type of
   VREG:mode:=mode or S_IFREG;
   VDIR:mode:=mode or S_IFDIR;
   VBLK:mode:=mode or S_IFBLK;
   VCHR:mode:=mode or S_IFCHR;
   VLNK:mode:=mode or S_IFLNK;
  VSOCK:mode:=mode or S_IFSOCK;
  VFIFO:mode:=mode or S_IFIFO;
  else
   Exit(EBADF);
 end;

 sb^.st_mode :=mode;
 sb^.st_nlink:=vap^.va_nlink;
 sb^.st_uid  :=vap^.va_uid;
 sb^.st_gid  :=vap^.va_gid;
 sb^.st_rdev :=vap^.va_rdev;

 if (vap^.va_size > High(Int64)) then
 begin
  Exit(EOVERFLOW);
 end;

 sb^.st_size:=vap^.va_size;
 sb^.st_atim:=vap^.va_atime;
 sb^.st_mtim:=vap^.va_mtime;
 sb^.st_ctim:=vap^.va_ctime;
 sb^.st_birthtim:=vap^.va_birthtime;

 {
  * According to www.opengroup.org, the meaning of st_blksize is
  *   "a filesystem-specific preferred I/O block size for this
  *    object.  In some filesystem types, this may vary from file
  *    to file"
  * Use miminum/default of PAGE_SIZE (e.g. for VCHR).
  }

 if (PAGE_SIZE>vap^.va_blocksize) then
 begin
  sb^.st_blksize:=PAGE_SIZE;
 end else
 begin
  sb^.st_blksize:=vap^.va_blocksize;
 end;

 sb^.st_flags:=vap^.va_flags;
 //if (priv_check(td, PRIV_VFS_GENERATION)) then
 // sb^.st_gen:=0;
 //else
  sb^.st_gen:=vap^.va_gen;

 sb^.st_blocks:=vap^.va_bytes div S_BLKSIZE;
 Exit(0);
end;


function foffset_get(fp:p_file):Int64; inline;
begin
 Result:=(foffset_lock(fp, FOF_NOLOCK));
end;

function foffset_lock(fp:p_file;flags:Integer):Int64;
var
 mtxp:p_mtx;
begin
 Result:=0;
 Assert((flags and FOF_OFFSET)=0, 'FOF_OFFSET passed');

 mtxp:=mtx_pool_find(mtxpool_sleep, fp);
 mtx_lock(mtxp^);

 if ((flags and FOF_NOLOCK)=0) then
 begin
  while ((fp^.f_vnread_flags and FOFFSET_LOCKED)<>0) do
  begin
   fp^.f_vnread_flags:=fp^.f_vnread_flags or FOFFSET_LOCK_WAITING;
   msleep(@fp^.f_vnread_flags, mtxp, PUSER, 'vofflock', 0);
  end;
  fp^.f_vnread_flags:=fp^.f_vnread_flags or FOFFSET_LOCKED;
 end;
 Result:=fp^.f_offset;

 mtx_unlock(mtxp^);
end;

procedure foffset_unlock(fp:p_file;val:Int64;flags:Integer);
var
 mtxp:p_mtx;
begin
 Assert((flags and FOF_OFFSET)=0,'FOF_OFFSET passed');

 mtxp:=mtx_pool_find(mtxpool_sleep, fp);
 mtx_lock(mtxp^);

 if ((flags and FOF_NOUPDATE)=0) then
 begin
  fp^.f_offset:=val;
 end;

 if ((flags and FOF_NEXTOFF)<>0) then
 begin
  fp^.f_nextoff:=val;
 end;

 if ((flags and FOF_NOLOCK)=0) then
 begin
  Assert((fp^.f_vnread_flags and FOFFSET_LOCKED)<>0,'Lost FOFFSET_LOCKED');

  if ((fp^.f_vnread_flags and FOFFSET_LOCK_WAITING)<>0) then
  begin
   wakeup(@fp^.f_vnread_flags);
  end;

  fp^.f_vnread_flags:=0;
 end;

 mtx_unlock(mtxp^);
end;

procedure foffset_lock_uio(fp:p_file;uio:p_uio;flags:Integer);
begin
 if ((flags and FOF_OFFSET)=0) then
 begin
  uio^.uio_offset:=foffset_lock(fp, flags);
 end;
end;

procedure foffset_unlock_uio(fp:p_file;uio:p_uio;flags:Integer);
begin
 if ((flags and FOF_OFFSET)=0) then
 begin
  foffset_unlock(fp, uio^.uio_offset, flags);
 end;
end;

{
 * Heuristic to detect sequential operation.
 }
function sequential_heuristic(uio:p_uio;fp:p_file):Integer;
begin
 if ((fp^.f_flag and FRDAHEAD)<>0) then
 begin
  Exit(fp^.f_seqcount shl IO_SEQSHIFT);
 end;

 {
  * Offset 0 is handled specially.  open() sets f_seqcount to 1 so
  * that the first I/O is normally considered to be slightly
  * sequential.  Seeking to offset 0 doesn't change sequentiality
  * unless previous seeks have reduced f_seqcount to 0, in which
  * case offset 0 is not special.
  }
 if ((uio^.uio_offset=0) and (fp^.f_seqcount > 0)) or
    (uio^.uio_offset=fp^.f_nextoff) then
 begin
  {
   * f_seqcount is in units of fixed-size blocks so that it
   * depends mainly on the amount of sequential I/O and not
   * much on the number of sequential I/O's.  The fixed size
   * of 16384 is hard-coded here since it is (not quite) just
   * a magic size that works well here.  This size is more
   * closely related to the best I/O size for real disks than
   * to any block size used by software.
   }
  Inc(fp^.f_seqcount,((uio^.uio_resid+(16384 - 1)) div 16384));

  if (fp^.f_seqcount > IO_SEQMAX) then
  begin
   fp^.f_seqcount:=IO_SEQMAX;
  end;

  Exit(fp^.f_seqcount shl IO_SEQSHIFT);
 end;

 { Not sequential.  Quickly draw-down sequentiality. }
 if (fp^.f_seqcount > 1) then
  fp^.f_seqcount:=1
 else
  fp^.f_seqcount:=0;

 Exit(0);
end;

function get_advice(fp:p_file;uio:p_uio):Integer;
var
 mtxp:p_mtx;
 f_advice:p_fadvise_info;
 ret:Integer;
begin
 ret:=POSIX_FADV_NORMAL;
 if (fp^.f_advice=nil) then Exit(ret);

 mtxp:=mtx_pool_find(mtxpool_sleep, fp);
 mtx_lock(mtxp^);

 f_advice:=fp^.f_advice;

 if (uio^.uio_offset >= f_advice^.fa_start) and
    (uio^.uio_offset + uio^.uio_resid <= f_advice^.fa_end) then
 begin
  ret:=f_advice^.fa_advice;
 end;

 mtx_unlock(mtxp^);
 Exit(ret);
end;

{
 * File table vnode read routine.
 }
function vn_read(fp:p_file;uio:p_uio;flags:Integer):Integer;
var
 td:p_kthread;
 vp:p_vnode;
 mtxp:p_mtx;
 error,ioflag:Integer;
 advice,vfslocked:Integer;
 offset,start,__end:Int64;
 f_advice:p_fadvise_info;
begin
 td:=curkthread;

 Assert(uio^.uio_td=td, 'uio_td %p is not td %p');
 Assert((flags and FOF_OFFSET)<>0, 'No FOF_OFFSET');
 vp:=fp^.f_vnode;

 ioflag:=0;

 if ((fp^.f_flag and FNONBLOCK)<>0) then
 begin
  ioflag:=ioflag or IO_NDELAY;
 end;
 if ((fp^.f_flag and O_DIRECT)<>0) then
 begin
  ioflag:=ioflag or IO_DIRECT;
 end;

 advice:=get_advice(fp, uio);
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 vn_lock(vp, LK_SHARED or LK_RETRY);

 case advice of
  POSIX_FADV_NORMAL,
  POSIX_FADV_SEQUENTIAL,
  POSIX_FADV_NOREUSE:
   ioflag:=ioflag or sequential_heuristic(uio, fp);
  POSIX_FADV_RANDOM:;
   { Disable read-ahead for random I/O. }
  else;
 end;
 offset:=uio^.uio_offset;

 //error:=mac_vnode_check_read(active_cred, fp^.f_cred, vp);
 //if (error=0) then

  error:=VOP_READ(vp, uio, ioflag);
 fp^.f_nextoff:=uio^.uio_offset;
 VOP_UNLOCK(vp, 0);
 if (error=0) and
    (advice=POSIX_FADV_NOREUSE) and
    (offset<>uio^.uio_offset) then
 begin
  start:=offset;
  __end:=uio^.uio_offset - 1;

  mtxp:=mtx_pool_find(mtxpool_sleep, fp);
  mtx_lock(mtxp^);

  f_advice:=fp^.f_advice;
  if (f_advice<>nil) then
  if (f_advice^.fa_advice=POSIX_FADV_NOREUSE) then
  begin
   if (start<>0) and (f_advice^.fa_prevend + 1=start) then
   begin
    start:=f_advice^.fa_prevstart;
   end else
   if (f_advice^.fa_prevstart<>0) and
      (f_advice^.fa_prevstart=__end + 1) then
   begin
    __end:=f_advice^.fa_prevend;
   end;
   f_advice^.fa_prevstart:=start;
   f_advice^.fa_prevend  :=__end;
  end;

  mtx_unlock(mtxp^);
 end;
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

{
 * File table vnode write routine.
 }
function vn_write(fp:p_file;uio:p_uio;flags:Integer):Integer;
label
 unlock;
var
 td:p_kthread;
 vp:p_vnode;
 mp:p_mount;
 mtxp:p_mtx;
 error,ioflag,lock_flags:Integer;
 advice,vfslocked:Integer;
 offset,start,__end:Int64;
 f_advice:p_fadvise_info;
begin
 td:=curkthread;

 Assert(uio^.uio_td=td, 'uio_td %p is not td %p');
 Assert((flags and FOF_OFFSET)<>0, 'No FOF_OFFSET');
 vp:=fp^.f_vnode;
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 //if (vp^.v_type=VREG) then
 // bwillwrite();

 ioflag:=IO_UNIT;

 if (vp^.v_type=VREG) and ((fp^.f_flag and O_APPEND)<>0) then
 begin
  ioflag:=ioflag or IO_APPEND;
 end;
 if ((fp^.f_flag and FNONBLOCK)<>0) then
 begin
  ioflag:=ioflag or IO_NDELAY;
 end;
 if ((fp^.f_flag and O_DIRECT)<>0) then
 begin
  ioflag:=ioflag or IO_DIRECT;
 end;
 if ((fp^.f_flag and O_FSYNC)<>0) then
 begin
  ioflag:=ioflag or IO_SYNC;
 end;

 if (vp^.v_mount<>nil) then
 if ((p_mount(vp^.v_mount)^.mnt_flag and MNT_SYNCHRONOUS)<>0) then
 begin
  ioflag:=ioflag or IO_SYNC;
 end;
 mp:=nil;
 if (vp^.v_type<>VCHR) then
 begin
  error:=vn_start_write(vp, @mp, V_WAIT or PCATCH);
  if (error<>0) then
   goto unlock;
 end;

 advice:=get_advice(fp, uio);

 if (MNT_SHARED_WRITES(mp) or
    ((mp=nil) and MNT_SHARED_WRITES(vp^.v_mount))) and
    ((flags and FOF_OFFSET)<>0) then
 begin
  lock_flags:=LK_SHARED;
 end else
 begin
  lock_flags:=LK_EXCLUSIVE;
 end;

 vn_lock(vp, lock_flags or LK_RETRY);
 case advice of
  POSIX_FADV_NORMAL,
  POSIX_FADV_SEQUENTIAL,
  POSIX_FADV_NOREUSE:
   ioflag:=ioflag or sequential_heuristic(uio, fp);
  POSIX_FADV_RANDOM:;
   { XXX: Is this correct? }
 end;
 offset:=uio^.uio_offset;

 //error:=mac_vnode_check_write(active_cred, fp^.f_cred, vp);
 //if (error=0) then

  error:=VOP_WRITE(vp, uio, ioflag);
 fp^.f_nextoff:=uio^.uio_offset;
 VOP_UNLOCK(vp, 0);

 if (vp^.v_type<>VCHR) then
  vn_finished_write(mp);

 if (error=0) and
    (advice=POSIX_FADV_NOREUSE) and
    (offset<>uio^.uio_offset) then
 begin
  start:=offset;
  __end:=uio^.uio_offset - 1;

  mtxp:=mtx_pool_find(mtxpool_sleep, fp);
  mtx_lock(mtxp^);

  f_advice:=fp^.f_advice;
  if (f_advice<>nil) then
  if (f_advice^.fa_advice=POSIX_FADV_NOREUSE) then
  begin
   if (start<>0) and (f_advice^.fa_prevend + 1=start) then
   begin
    start:=f_advice^.fa_prevstart;
   end else
   if (f_advice^.fa_prevstart<>0) and
      (f_advice^.fa_prevstart=__end + 1) then
   begin
    __end:=f_advice^.fa_prevend;
   end;
   f_advice^.fa_prevstart:=start;
   f_advice^.fa_prevend  :=__end;
  end;

  mtx_unlock(mtxp^);
 end;

unlock:
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function vn_io_fault(fp:p_file;uio:p_uio;flags:Integer):Integer;
label
 out_last;
var
 td:p_kthread;
 //vm_page_t ma[io_hold_cnt + 2];
 uio_clone:p_uio;
 short_uio:T_uio;
 short_iovec:array[0..0] of iovec;
 doio:fo_rdwr_t;
 vp:p_vnode;
 rl_cookie:Pointer;
 mp:p_mount;
 //vm_page_t *prev_td_ma;
 error,cnt,save,saveheld,prev_td_ma_cnt:Integer;
 addr,__end:QWORD;
 //vm_prot_t prot;
 len,resid:QWORD;
 adv:Int64;
begin
 td:=curkthread;

 if (uio^.uio_rw=UIO_READ) then
  doio:=@vn_read
 else
  doio:=@vn_write;

 vp:=fp^.f_vnode;
 foffset_lock_uio(fp, uio, flags);

 mp:=vp^.v_mount;
 if (mp<>nil) then
 if ((mp^.mnt_kern_flag and MNTK_NO_IOPF)=0) then
 begin
  error:=doio(fp, uio, flags or FOF_OFFSET);
  goto out_last;
 end;

 if (uio^.uio_segflg<>UIO_USERSPACE) or
    (vp^.v_type<>VREG) or
    {(not vn_io_fault_enable)} false then
 begin
  error:=doio(fp, uio, flags or FOF_OFFSET);
  goto out_last;
 end;

{
 uio_clone:=cloneuio(uio);
 resid:=uio^.uio_resid;

 short_uio.uio_segflg:=UIO_USERSPACE;
 short_uio.uio_rw:=uio^.uio_rw;
 short_uio.uio_td:=uio^.uio_td;

 if (uio^.uio_rw=UIO_READ) then
 begin
  prot:=VM_PROT_WRITE;
  rl_cookie:=vn_rangelock_rlock(vp, uio^.uio_offset, uio^.uio_offset + uio^.uio_resid);
 end else
 begin
  prot:=VM_PROT_READ;
  if ((fp^.f_flag and O_APPEND)<>0 or (flags and FOF_OFFSET)=0) then
   { For appenders, punt and lock the whole range. }
   rl_cookie:=vn_rangelock_wlock(vp, 0, High(Int64))
  else
   rl_cookie:=vn_rangelock_wlock(vp, uio^.uio_offset, uio^.uio_offset + uio^.uio_resid);
 end;

 save:=vm_fault_disable_pagefaults();
 error:=doio(fp, uio, flags or FOF_OFFSET, td);
 if (error<>EFAULT) then
  goto _out;

 atomic_add_long(@vn_io_faults_cnt, 1);
 uio_clone^.uio_segflg:=UIO_NOCOPY;
 uiomove(nil, resid - uio^.uio_resid, uio_clone);
 uio_clone^.uio_segflg:=uio^.uio_segflg;

 saveheld:=curthread_pflags_set(TDP_UIOHELD);
 prev_td_ma:=td^.td_ma;
 prev_td_ma_cnt:=td^.td_ma_cnt;

 while (uio_clone^.uio_resid<>0) do
 begin
  len:=uio_clone^.uio_iov^.iov_len;
  if (len=0) then
  begin
   Assert(uio_clone^.uio_iovcnt >= 1, 'iovcnt underflow');
   uio_clone^.uio_iov++;
   uio_clone^.uio_iovcnt--;
   continue;
  end;
  if (len > io_hold_cnt * PAGE_SIZE)
   len:=io_hold_cnt * PAGE_SIZE;
  addr:=(uintptr_t)uio_clone^.uio_iov^.iov_base;
  __end:=round_page(addr + len);
  if (__end < addr) then
  begin
   error:=EFAULT;
   break;
  end;
  cnt:=atop(__end - trunc_page(addr));
  {
   * A perfectly misaligned address and length could cause
   * both the start and the end of the chunk to use partial
   * page.  +2 accounts for such a situation.
   }
  cnt:=vm_fault_quick_hold_pages(@td^.td_proc^.p_vmspace^.vm_map,  addr, len, prot, ma, io_hold_cnt + 2);
  if (cnt=-1) then
  begin
   error:=EFAULT;
   break;
  end;
  short_uio.uio_iov:=@short_iovec[0];
  short_iovec[0].iov_base:=(void *)addr;
  short_uio.uio_iovcnt:=1;
  short_uio.uio_resid:=short_iovec[0].iov_len:=len;
  short_uio.uio_offset:=uio_clone^.uio_offset;

  td^.td_ma:=ma;
  td^.td_ma_cnt:=cnt;

  error:=doio(fp, @short_uio, flags or FOF_OFFSET);
  vm_page_unhold_pages(ma, cnt);
  adv:=len - short_uio.uio_resid;

  uio_clone^.uio_iov^.iov_base = (char *)uio_clone^.uio_iov^.iov_base + adv;
  uio_clone^.uio_iov^.iov_len -= adv;
  uio_clone^.uio_resid  -= adv;
  uio_clone^.uio_offset += adv;

  uio^.uio_resid  -= adv;
  uio^.uio_offset += adv;

  if (error<>0 or adv=0)
   break;
 end;
 td^.td_ma:=prev_td_ma;
 td^.td_ma_cnt:=prev_td_ma_cnt;
 curthread_pflags_restore(saveheld);
_out:
 vm_fault_enable_pagefaults(save);
 vn_rangelock_unlock(vp, rl_cookie);
 free(uio_clone, M_IOV);
}

out_last:
 foffset_unlock_uio(fp, uio, flags);
 Exit(error);
end;

{
 * File table truncate routine.
 }
function vn_truncate(fp:p_file;length:Int64):Integer;
label
 out1,
 _out;
var
 vattr:t_vattr;
 mp:p_mount;
 vp:p_vnode;
 rl_cookie:Pointer;
 vfslocked:Integer;
 error:Integer;
begin
 vp:=fp^.f_vnode;

 {
  * Lock the whole range for truncation.  Otherwise split i/o
  * might happen partly before and partly after the truncation.
  }
 rl_cookie:=vn_rangelock_wlock(vp, 0, High(Int64));
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 error:=vn_start_write(vp, @mp, V_WAIT or PCATCH);
 if (error<>0) then
 begin
  goto out1;
 end;

 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 if (vp^.v_type=VDIR) then
 begin
  error:=EISDIR;
  goto _out;
 end;

 //error:=mac_vnode_check_write(active_cred, fp^.f_cred, vp);
 //if (error<>0) then
 // goto _out;

 error:=vn_writechk(vp);
 if (error=0) then
 begin
  VATTR_NULL(@vattr);
  vattr.va_size:=length;
  error:=VOP_SETATTR(vp, @vattr);
 end;
_out:
 VOP_UNLOCK(vp, 0);
 vn_finished_write(mp);
out1:
 VFS_UNLOCK_GIANT(vfslocked);
 vn_rangelock_unlock(vp, rl_cookie);
 Exit(error);
end;

{
 * File table vnode ioctl routine.
 }
function vn_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
var
 vp:p_vnode;
 vattr:t_vattr;
 vfslocked:Integer;
 error:Integer;
begin
 vp:=fp^.f_vnode;

 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 error:=ENOTTY;
 case vp^.v_type of
  VREG,
  VDIR:
   begin
    if (com=FIONREAD) then
    begin
     vn_lock(vp, LK_SHARED or LK_RETRY);
     error:=VOP_GETATTR(vp, @vattr);
     VOP_UNLOCK(vp, 0);
     if (error=0) then
     begin
      PInteger(data)^:=vattr.va_size - fp^.f_offset;
     end;
    end else
    if (com=FIONBIO) or (com=FIOASYNC) then { XXX }
     error:=0
    else
     error:=VOP_IOCTL(vp, com, data, fp^.f_flag);
   end;
  else;
 end;
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

{
 * File table vnode poll routine.
 }
function vn_poll(fp:p_file;events:Integer):Integer;
var
 vp:p_vnode;
 vfslocked:Integer;
 error:Integer;
begin
 vp:=fp^.f_vnode;
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);

 //vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 //error:=mac_vnode_check_poll(active_cred, fp^.f_cred, vp);
 //VOP_UNLOCK(vp, 0);
 //if (error=0) then

 error:=VOP_POLL(vp, events);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

{
 * File table vnode stat routine.
 }
function vn_statfile(fp:p_file;sb:p_stat):Integer;
var
 vp:p_vnode;
 vfslocked:Integer;
 error:Integer;
begin
 vp:=fp^.f_vnode;
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 vn_lock(vp, LK_SHARED or LK_RETRY);
 error:=vn_stat(vp, sb);
 VOP_UNLOCK(vp, 0);
 VFS_UNLOCK_GIANT(vfslocked);

 Exit(error);
end;

{
 * File table vnode close routine.
 }
function vn_closefile(fp:p_file):Integer;
var
 vp:p_vnode;
 lf:t_flock;
 vfslocked:Integer;
 error:Integer;
begin
 vp:=fp^.f_vnode;

 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 if (fp^.f_type=DTYPE_VNODE) and ((fp^.f_flag and FHASLOCK)<>0) then
 begin
  lf.l_whence:=SEEK_SET;
  lf.l_start :=0;
  lf.l_len   :=0;
  lf.l_type  :=F_UNLCK;
  VOP_ADVLOCK(vp, fp, F_UNLCK, @lf, F_FLOCK);
 end;

 fp^.f_ops:=@badfileops;

 error:=vn_close(vp, fp^.f_flag);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function vn_chmod(fp:p_file;mode:mode_t):Integer;
var
 vp:p_vnode;
 error,vfslocked:Integer;
begin
 vp:=fp^.f_vnode;
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 error:=setfmode(vp, mode);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function vn_chown(fp:p_file;uid:uid_t;gid:gid_t):Integer;
var
 vp:p_vnode;
 error,vfslocked:Integer;
begin
 vp:=fp^.f_vnode;
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 error:=setfown(vp, uid, gid);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function vn_kqfilter(fp:p_file;kn:p_knote):Integer;
var
 error,vfslocked:Integer;
begin
 vfslocked:=VFS_LOCK_GIANT(fp^.f_vnode^.v_mount);
 error:=VOP_KQFILTER(fp^.f_vnode, kn);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;



end.

