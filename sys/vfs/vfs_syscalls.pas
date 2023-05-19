unit vfs_syscalls;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 time,
 vmount,
 vuio,
 vnamei,
 vstat,
 vfile,
 vfiledesc,
 vcapability,
 vfcntl,
 vfilio,
 vdisk,
 vfs_mount,
 vnode,
 vfs_vnops,
 vfs_subr,
 vfs_lookup;

function chroot_refuse_vdir_fds():Integer;
function getutimes(usrtvp:ptimeval;tvpseg:uio_seg;tsp:ptimespec):Integer;
function setfflags(vp:p_vnode;flags:Integer):Integer;
function setutimes(vp:p_vnode;ts:ptimespec;numtimes,nilflag:Integer):Integer;
function vn_access(vp:p_vnode;user_flags:Integer):Integer;
function setfown(vp:p_vnode;uid:uid_t;gid:gid_t):Integer;
function setfmode(vp:p_vnode;mode:Integer):Integer;
function getvnode(fd:Integer;rights:cap_rights_t;fpp:pp_file):Integer;

function sys_sync():Integer;
function sys_statfs(path:PChar;buf:p_statfs):Integer;
function sys_fstatfs(fd:Integer;buf:p_statfs):Integer;
function sys_getfsstat(buf:p_statfs;bufsize:QWORD;flags:Integer):Integer;
function sys_fchdir(fd:Integer):Integer;
function sys_chdir(path:PChar):Integer;
function sys_chroot(path:PChar):Integer;
function sys_open(path:PChar;flags,mode:Integer):Integer;
function sys_openat(fd:Integer;path:PChar;flags,mode:Integer):Integer;
function sys_mknod(path:PChar;mode,dev:Integer):Integer;
function sys_mknodat(fd:Integer;path:PChar;mode,dev:Integer):Integer;
function sys_mkfifo(path:PChar;mode:Integer):Integer;
function sys_mkfifoat(fd:Integer;path:PChar;mode:Integer):Integer;
function sys_link(path,link:PChar):Integer;
function sys_linkat(fd1:Integer;path1:PChar;fd2:Integer;path2:PChar;flag:Integer):Integer;
function sys_symlink(path,link:PChar):Integer;
function sys_symlinkat(path1:PChar;fd:Integer;path2:PChar):Integer;
function sys_unlinkat(fd:Integer;path:PChar;flag:Integer):Integer;
function sys_unlink(path:PChar):Integer;
function sys_lseek(fd:Integer;offset:Int64;whence:Integer):Integer;
function sys_access(path:PChar;flags:Integer):Integer;
function sys_stat(path:PChar;ub:p_stat):Integer;
function sys_fstatat(fd:Integer;path:PChar;buf:p_stat;flag:Integer):Integer;
function sys_lstat(path:PChar;ub:p_stat):Integer;
function sys_pathconf(path:PChar;name:Integer):Integer;
function sys_readlink(path,buf:PChar;count:QWORD):Integer;
function sys_chflags(path:PChar;flags:Integer):Integer;
function sys_lchflags(path:PChar;flags:Integer):Integer;
function sys_fchflags(fd,flags:Integer):Integer;
function sys_chmod(path:PChar;mode:Integer):Integer;
function sys_fchmodat(fd:Integer;path:PChar;mode,flag:Integer):Integer;
function sys_lchmod(path:PChar;mode:Integer):Integer;
function sys_fchmod(fd,mode:Integer):Integer;
function sys_chown(path:PChar;uid,gid:Integer):Integer;
function sys_fchownat(fd:Integer;path:PChar;uid,gid,flag:Integer):Integer;
function sys_lchown(path:PChar;uid,gid:Integer):Integer;
function sys_fchown(fd,uid,gid:Integer):Integer;
function sys_utimes(path:PChar;tptr:ptimeval):Integer;
function sys_futimesat(fd:Integer;path:PChar;times:ptimeval):Integer;
function sys_lutimes(path:PChar;tptr:ptimeval):Integer;
function sys_futimes(fd:Integer;tptr:ptimeval):Integer;
function sys_truncate(path:PChar;length:Int64):Integer;
function sys_fsync(fd:Integer):Integer;
function sys_fdatasync(fd:Integer):Integer;
function sys_rename(from,_to:PChar):Integer;
function sys_renameat(oldfd:Integer;old:PChar;newfd:Integer;new:PChar):Integer;
function sys_mkdir(path:PChar;mode:Integer):Integer;
function sys_mkdirat(fd:Integer;path:PChar;mode:Integer):Integer;
function sys_rmdir(path:PChar):Integer;
function sys_getdirentries(fd:Integer;buf:Pointer;count:DWORD;basep:PInt64):Integer;
function sys_getdents(fd:Integer;buf:Pointer;count:DWORD):Integer;
function sys_umask(newmask:Integer):Integer;
function sys_revoke(path:PChar):Integer;

function kern_statfs(path:PChar;pathseg:uio_seg;buf:p_statfs):Integer;
function kern_fstatfs(fd:Integer;buf:p_statfs):Integer;
function kern_getfsstat(buf:pp_statfs;bufsize:QWORD;bufseg:uio_seg;flags:Integer):Integer;
function kern_chdir(path:PChar;pathseg:uio_seg):Integer;
function kern_openat(fd:Integer;path:PChar;pathseg:uio_seg;flags,mode:Integer):Integer;
function kern_open(path:PChar;pathseg:uio_seg;flags,mode:Integer):Integer;
function kern_mkfifoat(fd:Integer;path:PChar;pathseg:uio_seg;mode:Integer):Integer;
function kern_mknodat(fd:Integer;path:PChar;pathseg:uio_seg;mode,dev:Integer):Integer;
function kern_mknod(path:PChar;pathseg:uio_seg;mode,dev:Integer):Integer;
function kern_linkat(fd1,fd2:Integer;path1,path2:PChar;segflg:uio_seg;follow:Integer):Integer;
function kern_link(path,link:PChar;segflg:uio_seg):Integer;
function kern_symlinkat(path1:PChar;fd:Integer;path2:PChar;segflg:uio_seg):Integer;
function kern_symlink(path,link:PChar;segflg:uio_seg):Integer;
function kern_unlinkat(fd:Integer;path:PChar;pathseg:uio_seg;oldinum:Integer):Integer;
function kern_unlink(path:PChar;pathseg:uio_seg):Integer;
function kern_accessat(fd:Integer;path:PChar;pathseg:uio_seg;flags,mode:Integer):Integer;
function kern_access(path:PChar;pathseg:uio_seg;mode:Integer):Integer;
function kern_statat(flag,fd:Integer;path:PChar;pathseg:uio_seg;sbp:p_stat):Integer;
function kern_stat(path:PChar;pathseg:uio_seg;sbp:p_stat):Integer;
function kern_lstat(path:PChar;pathseg:uio_seg;sbp:p_stat):Integer;
function kern_pathconf(path:PChar;pathseg:uio_seg;name:Integer;flags:QWORD):Integer;
function kern_readlinkat(fd:Integer;path:PChar;pathseg:uio_seg;buf:PChar;bufseg:uio_seg;count:QWORD):Integer;
function kern_readlink(path:PChar;pathseg:uio_seg;buf:PChar;bufseg:uio_seg;count:QWORD):Integer;
function kern_fchmodat(fd:Integer;path:PChar;pathseg:uio_seg;mode,flag:Integer):Integer;
function kern_chmod(path:PChar;pathseg:uio_seg;mode:Integer):Integer;
function kern_chown(path:PChar;pathseg:uio_seg;uid,gid:Integer):Integer;
function kern_lchown(path:PChar;pathseg:uio_seg;uid,gid:Integer):Integer;
function kern_utimesat(fd:Integer;path:PChar;pathseg:uio_seg;tptr:ptimeval;tptrseg:uio_seg):Integer;
function kern_utimes(path:PChar;pathseg:uio_seg;tptr:ptimeval;tptrseg:uio_seg):Integer;
function kern_lutimes(path:PChar;pathseg:uio_seg;tptr:ptimeval;tptrseg:uio_seg):Integer;
function kern_futimes(fd:Integer;tptr:ptimeval;tptrseg:uio_seg):Integer;
function kern_truncate(path:PChar;pathseg:uio_seg;length:Int64):Integer;
function kern_fsync(fd:Integer;fullsync:Boolean):Integer;
function kern_renameat(oldfd:Integer;old:PChar;newfd:Integer;new:PChar;pathseg:uio_seg):Integer;
function kern_rename(from,_to:PChar;pathseg:uio_seg):Integer;
function kern_mkdirat(fd:Integer;path:PChar;segflg:uio_seg;mode:Integer):Integer;
function kern_mkdir(path:PChar;segflg:uio_seg;mode:Integer):Integer;
function kern_rmdirat(fd:Integer;path:PChar;pathseg:uio_seg):Integer;
function kern_rmdir(path:PChar;pathseg:uio_seg):Integer;
function kern_getdirentries(fd:Integer;buf:Pointer;count:DWORD;basep:PInt64):Integer;

implementation

uses
 atomic,
 mqueue,
 systm,
 errno,
 kern_mtx,
 kern_thr,
 kern_synch,
 kern_descrip,
 vnode_if,
 sys_capability;

{
 * Sync each mounted filesystem.
 }
function sys_sync():Integer;
var
 mp,nmp:p_mount;
 save, vfslocked:Integer;
begin
 mtx_lock(mountlist_mtx);
 mp:=TAILQ_FIRST(@mountlist);
 while (mp<>nil) do
 begin
  if (vfs_busy(mp, MBF_NOWAIT or MBF_MNTLSTLOCK)<>0) then
  begin
   nmp:=TAILQ_NEXT(mp,@mp^.mnt_list);
   continue;
  end;
  vfslocked:=VFS_LOCK_GIANT(mp);
  if ((mp^.mnt_flag and MNT_RDONLY)=0) and
     (vn_start_write(nil, @mp, V_NOWAIT)=0) then
  begin
   save:=curthread_pflags_set(TDP_SYNCIO);
   vfs_msync(mp, MNT_NOWAIT);
   VFS_SYNC(mp, MNT_NOWAIT);
   curthread_pflags_restore(save);
   vn_finished_write(mp);
  end;
  VFS_UNLOCK_GIANT(vfslocked);
  mtx_lock(mountlist_mtx);
  nmp:=TAILQ_NEXT(mp,@mp^.mnt_list);
  vfs_unbusy(mp);

  mp:=nmp;
 end;
 mtx_unlock(mountlist_mtx);
 Exit(0);
end;

function kern_statfs(path:PChar;pathseg:uio_seg;buf:p_statfs):Integer;
label
 _out;
var
 mp:p_mount;
 sp:p_statfs;
 sb:t_statfs;
 vfslocked:Integer;
 error:Integer;
 nd:t_nameidata;
begin
 NDINIT(@nd, LOOKUP, FOLLOW or LOCKSHARED or LOCKLEAF or MPSAFE or AUDITVNODE1, pathseg, path, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 mp:=nd.ni_vp^.v_mount;
 vfs_ref(mp);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vput(nd.ni_vp);
 error:=vfs_busy(mp, 0);
 vfs_rel(mp);
 if (error<>0) then
 begin
  VFS_UNLOCK_GIANT(vfslocked);
  Exit(error);
 end;

 //error:=mac_mount_check_stat(td^.td_ucred, mp);
 //if (error<>0) then
 // goto _out;

 {
  * Set these in case the underlying filesystem fails to do so.
  }
 sp:=@mp^.mnt_stat;
 sp^.f_version:=STATFS_VERSION;
 sp^.f_namemax:=NAME_MAX;
 sp^.f_flags:=mp^.mnt_flag and MNT_VISFLAGMASK;
 error:=VFS_STATFS(mp, sp);
 if (error<>0) then
  goto _out;
 if {(priv_check(td, PRIV_VFS_GENERATION))} True then
 begin
  Move(sp^, sb, sizeof(sb));
  sb.f_fsid.val[0]:=0;
  sb.f_fsid.val[1]:=0;
  //prison_enforce_statfs(td^.td_ucred, mp, @sb);
  sp:=@sb;
 end;
 buf^:=sp^;
_out:
 vfs_unbusy(mp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

{
 * Get filesystem statistics.
 }
function sys_statfs(path:PChar;buf:p_statfs):Integer;
var
 sf:t_statfs;
 error:Integer;
begin
 error:=kern_statfs(path, UIO_USERSPACE, @sf);
 if (error=0) then
 begin
  error:=copyout(@sf, buf, sizeof(sf));
 end;
 Exit(error);
end;

function kern_fstatfs(fd:Integer;buf:p_statfs):Integer;
label
 _out;
var
 fp:p_file;
 mp:p_mount;
 sp:p_statfs;
 sb:t_statfs;
 vfslocked:Integer;
 vp:p_vnode;
 error:Integer;
begin
 error:=getvnode(fd, CAP_FSTATFS, @fp);
 if (error<>0) then
  Exit(error);
 vp:=fp^.f_vnode;
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 vn_lock(vp, LK_SHARED or LK_RETRY);
 mp:=vp^.v_mount;
 if (mp<>nil) then
  vfs_ref(mp);
 VOP_UNLOCK(vp, 0);
 fdrop(fp);
 if (mp=nil) then
 begin
  error:=EBADF;
  goto _out;
 end;
 error:=vfs_busy(mp, 0);
 vfs_rel(mp);
 if (error<>0) then
 begin
  VFS_UNLOCK_GIANT(vfslocked);
  Exit(error);
 end;

 //error:=mac_mount_check_stat(td^.td_ucred, mp);
 //if (error<>0) then
 // goto _out;

 {
  * Set these in case the underlying filesystem fails to do so.
  }
 sp:=@mp^.mnt_stat;
 sp^.f_version:=STATFS_VERSION;
 sp^.f_namemax:=NAME_MAX;
 sp^.f_flags:=mp^.mnt_flag and MNT_VISFLAGMASK;
 error:=VFS_STATFS(mp, sp);
 if (error<>0) then
  goto _out;
 if {(priv_check(td, PRIV_VFS_GENERATION))} True then
 begin
  Move(sp^, sb, sizeof(sb));
  sb.f_fsid.val[0]:=0;
  sb.f_fsid.val[1]:=0;
  //prison_enforce_statfs(td^.td_ucred, mp, @sb);
  sp:=@sb;
 end;
 buf^:=sp^;
_out:
 if (mp<>nil) then
  vfs_unbusy(mp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

{
 * Get filesystem statistics.
 }
function sys_fstatfs(fd:Integer;buf:p_statfs):Integer;
var
 sf:t_statfs;
 error:Integer;
begin
 error:=kern_fstatfs(fd, @sf);
 if (error=0) then
 begin
  error:=copyout(@sf, buf, sizeof(sf));
 end;
 Exit(error);
end;

{
 * If (bufsize > 0 and bufseg=UIO_SYSSPACE)
 *  The caller is responsible for freeing memory which will be allocated
 * in '*buf'.
 }

function kern_getfsstat(buf:pp_statfs;bufsize:QWORD;bufseg:uio_seg;flags:Integer):Integer;
var
 mp,nmp:p_mount;
 sfsp,sp:p_statfs;
 sb:t_statfs;
 count,maxcount:QWORD;
 vfslocked:Integer;
 error:Integer;
begin
 maxcount:=bufsize div sizeof(t_statfs);
 if (bufsize=0) then
 begin
  sfsp:=nil;
 end else
 if (bufseg=UIO_USERSPACE) then
 begin
  sfsp:=buf^;
 end else
 begin
  count:=0;
  mtx_lock(mountlist_mtx);
  mp:=TAILQ_FIRST(@mountlist);
  while (mp<>nil) do
  begin
   Inc(count);
   mp:=TAILQ_NEXT(mp,@mp^.mnt_list);
  end;
  mtx_unlock(mountlist_mtx);
  if (maxcount > count) then
   maxcount:=count;
  sfsp:=AllocMem(maxcount*sizeof(t_statfs));
  buf^:=sfsp;
 end;
 count:=0;
 mtx_lock(mountlist_mtx);
 mp:=TAILQ_FIRST(@mountlist);
 while (mp<>nil) do
 begin
  if {(prison_canseemount(td^.td_ucred, mp)<>0)} True then
  begin
   nmp:=TAILQ_NEXT(mp,@mp^.mnt_list);
   continue;
  end;

  //if (mac_mount_check_stat(td^.td_ucred, mp)<>0) then
  //begin
  // nmp:=TAILQ_NEXT(mp, mnt_list);
  // continue;
  //end;

  if (vfs_busy(mp, MBF_NOWAIT or MBF_MNTLSTLOCK)<>0) then
  begin
   nmp:=TAILQ_NEXT(mp,@mp^.mnt_list);
   continue;
  end;
  vfslocked:=VFS_LOCK_GIANT(mp);
  if (sfsp<>nil) and (count<maxcount) then
  begin
   sp:=@mp^.mnt_stat;
   {
    * Set these in case the underlying filesystem
    * fails to do so.
    }
   sp^.f_version:=STATFS_VERSION;
   sp^.f_namemax:=NAME_MAX;
   sp^.f_flags:=mp^.mnt_flag and MNT_VISFLAGMASK;
   {
    * If MNT_NOWAIT or MNT_LAZY is specified, do not
    * refresh the fsstat cache. MNT_NOWAIT or MNT_LAZY
    * overrides MNT_WAIT.
    }
   if ((flags and (MNT_LAZY or MNT_NOWAIT))=0) or
      ((flags and MNT_WAIT)<>0) then
   begin
    error:=VFS_STATFS(mp, sp);
    if (error<>0) then
    begin
     VFS_UNLOCK_GIANT(vfslocked);
     mtx_lock(mountlist_mtx);
     nmp:=TAILQ_NEXT(mp,@mp^.mnt_list);
     vfs_unbusy(mp);
     continue;
    end;
   end;
   if {(priv_check(td, PRIV_VFS_GENERATION))} True then
   begin
    Move(sp^, sb, sizeof(sb));
    sb.f_fsid.val[0]:=0;
    sb.f_fsid.val[1]:=0;
    //prison_enforce_statfs(td^.td_ucred, mp, @sb);
    sp:=@sb;
   end;
   if (bufseg=UIO_SYSSPACE) then
   begin
    Move(sp^, sfsp^, sizeof(sb));
   end else
   begin
    error:=copyout(sp, sfsp, sizeof(sb));
    if (error<>0) then
    begin
     vfs_unbusy(mp);
     VFS_UNLOCK_GIANT(vfslocked);
     Exit(error);
    end;
   end;
   Inc(sfsp);
  end;
  VFS_UNLOCK_GIANT(vfslocked);
  Inc(count);
  mtx_lock(mountlist_mtx);
  nmp:=TAILQ_NEXT(mp,@mp^.mnt_list);
  vfs_unbusy(mp);

  mp:=nmp;
 end;
 mtx_unlock(mountlist_mtx);
 if (sfsp<>nil) and (count>maxcount) then
  curkthread^.td_retval[0]:=maxcount
 else
  curkthread^.td_retval[0]:=count;
 Exit(0);
end;

{
 * Get statistics on all filesystems.
 }
function sys_getfsstat(buf:p_statfs;bufsize:QWORD;flags:Integer):Integer;
begin
 Exit(kern_getfsstat(@buf, bufsize, UIO_USERSPACE, flags));
end;

function change_dir(vp:p_vnode):Integer; forward;

{
 * Change current working directory to a given file descriptor.
 }
function sys_fchdir(fd:Integer):Integer;
var
 vp,tdp,vpold:p_vnode;
 mp:p_mount;
 fp:p_file;
 vfslocked:Integer;
 error:Integer;
 tvfslocked:Integer;
begin
 error:=getvnode(fd, CAP_FCHDIR, @fp);
 if (error<>0) then
  Exit(error);
 vp:=fp^.f_vnode;
 VREF(vp);
 fdrop(fp);
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 vn_lock(vp, LK_SHARED or LK_RETRY);

 error:=change_dir(vp);
 mp:=vp^.v_mountedhere;
 while (error=0) and (mp<>nil) do
 begin
  if (vfs_busy(mp, 0)<>0) then
   continue;
  tvfslocked:=VFS_LOCK_GIANT(mp);
  error:=VFS_ROOT(mp, LK_SHARED, @tdp);
  vfs_unbusy(mp);
  if (error<>0) then
  begin
   VFS_UNLOCK_GIANT(tvfslocked);
   break;
  end;
  vput(vp);
  VFS_UNLOCK_GIANT(vfslocked);
  vp:=tdp;
  vfslocked:=tvfslocked;

  mp:=vp^.v_mountedhere;
 end;
 if (error<>0) then
 begin
  vput(vp);
  VFS_UNLOCK_GIANT(vfslocked);
  Exit(error);
 end;
 VOP_UNLOCK(vp, 0);
 VFS_UNLOCK_GIANT(vfslocked);
 FILEDESC_XLOCK(@fd_table);
 vpold:=fd_table.fd_cdir;
 fd_table.fd_cdir:=vp;
 FILEDESC_XUNLOCK(@fd_table);
 vfslocked:=VFS_LOCK_GIANT(vpold^.v_mount);
 vrele(vpold);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(0);
end;

function kern_chdir(path:PChar;pathseg:uio_seg):Integer;
var
 error:Integer;
 nd:t_nameidata;
 vp:p_vnode;
 vfslocked:Integer;
begin
 NDINIT(@nd, LOOKUP, FOLLOW or LOCKSHARED or LOCKLEAF or AUDITVNODE1 or MPSAFE, pathseg, path, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 error:=change_dir(nd.ni_vp);
 if (error<>0) then
 begin
  vput(nd.ni_vp);
  VFS_UNLOCK_GIANT(vfslocked);
  NDFREE(@nd, NDF_ONLY_PNBUF);
  Exit(error);
 end;
 VOP_UNLOCK(nd.ni_vp, 0);
 VFS_UNLOCK_GIANT(vfslocked);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 FILEDESC_XLOCK(@fd_table);
 vp:=fd_table.fd_cdir;
 fd_table.fd_cdir:=nd.ni_vp;
 FILEDESC_XUNLOCK(@fd_table);
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 vrele(vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(0);
end;

{
 * Change current working directory (``.'').
 }
function sys_chdir(path:PChar):Integer;
begin
 Exit(kern_chdir(path, UIO_USERSPACE));
end;

{
 * Helper function for raised chroot(2) security function:  Refuse if
 * any filedescriptors are open directories.
 }
function chroot_refuse_vdir_fds():Integer;
var
 vp:p_vnode;
 fp:p_file;
 fd:Integer;
begin
 //FILEDESC_LOCK_ASSERT(@fd_table);

 For fd:=0 to fd_table.fd_nfiles-1 do
 begin
  fp:=fget_locked(@fd_table, fd);
  if (fp=nil) then
   continue;
  if (fp^.f_type=DTYPE_VNODE) then
  begin
   vp:=fp^.f_vnode;
   if (vp^.v_type=VDIR) then
    Exit(EPERM);
  end;
 end;
 Exit(0);
end;

{
 * This sysctl determines if we will allow a process to chroot(2) if it
 * has a directory open:
 * 0: disallowed for all processes.
 * 1: allowed for processes that were not already chroot(2)'ed.
 * 2: allowed for all processes.
 }
const
 chroot_allow_open_directories=0;

function change_root(vp:p_vnode):Integer; forward;

{
 * Change notion of root (``/'') directory.
 }
function sys_chroot(path:PChar):Integer;
label
 _error,
 e_vunlock;
var
 error:Integer;
 nd:t_nameidata;
 vfslocked:Integer;
begin
 error:=EPERM;
 //error:=priv_check(td, PRIV_VFS_CHROOT);
 if (error<>0) then
  Exit(error);
 NDINIT(@nd, LOOKUP, FOLLOW or LOCKSHARED or LOCKLEAF or MPSAFE or AUDITVNODE1, UIO_USERSPACE, path, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  goto _error;
 vfslocked:=NDHASGIANT(@nd);
 error:=change_dir(nd.ni_vp);
 if (error<>0) then
  goto e_vunlock;

 //if ((error:=mac_vnode_check_chroot(td^.td_ucred, nd.ni_vp)))
 // goto e_vunlock;

 VOP_UNLOCK(nd.ni_vp, 0);
 error:=change_root(nd.ni_vp);
 vrele(nd.ni_vp);
 VFS_UNLOCK_GIANT(vfslocked);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 Exit(error);
e_vunlock:
 vput(nd.ni_vp);
 VFS_UNLOCK_GIANT(vfslocked);
_error:
 NDFREE(@nd, NDF_ONLY_PNBUF);
 Exit(error);
end;

{
 * Common routine for chroot and chdir.  Callers must provide a locked vnode
 * instance.
 }

function change_dir(vp:p_vnode):Integer;
var
 error:Integer;
begin
 ASSERT_VOP_LOCKED(vp, 'change_dir(): vp not locked');
 if (vp^.v_type<>VDIR) then
  Exit(ENOTDIR);

 //error:=mac_vnode_check_chdir(td^.td_ucred, vp);
 //if (error<>0) then
 // Exit(error);

 error:=VOP_ACCESS(vp, VEXEC);
 Exit(error);
end;

{
 * Common routine for kern_chroot() and jail_attach().  The caller is
 * responsible for invoking priv_check() and mac_vnode_check_chroot() to
 * authorize this operation.
 }

function change_root(vp:p_vnode):Integer;
var
 oldvp:p_vnode;
 vfslocked:Integer;
 error:Integer;
begin
 VFS_ASSERT_GIANT(vp^.v_mount);
 FILEDESC_XLOCK(@fd_table);
 if (chroot_allow_open_directories=0) or
    ((chroot_allow_open_directories=1) and (fd_table.fd_rdir<>rootvnode)) then
 begin
  error:=chroot_refuse_vdir_fds();
  if (error<>0) then
  begin
   FILEDESC_XUNLOCK(@fd_table);
   Exit(error);
  end;
 end;
 oldvp:=fd_table.fd_rdir;
 fd_table.fd_rdir:=vp;
 VREF(fd_table.fd_rdir);
 if (fd_table.fd_jdir=nil) then
 begin
  fd_table.fd_jdir:=vp;
  VREF(fd_table.fd_jdir);
 end;
 FILEDESC_XUNLOCK(@fd_table);
 vfslocked:=VFS_LOCK_GIANT(oldvp^.v_mount);
 vrele(oldvp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(0);
end;

function flags_to_rights(flags:Integer):cap_rights_t;
var
 rights:cap_rights_t;
begin
 rights:=0;

 case (flags and O_ACCMODE) of
  O_RDONLY:
   begin
    rights:=rights or CAP_READ;
   end;

  O_RDWR:
   begin
    rights:=rights or CAP_READ or CAP_WRITE;
   end;

  O_WRONLY:
   begin
    rights:=rights or CAP_WRITE;
   end;

  O_EXEC:
   begin
    rights:=rights or CAP_FEXECVE;
   end;
 end;

 if ((flags and O_CREAT)<>0) then
  rights:=rights or CAP_CREATE;

 if ((flags and O_TRUNC)<>0) then
  rights:=rights or CAP_FTRUNCATE;

 if ((flags and O_EXLOCK)<>0) or ((flags and O_SHLOCK)<>0) then
  rights:=rights or CAP_FLOCK;

 Exit(rights);
end;

function kern_openat(fd:Integer;path:PChar;pathseg:uio_seg;flags,mode:Integer):Integer;
label
 success,
 bad_unlocked,
 bad;
var
 td:p_kthread;
 fp:p_file;
 vp:p_vnode;
 cmode:Integer;
 nfp:p_file;
 _type,indx,error,error_open:Integer;
 lf:t_flock;
 nd:t_nameidata;
 vfslocked:Integer;
 rights_needed:cap_rights_t;
begin
 td:=curkthread;
 indx:=-1;
 rights_needed:=CAP_LOOKUP;

 { XXX: audit dirfd }
 rights_needed:=rights_needed or flags_to_rights(flags);
 {
  * Only one of the O_EXEC, O_RDONLY, O_WRONLY and O_RDWR flags
  * may be specified.
  }
 if ((flags and O_EXEC)<>0) then
 begin
  if ((flags and O_ACCMODE)<>0) then
   Exit(EINVAL);
 end else
 if ((flags and O_ACCMODE)=O_ACCMODE) then
  Exit(EINVAL)
 else
  flags:=FFLAGS(flags);

 {
  * allocate the file descriptor, but don't install a descriptor yet
  }
 error:=falloc_noinstall(@nfp);
 if (error<>0) then
  Exit(error);
 { An extra reference on `nfp' has been held for us by falloc_noinstall(). }
 fp:=nfp;
 { Set the flags early so the finit in devfs can pick them up. }
 fp^.f_flag:=flags and FMASK;
 cmode:=((mode and (not fd_table.fd_cmask)) and ALLPERMS) and (not S_ISTXT);
 NDINIT_ATRIGHTS(@nd, LOOKUP, FOLLOW or AUDITVNODE1 or MPSAFE, pathseg, path, fd, rights_needed, td);
 td^.td_dupfd:=-1;  { XXX check for fdopen }
 error:=vn_open(@nd, @flags, cmode, fp);
 if (error<>0) then
 begin
  {
   * If the vn_open replaced the method vector, something
   * wonderous happened deep below and we just pass it up
   * pretending we know what we do.
   }
  if (error=ENXIO) and (fp^.f_ops<>@badfileops) then
   goto success;

  {
   * handle special fdopen() case.  bleh.  dupfdopen() is
   * responsible for dropping the old contents of ofiles[indx]
   * if it succeeds.
   *
   * Don't do this for relative (capability) lookups; we don't
   * understand exactly what would happen, and we don't think
   * that it ever should.
   }
  if (nd.ni_strictrelative=0) and
     ((error=ENODEV) or (error=ENXIO)) and
     (td^.td_dupfd >= 0) then
  begin
   { XXX from fdopen }
   error_open:=error;
   error:=finstall(fp, @indx, flags);
   if (error<>0) then
    goto bad_unlocked;
   error:=dupfdopen(indx, td^.td_dupfd, flags, error_open);
   if (error=0) then
    goto success;
  end;
  {
   * Clean up the descriptor, but only if another thread hadn't
   * replaced or closed it.
   }
  if (indx<>-1) then
  begin
   fdclose(fp, indx);
  end;
  fdrop(fp);

  Exit(error);
 end;
 td^.td_dupfd:=0;
 vfslocked:=NDHASGIANT(@nd);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vp:=nd.ni_vp;

 {
  * Store the vnode, for any f_type. Typically, the vnode use
  * count is decremented by direct call to vn_closefile() for
  * files that switched _type in the cdevsw fdopen() method.
  }
 fp^.f_vnode:=vp;
 {
  * If the file wasn't claimed by devfs bind it to the normal
  * vnode operations here.
  }
 if (fp^.f_ops=@badfileops) then
 begin
  Assert(vp^.v_type<>VFIFO,'Unexpected fifo.');
  fp^.f_seqcount:=1;
  finit(fp, flags and FMASK, DTYPE_VNODE, vp, @vnops);
 end;

 VOP_UNLOCK(vp, 0);
 if (fp^.f_type=DTYPE_VNODE) and ((flags and (O_EXLOCK or O_SHLOCK))<>0) then
 begin
  lf.l_whence:=SEEK_SET;
  lf.l_start:=0;
  lf.l_len:=0;
  if ((flags and O_EXLOCK)<>0) then
   lf.l_type:=F_WRLCK
  else
   lf.l_type:=F_RDLCK;
  _type:=F_FLOCK;
  if ((flags and FNONBLOCK)=0) then
   _type:=_type or F_WAIT;
  error:=VOP_ADVLOCK(vp, fp, F_SETLK, @lf, _type);
  if (error<>0) then
   goto bad;
  atomic_set_int(@fp^.f_flag, FHASLOCK);
 end;
 if ((flags and O_TRUNC)<>0) then
 begin
  error:=fo_truncate(fp, 0);
  if (error<>0) then
   goto bad;
 end;
 VFS_UNLOCK_GIANT(vfslocked);
success:
 {
  * If we haven't already installed the FD (for dupfdopen), do so now.
  }
 if (indx=-1) then
 begin

  if (nd.ni_strictrelative=1) then
  begin
   {
    * We are doing a strict relative lookup; wrap the
    * result in a capability.
    }
   error:=kern_capwrap(fp, nd.ni_baserights, @indx);
   if (error<>0) then
    goto bad_unlocked;
  end else
  begin
   error:=finstall(fp, @indx, flags);
   if (error<>0) then
    goto bad_unlocked;
  end;
 end;

 {
  * Release our private reference, leaving the one associated with
  * the descriptor table intact.
  }
 fdrop(fp);
 td^.td_retval[0]:=indx;
 Exit(0);
bad:
 VFS_UNLOCK_GIANT(vfslocked);
bad_unlocked:
 if (indx<>-1) then
  fdclose(fp, indx);
 fdrop(fp);
 td^.td_retval[0]:=QWORD(-1);
 Exit(error);
end;

function kern_open(path:PChar;pathseg:uio_seg;flags,mode:Integer):Integer;
begin
 Exit(kern_openat(AT_FDCWD, path, pathseg, flags, mode));
end;

{
 * Check permissions, allocate an open file structure, and call the device
 * open routine if any.
 }
function sys_open(path:PChar;flags,mode:Integer):Integer;
begin
 Exit(kern_open(path, UIO_USERSPACE, flags, mode));
end;

function sys_openat(fd:Integer;path:PChar;flags,mode:Integer):Integer;
begin
 Exit(kern_openat(fd, path, UIO_USERSPACE, flags, mode));
end;

function kern_mkfifoat(fd:Integer;path:PChar;pathseg:uio_seg;mode:Integer):Integer;
label
 restart,
 _out;
var
 mp:p_mount;
 vattr:t_vattr;
 error:Integer;
 nd:t_nameidata;
 vfslocked:Integer;
begin
restart:
 //bwillwrite();
 NDINIT_AT(@nd, CREATE, LOCKPARENT or SAVENAME or MPSAFE or AUDITVNODE1, pathseg, path, fd, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 if (nd.ni_vp<>nil) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  if (nd.ni_vp=nd.ni_dvp) then
   vrele(nd.ni_dvp)
  else
   vput(nd.ni_dvp);
  vrele(nd.ni_vp);
  VFS_UNLOCK_GIANT(vfslocked);
  Exit(EEXIST);
 end;
 if (vn_start_write(nd.ni_dvp, @mp, V_NOWAIT)<>0) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  vput(nd.ni_dvp);
  VFS_UNLOCK_GIANT(vfslocked);
  error:=vn_start_write(nil, @mp, V_XSLEEP or PCATCH);
  if (error<>0) then
   Exit(error);
  goto restart;
 end;
 VATTR_NULL(@vattr);
 vattr.va_type:=VFIFO;
 vattr.va_mode:=(mode and ALLPERMS) and (not fd_table.fd_cmask);

 //error:=mac_vnode_check_create(td^.td_ucred, nd.ni_dvp, @nd.ni_cnd, @vattr);
 //if (error<>0) then
 // goto _out;

 error:=VOP_MKNOD(nd.ni_dvp, @nd.ni_vp, @nd.ni_cnd, @vattr);
 if (error=0) then
  vput(nd.ni_vp);

_out:
 vput(nd.ni_dvp);
 vn_finished_write(mp);
 VFS_UNLOCK_GIANT(vfslocked);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 Exit(error);
end;

function kern_mknodat(fd:Integer;path:PChar;pathseg:uio_seg;mode,dev:Integer):Integer;
label
 restart;
var
 vp:p_vnode;
 mp:p_mount;
 vattr:t_vattr;
 error:Integer;
 whiteout:Integer;
 nd:t_nameidata;
 vfslocked:Integer;
begin
 whiteout:=0;

 case (mode and S_IFMT) of
  S_IFCHR,
  S_IFBLK:
   begin
    error:=EPERM;
    //error:=priv_check(td, PRIV_VFS_MKNOD_DEV);
   end;
  S_IFMT:
   begin
    error:=EPERM;
    //error:=priv_check(td, PRIV_VFS_MKNOD_BAD);
   end;
  S_IFWHT:
   begin
    error:=EPERM;
    //error:=priv_check(td, PRIV_VFS_MKNOD_WHT);
   end;
  S_IFIFO:
   begin
    if (dev=0) then
     Exit(kern_mkfifoat(fd, path, pathseg, mode));
    error:=EINVAL;
   end
  else
   error:=EINVAL;
 end;

 if (error<>0) then
  Exit(error);

restart:
 //bwillwrite();
 NDINIT_ATRIGHTS(@nd, CREATE, LOCKPARENT or SAVENAME or MPSAFE or AUDITVNODE1, pathseg, path, fd, CAP_MKFIFO, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 vp:=nd.ni_vp;
 if (vp<>nil) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  if (vp=nd.ni_dvp) then
   vrele(nd.ni_dvp)
  else
   vput(nd.ni_dvp);
  vrele(vp);
  VFS_UNLOCK_GIANT(vfslocked);
  Exit(EEXIST);
 end else
 begin
  VATTR_NULL(@vattr);
  vattr.va_mode:=(mode and ALLPERMS) and (not fd_table.fd_cmask);
  vattr.va_rdev:=dev;
  whiteout:=0;

  case (mode and S_IFMT) of
   S_IFMT: { used by badsect to flag bad sectors }
    vattr.va_type:=VBAD;
   S_IFCHR:
    vattr.va_type:=VCHR;
   S_IFBLK:
    vattr.va_type:=VBLK;
   S_IFWHT:
    whiteout:=1;
   else
    Assert(False,'kern_mknod: invalid mode');
  end;
 end;
 if (vn_start_write(nd.ni_dvp, @mp, V_NOWAIT)<>0) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  vput(nd.ni_dvp);
  VFS_UNLOCK_GIANT(vfslocked);
  error:=vn_start_write(nil, @mp, V_XSLEEP or PCATCH);
  if (error<>0) then
   Exit(error);
  goto restart;
 end;

 //if (error=0) and (whiteout=0) then
 // error:=mac_vnode_check_create(td^.td_ucred, nd.ni_dvp, @nd.ni_cnd, @vattr);

 if (error=0) then
 begin
  if (whiteout<>0) then
   error:=VOP_WHITEOUT(nd.ni_dvp, @nd.ni_cnd, CREATE)
  else begin
   error:=VOP_MKNOD(nd.ni_dvp, @nd.ni_vp, @nd.ni_cnd, @vattr);
   if (error=0) then
    vput(nd.ni_vp);
  end;
 end;
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vput(nd.ni_dvp);
 vn_finished_write(mp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function kern_mknod(path:PChar;pathseg:uio_seg;mode,dev:Integer):Integer;
begin
 Exit(kern_mknodat(AT_FDCWD, path, pathseg, mode, dev));
end;

{
 * Create a special file.
 }
function sys_mknod(path:PChar;mode,dev:Integer):Integer;
begin
 Exit(kern_mknod(path, UIO_USERSPACE, mode, dev));
end;

function sys_mknodat(fd:Integer;path:PChar;mode,dev:Integer):Integer;
begin
 Exit(kern_mknodat(fd, path, UIO_USERSPACE, mode, dev));
end;

function kern_mkfifo(path:PChar;pathseg:uio_seg;mode:Integer):Integer;
begin
 Exit(kern_mkfifoat(AT_FDCWD, path, pathseg, mode));
end;

{
 * Create a named pipe.
 }
function sys_mkfifo(path:PChar;mode:Integer):Integer;
begin
 Exit(kern_mkfifo(path, UIO_USERSPACE, mode));
end;

function sys_mkfifoat(fd:Integer;path:PChar;mode:Integer):Integer;
begin
 Exit(kern_mkfifoat(fd, path, UIO_USERSPACE, mode));
end;

function can_hardlink(vp:p_vnode):Integer; inline;
begin
 Exit(EPERM);
end;

function kern_linkat(fd1,fd2:Integer;path1,path2:PChar;segflg:uio_seg;follow:Integer):Integer;
var
 vp:p_vnode;
 mp:p_mount;
 nd:t_nameidata;
 vfslocked:Integer;
 lvfslocked:Integer;
 error:Integer;
begin
 //bwillwrite();
 NDINIT_AT(@nd, LOOKUP, follow or MPSAFE or AUDITVNODE1, segflg, path1, fd1, curkthread);

 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);

 vfslocked:=NDHASGIANT(@nd);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vp:=nd.ni_vp;
 if (vp^.v_type=VDIR) then
 begin
  vrele(vp);
  VFS_UNLOCK_GIANT(vfslocked);
  Exit(EPERM);  { POSIX }
 end;
 error:=vn_start_write(vp, @mp, V_WAIT or PCATCH);
 if (error<>0) then
 begin
  vrele(vp);
  VFS_UNLOCK_GIANT(vfslocked);
  Exit(error);
 end;
 NDINIT_AT(@nd, CREATE, LOCKPARENT or SAVENAME or MPSAFE or AUDITVNODE2, segflg, path2, fd2, curkthread);
 error:=nd_namei(@nd);
 if (error=0) then
 begin
  lvfslocked:=NDHASGIANT(@nd);
  if (nd.ni_vp<>nil) then
  begin
   if (nd.ni_dvp=nd.ni_vp) then
    vrele(nd.ni_dvp)
   else
    vput(nd.ni_dvp);
   vrele(nd.ni_vp);
   error:=EEXIST;
  end else
  begin
   error:=vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
   if (error=0) then
   begin
    error:=can_hardlink(vp);
    if (error=0) then

     // error:=mac_vnode_check_link(td^.td_ucred, nd.ni_dvp, vp, @nd.ni_cnd);
     //if (error=0) then

     error:=VOP_LINK(nd.ni_dvp, vp, @nd.ni_cnd);
    VOP_UNLOCK(vp, 0);
    vput(nd.ni_dvp);
   end;
  end;
  NDFREE(@nd, NDF_ONLY_PNBUF);
  VFS_UNLOCK_GIANT(lvfslocked);
 end;
 vrele(vp);
 vn_finished_write(mp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function kern_link(path,link:PChar;segflg:uio_seg):Integer;
begin
 Exit(kern_linkat(AT_FDCWD, AT_FDCWD, path,link, segflg, FOLLOW));
end;

{
 * Make a hard file link.
 }
function sys_link(path,link:PChar):Integer;
begin
 Exit(kern_link(path, link, UIO_USERSPACE));
end;

function sys_linkat(fd1:Integer;path1:PChar;fd2:Integer;path2:PChar;flag:Integer):Integer;
begin
 if ((flag and (not AT_SYMLINK_FOLLOW))<>0) then
  Exit(EINVAL);

 if ((flag and AT_SYMLINK_FOLLOW)<>0) then
 begin
  Exit(kern_linkat(fd1, fd2, path1, path2, UIO_USERSPACE, FOLLOW));
 end else
 begin
  Exit(kern_linkat(fd1, fd2, path1, path2, UIO_USERSPACE, NOFOLLOW));
 end;
end;

function kern_symlinkat(path1:PChar;fd:Integer;path2:PChar;segflg:uio_seg):Integer;
label
 _out,
 restart,
 out2;
var
 mp:p_mount;
 vattr:t_vattr;
 syspath:PChar;
 error:Integer;
 nd:t_nameidata;
 vfslocked:Integer;
begin
 if (segflg=UIO_SYSSPACE) then
 begin
  syspath:=path1;
 end else
 begin
  syspath:=AllocMem(SizeOf(MAXPATHLEN));
  error:=copyinstr(path1, syspath, MAXPATHLEN, nil);
  if (error<>0) then
   goto _out;
 end;
 restart:
 //bwillwrite();
 NDINIT_AT(@nd, CREATE, LOCKPARENT or SAVENAME or MPSAFE or AUDITVNODE1, segflg, path2, fd, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  goto _out;
 vfslocked:=NDHASGIANT(@nd);
 if (nd.ni_vp<>nil) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  if (nd.ni_vp=nd.ni_dvp) then
   vrele(nd.ni_dvp)
  else
   vput(nd.ni_dvp);
  vrele(nd.ni_vp);
  VFS_UNLOCK_GIANT(vfslocked);
  error:=EEXIST;
  goto _out;
 end;
 if (vn_start_write(nd.ni_dvp, @mp, V_NOWAIT)<>0) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  vput(nd.ni_dvp);
  VFS_UNLOCK_GIANT(vfslocked);
  error:=vn_start_write(nil, @mp, V_XSLEEP or PCATCH);
  if (error<>0) then
   goto _out;
  goto restart;
 end;
 VATTR_NULL(@vattr);
 vattr.va_mode:=ACCESSPERMS and (not fd_table.fd_cmask);

 //vattr.va_type:=VLNK;
 //error:=mac_vnode_check_create(td^.td_ucred, nd.ni_dvp, @nd.ni_cnd, @vattr);
 //if (error)
 // goto out2;

 error:=VOP_SYMLINK(nd.ni_dvp, @nd.ni_vp, @nd.ni_cnd, @vattr, syspath);
 if (error=0) then
  vput(nd.ni_vp);
out2:
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vput(nd.ni_dvp);
 vn_finished_write(mp);
 VFS_UNLOCK_GIANT(vfslocked);
_out:
 if (segflg<>UIO_SYSSPACE) then
  FreeMem(syspath);
 Exit(error);
end;

function kern_symlink(path,link:PChar;segflg:uio_seg):Integer;
begin
 Exit(kern_symlinkat(path, AT_FDCWD, link, segflg));
end;

{
 * Make a symbolic link.
 }
function sys_symlink(path,link:PChar):Integer;
begin
 Exit(kern_symlink(path, link, UIO_USERSPACE));
end;

function sys_symlinkat(path1:PChar;fd:Integer;path2:PChar):Integer;
begin
 Exit(kern_symlinkat(path1, fd, path2, UIO_USERSPACE));
end;

function kern_unlinkat(fd:Integer;path:PChar;pathseg:uio_seg;oldinum:Integer):Integer;
label
 restart,
 _out;
var
 mp:p_mount;
 vp:p_vnode;
 error:Integer;
 nd:t_nameidata;
 sb:t_stat;
 vfslocked:Integer;

 function _vn_stat:Integer; inline;
 begin
  error:=vn_stat(vp, @sb);
  Result:=error;
 end;

begin
 restart:
 //bwillwrite();
 NDINIT_AT(@nd, DELETE, LOCKPARENT or LOCKLEAF or MPSAFE or AUDITVNODE1, pathseg, path, fd, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
 begin
  if (error=EINVAL) then
  begin
   Exit(EPERM)
  end else
  begin
   Exit(error);
  end;
 end;
 vfslocked:=NDHASGIANT(@nd);
 vp:=nd.ni_vp;
 if (vp^.v_type=VDIR) and (oldinum=0) then
 begin
  error:=EPERM;  { POSIX }
 end else
 if (oldinum<>0) and
    (_vn_stat=0) and
    (sb.st_ino<>oldinum) then
 begin
  error:=EIDRM; { Identifier removed }
 end else
 begin
  {
   * The root of a mounted filesystem cannot be deleted.
   *
   * XXX: can this only be a VDIR case?
   }
  if ((vp^.v_vflag and VV_ROOT)<>0) then
   error:=EBUSY;
 end;
 if (error=0) then
 begin
  if (vn_start_write(nd.ni_dvp, @mp, V_NOWAIT)<>0) then
  begin
   NDFREE(@nd, NDF_ONLY_PNBUF);
   vput(nd.ni_dvp);
   if (vp=nd.ni_dvp) then
    vrele(vp)
   else
    vput(vp);
   VFS_UNLOCK_GIANT(vfslocked);
   error:=vn_start_write(nil, @mp, V_XSLEEP or PCATCH);
   if (error<>0) then
    Exit(error);
   goto restart;
  end;

  //error:=mac_vnode_check_unlink(td^.td_ucred, nd.ni_dvp, vp, @nd.ni_cnd);
  //if (error)
  // goto _out;

  error:=VOP_REMOVE(nd.ni_dvp, vp, @nd.ni_cnd);

  _out:
  vn_finished_write(mp);
 end;
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vput(nd.ni_dvp);
 if (vp=nd.ni_dvp) then
  vrele(vp)
 else
  vput(vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function kern_unlink(path:PChar;pathseg:uio_seg):Integer;
begin
 Exit(kern_unlinkat(AT_FDCWD, path, pathseg, 0));
end;

function sys_unlinkat(fd:Integer;path:PChar;flag:Integer):Integer;
begin
 if ((flag and (not AT_REMOVEDIR))<>0) then
  Exit(EINVAL);

 if ((flag and AT_REMOVEDIR)<>0) then
  Exit(kern_rmdirat(fd, path, UIO_USERSPACE))
 else
  Exit(kern_unlinkat(fd, path, UIO_USERSPACE, 0));
end;

{
 * Delete a name from the filesystem.
 }
function sys_unlink(path:PChar):Integer;
begin
 Exit(kern_unlink(path, UIO_USERSPACE));
end;

{
 * Reposition read/write file offset.
 }
function sys_lseek(fd:Integer;offset:Int64;whence:Integer):Integer;
label
 drop,
 _break;
var
 fp:p_file;
 vp:p_vnode;
 vattr:t_vattr;
 foffset,size:Int64;
 error,noneg:Integer;
 vfslocked:Integer;
begin
 error:=fget(fd, CAP_SEEK, @fp);
 if (error<>0) then
  Exit(error);
 if ((fp^.f_ops^.fo_flags and DFLAG_SEEKABLE)=0) then
 begin
  fdrop(fp);
  Exit(ESPIPE);
 end;
 vp:=fp^.f_vnode;
 foffset:=foffset_lock(fp, 0);
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 noneg:=ord(vp^.v_type<>VCHR);
 case whence of
  L_INCR:
   begin
    if (noneg<>0) and
       ((foffset < 0) or
        ((offset > 0) and (foffset > High(Int64) - offset))) then
    begin
     error:=EOVERFLOW;
     goto _break;
    end;
    Inc(offset,foffset);
   end;
  L_XTND:
   begin
    vn_lock(vp, LK_SHARED or LK_RETRY);
    error:=VOP_GETATTR(vp, @vattr);
    VOP_UNLOCK(vp, 0);
    if (error<>0) then
     goto _break;

    {
     * If the file references a disk device, then fetch
     * the media size and use that to determine the ending
     * offset.
     }
    if (vattr.va_size=0) and
       (vp^.v_type=VCHR) and
       (fo_ioctl(fp, DIOCGMEDIASIZE, @size)=0) then
     vattr.va_size:=size;

    if (noneg<>0) and
       ((vattr.va_size > High(Int64)) or
        ((offset > 0) and (vattr.va_size > High(Int64) - offset))) then
    begin
     error:=EOVERFLOW;
     goto _break;
    end;
    Inc(offset,vattr.va_size);
   end;
  L_SET,
  SEEK_DATA:
   error:=fo_ioctl(fp, FIOSEEKDATA, @offset);
  SEEK_HOLE:
   error:=fo_ioctl(fp, FIOSEEKHOLE, @offset);
  else
   error:=EINVAL;
 end;
 _break:
 if (error=0) and (noneg<>0) and (offset < 0) then
  error:=EINVAL;
 if (error<>0) then
  goto drop;
 VFS_KNOTE_UNLOCKED(vp, 0);
 curkthread^.td_retval[0]:=offset;
drop:
 fdrop(fp);
 VFS_UNLOCK_GIANT(vfslocked);

 if (error<>0) then
 begin
  foffset_unlock(fp, offset, FOF_NOUPDATE);
 end else
 begin
  foffset_unlock(fp, offset, 0);
 end;

 Exit(error);
end;

{
 * Check access permissions using passed credentials.
 }
function vn_access(vp:p_vnode;user_flags:Integer):Integer;
var
 error:Integer;
 accmode:accmode_t;

 function _vn_writechk:Integer; inline;
 begin
  error:=vn_writechk(vp);
  Result:=error;
 end;

begin
 { Flags=0 means only check for existence. }
 error:=0;
 if (user_flags<>0) then
 begin
  accmode:=0;
  if ((user_flags and R_OK)<>0) then
   accmode:=accmode or VREAD;
  if ((user_flags and W_OK)<>0) then
   accmode:=accmode or VWRITE;
  if ((user_flags and X_OK)<>0) then
   accmode:=accmode or VEXEC;

  //error:=mac_vnode_check_access(cred, vp, accmode);
  //if (error<>0) then
  // Exit(error);

  if ((accmode and VWRITE)=0) or (_vn_writechk=0) then
  begin
   error:=VOP_ACCESS(vp, accmode);
  end;
 end;
 Exit(error);
end;

function kern_accessat(fd:Integer;path:PChar;pathseg:uio_seg;flags,mode:Integer):Integer;
label
 out1;
var
 vp:p_vnode;
 nd:t_nameidata;
 vfslocked:Integer;
 error:Integer;
begin
 NDINIT_ATRIGHTS(@nd, LOOKUP, FOLLOW or LOCKSHARED or LOCKLEAF or MPSAFE or AUDITVNODE1, pathseg, path, fd, CAP_FSTAT, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  goto out1;
 vfslocked:=NDHASGIANT(@nd);
 vp:=nd.ni_vp;

 error:=vn_access(vp, mode);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vput(vp);
 VFS_UNLOCK_GIANT(vfslocked);
out1:
 Exit(error);
end;

function kern_access(path:PChar;pathseg:uio_seg;mode:Integer):Integer;
begin
 Exit(kern_accessat(AT_FDCWD, path, pathseg, 0, mode));
end;

{
 * Check access permissions using 'real' credentials.
 }
function sys_access(path:PChar;flags:Integer):Integer;
begin
 Exit(kern_access(path, UIO_USERSPACE, flags));
end;

type
 t_statat_hook_cb=procedure(vp:p_vnode;sbp:p_stat);

function kern_statat_vnhook(flag,fd:Integer;
                            path:PChar;
                            pathseg:uio_seg;
                            sbp:p_stat;
                            hook:t_statat_hook_cb):Integer;
var
 nd:t_nameidata;
 sb:t_stat;
 error, vfslocked:Integer;
begin
 if (flag and (not AT_SYMLINK_NOFOLLOW))<>0 then
  Exit(EINVAL);

 if ((flag and AT_SYMLINK_NOFOLLOW)<>0) then
 begin
  NDINIT_ATRIGHTS(@nd, LOOKUP, NOFOLLOW or LOCKSHARED or LOCKLEAF or AUDITVNODE1 or MPSAFE, pathseg,
      path, fd, CAP_FSTAT, curkthread);
 end else
 begin
  NDINIT_ATRIGHTS(@nd, LOOKUP, FOLLOW or LOCKSHARED or LOCKLEAF or AUDITVNODE1 or MPSAFE, pathseg,
      path, fd, CAP_FSTAT, curkthread);
 end;

 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);

 vfslocked:=NDHASGIANT(@nd);
 error:=vn_stat(nd.ni_vp, @sb);
 if (error=0) then
 begin
  if (hook<>nil) then
   hook(nd.ni_vp, @sb);
 end;
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vput(nd.ni_vp);
 VFS_UNLOCK_GIANT(vfslocked);
 if (error<>0) then
  Exit(error);

 sbp^:=sb;
 Exit(0);
end;

function kern_statat(flag,fd:Integer;path:PChar;
                     pathseg:uio_seg;sbp:p_stat):Integer;
begin
 Exit(kern_statat_vnhook(flag, fd, path, pathseg, sbp, nil));
end;

function kern_stat(path:PChar;pathseg:uio_seg;sbp:p_stat):Integer;
begin
 Exit(kern_statat(0, AT_FDCWD, path, pathseg, sbp));
end;

{
 * Get file status; this version follows links.
 }
function sys_stat(path:PChar;ub:p_stat):Integer;
var
 sb:t_stat;
 error:Integer;
begin
 error:=kern_stat(path, UIO_USERSPACE, @sb);
 if (error=0) then
  error:=copyout(@sb, ub, sizeof(sb));
 Exit(error);
end;

function sys_fstatat(fd:Integer;path:PChar;buf:p_stat;flag:Integer):Integer;
var
 sb:p_stat;
 error:Integer;
begin
 error:=kern_statat(flag, fd, path, UIO_USERSPACE, @sb);
 if (error=0) then
  error:=copyout(@sb, buf, sizeof(sb));
 Exit(error);
end;

function kern_lstat(path:PChar;pathseg:uio_seg;sbp:p_stat):Integer;
begin
 Exit(kern_statat(AT_SYMLINK_NOFOLLOW, AT_FDCWD, path, pathseg, sbp));
end;

{
 * Get file status; this version does not follow links.
 }
function sys_lstat(path:PChar;ub:p_stat):Integer;
var
 sb:t_stat;
 error:Integer;
begin
 error:=kern_lstat(path, UIO_USERSPACE, @sb);
 if (error=0) then
  error:=copyout(@sb, ub, sizeof(sb));
 Exit(error);
end;

function kern_pathconf(path:PChar;pathseg:uio_seg;name:Integer;flags:QWORD):Integer;
var
 td:p_kthread;
 nd:t_nameidata;
 error,vfslocked:Integer;
begin
 td:=curkthread;
 NDINIT(@nd, LOOKUP, LOCKSHARED or LOCKLEAF or MPSAFE or AUDITVNODE1 or flags, pathseg, path, td);
 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 NDFREE(@nd, NDF_ONLY_PNBUF);

 { If asynchronous I/O is available, it works for all files. }
 if (name=_PC_ASYNC_IO) then
  td^.td_retval[0]:=async_io_version
 else
  error:=VOP_PATHCONF(nd.ni_vp, name, @td^.td_retval);

 vput(nd.ni_vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

{
 * Get configurable pathname variables.
 }
function sys_pathconf(path:PChar;name:Integer):Integer;
begin
 Exit(kern_pathconf(path, UIO_USERSPACE, name, FOLLOW));
end;

function kern_readlinkat(fd:Integer;path:PChar;pathseg:uio_seg;
                         buf:PChar;bufseg:uio_seg;count:QWORD):Integer;
var
 td:p_kthread;
 vp:p_vnode;
 aiov:iovec;
 auio:t_uio;
 error:Integer;
 nd:t_nameidata;
 vfslocked:Integer;
begin
 if (count > IOSIZE_MAX) then
  Exit(EINVAL);

 td:=curkthread;
 NDINIT_AT(@nd, LOOKUP, NOFOLLOW or LOCKSHARED or LOCKLEAF or MPSAFE or AUDITVNODE1, pathseg, path, fd, td);

 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vfslocked:=NDHASGIANT(@nd);
 vp:=nd.ni_vp;


 //error:=mac_vnode_check_readlink(td^.td_ucred, vp);
 //if (error<>0) then
 //begin
 // vput(vp);
 // VFS_UNLOCK_GIANT(vfslocked);
 // Exit(error);
 //end;

 if (vp^.v_type<>VLNK) then
  error:=EINVAL
 else
 begin
  aiov.iov_base  :=buf;
  aiov.iov_len   :=count;
  auio.uio_iov   :=@aiov;
  auio.uio_iovcnt:=1;
  auio.uio_offset:=0;
  auio.uio_rw    :=UIO_READ;
  auio.uio_segflg:=bufseg;
  auio.uio_td    :=td;
  auio.uio_resid :=count;
  error:=VOP_READLINK(vp, @auio);
  td^.td_retval[0]:=count - auio.uio_resid;
 end;
 vput(vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function kern_readlink(path:PChar;pathseg:uio_seg;buf:PChar;
                       bufseg:uio_seg;count:QWORD):Integer;
begin
 Exit(kern_readlinkat(AT_FDCWD, path, pathseg, buf, bufseg, count));
end;

{
 * Exittarget name of a symbolic link.
 }
function sys_readlink(path,buf:PChar;count:QWORD):Integer;
begin
 Exit(kern_readlink(path, UIO_USERSPACE, buf, UIO_USERSPACE, count));
end;

{
 * Common implementation code for chflags() and fchflags().
 }
function setfflags(vp:p_vnode;flags:Integer):Integer;
var
 error:Integer;
 mp:p_mount;
 vattr:t_vattr;
begin
 { We can't support the value matching VNOVAL. }
 if (flags=VNOVAL) then
  Exit(EOPNOTSUPP);

 {
  * Prevent non-root users from setting flags on devices.  When
  * a device is reused, users can retain ownership of the device
  * if they are allowed to set flags and programs assume that
  * chown can't fail when done as root.
  }
 if (vp^.v_type=VCHR) or (vp^.v_type=VBLK) then
 begin
  error:=EPERM;
  //error:=priv_check(td, PRIV_VFS_CHFLAGS_DEV);
  if (error<>0) then
   Exit(error);
 end;

 error:=vn_start_write(vp, @mp, V_WAIT or PCATCH);
 if (error<>0) then
  Exit(error);
 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 VATTR_NULL(@vattr);
 vattr.va_flags:=flags;

 //error:=mac_vnode_check_setflags(td^.td_ucred, vp, vattr.va_flags);
 //if (error=0) then

  error:=VOP_SETATTR(vp, @vattr);
 VOP_UNLOCK(vp, 0);
 vn_finished_write(mp);
 Exit(error);
end;

{
 * Change flags of a file given a path name.
 }
function sys_chflags(path:PChar;flags:Integer):Integer;
var
 error:Integer;
 nd:t_nameidata;
 vfslocked:Integer;
begin
 NDINIT(@nd, LOOKUP, FOLLOW or MPSAFE or AUDITVNODE1, UIO_USERSPACE, path, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vfslocked:=NDHASGIANT(@nd);
 error:=setfflags(nd.ni_vp, flags);
 vrele(nd.ni_vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

{
 * Same as chflags() but doesn't follow symlinks.
 }
function sys_lchflags(path:PChar;flags:Integer):Integer;
var
 error:Integer;
 nd:t_nameidata;
 vfslocked:Integer;
begin
 NDINIT(@nd, LOOKUP, NOFOLLOW or MPSAFE or AUDITVNODE1, UIO_USERSPACE, path, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 error:=setfflags(nd.ni_vp, flags);
 vrele(nd.ni_vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

{
 * Change flags of a file given a file descriptor.
 }
function sys_fchflags(fd,flags:Integer):Integer;
var
 fp:p_file;
 vfslocked:Integer;
 error:Integer;
begin
 error:=getvnode(fd, CAP_FCHFLAGS, @fp);
 if (error<>0) then
  Exit(error);

 vfslocked:=VFS_LOCK_GIANT(fp^.f_vnode^.v_mount);

 error:=setfflags(fp^.f_vnode, flags);
 VFS_UNLOCK_GIANT(vfslocked);
 fdrop(fp);
 Exit(error);
end;

{
 * Common implementation code for chmod(), lchmod() and fchmod().
 }
function setfmode(vp:p_vnode;mode:Integer):Integer;
var
 error:Integer;
 mp:p_mount;
 vattr:t_vattr;
begin
 error:=vn_start_write(vp, @mp, V_WAIT or PCATCH);
 if (error<>0) then
  Exit(error);
 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 VATTR_NULL(@vattr);
 vattr.va_mode:=mode and ALLPERMS;

 //error:=mac_vnode_check_setmode(cred, vp, vattr.va_mode);
 //if (error=0) then

  error:=VOP_SETATTR(vp, @vattr);
 VOP_UNLOCK(vp, 0);
 vn_finished_write(mp);
 Exit(error);
end;

function kern_fchmodat(fd:Integer;path:PChar;pathseg:uio_seg;mode,flag:Integer):Integer;
var
 error:Integer;
 nd:t_nameidata;
 vfslocked:Integer;
 follow:Integer;
begin
 if (flag and AT_SYMLINK_NOFOLLOW)<>0 then
 begin
  follow:=NOFOLLOW;
 end else
 begin
  follow:=FOLLOW;
 end;

 NDINIT_ATRIGHTS(@nd, LOOKUP,  follow or MPSAFE or AUDITVNODE1, pathseg, path, fd, CAP_FCHMOD, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 error:=setfmode(nd.ni_vp, mode);
 vrele(nd.ni_vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function kern_chmod(path:PChar;pathseg:uio_seg;mode:Integer):Integer;
begin
 Exit(kern_fchmodat(AT_FDCWD, path, pathseg, mode, 0));
end;

{
 * Change mode of a file given path name.
 }
function sys_chmod(path:PChar;mode:Integer):Integer;
begin
 Exit(kern_chmod(path, UIO_USERSPACE, mode));
end;

function sys_fchmodat(fd:Integer;path:PChar;mode,flag:Integer):Integer;
begin
 if ((flag and (not AT_SYMLINK_NOFOLLOW))<>0) then
  Exit(EINVAL);

 Exit(kern_fchmodat(fd, path, UIO_USERSPACE, mode, flag));
end;

{
 * Change mode of a file given path name (don't follow links.)
 }
function sys_lchmod(path:PChar;mode:Integer):Integer;
begin
 Exit(kern_fchmodat(AT_FDCWD, path, UIO_USERSPACE, mode, AT_SYMLINK_NOFOLLOW));
end;

{
 * Change mode of a file given a file descriptor.
 }
function sys_fchmod(fd,mode:Integer):Integer;
var
 fp:p_file;
 error:Integer;
begin
 error:=fget(fd, CAP_FCHMOD, @fp);
 if (error<>0) then
  Exit(error);
 error:=fo_chmod(fp, mode);
 fdrop(fp);
 Exit(error);
end;

{
 * Common implementation for chown(), lchown(), and fchown()
 }
function setfown(vp:p_vnode;uid:uid_t;gid:gid_t):Integer;
var
 error:Integer;
 mp:p_mount;
 vattr:t_vattr;
begin
 error:=vn_start_write(vp, @mp, V_WAIT or PCATCH);

 if (error<>0) then
  Exit(error);
 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 VATTR_NULL(@vattr);
 vattr.va_uid:=uid;
 vattr.va_gid:=gid;

 //error:=mac_vnode_check_setowner(cred, vp, vattr.va_uid,
 //    vattr.va_gid);
 //if (error=0) then

  error:=VOP_SETATTR(vp, @vattr);
 VOP_UNLOCK(vp, 0);
 vn_finished_write(mp);
 Exit(error);
end;

function kern_fchownat(fd:Integer;path:PChar;pathseg:uio_seg;uid,gid,flag:Integer):Integer;
var
 nd:t_nameidata;
 error,vfslocked,follow:Integer;
begin
 if (flag and AT_SYMLINK_NOFOLLOW)<>0 then
 begin
  follow:=NOFOLLOW;
 end else
 begin
  follow:=FOLLOW;
 end;

 NDINIT_ATRIGHTS(@nd, LOOKUP, follow or MPSAFE or AUDITVNODE1, pathseg, path, fd, CAP_FCHOWN, curkthread);

 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 error:=setfown(nd.ni_vp, uid, gid);
 vrele(nd.ni_vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function kern_chown(path:PChar;pathseg:uio_seg;uid,gid:Integer):Integer;
begin
 Exit(kern_fchownat(AT_FDCWD, path, pathseg, uid, gid, 0));
end;

{
 * Set ownership given a path name.
 }
function sys_chown(path:PChar;uid,gid:Integer):Integer;
begin
 Exit(kern_chown(path, UIO_USERSPACE, uid, gid));
end;

function sys_fchownat(fd:Integer;path:PChar;uid,gid,flag:Integer):Integer;
begin
 if ((flag and (not AT_SYMLINK_NOFOLLOW))<>0) then
  Exit(EINVAL);

 Exit(kern_fchownat(fd, path, UIO_USERSPACE, uid, gid, flag));
end;

function kern_lchown(path:PChar;pathseg:uio_seg;uid,gid:Integer):Integer;
begin
 Exit(kern_fchownat(AT_FDCWD, path, pathseg, uid, gid,AT_SYMLINK_NOFOLLOW));
end;

{
 * Set ownership given a path name, do not cross symlinks.
 }
function sys_lchown(path:PChar;uid,gid:Integer):Integer;
begin
 Exit(kern_lchown(path, UIO_USERSPACE, uid, gid));
end;

{
 * Set ownership given a file descriptor.
 }
function sys_fchown(fd,uid,gid:Integer):Integer;
var
 fp:p_file;
 error:Integer;
begin
 error:=fget(fd, CAP_FCHOWN, @fp);
 if (error<>0) then
  Exit(error);
 error:=fo_chown(fp, uid, gid);
 fdrop(fp);
 Exit(error);
end;

{
 * Common implementation code for utimes(), lutimes(), and futimes().
 }
function getutimes(usrtvp:ptimeval;tvpseg:uio_seg;tsp:ptimespec):Integer;
var
 tv:array[0..1] of timeval;
 tvp:ptimeval;
 error:Integer;
begin
 if (usrtvp=nil) then
 begin
  vfs_timestamp(@tsp[0]);
  tsp[1]:=tsp[0];
 end else
 begin
  if (tvpseg=UIO_SYSSPACE) then
  begin
   tvp:=usrtvp;
  end else
  begin
   error:=copyin(usrtvp, @tv, sizeof(tv));
   if (error<>0) then
    Exit(error);
   tvp:=tv;
  end;

  if (tvp[0].tv_usec < 0) or (tvp[0].tv_usec >= 1000000) or
     (tvp[1].tv_usec < 0) or (tvp[1].tv_usec >= 1000000) then
   Exit(EINVAL);

  TIMEVAL_TO_TIMESPEC(@tvp[0], @tsp[0]);
  TIMEVAL_TO_TIMESPEC(@tvp[1], @tsp[1]);
 end;
 Exit(0);
end;

{
 * Common implementation code for utimes(), lutimes(), and futimes().
 }
function setutimes(vp:p_vnode;ts:ptimespec;numtimes,nilflag:Integer):Integer;
var
 error,setbirthtime:Integer;
 mp:p_mount;
 vattr:t_vattr;
begin
 error:=vn_start_write(vp, @mp, V_WAIT or PCATCH);
 if (error<>0) then
  Exit(error);
 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 setbirthtime:=0;
 if (numtimes < 3) and
    (VOP_GETATTR(vp, @vattr)=0) and
    (timespeccmp_lt(@ts[1], @vattr.va_birthtime)<>0) then //<
  setbirthtime:=1;

 VATTR_NULL(@vattr);
 vattr.va_atime:=ts[0];
 vattr.va_mtime:=ts[1];
 if (setbirthtime<>0) then
  vattr.va_birthtime:=ts[1];
 if (numtimes > 2) then
  vattr.va_birthtime:=ts[2];
 if (nilflag<>0) then
  vattr.va_vaflags:=vattr.va_vaflags or VA_UTIMES_NULL;

 //error:=mac_vnode_check_setutimes(td^.td_ucred, vp, vattr.va_atime, vattr.va_mtime);

 if (error=0) then
  error:=VOP_SETATTR(vp, @vattr);
 VOP_UNLOCK(vp, 0);
 vn_finished_write(mp);
 Exit(error);
end;

function kern_utimesat(fd:Integer;path:PChar;pathseg:uio_seg;
                       tptr:ptimeval;tptrseg:uio_seg):Integer;
var
 nd:t_nameidata;
 ts:array[0..1] of timespec;
 error,vfslocked:Integer;
begin
 error:=getutimes(tptr, tptrseg, ts);
 if (error<>0) then
  Exit(error);
 NDINIT_ATRIGHTS(@nd, LOOKUP, FOLLOW or MPSAFE or AUDITVNODE1, pathseg, path, fd, CAP_FUTIMES, curkthread);

 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 error:=setutimes(nd.ni_vp, ts, 2, ord(tptr=nil));
 vrele(nd.ni_vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function kern_utimes(path:PChar;pathseg:uio_seg;
                     tptr:ptimeval;tptrseg:uio_seg):Integer;
begin
 Exit(kern_utimesat(AT_FDCWD, path, pathseg, tptr, tptrseg));
end;

{
 * Set the access and modification times of a file.
 }
function sys_utimes(path:PChar;tptr:ptimeval):Integer;
begin
 Exit(kern_utimes(path, UIO_USERSPACE, tptr, UIO_USERSPACE));
end;

function sys_futimesat(fd:Integer;path:PChar;times:ptimeval):Integer;
begin
 Exit(kern_utimesat(fd, path, UIO_USERSPACE, times, UIO_USERSPACE));
end;

function kern_lutimes(path:PChar;pathseg:uio_seg;
                      tptr:ptimeval;tptrseg:uio_seg):Integer;
var
 ts:array[0..1] of timespec;
 error:Integer;
 nd:t_nameidata;
 vfslocked:Integer;
begin
 error:=getutimes(tptr, tptrseg, ts);
 if (error<>0) then
  Exit(error);
 NDINIT(@nd, LOOKUP, NOFOLLOW or MPSAFE or AUDITVNODE1, pathseg, path, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 error:=setutimes(nd.ni_vp, ts, 2, ord(tptr=nil));
 vrele(nd.ni_vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

{
 * Set the access and modification times of a file.
 }
function sys_lutimes(path:PChar;tptr:ptimeval):Integer;
begin
 Exit(kern_lutimes(path, UIO_USERSPACE, tptr, UIO_USERSPACE));
end;

function kern_futimes(fd:Integer;tptr:ptimeval;tptrseg:uio_seg):Integer;
var
 ts:array[0..1] of timespec;
 fp:p_file;
 vfslocked:Integer;
 error:Integer;
begin
 error:=getutimes(tptr, tptrseg, ts);
 if (error<>0) then
  Exit(error);

 error:=getvnode(fd, CAP_FUTIMES, @fp);
 if (error<>0) then
  Exit(error);
 vfslocked:=VFS_LOCK_GIANT(fp^.f_vnode^.v_mount);

 error:=setutimes(fp^.f_vnode, ts, 2, ord(tptr=nil));
 VFS_UNLOCK_GIANT(vfslocked);
 fdrop(fp);
 Exit(error);
end;

{
 * Set the access and modification times of a file.
 }
function sys_futimes(fd:Integer;tptr:ptimeval):Integer;
begin
 Exit(kern_futimes(fd, tptr, UIO_USERSPACE));
end;

function kern_truncate(path:PChar;pathseg:uio_seg;length:Int64):Integer;
var
 mp:p_mount;
 vp:p_vnode;
 rl_cookie:Pointer;
 vattr:t_vattr;
 nd:t_nameidata;
 error,vfslocked:Integer;
begin
 if (length < 0) then
  Exit(EINVAL);
 NDINIT(@nd, LOOKUP, FOLLOW or MPSAFE or AUDITVNODE1, pathseg, path, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 vp:=nd.ni_vp;
 rl_cookie:=vn_rangelock_wlock(vp, 0, High(Int64));
 error:=vn_start_write(vp, @mp, V_WAIT or PCATCH);
 if (error<>0) then
 begin
  vn_rangelock_unlock(vp, rl_cookie);
  vrele(vp);
  VFS_UNLOCK_GIANT(vfslocked);
  Exit(error);
 end;
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 if (vp^.v_type=VDIR) then
  error:=EISDIR

 //else if ((error:=mac_vnode_check_write(td^.td_ucred, NOCRED, vp)))
 //begin
 //end

 else
 begin
  error:=vn_writechk(vp);
  if (error=0) then
  begin
   error:=VOP_ACCESS(vp, VWRITE);
   if (error=0) then
   begin
    VATTR_NULL(@vattr);
    vattr.va_size:=length;
    error:=VOP_SETATTR(vp, @vattr);
   end;
  end;
 end;
 VOP_UNLOCK(vp, 0);
 vn_finished_write(mp);
 vn_rangelock_unlock(vp, rl_cookie);
 vrele(vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

{
 * Truncate a file given its path name.
 }
function sys_truncate(path:PChar;length:Int64):Integer;
begin
 Exit(kern_truncate(path, UIO_USERSPACE, length));
end;

{
 * Sync an open file.
 }
function kern_fsync(fd:Integer;fullsync:Boolean):Integer;
label
 drop;
var
 vp:p_vnode;
 mp:p_mount;
 fp:p_file;
 vfslocked:Integer;
 error,lock_flags:Integer;
begin
 error:=getvnode(fd, CAP_FSYNC, @fp);
 if (error<>0) then
  Exit(error);

 vp:=fp^.f_vnode;
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 error:=vn_start_write(vp, @mp, V_WAIT or PCATCH);
 if (error<>0) then
  goto drop;

 if MNT_SHARED_WRITES(mp) or
    ((mp=nil) and MNT_SHARED_WRITES(vp^.v_mount)) then
 begin
  lock_flags:=LK_SHARED;
 end else
 begin
  lock_flags:=LK_EXCLUSIVE;
 end;
 vn_lock(vp, lock_flags or LK_RETRY);

 //if (vp^.v_object<>nil) then
 //begin
 // VM_OBJECT_LOCK(vp^.v_object);
 // vm_object_page_clean(vp^.v_object, 0, 0, 0);
 // VM_OBJECT_UNLOCK(vp^.v_object);
 //end;
 error:=VOP_FSYNC(vp, MNT_WAIT);

 VOP_UNLOCK(vp, 0);
 vn_finished_write(mp);
drop:
 VFS_UNLOCK_GIANT(vfslocked);
 fdrop(fp);
 Exit(error);
end;

function sys_fsync(fd:Integer):Integer;
begin
 Exit(kern_fsync(fd, true));
end;

function sys_fdatasync(fd:Integer):Integer;
begin
 Exit(kern_fsync(fd, false));
end;

function kern_renameat(oldfd:Integer;old:PChar;newfd:Integer;new:PChar;pathseg:uio_seg):Integer;
label
 out1,
 _out;
var
 mp:p_mount;
 tvp,fvp,tdvp:p_vnode;
 fromnd,tond:t_nameidata;
 tvfslocked:Integer;
 fvfslocked:Integer;
 error:Integer;
begin
 mp:=nil;

 //bwillwrite();
 NDINIT_ATRIGHTS(@fromnd, DELETE, LOCKPARENT or LOCKLEAF or SAVESTART or
     MPSAFE or AUDITVNODE1, pathseg, old, oldfd, CAP_DELETE, curkthread);

 error:=nd_namei(@fromnd);
 if (error<>0) then
  Exit(error);
 fvfslocked:=NDHASGIANT(@fromnd);
 tvfslocked:=0;

 //error:=mac_vnode_check_rename_from(td^.td_ucred, fromnd.ni_dvp,
 //    fromnd.ni_vp, @fromnd.ni_cnd);

 VOP_UNLOCK(fromnd.ni_dvp, 0);
 if (fromnd.ni_dvp<>fromnd.ni_vp) then
  VOP_UNLOCK(fromnd.ni_vp, 0);

 fvp:=fromnd.ni_vp;
 if (error=0) then
  error:=vn_start_write(fvp, @mp, V_WAIT or PCATCH);
 if (error<>0) then
 begin
  NDFREE(@fromnd, NDF_ONLY_PNBUF);
  vrele(fromnd.ni_dvp);
  vrele(fvp);
  goto out1;
 end;
 NDINIT_ATRIGHTS(@tond, RENAME, LOCKPARENT or LOCKLEAF or NOCACHE or
     SAVESTART or MPSAFE or AUDITVNODE2, pathseg, new, newfd, CAP_CREATE,
     curkthread);
 if (fromnd.ni_vp^.v_type=VDIR) then
  tond.ni_cnd.cn_flags:=tond.ni_cnd.cn_flags or WILLBEDIR;

 error:=nd_namei(@tond);
 if (error<>0) then
 begin
  { Translate error code for rename('dir1', 'dir2/.'). }
  if (error=EISDIR) and (fvp^.v_type=VDIR) then
   error:=EINVAL;
  NDFREE(@fromnd, NDF_ONLY_PNBUF);
  vrele(fromnd.ni_dvp);
  vrele(fvp);
  vn_finished_write(mp);
  goto out1;
 end;
 tvfslocked:=NDHASGIANT(@tond);
 tdvp:=tond.ni_dvp;
 tvp:=tond.ni_vp;
 if (tvp<>nil) then
 begin
  if (fvp^.v_type=VDIR) and (tvp^.v_type<>VDIR) then
  begin
   error:=ENOTDIR;
   goto _out;
  end else
  if (fvp^.v_type<>VDIR) and (tvp^.v_type=VDIR) then
  begin
   error:=EISDIR;
   goto _out;
  end;
 end;
 if (fvp=tdvp) then
 begin
  error:=EINVAL;
  goto _out;
 end;
 {
  * If the source is the same as the destination (that is, if they
  * are links to the same vnode), then there is nothing to do.
  }
 if (fvp=tvp) then
  error:=-1;

 //else
 // error:=mac_vnode_check_rename_to(td^.td_ucred, tdvp,
 //     tond.ni_vp, fromnd.ni_dvp=tdvp, @tond.ni_cnd);

 _out:
 if (error=0) then
 begin
  error:=VOP_RENAME(fromnd.ni_dvp, fromnd.ni_vp, @fromnd.ni_cnd, tond.ni_dvp, tond.ni_vp, @tond.ni_cnd);
  NDFREE(@fromnd, NDF_ONLY_PNBUF);
  NDFREE(@tond, NDF_ONLY_PNBUF);
 end else
 begin
  NDFREE(@fromnd, NDF_ONLY_PNBUF);
  NDFREE(@tond, NDF_ONLY_PNBUF);
  if (tvp<>nil) then
   vput(tvp);
  if (tdvp=tvp) then
   vrele(tdvp)
  else
   vput(tdvp);
  vrele(fromnd.ni_dvp);
  vrele(fvp);
 end;
 vrele(tond.ni_startdir);
 vn_finished_write(mp);
out1:
 if (fromnd.ni_startdir<>nil) then
  vrele(fromnd.ni_startdir);
 VFS_UNLOCK_GIANT(fvfslocked);
 VFS_UNLOCK_GIANT(tvfslocked);
 if (error=-1) then
  Exit(0);
 Exit(error);
end;

function kern_rename(from,_to:PChar;pathseg:uio_seg):Integer;
begin
 Exit(kern_renameat(AT_FDCWD, from, AT_FDCWD, _to, pathseg));
end;

{
 * Rename files.  Source and destination must either both be directories, or
 * both not be directories.  If target is a directory, it must be empty.
 }
function sys_rename(from,_to:PChar):Integer;
begin
 Exit(kern_rename(from, _to, UIO_USERSPACE));
end;

function sys_renameat(oldfd:Integer;old:PChar;newfd:Integer;new:PChar):Integer;
begin
 Exit(kern_renameat(oldfd, old, newfd, new, UIO_USERSPACE));
end;

function kern_mkdirat(fd:Integer;path:PChar;segflg:uio_seg;mode:Integer):Integer;
label
 restart,
 _out;
var
 mp:p_mount;
 vp:p_vnode;
 vattr:t_vattr;
 error:Integer;
 nd:t_nameidata;
 vfslocked:Integer;
begin
restart:
 //bwillwrite();
 NDINIT_ATRIGHTS(@nd, CREATE, LOCKPARENT or SAVENAME or MPSAFE or
     AUDITVNODE1, segflg, path, fd, CAP_MKDIR, curkthread);
 nd.ni_cnd.cn_flags:=nd.ni_cnd.cn_flags or WILLBEDIR;

 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 vp:=nd.ni_vp;
 if (vp<>nil) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  {
   * XXX namei called with LOCKPARENT but not LOCKLEAF has
   * the strange behaviour of leaving the vnode unlocked
   * if the target is the same vnode as the parent.
   }
  if (vp=nd.ni_dvp) then
   vrele(nd.ni_dvp)
  else
   vput(nd.ni_dvp);
  vrele(vp);
  VFS_UNLOCK_GIANT(vfslocked);
  Exit(EEXIST);
 end;
 if (vn_start_write(nd.ni_dvp, @mp, V_NOWAIT)<>0) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  vput(nd.ni_dvp);
  VFS_UNLOCK_GIANT(vfslocked);
  error:=vn_start_write(nil, @mp, V_XSLEEP or PCATCH);
  if (error<>0) then
   Exit(error);
  goto restart;
 end;
 VATTR_NULL(@vattr);
 vattr.va_type:=VDIR;
 vattr.va_mode:=(mode and ACCESSPERMS) and (not fd_table.fd_cmask);

 //error:=mac_vnode_check_create(td^.td_ucred, nd.ni_dvp, @nd.ni_cnd, @vattr);
 //if (error)
 // goto out;

 error:=VOP_MKDIR(nd.ni_dvp, @nd.ni_vp, @nd.ni_cnd, @vattr);
_out:
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vput(nd.ni_dvp);
 if (error=0) then
  vput(nd.ni_vp);
 vn_finished_write(mp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function kern_mkdir(path:PChar;segflg:uio_seg;mode:Integer):Integer;
begin
 Exit(kern_mkdirat(AT_FDCWD, path, segflg, mode));
end;

{
 * Make a directory file.
 }
function sys_mkdir(path:PChar;mode:Integer):Integer;
begin
 Exit(kern_mkdir(path, UIO_USERSPACE, mode));
end;

function sys_mkdirat(fd:Integer;path:PChar;mode:Integer):Integer;
begin
 Exit(kern_mkdirat(fd, path, UIO_USERSPACE, mode));
end;

function kern_rmdirat(fd:Integer;path:PChar;pathseg:uio_seg):Integer;
label
 restart,
 _out;
var
 mp:p_mount;
 vp:p_vnode;
 error:Integer;
 nd:t_nameidata;
 vfslocked:Integer;
begin
restart:
 //bwillwrite();
 NDINIT_ATRIGHTS(@nd, DELETE, LOCKPARENT or LOCKLEAF or MPSAFE or
     AUDITVNODE1, pathseg, path, fd, CAP_RMDIR, curkthread);
 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 vp:=nd.ni_vp;
 if (vp^.v_type<>VDIR) then
 begin
  error:=ENOTDIR;
  goto _out;
 end;
 {
  * No rmdir '.' please.
  }
 if (nd.ni_dvp=vp) then
 begin
  error:=EINVAL;
  goto _out;
 end;
 {
  * The root of a mounted filesystem cannot be deleted.
  }
 if ((vp^.v_vflag and VV_ROOT)<>0) then
 begin
  error:=EBUSY;
  goto _out;
 end;

 //error:=mac_vnode_check_unlink(td^.td_ucred, nd.ni_dvp, vp, @nd.ni_cnd);
 //if (error<>0) then
 // goto _out;

 if (vn_start_write(nd.ni_dvp, @mp, V_NOWAIT)<>0) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  vput(vp);
  if (nd.ni_dvp=vp) then
   vrele(nd.ni_dvp)
  else
   vput(nd.ni_dvp);
  VFS_UNLOCK_GIANT(vfslocked);
  error:=vn_start_write(nil, @mp, V_XSLEEP or PCATCH);
  if (error<>0) then
   Exit(error);
  goto restart;
 end;
 error:=VOP_RMDIR(nd.ni_dvp, nd.ni_vp, @nd.ni_cnd);
 vn_finished_write(mp);
 _out:
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vput(vp);
 if (nd.ni_dvp=vp) then
  vrele(nd.ni_dvp)
 else
  vput(nd.ni_dvp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function kern_rmdir(path:PChar;pathseg:uio_seg):Integer;
begin
 Exit(kern_rmdirat(AT_FDCWD, path, pathseg));
end;

{
 * Remove a directory file.
 }
function sys_rmdir(path:PChar):Integer;
begin
 Exit(kern_rmdir(path, UIO_USERSPACE));
end;

function kern_getdirentries(fd:Integer;buf:Pointer;count:DWORD;basep:PInt64):Integer;
label
 unionread,
 fail;
var
 td:p_kthread;
 vp:p_vnode;
 fp:p_file;
 auio:t_uio;
 aiov:iovec;
 vfslocked:Integer;
 loff:Int64;
 error,eofflag:Integer;
 foffset:Int64;
 tvp:p_vnode;
begin
 td:=curkthread;
 auio.uio_resid:=count;

 if (auio.uio_resid > IOSIZE_MAX) then
  Exit(EINVAL);

 error:=getvnode(fd, CAP_READ or CAP_SEEK, @fp);
 if (error<>0) then
  Exit(error);

 if ((fp^.f_flag and FREAD)=0) then
 begin
  fdrop(fp);
  Exit(EBADF);
 end;
 vp:=fp^.f_vnode;
 foffset:=foffset_lock(fp, 0);
unionread:
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 if (vp^.v_type<>VDIR) then
 begin
  VFS_UNLOCK_GIANT(vfslocked);
  error:=EINVAL;
  goto fail;
 end;
 aiov.iov_base  :=buf;
 aiov.iov_len   :=count;
 auio.uio_iov   :=@aiov;
 auio.uio_iovcnt:=1;
 auio.uio_rw    :=UIO_READ;
 auio.uio_segflg:=UIO_USERSPACE;
 auio.uio_td    :=td;
 vn_lock(vp, LK_SHARED or LK_RETRY);

 loff:=foffset;
 auio.uio_offset:=foffset;

 //error:=mac_vnode_check_readdir(td^.td_ucred, vp);
 //if (error=0) then
  error:=VOP_READDIR(vp, @auio, @eofflag, nil, nil);

 foffset:=auio.uio_offset;
 if (error<>0) then
 begin
  VOP_UNLOCK(vp, 0);
  VFS_UNLOCK_GIANT(vfslocked);
  goto fail;
 end;
 if (count=auio.uio_resid) and
    ((vp^.v_vflag and VV_ROOT)<>0) and
    ((p_mount(vp^.v_mount)^.mnt_flag and MNT_UNION)<>0) then
 begin
  tvp:=vp;
  vp:=p_mount(vp^.v_mount)^.mnt_vnodecovered;
  VREF(vp);
  fp^.f_vnode:=vp;
  fp^.f_data:=vp;
  foffset:=0;
  vput(tvp);
  VFS_UNLOCK_GIANT(vfslocked);
  goto unionread;
 end;
 VOP_UNLOCK(vp, 0);
 VFS_UNLOCK_GIANT(vfslocked);
 basep^:=loff;
 td^.td_retval[0]:=count - auio.uio_resid;
fail:
 foffset_unlock(fp, foffset, 0);
 fdrop(fp);
 Exit(error);
end;

{
 * Read a block of directory entries in a filesystem independent format.
 }
function sys_getdirentries(fd:Integer;buf:Pointer;count:DWORD;basep:PInt64):Integer;
var
 base:Int64;
 error:Integer;
begin
 error:=kern_getdirentries(fd, buf, count, @base);
 if (error<>0) then
  Exit(error);

 if (basep<>nil) then
  error:=copyout(@base, basep, sizeof(Int64));

 Exit(error);
end;

function sys_getdents(fd:Integer;buf:Pointer;count:DWORD):Integer;
begin
 Exit(sys_getdirentries(fd,buf,count,nil));
end;

{
 * Set the mode mask for creation of filesystem nodes.
 }
function sys_umask(newmask:Integer):Integer;
begin
 FILEDESC_XLOCK(@fd_table);
 curkthread^.td_retval[0]:=fd_table.fd_cmask;
 fd_table.fd_cmask:=newmask and ALLPERMS;
 FILEDESC_XUNLOCK(@fd_table);
 Exit(0);
end;

{
 * Void all references to file by ripping underlying filesystem away from
 * vnode.
 }
function sys_revoke(path:PChar):Integer;
label
 _out;
var
 vp:p_vnode;
 vattr:t_vattr;
 error:Integer;
 nd:t_nameidata;
 vfslocked:Integer;
begin
 NDINIT(@nd, LOOKUP, FOLLOW or LOCKLEAF or MPSAFE or AUDITVNODE1,
     UIO_USERSPACE, path, curkthread);

 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 vfslocked:=NDHASGIANT(@nd);
 vp:=nd.ni_vp;
 NDFREE(@nd, NDF_ONLY_PNBUF);

 if (vp^.v_type<>VCHR) or (vp^.v_rdev=nil) then
 begin
  error:=EINVAL;
  goto _out;
 end;

 //error:=mac_vnode_check_revoke(td^.td_ucred, vp);
 //if (error<>0) then
 // goto _out;

 error:=VOP_GETATTR(vp, @vattr);
 if (error<>0) then
  goto _out;
 //if (td^.td_ucred^.cr_uid<>vattr.va_uid) then
 //begin
 // error:=priv_check(td, PRIV_VFS_ADMIN);
 // if (error<>0) then
 //  goto _out;
 //end;
 if (vcount(vp) > 1) then
  VOP_REVOKE(vp, REVOKEALL);
_out:
 vput(vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

{
 * Convert a user file descriptor to a kernel file entry and check that, if it
 * is a capability, the correct rights are present. A reference on the file
 * entry is held upon Exiting.
 }
function getvnode(fd:Integer;rights:cap_rights_t;fpp:pp_file):Integer;
var
 fp:p_file;
 fp_fromcap:p_file;
 error:Integer;
begin
 error:=0;
 fp:=fget_unlocked(fd);
 if (fp=nil) then
  Exit(EBADF);

 {
  * If the file descriptor is for a capability, test rights and use the
  * file descriptor referenced by the capability.
  }
 error:=cap_funwrap(fp, rights, @fp_fromcap);
 if (error<>0) then
 begin
  fdrop(fp);
  Exit(error);
 end;
 if (fp<>fp_fromcap) then
 begin
  fhold(fp_fromcap);
  fdrop(fp);
  fp:=fp_fromcap;
 end;

 {
  * The file could be not of the vnode type, or it may be not
  * yet fully initialized, in which case the f_vnode pointer
  * may be set, but f_ops is still badfileops.  E.g.,
  * devfs_open() transiently create such situation to
  * facilitate csw d_fdopen().
  *
  * Dupfdopen() handling in kern_openat() installs the
  * half-baked file into the process descriptor table, allowing
  * other thread to dereference it. Guard against the race by
  * checking f_ops.
  }
 if (fp^.f_vnode=nil) or (fp^.f_ops=@badfileops) then
 begin
  fdrop(fp);
  Exit(EINVAL);
 end;
 fpp^:=fp;
 Exit(0);
end;

end.

