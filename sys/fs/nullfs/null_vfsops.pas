unit null_vfsops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vnode,
 vmount,
 nullfs,
 null_subr;

function  nullfs_mount(mp:p_mount):Integer;
function  nullfs_unmount(mp:p_mount;mntflags:Integer):Integer;
function  nullfs_root(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
function  nullfs_quotactl(mp:p_mount;cmd,uid:Integer;arg:Pointer):Integer;
function  nullfs_statfs(mp:p_mount;sbp:p_statfs):Integer;
function  nullfs_sync(mp:p_mount;waitfor:Integer):Integer;
function  nullfs_vget(mp:p_mount;ino:DWORD;flags:Integer;vpp:pp_vnode):Integer;
function  nullfs_fhtovp(mp:p_mount;fidp:p_fid;flags:Integer;vpp:pp_vnode):Integer;
function  nullfs_extattrctl(mp:p_mount;cmd:Integer;filename_vp:p_vnode;namespace:Integer;attrname:PChar):Integer;

const
 _null_vfsops:vfsops=(
  vfs_mount          :@nullfs_mount;
  vfs_cmount         :nil;
  vfs_unmount        :@nullfs_unmount;
  vfs_root           :@nullfs_root;
  vfs_quotactl       :@nullfs_quotactl;
  vfs_statfs         :@nullfs_statfs;
  vfs_sync           :@nullfs_sync;
  vfs_vget           :@nullfs_vget;
  vfs_fhtovp         :@nullfs_fhtovp;
  vfs_checkexp       :nil;
  vfs_init           :@nullfs_init;
  vfs_uninit         :@nullfs_uninit;
  vfs_extattrctl     :@nullfs_extattrctl;
  vfs_sysctl         :nil;
  vfs_susp_clean     :nil;
 );

 //VFS_SET(null_vfsops, nullfs, VFCF_LOOPBACK or VFCF_JAIL);
 nullfs_vfsconf:vfsconf=(
  vfc_version :VFS_VERSION;
  vfc_name    :'nullfs';
  vfc_vfsops  :@_null_vfsops;
  vfc_typenum :-1;
  vfc_refcount:0;
  vfc_flags   :VFCF_LOOPBACK or VFCF_JAIL;
  vfc_opts    :nil;
  vfc_list    :(tqe_next:nil;tqe_prev:nil)
 );

implementation

uses
 errno,
 vuio,
 vnamei,
 vfs_mount,
 vfs_lookup,
 vfs_vnops,
 vfs_subr,
 vnode_if,
 null_vnops,
 kern_thr,
 kern_mtx,
 kern_synch;

function MOUNTTONULLMOUNT(mp:p_mount):p_null_mount; inline;
begin
 Result:=mp^.mnt_data;
end;

function VTONULL(vp:p_vnode):p_null_node; inline;
begin
 Result:=vp^.v_data;
end;

function NULLTOV(xp:p_null_node):p_vnode; inline;
begin
 Result:=xp^.null_vnode;
end;

{
 * Mount null layer
 }
function nullfs_mount(mp:p_mount):Integer;
var
 error:Integer;
 lowerrootvp,vp,nullm_rootvp:p_vnode;
 xmp:p_null_mount;
 target:PChar;
 isvnunlocked,len:Integer;
 nd:t_nameidata;
 ndp:p_nameidata;
 lvp:p_vnode;
 lmp:p_mount;
begin
 error:=0;
 isvnunlocked:=0;
 ndp:=@nd;

 //if (prison_allow(td^.td_ucred, PR_ALLOW_MOUNT_NULLFS)=0) then
 // Exit(EPERM);

 //if ((mp^.mnt_flag and MNT_ROOTFS)<>0) then
 // Exit(EOPNOTSUPP);

 {
  * Update is a no-op
  }
 if ((mp^.mnt_flag and MNT_UPDATE)<>0) then
 begin
  {
   * Only support update mounts for NFS export.
   }
  if (vfs_flagopt(mp^.mnt_optnew, 'export', nil, 0)<>0) then
   Exit(0)
  else
   Exit(EOPNOTSUPP);
 end;

 {
  * Get argument
  }
 error:=vfs_getopt(mp^.mnt_optnew, {'target'} 'from', @target, @len);
 if (error<>0) or (target[len - 1]<>#0) then
  Exit(EINVAL);

 {
  * Unlock lower node to avoid possible deadlock.
  }
 if (mp^.mnt_vnodecovered^.v_op=@null_vnodeops) and
    (VOP_ISLOCKED(mp^.mnt_vnodecovered)=LK_EXCLUSIVE) then
 begin
  VOP_UNLOCK(mp^.mnt_vnodecovered, 0);
  isvnunlocked:=1;
 end;

 {
  * Find lower node
  }
 NDINIT(ndp, LOOKUP, FOLLOW or LOCKLEAF, UIO_SYSSPACE, target, curkthread);
 error:=nd_namei(ndp);

 {
  * Re-lock vnode.
  * XXXKIB This is deadlock-prone as well.
  }
 if (isvnunlocked<>0) then
  vn_lock(mp^.mnt_vnodecovered, LK_EXCLUSIVE or LK_RETRY);

 if (error<>0) then
  Exit(error);

 NDFREE(ndp, NDF_ONLY_PNBUF);

 {
  * Sanity check on lower vnode
  }
 lowerrootvp:=ndp^.ni_vp;

 {
  * Check multi null mount to avoid `lock against myself' panic.
  }
 if (lowerrootvp=VTONULL(mp^.mnt_vnodecovered)^.null_lowervp) then
 begin
  vput(lowerrootvp);
  Exit(EDEADLK);
 end;

 xmp:=AllocMem(sizeof(t_null_mount));

 if (lowerrootvp=nil) then
 begin
  xmp^.nullm_vfs:=nil;

  vp:=nil;
  error:=0;
 end else
 begin
  {
   * Save reference to underlying FS
   }
  xmp^.nullm_vfs:=lowerrootvp^.v_mount;
 end;

 {
  * Save reference.  Each mount also holds
  * a reference on the root vnode.
  }
 error:=null_nodeget(mp, lowerrootvp, @vp);

 {
  * Make sure the node alias worked
  }
 if (error<>0) then
 begin
  FreeMem(xmp);
  Exit(error);
 end;

 {
  * Keep a held reference to the root vnode.
  * It is vrele'd in nullfs_unmount.
  }
 nullm_rootvp:=vp;
 nullm_rootvp^.v_vflag:=nullm_rootvp^.v_vflag or VV_ROOT;
 xmp^.nullm_rootvp:=nullm_rootvp;

 {
  * Unlock the node (either the lower or the alias)
  }
 VOP_UNLOCK(vp, 0);

 lvp:=NULLVPTOLOWERVP(nullm_rootvp);
 if (lvp<>nil) then
 begin
  lmp:=lvp^.v_mount;
  if ((lmp^.mnt_flag and MNT_LOCAL)<>0) then
  begin
   MNT_ILOCK(mp);
   mp^.mnt_flag:=mp^.mnt_flag or MNT_LOCAL;
   MNT_IUNLOCK(mp);
  end;
 end;

 xmp^.nullm_flags:=xmp^.nullm_flags or NULLM_CACHE;

 if (vfs_getopt(mp^.mnt_optnew, 'nocache', nil, nil)=0) then
  xmp^.nullm_flags:=xmp^.nullm_flags and (not NULLM_CACHE);

 MNT_ILOCK(mp);
 if ((xmp^.nullm_flags and NULLM_CACHE)<>0) then
 begin
  if (lowerrootvp<>nil) then
  begin
   lmp:=lowerrootvp^.v_mount;
   mp^.mnt_kern_flag:=mp^.mnt_kern_flag or (lmp^.mnt_kern_flag and (MNTK_MPSAFE or MNTK_SHARED_WRITES or MNTK_LOOKUP_SHARED or MNTK_EXTENDED_SHARED));
  end;
 end;
 mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_LOOKUP_EXCL_DOTDOT;
 MNT_IUNLOCK(mp);
 mp^.mnt_data:=xmp;
 vfs_getnewfsid(mp);
 if ((xmp^.nullm_flags and NULLM_CACHE)<>0) then
 if (xmp^.nullm_vfs<>nil) then
 begin
  MNT_ILOCK(xmp^.nullm_vfs);
  TAILQ_INSERT_TAIL(@xmp^.nullm_vfs^.mnt_uppers,mp,@mp^.mnt_upper_link);
  MNT_IUNLOCK(xmp^.nullm_vfs);
 end;

 vfs_mountedfrom(mp, target);

 Exit(0);
end;

{
 * Free reference to null layer
 }
function nullfs_unmount(mp:p_mount;mntflags:Integer):Integer;
var
 mntdata:p_null_mount;
 ump:p_mount;
 error,flags:Integer;
begin
 if ((mntflags and MNT_FORCE)<>0) then
  flags:=FORCECLOSE
 else
  flags:=0;

 { There is 1 extra root vnode reference (nullm_rootvp). }
 error:=vflush(mp, 1, flags);
 if (error<>0) then
  Exit(error);

 {
  * Finally, throw away the null_mount structure
  }
 mntdata:=mp^.mnt_data;
 ump:=mntdata^.nullm_vfs;

 if (ump<>nil) then
 if ((mntdata^.nullm_flags and NULLM_CACHE)<>0) then
 begin
  MNT_ILOCK(ump);
  while ((ump^.mnt_kern_flag and MNTK_VGONE_UPPER)<>0) do
  begin
   ump^.mnt_kern_flag:=ump^.mnt_kern_flag or MNTK_VGONE_WAITER;
   msleep(@ump^.mnt_uppers, @ump^.mnt_mtx, 0, 'vgnupw', 0);
  end;
  TAILQ_REMOVE(@ump^.mnt_uppers,mp,@mp^.mnt_upper_link);
  MNT_IUNLOCK(ump);
 end;

 mp^.mnt_data:=nil;

 FreeMem(mntdata);
 Exit(0);
end;

function nullfs_root(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
var
 vp:p_vnode;
 xmp:p_null_mount;
begin
 {
  * Return locked reference to root.
  }
 xmp:=MOUNTTONULLMOUNT(mp);
 if (xmp=nil) then Exit(EOPNOTSUPP);

 vp:=xmp^.nullm_rootvp;
 VREF(vp);

 //ASSERT_VOP_UNLOCKED(vp, 'root vnode is locked');
 vn_lock(vp, flags or LK_RETRY);
 vpp^:=vp;
 Exit(0);
end;

function nullfs_quotactl(mp:p_mount;cmd,uid:Integer;arg:Pointer):Integer;
var
 xmp:p_null_mount;
begin
 xmp:=MOUNTTONULLMOUNT(mp);
 if (xmp=nil) then Exit(EOPNOTSUPP);

 Exit(VFS_QUOTACTL(xmp^.nullm_vfs, cmd, uid, arg));
end;

function nullfs_statfs(mp:p_mount;sbp:p_statfs):Integer;
var
 error:Integer;
 mstat:t_statfs;
 xmp:p_null_mount;
begin
 xmp:=MOUNTTONULLMOUNT(mp);
 if (xmp=nil) then Exit(EOPNOTSUPP);

 if (xmp^.nullm_vfs=nil) then
 begin
  sbp^.f_flags :=0;
  sbp^.f_bsize :=DEV_BSIZE;
  sbp^.f_iosize:=DEV_BSIZE;
  sbp^.f_blocks:=2;
  sbp^.f_bfree :=0;
  sbp^.f_bavail:=0;
  sbp^.f_files :=0;
  sbp^.f_ffree :=0;
  Exit(0);
 end;

 mstat:=Default(t_statfs);
 error:=VFS_STATFS(xmp^.nullm_vfs, @mstat);
 if (error<>0) then
  Exit(error);

 { now copy across the 'interesting' information and fake the rest }
 sbp^.f_type  :=mstat.f_type;
 sbp^.f_flags :=mstat.f_flags;
 sbp^.f_bsize :=mstat.f_bsize;
 sbp^.f_iosize:=mstat.f_iosize;
 sbp^.f_blocks:=mstat.f_blocks;
 sbp^.f_bfree :=mstat.f_bfree;
 sbp^.f_bavail:=mstat.f_bavail;
 sbp^.f_files :=mstat.f_files;
 sbp^.f_ffree :=mstat.f_ffree;

 Exit(0);
end;

function nullfs_sync(mp:p_mount;waitfor:Integer):Integer;
begin
 {
  * XXX - Assumes no data cached at null layer.
  }
 Exit(0);
end;

function nullfs_vget(mp:p_mount;ino:DWORD;flags:Integer;vpp:pp_vnode):Integer;
var
 xmp:p_null_mount;
 error:Integer;
begin
 Assert((flags and LK_TYPE_MASK)<>0,'nullfs_vget: no lock requested');

 xmp:=MOUNTTONULLMOUNT(mp);
 if (xmp=nil) then Exit(EOPNOTSUPP);

 if (xmp^.nullm_vfs=nil) then
 begin
  Exit(ENOENT);
 end;

 error:=VFS_VGET(xmp^.nullm_vfs, ino, flags, vpp);
 if (error<>0) then
  Exit(error);

 Exit(null_nodeget(mp, vpp^, vpp));
end;

function nullfs_fhtovp(mp:p_mount;fidp:p_fid;flags:Integer;vpp:pp_vnode):Integer;
var
 xmp:p_null_mount;
 error:Integer;
begin
 xmp:=MOUNTTONULLMOUNT(mp);
 if (xmp=nil) then Exit(EOPNOTSUPP);

 if (xmp^.nullm_vfs=nil) then
 begin
  Exit(ENOENT);
 end;

 error:=VFS_FHTOVP(xmp^.nullm_vfs, fidp, flags, vpp);
 if (error<>0) then
  Exit(error);
 Exit(null_nodeget(mp, vpp^, vpp));
end;

function nullfs_extattrctl(mp:p_mount;cmd:Integer;filename_vp:p_vnode;namespace:Integer;attrname:PChar):Integer;
var
 xmp:p_null_mount;
begin
 xmp:=MOUNTTONULLMOUNT(mp);
 if (xmp=nil) then Exit(EOPNOTSUPP);

 if (xmp^.nullm_vfs=nil) then
 begin
  Exit(ENOENT);
 end;

 Exit(VFS_EXTATTRCTL(xmp^.nullm_vfs, cmd, filename_vp, namespace, attrname));
end;

end.

