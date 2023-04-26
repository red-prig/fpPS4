unit vfs_mount;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 vmount,
 vuio,
 vnamei,
 kern_mtx,
 kern_synch,
 kern_thr,
 vfs_vnode,
 vfs_init,
 vfs_lookup,
 vnode_if;

procedure vfs_ref(mp:p_mount); inline;
procedure vfs_rel(mp:p_mount); inline;

implementation

uses
 errno,
 vfs_vnops,
 vfs_subr;

procedure vfs_ref(mp:p_mount); inline;
begin
 MNT_REL(mp);
end;

procedure vfs_rel(mp:p_mount); inline;
begin
 MNT_REL(mp);
end;

procedure mount_init(mp:p_mount);
begin
 mtx_init(mp^.mnt_mtx    ,'struct mount mtx');
 mtx_init(mp^.mnt_explock,'explock');
end;

procedure mount_fini(mp:p_mount);
begin
 mtx_destroy(mp^.mnt_explock);
 mtx_destroy(mp^.mnt_mtx);
end;

function vfs_mount_alloc(vp    :p_vnode;
                         vfsp  :p_vfsconf;
                         fspath:PChar):p_mount;
var
 mp:p_mount;
begin
 mp:=AllocMem(SizeOf(mount));
 mount_init(mp);


 TAILQ_INIT(@mp^.mnt_nvnodelist);
 mp^.mnt_nvnodelistsize:=0;
 TAILQ_INIT(@mp^.mnt_activevnodelist);
 mp^.mnt_activevnodelistsize:=0;
 mp^.mnt_ref:=0;
 vfs_busy(mp, MBF_NOWAIT);
 mp^.mnt_op:=vfsp^.vfc_vfsops;
 mp^.mnt_vfc:=vfsp;
 Inc(vfsp^.vfc_refcount); // XXX Unlocked
 mp^.mnt_stat.f_type:=vfsp^.vfc_typenum;
 Inc(mp^.mnt_gen);

 strlcopy(mp^.mnt_stat.f_fstypename, vfsp^.vfc_name, MFSNAMELEN);

 mp^.mnt_vnodecovered:=vp;

 strlcopy(mp^.mnt_stat.f_mntonname, fspath, MNAMELEN);

 mp^.mnt_iosize_max:=DFLTPHYS;

 //mac_mount_init(mp);
 //mac_mount_create(cred, mp);

 mp^.mnt_hashseed:=$FEEDBABE; //arc4rand

 TAILQ_INIT(@mp^.mnt_uppers);
 Result:=mp;
end;

procedure vfs_mount_destroy(mp:p_mount);
begin
 MNT_ILOCK(mp);
 mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_REFEXPIRE;
 if ((mp^.mnt_kern_flag and MNTK_MWAIT)<>0) then
 begin
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag and (not MNTK_MWAIT);
  wakeup(mp);
 end;
 while (mp^.mnt_ref<>0) do
 begin
  msleep(mp, MNT_MTX(mp), PVFS, 'mntref', 0);
 end;
 Assert(mp^.mnt_ref=0,'invalid refcount in the drain path');
 Assert(mp^.mnt_writeopcount=0,'vfs_mount_destroy: nonzero writeopcount');
 Assert(mp^.mnt_secondary_writes=0,'vfs_mount_destroy: nonzero secondary_writes');
 Dec(mp^.mnt_vfc^.vfc_refcount);

 if (not TAILQ_EMPTY(@mp^.mnt_nvnodelist)) then
 begin
  Assert(false,'unmount: dangling vnode');
 end;
 Assert(TAILQ_EMPTY(@mp^.mnt_uppers),'mnt_uppers');
 Assert(mp^.mnt_nvnodelistsize=0,'vfs_mount_destroy: nonzero nvnodelistsize');
 Assert(mp^.mnt_activevnodelistsize=0,'vfs_mount_destroy: nonzero activevnodelistsize');
 Assert(mp^.mnt_lockref=0,'vfs_mount_destroy: nonzero lock refcount');
 MNT_IUNLOCK(mp);

 //mac_mount_destroy(mp);

 //if (mp^.mnt_opt<>nil) then
 // vfs_freeopts(mp^.mnt_opt);

 mount_fini(mp);
 FreeMem(mp);
end;


{
 * vfs_domount_first(): first file system mount (not update)
 }
function vfs_domount_first(vfsp:p_vfsconf;       { File system type. }
                           fspath:PChar;         { Mount path. }
                           vp:p_vnode;           { Vnode to be covered. }
                           fsflags:QWORD;        { Flags common to all filesystems. }
                           optlist:pp_vfsoptlist { Options local to the filesystem. }
                          ):Integer;
var
 //va:t_vattr;
 mp:p_mount;
 newdp:p_vnode;
 error:Integer;
begin
 mtx_assert(VFS_Giant);
 Assert((fsflags and MNT_UPDATE)=0,'MNT_UPDATE shouldnt be here');

 error:=0;
 //error:=vinvalbuf(vp, V_SAVE, 0, 0);
 //if (error=0) and (vp^.v_type<>VDIR) then
 // error:=ENOTDIR;

 if (error=0) then
 begin
  VI_LOCK(vp);
  if ((vp^.v_iflag and VI_MOUNT)=0) and (vp^.v_mountedhere=nil) then
   vp^.v_iflag:=vp^.v_iflag or VI_MOUNT
  else
   error:=EBUSY;
  VI_UNLOCK(vp);
 end;

 if (error<>0) then
 begin
  vput(vp);
  Exit (error);
 end;
 VOP_UNLOCK(vp, 0);

 { Allocate and initialize the filesystem. }
 mp:=vfs_mount_alloc(vp, vfsp, fspath);
 { XXXMAC: pass to vfs_mount_alloc? }
 mp^.mnt_optnew:=optlist^;
 { Set the mount level flags. }
 mp^.mnt_flag:=(fsflags and (MNT_UPDATEMASK or MNT_ROOTFS or MNT_RDONLY));

 {
  * Mount the filesystem.
  * XXX The final recipients of VFS_MOUNT just overwrite the ndp they
  * get.  No freeing of cn_pnbuf.
  }
 error:=vmount.VFS_MOUNT(mp);
 if (error<>0) then
 begin
  vfs_unbusy(mp);
  vfs_mount_destroy(mp);
  VI_LOCK(vp);
  vp^.v_iflag:=vp^.v_iflag and (not VI_MOUNT);
  VI_UNLOCK(vp);
  vrele(vp);
  Exit (error);
 end;

 //if (mp^.mnt_opt<>nil) then
 // vfs_freeopts(mp^.mnt_opt);

 mp^.mnt_opt:=mp^.mnt_optnew;

 optlist^:=nil;
 VFS_STATFS(mp,@mp^.mnt_stat);

 {
  * Prevent external consumers of mount options from reading mnt_optnew.
  }
 mp^.mnt_optnew:=nil;

 MNT_ILOCK(mp);
 if ((mp^.mnt_flag and MNT_ASYNC)<>0) and
    ((mp^.mnt_kern_flag and MNTK_NOASYNC)=0) then
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_ASYNC
 else
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag and (not MNTK_ASYNC);
 MNT_IUNLOCK(mp);

 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 //cache_purge(vp);
 VI_LOCK(vp);
 vp^.v_iflag:=vp^.v_iflag and (not VI_MOUNT);
 VI_UNLOCK(vp);
 //vp^.v_mountedhere:=mp;
 { Place the new filesystem at the end of the mount list. }
 mtx_lock(mountlist_mtx);
 TAILQ_INSERT_TAIL(@mountlist, mp,@mp^.mnt_list);
 mtx_unlock(mountlist_mtx);
 vfs_event_signal(nil, VQ_MOUNT, 0);
 if (VFS_ROOT(mp,LK_EXCLUSIVE,@newdp)<>0) then
  Assert(false,'mount: lost mount');
 VOP_UNLOCK(newdp, 0);
 VOP_UNLOCK(vp, 0);
 //mountcheckdirs(vp, newdp);
 vrele(newdp);
 //if ((mp^.mnt_flag and MNT_RDONLY)=0)
 // vfs_allocate_syncvnode(mp);
 vfs_unbusy(mp);
 Exit (0);
end;

{
 * vfs_domount_update(): update of mounted file system
 }
{
function vfs_domount_update(
 td:p_kthread;  { Calling thread. }
 vp:p_vnode;  { Mount point vnode. }
 fsflags:QWORD;  { Flags common to all filesystems. }
 optlist:pp_vfsoptlist { Options local to the filesystem. }
 ):Integer;
begin
 struct oexport_args oexport;
 struct export_args export;
 struct mount *mp;
 int error, export_error;
 uint64_t flag;

 mtx_assert(@Giant, MA_OWNED);
 ASSERT_VOP_ELOCKED(vp, __func__);
 KASSERT((fsflags and MNT_UPDATE)<>0, ("MNT_UPDATE should be here"));

 if ((vp^.v_vflag and VV_ROOT)=0) begin
  vput(vp);
  Exit (EINVAL);
 end;
 mp:=vp^.v_mount;
 {
  * We only allow the filesystem to be reloaded if it
  * is currently mounted read-only.
  }
 flag:=mp^.mnt_flag;
 if ((fsflags and MNT_RELOAD)<>0 and (flag and MNT_RDONLY)=0) begin
  vput(vp);
  Exit (EOPNOTSUPP); { Needs translation }
 end;
 {
  * Only privileged root, or (if MNT_USER is set) the user that
  * did the original mount is permitted to update it.
  }
 error:=vfs_suser(mp, td);
 if (error<>0) begin
  vput(vp);
  Exit (error);
 end;
 if (vfs_busy(mp, MBF_NOWAIT)) begin
  vput(vp);
  Exit (EBUSY);
 end;
 VI_LOCK(vp);
 if ((vp^.v_iflag and VI_MOUNT)<>0 or vp^.v_mountedhere<>nil) begin
  VI_UNLOCK(vp);
  vfs_unbusy(mp);
  vput(vp);
  Exit (EBUSY);
 end;
 vp^.v_iflag:= or VI_MOUNT;
 VI_UNLOCK(vp);
 VOP_UNLOCK(vp, 0);

 MNT_ILOCK(mp);
 mp^.mnt_flag:= and ~MNT_UPDATEMASK;
 mp^.mnt_flag:= or fsflags and (MNT_RELOAD or MNT_FORCE or MNT_UPDATE |
     MNT_SNAPSHOT or MNT_ROOTFS or MNT_UPDATEMASK or MNT_RDONLY);
 if ((mp^.mnt_flag and MNT_ASYNC)=0)
  mp^.mnt_kern_flag:= and ~MNTK_ASYNC;
 MNT_IUNLOCK(mp);
 mp^.mnt_optnew:=*optlist;
 vfs_mergeopts(mp^.mnt_optnew, mp^.mnt_opt);

 {
  * Mount the filesystem.
  * XXX The final recipients of VFS_MOUNT just overwrite the ndp they
  * get.  No freeing of cn_pnbuf.
  }
 error:=VFS_MOUNT(mp);

 export_error:=0;
 if (error=0) begin
  { Process the export option. }
  if (vfs_copyopt(mp^.mnt_optnew, "export", &export,
      sizeof(export))=0) begin
   export_error:=vfs_export(mp, &export);
  end; else if (vfs_copyopt(mp^.mnt_optnew, "export", &oexport,
      sizeof(oexport))=0) begin
   export.ex_flags:=oexport.ex_flags;
   export.ex_root:=oexport.ex_root;
   export.ex_anon:=oexport.ex_anon;
   export.ex_addr:=oexport.ex_addr;
   export.ex_addrlen:=oexport.ex_addrlen;
   export.ex_mask:=oexport.ex_mask;
   export.ex_masklen:=oexport.ex_masklen;
   export.ex_indexfile:=oexport.ex_indexfile;
   export.ex_numsecflavors:=0;
   export_error:=vfs_export(mp, &export);
  end;
 end;

 MNT_ILOCK(mp);
 if (error=0) begin
  mp^.mnt_flag:= and ~(MNT_UPDATE or MNT_RELOAD or MNT_FORCE |
      MNT_SNAPSHOT);
 end; else begin
  {
   * If we fail, restore old mount flags. MNT_QUOTA is special,
   * because it is not part of MNT_UPDATEMASK, but it could have
   * changed in the meantime if quotactl(2) was called.
   * All in all we want current value of MNT_QUOTA, not the old
   * one.
   }
  mp^.mnt_flag:=(mp^.mnt_flag and MNT_QUOTA) or (flag and ~MNT_QUOTA);
 end;
 if ((mp^.mnt_flag and MNT_ASYNC)<>0 &&
     (mp^.mnt_kern_flag and MNTK_NOASYNC)=0)
  mp^.mnt_kern_flag:= or MNTK_ASYNC;
 else
  mp^.mnt_kern_flag:= and ~MNTK_ASYNC;
 MNT_IUNLOCK(mp);

 if (error<>0)
  goto end;

 if (mp^.mnt_opt<>nil)
  vfs_freeopts(mp^.mnt_opt);
 mp^.mnt_opt:=mp^.mnt_optnew;
 *optlist:=nil;
 (void)VFS_STATFS(mp, &mp^.mnt_stat);
 {
  * Prevent external consumers of mount options from reading
  * mnt_optnew.
  }
 mp^.mnt_optnew:=nil;

 if ((mp^.mnt_flag and MNT_RDONLY)=0)
  vfs_allocate_syncvnode(mp);
 else
  vfs_deallocate_syncvnode(mp);
end:
 vfs_unbusy(mp);
 VI_LOCK(vp);
 vp^.v_iflag:= and ~VI_MOUNT;
 VI_UNLOCK(vp);
 vrele(vp);
 Exit (error<>0 ? error : export_error);
end;
}

{
 * vfs_domount(): actually attempt a filesystem mount.
 }
function vfs_domount(
 td:p_kthread;  { Calling thread. }
 fstype:PChar;  { Filesystem type. }
 fspath:PChar;   { Mount path. }
 fsflags:QWORD;  { Flags common to all filesystems. }
 optlist:pp_vfsoptlist { Options local to the filesystem. }
 ):Integer;
var
 vfsp:p_vfsconf;
 nd:nameidata;
 vp:p_vnode;
 pathbuf:PChar;
 error:Integer;
begin
 {
  * Be ultra-paranoid about making sure the type and fspath
  * variables will fit in our mp buffers, including the
  * terminating NUL.
  }
 if (strlen(fstype) >= MFSNAMELEN) or (strlen(fspath) >= MNAMELEN) then
  Exit (ENAMETOOLONG);

 { Load KLDs before we lock the covered vnode to avoid reversals. }
 vfsp:=nil;
 if ((fsflags and MNT_UPDATE)=0) then
 begin
  vfsp:=vfs_byname(fstype);

  if (vfsp=nil) then
   Exit(ENODEV);
 end;

 {
  * Get vnode to be covered or mount point's vnode in case of MNT_UPDATE.
  }
 NDINIT(@nd, LOOKUP, FOLLOW or LOCKLEAF or MPSAFE or AUDITVNODE1, UIO_SYSSPACE, fspath, td);

 error:=_namei(@nd);
 if (error<>0) then
  Exit (error);
 if (NDHASGIANT(@nd)=0) then
  mtx_lock(VFS_Giant);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vp:=nd.ni_vp;
 if ((fsflags and MNT_UPDATE)=0) then
 begin
  pathbuf:=AllocMem(MNAMELEN);
  strcopy(pathbuf, fspath);
  //error:=vn_path_to_global_path(td, vp, pathbuf, MNAMELEN);
  { debug.disablefullpath=1 results in ENODEV }
  //if (error=0) or (error=ENODEV) then
  //begin
   error:=vfs_domount_first(vfsp, pathbuf, vp, fsflags, optlist);
  //end;
  FreeMem(pathbuf);
 end{ else
  error:=vfs_domount_update(td, vp, fsflags, optlist)};
 mtx_unlock(VFS_Giant);

 ASSERT_VI_UNLOCKED (vp, {$I %LINE%});
 ASSERT_VOP_UNLOCKED(vp, {$I %LINE%});

 Exit(error);
end;





end.

