unit devfs_vfsops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vfs_vnode,
 vmount,
 devfs,
 kern_id;

var
 devfs_unr:p_id_desc_table;

function  devfs_mount(mp:p_mount):Integer;
function  devfs_unmount(mp:p_mount;mntflags:Integer):Integer;
function  devfs_root(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
function  devfs_statfs(mp:p_mount;sbp:p_statfs):Integer;
procedure devfs_unmount_final(fmp:p_devfs_mount);

const
 devfs_opts:array[0..3] of PChar=(
  'from','export','ruleset',nil
 );

 _devfs_vfsops:vfsops=(
  vfs_mount          :@devfs_mount;
  vfs_cmount         :nil;
  vfs_unmount        :@devfs_unmount;
  vfs_root           :@devfs_root;
  vfs_quotactl       :nil;
  vfs_statfs         :@devfs_statfs;
  vfs_sync           :nil;
  vfs_vget           :nil;
  vfs_fhtovp         :nil;
  vfs_checkexp       :nil;
  vfs_init           :nil;
  vfs_uninit         :nil;
  vfs_extattrctl     :nil;
  vfs_sysctl         :nil;
  vfs_susp_clean     :nil;
  vfs_reclaim_lowervp:nil;
  vfs_unlink_lowervp :nil;
 );

var
 //VFS_SET(devfs_vfsops, devfs, VFCF_SYNTHETIC or VFCF_JAIL);
 devfs_vfsconf:vfsconf=(
  vfc_version :VFS_VERSION;
  vfc_name    :'devfs';
  vfc_vfsops  :@_devfs_vfsops;
  vfc_typenum :-1;
  vfc_refcount:0;
  vfc_flags   :VFCF_SYNTHETIC or VFCF_JAIL;
  vfc_opts    :nil;
  vfc_list    :(tqe_next:nil;tqe_prev:nil)
 );

implementation

uses
 errno,
 kern_mtx,
 kern_sx,
 devfs_devs,
 devfs_rule,
 devfs_vnops,
 vfs_mount,
 vfs_subr,
 vnode_if;

function new_unrhdr(min,max:Integer):p_id_desc_table;
begin
 Result:=AllocMem(SizeOf(t_id_desc_table));
 id_table_init(Result,min);
 Result^.max_key:=max;
end;

function alloc_unr(p:p_id_desc_table):Integer;
begin
 if id_new(p,nil,@Result) then
 begin
  //
 end else
 begin
  Result:=-1;
 end;
end;

procedure free_unr(p:p_id_desc_table;i:Integer);
begin
 id_del(p,i,nil);
end;

{
 * Mount the filesystem
 }
function devfs_mount(mp:p_mount):Integer;
var
 error:Integer;
 fmp:p_devfs_mount;
 rvp:p_vnode;
 //struct thread *td:=curthread;
 {injail,}rsnum:Integer;
begin

 if (devfs_unr=nil) then
  devfs_unr:=new_unrhdr(0, High(Integer));

 error:=0;

 if ((mp^.mnt_flag and MNT_ROOTFS)<>0) then
  Exit(EOPNOTSUPP);

 //if (!prison_allow(td^.td_ucred, PR_ALLOW_MOUNT_DEVFS))
 // Exit(EPERM);

 rsnum:=0;
 //injail:=jailed(td^.td_ucred);

 if (mp^.mnt_optnew<>nil) then
 begin
  if (vfs_filteropt(mp^.mnt_optnew, devfs_opts)<>0) then
   Exit(EINVAL);

  if (vfs_flagopt(mp^.mnt_optnew, 'export', nil, 0)<>0) then
   Exit(EOPNOTSUPP);

  if (vfs_getopt(mp^.mnt_optnew, 'ruleset', nil, nil)=0) and
     ((vfs_scanopt(mp^.mnt_optnew, 'ruleset', '%d',[rsnum])<>1) or
      (rsnum < 0) or
      (rsnum > 65535)) then
  begin
   vfs_mount_error(mp, '%s', ['invalid ruleset specification']);
   Exit(EINVAL);
  end;

  //if (injail) and
  //   (rsnum<>0) and
  //   (rsnum<>td^.td_ucred^.cr_prison^.pr_devfs_rsnum) then
  // Exit(EPERM);
 end;

 { jails enforce their ruleset }
 //if (injail<>0) then
 // rsnum:=td^.td_ucred^.cr_prison^.pr_devfs_rsnum;

 if ((mp^.mnt_flag and MNT_UPDATE)<>0) then
 begin
  if (rsnum<>0) then
  begin
   fmp:=mp^.mnt_data;
   if (fmp<>nil) then
   begin
    sx_xlock(@fmp^.dm_lock);
    devfs_ruleset_set(devfs_rsnum(rsnum), fmp);
    devfs_ruleset_apply(fmp);
    sx_xunlock(@fmp^.dm_lock);
   end;
  end;
  Exit(0);
 end;

 fmp:=AllocMem(sizeof(t_devfs_mount));
 fmp^.dm_idx:=alloc_unr(devfs_unr);
 sx_init(@fmp^.dm_lock, 'devfsmount');
 fmp^.dm_holdcnt:=1;

 MNT_ILOCK(mp);
 mp^.mnt_flag:=mp^.mnt_flag or MNT_LOCAL;
 mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_MPSAFE or MNTK_LOOKUP_SHARED or MNTK_EXTENDED_SHARED;

 //MAC
 //mp^.mnt_flag:=mp^.mnt_flag or MNT_MULTILABEL;

 MNT_IUNLOCK(mp);
 fmp^.dm_mount:=mp;
 mp^.mnt_data:=fmp;
 vfs_getnewfsid(mp);

 fmp^.dm_rootdir:=devfs_vmkdir(fmp, nil, 0, nil, DEVFS_ROOTINO);

 error:=devfs_root(mp, LK_EXCLUSIVE, @rvp);
 if (error<>0) then
 begin
  sx_destroy(@fmp^.dm_lock);
  free_unr(devfs_unr, fmp^.dm_idx);
  FreeMem(fmp);
  Exit(error);
 end;

 if (rsnum<>0) then
 begin
  sx_xlock(@fmp^.dm_lock);
  devfs_ruleset_set(devfs_rsnum(rsnum), fmp);
  sx_xunlock(@fmp^.dm_lock);
 end;

 VOP_UNLOCK(rvp, 0);

 vfs_mountedfrom(mp, 'devfs');

 Exit(0);
end;

procedure devfs_unmount_final(fmp:p_devfs_mount);
begin
 sx_destroy(@fmp^.dm_lock);
 FreeMem(fmp);
end;

function devfs_unmount(mp:p_mount;mntflags:Integer):Integer;
var
 error:Integer;
 flags:Integer;
 fmp:p_devfs_mount;
 hold:Integer;
 idx:DWORD;
begin
 flags:=0;

 fmp:=VFSTODEVFS(mp);
 Assert(fmp^.dm_mount<>nil,
  ('devfs_unmount unmounted devfs_mount'));
 { There is 1 extra root vnode reference from devfs_mount(). }
 error:=vflush(mp, 1, flags);
 if (error<>0) then
  Exit(error);
 sx_xlock(@fmp^.dm_lock);
 devfs_cleanup(fmp);
 devfs_rules_cleanup(fmp);
 fmp^.dm_mount:=nil;
 Inc(fmp^.dm_holdcnt);
 hold:=fmp^.dm_holdcnt;
 mp^.mnt_data:=nil;
 idx:=fmp^.dm_idx;
 sx_xunlock(@fmp^.dm_lock);
 free_unr(devfs_unr, idx);
 if (hold=0) then
  devfs_unmount_final(fmp);
 Exit(0);
end;

{ Exitlocked reference to root.  }

function devfs_root(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
var
 error:Integer;
 vp:p_vnode;
 dmp:p_devfs_mount;
begin
 dmp:=VFSTODEVFS(mp);
 sx_xlock(@dmp^.dm_lock);
 error:=devfs_allocv(dmp^.dm_rootdir, mp, LK_EXCLUSIVE, @vp);
 if (error<>0) then
  Exit(error);
 vp^.v_vflag:=vp^.v_vflag or VV_ROOT;
 vpp^:=vp;
 Exit(0);
end;

function devfs_statfs(mp:p_mount;sbp:p_statfs):Integer;
begin
 sbp^.f_flags :=0;
 sbp^.f_bsize :=DEV_BSIZE;
 sbp^.f_iosize:=DEV_BSIZE;
 sbp^.f_blocks:=2;  { 1K to keep df happy }
 sbp^.f_bfree :=0;
 sbp^.f_bavail:=0;
 sbp^.f_files :=0;
 sbp^.f_ffree :=0;
 Exit(0);
end;


end.

