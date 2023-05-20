unit ufs;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vnode,
 vmount,
 vdirent,
 vfile,
 time,
 kern_mtx,
 kern_sx;

type
 pp_ufs_dirent=^p_ufs_dirent;
 p_ufs_dirent=^t_ufs_dirent;

 t_ufs_dirent=record
  ufs_inode  :Integer;
  ufs_flags  :Integer;      //UFS_*
  ufs_ref    :Integer;
  ufs_dirent :p_dirent;
  ufs_list   :TAILQ_ENTRY;
  ufs_dlist  :TAILQ_HEAD;   //dir list
  ufs_dir    :p_ufs_dirent; //parent
  ufs_links  :Integer;
  ufs_mode   :mode_t;       //S_IFMT
  ufs_uid    :uid_t;
  ufs_gid    :gid_t;
  ufs_size   :Int64;        // file size in bytes
  ufs_bytes  :Int64;        // bytes of disk space held by file
  ufs_atime  :timespec;     // time of last access
  ufs_mtime  :timespec;     // time of last data modification
  ufs_ctime  :timespec;     // time of last file status change
  ufs_btime  :timespec;     // time of file creation
  ufs_vnode  :p_vnode;
  ufs_symlink:PChar;
  ufs_md_lock:t_sx;
  ufs_md_fp  :Pointer; //host data
 end;

 p_ufs_mount=^t_ufs_mount;
 t_ufs_mount=record
  //ufs_idx       :DWORD;
  ufs_mount     :p_mount;
  ufs_rootdir   :p_ufs_dirent;
  ufs_generation:DWORD;
  ufs_ref       :Integer;
  ufs_lock      :t_sx;
  ufs_vnops     :p_vop_vector;
  ufs_path      :PChar;   //host path
  ufs_md_fp     :Pointer; //host data
 end;

function ufs_init(cf:p_vfsconf):Integer;
function ufs_uinit(cf:p_vfsconf):Integer;

function ufs_mount(mp:p_mount):Integer;
function ufs_root(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
function ufs_unmount(mp:p_mount;mntflags:Integer):Integer;
function ufs_statfs(mp:p_mount;sbp:p_statfs):Integer;

const
 _ufs_vfsops:vfsops=(
  vfs_mount          :@ufs_mount;
  vfs_cmount         :nil;
  vfs_unmount        :@ufs_unmount;
  vfs_root           :@ufs_root;
  vfs_quotactl       :nil;
  vfs_statfs         :@ufs_statfs;
  vfs_sync           :nil;
  vfs_vget           :nil;
  vfs_fhtovp         :nil;
  vfs_checkexp       :nil;
  vfs_init           :@ufs_init;
  vfs_uninit         :@ufs_uinit;
  vfs_extattrctl     :nil;
  vfs_sysctl         :nil;
  vfs_susp_clean     :nil;
 );

var
 //VFS_SET(ufs_vfsops, ufs, 0);
 ufs_vfsconf:vfsconf=(
  vfc_version :VFS_VERSION;
  vfc_name    :'ufs';
  vfc_vfsops  :@_ufs_vfsops;
  vfc_typenum :-1;
  vfc_refcount:0;
  vfc_flags   :0;
  vfc_opts    :nil;
  vfc_list    :(tqe_next:nil;tqe_prev:nil)
 );

function  ufs_alloc_cdp_inode():Integer;
procedure ufs_free_cdp_inode(ino:Integer);

procedure ufs_de_hold(p:p_ufs_dirent);
function  ufs_de_drop(p:p_ufs_dirent):Boolean;

procedure ufs_mp_hold(p:p_ufs_mount);
function  ufs_mp_drop(p:p_ufs_mount):Boolean;

function  ufs_allocv(de:p_ufs_dirent;mp:p_mount;lockmode:Integer;vpp:pp_vnode):Integer;

var
 ufs_interlock:mtx;

const
 UFS_ROOTINO =1;

 UFS_WHITEOUT=$01;
 UFS_DOT     =$02;
 UFS_DOTDOT  =$04;
 UFS_DOOMED  =$08;
 UFS_COVERED =$10;
 //UFS_USER    =$20;

 UFS_DEFAULT_MODE=&0755;

implementation

uses
 errno,
 vfs_mount,
 vfs_subr,
 vfs_vnops,
 vnode_if,
 ufs_vnops,
 md_vnops,
 kern_id;

var
 ufs_inos:t_id_desc_table;

function ufs_alloc_cdp_inode():Integer;
begin
 if id_new(@ufs_inos,nil,@Result) then
 begin
  //
 end else
 begin
  Result:=-1;
 end;
end;

procedure ufs_free_cdp_inode(ino:Integer);
begin
 if (ino>0) then
 begin
  id_del(@ufs_inos,ino,nil);
 end;
end;

function ufs_init(cf:p_vfsconf):Integer;
begin
 Result:=0;
 id_table_init(@ufs_inos,UFS_ROOTINO+1);
 mtx_init(ufs_interlock,'ufs_interlock');
end;

function ufs_uinit(cf:p_vfsconf):Integer;
begin
 Result:=0;
 id_table_fini(@ufs_inos);
 mtx_destroy(ufs_interlock);
end;

function VFSTOUFS(mp:p_mount):p_ufs_mount; inline;
begin
 Result:=mp^.mnt_data;
end;

procedure ufs_de_hold(p:p_ufs_dirent);
begin
 System.InterlockedIncrement(p^.ufs_ref);
end;

function ufs_de_drop(p:p_ufs_dirent):Boolean;
begin
 Result:=False;
 if (System.InterlockedDecrement(p^.ufs_ref)=0) then
 begin
  md_free_dirent(p);
  sx_destroy(@p^.ufs_md_lock);
  FreeMem(p);
  Result:=True;
 end;
end;

procedure ufs_mp_hold(p:p_ufs_mount);
begin
 System.InterlockedIncrement(p^.ufs_ref);
end;

function ufs_mp_drop(p:p_ufs_mount):Boolean;
begin
Result:=False;
 if (System.InterlockedDecrement(p^.ufs_ref)=0) then
 begin
  md_unmount(p);
  sx_destroy(@p^.ufs_lock);
  FreeMem(p^.ufs_path);
  FreeMem(p);
  Result:=True;
 end;
end;

function ufs_allocv_drop_refs(drop_lock:Integer;dmp:p_ufs_mount;de:p_ufs_dirent):Integer;
var
 not_found:Integer;
begin
 not_found:=0;
 if ((de^.ufs_flags and UFS_DOOMED)<>0) then
  not_found:=1;

 ufs_de_drop(de);

 if ufs_mp_drop(dmp) then
 begin
  not_found:=2;
  sx_xunlock(@dmp^.ufs_lock);
 end;

 if (not_found=1) or ((drop_lock<>0) and (not_found<>2)) then
  sx_unlock(@dmp^.ufs_lock);

 Exit(not_found);
end;

procedure ufs_insmntque_dtr(vp:p_vnode;arg:Pointer);
var
 de:p_ufs_dirent;
begin
 de:=p_ufs_dirent(arg);
 mtx_lock(ufs_interlock);
 vp^.v_data:=nil;
 de^.ufs_vnode:=nil;
 mtx_unlock(ufs_interlock);
 vgone(vp);
 vput(vp);
end;

function ufs_allocv(de:p_ufs_dirent;mp:p_mount;lockmode:Integer;vpp:pp_vnode):Integer;
label
 loop;
var
 error:Integer;
 vp:p_vnode;
 dmp:p_ufs_mount;
begin
 dmp:=VFSTOUFS(mp);

 if ((de^.ufs_flags and UFS_DOOMED)<>0) then
 begin
  sx_xunlock(@dmp^.ufs_lock);
  Exit(ENOENT);
 end;

loop:
 ufs_de_hold(de);
 ufs_mp_hold(dmp);

 mtx_lock(ufs_interlock);

 vp:=de^.ufs_vnode;
 if (vp<>nil) then
 begin
  VI_LOCK(vp);
  mtx_unlock(ufs_interlock);

  sx_xunlock(@dmp^.ufs_lock);
  vget(vp, lockmode or LK_INTERLOCK or LK_RETRY);
  sx_xlock(@dmp^.ufs_lock);

  if (ufs_allocv_drop_refs(0, dmp, de)<>0) then
  begin
   vput(vp);
   Exit(ENOENT);
  end else
  if ((vp^.v_iflag and VI_DOOMED)<>0) then
  begin
   mtx_lock(ufs_interlock);
   if (de^.ufs_vnode=vp) then
   begin
    de^.ufs_vnode:=nil;
    vp^.v_data:=nil;
   end;
   mtx_unlock(ufs_interlock);
   vput(vp);
   goto loop;
  end;

  sx_xunlock(@dmp^.ufs_lock);
  vpp^:=vp;
  Exit(0);
 end;
 mtx_unlock(ufs_interlock);

 error:=getnewvnode('ufs', mp, dmp^.ufs_vnops, @vp);
 if (error<>0) then
 begin
  ufs_allocv_drop_refs(1, dmp, de);
  Exit(error);
 end;

 vp^.v_type:=iftovt_tab[de^.ufs_dirent^.d_type];

 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY or LK_NOWITNESS);
 //VN_LOCK_ASHARE(vp);
 mtx_lock(ufs_interlock);
 vp^.v_data:=de;
 de^.ufs_vnode:=vp;
 mtx_unlock(ufs_interlock);

 error:=insmntque1(vp, mp, @ufs_insmntque_dtr, de);
 if (error<>0) then
 begin
  ufs_allocv_drop_refs(1, dmp, de);
  Exit(error);
 end;

 if (ufs_allocv_drop_refs(0, dmp, de)<>0) then
 begin
  vput(vp);
  Exit(ENOENT);
 end;

 sx_xunlock(@dmp^.ufs_lock);
 vpp^:=vp;
 Exit(0);
end;

function ufs_root(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
var
 error:Integer;
 vp:p_vnode;
 dmp:p_ufs_mount;
begin
 dmp:=VFSTOUFS(mp);
 sx_xlock(@dmp^.ufs_lock);

 error:=ufs_allocv(dmp^.ufs_rootdir, mp, LK_EXCLUSIVE, @vp);
 if (error<>0) then
  Exit(error);

 vp^.v_vflag:=vp^.v_vflag or VV_ROOT;
 vpp^:=vp;
 Exit(0);
end;

function ufs_mount(mp:p_mount):Integer;
label
 _err,
 _mount_err;
var
 error:Integer;
 fmp:p_ufs_mount;
 rvp:p_vnode;
 path:PChar;
 plen:Integer;
begin

 //if (ufs_unr=nil) then
 // ufs_unr:=new_unrhdr(0, High(Integer));

 error:=0;

 if ((mp^.mnt_flag and MNT_UPDATE)<>0) then
 begin
  Exit(0);
 end;

 if (mp^.mnt_optnew<>nil) then
 begin
  //if (vfs_filteropt(mp^.mnt_optnew, ufs_opts)<>0) then
  // Exit(EINVAL);
 end;

 if ((mp^.mnt_flag and MNT_ROOTFS)<>0) then
 begin
  path:=nil;
  plen:=0;
 end else
 begin
  if (mp^.mnt_optnew=nil) then Exit(EINVAL);
  if (vfs_getopt(mp^.mnt_optnew, 'from', @path, @plen)<>0) then Exit(EINVAL);
  if (path=nil) then Exit(EINVAL);
  if (plen=2) then
  begin
   if (path[0]='/') or (path[0]='\') then
   begin
    path[0]:=#0;
    plen:=1;
   end;
  end;
 end;

 fmp:=AllocMem(sizeof(t_ufs_mount));

 if (path<>nil) then
 begin
  fmp^.ufs_path:=AllocMem(plen);
  Move(path^,fmp^.ufs_path^,plen);
 end;

 //fmp^.ufs_idx:=alloc_ufs_unr);
 sx_init(@fmp^.ufs_lock, 'ufsmount');
 fmp^.ufs_ref:=1;

 MNT_ILOCK(mp);
 mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_MPSAFE or MNTK_LOOKUP_SHARED or MNTK_EXTENDED_SHARED;
 MNT_IUNLOCK(mp);

 fmp^.ufs_mount:=mp;
 mp^.mnt_data:=fmp;
 vfs_getnewfsid(mp);

 if ((mp^.mnt_flag and MNT_ROOTFS)<>0) then
 begin
  fmp^.ufs_vnops:=@ufs_vnodeops_root;

  fmp^.ufs_rootdir:=ufs_vmkdir(fmp, nil, 0, nil, UFS_ROOTINO);

  sx_xlock(@fmp^.ufs_lock);
  ufs_vmkdir(fmp,'dev',3,fmp^.ufs_rootdir,0);
  sx_xunlock(@fmp^.ufs_lock);
 end else
 begin
  fmp^.ufs_vnops:=@md_vnodeops_host;

  error:=md_mount(fmp);
  if (error<>0) then goto _mount_err;

  fmp^.ufs_rootdir:=md_vmkdir(fmp, nil, 0, nil);
 end;

 if (fmp^.ufs_rootdir=nil) then
 begin
  error:=EINVAL; //??
  goto _err;
 end;

 error:=ufs_root(mp, LK_EXCLUSIVE, @rvp);
 if (error<>0) then
 begin
  _err:
  md_unmount(fmp);
  _mount_err:
  sx_xlock(@fmp^.ufs_lock);
  ufs_purge(fmp,fmp^.ufs_rootdir);
  sx_xunlock(@fmp^.ufs_lock);
  sx_destroy(@fmp^.ufs_lock);
  //free_unr(ufs_unr, fmp^.ufs_idx);
  FreeMem(fmp);
  Exit(error);
 end;

 VOP_UNLOCK(rvp, 0);

 if (path=nil) then
 begin
  path:='ufs';
 end;

 vfs_mountedfrom(mp,path);

 Exit(0);
end;

function ufs_unmount(mp:p_mount;mntflags:Integer):Integer;
var
 error:Integer;
 flags:Integer;
 fmp:p_ufs_mount;
 //idx:DWORD;
begin
 flags:=0;

 fmp:=VFSTOUFS(mp);
 Assert(fmp^.ufs_mount<>nil,'devfs_unmount unmounted devfs_mount');

 error:=vflush(mp, 1, flags);
 if (error<>0) then
  Exit(error);

 sx_xlock(@fmp^.ufs_lock);

 fmp^.ufs_mount:=nil;
 mp^.mnt_data:=nil;

 //idx:=fmp^.ufs_idx;

 sx_xunlock(@fmp^.ufs_lock);

 //free_unr(ufs_unr, idx);

 ufs_mp_drop(fmp);

 Exit(0);
end;

function ufs_statfs(mp:p_mount;sbp:p_statfs):Integer;
begin
 sbp^.f_flags :=0;
 sbp^.f_bsize :=DEV_BSIZE;
 sbp^.f_iosize:=DEV_BSIZE;
 sbp^.f_blocks:=2;
 sbp^.f_bfree :=0;
 sbp^.f_bavail:=0;
 sbp^.f_files :=0;
 sbp^.f_ffree :=0;
 Exit(md_statfs(mp,sbp));
end;

end.

