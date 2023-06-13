unit fdesc_vfsops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vmount,
 vfs_mount,
 vnode,
 fdescfs;

{
 * /dev/fd Filesystem
 }
function fdesc_cmount(ma,data:Pointer;flags:QWORD):Integer;
function fdesc_mount(mp:p_mount):Integer;
function fdesc_unmount(mp:p_mount;mntflags:Integer):Integer;
function fdesc_root(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
function fdesc_statfs(mp:p_mount;sbp:p_statfs):Integer;

const
 _fdesc_vfsops:vfsops=(
  vfs_mount          :@fdesc_mount;
  vfs_cmount         :@fdesc_cmount;
  vfs_unmount        :@fdesc_unmount;
  vfs_root           :@fdesc_root;
  vfs_quotactl       :nil;
  vfs_statfs         :@fdesc_statfs;
  vfs_sync           :nil;
  vfs_vget           :nil;
  vfs_fhtovp         :nil;
  vfs_checkexp       :nil;
  vfs_init           :@fdesc_init;
  vfs_uninit         :@fdesc_uninit;
  vfs_extattrctl     :nil;
  vfs_sysctl         :nil;
  vfs_susp_clean     :nil;
 );

 //VFS_SET(fdesc_vfsops, fdescfs, VFCF_SYNTHETIC);
 fdescfs_vfsconf:vfsconf=(
  vfc_version :VFS_VERSION;
  vfc_name    :'fdescfs';
  vfc_vfsops  :@_fdesc_vfsops;
  vfc_typenum :-1;
  vfc_refcount:0;
  vfc_flags   :VFCF_SYNTHETIC;
  vfc_opts    :nil;
  vfc_list    :(tqe_next:nil;tqe_prev:nil)
 );

implementation

uses
 errno,
 vfiledesc,
 vfs_subr,
 vnode_if,
 kern_mtx,
 kern_id,
 fdesc_vnops;

function VFSTOFDESC(mp:p_mount):p_fdescmount; inline;
begin
 Result:=mp^.mnt_data;
end;

function VTOFDESC(vp:p_vnode):p_fdescnode; inline;
begin
 Result:=vp^.v_data;
end;

{
 * Compatibility shim for old mount(2) system call.
 }
function fdesc_cmount(ma,data:Pointer;flags:QWORD):Integer;
begin
 Exit(kernel_mount(ma, flags));
end;

{
 * Mount the per-process file descriptors (/dev/fd)
 }
function fdesc_mount(mp:p_mount):Integer;
var
 error:Integer;
 fmp:p_fdescmount;
 rvp:p_vnode;
begin
 error:=0;
 rvp:=nil;
 {
  * Update is a no-op
  }
 if ((mp^.mnt_flag and (MNT_UPDATE or MNT_ROOTFS))<>0) then
  Exit(EOPNOTSUPP);

 fmp:=AllocMem(sizeof(t_fdescmount)); { XXX }

 {
  * We need to initialize a few bits of our local mount point struct to
  * avoid confusion in allocvp.
  }
 mp^.mnt_data:=fmp;
 fmp^.flags:=0;

 error:=fdesc_allocvp(_Froot, -1, FD_ROOT, mp, @rvp);

 if (error<>0) then
 begin
  FreeMem(fmp);
  mp^.mnt_data:=nil;
  Exit(error);
 end;

 rvp^.v_type:=VDIR;
 rvp^.v_vflag:=rvp^.v_vflag or VV_ROOT;
 fmp^.f_root:=rvp;

 VOP_UNLOCK(rvp, 0);
 { XXX -- don't mark as local to work around fts() problems }
 {mp^.mnt_flag:=mp^.mnt_flag or MNT_LOCAL;}
 MNT_ILOCK(mp);
 mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_MPSAFE;
 MNT_IUNLOCK(mp);
 vfs_getnewfsid(mp);

 vfs_mountedfrom(mp, 'fdescfs');
 Exit(0);
end;

function fdesc_unmount(mp:p_mount;mntflags:Integer):Integer;
var
 fmp:p_fdescmount;
 data:Pointer;
 error:Integer;
 flags:Integer;
begin
 flags:=0;

 fmp:=p_fdescmount(mp^.mnt_data);

 if ((mntflags and MNT_FORCE)<>0) then
 begin
  { The hash mutex protects the private mount flags. }
  mtx_lock(fdesc_hashmtx);
  fmp^.flags:=fmp^.flags or FMNT_UNMOUNTF;
  mtx_unlock(fdesc_hashmtx);
  flags:=flags or FORCECLOSE;
 end;

 {
  * Clear out buffer cache.  I don't think we
  * ever get anything cached at this level at the
  * moment, but who knows...
  *
  * There is 1 extra root vnode reference corresponding
  * to f_root.
  }
  error:=vflush(mp, 1, flags);
 if (error<>0) then
  Exit(error);

 {
  * Finally, throw away the fdescmount structure. Hold the hashmtx to
  * protect the fdescmount structure.
  }
 mtx_lock(fdesc_hashmtx);
 data:=mp^.mnt_data;
 mp^.mnt_data:=nil;
 mtx_unlock(fdesc_hashmtx);
 FreeMem(data); { XXX }

 Exit(0);
end;

function fdesc_root(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
var
 vp:p_vnode;
begin
 {
  * Exitlocked reference to root.
  }
 vp:=VFSTOFDESC(mp)^.f_root;
 vget(vp, LK_EXCLUSIVE or LK_RETRY);
 vpp^:=vp;
 Exit(0);
end;

function fdesc_statfs(mp:p_mount;sbp:p_statfs):Integer;
var
 lim:Integer;
 freefd:Integer;
begin
 lim   :=fd_table.fd_nfiles;
 freefd:=fd_table.fd_freefd;

 sbp^.f_flags :=0;
 sbp^.f_bsize :=DEV_BSIZE;
 sbp^.f_iosize:=DEV_BSIZE;
 sbp^.f_blocks:=2;  { 1K to keep df happy }
 sbp^.f_bfree :=0;
 sbp^.f_bavail:=0;
 sbp^.f_files :=lim + 1;  { Allow for '.' }
 sbp^.f_ffree :=freefd;
 Exit(0);
end;



end.

