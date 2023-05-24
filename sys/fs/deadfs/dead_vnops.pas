unit dead_vnops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vnode,
 vfs_default,
 vnode_if;

{
 * Prototypes for dead operations on vnodes.
 }
function dead_lookup(ap:p_vop_lookup_args):Integer;
function dead_open(ap:p_vop_open_args):Integer;
function dead_poll(ap:p_vop_poll_args):Integer;
function dead_read(ap:p_vop_read_args):Integer;
function dead_write(ap:p_vop_write_args):Integer;
function dead_getwritemount(ap:p_vop_getwritemount_args):Integer;
function dead_rename(ap:p_vop_rename_args):Integer;

const
 dead_vnodeops:vop_vector=(
  vop_default       :@default_vnodeops;
  vop_bypass        :nil;
  vop_islocked      :nil;
  vop_lookup        :@dead_lookup;
  vop_create        :@VOP_PANIC;
  vop_whiteout      :nil;
  vop_mknod         :@VOP_PANIC;
  vop_open          :@dead_open;
  vop_close         :nil;
  vop_access        :@VOP_EBADF;
  vop_accessx       :nil;
  vop_getattr       :@VOP_EBADF;
  vop_setattr       :@VOP_EBADF;
  vop_markatime     :nil;
  vop_read          :@dead_read;
  vop_write         :@dead_write;
  vop_ioctl         :@VOP_EBADF;
  vop_poll          :@dead_poll;
  vop_kqfilter      :nil;
  vop_revoke        :nil;
  vop_fsync         :nil;
  vop_remove        :@VOP_PANIC;
  vop_link          :@VOP_PANIC;
  vop_rename        :@dead_rename;
  vop_mkdir         :@VOP_PANIC;
  vop_rmdir         :@VOP_PANIC;
  vop_symlink       :@VOP_PANIC;
  vop_readdir       :@VOP_EBADF;
  vop_readlink      :@VOP_EBADF;
  vop_inactive      :@VOP_NULL;
  vop_reclaim       :@VOP_NULL;
  vop_lock1         :nil;
  vop_unlock        :nil;
  vop_bmap          :@VOP_EBADF;
  vop_strategy      :nil;
  vop_getwritemount :@dead_getwritemount;
  vop_print         :nil;
  vop_pathconf      :@VOP_EBADF; { per pathconf(2) }
  vop_advlock       :@VOP_EBADF;
  vop_advlockasync  :nil;
  vop_advlockpurge  :nil;
  vop_reallocblks   :nil;
  vop_getpages      :nil;
  vop_putpages      :nil;
  vop_vptofh        :nil;
  vop_vptocnp       :@VOP_EBADF;
  vop_allocate      :nil;
  vop_unp_bind      :nil;
  vop_unp_connect   :nil;
  vop_unp_detach    :nil;
 );

implementation

uses
 errno,
 vpoll,
 vfs_subr;

function dead_getwritemount(ap:p_vop_getwritemount_args):Integer;
begin
 ap^.a_mpp^:=nil;
 Exit(0);
end;

{
 * Trivial lookup routine that always fails.
 }
function dead_lookup(ap:p_vop_lookup_args):Integer;
begin
 ap^.a_vpp^:=nil;
 Exit(ENOTDIR);
end;

{
 * Open always fails as if device did not exist.
 }
function dead_open(ap:p_vop_open_args):Integer;
begin
 Exit(ENXIO);
end;

{
 * Vnode op for read
 }
function dead_read(ap:p_vop_read_args):Integer;
begin
 {
  * ExitEOF for tty devices, EIO for others
  }
 if ((ap^.a_vp^.v_vflag and VV_ISTTY)=0) then
  Exit(EIO);
 Exit(0);
end;

{
 * Vnode op for write
 }
function dead_write(ap:p_vop_write_args):Integer;
begin
 Exit(EIO);
end;

{
 * Trivial poll routine that always returns POLLHUP.
 * This is necessary so that a process which is polling a file
 * gets notified when that file is revoke()d.
 }
function dead_poll(ap:p_vop_poll_args):Integer;
begin
 Exit(POLLHUP);
end;

function dead_rename(ap:p_vop_rename_args):Integer;
begin
 vop_rename_fail(ap);
 Exit(EXDEV);
end;


end.

