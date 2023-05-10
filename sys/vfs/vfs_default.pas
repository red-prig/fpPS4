unit vfs_default;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 vnode,
 vnode_if,
 vdirent,
 vuio,
 vfile,
 vmount,
 vfs_mount,
 vnamei,
 vfcntl,
 vpoll,
 kern_thr,
 kern_mtx;

function vop_eopnotsupp(ap:Pointer):Integer;
function vop_ebadf(ap:Pointer):Integer;
function vop_enotty(ap:Pointer):Integer;
function vop_einval(ap:Pointer):Integer;
function vop_enoent(ap:Pointer):Integer;
function VOP_NULL(ap:Pointer):Integer;
function vop_panic(ap:Pointer):Integer;
function vop_nolookup(ap:p_vop_lookup_args):Integer;
function vop_norename(ap:p_vop_rename_args):Integer;
function vop_nostrategy(ap:p_vop_strategy_args):Integer;

function get_next_dirent(vp:p_vnode;
                         dpp:PPointer;
                         dirbuf:PByte;
                         dirbuflen:Integer;
                         off:PQWORD;
                         cpos:PPByte;
                         len:PInteger;
                         eofflag:PInteger):Integer;

function dirent_exists(vp:p_vnode;dirname:PChar):Integer;
function vop_stdaccess(ap:p_vop_access_args):Integer;
function vop_stdaccessx(ap:p_vop_accessx_args):Integer;
function vop_stdadvlock(ap:p_vop_advlock_args):Integer;
function vop_stdadvlockasync(ap:p_vop_advlockasync_args):Integer;
function vop_stdadvlockpurge(ap:p_vop_advlockpurge_args):Integer;
function vop_stdpathconf(ap:p_vop_pathconf_args):Integer;
function vop_stdlock(ap:p_vop_lock1_args):Integer;
function vop_stdunlock(ap:p_vop_unlock_args):Integer;
function vop_stdislocked(ap:p_vop_islocked_args):Integer;
function vop_nopoll(ap:p_vop_poll_args):Integer;
function vop_stdpoll(ap:p_vop_poll_args):Integer;
function vop_stdgetwritemount(ap:p_vop_getwritemount_args):Integer;
function vop_stdbmap(ap:p_vop_bmap_args):Integer;
function vop_stdfsync(ap:p_vop_fsync_args):Integer;
function vop_stdgetpages(ap:p_vop_getpages_args):Integer;
function vop_stdkqfilter(ap:p_vop_kqfilter_args):Integer;
function vop_stdputpages(ap:p_vop_putpages_args):Integer;
function vop_stdvptofh(ap:p_vop_vptofh_args):Integer;
function vop_stdvptocnp(ap:p_vop_vptocnp_args):Integer;
function vop_stdallocate(ap:p_vop_allocate_args):Integer;
function vop_stdadvise(ap:p_vop_advise_args):Integer;
function vop_stdunp_bind(ap:p_vop_unp_bind_args):Integer;
function vop_stdunp_connect(ap:p_vop_unp_connect_args):Integer;
function vop_stdunp_detach(ap:p_vop_unp_detach_args):Integer;
function vop_stdis_text(ap:p_vop_is_text_args):Integer;
function vop_stdset_text(ap:p_vop_set_text_args):Integer;
function vop_stdunset_text(ap:p_vop_unset_text_args):Integer;
function vop_stdget_writecount(ap:p_vop_get_writecount_args):Integer;
function vop_stdadd_writecount(ap:p_vop_add_writecount_args):Integer;

function vfs_stdroot(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
function vfs_stdstatfs(mp:p_mount;sbp:p_statfs):Integer;
function vfs_stdquotactl(mp:p_mount;cmds:Integer;uid:uid_t;arg:Pointer):Integer;
function vfs_stdsync(mp:p_mount;waitfor:Integer):Integer;
function vfs_stdnosync(mp:p_mount;waitfor:Integer):Integer;
function vfs_stdvget(mp:p_mount;ino:DWORD;flags:Integer;vpp:pp_vnode):Integer;
function vfs_stdfhtovp(mp:p_mount;fhp:p_fid;flags:Integer;vpp:pp_vnode) :Integer;
function vfs_stdinit(vfsp:p_vfsconf):Integer;
function vfs_stduninit(vfsp:p_vfsconf):Integer;
function vfs_stdextattrctl(mp:p_mount;cmd:Integer;filename_vp:p_vnode;attrnamespace:Integer;attrname:PChar):Integer;
function vfs_stdsysctl(mp:p_mount;op:Integer;req:Pointer):Integer;

const
 DIRENT_MINSIZE=SizeOf(t_dirent)-(t_dirent.MAXNAMLEN+1)+4;

{
 * This vnode table stores what we want to do if the filesystem doesn't
 * implement a particular VOP.
 *
 * If there is no specific entry here, we will return EOPNOTSUPP.
 *
 * Note that every filesystem has to implement either vop_access
 * or vop_accessx; failing to do so will result in immediate crash
 * due to stack overflow, as vop_stdaccess() calls vop_stdaccessx(),
 * which calls vop_stdaccess() etc.
 }

const
 default_vnodeops:vop_vector=(
  vop_default       :nil                   ;
  vop_bypass        :@VOP_EOPNOTSUPP       ;

  vop_islocked      :@vop_stdislocked      ;
  vop_lookup        :@vop_nolookup         ;
  vop_cachedlookup  :nil                   ;
  vop_create        :nil                   ;
  vop_whiteout      :nil                   ;
  vop_mknod         :nil                   ;
  vop_open          :@VOP_NULL             ;
  vop_close         :@VOP_NULL             ;
  vop_access        :@vop_stdaccess        ;
  vop_accessx       :@vop_stdaccessx       ;
  vop_getattr       :nil                   ;
  vop_setattr       :nil                   ;
  vop_markatime     :nil                   ;
  vop_read          :nil                   ;
  vop_write         :nil                   ;
  vop_ioctl         :@VOP_ENOTTY           ;
  vop_poll          :@vop_nopoll           ;
  vop_kqfilter      :@vop_stdkqfilter      ;
  vop_revoke        :@VOP_PANIC            ;
  vop_fsync         :@VOP_NULL             ;
  vop_remove        :nil                   ;
  vop_link          :nil                   ;
  vop_rename        :@vop_norename         ;
  vop_mkdir         :nil                   ;
  vop_rmdir         :nil                   ;
  vop_symlink       :nil                   ;
  vop_readdir       :nil                   ;
  vop_readlink      :@VOP_EINVAL           ;
  vop_inactive      :@VOP_NULL             ;
  vop_reclaim       :nil                   ;
  vop_lock1         :@vop_stdlock          ;
  vop_unlock        :@vop_stdunlock        ;
  vop_bmap          :@vop_stdbmap          ;
  vop_strategy      :@vop_nostrategy       ;
  vop_getwritemount :@vop_stdgetwritemount ;
  vop_print         :nil                   ;
  vop_pathconf      :@VOP_EINVAL           ;
  vop_advlock       :@vop_stdadvlock       ;
  vop_advlockasync  :@vop_stdadvlockasync  ;
  vop_advlockpurge  :@vop_stdadvlockpurge  ;
  vop_reallocblks   :nil                   ;
  vop_getpages      :@vop_stdgetpages      ;
  vop_putpages      :@vop_stdputpages      ;
  vop_getacl        :nil                   ;
  vop_setacl        :nil                   ;
  vop_aclcheck      :nil                   ;
  vop_closeextattr  :nil                   ;
  vop_getextattr    :nil                   ;
  vop_listextattr   :nil                   ;
  vop_openextattr   :nil                   ;
  vop_deleteextattr :nil                   ;
  vop_setextattr    :nil                   ;
  vop_setlabel      :nil                   ;
  vop_vptofh        :@vop_stdvptofh        ;
  vop_vptocnp       :@vop_stdvptocnp       ;
  vop_allocate      :@vop_stdallocate      ;
  vop_advise        :@vop_stdadvise        ;
  vop_unp_bind      :@vop_stdunp_bind      ;
  vop_unp_connect   :@vop_stdunp_connect   ;
  vop_unp_detach    :@vop_stdunp_detach    ;
  vop_is_text       :@vop_stdis_text       ;
  vop_set_text      :@vop_stdset_text      ;
  vop_unset_text    :@vop_stdunset_text    ;
  vop_get_writecount:@vop_stdget_writecount;
  vop_add_writecount:@vop_stdadd_writecount
);

implementation

uses
 errno,
 vfs_subr,
 vfs_vnops,
 vfs_lookup,
 vsys_generic;

{
 * Series of placeholder functions for various error returns for
 * VOPs.
 }

function vop_eopnotsupp(ap:Pointer):Integer;
begin
 Exit(EOPNOTSUPP);
end;

function vop_ebadf(ap:Pointer):Integer;
begin
 Exit(EBADF);
end;

function vop_enotty(ap:Pointer):Integer;
begin
 Exit(ENOTTY);
end;

function vop_einval(ap:Pointer):Integer;
begin
 Exit(EINVAL);
end;

function vop_enoent(ap:Pointer):Integer;
begin
 Exit(ENOENT);
end;

function VOP_NULL(ap:Pointer):Integer;
begin
 Exit(0);
end;

{
 * Helper function to panic on some bad VOPs in some filesystems.
 }
function vop_panic(ap:Pointer):Integer;
begin
 Assert(false,'filesystem goof: vop_panic[%s]');
 Exit(ENOENT);
end;

{
 * vop_std<something> and vop_no<something> are default functions for use by
 * filesystems that need the 'default reasonable' implementation for a
 * particular operation.
 *
 * The documentation for the operations they implement exists (if it exists)
 * in the VOP_<SOMETHING>(9) manpage (all uppercase).
 }

{
 * Default vop for filesystems that do not support name lookup
 }
function vop_nolookup(ap:p_vop_lookup_args):Integer;
begin
 ap^.a_vpp^:=nil;
 Exit(ENOTDIR);
end;

{
 * vop_norename:
 *
 * Handle unlock and reference counting for arguments of vop_rename
 * for filesystems that do not implement rename operation.
 }
function vop_norename(ap:p_vop_rename_args):Integer;
begin
 vop_rename_fail(ap);
 Exit(EOPNOTSUPP);
end;

{
 * vop_nostrategy:
 *
 * Strategy routine for VFS devices that have none.
 *
 * BIO_ERROR and B_INVAL must be cleared prior to calling any strategy
 * routine.  Typically this is done for a BIO_READ strategy call.
 * Typically B_INVAL is assumed to already be clear prior to a write
 * and should not be cleared manually unless you just made the buffer
 * invalid.  BIO_ERROR should be cleared either way.
 }

function vop_nostrategy(ap:p_vop_strategy_args):Integer;
begin
 Writeln('No strategy for buffer at %p');
 //ap^.a_bp^.b_ioflags:=ap^.a_bp^.b_ioflags or BIO_ERROR;
 //ap^.a_bp^.b_error:=EOPNOTSUPP;
 //bufdone(ap^.a_bp);
 Exit(EOPNOTSUPP);
end;

function get_next_dirent(vp:p_vnode;
                         dpp:PPointer;
                         dirbuf:PByte;
                         dirbuflen:Integer;
                         off:PQWORD;
                         cpos:PPByte;
                         len:PInteger;
                         eofflag:PInteger):Integer;
var
 error, reclen:Integer;
 uio:t_uio;
 iov:iovec;
 dp:p_dirent;
begin
 Assert(VOP_ISLOCKED(vp)<>0,'vp %p is not locked');
 Assert(vp^.v_type=VDIR,'vp %p is not a directory');

 if (len^=0) then
 begin
  iov.iov_base:=dirbuf;
  iov.iov_len:=dirbuflen;

  uio.uio_iov:=@iov;
  uio.uio_iovcnt:=1;
  uio.uio_offset:=off^;
  uio.uio_resid:=dirbuflen;
  uio.uio_segflg:=UIO_SYSSPACE;
  uio.uio_rw:=UIO_READ;
  uio.uio_td:=curkthread;

  eofflag^:=0;


  //error:=mac_vnode_check_readdir(td^.td_ucred, vp);
  //if (error=0) then

  error:=VOP_READDIR(vp, @uio, eofflag, nil, nil);
  if (error<>0) then Exit(error);

  off^:=uio.uio_offset;

  cpos^:=dirbuf;
  len^:=(dirbuflen - uio.uio_resid);

  if (len^=0) then
   Exit(ENOENT);
 end;

 dp:=p_dirent(cpos^);
 reclen:=dp^.d_reclen;
 dpp^:=dp;

 { check for malformed directory.. }
 if (reclen < DIRENT_MINSIZE) then
  Exit(EINVAL);

 cpos^:=cpos^+reclen;
 len^:=len^-reclen;

 Exit(0);
end;

{
 * Check if a named file exists in a given directory vnode.
 }
function dirent_exists(vp:p_vnode;dirname:PChar):Integer;
label
 _out;
var
 dirbuf,cpos:PByte;
 error, eofflag, dirbuflen, len, found:Integer;
 off:QWORD;
 dp:p_dirent;
 va:t_vattr;
begin
 Assert(VOP_ISLOCKED(vp)<>0, 'vp %p is not locked');
 Assert(vp^.v_type=VDIR, 'vp %p is not a directory');

 found:=0;

 error:=VOP_GETATTR(vp, @va);
 if (error<>0) then
  Exit(found);

 dirbuflen:=DEV_BSIZE;
 if (dirbuflen < va.va_blocksize) then
  dirbuflen:=va.va_blocksize;
 dirbuf:=AllocMem(dirbuflen);

 off:=0;
 len:=0;
 repeat
  error:=get_next_dirent(vp, @dp, dirbuf, dirbuflen, @off, @cpos, @len, @eofflag);
  if (error<>0) then
   goto _out;

  if (dp^.d_type<>DT_WHT) and
     (StrComp(PChar(@dp^.d_name),PChar(dirname))=0) then
  begin
   found:=1;
   goto _out;
  end;
 until not ((len>0) or (eofflag=0));

_out:
 FreeMem(dirbuf);
 Exit(found);
end;

function vop_stdaccess(ap:p_vop_access_args):Integer;
begin
 Assert((ap^.a_accmode and (not (VEXEC or VWRITE or VREAD or VADMIN or VAPPEND)))=0, ('invalid bit in accmode'));

 Exit(VOP_ACCESSX(ap^.a_vp, ap^.a_accmode));
end;

function vop_stdaccessx(ap:p_vop_accessx_args):Integer;
var
 error:Integer;
 accmode:accmode_t;
begin
 accmode:=ap^.a_accmode;

 error:=vfs_unixify_accmode(@accmode);
 if (error<>0) then
  Exit(error);

 if (accmode=0) then
  Exit(0);

 Exit(VOP_ACCESS(ap^.a_vp, accmode));
end;

{
 * Advisory record locking support
 }
function vop_stdadvlock(ap:p_vop_advlock_args):Integer;
var
 vp:p_vnode;
 vattr:t_vattr;
 error:Integer;
begin
 vp:=ap^.a_vp;
 if (ap^.a_fl^.l_whence=SEEK_END) then
 begin
  {
   * The NFSv4 server must avoid doing a vn_lock() here, since it
   * can deadlock the nfsd threads, due to a LOR.  Fortunately
   * the NFSv4 server always uses SEEK_SET and this code is
   * only required for the SEEK_END case.
   }
  vn_lock(vp, LK_SHARED or LK_RETRY);
  error:=VOP_GETATTR(vp, @vattr);
  VOP_UNLOCK(vp, 0);
  if (error<>0) then
   Exit(error);
 end else
  vattr.va_size:=0;

 Exit(EOPNOTSUPP);
 //Exit(lf_advlock(ap, @(vp^.v_lockf), vattr.va_size));
end;

function vop_stdadvlockasync(ap:p_vop_advlockasync_args):Integer;
var
 vp:p_vnode;
 vattr:t_vattr;
 error:Integer;
begin
 vp:=ap^.a_vp;
 if (ap^.a_fl^.l_whence=SEEK_END) then
 begin
  { The size argument is only needed for SEEK_END. }
  vn_lock(vp, LK_SHARED or LK_RETRY);
  error:=VOP_GETATTR(vp, @vattr);
  VOP_UNLOCK(vp, 0);
  if (error<>0) then
   Exit(error);
 end else
  vattr.va_size:=0;

 Exit(EOPNOTSUPP);
 //Exit(lf_advlockasync(ap, @(vp^.v_lockf), vattr.va_size));
end;

function vop_stdadvlockpurge(ap:p_vop_advlockpurge_args):Integer;
var
 vp:p_vnode;
begin
 vp:=ap^.a_vp;

 Exit(EOPNOTSUPP);
 //lf_purgelocks(vp, @vp^.v_lockf);
 //Exit(0);
end;

{
 * vop_stdpathconf:
 *
 * Standard implementation of POSIX pathconf, to get information about limits
 * for a filesystem.
 * Override per filesystem for the case where the filesystem has smaller
 * limits.
 }
function vop_stdpathconf(ap:p_vop_pathconf_args):Integer;
begin
 case (ap^.a_name) of
  _PC_NAME_MAX:
   begin
    ap^.a_retval^:=NAME_MAX;
    Exit(0);
   end;
  _PC_PATH_MAX:
   begin
    ap^.a_retval^:=PATH_MAX;
    Exit(0);
   end;
  _PC_LINK_MAX:
   begin
    ap^.a_retval^:=LINK_MAX;
    Exit(0);
   end;
  _PC_MAX_CANON:
   begin
    ap^.a_retval^:=MAX_CANON;
    Exit(0);
   end;
  _PC_MAX_INPUT:
   begin
    ap^.a_retval^:=MAX_INPUT;
    Exit(0);
   end;
  _PC_PIPE_BUF:
   begin
    ap^.a_retval^:=PIPE_BUF;
    Exit(0);
   end;
  _PC_CHOWN_RESTRICTED:
   begin
    ap^.a_retval^:=1;
    Exit(0);
   end;
  _PC_VDISABLE:
   begin
    ap^.a_retval^:=_POSIX_VDISABLE;
    Exit(0);
   end;
  else
   Exit(EINVAL);
 end;
 { NOTREACHED }
end;

function lockmgr(lk:p_mtx;flags:Integer;ilk:p_mtx):Integer;
var
 op:Integer;
begin

 op:=(flags and LK_TYPE_MASK);
 case op of
  LK_SHARED,
  LK_EXCLUSIVE:
   begin
    mtx_lock(lk^);
   end;
  LK_RELEASE:
   begin
    mtx_unlock(lk^);
   end;
  LK_UPGRADE:
   begin
    if not mtx_owned(lk^) then
    begin
     mtx_lock(lk^);
    end;
   end;
   LK_DOWNGRADE:;//
  else
   Assert(false);
 end;

 if ((flags and LK_INTERLOCK)<>0) then
 begin
  mtx_unlock(ilk^);
 end;

 Result:=0;
end;

{
 * Standard lock, unlock and islocked functions.
 }
function vop_stdlock(ap:p_vop_lock1_args):Integer;
var
 vp:p_vnode;
begin
 vp:=ap^.a_vp;

 //Writeln('vop_std  lock:',HexStr(ap^.a_vp^.v_vnlock));

 Result:=lockmgr(vp^.v_vnlock,ap^.a_flags,VI_MTX(vp));

 //Exit(_lockmgr_args(vp^.v_vnlock, ap^.a_flags, VI_MTX(vp),
 //    LK_WMESG_DEFAULT, LK_PRIO_DEFAULT, LK_TIMO_DEFAULT, ap^.a_file,
 //    ap^.a_line));
end;

{ See above. }
function vop_stdunlock(ap:p_vop_unlock_args):Integer;
var
 vp:p_vnode;
begin
 vp:=ap^.a_vp;

 //Writeln('vop_stdunlock:',HexStr(ap^.a_vp^.v_vnlock));

 Result:=lockmgr(vp^.v_vnlock,ap^.a_flags or LK_RELEASE,VI_MTX(vp));

 //Exit(lockmgr(vp^.v_vnlock, ap^.a_flags or LK_RELEASE, VI_MTX(vp)));
end;

{ See above. }
function vop_stdislocked(ap:p_vop_islocked_args):Integer;
begin

 //Writeln('vop_stdislocked:',HexStr(ap^.a_vp^.v_vnlock),':',mtx_owned(ap^.a_vp^.v_vnlock^));

 if mtx_owned(ap^.a_vp^.v_vnlock^) then
  Exit(LK_EXCLUSIVE)
 else
  Exit(0);

 //Exit(lockstatus(ap^.a_vp^.v_vnlock));
end;

{
 * Exittrue for select/poll.
 }
function vop_nopoll(ap:p_vop_poll_args):Integer;
begin
 Exit(poll_no_poll(ap^.a_events));
end;

{
 * Implement poll for local filesystems that support it.
 }
function vop_stdpoll(ap:p_vop_poll_args):Integer;
begin
 if ((ap^.a_events and (not POLLSTANDARD))<>0) then
  Exit(vn_pollrecord(ap^.a_vp, ap^.a_events));
 Exit(ap^.a_events and (POLLIN or POLLOUT or POLLRDNORM or POLLWRNORM));
end;

{
 * Exitour mount point, as we will take charge of the writes.
 }
function vop_stdgetwritemount(ap:p_vop_getwritemount_args):Integer;
var
 mp:p_mount;
begin
 {
  * XXX Since this is called unlocked we may be recycled while
  * attempting to ref the mount.  If this is the case or mountpoint
  * will be set to nil.  We only have to prevent this call from
  * returning with a ref to an incorrect mountpoint.  It is not
  * harmful to Exitwith a ref to our previous mountpoint.
  }
 mp:=ap^.a_vp^.v_mount;
 if (mp<>nil) then
 begin
  vfs_ref(mp);
  if (mp<>ap^.a_vp^.v_mount) then
  begin
   vfs_rel(mp);
   mp:=nil;
  end;
 end;
 (ap^.a_mpp)^:=mp;
 Exit(0);
end;

{ XXX Needs good comment and VOP_BMAP(9) manpage }
function vop_stdbmap(ap:p_vop_bmap_args):Integer;
begin
 //if (ap^.a_bop<>nil) then
 // ap^.a_bop^:=@ap^.a_vp^.v_bufobj;
 //if (ap^.a_bnp<>nil) then
 // ap^.a_bnp^:=ap^.a_bn * btodb(ap^.a_vp^.v_mount^.mnt_stat.f_iosize);
 //if (ap^.a_runp<>nil) then
 // ap^.a_runp^:=0;
 //if (ap^.a_runb<>nil) then
 // ap^.a_runb^:=0;
 Exit(0);
end;

function vop_stdfsync(ap:p_vop_fsync_args):Integer;
var
 vp:p_vnode;
 //struct buf *bp;
 //struct bufobj *bo;
 //struct buf *nbp;
 error:Integer;
 maxretry:Integer;
begin
 vp:=ap^.a_vp;
 error:=0;
 maxretry:=1000;     { large, arbitrarily chosen }

{
 bo:=@vp^.v_bufobj;
 BO_LOCK(bo);

loop1:
 {
  * MARK/SCAN initialization to avoid infinite loops.
  }
 TAILQ_FOREACH(bp, @bo^.bo_dirty.bv_hd, b_bobufs)
 begin
  bp^.b_vflags:=bp^.b_vflags and (not BV_SCANNED);
  bp^.b_error:=0;
 end;

 {
  * Flush all dirty buffers associated with a vnode.
  }
loop2:
 TAILQ_FOREACH_SAFE(bp, @bo^.bo_dirty.bv_hd, b_bobufs, nbp)
 begin
  if ((bp^.b_vflags and BV_SCANNED)<>0)
   continue;
  bp^.b_vflags:=bp^.b_vflags or BV_SCANNED;
  if (BUF_LOCK(bp, LK_EXCLUSIVE or LK_NOWAIT, nil)) then
  begin
   if (ap^.a_waitfor<>MNT_WAIT)
    continue;
   if (BUF_LOCK(bp,
       LK_EXCLUSIVE or LK_INTERLOCK or LK_SLEEPFAIL,
       BO_MTX(bo))<>0) then
   begin
    BO_LOCK(bo);
    goto loop1;
   end;
   BO_LOCK(bo);
  end;
  BO_UNLOCK(bo);
  Assert(bp^.b_bufobj=bo,
      ('bp %p wrong b_bufobj %p should be %p',
      bp, bp^.b_bufobj, bo));
  if ((bp^.b_flags and B_DELWRI)=0) then
   panic('fsync: not dirty');
  if ((vp^.v_object<>nil) and (bp^.b_flags and B_CLUSTEROK)) then
  begin
   vfs_bio_awrite(bp);
  end else
  begin
   bremfree(bp);
   bawrite(bp);
  end;
  BO_LOCK(bo);
  goto loop2;
 end;

 {
  * If synchronous the caller expects us to completely resolve all
  * dirty buffers in the system.  Wait for in-progress I/O to
  * complete (which could include background bitmap writes), then
  * retry if dirty blocks still exist.
  }
 if (ap^.a_waitfor=MNT_WAIT) then
 begin
  bufobj_wwait(bo, 0, 0);
  if (bo^.bo_dirty.bv_cnt > 0) then
  begin
   {
    * If we are unable to write any of these buffers
    * then we fail now rather than trying endlessly
    * to write them out.
    }
   TAILQ_FOREACH(bp, @bo^.bo_dirty.bv_hd, b_bobufs)
    if ((error:=bp^.b_error)=0) then
     continue;
   if (error=0) and (--maxretry >= 0) then
    goto loop1;
   error:=EAGAIN;
  end;
 end;
 BO_UNLOCK(bo);
 if (error=EAGAIN) then
  vprint('fsync: giving up on dirty', vp);
 }

 Exit(error);
end;

{ XXX Needs good comment and more info in the manpage (VOP_GETPAGES(9)). }
function vop_stdgetpages(ap:p_vop_getpages_args):Integer;
begin
 Exit(EOPNOTSUPP);
 //Exit(vnode_pager_generic_getpages(ap^.a_vp, ap^.a_m, ap^.a_count, ap^.a_reqpage));
end;

function vop_stdkqfilter(ap:p_vop_kqfilter_args):Integer;
begin
 Exit(vfs_kqfilter(ap));
end;

{ XXX Needs good comment and more info in the manpage (VOP_PUTPAGES(9)). }
function vop_stdputpages(ap:p_vop_putpages_args):Integer;
begin
 Exit(EOPNOTSUPP);
 //Exit(vnode_pager_generic_putpages(ap^.a_vp, ap^.a_m, ap^.a_count, ap^.a_sync, ap^.a_rtvals));
end;

function vop_stdvptofh(ap:p_vop_vptofh_args):Integer;
begin
 Exit(EOPNOTSUPP);
end;

function vop_stdvptocnp(ap:p_vop_vptocnp_args):Integer;
label
 _out;
var
 vp:p_vnode;
 dvp:pp_vnode;
 buf:PChar;
 buflen:PInteger;
 dirbuf, cpos:PByte;
 i, error, eofflag, dirbuflen, flags, locked, len, covered:Integer;
 off:QWORD;
 fileno:DWORD;
 va:t_vattr;
 nd:t_nameidata;
 td:p_kthread;
 dp:p_dirent;
 mvp:p_vnode;
begin
 vp:=ap^.a_vp;
 dvp:=ap^.a_vpp;
 buf:=ap^.a_buf;
 buflen:=ap^.a_buflen;

 i:=buflen^;
 error:=0;
 covered:=0;
 td:=curkthread;

 if (vp^.v_type<>VDIR) then
  Exit(ENOENT);

 error:=VOP_GETATTR(vp, @va);
 if (error<>0) then
  Exit(error);

 VREF(vp);
 locked:=VOP_ISLOCKED(vp);
 VOP_UNLOCK(vp, 0);
 NDINIT_ATVP(@nd, LOOKUP, FOLLOW or LOCKLEAF, UIO_SYSSPACE, '..', vp, td);
 flags:=FREAD;
 error:=vn_open_cred(@nd, @flags, 0, VN_OPEN_NOAUDIT, nil);
 if (error<>0) then
 begin
  vn_lock(vp, locked or LK_RETRY);
  Exit(error);
 end;
 NDFREE(@nd, NDF_ONLY_PNBUF);

 mvp:=nd.ni_vp;
 dvp^:=mvp;

 if (vp^.v_mount<>dvp^^.v_mount) and
    ((dvp^^.v_vflag and VV_ROOT)<>0) and
    ((p_mount(dvp^^.v_mount)^.mnt_flag and MNT_UNION)<>0) then
 begin
  dvp^:=p_mount(dvp^^.v_mount)^.mnt_vnodecovered;
  VREF(mvp);
  VOP_UNLOCK(mvp, 0);
  vn_close(mvp, FREAD);
  VREF(dvp^);
  vn_lock(dvp^, LK_EXCLUSIVE or LK_RETRY);
  covered:=1;
 end;

 fileno:=va.va_fileid;

 dirbuflen:=DEV_BSIZE;
 if (dirbuflen < va.va_blocksize) then
  dirbuflen:=va.va_blocksize;
 dirbuf:=AllocMem(dirbuflen);

 if (dvp^^.v_type<>VDIR) then
 begin
  error:=ENOENT;
  goto _out;
 end;

 off:=0;
 len:=0;
 repeat
  { call VOP_READDIR of parent }
  error:=get_next_dirent(dvp^, @dp, dirbuf, dirbuflen, @off, @cpos, @len, @eofflag);
  if (error<>0) then
   goto _out;

  if (dp^.d_type<>DT_WHT) and
     (dp^.d_fileno=fileno) then
  begin
   if (covered<>0) then
   begin
    VOP_UNLOCK(dvp^, 0);
    vn_lock(mvp, LK_EXCLUSIVE or LK_RETRY);
    if (dirent_exists(mvp, dp^.d_name)<>0) then
    begin
     error:=ENOENT;
     VOP_UNLOCK(mvp, 0);
     vn_lock(dvp^, LK_EXCLUSIVE or LK_RETRY);
     goto _out;
    end;
    VOP_UNLOCK(mvp, 0);
    vn_lock(dvp^, LK_EXCLUSIVE or LK_RETRY);
   end;
   i -= dp^.d_namlen;

   if (i < 0) then
   begin
    error:=ENOMEM;
    goto _out;
   end;
   if (dp^.d_namlen=1) and (dp^.d_name[0]='.') then
   begin
    error:=ENOENT;
   end else
   begin
    Move(dp^.d_name, (buf + i)^, dp^.d_namlen);
    error:=0;
   end;
   goto _out;
  end;
 until not ((len>0) or (eofflag=0));
 error:=ENOENT;

_out:
 FreeMem(dirbuf);
 if (error=0) then
 begin
  buflen^:=i;
  vref(dvp^);
 end;
 if (covered<>0) then
 begin
  vput(dvp^);
  vrele(mvp);
 end else
 begin
  VOP_UNLOCK(mvp, 0);
  vn_close(mvp, FREAD);
 end;
 vn_lock(vp, locked or LK_RETRY);
 Exit(error);
end;

function vop_stdallocate(ap:p_vop_allocate_args):Integer;
label
 _out;
var
 aiov:iovec;
 vattr:t_vattr;
 vap:p_vattr;
 auio:t_uio;
 fsize, len, cur, offset:QWORD;
 buf:PByte;
 td:p_kthread;
 vp:p_vnode;
 iosize:QWORD;
 error:Integer;
begin
 buf:=nil;
 error:=0;
 td:=curkthread;
 vap:=@vattr;
 vp:=ap^.a_vp;
 len:=ap^.a_len^;
 offset:=ap^.a_offset^;

 error:=VOP_GETATTR(vp, vap);
 if (error<>0) then
  goto _out;
 fsize:=vap^.va_size;
 iosize:=vap^.va_blocksize;
 if (iosize=0) then
  iosize:=BLKDEV_IOSIZE;
 if (iosize > MAXPHYS) then
  iosize:=MAXPHYS;
 buf:=AllocMem(iosize);

 if (offset + len > vap^.va_size) then
 begin
  {
   * Test offset + len against the filesystem's maxfilesize.
   }
  VATTR_null(vap);
  vap^.va_size:=offset + len;
  error:=VOP_SETATTR(vp, vap);
  if (error<>0) then
   goto _out;
  VATTR_null(vap);
  vap^.va_size:=fsize;
  error:=VOP_SETATTR(vp, vap);
  if (error<>0) then
   goto _out;
 end;

 repeat
  {
   * Read and write back anything below the nominal file
   * size.  There's currently no way outside the filesystem
   * to know whether this area is sparse or not.
   }
  cur:=iosize;
  if ((offset mod iosize)<>0) then
   Dec(cur,(offset mod iosize));
  if (cur > len) then
   cur:=len;
  if (offset < fsize) then
  begin
   aiov.iov_base:=buf;
   aiov.iov_len:=cur;
   auio.uio_iov:=@aiov;
   auio.uio_iovcnt:=1;
   auio.uio_offset:=offset;
   auio.uio_resid:=cur;
   auio.uio_segflg:=UIO_SYSSPACE;
   auio.uio_rw:=UIO_READ;
   auio.uio_td:=td;
   error:=VOP_READ(vp, @auio, 0);
   if (error<>0) then
    break;
   if (auio.uio_resid > 0) then
   begin
    FillChar((buf + cur - auio.uio_resid)^,auio.uio_resid,0);
   end;
  end else
  begin
   FillChar(buf^,cur,0);
  end;

  aiov.iov_base:=buf;
  aiov.iov_len:=cur;
  auio.uio_iov:=@aiov;
  auio.uio_iovcnt:=1;
  auio.uio_offset:=offset;
  auio.uio_resid:=cur;
  auio.uio_segflg:=UIO_SYSSPACE;
  auio.uio_rw:=UIO_WRITE;
  auio.uio_td:=td;

  error:=VOP_WRITE(vp, @auio, 0);
  if (error<>0) then
   break;

  Dec(len,cur);
  Inc(offset,cur);
  if (len=0) then
   break;
  //if (should_yield())
  // break;
 until false;

 _out:
 ap^.a_len^:=len;
 ap^.a_offset^:=offset;
 FreeMem(buf);
 Exit(error);
end;

function vop_stdadvise(ap:p_vop_advise_args):Integer;
var
 vp:p_vnode;
 start, __end:QWORD;
 error, vfslocked:Integer;
begin
 vp:=ap^.a_vp;
 case (ap^.a_advice) of
  POSIX_FADV_WILLNEED:
   begin
    {
     * Do nothing for now.  Filesystems should provide a
     * custom method which starts an asynchronous read of
     * the requested region.
     }
    error:=0;
   end;
  POSIX_FADV_DONTNEED:
   begin
    {
     * Flush any open FS buffers and then remove pages
     * from the backing VM object.  Using vinvalbuf() here
     * is a bit heavy-handed as it flushes all buffers for
     * the given vnode, not just the buffers covering the
     * requested range.
     }
    error:=0;
    vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
    vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
    if ((vp^.v_iflag and VI_DOOMED)<>0) then
    begin
     VOP_UNLOCK(vp, 0);
     VFS_UNLOCK_GIANT(vfslocked);
     Exit(error);
    end;
    //vinvalbuf(vp, V_CLEANONLY, 0, 0);
    //if (vp^.v_object<>nil) then
    //begin
    // start:=trunc_page(ap^.a_start);
    // __end:=round_page(ap^.a_end);
    // VM_OBJECT_LOCK(vp^.v_object);
    // vm_object_page_cache(vp^.v_object, OFF_TO_IDX(start), OFF_TO_IDX(__end));
    // VM_OBJECT_UNLOCK(vp^.v_object);
    //end;
    VOP_UNLOCK(vp, 0);
    VFS_UNLOCK_GIANT(vfslocked);
   end;
 else
  begin
   error:=EINVAL;
  end;
 end;
 Exit(error);
end;

function vop_stdunp_bind(ap:p_vop_unp_bind_args):Integer;
begin
 ap^.a_vp^.v_un:=ap^.a_socket;
 Exit(0);
end;

function vop_stdunp_connect(ap:p_vop_unp_connect_args):Integer;
begin
 ap^.a_socket^:=ap^.a_vp^.v_socket;
 Exit(0);
end;

function vop_stdunp_detach(ap:p_vop_unp_detach_args):Integer;
begin
 ap^.a_vp^.v_un:=nil;
 Exit(0);
end;

function vop_stdis_text(ap:p_vop_is_text_args):Integer;
begin
 Exit(ord((ap^.a_vp^.v_vflag and VV_TEXT)<>0));
end;

function vop_stdset_text(ap:p_vop_set_text_args):Integer;
begin
 ap^.a_vp^.v_vflag:=ap^.a_vp^.v_vflag or VV_TEXT;
 Exit(0);
end;

function vop_stdunset_text(ap:p_vop_unset_text_args):Integer;
begin
 ap^.a_vp^.v_vflag:=ap^.a_vp^.v_vflag and (not VV_TEXT);
 Exit(0);
end;

function vop_stdget_writecount(ap:p_vop_get_writecount_args):Integer;
begin
 ap^.a_writecount^:=ap^.a_vp^.v_writecount;
 Exit(0);
end;

function vop_stdadd_writecount(ap:p_vop_add_writecount_args):Integer;
begin
 Inc(ap^.a_vp^.v_writecount,ap^.a_inc);
 Exit(0);
end;

{
 * vfs default ops
 * used to fill the vfs function table to get reasonable default Exitvalues.
 }
function vfs_stdroot(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
begin
 Exit(EOPNOTSUPP);
end;

function vfs_stdstatfs(mp:p_mount;sbp:p_statfs):Integer;
begin
 Exit(EOPNOTSUPP);
end;

function vfs_stdquotactl(mp:p_mount;cmds:Integer;uid:uid_t;arg:Pointer):Integer;
begin
 Exit(EOPNOTSUPP);
end;

function vfs_stdsync(mp:p_mount;waitfor:Integer):Integer;
label
 loop;
var
 vp,mvp:p_vnode;
 error,lockreq,allerror:Integer;
begin
 error:=0;
 lockreq:=0;
 allerror:=0;

 lockreq:=LK_EXCLUSIVE or LK_INTERLOCK;
 if (waitfor<>MNT_WAIT) then
  lockreq:=lockreq or LK_NOWAIT;
 {
  * Force stale buffer cache information to be flushed.
  }
loop:
 vp:=__mnt_vnode_first_all(@mvp,mp);
 while (vp<>nil) do
 begin
  //if (vp^.v_bufobj.bo_dirty.bv_cnt=0) then
  //begin
  // VI_UNLOCK(vp);
  // continue;
  //end;
  error:=vget(vp, lockreq);
  if (error<>0) then
  begin
   if (error=ENOENT) then
   begin
    //MNT_VNODE_FOREACH_ALL_ABORT(mp, mvp);
    MNT_ILOCK(mp);
    __mnt_vnode_markerfree_all(@mvp,mp);
    //MNT_VNODE_FOREACH_ALL_ABORT(mp, mvp);
    goto loop;
   end;
   continue;
  end;
  error:=VOP_FSYNC(vp, waitfor);
  if (error<>0) then
   allerror:=error;
  vput(vp);

  vp:=__mnt_vnode_next_all(@mvp,mp)
 end;
 Exit(allerror);
end;

function vfs_stdnosync(mp:p_mount;waitfor:Integer):Integer;
begin
 Exit(0);
end;

function vfs_stdvget(mp:p_mount;ino:DWORD;flags:Integer;vpp:pp_vnode):Integer;
begin
 Exit(EOPNOTSUPP);
end;

function vfs_stdfhtovp(mp:p_mount;fhp:p_fid;flags:Integer;vpp:pp_vnode) :Integer;
begin
 Exit(EOPNOTSUPP);
end;

function vfs_stdinit(vfsp:p_vfsconf):Integer;
begin
 Exit(0);
end;

function vfs_stduninit(vfsp:p_vfsconf):Integer;
begin
 Exit(0);
end;

function vfs_stdextattrctl(mp:p_mount;cmd:Integer;filename_vp:p_vnode;attrnamespace:Integer;attrname:PChar):Integer;
begin
 if (filename_vp<>nil) then
  VOP_UNLOCK(filename_vp, 0);
 Exit(EOPNOTSUPP);
end;

function vfs_stdsysctl(mp:p_mount;op:Integer;req:Pointer):Integer;
begin
 Exit(EOPNOTSUPP);
end;

{ end of vfs default ops }


end.

