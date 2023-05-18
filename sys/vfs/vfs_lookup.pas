unit vfs_lookup;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 systm,
 vcapability,
 vuio,
 vfile,
 vfiledesc,
 vfcntl,
 vnode,
 vnode_if,
 vnamei,
 vmount,
 kern_mtx,
 kern_thr,
 kern_descrip;

function  nd_namei(ndp:p_nameidata):Integer;
function  nd_lookup(ndp:p_nameidata):Integer;
procedure NDFREE(ndp:p_nameidata;flags:Integer);

procedure nameiinit; //SYSINIT(vfs, SI_SUB_VFS, SI_ORDER_SECOND, nameiinit, NULL);

implementation

uses
 errno,
 vfs_subr,
 vfs_vnops,
 dead_vnops;

var
 vp_crossmp:p_vnode;
 lookup_shared:Integer=1;

procedure nameiinit;
begin
 getnewvnode('crossmp', nil, @dead_vnodeops, @vp_crossmp);
 //vn_lock(vp_crossmp, LK_EXCLUSIVE);
 //VN_LOCK_ASHARE(vp_crossmp);
 //VOP_UNLOCK(vp_crossmp, 0);
end;

{
 * Convert a pathname into a pointer to a locked vnode.
 *
 * The FOLLOW flag is set when symbolic links are to be followed
 * when they occur at the end of the name translation process.
 * Symbolic links are always followed for all other pathname
 * components other than the last.
 *
 * The segflg defines whether the name is to be copied from user
 * space or kernel space.
 *
 * Overall outline of namei:
 *
 * copy in name
 * get starting directory
 * while (!done and !error) begin
 *  call lookup to search path.
 *  if symbolic link, massage name in buffer and continue
 * end;
 }
procedure namei_cleanup_cnp(cnp:p_componentname); inline;
begin
 FreeMem(cnp^.cn_pnbuf);
end;

function zalloc_namei:Pointer; inline;
begin
 Result:=AllocMem(vfile.MAXPATHLEN);
end;

function nd_namei(ndp:p_nameidata):Integer;
var
 cp:PChar;  { pointer into pathname argument }
 dp:p_vnode; { the directory we are searching }
 aiov:iovec;  { uio for reading symbolic links }
 auio:t_uio;
 error, linklen:Integer;
 cnp:p_componentname;
 td:p_kthread;
 vfslocked:Integer;
begin
 cnp:=@ndp^.ni_cnd;
 td:=cnp^.cn_thread;

 Assert(((cnp^.cn_flags and MPSAFE)<>0) or mtx_owned(VFS_Giant),'NOT MPSAFE and Giant not held');

 Assert((cnp^.cn_nameiop and (not OPMASK))=0,'namei: nameiop contaminated with flags');
 Assert((cnp^.cn_flags and OPMASK)=0,'namei: flags contaminated with nameiops');

 if (lookup_shared=0) then
  cnp^.cn_flags:=cnp^.cn_flags and (not LOCKSHARED);

 { We will set this ourselves if we need it. }
 cnp^.cn_flags:=cnp^.cn_flags and (not TRAILINGSLASH);

 {
  * Get a buffer for the name to be translated, and copy the
  * name into the buffer.
  }
 if ((cnp^.cn_flags and HASBUF)=0) then
  cnp^.cn_pnbuf:=zalloc_namei;

 if (ndp^.ni_segflg=UIO_SYSSPACE) then
 begin
  error:=copystr(ndp^.ni_dirp, cnp^.cn_pnbuf, MAXPATHLEN, @ndp^.ni_pathlen);
 end else
 begin
  error:=copyinstr(ndp^.ni_dirp, cnp^.cn_pnbuf, MAXPATHLEN, @ndp^.ni_pathlen);
 end;

 {
  * Don't allow empty pathnames.
  }
 if (error=0) and (cnp^.cn_pnbuf^=#0) then
  error:=ENOENT;

 if (error<>0) then
 begin
  namei_cleanup_cnp(cnp);
  ndp^.ni_vp:=nil;
  Exit(error);
 end;
 ndp^.ni_loopcnt:=0;

 {
  * Get starting point for the translation.
  }
 FILEDESC_SLOCK(@fd_table);
 ndp^.ni_rootdir:=fd_table.fd_rdir;
 ndp^.ni_topdir :=fd_table.fd_jdir;

 dp:=nil;
 if (cnp^.cn_pnbuf[0]<>'/') then
 begin
  if (ndp^.ni_startdir<>nil) then
  begin
   dp:=ndp^.ni_startdir;
   error:=0;
  end else
  if (ndp^.ni_dirfd<>AT_FDCWD) then
  begin
   error:=fgetvp_rights(ndp^.ni_dirfd,
                        ndp^.ni_rightsneeded or CAP_LOOKUP,
                        @ndp^.ni_baserights,
                        @dp);

  end;
  if (error<>0) or (dp<>nil) then
  begin
   FILEDESC_SUNLOCK(@fd_table);
   if (error=0) and (dp^.v_type<>VDIR) then
   begin
    vfslocked:=VFS_LOCK_GIANT(dp^.v_mount);
    vrele(dp);
    VFS_UNLOCK_GIANT(vfslocked);
    error:=ENOTDIR;
   end;
  end;
  if (error<>0) then
  begin
   namei_cleanup_cnp(cnp);
   Exit(error);
  end;
 end;
 if (dp=nil) then
 begin
  dp:=fd_table.fd_cdir;
  VREF(dp);
  FILEDESC_SUNLOCK(@fd_table);
  if (ndp^.ni_startdir<>nil) then
  begin
   vfslocked:=VFS_LOCK_GIANT(ndp^.ni_startdir^.v_mount);
   vrele(ndp^.ni_startdir);
   VFS_UNLOCK_GIANT(vfslocked);
  end;
 end;

 vfslocked:=VFS_LOCK_GIANT(dp^.v_mount);

 while (true) do
 begin
  {
   * Check if root directory should replace current directory.
   * Done at start of translation and after symbolic link.
   }
  cnp^.cn_nameptr:=cnp^.cn_pnbuf;
  if (cnp^.cn_nameptr^='/') then
  begin
   vrele(dp);
   VFS_UNLOCK_GIANT(vfslocked);
   if (ndp^.ni_strictrelative<>0) then
   begin
    namei_cleanup_cnp(cnp);
    Exit(ENOTCAPABLE);
   end;
   while (cnp^.cn_nameptr^='/') do
   begin
    Inc(cnp^.cn_nameptr);
    Dec(ndp^.ni_pathlen);
   end;
   dp:=ndp^.ni_rootdir;
   vfslocked:=VFS_LOCK_GIANT(dp^.v_mount);
   VREF(dp);
  end;
  if (vfslocked<>0) then
   ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags or GIANTHELD;

  ndp^.ni_startdir:=dp;
  error:=nd_lookup(ndp);
  if (error<>0) then
  begin
   namei_cleanup_cnp(cnp);
   Exit(error);
  end;
  vfslocked:=ord((ndp^.ni_cnd.cn_flags and GIANTHELD)<>0);
  ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags and (not GIANTHELD);
  {
   * If not a symbolic link, we're done.
   }
  if ((cnp^.cn_flags and ISSYMLINK)=0) then
  begin
   if ((cnp^.cn_flags and (SAVENAME or SAVESTART))=0) then
   begin
    namei_cleanup_cnp(cnp);
   end else
    cnp^.cn_flags:=cnp^.cn_flags or HASBUF;

   if ((cnp^.cn_flags and MPSAFE)=0) then
   begin
    VFS_UNLOCK_GIANT(vfslocked);
   end else
   if (vfslocked<>0) then
    ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags or GIANTHELD;

   Exit(0);
  end;

  Inc(ndp^.ni_loopcnt);
  if ((ndp^.ni_loopcnt-1)>=MAXSYMLINKS) then
  begin
   error:=ELOOP;
   break;
  end;

  //if ((cnp^.cn_flags and NOMACCHECK)=0) then
  //begin
  // error:=mac_vnode_check_readlink(td^.td_ucred, ndp^.ni_vp);
  // if (error<>0) then
  //  break;
  //end;

  if (ndp^.ni_pathlen>1) then
   cp:=zalloc_namei
  else
   cp:=cnp^.cn_pnbuf;

  aiov.iov_base  :=cp;
  aiov.iov_len   :=MAXPATHLEN;
  auio.uio_iov   :=@aiov;
  auio.uio_iovcnt:=1;
  auio.uio_offset:=0;
  auio.uio_rw    :=UIO_READ;
  auio.uio_segflg:=UIO_SYSSPACE;
  auio.uio_td    :=td;
  auio.uio_resid :=MAXPATHLEN;

  error:=VOP_READLINK(ndp^.ni_vp, @auio);
  if (error<>0) then
  begin
   if (ndp^.ni_pathlen > 1) then
    FreeMem(cp);
   break;
  end;
  linklen:=MAXPATHLEN - auio.uio_resid;
  if (linklen=0) then
  begin
   if (ndp^.ni_pathlen>1) then
    FreeMem(cp);
   error:=ENOENT;
   break;
  end;
  if (linklen + ndp^.ni_pathlen >= MAXPATHLEN) then
  begin
   if (ndp^.ni_pathlen > 1) then
    FreeMem(cp);
   error:=ENAMETOOLONG;
   break;
  end;
  if (ndp^.ni_pathlen > 1) then
  begin
   Move((cp + linklen)^,ndp^.ni_next^,ndp^.ni_pathlen);
   FreeMem(cnp^.cn_pnbuf);
   cnp^.cn_pnbuf:=cp;
  end else
   cnp^.cn_pnbuf[linklen]:=#0;
  Inc(ndp^.ni_pathlen,linklen);
  vput(ndp^.ni_vp);
  dp:=ndp^.ni_dvp;
 end;
 namei_cleanup_cnp(cnp);
 vput(ndp^.ni_vp);
 ndp^.ni_vp:=nil;
 vrele(ndp^.ni_dvp);
 VFS_UNLOCK_GIANT(vfslocked);

 Exit(error);
end;

function compute_cn_lkflags(mp:p_mount;lkflags,cnflags:Integer):Integer;
begin
 if (mp=nil) then
 begin
  lkflags:=lkflags and (not LK_SHARED);
  lkflags:=lkflags or LK_EXCLUSIVE;
  Exit(lkflags);
 end;

 if (((lkflags and LK_SHARED)<>0) and
    (((mp^.mnt_kern_flag and MNTK_LOOKUP_SHARED)=0) or
    (((cnflags and ISDOTDOT)<>0) and
    ((mp^.mnt_kern_flag and MNTK_LOOKUP_EXCL_DOTDOT)<>0)))) then
 begin
  lkflags:=lkflags and (not LK_SHARED);
  lkflags:=lkflags or LK_EXCLUSIVE;
 end;
 Exit(lkflags);
end;

function needs_exclusive_leaf(mp:p_mount;flags:Integer):Integer;
begin

 {
  * Intermediate nodes can use shared locks, we only need to
  * force an exclusive lock for leaf nodes.
  }
 if ((flags and (ISLASTCN or LOCKLEAF))<>(ISLASTCN or LOCKLEAF)) then
  Exit(0);

 { Always use exclusive locks if LOCKSHARED isn't set. }
 if ((flags and LOCKSHARED)=0) then
  Exit(1);

 {
  * For lookups during open(), if the mount point supports
  * extended shared operations, then use a shared lock for the
  * leaf node, otherwise use an exclusive lock.
  }
 if ((flags and ISOPEN)<>0) then
 begin
  if (mp<>nil) then
  if ((mp^.mnt_kern_flag and MNTK_EXTENDED_SHARED)<>0) then
  begin
   Exit(0)
  end;
  Exit(1);
 end;

 {
  * Lookup requests outside of open() that specify LOCKSHARED
  * only need a shared lock on the leaf vnode.
  }
 Exit(0);
end;

{
 * Search a pathname.
 * This is a very central and rather complicated routine.
 *
 * The pathname is pointed to by ni_ptr and is of length ni_pathlen.
 * The starting directory is taken from ni_startdir. The pathname is
 * descended until done, or a symbolic link is encountered. The variable
 * ni_more is clear if the path is completed; it is set to one if a
 * symbolic link needing interpretation is encountered.
 *
 * The flag argument is LOOKUP, CREATE, RENAME, or DELETE depending on
 * whether the name is to be looked up, created, renamed, or deleted.
 * When CREATE, RENAME, or DELETE is specified, information usable in
 * creating, renaming, or deleting a directory entry may be calculated.
 * If flag has LOCKPARENT or'ed into it, the parent directory is returned
 * locked. If flag has WANTPARENT or'ed into it, the parent directory is
 * returned unlocked. Otherwise the parent directory is not returned. If
 * the target of the pathname exists and LOCKLEAF is or'ed into the flag
 * the target is returned locked, otherwise it is returned unlocked.
 * When creating or renaming and LOCKPARENT is specified, the target may not
 * be '.'.  When deleting and LOCKPARENT is specified, the target may be '.'.
 *
 * Overall outline of lookup:
 *
 * dirloop:
 * identify next component of name at ndp^.ni_ptr
 * handle degenerate case where name is nil string
 * if .. and crossing mount points and on mounted filesys, find parent
 * call VOP_LOOKUP routine for next component name
 *     directory vnode returned in ni_dvp, unlocked unless LOCKPARENT set
 *     component vnode returned in ni_vp (if it exists), locked.
 * if result vnode is mounted on and crossing mount points,
 *     find mounted on vnode
 * if more components of name, do next level at dirloop
 * Exitthe answer in ni_vp, locked if LOCKLEAF set
 *     if LOCKPARENT set, Exitlocked parent in ni_dvp
 *     if WANTPARENT set, Exitunlocked parent in ni_dvp
 }

function nd_lookup(ndp:p_nameidata):Integer;
var
 cp             :PChar  ;  { pointer into pathname argument }
 dp             :p_vnode;  { the directory we are searching }
 tdp            :p_vnode;  { saved dp }
 mp             :p_mount;  { mount table entry }
 docache        :Integer;  {=0 do not cache last component }
 _wantparent    :Integer;  { 1 => wantparent or lockparent flag }
 _rdonly        :Integer;  { lookup read-only flag bit }
 error          :Integer;
 dpunlocked     :Integer;  { dp has already been unlocked }
 cnp            :p_componentname;
 vfslocked      :Integer;  { VFS Giant state for child }
 dvfslocked     :Integer;  { VFS Giant state for parent }
 tvfslocked     :Integer;
 lkflags_save   :Integer;
 ni_dvp_unlocked:Integer;
label
 dirloop,
 bad,
 success,
 nextname,
 unionlookup,
 bad2;
begin
 dp:=nil;
 error:=0;
 dpunlocked:=0;
 cnp:=@ndp^.ni_cnd;

 {
  * Setup: break out flag bits into variables.
  }
 dvfslocked:=ord((ndp^.ni_cnd.cn_flags and GIANTHELD)<>0);
 vfslocked:=0;
 ni_dvp_unlocked:=0;
 ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags and (not GIANTHELD);
 _wantparent:=cnp^.cn_flags and (LOCKPARENT or WANTPARENT);

 Assert((cnp^.cn_nameiop=LOOKUP) or (wantparent<>0),'CREATE, DELETE, RENAME require LOCKPARENT or WANTPARENT.');

 docache:=(cnp^.cn_flags and NOCACHE) xor NOCACHE;
 if (cnp^.cn_nameiop=DELETE) or
    ((_wantparent and cnp^.cn_nameiop<>CREATE) and
     (cnp^.cn_nameiop<>LOOKUP)) then
  docache:=0;

 _rdonly:=cnp^.cn_flags and RDONLY;
 cnp^.cn_flags:=cnp^.cn_flags and (not ISSYMLINK);
 ndp^.ni_dvp:=nil;
 {
  * We use shared locks until we hit the parent of the last cn then
  * we adjust based on the requesting flags.
  }
 if (lookup_shared<>0) then
  cnp^.cn_lkflags:=LK_SHARED
 else
  cnp^.cn_lkflags:=LK_EXCLUSIVE;

 dp:=ndp^.ni_startdir;
 ndp^.ni_startdir:=nil;
 vn_lock(dp,compute_cn_lkflags(dp^.v_mount,cnp^.cn_lkflags or LK_RETRY,cnp^.cn_flags));

dirloop:
 {
  * Search a new directory.
  *
  * The last component of the filename is left accessible via
  * cnp^.cn_nameptr for callers that need the name. Callers needing
  * the name set the SAVENAME flag. When done, they assume
  * responsibility for freeing the pathname buffer.
  }
 cnp^.cn_consume:=0;

 cp:=cnp^.cn_nameptr;
 while (cp^<>#0) and (cp^<>'/') do Inc(cp);

 cnp^.cn_namelen:=cp - cnp^.cn_nameptr;
 if (cnp^.cn_namelen > NAME_MAX) then
 begin
  error:=ENAMETOOLONG;
  goto bad;
 end;

 Dec(ndp^.ni_pathlen,cnp^.cn_namelen);
 ndp^.ni_next:=cp;

 {
  * Replace multiple slashes by a single slash and trailing slashes
  * by a nil.  This must be done before VOP_LOOKUP() because some
  * fs's don't know about trailing slashes.  Remember if there were
  * trailing slashes to handle symlinks, existing non-directories
  * and non-existing files that won't be directories specially later.
  }
 while ((cp^='/') and ((cp[1]='/') or (cp[1]=#0))) do
 begin
  Inc(cp);
  Dec(ndp^.ni_pathlen);
  if (cp^=#0) then
  begin
   ndp^.ni_next^:=#0;
   cnp^.cn_flags:=cnp^.cn_flags or TRAILINGSLASH;
  end;
 end;
 ndp^.ni_next:=cp;

 cnp^.cn_flags:=cnp^.cn_flags or MAKEENTRY;
 if (cp^=#0) and (docache=0) then
  cnp^.cn_flags:=cnp^.cn_flags and (not MAKEENTRY);

 if (cnp^.cn_namelen=2) and
    (cnp^.cn_nameptr[1]='.') and
    (cnp^.cn_nameptr[0]='.') then
  cnp^.cn_flags:=cnp^.cn_flags or ISDOTDOT
 else
  cnp^.cn_flags:=cnp^.cn_flags and (not ISDOTDOT);

 if (ndp^.ni_next^=#0) then
  cnp^.cn_flags:=cnp^.cn_flags or ISLASTCN
 else
  cnp^.cn_flags:=cnp^.cn_flags and (not ISLASTCN);

 if ((cnp^.cn_flags and ISLASTCN)<>0) and
    (cnp^.cn_namelen=1) and
    (cnp^.cn_nameptr[0]='.') and
    ((cnp^.cn_nameiop=DELETE) or (cnp^.cn_nameiop=RENAME)) then
 begin
  error:=EINVAL;
  goto bad;
 end;

 {
  * Check for degenerate name (e.g. / or '')
  * which is a way of talking about a directory,
  * e.g. like '/.' or '.'.
  }
 if (cnp^.cn_nameptr[0]=#0) then
 begin
  if (dp^.v_type<>VDIR) then
  begin
   error:=ENOTDIR;
   goto bad;
  end;
  if (cnp^.cn_nameiop<>LOOKUP) then
  begin
   error:=EISDIR;
   goto bad;
  end;
  if (_wantparent<>0) then
  begin
   ndp^.ni_dvp:=dp;
   VREF(dp);
  end;
  ndp^.ni_vp:=dp;

  if ((cnp^.cn_flags and (LOCKPARENT or LOCKLEAF))=0) then
   VOP_UNLOCK(dp, 0);
  { XXX This should probably move to the top of function. }
  if ((cnp^.cn_flags and SAVESTART)<>0) then
   Assert(false,'lookup: SAVESTART');
  goto success;
 end;

 {
  * Handle '..': five special cases.
  * 0. If doing a capability lookup, return ENOTCAPABLE (this is a
  *    fairly conservative design choice, but it's the only one that we
  *    are satisfied guarantees the property we're looking for).
  * 1. Exitan error if this is the last component of
  *    the name and the operation is DELETE or RENAME.
  * 2. If at root directory (e.g. after chroot)
  *    or at absolute root directory
  *    then ignore it so can't get out.
  * 3. If this vnode is the root of a mounted
  *    filesystem, then replace it with the
  *    vnode which was mounted on so we take the
  *    .. in the other filesystem.
  * 4. If the vnode is the top directory of
  *    the jail or chroot, don't let them out.
  }
 if ((cnp^.cn_flags and ISDOTDOT)<>0) then
 begin
  if (ndp^.ni_strictrelative<>0) then
  begin
   error:=ENOTCAPABLE;
   goto bad;
  end;
  if ((cnp^.cn_flags and ISLASTCN)<>0) and
     ((cnp^.cn_nameiop=DELETE) or (cnp^.cn_nameiop=RENAME)) then
  begin
   error:=EINVAL;
   goto bad;
  end;
  while (true) do
  begin

   if (dp=ndp^.ni_rootdir) or
      (dp=ndp^.ni_topdir) or
      (dp=rootvnode) or
      (((dp^.v_vflag and VV_ROOT)<>0) and
       ((cnp^.cn_flags and NOCROSSMOUNT)<>0)) then
   begin
    ndp^.ni_dvp:=dp;
    ndp^.ni_vp:=dp;
    vfslocked:=VFS_LOCK_GIANT(dp^.v_mount);
    VREF(dp);
    goto nextname;
   end;
   if ((dp^.v_vflag and VV_ROOT)=0) then
    break;
   if ((dp^.v_iflag and VI_DOOMED)<>0) then { forced unmount }
   begin
    error:=ENOENT;
    goto bad;
   end;
   tdp:=dp;
   dp:=p_mount(dp^.v_mount)^.mnt_vnodecovered;
   tvfslocked:=dvfslocked;
   dvfslocked:=VFS_LOCK_GIANT(dp^.v_mount);
   VREF(dp);
   vput(tdp);
   VFS_UNLOCK_GIANT(tvfslocked);
   vn_lock(dp,compute_cn_lkflags(dp^.v_mount,cnp^.cn_lkflags or LK_RETRY,ISDOTDOT));
  end;
 end;

 {
  * We now have a segment name to search for, and a directory to search.
  }
unionlookup:

 //if ((cnp^.cn_flags and NOMACCHECK)=0) begin
 // error:=mac_vnode_check_lookup(cnp^.cn_thread^.td_ucred, dp, cnp);
 // if (error)
 //  goto bad;
 //end;

 ndp^.ni_dvp:=dp;
 ndp^.ni_vp :=nil;
 ASSERT_VOP_LOCKED(dp, 'lookup');
 Assert(vfslocked=0,'lookup: vfslocked %d');
 {
  * If we have a shared lock we may need to upgrade the lock for the
  * last operation.
  }
 if (dp<>vp_crossmp) and
    (VOP_ISLOCKED(dp)=LK_SHARED) and
    ((cnp^.cn_flags and ISLASTCN)<>0) and ((cnp^.cn_flags and LOCKPARENT)<>0) then
 begin
  vn_lock(dp, LK_UPGRADE or LK_RETRY);
 end;

 if ((dp^.v_iflag and VI_DOOMED)<>0) then
 begin
  error:=ENOENT;
  goto bad;
 end;
 {
  * If we're looking up the last component and we need an exclusive
  * lock, adjust our lkflags.
  }
 if (needs_exclusive_leaf(dp^.v_mount, cnp^.cn_flags)<>0) then
  cnp^.cn_lkflags:=LK_EXCLUSIVE;

 lkflags_save:=cnp^.cn_lkflags;
 cnp^.cn_lkflags:=compute_cn_lkflags(dp^.v_mount, cnp^.cn_lkflags, cnp^.cn_flags);

 error:=VOP_LOOKUP(dp, @ndp^.ni_vp, cnp);
 if (error<>0) then
 begin
  cnp^.cn_lkflags:=lkflags_save;
  Assert(ndp^.ni_vp=nil, 'leaf should be empty');

  if (error=ENOENT) and
     ((dp^.v_vflag and VV_ROOT)<>0) and
     (dp^.v_mount<>nil) then
  if ((p_mount(dp^.v_mount)^.mnt_flag and MNT_UNION)<>0) then
  begin
   tdp:=dp;
   dp:=p_mount(dp^.v_mount)^.mnt_vnodecovered;
   tvfslocked:=dvfslocked;
   dvfslocked:=VFS_LOCK_GIANT(dp^.v_mount);
   VREF(dp);
   vput(tdp);
   VFS_UNLOCK_GIANT(tvfslocked);
   vn_lock(dp,compute_cn_lkflags(dp^.v_mount, cnp^.cn_lkflags or LK_RETRY, cnp^.cn_flags));
   goto unionlookup;
  end;

  if (error<>EJUSTRETURN) then
   goto bad;
  {
   * At this point, we know we're at the end of the
   * pathname.  If creating / renaming, we can consider
   * allowing the file or directory to be created / renamed,
   * provided we're not on a read-only filesystem.
   }
  if (_rdonly<>0) then
  begin
   error:=EROFS;
   goto bad;
  end;
  { trailing slash only allowed for directories }
  if ((cnp^.cn_flags and TRAILINGSLASH)<>0) and
     ((cnp^.cn_flags and WILLBEDIR)=0) then
  begin
   error:=ENOENT;
   goto bad;
  end;
  if ((cnp^.cn_flags and LOCKPARENT)=0) then
   VOP_UNLOCK(dp, 0);
  {
   * We Exitwith ni_vp nil to indicate that the entry
   * doesn't currently exist, leaving a pointer to the
   * (possibly locked) directory vnode in ndp^.ni_dvp.
   }
  if ((cnp^.cn_flags and SAVESTART)<>0) then
  begin
   ndp^.ni_startdir:=ndp^.ni_dvp;
   VREF(ndp^.ni_startdir);
  end;
  goto success;
 end else
  cnp^.cn_lkflags:=lkflags_save;

 {
  * Take into account any additional components consumed by
  * the underlying filesystem.
  }
 if (cnp^.cn_consume > 0) then
 begin
  Inc(cnp^.cn_nameptr,cnp^.cn_consume);
  Inc(ndp^.ni_next   ,cnp^.cn_consume);
  Dec(ndp^.ni_pathlen,cnp^.cn_consume);
  cnp^.cn_consume:=0;
 end;

 dp:=ndp^.ni_vp;
 vfslocked:=VFS_LOCK_GIANT(dp^.v_mount);

 {
  * Check to see if the vnode has been mounted on;
  * if so find the root of the mounted filesystem.
  }

 mp:=dp^.v_mountedhere;
 while (dp^.v_type=VDIR) and
       (mp<>nil) and
       ((cnp^.cn_flags and NOCROSSMOUNT)=0) do
 begin
  if (vfs_busy(mp, 0)<>0) then
   continue;

  vput(dp);
  VFS_UNLOCK_GIANT(vfslocked);
  vfslocked:=VFS_LOCK_GIANT(mp);

  if (dp<>ndp^.ni_dvp) then
   vput(ndp^.ni_dvp)
  else
   vrele(ndp^.ni_dvp);

  VFS_UNLOCK_GIANT(dvfslocked);
  dvfslocked:=0;
  vref(vp_crossmp);
  ndp^.ni_dvp:=vp_crossmp;
  error:=VFS_ROOT(mp, compute_cn_lkflags(mp, cnp^.cn_lkflags, cnp^.cn_flags), @tdp);
  vfs_unbusy(mp);

  if (vn_lock(vp_crossmp, LK_SHARED or LK_NOWAIT)<>0) then
   Assert(False,'vp_crossmp exclusively locked or reclaimed');

  if (error<>0) then
  begin
   dpunlocked:=1;
   goto bad2;
  end;
  dp:=tdp;
  ndp^.ni_vp:=tdp;

  mp:=dp^.v_mountedhere;
 end;

 {
  * Check for symbolic link
  }
 if (dp^.v_type=VLNK) and
    (((cnp^.cn_flags and FOLLOW)<>0) or
     ((cnp^.cn_flags and TRAILINGSLASH)<>0) or
     (ndp^.ni_next^='/')) then
 begin
  cnp^.cn_flags:=cnp^.cn_flags or ISSYMLINK;
  if ((dp^.v_iflag and VI_DOOMED)<>0) then
  begin
   {
    * We can't know whether the directory was mounted with
    * NOSYMFOLLOW, so we can't follow safely.
    }
   error:=ENOENT;
   goto bad2;
  end;
  if ((p_mount(dp^.v_mount)^.mnt_flag and MNT_NOSYMFOLLOW)<>0) then
  begin
   error:=EACCES;
   goto bad2;
  end;
  {
   * Symlink code always expects an unlocked dvp.
   }
  if (ndp^.ni_dvp<>ndp^.ni_vp) then
  begin
   VOP_UNLOCK(ndp^.ni_dvp, 0);
   ni_dvp_unlocked:=1;
  end;
  goto success;
 end;

nextname:
 {
  * Not a symbolic link that we will follow.  Continue with the
  * next component if there is any; otherwise, we're done.
  }
 Assert(((cnp^.cn_flags and ISLASTCN)<>0) or (ndp^.ni_next^='/'),'lookup: invalid path state.');
 if (ndp^.ni_next^='/') then
 begin
  cnp^.cn_nameptr:=ndp^.ni_next;
  while (cnp^.cn_nameptr^='/') do
  begin
   Inc(cnp^.cn_nameptr);
   Dec(ndp^.ni_pathlen);
  end;
  if (ndp^.ni_dvp<>dp) then
   vput(ndp^.ni_dvp)
  else
   vrele(ndp^.ni_dvp);

  VFS_UNLOCK_GIANT(dvfslocked);
  dvfslocked:=vfslocked; { dp becomes dvp in dirloop }
  vfslocked:=0;
  goto dirloop;
 end;
 {
  * If we're processing a path with a trailing slash,
  * check that the end result is a directory.
  }
 if ((cnp^.cn_flags and TRAILINGSLASH)<>0) and (dp^.v_type<>VDIR) then
 begin
  error:=ENOTDIR;
  goto bad2;
 end;
 {
  * Disallow directory write attempts on read-only filesystems.
  }
 if (_rdonly<>0) and
    ((cnp^.cn_nameiop=DELETE) or (cnp^.cn_nameiop=RENAME)) then
 begin
  error:=EROFS;
  goto bad2;
 end;
 if ((cnp^.cn_flags and SAVESTART)<>0) then
 begin
  ndp^.ni_startdir:=ndp^.ni_dvp;
  VREF(ndp^.ni_startdir);
 end;
 if (_wantparent=0) then
 begin
  ni_dvp_unlocked:=2;
  if (ndp^.ni_dvp<>dp) then
   vput(ndp^.ni_dvp)
  else
   vrele(ndp^.ni_dvp);

  VFS_UNLOCK_GIANT(dvfslocked);
  dvfslocked:=0;
 end else
 if ((cnp^.cn_flags and LOCKPARENT)=0) and (ndp^.ni_dvp<>dp) then
 begin
  VOP_UNLOCK(ndp^.ni_dvp, 0);
  ni_dvp_unlocked:=1;
 end;

 if ((cnp^.cn_flags and LOCKLEAF)=0) then
  VOP_UNLOCK(dp, 0);

success:
 {
  * Because of lookup_shared we may have the vnode shared locked, but
  * the caller may want it to be exclusively locked.
  }
 if (needs_exclusive_leaf(dp^.v_mount, cnp^.cn_flags)<>0) and
    (VOP_ISLOCKED(dp)<>LK_EXCLUSIVE) then
 begin
  vn_lock(dp, LK_UPGRADE or LK_RETRY);
  if ((dp^.v_iflag and VI_DOOMED)<>0) then
  begin
   error:=ENOENT;
   goto bad2;
  end;
 end;
 if (vfslocked<>0) and (dvfslocked<>0) then
 begin
  VFS_UNLOCK_GIANT(dvfslocked); { Only need one }
 end;

 if (vfslocked<>0) or (dvfslocked<>0) then
 begin
  ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags or GIANTHELD;
 end;

 Exit(0);

bad2:
 if (ni_dvp_unlocked<>2) then
 begin
  if (dp<>ndp^.ni_dvp) and (ni_dvp_unlocked=0) then
   vput(ndp^.ni_dvp)
  else
   vrele(ndp^.ni_dvp);
 end;
bad:
 if (dpunlocked=0) then
  vput(dp);

 VFS_UNLOCK_GIANT(vfslocked);
 VFS_UNLOCK_GIANT(dvfslocked);
 ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags and (not GIANTHELD);
 ndp^.ni_vp:=nil;
 Exit(error);
end;

{
 * relookup - lookup a path name component
 *    Used by lookup to re-acquire things.
 }
{
int
relookup(struct vnode *dvp, struct vnode **vpp, struct componentname *cnp)
begin
 struct vnode *dp:=0;  { the directory we are searching }
 int _wantparent;   { 1 => wantparent or lockparent flag }
 int _rdonly;   { lookup read-only flag bit }
 int error:=0;

 Assert(cnp^.cn_flags and ISLASTCN,
     ('relookup: Not given last component.'));
 {
  * Setup: break out flag bits into variables.
  }
 _wantparent:=cnp^.cn_flags and (LOCKPARENT or WANTPARENT);
 Assert(_wantparent, ('relookup: parent not wanted.'));
 _rdonly:=cnp^.cn_flags and RDONLY;
 cnp^.cn_flags:= and ~ISSYMLINK;
 dp:=dvp;
 cnp^.cn_lkflags:=LK_EXCLUSIVE;
 vn_lock(dp, LK_EXCLUSIVE or LK_RETRY);

 {
  * Search a new directory.
  *
  * The last component of the filename is left accessible via
  * cnp^.cn_nameptr for callers that need the name. Callers needing
  * the name set the SAVENAME flag. When done, they assume
  * responsibility for freeing the pathname buffer.
  }

 {
  * Check for '' which represents the root directory after slash
  * removal.
  }
 if (cnp^.cn_nameptr[0]='\0') begin
  {
   * Support only LOOKUP for '/' because lookup()
   * can't succeed for CREATE, DELETE and RENAME.
   }
  Assert(cnp^.cn_nameiop=LOOKUP, ('nameiop must be LOOKUP'));
  Assert(dp^.v_type=VDIR, ('dp is not a directory'));

  if (!(cnp^.cn_flags and LOCKLEAF))
   VOP_UNLOCK(dp, 0);
  *vpp:=dp;
  { XXX This should probably move to the top of function. }
  if (cnp^.cn_flags and SAVESTART)
   panic('lookup: SAVESTART');
  Exit(0);
 end;

 if (cnp^.cn_flags and ISDOTDOT)
  panic ('relookup: lookup on dot-dot');

 {
  * We now have a segment name to search for, and a directory to search.
  }
 if ((error:=VOP_LOOKUP(dp, vpp, cnp))<>0) begin
  Assert(*vpp=nil, ('leaf should be empty'));
  if (error<>EJUSTRETURN)
   goto bad;
  {
   * If creating and at end of pathname, then can consider
   * allowing file to be created.
   }
  if (_rdonly) begin
   error:=EROFS;
   goto bad;
  end;
  { ASSERT(dvp=ndp^.ni_startdir) }
  if (cnp^.cn_flags and SAVESTART)
   VREF(dvp);
  if ((cnp^.cn_flags and LOCKPARENT)=0)
   VOP_UNLOCK(dp, 0);
  {
   * We Exitwith ni_vp nil to indicate that the entry
   * doesn't currently exist, leaving a pointer to the
   * (possibly locked) directory vnode in ndp^.ni_dvp.
   }
  Exit(0);
 end;

 dp:=*vpp;

 {
  * Disallow directory write attempts on read-only filesystems.
  }
 if (_rdonly and
     (cnp^.cn_nameiop=DELETE or cnp^.cn_nameiop=RENAME)) begin
  if (dvp=dp)
   vrele(dvp);
  else
   vput(dvp);
  error:=EROFS;
  goto bad;
 end;
 {
  * Set the parent lock/ref state to the requested state.
  }
 if ((cnp^.cn_flags and LOCKPARENT)=0 and dvp<>dp) begin
  if (_wantparent)
   VOP_UNLOCK(dvp, 0);
  else
   vput(dvp);
 end; else if (!_wantparent)
  vrele(dvp);
 {
  * Check for symbolic link
  }
 Assert(dp^.v_type<>VLNK or !(cnp^.cn_flags and FOLLOW),
     ('relookup: symlink found.\n'));

 { ASSERT(dvp=ndp^.ni_startdir) }
 if (cnp^.cn_flags and SAVESTART)
  VREF(dvp);

 if ((cnp^.cn_flags and LOCKLEAF)=0)
  VOP_UNLOCK(dp, 0);
 Exit(0);
bad:
 vput(dp);
 *vpp:=nil;
 Exit(error);
end;
}

{
 * Free data allocated by namei(); see namei(9) for details.
 }
procedure NDFREE(ndp:p_nameidata;flags:Integer);
var
 unlock_dvp:Integer;
 unlock_vp :Integer;
begin
 unlock_dvp:=0;
 unlock_vp:=0;

 if ((flags and NDF_NO_FREE_PNBUF)=0) and
    ((ndp^.ni_cnd.cn_flags and HASBUF<>0)) then
 begin
  FreeMem(ndp^.ni_cnd.cn_pnbuf);
  ndp^.ni_cnd.cn_flags:=ndp^.ni_cnd.cn_flags and (not HASBUF);
 end;
 if ((flags and NDF_NO_VP_UNLOCK)=0) and
    ((ndp^.ni_cnd.cn_flags and LOCKLEAF)<>0) and
    (ndp^.ni_vp<>nil) then
  unlock_vp:=1;
 if (((flags and NDF_NO_VP_RELE)=0) and (ndp^.ni_vp<>nil)) then
 begin
  if (unlock_vp<>0) then
  begin
   vput(ndp^.ni_vp);
   unlock_vp:=0;
  end else
   vrele(ndp^.ni_vp);
  ndp^.ni_vp:=nil;
 end;
 if (unlock_vp<>0) then
  VOP_UNLOCK(ndp^.ni_vp, 0);
 if ((flags and NDF_NO_DVP_UNLOCK)=0) and
    ((ndp^.ni_cnd.cn_flags and LOCKPARENT)<>0) and
    (ndp^.ni_dvp<>ndp^.ni_vp) then
  unlock_dvp:=1;
 if ((flags and NDF_NO_DVP_RELE)=0) and
    ((ndp^.ni_cnd.cn_flags and (LOCKPARENT or WANTPARENT))<>0) then
 begin
  if (unlock_dvp<>0) then
  begin
   vput(ndp^.ni_dvp);
   unlock_dvp:=0;
  end else
   vrele(ndp^.ni_dvp);
  ndp^.ni_dvp:=nil;
 end;
 if (unlock_dvp<>0) then
  VOP_UNLOCK(ndp^.ni_dvp, 0);
 if ((flags and NDF_NO_STARTDIR_RELE)=0) and
    ((ndp^.ni_cnd.cn_flags and SAVESTART)<>0) then
 begin
  vrele(ndp^.ni_startdir);
  ndp^.ni_startdir:=nil;
 end;
end;


end.

