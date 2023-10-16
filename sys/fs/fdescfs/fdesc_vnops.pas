unit fdesc_vnops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_param,
 vmount,
 vnode,
 vfs_default,
 vnode_if,
 fdescfs;

{
 * /dev/fd Filesystem
 }
function fdesc_getattr(ap:p_vop_getattr_args):Integer;
function fdesc_lookup(ap:p_vop_lookup_args):Integer;
function fdesc_open(ap:p_vop_open_args):Integer;
function fdesc_readdir(ap:p_vop_readdir_args):Integer;
function fdesc_reclaim(ap:p_vop_reclaim_args):Integer;
function fdesc_setattr(ap:p_vop_setattr_args):Integer;

const
 fdesc_vnodeops:vop_vector=(
  vop_default       :@default_vnodeops;
  vop_bypass        :nil;

  vop_islocked      :nil;
  vop_lookup        :@fdesc_lookup;
  vop_create        :nil;
  vop_whiteout      :nil;
  vop_mknod         :nil;
  vop_open          :@fdesc_open;
  vop_close         :nil;
  vop_access        :@VOP_NULL;
  vop_accessx       :nil;
  vop_getattr       :@fdesc_getattr;
  vop_setattr       :@fdesc_setattr;
  vop_markatime     :nil;
  vop_read          :nil;
  vop_write         :nil;
  vop_ioctl         :nil;
  vop_poll          :nil;
  vop_kqfilter      :nil;
  vop_revoke        :nil;
  vop_fsync         :nil;
  vop_remove        :nil;
  vop_link          :nil;
  vop_rename        :nil;
  vop_mkdir         :nil;
  vop_rmdir         :nil;
  vop_symlink       :nil;
  vop_readdir       :@fdesc_readdir;
  vop_readlink      :nil;
  vop_inactive      :nil;
  vop_reclaim       :@fdesc_reclaim;
  vop_lock1         :nil;
  vop_unlock        :nil;
  vop_bmap          :nil;
  vop_strategy      :nil;
  vop_getwritemount :nil;
  vop_print         :nil;
  vop_pathconf      :@vop_stdpathconf;
  vop_advlock       :nil;
  vop_advlockasync  :nil;
  vop_advlockpurge  :nil;
  vop_reallocblks   :nil;
  vop_getpages      :nil;
  vop_putpages      :nil;
  vop_vptofh        :nil;
  vop_vptocnp       :nil;
  vop_allocate      :nil;
  vop_unp_bind      :nil;
  vop_unp_connect   :nil;
  vop_unp_detach    :nil;
 );

procedure fdesc_insmntque_dtr(vp:p_vnode;arg:Pointer);
procedure fdesc_remove_entry(fd:p_fdescnode);

function  fdesc_allocvp(ftype:fdntype;fd_fd,ix:Integer;mp:p_mount;vpp:pp_vnode):Integer;

implementation

uses
 sysutils,
 errno,
 time,
 vfile,
 vfiledesc,
 vnamei,
 vstat,
 vdirent,
 vcapability,
 vuio,
 subr_uio,
 vfs_vnops,
 vfs_subr,
 vfs_syscalls,
 kern_mtx,
 kern_descrip,
 kern_thr;

function VFSTOFDESC(mp:p_mount):p_fdescmount; inline;
begin
 Result:=mp^.mnt_data;
end;

function VTOFDESC(vp:p_vnode):p_fdescnode; inline;
begin
 Result:=vp^.v_data;
end;

{
 * If allocating vnode fails, call this.
 }
procedure fdesc_insmntque_dtr(vp:p_vnode;arg:Pointer);
begin
 vgone(vp);
 vput(vp);
end;

{
 * Remove an entry from the hash if it exists.
 }
procedure fdesc_remove_entry(fd:p_fdescnode);
var
 fc:p_fdhashhead;
 fd2:p_fdescnode;
begin
 fc:=FD_NHASH(fd^.fd_ix);
 mtx_lock(fdesc_hashmtx);
 fd2:=LIST_FIRST(fc);
 while (fd2<>nil) do
 begin
  if (fd=fd2) then
  begin
   LIST_REMOVE(fd,@fd^.fd_hash);
   break;
  end;
  fd2:=LIST_NEXT(fd2,@fd2^.fd_hash);
 end;
 mtx_unlock(fdesc_hashmtx);
end;

function fdesc_allocvp(ftype:fdntype;fd_fd,ix:Integer;mp:p_mount;vpp:pp_vnode):Integer;
label
 _or,
 loop;
var
 fmp:p_fdescmount;
 fc:p_fdhashhead;
 fd,fd2:p_fdescnode;
 vp,vp2:p_vnode;
 error:Integer;
begin
 error:=0;

 fc:=FD_NHASH(ix);
loop:
 mtx_lock(fdesc_hashmtx);
 {
  * If a forced unmount is progressing, we need to drop it. The flags are
  * protected by the hashmtx.
  }
 fmp:=p_fdescmount(mp^.mnt_data);

 if (fmp=nil) then
 begin
  mtx_unlock(fdesc_hashmtx);
  Exit(-1);
 end;

 if ((fmp^.flags and FMNT_UNMOUNTF)<>0) then
 begin
  mtx_unlock(fdesc_hashmtx);
  Exit(-1);
 end;

 fd:=LIST_FIRST(fc);

 while (fd<>nil) do
 begin
  if (fd^.fd_ix=ix) and (fd^.fd_vnode^.v_mount=mp) then
  begin
   { Get reference to vnode in case it's being free'd }
   vp:=fd^.fd_vnode;
   VI_LOCK(vp);
   mtx_unlock(fdesc_hashmtx);
   if (vget(vp, LK_EXCLUSIVE or LK_INTERLOCK)<>0) then
    goto loop;
   vpp^:=vp;
   Exit(0);
  end;
  fd:=LIST_NEXT(fd,@fd^.fd_hash);
 end;
 mtx_unlock(fdesc_hashmtx);

 fd:=AllocMem(sizeof(t_fdescnode));

 error:=getnewvnode('fdescfs', mp, @fdesc_vnodeops, @vp);
 if (error<>0) then
 begin
  FreeMem(fd);
  Exit(error);
 end;

 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);

 vp^.v_data  :=fd;
 fd^.fd_vnode:=vp;
 fd^.fd_type :=ftype;
 fd^.fd_fd   :=fd_fd;
 fd^.fd_ix   :=ix;

 error:=insmntque1(vp, mp, @fdesc_insmntque_dtr, nil);
 if (error<>0) then
 begin
  vpp^:=nil;
  Exit(error);
 end;

 { Make sure that someone didn't beat us when inserting the vnode. }
 mtx_lock(fdesc_hashmtx);
 {
  * If a forced unmount is progressing, we need to drop it. The flags are
  * protected by the hashmtx.
  }
 fmp:=p_fdescmount(mp^.mnt_data);

 if (fmp=nil) then goto _or;

 if ((fmp^.flags and FMNT_UNMOUNTF)<>0) then
 begin
  _or:
  mtx_unlock(fdesc_hashmtx);
  vgone(vp);
  vput(vp);
  vpp^:=nil;
  Exit(-1);
 end;

 fd2:=LIST_FIRST(fc);
 while (fd2<>nil) do
 begin
  if (fd2^.fd_ix=ix) and (fd2^.fd_vnode^.v_mount=mp) then
  begin
   { Get reference to vnode in case it's being free'd }
   vp2:=fd2^.fd_vnode;
   VI_LOCK(vp2);
   mtx_unlock(fdesc_hashmtx);
   error:=vget(vp2, LK_EXCLUSIVE or LK_INTERLOCK);
   { Someone beat us, dec use count and wait for reclaim }
   vgone(vp);
   vput(vp);
   { If we didn't get it, return no vnode. }
   if (error<>0) then
    vp2:=nil;
   vpp^:=vp2;
   Exit(error);
  end;
  fd2:=LIST_NEXT(fd2,@fd2^.fd_hash);
 end;

 { If we came here, we can insert it safely. }
 LIST_INSERT_HEAD(fc,fd,@fd^.fd_hash);
 mtx_unlock(fdesc_hashmtx);
 vpp^:=vp;
 Exit(0);
end;

{
 * vp is the current namei directory
 * ndp is the name to locate in that directory...
 }
function fdesc_lookup(ap:p_vop_lookup_args):Integer;
label
 bad;
var
 vpp:pp_vnode;
 dvp:p_vnode;
 cnp:p_componentname;
 pname:PChar;
 fp:p_file;
 nlen:Integer;
 fd,fd1:Integer;
 error:Integer;
 fvp:p_vnode;
begin
 vpp:=ap^.a_vpp;
 dvp:=ap^.a_dvp;
 cnp:=ap^.a_cnp;
 pname:=cnp^.cn_nameptr;
 nlen:=cnp^.cn_namelen;

 if ((cnp^.cn_flags and ISLASTCN)<>0) and
    ((cnp^.cn_nameiop=DELETE) or (cnp^.cn_nameiop=RENAME)) then
 begin
  error:=EROFS;
  goto bad;
 end;

 if (cnp^.cn_namelen=1) and (pname^='.') then
 begin
  vpp^:=dvp;
  VREF(dvp);
  Exit(0);
 end;

 if (VTOFDESC(dvp)^.fd_type<>_Froot) then
 begin
  error:=ENOTDIR;
  goto bad;
 end;

 fd:=0;
 { the only time a leading 0 is acceptable is if it's '0' }
 if (pname^='0') and (nlen<>1) then
 begin
  error:=ENOENT;
  goto bad;
 end;

 while (nlen<>0) do
 begin
  Dec(nlen);
  if (pname^ < '0') or (pname^ > '9') then
  begin
   error:=ENOENT;
   goto bad;
  end;
  fd1:=10 * fd + ord(pname^) - ord('0');
  Inc(pname);
  if (fd1 < fd) then
  begin
   error:=ENOENT;
   goto bad;
  end;
  fd:=fd1;
 end;

 {
  * No rights to check since 'fp' isn't actually used.
  }
 error:=fget(fd, 0, @fp);
 if (error<>0) then
  goto bad;

 { Check if we're looking up ourselves. }
 if (VTOFDESC(dvp)^.fd_ix=FD_DESC + fd) then
 begin
  {
   * In case we're holding the last reference to the file, the dvp
   * will be re-acquired.
   }
  vhold(dvp);
  VOP_UNLOCK(dvp, 0);
  fdrop(fp);

  { Re-aquire the lock afterwards. }
  vn_lock(dvp, LK_RETRY or LK_EXCLUSIVE);
  vdrop(dvp);
  fvp:=dvp;
 end else
 begin
  {
   * Unlock our root node (dvp) when doing this, since we might
   * deadlock since the vnode might be locked by another thread
   * and the root vnode lock will be obtained afterwards (in case
   * we're looking up the fd of the root vnode), which will be the
   * opposite lock order. Vhold the root vnode first so we don't
   * lose it.
   }
  vhold(dvp);
  VOP_UNLOCK(dvp, 0);
  error:=fdesc_allocvp(_Fdesc, fd, FD_DESC + fd, dvp^.v_mount, @fvp);
  fdrop(fp);
  {
   * The root vnode must be locked last to prevent deadlock condition.
   }
  vn_lock(dvp, LK_RETRY or LK_EXCLUSIVE);
  vdrop(dvp);
 end;

 if (error<>0) then
  goto bad;
 vpp^:=fvp;
 Exit(0);

bad:
 vpp^:=nil;
 Exit(error);
end;

function fdesc_open(ap:p_vop_open_args):Integer;
var
 vp:p_vnode;
 td:p_kthread;
begin
 vp:=ap^.a_vp;

 if (VTOFDESC(vp)^.fd_type=_Froot) then
  Exit(0);

 {
  * XXX Kludge: set td^.td_proc^.p_dupfd to contain the value of the file
  * descriptor being sought for duplication. The error Exitensures
  * that the vnode for this device will be released by vn_open. Open
  * will detect this special error and take the actions in dupfdopen.
  * Other callers of vn_open or VOP_OPEN will simply report the
  * error.
  }
 td:=curkthread;
 if (td<>nil) then
 begin
  td^.td_dupfd:=VTOFDESC(vp)^.fd_fd; { XXX }
 end;

 Exit(ENODEV);
end;

function fdesc_getattr(ap:p_vop_getattr_args):Integer;
var
 vp:p_vnode;
 vap:p_vattr;
begin
 vp:=ap^.a_vp;
 vap:=ap^.a_vap;

 vap^.va_mode     :=S_IRUSR or S_IXUSR or S_IRGRP or S_IXGRP or S_IROTH or S_IXOTH;
 vap^.va_fileid   :=VTOFDESC(vp)^.fd_ix;
 vap^.va_uid      :=0;
 vap^.va_gid      :=0;
 vap^.va_blocksize:=DEV_BSIZE;
 vap^.va_atime.tv_sec :=boottime.tv_sec;
 vap^.va_atime.tv_nsec:=0;
 vap^.va_mtime    :=vap^.va_atime;
 vap^.va_ctime    :=vap^.va_mtime;
 vap^.va_gen      :=0;
 vap^.va_flags    :=0;
 vap^.va_bytes    :=0;
 vap^.va_filerev  :=0;

 case VTOFDESC(vp)^.fd_type of
  _Froot:
   begin
    vap^.va_type :=VDIR;
    vap^.va_nlink:=2;
    vap^.va_size :=DEV_BSIZE;
    vap^.va_rdev :=NODEV;
   end;
  _Fdesc:
   begin
    vap^.va_type :=VCHR;
    vap^.va_nlink:=1;
    vap^.va_size :=0;
    vap^.va_rdev :=makedev(0, vap^.va_fileid);
   end;
  else
   Assert(False,'fdesc_getattr');
 end;

 vp^.v_type:=vap^.va_type;
 Exit(0);
end;

function fdesc_setattr(ap:p_vop_setattr_args):Integer;
var
 vap:p_vattr;
 vp:p_vnode;
 mp:p_mount;
 fp:p_file;
 fd:Integer;
 error:Integer;
begin
 vap:=ap^.a_vap;

 {
  * Can't mess with the root vnode
  }
 if (VTOFDESC(ap^.a_vp)^.fd_type=_Froot) then
  Exit(EACCES);

 fd:=VTOFDESC(ap^.a_vp)^.fd_fd;

 {
  * Allow setattr where there is an underlying vnode.
  }
 error:=getvnode(fd, CAP_EXTATTR_SET, @fp);
 if (error<>0) then
 begin
  {
   * getvnode() Exits EINVAL if the file descriptor is not
   * backed by a vnode.  Silently drop all changes except
   * chflags(2) in this case.
   }
  if (error=EINVAL) then
  begin
   if (vap^.va_flags<>VNOVAL) then
    error:=EOPNOTSUPP
   else
    error:=0;
  end;
  Exit(error);
 end;
 vp:=fp^.f_vnode;

 error:=vn_start_write(vp, @mp, V_WAIT or PCATCH);
 if (error=0) then
 begin
  vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
  error:=VOP_SETATTR(vp, ap^.a_vap);
  VOP_UNLOCK(vp, 0);
  vn_finished_write(mp);
 end;
 fdrop(fp);
 Exit(error);
end;

const
 UIO_MX=16;

function fdesc_readdir(ap:p_vop_readdir_args):Integer;
label
 _break,
 done;
var
 uio:p_uio;
 d:t_dirent;
 dp:p_dirent;
 error,i,off,fcnt:Integer;
 tmp:p_file;
begin
 uio:=ap^.a_uio;
 dp:=@d;

 if (VTOFDESC(ap^.a_vp)^.fd_type<>_Froot) then
  Assert(False,'fdesc_readdir: not dir');

 if (ap^.a_ncookies<>nil) then
  ap^.a_ncookies^:=0;

 off:=uio^.uio_offset;

 if (off<>uio^.uio_offset) or
    (off < 0) or
    ((off mod UIO_MX)<>0) or
    (uio^.uio_resid < UIO_MX) then
  Exit(EINVAL);
 i:=off div UIO_MX;

 error:=0;

 fcnt:=i - 2;  { The first two nodes are `.' and `..' }

 FILEDESC_SLOCK(@fd_table);

 while (i < fd_table.fd_nfiles + 2) and (uio^.uio_resid >= UIO_MX) do
 begin
  FillChar(dp^,UIO_MX,0);
  case i of
   0, { `.' }
   1: { `..' }
    begin
     dp^.d_fileno:=i + FD_ROOT;
     dp^.d_namlen:=i + 1;
     dp^.d_reclen:=UIO_MX;
     Move(PChar('..')^, dp^.d_name, dp^.d_namlen);
     dp^.d_name[i + 1]:=#0;
     dp^.d_type:=DT_DIR;
    end;
   else
    begin
     tmp:=fget_unlocked(fcnt); //check exists
     if (tmp=nil) then
      goto _break;
     fdrop(tmp);

     dp^.d_name  :=IntToStr(fcnt);
     dp^.d_namlen:=strlen(dp^.d_name);
     dp^.d_reclen:=UIO_MX;
     dp^.d_type  :=DT_CHR;
     dp^.d_fileno:=i + FD_DESC;
    end;
  end;

  _break:
  if (dp^.d_namlen<>0) then
  begin
   {
    * And ship to userland
    }
   FILEDESC_SUNLOCK(@fd_table);
   error:=uiomove(dp, UIO_MX, uio);
   if (error<>0) then
    goto done;
   FILEDESC_SLOCK(@fd_table);
  end;
  Inc(i);
  Inc(fcnt);
 end;
 FILEDESC_SUNLOCK(@fd_table);

done:
 uio^.uio_offset:=i * UIO_MX;
 Exit(error);
end;

function fdesc_reclaim(ap:p_vop_reclaim_args):Integer;
var
 vp:p_vnode;
 fd:p_fdescnode;
begin
 vp:=ap^.a_vp;
 fd:=VTOFDESC(vp);
 fdesc_remove_entry(fd);
 FreeMem(vp^.v_data);
 vp^.v_data:=nil;
 Exit(0);
end;


end.

