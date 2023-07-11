unit vfs_cache;

{$mode ObjFPC}{$H+}

interface

uses
 vnode;

function vn_fullpath1(vp,rdir:p_vnode;buf:PChar;retbuf:PPChar;buflen:DWORD):Integer;
function vn_fullpath(vn:p_vnode;retbuf,freebuf:PPChar):Integer;
function vn_fullpath_global(vn:p_vnode;retbuf,freebuf:PPChar):Integer;
function vn_path_to_global_path(vp:p_vnode;path:PChar;pathlen:DWORD):Integer;
function vn_vptocnp(vp:pp_vnode;buf:PChar;buflen:PDWORD):Integer;

function sys___getcwd(buf:PChar;buflen:DWORD):Integer;

implementation

uses
 sysutils,
 systm,
 errno,
 vuio,
 vmount,
 vnamei,
 vfiledesc,
 vfs_subr,
 vfs_vnops,
 vfs_lookup,
 vnode_if,
 kern_thr,
 kern_mtx;

{
 * Retrieve the full filesystem path that correspond to a vnode from the name
 * cache (if available)
 }
function vn_fullpath(vn:p_vnode;retbuf,freebuf:PPChar):Integer;
var
 buf:PChar;
 rdir:p_vnode;
 error,vfslocked:Integer;
begin
 //if (disablefullpath) then Exit(ENODEV);

 if (vn=nil) then
 begin
  Exit(EINVAL);
 end;

 buf:=AllocMem(MAXPATHLEN);

 FILEDESC_SLOCK(@fd_table);
 rdir:=fd_table.fd_rdir;
 VREF(rdir);
 FILEDESC_SUNLOCK(@fd_table);

 error:=vn_fullpath1(vn, rdir, buf, retbuf, MAXPATHLEN);
 vfslocked:=VFS_LOCK_GIANT(rdir^.v_mount);
 vrele(rdir);
 VFS_UNLOCK_GIANT(vfslocked);

 if (error=0) then
  freebuf^:=buf
 else
  FreeMem(buf);

 Exit(error);
end;

{
 * This function is similar to vn_fullpath, but it attempts to lookup the
 * pathname relative to the global root mount point.  This is required for the
 * auditing sub-system, as audited pathnames must be absolute, relative to the
 * global root mount point.
 }
function vn_fullpath_global(vn:p_vnode;retbuf,freebuf:PPChar):Integer;
var
 buf:PChar;
 error:Integer;
begin
 //if (disablefullpath) then Exit(ENODEV);

 if (vn=nil) then
  Exit(EINVAL);

 buf:=AllocMem(MAXPATHLEN);

 error:=vn_fullpath1(vn, rootvnode, buf, retbuf, MAXPATHLEN);

 if (error=0) then
  freebuf^:=buf
 else
  FreeMem(buf);

 Exit(error);
end;



{
 * This function updates path string to vnode's full global path
 * and checks the size of the new path string against the pathlen argument.
 *
 * Requires a locked, referenced vnode and GIANT lock held.
 * Vnode is re-locked on success or ENODEV, otherwise unlocked.
 *
 * If sysctl debug.disablefullpath is set, ENODEV is returned,
 * vnode is left locked and path remain untouched.
 *
 * If vp is a directory, the call to vn_fullpath_global() always succeeds
 * because it falls back to the ".." lookup if the namecache lookup fails.
 }
function vn_path_to_global_path(vp:p_vnode;path:PChar;pathlen:DWORD):Integer;
label
 _out;
var
 nd:t_nameidata;
 vp1:p_vnode;
 rpath,fbuf:PChar;
 error,vfslocked:Integer;
begin
 VFS_ASSERT_GIANT(vp^.v_mount);
 ASSERT_VOP_ELOCKED(vp, 'vn_path_to_global_path');

 { Return ENODEV if sysctl debug.disablefullpath==1 }
 //if (disablefullpath) then Exit(ENODEV);

 { Construct global filesystem path from vp. }
 VOP_UNLOCK(vp, 0);

 error:=vn_fullpath_global(vp, @rpath, @fbuf);
 if (error<>0) then
 begin
  vrele(vp);
  Exit(error);
 end;

 if (strlen(rpath) >= pathlen) then
 begin
  vrele(vp);
  error:=ENAMETOOLONG;
  goto _out;
 end;

 {
  * Re-lookup the vnode by path to detect a possible rename.
  * As a side effect, the vnode is relocked.
  * If vnode was renamed, return ENOENT.
  }
 NDINIT(@nd, LOOKUP, FOLLOW or LOCKLEAF or MPSAFE or AUDITVNODE1, UIO_SYSSPACE, path, curkthread);

 error:=nd_namei(@nd);
 if (error<>0) then
 begin
  vrele(vp);
  goto _out;
 end;

 vfslocked:=NDHASGIANT(@nd);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vp1:=nd.ni_vp;
 vrele(vp);
 if (vp1=vp) then
 begin
  strcopy(path, rpath);
 end else
 begin
  vput(vp1);
  error:=ENOENT;
 end;
 VFS_UNLOCK_GIANT(vfslocked);

_out:
 FreeMem(fbuf);
 Exit(error);
end;

function vn_vptocnp(vp:pp_vnode;buf:PChar;buflen:PDWORD):Integer;
var
 dvp:p_vnode;
 error,vfslocked:Integer;
begin
 //no cache

 vfslocked:=VFS_LOCK_GIANT((vp^)^.v_mount);
 vn_lock(vp^, LK_SHARED or LK_RETRY);
 error:=VOP_VPTOCNP(vp^, @dvp, buf, PInteger(buflen));
 vput(vp^);
 VFS_UNLOCK_GIANT(vfslocked);

 if (error<>0) then
 begin
  Exit(error);
 end;

 vp^:=dvp;

 if ((dvp^.v_iflag and VI_DOOMED)<>0) then
 begin
  { forced unmount }
  vfslocked:=VFS_LOCK_GIANT(dvp^.v_mount);
  vrele(dvp);
  VFS_UNLOCK_GIANT(vfslocked);
  error:=ENOENT;
  Exit(error);
 end;
 {
  * *vp has its use count incremented still.
  }

 Exit(0);
end;

{
 * The magic behind kern___getcwd() and vn_fullpath().
 }
function vn_fullpath1(vp,rdir:p_vnode;buf:PChar;retbuf:PPChar;buflen:DWORD):Integer;
var
 error,slash_prefixed,vfslocked:Integer;
 vp1:p_vnode;
begin
 Dec(buflen);
 buf[buflen]:=#0;
 error:=0;
 slash_prefixed:=0;

 vref(vp);

 if (vp^.v_type<>VDIR) then
 begin
  error:=vn_vptocnp(@vp, buf, @buflen);
  if (error<>0) then
   Exit(error);

  if (buflen=0) then
  begin
   vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
   vrele(vp);
   VFS_UNLOCK_GIANT(vfslocked);
   Exit(ENOMEM);
  end;
  Dec(buflen);
  buf[buflen]:='/';
  slash_prefixed:=1;
 end;
 while (vp<>rdir) and (vp<>rootvnode) do
 begin
  if ((vp^.v_vflag and VV_ROOT)<>0) then
  begin
   if ((vp^.v_iflag and VI_DOOMED)<>0) then
   begin { forced unmount }
    vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
    vrele(vp);
    VFS_UNLOCK_GIANT(vfslocked);
    error:=ENOENT;
    break;
   end;
   vp1:=p_mount(vp^.v_mount)^.mnt_vnodecovered;
   vref(vp1);
   vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
   vrele(vp);
   VFS_UNLOCK_GIANT(vfslocked);
   vp:=vp1;
   continue;
  end;
  if (vp^.v_type<>VDIR) then
  begin
   vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
   vrele(vp);
   VFS_UNLOCK_GIANT(vfslocked);
   error:=ENOTDIR;
   break;
  end;
  error:=vn_vptocnp(@vp, buf, @buflen);
  if (error<>0) then
   break;

  if (buflen=0) then
  begin
   vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
   vrele(vp);
   VFS_UNLOCK_GIANT(vfslocked);
   error:=ENOMEM;
   break;
  end;
  Dec(buflen);
  buf[buflen]:='/';
  slash_prefixed:=1;
 end;
 if (error<>0) then
  Exit(error);

 if (slash_prefixed=0) then
 begin
  if (buflen=0) then
  begin
   vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
   vrele(vp);
   VFS_UNLOCK_GIANT(vfslocked);
   Exit(ENOMEM);
  end;
  Dec(buflen);
  buf[buflen]:='/';
 end;
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 vrele(vp);
 VFS_UNLOCK_GIANT(vfslocked);

 retbuf^:=buf + buflen;
 Exit(0);
end;

function kern___getcwd(buf:PChar;bufseg:uio_seg;buflen:DWORD):Integer;
var
 bp,tmpbuf:PChar;
 cdir,rdir:p_vnode;
 error,vfslocked:Integer;
begin
 //if (disablecwd) then Exit(ENODEV);

 if (buflen < 2) then Exit(EINVAL);

 if (buflen > MAXPATHLEN) then
  buflen:=MAXPATHLEN;

 tmpbuf:=AllocMem(buflen);

 FILEDESC_SLOCK(@fd_table);
 cdir:=fd_table.fd_cdir;
 VREF(cdir);
 rdir:=fd_table.fd_rdir;
 VREF(rdir);
 FILEDESC_SUNLOCK(@fd_table);

 error:=vn_fullpath1(cdir, rdir, tmpbuf, @bp, buflen);
 vfslocked:=VFS_LOCK_GIANT(rdir^.v_mount);
 vrele(rdir);
 VFS_UNLOCK_GIANT(vfslocked);
 vfslocked:=VFS_LOCK_GIANT(cdir^.v_mount);
 vrele(cdir);
 VFS_UNLOCK_GIANT(vfslocked);

 if (error=0) then
 begin
  if (bufseg=UIO_SYSSPACE) then
   Move(bp^, buf^, strlen(bp) + 1)
  else
   error:=copyout(bp, buf, strlen(bp) + 1);
 end;

 FreeMem(tmpbuf);

 Exit(error);
end;

{ Implementation of the getcwd syscall. }
function sys___getcwd(buf:PChar;buflen:DWORD):Integer;
begin
 Exit(kern___getcwd(buf, UIO_USERSPACE, buflen));
end;

end.

