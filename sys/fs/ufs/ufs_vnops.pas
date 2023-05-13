unit ufs_vnops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 time,
 vfile,
 vfilio,
 vttycom,
 vmount,
 vdirent,
 vstat,
 vuio,
 vfcntl,
 vnode,
 vnamei,
 vnode_if,
 vfs_default,
 vfs_vnops,
 ufs,
 kern_mtx,
 kern_sx,
 kern_conf;

function ufs_vmkdir(dmp:p_ufs_mount;name:PChar;namelen:Integer;dotdot:p_ufs_dirent;inode:DWORD):p_ufs_dirent;

function ufs_lookup(ap:p_vop_lookup_args):Integer;
function ufs_access(ap:p_vop_access_args):Integer;
function ufs_getattr(ap:p_vop_getattr_args):Integer;
function ufs_readdir(ap:p_vop_readdir_args):Integer;
function ufs_readlink(ap:p_vop_readlink_args):Integer;

const
 ufs_vnodeops:vop_vector=(
  vop_default       :@default_vnodeops;
  vop_bypass        :nil;

  vop_islocked      :nil;
  vop_lookup        :@ufs_lookup;
  vop_create        :nil;
  vop_whiteout      :nil;
  vop_mknod         :nil;//@devfs_mknod;
  vop_open          :nil;
  vop_close         :nil;
  vop_access        :@ufs_access;
  vop_accessx       :nil;
  vop_getattr       :@ufs_getattr;
  vop_setattr       :nil;//@devfs_setattr;
  vop_markatime     :nil;
  vop_read          :nil;//@devfs_rread;
  vop_write         :nil;
  vop_ioctl         :nil;//@devfs_rioctl;
  vop_poll          :nil;
  vop_kqfilter      :nil;
  vop_revoke        :nil;//@devfs_revoke;
  vop_fsync         :nil;
  vop_remove        :nil;//@devfs_remove;
  vop_link          :nil;
  vop_rename        :nil;
  vop_mkdir         :nil;
  vop_rmdir         :nil;
  vop_symlink       :nil;//@devfs_symlink;
  vop_readdir       :@ufs_readdir;
  vop_readlink      :@ufs_readlink;
  vop_inactive      :nil;
  vop_reclaim       :nil;//@devfs_reclaim;
  vop_lock1         :nil;
  vop_unlock        :nil;
  vop_bmap          :nil;
  vop_strategy      :nil;
  vop_getwritemount :nil;
  vop_print         :nil;
  vop_pathconf      :nil;//@devfs_pathconf;
  vop_advlock       :nil;
  vop_advlockasync  :nil;
  vop_advlockpurge  :nil;
  vop_reallocblks   :nil;
  vop_getpages      :nil;
  vop_putpages      :nil;
  vop_getacl        :nil;
  vop_setacl        :nil;
  vop_aclcheck      :nil;
  vop_closeextattr  :nil;
  vop_getextattr    :nil;
  vop_listextattr   :nil;
  vop_openextattr   :nil;
  vop_deleteextattr :nil;
  vop_setextattr    :nil;
  vop_setlabel      :nil;//@devfs_setlabel;
  vop_vptofh        :nil;
  vop_vptocnp       :nil;//@devfs_vptocnp;
  vop_allocate      :nil;
  vop_unp_bind      :nil;
  vop_unp_connect   :nil;
  vop_unp_detach    :nil;
 );

implementation

uses
 sysutils,
 errno,
 systm,
 kern_thr,
 vfs_subr,
 vsys_generic,
 kern_descrip,
 kern_mtxpool,
 subr_uio;

function VFSTOUFS(mp:p_mount):p_ufs_mount; inline;
begin
 Result:=mp^.mnt_data;
end;

function ufs_newdirent(name:PChar;namelen:Integer):p_ufs_dirent;
var
 i:Integer;
 de:p_ufs_dirent;
 d:t_dirent;
begin
 d.d_namlen:=namelen;
 i:=sizeof(t_ufs_dirent) + GENERIC_DIRSIZ(@d);
 de:=AllocMem(i);
 de^.ufs_dirent:=p_dirent(de + 1);
 de^.ufs_dirent^.d_namlen:=namelen;
 de^.ufs_dirent^.d_reclen:=GENERIC_DIRSIZ(@d);

 Move(name^, de^.ufs_dirent^.d_name, namelen);

 de^.ufs_dirent^.d_name[namelen]:=#0;

 vfs_timestamp(@de^.ufs_ctime);
 de^.ufs_mtime  :=de^.ufs_ctime;
 de^.ufs_atime  :=de^.ufs_ctime;
 de^.ufs_links  :=1;
 de^.ufs_ref    :=1;

 Exit(de);
end;

function ufs_vmkdir(dmp:p_ufs_mount;name:PChar;namelen:Integer;dotdot:p_ufs_dirent;inode:DWORD):p_ufs_dirent;
var
 nd,de:p_ufs_dirent;
begin
 { Create the new directory }
 nd:=ufs_newdirent(name, namelen);

 TAILQ_INIT(@nd^.ufs_dlist);

 nd^.ufs_dirent^.d_type:=DT_DIR;
 nd^.ufs_mode :=&0555;
 nd^.ufs_links:=2;
 nd^.ufs_dir  :=nd;

 if (inode<>0) then
  nd^.ufs_inode:=inode
 else
  nd^.ufs_inode:=ufs_alloc_cdp_inode;

 {
  * '.' and '..' are always the two first entries in the
  * de_dlist list.
  *
  * Create the '.' entry in the new directory.
  }
 de:=ufs_newdirent('.', 1);
 de^.ufs_dirent^.d_type:=DT_DIR;
 de^.ufs_flags:=de^.ufs_flags or DE_DOT;
 TAILQ_INSERT_TAIL(@nd^.ufs_dlist,de,@de^.ufs_list);
 de^.ufs_dir:=nd;

 { Create the '..' entry in the new directory. }
 de:=ufs_newdirent('..', 2);
 de^.ufs_dirent^.d_type:=DT_DIR;
 de^.ufs_flags:=de^.ufs_flags or DE_DOTDOT;
 TAILQ_INSERT_TAIL(@nd^.ufs_dlist,de,@de^.ufs_list);

 if (dotdot=nil) then
 begin
  de^.ufs_dir:=nd;
 end else
 begin
  de^.ufs_dir:=dotdot;
  sx_assert(@dmp^.ufs_lock);
  TAILQ_INSERT_TAIL(@dotdot^.ufs_dlist,nd,@nd^.ufs_list);
  Inc(dotdot^.ufs_links);
 end;

 Exit(nd);
end;

function ufs_parent_dirent(de:p_ufs_dirent):p_ufs_dirent;
begin
 if (de^.ufs_dirent^.d_type<>DT_DIR) then
  Exit(de^.ufs_dir);

 if ((de^.ufs_flags and (DE_DOT or DE_DOTDOT))<>0) then
  Exit(nil);

 de:=TAILQ_FIRST(@de^.ufs_dlist); { '.' }
 if (de=nil) then
  Exit(nil);

 de:=TAILQ_NEXT(de,@de^.ufs_list);  { '..' }
 if (de=nil) then
  Exit(nil);

 Exit(de^.ufs_dir);
end;

function ufs_find(dd:p_ufs_dirent;name:PChar;namelen:Integer;_type:Integer):p_ufs_dirent;
var
 de:p_ufs_dirent;
begin
 de:=TAILQ_FIRST(@dd^.ufs_dlist);
 while (de<>nil) do
 begin
  if (namelen<>de^.ufs_dirent^.d_namlen) then
  begin
   de:=TAILQ_NEXT(de,@de^.ufs_list);
   continue;
  end;
  if (_type<>0) and (_type<>de^.ufs_dirent^.d_type) then
  begin
   de:=TAILQ_NEXT(de,@de^.ufs_list);
   continue;
  end;

  if (CompareByte(name^, de^.ufs_dirent^.d_name, namelen)<>0) then
  begin
   de:=TAILQ_NEXT(de,@de^.ufs_list);
   continue;
  end;
  break;
 end;

 Exit(de);
end;

function ufs_fqpn(buf:PChar;dmp:p_ufs_mount;dd:p_ufs_dirent;cnp:p_componentname):PChar;
var
 i:Integer;
 de:p_ufs_dirent;
begin
 sx_assert(@dmp^.ufs_lock);

 i:=SPECNAMELEN;
 buf[i]:=#0;
 if (cnp<>nil) then
  Dec(i,cnp^.cn_namelen);
 if (i < 0) then
   Exit(nil);
 if (cnp<>nil) then
  Move(cnp^.cn_nameptr^, (buf + i)^, cnp^.cn_namelen);
 de:=dd;
 while (de<>dmp^.ufs_rootdir) do
 begin
  if (cnp<>nil) or (i < SPECNAMELEN) then
  begin
   Dec(i);
   if (i < 0) then
     Exit(nil);
   buf[i]:='/';
  end;
  Dec(i,de^.ufs_dirent^.d_namlen);
  if (i < 0) then
    Exit(nil);

  Move(de^.ufs_dirent^.d_name, (buf + i)^, de^.ufs_dirent^.d_namlen);

  de:=ufs_parent_dirent(de);
  if (de=nil) then
   Exit(nil);
 end;
 Exit(buf + i);
end;

function ufs_lookupx(ap:p_vop_lookup_args;dm_unlock:PInteger):Integer;
label
 _or;
var
 td:p_kthread;
 cnp:p_componentname;
 dvp:p_vnode;
 vpp:pp_vnode;
 de,dd:p_ufs_dirent;
 dde:p_ufs_dirent;
 dmp:p_ufs_mount;
 error,flags,nameiop,dvplocked:Integer;
 specname:array[0..SPECNAMELEN] of Char;
 pname:PChar;
begin
 cnp:=ap^.a_cnp;
 vpp:=ap^.a_vpp;
 dvp:=ap^.a_dvp;
 pname:=cnp^.cn_nameptr;
 td:=cnp^.cn_thread;
 flags:=cnp^.cn_flags;
 nameiop:=cnp^.cn_nameiop;
 dmp:=VFSTOUFS(dvp^.v_mount);
 dd:=dvp^.v_data;
 vpp^:=nil;

 dm_unlock^:=0;

 //if (p_mount(dvp^.v_mount)^.mnt_flag and MNT_ROOTFS)<>0 then
 // Writeln(' MNT_ROOTFS')
 //else
 // Writeln('!MNT_ROOTFS');

 if ((flags and ISLASTCN)<>0) and (nameiop=RENAME) then
  Exit(EOPNOTSUPP);

 if (dvp^.v_type<>VDIR) then
  Exit(ENOTDIR);

 if (((flags and ISDOTDOT)<>0) and ((dvp^.v_vflag and VV_ROOT)<>0)) then
  Exit(EIO);

 //error:=VOP_ACCESS(dvp, VEXEC);
 //if (error<>0) then
 // Exit(error);

 if (cnp^.cn_namelen=1) and (pname^='.') then
 begin
  if ((flags and ISLASTCN) and nameiop<>LOOKUP) then
   Exit(EINVAL);

  vpp^:=dvp;
  VREF(dvp);

  Exit(0);
 end;

 dm_unlock^:=1;
 sx_xlock(@dmp^.ufs_lock);

 if ((flags and ISDOTDOT)<>0) then
 begin
  if ((flags and ISLASTCN)<>0) and (nameiop<>LOOKUP) then
   Exit(EINVAL);

  de:=ufs_parent_dirent(dd);

  if (de=nil) then
   Exit(ENOENT);

  dvplocked:=VOP_ISLOCKED(dvp);
  VOP_UNLOCK(dvp, 0);
  error:=ufs_allocv(de, dvp^.v_mount, cnp^.cn_lkflags and LK_TYPE_MASK, vpp);

  vn_lock(dvp, dvplocked or LK_RETRY);

  dm_unlock^:=0;
  Exit(error);
 end;

 dd:=dvp^.v_data;
 de:=ufs_find(dd, cnp^.cn_nameptr, cnp^.cn_namelen, 0);
 while (de=nil) do
 begin { While(...) so we can use break }

  if (nameiop=DELETE) then
   Exit(ENOENT);

  {
   * OK, we didn't have an entry for the name we were asked for
   * so we try to see if anybody can create it on demand.
   }
  pname:=ufs_fqpn(specname, dmp, dd, cnp);
  if (pname=nil) then
   break;

  //ufs_mp_hold(dmp);
  //sx_xunlock(@dmp^.ufs_lock);

  {
  if (cdev=nil) then
   sx_xlock(@dmp^.dm_lock)
  else
  if (devfs_populate_vp(dvp)<>0) then
  begin
   dm_unlock^:=0;
   sx_xlock(@dmp^.dm_lock);
   if (DEVFS_DMP_DROP(dmp)) then
   begin
    sx_xunlock(@dmp^.dm_lock);
    devfs_unmount_final(dmp);
   end else
    sx_xunlock(@dmp^.dm_lock);
   dev_rel(cdev);
   Exit(ENOENT);
  end;
  }

  //sx_xlock(@dmp^.ufs_lock);

  //if ufs_mp_drop(dmp) then
  //begin
  // //dm_unlock^:=0;
  // Exit(ENOENT);
  //end;

  //if (dde<>nil) then
  //if (dde^<>nil) then
  //begin
  // de:=dde^;
  //end;

  break;
 end;

 if (de=nil) then goto _or;

 if ((de^.ufs_flags and DE_WHITEOUT)<>0) then
 begin
  _or:
  if ((nameiop=CREATE) or (nameiop=RENAME)) and
     ((flags and (LOCKPARENT or WANTPARENT))<>0) and
     ((flags and ISLASTCN)<>0) then
  begin
   cnp^.cn_flags:=cnp^.cn_flags or SAVENAME;
   Exit(EJUSTRETURN);
  end;
  Exit(ENOENT);
 end;

 if (cnp^.cn_nameiop=DELETE) and ((flags and ISLASTCN)<>0) then
 begin
  error:=VOP_ACCESS(dvp, VWRITE);
  if (error<>0) then
   Exit(error);

  if (vpp^=dvp) then
  begin
   VREF(dvp);
   vpp^:=dvp;
   Exit(0);
  end;
 end;

 error:=ufs_allocv(de, dvp^.v_mount, cnp^.cn_lkflags and LK_TYPE_MASK, vpp);

 dm_unlock^:=0;
 Exit(error);
end;

function ufs_lookup(ap:p_vop_lookup_args):Integer;
var
 j:Integer;
 dmp:p_ufs_mount;
 dm_unlock:Integer;
begin
 dmp:=VFSTOUFS(ap^.a_dvp^.v_mount);
 dm_unlock:=1;

 j:=ufs_lookupx(ap, @dm_unlock);

 if (dm_unlock=1) then
 begin
  sx_xunlock(@dmp^.ufs_lock);
 end;
 Exit(j);
end;

function ufs_access(ap:p_vop_access_args):Integer;
var
 vp:p_vnode;
 de:p_ufs_dirent;
 error:Integer;
begin
 vp:=ap^.a_vp;

 de:=vp^.v_data;

 //if (vp^.v_type=VDIR) then
 // de:=de^.ufs_dir;

 error:=vaccess(vp^.v_type, de^.ufs_mode, de^.ufs_uid, de^.ufs_gid, ap^.a_accmode, nil);
 if (error=0) then
  Exit(0);
 if (error<>EACCES) then
  Exit(error);
 { We do, however, allow access to the controlling terminal }
 if ((p_leader.p_flag and P_CONTROLT)=0) then
  Exit(error);
 //if (ap^.a_td^.td_proc^.p_session^.s_ttydp=de^.de_cdp) then
 // Exit(0);
 Exit(error);
end;

function ufs_getattr(ap:p_vop_getattr_args):Integer;
var
 vp:p_vnode;
 vap:p_vattr;
 de:p_ufs_dirent;
 dmp:p_ufs_mount;

 procedure fix(var src,dst:timespec); inline;
 begin
  if (src.tv_sec <= 3600) then
  begin
   src.tv_sec :=boottime.tv_sec;
   src.tv_nsec:=boottime.tv_usec * 1000;
  end;
  dst:=src;
 end;

begin
 vp:=ap^.a_vp;

 vap:=ap^.a_vap;

 dmp:=VFSTOUFS(vp^.v_mount);

 de:=vp^.v_data;
 if (vp^.v_type=VDIR) then
 begin
  de:=de^.ufs_dir;
 end;

 vap^.va_uid :=de^.ufs_uid;
 vap^.va_gid :=de^.ufs_gid;
 vap^.va_mode:=de^.ufs_mode;

 if (vp^.v_type=VLNK) then
  vap^.va_size:=strlen(de^.ufs_symlink)
 else
 if (vp^.v_type=VDIR) then
 begin
  vap^.va_size :=DEV_BSIZE;
  vap^.va_bytes:=DEV_BSIZE;
 end else
  vap^.va_size:=0;

 if (vp^.v_type<>VDIR) then
  vap^.va_bytes:=0;

 vap^.va_blocksize:=DEV_BSIZE;
 vap^.va_type:=vp^.v_type;

 fix(de^.ufs_atime,vap^.va_atime);
 fix(de^.ufs_mtime,vap^.va_mtime);
 fix(de^.ufs_ctime,vap^.va_ctime);

 vap^.va_gen    :=0;
 vap^.va_flags  :=0;
 vap^.va_filerev:=0;
 vap^.va_nlink  :=de^.ufs_links;
 vap^.va_fileid :=de^.ufs_inode;

 Exit(0);
end;

function ufs_readdir(ap:p_vop_readdir_args):Integer;
var
 error:Integer;
 uio:p_uio;
 dp:p_dirent;
 dd:p_ufs_dirent;
 de:p_ufs_dirent;
 dmp:p_ufs_mount;
 mp:p_mount;
 off:Int64;
 tmp_ncookies:PInteger;
begin
 tmp_ncookies:=nil;

 if (ap^.a_vp^.v_type<>VDIR) then
  Exit(ENOTDIR);

 uio:=ap^.a_uio;
 if (uio^.uio_offset < 0) then
  Exit(EINVAL);

 {
  * XXX: This is a temporary hack to get around this filesystem not
  * supporting cookies. We store the location of the ncookies pointer
  * in a temporary variable before calling vfs_subr.c:vfs_read_dirent()
  * and set the number of cookies to 0. We then set the pointer to
  * nil so that vfs_read_dirent doesn't try to call realloc() on
  * ap^.a_cookies. Later in this function, we restore the ap^.a_ncookies
  * pointer to its original location before Exiting to the caller.
  }
 if (ap^.a_ncookies<>nil) then
 begin
  tmp_ncookies:=ap^.a_ncookies;
  ap^.a_ncookies^:=0;
  ap^.a_ncookies:=nil;
 end;

 mp:=ap^.a_vp^.v_mount;
 dmp:=VFSTOUFS(mp);

 sx_xlock(@dmp^.ufs_lock);

 error:=0;
 de:=ap^.a_vp^.v_data;
 off:=0;

 dd:=TAILQ_FIRST(@de^.ufs_dlist);
 while (dd<>nil) do
 begin

  if ((dd^.ufs_flags and (DE_COVERED or DE_WHITEOUT))<>0) then
  begin
   dd:=TAILQ_NEXT(dd,@dd^.ufs_list);
   continue;
  end;

  if (dd^.ufs_dirent^.d_type=DT_DIR) then
   de:=dd^.ufs_dir
  else
   de:=dd;
  dp:=dd^.ufs_dirent;

  if (dp^.d_reclen > uio^.uio_resid) then
   break;

  dp^.d_fileno:=de^.ufs_inode;

  if (off >= uio^.uio_offset) then
  begin
   error:=vfs_read_dirent(ap, dp, off);
   if (error<>0) then
    break;
  end;

  Inc(off,dp^.d_reclen);
  dd:=TAILQ_NEXT(dd,@dd^.ufs_list);
 end;

 sx_xunlock(@dmp^.ufs_lock);
 uio^.uio_offset:=off;

 {
  * Restore ap^.a_ncookies if it wasn't originally nil in the first
  * place.
  }
 if (tmp_ncookies<>nil) then
  ap^.a_ncookies:=tmp_ncookies;

 Exit(error);
end;

function ufs_readlink(ap:p_vop_readlink_args):Integer;
var
 de:p_ufs_dirent;
begin
 de:=ap^.a_vp^.v_data;
 Exit(uiomove(de^.ufs_symlink, strlen(de^.ufs_symlink), ap^.a_uio));
end;

end.

