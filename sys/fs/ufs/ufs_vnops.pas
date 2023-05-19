unit ufs_vnops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 time,
 vfile,
 vmount,
 vdirent,
 vuio,
 vnode,
 vnamei,
 vnode_if,
 vfs_default,
 vfs_vnops,
 ufs,
 kern_mtx,
 kern_sx;

function  ufs_vmkdir(dmp:p_ufs_mount;name:PChar;namelen:Integer;dotdot:p_ufs_dirent;inode:DWORD):p_ufs_dirent;
procedure ufs_purge(dm:p_ufs_mount;dd:p_ufs_dirent);

function  ufs_lookup(ap:p_vop_lookup_args):Integer;
function  ufs_access(ap:p_vop_access_args):Integer;
function  ufs_getattr(ap:p_vop_getattr_args):Integer;
function  ufs_setattr(ap:p_vop_setattr_args):Integer;
function  ufs_readdir(ap:p_vop_readdir_args):Integer;
function  ufs_rread(ap:p_vop_read_args):Integer;
function  ufs_readlink(ap:p_vop_readlink_args):Integer;
function  ufs_symlink(ap:p_vop_symlink_args):Integer;
function  ufs_remove(ap:p_vop_remove_args):Integer;
function  ufs_rmdir(ap:p_vop_rmdir_args):Integer;
function  ufs_mkdir(ap:p_vop_mkdir_args):Integer;
function  ufs_reclaim(ap:p_vop_reclaim_args):Integer;

const
 ufs_vnodeops_root:vop_vector=(
  vop_default       :@default_vnodeops;
  vop_bypass        :nil;

  vop_islocked      :nil;
  vop_lookup        :@ufs_lookup;
  vop_create        :nil;
  vop_whiteout      :nil;
  vop_mknod         :nil;
  vop_open          :nil;
  vop_close         :nil;
  vop_access        :@ufs_access;
  vop_accessx       :nil;
  vop_getattr       :@ufs_getattr;
  vop_setattr       :@ufs_setattr;
  vop_markatime     :nil;
  vop_read          :@ufs_rread;
  vop_write         :nil;
  vop_ioctl         :nil;
  vop_poll          :nil;
  vop_kqfilter      :nil;
  vop_revoke        :nil;
  vop_fsync         :nil;
  vop_remove        :@ufs_remove;
  vop_link          :nil;
  vop_rename        :nil;
  vop_mkdir         :@ufs_mkdir;
  vop_rmdir         :@ufs_rmdir;
  vop_symlink       :@ufs_symlink;
  vop_readdir       :@ufs_readdir;
  vop_readlink      :@ufs_readlink;
  vop_inactive      :nil;
  vop_reclaim       :@ufs_reclaim;
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
  vop_getacl        :nil;
  vop_setacl        :nil;
  vop_aclcheck      :nil;
  vop_closeextattr  :nil;
  vop_getextattr    :nil;
  vop_listextattr   :nil;
  vop_openextattr   :nil;
  vop_deleteextattr :nil;
  vop_setextattr    :nil;
  vop_vptofh        :nil;
  vop_vptocnp       :nil;//@ufs_vptocnp;
  vop_allocate      :nil;
  vop_unp_bind      :nil;
  vop_unp_connect   :nil;
  vop_unp_detach    :nil;
 );

implementation

uses
 sysutils,
 errno,
 kern_thr,
 vfs_subr,
 subr_uio;

const
 UFS_DEL_VNLOCKED =$01;
 UFS_DEL_NORECURSE=$02;

function VFSTOUFS(mp:p_mount):p_ufs_mount; inline;
begin
 Result:=mp^.mnt_data;
end;

function ufs_parent_dirent(de:p_ufs_dirent):p_ufs_dirent;
begin
 if (de^.ufs_dirent^.d_type<>DT_DIR) then
  Exit(de^.ufs_dir);

 if ((de^.ufs_flags and (UFS_DOT or UFS_DOTDOT))<>0) then
  Exit(nil);

 de:=TAILQ_FIRST(@de^.ufs_dlist); { '.' }
 if (de=nil) then Exit(de^.ufs_dir);

 de:=TAILQ_NEXT(de,@de^.ufs_list);  { '..' }
 if (de=nil) then Exit(de^.ufs_dir);

 Exit(de^.ufs_dir);
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
 de^.ufs_btime  :=de^.ufs_ctime;

 de^.ufs_links  :=1;
 de^.ufs_ref    :=1;

 TAILQ_INIT(@de^.ufs_dlist);

 Exit(de);
end;

function _ufs_rmdir(dm:p_ufs_mount;de:p_ufs_dirent):Integer; forward;

{
 * The caller needs to hold the dm for the duration of the call since
 * dm^.dm_lock may be temporary dropped.
 }
procedure ufs_delete(dm:p_ufs_mount;de:p_ufs_dirent;flags:Integer);
var
 dd:p_ufs_dirent;
 vp:p_vnode;
begin
 if (dm=nil) or (de=nil) then Exit;

 Assert((de^.ufs_flags and UFS_DOOMED)=0,'ufs_delete doomed dirent');
 de^.ufs_flags:=de^.ufs_flags or UFS_DOOMED;

 if ((flags and UFS_DEL_NORECURSE)=0) then
 begin
  dd:=ufs_parent_dirent(de);
  if (dd<>nil) then
   ufs_de_hold(dd);

  if (de^.ufs_flags and UFS_USER)<>0 then
  begin
   Assert(dd<>nil,'ufs_delete: nil dd');
  end;
 end else
  dd:=nil;

 mtx_lock(ufs_interlock);
 vp:=de^.ufs_vnode;
 if (vp<>nil) then
 begin
  VI_LOCK(vp);
  mtx_unlock(ufs_interlock);
  vholdl(vp);

  sx_unlock(@dm^.ufs_lock);

  if ((flags and UFS_DEL_VNLOCKED)=0) then
   vn_lock(vp, LK_EXCLUSIVE or LK_INTERLOCK or LK_RETRY)
  else
   VI_UNLOCK(vp);

  vgone(vp);

  if ((flags and UFS_DEL_VNLOCKED)=0) then
   VOP_UNLOCK(vp, 0);

  vdrop(vp);
  sx_xlock(@dm^.ufs_lock);
 end else
  mtx_unlock(ufs_interlock);

 if (de^.ufs_symlink<>nil) then
 begin
  FreeMem(de^.ufs_symlink);
  de^.ufs_symlink:=nil;
 end;

 if (de^.ufs_inode > UFS_ROOTINO) then
 begin
  ufs_free_cdp_inode(de^.ufs_inode);
  de^.ufs_inode:=0;
 end;

 if (dd<>nil) then
 begin
  TAILQ_REMOVE(@dd^.ufs_dlist,de,@de^.ufs_list);
  if ufs_de_drop(dd) then
  begin
   //
  end else
  if ((flags and UFS_DEL_NORECURSE)=0) then
  begin
   _ufs_rmdir(dm, de);
  end;
 end;

 ufs_de_drop(de);
end;

function _ufs_dir_status(dm:p_ufs_mount;de:p_ufs_dirent):Integer;
var
 de_dot,de_dotdot:p_ufs_dirent;
begin
 Result:=0;
 if (dm=nil) or (de=nil) then Exit;

 sx_assert(@dm^.ufs_lock);

 if (de^.ufs_dirent^.d_type<>DT_DIR) then Exit(ENOTDIR);

 if ((de^.ufs_flags and UFS_DOOMED)<>0) or (de=dm^.ufs_rootdir) then
  Exit(EBUSY);

 de_dot:=TAILQ_FIRST(@de^.ufs_dlist);
 Assert(de_dot<>nil, 'ufs_rmdir: . missing');

 de_dotdot:=TAILQ_NEXT(de_dot,@de_dot^.ufs_list);
 Assert(de_dotdot<>nil, 'ufs_rmdir: .. missing');

 { Exit if the directory is not empty. }
 if (TAILQ_NEXT(de_dotdot,@de_dotdot^.ufs_list)<>nil) then
  Exit(ENOTEMPTY);
end;

function _ufs_rmdir(dm:p_ufs_mount;de:p_ufs_dirent):Integer;
label
 next;
var
 dd,de_dot,de_dotdot:p_ufs_dirent;
begin
 Result:=0;
 if (dm=nil) or (de=nil) then Exit;

 sx_assert(@dm^.ufs_lock);

 Assert(de^.ufs_dirent^.d_type=DT_DIR,'ufs_rmdir: de is not a directory');

 if ((de^.ufs_flags and UFS_DOOMED)<>0) or (de=dm^.ufs_rootdir) then
  Exit(EBUSY);

 dd       :=nil;
 de_dot   :=nil;
 de_dotdot:=nil;

 de_dot:=TAILQ_FIRST(@de^.ufs_dlist);
 if (de_dot=nil) then
 begin
  goto next;
 end;

 de_dotdot:=TAILQ_NEXT(de_dot,@de_dot^.ufs_list);
 if (de_dotdot=nil) then
 begin
  goto next;
 end;

 { Exit if the directory is not empty. }
 if (TAILQ_NEXT(de_dotdot,@de_dotdot^.ufs_list)<>nil) then
  Exit(ENOTEMPTY);

 dd:=ufs_parent_dirent(de);

 next:

 ufs_de_hold(dd);
 ufs_delete(dm, de       ,UFS_DEL_NORECURSE);
 ufs_delete(dm, de_dot   ,UFS_DEL_NORECURSE);
 ufs_delete(dm, de_dotdot,UFS_DEL_NORECURSE);
 ufs_de_drop(dd);
end;

procedure ufs_purge(dm:p_ufs_mount;dd:p_ufs_dirent);
var
 de:p_ufs_dirent;
begin
 sx_assert(@dm^.ufs_lock);

 ufs_de_hold(dd);

 repeat
  de:=TAILQ_LAST(@dd^.ufs_dlist);
  if (de=nil) then break;

  if ((de^.ufs_flags and (UFS_DOT or UFS_DOTDOT))<>0) then
   ufs_delete(dm, de, UFS_DEL_NORECURSE)
  else
  if (de^.ufs_dirent^.d_type=DT_DIR) then
   ufs_purge(dm, de)
  else
   ufs_delete(dm, de, UFS_DEL_NORECURSE);

 until false;

 if ((dd^.ufs_flags and UFS_DOOMED)=0) then
  ufs_delete(dm, dd, UFS_DEL_NORECURSE);

 ufs_de_drop(dd);
end;

function ufs_vmkdir(dmp:p_ufs_mount;name:PChar;namelen:Integer;dotdot:p_ufs_dirent;inode:DWORD):p_ufs_dirent;
var
 nd,de:p_ufs_dirent;
 mp:p_mount;
begin
 { Create the new directory }
 nd:=ufs_newdirent(name, namelen);

 nd^.ufs_dirent^.d_type:=DT_DIR;
 nd^.ufs_mode :=UFS_DEFAULT_MODE;
 nd^.ufs_links:=2;
 nd^.ufs_dir  :=nd;

 if (inode<>0) then
  nd^.ufs_inode:=inode
 else
  nd^.ufs_inode:=ufs_alloc_cdp_inode;

 mp:=dmp^.ufs_mount;

 if (dmp^.ufs_rootdir=nil) and ((mp^.mnt_flag and MNT_ROOTFS)<>0) then
 begin
  //
 end else
 begin
  {
   * '.' and '..' are always the two first entries in the
   * de_dlist list.
   *
   * Create the '.' entry in the new directory.
   }
  de:=ufs_newdirent('.', 1);
  de^.ufs_dirent^.d_type:=DT_DIR;
  de^.ufs_flags:=de^.ufs_flags or UFS_DOT;
  TAILQ_INSERT_TAIL(@nd^.ufs_dlist,de,@de^.ufs_list);
  de^.ufs_dir:=nd;

  { Create the '..' entry in the new directory. }
  de:=ufs_newdirent('..', 2);
  de^.ufs_dirent^.d_type:=DT_DIR;
  de^.ufs_flags:=de^.ufs_flags or UFS_DOTDOT;
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
 end;

 Exit(nd);
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

function ufs_lookupx(ap:p_vop_lookup_args;dm_unlock:PBoolean):Integer;
label
 _error;
var
 cnp:p_componentname;
 dvp:p_vnode;
 vpp:pp_vnode;
 de,dd:p_ufs_dirent;
 error,flags,nameiop,dvplocked:Integer;
 pname:PChar;
begin
 cnp:=ap^.a_cnp;
 vpp:=ap^.a_vpp;
 dvp:=ap^.a_dvp;
 pname:=cnp^.cn_nameptr;
 flags:=cnp^.cn_flags;
 nameiop:=cnp^.cn_nameiop;
 dd:=dvp^.v_data;
 vpp^:=nil;

 if ((flags and ISLASTCN)<>0) and (nameiop=RENAME) then
  Exit(EOPNOTSUPP);

 if (dvp^.v_type<>VDIR) then
  Exit(ENOTDIR);

 if (((flags and ISDOTDOT)<>0) and ((dvp^.v_vflag and VV_ROOT)<>0)) then
  Exit(EIO);

 error:=VOP_ACCESS(dvp, VEXEC);
 if (error<>0) then
  Exit(error);

 if (cnp^.cn_namelen=1) and (pname^='.') then
 begin
  if ((flags and ISLASTCN) and nameiop<>LOOKUP) then
   Exit(EINVAL);

  vpp^:=dvp;
  VREF(dvp);

  Exit(0);
 end;

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
  dm_unlock^:=false;

  vn_lock(dvp, dvplocked or LK_RETRY);

  Exit(error);
 end;

 de:=ufs_find(dd, cnp^.cn_nameptr, cnp^.cn_namelen, 0);

 if (de=nil) then
 begin
  Case nameiop of
   CREATE:
    begin
     //if not last
     if ((flags and ISLASTCN)=0) then Exit(ENOENT);
    end;

   LOOKUP,
   DELETE,
   RENAME:Exit(ENOENT);
   else;
  end;
  goto _error;
 end;

 if ((de^.ufs_flags and UFS_WHITEOUT)<>0) then
 begin
  _error:
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
 dm_unlock^:=false;

 Exit(error);
end;

function ufs_lookup(ap:p_vop_lookup_args):Integer;
var
 dmp:p_ufs_mount;
 dm_unlock:Boolean;
begin
 dmp:=VFSTOUFS(ap^.a_dvp^.v_mount);

 dm_unlock:=True;
 sx_xlock(@dmp^.ufs_lock);

 Result:=ufs_lookupx(ap, @dm_unlock);

 if (dm_unlock) then
 begin
  sx_xunlock(@dmp^.ufs_lock);
 end;
end;

function ufs_access(ap:p_vop_access_args):Integer;
var
 vp:p_vnode;
 de:p_ufs_dirent;
 error:Integer;
begin
 vp:=ap^.a_vp;

 de:=vp^.v_data;

 error:=vaccess(vp^.v_type, de^.ufs_mode, de^.ufs_uid, de^.ufs_gid, ap^.a_accmode, nil);
 if (error=0) then
  Exit(0);

 if (error<>EACCES) then
  Exit(error);

 if ((p_leader.p_flag and P_CONTROLT)=0) then
  Exit(error);

 Exit(error);
end;

function ufs_getattr(ap:p_vop_getattr_args):Integer;
var
 vp:p_vnode;
 vap:p_vattr;
 de:p_ufs_dirent;

begin
 vp:=ap^.a_vp;
 vap:=ap^.a_vap;
 de:=vp^.v_data;

 vap^.va_uid :=de^.ufs_uid;
 vap^.va_gid :=de^.ufs_gid;
 vap^.va_mode:=de^.ufs_mode;

 case vp^.v_type of
  VLNK:
   begin
    vap^.va_size :=strlen(de^.ufs_symlink);
    vap^.va_bytes:=0;
   end;
  VDIR:
   begin
    vap^.va_size :=DEV_BSIZE;
    vap^.va_bytes:=DEV_BSIZE;
   end;
  else
   begin
    vap^.va_size :=de^.ufs_size;
    vap^.va_bytes:=de^.ufs_bytes;
   end;
 end;

 vap^.va_blocksize:=DEV_BSIZE;
 vap^.va_type:=vp^.v_type;

 vap^.va_atime    :=de^.ufs_atime;
 vap^.va_mtime    :=de^.ufs_mtime;
 vap^.va_ctime    :=de^.ufs_ctime;
 vap^.va_birthtime:=de^.ufs_btime;

 vap^.va_gen    :=0;
 vap^.va_flags  :=0;
 vap^.va_filerev:=0;
 vap^.va_nlink  :=de^.ufs_links;
 vap^.va_fileid :=de^.ufs_inode;

 Exit(0);
end;

function ufs_setattr(ap:p_vop_setattr_args):Integer;
var
 de:p_ufs_dirent;
 vap:p_vattr;
 vp:p_vnode;
 c,error:Integer;
 uid:uid_t;
 gid:gid_t;
begin
 vap:=ap^.a_vap;
 vp:=ap^.a_vp;

 if (vap^.va_type     <>VNON) or
    (vap^.va_nlink    <>VNOVAL) or
    (vap^.va_fsid     <>VNOVAL) or
    (vap^.va_fileid   <>VNOVAL) or
    (vap^.va_blocksize<>VNOVAL) or
    ((vap^.va_flags   <>VNOVAL) and (vap^.va_flags<>0)) or
    (vap^.va_rdev     <>VNOVAL) or
    (vap^.va_bytes    <>VNOVAL) or
    (vap^.va_gen      <>VNOVAL) then
 begin
  Exit(EINVAL);
 end;

 de:=vp^.v_data;

 error:=0;
 c:=0;

 if (vap^.va_uid=VNOVAL) then
  uid:=de^.ufs_uid
 else
  uid:=vap^.va_uid;

 if (vap^.va_gid=VNOVAL) then
  gid:=de^.ufs_gid
 else
  gid:=vap^.va_gid;

 if (uid<>de^.ufs_uid) or (gid<>de^.ufs_gid) then
 begin
  //if ((ap^.a_cred^.cr_uid<>de^.de_uid) or uid<>de^.de_uid or
  //    (gid<>de^.de_gid and !groupmember(gid, ap^.a_cred))) then
  //begin
  // error:=priv_check(td, PRIV_VFS_CHOWN);
  // if (error<>) then
  //  Exit(error);
  //end;
  de^.ufs_uid:=uid;
  de^.ufs_gid:=gid;
  c:=1;
 end;

 if (vap^.va_mode<>VNOVAL) then
 begin
  //if (ap^.a_cred^.cr_uid<>de^.de_uid) then
  //begin
  // error:=priv_check(td, PRIV_VFS_ADMIN);
  // if (error<>0) then
  //  Exit(error);
  //end;
  de^.ufs_mode:=vap^.va_mode;
  c:=1;
 end;

 if (vap^.va_atime.tv_sec<>VNOVAL) or
    (vap^.va_mtime.tv_sec<>VNOVAL) or
    (vap^.va_birthtime.tv_sec<>VNOVAL) then
 begin
  { See the comment in ufs_vnops::ufs_setattr(). }
  error:=VOP_ACCESS(vp, VADMIN);
  if (error<>0) then
  begin
   if ((vap^.va_vaflags and VA_UTIMES_NULL)=0) then Exit(error);
   error:=VOP_ACCESS(vp, VWRITE);
   if (error<>0) then Exit(error);
  end;
  if (vap^.va_atime.tv_sec<>VNOVAL) then
  begin
   de^.ufs_atime:=vap^.va_atime;
  end;
  if (vap^.va_mtime.tv_sec<>VNOVAL) then
  begin
   de^.ufs_mtime:=vap^.va_mtime;
  end;
  if (vap^.va_birthtime.tv_sec<>VNOVAL) then
  begin
   de^.ufs_btime:=vap^.va_birthtime;
  end;
  c:=1;
 end;

 if (c<>0) then
 begin
  vfs_timestamp(@de^.ufs_mtime);
 end;
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

 if (ap^.a_ncookies<>nil) then
 begin
  tmp_ncookies:=ap^.a_ncookies;
  ap^.a_ncookies^:=0;
  ap^.a_ncookies:=nil;
 end;

 mp:=ap^.a_vp^.v_mount;
 dmp:=VFSTOUFS(mp);

 error:=0;
 de:=ap^.a_vp^.v_data;
 off:=0;

 sx_xlock(@dmp^.ufs_lock);

 dd:=TAILQ_FIRST(@de^.ufs_dlist);
 while (dd<>nil) do
 begin

  if ((dd^.ufs_flags and (UFS_COVERED or UFS_WHITEOUT))<>0) then
  begin
   dd:=TAILQ_NEXT(dd,@dd^.ufs_list);
   continue;
  end;

  dp:=dd^.ufs_dirent;

  if (dp^.d_reclen > uio^.uio_resid) then break;

  dp^.d_fileno:=dd^.ufs_inode;

  if (off >= uio^.uio_offset) then
  begin
   error:=vfs_read_dirent(ap, dp, off);
   if (error<>0) then break;
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

function ufs_rread(ap:p_vop_read_args):Integer;
begin
 if (ap^.a_vp^.v_type<>VDIR) then
  Exit(EINVAL);

 Exit(VOP_READDIR(ap^.a_vp, ap^.a_uio, nil, nil, nil));
end;

function ufs_readlink(ap:p_vop_readlink_args):Integer;
var
 de:p_ufs_dirent;
begin
 de:=ap^.a_vp^.v_data;
 if (de^.ufs_dirent^.d_type<>DT_LNK) then Exit(EINVAL);

 Exit(uiomove(de^.ufs_symlink, strlen(de^.ufs_symlink), ap^.a_uio));
end;

function ufs_symlink(ap:p_vop_symlink_args):Integer;
var
 i,error:Integer;
 dd:p_ufs_dirent;
 de,de_cov:p_ufs_dirent;
 dmp:p_ufs_mount;
begin
 error:=0;
 //error:=priv_check(curkthread, PRIV_DEVFS_SYMLINK);
 if (error<>0) then
  Exit(error);

 dmp:=VFSTOUFS(ap^.a_dvp^.v_mount);

 sx_xlock(@dmp^.ufs_lock);

 dd:=ap^.a_dvp^.v_data;
 de_cov:=ufs_find(dd, ap^.a_cnp^.cn_nameptr, ap^.a_cnp^.cn_namelen, 0);

 if (de_cov<>nil) then
 begin
  sx_xunlock(@dmp^.ufs_lock);
  Exit(EEXIST);
 end;

 de:=ufs_newdirent(ap^.a_cnp^.cn_nameptr, ap^.a_cnp^.cn_namelen);
 de^.ufs_flags:=UFS_USER;
 de^.ufs_uid  :=0;
 de^.ufs_gid  :=0;
 de^.ufs_mode :=UFS_DEFAULT_MODE;
 de^.ufs_inode:=ufs_alloc_cdp_inode;
 de^.ufs_dir  :=dd;
 de^.ufs_dirent^.d_type:=DT_LNK;

 i:=strlen(ap^.a_target) + 1;
 de^.ufs_symlink:=AllocMem(i);
 Move(ap^.a_target^, de^.ufs_symlink^, i);

 TAILQ_INSERT_TAIL(@dd^.ufs_dlist,de,@de^.ufs_list);

 Exit(ufs_allocv(de, ap^.a_dvp^.v_mount, LK_EXCLUSIVE, ap^.a_vpp)); //sx_xunlock
end;

function ufs_remove(ap:p_vop_remove_args):Integer;
var
 dvp,vp:p_vnode;
 dd:p_ufs_dirent;
 de,de_cov:p_ufs_dirent;
 dmp:p_ufs_mount;
begin
 dvp:=ap^.a_dvp;
 vp:=ap^.a_vp;
 dmp:=VFSTOUFS(vp^.v_mount);

 ASSERT_VOP_ELOCKED(dvp, 'ufs_remove');
 ASSERT_VOP_ELOCKED(vp,  'ufs_remove');

 sx_xlock(@dmp^.ufs_lock);
 dd:=ap^.a_dvp^.v_data;
 de:=vp^.v_data;

 if (de^.ufs_dirent^.d_type=DT_DIR) then
 begin
  Result:=_ufs_dir_status(dmp, de);
  if (Result<>0) then
  begin
   sx_xunlock(@dmp^.ufs_lock);
   Exit;
  end;
 end;

 if (de^.ufs_dirent^.d_type=DT_LNK) then
 begin
  de_cov:=ufs_find(dd, de^.ufs_dirent^.d_name, de^.ufs_dirent^.d_namlen, 0);
  if (de_cov<>nil) then
   de_cov^.ufs_flags:=de_cov^.ufs_flags and (not UFS_COVERED);
 end;

 VOP_UNLOCK(vp, 0);

 if (dvp<>vp) then
  VOP_UNLOCK(dvp, 0);

 if (de^.ufs_dirent^.d_type=DT_DIR) then
 begin
  _ufs_rmdir(dmp, de);
 end else
 begin
  ufs_delete(dmp, de, UFS_DEL_NORECURSE);
 end;

 sx_xunlock(@dmp^.ufs_lock);

 if (dvp<>vp) then
  vn_lock(dvp, LK_EXCLUSIVE or LK_RETRY);

 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);

 Exit(0);
end;

function ufs_rmdir(ap:p_vop_rmdir_args):Integer;
var
 dvp,vp:p_vnode;
 de:p_ufs_dirent;
 dmp:p_ufs_mount;
begin
 dvp:=ap^.a_dvp;
 vp:=ap^.a_vp;
 dmp:=VFSTOUFS(vp^.v_mount);

 ASSERT_VOP_ELOCKED(dvp, 'ufs_remove');
 ASSERT_VOP_ELOCKED(vp,  'ufs_remove');

 sx_xlock(@dmp^.ufs_lock);

 de:=vp^.v_data;

 Result:=_ufs_dir_status(dmp, de);
 if (Result<>0) then
 begin
  sx_xunlock(@dmp^.ufs_lock);
  Exit;
 end;

 VOP_UNLOCK(vp, 0);

 if (dvp<>vp) then
  VOP_UNLOCK(dvp, 0);

 _ufs_rmdir(dmp, de);

 sx_xunlock(@dmp^.ufs_lock);

 if (dvp<>vp) then
  vn_lock(dvp, LK_EXCLUSIVE or LK_RETRY);

 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);

 Exit(0);
end;

function ufs_mkdir(ap:p_vop_mkdir_args):Integer;
var
 dvp:p_vnode;
 cnp:p_componentname;
 vap:p_vattr;
 dmp:p_ufs_mount;
 dd:p_ufs_dirent;
 de_cov:p_ufs_dirent;
 de:p_ufs_dirent;
begin
 dvp:=ap^.a_dvp;
 cnp:=ap^.a_cnp;
 vap:=ap^.a_vap;
 dmp:=VFSTOUFS(dvp^.v_mount);

 //priv_check

 sx_xlock(@dmp^.ufs_lock);

 dd:=dvp^.v_data;

 de_cov:=ufs_find(dd, cnp^.cn_nameptr, cnp^.cn_namelen, 0);

 if (de_cov<>nil) then
 begin
  sx_xunlock(@dmp^.ufs_lock);
  Exit(EEXIST);
 end;

 de:=ufs_vmkdir(dmp,ap^.a_cnp^.cn_nameptr,ap^.a_cnp^.cn_namelen,dd,0);

 de^.ufs_mode :=vap^.va_mode;

 Exit(ufs_allocv(de, ap^.a_dvp^.v_mount, LK_EXCLUSIVE, ap^.a_vpp)); //sx_xunlock
end;

function ufs_reclaim(ap:p_vop_reclaim_args):Integer;
var
 vp:p_vnode;
 de:p_ufs_dirent;
begin
 vp:=ap^.a_vp;

 mtx_lock(ufs_interlock);
 de:=vp^.v_data;
 if (de<>nil) then
 begin
  de^.ufs_vnode:=nil;
  vp^.v_data:=nil;
 end;
 mtx_unlock(ufs_interlock);

 //vnode_destroy_vobject(vp);

 Exit(0);
end;

end.

