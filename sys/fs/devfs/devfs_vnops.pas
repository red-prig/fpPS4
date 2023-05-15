unit devfs_vnops;

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
 devfs,
 kern_mtx,
 kern_sx,
 kern_conf;

var
 devfs_de_interlock:mtx; //MTX_SYSINIT(devfs_de_interlock, @devfs_de_interlock, 'devfs interlock', MTX_DEF);
 cdevpriv_mtx      :mtx; //MTX_SYSINIT(cdevpriv_mtx, @cdevpriv_mtx, 'cdevpriv lock', MTX_DEF);
 clone_drain_lock  :t_sx=(n:'clone events drain lock';c:nil;m:0);

function  devfs_fp_check(fp:p_file;devp:pp_cdev;dswp:pp_cdevsw;ref:PInteger):Integer;
function  devfs_get_cdevpriv(datap:PPointer):Integer;
function  devfs_set_cdevpriv(priv:Pointer;priv_dtr:cdevpriv_dtr_t):Integer;
procedure devfs_destroy_cdevpriv(p:p_cdev_privdata);
procedure devfs_fpdrop(fp:p_file);
procedure devfs_clear_cdevpriv();
function  devfs_populate_vp(vp:p_vnode):Integer;
function  devfs_vptocnp(ap:p_vop_vptocnp_args):Integer;
function  devfs_fqpn(buf:PChar;dmp:p_devfs_mount;dd:p_devfs_dirent;cnp:p_componentname):PChar;
function  devfs_allocv_drop_refs(drop_dm_lock:Integer;dmp:p_devfs_mount;de:p_devfs_dirent):Integer;
function  devfs_allocv(de:p_devfs_dirent;mp:p_mount;lockmode:Integer;vpp:pp_vnode):Integer;
function  devfs_access(ap:p_vop_access_args):Integer;
function  devfs_close(ap:p_vop_close_args):Integer;
function  devfs_close_f(fp:p_file):Integer;
function  devfs_fsync(ap:p_vop_fsync_args):Integer;
function  devfs_getattr(ap:p_vop_getattr_args):Integer;
function  devfs_ioctl_f(fp:p_file;com:QWORD;data:Pointer):Integer;
function  devfs_kqfilter_f(fp:p_file;kn:Pointer):Integer;
function  devfs_prison_check(de:p_devfs_dirent):Integer;
function  devfs_lookupx(ap:p_vop_lookup_args;dm_unlock:PInteger):Integer;
function  devfs_lookup(ap:p_vop_lookup_args):Integer;
function  devfs_mknod(ap:p_vop_mknod_args):Integer;
function  devfs_open(ap:p_vop_open_args):Integer;
function  devfs_pathconf(ap:p_vop_pathconf_args):Integer;
function  devfs_poll_f(fp:p_file;events:Integer):Integer;
function  devfs_print(ap:p_vop_print_args):Integer;
function  devfs_read_f(fp:p_file;uio:p_uio;flags:Integer):Integer;
function  devfs_readdir(ap:p_vop_readdir_args):Integer;
function  devfs_readlink(ap:p_vop_readlink_args):Integer;
function  devfs_reclaim(ap:p_vop_reclaim_args):Integer;
function  devfs_remove(ap:p_vop_remove_args):Integer;
function  devfs_revoke(ap:p_vop_revoke_args):Integer;
function  devfs_rioctl(ap:p_vop_ioctl_args):Integer;
function  devfs_rread(ap:p_vop_read_args):Integer;
function  devfs_setattr(ap:p_vop_setattr_args):Integer;
function  devfs_stat_f(fp:p_file;sb:p_stat):Integer;
function  devfs_symlink(ap:p_vop_symlink_args):Integer;
function  devfs_truncate_f(fp:p_file;length:Int64):Integer;
function  devfs_write_f(fp:p_file;uio:p_uio;flags:Integer):Integer;
function  dev2udev(x:p_cdev):Integer;

const
 devfs_vnodeops:vop_vector=(
  vop_default       :@default_vnodeops;
  vop_bypass        :nil;

  vop_islocked      :nil;
  vop_lookup        :@devfs_lookup;
  vop_create        :nil;
  vop_whiteout      :nil;
  vop_mknod         :@devfs_mknod;
  vop_open          :nil;
  vop_close         :nil;
  vop_access        :@devfs_access;
  vop_accessx       :nil;
  vop_getattr       :@devfs_getattr;
  vop_setattr       :@devfs_setattr;
  vop_markatime     :nil;
  vop_read          :@devfs_rread;
  vop_write         :nil;
  vop_ioctl         :@devfs_rioctl;
  vop_poll          :nil;
  vop_kqfilter      :nil;
  vop_revoke        :@devfs_revoke;
  vop_fsync         :nil;
  vop_remove        :@devfs_remove;
  vop_link          :nil;
  vop_rename        :nil;
  vop_mkdir         :nil;
  vop_rmdir         :nil;
  vop_symlink       :@devfs_symlink;
  vop_readdir       :@devfs_readdir;
  vop_readlink      :@devfs_readlink;
  vop_inactive      :nil;
  vop_reclaim       :@devfs_reclaim;
  vop_lock1         :nil;
  vop_unlock        :nil;
  vop_bmap          :nil;
  vop_strategy      :nil;
  vop_getwritemount :nil;
  vop_print         :nil;
  vop_pathconf      :@devfs_pathconf;
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
  vop_vptocnp       :@devfs_vptocnp;
  vop_allocate      :nil;
  vop_unp_bind      :nil;
  vop_unp_connect   :nil;
  vop_unp_detach    :nil;
 );

 devfs_specops:vop_vector=(
  vop_default       :@default_vnodeops;
  vop_bypass        :nil;

  vop_islocked      :nil;
  vop_lookup        :nil;
  vop_create        :@VOP_PANIC;
  vop_whiteout      :nil;
  vop_mknod         :@VOP_PANIC;
  vop_open          :@devfs_open;
  vop_close         :@devfs_close;
  vop_access        :@devfs_access;
  vop_accessx       :nil;
  vop_getattr       :@devfs_getattr;
  vop_setattr       :@devfs_setattr;
  vop_markatime     :nil;
  vop_read          :@VOP_PANIC;
  vop_write         :@VOP_PANIC;
  vop_ioctl         :nil;
  vop_poll          :nil;
  vop_kqfilter      :nil;
  vop_revoke        :@devfs_revoke;
  vop_fsync         :@devfs_fsync;
  vop_remove        :@devfs_remove;
  vop_link          :@VOP_PANIC;
  vop_rename        :@VOP_PANIC;
  vop_mkdir         :@VOP_PANIC;
  vop_rmdir         :@VOP_PANIC;
  vop_symlink       :@VOP_PANIC;
  vop_readdir       :@VOP_PANIC;
  vop_readlink      :@VOP_PANIC;
  vop_inactive      :nil;
  vop_reclaim       :@devfs_reclaim;
  vop_lock1         :nil;
  vop_unlock        :nil;
  vop_bmap          :@VOP_PANIC;
  vop_strategy      :@VOP_PANIC;
  vop_getwritemount :nil;
  vop_print         :@devfs_print;
  vop_pathconf      :@devfs_pathconf;
  vop_advlock       :nil;
  vop_advlockasync  :nil;
  vop_advlockpurge  :nil;
  vop_reallocblks   :@VOP_PANIC;
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
  vop_vptocnp       :@devfs_vptocnp;
  vop_allocate      :nil;
  vop_unp_bind      :nil;
  vop_unp_connect   :nil;
  vop_unp_detach    :nil;
 );

 devfs_ops_f:fileops=(
  fo_read    :@devfs_read_f;
  fo_write   :@devfs_write_f;
  fo_truncate:@devfs_truncate_f;
  fo_ioctl   :@devfs_ioctl_f;
  fo_poll    :@devfs_poll_f;
  fo_kqfilter:@devfs_kqfilter_f;
  fo_stat    :@devfs_stat_f;
  fo_close   :@devfs_close_f;
  fo_chmod   :@vn_chmod;
  fo_chown   :@vn_chown;
  fo_flags   :DFLAG_PASSABLE or DFLAG_SEEKABLE
 );

implementation

uses
 sysutils,
 errno,
 systm,
 kern_thr,
 vfs_subr,
 vsys_generic,
 devfs_devs,
 devfs_rule,
 devfs_vfsops,
 kern_descrip,
 kern_mtxpool,
 subr_uio;

function VFSTODEVFS(mp:p_mount):p_devfs_mount; inline;
begin
 Result:=mp^.mnt_data;
end;

function cdev2priv(c:Pointer):p_cdev_priv; inline;
begin
 Result:=c-ptruint(@p_cdev_priv(nil)^.cdp_c);
end;

function devfs_fp_check(fp:p_file;devp:pp_cdev;dswp:pp_cdevsw;ref:PInteger):Integer;
begin
 dswp^:=devvn_refthread(fp^.f_vnode, devp, ref);
 if (devp^<>fp^.f_data) then
 begin
  if (dswp^<>nil) then
   dev_relthread(devp^, ref^);
  Exit(ENXIO);
 end;
 Assert(devp^^.si_refcount > 0,'devfs: un-referenced struct cdev');
 if (dswp^=nil) then
  Exit(ENXIO);
 curkthread^.td_fpop:=fp;
 Exit(0);
end;

function devfs_get_cdevpriv(datap:PPointer):Integer;
var
 fp:p_file;
 p:p_cdev_privdata;
 error:Integer;
begin
 fp:=curkthread^.td_fpop;
 if (fp=nil) then
  Exit(EBADF);
 p:=fp^.f_cdevpriv;
 if (p<>nil) then
 begin
  error:=0;
  datap^:=p^.cdpd_data;
 end else
  error:=ENOENT;
 Exit(error);
end;

function devfs_set_cdevpriv(priv:Pointer;priv_dtr:cdevpriv_dtr_t):Integer;
var
 fp:p_file;
 cdp:p_cdev_priv;
 p:p_cdev_privdata;
 error:Integer;
begin
 fp:=curkthread^.td_fpop;
 if (fp=nil) then
  Exit(ENOENT);

 cdp:=cdev2priv(fp^.f_data);
 p:=AllocMem(sizeof(t_cdev_privdata));
 p^.cdpd_data:=priv;
 p^.cdpd_dtr :=priv_dtr;
 p^.cdpd_fp  :=fp;
 mtx_lock(cdevpriv_mtx);
 if (fp^.f_cdevpriv=nil) then
 begin
  LIST_INSERT_HEAD(@cdp^.cdp_fdpriv,p,@p^.cdpd_list);
  fp^.f_cdevpriv:=p;
  mtx_unlock(cdevpriv_mtx);
  error:=0;
 end else
 begin
  mtx_unlock(cdevpriv_mtx);
  FreeMem(p);
  error:=EBUSY;
 end;
 Exit(error);
end;

procedure devfs_destroy_cdevpriv(p:p_cdev_privdata);
begin
 mtx_assert(cdevpriv_mtx);
 p^.cdpd_fp^.f_cdevpriv:=nil;
 LIST_REMOVE(p,@p^.cdpd_list);
 mtx_unlock(cdevpriv_mtx);
 p^.cdpd_dtr(p^.cdpd_data);
 FreeMem(p);
end;

procedure devfs_fpdrop(fp:p_file);
var
 p:p_cdev_privdata;
begin
 mtx_lock(cdevpriv_mtx);
 p:=fp^.f_cdevpriv;
 if (p=nil) then
 begin
  mtx_unlock(cdevpriv_mtx);
  Exit;
 end;
 devfs_destroy_cdevpriv(p);
end;

procedure devfs_clear_cdevpriv();
var
 fp:p_file;
begin
 fp:=curkthread^.td_fpop;
 if (fp=nil) then
  Exit;
 devfs_fpdrop(fp);
end;

{
 * On success devfs_populate_vp() Exits with dmp^.dm_lock held.
 }
function devfs_populate_vp(vp:p_vnode):Integer;
var
 de:p_devfs_dirent;
 dmp:p_devfs_mount;
 locked:Integer;
begin
 ASSERT_VOP_LOCKED(vp, 'devfs_populate_vp');

 dmp:=VFSTODEVFS(vp^.v_mount);
 locked:=VOP_ISLOCKED(vp);

 sx_xlock(@dmp^.dm_lock);
 DEVFS_DMP_HOLD(dmp);

 { Can't call devfs_populate() with the vnode lock held. }
 VOP_UNLOCK(vp, 0);
 devfs_populate(dmp);

 sx_xunlock(@dmp^.dm_lock);
 vn_lock(vp, locked or LK_RETRY);
 sx_xlock(@dmp^.dm_lock);

 if DEVFS_DMP_DROP(dmp) then
 begin
  sx_xunlock(@dmp^.dm_lock);
  devfs_unmount_final(dmp);
  Exit(EBADF);
 end;
 if ((vp^.v_iflag and VI_DOOMED)<>0) then
 begin
  sx_xunlock(@dmp^.dm_lock);
  Exit(EBADF);
 end;
 de:=vp^.v_data;
 Assert(de<>nil,'devfs_populate_vp: vp^.v_data=nil but vnode not doomed');
 if ((de^.de_flags and DE_DOOMED)<>0) then
 begin
  sx_xunlock(@dmp^.dm_lock);
  Exit(EBADF);
 end;

 Exit(0);
end;

function devfs_vptocnp(ap:p_vop_vptocnp_args):Integer;
label
 finished;
var
 vp:p_vnode;
 dvp:pp_vnode;
 dmp:p_devfs_mount;
 buf:PChar;
 buflen:PInteger;
 dd,de:p_devfs_dirent;
 i,error:Integer;
begin
 vp    :=ap^.a_vp;
 dvp   :=ap^.a_vpp;
 buf   :=ap^.a_buf;
 buflen:=ap^.a_buflen;

 dmp:=VFSTODEVFS(vp^.v_mount);

 error:=devfs_populate_vp(vp);
 if (error<>0) then
  Exit(error);

 i:=buflen^;
 dd:=vp^.v_data;

 if (vp^.v_type=VCHR) then
 begin
  Dec(i,strlen(dd^.de_cdp^.cdp_c.si_name));
  if (i < 0) then
  begin
   error:=ENOMEM;
   goto finished;
  end;
  Move(dd^.de_cdp^.cdp_c.si_name^, (buf + i)^,strlen(dd^.de_cdp^.cdp_c.si_name));
  de:=dd^.de_dir;
 end else
 if (vp^.v_type=VDIR) then
 begin
  if (dd=dmp^.dm_rootdir) then
  begin
   dvp^:=vp;
   vref(dvp^);
   goto finished;
  end;
  Dec(i,dd^.de_dirent^.d_namlen);
  if (i < 0) then
  begin
   error:=ENOMEM;
   goto finished;
  end;
  Move(dd^.de_dirent^.d_name, (buf + i)^, dd^.de_dirent^.d_namlen);
  de:=dd;
 end else
 begin
  error:=ENOENT;
  goto finished;
 end;
 buflen^:=i;
 de:=devfs_parent_dirent(de);
 if (de=nil) then
 begin
  error:=ENOENT;
  goto finished;
 end;
 mtx_lock(devfs_de_interlock);
 dvp^:=de^.de_vnode;
 if (dvp^<>nil) then
 begin
  VI_LOCK(dvp^);
  mtx_unlock(devfs_de_interlock);
  vholdl(dvp^);
  VI_UNLOCK(dvp^);
  vref(dvp^);
  vdrop(dvp^);
 end else
 begin
  mtx_unlock(devfs_de_interlock);
  error:=ENOENT;
 end;
finished:
 sx_xunlock(@dmp^.dm_lock);
 Exit(error);
end;

{
 * Construct the fully qualified path name relative to the mountpoint.
 * If a nil cnp is provided, no '/' is appended to the resulting path.
 }
function devfs_fqpn(buf:PChar;dmp:p_devfs_mount;dd:p_devfs_dirent;cnp:p_componentname):PChar;
var
 i:Integer;
 de:p_devfs_dirent;
begin
 sx_assert(@dmp^.dm_lock);

 i:=SPECNAMELEN;
 buf[i]:=#0;
 if (cnp<>nil) then
  Dec(i,cnp^.cn_namelen);
 if (i < 0) then
   Exit(nil);
 if (cnp<>nil) then
  Move(cnp^.cn_nameptr^, (buf + i)^, cnp^.cn_namelen);
 de:=dd;
 while (de<>dmp^.dm_rootdir) do
 begin
  if (cnp<>nil) or (i < SPECNAMELEN) then
  begin
   Dec(i);
   if (i < 0) then
     Exit(nil);
   buf[i]:='/';
  end;
  Dec(i,de^.de_dirent^.d_namlen);
  if (i < 0) then
    Exit(nil);
  Move(de^.de_dirent^.d_name, (buf + i)^, de^.de_dirent^.d_namlen);
  de:=devfs_parent_dirent(de);
  if (de=nil) then
   Exit(nil);
 end;
 Exit(buf + i);
end;

function devfs_allocv_drop_refs(drop_dm_lock:Integer;dmp:p_devfs_mount;de:p_devfs_dirent):Integer;
var
 not_found:Integer;
begin
 not_found:=0;
 if ((de^.de_flags and DE_DOOMED)<>0) then
  not_found:=1;
 if DEVFS_DE_DROP(de) then
 begin
  Assert(not_found=1,'DEVFS de dropped but not doomed');
  devfs_dirent_free(de);
 end;
 if DEVFS_DMP_DROP(dmp) then
 begin
  Assert(not_found=1,'DEVFS mount struct freed before dirent');
  not_found:=2;
  sx_xunlock(@dmp^.dm_lock);
  devfs_unmount_final(dmp);
 end;
 if (not_found=1) or ((drop_dm_lock<>0) and (not_found<>2)) then
  sx_unlock(@dmp^.dm_lock);
 Exit(not_found);
end;

procedure devfs_insmntque_dtr(vp:p_vnode;arg:Pointer);
var
 de:p_devfs_dirent;
begin
 de:=p_devfs_dirent(arg);
 mtx_lock(devfs_de_interlock);
 vp^.v_data:=nil;
 de^.de_vnode:=nil;
 mtx_unlock(devfs_de_interlock);
 vgone(vp);
 vput(vp);
end;

{
 * devfs_allocv shall be entered with dmp^.dm_lock held, and it drops
 * it on Exit.
 }
function devfs_allocv(de:p_devfs_dirent;mp:p_mount;lockmode:Integer;vpp:pp_vnode):Integer;
label
 loop;
var
 error:Integer;
 vp:p_vnode;
 dev:p_cdev;
 dmp:p_devfs_mount;
 dsw:p_cdevsw;
begin
 dmp:=VFSTODEVFS(mp);
 if ((de^.de_flags and DE_DOOMED)<>0) then
 begin
  sx_xunlock(@dmp^.dm_lock);
  Exit(ENOENT);
 end;
loop:
 DEVFS_DE_HOLD(de);
 DEVFS_DMP_HOLD(dmp);
 mtx_lock(devfs_de_interlock);
 vp:=de^.de_vnode;
 if (vp<>nil) then
 begin
  VI_LOCK(vp);
  mtx_unlock(devfs_de_interlock);
  sx_xunlock(@dmp^.dm_lock);
  vget(vp, lockmode or LK_INTERLOCK or LK_RETRY);
  sx_xlock(@dmp^.dm_lock);
  if (devfs_allocv_drop_refs(0, dmp, de)<>0) then
  begin
   vput(vp);
   Exit(ENOENT);
  end else
  if ((vp^.v_iflag and VI_DOOMED)<>0) then
  begin
   mtx_lock(devfs_de_interlock);
   if (de^.de_vnode=vp) then
   begin
    de^.de_vnode:=nil;
    vp^.v_data:=nil;
   end;
   mtx_unlock(devfs_de_interlock);
   vput(vp);
   goto loop;
  end;
  sx_xunlock(@dmp^.dm_lock);
  vpp^:=vp;
  Exit(0);
 end;

 mtx_unlock(devfs_de_interlock);

 if (de^.de_dirent^.d_type=DT_CHR) then
 begin
  if ((de^.de_cdp^.cdp_flags and CDP_ACTIVE)=0) then
  begin
   devfs_allocv_drop_refs(1, dmp, de);
   Exit(ENOENT);
  end;
  dev:=@de^.de_cdp^.cdp_c;
 end else
 begin
  dev:=nil;
 end;

 error:=getnewvnode('devfs', mp, @devfs_vnodeops, @vp);
 if (error<>0) then
 begin
  devfs_allocv_drop_refs(1, dmp, de);
  Writeln('devfs_allocv: failed to allocate new vnode');
  Exit(error);
 end;

 if (de^.de_dirent^.d_type=DT_CHR) then
 begin
  vp^.v_type:=VCHR;
  VI_LOCK(vp);
  dev_lock();
  dev_refl(dev);
  { XXX: v_rdev should be protect by vnode lock }
  vp^.v_rdev:=dev;
  Assert(vp^.v_usecount=1);
  Inc(dev^.si_usecount,vp^.v_usecount);
  { Special casing of ttys for deadfs.  Probably redundant. }
  dsw:=dev^.si_devsw;

  if (dsw<>nil) then
  if ((dsw^.d_flags and D_TTY)<>0) then
  begin
   vp^.v_vflag:=vp^.v_vflag or VV_ISTTY;
  end;

  dev_unlock();
  VI_UNLOCK(vp);

  if ((dev^.si_flags and SI_ETERNAL)<>0) then
   vp^.v_vflag:=vp^.v_vflag or VV_ETERNALDEV;

  vp^.v_op:=@devfs_specops;
 end else
 if (de^.de_dirent^.d_type=DT_DIR) then
 begin
  vp^.v_type:=VDIR;
 end else
 if (de^.de_dirent^.d_type=DT_LNK) then
 begin
  vp^.v_type:=VLNK;
 end else
 begin
  vp^.v_type:=VBAD;
 end;

 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY or LK_NOWITNESS);
 //VN_LOCK_ASHARE(vp);

 mtx_lock(devfs_de_interlock);

 vp^.v_data:=de;
 de^.de_vnode:=vp;

 mtx_unlock(devfs_de_interlock);

 error:=insmntque1(vp, mp, @devfs_insmntque_dtr, de);

 if (error<>0) then
 begin
  devfs_allocv_drop_refs(1, dmp, de);
  Exit(error);
 end;

 if (devfs_allocv_drop_refs(0, dmp, de)<>0) then
 begin
  vput(vp);
  Exit(ENOENT);
 end;

 //mac_devfs_vnode_associate(mp, de, vp);

 sx_xunlock(@dmp^.dm_lock);
 vpp^:=vp;
 Exit(0);
end;

function devfs_access(ap:p_vop_access_args):Integer;
var
 vp:p_vnode;
 de:p_devfs_dirent;
 error:Integer;
begin
 vp:=ap^.a_vp;

 de:=vp^.v_data;
 if (vp^.v_type=VDIR) then
  de:=de^.de_dir;

 error:=vaccess(vp^.v_type, de^.de_mode, de^.de_uid, de^.de_gid, ap^.a_accmode, nil);
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

{ ARGSUSED }
function devfs_close(ap:p_vop_close_args):Integer;
var
 vp,oldvp:p_vnode;
 dev:p_cdev;
 dsw:p_cdevsw;
 vp_locked,error,ref:Integer;
begin
 vp:=ap^.a_vp;
 //struct thread *td:=ap^.a_td;
 dev:=vp^.v_rdev;

 {
  * XXX: Don't call d_close() if we were called because of
  * XXX: insmntque1() failure.
  }
 if (vp^.v_data=nil) then
  Exit(0);

 {
  * Hack: a tty device that is a controlling terminal
  * has a reference from the session structure.
  * We cannot easily tell that a character device is
  * a controlling terminal, unless it is the closing
  * process' controlling terminal.  In that case,
  * if the reference count is 2 (this last descriptor
  * plus the session), release the reference from the session.
  }
 oldvp:=nil;
 //sx_xlock(@proctree_lock);
 //if (td<>nil) then
 //if (vp=td^.td_proc^.p_session^.s_ttyvp) then
 //begin
 // SESS_LOCK(td^.td_proc^.p_session);
 // VI_LOCK(vp);
 // if (count_dev(dev)=2) and ((vp^.v_iflag and VI_DOOMED)=0) then
 // begin
 //  td^.td_proc^.p_session^.s_ttyvp:=nil;
 //  td^.td_proc^.p_session^.s_ttydp:=nil;
 //  oldvp:=vp;
 // end;
 // VI_UNLOCK(vp);
 // SESS_UNLOCK(td^.td_proc^.p_session);
 //end;
 //sx_xunlock(@proctree_lock);
 if (oldvp<>nil) then
  vrele(oldvp);
 {
  * We do not want to really close the device if it
  * is still in use unless we are trying to close it
  * forcibly. Since every use (buffer, vnode, swap, cmap)
  * holds a reference to the vnode, and because we mark
  * any other vnodes that alias this device, when the
  * sum of the reference counts on all the aliased
  * vnodes descends to one, we are on last close.
  }
 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
  Exit(ENXIO);
 VI_LOCK(vp);
 if ((vp^.v_iflag and VI_DOOMED)<>0) then
 begin
  { Forced close. }
 end else
 if ((dsw^.d_flags and D_TRACKCLOSE)<>0) then
 begin
  { Keep device updated on status. }
 end else
 if (count_dev(dev) > 1) then
 begin
  VI_UNLOCK(vp);
  dev_relthread(dev, ref);
  Exit(0);
 end;
 vholdl(vp);
 VI_UNLOCK(vp);
 vp_locked:=VOP_ISLOCKED(vp);
 VOP_UNLOCK(vp, 0);
 Assert(dev^.si_refcount > 0,'devfs_close() on un-referenced struct cdev');
 error:=dsw^.d_close(dev, ap^.a_fflag, S_IFCHR);
 dev_relthread(dev, ref);
 vn_lock(vp, vp_locked or LK_RETRY);
 vdrop(vp);
 Exit(error);
end;

function devfs_close_f(fp:p_file):Integer;
var
 error:Integer;
 fpop:p_file;
begin
 {
  * NB: td may be nil if this descriptor is closed due to
  * garbage collection from a closed UNIX domain socket.
  }
 fpop:=curkthread^.td_fpop;
 curkthread^.td_fpop:=fp;
 error:=vnops.fo_close(fp);
 curkthread^.td_fpop:=fpop;

 {
  * The f_cdevpriv cannot be assigned non-nil value while we
  * are destroying the file.
  }
 if (fp^.f_cdevpriv<>nil) then
  devfs_fpdrop(fp);
 Exit(error);
end;

function devfs_fsync(ap:p_vop_fsync_args):Integer;
var
 error:Integer;
 //struct bufobj *bo;
 de:p_devfs_dirent;
begin
 if vn_isdisk(ap^.a_vp, @error) then
 begin
  //bo:=@ap^.a_vp^.v_bufobj;
  de:=ap^.a_vp^.v_data;
  if (error=ENXIO) {and (bo^.bo_dirty.bv_cnt > 0)} then
  begin
   Writeln('Device %s went missing before all of the data ',
           'could be written to it; expect data loss.',
           de^.de_dirent^.d_name);

   error:=vop_stdfsync(ap);
   if {(bo^.bo_dirty.bv_cnt<>0) or} (error<>0) then
    Assert(False,'devfs_fsync: vop_stdfsync failed.');
  end;

  Exit(0);
 end;

 Exit(vop_stdfsync(ap));
end;

function devfs_getattr(ap:p_vop_getattr_args):Integer;
var
 vp:p_vnode;
 vap:p_vattr;
 error:Integer;
 de:p_devfs_dirent;
 dmp:p_devfs_mount;
 dev:p_cdev;

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

 error:=devfs_populate_vp(vp);
 if (error<>0) then
  Exit(error);

 dmp:=VFSTODEVFS(vp^.v_mount);
 sx_xunlock(@dmp^.dm_lock);

 de:=vp^.v_data;
 Assert(de<>nil,'nil dirent in devfs_getattr vp=%p');
 if (vp^.v_type=VDIR) then
 begin
  de:=de^.de_dir;
  Assert(de<>nil,'nil dir dirent in devfs_getattr vp=%p');
 end;

 vap^.va_uid :=de^.de_uid;
 vap^.va_gid :=de^.de_gid;
 vap^.va_mode:=de^.de_mode;

 if (vp^.v_type=VLNK) then
  vap^.va_size:=strlen(de^.de_symlink)
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

 if (vp^.v_type<>VCHR) then
 begin
  fix(de^.de_atime,vap^.va_atime);
  fix(de^.de_mtime,vap^.va_mtime);
  fix(de^.de_ctime,vap^.va_ctime);
 end else
 begin
  dev:=vp^.v_rdev;

  fix(dev^.si_atime,vap^.va_atime);
  fix(dev^.si_mtime,vap^.va_mtime);
  fix(dev^.si_ctime,vap^.va_ctime);

  vap^.va_rdev:=cdev2priv(dev)^.cdp_inode;
 end;

 vap^.va_gen    :=0;
 vap^.va_flags  :=0;
 vap^.va_filerev:=0;
 vap^.va_nlink  :=de^.de_links;
 vap^.va_fileid :=de^.de_inode;

 Exit(error);
end;

{ ARGSUSED }
function devfs_ioctl_f(fp:p_file;com:QWORD;data:Pointer):Integer;
var
 td:p_kthread;
 dev:p_cdev;
 dsw:p_cdevsw;
 vp:p_vnode;
 vpold:p_vnode;
 error,i,ref:Integer;
 p:PChar;
 fgn:p_fiodgname_arg;
 fpop:p_file;
begin
 td:=curkthread;
 fpop:=td^.td_fpop;
 error:=devfs_fp_check(fp, @dev, @dsw, @ref);
 if (error<>0) then
  Exit(error);

 if (com=FIODTYPE) then
 begin
  PInteger(data)^:=dsw^.d_flags and D_TYPEMASK;
  td^.td_fpop:=fpop;
  dev_relthread(dev, ref);
  Exit(0);
 end else
 if (com=FIODGNAME) then
 begin
  fgn:=data;
  p:=devtoname(dev);
  i:=strlen(p) + 1;
  if (i > fgn^.len) then
   error:=EINVAL
  else
   error:=copyout(p, fgn^.buf, i);
  td^.td_fpop:=fpop;
  dev_relthread(dev, ref);
  Exit(error);
 end;
 error:=dsw^.d_ioctl(dev, com, data, fp^.f_flag);
 td^.td_fpop:=nil;
 dev_relthread(dev, ref);
 if (error=ENOIOCTL) then
  error:=ENOTTY;
 if (error=0) and (com=TIOCSCTTY) then
 begin
  vp:=fp^.f_vnode;

  vpold:=nil;
  { Do nothing if reassigning same control tty }
  //sx_slock(@proctree_lock);
  //if (td^.td_proc^.p_session^.s_ttyvp=vp) then
  //begin
  // sx_sunlock(@proctree_lock);
  // Exit(0);
  //end;
  //
  //vpold:=td^.td_proc^.p_session^.s_ttyvp;
  //VREF(vp);
  //SESS_LOCK(td^.td_proc^.p_session);
  //td^.td_proc^.p_session^.s_ttyvp:=vp;
  //td^.td_proc^.p_session^.s_ttydp:=cdev2priv(dev);
  //SESS_UNLOCK(td^.td_proc^.p_session);
  //
  //sx_sunlock(@proctree_lock);

  { Get rid of reference to old control tty }
  if (vpold<>nil) then
   vrele(vpold);
 end;
 Exit(error);
end;

{ ARGSUSED }
function devfs_kqfilter_f(fp:p_file;kn:Pointer):Integer;
var
 td:p_kthread;
 dev:p_cdev;
 dsw:p_cdevsw;
 error,ref:Integer;
 fpop:p_file;
begin
 td:=curkthread;
 fpop:=td^.td_fpop;
 error:=devfs_fp_check(fp, @dev, @dsw, @ref);
 if (error<>0) then
  Exit(error);
 error:=dsw^.d_kqfilter(dev, kn);
 td^.td_fpop:=fpop;
 dev_relthread(dev, ref);
 Exit(error);
end;

function devfs_prison_check(de:p_devfs_dirent):Integer;
var
 cdp:p_cdev_priv;
 error:Integer;
begin
 cdp:=de^.de_cdp;
 if (cdp=nil) then
  Exit(0);

 error:=0;
 //error:=prison_check(td^.td_ucred, dcr);
 if (error=0) then
  Exit(0);
 { We do, however, allow access to the controlling terminal }
 if ((p_leader.p_flag and P_CONTROLT)=0) then
  Exit(error);
 //if (td^.td_proc^.p_session^.s_ttydp=cdp) then
 // Exit(0);
 Exit(error);
end;

function devfs_lookupx(ap:p_vop_lookup_args;dm_unlock:PInteger):Integer;
label
 _or;
var
 cnp:p_componentname;
 dvp:p_vnode;
 vpp:pp_vnode;
 de,dd:p_devfs_dirent;
 dde:pp_devfs_dirent;
 dmp:p_devfs_mount;
 cdev:p_cdev;
 error,flags,nameiop,dvplocked:Integer;
 specname:array[0..SPECNAMELEN] of Char;
 pname:PChar;
begin
 cnp:=ap^.a_cnp;
 vpp:=ap^.a_vpp;
 dvp:=ap^.a_dvp;
 pname:=cnp^.cn_nameptr;
 flags:=cnp^.cn_flags;
 nameiop:=cnp^.cn_nameiop;
 dmp:=VFSTODEVFS(dvp^.v_mount);
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
  de:=devfs_parent_dirent(dd);
  if (de=nil) then
   Exit(ENOENT);
  dvplocked:=VOP_ISLOCKED(dvp);
  VOP_UNLOCK(dvp, 0);
  error:=devfs_allocv(de, dvp^.v_mount, cnp^.cn_lkflags and LK_TYPE_MASK, vpp);
  dm_unlock^:=0;
  vn_lock(dvp, dvplocked or LK_RETRY);
  Exit(error);
 end;

 dd:=dvp^.v_data;
 de:=devfs_find(dd, cnp^.cn_nameptr, cnp^.cn_namelen, 0);
 while (de=nil) do
 begin { While(...) so we can use break }

  if (nameiop=DELETE) then
   Exit(ENOENT);

  {
   * OK, we didn't have an entry for the name we were asked for
   * so we try to see if anybody can create it on demand.
   }
  pname:=devfs_fqpn(specname, dmp, dd, cnp);
  if (pname=nil) then
   break;

  cdev:=nil;
  DEVFS_DMP_HOLD(dmp);
  sx_xunlock(@dmp^.dm_lock);

  //sx_slock(@clone_drain_lock);
  //EVENTHANDLER_INVOKE(dev_clone, td^.td_ucred, pname, strlen(pname), @cdev);
  //sx_sunlock(@clone_drain_lock);

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
  if DEVFS_DMP_DROP(dmp) then
  begin
   dm_unlock^:=0;
   sx_xunlock(@dmp^.dm_lock);
   devfs_unmount_final(dmp);
   if (cdev<>nil) then
    dev_rel(cdev);
   Exit(ENOENT);
  end;

  if (cdev=nil) then
   break;

  dev_lock();
  dde:=@cdev2priv(cdev)^.cdp_dirents[dmp^.dm_idx];

  if (dde<>nil) then
  if (dde^<>nil) then
  begin
   de:=dde^;
  end;

  dev_unlock();
  dev_rel(cdev);
  break;
 end;

 if (de=nil) then goto _or;

 if ((de^.de_flags and DE_WHITEOUT)<>0) then
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

 if (devfs_prison_check(de)<>0) then
  Exit(ENOENT);

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
 error:=devfs_allocv(de, dvp^.v_mount, cnp^.cn_lkflags and LK_TYPE_MASK, vpp);
 dm_unlock^:=0;
 Exit(error);
end;

function devfs_lookup(ap:p_vop_lookup_args):Integer;
var
 j:Integer;
 dmp:p_devfs_mount;
 dm_unlock:Integer;
begin
 if (devfs_populate_vp(ap^.a_dvp)<>0) then
 begin
  Exit(ENOTDIR);
 end;

 dmp:=VFSTODEVFS(ap^.a_dvp^.v_mount);
 dm_unlock:=1;
 j:=devfs_lookupx(ap, @dm_unlock);

 if (dm_unlock=1) then
 begin
  sx_xunlock(@dmp^.dm_lock);
 end;
 Exit(j);
end;

function devfs_mknod(ap:p_vop_mknod_args):Integer;
label
 notfound;
var
 cnp:p_componentname;
 dvp:p_vnode;
 vpp:pp_vnode;
 dd,de:p_devfs_dirent;
 dmp:p_devfs_mount;
 error:Integer;
begin
 {
  * The only type of node we should be creating here is a
  * character device, for anything else return EOPNOTSUPP.
  }
 if (ap^.a_vap^.va_type<>VCHR) then
  Exit(EOPNOTSUPP);

 dvp:=ap^.a_dvp;
 dmp:=VFSTODEVFS(dvp^.v_mount);

 cnp:=ap^.a_cnp;
 vpp:=ap^.a_vpp;
 dd:=dvp^.v_data;

 error:=ENOENT;
 sx_xlock(@dmp^.dm_lock);

 de:=TAILQ_FIRST(@dd^.de_dlist);
 while (de<>nil) do
 begin
  if (cnp^.cn_namelen<>de^.de_dirent^.d_namlen) then
  begin
   de:=TAILQ_NEXT(de,@de^.de_list);
   continue;
  end;
  if (CompareByte(cnp^.cn_nameptr^, de^.de_dirent^.d_name, de^.de_dirent^.d_namlen)<>0) then
  begin
   de:=TAILQ_NEXT(de,@de^.de_list);
   continue;
  end;
  if ((de^.de_flags and DE_WHITEOUT)<>0) then
   break;
  goto notfound;
 end;
 if (de=nil) then
  goto notfound;
 de^.de_flags:=de^.de_flags and (not DE_WHITEOUT);
 error:=devfs_allocv(de, dvp^.v_mount, LK_EXCLUSIVE, vpp);
 Exit(error);
notfound:
 sx_xunlock(@dmp^.dm_lock);
 Exit(error);
end;

{ ARGSUSED }
function devfs_open(ap:p_vop_open_args):Integer;
var
 td:p_kthread;
 vp:p_vnode;
 dev:p_cdev;
 fp:p_file;
 error,ref,vlocked:Integer;
 dsw:p_cdevsw;
 fpop:p_file;
 mtxp:p_mtx;
begin
 td:=curkthread;
 vp:=ap^.a_vp;
 dev:=vp^.v_rdev;
 fp:=ap^.a_fp;

 if (vp^.v_type=VBLK) then
  Exit(ENXIO);

 if (dev=nil) then
  Exit(ENXIO);

 { Make this field valid before any I/O in d_open. }
 if (dev^.si_iosize_max=0) then
  dev^.si_iosize_max:=DFLTPHYS;

 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
  Exit(ENXIO);
 if (fp=nil) and (dsw^.d_fdopen<>nil) then
 begin
  dev_relthread(dev, ref);
  Exit(ENXIO);
 end;

 vlocked:=VOP_ISLOCKED(vp);
 VOP_UNLOCK(vp, 0);

 fpop:=td^.td_fpop;
 td^.td_fpop:=fp;
 if (fp<>nil) then
 begin
  fp^.f_data:=dev;
  fp^.f_vnode:=vp;
 end;
 if (dsw^.d_fdopen<>nil) then
  error:=dsw^.d_fdopen(dev, ap^.a_mode, fp)
 else
  error:=dsw^.d_open(dev, ap^.a_mode, S_IFCHR);
 { cleanup any cdevpriv upon error }
 if (error<>0) then
  devfs_clear_cdevpriv();
 td^.td_fpop:=fpop;

 vn_lock(vp, vlocked or LK_RETRY);
 dev_relthread(dev, ref);
 if (error<>0) then
 begin
  if (error=ERESTART) then
   error:=EINTR;
  Exit(error);
 end;

 if (fp=nil) then
  Exit(error);

 if (fp^.f_ops=@badfileops) then
  finit(fp, fp^.f_flag, DTYPE_VNODE, dev, @devfs_ops_f);
 mtxp:=mtx_pool_find(mtxpool_sleep, fp);

 {
  * Hint to the dofilewrite() to not force the buffer draining
  * on the writer to the file.  Most likely, the write would
  * not need normal buffers.
  }
 mtx_lock(mtxp^);
 fp^.f_vnread_flags:=fp^.f_vnread_flags or FDEVFS_VNODE;
 mtx_unlock(mtxp^);
 Exit(error);
end;

function devfs_pathconf(ap:p_vop_pathconf_args):Integer;
begin
 case ap^.a_name of
  _PC_MAC_PRESENT:
   begin
    {
     * If MAC is enabled, devfs automatically supports
     * trivial non-persistant label storage.
     }
    ap^.a_retval^:=1;
    Exit(0);
   end
  else
   Exit(vop_stdpathconf(ap));
 end;
 { NOTREACHED }
end;

{ ARGSUSED }
function devfs_poll_f(fp:p_file;events:Integer):Integer;
var
 td:p_kthread;
 dev:p_cdev;
 dsw:p_cdevsw;
 error,ref:Integer;
 fpop:p_file;
begin
 td:=curkthread;
 fpop:=td^.td_fpop;
 error:=devfs_fp_check(fp, @dev, @dsw, @ref);
 if (error<>0) then
  Exit(poll_no_poll(events));
 error:=dsw^.d_poll(dev, events);
 td^.td_fpop:=fpop;
 dev_relthread(dev, ref);
 Exit(error);
end;

{
 * Print out the contents of a special device vnode.
 }
function devfs_print(ap:p_vop_print_args):Integer;
begin
 Writeln(Format('dev %s',[devtoname(ap^.a_vp^.v_rdev)]));
 Exit(0);
end;

function devfs_read_f(fp:p_file;uio:p_uio;flags:Integer):Integer;
var
 td:p_kthread;
 dev:p_cdev;
 ioflag,error,ref:Integer;
 resid:Int64;
 dsw:p_cdevsw;
 fpop:p_file;
begin
 if (uio^.uio_resid > DEVFS_IOSIZE_MAX) then
  Exit(EINVAL);
 td:=curkthread;
 fpop:=td^.td_fpop;
 error:=devfs_fp_check(fp, @dev, @dsw, @ref);
 if (error<>0) then
  Exit(error);
 resid:=uio^.uio_resid;
 ioflag:=fp^.f_flag and (O_NONBLOCK or O_DIRECT);
 if ((ioflag and O_DIRECT)<>0) then
  ioflag:=ioflag or IO_DIRECT;

 foffset_lock_uio(fp, uio, flags or FOF_NOLOCK);
 error:=dsw^.d_read(dev, uio, ioflag);
 if (uio^.uio_resid<>resid) or ((error=0) and (resid<>0)) then
  vfs_timestamp(@dev^.si_atime);
 td^.td_fpop:=fpop;
 dev_relthread(dev, ref);

 foffset_unlock_uio(fp, uio, flags or FOF_NOLOCK or FOF_NEXTOFF);
 Exit(error);
end;

function devfs_readdir(ap:p_vop_readdir_args):Integer;
var
 error:Integer;
 uio:p_uio;
 dp:p_dirent;
 dd:p_devfs_dirent;
 de:p_devfs_dirent;
 dmp:p_devfs_mount;
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

 dmp:=VFSTODEVFS(ap^.a_vp^.v_mount);
 if (devfs_populate_vp(ap^.a_vp)<>0) then
 begin
  if (tmp_ncookies<>nil) then
   ap^.a_ncookies:=tmp_ncookies;
  Exit(EIO);
 end;
 error:=0;
 de:=ap^.a_vp^.v_data;
 off:=0;

 dd:=TAILQ_FIRST(@de^.de_dlist);
 while (dd<>nil) do
 begin
  Assert(ptruint(dd^.de_cdp)<>$deadc0de);
  if ((dd^.de_flags and (DE_COVERED or DE_WHITEOUT))<>0) then
  begin
   dd:=TAILQ_NEXT(dd,@dd^.de_list);
   continue;
  end;
  if (devfs_prison_check(dd)<>0) then
  begin
   dd:=TAILQ_NEXT(dd,@dd^.de_list);
   continue;
  end;
  if (dd^.de_dirent^.d_type=DT_DIR) then
   de:=dd^.de_dir
  else
   de:=dd;
  dp:=dd^.de_dirent;
  if (dp^.d_reclen > uio^.uio_resid) then
   break;
  dp^.d_fileno:=de^.de_inode;
  if (off >= uio^.uio_offset) then
  begin
   error:=vfs_read_dirent(ap, dp, off);
   if (error<>0) then
    break;
  end;
  Inc(off,dp^.d_reclen);
  dd:=TAILQ_NEXT(dd,@dd^.de_list);
 end;
 sx_xunlock(@dmp^.dm_lock);
 uio^.uio_offset:=off;

 {
  * Restore ap^.a_ncookies if it wasn't originally nil in the first
  * place.
  }
 if (tmp_ncookies<>nil) then
  ap^.a_ncookies:=tmp_ncookies;

 Exit(error);
end;

function devfs_readlink(ap:p_vop_readlink_args):Integer;
var
 de:p_devfs_dirent;
begin
 de:=ap^.a_vp^.v_data;
 Exit(uiomove(de^.de_symlink, strlen(de^.de_symlink), ap^.a_uio));
end;

function devfs_reclaim(ap:p_vop_reclaim_args):Integer;
var
 vp:p_vnode;
 de:p_devfs_dirent;
 dev:p_cdev;
begin
 vp:=ap^.a_vp;

 mtx_lock(devfs_de_interlock);
 de:=vp^.v_data;
 if (de<>nil) then
 begin
  de^.de_vnode:=nil;
  vp^.v_data:=nil;
 end;
 mtx_unlock(devfs_de_interlock);

 //vnode_destroy_vobject(vp);

 VI_LOCK(vp);
 dev_lock();
 dev:=vp^.v_rdev;
 vp^.v_rdev:=nil;

 if (dev=nil) then
 begin
  dev_unlock();
  VI_UNLOCK(vp);
  Exit(0);
 end;

 Dec(dev^.si_usecount,vp^.v_usecount);
 dev_unlock();
 VI_UNLOCK(vp);
 dev_rel(dev);
 Exit(0);
end;

function devfs_remove(ap:p_vop_remove_args):Integer;
var
 dvp,vp:p_vnode;
 dd:p_devfs_dirent;
 de,de_cov:p_devfs_dirent;
 dmp:p_devfs_mount;
begin
 dvp:=ap^.a_dvp;
 vp:=ap^.a_vp;
 dmp:=VFSTODEVFS(vp^.v_mount);

 ASSERT_VOP_ELOCKED(dvp, 'devfs_remove');
 ASSERT_VOP_ELOCKED(vp, 'devfs_remove');

 sx_xlock(@dmp^.dm_lock);
 dd:=ap^.a_dvp^.v_data;
 de:=vp^.v_data;
 if (de^.de_cdp=nil) then
 begin
  TAILQ_REMOVE(@dd^.de_dlist,de,@de^.de_list);

  if (de^.de_dirent^.d_type=DT_LNK) then
  begin
   de_cov:=devfs_find(dd, de^.de_dirent^.d_name, de^.de_dirent^.d_namlen, 0);
   if (de_cov<>nil) then
    de_cov^.de_flags:=de_cov^.de_flags and (not DE_COVERED);
  end;

  { We need to unlock dvp because devfs_delete() may lock it. }
  VOP_UNLOCK(vp, 0);
  if (dvp<>vp) then
   VOP_UNLOCK(dvp, 0);

  devfs_delete(dmp, de, 0);
  sx_xunlock(@dmp^.dm_lock);
  if (dvp<>vp) then
   vn_lock(dvp, LK_EXCLUSIVE or LK_RETRY);

  vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 end else
 begin
  de^.de_flags:=de^.de_flags or DE_WHITEOUT;
  sx_xunlock(@dmp^.dm_lock);
 end;
 Exit(0);
end;

{
 * Revoke is called on a tty when a terminal session ends.  The vnode
 * is orphaned by setting v_op to deadfs so we need to let go of it
 * as well so that we create a new one next time around.
 *
 }
function devfs_revoke(ap:p_vop_revoke_args):Integer;
label
 loop;
var
 vp,vp2:p_vnode;
 dev:p_cdev;
 cdp:p_cdev_priv;
 de:p_devfs_dirent;
 i:DWORD;
begin
 vp:=ap^.a_vp;

 Assert((ap^.a_flags and REVOKEALL)<>0,'devfs_revoke !REVOKEALL');

 dev:=vp^.v_rdev;
 cdp:=cdev2priv(dev);

 dev_lock();
 Inc(cdp^.cdp_inuse);
 dev_unlock();

 vhold(vp);
 vgone(vp);
 vdrop(vp);

 VOP_UNLOCK(vp,0);
 loop:
 repeat
  mtx_lock(devfs_de_interlock);
  dev_lock();
  vp2:=nil;
  For i:=0 to cdp^.cdp_maxdirent do
  begin
   de:=cdp^.cdp_dirents[i];
   if (de=nil) then
    continue;

   vp2:=de^.de_vnode;
   if (vp2<>nil) then
   begin
    dev_unlock();
    VI_LOCK(vp2);
    mtx_unlock(devfs_de_interlock);
    if (vget(vp2, LK_EXCLUSIVE or LK_INTERLOCK)<>0) then
     goto loop;
    vhold(vp2);
    vgone(vp2);
    vdrop(vp2);
    vput(vp2);
    break;
   end;
  end;
  if (vp2<>nil) then
  begin
   continue;
  end;
  dev_unlock();
  mtx_unlock(devfs_de_interlock);
  break;
 until false;
 dev_lock();
 Dec(cdp^.cdp_inuse);
 if ((cdp^.cdp_flags and CDP_ACTIVE)=0) and (cdp^.cdp_inuse=0) then
 begin
  TAILQ_REMOVE(@cdevp_list,cdp,@cdp^.cdp_list);
  dev_unlock();
  dev_rel(@cdp^.cdp_c);
 end else
  dev_unlock();

 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 Exit(0);
end;

function devfs_rioctl(ap:p_vop_ioctl_args):Integer;
var
 vp:p_vnode;
 dmp:p_devfs_mount;
 error:Integer;
begin
 vp:=ap^.a_vp;
 vn_lock(vp, LK_SHARED or LK_RETRY);
 if ((vp^.v_iflag and VI_DOOMED)<>0) then
 begin
  VOP_UNLOCK(vp, 0);
  Exit(EBADF);
 end;
 dmp:=VFSTODEVFS(vp^.v_mount);
 sx_xlock(@dmp^.dm_lock);
 VOP_UNLOCK(vp, 0);
 DEVFS_DMP_HOLD(dmp);
 devfs_populate(dmp);
 if DEVFS_DMP_DROP(dmp) then
 begin
  sx_xunlock(@dmp^.dm_lock);
  devfs_unmount_final(dmp);
  Exit(ENOENT);
 end;
 error:=devfs_rules_ioctl(dmp, ap^.a_command, ap^.a_data);
 sx_xunlock(@dmp^.dm_lock);
 Exit(error);
end;

function devfs_rread(ap:p_vop_read_args):Integer;
begin
 if (ap^.a_vp^.v_type<>VDIR) then
  Exit(EINVAL);
 Exit(VOP_READDIR(ap^.a_vp, ap^.a_uio, nil, nil, nil));
end;

function devfs_setattr(ap:p_vop_setattr_args):Integer;
var
 de:p_devfs_dirent;
 vap:p_vattr;
 vp:p_vnode;
 c,error:Integer;
 uid:uid_t;
 gid:gid_t;
begin
 vap:=ap^.a_vap;
 vp:=ap^.a_vp;

 if (vap^.va_type<>VNON) or
    (vap^.va_nlink<>WORD(VNOVAL)) or
    (vap^.va_fsid<>VNOVAL) or
    (vap^.va_fileid<>QWORD(VNOVAL)) or
    (vap^.va_blocksize<>QWORD(VNOVAL)) or
    ((vap^.va_flags<>QWORD(VNOVAL)) and (vap^.va_flags<>0)) or
    (vap^.va_rdev<>VNOVAL) or
    (vap^.va_bytes<>QWORD(VNOVAL)) or
    (vap^.va_gen<>QWORD(VNOVAL)) then
 begin
  Exit(EINVAL);
 end;

 de:=vp^.v_data;
 if (vp^.v_type=VDIR) then
  de:=de^.de_dir;

 error:=0;
 c:=0;

 if (vap^.va_uid=uid_t(VNOVAL)) then
  uid:=de^.de_uid
 else
  uid:=vap^.va_uid;

 if (vap^.va_gid=gid_t(VNOVAL)) then
  gid:=de^.de_gid
 else
  gid:=vap^.va_gid;

 if (uid<>de^.de_uid) or (gid<>de^.de_gid) then
 begin
  //if ((ap^.a_cred^.cr_uid<>de^.de_uid) or uid<>de^.de_uid or
  //    (gid<>de^.de_gid and !groupmember(gid, ap^.a_cred))) then
  //begin
  // error:=priv_check(td, PRIV_VFS_CHOWN);
  // if (error<>) then
  //  Exit(error);
  //end;
  de^.de_uid:=uid;
  de^.de_gid:=gid;
  c:=1;
 end;

 if (vap^.va_mode<>WORD(VNOVAL)) then
 begin
  //if (ap^.a_cred^.cr_uid<>de^.de_uid) then
  //begin
  // error:=priv_check(td, PRIV_VFS_ADMIN);
  // if (error<>0) then
  //  Exit(error);
  //end;
  de^.de_mode:=vap^.va_mode;
  c:=1;
 end;

 if (vap^.va_atime.tv_sec<>VNOVAL) or (vap^.va_mtime.tv_sec<>VNOVAL) then
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
   if (vp^.v_type=VCHR) then
    p_cdev(vp^.v_rdev)^.si_atime:=vap^.va_atime
   else
    de^.de_atime:=vap^.va_atime;
  end;
  if (vap^.va_mtime.tv_sec<>VNOVAL) then
  begin
   if (vp^.v_type=VCHR) then
    p_cdev(vp^.v_rdev)^.si_mtime:=vap^.va_mtime
   else
    de^.de_mtime:=vap^.va_mtime;
  end;
  c:=1;
 end;

 if (c<>0) then
 begin
  if (vp^.v_type=VCHR) then
   vfs_timestamp(@p_cdev(vp^.v_rdev)^.si_ctime)
  else
   vfs_timestamp(@de^.de_mtime);
 end;
 Exit(0);
end;

function devfs_stat_f(fp:p_file;sb:p_stat):Integer;
begin
 Exit(vnops.fo_stat(fp, sb));
end;

function devfs_symlink(ap:p_vop_symlink_args):Integer;
var
 i, error:Integer;
 dd:p_devfs_dirent;
 de,de_cov,de_dotdot:p_devfs_dirent;
 dmp:p_devfs_mount;
begin
 error:=0;
 //error:=priv_check(curkthread, PRIV_DEVFS_SYMLINK);
 if (error<>0) then
  Exit(error);

 dmp:=VFSTODEVFS(ap^.a_dvp^.v_mount);

 if (devfs_populate_vp(ap^.a_dvp)<>0) then
  Exit(ENOENT);

 de_cov:=devfs_find(dd, ap^.a_cnp^.cn_nameptr, ap^.a_cnp^.cn_namelen, 0);

 if (de_cov<>nil) then
 begin
  if ((de_cov^.de_flags and DE_USER)<>0) then
  begin
   sx_xunlock(@dmp^.dm_lock);
   Exit(EEXIST);
  end;

  Assert((de_cov^.de_flags and DE_COVERED)=0,'devfs_symlink: entry %p already covered');
  de_cov^.de_flags:=de_cov^.de_flags or DE_COVERED;
 end;

 dd:=ap^.a_dvp^.v_data;
 de:=devfs_newdirent(ap^.a_cnp^.cn_nameptr, ap^.a_cnp^.cn_namelen);
 de^.de_flags:=DE_USER;
 de^.de_uid  :=0;
 de^.de_gid  :=0;
 de^.de_mode :=&0755;
 de^.de_inode:=devfs_alloc_cdp_inode;
 de^.de_dir  :=dd;
 de^.de_dirent^.d_type:=DT_LNK;

 i:=strlen(ap^.a_target) + 1;
 de^.de_symlink:=AllocMem(i);
 Move(ap^.a_target^, de^.de_symlink^, i);

 //mac_devfs_create_symlink(ap^.a_cnp^.cn_cred, dmp^.dm_mount, dd, de);

 de_dotdot:=TAILQ_FIRST(@dd^.de_dlist);  { '.' }
 de_dotdot:=TAILQ_NEXT(de_dotdot,@de_dotdot^.de_list); { '..' }
 TAILQ_INSERT_AFTER(@dd^.de_dlist,de_dotdot,de,@de^.de_list);
 devfs_dir_ref_de(dmp, dd);
 devfs_rules_apply(dmp, de);

 Exit(devfs_allocv(de, ap^.a_dvp^.v_mount, LK_EXCLUSIVE, ap^.a_vpp));
end;

function devfs_truncate_f(fp:p_file;length:Int64):Integer;
begin
 Exit(vnops.fo_truncate(fp, length));
end;

function devfs_write_f(fp:p_file;uio:p_uio;flags:Integer):Integer;
var
 td:p_kthread;
 dev:p_cdev;
 error,ioflag,ref:Integer;
 resid:Int64;
 dsw:p_cdevsw;
 fpop:p_file;
begin
 td:=curkthread;
 if (uio^.uio_resid > DEVFS_IOSIZE_MAX) then
  Exit(EINVAL);
 fpop:=td^.td_fpop;
 error:=devfs_fp_check(fp, @dev, @dsw, @ref);
 if (error<>0) then
  Exit(error);
 Assert(uio^.uio_td=td, 'uio_td %p is not td %p');
 ioflag:=fp^.f_flag and (O_NONBLOCK or O_DIRECT or O_FSYNC);
 if ((ioflag and O_DIRECT)<>0) then
  ioflag:=ioflag or IO_DIRECT;
 foffset_lock_uio(fp, uio, flags or FOF_NOLOCK);

 resid:=uio^.uio_resid;

 error:=dsw^.d_write(dev, uio, ioflag);
 if (uio^.uio_resid<>resid) or ((error=0) and (resid<>0)) then
 begin
  vfs_timestamp(@dev^.si_ctime);
  dev^.si_mtime:=dev^.si_ctime;
 end;
 td^.td_fpop:=fpop;
 dev_relthread(dev, ref);

 foffset_unlock_uio(fp, uio, flags or FOF_NOLOCK or FOF_NEXTOFF);
 Exit(error);
end;

function dev2udev(x:p_cdev):Integer;
begin
 if (x=nil) then
  Exit(NODEV);

 Exit(cdev2priv(x)^.cdp_inode);
end;


end.

