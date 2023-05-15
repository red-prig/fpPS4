unit null_vnops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vnode,
 vnode_if,
 vfs_default;

function null_bypass(ap:p_vop_generic_args):Integer;
function null_lookup(ap:p_vop_lookup_args):Integer;
function null_open(ap:p_vop_open_args):Integer;
function null_setattr(ap:p_vop_setattr_args):Integer;
function null_getattr(ap:p_vop_getattr_args):Integer;
function null_access(ap:p_vop_access_args):Integer;
function null_accessx(ap:p_vop_accessx_args):Integer;
function null_remove(ap:p_vop_remove_args):Integer;
function null_rename(ap:p_vop_rename_args):Integer;
function null_rmdir(ap:p_vop_rmdir_args):Integer;
function null_lock(ap:p_vop_lock1_args):Integer;
function null_unlock(ap:p_vop_unlock_args):Integer;
function null_inactive(ap:p_vop_inactive_args):Integer;
function null_reclaim(ap:p_vop_reclaim_args):Integer;
function null_print(ap:p_vop_print_args):Integer;
function null_getwritemount(ap:p_vop_getwritemount_args):Integer;
function null_vptofh(ap:p_vop_vptofh_args):Integer;
function null_vptocnp(ap:p_vop_vptocnp_args):Integer;
function null_link(ap:p_vop_link_args):Integer;

const
 null_vnodeops:vop_vector=(
  vop_default       :nil;
  vop_bypass        :@null_bypass;

  vop_islocked      :@vop_stdislocked;
  vop_lookup        :@null_lookup;
  vop_create        :nil;
  vop_whiteout      :nil;
  vop_mknod         :nil;
  vop_open          :@null_open;
  vop_close         :nil;
  vop_access        :@null_access;
  vop_accessx       :@null_accessx;
  vop_getattr       :@null_getattr;
  vop_setattr       :@null_setattr;
  vop_markatime     :nil;
  vop_read          :nil;
  vop_write         :nil;
  vop_ioctl         :nil;
  vop_poll          :nil;
  vop_kqfilter      :nil;
  vop_revoke        :nil;
  vop_fsync         :nil;
  vop_remove        :@null_remove;
  vop_link          :@null_link;
  vop_rename        :@null_rename;
  vop_mkdir         :nil;
  vop_rmdir         :@null_rmdir;
  vop_symlink       :nil;
  vop_readdir       :nil;
  vop_readlink      :nil;
  vop_inactive      :@null_inactive;
  vop_reclaim       :@null_reclaim;
  vop_lock1         :@null_lock;
  vop_unlock        :@null_unlock;
  vop_bmap          :@VOP_EOPNOTSUPP;
  vop_strategy      :@VOP_EOPNOTSUPP;
  vop_getwritemount :@null_getwritemount;
  vop_print         :@null_print;
  vop_pathconf      :nil;
  vop_advlock       :nil;
  vop_advlockasync  :nil;
  vop_advlockpurge  :@vop_stdadvlockpurge;
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
  vop_vptofh        :@null_vptofh;
  vop_vptocnp       :@null_vptocnp;
  vop_allocate      :nil;
  vop_unp_bind      :nil;
  vop_unp_connect   :nil;
  vop_unp_detach    :nil;
 );

implementation

uses
 sysutils,
 nullfs,
 null_subr,
 errno,
 vnamei,
 vmount,
 vfs_subr,
 vfs_vnops,
 vfs_cache,
 kern_mtx;

type
 ppp_vnode=^pp_vnode;

function MOUNTTONULLMOUNT(mp:p_mount):p_null_mount; inline;
begin
 Result:=mp^.mnt_data;
end;

function VTONULL(vp:p_vnode):p_null_node; inline;
begin
 Result:=vp^.v_data;
end;

function NULLTOV(xp:p_null_node):p_vnode; inline;
begin
 Result:=xp^.null_vnode;
end;

function null_bypass(ap:p_vop_generic_args):Integer;
label
 _out,
 _nxt,
 _err;
var
 this_vp_p:pp_vnode;
 error:Integer;
 old_vps:array[0..VDESC_MAX_VPS-1] of p_vnode;
 vps_p  :array[0..VDESC_MAX_VPS-1] of pp_vnode;
 vppp:ppp_vnode;
 descp:p_vnodeop_desc;
 reles,i:Integer;
begin
 descp:=ap^.a_desc;

 {
  * Map the vnodes going in.
  * Later, we'll invoke the operation based on
  * the first mapped vnode's operation vector.
  }
 reles:=descp^.vdesc_flags;
 i:=0;
 while (i < VDESC_MAX_VPS) do
 begin
  if (descp^.vdesc_vp_offsets[i]=Byte(-1)) then
   break;   { bail out at end of list }

  this_vp_p:=VOPARG_OFFSETTO(descp^.vdesc_vp_offsets[i],ap);
  vps_p[i]:=this_vp_p;
  {
   * We're not guaranteed that any but the first vnode
   * are of our type.  Check for and don't map any
   * that aren't.  (We must always map first vp or vclean fails.)
   }
  if (i<>0) then
  begin
   if (this_vp_p^=nil) then
   begin
    old_vps[i]:=nil;
    goto _nxt;
   end;
   if (this_vp_p^^.v_op<>@null_vnodeops) then
   begin
    old_vps[i]:=nil;
    goto _nxt;
   end;
  end;

  old_vps[i]:=this_vp_p^;
  vps_p[i]^:=NULLVPTOLOWERVP(this_vp_p^);
  {
   * XXX - Several operations have the side effect
   * of vrele'ing their vp's.  We must account for
   * that.  (This should go away in the future.)
   }
  if ((reles and VDESC_VP0_WILLRELE)<>0) then
   VREF(this_vp_p^);

  //
  _nxt:
  reles:=reles shr 1;
  Inc(i);
 end;

 {
  * Call the operation on the lower layer
  * with the modified argument structure.
  }
 if (vps_p[0]=nil) then goto _err;

 if (vps_p[0]^<>nil) then
 begin
  error:=VCALL(ap);
 end else
 begin
  _err:
  Writeln(Format('null_bypass: no map for %s',[descp^.vdesc_name]));
  error:=EINVAL;
 end;

 {
  * Maintain the illusion of call-by-value
  * by restoring vnodes in the argument structure
  * to their original value.
  }
 reles:=descp^.vdesc_flags;
 i:=0;
 while (i < VDESC_MAX_VPS) do
 begin
  if (descp^.vdesc_vp_offsets[i]=Byte(-1)) then
   break;   { bail out at end of list }
  if (old_vps[i]<>nil) then
  begin
   vps_p[i]^:=old_vps[i];
   if ((reles and VDESC_VP0_WILLRELE)<>0) then
    vrele((vps_p[i])^);
  end;
  //
  reles:=reles shr 1;
  Inc(i);
 end;

 {
  * Map the possible out-going vpp
  * (Assumes that the lower layer always Exits
  * a VREF'ed vpp unless it gets an error.)
  }
 if (descp^.vdesc_vpp_offset<>VDESC_NO_OFFSET) and
    ((descp^.vdesc_flags and VDESC_NOMAP_VPP)=0) and
    (error=0) then
 begin
  {
   * XXX - even though some ops have vpp Exited vp's,
   * several ops actually vrele this before Exiting.
   * We must avoid these ops.
   * (This should go away when these ops are regularized.)
   }
  if ((descp^.vdesc_flags and VDESC_VPP_WILLRELE)<>0) then
   goto _out;

  vppp:=VOPARG_OFFSETTO(descp^.vdesc_vpp_offset,ap);

  if (vppp^<>nil) then
   error:=null_nodeget(old_vps[0]^.v_mount,vppp^^,vppp^);
 end;

_out:
 Exit(error);
end;

{
 * We have to carry on the locking protocol on the null layer vnodes
 * as we progress through the tree. We also have to enforce read-only
 * if this layer is mounted read-only.
 }
function null_lookup(ap:p_vop_lookup_args):Integer;
var
 cnp:p_componentname;
 dvp:p_vnode;
 flags:Integer;
 vp,ldvp,lvp:p_vnode;
 error:Integer;
begin
 cnp:=ap^.a_cnp;
 dvp:=ap^.a_dvp;
 flags:=cnp^.cn_flags;

 if ((flags and ISLASTCN)<>0) and
    ((p_mount(dvp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0) and
    ((cnp^.cn_nameiop=DELETE) or (cnp^.cn_nameiop=RENAME)) then
  Exit(EROFS);
 {
  * Although it is possible to call null_bypass(), we'll do
  * a direct call to reduce overhead
  }
 ldvp:=NULLVPTOLOWERVP(dvp);
 vp:=nil;
 lvp:=nil;

 if (ldvp<>nil) then
 begin
  error:=VOP_LOOKUP(ldvp, @lvp, cnp);
 end else
 begin
  error:=ENOENT;
 end;

 if (error=EJUSTRETURN) and
    ((flags and ISLASTCN)<>0) and
    ((p_mount(dvp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0) and
    ((cnp^.cn_nameiop=CREATE) or (cnp^.cn_nameiop=RENAME)) then
  error:=EROFS;

 if ((error=0) or (error=EJUSTRETURN)) and (lvp<>nil) then
 begin
  if (ldvp=lvp) then
  begin
   ap^.a_vpp^:=dvp;
   VREF(dvp);
   vrele(lvp);
  end else
  begin
   error:=null_nodeget(dvp^.v_mount, lvp, @vp);
   if (error=0) then
    ap^.a_vpp^:=vp;
  end;
 end;
 Exit(error);
end;

function null_open(ap:p_vop_open_args):Integer;
var
 retval:Integer;
 vp,ldvp:p_vnode;
begin
 vp:=ap^.a_vp;
 ldvp:=NULLVPTOLOWERVP(vp);

 if (ldvp=nil) then Exit(0);

 retval:=null_bypass(Pointer(ap));
 //if (retval=0) then
 // vp^.v_object:=ldvp^.v_object;
 Exit(retval);
end;

{
 * Setattr call. Disallow write attempts if the layer is mounted read-only.
 }
function null_setattr(ap:p_vop_setattr_args):Integer;
var
 vp:p_vnode;
 vap:p_vattr;
begin
 vp:=ap^.a_vp;
 vap:=ap^.a_vap;

 if ((vap^.va_flags<>QWORD(VNOVAL)) or
     (vap^.va_uid<>Integer(VNOVAL)) or
     (vap^.va_gid<>Integer(VNOVAL)) or
     (vap^.va_atime.tv_sec<>VNOVAL) or
     (vap^.va_mtime.tv_sec<>VNOVAL) or
     (vap^.va_mode<>Word(VNOVAL))
    ) and
    ((p_mount(vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0) then
  Exit(EROFS);

 if (vap^.va_size<>QWORD(VNOVAL)) then
 begin
  case (vp^.v_type) of
   VDIR:
    begin
     Exit(EISDIR);
    end;
   VCHR,
   VBLK,
   VSOCK,
   VFIFO:
    begin
     if (vap^.va_flags<>QWORD(VNOVAL)) then
      Exit(EOPNOTSUPP);
     Exit(0);
    end;
   else
    begin
     {
      * Disallow write attempts if the filesystem is
      * mounted read-only.
      }
     if ((p_mount(vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0) then
      Exit(EROFS);
    end;
  end;
 end;

 if (NULLVPTOLOWERVP(vp)=nil) then Exit(0);

 Exit(null_bypass(Pointer(ap)));
end;

{
 *  We handle getattr only to change the fsid.
 }
function null_getattr(ap:p_vop_getattr_args):Integer;
var
 error:Integer;
begin
 if (NULLVPTOLOWERVP(ap^.a_vp)=nil) then Exit(0);

 error:=null_bypass(Pointer(ap));

 if (error<>0) then
  Exit(error);

 ap^.a_vap^.va_fsid:=p_mount(ap^.a_vp^.v_mount)^.mnt_stat.f_fsid.val[0];
 Exit(0);
end;

{
 * Handle to disallow write access if mounted read-only.
 }
function null_access(ap:p_vop_access_args):Integer;
var
 vp:p_vnode;
 accmode:accmode_t;
begin
 vp:=ap^.a_vp;
 accmode:=ap^.a_accmode;

 {
  * Disallow write attempts on read-only layers;
  * unless the file is a socket, fifo, or a block or
  * character device resident on the filesystem.
  }
 if ((accmode and VWRITE)<>0) then
 begin
  case (vp^.v_type) of
   VDIR,
   VLNK,
   VREG:
    begin
     if ((p_mount(vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0) then
      Exit(EROFS);
    end;
   else;
  end;
 end;

 if (NULLVPTOLOWERVP(vp)=nil) then Exit(0);

 Exit(null_bypass(Pointer(ap)));
end;

function null_accessx(ap:p_vop_accessx_args):Integer;
var
 vp:p_vnode;
 accmode:accmode_t;
begin
 vp:=ap^.a_vp;
 accmode:=ap^.a_accmode;

 {
  * Disallow write attempts on read-only layers;
  * unless the file is a socket, fifo, or a block or
  * character device resident on the filesystem.
  }
 if ((accmode and VWRITE)<>0) then
 begin
  case (vp^.v_type) of
   VDIR,
   VLNK,
   VREG:
    begin
     if ((p_mount(vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0) then
      Exit(EROFS);
    end;
   else;
  end;
 end;

 if (NULLVPTOLOWERVP(vp)=nil) then Exit(0);

 Exit(null_bypass(Pointer(ap)));
end;

{
 * Increasing refcount of lower vnode is needed at least for the case
 * when lower FS is NFS to do sillyrename if the file is in use.
 * Unfortunately v_usecount is incremented in many places in
 * the kernel and, as such, there may be races that result in
 * the NFS client doing an extraneous silly rename, but that seems
 * preferable to not doing a silly rename when it is needed.
 }
function null_remove(ap:p_vop_remove_args):Integer;
var
 retval,vreleit:Integer;
 lvp,vp:p_vnode;
 tnn:p_null_node;
begin
 vp:=ap^.a_vp;
 if (vrefcnt(vp) > 1) then
 begin
  lvp:=NULLVPTOLOWERVP(vp);
  if (lvp<>nil) then
   VREF(lvp);
  vreleit:=1;
 end else
  vreleit:=0;

 if (lvp<>nil) then
 begin
  tnn:=VTONULL(vp);
  tnn^.null_flags:=tnn^.null_flags or NULLV_DROP;

  retval:=null_bypass(Pointer(ap));

  if (vreleit<>0) then
   vrele(lvp);
 end else
 begin
  retval:=0;
 end;

 Exit(retval);
end;

{
 * We handle this to eliminate null FS to lower FS
 * file moving. Don't know why we don't allow this,
 * possibly we should.
 }
function null_rename(ap:p_vop_rename_args):Integer;
label
 _cross;
var
 tdvp,fvp,fdvp,tvp:p_vnode;
 tnn:p_null_node;
begin
 tdvp:=ap^.a_tdvp;
 fvp:=ap^.a_fvp;
 fdvp:=ap^.a_fdvp;
 tvp:=ap^.a_tvp;

 if (tvp<>nil) then
 begin
  if (fvp^.v_mount<>tvp^.v_mount) then goto _cross;
 end;

 { Check for cross-device rename. }
 if (fvp^.v_mount<>tdvp^.v_mount) then
 begin
  _cross:
  if (tdvp=tvp) then
   vrele(tdvp)
  else
   vput(tdvp);

  if (tvp<>nil) then
   vput(tvp);
  vrele(fdvp);
  vrele(fvp);
  Exit(EXDEV);
 end;

 if (tvp<>nil) then
 begin
  tnn:=VTONULL(tvp);
  tnn^.null_flags:=tnn^.null_flags or NULLV_DROP;
 end;

 if (tnn^.null_lowervp=nil) then Exit(0);

 Exit(null_bypass(Pointer(ap)));
end;

function null_rmdir(ap:p_vop_rmdir_args):Integer;
var
 tnn:p_null_node;
begin
 tnn:=VTONULL(ap^.a_vp);

 if (tnn^.null_lowervp=nil) then Exit(0);

 tnn^.null_flags:=tnn^.null_flags or NULLV_DROP;
 Exit(null_bypass(Pointer(ap)));
end;

{
 * We need to process our own vnode lock and then clear the
 * interlock flag as it applies only to our vnode, not the
 * vnodes below us on the stack.
 }
function null_lock(ap:p_vop_lock1_args):Integer;
var
 vp:p_vnode;
 flags:Integer;
 nn:p_null_node;
 lvp:p_vnode;
 error:Integer;
begin
 vp:=ap^.a_vp;
 flags:=ap^.a_flags;

 if ((flags and LK_INTERLOCK)=0) then
 begin
  VI_LOCK(vp);
  flags:=flags or LK_INTERLOCK;
  ap^.a_flags:=flags;
 end;
 nn:=VTONULL(vp);
 {
  * If we're still active we must ask the lower layer to
  * lock as ffs has special lock considerations in it's
  * vop lock.
  }
 if (nn<>nil) then
 begin
  lvp:=NULLVPTOLOWERVP(vp);
  if (lvp<>nil) then
  begin
   VI_LOCK(lvp);
   VI_UNLOCK(vp);
   {
    * We have to hold the vnode here to solve a potential
    * reclaim race.  If we're forcibly vgone'd while we
    * still have refs, a thread could be sleeping inside
    * the lowervp's vop_lock routine.  When we vgone we will
    * drop our last ref to the lowervp, which would allow it
    * to be reclaimed.  The lowervp could then be recycled,
    * in which case it is not legal to be sleeping in it's VOP.
    * We prevent it from being recycled by holding the vnode
    * here.
    }
   vholdl(lvp);
   error:=VOP_LOCK(lvp,flags,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%});

   {
    * We might have slept to get the lock and someone might have
    * clean our vnode already, switching vnode lock from one in
    * lowervp to v_lock in our own vnode structure.  Handle this
    * case by reacquiring correct lock in requested mode.
    }
   if (VTONULL(vp)=nil) and (error=0) then
   begin
    ap^.a_flags:=ap^.a_flags and (not (LK_TYPE_MASK or LK_INTERLOCK));
    case (flags and LK_TYPE_MASK) of
     LK_SHARED:
      begin
       ap^.a_flags:=ap^.a_flags or LK_SHARED;
      end;
     LK_UPGRADE,
     LK_EXCLUSIVE:
      begin
       ap^.a_flags:=ap^.a_flags or LK_EXCLUSIVE;
      end;
     else
      Assert(False,'Unsupported lock request');
    end;
    VOP_UNLOCK(lvp, 0);
    error:=vop_stdlock(ap);
   end;
   vdrop(lvp);
  end else
   error:=vop_stdlock(ap);
 end else
  error:=vop_stdlock(ap);

 Exit(error);
end;

{
 * We need to process our own vnode unlock and then clear the
 * interlock flag as it applies only to our vnode, not the
 * vnodes below us on the stack.
 }
function null_unlock(ap:p_vop_unlock_args):Integer;
label
 _else;
var
 vp:p_vnode;
 flags:Integer;
 mtxlkflag:Integer;
 nn:p_null_node;
 lvp:p_vnode;
 error:Integer;
begin
 vp:=ap^.a_vp;
 flags:=ap^.a_flags;
 mtxlkflag:=0;

 if ((flags and LK_INTERLOCK)<>0) then
  mtxlkflag:=1
 else
 if (not mtx_owned(VI_MTX(vp)^)) then
 begin
  VI_LOCK(vp);
  mtxlkflag:=2;
 end;
 nn:=VTONULL(vp);
 if (nn<>nil) then
 begin
  lvp:=NULLVPTOLOWERVP(vp);
  if (lvp=nil) then goto _else;

  VI_LOCK(lvp);
  flags:=flags or LK_INTERLOCK;
  vholdl(lvp);
  VI_UNLOCK(vp);
  error:=VOP_UNLOCK(lvp, flags);
  vdrop(lvp);
  if (mtxlkflag=0) then
   VI_LOCK(vp);
 end else
 begin
  _else:
  if (mtxlkflag=2) then
   VI_UNLOCK(vp);
  error:=vop_stdunlock(ap);
 end;

 Exit(error);
end;

{
 * Do not allow the VOP_INACTIVE to be passed to the lower layer,
 * since the reference count on the lower vnode is not related to
 * ours.
 }
function null_inactive(ap:p_vop_inactive_args):Integer;
var
 vp,lvp:p_vnode;
 xp:p_null_node;
 mp:p_mount;
 xmp:p_null_mount;
begin
 vp:=ap^.a_vp;
 xp:=VTONULL(vp);
 lvp:=NULLVPTOLOWERVP(vp);

 if (lvp=nil) then Exit(0);

 mp:=vp^.v_mount;
 xmp:=MOUNTTONULLMOUNT(mp);

 if ((xmp^.nullm_flags and NULLM_CACHE)=0) or
    ((xp^.null_flags and NULLV_DROP)<>0) or
    ((lvp^.v_vflag and VV_NOSYNC)<>0) then
 begin
  {
   * If this is the last reference and caching of the
   * nullfs vnodes is not enabled, or the lower vnode is
   * deleted, then free up the vnode so as not to tie up
   * the lower vnodes.
   }
  //vp^.v_object:=nil;
  vrecycle(vp);
 end;
 Exit(0);
end;

{
 * Now, the nullfs vnode and, due to the sharing lock, the lower
 * vnode, are exclusively locked, and we shall destroy the null vnode.
 }
function null_reclaim(ap:p_vop_reclaim_args):Integer;
var
 vp:p_vnode;
 xp:p_null_node;
 lowervp:p_vnode;
begin
 vp:=ap^.a_vp;
 xp:=VTONULL(vp);
 lowervp:=xp^.null_lowervp;

 Assert((vp^.v_vnlock<>@vp^.v_lock),'Reclaiming incomplete null vnode');

 null_hashrem(xp);
 {
  * Use the interlock to protect the clearing of v_data to
  * prevent faults in null_lock().
  }
 mtx_lock(vp^.v_lock);
 //lockmgr(@vp^.v_lock, LK_EXCLUSIVE, nil);

 VI_LOCK(vp);
 vp^.v_data:=nil;
 //vp^.v_object:=nil;
 vp^.v_vnlock:=@vp^.v_lock;
 VI_UNLOCK(vp);

 if (lowervp<>nil) then
 begin
  if ((xp^.null_flags and NULLV_NOUNLOCK)<>0) then
   vunref(lowervp)
  else
   vput(lowervp);
 end;

 FreeMem(xp);

 Exit(0);
end;

function null_print(ap:p_vop_print_args):Integer;
var
 vp:p_vnode;
begin
 vp:=ap^.a_vp;

 Writeln(Format('vp=%p, lowervp=%p', [vp,VTONULL(vp)^.null_lowervp]));
 Exit(0);
end;

{ ARGSUSED }
function null_getwritemount(ap:p_vop_getwritemount_args):Integer;
label
 _else;
var
 xp:p_null_node;
 lowervp:p_vnode;
 vp:p_vnode;
begin
 vp:=ap^.a_vp;
 VI_LOCK(vp);
 xp:=VTONULL(vp);
 if (xp<>nil) then
 begin
  lowervp:=xp^.null_lowervp;
  if (lowervp=nil) then goto _else;

  VI_LOCK(lowervp);
  VI_UNLOCK(vp);
  vholdl(lowervp);
  VI_UNLOCK(lowervp);
  VOP_GETWRITEMOUNT(lowervp, ap^.a_mpp);
  vdrop(lowervp);
 end else
 begin
  _else:
  VI_UNLOCK(vp);
  ap^.a_mpp^:=nil;
 end;
 Exit(0);
end;

function null_vptofh(ap:p_vop_vptofh_args):Integer;
var
 lvp:p_vnode;
begin
 lvp:=NULLVPTOLOWERVP(ap^.a_vp);

 if (lvp=nil) then Exit(EOPNOTSUPP);

 Exit(VOP_VPTOFH(lvp, ap^.a_fhp));
end;

function null_vptocnp(ap:p_vop_vptocnp_args):Integer;
var
 vp:p_vnode;
 dvp:pp_vnode;
 lvp,ldvp:p_vnode;
 error,locked:Integer;
begin
 vp:=ap^.a_vp;
 dvp:=ap^.a_vpp;

 if (vp^.v_type=VDIR) then
  Exit(vop_stdvptocnp(ap));

 locked:=VOP_ISLOCKED(vp);
 lvp:=NULLVPTOLOWERVP(vp);

 if (lvp=nil) then
 begin
  ap^.a_buf[0]:='/';
  ap^.a_buf[1]:=#0;
  ap^.a_buflen^:=1;
  Exit(0);
 end;

 vhold(lvp);
 VOP_UNLOCK(vp, 0); { vp is held by vn_vptocnp_locked that called us }
 ldvp:=lvp;
 vref(lvp);
 error:=vn_vptocnp(@ldvp, ap^.a_buf, PDWORD(ap^.a_buflen));
 vdrop(lvp);
 if (error<>0) then
 begin
  vn_lock(vp, locked or LK_RETRY);
  Exit(ENOENT);
 end;

 {
  * Exclusive lock is required by insmntque1 call in
  * null_nodeget()
  }
 error:=vn_lock(ldvp, LK_EXCLUSIVE);
 if (error<>0) then
 begin
  vrele(ldvp);
  vn_lock(vp, locked or LK_RETRY);
  Exit(ENOENT);
 end;
 vref(ldvp);
 error:=null_nodeget(vp^.v_mount, ldvp, dvp);
 if (error=0) then
 begin
  VOP_UNLOCK(dvp^, 0); { keep reference on *dvp }
 end;
 vn_lock(vp, locked or LK_RETRY);
 Exit(error);
end;

function null_link(ap:p_vop_link_args):Integer;
begin
 if (ap^.a_tdvp^.v_mount<>ap^.a_vp^.v_mount) then
  Exit(EXDEV);

 if (NULLVPTOLOWERVP(ap^.a_vp)=nil) then Exit(0);

 Exit(null_bypass(Pointer(ap)));
end;



end.

