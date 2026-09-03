unit union_vnops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 errno,
 unionfs,
 vuio,
 vnode,
 vnamei,
 vmount,
 vstat,
 vfcntl,
 vnode_if,
 vfs_vnops,
 vfs_subr,
 vfs_default,
 kern_param;

implementation

uses
 kern_mtx,
 kern_malloc;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

procedure KASSERT_UNIONFS_VNODE(vp:p_vnode); inline;
begin
 Assert(vp^.v_op=@unionfs_vnodeops,'unionfs: it is not unionfs-vnode');
end;

function unionfs_lookup(ap:p_vop_lookup_args):Integer;
label
 _unionfs_lookup_out;
var
 iswhiteout:Integer;
 lockflag:Integer;
 error,uerror,lerror:Integer;
 nameiop:Integer;
 cnflags:QWORD;
 cnflagsbk:QWORD;
 dunp:p_unionfs_node;
 dvp,udvp,ldvp,vp,uvp,lvp,dtmpvp:p_vnode;
 va:t_vattr;
 cnp:p_componentname;
begin
 iswhiteout:=0;
 lockflag:=0;
 error :=ENOENT;
 uerror:=ENOENT;
 lerror:=ENOENT;
 cnp:=ap^.a_cnp;
 nameiop:=cnp^.cn_nameiop;
 cnflags:=cnp^.cn_flags;
 dvp:=ap^.a_dvp;
 dunp:=VTOUNIONFS(dvp);
 udvp:=dunp^.un_uppervp;
 ldvp:=dunp^.un_lowervp;
 vp :=nil;
 uvp:=nil;
 lvp:=nil;
 (ap^.a_vpp)^:=nil;

 LOG_DEBUG('unionfs_lookup: enter: nameiop=', nameiop, ' flags=', cnflags,' path=', cnp^.cn_nameptr);

 if (dvp^.v_type<>VDIR) then Exit(ENOTDIR);

 {
  * If read-only and op is not LOOKUP, will return EROFS.
  }
 if ((cnflags and ISLASTCN)<>0) AND
    ((p_mount(dvp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0) AND
    (LOOKUP<>nameiop) then
 begin
  Exit(EROFS);
 end;

 {
  * lookup dotdot
  }
 if (cnflags and ISDOTDOT)<>0 then
 begin
  if (LOOKUP<>nameiop) AND (udvp=nil) then
  begin
   Exit(EROFS);
  end;

  if (udvp<>nil) then
  begin
   dtmpvp:=udvp;
   if (ldvp<>nil) then
   begin
    VOP_UNLOCK(ldvp, LK_RELEASE);
   end;
  end else
  begin
   dtmpvp:=ldvp;
  end;

  error:=VOP_LOOKUP(dtmpvp, @vp, cnp);

  if (dtmpvp=udvp) AND (ldvp<>nil) then
  begin
   VOP_UNLOCK(udvp, LK_RELEASE);
   vn_lock(dvp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  end;

  if (error=0) then
  begin
   {
    * Exchange lock and reference from vp to
    * dunp^.un_dvp. vp is upper/lower vnode, but it
    * will need to return the unionfs vnode.
    }
   if (nameiop=DELETE) OR (nameiop=RENAME) OR
      ((cnp^.cn_lkflags and LK_TYPE_MASK)<>0) then
   begin
    VOP_UNLOCK(vp, LK_RELEASE);
   end;

   vrele(vp);

   VOP_UNLOCK(dvp, LK_RELEASE);
   (ap^.a_vpp)^:=dunp^.un_dvp;
   vref(dunp^.un_dvp);

   if (nameiop=DELETE) OR (nameiop=RENAME) then
   begin
    vn_lock(dunp^.un_dvp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
   end else
   if (cnp^.cn_lkflags and LK_TYPE_MASK)<>0 then
   begin
    vn_lock(dunp^.un_dvp, cnp^.cn_lkflags or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
   end;

   vn_lock(dvp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  end else
  if (error=ENOENT) AND ((cnflags and MAKEENTRY)<>0) AND (nameiop<>CREATE) then
  begin
   //cache_enter(dvp, nil, cnp);
  end;

  LOG_DEBUG('unionfs_lookup: leave (', error, ')');

  Exit(error);
 end;

 {
  * lookup upper layer
  }
 if (udvp<>nil) then
 begin
  uerror:=VOP_LOOKUP(udvp, @uvp, cnp);

  if (uerror=0) then
  begin

   if (udvp=uvp) then
   begin { is dot }
    vrele(uvp);
    (ap^.a_vpp)^:=dvp;
    vref(dvp);

    LOG_DEBUG('unionfs_lookup: leave ', uerror);

    Exit(uerror);
   end;

   if (nameiop=DELETE) OR (nameiop=RENAME) OR ((cnp^.cn_lkflags and LK_TYPE_MASK)<>0) then
   begin
    VOP_UNLOCK(uvp, LK_RELEASE);
   end;
  end;

  { check whiteout }
  if (uerror=ENOENT) OR (uerror=EJUSTRETURN) then
   if (cnp^.cn_flags and ISWHITEOUT)<>0 then
    iswhiteout:=1; { don't lookup lower }

  if (iswhiteout=0) AND (ldvp<>nil) then
   if (VOP_GETATTR(udvp, @va)=0) AND ((va.va_flags and OPAQUE)<>0) then
    iswhiteout:=1; { don't lookup lower }

  LOG_DEBUG('unionfs_lookup: debug: whiteout=', iswhiteout, ' path=', cnp^.cn_nameptr);
 end;

 {
  * lookup lower layer
  }
 if (ldvp<>nil) AND ((cnflags and DOWHITEOUT)=0) AND (iswhiteout=0) then
 begin
  { always op is LOOKUP }
  cnp^.cn_nameiop:=LOOKUP;
  cnflagsbk:=cnp^.cn_flags;
  cnp^.cn_flags:=cnflags;

  lerror:=VOP_LOOKUP(ldvp, @lvp, cnp);

  cnp^.cn_nameiop:=nameiop;

  if (udvp<>nil) AND ((uerror=0) OR (uerror=EJUSTRETURN)) then
  begin
   cnp^.cn_flags:=cnflagsbk;
  end;

  if (lerror=0) then
  begin
   if (ldvp=lvp) then
   begin { is dot }
    if (uvp<>nil) then
    begin
     vrele(uvp); { no need? }
    end;

    vrele(lvp);
    (ap^.a_vpp)^:=dvp;
    vref(dvp);

    LOG_DEBUG('unionfs_lookup: leave ', lerror);

    Exit(lerror);
   end;
   if (cnp^.cn_lkflags and LK_TYPE_MASK)<>0 then
   begin
    VOP_UNLOCK(lvp, LK_RELEASE);
   end;
  end;
 end;

 {
  * check lookup result
  }
 if (uvp=nil) AND (lvp=nil) then
 begin
  if (udvp<>nil) then
   Result:=uerror
  else
   Result:=lerror;

  LOG_DEBUG('unionfs_lookup: leave ', Result);
  Exit;
 end;

 {
  * check vnode type
  }
 if (uvp<>nil) AND (lvp<>nil) AND (uvp^.v_type<>lvp^.v_type) then
 begin
  vrele(lvp);
  lvp:=nil;
 end;

 {
  * check shadow dir
  }
 if (uerror<>0) AND (uerror<>EJUSTRETURN) AND (udvp<>nil) AND
    (lerror=0) AND (lvp<>nil) AND (lvp^.v_type=VDIR) AND
    ((p_mount(dvp^.v_mount)^.mnt_flag and MNT_RDONLY)=0) AND
    ((1 < cnp^.cn_namelen) OR (cnp^.cn_nameptr[0]<>'.')) then
 begin
  { get unionfs vnode in order to create a new shadow dir. }
  error:=unionfs_nodeget(dvp^.v_mount, nil, lvp, dvp, @vp, cnp);
  if (error<>0) then
  begin
   goto _unionfs_lookup_out;
  end;

  if (LK_SHARED=(cnp^.cn_lkflags and LK_TYPE_MASK)) then
  begin
   VOP_UNLOCK(vp, LK_RELEASE);
  end;

  if (LK_EXCLUSIVE<>VOP_ISLOCKED(vp)) then
  begin
   vn_lock(vp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
   lockflag:=1;
  end;

  error:=unionfs_mkshadowdir(MOUNTTOUNIONFSMOUNT(dvp^.v_mount), udvp, VTOUNIONFS(vp), cnp);

  if (lockflag<>0) then
  begin
   VOP_UNLOCK(vp, LK_RELEASE);
  end;

  if (error<>0) then
  begin
   LOG_DEBUG('unionfs_lookup: Unable to create shadow dir.');

   if ((cnp^.cn_lkflags and LK_TYPE_MASK)=LK_EXCLUSIVE) then
    vput(vp)
   else
    vrele(vp);

   goto _unionfs_lookup_out;
  end;

  if ((cnp^.cn_lkflags and LK_TYPE_MASK)=LK_SHARED) then
  begin
   vn_lock(vp, LK_SHARED or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  end;

 end
 {
  * get unionfs vnode.
  }
 else
 begin

  if (uvp<>nil) then
   error:=uerror
  else
   error:=lerror;

  if (error<>0) then
  begin
   goto _unionfs_lookup_out;
  end;

  {
   * get socket vnode.
   }
  if (uvp<>nil) AND (uvp^.v_type=VSOCK) then
  begin
   vp:=uvp;
   vref(vp);
   if (cnp^.cn_lkflags and LK_TYPE_MASK)<>0 then
   begin
    vn_lock(vp, cnp^.cn_lkflags or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
   end;
  end else
  if (lvp<>nil) AND (lvp^.v_type=VSOCK) then
  begin
   vp:=lvp;
   vref(vp);
   if (cnp^.cn_lkflags and LK_TYPE_MASK)<>0 then
   begin
    vn_lock(vp, cnp^.cn_lkflags or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
   end;
  end
  {
   * get unionfs vnode.
   }
  else
  begin
   error:=unionfs_nodeget(dvp^.v_mount, uvp, lvp, dvp, @vp, cnp);
  end;

  if (error<>0) then
  begin
   LOG_DEBUG('unionfs_lookup: Unable to create unionfs vnode.');
   goto _unionfs_lookup_out;
  end;

  if ((nameiop=DELETE) OR (nameiop=RENAME)) AND
     ((cnp^.cn_lkflags and LK_TYPE_MASK)=0) then
  begin
   vn_lock(vp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  end;

 end;

 (ap^.a_vpp)^:=vp;

 if ((cnflags and MAKEENTRY)<>0) AND (vp^.v_type<>VSOCK) then
 begin
  //cache_enter(dvp, vp, cnp);
 end;

_unionfs_lookup_out:
 if (uvp<>nil) then
 begin
  vrele(uvp);
 end;
 if (lvp<>nil) then
 begin
  vrele(lvp);
 end;

 if (error=ENOENT) AND ((cnflags and MAKEENTRY)<>0) AND (nameiop<>CREATE) then
 begin
  //cache_enter(dvp, nil, cnp);
 end;

 LOG_DEBUG('unionfs_lookup: leave ', error);

 Exit(error);
end;

function unionfs_create(ap:p_vop_create_args):Integer;
label
 _unionfs_create_abort;
var
 dunp:p_unionfs_node;
 cnp:p_componentname;
 udvp:p_vnode;
 vp:p_vnode;
 error:Integer;
begin
 LOG_DEBUG('unionfs_create: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_dvp);

 dunp:=VTOUNIONFS(ap^.a_dvp);
 cnp:=ap^.a_cnp;
 udvp:=dunp^.un_uppervp;
 error:=EROFS;

 if (udvp<>nil) then
 begin
  error:=VOP_CREATE(udvp, @vp, cnp, ap^.a_vap, ap^.a_flagp);
  if (error<>0) then
  begin
   goto _unionfs_create_abort;
  end;

  if (vp^.v_type=VSOCK) then
  begin
   (ap^.a_vpp)^:=vp;
  end else
  begin
   VOP_UNLOCK(vp, LK_RELEASE);
   error:=unionfs_nodeget(ap^.a_dvp^.v_mount, vp, nil, ap^.a_dvp, ap^.a_vpp, cnp);
   vrele(vp);
  end;
 end;

_unionfs_create_abort:
 LOG_DEBUG('unionfs_create: leave ', error);

 Exit(error);
end;

function unionfs_whiteout(ap:p_vop_whiteout_args):Integer;
var
 dunp:p_unionfs_node;
 cnp:p_componentname;
 udvp:p_vnode;
 error:Integer;
begin
 LOG_DEBUG('unionfs_whiteout: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_dvp);

 dunp:=VTOUNIONFS(ap^.a_dvp);
 cnp:=ap^.a_cnp;
 udvp:=dunp^.un_uppervp;
 error:=EOPNOTSUPP;

 if (udvp<>nil) then
 begin
  case (ap^.a_flags) of
   CREATE,
   DELETE,
   LOOKUP:
    error:=VOP_WHITEOUT(udvp, cnp, ap^.a_flags);
   else
    error:=EINVAL;
  end;
 end;

 LOG_DEBUG('unionfs_whiteout: leave ', error);

 Exit(error);
end;

function unionfs_mknod(ap:p_vop_mknod_args):Integer;
label
 _unionfs_mknod_abort;
var
 dunp:p_unionfs_node;
 cnp:p_componentname;
 udvp:p_vnode;
 vp:p_vnode;
 error:Integer;
begin
 LOG_DEBUG('unionfs_mknod: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_dvp);

 dunp:=VTOUNIONFS(ap^.a_dvp);
 cnp:=ap^.a_cnp;
 udvp:=dunp^.un_uppervp;
 error:=EROFS;

 if (udvp<>nil) then
 begin
  error:=VOP_MKNOD(udvp, @vp, cnp, ap^.a_vap);
  if (error<>0) then
  begin
   goto _unionfs_mknod_abort;
  end;

  if (vp^.v_type=VSOCK) then
  begin
   (ap^.a_vpp)^:=vp;
  end else
  begin
   VOP_UNLOCK(vp, LK_RELEASE);
   error:=unionfs_nodeget(ap^.a_dvp^.v_mount, vp, nil, ap^.a_dvp, ap^.a_vpp, cnp);
   vrele(vp);
  end;
 end;

_unionfs_mknod_abort:
 LOG_DEBUG('unionfs_mknod: leave ', error);

 Exit(error);
end;

function unionfs_open(ap:p_vop_open_args):Integer;
label
 _unionfs_open_abort;
var
 error:Integer;
 unp:p_unionfs_node;
 unsp:p_unionfs_node_status;
 uvp:p_vnode;
 lvp:p_vnode;
 targetvp:p_vnode;
 //struct ucred   *cred;
 //struct thread  *td;
begin
 LOG_DEBUG('unionfs_open: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 error:=0;
 unp:=VTOUNIONFS(ap^.a_vp);
 uvp:=unp^.un_uppervp;
 lvp:=unp^.un_lowervp;
 targetvp:=nil;
 //cred:=ap^.a_cred;
 //td:=ap^.a_td;

 unionfs_get_node_status(unp, @unsp);

 if (unsp^.uns_lower_opencnt > 0) OR (unsp^.uns_upper_opencnt > 0) then
 begin
  { vnode is already opend. }
  if (unsp^.uns_upper_opencnt > 0) then
   targetvp:=uvp
  else
   targetvp:=lvp;

  if (targetvp=lvp) AND ((ap^.a_mode and FWRITE)<>0) AND (lvp^.v_type=VREG) then
  begin
   targetvp:=nil;
  end;
 end;

 if (targetvp=nil) then
 begin
  if (uvp=nil) then
  begin
   if ((ap^.a_mode and FWRITE)<>0) AND (lvp^.v_type=VREG) then
   begin
    error:=unionfs_copyfile(unp, Ord((ap^.a_mode and O_TRUNC)=0));
    if (error<>0) then
    begin
     goto _unionfs_open_abort;
    end;
    targetvp:=unp^.un_uppervp;
    uvp:=unp^.un_uppervp;
   end else
   begin
    targetvp:=lvp;
   end;
  end else
  begin
   targetvp:=uvp;
  end;
 end;

 error:=VOP_OPEN(targetvp, ap^.a_mode, ap^.a_fp, ap^.a_flagp);
 if (error=0) then
 begin
  if (targetvp=uvp) then
  begin
   if (uvp^.v_type=VDIR) AND (lvp<>nil) AND
      (unsp^.uns_lower_opencnt <= 0) then
   begin
    { open lower for readdir }
    error:=VOP_OPEN(lvp, FREAD, nil, nil);
    if (error<>0) then
    begin
     VOP_CLOSE(uvp, ap^.a_mode);
     goto _unionfs_open_abort;
    end;

    unsp^.uns_node_flag:=unsp^.uns_node_flag or UNS_OPENL_4_READDIR;
    Inc(unsp^.uns_lower_opencnt);
   end;
   Inc(unsp^.uns_upper_opencnt);
  end else
  begin
   Inc(unsp^.uns_lower_opencnt);
   unsp^.uns_lower_openmode:=ap^.a_mode;
  end;
  ap^.a_vp^.v_object:=targetvp^.v_object;
 end;

_unionfs_open_abort:
 if (error<>0) then
 begin
  unionfs_tryrem_node_status(unp, unsp);
 end;

 LOG_DEBUG('unionfs_open: leave ', error);

 Exit(error);
end;

function unionfs_close(ap:p_vop_close_args):Integer;
label
 _unionfs_close_abort;
var
 error:Integer;
 locked:Integer;
 unp:p_unionfs_node;
 unsp:p_unionfs_node_status;
 //struct ucred   *cred;
 //struct thread  *td;
 vp:p_vnode;
 ovp:p_vnode;
begin
 LOG_DEBUG('unionfs_close: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 locked:=0;
 vp:=ap^.a_vp;
 unp:=VTOUNIONFS(vp);
 //cred:=ap^.a_cred;
 //td:=ap^.a_td;

 if (VOP_ISLOCKED(vp)<>LK_EXCLUSIVE) then
 begin
  if (vn_lock(vp, LK_UPGRADE, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%})<>0) then
  begin
   vn_lock(vp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  end;
  locked:=1;
 end;
 unionfs_get_node_status(unp, @unsp);

 if (unsp^.uns_lower_opencnt <= 0) AND (unsp^.uns_upper_opencnt <= 0) then
 begin
  LOG_DEBUG('unionfs_close: warning: open count is 0');

  if (unp^.un_uppervp<>nil) then
   ovp:=unp^.un_uppervp
  else
   ovp:=unp^.un_lowervp;

 end else
 if (unsp^.uns_upper_opencnt > 0) then
  ovp:=unp^.un_uppervp
 else
  ovp:=unp^.un_lowervp;

 error:=VOP_CLOSE(ovp, ap^.a_fflag);

 if (error<>0) then
 begin
  goto _unionfs_close_abort;
 end;

 vp^.v_object:=ovp^.v_object;

 if (ovp=unp^.un_uppervp) then
 begin
  Dec(unsp^.uns_upper_opencnt);
  if (unsp^.uns_upper_opencnt=0) then
  begin
   if (unsp^.uns_node_flag and UNS_OPENL_4_READDIR)<>0 then
   begin
    VOP_CLOSE(unp^.un_lowervp, FREAD);
    unsp^.uns_node_flag:=unsp^.uns_node_flag and (not UNS_OPENL_4_READDIR);
    Dec(unsp^.uns_lower_opencnt);
   end;
   if (unsp^.uns_lower_opencnt > 0) then
   begin
    vp^.v_object:=unp^.un_lowervp^.v_object;
   end;
  end;
 end else
 begin
  Dec(unsp^.uns_lower_opencnt);
 end;

_unionfs_close_abort:
 unionfs_tryrem_node_status(unp, unsp);

 if (locked<>0) then
 begin
  vn_lock(vp, LK_DOWNGRADE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 end;

 LOG_DEBUG('unionfs_close: leave ', error);

 Exit(error);
end;

{
 * Check the access mode toward shadow file/dir.
 }
function unionfs_check_corrected_access(accmode:accmode_t;va:p_vattr):Integer;
var
 count:Integer;
 uid,gid:Integer;
 vmode,mask:Word;
begin
 mask:=0;
 uid:=va^.va_uid;
 gid:=va^.va_gid;
 vmode:=va^.va_mode;

 { check owner }
 //if (cred^.cr_uid=uid) then
 begin
  if (accmode and VEXEC)<>0 then
  begin
   mask:=mask or S_IXUSR;
  end;
  if (accmode and VREAD)<>0 then
  begin
   mask:=mask or S_IRUSR;
  end;
  if (accmode and VWRITE)<>0 then
  begin
   mask:=mask or S_IWUSR;
  end;
  if ((vmode and mask)=mask) then Exit(0) else Exit(EACCES);
 end;

 { check group }
 count:=0;
 //if (groupmember(gid, cred)) then
 begin
  if (accmode and VEXEC)<>0 then
  begin
   mask:=mask or S_IXGRP;
  end;
  if (accmode and VREAD)<>0 then
  begin
   mask:=mask or S_IRGRP;
  end;
  if (accmode and VWRITE)<>0 then
  begin
   mask:=mask or S_IWGRP;
  end;
  if ((vmode and mask)=mask) then Exit(0) else Exit(EACCES);
 end;

 { check other }
 if (accmode and VEXEC)<>0 then
 begin
  mask:=mask or S_IXOTH;
 end;
 if (accmode and VREAD)<>0 then
 begin
  mask:=mask or S_IROTH;
 end;
 if (accmode and VWRITE)<>0 then
 begin
  mask:=mask or S_IWOTH;
 end;

 if ((vmode and mask)=mask) then Exit(0) else Exit(EACCES);
end;

function unionfs_access(ap:p_vop_access_args):Integer;
var
 ump:p_unionfs_mount;
 unp:p_unionfs_node;
 uvp:p_vnode;
 lvp:p_vnode;
 //struct thread  *td;
 va:t_vattr;
 accmode:accmode_t;
 error:Integer;
begin
 LOG_DEBUG('unionfs_access: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 ump:=MOUNTTOUNIONFSMOUNT(ap^.a_vp^.v_mount);
 unp:=VTOUNIONFS(ap^.a_vp);
 uvp:=unp^.un_uppervp;
 lvp:=unp^.un_lowervp;
 //td:=ap^.a_td;
 accmode:=ap^.a_accmode;
 error:=EACCES;

 if (accmode and VWRITE) AND (p_mount(ap^.a_vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0 then
 begin
  case (ap^.a_vp^.v_type) of
   VREG,
   VDIR,
   VLNK:
    Exit(EROFS);
   else;
  end;
 end;

 if (uvp<>nil) then
 begin
  error:=VOP_ACCESS(uvp, accmode);

  LOG_DEBUG('unionfs_access: leave ', error);

  Exit(error);
 end;

 if (lvp<>nil) then
 begin
  if (accmode and VWRITE)<>0 then
  begin
   if (p_mount(ump^.um_uppervp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0 then
   begin
    case (ap^.a_vp^.v_type) of
     VREG,
     VDIR,
     VLNK:
      Exit(EROFS);
     else;
    end;
   end else
   if (ap^.a_vp^.v_type=VREG) OR (ap^.a_vp^.v_type=VDIR) then
   begin
    { check shadow file/dir }
    if (ump^.um_copymode<>UNIONFS_TRANSPARENT) then
    begin
     error:=unionfs_create_uppervattr(ump, lvp, @va);
     if (error<>0) then Exit(error);

     error:=unionfs_check_corrected_access(accmode, @va);
     if (error<>0) then Exit(error);
    end;
   end;
   accmode:=accmode and (not (VWRITE or VAPPEND));
   accmode:=accmode or VREAD; { will copy to upper }
  end;
  error:=VOP_ACCESS(lvp, accmode);
 end;

 LOG_DEBUG('unionfs_access: leave ', error);

 Exit(error);
end;

function unionfs_getattr(ap:p_vop_getattr_args):Integer;
var
 error:Integer;
 unp:p_unionfs_node;
 ump:p_unionfs_mount;
 uvp:p_vnode;
 lvp:p_vnode;
 //struct thread  *td;
 va:t_vattr;
begin
 LOG_DEBUG('unionfs_getattr: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 unp:=VTOUNIONFS(ap^.a_vp);
 ump:=MOUNTTOUNIONFSMOUNT(ap^.a_vp^.v_mount);
 uvp:=unp^.un_uppervp;
 lvp:=unp^.un_lowervp;
 //td:=curthread;

 if (uvp<>nil) then
 begin
  error:=VOP_GETATTR(uvp, ap^.a_vap);
  if (error=0) then
  begin
   ap^.a_vap^.va_fsid:=p_mount(ap^.a_vp^.v_mount)^.mnt_stat.f_fsid.val[0];
  end;

  LOG_DEBUG('unionfs_getattr: leave mode=',
      ap^.a_vap^.va_mode, ' uid=',ap^.a_vap^.va_uid, ' gid=',
      ap^.a_vap^.va_gid, ' ',error);

  Exit(error);
 end;

 error:=VOP_GETATTR(lvp, ap^.a_vap);

 if (error=0) AND ((p_mount(ump^.um_uppervp^.v_mount)^.mnt_flag and MNT_RDONLY)=0) then
 begin
  { correct the attr toward shadow file/dir. }
  if (ap^.a_vp^.v_type=VREG) OR (ap^.a_vp^.v_type=VDIR) then
  begin
   unionfs_create_uppervattr_core(ump, ap^.a_vap, @va);
   ap^.a_vap^.va_mode:=va.va_mode;
   ap^.a_vap^.va_uid:=va.va_uid;
   ap^.a_vap^.va_gid:=va.va_gid;
  end;
 end;

 if (error=0) then
 begin
  ap^.a_vap^.va_fsid:=p_mount(ap^.a_vp^.v_mount)^.mnt_stat.f_fsid.val[0];
 end;

 LOG_DEBUG('unionfs_getattr: leave mode=',
     ap^.a_vap^.va_mode, ' uid=', ap^.a_vap^.va_uid, ' gid=', ap^.a_vap^.va_gid, ' ', error);

 Exit(error);
end;

function unionfs_setattr(ap:p_vop_setattr_args):Integer;
var
 error:Integer;
 unp:p_unionfs_node;
 uvp:p_vnode;
 lvp:p_vnode;
 //struct thread  *td;
 vap:p_vattr;
begin

 LOG_DEBUG('unionfs_setattr: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 error:=EROFS;
 unp:=VTOUNIONFS(ap^.a_vp);
 uvp:=unp^.un_uppervp;
 lvp:=unp^.un_lowervp;
 //td:=curthread;
 vap:=ap^.a_vap;

 if ((p_mount(ap^.a_vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0) AND
    ((vap^.va_flags<>VNOVAL) OR (vap^.va_uid<>VNOVAL) OR
     (vap^.va_gid<>VNOVAL) OR (vap^.va_atime.tv_sec<>VNOVAL) OR
     (vap^.va_mtime.tv_sec<>VNOVAL) OR (vap^.va_mode<>VNOVAL)) then
 begin
  Exit(EROFS);
 end;

 if (uvp=nil) AND (lvp^.v_type=VREG) then
 begin
  error:=unionfs_copyfile(unp, ord(vap^.va_size<>0));
  if (error<>0) then Exit(error);
  uvp:=unp^.un_uppervp;
 end;

 if (uvp<>nil) then
 begin
  error:=VOP_SETATTR(uvp, vap);
 end;

 LOG_DEBUG('unionfs_setattr: leave ', error);

 Exit(error);
end;

function unionfs_read(ap:p_vop_read_args):Integer;
var
 error:Integer;
 unp:p_unionfs_node;
 tvp:p_vnode;
begin
 { LOG_DEBUG('unionfs_read: enter'); }

 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 unp:=VTOUNIONFS(ap^.a_vp);

 if (unp^.un_uppervp<>nil) then
  tvp:=unp^.un_uppervp
 else
  tvp:=unp^.un_lowervp;

 error:=VOP_READ(tvp, ap^.a_uio, ap^.a_ioflag);

 { LOG_DEBUG('unionfs_read: leave (%d)', error); }

 Exit(error);
end;

function unionfs_write(ap:p_vop_write_args):Integer;
var
 error:Integer;
 unp:p_unionfs_node;
 tvp:p_vnode;
begin
 { LOG_DEBUG('unionfs_write: enter'); }

 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 unp:=VTOUNIONFS(ap^.a_vp);

 if (unp^.un_uppervp<>nil) then
  tvp:=unp^.un_uppervp
 else
  tvp:=unp^.un_lowervp;

 error:=VOP_WRITE(tvp, ap^.a_uio, ap^.a_ioflag);

 { LOG_DEBUG('unionfs_write: leave (%d)', error); }

 Exit(error);
end;

function unionfs_ioctl(ap:p_vop_ioctl_args):Integer;
var
 error:Integer;
 unp:p_unionfs_node;
 unsp:p_unionfs_node_status;
 ovp:p_vnode;
begin
 LOG_DEBUG('unionfs_ioctl: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 vn_lock(ap^.a_vp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});

 unp:=VTOUNIONFS(ap^.a_vp);
 unionfs_get_node_status(unp, @unsp);

 if (unsp^.uns_upper_opencnt<>0) then
  ovp:=unp^.un_uppervp
 else
  ovp:=unp^.un_lowervp;

 unionfs_tryrem_node_status(unp, unsp);
 VOP_UNLOCK(ap^.a_vp, LK_RELEASE);

 if (ovp=nil) then Exit(EBADF);

 error:=VOP_IOCTL(ovp, ap^.a_command, ap^.a_data, ap^.a_fflag);

 LOG_DEBUG('unionfs_ioctl: leave ', error);

 Exit(error);
end;

function unionfs_poll(ap:p_vop_poll_args):Integer;
var
 unp:p_unionfs_node;
 unsp:p_unionfs_node_status;
 ovp:p_vnode;
begin
 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 vn_lock(ap^.a_vp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});

 unp:=VTOUNIONFS(ap^.a_vp);
 unionfs_get_node_status(unp, @unsp);

 if (unsp^.uns_upper_opencnt<>0) then
  ovp:=unp^.un_uppervp
 else
  ovp:=unp^.un_lowervp;

 unionfs_tryrem_node_status(unp, unsp);
 VOP_UNLOCK(ap^.a_vp, LK_RELEASE);

 if (ovp=nil) then Exit(EBADF);

 Exit(VOP_POLL(ovp, ap^.a_events));
end;

function unionfs_fsync(ap:p_vop_fsync_args):Integer;
var
 unp:p_unionfs_node;
 unsp:p_unionfs_node_status;
 ovp:p_vnode;
begin
 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 unp:=VTOUNIONFS(ap^.a_vp);
 unionfs_get_node_status(unp, @unsp);

 if (unsp^.uns_upper_opencnt<>0) then
  ovp:=unp^.un_uppervp
 else
  ovp:=unp^.un_lowervp;

 unionfs_tryrem_node_status(unp, unsp);

 if (ovp=nil) then Exit(EBADF);

 Exit(VOP_FSYNC(ovp, ap^.a_waitfor));
end;

function unionfs_remove(ap:p_vop_remove_args):Integer;
var
 error:Integer;
 path:PChar;
 dunp:p_unionfs_node;
 unp:p_unionfs_node;
 ump:p_unionfs_mount;
 udvp:p_vnode;
 uvp:p_vnode;
 lvp:p_vnode;
 vp:p_vnode;
 cnp:p_componentname;
 cn:componentname;
 //struct thread  *td;
begin
 LOG_DEBUG('unionfs_remove: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_dvp);

 error:=0;
 dunp:=VTOUNIONFS(ap^.a_dvp);
 udvp:=dunp^.un_uppervp;
 cnp:=ap^.a_cnp;
 //td:=curthread;

 if (ap^.a_vp^.v_op<>@unionfs_vnodeops) then
 begin
  if (ap^.a_vp^.v_type<>VSOCK) then Exit(EINVAL);
  ump:=nil;
  vp:=nil;
  uvp:=nil;
  lvp:=nil;

  { search vnode }
  VOP_UNLOCK(ap^.a_vp, LK_RELEASE);

  error:=unionfs_relookup(udvp, @vp, cnp, @cn, cnp^.cn_nameptr, strlen(cnp^.cn_nameptr), DELETE);
  if (error<>0) AND (error<>ENOENT) then
  begin
   vn_lock(ap^.a_vp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
   Exit(error);
  end;

  if (error=0) AND (vp=ap^.a_vp) then
  begin
   { target vnode in upper }
   uvp:=vp;
   vrele(vp);
   path:=nil;
  end else
  begin
   { target vnode in lower }
   if (vp<>nil) then
   begin
    if (udvp=vp) then
     vrele(vp)
    else
     vput(vp);
   end;
   vn_lock(ap^.a_vp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
   lvp:=ap^.a_vp;
   path:=ap^.a_cnp^.cn_nameptr;
  end;
 end else
 begin
  ump:=MOUNTTOUNIONFSMOUNT(ap^.a_vp^.v_mount);
  unp:=VTOUNIONFS(ap^.a_vp);
  uvp:=unp^.un_uppervp;
  lvp:=unp^.un_lowervp;
  path:=unp^.un_path;
 end;

 if (udvp=nil) then Exit(EROFS);

 if (uvp<>nil) then
 begin
  {
   * XXX: if the vnode type is VSOCK, it will create whiteout
   *      after remove.
   }
  if (ump=nil) OR (ump^.um_whitemode=UNIONFS_WHITE_ALWAYS) OR (lvp<>nil) then
  begin
   cnp^.cn_flags:=cnp^.cn_flags or DOWHITEOUT;
  end;

  error:=VOP_REMOVE(udvp, uvp, cnp);
 end else
 if (lvp<>nil) then
 begin
  error:=unionfs_mkwhiteout(udvp, cnp, path);
 end;

 LOG_DEBUG('unionfs_remove: leave (', error, ')');

 Exit(error);
end;

function unionfs_link(ap:p_vop_link_args):Integer;
var
 error:Integer;
  needrelookup:Integer;
 dunp:p_unionfs_node;
 unp:p_unionfs_node;
 udvp:p_vnode;
 uvp:p_vnode;
 cnp:p_componentname;
 //struct thread  *td;
begin
 LOG_DEBUG('unionfs_link: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_tdvp);
 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 error:=0;
 needrelookup:=0;
 dunp:=VTOUNIONFS(ap^.a_tdvp);
 unp:=nil;
 udvp:=dunp^.un_uppervp;
 uvp:=nil;
 cnp:=ap^.a_cnp;
 //td:=curthread;

 if (udvp=nil) then Exit(EROFS);

 if (ap^.a_vp^.v_op<>@unionfs_vnodeops) then
 begin
  uvp:=ap^.a_vp;
 end else
 begin
  unp:=VTOUNIONFS(ap^.a_vp);

  if (unp^.un_uppervp=nil) then
  begin
   if (ap^.a_vp^.v_type<>VREG) then
   begin
    Exit(EOPNOTSUPP);
   end;

   error:=unionfs_copyfile(unp, 1);
   if (error<>0) then Exit(error);

   needrelookup:=1;
  end;

  uvp:=unp^.un_uppervp;
 end;

 if (needrelookup<>0) then
 begin
  error:=unionfs_relookup_for_create(ap^.a_tdvp, cnp);
 end;

 if (error=0) then
 begin
  error:=VOP_LINK(udvp, uvp, cnp);
 end;

 LOG_DEBUG('unionfs_link: leave ', error);

 Exit(error);
end;

function unionfs_rename(ap:p_vop_rename_args):Integer;
label
 _unionfs_rename_abort;
var
 error:Integer;
 fdvp:p_vnode;
 fvp:p_vnode;
 fcnp:p_componentname;
 tdvp:p_vnode;
 tvp:p_vnode;
 tcnp:p_componentname;
 ltdvp:p_vnode;
 ltvp:p_vnode;
 //struct thread  *td;

 { rename target vnodes }
 rfdvp:p_vnode;
 rfvp:p_vnode;
 rtdvp:p_vnode;
 rtvp:p_vnode;

 needrelookup:Integer;
 ump:p_unionfs_mount;
 unp:p_unionfs_node;
begin
 LOG_DEBUG('unionfs_rename: enter');

 error:=0;
 fdvp:=ap^.a_fdvp;
 fvp:=ap^.a_fvp;
 fcnp:=ap^.a_fcnp;
 tdvp:=ap^.a_tdvp;
 tvp:=ap^.a_tvp;
 tcnp:=ap^.a_tcnp;
 ltdvp:=nil;
 ltvp:=nil;
 //td:=curthread;
 rfdvp:=fdvp;
 rfvp:=fvp;
 rtdvp:=tdvp;
 rtvp:=tvp;
 needrelookup:=0;

 if ((fcnp^.cn_flags and HASBUF)=0) OR ((tcnp^.cn_flags and HASBUF)=0) then
 begin
  Assert(False,'unionfs_rename: no name');
 end;

 { check for cross device rename }
 if (fvp^.v_mount<>tdvp^.v_mount) OR
    ((tvp<>nil) AND (fvp^.v_mount<>tvp^.v_mount)) then
 begin
  if (fvp^.v_op<>@unionfs_vnodeops) then
   error:=ENODEV
  else
   error:=EXDEV;

  goto _unionfs_rename_abort;
 end;

 { Renaming a file to itself has no effect. }
 if (fvp=tvp) then
 begin
  goto _unionfs_rename_abort;
 end;

 {
  * from/to vnode is unionfs node.
  }

 KASSERT_UNIONFS_VNODE(fdvp);
 KASSERT_UNIONFS_VNODE(fvp);
 KASSERT_UNIONFS_VNODE(tdvp);
 if (tvp<>nil) then
 begin
  KASSERT_UNIONFS_VNODE(tvp);
 end;

 unp:=VTOUNIONFS(fdvp);

 //LOG_DEBUG('fdvp=%p, ufdvp=%p, lfdvp=%p', fdvp, unp^.un_uppervp, unp^.un_lowervp);

 if (unp^.un_uppervp=nil) then
 begin
  error:=ENODEV;
  goto _unionfs_rename_abort;
 end;

 rfdvp:=unp^.un_uppervp;
 vref(rfdvp);

 unp:=VTOUNIONFS(fvp);

 //LOG_DEBUG('fvp=%p, ufvp=%p, lfvp=%p', fvp, unp^.un_uppervp, unp^.un_lowervp);

 ump:=MOUNTTOUNIONFSMOUNT(fvp^.v_mount);
 if (unp^.un_uppervp=nil) then
 begin
  case (fvp^.v_type) of
   VREG:
    begin
     error:=vn_lock(fvp, LK_EXCLUSIVE, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
     if (error<>0) then
     begin
      goto _unionfs_rename_abort;
     end;

     error:=unionfs_copyfile(unp, 1);
     VOP_UNLOCK(fvp, LK_RELEASE);

     if (error<>0) then
     begin
      goto _unionfs_rename_abort;
     end;
    end;
   VDIR:
    begin
     error:=vn_lock(fvp, LK_EXCLUSIVE, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
     if (error<>0) then
     begin
      goto _unionfs_rename_abort;
     end;

     error:=unionfs_mkshadowdir(ump, rfdvp, unp, fcnp);
     VOP_UNLOCK(fvp, LK_RELEASE);

     if (error<>0) then
     begin
      goto _unionfs_rename_abort;
     end;
    end;
   else
    begin
     error:=ENODEV;
     goto _unionfs_rename_abort;
    end;
  end;

  needrelookup:=1;
 end;

 if (unp^.un_lowervp<>nil) then
 begin
  fcnp^.cn_flags:=fcnp^.cn_flags or DOWHITEOUT;
 end;

 rfvp:=unp^.un_uppervp;
 vref(rfvp);

 unp:=VTOUNIONFS(tdvp);

 //LOG_DEBUG('tdvp=%p, utdvp=%p, ltdvp=%p', tdvp, unp^.un_uppervp, unp^.un_lowervp);

 if (unp^.un_uppervp=nil) then
 begin
  error:=ENODEV;
  goto _unionfs_rename_abort;
 end;

 rtdvp:=unp^.un_uppervp;
 ltdvp:=unp^.un_lowervp;
 vref(rtdvp);

 if (tdvp=tvp) then
 begin
  rtvp:=rtdvp;
  vref(rtvp);
 end else
 if (tvp<>nil) then
 begin
  unp:=VTOUNIONFS(tvp);

  //LOG_DEBUG('tvp=%p, utvp=%p, ltvp=%p', tvp, unp^.un_uppervp, unp^.un_lowervp);

  if (unp^.un_uppervp=nil) then
  begin
   rtvp:=nil;
  end else
  begin
   if (tvp^.v_type=VDIR) then
   begin
    error:=EINVAL;
    goto _unionfs_rename_abort;
   end;
   rtvp:=unp^.un_uppervp;
   ltvp:=unp^.un_lowervp;
   vref(rtvp);
  end;
 end;

 if (rfvp=rtvp) then
 begin
  goto _unionfs_rename_abort;
 end;

 if (needrelookup<>0) then
 begin
  error:=vn_lock(fdvp, LK_EXCLUSIVE, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  if (error<>0) then
  begin
   goto _unionfs_rename_abort;
  end;

  error:=unionfs_relookup_for_delete(fdvp, fcnp);
  VOP_UNLOCK(fdvp, LK_RELEASE);
  if (error<>0) then
  begin
   goto _unionfs_rename_abort;
  end;

  { Locke of tvp is canceled in order to avoid recursive lock. }
  if (tvp<>nil) AND (tvp<>tdvp) then
  begin
   VOP_UNLOCK(tvp, LK_RELEASE);
  end;

  error:=unionfs_relookup_for_rename(tdvp, tcnp);
  if (tvp<>nil) AND (tvp<>tdvp) then
  begin
   vn_lock(tvp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  end;

  if (error<>0) then
  begin
   goto _unionfs_rename_abort;
  end;
 end;

 error:=VOP_RENAME(rfdvp, rfvp, fcnp, rtdvp, rtvp, tcnp);

 if (error=0) then
 begin
  if (rtvp<>nil) AND (rtvp^.v_type=VDIR) then
  begin
   //cache_purge(tdvp);
  end;

  if (fvp^.v_type=VDIR) AND (fdvp<>tdvp) then
  begin
   //cache_purge(fdvp);
  end;
 end;

 if (ltdvp<>nil) then
 begin
  VOP_UNLOCK(ltdvp, LK_RELEASE);
 end;

 if (tdvp<>rtdvp) then
 begin
  vrele(tdvp);
 end;
 if (ltvp<>nil) then
 begin
  VOP_UNLOCK(ltvp, LK_RELEASE);
 end;

 if (tvp<>rtvp) AND (tvp<>nil) then
 begin
  if (rtvp=nil) then
   vput(tvp)
  else
   vrele(tvp);
 end;

 if (fdvp<>rfdvp) then
 begin
  vrele(fdvp);
 end;
 if (fvp<>rfvp) then
 begin
  vrele(fvp);
 end;

 LOG_DEBUG('unionfs_rename: leave ', error);

 Exit(error);

_unionfs_rename_abort:
 vput(tdvp);

 if (tdvp<>rtdvp) then
 begin
  vrele(rtdvp);
 end;
 if (tvp<>nil) then
 begin
  if (tdvp<>tvp) then
   vput(tvp)
  else
   vrele(tvp);
 end;

 if (tvp<>rtvp) AND (rtvp<>nil) then
 begin
  vrele(rtvp);
 end;

 if (fdvp<>rfdvp) then
 begin
  vrele(rfdvp);
 end;
 if (fvp<>rfvp) then
 begin
  vrele(rfvp);
 end;
 vrele(fdvp);
 vrele(fvp);

 LOG_DEBUG('unionfs_rename: leave ', error);

 Exit(error);
end;

function unionfs_mkdir(ap:p_vop_mkdir_args):Integer;
var
 error:Integer;
 lkflags:Integer;
 dunp:p_unionfs_node;
 cnp:p_componentname;
 //struct thread  *td;
 udvp:p_vnode;
 uvp:p_vnode;
 va:t_vattr;
begin
 LOG_DEBUG('unionfs_mkdir: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_dvp);

 error:=EROFS;
 dunp:=VTOUNIONFS(ap^.a_dvp);
 cnp:=ap^.a_cnp;
 lkflags:=cnp^.cn_lkflags;
 //td:=curthread;
 udvp:=dunp^.un_uppervp;

 if (udvp<>nil) then
 begin
  { check opaque }
  if ((cnp^.cn_flags and ISWHITEOUT)=0) then
  begin
   error:=VOP_GETATTR(udvp, @va);
   if (error<>0) then Exit(error);

   if (va.va_flags and OPAQUE)<>0 then
   begin
    cnp^.cn_flags:=cnp^.cn_flags or ISWHITEOUT;
   end;
  end;

  error:=VOP_MKDIR(udvp, @uvp, cnp, ap^.a_vap);
  if (error=0) then
  begin
   VOP_UNLOCK(uvp, LK_RELEASE);
   cnp^.cn_lkflags:=LK_EXCLUSIVE;
   error:=unionfs_nodeget(ap^.a_dvp^.v_mount, uvp, nil, ap^.a_dvp, ap^.a_vpp, cnp);
   cnp^.cn_lkflags:=lkflags;
   vrele(uvp);
  end;
 end;

 LOG_DEBUG('unionfs_mkdir: leave (', error, ')');

 Exit(error);
end;

function unionfs_rmdir(ap:p_vop_rmdir_args):Integer;
var
 error:Integer;
 dunp:p_unionfs_node;
 unp:p_unionfs_node;
 ump:p_unionfs_mount;
 cnp:p_componentname;
 //struct thread  *td;
 udvp:p_vnode;
 uvp:p_vnode;
 lvp:p_vnode;
begin
 LOG_DEBUG('unionfs_rmdir: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_dvp);
 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 error:=0;
 dunp:=VTOUNIONFS(ap^.a_dvp);
 unp:=VTOUNIONFS(ap^.a_vp);
 cnp:=ap^.a_cnp;
 //td:=curthread;
 udvp:=dunp^.un_uppervp;
 uvp:=unp^.un_uppervp;
 lvp:=unp^.un_lowervp;

 if (udvp=nil) then Exit(EROFS);

 if (udvp=uvp) then
 begin
  Exit(EOPNOTSUPP);
 end;

 if (uvp<>nil) then
 begin
  if (lvp<>nil) then
  begin
   error:=unionfs_check_rmdir(ap^.a_vp);
   if (error<>0) then Exit(error);
  end;

  ump:=MOUNTTOUNIONFSMOUNT(ap^.a_vp^.v_mount);

  if (ump^.um_whitemode=UNIONFS_WHITE_ALWAYS) OR (lvp<>nil) then
  begin
   cnp^.cn_flags:=cnp^.cn_flags or DOWHITEOUT;
  end;

  error:=unionfs_relookup_for_delete(ap^.a_dvp, cnp);
  if (error=0) then
  begin
   error:=VOP_RMDIR(udvp, uvp, cnp);
  end;

 end else
 if (lvp<>nil) then
 begin
  error:=unionfs_mkwhiteout(udvp, cnp, unp^.un_path);
 end;

 if (error=0) then
 begin
  //cache_purge(ap^.a_dvp);
  //cache_purge(ap^.a_vp);
 end;

 LOG_DEBUG('unionfs_rmdir: leave ', error);

 Exit(error);
end;

function unionfs_symlink(ap:p_vop_symlink_args):Integer;
var
 error:Integer;
 lkflags:Integer;
 dunp:p_unionfs_node;
 cnp:p_componentname;
 //struct thread  *td;
 udvp:p_vnode;
 uvp:p_vnode;
begin
 LOG_DEBUG('unionfs_symlink: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_dvp);

 error:=EROFS;
 dunp:=VTOUNIONFS(ap^.a_dvp);
 cnp:=ap^.a_cnp;
 lkflags:=cnp^.cn_lkflags;
 //td:=curthread;
 udvp:=dunp^.un_uppervp;

 if (udvp<>nil) then
 begin
  error:=VOP_SYMLINK(udvp, @uvp, cnp, ap^.a_vap, ap^.a_target);
  if (error=0) then
  begin
   VOP_UNLOCK(uvp, LK_RELEASE);
   cnp^.cn_lkflags:=LK_EXCLUSIVE;
   error:=unionfs_nodeget(ap^.a_dvp^.v_mount, uvp, nil, ap^.a_dvp, ap^.a_vpp, cnp);
   cnp^.cn_lkflags:=lkflags;
   vrele(uvp);
  end;
 end;

 LOG_DEBUG('unionfs_symlink: leave (', error, ')');

 Exit(error);
end;

procedure memcpy(dst,src:Pointer;n:QWORD); inline;
begin
 Move(src^,dst^,n);
end;

function unionfs_readdir(ap:p_vop_readdir_args):Integer;
label
 _unionfs_readdir_exit;
var
 error:Integer;
 eofflag:Integer;
 locked:Integer;
 uio_offset_bk:Integer;
 unp:p_unionfs_node;
 unsp:p_unionfs_node_status;
 uio:p_uio;
 vp:p_vnode;
 uvp:p_vnode;
 lvp:p_vnode;
 //struct thread  *td;
 va:t_vattr;

 ncookies_bk:Integer;
 cookies_bk:PQWORD;

 size:Integer;
 newcookies,pos:PQWORD;
begin
 LOG_DEBUG('unionfs_readdir: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 error:=0;
 eofflag:=0;
 locked:=0;
 uio_offset_bk:=0;
 uio:=ap^.a_uio;
 uvp:=nil;
 lvp:=nil;
 //td:=uio^.uio_td;
 ncookies_bk:=0;
 cookies_bk:=nil;

 vp:=ap^.a_vp;

 if (vp^.v_type<>VDIR) then
 begin
  Exit(ENOTDIR);
 end;

 { check the open count. unionfs needs to open before readdir. }
 if (VOP_ISLOCKED(vp)<>LK_EXCLUSIVE) then
 begin
  if (vn_lock(vp, LK_UPGRADE, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%})<>0) then
  begin
   vn_lock(vp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  end;
  locked:=1;
 end;

 unp:=VTOUNIONFS(vp);
 if (unp=nil) then
 begin
  error:=EBADF;
 end else
 begin
  uvp:=unp^.un_uppervp;
  lvp:=unp^.un_lowervp;
  unionfs_get_node_status(unp, @unsp);
  if ((uvp<>nil) AND (unsp^.uns_upper_opencnt <= 0)) OR
     ((lvp<>nil) AND (unsp^.uns_lower_opencnt <= 0)) then
  begin
   unionfs_tryrem_node_status(unp, unsp);
   error:=EBADF;
  end;
 end;

 if (locked<>0) then
 begin
  vn_lock(vp, LK_DOWNGRADE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 end;

 if (error<>0) then
 begin
  goto _unionfs_readdir_exit;
 end;

 { check opaque }
 if (uvp<>nil) AND (lvp<>nil) then
 begin
  error:=VOP_GETATTR(uvp, @va);
  if (error<>0) then
  begin
   goto _unionfs_readdir_exit;
  end;

  if (va.va_flags and OPAQUE)<>0 then
  begin
   lvp:=nil;
  end;
 end;

 { upper only }
 if (uvp<>nil) AND (lvp=nil) then
 begin
  error:=VOP_READDIR(uvp, uio, ap^.a_eofflag, ap^.a_ncookies, ap^.a_cookies);
  unsp^.uns_readdir_status:=0;

  goto _unionfs_readdir_exit;
 end;

 { lower only }
 if (uvp=nil) AND (lvp<>nil) then
 begin
  error:=VOP_READDIR(lvp, uio, ap^.a_eofflag, ap^.a_ncookies, ap^.a_cookies);
  unsp^.uns_readdir_status:=2;

  goto _unionfs_readdir_exit;
 end;

 {
  * readdir upper and lower
  }
 Assert(uvp<>nil, ('unionfs_readdir: null upper vp'));
 Assert(lvp<>nil, ('unionfs_readdir: null lower vp'));

 if (uio^.uio_offset=0) then
 begin
  unsp^.uns_readdir_status:=0;
 end;

 if (unsp^.uns_readdir_status=0) then
 begin
  { read upper }
  error:=VOP_READDIR(uvp, uio, @eofflag, ap^.a_ncookies, ap^.a_cookies);

  if (error<>0) OR (eofflag=0) then
  begin
   goto _unionfs_readdir_exit;
  end;
  unsp^.uns_readdir_status:=1;

  {
   * UFS(and other FS) needs size of uio_resid larger than
   * DIRBLKSIZ.
   * size of DIRBLKSIZ equals DEV_BSIZE.
   * (see: ufs/ufs/ufs_vnops.c ufs_readdir func , ufs/ufs/dir.h)
   }
  if (uio^.uio_resid <= (uio^.uio_resid and (DEV_BSIZE-1))) then
  begin
   goto _unionfs_readdir_exit;
  end;

  {
   * Backup cookies.
   * It prepares to readdir in lower.
   }
  if (ap^.a_ncookies<>nil) then
  begin
   ncookies_bk:=(ap^.a_ncookies)^;
   (ap^.a_ncookies)^:=0;
  end;

  if (ap^.a_cookies<>nil) then
  begin
   cookies_bk:=(ap^.a_cookies)^;
   (ap^.a_cookies)^:=nil;
  end;
 end;

 { initialize for readdir in lower }
 if (unsp^.uns_readdir_status=1) then
 begin
  unsp^.uns_readdir_status:=2;
  {
   * Backup uio_offset. See the comment after the
   * VOP_READDIR call on the lower layer.
   }
  uio_offset_bk:=uio^.uio_offset;
  uio^.uio_offset:=0;
 end;

 if (lvp=nil) then
 begin
  error:=EBADF;
  goto _unionfs_readdir_exit;
 end;

 { read lower }
 error:=VOP_READDIR(lvp, uio, ap^.a_eofflag, ap^.a_ncookies, ap^.a_cookies);

 {
  * We can't return an uio_offset of 0: this would trigger an
  * infinite loop, because the next call to unionfs_readdir would
  * always restart with the upper layer (uio_offset=0) and
  * always return some data.
  *
  * This happens when the lower layer root directory is removed.
  * (A root directory deleting of unionfs should not be permitted.
  *  But current VFS can not do it.)
  }
 if (uio^.uio_offset=0) then
 begin
  uio^.uio_offset:=uio_offset_bk;
 end;

 if (cookies_bk<>nil) then
 begin
  { merge cookies }
  size:=(ap^.a_ncookies)^ + ncookies_bk;
  newcookies:=malloc(size * sizeof(QWORD));
  pos:=newcookies;

  memcpy(pos, cookies_bk, ncookies_bk * sizeof(QWORD));
  pos:=pos + ncookies_bk;
  memcpy(pos, (ap^.a_cookies)^, (ap^.a_ncookies)^ * sizeof(QWORD));
  free(cookies_bk);
  free((ap^.a_cookies)^);
  (ap^.a_ncookies)^:=size;
  (ap^.a_cookies)^:=newcookies;
 end;

_unionfs_readdir_exit:
 if (error<>0) AND (ap^.a_eofflag<>nil) then
 begin
  (ap^.a_eofflag)^:=1;
 end;

 LOG_DEBUG('unionfs_readdir: leave ', error);

 Exit(error);
end;

function unionfs_readlink(ap:p_vop_readlink_args):Integer;
var
 error:Integer;
 unp:p_unionfs_node;
 vp:p_vnode;
begin
 LOG_DEBUG('unionfs_readlink: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 unp:=VTOUNIONFS(ap^.a_vp);

 if (unp^.un_uppervp<>nil) then
  vp:=unp^.un_uppervp
 else
  vp:=unp^.un_lowervp;

 error:=VOP_READLINK(vp, ap^.a_uio);

 LOG_DEBUG('unionfs_readlink: leave ', error);

 Exit(error);
end;

function unionfs_getwritemount(ap:p_vop_getwritemount_args):Integer;
var
 error:Integer;
 uvp:p_vnode;
 vp:p_vnode;
begin
 LOG_DEBUG('unionfs_getwritemount: enter');

 error:=0;
 vp:=ap^.a_vp;

 if (vp=nil) OR ((p_mount(vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0) then
 begin
  Exit(EACCES);
 end;

 KASSERT_UNIONFS_VNODE(vp);

 uvp:=UNIONFSVPTOUPPERVP(vp, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 if (uvp=nil) AND (VREG=vp^.v_type) then
 begin
  uvp:=UNIONFSVPTOUPPERVP(VTOUNIONFS(vp)^.un_dvp, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 end;

 if (uvp<>nil) then
 begin
  error:=VOP_GETWRITEMOUNT(uvp, ap^.a_mpp);
 end else
 begin
  VI_LOCK(vp);

  if (vp^.v_iflag and VI_FREE)<>0 then
   error:=EOPNOTSUPP
  else
   error:=EACCES;

  VI_UNLOCK(vp);
 end;

 LOG_DEBUG('unionfs_getwritemount: leave (', error, ')');

 Exit(error);
end;

function unionfs_inactive(ap:p_vop_inactive_args):Integer;
begin
 ap^.a_vp^.v_object:=nil;
 vrecycle(ap^.a_vp);
 Exit(0);
end;

function unionfs_reclaim(ap:p_vop_reclaim_args):Integer;
begin
 { LOG_DEBUG('unionfs_reclaim: enter'); }

 unionfs_noderem(ap^.a_vp);

 { LOG_DEBUG('unionfs_reclaim: leave'); }

 Exit(0);
end;

function unionfs_print(ap:p_vop_print_args):Integer;
var
 unp:p_unionfs_node;
 { unsp:p_unionfs_node_status; }
begin
 unp:=VTOUNIONFS(ap^.a_vp);
 { unionfs_get_node_status(unp, curthread, @unsp); }

 Writeln('unionfs_vp=', HexStr(ap^.a_vp), ' uppervp=', HexStr(unp^.un_uppervp), ' lowervp=', HexStr(unp^.un_lowervp));

 {
 printf('unionfs opencnt: uppervp=%d, lowervp=%d',
     unsp^.uns_upper_opencnt, unsp^.uns_lower_opencnt);
 }

 if (unp^.un_uppervp<>nil) then Writeln('unionfs: upper', HexStr(unp^.un_uppervp));
 if (unp^.un_lowervp<>nil) then Writeln('unionfs: lower', HexStr(unp^.un_lowervp));

 Exit(0);
end;

function unionfs_islocked(ap:p_vop_islocked_args):Integer;
var
 unp:p_unionfs_node;
begin
 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 unp:=VTOUNIONFS(ap^.a_vp);

 if (unp=nil) then
 begin
  Exit(vop_stdislocked(ap));
 end;

 if (unp^.un_uppervp<>nil) then
 begin
  Exit(VOP_ISLOCKED(unp^.un_uppervp));
 end;

 if (unp^.un_lowervp<>nil) then
 begin
  Exit(VOP_ISLOCKED(unp^.un_lowervp));
 end;

 Exit(vop_stdislocked(ap));
end;

function unionfs_get_llt_revlock(vp:p_vnode;flags:Integer):Integer;
var
 revlock:Integer;
begin
 revlock:=0;

 case (flags and LK_TYPE_MASK) of
  LK_SHARED:
   begin
    if (VOP_ISLOCKED(vp)=LK_EXCLUSIVE) then
     revlock:=LK_UPGRADE
    else
     revlock:=LK_RELEASE;
   end;
  LK_EXCLUSIVE,
  LK_UPGRADE:
   revlock:=LK_RELEASE;
  LK_DOWNGRADE:
   revlock:=LK_UPGRADE;
  else;
 end;

 Exit(revlock);
end;

{
 * The state of an acquired lock is adjusted similarly to
 * the time of error generating.
 * flags: LK_RELEASE or LK_UPGRADE
 }
procedure unionfs_revlock(vp:p_vnode;flags:Integer);
begin
 if (flags and LK_RELEASE)<>0 then
 begin
  VOP_UNLOCK(vp, flags);
 end else
 begin
  { UPGRADE }
  if (vn_lock(vp, flags, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%})<>0) then
  begin
   vn_lock(vp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  end;
 end;
end;

function unionfs_lock(ap:p_vop_lock1_args):Integer;
label
 _unionfs_lock_null_vnode;
var
 error:Integer;
 flags:Integer;
 revlock:Integer;
 interlock:Integer;
 uhold:Integer;
 mp:p_mount;
 ump:p_unionfs_mount;
 unp:p_unionfs_node;
 vp:p_vnode;
 uvp:p_vnode;
 lvp:p_vnode;
begin
 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 error:=0;
 interlock:=1;
 uhold:=0;
 flags:=ap^.a_flags;
 vp:=ap^.a_vp;

 if (LK_RELEASE=(flags and LK_TYPE_MASK)) OR ((flags and LK_TYPE_MASK)=0) then
 begin
  Exit(VOP_UNLOCK(vp, flags or LK_RELEASE));
 end;

 if ((flags and LK_INTERLOCK)=0) then
 begin
  VI_LOCK(vp);
 end;

 mp:=vp^.v_mount;

 if (mp=nil) then
 begin
  goto _unionfs_lock_null_vnode;
 end;

 ump:=MOUNTTOUNIONFSMOUNT(mp);
 unp:=VTOUNIONFS(vp);

 if (ump=nil) OR (unp=nil) then
 begin
  goto _unionfs_lock_null_vnode;
 end;

 lvp:=unp^.un_lowervp;
 uvp:=unp^.un_uppervp;

 revlock:=unionfs_get_llt_revlock(vp, flags);
 if (revlock=0) then
 begin
  Assert(False,'unknown lock type: '+HexStr(flags and LK_TYPE_MASK,8));
 end;

 if ((mp^.mnt_kern_flag and MNTK_MPSAFE)<>0) AND
    ((vp^.v_iflag and VI_OWEINACT)<>0) then
 begin
  flags:=flags or LK_NOWAIT;
 end;

 {
  * Sometimes, lower or upper is already exclusive locked.
  * (ex. vfs_domount: mounted vnode is already locked.)
  }
 if ((flags and LK_TYPE_MASK)=LK_EXCLUSIVE) AND (vp=ump^.um_rootvp) then
 begin
  flags:=flags or LK_CANRECURSE;
 end;

 if (lvp<>nil) then
 begin
  if (uvp<>nil) AND ((flags and LK_UPGRADE)<>0) then
  begin
   { Share Lock is once released and a deadlock is avoided.  }
   VI_LOCK(uvp);
   vholdl(uvp);
   uhold:=1;
   VI_UNLOCK(vp);
   VOP_UNLOCK(uvp, LK_RELEASE or LK_INTERLOCK);
   VI_LOCK(vp);
   unp:=VTOUNIONFS(vp);
   if (unp=nil) then
   begin
    { vnode is released. }
    VI_UNLOCK(vp);
    VOP_UNLOCK(lvp, LK_RELEASE);
    vdrop(uvp);
    Exit(EBUSY);
   end;
  end;

  VI_LOCK(lvp);
  flags:=flags or LK_INTERLOCK;
  vholdl(lvp);

  VI_UNLOCK(vp);
  ap^.a_flags:=ap^.a_flags and (not LK_INTERLOCK);

  error:=VOP_LOCK(lvp, flags, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});

  VI_LOCK(vp);
  unp:=VTOUNIONFS(vp);

  if (unp=nil) then
  begin
   { vnode is released. }
   VI_UNLOCK(vp);

   if (error=0) then
   begin
    VOP_UNLOCK(lvp, LK_RELEASE);
   end;

   vdrop(lvp);

   if (uhold<>0) then
   begin
    vdrop(uvp);
   end;

   Exit(vop_stdlock(ap));
  end;

 end;

 if (error=0) AND (uvp<>nil) then
 begin
  if (uhold<>0) AND ((flags and LK_UPGRADE)<>0) then
  begin
   flags:=flags and (not LK_TYPE_MASK);
   flags:=flags or LK_EXCLUSIVE;
  end;

  VI_LOCK(uvp);
  flags:=flags or LK_INTERLOCK;

  if (uhold=0) then
  begin
   vholdl(uvp);
   uhold:=1;
  end;

  VI_UNLOCK(vp);
  ap^.a_flags:=ap^.a_flags and (not LK_INTERLOCK);

  error:=VOP_LOCK(uvp, flags, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});

  VI_LOCK(vp);
  unp:=VTOUNIONFS(vp);

  if (unp=nil) then
  begin
   { vnode is released. }
   VI_UNLOCK(vp);

   if (error=0) then
   begin
    VOP_UNLOCK(uvp, LK_RELEASE);
   end;

   vdrop(uvp);

   if (lvp<>nil) then
   begin
    VOP_UNLOCK(lvp, LK_RELEASE);
    vdrop(lvp);
   end;

   Exit(vop_stdlock(ap));
  end;

  if (error<>0) AND (lvp<>nil) then
  begin
   { rollback }
   VI_UNLOCK(vp);
   unionfs_revlock(lvp, revlock);
   interlock:=0;
  end;

 end;

 if (interlock<>0) then
 begin
  VI_UNLOCK(vp);
 end;
 if (lvp<>nil) then
 begin
  vdrop(lvp);
 end;
 if (uhold<>0) then
 begin
  vdrop(uvp);
 end;

 Exit(error);

_unionfs_lock_null_vnode:
 ap^.a_flags:=ap^.a_flags or LK_INTERLOCK;
 Exit(vop_stdlock(ap));
end;

function unionfs_unlock(ap:p_vop_unlock_args):Integer;
label
 _unionfs_unlock_null_vnode;
var
 error:Integer;
 flags:Integer;
 mtxlkflag:Integer;
 uhold:Integer;
 vp:p_vnode;
 lvp:p_vnode;
 uvp:p_vnode;
 unp:p_unionfs_node;
begin
 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 error:=0;
 mtxlkflag:=0;
 uhold:=0;
 flags:=ap^.a_flags or LK_RELEASE;
 vp:=ap^.a_vp;

 if ((flags and LK_INTERLOCK)<>0) then
 begin
  mtxlkflag:=1;
 end else
 if not mtx_owned(VI_MTX(vp)^) then
 begin
  VI_LOCK(vp);
  mtxlkflag:=2;
 end;

 unp:=VTOUNIONFS(vp);
 if (unp=nil) then
 begin
  goto _unionfs_unlock_null_vnode;
 end;

 lvp:=unp^.un_lowervp;
 uvp:=unp^.un_uppervp;

 if (lvp<>nil) then
 begin
  VI_LOCK(lvp);
  flags:=flags or LK_INTERLOCK;
  vholdl(lvp);

  VI_UNLOCK(vp);
  ap^.a_flags:=ap^.a_flags and (not LK_INTERLOCK);

  error:=VOP_UNLOCK(lvp, flags);

  VI_LOCK(vp);
 end;

 if (error=0) AND (uvp<>nil) then
 begin
  VI_LOCK(uvp);
  flags:=flags or LK_INTERLOCK;
  vholdl(uvp);
  uhold:=1;

  VI_UNLOCK(vp);
  ap^.a_flags:=ap^.a_flags and (not LK_INTERLOCK);

  error:=VOP_UNLOCK(uvp, flags);

  VI_LOCK(vp);
 end;

 VI_UNLOCK(vp);
 if (lvp<>nil) then
 begin
  vdrop(lvp);
 end;
 if (uhold<>0) then
 begin
  vdrop(uvp);
 end;
 if (mtxlkflag=0) then
 begin
  VI_LOCK(vp);
 end;

 Exit(error);

_unionfs_unlock_null_vnode:
 if (mtxlkflag=2) then
 begin
  VI_UNLOCK(vp);
 end;

 Exit(vop_stdunlock(ap));
end;

function unionfs_pathconf(ap:p_vop_pathconf_args):Integer;
var
 unp:p_unionfs_node;
 vp:p_vnode;
begin
 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 unp:=VTOUNIONFS(ap^.a_vp);

 if (unp^.un_uppervp<>nil) then
  vp:=unp^.un_uppervp
 else
  vp:=unp^.un_lowervp;

 Exit(VOP_PATHCONF(vp, ap^.a_name, ap^.a_retval));
end;

function unionfs_advlock(ap:p_vop_advlock_args):Integer;
label
 _unionfs_advlock_abort;
var
 error:Integer;
 unp:p_unionfs_node;
 unsp:p_unionfs_node_status;
 vp:p_vnode;
 uvp:p_vnode;
 //struct thread  *td;
begin
 LOG_DEBUG('unionfs_advlock: enter');

 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 vp:=ap^.a_vp;
 //td:=curthread;

 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});

 unp:=VTOUNIONFS(ap^.a_vp);
 uvp:=unp^.un_uppervp;

 if (uvp=nil) then
 begin
  error:=unionfs_copyfile(unp, 1);
  if (error<>0) then
  begin
   goto _unionfs_advlock_abort;
  end;

  uvp:=unp^.un_uppervp;

  unionfs_get_node_status(unp, @unsp);
  if (unsp^.uns_lower_opencnt > 0) then
  begin
   { try reopen the vnode }
   error:=VOP_OPEN(uvp, unsp^.uns_lower_openmode, nil, nil);
   if (error<>0) then
   begin
    goto _unionfs_advlock_abort;
   end;

   Inc(unsp^.uns_upper_opencnt);
   VOP_CLOSE(unp^.un_lowervp, unsp^.uns_lower_openmode);
   Dec(unsp^.uns_lower_opencnt);
  end else
  begin
   unionfs_tryrem_node_status(unp, unsp);
  end;
 end;

 VOP_UNLOCK(vp, LK_RELEASE);

 error:=VOP_ADVLOCK(uvp, ap^.a_id, ap^.a_op, ap^.a_fl, ap^.a_flags);

 LOG_DEBUG('unionfs_advlock: leave (', error, ')');

 Exit(error);

_unionfs_advlock_abort:
 VOP_UNLOCK(vp, LK_RELEASE);

 LOG_DEBUG('unionfs_advlock: leave (', error, ')');

 Exit(error);
end;

function unionfs_strategy(ap:p_vop_strategy_args):Integer;
var
 unp:p_unionfs_node;
 vp:p_vnode;
begin
 KASSERT_UNIONFS_VNODE(ap^.a_vp);

 unp:=VTOUNIONFS(ap^.a_vp);

 if (unp^.un_uppervp<>nil) then
  vp:=unp^.un_uppervp
 else
  vp:=unp^.un_lowervp;

 Assert(vp<>nil,'unionfs_strategy: nil');

 //if (ap^.a_bp^.b_iocmd=BIO_WRITE) AND (vp=unp^.un_lowervp) then
 //begin
 // Assert(False,'unionfs_strategy: writing to lowervp');
 //end;

 Exit(VOP_STRATEGY(vp, ap^.a_bp));
end;

function unionfs_vptofh(ap:p_vop_vptofh_args):Integer;
begin
 Exit(EOPNOTSUPP);
end;

var
 unionfs_vnodeops:vop_vector=(
  vop_default       :@default_vnodeops;
  vop_bypass        :nil;

  vop_islocked      :@unionfs_islocked;
  vop_lookup        :@unionfs_lookup;
  vop_create        :@unionfs_create;
  vop_whiteout      :@unionfs_whiteout;
  vop_mknod         :@unionfs_mknod;
  vop_open          :@unionfs_open;
  vop_close         :@unionfs_close;
  vop_access        :@unionfs_access;
  vop_accessx       :nil;
  vop_getattr       :@unionfs_getattr;
  vop_setattr       :@unionfs_setattr;
  vop_markatime     :nil;
  vop_read          :@unionfs_read;
  vop_write         :@unionfs_write;
  vop_ioctl         :@unionfs_ioctl;
  vop_poll          :@unionfs_poll;
  vop_kqfilter      :nil;
  vop_revoke        :nil;
  vop_fsync         :@unionfs_fsync;
  vop_remove        :@unionfs_remove;
  vop_link          :@unionfs_link;
  vop_rename        :@unionfs_rename;
  vop_mkdir         :@unionfs_mkdir;
  vop_rmdir         :@unionfs_rmdir;
  vop_symlink       :@unionfs_symlink;
  vop_readdir       :@unionfs_readdir;
  vop_readlink      :@unionfs_readlink;
  vop_inactive      :@unionfs_inactive;
  vop_reclaim       :@unionfs_reclaim;
  vop_lock1         :@unionfs_lock;
  vop_unlock        :@unionfs_unlock;
  vop_bmap          :@VOP_EOPNOTSUPP;
  vop_strategy      :@unionfs_strategy;
  vop_getwritemount :@unionfs_getwritemount;
  vop_print         :@unionfs_print;
  vop_pathconf      :@unionfs_pathconf;
  vop_advlock       :@unionfs_advlock;
  vop_advlockasync  :nil;
  vop_advlockpurge  :nil;
  vop_reallocblks   :nil;
  vop_getpages      :nil;
  vop_putpages      :nil;
  vop_vptofh        :@unionfs_vptofh;
  vop_vptocnp       :nil;
  vop_allocate      :nil;
  vop_unp_bind      :nil;
  vop_unp_connect   :nil;
  vop_unp_detach    :nil;
  vop_get_int_obj   :nil;
 ); public;


end.

