unit union_subr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 errno,
 mqueue,
 unionfs,
 uma,
 vuio,
 vnode,
 vmount,
 vnamei,
 vfcntl,
 vdirent,
 vstat,
 vfs_default,
 vfs_subr,
 vfs_vnops,
 vfs_lookup,
 vnode_if,
 kern_param,
 kern_mtx,
 kern_proc,
 kern_thr;

const
 NUNIONFSNODECACHE=16;

implementation

uses
 vfiledesc,
 subr_hash,
 kern_malloc;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

function strcmp(s1,s2:PChar):Integer;
begin
 while (s1^ = s2^) do
 begin
  if (s1^ = #0) then Exit(0);
  Inc(s1);
  Inc(s2);
 end;
 Result:=Ord(s1^) - Ord(s2^);
end;

{
 * Initialize
 }
function unionfs_init(vfsp:p_vfsconf):Integer; public;
begin
 LOG_DEBUG('unionfs_init'); { printed during system boot }
 Exit(0);
end;

{
 * Uninitialize
 }
function unionfs_uninit(vfsp:p_vfsconf):Integer; public;
begin
 Exit(0);
end;

function unionfs_get_hashhead(dvp:p_vnode;path:PChar):p_unionfs_node_hashhead;
var
 count:Integer;
 hash:Byte;
 unp:p_unionfs_node;
begin
 hash:=0;
 unp:=VTOUNIONFS(dvp);

 if (path<>nil) then
 begin
  count:=0;
  while (path[count]<>#0) do
  begin
   hash:=hash + ord(path[count]);
   //
   Inc(count);
  end;
 end;

 Exit(@(unp^.un_hashtbl[hash and unp^.un_hashmask]));
end;

{
 * Get the cached vnode.
 }
function unionfs_get_cached_vnode(uvp,lvp,dvp:p_vnode;path:PChar):p_vnode;
var
 hd:p_unionfs_node_hashhead;
 unp:p_unionfs_node;
 vp:p_vnode;
begin
 Assert((uvp=nil) OR (uvp^.v_type=VDIR),'unionfs_get_cached_vnode: v_type<>VDIR');
 Assert((lvp=nil) OR (lvp^.v_type=VDIR),'unionfs_get_cached_vnode: v_type<>VDIR');

 VI_LOCK(dvp);

 hd:=unionfs_get_hashhead(dvp, path);

 //LIST_FOREACH(unp, hd, un_hash)

 unp:=LIST_FIRST(hd);
 while (unp<>nil) do
 begin
  if (strcmp(unp^.un_path, path)=0) then
  begin
   vp:=UNIONFSTOV(unp);
   VI_LOCK(vp);
   VI_UNLOCK(dvp);
   vp^.v_iflag:=vp^.v_iflag and (not VI_OWEINACT);
   if ((vp^.v_iflag and (VI_DOOMED or VI_DOINGINACT))<>0) then
   begin
    VI_UNLOCK(vp);
    vp:=nil;
   end else
   begin
    VI_UNLOCK(vp);
   end;
   Exit(vp);
  end;
  //
  unp:=LIST_NEXT(unp,@unp^.un_hash);
 end;

 VI_UNLOCK(dvp);

 Exit(nil);
end;

{
 * Add the new vnode into cache.
 }
function unionfs_ins_cached_vnode(uncp:p_unionfs_node;dvp:p_vnode;path:PChar):p_vnode;
var
 hd:p_unionfs_node_hashhead;
 unp:p_unionfs_node;
 vp:p_vnode;
begin
 Assert((uncp^.un_uppervp=nil) OR (uncp^.un_uppervp^.v_type=VDIR),'unionfs_ins_cached_vnode: v_type<>VDIR');
 Assert((uncp^.un_lowervp=nil) OR (uncp^.un_lowervp^.v_type=VDIR),'unionfs_ins_cached_vnode: v_type<>VDIR');

 VI_LOCK(dvp);
 hd:=unionfs_get_hashhead(dvp, path);

 unp:=LIST_FIRST(hd);
 while (unp<>nil) do
 begin
  if (strcmp(unp^.un_path, path)=0) then
  begin
   vp:=UNIONFSTOV(unp);
   VI_LOCK(vp);
   vp^.v_iflag:=vp^.v_iflag and (not VI_OWEINACT);
   if ((vp^.v_iflag and (VI_DOOMED or VI_DOINGINACT))<>0) then
   begin
    LIST_INSERT_HEAD(hd, uncp, @uncp^.un_hash);
    VI_UNLOCK(vp);
    vp:=nil;
   end else
   begin
    VI_UNLOCK(vp);
   end;
   VI_UNLOCK(dvp);
   Exit(vp);
  end;
  //
  unp:=LIST_NEXT(unp,@unp^.un_hash);
 end;

 LIST_INSERT_HEAD(hd, uncp, @uncp^.un_hash);
 VI_UNLOCK(dvp);

 Exit(nil);
end;

{
 * Remove the vnode.
 }
procedure unionfs_rem_cached_vnode(unp:p_unionfs_node;dvp:p_vnode);
begin
 Assert(unp<>nil,'unionfs_rem_cached_vnode: null node');
 Assert(dvp<>nil,'unionfs_rem_cached_vnode: null parent vnode');
 Assert(unp^.un_hash.le_prev<>nil,'unionfs_rem_cached_vnode: null hash');

 VI_LOCK(dvp);
 LIST_REMOVE(unp, @unp^.un_hash);
 unp^.un_hash.le_next:=nil;
 unp^.un_hash.le_prev:=nil;
 VI_UNLOCK(dvp);
end;

{
 * Make a new or get existing unionfs node.
 *
 * uppervp and lowervp should be unlocked. Because if new unionfs vnode is
 * locked, uppervp or lowervp is locked too. In order to prevent dead lock,
 * you should not lock plurality simultaneously.
 }
function unionfs_nodeget(mp:p_mount;uppervp,lowervp,dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname):Integer; public;
label
 _unionfs_nodeget_out;
var
 ump:p_unionfs_mount;
 unp:p_unionfs_node;
 vp:p_vnode;
 error:Integer;
 lkflags:Integer;
 vt:vtype;
 path:PChar;
begin
 ump:=MOUNTTOUNIONFSMOUNT(mp);

 if (cnp<>nil) then
 begin
  lkflags:=cnp^.cn_lkflags;
  path   :=cnp^.cn_nameptr;
 end else
 begin
  lkflags:=0;
  path   :=nil;
 end;

 vpp^:=nil;

 if (uppervp=nil) AND (lowervp=nil) then
 begin
  LOG_CRITICAL(stderr,'unionfs_nodeget: upper and lower is null');
  Assert(False,'unionfs_nodeget: upper and lower is null');
 end;

 if (uppervp<>nil) then
 begin
  vt:=uppervp^.v_type;
 end else
 begin
  vt:=lowervp^.v_type;
 end;

 { If it has no ISLASTCN flag, path check is skipped. }
 if (cnp<>nil) AND ((cnp^.cn_flags and ISLASTCN)=0) then
 begin
  path:=nil;
 end;

 { check the cache }
 if (path<>nil) AND (dvp<>nil) AND (vt=VDIR) then
 begin
  vp:=unionfs_get_cached_vnode(uppervp, lowervp, dvp, path);
  if (vp<>nil) then
  begin
   vref(vp);
   vpp^:=vp;
   goto _unionfs_nodeget_out;
  end;
 end;

 if (uppervp=nil) OR (ump^.um_uppervp<>uppervp) OR
    (lowervp=nil) OR (ump^.um_lowervp<>lowervp) then
 begin
  { dvp will be nil only in case of root vnode. }
  if (dvp=nil) then Exit(EINVAL);
 end;

 {
  * Do the MALLOC before the getnewvnode since doing so afterward
  * might cause a bogus v_data pointer to get dereferenced elsewhere
  * if MALLOC should block.
  }
 unp:=calloc(sizeof(unionfs_node));

 error:=getnewvnode('unionfs', mp, @unionfs_vnodeops, @vp);
 if (error<>0) then
 begin
  free(unp);
  Exit(error);
 end;

 error:=insmntque(vp, mp); { XXX: Too early for mpsafe fs }
 if (error<>0) then
 begin
  free(unp);
  Exit(error);
 end;

 if (dvp<>nil) then
 begin
  vref(dvp);
 end;
 if (uppervp<>nil) then
 begin
  vref(uppervp);
 end;
 if (lowervp<>nil) then
 begin
  vref(lowervp);
 end;

 if (vt=VDIR) then
 begin
  unp^.un_hashtbl:=hashinit(NUNIONFSNODECACHE, @(unp^.un_hashmask));
 end;

 unp^.un_vnode  :=vp;
 unp^.un_uppervp:=uppervp;
 unp^.un_lowervp:=lowervp;
 unp^.un_dvp    :=dvp;

 if (uppervp<>nil) then
  vp^.v_vnlock:=uppervp^.v_vnlock
 else
  vp^.v_vnlock:=lowervp^.v_vnlock;

 if (path<>nil) then
 begin
  unp^.un_path:=calloc(cnp^.cn_namelen+1);
  Move(cnp^.cn_nameptr^, unp^.un_path^, cnp^.cn_namelen);
  unp^.un_path[cnp^.cn_namelen]:=#0;
 end;

 vp^.v_type:=vt;
 vp^.v_data:=unp;

 if (uppervp<>nil) AND (ump^.um_uppervp=uppervp) AND
    (lowervp<>nil) AND (ump^.um_lowervp=lowervp) then
 begin
  vp^.v_vflag:=vp^.v_vflag or VV_ROOT;
 end;

 if (path<>nil) AND (dvp<>nil) AND (vt=VDIR) then
 begin
  vpp^:=unionfs_ins_cached_vnode(unp, dvp, path);
 end;

 if ((vpp^)<>nil) then
 begin
  if (dvp<>nil) then
  begin
   vrele(dvp);
  end;
  if (uppervp<>nil) then
  begin
   vrele(uppervp);
  end;
  if (lowervp<>nil) then
  begin
   vrele(lowervp);
  end;

  unp^.un_uppervp:=nil;
  unp^.un_lowervp:=nil;
  unp^.un_dvp    :=nil;

  vrele(vp);
  vp:=vpp^;
  vref(vp);
 end else
 begin
  vpp^:=vp;
 end;

_unionfs_nodeget_out:
 if (lkflags and LK_TYPE_MASK)<>0 then
 begin
  vn_lock(vp, lkflags or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 end;

 Exit(0);
end;

{
 * Clean up the unionfs node.
 }
procedure unionfs_noderem(vp:p_vnode); public;
var
 vfslocked:Integer;
 count:Integer;
 unp,unp_t1,unp_t2:p_unionfs_node;
 hd:p_unionfs_node_hashhead;
 unsp,unsp_tmp:p_unionfs_node_status;
 lvp:p_vnode;
 uvp:p_vnode;
 dvp:p_vnode;
begin
 {
  * Use the interlock to protect the clearing of v_data to
  * prevent faults in unionfs_lock().
  }
 VI_LOCK(vp);
 unp:=VTOUNIONFS(vp);
 lvp:=unp^.un_lowervp;
 uvp:=unp^.un_uppervp;
 dvp:=unp^.un_dvp;
 unp^.un_lowervp:=nil;
 unp^.un_uppervp:=nil;
 vp^.v_vnlock:=@(vp^.v_lock);
 vp^.v_data:=nil;
 vp^.v_object:=nil;
 VI_UNLOCK(vp);

 if (lvp<>nil) then
 begin
  VOP_UNLOCK(lvp, LK_RELEASE);
 end;
 if (uvp<>nil) then
 begin
  VOP_UNLOCK(uvp, LK_RELEASE);
 end;

 if (dvp<>nil) AND (unp^.un_hash.le_prev<>nil) then
 begin
  unionfs_rem_cached_vnode(unp, dvp);
 end;

 if (lockmgr(vp^.v_vnlock, LK_EXCLUSIVE, VI_MTX(vp))<>0) then
 begin
  LOG_CRITICAL(stderr,'the lock for deletion is unacquirable.');
  Assert(False,'the lock for deletion is unacquirable.');
 end;

 if (lvp<>nil) then
 begin
  vfslocked:=VFS_LOCK_GIANT(lvp^.v_mount);
  vrele(lvp);
  VFS_UNLOCK_GIANT(vfslocked);
 end;

 if (uvp<>nil) then
 begin
  vfslocked:=VFS_LOCK_GIANT(uvp^.v_mount);
  vrele(uvp);
  VFS_UNLOCK_GIANT(vfslocked);
 end;

 if (dvp<>nil) then
 begin
  vfslocked:=VFS_LOCK_GIANT(dvp^.v_mount);
  vrele(dvp);
  VFS_UNLOCK_GIANT(vfslocked);
  unp^.un_dvp:=nil;
 end;

 if (unp^.un_path<>nil) then
 begin
  free(unp^.un_path);
  unp^.un_path:=nil;
 end;

 if (unp^.un_hashtbl<>nil) then
 begin
  For count:=0 to unp^.un_hashmask do
  begin
   hd:=unp^.un_hashtbl + count;

   unp_t1:=LIST_FIRST(hd);
   while (unp_t1<>nil) do
   begin
    unp_t2:=LIST_NEXT(unp_t1, @unp_t1^.un_hash);
    //
    LIST_REMOVE(unp_t1, @unp_t1^.un_hash);
    unp_t1^.un_hash.le_next:=nil;
    unp_t1^.un_hash.le_prev:=nil;
    //
    unp_t1:=unp_t2;
   end;
  end;
  hashdestroy(unp^.un_hashtbl, unp^.un_hashmask);
 end;

 unsp:=LIST_FIRST(@(unp^.un_unshead));
 while (unsp<>nil) do
 begin
  unsp_tmp:=LIST_NEXT(unsp, @unsp^.uns_list);
  //
  LIST_REMOVE(unsp, @unsp^.uns_list);
  free(unsp);
  //
  unsp:=unsp_tmp;
 end;

 free(unp);
end;

{
 * Get the unionfs node status.
 * You need exclusive lock this vnode.
 }
procedure unionfs_get_node_status(unp:p_unionfs_node;unspp:pp_unionfs_node_status); public;
var
 unsp:p_unionfs_node_status;
 pid:Integer;
begin
 pid:=p_proc.p_pid;

 Assert(nil<>unspp, ('null pointer'));
 ASSERT_VOP_ELOCKED(UNIONFSTOV(unp), 'unionfs_get_node_status');

 unsp:=LIST_FIRST(@(unp^.un_unshead));
 while (unsp<>nil) do
 begin
  if (unsp^.uns_pid=pid) then
  begin
   unspp^:=unsp;
   Exit;
  end;
  //
  unsp:=LIST_NEXT(unsp, @unsp^.uns_list);
 end;

 { create a new unionfs node status }
 unsp:=calloc(sizeof(unionfs_node_status));

 unsp^.uns_pid:=pid;
 LIST_INSERT_HEAD(@(unp^.un_unshead), unsp, @unsp^.uns_list);

 unspp^:=unsp;
end;

{
 * Remove the unionfs node status, if you can.
 * You need exclusive lock this vnode.
 }
procedure unionfs_tryrem_node_status(unp:p_unionfs_node;unsp:p_unionfs_node_status); public;
begin
 Assert(nil<>unsp, 'null pointer');
 ASSERT_VOP_ELOCKED(UNIONFSTOV(unp), 'unionfs_get_node_status');

 if (0 < unsp^.uns_lower_opencnt) OR (0 < unsp^.uns_upper_opencnt) then
 begin
  Exit;
 end;

 LIST_REMOVE(unsp, @unsp^.uns_list);
 free(unsp);
end;

{
 * Create upper node attr.
 }
procedure unionfs_create_uppervattr_core(ump:p_unionfs_mount;lva,uva:p_vattr); public;
begin
 VATTR_NULL(uva);

 uva^.va_type :=lva^.va_type;
 uva^.va_atime:=lva^.va_atime;
 uva^.va_mtime:=lva^.va_mtime;
 uva^.va_ctime:=lva^.va_ctime;

 case (ump^.um_copymode) of
  UNIONFS_TRANSPARENT:
   begin
    uva^.va_mode:=lva^.va_mode;
    uva^.va_uid :=lva^.va_uid;
    uva^.va_gid :=lva^.va_gid;
   end;
  UNIONFS_MASQUERADE:
   if (ump^.um_uid=lva^.va_uid) then
   begin
    uva^.va_mode:=lva^.va_mode and &077077;

    if (lva^.va_type=VDIR) then
    begin
     uva^.va_mode:=uva^.va_mode or (ump^.um_udir and &0700);
    end else
    begin
     uva^.va_mode:=uva^.va_mode or (ump^.um_ufile and &0700);
    end;

    uva^.va_uid:=lva^.va_uid;
    uva^.va_gid:=lva^.va_gid;
   end else
   begin

    if (lva^.va_type=VDIR) then
    begin
     uva^.va_mode:=ump^.um_udir;
    end else
    begin
     uva^.va_mode:=ump^.um_ufile;
    end;

    uva^.va_uid:=ump^.um_uid;
    uva^.va_gid:=ump^.um_gid;
   end;
  else  { UNIONFS_TRADITIONAL }
   begin
    uva^.va_mode:=&0777 and (not fd_table.fd_cmask);
    uva^.va_uid :=ump^.um_uid;
    uva^.va_gid :=ump^.um_gid;
   end;
 end;

end;

{
 * Create upper node attr.
 }
function unionfs_create_uppervattr(ump:p_unionfs_mount;lvp:p_vnode;uva:p_vattr):Integer; public;
var
 error:Integer;
 lva:t_vattr;
begin
 error:=VOP_GETATTR(lvp, @lva);
 if (error<>0) then Exit(error);

 unionfs_create_uppervattr_core(ump, @lva, uva);

 Exit(error);
end;

{
 * relookup
 *
 * dvp should be locked on entry and will be locked on return.
 *
 * If an error is returned, *vpp will be invalid, otherwise it will hold a
 * locked, referenced vnode. If *vpp=dvp then remember that only one
 * LK_EXCLUSIVE lock is held.
 }
function unionfs_relookup(dvp:p_vnode;vpp:pp_vnode;cnp,cn:p_componentname;path:PChar;pathlen:Integer;nameiop:QWORD):Integer; public;
var
 error:Integer;
begin
 cn^.cn_namelen:=pathlen;
 cn^.cn_pnbuf:=uma_zalloc(namei_zone, M_WAITOK);
 Move(path^, cn^.cn_pnbuf^, pathlen);
 cn^.cn_pnbuf[pathlen]:=#0;

 cn^.cn_nameiop:=nameiop;
 cn^.cn_flags  :=(LOCKPARENT or LOCKLEAF or HASBUF or SAVENAME or ISLASTCN);
 cn^.cn_lkflags:=LK_EXCLUSIVE;
 cn^.cn_thread :=curkthread;

 cn^.cn_nameptr:=cn^.cn_pnbuf;
 cn^.cn_consume:=cnp^.cn_consume;

 if (nameiop=DELETE) then
 begin
  cn^.cn_flags:=cn^.cn_flags or (cnp^.cn_flags and (DOWHITEOUT or SAVESTART));
 end else if (RENAME=nameiop) then
 begin
  cn^.cn_flags:=cn^.cn_flags or (cnp^.cn_flags and SAVESTART);
 end;

 vref(dvp);
 VOP_UNLOCK(dvp, LK_RELEASE);

 error:=nd_relookup(dvp, vpp, cn);
 if (error<>0) then
 begin
  uma_zfree(namei_zone, cn^.cn_pnbuf);
  cn^.cn_flags:=cn^.cn_flags and (not HASBUF);
  vn_lock(dvp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 end else
 begin
  vrele(dvp);
 end;

 Exit(error);
end;

{
 * relookup for CREATE namei operation.
 *
 * dvp is unionfs vnode. dvp should be locked.
 *
 * If it called 'unionfs_copyfile' function by unionfs_link etc,
 * VOP_LOOKUP information is broken.
 * So it need relookup in order to create link etc.
 }
function unionfs_relookup_for_create(dvp:p_vnode;cnp:p_componentname):Integer; public;
var
 error:Integer;
 udvp:p_vnode;
 vp:p_vnode;
 cn:componentname;
begin
 udvp:=UNIONFSVPTOUPPERVP(dvp, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 vp:=nil;

 error:=unionfs_relookup(udvp, @vp, cnp, @cn, cnp^.cn_nameptr, strlen(cnp^.cn_nameptr), CREATE);
 if (error<>0) then Exit(error);

 if (vp<>nil) then
 begin
  if (udvp=vp) then
   vrele(vp)
  else
   vput(vp);

  error:=EEXIST;
 end;

 if (cn.cn_flags and HASBUF)<>0 then
 begin
  uma_zfree(namei_zone, cn.cn_pnbuf);
  cn.cn_flags:=cn.cn_flags and (not HASBUF);
 end;

 if (error=0) then
 begin
  cn.cn_flags  :=cn.cn_flags or (cnp^.cn_flags and HASBUF);
  cnp^.cn_flags:=cn.cn_flags;
 end;

 Exit(error);
end;

{
 * relookup for DELETE namei operation.
 *
 * dvp is unionfs vnode. dvp should be locked.
 }
function unionfs_relookup_for_delete(dvp:p_vnode;cnp:p_componentname):Integer; public;
var
 error:Integer;
 udvp:p_vnode;
 vp:p_vnode;
 cn:componentname;
begin
 udvp:=UNIONFSVPTOUPPERVP(dvp, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 vp:=nil;

 error:=unionfs_relookup(udvp, @vp, cnp, @cn, cnp^.cn_nameptr, strlen(cnp^.cn_nameptr), DELETE);
 if (error<>0) then Exit(error);

 if (vp=nil) then
 begin
  error:=ENOENT;
 end else
 begin
  if (udvp=vp) then
   vrele(vp)
  else
   vput(vp);
 end;

 if (cn.cn_flags and HASBUF)<>0 then
 begin
  uma_zfree(namei_zone, cn.cn_pnbuf);
  cn.cn_flags:=cn.cn_flags and (not HASBUF);
 end;

 if (error=0) then
 begin
  cn.cn_flags  :=cn.cn_flags or (cnp^.cn_flags and HASBUF);
  cnp^.cn_flags:=cn.cn_flags;
 end;

 Exit(error);
end;

{
 * relookup for RENAME namei operation.
 *
 * dvp is unionfs vnode. dvp should be locked.
 }
function unionfs_relookup_for_rename(dvp:p_vnode;cnp:p_componentname):Integer; public;
var
 error:Integer;
 udvp:p_vnode;
 vp:p_vnode;
 cn:componentname;
begin
 udvp:=UNIONFSVPTOUPPERVP(dvp, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 vp:=nil;

 error:=unionfs_relookup(udvp, @vp, cnp, @cn, cnp^.cn_nameptr, strlen(cnp^.cn_nameptr), RENAME);
 if (error<>0) then Exit(error);

 if (vp<>nil) then
 begin
  if (udvp=vp) then
   vrele(vp)
  else
   vput(vp);
 end;

 if (cn.cn_flags and HASBUF)<>0 then
 begin
  uma_zfree(namei_zone, cn.cn_pnbuf);
  cn.cn_flags:=cn.cn_flags and (not HASBUF);
 end;

 if (error=0) then
 begin
  cn.cn_flags  :=cn.cn_flags or (cnp^.cn_flags and HASBUF);
  cnp^.cn_flags:=cn.cn_flags;
 end;

 Exit(error);
end;

{
 * Update the unionfs_node.
 *
 * uvp is new locked upper vnode. unionfs vnode's lock will be exchanged to the
 * uvp's lock and lower's lock will be unlocked.
 }
procedure unionfs_node_update(unp:p_unionfs_node;uvp:p_vnode);
var
 count,lockrec:DWORD;
 vp:p_vnode;
 lvp:p_vnode;
 dvp:p_vnode;
 hd:p_unionfs_node_hashhead;
begin
 vp:=UNIONFSTOV(unp);
 lvp:=unp^.un_lowervp;
 ASSERT_VOP_ELOCKED(lvp, 'unionfs_node_update');
 dvp:=unp^.un_dvp;

 {
  * lock update
  }
 VI_LOCK(vp);
 unp^.un_uppervp:=uvp;
 vp^.v_vnlock:=uvp^.v_vnlock;
 VI_UNLOCK(vp);
 lockrec:=lvp^.v_vnlock^.lk_recurse;

 if (lockrec<>0) then
 For count:=0 to lockrec-1 do
 begin
  vn_lock(uvp, LK_EXCLUSIVE or LK_CANRECURSE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 end;

 {
  * cache update
  }
 if (unp^.un_path<>nil) AND (dvp<>nil) AND (vp^.v_type=VDIR) then
 begin
  VI_LOCK(dvp);
  hd:=unionfs_get_hashhead(dvp, unp^.un_path);
  LIST_REMOVE(unp, @unp^.un_hash);
  LIST_INSERT_HEAD(hd, unp, @unp^.un_hash);
  VI_UNLOCK(dvp);
 end;
end;

{
 * Create a new shadow dir.
 *
 * udvp should be locked on entry and will be locked on return.
 *
 * If no error returned, unp will be updated.
 }
function unionfs_mkshadowdir(ump:p_unionfs_mount;udvp:p_vnode;unp:p_unionfs_node;cnp:p_componentname):Integer; public;
label
 _unionfs_mkshadowdir_abort,
 _unionfs_mkshadowdir_free_out;
var
 error:Integer;
 lvp:p_vnode;
 uvp:p_vnode;
 va:t_vattr;
 lva:t_vattr;
 cn:componentname;
 mp:p_mount;
 //struct ucred   *cred;
 //struct ucred   *credbk;
 //struct uidinfo *rootinfo;
begin
 if (unp^.un_uppervp<>nil) then
 begin
  Exit(EEXIST);
 end;

 lvp:=unp^.un_lowervp;
 uvp:=nil;
 //credbk:=cnp^.cn_cred;

 { Authority change to root }
 //rootinfo:=uifind(0);
 //cred:=crdup(cnp^.cn_cred);

 {
  * The calls to chgproccnt() are needed to compensate for change_ruid()
  * calling chgproccnt().
  }
 //chgproccnt(cred^.cr_ruidinfo, 1, 0);
 //change_euid(cred, rootinfo);
 //change_ruid(cred, rootinfo);
 //change_svuid(cred, (uid_t)0);
 //uifree(rootinfo);
 //cnp^.cn_cred:=cred;

 cn:=Default(componentname);

 error:=VOP_GETATTR(lvp, @lva);
 if (error<>0) then
 begin
  goto _unionfs_mkshadowdir_abort;
 end;

 error:=unionfs_relookup(udvp, @uvp, cnp, @cn, cnp^.cn_nameptr, cnp^.cn_namelen, CREATE);
 if (error<>0) then
 begin
  goto _unionfs_mkshadowdir_abort;
 end;

 if (uvp<>nil) then
 begin
  if (udvp=uvp) then
   vrele(uvp)
  else
   vput(uvp);

  error:=EEXIST;
  goto _unionfs_mkshadowdir_free_out;
 end;

 error:=vn_start_write(udvp, @mp, V_WAIT or PCATCH);
 if (error<>0) then
 begin
  goto _unionfs_mkshadowdir_free_out;
 end;

 unionfs_create_uppervattr_core(ump, @lva, @va);

 error:=VOP_MKDIR(udvp, @uvp, @cn, @va);

 if (error=0) then
 begin
  unionfs_node_update(unp, uvp);

  {
   * XXX The bug which cannot set uid/gid was corrected.
   * Ignore errors.
   }
  va.va_type:=VNON;
  VOP_SETATTR(uvp, @va);
 end;

 vn_finished_write(mp);

_unionfs_mkshadowdir_free_out:
 if (cn.cn_flags and HASBUF)<>0 then
 begin
  uma_zfree(namei_zone, cn.cn_pnbuf);
  cn.cn_flags:=cn.cn_flags and (not HASBUF);
 end;

_unionfs_mkshadowdir_abort:
 //cnp^.cn_cred:=credbk;
 //chgproccnt(cred^.cr_ruidinfo, -1, 0);
 //crfree(cred);

 Exit(error);
end;

{
 * Create a new whiteout.
 *
 * dvp should be locked on entry and will be locked on return.
 }
function unionfs_mkwhiteout(dvp:p_vnode;cnp:p_componentname;path:PChar):Integer; public;
label
 _unionfs_mkwhiteout_free_out;
var
 error:Integer;
 wvp:p_vnode;
 cn:componentname;
 mp:p_mount;
begin
 if (path=nil) then
 begin
  path:=cnp^.cn_nameptr;
 end;

 wvp:=nil;
 error:=unionfs_relookup(dvp, @wvp, cnp, @cn, path, strlen(path), CREATE);
 if (error<>0) then Exit(error);

 if (wvp<>nil) then
 begin
  if (cn.cn_flags and HASBUF)<>0 then
  begin
   uma_zfree(namei_zone, cn.cn_pnbuf);
   cn.cn_flags:=cn.cn_flags and (not HASBUF);
  end;

  if (dvp=wvp) then
   vrele(wvp)
  else
   vput(wvp);

  Exit(EEXIST);
 end;

 error:=vn_start_write(dvp, @mp, V_WAIT or PCATCH);
 if (error<>0) then
 begin
  goto _unionfs_mkwhiteout_free_out;
 end;

 error:=VOP_WHITEOUT(dvp, @cn, CREATE);

 vn_finished_write(mp);

_unionfs_mkwhiteout_free_out:
 if (cn.cn_flags and HASBUF)<>0 then
 begin
  uma_zfree(namei_zone, cn.cn_pnbuf);
  cn.cn_flags:=cn.cn_flags and (not HASBUF);
 end;

 Exit(error);
end;

{
 * Create a new vnode for create a new shadow file.
 *
 * If an error is returned, *vpp will be invalid, otherwise it will hold a
 * locked, referenced and opened vnode.
 *
 * unp is never updated.
 }
function unionfs_vn_create_on_upper(vpp:pp_vnode;udvp:p_vnode;unp:p_unionfs_node;uvap:p_vattr):Integer;
label
 _unionfs_vn_create_on_upper_free_out1,
 _unionfs_vn_create_on_upper_free_out2;
var
 ump:p_unionfs_mount;
 vp:p_vnode;
 lvp:p_vnode;
 //struct ucred   *cred;
 lva:t_vattr;
 fmode:Integer;
 error:Integer;
 cn:componentname;
begin
 ump:=MOUNTTOUNIONFSMOUNT(UNIONFSTOV(unp)^.v_mount);
 vp:=nil;
 lvp:=unp^.un_lowervp;
 //cred:=td^.td_ucred;
 fmode:=FFLAGS(O_WRONLY or O_CREAT or O_TRUNC or O_EXCL);
 error:=0;

 error:=VOP_GETATTR(lvp, @lva);
 if (error<>0) then Exit(error);

 unionfs_create_uppervattr_core(ump, @lva, uvap);

 if (unp^.un_path=nil) then
 begin
  Assert(False, 'unionfs: un_path is null');
 end;

 cn.cn_namelen:=strlen(unp^.un_path);
 cn.cn_pnbuf:=uma_zalloc(namei_zone, M_WAITOK);
 Move(unp^.un_path^, cn.cn_pnbuf^, cn.cn_namelen + 1);

 cn.cn_nameiop:=CREATE;
 cn.cn_flags  :=(LOCKPARENT or LOCKLEAF or HASBUF or SAVENAME or ISLASTCN);
 cn.cn_lkflags:=LK_EXCLUSIVE;
 cn.cn_thread :=curkthread;
 //cn.cn_cred   :=cred;
 cn.cn_nameptr:=cn.cn_pnbuf;
 cn.cn_consume:=0;

 vref(udvp);

 error:=nd_relookup(udvp, @vp, @cn);
 if (error<>0) then
 begin
  goto _unionfs_vn_create_on_upper_free_out2;
 end;

 vrele(udvp);

 if (vp<>nil) then
 begin
  if (vp=udvp) then
   vrele(vp)
  else
   vput(vp);

  error:=EEXIST;
  goto _unionfs_vn_create_on_upper_free_out1;
 end;

 error:=VOP_CREATE(udvp, @vp, @cn, uvap, @fmode);
 if (error<>0) then
 begin
  goto _unionfs_vn_create_on_upper_free_out1;
 end;

 error:=VOP_OPEN(vp, fmode, nil, @fmode);
 if (error<>0) then
 begin
  vput(vp);
  goto _unionfs_vn_create_on_upper_free_out1;
 end;

 VOP_ADD_WRITECOUNT(vp, 1);
 vpp^:=vp;

_unionfs_vn_create_on_upper_free_out1:
 VOP_UNLOCK(udvp, LK_RELEASE);

_unionfs_vn_create_on_upper_free_out2:
 if (cn.cn_flags and HASBUF)<>0 then
 begin
  uma_zfree(namei_zone, cn.cn_pnbuf);
  cn.cn_flags:=cn.cn_flags and (not HASBUF);
 end;

 Exit(error);
end;

{
 * Copy from lvp to uvp.
 *
 * lvp and uvp should be locked and opened on entry and will be locked and
 * opened on return.
 }
function unionfs_copyfile_core(lvp,uvp:p_vnode):Integer;
var
 error:Integer;
 offset:Int64;
 count:Integer;
 bufoffset:Integer;
 buf:PChar;
 uio:t_uio;
 iov:iovec;
begin
 error:=0;
 uio:=Default(t_uio);

 uio.uio_td    :=curkthread;
 uio.uio_segflg:=UIO_SYSSPACE;
 uio.uio_offset:=0;

 buf:=malloc(MAXBSIZE);

 while (error=0) do
 begin
  offset:=uio.uio_offset;

  uio.uio_iov   :=@iov;
  uio.uio_iovcnt:=1;
  iov.iov_base  :=buf;
  iov.iov_len   :=MAXBSIZE;
  uio.uio_resid :=iov.iov_len;
  uio.uio_rw    :=UIO_READ;

  error:=VOP_READ(lvp, @uio, 0);
  if (error<>0) then break;

  count:=MAXBSIZE - uio.uio_resid;
  if (count=0) then break;

  bufoffset:=0;
  while (bufoffset < count) do
  begin
   uio.uio_iov   :=@iov;
   uio.uio_iovcnt:=1;
   iov.iov_base  :=buf + bufoffset;
   iov.iov_len   :=count - bufoffset;
   uio.uio_offset:=offset + bufoffset;
   uio.uio_resid :=iov.iov_len;
   uio.uio_rw    :=UIO_WRITE;

   error:=VOP_WRITE(uvp, @uio, 0);
   if (error<>0) then break;

   bufoffset:=bufoffset + ((count - bufoffset) - uio.uio_resid);
  end;

  uio.uio_offset:=offset + bufoffset;
 end;

 free(buf);

 Exit(error);
end;

{
 * Copy file from lower to upper.
 *
 * If you need copy of the contents, set 1 to docopy. Otherwise, set 0 to
 * docopy.
 *
 * If no error returned, unp will be updated.
 }
function unionfs_copyfile(unp:p_unionfs_node;docopy:Integer):Integer; public;
var
 error:Integer;
 mp:p_mount;
 udvp:p_vnode;
 lvp:p_vnode;
 uvp:p_vnode;
 uva:t_vattr;
begin
 lvp:=unp^.un_lowervp;
 uvp:=nil;

 if (p_mount(UNIONFSTOV(unp)^.v_mount)^.mnt_flag and MNT_RDONLY)<>0 then
 begin
  Exit(EROFS);
 end;

 if (unp^.un_dvp=nil) then Exit(EINVAL);

 if (unp^.un_uppervp<>nil) then Exit(EEXIST);

 udvp:=VTOUNIONFS(unp^.un_dvp)^.un_uppervp;
 if (udvp=nil) then Exit(EROFS);

 if (p_mount(udvp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0 then
 begin
  Exit(EROFS);
 end;

 error:=VOP_ACCESS(lvp, VREAD);
 if (error<>0) then Exit(error);

 error:=vn_start_write(udvp, @mp, V_WAIT or PCATCH);
 if (error<>0) then Exit(error);

 error:=unionfs_vn_create_on_upper(@uvp, udvp, unp, @uva);

 if (error<>0) then
 begin
  vn_finished_write(mp);
  Exit(error);
 end;

 if (docopy<>0) then
 begin
  error:=VOP_OPEN(lvp, FREAD, nil, nil);
  if (error=0) then
  begin
   error:=unionfs_copyfile_core(lvp, uvp);
   VOP_CLOSE(lvp, FREAD);
  end;
 end;

 VOP_CLOSE(uvp, FWRITE);
 VOP_ADD_WRITECOUNT(uvp, -1);

 vn_finished_write(mp);

 if (error=0) then
 begin
  { Reset the attributes. Ignore errors. }
  uva.va_type:=VNON;
  VOP_SETATTR(uvp, @uva);
 end;

 unionfs_node_update(unp, uvp);

 Exit(error);
end;

{
 * It checks whether vp can rmdir. (check empty)
 *
 * vp is unionfs vnode.
 * vp should be locked.
 }
function unionfs_check_rmdir(vp:p_vnode):Integer; public;
label
 _continue;
var
 error:Integer;
 eofflag:Integer;
 lookuperr:Integer;
 uvp:p_vnode;
 lvp:p_vnode;
 tvp:p_vnode;
 va:t_vattr;
 cn:componentname;
 buf:PByte;
 dp:p_dirent;
 edp:p_dirent;
 uio:t_uio;
 iov:iovec;
const
 SIZEOFBUF=256 * 6; //The size of buf needs to be larger than DIRBLKSIZ.
begin
 ASSERT_VOP_ELOCKED(vp, 'unionfs_check_rmdir');

 eofflag:=0;
 uvp:=UNIONFSVPTOUPPERVP(vp, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 lvp:=UNIONFSVPTOLOWERVP(vp, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});

 { check opaque }
 error:=VOP_GETATTR(uvp, @va);
 if (error<>0) then
 begin
  Exit(error);
 end;

 if (va.va_flags and OPAQUE)<>0 then
 begin
  Exit(0);
 end;

 { open vnode }

 error:=VOP_ACCESS(vp, VEXEC or VREAD);
 if (error<>0) then Exit(error);

 error:=VOP_OPEN(vp, FREAD, nil, nil);
 if (error<>0) then Exit(error);

 uio.uio_rw    :=UIO_READ;
 uio.uio_segflg:=UIO_SYSSPACE;
 uio.uio_td    :=curkthread;
 uio.uio_offset:=0;

 buf:=nil;

 while (error=0) AND (eofflag=0) do
 begin

  if (buf=nil) then
  begin
   buf:=malloc(SIZEOFBUF);
  end;

  iov.iov_base  :=buf;
  iov.iov_len   :=SIZEOFBUF;
  uio.uio_iov   :=@iov;
  uio.uio_iovcnt:=1;
  uio.uio_resid :=iov.iov_len;

  error:=VOP_READDIR(lvp, @uio, @eofflag, nil, nil);
  if (error<>0) then break;

  if (eofflag=0) AND (uio.uio_resid=SIZEOFBUF) then
  begin
   LOG_ERROR(stderr,'bad readdir response from lower FS.');
   break;
  end;

  edp:=@buf[SIZEOFBUF - uio.uio_resid];

  dp:=@buf[0];

  while (error=0) AND (dp < edp) do
  begin

   if (dp^.d_type=DT_WHT) OR
      ((dp^.d_namlen=1) AND (dp^.d_name[0]='.')) OR
      ((dp^.d_namlen=2) AND (PWORD(@dp^.d_name)^=$2E2E)) then
   begin
    goto _continue;
   end;

   cn.cn_namelen:=dp^.d_namlen;
   cn.cn_pnbuf  :=nil;
   cn.cn_nameptr:=dp^.d_name;
   cn.cn_nameiop:=LOOKUP;
   cn.cn_flags  :=(LOCKPARENT or LOCKLEAF or SAVENAME or RDONLY or ISLASTCN);
   cn.cn_lkflags:=LK_EXCLUSIVE;
   cn.cn_thread :=curkthread;
   cn.cn_consume:=0;

   {
    * check entry in lower.
    * Sometimes, readdir function returns
    * wrong entry.
    }
   lookuperr:=VOP_LOOKUP(lvp, @tvp, @cn);

   if (lookuperr=0) then
    vput(tvp)
   else
    goto _continue;   { skip entry }

   {
    * check entry
    * If it has no exist/whiteout entry in upper,
    * directory is not empty.
    }
   cn.cn_flags:=(LOCKPARENT or LOCKLEAF or SAVENAME or RDONLY or ISLASTCN);
   lookuperr:=VOP_LOOKUP(uvp, @tvp, @cn);

   if (lookuperr=0) then
   begin
    vput(tvp);
   end;

   { ignore exist or whiteout entry }
   if (lookuperr=0) OR
      ((lookuperr=ENOENT) AND ((cn.cn_flags and ISWHITEOUT)<>0)) then
   begin
    goto _continue;
   end;

   error:=ENOTEMPTY;

   _continue:
    dp:=(Pointer(dp) + dp^.d_reclen);
  end; //for

 end; //while

 if (buf<>nil) then
 begin
  free(buf);
 end;

 { close vnode }
 VOP_CLOSE(vp, FREAD);

 Exit(error);
end;

function unionfs_checkuppervp(vp:p_vnode;fil:PChar;lno:Integer):p_vnode; public;
var
 unp:p_unionfs_node;
begin
 unp:=VTOUNIONFS(vp);

 if (vp^.v_op<>@unionfs_vnodeops) then
 begin
  LOG_CRITICAL(stderr,'unionfs_checkuppervp: on non-unionfs-node.');
  Assert(False, 'unionfs_checkuppervp: on non-unionfs-node.');
 end;

 Exit(unp^.un_uppervp);
end;

function unionfs_checklowervp(vp:p_vnode;fil:PChar;lno:Integer):p_vnode; public;
var
 unp:p_unionfs_node;
begin
 unp:=VTOUNIONFS(vp);

 if (vp^.v_op<>@unionfs_vnodeops) then
 begin
  LOG_CRITICAL(stderr,'unionfs_checklowervp: on non-unionfs-node.');
  Assert(False, 'unionfs_checklowervp: on non-unionfs-node.');
 end;

 Exit(unp^.un_lowervp);
end;


end.



