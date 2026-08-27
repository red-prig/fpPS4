unit tmpfs_vnops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 errno,
 mqueue,
 time,
 vnode,
 vmount,
 vnamei,
 vfile,
 vfs_default,
 vmparam,
 kern_param,
 sys_resource,
 kern_resource,
 vuio,
 vstat,
 vfcntl,
 subr_uio,
 vfs_subr,
 vfs_vnops,
 vnode_pager,
 vnode_if,
 subr_unit,
 vm,
 vm_object,
 kern_mtx,
 tmpfs;

function tmpfs_access (v:p_vop_access_args):Integer;
function tmpfs_getattr(v:p_vop_getattr_args):Integer;
function tmpfs_setattr(v:p_vop_setattr_args):Integer;
function tmpfs_reclaim(v:p_vop_reclaim_args):Integer;

implementation

var
 tmpfs_rename_restarts:Integer=0;

function tmpfs_lookup(v:p_vop_lookup_args):Integer;
label
 _out;
var
 dvp:p_vnode;
 vpp:pp_vnode;
 cnp:p_componentname;
 error:Integer;
 ltype:Integer;
 de:p_tmpfs_dirent;
 dnode:p_tmpfs_node;
 tnode:p_tmpfs_node;
begin
 dvp:=v^.a_dvp;
 vpp:=v^.a_vpp;
 cnp:=v^.a_cnp;

 dnode:=VP_TO_TMPFS_DIR(dvp);
 vpp^:=nil;

 { Check accessibility of requested node as a first step. }
 error:=VOP_ACCESS(dvp, VEXEC);
 if (error<>0) then
  goto _out;

 { We cannot be requesting the parent directory of the root node. }
 Assert(IMPLIES((dnode^.tn_type=VDIR) AND (dnode^.tn_parent=dnode),(cnp^.cn_flags and ISDOTDOT)=0));

 TMPFS_ASSERT_LOCKED(dnode);
 if (dnode^.tn_parent=nil) then
 begin
  error:=ENOENT;
  goto _out;
 end;

 if (cnp^.cn_flags and ISDOTDOT)<>0 then
 begin
  ltype:=VOP_ISLOCKED(dvp);
  vhold(dvp);
  VOP_UNLOCK(dvp, 0);
  { Allocate a new vnode on the matching entry. }
  error:=tmpfs_alloc_vp(dvp^.v_mount, dnode^.tn_parent, cnp^.cn_lkflags, vpp);

  vn_lock(dvp, ltype or LK_RETRY,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  vdrop(dvp);
 end else
 if (cnp^.cn_namelen=1) AND (cnp^.cn_nameptr[0]='.') then
 begin
  VREF(dvp);
  vpp^:=dvp;
  error:=0;
 end else
 begin
  de:=tmpfs_dir_lookup(dnode, nil, cnp);

  if (de<>nil) AND (de^.td_node=nil) then
   cnp^.cn_flags:=cnp^.cn_flags or ISWHITEOUT;

  if (de=nil) OR (de^.td_node=nil) then
  begin
   { The entry was not found in the directory.
    * This is OK if we are creating or renaming an
    * entry and are working on the last component of
    * the path name. }
   if ((cnp^.cn_flags and ISLASTCN)<>0) AND
       ((cnp^.cn_nameiop=CREATE) OR
        (cnp^.cn_nameiop=RENAME) OR
       ((cnp^.cn_nameiop=DELETE) AND
        ((cnp^.cn_flags and DOWHITEOUT)<>0) AND
        ((cnp^.cn_flags and ISWHITEOUT)<>0))) then
   begin
    error:=VOP_ACCESS(dvp, VWRITE);
    if (error<>0) then
     goto _out;

    { Keep the component name in the buffer for
     * future uses. }
    cnp^.cn_flags:=cnp^.cn_flags or SAVENAME;

    error:=EJUSTRETURN;
   end else
    error:=ENOENT;

  end else
  begin
   { The entry was found, so get its associated
    * tmpfs_node. }
   tnode:=de^.td_node;

   { If we are not at the last path component and
    * found a non-directory or non-link entry (which
    * may itself be pointing to a directory), raise
    * an error. }
   if  (tnode^.tn_type<>VDIR) AND
       (tnode^.tn_type<>VLNK) AND
       ((cnp^.cn_flags and ISLASTCN)=0) then
   begin
    error:=ENOTDIR;
    goto _out;
   end;

   { If we are deleting or renaming the entry, keep
    * track of its tmpfs_dirent so that it can be
    * easily deleted later. }
   if ((cnp^.cn_flags and ISLASTCN)<>0) AND
       ((cnp^.cn_nameiop=DELETE) OR
        (cnp^.cn_nameiop=RENAME)) then
   begin
    error:=VOP_ACCESS(dvp, VWRITE);
    if (error<>0) then
     goto _out;

    { Allocate a new vnode on the matching entry. }
    error:=tmpfs_alloc_vp(dvp^.v_mount, tnode, cnp^.cn_lkflags, vpp);
    if (error<>0) then
     goto _out;

    if ((dnode^.tn_mode and S_ISTXT)<>0) AND
       (VOP_ACCESS(dvp, VADMIN)<>0) AND
       (VOP_ACCESS(vpp^, VADMIN)<>0) then
    begin
     error:=EPERM;
     vput(vpp^);
     vpp^:=nil;
     goto _out;
    end;

    cnp^.cn_flags:=cnp^.cn_flags or SAVENAME;
   end else
   begin
    error:=tmpfs_alloc_vp(dvp^.v_mount, tnode, cnp^.cn_lkflags, vpp);
   end;
  end;
 end;

 { Store the result of this lookup in the cache.  Avoid this if the
  * request was for creation, as it does not improve timings on
  * emprical tests. }
 //if ((cnp^.cn_flags and MAKEENTRY)<>0) AND (cnp^.cn_nameiop<>CREATE) then
 // cache_enter(dvp, vpp^, cnp);

_out:
 { If there were no errors, *vpp cannot be null and it must be
  * locked. }
 Assert(IFF(error=0, (vpp^<>nil) AND (VOP_ISLOCKED(vpp^)<>0) ));

 Result:=error;
end;

function tmpfs_create(v:p_vop_create_args):Integer;
var
 dvp:p_vnode;
 vpp:pp_vnode;
 cnp:p_componentname;
 vap:p_vattr;
begin
 dvp:=v^.a_dvp;
 vpp:=v^.a_vpp;
 cnp:=v^.a_cnp;
 vap:=v^.a_vap;

 Assert((vap^.va_type=VREG) OR (vap^.va_type=VSOCK));

 Result:=tmpfs_alloc_file(dvp, vpp, vap, cnp, nil);
end;

function tmpfs_mknod(v:p_vop_mknod_args):Integer;
var
 dvp:p_vnode;
 vpp:pp_vnode;
 cnp:p_componentname;
 vap:p_vattr;
begin
 dvp:=v^.a_dvp;
 vpp:=v^.a_vpp;
 cnp:=v^.a_cnp;
 vap:=v^.a_vap;

 if (vap^.va_type<>VBLK) AND
    (vap^.va_type<>VCHR) AND
    (vap^.va_type<>VFIFO) then
  Exit(EINVAL);

 Result:=tmpfs_alloc_file(dvp, vpp, vap, cnp, nil);
end;

function tmpfs_open(v:p_vop_open_args):Integer;
var
 vp:p_vnode;
 mode:Integer;
 error:Integer;
 node:p_tmpfs_node;
 mp:p_mount;
begin
 vp:=v^.a_vp;
 mode:=v^.a_mode;

 Assert(VOP_ISLOCKED(vp)<>0);

 node:=VP_TO_TMPFS_NODE(vp);

 { The file is still active but all its names have been removed
  * (e.g. by a 'rmdir $(pwd)').  It cannot be opened any more as
  * it is about to die. }
 if (node^.tn_links < 1) then
  Exit(ENOENT);

 { If the file is marked append-only, deny write requests. }
 if ((node^.tn_flags and APPEND)<>0) AND
    ((mode and (FWRITE or O_APPEND))=FWRITE) then
 begin
  error:=EPERM;
 end else
 begin
  error:=0;
  mp:=vp^.v_mount;
  vnode_create_vobject(vp, node^.tn_size, mp^.mnt_budget_id);
 end;

 Assert(VOP_ISLOCKED(vp)<>0);

 Result:=error;
end;

function tmpfs_close(v:p_vop_close_args):Integer;
var
 vp:p_vnode;
begin
 vp:=v^.a_vp;

 Assert(VOP_ISLOCKED(vp)<>0);

 { Update node times. }
 tmpfs_update(vp);

 Exit(0);
end;

function tmpfs_access(v:p_vop_access_args):Integer;
label
 _out;
var
 vp:p_vnode;
 accmode:accmode_t;
 error:Integer;
 node:p_tmpfs_node;
begin
 vp:=v^.a_vp;
 accmode:=v^.a_accmode;

 Assert(VOP_ISLOCKED(vp)<>0);

 node:=VP_TO_TMPFS_NODE(vp);

 case (vp^.v_type) of
  VDIR,
  VLNK,
  VREG:
   if ((accmode and VWRITE)<>0) AND
      ((p_mount(vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0) then
   begin
    error:=EROFS;
    goto _out;
   end;

  VBLK,
  VCHR,
  VSOCK,
  VFIFO:;

  else
   begin
    error:=EINVAL;
    goto _out;
   end;
 end;

 if ((accmode and VWRITE)<>0) AND ((node^.tn_flags and IMMUTABLE)<>0) then
 begin
  error:=EPERM;
  goto _out;
 end;

 error:=vaccess(vp^.v_type, node^.tn_mode, node^.tn_uid,node^.tn_gid, accmode, nil);

_out:
 Assert(VOP_ISLOCKED(vp)<>0);

 Result:=error;
end;

function tmpfs_getattr(v:p_vop_getattr_args):Integer;
var
 vp:p_vnode;
 vap:p_vattr;
 node:p_tmpfs_node;
begin
 vp:=v^.a_vp;
 vap:=v^.a_vap;

 node:=VP_TO_TMPFS_NODE(vp);

 tmpfs_update(vp);

 vap^.va_type     :=vp^.v_type;
 vap^.va_mode     :=node^.tn_mode;
 vap^.va_nlink    :=node^.tn_links;
 vap^.va_uid      :=node^.tn_uid;
 vap^.va_gid      :=node^.tn_gid;
 vap^.va_fsid     :=p_mount(vp^.v_mount)^.mnt_stat.f_fsid.val[0];
 vap^.va_fileid   :=node^.tn_id;
 vap^.va_size     :=node^.tn_size;
 vap^.va_blocksize:=PAGE_SIZE;
 vap^.va_atime    :=node^.tn_atime;
 vap^.va_mtime    :=node^.tn_mtime;
 vap^.va_ctime    :=node^.tn_ctime;
 vap^.va_birthtime:=node^.tn_birthtime;
 vap^.va_gen      :=node^.tn_gen;
 vap^.va_flags    :=node^.tn_flags;

 if (vp^.v_type=VBLK) OR (vp^.v_type=VCHR) then
  vap^.va_rdev    :=node^.tn_rdev
 else
  vap^.va_rdev    :=NODEV;

 vap^.va_bytes    :=round_page(node^.tn_size);
 vap^.va_filerev  :=0;

 Result:=0;
end;

function tmpfs_setattr(v:p_vop_setattr_args):Integer;
var
 vp:p_vnode;
 vap:p_vattr;
 error:Integer;
begin
 vp:=v^.a_vp;
 vap:=v^.a_vap;

 Assert(VOP_ISLOCKED(vp)<>0);

 error:=0;

 { Abort if any unsettable attribute is given. }
 if (vap^.va_type<>VNON) OR
    (vap^.va_nlink<>VNOVAL) OR
    (vap^.va_fsid<>VNOVAL) OR
    (vap^.va_fileid<>VNOVAL) OR
    (vap^.va_blocksize<>VNOVAL) OR
    (vap^.va_gen<>VNOVAL) OR
    (vap^.va_rdev<>VNOVAL) OR
    (vap^.va_bytes<>VNOVAL) then
  error:=EINVAL;

 if (error=0) AND (vap^.va_flags<>VNOVAL) then
  error:=tmpfs_chflags(vp, vap^.va_flags);

 if (error=0) AND (vap^.va_size<>VNOVAL) then
  error:=tmpfs_chsize(vp, vap^.va_size);

 if (error=0) AND ((vap^.va_uid<>VNOVAL) OR (vap^.va_gid<>VNOVAL)) then
  error:=tmpfs_chown(vp, vap^.va_uid, vap^.va_gid);

 if (error=0) AND (vap^.va_mode<>VNOVAL) then
  error:=tmpfs_chmod(vp, vap^.va_mode);

 if (error=0) AND
    (((vap^.va_atime.tv_sec<>VNOVAL) AND
      (vap^.va_atime.tv_nsec<>VNOVAL)) OR
     ((vap^.va_mtime.tv_sec<>VNOVAL) AND
      (vap^.va_mtime.tv_nsec<>VNOVAL)) OR
     ((vap^.va_birthtime.tv_sec<>VNOVAL) AND
      (vap^.va_birthtime.tv_nsec<>VNOVAL))) then
  error:=tmpfs_chtimes(vp, @vap^.va_atime, @vap^.va_mtime, @vap^.va_birthtime, vap^.va_vaflags);

 { Update the node times.  We give preference to the error codes
  * generated by this function rather than the ones that may arise
  * from tmpfs_update. }
 tmpfs_update(vp);

 Assert(VOP_ISLOCKED(vp)<>0);

 Result:=error;
end;

{
function tmpfs_nocacheread(vm_object_t tobj, vm_pindex_t idx,vm_offset_t offset, size_t tlen, struct uio *uio):Integer;
begin
 vm_page_t m;
 int  error, rv;

 VM_OBJECT_LOCK(tobj);
 m:=vm_page_grab(tobj, idx, VM_ALLOC_WIRED or
     VM_ALLOC_NORMAL or VM_ALLOC_RETRY);
 if (m^.valid<>VM_PAGE_BITS_ALL) begin
  if (vm_pager_has_page(tobj, idx, nil, nil)) begin
   rv:=vm_pager_get_pages(tobj, @m, 1, 0);
   if (rv<>VM_PAGER_OK) begin
    vm_page_lock(m);
    vm_page_free(m);
    vm_page_unlock(m);
    VM_OBJECT_UNLOCK(tobj);
    Exit(EIO);
   end
  end else
   vm_page_zero_invalid(m, TRUE);
 end
 VM_OBJECT_UNLOCK(tobj);
 error:=uiomove_fromphys(@m, offset, tlen, uio);
 VM_OBJECT_LOCK(tobj);
 vm_page_lock(m);
 vm_page_unwire(m, TRUE);
 vm_page_unlock(m);
 vm_page_wakeup(m);
 VM_OBJECT_UNLOCK(tobj);

 Exit(error);
end;
}

{
static __inline int
tmpfs_nocacheread_buf(vm_object_t tobj, vm_pindex_t idx, vm_offset_t offset, size_t tlen, void *buf)
begin
 struct uio uio;
 struct iovec iov;

 uio.uio_iovcnt:=1;
 uio.uio_iov:=@iov;
 iov.iov_base:=buf;
 iov.iov_len:=tlen;

 uio.uio_offset:=0;
 uio.uio_resid:=tlen;
 uio.uio_rw:=UIO_READ;
 uio.uio_segflg:=UIO_SYSSPACE;
 uio.uio_td:=curthread;

 Exit(tmpfs_nocacheread(tobj, idx, offset, tlen, @uio));
end;
}

{
static int
tmpfs_mappedread(vm_object_t vobj, vm_object_t tobj, size_t len, struct uio *uio)
begin
 struct sf_buf *sf;
 vm_pindex_t idx;
 vm_page_t m;
 vm_offset_t offset;
 off_t  addr;
 size_t  tlen;
 char  *ma;
 int  error;

 addr:=uio^.uio_offset;
 idx:=OFF_TO_IDX(addr);
 offset:=addr and PAGE_MASK;
 tlen:=MIN(PAGE_SIZE - offset, len);

 if ((vobj=nil) OR
     (vobj^.resident_page_count=0 AND vobj^.cache=nil))
  goto nocache;

 VM_OBJECT_LOCK(vobj);
lookupvpg:
 if (((m:=vm_page_lookup(vobj, idx))<>nil) AND
     vm_page_is_valid(m, offset, tlen)) begin
  if ((m^.oflags and VPO_BUSY)<>0) begin
   {
    * Reference the page before unlocking and sleeping so
    * that the page daemon is less likely to reclaim it.
    }
   vm_page_reference(m);
   vm_page_sleep(m, 'tmfsmr');
   goto lookupvpg;
  end
  vm_page_busy(m);
  VM_OBJECT_UNLOCK(vobj);
  error:=uiomove_fromphys(@m, offset, tlen, uio);
  VM_OBJECT_LOCK(vobj);
  vm_page_wakeup(m);
  VM_OBJECT_UNLOCK(vobj);
  Exit(error);
 end else if (m<>nil AND uio^.uio_segflg=UIO_NOCOPY) begin
  Assert(offset=0,
      ('unexpected offset in tmpfs_mappedread for sendfile'));
  if ((m^.oflags and VPO_BUSY)<>0) begin
   {
    * Reference the page before unlocking and sleeping so
    * that the page daemon is less likely to reclaim it.
    }
   vm_page_reference(m);
   vm_page_sleep(m, 'tmfsmr');
   goto lookupvpg;
  end
  vm_page_busy(m);
  VM_OBJECT_UNLOCK(vobj);
  sched_pin();
  sf:=sf_buf_alloc(m, SFB_CPUPRIVATE);
  ma:=(char *)sf_buf_kva(sf);
  error:=tmpfs_nocacheread_buf(tobj, idx, 0, tlen, ma);
  if (error=0) begin
   if (tlen<>PAGE_SIZE)
    bzero(ma + tlen, PAGE_SIZE - tlen);
   uio^.uio_offset += tlen;
   uio^.uio_resid -= tlen;
  end
  sf_buf_free(sf);
  sched_unpin();
  VM_OBJECT_LOCK(vobj);
  if (error=0)
   m^.valid:=VM_PAGE_BITS_ALL;
  vm_page_wakeup(m);
  VM_OBJECT_UNLOCK(vobj);
  Exit(error);
 end
 VM_OBJECT_UNLOCK(vobj);
nocache:
 error:=tmpfs_nocacheread(tobj, idx, offset, tlen, uio);

 Exit(error);
end;
}

function Min(a,b:QWORD):QWORD; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

function tmpfs_read(v:p_vop_read_args):Integer;
label
 _out;
var
 vp:p_vnode;
 uio:p_uio;
 node:p_tmpfs_node;
 uobj:vm_object_t;
 len:QWORD;
 resid:Integer;
 error:Integer;
begin
 vp:=v^.a_vp;
 uio:=v^.a_uio;

 error:=0;

 node:=VP_TO_TMPFS_NODE(vp);

 if (vp^.v_type<>VREG) then
 begin
  error:=EISDIR;
  goto _out;
 end;

 if (uio^.uio_offset < 0) then
 begin
  error:=EINVAL;
  goto _out;
 end;

 node^.tn_status:=node^.tn_status or TMPFS_NODE_ACCESSED;

 uobj:=node^.tn_aobj;
 resid:=uio^.uio_resid;
 while (resid > 0) do
 begin
  error:=0;
  if (node^.tn_size <= uio^.uio_offset) then
   break;

  len:=MIN(node^.tn_size - uio^.uio_offset, resid);
  if (len=0) then
   break;

  //error:=tmpfs_mappedread(vp^.v_object, uobj, len, uio);

  if (error<>0) OR (resid=uio^.uio_resid) then
   break;

  //
  resid:=uio^.uio_resid;
 end;

_out:

 Result:=error;
end;

{
static int
tmpfs_mappedwrite(vm_object_t vobj, vm_object_t tobj, size_t len, struct uio *uio)
begin
 vm_pindex_t idx;
 vm_page_t vpg, tpg;
 vm_offset_t offset;
 off_t  addr;
 size_t  tlen;
 int  error, rv;

 error:=0;

 addr:=uio^.uio_offset;
 idx:=OFF_TO_IDX(addr);
 offset:=addr and PAGE_MASK;
 tlen:=MIN(PAGE_SIZE - offset, len);

 if ((vobj=nil) OR
     (vobj^.resident_page_count=0 AND vobj^.cache=nil)) begin
  vpg:=nil;
  goto nocache;
 end

 VM_OBJECT_LOCK(vobj);
lookupvpg:
 if (((vpg:=vm_page_lookup(vobj, idx))<>nil) AND
     vm_page_is_valid(vpg, offset, tlen)) begin
  if ((vpg^.oflags and VPO_BUSY)<>0) begin
   {
    * Reference the page before unlocking and sleeping so
    * that the page daemon is less likely to reclaim it.
    }
   vm_page_reference(vpg);
   vm_page_sleep(vpg, 'tmfsmw');
   goto lookupvpg;
  end
  vm_page_busy(vpg);
  vm_page_undirty(vpg);
  VM_OBJECT_UNLOCK(vobj);
  error:=uiomove_fromphys(@vpg, offset, tlen, uio);
 end else begin
  if (__predict_false(vobj^.cache<>nil))
   vm_page_cache_free(vobj, idx, idx + 1);
  VM_OBJECT_UNLOCK(vobj);
  vpg:=nil;
 end
nocache:
 VM_OBJECT_LOCK(tobj);
 tpg:=vm_page_grab(tobj, idx, VM_ALLOC_WIRED or
     VM_ALLOC_NORMAL or VM_ALLOC_RETRY);
 if (tpg^.valid<>VM_PAGE_BITS_ALL) begin
  if (vm_pager_has_page(tobj, idx, nil, nil)) begin
   rv:=vm_pager_get_pages(tobj, @tpg, 1, 0);
   if (rv<>VM_PAGER_OK) begin
    vm_page_lock(tpg);
    vm_page_free(tpg);
    vm_page_unlock(tpg);
    error:=EIO;
    goto out;
   end
  end else
   vm_page_zero_invalid(tpg, TRUE);
 end
 VM_OBJECT_UNLOCK(tobj);
 if (vpg=nil)
  error:=uiomove_fromphys(@tpg, offset, tlen, uio);
 else begin
  Assert(vpg^.valid=VM_PAGE_BITS_ALL, ('parts of vpg invalid'));
  pmap_copy_page(vpg, tpg);
 end
 VM_OBJECT_LOCK(tobj);
 if (error=0) begin
  Assert(tpg^.valid=VM_PAGE_BITS_ALL,
      ('parts of tpg invalid'));
  vm_page_dirty(tpg);
 end
 vm_page_lock(tpg);
 vm_page_unwire(tpg, TRUE);
 vm_page_unlock(tpg);
 vm_page_wakeup(tpg);
out:
 VM_OBJECT_UNLOCK(tobj);
 if (vpg<>nil) begin
  VM_OBJECT_LOCK(vobj);
  vm_page_wakeup(vpg);
  VM_OBJECT_UNLOCK(vobj);
 end

 Exit(error);
end;
}

function vn_rlimit_fsize(vp:p_vnode;uio:p_uio):Integer;
begin
 if (vp^.v_type<>VREG) then Exit(0);

 if (uio^.uio_offset + uio^.uio_resid) > lim_cur(RLIMIT_FSIZE) then
 begin
  Exit(EFBIG);
 end;

 Result:=0;
end;

function tmpfs_write(v:p_vop_write_args):Integer;
label
 _out;
var
 vp:p_vnode;
 uio:p_uio;
 ioflag:Integer;
 extended:Boolean;
 error:Integer;
 resid:Integer;
 oldsize:Int64;
 node:p_tmpfs_node;
 uobj:vm_object_t;
 len:QWORD;
begin
 vp:=v^.a_vp;
 uio:=v^.a_uio;
 ioflag:=v^.a_ioflag;

 error:=0;

 node:=VP_TO_TMPFS_NODE(vp);
 oldsize:=node^.tn_size;

 if (uio^.uio_offset < 0) OR (vp^.v_type<>VREG) then
 begin
  error:=EINVAL;
  goto _out;
 end;

 if (uio^.uio_resid=0) then
 begin
  error:=0;
  goto _out;
 end;

 if (ioflag and IO_APPEND)<>0 then
  uio^.uio_offset:=node^.tn_size;

 if (uio^.uio_offset + uio^.uio_resid > VFS_TO_TMPFS(vp^.v_mount)^.tm_maxfilesize) then
  Exit(EFBIG);

 if (vn_rlimit_fsize(vp, uio)<>0) then
  Exit(EFBIG);

 extended:=uio^.uio_offset + uio^.uio_resid > node^.tn_size;
 if (extended) then
 begin
  error:=tmpfs_reg_resize(vp, uio^.uio_offset + uio^.uio_resid, FALSE);
  if (error<>0) then
   goto _out;
 end;

 uobj:=node^.tn_aobj;
 resid:=uio^.uio_resid;
 while (resid > 0) do
 begin
  if (node^.tn_size <= uio^.uio_offset) then
   break;

  len:=MIN(node^.tn_size - uio^.uio_offset, resid);
  if (len=0) then
   break;

  //error:=tmpfs_mappedwrite(vp^.v_object, uobj, len, uio);
  if (error<>0) OR (resid=uio^.uio_resid) then
   break;

  //
  resid:=uio^.uio_resid;
 end;

 node^.tn_status:=node^.tn_status or (TMPFS_NODE_ACCESSED or TMPFS_NODE_MODIFIED) or (ord(extended)*TMPFS_NODE_CHANGED);

 if (node^.tn_mode and (S_ISUID or S_ISGID))<>0 then
 begin
  //if (priv_check_cred(v^.a_cred, PRIV_VFS_RETAINSUGID, 0)) then
  // node^.tn_mode:=node^.tn_mode and (not (S_ISUID or S_ISGID));
 end;

 if (error<>0) then
  tmpfs_reg_resize(vp, oldsize, TRUE);

_out:
 Assert(IMPLIES(error=0, uio^.uio_resid=0));
 Assert(IMPLIES(error<>0, oldsize=node^.tn_size));

 Result:=error;
end;

function tmpfs_fsync(v:p_vop_fsync_args):Integer;
var
 vp:p_vnode;
begin
 vp:=v^.a_vp;

 Assert(VOP_ISLOCKED(vp)<>0);

 tmpfs_update(vp);

 Result:=0;
end;

function tmpfs_remove(v:p_vop_remove_args):Integer;
label
 _out;
var
 dvp:p_vnode;
 vp:p_vnode;
 error:Integer;
 de:p_tmpfs_dirent;
 tmp:p_tmpfs_mount;
 dnode:p_tmpfs_node;
 node:p_tmpfs_node;
begin
 dvp:=v^.a_dvp;
 vp:=v^.a_vp;

 Assert(VOP_ISLOCKED(dvp)<>0);
 Assert(VOP_ISLOCKED(vp)<>0);

 if (vp^.v_type=VDIR) then
 begin
  error:=EISDIR;
  goto _out;
 end;

 dnode:=VP_TO_TMPFS_DIR(dvp);
 node:=VP_TO_TMPFS_NODE(vp);
 tmp:=VFS_TO_TMPFS(vp^.v_mount);
 de:=tmpfs_dir_lookup(dnode, node, v^.a_cnp);
 Assert(de<>nil);

 { Files marked as immutable or append-only cannot be deleted. }
 if ((node^.tn_flags and (IMMUTABLE or APPEND or NOUNLINK))<>0) OR
    ((dnode^.tn_flags and APPEND)<>0) then
 begin
  error:=EPERM;
  goto _out;
 end;

 { Remove the entry from the directory; as it is a file, we do not
  * have to change the number of hard links of the directory. }
 tmpfs_dir_detach(dvp, de);

 if (v^.a_cnp^.cn_flags and DOWHITEOUT)<>0 then
  tmpfs_dir_whiteout_add(dvp, v^.a_cnp);

 { Free the directory entry we just deleted.  Note that the node
  * referred by it will not be removed until the vnode is really
  * reclaimed. }
 tmpfs_free_dirent(tmp, de, TRUE);

 node^.tn_status:=node^.tn_status or TMPFS_NODE_ACCESSED or TMPFS_NODE_CHANGED;
 error:=0;

_out:

 Result:=error;
end;

function tmpfs_link(v:p_vop_link_args):Integer;
label
 _out;
var
 dvp:p_vnode;
 vp:p_vnode;
 cnp:p_componentname;
 de:p_tmpfs_dirent;
 node:p_tmpfs_node;
 error:Integer;
begin
 dvp:=v^.a_tdvp;
 vp:=v^.a_vp;
 cnp:=v^.a_cnp;

 Assert(VOP_ISLOCKED(dvp)<>0);
 Assert((cnp^.cn_flags and HASBUF)<>0);
 Assert(dvp<>vp); { XXX When can this be false? }

 node:=VP_TO_TMPFS_NODE(vp);

 { XXX: Why aren't the following two tests done by the caller? }

 { Hard links of directories are forbidden. }
 if (vp^.v_type=VDIR) then
 begin
  error:=EPERM;
  goto _out;
 end;

 { Cannot create cross-device links. }
 if (dvp^.v_mount<>vp^.v_mount) then
 begin
  error:=EXDEV;
  goto _out;
 end;

 { Ensure that we do not overflow the maximum number of links imposed
  * by the system. }
 Assert(node^.tn_links <= LINK_MAX);
 if (node^.tn_links=LINK_MAX) then
 begin
  error:=EMLINK;
  goto _out;
 end;

 { We cannot create links of files marked immutable or append-only. }
 if (node^.tn_flags and (IMMUTABLE or APPEND))<>0 then
 begin
  error:=EPERM;
  goto _out;
 end;

 { Allocate a new directory entry to represent the node. }
 error:=tmpfs_alloc_dirent(VFS_TO_TMPFS(vp^.v_mount), node, cnp^.cn_nameptr, cnp^.cn_namelen, @de);
 if (error<>0) then
  goto _out;

 { Insert the new directory entry into the appropriate directory. }
 if (cnp^.cn_flags and ISWHITEOUT)<>0 then
  tmpfs_dir_whiteout_remove(dvp, cnp);

 tmpfs_dir_attach(dvp, de);

 { vp link count has changed, so update node times. }
 node^.tn_status:=node^.tn_status or TMPFS_NODE_CHANGED;
 tmpfs_update(vp);

 error:=0;

_out:
 Result:=error;
end;

function tmpfs_rename_relock(fdvp:p_vnode;fvpp:pp_vnode;tdvp:p_vnode;tvpp:pp_vnode;fcnp,tcnp:p_componentname):Integer;
label
 _relock,
 _releout;
var
 nvp:p_vnode;
 mp:p_mount;
 de:p_tmpfs_dirent;
 error,restarts:Integer;
begin
 restarts:=0;

 VOP_UNLOCK(tdvp, 0);
 if (tvpp^<>nil) AND (tvpp^<>tdvp) then
  VOP_UNLOCK(tvpp^, 0);

 mp:=fdvp^.v_mount;

_relock:
 restarts:=restarts + 1;
 error:=vn_lock(fdvp, LK_EXCLUSIVE,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 if (error<>0) then
  goto _releout;

 if (vn_lock(tdvp, LK_EXCLUSIVE or LK_NOWAIT,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%})<>0) then
 begin
  VOP_UNLOCK(fdvp, 0);
  error:=vn_lock(tdvp, LK_EXCLUSIVE,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  if (error<>0) then
   goto _releout;

  VOP_UNLOCK(tdvp, 0);
  goto _relock;
 end;

 {
  * Re-resolve fvp to be certain it still exists and fetch the
  * correct vnode.
  }
 de:=tmpfs_dir_lookup(VP_TO_TMPFS_DIR(fdvp), nil, fcnp);
 if (de=nil) then
 begin
  VOP_UNLOCK(fdvp, 0);
  VOP_UNLOCK(tdvp, 0);

  if ((fcnp^.cn_flags and ISDOTDOT)<>0) OR
     ((fcnp^.cn_namelen=1) AND (fcnp^.cn_nameptr[0]='.')) then
   error:=EINVAL
  else
   error:=ENOENT;

  goto _releout;
 end;

 error:=tmpfs_alloc_vp(mp, de^.td_node, LK_EXCLUSIVE or LK_NOWAIT, @nvp);
 if (error<>0) then
 begin
  VOP_UNLOCK(fdvp, 0);
  VOP_UNLOCK(tdvp, 0);
  if (error<>EBUSY) then
   goto _releout;

  error:=tmpfs_alloc_vp(mp, de^.td_node, LK_EXCLUSIVE, @nvp);
  if (error<>0) then
   goto _releout;

  VOP_UNLOCK(nvp, 0);
  {
   * Concurrent rename race.
   }
  if (nvp=tdvp) then
  begin
   vrele(nvp);
   error:=EINVAL;
   goto _releout;
  end;

  vrele(fvpp^);
  fvpp^:=nvp;
  goto _relock;
 end;

 vrele(fvpp^);
 fvpp^:=nvp;
 VOP_UNLOCK(fvpp^, 0);
 {
  * Re-resolve tvp and acquire the vnode lock if present.
  }
 de:=tmpfs_dir_lookup(VP_TO_TMPFS_DIR(tdvp), nil, tcnp);
 {
  * If tvp disappeared we just carry on.
  }
 if (de=nil) AND (tvpp^<>nil) then
 begin
  vrele(tvpp^);
  tvpp^:=nil;
 end;

 {
  * Get the tvp ino if the lookup succeeded.  We may have to restart
  * if the non-blocking acquire fails.
  }
 if (de<>nil) then
 begin
  nvp:=nil;
  error:=tmpfs_alloc_vp(mp, de^.td_node, LK_EXCLUSIVE or LK_NOWAIT, @nvp);
  if (tvpp^<>nil) then
   vrele(tvpp^);

  tvpp^:=nvp;

  if (error<>0) then
  begin
   VOP_UNLOCK(fdvp, 0);
   VOP_UNLOCK(tdvp, 0);
   if (error<>EBUSY) then
    goto _releout;

   error:=tmpfs_alloc_vp(mp, de^.td_node, LK_EXCLUSIVE, @nvp);

   if (error<>0) then
    goto _releout;

   VOP_UNLOCK(nvp, 0);
   {
    * fdvp contains fvp, thus tvp (=fdvp) is not empty.
    }
   if (nvp=fdvp) then
   begin
    error:=ENOTEMPTY;
    goto _releout;
   end;

   goto _relock;
  end;
 end;

 tmpfs_rename_restarts:=tmpfs_rename_restarts + restarts;

 Exit(0);

_releout:
 vrele(fdvp);
 vrele(fvpp^);
 vrele(tdvp);
 if (tvpp^<>nil) then
  vrele(tvpp^);

 tmpfs_rename_restarts:= + restarts;

 Exit(error);
end;

function tmpfs_rename(v:p_vop_rename_args):Integer;
label
 _out,
 _out_locked;
var
 fdvp:p_vnode;
 fvp:p_vnode;
 fcnp:p_componentname;
 tdvp:p_vnode;
 tvp:p_vnode;
 tcnp:p_componentname;
 mp:p_mount;
 newname:PChar;
 error:Integer;
 de:p_tmpfs_dirent;
 tmp:p_tmpfs_mount;
 fdnode:p_tmpfs_node;
 fnode:p_tmpfs_node;
 tnode:p_tmpfs_node;
 tdnode:p_tmpfs_node;
 n:p_tmpfs_node;
 parent:p_tmpfs_node;
begin
 fdvp:=v^.a_fdvp;
 fvp:=v^.a_fvp;
 fcnp:=v^.a_fcnp;
 tdvp:=v^.a_tdvp;
 tvp:=v^.a_tvp;
 tcnp:=v^.a_tcnp;
 mp:=nil;

 Assert(VOP_ISLOCKED(tdvp)<>0);
 Assert(IMPLIES(tvp<>nil, VOP_ISLOCKED(tvp)<>0));
 Assert((fcnp^.cn_flags and HASBUF)<>0);
 Assert((tcnp^.cn_flags and HASBUF)<>0);

 { Disallow cross-device renames.
  * XXX Why isn't this done by the caller? }
 if (fvp^.v_mount<>tdvp^.v_mount) OR
    ((tvp<>nil) AND (fvp^.v_mount<>tvp^.v_mount)) then
 begin
  error:=EXDEV;
  goto _out;
 end;

 { If source and target are the same file, there is nothing to do. }
 if (fvp=tvp) then
 begin
  error:=0;
  goto _out;
 end;

 { If we need to move the directory between entries, lock the
  * source so that we can safely operate on it. }
 if (fdvp<>tdvp) AND (fdvp<>tvp) then
 begin
  if (vn_lock(fdvp, LK_EXCLUSIVE or LK_NOWAIT,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%})<>0) then
  begin
   mp:=tdvp^.v_mount;
   error:=vfs_busy(mp, 0);
   if (error<>0) then
   begin
    mp:=nil;
    goto _out;
   end;

   error:=tmpfs_rename_relock(fdvp, @fvp, tdvp, @tvp, fcnp, tcnp);
   if (error<>0) then
   begin
    vfs_unbusy(mp);
    Exit(error);
   end;

   ASSERT_VOP_ELOCKED(fdvp,'tmpfs_rename: fdvp not locked');
   ASSERT_VOP_ELOCKED(tdvp,'tmpfs_rename: tdvp not locked');
   if (tvp<>nil) then
    ASSERT_VOP_ELOCKED(tvp,'tmpfs_rename: tvp not locked');

   if (fvp=tvp) then
   begin
    error:=0;
    goto _out_locked;
   end;

  end;
 end;

 tmp:=VFS_TO_TMPFS(tdvp^.v_mount);
 tdnode:=VP_TO_TMPFS_DIR(tdvp);

 if (tvp=nil) then
  tnode:=nil
 else
  tnode:=VP_TO_TMPFS_NODE(tvp);

 fdnode:=VP_TO_TMPFS_DIR(fdvp);
 fnode:=VP_TO_TMPFS_NODE(fvp);
 de:=tmpfs_dir_lookup(fdnode, fnode, fcnp);

 { Entry can disappear before we lock fdvp,
  * also avoid manipulating '.' and '..' entries. }
 if (de=nil) then
 begin
  if ((fcnp^.cn_flags and ISDOTDOT)<>0) OR
     ((fcnp^.cn_namelen=1) AND (fcnp^.cn_nameptr[0]='.')) then
   error:=EINVAL
  else
   error:=ENOENT;
  goto _out_locked;
 end;

 Assert(de^.td_node=fnode);

 { If re-naming a directory to another preexisting directory
  * ensure that the target directory is empty so that its
  * removal causes no side effects.
  * Kern_rename gurantees the destination to be a directory
  * if the source is one. }
 if (tvp<>nil) then
 begin
  Assert(tnode<>nil);

  if ((tnode^.tn_flags and (NOUNLINK or IMMUTABLE or APPEND))<>0) OR
     ((tdnode^.tn_flags and (APPEND or IMMUTABLE))<>0) then
  begin
   error:=EPERM;
   goto _out_locked;
  end;

  if (fnode^.tn_type=VDIR) AND (tnode^.tn_type=VDIR) then
  begin
   if (tnode^.tn_size > 0) then
   begin
    error:=ENOTEMPTY;
    goto _out_locked;
   end;
  end else
  if (fnode^.tn_type=VDIR) AND (tnode^.tn_type<>VDIR) then
  begin
   error:=ENOTDIR;
   goto _out_locked;
  end else
  if (fnode^.tn_type<>VDIR) AND (tnode^.tn_type=VDIR) then
  begin
   error:=EISDIR;
   goto _out_locked;
  end else
  begin
   Assert((fnode^.tn_type<>VDIR) AND (tnode^.tn_type<>VDIR));
  end;
 end;

 if ((fnode^.tn_flags and (NOUNLINK or IMMUTABLE or APPEND))<>0) OR
    ((fdnode^.tn_flags and (APPEND or IMMUTABLE))<>0) then
 begin
  error:=EPERM;
  goto _out_locked;
 end;

 { Ensure that we have enough memory to hold the new name, if it
  * has to be changed. }
 if (fcnp^.cn_namelen<>tcnp^.cn_namelen) OR
    (CompareByte(fcnp^.cn_nameptr^, tcnp^.cn_nameptr^, fcnp^.cn_namelen)<>0) then
 begin
  newname:=AllocMem(tcnp^.cn_namelen)
 end else
  newname:=nil;

 { If the node is being moved to another directory, we have to do
  * the move. }
 if (fdnode<>tdnode) then
 begin
  { In case we are moving a directory, we have to adjust its
   * parent to point to the new parent. }
  if (de^.td_node^.tn_type=VDIR) then
  begin
   { Ensure the target directory is not a child of the
    * directory being moved.  Otherwise, we'd end up
    * with stale nodes. }
   n:=tdnode;
   { TMPFS_LOCK garanties that no nodes are freed while
    * traversing the list. Nodes can only be marked as
    * removed: tn_parent=nil. }
   TMPFS_LOCK(tmp);
   TMPFS_NODE_LOCK(n);

   while (n<>n^.tn_parent) do
   begin

    if (n=fnode) then
    begin
     TMPFS_NODE_UNLOCK(n);
     TMPFS_UNLOCK(tmp);
     error:=EINVAL;
     if (newname<>nil) then  FreeMem(newname);
     goto _out_locked;
    end;

    parent:=n^.tn_parent;
    TMPFS_NODE_UNLOCK(n);
    if (parent=nil) then
    begin
     n:=nil;
     break;
    end;

    TMPFS_NODE_LOCK(parent);
    if (parent^.tn_parent=nil) then
    begin
     TMPFS_NODE_UNLOCK(parent);
     n:=nil;
     break;
    end;
    n:=parent;
   end; //while

   TMPFS_UNLOCK(tmp);
   if (n=nil) then
   begin
    error:=EINVAL;
    if (newname<>nil) then FreeMem(newname);
    goto _out_locked;
   end;

   TMPFS_NODE_UNLOCK(n);

   { Adjust the parent pointer. }
   TMPFS_VALIDATE_DIR(fnode);
   TMPFS_NODE_LOCK(de^.td_node);
   de^.td_node^.tn_parent:=tdnode;
   TMPFS_NODE_UNLOCK(de^.td_node);

   { As a result of changing the target of the '..'
    * entry, the link count of the source and target
    * directories has to be adjusted. }
   TMPFS_NODE_LOCK(tdnode);
   TMPFS_ASSERT_LOCKED(tdnode);
   Inc(tdnode^.tn_links);
   TMPFS_NODE_UNLOCK(tdnode);

   TMPFS_NODE_LOCK(fdnode);
   TMPFS_ASSERT_LOCKED(fdnode);
   Dec(fdnode^.tn_links);
   TMPFS_NODE_UNLOCK(fdnode);
  end;

  { Do the move: just remove the entry from the source directory
   * and insert it into the target one. }
  tmpfs_dir_detach(fdvp, de);

  if (fcnp^.cn_flags and DOWHITEOUT)<>0 then
   tmpfs_dir_whiteout_add(fdvp, fcnp);

  if (tcnp^.cn_flags and ISWHITEOUT)<>0 then
   tmpfs_dir_whiteout_remove(tdvp, tcnp);

  tmpfs_dir_attach(tdvp, de);
 end;

 { If the name has changed, we need to make it effective by changing
  * it in the directory entry. }
 if (newname<>nil) then
 begin
  Assert(tcnp^.cn_namelen <= MAXNAMLEN);

  FreeMem(de^.td_name);
  de^.td_namelen:=tcnp^.cn_namelen;
  Move(tcnp^.cn_nameptr^, newname^, tcnp^.cn_namelen);
  de^.td_name:=newname;

  fnode^.tn_status :=fnode^.tn_status  or TMPFS_NODE_CHANGED;
  tdnode^.tn_status:=tdnode^.tn_status or TMPFS_NODE_MODIFIED;
 end;

 { If we are overwriting an entry, we have to remove the old one
  * from the target directory. }
 if (tvp<>nil) then
 begin
  { Remove the old entry from the target directory. }
  de:=tmpfs_dir_lookup(tdnode, tnode, tcnp);
  tmpfs_dir_detach(tdvp, de);

  { Free the directory entry we just deleted.  Note that the
   * node referred by it will not be removed until the vnode is
   * really reclaimed. }
  tmpfs_free_dirent(VFS_TO_TMPFS(tvp^.v_mount), de, TRUE);
 end;

 //cache_purge(fvp);
 //if (tvp<>nil) then
 // cache_purge(tvp);
 //cache_purge_negative(tdvp);

 error:=0;

_out_locked:
 if (fdvp<>tdvp) AND (fdvp<>tvp) then
  VOP_UNLOCK(fdvp, 0);

_out:
 { Release target nodes. }
 { XXX: I don't understand when tdvp can be the same as tvp, but
  * other code takes care of this... }
 if (tdvp=tvp) then
  vrele(tdvp)
 else
  vput(tdvp);

 if (tvp<>nil) then
  vput(tvp);

 { Release source nodes. }
 vrele(fdvp);
 vrele(fvp);

 if (mp<>nil) then
  vfs_unbusy(mp);

 Result:=error;
end;

function tmpfs_mkdir(v:p_vop_mkdir_args):Integer;
var
 dvp:p_vnode;
 vpp:pp_vnode;
 cnp:p_componentname;
 vap:p_vattr;
begin
 dvp:=v^.a_dvp;
 vpp:=v^.a_vpp;
 cnp:=v^.a_cnp;
 vap:=v^.a_vap;

 Assert(vap^.va_type=VDIR);

 Result:=tmpfs_alloc_file(dvp, vpp, vap, cnp, nil);
end;

function tmpfs_rmdir(v:p_vop_rmdir_args):Integer;
label
 _out;
var
 dvp:p_vnode;
 vp:p_vnode;
 error:Integer;
 de:p_tmpfs_dirent;
 tmp:p_tmpfs_mount;
 dnode:p_tmpfs_node;
 node:p_tmpfs_node;
begin
 dvp:=v^.a_dvp;
 vp:=v^.a_vp;

 Assert(VOP_ISLOCKED(dvp)<>0);
 Assert(VOP_ISLOCKED(vp)<>0);

 tmp:=VFS_TO_TMPFS(dvp^.v_mount);
 dnode:=VP_TO_TMPFS_DIR(dvp);
 node:=VP_TO_TMPFS_DIR(vp);

 { Directories with more than two entries ('.' and '..') cannot be
  * removed. }
 if (node^.tn_size > 0) then
 begin
  error:=ENOTEMPTY;
  goto _out;
 end;

 if ((dnode^.tn_flags and APPEND)<>0) OR
    ((node^.tn_flags and (NOUNLINK or IMMUTABLE or APPEND))<>0) then
 begin
  error:=EPERM;
  goto _out;
 end;

 { This invariant holds only if we are not trying to remove '..'.
   * We checked for that above so this is safe now. }
 Assert(node^.tn_parent=dnode);

 { Get the directory entry associated with node (vp).  This was
  * filled by tmpfs_lookup while looking up the entry. }
 de:=tmpfs_dir_lookup(dnode, node, v^.a_cnp);
 Assert(TMPFS_DIRENT_MATCHES(de, v^.a_cnp^.cn_nameptr, v^.a_cnp^.cn_namelen));

 { Check flags to see if we are allowed to remove the directory. }
 if ((dnode^.tn_flags and APPEND)<>0) OR ((node^.tn_flags and (NOUNLINK or IMMUTABLE or APPEND))<>0) then
 begin
  error:=EPERM;
  goto _out;
 end;

 { Detach the directory entry from the directory (dnode). }
 tmpfs_dir_detach(dvp, de);

 if (v^.a_cnp^.cn_flags and DOWHITEOUT)<>0 then
  tmpfs_dir_whiteout_add(dvp, v^.a_cnp);

 { No vnode should be allocated for this entry from this point }
 TMPFS_NODE_LOCK(node);
 TMPFS_ASSERT_ELOCKED(node);
 Dec(node^.tn_links);
 node^.tn_parent:=nil;
 node^.tn_status:=node^.tn_status or TMPFS_NODE_ACCESSED or TMPFS_NODE_CHANGED or TMPFS_NODE_MODIFIED;

 TMPFS_NODE_UNLOCK(node);

 TMPFS_NODE_LOCK(dnode);
 TMPFS_ASSERT_ELOCKED(dnode);
 Dec(dnode^.tn_links);
 dnode^.tn_status:=dnode^.tn_status or TMPFS_NODE_ACCESSED or TMPFS_NODE_CHANGED or TMPFS_NODE_MODIFIED;
 TMPFS_NODE_UNLOCK(dnode);

 //cache_purge(dvp);
 //cache_purge(vp);

 { Free the directory entry we just deleted.  Note that the node
  * referred by it will not be removed until the vnode is really
  * reclaimed. }
 tmpfs_free_dirent(tmp, de, TRUE);

 { Release the deleted vnode (will destroy the node, notify
  * interested parties and clean it from the cache). }

 dnode^.tn_status:=dnode^.tn_status or TMPFS_NODE_CHANGED;
 tmpfs_update(dvp);

 error:=0;

_out:
 Result:=error;
end;

function tmpfs_symlink(v:p_vop_symlink_args):Integer;
var
 dvp:p_vnode;
 vpp:pp_vnode;
 cnp:p_componentname;
 vap:p_vattr;
 target:PChar;
begin
 dvp:=v^.a_dvp;
 vpp:=v^.a_vpp;
 cnp:=v^.a_cnp;
 vap:=v^.a_vap;
 target:=v^.a_target;

 vap^.va_type:=VLNK;

 Result:=tmpfs_alloc_file(dvp, vpp, vap, cnp, target);
end;

function tmpfs_readdir(v:p_vop_readdir_args):Integer;
label
 _outok;
var
 vp:p_vnode;
 uio:p_uio;
 eofflag:PInteger;
 cookies:^PQWORD;
 ncookies:PInteger;
 error:Integer;
 startoff:Int64;
 cnt:Int64;
 node:p_tmpfs_node;
 i:Int64;
 off:Int64;
 de:p_tmpfs_dirent;
begin
 vp:=v^.a_vp;
 uio:=v^.a_uio;
 eofflag:=v^.a_eofflag;
 cookies:=v^.a_cookies;
 ncookies:=v^.a_ncookies;

 cnt:=0;

 { This operation only makes sense on directory nodes. }
 if (vp^.v_type<>VDIR) then
  Exit(ENOTDIR);

 node:=VP_TO_TMPFS_DIR(vp);

 startoff:=uio^.uio_offset;

 if (uio^.uio_offset=TMPFS_DIRCOOKIE_DOT) then
 begin
  error:=tmpfs_dir_getdotdent(node, uio);
  if (error<>0) then
   goto _outok;

  Inc(cnt);
 end;

 if (uio^.uio_offset=TMPFS_DIRCOOKIE_DOTDOT) then
 begin
  error:=tmpfs_dir_getdotdotdent(node, uio);
  if (error<>0) then
   goto _outok;

  Inc(cnt);
 end;

 error:=tmpfs_dir_getdents(node, uio, @cnt);

_outok:
 Assert(error >= -1);

 if (error=-1) then
 begin
  if (cnt<>0) then
   error:=0
  else
   error:=EINVAL;
 end;

 if (eofflag<>nil) then
  eofflag^:=ord((error=0) AND (uio^.uio_offset=TMPFS_DIRCOOKIE_EOF));

 { Update NFS-related variables. }
 if (error=0) AND (cookies<>nil) AND (ncookies<>nil) then
 begin
  off:=startoff;
  de:=nil;

  ncookies^:=cnt;
  cookies^:=AllocMem(cnt * sizeof(Int64));

  if (cnt<>0) then
  for i:=0 to cnt-1 do
  begin
   Assert(off<>TMPFS_DIRCOOKIE_EOF);

   if (off=TMPFS_DIRCOOKIE_DOT) then
   begin
    off:=TMPFS_DIRCOOKIE_DOTDOT;
   end else
   begin
    if (off=TMPFS_DIRCOOKIE_DOTDOT) then
    begin
     de:=TAILQ_FIRST(@node^.tn_spec.tn_dir.tn_dirhead);
    end else
    if (de<>nil) then
    begin
     de:=TAILQ_NEXT(de, @de^.td_entries);
    end else
    begin
     de:=tmpfs_dir_lookupbycookie(node,off);
     Assert(de<>nil);
     de:=TAILQ_NEXT(de, @de^.td_entries);
    end;

    if (de=nil) then
     off:=TMPFS_DIRCOOKIE_EOF
    else
     off:=tmpfs_dircookie(de);

   end;

   (cookies^)[i]:=off;
  end;

  Assert(uio^.uio_offset=off);
 end;

 Result:=error;
end;

function tmpfs_readlink(v:p_vop_readlink_args):Integer;
var
 vp:p_vnode;
 uio:p_uio;
 error:Integer;
 node:p_tmpfs_node;
begin
 vp:=v^.a_vp;
 uio:=v^.a_uio;

 Assert(uio^.uio_offset=0);
 Assert(vp^.v_type=VLNK);

 node:=VP_TO_TMPFS_NODE(vp);

 error:=uiomove(node^.tn_link, MIN(node^.tn_size, uio^.uio_resid), uio);

 node^.tn_status:=node^.tn_status or TMPFS_NODE_ACCESSED;

 Result:=error;
end;

function tmpfs_inactive(v:p_vop_inactive_args):Integer;
var
 vp:p_vnode;
 node:p_tmpfs_node;
begin
 vp:=v^.a_vp;

 Assert(VOP_ISLOCKED(vp)<>0);

 node:=VP_TO_TMPFS_NODE(vp);

 if (node^.tn_links=0) then
  vrecycle(vp);

 Result:=0;
end;

function tmpfs_reclaim(v:p_vop_reclaim_args):Integer;
var
 vp:p_vnode;
 tmp:p_tmpfs_mount;
 node:p_tmpfs_node;
begin
 vp:=v^.a_vp;

 node:=VP_TO_TMPFS_NODE(vp);
 tmp:=VFS_TO_TMPFS(vp^.v_mount);

 vnode_destroy_vobject(vp);
 //cache_purge(vp);

 TMPFS_NODE_LOCK(node);
 TMPFS_ASSERT_ELOCKED(node);
 tmpfs_free_vp(vp);

 { If the node referenced by this vnode was deleted by the user,
  * we must free its associated data structures (now that the vnode
  * is being reclaimed). }
 if (node^.tn_links=0) AND
    ((node^.tn_vpstate and TMPFS_VNODE_ALLOCATING)=0) then
 begin
  node^.tn_vpstate:=TMPFS_VNODE_DOOMED;
  TMPFS_NODE_UNLOCK(node);
  tmpfs_free_node(tmp, node);
 end else
  TMPFS_NODE_UNLOCK(node);

 Assert(vp^.v_data=nil);
 Result:=0;
end;

function tmpfs_print(v:p_vop_print_args):Integer;
var
 vp:p_vnode;
 node:p_tmpfs_node;
begin
 vp:=v^.a_vp;

 node:=VP_TO_TMPFS_NODE(vp);

 //printf('tag VT_TMPFS, tmpfs_node %p, flags 0x%x, links %d\n', node, node^.tn_flags, node^.tn_links);
 //printf('\tmode 0%o, owner %d, group %d, size %' PRIdMAX
 //    ', status 0x%x\n',
 //    node^.tn_mode, node^.tn_uid, node^.tn_gid,
 //    (uintmax_t)node^.tn_size, node^.tn_status);

 //if (vp^.v_type=VFIFO) then
 // fifo_printinfo(vp);

 //printf('\n');

 Result:=0;
end;

function tmpfs_pathconf(v:p_vop_pathconf_args):Integer;
var
 error:Integer;
 name:Integer;
 retval:PPtrUint;
begin
 name:=v^.a_name;
 retval:=v^.a_retval;

 error:=0;

 case (name) of
  _PC_LINK_MAX:
   retval^:=LINK_MAX;

  _PC_NAME_MAX:
   retval^:=NAME_MAX;

  _PC_PATH_MAX:
   retval^:=PATH_MAX;

  _PC_PIPE_BUF:
   retval^:=PIPE_BUF;

  _PC_CHOWN_RESTRICTED:
   retval^:=1;

  _PC_NO_TRUNC:
   retval^:=1;

  _PC_SYNC_IO:
   retval^:=1;

  _PC_FILESIZEBITS:
   retval^:=0; { XXX Don't know which value should I return. }

  else
   error:=EINVAL;
 end;

 Result:=error;
end;

function tmpfs_vptofh(ap:p_vop_vptofh_args):Integer;
var
 tfhp:p_tmpfs_fid;
 node:p_tmpfs_node;
begin
 tfhp:=p_tmpfs_fid(ap^.a_fhp);
 node:=VP_TO_TMPFS_NODE(ap^.a_vp);

 tfhp^.tf_len:=sizeof(tmpfs_fid);
 tfhp^.tf_id :=node^.tn_id;
 tfhp^.tf_gen:=node^.tn_gen;

 Exit(0);
end;

function tmpfs_whiteout(ap:p_vop_whiteout_args):Integer;
var
 dvp:p_vnode;
 cnp:p_componentname;
 de:p_tmpfs_dirent;
begin
 dvp:=ap^.a_dvp;
 cnp:=ap^.a_cnp;

 case (ap^.a_flags) of
  LOOKUP:
   Exit(0);
  CREATE:
   begin
    de:=tmpfs_dir_lookup(VP_TO_TMPFS_DIR(dvp), nil, cnp);

    if (de<>nil) then
    begin
     if (de^.td_node=nil) then
      Exit(0)
     else
      Exit(EEXIST);
    end;

    Exit(tmpfs_dir_whiteout_add(dvp, cnp));
   end;
  DELETE:
   begin
    tmpfs_dir_whiteout_remove(dvp, cnp);
    Exit(0);
   end;
  else
   Assert(false,'tmpfs_whiteout: unknown op');
 end;
end;

var
 tmpfs_vnodeop_entries:vop_vector=(
  vop_default       :@default_vnodeops;
  vop_bypass        :nil;
  vop_islocked      :nil;
  vop_lookup        :@tmpfs_lookup;
  vop_create        :@tmpfs_create;
  vop_whiteout      :@tmpfs_whiteout;
  vop_mknod         :@tmpfs_mknod;
  vop_open          :@tmpfs_open;
  vop_close         :@tmpfs_close;
  vop_access        :@tmpfs_access;
  vop_accessx       :nil;
  vop_getattr       :@tmpfs_getattr;
  vop_setattr       :@tmpfs_setattr;
  vop_markatime     :nil;
  vop_read          :@tmpfs_read;
  vop_write         :@tmpfs_write;
  vop_ioctl         :nil;
  vop_poll          :nil;
  vop_kqfilter      :nil;
  vop_revoke        :nil;
  vop_fsync         :@tmpfs_fsync;
  vop_remove        :@tmpfs_remove;
  vop_link          :@tmpfs_link;
  vop_rename        :@tmpfs_rename;
  vop_mkdir         :@tmpfs_mkdir;
  vop_rmdir         :@tmpfs_rmdir;
  vop_symlink       :@tmpfs_symlink;
  vop_readdir       :@tmpfs_readdir;
  vop_readlink      :@tmpfs_readlink;
  vop_inactive      :@tmpfs_inactive;
  vop_reclaim       :@tmpfs_reclaim;
  vop_lock1         :nil;
  vop_unlock        :nil;
  vop_bmap          :@VOP_EOPNOTSUPP;
  vop_strategy      :nil;
  vop_getwritemount :nil;
  vop_print         :@tmpfs_print;
  vop_pathconf      :@tmpfs_pathconf;
  vop_advlock       :nil;
  vop_advlockasync  :nil;
  vop_advlockpurge  :nil;
  vop_reallocblks   :nil;
  vop_getpages      :nil;
  vop_putpages      :nil;
  vop_vptofh        :@tmpfs_vptofh;
  vop_vptocnp       :nil;
  vop_allocate      :nil;
  vop_unp_bind      :nil;
  vop_unp_connect   :nil;
  vop_unp_detach    :nil;
 ); public;


end.

