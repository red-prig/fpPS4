unit tmpfs_subr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 errno,
 mqueue,
 systm,
 time,
 vnode,
 vmount,
 vdirent,
 vnamei,
 vmparam,
 kern_param,
 uma,
 vuio,
 vstat,
 subr_uio,
 vfs_subr,
 vfs_vnops,
 vnode_pager,
 vnode_if,
 subr_unit,
 vm,
 vm_pager,
 vm_object,
 kern_mtx,
 tmpfs;

implementation

uses
 tmpfs_fifoops,
 tmpfs_vnops;

function tmpfs_mem_avail():QWORD; public;
begin
 //vm_ooffset_t avail;

 //avail:=swap_pager_avail + cnt.v_free_count + cnt.v_cache_count - tmpfs_pages_reserved;
 //if (__predict_false(avail < 0))
 // avail:=0;

 //Exit(avail);

 Exit(High(Int64));
end;

function howmany(x,y:QWORD):QWORD; inline;
begin
 Result:=(x+(y-1)) div y;
end;

function tmpfs_pages_used(tmp:p_tmpfs_mount):QWORD; public;
const
 node_size=sizeof(tmpfs_node) + sizeof(tmpfs_dirent);
var
 meta_pages:QWORD;
begin
 meta_pages:=howmany(tmp^.tm_nodes_inuse * node_size, PAGE_SIZE);

 Exit(meta_pages + tmp^.tm_pages_used);
end;

function tmpfs_pages_check_avail(tmp:p_tmpfs_mount;req_pages:QWORD):Integer;
begin
 if (tmpfs_mem_avail() < req_pages) then
  Exit(0);

 if (tmp^.tm_pages_max<>High(QWORD)) AND
    (tmp^.tm_pages_max < (req_pages + tmpfs_pages_used(tmp))) then
   Exit(0);

 Exit(1);
end;

//

function tmpfs_alloc_node(tmp:p_tmpfs_mount;_type:vtype;uid,gid,mode:DWORD;parent:p_tmpfs_node;target:PChar;rdev:Integer;node:pp_tmpfs_node):Integer; public;
var
 nnode:p_tmpfs_node;
begin
 Assert(IMPLIES((tmp^.tm_root=nil), (parent=nil) AND (_type=VDIR)));

 Assert(IFF((_type=VLNK), (target<>nil)));
 Assert(IFF((_type=VBLK) OR (_type=VCHR), (rdev<>VNOVAL)));

 if (tmp^.tm_nodes_inuse >= tmp^.tm_nodes_max) then
  Exit(ENOSPC);

 if (tmpfs_pages_check_avail(tmp, 1)=0) then
  Exit(ENOSPC);

 nnode:=uma_zalloc_arg(tmp^.tm_node_pool, tmp, M_WAITOK);

 { Generic initialization. }
 nnode^.tn_type:=_type;

 vfs_timestamp(@nnode^.tn_atime);
 nnode^.tn_birthtime:=nnode^.tn_atime;
 nnode^.tn_ctime    :=nnode^.tn_atime;
 nnode^.tn_mtime    :=nnode^.tn_atime;

 nnode^.tn_uid :=uid;
 nnode^.tn_gid :=gid;
 nnode^.tn_mode:=mode;
 nnode^.tn_id  :=alloc_unr(tmp^.tm_ino_unr);

 { Type-specific initialization. }
 case (nnode^.tn_type) of
  VBLK,
  VCHR:
   nnode^.tn_rdev:=rdev;

  VDIR:
   begin
    TAILQ_INIT(@nnode^.tn_spec.tn_dir.tn_dirhead);
    Assert(parent<>nnode);
    Assert(IMPLIES(parent=nil, tmp^.tm_root=nil));

    if (parent=nil) then
     nnode^.tn_parent:=nnode
    else
     nnode^.tn_parent:=parent;

    nnode^.tn_readdir_lastn:=0;
    nnode^.tn_readdir_lastp:=nil;
    Inc(nnode^.tn_links);
    TMPFS_NODE_LOCK(nnode^.tn_parent);
    Inc(nnode^.tn_parent^.tn_links);
    TMPFS_NODE_UNLOCK(nnode^.tn_parent);
   end;

  VFIFO,
  VSOCK:;

  VLNK:
   begin
    Assert(strlen(target) < MAXPATHLEN);
    nnode^.tn_size:=strlen(target);
    nnode^.tn_link:=AllocMem(nnode^.tn_size);
    Move(target^, nnode^.tn_link^, nnode^.tn_size);
   end;

  VREG:
   nnode^.tn_aobj:=vm_pager_allocate(OBJT_SWAP, nil, 0, VM_PROT_DEFAULT, 0);

  else
   Assert(false,'tmpfs_alloc_node: type %p %d');
 end;

 TMPFS_LOCK(tmp);
 LIST_INSERT_HEAD(@tmp^.tm_nodes_used, nnode, @nnode^.tn_entries);
 Inc(tmp^.tm_nodes_inuse);
 TMPFS_UNLOCK(tmp);

 node^:=nnode;
 Result:=0;
end;

procedure tmpfs_free_node(tmp:p_tmpfs_mount;node:p_tmpfs_node); public;
var
 uobj:vm_object_t;
begin

 TMPFS_NODE_LOCK(node);
 Assert(node^.tn_vnode=nil);
 Assert((node^.tn_vpstate and TMPFS_VNODE_ALLOCATING)=0);
 TMPFS_NODE_UNLOCK(node);

 TMPFS_LOCK(tmp);
 LIST_REMOVE(node, @node^.tn_entries);
 Dec(tmp^.tm_nodes_inuse);
 TMPFS_UNLOCK(tmp);

 case (node^.tn_type) of
  VNON,
  VBLK,
  VCHR,
  VDIR,
  VFIFO,
  VSOCK:;

  VLNK:
   FreeMem(node^.tn_link);

  VREG:
   begin
    uobj:=node^.tn_aobj;
    if (uobj<>nil) then
    begin
     TMPFS_LOCK(tmp);
     tmp^.tm_pages_used:=tmp^.tm_pages_used - uobj^.size;
     TMPFS_UNLOCK(tmp);
     vm_object_deallocate(uobj);
    end
   end;

  else
   Assert(false,'tmpfs_free_node: type %p %d');
 end;

 free_unr(tmp^.tm_ino_unr, node^.tn_id);
 uma_zfree(tmp^.tm_node_pool, node);
end;

function tmpfs_alloc_dirent(tmp:p_tmpfs_mount;node:p_tmpfs_node;name:PChar;len:Word;de:pp_tmpfs_dirent):Integer; public;
var
 nde:p_tmpfs_dirent;
begin
 nde:=uma_zalloc(tmp^.tm_dirent_pool, M_WAITOK);

 nde^.td_name:=AllocMem(len);
 nde^.td_namelen:=len;
 Move(name^, nde^.td_name^, len);

 nde^.td_node:=node;
 if (node<>nil) then
  Inc(node^.tn_links);

 de^:=nde;

 Result:=0;
end;

procedure tmpfs_free_dirent(tmp:p_tmpfs_mount;de:p_tmpfs_dirent;node_exists:Boolean); public;
var
 node:p_tmpfs_node;
begin
 if (node_exists) then
 begin
  node:=de^.td_node;
  if (node<>nil) then
  begin
   Assert(node^.tn_links > 0);
   Dec(node^.tn_links);
  end;
 end;

 FreeMem(de^.td_name);
 uma_zfree(tmp^.tm_dirent_pool, de);
end;

function tmpfs_alloc_vp(mp:p_mount;node:p_tmpfs_node;lkflag:Integer;vpp:pp_vnode):Integer; public;
label
 _loop,
 _loop1,
 _out,
 _unlock;
var
 error:Integer;
 vp:p_vnode;
begin

_loop:
 TMPFS_NODE_LOCK(node);
_loop1:
 vp:=node^.tn_vnode;
 if (vp<>nil) then
 begin
  Assert((node^.tn_vpstate and TMPFS_VNODE_DOOMED)=0);
  VI_LOCK(vp);
  if ((node^.tn_type=VDIR) AND (node^.tn_parent=nil)) OR
     (((vp^.v_iflag and VI_DOOMED)<>0) AND
      ((lkflag and LK_NOWAIT)<>0)) then
  begin
   VI_UNLOCK(vp);
   TMPFS_NODE_UNLOCK(node);
   error:=ENOENT;
   vp:=nil;
   goto _out;
  end;
  if ((vp^.v_iflag and VI_DOOMED)<>0) then
  begin
   VI_UNLOCK(vp);
   node^.tn_vpstate:=node^.tn_vpstate or TMPFS_VNODE_WRECLAIM;
   while ((node^.tn_vpstate and TMPFS_VNODE_WRECLAIM)<>0) do
   begin
    msleep(@node^.tn_vnode, TMPFS_NODE_MTX(node), 0, 'tmpfsE', 0);
   end;
   goto _loop1;
  end;
  TMPFS_NODE_UNLOCK(node);

  error:=vget(vp, lkflag or LK_INTERLOCK);
  if (error=ENOENT) then
   goto _loop;

  if (error<>0) then
  begin
   vp:=nil;
   goto _out;
  end;

  {
   * Make sure the vnode is still there after
   * getting the interlock to avoid racing a free.
   }
  if (node^.tn_vnode=nil) OR (node^.tn_vnode<>vp) then
  begin
   vput(vp);
   goto _loop;
  end;

  goto _out;
 end;

 if ((node^.tn_vpstate and TMPFS_VNODE_DOOMED)<>0) OR
    ((node^.tn_type=VDIR) AND (node^.tn_parent=nil)) then
 begin
  TMPFS_NODE_UNLOCK(node);
  error:=ENOENT;
  vp:=nil;
  goto _out;
 end;

 {
  * otherwise lock the vp list while we call getnewvnode
  * since that can block.
  }
 if (node^.tn_vpstate and TMPFS_VNODE_ALLOCATING)<>0 then
 begin
  node^.tn_vpstate:=node^.tn_vpstate or TMPFS_VNODE_WANT;
  error:=msleep(@node^.tn_vpstate, TMPFS_NODE_MTX(node), PDROP or PCATCH, 'tmpfs_alloc_vp', 0);
  if (error<>0) then
   Exit(error);

  goto _loop;
 end else
  node^.tn_vpstate:=node^.tn_vpstate or TMPFS_VNODE_ALLOCATING;

 TMPFS_NODE_UNLOCK(node);

 { Get a new vnode and associate it with our node. }
 error:=getnewvnode('tmpfs', mp, @tmpfs_vnodeop_entries, @vp);
 if (error<>0) then
  goto _unlock;

 Assert(vp<>nil);

 vn_lock(vp, lkflag or LK_RETRY,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%});

 vp^.v_data:=node;
 vp^.v_type:=node^.tn_type;

 { Type-specific initialization. }
 case (node^.tn_type) of
  VBLK,
  VCHR,
  VLNK,
  VREG,
  VSOCK:;

  VFIFO:
   vp^.v_op:=@tmpfs_fifoop_entries;

  VDIR:
   begin
    Assert(node^.tn_parent<>nil);
    if (node^.tn_parent=node) then
     vp^.v_vflag:=vp^.v_vflag or VV_ROOT;
   end

  else
   Assert(false,'tmpfs_alloc_vp: type %p %d');
 end;

 vnode_pager_setsize(vp, node^.tn_size);
 error:=insmntque(vp, mp);
 if (error<>0) then
  vp:=nil;

_unlock:
 TMPFS_NODE_LOCK(node);

 Assert((node^.tn_vpstate and TMPFS_VNODE_ALLOCATING)<>0);
 node^.tn_vpstate:=node^.tn_vpstate and (not TMPFS_VNODE_ALLOCATING);
 node^.tn_vnode  :=vp;

 if (node^.tn_vpstate and TMPFS_VNODE_WANT)<>0 then
 begin
  node^.tn_vpstate:=node^.tn_vpstate and (not TMPFS_VNODE_WANT);
  TMPFS_NODE_UNLOCK(node);
  wakeup(@node^.tn_vpstate);
 end else
  TMPFS_NODE_UNLOCK(node);

_out:
 vpp^:=vp;

 if (error=0) then
 begin
  Assert((vpp^<>nil) AND (VOP_ISLOCKED(vpp^)<>0));
  TMPFS_NODE_LOCK(node);
  Assert(vpp^=node^.tn_vnode);
  TMPFS_NODE_UNLOCK(node);
 end;

 Result:=error;
end;

procedure tmpfs_free_vp(vp:p_vnode); public;
var
 node:p_tmpfs_node;
begin
 node:=VP_TO_TMPFS_NODE(vp);

 mtx_assert(TMPFS_NODE_MTX(node)^);
 node^.tn_vnode:=nil;

 if ((node^.tn_vpstate and TMPFS_VNODE_WRECLAIM)<>0) then
  wakeup(@node^.tn_vnode);

 node^.tn_vpstate:=node^.tn_vpstate and (not TMPFS_VNODE_WRECLAIM);
 vp^.v_data:=nil;
end;

function tmpfs_alloc_file(dvp:p_vnode;vpp:pp_vnode;vap:p_vattr;cnp:p_componentname;target:pchar):Integer; public;
label
 _out;
var
 error :Integer;
 de    :p_tmpfs_dirent;
 tmp   :p_tmpfs_mount;
 dnode :p_tmpfs_node;
 node  :p_tmpfs_node;
 parent:p_tmpfs_node;
begin
 Assert(VOP_ISLOCKED(dvp)<>0);
 Assert((cnp^.cn_flags and HASBUF)<>0);

 tmp:=VFS_TO_TMPFS(dvp^.v_mount);
 dnode:=VP_TO_TMPFS_DIR(dvp);
 vpp^:=nil;

 { If the entry we are creating is a directory, we cannot overflow
  * the number of links of its parent, because it will get a new
  * link. }
 if (vap^.va_type=VDIR) then
 begin
  { Ensure that we do not overflow the maximum number of links
   * imposed by the system. }
  Assert(dnode^.tn_links <= LINK_MAX);
  if (dnode^.tn_links=LINK_MAX) then
  begin
   error:=EMLINK;
   goto _out;
  end;

  parent:=dnode;
  Assert(parent<>nil);
 end else
  parent:=nil;

 { Allocate a node that represents the new file. }
 error:=tmpfs_alloc_node(tmp, vap^.va_type, 0, 0, vap^.va_mode, parent, target, vap^.va_rdev, @node);
 if (error<>0) then
  goto _out;

 { Allocate a directory entry that points to the new file. }
 error:=tmpfs_alloc_dirent(tmp, node, cnp^.cn_nameptr, cnp^.cn_namelen, @de);
 if (error<>0) then
 begin
  tmpfs_free_node(tmp, node);
  goto _out;
 end;

 { Allocate a vnode for the new file. }
 error:=tmpfs_alloc_vp(dvp^.v_mount, node, LK_EXCLUSIVE, vpp);
 if (error<>0) then
 begin
  tmpfs_free_dirent(tmp, de, TRUE);
  tmpfs_free_node(tmp, node);
  goto _out;
 end;

 { Now that all required items are allocated, we can proceed to
  * insert the new node into the directory, an operation that
  * cannot fail. }
 if (cnp^.cn_flags and ISWHITEOUT)<>0 then
  tmpfs_dir_whiteout_remove(dvp, cnp);

 tmpfs_dir_attach(dvp, de);

_out:

 Result:=error;
end;

procedure tmpfs_dir_attach(vp:p_vnode;de:p_tmpfs_dirent); public;
var
 dnode:p_tmpfs_node;
begin
 ASSERT_VOP_ELOCKED(vp, {$INCLUDE %CURRENTROUTINE%});
 dnode:=VP_TO_TMPFS_DIR(vp);
 TAILQ_INSERT_TAIL(@dnode^.tn_spec.tn_dir.tn_dirhead, de, @de^.td_entries);
 dnode^.tn_size:=dnode^.tn_size + sizeof(tmpfs_dirent);
 dnode^.tn_status:=dnode^.tn_status or (TMPFS_NODE_ACCESSED or TMPFS_NODE_CHANGED or TMPFS_NODE_MODIFIED);
end;

procedure tmpfs_dir_detach(vp:p_vnode;de:p_tmpfs_dirent); public;
var
 dnode:p_tmpfs_node;
begin
 ASSERT_VOP_ELOCKED(vp, {$INCLUDE %CURRENTROUTINE%});
 dnode:=VP_TO_TMPFS_DIR(vp);

 if (dnode^.tn_readdir_lastp=de) then
 begin
  dnode^.tn_readdir_lastn:=0;
  dnode^.tn_readdir_lastp:=nil;
 end;

 TAILQ_REMOVE(@dnode^.tn_spec.tn_dir.tn_dirhead, de, @de^.td_entries);
 dnode^.tn_size  :=dnode^.tn_size - sizeof(tmpfs_dirent);
 dnode^.tn_status:=dnode^.tn_status or (TMPFS_NODE_ACCESSED or TMPFS_NODE_CHANGED or TMPFS_NODE_MODIFIED);
end;

function tmpfs_dir_lookup(node:p_tmpfs_node;f:p_tmpfs_node;cnp:p_componentname):p_tmpfs_dirent; public;
var
 de:p_tmpfs_dirent;
begin
 Assert(IMPLIES(cnp^.cn_namelen=1, cnp^.cn_nameptr[0]<>'.'));
 Assert(IMPLIES(cnp^.cn_namelen=2, not ((cnp^.cn_nameptr[0]='.') AND (cnp^.cn_nameptr[1]='.'))));
 TMPFS_VALIDATE_DIR(node);

 de:=TAILQ_FIRST(@node^.tn_spec.tn_dir.tn_dirhead);

 while (de<>nil) do
 begin
  if (f<>nil) AND (de^.td_node<>f) then
  begin
   de:=TAILQ_NEXT(de,@de^.td_entries);
   continue;
  end;

  Assert(cnp^.cn_namelen < $ffff);

  if (de^.td_namelen=cnp^.cn_namelen) AND
     (CompareByte(de^.td_name^, cnp^.cn_nameptr^, de^.td_namelen)=0) then
  begin
   break;
  end;

  //
  de:=TAILQ_NEXT(de,@de^.td_entries);
 end;

 node^.tn_status:=node^.tn_status or TMPFS_NODE_ACCESSED;

 Result:=de;
end;

function tmpfs_dir_getdotdent(node:p_tmpfs_node;uio:p_uio):Integer; public;
var
 error:Integer;
 dent:t_dirent;
begin
 TMPFS_VALIDATE_DIR(node);
 Assert(uio^.uio_offset=TMPFS_DIRCOOKIE_DOT);

 dent.d_fileno:=node^.tn_id;
 dent.d_type  :=DT_DIR;
 dent.d_namlen:=1;
 dent.d_name[0]:='.';
 dent.d_name[1]:=#0;
 dent.d_reclen:=GENERIC_DIRSIZ(@dent);

 if (dent.d_reclen > uio^.uio_resid) then
 begin
  error:=-1;
 end else
 begin
  error:=uiomove(@dent, dent.d_reclen, uio);
  if (error=0) then
   uio^.uio_offset:=TMPFS_DIRCOOKIE_DOTDOT;
 end;

 node^.tn_status:=node^.tn_status or TMPFS_NODE_ACCESSED;

 Result:=error;
end;

function tmpfs_dir_getdotdotdent(node:p_tmpfs_node;uio:p_uio):Integer; public;
var
 error:Integer;
 dent:t_dirent;
 de:p_tmpfs_dirent;
begin
 TMPFS_VALIDATE_DIR(node);
 Assert(uio^.uio_offset=TMPFS_DIRCOOKIE_DOTDOT);

 {
  * Return ENOENT if the current node is already removed.
  }
 TMPFS_ASSERT_LOCKED(node);
 if (node^.tn_parent=nil) then
 begin
  Exit(ENOENT);
 end;

 TMPFS_NODE_LOCK(node^.tn_parent);
 dent.d_fileno:=node^.tn_parent^.tn_id;
 TMPFS_NODE_UNLOCK(node^.tn_parent);

 dent.d_type  :=DT_DIR;
 dent.d_namlen:=2;
 dent.d_name[0]:='.';
 dent.d_name[1]:='.';
 dent.d_name[2]:=#0;
 dent.d_reclen:=GENERIC_DIRSIZ(@dent);

 if (dent.d_reclen > uio^.uio_resid) then
 begin
  error:=-1;
 end else
 begin
  error:=uiomove(@dent, dent.d_reclen, uio);
  if (error=0) then
  begin
   de:=TAILQ_FIRST(@node^.tn_spec.tn_dir.tn_dirhead);
   if (de=nil) then
    uio^.uio_offset:=TMPFS_DIRCOOKIE_EOF
   else
    uio^.uio_offset:=tmpfs_dircookie(de);
  end;
 end;

 node^.tn_status:=node^.tn_status or TMPFS_NODE_ACCESSED;

 Result:=error;
end;

function tmpfs_dir_lookupbycookie(node:p_tmpfs_node;cookie:QWORD):p_tmpfs_dirent; public;
var
 de:p_tmpfs_dirent;
begin

 if (cookie=node^.tn_readdir_lastn) AND
    (node^.tn_readdir_lastp<>nil) then
 begin
  Exit(node^.tn_readdir_lastp);
 end;

 de:=TAILQ_FIRST(@node^.tn_spec.tn_dir.tn_dirhead);
 while (de<>nil) do
 begin
  if (tmpfs_dircookie(de)=cookie) then
  begin
   break;
  end;
  //
  de:=TAILQ_NEXT(de,@de^.td_entries);
 end;

 Result:=de;
end;

function tmpfs_dir_getdents(node:p_tmpfs_node;uio:p_uio;cntp:PQWORD):Integer; public;
var
 error:Integer;
 startcookie:QWORD;
 de:p_tmpfs_dirent;
 d:t_dirent;
begin
 TMPFS_VALIDATE_DIR(node);

 { Locate the first directory entry we have to return.  We have cached
  * the last readdir in the node, so use those values if appropriate.
  * Otherwise do a linear scan to find the requested entry. }
 startcookie:=uio^.uio_offset;
 Assert(startcookie<>TMPFS_DIRCOOKIE_DOT);
 Assert(startcookie<>TMPFS_DIRCOOKIE_DOTDOT);

 if (startcookie=TMPFS_DIRCOOKIE_EOF) then
 begin
  Exit(0);
 end else
 begin
  de:=tmpfs_dir_lookupbycookie(node, startcookie);
 end;

 if (de=nil) then
 begin
  Exit(EINVAL);
 end;

 { Read as much entries as possible; i.e., until we reach the end of
  * the directory or we exhaust uio space. }
 repeat

  { Create a dirent structure representing the current
   * tmpfs_node and fill it. }
  if (de^.td_node=nil) then
  begin
   d.d_fileno:=1;
   d.d_type  :=DT_WHT;
  end else
  begin
   d.d_fileno:=de^.td_node^.tn_id;
   case (de^.td_node^.tn_type) of
    VBLK :d.d_type:=DT_BLK;
    VCHR :d.d_type:=DT_CHR;
    VDIR :d.d_type:=DT_DIR;
    VFIFO:d.d_type:=DT_FIFO;
    VLNK :d.d_type:=DT_LNK;
    VREG :d.d_type:=DT_REG;
    VSOCK:d.d_type:=DT_SOCK;
    else
     Assert(False,'tmpfs_dir_getdents: type %p %d');
   end;
  end;

  d.d_namlen:=de^.td_namelen;
  Assert(de^.td_namelen < sizeof(d.d_name));
  Move(de^.td_name^, d.d_name, de^.td_namelen);
  d.d_name[de^.td_namelen]:=#0;
  d.d_reclen:=GENERIC_DIRSIZ(@d);

  { Stop reading if the directory entry we are treating is
   * bigger than the amount of data that can be returned. }
  if (d.d_reclen > uio^.uio_resid) then
  begin
   error:=-1;
   break;
  end;

  { Copy the new dirent structure into the output buffer and
   * advance pointers. }
  error:=uiomove(@d, d.d_reclen, uio);
  if (error=0) then
  begin
   Inc(cntp^);
   de:=TAILQ_NEXT(de, @de^.td_entries);
  end;

 until not ((error=0) AND (uio^.uio_resid > 0) AND (de<>nil));

 { Update the offset and cache. }
 if (de=nil) then
 begin
  uio^.uio_offset:=TMPFS_DIRCOOKIE_EOF;
  node^.tn_readdir_lastn:=0;
  node^.tn_readdir_lastp:=nil;
 end else
 begin
  uio^.uio_offset:=tmpfs_dircookie(de);
  node^.tn_readdir_lastn:=uio^.uio_offset;
  node^.tn_readdir_lastp:=de;
 end;

 node^.tn_status:=node^.tn_status or TMPFS_NODE_ACCESSED;

 Result:=error;
end;

function tmpfs_dir_whiteout_add(dvp:p_vnode;cnp:p_componentname):Integer; public;
var
 de:p_tmpfs_dirent;
 error:Integer;
begin
 error:=tmpfs_alloc_dirent(VFS_TO_TMPFS(dvp^.v_mount), nil, cnp^.cn_nameptr, cnp^.cn_namelen, @de);

 if (error<>0) then
  Exit(error);

 tmpfs_dir_attach(dvp, de);
 Exit(0);
end;

procedure tmpfs_dir_whiteout_remove(dvp:p_vnode;cnp:p_componentname); public;
var
 de:p_tmpfs_dirent;
begin
 de:=tmpfs_dir_lookup(VP_TO_TMPFS_DIR(dvp), nil, cnp);
 Assert((de<>nil) AND (de^.td_node=nil));
 tmpfs_dir_detach(dvp, de);
 tmpfs_free_dirent(VFS_TO_TMPFS(dvp^.v_mount), de, TRUE);
end;

function OFF_TO_IDX(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function tmpfs_reg_resize(vp:p_vnode;newsize:QWORD;ignerr:Boolean):Integer; public;
label
 _retry;
var
 tmp:p_tmpfs_mount;
 node:p_tmpfs_node;
 uobj:vm_object_t;
 //vm_page_t m, ma[1];
 idx,newpages,oldpages:vm_pindex_t;
 oldsize:QWORD;
 base,rv:Integer;
begin
 Assert(vp^.v_type=VREG);

 node:=VP_TO_TMPFS_NODE(vp);
 uobj:=node^.tn_aobj;
 tmp:=VFS_TO_TMPFS(vp^.v_mount);

 {
  * Convert the old and new sizes to the number of pages needed to
  * store them.  It may happen that we do not need to do anything
  * because the last allocated page can accommodate the change on
  * its own.
  }
 oldsize:=node^.tn_size;
 oldpages:=OFF_TO_IDX(oldsize + PAGE_MASK);
 Assert(oldpages=uobj^.size);
 newpages:=OFF_TO_IDX(newsize + PAGE_MASK);

 if (newpages > oldpages) AND
    (tmpfs_pages_check_avail(tmp, newpages - oldpages)=0) then
  Exit(ENOSPC);

 VM_OBJECT_LOCK(uobj);
 if (newsize < oldsize) then
 begin
  {
   * Zero the truncated part of the last page.
   }
  base:=newsize and PAGE_MASK;
  if (base<>0) then
  begin
   idx:=OFF_TO_IDX(newsize);
_retry:
   {
   m:=vm_page_lookup(uobj, idx);
   if (m<>nil) then
   begin
    if ((m^.oflags and VPO_BUSY)<>0) OR (m^.busy<>0) then
    begin
     vm_page_sleep(m, "tmfssz");
     goto _retry;
    end;
    Assert(m^.valid=VM_PAGE_BITS_ALL);
   end else
   if (vm_pager_has_page(uobj, idx, nil, nil)) then
   begin
    m:=vm_page_alloc(uobj, idx, VM_ALLOC_NORMAL);
    if (m=nil) then
    begin
     VM_OBJECT_UNLOCK(uobj);
     VM_WAIT;
     VM_OBJECT_LOCK(uobj);
     goto retry;
    end else
    if (m^.valid<>VM_PAGE_BITS_ALL) then
    begin
     ma[0]:=m;
     rv:=vm_pager_get_pages(uobj, ma, 1, 0);
     m:=vm_page_lookup(uobj, idx);
    end else
     { A cached page was reactivated. }
     rv:=VM_PAGER_OK;

    vm_page_lock(m);
    if (rv=VM_PAGER_OK) then
    begin
     vm_page_deactivate(m);
     vm_page_unlock(m);
     vm_page_wakeup(m);
    end else
    begin
     vm_page_free(m);
     vm_page_unlock(m);
     if (ignerr) then
      m:=nil
     else
     begin
      VM_OBJECT_UNLOCK(uobj);
      Exit(EIO);
     end;
    end;
   end;
   if (m<>nil) then
   begin
    pmap_zero_page_area(m, base, PAGE_SIZE - base);
    vm_page_dirty(m);
    vm_pager_page_unswapped(m);
   end;
   }
  end;

  {
   * Release any swap space and free any whole pages.
   }
  if (newpages < oldpages) then
  begin
   //swap_pager_freespace(uobj, newpages, oldpages - newpages);
   vm_object_page_remove(uobj, newpages, 0, 0);
  end;
 end;

 uobj^.size:=newpages;
 VM_OBJECT_UNLOCK(uobj);

 TMPFS_LOCK(tmp);
 tmp^.tm_pages_used:=tmp^.tm_pages_used + (newpages - oldpages);
 TMPFS_UNLOCK(tmp);

 node^.tn_size:=newsize;
 vnode_pager_setsize(vp, newsize);
 Exit(0);
end;

function tmpfs_chflags(vp:p_vnode;flags:Integer):Integer; public;
var
 error:Integer;
 node:p_tmpfs_node;
begin
 Assert(VOP_ISLOCKED(vp)<>0);

 node:=VP_TO_TMPFS_NODE(vp);

 { Disallow this operation if the file system is mounted read-only. }
 if (p_mount(vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0 then
  Exit(EROFS);

 {
  * Callers may only modify the file flags on objects they
  * have VADMIN rights for.
  }
 error:=VOP_ACCESS(vp, VADMIN);
 if (error<>0) then
  Exit(error);
 {
  * Unprivileged processes are not permitted to unset system
  * flags, or modify flags if any system flags are set.
  }
 //if (!priv_check_cred(cred, PRIV_VFS_SYSFLAGS, 0)) then
 if true then
 begin

  //if (node^.tn_flags and (SF_NOUNLINK or SF_IMMUTABLE or SF_APPEND)) then
  //begin
  // error:=securelevel_gt(cred, 0);
  // if (error) then
  //  Exit(error);
  //end;

  { Snapshot flag cannot be set or cleared }
  if (((flags and SF_SNAPSHOT)<>0) AND
      ((node^.tn_flags and SF_SNAPSHOT)=0)) OR
     (((flags and SF_SNAPSHOT)=0) AND
      ((node^.tn_flags and SF_SNAPSHOT)<>0)) then
   Exit(EPERM);

  node^.tn_flags:=flags;
 end else
 begin
  if ((node^.tn_flags and (SF_NOUNLINK or SF_IMMUTABLE or SF_APPEND))<>0) OR
     ((flags and UF_SETTABLE)<>flags) then
   Exit(EPERM);

  node^.tn_flags:=node^.tn_flags and SF_SETTABLE;
  node^.tn_flags:=node^.tn_flags or (flags and UF_SETTABLE);
 end;

 node^.tn_status:=node^.tn_status or TMPFS_NODE_CHANGED;

 Assert(VOP_ISLOCKED(vp)<>0);

 Result:=0;
end;

function tmpfs_chmod(vp:p_vnode;mode:Word):Integer; public;
var
 error:Integer;
 node:p_tmpfs_node;
begin
 Assert(VOP_ISLOCKED(vp)<>0);

 node:=VP_TO_TMPFS_NODE(vp);

 { Disallow this operation if the file system is mounted read-only. }
 if (p_mount(vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0 then
  Exit(EROFS);

 { Immutable or append-only files cannot be modified, either. }
 if (node^.tn_flags and (IMMUTABLE or APPEND))<>0 then
  Exit(EPERM);

 {
  * To modify the permissions on a file, must possess VADMIN
  * for that file.
  }
 error:=VOP_ACCESS(vp, VADMIN);
 if (error<>0) then
  Exit(error);

 {
  * Privileged processes may set the sticky bit on non-directories,
  * as well as set the setgid bit on a file with a group that the
  * process is not a member of.
  }
 if (vp^.v_type<>VDIR) AND ((mode and S_ISTXT)<>0) then
 begin
  //if (priv_check_cred(cred, PRIV_VFS_STICKYFILE, 0)) then
   Exit(EFTYPE);
 end;

 //if (!groupmember(node^.tn_gid, cred) AND (mode and S_ISGID)) then
 //begin
 // error:=priv_check_cred(cred, PRIV_VFS_SETGID, 0);
 // if (error)
 //  Exit(error);
 //end;

 node^.tn_mode:=node^.tn_mode and (not ALLPERMS);
 node^.tn_mode:=node^.tn_mode or (mode and ALLPERMS);

 node^.tn_status:=node^.tn_status or TMPFS_NODE_CHANGED;

 Assert(VOP_ISLOCKED(vp)<>0);

 Result:=0;
end;

function tmpfs_chown(vp:p_vnode;uid,gid:Integer):Integer; public;
var
 error:Integer;
 node:p_tmpfs_node;
 ouid:DWORD;
 ogid:DWORD;
begin
 Assert(VOP_ISLOCKED(vp)<>0);

 node:=VP_TO_TMPFS_NODE(vp);

 { Assign default values if they are unknown. }
 Assert((uid<>VNOVAL) OR (gid<>VNOVAL));

 if (uid=VNOVAL) then
  uid:=node^.tn_uid;

 if (gid=VNOVAL) then
  gid:=node^.tn_gid;

 Assert((uid<>VNOVAL) AND (gid<>VNOVAL));

 { Disallow this operation if the file system is mounted read-only. }
 if (p_mount(vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0 then
  Exit(EROFS);

 { Immutable or append-only files cannot be modified, either. }
 if (node^.tn_flags and (IMMUTABLE or APPEND))<>0 then
  Exit(EPERM);

 {
  * To modify the ownership of a file, must possess VADMIN for that
  * file.
  }
 error:=VOP_ACCESS(vp, VADMIN);
 if (error<>0) then
  Exit(error);

 {
  * To change the owner of a file, or change the group of a file to a
  * group of which we are not a member, the caller must have
  * privilege.
  }
 //if ((uid<>node^.tn_uid) OR
 //   ((gid<>node^.tn_gid) AND !groupmember(gid, cred))) AND
 //   (error:=priv_check_cred(cred, PRIV_VFS_CHOWN, 0)) then
 // Exit(error);

 ogid:=node^.tn_gid;
 ouid:=node^.tn_uid;

 node^.tn_uid:=uid;
 node^.tn_gid:=gid;

 node^.tn_status:=node^.tn_status or TMPFS_NODE_CHANGED;

 if ((node^.tn_mode and (S_ISUID or S_ISGID))<>0) AND ((ouid<>uid) OR (ogid<>gid)) then
 begin
  //if (priv_check_cred(cred, PRIV_VFS_RETAINSUGID, 0)) then
  // node^.tn_mode:=node^.tn_mode and (not (S_ISUID or S_ISGID));
 end;

 Assert(VOP_ISLOCKED(vp)<>0);

 Result:=0;
end;

function tmpfs_chsize(vp:p_vnode;size:Int64):Integer; public;
var
 error:Integer;
 node:p_tmpfs_node;
begin
 Assert(VOP_ISLOCKED(vp)<>0);

 node:=VP_TO_TMPFS_NODE(vp);

 { Decide whether this is a valid operation based on the file type. }
 error:=0;
 case (vp^.v_type) of
  VDIR:
   Exit(EISDIR);

  VREG:
   if (p_mount(vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0 then
    Exit(EROFS);

  VBLK,
  VCHR,
  VFIFO:
   { Allow modifications of special files even if in the file
    * system is mounted read-only (we are not modifying the
    * files themselves, but the objects they represent). }
   Exit(0);

  else
   { Anything else is unsupported. }
   Exit(EOPNOTSUPP);
 end;

 { Immutable or append-only files cannot be modified, either. }
 if (node^.tn_flags and (IMMUTABLE or APPEND))<>0 then
  Exit(EPERM);

 error:=tmpfs_truncate(vp, size);
 { tmpfs_truncate will raise the NOTE_EXTEND and NOTE_ATTRIB kevents
  * for us, as will update tn_status; no need to do that here. }

 Assert(VOP_ISLOCKED(vp)<>0);

 Result:=error;
end;

function tmpfs_chtimes(vp:p_vnode;atime,mtime,birthtime:p_timespec;vaflags:Integer):Integer; public;
var
 error:Integer;
 node:p_tmpfs_node;
begin
 Assert(VOP_ISLOCKED(vp)<>0);

 node:=VP_TO_TMPFS_NODE(vp);

 { Disallow this operation if the file system is mounted read-only. }
 if (p_mount(vp^.v_mount)^.mnt_flag and MNT_RDONLY)<>0 then
  Exit(EROFS);

 { Immutable or append-only files cannot be modified, either. }
 if (node^.tn_flags and (IMMUTABLE or APPEND))<>0 then
  Exit(EPERM);

 { Determine if the user have proper privilege to update time. }
 if (vaflags and VA_UTIMES_NULL)<>0 then
 begin
  error:=VOP_ACCESS(vp, VADMIN);
  if (error<>0) then
   error:=VOP_ACCESS(vp, VWRITE);
 end else
  error:=VOP_ACCESS(vp, VADMIN);

 if (error<>0) then
  Exit(error);

 if (atime^.tv_sec<>VNOVAL) AND (atime^.tv_nsec<>VNOVAL) then
  node^.tn_status:=node^.tn_status or TMPFS_NODE_ACCESSED;

 if (mtime^.tv_sec<>VNOVAL) AND (mtime^.tv_nsec<>VNOVAL) then
  node^.tn_status:=node^.tn_status or TMPFS_NODE_MODIFIED;

 if (birthtime^.tv_nsec<>VNOVAL) AND (birthtime^.tv_nsec<>VNOVAL) then
  node^.tn_status:=node^.tn_status or TMPFS_NODE_MODIFIED;

 tmpfs_itimes(vp, atime, mtime);

 if (birthtime^.tv_nsec<>VNOVAL) AND (birthtime^.tv_nsec<>VNOVAL) then
  node^.tn_birthtime:=birthtime^;

 Assert(VOP_ISLOCKED(vp)<>0);

 Result:=0;
end;

procedure tmpfs_itimes(vp:p_vnode;acc,_mod:p_timespec); public;
var
 node:p_tmpfs_node;
 now:timespec;
begin
 node:=VP_TO_TMPFS_NODE(vp);

 if ((node^.tn_status and (TMPFS_NODE_ACCESSED or TMPFS_NODE_MODIFIED or TMPFS_NODE_CHANGED))=0) then
  Exit;

 vfs_timestamp(@now);

 if (node^.tn_status and TMPFS_NODE_ACCESSED)<>0 then
 begin
  if (acc=nil) then
    acc:=@now;

  node^.tn_atime:=acc^;
 end;

 if (node^.tn_status and TMPFS_NODE_MODIFIED)<>0 then
 begin
  if (_mod=nil) then
   _mod:=@now;

  node^.tn_mtime:=_mod^;
 end;

 if (node^.tn_status and TMPFS_NODE_CHANGED)<>0 then
 begin
  node^.tn_ctime:=now;
 end;

 node^.tn_status:=node^.tn_status and (not (TMPFS_NODE_ACCESSED or TMPFS_NODE_MODIFIED or TMPFS_NODE_CHANGED));
end;

procedure tmpfs_update(vp:p_vnode); public;
begin
 tmpfs_itimes(vp, nil, nil);
end;

function tmpfs_truncate(vp:p_vnode;length:Int64):Integer; public;
label
 _out;
var
 error:Integer;
 node:p_tmpfs_node;
begin
 node:=VP_TO_TMPFS_NODE(vp);

 if (length < 0) then
 begin
  error:=EINVAL;
  goto _out;
 end;

 if (node^.tn_size=length) then
 begin
  error:=0;
  goto _out;
 end;

 if (length > VFS_TO_TMPFS(vp^.v_mount)^.tm_maxfilesize) then
  Exit(EFBIG);

 error:=tmpfs_reg_resize(vp, length, FALSE);

 if (error=0) then
 begin
  node^.tn_status:=node^.tn_status or (TMPFS_NODE_CHANGED or TMPFS_NODE_MODIFIED);
 end;

_out:
 tmpfs_update(vp);

 Result:=error;
end;


end.

