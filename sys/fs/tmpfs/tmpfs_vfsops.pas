unit tmpfs_vfsops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_malloc,
 sysutils,
 errno,
 mqueue,
 vnode,
 vmount,
 vmparam,
 kern_param,
 vstat,
 vfs_subr,
 vfs_vnops,
 vfs_mount,
 vnode_if,
 subr_unit,
 uma,
 vm,
 kern_mtx,
 tmpfs;

function tmpfs_mount  (mp:p_mount):Integer;
function tmpfs_unmount(mp:p_mount;mntflags:Integer):Integer;
function tmpfs_root   (mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
function tmpfs_fhtovp (mp:p_mount;fhp:p_fid;flags:Integer;vpp:pp_vnode):Integer;
function tmpfs_statfs (mp:p_mount;sbp:p_statfs):Integer;

const
 _tmpfs_vfsops:vfsops=(
  vfs_mount     :@tmpfs_mount;
  vfs_cmount    :nil;
  vfs_unmount   :@tmpfs_unmount;
  vfs_root      :@tmpfs_root;
  vfs_quotactl  :nil;
  vfs_statfs    :@tmpfs_statfs;
  vfs_sync      :nil;
  vfs_vget      :nil;
  vfs_fhtovp    :@tmpfs_fhtovp;
  vfs_checkexp  :nil;
  vfs_init      :nil;
  vfs_uninit    :nil;
  vfs_extattrctl:nil;
  vfs_sysctl    :nil;
  vfs_susp_clean:nil;
 );

var
 //VFS_SET(tmpfs_vfsops, tmpfs, 0);
 tmpfs_vfsconf:vfsconf=(
  vfc_version :VFS_VERSION;
  vfc_name    :'tmpfs';
  vfc_vfsops  :@_tmpfs_vfsops;
  vfc_typenum :-1;
  vfc_refcount:0;
  vfc_flags   :0;
  vfc_opts    :nil;
  vfc_list    :(tqe_next:nil;tqe_prev:nil)
 );

implementation

uses
 tmpfs_fifoops,
 tmpfs_subr,
 tmpfs_vnops,
 md_arc4random;

const
 TMPFS_DEFAULT_ROOT_MODE=(S_IRWXU or S_IRGRP or S_IXGRP or S_IROTH or S_IXOTH);

const
 tmpfs_opts:array[0..8] of PChar=(
  'from', 'size', 'maxfilesize', 'inodes', 'uid', 'gid', 'mode', 'export', nil
 );

 tmpfs_updateopts:array[0..2] of PChar=(
  'from', 'export', nil
 );

function strtoq(const nptr: PChar; endptr: PPChar; base: Integer): Int64;
var
  p: PChar;
  neg: Boolean;
  val: QWord;
  overflow: Boolean;
  digit: Integer;
  any: Boolean;
  c: Char;
begin
 //strtoq_errno := STRTOQ_OK;
 overflow := False;
 any := False;
 val := 0;

 p := nptr;
 while (p^ <> #0) and (p^ in [' ', #9, #10, #13, #11, #12]) do
   Inc(p);

 neg := False;
 if (p^ = '-') then
 begin
  neg := True;
  Inc(p);
 end else
 if (p^ = '+') then
   Inc(p);

 if (base = 0) then
 begin
  if p^ = '0' then
  begin
   Inc(p);
   if (p^ = 'x') or (p^ = 'X') then
   begin
    Inc(p);
    base := 16;
   end else
    base := 8;
  end else
   base := 10;
 end;

 if (base < 2) or (base > 36) then
 begin
  //strtoq_errno := STRTOQ_EINVAL;
  if (endptr <> nil) then
    endptr^ := nptr;
  Exit(0);
 end;

 while True do
 begin
   c := p^;
   if (c >= '0') and (c <= '9') then
     digit := Ord(c) - Ord('0')
   else if (c >= 'A') and (c <= 'Z') then
     digit := Ord(c) - Ord('A') + 10
   else if (c >= 'a') and (c <= 'z') then
     digit := Ord(c) - Ord('a') + 10
   else
     Break;

   if (digit >= base) then
     Break;

   if (val > (High(QWord) div QWord(base))) or
      ((val = (High(QWord) div QWord(base))) and (QWord(digit) > (High(QWord) mod QWord(base)))) then
   begin
    overflow := True;
   end else
    val := val * QWord(base) + QWord(digit);

   any := True;
   Inc(p);
 end;

 if not any then
 begin
   if (endptr <> nil) then
     endptr^ := nptr;
   Exit(0);
 end;

 if (endptr <> nil) then
   endptr^ := p;

 if overflow then
 begin
  //strtoq_errno := STRTOQ_ERANGE;
  if neg then
    Result := Low(Int64)
  else
    Result := High(Int64);

  Exit;
 end;

 if neg then
 begin
   if val > (QWord(High(Int64)) + 1) then
   begin
    //strtoq_errno := STRTOQ_ERANGE;
    Result := Low(Int64);
   end else
   if val = (QWord(High(Int64)) + 1) then
     Result := Low(Int64)
   else
     Result := -Int64(val);
 end else
 begin
   if val > High(Int64) then
   begin
    //strtoq_errno := STRTOQ_ERANGE;
    Result := High(Int64);
   end else
     Result := Int64(val);
 end;
end;

function tmpfs_getopt_size(opts:p_vfsoptlist;name:PChar;value:PInt64):Integer;
var
 opt_value,vtp:PChar;
 iv:Int64;
 error,opt_len:Integer;
begin
 error:=vfs_getopt(opts, name, @opt_value, @opt_len);

 if (error<>0) then
  Exit(error);

 if (opt_len=0) OR (opt_value=nil) then
  Exit(EINVAL);

 if (opt_value[0]=#0) OR (opt_value[opt_len - 1]<>#0) then
  Exit(EINVAL);

 iv:=strtoq(opt_value, @vtp, 0);

 if (vtp=opt_value) OR ((vtp[0]<>#0) AND (vtp[1]<>#0)) then
  Exit(EINVAL);

 if (iv < 0) then
  Exit(EINVAL);

 case (vtp[0]) of
  't',
  'T':
   iv:=iv shl 40; //1024^4
  'g',
  'G':
   iv:=iv shl 30; //1024^3
  'm',
  'M':
   iv:=iv shl 20; //1024^2
  'k',
  'K':
   iv:=iv shl 10; //1024
  #0:;
  else
   Exit(EINVAL);
 end;

 value^:=iv;

 Exit(0);
end;

function tmpfs_node_ctor(mem:Pointer;size:Integer;arg:Pointer;flags:Integer):Integer;
var
 node:p_tmpfs_node;
begin
 node:=mem;

 Inc(node^.tn_gen);
 node^.tn_size   :=0;
 node^.tn_status :=0;
 node^.tn_flags  :=0;
 node^.tn_links  :=0;
 node^.tn_vnode  :=nil;
 node^.tn_vpstate:=0;

 Exit(0);
end;

procedure tmpfs_node_dtor(mem:Pointer;size:Integer;arg:Pointer);
var
 node:p_tmpfs_node;
begin
 node:=mem;
 node^.tn_type:=VNON;
end;

function tmpfs_node_init(mem:Pointer;size,flags:Integer):Integer;
var
 node:p_tmpfs_node;
begin
 node:=mem;
 node^.tn_id:=0;

 mtx_init(node^.tn_interlock, 'tmpfs node interlock');
 node^.tn_gen:=arc4random();

 Exit(0);
end;

procedure tmpfs_node_fini(mem:Pointer;size:Integer);
var
 node:p_tmpfs_node;
begin
 node:=mem;
 mtx_destroy(node^.tn_interlock);
end;

function howmany(x,y:QWORD):QWORD; inline;
begin
 Result:=(x+(y-1)) div y;
end;

function tmpfs_mount(mp:p_mount):Integer;
const
 node_size     =sizeof(tmpfs_dirent) + sizeof(tmpfs_node);
 nodes_per_page=(PAGE_SIZE+(node_size-1)) div node_size;
var
 tmp:p_tmpfs_mount;
 root:p_tmpfs_node;
 error:Integer;
 pages:Int64;
 nodes_max, size_max, maxfilesize:Int64;
 root_uid,root_gid:DWORD;
 root_mode:WORD;
 va:t_vattr;
begin
 if (vfs_filteropt(mp^.mnt_optnew, tmpfs_opts))<>0 then
 begin
  Exit(EINVAL);
 end;

 if (mp^.mnt_flag and MNT_UPDATE)<>0 then
 begin
  { Only support update mounts for certain options. }
  if (vfs_filteropt(mp^.mnt_optnew, tmpfs_updateopts)<>0) then
  begin
   Exit(EOPNOTSUPP);
  end;

  if (vfs_flagopt(mp^.mnt_optnew, 'ro', nil, 0)<>p_tmpfs_mount(mp^.mnt_data)^.tm_ronly) then
  begin
   Exit(EOPNOTSUPP);
  end;

  Exit(0);
 end;

 vn_lock(mp^.mnt_vnodecovered, LK_SHARED or LK_RETRY,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%});

 error:=VOP_GETATTR(mp^.mnt_vnodecovered, @va);
 VOP_UNLOCK(mp^.mnt_vnodecovered, 0);

 if (error<>0) then Exit(error);

 root_gid:=0;
 if //(mp^.mnt_cred^.cr_ruid<>0) OR
    (vfs_scanopt(mp^.mnt_optnew, 'gid', '%d', [root_gid])<>1) then
  root_gid:=va.va_gid;

 root_uid:=0;
 if //(mp^.mnt_cred^.cr_ruid<>0) OR
    (vfs_scanopt(mp^.mnt_optnew, 'uid', '%d', [root_uid])<>1) then
  root_uid:=va.va_uid;

 root_mode:=0;
 if //(mp^.mnt_cred^.cr_ruid<>0) OR
    (vfs_scanopt(mp^.mnt_optnew, 'mode', '%ho', [root_mode])<>1) then
  root_mode:=va.va_mode;

 if (tmpfs_getopt_size(mp^.mnt_optnew, 'inodes', @nodes_max)<>0) then
  nodes_max:=0;

 if (tmpfs_getopt_size(mp^.mnt_optnew, 'size', @size_max)<>0) then
  size_max:=0;

 if (tmpfs_getopt_size(mp^.mnt_optnew, 'maxfilesize', @maxfilesize)<>0) then
  maxfilesize:=0;

 { Do not allow mounts if we do not have enough memory to preserve
  * the minimum reserved pages. }
 if (tmpfs_mem_avail() < TMPFS_PAGES_MINRESERVED) then
 begin
  Exit(ENOSPC);
 end;

 { Get the maximum number of memory pages this file system is
  * allowed to use, based on the maximum size the user passed in
  * the mount structure.  A value of zero is treated as if the
  * maximum available space was requested. }
 if (size_max=0) OR (size_max > (High(Int64) - PAGE_SIZE)) then
 begin
  pages:=High(Int64);
 end else
 begin
  size_max:=roundup(size_max, PAGE_SIZE);
  pages:=howmany(size_max, PAGE_SIZE);
 end;

 Assert(pages > 0);

 if (nodes_max <= 3) then
 begin
  if (pages < High(Integer) div nodes_per_page) then
   nodes_max:=pages * nodes_per_page
  else
   nodes_max:=High(Integer);
 end;

 if (nodes_max > High(Integer)) then
  nodes_max:=High(Integer);

 Assert(nodes_max >= 3);

 { Allocate the tmpfs mount structure and fill it. }
 tmp:=calloc(sizeof(p_tmpfs_mount^));

 mtx_init(tmp^.allnode_lock, 'tmpfs allnode lock');
 tmp^.tm_nodes_max:=nodes_max;
 tmp^.tm_nodes_inuse:=0;

 if (maxfilesize > 0) then
  tmp^.tm_maxfilesize:=maxfilesize
 else
  tmp^.tm_maxfilesize:=High(Int64);

 LIST_INIT(@tmp^.tm_nodes_used);

 tmp^.tm_pages_max  :=pages;
 tmp^.tm_pages_used :=0;
 tmp^.tm_ino_unr    :=new_unrhdr(2, High(Integer), @tmp^.allnode_lock);
 tmp^.tm_dirent_pool:=uma_zcreate('TMPFS dirent',
     sizeof(tmpfs_dirent),
     nil, nil, nil, nil,
     UMA_ALIGN_PTR, 0);

 tmp^.tm_node_pool:=uma_zcreate('TMPFS node',
     sizeof(tmpfs_node),
     @tmpfs_node_ctor, @tmpfs_node_dtor,
     @tmpfs_node_init, @tmpfs_node_fini,
     UMA_ALIGN_PTR, 0);

 tmp^.tm_ronly:=ord((mp^.mnt_flag and MNT_RDONLY)<>0);

 { Allocate the root node. }
 error:=tmpfs_alloc_node(tmp, VDIR, root_uid,
     root_gid, root_mode and ALLPERMS, nil, nil,
     VNOVAL, @root);

 if (error<>0) OR (root=nil) then
 begin
  uma_zdestroy(tmp^.tm_node_pool);
  uma_zdestroy(tmp^.tm_dirent_pool);
  delete_unrhdr(tmp^.tm_ino_unr);
  free(tmp);
  Exit(error);
 end;

 Assert(root^.tn_id=2, 'tmpfs root with invalid ino: ' + IntToStr(root^.tn_id));
 tmp^.tm_root:=root;

 MNT_ILOCK(mp);
 mp^.mnt_flag:=mp^.mnt_flag or MNT_LOCAL;
 mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_MPSAFE;
 MNT_IUNLOCK(mp);

 mp^.mnt_data:=tmp;
 mp^.mnt_stat.f_namemax:=MAXNAMLEN;
 vfs_getnewfsid(mp);
 vfs_mountedfrom(mp, 'tmpfs');

 Result:=0;
end;

function tmpfs_unmount(mp:p_mount;mntflags:Integer):Integer;
var
 error:Integer;
 flags:Integer;
 tmp:p_tmpfs_mount;
 node:p_tmpfs_node;
 next:p_tmpfs_node;
 nde:p_tmpfs_dirent;
 de:p_tmpfs_dirent;
begin
 flags:=0;

 { Handle forced unmounts. }
 if (mntflags and MNT_FORCE)<>0 then
  flags:=flags or FORCECLOSE;

 { Finalize all pending I/O. }
 error:=vflush(mp, 0, flags);
 if (error<>0) then
  Exit(error);

 tmp:=VFS_TO_TMPFS(mp);

 { Free all associated data.  The loop iterates over the linked list
  * we have containing all used nodes.  For each of them that is
  * a directory, we free all its directory entries.  Note that after
  * freeing a node, it will automatically go to the available list,
  * so we will later have to iterate over it to release its items. }
 node:=LIST_FIRST(@tmp^.tm_nodes_used);
 while (node<>nil) do
 begin

  if (node^.tn_type=VDIR) then
  begin
   de:=TAILQ_FIRST(@node^.tn_spec.tn_dir.tn_dirhead);
   while (de<>nil) do
   begin
    nde:=TAILQ_NEXT(de, @de^.td_entries);
    tmpfs_free_dirent(tmp, de, FALSE);
    de:=nde;
    node^.tn_size:=node^.tn_size - sizeof(tmpfs_dirent);
   end;
  end;

  next:=LIST_NEXT(node, @node^.tn_entries);
  tmpfs_free_node(tmp, node);
  node:=next;
 end;

 uma_zdestroy(tmp^.tm_dirent_pool);
 uma_zdestroy(tmp^.tm_node_pool);
 delete_unrhdr(tmp^.tm_ino_unr);

 mtx_destroy(tmp^.allnode_lock);
 Assert(tmp^.tm_pages_used=0);
 Assert(tmp^.tm_nodes_inuse=0);

 { Throw away the tmpfs_mount structure. }
 free(mp^.mnt_data);
 mp^.mnt_data:=nil;

 MNT_ILOCK(mp);
 mp^.mnt_flag:=mp^.mnt_flag and (not MNT_LOCAL);
 MNT_IUNLOCK(mp);

 Result:=0;
end;

function tmpfs_root(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
var
 error:Integer;
begin
 error:=tmpfs_alloc_vp(mp, VFS_TO_TMPFS(mp)^.tm_root, flags, vpp);

 if (error=0) then
  (vpp^)^.v_vflag:=(vpp^)^.v_vflag or VV_ROOT;

 Result:=error;
end;

function tmpfs_fhtovp(mp:p_mount;fhp:p_fid;flags:Integer;vpp:pp_vnode):Integer;
var
 tfhp:p_tmpfs_fid;
 tmp:p_tmpfs_mount;
 node:p_tmpfs_node;
begin
 tmp:=VFS_TO_TMPFS(mp);

 tfhp:=p_tmpfs_fid(fhp);

 if (tfhp^.tf_len<>sizeof(tmpfs_fid)) then
  Exit(EINVAL);

 if (tfhp^.tf_id >= tmp^.tm_nodes_max) then
  Exit(EINVAL);

 TMPFS_LOCK(tmp);

 node:=LIST_FIRST(@tmp^.tm_nodes_used);
 while (node<>nil) do
 begin
  if (node^.tn_id=tfhp^.tf_id) AND
     (node^.tn_gen=tfhp^.tf_gen) then
  begin
   break;
  end;
  //
  node:=LIST_NEXT(node,@node^.tn_entries);
 end;

 TMPFS_UNLOCK(tmp);

 if (node<>nil) then
  Exit(tmpfs_alloc_vp(mp, node, LK_EXCLUSIVE, vpp));

 Exit(EINVAL);
end;

function tmpfs_statfs(mp:p_mount;sbp:p_statfs):Integer;
var
 tmp:p_tmpfs_mount;
 used:QWORD;
begin
 tmp:=VFS_TO_TMPFS(mp);

 sbp^.f_iosize:=PAGE_SIZE;
 sbp^.f_bsize :=PAGE_SIZE;

 used:=tmpfs_pages_used(tmp);

 if (tmp^.tm_pages_max<>High(QWORD)) then
   sbp^.f_blocks:=tmp^.tm_pages_max
 else
   sbp^.f_blocks:=used + tmpfs_mem_avail();

 if (sbp^.f_blocks <= used) then
  sbp^.f_bavail:=0
 else
  sbp^.f_bavail:=sbp^.f_blocks - used;

 sbp^.f_bfree:=sbp^.f_bavail;
 used:=tmp^.tm_nodes_inuse;
 sbp^.f_files:=tmp^.tm_nodes_max;

 if (sbp^.f_files <= used) then
  sbp^.f_ffree:=0
 else
  sbp^.f_ffree:=sbp^.f_files - used;

 { sbp^.f_owner:=tmp^.tn_uid; }

 Result:=0;
end;

end.



