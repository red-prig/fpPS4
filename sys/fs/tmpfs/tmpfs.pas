unit tmpfs;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vmount,
 vnode,
 vnamei,
 kern_mtx,
 time,
 vm_object,
 vuio,
 vfile,
 vfs_subr,
 vnode_if,
 uma,
 vmparam,
 subr_unit;

const
 TMPFS_DIRCOOKIE_DOT   =0;
 TMPFS_DIRCOOKIE_DOTDOT=1;
 TMPFS_DIRCOOKIE_EOF   =2;

 TMPFS_NODE_ACCESSED=(1 shl 1);
 TMPFS_NODE_MODIFIED=(1 shl 2);
 TMPFS_NODE_CHANGED =(1 shl 3);

type
 pp_tmpfs_node=^p_tmpfs_node;
 p_tmpfs_node=^tmpfs_node;

 pp_tmpfs_dirent=^p_tmpfs_dirent;
 p_tmpfs_dirent=^tmpfs_dirent;
 tmpfs_dirent=record
  td_entries:TAILQ_ENTRY; //tmpfs_dirent
  td_namelen:Word;
  td_name   :PChar;
  td_node   :p_tmpfs_node;
 end;

 tmpfs_dir=TAILQ_HEAD; //tmpfs_dirent

 tmpfs_node=packed object
  tn_entries:LIST_ENTRY; //tmpfs_node
  tn_type     :vtype;
  tn_id       :DWORD;
  tn_status   :Integer;
  tn_size     :QWORD;
  tn_uid      :DWORD;
  tn_gid      :DWORD;
  tn_mode     :Word;
  tn_links    :Word;
  tn_flags    :Integer;
  tn_atime    :timespec;
  tn_mtime    :timespec;
  tn_ctime    :timespec;
  tn_birthtime:timespec;
  tn_gen      :Int64;
  tn_vnode    :p_vnode;
  tn_interlock:mtx;
  tn_vpstate  :Integer;

  tn_spec:record
   Case Byte of
    0:(tn_rdev:DWORD);
    1:(tn_dir:record
        tn_parent       :p_tmpfs_node;
        tn_dirhead      :tmpfs_dir;
        tn_readdir_lastn:QWORD;
        tn_readdir_lastp:p_tmpfs_dirent;
       end);
    2:(tn_link:PChar);
    3:(tn_reg:record
        tn_aobj:vm_object_t;
       end);
    4:(tn_fifo:record
        tn_fo_read :fo_rdwr_t;
        tn_fo_write:fo_rdwr_t;
       end);
  end;

  property tn_rdev         :DWORD          read tn_spec.tn_rdev                 write tn_spec.tn_rdev;
  property tn_parent       :p_tmpfs_node   read tn_spec.tn_dir.tn_parent        write tn_spec.tn_dir.tn_parent;
  property tn_readdir_lastn:QWORD          read tn_spec.tn_dir.tn_readdir_lastn write tn_spec.tn_dir.tn_readdir_lastn;
  property tn_readdir_lastp:p_tmpfs_dirent read tn_spec.tn_dir.tn_readdir_lastp write tn_spec.tn_dir.tn_readdir_lastp;
  property tn_link         :PChar          read tn_spec.tn_link                 write tn_spec.tn_link;
  property tn_aobj         :vm_object_t    read tn_spec.tn_reg.tn_aobj          write tn_spec.tn_reg.tn_aobj;
  //property tn_fifo: tn_spec.tn_fifo
 end;

 tmpfs_node_list=LIST_HEAD; //tmpfs_node

procedure TMPFS_NODE_LOCK  (node:p_tmpfs_node); inline;
procedure TMPFS_NODE_UNLOCK(node:p_tmpfs_node); inline;
function  TMPFS_NODE_MTX   (node:p_tmpfs_node):p_mtx; inline;

procedure TMPFS_ASSERT_LOCKED (node:p_tmpfs_node); inline;
procedure TMPFS_ASSERT_ELOCKED(node:p_tmpfs_node); inline;

function  tmpfs_dircookie(de:p_tmpfs_dirent):QWORD; inline;

const
 TMPFS_VNODE_ALLOCATING=1;
 TMPFS_VNODE_WANT      =2;
 TMPFS_VNODE_DOOMED    =4;
 TMPFS_VNODE_WRECLAIM  =8;

type
 p_tmpfs_mount=^tmpfs_mount;
 tmpfs_mount=packed record
  tm_pages_max  :QWORD;
  tm_pages_used :QWORD;
  tm_root       :p_tmpfs_node;
  tm_nodes_max  :DWORD;
  tm_nodes_inuse:DWORD;
  tm_ino_unr    :p_unrhdr;
  tm_maxfilesize:QWORD;
  tm_nodes_used :tmpfs_node_list;
  allnode_lock  :mtx;
  tm_dirent_pool:uma_zone_t;
  tm_node_pool  :uma_zone_t;
  tm_ronly      :Integer;
 end;

procedure TMPFS_LOCK  (tm:p_tmpfs_mount); inline;
procedure TMPFS_UNLOCK(tm:p_tmpfs_mount); inline;

function  IMPLIES(a,b:Boolean):Boolean; inline;
function  IFF    (a,b:Boolean):Boolean; inline;

function  TMPFS_DIRENT_MATCHES(de:p_tmpfs_dirent;name:PChar;len:Word):Boolean; inline;
procedure TMPFS_VALIDATE_DIR(node:p_tmpfs_node); inline;

type
 p_tmpfs_fid=^tmpfs_fid;
 tmpfs_fid=packed record
  tf_len:Word;
  tf_pad:Word;
  tf_id :DWORD;
  tf_gen:QWORD;
 end;

function  tmpfs_alloc_node(tmp:p_tmpfs_mount;_type:vtype;uid,gid,mode:DWORD;parent:p_tmpfs_node;target:PChar;rdev:Integer;node:pp_tmpfs_node):Integer; external;
procedure tmpfs_free_node(tmp:p_tmpfs_mount;node:p_tmpfs_node); external;
function  tmpfs_alloc_dirent(tmp:p_tmpfs_mount;node:p_tmpfs_node;name:PChar;len:Word;de:pp_tmpfs_dirent):Integer; external;
procedure tmpfs_free_dirent(tmp:p_tmpfs_mount;de:p_tmpfs_dirent;node_exists:Boolean); external;
function  tmpfs_alloc_vp(mp:p_mount;node:p_tmpfs_node;lkflag:Integer;vpp:pp_vnode):Integer; external;
procedure tmpfs_free_vp(vp:p_vnode); external;
function  tmpfs_alloc_file(dvp:p_vnode;vpp:pp_vnode;vap:p_vattr;cnp:p_componentname;target:pchar):Integer; external;
procedure tmpfs_dir_attach(vp:p_vnode;de:p_tmpfs_dirent); external;
procedure tmpfs_dir_detach(vp:p_vnode;de:p_tmpfs_dirent); external;
function  tmpfs_dir_lookup(node:p_tmpfs_node;f:p_tmpfs_node;cnp:p_componentname):p_tmpfs_dirent; external;
function  tmpfs_dir_getdotdent(node:p_tmpfs_node;uio:p_uio):Integer; external;
function  tmpfs_dir_getdotdotdent(node:p_tmpfs_node;uio:p_uio):Integer; external;
function  tmpfs_dir_lookupbycookie(node:p_tmpfs_node;cookie:QWORD):p_tmpfs_dirent; external;
function  tmpfs_dir_getdents(node:p_tmpfs_node;uio:p_uio;cntp:PQWORD):Integer; external;
function  tmpfs_dir_whiteout_add(dvp:p_vnode;cnp:p_componentname):Integer; external;
procedure tmpfs_dir_whiteout_remove(dvp:p_vnode;cnp:p_componentname); external;
function  tmpfs_reg_resize(vp:p_vnode;newsize:QWORD;ignerr:Boolean):Integer; external;
function  tmpfs_chflags(vp:p_vnode;flags:Integer):Integer; external;
function  tmpfs_chmod(vp:p_vnode;mode:Word):Integer; external;
function  tmpfs_chown(vp:p_vnode;uid,gid:Integer):Integer; external;
function  tmpfs_chsize(vp:p_vnode;size:Int64):Integer; external;
function  tmpfs_chtimes(vp:p_vnode;atime,mtime,birthtime:p_timespec;vaflags:Integer):Integer; external;
procedure tmpfs_itimes(vp:p_vnode;acc,_mod:p_timespec); external;
//
procedure tmpfs_update(vp:p_vnode); external;
function  tmpfs_truncate(vp:p_vnode;length:Int64):Integer; external;

const
 TMPFS_PAGES_MINRESERVED=(4 * 1024 * 1024 / PAGE_SIZE);

function tmpfs_mem_avail():QWORD; external;
function tmpfs_pages_used(tmp:p_tmpfs_mount):QWORD; external;

function VFS_TO_TMPFS    (mp:p_mount):p_tmpfs_mount; inline;
function VP_TO_TMPFS_NODE(vp:p_vnode):p_tmpfs_node; inline;
function VP_TO_TMPFS_DIR (vp:p_vnode):p_tmpfs_node; inline;

var
 tmpfs_vnodeop_entries:vop_vector; external;
 tmpfs_fifoop_entries :vop_vector; external;

implementation

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

procedure TMPFS_NODE_LOCK(node:p_tmpfs_node); inline;
begin
 mtx_lock(node^.tn_interlock);
end;

procedure TMPFS_NODE_UNLOCK(node:p_tmpfs_node); inline;
begin
 mtx_unlock(node^.tn_interlock);
end;

function TMPFS_NODE_MTX(node:p_tmpfs_node):p_mtx; inline;
begin
 Result:=@node^.tn_interlock;
end;

procedure TMPFS_ASSERT_LOCKED(node:p_tmpfs_node); inline;
begin
 Assert(node<>nil);
 Assert(node^.tn_vnode<>nil);

 if (VOP_ISLOCKED(node^.tn_vnode)=0) and
    (not mtx_owned(node^.tn_interlock)) then
 begin
  LOG_CRITICAL('tmpfs: node is not locked: ',HexStr(node));
  Assert(false,'tmpfs: node is not locked: ');
 end;

end;

procedure TMPFS_ASSERT_ELOCKED(node:p_tmpfs_node); inline;
begin
 Assert(node<>nil);
 Assert(node^.tn_vnode<>nil);

 mtx_assert(node^.tn_interlock);
 ASSERT_VOP_LOCKED(node^.tn_vnode, 'tmpfs');
end;

function tmpfs_dircookie(de:p_tmpfs_dirent):QWORD; inline;
begin
 Result:=(QWORD(de) shr 1) and $7FFFFFFF;
 Assert(Result<>TMPFS_DIRCOOKIE_DOT);
 Assert(Result<>TMPFS_DIRCOOKIE_DOTDOT);
 Assert(Result<>TMPFS_DIRCOOKIE_EOF);
end;

procedure TMPFS_LOCK(tm:p_tmpfs_mount); inline;
begin
 mtx_lock(tm^.allnode_lock);
end;

procedure TMPFS_UNLOCK(tm:p_tmpfs_mount); inline;
begin
 mtx_unlock(tm^.allnode_lock);
end;

function IMPLIES(a,b:Boolean):Boolean; inline;
begin
 Result:=(not a) or b;
end;

function IFF(a,b:Boolean):Boolean; inline;
begin
 Result:=IMPLIES(a, b) and IMPLIES(b, a);
end;

function TMPFS_DIRENT_MATCHES(de:p_tmpfs_dirent;name:PChar;len:Word):Boolean; inline;
begin
 Result:=(de^.td_namelen=len) and
         (CompareByte(de^.td_name^,name^,de^.td_namelen)=0);
end;

procedure TMPFS_VALIDATE_DIR(node:p_tmpfs_node); inline;
begin
 Assert(node^.tn_type=VDIR);
 Assert((node^.tn_size mod sizeof(tmpfs_dirent)) = 0);
 Assert((node^.tn_readdir_lastp = nil) or
        (tmpfs_dircookie(node^.tn_readdir_lastp)=node^.tn_readdir_lastn)
       );
end;

function VFS_TO_TMPFS(mp:p_mount):p_tmpfs_mount; inline;
begin
 Assert(mp<>nil);
 Assert(mp^.mnt_data<>nil);

 Result:=mp^.mnt_data;
end;

function VP_TO_TMPFS_NODE(vp:p_vnode):p_tmpfs_node; inline;
begin
 Assert(vp<>nil);
 Assert(vp^.v_data<>nil);

 Result:=vp^.v_data;
end;

function VP_TO_TMPFS_DIR(vp:p_vnode):p_tmpfs_node; inline;
begin
 Result:=VP_TO_TMPFS_NODE(vp);
 TMPFS_VALIDATE_DIR(Result);
end;

end.




