unit unionfs;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vnode,
 vmount,
 vnamei;

type
 { copy method of attr from lower to upper }
 unionfs_copymode=(
  UNIONFS_TRADITIONAL,
  UNIONFS_TRANSPARENT,
  UNIONFS_MASQUERADE
 );

 { whiteout policy of upper layer }
 unionfs_whitemode=(
  UNIONFS_WHITE_ALWAYS,
  UNIONFS_WHITE_WHENNEEDED
 );

 p_unionfs_mount=^unionfs_mount;
 unionfs_mount=record
  um_lowervp  :p_vnode; { VREFed once }
  um_uppervp  :p_vnode; { VREFed once }
  um_rootvp   :p_vnode; { ROOT vnode }
  um_copymode :unionfs_copymode;
  um_whitemode:unionfs_whitemode;
  um_uid      :DWORD;
  um_gid      :DWORD;
  um_udir     :WORD;
  um_ufile    :WORD;
 end;

 { unionfs status list }
 pp_unionfs_node_status=^p_unionfs_node_status;
 p_unionfs_node_status=^unionfs_node_status;
 unionfs_node_status=record
  uns_list          :LIST_ENTRY; { Status list } //unionfs_node_status
  uns_pid           :DWORD;      { current process id }
  uns_node_flag     :Integer;    { uns flag }
  uns_lower_opencnt :Integer;    { open count of lower }
  uns_upper_opencnt :Integer;    { open count of upper }
  uns_lower_openmode:Integer;    { open mode of lower }
  uns_readdir_status:Integer;    { read status of readdir }
 end;

const
 { union node status flags }
 UNS_OPENL_4_READDIR=$01; { open lower layer for readdir }

type
 p_unionfs_node_hashhead=P_LIST_HEAD; //unionfs_node

 { A cache of vnode references }
 p_unionfs_node=^unionfs_node;
 unionfs_node=record
  un_lowervp :p_vnode;    { lower side vnode }
  un_uppervp :p_vnode;    { upper side vnode }
  un_dvp     :p_vnode;    { parent unionfs vnode }
  un_vnode   :p_vnode;    { Back pointer }
  un_unshead :LIST_HEAD;  //unionfs_node_status
  { unionfs status head }
  un_hashtbl :p_unionfs_node_hashhead;
  { dir vnode hash table }
  un_hash    :LIST_ENTRY; { hash list entry }
  un_hashmask:QWORD;      { bit mask }
  un_path    :PChar;      { path }
  un_flag    :Integer;    { unionfs node flag }
 end;

const
 {
  * unionfs node flags
  * It needs the vnode with exclusive lock, when changing the un_flag variable.
  }
 UNIONFS_OPENEXTL=$01; { openextattr (lower) }
 UNIONFS_OPENEXTU=$02; { openextattr (upper) }

function  MOUNTTOUNIONFSMOUNT(mp:p_mount):p_unionfs_mount; inline;
function  VTOUNIONFS         (vp:p_vnode):p_unionfs_node; inline;
function  UNIONFSTOV         (xp:p_unionfs_node):p_vnode; inline;

function  unionfs_init  (vfsp:p_vfsconf):Integer; external;
function  unionfs_uninit(vfsp:p_vfsconf):Integer; external;
function  unionfs_nodeget(mp:p_mount;uppervp,lowervp,dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname):Integer; external;
procedure unionfs_noderem(vp:p_vnode); external;
procedure unionfs_get_node_status(unp:p_unionfs_node;unspp:pp_unionfs_node_status); external;
procedure unionfs_tryrem_node_status(unp:p_unionfs_node;unsp:p_unionfs_node_status); external;

function  unionfs_check_rmdir           (vp:p_vnode):Integer; external;
function  unionfs_copyfile              (unp:p_unionfs_node;docopy:Integer):Integer; external;
procedure unionfs_create_uppervattr_core(ump:p_unionfs_mount;lva,uva:p_vattr); external;
function  unionfs_create_uppervattr     (ump:p_unionfs_mount;lvp:p_vnode;uva:p_vattr):Integer; external;
function  unionfs_mkshadowdir           (ump:p_unionfs_mount;udvp:p_vnode;unp:p_unionfs_node;cnp:p_componentname):Integer; external;
function  unionfs_mkwhiteout            (dvp:p_vnode;cnp:p_componentname;path:PChar):Integer; external;
function  unionfs_relookup              (dvp:p_vnode;vpp:pp_vnode;cnp,cn:p_componentname;path:PChar;pathlen:Integer;nameiop:QWORD):Integer; external;
function  unionfs_relookup_for_create   (dvp:p_vnode;cnp:p_componentname):Integer; external;
function  unionfs_relookup_for_delete   (dvp:p_vnode;cnp:p_componentname):Integer; external;
function  unionfs_relookup_for_rename   (dvp:p_vnode;cnp:p_componentname):Integer; external;

function  UNIONFSVPTOLOWERVP(vp:p_vnode;fil:PChar;lno:Integer):p_vnode; external name 'unionfs_checklowervp';
function  UNIONFSVPTOUPPERVP(vp:p_vnode;fil:PChar;lno:Integer):p_vnode; external name 'unionfs_checkuppervp';

var
 unionfs_vnodeops:vop_vector; external;

implementation

function MOUNTTOUNIONFSMOUNT(mp:p_mount):p_unionfs_mount; inline;
begin
 Result:=mp^.mnt_data;
end;

function VTOUNIONFS(vp:p_vnode):p_unionfs_node; inline;
begin
 Result:=vp^.v_data;
end;

function UNIONFSTOV(xp:p_unionfs_node):p_vnode; inline;
begin
 Result:=xp^.un_vnode;
end;


end.

