unit tmpfs_fifoops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 tmpfs,
 vnode,
 vnode_if,
 sys_event,
 tmpfs_vnops,
 dead_vnops;

{$MACRO ON}
{$DEFINE fifo_specops:=dead_vnodeops} //fifo not support

function tmpfs_fifo_kqfilter(ap:p_vop_kqfilter_args):Integer;
function tmpfs_fifo_close(v:p_vop_close_args):Integer;

var
 tmpfs_fifoop_entries:vop_vector=(
  vop_default       :@fifo_specops;
  vop_bypass        :nil;
  vop_islocked      :nil;
  vop_lookup        :nil;
  vop_create        :nil;
  vop_whiteout      :nil;
  vop_mknod         :nil;
  vop_open          :nil;
  vop_close         :@tmpfs_fifo_close;
  vop_access        :@tmpfs_access;
  vop_accessx       :nil;
  vop_getattr       :@tmpfs_getattr;
  vop_setattr       :@tmpfs_setattr;
  vop_markatime     :nil;
  vop_read          :nil;
  vop_write         :nil;
  vop_ioctl         :nil;
  vop_poll          :nil;
  vop_kqfilter      :@tmpfs_fifo_kqfilter;
  vop_revoke        :nil;
  vop_fsync         :nil;
  vop_remove        :nil;
  vop_link          :nil;
  vop_rename        :nil;
  vop_mkdir         :nil;
  vop_rmdir         :nil;
  vop_symlink       :nil;
  vop_readdir       :nil;
  vop_readlink      :nil;
  vop_inactive      :nil;
  vop_reclaim       :@tmpfs_reclaim;
  vop_lock1         :nil;
  vop_unlock        :nil;
  vop_bmap          :nil;
  vop_strategy      :nil;
  vop_getwritemount :nil;
  vop_print         :nil;
  vop_pathconf      :nil;
  vop_advlock       :nil;
  vop_advlockasync  :nil;
  vop_advlockpurge  :nil;
  vop_reallocblks   :nil;
  vop_getpages      :nil;
  vop_putpages      :nil;
  vop_vptofh        :nil;
  vop_vptocnp       :nil;
  vop_allocate      :nil;
  vop_unp_bind      :nil;
  vop_unp_connect   :nil;
  vop_unp_detach    :nil;
 ); public;

implementation

function tmpfs_fifo_kqfilter(ap:p_vop_kqfilter_args):Integer;
var
 vp:p_vnode;
 node:p_tmpfs_node;
begin
 vp:=ap^.a_vp;
 node:=VP_TO_TMPFS_NODE(vp);

 case (ap^.a_kn^.kn_filter) of
  EVFILT_READ :node^.tn_status:=node^.tn_status or TMPFS_NODE_ACCESSED;
  EVFILT_WRITE:node^.tn_status:=node^.tn_status or TMPFS_NODE_MODIFIED;
 end;

 Result:=vop_kqfilter_t(fifo_specops.vop_kqfilter)(ap);
end;

function tmpfs_fifo_close(v:p_vop_close_args):Integer;
var
 node:p_tmpfs_node;
begin
 node:=VP_TO_TMPFS_NODE(v^.a_vp);
 node^.tn_status:=node^.tn_status or TMPFS_NODE_ACCESSED;

 tmpfs_update(v^.a_vp);

 Result:=vop_close_t(fifo_specops.vop_close)(v);
end;


end.

