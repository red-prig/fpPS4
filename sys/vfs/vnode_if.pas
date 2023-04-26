unit vnode_if;

interface

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

uses
 vfs_vnode,
 vnamei,
 vfile,
 vuio,
 vmount,
 vfcntl,
 vsocketvar;

type
 PPPtrUint     =^PPtrUint;
 pp_bufobj     =Pointer;
 daddr_t       =PtrUint;
 p_daddr_t     =PPtrUint;
 p_buf         =Pointer;
 p_task        =Pointer;
 p_cluster_save=Pointer;
 p_vm_page_t   =Pointer;
 acl_type_t    =Integer;
 p_acl         =Pointer;
 p_label       =Pointer;

 p_vop_islocked_args=^vop_islocked_args;
 vop_islocked_args=record
  a_vp:p_vnode;
 end;

 p_vop_lookup_args=^vop_lookup_args;
 vop_lookup_args=record
  a_dvp:p_vnode;
  a_vpp:pp_vnode;
  a_cnp:p_componentname;
 end;

 p_vop_cachedlookup_args=^vop_cachedlookup_args;
 vop_cachedlookup_args=record
  a_dvp:p_vnode;
  a_vpp:pp_vnode;
  a_cnp:p_componentname;
 end;

 p_vop_create_args=^vop_create_args;
 vop_create_args=record
  a_dvp:p_vnode;
  a_vpp:pp_vnode;
  a_cnp:p_componentname;
  a_vap:p_vattr;
 end;

 p_vop_whiteout_args=^vop_whiteout_args;
 vop_whiteout_args=record
  a_dvp  :p_vnode;
  a_cnp  :p_componentname;
  a_flags:Integer;
 end;

 p_vop_mknod_args=^vop_mknod_args;
 vop_mknod_args=record
  a_dvp:p_vnode;
  a_vpp:pp_vnode;
  a_cnp:p_componentname;
  a_vap:p_vattr;
 end;

 p_vop_open_args=^vop_open_args;
 vop_open_args=record
  a_vp  :p_vnode;
  a_mode:Integer;
  a_fp  :p_file;
 end;

 p_vop_close_args=^vop_close_args;
 vop_close_args=record
  a_vp   :p_vnode;
  a_fflag:Integer;
 end;

 p_vop_access_args=^vop_access_args;
 vop_access_args=record
  a_vp     :p_vnode;
  a_accmode:accmode_t;
 end;

 p_vop_accessx_args=^vop_accessx_args;
 vop_accessx_args=record
  a_vp     :p_vnode;
  a_accmode:accmode_t;
 end;

 p_vop_getattr_args=^vop_getattr_args;
 vop_getattr_args=record
  a_vp :p_vnode;
  a_vap:p_vattr;
 end;

 p_vop_setattr_args=^vop_setattr_args;
 vop_setattr_args=record
  a_vp :p_vnode;
  a_vap:p_vattr;
 end;

 p_vop_markatime_args=^vop_markatime_args;
 vop_markatime_args=record
  a_vp:p_vnode;
 end;

 p_vop_read_args=^vop_read_args;
 vop_read_args=record
  a_vp    :p_vnode;
  a_uio   :p_uio;
  a_ioflag:Integer;
 end;

 p_vop_write_args=^vop_write_args;
 vop_write_args=record
  a_vp    :p_vnode;
  a_uio   :p_uio;
  a_ioflag:Integer;
 end;

 p_vop_ioctl_args=^vop_ioctl_args;
 vop_ioctl_args=record
  a_vp     :p_vnode;
  a_command:PtrUint;
  a_data   :Pointer;
  a_fflag  :Integer;
 end;

 p_vop_poll_args=^vop_poll_args;
 vop_poll_args=record
  a_vp    :p_vnode;
  a_events:Integer;
 end;

 p_vop_kqfilter_args=^vop_kqfilter_args;
 vop_kqfilter_args=record
  a_vp:p_vnode;
  a_kn:p_knote;
 end;

 p_vop_revoke_args=^vop_revoke_args;
 vop_revoke_args=record
  a_vp   :p_vnode;
  a_flags:Integer;
 end;

 p_vop_fsync_args=^vop_fsync_args;
 vop_fsync_args=record
  a_vp     :p_vnode;
  a_waitfor:Integer;
 end;

 p_vop_remove_args=^vop_remove_args;
 vop_remove_args=record
  a_dvp:p_vnode;
  a_vp :p_vnode;
  a_cnp:p_componentname;
 end;

 p_vop_link_args=^vop_link_args;
 vop_link_args=record
  a_tdvp:p_vnode;
  a_vp  :p_vnode;
  a_cnp :p_componentname;
 end;

 p_vop_rename_args=^vop_rename_args;
 vop_rename_args=record
  a_fdvp:p_vnode;
  a_fvp :p_vnode;
  a_fcnp:p_componentname;
  a_tdvp:p_vnode;
  a_tvp :p_vnode;
  a_tcnp:p_componentname;
 end;

 p_vop_mkdir_args=^vop_mkdir_args;
 vop_mkdir_args=record
  a_dvp:p_vnode;
  a_vpp:pp_vnode;
  a_cnp:p_componentname;
  a_vap:p_vattr;
 end;

 p_vop_rmdir_args=^vop_rmdir_args;
 vop_rmdir_args=record
  a_dvp:p_vnode;
  a_vp :p_vnode;
  a_cnp:p_componentname;
 end;

 p_vop_symlink_args=^vop_symlink_args;
 vop_symlink_args=record
  a_dvp   :p_vnode;
  a_vpp   :pp_vnode;
  a_cnp   :p_componentname;
  a_vap   :p_vattr;
  a_target:PChar;
 end;

 p_vop_readdir_args=^vop_readdir_args;
 vop_readdir_args=record
  a_vp      :p_vnode;
  a_uio     :p_uio;
  a_eofflag :PInteger;
  a_ncookies:PInteger;
  a_cookies :PPPtrUint;
 end;

 p_vop_readlink_args=^vop_readlink_args;
 vop_readlink_args=record
  a_vp :p_vnode;
  a_uio:p_uio;
 end;

 p_vop_inactive_args=^vop_inactive_args;
 vop_inactive_args=record
  a_vp:p_vnode;
 end;

 p_vop_reclaim_args=^vop_reclaim_args;
 vop_reclaim_args=record
  a_vp:p_vnode;
 end;

 p_vop_lock1_args=^vop_lock1_args;
 vop_lock1_args=record
  a_vp   :p_vnode;
  a_flags:Integer;
  a_file :PChar;
  a_line :Integer;
 end;

 p_vop_unlock_args=^vop_unlock_args;
 vop_unlock_args=record
  a_vp   :p_vnode;
  a_flags:Integer;
 end;

 p_vop_bmap_args=^vop_bmap_args;
 vop_bmap_args=record
  a_vp  :p_vnode;
  a_bn  :daddr_t;
  a_bop :pp_bufobj;
  a_bnp :p_daddr_t;
  a_runp:PInteger;
  a_runb:PInteger;
 end;

 p_vop_strategy_args=^vop_strategy_args;
 vop_strategy_args=record
  a_vp:p_vnode;
  a_bp:p_buf;
 end;

 p_vop_getwritemount_args=^vop_getwritemount_args;
 vop_getwritemount_args=record
  a_vp :p_vnode;
  a_mpp:pp_mount;
 end;

 p_vop_print_args=^vop_print_args;
 vop_print_args=record
  a_vp:p_vnode;
 end;

 p_vop_pathconf_args=^vop_pathconf_args;
 vop_pathconf_args=record
  a_vp    :p_vnode;
  a_name  :Integer;
  a_retval:PPtrUint;
 end;

 p_vop_advlock_args=^vop_advlock_args;
 vop_advlock_args=record
  a_vp   :p_vnode;
  a_id   :Pointer;
  a_op   :Integer;
  a_fl   :p_flock;
  a_flags:Integer;
 end;

 p_vop_advlockasync_args=^vop_advlockasync_args;
 vop_advlockasync_args=record
  a_vp     :p_vnode;
  a_id     :Pointer;
  a_op     :Integer;
  a_fl     :p_flock;
  a_flags  :Integer;
  a_task   :p_task;
  a_cookiep:PPointer;
 end;

 p_vop_advlockpurge_args=^vop_advlockpurge_args;
 vop_advlockpurge_args=record
  a_vp:p_vnode;
 end;

 p_vop_reallocblks_args=^vop_reallocblks_args;
 vop_reallocblks_args=record
  a_vp     :p_vnode;
  a_buflist:p_cluster_save;
 end;

 p_vop_getpages_args=^vop_getpages_args;
 vop_getpages_args=record
  a_vp     :p_vnode;
  a_m      :p_vm_page_t;
  a_count  :Integer;
  a_reqpage:Integer;
  a_offset :PtrUint;
 end;

 p_vop_putpages_args=^vop_putpages_args;
 vop_putpages_args=record
  a_vp    :p_vnode;
  a_m     :p_vm_page_t;
  a_count :Integer;
  a_sync  :Integer;
  a_rtvals:PInteger;
  a_offset:PtrUint;
 end;

 p_vop_getacl_args=^vop_getacl_args;
 vop_getacl_args=record
  a_vp  :p_vnode;
  a_type:acl_type_t;
  a_aclp:p_acl;
 end;

 p_vop_setacl_args=^vop_setacl_args;
 vop_setacl_args=record
  a_vp  :p_vnode;
  a_type:acl_type_t;
  a_aclp:p_acl;
 end;

 p_vop_aclcheck_args=^vop_aclcheck_args;
 vop_aclcheck_args=record
  a_vp  :p_vnode;
  a_type:acl_type_t;
  a_aclp:p_acl;
 end;

 p_vop_closeextattr_args=^vop_closeextattr_args;
 vop_closeextattr_args=record
  a_vp    :p_vnode;
  a_commit:Integer;
 end;

 p_vop_getextattr_args=^vop_getextattr_args;
 vop_getextattr_args=record
  a_vp           :p_vnode;
  a_attrnamespace:Integer;
  a_name         :PChar;
  a_uio          :p_uio;
  a_size         :PPtrUint;
 end;

 p_vop_listextattr_args=^vop_listextattr_args;
 vop_listextattr_args=record
  a_vp           :p_vnode;
  a_attrnamespace:Integer;
  a_uio          :p_uio;
  a_size         :PPtrUint;
 end;

 p_vop_openextattr_args=^vop_openextattr_args;
 vop_openextattr_args=record
  a_vp:p_vnode;
 end;

 p_vop_deleteextattr_args=^vop_deleteextattr_args;
 vop_deleteextattr_args=record
  a_vp           :p_vnode;
  a_attrnamespace:Integer;
  a_name         :PChar;
 end;

 p_vop_setextattr_args=^vop_setextattr_args;
 vop_setextattr_args=record
  a_vp           :p_vnode;
  a_attrnamespace:Integer;
  a_name         :PChar;
  a_uio          :p_uio;
 end;

 p_vop_setlabel_args=^vop_setlabel_args;
 vop_setlabel_args=record
  a_vp   :p_vnode;
  a_label:p_label;
 end;

 p_vop_vptofh_args=^vop_vptofh_args;
 vop_vptofh_args=record
  a_vp :p_vnode;
  a_fhp:p_fid;
 end;

 p_vop_vptocnp_args=^vop_vptocnp_args;
 vop_vptocnp_args=record
  a_vp    :p_vnode;
  a_vpp   :pp_vnode;
  a_buf   :PChar;
  a_buflen:PInteger;
 end;

 p_vop_allocate_args=^vop_allocate_args;
 vop_allocate_args=record
  a_vp    :p_vnode;
  a_offset:PPtrUint;
  a_len   :PPtrUint;
 end;

 p_vop_advise_args=^vop_advise_args;
 vop_advise_args=record
  a_vp    :p_vnode;
  a_start :PtrUint;
  a___end :PtrUint;
  a_advice:Integer;
 end;

 p_vop_unp_bind_args=^vop_unp_bind_args;
 vop_unp_bind_args=record
  a_vp    :p_vnode;
  a_socket:p_socket;
 end;

 p_vop_unp_connect_args=^vop_unp_connect_args;
 vop_unp_connect_args=record
  a_vp    :p_vnode;
  a_socket:pp_socket;
 end;

 p_vop_unp_detach_args=^vop_unp_detach_args;
 vop_unp_detach_args=record
  a_vp:p_vnode;
 end;

 p_vop_is_text_args=^vop_is_text_args;
 vop_is_text_args=record
  a_vp:p_vnode;
 end;

 p_vop_set_text_args=^vop_set_text_args;
 vop_set_text_args=record
  a_vp:p_vnode;
 end;

 p_vop_unset_text_args=^vop_unset_text_args;
 vop_unset_text_args=record
  a_vp:p_vnode;
 end;

 p_vop_get_writecount_args=^vop_get_writecount_args;
 vop_get_writecount_args=record
  a_vp        :p_vnode;
  a_writecount:PInteger;
 end;

 p_vop_add_writecount_args=^vop_add_writecount_args;
 vop_add_writecount_args=record
  a_vp :p_vnode;
  a_inc:Integer;
 end;

 vop_islocked_t      =function(ap:p_vop_islocked_args):Integer;
 vop_lookup_t        =function(ap:p_vop_lookup_args):Integer;
 vop_cachedlookup_t  =function(ap:p_vop_cachedlookup_args):Integer;
 vop_create_t        =function(ap:p_vop_create_args):Integer;
 vop_whiteout_t      =function(ap:p_vop_whiteout_args):Integer;
 vop_mknod_t         =function(ap:p_vop_mknod_args):Integer;
 vop_open_t          =function(ap:p_vop_open_args):Integer;
 vop_close_t         =function(ap:p_vop_close_args):Integer;
 vop_access_t        =function(ap:p_vop_access_args):Integer;
 vop_accessx_t       =function(ap:p_vop_accessx_args):Integer;
 vop_getattr_t       =function(ap:p_vop_getattr_args):Integer;
 vop_setattr_t       =function(ap:p_vop_setattr_args):Integer;
 vop_markatime_t     =function(ap:p_vop_markatime_args):Integer;
 vop_read_t          =function(ap:p_vop_read_args):Integer;
 vop_write_t         =function(ap:p_vop_write_args):Integer;
 vop_ioctl_t         =function(ap:p_vop_ioctl_args):Integer;
 vop_poll_t          =function(ap:p_vop_poll_args):Integer;
 vop_kqfilter_t      =function(ap:p_vop_kqfilter_args):Integer;
 vop_revoke_t        =function(ap:p_vop_revoke_args):Integer;
 vop_fsync_t         =function(ap:p_vop_fsync_args):Integer;
 vop_remove_t        =function(ap:p_vop_remove_args):Integer;
 vop_link_t          =function(ap:p_vop_link_args):Integer;
 vop_rename_t        =function(ap:p_vop_rename_args):Integer;
 vop_mkdir_t         =function(ap:p_vop_mkdir_args):Integer;
 vop_rmdir_t         =function(ap:p_vop_rmdir_args):Integer;
 vop_symlink_t       =function(ap:p_vop_symlink_args):Integer;
 vop_readdir_t       =function(ap:p_vop_readdir_args):Integer;
 vop_readlink_t      =function(ap:p_vop_readlink_args):Integer;
 vop_inactive_t      =function(ap:p_vop_inactive_args):Integer;
 vop_reclaim_t       =function(ap:p_vop_reclaim_args):Integer;
 vop_lock1_t         =function(ap:p_vop_lock1_args):Integer;
 vop_unlock_t        =function(ap:p_vop_unlock_args):Integer;
 vop_bmap_t          =function(ap:p_vop_bmap_args):Integer;
 vop_strategy_t      =function(ap:p_vop_strategy_args):Integer;
 vop_getwritemount_t =function(ap:p_vop_getwritemount_args):Integer;
 vop_print_t         =function(ap:p_vop_print_args):Integer;
 vop_pathconf_t      =function(ap:p_vop_pathconf_args):Integer;
 vop_advlock_t       =function(ap:p_vop_advlock_args):Integer;
 vop_advlockasync_t  =function(ap:p_vop_advlockasync_args):Integer;
 vop_advlockpurge_t  =function(ap:p_vop_advlockpurge_args):Integer;
 vop_reallocblks_t   =function(ap:p_vop_reallocblks_args):Integer;
 vop_getpages_t      =function(ap:p_vop_getpages_args):Integer;
 vop_putpages_t      =function(ap:p_vop_putpages_args):Integer;
 vop_getacl_t        =function(ap:p_vop_getacl_args):Integer;
 vop_setacl_t        =function(ap:p_vop_setacl_args):Integer;
 vop_aclcheck_t      =function(ap:p_vop_aclcheck_args):Integer;
 vop_closeextattr_t  =function(ap:p_vop_closeextattr_args):Integer;
 vop_getextattr_t    =function(ap:p_vop_getextattr_args):Integer;
 vop_listextattr_t   =function(ap:p_vop_listextattr_args):Integer;
 vop_openextattr_t   =function(ap:p_vop_openextattr_args):Integer;
 vop_deleteextattr_t =function(ap:p_vop_deleteextattr_args):Integer;
 vop_setextattr_t    =function(ap:p_vop_setextattr_args):Integer;
 vop_setlabel_t      =function(ap:p_vop_setlabel_args):Integer;
 vop_vptofh_t        =function(ap:p_vop_vptofh_args):Integer;
 vop_vptocnp_t       =function(ap:p_vop_vptocnp_args):Integer;
 vop_allocate_t      =function(ap:p_vop_allocate_args):Integer;
 vop_advise_t        =function(ap:p_vop_advise_args):Integer;
 vop_unp_bind_t      =function(ap:p_vop_unp_bind_args):Integer;
 vop_unp_connect_t   =function(ap:p_vop_unp_connect_args):Integer;
 vop_unp_detach_t    =function(ap:p_vop_unp_detach_args):Integer;
 vop_is_text_t       =function(ap:p_vop_is_text_args):Integer;
 vop_set_text_t      =function(ap:p_vop_set_text_args):Integer;
 vop_unset_text_t    =function(ap:p_vop_unset_text_args):Integer;
 vop_get_writecount_t=function(ap:p_vop_get_writecount_args):Integer;
 vop_add_writecount_t=function(ap:p_vop_add_writecount_args):Integer;

function VOP_ISLOCKED(vp:p_vnode):Integer;
function VOP_LOOKUP(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname):Integer;
function VOP_CACHEDLOOKUP(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname):Integer;
function VOP_CREATE(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname;vap:p_vattr):Integer;
function VOP_WHITEOUT(dvp:p_vnode;cnp:p_componentname;flags:Integer):Integer;
function VOP_MKNOD(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname;vap:p_vattr):Integer;
function VOP_OPEN(vp:p_vnode;mode:Integer;fp:p_file):Integer;
function VOP_CLOSE(vp:p_vnode;fflag:Integer):Integer;
function VOP_ACCESS(vp:p_vnode;accmode:accmode_t):Integer;
function VOP_ACCESSX(vp:p_vnode;accmode:accmode_t):Integer;
function VOP_GETATTR(vp:p_vnode;vap:p_vattr):Integer;
function VOP_SETATTR(vp:p_vnode;vap:p_vattr):Integer;
function VOP_MARKATIME(vp:p_vnode):Integer;
function VOP_READ(vp:p_vnode;uio:p_uio;ioflag:Integer):Integer;
function VOP_WRITE(vp:p_vnode;uio:p_uio;ioflag:Integer):Integer;
function VOP_IOCTL(vp:p_vnode;command:PtrUint;data:Pointer;fflag:Integer):Integer;
function VOP_POLL(vp:p_vnode;events:Integer):Integer;
function VOP_KQFILTER(vp:p_vnode;kn:p_knote):Integer;
function VOP_REVOKE(vp:p_vnode;flags:Integer):Integer;
function VOP_FSYNC(vp:p_vnode;waitfor:Integer):Integer;
function VOP_REMOVE(dvp:p_vnode;vp:p_vnode;cnp:p_componentname):Integer;
function VOP_LINK(tdvp:p_vnode;vp:p_vnode;cnp:p_componentname):Integer;
function VOP_RENAME(fdvp:p_vnode;fvp:p_vnode;fcnp:p_componentname;tdvp:p_vnode;tvp:p_vnode;tcnp:p_componentname):Integer;
function VOP_MKDIR(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname;vap:p_vattr):Integer;
function VOP_RMDIR(dvp:p_vnode;vp:p_vnode;cnp:p_componentname):Integer;
function VOP_SYMLINK(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname;vap:p_vattr;target:PChar):Integer;
function VOP_READDIR(vp:p_vnode;uio:p_uio;eofflag:PInteger;ncookies:PInteger;cookies:PPPtrUint):Integer;
function VOP_READLINK(vp:p_vnode;uio:p_uio):Integer;
function VOP_INACTIVE(vp:p_vnode):Integer;
function VOP_RECLAIM(vp:p_vnode):Integer;
function VOP_LOCK(vp:p_vnode;flags:Integer;_file:PChar;line:Integer):Integer;
function VOP_UNLOCK(vp:p_vnode;flags:Integer):Integer;
function VOP_BMAP(vp:p_vnode;bn:daddr_t;bop:pp_bufobj;bnp:p_daddr_t;runp:PInteger;runb:PInteger):Integer;
function VOP_STRATEGY(vp:p_vnode;bp:p_buf):Integer;
function VOP_GETWRITEMOUNT(vp:p_vnode;mpp:pp_mount):Integer;
function VOP_PRINT(vp:p_vnode):Integer;
function VOP_PATHCONF(vp:p_vnode;name:Integer;retval:PPtrUint):Integer;
function VOP_ADVLOCK(vp:p_vnode;id:Pointer;op:Integer;fl:p_flock;flags:Integer):Integer;
function VOP_ADVLOCKASYNC(vp:p_vnode;id:Pointer;op:Integer;fl:p_flock;flags:Integer;task:p_task;cookiep:PPointer):Integer;
function VOP_ADVLOCKPURGE(vp:p_vnode):Integer;
function VOP_REALLOCBLKS(vp:p_vnode;buflist:p_cluster_save):Integer;
function VOP_GETPAGES(vp:p_vnode;m:p_vm_page_t;count:Integer;reqpage:Integer;offset:PtrUint):Integer;
function VOP_PUTPAGES(vp:p_vnode;m:p_vm_page_t;count:Integer;sync:Integer;rtvals:PInteger;offset:PtrUint):Integer;
function VOP_GETACL(vp:p_vnode;_type:acl_type_t;aclp:p_acl):Integer;
function VOP_SETACL(vp:p_vnode;_type:acl_type_t;aclp:p_acl):Integer;
function VOP_ACLCHECK(vp:p_vnode;_type:acl_type_t;aclp:p_acl):Integer;
function VOP_CLOSEEXTATTR(vp:p_vnode;commit:Integer):Integer;
function VOP_GETEXTATTR(vp:p_vnode;attrnamespace:Integer;name:PChar;uio:p_uio;size:PPtrUint):Integer;
function VOP_LISTEXTATTR(vp:p_vnode;attrnamespace:Integer;uio:p_uio;size:PPtrUint):Integer;
function VOP_OPENEXTATTR(vp:p_vnode):Integer;
function VOP_DELETEEXTATTR(vp:p_vnode;attrnamespace:Integer;name:PChar):Integer;
function VOP_SETEXTATTR(vp:p_vnode;attrnamespace:Integer;name:PChar;uio:p_uio):Integer;
function VOP_SETLABEL(vp:p_vnode;_label:p_label):Integer;
function VOP_VPTOFH(vp:p_vnode;fhp:p_fid):Integer;
function VOP_VPTOCNP(vp:p_vnode;vpp:pp_vnode;buf:PChar;buflen:PInteger):Integer;
function VOP_ALLOCATE(vp:p_vnode;offset:PPtrUint;len:PPtrUint):Integer;
function VOP_ADVISE(vp:p_vnode;start:PtrUint;__end:PtrUint;advice:Integer):Integer;
function VOP_UNP_BIND(vp:p_vnode;socket:p_socket):Integer;
function VOP_UNP_CONNECT(vp:p_vnode;socket:pp_socket):Integer;
function VOP_UNP_DETACH(vp:p_vnode):Integer;
function VOP_IS_TEXT(vp:p_vnode):Integer;
function VOP_SET_TEXT(vp:p_vnode):Integer;
function VOP_UNSET_TEXT(vp:p_vnode):Integer;
function VOP_GET_WRITECOUNT(vp:p_vnode;writecount:PInteger):Integer;
function VOP_ADD_WRITECOUNT(vp:p_vnode;inc:Integer):Integer;

implementation

uses
 vfs_subr;

function VOP_ISLOCKED(vp:p_vnode):Integer;
var
 v:p_vop_vector;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_islocked<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_ISLOCKED');
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_islocked<>nil) then
 begin
  Result:=vop_islocked_t(v^.vop_islocked)(@vp);
 end else
 begin
  Result:=vop_islocked_t(v^.vop_bypass)(@vp);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_LOOKUP(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname):Integer;
var
 v:p_vop_vector;
 a:vop_lookup_args;
 s:Boolean;
begin
 v:=dvp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_lookup<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_LOOKUP');
 a.a_dvp:=dvp;
 a.a_vpp:=vpp;
 a.a_cnp:=cnp;
 s:=VFS_PROLOGUE(dvp^.v_mount);
 if (v^.vop_lookup<>nil) then
 begin
  Result:=vop_lookup_t(v^.vop_lookup)(@a);
 end else
 begin
  Result:=vop_lookup_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_CACHEDLOOKUP(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname):Integer;
var
 v:p_vop_vector;
 a:vop_cachedlookup_args;
 s:Boolean;
begin
 v:=dvp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_cachedlookup<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_CACHEDLOOKUP');
 a.a_dvp:=dvp;
 a.a_vpp:=vpp;
 a.a_cnp:=cnp;
 s:=VFS_PROLOGUE(dvp^.v_mount);
 if (v^.vop_cachedlookup<>nil) then
 begin
  Result:=vop_cachedlookup_t(v^.vop_cachedlookup)(@a);
 end else
 begin
  Result:=vop_cachedlookup_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_CREATE(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname;vap:p_vattr):Integer;
var
 v:p_vop_vector;
 a:vop_create_args;
 s:Boolean;
begin
 v:=dvp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_create<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_CREATE');
 a.a_dvp:=dvp;
 a.a_vpp:=vpp;
 a.a_cnp:=cnp;
 a.a_vap:=vap;
 s:=VFS_PROLOGUE(dvp^.v_mount);
 if (v^.vop_create<>nil) then
 begin
  Result:=vop_create_t(v^.vop_create)(@a);
 end else
 begin
  Result:=vop_create_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
 vop_create_post(@a,Result);
end;

function VOP_WHITEOUT(dvp:p_vnode;cnp:p_componentname;flags:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_whiteout_args;
 s:Boolean;
begin
 v:=dvp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_whiteout<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_WHITEOUT');
 a.a_dvp  :=dvp;
 a.a_cnp  :=cnp;
 a.a_flags:=flags;
 s:=VFS_PROLOGUE(dvp^.v_mount);
 if (v^.vop_whiteout<>nil) then
 begin
  Result:=vop_whiteout_t(v^.vop_whiteout)(@a);
 end else
 begin
  Result:=vop_whiteout_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_MKNOD(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname;vap:p_vattr):Integer;
var
 v:p_vop_vector;
 a:vop_mknod_args;
 s:Boolean;
begin
 v:=dvp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_mknod<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_MKNOD');
 a.a_dvp:=dvp;
 a.a_vpp:=vpp;
 a.a_cnp:=cnp;
 a.a_vap:=vap;
 s:=VFS_PROLOGUE(dvp^.v_mount);
 if (v^.vop_mknod<>nil) then
 begin
  Result:=vop_mknod_t(v^.vop_mknod)(@a);
 end else
 begin
  Result:=vop_mknod_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
 vop_mknod_post(@a,Result);
end;

function VOP_OPEN(vp:p_vnode;mode:Integer;fp:p_file):Integer;
var
 v:p_vop_vector;
 a:vop_open_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_open<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_OPEN');
 a.a_vp  :=vp;
 a.a_mode:=mode;
 a.a_fp  :=fp;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_open<>nil) then
 begin
  Result:=vop_open_t(v^.vop_open)(@a);
 end else
 begin
  Result:=vop_open_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_CLOSE(vp:p_vnode;fflag:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_close_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_close<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_CLOSE');
 a.a_vp   :=vp;
 a.a_fflag:=fflag;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_close<>nil) then
 begin
  Result:=vop_close_t(v^.vop_close)(@a);
 end else
 begin
  Result:=vop_close_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_ACCESS(vp:p_vnode;accmode:accmode_t):Integer;
var
 v:p_vop_vector;
 a:vop_access_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_access<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_ACCESS');
 a.a_vp     :=vp;
 a.a_accmode:=accmode;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_access<>nil) then
 begin
  Result:=vop_access_t(v^.vop_access)(@a);
 end else
 begin
  Result:=vop_access_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_ACCESSX(vp:p_vnode;accmode:accmode_t):Integer;
var
 v:p_vop_vector;
 a:vop_accessx_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_accessx<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_ACCESSX');
 a.a_vp     :=vp;
 a.a_accmode:=accmode;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_accessx<>nil) then
 begin
  Result:=vop_accessx_t(v^.vop_accessx)(@a);
 end else
 begin
  Result:=vop_accessx_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_GETATTR(vp:p_vnode;vap:p_vattr):Integer;
var
 v:p_vop_vector;
 a:vop_getattr_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_getattr<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_GETATTR');
 a.a_vp :=vp;
 a.a_vap:=vap;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_getattr<>nil) then
 begin
  Result:=vop_getattr_t(v^.vop_getattr)(@a);
 end else
 begin
  Result:=vop_getattr_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_SETATTR(vp:p_vnode;vap:p_vattr):Integer;
var
 v:p_vop_vector;
 a:vop_setattr_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_setattr<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_SETATTR');
 a.a_vp :=vp;
 a.a_vap:=vap;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_setattr<>nil) then
 begin
  Result:=vop_setattr_t(v^.vop_setattr)(@a);
 end else
 begin
  Result:=vop_setattr_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
 vop_setattr_post(@a,Result);
end;

function VOP_MARKATIME(vp:p_vnode):Integer;
var
 v:p_vop_vector;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_markatime<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_MARKATIME');
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_markatime<>nil) then
 begin
  Result:=vop_markatime_t(v^.vop_markatime)(@vp);
 end else
 begin
  Result:=vop_markatime_t(v^.vop_bypass)(@vp);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_READ(vp:p_vnode;uio:p_uio;ioflag:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_read_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_read<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_READ');
 a.a_vp    :=vp;
 a.a_uio   :=uio;
 a.a_ioflag:=ioflag;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_read<>nil) then
 begin
  Result:=vop_read_t(v^.vop_read)(@a);
 end else
 begin
  Result:=vop_read_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_WRITE(vp:p_vnode;uio:p_uio;ioflag:Integer):Integer;
var
 osize,ooffset:Int64;
 v:p_vop_vector;
 a:vop_write_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_write<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_WRITE');
 a.a_vp    :=vp;
 a.a_uio   :=uio;
 a.a_ioflag:=ioflag;
 VOP_WRITE_PRE(@a,osize,ooffset);
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_write<>nil) then
 begin
  Result:=vop_write_t(v^.vop_write)(@a);
 end else
 begin
  Result:=vop_write_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
 VOP_WRITE_POST(@a,Result,osize,ooffset);
end;

function VOP_IOCTL(vp:p_vnode;command:PtrUint;data:Pointer;fflag:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_ioctl_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_ioctl<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_IOCTL');
 a.a_vp     :=vp;
 a.a_command:=command;
 a.a_data   :=data;
 a.a_fflag  :=fflag;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_ioctl<>nil) then
 begin
  Result:=vop_ioctl_t(v^.vop_ioctl)(@a);
 end else
 begin
  Result:=vop_ioctl_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_POLL(vp:p_vnode;events:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_poll_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_poll<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_POLL');
 a.a_vp    :=vp;
 a.a_events:=events;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_poll<>nil) then
 begin
  Result:=vop_poll_t(v^.vop_poll)(@a);
 end else
 begin
  Result:=vop_poll_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_KQFILTER(vp:p_vnode;kn:p_knote):Integer;
var
 v:p_vop_vector;
 a:vop_kqfilter_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_kqfilter<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_KQFILTER');
 a.a_vp:=vp;
 a.a_kn:=kn;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_kqfilter<>nil) then
 begin
  Result:=vop_kqfilter_t(v^.vop_kqfilter)(@a);
 end else
 begin
  Result:=vop_kqfilter_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_REVOKE(vp:p_vnode;flags:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_revoke_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_revoke<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_REVOKE');
 a.a_vp   :=vp;
 a.a_flags:=flags;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_revoke<>nil) then
 begin
  Result:=vop_revoke_t(v^.vop_revoke)(@a);
 end else
 begin
  Result:=vop_revoke_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_FSYNC(vp:p_vnode;waitfor:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_fsync_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_fsync<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_FSYNC');
 a.a_vp     :=vp;
 a.a_waitfor:=waitfor;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_fsync<>nil) then
 begin
  Result:=vop_fsync_t(v^.vop_fsync)(@a);
 end else
 begin
  Result:=vop_fsync_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_REMOVE(dvp:p_vnode;vp:p_vnode;cnp:p_componentname):Integer;
var
 v:p_vop_vector;
 a:vop_remove_args;
 s:Boolean;
begin
 v:=dvp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_remove<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_REMOVE');
 a.a_dvp:=dvp;
 a.a_vp :=vp;
 a.a_cnp:=cnp;
 s:=VFS_PROLOGUE(dvp^.v_mount);
 if (v^.vop_remove<>nil) then
 begin
  Result:=vop_remove_t(v^.vop_remove)(@a);
 end else
 begin
  Result:=vop_remove_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
 vop_remove_post(@a,Result);
end;

function VOP_LINK(tdvp:p_vnode;vp:p_vnode;cnp:p_componentname):Integer;
var
 v:p_vop_vector;
 a:vop_link_args;
 s:Boolean;
begin
 v:=tdvp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_link<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_LINK');
 a.a_tdvp:=tdvp;
 a.a_vp  :=vp;
 a.a_cnp :=cnp;
 s:=VFS_PROLOGUE(tdvp^.v_mount);
 if (v^.vop_link<>nil) then
 begin
  Result:=vop_link_t(v^.vop_link)(@a);
 end else
 begin
  Result:=vop_link_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
 vop_link_post(@a,Result);
end;

function VOP_RENAME(fdvp:p_vnode;fvp:p_vnode;fcnp:p_componentname;tdvp:p_vnode;tvp:p_vnode;tcnp:p_componentname):Integer;
var
 v:p_vop_vector;
 a:vop_rename_args;
 s:Boolean;
begin
 v:=fdvp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_rename<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_RENAME');
 a.a_fdvp:=fdvp;
 a.a_fvp :=fvp;
 a.a_fcnp:=fcnp;
 a.a_tdvp:=tdvp;
 a.a_tvp :=tvp;
 a.a_tcnp:=tcnp;
 vop_rename_pre(@a);
 s:=VFS_PROLOGUE(fdvp^.v_mount);
 if (v^.vop_rename<>nil) then
 begin
  Result:=vop_rename_t(v^.vop_rename)(@a);
 end else
 begin
  Result:=vop_rename_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
 vop_rename_post(@a,Result);
end;

function VOP_MKDIR(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname;vap:p_vattr):Integer;
var
 v:p_vop_vector;
 a:vop_mkdir_args;
 s:Boolean;
begin
 v:=dvp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_mkdir<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_MKDIR');
 a.a_dvp:=dvp;
 a.a_vpp:=vpp;
 a.a_cnp:=cnp;
 a.a_vap:=vap;
 s:=VFS_PROLOGUE(dvp^.v_mount);
 if (v^.vop_mkdir<>nil) then
 begin
  Result:=vop_mkdir_t(v^.vop_mkdir)(@a);
 end else
 begin
  Result:=vop_mkdir_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
 vop_mkdir_post(@a,Result);
end;

function VOP_RMDIR(dvp:p_vnode;vp:p_vnode;cnp:p_componentname):Integer;
var
 v:p_vop_vector;
 a:vop_rmdir_args;
 s:Boolean;
begin
 v:=dvp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_rmdir<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_RMDIR');
 a.a_dvp:=dvp;
 a.a_vp :=vp;
 a.a_cnp:=cnp;
 s:=VFS_PROLOGUE(dvp^.v_mount);
 if (v^.vop_rmdir<>nil) then
 begin
  Result:=vop_rmdir_t(v^.vop_rmdir)(@a);
 end else
 begin
  Result:=vop_rmdir_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
 vop_rmdir_post(@a,Result);
end;

function VOP_SYMLINK(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname;vap:p_vattr;target:PChar):Integer;
var
 v:p_vop_vector;
 a:vop_symlink_args;
 s:Boolean;
begin
 v:=dvp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_symlink<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_SYMLINK');
 a.a_dvp   :=dvp;
 a.a_vpp   :=vpp;
 a.a_cnp   :=cnp;
 a.a_vap   :=vap;
 a.a_target:=target;
 s:=VFS_PROLOGUE(dvp^.v_mount);
 if (v^.vop_symlink<>nil) then
 begin
  Result:=vop_symlink_t(v^.vop_symlink)(@a);
 end else
 begin
  Result:=vop_symlink_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
 vop_symlink_post(@a,Result);
end;

function VOP_READDIR(vp:p_vnode;uio:p_uio;eofflag:PInteger;ncookies:PInteger;cookies:PPPtrUint):Integer;
var
 v:p_vop_vector;
 a:vop_readdir_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_readdir<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_READDIR');
 a.a_vp      :=vp;
 a.a_uio     :=uio;
 a.a_eofflag :=eofflag;
 a.a_ncookies:=ncookies;
 a.a_cookies :=cookies;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_readdir<>nil) then
 begin
  Result:=vop_readdir_t(v^.vop_readdir)(@a);
 end else
 begin
  Result:=vop_readdir_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_READLINK(vp:p_vnode;uio:p_uio):Integer;
var
 v:p_vop_vector;
 a:vop_readlink_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_readlink<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_READLINK');
 a.a_vp :=vp;
 a.a_uio:=uio;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_readlink<>nil) then
 begin
  Result:=vop_readlink_t(v^.vop_readlink)(@a);
 end else
 begin
  Result:=vop_readlink_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_INACTIVE(vp:p_vnode):Integer;
var
 v:p_vop_vector;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_inactive<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_INACTIVE');
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_inactive<>nil) then
 begin
  Result:=vop_inactive_t(v^.vop_inactive)(@vp);
 end else
 begin
  Result:=vop_inactive_t(v^.vop_bypass)(@vp);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_RECLAIM(vp:p_vnode):Integer;
var
 v:p_vop_vector;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_reclaim<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_RECLAIM');
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_reclaim<>nil) then
 begin
  Result:=vop_reclaim_t(v^.vop_reclaim)(@vp);
 end else
 begin
  Result:=vop_reclaim_t(v^.vop_bypass)(@vp);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_LOCK(vp:p_vnode;flags:Integer;_file:PChar;line:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_lock1_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_lock1<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_LOCK');
 a.a_vp   :=vp;
 a.a_flags:=flags;
 a.a_file :=_file;
 a.a_line :=line;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_lock1<>nil) then
 begin
  Result:=vop_lock1_t(v^.vop_lock1)(@a);
 end else
 begin
  Result:=vop_lock1_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_UNLOCK(vp:p_vnode;flags:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_unlock_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_unlock<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_UNLOCK');
 a.a_vp   :=vp;
 a.a_flags:=flags;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_unlock<>nil) then
 begin
  Result:=vop_unlock_t(v^.vop_unlock)(@a);
 end else
 begin
  Result:=vop_unlock_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_BMAP(vp:p_vnode;bn:daddr_t;bop:pp_bufobj;bnp:p_daddr_t;runp:PInteger;runb:PInteger):Integer;
var
 v:p_vop_vector;
 a:vop_bmap_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_bmap<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_BMAP');
 a.a_vp  :=vp;
 a.a_bn  :=bn;
 a.a_bop :=bop;
 a.a_bnp :=bnp;
 a.a_runp:=runp;
 a.a_runb:=runb;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_bmap<>nil) then
 begin
  Result:=vop_bmap_t(v^.vop_bmap)(@a);
 end else
 begin
  Result:=vop_bmap_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_STRATEGY(vp:p_vnode;bp:p_buf):Integer;
var
 v:p_vop_vector;
 a:vop_strategy_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_strategy<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_STRATEGY');
 a.a_vp:=vp;
 a.a_bp:=bp;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_strategy<>nil) then
 begin
  Result:=vop_strategy_t(v^.vop_strategy)(@a);
 end else
 begin
  Result:=vop_strategy_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_GETWRITEMOUNT(vp:p_vnode;mpp:pp_mount):Integer;
var
 v:p_vop_vector;
 a:vop_getwritemount_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_getwritemount<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_GETWRITEMOUNT');
 a.a_vp :=vp;
 a.a_mpp:=mpp;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_getwritemount<>nil) then
 begin
  Result:=vop_getwritemount_t(v^.vop_getwritemount)(@a);
 end else
 begin
  Result:=vop_getwritemount_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_PRINT(vp:p_vnode):Integer;
var
 v:p_vop_vector;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_print<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_PRINT');
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_print<>nil) then
 begin
  Result:=vop_print_t(v^.vop_print)(@vp);
 end else
 begin
  Result:=vop_print_t(v^.vop_bypass)(@vp);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_PATHCONF(vp:p_vnode;name:Integer;retval:PPtrUint):Integer;
var
 v:p_vop_vector;
 a:vop_pathconf_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_pathconf<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_PATHCONF');
 a.a_vp    :=vp;
 a.a_name  :=name;
 a.a_retval:=retval;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_pathconf<>nil) then
 begin
  Result:=vop_pathconf_t(v^.vop_pathconf)(@a);
 end else
 begin
  Result:=vop_pathconf_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_ADVLOCK(vp:p_vnode;id:Pointer;op:Integer;fl:p_flock;flags:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_advlock_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_advlock<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_ADVLOCK');
 a.a_vp   :=vp;
 a.a_id   :=id;
 a.a_op   :=op;
 a.a_fl   :=fl;
 a.a_flags:=flags;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_advlock<>nil) then
 begin
  Result:=vop_advlock_t(v^.vop_advlock)(@a);
 end else
 begin
  Result:=vop_advlock_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_ADVLOCKASYNC(vp:p_vnode;id:Pointer;op:Integer;fl:p_flock;flags:Integer;task:p_task;cookiep:PPointer):Integer;
var
 v:p_vop_vector;
 a:vop_advlockasync_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_advlockasync<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_ADVLOCKASYNC');
 a.a_vp     :=vp;
 a.a_id     :=id;
 a.a_op     :=op;
 a.a_fl     :=fl;
 a.a_flags  :=flags;
 a.a_task   :=task;
 a.a_cookiep:=cookiep;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_advlockasync<>nil) then
 begin
  Result:=vop_advlockasync_t(v^.vop_advlockasync)(@a);
 end else
 begin
  Result:=vop_advlockasync_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_ADVLOCKPURGE(vp:p_vnode):Integer;
var
 v:p_vop_vector;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_advlockpurge<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_ADVLOCKPURGE');
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_advlockpurge<>nil) then
 begin
  Result:=vop_advlockpurge_t(v^.vop_advlockpurge)(@vp);
 end else
 begin
  Result:=vop_advlockpurge_t(v^.vop_bypass)(@vp);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_REALLOCBLKS(vp:p_vnode;buflist:p_cluster_save):Integer;
var
 v:p_vop_vector;
 a:vop_reallocblks_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_reallocblks<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_REALLOCBLKS');
 a.a_vp     :=vp;
 a.a_buflist:=buflist;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_reallocblks<>nil) then
 begin
  Result:=vop_reallocblks_t(v^.vop_reallocblks)(@a);
 end else
 begin
  Result:=vop_reallocblks_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_GETPAGES(vp:p_vnode;m:p_vm_page_t;count:Integer;reqpage:Integer;offset:PtrUint):Integer;
var
 v:p_vop_vector;
 a:vop_getpages_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_getpages<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_GETPAGES');
 a.a_vp     :=vp;
 a.a_m      :=m;
 a.a_count  :=count;
 a.a_reqpage:=reqpage;
 a.a_offset :=offset;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_getpages<>nil) then
 begin
  Result:=vop_getpages_t(v^.vop_getpages)(@a);
 end else
 begin
  Result:=vop_getpages_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_PUTPAGES(vp:p_vnode;m:p_vm_page_t;count:Integer;sync:Integer;rtvals:PInteger;offset:PtrUint):Integer;
var
 v:p_vop_vector;
 a:vop_putpages_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_putpages<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_PUTPAGES');
 a.a_vp    :=vp;
 a.a_m     :=m;
 a.a_count :=count;
 a.a_sync  :=sync;
 a.a_rtvals:=rtvals;
 a.a_offset:=offset;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_putpages<>nil) then
 begin
  Result:=vop_putpages_t(v^.vop_putpages)(@a);
 end else
 begin
  Result:=vop_putpages_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_GETACL(vp:p_vnode;_type:acl_type_t;aclp:p_acl):Integer;
var
 v:p_vop_vector;
 a:vop_getacl_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_getacl<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_GETACL');
 a.a_vp  :=vp;
 a.a_type:=_type;
 a.a_aclp:=aclp;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_getacl<>nil) then
 begin
  Result:=vop_getacl_t(v^.vop_getacl)(@a);
 end else
 begin
  Result:=vop_getacl_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_SETACL(vp:p_vnode;_type:acl_type_t;aclp:p_acl):Integer;
var
 v:p_vop_vector;
 a:vop_setacl_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_setacl<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_SETACL');
 a.a_vp  :=vp;
 a.a_type:=_type;
 a.a_aclp:=aclp;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_setacl<>nil) then
 begin
  Result:=vop_setacl_t(v^.vop_setacl)(@a);
 end else
 begin
  Result:=vop_setacl_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_ACLCHECK(vp:p_vnode;_type:acl_type_t;aclp:p_acl):Integer;
var
 v:p_vop_vector;
 a:vop_aclcheck_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_aclcheck<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_ACLCHECK');
 a.a_vp  :=vp;
 a.a_type:=_type;
 a.a_aclp:=aclp;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_aclcheck<>nil) then
 begin
  Result:=vop_aclcheck_t(v^.vop_aclcheck)(@a);
 end else
 begin
  Result:=vop_aclcheck_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_CLOSEEXTATTR(vp:p_vnode;commit:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_closeextattr_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_closeextattr<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_CLOSEEXTATTR');
 a.a_vp    :=vp;
 a.a_commit:=commit;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_closeextattr<>nil) then
 begin
  Result:=vop_closeextattr_t(v^.vop_closeextattr)(@a);
 end else
 begin
  Result:=vop_closeextattr_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_GETEXTATTR(vp:p_vnode;attrnamespace:Integer;name:PChar;uio:p_uio;size:PPtrUint):Integer;
var
 v:p_vop_vector;
 a:vop_getextattr_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_getextattr<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_GETEXTATTR');
 a.a_vp           :=vp;
 a.a_attrnamespace:=attrnamespace;
 a.a_name         :=name;
 a.a_uio          :=uio;
 a.a_size         :=size;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_getextattr<>nil) then
 begin
  Result:=vop_getextattr_t(v^.vop_getextattr)(@a);
 end else
 begin
  Result:=vop_getextattr_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_LISTEXTATTR(vp:p_vnode;attrnamespace:Integer;uio:p_uio;size:PPtrUint):Integer;
var
 v:p_vop_vector;
 a:vop_listextattr_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_listextattr<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_LISTEXTATTR');
 a.a_vp           :=vp;
 a.a_attrnamespace:=attrnamespace;
 a.a_uio          :=uio;
 a.a_size         :=size;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_listextattr<>nil) then
 begin
  Result:=vop_listextattr_t(v^.vop_listextattr)(@a);
 end else
 begin
  Result:=vop_listextattr_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_OPENEXTATTR(vp:p_vnode):Integer;
var
 v:p_vop_vector;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_openextattr<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_OPENEXTATTR');
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_openextattr<>nil) then
 begin
  Result:=vop_openextattr_t(v^.vop_openextattr)(@vp);
 end else
 begin
  Result:=vop_openextattr_t(v^.vop_bypass)(@vp);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_DELETEEXTATTR(vp:p_vnode;attrnamespace:Integer;name:PChar):Integer;
var
 v:p_vop_vector;
 a:vop_deleteextattr_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_deleteextattr<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_DELETEEXTATTR');
 a.a_vp           :=vp;
 a.a_attrnamespace:=attrnamespace;
 a.a_name         :=name;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_deleteextattr<>nil) then
 begin
  Result:=vop_deleteextattr_t(v^.vop_deleteextattr)(@a);
 end else
 begin
  Result:=vop_deleteextattr_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
 vop_deleteextattr_post(@a,Result);
end;

function VOP_SETEXTATTR(vp:p_vnode;attrnamespace:Integer;name:PChar;uio:p_uio):Integer;
var
 v:p_vop_vector;
 a:vop_setextattr_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_setextattr<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_SETEXTATTR');
 a.a_vp           :=vp;
 a.a_attrnamespace:=attrnamespace;
 a.a_name         :=name;
 a.a_uio          :=uio;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_setextattr<>nil) then
 begin
  Result:=vop_setextattr_t(v^.vop_setextattr)(@a);
 end else
 begin
  Result:=vop_setextattr_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
 vop_setextattr_post(@a,Result);
end;

function VOP_SETLABEL(vp:p_vnode;_label:p_label):Integer;
var
 v:p_vop_vector;
 a:vop_setlabel_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_setlabel<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_SETLABEL');
 a.a_vp   :=vp;
 a.a_label:=_label;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_setlabel<>nil) then
 begin
  Result:=vop_setlabel_t(v^.vop_setlabel)(@a);
 end else
 begin
  Result:=vop_setlabel_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_VPTOFH(vp:p_vnode;fhp:p_fid):Integer;
var
 v:p_vop_vector;
 a:vop_vptofh_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_vptofh<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_VPTOFH');
 a.a_vp :=vp;
 a.a_fhp:=fhp;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_vptofh<>nil) then
 begin
  Result:=vop_vptofh_t(v^.vop_vptofh)(@a);
 end else
 begin
  Result:=vop_vptofh_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_VPTOCNP(vp:p_vnode;vpp:pp_vnode;buf:PChar;buflen:PInteger):Integer;
var
 v:p_vop_vector;
 a:vop_vptocnp_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_vptocnp<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_VPTOCNP');
 a.a_vp    :=vp;
 a.a_vpp   :=vpp;
 a.a_buf   :=buf;
 a.a_buflen:=buflen;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_vptocnp<>nil) then
 begin
  Result:=vop_vptocnp_t(v^.vop_vptocnp)(@a);
 end else
 begin
  Result:=vop_vptocnp_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_ALLOCATE(vp:p_vnode;offset:PPtrUint;len:PPtrUint):Integer;
var
 v:p_vop_vector;
 a:vop_allocate_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_allocate<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_ALLOCATE');
 a.a_vp    :=vp;
 a.a_offset:=offset;
 a.a_len   :=len;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_allocate<>nil) then
 begin
  Result:=vop_allocate_t(v^.vop_allocate)(@a);
 end else
 begin
  Result:=vop_allocate_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_ADVISE(vp:p_vnode;start:PtrUint;__end:PtrUint;advice:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_advise_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_advise<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_ADVISE');
 a.a_vp    :=vp;
 a.a_start :=start;
 a.a___end :=__end;
 a.a_advice:=advice;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_advise<>nil) then
 begin
  Result:=vop_advise_t(v^.vop_advise)(@a);
 end else
 begin
  Result:=vop_advise_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_UNP_BIND(vp:p_vnode;socket:p_socket):Integer;
var
 v:p_vop_vector;
 a:vop_unp_bind_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_unp_bind<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_UNP_BIND');
 a.a_vp    :=vp;
 a.a_socket:=socket;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_unp_bind<>nil) then
 begin
  Result:=vop_unp_bind_t(v^.vop_unp_bind)(@a);
 end else
 begin
  Result:=vop_unp_bind_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_UNP_CONNECT(vp:p_vnode;socket:pp_socket):Integer;
var
 v:p_vop_vector;
 a:vop_unp_connect_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_unp_connect<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_UNP_CONNECT');
 a.a_vp    :=vp;
 a.a_socket:=socket;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_unp_connect<>nil) then
 begin
  Result:=vop_unp_connect_t(v^.vop_unp_connect)(@a);
 end else
 begin
  Result:=vop_unp_connect_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_UNP_DETACH(vp:p_vnode):Integer;
var
 v:p_vop_vector;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_unp_detach<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_UNP_DETACH');
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_unp_detach<>nil) then
 begin
  Result:=vop_unp_detach_t(v^.vop_unp_detach)(@vp);
 end else
 begin
  Result:=vop_unp_detach_t(v^.vop_bypass)(@vp);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_IS_TEXT(vp:p_vnode):Integer;
var
 v:p_vop_vector;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_is_text<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_IS_TEXT');
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_is_text<>nil) then
 begin
  Result:=vop_is_text_t(v^.vop_is_text)(@vp);
 end else
 begin
  Result:=vop_is_text_t(v^.vop_bypass)(@vp);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_SET_TEXT(vp:p_vnode):Integer;
var
 v:p_vop_vector;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_set_text<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_SET_TEXT');
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_set_text<>nil) then
 begin
  Result:=vop_set_text_t(v^.vop_set_text)(@vp);
 end else
 begin
  Result:=vop_set_text_t(v^.vop_bypass)(@vp);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_UNSET_TEXT(vp:p_vnode):Integer;
var
 v:p_vop_vector;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_unset_text<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_UNSET_TEXT');
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_unset_text<>nil) then
 begin
  Result:=vop_unset_text_t(v^.vop_unset_text)(@vp);
 end else
 begin
  Result:=vop_unset_text_t(v^.vop_bypass)(@vp);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_GET_WRITECOUNT(vp:p_vnode;writecount:PInteger):Integer;
var
 v:p_vop_vector;
 a:vop_get_writecount_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_get_writecount<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_GET_WRITECOUNT');
 a.a_vp        :=vp;
 a.a_writecount:=writecount;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_get_writecount<>nil) then
 begin
  Result:=vop_get_writecount_t(v^.vop_get_writecount)(@a);
 end else
 begin
  Result:=vop_get_writecount_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

function VOP_ADD_WRITECOUNT(vp:p_vnode;inc:Integer):Integer;
var
 v:p_vop_vector;
 a:vop_add_writecount_args;
 s:Boolean;
begin
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  if (v^.vop_add_writecount<>nil) or (v^.vop_bypass<>nil) then Break;
  v:=v^.vop_default;
 end;
 Assert(v<>nil,'VOP_ADD_WRITECOUNT');
 a.a_vp :=vp;
 a.a_inc:=inc;
 s:=VFS_PROLOGUE(vp^.v_mount);
 if (v^.vop_add_writecount<>nil) then
 begin
  Result:=vop_add_writecount_t(v^.vop_add_writecount)(@a);
 end else
 begin
  Result:=vop_add_writecount_t(v^.vop_bypass)(@a);
 end;
 VFS_EPILOGUE(s);
end;

end.

