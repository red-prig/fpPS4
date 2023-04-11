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
 vfcntl;

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
 p_socket      =Pointer;
 pp_socket     =Pointer;

 p_vop_islocked_args=^vop_islocked_args;
 vop_islocked_args=packed record
  a_vp:p_vnode;
 end;

 p_vop_lookup_args=^vop_lookup_args;
 vop_lookup_args=packed record
  a_dvp:p_vnode;
  a_vpp:pp_vnode;
  a_cnp:p_componentname;
 end;

 p_vop_cachedlookup_args=^vop_cachedlookup_args;
 vop_cachedlookup_args=packed record
  a_dvp:p_vnode;
  a_vpp:pp_vnode;
  a_cnp:p_componentname;
 end;

 p_vop_create_args=^vop_create_args;
 vop_create_args=packed record
  a_dvp:p_vnode;
  a_vpp:pp_vnode;
  a_cnp:p_componentname;
  a_vap:p_vattr;
 end;

 p_vop_whiteout_args=^vop_whiteout_args;
 vop_whiteout_args=packed record
  a_dvp  :p_vnode;
  a_cnp  :p_componentname;
  a_flags:Integer;
 end;

 p_vop_mknod_args=^vop_mknod_args;
 vop_mknod_args=packed record
  a_dvp:p_vnode;
  a_vpp:pp_vnode;
  a_cnp:p_componentname;
  a_vap:p_vattr;
 end;

 p_vop_open_args=^vop_open_args;
 vop_open_args=packed record
  a_vp  :p_vnode;
  a_mode:Integer;
  a_fp  :p_file;
 end;

 p_vop_close_args=^vop_close_args;
 vop_close_args=packed record
  a_vp   :p_vnode;
  a_fflag:Integer;
 end;

 p_vop_access_args=^vop_access_args;
 vop_access_args=packed record
  a_vp     :p_vnode;
  a_accmode:accmode_t;
 end;

 p_vop_accessx_args=^vop_accessx_args;
 vop_accessx_args=packed record
  a_vp     :p_vnode;
  a_accmode:accmode_t;
 end;

 p_vop_getattr_args=^vop_getattr_args;
 vop_getattr_args=packed record
  a_vp :p_vnode;
  a_vap:p_vattr;
 end;

 p_vop_setattr_args=^vop_setattr_args;
 vop_setattr_args=packed record
  a_vp :p_vnode;
  a_vap:p_vattr;
 end;

 p_vop_markatime_args=^vop_markatime_args;
 vop_markatime_args=packed record
  a_vp:p_vnode;
 end;

 p_vop_read_args=^vop_read_args;
 vop_read_args=packed record
  a_vp    :p_vnode;
  a_uio   :p_uio;
  a_ioflag:Integer;
 end;

 p_vop_write_args=^vop_write_args;
 vop_write_args=packed record
  a_vp    :p_vnode;
  a_uio   :p_uio;
  a_ioflag:Integer;
 end;

 p_vop_ioctl_args=^vop_ioctl_args;
 vop_ioctl_args=packed record
  a_vp     :p_vnode;
  a_command:PtrUint;
  a_data   :Pointer;
  a_fflag  :Integer;
 end;

 p_vop_poll_args=^vop_poll_args;
 vop_poll_args=packed record
  a_vp    :p_vnode;
  a_events:Integer;
 end;

 p_vop_kqfilter_args=^vop_kqfilter_args;
 vop_kqfilter_args=packed record
  a_vp:p_vnode;
  a_kn:p_knote;
 end;

 p_vop_revoke_args=^vop_revoke_args;
 vop_revoke_args=packed record
  a_vp   :p_vnode;
  a_flags:Integer;
 end;

 p_vop_fsync_args=^vop_fsync_args;
 vop_fsync_args=packed record
  a_vp     :p_vnode;
  a_waitfor:Integer;
 end;

 p_vop_remove_args=^vop_remove_args;
 vop_remove_args=packed record
  a_dvp:p_vnode;
  a_vp :p_vnode;
  a_cnp:p_componentname;
 end;

 p_vop_link_args=^vop_link_args;
 vop_link_args=packed record
  a_tdvp:p_vnode;
  a_vp  :p_vnode;
  a_cnp :p_componentname;
 end;

 p_vop_rename_args=^vop_rename_args;
 vop_rename_args=packed record
  a_fdvp:p_vnode;
  a_fvp :p_vnode;
  a_fcnp:p_componentname;
  a_tdvp:p_vnode;
  a_tvp :p_vnode;
  a_tcnp:p_componentname;
 end;

 p_vop_mkdir_args=^vop_mkdir_args;
 vop_mkdir_args=packed record
  a_dvp:p_vnode;
  a_vpp:pp_vnode;
  a_cnp:p_componentname;
  a_vap:p_vattr;
 end;

 p_vop_rmdir_args=^vop_rmdir_args;
 vop_rmdir_args=packed record
  a_dvp:p_vnode;
  a_vp :p_vnode;
  a_cnp:p_componentname;
 end;

 p_vop_symlink_args=^vop_symlink_args;
 vop_symlink_args=packed record
  a_dvp   :p_vnode;
  a_vpp   :pp_vnode;
  a_cnp   :p_componentname;
  a_vap   :p_vattr;
  a_target:PChar;
 end;

 p_vop_readdir_args=^vop_readdir_args;
 vop_readdir_args=packed record
  a_vp      :p_vnode;
  a_uio     :p_uio;
  a_eofflag :PInteger;
  a_ncookies:PInteger;
  a_cookies :PPPtrUint;
 end;

 p_vop_readlink_args=^vop_readlink_args;
 vop_readlink_args=packed record
  a_vp :p_vnode;
  a_uio:p_uio;
 end;

 p_vop_inactive_args=^vop_inactive_args;
 vop_inactive_args=packed record
  a_vp:p_vnode;
 end;

 p_vop_reclaim_args=^vop_reclaim_args;
 vop_reclaim_args=packed record
  a_vp:p_vnode;
 end;

 p_vop_lock1_args=^vop_lock1_args;
 vop_lock1_args=packed record
  a_vp   :p_vnode;
  a_flags:Integer;
  a_file :PChar;
  a_line :Integer;
 end;

 p_vop_unlock_args=^vop_unlock_args;
 vop_unlock_args=packed record
  a_vp   :p_vnode;
  a_flags:Integer;
 end;

 p_vop_bmap_args=^vop_bmap_args;
 vop_bmap_args=packed record
  a_vp  :p_vnode;
  a_bn  :daddr_t;
  a_bop :pp_bufobj;
  a_bnp :p_daddr_t;
  a_runp:PInteger;
  a_runb:PInteger;
 end;

 p_vop_strategy_args=^vop_strategy_args;
 vop_strategy_args=packed record
  a_vp:p_vnode;
  a_bp:p_buf;
 end;

 p_vop_getwritemount_args=^vop_getwritemount_args;
 vop_getwritemount_args=packed record
  a_vp :p_vnode;
  a_mpp:pp_mount;
 end;

 p_vop_print_args=^vop_print_args;
 vop_print_args=packed record
  a_vp:p_vnode;
 end;

 p_vop_pathconf_args=^vop_pathconf_args;
 vop_pathconf_args=packed record
  a_vp    :p_vnode;
  a_name  :Integer;
  a_retval:PPtrUint;
 end;

 p_vop_advlock_args=^vop_advlock_args;
 vop_advlock_args=packed record
  a_vp   :p_vnode;
  a_id   :Pointer;
  a_op   :Integer;
  a_fl   :p_flock;
  a_flags:Integer;
 end;

 p_vop_advlockasync_args=^vop_advlockasync_args;
 vop_advlockasync_args=packed record
  a_vp     :p_vnode;
  a_id     :Pointer;
  a_op     :Integer;
  a_fl     :p_flock;
  a_flags  :Integer;
  a_task   :p_task;
  a_cookiep:PPointer;
 end;

 p_vop_advlockpurge_args=^vop_advlockpurge_args;
 vop_advlockpurge_args=packed record
  a_vp:p_vnode;
 end;

 p_vop_reallocblks_args=^vop_reallocblks_args;
 vop_reallocblks_args=packed record
  a_vp     :p_vnode;
  a_buflist:p_cluster_save;
 end;

 p_vop_getpages_args=^vop_getpages_args;
 vop_getpages_args=packed record
  a_vp     :p_vnode;
  a_m      :p_vm_page_t;
  a_count  :Integer;
  a_reqpage:Integer;
  a_offset :PtrUint;
 end;

 p_vop_putpages_args=^vop_putpages_args;
 vop_putpages_args=packed record
  a_vp    :p_vnode;
  a_m     :p_vm_page_t;
  a_count :Integer;
  a_sync  :Integer;
  a_rtvals:PInteger;
  a_offset:PtrUint;
 end;

 p_vop_getacl_args=^vop_getacl_args;
 vop_getacl_args=packed record
  a_vp  :p_vnode;
  a_type:acl_type_t;
  a_aclp:p_acl;
 end;

 p_vop_setacl_args=^vop_setacl_args;
 vop_setacl_args=packed record
  a_vp  :p_vnode;
  a_type:acl_type_t;
  a_aclp:p_acl;
 end;

 p_vop_aclcheck_args=^vop_aclcheck_args;
 vop_aclcheck_args=packed record
  a_vp  :p_vnode;
  a_type:acl_type_t;
  a_aclp:p_acl;
 end;

 p_vop_closeextattr_args=^vop_closeextattr_args;
 vop_closeextattr_args=packed record
  a_vp    :p_vnode;
  a_commit:Integer;
 end;

 p_vop_getextattr_args=^vop_getextattr_args;
 vop_getextattr_args=packed record
  a_vp           :p_vnode;
  a_attrnamespace:Integer;
  a_name         :PChar;
  a_uio          :p_uio;
  a_size         :PPtrUint;
 end;

 p_vop_listextattr_args=^vop_listextattr_args;
 vop_listextattr_args=packed record
  a_vp           :p_vnode;
  a_attrnamespace:Integer;
  a_uio          :p_uio;
  a_size         :PPtrUint;
 end;

 p_vop_openextattr_args=^vop_openextattr_args;
 vop_openextattr_args=packed record
  a_vp:p_vnode;
 end;

 p_vop_deleteextattr_args=^vop_deleteextattr_args;
 vop_deleteextattr_args=packed record
  a_vp           :p_vnode;
  a_attrnamespace:Integer;
  a_name         :PChar;
 end;

 p_vop_setextattr_args=^vop_setextattr_args;
 vop_setextattr_args=packed record
  a_vp           :p_vnode;
  a_attrnamespace:Integer;
  a_name         :PChar;
  a_uio          :p_uio;
 end;

 p_vop_setlabel_args=^vop_setlabel_args;
 vop_setlabel_args=packed record
  a_vp   :p_vnode;
  a_label:p_label;
 end;

 p_vop_vptofh_args=^vop_vptofh_args;
 vop_vptofh_args=packed record
  a_vp :p_vnode;
  a_fhp:p_fid;
 end;

 p_vop_vptocnp_args=^vop_vptocnp_args;
 vop_vptocnp_args=packed record
  a_vp    :p_vnode;
  a_vpp   :pp_vnode;
  a_buf   :PChar;
  a_buflen:PInteger;
 end;

 p_vop_allocate_args=^vop_allocate_args;
 vop_allocate_args=packed record
  a_vp    :p_vnode;
  a_offset:PPtrUint;
  a_len   :PPtrUint;
 end;

 p_vop_advise_args=^vop_advise_args;
 vop_advise_args=packed record
  a_vp    :p_vnode;
  a_start :PtrUint;
  a___end :PtrUint;
  a_advice:Integer;
 end;

 p_vop_unp_bind_args=^vop_unp_bind_args;
 vop_unp_bind_args=packed record
  a_vp    :p_vnode;
  a_socket:p_socket;
 end;

 p_vop_unp_connect_args=^vop_unp_connect_args;
 vop_unp_connect_args=packed record
  a_vp    :p_vnode;
  a_socket:pp_socket;
 end;

 p_vop_unp_detach_args=^vop_unp_detach_args;
 vop_unp_detach_args=packed record
  a_vp:p_vnode;
 end;

 p_vop_is_text_args=^vop_is_text_args;
 vop_is_text_args=packed record
  a_vp:p_vnode;
 end;

 p_vop_set_text_args=^vop_set_text_args;
 vop_set_text_args=packed record
  a_vp:p_vnode;
 end;

 p_vop_unset_text_args=^vop_unset_text_args;
 vop_unset_text_args=packed record
  a_vp:p_vnode;
 end;

 p_vop_get_writecount_args=^vop_get_writecount_args;
 vop_get_writecount_args=packed record
  a_vp        :p_vnode;
  a_writecount:PInteger;
 end;

 p_vop_add_writecount_args=^vop_add_writecount_args;
 vop_add_writecount_args=packed record
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
begin
 Result:=vop_islocked_t(vp^.v_op^.vop_islocked)(@vp);
end;

function VOP_LOOKUP(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname):Integer;
var
 a:vop_lookup_args;
begin
 a.a_dvp:=dvp;
 a.a_vpp:=vpp;
 a.a_cnp:=cnp;
 Result:=vop_lookup_t(dvp^.v_op^.vop_lookup)(@a);
end;

function VOP_CACHEDLOOKUP(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname):Integer;
var
 a:vop_cachedlookup_args;
begin
 a.a_dvp:=dvp;
 a.a_vpp:=vpp;
 a.a_cnp:=cnp;
 Result:=vop_cachedlookup_t(dvp^.v_op^.vop_cachedlookup)(@a);
end;

function VOP_CREATE(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname;vap:p_vattr):Integer;
var
 a:vop_create_args;
begin
 a.a_dvp:=dvp;
 a.a_vpp:=vpp;
 a.a_cnp:=cnp;
 a.a_vap:=vap;
 Result:=vop_create_t(dvp^.v_op^.vop_create)(@a);
end;

function VOP_WHITEOUT(dvp:p_vnode;cnp:p_componentname;flags:Integer):Integer;
var
 a:vop_whiteout_args;
begin
 a.a_dvp  :=dvp;
 a.a_cnp  :=cnp;
 a.a_flags:=flags;
 Result:=vop_whiteout_t(dvp^.v_op^.vop_whiteout)(@a);
end;

function VOP_MKNOD(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname;vap:p_vattr):Integer;
var
 a:vop_mknod_args;
begin
 a.a_dvp:=dvp;
 a.a_vpp:=vpp;
 a.a_cnp:=cnp;
 a.a_vap:=vap;
 Result:=vop_mknod_t(dvp^.v_op^.vop_mknod)(@a);
end;

function VOP_OPEN(vp:p_vnode;mode:Integer;fp:p_file):Integer;
var
 a:vop_open_args;
begin
 a.a_vp  :=vp;
 a.a_mode:=mode;
 a.a_fp  :=fp;
 Result:=vop_open_t(vp^.v_op^.vop_open)(@a);
end;

function VOP_CLOSE(vp:p_vnode;fflag:Integer):Integer;
var
 a:vop_close_args;
begin
 a.a_vp   :=vp;
 a.a_fflag:=fflag;
 Result:=vop_close_t(vp^.v_op^.vop_close)(@a);
end;

function VOP_ACCESS(vp:p_vnode;accmode:accmode_t):Integer;
var
 a:vop_access_args;
begin
 a.a_vp     :=vp;
 a.a_accmode:=accmode;
 Result:=vop_access_t(vp^.v_op^.vop_access)(@a);
end;

function VOP_ACCESSX(vp:p_vnode;accmode:accmode_t):Integer;
var
 a:vop_accessx_args;
begin
 a.a_vp     :=vp;
 a.a_accmode:=accmode;
 Result:=vop_accessx_t(vp^.v_op^.vop_accessx)(@a);
end;

function VOP_GETATTR(vp:p_vnode;vap:p_vattr):Integer;
var
 a:vop_getattr_args;
begin
 a.a_vp :=vp;
 a.a_vap:=vap;
 Result:=vop_getattr_t(vp^.v_op^.vop_getattr)(@a);
end;

function VOP_SETATTR(vp:p_vnode;vap:p_vattr):Integer;
var
 a:vop_setattr_args;
begin
 a.a_vp :=vp;
 a.a_vap:=vap;
 Result:=vop_setattr_t(vp^.v_op^.vop_setattr)(@a);
end;

function VOP_MARKATIME(vp:p_vnode):Integer;
begin
 Result:=vop_markatime_t(vp^.v_op^.vop_markatime)(@vp);
end;

function VOP_READ(vp:p_vnode;uio:p_uio;ioflag:Integer):Integer;
var
 a:vop_read_args;
begin
 a.a_vp    :=vp;
 a.a_uio   :=uio;
 a.a_ioflag:=ioflag;
 Result:=vop_read_t(vp^.v_op^.vop_read)(@a);
end;

function VOP_WRITE(vp:p_vnode;uio:p_uio;ioflag:Integer):Integer;
var
 a:vop_write_args;
begin
 a.a_vp    :=vp;
 a.a_uio   :=uio;
 a.a_ioflag:=ioflag;
 Result:=vop_write_t(vp^.v_op^.vop_write)(@a);
end;

function VOP_IOCTL(vp:p_vnode;command:PtrUint;data:Pointer;fflag:Integer):Integer;
var
 a:vop_ioctl_args;
begin
 a.a_vp     :=vp;
 a.a_command:=command;
 a.a_data   :=data;
 a.a_fflag  :=fflag;
 Result:=vop_ioctl_t(vp^.v_op^.vop_ioctl)(@a);
end;

function VOP_POLL(vp:p_vnode;events:Integer):Integer;
var
 a:vop_poll_args;
begin
 a.a_vp    :=vp;
 a.a_events:=events;
 Result:=vop_poll_t(vp^.v_op^.vop_poll)(@a);
end;

function VOP_KQFILTER(vp:p_vnode;kn:p_knote):Integer;
var
 a:vop_kqfilter_args;
begin
 a.a_vp:=vp;
 a.a_kn:=kn;
 Result:=vop_kqfilter_t(vp^.v_op^.vop_kqfilter)(@a);
end;

function VOP_REVOKE(vp:p_vnode;flags:Integer):Integer;
var
 a:vop_revoke_args;
begin
 a.a_vp   :=vp;
 a.a_flags:=flags;
 Result:=vop_revoke_t(vp^.v_op^.vop_revoke)(@a);
end;

function VOP_FSYNC(vp:p_vnode;waitfor:Integer):Integer;
var
 a:vop_fsync_args;
begin
 a.a_vp     :=vp;
 a.a_waitfor:=waitfor;
 Result:=vop_fsync_t(vp^.v_op^.vop_fsync)(@a);
end;

function VOP_REMOVE(dvp:p_vnode;vp:p_vnode;cnp:p_componentname):Integer;
var
 a:vop_remove_args;
begin
 a.a_dvp:=dvp;
 a.a_vp :=vp;
 a.a_cnp:=cnp;
 Result:=vop_remove_t(dvp^.v_op^.vop_remove)(@a);
end;

function VOP_LINK(tdvp:p_vnode;vp:p_vnode;cnp:p_componentname):Integer;
var
 a:vop_link_args;
begin
 a.a_tdvp:=tdvp;
 a.a_vp  :=vp;
 a.a_cnp :=cnp;
 Result:=vop_link_t(tdvp^.v_op^.vop_link)(@a);
end;

function VOP_RENAME(fdvp:p_vnode;fvp:p_vnode;fcnp:p_componentname;tdvp:p_vnode;tvp:p_vnode;tcnp:p_componentname):Integer;
var
 a:vop_rename_args;
begin
 a.a_fdvp:=fdvp;
 a.a_fvp :=fvp;
 a.a_fcnp:=fcnp;
 a.a_tdvp:=tdvp;
 a.a_tvp :=tvp;
 a.a_tcnp:=tcnp;
 vop_rename_pre(@a);
 Result:=vop_rename_t(fdvp^.v_op^.vop_rename)(@a);
end;

function VOP_MKDIR(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname;vap:p_vattr):Integer;
var
 a:vop_mkdir_args;
begin
 a.a_dvp:=dvp;
 a.a_vpp:=vpp;
 a.a_cnp:=cnp;
 a.a_vap:=vap;
 Result:=vop_mkdir_t(dvp^.v_op^.vop_mkdir)(@a);
end;

function VOP_RMDIR(dvp:p_vnode;vp:p_vnode;cnp:p_componentname):Integer;
var
 a:vop_rmdir_args;
begin
 a.a_dvp:=dvp;
 a.a_vp :=vp;
 a.a_cnp:=cnp;
 Result:=vop_rmdir_t(dvp^.v_op^.vop_rmdir)(@a);
end;

function VOP_SYMLINK(dvp:p_vnode;vpp:pp_vnode;cnp:p_componentname;vap:p_vattr;target:PChar):Integer;
var
 a:vop_symlink_args;
begin
 a.a_dvp   :=dvp;
 a.a_vpp   :=vpp;
 a.a_cnp   :=cnp;
 a.a_vap   :=vap;
 a.a_target:=target;
 Result:=vop_symlink_t(dvp^.v_op^.vop_symlink)(@a);
end;

function VOP_READDIR(vp:p_vnode;uio:p_uio;eofflag:PInteger;ncookies:PInteger;cookies:PPPtrUint):Integer;
var
 a:vop_readdir_args;
begin
 a.a_vp      :=vp;
 a.a_uio     :=uio;
 a.a_eofflag :=eofflag;
 a.a_ncookies:=ncookies;
 a.a_cookies :=cookies;
 Result:=vop_readdir_t(vp^.v_op^.vop_readdir)(@a);
end;

function VOP_READLINK(vp:p_vnode;uio:p_uio):Integer;
var
 a:vop_readlink_args;
begin
 a.a_vp :=vp;
 a.a_uio:=uio;
 Result:=vop_readlink_t(vp^.v_op^.vop_readlink)(@a);
end;

function VOP_INACTIVE(vp:p_vnode):Integer;
begin
 Result:=vop_inactive_t(vp^.v_op^.vop_inactive)(@vp);
end;

function VOP_RECLAIM(vp:p_vnode):Integer;
begin
 Result:=vop_reclaim_t(vp^.v_op^.vop_reclaim)(@vp);
end;

function VOP_LOCK(vp:p_vnode;flags:Integer;_file:PChar;line:Integer):Integer;
var
 a:vop_lock1_args;
begin
 a.a_vp   :=vp;
 a.a_flags:=flags;
 a.a_file :=_file;
 a.a_line :=line;
 Result:=vop_lock1_t(vp^.v_op^.vop_lock1)(@a);
end;

function VOP_UNLOCK(vp:p_vnode;flags:Integer):Integer;
var
 a:vop_unlock_args;
begin
 a.a_vp   :=vp;
 a.a_flags:=flags;
 Result:=vop_unlock_t(vp^.v_op^.vop_unlock)(@a);
end;

function VOP_BMAP(vp:p_vnode;bn:daddr_t;bop:pp_bufobj;bnp:p_daddr_t;runp:PInteger;runb:PInteger):Integer;
var
 a:vop_bmap_args;
begin
 a.a_vp  :=vp;
 a.a_bn  :=bn;
 a.a_bop :=bop;
 a.a_bnp :=bnp;
 a.a_runp:=runp;
 a.a_runb:=runb;
 Result:=vop_bmap_t(vp^.v_op^.vop_bmap)(@a);
end;

function VOP_STRATEGY(vp:p_vnode;bp:p_buf):Integer;
var
 a:vop_strategy_args;
begin
 a.a_vp:=vp;
 a.a_bp:=bp;
 Result:=vop_strategy_t(vp^.v_op^.vop_strategy)(@a);
end;

function VOP_GETWRITEMOUNT(vp:p_vnode;mpp:pp_mount):Integer;
var
 a:vop_getwritemount_args;
begin
 a.a_vp :=vp;
 a.a_mpp:=mpp;
 Result:=vop_getwritemount_t(vp^.v_op^.vop_getwritemount)(@a);
end;

function VOP_PRINT(vp:p_vnode):Integer;
begin
 Result:=vop_print_t(vp^.v_op^.vop_print)(@vp);
end;

function VOP_PATHCONF(vp:p_vnode;name:Integer;retval:PPtrUint):Integer;
var
 a:vop_pathconf_args;
begin
 a.a_vp    :=vp;
 a.a_name  :=name;
 a.a_retval:=retval;
 Result:=vop_pathconf_t(vp^.v_op^.vop_pathconf)(@a);
end;

function VOP_ADVLOCK(vp:p_vnode;id:Pointer;op:Integer;fl:p_flock;flags:Integer):Integer;
var
 a:vop_advlock_args;
begin
 a.a_vp   :=vp;
 a.a_id   :=id;
 a.a_op   :=op;
 a.a_fl   :=fl;
 a.a_flags:=flags;
 Result:=vop_advlock_t(vp^.v_op^.vop_advlock)(@a);
end;

function VOP_ADVLOCKASYNC(vp:p_vnode;id:Pointer;op:Integer;fl:p_flock;flags:Integer;task:p_task;cookiep:PPointer):Integer;
var
 a:vop_advlockasync_args;
begin
 a.a_vp     :=vp;
 a.a_id     :=id;
 a.a_op     :=op;
 a.a_fl     :=fl;
 a.a_flags  :=flags;
 a.a_task   :=task;
 a.a_cookiep:=cookiep;
 Result:=vop_advlockasync_t(vp^.v_op^.vop_advlockasync)(@a);
end;

function VOP_ADVLOCKPURGE(vp:p_vnode):Integer;
begin
 Result:=vop_advlockpurge_t(vp^.v_op^.vop_advlockpurge)(@vp);
end;

function VOP_REALLOCBLKS(vp:p_vnode;buflist:p_cluster_save):Integer;
var
 a:vop_reallocblks_args;
begin
 a.a_vp     :=vp;
 a.a_buflist:=buflist;
 Result:=vop_reallocblks_t(vp^.v_op^.vop_reallocblks)(@a);
end;

function VOP_GETPAGES(vp:p_vnode;m:p_vm_page_t;count:Integer;reqpage:Integer;offset:PtrUint):Integer;
var
 a:vop_getpages_args;
begin
 a.a_vp     :=vp;
 a.a_m      :=m;
 a.a_count  :=count;
 a.a_reqpage:=reqpage;
 a.a_offset :=offset;
 Result:=vop_getpages_t(vp^.v_op^.vop_getpages)(@a);
end;

function VOP_PUTPAGES(vp:p_vnode;m:p_vm_page_t;count:Integer;sync:Integer;rtvals:PInteger;offset:PtrUint):Integer;
var
 a:vop_putpages_args;
begin
 a.a_vp    :=vp;
 a.a_m     :=m;
 a.a_count :=count;
 a.a_sync  :=sync;
 a.a_rtvals:=rtvals;
 a.a_offset:=offset;
 Result:=vop_putpages_t(vp^.v_op^.vop_putpages)(@a);
end;

function VOP_GETACL(vp:p_vnode;_type:acl_type_t;aclp:p_acl):Integer;
var
 a:vop_getacl_args;
begin
 a.a_vp  :=vp;
 a.a_type:=_type;
 a.a_aclp:=aclp;
 Result:=vop_getacl_t(vp^.v_op^.vop_getacl)(@a);
end;

function VOP_SETACL(vp:p_vnode;_type:acl_type_t;aclp:p_acl):Integer;
var
 a:vop_setacl_args;
begin
 a.a_vp  :=vp;
 a.a_type:=_type;
 a.a_aclp:=aclp;
 Result:=vop_setacl_t(vp^.v_op^.vop_setacl)(@a);
end;

function VOP_ACLCHECK(vp:p_vnode;_type:acl_type_t;aclp:p_acl):Integer;
var
 a:vop_aclcheck_args;
begin
 a.a_vp  :=vp;
 a.a_type:=_type;
 a.a_aclp:=aclp;
 Result:=vop_aclcheck_t(vp^.v_op^.vop_aclcheck)(@a);
end;

function VOP_CLOSEEXTATTR(vp:p_vnode;commit:Integer):Integer;
var
 a:vop_closeextattr_args;
begin
 a.a_vp    :=vp;
 a.a_commit:=commit;
 Result:=vop_closeextattr_t(vp^.v_op^.vop_closeextattr)(@a);
end;

function VOP_GETEXTATTR(vp:p_vnode;attrnamespace:Integer;name:PChar;uio:p_uio;size:PPtrUint):Integer;
var
 a:vop_getextattr_args;
begin
 a.a_vp           :=vp;
 a.a_attrnamespace:=attrnamespace;
 a.a_name         :=name;
 a.a_uio          :=uio;
 a.a_size         :=size;
 Result:=vop_getextattr_t(vp^.v_op^.vop_getextattr)(@a);
end;

function VOP_LISTEXTATTR(vp:p_vnode;attrnamespace:Integer;uio:p_uio;size:PPtrUint):Integer;
var
 a:vop_listextattr_args;
begin
 a.a_vp           :=vp;
 a.a_attrnamespace:=attrnamespace;
 a.a_uio          :=uio;
 a.a_size         :=size;
 Result:=vop_listextattr_t(vp^.v_op^.vop_listextattr)(@a);
end;

function VOP_OPENEXTATTR(vp:p_vnode):Integer;
begin
 Result:=vop_openextattr_t(vp^.v_op^.vop_openextattr)(@vp);
end;

function VOP_DELETEEXTATTR(vp:p_vnode;attrnamespace:Integer;name:PChar):Integer;
var
 a:vop_deleteextattr_args;
begin
 a.a_vp           :=vp;
 a.a_attrnamespace:=attrnamespace;
 a.a_name         :=name;
 Result:=vop_deleteextattr_t(vp^.v_op^.vop_deleteextattr)(@a);
end;

function VOP_SETEXTATTR(vp:p_vnode;attrnamespace:Integer;name:PChar;uio:p_uio):Integer;
var
 a:vop_setextattr_args;
begin
 a.a_vp           :=vp;
 a.a_attrnamespace:=attrnamespace;
 a.a_name         :=name;
 a.a_uio          :=uio;
 Result:=vop_setextattr_t(vp^.v_op^.vop_setextattr)(@a);
end;

function VOP_SETLABEL(vp:p_vnode;_label:p_label):Integer;
var
 a:vop_setlabel_args;
begin
 a.a_vp   :=vp;
 a.a_label:=_label;
 Result:=vop_setlabel_t(vp^.v_op^.vop_setlabel)(@a);
end;

function VOP_VPTOFH(vp:p_vnode;fhp:p_fid):Integer;
var
 a:vop_vptofh_args;
begin
 a.a_vp :=vp;
 a.a_fhp:=fhp;
 Result:=vop_vptofh_t(vp^.v_op^.vop_vptofh)(@a);
end;

function VOP_VPTOCNP(vp:p_vnode;vpp:pp_vnode;buf:PChar;buflen:PInteger):Integer;
var
 a:vop_vptocnp_args;
begin
 a.a_vp    :=vp;
 a.a_vpp   :=vpp;
 a.a_buf   :=buf;
 a.a_buflen:=buflen;
 Result:=vop_vptocnp_t(vp^.v_op^.vop_vptocnp)(@a);
end;

function VOP_ALLOCATE(vp:p_vnode;offset:PPtrUint;len:PPtrUint):Integer;
var
 a:vop_allocate_args;
begin
 a.a_vp    :=vp;
 a.a_offset:=offset;
 a.a_len   :=len;
 Result:=vop_allocate_t(vp^.v_op^.vop_allocate)(@a);
end;

function VOP_ADVISE(vp:p_vnode;start:PtrUint;__end:PtrUint;advice:Integer):Integer;
var
 a:vop_advise_args;
begin
 a.a_vp    :=vp;
 a.a_start :=start;
 a.a___end :=__end;
 a.a_advice:=advice;
 Result:=vop_advise_t(vp^.v_op^.vop_advise)(@a);
end;

function VOP_UNP_BIND(vp:p_vnode;socket:p_socket):Integer;
var
 a:vop_unp_bind_args;
begin
 a.a_vp    :=vp;
 a.a_socket:=socket;
 Result:=vop_unp_bind_t(vp^.v_op^.vop_unp_bind)(@a);
end;

function VOP_UNP_CONNECT(vp:p_vnode;socket:pp_socket):Integer;
var
 a:vop_unp_connect_args;
begin
 a.a_vp    :=vp;
 a.a_socket:=socket;
 Result:=vop_unp_connect_t(vp^.v_op^.vop_unp_connect)(@a);
end;

function VOP_UNP_DETACH(vp:p_vnode):Integer;
begin
 Result:=vop_unp_detach_t(vp^.v_op^.vop_unp_detach)(@vp);
end;

function VOP_IS_TEXT(vp:p_vnode):Integer;
begin
 Result:=vop_is_text_t(vp^.v_op^.vop_is_text)(@vp);
end;

function VOP_SET_TEXT(vp:p_vnode):Integer;
begin
 Result:=vop_set_text_t(vp^.v_op^.vop_set_text)(@vp);
end;

function VOP_UNSET_TEXT(vp:p_vnode):Integer;
begin
 Result:=vop_unset_text_t(vp^.v_op^.vop_unset_text)(@vp);
end;

function VOP_GET_WRITECOUNT(vp:p_vnode;writecount:PInteger):Integer;
var
 a:vop_get_writecount_args;
begin
 a.a_vp        :=vp;
 a.a_writecount:=writecount;
 Result:=vop_get_writecount_t(vp^.v_op^.vop_get_writecount)(@a);
end;

function VOP_ADD_WRITECOUNT(vp:p_vnode;inc:Integer):Integer;
var
 a:vop_add_writecount_args;
begin
 a.a_vp :=vp;
 a.a_inc:=inc;
 Result:=vop_add_writecount_t(vp^.v_op^.vop_add_writecount)(@a);
end;

end.
