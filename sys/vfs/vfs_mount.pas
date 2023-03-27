unit vfs_mount;

{$mode ObjFPC}{$H+}

interface

uses
 mqueue,
 time,
 kern_mtx,
 vfs_vnode;

const
 MAXFIDSZ=16;

 MFSNAMELEN=16; // length of type name including null
 MNAMELEN  =88; // size of on/from name bufs

 //User specifiable flags, stored in mnt_flag.
 MNT_RDONLY     =$0000000000000001; // read only filesystem
 MNT_SYNCHRONOUS=$0000000000000002; // fs written synchronously
 MNT_NOEXEC     =$0000000000000004; // can't exec from filesystem
 MNT_NOSUID     =$0000000000000008; // don't honor setuid fs bits
 MNT_NFS4ACLS   =$0000000000000010; // enable NFS version 4 ACLs
 MNT_UNION      =$0000000000000020; // union with underlying fs
 MNT_ASYNC      =$0000000000000040; // fs written asynchronously
 MNT_SUIDDIR    =$0000000000100000; // special SUID dir handling
 MNT_SOFTDEP    =$0000000000200000; // using soft updates
 MNT_NOSYMFOLLOW=$0000000000400000; // do not follow symlinks
 MNT_GJOURNAL   =$0000000002000000; // GEOM journal support enabled
 MNT_MULTILABEL =$0000000004000000; // MAC support for objects
 MNT_ACLS       =$0000000008000000; // ACL support enabled
 MNT_NOATIME    =$0000000010000000; // dont update file access time
 MNT_NOCLUSTERR =$0000000040000000; // disable cluster read
 MNT_NOCLUSTERW =$0000000080000000; // disable cluster write
 MNT_SUJ        =$0000000100000000; // using journaled soft updates

{
  * Flags set by internal operations,
  * but visible to the user.
  * XXX some of these are not quite right.. (I've never seen the root flag set)
}
 MNT_LOCAL =$0000000000001000; // filesystem is stored locally
 MNT_QUOTA =$0000000000002000; // quotas are enabled on fs
 MNT_ROOTFS=$0000000000004000; // identifies the root fs
 MNT_USER  =$0000000000008000; // mounted by a user
 MNT_IGNORE=$0000000000800000; // do not show entry in df

{
  * External filesystem command modifier flags.
  * Unmount can use the MNT_FORCE flag.
  * XXX: These are not STATES and really should be somewhere else.
  * XXX: MNT_BYFSID collides with MNT_ACLS, but because MNT_ACLS is only used for
  *      mount(2) and MNT_BYFSID is only used for unmount(2) it's harmless.
}
 MNT_UPDATE   =$0000000000010000; // not real mount, just update
 MNT_DELEXPORT=$0000000000020000; // delete export host lists
 MNT_RELOAD   =$0000000000040000; // reload filesystem data
 MNT_FORCE    =$0000000000080000; // force unmount or readonly
 MNT_SNAPSHOT =$0000000001000000; // snapshot the filesystem
 MNT_BYFSID   =$0000000008000000; // specify filesystem by ID.

{
  * Internal filesystem control flags stored in mnt_kern_flag.
  *
  * MNTK_UNMOUNT locks the mount entry so that name lookup cannot proceed
  * past the mount point.  This keeps the subtree stable during mounts
  * and unmounts.
  *
  * MNTK_UNMOUNTF permits filesystems to detect a forced unmount while
  * dounmount() is still waiting to lock the mountpoint. This allows
  * the filesystem to cancel operations that might otherwise deadlock
  * with the unmount attempt (used by NFS).
  *
  * MNTK_NOINSMNTQ is strict subset of MNTK_UNMOUNT. They are separated
  * to allow for failed unmount attempt to restore the syncer vnode for
  * the mount.
}
 MNTK_UNMOUNTF          =$00000001; // forced unmount in progress
 MNTK_ASYNC             =$00000002; // filtered async flag
 MNTK_SOFTDEP           =$00000004; // async disabled by softdep
 MNTK_NOINSMNTQ         =$00000008; // insmntque is not allowed
 MNTK_DRAINING          =$00000010; // lock draining is happening
 MNTK_REFEXPIRE         =$00000020; // refcount expiring is happening
 MNTK_EXTENDED_SHARED   =$00000040; // Allow shared locking for more ops
 MNTK_SHARED_WRITES     =$00000080; // Allow shared locking for writes
 MNTK_NO_IOPF           =$00000100; { Disallow page faults during reads
                                      and writes. Filesystem shall properly
                                      handle i/o state on EFAULT.}
 MNTK_VGONE_UPPER       =$00000200;
 MNTK_VGONE_WAITER      =$00000400;
 MNTK_LOOKUP_EXCL_DOTDOT=$00000800;
 MNTK_MARKER            =$00001000;
 MNTK_UNMAPPED_BUFS     =$00002000;
 MNTK_NOASYNC           =$00800000; // disable async
 MNTK_UNMOUNT           =$01000000; // unmount in progress
 MNTK_MWAIT             =$02000000; // waiting for unmount to finish
 MNTK_SUSPEND           =$08000000; // request write suspension
 MNTK_SUSPEND2          =$04000000; // block secondary writes
 MNTK_SUSPENDED         =$10000000; // write operations are suspended
 MNTK_MPSAFE            =$20000000; // Filesystem is MPSAFE.
 MNTK_LOOKUP_SHARED     =$40000000; // FS supports shared lock lookups
 MNTK_NOKNOTE           =$80000000; // Don't send KNOTEs from VOP hooks

{
  * Sysctl CTL_VFS definitions.
  *
  * Second level identifier specifies which filesystem. Second level
  * identifier VFS_VFSCONF returns information about all filesystems.
  * Second level identifier VFS_GENERIC is non-terminal.
}
 VFS_VFSCONF=0; // get configured filesystems
 VFS_GENERIC=0; // generic filesystem information

{
  * Third level identifiers for VFS_GENERIC are given below; third
  * level identifiers for specific filesystems are given in their
  * mount specific header files.
}
 VFS_MAXTYPENUM=1; // int: highest defined filesystem type
 VFS_CONF      =2; // struct: vfsconf for filesystem given as next argument

{
  * Flags for various system call interfaces.
  *
  * waitfor flags to vfs_sync() and getfsstat()
}
 MNT_WAIT   =1; // synchronously wait for I/O to complete
 MNT_NOWAIT =2; // start all I/O, but do not wait for it
 MNT_LAZY   =3; // push data not written by filesystem syncer
 MNT_SUSPEND=4; // Suspend file system after sync

type
 fsid_t=packed record  // filesystem id type
  val:array[0..1] of Integer;
 end;

{
 * File identifier.
 * These are unique per filesystem on a single machine.
}
 p_fid=^fid;
 fid=packed record
  fid_len  :Word;  // length of data in bytes
  fid_data0:Word;  // force longword alignment
  fid_data :array[0..MAXFIDSZ-1] of Byte; // data (variable length)
 end;

 vnodelst=TAILQ_HEAD; //vnode

 p_vfsoptlist=^vfsoptlist;
 vfsoptlist=TAILQ_HEAD; //vfsopt

 // Mount options list
 p_vfsopt=^vfsopt;
 vfsopt=packed record
  link :TAILQ_ENTRY; //vfsopt
  name :PChar;
  value:Pointer;
  len  :Integer;
  pos  :Integer;
  seen :Integer;
 end;

{
 * Operations supported on mounted filesystem.
}

 PPInteger=^PInteger;

 vfs_cmount_t        =function (ma,data:Pointer;flags:QWORD):Integer;
 vfs_unmount_t       =function (mp:Pointer;mntflags:Integer):Integer;
 vfs_root_t          =function (mp:Pointer;flags:Integer;vpp:pp_vnode):Integer;
 vfs_quotactl_t      =function (mp:Pointer;cmds:Integer;uid:QWORD;arg:Pointer):Integer;
 vfs_statfs_t        =function (mp:Pointer;sbp:Pointer):Integer;
 vfs_sync_t          =function (mp:Pointer;waitfor:Integer):Integer;
 vfs_vget_t          =function (mp:Pointer;ino:QWORD;flags:Integer;vpp:pp_vnode):Integer;
 vfs_fhtovp_t        =function (mp:Pointer;fhp:p_fid;flags:Integer;vpp:pp_vnode):Integer;
 vfs_checkexp_t      =function (mp:Pointer;nam:Pointer;extflagsp,numsecflavors:Pinteger;secflavors:PPInteger):Integer;
 vfs_init_t          =function (cf:Pointer):Integer;
 vfs_uninit_t        =function (cf:Pointer):Integer;
 vfs_extattrctl_t    =function (mp:Pointer;cmd:Integer;filename_vp:p_vnode;attrnamespace:Integer;attrname:PChar):Integer;
 vfs_mount_t         =function (mp:Pointer):Integer;
 vfs_sysctl_t        =function (mp:Pointer;op:QWORD;req:Pointer):Integer;
 vfs_susp_clean_t    =procedure(mp:Pointer);
 vfs_notify_lowervp_t=procedure(mp:Pointer;lowervp:p_vnode);

 p_vfsops=^vfsops;
 vfsops=packed record
  vfs_mount          :vfs_mount_t         ;
  vfs_cmount         :vfs_cmount_t        ;
  vfs_unmount        :vfs_unmount_t       ;
  vfs_root           :vfs_root_t          ;
  vfs_quotactl       :vfs_quotactl_t      ;
  vfs_statfs         :vfs_statfs_t        ;
  vfs_sync           :vfs_sync_t          ;
  vfs_vget           :vfs_vget_t          ;
  vfs_fhtovp         :vfs_fhtovp_t        ;
  vfs_checkexp       :vfs_checkexp_t      ;
  vfs_init           :vfs_init_t          ;
  vfs_uninit         :vfs_uninit_t        ;
  vfs_extattrctl     :vfs_extattrctl_t    ;
  vfs_sysctl         :vfs_sysctl_t        ;
  vfs_susp_clean     :vfs_susp_clean_t    ;
  vfs_reclaim_lowervp:vfs_notify_lowervp_t;
  vfs_unlink_lowervp :vfs_notify_lowervp_t;
 end;

{
  * Filesystem configuration information. One of these exists for each
  * type of filesystem supported by the kernel. These are searched at
  * mount time to identify the requested filesystem.
  *
  * XXX: Never change the first two arguments!
}
 p_vfsoptdecl=Pointer;

 p_vfsconf=^vfsconf;
 vfsconf=packed record
  vfc_version :DWORD       ; // ABI version number
  vfc_name    :array[0..MFSNAMELEN-1] of Char; // filesystem type name
  vfc_vfsops  :p_vfsops    ; // filesystem operations vector
  vfc_typenum :Integer     ; // historic filesystem type number
  vfc_refcount:Integer     ; // number mounted of this type
  vfc_flags   :Integer     ; // permanent flags
  vfc_opts    :p_vfsoptdecl; // mount options
  vfc_list    :TAILQ_ENTRY ; // list of vfscons
 end;

 {
   * Structure per mounted filesystem.  Each mounted filesystem has an
   * array of operations and an instance record.  The filesystems are
   * put on a doubly linked list.
   *
   * Lock reference:
   * m - mountlist_mtx
   * i - interlock
   * v - vnode freelist mutex
   *
   * Unmarked fields are considered stable as long as a ref is held.
   *
 }
 p_mount=^mount;
 mount=packed record
  mnt_mtx                :mtx         ;// mount structure interlock
  mnt_gen                :Integer     ;// mount generation
  mnt_list               :TAILQ_ENTRY ;// (m) mount list
  mnt_op                 :p_vfsops    ;// operations on fs
  mnt_vfc                :p_vfsconf   ;// configuration info
  mnt_vnodecovered       :p_vnode     ;// vnode we mounted on
  mnt_syncer             :p_vnode     ;// syncer vnode
  mnt_ref                :Integer     ;// (i) Reference count
  mnt_nvnodelist         :vnodelst    ;// (i) list of vnodes
  mnt_nvnodelistsize     :Integer     ;// (i) # of vnodes
  mnt_activevnodelist    :vnodelst    ;// (v) list of active vnodes
  mnt_activevnodelistsize:Integer     ;// (v) # of active vnodes
  mnt_writeopcount       :Integer     ;// (i) write syscalls pending
  mnt_kern_flag          :Integer     ;// (i) kernel only flags
  mnt_flag               :QWORD       ;// (i) flags shared with user
  mnt_pad_noasync        :DWORD       ;
  mnt_opt                :p_vfsoptlist;// current mount options
  mnt_optnew             :p_vfsoptlist;// new options passed to fs
  mnt_maxsymlinklen      :Integer     ;// max size of short symlink
  //mnt_stat               :statfs      ;// cache of filesystem stats
  mnt_data               :Pointer     ;// private data
  mnt_time               :time_t      ;// last time written*/
  mnt_iosize_max         :Integer     ;// max size for clusters, etc
  //mnt_export             :p_netexport ;// export list
  //mnt_label              :p_label     ;// MAC label for the fs
  mnt_hashseed           :DWORD       ;// Random seed for vfs_hash
  mnt_lockref            :Integer     ;// (i) Lock reference count
  mnt_secondary_writes   :Integer     ;// (i) # of secondary writes
  mnt_secondary_accwrites:Integer     ;// (i) secondary wr. starts
  mnt_susp_owner         :Pointer     ;// (i) thread owning suspension
  mnt_explock            :mtx         ;// vfs_export walkers lock
  mnt_upper_link         :TAILQ_ENTRY ;// (m) we in the all uppers
  mnt_uppers             :TAILQ_HEAD  ;// (m) upper mounts over us
 end;

 //Generic file handle
 fhandle_t=packed record
  fh_fsid:fsid_t; // Filesystem id of mount point
  fh_fid :fid     // Filesys specific id
 end;


implementation

end.

