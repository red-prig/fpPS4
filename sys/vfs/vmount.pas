unit vmount;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_param,
 time,
 sys_event,
 kern_mtx,
 //kern_synch,
 signal,
 vnode;

const
 STATFS_VERSION=$20030518; // current version number

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

 //NFS export related mount flags.
 MNT_EXRDONLY   =$0000000000000080; // exported read only
 MNT_EXPORTED   =$0000000000000100; // filesystem is exported
 MNT_DEFEXPORTED=$0000000000000200; // exported to the world
 MNT_EXPORTANON =$0000000000000400; // anon uid mapping for all
 MNT_EXKERB     =$0000000000000800; // exported with Kerberos
 MNT_EXPUBLIC   =$0000000020000000; // public export (WebNFS)

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

 MNT_VISFLAGMASK=(MNT_RDONLY or MNT_SYNCHRONOUS or MNT_NOEXEC or
    MNT_NOSUID or MNT_UNION or MNT_SUJ or
    MNT_ASYNC or MNT_EXRDONLY or MNT_EXPORTED or
    MNT_DEFEXPORTED or MNT_EXPORTANON or MNT_EXKERB or
    MNT_LOCAL or MNT_USER or MNT_QUOTA or
    MNT_ROOTFS or MNT_NOATIME or MNT_NOCLUSTERR or
    MNT_NOCLUSTERW or MNT_SUIDDIR or MNT_SOFTDEP or
    MNT_IGNORE or MNT_EXPUBLIC or MNT_NOSYMFOLLOW or
    MNT_GJOURNAL or MNT_MULTILABEL or MNT_ACLS or
    MNT_NFS4ACLS);

 MNT_UPDATEMASK=(MNT_NOSUID or MNT_NOEXEC or
    MNT_SYNCHRONOUS or MNT_UNION or MNT_ASYNC or
    MNT_NOATIME or
    MNT_NOSYMFOLLOW or MNT_IGNORE or
    MNT_NOCLUSTERR or MNT_NOCLUSTERW or MNT_SUIDDIR or
    MNT_ACLS or MNT_USER or MNT_NFS4ACLS);

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

{
 * vfs_busy specific flags and mask.
}
 MBF_NOWAIT    =$01;
 MBF_MNTLSTLOCK=$02;
 MBF_MASK      =(MBF_NOWAIT or MBF_MNTLSTLOCK);

 VFCF_STATIC    =$00010000; // statically compiled into kernel
 VFCF_NETWORK   =$00020000; // may get data over the network
 VFCF_READONLY  =$00040000; // writes are not implemented
 VFCF_SYNTHETIC =$00080000; // data does not represent real files
 VFCF_LOOPBACK  =$00100000; // aliases some other mounted FS
 VFCF_UNICODE   =$00200000; // stores file names as Unicode
 VFCF_JAIL      =$00400000; // can be mounted from within a jail
 VFCF_DELEGADMIN=$00800000; // supports delegated administration
 VFCF_SBDRY     =$01000000; // defer stop requests

 { vfsquery flags }
 VQ_NOTRESP    =$0001; { server down }
 VQ_NEEDAUTH   =$0002; { server bad auth }
 VQ_LOWDISK    =$0004; { we're low on space }
 VQ_MOUNT      =$0008; { new filesystem arrived }
 VQ_UNMOUNT    =$0010; { filesystem has left }
 VQ_DEAD       =$0020; { filesystem is dead, needs force unmount }
 VQ_ASSIST     =$0040; { filesystem needs assistance from external program }
 VQ_NOTRESPLOCK=$0080; { server lockd down }

type
 p_fsid=^fsid_t;
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

 pp_vfsoptlist=^p_vfsoptlist;
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

 pp_mount=^p_mount;
 p_mount=^t_mount;

 pp_statfs=^p_statfs;
 p_statfs=^t_statfs;
 p_vfsconf=^vfsconf;

 vfs_cmount_t        =function (ma,data:Pointer;flags:QWORD):Integer;
 vfs_unmount_t       =function (mp:p_mount;mntflags:Integer):Integer;
 vfs_root_t          =function (mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
 vfs_quotactl_t      =function (mp:p_mount;cmds,uid:Integer;arg:Pointer):Integer;
 vfs_statfs_t        =function (mp:p_mount;sbp:p_statfs):Integer;
 vfs_sync_t          =function (mp:p_mount;waitfor:Integer):Integer;
 vfs_vget_t          =function (mp:p_mount;ino:DWORD;flags:Integer;vpp:pp_vnode):Integer;
 vfs_fhtovp_t        =function (mp:p_mount;fhp:p_fid;flags:Integer;vpp:pp_vnode):Integer;
 vfs_checkexp_t      =function (mp:p_mount;nam:Pointer;extflagsp,numsecflavors:Pinteger;secflavors:PPInteger):Integer;
 vfs_init_t          =function (cf:p_vfsconf):Integer;
 vfs_uninit_t        =function (cf:p_vfsconf):Integer;
 vfs_extattrctl_t    =function (mp:p_mount;cmd:Integer;filename_vp:p_vnode;attrnamespace:Integer;attrname:PChar):Integer;
 vfs_mount_t         =function (mp:p_mount):Integer;
 vfs_sysctl_t        =function (mp:p_mount;op:Integer;req:Pointer):Integer;
 vfs_susp_clean_t    =procedure(mp:p_mount);
 vfs_notify_lowervp_t=procedure(mp:p_mount;lowervp:p_vnode);

 p_vfsops=^vfsops;
 vfsops=packed record
  vfs_mount     :vfs_mount_t     ;
  vfs_cmount    :vfs_cmount_t    ;
  vfs_unmount   :vfs_unmount_t   ;
  vfs_root      :vfs_root_t      ;
  vfs_quotactl  :vfs_quotactl_t  ;
  vfs_statfs    :vfs_statfs_t    ;
  vfs_sync      :vfs_sync_t      ;
  vfs_vget      :vfs_vget_t      ;
  vfs_fhtovp    :vfs_fhtovp_t    ;
  vfs_checkexp  :vfs_checkexp_t  ;
  vfs_init      :vfs_init_t      ;
  vfs_uninit    :vfs_uninit_t    ;
  vfs_extattrctl:vfs_extattrctl_t;
  vfs_sysctl    :vfs_sysctl_t    ;
  vfs_susp_clean:vfs_susp_clean_t;
 end;

 t_fsnamelen=array[0..MFSNAMELEN-1] of AnsiChar;
 t_mname    =array[0..MNAMELEN-1]   of AnsiChar;

{
  * Filesystem configuration information. One of these exists for each
  * type of filesystem supported by the kernel. These are searched at
  * mount time to identify the requested filesystem.
  *
  * XXX: Never change the first two arguments!
}
 p_vfsoptdecl=Pointer;

 vfsconf=record
  vfc_version :DWORD       ; // ABI version number
  vfc_name    :t_fsnamelen ; // filesystem type name
  vfc_vfsops  :p_vfsops    ; // filesystem operations vector
  vfc_typenum :Integer     ; // historic filesystem type number
  vfc_refcount:Integer     ; // number mounted of this type
  vfc_flags   :Integer     ; // permanent flags
  vfc_opts    :p_vfsoptdecl; // mount options
  vfc_list    :TAILQ_ENTRY ; // list of vfscons
 end;

 t_statfs=packed record
  f_version    :DWORD;  // structure version number
  f_type       :DWORD;  // type of filesystem
  f_flags      :QWORD;  // copy of mount exported flags
  f_bsize      :QWORD;  // filesystem fragment size
  f_iosize     :QWORD;  // optimal transfer block size
  f_blocks     :QWORD;  // total data blocks in filesystem
  f_bfree      :QWORD;  // free blocks in filesystem
  f_bavail     :Int64;  // free blocks avail to non-superuser
  f_files      :QWORD;  // total file nodes in filesystem
  f_ffree      :Int64;  // free nodes avail to non-superuser
  f_syncwrites :QWORD;  // count of sync writes since mount
  f_asyncwrites:QWORD;  // count of async writes since mount
  f_syncreads  :QWORD;  // count of sync reads since mount
  f_asyncreads :QWORD;  // count of async reads since mount
  f_spare:array[0..9] of QWORD;  // unused spare
  f_namemax    :DWORD;  // maximum filename length
  f_owner      :DWORD;  // user that mounted the filesystem
  f_fsid       :fsid_t; // filesystem id
  f_charspare  :array[0..79] of AnsiChar; // spare string space
  f_fstypename :t_fsnamelen; // filesystem type name
  f_mntfromname:t_mname;     // mounted filesystem
  f_mntonname  :t_mname;     // directory on which mounted
 end;
 {$IF sizeof(t_statfs)<>472}{$STOP sizeof(t_statfs)<>472}{$ENDIF}

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
 t_mount=packed record
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
  mnt_stat               :t_statfs    ;// cache of filesystem stats
  mnt_data               :Pointer     ;// private data
  mnt_time               :time_t      ;// last time written
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

const
 VFS_NOTIFY_UPPER_RECLAIM=1;
 VFS_NOTIFY_UPPER_UNLINK =2;

 VFS_VERSION=$19660120;

var
 mountlist_mtx:mtx;
 mountlist:TAILQ_HEAD=(tqh_first:nil;tqh_last:@mountlist.tqh_first);

 VFS_Giant:mtx;

function  MNT_SHARED_WRITES(mp:p_mount):Boolean; inline;

procedure MNT_ILOCK(mp:p_mount); inline;
function  MNT_ITRYLOCK(mp:p_mount):Boolean; inline;
procedure MNT_IUNLOCK(mp:p_mount); inline;
function  MNT_MTX(mp:p_mount):p_mtx; inline;

procedure MNT_REF(mp:p_mount); inline;
procedure MNT_REL(mp:p_mount); inline;

function  VFS_NEEDSGIANT(mp:p_mount):Boolean;
function  VFS_LOCK_GIANT(mp:p_mount):Integer;
procedure VFS_UNLOCK_GIANT(locked:Integer);
procedure VFS_ASSERT_GIANT(mp:p_mount);

function  VFS_PROLOGUE(mp:p_mount):Boolean;
procedure VFS_EPILOGUE(_enable_stops:Boolean); inline;

function  VFS_MOUNT(mp:p_mount):Integer;
function  VFS_UNMOUNT(mp:p_mount;FORCE:Integer):Integer;
function  VFS_ROOT(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
function  VFS_STATFS(mp:p_mount;sbp:p_statfs):Integer;
function  VFS_SYNC(mp:p_mount;WAIT:Integer):Integer;
function  VFS_VGET(mp:p_mount;ino:QWORD;flags:Integer;vpp:pp_vnode):Integer;
function  VFS_QUOTACTL(mp:p_mount;c,u:Integer;a:Pointer):Integer;
function  VFS_FHTOVP(mp:p_mount;fidp:p_fid;flags:Integer;vpp:pp_vnode):Integer;
function  VFS_EXTATTRCTL(mp:p_mount;c:Integer;fn:p_vnode;ns:Integer;n:PChar):Integer;
procedure VFS_KNOTE_LOCKED(vp:p_vnode;hint:Integer);
procedure VFS_KNOTE_UNLOCKED(vp:p_vnode;hint:Integer);

procedure vmountinit; //SYSINIT

implementation

//

procedure wakeup(ident:Pointer); external;

//

function MNT_SHARED_WRITES(mp:p_mount):Boolean; inline;
begin
 if (mp<>nil) then
 begin
  Result:=((mp^.mnt_kern_flag and MNTK_SHARED_WRITES)<>0);
 end else
 begin
  Result:=False;
 end;
end;

procedure MNT_ILOCK(mp:p_mount); inline;
begin
 mtx_lock(mp^.mnt_mtx);
end;

function MNT_ITRYLOCK(mp:p_mount):Boolean; inline;
begin
 Result:=mtx_trylock(mp^.mnt_mtx);
end;

procedure MNT_IUNLOCK(mp:p_mount); inline;
begin
 mtx_unlock(mp^.mnt_mtx);
end;

function MNT_MTX(mp:p_mount):p_mtx; inline;
begin
 Result:=@mp^.mnt_mtx;
end;

procedure MNT_REF(mp:p_mount); inline;
begin
 System.InterlockedIncrement(mp^.mnt_ref);
end;

procedure MNT_REL(mp:p_mount); inline;
begin
 if (System.InterlockedDecrement(mp^.mnt_ref)=0) then
 begin
  wakeup(mp)
 end;
end;

function VFS_NEEDSGIANT(mp:p_mount):Boolean;
begin
 if (mp<>nil) then
 begin
  Result:=((mp^.mnt_kern_flag and MNTK_MPSAFE)=0);
 end else
 begin
  Result:=False;
 end;
end;

function VFS_LOCK_GIANT(mp:p_mount):Integer; public;
begin
 if VFS_NEEDSGIANT(mp) then
 begin
  mtx_lock(VFS_Giant);
  Result:=1;
 end else
  Result:=0;
end;

procedure VFS_UNLOCK_GIANT(locked:Integer);
begin
 if (locked<>0) then
  mtx_unlock(VFS_Giant);
end;

procedure VFS_ASSERT_GIANT(mp:p_mount);
begin
 if VFS_NEEDSGIANT(mp) then
  mtx_assert(VFS_Giant);
end;

function VFS_PROLOGUE(mp:p_mount):Boolean; public;
begin
 if (mp<>nil) then
 begin
  Result:=((mp^.mnt_vfc^.vfc_flags and VFCF_SBDRY)<>0) and (sigdeferstop<>0);
 end else
 begin
  Result:=False;
 end;
end;

procedure VFS_EPILOGUE(_enable_stops:Boolean); public;
begin
 if _enable_stops then sigallowstop;
end;

function VFS_MOUNT(mp:p_mount):Integer;
var
 _enable_stops:Boolean;
begin
 _enable_stops:=VFS_PROLOGUE(MP);
 Result:=mp^.mnt_op^.vfs_mount(mp);
 VFS_EPILOGUE(_enable_stops);
end;

function VFS_UNMOUNT(mp:p_mount;FORCE:Integer):Integer;
var
 _enable_stops:Boolean;
begin
 _enable_stops:=VFS_PROLOGUE(MP);
 Result:=mp^.mnt_op^.vfs_unmount(mp,FORCE);
 VFS_EPILOGUE(_enable_stops);
end;

function VFS_ROOT(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
var
 _enable_stops:Boolean;
begin
 _enable_stops:=VFS_PROLOGUE(MP);
 Result:=mp^.mnt_op^.vfs_root(mp,flags,vpp);
 VFS_EPILOGUE(_enable_stops);
end;

function VFS_STATFS(mp:p_mount;sbp:p_statfs):Integer;
var
 _enable_stops:Boolean;
begin
 _enable_stops:=VFS_PROLOGUE(MP);
 Result:=mp^.mnt_op^.vfs_statfs(mp,@mp^.mnt_stat);
 if (sbp<>@mp^.mnt_stat) then
 begin
  sbp^:=mp^.mnt_stat;
 end;
 VFS_EPILOGUE(_enable_stops);
end;

function VFS_SYNC(mp:p_mount;WAIT:Integer):Integer;
var
 _enable_stops:Boolean;
begin
 _enable_stops:=VFS_PROLOGUE(MP);
 Result:=mp^.mnt_op^.vfs_sync(mp,WAIT);
 VFS_EPILOGUE(_enable_stops);
end;

function VFS_VGET(mp:p_mount;ino:QWORD;flags:Integer;vpp:pp_vnode):Integer;
var
 _enable_stops:Boolean;
begin
 _enable_stops:=VFS_PROLOGUE(MP);
 Result:=mp^.mnt_op^.vfs_vget(mp,ino,flags,vpp);
 VFS_EPILOGUE(_enable_stops);
end;

function VFS_QUOTACTL(mp:p_mount;c,u:Integer;a:Pointer):Integer;
var
 _enable_stops:Boolean;
begin
 _enable_stops:=VFS_PROLOGUE(MP);
 Result:=mp^.mnt_op^.vfs_quotactl(mp,c,u,a);
 VFS_EPILOGUE(_enable_stops);
end;

function VFS_FHTOVP(mp:p_mount;fidp:p_fid;flags:Integer;vpp:pp_vnode):Integer;
var
 _enable_stops:Boolean;
begin
 _enable_stops:=VFS_PROLOGUE(MP);
 Result:=mp^.mnt_op^.vfs_fhtovp(mp,fidp,flags,vpp);
 VFS_EPILOGUE(_enable_stops);
end;

function VFS_EXTATTRCTL(mp:p_mount;c:Integer;fn:p_vnode;ns:Integer;n:PChar):Integer;
var
 _enable_stops:Boolean;
begin
 _enable_stops:=VFS_PROLOGUE(MP);
 Result:=mp^.mnt_op^.vfs_extattrctl(mp,c,fn,ns,n);
 VFS_EPILOGUE(_enable_stops);
end;

procedure VFS_KNOTE_LOCKED(vp:p_vnode;hint:Integer);
begin
 if ((vp^.v_vflag and VV_NOKNOTE)=0) then
  VN_KNOTE(vp, hint, KNF_LISTLOCKED);
end;

procedure VFS_KNOTE_UNLOCKED(vp:p_vnode;hint:Integer);
begin
 if ((vp^.v_vflag and VV_NOKNOTE)=0) then
  VN_KNOTE(vp, hint, 0);
end;

procedure vmountinit;
begin
 mtx_init(mountlist_mtx,'mountlist');
 mtx_init(VFS_Giant    ,'VFS_Giant');
end;

end.

