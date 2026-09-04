unit vnode;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 uma,
 kern_mtx,
 kern_param,
 vselinfo,
 kern_rangelock,
 time;

const
 VI_MOUNT      =$0020; // Mount in progress
 VI_AGE        =$0040; // Insert vnode at head of free list
 VI_DOOMED     =$0080; // This vnode is being recycled
 VI_FREE       =$0100; // This vnode is on the freelist
 VI_ACTIVE     =$0200; // This vnode is on the active list
 VI_DOINGINACT =$0800; // VOP_INACTIVE is in progress
 VI_OWEINACT   =$1000; // Need to call inactive

 VV_ROOT       =$0001; // root of its filesystem
 VV_ISTTY      =$0002; // vnode represents a tty
 VV_NOSYNC     =$0004; // unlinked, stop syncing
 VV_ETERNALDEV =$0008; // device that is never destroyed
 VV_CACHEDLABEL=$0010; // Vnode has valid cached MAC label
 VV_TEXT       =$0020; // vnode is a pure text prototype
 VV_COPYONWRITE=$0040; // vnode is doing copy-on-write
 VV_SYSTEM     =$0080; // vnode being used by kernel
 VV_PROCDEP    =$0100; // vnode is process dependent
 VV_NOKNOTE    =$0200; // don't activate knotes on this vnode
 VV_DELETED    =$0400; // should be removed
 VV_MD         =$0800; // vnode backs the md device
 VV_FORCEINSMQ =$1000; // force the insmntque to succeed

 // Sony extension
 VV_0x2000            =   $2000;
 VV_LARGEWRITE        =   $4000;
 VV_NOCRYPT           =   $8000;
 VV_COMPRESS          =  $10000;
 VV_LARGEWRITEMMAPABLE=  $20000;
 VV_LVD               =  $40000;
 VV_RDONLYMAPPING     =  $80000;
 VV_WRONLYMAPPING     = $100000;
 VV_ROLE_DATA         =$1000000;

 //Flags for va_vaflags.
 VA_UTIMES_NULL=$01; // utimes argument was NULL
 VA_EXCLUSIVE  =$02; // exclusive create request

 {
 * Flags for ioflag. (high 16 bits used to ask for read-ahead and
 * help with write clustering)
 * NB: IO_NDELAY and IO_DIRECT are linked to fcntl.h
 }
 IO_UNIT      =$0001; { do I/O as atomic unit }
 IO_APPEND    =$0002; { append write to end }
 IO_NDELAY    =$0004; { FNDELAY flag set in file table }
 IO_NODELOCKED=$0008; { underlying node already locked }
 IO_ASYNC     =$0010; { bawrite rather then bdwrite }
 IO_VMIO      =$0020; { data already in VMIO space }
 IO_INVAL     =$0040; { invalidate after I/O }
 IO_SYNC      =$0080; { do I/O synchronously }
 IO_DIRECT    =$0100; { attempt to bypass buffer cache }
 IO_EXT       =$0400; { operate on external attributes }
 IO_NORMAL    =$0800; { operate on regular data }
 IO_NOMACCHECK=$1000; { MAC checks unnecessary }
 IO_BUFLOCKED =$2000; { ffs flag; indir buf is locked }

 IO_SEQMAX    =$7F;   { seq heuristic max value }
 IO_SEQSHIFT  =16;    { seq heuristic in upper 16 bits }

 //Flags for accmode_t.
 VEXEC  =&000000000100; // execute/search permission
 VWRITE =&000000000200; // write permission
 VREAD  =&000000000400; // read permission
 VADMIN =&000000010000; // being the file owner
 VAPPEND=&000000040000; // permission to write/append

 VEXPLICIT_DENY    =&000000100000;
 VDELETE_CHILD     =&000001000000;
 VREAD_ATTRIBUTES  =&000002000000; // permission to stat(2)
 VWRITE_ATTRIBUTES =&000004000000; // change {m,c,a}time
 VDELETE           =&000010000000;
 VREAD_ACL         =&000020000000; // read ACL and file mode
 VWRITE_ACL        =&000040000000; // change ACL and/or file mode
 VWRITE_OWNER      =&000100000000; // change file owner
 VSYNCHRONIZE      =&000200000000;

 VADMIN_PERMS      =VADMIN or VWRITE_ATTRIBUTES or VWRITE_ACL or VWRITE_OWNER;

 //Permissions that were traditionally granted to everyone.
 VSTAT_PERMS       =VREAD_ATTRIBUTES or VREAD_ACL;

 //Permissions that allow to change the state of the file in any way.
 VMODIFY_PERMS    =VWRITE or VAPPEND or VADMIN_PERMS or VDELETE_CHILD or VDELETE;

 // vn_open_flags
 VN_OPEN_NOAUDIT=$00000001;

 //Flags to various vnode functions.
 SKIPSYSTEM =$0001; // vflush: skip vnodes marked VSYSTEM
 FORCECLOSE =$0002; // vflush: force file closure
 WRITECLOSE =$0004; // vflush: only close writable files
 EARLYFLUSH =$0008; // vflush: early call for ffs_flushfiles
 V_SAVE     =$0001; // vinvalbuf: sync file first
 V_ALT      =$0002; // vinvalbuf: invalidate only alternate bufs
 V_NORMAL   =$0004; // vinvalbuf: invalidate only regular bufs
 V_CLEANONLY=$0008; // vinvalbuf: invalidate only clean bufs
 REVOKEALL  =$0001; // vop_revoke: revoke all aliases
 V_WAIT     =$0001; // vn_start_write: sleep for suspend
 V_NOWAIT   =$0002; // vn_start_write: don't sleep for suspend
 V_XSLEEP   =$0004; // vn_start_write: just return after sleep

 VR_START_WRITE=$0001; // vfs_write_resume: start write atomically
 VR_NO_SUSPCLR =$0002; // vfs_write_resume: do not clear suspension

 VNOVAL=-1;

 //Flags for vdesc_flags:
 VDESC_MAX_VPS=16;
 // Low order 16 flag bits are reserved for willrele flags for vp arguments.
 VDESC_VP0_WILLRELE=$0001;
 VDESC_VP1_WILLRELE=$0002;
 VDESC_VP2_WILLRELE=$0004;
 VDESC_VP3_WILLRELE=$0008;
 VDESC_NOMAP_VPP   =$0100;
 VDESC_VPP_WILLRELE=$0200;

 {
  * VDESC_NO_OFFSET is used to identify the end of the offset list
  * and in places where no such field exists.
  }
 VDESC_NO_OFFSET=-1;

type
 p_accmode_t=^accmode_t;
 accmode_t=Integer;

 { This structure describes the vnode operation taking place. }
 p_vnodeop_desc=^t_vnodeop_desc;
 t_vnodeop_desc=record
  vdesc_name                :PChar;    { a readable name for debugging }
  vdesc_call                :Pointer;  { Function to call }

  {
   * These ops are used by bypass routines to map and locate arguments.
   * Creds and procs are not needed in bypass routines, but sometimes
   * they are useful to (for example) transport layers.
   * Nameidata is useful because it has a cred in it.
   }
  vdesc_vp_offsets          :PByte;    { list ended by VDESC_NO_OFFSET }
  vdesc_flags               :Integer;  { VDESC_* flags }
  vdesc_vpp_offset          :Integer;  { return vpp location }
 end;

 p_vop_generic_args=^t_vop_generic_args;
 t_vop_generic_args=record
  a_desc:p_vnodeop_desc;
  //other random data follows, presumably
 end;

 vop_bypass_t=function(ap:Pointer):Integer;

 pp_vnode=^p_vnode;
 p_vnode=^t_vnode;

 p_vop_vector=^vop_vector;
 vop_vector=packed record
  vop_default       :p_vop_vector;
  vop_bypass        :Pointer;

  vop_islocked      :Pointer;
  vop_lookup        :Pointer;
  vop_create        :Pointer;
  vop_whiteout      :Pointer;
  vop_mknod         :Pointer;
  vop_open          :Pointer;
  vop_close         :Pointer;
  vop_access        :Pointer;
  vop_accessx       :Pointer;
  vop_getattr       :Pointer;
  vop_setattr       :Pointer;
  vop_markatime     :Pointer;
  vop_read          :Pointer;
  vop_write         :Pointer;
  vop_ioctl         :Pointer;
  vop_poll          :Pointer;
  vop_kqfilter      :Pointer;
  vop_revoke        :Pointer;
  vop_fsync         :Pointer;
  vop_remove        :Pointer;
  vop_link          :Pointer;
  vop_rename        :Pointer;
  vop_mkdir         :Pointer;
  vop_rmdir         :Pointer;
  vop_symlink       :Pointer;
  vop_readdir       :Pointer;
  vop_readlink      :Pointer;
  vop_inactive      :Pointer;
  vop_reclaim       :Pointer;
  vop_lock1         :Pointer;
  vop_unlock        :Pointer;
  vop_bmap          :Pointer;
  vop_strategy      :Pointer;
  vop_getwritemount :Pointer;
  vop_print         :Pointer;
  vop_pathconf      :Pointer;
  vop_advlock       :Pointer;
  vop_advlockasync  :Pointer;
  vop_advlockpurge  :Pointer;
  vop_reallocblks   :Pointer;
  vop_getpages      :Pointer;
  vop_putpages      :Pointer;
  vop_vptofh        :Pointer;
  vop_vptocnp       :Pointer;
  vop_allocate      :Pointer;
  vop_unp_bind      :Pointer;
  vop_unp_connect   :Pointer;
  vop_unp_detach    :Pointer;

  //emu ext
  vop_get_int_obj   :Pointer;
 end;

 vtype=(VNON,VREG,VDIR,VBLK,VCHR,VLNK,VSOCK,VFIFO,VBAD,VMARKER);

 p_vpollinfo=^vpollinfo;
 vpollinfo=packed record
  vpi_lock   :mtx;       // lock to protect below
  vpi_selinfo:t_selinfo; // identity of poller(s)
  vpi_events :Word;      // what they are looking for
  vpi_revents:Word;      // what has happened
 end;

 pp_mount=^p_mount;
 p_mount =^t_mount;

 t_vnode=packed object

  //Fields which define the identity of the vnode
  v_type:vtype;         // u vnode type
  v_prot:Integer;       // emu ext
  v_tag :PChar;         // u type of underlying data
  v_op  :p_vop_vector;  // u vnode operations vector
  v_data:Pointer;       // u private data for fs

  //Filesystem instance stuff
  v_mount     :p_mount;     //mount
  v_nmntvnodes:TAILQ_ENTRY; //vnode

  v_un:Pointer; //Type specific fields, only one applies to any given vnode

  v_hash:DWORD;

  v_holdcnt   :Integer;  //i prevents recycling.
  v_usecount  :Integer;  //i ref count of users
  v_writecount:Integer;  //v ref count of writers

  v_lock     :mtx;       //u (if fs don't have one)
  v_interlock:mtx;       //lock for "i" things
  v_vnlock   :p_mtx;     //u pointer to vnode lock

  v_iflag:QWORD;         //i vnode flags (see below)
  v_vflag:QWORD;         //v vnode flags

  v_object:Pointer;

  v_actfreelist:TAILQ_ENTRY;

  v_pollinfo:p_vpollinfo; // i Poll events, p for *v_pi

  v_rl:rangelock;         //Byte-range lock

  property v_mountedhere:Pointer read v_un{.vu_mount   } write v_un; //mount
  property v_socket     :Pointer read v_un{.vu_socket  } write v_un; //socket
  property v_rdev       :Pointer read v_un{.vu_cdev    } write v_un; //cdev
  property v_fifoinfo   :Pointer read v_un{.vu_fifoinfo} write v_un; //fifoinfo
 end;

 p_vattr=^t_vattr;
 t_vattr=record
  va_type     :vtype;    // vnode type (for create)
  va_mode     :SmallInt; // files access mode and type
  va_nlink    :SmallInt; // number of references to file
  va_uid      :Integer;  // owner user id
  va_gid      :Integer;  // owner group id
  va_fsid     :Integer;  // filesystem id
  va_fileid   :Int64;    // file id
  va_size     :Int64;    // file size in bytes
  va_blocksize:Int64;    // blocksize preferred for i/o
  va_atime    :timespec; // time of last access
  va_mtime    :timespec; // time of last modification
  va_ctime    :timespec; // time file changed
  va_birthtime:timespec; // time file created
  va_gen      :Int64;    // generation number of file
  va_flags    :Int64;    // flags defined for file
  va_rdev     :Integer;  // device the special file represents
  va_bytes    :Int64;    // bytes of disk space held by file
  va_filerev  :Int64;    // file modification number
  va_vaflags  :Integer;  // operations flags, see below
  va_spare    :Int64;    // remain quad aligned
 end;

 //merged mount header ---

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
  mnt_budget_id          :Integer     ;
 end;

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

 //merged mount header ---

const
 iftovt_tab:array[0..15] of vtype=(
  VNON, VFIFO, VCHR, VNON, VDIR, VNON, VBLK, VNON,
  VREG, VNON, VLNK, VNON, VSOCK, VNON, VNON, VBAD
 );

function  VOPARG_OFFSETTO(s_offset:Integer;struct_p:Pointer):Pointer;
function  VCALL(c:Pointer):Integer;

function  VN_KNLIST_EMPTY(vp:p_vnode):Boolean;
procedure VN_KNOTE(vp:p_vnode;a:Integer;b:QWORD);
procedure VN_KNOTE_LOCKED(vp:p_vnode;b:QWORD);
procedure VN_KNOTE_UNLOCKED(vp:p_vnode;b:QWORD);

procedure VI_LOCK(vp:p_vnode);
function  VI_TRYLOCK(vp:p_vnode):Boolean;
procedure VI_UNLOCK(vp:p_vnode);
function  VI_MTX(vp:p_vnode):p_mtx;

function  IGNORE_LOCK(vp:p_vnode):Boolean;

procedure VOP_ADD_WRITECOUNT(vp:p_vnode;i:Integer);

procedure vn_rangelock_unlock(vp:p_vnode;cookie:Pointer);
procedure vn_rangelock_unlock_range(vp:p_vnode;cookie:Pointer;start,__end:Int64);
function  vn_rangelock_rlock(vp:p_vnode;start,__end:Int64):Pointer;
function  vn_rangelock_wlock(vp:p_vnode;start,__end:Int64):Pointer;

function  vn_canvmio(vp:p_vnode):Boolean;

//emu ext
type
 p_vop_get_int_obj_args=^vop_get_int_obj_args;
 vop_get_int_obj_args=record
  a_gen   :p_vnodeop_desc;
  a_vp    :p_vnode;
  a_offset:QWORD;   // in/out
  a_length:QWORD;   // in/out
  a_obj   :Pointer; // out p_vm_int_obj
 end;
 vop_get_int_obj_t=function(ap:p_vop_get_int_obj_args):Integer;

const
 vop_get_int_obj_vp_offsets:array[0..1] of Byte=(Byte(ptrint(@p_vop_get_int_obj_args(nil)^.a_vp)),Byte(-1));

 vop_get_int_obj_desc:t_vnodeop_desc=(
  vdesc_name                :'vop_get_int_obj';
  vdesc_call                :@p_vop_vector(nil)^.vop_get_int_obj;
  vdesc_vp_offsets          :@vop_get_int_obj_vp_offsets;
  vdesc_flags               :0;
  vdesc_vpp_offset          :-1;
 );

function VOP_GET_INT_OBJ(vp:p_vnode;var offset,length:QWORD;var obj:Pointer):Integer;

//emu ext

var
 rootvnode:p_vnode=nil;
 namei_zone:uma_zone_t=nil;

implementation

uses
 sys_event;

//

function  VFS_PROLOGUE(mp:Pointer):Boolean; external;
procedure VFS_EPILOGUE(_enable_stops:Boolean); external;

//

function VOPARG_OFFSETTO(s_offset:Integer;struct_p:Pointer):Pointer;
begin
 Result:=struct_p+s_offset;
end;

function get_vp_cb(vp:p_vnode;offset:Pointer):Pointer; inline;
var
 v:p_vop_vector;
 p:Pointer;
begin
 Result:=nil;
 if (vp=nil) then Exit;
 v:=vp^.v_op;
 while (v<>nil) do
 begin
  p:=PPointer(Pointer(v)+ptrint(offset))^;
  if (p<>nil) then
  begin
   Exit(p);
  end;
  p:=v^.vop_bypass;
  if (p<>nil) then
  begin
   Exit(p);
  end;
  v:=v^.vop_default;
 end;
end;

function vcall_panic:Integer; inline;
begin
 Assert(false,'filesystem goof: vcall_panic');
 Exit(2);
end;

type
 p_vop_vcall_args=^t_vop_vcall_args;
 t_vop_vcall_args=record
  a_desc:p_vnodeop_desc;
  a_vp  :p_vnode;
 end;

function VCALL(c:Pointer):Integer;
var
 ap:p_vop_vcall_args;
 s:Boolean;
begin
 if (c=nil) then Exit(vcall_panic);
 ap:=c;
 if (ap^.a_desc=nil) then Exit(vcall_panic);
 if (ap^.a_vp=nil)   then Exit(vcall_panic);
 if (ap^.a_desc^.vdesc_call=nil) then Exit(vcall_panic);
 c:=get_vp_cb(ap^.a_vp,ap^.a_desc^.vdesc_call);
 Assert(c<>nil,'VCALL');
 s:=VFS_PROLOGUE(ap^.a_vp^.v_mount);
 Result:=vop_bypass_t(c)(ap);
 VFS_EPILOGUE(s);
end;

// We don't need to lock the knlist
function VN_KNLIST_EMPTY(vp:p_vnode):Boolean;
begin
 if (vp^.v_pollinfo=nil) then Exit(True);
 Result:=M_KNLIST_EMPTY(@vp^.v_pollinfo^.vpi_selinfo.si_note)
end;

procedure VN_KNOTE(vp:p_vnode;a:Integer;b:QWORD);
begin
 if (not VN_KNLIST_EMPTY(vp)) then
 begin
  KNOTE(@vp^.v_pollinfo^.vpi_selinfo.si_note,b,a or KNF_NOKQLOCK);
 end;
end;

procedure VN_KNOTE_LOCKED(vp:p_vnode;b:QWORD);
begin
 VN_KNOTE(vp, b, KNF_LISTLOCKED);
end;

procedure VN_KNOTE_UNLOCKED(vp:p_vnode;b:QWORD);
begin
 VN_KNOTE(vp, b, 0);
end;

procedure VI_LOCK(vp:p_vnode);
begin
 mtx_lock(vp^.v_interlock);
end;

function VI_TRYLOCK(vp:p_vnode):Boolean;
begin
 Result:=mtx_trylock(vp^.v_interlock);
end;

procedure VI_UNLOCK(vp:p_vnode);
begin
 mtx_unlock(vp^.v_interlock);
end;

function VI_MTX(vp:p_vnode):p_mtx;
begin
 Result:=@vp^.v_interlock;
end;

function IGNORE_LOCK(vp:p_vnode):Boolean;
begin
 if (vp=nil) then Exit(True);
 Result:=(vp^.v_type=VCHR) or (vp^.v_type=VBAD);
end;

procedure VOP_ADD_WRITECOUNT(vp:p_vnode;i:Integer);
begin
 System.InterlockedExchangeAdd(vp^.v_writecount,i);
end;

procedure vn_rangelock_unlock(vp:p_vnode;cookie:Pointer);
begin
 rangelock_unlock(@vp^.v_rl, (cookie), VI_MTX(vp))
end;

procedure vn_rangelock_unlock_range(vp:p_vnode;cookie:Pointer;start,__end:Int64);
begin
 rangelock_unlock_range(@vp^.v_rl, (cookie), start, __end, VI_MTX(vp))
end;

function vn_rangelock_rlock(vp:p_vnode;start,__end:Int64):Pointer;
begin
 Result:=rangelock_rlock(@vp^.v_rl, start, __end, VI_MTX(vp))
end;

function vn_rangelock_wlock(vp:p_vnode;start,__end:Int64):Pointer;
begin
 Result:=rangelock_wlock(@vp^.v_rl, start, __end, VI_MTX(vp))
end;

const
 vmiodirenable=False;

function vn_canvmio(vp:p_vnode):Boolean;
begin
 if (vp<>nil) then
 begin
  if (vp^.v_type=VREG) or
     (vmiodirenable and (vp^.v_type=VDIR)) then
  begin
   Exit(True);
  end;
 end;
 Result:=False;
end;

//emu ext
function VOP_GET_INT_OBJ(vp:p_vnode;var offset,length:QWORD;var obj:Pointer):Integer;
var
 c:Pointer;
 a:vop_get_int_obj_args;
 s:Boolean;
begin
 c:=get_vp_cb(vp,vop_get_int_obj_desc.vdesc_call);
 Assert(c<>nil,'VOP_GET_INT_OBJ');
 //
 a.a_gen   :=@vop_get_int_obj_desc;
 a.a_vp    :=vp;
 a.a_offset:=offset;
 a.a_length:=length;
 a.a_obj   :=nil;
 //
 s:=VFS_PROLOGUE(vp^.v_mount);
 Result:=vop_get_int_obj_t(c)(@a);
 VFS_EPILOGUE(s);
 //
 offset:=a.a_offset;
 length:=a.a_length;
 obj   :=a.a_obj   ;
end;


end.

