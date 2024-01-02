unit vnode;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_mtx,
 vselinfo,
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
 end;

 vtype=(VNON,VREG,VDIR,VBLK,VCHR,VLNK,VSOCK,VFIFO,VBAD,VMARKER);

 p_vpollinfo=^vpollinfo;
 vpollinfo=packed record
  vpi_lock   :mtx;       // lock to protect below
  vpi_selinfo:t_selinfo; // identity of poller(s)
  vpi_events :Word;      // what they are looking for
  vpi_revents:Word;      // what has happened
 end;

 t_vnode=packed object

  //Fields which define the identity of the vnode
  v_type:vtype;         // u vnode type
  v_tag :PChar;         // u type of underlying data
  v_op  :p_vop_vector;  // u vnode operations vector
  v_data:Pointer;       // u private data for fs

  //Filesystem instance stuff
  v_mount:Pointer;          //mount
  v_nmntvnodes:TAILQ_ENTRY; //vnode

  v_un:Pointer; //Type specific fields, only one applies to any given vnode

  v_hash:DWORD;

  v_holdcnt :Integer;    //i prevents recycling.
  v_usecount:Integer;    //i ref count of users
  v_writecount:Integer;  //v ref count of writers

  v_lock:mtx;            // u (if fs don't have one)
  v_interlock:mtx;       // lock for "i" things
  v_vnlock:p_mtx;        //u pointer to vnode lock

  v_iflag:QWORD;         //i vnode flags (see below)
  v_vflag:QWORD;         //v vnode flags

  v_object:Pointer;

  v_actfreelist:TAILQ_ENTRY;

  v_pollinfo:p_vpollinfo; // i Poll events, p for *v_pi

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
  va_fsid     :Int64;    // filesystem id
  va_fileid   :Integer;  // file id
  va_size     :Int64;    // file size in bytes
  va_blocksize:Integer;  // blocksize preferred for i/o
  va_atime    :timespec; // time of last access
  va_mtime    :timespec; // time of last modification
  va_ctime    :timespec; // time file changed
  va_birthtime:timespec; // time file created
  va_gen      :Integer;  // generation number of file
  va_flags    :Integer;  // flags defined for file
  va_rdev     :Integer;  // device the special file represents
  va_bytes    :Int64;    // bytes of disk space held by file
  va_filerev  :Int64;    // file modification number
  va_vaflags  :Integer;  // operations flags, see below
  va_spare    :Integer;  // remain quad aligned
 end;

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

var
 rootvnode:p_vnode=nil;

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
 //rangelock_unlock(@vp^.v_rl, (cookie), VI_MTX(vp))
end;

procedure vn_rangelock_unlock_range(vp:p_vnode;cookie:Pointer;start,__end:Int64);
begin
 //rangelock_unlock_range(@vp^.v_rl, (cookie), start, __end, VI_MTX(vp))
end;

function vn_rangelock_rlock(vp:p_vnode;start,__end:Int64):Pointer;
begin
 Result:=nil;
 //Result:=rangelock_rlock(@vp^.v_rl, start, __end, VI_MTX(vp))
end;

function vn_rangelock_wlock(vp:p_vnode;start,__end:Int64):Pointer;
begin
 Result:=nil;
 //Result:=rangelock_wlock(@vp^.v_rl, start, __end, VI_MTX(vp))
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


end.

