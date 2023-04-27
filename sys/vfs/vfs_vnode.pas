unit vfs_vnode;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_thr,
 kern_mtx,
 vselinfo,
 time,
 vmparam;

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
 VMODIFY_PERMS	   =VWRITE or VAPPEND or VADMIN_PERMS or VDELETE_CHILD or VDELETE;

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

 DEV_BSHIFT=9;
 DEV_BSIZE =(1 shl DEV_BSHIFT);

 BLKDEV_IOSIZE=PAGE_SIZE;
 DFLTPHYS     =(64 * 1024);
 MAXPHYS      =(128 * 1024);

type
 p_accmode_t=^accmode_t;
 accmode_t=Integer;

 pp_vnode=^p_vnode;
 p_vnode=^t_vnode;

 p_vop_vector=^vop_vector;
 vop_vector=packed record
  vop_default       :p_vop_vector;
  vop_bypass        :Pointer;

  vop_islocked      :Pointer;
  vop_lookup        :Pointer;
  vop_cachedlookup  :Pointer;
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
  vop_getacl        :Pointer;
  vop_setacl        :Pointer;
  vop_aclcheck      :Pointer;
  vop_closeextattr  :Pointer;
  vop_getextattr    :Pointer;
  vop_listextattr   :Pointer;
  vop_openextattr   :Pointer;
  vop_deleteextattr :Pointer;
  vop_setextattr    :Pointer;
  vop_setlabel      :Pointer;
  vop_vptofh        :Pointer;
  vop_vptocnp       :Pointer;
  vop_allocate      :Pointer;
  vop_advise        :Pointer;
  vop_unp_bind      :Pointer;
  vop_unp_connect   :Pointer;
  vop_unp_detach    :Pointer;
  vop_is_text       :Pointer;
  vop_set_text      :Pointer;
  vop_unset_text    :Pointer;
  vop_get_writecount:Pointer;
  vop_add_writecount:Pointer;
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
  v_type:vtype;
  v_tag :PChar;
  v_op  :p_vop_vector;
  v_data:Pointer;

  v_mount:Pointer;          //mount
  v_nmntvnodes:TAILQ_ENTRY; //vnode

  v_un:Pointer;

  v_hash:DWORD;

  v_holdcnt :Integer;
  v_usecount:Integer;
  v_writecount:Integer;

  v_lock:mtx;
  v_interlock:mtx;

  v_iflag:QWORD;
  v_vflag:QWORD;

  v_actfreelist:TAILQ_ENTRY;

  v_pollinfo:p_vpollinfo; // i Poll events, p for *v_pi

  property v_mountedhere:Pointer read v_un{.vu_mount   } write v_un;
  property v_socket     :Pointer read v_un{.vu_socket  } write v_un;
  property v_rdev       :Pointer read v_un{.vu_cdev    } write v_un;
  property v_fifoinfo   :Pointer read v_un{.vu_fifoinfo} write v_un;
 end;

 p_vattr=^t_vattr;
 t_vattr=packed record
  va_type     :vtype;
  va_mode     :Word;
  va_nlink    :Word;
  va_uid      :Integer;
  va_gid      :Integer;
  va_fsid     :Integer;
  va_fileid   :QWORD;
  va_size     :QWORD;
  va_blocksize:QWORD;
  va_atime    :timespec;
  va_mtime    :timespec;
  va_ctime    :timespec;
  va_birthtime:timespec;
  va_gen      :QWORD;
  va_flags    :QWORD;
  va_rdev     :Integer;
  va_bytes    :QWORD;
  va_filerev  :QWORD;
  va_vaflags  :DWORD;
 end;

procedure VI_LOCK(vp:p_vnode);
function  VI_TRYLOCK(vp:p_vnode):Boolean;
procedure VI_UNLOCK(vp:p_vnode);
function  VI_MTX(vp:p_vnode):p_mtx; inline;

function  IGNORE_LOCK(vp:p_vnode):Boolean; inline;

procedure vn_rangelock_unlock(vp:p_vnode;cookie:Pointer);
procedure vn_rangelock_unlock_range(vp:p_vnode;cookie:Pointer;start,__end:Int64);
function  vn_rangelock_rlock(vp:p_vnode;start,__end:Int64):Pointer;
function  vn_rangelock_wlock(vp:p_vnode;start,__end:Int64):Pointer;

var
 rootvnode:p_vnode;

implementation

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

function VI_MTX(vp:p_vnode):p_mtx; inline;
begin
 Result:=@vp^.v_interlock;
end;

function IGNORE_LOCK(vp:p_vnode):Boolean; inline;
begin
 Result:=(vp=nil) or (vp^.v_type=VCHR) or (vp^.v_type=VBAD);
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

end.

