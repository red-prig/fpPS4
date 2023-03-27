unit vnode;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_mtx,
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


type
 p_vop_vector=^vop_vector;
 vop_vector=packed record
  //
 end;

 vtype=(VNON,VREG,VDIR,VBLK,VCHR,VLNK,VSOCK,VFIFO,VBAD,VMARKER);

 p_vnode=^t_vnode;
 t_vnode=packed record
  v_type:vtype;
  v_tag :PChar;
  v_op  :p_vop_vector;
  v_data:Pointer;

  v_mount:Pointer;          //mount
  v_nmntvnodes:TAILQ_ENTRY; //vnode

  v_un:Pointer;

  v_hash:DWORD;

  v_usecount:DWORD;

  v_lock:mtx;

  v_iflag:QWORD;
  v_vflag:QWORD;
 end;

 vattr=packed record
  va_type     :vtype;
  va_mode     :Word;
  va_nlink    :Word;
  va_fsid     :DWORD;
  va_fileid   :QWORD;
  va_size     :QWORD;
  va_blocksize:QWORD;
  va_atime    :timespec;
  va_mtime    :timespec;
  va_ctime    :timespec;
  va_birthtime:timespec;
  va_gen      :QWORD;
  va_flags    :QWORD;
 end;




implementation

end.

