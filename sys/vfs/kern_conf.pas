unit kern_conf;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vmount,
 vfile,
 vuio,
 vfs_vnode,
 time,
 vm,
 vm_object,
 kern_mtx;

const
 SI_ETERNAL   =$0001; { never destroyed }
 SI_ALIAS     =$0002; { carrier of alias name }
 SI_NAMED     =$0004; { make_dev _alias  has been called }
 SI_CHEAPCLONE=$0008; { can be removed_dev'ed when vnode reclaims }
 SI_CHILD     =$0010; { child of another struct cdev *}
 SI_DEVOPEN   =$0020; { opened by device }
 SI_CONSOPEN  =$0040; { opened by console }
 SI_DUMPDEV   =$0080; { is kernel dumpdev }
 SI_CANDELETE =$0100; { can do BIO_DELETE }
 SI_CLONELIST =$0200; { on a clone list }
 SI_UNMAPPED  =$0400; { can handle unmapped I/O }

 SPECNAMELEN  =63;    { max length of devicename }

type
 pp_cdev=^p_cdev;
 p_cdev=^t_cdev;
 t_cdev=packed record
  si_mountpt    :p_mount;
  si_flags      :DWORD;
  _align1       :Integer;
  si_atime      :timespec;
  si_ctime      :timespec;
  si_mtime      :timespec;
  si_uid        :uid_t;
  si_gid        :gid_t;
  si_mode       :mode_t;
  si_drv0       :Integer;
  si_refcount   :Integer;
  _align2       :Integer;
  si_list       :LIST_ENTRY; //(cdev)
  si_clone      :LIST_ENTRY; //(cdev)
  si_children   :Pointer   ; //(cdev)
  si_siblings   :LIST_ENTRY; //(cdev)
  si_parent     :p_cdev;
  si_name       :PChar;
  si_drv1       :Pointer;
  si_drv2       :Pointer;
  si_devsw      :Pointer; //cdevsw
  si_iosize_max :Integer; { maximum I/O size (for physio &al) }
  _align3       :Integer;
  si_usecount   :QWORD;
  si_threadcount:QWORD;
  si_snapdata   :Pointer; //snapdata
  __si_namebuf  :array[0..SPECNAMELEN] of AnsiChar;
 end;

 p_vm_paddr_t=Pointer;

 d_open_t       =Function (dev:p_cdev; oflags:Integer;devtype:Integer):Integer;
 d_fdopen_t     =Function (dev:p_cdev; oflags:Integer;fp:p_file):Integer;
 d_close_t      =Function (dev:p_cdev; fflag:Integer;devtype:Integer):Integer;
 d_strategy_t   =Procedure(bp:Pointer);//bio
 d_ioctl_t      =Function (dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;

 d_read_t       =Function (dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
 d_write_t      =Function (dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
 d_poll_t       =Function (dev:p_cdev;events:Integer):Integer;
 d_kqfilter_t   =Function (dev:p_cdev;kn:Pointer):Integer; //knote
 d_mmap_t       =Function (dev:p_cdev;offset:vm_ooffset_t;paddr:p_vm_paddr_t;nprot:Integer;memattr:p_vm_memattr_t):Integer;
 d_mmap_single_t=Function (cdev:p_cdev;offset:p_vm_ooffset_t;size:vm_size_t;obj:p_vm_object_t;nprot:Integer):Integer;
 d_purge_t      =Procedure(dev:p_cdev);

 dumper_t=Function(
  _priv:Pointer        ;  { Private to the driver. }
  _virtual:Pointer     ;  { Virtual (mapped) address. }
  _physical:vm_offset_t;  { Physical address of virtual. }
  _offset:Int64        ;  { Byte-offset to write at. }
  _length:QWORD):Integer; { Number of bytes to dump. }

const
 {
  * Types for d_flags.
  }
  D_TAPE=$0001;
  D_DISK=$0002;
  D_TTY =$0004;
  D_MEM =$0008;

  D_TYPEMASK=$ffff;

 {
  * Flags for d_flags which the drivers can set.
  }
  D_TRACKCLOSE =$00080000; { track all closes }
  D_MMAP_ANON  =$00100000; { special treatment in vm_mmap.c }
  D_PSEUDO     =$00200000; { make_dev() can return NULL }
  D_NEEDGIANT  =$00400000; { driver want Giant }
  D_NEEDMINOR  =$00800000; { driver uses clone_create() }
  D_UNMAPPED_IO=$01000000; { d_strategy can accept unmapped IO }

 {
  * Version numbers.
  }
  D_VERSION_00=$20011966;
  D_VERSION_01=$17032005; { Add d_uid,gid,mode & kind }
  D_VERSION_02=$28042009; { Add d_mmap_single }
  D_VERSION_03=$17122009; { d_mmap takes memattr,vm_ooffset_t }
  D_VERSION=D_VERSION_03;

 {
  * Flags used for internal housekeeping
  }
  D_INIT =$80000000; { cdevsw initialized }

 {
  * Character device switch table
  }
type
 pp_cdevsw=^p_cdevsw;
 p_cdevsw=^t_cdevsw;
 t_cdevsw=packed record
  d_version    :Integer;
  d_flags      :DWORD;
  d_name       :PChar;
  d_open       :d_open_t;
  d_fdopen     :d_fdopen_t;
  d_close      :d_close_t;
  d_read       :d_read_t;
  d_write      :d_write_t;
  d_ioctl      :d_ioctl_t;
  d_poll       :d_poll_t;
  d_mmap       :d_mmap_t;
  d_strategy   :d_strategy_t;
  d_dump       :dumper_t;
  d_kqfilter   :d_kqfilter_t;
  d_purge      :d_purge_t;
  d_mmap_single:d_mmap_single_t;

  d_spare0:array[0..3] of Integer;
  d_spare1:array[0..2] of Pointer;

  { These fields should not be messed with by drivers }
  d_devs:Pointer; //cdev
  d_spare2:array[0..1] of Integer;
  d_gianttrick:p_cdevsw;
  d_postfree_list:SLIST_ENTRY; //cdevsw
 end;

const
 MAKEDEV_REF      =$01;
 MAKEDEV_WHTOUT   =$02;
 MAKEDEV_NOWAIT   =$04;
 MAKEDEV_WAITOK   =$08;
 MAKEDEV_ETERNAL  =$10;
 MAKEDEV_CHECKNAME=$20;

function dev2unit(d:p_cdev):Integer; inline;
function devtoname(dev:p_cdev):PChar;

type
 cdevpriv_dtr_t=Procedure(data:Pointer);

var
 devmtx:mtx;

procedure dev_lock();
procedure dev_unlock();
procedure dev_ref(dev:p_cdev);
procedure dev_refl(dev:p_cdev);
procedure dev_rel(dev:p_cdev);

function  dev_refthread(dev:p_cdev;ref:PInteger):p_cdevsw;
function  devvn_refthread(vp:p_vnode;devp:pp_cdev;ref:PInteger):p_cdevsw;
procedure dev_relthread(dev:p_cdev;ref:Integer);

implementation

uses
 devfs,
 devfs_devs;

function dev2unit(d:p_cdev):Integer; inline;
begin
 Result:=d^.si_drv0;
end;

function devtoname(dev:p_cdev):PChar; inline;
begin
 Result:=dev^.si_name;
end;

procedure dev_lock();
begin
 mtx_lock(devmtx);
end;

procedure dev_unlock();
begin
 mtx_unlock(devmtx);
end;

procedure dev_ref(dev:p_cdev);
begin
 mtx_lock(devmtx);
 Inc(dev^.si_refcount);
 mtx_unlock(devmtx);
end;

procedure dev_refl(dev:p_cdev);
begin
 mtx_assert(devmtx);
 Inc(dev^.si_refcount);
end;

procedure dev_rel(dev:p_cdev);
var
 flag:Integer;
begin
 flag:=0;

 dev_lock();
 Dec(dev^.si_refcount);
 Assert(dev^.si_refcount >= 0,'dev_rel(%s) gave negative count');

 if (dev^.si_devsw=nil) and
    (dev^.si_refcount=0) then
 begin
  LIST_REMOVE(dev,@dev^.si_list);
  flag:=1;
 end;
 dev_unlock();
 if (flag<>0) then
  devfs_free(dev);
end;

function dev_refthread(dev:p_cdev;ref:PInteger):p_cdevsw;
var
 csw:p_cdevsw;
 cdp:p_cdev_priv;
begin
 if ((dev^.si_flags and SI_ETERNAL)<>0) then
 begin
  ref^:=0;
  Exit(dev^.si_devsw);
 end;
 dev_lock();
 csw:=dev^.si_devsw;
 if (csw<>nil) then
 begin
  cdp:=cdev2priv(dev);
  if ((cdp^.cdp_flags and CDP_SCHED_DTR)=0) then
   Inc(dev^.si_threadcount)
  else
   csw:=nil;
 end;
 dev_unlock();
 ref^:=1;
 Exit(csw);
end;

function devvn_refthread(vp:p_vnode;devp:pp_cdev;ref:PInteger):p_cdevsw;
var
 csw:p_cdevsw;
 cdp:p_cdev_priv;
 dev:p_cdev;
begin
 if ((vp^.v_vflag and VV_ETERNALDEV)<>0) then
 begin
  dev:=vp^.v_rdev;
  if (dev=nil) then
   Exit(nil);
  Assert((dev^.si_flags and SI_ETERNAL)<>0,'Not eternal cdev');
  ref^:=0;
  csw:=dev^.si_devsw;
  Assert(csw<>nil,'Eternal cdev is destroyed');
  devp^:=dev;
  Exit(csw);
 end;

 csw:=nil;
 dev_lock();
 dev:=vp^.v_rdev;
 if (dev=nil) then
 begin
  dev_unlock();
  Exit(nil);
 end;
 cdp:=cdev2priv(dev);
 if ((cdp^.cdp_flags and CDP_SCHED_DTR)=0) then
 begin
  csw:=dev^.si_devsw;
  if (csw<>nil) then
   Inc(dev^.si_threadcount);
 end;
 dev_unlock();
 if (csw<>nil) then
 begin
  devp^:=dev;
  ref^:=1;
 end;
 Exit(csw);
end;

procedure dev_relthread(dev:p_cdev;ref:Integer);
begin
 if (ref=0) then Exit;
 dev_lock();
 Assert(dev^.si_threadcount > 0,'%s threadcount is wrong');
 Dec(dev^.si_threadcount);
 dev_unlock();
end;

end.

