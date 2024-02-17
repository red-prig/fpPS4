unit sys_conf;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 time,
 kern_param,
 vmount,
 vfile,
 vuio,
 vnode,
 sys_event,
 vm,
 sys_vm_object,
 kern_mtx;

const
 //modeventtype
 MOD_LOAD    =0;
 MOD_UNLOAD  =1;
 MOD_SHUTDOWN=2;
 MOD_QUIESCE =3;

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
  si_children   :LIST_HEAD ; //(cdev)
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

 d_open_t        =Function (dev:p_cdev;oflags,devtype:Integer):Integer;
 d_fdopen_t      =Function (dev:p_cdev;oflags:Integer;fp:p_file):Integer;
 d_close_t       =Function (dev:p_cdev;fflag,devtype:Integer):Integer;
 d_strategy_t    =Procedure(bp:Pointer);//bio
 d_ioctl_t       =Function (dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;

 d_read_t        =Function (dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
 d_write_t       =Function (dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
 d_poll_t        =Function (dev:p_cdev;events:Integer):Integer;
 d_kqfilter_t    =Function (dev:p_cdev;kn:p_knote):Integer;
 d_mmap_t        =Function (dev:p_cdev;offset:vm_ooffset_t;paddr:p_vm_paddr_t;nprot:Integer;memattr:p_vm_memattr_t):Integer;
 d_mmap_single_t =Function (dev:p_cdev;offset:p_vm_ooffset_t;size:vm_size_t;obj:p_vm_object_t;nprot:Integer):Integer;
 d_mmap_single2_t=Function (dev:p_cdev;offset:p_vm_ooffset_t;size:vm_size_t;obj:p_vm_object_t;nprot:Integer;maxprotp:p_vm_prot_t;flagsp:PInteger):Integer;
 d_purge_t       =Procedure(dev:p_cdev);

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
  d_version     :Integer;
  d_flags       :DWORD;
  d_name        :PChar;
  d_open        :d_open_t;
  d_fdopen      :d_fdopen_t;
  d_close       :d_close_t;
  d_read        :d_read_t;
  d_write       :d_write_t;
  d_ioctl       :d_ioctl_t;
  d_poll        :d_poll_t;
  d_mmap        :d_mmap_t;
  d_strategy    :d_strategy_t;
  d_dump        :dumper_t;
  d_kqfilter    :d_kqfilter_t;
  d_purge       :d_purge_t;
  d_mmap_single :d_mmap_single_t;
  d_mmap_single2:d_mmap_single2_t;

  d_spare0:array[0..1] of Integer;
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

 UID_ROOT    =0;
 UID_BIN     =3;
 UID_UUCP    =66;
 UID_NOBODY  =65534;

 GID_WHEEL   =0;
 GID_KMEM    =2;
 GID_TTY     =4;
 GID_OPERATOR=5;
 GID_BIN     =7;
 GID_GAMES   =13;
 GID_DIALER  =68;
 GID_NOBODY  =65534;

var
 devmtx:mtx;

 cdevp_free_list:TAILQ_HEAD=(tqh_first:nil;tqh_last:@cdevp_free_list.tqh_first);
 cdevsw_gt_post_list:SLIST_HEAD=(slh_first:nil);

type
 cdevpriv_dtr_t=Procedure(data:Pointer);

function  devfs_get_cdevpriv(datap:PPointer):Integer; external;
function  devfs_set_cdevpriv(priv:Pointer;priv_dtr:cdevpriv_dtr_t):Integer; external;
procedure devfs_clear_cdevpriv(); external;
procedure devfs_fpdrop(fp:p_file); external;

type
 ino_t=Integer;

function  devfs_alloc_cdp_inode():ino_t;   external;
procedure devfs_free_cdp_inode(ino:ino_t); external;

procedure dev_lock();   external;
procedure dev_unlock(); external;
procedure dev_ref(dev:p_cdev); external;
procedure dev_refl(dev:p_cdev); external;
procedure dev_rel(dev:p_cdev); external;

function  dev_refthread(dev:p_cdev;ref:PInteger):p_cdevsw; external;
function  devvn_refthread(vp:p_vnode;devp:pp_cdev;ref:PInteger):p_cdevsw; external;
procedure dev_relthread(dev:p_cdev;ref:Integer); external;

procedure destroy_dev(dev:p_cdev); external;

function  dev2unit(d:p_cdev):Integer;
function  devtoname(dev:p_cdev):PChar;

function  make_dev_credv(flags:Integer;
                         dres:pp_cdev;
                         devsw:p_cdevsw;
                         _unit:Integer;
                         uid:uid_t;
                         gid:gid_t;
                         mode:Integer;
                         fmt:PChar;
                         const Args:Array of const):Integer; register; external;

function  make_dev(devsw:p_cdevsw;
                   _unit:Integer;
                   uid:uid_t;
                   gid:gid_t;
                   mode:Integer;
                   fmt:PChar;
                   const Args:Array of const):p_cdev; register;

function  make_dev_cred(devsw:p_cdevsw;
                        _unit:Integer;
                        uid:uid_t;
                        gid:gid_t;
                        mode:Integer;
                        fmt:PChar;
                        const Args:Array of const):p_cdev; register;

function  make_dev_credf(flags:Integer;
                         devsw:p_cdevsw;
                         _unit:Integer;
                         uid:uid_t;
                         gid:gid_t;
                         mode:Integer;
                         fmt:PChar;
                         const Args:Array of const):p_cdev; register;

function  make_dev_p(flags:Integer;
                     cdev:pp_cdev;
                     devsw:p_cdevsw;
                     uid:uid_t;
                     gid:gid_t;
                     mode:Integer;
                     fmt:PChar;
                     const Args:Array of const):Integer; register;

function  make_dev_alias_v(flags:Integer;
                           cdev:pp_cdev;
                           pdev:p_cdev;
                           fmt:PChar;
                           const Args:Array of const):Integer; register; external;

function  make_dev_alias(pdev:p_cdev;
                         fmt:PChar;
                         const Args:Array of const):p_cdev; register;

function  make_dev_alias_p(flags:Integer;
                           cdev:pp_cdev;
                           pdev:p_cdev;
                           fmt:PChar;
                           const Args:Array of const):Integer; register;

function  make_dev_physpath_alias(flags:Integer;
                                  cdev:pp_cdev;
                                  pdev,old_alias:p_cdev;
                                  physpath:PChar):Integer; external;

function  _nullop():Integer;
function  _eopnotsupp():Integer;
function  _enxio():Integer;
function  _enodev():Integer;
function  _einval():Integer;

implementation

uses
 errno;

function dev2unit(d:p_cdev):Integer; inline;
begin
 Result:=d^.si_drv0;
end;

function devtoname(dev:p_cdev):PChar; inline;
begin
 Result:=dev^.si_name;
end;

function _nullop():Integer;
begin
 Exit(0);
end;

function _eopnotsupp():Integer;
begin
 Exit(EOPNOTSUPP);
end;

function _enxio():Integer;
begin
 Exit(ENXIO);
end;

function _enodev():Integer;
begin
 Exit(ENODEV);
end;

function _einval():Integer;
begin
 Exit(EINVAL);
end;

function make_dev(devsw:p_cdevsw;
                  _unit:Integer;
                  uid:uid_t;
                  gid:gid_t;
                  mode:Integer;
                  fmt:PChar;
                  const Args:Array of const):p_cdev; register; public;
var
 dev:p_cdev;
 res:Integer;
begin
 res:=make_dev_credv(0, @dev, devsw, _unit, uid, gid, mode, fmt, Args);

 Assert((res=0) and (dev<>nil),'make_dev: failed make_dev_credv (error=%d)');

 if (res=0) then
  Exit(dev)
 else
  Exit(nil);
end;

function make_dev_cred(devsw:p_cdevsw;
                       _unit:Integer;
                       uid:uid_t;
                       gid:gid_t;
                       mode:Integer;
                       fmt:PChar;
                       const Args:Array of const):p_cdev; register; public;
var
 dev:p_cdev;
 res:Integer;
begin
 res:=make_dev_credv(0, @dev, devsw, _unit, uid, gid, mode, fmt, Args);

 Assert((res=0) and (dev<>nil),'make_dev_cred: failed make_dev_credv (error=%d)');

 if (res=0) then
  Exit(dev)
 else
  Exit(nil);
end;

function make_dev_credf(flags:Integer;
                        devsw:p_cdevsw;
                        _unit:Integer;
                        uid:uid_t;
                        gid:gid_t;
                        mode:Integer;
                        fmt:PChar;
                        const Args:Array of const):p_cdev; register; public;
var
 dev:p_cdev;
 res:Integer;
begin
 res:=make_dev_credv(flags, @dev, devsw, _unit, uid, gid, mode, fmt, Args);

 Assert((((flags and MAKEDEV_NOWAIT)<>0) and (res=ENOMEM)) or
        (((flags and MAKEDEV_CHECKNAME)<>0) and (res<>ENOMEM)) or
        (res=0),'make_dev_credf: failed make_dev_credv (error=%d)');

 if (res=0) then
  Exit(dev)
 else
  Exit(nil);
end;

function make_dev_p(flags:Integer;
                    cdev:pp_cdev;
                    devsw:p_cdevsw;
                    uid:uid_t;
                    gid:gid_t;
                    mode:Integer;
                    fmt:PChar;
                    const Args:Array of const):Integer; register; public;
var
 res:Integer;
begin
 res:=make_dev_credv(flags, cdev, devsw, 0, uid, gid, mode, fmt, Args);

 Assert((((flags and MAKEDEV_NOWAIT)<>0) and (res=ENOMEM)) or
        (((flags and MAKEDEV_CHECKNAME)<>0) and (res<>ENOMEM)) or
        (res=0),'make_dev_p: failed make_dev_credv (error=%d)');

 Exit(res);
end;

function make_dev_alias(pdev:p_cdev;
                        fmt:PChar;
                        const Args:Array of const):p_cdev; register; public;
var
 dev:p_cdev;
 res:Integer;
begin
 res:=make_dev_alias_v(MAKEDEV_WAITOK, @dev, pdev, fmt, Args);

 Assert((res=0) and (dev<>nil),'make_dev_alias: failed make_dev_alias_v (error=%d)');

 if (res=0) then
  Exit(dev)
 else
  Exit(nil);
end;

function make_dev_alias_p(flags:Integer;
                          cdev:pp_cdev;
                          pdev:p_cdev;
                          fmt:PChar;
                          const Args:Array of const):Integer; register; public;
var
 res:Integer;
begin
 res:=make_dev_alias_v(flags, cdev, pdev, fmt, Args);

 Exit(res);
end;

end.

