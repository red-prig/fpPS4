unit kern_conf;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vmount,
 vfile,
 vuio,
 vnode,
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

 p_vm_paddr_t=Pointer;

 d_open_t       =Function (dev:p_cdev;oflags,devtype:Integer):Integer;
 d_fdopen_t     =Function (dev:p_cdev;oflags:Integer;fp:p_file):Integer;
 d_close_t      =Function (dev:p_cdev;fflag,devtype:Integer):Integer;
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

function dev2unit(d:p_cdev):Integer; inline;
function devtoname(dev:p_cdev):PChar;

type
 cdevpriv_dtr_t=Procedure(data:Pointer);

var
 devmtx:mtx;

 cdevp_free_list:TAILQ_HEAD=(tqh_first:nil;tqh_last:@cdevp_free_list.tqh_first);
 cdevsw_gt_post_list:SLIST_HEAD=(slh_first:nil);

procedure dev_lock();
procedure dev_unlock();
procedure dev_ref(dev:p_cdev);
procedure dev_refl(dev:p_cdev);
procedure dev_rel(dev:p_cdev);

function  dev_refthread(dev:p_cdev;ref:PInteger):p_cdevsw;
function  devvn_refthread(vp:p_vnode;devp:pp_cdev;ref:PInteger):p_cdevsw;
procedure dev_relthread(dev:p_cdev;ref:Integer);

procedure destroy_dev(dev:p_cdev);

function  make_dev_credv(flags:Integer;
                         dres:pp_cdev;
                         devsw:p_cdevsw;
                         _unit:Integer;
                         uid:uid_t;
                         gid:gid_t;
                         mode:Integer;
                         fmt:PChar;
                         const Args:Array of const):Integer; register;

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
                           const Args:Array of const):Integer; register;

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
                                  physpath:PChar):Integer;

implementation

uses
 sysutils,
 errno,
 devfs,
 devfs_devs,
 devfs_vnops,
 vsys_generic,
 kern_synch;

function dev2unit(d:p_cdev):Integer; inline;
begin
 Result:=d^.si_drv0;
end;

function devtoname(dev:p_cdev):PChar; inline;
begin
 Result:=dev^.si_name;
end;

function cdev2priv(c:Pointer):p_cdev_priv; inline;
begin
 Result:=c-ptruint(@p_cdev_priv(nil)^.cdp_c);
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

{
 * Free all the memory collected while the cdev mutex was
 * locked. Since devmtx is after the system map mutex, free() cannot
 * be called immediately and is postponed until cdev mutex can be
 * dropped.
 }
procedure dev_unlock_and_free();
var
 cdp_free:TAILQ_HEAD;
 csw_free:SLIST_HEAD;
 cdp:p_cdev_priv;
 csw:p_cdevsw;
begin
 mtx_assert(devmtx);

 {
  * Make the local copy of the list heads while the dev_mtx is
  * held. Free it later.
  }
 TAILQ_INIT(@cdp_free);
 TAILQ_CONCAT(@cdp_free,@cdevp_free_list,@p_cdev_priv(nil)^.cdp_list);
 csw_free:=cdevsw_gt_post_list;
 SLIST_INIT(@cdevsw_gt_post_list);

 mtx_unlock(devmtx);

 cdp:=TAILQ_FIRST(@cdp_free);
 while (cdp<>nil) do
 begin
  TAILQ_REMOVE(@cdp_free,cdp,@cdp^.cdp_list);
  devfs_free(@cdp^.cdp_c);
  cdp:=TAILQ_FIRST(@cdp_free);
 end;

 csw:=SLIST_FIRST(@csw_free);
 while (csw<>nil) do
 begin
  SLIST_REMOVE_HEAD(@csw_free,@p_cdevsw(nil)^.d_postfree_list);
  FreeMem(csw);
  csw:=SLIST_FIRST(@csw_free);
 end;
end;

procedure dev_free_devlocked(cdev:p_cdev);
var
 cdp:p_cdev_priv;
begin
 mtx_assert(devmtx);
 cdp:=cdev2priv(cdev);
 Assert((cdp^.cdp_flags and CDP_UNREF_DTR)=0,'destroy_dev() was not called after delist_dev(%p)');
 TAILQ_INSERT_HEAD(@cdevp_free_list,cdp,@cdp^.cdp_list);
end;

procedure cdevsw_free_devlocked(csw:p_cdevsw);
begin
 mtx_assert(devmtx);
 SLIST_INSERT_HEAD(@cdevsw_gt_post_list,csw,@csw^.d_postfree_list);
end;

function nullop():Integer;
begin
 Exit(0);
end;

function eopnotsupp():Integer;
begin
 Exit(EOPNOTSUPP);
end;

function enxio():Integer;
begin
 Exit(ENXIO);
end;

function enodev():Integer;
begin
 Exit(ENODEV);
end;

procedure dead_strategy(bp:Pointer);
begin
 //biofinish(bp, nil, ENXIO);
end;

const
 dead_cdevsw:t_cdevsw=(
  d_version    :D_VERSION;
  d_flags      :0;
  d_name       :'dead';
  d_open       :d_open_t(@enxio);
  d_fdopen     :nil;
  d_close      :d_close_t(@enxio);
  d_read       :d_read_t(@enxio);
  d_write      :d_write_t(@enxio);
  d_ioctl      :d_ioctl_t(@enxio);
  d_poll       :d_poll_t(@enodev);
  d_mmap       :d_mmap_t(@enodev);
  d_strategy   :@dead_strategy;
  d_dump       :dumper_t(@enxio);
  d_kqfilter   :d_kqfilter_t(@enxio);
  d_purge      :nil;
  d_mmap_single:d_mmap_single_t(@enodev);
 );

procedure no_strategy(bp:Pointer);
begin
 //biofinish(bp, nil, ENODEV);
end;

function no_poll(dev:p_cdev;events:Integer):Integer;
begin
 Exit(poll_no_poll(events));
end;

function giant_open(dev:p_cdev;oflags,devtype:Integer):Integer;
var
 dsw:p_cdevsw;
 ref,retval:Integer;
begin
 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
  Exit(ENXIO);
 mtx_lock(VFS_Giant);
 retval:=dsw^.d_gianttrick^.d_open(dev, oflags, devtype);
 mtx_unlock(VFS_Giant);
 dev_relthread(dev, ref);
 Exit(retval);
end;

function giant_fdopen(dev:p_cdev;oflags:Integer;fp:p_file):Integer;
var
 dsw:p_cdevsw;
 ref,retval:Integer;
begin
 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
  Exit(ENXIO);
 mtx_lock(VFS_Giant);
 retval:=dsw^.d_gianttrick^.d_fdopen(dev, oflags, fp);
 mtx_unlock(VFS_Giant);
 dev_relthread(dev, ref);
 Exit(retval);
end;

function giant_close(dev:p_cdev;fflag,devtype:Integer):Integer;
var
 dsw:p_cdevsw;
 ref,retval:Integer;
begin
 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
  Exit(ENXIO);
 mtx_lock(VFS_Giant);
 retval:=dsw^.d_gianttrick^.d_close(dev, fflag, devtype);
 mtx_unlock(VFS_Giant);
 dev_relthread(dev, ref);
 Exit(retval);
end;

procedure giant_strategy(bp:Pointer);
var
 dsw:p_cdevsw;
 dev:p_cdev;
 ref:Integer;
begin
 //dev:=bp^.bio_dev;
 //dsw:=dev_refthread(dev, @ref);
 //if (dsw=nil) then
 //begin
 // biofinish(bp, nil, ENXIO);
 // Exit;
 //end;
 //mtx_lock(VFS_Giant);
 //dsw^.d_gianttrick^.d_strategy(bp);
 //mtx_unlock(VFS_Giant);
 //dev_relthread(dev, ref);
end;

function giant_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
var
 dsw:p_cdevsw;
 ref,retval:Integer;
begin
 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
  Exit(ENXIO);
 mtx_lock(VFS_Giant);
 retval:=dsw^.d_gianttrick^.d_ioctl(dev, cmd, data, fflag);
 mtx_unlock(VFS_Giant);
 dev_relthread(dev, ref);
 Exit(retval);
end;

function giant_read(dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
var
 dsw:p_cdevsw;
 ref,retval:Integer;
begin
 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
  Exit(ENXIO);
 mtx_lock(VFS_Giant);
 retval:=dsw^.d_gianttrick^.d_read(dev, uio, ioflag);
 mtx_unlock(VFS_Giant);
 dev_relthread(dev, ref);
 Exit(retval);
end;

function giant_write(dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
var
 dsw:p_cdevsw;
 ref,retval:Integer;
begin
 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
  Exit(ENXIO);
 mtx_lock(VFS_Giant);
 retval:=dsw^.d_gianttrick^.d_write(dev, uio, ioflag);
 mtx_unlock(VFS_Giant);
 dev_relthread(dev, ref);
 Exit(retval);
end;

function giant_poll(dev:p_cdev;events:Integer):Integer;
var
 dsw:p_cdevsw;
 ref,retval:Integer;
begin
 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
  Exit(ENXIO);
 mtx_lock(VFS_Giant);
 retval:=dsw^.d_gianttrick^.d_poll(dev, events);
 mtx_unlock(VFS_Giant);
 dev_relthread(dev, ref);
 Exit(retval);
end;

function giant_kqfilter(dev:p_cdev;kn:Pointer):Integer;
var
 dsw:p_cdevsw;
 ref,retval:Integer;
begin
 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
  Exit(ENXIO);
 mtx_lock(VFS_Giant);
 retval:=dsw^.d_gianttrick^.d_kqfilter(dev, kn);
 mtx_unlock(VFS_Giant);
 dev_relthread(dev, ref);
 Exit(retval);
end;

Function giant_mmap(dev:p_cdev;offset:vm_ooffset_t;paddr:p_vm_paddr_t;nprot:Integer;memattr:p_vm_memattr_t):Integer;
var
 dsw:p_cdevsw;
 ref,retval:Integer;
begin
 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
  Exit(ENXIO);
 mtx_lock(VFS_Giant);
 retval:=dsw^.d_gianttrick^.d_mmap(dev, offset, paddr, nprot, memattr);
 mtx_unlock(VFS_Giant);
 dev_relthread(dev, ref);
 Exit(retval);
end;

Function giant_mmap_single(dev:p_cdev;offset:p_vm_ooffset_t;size:vm_size_t;obj:p_vm_object_t;nprot:Integer):Integer;
var
 dsw:p_cdevsw;
 ref,retval:Integer;
begin
 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
  Exit(ENXIO);
 mtx_lock(VFS_Giant);
 retval:=dsw^.d_gianttrick^.d_mmap_single(dev, offset, size, obj, nprot);
 mtx_unlock(VFS_Giant);
 dev_relthread(dev, ref);
 Exit(retval);
end;

procedure notify(dev:p_cdev;ev:PChar;flags:Integer);
const
 prefix:PChar='cdev=';
var
 data:PChar;
 namelen:Integer;
begin
 //if (cold) then Exit;
 namelen:=strlen(dev^.si_name);
 data:=AllocMem(namelen + sizeof(prefix));
 if (data=nil) then
  Exit;
 Move(prefix^, data^, sizeof(prefix) - 1);
 Move(dev^.si_name^, (data + sizeof(prefix) - 1)^, namelen + 1);
 //devctl_notify_f('DEVFS', 'CDEV', ev, data, mflags);
 FreeMem(data);
end;

procedure notify_create(dev:p_cdev;flags:Integer);
begin
 notify(dev, 'CREATE', flags);
end;

procedure notify_destroy(dev:p_cdev);
begin
 notify(dev, 'DESTROY', MAKEDEV_WAITOK);
end;

function newdev(csw:p_cdevsw;_unit:Integer;si:p_cdev):p_cdev;
var
 si2:p_cdev;
begin
 mtx_assert(devmtx);
 if ((csw^.d_flags and D_NEEDMINOR)<>0) then
 begin
  { We may want to Exitan existing device }
  si2:=LIST_FIRST(@csw^.d_devs);
  while (si2<>nil) do
  begin
   if (dev2unit(si2)=_unit) then
   begin
    dev_free_devlocked(si);
    Exit(si2);
   end;
   si2:=LIST_NEXT(si2,@si2^.si_list);
  end;
 end;
 si^.si_drv0 :=_unit;
 si^.si_devsw:=csw;
 LIST_INSERT_HEAD(@csw^.d_devs,si,@si^.si_list);
 Exit(si);
end;

procedure fini_cdevsw(devsw:p_cdevsw);
var
 gt:p_cdevsw;
begin
 if (devsw^.d_gianttrick<>nil) then
 begin
  gt:=devsw^.d_gianttrick;
  Move(gt^, devsw^, sizeof(t_cdevsw));
  cdevsw_free_devlocked(gt);
  devsw^.d_gianttrick:=nil;
 end;
 devsw^.d_flags:=devsw^.d_flags and (not D_INIT);
end;

function prep_cdevsw(devsw:p_cdevsw;flags:Integer):Integer;
var
 dsw2:p_cdevsw;

 procedure FIXUP(member:PPointer;noop,giant:Pointer); inline;
 begin
  if (member^=nil) then
  begin
   member^:=noop;
  end else
  if ((devsw^.d_flags and D_NEEDGIANT)<>0) then
  begin
   member^:=giant;
  end;
 end;

begin
 mtx_assert(devmtx);
 if ((devsw^.d_flags and D_INIT)<>0) then
  Exit(0);
 if ((devsw^.d_flags and D_NEEDGIANT)<>0) then
 begin
  dev_unlock();
  dsw2:=AllocMem(sizeof(t_cdevsw));
  dev_lock();
  if (dsw2=nil) and ((devsw^.d_flags and D_INIT)=0) then
   Exit(ENOMEM);
 end else
  dsw2:=nil;
 if ((devsw^.d_flags and D_INIT)<>0) then
 begin
  if (dsw2<>nil) then
   cdevsw_free_devlocked(dsw2);
  Exit(0);
 end;

 if (devsw^.d_version<>D_VERSION_03) then
 begin
  Writeln('WARNING: Device driver has wrong version');
  devsw^.d_open       :=d_open_t(@enxio);
  devsw^.d_close      :=d_close_t(@enxio);
  devsw^.d_read       :=d_read_t(@enxio);
  devsw^.d_write      :=d_write_t(@enxio);
  devsw^.d_ioctl      :=d_ioctl_t(@enxio);
  devsw^.d_poll       :=d_poll_t(@enodev);
  devsw^.d_mmap       :=d_mmap_t(@enodev);
  devsw^.d_mmap_single:=d_mmap_single_t(@enodev);
  devsw^.d_strategy   :=@dead_strategy;
  devsw^.d_dump       :=dumper_t(@enxio);
  devsw^.d_kqfilter   :=d_kqfilter_t(@enxio);
 end;

 if ((devsw^.d_flags and D_NEEDGIANT)<>0) then
 begin
  if (devsw^.d_gianttrick=nil) then
  begin
   Move(devsw^, dsw2^, sizeof(t_cdevsw));
   devsw^.d_gianttrick:=dsw2;
   dsw2:=nil;
  end;
 end;

 FIXUP(@devsw^.d_open,        @nullop,      @giant_open);
 FIXUP(@devsw^.d_fdopen,      nil,          @giant_fdopen);
 FIXUP(@devsw^.d_close,       @nullop,      @giant_close);
 FIXUP(@devsw^.d_read,        @enodev,      @giant_read);
 FIXUP(@devsw^.d_write,       @enodev,      @giant_write);
 FIXUP(@devsw^.d_ioctl,       @enodev,      @giant_ioctl);
 FIXUP(@devsw^.d_poll,        @no_poll,     @giant_poll);
 FIXUP(@devsw^.d_mmap,        @enodev,      @giant_mmap);
 FIXUP(@devsw^.d_strategy,    @no_strategy, @giant_strategy);
 FIXUP(@devsw^.d_kqfilter,    @enodev,      @giant_kqfilter);
 FIXUP(@devsw^.d_mmap_single, @enodev,      @giant_mmap_single);

 if (devsw^.d_dump=nil) then devsw^.d_dump:=dumper_t(@enodev);

 LIST_INIT(@devsw^.d_devs);

 devsw^.d_flags:=devsw^.d_flags or D_INIT;

 if (dsw2<>nil) then
  cdevsw_free_devlocked(dsw2);
 Exit(0);
end;

function prep_devname(dev:p_cdev;fmt:PChar;const Args:Array of const):Integer; register;
var
 R:RawByteString;
 len:Integer;
 from,q,s,_to:PChar;
begin
 mtx_assert(devmtx);

 R:=Format(fmt,Args);
 len:=Length(R);

 if (len > sizeof(dev^.__si_namebuf) - 1) then
  Exit(ENAMETOOLONG);

 Move(PChar(R)^,dev^.__si_namebuf,len);

 { Strip leading slashes. }
 from:=@dev^.__si_namebuf;
 while (from='/') do Inc(from);

 _to:=@dev^.__si_namebuf;
 while (from^<>#0) do
 begin
  { Treat multiple sequential slashes as single. }
  while (from[0]='/') and (from[1]='/') do
   Inc(from);
  { Trailing slash is considered invalid. }
  if (from[0]='/') and (from[1]=#0) then
   Exit(EINVAL);
  _to^:=from^;
  //
  Inc(from);
  Inc(_to);
 end;
 _to^:=#0;

 if (dev^.__si_namebuf[0]=#0) then
  Exit(EINVAL);

 { Disallow '.' and '..' components. }
 s:=@dev^.__si_namebuf;
 while true do
 begin
  q:=s;
  while (q^<>'/') and (q^<>#0) do Inc(q);

  if (q - s=1) and (s[0]='.') then
   Exit(EINVAL);
  if (q - s=2) and (s[0]='.') and (s[1]='.') then
   Exit(EINVAL);
  if (q^<>'/') then
   break;
  s:=q + 1;
 end;

 if (devfs_dev_exists(dev^.__si_namebuf)<>0) then
  Exit(EEXIST);

 Exit(0);
end;

function make_dev_credv(flags:Integer;
                        dres:pp_cdev;
                        devsw:p_cdevsw;
                        _unit:Integer;
                        uid:uid_t;
                        gid:gid_t;
                        mode:Integer;
                        fmt:PChar;
                        const Args:Array of const):Integer; register;
var
 dev,dev_new:p_cdev;
 res:Integer;
begin
 Assert(((flags and MAKEDEV_WAITOK)=0) or ((flags and MAKEDEV_NOWAIT)=0),'make_dev_credv: both WAITOK and NOWAIT specified');
 dev_new:=devfs_alloc(flags);
 if (dev_new=nil) then
  Exit(ENOMEM);
 dev_lock();
 res:=prep_cdevsw(devsw, flags);
 if (res<>0) then
 begin
  dev_unlock();
  devfs_free(dev_new);
  Exit(res);
 end;
 dev:=newdev(devsw, _unit, dev_new);
 if ((dev^.si_flags and SI_NAMED)=0) then
 begin
  res:=prep_devname(dev, fmt, Args);
  if (res<>0) then
  begin
   if ((flags and MAKEDEV_CHECKNAME)=0) then
   begin
    Assert(False,'make_dev_credv: bad si_name (error=%d, si_name=%s)');
   end;
   if (dev=dev_new) then
   begin
    LIST_REMOVE(dev,@dev^.si_list);
    dev_unlock();
    devfs_free(dev);
   end else
    dev_unlock();
   Exit(res);
  end;
 end;
 if ((flags and MAKEDEV_REF)<>0) then
  dev_refl(dev);
 if ((flags and MAKEDEV_ETERNAL)<>0) then
  dev^.si_flags:=dev^.si_flags or SI_ETERNAL;
 if ((dev^.si_flags and SI_CHEAPCLONE)<>0) and
    ((dev^.si_flags and SI_NAMED)<>0) then
 begin
  {
   * This is allowed as it removes races and generally
   * simplifies cloning devices.
   * XXX: still ??
   }
  dev_unlock_and_free();
  dres^:=dev;
  Exit(0);
 end;
 Assert((dev^.si_flags and SI_NAMED)=0,'make_dev() by driver %s on pre-existing device (min=%x, name=%s)');
 dev^.si_flags:=dev^.si_flags or SI_NAMED;

 dev^.si_uid :=uid;
 dev^.si_gid :=gid;
 dev^.si_mode:=mode;

 devfs_create(dev);
 //clean_unrhdrl(devfs_inos);
 dev_unlock_and_free();

 notify_create(dev, flags);

 dres^:=dev;
 Exit(0);
end;

function make_dev(devsw:p_cdevsw;
                  _unit:Integer;
                  uid:uid_t;
                  gid:gid_t;
                  mode:Integer;
                  fmt:PChar;
                  const Args:Array of const):p_cdev; register;
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
                       const Args:Array of const):p_cdev; register;
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
                        const Args:Array of const):p_cdev; register;
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
                    const Args:Array of const):Integer; register;
var
 res:Integer;
begin
 res:=make_dev_credv(flags, cdev, devsw, 0, uid, gid, mode, fmt, Args);

 Assert((((flags and MAKEDEV_NOWAIT)<>0) and (res=ENOMEM)) or
        (((flags and MAKEDEV_CHECKNAME)<>0) and (res<>ENOMEM)) or
        (res=0),'make_dev_p: failed make_dev_credv (error=%d)');

 Exit(res);
end;

procedure dev_dependsl(pdev,cdev:p_cdev);
begin
 cdev^.si_parent:=pdev;
 cdev^.si_flags :=cdev^.si_flags or SI_CHILD;
 LIST_INSERT_HEAD(@pdev^.si_children,cdev,@cdev^.si_siblings);
end;

procedure dev_depends(pdev,cdev:p_cdev);
begin
 dev_lock();
 dev_dependsl(pdev, cdev);
 dev_unlock();
end;

function make_dev_alias_v(flags:Integer;
                          cdev:pp_cdev;
                          pdev:p_cdev;
                          fmt:PChar;
                          const Args:Array of const):Integer; register;
var
 dev:p_cdev;
 error:Integer;
begin
 Assert(pdev<>nil,'make_dev_alias_v: pdev is nil');
 Assert(((flags and MAKEDEV_WAITOK)=0) or ((flags and MAKEDEV_NOWAIT)=0),'make_dev_alias_v: both WAITOK and NOWAIT specified');
 Assert((flags and (not (MAKEDEV_WAITOK or MAKEDEV_NOWAIT or MAKEDEV_CHECKNAME)))=0,'make_dev_alias_v: invalid flags specified (flags=%02x)');

 dev:=devfs_alloc(flags);
 if (dev=nil) then
  Exit(ENOMEM);
 dev_lock();
 dev^.si_flags:=dev^.si_flags or SI_ALIAS;
 error:=prep_devname(dev, fmt, Args);
 if (error<>0) then
 begin
  if ((flags and MAKEDEV_CHECKNAME)=0) then
  begin
   Assert(False,'make_dev_alias_v: bad si_name');
  end;
  dev_unlock();
  devfs_free(dev);
  Exit(error);
 end;
 dev^.si_flags:=dev^.si_flags or SI_NAMED;
 devfs_create(dev);
 dev_dependsl(pdev, dev);
 //clean_unrhdrl(devfs_inos);
 dev_unlock();

 notify_create(dev, flags);
 cdev^:=dev;

 Exit(0);
end;

function make_dev_alias(pdev:p_cdev;
                        fmt:PChar;
                        const Args:Array of const):p_cdev; register;
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
                          const Args:Array of const):Integer; register;
var
 res:Integer;
begin
 res:=make_dev_alias_v(flags, cdev, pdev, fmt, Args);

 Exit(res);
end;

function make_dev_physpath_alias(flags:Integer;
                                 cdev:pp_cdev;
                                 pdev,old_alias:p_cdev;
                                 physpath:PChar):Integer;
label
 _ret,
 _out;
var
 devfspath:PChar;
 physpath_len:Integer;
 max_parentpath_len:Integer;
 parentpath_len:Integer;
 devfspathbuf_len:Integer;
 ret:Integer;
 R:RawByteString;
begin
 cdev^:=nil;
 devfspath:=nil;
 physpath_len:=strlen(physpath);
 ret:=EINVAL;
 if (physpath_len=0) then
  goto _out;

 if (strlcomp('id1,', physpath, 4)=0) then
 begin
  Inc(physpath,4);
  Dec(physpath_len,4);
  if (physpath_len=0) then
   goto _out;
 end;

 max_parentpath_len:=SPECNAMELEN - physpath_len - 1;
 parentpath_len:=strlen(pdev^.si_name);
 if (max_parentpath_len < parentpath_len) then
 begin
  ret:=ENAMETOOLONG;
  goto _out;
 end;

 devfspathbuf_len:=physpath_len + 1 + parentpath_len + 1;
 devfspath:=AllocMem(devfspathbuf_len);
 if (devfspath=nil) then
 begin
  ret:=ENOMEM;
  goto _out;
 end;

 R:=Format('%s/%s',[physpath,pdev^.si_name]);
 Move(PChar(R)^,devfspath^,Length(R)+1);

 if (old_alias=nil) then goto _ret;

 if (strcomp(old_alias^.si_name, devfspath)=0) then
 begin
  _ret:
  { Retain the existing alias. }
  cdev^:=old_alias;
  old_alias:=nil;
  ret:=0;
 end else
 begin
  ret:=make_dev_alias_p(flags, cdev, pdev, '%s', [devfspath]);
 end;
_out:
 if (old_alias<>nil) then
  destroy_dev(old_alias);
 if (devfspath<>nil) then
  FreeMem(devfspath);
 Exit(ret);
end;

procedure destroy_devl(dev:p_cdev);
var
 csw:p_cdevsw;
 p:p_cdev_privdata;
 cdp:p_cdev_priv;
begin

 mtx_assert(devmtx);
 Assert((dev^.si_flags and SI_NAMED)<>0,'WARNING: Driver mistake: destroy_dev on %dn');
 Assert((dev^.si_flags and SI_ETERNAL)=0,'WARNING: Driver mistake: destroy_dev on eternal %dn');

 cdp:=cdev2priv(dev);
 if ((cdp^.cdp_flags and CDP_UNREF_DTR)=0) then
 begin
  {
   * Avoid race with dev_rel(), e.g. from the populate
   * loop.  If CDP_UNREF_DTR flag is set, the reference
   * to be dropped at the end of destroy_devl() was
   * already taken by delist_dev_locked().
   }
  dev_refl(dev);

  devfs_destroy(dev);
 end;

 { Remove name marking }
 dev^.si_flags:=dev^.si_flags and (not SI_NAMED);

 { If we are a child, remove us from the parents list }
 if ((dev^.si_flags and SI_CHILD)<>0) then
 begin
  LIST_REMOVE(dev,@dev^.si_siblings);
  dev^.si_flags:=dev^.si_flags and (not SI_CHILD);
 end;

 { Kill our children }
 while (not LIST_EMPTY(@dev^.si_children)) do
 begin
  destroy_devl(LIST_FIRST(@dev^.si_children));
 end;

 { Remove from clone list }
 if ((dev^.si_flags and SI_CLONELIST)<>0) then
 begin
  LIST_REMOVE(dev,@dev^.si_clone);
  dev^.si_flags:=dev^.si_flags and (not SI_CLONELIST);
 end;

 csw:=dev^.si_devsw;
 dev^.si_devsw:=nil; { already nil for SI_ALIAS }
 while (csw<>nil) and (dev^.si_threadcount<>0) do
 begin
  if (csw^.d_purge=nil) then Break;
  csw^.d_purge(dev);
  msleep(csw, @devmtx, PRIBIO, 'devprg', hz div 10);
  if (dev^.si_threadcount<>0) then
   Writeln(Format('Still %lu threads in %sn',[dev^.si_threadcount, devtoname(dev)]));
 end;
 while (dev^.si_threadcount<>0) do
 begin
  { Use unique dummy wait ident }
  msleep(@csw, @devmtx, PRIBIO, 'devdrn', hz div 10);
 end;

 dev_unlock();
 if ((cdp^.cdp_flags and CDP_UNREF_DTR)=0) then
 begin
  { avoid out of order notify events }
  notify_destroy(dev);
 end;
 mtx_lock(cdevpriv_mtx);
 p:=LIST_FIRST(@cdp^.cdp_fdpriv);
 while (p<>nil) do
 begin
  devfs_destroy_cdevpriv(p);
  mtx_lock(cdevpriv_mtx);
  //
  p:=LIST_FIRST(@cdp^.cdp_fdpriv);
 end;
 mtx_unlock(cdevpriv_mtx);
 dev_lock();

 dev^.si_drv1:=nil;
 dev^.si_drv2:=nil;
 FillChar(dev^.__si_namebuf, sizeof(dev^.__si_namebuf),0);

 if ((dev^.si_flags and SI_ALIAS)=0) then
 begin
  { Remove from cdevsw list }
  LIST_REMOVE(dev,@dev^.si_list);

  { If cdevsw has no more struct cdev *'s, clean it }
  if LIST_EMPTY(@csw^.d_devs) then
  begin
   fini_cdevsw(csw);
   wakeup(@csw^.d_devs);
  end;
 end;
 dev^.si_flags :=dev^.si_flags and (not SI_ALIAS);
 cdp^.cdp_flags:=cdp^.cdp_flags and (not CDP_UNREF_DTR);
 Dec(dev^.si_refcount);

 if (dev^.si_refcount > 0) then
  LIST_INSERT_HEAD(@dead_cdevsw.d_devs,dev,@dev^.si_list)
 else
  dev_free_devlocked(dev);
end;

procedure delist_dev_locked(dev:p_cdev);
var
 cdp:p_cdev_priv;
 child:p_cdev;
begin
 mtx_assert(devmtx);
 cdp:=cdev2priv(dev);
 if ((cdp^.cdp_flags and CDP_UNREF_DTR)<>0) then
  Exit;
 cdp^.cdp_flags:=cdp^.cdp_flags or CDP_UNREF_DTR;
 dev_refl(dev);
 devfs_destroy(dev);

 child:=LIST_FIRST(@dev^.si_children);
 while (child<>nil) do
 begin
  delist_dev_locked(child);
  child:=LIST_NEXT(child,@child^.si_siblings);
 end;

 dev_unlock();
 { ensure the destroy event is queued in order }
 notify_destroy(dev);
 dev_lock();
end;

{
 * This function will delist a character device and its children from
 * the directory listing and create a destroy event without waiting
 * for all character device references to go away. At some later point
 * destroy_dev() must be called to complete the character device
 * destruction. After calling this function the character device name
 * can instantly be re-used.
 }
procedure delist_dev(dev:p_cdev);
begin
 dev_lock();
 delist_dev_locked(dev);
 dev_unlock();
end;

procedure destroy_dev(dev:p_cdev);
begin
 dev_lock();
 destroy_devl(dev);
 dev_unlock_and_free();
end;


end.

