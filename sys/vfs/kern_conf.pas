unit kern_conf;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 sys_conf,
 kern_param,
 vmount,
 vfile,
 vuio,
 vnode,
 time,
 vm,
 sys_vm_object,
 kern_mtx;

implementation

uses
 sysutils,
 errno,
 devfs_int,
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

procedure dev_lock(); public;
begin
 mtx_lock(devmtx);
end;

procedure dev_unlock(); public;
begin
 mtx_unlock(devmtx);
end;

procedure dev_ref(dev:p_cdev); public;
begin
 mtx_lock(devmtx);
 Inc(dev^.si_refcount);
 mtx_unlock(devmtx);
end;

procedure dev_refl(dev:p_cdev); public;
begin
 mtx_assert(devmtx);
 Inc(dev^.si_refcount);
end;

procedure dev_rel(dev:p_cdev); public;
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
 begin
  devfs_free(dev);
 end;
end;

function dev_refthread(dev:p_cdev;ref:PInteger):p_cdevsw; public;
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

function devvn_refthread(vp:p_vnode;devp:pp_cdev;ref:PInteger):p_cdevsw; public;
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

procedure dev_relthread(dev:p_cdev;ref:Integer); public;
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

procedure dead_strategy(bp:Pointer);
begin
 //biofinish(bp, nil, ENXIO);
end;

const
 dead_cdevsw:t_cdevsw=(
  d_version     :D_VERSION;
  d_flags       :0;
  d_name        :'dead';
  d_open        :d_open_t(@_enxio);
  d_fdopen      :nil;
  d_close       :d_close_t(@_enxio);
  d_read        :d_read_t(@_enxio);
  d_write       :d_write_t(@_enxio);
  d_ioctl       :d_ioctl_t(@_enxio);
  d_poll        :d_poll_t(@_enodev);
  d_mmap        :d_mmap_t(@_enodev);
  d_strategy    :@dead_strategy;
  d_dump        :dumper_t(@_enxio);
  d_kqfilter    :d_kqfilter_t(@_enxio);
  d_purge       :nil;
  d_mmap_single :d_mmap_single_t(@_enodev);
  d_mmap_single2:d_mmap_single2_t(@_enodev);
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
 begin
  Exit(ENXIO);
 end;
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
 begin
  Exit(ENXIO);
 end;
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
 begin
  Exit(ENXIO);
 end;
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
 begin
  Exit(ENXIO);
 end;
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
 begin
  Exit(ENXIO);
 end;
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
 begin
  Exit(ENXIO);
 end;
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
 begin
  Exit(ENXIO);
 end;
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
 begin
  Exit(ENXIO);
 end;
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
 begin
  Exit(ENXIO);
 end;
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
 begin
  Exit(ENXIO);
 end;
 mtx_lock(VFS_Giant);
 retval:=dsw^.d_gianttrick^.d_mmap_single(dev, offset, size, obj, nprot);
 mtx_unlock(VFS_Giant);
 dev_relthread(dev, ref);
 Exit(retval);
end;

Function giant_mmap_single2(dev:p_cdev;offset:p_vm_ooffset_t;size:vm_size_t;obj:p_vm_object_t;nprot:Integer;maxprotp:p_vm_prot_t;flagsp:PInteger):Integer;
var
 dsw:p_cdevsw;
 ref,retval:Integer;
begin
 dsw:=dev_refthread(dev, @ref);
 if (dsw=nil) then
 begin
  Exit(ENXIO);
 end;
 mtx_lock(VFS_Giant);
 retval:=dsw^.d_gianttrick^.d_mmap_single2(dev, offset, size, obj, nprot, maxprotp, flagsp);
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
 if (data=nil) then Exit;
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
 begin
  Exit(0);
 end;
 if ((devsw^.d_flags and D_NEEDGIANT)<>0) then
 begin
  dev_unlock();
  dsw2:=AllocMem(sizeof(t_cdevsw));
  dev_lock();
  if (dsw2=nil) and ((devsw^.d_flags and D_INIT)=0) then
  begin
   Exit(ENOMEM);
  end;
 end else
  dsw2:=nil;
 if ((devsw^.d_flags and D_INIT)<>0) then
 begin
  if (dsw2<>nil) then
  begin
   cdevsw_free_devlocked(dsw2);
  end;
  Exit(0);
 end;

 if (devsw^.d_version<>D_VERSION_03) then
 begin
  Writeln('WARNING: Device driver has wrong version');
  devsw^.d_open        :=d_open_t(@_enxio);
  devsw^.d_close       :=d_close_t(@_enxio);
  devsw^.d_read        :=d_read_t(@_enxio);
  devsw^.d_write       :=d_write_t(@_enxio);
  devsw^.d_ioctl       :=d_ioctl_t(@_enxio);
  devsw^.d_poll        :=d_poll_t(@_enodev);
  devsw^.d_mmap        :=d_mmap_t(@_enodev);
  devsw^.d_mmap_single :=d_mmap_single_t(@_enodev);
  devsw^.d_mmap_single2:=d_mmap_single2_t(@_enodev);
  devsw^.d_strategy    :=@dead_strategy;
  devsw^.d_dump        :=dumper_t(@_enxio);
  devsw^.d_kqfilter    :=d_kqfilter_t(@_enxio);
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

 FIXUP(@devsw^.d_open,         @_nullop,      @giant_open);
 FIXUP(@devsw^.d_fdopen,       nil,           @giant_fdopen);
 FIXUP(@devsw^.d_close,        @_nullop,      @giant_close);
 FIXUP(@devsw^.d_read,         @_enodev,      @giant_read);
 FIXUP(@devsw^.d_write,        @_enodev,      @giant_write);
 FIXUP(@devsw^.d_ioctl,        @_enodev,      @giant_ioctl);
 FIXUP(@devsw^.d_poll,         @no_poll,      @giant_poll);
 FIXUP(@devsw^.d_mmap,         @_enodev,      @giant_mmap);
 FIXUP(@devsw^.d_strategy,     @no_strategy,  @giant_strategy);
 FIXUP(@devsw^.d_kqfilter,     @_enodev,      @giant_kqfilter);
 FIXUP(@devsw^.d_mmap_single,  @_enodev,      @giant_mmap_single);
 FIXUP(@devsw^.d_mmap_single2, @_enodev,      @giant_mmap_single2);

 if (devsw^.d_dump=nil) then devsw^.d_dump:=dumper_t(@_enodev);

 LIST_INIT(@devsw^.d_devs);

 devsw^.d_flags:=devsw^.d_flags or D_INIT;

 if (dsw2<>nil) then
 begin
  cdevsw_free_devlocked(dsw2);
 end;

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
 begin
  Exit(ENAMETOOLONG);
 end;

 Move(PChar(R)^,dev^.__si_namebuf,len);

 { Strip leading slashes. }
 from:=@dev^.__si_namebuf;
 while (from='/') do Inc(from);

 _to:=@dev^.__si_namebuf;
 while (from^<>#0) do
 begin
  { Treat multiple sequential slashes as single. }
  while (from[0]='/') and (from[1]='/') do
  begin
   Inc(from);
  end;
  { Trailing slash is considered invalid. }
  if (from[0]='/') and (from[1]=#0) then
  begin
   Exit(EINVAL);
  end;
  _to^:=from^;
  //
  Inc(from);
  Inc(_to);
 end;
 _to^:=#0;

 if (dev^.__si_namebuf[0]=#0) then
 begin
  Exit(EINVAL);
 end;

 { Disallow '.' and '..' components. }
 s:=@dev^.__si_namebuf;
 while true do
 begin
  q:=s;
  while (q^<>'/') and (q^<>#0) do Inc(q);

  if (q - s=1) and (s[0]='.') then
  begin
   Exit(EINVAL);
  end;
  if (q - s=2) and (s[0]='.') and (s[1]='.') then
  begin
   Exit(EINVAL);
  end;
  if (q^<>'/') then
  begin
   break;
  end;
  s:=q + 1;
 end;

 if (devfs_dev_exists(dev^.__si_namebuf)<>0) then
 begin
  Exit(EEXIST);
 end;

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
                        const Args:Array of const):Integer; register; public name 'make_dev_credv';
var
 dev,dev_new:p_cdev;
 res:Integer;
begin
 Assert(((flags and MAKEDEV_WAITOK)=0) or ((flags and MAKEDEV_NOWAIT)=0),'make_dev_credv: both WAITOK and NOWAIT specified');
 dev_new:=devfs_alloc(flags);
 if (dev_new=nil) then
 begin
  Exit(ENOMEM);
 end;
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
 begin
  dev_refl(dev);
 end;
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
                          const Args:Array of const):Integer; register; public name 'make_dev_alias_v';
var
 dev:p_cdev;
 error:Integer;
begin
 Assert(pdev<>nil,'make_dev_alias_v: pdev is nil');
 Assert(((flags and MAKEDEV_WAITOK)=0) or ((flags and MAKEDEV_NOWAIT)=0),'make_dev_alias_v: both WAITOK and NOWAIT specified');
 Assert((flags and (not (MAKEDEV_WAITOK or MAKEDEV_NOWAIT or MAKEDEV_CHECKNAME)))=0,'make_dev_alias_v: invalid flags specified (flags=%02x)');

 dev:=devfs_alloc(flags);
 if (dev=nil) then
 begin
  Exit(ENOMEM);
 end;
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

function make_dev_physpath_alias(flags:Integer;
                                 cdev:pp_cdev;
                                 pdev,old_alias:p_cdev;
                                 physpath:PChar):Integer; public;
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
  begin
   goto _out;
  end;
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
 begin
  destroy_dev(old_alias);
 end;
 if (devfspath<>nil) then
 begin
  FreeMem(devfspath);
 end;
 Exit(ret);
end;

procedure destroy_devl(dev:p_cdev);
var
 csw:p_cdevsw;
 p:p_cdev_privdata;
 cdp:p_cdev_priv;
begin

 mtx_assert(devmtx);
 Assert((dev^.si_flags and SI_NAMED)<>0 ,'WARNING: Driver mistake: destroy_dev on %dn');
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
 if ((cdp^.cdp_flags and CDP_UNREF_DTR)<>0) then Exit;

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

procedure destroy_dev(dev:p_cdev); public;
begin
 dev_lock();
 destroy_devl(dev);
 dev_unlock_and_free();
end;


end.

