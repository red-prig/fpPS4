unit vfs_mountroot;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vmount;

procedure vfs_mountroot();
function  vfs_mountroot_simple(fstype,fspath,from,opts:PChar):Integer;

implementation

uses
 sysutils,
 errno,
 vuio,
 vnamei,
 vfs_vnode,
 vnode_if,
 vfiledesc,
 vfs_subr,
 vfs_init,
 vfs_mount,
 vfs_syscalls,
 vfs_lookup,
 kern_thr,
 kern_mtx;

function strdup(src:PChar):PChar; inline;
var
 i:ptrint;
begin
 i:=strlen(src);
 Result:=AllocMem(i+1);
 Move(src^,Result^,i);
end;

function strsep(stringp:PPChar;delim:PChar):PChar;
var
 b,e:PChar;
begin
 b:=stringp^;
 if (b=nil) then Exit(nil);

 e:=strpos(b,delim)+strlen(delim);

 if (e^<>#0) then
 begin
  e^:=#0;
  Inc(e);
  stringp^:=e;
 end else
 begin
  stringp^:=nil;
 end;

 Result:=b;
end;

function parse_mountroot_options(ma:p_mntarg;options:PChar):p_mntarg;
var
 p:PChar;
 name,name_arg:PChar;
 val,val_arg:PChar;
 opts:PChar;
begin
 if (options=nil) or (options[0]=#0) then
  Exit(ma);

 p:=strdup(options);
 opts:=p;

 if (opts=nil) then
 begin
  Exit(ma);
 end;

 name:=strsep(@p, ',');
 while (name<>nil) do
 begin
  if (name[0]=#0) then
   break;

  val:=strscan(name, '=');
  if (val<>nil) then
  begin
   val^:=#0;
   Inc(val);
  end;

  name_arg:=strdup(name);
  val_arg:=nil;

  if (val<>nil) then
   val_arg:=strdup(val);

  ma:=mount_arg(ma, name_arg, val_arg, (ord(val_arg<>nil)*(-1)));

  name:=strsep(@p, ',');
 end;

 FreeMem(opts);
 Exit(ma);
end;

procedure set_rootvnode();
begin
 if (VFS_ROOT(TAILQ_FIRST(@mountlist), LK_EXCLUSIVE, @rootvnode)<>0) then
  Assert(False,'Cannot find root vnode');

 VOP_UNLOCK(rootvnode, 0);

 FILEDESC_XLOCK(@fd_table);

 if (fd_table.fd_cdir<>nil) then
  vrele(fd_table.fd_cdir);

 fd_table.fd_cdir:=rootvnode;
 VREF(rootvnode);

 if (fd_table.fd_rdir<>nil) then
  vrele(fd_table.fd_rdir);

 fd_table.fd_rdir:=rootvnode;
 VREF(rootvnode);

 FILEDESC_XUNLOCK(@fd_table);
end;

function vfs_mountroot_devfs(mpp:pp_mount):Integer;
var
 opts:p_vfsoptlist;
 vfsp:p_vfsconf;
 mp:p_mount;
 error:Integer;
begin
 mpp^:=nil;

 vfsp:=vfs_byname('devfs');
 Assert(vfsp<>nil,'Could not find devfs by name');

 if (vfsp=nil) then
  Exit(ENOENT);

 mp:=vfs_mount_alloc(nil, vfsp, '/dev');

 error:=vmount.VFS_MOUNT(mp);
 Assert(error=0,'VFS_MOUNT(devfs) failed');

 if (error<>0) then
  Exit(error);

 opts:=AllocMem(sizeof(vfsoptlist));
 TAILQ_INIT(opts);
 mp^.mnt_opt:=opts;

 mtx_lock(mountlist_mtx);
 TAILQ_INSERT_HEAD(@mountlist,mp,@mp^.mnt_list);
 mtx_unlock(mountlist_mtx);

 mpp^:=mp;
 set_rootvnode();

 error:=kern_symlink('/', 'dev', UIO_SYSSPACE);
 if (error<>0) then
 begin
  Writeln('kern_symlink /dev / returns ',error);
 end;

 Exit(error);
end;


function vfs_mountroot_shuffle(mpdevfs:p_mount):Integer;
var
 nd:t_nameidata;
 mporoot,mpnroot:p_mount;
 vp,vporoot,vpdevfs:p_vnode;
 fspath:PChar;
 error:Integer;
begin
 mpnroot:=TAILQ_NEXT(mpdevfs,@mpdevfs^.mnt_list);

 { Shuffle the mountlist. }
 mtx_lock(mountlist_mtx);
 mporoot:=TAILQ_FIRST(@mountlist);
 TAILQ_REMOVE(@mountlist,mpdevfs,@mpdevfs^.mnt_list);
 if (mporoot<>mpdevfs) then
 begin
  TAILQ_REMOVE(@mountlist,mpnroot,@mpnroot^.mnt_list);
  TAILQ_INSERT_HEAD(@mountlist,mpnroot,@mpnroot^.mnt_list);
 end;
 TAILQ_INSERT_TAIL(@mountlist,mpdevfs,@mpdevfs^.mnt_list);
 mtx_unlock(mountlist_mtx);

 //cache_purgevfs(mporoot);
 //if (mporoot<>mpdevfs) then
 // cache_purgevfs(mpdevfs);

 VFS_ROOT(mporoot, LK_EXCLUSIVE, @vporoot);

 VI_LOCK(vporoot);
 vporoot^.v_iflag:=vporoot^.v_iflag and (not VI_MOUNT);
 VI_UNLOCK(vporoot);
 vporoot^.v_mountedhere:=nil;
 mporoot^.mnt_flag:=mporoot^.mnt_flag and (not MNT_ROOTFS);
 mporoot^.mnt_vnodecovered:=nil;
 vput(vporoot);

 { Set up the new rootvnode, and purge the cache }
 mpnroot^.mnt_vnodecovered:=nil;
 set_rootvnode();
 //cache_purgevfs(rootvnode^.v_mount);

 if (mporoot<>mpdevfs) then
 begin
  { Remount old root under /.mount or /mnt }
  fspath:='/.mount';
  NDINIT(@nd, LOOKUP, FOLLOW or LOCKLEAF, UIO_SYSSPACE, fspath, curkthread);
  error:=nd_namei(@nd);
  if (error<>0) then
  begin
   NDFREE(@nd, NDF_ONLY_PNBUF);
   fspath:='/mnt';
   NDINIT(@nd, LOOKUP, FOLLOW or LOCKLEAF, UIO_SYSSPACE, fspath, curkthread);
   error:=nd_namei(@nd);
  end;
  if (error=0) then
  begin
   vp:=nd.ni_vp;

   if (vp^.v_type=VDIR) then
    error:=0
   else
    error:=ENOTDIR;

   //if (error=0) then
   // error:=vinvalbuf(vp, V_SAVE, 0, 0);

   if (error=0) then
   begin
    //cache_purge(vp);
    mporoot^.mnt_vnodecovered:=vp;
    vp^.v_mountedhere:=mporoot;
    strlcopy(@mporoot^.mnt_stat.f_mntonname, fspath, MNAMELEN);
    VOP_UNLOCK(vp, 0);
   end else
    vput(vp);
  end;
  NDFREE(@nd, NDF_ONLY_PNBUF);

  if (error<>0) then
   Writeln('mountroot: unable to remount previous root under /.mount or /mnt ', error);
 end;

 { Remount devfs under /dev }
 NDINIT(@nd, LOOKUP, FOLLOW or LOCKLEAF, UIO_SYSSPACE, '/dev', curkthread);
 error:=nd_namei(@nd);
 if (error=0) then
 begin
  vp:=nd.ni_vp;

  if (vp^.v_type=VDIR) then
   error:=0
  else
   error:=ENOTDIR;

  //if (error=0) then
  // error:=vinvalbuf(vp, V_SAVE, 0, 0);

  if (error=0) then
  begin
   vpdevfs:=mpdevfs^.mnt_vnodecovered;
   if (vpdevfs<>nil) then
   begin
    //cache_purge(vpdevfs);
    vpdevfs^.v_mountedhere:=nil;
    vrele(vpdevfs);
   end;
   mpdevfs^.mnt_vnodecovered:=vp;
   vp^.v_mountedhere:=mpdevfs;
   VOP_UNLOCK(vp, 0);
  end else
   vput(vp);
 end;
 if (error<>0) then
  Writeln('mountroot: unable to remount devfs under /dev ', error);

 NDFREE(@nd, NDF_ONLY_PNBUF);

 if (mporoot=mpdevfs) then
 begin
  vfs_unbusy(mpdevfs);
  { Unlink the no longer needed /dev/dev ^. / symlink }
  error:=kern_unlink('/dev/dev', UIO_SYSSPACE);
  if (error<>0) then
   Writeln('mountroot: unable to unlink /dev/dev ', error);
 end;

 Exit(0);
end;

function vfs_mountroot_simple(fstype,fspath,from,opts:PChar):Integer;
const
 ERRMSGL=255;
var
 ma:p_mntarg;
 errmsg:RawByteString;
begin
 errmsg:='';
 SetLength(errmsg,ERRMSGL);
 FillChar(PChar(errmsg)^,ERRMSGL,0);

 if (vfs_byname(fstype)=nil) then
 begin
  strlcopy(PChar(errmsg),'unknown file system',ERRMSGL);
  Exit(ENOENT);
 end;

 ma:=nil;
 ma:=mount_arg(ma, 'fstype', fstype, -1);
 ma:=mount_arg(ma, 'fspath', fspath, -1);
 ma:=mount_arg(ma, 'from'  , from  , -1);
 ma:=mount_arg(ma, 'errmsg', PChar(errmsg), ERRMSGL);

 ma:=parse_mountroot_options(ma, opts);
 Result:=kernel_mount(ma, MNT_ROOTFS);
end;

procedure vfs_mountroot();
var
 mp:p_mount;
 //opt:p_vfsoptlist;
 error:Integer;
begin
 mtx_lock(VFS_Giant);

 error:=vfs_mountroot_devfs(@mp);

 //opt:=nil;
 //vfs_domount('fdescfs','/dev/fd',MNT_RDONLY,@opt);

 //if (error=0) then
 //begin
 // error:=vfs_mountroot_shuffle(mp);
 //end;

 mtx_unlock(VFS_Giant);
end;

end.

