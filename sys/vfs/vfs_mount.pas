unit vfs_mount;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 vmount,
 vuio,
 vnamei,
 kern_mtx,
 kern_synch,
 kern_thr,
 vfs_vnode,
 vfs_init,
 vfs_lookup,
 vnode_if;

const
 VFS_MOUNTARG_SIZE_MAX=(1024 * 64);

 global_opts:array[0..7] of PChar=(
  'errmsg',
  'fstype',
  'fspath',
  'ro',
  'rw',
  'nosuid',
  'noexec',
  nil
 );

type
 { A memory allocation which must be freed when we are done }
 p_mntaarg=^t_mntaarg;
 t_mntaarg=packed record
  next:SLIST_ENTRY; //mntaarg
 end;

 { The header for the mount arguments }
 p_mntarg=^t_mntarg;
 t_mntarg=packed record
      v:p_iovec;
    len:Integer;
  error:Integer;
   list:SLIST_HEAD; //mntaarg
 end;

procedure vfs_freeopt(opts:p_vfsoptlist;opt:p_vfsopt);
procedure vfs_freeopts(opts:p_vfsoptlist);
procedure vfs_deleteopt(opts:p_vfsoptlist;name:PChar);
function  vfs_isopt_ro(opt:PChar):Integer;
function  vfs_isopt_rw(opt:PChar):Integer;
function  vfs_equalopts(opt1,opt2:PChar):Integer;
procedure vfs_sanitizeopts(opts:p_vfsoptlist);
function  vfs_buildopts(auio:p_uio;options:pp_vfsoptlist):Integer;
procedure vfs_mergeopts(toopts,oldopts:p_vfsoptlist);

procedure vfs_mount_error(mp:p_mount;fmt:PChar;const Args:Array of const); register;
procedure vfs_opterror(opts:p_vfsoptlist;fmt:PChar;const Args:Array of const); register;
function  vfs_filteropt(opts:p_vfsoptlist;legal:ppchar):Integer;
function  vfs_getopt(opts:p_vfsoptlist;name:PChar;buf:PPointer;len:PInteger):Integer;
function  vfs_getopt_pos(opts:p_vfsoptlist;name:PChar):Integer;
function  vfs_getopts(opts:p_vfsoptlist;name:PChar;error:PInteger):PChar;
function  vfs_flagopt(opts:p_vfsoptlist;name:PChar;w:PQWORD;val:QWORD):Integer;
function  vfs_scanopt(opts:p_vfsoptlist;name,fmt:PChar;const Args:Array of const):Integer; register;
function  vfs_setopt(opts:p_vfsoptlist;name,value:PChar;len:Integer):Integer;
function  vfs_setopt_part(opts:p_vfsoptlist;name,value:PChar;len:Integer):Integer;
function  vfs_setopts(opts:p_vfsoptlist;name,value:PChar):Integer;
procedure vfs_mountedfrom(mp:p_mount;from:PChar);

procedure vfs_ref(mp:p_mount); inline;
procedure vfs_rel(mp:p_mount); inline;

procedure mount_init(mp:p_mount);
procedure mount_fini(mp:p_mount);

function  vfs_mount_alloc(vp    :p_vnode;
                          vfsp  :p_vfsconf;
                          fspath:PChar):p_mount;
procedure vfs_mount_destroy(mp:p_mount);

function  vfs_domount(fstype:PChar;         { Filesystem type. }
                      fspath:PChar;         { Mount path. }
                      fsflags:QWORD;        { Flags common to all filesystems. }
                      optlist:pp_vfsoptlist { Options local to the filesystem. }
                     ):Integer;

function  vfs_donmount(fsflags:QWORD;fsoptions:p_uio):Integer;

function  dounmount(mp:p_mount;flags:Integer):Integer;

function  mount_argb(ma:p_mntarg;flag:Integer;name:PChar):p_mntarg;
function  mount_argf(ma:p_mntarg;name,fmt:PChar;const Args:Array of const):p_mntarg; register;
function  mount_argsu(ma:p_mntarg;name:PChar;val:Pointer;len:Integer):p_mntarg;
function  mount_arg(ma:p_mntarg;name:PChar;val:Pointer;len:Integer):p_mntarg;
procedure free_mntarg(ma:p_mntarg);
function  kernel_mount(ma:p_mntarg;flags:QWORD):Integer;

implementation

uses
 errno,
 systm,
 vfs_vnops,
 vfs_subr;

{
 * ---------------------------------------------------------------------
 * Functions for building and sanitizing the mount options
 }

{ Remove one mount option. }
procedure vfs_freeopt(opts:p_vfsoptlist;opt:p_vfsopt);
begin
 TAILQ_REMOVE(opts,opt,@opt^.link);
 FreeMem(opt^.name);
 if (opt^.value<>nil) then
  FreeMem(opt^.value);
 FreeMem(opt);
end;

{ Release all resources related to the mount options. }
procedure vfs_freeopts(opts:p_vfsoptlist);
var
 opt:p_vfsopt;
begin
 while (not TAILQ_EMPTY(opts)) do
 begin
  opt:=TAILQ_FIRST(opts);
  vfs_freeopt(opts, opt);
 end;
 FreeMem(opts);
end;

procedure vfs_deleteopt(opts:p_vfsoptlist;name:PChar);
var
 opt,temp:p_vfsopt;
begin
 if (opts=nil) then
  Exit;
 opt:=TAILQ_FIRST(opts);
 while (opt<>nil) do
 begin
  temp:=TAILQ_NEXT(opt,@opt^.link);
  //
  if (strcomp(opt^.name, name)=0) then
   vfs_freeopt(opts, opt);
  //
  opt:=temp;
 end;
end;

function vfs_isopt_ro(opt:PChar):Integer;
begin
 if (strcomp(opt, 'ro')=0) or
    (strcomp(opt, 'rdonly')=0) or
    (strcomp(opt, 'norw')=0) then
  Exit(1);
 Exit(0);
end;

function vfs_isopt_rw(opt:PChar):Integer;
begin
 if (strcomp(opt, 'rw')=0) or
    (strcomp(opt, 'noro')=0) then
  Exit(1);
 Exit(0);
end;

{
 * Check if options are equal (with or without the 'no' prefix).
 }
function vfs_equalopts(opt1,opt2:PChar):Integer;
var
 p:PChar;
begin
 { 'opt' vs. 'opt' or 'noopt' vs. 'noopt' }
 if (strcomp(opt1, opt2)=0) then
  Exit(1);
 { 'noopt' vs. 'opt' }
 if (strlcomp(opt1, 'no', 2)=0) and
    (strcomp(opt1 + 2, opt2)=0) then
  Exit(1);
 { 'opt' vs. 'noopt' }
 if (strlcomp(opt2, 'no', 2)=0) and
    (strcomp(opt1, opt2 + 2)=0) then
  Exit(1);

 p:=strscan(opt1, '.');
 while (p<>nil) and
       (strlcomp(opt1,opt2,(p+1)-opt1)=0) do
 begin
  Inc(p);
  Inc(opt2,p-opt1);
  opt1:=p;
  { 'foo.noopt' vs. 'foo.opt' }
  if (strlcomp(opt1, 'no', 2)=0) and
     (strcomp(opt1 + 2, opt2)=0) then
   Exit(1);
  { 'foo.opt' vs. 'foo.noopt' }
  if (strlcomp(opt2, 'no', 2)=0) and
     (strcomp(opt1, opt2 + 2)=0) then
   Exit(1);
  //
  p:=strscan(opt1, '.');
 end;
 { 'ro' / 'rdonly' / 'norw' / 'rw' / 'noro' }
 if ((vfs_isopt_ro(opt1)<>0) or
     (vfs_isopt_rw(opt1)<>0)) and
    ((vfs_isopt_ro(opt2)<>0) or
     (vfs_isopt_rw(opt2)<>0)) then
  Exit(1);
 Exit(0);
end;

{
 * If a mount option is specified several times,
 * (with or without the 'no' prefix) only keep
 * the last occurence of it.
 }
procedure vfs_sanitizeopts(opts:p_vfsoptlist);
var
 opt,opt2,tmp:p_vfsopt;
begin
 opt:=TAILQ_LAST(opts);
 while (opt<>nil) do
 begin
  opt2:=TAILQ_PREV(opt,@opt^.link);
  while (opt2<>nil) do
  begin
   if (vfs_equalopts(opt^.name, opt2^.name)<>0) then
   begin
    tmp:=TAILQ_PREV(opt2,@opt2^.link);
    vfs_freeopt(opts, opt2);
    opt2:=tmp;
   end else
   begin
    opt2:=TAILQ_PREV(opt2,@opt2^.link);
   end;
  end;
  //
  opt:=TAILQ_PREV(opt,@opt^.link);
 end;
end;

{
 * Build a linked list of mount options from a struct uio.
 }
function vfs_buildopts(auio:p_uio;options:pp_vfsoptlist):Integer;
label
 bad;
var
 opts:p_vfsoptlist;
 opt:p_vfsopt;
 memused,namelen,optlen:QWORD;
 i,iovcnt:DWORD;
 error:Integer;
begin
 opts:=AllocMem(sizeof(vfsoptlist));
 TAILQ_INIT(opts);
 memused:=0;
 iovcnt:=auio^.uio_iovcnt;
 i:=0;
 while (i < iovcnt) do
 begin
  namelen:=auio^.uio_iov[i].iov_len;
  optlen :=auio^.uio_iov[i + 1].iov_len;
  Inc(memused,sizeof(vfsopt)+optlen+namelen);
  {
   * Avoid consuming too much memory, and attempts to overflow
   * memused.
   }
  if (memused > VFS_MOUNTARG_SIZE_MAX) or
     (optlen  > VFS_MOUNTARG_SIZE_MAX) or
     (namelen > VFS_MOUNTARG_SIZE_MAX) then
  begin
   error:=EINVAL;
   goto bad;
  end;

  opt:=AllocMem(sizeof(vfsopt));
  opt^.name :=AllocMem(namelen);
  opt^.value:=nil;
  opt^.len  :=0;
  opt^.pos  :=i div 2;
  opt^.seen :=0;

  {
   * Do this early, so jumps to 'bad' will free the current
   * option.
   }
  TAILQ_INSERT_TAIL(opts,opt,@opt^.link);

  if (auio^.uio_segflg=UIO_SYSSPACE) then
  begin
   Move(auio^.uio_iov[i].iov_base^, opt^.name^, namelen);
  end else
  begin
   error:=copyin(auio^.uio_iov[i].iov_base, opt^.name, namelen);
   if (error<>0) then
    goto bad;
  end;
  { Ensure names are nil-terminated strings. }
  if (namelen=0) or (opt^.name[namelen - 1]<>#0) then
  begin
   error:=EINVAL;
   goto bad;
  end;
  if (optlen<>0)  then
  begin
   opt^.len:=optlen;
   opt^.value:=AllocMem(optlen);
   if (auio^.uio_segflg=UIO_SYSSPACE) then
   begin
    Move(auio^.uio_iov[i + 1].iov_base^, opt^.value^, optlen);
   end else
   begin
    error:=copyin(auio^.uio_iov[i + 1].iov_base, opt^.value, optlen);
    if (error<>0) then
     goto bad;
   end;
  end;
  //
  Inc(i,2);
 end;
 vfs_sanitizeopts(opts);
 options^:=opts;
 Exit(0);
bad:
 vfs_freeopts(opts);
 Exit(error);
end;

function strdup(src:PChar):PChar; inline;
var
 i:ptrint;
begin
 i:=strlen(src);
 Result:=AllocMem(i+1);
 Move(src^,Result^,i);
end;

{
 * Merge the old mount options with the new ones passed
 * in the MNT_UPDATE case.
 *
 * XXX: This function will keep a 'nofoo' option in the new
 * options.  E.g, if the option's canonical name is 'foo',
 * 'nofoo' ends up in the mount point's active options.
 }
procedure vfs_mergeopts(toopts,oldopts:p_vfsoptlist);
var
 opt,new:p_vfsopt;
begin
 opt:=TAILQ_FIRST(oldopts);
 while (opt<>nil) do
 begin
  new:=AllocMem(sizeof(vfsopt));
  new^.name:=strdup(opt^.name);
  if (opt^.len<>0) then
  begin
   new^.value:=AllocMem(opt^.len);
   Move(opt^.value^, new^.value^, opt^.len);
  end else
   new^.value:=nil;
  new^.len :=opt^.len;
  new^.seen:=opt^.seen;
  TAILQ_INSERT_HEAD(toopts,new,@new^.link);
  //
  opt:=TAILQ_NEXT(opt,@opt^.link);
 end;
 vfs_sanitizeopts(toopts);
end;

{
 * Report errors during filesystem mounting.
 }
procedure vfs_mount_error(mp:p_mount;fmt:PChar;const Args:Array of const); register;
var
 moptlist:p_vfsoptlist;
 error,len:Integer;
 errmsg:PChar;
 S:RawByteString;
begin
 moptlist:=mp^.mnt_optnew;

 error:=vfs_getopt(moptlist, 'errmsg', @errmsg, @len);
 if (error<>0) or
    (errmsg=nil) or
    (len<=0) then
  Exit;

 S:=Format(fmt,Args);
 if (len>(Length(S)+1)) then len:=Length(S)+1;
 Move(PChar(S)^,errmsg^,len);
end;

procedure vfs_opterror(opts:p_vfsoptlist;fmt:PChar;const Args:Array of const); register;
var
 error,len:Integer;
 errmsg:PChar;
 S:RawByteString;
begin
 error:=vfs_getopt(opts, 'errmsg', @errmsg, @len);
 if (error<>0) or
    (errmsg=nil) or
    (len<=0) then
  Exit;

 S:=Format(fmt,Args);
 if (len>(Length(S)+1)) then len:=Length(S)+1;
 Move(PChar(S)^,errmsg^,len);
end;

{
 * Check that no unknown options are given
 }
function vfs_filteropt(opts:p_vfsoptlist;legal:ppchar):Integer;
var
 opt:p_vfsopt;
 errmsg:array[0..254] of Char;
 t:ppchar;
 p,q:pchar;
begin
 opt:=TAILQ_FIRST(opts);
 while (opt<>nil) do
 begin
  p:=opt^.name;
  q:=nil;
  if (p[0]='n') and (p[1]='o') then
   q:=p + 2;
  t:=@global_opts;
  while (t^<>nil) do
  begin
   if (strcomp(t^, p)=0) then
    break;
   if (q<>nil) then
   begin
    if (strcomp(t^, q)=0) then
     break;
   end;
   Inc(t);
  end;
  if (t^<>nil) then
   continue;
  t:=legal;
  while (t^<>nil) do
  begin
   if (strcomp(t^, p)=0) then
    break;
   if (q<>nil) then
   begin
    if (strcomp(t^, q)=0) then
     break;
   end;
   Inc(t);
  end;
  if (t^<>nil) then
   continue;
  errmsg:='mount option is unknown';
  Result:=EINVAL;
  opt:=TAILQ_NEXT(opt,@opt^.link);
 end;
 if (Result<>0) then
 begin
  opt:=TAILQ_FIRST(opts);
  while (opt<>nil) do
  begin
   if (strcomp(opt^.name, 'errmsg')=0) then
   begin
    strlcopy(opt^.value, errmsg, opt^.len);
    break;
   end;
   opt:=TAILQ_NEXT(opt,@opt^.link);
  end;
  if (opt=nil) then
   Writeln(errmsg);
 end;
end;

{
 * Get a mount option by its name.
 *
 * return 0 if the option was found, ENOENT otherwise.
 * If len is non-nil it will be filled with the length
 * of the option. If buf is non-nil, it will be filled
 * with the address of the option.
 }
function vfs_getopt(opts:p_vfsoptlist;name:PChar;buf:PPointer;len:PInteger):Integer;
var
 opt:p_vfsopt;
begin
 Assert(opts<>nil,'vfs_getopt: caller passed opts as nil');

 opt:=TAILQ_FIRST(opts);
 while (opt<>nil) do
 begin
  if (strcomp(name, opt^.name)=0) then
  begin
   opt^.seen:=1;
   if (len<>nil) then
    len^:=opt^.len;
   if (buf<>nil) then
    buf^:=opt^.value;
   Exit(0);
  end;
  //
  opt:=TAILQ_NEXT(opt,@opt^.link);
 end;
 Exit(ENOENT);
end;

function vfs_getopt_pos(opts:p_vfsoptlist;name:PChar):Integer;
var
 opt:p_vfsopt;
begin
 if (opts=nil) then
  Exit(-1);

 opt:=TAILQ_FIRST(opts);
 while (opt<>nil) do
 begin
  if (strcomp(name, opt^.name)=0) then
  begin
   opt^.seen:=1;
   Exit(opt^.pos);
  end;
  //
  opt:=TAILQ_NEXT(opt,@opt^.link);
 end;
 Exit(-1);
end;

function vfs_getopts(opts:p_vfsoptlist;name:PChar;error:PInteger):PChar;
var
 opt:p_vfsopt;
begin
 error^:=0;
 opt:=TAILQ_FIRST(opts);
 while (opt<>nil) do
 begin
  if (strcomp(name, opt^.name)<>0) then
  begin
   opt:=TAILQ_NEXT(opt,@opt^.link);
   continue;
  end;
  opt^.seen:=1;
  if (opt^.len=0) or
     (PChar(opt^.value)[opt^.len - 1]<>#0) then
  begin
   error^:=EINVAL;
   Exit(nil);
  end;
  Exit(opt^.value);
 end;
 error^:=ENOENT;
 Exit(nil);
end;

function vfs_flagopt(opts:p_vfsoptlist;name:PChar;w:PQWORD;val:QWORD):Integer;
var
 opt:p_vfsopt;
begin
 opt:=TAILQ_FIRST(opts);
 while (opt<>nil) do
 begin
  if (strcomp(name, opt^.name)=0) then
  begin
   opt^.seen:=1;
   if (w<>nil) then
    w^:=w^ or val;
   Exit(1);
  end;
  //
  opt:=TAILQ_NEXT(opt,@opt^.link);
 end;
 if (w<>nil) then
  w^:=w^ and (not val);
 Exit(0);
end;

function vfs_scanopt(opts:p_vfsoptlist;name,fmt:PChar;const Args:Array of const):Integer; register;
var
 opt:p_vfsopt;
 S:RawByteString;
begin
 Assert(opts<>nil, 'vfs_getopt: caller passed opts as nil');

 opt:=TAILQ_FIRST(opts);
 while (opt<>nil) do
 begin
  if (strcomp(name, opt^.name)<>0) then
  begin
   opt:=TAILQ_NEXT(opt,@opt^.link);
   continue;
  end;
  opt^.seen:=1;
  if (opt^.len=0) or (opt^.value=nil) then
   Exit(0);
  if (PChar(opt^.value)[opt^.len - 1]<>#0) then
   Exit(0);

  S:=Format(fmt,Args);
  Move(PChar(S)^,opt^.value^,Length(S)+1);
  Exit(0);
 end;
 Exit(0);
end;

function vfs_setopt(opts:p_vfsoptlist;name,value:PChar;len:Integer):Integer;
var
 opt:p_vfsopt;
begin
 opt:=TAILQ_FIRST(opts);
 while (opt<>nil) do
 begin
  if (strcomp(name, opt^.name)<>0) then
  begin
   opt:=TAILQ_NEXT(opt,@opt^.link);
   continue;
  end;
  opt^.seen:=1;
  if (opt^.value=nil) then
   opt^.len:=len
  else
  begin
   if (opt^.len<>len) then
    Exit(EINVAL);
   Move(value^, opt^.value^, len);
  end;
  Exit(0);
 end;
 Exit(ENOENT);
end;

function vfs_setopt_part(opts:p_vfsoptlist;name,value:PChar;len:Integer):Integer;
var
 opt:p_vfsopt;
begin
 opt:=TAILQ_FIRST(opts);
 while (opt<>nil) do
 begin
  if (strcomp(name, opt^.name)<>0) then
  begin
   opt:=TAILQ_NEXT(opt,@opt^.link);
   continue;
  end;
  opt^.seen:=1;
  if (opt^.value=nil) then
   opt^.len:=len
  else
  begin
   if (opt^.len < len) then
    Exit(EINVAL);
   opt^.len:=len;
   Move(value^, opt^.value^, len);
  end;
  Exit(0);
 end;
 Exit(ENOENT);
end;

function strlcpy(dst,src:PChar;size:ptrint):ptrint; inline;
begin
 strlcopy(dst,src,size);
 Result:=strlen(dst);
end;

function vfs_setopts(opts:p_vfsoptlist;name,value:PChar):Integer;
var
 opt:p_vfsopt;
begin
 opt:=TAILQ_FIRST(opts);
 while (opt<>nil) do
 begin
  if (strcomp(name, opt^.name)<>0) then
  begin
   opt:=TAILQ_NEXT(opt,@opt^.link);
   continue;
  end;
  opt^.seen:=1;
  if (opt^.value=nil) then
   opt^.len:=strlen(value) + 1
  else
  if (strlcpy(opt^.value, value, opt^.len) >= opt^.len) then
   Exit(EINVAL);
  Exit(0);
 end;
 Exit(ENOENT);
end;

procedure vfs_mountedfrom(mp:p_mount;from:PChar);
begin
 FillChar(mp^.mnt_stat.f_mntfromname,sizeof(mp^.mnt_stat.f_mntfromname),0);
 strlcopy(@mp^.mnt_stat.f_mntfromname, from, sizeof(mp^.mnt_stat.f_mntfromname));
end;

///

procedure vfs_ref(mp:p_mount); inline;
begin
 MNT_REL(mp);
end;

procedure vfs_rel(mp:p_mount); inline;
begin
 MNT_REL(mp);
end;

procedure mount_init(mp:p_mount);
begin
 mtx_init(mp^.mnt_mtx    ,'struct mount mtx');
 mtx_init(mp^.mnt_explock,'explock');
end;

procedure mount_fini(mp:p_mount);
begin
 mtx_destroy(mp^.mnt_explock);
 mtx_destroy(mp^.mnt_mtx);
end;

function vfs_mount_alloc(vp    :p_vnode;
                         vfsp  :p_vfsconf;
                         fspath:PChar):p_mount;
var
 mp:p_mount;
begin
 mp:=AllocMem(SizeOf(t_mount));
 mount_init(mp);

 TAILQ_INIT(@mp^.mnt_nvnodelist);
 mp^.mnt_nvnodelistsize:=0;
 TAILQ_INIT(@mp^.mnt_activevnodelist);
 mp^.mnt_activevnodelistsize:=0;
 mp^.mnt_ref:=0;
 vfs_busy(mp, MBF_NOWAIT);
 mp^.mnt_op:=vfsp^.vfc_vfsops;
 mp^.mnt_vfc:=vfsp;
 Inc(vfsp^.vfc_refcount); // XXX Unlocked
 mp^.mnt_stat.f_type:=vfsp^.vfc_typenum;
 Inc(mp^.mnt_gen);

 strlcopy(@mp^.mnt_stat.f_fstypename, @vfsp^.vfc_name, MFSNAMELEN);

 mp^.mnt_vnodecovered:=vp;

 strlcopy(@mp^.mnt_stat.f_mntonname, fspath, MNAMELEN);

 mp^.mnt_iosize_max:=DFLTPHYS;

 //mac_mount_init(mp);
 //mac_mount_create(cred, mp);

 mp^.mnt_hashseed:=$FEEDBABE; //arc4rand

 TAILQ_INIT(@mp^.mnt_uppers);
 Result:=mp;
end;

procedure vfs_mount_destroy(mp:p_mount);
begin
 MNT_ILOCK(mp);
 mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_REFEXPIRE;
 if ((mp^.mnt_kern_flag and MNTK_MWAIT)<>0) then
 begin
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag and (not MNTK_MWAIT);
  wakeup(mp);
 end;
 while (mp^.mnt_ref<>0) do
 begin
  msleep(mp, MNT_MTX(mp), PVFS, 'mntref', 0);
 end;
 Assert(mp^.mnt_ref=0,'invalid refcount in the drain path');
 Assert(mp^.mnt_writeopcount=0,'vfs_mount_destroy: nonzero writeopcount');
 Assert(mp^.mnt_secondary_writes=0,'vfs_mount_destroy: nonzero secondary_writes');
 Dec(mp^.mnt_vfc^.vfc_refcount);

 if (not TAILQ_EMPTY(@mp^.mnt_nvnodelist)) then
 begin
  Assert(false,'unmount: dangling vnode');
 end;
 Assert(TAILQ_EMPTY(@mp^.mnt_uppers),'mnt_uppers');
 Assert(mp^.mnt_nvnodelistsize=0,'vfs_mount_destroy: nonzero nvnodelistsize');
 Assert(mp^.mnt_activevnodelistsize=0,'vfs_mount_destroy: nonzero activevnodelistsize');
 Assert(mp^.mnt_lockref=0,'vfs_mount_destroy: nonzero lock refcount');
 MNT_IUNLOCK(mp);

 //mac_mount_destroy(mp);

 if (mp^.mnt_opt<>nil) then
  vfs_freeopts(mp^.mnt_opt);

 mount_fini(mp);
 FreeMem(mp);
end;


{
 * vfs_domount_first(): first file system mount (not update)
 }
function vfs_domount_first(vfsp:p_vfsconf;       { File system type. }
                           fspath:PChar;         { Mount path. }
                           vp:p_vnode;           { Vnode to be covered. }
                           fsflags:QWORD;        { Flags common to all filesystems. }
                           optlist:pp_vfsoptlist { Options local to the filesystem. }
                          ):Integer;
var
 //va:t_vattr;
 mp:p_mount;
 newdp:p_vnode;
 error:Integer;
begin
 mtx_assert(VFS_Giant);
 Assert((fsflags and MNT_UPDATE)=0,'MNT_UPDATE shouldnt be here');

 error:=0;
 //error:=vinvalbuf(vp, V_SAVE, 0, 0);
 //if (error=0) and (vp^.v_type<>VDIR) then
 // error:=ENOTDIR;

 if (error=0) then
 begin
  VI_LOCK(vp);
  if ((vp^.v_iflag and VI_MOUNT)=0) and (vp^.v_mountedhere=nil) then
   vp^.v_iflag:=vp^.v_iflag or VI_MOUNT
  else
   error:=EBUSY;
  VI_UNLOCK(vp);
 end;

 if (error<>0) then
 begin
  vput(vp);
  Exit (error);
 end;
 VOP_UNLOCK(vp, 0);

 { Allocate and initialize the filesystem. }
 mp:=vfs_mount_alloc(vp, vfsp, fspath);
 { XXXMAC: pass to vfs_mount_alloc? }
 mp^.mnt_optnew:=optlist^;
 { Set the mount level flags. }
 mp^.mnt_flag:=(fsflags and (MNT_UPDATEMASK or MNT_ROOTFS or MNT_RDONLY));

 {
  * Mount the filesystem.
  * XXX The final recipients of VFS_MOUNT just overwrite the ndp they
  * get.  No freeing of cn_pnbuf.
  }
 error:=vmount.VFS_MOUNT(mp);
 if (error<>0) then
 begin
  vfs_unbusy(mp);
  vfs_mount_destroy(mp);
  VI_LOCK(vp);
  vp^.v_iflag:=vp^.v_iflag and (not VI_MOUNT);
  VI_UNLOCK(vp);
  vrele(vp);
  Exit (error);
 end;

 if (mp^.mnt_opt<>nil) then
  vfs_freeopts(mp^.mnt_opt);

 mp^.mnt_opt:=mp^.mnt_optnew;

 optlist^:=nil;
 VFS_STATFS(mp,@mp^.mnt_stat);

 {
  * Prevent external consumers of mount options from reading mnt_optnew.
  }
 mp^.mnt_optnew:=nil;

 MNT_ILOCK(mp);
 if ((mp^.mnt_flag and MNT_ASYNC)<>0) and
    ((mp^.mnt_kern_flag and MNTK_NOASYNC)=0) then
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_ASYNC
 else
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag and (not MNTK_ASYNC);
 MNT_IUNLOCK(mp);

 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 //cache_purge(vp);
 VI_LOCK(vp);
 vp^.v_iflag:=vp^.v_iflag and (not VI_MOUNT);
 VI_UNLOCK(vp);
 vp^.v_mountedhere:=mp;
 { Place the new filesystem at the end of the mount list. }
 mtx_lock(mountlist_mtx);
 TAILQ_INSERT_TAIL(@mountlist, mp,@mp^.mnt_list);
 mtx_unlock(mountlist_mtx);
 vfs_event_signal(nil, VQ_MOUNT, 0);
 if (VFS_ROOT(mp,LK_EXCLUSIVE,@newdp)<>0) then
  Assert(false,'mount: lost mount');
 VOP_UNLOCK(newdp, 0);
 VOP_UNLOCK(vp, 0);
 //mountcheckdirs(vp, newdp);
 vrele(newdp);
 //if ((mp^.mnt_flag and MNT_RDONLY)=0)
 // vfs_allocate_syncvnode(mp);
 vfs_unbusy(mp);
 Exit (0);
end;

{
 * vfs_domount_update(): update of mounted file system
 }
function vfs_domount_update(vp:p_vnode;           { Mount point vnode. }
                            fsflags:QWORD;        { Flags common to all filesystems. }
                            optlist:pp_vfsoptlist { Options local to the filesystem. }
                           ):Integer;
label
 _end;
var
 //struct oexport_args oexport;
 //struct export_args export;
 mp:p_mount;
 error,export_error:Integer;
 flag:QWORD;
begin
 mtx_assert(VFS_Giant);
 ASSERT_VOP_ELOCKED(vp, 'vfs_domount_update');
 Assert((fsflags and MNT_UPDATE)<>0, 'MNT_UPDATE should be here');

 if ((vp^.v_vflag and VV_ROOT)=0) then
 begin
  vput(vp);
  Exit(EINVAL);
 end;
 mp:=vp^.v_mount;
 {
  * We only allow the filesystem to be reloaded if it
  * is currently mounted read-only.
  }
 flag:=mp^.mnt_flag;
 if ((fsflags and MNT_RELOAD)<>0) and ((flag and MNT_RDONLY)=0) then
 begin
  vput(vp);
  Exit(EOPNOTSUPP); { Needs translation }
 end;
 {
  * Only privileged root, or (if MNT_USER is set) the user that
  * did the original mount is permitted to update it.
  }
 error:=0;
 //error:=vfs_suser(mp, td);
 if (error<>0) then
 begin
  vput(vp);
  Exit(error);
 end;
 if (vfs_busy(mp, MBF_NOWAIT)<>0) then
 begin
  vput(vp);
  Exit(EBUSY);
 end;
 VI_LOCK(vp);
 if ((vp^.v_iflag and VI_MOUNT)<>0) or (vp^.v_mountedhere<>nil) then
 begin
  VI_UNLOCK(vp);
  vfs_unbusy(mp);
  vput(vp);
  Exit(EBUSY);
 end;
 vp^.v_iflag:=vp^.v_iflag or VI_MOUNT;
 VI_UNLOCK(vp);
 VOP_UNLOCK(vp, 0);

 MNT_ILOCK(mp);
 mp^.mnt_flag:=mp^.mnt_flag and (not MNT_UPDATEMASK);
 mp^.mnt_flag:=(mp^.mnt_flag or fsflags) and (MNT_RELOAD or MNT_FORCE or MNT_UPDATE or
     MNT_SNAPSHOT or MNT_ROOTFS or MNT_UPDATEMASK or MNT_RDONLY);

 if ((mp^.mnt_flag and MNT_ASYNC)=0) then
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag and (not MNTK_ASYNC);
 MNT_IUNLOCK(mp);
 mp^.mnt_optnew:=optlist^;
 vfs_mergeopts(mp^.mnt_optnew, mp^.mnt_opt);

 {
  * Mount the filesystem.
  * XXX The final recipients of VFS_MOUNT just overwrite the ndp they
  * get.  No freeing of cn_pnbuf.
  }
 error:=vmount.VFS_MOUNT(mp);

 export_error:=0;
 if (error=0) then
 begin
  { Process the export option. }
  //if (vfs_copyopt(mp^.mnt_optnew, 'export', @export, sizeof(export))=0) then
  //begin
  // export_error:=vfs_export(mp, @export);
  //end else
  //if (vfs_copyopt(mp^.mnt_optnew, 'export', @oexport, sizeof(oexport))=0) then
  //begin
  // export.ex_flags        :=oexport.ex_flags;
  // export.ex_root         :=oexport.ex_root;
  // export.ex_anon         :=oexport.ex_anon;
  // export.ex_addr         :=oexport.ex_addr;
  // export.ex_addrlen      :=oexport.ex_addrlen;
  // export.ex_mask         :=oexport.ex_mask;
  // export.ex_masklen      :=oexport.ex_masklen;
  // export.ex_indexfile    :=oexport.ex_indexfile;
  // export.ex_numsecflavors:=0;
  // export_error:=vfs_export(mp, @export);
  //end;
 end;

 MNT_ILOCK(mp);
 if (error=0) then
 begin
  mp^.mnt_flag:=mp^.mnt_flag and (not (MNT_UPDATE or MNT_RELOAD or MNT_FORCE or MNT_SNAPSHOT));
 end else
 begin
  {
   * If we fail, restore old mount flags. MNT_QUOTA is special,
   * because it is not part of MNT_UPDATEMASK, but it could have
   * changed in the meantime if quotactl(2) was called.
   * All in all we want current value of MNT_QUOTA, not the old
   * one.
   }
  mp^.mnt_flag:=(mp^.mnt_flag and MNT_QUOTA) or (flag and (not MNT_QUOTA));
 end;
 if ((mp^.mnt_flag and MNT_ASYNC)<>0) and
    ((mp^.mnt_kern_flag and MNTK_NOASYNC)=0) then
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_ASYNC
 else
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag and (not MNTK_ASYNC);
 MNT_IUNLOCK(mp);

 if (error<>0) then
  goto _end;

 if (mp^.mnt_opt<>nil) then
  vfs_freeopts(mp^.mnt_opt);
 mp^.mnt_opt:=mp^.mnt_optnew;
 optlist^:=nil;
 VFS_STATFS(mp, @mp^.mnt_stat);
 {
  * Prevent external consumers of mount options from reading
  * mnt_optnew.
  }
 mp^.mnt_optnew:=nil;

 //if ((mp^.mnt_flag and MNT_RDONLY)=0) then
 // vfs_allocate_syncvnode(mp)
 //else
 // vfs_deallocate_syncvnode(mp);
_end:
 vfs_unbusy(mp);
 VI_LOCK(vp);
 vp^.v_iflag:=vp^.v_iflag and (not VI_MOUNT);
 VI_UNLOCK(vp);
 vrele(vp);

 if (error<>0) then
  Exit(error)
 else
  Exit(export_error);
end;

{
 * vfs_domount(): actually attempt a filesystem mount.
 }
function vfs_domount(fstype:PChar;         { Filesystem type. }
                     fspath:PChar;         { Mount path. }
                     fsflags:QWORD;        { Flags common to all filesystems. }
                     optlist:pp_vfsoptlist { Options local to the filesystem. }
                    ):Integer;
var
 vfsp:p_vfsconf;
 nd:t_nameidata;
 vp:p_vnode;
 pathbuf:PChar;
 error:Integer;
begin
 {
  * Be ultra-paranoid about making sure the type and fspath
  * variables will fit in our mp buffers, including the
  * terminating NUL.
  }
 if (strlen(fstype) >= MFSNAMELEN) or (strlen(fspath) >= MNAMELEN) then
  Exit(ENAMETOOLONG);

 { Load KLDs before we lock the covered vnode to avoid reversals. }
 vfsp:=nil;
 if ((fsflags and MNT_UPDATE)=0) then
 begin
  vfsp:=vfs_byname(fstype);

  if (vfsp=nil) then
   Exit(ENODEV);
 end;

 {
  * Get vnode to be covered or mount point's vnode in case of MNT_UPDATE.
  }
 NDINIT(@nd, LOOKUP, FOLLOW or LOCKLEAF or MPSAFE or AUDITVNODE1, UIO_SYSSPACE, fspath, curkthread);

 error:=nd_namei(@nd);
 if (error<>0) then
  Exit(error);
 if (NDHASGIANT(@nd)=0) then
  mtx_lock(VFS_Giant);
 NDFREE(@nd, NDF_ONLY_PNBUF);
 vp:=nd.ni_vp;
 if ((fsflags and MNT_UPDATE)=0) then
 begin
  pathbuf:=AllocMem(MNAMELEN);
  strcopy(pathbuf, fspath);
  //error:=vn_path_to_global_path(td, vp, pathbuf, MNAMELEN);
  { debug.disablefullpath=1 results in ENODEV }
  //if (error=0) or (error=ENODEV) then
  //begin
   error:=vfs_domount_first(vfsp, pathbuf, vp, fsflags, optlist);
  //end;
  FreeMem(pathbuf);
 end else
  error:=vfs_domount_update(vp, fsflags, optlist);
 mtx_unlock(VFS_Giant);

 ASSERT_VI_UNLOCKED (vp, {$I %LINE%});
 ASSERT_VOP_UNLOCKED(vp, {$I %LINE%});

 Exit(error);
end;

function vfs_donmount(fsflags:QWORD;fsoptions:p_uio):Integer;
label
 bail;
var
 optlist:p_vfsoptlist;
 opt,tmp_opt:p_vfsopt;
 fstype,fspath,errmsg:PChar;
 error,fstypelen,fspathlen,errmsg_len,errmsg_pos:Integer;
begin
 errmsg:=nil;
 fspath:=nil;
 errmsg_len:=0;
 fspathlen :=0;
 errmsg_pos:=-1;

 error:=vfs_buildopts(fsoptions, @optlist);
 if (error<>0) then
  Exit(error);

 if (vfs_getopt(optlist, 'errmsg', @errmsg, @errmsg_len)=0) then
  errmsg_pos:=vfs_getopt_pos(optlist, 'errmsg');

 {
  * We need these two options before the others,
  * and they are mandatory for any filesystem.
  * Ensure they are NUL terminated as well.
  }
 fstypelen:=0;
 error:=vfs_getopt(optlist, 'fstype', @fstype, @fstypelen);
 if (error<>0) or (fstype[fstypelen - 1]<>#0) then
 begin
  error:=EINVAL;
  if (errmsg<>nil) then
   strlcopy(errmsg, 'Invalid fstype', errmsg_len);
  goto bail;
 end;
 fspathlen:=0;
 error:=vfs_getopt(optlist, 'fspath', @fspath, @fspathlen);
 if (error<>0) or (fspath[fspathlen - 1]<>#0) then
 begin
  error:=EINVAL;
  if (errmsg<>nil) then
   strlcopy(errmsg, 'Invalid fspath', errmsg_len);
  goto bail;
 end;

 {
  * We need to see if we have the 'update' option
  * before we call vfs_domount(), since vfs_domount() has special
  * logic based on MNT_UPDATE.  This is very important
  * when we want to update the root filesystem.
  }

 opt:=TAILQ_FIRST(optlist);
 while (opt<>nil) do
 begin
  tmp_opt:=TAILQ_NEXT(opt,@opt^.link);
  //
  if (strcomp(opt^.name, 'update')=0) then
  begin
   fsflags:=fsflags or MNT_UPDATE;
   vfs_freeopt(optlist, opt);
  end else
  if (strcomp(opt^.name, 'async')=0) then
   fsflags:=fsflags or MNT_ASYNC
  else
  if (strcomp(opt^.name, 'force')=0) then
  begin
   fsflags:=fsflags or MNT_FORCE;
   vfs_freeopt(optlist, opt);
  end else
  if (strcomp(opt^.name, 'reload')=0) then
  begin
   fsflags:=fsflags or MNT_RELOAD;
   vfs_freeopt(optlist, opt);
  end else
  if (strcomp(opt^.name, 'multilabel')=0) then
   fsflags:=fsflags or MNT_MULTILABEL
  else
  if (strcomp(opt^.name, 'noasync')=0) then
   fsflags:=fsflags and (not MNT_ASYNC)
  else
  if (strcomp(opt^.name, 'noatime')=0) then
   fsflags:=fsflags or MNT_NOATIME
  else
  if (strcomp(opt^.name, 'atime')=0) then
  begin
   FreeMem(opt^.name);
   opt^.name:=strdup('nonoatime');
  end else
  if (strcomp(opt^.name, 'noclusterr')=0) then
   fsflags:=fsflags or MNT_NOCLUSTERR
  else
  if (strcomp(opt^.name, 'clusterr')=0) then
  begin
   FreeMem(opt^.name);
   opt^.name:=strdup('nonoclusterr');
  end else
  if (strcomp(opt^.name, 'noclusterw')=0) then
   fsflags:=fsflags or MNT_NOCLUSTERW
  else if (strcomp(opt^.name, 'clusterw')=0) then
  begin
   FreeMem(opt^.name);
   opt^.name:=strdup('nonoclusterw');
  end else
  if (strcomp(opt^.name, 'noexec')=0) then
   fsflags:=fsflags or MNT_NOEXEC
  else if (strcomp(opt^.name, 'exec')=0) then
  begin
   FreeMem(opt^.name);
   opt^.name:=strdup('nonoexec');
  end else
  if (strcomp(opt^.name, 'nosuid')=0) then
   fsflags:=fsflags or MNT_NOSUID
  else
  if (strcomp(opt^.name, 'suid')=0) then
  begin
   FreeMem(opt^.name);
   opt^.name:=strdup('nonosuid');
  end else
  if (strcomp(opt^.name, 'nosymfollow')=0) then
   fsflags:=fsflags or MNT_NOSYMFOLLOW
  else
  if (strcomp(opt^.name, 'symfollow')=0) then
  begin
   FreeMem(opt^.name);
   opt^.name:=strdup('nonosymfollow');
  end else
  if (strcomp(opt^.name, 'noro')=0) then
   fsflags:=fsflags and (not MNT_RDONLY)
  else
  if (strcomp(opt^.name, 'rw')=0) then
   fsflags:=fsflags and (not MNT_RDONLY)
  else
  if (strcomp(opt^.name, 'ro')=0) then
   fsflags:=fsflags or MNT_RDONLY
  else
  if (strcomp(opt^.name, 'rdonly')=0) then
  begin
   FreeMem(opt^.name);
   opt^.name:=strdup('ro');
   fsflags:=fsflags or MNT_RDONLY;
  end else
  if (strcomp(opt^.name, 'suiddir')=0) then
   fsflags:=fsflags or MNT_SUIDDIR
  else
  if (strcomp(opt^.name, 'sync')=0) then
   fsflags:=fsflags or MNT_SYNCHRONOUS
  else
  if (strcomp(opt^.name, 'union')=0) then
   fsflags:=fsflags or MNT_UNION;
  //
  opt:=tmp_opt;
 end;

 {
  * Be ultra-paranoid about making sure the type and fspath
  * variables will fit in our mp buffers, including the
  * terminating NUL.
  }
 if (fstypelen >= MFSNAMELEN - 1) or
    (fspathlen >= MNAMELEN - 1) then
 begin
  error:=ENAMETOOLONG;
  goto bail;
 end;

 error:=vfs_domount(fstype, fspath, fsflags, @optlist);
bail:
 { copyout the errmsg }
 if (errmsg_pos<>-1) and
    ((2 * errmsg_pos + 1) < fsoptions^.uio_iovcnt) and
    (errmsg_len > 0) and
    (errmsg<>nil) then
 begin
  if (fsoptions^.uio_segflg=UIO_SYSSPACE) then
  begin
   Move(errmsg^,
       fsoptions^.uio_iov[2 * errmsg_pos + 1].iov_base^,
       fsoptions^.uio_iov[2 * errmsg_pos + 1].iov_len);
  end else
  begin
   copyout(errmsg,
       fsoptions^.uio_iov[2 * errmsg_pos + 1].iov_base,
       fsoptions^.uio_iov[2 * errmsg_pos + 1].iov_len);
  end;
 end;

 if (optlist<>nil) then
  vfs_freeopts(optlist);
 Exit(error);
end;

{
 * Do the actual filesystem unmount.
 }
function dounmount(mp:p_mount;flags:Integer):Integer;
var
 coveredvp,fsrootvp:p_vnode;
 error:Integer;
 async_flag:QWORD;
 mnt_gen_r:Integer;
begin
 mtx_assert(VFS_Giant);

 coveredvp:=mp^.mnt_vnodecovered;
 if (coveredvp<>nil) then
 begin
  mnt_gen_r:=mp^.mnt_gen;
  VI_LOCK(coveredvp);
  vholdl(coveredvp);
  vn_lock(coveredvp, LK_EXCLUSIVE or LK_INTERLOCK or LK_RETRY);
  vdrop(coveredvp);
  {
   * Check for mp being unmounted while waiting for the
   * covered vnode lock.
   }
  if (coveredvp^.v_mountedhere<>mp) or
     (p_mount(coveredvp^.v_mountedhere)^.mnt_gen<>mnt_gen_r) then
  begin
   VOP_UNLOCK(coveredvp, 0);
   Exit(EBUSY);
  end;
 end;
 {
  * Only privileged root, or (if MNT_USER is set) the user that did the
  * original mount is permitted to unmount this filesystem.
  }
 error:=0;
 //error:=vfs_suser(mp, td);
 if (error<>0) then
 begin
  if (coveredvp<>nil) then
   VOP_UNLOCK(coveredvp, 0);
  Exit(error);
 end;

 vn_start_write(nil, @mp, V_WAIT);
 MNT_ILOCK(mp);
 if ((mp^.mnt_kern_flag and MNTK_UNMOUNT)<>0) or
    (not TAILQ_EMPTY(@mp^.mnt_uppers)) then
 begin
  MNT_IUNLOCK(mp);
  if (coveredvp<>nil) then
   VOP_UNLOCK(coveredvp, 0);
  vn_finished_write(mp);
  Exit(EBUSY);
 end;
 mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_UNMOUNT or MNTK_NOINSMNTQ;
 { Allow filesystems to detect that a forced unmount is in progress. }
 if ((flags and MNT_FORCE)<>0) then
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_UNMOUNTF;
 error:=0;
 if (mp^.mnt_lockref<>0) then
 begin
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_DRAINING;
  error:=msleep(@mp^.mnt_lockref, MNT_MTX(mp), PVFS, 'mount drain', 0);
 end;
 MNT_IUNLOCK(mp);
 Assert(mp^.mnt_lockref=0,'%s: invalid lock refcount in the drain path @ %s:%d');
 Assert(error=0,'%s: invalid Exitvalue for msleep in the drain path @ %s:%d');

 //if (mp^.mnt_flag and MNT_EXPUBLIC) then
 // vfs_setpublicfs(nil, nil, nil);

 vfs_msync(mp, MNT_WAIT);
 MNT_ILOCK(mp);
 async_flag:=mp^.mnt_flag and MNT_ASYNC;
 mp^.mnt_flag:=mp^.mnt_flag and (not MNT_ASYNC);
 mp^.mnt_kern_flag:=mp^.mnt_kern_flag and (not MNTK_ASYNC);
 MNT_IUNLOCK(mp);
 //cache_purgevfs(mp); { remove cache entries for this file sys }
 //vfs_deallocate_syncvnode(mp);
 {
  * For forced unmounts, move process cdir/rdir refs on the fs root
  * vnode to the covered vnode.  For non-forced unmounts we want
  * such references to cause an EBUSY error.
  }
 if ((flags and MNT_FORCE)<>0) and
    (VFS_ROOT(mp, LK_EXCLUSIVE, @fsrootvp)=0) then
 begin
  //if (mp^.mnt_vnodecovered<>nil) and
  //   ((mp^.mnt_flag and MNT_IGNORE)=0) then
  // mountcheckdirs(fsrootvp, mp^.mnt_vnodecovered);
  if (fsrootvp=rootvnode) then
  begin
   vrele(rootvnode);
   rootvnode:=nil;
  end;
  vput(fsrootvp);
 end;
 error:=VFS_SYNC(mp, MNT_WAIT);
 if ((mp^.mnt_flag and MNT_RDONLY)<>0) or
    (error=0) or
    ((flags and MNT_FORCE)<>0) then
 begin
  error:=VFS_UNMOUNT(mp, flags);
 end;
 vn_finished_write(mp);
 {
  * If we failed to flush the dirty blocks for this mount point,
  * undo all the cdir/rdir and rootvnode changes we made above.
  * Unless we failed to do so because the device is reporting that
  * it doesn't exist anymore.
  }
 if (error and error<>ENXIO) then
 begin
  if ((flags and MNT_FORCE)<>0) and
     (VFS_ROOT(mp, LK_EXCLUSIVE, @fsrootvp)=0) then
  begin
   //if (mp^.mnt_vnodecovered<>nil) and
   //   ((mp^.mnt_flag and MNT_IGNORE)=0) then
   // mountcheckdirs(mp^.mnt_vnodecovered, fsrootvp);
   if (rootvnode=nil) then
   begin
    rootvnode:=fsrootvp;
    vref(rootvnode);
   end;
   vput(fsrootvp);
  end;
  MNT_ILOCK(mp);
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag and (not MNTK_NOINSMNTQ);
  if ((mp^.mnt_flag and MNT_RDONLY)=0) then
  begin
   //MNT_IUNLOCK(mp);
   //vfs_allocate_syncvnode(mp);
   //MNT_ILOCK(mp);
  end;
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag and (not (MNTK_UNMOUNT or MNTK_UNMOUNTF));
  mp^.mnt_flag:=mp^.mnt_flag or async_flag;
  if ((mp^.mnt_flag and MNT_ASYNC)<>0) and
     ((mp^.mnt_kern_flag and MNTK_NOASYNC)=0) then
   mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_ASYNC;
  if ((mp^.mnt_kern_flag and MNTK_MWAIT)<>0) then
  begin
   mp^.mnt_kern_flag:=mp^.mnt_kern_flag and (not MNTK_MWAIT);
   wakeup(mp);
  end;
  MNT_IUNLOCK(mp);
  if (coveredvp<>nil) then
   VOP_UNLOCK(coveredvp, 0);
  Exit(error);
 end;
 mtx_lock(mountlist_mtx);
 TAILQ_REMOVE(@mountlist,mp,@mp^.mnt_list);
 mtx_unlock(mountlist_mtx);
 if (coveredvp<>nil) then
 begin
  coveredvp^.v_mountedhere:=nil;
  vput(coveredvp);
 end;
 vfs_event_signal(nil, VQ_UNMOUNT, 0);
 vfs_mount_destroy(mp);
 Exit(0);
end;

{
 * Add a boolean argument.
 *
 * flag is the boolean value.
 * name must start with 'no'.
 }
function mount_argb(ma:p_mntarg;flag:Integer;name:PChar):p_mntarg;
begin
 Assert((name[0]='n') and (name[1]='o'),'mount_argb(...,%s): name must start with no');

 Exit(mount_arg(ma, name + (ord(flag<>0)*2), nil, 0));
end;

{
 * Add an argument printf style
 }
function mount_argf(ma:p_mntarg;name,fmt:PChar;const Args:Array of const):p_mntarg; register;
var
 maa:p_mntaarg;
 len:Integer;
 sb:RawByteString;
begin
 if (ma=nil) then
 begin
  ma:=AllocMem(sizeof(t_mntarg));
  SLIST_INIT(@ma^.list);
 end;
 if (ma^.error<>0) then
  Exit(ma);

 ma^.v:=ReAllocMem(ma^.v, sizeof(iovec) * (ma^.len + 2));
 ma^.v[ma^.len].iov_base:=name;
 ma^.v[ma^.len].iov_len :=strlen(name) + 1;
 Inc(ma^.len);

 sb:=Format(fmt,Args);
 len:=Length(sb) + 1;
 maa:=AllocMem(sizeof(t_mntaarg) + len);
 SLIST_INSERT_HEAD(@ma^.list,maa,@maa^.next);
 Move(PChar(sb)^, (maa + 1)^, len);

 ma^.v[ma^.len].iov_base:=maa + 1;
 ma^.v[ma^.len].iov_len :=len;
 Inc(ma^.len);

 Exit(ma);
end;

{
 * Add an argument which is a userland string.
 }
function mount_argsu(ma:p_mntarg;name:PChar;val:Pointer;len:Integer):p_mntarg;
var
 maa:p_mntaarg;
 tbuf:Pointer;
begin
 if (val=nil) then
  Exit(ma);
 if (ma=nil) then
 begin
  ma:=AllocMem(sizeof(t_mntarg));
  SLIST_INIT(@ma^.list);
 end;
 if (ma^.error<>0) then
  Exit(ma);
 maa:=AllocMem(sizeof(t_mntaarg) + len);
 SLIST_INSERT_HEAD(@ma^.list,maa,@maa^.next);
 tbuf:=Pointer(maa + 1);
 ma^.error:=copyinstr(val, tbuf, len, nil);
 Exit(mount_arg(ma, name, tbuf, -1));
end;

{
 * Plain argument.
 *
 * If length is -1, treat value as a C string.
 }
function mount_arg(ma:p_mntarg;name:PChar;val:Pointer;len:Integer):p_mntarg;
begin
 if (ma=nil) then
 begin
  ma:=AllocMem(sizeof(t_mntarg));
  SLIST_INIT(@ma^.list);
 end;
 if (ma^.error<>0) then
  Exit(ma);

 ma^.v:=ReAllocMem(ma^.v, sizeof(iovec) * (ma^.len + 2));
 ma^.v[ma^.len].iov_base:=name;
 ma^.v[ma^.len].iov_len :=strlen(name) + 1;
 Inc(ma^.len);

 ma^.v[ma^.len].iov_base:=val;
 if (len < 0) then
  ma^.v[ma^.len].iov_len:=strlen(val) + 1
 else
  ma^.v[ma^.len].iov_len:=len;
 Inc(ma^.len);

 Exit(ma);
end;

{
 * Free a mntarg structure
 }
procedure free_mntarg(ma:p_mntarg);
var
 maa:p_mntaarg;
begin
 while (not SLIST_EMPTY(@ma^.list)) do
 begin
  maa:=SLIST_FIRST(@ma^.list);
  SLIST_REMOVE_HEAD(@ma^.list,@p_mntaarg(nil)^.next);
  FreeMem(maa);
 end;
 FreeMem(ma^.v);
 FreeMem(ma);
end;

{
 * Mount a filesystem
 }
function kernel_mount(ma:p_mntarg;flags:QWORD):Integer;
var
 auio:t_uio;
 error:Integer;
begin
 Assert(ma<>nil, 'kernel_mount nil ma');
 Assert(ma^.v<>nil, 'kernel_mount nil ma^.v');
 Assert((ma^.len and 1)=0, 'kernel_mount odd ma^.len (%d)');

 auio.uio_iov:=ma^.v;
 auio.uio_iovcnt:=ma^.len;
 auio.uio_segflg:=UIO_SYSSPACE;

 error:=ma^.error;
 if (error=0) then
  error:=vfs_donmount(flags, @auio);
 free_mntarg(ma);
 Exit(error);
end;







end.

