unit union_vfsops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 errno,
 unionfs,
 vuio,
 vnode,
 vmount,
 vnamei,
 vstat,
 vfs_mount,
 vfs_vnops,
 vfs_subr,
 vnode_if,
 kern_param,
 kern_mtx,
 kern_thr,
 kern_malloc,
 systm;

function unionfs_domount   (mp:p_mount):Integer;
function unionfs_unmount   (mp:p_mount;mntflags:Integer):Integer;
function unionfs_root      (mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
function unionfs_quotactl  (mp:p_mount;cmd,uid:Integer;arg:Pointer):Integer;
function unionfs_statfs    (mp:p_mount;sbp:p_statfs):Integer;
function unionfs_sync      (mp:p_mount;waitfor:Integer):Integer;
function unionfs_vget      (mp:p_mount;ino:DWORD;flags:Integer;vpp:pp_vnode):Integer;
function unionfs_fhtovp    (mp:p_mount;fidp:p_fid;flags:Integer;vpp:pp_vnode):Integer;
function unionfs_checkexp  (mp:p_mount;nam:Pointer;extflagsp,numsecflavors:Pinteger;secflavors:PPInteger):Integer;
function unionfs_extattrctl(mp:p_mount;cmd:Integer;filename_vp:p_vnode;namespace:Integer;attrname:PChar):Integer;

const
 _unionfs_vfsops:vfsops=(
  vfs_mount     :@unionfs_domount;
  vfs_cmount    :nil;
  vfs_unmount   :@unionfs_unmount;
  vfs_root      :@unionfs_root;
  vfs_quotactl  :@unionfs_quotactl;
  vfs_statfs    :@unionfs_statfs;
  vfs_sync      :@unionfs_sync;
  vfs_vget      :@unionfs_vget;
  vfs_fhtovp    :@unionfs_fhtovp;
  vfs_checkexp  :@unionfs_checkexp;
  vfs_init      :nil; //////@unionfs_init;
  vfs_uninit    :nil; //////@unionfs_uninit;
  vfs_extattrctl:@unionfs_extattrctl;
  vfs_sysctl    :nil;
  vfs_susp_clean:nil;
 );

var
 //VFS_SET(unionfs_vfsops, unionfs, VFCF_LOOPBACK);
 unionfs_vfsconf:vfsconf=(
  vfc_version :VFS_VERSION;
  vfc_name    :'unionfs';
  vfc_vfsops  :@_unionfs_vfsops;
  vfc_typenum :-1;
  vfc_refcount:0;
  vfc_flags   :VFCF_LOOPBACK;
  vfc_opts    :nil;
  vfc_list    :(tqe_next:nil;tqe_prev:nil)
 );

implementation

uses
 union_subr,
 union_vnops;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

function isspace(c: Char):Boolean; inline;
begin
 Result:=(c = ' ') or (c = #9) or (c = #10) or (c = #13) or
         (c = #11) or (c = #12);
end;

function isascii(c: Char):Boolean; inline;
begin
 Result:=Ord(c) < 128;
end;

function isdigit(c: Char):Boolean; inline;
begin
 Result:=(c >= '0') and (c <= '9');
end;

function isalpha(c: Char):Boolean; inline;
begin
 Result:=((c >= 'A') and (c <= 'Z')) or ((c >= 'a') and (c <= 'z'));
end;

function isupper(c: Char):Boolean; inline;
begin
 Result:=(c >= 'A') and (c <= 'Z');
end;

function strtol(nptr:PChar;endptr:PPChar;base:Integer):Int64;
var
 s:PChar;
 acc:QWord;
 c:Char;
 cutoff:QWord;
 neg:Integer;
 any:Integer;
 cutlim:QWord;
 digit:Integer;
begin
 s:=nptr;
 neg:=0;

 repeat
  c:=s^;
  Inc(s);
 until not isspace(c);

 if (c='-') then
 begin
  neg:=1;
  c:=s^;
  Inc(s);
 end else
 if (c='+') then
 begin
  c:=s^;
  Inc(s);
 end;

 if ((base = 0) or (base = 16)) and
    (c = '0') and
    ((s^ = 'x') or (s^ = 'X')) then
 begin
  c:=s[1];
  s:=s + 2;
  base:=16;
 end;

 if (base = 0) then
 begin
  if (c = '0') then
    base:=8
  else
    base:=10;
 end;

 if (neg <> 0) then
  cutoff:=QWord(Low(Int64))
 else
  cutoff:=High(Int64);

 cutlim:=cutoff mod QWord(base);
 cutoff:=cutoff div QWord(base);

 acc:=0;
 any:=0;

 while True do
 begin
  if not isascii(c) then Break;

  if isdigit(c) then
  begin
   digit:=Ord(c) - Ord('0');
  end else
  if isalpha(c) then
  begin
   if isupper(c) then
    digit:=Ord(c) - (Ord('A') - 10)
   else
    digit:=Ord(c) - (Ord('a') - 10);
  end else
  begin
   Break;
  end;

  if (digit >= base) then Break;

  if (any < 0) or (acc > cutoff) or ((acc = cutoff) and (QWord(digit) > cutlim)) then
  begin
   any:=-1
  end else
  begin
   any:=1;
   acc:=acc * QWord(base) + QWord(digit);
  end;

  c:=s^;
  Inc(s);
 end;

 if (any < 0) then
 begin
  if (neg <> 0) then
   acc:=QWord(Low(Int64))
  else
   acc:=High(Int64);
 end else
 if (neg <> 0) then
 begin
  acc:=-acc;
 end;

 if (endptr <> nil) then
 begin
  if (any <> 0) then
   endptr^:=s - 1
  else
   endptr^:=nptr;
 end;

 Result:=acc;
end;

function strcasecmp(str1,str2:PChar):Integer;
begin
 repeat
  if (LowerCase(str1^)<>LowerCase(str2^)) then
  begin
   Exit(ord(LowerCase(str1^))-ord(LowerCase(str2^)));
  end;

  if (str1^=#0) then break;

  Inc(str1);
  Inc(str2);
 until False;

 Result:=0;
end;

{
 * Mount unionfs layer.
}
function unionfs_domount(mp:p_mount):Integer;
var
 error:Integer;
 lowerrootvp:p_vnode;
 upperrootvp:p_vnode;
 ump:p_unionfs_mount;
 td:p_kthread;
 target:PChar;
 tmp:PChar;
 ep:PChar;
 len:Integer;
 done:Int64;
 below:Integer;
 uid:DWORD;
 gid:DWORD;
 udir :Word;
 ufile:Word;
 copymode :unionfs_copymode;
 whitemode:unionfs_whitemode;
 nd:t_nameidata;
 ndp:p_nameidata;
 va:t_vattr;
begin

 LOG_DEBUG('unionfs_mount(mp:=', HexStr(mp));

 error:=0;
 below:=0;
 uid:=0;
 gid:=0;
 udir:=0;
 ufile:=0;
 copymode:=UNIONFS_TRANSPARENT; { default}
 whitemode:=UNIONFS_WHITE_ALWAYS;
 ndp:=@nd;
 td:=curkthread;

 if (mp^.mnt_flag and MNT_ROOTFS)<>0 then
 begin
  vfs_mount_error(mp, 'Cannot union mount root filesystem', []);
  Exit(EOPNOTSUPP);
 end;

 {
  * Update is a no operation.
 }
 if (mp^.mnt_flag and MNT_UPDATE)<>0 then
 begin
  vfs_mount_error(mp, 'unionfs does not support mount update', []);
  Exit(EOPNOTSUPP);
 end;

 {
  * Get argument
 }
 error:=vfs_getopt(mp^.mnt_optnew, 'target', @target, @len);

 if (error<>0) then
 begin
  error:=vfs_getopt(mp^.mnt_optnew, 'from', @target, @len);
 end;

 if (error<>0) OR (target[len - 1]<>#0) then
 begin
  vfs_mount_error(mp, 'Invalid target', []);
  Exit(EINVAL);
 end;

 if (vfs_getopt(mp^.mnt_optnew, 'below', nil, nil)=0) then
 begin
  below:=1;
 end;

 if (vfs_getopt(mp^.mnt_optnew, 'udir', @tmp, nil)=0) then
 begin
  if (tmp<>nil) then
  begin
   udir:=strtol(tmp, @ep, 8);
  end;

  if (tmp=nil) OR (ep^<>#0) then
  begin
   vfs_mount_error(mp, 'Invalid udir', []);
   Exit(EINVAL);
  end;

  udir:=udir and (S_IRWXU or S_IRWXG or S_IRWXO);
 end;

 if (vfs_getopt(mp^.mnt_optnew, 'ufile', @tmp, nil)=0) then
 begin
  if (tmp<>nil) then
  begin
   ufile:=strtol(tmp, @ep, 8);
  end;

  if (tmp=nil) OR (ep^<>#0) then
  begin
   vfs_mount_error(mp, 'Invalid ufile', []);
   Exit(EINVAL);
  end;

  ufile:=ufile and (S_IRWXU or S_IRWXG or S_IRWXO);
 end;

 { check umask, uid and gid}
 if (udir=0) AND (ufile<>0) then
 begin
  udir:=ufile;
 end;

 if (ufile=0) AND (udir<>0) then
 begin
  ufile:=udir;
 end;

 vn_lock(mp^.mnt_vnodecovered, LK_SHARED or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 error:=VOP_GETATTR(mp^.mnt_vnodecovered, @va);

 if (error=0) then
 begin
  if (udir=0) then
  begin
   udir:=va.va_mode;
  end;

  if (ufile=0) then
  begin
   ufile:=va.va_mode;
  end;

  uid:=va.va_uid;
  gid:=va.va_gid;
 end;

 VOP_UNLOCK(mp^.mnt_vnodecovered, LK_RELEASE);
 if (error<>0) then Exit(error);

 //if (mp^.mnt_cred^.cr_ruid=0) then { root only}
 begin
  if (vfs_getopt(mp^.mnt_optnew, 'uid', @tmp, nil)=0) then
  begin
   if (tmp<>nil) then
   begin
    uid:=strtol(tmp, @ep, 10);
   end;
   if (tmp=nil) OR (ep^<>#0) then
   begin
    vfs_mount_error(mp, 'Invalid uid', []);
    Exit(EINVAL);
   end;
  end;
  if (vfs_getopt(mp^.mnt_optnew, 'gid', @tmp, nil)=0) then
  begin
   if (tmp<>nil) then
   begin
    gid:=strtol(tmp, @ep, 10);
   end;
   if (tmp=nil) OR (ep^<>#0) then
   begin
    vfs_mount_error(mp, 'Invalid gid', []);
    Exit(EINVAL);
   end;
  end;
  if (vfs_getopt(mp^.mnt_optnew, 'copymode', @tmp, nil)=0) then
  begin
   if (tmp=nil) then
   begin
    vfs_mount_error(mp, 'Invalid copymode', []);
    Exit(EINVAL);
   end else
   if (strcasecmp(tmp, 'traditional')=0) then
    copymode:=UNIONFS_TRADITIONAL
   else
   if (strcasecmp(tmp, 'transparent')=0) then
    copymode:=UNIONFS_TRANSPARENT
   else
   if (strcasecmp(tmp, 'masquerade')=0) then
    copymode:=UNIONFS_MASQUERADE
   else
   begin
    vfs_mount_error(mp, 'Invalid copymode', []);
    Exit(EINVAL);
   end;
  end;
  if (vfs_getopt(mp^.mnt_optnew, 'whiteout', @tmp, nil)=0) then
  begin
   if (tmp=nil) then
   begin
    vfs_mount_error(mp, 'Invalid whiteout mode', []);
    Exit(EINVAL);
   end else
   if (strcasecmp(tmp, 'always')=0) then
    whitemode:=UNIONFS_WHITE_ALWAYS
   else
   if (strcasecmp(tmp, 'whenneeded')=0) then
    whitemode:=UNIONFS_WHITE_WHENNEEDED
   else
   begin
    vfs_mount_error(mp, 'Invalid whiteout mode', []);
    Exit(EINVAL);
   end;
  end;
 end;

 { If copymode is UNIONFS_TRADITIONAL, uid/gid is mounted user.}
 //if (copymode=UNIONFS_TRADITIONAL) then
 //begin
 // uid:=mp^.mnt_cred^.cr_ruid;
 // gid:=mp^.mnt_cred^.cr_rgid;
 //end;

 LOG_DEBUG('unionfs_mount: ', uid, ' ', gid);
 LOG_DEBUG('unionfs_mount: ', udir, ' ', ufile);
 LOG_DEBUG('unionfs_mount: copymode=', copymode);

 {
  * Find upper node
 }
 NDINIT(ndp, LOOKUP, FOLLOW or LOCKLEAF, UIO_SYSSPACE, target, td);
 error:=nd_namei(ndp);
 if (error<>0) then Exit(error);

 NDFREE(ndp, NDF_ONLY_PNBUF);

 { get root vnodes}
 lowerrootvp:=mp^.mnt_vnodecovered;
 upperrootvp:=ndp^.ni_vp;

 { create unionfs_mount}
 ump:=calloc(sizeof(unionfs_mount));

 {
  * Save reference
 }
 if (below<>0) then
 begin
  VOP_UNLOCK(upperrootvp, LK_RELEASE);
  vn_lock(lowerrootvp, LK_EXCLUSIVE or LK_RETRY, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
  ump^.um_lowervp:=upperrootvp;
  ump^.um_uppervp:=lowerrootvp;
 end else
 begin
  ump^.um_lowervp:=lowerrootvp;
  ump^.um_uppervp:=upperrootvp;
 end;

 ump^.um_rootvp   :=nil;
 ump^.um_uid      :=uid;
 ump^.um_gid      :=gid;
 ump^.um_udir     :=udir;
 ump^.um_ufile    :=ufile;
 ump^.um_copymode :=copymode;
 ump^.um_whitemode:=whitemode;

 MNT_ILOCK(mp);
 if ((p_mount(lowerrootvp^.v_mount)^.mnt_kern_flag and MNTK_MPSAFE)<>0) AND
    ((p_mount(upperrootvp^.v_mount)^.mnt_kern_flag and MNTK_MPSAFE)<>0) then
 begin
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_MPSAFE;
 end;
 MNT_IUNLOCK(mp);
 mp^.mnt_data:=ump;

 {
  * Copy upper layer's RDONLY flag.
 }
 mp^.mnt_flag:=mp^.mnt_flag or (p_mount(ump^.um_uppervp^.v_mount)^.mnt_flag and MNT_RDONLY);

 {
  * Unlock the node
 }
 VOP_UNLOCK(ump^.um_uppervp, LK_RELEASE);

 {
  * Get the unionfs root vnode.
 }
 error:=unionfs_nodeget(mp, ump^.um_uppervp, ump^.um_lowervp, nil, @(ump^.um_rootvp), nil);
 vrele(upperrootvp);

 if (error<>0) then
 begin
  free(ump);
  mp^.mnt_data:=nil;
  Exit(error);
 end;

 {
  * Check mnt_flag
 }
 if ((p_mount(ump^.um_lowervp^.v_mount)^.mnt_flag and MNT_LOCAL)<>0) AND
    ((p_mount(ump^.um_uppervp^.v_mount)^.mnt_flag and MNT_LOCAL)<>0) then
 begin
  mp^.mnt_flag:=mp^.mnt_flag or MNT_LOCAL;
 end;

 {
  * Get new fsid
 }
 vfs_getnewfsid(mp);

 len:=MNAMELEN - 1;
 tmp:=mp^.mnt_stat.f_mntfromname;

 if (below<>0) then
  copystr('<below>:', tmp, len, @done)
 else
  copystr('<above>:', tmp, len, @done);

 len:=len - (done - 1);
 tmp:=tmp + (done - 1);
 copystr(target, tmp, len, nil);

 LOG_DEBUG('unionfs_mount: from ',mp^.mnt_stat.f_mntfromname, ' to ', mp^.mnt_stat.f_mntonname);

 Exit(0);
end;

{
 * Free reference to unionfs layer
}
function unionfs_unmount(mp:p_mount;mntflags:Integer):Integer;
var
 ump    :p_unionfs_mount;
 error  :Integer;
 num    :Integer;
 freeing:Integer;
 flags  :Integer;
begin
 LOG_DEBUG('unionfs_unmount: mp:=', HexStr(mp));

 ump:=MOUNTTOUNIONFSMOUNT(mp);
 flags:=0;

 if (mntflags and MNT_FORCE)<>0 then
 begin
  flags:=flags or FORCECLOSE;
 end;

 { vflush (no need to call vrele)}
 freeing:=0;
 while True do
 begin
  error:=vflush(mp, 1, flags);
  if (error=0) then Break;
  //
  num:=mp^.mnt_nvnodelistsize;
  if (num=freeing) then break;
  freeing:=num;
 end;

 if (error<>0) then Exit(error);

 free(ump);
 mp^.mnt_data:=nil;

 Exit(0);
end;

function unionfs_root(mp:p_mount;flags:Integer;vpp:pp_vnode):Integer;
var
 ump:p_unionfs_mount;
 vp:p_vnode;
begin
 ump:=MOUNTTOUNIONFSMOUNT(mp);
 vp:=ump^.um_rootvp;

 LOG_DEBUG('unionfs_root: rootvp=', HexStr(vp), ' locked=', VOP_ISLOCKED(vp));

 vref(vp);

 if (flags and LK_TYPE_MASK)<>0 then
 begin
  vn_lock(vp, flags, {$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
 end;

 vpp^:=vp;

 Exit(0);
end;

function unionfs_quotactl(mp:p_mount;cmd,uid:Integer;arg:Pointer):Integer;
var
 ump:p_unionfs_mount;
begin
 ump:=MOUNTTOUNIONFSMOUNT(mp);

 {
  * Writing is always performed to upper vnode.
 }
 Exit(VFS_QUOTACTL(ump^.um_uppervp^.v_mount, cmd, uid, arg));
end;

function unionfs_statfs(mp:p_mount;sbp:p_statfs):Integer;
var
 ump:p_unionfs_mount;
 error:Integer;
 mstat:t_statfs;
 lbsize:QWORD;
begin
 ump:=MOUNTTOUNIONFSMOUNT(mp);

 LOG_DEBUG('unionfs_statfs mp=', HexStr(mp),'  lvp=', HexStr(ump^.um_lowervp),'  uvp=', HexStr(ump^.um_uppervp));

 FillChar(mstat,SizeOf(mstat),0);

 error:=VFS_STATFS(ump^.um_lowervp^.v_mount, @mstat);
 if (error<>0) then Exit(error);

 { now copy across the 'interesting' information and fake the rest}
 sbp^.f_blocks:=mstat.f_blocks;
 sbp^.f_files :=mstat.f_files;

 lbsize:=mstat.f_bsize;

 error:=VFS_STATFS(ump^.um_uppervp^.v_mount, @mstat);
 if (error<>0) then Exit(error);

 {
  * The FS type etc is copy from upper vfs.
  * (write able vfs have priority)
 }
 sbp^.f_type  :=mstat.f_type;
 sbp^.f_flags :=mstat.f_flags;
 sbp^.f_bsize :=mstat.f_bsize;
 sbp^.f_iosize:=mstat.f_iosize;

 if (mstat.f_bsize<>lbsize) then
 begin
  sbp^.f_blocks:=(sbp^.f_blocks * lbsize) div mstat.f_bsize;
 end;

 sbp^.f_blocks:=sbp^.f_blocks + mstat.f_blocks;
 sbp^.f_bfree :=mstat.f_bfree;
 sbp^.f_bavail:=mstat.f_bavail;
 sbp^.f_files :=sbp^.f_files + mstat.f_files;
 sbp^.f_ffree :=mstat.f_ffree;

 Exit(0);
end;

function unionfs_sync(mp:p_mount;waitfor:Integer):Integer;
begin
 { nothing to do}
 Exit(0);
end;

function unionfs_vget(mp:p_mount;ino:DWORD;flags:Integer;vpp:pp_vnode):Integer;
begin
 Exit(EOPNOTSUPP);
end;

function unionfs_fhtovp(mp:p_mount;fidp:p_fid;flags:Integer;vpp:pp_vnode):Integer;
begin
 Exit(EOPNOTSUPP);
end;

function unionfs_checkexp(mp:p_mount;nam:Pointer;extflagsp,numsecflavors:Pinteger;secflavors:PPInteger):Integer;
begin
 Exit(EOPNOTSUPP);
end;

function unionfs_extattrctl(mp:p_mount;cmd:Integer;filename_vp:p_vnode;namespace:Integer;attrname:PChar):Integer;
//var
// ump:p_unionfs_mount;
// unp:p_unionfs_node;
begin
 Exit(EOPNOTSUPP);

 //ump:=MOUNTTOUNIONFSMOUNT(mp);
 //unp:=VTOUNIONFS(filename_vp);
 //
 //if (unp^.un_uppervp<>NULLVP) then
 //begin
 // Exit(VFS_EXTATTRCTL(ump^.um_uppervp^.v_mount, cmd, unp^.un_uppervp, namespace, attrname));
 //end else
 //begin
 // Exit(VFS_EXTATTRCTL(ump^.um_lowervp^.v_mount, cmd, unp^.un_lowervp, namespace, attrname));
 //end;
end;


end.

