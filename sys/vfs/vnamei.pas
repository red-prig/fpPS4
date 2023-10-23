unit vnamei;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vcapability,
 vfcntl,
 vuio,
 vnode,
 kern_thr;

const
{
 * namei operations
 }
 LOOKUP=0; { perform name lookup only }
 CREATE=1; { setup for file creation }
 DELETE=2; { setup for file deletion }
 RENAME=3; { setup for file renaming }
 OPMASK=3; { mask for operation }
{
 * namei operational modifier flags,stored in ni_cnd.flags
 }
 LOCKLEAF  =$0004; { lock inode on return }
 LOCKPARENT=$0008; { want parent vnode returned locked }
 WANTPARENT=$0010; { want parent vnode returned unlocked }
 NOCACHE   =$0020; { name must not be left in cache }
 FOLLOW    =$0040; { follow symbolic links }
 LOCKSHARED=$0100; { Shared lock leaf }
 NOFOLLOW  =$0000; { do not follow symbolic links (pseudo) }
 MODMASK   =$01fc; { mask of operational modifiers }
{
 * Namei parameter descriptors.
 *
 * SAVENAME may be set by either the callers of namei or by VOP_LOOKUP.
 * If the caller of namei sets the flag (for example execve wants to
 * know the name of the program that is being executed),then it must
 * free the buffer. If VOP_LOOKUP sets the flag,then the buffer must
 * be freed by either the commit routine or the VOP_ABORT routine.
 * SAVESTART is set only by the callers of namei. It implies SAVENAME
 * plus the addition of saving the parent directory that contains the
 * name in ni_startdir. It allows repeated calls to lookup for the
 * name being sought. The caller is responsible for releasing the
 * buffer and for vrele'ing ni_startdir.
 }
 RDONLY       =$00000200; { lookup with read-only semantics }
 HASBUF       =$00000400; { has allocated pathname buffer }
 SAVENAME     =$00000800; { save pathname buffer }
 SAVESTART    =$00001000; { save starting directory }
 ISDOTDOT     =$00002000; { current component name is .. }
 MAKEENTRY    =$00004000; { entry is to be added to name cache }
 ISLASTCN     =$00008000; { this is last component of pathname }
 ISSYMLINK    =$00010000; { symlink needs interpretation }
 ISWHITEOUT   =$00020000; { found whiteout }
 DOWHITEOUT   =$00040000; { do whiteouts }
 WILLBEDIR    =$00080000; { new files will be dirs; allow trailing / }
 ISUNICODE    =$00100000; { current component name is unicode}
 ISOPEN       =$00200000; { caller is opening; return a real vnode. }
 NOCROSSMOUNT =$00400000; { do not cross mount points }
 NOMACCHECK   =$00800000; { do not perform MAC checks }
 MPSAFE       =$01000000; { namei() must acquire Giant if needed. }
 GIANTHELD    =$02000000; { namei() is holding giant. }
 AUDITVNODE1  =$04000000; { audit the looked up vnode information }
 AUDITVNODE2  =$08000000; { audit the looked up vnode information }
 TRAILINGSLASH=$10000000; { path ended in a slash }
 PARAMASK     =$1ffffe00; { mask of parameter descriptors }

 NDF_NO_DVP_RELE     =$00000001;
 NDF_NO_DVP_UNLOCK   =$00000002;
 NDF_NO_DVP_PUT      =$00000003;
 NDF_NO_VP_RELE      =$00000004;
 NDF_NO_VP_UNLOCK    =$00000008;
 NDF_NO_VP_PUT       =$0000000c;
 NDF_NO_STARTDIR_RELE=$00000010;
 NDF_NO_FREE_PNBUF   =$00000020;
 NDF_ONLY_PNBUF      =(not NDF_NO_FREE_PNBUF);

type
 p_componentname=^componentname;
 componentname=packed record
  {
   * Arguments to lookup.
   }
  cn_nameiop:Integer;   { namei operation }
  cn_lkflags:Integer;   { Lock flags LK_EXCLUSIVE or LK_SHARED }
  cn_flags  :QWORD;     { flags to namei }
  cn_thread :p_kthread; { thread requesting lookup }
  {
   * Shared between lookup and commit routines.
   }
  cn_pnbuf  :PChar; { pathname buffer }
  cn_nameptr:PChar; { pointer to looked up name }
  cn_namelen:Int64; { length of looked up component }
  cn_consume:Int64; { chars to consume in lookup() }
 end;

{
 * Encapsulation of namei parameters.
 }
 p_nameidata=^t_nameidata;
 t_nameidata=record
  {
   * Arguments to namei/lookup.
   }
  ni_dirp:PChar;                { pathname pointer }
  ni_segflg:uio_seg;            { location of pathname }
  ni_rightsneeded:cap_rights_t; { rights required to look up vnode }
  {
   * Arguments to lookup.
   }
  ni_startdir:p_vnode;       { starting directory }
  ni_rootdir :p_vnode;       { logical root directory }
  ni_topdir  :p_vnode;       { logical top directory }
  ni_dirfd   :Integer;       { starting directory for *at functions }
  ni_strictrelative:Integer; { relative lookup only; no '..' }
  {
   * Results: returned from namei
   }
  ni_baserights:cap_rights_t; { rights the *at base has (or -1) }
  {
   * Results: returned from/manipulated by lookup
   }
  ni_vp :p_vnode;  { vnode of result }
  ni_dvp:p_vnode;  { vnode of intermediate directory }
  {
   * Shared between namei and lookup/commit routines.
   }
  ni_pathlen:QWORD;  { remaining chars in path }
  ni_next   :PChar;  { next location in pathname }
  ni_loopcnt:QWORD;  { count of symlinks encountered }
  {
   * Lookup parameters: this structure describes the subset of
   * information from the nameidata structure that is passed
   * through the VOP interface.
   }
  ni_cnd:componentname;
 end;

function NDHASGIANT(ndp:p_nameidata):Integer; inline;

procedure NDINIT_ALL(
 ndp:p_nameidata;
 op:QWORD;
 flags:QWORD;
 segflg:uio_seg;
 namep:PChar;
 dirfd:Integer;
 startdir:p_vnode;
 rights:cap_rights_t;
 td:p_kthread); inline;

procedure NDINIT(
 ndp:p_nameidata;
 op:QWORD;
 flags:QWORD;
 segflg:uio_seg;
 namep:PChar;
 td:p_kthread); inline;

procedure NDINIT_AT(
 ndp:p_nameidata;
 op:QWORD;
 flags:QWORD;
 segflg:uio_seg;
 namep:PChar;
 dirfd:Integer;
 td:p_kthread); inline;

procedure NDINIT_ATRIGHTS(
 ndp:p_nameidata;
 op:QWORD;
 flags:QWORD;
 segflg:uio_seg;
 namep:PChar;
 dirfd:Integer;
 rights:cap_rights_t;
 td:p_kthread); inline;

procedure NDINIT_ATVP(
 ndp:p_nameidata;
 op:QWORD;
 flags:QWORD;
 segflg:uio_seg;
 namep:PChar;
 vp:p_vnode;
 td:p_kthread); inline;

function  nd_namei(ndp:p_nameidata):Integer;     external;
function  nd_lookup(ndp:p_nameidata):Integer;    external;
procedure NDFREE(ndp:p_nameidata;flags:Integer); external;

implementation

function NDHASGIANT(ndp:p_nameidata):Integer; inline;
begin
 Result:=ord((ndp^.ni_cnd.cn_flags and GIANTHELD)<>0);
end;

procedure NDINIT_ALL(
 ndp:p_nameidata;
 op:QWORD;
 flags:QWORD;
 segflg:uio_seg;
 namep:PChar;
 dirfd:Integer;
 startdir:p_vnode;
 rights:cap_rights_t;
 td:p_kthread); inline;
begin
 ndp^:=Default(t_nameidata);
 ndp^.ni_cnd.cn_nameiop:=op;
 ndp^.ni_cnd.cn_flags  :=flags;
 ndp^.ni_segflg        :=segflg;
 ndp^.ni_dirp          :=namep;
 ndp^.ni_dirfd         :=dirfd;
 ndp^.ni_startdir      :=startdir;
 ndp^.ni_strictrelative:=0;
 ndp^.ni_rightsneeded  :=rights;
 ndp^.ni_baserights    :=0;
 ndp^.ni_cnd.cn_thread :=td;
end;

procedure NDINIT(
 ndp:p_nameidata;
 op:QWORD;
 flags:QWORD;
 segflg:uio_seg;
 namep:PChar;
 td:p_kthread); inline;
begin
 NDINIT_ALL(ndp,op,flags,segflg,namep,AT_FDCWD,nil,0,td);
end;

procedure NDINIT_AT(
 ndp:p_nameidata;
 op:QWORD;
 flags:QWORD;
 segflg:uio_seg;
 namep:PChar;
 dirfd:Integer;
 td:p_kthread); inline;
begin
 NDINIT_ALL(ndp,op,flags,segflg,namep,dirfd,nil,0,td);
end;

procedure NDINIT_ATRIGHTS(
 ndp:p_nameidata;
 op:QWORD;
 flags:QWORD;
 segflg:uio_seg;
 namep:PChar;
 dirfd:Integer;
 rights:cap_rights_t;
 td:p_kthread); inline;
begin
 NDINIT_ALL(ndp,op,flags,segflg,namep,dirfd,nil,rights,td);
end;

procedure NDINIT_ATVP(
 ndp:p_nameidata;
 op:QWORD;
 flags:QWORD;
 segflg:uio_seg;
 namep:PChar;
 vp:p_vnode;
 td:p_kthread); inline;
begin
 NDINIT_ALL(ndp,op,flags,segflg,namep,AT_FDCWD,vp,0,td);
end;


end.

