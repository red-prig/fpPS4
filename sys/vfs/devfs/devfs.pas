unit devfs;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 vfile,
 vdirent,
 vfs_vnode,
 vmount,
 time,
 kern_mtx,
 kern_sx;

const
 DEVFS_MAGIC=$db0a087a;

{
 * Identifiers.  The ruleset and rule numbers are 16-bit values.  The
 * "rule ID" is a combination of the ruleset and rule number; it
 * should be able to univocally describe a rule in the system.  In
 * this implementation, the upper 16 bits of the rule ID is the
 * ruleset number; the lower 16 bits, the rule number within the
 * aforementioned ruleset.
 }
type
 p_devfs_rnum=^devfs_rnum;
 devfs_rnum=WORD;

 p_devfs_rsnum=^devfs_rsnum;
 devfs_rsnum=WORD;

 p_devfs_rid=^devfs_rid;
 devfs_rid=DWORD;

 ino_t=Integer;

const
 DRC_DSWFLAGS    =$001;
 DRC_PATHPTRN    =$002;
 DEVFS_MAXPTRNLEN=200;

 DRA_BACTS =$001;
 DRA_UID   =$002;
 DRA_GID   =$004;
 DRA_MODE  =$008;
 DRA_INCSET=$010;

 DRB_HIDE  =$001; { Hide entry (DE_WHITEOUT). }
 DRB_UNHIDE=$002; { Unhide entry. }

{
 * Plain DEVFS rule.  This gets shared between kernel and userland
 * verbatim, so it shouldn't contain any pointers or other kernel- or
 * userland-specific values.
 }
type
 p_devfs_rule=^t_devfs_rule;
 t_devfs_rule=packed record
  dr_magic:DWORD;     { Magic number. }
  dr_id   :devfs_rid; { Identifier. }

  {
   * Conditions under which this rule should be applied.  These
   * are ANDed together since OR can be simulated by using
   * multiple rules.  dr_icond determines which of the other
   * variables we should process.
   }
  dr_icond:Integer;

  dr_dswflags:Integer;   { cdevsw flags to match. }

  dr_pathptrn:array[0..DEVFS_MAXPTRNLEN-1] of AnsiChar; { Pattern to match path. }

  {
   * Things to change.  dr_iacts determines which of the other
   * variables we should process.
   }
  dr_iacts:Integer;

  dr_bacts:Integer;    { Boolean (on/off) action. }

  dr_uid   :uid_t;
  dr_gid   :gid_t;
  dr_mode  :mode_t;
  dr_incset:devfs_rsnum;   { Included ruleset. }
 end;

const
{
 * Rule-related ioctls.
 }
 DEVFSIO_RADD    =$C0F04400;
 DEVFSIO_RDEL    =$80044401;
 DEVFSIO_RAPPLY  =$80F04402;
 DEVFSIO_RAPPLYID=$80044403;
 DEVFSIO_RGETNEXT=$C0F04404;

 DEVFSIO_SUSE    =$8002440A;
 DEVFSIO_SAPPLY  =$8002440B;
 DEVFSIO_SGETNEXT=$C002440C;

{ XXX: DEVFSIO_RS_GET_INFO for refcount, active if any, etc. }

//struct componentname;

 DE_WHITEOUT=$01;
 DE_DOT     =$02;
 DE_DOTDOT  =$04;
 DE_DOOMED  =$08;
 DE_COVERED =$10;
 DE_USER    =$20;

 CDP_ACTIVE   =(1 shl 0);
 CDP_SCHED_DTR=(1 shl 1);
 CDP_UNREF_DTR=(1 shl 2);

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
  __si_namebuf  :array[0..SPECNAMELEN] of AnsiChar;
 end;

 t_cdpd_dtr=procedure(P:Pointer);

 p_cdev_privdata=^t_cdev_privdata;
 t_cdev_privdata=packed record
  cdpd_fp  :p_file;
  cdpd_data:Pointer;
  cdpd_dtr :t_cdpd_dtr;
  cdpd_list:LIST_ENTRY; //cdev_privdata
 end;

 pp_devfs_dirent=^p_devfs_dirent;
 p_devfs_dirent=^t_devfs_dirent;

 p_cdev_priv=^t_cdev_priv;
 t_cdev_priv=packed record
  cdp_c:t_cdev;
  cdp_list:TAILQ_ENTRY; //cdev_priv

  cdp_inode:DWORD;
  cdp_flags:DWORD;

  cdp_inuse    :DWORD;
  cdp_maxdirent:DWORD;

  cdp_dirents:pp_devfs_dirent;
  cdp_dirent0:p_devfs_dirent;

  cdp_dtr_list:TAILQ_ENTRY; //cdev_priv

  cdp_dtr_cb    :t_cdpd_dtr;
  cdp_dtr_cb_arg:Pointer;

  cdp_fdpriv:Pointer; //cdev_privdata
 end;

 devfs_dlist_head=TAILQ_HEAD; //devfs_dirent

 t_devfs_dirent=packed record
  de_cdp    :p_cdev_priv;
  de_inode  :Integer;
  de_flags  :Integer;
  de_holdcnt:Integer;
  _align    :Integer;
  de_dirent :p_dirent;
  de_list   :TAILQ_ENTRY;
  de_dlist  :devfs_dlist_head;
  de_dir    :p_devfs_dirent;
  de_links  :Integer;
  de_mode   :mode_t;
  de_uid    :uid_t;
  de_gid    :gid_t;
  //label  *de_label;
  de_atime  :timespec;
  de_mtime  :timespec;
  de_ctime  :timespec;
  de_vnode  :p_vnode;
  de_symlink:PChar;
 end;

 p_devfs_mount=^t_devfs_mount;
 t_devfs_mount=packed record
  dm_idx       :DWORD         ;
  dm_mount     :p_mount       ;
  dm_rootdir   :p_devfs_dirent;
  dm_generation:DWORD         ;
  dm_holdcnt   :Integer       ;
  dm_lock      :t_sx          ;
  dm_ruleset   :devfs_rsnum   ;
 end;

const
 DEVFS_ROOTINO=2;

 DEVFS_DEL_VNLOCKED =$01;
 DEVFS_DEL_NORECURSE=$02;

var
 devfs_rule_depth:DWORD=0;

function  rid2rsn(rid:devfs_rid):devfs_rsnum;
function  rid2rn (rid:devfs_rid):devfs_rnum;
function  mkrid  (rsn:devfs_rsnum;rn:devfs_rnum):devfs_rid;

function  VFSTODEVFS(mp:p_mount):p_devfs_mount; inline;

procedure DEVFS_DE_HOLD(de:p_devfs_dirent); inline;
function  DEVFS_DE_DROP(de:p_devfs_dirent):Boolean; inline;

procedure DEVFS_DMP_HOLD(dmp:p_devfs_mount); inline;
function  DEVFS_DMP_DROP(dmp:p_devfs_mount):Boolean; inline;

function  cdev2priv(c:Pointer):p_cdev_priv; inline;

var
 devmtx:mtx;
 devfs_de_interlock:mtx;

//

type
 p_dirlistent=^t_dirlistent;
 t_dirlistent=packed record
  dir   :PChar;
  refcnt:Integer;
  _align:Integer;
  link  :LIST_ENTRY; //dirlistent
 end;

var
 devfs_dirlist:Pointer=nil; //dirlistent
 dirlist_mtx  :mtx;

 devfs_generation:DWORD=0;

procedure dev_lock();
procedure dev_unlock();
procedure dev_ref(dev:p_cdev);
procedure dev_refl(dev:p_cdev);
procedure dev_rel(dev:p_cdev);

function  devfs_dir_find(path:PChar):Integer;
function  devfs_dir_findent_locked(dir:PChar):p_dirlistent;
procedure devfs_dir_ref(dir:PChar);
procedure devfs_dir_ref_de(dm:p_devfs_mount;de:p_devfs_dirent);
procedure devfs_dir_unref(dir:PChar);
procedure devfs_dir_unref_de(dm:p_devfs_mount;de:p_devfs_dirent);
function  devfs_pathpath(p1,p2:PChar):Integer;
procedure devfs_mtx_init; //MTX_SYSINIT(dirlist_mtx, &dirlist_mtx, "devfs dirlist lock", MTX_DEF);
                          //MTX_SYSINIT(devfs_de_interlock, &devfs_de_interlock, "devfs interlock", MTX_DEF);

implementation

uses
 devfs_devs;

{
 * Identifier manipulators.
 }
function rid2rsn(rid:devfs_rid):devfs_rsnum;
begin
 Result:=rid shr 16;
end;

function rid2rn(rid:devfs_rid):devfs_rnum;
begin
 Result:=rid and $ffff;
end;

function mkrid(rsn:devfs_rsnum;rn:devfs_rnum):devfs_rid;
begin
 Result:=rn or (rsn shl 16);
end;

function VFSTODEVFS(mp:p_mount):p_devfs_mount; inline;
begin
 Result:=mp^.mnt_data;
end;

procedure DEVFS_DE_HOLD(de:p_devfs_dirent); inline;
begin
 Inc(de^.de_holdcnt);
end;

function DEVFS_DE_DROP(de:p_devfs_dirent):Boolean; inline;
begin
 Dec(de^.de_holdcnt);
 Result:=(de^.de_holdcnt=0);
end;

procedure DEVFS_DMP_HOLD(dmp:p_devfs_mount); inline;
begin
 Inc(dmp^.dm_holdcnt);
end;

function DEVFS_DMP_DROP(dmp:p_devfs_mount):Boolean; inline;
begin
 Dec(dmp^.dm_holdcnt);
 Result:=(dmp^.dm_holdcnt=0);
end;

function cdev2priv(c:Pointer):p_cdev_priv; inline;
begin
 Result:=c-ptruint(@p_cdev_priv(nil)^.cdp_c);
end;

//

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

//

{ Exits 1 if the path is in the directory list. }
function devfs_dir_find(path:PChar):Integer;
var
 dle:p_dirlistent;
begin
 mtx_lock(dirlist_mtx);

 dle:=LIST_FIRST(@devfs_dirlist);
 while (dle<>nil) do
 begin
  if (devfs_pathpath(dle^.dir, path)<>0) then
  begin
   mtx_unlock(dirlist_mtx);
   Exit(1);
  end;
  //
  dle:=LIST_NEXT(dle,@dle^.link);
 end;
 mtx_unlock(dirlist_mtx);

 Exit(0);
end;

function devfs_dir_findent_locked(dir:PChar):p_dirlistent;
var
 dle:p_dirlistent;
begin
 mtx_assert(dirlist_mtx);

 dle:=LIST_FIRST(@devfs_dirlist);
 while (dle<>nil) do
 begin
  if (strcomp(dir, dle^.dir)=0) then
   Exit(dle);
  //
  dle:=LIST_NEXT(dle,@dle^.link);
 end;

 Exit(nil);
end;

function strdup(src:PChar):PChar; inline;
var
 i:ptrint;
begin
 i:=strlen(src);
 Result:=AllocMem(i+1);
 Move(src^,Result^,i);
end;

procedure devfs_dir_ref(dir:PChar);
var
 dle,dle_new:p_dirlistent;
begin
 if (dir^=#0) then
  Exit;

 dle_new:=AllocMem(sizeof(t_dirlistent));
 dle_new^.dir:=strdup(dir);
 dle_new^.refcnt:=1;

 mtx_lock(dirlist_mtx);
 dle:=devfs_dir_findent_locked(dir);
 if (dle<>nil) then
 begin
  Inc(dle^.refcnt);
  mtx_unlock(dirlist_mtx);
  FreeMem(dle_new^.dir);
  FreeMem(dle_new);
  Exit;
 end;
 LIST_INSERT_HEAD(@devfs_dirlist,dle_new,@dle_new^.link);
 mtx_unlock(dirlist_mtx);
end;

procedure devfs_dir_ref_de(dm:p_devfs_mount;de:p_devfs_dirent);
var
 dirname:array[0..SPECNAMELEN] of AnsiChar;
 namep:PChar;
begin
 namep:=nil;
 //devfs_vnops
 //namep:=devfs_fqpn(dirname, dm, de, nil);
 Assert(namep<>nil,'devfs_ref_dir_de: nil namep');

 devfs_dir_ref(namep);
end;

procedure devfs_dir_unref(dir:PChar);
var
 dle:p_dirlistent;
begin
 if (dir^=#0) then
  Exit;

 mtx_lock(dirlist_mtx);
 dle:=devfs_dir_findent_locked(dir);
 Assert(dle<>nil, 'devfs_dir_unref: dir %s not referenced');
 Dec(dle^.refcnt);
 Assert(dle^.refcnt >= 0, 'devfs_dir_unref: negative refcnt');
 if (dle^.refcnt=0) then
 begin
  LIST_REMOVE(dle,@dle^.link);
  mtx_unlock(dirlist_mtx);
  FreeMem(dle^.dir);
  FreeMem(dle);
 end else
  mtx_unlock(dirlist_mtx);
end;

procedure devfs_dir_unref_de(dm:p_devfs_mount;de:p_devfs_dirent);
var
 dirname:array[0..SPECNAMELEN] of AnsiChar;
 namep:PChar;
begin
 namep:=nil;
 //devfs_vnops
 //namep:=devfs_fqpn(dirname, dm, de, nil);
 Assert(namep<>nil, 'devfs_unref_dir_de: nil namep');

 devfs_dir_unref(namep);
end;

{ Exits 1 if the path p1 contains the path p2. }
function devfs_pathpath(p1,p2:PChar):Integer;
begin
 repeat
  if (p1^<>p2^) then
  begin
   if (p1^='/') and (p2^=#0) then
    Exit(1)
   else
    Exit(0);
  end else if (p1^=#0) then
   Exit(1);
 until false;
 { NOTREACHED }
end;

procedure devfs_mtx_init;
begin
 mtx_init(devmtx,'cdev');
 mtx_init(dirlist_mtx,'devfs dirlist lock');
 mtx_init(devfs_de_interlock,'devfs interlock');
end;


end.


