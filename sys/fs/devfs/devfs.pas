unit devfs;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 kern_param,
 vfile,
 vdirent,
 vnode,
 vmount,
 time,
 kern_conf,
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

 DE_WHITEOUT=$01;
 DE_DOT     =$02;
 DE_DOTDOT  =$04;
 DE_DOOMED  =$08;
 DE_COVERED =$10;
 DE_USER    =$20;

 CDP_ACTIVE   =(1 shl 0);
 CDP_SCHED_DTR=(1 shl 1);
 CDP_UNREF_DTR=(1 shl 2);

type
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

  cdp_fdpriv:LIST_HEAD; //cdev_privdata
 end;

 devfs_dlist_head=TAILQ_HEAD; //devfs_dirent

 t_devfs_dirent=record
  de_cdp    :p_cdev_priv;
  de_inode  :Integer;
  de_flags  :Integer;
  de_holdcnt:Integer;
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
 t_devfs_mount=record
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

procedure DEVFS_DE_HOLD(de:p_devfs_dirent);
function  DEVFS_DE_DROP(de:p_devfs_dirent):Boolean;

procedure DEVFS_DMP_HOLD(dmp:p_devfs_mount);
function  DEVFS_DMP_DROP(dmp:p_devfs_mount):Boolean;

//

type
 p_dirlistent=^t_dirlistent;
 t_dirlistent=record
  dir   :PChar;
  refcnt:Integer;
  link  :LIST_ENTRY; //dirlistent
 end;

var
 devfs_dirlist:LIST_HEAD=(lh_first:nil); //dirlistent
 dirlist_mtx  :mtx; //MTX_SYSINIT(dirlist_mtx, &dirlist_mtx, "devfs dirlist lock", MTX_DEF);

 devfs_de_interlock:mtx; //MTX_SYSINIT(devfs_de_interlock, @devfs_de_interlock, 'devfs interlock', MTX_DEF);
 cdevpriv_mtx      :mtx; //MTX_SYSINIT(cdevpriv_mtx, @cdevpriv_mtx, 'cdevpriv lock', MTX_DEF);
 clone_drain_lock  :t_sx=(n:'clone events drain lock';c:nil;m:0);

 devfs_generation:DWORD=0;

function  devfs_dir_find(path:PChar):Integer;
function  devfs_dir_findent_locked(dir:PChar):p_dirlistent;
procedure devfs_dir_ref(dir:PChar);
procedure devfs_dir_ref_de(dm:p_devfs_mount;de:p_devfs_dirent);
procedure devfs_dir_unref(dir:PChar);
procedure devfs_dir_unref_de(dm:p_devfs_mount;de:p_devfs_dirent);
function  devfs_pathpath(p1,p2:PChar):Integer;
procedure devfs_mtx_init;

var
 cdevp_list:TAILQ_HEAD=(tqh_first:nil;tqh_last:@cdevp_list.tqh_first);

function  devfs_alloc(flags:Integer):p_cdev;                                                                           external;
function  devfs_dev_exists(name:PChar):Integer;                                                                        external;
procedure devfs_free(cdev:p_cdev);                                                                                     external;
function  devfs_find(dd:p_devfs_dirent;name:PChar;namelen:Integer;_type:Integer):p_devfs_dirent;                       external;
function  devfs_newdirent(name:PChar;namelen:Integer):p_devfs_dirent;                                                  external;
function  devfs_parent_dirent(de:p_devfs_dirent):p_devfs_dirent;                                                       external;
function  devfs_vmkdir(dmp:p_devfs_mount;name:PChar;namelen:Integer;dotdot:p_devfs_dirent;inode:DWORD):p_devfs_dirent; external;
procedure devfs_dirent_free(de:p_devfs_dirent);                                                                        external;
procedure devfs_rmdir_empty(dm:p_devfs_mount;de:p_devfs_dirent);                                                       external;
procedure devfs_delete(dm:p_devfs_mount;de:p_devfs_dirent;flags:Integer);                                              external;
procedure devfs_purge(dm:p_devfs_mount;dd:p_devfs_dirent);                                                             external;
procedure devfs_metoo(cdp:p_cdev_priv;dm:p_devfs_mount);                                                               external;
function  devfs_populate_loop(dm:p_devfs_mount;cleanup:Integer):Integer;                                               external;
procedure devfs_populate(dm:p_devfs_mount);                                                                            external;
procedure devfs_cleanup(dm:p_devfs_mount);                                                                             external;
procedure devfs_create(dev:p_cdev);                                                                                    external;
procedure devfs_destroy(dev:p_cdev);                                                                                   external;
function  devfs_alloc_cdp_inode():ino_t;                                                                               external;
procedure devfs_free_cdp_inode(ino:ino_t);                                                                             external;

function  devfs_fqpn(buf:PChar;dmp:p_devfs_mount;dd:p_devfs_dirent;cnp:Pointer):PChar; external;

implementation

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

procedure DEVFS_DE_HOLD(de:p_devfs_dirent);
begin
 Inc(de^.de_holdcnt);
end;

function DEVFS_DE_DROP(de:p_devfs_dirent):Boolean;
begin
 Dec(de^.de_holdcnt);
 Result:=(de^.de_holdcnt=0);
end;

procedure DEVFS_DMP_HOLD(dmp:p_devfs_mount);
begin
 Inc(dmp^.dm_holdcnt);
end;

function DEVFS_DMP_DROP(dmp:p_devfs_mount):Boolean;
begin
 Dec(dmp^.dm_holdcnt);
 Result:=(dmp^.dm_holdcnt=0);
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
 if (dir^=#0) then Exit;

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
 namep:=devfs_fqpn(dirname, dm, de, nil);
 Assert(namep<>nil,'devfs_ref_dir_de: nil namep');

 devfs_dir_ref(namep);
end;

procedure devfs_dir_unref(dir:PChar);
var
 dle:p_dirlistent;
begin
 if (dir^=#0) then Exit;

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
 begin
  mtx_unlock(dirlist_mtx);
 end;
end;

procedure devfs_dir_unref_de(dm:p_devfs_mount;de:p_devfs_dirent);
var
 dirname:array[0..SPECNAMELEN] of AnsiChar;
 namep:PChar;
begin
 namep:=devfs_fqpn(dirname, dm, de, nil);
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
  end else
  if (p1^=#0) then
   Exit(1);
  Inc(p1);
  Inc(p2);
 until false;
 { NOTREACHED }
end;

procedure devfs_mtx_init;
begin
 mtx_init(devmtx,'cdev');
 mtx_init(dirlist_mtx,'devfs dirlist lock');
 mtx_init(devfs_de_interlock,'devfs interlock');
 mtx_init(cdevpriv_mtx,'cdevpriv lock');
end;


end.


