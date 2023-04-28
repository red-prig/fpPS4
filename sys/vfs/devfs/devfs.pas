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
 kern_id;

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
 devfs_rnum =WORD;
 devfs_rsnum=WORD;
 devfs_rid  =DWORD;

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
 p_devfs_rule=^devfs_rule;
 devfs_rule=packed record
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

 p_devfs_mount=^devfs_mount;
 devfs_mount=packed record
  dm_idx       :DWORD         ;
  dm_mount     :p_mount       ;
  dm_rootdir   :p_devfs_dirent;
  dm_generation:DWORD         ;
  dm_holdcnt   :integer       ;
  dm_lock      :Pointer       ;
  dm_ruleset   :devfs_rsnum   ;
 end;

const
 DEVFS_ROOTINO=2;

 DEVFS_DEL_VNLOCKED =$01;
 DEVFS_DEL_NORECURSE=$02;

//extern unsigned devfs_rule_depth;

function  rid2rsn(rid:devfs_rid):devfs_rsnum; inline;
function  rid2rn (rid:devfs_rid):devfs_rnum; inline;
function  mkrid  (rsn:devfs_rsnum;rn:devfs_rnum):devfs_rid; inline;

function  VFSTODEVFS(mp:p_mount):p_devfs_mount; inline;

procedure DEVFS_DE_HOLD(de:p_devfs_dirent); inline;
function  DEVFS_DE_DROP(de:p_devfs_dirent):Boolean; inline;

procedure DEVFS_DMP_HOLD(dmp:p_devfs_mount); inline;
function  DEVFS_DMP_DROP(dmp:p_devfs_mount):Boolean; inline;

function  cdev2priv(c:Pointer):p_cdev_priv; inline;

var
 cdevp_list:TAILQ_HEAD=(tqh_first:nil;tqh_last:@cdevp_list.tqh_first);
 devfs_inos:t_id_desc_table;
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

procedure dev_lock(); inline;
procedure dev_unlock(); inline;
procedure dev_ref(dev:p_cdev);
procedure dev_rel(dev:p_cdev);

function  devfs_dir_find(path:PChar):Integer;
function  devfs_dir_findent_locked(dir:PChar):p_dirlistent;
procedure devfs_dir_ref(dir:PChar);
procedure devfs_dir_ref_de(dm:p_devfs_mount;de:p_devfs_dirent);
procedure devfs_dir_unref(dir:PChar);
procedure devfs_dir_unref_de(dm:p_devfs_mount;de:p_devfs_dirent);
function  devfs_pathpath(p1,p2:PChar):Integer;
procedure dirlist_mtx_init; //MTX_SYSINIT(dirlist_mtx, &dirlist_mtx, "devfs dirlist lock", MTX_DEF);
                            //MTX_SYSINIT(devfs_de_interlock, &devfs_de_interlock, "devfs interlock", MTX_DEF);

function  devfs_alloc(flags:Integer):p_cdev;
function  devfs_dev_exists(name:PChar):Integer;
procedure devfs_free(cdev:p_cdev);
function  devfs_find(dd:p_devfs_dirent;name:PChar;namelen:Integer;_type:Integer):p_devfs_dirent;
function  devfs_newdirent(name:PChar;namelen:Integer):p_devfs_dirent;
function  devfs_parent_dirent(de:p_devfs_dirent):p_devfs_dirent;
function  devfs_vmkdir(dmp:p_devfs_mount;name:PChar;namelen:Integer;dotdot:p_devfs_dirent;inode:DWORD):p_devfs_dirent;
procedure devfs_dirent_free(de:p_devfs_dirent);
procedure devfs_rmdir_empty(dm:p_devfs_mount;de:p_devfs_dirent);
procedure devfs_delete(dm:p_devfs_mount;de:p_devfs_dirent;flags:Integer);
procedure devfs_purge(dm:p_devfs_mount;dd:p_devfs_dirent);
procedure devfs_metoo(cdp:p_cdev_priv;dm:p_devfs_mount);
function  devfs_populate_loop(dm:p_devfs_mount;cleanup:Integer):Integer;
procedure devfs_populate(dm:p_devfs_mount);
procedure devfs_cleanup(dm:p_devfs_mount);
procedure devfs_create(dev:p_cdev);
procedure devfs_destroy(dev:p_cdev);
function  devfs_alloc_cdp_inode():ino_t;
procedure devfs_free_cdp_inode(ino:ino_t);
procedure devfs_devs_init(); //SYSINIT(devfs_devs, SI_SUB_DEVFS, SI_ORDER_FIRST, devfs_devs_init, NULL);

implementation

uses
 kern_rwlock,
 vfs_vnops,
 vfs_subr,
 vnode_if;

{
 * Identifier manipulators.
 }
function rid2rsn(rid:devfs_rid):devfs_rsnum; inline;
begin
 Result:=rid shr 16;
end;

function rid2rn(rid:devfs_rid):devfs_rnum; inline;
begin
 Result:=rid and $ffff;
end;

function mkrid(rsn:devfs_rsnum;rn:devfs_rnum):devfs_rid; inline;
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

procedure sx_assert(p:PPointer); inline;
begin
 //
end;

procedure sx_xlock(p:PPointer); inline;
begin
 rw_wlock(p^);
end;

procedure sx_unlock(p:PPointer); inline;
begin
 rw_wunlock(p^);
end;

//

procedure dev_lock(); inline;
begin
 mtx_lock(devmtx);
end;

procedure dev_unlock(); inline;
begin
 mtx_unlock(devmtx);
end;

procedure dev_ref(dev:p_cdev);
begin
 mtx_lock(devmtx);
 Inc(dev^.si_refcount);
 mtx_unlock(devmtx);
end;

procedure dev_refl(dev:p_cdev); inline;
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

procedure dirlist_mtx_init;
begin
 mtx_init(dirlist_mtx,'devfs dirlist lock');
 mtx_init(devfs_de_interlock,'devfs interlock');
end;

//

function devfs_alloc(flags:Integer):p_cdev;
var
 cdp:p_cdev_priv;
 cdev:p_cdev;
 ts:timespec;
begin
 cdp:=AllocMem(SizeOf(t_cdev_priv));

 if (cdp=nil) then
  Exit(nil);

 cdp^.cdp_dirents:=@cdp^.cdp_dirent0;
 cdp^.cdp_dirent0:=nil;
 cdp^.cdp_maxdirent:=0;
 cdp^.cdp_inode:=0;

 cdev:=@cdp^.cdp_c;

 cdev^.si_name:=cdev^.__si_namebuf;
 LIST_INIT(@cdev^.si_children);
 vfs_timestamp(@ts);
 cdev^.si_atime:=ts;
 cdev^.si_mtime:=ts;
 cdev^.si_ctime:=ts;

 Exit(cdev);
end;

function devfs_dev_exists(name:PChar):Integer;
var
 cdp:p_cdev_priv;
begin
 mtx_assert(devmtx);

 cdp:=TAILQ_FIRST(@cdevp_list);
 while (cdp<>nil) do
 begin
  if ((cdp^.cdp_flags and CDP_ACTIVE)=0) then
  begin
   cdp:=TAILQ_NEXT(cdp,@cdp^.cdp_list);
   continue;
  end;
  if (devfs_pathpath(cdp^.cdp_c.si_name, name)<>0) then
   Exit(1);
  if (devfs_pathpath(name, cdp^.cdp_c.si_name)<>0) then
   Exit(1);
  //
  cdp:=TAILQ_NEXT(cdp,@cdp^.cdp_list);
 end;
 if (devfs_dir_find(name)<>0) then
  Exit(1);

 Exit(0);
end;

procedure devfs_free(cdev:p_cdev);
var
 cdp:p_cdev_priv;
begin
 cdp:=cdev2priv(cdev);
 devfs_free_cdp_inode(cdp^.cdp_inode);
 if (cdp^.cdp_maxdirent > 0) then
  FreeMem(cdp^.cdp_dirents);
 FreeMem(cdp);
end;

function devfs_find(dd:p_devfs_dirent;name:PChar;namelen:Integer;_type:Integer):p_devfs_dirent;
var
 de:p_devfs_dirent;
begin
 de:=TAILQ_FIRST(@dd^.de_dlist);
 while (de<>nil) do
 begin
  if (namelen<>de^.de_dirent^.d_namlen) then
  begin
   de:=TAILQ_NEXT(de,@de^.de_list);
   continue;
  end;
  if (_type<>0) and (_type<>de^.de_dirent^.d_type) then
  begin
   de:=TAILQ_NEXT(de,@de^.de_list);
   continue;
  end;

  if (CompareByte(name^, de^.de_dirent^.d_name, namelen)<>0) then
  begin
   de:=TAILQ_NEXT(de,@de^.de_list);
   continue;
  end;
  break;
 end;
 Assert((de=nil) or ((de^.de_flags and DE_DOOMED)=0),'devfs_find: Exiting a doomed entry');
 Exit(de);
end;

function devfs_newdirent(name:PChar;namelen:Integer):p_devfs_dirent;
var
 i:Integer;
 de:p_devfs_dirent;
 d:t_dirent;
begin
 d.d_namlen:=namelen;
 i:=sizeof(t_devfs_dirent) + GENERIC_DIRSIZ(@d);
 de:=AllocMem(i);
 de^.de_dirent:=p_dirent(de + 1);
 de^.de_dirent^.d_namlen:=namelen;
 de^.de_dirent^.d_reclen:=GENERIC_DIRSIZ(@d);
 Move(name^, de^.de_dirent^.d_name, namelen);
 de^.de_dirent^.d_name[namelen]:=#0;
 vfs_timestamp(@de^.de_ctime);
 de^.de_mtime  :=de^.de_ctime;
 de^.de_atime  :=de^.de_ctime;
 de^.de_links  :=1;
 de^.de_holdcnt:=1;

 //mac_devfs_init(de);

 Exit(de);
end;

function devfs_parent_dirent(de:p_devfs_dirent):p_devfs_dirent;
begin

 if (de^.de_dirent^.d_type<>DT_DIR) then
  Exit(de^.de_dir);

 if ((de^.de_flags and (DE_DOT or DE_DOTDOT))<>0) then
  Exit(nil);

 de:=TAILQ_FIRST(@de^.de_dlist); { '.' }
 if (de=nil) then
  Exit(nil);

 de:=TAILQ_NEXT(de,@de^.de_list);  { '..' }
 if (de=nil) then
  Exit(nil);

 Exit(de^.de_dir);
end;

function devfs_vmkdir(dmp:p_devfs_mount;name:PChar;namelen:Integer;dotdot:p_devfs_dirent;inode:DWORD):p_devfs_dirent;
var
 dd,de:p_devfs_dirent;
begin
 { Create the new directory }
 dd:=devfs_newdirent(name, namelen);
 TAILQ_INIT(@dd^.de_dlist);
 dd^.de_dirent^.d_type:=DT_DIR;
 dd^.de_mode :=&0555;
 dd^.de_links:=2;
 dd^.de_dir  :=dd;

 if (inode<>0) then
  dd^.de_inode:=inode
 else
  dd^.de_inode:=devfs_alloc_cdp_inode;

 {
  * '.' and '..' are always the two first entries in the
  * de_dlist list.
  *
  * Create the '.' entry in the new directory.
  }
 de:=devfs_newdirent('.', 1);
 de^.de_dirent^.d_type:=DT_DIR;
 de^.de_flags:=de^.de_flags or DE_DOT;
 TAILQ_INSERT_TAIL(@dd^.de_dlist,de,@de^.de_list);
 de^.de_dir:=dd;

 { Create the '..' entry in the new directory. }
 de:=devfs_newdirent('..', 2);
 de^.de_dirent^.d_type:=DT_DIR;
 de^.de_flags:=de^.de_flags or DE_DOTDOT;
 TAILQ_INSERT_TAIL(@dd^.de_dlist,de,@de^.de_list);
 if (dotdot=nil) then
 begin
  de^.de_dir:=dd;
 end else
 begin
  de^.de_dir:=dotdot;
  sx_assert(@dmp^.dm_lock);
  TAILQ_INSERT_TAIL(@dotdot^.de_dlist,dd,@dd^.de_list);
  Inc(dotdot^.de_links);
  //devfs_rules_apply(dmp, dd);
 end;

 //mac_devfs_create_directory(dmp^.dm_mount, name, namelen, dd);

 Exit(dd);
end;

procedure devfs_dirent_free(de:p_devfs_dirent);
begin
 FreeMem(de);
end;

{
 * Removes a directory if it is empty. Also empty parent directories are
 * removed recursively.
 }
procedure devfs_rmdir_empty(dm:p_devfs_mount;de:p_devfs_dirent);
var
 dd,de_dot,de_dotdot:p_devfs_dirent;
begin
 sx_assert(@dm^.dm_lock);

 repeat
  Assert(de^.de_dirent^.d_type=DT_DIR,'devfs_rmdir_empty: de is not a directory');

  if ((de^.de_flags and DE_DOOMED)<>0) or (de=dm^.dm_rootdir) then
   Exit;

  de_dot:=TAILQ_FIRST(@de^.de_dlist);
  Assert(de_dot<>nil, ('devfs_rmdir_empty: . missing'));
  de_dotdot:=TAILQ_NEXT(de_dot,@de_dot^.de_list);
  Assert(de_dotdot<>nil, ('devfs_rmdir_empty: .. missing'));
  { Exitif the directory is not empty. }
  if (TAILQ_NEXT(de_dotdot,@de_dotdot^.de_list)<>nil) then
   Exit;

  dd:=devfs_parent_dirent(de);
  Assert(dd<>nil, ('devfs_rmdir_empty: nil dd'));
  TAILQ_REMOVE(@de^.de_dlist,de_dot,@de_dot^.de_list);
  TAILQ_REMOVE(@de^.de_dlist,de_dotdot,@de_dotdot^.de_list);
  TAILQ_REMOVE(@dd^.de_dlist,de,@de^.de_list);
  DEVFS_DE_HOLD(dd);
  devfs_delete(dm, de, DEVFS_DEL_NORECURSE);
  devfs_delete(dm, de_dot, DEVFS_DEL_NORECURSE);
  devfs_delete(dm, de_dotdot, DEVFS_DEL_NORECURSE);
  if (DEVFS_DE_DROP(dd)) then
  begin
   devfs_dirent_free(dd);
   Exit;
  end;

  de:=dd;
 until false;
end;

{
 * The caller needs to hold the dm for the duration of the call since
 * dm^.dm_lock may be temporary dropped.
 }
procedure devfs_delete(dm:p_devfs_mount;de:p_devfs_dirent;flags:Integer);
var
 dd:p_devfs_dirent;
 vp:p_vnode;
begin
 Assert((de^.de_flags and DE_DOOMED)=0,'devfs_delete doomed dirent');
 de^.de_flags:=de^.de_flags or DE_DOOMED;

 if ((flags and DEVFS_DEL_NORECURSE)=0) then
 begin
  dd:=devfs_parent_dirent(de);
  if (dd<>nil) then
   DEVFS_DE_HOLD(dd);
  if (de^.de_flags and DE_USER)<>0 then
  begin
   Assert(dd<>nil,'devfs_delete: nil dd');
   devfs_dir_unref_de(dm, dd);
  end;
 end else
  dd:=nil;

 mtx_lock(devfs_de_interlock);
 vp:=de^.de_vnode;
 if (vp<>nil) then
 begin
  VI_LOCK(vp);
  mtx_unlock(devfs_de_interlock);
  vholdl(vp);
  sx_unlock(@dm^.dm_lock);
  if ((flags and DEVFS_DEL_VNLOCKED)=0) then
   vn_lock(vp, LK_EXCLUSIVE or LK_INTERLOCK or LK_RETRY)
  else
   VI_UNLOCK(vp);
  vgone(vp);
  if ((flags and DEVFS_DEL_VNLOCKED)=0) then
   VOP_UNLOCK(vp, 0);
  vdrop(vp);
  sx_xlock(@dm^.dm_lock);
 end else
  mtx_unlock(devfs_de_interlock);
 if (de^.de_symlink<>nil) then
 begin
  FreeMem(de^.de_symlink);
  de^.de_symlink:=nil;
 end;

 //mac_devfs_destroy(de);

 if (de^.de_inode > DEVFS_ROOTINO) then
 begin
  devfs_free_cdp_inode(de^.de_inode);
  de^.de_inode:=0;
 end;
 if DEVFS_DE_DROP(de) then
  devfs_dirent_free(de);

 if (dd<>nil) then
 begin
  if DEVFS_DE_DROP(dd) then
   devfs_dirent_free(dd)
  else
   devfs_rmdir_empty(dm, dd);
 end;
end;

{
 * Called on unmount.
 * Recursively removes the entire tree.
 * The caller needs to hold the dm for the duration of the call.
 }
procedure devfs_purge(dm:p_devfs_mount;dd:p_devfs_dirent);
var
 de:p_devfs_dirent;
begin
 sx_assert(@dm^.dm_lock);

 DEVFS_DE_HOLD(dd);
 repeat
  {
   * Use TAILQ_LAST() to remove '.' and '..' last.
   * We might need '..' to resolve a path in
   * devfs_dir_unref_de().
   }
  de:=TAILQ_LAST(@dd^.de_dlist);
  if (de=nil) then
   break;
  TAILQ_REMOVE(@dd^.de_dlist,de,@de^.de_list);
  if ((de^.de_flags and DE_USER)<>0) then
   devfs_dir_unref_de(dm, dd);
  if ((de^.de_flags and (DE_DOT or DE_DOTDOT))<>0) then
   devfs_delete(dm, de, DEVFS_DEL_NORECURSE)
  else
  if (de^.de_dirent^.d_type=DT_DIR) then
   devfs_purge(dm, de)
  else
   devfs_delete(dm, de, DEVFS_DEL_NORECURSE);
 until false;
 if DEVFS_DE_DROP(dd) then
  devfs_dirent_free(dd)
 else
 if ((dd^.de_flags and DE_DOOMED)=0) then
  devfs_delete(dm, dd, DEVFS_DEL_NORECURSE);
end;

{
 * Each cdev_priv has an array of pointers to devfs_dirent which is indexed
 * by the mount points dm_idx.
 * This function extends the array when necessary, taking into account that
 * the default array is 1 element and not malloc'ed.
 }
procedure devfs_metoo(cdp:p_cdev_priv;dm:p_devfs_mount);
var
 dep:pp_devfs_dirent;
 siz:Integer;
begin
 siz:=(dm^.dm_idx + 1) * sizeof(Pointer);
 dep:=AllocMem(siz);
 dev_lock();
 if (dm^.dm_idx <= cdp^.cdp_maxdirent) then
 begin
  { We got raced }
  dev_unlock();
  FreeMem(dep);
  Exit;
 end;
 Move(cdp^.cdp_dirents^,dep^,(cdp^.cdp_maxdirent + 1)*SizeOf(Pointer));
 if (cdp^.cdp_maxdirent > 0) then
  FreeMem(cdp^.cdp_dirents);
 cdp^.cdp_dirents:=dep;
 {
  * XXX: if malloc told us how much we actually got this could
  * XXX: be optimized.
  }
 cdp^.cdp_maxdirent:=dm^.dm_idx;
 dev_unlock();
end;

{
 * The caller needs to hold the dm for the duration of the call.
 }
function devfs_populate_loop(dm:p_devfs_mount;cleanup:Integer):Integer;
var
 cdp:p_cdev_priv;
 de:p_devfs_dirent;
 dd:p_devfs_dirent;
 pdev:p_cdev;
 de_flags,j:Integer;
 q,s:PChar;
begin
 sx_assert(dm^.dm_lock);
 dev_lock();

 cdp:=TAILQ_FIRST(@cdevp_list);
 while (cdp<>nil) do
 begin
  Assert(cdp^.cdp_dirents<>nil, ('nil cdp_dirents'));

  {
   * If we are unmounting, or the device has been destroyed,
   * clean up our dirent.
   }
  if ((cleanup<>0) or
      ((cdp^.cdp_flags and CDP_ACTIVE)=0)) and
     (dm^.dm_idx <= cdp^.cdp_maxdirent) and
     (cdp^.cdp_dirents[dm^.dm_idx]<>nil) then
  begin
   de:=cdp^.cdp_dirents[dm^.dm_idx];
   cdp^.cdp_dirents[dm^.dm_idx]:=nil;

   Assert(cdp=de^.de_cdp,cdp^.cdp_c.si_name);
   Assert(de^.de_dir<>nil,'nil de^.de_dir');
   dev_unlock();

   TAILQ_REMOVE(@de^.de_dir^.de_dlist,de,@de^.de_list);
   de^.de_cdp  :=nil;
   de^.de_inode:=0;
   devfs_delete(dm, de, 0);
   dev_lock();
   Dec(cdp^.cdp_inuse);
   dev_unlock();
   Exit(1);
  end;
  {
    * GC any lingering devices
   }
  if ((cdp^.cdp_flags and CDP_ACTIVE)=0) then
  begin
   if (cdp^.cdp_inuse > 0) then
   begin
    cdp:=TAILQ_NEXT(cdp,@cdp^.cdp_list);
    continue;
   end;
   TAILQ_REMOVE(@cdevp_list,cdp,@cdp^.cdp_list);
   dev_unlock();
   dev_rel(@cdp^.cdp_c);
   Exit(1);
  end;
  {
   * Don't create any new dirents if we are unmounting
   }
  if (cleanup<>0) then
  begin
   cdp:=TAILQ_NEXT(cdp,@cdp^.cdp_list);
   continue;
  end;
  Assert((cdp^.cdp_flags and CDP_ACTIVE)<>0,'Bogons, I tell ya!');

  if (dm^.dm_idx <= cdp^.cdp_maxdirent) and
     (cdp^.cdp_dirents[dm^.dm_idx]<>nil) then
  begin
   de:=cdp^.cdp_dirents[dm^.dm_idx];
   Assert(cdp=de^.de_cdp, 'inconsistent cdp');
   cdp:=TAILQ_NEXT(cdp,@cdp^.cdp_list);
   continue;
  end;


  Inc(cdp^.cdp_inuse);
  dev_unlock();

  if (dm^.dm_idx > cdp^.cdp_maxdirent) then
   devfs_metoo(cdp, dm);

  dd:=dm^.dm_rootdir;
  s:=cdp^.cdp_c.si_name;
  repeat
   q:=s;
   while (q^<>'/') and (q^<>#0) do Inc(q);
   if (q^<>'/') then
    break;
   de:=devfs_find(dd, s, q - s, 0);
   if (de=nil) then
    de:=devfs_vmkdir(dm, s, q - s, dd, 0)
   else
   if (de^.de_dirent^.d_type=DT_LNK) then
   begin
    de:=devfs_find(dd, s, q - s, DT_DIR);
    if (de=nil) then
     de:=devfs_vmkdir(dm, s, q - s, dd, 0);
    de^.de_flags:=de^.de_flags or DE_COVERED;
   end;
   s:=q + 1;
   dd:=de;
   Assert((dd^.de_dirent^.d_type=DT_DIR) and ((dd^.de_flags and (DE_DOT or DE_DOTDOT))=0),'invalid directory');
  until false;
  de_flags:=0;
  de:=devfs_find(dd, s, q - s, DT_LNK);
  if (de<>nil) then
   de_flags:=de_flags or DE_COVERED;

  de:=devfs_newdirent(s, q - s);
  if ((cdp^.cdp_c.si_flags and SI_ALIAS)<>0) then
  begin
   de^.de_uid :=0;
   de^.de_gid :=0;
   de^.de_mode:=&0755;
   de^.de_dirent^.d_type:=DT_LNK;
   pdev:=cdp^.cdp_c.si_parent;
   j:=strlen(pdev^.si_name) + 1;
   de^.de_symlink:=AllocMem(j);
   Move(pdev^.si_name^,de^.de_symlink^,j);
  end else
  begin
   de^.de_uid :=cdp^.cdp_c.si_uid;
   de^.de_gid :=cdp^.cdp_c.si_gid;
   de^.de_mode:=cdp^.cdp_c.si_mode;
   de^.de_dirent^.d_type:=DT_CHR;
  end;
  de^.de_flags:=de^.de_flags or de_flags;
  de^.de_inode:=cdp^.cdp_inode;
  de^.de_cdp  :=cdp;

  //mac_devfs_create_device(cdp^.cdp_c.si_cred, dm^.dm_mount, @cdp^.cdp_c, de);

  de^.de_dir:=dd;
  TAILQ_INSERT_TAIL(@dd^.de_dlist,de,@de^.de_list);
  //devfs_rules_apply(dm, de);
  dev_lock();
  { XXX: could check that cdp is still active here }
  Assert(cdp^.cdp_dirents[dm^.dm_idx]=nil);
  cdp^.cdp_dirents[dm^.dm_idx]:=de;
  Assert(de^.de_cdp<>Pointer(ptruint($deadc0de)));
  dev_unlock();
  Exit(1);
 end;
 dev_unlock();
 Exit(0);
end;

{
 * The caller needs to hold the dm for the duration of the call.
 }
procedure devfs_populate(dm:p_devfs_mount);
var
 gen:DWORD;
begin
 sx_assert(@dm^.dm_lock);
 gen:=devfs_generation;
 if (dm^.dm_generation=gen) then
  Exit;
 while (devfs_populate_loop(dm, 0)<>0) do;
 dm^.dm_generation:=gen;
end;

{
 * The caller needs to hold the dm for the duration of the call.
 }
procedure devfs_cleanup(dm:p_devfs_mount);
begin
 sx_assert(@dm^.dm_lock);
 while (devfs_populate_loop(dm, 1)<>0) do;
 devfs_purge(dm, dm^.dm_rootdir);
end;

{
 * devfs_create() and devfs_destroy() are called from kern_conf.c and
 * in both cases the devlock() mutex is held, so no further locking
 * is necesary and no sleeping allowed.
 }
procedure devfs_create(dev:p_cdev);
var
 cdp:p_cdev_priv;
begin
 mtx_assert(devmtx);
 cdp:=cdev2priv(dev);
 cdp^.cdp_flags:=cdp^.cdp_flags or CDP_ACTIVE;
 cdp^.cdp_inode:=devfs_alloc_cdp_inode;
 dev_refl(dev);
 TAILQ_INSERT_TAIL(@cdevp_list,cdp,@cdp^.cdp_list);
 Inc(devfs_generation);
end;

procedure devfs_destroy(dev:p_cdev);
var
 cdp:p_cdev_priv;
begin
 mtx_assert(devmtx);
 cdp:=cdev2priv(dev);
 cdp^.cdp_flags:=cdp^.cdp_flags and (not CDP_ACTIVE);
 Inc(devfs_generation);
end;

function devfs_alloc_cdp_inode():ino_t;
begin
 if id_new(@devfs_inos,nil,@Result) then
 begin
  //
 end else
 begin
  Result:=-1;
 end;
end;

procedure devfs_free_cdp_inode(ino:ino_t);
begin
 if (ino>0) then
 begin
  id_del(@devfs_inos,ino,nil);
 end;
end;

procedure devfs_devs_init();
begin
 id_table_init(@devfs_inos,DEVFS_ROOTINO + 1);
end;


end.


