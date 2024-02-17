unit devfs;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 devfs_int,
 mqueue,
 kern_param,
 vnode,
 vmount,
 sys_conf,
 kern_mtx;

function  rid2rsn(rid:devfs_rid):devfs_rsnum;
function  rid2rn (rid:devfs_rid):devfs_rnum;
function  mkrid  (rsn:devfs_rsnum;rn:devfs_rnum):devfs_rid;

procedure DEVFS_DE_HOLD(de:p_devfs_dirent);
function  DEVFS_DE_DROP(de:p_devfs_dirent):Boolean;

procedure DEVFS_DMP_HOLD(dmp:p_devfs_mount);
function  DEVFS_DMP_DROP(dmp:p_devfs_mount):Boolean;

//

function  devfs_dir_find(path:PChar):Integer;
procedure devfs_dir_ref_de(dm:p_devfs_mount;de:p_devfs_dirent);
procedure devfs_dir_unref_de(dm:p_devfs_mount;de:p_devfs_dirent);
function  devfs_pathpath(p1,p2:PChar):Integer;
procedure devfs_mtx_init;

procedure devfs_rules_apply(dm:p_devfs_mount;de:p_devfs_dirent); external;
procedure devfs_rules_cleanup(dm:p_devfs_mount); external;
function  devfs_rules_ioctl(dm:p_devfs_mount;cmd:QWORD;data:Pointer):Integer; external;

procedure devfs_ruleset_set(rsnum:devfs_rsnum;dm:p_devfs_mount); external;
procedure devfs_ruleset_apply(dm:p_devfs_mount); external;
function  devfs_allocv(de:p_devfs_dirent;mp:p_mount;lockmode:Integer;vpp:pp_vnode):Integer; external;
function  devfs_fqpn(buf:PChar;dmp:p_devfs_mount;dd:p_devfs_dirent;cnp:Pointer):PChar; external;

procedure devfs_delete(dm:p_devfs_mount;de:p_devfs_dirent;flags:Integer); external;
procedure devfs_dirent_free(de:p_devfs_dirent); external;
procedure devfs_populate(dm:p_devfs_mount); external;
procedure devfs_cleanup(dm:p_devfs_mount); external;

procedure devfs_unmount_final(fmp:p_devfs_mount); external;

function  devfs_newdirent(name:PChar;namelen:Integer):p_devfs_dirent; external;
function  devfs_parent_dirent(de:p_devfs_dirent):p_devfs_dirent; external;
function  devfs_vmkdir(dmp:p_devfs_mount;
                       name:PChar;namelen:Integer;
                       dotdot:p_devfs_dirent;
                       inode:DWORD):p_devfs_dirent; external;
function  devfs_find(dd:p_devfs_dirent;
                     name:PChar;namelen:Integer;
                     _type:Integer):p_devfs_dirent; external;

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
function devfs_dir_find(path:PChar):Integer; public;
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

procedure devfs_dir_unref_de(dm:p_devfs_mount;de:p_devfs_dirent); public;
var
 dirname:array[0..SPECNAMELEN] of AnsiChar;
 namep:PChar;
begin
 namep:=devfs_fqpn(dirname, dm, de, nil);
 Assert(namep<>nil, 'devfs_unref_dir_de: nil namep');

 devfs_dir_unref(namep);
end;

{ Exits 1 if the path p1 contains the path p2. }
function devfs_pathpath(p1,p2:PChar):Integer; public;
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


