unit devfs_devs;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_id,
 devfs,
 devfs_rule;

var
 cdevp_list:TAILQ_HEAD=(tqh_first:nil;tqh_last:@cdevp_list.tqh_first);
 devfs_inos:t_id_desc_table;

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
 time,
 vdirent,
 vfs_vnode,
 kern_mtx,
 kern_sx,
 vfs_vnops,
 vfs_subr,
 vnode_if;

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
  devfs_rules_apply(dmp, dd);
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
 sx_assert(@dm^.dm_lock);
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
  devfs_rules_apply(dm, de);
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

