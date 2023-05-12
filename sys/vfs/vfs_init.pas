unit vfs_init;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 murmurhash,
 mqueue,
 vmount;

function vfs_byname(name:PChar):p_vfsconf;
function vfs_register(vfc:p_vfsconf):Integer;
function vfs_unregister(vfc:p_vfsconf):Integer;

implementation

uses
 errno,
 vfs_default;

var
 maxvfsconf:Integer=VFS_GENERIC+1;

{
 * Single-linked list of configured VFSes.
 * New entries are added/deleted by vfs_register()/vfs_unregister()
 }
 vfsconf:TAILQ_HEAD=(tqh_first:nil;tqh_last:@vfsconf.tqh_first);

{
 * Loader.conf variable vfs.typenumhash enables setting vfc_typenum using a hash
 * calculation on vfc_name, so that it doesn't change when file systems are
 * loaded in a different order. This will avoid the NFS server file handles from
 * changing for file systems that use vfc_typenum in their fsid.
 }
 vfs_typenumhash:Integer=1;

{
 * vfs_init.c
 *
 * Allocate and fill in operations vectors.
 *
 * An undocumented feature of this approach to defining operations is that
 * there can be multiple entries in vfs_opv_descs for the same operations
 * vector. This allows third parties to extend the set of operations
 * supported by another layer in a binary compatibile way. For example,
 * assume that NFS needed to be modified to support Ficus. NFS has an entry
 * (probably nfs_vnopdeop_decls) declaring all the operations NFS supports by
 * default. Ficus could add another entry (ficus_nfs_vnodeop_decl_entensions)
 * listing those new operations Ficus adds to NFS, all without modifying the
 * NFS code. (Of couse, the OTW NFS protocol still needs to be munged, but
 * that is a(whole)nother story.) This is a feature.
 }

{
 * Routines having to do with the management of the vnode table.
 }
function vfs_byname(name:PChar):p_vfsconf;
var
 vfsp:p_vfsconf;
begin
 Result:=nil;
 vfsp:=TAILQ_FIRST(@vfsconf);
 while (vfsp<>nil) do
 begin
  if (strcomp(name, vfsp^.vfc_name)=0) then Exit(vfsp);
  vfsp:=TAILQ_NEXT(vfsp,@vfsp^.vfc_list);
 end;
end;

{ Register a new filesystem type in the global table }
function vfs_register(vfc:p_vfsconf):Integer;
var
 vfsops:p_vfsops;
 tvfc:p_vfsconf;
 hashval:DWORD;
 secondpass:Integer;
begin
 //struct sysctl_oid *oidp;

 if (vfc^.vfc_version<>VFS_VERSION) then
 begin
  Assert(false,'ERROR: filesystem %s, unsupported ABI version');
  Exit(EINVAL);
 end;

 if (vfs_byname(vfc^.vfc_name)<>nil) then
  Exit(EEXIST);

 if (vfs_typenumhash<>0) then
 begin
  {
   * Calculate a hash on vfc_name to use for vfc_typenum. Unless
   * all of 1<^.255 are assigned, it is limited to 8bits since
   * that is what ZFS uses from vfc_typenum and is also the
   * preferred range for vfs_getnewfsid().
   }
  hashval:=MurmurHash64A(@vfc^.vfc_name,strlen(vfc^.vfc_name),0);
  hashval:=hashval and $ff;
  secondpass:=0;
  repeat
   { Look for and fix any collision. }

   tvfc:=TAILQ_FIRST(@vfsconf);
   while (tvfc<>nil) do
   begin
    if (hashval=tvfc^.vfc_typenum) then
    begin
     if (hashval=255) and (secondpass=0) then
     begin
      hashval:=1;
      secondpass:=1;
     end else
      Inc(hashval);
     break;
    end;

    tvfc:=TAILQ_NEXT(tvfc,@tvfc^.vfc_list);
   end;
  until (tvfc=nil);
  vfc^.vfc_typenum:=hashval;
  if (vfc^.vfc_typenum >= maxvfsconf) then
   maxvfsconf:=vfc^.vfc_typenum + 1;
 end else
 begin
  Inc(maxvfsconf);
  vfc^.vfc_typenum:=maxvfsconf;
 end;
 TAILQ_INSERT_TAIL(@vfsconf,vfc,@vfc^.vfc_list);

 {
  * If this filesystem has a sysctl node under vfs
  * (i.e. vfs.xxfs), then change the oid number of that node to
  * match the filesystem's type number.  This allows user code
  * which uses the type number to read sysctl variables defined
  * by the filesystem to continue working. Since the oids are
  * in a sorted list, we need to make sure the order is
  * preserved by re-registering the oid after modifying its
  * number.
  }
 //sysctl_lock();
 //SLIST_FOREACH(oidp, @sysctl__vfs_children, oid_link)
 // if (strcmp(oidp^.oid_name, vfc^.vfc_name)=0) begin
 //  sysctl_unregister_oid(oidp);
 //  oidp^.oid_number:=vfc^.vfc_typenum;
 //  sysctl_register_oid(oidp);
 //  break;
 // end;
 //sysctl_unlock();

 {
  * Initialise unused ``struct vfsops'' fields, to use
  * the vfs_std*() functions.  Note, we need the mount
  * and unmount operations, at the least.  The check
  * for vfsops available is just a debugging aid.
  }
 Assert(vfc^.vfc_vfsops<>nil,'Filesystem %s has no vfsops');
 {
  * Check the mount and unmount operations.
  }
 vfsops:=vfc^.vfc_vfsops;

 Assert(vfsops^.vfs_mount<>nil,'Filesystem %s has no mount op');
 Assert(vfsops^.vfs_unmount<>nil,'Filesystem %s has no unmount op');

 if (vfsops^.vfs_root=nil) then
  { Exit file system's root vnode }
  vfsops^.vfs_root:=@vfs_stdroot;
 if (vfsops^.vfs_quotactl=nil) then
  { quota control }
  vfsops^.vfs_quotactl:=@vfs_stdquotactl;
 if (vfsops^.vfs_statfs=nil) then
  { Exit file system's status }
  vfsops^.vfs_statfs:=@vfs_stdstatfs;
 if (vfsops^.vfs_sync=nil) then
  {
   * flush unwritten data (nosync)
   * file systems can use vfs_stdsync
   * explicitly by setting it in the
   * vfsop vector.
   }
  vfsops^.vfs_sync:=@vfs_stdnosync;
 if (vfsops^.vfs_vget=nil) then
  { convert an inode number to a vnode }
  vfsops^.vfs_vget:=@vfs_stdvget;
 if (vfsops^.vfs_fhtovp=nil) then
  { turn an NFS file handle into a vnode }
  vfsops^.vfs_fhtovp:=@vfs_stdfhtovp;
 //if (vfsops^.vfs_checkexp=nil) then
 // { check if file system is exported }
 // vfsops^.vfs_checkexp:=@vfs_stdcheckexp;
 if (vfsops^.vfs_init=nil) then
  { file system specific initialisation }
  vfsops^.vfs_init:=@vfs_stdinit;
 if (vfsops^.vfs_uninit=nil) then
  { file system specific uninitialisation }
  vfsops^.vfs_uninit:=@vfs_stduninit;
 if (vfsops^.vfs_extattrctl=nil) then
  { extended attribute control }
  vfsops^.vfs_extattrctl:=@vfs_stdextattrctl;
 if (vfsops^.vfs_sysctl=nil) then
  vfsops^.vfs_sysctl:=@vfs_stdsysctl;

 {
  * Call init function for this VFS...
  }
 vfc^.vfc_vfsops^.vfs_init(vfc);

 Result:=0;
end;


{ Remove registration of a filesystem type }
function vfs_unregister(vfc:p_vfsconf):Integer;
var
 vfsp:p_vfsconf;
 error,maxtypenum:Integer;
begin
 vfsp:=vfs_byname(vfc^.vfc_name);
 if (vfsp=nil) then
  Exit(EINVAL);
 if (vfsp^.vfc_refcount<>0) then
  Exit(EBUSY);
 if (vfc^.vfc_vfsops^.vfs_uninit<>nil) then
 begin
  error:=vfc^.vfc_vfsops^.vfs_uninit(vfsp);
  if (error<>0) then
   Exit(error);
 end;
 TAILQ_REMOVE(@vfsconf,vfsp,@vfsp^.vfc_list);
 maxtypenum:=VFS_GENERIC;

 vfsp:=TAILQ_FIRST(@vfsconf);
 while (vfsp<>nil) do
 begin
  if (maxtypenum<vfsp^.vfc_typenum) then
   maxtypenum:=vfsp^.vfc_typenum;

  vfsp:=TAILQ_NEXT(vfsp,@vfsp^.vfc_list);
 end;

 maxvfsconf:=maxtypenum + 1;
 Result:=0;
end;

{
 * Standard kernel module handling code for filesystem modules.
 * Referenced from VFS_SET().
 }
{
int
vfs_modevent(module_t mod, int type, void *data)
begin
 p_vfsconf vfc;
 int error:=0;

 vfc:=(p_vfsconf )data;

 switch (type) begin
 case MOD_LOAD:
  if (vfc)
   error:=vfs_register(vfc);
  break;

 case MOD_UNLOAD:
  if (vfc)
   error:=vfs_unregister(vfc);
  break;
 default:
  error:=EOPNOTSUPP;
  break;
 end;
 Exit (error);
end;
}

end.

