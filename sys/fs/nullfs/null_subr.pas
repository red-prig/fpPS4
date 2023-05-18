unit null_subr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vmount,
 vnode,
 nullfs;

{
 * null layer cache:
 * Each cache entry holds a reference to the lower vnode
 * along with a pointer to the alias vnode.  When an
 * entry is added the lower vnode is VREF'd.  When the
 * alias is removed the lower vnode is vrele'd.
 }
function  nullfs_init(vfsp:p_vfsconf):Integer;
function  nullfs_uninit(vfsp:p_vfsconf):Integer;

function  null_hashget(mp:p_mount;lowervp:p_vnode):p_vnode;
function  null_hashins(mp:p_mount;xp:p_null_node):p_vnode;
procedure null_destroy_proto(vp:p_vnode;xp:Pointer);
procedure null_insmntque_dtr(vp:p_vnode;xp:Pointer);
function  null_nodeget(mp:p_mount;lowervp:p_vnode;vpp:pp_vnode):Integer;
procedure null_hashrem(xp:p_null_node);

implementation

uses
 hamt,
 mqueue,
 errno,
 vfs_subr,
 vfs_vnops,
 dead_vnops,
 vnode_if,
 null_vnops,
 kern_mtx;

var
 null_node_hashtbl:TSTUB_HAMT32;
 null_hashmtx:mtx;

function MOUNTTONULLMOUNT(mp:p_mount):p_null_mount; inline;
begin
 Result:=mp^.mnt_data;
end;

function VTONULL(vp:p_vnode):p_null_node; inline;
begin
 Result:=vp^.v_data;
end;

function NULLTOV(xp:p_null_node):p_vnode; inline;
begin
 Result:=xp^.null_vnode;
end;

function vfs_hash_index(vp:p_vnode):DWORD;
begin
 if (vp=nil) then Exit(0);
 Result:=(vp^.v_hash + p_mount(vp^.v_mount)^.mnt_hashseed);
end;

function NULL_NHASH(vp:p_vnode;force:Boolean):Pointer;
var
 data:PPointer;
begin
 Result:=nil;
 data:=HAMT_search32(@null_node_hashtbl,vfs_hash_index(vp));
 if (data<>nil) then
 begin
  Result:=data^;
 end else
 if force then
 begin
  Result:=AllocMem(SizeOf(LIST_HEAD));
  if (Result=nil) then Exit;
  data:=HAMT_insert32(@null_node_hashtbl,vfs_hash_index(vp),Result);
  if (data=nil) then
  begin
   FreeMem(Result);
   Result:=nil;
  end else
  if (data^<>Result) then
  begin
   FreeMem(Result);
   Result:=data^;
  end;
 end;
end;

{
 * Initialise cache headers
 }
function nullfs_init(vfsp:p_vfsconf):Integer;
begin
 FillChar(null_node_hashtbl,SizeOf(null_node_hashtbl),0);
 mtx_init(null_hashmtx, 'nullhs');
 Exit(0);
end;

procedure free_hash_data_cb(data,userdata:Pointer); register;
begin
 if (data<>nil) then
 begin
  FreeMem(data);
 end;
end;

function nullfs_uninit(vfsp:p_vfsconf):Integer;
begin
 mtx_destroy(null_hashmtx);
 HAMT_clear32(@null_node_hashtbl,@free_hash_data_cb,nil);
 Exit(0);
end;

{
 * Exita VREF'ed alias for lower vnode if already exists, else 0.
 * Lower vnode should be locked on entry and will be left locked on exit.
 }
function null_hashget(mp:p_mount;lowervp:p_vnode):p_vnode;
var
 hd:Pointer;
 a:p_null_node;
 vp:p_vnode;
begin
 ASSERT_VOP_LOCKED(lowervp, 'nil_hashget');

 {
  * Find hash base, and then search the (two-way) linked
  * list looking for a nil_node structure which is referencing
  * the lower vnode.  If found, the increment the nil_node
  * reference count (but NOT the lower vnode's VREF counter).
  }
 mtx_lock(null_hashmtx);

 hd:=NULL_NHASH(lowervp,false);
 if (hd=nil) then
 begin
  mtx_unlock(null_hashmtx);
  Exit(nil);
 end;

 a:=LIST_FIRST(hd);
 while (a<>nil) do
 begin
  if (a^.null_lowervp=lowervp) and (NULLTOV(a)^.v_mount=mp) then
  begin
   {
    * Since we have the lower node locked the nullfs
    * node can not be in the process of recycling.  If
    * it had been recycled before we grabed the lower
    * lock it would not have been found on the hash.
    }
   vp:=NULLTOV(a);
   vref(vp);
   mtx_unlock(null_hashmtx);
   Exit(vp);
  end;
  a:=LIST_NEXT(a,@a^.null_hash);
 end;
 mtx_unlock(null_hashmtx);
 Exit(nil);
end;

{
 * Act like nil_hashget, but add passed null_node to hash if no existing
 * node found.
 }
function null_hashins(mp:p_mount;xp:p_null_node):p_vnode;
var
 hd:Pointer;
 oxp:p_null_node;
 ovp:p_vnode;
begin
 mtx_lock(null_hashmtx);

 hd:=NULL_NHASH(xp^.null_lowervp,true);
 if (hd=nil) then
 begin
  mtx_unlock(null_hashmtx);
  Exit(nil);
 end;

 oxp:=LIST_FIRST(hd);
 while (oxp<>nil) do
 begin
  if (oxp^.null_lowervp=xp^.null_lowervp) and
     (NULLTOV(oxp)^.v_mount=mp) then
  begin
   {
    * See nil_hashget for a description of this
    * operation.
    }
   ovp:=NULLTOV(oxp);
   vref(ovp);
   mtx_unlock(null_hashmtx);
   Exit(ovp);
  end;
  oxp:=LIST_NEXT(oxp,@oxp^.null_hash);
 end;
 LIST_INSERT_HEAD(hd, xp,@xp^.null_hash);
 mtx_unlock(null_hashmtx);
 Exit(nil);
end;

procedure null_destroy_proto(vp:p_vnode;xp:Pointer);
begin
 mtx_lock(vp^.v_lock);
 //lockmgr(@vp^.v_lock, LK_EXCLUSIVE, nil);
 VI_LOCK(vp);
 vp^.v_data  :=nil;
 vp^.v_vnlock:=@vp^.v_lock;
 vp^.v_op    :=@dead_vnodeops;
 VI_UNLOCK(vp);
 vgone(vp);
 vput(vp);

 FreeMem(xp);
end;

procedure null_insmntque_dtr(vp:p_vnode;xp:Pointer);
begin
 vput(p_null_node(xp)^.null_lowervp);
 null_destroy_proto(vp, xp);
end;

{
 * Make a new or get existing nullfs node.
 * Vp is the alias vnode, lowervp is the lower vnode.
 *
 * The lowervp assumed to be locked and having 'spare' reference. This routine
 * vrele lowervp if nullfs node was taken from hash. Otherwise it 'transfers'
 * the caller's 'spare' reference to created nullfs vnode.
 }
function null_nodeget(mp:p_mount;lowervp:p_vnode;vpp:pp_vnode):Integer;
var
 xp:p_null_node;
 vp:p_vnode;
 error:Integer;
begin
 ASSERT_VOP_LOCKED(lowervp, 'lowervp');

 if (lowervp<>nil) then
 begin
  Assert(lowervp^.v_usecount >= 1,'Unreferenced vnode %p');
 end;

 { Lookup the hash firstly. }
 vpp^:=null_hashget(mp, lowervp);
 if (vpp^<>nil) then
 begin
  vrele(lowervp);
  Exit(0);
 end;

 {
  * The insmntque1() call below requires the exclusive lock on
  * the nullfs vnode.  Upgrade the lock now if hash failed to
  * provide ready to use vnode.
  }
 if (lowervp<>nil) then
 if (VOP_ISLOCKED(lowervp)<>LK_EXCLUSIVE) then
 begin
  Assert((MOUNTTONULLMOUNT(mp)^.nullm_flags and NULLM_CACHE)<>0,'lowervp %p is not excl locked and cache is disabled');
  vn_lock(lowervp, LK_UPGRADE or LK_RETRY);
  if ((lowervp^.v_iflag and VI_DOOMED)<>0) then
  begin
   vput(lowervp);
   Exit(ENOENT);
  end;
 end;

 {
  * We do not serialize vnode creation, instead we will check for
  * duplicates later, when adding new vnode to hash.
  * Note that duplicate can only appear in hash if the lowervp is
  * locked LK_SHARED.
  *
  * Do the MALLOC before the getnewvnode since doing so afterward
  * might cause a bogus v_data pointer to get dereferenced
  * elsewhere if MALLOC should block.
  }
 xp:=AllocMem(sizeof(t_null_node));

 error:=getnewvnode('nil', mp, @null_vnodeops, @vp);
 if (error<>0) then
 begin
  vput(lowervp);
  FreeMem(xp);
  Exit(error);
 end;

 xp^.null_vnode  :=vp;
 xp^.null_lowervp:=lowervp;
 xp^.null_flags  :=0;

 vp^.v_data  :=xp;

 if (lowervp=nil) then
 begin
  vp^.v_type  :=VDIR;
  vp^.v_vnlock:=@vp^.v_lock;
 end else
 begin
  vp^.v_type  :=lowervp^.v_type;
  vp^.v_vnlock:=lowervp^.v_vnlock;
 end;

 error:=insmntque1(vp, mp, @null_insmntque_dtr, xp);
 if (error<>0) then
  Exit(error);
 {
  * Atomically insert our new node into the hash or vget existing
  * if someone else has beaten us to it.
  }
 vpp^:=null_hashins(mp, xp);
 if (vpp^<>nil) then
 begin
  vrele(lowervp);
  null_destroy_proto(vp, xp);
  Exit(0);
 end;
 vpp^:=vp;

 Exit(0);
end;

{
 * Remove node from hash.
 }
procedure null_hashrem(xp:p_null_node);
var
 hd:Pointer;
 data:PPointer;
begin
 mtx_lock(null_hashmtx);
 LIST_REMOVE(xp,@xp^.null_hash);

 data:=HAMT_search32(@null_node_hashtbl,vfs_hash_index(xp^.null_lowervp));
 if (data<>nil) then
 begin
  hd:=data^;
  if LIST_EMPTY(hd) then
  if HAMT_delete32(@null_node_hashtbl,vfs_hash_index(xp^.null_lowervp),nil) then
  begin
   FreeMem(hd);
  end;
 end;

 mtx_unlock(null_hashmtx);
end;


end.

