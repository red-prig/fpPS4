unit vm_object;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vm,
 kern_mtx,
 kern_synch;

type
 obj_type=(
   OBJT_DEFAULT  ,//0
   OBJT_SWAP     ,//1
   OBJT_VNODE    ,//2
   OBJT_DEVICE   ,//3
   OBJT_PHYS     ,//4
   OBJT_DEAD     ,//5
   OBJT_SG       ,//6
   OBJT_JITSHM   ,//7
   OBJT_SELF     ,//8
   OBJT_TRCMEM   ,//9
   OBJT_PHYSHM   ,//10
   OBJT_BLOCKPOOL //11
  );

 objtype_t=obj_type;

 p_vm_object_t=^vm_object_t;
 vm_object_t=^_vm_object;
 _vm_object=packed record
  mtx               :mtx;
  memq              :TAILQ_HEAD;              // list of resident pages
  patchq            :TAILQ_HEAD;              // list of patches
  root              :Pointer;                 // root of the resident page splay tree
  size              :vm_pindex_t;             // Object size
  generation        :Integer;                 // generation ID
  ref_count         :Integer;                 // How many refs??
  otype             :objtype_t;               // type of pager
  flags             :Word;                    // see below
  handle            :Pointer;
  paging_in_progress:Integer;
  un_pager:packed record
   vnp:packed record
    vnp_size:QWORD;
    writemappings:vm_ooffset_t;
   end;
  end;
 end;

const
 //Flags
 OBJ_ACTIVE       =$0004; // active objects
 OBJ_DEAD         =$0008; // dead objects (during rundown)
 OBJ_NOSPLIT      =$0010; // dont split this object
 OBJ_PIPWNT       =$0040; // paging in progress wanted
 OBJ_MIGHTBEDIRTY =$0100; // object might be dirty, only for vnode
 OBJ_COLORED      =$1000; // pg_color is defined
 OBJ_ONEMAPPING   =$2000; // One USE (a single, non-forked) mapping flag
 OBJ_DISCONNECTWNT=$4000; // disconnect from vnode wanted

 OBJPC_SYNC  =$1;  // sync I/O
 OBJPC_INVAL =$2;  // invalidate
 OBJPC_NOSYNC=$4;  // skip if PG_NOSYNC

 //The following options are supported by vm_object_page_remove().
 OBJPR_CLEANONLY=$1; // Don't remove dirty pages.
 OBJPR_NOTMAPPED=$2; // Don't unmap pages.
 OBJPR_NOTWIRED =$4; // Don't remove wired pages.

procedure vm_object_reference   (obj:vm_object_t);
function  vm_object_allocate    (t:objtype_t;size:vm_pindex_t):vm_object_t;
procedure vm_object_deallocate  (obj:vm_object_t);

procedure vm_object_set_flag    (obj:vm_object_t;bits:Word);
procedure vm_object_clear_flag  (obj:vm_object_t;bits:Word);

procedure vm_object_pip_add     (obj:vm_object_t;i:word);
procedure vm_object_pip_subtract(obj:vm_object_t;i:word);
procedure vm_object_pip_wakeup  (obj:vm_object_t);
procedure vm_object_pip_wakeupn (obj:vm_object_t;i:word);
procedure vm_object_pip_wait    (obj:vm_object_t;waitid:pchar);

function  vm_object_page_clean(obj:vm_object_t;
                               start,__end:vm_ooffset_t;
                               flags:Integer):Boolean;

procedure vm_object_page_remove(obj:vm_object_t;
                                start:vm_pindex_t;
                                __end:vm_pindex_t;
                                options:Integer);

procedure vm_object_collapse   (obj:vm_object_t);

function  VM_OBJECT_MTX        (obj:vm_object_t):p_mtx;
procedure VM_OBJECT_LOCK       (obj:vm_object_t);
function  VM_OBJECT_TRYLOCK    (obj:vm_object_t):Boolean;
procedure VM_OBJECT_UNLOCK     (obj:vm_object_t);
function  VM_OBJECT_LOCKED     (obj:vm_object_t):Boolean;
procedure VM_OBJECT_LOCK_ASSERT(obj:vm_object_t);

function  IDX_TO_OFF(x:DWORD):QWORD; inline;
function  OFF_TO_IDX(x:QWORD):DWORD; inline;

function  vm_object_coalesce(prev_object:vm_object_t;
                             prev_offset:vm_ooffset_t;
                             prev_size  :vm_ooffset_t;
                             next_size  :vm_ooffset_t;
                             reserved   :Boolean):Boolean;

implementation

uses
 vmparam,
 vnode,
 vfs_subr,
 vm_patch_link;

function IDX_TO_OFF(x:DWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function VM_OBJECT_MTX(obj:vm_object_t):p_mtx;
begin
 Result:=@obj^.mtx;
end;

procedure VM_OBJECT_LOCK(obj:vm_object_t);
begin
 mtx_lock(obj^.mtx);
end;

function VM_OBJECT_TRYLOCK(obj:vm_object_t):Boolean;
begin
 Result:=mtx_trylock(obj^.mtx);
end;

procedure VM_OBJECT_UNLOCK(obj:vm_object_t);
begin
 mtx_unlock(obj^.mtx);
end;

function VM_OBJECT_LOCKED(obj:vm_object_t):Boolean;
begin
 Result:=(mtx_owned(obj^.mtx));
end;

procedure VM_OBJECT_LOCK_ASSERT(obj:vm_object_t);
begin
 Assert(mtx_owned(obj^.mtx));
end;

procedure vm_object_set_flag(obj:vm_object_t;bits:Word);
begin
 VM_OBJECT_LOCK_ASSERT(obj);
 obj^.flags:=obj^.flags or bits;
end;

procedure vm_object_clear_flag(obj:vm_object_t;bits:Word);
begin
 VM_OBJECT_LOCK_ASSERT(obj);
 obj^.flags:=obj^.flags and (not bits);
end;

function vm_object_allocate(t:objtype_t;size:vm_pindex_t):vm_object_t;
begin
 Result:=AllocMem(SizeOf(_vm_object));

 mtx_init(Result^.mtx,'vm_object');

 TAILQ_INIT(@Result^.memq);
 TAILQ_INIT(@Result^.patchq);

 Result^.otype     :=t;
 Result^.size      :=size;
 Result^.generation:=1;
 Result^.ref_count :=1;

 if (t=OBJT_DEFAULT) then
 begin
  Result^.flags:=OBJ_ONEMAPPING;
 end;
end;

procedure vm_object_destroy(obj:vm_object_t);
begin
 Case obj^.otype of
  OBJT_DEFAULT:;
 end;
 mtx_destroy(obj^.mtx);
 FreeMem(obj);
end;

procedure vm_object_reference(obj:vm_object_t);
begin
 if (obj=nil) then Exit;

 System.InterlockedIncrement(obj^.ref_count);
end;

{
 vm_object_terminate actually destroys the specified object, freeing
 up all previously used resources.

 The object must be locked.
 This routine may block.
}
procedure vm_object_terminate(obj:vm_object_t);
var
 vp:p_vnode;
begin
 VM_OBJECT_LOCK_ASSERT(obj);

 {
  * Make sure no one uses us.
  }
 vm_object_set_flag(obj, OBJ_DEAD);

 {
  * wait for the pageout daemon to be done with the obj
  }
 vm_object_pip_wait(obj, 'objtrm');

 Assert(obj^.paging_in_progress=0,'vm_object_terminate: pageout in progress');

 vm_object_patch_remove(obj,0,0);

 {
  * Clean and free the pages, as appropriate. All references to the
  * obj are gone, so we don't need to lock it.
  }
 if (obj^.otype=OBJT_VNODE) then
 begin
  vp:=obj^.handle;

  {
   * Clean pages and flush buffers.
   }
  vm_object_page_clean(obj, 0, 0, OBJPC_SYNC);
  VM_OBJECT_UNLOCK(obj);

  vinvalbuf(vp, V_SAVE, 0, 0);

  VM_OBJECT_LOCK(obj);
 end;

 Assert(obj^.ref_count=0,'vm_object_terminate: obj with references');

 VM_OBJECT_UNLOCK(obj);

 vm_object_destroy(obj);
end;

procedure vm_object_deallocate(obj:vm_object_t);
var
 ref:Integer;
begin
 if (obj=nil) then Exit;

 ref:=System.InterlockedDecrement(obj^.ref_count);

 if (ref=1) then
 begin
  VM_OBJECT_LOCK(obj);
  vm_object_set_flag(obj, OBJ_ONEMAPPING);
  VM_OBJECT_UNLOCK(obj);
 end else
 if (ref=0) then
 if ((obj^.flags and OBJ_DEAD)=0) then
 begin
  VM_OBJECT_LOCK(obj);
  vm_object_terminate(obj);
 end;
end;

procedure vm_object_pip_add(obj:vm_object_t;i:word);
begin
 if (obj=nil) then Exit;

 VM_OBJECT_LOCK_ASSERT(obj);
 obj^.paging_in_progress:=obj^.paging_in_progress+i;
end;

procedure vm_object_pip_subtract(obj:vm_object_t;i:word);
begin
 if (obj=nil) then Exit;

 VM_OBJECT_LOCK_ASSERT(obj);
 obj^.paging_in_progress:=obj^.paging_in_progress-i;
end;

procedure vm_object_pip_wakeup(obj:vm_object_t);
begin
 if (obj=nil) then Exit;

 VM_OBJECT_LOCK_ASSERT(obj);
 Dec(obj^.paging_in_progress);
 if ((obj^.flags and OBJ_PIPWNT)<>0) and (obj^.paging_in_progress=0) then
 begin
  vm_object_clear_flag(obj, OBJ_PIPWNT);
  wakeup(obj);
 end;
end;

procedure vm_object_pip_wakeupn(obj:vm_object_t;i:word);
begin
 if (obj=nil) then Exit;

 VM_OBJECT_LOCK_ASSERT(obj);
 Dec(obj^.paging_in_progress,i);
 if ((obj^.flags and OBJ_PIPWNT)<>0) and (obj^.paging_in_progress=0) then
 begin
  vm_object_clear_flag(obj, OBJ_PIPWNT);
  wakeup(obj);
 end;
end;

procedure vm_object_pip_wait(obj:vm_object_t;waitid:pchar);
begin
 if (obj=nil) then Exit;

 VM_OBJECT_LOCK_ASSERT(obj);
 while (obj^.paging_in_progress<>0) do
 begin
  obj^.flags:=obj^.flags or OBJ_PIPWNT;
  msleep(obj, VM_OBJECT_MTX(obj), PVM, waitid, 0);
 end;
end;

{
 vm_object_page_clean

 Clean all dirty pages in the specified range of object.  Leaves page
 on whatever queue it is currently on.   If NOSYNC is set then do not
 write out pages with VPO_NOSYNC set (originally comes from MAP_NOSYNC),
 leaving the object dirty.

 When stuffing pages asynchronously, allow clustering.  XXX we need a
 synchronous clustering mode implementation.

 Odd semantics: if start == end, we clean everything.

 The object must be locked.

 Returns FALSE if some page from the range was not written, as
 reported by the pager, and TRUE otherwise.
}

function  vm_object_page_clean(obj:vm_object_t;
                               start,__end:vm_ooffset_t;
                               flags:Integer):Boolean;
begin
 Result:=True;
end;

{
 vm_object_page_remove:

 For the given object, either frees or invalidates each of the
 specified pages.  In general, a page is freed.  However, if a page is
 wired for any reason other than the existence of a managed, wired
 mapping, then it may be invalidated but not removed from the object.
 Pages are specified by the given range ["start", "end") and the option
 OBJPR_CLEANONLY.  As a special case, if "end" is zero, then the range
 extends from "start" to the end of the object.  If the option
 OBJPR_CLEANONLY is specified, then only the non-dirty pages within the
 specified range are affected.  If the option OBJPR_NOTMAPPED is
 specified, then the pages within the specified range must have no
 mappings.  Otherwise, if this option is not specified, any mappings to
 the specified pages are removed before the pages are freed or
 invalidated.

 In general, this operation should only be performed on objects that
 contain managed pages.  There are, however, two exceptions.  First, it
 is performed on the kernel and kmem objects by vm_map_entry_delete().
 Second, it is used by msync(..., MS_INVALIDATE) to invalidate device-
 backed pages.  In both of these cases, the option OBJPR_CLEANONLY must
 not be specified and the option OBJPR_NOTMAPPED must be specified.

 The object must be locked.
}

procedure vm_object_page_remove(obj:vm_object_t;
                                start:vm_pindex_t;
                                __end:vm_pindex_t;
                                options:Integer);
begin
 vm_object_patch_remove(obj,start,__end);
end;

{
 vm_object_collapse:

 Collapse an object with the object backing it.
 Pages in the backing object are moved into the
 parent, and the backing object is deallocated.
}

procedure vm_object_collapse(obj:vm_object_t);
begin
 //
end;

{
 Routine: vm_object_coalesce
 Function: Coalesces two objects backing up adjoining
   regions of memory into a single object.

 returns TRUE if objects were combined.

 NOTE: Only works at the moment if the second object is NULL -
  if it's not, which object do we lock first?

 Parameters:
  prev_object First object to coalesce
  prev_offset Offset into prev_object
  prev_size Size of reference to prev_object
  next_size Size of reference to the second object
  reserved Indicator that extension region has
    swap accounted for

 Conditions:
 The object must *not* be locked.
}

function vm_object_coalesce(prev_object:vm_object_t;
                            prev_offset:vm_ooffset_t;
                            prev_size  :vm_ooffset_t;
                            next_size  :vm_ooffset_t;
                            reserved   :Boolean):Boolean;
var
 next_pindex:vm_pindex_t;
begin
 if (prev_object=nil) then Exit(TRUE);

 VM_OBJECT_LOCK(prev_object);
 if (prev_object^.otype<>OBJT_DEFAULT) then
 begin
  VM_OBJECT_UNLOCK(prev_object);
  Exit(FALSE);
 end;

 {
  * Try to collapse the object first
  }
 vm_object_collapse(prev_object);

 prev_size:=prev_size shr PAGE_SHIFT;
 next_size:=next_size shr PAGE_SHIFT;
 next_pindex:=OFF_TO_IDX(prev_offset) + prev_size;

 if (prev_object^.ref_count > 1) and
    (prev_object^.size<>next_pindex) then
 begin
  VM_OBJECT_UNLOCK(prev_object);
  Exit(FALSE);
 end;

 {
  * Extend the object if necessary.
  }
 if (next_pindex + next_size > prev_object^.size) then
 begin
  prev_object^.size:=next_pindex + next_size;
 end;

 VM_OBJECT_UNLOCK(prev_object);
 Result:=(TRUE);
end;

end.

