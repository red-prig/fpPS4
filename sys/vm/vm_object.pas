unit vm_object;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 sys_vm_object,
 kern_param;

procedure vm_object_deallocate  (obj:vm_object_t);

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

function  vm_object_coalesce(prev_object:vm_object_t;
                             prev_offset:vm_ooffset_t;
                             prev_size  :vm_ooffset_t;
                             next_size  :vm_ooffset_t;
                             reserved   :Boolean):Boolean;

implementation

uses
 vmparam,
 vnode,
 vnode_if,
 vmount,
 vfs_subr,
 vfs_vnops,
 kern_mtx;

//

procedure vm_pager_deallocate(obj:vm_object_t); external;

function  msleep(ident   :Pointer;
                 lock    :Pointer;
                 priority:Integer;
                 wmesg   :PChar;
                 timo    :Int64):Integer; external;

procedure wakeup(ident:Pointer); external;

//

function IDX_TO_OFF(x:DWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

{
 vm_object_terminate actually destroys the specified object, freeing
 up all previously used resources.

 The object must be locked.
 This routine may block.
}
procedure vm_object_terminate(obj:vm_object_t); public;
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

 vm_pager_deallocate(obj);
 VM_OBJECT_UNLOCK(obj);

 vm_object_destroy(obj);
end;

{
 Handle deallocating an object of type OBJT_VNODE.
}
procedure vm_object_vndeallocate(obj:vm_object_t);
var
 vp:p_vnode;
begin
 vp:=obj^.handle;

 VFS_ASSERT_GIANT(vp^.v_mount);
 VM_OBJECT_LOCK_ASSERT(obj);

 Assert(obj^.otype=OBJT_VNODE,'vm_object_vndeallocate: not a vnode obj');
 Assert(vp<>nil, 'vm_object_vndeallocate: missing vp');

 if (obj^.ref_count > 1) then
 begin
  VM_OBJECT_UNLOCK(obj);
  // vrele may need the vnode lock.
  vrele(vp);
 end else
 begin
  vhold(vp);
  VM_OBJECT_UNLOCK(obj);

  vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
  vdrop(vp);

  VM_OBJECT_LOCK(obj);

  Dec(obj^.ref_count);

  if (obj^.otype=OBJT_DEAD) then
  begin
   VM_OBJECT_UNLOCK(obj);
   VOP_UNLOCK(vp, 0);
  end else
  begin
   if (obj^.ref_count=0) then
   begin
    //VOP_UNSET_TEXT(vp);
   end;
   VM_OBJECT_UNLOCK(obj);
   vput(vp);
  end;
 end;
end;

{
 vm_object_deallocate:

 Release a reference to the specified object,
 gained either through a vm_object_allocate
 or a vm_object_reference call.  When all references
 are gone, storage associated with this object
 may be relinquished.

 No object may be locked.
}
procedure vm_object_deallocate(obj:vm_object_t); public;
label
 restart;
var
 vfslocked:Integer;
 vp:p_vnode;
begin
 if (obj=nil) then Exit;

 VM_OBJECT_LOCK(obj);

  if (obj^.otype=OBJT_VNODE) then
  begin
   restart:

    vp:=obj^.handle;

    vfslocked:=0;
    if VFS_NEEDSGIANT(vp^.v_mount) then
    begin
     vfslocked:=1;
     if not mtx_trylock(VFS_Giant) then
     begin
      VM_OBJECT_UNLOCK(obj);
      mtx_lock(VFS_Giant);
      goto restart;
     end;
    end;

    vm_object_vndeallocate(obj);
    VFS_UNLOCK_GIANT(vfslocked);
    Exit;
  end;

  Dec(obj^.ref_count);

  if (obj^.ref_count=1) then
  begin
   vm_object_set_flag(obj, OBJ_ONEMAPPING);
  end else
  if (obj^.ref_count=0) then
  if ((obj^.flags and OBJ_DEAD)=0) then
  begin
   vm_object_terminate(obj);
  end;

 VM_OBJECT_UNLOCK(obj);
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

procedure vm_object_pip_wait(obj:vm_object_t;waitid:pchar); public;
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
                               flags:Integer):Boolean; public;
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
                                options:Integer); public;
begin
 vm_object_patch_remove(obj,start,__end);
end;

{
 vm_object_collapse:

 Collapse an object with the object backing it.
 Pages in the backing object are moved into the
 parent, and the backing object is deallocated.
}

procedure vm_object_collapse(obj:vm_object_t); public;
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
                            reserved   :Boolean):Boolean; public;
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

