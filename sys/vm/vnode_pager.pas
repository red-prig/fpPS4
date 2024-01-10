unit vnode_pager;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vnode,
 vm,
 vmparam,
 sys_vm_object;

function  vnode_pager_alloc(handle:Pointer;
                            size:vm_ooffset_t;
                            prot:vm_prot_t;
                            offset:vm_ooffset_t):vm_object_t;

function  vnode_create_vobject(vp:p_vnode;isize:vm_ooffset_t):Integer;

procedure vnode_destroy_vobject(vp:p_vnode);

procedure vnode_pager_dealloc(obj:vm_object_t);

procedure vnode_pager_setsize(vp:p_vnode;nsize:vm_ooffset_t);

implementation

uses
 vnode_if,
 vfs_subr,
 vfs_vnops,
 kern_synch,
 kern_param,
 kern_mtx;

//

procedure vm_object_terminate(obj:vm_object_t); external;
procedure vm_pager_deallocate(obj:vm_object_t); external;
procedure vm_object_pip_wait(obj:vm_object_t;waitid:pchar); external;

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
 * Allocate (or lookup) pager for a vnode.
 * Handle is a vnode pointer.
 *
 * MPSAFE
 }

function vnode_pager_alloc(handle:Pointer;
                           size:vm_ooffset_t;
                           prot:vm_prot_t;
                           offset:vm_ooffset_t):vm_object_t;
label
 retry;
var
 obj:vm_object_t;
 vp:p_vnode;
begin
 {
  * Pageout to vnode, no can do yet.
  }
 if (handle=nil) then Exit(nil);

 vp:=handle;

 {
  * If the obj is being terminated, wait for it to
  * go away.
  }
retry:
 obj:=vp^.v_object;
 while (obj<>nil) do
 begin
  VM_OBJECT_LOCK(obj);
  if ((obj^.flags and OBJ_DEAD)=0) then
  begin
   break;
  end;
  vm_object_set_flag(obj, OBJ_DISCONNECTWNT);
  msleep(obj, VM_OBJECT_MTX(obj), PDROP or PVM, 'vadead', 0);
  obj:=vp^.v_object;
 end;

 Assert(vp^.v_usecount<>0, 'vnode_pager_alloc: no vnode reference');

 if (obj=nil) then
 begin
  {
   * Add an obj of the appropriate size
   }
  obj:=vm_object_allocate(OBJT_VNODE, OFF_TO_IDX(round_page(size)));

  obj^.un_pager.vnp.vnp_size:=size;
  obj^.un_pager.vnp.writemappings:=0;

  obj^.handle:=handle;

  VI_LOCK(vp);
  if (vp^.v_object<>nil) then
  begin
   // Obj has been created while we were sleeping
   VI_UNLOCK(vp);
   VM_OBJECT_LOCK(obj);
   Assert(obj^.ref_count=1, 'leaked ref %p %d');

   obj^.otype:=OBJT_DEAD;
   obj^.ref_count:=0;
   VM_OBJECT_UNLOCK(obj);
   vm_object_destroy(obj);
   goto retry;
  end;

  vp^.v_object:=obj;
  VI_UNLOCK(vp);
 end else
 begin
  Inc(obj^.ref_count);
  VM_OBJECT_UNLOCK(obj);
 end;

 vref(vp);
 Exit(obj);
end;

{ Create the VM system backing object for this vnode }
function vnode_create_vobject(vp:p_vnode;isize:vm_ooffset_t):Integer;
var
 obj:vm_object_t;
 size:QWORD;
 va:t_vattr;
begin
 size:=isize;

 if (not vn_isdisk(vp, nil)) and (vn_canvmio(vp)=FALSE) then
 begin
  Exit(0);
 end;

 obj:=vp^.v_object;
 while (obj<>nil) do
 begin
  VM_OBJECT_LOCK(obj);
  if ((obj^.flags and OBJ_DEAD)=0) then
  begin
   VM_OBJECT_UNLOCK(obj);
   Exit(0);
  end;
  VOP_UNLOCK(vp, 0);
  vm_object_set_flag(obj, OBJ_DISCONNECTWNT);
  msleep(obj, VM_OBJECT_MTX(obj), PDROP or PVM, 'vodead', 0);
  vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
  //
  obj:=vp^.v_object;
 end;

 if (size=0) then
 begin
  if (vn_isdisk(vp, nil)) then
  begin
   size:=IDX_TO_OFF(High(Integer));
  end else
  begin
   if (VOP_GETATTR(vp, @va)<>0) then
   begin
    Exit(0);
   end;
   size:=va.va_size;
  end;
 end;

 obj:=vnode_pager_alloc(vp, size, 0, 0);

 {
  * Dereference the reference we just created.  This assumes
  * that the obj is associated with the vp.
  }
 VM_OBJECT_LOCK(obj);
  Dec(obj^.ref_count);
 VM_OBJECT_UNLOCK(obj);

 vrele(vp);

 Assert(vp^.v_object<>nil, 'vnode_create_vobject: nil obj');

 Exit(0);
end;

procedure vnode_destroy_vobject(vp:p_vnode);
var
 obj:vm_object_t;
begin
 obj:=vp^.v_object;
 if (obj=nil) then Exit;

 ASSERT_VOP_ELOCKED(vp, 'vnode_destroy_vobject');

 VM_OBJECT_LOCK(obj);
 if (obj^.ref_count=0) then
 begin
  {
   * vclean() may be called twice. The first time
   * removes the primary reference to the object,
   * the second time goes one further and is a
   * special-case to terminate the object.
   *
   * don't double-terminate the object
   }
  if ((obj^.flags and OBJ_DEAD)=0) then
   vm_object_terminate(obj)
  else
   VM_OBJECT_UNLOCK(obj);

 end else
 begin
  {
   * Woe to the process that tries to page now :-).
   }
  vm_pager_deallocate(obj);
  VM_OBJECT_UNLOCK(obj);
 end;
 vp^.v_object:=nil;
end;


{
 * The object must be locked.
 }
procedure vnode_pager_dealloc(obj:vm_object_t);
var
 vp:p_vnode;
 refs:Integer;
begin
 vp:=obj^.handle;

 if (vp=nil) then
 begin
  Writeln(StdErr,'vnode_pager_dealloc: pager already dealloced');
  Exit;
 end;

 VM_OBJECT_LOCK_ASSERT(obj);
 vm_object_pip_wait(obj, 'vnpdea');
 refs:=obj^.ref_count;

 obj^.handle:=nil;
 obj^.otype:=OBJT_DEAD;

 if ((obj^.flags and OBJ_DISCONNECTWNT)<>0) then
 begin
  vm_object_clear_flag(obj, OBJ_DISCONNECTWNT);
  wakeup(obj);
 end;

 ASSERT_VOP_ELOCKED(vp, 'vnode_pager_dealloc');

 if (obj^.un_pager.vnp.writemappings > 0) then
 begin
  obj^.un_pager.vnp.writemappings:=0;
  VOP_ADD_WRITECOUNT(vp, -1);
 end;
 vp^.v_object:=nil;
 //VOP_UNSET_TEXT(vp);
 VM_OBJECT_UNLOCK(obj);

 while (refs>0) do
 begin
  vunref(vp);
  Dec(refs);
 end;

 VM_OBJECT_LOCK(obj);
end;

{
 Lets the VM system know about a change in size for a file.
 We adjust our own internal size and flush any cached pages in
 the associated object that are affected by the size change.

 Note: this routine may be invoked as a result of a pager put
 operation (possibly at object termination time), so we must be careful.
}
procedure vnode_pager_setsize(vp:p_vnode;nsize:vm_ooffset_t);
var
 obj:vm_object_t;
 nobjsize:DWORD;
begin
 obj:=vp^.v_object;
 if (obj=nil) then Exit;

 nobjsize:=OFF_TO_IDX(nsize + PAGE_MASK);

 VM_OBJECT_LOCK(obj);

 if (obj^.otype=OBJT_DEAD) then
 begin
  VM_OBJECT_UNLOCK(obj);
  Exit;
 end;

 Assert(obj^.otype=OBJT_VNODE,'not vnode-backed obj %p');

 obj^.un_pager.vnp.vnp_size:=nsize;
 obj^.size:=nobjsize;

 VM_OBJECT_UNLOCK(obj);
end;


end.



