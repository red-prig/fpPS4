unit device_pager;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 sys_vm_object;

function dev_pager_alloc(
          handle:Pointer;
          size  :vm_ooffset_t;
          prot  :vm_prot_t;
          foff  :vm_ooffset_t):vm_object_t;

procedure dev_pager_dealloc(obj:vm_object_t);

implementation

uses
 errno,
 vmparam,
 vm_pmap,
 sys_conf;

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function old_dev_pager_ctor(
          handle:Pointer;
          size  :vm_ooffset_t;
          prot  :vm_prot_t;
          foff  :vm_ooffset_t;
          color :pword):Integer;
var
 dev:p_cdev;
 csw:p_cdevsw;
 dummy:vm_memattr_t;
 off:vm_ooffset_t;
 paddr:vm_paddr_t;
 npages:DWORD;
 ref:Integer;
begin
 {
  * Make sure this device can be mapped.
  }
 dev:=handle;
 csw:=dev_refthread(dev, @ref);
 if (csw=nil) then
 begin
  Exit(ENXIO);
 end;

 {
  * Check that the specified range of the device allows the desired
  * protection.
  *
  * XXX assumes VM_PROT_*=PROT_*
  }
 npages:=OFF_TO_IDX(size);
 paddr:=0; { Make paddr initialized for the case of size=0. }

 off:=foff;
 while (npages<>0) do
 begin
  if (csw^.d_mmap(dev, off, @paddr, prot, @dummy)<>0) then
  begin
   dev_relthread(dev, ref);
   Exit(EINVAL);
  end;
  //
  Dec(npages);
  Inc(off,PAGE_SIZE);
 end;

 dev_ref(dev);
 dev_relthread(dev, ref);

 color^:=atop(paddr) - OFF_TO_IDX(off - PAGE_SIZE);
 Exit(0);
end;

procedure old_dev_pager_dtor(handle:Pointer);
begin
 dev_rel(handle);
end;

function cdev_pager_allocate(
          handle:Pointer;
          tp    :obj_type;
          size  :vm_ooffset_t;
          prot  :vm_prot_t;
          foff  :vm_ooffset_t):vm_object_t;
var
 obj,obj1:vm_object_t;
 pindex:vm_pindex_t;
 color:word;
begin
 if (tp<>OBJT_DEVICE) then
 begin
  Exit(nil);
 end;

 {
  * Offset should be page aligned.
  }
 if ((foff and PAGE_MASK)<>0) then
 begin
  Exit(nil);
 end;

 size:=round_page(size);
 pindex:=OFF_TO_IDX(foff + size);

 if (old_dev_pager_ctor(handle, size, prot, foff, @color)<>0) then
 begin
  Exit(nil);
 end;

 //mtx_lock(@dev_pager_mtx);

 {
  * Look up pager, creating as necessary.
  }
 obj1:=nil;
 obj:=nil;
 //obj:=vm_pager_object_lookup(@dev_pager_object_list, handle);
 if (obj=nil) then
 begin
  {
   * Allocate obj and associate it with the pager.  Initialize
   * the obj's pg_color based upon the physical address of the
   * device's memory.
   }
  //mtx_unlock(@dev_pager_mtx);

  obj1:=vm_object_allocate(tp, pindex);
  obj1^.flags:=obj1^.flags or OBJ_COLORED;
  obj1^.pg_color:=color;
  obj1^.handle:=handle;
  //obj1^.un_pager.devp.ops:=ops;
  //TAILQ_INIT(@obj1^.un_pager.devp.devp_pglist);
  //mtx_lock(@dev_pager_mtx);

  obj:=nil;
  //obj:=vm_pager_object_lookup(@dev_pager_object_list, handle);
  if (obj<>nil) then
  begin
   {
    * We raced with other thread while allocating obj.
    }
   if (pindex > obj^.size) then
   begin
    obj^.size:=pindex;
   end;
  end else
  begin
   obj:=obj1;
   obj1:=nil;
   obj^.handle:=handle;
   //TAILQ_INSERT_TAIL(@dev_pager_object_list, obj, pager_object_list);
   Assert(obj^.otype=tp,'Inconsistent device pager otype');
  end;
 end else
 begin
  if (pindex > obj^.size) then
  begin
   obj^.size:=pindex;
  end;
 end;

 //mtx_unlock(@dev_pager_mtx);

 if (obj1<>nil) then
 begin
  obj1^.handle:=obj1;
  //mtx_lock(@dev_pager_mtx);
  //TAILQ_INSERT_TAIL(@dev_pager_object_list, obj1, pager_object_list);
  //mtx_unlock(@dev_pager_mtx);
  vm_object_deallocate(obj1);
 end;
 Exit(obj);
end;

function dev_pager_alloc(
          handle:Pointer;
          size  :vm_ooffset_t;
          prot  :vm_prot_t;
          foff  :vm_ooffset_t):vm_object_t;
begin
 Exit(cdev_pager_allocate(handle, OBJT_DEVICE, size, prot, foff));
end;

procedure dev_pager_dealloc(obj:vm_object_t);
begin
 //vm_page_t m;

 VM_OBJECT_UNLOCK(obj);
 old_dev_pager_dtor(obj^.handle);

 //mtx_lock(@dev_pager_mtx);
 //TAILQ_REMOVE(@dev_pager_object_list, obj, pager_object_list);
 //mtx_unlock(@dev_pager_mtx);
 VM_OBJECT_LOCK(obj);

 if (obj^.otype=OBJT_DEVICE) then
 begin
  {
   * Free up our fake pages.
   }
  //while ((m:=TAILQ_FIRST(@obj^.un_pager.devp.devp_pglist))<>nil) do
  //begin
  // dev_pager_free_page(obj, m);
  //end;
 end;
 obj^.handle:=nil;
 obj^.otype:=OBJT_DEAD;
end;



end.

