unit vm_pager;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 sys_vm_object;

function  vm_pager_allocate(otype:objtype_t;
                           handle:Pointer;
                           size:vm_ooffset_t;
                           prot:vm_prot_t;
                           off:vm_ooffset_t):vm_object_t;

procedure vm_pager_deallocate(obj:vm_object_t);

implementation

uses
 vmparam,
 device_pager;

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function vm_pager_allocate(otype:objtype_t;
                           handle:Pointer;
                           size:vm_ooffset_t;
                           prot:vm_prot_t;
                           off:vm_ooffset_t):vm_object_t;
begin
 case otype of
  OBJT_DEFAULT  :Result:=vm_object_allocate(otype,OFF_TO_IDX(size));
  //OBJT_SWAP     :;
  //OBJT_VNODE    :;
  OBJT_DEVICE   :Result:=dev_pager_alloc(handle,size,prot,off);
  //OBJT_PHYS     :;
  //OBJT_DEAD     :;
  //OBJT_SG       :;
  //OBJT_JITSHM   :;
  OBJT_SELF     :Result:=vm_object_allocate(otype,OFF_TO_IDX(size));
  //OBJT_TRCMEM   :;
  //OBJT_PHYSHM   :;
  //OBJT_BLOCKPOOL:;
  else
       Assert(False);
 end;
end;

procedure default_dealloc(obj:vm_object_t); inline;
begin
 obj^.handle:=nil;
 obj^.otype:=OBJT_DEAD;
end;

procedure vm_pager_deallocate(obj:vm_object_t); public;
begin
 if (obj=nil) then Exit;

 case obj^.otype of
  OBJT_DEFAULT  :default_dealloc(obj);
  OBJT_SWAP     :;
  OBJT_VNODE    :;
  OBJT_DEVICE   :dev_pager_dealloc(obj);
  OBJT_PHYS     :;
  OBJT_DEAD     :;
  OBJT_SG       :;
  OBJT_JITSHM   :;
  OBJT_SELF     :default_dealloc(obj);
  OBJT_TRCMEM   :;
  OBJT_PHYSHM   :;
  OBJT_BLOCKPOOL:;
 end;
end;


end.

