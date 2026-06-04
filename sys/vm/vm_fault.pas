unit vm_fault;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_thr,
 vm,
 vmparam,
 vm_map,
 vm_pmap,
 vm_object;

function vm_fault_hold(map       :vm_map_t;
                       mem_addr  :vm_offset_t;
                       rip_addr  :vm_offset_t;
                       fault_type:vm_prot_t):Integer;


function vm_fault_wire(map  :vm_map_t;
                       start:vm_offset_t;
                       __end:vm_offset_t):Integer;

implementation

uses
 systm,
 trap,
 x86_fpdbgdisas,
 kern_stub,
 vm_patch_link;

function vm_check_patch_entry(map:vm_map_t;vaddr:vm_offset_t;p_entry:p_vm_map_entry_t):Boolean;
var
 entry:vm_map_entry_t;
begin
 if (vm_map_lookup_entry(map,vaddr,@entry)) then
 begin
  p_entry^:=entry;
  Result:=(entry^.inheritance=VM_INHERIT_PATCH);
 end else
 begin
  p_entry^:=nil;
  Result:=True;
 end;
end;

function AlignUp(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
var
 tmp:PtrUInt;
begin
 if (alignment=0) then Exit(addr);
 tmp:=addr+PtrUInt(alignment-1);
 Result:=tmp-(tmp mod alignment)
end;

function AlignDw(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
begin
 Result:=addr-(addr mod alignment);
end;

function vm_fault_internal(map        :vm_map_t;
                           mem_addr   :vm_offset_t;
                           mem__end   :vm_offset_t;
                           rip_addr   :vm_offset_t;
                           fault_type :vm_prot_t;
                           fault_flags:Integer):Integer;
label
 RetryFault;
var
 growstack:Boolean;
 entry:vm_map_entry_t;
 obj:vm_object_t;
 pindex:vm_pindex_t;
 prot:vm_prot_t;
 wired:Boolean;
begin
 growstack:=true;

 RetryFault:

 Result:=vm_map_lookup(@map,
                       mem_addr,
                       fault_type,
                       @entry,
                       @obj,
                       @pindex,
                       @prot,
                       @wired);

 if (Result<>KERN_SUCCESS) then
 begin
  if growstack and
    (Result=KERN_INVALID_ADDRESS) then
  begin
   Result:=vm_map_growstack(map, mem_addr);
   if (Result<>KERN_SUCCESS) then
   begin
    Exit(KERN_FAILURE);
   end;
   growstack:=false;
   goto RetryFault;
  end;
  Exit();
 end;

 if ((entry^.eflags and MAP_ENTRY_NOFAULT)<>0) then
 begin
  if ((curkthread^.td_pflags and TDP_DEVMEMIO)<>0) then
  begin
   vm_map_lookup_done(map,entry);
   Exit(KERN_FAILURE);
  end;
  Assert(false,'vm_fault: fault on nofault entry 0x'+HexStr(mem_addr,16));
 end;

 //Next, various actions with a vm map

 if (wired) then
 begin
  fault_type:=prot or (fault_type and VM_PROT_COPY);
 end;

 if ((fault_type and (VM_PROT_COPY or VM_PROT_WRITE)) <> 0) then
 if (obj^.backing_object<>nil) then
 begin
  pmap_copy_pages(map^.pmap,mem_addr,mem__end,prot);
 end;

 if (Result=KERN_SUCCESS) then
 if is_guest_addr(rip_addr) then
 begin
  //Result:=vm_try_jit_patch(map,mem_addr,rip_addr);
 end;

 vm_map_lookup_done(map,entry);
end;

function vm_fault_hold(map       :vm_map_t;
                       mem_addr  :vm_offset_t;
                       rip_addr  :vm_offset_t;
                       fault_type:vm_prot_t):Integer;
begin
 mem_addr:=trunc_page(mem_addr);
 Exit(vm_fault_internal(map,mem_addr,mem_addr+PAGE_SIZE,rip_addr,fault_type,VM_FAULT_NORMAL));
end;

function vm_fault_wire(map  :vm_map_t;
                       start:vm_offset_t;
                       __end:vm_offset_t):Integer; public;
begin
 Exit(vm_fault_internal(map,start,__end,0,VM_PROT_NONE,VM_FAULT_CHANGE_WIRING));
end;

function OFF_TO_IDX(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

procedure vm_fault_copy_entry(dst_map,src_map:vm_map_t;
                              dst_entry,src_entry:vm_map_entry_t;
                              fork_charge:p_vm_ooffset_t); public;
var
 dst_obj, src_obj:vm_object_t;
 access, prot:vm_prot_t;
 upgrade:boolean;
begin

 upgrade:=src_entry=dst_entry;

 src_obj:=src_entry^.vm_obj;

 {
  * Create the top-level obj for the destination entry. (Doesn't
  * actually shadow anything - we copy the pages directly.)
  }
 dst_obj:=vm_object_allocate(OBJT_DEFAULT, OFF_TO_IDX(dst_entry^.__end - dst_entry^.start));

 dst_obj^.flags:=dst_obj^.flags or OBJ_COLORED;
 dst_obj^.pg_color:=atop(dst_entry^.start);

 VM_OBJECT_LOCK(dst_obj);
 Assert(upgrade or (dst_entry^.vm_obj=nil),'vm_fault_copy_entry: vm_object not nil');

 dst_entry^.vm_obj:=dst_obj;
 dst_entry^.offset:=0;
 dst_obj^.charge:=dst_entry^.__end - dst_entry^.start;

 if (fork_charge<>nil) then
 begin
  Assert(dst_entry^.cred=False,'vm_fault_copy_entry: leaked swp charge');
  dst_obj^.cred:=True;
  fork_charge^:=fork_charge^ + dst_obj^.charge;
 end else
 begin
  dst_obj^.cred:=dst_entry^.cred;
  dst_entry^.cred:=False;
 end;
 prot:=dst_entry^.protection;
 access:=prot;

 {
  * If not an upgrade, then enter the mappings in the pmap as
  * read and/or execute accesses.  Otherwise, enter them as
  * write accesses.
  *
  * A writeable large page mapping is only created if all of
  * the constituent small page mappings are modified. Marking
  * PTEs as modified on inception allows promotion to happen
  * without taking potentially large number of soft faults.
  }
 if (not upgrade) then
 begin
  access:=access and (not VM_PROT_WRITE);
 end;

 if (upgrade) then
 begin
  pmap_copy_pages(dst_map^.pmap,dst_entry^.start,dst_entry^.__end,access);
 end;

 VM_OBJECT_UNLOCK(dst_obj);

 if (upgrade) then
 begin
  dst_entry^.eflags:=dst_entry^.eflags and (not (MAP_ENTRY_COW or MAP_ENTRY_NEEDS_COPY));
  vm_object_deallocate(src_obj);
 end;
end;



end.

