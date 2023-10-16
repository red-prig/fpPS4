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

function vm_fault(map        :vm_map_t;
                  mem_addr   :vm_offset_t;
                  rip_addr   :vm_offset_t;
                  fault_type :vm_prot_t;
                  fault_flags:Integer):Integer;

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

type
 p_jmp32_trampoline=^t_jmp32_trampoline;
 t_jmp32_trampoline=packed record
  inst:Byte;    //E9
  addr:Integer;
 end;

const
 c_jmpl32_trampoline:t_jmp32_trampoline=(inst:$E9;addr:0);

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

function vm_fault(map        :vm_map_t;
                  mem_addr   :vm_offset_t;
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
begin
 growstack:=true;

 RetryFault:

 Result:=vm_map_lookup(@map,
                       mem_addr,
                       fault_type,
                       @entry,
                       @obj,
                       @pindex,
                       @prot);

 if (Result<>KERN_SUCCESS) then
 begin
  if growstack and
    (Result=KERN_INVALID_ADDRESS) then
  begin
   Result:=vm_map_growstack(map, mem_addr);
   vm_map_lookup_done(map,entry);
   if (Result<>KERN_SUCCESS) then
   begin
    Exit(KERN_FAILURE);
   end;
   growstack:=false;
   goto RetryFault;
  end;
  vm_map_lookup_done(map,entry);
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

 if (Result=KERN_SUCCESS) then
 if is_guest_addr(rip_addr) then
 begin
  //Result:=vm_try_jit_patch(map,mem_addr,rip_addr);
 end;

 vm_map_lookup_done(map,entry);
end;



end.

