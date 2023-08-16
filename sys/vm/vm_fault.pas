unit vm_fault;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_thr,
 vm,
 vm_map,
 vm_object;

function vm_try_jit_patch(map:vm_map_t;vaddr:vm_offset_t):Integer;

function vm_check_patch(map:vm_map_t;vaddr:vm_offset_t):Boolean;

function vm_fault(map        :vm_map_t;
                  vaddr      :vm_offset_t;
                  fault_type :vm_prot_t;
                  fault_flags:Integer):Integer;

implementation

uses
 systm,
 x86_fpdbgdisas;

function vm_try_jit_patch(map:vm_map_t;vaddr:vm_offset_t):Integer;
var
 err:Integer;
 data:array[0..15] of Byte;

 dis:TX86Disassembler;
 din:TInstruction;
 ptr:Pointer;

 rv:Integer;
begin
 err:=copyin(Pointer(vaddr),@data,SizeOf(data));
 if (err<>0) then Exit(KERN_PROTECTION_FAILURE);

 dis:=Default(TX86Disassembler);
 din:=Default(TInstruction);

 ptr:=@data;
 dis.Disassemble(dm64,ptr,din);

 Result:=0;
end;

function vm_check_patch(map:vm_map_t;vaddr:vm_offset_t):Boolean;
var
 entry:vm_map_entry_t;
begin
 Result:=False;

 vm_map_lock(map);

 entry:=nil;
 if vm_map_lookup_entry(map, vaddr, @entry) then
 begin
  Result:=(entry^.name='#patch'); //TODO create flag
 end;

 vm_map_unlock(map);
end;

function vm_fault(map        :vm_map_t;
                  vaddr      :vm_offset_t;
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
                       vaddr,
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
   Result:=vm_map_growstack(map, vaddr);
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
  Assert(false,'vm_fault: fault on nofault entry 0x'+HexStr(vaddr,16));
 end;

 //Next, various actions with a vm map

 vm_map_lookup_done(map,entry);
end;



end.

