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
 vm_patch_link,
 kern_jit;

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

const
 //EBFE jmp -$02
 pause_stub:DWORD=$9090FEEB;

procedure copy_xchg(src,dst:Pointer;vsize:Integer);
var
 data:t_data16;
begin
 case vsize of
  0:;
  1..3:
   begin
    data:=c_data16; //init
    PDWORD(@data)^:=PDWORD(dst)^; //read mask
    Move(src^,data,vsize);        //set new
    System.InterlockedExchange(PDWORD(dst)^,PDWORD(@data)^);
   end;
  4:
   begin
    System.InterlockedExchange(PDWORD(dst)^,PDWORD(src)^);
   end;
  5..7:
   begin
    data:=c_data16; //init
    PQWORD(@data)^:=PQWORD(dst)^; //read mask
    Move(src^,data,vsize);        //set new
    System.InterlockedExchange64(PQWORD(dst)^,PQWORD(@data)^);
   end;
  8:
   begin
    System.InterlockedExchange64(PQWORD(dst)^,PQWORD(src)^);
   end;
  9..16:
   begin
    System.InterlockedExchange(PDWORD(dst)^,pause_stub);

    Move(PDWORD(src)[1],PDWORD(dst)[1],vsize-SizeOf(DWORD));

    System.InterlockedExchange(PDWORD(dst)^,PDWORD(src)^);
   end;
  else
   begin
    Writeln('copy_xchg:',vsize);
    Assert(false,'copy_xchg');
   end;
 end;
end;

procedure patch_original(map:vm_map_t;
                         entry:vm_map_entry_t;
                         vaddr:vm_offset_t;
                         vsize:Integer;
                         vsrc :Pointer);
var
 start   :vm_offset_t;
 __end   :vm_offset_t;
 new_prot:vm_prot_t;
begin
 new_prot:=entry^.protection or VM_PROT_READ or VM_PROT_WRITE;

 if (new_prot<>entry^.protection) then
 begin
  start:=AlignDw(vaddr,PAGE_SIZE);
  __end:=AlignUp(vaddr+vsize,PAGE_SIZE);

  pmap_protect(map^.pmap,
               start,
               __end,
               new_prot,
               entry^.protection);
 end;

 copy_xchg(vsrc,Pointer(vaddr),vsize);

 md_cacheflush(Pointer(vaddr),vsize,ICACHE);

 if (new_prot<>entry^.protection) then
 begin
  pmap_protect(map^.pmap,
               start,
               __end,
               entry^.protection,
               new_prot);
 end;
end;

procedure patch_original(map:vm_map_t;
                         entry:vm_map_entry_t;
                         vaddr:vm_offset_t;
                         vsize:Integer;
                         delta:Integer);
var
 trampoline:t_jmp32_trampoline;
begin
 trampoline:=c_jmpl32_trampoline;
 trampoline.addr:=delta;

 patch_original(map,entry,vaddr,vsize,@trampoline);
end;

function vm_try_jit_patch(map:vm_map_t;
                          mem_addr:vm_offset_t;
                          rip_addr:vm_offset_t):Integer;
var
 err:Integer;
 vsize:Integer;

 entry:vm_map_entry_t;
 obj:vm_map_object;

 tmp_jit:p_jit_code;

 ctx:t_jit_context;
 ptr:Pointer;

 chunk_prolog:p_stub_chunk;
 chunk_jit   :p_stub_chunk;

 jit_prolog:p_jit_prolog;

 delta:Int64;

 rip_addr_jmp:vm_offset_t;

 mask:DWORD;

 info:t_patch_info;
begin
 Result:=0;

 info:=vm_get_patch_link(Pointer(rip_addr),0,pt_jit_frame);

 //Is the exception already patched?
 if (info.stub<>nil) then
 begin
  tmp_jit:=@info.stub^.body;

  if (tmp_jit^.prolog=nil) then
  begin
   //Prologue not created? Switch code on exit
   switch_to_jit(@tmp_jit^.frame);
  end;

  p_dec_ref(info.stub); //release (vm_get_patch_link)
  Exit(0);
 end;

 //Did the exception happen inside a patch? just going out
 if vm_check_patch_entry(map,rip_addr,@entry) then
 begin
  Exit(0);
 end;

 obj:=entry^.vm_obj;
 Assert(obj<>nil,'vm_try_jit_patch (obj=nil)');

 Writeln('mmaped addr 0x',HexStr(mem_addr,16),' to 0x',HexStr(uplift(Pointer(mem_addr))));

 ctx:=Default(t_jit_context);
 ctx.Code:=c_data16;

 err:=copyin_nofault(Pointer(rip_addr),@ctx.Code,SizeOf(ctx.Code));
 if (err<>0) then Exit(KERN_PROTECTION_FAILURE);

 ptr:=@ctx.Code;
 ctx.dis.Disassemble(dm64,ptr,ctx.din);

 vsize:=ctx.dis.CodeIdx;

 print_disassemble(@ctx.Code,vsize);

 //Get prev patch
 info:=vm_get_patch_link(Pointer(rip_addr-1),1,pt_jit_frame);

 if (info.stub<>nil) then
 begin

  //Overlap?
  if (info.vsize<5) then
  begin
   //delete patch

   tmp_jit:=@info.stub^.body;

   patch_original(map,entry,QWORD(info.vaddr),tmp_jit^.o_len,@tmp_jit^.o_data);

   vm_rem_patch_link(obj,info.vaddr);
  end;

  p_dec_ref(info.stub); //release (vm_get_patch_link)
 end;

 rip_addr_jmp:=rip_addr+SizeOf(t_jmp32_trampoline);

 chunk_prolog:=nil;

 case vsize of
  1..2:
    begin
     //Not possible
     chunk_prolog:=nil;
    end;
  3:
    begin
     //Overlapping jmp instructions [00 11 22] [MM MM]
     mask:=PWORD(@ctx.Code[3])^;
     mask:=mask shl 16;
     mask:=mask or 1;

     chunk_prolog:=p_alloc_m(Pointer(rip_addr_jmp),SizeOf(t_jit_prolog),mask);
    end;
  4:
    begin
     //Overlapping jmp instructions [00 11 22 33] [MM]
     mask:=ctx.Code[4];
     mask:=mask shl 24;

     chunk_prolog:=p_alloc_m(Pointer(rip_addr_jmp),SizeOf(t_jit_prolog),mask);
    end;
  5..13:
    begin
     //Near 32bit jmp instructions

     chunk_prolog:=p_alloc(Pointer(rip_addr_jmp),SizeOf(t_jit_prolog));
    end;
  14..16:
    begin
     //Far 64bit jmpq 0(%rip) TODO

     chunk_prolog:=p_alloc(Pointer(rip_addr_jmp),SizeOf(t_jit_prolog));
    end;
  else
    Assert(false,'vm_try_jit_patch (vsize)');
 end;

 ctx.rip_addr:=rip_addr;
 chunk_jit:=generate_jit(ctx);

 if (chunk_prolog=nil) then
 begin
  //Prologue not created?
 end else
 begin
  jit_prolog:=@chunk_prolog^.body;
  jit_prolog^:=c_jit_prolog;
  jit_prolog^.jframe:=@ctx.jit_code^.frame;

  ctx.jit_code^.prolog:=chunk_prolog;

  delta:=Int64(jit_prolog)-(Int64(rip_addr_jmp));

  if (vsize<5) then
  begin
   Assert(is_mask_valid(Pointer(rip_addr_jmp),jit_prolog,mask),'vm_try_jit_patch (is_mask_valid)');
  end else
  begin
   Assert(is_near_valid(Pointer(rip_addr_jmp),jit_prolog),'vm_try_jit_patch (is_near_valid)');
  end;

  patch_original(map,entry,rip_addr,vsize,Integer(delta));

  vm_add_patch_link(obj,Pointer(rip_addr),vsize,pt_jit_prolog,chunk_prolog);
 end;

 vm_add_patch_link(obj,Pointer(rip_addr),vsize,pt_jit_frame,chunk_jit);

 //Switch code on exit
 switch_to_jit(@ctx.jit_code^.frame);
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
  Result:=vm_try_jit_patch(map,mem_addr,rip_addr);
 end;

 vm_map_lookup_done(map,entry);
end;



end.

