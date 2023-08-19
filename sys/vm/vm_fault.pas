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
 ucontext;

{
TRegValue = object
 AType : TRegisterType;
 ASize : TOperandSize;
 AIndex: Byte;
 AScale: Byte;
 //
 function StrValue:RawByteString;
end;
}

function GetFrameOffset(RegValue:TRegValue): Pointer;
begin
  Result := nil;

  case RegValue.AType of
    regNone:
    begin
      Result := nil;
    end;
    regRip:
    begin
      Result := @p_trapframe(nil)^.tf_rip;
    end;
    regOne:
    begin
      Result := nil; //'1';
    end;
    regGeneral:
    begin
      case RegValue.ASize of
        os8,
        os16,
        os32,
        os64:
        begin
          case RegValue.AIndex of
            0:Result := @p_trapframe(nil)^.tf_rax;
            1:Result := @p_trapframe(nil)^.tf_rcx;
            2:Result := @p_trapframe(nil)^.tf_rdx;
            3:Result := @p_trapframe(nil)^.tf_rbx;
            4:Result := @p_trapframe(nil)^.tf_rsp;
            5:Result := @p_trapframe(nil)^.tf_rbp;
            6:Result := @p_trapframe(nil)^.tf_rsi;
            7:Result := @p_trapframe(nil)^.tf_rdi;
            8:Result := @p_trapframe(nil)^.tf_r8 ;
            9:Result := @p_trapframe(nil)^.tf_r9 ;
           10:Result := @p_trapframe(nil)^.tf_r10;
           11:Result := @p_trapframe(nil)^.tf_r11;
           12:Result := @p_trapframe(nil)^.tf_r12;
           13:Result := @p_trapframe(nil)^.tf_r13;
           14:Result := @p_trapframe(nil)^.tf_r14;
           15:Result := @p_trapframe(nil)^.tf_r15;
           else;
          end;
        end;
      else;
      end;
    end;
    regGeneralH:
    begin
      case RegValue.ASize of
        os8:
        begin
          case RegValue.AIndex of
            0:Result := @p_trapframe(nil)^.tf_rax+1;
            1:Result := @p_trapframe(nil)^.tf_rcx+1;
            2:Result := @p_trapframe(nil)^.tf_rdx+1;
            3:Result := @p_trapframe(nil)^.tf_rbx+1;
            else;
          end;
        end;
      else;
      end;
    end;
    regMm:
    begin
      Result := nil; //Format('mm%u', [AIndex]);
    end;
    regXmm:
    begin
      case RegValue.ASize of
        os32,
        os64,
        os128:
        begin
          Result := nil; //Format('xmm%u', [AIndex]);
        end;
        os256:
        begin
          Result := nil; //Format('ymm%u', [AIndex]);
        end;
        os512:
        begin
          Result := nil; //Format('zmm%u', [AIndex]);
        end;
      else;
      end;
    end;
    regSegment:
    begin
      case RegValue.AIndex of
        0:Result := @p_trapframe(nil)^.tf_es;
        1:Result := @p_trapframe(nil)^.tf_cs;
        2:Result := @p_trapframe(nil)^.tf_ss;
        3:Result := @p_trapframe(nil)^.tf_ds;
        4:Result := @p_trapframe(nil)^.tf_fs;
        5:Result := @p_trapframe(nil)^.tf_gs;
        else;
      end;
    end;
    regFlags:
    begin
      Result := @p_trapframe(nil)^.tf_rflags;
    end;
  else;
  end;
end;

function vm_check_patch(map:vm_map_t;vaddr:vm_offset_t):Boolean;
begin
 Result:=(pmap_get_raw(vaddr) and PAGE_PATCH_FLAG)<>0;
end;

procedure test_jit;
begin
 writeln('test');
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

procedure patch_original(map:vm_map_t;
                         vaddr:vm_offset_t;
                         tsize:Integer;
                         delta:Integer);
var
 trampoline:t_jmp32_trampoline;
 entry:vm_map_entry_t;
 start   :vm_offset_t;
 __end   :vm_offset_t;
 new_prot:vm_prot_t;
 err:Integer;
begin

 entry:=nil;
 if not vm_map_lookup_entry(map,vaddr,@entry) then
 begin
  Assert(False,'patch_original');
 end;

 trampoline:=c_jmpl32_trampoline;
 trampoline.addr:=delta;

 new_prot:=entry^.protection or VM_PROT_WRITE;

 if (new_prot<>entry^.protection) then
 begin
  start:=AlignDw(vaddr,PAGE_SIZE);
  __end:=AlignUp(vaddr+tsize,PAGE_SIZE);

  pmap_protect(map^.pmap,
               start,
               __end,
               new_prot,
               entry^.protection);
 end;

 err:=copyout(@trampoline,Pointer(vaddr),tsize);

 if (err<>0) then Assert(False,'patch_original');

 if (new_prot<>entry^.protection) then
 begin
  pmap_protect(map^.pmap,
               start,
               __end,
               entry^.protection,
               new_prot);
 end;
end;

function vm_try_jit_patch(map:vm_map_t;
                          vaddr:vm_offset_t):Integer;
var
 err:Integer;
 data:array[0..15] of Byte;

 dis:TX86Disassembler;
 din:TInstruction;
 ptr:Pointer;

 chunk:p_stub_chunk;

 jit_prolog:p_jit_prolog;
 jit_frame:p_jit_frame;
 delta:Int64;

 rv:Integer;
 mask:DWORD;
begin
 Result:=0;

 if vm_check_patch(map,vaddr) then
 begin
  vm_map_unlock(map);
  Exit(0);
 end;

 err:=copyin(Pointer(vaddr),@data,SizeOf(data));
 if (err<>0) then Exit(KERN_PROTECTION_FAILURE);

 dis:=Default(TX86Disassembler);
 din:=Default(TInstruction);

 ptr:=@data;
 dis.Disassemble(dm64,ptr,din);

 if (dis.CodeIdx=4) then
 begin
  mask:=data[4];
  mask:=mask shl 24;

  chunk:=p_alloc_m(Pointer(vaddr+SizeOf(t_jmp32_trampoline)),SizeOf(t_jit_prolog),mask);

  if (chunk=nil) then Exit(KERN_NO_SPACE);

  jit_frame:=AllocMem(SizeOf(t_jit_frame));

  jit_frame^.call:=@test_jit;
  jit_frame^.addr:=Pointer(vaddr);
  jit_frame^.reta:=Pointer(vaddr+dis.CodeIdx);

  jit_prolog:=@chunk^.body;
  jit_prolog^:=c_jit_prolog;
  jit_prolog^.jframe:=jit_frame;

  delta:=Int64(jit_prolog)-(Int64(vaddr)+SizeOf(t_jmp32_trampoline));

  Assert(is_mask_valid(Pointer(vaddr)+SizeOf(t_jmp32_trampoline),jit_prolog,mask),'vm_try_jit_patch');

  patch_original(map,vaddr,dis.CodeIdx,Integer(delta));
 end;


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

 Result:=vm_try_jit_patch(map,rip_addr);

 vm_map_lookup_done(map,entry);
end;



end.

