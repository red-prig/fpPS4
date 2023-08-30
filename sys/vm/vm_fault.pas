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
 x86_jit,
 kern_stub,
 ucontext,
 vm_patch_link;


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
      Result := @p_kthread(nil)^.td_frame.tf_rip;
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
            0:Result := @p_kthread(nil)^.td_frame.tf_rax;
            1:Result := @p_kthread(nil)^.td_frame.tf_rcx;
            2:Result := @p_kthread(nil)^.td_frame.tf_rdx;
            3:Result := @p_kthread(nil)^.td_frame.tf_rbx;
            4:Result := @p_kthread(nil)^.td_frame.tf_rsp;
            5:Result := @p_kthread(nil)^.td_frame.tf_rbp;
            6:Result := @p_kthread(nil)^.td_frame.tf_rsi;
            7:Result := @p_kthread(nil)^.td_frame.tf_rdi;
            8:Result := @p_kthread(nil)^.td_frame.tf_r8 ;
            9:Result := @p_kthread(nil)^.td_frame.tf_r9 ;
           10:Result := @p_kthread(nil)^.td_frame.tf_r10;
           11:Result := @p_kthread(nil)^.td_frame.tf_r11;
           12:Result := @p_kthread(nil)^.td_frame.tf_r12;
           13:Result := @p_kthread(nil)^.td_frame.tf_r13;
           14:Result := @p_kthread(nil)^.td_frame.tf_r14;
           15:Result := @p_kthread(nil)^.td_frame.tf_r15;
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
            0:Result := @p_kthread(nil)^.td_frame.tf_rax+1;
            1:Result := @p_kthread(nil)^.td_frame.tf_rcx+1;
            2:Result := @p_kthread(nil)^.td_frame.tf_rdx+1;
            3:Result := @p_kthread(nil)^.td_frame.tf_rbx+1;
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
      else;
      end;
    end;
    regSegment:
    begin
      case RegValue.AIndex of
        0:Result := @p_kthread(nil)^.td_frame.tf_es;
        1:Result := @p_kthread(nil)^.td_frame.tf_cs;
        2:Result := @p_kthread(nil)^.td_frame.tf_ss;
        3:Result := @p_kthread(nil)^.td_frame.tf_ds;
        4:Result := @p_kthread(nil)^.td_frame.tf_fs;
        5:Result := @p_kthread(nil)^.td_frame.tf_gs;
        else;
      end;
    end;
    regFlags:
    begin
      Result := @p_kthread(nil)^.td_frame.tf_rflags;
    end;
  else;
  end;
end;

function GetFrameOffsetInt(RegValue:TRegValue):Integer; inline;
begin
 Result:=Integer(ptruint(GetFrameOffset(RegValue)));
end;

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

procedure test_jit;
begin
 writeln('test_jit');
end;

type
 p_jmp32_trampoline=^t_jmp32_trampoline;
 t_jmp32_trampoline=packed record
  inst:Byte;    //E9
  addr:Integer;
 end;

 t_data16=array[0..15] of Byte;

const
 c_jmpl32_trampoline:t_jmp32_trampoline=(inst:$E9;addr:0);
 c_data16:t_data16=($90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90);

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

type
 tcopy_cb=procedure(vaddr:Pointer); //rdi

//rdi,rsi,edx
procedure copyout_mov(vaddr:Pointer;cb:tcopy_cb;size:Integer);
var
 data:array[0..31] of Byte;
begin
 if pmap_test_cross(QWORD(vaddr),size-1) then
 begin
  cb(@data); //xmm->data
  copyout(@data,vaddr,size);
 end else
 begin
  vaddr:=uplift(vaddr);
  cb(vaddr); //xmm->vaddr
 end;
end;

//rdi,rsi,edx
procedure copyin_mov(vaddr:Pointer;cb:tcopy_cb;size:Integer);
var
 data:array[0..31] of Byte;
begin
 if pmap_test_cross(QWORD(vaddr),size-1) then
 begin
  copyin(vaddr,@data,size);
  cb(@data); //data->xmm
 end else
 begin
  vaddr:=uplift(vaddr);
  cb(vaddr); //vaddr->xmm
 end;
end;

type
 t_memop_type=(moCopyout,moCopyin);

function classif_memop(var din:TInstruction):t_memop_type;
begin
 if (ofMemory in din.Operand[1].Flags) then
 begin
  Result:=moCopyout;
 end else
 if (ofMemory in din.Operand[2].Flags) then
 begin
  Result:=moCopyin;
 end else
 begin
  Assert(false,'classif_memop');
 end;
end;

function get_lea_id(memop:t_memop_type):Byte;
begin
 case memop of
  moCopyout:Result:=1;
  moCopyin :Result:=2;
 end;
end;

function get_reg_id(memop:t_memop_type):Byte;
begin
 case memop of
  moCopyout:Result:=2;
  moCopyin :Result:=1;
 end;
end;

type
 p_jit_code=^t_jit_code;
 t_jit_code=record
  frame :t_jit_frame;
  prolog:p_stub_chunk;
  o_len :Byte;
  o_data:t_data16;
  code  :record end;
 end;

type
 t_jit_context=record
  rip_addr:QWORD;

  Code:t_data16;

  dis:TX86Disassembler;
  din:TInstruction;

  builder:t_jit_builder;

  jit_code:p_jit_code;
 end;

function GetTargetOfs(var ctx:t_jit_context;id:Byte;var ofs:Int64):Boolean;
var
 i:Integer;
begin
 Result:=True;
 i:=ctx.din.Operand[id].CodeIndex;
 case ctx.din.Operand[id].ByteCount of
   1: ofs:=PShortint(@ctx.Code[i])^;
   2: ofs:=PSmallint(@ctx.Code[i])^;
   4: ofs:=PInteger (@ctx.Code[i])^;
   8: ofs:=PInt64   (@ctx.Code[i])^;
   else
      Result:=False;
 end;
end;

procedure build_lea(var ctx:t_jit_context;id:Byte);
var
 RegValue:TRegValues;
 adr,tdr:t_jit_reg;
 i:Integer;
 ofs:Int64;
begin
 RegValue:=ctx.din.Operand[id].RegValue;

 adr:=t_jit_builder.rdi;
 adr.ARegValue[0].ASize:=RegValue[0].ASize;

 tdr:=t_jit_builder.rsi;

 with ctx.builder do
 begin
  movq(tdr,[GS+Integer(teb_thread)]);

  i:=GetFrameOffsetInt(RegValue[0]);
  Assert(i<>0,'build_lea');

  //xor adr,adr needed

  movq(adr,[tdr+i]);

  if (RegValue[0].AScale>1) then
  begin
   lea(adr,[adr*RegValue[0].AScale])
  end;

  if (RegValue[1].AType<>regNone) then
  begin
   i:=GetFrameOffsetInt(RegValue[1]);
   Assert(i<>0,'build_lea');

   addq(adr,[tdr+i]);
  end;
 end;

 ofs:=0;
 if GetTargetOfs(ctx,id,ofs) then
 begin
  with ctx.builder do
  begin
   lea(adr,[adr+ofs]);
  end;
 end;
end;

//

procedure build_mov(var ctx:t_jit_context;id:Byte;memop:t_memop_type);
var
 i,copy_size:Integer;
 imm:Int64;
 link:t_jit_i_link;
 dst:t_jit_reg;
begin

 case ctx.din.Operand[id].Size of
  os32:
   begin
    copy_size:=4;
    dst:=t_jit_builder.esi;
   end;
  os64:
   begin
    copy_size:=8;
    dst:=t_jit_builder.rsi;
   end
  else
   begin
    Writeln('build_mov (',ctx.din.Operand[id].Size,')');
    Assert(false,'build_mov (size)');
   end;
 end;

 if (ctx.din.Operand[1].Size<>ctx.din.Operand[2].Size) then
 begin
  Writeln(ctx.din.OpCode.Opcode,' ',
          ctx.din.OpCode.Suffix,' ',
          ctx.din.Operand[1].Size,' ',
          ctx.din.Operand[2].Size);

  Assert(false,'TODO');
 end;

 case memop of
  moCopyout:
   begin
    with ctx.builder do
    begin
     //input:rdi

     link:=leaj(rsi,[rip+$FFFF],-1);

     movi(edx,copy_size);

     call(@copyout_mov); //rdi,rsi,edx

     reta;

     //input:rdi

     link._label:=_label;

     imm:=0;
     if GetTargetOfs(ctx,id,imm) then
     begin
      //imm const
      movi(dst,imm);
      movq([rdi],dst); //TODO movi([rdi],imm);
     end else
     begin
      movq(rax,[GS+Integer(teb_thread)]);

      i:=GetFrameOffsetInt(ctx.din.Operand[id].RegValue[0]);
      Assert(i<>0,'build_mov');

      movq(dst,[rax+i]);
      movq([rdi],dst);
     end;

     reta;

    end;
   end;
  moCopyin:
   begin
    with ctx.builder do
    begin
     //input:rdi

     link:=movj(rsi,[rip+$FFFF],-1);

     movi(edx,copy_size);

     call(@copyin_mov); //rdi,rsi,edx

     reta;

     //input:rdi

     link._label:=_label;

     movq(rax,[GS+Integer(teb_thread)]);

     i:=GetFrameOffsetInt(ctx.din.Operand[id].RegValue[0]);
     Assert(i<>0,'build_mov');

     movq(dst,[rdi]);
     movq([rax+i],dst);

     reta;
    end;
   end;
 end;
end;

//

procedure build_vmovdqu(var ctx:t_jit_context;id:Byte;memop:t_memop_type);
var
 link:t_jit_i_link;
 dst:t_jit_reg;
begin
 case memop of
  moCopyout:
   begin
    with ctx.builder do
    begin
     //input:rdi

     link:=leaj(rsi,[rip+$FFFF],-1);

     case ctx.din.Operand[id].RegValue[0].ASize of
      os128:movi(edx,16);
      os256:movi(edx,32);
      else
       Assert(false);
     end;

     call(@copyout_mov); //rdi,rsi,edx

     reta;

     //input:rdi

     link._label:=_label;

     dst:=Default(t_jit_reg);
     dst.ARegValue:=ctx.din.Operand[id].RegValue;

     vmovdqu([rdi],dst);

     reta;
    end;
   end;
  moCopyin:
   begin
    with ctx.builder do
    begin
     //input:rdi

     link:=movj(rsi,[rip+$FFFF],-1);

     case ctx.din.Operand[id].RegValue[0].ASize of
      os128:movi(edx,16);
      os256:movi(edx,32);
      else
       Assert(false);
     end;

     call(@copyin_mov); //rdi,rsi,edx

     reta;

     //input:rdi

     link._label:=_label;

     dst:=Default(t_jit_reg);
     dst.ARegValue:=ctx.din.Operand[id].RegValue;

     vmovdqu(dst,[rdi]);

     reta;
    end;
   end;
 end;
end;

procedure build_vmovups(var ctx:t_jit_context;id:Byte;memop:t_memop_type);
var
 link:t_jit_i_link;
 dst:t_jit_reg;
begin
 case memop of
  moCopyout:
   begin
    with ctx.builder do
    begin
     //input:rdi

     link:=leaj(rsi,[rip+$FFFF],-1);

     case ctx.din.Operand[id].RegValue[0].ASize of
      os128:movi(edx,16);
      os256:movi(edx,32);
      else
       Assert(false);
     end;

     call(@copyout_mov); //rdi,rsi,edx

     reta;

     //input:rdi

     link._label:=_label;

     dst:=Default(t_jit_reg);
     dst.ARegValue:=ctx.din.Operand[id].RegValue;

     vmovups([rdi],dst);

     reta;
    end;
   end;
  moCopyin:
   begin
    with ctx.builder do
    begin
     //input:rdi

     link:=movj(rsi,[rip+$FFFF],-1);

     case ctx.din.Operand[id].RegValue[0].ASize of
      os128:movi(edx,16);
      os256:movi(edx,32);
      else
       Assert(false);
     end;

     call(@copyin_mov); //rdi,rsi,edx

     reta;

     //input:rdi

     link._label:=_label;

     dst:=Default(t_jit_reg);
     dst.ARegValue:=ctx.din.Operand[id].RegValue;

     vmovups(dst,[rdi]);

     reta;
    end;
   end;
 end;
end;

procedure build_vmovdqa(var ctx:t_jit_context;id:Byte;memop:t_memop_type);
var
 dst:t_jit_reg;
begin
 case memop of
  moCopyout:
   begin
    with ctx.builder do
    begin
     //input:rdi

     call(@uplift); //input:rdi output:rax

     dst:=Default(t_jit_reg);
     dst.ARegValue:=ctx.din.Operand[id].RegValue;

     vmovdqa([rax],dst);

     reta;
    end;
   end;
  moCopyin:
   begin
    with ctx.builder do
    begin
     //input:rdi

     call(@uplift); //input:rdi output:rax

     dst:=Default(t_jit_reg);
     dst.ARegValue:=ctx.din.Operand[id].RegValue;

     vmovdqa(dst,[rax]);

     reta;
    end;
   end;
 end;
end;

procedure print_disassemble(addr:Pointer;vsize:Integer);
var
 proc:TDbgProcess;
 adec:TX86AsmDecoder;
 ptr,fin:Pointer;
 ACodeBytes,ACode:RawByteString;
begin
 ptr:=addr;
 fin:=addr+vsize;

 proc:=TDbgProcess.Create(dm64);
 adec:=TX86AsmDecoder.Create(proc);

 while (ptr<fin) do
 begin
  adec.Disassemble(ptr,ACodeBytes,ACode);
  Writeln(ACodeBytes:32,' ',ACode);
 end;

 adec.Free;
 proc.Free;
end;

function generate_jit(var ctx:t_jit_context):p_stub_chunk;
label
 _err;
var
 memop:t_memop_type;
 jit_size,size,i:Integer;
begin

 ctx.builder.call(@test_jit); //test

 case ctx.din.OpCode.Opcode of
  OPmov:
   begin
    case ctx.din.OpCode.Suffix of

     OPSnone:
      begin
       memop:=classif_memop(ctx.din);

       build_lea(ctx,get_lea_id(memop));
       build_mov(ctx,get_reg_id(memop),memop);
      end;

     OPSx_dqu:
      begin
       memop:=classif_memop(ctx.din);

       build_lea(ctx,get_lea_id(memop));
       build_vmovdqu(ctx,get_reg_id(memop),memop);
      end;

     OPSx_dqa:
      begin
       memop:=classif_memop(ctx.din);

       build_lea(ctx,get_lea_id(memop));
       build_vmovdqa(ctx,get_reg_id(memop),memop);
      end

     else
      goto _err;
    end; //case
   end; //OPmov

  OPmovu:
   begin
    case ctx.din.OpCode.Suffix of

     OPSx_ps:
       begin
        memop:=classif_memop(ctx.din);

        build_lea(ctx,get_lea_id(memop));
        build_vmovups(ctx,get_reg_id(memop),memop);
       end;

     else
      goto _err;
    end; //case
   end; //OPmovu

  else
   begin
    _err:
    Writeln('Unhandled jit:',
            ctx.din.OpCode.Opcode,' ',
            ctx.din.OpCode.Suffix,' ',
            ctx.din.Operand[1].Size,' ',
            ctx.din.Operand[2].Size);
    Assert(false);
   end;
 end;

 jit_size:=ctx.builder.GetMemSize;
 size:=SizeOf(t_jit_code)+jit_size;

 Result:=p_alloc(nil,size);

 ctx.jit_code:=@Result^.body;
 ctx.jit_code^:=Default(t_jit_code);

 ctx.jit_code^.frame.call:=@ctx.jit_code^.code;
 ctx.jit_code^.frame.addr:=Pointer(ctx.rip_addr);
 ctx.jit_code^.frame.reta:=Pointer(ctx.rip_addr+ctx.dis.CodeIdx);

 ctx.jit_code^.o_len :=ctx.dis.CodeIdx;
 ctx.jit_code^.o_data:=c_data16;

 Move(ctx.Code,ctx.jit_code^.o_data,ctx.dis.CodeIdx);

 ctx.builder.SaveTo(@ctx.jit_code^.code,jit_size);

 Writeln('--------------------------------':32,' ','print patch');
 print_disassemble(@ctx.jit_code^.code,ctx.builder.GetInstructionsSize);

 if (Length(ctx.builder.AData))<>0 then
 begin
  Writeln('--------------------------------':32,' ','print data');
  For i:=0 to High(ctx.builder.AData) do
  begin
   Writeln('[0x'+HexStr(ctx.builder.AData[i])+']':32,' data:',i);
  end;
  Writeln('--------------------------------':32,' ','print data');
 end;

 Writeln;

 ///
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

