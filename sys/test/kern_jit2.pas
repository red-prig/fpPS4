unit kern_jit2;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 x86_fpdbgdisas,
 x86_jit,
 kern_jit2_ctx;

var
 print_asm:Boolean=False;

procedure pick(var ctx:t_jit_context2);
procedure pick_locked(var ctx:t_jit_context2);

implementation

uses
 sysutils,
 kern_thr,
 ucontext,
 vmparam,
 vm_pmap,
 vm_map,
 systm,
 trap,
 md_context,
 kern_sig,
 kern_jit2_ops,
 kern_jit2_ops_sse,
 kern_jit2_ops_avx,
 kern_jit_dynamic;


procedure jit_syscall; assembler; nostackframe;
label
 _after_call,
 _doreti,
 _fail,
 _ast,
 _doreti_exit;
asm
 //prolog (debugger)
 pushq %rbp
 movqq %rsp,%rbp

 andq  $-16,%rsp //align stack

 movqq %rax,jit_frame.tf_rip(%r15) //save %rax to tf_rip

 pushf
 pop %rax

 movqq %gs:teb.thread,%r14 //curkthread
 test  %r14,%r14
 jz    _fail

 andl  NOT_PCB_FULL_IRET,kthread.pcb_flags(%r14) //clear PCB_FULL_IRET

 movqq %rax,kthread.td_frame.tf_rflags(%r14) //save flags

 movqq %rdi,kthread.td_frame.tf_rdi(%r14)
 movqq %rsi,kthread.td_frame.tf_rsi(%r14)
 movqq %rdx,kthread.td_frame.tf_rdx(%r14)
 movqq   $0,kthread.td_frame.tf_rcx(%r14)
 movqq %r8 ,kthread.td_frame.tf_r8 (%r14)
 movqq %r9 ,kthread.td_frame.tf_r9 (%r14)
 movqq %rbx,kthread.td_frame.tf_rbx(%r14)
 movqq %r10,kthread.td_frame.tf_r10(%r14)
 movqq   $0,kthread.td_frame.tf_r11(%r14)
 movqq %r12,kthread.td_frame.tf_r12(%r14)
 movqq %r13,kthread.td_frame.tf_r13(%r14)

 movqq   $1,kthread.td_frame.tf_trapno(%r14)
 movqq   $0,kthread.td_frame.tf_addr  (%r14)
 movqq   $0,kthread.td_frame.tf_flags (%r14)
 movqq   $2,kthread.td_frame.tf_err   (%r14) //sizeof(syscall)

 movqq jit_frame.tf_rax(%r15),%rax
 movqq %rax,kthread.td_frame.tf_rax(%r14)

 movqq jit_frame.tf_rsp(%r15),%rax
 movqq %rax,kthread.td_frame.tf_rsp(%r14)

 movqq jit_frame.tf_rbp(%r15),%rax
 movqq %rax,kthread.td_frame.tf_rbp(%r14)

 movqq jit_frame.tf_r14(%r15),%rax
 movqq %rax,kthread.td_frame.tf_r14(%r14)

 movqq jit_frame.tf_r15(%r15),%rax
 movqq %rax,kthread.td_frame.tf_r15(%r14)

 movqq jit_frame.tf_rip(%r15),%rax
 movqq %rax,kthread.td_frame.tf_rip(%r14)

 call amd64_syscall

 _after_call:

 movqq %gs:teb.thread          ,%r14 //curkthread
 movqq kthread.td_jit_ctx(%r14),%r15 //jit_frame

 //Requested full context restore
 testl PCB_FULL_IRET,kthread.pcb_flags(%r14)
 jnz _doreti

 testl TDF_AST,kthread.td_flags(%r14)
 jne _ast

 //Restore preserved registers.

 //get flags
 movqq kthread.td_frame.tf_rflags(%r14),%rax
 push %rax
 popf

 movqq kthread.td_frame.tf_rdi(%r14),%rdi
 movqq kthread.td_frame.tf_rsi(%r14),%rsi
 movqq kthread.td_frame.tf_rdx(%r14),%rdx

 movqq kthread.td_frame.tf_rax(%r14),%rax
 movqq %rax,jit_frame.tf_rax(%r15)

 movqq kthread.td_frame.tf_rsp(%r14),%rax
 movqq %rax,jit_frame.tf_rsp(%r15)

 movqq kthread.td_frame.tf_rbp(%r14),%rax
 movqq %rax,jit_frame.tf_rbp(%r15)

 movqq $0,%rcx
 movqq $0,%r11

 //epilog (debugger)
 movq  %rbp,%rsp
 popq  %rbp
 ret

 //fail (curkthread=nil)
 _fail:

 or $1,%rax //set CF
 push %rax
 popf

 movqq $14,jit_frame.tf_rax(%r15) //EFAULT
 movqq  $0,%rdx
 movqq  $0,%rcx
 movqq  $0,%r11

 //epilog (debugger)
 movq  %rbp,%rsp
 popq  %rbp
 ret

 //ast
 _ast:

  call ast
  jmp _after_call

 //doreti
 _doreti:

  //%r14=curkthread
  testl TDF_AST,kthread.td_flags(%r14)
  je _doreti_exit

  call ast
  jmp _doreti

 _doreti_exit:

  //Restore full.
  call  ipi_sigreturn
  hlt
end;

procedure jit_save_ctx; assembler; nostackframe;
asm
 push %rax

 movqq %gs:teb.thread,%rax //curkthread

 movqq %rdi,kthread.td_frame.tf_rdi(%rax)
 movqq %rsi,kthread.td_frame.tf_rsi(%rax)
 movqq %rdx,kthread.td_frame.tf_rdx(%rax)
 movqq %rcx,kthread.td_frame.tf_rcx(%rax)
 movqq %r8 ,kthread.td_frame.tf_r8 (%rax)
 movqq %r9 ,kthread.td_frame.tf_r9 (%rax)
 movqq %rbx,kthread.td_frame.tf_rbx(%rax)
 movqq %r10,kthread.td_frame.tf_r10(%rax)
 movqq %r11,kthread.td_frame.tf_r11(%rax)
 movqq %r12,kthread.td_frame.tf_r12(%rax)
 movqq %r13,kthread.td_frame.tf_r13(%rax)

 pushf
 pop %rdi
 movqq %rdi,kthread.td_frame.tf_rflags(%rax);

 lea kthread.td_fpstate(%rax),%rdi
 and $-32,%rdi

 vmovdqa %ymm0 ,0x000(%rdi)
 vmovdqa %ymm1 ,0x020(%rdi)
 vmovdqa %ymm2 ,0x040(%rdi)
 vmovdqa %ymm3 ,0x060(%rdi)
 vmovdqa %ymm4 ,0x080(%rdi)
 vmovdqa %ymm5 ,0x0A0(%rdi)
 vmovdqa %ymm6 ,0x0C0(%rdi)
 vmovdqa %ymm7 ,0x0E0(%rdi)
 vmovdqa %ymm8 ,0x100(%rdi)
 vmovdqa %ymm9 ,0x120(%rdi)
 vmovdqa %ymm10,0x140(%rdi)
 vmovdqa %ymm11,0x160(%rdi)
 vmovdqa %ymm12,0x180(%rdi)
 vmovdqa %ymm13,0x1A0(%rdi)
 vmovdqa %ymm14,0x1C0(%rdi)
 vmovdqa %ymm15,0x1E0(%rdi)

 pop %rax
end;

procedure jit_load_ctx; assembler; nostackframe;
asm
 push %rax

 movqq %gs:teb.thread,%rax //curkthread

 lea kthread.td_fpstate(%rax),%rdi
 and $-32,%rdi

 vmovdqa 0x000(%rdi),%ymm0
 vmovdqa 0x020(%rdi),%ymm1
 vmovdqa 0x040(%rdi),%ymm2
 vmovdqa 0x060(%rdi),%ymm3
 vmovdqa 0x080(%rdi),%ymm4
 vmovdqa 0x0A0(%rdi),%ymm5
 vmovdqa 0x0C0(%rdi),%ymm6
 vmovdqa 0x0E0(%rdi),%ymm7
 vmovdqa 0x100(%rdi),%ymm8
 vmovdqa 0x120(%rdi),%ymm9
 vmovdqa 0x140(%rdi),%ymm10
 vmovdqa 0x160(%rdi),%ymm11
 vmovdqa 0x180(%rdi),%ymm12
 vmovdqa 0x1A0(%rdi),%ymm13
 vmovdqa 0x1C0(%rdi),%ymm14
 vmovdqa 0x1E0(%rdi),%ymm15

 movqq kthread.td_frame.tf_rflags(%rax),%rdi
 push %rdi
 popf

 movqq kthread.td_frame.tf_rdi(%rax),%rdi
 movqq kthread.td_frame.tf_rsi(%rax),%rsi
 movqq kthread.td_frame.tf_rdx(%rax),%rdx
 movqq kthread.td_frame.tf_rcx(%rax),%rcx
 movqq kthread.td_frame.tf_r8 (%rax),%r8
 movqq kthread.td_frame.tf_r9 (%rax),%r9
 movqq kthread.td_frame.tf_rbx(%rax),%rbx
 movqq kthread.td_frame.tf_r10(%rax),%r10
 movqq kthread.td_frame.tf_r11(%rax),%r11
 movqq kthread.td_frame.tf_r12(%rax),%r12
 movqq kthread.td_frame.tf_r13(%rax),%r13

 pop %rax
end;

procedure jit_jmp_dispatch; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 andq  $-16,%rsp //align stack

 call jit_save_ctx

 mov  %rax,%rdi
 mov    $0,%rsi

 call jmp_dispatcher

 call jit_load_ctx

 //epilog
 movq %rbp,%rsp
 pop  %rbp

 lea  8(%rsp),%rsp
 jmp  %rax
end;

procedure jit_call_dispatch; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 andq  $-16,%rsp //align stack

 call jit_save_ctx

 mov  %rax,%rdi
 mov    $1,%rsi

 call jmp_dispatcher

 call jit_load_ctx

 //epilog
 movq %rbp,%rsp
 pop  %rbp

 lea  8(%rsp),%rsp
 jmp  %rax
end;

procedure jit_assert;
begin
 Writeln('TODO:jit_assert');
 Assert(False);
end;

procedure jit_system_error;
begin
 Writeln('TODO:jit_system_error');
 Assert(False);
end;

procedure jit_exit_proc;
begin
 Writeln('TODO:jit_exit_proc');
 //Assert(False);
end;

//0x0
//0x1
//0x4
//0x6
//0xb

//0x40000000
//0x40000010

//0x80000000
//0x80000001
//0x80000002
//0x80000004
//0x80000005
//0x80000006
//0x80000008

//0xc0000000
//0xc0000001
procedure jit_cpuid; assembler; nostackframe;
label
 _cpuid_0,
 _cpuid_1,
 _cpuid_80000000,
 _cpuid_80000001,
 _cpuid_80000008;
asm
 pushf

 mov jit_frame.tf_rax(%r15),%rax

 cmp $0,%eax
 je _cpuid_0

 cmp $1,%eax
 je _cpuid_1

 cmp $0x80000000,%eax
 je _cpuid_80000000

 cmp $0x80000001,%eax
 je _cpuid_80000001


 cmp $0x80000008,%eax
 je _cpuid_80000008

 ud2






 _cpuid_0:

 //cpu_high TODO check
 mov $0xF,%eax

 //cpu_vendor
 mov $0x68747541,%ebx
 mov $0x69746E65,%edx
 mov $0x444D4163,%ecx

 mov %rax,jit_frame.tf_rax(%r15)
 popf
 ret

 _cpuid_1:

 //get host
 cpuid

 //if ((cpu_id & 0xffffff80) == 0x740f00) then
 //if "machdep.bootparams.base_ps4_mode" then sceKernelHasNeoMode

 //if ((cpu_id & 0xffffff80) == 0x740f00) then sceKernelIsAuthenticNeo

 mov $0x00710f13,%eax //cpu_id
 mov $0x178bfbff,%edx //cpu_feature
 mov $0x36d8220b,%ecx //cpu_feature2

//CPUID_BRAND_INDEX   0x000000ff
//CPUID_CLFUSH_SIZE   0x0000ff00
//CPUID_HTT_CORES     0x00ff0000  //sceKernelGetCurrentCpu 0..7
//CPUID_LOCAL_APIC_ID 0xff000000

 and $0xFF070000,%ebx //filter CPUID_LOCAL_APIC_ID|CPUID_HTT_CORES

 or $0x00000800,%ebx //cpu_procinfo

 mov %rax,jit_frame.tf_rax(%r15)
 popf
 ret

 _cpuid_80000000:

 //cpu_exthigh TODO check
 mov $0xC0000001,%eax

 //cpu_vendor
 mov $0x68747541,%ebx
 mov $0x69746E65,%edx
 mov $0x444D4163,%ecx

 mov %rax,jit_frame.tf_rax(%r15)
 popf
 ret

 _cpuid_80000001:

 mov $0x2e500800,%edx //amd_feature
 mov $0x154837fb,%ecx //amd_feature2

 popf
 ret

 _cpuid_80000008:

 mov $0x00003030,%eax //TODO check
 mov $0x00001007,%ebx //TODO check
 mov $0x00000000,%edx //TODO check
 mov $0x00004007,%ecx //cpu_procinfo2 TODO check

 mov %rax,jit_frame.tf_rax(%r15)
 popf
 ret

end;

procedure op_jmp_dispatcher(var ctx:t_jit_context2);
begin
 ctx.builder.call_far(@jit_jmp_dispatch); //input:rax
end;

procedure op_call_dispatcher(var ctx:t_jit_context2);
begin
 ctx.builder.call_far(@jit_call_dispatch); //input:rax
end;

procedure op_push_rip(var ctx:t_jit_context2);
var
 stack:TRegValue;
 imm:Int64;
begin
 //lea rsp,[rsp-8]
 //mov [rsp],rax

 with ctx.builder do
 begin
  stack:=r_tmp0;

  op_load_rsp(ctx,stack);
  leaq(stack,[stack-8]);
  op_save_rsp(ctx,stack);

  call_far(@uplift_jit); //in/out:rax uses:r14

  imm:=Int64(ctx.ptr_next);

  if (classif_offset_se64(imm)=os64) then
  begin
   if (classif_offset_u64(imm)=os64) then
   begin
    //64bit imm
    movi64(r_tmp1,imm);
    movq([stack],r_tmp1);
   end else
   begin
    //32bit zero extend
    movi(new_reg_size(r_tmp1,os32),imm);
    movq([stack],r_tmp1);
   end;
  end else
  begin
   //32bit sign extend
   movi([stack,os64],imm);
  end;

 end;
end;

procedure op_pop_rip(var ctx:t_jit_context2;imm:Word); //out:rax
var
 stack:TRegValue;
begin
 //mov rax,[rsp]
 //lea rsp,[rsp+8+imm]

 with ctx.builder do
 begin
  stack:=r_tmp0;

  op_load_rsp(ctx,stack);

  call_far(@uplift_jit); //in/out:rax uses:r14

  movq(r_tmp1,[stack]);

  op_load_rsp(ctx,stack);
  leaq(stack,[stack+8+imm]);
  op_save_rsp(ctx,stack);

  movq(r_tmp0,r_tmp1);
 end;
end;

procedure op_set_rax_imm(var ctx:t_jit_context2;imm:Int64);
begin
 with ctx.builder do
  if (classif_offset_u64(imm)=os64) then
  begin
   //64bit imm
   movi64(r_tmp0,imm);
  end else
  begin
   //32bit zero extend
   movi(new_reg_size(r_tmp0,os32),imm);
  end;
end;

procedure op_call(var ctx:t_jit_context2);
var
 id:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 new1,new2:TRegValue;
 link:t_jit_i_link;
begin
 op_push_rip(ctx);

 if (ctx.din.Operand[1].RegValue[0].AType=regNone) then
 begin
  //imm offset

  ofs:=0;
  GetTargetOfs(ctx.din,ctx.code,1,ofs);

  dst:=ctx.ptr_next+ofs;

  if ctx.is_text_addr(QWORD(dst)) and
     (not exist_entry(dst)) then
  begin
   link:=ctx.get_link(dst);

   if (link<>nil_link) then
   begin
    ctx.builder.jmp(link);
    ctx.add_forward_point(nil_link,dst);
   end else
   begin
    id:=ctx.builder.jmp(nil_link);
    ctx.add_forward_point(id,dst);
   end;
  end else
  begin
   op_set_rax_imm(ctx,Int64(dst));
   //
   op_call_dispatcher(ctx);
  end;

 end else
 if is_memory(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  build_lea(ctx,1,new1,[inc8_rsp,code_ref]);
  //
  ctx.builder.call_far(@uplift_jit); //in/out:rax uses:r14
  //
  ctx.builder.movq(new1,[new1]);
  //
  op_call_dispatcher(ctx);
 end else
 if is_preserved(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  op_load(ctx,new1,1);
  //
  if is_rsp(ctx.din.Operand[1].RegValue[0]) then
  begin
   ctx.builder.leaq(new1,[new1+8]);
  end;
  //
  op_call_dispatcher(ctx);
 end else
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  new2:=new_reg(ctx.din.Operand[1]);
  //
  ctx.builder.movq(new1,new2);
  //
  op_call_dispatcher(ctx);
 end;

 //
 ctx.add_forward_point(nil_link,ctx.ptr_next);
end;

procedure op_ret(var ctx:t_jit_context2);
var
 imm:Int64;
begin
 imm:=0;
 GetTargetOfs(ctx.din,ctx.code,1,imm);
 //
 op_pop_rip(ctx,imm); //out:rax
 //
 op_jmp_dispatcher(ctx);
 //
 ctx.trim:=True;
end;

procedure op_jmp(var ctx:t_jit_context2);
var
 id:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 new1,new2:TRegValue;
 link:t_jit_i_link;
begin
 if (ctx.din.Operand[1].RegValue[0].AType=regNone) then
 begin
  ofs:=0;
  GetTargetOfs(ctx.din,ctx.code,1,ofs);

  dst:=ctx.ptr_next+ofs;

  if ctx.is_text_addr(QWORD(dst)) and
     (not exist_entry(dst)) then
  begin
   link:=ctx.get_link(dst);

   if (link<>nil_link) then
   begin
    ctx.builder.jmp(link);
    ctx.add_forward_point(nil_link,dst);
   end else
   begin
    id:=ctx.builder.jmp(nil_link);
    ctx.add_forward_point(id,dst);
   end;
  end else
  begin
   op_set_rax_imm(ctx,Int64(dst));
   //
   op_jmp_dispatcher(ctx);
  end;

 end else
 if is_memory(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  build_lea(ctx,1,new1,[code_ref]);
  //
  ctx.builder.call_far(@uplift_jit); //in/out:rax uses:r14
  //
  ctx.builder.movq(new1,[new1]);
  //
  op_jmp_dispatcher(ctx);
 end else
 if is_preserved(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  op_load(ctx,new1,1);
  //
  op_jmp_dispatcher(ctx);
 end else
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  new2:=new_reg(ctx.din.Operand[1]);
  //
  ctx.builder.movq(new1,new2);
  //
  op_jmp_dispatcher(ctx);
 end;
 //
 ctx.trim:=True;
end;

procedure op_jcc(var ctx:t_jit_context2);
var
 id,id2:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 link:t_jit_i_link;
begin
 ofs:=0;
 GetTargetOfs(ctx.din,ctx.code,1,ofs);

 dst:=ctx.ptr_next+ofs;

 if ctx.is_text_addr(QWORD(dst)) and
    (not exist_entry(dst)) then
 begin
  link:=ctx.get_link(dst);

  if (link<>nil_link) then
  begin
   ctx.builder.jcc(ctx.din.OpCode.Suffix,link);
   ctx.add_forward_point(nil_link,dst);
  end else
  begin
   id:=ctx.builder.jcc(ctx.din.OpCode.Suffix,nil_link);
   ctx.add_forward_point(id,dst);
  end;
 end else
 begin
  id:=ctx.builder.jcc(ctx.din.OpCode.Suffix,nil_link,os8);

  id2:=ctx.builder.jmp(nil_link,os8);

   id._label:=ctx.builder.get_curr_label.after;
   //
   op_set_rax_imm(ctx,Int64(dst));
   //
   op_jmp_dispatcher(ctx);

  id2._label:=ctx.builder.get_curr_label.after;
 end;
end;

const
 movsx8_desc:t_op_type=(op:$0FBE);
 movsxd_desc:t_op_type=(op:$63);

procedure op_push(var ctx:t_jit_context2);
var
 imm:Int64;
 stack,new:TRegValue;
begin
 //lea rsp,[rsp-len]
 //mov [rsp],reg

 with ctx.builder do
 begin
  stack:=r_tmp0;

  if is_memory(ctx.din) then
  begin
   build_lea(ctx,1,r_tmp0);

   call_far(@uplift_jit); //in/out:rax uses:r14

   new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

   movq(new,[r_tmp0]);
  end else
  if (ctx.din.Operand[1].ByteCount<>0) then
  begin
   imm:=0;
   GetTargetOfs(ctx.din,ctx.code,1,imm);

   new:=new_reg_size(r_tmp1,ctx.din.Operand[1].Size);

   movi(new,imm);
  end else
  if is_preserved(ctx.din) then
  begin
   new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

   op_load(ctx,new,1);
  end else
  begin
   new:=new_reg(ctx.din.Operand[1]);
  end;

  //sign extend
  case new.ASize of
    os8:
     begin
      ctx.builder._RR(movsx8_desc,new,new,os64);
      new:=new_reg_size(new,os64);
     end;
   os32:
     begin
      ctx.builder._RR(movsxd_desc,new,new,os64);
      new:=new_reg_size(new,os64);
     end
   else;
  end;

  op_load_rsp(ctx,stack);
  leaq(stack,[stack-OPERAND_BYTES[new.ASize]]);
  op_save_rsp(ctx,stack);

  call_far(@uplift_jit); //in/out:rax uses:r14

  movq([stack],new);
 end;
end;

procedure op_pushfq(var ctx:t_jit_context2);
var
 mem_size:TOperandSize;
 stack,new:TRegValue;
begin
 //lea rsp,[rsp-len]
 //mov [rsp],rflags

 with ctx.builder do
 begin
  stack:=r_tmp0;

  new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

  mem_size:=ctx.din.Operand[1].Size;

  pushfq(mem_size);
  pop(new);

  op_load_rsp(ctx,stack);
  leaq(stack,[stack-OPERAND_BYTES[new.ASize]]);
  op_save_rsp(ctx,stack);

  call_far(@uplift_jit); //in/out:rax uses:r14

  movq([stack],new);
 end;
end;

procedure op_pop(var ctx:t_jit_context2);
var
 new,stack:TRegValue;
begin
 //mov reg,[rsp]
 //lea rsp,[rsp+len]

 with ctx.builder do
 begin
  stack:=r_tmp0;

  op_load_rsp(ctx,stack);

  call_far(@uplift_jit); //in/out:rax uses:r14

  if is_memory(ctx.din) then
  begin
   new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

   movq(new,[stack]);

   build_lea(ctx,1,r_tmp0);

   call_far(@uplift_jit); //in/out:rax uses:r14

   movq([r_tmp0],new);
  end else
  if is_preserved(ctx.din) then
  begin
   new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

   movq(new,[stack]);

   op_save(ctx,1,fix_size(new));
  end else
  begin
   new:=new_reg(ctx.din.Operand[1]);

   movq(new,[stack]);
  end;

  op_load_rsp(ctx,stack);
  leaq(stack,[stack+OPERAND_BYTES[new.ASize]]);
  op_save_rsp(ctx,stack);
 end;
end;

procedure op_syscall(var ctx:t_jit_context2);
begin
 ctx.add_forward_point(nil_link,ctx.ptr_curr);
 ctx.add_forward_point(nil_link,ctx.ptr_next);
 //
 op_set_rax_imm(ctx,Int64(ctx.ptr_next));
 //
 ctx.builder.call_far(@jit_syscall); //syscall dispatcher
end;

procedure op_int(var ctx:t_jit_context2);
var
 i:Integer;
 id:Byte;
begin
 i:=ctx.din.Operand[1].ByteCount;
 Assert(i=1);
 id:=PByte(ctx.code)[i];

 case id of
  $41: //assert?
   begin
    //
    ctx.builder.call_far(@jit_assert); //TODO error dispatcher
   end;

  $44: //system error?
   begin
    //
    ctx.builder.call_far(@jit_system_error); //TODO error dispatcher
     ctx.trim:=True;
   end;
  else
   begin
    Assert(False);
   end;
 end;
end;

procedure op_ud2(var ctx:t_jit_context2);
begin
 //exit proc?
 ctx.builder.call_far(@jit_exit_proc); //TODO exit dispatcher
 ctx.trim:=True;
end;

procedure op_iretq(var ctx:t_jit_context2);
begin
 //exit proc?
 ctx.builder.call_far(@jit_exit_proc); //TODO exit dispatcher
 ctx.trim:=True;
end;

procedure op_hlt(var ctx:t_jit_context2);
begin
 //stop thread?
 ctx.builder.call_far(@jit_exit_proc); //TODO exit dispatcher
end;

procedure op_cpuid(var ctx:t_jit_context2);
begin
 ctx.builder.call_far(@jit_cpuid); //TODO CPUID
end;

procedure op_rdtsc(var ctx:t_jit_context2);
begin
 add_orig(ctx);
 op_save_rax(ctx,ctx.builder.rax);
end;

procedure op_nop(var ctx:t_jit_context2);
begin
 //align?
end;

procedure _op_rep_cmps(var ctx:t_jit_context2;dflag:Integer);
var
 op:DWORD;
 size:TOperandSize;

 link_start:t_jit_i_link;
 link___end:t_jit_i_link;

 link_jmp0:t_jit_i_link;
 link_jmp1:t_jit_i_link;
begin
 //rdi,rsi
 //prefix $67 TODO

 op:=$A7;
 case ctx.din.OpCode.Suffix of
  OPSx_b:
   begin
    size:=os8;
    op:=$A6;
   end;
  OPSx_w:size:=os16;
  OPSx_d:size:=os32;
  OPSx_q:size:=os64;
  else;
   Assert(False);
 end;

 //(r_tmp0)rax <-> rdi
 //(r_tmp1)r14 <-> rsi
 with ctx.builder do
 begin

  link_jmp0:=nil_link;
  link_jmp1:=nil_link;

  link_start:=ctx.builder.get_curr_label.after;

  //repeat
   seto(al);
   lahf;
    testq(rcx,rcx);
    link_jmp0:=jcc(OPSc_z,nil_link,os8);
   addi(al,127);
   sahf;

   movq(r_tmp0,rsi);
   call_far(@uplift_jit); //in/out:rax uses:r14
   movq(r_tmp1,r_tmp0);

   movq(r_tmp0,rdi);
   call_far(@uplift_jit); //in/out:rax uses:r14

   xchgq(rdi,r_tmp0);
   xchgq(rsi,r_tmp1);

   _O(op,Size);

   xchgq(rdi,r_tmp0);
   xchgq(rsi,r_tmp1);

   leaq(rcx,[rcx-1]);

   if (dflag=0) then
   begin
    leaq(rdi,[rdi+OPERAND_BYTES[size]]);
    leaq(rsi,[rsi+OPERAND_BYTES[size]]);
   end else
   begin
    leaq(rdi,[rdi-OPERAND_BYTES[size]]);
    leaq(rsi,[rsi-OPERAND_BYTES[size]]);
   end;

   if (ifPrefixRepE in ctx.din.Flags) then
   begin
    //if a[i]<>b[i] then exit
    link_jmp1:=jcc(OPSc_nz,nil_link,os8);
   end else
   if (ifPrefixRepNe in ctx.din.Flags) then
   begin
    //if a[i]=b[i] then exit
    link_jmp1:=jcc(OPSc_z,nil_link,os8);
   end;

  //until
  jmp(link_start,os8);

  //exit1
  addi(al,127);
  sahf;

  //exit2

  link___end:=ctx.builder.get_curr_label.before; //exit1

  link_jmp0._label:=link___end;

  link___end:=link___end.after; //exit2

  link_jmp1._label:=link___end;
 end;

end;

procedure op_rep_cmps(var ctx:t_jit_context2);
var
 link_jmp0:t_jit_i_link;
 link_jmp1:t_jit_i_link;
begin
 with ctx.builder do
 begin

  //get d flag
  pushfq(os64);
  bti8([rsp,os64],10); //bt rax, 10

  link_jmp0:=jcc(OPSc_b,nil_link,os8);

  popfq(os64);
  _op_rep_cmps(ctx,0);

  link_jmp1:=jmp(nil_link,os8);

  link_jmp0._label:=ctx.builder.get_curr_label.after;

  popfq(os64);
  _op_rep_cmps(ctx,1);

  link_jmp1._label:=ctx.builder.get_curr_label.after;

 end;
end;

///

procedure _op_rep_stos(var ctx:t_jit_context2;dflag:Integer);
var
 size:TOperandSize;

 new:TRegValue;

 link_start:t_jit_i_link;
 link___end:t_jit_i_link;

 link_jmp0:t_jit_i_link;
begin
 //rdi,rsi
 //prefix $67 TODO

 case ctx.din.OpCode.Suffix of
  OPSx_b:size:=os8;
  OPSx_w:size:=os16;
  OPSx_d:size:=os32;
  OPSx_q:size:=os64;
  else;
   Assert(False);
 end;

 //(r_tmp0)rax <-> rdi
 //(r_tmp1)r14 <-> rax
 with ctx.builder do
 begin

  link_jmp0:=nil_link;

  new:=new_reg_size(r_tmp1,size);

  op_load_rax(ctx,new);

  link_start:=ctx.builder.get_curr_label.after;

  //repeat
   seto(al);
   lahf;
    testq(rcx,rcx);
    link_jmp0:=jcc(OPSc_z,nil_link,os8);
   addi(al,127);
   sahf;

   movq(r_tmp0,rdi);
   call_far(@uplift_jit); //in/out:rax uses:r14

   movq([r_tmp0],new);

   leaq(rcx,[rcx-1]);

   if (dflag=0) then
   begin
    leaq(rdi,[rdi+OPERAND_BYTES[size]]);
   end else
   begin
    leaq(rdi,[rdi-OPERAND_BYTES[size]]);
   end;

  //until
  jmp(link_start,os8);

  //exit1
  sahf;

  //exit2

  link___end:=ctx.builder.get_curr_label.before; //exit1

  link_jmp0._label:=link___end;
 end;

end;

procedure op_rep_stos(var ctx:t_jit_context2);
var
 link_jmp0:t_jit_i_link;
 link_jmp1:t_jit_i_link;
begin
 with ctx.builder do
 begin

  //get d flag
  pushfq(os64);
  bti8([rsp,os64],10); //bt rax, 10

  link_jmp0:=jcc(OPSc_b,nil_link,os8);

  popfq(os64);
  _op_rep_stos(ctx,0);

  link_jmp1:=jmp(nil_link,os8);

  link_jmp0._label:=ctx.builder.get_curr_label.after;

  popfq(os64);
  _op_rep_stos(ctx,1);

  link_jmp1._label:=ctx.builder.get_curr_label.after;

 end;
end;

procedure op_debug_info(var ctx:t_jit_context2);
var
 link_jmp:t_jit_i_link;
begin
 //debug
  link_jmp:=ctx.builder.jmp(nil_link,os8);
  //
  op_set_rax_imm(ctx,$FACEADD7);
  op_set_rax_imm(ctx,Int64(ctx.ptr_curr));
  add_orig(ctx);
  op_set_rax_imm(ctx,Int64(ctx.ptr_next));
  op_set_rax_imm(ctx,$FACEADDE);
  //
  link_jmp._label:=ctx.builder.get_curr_label.after;
 //debug
end;

procedure init_cbs;
begin
 jit_cbs[OPPnone,OPcall,OPSnone]:=@op_call;
 jit_cbs[OPPnone,OPjmp ,OPSnone]:=@op_jmp;
 jit_cbs[OPPnone,OPret ,OPSnone]:=@op_ret;
 jit_cbs[OPPnone,OPretf,OPSnone]:=@op_ret;

 jit_cbs[OPPnone,OPj__,OPSc_o  ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_no ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_b  ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_nb ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_z  ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_nz ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_be ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_nbe]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_s  ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_ns ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_p  ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_np ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_l  ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_nl ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_le ]:=@op_jcc;
 jit_cbs[OPPnone,OPj__,OPSc_nle]:=@op_jcc;

 jit_cbs[OPPnone,OPpush,OPSnone]:=@op_push;
 jit_cbs[OPPnone,OPpop ,OPSnone]:=@op_pop;

 jit_cbs[OPPnone,OPpushf ,OPSnone]:=@op_pushfq;
 jit_cbs[OPPnone,OPpushf ,OPSx_q ]:=@op_pushfq;
 //
 //jit_cbs[OPpopf  ,OPSnone]:=@;
 //jit_cbs[OPpopf  ,OPSx_q ]:=@;

 jit_cbs[OPPnone,OPsyscall,OPSnone]:=@op_syscall;
 jit_cbs[OPPnone,OPint    ,OPSnone]:=@op_int;
 jit_cbs[OPPnone,OPud2    ,OPSnone]:=@op_ud2;

 jit_cbs[OPPnone,OPiret,OPSnone]:=@op_iretq;
 jit_cbs[OPPnone,OPiret,OPSx_d ]:=@op_iretq;
 jit_cbs[OPPnone,OPiret,OPSx_q ]:=@op_iretq;

 jit_cbs[OPPnone,OPhlt ,OPSnone]:=@op_hlt;

 jit_cbs[OPPnone,OPcpuid,OPSnone]:=@op_cpuid;
 jit_cbs[OPPnone,OPrdtsc,OPSnone]:=@op_rdtsc;

 jit_cbs[OPPnone,OPnop,OPSnone]:=@op_nop;

end;

function test_disassemble(addr:Pointer;vsize:Integer):Boolean;
var
 proc:TDbgProcess;
 adec:TX86AsmDecoder;
 ptr,fin:Pointer;
 ACodeBytes,ACode:RawByteString;
begin
 Result:=True;

 ptr:=addr;
 fin:=addr+vsize;

 proc:=TDbgProcess.Create(dm64);
 adec:=TX86AsmDecoder.Create(proc);

 while (ptr<fin) do
 begin
  adec.Disassemble(ptr,ACodeBytes,ACode);

  case adec.Instr.OpCode.Opcode of
   OPX_Invalid..OPX_GroupP:
    begin
     Result:=False;
     Break;
    end;
   else;
  end;

  if (adec.Instr.Flags * [ifOnly32, ifOnly64, ifOnlyVex] <> []) or
     is_invalid(adec.Instr) then
  begin
   Result:=False;
   Break;
  end;

 end;

 adec.Free;
 proc.Free;
end;

procedure pick(var ctx:t_jit_context2);
begin
 vm_map_lock  (@g_vmspace.vm_map);

 pick_locked(ctx);

 vm_map_unlock(@g_vmspace.vm_map);
end;

procedure pick_locked(var ctx:t_jit_context2);
const
 SCODES:array[TSimdOpcode] of Byte=(0,0,1,3,2);
 MCODES:array[0..3] of RawByteString=('','0F','0F38','0F3A');
label
 _next,
 _build;
var
 addr:Pointer;
 ptr:Pointer;

 links:t_jit_context2.t_forward_links;
 entry_link:Pointer;

 proc:TDbgProcess;
 adec:TX86AsmDecoder;
 ACodeBytes,ACode:RawByteString;

 cb:t_jit_cb;

 link_new :t_jit_i_link;
 link_curr:t_jit_i_link;
 link_next:t_jit_i_link;

 node,node_curr,node_next:p_jit_instruction;
begin
 if (ctx.max=QWORD(-1)) then
 begin
  //dont scan rip relative
  ctx.max:=0;
 end else
 begin
  ctx.max:=QWORD(ctx.max_forward_point);
 end;

 Writeln(' ctx.text_start:0x',HexStr(ctx.text_start,16));
 Writeln(' ctx.max       :0x',HexStr(ctx.max,16));
 Writeln(' ctx.text___end:0x',HexStr(ctx.text___end,16));
 Writeln(' ctx.map____end:0x',HexStr(ctx.map____end,16));

 links:=Default(t_jit_context2.t_forward_links);
 addr:=nil;

 if not ctx.fetch_forward_point(links,addr) then
 begin
  ctx.Free;
  Exit;
 end;

 ctx.trim:=False;

 entry_link:=addr;

 ctx.builder._new_chunk(QWORD(entry_link));

 ptr:=addr;

 proc:=TDbgProcess.Create(dm64);
 adec:=TX86AsmDecoder.Create(proc);

 while True do
 begin

  if ((pmap_get_raw(QWORD(ptr)) and PAGE_PROT_EXECUTE)=0) then
  begin
   writeln('not excec:0x',HexStr(ptr));

   link_curr:=ctx.builder.get_curr_label.after;
   ctx.builder.ud2;
   link_next:=ctx.builder.get_curr_label.after;

   ctx.trim:=True;
   goto _next; //trim
  end;

  ctx.ptr_curr:=ptr;

  adec.Disassemble(ptr,ACodeBytes,ACode);

  ctx.ptr_next:=ptr;

  case adec.Instr.OpCode.Opcode of
   OPX_Invalid..OPX_GroupP:
    begin
     //invalid
     writeln('invalid:0x',HexStr(ctx.ptr_curr));

     link_curr:=ctx.builder.get_curr_label.after;
     ctx.builder.ud2;
     link_next:=ctx.builder.get_curr_label.after;

     ctx.trim:=True;
     goto _next; //trim
    end;
   else;
  end;

  if (adec.Instr.Flags * [ifOnly32, ifOnly64, ifOnlyVex] <> []) or
     is_invalid(adec.Instr) then
  begin
   writeln('invalid:0x',HexStr(ctx.ptr_curr));

   link_curr:=ctx.builder.get_curr_label.after;
   ctx.builder.ud2;
   link_next:=ctx.builder.get_curr_label.after;

   ctx.trim:=True;
   goto _next; //trim
  end;

  if print_asm then
  begin
   Writeln('original------------------------':32,' ','0x',HexStr(ptr-adec.Disassembler.CodeIdx));
   Writeln(ACodeBytes:32,' ',ACode);
   Writeln('original------------------------':32,' ','0x',HexStr(ptr));
  end;

  ctx.code:=ctx.ptr_curr;

  ctx.dis:=adec.Disassembler;
  ctx.din:=adec.Instr;

  if is_rep_prefix(ctx.din) then
  begin
   cb:=nil;
   if (ctx.din.OpCode.Prefix=OPPnone) then
   begin
    case ctx.din.OpCode.Opcode of
     OPcmps:cb:=@op_rep_cmps;
     OPstos:cb:=@op_rep_stos;
     else;
    end;
   end;
  end else
  begin
   cb:=jit_cbs[ctx.din.OpCode.Prefix,ctx.din.OpCode.Opcode,ctx.din.OpCode.Suffix];
  end;

  if (cb=nil) then
  begin
   Writeln('original------------------------':32,' ','0x',HexStr(ptr-adec.Disassembler.CodeIdx));
   Writeln(ACodeBytes:32,' ',ACode);
   Writeln('original------------------------':32,' ','0x',HexStr(ptr));

   Writeln('Unhandled jit:',
           ctx.din.OpCode.Prefix,' ',
           ctx.din.OpCode.Opcode,' ',
           ctx.din.OpCode.Suffix,' ',
           ctx.din.Operand[1].Size,' ',
           ctx.din.Operand[2].Size);
   Writeln('opcode=$',HexStr(ctx.dis.opcode,2),' ',
           'MIndex=',ctx.dis.ModRM.Index,' ',
           'SOpcode=',ctx.dis.SimdOpcode,':',SCODES[ctx.dis.SimdOpcode],' ',
           'mm=',ctx.dis.mm,':',MCODES[ctx.dis.mm and 3]);
   Assert(false);
  end;

  link_curr:=ctx.builder.get_curr_label.after;
  node_curr:=link_curr._node;

  cb(ctx);

  link_next:=ctx.builder.get_curr_label.after;
  node_next:=link_next._node;

  {
  if (node_curr<>node_next) and
     (node_curr<>nil) then
  begin
   node:=TAILQ_NEXT(node_curr,@node_curr^.link);

   while (node<>nil) do
   begin

    if not test_disassemble(@node^.AData,node^.ASize) then
    begin
     print_asm:=True;
     Break;
    end;


    node:=TAILQ_NEXT(node,@node^.link);
   end;
  end;
  }

  //debug print
  if print_asm then
  if (node_curr<>node_next) and
     (node_curr<>nil) then
  begin
   node:=TAILQ_NEXT(node_curr,@node_curr^.link);

   Writeln('recompiled----------------------':32,' ','');
   while (node<>nil) do
   begin

    print_disassemble(@node^.AData,node^.ASize);


    node:=TAILQ_NEXT(node,@node^.link);
   end;
   Writeln('recompiled----------------------':32,' ','');
  end;

  {
  if (qword(ptr) and $FFFFF) = $03340 then
  begin
   //print_asm:=true;
   ctx.builder.int3;
  end;

  if (qword(ptr)) = $80A2B8e9 then
  begin
   print_asm:=true;
   ctx.builder.int3;
  end;
  }

  {
  if (qword(ptr) and $FFFFF) = $29e53 then
  begin
   //print_asm:=true;
   ctx.builder.int3;
  end;

  if (qword(ptr) and $FFFFF) = $29e42 then
  begin
   //print_asm:=true;
   ctx.builder.int3;
  end;

  if (qword(ptr) and $FFFFF) = $29e45 then
  begin
   //print_asm:=true;
   ctx.builder.int3;
  end;

  if (qword(ptr) and $FFFFF) = $2b8a0 then
  begin
   //print_asm:=true;
   ctx.builder.int3;
  end;
  }

  _next:

  //debug
   op_debug_info(ctx);
  //debug

  //resolve forward links
  if (links.root<>nil) then
  begin
   links.Resolve(link_curr);
   links.root:=nil;
  end;

  //add new entry point
  if (entry_link<>nil) then
  begin
   ctx.add_entry_point(entry_link,link_curr);
   entry_link:=nil;
  end;

  //label exist in current blob
  if not ctx.trim then
  begin
   link_new:=ctx.get_link(ptr);

   if (link_new<>nil_link) then
   begin
    ctx.builder.jmp(link_new);
    //Writeln('jmp next:0x',HexStr(ptr));
    ctx.trim:=True;
   end;
  end;

  //entry exist in another blob
  if not ctx.trim then
  if exist_entry(ptr) then
  begin
   op_set_rax_imm(ctx,Int64(ptr));
   //
   op_jmp_dispatcher(ctx);
   //
   ctx.trim:=True;
  end;

  //add new label [link_curr..link_next]
  begin
   //update link_next
   link_next:=ctx.builder.get_curr_label.after;

   ctx.add_label(ctx.ptr_curr,
                 ctx.ptr_next,
                 link_curr,
                 link_next);
  end;

  if ctx.trim then
  begin
   ctx.trim:=False;

   //close chunk
   ctx.builder._end_chunk(QWORD(ctx.ptr_next));

   repeat

    if not ctx.fetch_forward_point(links,addr) then
    begin
     goto _build;
    end;

    link_new:=ctx.get_link(addr);
    if (link_new=nil_link) then
    begin
     //Writeln('not found:0x',HexStr(addr));
     Break;
    end else
    begin
     links.Resolve(link_new);
     links.root:=nil;
     //
     ctx.add_entry_point(addr,link_new);
    end;

   until false;

   entry_link:=addr;

   ctx.builder._new_chunk(QWORD(entry_link));

   ptr:=addr;
  end;

 end;

 _build:
 //build blob

 ctx.builder.ud2;

 build(ctx);
 ctx.Free;

 adec.Free;
 proc.Free;
end;

initialization
 init_cbs;


end.


