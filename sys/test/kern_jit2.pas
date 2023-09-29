unit kern_jit2;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 x86_fpdbgdisas,
 x86_jit,
 kern_stub,
 kern_jit2_ctx,
 kern_jit2_ops_avx;

const
 print_asm=false;

procedure pick(var ctx:t_jit_context2);

implementation

uses
 sysutils,
 kern_thr,
 ucontext,
 vmparam,
 vm_pmap,
 systm,
 trap,
 md_context,
 kern_sig,
 kern_jit2_ops,
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

procedure jit_before_start; assembler; nostackframe;
asm
 nop
end;

procedure jit_jmp_dispatch; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 push %rdi
 push %rsi
 push %rdx
 push %rcx
 push %r8
 push %r9
 push %r10
 push %r11

 mov  %rax,%rdi

 call jmp_dispatcher

 pop  %r11
 pop  %r10
 pop  %r9
 pop  %r8
 pop  %rcx
 pop  %rdx
 pop  %rsi
 pop  %rdi

 //epilog
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

procedure jit_cpuid;
begin
 Writeln('TODO:jit_cpuid');
 Assert(False);
end;

procedure op_jmp_dispatcher(var ctx:t_jit_context2);
begin
 ctx.builder.call_far(@jit_jmp_dispatch); //input:rax
end;

procedure op_push_rip(var ctx:t_jit_context2;used_r_tmp0:Boolean);
var
 i:Integer;
 stack:TRegValue;
 imm:Int64;
begin
 //lea rsp,[rsp-8]
 //mov [rsp],rax

 with ctx.builder do
 begin
  if used_r_tmp0 then
  begin
   push(r_tmp0);
  end;

  stack:=r_tmp0;

  i:=GetFrameOffset(rsp);
  movq(stack,[r_thrd+i]);
  leaq(stack,[stack-8]);
  movq([r_thrd+i],stack);

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
   movi(os64,[stack],imm);
  end;

  if used_r_tmp0 then
  begin
   pop(r_tmp0);
  end;
 end;
end;

procedure op_pop_rip(var ctx:t_jit_context2); //out:rax
var
 i:Integer;
 stack:TRegValue;
begin
 //mov rax,[rsp]
 //lea rsp,[rsp+8]

 with ctx.builder do
 begin
  stack:=r_tmp0;

  i:=GetFrameOffset(rsp);
  movq(stack,[r_thrd+i]);

  call_far(@uplift_jit); //in/out:rax uses:r14

  movq(r_tmp1,[stack]);

  seto(al);
  lahf;
   addi8(os64,[r_thrd+i],8);
  addi(al,127);
  sahf;

  movq(r_tmp0,r_tmp1);
 end;
end;

function is_rsp(const r:TRegValue):Boolean; inline;
begin
 Result:=False;
 case r.AType of
  regGeneral:
   case r.AIndex of
     4:Result:=True;
    else;
   end;
  else;
 end;
end;

function is_rsp(const r:TRegValues):Boolean; inline;
begin
 Result:=is_rsp(r[0]) or is_rsp(r[1]);
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
 i:Integer;
 label_id:t_jit_i_link;
begin
 if (ctx.din.Operand[1].RegValue[0].AType=regNone) then
 begin
  ofs:=0;
  if not GetTargetOfs(ctx.din,ctx.code,1,ofs) then
  begin
   Assert(false);
  end;

  dst:=ctx.ptr_next+ofs;

  if ctx.is_curr_addr(QWORD(dst)) then
  begin
   label_id:=ctx.find_label(dst);

   if (label_id<>nil_link) then
   begin
    op_push_rip(ctx,false);
    //
    ctx.builder.jmp(label_id);
   end else
   begin
    op_push_rip(ctx,false);
    //
    id:=ctx.builder.jmp(nil_link);
    ctx.add_forward_point(id,dst);
   end;
  end else
  begin
   op_push_rip(ctx,false);
   //
   op_set_rax_imm(ctx,Int64(dst));
   //
   op_jmp_dispatcher(ctx);
  end;

 end else
 if is_memory(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  if is_rsp(ctx.din.Operand[1].RegValue) then
  begin
   build_lea(ctx,1,new1);
   //
   op_push_rip(ctx,true);
  end else
  begin
   op_push_rip(ctx,false);
   //
   build_lea(ctx,1,new1);
  end;
  //
  op_jmp_dispatcher(ctx);
 end else
 if is_preserved(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  op_push_rip(ctx,false);
  //
  i:=GetFrameOffset(ctx.din.Operand[1].RegValue[0]);
  ctx.builder.movq(new1,[r_thrd+i]);
  //
  op_jmp_dispatcher(ctx);
 end else
 begin
  op_push_rip(ctx,false);
  //
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  new2:=new_reg(ctx.din.Operand[1]);
  //
  ctx.builder.movq(new1,new2);
  //
  op_jmp_dispatcher(ctx);
 end;

 //
 ctx.add_forward_point(nil_link,ctx.ptr_next);
end;

procedure op_ret(var ctx:t_jit_context2);
begin
 Assert(ctx.din.Operand[1].ByteCount=0);

 op_pop_rip(ctx); //out:rax
 //
 op_jmp_dispatcher(ctx);
 //
 ctx.ptr_next:=nil; //trim
end;

procedure op_jmp(var ctx:t_jit_context2);
var
 id:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 new1,new2:TRegValue;
 i:Integer;
 label_id:t_jit_i_link;
begin
 if (ctx.din.Operand[1].RegValue[0].AType=regNone) then
 begin
  ofs:=0;
  if not GetTargetOfs(ctx.din,ctx.code,1,ofs) then
  begin
   Assert(false);
  end;

  dst:=ctx.ptr_next+ofs;

  if ctx.is_curr_addr(QWORD(dst)) then
  begin
   label_id:=ctx.find_label(dst);

   if (label_id<>nil_link) then
   begin
    ctx.builder.jmp(label_id);
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
  build_lea(ctx,1,new1);
  //
  op_jmp_dispatcher(ctx);
 end else
 if is_preserved(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  i:=GetFrameOffset(ctx.din.Operand[1].RegValue[0]);
  ctx.builder.movq(new1,[r_thrd+i]);
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
 ctx.ptr_next:=nil; //trim
end;

procedure op_jcc(var ctx:t_jit_context2);
var
 id,id2:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 label_id:t_jit_i_link;
begin
 ofs:=0;
 if not GetTargetOfs(ctx.din,ctx.code,1,ofs) then
 begin
  Assert(false);
 end;

 dst:=ctx.ptr_next+ofs;

 if ctx.is_curr_addr(QWORD(dst)) then
 begin
  label_id:=ctx.find_label(dst);

  if (label_id<>nil_link) then
  begin
   ctx.builder.jcc(ctx.din.OpCode.Suffix,label_id);
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
 i:Integer;
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

   i:=GetFrameOffset(ctx.din.Operand[1]);
   movq(new,[r_thrd+i]);
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

  i:=GetFrameOffset(rsp);
  movq(stack,[r_thrd+i]);
  leaq(stack,[stack-OPERAND_BYTES[new.ASize]]);
  movq([r_thrd+i],stack);

  call_far(@uplift_jit); //in/out:rax uses:r14

  movq([stack],new);
 end;
end;

procedure op_pushfq(var ctx:t_jit_context2);
var
 i:Integer;
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

  i:=GetFrameOffset(rsp);
  movq(stack,[r_thrd+i]);
  leaq(stack,[stack-OPERAND_BYTES[new.ASize]]);
  movq([r_thrd+i],stack);

  call_far(@uplift_jit); //in/out:rax uses:r14

  movq([stack],new);
 end;
end;

procedure op_pop(var ctx:t_jit_context2);
var
 i:Integer;
 new,stack:TRegValue;
begin
 //mov reg,[rsp]
 //lea rsp,[rsp+len]

 with ctx.builder do
 begin
  stack:=r_tmp0;

  i:=GetFrameOffset(rsp);
  movq(stack,[r_thrd+i]);

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

   i:=GetFrameOffset(ctx.din.Operand[1]);
   movq([r_thrd+i],new);
  end else
  begin
   new:=new_reg(ctx.din.Operand[1]);

   movq(new,[stack]);
  end;

  i:=GetFrameOffset(rsp);

  seto(al);
  lahf;
   addi8(os64,[r_thrd+i],OPERAND_BYTES[new.ASize]);
  addi(al,127);
  sahf;
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
    ctx.ptr_next:=nil; //trim
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
 ctx.ptr_next:=nil; //trim
end;

procedure op_iretq(var ctx:t_jit_context2);
begin
 //exit proc?
 ctx.builder.call_far(@jit_exit_proc); //TODO exit dispatcher
 ctx.ptr_next:=nil; //trim
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

const
 test_desc:t_op_type=(op:$85;index:0);

procedure op_rep_cmps(var ctx:t_jit_context2);
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
    _RR(test_desc,rcx,rcx,os0);
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



   leaq(rdi,[rdi+OPERAND_BYTES[size]]);
   leaq(rsi,[rsi+OPERAND_BYTES[size]]);

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

///

procedure op_rep_stos(var ctx:t_jit_context2);
var
 i:Integer;
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

  i:=GetFrameOffset(rax);
  movq(new,[r_thrd+i]);

  link_start:=ctx.builder.get_curr_label.after;

  //repeat
   seto(al);
   lahf;
    _RR(test_desc,rcx,rcx,os0);
    link_jmp0:=jcc(OPSc_z,nil_link,os8);
   addi(al,127);
   sahf;

   movq(r_tmp0,rdi);
   call_far(@uplift_jit); //in/out:rax uses:r14

   movq([r_tmp0],new);

   leaq(rcx,[rcx-1]);
   leaq(rdi,[rdi+OPERAND_BYTES[size]]);

  //until
  jmp(link_start,os8);

  //exit1
  sahf;

  //exit2

  link___end:=ctx.builder.get_curr_label.before; //exit1

  link_jmp0._label:=link___end;
 end;

end;


var
 inited:Integer=0;

procedure init_cbs;
begin
 if (inited<>0) then Exit;

 jit_cbs[OPPnone,OPcall,OPSnone]:=@op_call;
 jit_cbs[OPPnone,OPjmp ,OPSnone]:=@op_jmp;
 jit_cbs[OPPnone,OPret ,OPSnone]:=@op_ret;

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

 kern_jit2_ops.init_cbs;
 init_cbs_avx;

 inited:=1;
end;

procedure pick(var ctx:t_jit_context2);
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

 node_new,node_curr:t_jit_i_link;
 node,node_code1,node_code2:p_jit_instruction;

 blob:p_jit_dynamic;
 entry_point:t_jit_context2.p_entry_point;

 //F:THandle;
begin

 init_cbs;

 ctx.max:=QWORD(ctx.max_forward_point);
 Writeln(' ctx.text_start:0x',HexStr(ctx.text_start,16));
 Writeln(' ctx.max       :0x',HexStr(ctx.max,16));
 Writeln(' ctx.text___end:0x',HexStr(ctx.text___end,16));

 links:=Default(t_jit_context2.t_forward_links);
 addr:=nil;

 if not ctx.fetch_forward_point(links,addr) then
 begin
  Exit;
 end;

 entry_link:=addr;

 Writeln('0x',HexStr(entry_link));

 //debug
   ctx.builder.call_far(@jit_before_start);
 //debug

 ptr:=addr;

 proc:=TDbgProcess.Create(dm64);
 adec:=TX86AsmDecoder.Create(proc);

 while True do
 begin

  if ((pmap_get_raw(QWORD(ptr)) and PAGE_PROT_EXECUTE)=0) then
  begin
   //writeln('not excec:0x',HexStr(ptr));
   ctx.builder.ud2;
   goto _next;
  end;

  ctx.ptr_curr:=ptr;

  adec.Disassemble(ptr,ACodeBytes,ACode);

  case adec.Instr.OpCode.Opcode of
   OPX_Invalid..OPX_GroupP:
    begin
     //invalid
     //writeln('invalid:0x',HexStr(ctx.ptr_curr));
     ctx.builder.ud2;
     goto _next;
    end;
   else;
  end;

  if (adec.Instr.Flags * [ifOnly32, ifOnly64, ifOnlyVex] <> []) or
     is_invalid(adec.Instr) then
  begin
   //writeln('invalid:0x',HexStr(ctx.ptr_curr));
   ctx.builder.ud2;
   goto _next;
  end;

  if print_asm then
  begin
   Writeln('original------------------------':32,' ','0x',HexStr(ptr-adec.Disassembler.CodeIdx));
   Writeln(ACodeBytes:32,' ',ACode);
   Writeln('original------------------------':32,' ','0x',HexStr(ptr));
  end;

  ctx.ptr_next:=ptr;

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
   Writeln('MIndex=',ctx.dis.ModRM.Index,' ',
           'SOpcode=',ctx.dis.SimdOpcode,':',SCODES[ctx.dis.SimdOpcode],' ',
           'mm=',ctx.dis.mm,':',MCODES[ctx.dis.mm and 3]);
   Assert(false);
  end;

  node_curr:=ctx.builder.get_curr_label.after;
  node_code1:=node_curr._node;

  cb(ctx);

  node_code2:=ctx.builder.get_curr_label._node;

  if print_asm then
  if (node_code1<>node_code2) and
     (node_code1<>nil) then
  begin
   node:=TAILQ_NEXT(node_code1,@node_code1^.link);

   Writeln('recompiled----------------------':32,' ','');
   while (node<>nil) do
   begin

    print_disassemble(@node^.AData,node^.ASize);


    node:=TAILQ_NEXT(node,@node^.link);
   end;
   Writeln('recompiled----------------------':32,' ','');
  end;

  //debug
   op_set_rax_imm(ctx,$FACEADD7);
   op_set_rax_imm(ctx,Int64(ctx.ptr_next));
   op_set_rax_imm(ctx,0);
  //debug

  begin
   ctx.add_label(ctx.ptr_curr,node_curr);
  end;

  if (links.root<>nil) then
  begin
   links.Resolve(node_curr);
   links.root:=nil;
  end;

  if (entry_link<>nil) then
  begin
   ctx.add_entry_point(entry_link,node_curr);
   entry_link:=nil;
  end;

  begin
   node_new:=ctx.find_label(ptr);

   if (node_new<>nil_link) then
   begin
    ctx.builder.jmp(node_new);
    ctx.ptr_next:=nil;
    //Writeln('jmp next:0x',HexStr(ptr));
   end;

  end;

  if (ctx.ptr_next=nil) then
  begin
   _next:
   repeat

    if not ctx.fetch_forward_point(links,addr) then
    begin
     goto _build;
    end;

    node_new:=ctx.find_label(addr);
    if (node_new=nil_link) then
    begin
     //Writeln('not found:0x',HexStr(addr));
     Break;
    end else
    begin
     links.Resolve(node_new);
     links.root:=nil;
     //
     ctx.add_entry_point(addr,node_new);
    end;

   until false;

   //debug
    ctx.builder.call_far(@jit_before_start);
   //debug

   entry_link:=addr;

   ptr:=addr;
  end;

 end;

 _build:
 //build blob

 ctx.builder.ud2;

 blob:=new_blob(ctx.builder.GetMemSize);

 ctx.builder.SaveTo(blob^.base,ctx.builder.GetMemSize);

 //F:=FileCreate('recompile.bin');
 //FileWrite(F,data^,ctx.builder.GetMemSize);
 //FileClose(F);

  //copy entrys
 entry_point:=ctx.entry_list;
 while (entry_point<>nil) do
 begin
  addr:=blob^.base+entry_point^.label_id.offset;
  //
  blob^.add_entry_point(entry_point^.src,addr);
  //
  entry_point:=entry_point^.next;
 end;

 blob^.attach;

 adec.Free;
 proc.Free;

 ctx.builder.Free;
end;


end.

