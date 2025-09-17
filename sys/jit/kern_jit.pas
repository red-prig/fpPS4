unit kern_jit;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 md_systm,
 ps4libdoc,
 mqueue,
 systm,
 x86_fpdbgdisas,
 x86_jit,
 kern_jit_ctx;

var
 print_asm :Boolean=False;
 debug_info:Boolean=False;

procedure pick(var ctx:t_jit_context2;preload:Pointer);

implementation

uses
 sysutils,
 time,
 vm,
 vmparam,
 vm_pmap_prot,
 vm_pmap,
 vm_map,
 vm_tracking_map,
 sys_bootparam,
 kern_proc,
 kern_jit_ops,
 kern_jit_ops_sse,
 kern_jit_ops_avx,
 kern_jit_dynamic,
 kern_jit_test,
 kern_jit_asm,
 kern_thr,
 subr_backtrace;

procedure jit_assert(tf_rip:QWORD);
var
 td:p_kthread;
begin
 td:=curkthread;
 jit_save_to_sys_save(td);
 td^.td_frame.tf_rip:=tf_rip;
 print_error_td('Assert in guest code!');
 Assert(false);
end;

procedure _jit_assert; assembler; nostackframe;
asm
 call jit_save_ctx
 mov  %r14,%rdi
 jmp  jit_assert
end;

procedure jit_system_error(tf_rip:QWORD);
var
 td:p_kthread;
begin
 td:=curkthread;
 jit_save_to_sys_save(td);
 td^.td_frame.tf_rip:=tf_rip;
 print_error_td('System error in guest code!');
 Assert(false);
end;

procedure _jit_system_error; assembler; nostackframe;
asm
 call jit_save_ctx
 mov  %r14,%rdi
 jmp  jit_system_error
end;

procedure jit_unknow_int;
begin
 Assert(False,'jit_unknow_int');
end;

procedure jit_exit_proc(tf_rip:QWORD);
var
 td:p_kthread;
begin
 td:=curkthread;
 jit_save_to_sys_save(td);
 td^.td_frame.tf_rip:=tf_rip;
 print_error_td('TODO:jit_exit_proc');
 Assert(False);
end;

procedure _jit_exit_proc; assembler; nostackframe;
asm
 call jit_save_ctx
 mov  %r14,%rdi
 jmp  jit_exit_proc
end;

//in:r14(addr) out:r14(addr)
procedure op_jmp_plt(var ctx:t_jit_context2);
var
 plt      :t_jit_i_link;
 link_jcxz:t_jit_i_link;
 link_jmp :t_jit_i_link;
 //link_jne :t_jit_i_link;
begin
 with ctx.builder do
 begin

  movq(r13,rcx); //save rcx (break jit_frame)

  plt:=leap(r15);
  movq(r15,[r15]); //plt^

  movq(rcx,[r15+(@p_jplt_cache_asm(nil)^.neg)]); //plt^.neg

  leaq(rcx,[rcx+r14]);

  link_jcxz:=jcxz(nil_link,as64,os8);

  //plt cache fail

  movq(rcx,r13); //restore rcx

  //restore jit_frame in jit_jmp_dispatch

  //reload plt link
  leap(r15,plt);
  call_far(@jit_jmp_dispatch); //input:r14,r15 out:r14

  //exit:
  link_jmp:=jmp(nil_link,os8); //jmp _exit

  //plt cache succes
  link_jcxz.target:=ctx.builder.get_curr_label.after;

  movq(rcx,r13); //restore rcx

  //restore jit_frame
  movq(r13,[GS +teb_thread]);
  leaq(r13,[r13+jit_frame_offset]);

  movq(r14,[r15+(@p_jplt_cache_asm(nil)^.dst)]); //plt^.dst

  //exit
  link_jmp.target:=ctx.builder.get_curr_label.after;

  /////////////////////////////////////////

  {

  plt:=leap(r15);
  movq(r15,[r15]); //plt^

  pushfq(os64);

  cmpq(r14,[r15+(@p_jplt_cache_asm(nil)^.src)]);

  //next
  instr.target:=get_curr_label.after;

  link_jne:=jcc(OPSc_nz,nil_link,os8); //jne _non_cache

  popfq(os64);

  //get blk
  movq(r14,[r15+(@p_jplt_cache_asm(nil)^.blk)]);

  //save current block
  movq([r13+
           (
            -(@p_kthread(nil)^.td_frame.tf_r13)
            +(@p_kthread(nil)^.td_jctx.block)
           )
       ],r14);

  //get dst
  movq(r14,[r15+(@p_jplt_cache_asm(nil)^.dst)]);

  //interrupt
  //jmp %gs:teb.jit_trp

  link_jmp:=jmp(nil_link,os8); //jmp _exit

  //_non_cache:
  link_jne.target:=ctx.builder.get_curr_label.after;

  popfq(os64);

  leap(r15,plt);
  call_far(@jit_jmp_dispatch); //input:r14,r15 out:r14

  //_exit:
  link_jmp.target:=ctx.builder.get_curr_label.after;

  }


 end;
end;

procedure op_jmp_dispatcher(var ctx:t_jit_context2;cb:t_jit_cb);
begin
 with ctx.builder do
 begin

  //leap(r15);
  //call_far(@jit_jmp_plt_cache); //input:r14,r15 out:r14

  op_jmp_plt(ctx);

  if (cb<>nil) then
  begin
   cb(ctx);
  end;

  jmp(r14);
 end;
end;

function op_add_local_cache(var ctx:t_jit_context2):t_jit_i_link;
begin
 with ctx.builder do
 begin
  movq(r15,[r13-jit_frame_offset+(@p_kthread(nil)^.td_jctx.call_ret_cache)]);

  movi(r15w,Word(QWORD(ctx.ptr_next)*16));

  Result:=leaj(r14,[rip+$7FFFFFFF],nil_link); //set deferred
  movq([r15+8,os64],r14); //dst

  op_set_mem_imm(ctx,[r15+0,os64],-Int64(ctx.ptr_next),[not_use_r_tmp1]); //-src
 end;
end;

procedure op_call_dispatcher(var ctx:t_jit_context2;cb:t_jit_cb);
begin
 with ctx.builder do
 begin

  //leap(r15);
  //call_far(@jit_jmp_plt_cache); //input:r14,r15 out:r14

  op_jmp_plt(ctx);

  if (cb<>nil) then
  begin
   cb(ctx);
  end;

  jmp(r14);
 end;
end;

//r14
procedure op_ret_dispatcher(var ctx:t_jit_context2;cb:t_jit_cb);
var
 link_jcxz:t_jit_i_link;
 link_jmp :t_jit_i_link;
begin
 with ctx.builder do
 begin
  movq(r15,[r13-jit_frame_offset+(@p_kthread(nil)^.td_jctx.call_ret_cache)]);

  movq(r13,rcx); //save rcx

  movi(ecx,0);
  leaq(rcx,[r14*8+rcx]);
  leaq(r15w,[rcx+rcx]); //Word(r14*16)

  movq(rcx,[r15]); //-src

  leaq(rcx,[r14+rcx]); //r14+(-src)

  link_jcxz:=jcxz(nil_link,as64,os8); //r14+(-src)=0

  //plt cache fail

  movq(rcx,r13); //restore rcx

  //restore jit_frame in jit_jmp_dispatch

  //stub plt link
  movi(r15d,0);
  call_far(@jit_jmp_dispatch); //input:r14,r15 out:r14

  //exit:
  link_jmp:=jmp(nil_link,os8); //jmp _exit

  //plt cache succes
  link_jcxz.target:=ctx.builder.get_curr_label.after;

  movq(rcx,r13); //restore rcx

  //restore jit_frame
  movq(r13,[GS +teb_thread]);
  leaq(r13,[r13+jit_frame_offset]);

  movq(r14,[r15+8]); //dst

  //exit
  link_jmp.target:=ctx.builder.get_curr_label.after;

  //leap(r15);
  //call_far(@jit_jmp_plt_cache); //input:r14,r15 out:r14

  //op_jmp_plt(ctx);

  if (cb<>nil) then
  begin
   cb(ctx);
  end;

  jmp(r14);
 end;
end;

var
 jit_nop_sequence:Boolean=False;

function is_nop_sequence(addr:Pointer):Boolean;
type
 t_data_16=array[0..15] of Byte;
var
 beg:Pointer;
 ptr:Pointer;
 data:t_data_16;

 dis:TX86Disassembler;
 din:TInstruction;
begin
 Result:=False;

 if not jit_nop_sequence then Exit;

 dis:=Default(TX86Disassembler);
 din:=Default(TInstruction);

 repeat

  data:=Default(t_data_16);
  md_copyin(addr,@data,16,nil);

  ptr:=@data;
  beg:=ptr;

  if (PWORD(beg)^=0) then //00 00
  begin
   //is end of code
   Exit(False);
  end;

  if (PDWORD(beg)^=$e5894855) then //55 48 89 e5
  begin
   //is func prolog
   Exit(True);
  end;

  dis.Disassemble(dm64,ptr,din);

  addr:=addr+(ptr-beg);

  case din.OpCode.Opcode of
   OPnop:
    begin
     //mark is nop
     Result:=True;
    end;
   OPX_Invalid..OPX_GroupP:
    begin
     //invalid code
     Exit(False);
    end;
   else
    begin
     //end of sequence
     Exit;
    end;
  end;

 until false;

end;

procedure trim_flow(var ctx:t_jit_context2);
begin
 if is_nop_sequence(ctx.ptr_next) then
 begin
  //
 end else
 begin
  ctx.trim:=True;
 end;
end;

procedure op_push_rip_part0(var ctx:t_jit_context2);
var
 stack:TRegValue;
 imm:Int64;
begin
 //lea rsp,[rsp-8]
 //mov [rsp],r14

 with ctx.builder do
 begin
  stack:=r_tmp0;

  op_load_rsp(ctx,stack);
  leaq(stack,[stack-8]);

  op_uplift(ctx,stack,os64); //in/out:r14

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

  //For transactionality,
  //first we move the memory,
  //then we update the register
  //[op_uplift] op_load_rsp(ctx,stack);
  //[op_uplift] leaq(stack,[stack-8]);
  //op_save_rsp(ctx,stack);

 end;
end;

procedure op_push_rip_part1(var ctx:t_jit_context2);
var
 stack:TRegValue;
begin
 //lea rsp,[rsp-8]
 //mov [rsp],r14

 with ctx.builder do
 begin
  stack:=r_tmp1;

  //For transactionality,
  //first we move the memory,
  //then we update the register
  op_load_rsp(ctx,stack);
  leaq(stack,[stack-8]);
  op_save_rsp(ctx,stack);

 end;
end;

procedure op_pop_rip_part0(var ctx:t_jit_context2;imm:Word); //out:r14
var
 stack:TRegValue;
begin
 //mov r14,[rsp]
 //lea rsp,[rsp+8+imm]

 with ctx.builder do
 begin
  stack:=r_tmp0;

  op_load_rsp(ctx,stack);

  op_uplift(ctx,stack,os64); //in/out:r14

  //load to tmp
  movq(r_tmp0,[stack]);

  //For transactionality,
  //first we move the memory,
  //then we update the register
  //[op_uplift] op_load_rsp(ctx,stack);
  //leaq(stack,[stack+8+imm]);
  //op_save_rsp(ctx,stack);

  //out:r14
 end;
end;

procedure op_pop_rip_part1(var ctx:t_jit_context2);
var
 stack:TRegValue;
begin
 //mov r14,[rsp]
 //lea rsp,[rsp+8+imm]

 with ctx.builder do
 begin
  stack:=r_tmp1;

  //For transactionality,
  //first we move the memory,
  //then we update the register
  op_load_rsp(ctx,stack);
  leaq(stack,[stack+8+ctx.imm]);
  op_save_rsp(ctx,stack);

 end;
end;

procedure op_call(var ctx:t_jit_context2);
var
 id:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 new1,new2:TRegValue;
 link:t_jit_i_link;
 ret_dst:t_jit_i_link;
begin
 ctx.label_flags:=ctx.label_flags or LF_JMP;

 ret_dst:=op_add_local_cache(ctx);

 op_push_rip_part0(ctx);

 if (ctx.din.Operand[1].RegValue[0].AType=regNone) then
 begin
  //imm offset

  ofs:=0;
  GetTargetOfs(ctx.din,ctx.code,1,ofs);

  dst:=ctx.ptr_next+ofs;

  if ctx.is_text_addr(QWORD(dst)) and
     (not exist_entry(dst)) then
  begin
   //near

   op_push_rip_part1(ctx);

   link:=ctx.get_link(dst);

   id:=ctx.builder.jmp(link);

   if (link<>nil_link) then
   begin
    ctx.add_forward_point(fpCall,dst);
   end else
   begin
    ctx.add_forward_point(fpCall,id,dst);
   end;
  end else
  begin
   op_set_r14_imm(ctx,Int64(dst));
   //
   op_call_dispatcher(ctx,@op_push_rip_part1);
  end;

 end else
 if is_memory(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  build_lea(ctx,1,new1,[{inc8_rsp,}code_ref]);
  //
  op_uplift(ctx,new1,os64); //in/out:r14
  //
  ctx.builder.movq(new1,[new1]);
  //
  op_call_dispatcher(ctx,@op_push_rip_part1);
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
  op_call_dispatcher(ctx,@op_push_rip_part1);
 end else
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  new2:=new_reg(ctx.din.Operand[1]);
  //
  ctx.builder.movq(new1,new2);
  //
  op_call_dispatcher(ctx,@op_push_rip_part1);
 end;

 if (ret_dst<>nil_link) then
 begin
  ret_dst.target:=ctx.builder.get_curr_label.after;
 end;

 //
 ctx.add_forward_point(fpCall,ctx.ptr_next);
end;

procedure op_ret(var ctx:t_jit_context2);
var
 imm:Int64;
begin
 ctx.label_flags:=ctx.label_flags or LF_JMP;

 imm:=0;
 GetTargetOfs(ctx.din,ctx.code,1,imm);
 //
 ctx.imm:=imm;

 //
 op_pop_rip_part0(ctx,imm); //out:r14
 //

 op_ret_dispatcher(ctx,@op_pop_rip_part1);
 //
 trim_flow(ctx);
end;

procedure op_jmp(var ctx:t_jit_context2);
var
 id:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 new1,new2:TRegValue;
 link:t_jit_i_link;
begin
 ctx.label_flags:=ctx.label_flags or LF_JMP;

 if (ctx.din.Operand[1].RegValue[0].AType=regNone) then
 begin
  //imm offset

  ofs:=0;
  GetTargetOfs(ctx.din,ctx.code,1,ofs);

  dst:=ctx.ptr_next+ofs;

  if ctx.is_text_addr(QWORD(dst)) and
     (not exist_entry(dst)) then
  begin
   //near

   link:=ctx.get_link(dst);

   id:=ctx.builder.jmp(link);

   if (link<>nil_link) then
   begin
    ctx.add_forward_point(fpCall,dst);
   end else
   begin
    ctx.add_forward_point(fpCall,id,dst);
   end;
  end else
  begin
   op_set_r14_imm(ctx,Int64(dst));
   //
   op_jmp_dispatcher(ctx,nil);
  end;

 end else
 if is_memory(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  build_lea(ctx,1,new1,[code_ref]);
  //
  op_uplift(ctx,new1,os64); //in/out:r14
  //
  ctx.builder.movq(new1,[new1]);
  //
  op_jmp_dispatcher(ctx,nil);
 end else
 if is_preserved(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  op_load(ctx,new1,1);
  //
  op_jmp_dispatcher(ctx,nil);
 end else
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  new2:=new_reg(ctx.din.Operand[1]);
  //
  ctx.builder.movq(new1,new2);
  //
  op_jmp_dispatcher(ctx,nil);
 end;
 //
 trim_flow(ctx);
end;

function invert_cond(s:TOpCodeSuffix):TOpCodeSuffix;
begin
 case s of
  OPSc_o  :Result:=OPSc_no;
  OPSc_no :Result:=OPSc_o;
  OPSc_b  :Result:=OPSc_nb;
  OPSc_nb :Result:=OPSc_b;
  OPSc_z  :Result:=OPSc_nz;
  OPSc_nz :Result:=OPSc_z;
  OPSc_be :Result:=OPSc_nbe;
  OPSc_nbe:Result:=OPSc_be;
  OPSc_s  :Result:=OPSc_ns;
  OPSc_ns :Result:=OPSc_s;
  OPSc_p  :Result:=OPSc_np;
  OPSc_np :Result:=OPSc_p;
  OPSc_l  :Result:=OPSc_nl;
  OPSc_nl :Result:=OPSc_l;
  OPSc_le :Result:=OPSc_nle;
  OPSc_nle:Result:=OPSc_le;
  OPSc_e  :Result:=OPSc_ne;
  OPSc_ne :Result:=OPSc_e;
  OPSc_u  :Result:=OPSc_nu;
  OPSc_nu :Result:=OPSc_u;
  else;
   Assert(false,'invert_cond');
 end;
end;

procedure op_jcc(var ctx:t_jit_context2);
var
 id1:t_jit_i_link;
 //id2:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 link:t_jit_i_link;
begin
 ctx.label_flags:=ctx.label_flags or LF_JMP;

 ofs:=0;
 GetTargetOfs(ctx.din,ctx.code,1,ofs);

 dst:=ctx.ptr_next+ofs;

 if ctx.is_text_addr(QWORD(dst)) and
    (not exist_entry(dst)) then
 begin
  //near

  link:=ctx.get_link(dst);

  id1:=ctx.builder.jcc(ctx.din.OpCode.Suffix,link);

  if (link<>nil_link) then
  begin
   ctx.add_forward_point(fpCall,dst);
  end else
  begin
   ctx.add_forward_point(fpCall,id1,dst);
  end;
 end else
 begin
  //far

  //invert cond jump
  id1:=ctx.builder.jcc(invert_cond(ctx.din.OpCode.Suffix),nil_link,os8);
   op_set_r14_imm(ctx,Int64(dst));
   op_jmp_dispatcher(ctx,nil);
  id1.target:=ctx.builder.get_curr_label.after;

  {
  id1:=ctx.builder.jcc(ctx.din.OpCode.Suffix,nil_link,os8);

  id2:=ctx.builder.jmp(nil_link,os8);
   id1._label:=ctx.builder.get_curr_label.after;
   op_set_r14_imm(ctx,Int64(dst));
   op_jmp_dispatcher(ctx,nil);
  id2._label:=ctx.builder.get_curr_label.after;
  }
 end;
end;

procedure op_loop(var ctx:t_jit_context2);
var
 id1,id2:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 link:t_jit_i_link;
begin
 ctx.label_flags:=ctx.label_flags or LF_JMP;

 ofs:=0;
 GetTargetOfs(ctx.din,ctx.code,1,ofs);

 dst:=ctx.ptr_next+ofs;

 if ctx.is_text_addr(QWORD(dst)) and
    (not exist_entry(dst)) then
 begin
  //near

  link:=ctx.get_link(dst);

  //case handling in micro core
  id1:=ctx.builder.loop(ctx.din.OpCode.Suffix,link,ctx.dis.AddressSize);

  if (link<>nil_link) then
  begin
   ctx.add_forward_point(fpCall,dst);
  end else
  begin
   ctx.add_forward_point(fpCall,id1,dst);
  end;
 end else
 begin
  //far

  id1:=ctx.builder.loop(ctx.din.OpCode.Suffix,nil_link,ctx.dis.AddressSize,os8);

  id2:=ctx.builder.jmp(nil_link,os8);
   id1.target:=ctx.builder.get_curr_label.after;
   op_set_r14_imm(ctx,Int64(dst));
   op_jmp_dispatcher(ctx,nil);
  id2.target:=ctx.builder.get_curr_label.after;

 end;
end;

procedure op_jcxz(var ctx:t_jit_context2);
var
 id1,id2:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 link:t_jit_i_link;
begin
 ctx.label_flags:=ctx.label_flags or LF_JMP;

 ofs:=0;
 GetTargetOfs(ctx.din,ctx.code,1,ofs);

 dst:=ctx.ptr_next+ofs;

 if ctx.is_text_addr(QWORD(dst)) and
    (not exist_entry(dst)) then
 begin
  //near

  link:=ctx.get_link(dst);

  //case handling in micro core
  id1:=ctx.builder.jcxz(link,ctx.dis.AddressSize);

  if (link<>nil_link) then
  begin
   ctx.add_forward_point(fpCall,dst);
  end else
  begin
   ctx.add_forward_point(fpCall,id1,dst);
  end;
 end else
 begin
  //far

  id1:=ctx.builder.jcxz(nil_link,ctx.dis.AddressSize,os8);

  id2:=ctx.builder.jmp(nil_link,os8);
   id1.target:=ctx.builder.get_curr_label.after;
   op_set_r14_imm(ctx,Int64(dst));
   op_jmp_dispatcher(ctx,nil);
  id2.target:=ctx.builder.get_curr_label.after;

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

   op_uplift(ctx,r_tmp0,os64); //in/out:r14

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

  if (new.AIndex=r_tmp1.AIndex) then
  begin
   op_uplift(ctx,stack,new.ASize,[not_use_r_tmp1]); //in/out:r14
  end else
  begin
   op_uplift(ctx,stack,new.ASize); //in/out:r14
  end;

  movq([stack],new);

  //For transactionality,
  //first we move the memory,
  //then we update the register
  //[op_uplift] op_load_rsp(ctx,stack);
  //[op_uplift] leaq(stack,[stack-OPERAND_BYTES[new.ASize]]);
  op_save_rsp(ctx,stack);

 end;
end;

procedure op_pushf(var ctx:t_jit_context2);
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

  op_load_rsp(ctx,stack);
  leaq(stack,[stack-OPERAND_BYTES[mem_size]]);

  op_uplift(ctx,stack,mem_size); //in/out:r14

  //get all flags
  pushfq(mem_size);
  pop(new);

  movq([stack],new);

  //For transactionality,
  //first we move the memory,
  //then we update the register
  //[op_uplift] op_load_rsp(ctx,stack);
  //[op_uplift] leaq(stack,[stack-OPERAND_BYTES[mem_size]]);
  op_save_rsp(ctx,stack);

 end;
end;

procedure op_leave(var ctx:t_jit_context2);
var
 new,stack:TRegValue;
begin
 //mov rsp,rbp
 //mov rbp,[rsp]
 //lea rsp,[rsp+len]

 with ctx.builder do
 begin
  stack:=r_tmp0;
  new  :=r_tmp1;

  op_load_rbp(ctx,stack);

  op_uplift(ctx,stack,os64); //in/out:r14

  movq(new,[stack]);

  //For transactionality,
  //first we move the memory,
  //then we update the register
  //[op_uplift] op_load_rbp(ctx,stack);
  //[op_uplift] op_save_rsp(ctx,stack);
  op_save_rbp(ctx,new);

  //[op_uplift] op_load_rsp(ctx,stack);
  leaq(stack,[stack+OPERAND_BYTES[ctx.dis.OperandSize]]);
  op_save_rsp(ctx,stack);
 end;

end;

procedure op_popf(var ctx:t_jit_context2);
var
 mem_size:TOperandSize;
 new,stack:TRegValue;
begin
 //mov rflags,[rsp]
 //lea rsp,[rsp+len]

 with ctx.builder do
 begin
  stack:=r_tmp0;

  new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

  mem_size:=ctx.din.Operand[1].Size;

  op_load_rsp(ctx,stack);

  op_uplift(ctx,stack,mem_size); //in/out:r14

  movq(new,[stack]);
  push(new);
  popfq(mem_size);

  //For transactionality,
  //first we move the memory,
  //then we update the register
  //[op_uplift] op_load_rsp(ctx,stack);
  leaq(stack,[stack+OPERAND_BYTES[new.ASize]]);
  op_save_rsp(ctx,stack);
 end;
end;

procedure op_pop(var ctx:t_jit_context2);
var
 new,stack:TRegValue;
 reload_rsp:Boolean;
begin
 //mov reg,[rsp]
 //lea rsp,[rsp+len]

 with ctx.builder do
 begin
  stack:=r_tmp0;

  op_load_rsp(ctx,stack);

  op_uplift(ctx,stack,os64); //in/out:r14

  reload_rsp:=False;

  if is_memory(ctx.din) then
  begin
   new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

   movq(new,[stack]);

   build_lea(ctx,1,stack,[not_use_r_tmp1]);

   op_uplift(ctx,stack,os64,[not_use_r_tmp1]); //in/out:r14

   movq([stack],new);

   reload_rsp:=True;
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

  //For transactionality,
  //first we move the memory,
  //then we update the register
  if reload_rsp then
  begin
   op_load_rsp(ctx,stack);
  end;
  leaq(stack,[stack+OPERAND_BYTES[new.ASize]]);
  op_save_rsp(ctx,stack);
 end;
end;

procedure op_syscall(var ctx:t_jit_context2);
begin
 ctx.label_flags:=ctx.label_flags or LF_JMP;

 ctx.add_forward_point(fpCall,ctx.ptr_curr);
 ctx.add_forward_point(fpCall,ctx.ptr_next);
 //
 op_set_rip_imm(ctx,Int64(ctx.ptr_next));
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
  1,3:
   begin
    add_orig(ctx);
   end;

  $41: //assert?
   begin
    //
    op_set_r14_imm(ctx,Int64(ctx.ptr_curr));
    ctx.builder.call_far(@_jit_assert);
    trim_flow(ctx);
   end;

  $44: //system error?
   begin
    //
    op_set_r14_imm(ctx,Int64(ctx.ptr_curr));
    ctx.builder.call_far(@_jit_system_error);
    trim_flow(ctx);
   end;

  else
   begin
    ctx.builder.call_far(@jit_unknow_int);
    trim_flow(ctx);
   end;
 end;
end;

procedure op_ud2(var ctx:t_jit_context2);
begin
 //exit proc?
 ctx.builder.int3;
 op_set_r14_imm(ctx,Int64(ctx.ptr_curr));
 ctx.builder.call_far(@_jit_exit_proc); //TODO exit dispatcher
 trim_flow(ctx);
end;

procedure op_iretq(var ctx:t_jit_context2);
begin
 //exit proc?
 ctx.builder.int3;
 op_set_r14_imm(ctx,Int64(ctx.ptr_curr));
 ctx.builder.call_far(@_jit_exit_proc); //TODO exit dispatcher
 trim_flow(ctx);
end;

procedure op_hlt(var ctx:t_jit_context2);
begin
 //stop thread?
 ctx.builder.int3;
 op_set_r14_imm(ctx,Int64(ctx.ptr_curr));
 ctx.builder.call_far(@_jit_exit_proc); //TODO exit dispatcher
end;

procedure op_cpuid(var ctx:t_jit_context2);
begin
 op_set_r14_imm(ctx,Int64(ctx.ptr_curr));
 ctx.builder.call_far(@jit_cpuid);
end;

procedure op_rdtsc(var ctx:t_jit_context2);
begin
 if time.strict_ps4_freq then
 begin
  ctx.builder.call_far(@strict_ps4_rdtsc_jit);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_rdtscp(var ctx:t_jit_context2);
begin
 if time.strict_ps4_freq then
 begin
  ctx.builder.call_far(@strict_ps4_rdtscp_jit);
 end else
 with ctx.builder do
 begin
  //rdx //result0
  //rax //result1
  //rcx //result3
  //rbx //backup -> CPUID_LOCAL_APIC_ID 0xff000000 0..7

  movq(r_tmp0,rbx); //save rbx

  movi(eax,1);
  _O($0FA2);    //cpuid

  //load flags to al,ah
  laxf;

  shri8  (ebx,6); //cpu_id
  andi8se(ebx,7); //0..7

  movi   (ecx,7);
  subq   (ecx,ebx); //7-cpu_id

  //store flags from al,ah
  saxf;

  movq(rbx,r_tmp0); //restore rbx

  _O($0FAEE8); //lfence
  _O($0F31);   //rdtsc
  _O($0FAEE8); //lfence
 end;
end;

procedure op_nop(var ctx:t_jit_context2);
begin
 //align?
end;

procedure op_invalid(var ctx:t_jit_context2);
begin
 op_ud2(ctx);
 ctx.trim:=True; //force
end;

{
 //load flags to al,ah
 seto(al);
 lahf;

 //store flags from al,ah
 addi(al,127);
 sahf;
}

//
procedure op_debug_info(var ctx:t_jit_context2);
var
 link_jmp:t_jit_i_link;
begin
 //debug
 if debug_info then
 begin
  link_jmp:=ctx.builder.jmp(nil_link,os8);
  //
  //ctx.builder.int3;
  ctx.builder.cli;
  //op_set_r14_imm(ctx,$FACEADD7);
  op_set_r14_imm(ctx,Int64(ctx.ptr_curr));
  add_orig(ctx);
  op_set_r14_imm(ctx,Int64(ctx.ptr_next));
  //op_set_r14_imm(ctx,$FACEADDE);
  ctx.builder.sti;
  //
  link_jmp.target:=ctx.builder.get_curr_label.after;
 end;
 //debug
end;

type
 t_convert_mode=(mInstruction,mExport,mImport);

procedure op_jit2native(var ctx:t_jit_context2;mode:t_convert_mode);
var
 i:Integer;
begin
 with ctx.builder do
 begin

  case mode of
   mExport,
   mImport:
    begin
     //set PCB_IS_HLE
     ori([r13-jit_frame_offset+(@p_kthread(nil)^.pcb_flags),os8],Byte(PCB_IS_HLE));
    end;
   else;
  end;

  case mode of
   mInstruction:
    begin
     //save internal stack
     movq([r13-jit_frame_offset+(@p_kthread(nil)^.td_jctx.rsp)],rsp);
     movq([r13-jit_frame_offset+(@p_kthread(nil)^.td_jctx.rbp)],rbp);

     //load guest stack
     movq(r14,[r13-jit_frame_offset+(@p_kthread(nil)^.td_ustack.stack)]);
     movq(r15,[r13-jit_frame_offset+(@p_kthread(nil)^.td_ustack.sttop)]);

     //set teb
     movq([GS+teb_stack],r14);
     movq([GS+teb_sttop],r15);

     //load rsp,rbp
     movq(rsp,[r13+(@p_jit_frame(nil)^.tf_rsp)]);
     movq(rbp,[r13+(@p_jit_frame(nil)^.tf_rbp)]);
     //
    end;
   mExport:
    begin
     //load guest stack

     //pushq %rbp
     //////////push([r13+(@p_jit_frame(nil)^.tf_rbp),os64]);

     //movq  %rsp,%rbp
     movq(r14,[r13+(@p_jit_frame(nil)^.tf_rsp)]); //<-rsp
     //////////movq([r13+(@p_jit_frame(nil)^.tf_rbp)],r14); //->rbp

     //prolog (debugger)
     push(rbp);
     movq(rbp,rsp);

     leaq(rsp,[rsp-$8]); //shift guard

     //alloc host stack
     leaq(rsp,[rsp-$50]);

     //preload stack argc

     //$50 = 10*8
     For i:=0 to 7 do
     begin
      movq(r15,[r14+i*8+(8)]); //call(8)
      movq([rsp+i*8],r15);
     end;
     //
    end;
   mImport:
    begin
     //restore guest/host stack

     //movq  %rbp,%rsp
     movq(r14,[r13+(@p_jit_frame(nil)^.tf_rbp)]); //<-rbp
     movq([r13+(@p_jit_frame(nil)^.tf_rsp)],r14); //->rsp

     //popq  %rbp
     pop([r13+(@p_jit_frame(nil)^.tf_rbp),os64]);
     //
    end;
   else;
  end;

  //load r14,r15,r13
  movq(r14,[r13+(@p_jit_frame(nil)^.tf_r14)]);
  movq(r15,[r13+(@p_jit_frame(nil)^.tf_r15)]);
  movq(r13,[r13+(@p_jit_frame(nil)^.tf_r13)]);
 end;
end;

procedure op_native2jit(var ctx:t_jit_context2;mode:t_convert_mode);
var
 i:Integer;
begin
 with ctx.builder do
 begin

  //save r13
  movq([GS+teb_jitcall],r13);

  //load curkthread,jit_ctx
  movq(r13,[GS +teb_thread]);
  leaq(r13,[r13+jit_frame_offset   ]);

  //load r14,r15
  movq([r13+(@p_jit_frame(nil)^.tf_r14)],r14);
  movq([r13+(@p_jit_frame(nil)^.tf_r15)],r15);

  //load r13
  movq(r14,[GS+teb_jitcall]);
  movq([r13+(@p_jit_frame(nil)^.tf_r13)],r14);

  case mode of
   mInstruction:
    begin
     //load rsp,rbp
     movq([r13+(@p_jit_frame(nil)^.tf_rsp)],rsp);
     movq([r13+(@p_jit_frame(nil)^.tf_rbp)],rbp);

     //load host stack
     movq(r14,[r13-jit_frame_offset+(@p_kthread(nil)^.td_kstack.stack)]);
     movq(r15,[r13-jit_frame_offset+(@p_kthread(nil)^.td_kstack.sttop)]);

     //set teb
     movq([GS+teb_stack],r14);
     movq([GS+teb_sttop],r15);

     //load internal stack
     movq(rsp,[r13-jit_frame_offset+(@p_kthread(nil)^.td_jctx.rsp)]);
     movq(rbp,[r13-jit_frame_offset+(@p_kthread(nil)^.td_jctx.rbp)]);
     //
    end;
   mExport:
    begin
     //free host stack

     //epilog (debugger)
     movq(rsp,rbp);
     pop (rbp);

     //restore guest/host stack

     //movq  %rbp,%rsp
     //////////movq(r14,[r13+(@p_jit_frame(nil)^.tf_rbp)]); //<-rbp
     //////////movq([r13+(@p_jit_frame(nil)^.tf_rsp)],r14); //->rsp

     //popq  %rbp
     //////////pop([r13+(@p_jit_frame(nil)^.tf_rbp),os64]);
     //
    end;
   mImport:
    begin
     //load guest stack

     //pushq %rbp
     push([r13+(@p_jit_frame(nil)^.tf_rbp),os64]);

     //movq  %rsp,%rbp
     movq(r14,[r13+(@p_jit_frame(nil)^.tf_rsp)]); //<-rsp
     movq([r13+(@p_jit_frame(nil)^.tf_rbp)],r14); //->rbp

     leaq(r14,[r14-$8]); //shift guard

     //alloc guest rsp
     leaq(r14,[r14-$50]);
     movq([r13+(@p_jit_frame(nil)^.tf_rsp)],r14); //rsp

     //preload stack argc

     //$50 = 10*8
     For i:=0 to 7 do
     begin
      movq(r15,[rsp+i*8+(8+8)]); //call(8) + push(8)
      movq([r14+i*8],r15);
     end;
     //
    end;
   else;
  end;

  case mode of
   mExport,
   mImport:
    begin
     //reset PCB_IS_HLE
     andi([r13-jit_frame_offset+(@p_kthread(nil)^.pcb_flags),os8],not Byte(PCB_IS_HLE));
     //
    end;
   else;
  end;

 end;
end;

//rdi -> addr
//rsi -> rdi
//rdx -> rsi
//rcx -> rdx
//r8  -> rcx
//r9  -> r8

// EXECUTE_MAGIC_ADDR $FFFFFFFFFCAFEDAD
// $0xFCAFEDAD = -55579219

procedure ExecuteGuest; SysV_ABI_CDecl; assembler; nostackframe; public;
asm
 //save r13
 movq %r13,%gs:teb.jitcall

 //load curkthread,jit_ctx
 movq %gs:teb.thread               ,%r13 //curkthread
 leaq kthread.td_frame.tf_r13(%r13),%r13 //jit_frame

 //load r14,r15
 movq %r14,jit_frame.tf_r14(%r13)
 movq %r15,jit_frame.tf_r15(%r13)

 //load r13
 movq %gs:teb.jitcall,%r14
 movq %r14,jit_frame.tf_r13(%r13)

 //alloc rbp,rsp

 //load addr
 movq %rdi,%r14

 //move params
 movq %rsi,%rdi
 movq %rdx,%rsi
 movq %rcx,%rdx
 movq %r8 ,%rcx
 movq %r9 ,%r8

 //call
 xorq %r15,%r15
 call jit_jmp_dispatch //in:r14(addr) r15(plt) out:r14(addr)

 movq jit_frame.tf_rsp(%r13), %r15 //mov r15,rsp
 leaq -8(%r15), %r15               //lea r15,[r15-8]

 movq $-55579219,(%r15)            //mov [r15],magic (sign extend)

 movq %r15, jit_frame.tf_rsp(%r13) //mov rsp,r15

 call %r14

 //restore rbp,rsp

 //load r14,r15,r13
 movq jit_frame.tf_r14(%r13),%r14
 movq jit_frame.tf_r15(%r13),%r15
 movq jit_frame.tf_r13(%r13),%r13
end;

function is_push_op(Opcode:TOpcode):Boolean; inline;
begin
 case Opcode of
  OPpush,
  OPpop,
  OPpushf,
  OPpopf:
   Result:=True;
  else
   Result:=False;
 end;
end;

const
 use_lazy_jit=False;

function op_lazy_jit(var ctx:t_jit_context2):Boolean;
begin
 Result:=False;

 if not use_lazy_jit then
 begin
  Exit;
 end;

 if (jit_cbs[ctx.din.OpCode.Prefix,ctx.din.OpCode.Opcode,ctx.din.OpCode.Suffix]=@op_invalid) then
 begin
  Exit;
 end;

 case ctx.din.OpCode.Opcode of
  OPcall,
  OPjmp,
  OPret,
  OPretf,
  OPj__,
  OPloop,
  OPjcxz,
  OPjecxz,
  OPjrcxz,
  //OPpush,
  //OPpop,
  //OPpushf,
  //OPpopf,
  OPenter,
  OPleave,
  OPsyscall,
  OPint,
  OPint1,
  OPint3,
  OPud1,
  OPud2,
  OPiret,
  OPhlt,
  OPcpuid,
  OPrdtsc,
  OPnop  :Exit;
  else;
 end;

 if is_rep_prefix(ctx.din) then
 begin
  Exit;
 end;

 if is_segment(ctx.din) then
 begin
  Exit;
 end;

 if is_push_op(ctx.din.OpCode.Opcode) or is_preserved(ctx.din) then
 begin
  if is_rip(ctx.din) then
  begin
   Exit;
  end;
 end else
 begin
  add_orig(ctx);
  Exit(True);
 end;

 op_jit2native(ctx,mInstruction);

 add_orig(ctx);

 op_native2jit(ctx,mInstruction);

 Result:=True;
end;

procedure init_cbs;
begin

 //

 jit_rep_cbs[repOPins ]:=@op_invalid;
 jit_rep_cbs[repOPouts]:=@op_invalid;
 jit_rep_cbs[repOPret ]:=@op_ret;

 //

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

 jit_cbs[OPPnone,OPloop,OPSnone]:=@op_loop;
 jit_cbs[OPPnone,OPloop,OPSc_ne]:=@op_loop;
 jit_cbs[OPPnone,OPloop,OPSc_e ]:=@op_loop;

 jit_cbs[OPPnone,OPjcxz ,OPSnone]:=@op_jcxz;
 jit_cbs[OPPnone,OPjecxz,OPSnone]:=@op_jcxz;
 jit_cbs[OPPnone,OPjrcxz,OPSnone]:=@op_jcxz;

 jit_cbs[OPPnone,OPpush,OPSnone]:=@op_push;
 jit_cbs[OPPnone,OPpop ,OPSnone]:=@op_pop;

 jit_cbs[OPPnone,OPpushf ,OPSnone]:=@op_pushf;
 jit_cbs[OPPnone,OPpushf ,OPSx_q ]:=@op_pushf;

 jit_cbs[OPPnone,OPenter ,OPSnone]:=@op_invalid; //TODO
 jit_cbs[OPPnone,OPleave ,OPSnone]:=@op_leave;

 jit_cbs[OPPnone,OPpopf  ,OPSnone]:=@op_popf;
 jit_cbs[OPPnone,OPpopf  ,OPSx_q ]:=@op_popf;

 jit_cbs[OPPnone,OPsyscall,OPSnone]:=@op_syscall;
 jit_cbs[OPPnone,OPint    ,OPSnone]:=@op_int;
 jit_cbs[OPPnone,OPint1   ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPint3   ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPud1    ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPud2    ,OPSnone]:=@op_ud2;

 jit_cbs[OPPnone,OPiret,OPSnone]:=@op_iretq;
 jit_cbs[OPPnone,OPiret,OPSx_d ]:=@op_iretq;
 jit_cbs[OPPnone,OPiret,OPSx_q ]:=@op_iretq;

 jit_cbs[OPPnone,OPhlt ,OPSnone]:=@op_hlt;

 jit_cbs[OPPnone,OPcpuid,OPSnone]:=@op_cpuid;
 jit_cbs[OPPnone,OPrdtsc,OPSnone]:=@op_rdtsc;
 jit_cbs[OPPnone,OPrdtsc,OPSx_p ]:=@op_rdtscp;

 jit_cbs[OPPnone,OPnop,OPSnone]:=@op_nop;

 jit_cbs[OPPnone,OPin  ,OPSnone]:=@op_invalid;
 jit_cbs[OPPnone,OPins ,OPSx_b ]:=@op_invalid;
 jit_cbs[OPPnone,OPins ,OPSx_w ]:=@op_invalid;
 jit_cbs[OPPnone,OPins ,OPSx_d ]:=@op_invalid;

 jit_cbs[OPPnone,OPout ,OPSnone]:=@op_invalid;
 jit_cbs[OPPnone,OPouts,OPSx_b ]:=@op_invalid;
 jit_cbs[OPPnone,OPouts,OPSx_w ]:=@op_invalid;
 jit_cbs[OPPnone,OPouts,OPSx_d ]:=@op_invalid;

 jit_cbs[OPPnone,OPrdmsr,OPSnone]:=@op_invalid;
 jit_cbs[OPPnone,OPwrmsr,OPSnone]:=@op_invalid;

 jit_cbs[OPPnone,OPsldt,OPSnone]:=@op_invalid;
 jit_cbs[OPPnone,OPlldt,OPSnone]:=@op_invalid;

 jit_cbs[OPPnone,OPxbegin,OPSnone]:=@op_invalid;
 jit_cbs[OPPnone,OPxend  ,OPSnone]:=@op_invalid;

 jit_cbs[OPPnone,OPwbinvd,OPSnone]:=@op_invalid;

 jit_cbs[OPPnone,OPstr,OPSnone]:=@op_invalid;

 jit_cbs[OPPnone,OPbndldx,OPSnone]:=@op_invalid;
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
     (adec.Instr.ParseFlags * [preF3,preF2] <> []) or
     is_invalid(adec.Instr) then
  begin
   Result:=False;
   Break;
  end;

 end;

 adec.Free;
 proc.Free;
end;

function pick_on_destroy(handle:Pointer):Integer;
begin
 Result:=DO_NOTHING;

 Assert(false,'TODO: destroy in code analize');
end;

function pick_on_trigger(handle:Pointer;mode:T_TRIGGER_MODE):Integer;
begin
 case mode of
  M_CPU_WRITE :;
  M_DMEM_WRITE:;
  else
   Exit;
 end;

 Result:=DO_NOTHING;

 Assert(false,'TODO: trigger in code analize');
end;

function  pick_locked_internal(var ctx:t_jit_context2):p_jit_dynamic_blob; forward;
function  pick_locked_normal  (var ctx:t_jit_context2):p_jit_dynamic_blob; forward;

procedure pick(var ctx:t_jit_context2;preload:Pointer); [public, alias:'kern_jit_pick'];
label
 _exit;
var
 map:vm_map_t;
 lock:Pointer;
 node:p_jit_entry_point;

 lock_start:QWORD;
 lock___end:QWORD;

 tobj:p_vm_track_object;

 blob:p_jit_dynamic_blob;
begin
 map:=p_proc.p_vmspace;

 lock_start:=ctx.text_start;
 lock___end:=ctx.text___end;

 lock:=pmap_wlock(map^.pmap,lock_start,lock___end);

  if (preload<>nil) then
  begin
   //recheck
   node:=preload_entry(preload);
   if (node<>nil) then
   begin
    node^.dec_ref('preload_entry');
    goto _exit;
   end;
  end;

  //lock pageflt read-only  (mirrors?)

  //TODO: Works slowly, needs optimization
  {
  tobj:=vm_track_object_allocate(node,lock_start,lock___end,H_ZERO,PAGE_TRACK_W);
  tobj^.on_destroy:=@pick_on_destroy;
  tobj^.on_trigger:=@pick_on_trigger;

  vm_map_track_insert(p_proc.p_vmspace,tobj);
  }

  if (cmInternal in ctx.modes) then
  begin
   blob:=pick_locked_internal(ctx);
  end else
  begin
   blob:=pick_locked_normal(ctx);
  end;

  if (blob<>nil) then
  begin
   blob^.attach; //blob.attach-> blob_track-> vm_map_track_insert
  end;

  //restore non tracked  (mirrors?)

  //TODO: Works slowly, needs optimization
  {
  vm_map_track_remove(p_proc.p_vmspace,tobj);

  tobj^.on_destroy:=nil;
  tobj^.on_trigger:=nil;

  vm_track_object_deallocate(tobj); //<-vm_track_object_allocate
  }

 _exit:

 pmap_unlock(map^.pmap,lock);
end;

procedure op_debug_info_addr(var ctx:t_jit_context2;addr:Pointer);
var
 link_jmp:t_jit_i_link;
begin
 //debug
 if debug_info then
 begin
  link_jmp:=ctx.builder.jmp(nil_link,os8);
  //
  ctx.builder.cli;
  op_set_r14_imm(ctx,Int64(addr));
  ctx.builder.sti;
  //
  link_jmp.target:=ctx.builder.get_curr_label.after;
 end;
 //debug
end;

const
 trace_hle_call=False;

procedure print_beg_hle(nid:QWORD); SysV_ABI_CDecl;
var
 str:shortstring;
begin
 str:=ps4libdoc.GetFunctName(nid);
 if (str='Unknow') then
 begin
  str:='0x'+HexStr(nid,16);
 end;

 if Pos('sceAudioOut',str)<>0 then Exit;

 case str of
  'sceNpCheckCallback'                :Exit;
  'sceNetCtlCheckCallback'            :Exit;
  'sceUserServiceGetEvent'            :Exit;
  'sceNpCheckCallbackForLib'          :Exit;
  'sceNetCtlCheckCallbackForNpToolkit':Exit;
  'sceSystemServiceGetStatus'         :Exit;
 end;

 Writeln(str,'->');
end;

procedure print_end_hle(nid:QWORD); SysV_ABI_CDecl;
var
 str:shortstring;
begin
 str:=ps4libdoc.GetFunctName(nid);
 if (str='Unknow') then
 begin
  str:='0x'+HexStr(nid,16);
 end;

 if Pos('sceAudioOut',str)<>0 then Exit;

 case str of
  'sceNpCheckCallback'                :Exit;
  'sceNetCtlCheckCallback'            :Exit;
  'sceUserServiceGetEvent'            :Exit;
  'sceNpCheckCallbackForLib'          :Exit;
  'sceNetCtlCheckCallbackForNpToolkit':Exit;
  'sceSystemServiceGetStatus'         :Exit;
 end;

 Writeln(str,'<-');
end;

function pick_locked_internal(var ctx:t_jit_context2):p_jit_dynamic_blob;
var
 node_export:t_jit_context2.p_export_point;
 node_import:t_jit_context2.p_import_point;

 link_curr:t_jit_i_link;
 link_next:t_jit_i_link;
begin
 Result:=nil;

 node_export:=ctx.export_list;
 node_import:=ctx.import_list;

 if (node_export=nil) and (node_import=nil) then
 begin
  ctx.Free;
  Exit;
 end;

 ctx.ptr_curr:=Pointer(ctx.text_start);
 ctx.ptr_next:=ctx.ptr_curr;

 ctx.new_chunk(fpCall,ctx.ptr_curr);

 //export
 while (node_export<>nil) do
 begin
  ctx.ptr_curr:=ctx.ptr_next;
  ctx.ptr_next:=ctx.ptr_curr+8;

  if (ctx.ptr_curr>=Pointer(ctx.text___end)) then
  begin
   Assert(false,'pick_locked_internal');
  end;

  //
  link_curr:=ctx.builder.get_curr_label.after;
  //

  //beg
  if trace_hle_call then
  with ctx.builder do
  begin
   movi64(r14,node_export^.nid);
   movi64(r15,QWORD(@print_beg_hle));
   call_far(@jit_hle_trace);
  end;

  op_jit2native(ctx,mExport);
  //[JIT->HLE]

  ctx.builder.call_far(node_export^.native);

  op_debug_info_addr(ctx,node_export^.native);

  //[HLE->JIT]
  op_native2jit(ctx,mExport); //TODO: [HLE->JIT] combine with [ret]

  //end
  if trace_hle_call then
  with ctx.builder do
  begin
   movi64(r14,node_export^.nid);
   movi64(r15,QWORD(@print_end_hle));
   call_far(@jit_hle_trace);
  end;

  //save last call
  {
  if debug_info then
  with ctx.builder do
  begin
   ctx.builder.movi64(r14,QWORD(node_export^.native));
   ctx.builder.movq  ([GS+$100],r14);
  end;
  }

  //
  op_pop_rip_part0(ctx,0); //out:r14
  ctx.imm:=0;
  op_jmp_dispatcher(ctx,@op_pop_rip_part1);
  //

  //
  if (node_export^.dst<>nil) then
  begin
   node_export^.dst^:=ctx.ptr_curr;
  end;
  //

  //
  link_next:=ctx.builder.get_curr_label.after;

  ctx.add_label(ctx.ptr_curr,
                ctx.ptr_next,
                link_curr,
                link_next,
                LF_JMP);
  //
  ctx.add_entry_point(ctx.ptr_curr,link_curr);
  //

  //
  node_export:=node_export^.next;
 end;
 //export

 //import
 while (node_import<>nil) do
 begin
  ctx.ptr_curr:=ctx.ptr_next;
  ctx.ptr_next:=ctx.ptr_curr+8;

  if (ctx.ptr_curr>=Pointer(ctx.text___end)) then
  begin
   Assert(false,'pick_locked_internal');
  end;

  //
  link_curr:=ctx.builder.get_curr_label.after;
  //

  //[HLE->JIT]
  op_native2jit(ctx,mImport);

  with ctx.builder do
  begin
   op_push_rip_part0(ctx);

   op_set_r14_imm(ctx,Int64(node_import^.guest));
   movq(r14,[r14]);

   op_call_dispatcher(ctx,@op_push_rip_part1);
  end;

  //---RETURN ENTRY POINT----

  //
  link_next:=ctx.builder.get_curr_label.after;
  //
  ctx.add_label(ctx.ptr_curr,
                ctx.ptr_next,
                link_curr,
                link_next,
                LF_JMP);

  //
  ctx.add_entry_point(ctx.ptr_curr,link_curr);
  //

  //
  if (node_import^.dst<>nil) then
  begin
   node_import^.dst^:=ctx.ptr_curr;
  end;
  //

  //
  ctx.ptr_curr:=ctx.ptr_next;
  ctx.ptr_next:=ctx.ptr_curr+8;
  //

  //
  link_curr:=ctx.builder.get_curr_label.after;
  //

  //--EPILOG--

  op_jit2native(ctx,mImport);
  //[JIT->HLE]

  //
  ctx.builder.reta;
  //

  //
  link_next:=ctx.builder.get_curr_label.after;

  ctx.add_label(ctx.ptr_curr,
                ctx.ptr_next,
                link_curr,
                link_next,
                LF_JMP);
  //
  ctx.add_entry_point(ctx.ptr_curr,link_curr);
  //

  node_import:=node_import^.next;
 end;
 //import

 ctx.end_chunk(ctx.ptr_next);

 Result:=build(ctx);

 ctx.Free;
end;

function scan_step_switchtable(var  ctx:t_jit_context2;
                                sw_table:t_jit_context2.p_switchtable_point;
                                sw_next :PInteger):Boolean;
var
 sw_data:PInteger;
 ofs:Int64;
 rel:Integer;
begin
 Result:=False;
 Assert(sw_table^.curr<>nil);
 //
 sw_data:=sw_table^.curr;
 //
 if print_asm then
 begin
  Writeln('switchtable:0x',HexStr(QWORD(sw_table^.table),11),'..0x',HexStr(QWORD(sw_next),11));
 end;
 //
 if ctx.is_text_addr(QWORD(sw_data)) and
       (
        (sw_next=nil) or
        (QWORD(sw_data)<QWORD(sw_next))
       ) then
 begin

  rel:=0;
  if (copyin(sw_data,@rel,SizeOf(Integer))<>0) then
  begin
   Exit;
  end;

  if (DWORD(rel) and $FFFF0000)=$FFFF0000 then
  begin
   ofs:=Int64(sw_table^.table)+rel;

   if ctx.is_text_addr(ofs) and (ofs<=ctx.max_reloc) then
   begin
    if print_asm then
    begin
     Writeln(' [0x',HexStr(QWORD(sw_data),11),']->0x',HexStr(ofs,11));
    end;
    //
    ctx.add_forward_point(fpCall,Pointer(ofs));
    //step up
    sw_table^.curr:=sw_data+1; //SizeOf(Integer)
    //
    Result:=True;
   end else
   begin
    //Terminate
    sw_table^.curr:=nil;
   end;

  end else
  begin
   //Terminate
   sw_table^.curr:=nil;
  end;

 end;
end;

procedure on_builder_error(const Msg:RawByteString;userdata:Pointer); register;
var
 ctx:p_jit_context2;
begin
 ctx:=userdata;

 with ctx^ do
 begin
  Writeln('original------------------------':32,' ','0x',HexStr(ptr_curr));
  print_disassemble(code,dis.CodeIdx);
  Writeln('original------------------------':32,' ','0x',HexStr(ptr_next));

  Writeln('builder error:',
          din.OpCode.Prefix,',',
          din.OpCode.Opcode,',',
          din.OpCode.Suffix,' ',
          din.Operand[1].Size,' ',
          din.Operand[2].Size);

  Writeln('opcode=$',HexStr(dis.opcode,8),' ',
          'MIndex=',dis.ModRM.Index,' ',
          'SimdOp=',dis.SimdOpcode,':',SCODES[dis.SimdOpcode],' ',
          'mm=',MCODES[dis.mm and 3],':',dis.mm);

 end;

end;

var
 _print_stat:Integer=0;

 function pick_locked_normal(var ctx:t_jit_context2):p_jit_dynamic_blob;
label
 _next,
 _next_forward,
 _switchtables,
 _invalid;
var
 addr:Pointer;
 ptr :Pointer;

 sw_table:t_jit_context2.p_switchtable_point;
 sw_next :PInteger;

 links:t_jit_context2.t_forward_links;
 entry_link:Pointer;

 dis:TX86Disassembler;
 din:TInstruction;

 cb:t_jit_cb;

 link_new :t_jit_i_link;
 link_curr:t_jit_i_link;
 link_next:t_jit_i_link;

 node,node_curr,node_next:p_jit_instruction;

 i:Integer;
begin
 Result:=nil;

 //init error cbs
 with ctx.builder do
 begin
  on_error:=@on_builder_error;
  on_udata:=@ctx;
 end;

 if (cmDontScanRipRel in ctx.modes) then
 begin
  //dont scan rip relative
  ctx.max_reloc:=0;
 end else
 begin
  ctx.max_reloc:=QWORD(ctx.max_forward_point);
 end;

 if (p_print_jit_preload) then
 begin
  Writeln(' ctx.text_start:0x',HexStr(ctx.text_start,16));
  Writeln(' ctx.max_reloc :0x',HexStr(ctx.max_reloc ,16));
  Writeln(' ctx.text___end:0x',HexStr(ctx.text___end,16));
  Writeln(' ctx.map____end:0x',HexStr(ctx.map____end,16));
 end;

 if System.InterlockedExchange(_print_stat,1)=0 then
 begin
  print_test_jit_cbs(False,True);
 end;

 links:=Default(t_jit_context2.t_forward_links);
 addr:=nil;

 if not ctx.fetch_forward_point(links,addr) then
 begin
  //No entry points? early exit
  ctx.Free;
  Exit;
 end;

 ctx.trim:=False;

 entry_link:=addr;

 ctx.new_chunk(links.ptype,entry_link);

 ptr:=addr;

 dis:=Default(TX86Disassembler);
 din:=Default(TInstruction);

 while True do
 begin

  if not ctx.is_text_addr(QWORD(ptr)) then
  begin
   if (p_print_jit_preload) then
   begin
    writeln('not excec:0x',HexStr(ptr));
   end;
   ctx.ptr_curr:=ptr;
   goto _invalid;
  end;

  if ((ppmap_get_prot(QWORD(ptr)) and PAGE_PROT_EXECUTE)=0) then
  begin
   if (p_print_jit_preload) then
   begin
    writeln('not excec:0x',HexStr(ptr));
   end;
   ctx.ptr_curr:=ptr;
   goto _invalid;
  end;

  ctx.label_flags:=0;

  ctx.ptr_curr:=ptr;

  //pre check
  if exist_entry(ptr) then
  begin
   if (entry_link=ptr) then
   begin
    entry_link:=nil; //clear
   end;

   link_curr:=ctx.builder.get_curr_label.after;
   node_curr:=link_curr._node;

   op_set_r14_imm(ctx,Int64(ptr));
   //
   op_jmp_dispatcher(ctx,nil);

   link_next:=ctx.builder.get_curr_label.after;
   node_next:=link_next._node;

   cb:=@op_invalid;
   ctx.trim:=True;
   goto _next; //trim
  end;

  //guest->host ptr
  ctx.code:=uplift(ptr);
  ptr:=ctx.code;

  dis.Disassemble(dm64,ptr,din);

  apply_din_stat(din,(ptr-ctx.code));

  ctx.ptr_next:=ctx.ptr_curr+(ptr-ctx.code);

  case din.OpCode.Opcode of
   OPX_Invalid..OPX_GroupP:
    begin
     //invalid
     if (p_print_jit_preload) then
     begin
      writeln('invalid1:0x',HexStr(ctx.ptr_curr));
     end;

     _invalid:

     if (p_print_jit_preload) then
     begin
      print_frame(stdout,ctx.ptr_curr);
      Writeln('original------------------------':32,' ','0x',HexStr(ctx.ptr_curr));
      print_disassemble(ctx.code,dis.CodeIdx);
      Writeln('original------------------------':32,' ','0x',HexStr(ctx.ptr_next));
     end;

     ctx.mark_chunk(fpInvalid);

     link_curr:=ctx.builder.get_curr_label.after;
     node_curr:=link_curr._node;

     ctx.builder.int3;
     ctx.builder.int3;
     ctx.builder.ud2;

     if debug_info then
     begin
      op_set_r14_imm(ctx,Int64(ctx.ptr_curr));
     end;

     link_next:=ctx.builder.get_curr_label.after;
     node_next:=link_next._node;

     cb:=@op_invalid;
     ctx.trim:=True;
     goto _next; //trim
    end;
   else;
  end;

  if (din.Flags * [ifOnly32, ifOnly64, ifOnlyVex] <> []) or
     (din.ParseFlags * [preF3,preF2] <> []) or
     is_invalid(din) then
  begin
   if (p_print_jit_preload) then
   begin
    writeln('invalid2:0x',HexStr(ctx.ptr_curr));
   end;
   goto _invalid;
  end;

  if print_asm then
  begin
   Writeln('original------------------------':32,' ','0x',HexStr(ctx.ptr_curr));
   print_disassemble(ctx.code,dis.CodeIdx);
   Writeln('original------------------------':32,' ','0x',HexStr(ctx.ptr_next));
  end;

  ctx.dis:=dis;
  ctx.din:=din;

  if is_rep_prefix(ctx.din) then
  begin
   cb:=@op_invalid;
   if (ctx.din.OpCode.Prefix=OPPnone) then
   begin
    case ctx.din.OpCode.Opcode of
     OPins :cb:=jit_rep_cbs[repOPins ];
     OPouts:cb:=jit_rep_cbs[repOPouts];
     OPmovs:cb:=jit_rep_cbs[repOPmovs];
     OPlods:cb:=jit_rep_cbs[repOPlods];
     OPstos:cb:=jit_rep_cbs[repOPstos];
     OPcmps:cb:=jit_rep_cbs[repOPcmps];
     OPscas:cb:=jit_rep_cbs[repOPscas];
     OPret :cb:=jit_rep_cbs[repOPret ];
     else;
    end;
   end;
  end else
  begin
   cb:=jit_cbs[ctx.din.OpCode.Prefix,ctx.din.OpCode.Opcode,ctx.din.OpCode.Suffix];
  end;

  if (cb=@op_invalid) then
  begin
   case ctx.get_chunk_ptype of
    fpData,
    fpInvalid:
     begin
      writeln('skip:0x',HexStr(ctx.ptr_curr));
      goto _invalid;
     end
    else;
   end;
  end;

  if (cb=nil) then
  begin
   print_error_td('Unhandled jit opcode!');

   with ctx do
   begin
    Writeln('original------------------------':32,' ','0x',HexStr(ptr_curr));
    print_disassemble(code,dis.CodeIdx);
    Writeln('original------------------------':32,' ','0x',HexStr(ptr_next));

    Writeln('Unhandled jit:',
            din.OpCode.Prefix,',',
            din.OpCode.Opcode,',',
            din.OpCode.Suffix,' ',
            din.Operand[1].Size,' ',
            din.Operand[2].Size);

    Writeln('opcode=$',HexStr(dis.opcode,8),' ',
            'MIndex=',dis.ModRM.Index,' ',
            'SimdOp=',dis.SimdOpcode,':',SCODES[dis.SimdOpcode],' ',
            'mm=',MCODES[dis.mm and 3],':',dis.mm);
   end;

   Assert(false);
  end;

  link_curr:=ctx.builder.get_curr_label.after;
  node_curr:=link_curr._node;

  {
  op_set_r14_imm(ctx,Int64(ctx.ptr_curr));
  with ctx.builder do
   movq([GS+teb_jitcall],r14);
  }

  {
  if (qword(ctx.ptr_curr) and $FFFFFF) = $2f662e then
  begin
   //print_asm:=true;
   ctx.builder.int3;
  end;
  }

  if op_lazy_jit(ctx) then
  begin
   //
  end else
  begin
   cb(ctx);
  end;

  {
  The main idea of interrupting JIT code:

   If the LF_JMP flag is set for the label,
    then it is enough to set the value to %gs:teb.jit_trp.
   Otherwise, for each thread, generate
    an individual lacuna with JIT code,
    containing:
     instructions to the end of the current guest instruction (start...rip...t_jinstr_len.recompil)
     then jmp_dispatcher to return to the code.
   If necessary, you can generate a page lock
    recovery after the current command:
    current instruction
    then jmp_dispatcher
  }

  //op_jit_interrupt(ctx);

  //cb(ctx);

  link_next:=ctx.builder.get_curr_label.after;
  node_next:=link_next._node;

  //////
  i:=0;
  if (node_curr<>node_next) and
     (node_curr<>nil) then
  begin
   node:=TAILQ_NEXT(node_curr,@node_curr^.entry);

   while (node<>nil) do
   begin

    i:=i+node^.AInstructionSize;

    {
    if not test_disassemble(@node^.AData,node^.ASize) then
    begin
     print_asm:=True;
     Break;
    end;
    }


    node:=TAILQ_NEXT(node,@node^.entry);
   end;
  end;

  apply_jit_stat(i);
  //////

  {
  if print_asm then
  begin
   Writeln('original------------------------':32,' ','0x',HexStr(ctx.ptr_curr));
   print_disassemble(ctx.code,dis.CodeIdx);
   Writeln('original------------------------':32,' ','0x',HexStr(ctx.ptr_next));
  end;
  }

  //debug print
  if print_asm then
  if (node_curr<>node_next) and
     (node_curr<>nil) then
  begin
   node:=TAILQ_NEXT(node_curr,@node_curr^.entry);

   Writeln('recompiled----------------------':32,' ','');
   while (node<>nil) do
   begin

    print_disassemble(@node^.AData,node^.AInstructionSize);


    node:=TAILQ_NEXT(node,@node^.entry);
   end;
   Writeln('recompiled----------------------':32,' ','');
  end;

  //print_asm:=False;

  _next:

  //debug
  if (cb<>@op_invalid) then
  begin
   op_debug_info(ctx);
  end;
  //debug

  //resolve forward links
  if (links.root<>nil) then
  begin
   ctx.Resolve_forwards(links,link_curr);
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
   link_new:=ctx.get_link(ctx.ptr_next);

   if (link_new<>nil_link) then
   begin
    ctx.builder.jmp(link_new);
    //Writeln('jmp next:0x',HexStr(ptr));
    ctx.trim:=True;
   end;
  end;

  //entry exist in another blob
  if not ctx.trim then
  if exist_entry(ctx.ptr_next) then
  begin
   op_set_r14_imm(ctx,Int64(ctx.ptr_next));
   //
   op_jmp_dispatcher(ctx,nil);
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
                 link_next,
                 ctx.label_flags);

   ctx.label_flags:=0;
  end;

  if ctx.trim then
  begin

   //close chunk
   ctx.end_chunk(ctx.ptr_next);

   _next_forward:

   ctx.trim:=False;

   repeat

    if not ctx.fetch_forward_point(links,addr) then
    begin
     goto _switchtables;
    end;

    link_new:=ctx.get_link(addr);
    if (link_new=nil_link) then
    begin
     //Writeln('not found:0x',HexStr(addr));
     Break;
    end else
    begin
     ctx.Resolve_forwards(links,link_new);
     //
     ctx.add_entry_point(addr,link_new);
    end;

   until false;

   entry_link:=addr;

   ctx.new_chunk(links.ptype,entry_link);

   ptr:=addr;
  end;

 end;

 _switchtables:

 //scan switchtables
 sw_table:=nil;
 sw_next :=nil;
 if ctx.fetch_switchtable(sw_table,sw_next) then
 begin
  //
  repeat
   if scan_step_switchtable(ctx,sw_table,sw_next) then
   begin
    goto _next_forward;
   end;
  until not ctx.fetch_switchtable(sw_table,sw_next);
  //
 end;

 //build blob

 ctx.builder.int3;
 ctx.builder.int3;
 ctx.builder.int3;
 ctx.builder.ud2;

 if debug_info then
 begin
  op_set_r14_imm(ctx,Int64(ctx.ptr_curr));
 end;

 Result:=build(ctx);

 ctx.Free;

 //print_din_stats;
end;

initialization
 init_cbs;


end.


