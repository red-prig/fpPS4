unit kern_jit_ops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 x86_fpdbgdisas,
 x86_jit,
 kern_jit_ctx;

type
 t_jit_cb=procedure(var ctx:t_jit_context2);

 t_rep_op=(repOPins,repOPouts,repOPmovs,repOPlods,repOPstos,repOPcmps,repOPscas,repOPret);

var
 jit_cbs:array[TOpcodePrefix,TOpCode,TOpCodeSuffix] of t_jit_cb;
 jit_rep_cbs:array[t_rep_op] of t_jit_cb;

implementation

//

procedure _op_rep_cmps(var ctx:t_jit_context2;dflag:Integer);
var
 size:TOperandSize;

 link_start:t_jit_i_link;
 link___end:t_jit_i_link;

 link_jmp0:t_jit_i_link;
 link_jmp1:t_jit_i_link;
begin
 //rdi,rsi

 Assert(ctx.dis.AddressSize=as64,'prefix $67 TODO');

 case ctx.din.OpCode.Suffix of
  OPSx_b:size:=os8;
  OPSx_w:size:=os16;
  OPSx_d:size:=os32;
  OPSx_q:size:=os64;
  else;
   Assert(False);
 end;

 //(r_tmp0)r14 <-> rdi
 //(r_tmp1)r15 <-> rsi
 with ctx.builder do
 begin

  link_jmp0:=nil_link;
  link_jmp1:=nil_link;

  link_start:=ctx.builder.get_curr_label.after;

  //repeat
   link_jmp0:=jcxz(nil_link,ctx.dis.AddressSize);

   movq(r_tmp0,rdi);
   op_uplift(ctx,r_tmp0,size); //in/out:r14

   movq(r_tmp1,rsi);
   op_uplift(ctx,r_tmp1,size,[not_use_r_tmp0]); //in/out:r15

   xchgq(rdi,r_tmp0);
   xchgq(rsi,r_tmp1);

   _O($A7,Size);

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

  //exit

  link___end:=ctx.builder.get_curr_label.after; //exit

  link_jmp0._label:=link___end;
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
  bti8([rsp,os64],10); //bt [rsp], 10

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

 Assert(ctx.dis.AddressSize=as64,'prefix $67 TODO');

 case ctx.din.OpCode.Suffix of
  OPSx_b:size:=os8;
  OPSx_w:size:=os16;
  OPSx_d:size:=os32;
  OPSx_q:size:=os64;
  else;
   Assert(False);
 end;

 //(r_tmp0)r14 <-> rdi
 with ctx.builder do
 begin

  link_jmp0:=nil_link;

  new:=new_reg_size(rax,size);

  link_start:=ctx.builder.get_curr_label.after;

  //repeat
   link_jmp0:=jcxz(nil_link,ctx.dis.AddressSize);

   movq(r_tmp0,rdi);
   op_uplift(ctx,r_tmp0,size); //in/out:r14

   movq([r_tmp0],new); // mov [r14],rax

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

  //exit

  link___end:=ctx.builder.get_curr_label.after; //exit

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
  bti8([rsp,os64],10); //bt [rsp], 10

  link_jmp0:=jcc(OPSc_b,nil_link,os8);

  _op_rep_stos(ctx,0);

  link_jmp1:=jmp(nil_link,os8);

  link_jmp0._label:=ctx.builder.get_curr_label.after;

  _op_rep_stos(ctx,1);

  link_jmp1._label:=ctx.builder.get_curr_label.after;

  popfq(os64);

 end;
end;

//

procedure _op_rep_movs(var ctx:t_jit_context2;dflag:Integer);
var
 size:TOperandSize;

 link_start:t_jit_i_link;
 link___end:t_jit_i_link;

 link_jmp0:t_jit_i_link;
begin
 //rdi,rsi

 Assert(ctx.dis.AddressSize=as64,'prefix $67 TODO');

 case ctx.din.OpCode.Suffix of
  OPSx_b:size:=os8;
  OPSx_w:size:=os16;
  OPSx_d:size:=os32;
  OPSx_q:size:=os64;
  else;
   Assert(False);
 end;

 //(r_tmp0)r14 <-> rdi
 //(r_tmp1)r15 <-> rsi
 with ctx.builder do
 begin

  link_jmp0:=nil_link;

  link_start:=ctx.builder.get_curr_label.after;

  //repeat
   link_jmp0:=jcxz(nil_link,ctx.dis.AddressSize);

   movq(r_tmp0,rdi);
   op_uplift(ctx,r_tmp0,size); //in/out:r14

   movq(r_tmp1,rsi);
   op_uplift(ctx,r_tmp1,size,[not_use_r_tmp0]); //in/out:r15

   //[RSI] -> [RDI].

   xchgq(rdi,r_tmp0);
   xchgq(rsi,r_tmp1);

   _O($A5,Size);

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

  //until
  jmp(link_start,os8);

  //exit

  link___end:=ctx.builder.get_curr_label.after; //exit

  link_jmp0._label:=link___end;
 end;

end;

procedure op_rep_movs(var ctx:t_jit_context2);
var
 link_jmp0:t_jit_i_link;
 link_jmp1:t_jit_i_link;
begin
 with ctx.builder do
 begin

  //get d flag
  pushfq(os64);
  bti8([rsp,os64],10); //bt [rsp], 10

  link_jmp0:=jcc(OPSc_b,nil_link,os8);

  _op_rep_movs(ctx,0);

  link_jmp1:=jmp(nil_link,os8);

  link_jmp0._label:=ctx.builder.get_curr_label.after;

  _op_rep_movs(ctx,1);

  link_jmp1._label:=ctx.builder.get_curr_label.after;

  popfq(os64);

 end;
end;


//

procedure _op_cmps(var ctx:t_jit_context2;dflag:Integer);
var
 size:TOperandSize;
begin
 //rdi,rsi

 Assert(ctx.dis.AddressSize=as64,'prefix $67 TODO');

 case ctx.din.OpCode.Suffix of
  OPSx_b:size:=os8;
  OPSx_w:size:=os16;
  OPSx_d:size:=os32;
  OPSx_q:size:=os64;
  else;
   Assert(False);
 end;

 //(r_tmp0)r14 <-> rdi
 //(r_tmp1)r15 <-> rsi
 with ctx.builder do
 begin

   movq(r_tmp0,rdi);
   op_uplift(ctx,r_tmp0,size); //in/out:r14

   movq(r_tmp1,rsi);
   op_uplift(ctx,r_tmp1,size,[not_use_r_tmp0]); //in/out:r15

   xchgq(rdi,r_tmp0);
   xchgq(rsi,r_tmp1);

   _O($A7,Size);

   xchgq(rdi,r_tmp0);
   xchgq(rsi,r_tmp1);

   if (dflag=0) then
   begin
    leaq(rdi,[rdi+OPERAND_BYTES[size]]);
    leaq(rsi,[rsi+OPERAND_BYTES[size]]);
   end else
   begin
    leaq(rdi,[rdi-OPERAND_BYTES[size]]);
    leaq(rsi,[rsi-OPERAND_BYTES[size]]);
   end;

 end;

end;

procedure op_cmps(var ctx:t_jit_context2);
var
 link_jmp0:t_jit_i_link;
 link_jmp1:t_jit_i_link;
begin
 with ctx.builder do
 begin

  //get d flag
  pushfq(os64);
  bti8([rsp,os64],10); //bt [rsp], 10

  link_jmp0:=jcc(OPSc_b,nil_link,os8);

  popfq(os64);
  _op_cmps(ctx,0);

  link_jmp1:=jmp(nil_link,os8);

  link_jmp0._label:=ctx.builder.get_curr_label.after;

  popfq(os64);
  _op_cmps(ctx,1);

  link_jmp1._label:=ctx.builder.get_curr_label.after;

 end;
end;

//

procedure _op_movs(var ctx:t_jit_context2;dflag:Integer);
var
 size:TOperandSize;
begin
 //rdi,rsi

 Assert(ctx.dis.AddressSize=as64,'prefix $67 TODO');

 case ctx.din.OpCode.Suffix of
  OPSx_b:size:=os8;
  OPSx_w:size:=os16;
  OPSx_d:size:=os32;
  OPSx_q:size:=os64;
  else;
   Assert(False);
 end;

 //(r_tmp0)r14 <-> rdi
 //(r_tmp1)r15 <-> rsi
 with ctx.builder do
 begin

   movq(r_tmp0,rdi);
   op_uplift(ctx,r_tmp0,size); //in/out:r14

   movq(r_tmp1,rsi);
   op_uplift(ctx,r_tmp1,size,[not_use_r_tmp0]); //in/out:r15

   //[RSI] -> [RDI].

   xchgq(rdi,r_tmp0);
   xchgq(rsi,r_tmp1);

   _O($A5,Size);

   xchgq(rdi,r_tmp0);
   xchgq(rsi,r_tmp1);

   if (dflag=0) then
   begin
    leaq(rdi,[rdi+OPERAND_BYTES[size]]);
    leaq(rsi,[rsi+OPERAND_BYTES[size]]);
   end else
   begin
    leaq(rdi,[rdi-OPERAND_BYTES[size]]);
    leaq(rsi,[rsi-OPERAND_BYTES[size]]);
   end;

 end;

end;

procedure op_movs(var ctx:t_jit_context2);
var
 link_jmp0:t_jit_i_link;
 link_jmp1:t_jit_i_link;
begin
 with ctx.builder do
 begin

  //get d flag
  pushfq(os64);
  bti8([rsp,os64],10); //bt [rsp], 10

  link_jmp0:=jcc(OPSc_b,nil_link,os8);

  _op_movs(ctx,0);

  link_jmp1:=jmp(nil_link,os8);

  link_jmp0._label:=ctx.builder.get_curr_label.after;

  _op_movs(ctx,1);

  link_jmp1._label:=ctx.builder.get_curr_label.after;

  popfq(os64);

 end;
end;

//

procedure _op_stos(var ctx:t_jit_context2;dflag:Integer);
var
 size:TOperandSize;

 new:TRegValue;
begin
 //rdi,rsi

 Assert(ctx.dis.AddressSize=as64,'prefix $67 TODO');

 case ctx.din.OpCode.Suffix of
  OPSx_b:size:=os8;
  OPSx_w:size:=os16;
  OPSx_d:size:=os32;
  OPSx_q:size:=os64;
  else;
   Assert(False);
 end;

 //(r_tmp0)r14 <-> rdi
 with ctx.builder do
 begin

  new:=new_reg_size(rax,size);

   movq(r_tmp0,rdi);
   op_uplift(ctx,r_tmp0,size); //in/out:r14

   movq([r_tmp0],new);

   if (dflag=0) then
   begin
    leaq(rdi,[rdi+OPERAND_BYTES[size]]);
   end else
   begin
    leaq(rdi,[rdi-OPERAND_BYTES[size]]);
   end;

 end;

end;

procedure op_stos(var ctx:t_jit_context2);
var
 link_jmp0:t_jit_i_link;
 link_jmp1:t_jit_i_link;
begin
 with ctx.builder do
 begin

  //get d flag
  pushfq(os64);
  bti8([rsp,os64],10); //bt [rsp], 10

  link_jmp0:=jcc(OPSc_b,nil_link,os8);

  _op_stos(ctx,0);

  link_jmp1:=jmp(nil_link,os8);

  link_jmp0._label:=ctx.builder.get_curr_label.after;

  _op_stos(ctx,1);

  link_jmp1._label:=ctx.builder.get_curr_label.after;

  popfq(os64);

 end;
end;

//

procedure _op_lods(var ctx:t_jit_context2;dflag:Integer);
var
 size:TOperandSize;

 new:TRegValue;
begin
 //rdi,rsi

 Assert(ctx.dis.AddressSize=as64,'prefix $67 TODO');

 case ctx.din.OpCode.Suffix of
  OPSx_b:size:=os8;
  OPSx_w:size:=os16;
  OPSx_d:size:=os32;
  OPSx_q:size:=os64;
  else;
   Assert(False);
 end;

 //(r_tmp0)r14 <-> rdi
 with ctx.builder do
 begin

  new:=new_reg_size(rax,size);

   movq(r_tmp0,rdi);
   op_uplift(ctx,r_tmp0,size); //in/out:r14

   movq(new,[r_tmp0]);

   if (dflag=0) then
   begin
    leaq(rdi,[rdi+OPERAND_BYTES[size]]);
   end else
   begin
    leaq(rdi,[rdi-OPERAND_BYTES[size]]);
   end;

 end;

end;

procedure op_lods(var ctx:t_jit_context2);
var
 link_jmp0:t_jit_i_link;
 link_jmp1:t_jit_i_link;
begin
 with ctx.builder do
 begin

  //get d flag
  pushfq(os64);
  bti8([rsp,os64],10); //bt [rsp], 10

  link_jmp0:=jcc(OPSc_b,nil_link,os8);

  _op_lods(ctx,0);

  link_jmp1:=jmp(nil_link,os8);

  link_jmp0._label:=ctx.builder.get_curr_label.after;

  _op_lods(ctx,1);

  link_jmp1._label:=ctx.builder.get_curr_label.after;

  popfq(os64);

 end;
end;

//

procedure _op_scas(var ctx:t_jit_context2;dflag:Integer);
var
 size:TOperandSize;
begin
 //rdi,rsi

 Assert(ctx.dis.AddressSize=as64,'prefix $67 TODO');

 case ctx.din.OpCode.Suffix of
  OPSx_b:size:=os8;
  OPSx_w:size:=os16;
  OPSx_d:size:=os32;
  OPSx_q:size:=os64;
  else;
   Assert(False);
 end;

 //(r_tmp0)r14 <-> rdi
 with ctx.builder do
 begin

   movq(r_tmp0,rdi);
   op_uplift(ctx,r_tmp0,size); //in/out:r14

   xchgq(rdi,r_tmp0);

   _O($AF,Size);

   xchgq(rdi,r_tmp0);

   if (dflag=0) then
   begin
    leaq(rdi,[rdi+OPERAND_BYTES[size]]);
   end else
   begin
    leaq(rdi,[rdi-OPERAND_BYTES[size]]);
   end;

 end;

end;

procedure op_scas(var ctx:t_jit_context2);
var
 link_jmp0:t_jit_i_link;
 link_jmp1:t_jit_i_link;
begin
 with ctx.builder do
 begin

  //get d flag
  pushfq(os64);
  bti8([rsp,os64],10); //bt [rsp], 10

  link_jmp0:=jcc(OPSc_b,nil_link,os8);

  popfq(os64);
  _op_scas(ctx,0);

  link_jmp1:=jmp(nil_link,os8);

  link_jmp0._label:=ctx.builder.get_curr_label.after;

  popfq(os64);
  _op_scas(ctx,1);

  link_jmp1._label:=ctx.builder.get_curr_label.after;

 end;
end;

//

const
 xor_desc:t_op_desc=(
  mem_reg:(op:$31;index:0);
  reg_mem:(op:$33;index:0);
  reg_imm:(op:$81;index:6);
  reg_im8:(op:$83;index:6);
  hint:[his_xor,his_rw];
 );

procedure op_xor(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,xor_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 or_desc:t_op_desc=(
  mem_reg:(op:$09;index:0);
  reg_mem:(op:$0B;index:0);
  reg_imm:(op:$81;index:1);
  reg_im8:(op:$83;index:1);
  hint:[his_rw];
 );

procedure op_or(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,or_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 and_desc:t_op_desc=(
  mem_reg:(op:$21;index:0);
  reg_mem:(op:$23;index:0);
  reg_imm:(op:$81;index:4);
  reg_im8:(op:$83;index:4);
  hint:[his_rw];
 );

procedure op_and(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,and_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 sub_desc:t_op_desc=(
  mem_reg:(op:$29;index:0);
  reg_mem:(op:$2B;index:0);
  reg_imm:(op:$81;index:5);
  reg_im8:(op:$83;index:5);
  hint:[his_rw];
 );

procedure op_sub(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,sub_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 sbb_desc:t_op_desc=(
  mem_reg:(op:$19;index:0);
  reg_mem:(op:$1B;index:0);
  reg_imm:(op:$81;index:3);
  reg_im8:(op:$83;index:3);
  hint:[his_rw];
 );

procedure op_sbb(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,sbb_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 add_desc:t_op_desc=(
  mem_reg:(op:$01;index:0);
  reg_mem:(op:$03;index:0);
  reg_imm:(op:$81;index:0);
  reg_im8:(op:$83;index:0);
  hint:[his_rw];
 );

procedure op_add(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,add_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 adc_desc:t_op_desc=(
  mem_reg:(op:$11;index:0);
  reg_mem:(op:$13;index:0);
  reg_imm:(op:$81;index:2);
  reg_im8:(op:$83;index:2);
  hint:[his_rw];
 );

procedure op_adc(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,adc_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 imul_desc1:t_op_type=(op:$F7;index:5);

 imul_desc2:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$0FAF;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_rw];
 );

 imul_desc3:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(opt:[not_impl]);
  reg_imm:(op:$69;index:0);
  reg_im8:(op:$6B;index:0);
  hint:[];
 );

procedure op_imul(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  case ctx.din.OperCnt of
   1:op_emit1(ctx,imul_desc1,[his_ro]); //R
   2:op_emit2(ctx,imul_desc2); //RM
   3:op_emit2(ctx,imul_desc3); //RMI
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_emit1_gn(var ctx:t_jit_context2);
var
 tmp:t_op_type;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=Default(t_op_type);

  tmp.op   :=ctx.dis.opcode;
  tmp.index:=ctx.dis.ModRM.Index;
  tmp.opt  :=[not_os8];

  op_emit1(ctx,tmp,[]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_emit1_gn_np(var ctx:t_jit_context2);
var
 tmp:t_op_type;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=Default(t_op_type);

  tmp.op   :=ctx.dis.opcode;
  tmp.index:=ctx.dis.ModRM.Index;
  tmp.opt  :=[not_os8,not_prefix];

  op_emit1(ctx,tmp,[]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_emit1_ro(var ctx:t_jit_context2);
var
 tmp:t_op_type;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=Default(t_op_type);

  tmp.op   :=ctx.dis.opcode;
  tmp.index:=ctx.dis.ModRM.Index;
  tmp.opt  :=[not_os8];

  op_emit1(ctx,tmp,[his_ro]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_emit1_ro_np(var ctx:t_jit_context2);
var
 tmp:t_op_type;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=Default(t_op_type);

  tmp.op   :=ctx.dis.opcode;
  tmp.index:=ctx.dis.ModRM.Index;
  tmp.opt  :=[not_os8,not_prefix];

  op_emit1(ctx,tmp,[his_ro]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_emit1_rw(var ctx:t_jit_context2);
var
 tmp:t_op_type;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=Default(t_op_type);

  tmp.op   :=ctx.dis.opcode;
  tmp.index:=ctx.dis.ModRM.Index;
  tmp.opt  :=[not_os8];

  op_emit1(ctx,tmp,[his_rw]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_emit1_rw_np(var ctx:t_jit_context2);
var
 tmp:t_op_type;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=Default(t_op_type);

  tmp.op   :=ctx.dis.opcode;
  tmp.index:=ctx.dis.ModRM.Index;
  tmp.opt  :=[not_os8,not_prefix];

  op_emit1(ctx,tmp,[his_rw]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 bt_desc:t_op_desc=(
  mem_reg:(op:$0FA3;index:0);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(op:$0FBA;index:4);
  hint:[his_ro];
 );

procedure op_bt(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,bt_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 btc_desc:t_op_desc=(
  mem_reg:(op:$0FBB;index:0);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(op:$0FBA;index:7);
  hint:[his_rw];
 );

procedure op_btc(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,btc_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 bts_desc:t_op_desc=(
  mem_reg:(op:$0FAB;index:0);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(op:$0FBA;index:5);
  hint:[his_rw];
 );

procedure op_bts(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,bts_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 btr_desc:t_op_desc=(
  mem_reg:(op:$0FB3;index:0);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(op:$0FBA;index:6);
  hint:[his_rw];
 );

procedure op_btr(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,btr_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 bsf_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$0FBC;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
 );

procedure op_bsf(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  //If the second operand contains 0,
  //the instruction sets ZF to 1
  //and does not change the contents
  //of the destination register.
  op_emit2(ctx,bsf_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 bsr_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$0FBD;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
 );

procedure op_bsr(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  //If the second operand contains 0,
  //the instruction sets ZF to 1
  //and does not change the contents
  //of the destination register.
  op_emit2(ctx,bsr_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;


//

const
 xchg_desc:t_op_desc=(
  mem_reg:(op:$87;index:0);
  reg_mem:(op:$87;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_xchg,his_rw];
 );

procedure op_xchg(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,xchg_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 mov_desc:t_op_desc=(
  mem_reg:(op:$89;index:0);
  reg_mem:(op:$8B;index:0);
  reg_imm:(op:$C7;index:0);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_mov(var ctx:t_jit_context2);
var
 data:array[0..15] of Byte;
 Code:Pointer;
 Operand:TOperand;
 i:Byte;
begin
 if is_segment(ctx.din.Operand[1]) then
 begin
  Exit; //skip segment change
 end;

 if is_segment(ctx.din.Operand[2]) then
 begin
  if is_preserved(ctx.din) or is_memory(ctx.din) then
  begin
   i:=ctx.dis.CodeIdx;
   Move(ctx.Code^,data,i);

   data[i]:=get_segment_value(ctx.din.Operand[2]);

   Code:=ctx.Code;
   Operand:=ctx.din.Operand[2];

   ctx.din.Operand[2].RegValue:=Default(TRegValues);
   ctx.din.Operand[2].Size:=os8;
   ctx.din.Operand[2].ByteCount:=i;

   op_emit2(ctx,mov_desc);

   ctx.din.Operand[2]:=Operand;
   ctx.Code:=Code;
  end else
  begin
   //reg_imm
   ctx.builder.movi(new_reg(ctx.din.Operand[1]),get_segment_value(ctx.din.Operand[2]));
  end;

  Exit;
 end;

 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,mov_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 CMOV_8:array[OPSc_o..OPSc_nle] of Byte=(
  $40,$41,$42,$43,$44,$45,$46,$47,
  $48,$49,$4A,$4B,$4C,$4D,$4E,$4F
 );

const
 cmov_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$00;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_cmov(var ctx:t_jit_context2);
var
 desc:t_op_desc;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  desc:=cmov_desc;
  desc.reg_mem.op:=$0F00 or CMOV_8[ctx.din.OpCode.Suffix];
  //
  op_emit2(ctx,desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 movx_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$00;opt:[not_os8]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_movzx(var ctx:t_jit_context2);
var
 desc:t_op_desc;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  desc:=movx_desc;

  //
  case ctx.din.Operand[2].Size of
    os8:desc.reg_mem.op:=$0FB6;
   os16:desc.reg_mem.op:=$0FB7;
   else
    Assert(false);
  end;

  op_emit2(ctx,desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_movsx(var ctx:t_jit_context2);
var
 desc:t_op_desc;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  desc:=movx_desc;

  //
  case ctx.din.Operand[2].Size of
    os8:desc.reg_mem.op:=$0FBE;
   os16:desc.reg_mem.op:=$0FBF;
   else
    Assert(false);
  end;

  op_emit2(ctx,desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 movbe_desc:t_op_desc=(
  mem_reg:(op:$0F38F1;opt:[not_os8]);
  reg_mem:(op:$0F38F0;opt:[not_os8]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_movbe(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,movbe_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 movsxd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$63;opt:[not_os8]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_movsxd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,movsxd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 test_desc:t_op_desc=(
  mem_reg:(op:$85;index:0);
  reg_mem:(opt:[not_impl]);
  reg_imm:(op:$F7;index:0);
  reg_im8:(opt:[not_impl]);
  hint:[his_ro];
 );

procedure op_test(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,test_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 cmp_desc:t_op_desc=(
  mem_reg:(op:$39;index:0);
  reg_mem:(op:$3B;index:0);
  reg_imm:(op:$81;index:7);
  reg_im8:(op:$83;index:7);
  hint:[his_ro];
 );

procedure op_cmp(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,cmp_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 cmpxchg_desc:t_op_desc=(
  mem_reg:(op:$0FB1;index:0);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_xchg,his_rw];
 );

procedure op_cmpxchg(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,cmpxchg_desc);
 end else
 with ctx.builder do
 begin
  add_orig(ctx);
 end;
end;

const
 cmpxchg_8b_16b_desc:t_op_type=(op:$0FC7;index:1);

procedure op_cmpxchg_8b_16b(var ctx:t_jit_context2);
begin
 op_emit1(ctx,cmpxchg_8b_16b_desc,[his_xchg,his_rw]);
end;

const
 xadd_desc:t_op_desc=(
  mem_reg:(op:$0FC1;index:0);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_rw];
 );

procedure op_xadd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,xadd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 lzcnt_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F30FBD;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
 );

procedure op_lzcnt(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,lzcnt_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 tzcnt_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F30FBC;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
 );

procedure op_tzcnt(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,tzcnt_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 popcnt_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F30FB8;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
 );

procedure op_popcnt(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,popcnt_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 crc32_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F20F38F1;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_crc32(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,crc32_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

procedure op_shift2_gen(var ctx:t_jit_context2);
const
 desc:t_op_shift=(
  reg_im8:(op:$C1;index:0);
  mem__cl:(op:$D3;index:0);
  mem_one:(op:$D1;index:0);
 );
var
 tmp:t_op_shift;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=desc;
  tmp.reg_im8.index:=ctx.dis.ModRM.Index;
  tmp.mem__cl.index:=ctx.dis.ModRM.Index;
  tmp.mem_one.index:=ctx.dis.ModRM.Index;
  //
  op_emit_shift2(ctx,tmp);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 shld_desc:t_op_shift=(
  reg_im8:(op:$0FA4);
  mem__cl:(op:$0FA5);
  mem_one:(opt:[not_impl]);
 );

procedure op_shld(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_shift3(ctx,shld_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 shrd_desc:t_op_shift=(
  reg_im8:(op:$0FAC);
  mem__cl:(op:$0FAD);
  mem_one:(opt:[not_impl]);
 );

procedure op_shrd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_shift3(ctx,shrd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 bswap_desc:t_op_type=(
  op:$0FC8;index:0
 );

procedure op_bswap(var ctx:t_jit_context2);
var
 new:TRegValue;
begin
 if is_preserved(ctx.din) then
 begin
  with ctx.builder do
  begin
   new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

   op_load(ctx,new,1);

   _O(bswap_desc,new);

   op_save(ctx,1,fix_size(new));
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_lea(var ctx:t_jit_context2);
var
 new:TRegValue;
begin
 if is_preserved(ctx.din) then
 begin
  if is_preserved(ctx.din.Operand[1]) then
  begin
   new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
   build_lea(ctx,2,new,[not_use_segment]);
   //
   op_save(ctx,1,fix_size(new));
  end else
  begin
   new:=new_reg(ctx.din.Operand[1]);
   //
   if (new.ASize=os16) then
   begin
    //low part
    build_lea(ctx,2,r_tmp0);
    //
    ctx.builder.movq(new,r_tmp0);
   end else
   begin
    build_lea(ctx,2,new);
   end;
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

//

procedure init_cbs;
begin
 //

 jit_rep_cbs[repOPmovs]:=@op_rep_movs;
 jit_rep_cbs[repOPlods]:=nil;
 jit_rep_cbs[repOPstos]:=@op_rep_stos;
 jit_rep_cbs[repOPcmps]:=@op_rep_cmps;
 jit_rep_cbs[repOPscas]:=nil;

 jit_cbs[OPPnone,OPcmps,OPSx_b]:=@op_cmps;
 jit_cbs[OPPnone,OPcmps,OPSx_w]:=@op_cmps;
 jit_cbs[OPPnone,OPcmps,OPSx_d]:=@op_cmps;
 jit_cbs[OPPnone,OPcmps,OPSx_q]:=@op_cmps;


 jit_cbs[OPPnone,OPmovs,OPSx_b]:=@op_movs;
 jit_cbs[OPPnone,OPmovs,OPSx_w]:=@op_movs;
 jit_cbs[OPPnone,OPmovs,OPSx_d]:=@op_movs;
 jit_cbs[OPPnone,OPmovs,OPSx_q]:=@op_movs;

 jit_cbs[OPPnone,OPstos,OPSx_b]:=@op_stos;
 jit_cbs[OPPnone,OPstos,OPSx_w]:=@op_stos;
 jit_cbs[OPPnone,OPstos,OPSx_d]:=@op_stos;
 jit_cbs[OPPnone,OPstos,OPSx_q]:=@op_stos;

 jit_cbs[OPPnone,OPlods,OPSx_b]:=@op_lods;
 jit_cbs[OPPnone,OPlods,OPSx_w]:=@op_lods;
 jit_cbs[OPPnone,OPlods,OPSx_d]:=@op_lods;
 jit_cbs[OPPnone,OPlods,OPSx_q]:=@op_lods;

 jit_cbs[OPPnone,OPscas,OPSx_b]:=@op_scas;
 jit_cbs[OPPnone,OPscas,OPSx_w]:=@op_scas;
 jit_cbs[OPPnone,OPscas,OPSx_d]:=@op_scas;
 jit_cbs[OPPnone,OPscas,OPSx_q]:=@op_scas;

 //

 jit_cbs[OPPnone,OPxor ,OPSnone]:=@op_xor;
 jit_cbs[OPPnone,OPor  ,OPSnone]:=@op_or;
 jit_cbs[OPPnone,OPand ,OPSnone]:=@op_and;
 jit_cbs[OPPnone,OPsub ,OPSnone]:=@op_sub;
 jit_cbs[OPPnone,OPsbb ,OPSnone]:=@op_sbb;
 jit_cbs[OPPnone,OPadd ,OPSnone]:=@op_add;
 jit_cbs[OPPnone,OPadc ,OPSnone]:=@op_adc;

 jit_cbs[OPPnone,OPimul,OPSnone]:=@op_imul;
 jit_cbs[OPPnone,OPmul ,OPSnone]:=@op_emit1_ro;
 jit_cbs[OPPnone,OPidiv,OPSnone]:=@op_emit1_ro;
 jit_cbs[OPPnone,OPdiv ,OPSnone]:=@op_emit1_ro;

 jit_cbs[OPPnone,OPbt  ,OPSnone]:=@op_bt;
 jit_cbs[OPPnone,OPbtc ,OPSnone]:=@op_btc;
 jit_cbs[OPPnone,OPbts ,OPSnone]:=@op_bts;
 jit_cbs[OPPnone,OPbtr ,OPSnone]:=@op_btr;

 jit_cbs[OPPnone,OPbsf,OPSnone]:=@op_bsf;
 jit_cbs[OPPnone,OPbsr,OPSnone]:=@op_bsr;

 jit_cbs[OPPnone,OPxchg,OPSnone]:=@op_xchg;

 jit_cbs[OPPnone,OPmov ,OPSnone]:=@op_mov;

 jit_cbs[OPPnone,OPcmov__,OPSc_o  ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_no ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_b  ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_nb ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_z  ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_nz ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_be ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_nbe]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_s  ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_ns ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_p  ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_np ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_l  ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_nl ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_le ]:=@op_cmov;
 jit_cbs[OPPnone,OPcmov__,OPSc_nle]:=@op_cmov;

 jit_cbs[OPPnone,OPmovzx,OPSnone]:=@op_movzx;
 jit_cbs[OPPnone,OPmovsx,OPSnone]:=@op_movsx;
 jit_cbs[OPPnone,OPmov  ,OPSc_be]:=@op_movbe;
 jit_cbs[OPPnone,OPmovsx,OPSx_d ]:=@op_movsxd;

 jit_cbs[OPPnone,OPtest,OPSnone]:=@op_test;
 jit_cbs[OPPnone,OPcmp ,OPSnone]:=@op_cmp;

 jit_cbs[OPPnone,OPcmpxchg,OPSnone ]:=@op_cmpxchg;
 jit_cbs[OPPnone,OPcmpxchg,OPSx_8b ]:=@op_cmpxchg_8b_16b;
 jit_cbs[OPPnone,OPcmpxchg,OPSx_16b]:=@op_cmpxchg_8b_16b;

 jit_cbs[OPPnone,OPxadd   ,OPSnone]:=@op_xadd;

 jit_cbs[OPPnone,OPlzcnt  ,OPSnone]:=@op_lzcnt;
 jit_cbs[OPPnone,OPtzcnt  ,OPSnone]:=@op_tzcnt;
 jit_cbs[OPPnone,OPpopcnt ,OPSnone]:=@op_popcnt;
 jit_cbs[OPPnone,OPcrc32  ,OPSnone]:=@op_crc32;

 jit_cbs[OPPnone,OProl ,OPSnone]:=@op_shift2_gen;
 jit_cbs[OPPnone,OPror ,OPSnone]:=@op_shift2_gen;
 jit_cbs[OPPnone,OPrcl ,OPSnone]:=@op_shift2_gen;
 jit_cbs[OPPnone,OPrcr ,OPSnone]:=@op_shift2_gen;
 jit_cbs[OPPnone,OPshl ,OPSnone]:=@op_shift2_gen;
 jit_cbs[OPPnone,OPshr ,OPSnone]:=@op_shift2_gen;
 jit_cbs[OPPnone,OPsar ,OPSnone]:=@op_shift2_gen;
 jit_cbs[OPPnone,OPsal ,OPSnone]:=@op_shift2_gen;

 jit_cbs[OPPnone,OPshl ,OPSx_d ]:=@op_shld;
 jit_cbs[OPPnone,OPshr ,OPSx_d ]:=@op_shrd;

 jit_cbs[OPPnone,OPset__,OPSc_o  ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_no ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_b  ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_nb ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_z  ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_nz ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_be ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_nbe]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_s  ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_ns ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_p  ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_np ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_l  ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_nl ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_le ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPset__,OPSc_nle]:=@op_emit1_gn_np;

 jit_cbs[OPPnone,OPemms      ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfemms     ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPvzeroall  ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPvzeroupper,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPpause,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPlfence,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPmfence,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPsfence,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPcmc,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPcli ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPsti ,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPclac,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPclc ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPcld ,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPstc ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPstd ,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPcwd ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPcdq ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPcqo ,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPcbw ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPcwde,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPcdqe,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPgetbv,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPrdpmc,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPlea,OPSnone]:=@op_lea;

 jit_cbs[OPPnone,OPinc,OPSnone]:=@op_emit1_rw;
 jit_cbs[OPPnone,OPdec,OPSnone]:=@op_emit1_rw;
 jit_cbs[OPPnone,OPneg,OPSnone]:=@op_emit1_rw;
 jit_cbs[OPPnone,OPnot,OPSnone]:=@op_emit1_rw;

 jit_cbs[OPPnone,OPbswap,OPSnone]:=@op_bswap;

 jit_cbs[OPPnone,OPsahf,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPlahf,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPxlat,OPSnone]:=@add_orig;

 //fpu

 jit_cbs[OPPnone,OPfninit  ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfrndint ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfchs    ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfxch    ,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPfcmov__ ,OPSc_b  ]:=@add_orig;
 jit_cbs[OPPnone,OPfcmov__ ,OPSc_e  ]:=@add_orig;
 jit_cbs[OPPnone,OPfcmov__ ,OPSc_be ]:=@add_orig;
 jit_cbs[OPPnone,OPfcmov__ ,OPSc_u  ]:=@add_orig;
 jit_cbs[OPPnone,OPfcmov__ ,OPSc_nb ]:=@add_orig;
 jit_cbs[OPPnone,OPfcmov__ ,OPSc_ne ]:=@add_orig;
 jit_cbs[OPPnone,OPfcmov__ ,OPSc_nbe]:=@add_orig;
 jit_cbs[OPPnone,OPfcmov__ ,OPSc_nu ]:=@add_orig;

 jit_cbs[OPPnone,OPfld1  ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfldl2e,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfldl2t,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfldlg2,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfldln2,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfldpi ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfldz  ,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPfucom,OPSx_i ]:=@add_orig;
 jit_cbs[OPPnone,OPfucom,OPSx_ip]:=@add_orig;
 jit_cbs[OPPnone,OPfucom,OPSx_p ]:=@add_orig;

 jit_cbs[OPPnone,OPfcom,OPSx_i ]:=@add_orig;
 jit_cbs[OPPnone,OPfcom,OPSx_ip]:=@add_orig;
 jit_cbs[OPPnone,OPfcom,OPSx_p ]:=@add_orig;

 jit_cbs[OPPnone,OPfadd   ,OPSx_p ]:=@add_orig;
 jit_cbs[OPPnone,OPfsub   ,OPSx_p ]:=@add_orig;
 jit_cbs[OPPnone,OPfsubr  ,OPSx_p ]:=@add_orig;
 jit_cbs[OPPnone,OPfmul   ,OPSx_p ]:=@add_orig;
 jit_cbs[OPPnone,OPfdiv   ,OPSx_p ]:=@add_orig;
 jit_cbs[OPPnone,OPfdivr  ,OPSx_p ]:=@add_orig;
 jit_cbs[OPPnone,OPfsqrt  ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfabs   ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfscale ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfprem  ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfprem1 ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfxtract,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfwait  ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfnclex ,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPfsin   ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfcos   ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfsincos,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfpatan ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfptan  ,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPfyl2x  ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfyl2xp1,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPf2xm1  ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfdecstp,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPffree  ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfincstp,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPftst   ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfxam   ,OPSnone]:=@add_orig;

 //

 jit_cbs[OPPnone,OPfldcw  ,OPSnone]:=@op_emit1_ro_np;
 jit_cbs[OPPnone,OPfld    ,OPSnone]:=@op_emit1_ro_np;
 jit_cbs[OPPnone,OPfild   ,OPSnone]:=@op_emit1_ro_np;

 jit_cbs[OPPnone,OPfldenv ,OPSnone]:=@op_emit1_ro_np;
 jit_cbs[OPPnone,OPfnstenv,OPSnone]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPfnstcw ,OPSnone]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPfnstsw ,OPSnone]:=@op_emit1_gn_np;

 jit_cbs[OPPnone,OPfxsave ,OPSnone]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPfxrstor,OPSnone]:=@op_emit1_ro_np;
 jit_cbs[OPPnone,OPfst    ,OPSnone]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPfst    ,OPSx_p ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPfist   ,OPSnone]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPfist   ,OPSx_p ]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPfisttp ,OPSnone]:=@op_emit1_gn_np;
 jit_cbs[OPPnone,OPfadd   ,OPSnone]:=@op_emit1_ro_np;
 jit_cbs[OPPnone,OPfiadd  ,OPSnone]:=@op_emit1_ro_np;
 jit_cbs[OPPnone,OPfmul   ,OPSnone]:=@op_emit1_ro_np;
 jit_cbs[OPPnone,OPfimul  ,OPSnone]:=@op_emit1_ro_np;
 jit_cbs[OPPnone,OPfsub   ,OPSnone]:=@op_emit1_ro_np;
 jit_cbs[OPPnone,OPfsubr  ,OPSnone]:=@op_emit1_ro_np;
 jit_cbs[OPPnone,OPfisub  ,OPSnone]:=@op_emit1_ro_np;
 jit_cbs[OPPnone,OPfdiv   ,OPSnone]:=@op_emit1_ro_np;
 jit_cbs[OPPnone,OPfdivr  ,OPSnone]:=@op_emit1_ro_np;

 jit_cbs[OPPnone,OPclflush,OPSnone]:=@op_emit1_rw_np;

 //fpu

 jit_cbs[OPPnone,OPprefetch,OPSnone ]:=@op_emit1_rw_np;
 jit_cbs[OPPnone,OPprefetch,OPSp_w  ]:=@op_emit1_rw_np;
 jit_cbs[OPPnone,OPprefetch,OPSp_nta]:=@op_emit1_rw_np;
 jit_cbs[OPPnone,OPprefetch,OPSp_t0 ]:=@op_emit1_rw_np;
 jit_cbs[OPPnone,OPprefetch,OPSp_t1 ]:=@op_emit1_rw_np;
 jit_cbs[OPPnone,OPprefetch,OPSp_t2 ]:=@op_emit1_rw_np;

end;

initialization
 init_cbs;

end.

