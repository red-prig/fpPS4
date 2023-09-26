unit kern_jit2_ops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 x86_fpdbgdisas,
 x86_jit,
 kern_jit2_ctx;

type
 t_jit_cb=procedure(var ctx:t_jit_context2);

var
 jit_cbs:array[TOpcodePrefix,TOpCode,TOpCodeSuffix] of t_jit_cb;

procedure init_cbs;

implementation

uses
 machdep;

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
   1:op_emit1(ctx,imul_desc1,[his_rax]); //R
   2:op_emit2(ctx,imul_desc2); //RM
   3:op_emit2(ctx,imul_desc3); //RMI
   else
    Assert(false);
  end;
 end else
 begin
  case ctx.din.OperCnt of
   1:begin
      op_load_rax(ctx,ctx.builder.rax);
      add_orig(ctx);
      op_save_rax(ctx,ctx.builder.rax);
     end;
   else
     add_orig(ctx);
  end;
 end;
end;

const
 mul_desc:t_op_type=(op:$F7;index:4);

procedure op_mul(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit1(ctx,mul_desc,[his_rax]); //R
 end else
 begin
  op_load_rax(ctx,ctx.builder.rax);
  add_orig(ctx);
  op_save_rax(ctx,ctx.builder.rax);
 end;
end;

const
 idiv_desc1:t_op_type=(op:$F7;index:7);

procedure op_idiv(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit1(ctx,idiv_desc1,[his_rax]); //R
 end else
 begin
  op_load_rax(ctx,ctx.builder.rax);
  add_orig(ctx);
  op_save_rax(ctx,ctx.builder.rax);
 end;
end;

const
 div_desc:t_op_type=(op:$F7;index:6);

procedure op_div(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit1(ctx,div_desc,[his_rax]); //R
 end else
 begin
  op_load_rax(ctx,ctx.builder.rax);
  add_orig(ctx);
  op_save_rax(ctx,ctx.builder.rax);
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
 bts_desc:t_op_desc=(
  mem_reg:(op:$0FAB;index:0);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(op:$0FBA;index:5);
  hint:[his_ro];
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
 xchg_desc:t_op_desc=(
  mem_reg:(op:$87;index:0);
  reg_mem:(op:$87;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_xchg];
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

function is_segment(const r:TOperand):Boolean; inline;
begin
 Result:=(r.RegValue[0].AType=regSegment) and
         (not (ofMemory in r.Flags));
end;

function get_segment_value(const Operand:TOperand):Byte;
const
 REG_ES = 0;
 REG_CS = 1;
 REG_SS = 2;
 REG_DS = 3;
 REG_FS = 4;
 REG_GS = 5;
begin
 case Operand.RegValue[0].AIndex of
  REG_ES:Result:=_udatasel;
  REG_CS:Result:=_ucodesel;
  REG_SS:Result:=_udatasel;
  REG_DS:Result:=_udatasel;
  REG_FS:Result:=_ufssel;
  REG_GS:Result:=_ugssel;
  else
         Result:=0;
 end;
end;

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
  reg_mem:(op:$00;index:0);
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
 movsxd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$63;index:0);
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

const
 movbe_desc:t_op_desc=(
  mem_reg:(op:$0F38F1;index:0);
  reg_mem:(op:$0F38F0;index:0);
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
 vmovups_desc:t_op_desc=(
  mem_reg:(op:$11;index:0;mm:1);
  reg_mem:(op:$10;index:0;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vmovups(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmovups_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovupd_desc:t_op_desc=(
  mem_reg:(op:$11;index:1;mm:1);
  reg_mem:(op:$10;index:1;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vmovupd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmovupd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovaps_desc:t_op_desc=(
  mem_reg:(op:$29;index:0;mm:1);
  reg_mem:(op:$28;index:0;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo,his_align];
 );

procedure op_vmovaps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmovaps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovapd_desc:t_op_desc=(
  mem_reg:(op:$29;index:1;mm:1);
  reg_mem:(op:$28;index:1;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo,his_align];
 );

procedure op_vmovapd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmovapd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovdqu_desc:t_op_desc=(
  mem_reg:(op:$7F;index:2;mm:1);
  reg_mem:(op:$6F;index:2;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vmovdqu(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmovdqu_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovdqa_desc:t_op_desc=(
  mem_reg:(op:$7F;index:1;mm:1);
  reg_mem:(op:$6F;index:1;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo,his_align];
 );

procedure op_vmovdqa(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmovdqa_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovntdqa_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$2A;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo,his_align];
 );

procedure op_vmovntdqa(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmovntdqa_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovntdq_desc:t_op_desc=(
  mem_reg:(op:$E7;index:1;mm:1);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vmovntdq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmovntdq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovddup_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$12;index:3;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vmovddup(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmovddup_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovq_desc:t_op_desc=( //vmovd_desc
  mem_reg:(op:$7E;index:1;mm:1);
  reg_mem:(op:$6E;index:1;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vmovq(var ctx:t_jit_context2); //op_vmovd
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmovq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovss_mrr_desc:t_op_type=(op:$11;index:2;mm:1;opt:[not_vex_len]);
 vmovss_rrm_desc:t_op_type=(op:$10;index:2;mm:1;opt:[not_vex_len]);

procedure op_vmovss(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  if is_memory(ctx.din.Operand[1]) then
  begin
   op_emit_avx3(ctx,vmovss_mrr_desc);
  end else
  if is_memory(ctx.din.Operand[3]) then
  begin
   op_emit_avx3(ctx,vmovss_rrm_desc);
  end else
  begin
   Assert(False);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovsd_mrr_desc:t_op_type=(op:$11;index:3;mm:1;opt:[not_vex_len]);
 vmovsd_rrm_desc:t_op_type=(op:$10;index:3;mm:1;opt:[not_vex_len]);

procedure op_vmovsd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  if is_memory(ctx.din.Operand[1]) then
  begin
   op_emit_avx3(ctx,vmovsd_mrr_desc);
  end else
  if is_memory(ctx.din.Operand[3]) then
  begin
   op_emit_avx3(ctx,vmovsd_rrm_desc);
  end else
  begin
   Assert(False);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovlpd_desc2:t_op_desc=(
  mem_reg:(op:$13;index:1;mm:1);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

 vmovlpd_rrm_desc:t_op_type=(op:$12;index:1;mm:1);

procedure op_vmovlpd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  case ctx.din.OperCnt of
   2:op_emit_avx2(ctx,vmovlpd_desc2);
   3:op_emit_avx3(ctx,vmovlpd_rrm_desc);
   else
    Assert(False);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovhpd_desc2:t_op_desc=(
  mem_reg:(op:$17;index:1;mm:1);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

 vmovhpd_rrm_desc:t_op_type=(op:$16;index:1;mm:1);

procedure op_vmovhpd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  case ctx.din.OperCnt of
   2:op_emit_avx2(ctx,vmovhpd_desc2);
   3:op_emit_avx3(ctx,vmovhpd_rrm_desc);
   else
    Assert(False);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovsldup_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$12;index:2;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vmovsldup(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmovsldup_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovshdup_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$16;index:2;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vmovshdup(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmovshdup_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpmovsxbw_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$20;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vpmovsxbw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vpmovsxbw_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpmovsxdq_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$25;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vpmovsxdq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vpmovsxdq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vucomiss_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$2E;index:0;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_vucomiss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vucomiss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vucomisd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$2E;index:1;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_vucomisd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vucomisd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vptest_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$17;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_ro];
 );

procedure op_vptest(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vptest_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 blsr_desc:t_op_type=(
  op:$F3;index:1;mm:2
 );

procedure op_blsr(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx_F3(ctx,blsr_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 blsi_desc:t_op_type=(
  op:$F3;index:3;mm:2
 );

procedure op_blsi(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx_F3(ctx,blsi_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 SETcc_8:array[OPSc_o..OPSc_nle] of Byte=(
  $90,$91,$92,$93,$94,$95,$96,$97,
  $98,$99,$9A,$9B,$9C,$9D,$9E,$9F
 );

procedure op_setcc(var ctx:t_jit_context2);
var
 desc:t_op_type;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  desc:=Default(t_op_type);
  desc.op:=$0F00 or SETcc_8[ctx.din.OpCode.Suffix];
  //
  op_emit1(ctx,desc,[]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 test_desc:t_op_desc=(
  mem_reg:(op:$85;index:0);
  reg_mem:(op:$00;index:0);
  reg_imm:(op:$F7;index:0);
  reg_im8:(op:$00;index:0);
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
  hint:[his_xchg,his_rax,his_rw];
 );

procedure op_cmpxchg(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,cmpxchg_desc);
 end else
 begin
  op_load_rax(ctx,ctx.builder.rax);
  add_orig(ctx);
  op_save_rax(ctx,ctx.builder.rax);
 end;
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

const
 shl_desc:t_op_shift=(
  reg_im8:(op:$C1;index:4);
  mem__cl:(op:$D3;index:4);
  mem_one:(op:$D1;index:4);
 );

procedure op_shl(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_shift(ctx,shl_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 shr_desc:t_op_shift=(
  reg_im8:(op:$C1;index:5);
  mem__cl:(op:$D3;index:5);
  mem_one:(op:$D1;index:5);
 );

procedure op_shr(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_shift(ctx,shr_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 sar_desc:t_op_shift=(
  reg_im8:(op:$C1;index:7);
  mem__cl:(op:$D3;index:7);
  mem_one:(op:$D1;index:7);
 );

procedure op_sar(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_shift(ctx,sar_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 rol_desc:t_op_shift=(
  reg_im8:(op:$C1;index:0);
  mem__cl:(op:$D3;index:0);
  mem_one:(op:$D1;index:0);
 );

procedure op_rol(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_shift(ctx,rol_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 inc_desc:t_op_type=(
  op:$FF;index:0
 );

procedure op_inc(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit1(ctx,inc_desc,[his_rw]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 dec_desc:t_op_type=(
  op:$FF;index:1
 );

procedure op_dec(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit1(ctx,dec_desc,[his_rw]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 neg_desc:t_op_type=(
  op:$F7;index:3
 );

procedure op_neg(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit1(ctx,neg_desc,[his_rw]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 not_desc:t_op_type=(
  op:$F7;index:2
 );

procedure op_not(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit1(ctx,not_desc,[his_rw]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 bswap_desc:t_op_type=(
  op:$0FC8;index:0
 );

procedure op_bswap(var ctx:t_jit_context2);
var
 i:Integer;
begin
 if is_preserved(ctx.din) then
 begin
  with ctx.builder do
  begin
   i:=GetFrameOffset(ctx.din.Operand[1]);

   movq(r_tmp0,[r_thrd+i]);

   _O(bswap_desc,r_tmp0);

   movq([r_thrd+i],r_tmp0);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;


procedure op_lea(var ctx:t_jit_context2);
var
 new:TRegValue;
 i:Integer;
begin
 if is_preserved(ctx.din) then
 begin
  if is_preserved(ctx.din.Operand[1]) then
  begin
   new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
   build_lea(ctx,2,new);
   //
   i:=GetFrameOffset(ctx.din.Operand[1].RegValue[0]);
   ctx.builder.movq([r_thrd+i],new);
  end else
  begin
   new:=new_reg(ctx.din.Operand[1]);
   build_lea(ctx,2,new);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_cdq(var ctx:t_jit_context2);
var
 i:Integer;
begin
 with ctx.builder do
 begin
  i:=GetFrameOffset(rax);
  movq(rax,[r_thrd+i]);

  add_orig(ctx);

  movq([r_thrd+i],rax);
 end;
end;

const
 pxor_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$0FEF;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_xor,his_rw];
 );

procedure op_pxor(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2(ctx,pxor_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vxorps_desc:t_op_type=(
  op:$57;index:0;mm:1;
 );

procedure op_vxorps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vxorps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vxorpd_desc:t_op_type=(
  op:$57;index:1;mm:1;
 );

procedure op_vxorpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vxorpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpcmpeqb_desc:t_op_type=(
  op:$74;index:1;mm:1;
 );

procedure op_vpcmpeqb(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpcmpeqb_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpcmpeqw_desc:t_op_type=(
  op:$75;index:1;mm:1;
 );

procedure op_vpcmpeqw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpcmpeqw_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpcmpeqd_desc:t_op_type=(
  op:$76;index:1;mm:1;
 );

procedure op_vpcmpeqd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpcmpeqd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpcmpgtd_desc:t_op_type=(
  op:$66;index:1;mm:1;
 );

procedure op_vpcmpgtd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpcmpgtd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpcmpestri_desc:t_op_avx3_imm=(
  rmi:(op:$61;index:1;mm:3);
  mri:(opt:[not_impl]);
 );

procedure op_vpcmpestri(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3_imm8(ctx,vpcmpestri_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpcmpistrm_desc:t_op_avx3_imm=(
  rmi:(op:$62;index:1;mm:3);
  mri:(opt:[not_impl]);
 );

procedure op_vpcmpistrm(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3_imm8(ctx,vpcmpistrm_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpcmpistri_desc:t_op_avx3_imm=(
  rmi:(op:$63;index:1;mm:3);
  mri:(opt:[not_impl]);
 );

procedure op_vpcmpistri(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3_imm8(ctx,vpcmpistri_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vsubsd_desc:t_op_type=(
  op:$5C;index:3;mm:1;
 );

procedure op_vsubsd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vsubsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vsubss_desc:t_op_type=(
  op:$5C;index:2;mm:1;
 );

procedure op_vsubss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vsubss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vsubps_desc:t_op_type=(
  op:$5C;index:0;mm:1;
 );

procedure op_vsubps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vsubps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vsubpd_desc:t_op_type=(
  op:$5C;index:1;mm:1;
 );

procedure op_vsubpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vsubpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpsubw_desc:t_op_type=(
  op:$F9;index:1;mm:1;
 );

procedure op_vpsubw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpsubw_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpsubd_desc:t_op_type=(
  op:$FA;index:1;mm:1;
 );

procedure op_vpsubd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpsubd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpsubq_desc:t_op_type=(
  op:$FB;index:1;mm:1;
 );

procedure op_vpsubq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpsubq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpaddb_desc:t_op_type=(
  op:$FC;index:1;mm:1;
 );

procedure op_vpaddb(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpaddb_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpaddw_desc:t_op_type=(
  op:$FD;index:1;mm:1;
 );

procedure op_vpaddw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpaddw_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpaddd_desc:t_op_type=(
  op:$FE;index:1;mm:1;
 );

procedure op_vpaddd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpaddd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpaddq_desc:t_op_type=(
  op:$D4;index:1;mm:1;
 );

procedure op_vpaddq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpaddq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vaddps_desc:t_op_type=(
  op:$58;index:0;mm:1;
 );

procedure op_vaddps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vaddps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vaddpd_desc:t_op_type=(
  op:$58;index:1;mm:1;
 );

procedure op_vaddpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vaddpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vaddss_desc:t_op_type=(
  op:$58;index:2;mm:1;
 );

procedure op_vaddss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vaddss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vaddsd_desc:t_op_type=(
  op:$58;index:3;mm:1;
 );

procedure op_vaddsd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vaddsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vphaddw_desc:t_op_type=(
  op:$01;index:1;mm:2;
 );

procedure op_vphaddw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vphaddw_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;


const
 vphaddd_desc:t_op_type=(
  op:$02;index:1;mm:2;
 );

procedure op_vphaddd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vphaddd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vdivps_desc:t_op_type=(
  op:$5E;index:0;mm:1;
 );

procedure op_vdivps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vdivps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vdivpd_desc:t_op_type=(
  op:$5E;index:1;mm:1;
 );

procedure op_vdivpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vdivpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vdivss_desc:t_op_type=(
  op:$5E;index:2;mm:1;
 );

procedure op_vdivss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vdivss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vdivsd_desc:t_op_type=(
  op:$5E;index:3;mm:1;
 );

procedure op_vdivsd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vdivsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmulps_desc:t_op_type=(
  op:$59;index:0;mm:1;
 );

procedure op_vmulps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vmulps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmulpd_desc:t_op_type=(
  op:$59;index:1;mm:1;
 );

procedure op_vmulpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vmulpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmulss_desc:t_op_type=(
  op:$59;index:2;mm:1;
 );

procedure op_vmulss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vmulss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmulsd_desc:t_op_type=(
  op:$59;index:3;mm:1;
 );

procedure op_vmulsd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vmulsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpmullw_desc:t_op_type=(
  op:$D5;index:1;mm:1;
 );

procedure op_vpmullw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpmullw_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpmulhuw_desc:t_op_type=(
  op:$E4;index:1;mm:1;
 );

procedure op_vpmulhuw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpmulhuw_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpunpcklqdq_desc:t_op_type=(
  op:$6C;index:1;mm:1;
 );

procedure op_vpunpcklqdq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpunpcklqdq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpunpcklwd_desc:t_op_type=(
  op:$61;index:1;mm:1;
 );

procedure op_vpunpcklwd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpunpcklwd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpunpckldq_desc:t_op_type=(
  op:$62;index:1;mm:1;
 );

procedure op_vpunpckldq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpunpckldq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vunpcklpd_desc:t_op_type=(
  op:$14;index:1;mm:1;
 );

procedure op_vunpcklpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vunpcklpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcmpps_desc:t_op_type=(
  op:$C2;index:0;mm:1;
 );

procedure op_vcmpps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vcmpps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcmppd_desc:t_op_type=(
  op:$C2;index:1;mm:1;
 );

procedure op_vcmppd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vcmppd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcmpsd_desc:t_op_type=(
  op:$C2;index:3;mm:1;
 );

procedure op_vcmpsd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vcmpsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcmpss_desc:t_op_type=(
  op:$C2;index:2;mm:1;
 );

procedure op_vcmpss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vcmpss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpshufd_desc:t_op_type=(
  op:$70;index:1;mm:1;
 );

procedure op_vpshufd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpshufd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpshufb_desc:t_op_type=(
  op:$00;index:1;mm:2;
 );

procedure op_vpshufb(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpshufb_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpermilps_rrm_desc:t_op_type=(
  op:$0C;index:1;mm:2;
 );

 vpermilps_rmi_desc:t_op_avx3_imm=(
  rmi:(op:$04;index:1;mm:3);
  mri:(opt:[not_impl]);
 );

procedure op_vpermilps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  if is_memory(ctx.din.Operand[3]) then
  begin
   op_emit_avx3(ctx,vpermilps_rrm_desc);
  end else
  if (ctx.din.Operand[3].ByteCount<>0) then
  begin
   op_emit_avx3_imm8(ctx,vpermilps_rmi_desc);
  end else
  begin
   Assert(False);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpermilpd_rrm_desc:t_op_type=(
  op:$0D;index:1;mm:2;
 );

 vpermilpd_rmi_desc:t_op_avx3_imm=(
  rmi:(op:$05;index:1;mm:3);
  mri:(opt:[not_impl]);
 );

procedure op_vpermilpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  if is_memory(ctx.din.Operand[3]) then
  begin
   op_emit_avx3(ctx,vpermilpd_rrm_desc);
  end else
  if (ctx.din.Operand[3].ByteCount<>0) then
  begin
   op_emit_avx3_imm8(ctx,vpermilpd_rmi_desc);
  end else
  begin
   Assert(False);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vperm2f128_desc:t_op_type=(
  op:$06;index:1;mm:3;opt:[not_vex_len]
 );

procedure op_vperm2f128(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vperm2f128_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpsrlw_desc:t_op_type=(
  op:$D1;index:1;mm:1;
 );

procedure op_vpsrlw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpsrlw_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpsrlq_desc:t_op_type=(
  op:$D3;index:1;mm:1;
 );

procedure op_vpsrlq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpsrlq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpminud_desc:t_op_type=(
  op:$3B;index:1;mm:2;
 );

procedure op_vpminud(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpminud_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;


const
 vmaskmovps_rrm_desc:t_op_type=(
  op:$2C;index:1;mm:2;
 );

 vmaskmovps_mrr_desc:t_op_type=(
  op:$2E;index:1;mm:2;
 );


procedure op_vmaskmovps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  if is_memory(ctx.din.Operand[3]) then
  begin
   op_emit_avx3(ctx,vmaskmovps_rrm_desc);
  end else
  begin
   op_emit_avx3(ctx,vmaskmovps_mrr_desc);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpxor_desc:t_op_type=(
  op:$EF;index:1;mm:1;
 );

procedure op_vpxor(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpxor_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vorps_desc:t_op_type=(
  op:$56;index:0;mm:1;
 );

procedure op_vorps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vorps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vorpd_desc:t_op_type=(
  op:$56;index:1;mm:1;
 );

procedure op_vorpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vorpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpor_desc:t_op_type=(
  op:$EB;index:1;mm:1;
 );

procedure op_vpor(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpor_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vandps_desc:t_op_type=(
  op:$54;index:0;mm:1;
 );

procedure op_vandps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vandps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vandpd_desc:t_op_type=(
  op:$54;index:1;mm:1;
 );

procedure op_vandpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vandpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vandnps_desc:t_op_type=(
  op:$55;index:0;mm:1;
 );

procedure op_vandnps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vandnps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vandnpd_desc:t_op_type=(
  op:$55;index:1;mm:1;
 );

procedure op_vandnpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vandnpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpand_desc:t_op_type=(
  op:$DB;index:1;mm:1;
 );

procedure op_vpand(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpand_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpandn_desc:t_op_type=(
  op:$DF;index:1;mm:1;
 );

procedure op_vpandn(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpandn_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vblendps_desc:t_op_type=(
  op:$0C;index:1;mm:3
 );

procedure op_vblendps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vblendps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vblendpd_desc:t_op_type=(
  op:$0D;index:1;mm:3
 );

procedure op_vblendpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vblendpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpblendw_desc:t_op_type=(
  op:$0E;index:1;mm:3
 );

procedure op_vpblendw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpblendw_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vblendvps_desc:t_op_type=(
  op:$4A;index:1;mm:3
 );

procedure op_vblendvps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx4(ctx,vblendvps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vblendvpd_desc:t_op_type=(
  op:$4B;index:1;mm:3
 );

procedure op_vblendvpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx4(ctx,vblendvpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcvtsi2ss_desc:t_op_type=(
  op:$2A;index:2;mm:1;
 );

procedure op_vcvtsi2ss(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vcvtsi2ss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcvtsi2sd_desc:t_op_type=(
  op:$2A;index:3;mm:1;
 );

procedure op_vcvtsi2sd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vcvtsi2sd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcvtss2sd_desc:t_op_type=(
  op:$5A;index:2;mm:1;
 );

procedure op_vcvtss2sd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vcvtss2sd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcvtsd2ss_desc:t_op_type=(
  op:$5A;index:3;mm:1;
 );

procedure op_vcvtsd2ss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vcvtsd2ss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcvttps2dq_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$5B;index:2;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_vcvttps2dq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vcvttps2dq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcvttpd2dq_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$E6;index:1;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_vcvttpd2dq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vcvttpd2dq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcvtdq2ps_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$5B;index:0;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_vcvtdq2ps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vcvtdq2ps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcvtdq2pd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$E6;index:2;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_vcvtdq2pd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vcvtdq2pd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcvttss2si_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$2C;index:2;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_vcvttss2si(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vcvttss2si_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vcvttsd2si_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$2C;index:3;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_vcvttsd2si(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vcvttsd2si_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 bextr_desc:t_op_type=(
  op:$F7;index:0;mm:2;
 );

procedure op_bextr(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_bmi_rmr(ctx,bextr_desc); //r64a, r/m64, r64b
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 andn_desc:t_op_type=(
  op:$F2;index:0;mm:2;
 );

procedure op_andn(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_bmi_rrm(ctx,andn_desc); //r64a, r64b, r/m64
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vpextrq_desc:t_op_avx3_imm=( //vpextrd_desc
  rmi:(opt:[not_impl]);
  mri:(op:$16;index:1;mm:3);
 );

procedure op_vpextrq(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx3_imm8(ctx,vpextrq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vextractps_desc:t_op_avx3_imm=(
  rmi:(opt:[not_impl]);
  mri:(op:$17;index:1;mm:3);
 );

procedure op_vextractps(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx3_imm8(ctx,vextractps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vinsertf128_desc:t_op_type=(
  op:$18;index:1;mm:3;opt:[not_vex_len]
 );

procedure op_vinsertf128(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vinsertf128_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vinsertps_desc:t_op_type=(
  op:$21;index:1;mm:3
 );

procedure op_vinsertps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vinsertps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vextractf128_desc:t_op_type=(
  op:$19;index:1;mm:3;opt:[not_vex_len]
 );

procedure op_vextractf128(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vextractf128_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vroundss_desc:t_op_type=(
  op:$0A;index:1;mm:3
 );

procedure op_vroundss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vroundss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vroundsd_desc:t_op_type=(
  op:$0B;index:1;mm:3
 );

procedure op_vroundsd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vroundsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vsqrtss_desc:t_op_type=(
  op:$51;index:2;mm:1
 );

procedure op_vsqrtss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vsqrtss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vsqrtsd_desc:t_op_type=(
  op:$51;index:3;mm:1
 );

procedure op_vsqrtsd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vsqrtsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vbroadcastss_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$18;index:1;mm:2;opt:[not_vex_len]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_vbroadcastss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vbroadcastss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vbroadcastsd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$19;index:1;mm:2;opt:[not_vex_len]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_vbroadcastsd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vbroadcastsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vbroadcastf128_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$1A;index:1;mm:2;opt:[not_vex_len]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );

procedure op_vbroadcastf128(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vbroadcastf128_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpinsrd_desc:t_op_type=(
  op:$22;index:1;mm:3
 );

procedure op_vpinsrd(var ctx:t_jit_context2); //vpinsrq
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpinsrd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpackusdw_desc:t_op_type=(
  op:$2B;index:1;mm:2
 );

procedure op_vpackusdw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpackusdw_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpackuswb_desc:t_op_type=(
  op:$67;index:1;mm:1
 );

procedure op_vpackuswb(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpackuswb_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpsrad_desc:t_op_type=(
  op:$E2;index:1;mm:1
 );

procedure op_vpsrad(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpsrad_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpslld_desc:t_op_type=(
  op:$F2;index:1;mm:1
 );

procedure op_vpslld(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpslld_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpsllq_desc:t_op_type=(
  op:$F3;index:1;mm:1
 );

procedure op_vpsllq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpsllq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmaxsd_desc:t_op_type=(
  op:$5F;index:3;mm:1
 );

procedure op_vmaxsd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vmaxsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vhaddpd_desc:t_op_type=(
  op:$7C;index:1;mm:1
 );

procedure op_vhaddpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vhaddpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vhaddps_desc:t_op_type=(
  op:$7C;index:3;mm:1
 );

procedure op_vhaddps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vhaddps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

///

const
 fldenv_desc:t_op_type=(
  op:$D9;index:4;opt:[not_prefix];
 );

procedure op_fldenv(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit1(ctx,fldenv_desc,[his_ro]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fnstenv_desc:t_op_type=(
  op:$D9;index:6;opt:[not_prefix];
 );

procedure op_fnstenv(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit1(ctx,fnstenv_desc,[]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fnstsw_desc:t_op_type=(
  op:$DD;index:7;opt:[not_prefix];
 );

procedure op_fnstsw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit1(ctx,fnstsw_desc,[]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fnstcw_desc:t_op_type=(
  op:$D9;index:7;opt:[not_prefix];
 );

procedure op_fnstcw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit1(ctx,fnstcw_desc,[]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fxsave_desc:t_op_type=(
  op:$0FAE;index:0;opt:[not_prefix];
 );

procedure op_fxsave(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit1(ctx,fxsave_desc,[]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fxrstor_desc:t_op_type=(
  op:$0FAE;index:1;opt:[not_prefix];
 );

procedure op_fxrstor(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit1(ctx,fxrstor_desc,[his_ro]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 ldmxcsr_desc:t_op_type=(
  op:$0FAE;index:2;opt:[not_prefix];
 );

procedure op_ldmxcsr(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit1(ctx,ldmxcsr_desc,[his_ro]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 stmxcsr_desc:t_op_type=(
  op:$0FAE;index:3;opt:[not_prefix];
 );

procedure op_stmxcsr(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit1(ctx,stmxcsr_desc,[]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fldcw_desc:t_op_type=(
  op:$D9;index:5;opt:[not_prefix];
 );

procedure op_fldcw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit1(ctx,fldcw_desc,[his_ro]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fld_32_desc:t_op_type=(
  op:$D9;index:0;opt:[not_prefix];
 );

 fld_64_desc:t_op_type=(
  op:$DD;index:0;opt:[not_prefix];
 );

 fld_80_desc:t_op_type=(
  op:$DB;index:5;opt:[not_prefix];
 );

procedure op_fld(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os32:op_emit1(ctx,fld_32_desc,[his_ro]);
   os64:op_emit1(ctx,fld_64_desc,[his_ro]);
   os80:op_emit1(ctx,fld_80_desc,[his_ro]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fild_16_desc:t_op_type=(
  op:$DF;index:0;opt:[not_prefix];
 );

 fild_32_desc:t_op_type=(
  op:$DB;index:0;opt:[not_prefix];
 );

 fild_64_desc:t_op_type=(
  op:$DF;index:5;opt:[not_prefix];
 );

procedure op_fild(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os16:op_emit1(ctx,fild_16_desc,[his_ro]);
   os32:op_emit1(ctx,fild_32_desc,[his_ro]);
   os64:op_emit1(ctx,fild_64_desc,[his_ro]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 fst_32_desc:t_op_type=(
  op:$D9;index:2;opt:[not_prefix];
 );

 fst_64_desc:t_op_type=(
  op:$DD;index:2;opt:[not_prefix];
 );

procedure op_fst(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os32:op_emit1(ctx,fst_32_desc,[]);
   os64:op_emit1(ctx,fst_64_desc,[]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fstp_32_desc:t_op_type=(
  op:$D9;index:3;opt:[not_prefix];
 );

 fstp_64_desc:t_op_type=(
  op:$DD;index:3;opt:[not_prefix];
 );

 fstp_80_desc:t_op_type=(
  op:$DB;index:7;opt:[not_prefix];
 );

procedure op_fstp(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os32:op_emit1(ctx,fstp_32_desc,[]);
   os64:op_emit1(ctx,fstp_64_desc,[]);
   os80:op_emit1(ctx,fstp_80_desc,[]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fisttp_16_desc:t_op_type=(
  op:$DF;index:1;opt:[not_prefix];
 );

 fisttp_32_desc:t_op_type=(
  op:$DB;index:1;opt:[not_prefix];
 );

 fisttp_64_desc:t_op_type=(
  op:$DD;index:1;opt:[not_prefix];
 );

procedure op_fisttp(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os16:op_emit1(ctx,fisttp_16_desc,[]);
   os32:op_emit1(ctx,fisttp_32_desc,[]);
   os64:op_emit1(ctx,fisttp_64_desc,[]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fadd_32_desc:t_op_type=(
  op:$D8;index:0;opt:[not_prefix];
 );

 fadd_64_desc:t_op_type=(
  op:$DC;index:0;opt:[not_prefix];
 );

procedure op_fadd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os32:op_emit1(ctx,fadd_32_desc,[his_ro]);
   os64:op_emit1(ctx,fadd_64_desc,[his_ro]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;


const
 fiadd_16_desc:t_op_type=(
  op:$DE;index:0;opt:[not_prefix];
 );

 fiadd_32_desc:t_op_type=(
  op:$DA;index:0;opt:[not_prefix];
 );

procedure op_fiadd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os16:op_emit1(ctx,fiadd_16_desc,[his_ro]);
   os32:op_emit1(ctx,fiadd_32_desc,[his_ro]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fmul_32_desc:t_op_type=(
  op:$D8;index:1;opt:[not_prefix];
 );

 fmul_64_desc:t_op_type=(
  op:$DC;index:1;opt:[not_prefix];
 );

procedure op_fmul(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os32:op_emit1(ctx,fmul_32_desc,[his_ro]);
   os64:op_emit1(ctx,fmul_64_desc,[his_ro]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fsub_32_desc:t_op_type=(
  op:$D8;index:4;opt:[not_prefix];
 );

 fsub_64_desc:t_op_type=(
  op:$DC;index:4;opt:[not_prefix];
 );

procedure op_fsub(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os32:op_emit1(ctx,fsub_32_desc,[his_ro]);
   os64:op_emit1(ctx,fsub_64_desc,[his_ro]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fsubr_32_desc:t_op_type=(
  op:$D8;index:5;opt:[not_prefix];
 );

 fsubr_64_desc:t_op_type=(
  op:$DC;index:5;opt:[not_prefix];
 );

procedure op_fsubr(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os32:op_emit1(ctx,fsubr_32_desc,[his_ro]);
   os64:op_emit1(ctx,fsubr_64_desc,[his_ro]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fdiv_32_desc:t_op_type=(
  op:$D8;index:6;opt:[not_prefix];
 );

 fdiv_64_desc:t_op_type=(
  op:$DC;index:6;opt:[not_prefix];
 );

procedure op_fdiv(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os32:op_emit1(ctx,fdiv_32_desc,[his_ro]);
   os64:op_emit1(ctx,fdiv_64_desc,[his_ro]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fdivr_32_desc:t_op_type=(
  op:$D8;index:7;opt:[not_prefix];
 );

 fdivr_64_desc:t_op_type=(
  op:$DC;index:7;opt:[not_prefix];
 );

procedure op_fdivr(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os32:op_emit1(ctx,fdivr_32_desc,[his_ro]);
   os64:op_emit1(ctx,fdivr_64_desc,[his_ro]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 prefetchnta_desc:t_op_type=(
  op:$0F18;index:0;opt:[not_prefix];
 );

procedure op_prefetchnta(var ctx:t_jit_context2);
begin
 op_emit1(ctx,prefetchnta_desc,[his_rw]);
end;

const
 prefetch_desc:t_op_type=(
  op:$0F0D;index:0;opt:[not_prefix];
 );

procedure op_prefetch(var ctx:t_jit_context2);
begin
 op_emit1(ctx,prefetch_desc,[his_rw]);
end;

const
 prefetchw_desc:t_op_type=(
  op:$0F0D;index:1;opt:[not_prefix];
 );

procedure op_prefetchw(var ctx:t_jit_context2);
begin
 op_emit1(ctx,prefetchw_desc,[his_rw]);
end;

const
 prefetch0_desc:t_op_type=(
  op:$0F18;index:1;opt:[not_prefix];
 );

procedure op_prefetch0(var ctx:t_jit_context2);
begin
 op_emit1(ctx,prefetch0_desc,[his_rw]);
end;

const
 prefetch1_desc:t_op_type=(
  op:$0F18;index:2;opt:[not_prefix];
 );

procedure op_prefetch1(var ctx:t_jit_context2);
begin
 op_emit1(ctx,prefetch1_desc,[his_rw]);
end;

const
 prefetch2_desc:t_op_type=(
  op:$0F18;index:3;opt:[not_prefix];
 );

procedure op_prefetch2(var ctx:t_jit_context2);
begin
 op_emit1(ctx,prefetch2_desc,[his_rw]);
end;


var
 inited:Integer=0;

procedure init_cbs;
begin
 if (inited<>0) then Exit;

 jit_cbs[OPPnone,OPxor ,OPSnone]:=@op_xor;
 jit_cbs[OPPnone,OPor  ,OPSnone]:=@op_or;
 jit_cbs[OPPnone,OPand ,OPSnone]:=@op_and;
 jit_cbs[OPPnone,OPsub ,OPSnone]:=@op_sub;
 jit_cbs[OPPnone,OPsbb ,OPSnone]:=@op_sbb;
 jit_cbs[OPPnone,OPadd ,OPSnone]:=@op_add;
 jit_cbs[OPPnone,OPadc ,OPSnone]:=@op_adc;

 jit_cbs[OPPnone,OPimul,OPSnone]:=@op_imul;
 jit_cbs[OPPnone,OPmul ,OPSnone]:=@op_mul;
 jit_cbs[OPPnone,OPidiv,OPSnone]:=@op_idiv;
 jit_cbs[OPPnone,OPdiv ,OPSnone]:=@op_div;

 jit_cbs[OPPnone,OPbt  ,OPSnone]:=@op_bt;
 jit_cbs[OPPnone,OPbts ,OPSnone]:=@op_bts;

 jit_cbs[OPPnone,OPxchg,OPSnone]:=@op_xchg;

 jit_cbs[OPPnone,OPmov ,OPSnone]:=@op_mov;

 jit_cbs[OPPv,OPmovu,OPSx_ps ]:=@op_vmovups;
 jit_cbs[OPPv,OPmovu,OPSx_pd ]:=@op_vmovupd;
 jit_cbs[OPPv,OPmova,OPSx_ps ]:=@op_vmovaps;
 jit_cbs[OPPv,OPmova,OPSx_pd ]:=@op_vmovapd;
 jit_cbs[OPPv,OPmov ,OPSx_dqu]:=@op_vmovdqu;
 jit_cbs[OPPv,OPmov ,OPSx_dqa]:=@op_vmovdqa;

 jit_cbs[OPPv,OPmovnt,OPSx_dqa]:=@op_vmovntdqa;
 jit_cbs[OPPv,OPmovnt,OPSx_dq ]:=@op_vmovntdq;

 jit_cbs[OPPv,OPmovddup,OPSnone]:=@op_vmovddup;

 jit_cbs[OPPv,OPmov ,OPSx_d  ]:=@op_vmovq;
 jit_cbs[OPPv,OPmov ,OPSx_q  ]:=@op_vmovq;

 jit_cbs[OPPv,OPmov ,OPSx_ss ]:=@op_vmovss;
 jit_cbs[OPPv,OPmov ,OPSx_sd ]:=@op_vmovsd;

 jit_cbs[OPPv,OPmovl,OPSx_pd]:=@op_vmovlpd;
 jit_cbs[OPPv,OPmovh,OPSx_pd]:=@op_vmovhpd;

 jit_cbs[OPPv,OPmovsldup,OPSnone]:=@op_vmovsldup;
 jit_cbs[OPPv,OPmovshdup,OPSnone]:=@op_vmovshdup;

 jit_cbs[OPPv,OPpmovsx,OPSv_bw]:=@op_vpmovsxbw;
 jit_cbs[OPPv,OPpmovsx,OPSv_dq]:=@op_vpmovsxdq;

 jit_cbs[OPPv,OPucomi,OPSx_ss]:=@op_vucomiss;
 jit_cbs[OPPv,OPucomi,OPSx_sd]:=@op_vucomisd;

 jit_cbs[OPPv,OPptest,OPSnone]:=@op_vptest;

 jit_cbs[OPPnone,OPblsr,OPSnone ]:=@op_blsr;
 jit_cbs[OPPnone,OPblsi,OPSnone ]:=@op_blsi;

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
 jit_cbs[OPPnone,OPmovsx,OPSx_d ]:=@op_movsxd;
 jit_cbs[OPPnone,OPmov  ,OPSc_be]:=@op_movbe;

 jit_cbs[OPPnone,OPtest,OPSnone]:=@op_test;
 jit_cbs[OPPnone,OPcmp ,OPSnone]:=@op_cmp;

 jit_cbs[OPPnone,OPcmpxchg,OPSnone]:=@op_cmpxchg;
 jit_cbs[OPPnone,OPxadd   ,OPSnone]:=@op_xadd;

 jit_cbs[OPPnone,OPlzcnt  ,OPSnone]:=@op_lzcnt;
 jit_cbs[OPPnone,OPtzcnt  ,OPSnone]:=@op_tzcnt;
 jit_cbs[OPPnone,OPpopcnt ,OPSnone]:=@op_popcnt;

 jit_cbs[OPPnone,OPshl ,OPSnone]:=@op_shl;
 jit_cbs[OPPnone,OPshr ,OPSnone]:=@op_shr;
 jit_cbs[OPPnone,OPsar ,OPSnone]:=@op_sar;
 jit_cbs[OPPnone,OProl ,OPSnone]:=@op_rol;

 jit_cbs[OPPnone,OPset__,OPSc_o  ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_no ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_b  ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_nb ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_z  ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_nz ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_be ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_nbe]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_s  ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_ns ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_p  ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_np ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_l  ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_nl ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_le ]:=@op_setcc;
 jit_cbs[OPPnone,OPset__,OPSc_nle]:=@op_setcc;

 jit_cbs[OPPnone,OPpxor,OPSnone]:=@op_pxor;

 jit_cbs[OPPnone,OPemms    ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPvzeroall,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPfninit  ,OPSnone]:=@add_orig;
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

 jit_cbs[OPPnone,OPfadd ,OPSx_p ]:=@add_orig;
 jit_cbs[OPPnone,OPfsub ,OPSx_p ]:=@add_orig;
 jit_cbs[OPPnone,OPfsubr,OPSx_p ]:=@add_orig;
 jit_cbs[OPPnone,OPfmul ,OPSx_p ]:=@add_orig;
 jit_cbs[OPPnone,OPfdiv ,OPSx_p ]:=@add_orig;
 jit_cbs[OPPnone,OPfdivr,OPSx_p ]:=@add_orig;
 jit_cbs[OPPnone,OPfsqrt,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfabs ,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPfwait ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfnclex,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPpause,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPlfence,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPmfence,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPsfence,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPcmc,OPSnone]:=@add_orig;

 jit_cbs[OPPv,OPxor   ,OPSx_ps]:=@op_vxorps;
 jit_cbs[OPPv,OPxor   ,OPSx_pd]:=@op_vxorpd;

 jit_cbs[OPPv,OPpcmpeq,OPSx_b ]:=@op_vpcmpeqb;
 jit_cbs[OPPv,OPpcmpeq,OPSx_w ]:=@op_vpcmpeqw;
 jit_cbs[OPPv,OPpcmpeq,OPSx_d ]:=@op_vpcmpeqd;

 jit_cbs[OPPv,OPpcmpgt,OPSx_d ]:=@op_vpcmpgtd;

 jit_cbs[OPPv,OPpcmpestri,OPSnone]:=@op_vpcmpestri;
 jit_cbs[OPPv,OPpcmpistrm,OPSnone]:=@op_vpcmpistrm;
 jit_cbs[OPPv,OPpcmpistri,OPSnone]:=@op_vpcmpistri;

 jit_cbs[OPPv,OPsub   ,OPSx_sd]:=@op_vsubsd;
 jit_cbs[OPPv,OPsub   ,OPSx_ss]:=@op_vsubss;
 jit_cbs[OPPv,OPsub   ,OPSx_ps]:=@op_vsubps;
 jit_cbs[OPPv,OPsub   ,OPSx_pd]:=@op_vsubpd;

 jit_cbs[OPPv,OPpsub  ,OPSx_w ]:=@op_vpsubw;
 jit_cbs[OPPv,OPpsub  ,OPSx_d ]:=@op_vpsubd;
 jit_cbs[OPPv,OPpsub  ,OPSx_q ]:=@op_vpsubq;

 jit_cbs[OPPv,OPpadd  ,OPSx_b ]:=@op_vpaddb;
 jit_cbs[OPPv,OPpadd  ,OPSx_w ]:=@op_vpaddw;
 jit_cbs[OPPv,OPpadd  ,OPSx_d ]:=@op_vpaddd;
 jit_cbs[OPPv,OPpadd  ,OPSx_q ]:=@op_vpaddq;

 jit_cbs[OPPv,OPadd   ,OPSx_ps]:=@op_vaddps;
 jit_cbs[OPPv,OPadd   ,OPSx_pd]:=@op_vaddpd;
 jit_cbs[OPPv,OPadd   ,OPSx_ss]:=@op_vaddss;
 jit_cbs[OPPv,OPadd   ,OPSx_sd]:=@op_vaddsd;

 jit_cbs[OPPv,OPphadd ,OPSx_w]:=@op_vphaddw;
 jit_cbs[OPPv,OPphadd ,OPSx_d]:=@op_vphaddd;

 jit_cbs[OPPv,OPdiv   ,OPSx_ps]:=@op_vdivps;
 jit_cbs[OPPv,OPdiv   ,OPSx_pd]:=@op_vdivpd;
 jit_cbs[OPPv,OPdiv   ,OPSx_ss]:=@op_vdivss;
 jit_cbs[OPPv,OPdiv   ,OPSx_sd]:=@op_vdivsd;

 jit_cbs[OPPv,OPmul   ,OPSx_ps]:=@op_vmulps;
 jit_cbs[OPPv,OPmul   ,OPSx_pd]:=@op_vmulpd;
 jit_cbs[OPPv,OPmul   ,OPSx_ss]:=@op_vmulss;
 jit_cbs[OPPv,OPmul   ,OPSx_sd]:=@op_vmulsd;

 jit_cbs[OPPv,OPpmull ,OPSx_w]:=@op_vpmullw;

 jit_cbs[OPPv,OPpmulhuw,OPSnone]:=@op_vpmulhuw;

 jit_cbs[OPPv,OPpunpcklqdq,OPSnone]:=@op_vpunpcklqdq;
 jit_cbs[OPPv,OPpunpcklwd ,OPSnone]:=@op_vpunpcklwd;
 jit_cbs[OPPv,OPpunpckldq ,OPSnone]:=@op_vpunpckldq;

 jit_cbs[OPPv,OPunpckl    ,OPSx_pd]:=@op_vunpcklpd;

 jit_cbs[OPPv,OPcmp   ,OPSx_ps]:=@op_vcmpps;
 jit_cbs[OPPv,OPcmp   ,OPSx_pd]:=@op_vcmppd;
 jit_cbs[OPPv,OPcmp   ,OPSx_sd]:=@op_vcmpsd;
 jit_cbs[OPPv,OPcmp   ,OPSx_ss]:=@op_vcmpss;

 jit_cbs[OPPv,OPpshuf ,OPSx_d ]:=@op_vpshufd;
 jit_cbs[OPPv,OPpshuf ,OPSx_b ]:=@op_vpshufb;

 jit_cbs[OPPnone,OPvpermil,OPSx_ps]:=@op_vpermilps;
 jit_cbs[OPPnone,OPvpermil,OPSx_pd]:=@op_vpermilpd;

 jit_cbs[OPPnone,OPvperm2,OPSx_f128]:=@op_vperm2f128;

 jit_cbs[OPPv,OPpsrl,OPSx_w]:=@op_vpsrlw;
 jit_cbs[OPPv,OPpsrl,OPSx_q]:=@op_vpsrlq;

 jit_cbs[OPPv,OPpminu  ,OPSx_d ]:=@op_vpminud;
 jit_cbs[OPPv,OPmaskmov,OPSx_ps]:=@op_vmaskmovps;
 jit_cbs[OPPv,OPpxor   ,OPSnone]:=@op_vpxor;
 jit_cbs[OPPv,OPor     ,OPSx_ps]:=@op_vorps;
 jit_cbs[OPPv,OPor     ,OPSx_pd]:=@op_vorpd;
 jit_cbs[OPPv,OPpor    ,OPSnone]:=@op_vpor;
 jit_cbs[OPPv,OPand    ,OPSx_ps]:=@op_vandps;
 jit_cbs[OPPv,OPand    ,OPSx_pd]:=@op_vandpd;
 jit_cbs[OPPv,OPandn   ,OPSx_ps]:=@op_vandnps;
 jit_cbs[OPPv,OPandn   ,OPSx_pd]:=@op_vandnpd;
 jit_cbs[OPPv,OPpand   ,OPSnone]:=@op_vpand;
 jit_cbs[OPPv,OPpandn  ,OPSnone]:=@op_vpandn;

 jit_cbs[OPPv,OPblend  ,OPSx_ps]:=@op_vblendps;
 jit_cbs[OPPv,OPblend  ,OPSx_pd]:=@op_vblendpd;
 jit_cbs[OPPv,OPpblend ,OPSx_w ]:=@op_vpblendw;

 jit_cbs[OPPv,OPblendv ,OPSx_ps]:=@op_vblendvps;
 jit_cbs[OPPv,OPblendv ,OPSx_pd]:=@op_vblendvpd;

 jit_cbs[OPPv,OPcvtsi2 ,OPSx_ss]:=@op_vcvtsi2ss;
 jit_cbs[OPPv,OPcvtsi2 ,OPSx_sd]:=@op_vcvtsi2sd;
 jit_cbs[OPPv,OPcvtss2 ,OPSx_sd]:=@op_vcvtss2sd;
 jit_cbs[OPPv,OPcvtsd2 ,OPSx_ss]:=@op_vcvtsd2ss;
 jit_cbs[OPPv,OPcvttps2,OPSx_dq]:=@op_vcvttps2dq;
 jit_cbs[OPPv,OPcvttpd2,OPSx_dq]:=@op_vcvttpd2dq;
 jit_cbs[OPPv,OPcvtdq2 ,OPSx_ps]:=@op_vcvtdq2ps;
 jit_cbs[OPPv,OPcvtdq2 ,OPSx_pd]:=@op_vcvtdq2pd;

 jit_cbs[OPPv,OPcvttss2,OPSx_si]:=@op_vcvttss2si;
 jit_cbs[OPPv,OPcvttsd2,OPSx_si]:=@op_vcvttsd2si;

 jit_cbs[OPPnone,OPbextr,OPSnone]:=@op_bextr;
 jit_cbs[OPPnone,OPandn ,OPSnone]:=@op_andn;

 jit_cbs[OPPv,OPpextr,OPSx_d]:=@op_vpextrq;
 jit_cbs[OPPv,OPpextr,OPSx_q]:=@op_vpextrq;

 jit_cbs[OPPv,OPextract,OPSx_ps]:=@op_vextractps;

 jit_cbs[OPPv,OPinsert ,OPSx_f128]:=@op_vinsertf128;
 jit_cbs[OPPv,OPinsert ,OPSx_ps  ]:=@op_vinsertps;

 jit_cbs[OPPv,OPextract,OPSx_f128]:=@op_vextractf128;

 jit_cbs[OPPv,OPround,OPSx_ss]:=@op_vroundss;
 jit_cbs[OPPv,OPround,OPSx_sd]:=@op_vroundsd;

 jit_cbs[OPPv,OPsqrt ,OPSx_sd]:=@op_vsqrtsd;
 jit_cbs[OPPv,OPsqrt ,OPSx_ss]:=@op_vsqrtss;

 jit_cbs[OPPnone,OPvbroadcast,OPSx_ss  ]:=@op_vbroadcastss;
 jit_cbs[OPPnone,OPvbroadcast,OPSx_sd  ]:=@op_vbroadcastsd;
 jit_cbs[OPPnone,OPvbroadcast,OPSx_f128]:=@op_vbroadcastf128;

 jit_cbs[OPPv,OPpinsr,OPSx_d]:=@op_vpinsrd;
 jit_cbs[OPPv,OPpinsr,OPSx_q]:=@op_vpinsrd;

 jit_cbs[OPPv,OPpackusdw,OPSnone]:=@op_vpackusdw;
 jit_cbs[OPPv,OPpackuswb,OPSnone]:=@op_vpackuswb;

 jit_cbs[OPPv,OPpsra,OPSx_d]:=@op_vpsrad;
 jit_cbs[OPPv,OPpsll,OPSx_d]:=@op_vpslld;
 jit_cbs[OPPv,OPpsll,OPSx_q]:=@op_vpsllq;

 jit_cbs[OPPv,OPmax,OPSx_sd]:=@op_vmaxsd;

 jit_cbs[OPPv,OPhadd,OPSx_pd]:=@op_vhaddpd;
 jit_cbs[OPPv,OPhadd,OPSx_ps]:=@op_vhaddps;

 jit_cbs[OPPnone,OPcwd ,OPSnone]:=@op_cdq;
 jit_cbs[OPPnone,OPcdq ,OPSnone]:=@op_cdq;
 jit_cbs[OPPnone,OPcqo ,OPSnone]:=@op_cdq;

 jit_cbs[OPPnone,OPcbw ,OPSnone]:=@op_cdq;
 jit_cbs[OPPnone,OPcwde,OPSnone]:=@op_cdq;
 jit_cbs[OPPnone,OPcdqe,OPSnone]:=@op_cdq;

 jit_cbs[OPPnone,OPlea,OPSnone]:=@op_lea;

 jit_cbs[OPPnone,OPinc,OPSnone]:=@op_inc;
 jit_cbs[OPPnone,OPdec,OPSnone]:=@op_dec;
 jit_cbs[OPPnone,OPneg,OPSnone]:=@op_neg;
 jit_cbs[OPPnone,OPnot,OPSnone]:=@op_not;
 jit_cbs[OPPnone,OPbswap,OPSnone]:=@op_bswap;

 jit_cbs[OPPnone,OPfldcw ,OPSnone]:=@op_fldcw;
 jit_cbs[OPPnone,OPfld   ,OPSnone]:=@op_fld;
 jit_cbs[OPPnone,OPfild  ,OPSnone]:=@op_fild;

 jit_cbs[OPPnone,OPfldenv ,OPSnone]:=@op_fldenv;
 jit_cbs[OPPnone,OPfnstenv,OPSnone]:=@op_fnstenv;
 jit_cbs[OPPnone,OPfnstcw ,OPSnone]:=@op_fnstcw;
 jit_cbs[OPPnone,OPfnstsw ,OPSnone]:=@op_fnstsw;

 jit_cbs[OPPnone,OPfxsave ,OPSnone]:=@op_fxsave;
 jit_cbs[OPPnone,OPfxrstor,OPSnone]:=@op_fxrstor;
 jit_cbs[OPPnone,OPldmxcsr,OPSnone]:=@op_ldmxcsr;
 jit_cbs[OPPnone,OPstmxcsr,OPSnone]:=@op_stmxcsr;
 jit_cbs[OPPnone,OPfst    ,OPSnone]:=@op_fst;
 jit_cbs[OPPnone,OPfst    ,OPSx_p ]:=@op_fstp;
 jit_cbs[OPPnone,OPfisttp ,OPSnone]:=@op_fisttp;
 jit_cbs[OPPnone,OPfadd   ,OPSnone]:=@op_fadd;
 jit_cbs[OPPnone,OPfiadd  ,OPSnone]:=@op_fiadd;
 jit_cbs[OPPnone,OPfmul   ,OPSnone]:=@op_fmul;
 jit_cbs[OPPnone,OPfsub   ,OPSnone]:=@op_fsub;
 jit_cbs[OPPnone,OPfsubr  ,OPSnone]:=@op_fsubr;
 jit_cbs[OPPnone,OPfdiv   ,OPSnone]:=@op_fdiv;
 jit_cbs[OPPnone,OPfdivr  ,OPSnone]:=@op_fdivr;

 jit_cbs[OPPnone,OPprefetch,OPSnone ]:=@op_prefetch;
 jit_cbs[OPPnone,OPprefetch,OPSp_w  ]:=@op_prefetchw;
 jit_cbs[OPPnone,OPprefetch,OPSp_nta]:=@op_prefetchnta;
 jit_cbs[OPPnone,OPprefetch,OPSp_t0 ]:=@op_prefetch0;
 jit_cbs[OPPnone,OPprefetch,OPSp_t1 ]:=@op_prefetch1;
 jit_cbs[OPPnone,OPprefetch,OPSp_t2 ]:=@op_prefetch2;

 inited:=1;
end;


end.

