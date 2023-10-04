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
 movsd_desc:t_op_desc=(
  mem_reg:(op:$F20F11;opt:[not_prefix]);
  reg_mem:(op:$F20F10;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_movsd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,movsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 movss_desc:t_op_desc=(
  mem_reg:(op:$F30F11;opt:[not_prefix]);
  reg_mem:(op:$F30F10;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_movss(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,movss_desc);
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
 movd_desc:t_op_desc=( //movq
  mem_reg:(op:$0F7E;index:0);
  reg_mem:(op:$0F6E;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

const
 movd_xmm_desc:t_op_desc=( //movq_xmm
  mem_reg:(op:$660F7E;index:0);
  reg_mem:(op:$660F6E;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_movd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  if is_xmm(ctx.din) then
  begin
   op_emit2(ctx,movd_xmm_desc);
  end else
  begin
   op_emit2(ctx,movd_desc);
  end;
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
  desc.opt:=[not_prefix];
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
 cmpxchg16b_desc:t_op_type=(op:$0FC7;index:1);

procedure op_cmpxchg16b(var ctx:t_jit_context2);
begin
 op_emit1(ctx,cmpxchg16b_desc,[his_xchg,his_rax,his_rw]);
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
 rol_desc:t_op_shift=(
  reg_im8:(op:$C1;index:0);
  mem__cl:(op:$D3;index:0);
  mem_one:(op:$D1;index:0);
 );

procedure op_rol(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_shift2(ctx,rol_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 ror_desc:t_op_shift=(
  reg_im8:(op:$C1;index:1);
  mem__cl:(op:$D3;index:1);
  mem_one:(op:$D1;index:1);
 );

procedure op_ror(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_shift2(ctx,ror_desc);
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
  op_emit_shift2(ctx,shl_desc);
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
  op_emit_shift2(ctx,shr_desc);
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
  op_emit_shift2(ctx,sar_desc);
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
 new1,new2:TRegValue;
 i:Integer;
begin
 if is_preserved(ctx.din) then
 begin
  if is_preserved(ctx.din.Operand[1]) then
  begin
   new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
   build_lea(ctx,2,new1,[not_use_segment]);
   //
   i:=GetFrameOffset(ctx.din.Operand[1].RegValue[0]);
   ctx.builder.movq([r_thrd+i],new1);
  end else
  begin
   new1:=new_reg(ctx.din.Operand[1]);
   build_lea(ctx,2,new1);
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

//

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
 if is_preserved(ctx.din) or is_memory(ctx.din) then
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
 if is_preserved(ctx.din) or is_memory(ctx.din) then
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
 fist_16_desc:t_op_type=(
  op:$DF;index:2;opt:[not_prefix];
 );

 fist_32_desc:t_op_type=(
  op:$DB;index:2;opt:[not_prefix];
 );

procedure op_fist(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os16:op_emit1(ctx,fist_16_desc,[]);
   os32:op_emit1(ctx,fist_32_desc,[]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fistp_16_desc:t_op_type=(
  op:$DF;index:3;opt:[not_prefix];
 );

 fistp_32_desc:t_op_type=(
  op:$DB;index:3;opt:[not_prefix];
 );

 fistp_64_desc:t_op_type=(
  op:$DF;index:7;opt:[not_prefix];
 );

procedure op_fistp(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os16:op_emit1(ctx,fistp_16_desc,[]);
   os32:op_emit1(ctx,fistp_32_desc,[]);
   os64:op_emit1(ctx,fistp_64_desc,[]);
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
 fisub_16_desc:t_op_type=(
  op:$DE;index:4;opt:[not_prefix];
 );

 fisub_32_desc:t_op_type=(
  op:$DA;index:4;opt:[not_prefix];
 );

procedure op_fisub(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  case ctx.din.Operand[1].Size of
   os32:op_emit1(ctx,fisub_16_desc,[his_ro]);
   os64:op_emit1(ctx,fisub_32_desc,[his_ro]);
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

//

const
 orps_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$0F56;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_rw];
 );

procedure op_orps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2(ctx,orps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 orpd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$660F56;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_rw];
 );

procedure op_orpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2(ctx,orpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 pxor_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$0FEF;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_xor,his_rw];
 );

const
 pxor_xmm_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$660FEF;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_xor,his_rw];
 );

procedure op_pxor(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  if is_xmm(ctx.din) then
  begin
   op_emit2(ctx,pxor_xmm_desc);
  end else
  begin
   op_emit2(ctx,pxor_desc);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 addsd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F20F58;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_rw];
 );

procedure op_addsd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,addsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 addss_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F30F58;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_rw];
 );

procedure op_addss(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,addss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 subsd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F20F5C;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_rw];
 );

procedure op_subsd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,subsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 subss_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F30F5C;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_rw];
 );

procedure op_subss(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,subss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 mulsd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F20F59;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_rw];
 );

procedure op_mulsd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,mulsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 mulss_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F30F59;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_rw];
 );

procedure op_mulss(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,mulss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 cvtsi2ss_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F30F2A);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
 );

procedure op_cvtsi2ss(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,cvtsi2ss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 cvtsd2si_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F20F2D);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
 );

procedure op_cvtsd2si(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,cvtsd2si_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 cvtss2si_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F30F2D);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
 );

procedure op_cvtss2s(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,cvtss2si_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 sqrtsd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F20F51;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
 );

procedure op_sqrtsd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2(ctx,sqrtsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 sqrtss_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$F30F51;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
 );

procedure op_sqrtss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2(ctx,sqrtss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

procedure init_cbs;
begin
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

 jit_cbs[OPPnone,OPmov  ,OPSx_sd]:=@op_movsd;
 jit_cbs[OPPnone,OPmov  ,OPSx_ss]:=@op_movss;
 jit_cbs[OPPnone,OPmov  ,OPSc_be]:=@op_movbe;

 jit_cbs[OPPnone,OPmov  ,OPSx_d ]:=@op_movd;
 jit_cbs[OPPnone,OPmov  ,OPSx_q ]:=@op_movd;

 jit_cbs[OPPnone,OPmovsx,OPSx_d ]:=@op_movsxd;

 jit_cbs[OPPnone,OPtest,OPSnone]:=@op_test;
 jit_cbs[OPPnone,OPcmp ,OPSnone]:=@op_cmp;

 jit_cbs[OPPnone,OPcmpxchg,OPSnone ]:=@op_cmpxchg;
 jit_cbs[OPPnone,OPcmpxchg,OPSx_16b]:=@op_cmpxchg16b;

 jit_cbs[OPPnone,OPxadd   ,OPSnone]:=@op_xadd;

 jit_cbs[OPPnone,OPlzcnt  ,OPSnone]:=@op_lzcnt;
 jit_cbs[OPPnone,OPtzcnt  ,OPSnone]:=@op_tzcnt;
 jit_cbs[OPPnone,OPpopcnt ,OPSnone]:=@op_popcnt;

 jit_cbs[OPPnone,OProl ,OPSnone]:=@op_rol;
 jit_cbs[OPPnone,OPror ,OPSnone]:=@op_ror;
 jit_cbs[OPPnone,OPshl ,OPSnone]:=@op_shl;
 jit_cbs[OPPnone,OPshr ,OPSnone]:=@op_shr;
 jit_cbs[OPPnone,OPsar ,OPSnone]:=@op_sar;

 jit_cbs[OPPnone,OPshl ,OPSx_d ]:=@op_shld;
 jit_cbs[OPPnone,OPshr ,OPSx_d ]:=@op_shrd;

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

 jit_cbs[OPPnone,OPemms    ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPvzeroall,OPSnone]:=@add_orig;

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

 jit_cbs[OPPnone,OPpause,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPlfence,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPmfence,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPsfence,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPcmc,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPclac,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPclc ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPcld ,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPstc ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPstd ,OPSnone]:=@add_orig;

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

 //fpu

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
 jit_cbs[OPPnone,OPfist   ,OPSnone]:=@op_fist;
 jit_cbs[OPPnone,OPfist   ,OPSx_p ]:=@op_fistp;
 jit_cbs[OPPnone,OPfisttp ,OPSnone]:=@op_fisttp;
 jit_cbs[OPPnone,OPfadd   ,OPSnone]:=@op_fadd;
 jit_cbs[OPPnone,OPfiadd  ,OPSnone]:=@op_fiadd;
 jit_cbs[OPPnone,OPfmul   ,OPSnone]:=@op_fmul;
 jit_cbs[OPPnone,OPfsub   ,OPSnone]:=@op_fsub;
 jit_cbs[OPPnone,OPfsubr  ,OPSnone]:=@op_fsubr;
 jit_cbs[OPPnone,OPfisub  ,OPSnone]:=@op_fisub;
 jit_cbs[OPPnone,OPfdiv   ,OPSnone]:=@op_fdiv;
 jit_cbs[OPPnone,OPfdivr  ,OPSnone]:=@op_fdivr;

 //fpu

 jit_cbs[OPPnone,OPprefetch,OPSnone ]:=@op_prefetch;
 jit_cbs[OPPnone,OPprefetch,OPSp_w  ]:=@op_prefetchw;
 jit_cbs[OPPnone,OPprefetch,OPSp_nta]:=@op_prefetchnta;
 jit_cbs[OPPnone,OPprefetch,OPSp_t0 ]:=@op_prefetch0;
 jit_cbs[OPPnone,OPprefetch,OPSp_t1 ]:=@op_prefetch1;
 jit_cbs[OPPnone,OPprefetch,OPSp_t2 ]:=@op_prefetch2;

 //sse

 jit_cbs[OPPnone,OPor,OPSx_ps]:=@op_orps;
 jit_cbs[OPPnone,OPor,OPSx_pd]:=@op_orpd;

 jit_cbs[OPPnone,OPpxor,OPSnone]:=@op_pxor;

 jit_cbs[OPPnone,OPadd ,OPSx_sd]:=@op_addsd;
 jit_cbs[OPPnone,OPadd ,OPSx_ss]:=@op_addss;
 jit_cbs[OPPnone,OPsub ,OPSx_sd]:=@op_subsd;
 jit_cbs[OPPnone,OPsub ,OPSx_ss]:=@op_subss;
 jit_cbs[OPPnone,OPmul ,OPSx_sd]:=@op_mulsd;
 jit_cbs[OPPnone,OPmul ,OPSx_ss]:=@op_mulss;

 jit_cbs[OPPnone,OPcvtsi2,OPSx_ss]:=@op_cvtsi2ss;
 jit_cbs[OPPnone,OPcvtsd2,OPSx_si]:=@op_cvtsd2si;
 jit_cbs[OPPnone,OPcvtss2,OPSx_si]:=@op_cvtss2s;

 jit_cbs[OPPnone,OPsqrt,OPSx_sd]:=@op_sqrtsd;
 jit_cbs[OPPnone,OPsqrt,OPSx_ss]:=@op_sqrtss;

 //sse
end;

initialization
 init_cbs;

end.

