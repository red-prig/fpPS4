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
  hint:[his_xor];
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
  hint:[];
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
  reg_mem:(op:$33;index:0);
  reg_imm:(op:$81;index:4);
  reg_im8:(op:$83;index:4);
  hint:[his_xor];
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
  hint:[];
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
  hint:[];
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
  hint:[];
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
  hint:[];
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
   3:op_emit2(ctx,imul_desc2); //RMI
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
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
  add_orig(ctx);
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
  add_orig(ctx);
 end;
end;

const
 bt_desc:t_op_desc=(
  mem_reg:(op:$0FA3;index:0);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(op:$0FBA;index:4);
  hint:[];
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
  hint:[];
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
  hint:[his_mov];
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
  hint:[his_mov];
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
  hint:[his_mov];
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
  hint:[his_mov];
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
  hint:[his_mov];
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
 vmovaps_desc:t_op_desc=(
  mem_reg:(op:$29;index:0;mm:1);
  reg_mem:(op:$28;index:0;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_align];
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
 vmovdqu_desc:t_op_desc=(
  mem_reg:(op:$7F;index:2;mm:1);
  reg_mem:(op:$6F;index:2;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov];
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
  hint:[his_mov,his_align];
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
 vmovq_desc:t_op_desc=( //vmovd_desc
  mem_reg:(op:$7E;index:1;mm:1);
  reg_mem:(op:$6E;index:1;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov];
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
 vptest_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$17;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov];
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
  hint:[his_cmp];
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
  hint:[his_cmp];
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
  add_orig(ctx);
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

procedure op_cdqe(var ctx:t_jit_context2);
var
 i:Integer;
begin
 with ctx.builder do
 begin
  i:=GetFrameOffset(rax);
  movq(r_tmp0,[r_thrd+i]);

  add_orig(ctx);

  movq([r_thrd+i],r_tmp0);
 end;
end;

const
 pxor_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$0FEF;index:0);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_xor];
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
  op:$57;index:1;mm:1;
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
 vmaskmovps_desc:t_op_type=(
  op:$2E;index:1;mm:2;
 );

procedure op_vmaskmovps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vmaskmovps_desc);
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
 vpextrq_desc:t_op_type=(
  op:$16;index:1;mm:3;
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
 vinsertf128_desc:t_op_type=(
  op:$18;index:1;mm:3;opt:[not_vex_len]
 );

procedure op_vinsertf128(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx3_imm8(ctx,vinsertf128_desc);
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
 fldcw_desc:t_op_type=(
  op:$D9;index:5;opt:[not_prefix];
 );

procedure op_fldcw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit1(ctx,fldcw_desc,[his_rd]);
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
  op_emit1(ctx,fxrstor_desc,[his_rd]);
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
   os32:op_emit1(ctx,fld_32_desc,[his_rd]);
   os64:op_emit1(ctx,fld_64_desc,[his_rd]);
   os80:op_emit1(ctx,fld_80_desc,[his_rd]);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

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
 jit_cbs[OPPnone,OPdiv ,OPSnone]:=@op_div;

 jit_cbs[OPPnone,OPbt  ,OPSnone]:=@op_bt;
 jit_cbs[OPPnone,OPbts ,OPSnone]:=@op_bts;

 jit_cbs[OPPnone,OPxchg,OPSnone]:=@op_xchg;

 jit_cbs[OPPnone,OPmov ,OPSnone]:=@op_mov;

 jit_cbs[OPPv,OPmovu,OPSx_ps ]:=@op_vmovups;
 jit_cbs[OPPv,OPmova,OPSx_ps ]:=@op_vmovaps;
 jit_cbs[OPPv,OPmov ,OPSx_dqu]:=@op_vmovdqu;
 jit_cbs[OPPv,OPmov ,OPSx_dqa]:=@op_vmovdqa;

 jit_cbs[OPPv,OPmov ,OPSx_d  ]:=@op_vmovq;
 jit_cbs[OPPv,OPmov ,OPSx_q  ]:=@op_vmovq;

 jit_cbs[OPPv,OPptest,OPSnone]:=@op_vptest;

 jit_cbs[OPPnone,OPblsr,OPSnone ]:=@op_blsr;

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

 jit_cbs[OPPnone,OPemms,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPvzeroall,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfninit,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPrdtsc ,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPpause ,OPSnone]:=@add_orig;

 jit_cbs[OPPv,OPxor   ,OPSx_ps]:=@op_vxorps;
 jit_cbs[OPPv,OPpcmpeq,OPSx_d ]:=@op_vpcmpeqd;
 jit_cbs[OPPv,OPpsub  ,OPSx_q ]:=@op_vpsubq;
 jit_cbs[OPPv,OPpadd  ,OPSx_d ]:=@op_vpaddd;
 jit_cbs[OPPv,OPpadd  ,OPSx_q ]:=@op_vpaddq;
 jit_cbs[OPPv,OPpunpcklqdq,OPSnone]:=@op_vpunpcklqdq;

 jit_cbs[OPPv,OPcmp   ,OPSx_ps]:=@op_vcmpps;
 jit_cbs[OPPv,OPcmp   ,OPSx_pd]:=@op_vcmppd;

 jit_cbs[OPPv,OPpshuf ,OPSx_d ]:=@op_vpshufd;

 jit_cbs[OPPv,OPpminu  ,OPSx_d ]:=@op_vpminud;
 jit_cbs[OPPv,OPmaskmov,OPSx_ps]:=@op_vmaskmovps;
 jit_cbs[OPPv,OPpxor   ,OPSnone]:=@op_vpxor;
 jit_cbs[OPPv,OPpor    ,OPSnone]:=@op_vpor;

 jit_cbs[OPPnone,OPbextr,OPSnone]:=@op_bextr;
 jit_cbs[OPPnone,OPandn ,OPSnone]:=@op_andn;

 jit_cbs[OPPv,OPpextr ,OPSx_q ]:=@op_vpextrq;
 jit_cbs[OPPv,OPinsert,OPSx_f128]:=@op_vinsertf128;

 jit_cbs[OPPnone,OPcbw ,OPSnone]:=@op_cdqe;
 jit_cbs[OPPnone,OPcwde,OPSnone]:=@op_cdqe;
 jit_cbs[OPPnone,OPcdqe,OPSnone]:=@op_cdqe;

 jit_cbs[OPPnone,OPlea,OPSnone]:=@op_lea;

 jit_cbs[OPPnone,OPinc,OPSnone]:=@op_inc;
 jit_cbs[OPPnone,OPdec,OPSnone]:=@op_dec;
 jit_cbs[OPPnone,OPneg,OPSnone]:=@op_neg;
 jit_cbs[OPPnone,OPnot,OPSnone]:=@op_not;
 jit_cbs[OPPnone,OPbswap,OPSnone]:=@op_bswap;

 jit_cbs[OPPnone,OPfnstcw,OPSnone]:=@op_fnstcw;
 jit_cbs[OPPnone,OPfldcw ,OPSnone]:=@op_fldcw;

 jit_cbs[OPPnone,OPfxsave ,OPSnone]:=@op_fxsave;
 jit_cbs[OPPnone,OPfxrstor,OPSnone]:=@op_fxrstor;
 jit_cbs[OPPnone,OPfld    ,OPSnone]:=@op_fld;
 jit_cbs[OPPnone,OPfst    ,OPSnone]:=@op_fst;
 jit_cbs[OPPnone,OPfst    ,OPSx_p ]:=@op_fstp;

 inited:=1;
end;


end.

