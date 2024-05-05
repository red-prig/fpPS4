unit kern_jit_ops_avx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

implementation

uses
 x86_fpdbgdisas,
 x86_jit,
 kern_jit_ops,
 kern_jit_ctx;

///verif

procedure op_emit_avx1(var ctx:t_jit_context2;const desc:t_op_type;hint:t_op_hint);
var
 tmp:t_op_type;
begin
 tmp:=desc;

 tmp.opt   :=tmp.opt+[verif_vex_len,verif_rexw];
 tmp.vx_len:=ctx.dis.Vex.Length;
 tmp.rexw  :=rexW in ctx.dis.Flags;

 kern_jit_ctx.op_emit_avx1(ctx,tmp,hint);
end;

procedure op_emit_avx2_rr(var ctx:t_jit_context2;const desc:t_op_type);
var
 tmp:t_op_type;
begin
 tmp:=desc;

 tmp.opt   :=tmp.opt+[verif_vex_len,verif_rexw];
 tmp.vx_len:=ctx.dis.Vex.Length;
 tmp.rexw  :=rexW in ctx.dis.Flags;

 kern_jit_ctx.op_emit_avx2_rr(ctx,tmp);
end;

procedure op_emit_avx2(var ctx:t_jit_context2;const desc:t_op_desc);
var
 tmp:t_op_desc;
begin
 tmp:=desc;

 tmp.mem_reg.opt   :=tmp.mem_reg.opt+[verif_vex_len,verif_rexw];
 tmp.mem_reg.vx_len:=ctx.dis.Vex.Length;
 tmp.mem_reg.rexw  :=rexW in ctx.dis.Flags;

 tmp.reg_mem.opt   :=tmp.reg_mem.opt+[verif_vex_len,verif_rexw];
 tmp.reg_mem.vx_len:=ctx.dis.Vex.Length;
 tmp.reg_mem.rexw  :=rexW in ctx.dis.Flags;

 tmp.reg_imm.opt   :=tmp.reg_imm.opt+[verif_vex_len,verif_rexw];
 tmp.reg_imm.vx_len:=ctx.dis.Vex.Length;
 tmp.reg_imm.rexw  :=rexW in ctx.dis.Flags;

 tmp.reg_im8.opt   :=tmp.reg_im8.opt+[verif_vex_len,verif_rexw];
 tmp.reg_im8.vx_len:=ctx.dis.Vex.Length;
 tmp.reg_im8.rexw  :=rexW in ctx.dis.Flags;

 kern_jit_ctx.op_emit_avx2(ctx,tmp);
end;

procedure op_emit_avx3(var ctx:t_jit_context2;const desc:t_op_type);
var
 tmp:t_op_type;
begin
 tmp:=desc;

 tmp.opt   :=tmp.opt+[verif_vex_len,verif_rexw];
 tmp.vx_len:=ctx.dis.Vex.Length;
 tmp.rexw  :=rexW in ctx.dis.Flags;

 kern_jit_ctx.op_emit_avx3(ctx,tmp);
end;

procedure op_emit_avx3_imm8(var ctx:t_jit_context2;const desc:t_op_avx3_imm);
var
 tmp:t_op_avx3_imm;
begin
 tmp:=desc;

 tmp.rmi.opt   :=tmp.rmi.opt+[verif_vex_len,verif_rexw];
 tmp.rmi.vx_len:=ctx.dis.Vex.Length;
 tmp.rmi.rexw  :=rexW in ctx.dis.Flags;

 tmp.mri.opt   :=tmp.mri.opt+[verif_vex_len,verif_rexw];
 tmp.mri.vx_len:=ctx.dis.Vex.Length;
 tmp.mri.rexw  :=rexW in ctx.dis.Flags;

 kern_jit_ctx.op_emit_avx3_imm8(ctx,tmp);
end;

procedure op_emit_avx_F3(var ctx:t_jit_context2;const desc:t_op_type);
var
 tmp:t_op_type;
begin
 tmp:=desc;

 tmp.opt   :=tmp.opt+[verif_vex_len,verif_rexw];
 tmp.vx_len:=ctx.dis.Vex.Length;
 tmp.rexw  :=rexW in ctx.dis.Flags;

 kern_jit_ctx.op_emit_avx_F3(ctx,tmp);
end;

procedure op_emit_avx4(var ctx:t_jit_context2;const desc:t_op_type);
var
 tmp:t_op_type;
begin
 tmp:=desc;

 tmp.opt   :=tmp.opt+[verif_vex_len,verif_rexw];
 tmp.vx_len:=ctx.dis.Vex.Length;
 tmp.rexw  :=rexW in ctx.dis.Flags;

 kern_jit_ctx.op_emit_avx4(ctx,tmp);
end;

procedure op_emit_bmi_rmr(var ctx:t_jit_context2;const desc:t_op_type);
var
 tmp:t_op_type;
begin
 tmp:=desc;

 tmp.opt   :=tmp.opt+[verif_vex_len,verif_rexw];
 tmp.vx_len:=ctx.dis.Vex.Length;
 tmp.rexw  :=rexW in ctx.dis.Flags;

 kern_jit_ctx.op_emit_bmi_rmr(ctx,tmp);
end;

procedure op_emit_bmi_rrm(var ctx:t_jit_context2;const desc:t_op_type);
var
 tmp:t_op_type;
begin
 tmp:=desc;

 tmp.opt   :=tmp.opt+[verif_vex_len,verif_rexw];
 tmp.vx_len:=ctx.dis.Vex.Length;
 tmp.rexw  :=rexW in ctx.dis.Flags;

 kern_jit_ctx.op_emit_bmi_rrm(ctx,tmp);
end;

///verif

function get_vw_mode(var ctx:t_jit_context2):t_vw_mode;
begin
 Result:=vwZero;
 case ctx.din.OpCode.Opcode of

  OPcvtsi2:Result:=vwM64;

  OPcvtss2:
   case ctx.din.OpCode.Suffix of
    OPSx_si:Result:=vwR64;
    else;
   end;

  OPcvtsd2:
   case ctx.din.OpCode.Suffix of
    OPSx_si:Result:=vwR64;
    else;
   end;

  OPcvttss2:Result:=vwR64;
  OPcvttsd2:Result:=vwR64;

  OPpextr:
    case ctx.din.OpCode.Suffix of
     OPSx_q:Result:=vwOne;
     else;
    end;

  OPpinsr:
    case ctx.din.OpCode.Suffix of
     OPSx_q:Result:=vwOne;
     else;
    end;

  OPvperm:
    case ctx.din.OpCode.Suffix of
     OPSx_q :Result:=vwOne;
     OPSx_pd:Result:=vwOne;
     else;
    end;

  else;
 end;
end;

function get_vl_mode(var ctx:t_jit_context2):t_vl_mode;
begin
 Result:=vl256;
 case ctx.din.OpCode.Opcode of

  OPvperm2:Result:=vlOne;
  OPvperm :Result:=vlOne;

  OPinsert:
    case ctx.din.OpCode.Suffix of
     OPSx_f128:Result:=vlOne;
     else;
    end;

  OPextract:
    case ctx.din.OpCode.Suffix of
     OPSx_f128:Result:=vlOne;
     else;
    end;

  else;
 end;
end;

procedure op_emit_avx2_mem_reg(var ctx:t_jit_context2;hint:t_op_hint);
const
 desc:t_op_desc=(
  mem_reg:();
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );
var
 tmp:t_op_desc;
begin
 tmp:=desc;
 tmp.mem_reg.op     :=ctx.dis.opcode and $FF;
 tmp.mem_reg.simdop :=SCODES[ctx.dis.SimdOpcode];
 tmp.mem_reg.mm     :=ctx.dis.mm;
 tmp.mem_reg.vw_mode:=get_vw_mode(ctx);
 tmp.hint:=hint;
 //
 op_emit_avx2(ctx,tmp);
end;

procedure op_emit_avx2_reg_mem(var ctx:t_jit_context2;hint:t_op_hint);
const
 desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:();
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );
var
 tmp:t_op_desc;
begin
 tmp:=desc;
 tmp.reg_mem.op     :=ctx.dis.opcode and $FF;
 tmp.reg_mem.simdop :=SCODES[ctx.dis.SimdOpcode];
 tmp.reg_mem.mm     :=ctx.dis.mm;
 tmp.reg_mem.vw_mode:=get_vw_mode(ctx);
 tmp.hint:=hint;
 //
 op_emit_avx2(ctx,tmp);
end;

procedure op_avx2_mem_reg_mov_wo(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx2_mem_reg(ctx,[his_mov,his_wo]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_avx2_reg_mem_mov_wo(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx2_reg_mem(ctx,[his_mov,his_wo]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_avx2_reg_mem_wo(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx2_reg_mem(ctx,[his_wo]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_avx2_reg_mem_ro(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx2_reg_mem(ctx,[his_ro]);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

procedure op_avx2_rr(var ctx:t_jit_context2);
var
 tmp:t_op_type;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=Default(t_op_type);
  tmp.op     :=ctx.dis.opcode and $FF;
  tmp.simdop :=SCODES[ctx.dis.SimdOpcode];
  tmp.mm     :=ctx.dis.mm;
  tmp.vw_mode:=get_vw_mode(ctx);
  //
  op_emit_avx2_rr(ctx,tmp);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

procedure op_avx3_gen(var ctx:t_jit_context2);
var
 tmp:t_op_type;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=Default(t_op_type);
  tmp.op     :=ctx.dis.opcode and $FF;
  tmp.simdop :=SCODES[ctx.dis.SimdOpcode];
  tmp.mm     :=ctx.dis.mm;
  tmp.vw_mode:=get_vw_mode(ctx);
  tmp.vl_mode:=get_vl_mode(ctx);
  //
  op_emit_avx3(ctx,tmp);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_avx3_vx_zero(var ctx:t_jit_context2);
var
 tmp:t_op_type;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=Default(t_op_type);
  tmp.op     :=ctx.dis.opcode and $FF;
  tmp.simdop :=SCODES[ctx.dis.SimdOpcode];
  tmp.mm     :=ctx.dis.mm;
  tmp.vw_mode:=get_vw_mode(ctx);
  tmp.vl_mode:=vlZero;
  //
  op_emit_avx3(ctx,tmp);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

procedure op_avx3_rmi(var ctx:t_jit_context2);
const
 desc:t_op_avx3_imm=(
  rmi:();
  mri:(opt:[not_impl]);
 );
var
 tmp:t_op_avx3_imm;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=desc;
  tmp.rmi.op     :=ctx.dis.opcode and $FF;
  tmp.rmi.simdop :=SCODES[ctx.dis.SimdOpcode];
  tmp.rmi.mm     :=ctx.dis.mm;
  tmp.rmi.vw_mode:=get_vw_mode(ctx);
  tmp.rmi.vl_mode:=get_vl_mode(ctx);
  //
  op_emit_avx3_imm8(ctx,tmp);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_avx3_mri(var ctx:t_jit_context2);
const
 desc:t_op_avx3_imm=(
  rmi:(opt:[not_impl]);
  mri:();
 );
var
 tmp:t_op_avx3_imm;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=desc;
  tmp.mri.op     :=ctx.dis.opcode and $FF;
  tmp.mri.simdop :=SCODES[ctx.dis.SimdOpcode];
  tmp.mri.mm     :=ctx.dis.mm;
  tmp.mri.vw_mode:=get_vw_mode(ctx);
  tmp.mri.vl_mode:=get_vl_mode(ctx);
  //
  op_emit_avx3_imm8(ctx,tmp);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vmovups_desc:t_op_desc=(
  mem_reg:(op:$11;simdop:0;mm:1);
  reg_mem:(op:$10;simdop:0;mm:1);
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
  mem_reg:(op:$11;simdop:1;mm:1);
  reg_mem:(op:$10;simdop:1;mm:1);
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
  mem_reg:(op:$29;simdop:0;mm:1);
  reg_mem:(op:$28;simdop:0;mm:1);
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
  mem_reg:(op:$29;simdop:1;mm:1);
  reg_mem:(op:$28;simdop:1;mm:1);
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
  mem_reg:(op:$7F;simdop:2;mm:1);
  reg_mem:(op:$6F;simdop:2;mm:1);
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
  mem_reg:(op:$7F;simdop:1;mm:1);
  reg_mem:(op:$6F;simdop:1;mm:1);
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

procedure op_vmovntdqa(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2_reg_mem(ctx,[his_mov,his_wo,his_align]);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmov_d_desc:t_op_desc=(
  mem_reg:(op:$7E;simdop:1;mm:1;vw_mode:vwZero);
  reg_mem:(op:$6E;simdop:1;mm:1;vw_mode:vwZero);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vmovd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vmov_d_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmov_q_desc:t_op_desc=(
  mem_reg:(op:$7E;simdop:1;mm:1;vw_mode:vwOne);
  reg_mem:(op:$6E;simdop:1;mm:1;vw_mode:vwOne);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

 vmovq_desc:t_op_desc=(
  mem_reg:(op:$D6;simdop:1;mm:1;vw_mode:vwZero);
  reg_mem:(op:$7E;simdop:2;mm:1;vw_mode:vwZero);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vmovq(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  if (ctx.dis.SimdOpcode=so66) and
     ((ctx.dis.opcode and $FF) in [$6E,$7E]) then
  begin
   op_emit_avx2(ctx,vmov_q_desc);
  end else
  begin
   op_emit_avx2(ctx,vmovq_desc);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmov_ss_desc:t_op_desc=(
  mem_reg:(op:$11;simdop:2;mm:1);
  reg_mem:(op:$10;simdop:2;mm:1);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vmov_ss_sd(var ctx:t_jit_context2);
var
 desc:t_op_desc;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  case ctx.din.OperCnt of
   2:begin
      desc:=vmov_ss_desc;
      desc.mem_reg.simdop:=SCODES[ctx.dis.SimdOpcode];
      desc.reg_mem.simdop:=SCODES[ctx.dis.SimdOpcode];
      //
      op_emit_avx2(ctx,desc);
     end;
   3:op_avx3_vx_zero(ctx);
   else
    Assert(False,'op_vmov_ss_sd');
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vmovlps_desc2:t_op_desc=(
  mem_reg:(op:$13;simdop:0;mm:1;vl_mode:vlZero);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

 vmovlps_rrm_desc:t_op_type=(op:$12;simdop:0;mm:1);

procedure op_vmovlps(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  case ctx.din.OperCnt of
   2:op_emit_avx2(ctx,vmovlps_desc2);
   3:op_emit_avx3(ctx,vmovlps_rrm_desc);
   else
    Assert(False,'op_vmovlps');
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vmovlpd_desc2:t_op_desc=(
  mem_reg:(op:$13;simdop:1;mm:1;vl_mode:vlZero);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

 vmovlpd_rrm_desc:t_op_type=(op:$12;simdop:1;mm:1;vl_mode:vlZero);

procedure op_vmovlpd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  case ctx.din.OperCnt of
   2:op_emit_avx2(ctx,vmovlpd_desc2);
   3:op_emit_avx3(ctx,vmovlpd_rrm_desc);
   else
    Assert(False,'op_vmovlpd');
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vmovhps_desc2:t_op_desc=(
  mem_reg:(op:$17;simdop:0;mm:1;vl_mode:vlZero);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

 vmovhps_rrm_desc:t_op_type=(op:$16;simdop:0;mm:1;vl_mode:vlZero);

procedure op_vmovhps(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  case ctx.din.OperCnt of
   2:op_emit_avx2(ctx,vmovhps_desc2);
   3:op_emit_avx3(ctx,vmovhps_rrm_desc);
   else
    Assert(False,'op_vmovhps');
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vmovhpd_desc2:t_op_desc=(
  mem_reg:(op:$17;simdop:1;mm:1;vl_mode:vlZero);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

 vmovhpd_rrm_desc:t_op_type=(op:$16;simdop:1;mm:1;vl_mode:vlZero);

procedure op_vmovhpd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  case ctx.din.OperCnt of
   2:op_emit_avx2(ctx,vmovhpd_desc2);
   3:op_emit_avx3(ctx,vmovhpd_rrm_desc);
   else
    Assert(False,'op_vmovhpd');
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vptest_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$17;simdop:1;mm:2;vl_mode:vlZero);
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

procedure op_bmi_gen(var ctx:t_jit_context2);
const
 desc:t_op_type=(
  op:$F3;index:0;mm:0;vw_mode:vwR64;vl_mode:vlZero
 );
var
 tmp:t_op_type;
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  tmp:=desc;
  tmp.index:=ctx.dis.ModRM.Index;
  tmp.mm   :=ctx.dis.mm;
  //
  op_emit_avx_F3(ctx,tmp);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vpermilps_rrm_desc:t_op_type=(
  op:$0C;simdop:1;mm:2;
 );

 vpermilps_rmi_desc:t_op_avx3_imm=(
  rmi:(op:$04;simdop:1;mm:3);
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
   Assert(False,'op_vpermilps');
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vpermilpd_rrm_desc:t_op_type=(
  op:$0D;simdop:1;mm:2;
 );

 vpermilpd_rmi_desc:t_op_avx3_imm=(
  rmi:(op:$05;simdop:1;mm:3);
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
   Assert(False,'op_vpermilpd');
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vpblendvb_desc:t_op_type=(
  op:$4C;simdop:1;mm:3
 );

procedure op_vpblendvb(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx4(ctx,vpblendvb_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vblendvps_desc:t_op_type=(
  op:$4A;simdop:1;mm:3
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
  op:$4B;simdop:1;mm:3
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

//

const
 bextr_desc:t_op_type=(
  op:$F7;simdop:0;mm:2;vw_mode:vwR64;
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
  op:$F2;simdop:0;mm:2;vw_mode:vwR64;
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
 vbroadcastss_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$18;simdop:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
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
  reg_mem:(op:$19;simdop:1;mm:2;vw_mode:vwZero);
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
  reg_mem:(op:$1A;simdop:1;mm:2;vl_mode:vlOne);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
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

//

const
 vldmxcsr_desc:t_op_type=(
  op:$AE;index:2;simdop:0;mm:1;
 );

procedure op_vldmxcsr(var ctx:t_jit_context2);
begin
 op_emit_avx1(ctx,vldmxcsr_desc,[his_ro]);
end;

const
 vstmxcsr_desc:t_op_type=(
  op:$AE;index:3;simdop:0;mm:1;
 );

procedure op_vstmxcsr(var ctx:t_jit_context2);
begin
 op_emit_avx1(ctx,vstmxcsr_desc,[his_wo]);
end;

///

procedure init_cbs_avx;
begin
 jit_cbs[OPPv,OPmovu,OPSx_ps ]:=@op_vmovups;
 jit_cbs[OPPv,OPmovu,OPSx_pd ]:=@op_vmovupd;
 jit_cbs[OPPv,OPmova,OPSx_ps ]:=@op_vmovaps;
 jit_cbs[OPPv,OPmova,OPSx_pd ]:=@op_vmovapd;
 jit_cbs[OPPv,OPmov ,OPSx_dqu]:=@op_vmovdqu;
 jit_cbs[OPPv,OPmov ,OPSx_dqa]:=@op_vmovdqa;

 jit_cbs[OPPv,OPlddqu,OPSnone]:=@op_avx2_reg_mem_wo;

 jit_cbs[OPPv,OPmovnt,OPSx_dqa]:=@op_vmovntdqa;
 jit_cbs[OPPv,OPmovnt,OPSx_dq ]:=@op_avx2_mem_reg_mov_wo;

 jit_cbs[OPPv,OPmovnt,OPSx_ps ]:=@op_avx2_mem_reg_mov_wo;
 jit_cbs[OPPv,OPmovnt,OPSx_pd ]:=@op_avx2_mem_reg_mov_wo;

 jit_cbs[OPPv,OPmovddup,OPSnone]:=@op_avx2_reg_mem_mov_wo;

 jit_cbs[OPPv,OPmov ,OPSx_d  ]:=@op_vmovd;
 jit_cbs[OPPv,OPmov ,OPSx_q  ]:=@op_vmovq;

 jit_cbs[OPPv,OPmov ,OPSx_ss ]:=@op_vmov_ss_sd;
 jit_cbs[OPPv,OPmov ,OPSx_sd ]:=@op_vmov_ss_sd;

 jit_cbs[OPPv,OPmovl,OPSx_ps]:=@op_vmovlps;
 jit_cbs[OPPv,OPmovl,OPSx_pd]:=@op_vmovlpd;

 jit_cbs[OPPv,OPmovh,OPSx_ps]:=@op_vmovhps;
 jit_cbs[OPPv,OPmovh,OPSx_pd]:=@op_vmovhpd;

 jit_cbs[OPPv,OPmovhlps,OPSnone]:=@add_orig;

 jit_cbs[OPPv,OPmovlh,OPSx_ps]:=@add_orig;

 jit_cbs[OPPv,OPmovsldup,OPSnone]:=@op_avx2_reg_mem_mov_wo;
 jit_cbs[OPPv,OPmovshdup,OPSnone]:=@op_avx2_reg_mem_mov_wo;

 jit_cbs[OPPv,OPpmovsx,OPSv_bw]:=@op_avx2_reg_mem_mov_wo;
 jit_cbs[OPPv,OPpmovsx,OPSv_bd]:=@op_avx2_reg_mem_mov_wo;
 jit_cbs[OPPv,OPpmovsx,OPSv_bq]:=@op_avx2_reg_mem_mov_wo;
 jit_cbs[OPPv,OPpmovsx,OPSv_wd]:=@op_avx2_reg_mem_mov_wo;
 jit_cbs[OPPv,OPpmovsx,OPSv_wq]:=@op_avx2_reg_mem_mov_wo;
 jit_cbs[OPPv,OPpmovsx,OPSv_dq]:=@op_avx2_reg_mem_mov_wo;

 jit_cbs[OPPv,OPpmovzx,OPSv_bw]:=@op_avx2_reg_mem_mov_wo;
 jit_cbs[OPPv,OPpmovzx,OPSv_bd]:=@op_avx2_reg_mem_mov_wo;
 jit_cbs[OPPv,OPpmovzx,OPSv_bq]:=@op_avx2_reg_mem_mov_wo;
 jit_cbs[OPPv,OPpmovzx,OPSv_wd]:=@op_avx2_reg_mem_mov_wo;
 jit_cbs[OPPv,OPpmovzx,OPSv_wq]:=@op_avx2_reg_mem_mov_wo;
 jit_cbs[OPPv,OPpmovzx,OPSv_dq]:=@op_avx2_reg_mem_mov_wo;

 jit_cbs[OPPv,OPmovmsk  ,OPSx_ps]:=@op_avx2_rr;
 jit_cbs[OPPv,OPmovmsk  ,OPSx_pd]:=@op_avx2_rr;
 jit_cbs[OPPv,OPpmovmskb,OPSnone]:=@op_avx2_rr;

 jit_cbs[OPPv,OPmaskmov,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPmaskmov,OPSx_pd]:=@op_avx3_gen;

 jit_cbs[OPPv,OPcomi ,OPSx_ss]:=@op_avx2_reg_mem_ro;
 jit_cbs[OPPv,OPcomi ,OPSx_sd]:=@op_avx2_reg_mem_ro;

 jit_cbs[OPPv,OPucomi,OPSx_ss]:=@op_avx2_reg_mem_ro;
 jit_cbs[OPPv,OPucomi,OPSx_sd]:=@op_avx2_reg_mem_ro;

 jit_cbs[OPPv,OPptest,OPSnone]:=@op_vptest;

 jit_cbs[OPPnone,OPblsr  ,OPSnone]:=@op_bmi_gen;
 jit_cbs[OPPnone,OPblsmsk,OPSnone]:=@op_bmi_gen;
 jit_cbs[OPPnone,OPblsi  ,OPSnone]:=@op_bmi_gen;

 jit_cbs[OPPv,OPpcmpeq,OPSx_b ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpcmpeq,OPSx_w ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpcmpeq,OPSx_d ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpcmpeq,OPSx_q ]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpcmpgt,OPSx_b ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpcmpgt,OPSx_w ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpcmpgt,OPSx_d ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpcmpgt,OPSx_q ]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpcmpestrm,OPSnone]:=@op_avx3_rmi;
 jit_cbs[OPPv,OPpcmpestri,OPSnone]:=@op_avx3_rmi;
 jit_cbs[OPPv,OPpcmpistrm,OPSnone]:=@op_avx3_rmi;
 jit_cbs[OPPv,OPpcmpistri,OPSnone]:=@op_avx3_rmi;

 jit_cbs[OPPv,OPsub   ,OPSx_sd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPsub   ,OPSx_ss]:=@op_avx3_gen;
 jit_cbs[OPPv,OPsub   ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPsub   ,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsub  ,OPSx_b ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsub  ,OPSx_w ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsub  ,OPSx_d ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsub  ,OPSx_q ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsubs ,OPSx_b ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsubs ,OPSx_w ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsubus,OPSx_b ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsubus,OPSx_w ]:=@op_avx3_gen;

 jit_cbs[OPPv,OPadd   ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPadd   ,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPadd   ,OPSx_ss]:=@op_avx3_gen;
 jit_cbs[OPPv,OPadd   ,OPSx_sd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpadd  ,OPSx_b ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpadd  ,OPSx_w ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpadd  ,OPSx_d ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpadd  ,OPSx_q ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpadds ,OPSx_b ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpadds ,OPSx_w ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpaddus,OPSx_b ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpaddus,OPSx_w ]:=@op_avx3_gen;

 jit_cbs[OPPv,OPphadd ,OPSx_w]:=@op_avx3_gen;
 jit_cbs[OPPv,OPphadd ,OPSx_d]:=@op_avx3_gen;

 jit_cbs[OPPv,OPaddsub,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPaddsub,OPSx_ps]:=@op_avx3_gen;

 jit_cbs[OPPv,OPdiv   ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPdiv   ,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPdiv   ,OPSx_ss]:=@op_avx3_gen;
 jit_cbs[OPPv,OPdiv   ,OPSx_sd]:=@op_avx3_gen;

 jit_cbs[OPPv,OPmul   ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPmul   ,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPmul   ,OPSx_ss]:=@op_avx3_gen;
 jit_cbs[OPPv,OPmul   ,OPSx_sd]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpmull ,OPSx_d]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpmull ,OPSx_w]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpmuludq ,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpmulhuw ,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpmulhrsw,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpmulhw  ,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpmuldq  ,OPSnone]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpunpcklbw ,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpunpcklwd ,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpunpckldq ,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpunpcklqdq,OPSnone]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpunpckhbw ,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpunpckhwd ,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpunpckhdq ,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpunpckhqdq,OPSnone]:=@op_avx3_gen;

 jit_cbs[OPPv,OPunpckl    ,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPunpckl    ,OPSx_ps]:=@op_avx3_gen;

 jit_cbs[OPPv,OPunpckh    ,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPunpckh    ,OPSx_ps]:=@op_avx3_gen;

 jit_cbs[OPPv,OPcmp   ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPcmp   ,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPcmp   ,OPSx_sd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPcmp   ,OPSx_ss]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpshuf ,OPSx_b ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpshuf ,OPSx_d ]:=@op_avx3_rmi;
 jit_cbs[OPPv,OPpshuf ,OPSx_hw]:=@op_avx3_rmi;
 jit_cbs[OPPv,OPpshuf ,OPSx_lw]:=@op_avx3_rmi;

 jit_cbs[OPPv,OPshuf  ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPshuf  ,OPSx_pd]:=@op_avx3_gen;

 jit_cbs[OPPnone,OPvpermil,OPSx_ps]:=@op_vpermilps;
 jit_cbs[OPPnone,OPvpermil,OPSx_pd]:=@op_vpermilpd;

 jit_cbs[OPPnone,OPvperm2,OPSx_f128]:=@op_avx3_gen;
 jit_cbs[OPPnone,OPvperm2,OPSx_i128]:=@op_avx3_gen;

 jit_cbs[OPPnone,OPvperm,OPSx_d]:=@op_avx3_gen;
 jit_cbs[OPPnone,OPvperm,OPSx_q]:=@op_avx3_rmi;

 jit_cbs[OPPnone,OPvperm,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPnone,OPvperm,OPSx_pd]:=@op_avx3_rmi;

 jit_cbs[OPPv,OPxor    ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPxor    ,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpxor   ,OPSnone]:=@op_avx3_gen;

 jit_cbs[OPPv,OPor     ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPor     ,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpor    ,OPSnone]:=@op_avx3_gen;

 jit_cbs[OPPv,OPand    ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPand    ,OPSx_pd]:=@op_avx3_gen;

 jit_cbs[OPPv,OPandn   ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPandn   ,OPSx_pd]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpand   ,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpandn  ,OPSnone]:=@op_avx3_gen;

 jit_cbs[OPPv,OPblend  ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPblend  ,OPSx_pd]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpblend ,OPSx_w ]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpblendvb,OPSnone]:=@op_vpblendvb;

 jit_cbs[OPPv,OPblendv ,OPSx_ps]:=@op_vblendvps;
 jit_cbs[OPPv,OPblendv ,OPSx_pd]:=@op_vblendvpd;

 jit_cbs[OPPv,OPcvtsi2 ,OPSx_ss]:=@op_avx3_gen;
 jit_cbs[OPPv,OPcvtsi2 ,OPSx_sd]:=@op_avx3_gen;

 jit_cbs[OPPv,OPcvtss2 ,OPSx_sd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPcvtss2 ,OPSx_si]:=@op_avx2_reg_mem_wo;

 jit_cbs[OPPv,OPcvtsd2 ,OPSx_ss]:=@op_avx3_gen;
 jit_cbs[OPPv,OPcvtsd2 ,OPSx_si]:=@op_avx2_reg_mem_wo;

 jit_cbs[OPPv,OPcvttps2,OPSx_dq]:=@op_avx2_reg_mem_wo;
 jit_cbs[OPPv,OPcvttpd2,OPSx_dq]:=@op_avx2_reg_mem_wo;

 jit_cbs[OPPv,OPcvtdq2 ,OPSx_ps]:=@op_avx2_reg_mem_wo;
 jit_cbs[OPPv,OPcvtdq2 ,OPSx_pd]:=@op_avx2_reg_mem_wo;

 jit_cbs[OPPv,OPcvttss2,OPSx_si]:=@op_avx2_reg_mem_wo;
 jit_cbs[OPPv,OPcvttsd2,OPSx_si]:=@op_avx2_reg_mem_wo;

 jit_cbs[OPPv,OPcvtpd2 ,OPSx_ps]:=@op_avx2_reg_mem_wo;
 jit_cbs[OPPv,OPcvtpd2 ,OPSx_dq]:=@op_avx2_reg_mem_wo;

 jit_cbs[OPPv,OPcvtps2 ,OPSx_pd]:=@op_avx2_reg_mem_wo;
 jit_cbs[OPPv,OPcvtps2 ,OPSx_dq]:=@op_avx2_reg_mem_wo;

 jit_cbs[OPPv,OPcvtps2 ,OPSx_ph]:=@op_avx3_mri;

 jit_cbs[OPPnone,OPvcvtph2ps,OPSnone]:=@op_avx2_reg_mem_wo;

 jit_cbs[OPPnone,OPbextr,OPSnone]:=@op_bextr;
 jit_cbs[OPPnone,OPandn ,OPSnone]:=@op_andn;

 jit_cbs[OPPv,OPpextr,OPSx_b]:=@op_avx3_mri;
 jit_cbs[OPPv,OPpextr,OPSx_d]:=@op_avx3_mri;
 jit_cbs[OPPv,OPpextr,OPSx_q]:=@op_avx3_mri;
 jit_cbs[OPPv,OPpextr,OPSx_w]:=@op_avx3_mri;

 jit_cbs[OPPv,OPextract,OPSx_ps  ]:=@op_avx3_mri;
 jit_cbs[OPPv,OPextract,OPSx_f128]:=@op_avx3_mri;

 jit_cbs[OPPv,OPinsert ,OPSx_ps  ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPinsert ,OPSx_f128]:=@op_avx3_gen;

 jit_cbs[OPPv,OPround,OPSx_ps]:=@op_avx3_rmi;
 jit_cbs[OPPv,OPround,OPSx_pd]:=@op_avx3_rmi;
 jit_cbs[OPPv,OPround,OPSx_ss]:=@op_avx3_gen; //rrmi
 jit_cbs[OPPv,OPround,OPSx_sd]:=@op_avx3_gen; //rrmi

 jit_cbs[OPPv,OPsqrt ,OPSx_ps]:=@op_avx2_reg_mem_wo;
 jit_cbs[OPPv,OPsqrt ,OPSx_pd]:=@op_avx2_reg_mem_wo;
 jit_cbs[OPPv,OPsqrt ,OPSx_sd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPsqrt ,OPSx_ss]:=@op_avx3_gen;

 jit_cbs[OPPv,OPrsqrt,OPSx_ps]:=@op_avx2_reg_mem_wo;
 jit_cbs[OPPv,OPrsqrt,OPSx_ss]:=@op_avx3_gen;

 jit_cbs[OPPv,OPrcp  ,OPSx_ps]:=@op_avx2_reg_mem_wo;
 jit_cbs[OPPv,OPrcp  ,OPSx_ss]:=@op_avx3_gen;

 jit_cbs[OPPnone,OPvbroadcast,OPSx_ss  ]:=@op_vbroadcastss;
 jit_cbs[OPPnone,OPvbroadcast,OPSx_sd  ]:=@op_vbroadcastsd;
 jit_cbs[OPPnone,OPvbroadcast,OPSx_f128]:=@op_vbroadcastf128;

 jit_cbs[OPPv,OPpclmulqdq,OPSnone]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpinsr,OPSx_b]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpinsr,OPSx_d]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpinsr,OPSx_q]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpinsr,OPSx_w]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpacksswb,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpackssdw,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpackusdw,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpackuswb,OPSnone]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpabs,OPSx_b]:=@op_avx2_reg_mem_wo;
 jit_cbs[OPPv,OPpabs,OPSx_w]:=@op_avx2_reg_mem_wo;
 jit_cbs[OPPv,OPpabs,OPSx_d]:=@op_avx2_reg_mem_wo;

 jit_cbs[OPPv,OPpsra,OPSx_w]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsra,OPSx_d]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpsrl,OPSx_w ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsrl,OPSx_d ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsrl,OPSx_q ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsrl,OPSx_dq]:=@add_orig;

 jit_cbs[OPPv,OPpsll,OPSx_w ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsll,OPSx_d ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsll,OPSx_q ]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsll,OPSx_dq]:=@add_orig;

 jit_cbs[OPPv,OPpminu,OPSx_b]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpminu,OPSx_w]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpminu,OPSx_d]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpmins,OPSx_b]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpmins,OPSx_w]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpmins,OPSx_d]:=@op_avx3_gen;

 jit_cbs[OPPv,OPmin ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPmin ,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPmin ,OPSx_ss]:=@op_avx3_gen;
 jit_cbs[OPPv,OPmin ,OPSx_sd]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpmaxu,OPSx_b]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpmaxu,OPSx_w]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpmaxu,OPSx_d]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpmaxs,OPSx_b]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpmaxs,OPSx_w]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpmaxs,OPSx_d]:=@op_avx3_gen;

 jit_cbs[OPPv,OPmax ,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPmax ,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPmax ,OPSx_ss]:=@op_avx3_gen;
 jit_cbs[OPPv,OPmax ,OPSx_sd]:=@op_avx3_gen;

 jit_cbs[OPPv,OPhadd,OPSx_pd]:=@op_avx3_gen;
 jit_cbs[OPPv,OPhadd,OPSx_ps]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpmaddubsw,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpmaddwd  ,OPSnone]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpsadbw,OPSnone]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpalignr,OPSnone]:=@op_avx3_gen;

 jit_cbs[OPPv,OPpsign,OPSx_b]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsign,OPSx_w]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpsign,OPSx_d]:=@op_avx3_gen;

 jit_cbs[OPPv,OPldmxcsr,OPSnone]:=@op_vldmxcsr;
 jit_cbs[OPPv,OPstmxcsr,OPSnone]:=@op_vstmxcsr;

 jit_cbs[OPPv,OPpavg,OPSx_b]:=@op_avx3_gen;
 jit_cbs[OPPv,OPpavg,OPSx_w]:=@op_avx3_gen;

 jit_cbs[OPPv,OPdp,OPSx_ps]:=@op_avx3_gen;
 jit_cbs[OPPv,OPdp,OPSx_pd]:=@op_avx3_gen;

 jit_cbs[OPPv,OPaeskeygenassist,OPSnone]:=@op_avx3_rmi;
 jit_cbs[OPPv,OPaesimc         ,OPSnone]:=@op_avx2_reg_mem_wo;

 jit_cbs[OPPv,OPaesenc    ,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPaesenclast,OPSnone]:=@op_avx3_gen;

 jit_cbs[OPPv,OPaesdec    ,OPSnone]:=@op_avx3_gen;
 jit_cbs[OPPv,OPaesdeclast,OPSnone]:=@op_avx3_gen;

end;

initialization
 init_cbs_avx;

end.

