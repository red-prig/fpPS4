unit kern_jit2_ops_avx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 x86_fpdbgdisas,
 x86_jit,
 kern_jit2_ops,
 kern_jit2_ctx;

implementation

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

//

const
 vmovlps_desc2:t_op_desc=(
  mem_reg:(op:$13;index:0;mm:1);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

 vmovlps_rrm_desc:t_op_type=(op:$12;index:0;mm:1);

procedure op_vmovlps(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  case ctx.din.OperCnt of
   2:op_emit_avx2(ctx,vmovlps_desc2);
   3:op_emit_avx3(ctx,vmovlps_rrm_desc);
   else
    Assert(False);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

//

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

//

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

//

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
 vpmovsxbd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$21;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vpmovsxbd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vpmovsxbd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpmovsxbq_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$22;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vpmovsxbq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vpmovsxbq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpmovsxwd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$23;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vpmovsxwd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vpmovsxwd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpmovsxwq_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$24;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vpmovsxwq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vpmovsxwq_desc);
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
 vpmovzxbw_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$30;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vpmovzxbw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vpmovzxbw_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpmovzxbd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$31;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vpmovzxbd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vpmovzxbd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpmovzxbq_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$32;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vpmovzxbq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vpmovzxbq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpmovzxwd_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$33;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vpmovzxwd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vpmovzxwd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpmovzxwq_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$34;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vpmovzxwq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vpmovzxwq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpmovzxdq_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$35;index:1;mm:2);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_vpmovzxdq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx2(ctx,vpmovzxdq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vmovmskps_desc:t_op_type=(op:$50;index:0;mm:1);

procedure op_vmovmskps(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) then
 begin
  op_emit_avx2_rr(ctx,vmovmskps_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovmskpd_desc:t_op_type=(op:$50;index:1;mm:1);

procedure op_vmovmskpd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) then
 begin
  op_emit_avx2_rr(ctx,vmovmskpd_desc);
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

//

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
 vpcmpeqq_desc:t_op_type=(
  op:$29;index:1;mm:2;
 );

procedure op_vpcmpeqq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpcmpeqq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 vpcmpgtb_desc:t_op_type=(
  op:$64;index:1;mm:1;
 );

procedure op_vpcmpgtb(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpcmpgtb_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpcmpgtw_desc:t_op_type=(
  op:$65;index:1;mm:1;
 );

procedure op_vpcmpgtw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpcmpgtw_desc);
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
 vpcmpgtq_desc:t_op_type=(
  op:$37;index:1;mm:2;
 );

procedure op_vpcmpgtq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpcmpgtq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

///

const
 vpcmpestrm_desc:t_op_avx3_imm=(
  rmi:(op:$60;index:1;mm:3);
  mri:(opt:[not_impl]);
 );

procedure op_vpcmpestrm(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3_imm8(ctx,vpcmpestrm_desc);
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
 vaddsubpd_desc:t_op_type=(
  op:$D0;index:1;mm:1;
 );

procedure op_vaddsubpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vaddsubpd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vaddsubps_desc:t_op_type=(
  op:$D0;index:3;mm:1;
 );

procedure op_vaddsubps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vaddsubps_desc);
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
 vpmulld_desc:t_op_type=(
  op:$40;index:1;mm:2;
 );

procedure op_vpmulld(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpmulld_desc);
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
 vpmuludq_desc:t_op_type=(
  op:$F4;index:1;mm:1;
 );

procedure op_vpmuludq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpmuludq_desc);
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

//

const
 vpunpcklbw_desc:t_op_type=(
  op:$60;index:1;mm:1;
 );

procedure op_vpunpcklbw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpunpcklbw_desc);
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

//

const
 vpunpckhbw_desc:t_op_type=(
  op:$68;index:1;mm:1;
 );

procedure op_vpunpckhbw(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpunpckhbw_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpunpckhwd_desc:t_op_type=(
  op:$69;index:1;mm:1;
 );

procedure op_vpunpckhwd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpunpckhwd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpunpckhdq_desc:t_op_type=(
  op:$6A;index:1;mm:1;
 );

procedure op_vpunpckhdq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpunpckhdq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vpunpckhqdq_desc:t_op_type=(
  op:$6D;index:1;mm:1;
 );

procedure op_vpunpckhqdq(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpunpckhqdq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

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
 vunpckhpd_desc:t_op_type=(
  op:$15;index:1;mm:1;
 );

procedure op_vunpckhpd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vunpckhpd_desc);
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
 vpsrld_desc:t_op_type=(
  op:$D2;index:1;mm:1;
 );

procedure op_vpsrld(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vpsrld_desc);
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
 vminss_desc:t_op_type=(
  op:$5D;index:2;mm:1
 );

procedure op_vminss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vminss_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vminsd_desc:t_op_type=(
  op:$5D;index:3;mm:1
 );

procedure op_vminsd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vminsd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmaxss_desc:t_op_type=(
  op:$5F;index:2;mm:1
 );

procedure op_vmaxss(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_avx3(ctx,vmaxss_desc);
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

procedure init_cbs_avx;
begin
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

 jit_cbs[OPPv,OPmovl,OPSx_ps]:=@op_vmovlps;

 jit_cbs[OPPv,OPmovl,OPSx_pd]:=@op_vmovlpd;
 jit_cbs[OPPv,OPmovh,OPSx_pd]:=@op_vmovhpd;

 jit_cbs[OPPv,OPmovsldup,OPSnone]:=@op_vmovsldup;
 jit_cbs[OPPv,OPmovshdup,OPSnone]:=@op_vmovshdup;

 jit_cbs[OPPv,OPpmovsx,OPSv_bw]:=@op_vpmovsxbw;
 jit_cbs[OPPv,OPpmovsx,OPSv_bd]:=@op_vpmovsxbd;
 jit_cbs[OPPv,OPpmovsx,OPSv_bq]:=@op_vpmovsxbq;
 jit_cbs[OPPv,OPpmovsx,OPSv_wd]:=@op_vpmovsxwd;
 jit_cbs[OPPv,OPpmovsx,OPSv_wq]:=@op_vpmovsxwq;
 jit_cbs[OPPv,OPpmovsx,OPSv_dq]:=@op_vpmovsxdq;

 jit_cbs[OPPv,OPpmovzx,OPSv_bw]:=@op_vpmovzxbw;
 jit_cbs[OPPv,OPpmovzx,OPSv_bd]:=@op_vpmovzxbd;
 jit_cbs[OPPv,OPpmovzx,OPSv_bq]:=@op_vpmovzxbq;
 jit_cbs[OPPv,OPpmovzx,OPSv_wd]:=@op_vpmovzxwd;
 jit_cbs[OPPv,OPpmovzx,OPSv_wq]:=@op_vpmovzxwq;
 jit_cbs[OPPv,OPpmovzx,OPSv_dq]:=@op_vpmovzxdq;

 jit_cbs[OPPv,OPmovmsk,OPSx_ps]:=@op_vmovmskps;
 jit_cbs[OPPv,OPmovmsk,OPSx_pd]:=@op_vmovmskpd;

 jit_cbs[OPPv,OPucomi,OPSx_ss]:=@op_vucomiss;
 jit_cbs[OPPv,OPucomi,OPSx_sd]:=@op_vucomisd;

 jit_cbs[OPPv,OPptest,OPSnone]:=@op_vptest;

 jit_cbs[OPPnone,OPblsr,OPSnone ]:=@op_blsr;
 jit_cbs[OPPnone,OPblsi,OPSnone ]:=@op_blsi;

 jit_cbs[OPPv,OPxor   ,OPSx_ps]:=@op_vxorps;
 jit_cbs[OPPv,OPxor   ,OPSx_pd]:=@op_vxorpd;

 jit_cbs[OPPv,OPpcmpeq,OPSx_b ]:=@op_vpcmpeqb;
 jit_cbs[OPPv,OPpcmpeq,OPSx_w ]:=@op_vpcmpeqw;
 jit_cbs[OPPv,OPpcmpeq,OPSx_d ]:=@op_vpcmpeqd;
 jit_cbs[OPPv,OPpcmpeq,OPSx_q ]:=@op_vpcmpeqq;

 jit_cbs[OPPv,OPpcmpgt,OPSx_b ]:=@op_vpcmpgtb;
 jit_cbs[OPPv,OPpcmpgt,OPSx_w ]:=@op_vpcmpgtw;
 jit_cbs[OPPv,OPpcmpgt,OPSx_d ]:=@op_vpcmpgtd;
 jit_cbs[OPPv,OPpcmpgt,OPSx_q ]:=@op_vpcmpgtq;

 jit_cbs[OPPv,OPpcmpestrm,OPSnone]:=@op_vpcmpestrm;
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

 jit_cbs[OPPv,OPaddsub,OPSx_pd]:=@op_vaddsubpd;
 jit_cbs[OPPv,OPaddsub,OPSx_ps]:=@op_vaddsubps;

 jit_cbs[OPPv,OPdiv   ,OPSx_ps]:=@op_vdivps;
 jit_cbs[OPPv,OPdiv   ,OPSx_pd]:=@op_vdivpd;
 jit_cbs[OPPv,OPdiv   ,OPSx_ss]:=@op_vdivss;
 jit_cbs[OPPv,OPdiv   ,OPSx_sd]:=@op_vdivsd;

 jit_cbs[OPPv,OPmul   ,OPSx_ps]:=@op_vmulps;
 jit_cbs[OPPv,OPmul   ,OPSx_pd]:=@op_vmulpd;
 jit_cbs[OPPv,OPmul   ,OPSx_ss]:=@op_vmulss;
 jit_cbs[OPPv,OPmul   ,OPSx_sd]:=@op_vmulsd;

 jit_cbs[OPPv,OPpmull ,OPSx_d]:=@op_vpmulld;
 jit_cbs[OPPv,OPpmull ,OPSx_w]:=@op_vpmullw;

 jit_cbs[OPPv,OPpmuludq,OPSnone]:=@op_vpmuludq;
 jit_cbs[OPPv,OPpmulhuw,OPSnone]:=@op_vpmulhuw;

 jit_cbs[OPPv,OPpunpcklbw ,OPSnone]:=@op_vpunpcklbw;
 jit_cbs[OPPv,OPpunpcklwd ,OPSnone]:=@op_vpunpcklwd;
 jit_cbs[OPPv,OPpunpckldq ,OPSnone]:=@op_vpunpckldq;
 jit_cbs[OPPv,OPpunpcklqdq,OPSnone]:=@op_vpunpcklqdq;

 jit_cbs[OPPv,OPpunpckhbw ,OPSnone]:=@op_vpunpckhbw;
 jit_cbs[OPPv,OPpunpckhwd ,OPSnone]:=@op_vpunpckhwd;
 jit_cbs[OPPv,OPpunpckhdq ,OPSnone]:=@op_vpunpckhdq;
 jit_cbs[OPPv,OPpunpckhqdq,OPSnone]:=@op_vpunpckhqdq;

 jit_cbs[OPPv,OPunpckl    ,OPSx_pd]:=@op_vunpcklpd;
 jit_cbs[OPPv,OPunpckh    ,OPSx_pd]:=@op_vunpckhpd;

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
 jit_cbs[OPPv,OPpsrl,OPSx_d]:=@op_vpsrld;
 jit_cbs[OPPv,OPpsrl,OPSx_q]:=@op_vpsrlq;
 jit_cbs[OPPv,OPpsrl,OPSx_dq]:=@add_orig;

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

 jit_cbs[OPPv,OPmin,OPSx_ss]:=@op_vminss;
 jit_cbs[OPPv,OPmin,OPSx_sd]:=@op_vminsd;

 jit_cbs[OPPv,OPmax,OPSx_ss]:=@op_vmaxss;
 jit_cbs[OPPv,OPmax,OPSx_sd]:=@op_vmaxsd;

 jit_cbs[OPPv,OPhadd,OPSx_pd]:=@op_vhaddpd;
 jit_cbs[OPPv,OPhadd,OPSx_ps]:=@op_vhaddps;


end;

initialization
 init_cbs_avx;

end.

