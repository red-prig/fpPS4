unit kern_jit_ops_sse;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

implementation

uses
 x86_fpdbgdisas,
 x86_jit,
 kern_jit_ops,
 kern_jit_ctx;

var
 _SSE4aSupport:Boolean=False;

procedure _ins_op(var op:DWORD;i:Byte); inline;
begin
 case op of
  $00..$FF:
   begin
    op:=op or (DWORD(i) shl 8);
   end;
  $100..$FFFF:
   begin
    op:=op or (DWORD(i) shl 16);
   end;
  else
   begin
    op:=op or (DWORD(i) shl 24);
   end;
 end;
end;

procedure _ins_op(var desc:t_op_desc;i:Byte); inline;
begin
 _ins_op(desc.mem_reg.op,i);
 _ins_op(desc.reg_mem.op,i);
end;

procedure op_emit2_simd(var ctx:t_jit_context2;const desc:t_op_desc);
var
 tmp:t_op_desc;
begin
 tmp:=desc;

 case ctx.dis.SimdOpcode of
  soNone:;
    so66:_ins_op(tmp,$66);
    soF2:_ins_op(tmp,$F2);
    soF3:_ins_op(tmp,$F3);
  else
    Assert(False);
 end;

 op_emit2(ctx,tmp);
end;

procedure op_emit2_simd_mem_reg(var ctx:t_jit_context2;hint:t_op_hint);
const
 desc:t_op_desc=(
  mem_reg:(op:0;opt:[not_prefix]);
  reg_mem:(opt:[not_impl]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );
var
 tmp:t_op_desc;
begin
 tmp:=desc;
 tmp.mem_reg.op:=ctx.dis.opcode;
 tmp.hint:=hint;

 op_emit2_simd(ctx,tmp);
end;

procedure op_emit2_simd_reg_mem(var ctx:t_jit_context2;hint:t_op_hint);
const
 desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:0;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[];
 );
var
 tmp:t_op_desc;
begin
 tmp:=desc;
 tmp.reg_mem.op:=ctx.dis.opcode;
 tmp.hint:=hint;

 op_emit2_simd(ctx,tmp);
end;

//

procedure op_reg_mem_rw(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2_simd_reg_mem(ctx,[his_rw]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_reg_mem_wo(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2_simd_reg_mem(ctx,[his_wo]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_reg_mem_ro(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2_simd_reg_mem(ctx,[his_ro]);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

procedure op_mem_reg_mov_wo(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2_simd_mem_reg(ctx,[his_mov,his_wo]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_reg_mem_mov_wo(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2_simd_reg_mem(ctx,[his_mov,his_wo]);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

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
 mov_dq_desc:t_op_desc=(
  mem_reg:(op:$0F7E;opt:[not_prefix]);
  reg_mem:(op:$0F6E;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_movd_dq(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2_simd(ctx,mov_dq_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 movdqa_desc:t_op_desc=(
  mem_reg:(op:$660F7F;opt:[not_prefix]);
  reg_mem:(op:$660F6F;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo,his_align];
 );

procedure op_movdqa(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2(ctx,movdqa_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 movdqu_desc:t_op_desc=(
  mem_reg:(op:$F30F7F;opt:[not_prefix]);
  reg_mem:(op:$F30F6F;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_movdqu(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2(ctx,movdqu_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 movu_ps_pd_desc:t_op_desc=(
  mem_reg:(op:$0F11;opt:[not_prefix]);
  reg_mem:(op:$0F10;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_movu_ps_pd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2_simd(ctx,movu_ps_pd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 mova_ps_pd_desc:t_op_desc=(
  mem_reg:(op:$0F29;opt:[not_prefix]);
  reg_mem:(op:$0F28;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo,his_align];
 );

procedure op_mova_ps_pd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2_simd(ctx,mova_ps_pd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 movntdqa_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$660F382A;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo,his_align];
 );

procedure op_movntdqa(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2(ctx,movntdqa_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

procedure op_movnt_sd_ss(var ctx:t_jit_context2);
begin
 op_emit2_simd_mem_reg(ctx,[his_mov,his_wo]);
end;

//

const
 movl_ps_pd_desc:t_op_desc=(
  mem_reg:(op:$0F13;opt:[not_prefix]);
  reg_mem:(op:$0F12;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_movl_ps_pd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2_simd(ctx,movl_ps_pd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 movh_ps_pd_desc:t_op_desc=(
  mem_reg:(op:$0F17;opt:[not_prefix]);
  reg_mem:(op:$0F16;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_mov,his_wo];
 );

procedure op_movh_ps_pd(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2_simd(ctx,movh_ps_pd_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;


//

const
 ldmxcsr_desc:t_op_type=(
  op:$0FAE;index:2;opt:[not_prefix];
 );

procedure op_ldmxcsr(var ctx:t_jit_context2);
begin
 op_emit1(ctx,ldmxcsr_desc,[his_ro]);
end;

const
 stmxcsr_desc:t_op_type=(
  op:$0FAE;index:3;opt:[not_prefix];
 );

procedure op_stmxcsr(var ctx:t_jit_context2);
begin
 op_emit1(ctx,stmxcsr_desc,[his_wo]);
end;

//

procedure init_cbs_sse;
begin
 jit_cbs[OPPnone,OPmov  ,OPSx_sd]:=@op_movsd;
 jit_cbs[OPPnone,OPmov  ,OPSx_ss]:=@op_movss;

 jit_cbs[OPPnone,OPmov  ,OPSx_d ]:=@op_movd_dq;
 jit_cbs[OPPnone,OPmov  ,OPSx_q ]:=@op_movd_dq;

 jit_cbs[OPPnone,OPmov,OPSx_dqa]:=@op_movdqa;
 jit_cbs[OPPnone,OPmov,OPSx_dqu]:=@op_movdqu;

 jit_cbs[OPPnone,OPmovu,OPSx_ps]:=@op_movu_ps_pd;
 jit_cbs[OPPnone,OPmovu,OPSx_pd]:=@op_movu_ps_pd;

 jit_cbs[OPPnone,OPmova,OPSx_ps]:=@op_mova_ps_pd;
 jit_cbs[OPPnone,OPmova,OPSx_pd]:=@op_mova_ps_pd;

 jit_cbs[OPPnone,OPmovl,OPSx_ps]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPmovl,OPSx_pd]:=@op_reg_mem_mov_wo;

 jit_cbs[OPPnone,OPmovh,OPSx_ps]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPmovh,OPSx_pd]:=@op_reg_mem_mov_wo;

 jit_cbs[OPPnone,OPmovhlps,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPmovlh,OPSx_ps]:=@add_orig;

 jit_cbs[OPPnone,OPmovsldup,OPSnone]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPmovshdup,OPSnone]:=@op_reg_mem_mov_wo;

 jit_cbs[OPPnone,OPmovnt,OPSx_dqa]:=@op_movntdqa;
 jit_cbs[OPPnone,OPmovnt,OPSx_dq ]:=@op_mem_reg_mov_wo;
 jit_cbs[OPPnone,OPmovnt,OPSx_i  ]:=@op_mem_reg_mov_wo;
 jit_cbs[OPPnone,OPmovnt,OPSx_ps ]:=@op_mem_reg_mov_wo;
 jit_cbs[OPPnone,OPmovnt,OPSx_pd ]:=@op_mem_reg_mov_wo;
 jit_cbs[OPPnone,OPmovnt,OPSx_q  ]:=@op_mem_reg_mov_wo;

 jit_cbs[OPPnone,OPmovdq2q,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPmovq2dq,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPmovddup,OPSnone]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPmovl,OPSx_ps]:=@op_movl_ps_pd;
 jit_cbs[OPPnone,OPmovl,OPSx_pd]:=@op_movl_ps_pd;

 jit_cbs[OPPnone,OPmovh,OPSx_ps]:=@op_movh_ps_pd;
 jit_cbs[OPPnone,OPmovh,OPSx_pd]:=@op_movh_ps_pd;

 jit_cbs[OPPnone,OPpmovsx,OPSv_bw]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPpmovsx,OPSv_bd]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPpmovsx,OPSv_bq]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPpmovsx,OPSv_wd]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPpmovsx,OPSv_wq]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPpmovsx,OPSv_dq]:=@op_reg_mem_mov_wo;

 jit_cbs[OPPnone,OPpmovzx,OPSv_bw]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPpmovzx,OPSv_bd]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPpmovzx,OPSv_bq]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPpmovzx,OPSv_wd]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPpmovzx,OPSv_wq]:=@op_reg_mem_mov_wo;
 jit_cbs[OPPnone,OPpmovzx,OPSv_dq]:=@op_reg_mem_mov_wo;

 //jit_cbs[OPPnone,OPmovmsk  ,OPSx_ps]:=@op_rr;
 //jit_cbs[OPPnone,OPmovmsk  ,OPSx_pd]:=@op_rr;
 //jit_cbs[OPPnone,OPpmovmskb,OPSnone]:=@op_rr;

 jit_cbs[OPPnone,OPcomi ,OPSx_ss]:=@op_reg_mem_ro;
 jit_cbs[OPPnone,OPcomi ,OPSx_sd]:=@op_reg_mem_ro;

 jit_cbs[OPPnone,OPucomi,OPSx_ss]:=@op_reg_mem_ro;
 jit_cbs[OPPnone,OPucomi,OPSx_sd]:=@op_reg_mem_ro;

 jit_cbs[OPPnone,OPxor,OPSx_ps]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPxor,OPSx_pd]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpxor,OPSnone]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPor ,OPSx_ps]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPor ,OPSx_pd]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpor,OPSnone]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPand  ,OPSx_ps]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPand  ,OPSx_pd]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPandn ,OPSx_ps]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPandn ,OPSx_pd]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPpand ,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpandn,OPSnone]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPpcmpeq,OPSx_b ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpcmpeq,OPSx_w ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpcmpeq,OPSx_d ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpcmpeq,OPSx_q ]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPpcmpgt,OPSx_b ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpcmpgt,OPSx_w ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpcmpgt,OPSx_d ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpcmpgt,OPSx_q ]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPpcmpestrm,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpcmpestri,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpcmpistrm,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpcmpistri,OPSnone]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPdiv ,OPSx_ps]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPdiv ,OPSx_pd]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPdiv ,OPSx_ss]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPdiv ,OPSx_sd]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPmul ,OPSx_ps]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPmul ,OPSx_pd]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPmul ,OPSx_sd]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPmul ,OPSx_ss]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPpmull ,OPSx_d]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpmull ,OPSx_w]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPpmuludq ,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpmulhuw ,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpmulhrsw,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpmulhw  ,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpmuldq  ,OPSnone]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPadd ,OPSx_sd]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPadd ,OPSx_ss]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpadd,OPSx_b ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpadd,OPSx_w ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpadd,OPSx_d ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpadd,OPSx_q ]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPsub ,OPSx_sd]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPsub ,OPSx_ss]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpsub,OPSx_b ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpsub,OPSx_w ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpsub,OPSx_d ]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPpmaddubsw,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpmaddwd  ,OPSnone]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPcvtsi2 ,OPSx_ss]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPcvtsi2 ,OPSx_sd]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPcvtss2 ,OPSx_sd]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPcvtss2 ,OPSx_si]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPcvtsd2 ,OPSx_ss]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPcvtsd2 ,OPSx_si]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPcvttps2,OPSx_dq]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPcvttps2,OPSx_pi]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPcvttpd2,OPSx_dq]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPcvttpd2,OPSx_pi]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPcvtdq2 ,OPSx_ps]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPcvtdq2 ,OPSx_pd]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPcvttss2,OPSx_si]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPcvttsd2,OPSx_si]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPcvtpd2 ,OPSx_ps]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPcvtpd2 ,OPSx_dq]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPcvtpd2 ,OPSx_pi]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPcvtps2 ,OPSx_pd]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPcvtps2 ,OPSx_dq]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPcvtps2 ,OPSx_pi]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPsqrt,OPSx_ps]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPsqrt,OPSx_pd]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPsqrt,OPSx_sd]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPsqrt,OPSx_ss]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPrsqrt,OPSx_ps]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPrsqrt,OPSx_ss]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPrcp ,OPSx_ps]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPrcp ,OPSx_ss]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPpshuf,OPSx_b ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpshuf,OPSx_d ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpshuf,OPSx_hw]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpshuf,OPSx_lw]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpshuf,OPSx_w ]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPpsra,OPSx_w ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpsra,OPSx_d ]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPpsrl,OPSx_w ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpsrl,OPSx_d ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpsrl,OPSx_q ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpsrl,OPSx_dq]:=@add_orig;

 jit_cbs[OPPnone,OPpsll,OPSx_w ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpsll,OPSx_d ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpsll,OPSx_q ]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpsll,OPSx_dq]:=@add_orig;

 jit_cbs[OPPnone,OPpacksswb,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpackssdw,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpackusdw,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpackuswb,OPSnone]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPpunpcklbw ,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpunpcklwd ,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpunpckldq ,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpunpcklqdq,OPSnone]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPpunpckhbw ,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpunpckhwd ,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpunpckhdq ,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPpunpckhqdq,OPSnone]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPunpckl    ,OPSx_pd]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPunpckl    ,OPSx_ps]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPunpckh    ,OPSx_pd]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPunpckh    ,OPSx_ps]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPpalignr,OPSnone]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPldmxcsr,OPSnone]:=@op_ldmxcsr;
 jit_cbs[OPPnone,OPstmxcsr,OPSnone]:=@op_stmxcsr;

 if _SSE4aSupport then
 begin
  jit_cbs[OPPnone,OPmovnt,OPSx_sd]:=@op_movnt_sd_ss;
  jit_cbs[OPPnone,OPmovnt,OPSx_ss]:=@op_movnt_sd_ss;
 end else
 begin
  jit_cbs[OPPnone,OPmovnt,OPSx_sd]:=@op_movsd;
  jit_cbs[OPPnone,OPmovnt,OPSx_ss]:=@op_movss;
 end;

 jit_cbs[OPPnone,OPaeskeygenassist,OPSnone]:=@op_reg_mem_wo;
 jit_cbs[OPPnone,OPaesimc         ,OPSnone]:=@op_reg_mem_wo;

 jit_cbs[OPPnone,OPaesenc    ,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPaesenclast,OPSnone]:=@op_reg_mem_rw;

 jit_cbs[OPPnone,OPaesdec    ,OPSnone]:=@op_reg_mem_rw;
 jit_cbs[OPPnone,OPaesdeclast,OPSnone]:=@op_reg_mem_rw;

end;

procedure SetupSupport;
var
 _ecx:longint;
begin
 asm
    movl $0x80000001,%eax
    cpuid
    movl %ecx,_ecx
 end ['rax','rbx','rcx','rdx'];
 _SSE4aSupport:=(_ecx and $40)<>0;
end;

initialization
 SetupSupport;
 init_cbs_sse;

end.

