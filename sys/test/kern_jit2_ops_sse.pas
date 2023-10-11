unit kern_jit2_ops_sse;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 x86_fpdbgdisas,
 x86_jit,
 kern_jit2_ops,
 kern_jit2_ctx;

implementation

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

procedure op_emit2_simd_reg_mem(var ctx:t_jit_context2;op:DWORD;hint:t_op_hint);
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
 tmp.reg_mem.op:=op or ctx.dis.opcode;
 tmp.hint:=hint;

 op_emit2_simd(ctx,tmp);
end;

//

procedure op_reg_mem_0F_rw(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2_simd_reg_mem(ctx,$0F00,[his_rw]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_reg_mem_0F_wo(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2_simd_reg_mem(ctx,$0F00,[his_wo]);
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
 pshufb_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$0F3800;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_rw];
 );

procedure op_pshufb(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2_simd(ctx,pshufb_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

//

const
 palignr_desc:t_op_desc=(
  mem_reg:(opt:[not_impl]);
  reg_mem:(op:$0F3A0F;opt:[not_prefix]);
  reg_imm:(opt:[not_impl]);
  reg_im8:(opt:[not_impl]);
  hint:[his_wo];
 );

procedure op_palignr(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit2(ctx,palignr_desc);
 end else
 begin
  add_orig(ctx);
 end;
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

 jit_cbs[OPPnone,OPmovdq2q,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPmovq2dq,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPor,OPSx_ps]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPor,OPSx_pd]:=@op_reg_mem_0F_rw;

 jit_cbs[OPPnone,OPpor  ,OPSnone]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpxor ,OPSnone]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpand ,OPSnone]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpandn,OPSnone]:=@op_reg_mem_0F_rw;

 jit_cbs[OPPnone,OPadd ,OPSx_sd]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPadd ,OPSx_ss]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPsub ,OPSx_sd]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPsub ,OPSx_ss]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPmul ,OPSx_sd]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPmul ,OPSx_ss]:=@op_reg_mem_0F_rw;

 jit_cbs[OPPnone,OPpadd,OPSx_b]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpadd,OPSx_w]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpadd,OPSx_d]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpadd,OPSx_q]:=@op_reg_mem_0F_rw;

 jit_cbs[OPPnone,OPpsub,OPSx_b]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpsub,OPSx_w]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpsub,OPSx_d]:=@op_reg_mem_0F_rw;

 jit_cbs[OPPnone,OPcvtsi2,OPSx_ss]:=@op_reg_mem_0F_wo;
 jit_cbs[OPPnone,OPcvtsd2,OPSx_si]:=@op_reg_mem_0F_wo;
 jit_cbs[OPPnone,OPcvtss2,OPSx_si]:=@op_reg_mem_0F_wo;

 jit_cbs[OPPnone,OPsqrt,OPSx_sd]:=@op_reg_mem_0F_wo;
 jit_cbs[OPPnone,OPsqrt,OPSx_ss]:=@op_reg_mem_0F_wo;

 jit_cbs[OPPnone,OPpshuf,OPSx_b ]:=@op_pshufb;
 jit_cbs[OPPnone,OPpshuf,OPSx_d ]:=@op_reg_mem_0F_wo;
 jit_cbs[OPPnone,OPpshuf,OPSx_hw]:=@op_reg_mem_0F_wo;
 jit_cbs[OPPnone,OPpshuf,OPSx_lw]:=@op_reg_mem_0F_wo;
 jit_cbs[OPPnone,OPpshuf,OPSx_w ]:=@op_reg_mem_0F_wo;

 jit_cbs[OPPnone,OPpsrl,OPSx_w ]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpsrl,OPSx_d ]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpsrl,OPSx_q ]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpsrl,OPSx_dq]:=@add_orig;

 jit_cbs[OPPnone,OPpsll,OPSx_w ]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpsll,OPSx_d ]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpsll,OPSx_q ]:=@op_reg_mem_0F_rw;
 jit_cbs[OPPnone,OPpsll,OPSx_dq]:=@add_orig;

 jit_cbs[OPPnone,OPpalignr,OPSnone]:=@op_palignr;

end;

initialization
 init_cbs_sse;

end.

