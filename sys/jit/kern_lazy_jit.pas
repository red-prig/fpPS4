unit kern_lazy_jit;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 strutils,
 //subr_backtrace,
 x86_fpdbgdisas,
 x86_jit,
 kern_thr,
 kern_jit_ctx,
 kern_jit_asm;

//simplified recompilation mode, for debugging purposes only
const
 use_lazy_jit=False;

function op_lazy_jit(var ctx:t_jit_context2):Boolean;

implementation

type
 TModRM=bitpacked record
  RM   :0..7;
  Index:0..7;
  Mode :0..3;
 end;
 TREX=bitpacked record
  B:0..1;
  X:0..1;
  R:0..1;
  W:0..1;
 end;
 TRXBm=bitpacked record
  MM:0..30;
  NB:0..1;
  NX:0..1;
  NR:0..1;
 end;
 TRvvv=bitpacked record //VEX2
  OP:0..2;
  VL:0..1;
  VI:0..15;
  NR:0..1;
 end;
 TWvvv=bitpacked record //VEX3
  OP:0..2;
  VL:0..1;
  VI:0..15;
   W:0..1;
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

type
 t_preserved_regs=Set of (_rip,_rsp,_rbp,_r13,_r14,_r15);

function get_preserved_regs(const r:TRegValue):t_preserved_regs;
begin
 Result:=[];

 case r.AType of
  regRip    :Result:=[_rip];
  regGeneral:
   case r.AIndex of
     4:Result:=[_rsp];
     5:Result:=[_rbp];
    13:Result:=[_r13];
    14:Result:=[_r14];
    15:Result:=[_r15];
   end;
  else;
 end;

end;

function get_preserved_regs(const r:TRegValues):t_preserved_regs; inline;
begin
 Result:=get_preserved_regs(r[0]) + get_preserved_regs(r[1]);
end;

function get_preserved_regs(const r:TOperand):t_preserved_regs; inline;
begin
 Result:=get_preserved_regs(r.RegValue);
end;

function get_preserved_regs(const r:TInstruction):t_preserved_regs;
var
 i:Integer;
begin
 Result:=[];
 //
 if (r.OperCnt<>0) then
 For i:=1 to r.OperCnt do
 begin
  Result:=Result + get_preserved_regs(r.Operand[i]);
 end;
 //
 if is_push_op(r.OpCode.Opcode) then
 begin
  Result:=Result + [_rsp];
 end;
end;

type
 t_rip_replace_ctx=record
  reg_imm:TRegValue;
  reg_rmm:Byte;
  reg_str:string[3];
 end;

function get_rip_replace_ctx(p_regs:t_preserved_regs;var ctx:t_rip_replace_ctx):Boolean;
begin
 if not (_r14 in p_regs) then
 begin
  ctx.reg_imm:=t_jit_builder.r14;
  ctx.reg_rmm:=6;
  ctx.reg_str:='r14';
  Result:=True;
 end else
 if not (_r15 in p_regs) then
 begin
  ctx.reg_imm:=t_jit_builder.r15;
  ctx.reg_rmm:=7;
  ctx.reg_str:='r15';
  Result:=True;
 end else
 if not (_r13 in p_regs) then
 begin
  ctx.reg_imm:=t_jit_builder.r13;
  ctx.reg_rmm:=5;
  ctx.reg_str:='r13';
  Result:=True;
 end else
 begin
  Result:=False;
 end;
end;

function get_disassemble_str(addr:Pointer):RawByteString;
var
 proc:TDbgProcess;
 adec:TX86AsmDecoder;
 ptr:Pointer;
 ACodeBytes,ACode:RawByteString;
begin
 Result:='';
 ptr:=addr;

 proc:=TDbgProcess.Create(dm64);
 adec:=TX86AsmDecoder.Create(proc);

 adec.Disassemble(ptr,ACodeBytes,ACode);
 Result:=ACode;

 adec.Free;
 proc.Free;
end;

function check_dis_rip(src,dst:Pointer;const name:string):Boolean;
var
 s,d:RawByteString;
begin
 s:=get_disassemble_str(src);
 d:=get_disassemble_str(dst);

 s:=ReplaceStr(s, 'rip', name);

 Result:=(s=d);
end;

function GetRipIdx(var din:TInstruction):Integer;
var
 i:Integer;
begin
 Result:=-1;
 if (din.OperCnt<>0) then
 For i:=1 to din.OperCnt do
 if is_rip(din.Operand[i]) then
 begin
  Exit(din.Operand[i].CodeIndex-1);
 end;
end;

function is_prefix(b:Byte):Boolean; inline;
begin
 case b of
  $26,
  $2E,
  $36,
  $3E,
  $64,
  $65,
  $66,
  $67,
  $F0,
  $F3:Result:=True;
  else
      Result:=False;
 end;
end;

type
 t_rex_type=(_rex,_vex);

 t_rex_rec=record
  rtp:t_rex_type;
  idx:shortint;
  NR :Byte;
 end;

function GetRexIdx(Code:Pbyte;max:Byte):t_rex_rec;
var
 i:Byte;
 b:Byte;
begin
 i:=0;
 while (max<>0) do
 begin

  b:=Code^;

  if (b=$C4) then //VEX3
  begin
   Result.rtp:=_vex;
   Result.idx:=i+1;
   Exit;
  end else
  if (b=$C5) then //VEX2
  begin
   Result.rtp:=_vex;
   Result.idx:=-1;
   Result.NR :=TRvvv(Code[1]).NR;
   Exit;
  end else
  if ((b and $F0)=$40) then
  begin
   Result.rtp:=_rex;
   Result.idx:=i;
   Exit;
  end else
  if not is_prefix(b) then
  begin
   Break;
  end;

  Inc(i);
  Inc(Code);
  Dec(max);
 end;

 Result.rtp:=_rex;
 Result.idx:=-1;
end;

procedure insert_rex(var ji:t_jit_instruction;rex:Byte);
var
 i:Byte;
 b:Byte;
begin
 Move(ji.AData[0],ji.AData[1],ji.AInstructionSize);
 ji.AInstructionSize:=ji.AInstructionSize+1;

 i:=1;
 while (i<ji.AInstructionSize) do
 begin

  b:=ji.AData[i];

  if is_prefix(b) then
  begin
   ji.AData[i-1]:=b;
  end else
  if (b=$C5) then //VEX2
  begin
   ji.AData[i-1]:=$C4; //VEX3
   ji.AData[i+0]:=rex;

   b:=ji.AData[i+1];
   TWvvv(b).W:=0;
   ji.AData[i+1]:=b;
   Break;
  end else
  begin
   ji.AData[i-1]:=rex;
   Break;
  end;

  Inc(i);
 end;

end;

procedure op_prolog(var ctx:t_jit_context2;p_regs:t_preserved_regs);
begin
 with ctx.builder do
 begin
  //save internal stack
  if (_rsp in p_regs) then
  begin
   movq([r13-jit_frame_offset+(@p_kthread(nil)^.td_jctx.rsp)],rsp);
  end;
  if (_rbp in p_regs) then
  begin
   movq([r13-jit_frame_offset+(@p_kthread(nil)^.td_jctx.rbp)],rbp);
  end;

  //load rsp,rbp
  if (_rsp in p_regs) then
  begin
   movq(rsp,[r13+(@p_jit_frame(nil)^.tf_rsp)]);
  end;
  if (_rbp in p_regs) then
  begin
   movq(rbp,[r13+(@p_jit_frame(nil)^.tf_rbp)]);
  end;
  //

  //load r14,r15,r13
  if (_r14 in p_regs) then
  begin
   movq(r14,[r13+(@p_jit_frame(nil)^.tf_r14)]);
  end;
  if (_r15 in p_regs) then
  begin
   movq(r15,[r13+(@p_jit_frame(nil)^.tf_r15)]);
  end;
  if (_r13 in p_regs) then
  begin
   movq(r13,[r13+(@p_jit_frame(nil)^.tf_r13)]);
  end;
 end;
end;

procedure op_epilog(var ctx:t_jit_context2;p_regs:t_preserved_regs);
begin
 with ctx.builder do
 begin
  //save r13
  if (_r13 in p_regs) then
  begin
   movq([GS+teb_jitcall],r13);

   //load curkthread,jit_ctx
   movq(r13,[GS +teb_thread]);
   leaq(r13,[r13+jit_frame_offset]);
  end;

  //load r14,r15
  if (_r14 in p_regs) then
  begin
   movq([r13+(@p_jit_frame(nil)^.tf_r14)],r14);
  end;
  if (_r15 in p_regs) then
  begin
   movq([r13+(@p_jit_frame(nil)^.tf_r15)],r15);
  end;

  //load r13
  if (_r13 in p_regs) then
  begin
   movq(r14,[GS+teb_jitcall]);
   movq([r13+(@p_jit_frame(nil)^.tf_r13)],r14);
  end;

  //load rsp,rbp
  if (_rsp in p_regs) then
  begin
   movq([r13+(@p_jit_frame(nil)^.tf_rsp)],rsp);
  end;
  if (_rbp in p_regs) then
  begin
   movq([r13+(@p_jit_frame(nil)^.tf_rbp)],rbp);
  end;

  //load internal stack
  if (_rsp in p_regs) then
  begin
   movq(rsp,[r13-jit_frame_offset+(@p_kthread(nil)^.td_jctx.rsp)]);
  end;
  if (_rbp in p_regs) then
  begin
   movq(rbp,[r13-jit_frame_offset+(@p_kthread(nil)^.td_jctx.rbp)]);
  end;
  //
 end;
end;

function op_lazy_jit(var ctx:t_jit_context2):Boolean;
var
 p_regs:t_preserved_regs;
 rr_ctx:t_rip_replace_ctx;
 RipIdx:Integer;
 RexIdx:t_rex_rec;
 ModRM:TModRM;
 REX:TREX;
 RXBm:TRXBm;
 ji:t_jit_instruction;
begin
 Result:=False;

 if not use_lazy_jit then
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

 p_regs:=get_preserved_regs(ctx.din);

 if (p_regs=[]) then
 begin
  add_orig(ctx);
  Exit(True);
 end;

 if (_rip in p_regs) then
 begin

  if not get_rip_replace_ctx(p_regs,rr_ctx) then
  begin
   Exit;
  end;

  ji:=default_jit_instruction;
  Move(ctx.code^,ji.AData,ctx.dis.CodeIdx);
  ji.AInstructionSize:=ctx.dis.CodeIdx;

  RipIdx:=GetRipIdx(ctx.din);
  RexIdx:=GetRexIdx(ctx.code,ctx.dis.CodeIdx);

  if (RipIdx>=0) then
  begin
   //RM:6 Mode:2
   Byte(ModRM):=ji.AData[RipIdx];
   ModRM.RM  :=rr_ctx.reg_rmm;
   ModRM.Mode:=2;
   ji.AData[RipIdx]:=Byte(ModRM);
  end;

  case RexIdx.rtp of
   _rex:
    begin
     if (RexIdx.idx>=0) then
     begin
      Byte(REX):=ji.AData[RexIdx.idx];
      REX.B:=1;
      ji.AData[RexIdx.idx]:=Byte(REX);
     end else
     begin
      Byte(REX):=$40;
      REX.B:=1;
      insert_rex(ji,Byte(REX));
     end;
    end;
   _vex:
    begin
     if (RexIdx.idx>=0) then
     begin
      Byte(RXBm):=ji.AData[RexIdx.idx];
      RXBm.NB:=0;
      ji.AData[RexIdx.idx]:=Byte(RXBm);
     end else
     begin
      Byte(RXBm):=$E1;
      RXBm.NB:=0;
      RXBm.NR:=RexIdx.NR;
      insert_rex(ji,Byte(RXBm));
     end;
    end;
  end;

  if check_dis_rip(ctx.code,@ji.AData,rr_ctx.reg_str) then
  begin

   op_prolog(ctx,p_regs);

   op_set_reg_imm(ctx,rr_ctx.reg_imm,Int64(ctx.ptr_next));

   ctx.builder._add(ji);

   op_epilog(ctx,p_regs);

   Exit(True);
  end else
  begin

   //print_disassemble(ctx.code,ctx.dis.CodeIdx);
   //print_disassemble(@ji.AData,ji.AInstructionSize);

   //print_asm:=True;
   Exit(False);
  end;

 end;

 op_prolog(ctx,p_regs);

 add_orig(ctx);

 op_epilog(ctx,p_regs);

 Result:=True;
end;


end.

