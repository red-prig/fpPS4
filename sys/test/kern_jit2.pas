unit kern_jit2;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_thr,
 ucontext,
 vmparam,
 vm_pmap,
 systm,
 trap,
 x86_fpdbgdisas,
 x86_jit,
 kern_stub,
 kern_jit2_ctx,
 kern_jit2_ops_avx;

procedure pick(var ctx:t_jit_context2);

implementation

uses
 sysutils,
 kern_jit2_ops;

procedure op_jmp_dispatcher(var ctx:t_jit_context2);
begin
 ctx.builder.call_far(nil); //input:rax TODO jmp dispatcher
end;

procedure op_push_rip(var ctx:t_jit_context2;used_r_tmp0:Boolean);
var
 i:Integer;
 stack:TRegValue;
 imm:Int64;
begin
 with ctx.builder do
 begin
  if used_r_tmp0 then
  begin
   push(r_tmp0);
  end;

  stack:=r_tmp0;

  i:=GetFrameOffset(rsp);
  movq(stack,[r_thrd+i]);
  leaq(stack,[stack-8]);
  movq([r_thrd+i],stack);

  call_far(@uplift_jit); //in/out:rax uses:r14

  imm:=Int64(ctx.ptr_next);

  if (classif_offset_se64(imm)=os64) then
  begin
   movi64(r_tmp1,imm);
   movq([stack],r_tmp1);
  end else
  begin
   movi(os64,[stack],imm);
  end;

  if used_r_tmp0 then
  begin
   pop(r_tmp0);
  end;
 end;
end;

procedure op_pop_rip(var ctx:t_jit_context2);
var
 i:Integer;
 new,stack1,stack2:TRegValue;
begin
 with ctx.builder do
 begin
  new   :=r_tmp0;
  stack1:=r_tmp0;
  stack2:=r_tmp1;

  i:=GetFrameOffset(rsp);
  movq(stack1,[r_thrd+i]);

  call_far(@uplift_jit); //in/out:rax uses:r14

  movq(stack2,stack1);

  movq(new,[stack1]);

  leaq(stack2,[stack2+8]);
  movq([r_thrd+i],stack2);
 end;
end;

function is_rsp(const r:TRegValue):Boolean; inline;
begin
 Result:=False;
 case r.AType of
  regGeneral:
   case r.AIndex of
     4:Result:=True;
    else;
   end;
  else;
 end;
end;

function is_rsp(const r:TRegValues):Boolean; inline;
begin
 Result:=is_rsp(r[0]) or is_rsp(r[1]);
end;

procedure op_call(var ctx:t_jit_context2);
var
 id:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 new1,new2:TRegValue;
 i:Integer;
 label_id:t_jit_i_link;
begin
 if (ctx.din.Operand[1].RegValue[0].AType=regNone) then
 begin
  ofs:=0;
  if not GetTargetOfs(ctx.din,ctx.code,1,ofs) then
  begin
   Assert(false);
  end;

  dst:=ctx.ptr_next+ofs;

  label_id:=ctx.find_label(dst);

  if (label_id<>nil_link) then
  begin
   op_push_rip(ctx,false);
   //
   ctx.builder.jmp(label_id);
  end else
  begin
   op_push_rip(ctx,false);
   //
   id:=ctx.builder.jmp(nil_link);
   ctx.add_forward_point(id,dst);
  end;
 end else
 if is_memory(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  if is_rsp(ctx.din.Operand[1].RegValue) then
  begin
   build_lea(ctx,1,new1);
   //
   op_push_rip(ctx,true);
  end else
  begin
   op_push_rip(ctx,false);
   //
   build_lea(ctx,1,new1);
  end;
  //
  op_jmp_dispatcher(ctx);
 end else
 if is_preserved(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  op_push_rip(ctx,false);
  //
  i:=GetFrameOffset(ctx.din.Operand[1].RegValue[0]);
  ctx.builder.movq(new1,[r_thrd+i]);
  //
  op_jmp_dispatcher(ctx);
 end else
 begin
  op_push_rip(ctx,false);
  //
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  new2:=new_reg(ctx.din.Operand[1]);
  //
  ctx.builder.movq(new1,new2);
  //
  op_jmp_dispatcher(ctx);
 end;
end;

procedure op_ret(var ctx:t_jit_context2);
begin
 Assert(ctx.din.Operand[1].ByteCount=0);

 op_pop_rip(ctx); //out:rax
 //
 op_jmp_dispatcher(ctx);
 //
 ctx.ptr_next:=nil; //trim
end;

procedure op_jmp(var ctx:t_jit_context2);
var
 id:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 new1,new2:TRegValue;
 i:Integer;
 label_id:t_jit_i_link;
begin
 if (ctx.din.Operand[1].RegValue[0].AType=regNone) then
 begin
  ofs:=0;
  if not GetTargetOfs(ctx.din,ctx.code,1,ofs) then
  begin
   Assert(false);
  end;

  dst:=ctx.ptr_next+ofs;

  label_id:=ctx.find_label(dst);

  if (label_id<>nil_link) then
  begin
   ctx.builder.jmp(label_id);
  end else
  begin
   id:=ctx.builder.jmp(nil_link);
   ctx.add_forward_point(id,dst);
  end;

 end else
 if is_memory(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  build_lea(ctx,1,new1);
  //
  op_jmp_dispatcher(ctx);
 end else
 if is_preserved(ctx.din) then
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  i:=GetFrameOffset(ctx.din.Operand[1].RegValue[0]);
  ctx.builder.movq(new1,[r_thrd+i]);
  //
  op_jmp_dispatcher(ctx);
 end else
 begin
  new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  new2:=new_reg(ctx.din.Operand[1]);
  //
  ctx.builder.movq(new1,new2);
  //
  op_jmp_dispatcher(ctx);
 end;
 //
 ctx.ptr_next:=nil; //trim
end;

procedure op_jcc(var ctx:t_jit_context2);
var
 id:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
 label_id:t_jit_i_link;
begin
 ofs:=0;
 if not GetTargetOfs(ctx.din,ctx.code,1,ofs) then
 begin
  Assert(false);
 end;

 dst:=ctx.ptr_next+ofs;

 label_id:=ctx.find_label(dst);

 if (label_id<>nil_link) then
 begin
  ctx.builder.jcc(ctx.din.OpCode.Suffix,label_id);
 end else
 begin
  id:=ctx.builder.jcc(ctx.din.OpCode.Suffix,nil_link);
  ctx.add_forward_point(id,dst);
 end;
end;

const
 movsx8_desc:t_op_type=(op:$0FBE);
 movsxd_desc:t_op_type=(op:$63);

procedure op_push(var ctx:t_jit_context2);
var
 i:Integer;
 imm:Int64;
 stack,new:TRegValue;
begin
 with ctx.builder do
 begin
  stack:=r_tmp0;

  if is_memory(ctx.din) then
  begin
   build_lea(ctx,1,r_tmp0);

   call_far(@uplift_jit); //in/out:rax uses:r14

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

   i:=GetFrameOffset(ctx.din.Operand[1]);
   movq(new,[r_thrd+i]);
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

  i:=GetFrameOffset(rsp);
  movq(stack,[r_thrd+i]);
  leaq(stack,[stack-OPERAND_BYTES[new.ASize]]);
  movq([r_thrd+i],stack);

  call_far(@uplift_jit); //in/out:rax uses:r14

  movq([stack],new);
 end;
end;

procedure op_pushfq(var ctx:t_jit_context2);
var
 i:Integer;
 mem_size:TOperandSize;
 stack,new:TRegValue;
begin
 with ctx.builder do
 begin
  stack:=r_tmp0;

  new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

  mem_size:=ctx.din.Operand[1].Size;

  pushfq(mem_size);
  pop(new);

  i:=GetFrameOffset(rsp);
  movq(stack,[r_thrd+i]);
  leaq(stack,[stack-OPERAND_BYTES[new.ASize]]);
  movq([r_thrd+i],stack);

  call_far(@uplift_jit); //in/out:rax uses:r14

  movq([stack],new);
 end;
end;


procedure op_pop(var ctx:t_jit_context2);
var
 i:Integer;
 new,stack:TRegValue;
begin
 with ctx.builder do
 begin
  stack:=r_tmp0;

  i:=GetFrameOffset(rsp);
  movq(stack,[r_thrd+i]);

  call_far(@uplift_jit); //in/out:rax uses:r14

  if is_memory(ctx.din) then
  begin
   new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

   movq(new,[stack]);

   build_lea(ctx,1,r_tmp0);

   call_far(@uplift_jit); //in/out:rax uses:r14

   movq([r_tmp0],new);
  end else
  if is_preserved(ctx.din) then
  begin
   new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

   movq(new,[stack]);

   i:=GetFrameOffset(ctx.din.Operand[1]);
   movq([r_thrd+i],new);
  end else
  begin
   new:=new_reg(ctx.din.Operand[1]);

   movq(new,[stack]);
  end;

  i:=GetFrameOffset(rsp);
  movq(stack,[r_thrd+i]);
  leaq(stack,[stack+OPERAND_BYTES[new.ASize]]);
  movq([r_thrd+i],stack);
 end;
end;

procedure op_syscall(var ctx:t_jit_context2);
begin
 ctx.builder.call_far(nil); //TODO syscall dispatcher
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
  $41: //assert?
   begin
    //
    ctx.builder.call_far(nil); //TODO error dispatcher
   end;

  $44: //system error?
   begin
    //
    ctx.builder.call_far(nil); //TODO error dispatcher
    ctx.ptr_next:=nil; //trim
   end;
  else
   begin
    Assert(False);
   end;
 end;
end;

procedure op_ud2(var ctx:t_jit_context2);
begin
 //exit proc?
 ctx.builder.call_far(nil); //TODO exit dispatcher
 ctx.ptr_next:=nil; //trim
end;

procedure op_iretq(var ctx:t_jit_context2);
begin
 //exit proc?
 ctx.builder.call_far(nil); //TODO exit dispatcher
 ctx.ptr_next:=nil; //trim
end;

procedure op_hlt(var ctx:t_jit_context2);
begin
 //stop thread?
 ctx.builder.call_far(nil); //TODO exit dispatcher
end;

procedure op_cpuid(var ctx:t_jit_context2);
begin
 ctx.builder.call_far(nil); //TODO CPUID
end;

procedure op_rdtsc(var ctx:t_jit_context2);
begin
 add_orig(ctx);
 op_save_rax(ctx,ctx.builder.rax);
end;

procedure op_nop(var ctx:t_jit_context2);
begin
 //align?
end;

var
 inited:Integer=0;

procedure init_cbs;
begin
 if (inited<>0) then Exit;

 jit_cbs[OPPnone,OPcall,OPSnone]:=@op_call;
 jit_cbs[OPPnone,OPjmp ,OPSnone]:=@op_jmp;
 jit_cbs[OPPnone,OPret ,OPSnone]:=@op_ret;

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

 jit_cbs[OPPnone,OPpush,OPSnone]:=@op_push;
 jit_cbs[OPPnone,OPpop ,OPSnone]:=@op_pop;

 jit_cbs[OPPnone,OPpushf ,OPSnone]:=@op_pushfq;
 jit_cbs[OPPnone,OPpushf ,OPSx_q ]:=@op_pushfq;
 //
 //jit_cbs[OPpopf  ,OPSnone]:=@;
 //jit_cbs[OPpopf  ,OPSx_q ]:=@;

 jit_cbs[OPPnone,OPsyscall,OPSnone]:=@op_syscall;
 jit_cbs[OPPnone,OPint    ,OPSnone]:=@op_int;
 jit_cbs[OPPnone,OPud2    ,OPSnone]:=@op_ud2;

 jit_cbs[OPPnone,OPiret,OPSnone]:=@op_iretq;
 jit_cbs[OPPnone,OPiret,OPSx_d ]:=@op_iretq;
 jit_cbs[OPPnone,OPiret,OPSx_q ]:=@op_iretq;

 jit_cbs[OPPnone,OPhlt ,OPSnone]:=@op_hlt;

 jit_cbs[OPPnone,OPcpuid,OPSnone]:=@op_cpuid;
 jit_cbs[OPPnone,OPrdtsc,OPSnone]:=@op_rdtsc;

 jit_cbs[OPPnone,OPnop,OPSnone]:=@op_nop;

 kern_jit2_ops.init_cbs;
 init_cbs_avx;

 inited:=1;
end;

procedure pick(var ctx:t_jit_context2);
const
 SCODES:array[TSimdOpcode] of Byte=(0,0,1,3,2);
 MCODES:array[0..3] of RawByteString=('','0F','0F38','0F3A');
label
 _next;
var
 addr:Pointer;
 links:t_jit_context2.t_forward_links;

 proc:TDbgProcess;
 adec:TX86AsmDecoder;
 ptr,fin:Pointer;
 ACodeBytes,ACode:RawByteString;

 cb:t_jit_cb;

 node_new,node_curr:t_jit_i_link;
 node,node_code1,node_code2:p_jit_instruction;

 data:Pointer;
 F:THandle;
begin

 init_cbs;

 ctx.max:=QWORD(ctx.max_forward_point);
 Writeln(' ctx.text_start:0x',HexStr(ctx.text_start,16));
 Writeln(' ctx.max       :0x',HexStr(ctx.max,16));
 Writeln(' ctx.text___end:0x',HexStr(ctx.text___end,16));

 links:=Default(t_jit_context2.t_forward_links);
 addr:=nil;

 if not ctx.fetch_forward_point(links,addr) then
 begin
  Assert(false);
 end;

 ptr:=addr;
 fin:=Pointer(Int64(-1));

 proc:=TDbgProcess.Create(dm64);
 adec:=TX86AsmDecoder.Create(proc);

 while (ptr<fin) do
 begin

  if ((pmap_get_raw(QWORD(ptr)) and PAGE_PROT_EXECUTE)=0) then
  begin
   writeln('not excec:0x',HexStr(ptr));
   goto _next;
  end;

  ctx.ptr_curr:=ptr;

  adec.Disassemble(ptr,ACodeBytes,ACode);

  case adec.Instr.OpCode.Opcode of
   OPX_Invalid..OPX_GroupP:
    begin
     //invalid
     writeln('invalid:0x',HexStr(ctx.ptr_curr));

     ptr:=ctx.ptr_curr;
     adec.Disassemble(ptr,ACodeBytes,ACode);


     goto _next;
    end;
   else;
  end;

  if (adec.Instr.Flags * [ifOnly32, ifOnly64, ifOnlyVex] <> []) or
     is_invalid(adec.Instr) then
  begin
   writeln('invalid:0x',HexStr(ctx.ptr_curr));
   goto _next;
  end;

  Writeln('original------------------------':32,' ','0x',HexStr(ptr-adec.Disassembler.CodeIdx));
  Writeln(ACodeBytes:32,' ',ACode);
  Writeln('original------------------------':32,' ','0x',HexStr(ptr));

  ctx.ptr_next:=ptr;

  ctx.code:=ctx.ptr_curr;

  ctx.dis:=adec.Disassembler;
  ctx.din:=adec.Instr;

  cb:=jit_cbs[ctx.din.OpCode.Prefix,ctx.din.OpCode.Opcode,ctx.din.OpCode.Suffix];

  if (cb=nil) then
  begin
   Writeln('Unhandled jit:',
           ctx.din.OpCode.Prefix,' ',
           ctx.din.OpCode.Opcode,' ',
           ctx.din.OpCode.Suffix,' ',
           ctx.din.Operand[1].Size,' ',
           ctx.din.Operand[2].Size);
   Writeln('MIndex=',ctx.dis.ModRM.Index,' ',
           'SOpcode=',ctx.dis.SimdOpcode,':',SCODES[ctx.dis.SimdOpcode],' ',
           'mm=',ctx.dis.mm,':',MCODES[ctx.dis.mm and 3]);
   Assert(false);
  end;

  node_curr:=ctx.builder.get_curr_label.after;
  node_code1:=node_curr._node;

  cb(ctx);

  node_code2:=ctx.builder.get_curr_label._node;

  if (node_code1<>node_code2) and
     (node_code1<>nil) then
  begin
   node:=TAILQ_NEXT(node_code1,@node_code1^.link);

   Writeln('recompiled----------------------':32,' ','');
   while (node<>nil) do
   begin

    print_disassemble(@node^.AData,node^.ASize);


    node:=TAILQ_NEXT(node,@node^.link);
   end;
   Writeln('recompiled----------------------':32,' ','');
  end;

  //if (node_code1<>node_code2) then
  begin
   ctx.add_label(ctx.ptr_curr,node_curr);
  end;

  if (links.root<>nil) then
  begin
   links.Resolve(node_curr);
   links.root:=nil;
  end;

  //if (len1<>len2) then
  begin
   node_new:=ctx.find_label(ptr);

   if (node_new<>nil_link) then
   begin
    ctx.builder.jmp(node_new);
    ctx.ptr_next:=nil;
    Writeln('jmp next:0x',HexStr(ptr));
   end;

  end;

  if (ctx.ptr_next=nil) then
  begin
   _next:
   repeat

    if not ctx.fetch_forward_point(links,addr) then
    begin
     ctx.builder.ud2;

     data:=AllocMem(ctx.builder.GetMemSize);
     ctx.builder.SaveTo(data,ctx.builder.GetMemSize);

     F:=FileCreate('recompile.bin');
     FileWrite(F,data^,ctx.builder.GetMemSize);
     FileClose(F);

     Assert(false);
    end;

    node_new:=ctx.find_label(addr);
    if (node_new=nil_link) then
    begin
     Writeln('not found:0x',HexStr(addr));
     writeln;
     Break;
    end else
    begin
     links.Resolve(node_new);
     links.root:=nil;
    end;

   until false;

   ptr:=addr;
  end;

 end;

 adec.Free;
 proc.Free;
end;


end.

