unit kern_jit_interrupt;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 windows,
 kern_stub,
 kern_thr,
 kern_jit_dynamic,
 kern_jit_ctx,
 x86_fpdbgdisas,
 x86_jit;

function  GET_JIT_FUNC(rip:qword):Byte; external;
procedure jit_interrupt_ud2; external;
procedure jit_interrupt_ast; external;

function  is_jit_track_blocks(td:p_kthread;Rip:QWORD):Boolean; inline;

procedure JIT_AST_HANDLER(td:p_kthread;var Rip,Rsp:QWORD;var EFlags:DWORD;DrX:PQWORD);

procedure ON_JIT_TRACK(td:p_kthread;Context:PCONTEXT);
function  JIT_TRACK_HANDLER(td:p_kthread;var Rip,Rsp:QWORD;var info:t_instruction_info):Boolean;

implementation

uses
 kern_proc,
 vmparam,
 vm_map,
 vm_pmap,
 vm_pmap_prot,
 vm_tracking_map,
 kern_jit_asm,
 subr_backtrace,
 g_node_splay,
 kern_urcu,
 md_map;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

type
 t_next_addr_type=(naCurr,naNext,naRipUnknow);

 p_label=^t_label;
 t_label=object
  pLeft    :p_label;
  pRight   :p_label;
  curr     :Pointer;
  link     :t_jit_i_link;
  function c(n1,n2:p_label):Integer; static;
 end;
 t_label_set=specialize TNodeSplay<t_label>;

 p_linkg=^t_linkg;
 t_linkg=object
  next       :p_linkg;
  dst        :Pointer;
  instruction:t_jit_i_link;
 end;

 t_jit_interrupt_ctx=object
  orig :QWORD;
  caddr:QWORD;
  start:QWORD;
  __end:QWORD;
  adest:QWORD;
  atype:t_next_addr_type;
  debug:Boolean;
  //
  dis:TX86Disassembler;
  din:TInstruction;
  //
  code    :Pointer;
  ptr_curr:Pointer;
  ptr_next:Pointer;
  //
  builder  :t_jit_builder;
  label_set:t_label_set;
  link_list:p_linkg;
  //
  procedure Init(addr:Pointer;info:p_jit_addr_info);
  function  add_label(curr:Pointer;link:t_jit_i_link):p_label;
  function  get_label(src:Pointer):p_label;
  function  add_linkg(instruction:t_jit_i_link;dst:Pointer):p_linkg;
 end;

function t_label.c(n1,n2:p_label):Integer;
begin
 Result:=Integer(n1^.curr>n2^.curr)-Integer(n1^.curr<n2^.curr);
end;

procedure t_jit_interrupt_ctx.Init(addr:Pointer;info:p_jit_addr_info);
begin
 orig :=info^.original;
 caddr:=QWORD(addr);
 start:=info^.recompil;
 __end:=start + info^.jflags.recompil;
end;

function t_jit_interrupt_ctx.add_label(curr:Pointer;link:t_jit_i_link):p_label;
var
 node:t_label;
begin
 if (curr=nil) then Exit;
 node.curr:=curr;
 Result:=label_set.Find(@node);
 if (Result<>nil) then Exit;
 Result:=builder.Alloc(Sizeof(t_label));
 //
 Result^.curr:=curr;
 Result^.link:=link;
 //
 label_set.Insert(Result);
end;

function t_jit_interrupt_ctx.get_label(src:Pointer):p_label;
var
 node:t_label;
begin
 Result:=nil;
 node.curr:=src;
 Result:=label_set.Find(@node);
end;

function t_jit_interrupt_ctx.add_linkg(instruction:t_jit_i_link;dst:Pointer):p_linkg;
begin
 if (dst=nil) then Exit;
 Result:=builder.Alloc(Sizeof(t_linkg));
 Result^.instruction:=instruction;
 Result^.dst        :=dst;
 Result^.next       :=link_list;
 link_list:=Result;
end;

////

function FixupEnd(addr:Pointer;rstart,rend:QWORD):Pointer;
var
 beg:Pointer;
 ptr:Pointer;
 ofs:Int64;

 dis:TX86Disassembler;
 din:TInstruction;
begin
 if (QWORD(addr)=rend) then Exit(addr);

 dis:=Default(TX86Disassembler);
 din:=Default(TInstruction);

 ptr:=addr;

 repeat

  beg:=ptr;
  dis.Disassemble(dm64,ptr,din);

  case din.OpCode.Opcode of
   OPjmp:
    begin
     if (din.Operand[1].RegValue[0].AType=regNone) then
     begin
      //imm offset
      ofs:=0;
      GetTargetOfs(din,beg,1,ofs);
      ofs:=QWORD(ptr)+ofs;

      if (ofs=rend) then
      begin
       Exit(Pointer(rend));
      end else
      if (ofs>=rstart) and
         (ofs< rend) then
      begin
       //next
       ptr:=Pointer(ofs);
      end else
      begin
       //oob jmp
       Break;
      end;

     end else
     begin
      //other jmp
      Break;
     end;
    end;

   else
    //any instr
    Break;
  end;

 until false;

 //not
 Exit(addr);
end;

//

procedure rev_dispatcher(addr:Pointer); public;
var
 info:t_jit_addr_info;
 fin:Pointer;
begin
 urcu_qs(curkthread,True);

 if exist_jit_host(addr,@info) then
 begin

  if (info.recompil = QWORD(addr)) then
  begin
   with curkthread^.td_frame do
   begin
    tf_rip:=(info.original); //need by AST
    LOG_TRACE('rev_dispatcher:0:',HexStr(addr),'->',HexStr(info.original,16));
   end;
   Exit;
  end;

  fin :=Pointer(info.recompil + info.jflags.recompil);
  addr:=FixupEnd(addr,QWORD(info.recompil),QWORD(fin));

  if (addr=fin) then
  begin

   with curkthread^.td_frame do
   begin
    tf_rip:=(info.original + info.jflags.original); //need by AST

    //recheck
    exist_jit_host(addr,@info);

    Assert((info.original = tf_rip),'rev_dispatcher:1');

    //LOG_TRACE('rev_dispatcher:2:',HexStr(addr),'->',HexStr(info.original,16));
   end;

   Exit;
  end else
  begin
   LOG_CRITICAL(StdErr,'rev_dispatcher:3:',HexStr(addr));
   Assert(False,'rev_dispatcher:3');
  end;

 end else
 begin
  LOG_CRITICAL(StdErr,'rev_dispatcher:4:',HexStr(addr));
  Assert(False,'rev_dispatcher:4');
 end;
end;

function IsInRange(addr:Pointer;var ctx:t_jit_interrupt_ctx):Boolean; inline;
begin
 Result:=(QWORD(addr)>=ctx.start) and (QWORD(addr)<=ctx.__end);
end;

procedure add_orig(var ctx:t_jit_interrupt_ctx);
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 Move(ctx.code^,ji.AData^,ctx.dis.CodeIdx);

 ji.AInstructionSize:=ctx.dis.CodeIdx;

 ctx.builder._add(ji);
end;

procedure op_call(var ctx:t_jit_interrupt_ctx);
var
 id:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
begin

 if is_imm(ctx.din) then
 begin
  //imm offset
  ofs:=0;
  GetTargetOfs(ctx.din,ctx.code,1,ofs);
  dst:=ctx.ptr_next+ofs;

  if (dst<>Pointer(ctx.__end)) and IsInRange(dst,ctx) then
  begin
   //near imm
   id:=ctx.builder.call(nil_link);

   ctx.add_linkg(id,dst);
  end else
  begin
   //far imm
   ctx.builder.call_far(dst);
  end;

 end else
 if is_memory(ctx.din) then
 begin

  if is_rip(ctx.din) then
  begin
   //call [rip+$offset]
   ofs:=0;
   GetTargetOfs(ctx.din,ctx.code,1,ofs);
   dst:=ctx.ptr_next+ofs;

   dst:=PPointer(dst)^;

   ctx.builder.call_far(dst);
  end else
  begin
   //call [mem]
   add_orig(ctx);
  end;

 end else
 begin
  //call reg
  add_orig(ctx);
 end;

end;

procedure call_interrupt(var ctx:t_jit_interrupt_ctx;dst:Pointer);
begin
 with ctx.builder do
 begin
  movi64(r14,QWORD(dst));
  push(r14);
  jmp_far(@jit_interrupt_ast);
 end;
end;

procedure call_interrupt(var ctx:t_jit_interrupt_ctx;reg:TRegValue);
begin
 with ctx.builder do
 begin
  push(reg);
  jmp_far(@jit_interrupt_ast);
 end;
end;

procedure op_jcc(var ctx:t_jit_interrupt_ctx);
var
 id1:t_jit_i_link;
 id2:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
begin
 //imm offset
 ofs:=0;
 GetTargetOfs(ctx.din,ctx.code,1,ofs);
 dst:=ctx.ptr_next+ofs;

 case ctx.din.OpCode.Opcode of
  OPj__  :id1:=ctx.builder.jcc (ctx.din.OpCode.Suffix,nil_link);
  OPloop :id1:=ctx.builder.loop(ctx.din.OpCode.Suffix,nil_link,ctx.dis.AddressSize);
  OPjcxz :id1:=ctx.builder.jcxz(nil_link,ctx.dis.AddressSize);
  OPjecxz:id1:=ctx.builder.jcxz(nil_link,ctx.dis.AddressSize);
  OPjrcxz:id1:=ctx.builder.jcxz(nil_link,ctx.dis.AddressSize);
  else;
 end;

 if (dst<>Pointer(ctx.__end)) and IsInRange(dst,ctx) then
 begin
  //near imm
  ctx.add_linkg(id1,dst);
 end else
 begin
  //far imm
  id2:=ctx.builder.jmp(nil_link,os8);
   id1.target:=ctx.builder.get_curr_label.after;

   call_interrupt(ctx,dst);

  id2.target:=ctx.builder.get_curr_label.after;
 end;

end;

procedure op_jmp(var ctx:t_jit_interrupt_ctx);
var
 id:t_jit_i_link;
 ofs:Int64;
 dst:Pointer;
begin

 if is_imm(ctx.din) then
 begin
  //imm offset
  ofs:=0;
  GetTargetOfs(ctx.din,ctx.code,1,ofs);
  dst:=ctx.ptr_next+ofs;

  dst:=FixupEnd(dst,ctx.start,ctx.__end);

  if (dst=Pointer(ctx.__end)) then
  begin
   //endpoint
   call_interrupt(ctx,dst);
  end else
  if IsInRange(dst,ctx) then
  begin
   //near imm
   id:=ctx.builder.jmp(nil_link);

   ctx.add_linkg(id,dst);
  end else
  begin
   //far imm
   call_interrupt(ctx,dst);
  end;

 end else
 if is_memory(ctx.din) then
 begin

  if is_rip(ctx.din) then
  begin
   //jmp [rip+$offset]
   ofs:=0;
   GetTargetOfs(ctx.din,ctx.code,1,ofs);
   dst:=ctx.ptr_next+ofs;

   dst:=PPointer(dst)^;

   call_interrupt(ctx,dst);
  end else
  begin
   //jmp [mem]
   add_orig(ctx);
  end;

 end else
 begin
  //jmp reg
  call_interrupt(ctx,ctx.din.Operand[1].RegValue[0]);
 end;

end;

procedure op_lea(var ctx:t_jit_interrupt_ctx);
var
 new:TRegValue;
 ofs:Int64;
 dst:Pointer;
begin
 if is_rip(ctx.din) then
 begin
  new:=new_reg(ctx.din.Operand[1]);

  //lea reg, [rip+$offset]
  ofs:=0;
  GetTargetOfs(ctx.din,ctx.code,2,ofs);
  dst:=ctx.ptr_next+ofs;

  ctx.builder.movi64(new,QWORD(dst));
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure jit_analize(addr:Pointer;info:p_jit_addr_info;var ctx:t_jit_interrupt_ctx);
var
 ptr:Pointer;
 fin:Pointer;

 link_curr:t_jit_i_link;
begin
 ctx:=Default(t_jit_interrupt_ctx);

 ctx.Init(addr,info);

 if (ctx.caddr=ctx.start) then
 begin
  call_interrupt(ctx,addr);

  ctx.adest:=ctx.caddr;
  ctx.atype:=naCurr;
  Exit;
 end;

 //is bugged
 {
 if (info^.jflags.CAN_RESTART<>0) then
 begin
  call_interrupt(ctx,addr);

  ctx.adest:=ctx.caddr;
  ctx.atype:=naCurr;
  Exit;
 end;
 }

 if FixupEnd(addr,ctx.start,ctx.__end)=Pointer(ctx.__end) then
 begin
  call_interrupt(ctx,Pointer(ctx.__end));

  ctx.adest:=ctx.__end;
  ctx.atype:=naNext;
  Exit;
 end;

 ptr:=addr;
 fin:=Pointer(ctx.__end);

 while (ptr<fin) do
 begin

  ctx.code    :=ptr;
  ctx.ptr_curr:=ptr;
  ctx.dis.Disassemble(dm64,ptr,ctx.din);
  ctx.ptr_next:=ptr;

  link_curr:=ctx.builder.get_curr_label.after;

  case ctx.din.OpCode.Opcode of
   OPcli:ctx.debug:=True;  //start debug info
   OPsti:ctx.debug:=False; //end   debug info
  end;

  if ctx.debug then
  begin
   add_orig(ctx);
  end else
  case ctx.din.OpCode.Opcode of

   OPcall :op_call(ctx);
   OPj__  ,
   OPloop ,
   OPjcxz ,
   OPjecxz,
   OPjrcxz:op_jcc(ctx);
   OPjmp  :op_jmp(ctx);
   OPlea  :op_lea(ctx);

   //op_mov + rip ???

   else
    if is_rip(ctx.din) then
    begin
     ctx.adest:=QWORD(ctx.code);
     ctx.atype:=naRipUnknow;
     Exit;
    end else
    begin
     add_orig(ctx);
    end;
  end;

  ctx.add_label(ctx.ptr_curr,link_curr);

 end; //while

 call_interrupt(ctx,fin);

 ctx.adest:=ctx.__end;
 ctx.atype:=naNext;
end;

procedure jit_link(var ctx:t_jit_interrupt_ctx);
var
 node:p_linkg;
 dest:p_label;
begin
 node:=ctx.link_list;

 while (node<>nil) do
 begin

  dest:=ctx.get_label(node^.dst);

  Assert(dest<>nil,'dest=nil');

  node^.instruction.target:=dest^.link;

  //
  node:=node^.next;
 end;

end;

procedure jit_build(td:p_kthread;var ctx:t_jit_interrupt_ctx);
var
 mem_size:QWORD;
 mchunk:p_stub_chunk;
begin
 jit_link(ctx);

 mem_size:=ctx.builder.GetMemSize;

 if (mem_size>td^.td_jctx.lacuna.size) then
 begin
  if (td^.td_jctx.lacuna.chnk<>nil) then
  begin
   p_free(td^.td_jctx.lacuna.chnk);
   td^.td_jctx.lacuna.chnk:=nil;
  end;

  mchunk:=p_alloc(nil,mem_size,False);

  td^.td_jctx.lacuna.addr:=@mchunk^.body;
  td^.td_jctx.lacuna.chnk:=mchunk;
  td^.td_jctx.lacuna.size:=mem_size;
 end;

 td^.td_jctx.lacuna.orig:=ctx.orig;

 ctx.builder.SaveTo(td^.td_jctx.lacuna.addr,mem_size);
end;

function is_jit_track_blocks(td:p_kthread;Rip:QWORD):Boolean; inline;
begin
 Result:=(Rip>=QWORD(td^.td_jctx.track_blocks.addr)) and
         (Rip<=(QWORD(td^.td_jctx.track_blocks.addr + td^.td_jctx.track_blocks.size)));
end;

procedure JIT_AST_HANDLER(td:p_kthread;var Rip,Rsp:QWORD;var EFlags:DWORD;DrX:PQWORD);
label
 _start;
var
 f:Byte;
 info:t_jit_addr_info;
 ctx :t_jit_interrupt_ctx;

begin
_start:

 f:=GET_JIT_FUNC(Rip);

 if (f<>3) and (td^.td_teb^.iflag<>0) then
 begin
  LOG_WARNING('TODO:rare ipi case!');
  td^.td_teb^.jit_trp:=@jit_interrupt_ud2;
  Exit;
 end;

 case f of
  1:
   begin
    //jit handler
    LOG_TRACE('jit handler');
    td^.td_teb^.jit_trp:=@jit_interrupt_ud2;
    Exit;
   end;
  2:
   begin
    //jit nop handler
    LOG_TRACE('jit nop handler');

    //pop %rip
    Rip:=PQWORD(Rsp)[0];
    Rsp:=Rsp+8;
    goto _start;
   end;
  3:
   begin
    //ipi nop
    LOG_TRACE('ipi nop');
    td^.td_teb^.iflag:=0;
    Rip:=QWORD(td^.td_teb^.ipi_rip);
    goto _start;
   end;
  else
   begin
    if (Rip>=QWORD(td^.td_jctx.lacuna.addr)) and
       (Rip<=(QWORD(td^.td_jctx.lacuna.addr + td^.td_jctx.lacuna.size))) then
    begin
     //jit lacuna
     LOG_TRACE('jit lacuna');
     //skip
     Exit;
    end else
    if is_jit_track_blocks(td,Rip) then
    begin
     //jit track blobs
     LOG_TRACE('jit track blobs');

     Assert(False,'TODO');
     Exit;
    end else
    if exist_jit_host(Pointer(Rip),@info) then
    begin

     if (info.recompil = Rip) then
     begin
      LOG_TRACE('jit_direct');

      //push %rip
      Rsp:=Rsp-8;
      PQWORD(Rsp)[0]:=Rip;
      //
      Rip:=QWORD(@jit_interrupt_ast);
      //
      Exit;
     end;

     jit_analize(Pointer(Rip),@info,ctx);

     LOG_TRACE('jit_analize=',ctx.atype);

     case ctx.atype of
      naRipUnknow:
       begin
        ctx.builder.Free;
        LOG_WARNING('TODO:naRipUnknow');
        td^.td_teb^.jit_trp:=@jit_interrupt_ud2;
        Exit;
       end;
      else;
     end;

     jit_build(td,ctx);

     {
     LOG_TRACE('-----------------------------');
     LOG_TRACE(HexStr(td^.td_jctx.lacuna.addr));

     print_disassemble(td^.td_jctx.lacuna.addr,ctx.builder.GetInstructionsSize);

     LOG_TRACE(HexStr(td^.td_jctx.lacuna.addr+ctx.builder.GetInstructionsSize));
     LOG_TRACE('-----------------------------');
     }

     ctx.builder.Free;

     Rip:=QWORD(td^.td_jctx.lacuna.addr);
     //
     Exit;
    end else
    begin
     //internal? hle?
     LOG_TRACE('internal handler');
     td^.td_teb^.jit_trp:=@jit_interrupt_ud2;
     Exit;
    end;
   end;
 end;
end;

type
 p_track_jit_block=^t_track_jit_block;
 t_track_jit_block=packed record
  code_addr:QWORD;
  fault_ofs:DWORD;
  mema_size:DWORD;
  disp     :Int64;
  data:array[0..64-1-8-4-4-8] of Byte;
 end;

const
 m_chunk_count=(t_track_blob.size div sizeof(t_track_jit_block));

function hash_addr(addr:QWORD):Word; inline;
begin
 Result:=(Word(addr shr 4) xor Word(addr shr 14)) mod m_chunk_count;
end;

function index_addr(addr:QWORD):Word;// inline;
begin
 Result:=Word(addr) div sizeof(t_track_jit_block);
end;

procedure add_orig(var info:t_instruction_info;var builder:t_jit_builder); inline;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 Move(info.data[0],ji.AData^,info.code_size);

 ji.AInstructionSize:=info.code_size;

 builder._add(ji);
end;

procedure ON_JIT_TRACK(td:p_kthread;Context:PCONTEXT);
var
 i:Word;
 mblock:p_track_jit_block;
 reg:Pointer;
 addr:Int64;
begin
 i:=index_addr(Context^.Rip);

 mblock:=td^.td_jctx.track_blocks.addr;
 mblock:=@mblock[i];

 reg:=Pointer(Context)+mblock^.fault_ofs;
 reg:=PPointer(reg)^;

 //effective address is base+disp
 addr:=Int64(PtrUint(reg))+mblock^.disp;

 pmap_track_trigger(vmspace_pmap(p_proc.p_vmspace),
                    QWORD(addr),
                    QWORD(addr+mblock^.mema_size),
                    nil,
                    M_CPU_WRITE,
                    False);

 Context^.Rip:=Context^.Rip+2; //skip ud2
end;

function JIT_TRACK_HANDLER(td:p_kthread;var Rip,Rsp:QWORD;var info:t_instruction_info):Boolean;
label
 _run;
var
 base:Pointer;
 mblock:p_track_jit_block;

 builder:t_jit_builder;
begin

 if is_jit_track_blocks(td,Rip) then
 begin
  Exit(False);
 end;

 if (info.mema_size=0) then
 begin
  Exit(False);
 end;

 if (td^.td_jctx.track_blocks.addr=nil) then
 begin
  base:=Pointer(VM_MIN_JIT);
  if (md_mmap(base,t_track_blob.size,VM_RWX)<>0) then
  begin
   Exit(False);
  end;
  //
  td^.td_jctx.track_blocks.addr:=base;
 end;

 mblock:=td^.td_jctx.track_blocks.addr;
 mblock:=@mblock[hash_addr(Rip)];

 if (mblock^.code_addr=Rip) then
 begin
  _run:
  //push %rip
  Rsp:=Rsp-8;
  PQWORD(Rsp)[0]:=(Rip+info.code_size);
  //
  Rip:=QWORD(@mblock^.data);
  Exit(True);
 end;

 if (info.RegValues[1].AType<>regNone) or     //only one reg
    (info.RegValues[0].AType<>regGeneral) or  //only general reg
    (info.RegValues[0].AScale>1) then         //no scale
 begin
  Exit(False);
 end;

 builder:=Default(t_jit_builder);

 with builder do
 begin
  add_orig(info,builder);
  ud2;
  jmp([GS+teb_jit_trp]);
 end;

 Assert(builder.GetMemSize<=32,'builder.GetMemSize<=32');

 builder.SaveTo(@mblock^.data,32);
 builder.Free;

 mblock^.code_addr:=Rip;
 mblock^.mema_size:=info.mema_size;
 mblock^.disp     :=info.disp;

 case info.RegValues[0].AIndex of
   0:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.rax);//rax
   1:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.rcx);//rcx
   2:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.rdx);//rdx
   3:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.rbx);//rbx
   4:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.rsp);//rsp
   5:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.rbp);//rbp
   6:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.rsi);//rsi
   7:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.rdi);//rdi
   8:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.r8 );//r8
   9:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.r9 );//r9
  10:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.r10);//r10
  11:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.r11);//r11
  12:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.r12);//r12
  13:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.r13);//r13
  14:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.r14);//r14
  15:mblock^.fault_ofs:=DWORD(@PCONTEXT(nil)^.r15);//r15
  else
   Assert(False);
 end;

 goto _run;
end;



end.





