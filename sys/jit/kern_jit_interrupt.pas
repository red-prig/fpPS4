unit kern_jit_interrupt;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_stub,
 kern_thr,
 kern_jit_dynamic,
 kern_jit_ctx,
 kern_jit_asm,
 x86_fpdbgdisas,
 x86_jit;

function  GET_JIT_FUNC(rip:qword):Byte; external;
procedure jit_interrupt_ud2; external;
procedure jit_interrupt_ast; external;

procedure JIT_AST_HANDLER(td:p_kthread;var Rip,Rsp:QWORD;var EFlags:DWORD;DrX:PQWORD);

function  is_jit_track_blocks(td:p_kthread;Rip:QWORD):Boolean; inline;

function  JIT_TRACK_HANDLER(td:p_kthread;var Rip:QWORD;var info:t_instruction_info):Boolean;

function  jit_track_lookup(td:p_kthread;Rip:QWORD;var info:t_instruction_info):Boolean;
procedure jit_track_reset(td:p_kthread;base,bend:QWORD);

type
 p_track_jit_block=^t_track_jit_block;
 t_track_jit_block=packed record
  code_addr:QWORD;
  code_next:QWORD;
  reg_index:Byte;
  disp_idx :Byte;
  disp_cnt :Byte;
  code_size:Byte;
  mema_size:Word;
  data     :array[0..31] of Byte;
 end;

const
 m_chunk_count=(t_track_blob.size div sizeof(t_track_jit_block));

implementation

uses
 kern_proc,
 vmparam,
 vm_map,
 vm_pmap,
 vm_pmap_prot,
 vm_tracking_map,
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
 fin,str:Pointer;
begin
 urcu_qs(curkthread,True);

 if exist_jit_host(addr,@info) and info.mapped then
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

  fin:=Pointer(info.recompil + info.jflags.recompil);
  str:=FixupEnd(addr,QWORD(info.recompil),QWORD(fin));

  if (str=fin) then
  begin

   with curkthread^.td_frame do
   begin
    tf_rip:=(info.original + info.jflags.original); //need by AST

    //recheck
    exist_jit_host(str,@info);

    Assert((info.original = tf_rip),'rev_dispatcher:1');

    //LOG_TRACE('rev_dispatcher:2:',HexStr(str),'->',HexStr(info.original,16));
   end;

   Exit;
  end else
  begin
   LOG_CRITICAL(StdErr,'rev_dispatcher:3:',HexStr(addr),' ',HexStr(str),' ',HexStr(fin),' ',HexStr(info.original,16),' ',HexStr(info.recompil,16));
   Assert(False,'rev_dispatcher:3');
  end;

 end else
 begin
  LOG_CRITICAL(StdErr,'rev_dispatcher:4:',HexStr(addr),' internal:',info.internal,' mapped:',info.mapped);
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
  LOG_WARNING('TODO:rare ipi case!:',HexStr(Rip,16));
  td^.td_teb^.jit_trp:=@jit_interrupt_ud2;
  Exit;
 end;

 case f of
  1:
   begin
    //jit handler
    LOG_TRACE('jit handler:',HexStr(Rip,16));
    td^.td_teb^.jit_trp:=@jit_interrupt_ud2;
    Exit;
   end;
  2:
   begin
    //jit nop handler
    LOG_TRACE('jit nop handler:',HexStr(Rip,16),'->',HexStr(PQWORD(Rsp)[0],16));

    //pop %rip
    Rip:=PQWORD(Rsp)[0];
    Rsp:=Rsp+8;
    goto _start;
   end;
  3:
   begin
    //ipi nop
    LOG_TRACE('ipi nop:',HexStr(Rip,16));
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
     LOG_TRACE('jit lacuna:',HexStr(Rip,16));
     //skip
     Exit;
    end else
    if exist_jit_host(Pointer(Rip),@info) then
    begin

     if info.internal then
     begin
      LOG_TRACE('jit internal:',HexStr(Rip,16));
      td^.td_teb^.jit_trp:=@jit_interrupt_ud2;
      Exit;
     end;

     if (info.recompil = Rip) then
     begin
      LOG_TRACE('jit_direct:',HexStr(Rip,16));

      //push %rip
      Rsp:=Rsp-8;
      PQWORD(Rsp)[0]:=Rip;
      //
      Rip:=QWORD(@jit_interrupt_ast);
      //
      Exit;
     end;

     jit_analize(Pointer(Rip),@info,ctx);

     LOG_TRACE('jit_analize=',ctx.atype,':',HexStr(Rip,16));

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
    if is_jit_track_blocks(td,Rip) then
    begin
     //jit track blobs
     LOG_TRACE('jit track blobs:',HexStr(Rip,16));
     td^.td_teb^.jit_trp:=@jit_interrupt_ud2;
     Exit;
    end else
    begin
     //internal? hle?
     LOG_TRACE('internal handler:',HexStr(Rip,16));
     td^.td_teb^.jit_trp:=@jit_interrupt_ud2;
     Exit;
    end;
   end;
 end;
end;

function is_jit_track_blocks(td:p_kthread;Rip:QWORD):Boolean; inline;
begin
 with td^.td_jctx.track_blocks do
 begin
  Result:=(addr<>nil) and
          (Rip>=QWORD(addr)) and
          (Rip<QWORD(addr)+QWORD(size));
 end;
end;

function hash_addr(addr:QWORD):Word; inline;
begin
 Result:=(Word(addr shr 4) xor Word(addr shr 14)) mod m_chunk_count;
end;

function index_addr(addr:QWORD):Word; inline;
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

function jit_track_lookup(td:p_kthread;Rip:QWORD;var info:t_instruction_info):Boolean;
var
 mblock:p_track_jit_block;
begin
 Result:=False;

 if (td^.td_jctx.track_blocks.addr=nil) then Exit;

 mblock:=td^.td_jctx.track_blocks.addr;
 mblock:=@mblock[hash_addr(Rip)];

 if (Rip=0) or (mblock^.code_addr<>Rip) then Exit;

 info:=Default(t_instruction_info);

 info.code_size:=mblock^.code_size;
 info.disp_idx :=mblock^.disp_idx ;
 info.disp_cnt :=mblock^.disp_cnt ;
 info.mema_size:=mblock^.mema_size;

 Result:=True;
end;

procedure jit_track_reset(td:p_kthread;base,bend:QWORD); public;
var
 i:Integer;
 mblock:p_track_jit_block;
begin

 with td^.td_jctx.track_blocks do
 if (addr<>nil) then
 For i:=0 to m_chunk_count-1 do
 begin
  mblock:=@p_track_jit_block(addr)[i];

  if ((QWORD(mblock^.code_addr)>=base) and (QWORD(mblock^.code_addr)<bend)) then
  begin
   mblock^.code_addr:=0;
  end;
 end;

end;

function jit_track_trigger(rip,r13,r14,r15:QWORD):QWORD; public;
var
 td:p_kthread;
 mblock:p_track_jit_block;
 addr:QWORD;
 code_next:QWORD;
 i:Word;
begin
 td:=curkthread;

 i:=index_addr(rip);

 mblock:=td^.td_jctx.track_blocks.addr;
 Assert(mblock<>nil);

 mblock:=@mblock[i];

 code_next:=mblock^.code_next;

 case mblock^.reg_index of
   0:addr:=td^.td_frame.tf_rax; //rax
   1:addr:=td^.td_frame.tf_rcx; //rcx
   2:addr:=td^.td_frame.tf_rdx; //rdx
   3:addr:=td^.td_frame.tf_rbx; //rbx
   4:addr:=td^.td_frame.tf_rsp; //rsp
   5:addr:=td^.td_frame.tf_rbp; //rbp
   6:addr:=td^.td_frame.tf_rsi; //rsi
   7:addr:=td^.td_frame.tf_rdi; //rdi
   8:addr:=td^.td_frame.tf_r8 ; //r8
   9:addr:=td^.td_frame.tf_r9 ; //r9
  10:addr:=td^.td_frame.tf_r10; //r10
  11:addr:=td^.td_frame.tf_r11; //r11
  12:addr:=td^.td_frame.tf_r12; //r12
  13:addr:=r13; //r13
  14:addr:=r14; //r14
  15:addr:=r15; //r15
  else
   Assert(False);
 end;

 case mblock^.disp_cnt of
   1: addr:=addr+PShortint(@mblock^.data[mblock^.disp_idx])^;
   2: addr:=addr+PSmallint(@mblock^.data[mblock^.disp_idx])^;
   4: addr:=addr+PInteger (@mblock^.data[mblock^.disp_idx])^;
   8: addr:=addr+PInt64   (@mblock^.data[mblock^.disp_idx])^;
   else;
 end;

 pmap_track_trigger(vmspace_pmap(p_proc.p_vmspace),
                    QWORD(addr),
                    QWORD(addr+mblock^.mema_size),
                    nil,
                    M_CPU_WRITE,
                    False);

 Result:=code_next;
end;

function JIT_TRACK_HANDLER(td:p_kthread;var Rip:QWORD;var info:t_instruction_info):Boolean;
label
 _run;
var
 base:Pointer;
 mblock:p_track_jit_block;
 is_jit:Boolean;

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

 if (Rip<>0) and (mblock^.code_addr=Rip) then
 begin
  _run:
   Rip:=QWORD(@mblock^.data);
   Exit(True);
 end;

 is_jit:=(QWORD(Rip)>=VM_MIN_JIT) and (QWORD(Rip)<VM_MAX_JIT);

 if (info.RegValues[1].AType<>regNone)    or  //only one reg
    (info.RegValues[0].AType<>regGeneral) or  //only general reg
    (info.RegValues[0].ASize<>os64)       or  //only 64bit
    (info.RegValues[0].AScale>1) then         //no scale
 begin
  //not support
  Exit(False);
 end;

 if is_jit and (info.RegValues[0].AIndex=4) then
 begin
  Exit(False);
 end;

 builder:=Default(t_jit_builder);

 with builder do
 begin
  add_orig(info,builder);

  if is_jit then
  begin
   call_far(@jit_track_guest);
  end else
  begin
   ud2;
  end;

 end;

 if (builder.GetMemSize>sizeof(mblock^.data)) then
 begin
  builder.Free;
  Exit(False);
 end;

 builder.SaveTo(@mblock^.data,sizeof(mblock^.data));
 builder.Free;

 mblock^.code_addr:=Rip;
 mblock^.code_next:=Rip+info.code_size;
 mblock^.reg_index:=info.RegValues[0].AIndex;
 mblock^.disp_idx :=info.disp_idx;
 mblock^.disp_cnt :=info.disp_cnt;
 mblock^.code_size:=info.code_size;
 mblock^.mema_size:=info.mema_size;
 mblock^.code_addr:=Rip;

 goto _run;
end;

end.





