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
 kern_stub;

procedure add_entry_point(dst:Pointer);

procedure pick();

implementation

uses
 machdep;

type
 p_jit_frame=^jit_frame;
 jit_frame=packed record
  tf_rax:QWORD;
  tf_rsp:QWORD;
  tf_rbp:QWORD;
  tf_r14:QWORD;
  tf_r15:QWORD;
 end;

function GetFrameOffset(const RegValue:TRegValue):Integer; inline;
begin
 Result:=-1;

 case RegValue.AType of
  regGeneral:
  begin
   case RegValue.ASize of
    os8,
    os16,
    os32,
    os64:
    begin
     case RegValue.AIndex of
       0:Result:=Integer(@p_jit_frame(nil)^.tf_rax);
       4:Result:=Integer(@p_jit_frame(nil)^.tf_rsp);
       5:Result:=Integer(@p_jit_frame(nil)^.tf_rbp);
      14:Result:=Integer(@p_jit_frame(nil)^.tf_r14);
      15:Result:=Integer(@p_jit_frame(nil)^.tf_r15);
      else;
     end;
    end;
   else;
   end;
  end;
  regGeneralH:
  begin
   case RegValue.ASize of
    os8:
    begin
     case RegValue.AIndex of
      0:Result:=Integer(@p_jit_frame(nil)^.tf_rax)+1;
      else;
     end;
    end;
   else;
   end;
  end;
 else;
 end;

 Assert(Result<>-1,'GetFrameOffset');
end;

function GetFrameOffset(const r:TOperand):Integer; inline;
begin
 Result:=GetFrameOffset(r.RegValue[0])
end;

function GetTargetOfs(var din:TInstruction;Code:PByte;id:Byte;var ofs:Int64):Boolean;
var
 i:Integer;
begin
 Result:=True;
 i:=din.Operand[id].CodeIndex;
 case din.Operand[id].ByteCount of
   1: ofs:=PShortint(@Code[i])^;
   2: ofs:=PSmallint(@Code[i])^;
   4: ofs:=PInteger (@Code[i])^;
   8: ofs:=PInt64   (@Code[i])^;
   else
      Result:=False;
 end;
end;

{
change: rsp,rbp,rip
rax?

eflahs? temp change?

change: push/pop

thread: r15

}

function is_preserved(const r:TRegValue):Boolean;
begin
 Result:=False;

 case r.AType of
  regRip:Result:=True;
  regGeneral:
    begin
     case r.AIndex of
       0, //rax
       4, //rsp
       5, //rbp
      14, //r14
      15: //r15
         Result:=True;
      else;
     end;
    end;
  else;
 end;

end;

function is_preserved(const r:TRegValues):Boolean; inline;
begin
 Result:=is_preserved(r[0]) or is_preserved(r[1]);
end;

function is_preserved(const r:TOperand):Boolean; inline;
begin
 Result:=is_preserved(r.RegValue) and
         (not (ofMemory in r.Flags));
end;

function is_preserved(const r:TInstruction):Boolean;
var
 i:Integer;
begin
 Result:=False;
 if (r.OperCnt<>0) then
 For i:=1 to r.OperCnt do
 begin
  Result:=is_preserved(r.Operand[i]);
  if Result then Exit;
 end;
end;

function is_rip(const r:TRegValue):Boolean; inline;
begin
 Result:=False;
 case r.AType of
  regRip:Result:=True;
  else;
 end;
end;

function is_memory(const r:TOperand):Boolean; inline;
begin
 Result:=ofMemory in r.Flags;
end;

function is_memory(const r:TInstruction):Boolean;
var
 i:Integer;
begin
 Result:=False;
 if (r.OperCnt<>0) then
 For i:=1 to r.OperCnt do
 begin
  Result:=is_memory(r.Operand[i]);
  if Result then Exit;
 end;
end;

function is_segment(const r:TOperand):Boolean; inline;
begin
 Result:=(r.RegValue[0].AType=regSegment) and
         (not (ofMemory in r.Flags));
end;

type
 p_jit_context2=^t_jit_context2;
 t_jit_context2=record
  Code    :Pointer;
  ptr_curr:Pointer;
  ptr_next:Pointer;

  dis:TX86Disassembler;
  din:TInstruction;

  builder:t_jit_builder;

 end;

 t_jit_cb=procedure(var ctx:t_jit_context2);

var
 jit_cbs:array[TOpcodePrefix,TOpCode,TOpCodeSuffix] of t_jit_cb;

procedure add_orig(var ctx:t_jit_context2);
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 Move(ctx.code^,ji.AData,ctx.dis.CodeIdx);

 ji.ASize:=ctx.dis.CodeIdx;

 ctx.builder._add(ji);
end;

const
 r_thrd:TRegValue=(AType:regGeneral;ASize:os64;AIndex:15); //r15
 r_tmp0:TRegValue=(AType:regGeneral;ASize:os64;AIndex: 0); //rax
 r_tmp1:TRegValue=(AType:regGeneral;ASize:os64;AIndex:14); //r14

function new_reg(const r:TRegValue):TRegValue; inline;
begin
 Result:=r;
end;

function new_reg(const Operand:TOperand):TRegValue; inline;
begin
 Result:=Operand.RegValue[0];
end;

function new_reg_size(const r:TRegValue;ASize:TOperandSize):TRegValue; inline;
begin
 Result:=r;
 Result.ASize:=ASize;
end;

function new_reg_size(const r:TRegValue;const RegValue:TRegValues):TRegValue; inline;
begin
 Result:=new_reg_size(r,RegValue[0].ASize);
end;

function new_reg_size(const r:TRegValue;const Operand:TOperand):TRegValue; inline;
begin
 Result:=new_reg_size(r,Operand.RegValue[0].ASize);
end;

function fix_size(const r:TRegValue):TRegValue; inline;
begin
 Result:=r;
 if (Result.ASize=os32) then
 begin
  Result.ASize:=os64;
 end;
end;

function flags(const i:TInstruction):t_jit_reg;
begin
 Result:=Default(t_jit_reg);

 if (ifPrefixLock in i.Flags) then
 begin
  Result:=Result+t_jit_builder.LOCK;
 end;

 if (ifPrefixRep in i.Flags) then
 begin
  Assert(false);
 end;

 if (ifPrefixRepE in i.Flags) then
 begin
  Assert(false);
 end;

 if (ifPrefixRepNe in i.Flags) then
 begin
  Assert(false);
 end;

 case i.SegmentReg of
  4:Result:=Result+t_jit_builder.FS;
  5:Result:=Result+t_jit_builder.GS;
  else;
 end;
end;

function flags(const ctx:t_jit_context2):t_jit_reg; inline;
begin
 Result:=flags(ctx.din);
end;

procedure build_lea(var ctx:t_jit_context2;id:Byte;reg:TRegValue);
var
 RegValue:TRegValues;
 adr,new1,new2:TRegValue;
 ofs:Int64;
 i:Integer;
 AScale:Byte;
begin
 RegValue:=ctx.din.Operand[id].RegValue;

 adr:=new_reg_size(reg,RegValue);

 with ctx.builder do
 begin
  if (RegValue[0].ASize<>os64) then
  begin
   xorq(reg,reg);
  end;

  if is_rip(RegValue[0]) then
  begin
   ofs:=0;
   GetTargetOfs(ctx.din,ctx.code,id,ofs);
   ofs:=Int64(ctx.ptr_next)+ofs;

   if (classif_offset_se64(ofs)=os64) then
   begin
    movi64(adr,ofs);
   end else
   begin
    movi(adr,ofs);
   end;
  end else
  if is_preserved(RegValue) then
  begin
   AScale:=RegValue[0].AScale;

   ofs:=0;
   GetTargetOfs(ctx.din,ctx.code,id,ofs);

   if is_preserved(RegValue[0]) then
   begin
    i:=GetFrameOffset(RegValue[0]);
    movq(adr,[r_thrd+i]);

    if (AScale>1) or (ofs<>0) then
    begin
     leaq(adr,[adr*AScale+ofs]);
    end;
   end else
   begin
    new1:=new_reg(RegValue[0]);
    //
    //AScale in new
    leaq(adr,[new1+ofs]);
   end;

   if (RegValue[1].AType<>regNone) then
   begin
    if is_preserved(RegValue[1]) then
    begin
     i:=GetFrameOffset(RegValue[1]);
     addq(adr,[r_thrd+i]);
    end else
    begin
     new1:=new_reg(RegValue[1]);
     addq(adr,new1);
    end;
   end;
  end else
  begin
   ofs:=0;
   GetTargetOfs(ctx.din,ctx.code,id,ofs);

   new1:=new_reg(RegValue[0]);

   if (RegValue[1].AType<>regNone) then
   begin
    new2:=new_reg(RegValue[1]);
    //
    //AScale in new
    leaq(adr,[new1+new2+ofs]);
   end else
   begin
    //AScale in new
    leaq(adr,[new1+ofs]);
   end;

  end;

 end;

end;

type
 t_memop_type1=(mo_reg,
                mo_mem,
                mo_ctx
               );

 t_memop_type2=(mo_reg_reg,
                mo_mem_reg,
                mo_reg_mem,
                mo_reg_imm,
                mo_mem_imm,

                mo_ctx_reg,
                mo_reg_ctx,
                mo_ctx_ctx,

                mo_mem_ctx,

                mo_ctx_mem,
                mo_ctx_imm
               );

 t_memop_shift=(mo_mem_imm8,
                mo_reg_imm8,

                mo_mem_cl,
                mo_reg_cl,

                mo_mem_one,
                mo_reg_one,

                mo_ctx_imm8,
                mo_ctx_cl,
                mo_ctx_one
               );

function get_lea_id(memop:t_memop_type2):Byte;
begin
 case memop of
  mo_mem_reg:Result:=1;
  mo_reg_mem:Result:=2;
  mo_mem_imm:Result:=1;
  mo_mem_ctx:Result:=1;
  mo_ctx_mem:Result:=2;
  else
   Assert(False);
 end;
end;

function classif_memop1(var din:TInstruction):t_memop_type1;
begin
 if (ofMemory in din.Operand[1].Flags) then
 begin
  Result:=mo_mem;
 end else
 if is_preserved(din.Operand[1]) then
 begin
  Result:=mo_ctx;
 end else
 begin
  Result:=mo_reg;
 end;
end;

function classif_memop2(var din:TInstruction):t_memop_type2;
begin
 if (ofMemory in din.Operand[1].Flags) then
 begin
  if (din.Operand[2].ByteCount<>0) then
  begin
   Result:=mo_mem_imm;
  end else
  if is_preserved(din.Operand[2]) then
  begin
   Result:=mo_mem_ctx;
  end else
  begin
   Result:=mo_mem_reg;
  end;
 end else
 if (ofMemory in din.Operand[2].Flags) then
 begin
  if is_preserved(din.Operand[1]) then
  begin
   Result:=mo_ctx_mem;
  end else
  begin
   Result:=mo_reg_mem;
  end;
 end else
 if (din.Operand[2].ByteCount<>0) then
 begin
  if is_preserved(din.Operand[1]) then
  begin
   Result:=mo_ctx_imm;
  end else
  begin
   Result:=mo_reg_imm;
  end;
 end else
 if is_preserved(din.Operand[1]) and
    is_preserved(din.Operand[2]) then
 begin
  Result:=mo_ctx_ctx;
 end else
 if is_preserved(din.Operand[1]) then
 begin
  Result:=mo_ctx_reg;
 end else
 if is_preserved(din.Operand[2]) then
 begin
  Result:=mo_reg_ctx;
 end else
 begin
  Result:=mo_reg_reg;
 end;
end;

//

function is_cl(const r:TRegValue):Boolean; inline;
begin
 Result:=False;
 if (r.ASize=os8) then
 case r.AType of
  regGeneral:
   case r.AIndex of
     1:Result:=True;
    else;
   end;
  else;
 end;
end;

function is_one(const r:TRegValue):Boolean; inline;
begin
 Result:=False;
 case r.AType of
  regOne:Result:=True;
  else;
 end;
end;

function classif_shift2(var din:TInstruction):t_memop_shift;
begin
 if (ofMemory in din.Operand[1].Flags) then
 begin
  if (din.Operand[2].ByteCount<>0) then
  begin
   Assert(din.Operand[2].Size=os8);
   Result:=mo_mem_imm8;
  end else
  if is_cl(din.Operand[2].RegValue[0]) then
  begin
   Result:=mo_mem_cl;
  end else
  if is_one(din.Operand[2].RegValue[0]) then
  begin
   Result:=mo_mem_one;
  end else
  begin
   Assert(false);
  end;
 end else
 if is_preserved(din.Operand[1]) then
 begin
  if (din.Operand[2].ByteCount<>0) then
  begin
   Assert(din.Operand[2].Size=os8);
   Result:=mo_ctx_imm8;
  end else
  if is_cl(din.Operand[2].RegValue[0]) then
  begin
   Result:=mo_ctx_cl;
  end else
  if is_one(din.Operand[2].RegValue[0]) then
  begin
   Result:=mo_ctx_one;
  end else
  begin
   Assert(false);
  end;
 end else
 if (din.Operand[2].ByteCount<>0) then
 begin
  Assert(din.Operand[2].Size=os8);
  Result:=mo_reg_imm8;
 end else
 if is_cl(din.Operand[2].RegValue[0]) then
 begin
  Result:=mo_reg_cl;
 end else
 if is_one(din.Operand[2].RegValue[0]) then
 begin
  Result:=mo_reg_one;
 end else
 begin
  Assert(false);
 end;
end;

type
 t_op_desc=packed record
  mem_reg:t_op_type; //reg_reg
  reg_mem:t_op_type; //reg_reg
  reg_imm:t_op_type; //mem_imm
  reg_im8:t_op_type; //mem_im8
  hint:Set of (his_mov,his_xor,his_cmp,his_xchg,his_align);
 end;

 t_op_shift=packed record
  reg_im8:t_op_type; //mem_im8
  mem__cl:t_op_type; //reg__cl
  mem_one:t_op_type; //reg_one
 end;

//in/out:rax uses:r14
procedure uplift_jit; assembler; nostackframe;
label
 _exit;
asm
 pushfq
 push %r14
 //
 //low addr (r14)
 mov %rax,%r14
 and PAGE_MASK,%r14
 //high addr (rax)
 shr PAGE_SHIFT   ,%rax
 and PAGE_MAP_MASK,%rax
 //uplift (rax)
 mov PAGE_MAP,%rax
 mov (%rax,%rax,4),%edi
 //filter (rax)
 and PAGE_OFS_MASK,%rax
 jz _exit
 //combine (rax|r14)
 shl PAGE_SHIFT,%rax
 or  %r14,%rax
 _exit:
 //
 pop %r14
 popfq
end;

//in:rax(addr),r14b:(mem_size) out:ZF
procedure page_test; assembler; nostackframe;
label
 _exit;
asm
 push %rdi
 push %rsi
 //
 mov %rax,%rdi
 movzbq %r14b,%rsi
 sub $1,%rsi
 //addr2:=addr+mem_high (rsi)
 add %rdi,%rsi
 //high addr (rdi,rsi)
 shr PAGE_SHIFT   ,%rdi
 shr PAGE_SHIFT   ,%rsi
 and PAGE_MAP_MASK,%rdi
 and PAGE_MAP_MASK,%rsi
 //
 cmp %rdi,%rsi
 je _exit
 //uplift (rdi,rsi)
 lea (,%rdi,4),%rdi
 lea (,%rsi,4),%rsi
 //
 add PAGE_MAP,%rdi
 add PAGE_MAP,%rsi
 //
 mov (%rdi),%edi
 mov (%rsi),%esi
 //filter (rdi,rsi)
 and PAGE_OFS_MASK,%rdi
 and PAGE_OFS_MASK,%rsi
 //
 cmp %rdi,%rsi
 _exit:
 //
 pop %rsi
 pop %rdi
end;

//in:rax(addr),r14b:(size)
procedure copyout_mov; assembler;
label
 _simple;
var
 data:array[0..31] of Byte;
asm
 pushfq
 //
 call page_test
 je _simple

  popfq //restore flags before call

  push %rdi
  push %rsi
  push %rdx
  push %rcx
  push %r8
  push %r9
  push %r10
  push %r11

  push %rax

  mov  data,%rax

  mov  8(%rbp) ,%rdi //ret addr
  lea (,%rdi,2),%rdi //jmp near

  call %rdi         //reg->data

  pop     %rsi      //vaddr
  mov     data,%rdi //data
  movzbq %r14b,%rdx //size

  pushfq

  call copyout

  popfq

  pop  %r11
  pop  %r10
  pop  %r9
  pop  %r8
  pop  %rcx
  pop  %rdx
  pop  %rsi
  pop  %rdi

  ret
 _simple:

  call uplift_jit

  mov  8(%rbp) ,%r14 //ret addr
  lea (,%r14,2),%r14 //jmp near

  popfq //restore flags before call

  call %r14         //reg->data

end;

//in:rax(addr),r14b:(size) out:rax
procedure copyin_mov; assembler;
label
 _simple,
 _exit;
var
 data:array[0..31] of Byte;
asm
 pushfq
 //
 call page_test
 je _simple

  push %rdi
  push %rsi
  push %rdx
  push %rcx
  push %r8
  push %r9
  push %r10
  push %r11

  mov     %rax,%rdi //vaddr
  mov     data,%rsi //data
  movzbq %r14b,%rdx //size

  call copyin

  pop  %r11
  pop  %r10
  pop  %r9
  pop  %r8
  pop  %rcx
  pop  %rdx
  pop  %rsi
  pop  %rdi

  mov data,%rax //vaddr:=data

  jmp _exit
 _simple:

  call uplift_jit

 _exit:
 //
 popfq
end;

function cmp_reg(const r1,r2:TRegValue):Boolean; inline;
begin
 Result:=(r1.AType =r2.AType) and
         (r1.ASize =r2.ASize) and
         (r1.AIndex=r2.AIndex);
end;

const
 OPERAND_BYTES:array[TOperandSize] of Word=(0,1,2,4,8,6,10,16,32,64,512);

procedure op_emit1(var ctx:t_jit_context2;const desc:t_op_type;used_rax:Boolean);
var
 i:Integer;
 memop:t_memop_type1;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 procedure mem_out;
 begin
  with ctx.builder do
  begin
   //input:rax

   _M(desc,mem_size,[flags(ctx)+r_tmp0]);
  end;
 end;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:rax

   i:=GetFrameOffset(rax);
   movq(rax,[r_thrd+i]);

   _M(desc,mem_size,[flags(ctx)+r_tmp1]);

   movq([r_thrd+i],rax);
  end;
 end;

begin
 memop:=classif_memop1(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem:
    begin

     if used_rax then
     begin
      //RAX:=RAX X [mem]
      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;

      if (mem_size=os8) then
      begin
       call(@uplift_jit); //in/out:rax uses:r14

       mem_in;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call(@copyin_mov); //in:rax(addr),r14:(size) out:rax

       mem_in;
      end;

     end else
     begin
      //[mem]:=DATA
      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;

      if (mem_size=os8) then
      begin

       call(@uplift_jit); //in/out:rax uses:r14

       mem_out;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call(@copyout_mov); //in:rax(addr),r14:(size)

       link_next:=jmp8(0);

       mem_out;

       reta;

       link_next._label:=_label;
      end;
     end;

    end;

   mo_ctx:
    begin
     mem_size:=ctx.din.Operand[1].Size;
     i:=GetFrameOffset(ctx.din.Operand[1]);

     if (mem_size=os32) then
     begin
      mem_size:=os64; //fix size
     end;

     _M(desc,mem_size,[r_thrd+i]);
    end;

   else
    Assert(false);
  end;

end;

procedure op_emit2(var ctx:t_jit_context2;const desc:t_op_desc);
var
 i:Integer;
 memop:t_memop_type2;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 imm:Int64;
 imm_size:TOperandSize;

 new1,new2:TRegValue;

 procedure mem_out;
 begin
  with ctx.builder do
   case memop of
    mo_mem_reg:
      begin
       //input:rax

       new1:=new_reg(ctx.din.Operand[2]);
       _RM(desc.mem_reg,new1,[flags(ctx)+r_tmp0]);
      end;
    mo_mem_imm:
      begin
       //input:rax

       imm:=0;
       GetTargetOfs(ctx.din,ctx.code,2,imm);

       imm_size:=ctx.din.Operand[2].Size;

       Assert(imm_size<>os64);

       if (imm_size=os8) and
          (mem_size<>os8) and
          (not (not_impl in desc.reg_im8.opt)) then
       begin
        _MI8(desc.reg_im8,mem_size,[flags(ctx)+r_tmp0],imm);
       end else
       begin
        _MI(desc.reg_imm,mem_size,[flags(ctx)+r_tmp0],imm);
       end;

      end;
    mo_mem_ctx:
      begin
       //input:rax

       new1:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

       i:=GetFrameOffset(ctx.din.Operand[2]);
       movq(new1,[r_thrd+i]);

       _RM(desc.mem_reg,new1,[flags(ctx)+r_tmp0]);

       if (his_xchg in desc.hint) then
       begin
        movq([r_thrd+i],new1);
       end;

      end;
    else;
   end;
 end;

 procedure mem_in;
 begin
  with ctx.builder do
   case memop of
    mo_reg_mem:
      begin
       //input:rax

       new1:=new_reg(ctx.din.Operand[1]);

       imm:=0;
       if GetTargetOfs(ctx.din,ctx.code,3,imm) then
       begin
        imm_size:=ctx.din.Operand[3].Size;
        mem_size:=ctx.din.Operand[1].Size;

        if (imm_size=os8) and
           (mem_size<>os8) and
           (not (not_impl in desc.reg_im8.opt)) then
        begin
         _RMI8(desc.reg_im8,new1,[flags(ctx)+r_tmp0],imm);
        end else
        begin
         _RMI(desc.reg_imm,new1,[flags(ctx)+r_tmp0],imm);
        end;

       end else
       begin
        _RM(desc.reg_mem,new1,[flags(ctx)+r_tmp0]);
       end;

      end;
    mo_ctx_mem:
      begin
       //input:rax

       new1:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

       if (not (his_mov in desc.hint)) or
          (new1.ASize in [os8,os16]) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq(fix_size(new1),[r_thrd+i]);
       end;

       imm:=0;
       if GetTargetOfs(ctx.din,ctx.code,3,imm) then
       begin
        imm_size:=ctx.din.Operand[3].Size;
        mem_size:=ctx.din.Operand[1].Size;

        if (imm_size=os8) and
           (mem_size<>os8) and
           (not (not_impl in desc.reg_im8.opt)) then
        begin
         _RMI8(desc.reg_im8,new1,[flags(ctx)+r_tmp0],imm);
        end else
        begin
         _RMI(desc.reg_imm,new1,[flags(ctx)+r_tmp0],imm);
        end;

       end else
       begin
        _RM(desc.reg_mem,new1,[flags(ctx)+r_tmp0]);
       end;

       if not (his_cmp in desc.hint) then
       begin
        movq([r_thrd+i],fix_size(new1));
       end;

      end;
    else;
   end;
 end;

begin
 memop:=classif_memop2(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem_reg,
   mo_reg_mem,
   mo_mem_imm,
   mo_mem_ctx,
   mo_ctx_mem:
     begin
      build_lea(ctx,get_lea_id(memop),r_tmp0);
      mem_size:=ctx.din.Operand[get_lea_id(memop)].Size;
     end;
   else;
  end;

 with ctx.builder do
  case memop of
   mo_mem_reg,
   mo_mem_imm,
   mo_mem_ctx:
     begin
      if (mem_size=os8) then
      begin
       call(@uplift_jit); //in/out:rax uses:r14

       mem_out;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call(@copyout_mov); //in:rax(addr),r14:(size)

       link_next:=jmp8(0);

       mem_out;

       reta;

       link_next._label:=_label;
      end;
     end;

   mo_reg_mem,
   mo_ctx_mem:
     begin
      if (mem_size=os8) then
      begin
       call(@uplift_jit); //in/out:rax uses:r14

       mem_in;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call(@copyin_mov); //in:rax(addr),r14:(size) out:rax

       mem_in;
      end;
     end;

   mo_ctx_reg:
     begin
      new1:=new_reg(ctx.din.Operand[2]);

      imm:=0;
      if GetTargetOfs(ctx.din,ctx.code,3,imm) then
      begin
       new2:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       imm_size:=ctx.din.Operand[3].Size;
       mem_size:=ctx.din.Operand[1].Size;

       if (imm_size=os8) and
          (mem_size<>os8) and
          (not (not_impl in desc.reg_im8.opt)) then
       begin
        _RRI8(desc.reg_im8,new1,new2,imm,mem_size); //swapped
       end else
       begin
        _RRI(desc.reg_imm,new1,new2,imm,mem_size); //swapped
       end;

       i:=GetFrameOffset(ctx.din.Operand[1]);
       movq([r_thrd+i],fix_size(new2));

      end else
      if (new1.ASize=os32) or
         (not_impl in desc.mem_reg.opt) then
      begin
       new2:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       if (not (his_mov in desc.hint)) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq(fix_size(new2),[r_thrd+i]);
       end;

       mem_size:=ctx.din.Operand[1].RegValue[0].ASize;

       if (not_impl in desc.mem_reg.opt) then
       begin
        _RR(desc.reg_mem,new1,new2,mem_size); //swapped
       end else
       begin
        _RR(desc.mem_reg,new2,new1,mem_size);
       end;

       if not (his_cmp in desc.hint) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq([r_thrd+i],fix_size(new2));
       end;

      end else
      begin
       i:=GetFrameOffset(ctx.din.Operand[1]);
       _RM(desc.mem_reg,new1,[r_thrd+i]);
      end;

     end;
   mo_reg_ctx:
     begin
      new1:=new_reg(ctx.din.Operand[1]);

      i:=GetFrameOffset(ctx.din.Operand[2]);

      imm:=0;
      if GetTargetOfs(ctx.din,ctx.code,3,imm) then
      begin
       imm_size:=ctx.din.Operand[3].Size;
       mem_size:=ctx.din.Operand[1].Size;

       if (imm_size=os8) and
          (mem_size<>os8) and
          (not (not_impl in desc.reg_im8.opt)) then
       begin
        _RMI8(desc.reg_im8,new1,[r_thrd+i],imm);
       end else
       begin
        _RMI(desc.reg_imm,new1,[r_thrd+i],imm);
       end;

      end else
      begin

       if (not_impl in desc.reg_mem.opt) then
       begin
        new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        i:=GetFrameOffset(ctx.din.Operand[2]);
        movq(fix_size(new2),[r_thrd+i]);

        mem_size:=ctx.din.Operand[1].RegValue[0].ASize;

        _RR(desc.mem_reg,new1,new2,mem_size);
       end else
       begin
        _RM(desc.reg_mem,new1,[r_thrd+i]);
       end;

      end;

     end;
   mo_ctx_ctx:
     begin

      imm:=0;
      if GetTargetOfs(ctx.din,ctx.code,3,imm) then
      begin
       imm_size:=ctx.din.Operand[3].Size;
       mem_size:=ctx.din.Operand[1].Size;

       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
       new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

       i:=GetFrameOffset(ctx.din.Operand[2]);
       movq(fix_size(new2),[r_thrd+i]);

       if (imm_size=os8) and
         (mem_size<>os8) and
         (not (not_impl in desc.reg_im8.opt)) then
       begin
        _RRI8(desc.reg_im8,new2,new1,imm,mem_size); //swapped
       end else
       begin
        _RRI(desc.reg_imm,new2,new1,imm,mem_size); //swapped
       end;

       i:=GetFrameOffset(ctx.din.Operand[1]);
       movq([r_thrd+i],fix_size(new1));
      end else
      begin

       if cmp_reg(ctx.din.Operand[1].RegValue[0],
                  ctx.din.Operand[2].RegValue[0]) then
       begin
        new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
        new2:=new1;

        if not (his_xor in desc.hint) then
        begin
         i:=GetFrameOffset(ctx.din.Operand[1]);
         movq(fix_size(new1),[r_thrd+i]);
        end;

       end else
       begin
        new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
        new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        if (not (his_mov in desc.hint)) then
        begin
         i:=GetFrameOffset(ctx.din.Operand[1]);
         movq(fix_size(new1),[r_thrd+i]);
        end;

        i:=GetFrameOffset(ctx.din.Operand[2]);
        movq(fix_size(new2),[r_thrd+i]);
       end;

       mem_size:=ctx.din.Operand[1].RegValue[0].ASize;

       if (his_mov in desc.hint) and
          (not (not_impl in desc.mem_reg.opt)) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        _RM(desc.mem_reg,fix_size(new2),[r_thrd+i]);
       end else
       begin
        if (not_impl in desc.mem_reg.opt) then
        begin
         _RR(desc.reg_mem,new2,new1,mem_size); //swapped
        end else
        begin
         _RR(desc.mem_reg,new1,new2,mem_size);
        end;

        if not (his_cmp in desc.hint) then
        begin
         i:=GetFrameOffset(ctx.din.Operand[1]);
         movq([r_thrd+i],fix_size(new1));
        end;
       end;

      end;

     end;
   mo_ctx_imm:
     begin
      mem_size:=ctx.din.Operand[1].Size;
      i:=GetFrameOffset(ctx.din.Operand[1]);

      imm:=0;
      if not GetTargetOfs(ctx.din,ctx.code,2,imm) then
      begin
       Assert(false);
      end;

      imm_size:=ctx.din.Operand[2].Size;

      if (mem_size=os32) then
      begin
       mem_size:=os64; //fix size
      end;

      if (imm_size=os8) and
         (mem_size<>os8) and
         (not (not_impl in desc.reg_im8.opt)) then
      begin
       _MI8(desc.reg_im8,mem_size,[r_thrd+i],imm);
      end else
      if (imm_size=os64) and
         (classif_offset_se64(imm)=os64) then
      begin
       Assert(his_mov in desc.hint);

       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       movi64(new1,imm);

       movq([r_thrd+i],fix_size(new1));
      end else
      begin
       _MI(desc.reg_imm,mem_size,[r_thrd+i],imm);
      end;

     end;
   else
    Assert(false);
  end;

end;

//

procedure op_emit_shift(var ctx:t_jit_context2;const desc:t_op_shift);
var
 i:Integer;
 memop:t_memop_shift;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 imm:Int64;
 imm_size:TOperandSize;

 procedure mem_out;
 begin
  with ctx.builder do
   case memop of
    mo_mem_imm8:
      begin
       //input:rax

       imm:=0;
       if not GetTargetOfs(ctx.din,ctx.code,2,imm) then
       begin
        Assert(false);
       end;

       imm_size:=ctx.din.Operand[2].Size;

       Assert(imm_size=os8);
       Assert(not (not_impl in desc.reg_im8.opt));

       _MI8(desc.reg_im8,mem_size,[flags(ctx)+r_tmp0],imm);
      end;
    mo_mem_cl:
      begin
       //input:rax

       _M(desc.mem__cl,mem_size,[flags(ctx)+r_tmp0]);
      end;
    mo_mem_one:
     begin
      //input:rax

      _M(desc.mem_one,mem_size,[flags(ctx)+r_tmp0]);
     end

    else;
   end;
 end;

begin
 memop:=classif_shift2(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem_imm8,
   mo_mem_cl,
   mo_mem_one:
     begin
      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;
     end;
   else;
  end;

 with ctx.builder do
  case memop of
   mo_mem_imm8,
   mo_mem_cl,
   mo_mem_one:
     begin
      if (mem_size=os8) then
      begin
       call(@uplift_jit); //in/out:rax uses:r14

       mem_out;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call(@copyout_mov); //in:rax(addr),r14:(size)

       link_next:=jmp8(0);

       mem_out;

       reta;

       link_next._label:=_label;
      end;
     end;

   mo_ctx_imm8:
     begin
      mem_size:=ctx.din.Operand[1].Size;
      i:=GetFrameOffset(ctx.din.Operand[1]);

      imm:=0;
      if not GetTargetOfs(ctx.din,ctx.code,2,imm) then
      begin
       Assert(false);
      end;

      imm_size:=ctx.din.Operand[2].Size;
      Assert(imm_size=os8);

      if (mem_size=os32) then
      begin
       mem_size:=os64; //fix size
      end;

      _MI8(desc.reg_im8,mem_size,[r_thrd+i],imm);
     end;
    mo_ctx_cl:
     begin
      mem_size:=ctx.din.Operand[1].Size;
      i:=GetFrameOffset(ctx.din.Operand[1]);

      if (mem_size=os32) then
      begin
       mem_size:=os64; //fix size
      end;

      _M(desc.mem__cl,mem_size,[r_thrd+i]);
     end;
    mo_ctx_one:
     begin
      mem_size:=ctx.din.Operand[1].Size;
      i:=GetFrameOffset(ctx.din.Operand[1]);

      if (mem_size=os32) then
      begin
       mem_size:=os64; //fix size
      end;

      _M(desc.mem_one,mem_size,[r_thrd+i]);
     end;

   else
    Assert(false);
  end;

end;

//

procedure op_emit_avx2(var ctx:t_jit_context2;const desc:t_op_desc);
var
 i:Integer;
 memop:t_memop_type2;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 new:TRegValue;

 procedure mem_out;
 begin
  with ctx.builder do
  begin
   //input:rax

   new:=new_reg(ctx.din.Operand[2]);
   _VM(desc.mem_reg,new,[flags(ctx)+r_tmp0],mem_size);
  end;
 end;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:rax

   new:=new_reg(ctx.din.Operand[1]);
   _VM(desc.reg_mem,new,[flags(ctx)+r_tmp0],mem_size);
  end;
 end;

begin
 memop:=classif_memop2(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem_reg,
   mo_reg_mem:
     begin
      build_lea(ctx,get_lea_id(memop),r_tmp0);
      mem_size:=ctx.din.Operand[get_lea_id(memop)].Size;
     end;
   else;
  end;

 with ctx.builder do
  case memop of
   mo_mem_reg:
     begin
      if (his_align in desc.hint) then
      begin
       call(@uplift_jit); //in/out:rax uses:r14

       mem_out;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call(@copyout_mov); //in:rax(addr),r14:(size)

       link_next:=jmp8(0);

       mem_out;

       reta;

       link_next._label:=_label;
      end;
     end;

   mo_reg_mem:
     begin
      if (his_align in desc.hint) then
      begin
       call(@uplift_jit); //in/out:rax uses:r14

       mem_in;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call(@copyin_mov); //in:rax(addr),r14:(size) out:rax

       mem_in;
      end;
     end;

   mo_ctx_reg:
     begin
      new:=new_reg(ctx.din.Operand[2]);

      mem_size:=ctx.din.Operand[1].Size;

      i:=GetFrameOffset(ctx.din.Operand[1]);
      _VM(desc.mem_reg,new,[r_thrd+i],mem_size);
     end;
   mo_reg_ctx:
     begin
      new:=new_reg(ctx.din.Operand[1]);

      mem_size:=ctx.din.Operand[2].Size;

      i:=GetFrameOffset(ctx.din.Operand[2]);
      _VM(desc.reg_mem,new,[r_thrd+i],mem_size);
     end;

   else
    Assert(false);
  end;

end;

procedure op_emit_avx3(var ctx:t_jit_context2;const desc:t_op_type);
var
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 imm:Int64;

 new1,new2:TRegValue;

 procedure mem_out;
 begin
  with ctx.builder do
  begin
   //input:rax

   new1:=new_reg(ctx.din.Operand[2]);
   new2:=new_reg(ctx.din.Operand[3]);

   imm:=0;
   if GetTargetOfs(ctx.din,ctx.code,4,imm) then
   begin
    _VVMI8(desc,new2,new1,[flags(ctx)+r_tmp0],imm);
   end else
   begin
    _VVM(desc,new2,new1,[flags(ctx)+r_tmp0]); //[mem],arg2,arg3 -> arg3,arg2,[mem]
   end;

  end;
 end;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:rax

   new1:=new_reg(ctx.din.Operand[1]);
   new2:=new_reg(ctx.din.Operand[2]);

   imm:=0;
   if GetTargetOfs(ctx.din,ctx.code,4,imm) then
   begin
    _VVMI8(desc,new1,new2,[flags(ctx)+r_tmp0],imm);
   end else
   begin
    _VVM(desc,new1,new2,[flags(ctx)+r_tmp0]);
   end;

  end;
 end;

begin
 if (ofMemory in ctx.din.Operand[3].Flags) then
 begin
  //mo_reg_reg_mem

  build_lea(ctx,3,r_tmp0);
  mem_size:=ctx.din.Operand[3].Size;

  with ctx.builder do
  begin

   if false then
   begin
    call(@uplift_jit); //in/out:rax uses:r14

    mem_in;
   end else
   begin
    //mem_size
    movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

    call(@copyin_mov); //in:rax(addr),r14:(size) out:rax

    mem_in;
   end;

  end;

 end else
 if (ofMemory in ctx.din.Operand[1].Flags) then
 begin
  //mo_mem_reg_reg

  build_lea(ctx,1,r_tmp0);
  mem_size:=ctx.din.Operand[1].Size;

  with ctx.builder do
  begin

   if false then
   begin
    call(@uplift_jit); //in/out:rax uses:r14

    mem_out;
   end else
   begin
    //mem_size
    movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

    call(@copyout_mov); //in:rax(addr),r14:(size)

    link_next:=jmp8(0);

    mem_out;

    reta;

    link_next._label:=_label;
   end;

  end;

 end else
 begin
  Assert(False);
 end;
end;

procedure op_emit_avx3_imm8(var ctx:t_jit_context2;const desc:t_op_type);
var
 i:Integer;
 memop:t_memop_type1;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 new1,new2:TRegValue;

 imm:Int64;

 procedure mem_out;
 begin
  with ctx.builder do
  begin
   //input:rax

   new2:=new_reg(ctx.din.Operand[2]);

   imm:=0;
   GetTargetOfs(ctx.din,ctx.code,3,imm);

   _MVI8(desc,[flags(ctx)+r_tmp0],new2,imm);
  end;
 end;

begin
 memop:=classif_memop1(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem:
    begin
     build_lea(ctx,1,r_tmp0);
     mem_size:=ctx.din.Operand[1].Size;

     if (mem_size=os8) then
     begin
      call(@uplift_jit); //in/out:rax uses:r14

      mem_out;
     end else
     begin
      //mem_size
      movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

      call(@copyout_mov); //in:rax(addr),r14:(size)

      link_next:=jmp8(0);

      mem_out;

      reta;

      link_next._label:=_label;
     end;
    end;

   mo_ctx:
    begin
     mem_size:=ctx.din.Operand[1].Size;
     i:=GetFrameOffset(ctx.din.Operand[1]);

     new2:=new_reg(ctx.din.Operand[2]);

     imm:=0;
     GetTargetOfs(ctx.din,ctx.code,3,imm);

     if (mem_size=os32) then
     begin
      new1:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

      _VVI8(desc,new1,new2,imm);

      movq([r_thrd+i],new1);
     end else
     begin
      _MVI8(desc,[r_thrd+i],new2,imm);
     end;

    end;

   else
    Assert(false);
  end;

end;

procedure print_disassemble(addr:Pointer;vsize:Integer);
var
 proc:TDbgProcess;
 adec:TX86AsmDecoder;
 ptr,fin:Pointer;
 ACodeBytes,ACode:RawByteString;
begin
 ptr:=addr;
 fin:=addr+vsize;

 proc:=TDbgProcess.Create(dm64);
 adec:=TX86AsmDecoder.Create(proc);

 while (ptr<fin) do
 begin
  adec.Disassemble(ptr,ACodeBytes,ACode);
  Writeln(ACodeBytes:32,' ',ACode);
 end;

 adec.Free;
 proc.Free;
end;

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
   1:op_emit1(ctx,imul_desc1,True); //R
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
  op_emit1(ctx,mul_desc,True); //R
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
  op_emit1(ctx,div_desc,True); //R
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

procedure op_emit_avx_F3(var ctx:t_jit_context2;const desc:t_op_type);
var
 i:Integer;
 memop:t_memop_type2;
 mem_size:TOperandSize;

 new1,new2:TRegValue;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   case memop of
    mo_reg_mem:
     begin
      //input:rax

      new1:=new_reg(ctx.din.Operand[1]);
      _VM_F3(desc,new1,[flags(ctx)+r_tmp0],mem_size);
     end;
    mo_ctx_mem:
     begin
      //input:rax

      //load?

      new1:=new_reg_size(r_tmp1,ctx.din.Operand[1]);
      _VM_F3(desc,new1,[flags(ctx)+r_tmp0],mem_size);

      i:=GetFrameOffset(ctx.din.Operand[1]);
      movq([r_thrd+i],fix_size(new1));
     end;
    else;
   end;
  end;
 end;

begin
 memop:=classif_memop2(ctx.din);

 with ctx.builder do
  case memop of
   mo_reg_mem,
   mo_ctx_mem:
     begin
      build_lea(ctx,get_lea_id(memop),r_tmp0);
      mem_size:=ctx.din.Operand[get_lea_id(memop)].Size;
     end;
   else;
  end;

 with ctx.builder do
  case memop of
   mo_reg_mem,
   mo_ctx_mem:
     begin
      if false then
      begin
       call(@uplift_jit); //in/out:rax uses:r14

       mem_in;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call(@copyin_mov); //in:rax(addr),r14:(size) out:rax

       mem_in;
      end;
     end;

   mo_ctx_reg:
     begin
      new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
      new2:=new_reg(ctx.din.Operand[2]);

      mem_size:=ctx.din.Operand[1].Size;

      //load?

      _VV_F3(desc,new1,new2,mem_size);

      i:=GetFrameOffset(ctx.din.Operand[1]);
      movq([r_thrd+i],fix_size(new1));
     end;
   mo_reg_ctx:
     begin
      new1:=new_reg(ctx.din.Operand[1]);
      new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

      mem_size:=ctx.din.Operand[2].Size;

      i:=GetFrameOffset(ctx.din.Operand[2]);
      movq(fix_size(new2),[r_thrd+i]);

      _VV_F3(desc,new1,new2,mem_size);
     end;

   mo_ctx_ctx:
     begin
      new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
      new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

      //load?

      i:=GetFrameOffset(ctx.din.Operand[2]);
      movq(fix_size(new2),[r_thrd+i]);

      mem_size:=ctx.din.Operand[1].RegValue[0].ASize;

      _VV_F3(desc,new1,new2,mem_size);

      i:=GetFrameOffset(ctx.din.Operand[1]);
      movq([r_thrd+i],fix_size(new1));
     end;

   else
    Assert(false);
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

type
 p_jit_label=^t_jit_label;
 t_jit_label=record
  link:TAILQ_ENTRY;
  src:Pointer;
  id:Integer;
 end;

 p_jit_entry_point=^t_jit_entry_point;
 t_jit_entry_point=record
  link:TAILQ_ENTRY;
  id:t_jit_i_link;
  dst:Pointer;
 end;

var
 labels:TAILQ_HEAD=(tqh_first:nil;tqh_last:@labels.tqh_first);
 lfrwrd:TAILQ_HEAD=(tqh_first:nil;tqh_last:@lfrwrd.tqh_first);

procedure add_jit_label(head:P_TAILQ_HEAD;src:Pointer;id:Integer);
var
 entry:p_jit_label;
begin
 entry:=AllocMem(Sizeof(t_jit_label));
 entry^.src:=src;
 entry^.id:=id;
 TAILQ_INSERT_TAIL(head,entry,@entry^.link);
end;

function find_jit_label(head:P_TAILQ_HEAD;src:Pointer):Integer;
var
 entry:p_jit_label;
begin
 Result:=-1;

 entry:=TAILQ_FIRST(head);
 while (entry<>nil) do
 begin
  if (entry^.src=src) then
  begin
   Exit(entry^.id);
  end;

  entry:=TAILQ_NEXT(entry,@entry^.link);
 end;

end;

procedure add_entry_point(head:P_TAILQ_HEAD;id:t_jit_i_link;dst:Pointer);
var
 entry:p_jit_entry_point;
begin
 entry:=AllocMem(Sizeof(t_jit_entry_point));
 entry^.id:=id;
 entry^.dst:=dst;
 TAILQ_INSERT_TAIL(head,entry,@entry^.link);
end;

procedure add_entry_point(dst:Pointer);
begin
 if (dst=nil) then Exit;
 add_entry_point(@lfrwrd,Default(t_jit_i_link),dst);
end;

function fetch_entry_point(head:P_TAILQ_HEAD;var id:t_jit_i_link;var dst:Pointer):Boolean;
var
 entry:p_jit_entry_point;
 min:p_jit_entry_point;
begin
 Result:=false;
 min:=nil;

 entry:=TAILQ_FIRST(head);
 while (entry<>nil) do
 begin
  if (min=nil) then
  begin
   min:=entry;
  end else
  if (min^.dst>entry^.dst) then
  begin
   min:=entry;
  end;

  entry:=TAILQ_NEXT(entry,@entry^.link);
 end;

 if (min<>nil) then
 begin
  id :=min^.id;
  dst:=min^.dst;
  TAILQ_REMOVE(head,min,@min^.link);
  FreeMem(min);
  Result:=True;
 end;
end;

procedure op_jmp_dispatcher(var ctx:t_jit_context2);
begin
 ctx.builder.call(nil); //input:rax TODO jmp dispatcher
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

  call(@uplift_jit); //in/out:rax uses:r14

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

  call(@uplift_jit); //in/out:rax uses:r14

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
begin
 if (ctx.din.Operand[1].RegValue[0].AType=regNone) then
 begin
  ofs:=0;
  if not GetTargetOfs(ctx.din,ctx.code,1,ofs) then
  begin
   Assert(false);
  end;

  dst:=ctx.ptr_next+ofs;

  i:=find_jit_label(@labels,dst);

  if (i<>-1) then
  begin
   op_push_rip(ctx,false);
   //
   ctx.builder.jmp(i);
  end else
  begin
   op_push_rip(ctx,false);
   //
   id:=ctx.builder.jmp(-1);
   add_entry_point(@lfrwrd,id,dst);
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
begin
 if (ctx.din.Operand[1].RegValue[0].AType=regNone) then
 begin
  ofs:=0;
  if not GetTargetOfs(ctx.din,ctx.code,1,ofs) then
  begin
   Assert(false);
  end;

  dst:=ctx.ptr_next+ofs;

  i:=find_jit_label(@labels,dst);

  if (i<>-1) then
  begin
   ctx.builder.jmp(i);
  end else
  begin
   id:=ctx.builder.jmp(-1);
   add_entry_point(@lfrwrd,id,dst);
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
 i:Integer;
begin
 ofs:=0;
 if not GetTargetOfs(ctx.din,ctx.code,1,ofs) then
 begin
  Assert(false);
 end;

 dst:=ctx.ptr_next+ofs;

 i:=find_jit_label(@labels,dst);

 if (i<>-1) then
 begin
  ctx.builder.jcc(ctx.din.OpCode.Suffix,i);
 end else
 begin
  id:=ctx.builder.jcc(ctx.din.OpCode.Suffix,-1);
  add_entry_point(@lfrwrd,id,dst);
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
  op_emit1(ctx,desc,False);
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
  hint:[];
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
  op_emit1(ctx,inc_desc,False);
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
  op_emit1(ctx,dec_desc,False);
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
  op_emit1(ctx,neg_desc,False);
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
  op_emit1(ctx,not_desc,False);
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

procedure op_push(var ctx:t_jit_context2);
var
 i:Integer;
 stack,new:TRegValue;
begin
 with ctx.builder do
 begin
  stack:=r_tmp0;

  if is_memory(ctx.din) then
  begin
   build_lea(ctx,1,r_tmp0);

   call(@uplift_jit); //in/out:rax uses:r14

   new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

   movq(new,[r_tmp0]);
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

  i:=GetFrameOffset(rsp);
  movq(stack,[r_thrd+i]);
  leaq(stack,[stack-OPERAND_BYTES[new.ASize]]);
  movq([r_thrd+i],stack);

  call(@uplift_jit); //in/out:rax uses:r14

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

  call(@uplift_jit); //in/out:rax uses:r14

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

  call(@uplift_jit); //in/out:rax uses:r14

  if is_memory(ctx.din) then
  begin
   new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

   movq(new,[stack]);

   build_lea(ctx,1,r_tmp0);

   call(@uplift_jit); //in/out:rax uses:r14

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

procedure op_int(var ctx:t_jit_context2);
var
 i:Integer;
 id:Byte;
begin
 i:=ctx.din.Operand[1].ByteCount;
 Assert(i=1);
 id:=PByte(ctx.code)[i];

 case id of
  $44: //system error?
   begin
    //
    ctx.builder.call(nil); //TODO error dispatcher
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
 ctx.builder.call(nil); //TODO exit dispatcher
 ctx.ptr_next:=nil; //trim
end;

procedure op_iretq(var ctx:t_jit_context2);
begin
 //exit proc?
 ctx.builder.call(nil); //TODO exit dispatcher
 ctx.ptr_next:=nil; //trim
end;

procedure op_cpuid(var ctx:t_jit_context2);
begin
 ctx.builder.call(nil); //TODO CPUID
end;

procedure op_nop(var ctx:t_jit_context2);
begin
 //align?
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

procedure op_emit_bmi_rmr(var ctx:t_jit_context2;const desc:t_op_type);
var
 i:Integer;
 mem_size:TOperandSize;

 new1,new2,new3:TRegValue;
begin
 with ctx.builder do
 begin

  if is_preserved(ctx.din.Operand[1]) then
  begin
   new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  end else
  begin
   new1:=new_reg(ctx.din.Operand[1]);
  end;

  if is_memory(ctx.din.Operand[2]) then
  begin
   new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

   mem_size:=ctx.din.Operand[2].Size;

   //mem_size
   movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

   call(@copyin_mov); //in:rax(addr),r14:(size) out:rax

   movq(new2,[r_tmp0]);
  end else
  if is_preserved(ctx.din.Operand[2]) then
  begin
   new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);
   //
   i:=GetFrameOffset(ctx.din.Operand[2]);
   movq(new2,[r_thrd+i]);
  end else
  begin
   new2:=new_reg(ctx.din.Operand[2]);
  end;

  if is_preserved(ctx.din.Operand[3]) then
  begin
   new3:=new_reg_size(r_tmp1,ctx.din.Operand[3]);
   //
   i:=GetFrameOffset(ctx.din.Operand[3]);
   movq(new3,[r_thrd+i]);
  end else
  begin
   new3:=new_reg(ctx.din.Operand[3]);
  end;

  _VVV(desc,new1,new3,new2); //1 3 2

  if is_preserved(ctx.din.Operand[1]) then
  begin
   i:=GetFrameOffset(ctx.din.Operand[1]);
   movq([r_thrd+i],fix_size(new1));
  end;

 end;
end;

//

procedure op_emit_bmi_rrm(var ctx:t_jit_context2;const desc:t_op_type);
var
 i:Integer;
 mem_size:TOperandSize;

 new1,new2,new3:TRegValue;
begin
 with ctx.builder do
 begin

  if is_preserved(ctx.din.Operand[1]) then
  begin
   new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  end else
  begin
   new1:=new_reg(ctx.din.Operand[1]);
  end;

  if is_preserved(ctx.din.Operand[2]) then
  begin
   new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);
   //
   i:=GetFrameOffset(ctx.din.Operand[2]);
   movq(new2,[r_thrd+i]);
  end else
  begin
   new2:=new_reg(ctx.din.Operand[2]);
  end;

  if is_memory(ctx.din.Operand[2]) then
  begin
   new3:=new_reg_size(r_tmp1,ctx.din.Operand[3]);

   mem_size:=ctx.din.Operand[3].Size;

   //mem_size
   movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

   call(@copyin_mov); //in:rax(addr),r14:(size) out:rax

   movq(new3,[r_tmp0]);
  end else
  if is_preserved(ctx.din.Operand[3]) then
  begin
   new3:=new_reg_size(r_tmp1,ctx.din.Operand[3]);
   //
   i:=GetFrameOffset(ctx.din.Operand[3]);
   movq(new3,[r_thrd+i]);
  end else
  begin
   new3:=new_reg(ctx.din.Operand[3]);
  end;

  _VVV(desc,new1,new2,new3); //1 2 3

  if is_preserved(ctx.din.Operand[1]) then
  begin
   i:=GetFrameOffset(ctx.din.Operand[1]);
   movq([r_thrd+i],fix_size(new1));
  end;

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
  op_emit1(ctx,fnstcw_desc,False);
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
  op_emit1(ctx,fldcw_desc,False);
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
  op_emit1(ctx,fxsave_desc,False);
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
  op_emit1(ctx,fxrstor_desc,False);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure pick();
var
 addr:Pointer;
 id:t_jit_i_link;

 proc:TDbgProcess;
 adec:TX86AsmDecoder;
 ptr,fin:Pointer;
 ACodeBytes,ACode:RawByteString;
 ctx:t_jit_context2;
 cb:t_jit_cb;

 len1,len2,i:Integer;
begin

 if not fetch_entry_point(@lfrwrd,id,addr) then
 begin
  Assert(false);
 end;

 ptr:=addr;
 fin:=Pointer(Int64(-1));

 Writeln(SizeOf(TOpCode),' ',Succ(Ord(High(TOpCode))));
 Writeln(SizeOf(TOpCodeSuffix),' ',Succ(Ord(High(TOpCodeSuffix))));

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

 jit_cbs[OPPnone,OPcall,OPSnone]:=@op_call;
 jit_cbs[OPPnone,OPjmp ,OPSnone]:=@op_jmp;
 jit_cbs[OPPnone,OPret ,OPSnone]:=@op_ret;

 jit_cbs[OPPnone,OPtest,OPSnone]:=@op_test;
 jit_cbs[OPPnone,OPcmp ,OPSnone]:=@op_cmp;

 jit_cbs[OPPnone,OPcmpxchg,OPSnone]:=@op_cmpxchg;

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

 jit_cbs[OPPnone,OPpxor,OPSnone]:=@op_pxor;

 jit_cbs[OPPnone,OPemms,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPvzeroall,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPfninit,OPSnone]:=@add_orig;
 jit_cbs[OPPnone,OPrdtsc ,OPSnone]:=@add_orig;

 jit_cbs[OPPnone,OPpushf ,OPSnone]:=@op_pushfq;
 jit_cbs[OPPnone,OPpushf ,OPSx_q ]:=@op_pushfq;
 //
 //jit_cbs[OPpopf  ,OPSnone]:=@;
 //jit_cbs[OPpopf  ,OPSx_q ]:=@;

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
 jit_cbs[OPPnone,OPint,OPSnone]:=@op_int;
 jit_cbs[OPPnone,OPud2,OPSnone]:=@op_ud2;

 jit_cbs[OPPnone,OPiret,OPSnone]:=@op_iretq;
 jit_cbs[OPPnone,OPiret,OPSx_d ]:=@op_iretq;
 jit_cbs[OPPnone,OPiret,OPSx_q ]:=@op_iretq;

 jit_cbs[OPPnone,OPcpuid,OPSnone]:=@op_cpuid;

 jit_cbs[OPPnone,OPnop,OPSnone]:=@op_nop;

 jit_cbs[OPPnone,OPinc,OPSnone]:=@op_inc;
 jit_cbs[OPPnone,OPdec,OPSnone]:=@op_dec;
 jit_cbs[OPPnone,OPneg,OPSnone]:=@op_neg;
 jit_cbs[OPPnone,OPnot,OPSnone]:=@op_not;
 jit_cbs[OPPnone,OPbswap,OPSnone]:=@op_bswap;

 jit_cbs[OPPnone,OPfnstcw,OPSnone]:=@op_fnstcw;
 jit_cbs[OPPnone,OPfldcw ,OPSnone]:=@op_fldcw;

 jit_cbs[OPPnone,OPfxsave ,OPSnone]:=@op_fxsave;
 jit_cbs[OPPnone,OPfxrstor,OPSnone]:=@op_fxrstor;

 proc:=TDbgProcess.Create(dm64);
 adec:=TX86AsmDecoder.Create(proc);

 while (ptr<fin) do
 begin

  adec.Disassemble(ptr,ACodeBytes,ACode);

  Writeln('original------------------------':32,' ','0x',HexStr(ptr-adec.Disassembler.CodeIdx));
  Writeln(ACodeBytes:32,' ',ACode);
  Writeln('original------------------------':32,' ','0x',HexStr(ptr));

  ctx.ptr_next:=ptr;
  ctx.ptr_curr:=ptr-adec.Disassembler.CodeIdx;

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
   Assert(false);
  end;

  len1:=Length(ctx.builder.AInstructions);

  cb(ctx);

  len2:=Length(ctx.builder.AInstructions);

  if (len1<>len2) then
  begin
   Writeln('recompiled----------------------':32,' ','');
   For i:=len1 to len2-1 do
   begin

    print_disassemble(@ctx.builder.AInstructions[i].AData,
                       ctx.builder.AInstructions[i].ASize);

   end;
   Writeln('recompiled----------------------':32,' ','');
  end;

  if (len1<>len2) then
  begin
   add_jit_label(@labels,ctx.ptr_curr,len1);
  end;

  if (len1<>len2) then
  begin
   i:=find_jit_label(@labels,ptr);

   if (i<>-1) then
   begin
    ctx.builder.jmp(i);
    ctx.ptr_next:=nil;
    Writeln('jmp next:0x',HexStr(ptr));
   end;

  end;

  if (ctx.ptr_next=nil) then
  begin
   repeat

    if not fetch_entry_point(@lfrwrd,id,addr) then
    begin
     Assert(false);
    end;

    i:=find_jit_label(@labels,addr);
    if (i=-1) then
    begin
     Writeln('not found:0x',HexStr(addr));
     writeln;
     Break;
    end;

   until false;

   ptr:=addr;
  end;

  //ptr:=ctx.ptr_next; //jmp switch
 end;

 adec.Free;
 proc.Free;
end;


end.

