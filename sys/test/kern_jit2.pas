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
 Result:=is_preserved(r.RegValue);
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

type
 p_jit_context2=^t_jit_context2;
 t_jit_context2=record
  ptr_curr:Pointer;
  ptr_next:Pointer;

  dis:TX86Disassembler;
  din:TInstruction;

  builder:t_jit_builder;

 end;

 t_jit_cb=procedure(var ctx:t_jit_context2);

var
 jit_cbs:array[TOpCode,TOpCodeSuffix] of t_jit_cb;

procedure add_orig(var ctx:t_jit_context2);
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 Move(ctx.ptr_curr^,ji.AData,ctx.dis.CodeIdx);

 ji.ASize:=ctx.dis.CodeIdx;

 ctx.builder._add(ji);
end;

const
 r_thrd:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex:15),(AType:regNone))); //r15
 r_tmp0:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex: 0),(AType:regNone))); //rax
 r_tmp1:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex:14),(AType:regNone))); //r14

function new_reg(const r:TRegValue):t_jit_reg; inline;
begin
 Result:=Default(t_jit_reg);
 Result.ARegValue[0]:=r;
end;

function new_reg(const Operand:TOperand):t_jit_reg; inline;
begin
 Result:=Default(t_jit_reg);
 Result.ARegValue[0]:=Operand.RegValue[0];
end;

function new_reg_size(const r:t_jit_reg;ASize:TOperandSize):t_jit_reg; inline;
begin
 Result:=r;
 Result.ARegValue[0].ASize:=ASize;
end;

function new_reg_size(const r:t_jit_reg;const RegValue:TRegValues):t_jit_reg; inline;
begin
 Result:=new_reg_size(r,RegValue[0].ASize);
end;

function new_reg_size(const r:t_jit_reg;const Operand:TOperand):t_jit_reg; inline;
begin
 Result:=new_reg_size(r,Operand.RegValue[0].ASize);
end;

function fix_size(const r:t_jit_reg):t_jit_reg; inline;
begin
 Result:=r;
 if (Result.ARegValue[0].ASize=os32) then
 begin
  Result.ARegValue[0].ASize:=os64;
 end;
end;

procedure build_lea(var ctx:t_jit_context2;id:Byte;reg:t_jit_reg);
var
 RegValue:TRegValues;
 adr,new,new2:t_jit_reg;
 ofs:Int64;
 i:Integer;
 AScale:Byte;
begin
 Assert(ctx.din.SegmentReg=-1);

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
   GetTargetOfs(ctx.din,ctx.ptr_curr,id,ofs);
   ofs:=Int64(ctx.ptr_next)+ofs;

   if (classif_offset_se64(ofs)=3) then
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
   GetTargetOfs(ctx.din,ctx.ptr_curr,id,ofs);

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
    new:=new_reg(RegValue[0]);
    //
    //AScale in new
    leaq(adr,[new+ofs]);
   end;

   if (RegValue[1].AType<>regNone) then
   begin
    if is_preserved(RegValue[1]) then
    begin
     i:=GetFrameOffset(RegValue[1]);
     addq(adr,[r_thrd+i]);
    end else
    begin
     new:=new_reg(RegValue[1]);
     addq(adr,new);
    end;
   end;
  end else
  begin
   ofs:=0;
   GetTargetOfs(ctx.din,ctx.ptr_curr,id,ofs);

   new:=new_reg(RegValue[0]);

   if (RegValue[1].AType<>regNone) then
   begin
    new2:=new_reg(RegValue[1]);
    //
    //AScale in new
    leaq(adr,[new+new2+ofs]);
   end else
   begin
    //AScale in new
    leaq(adr,[new+ofs]);
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
 t_op_type=packed record
  op,op8,index:Byte;
 end;

 t_op_desc=packed record
  mem_reg:t_op_type; //reg_reg
  reg_mem:t_op_type;
  reg_imm:t_op_type; //mem_imm
  reg_im8:t_op_type; //mem_im8
  mem__cl:t_op_type; //reg__cl
  mem_one:t_op_type; //reg_one
  hint:Set of (his_mov,his_xor,his_cmp,his_align);
 end;

//in/out:rax uses:r14
procedure uplift_jit; assembler; nostackframe;
label
 _exit;
asm
 pushfq
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

  push %rax

  mov  data,%rax

  mov  8(%rbp),%rdi //ret addr
  add  $2,%rdi      //jmp near
  call %rdi         //reg->data

  pop     %rsi      //vaddr
  mov     data,%rdi //data
  movzbq %r14b,%rdx //size

  call copyout

  pop  %r11
  pop  %r10
  pop  %r9
  pop  %r8
  pop  %rcx
  pop  %rdx
  pop  %rsi
  pop  %rdi

  jmp _exit
 _simple:

  call uplift_jit

  mov  8(%rbp),%r14 //ret addr
  add  $2,%r14      //jmp near
  call %r14         //reg->data

 _exit:
 //
 popfq
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

procedure op_emit1(var ctx:t_jit_context2;const desc:t_op_type);
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

  _mov(desc.op,desc.op8,desc.index,mem_size,[r_tmp0]);
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
      call(@uplift_jit); //in/out:rax

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

     if (mem_size=os32) then
     begin
      mem_size:=os64; //fix size
     end;

     _mov(desc.op,desc.op8,desc.index,mem_size,[r_thrd+i]);
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

 new,new2:t_jit_reg;

 procedure mem_out;
 begin
  with ctx.builder do
   case memop of
    mo_mem_reg:
      begin
       //input:rax

       new:=new_reg(ctx.din.Operand[2]);
       _mov(desc.mem_reg.op,desc.mem_reg.op8,new,[r_tmp0]);
      end;
    mo_mem_imm:
      begin
       //input:rax

       imm:=0;
       if not GetTargetOfs(ctx.din,ctx.ptr_curr,2,imm) then
       begin
        Assert(false);
       end;

       imm_size:=ctx.din.Operand[2].Size;

       Assert(imm_size<>os64);

       if (imm_size=os8) and (mem_size<>os8) and (desc.reg_im8.index<>255) then
       begin
        _movi8(desc.reg_im8.op,desc.reg_im8.index,mem_size,[r_tmp0],imm);
       end else
       begin
        _movi(desc.reg_imm.op,desc.reg_imm.op8,desc.reg_imm.index,mem_size,[r_tmp0],imm);
       end;
      end;
    mo_mem_ctx:
      begin
       //input:rax

       new:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

       i:=GetFrameOffset(ctx.din.Operand[2]);
       movq(new,[r_thrd+i]);

       _mov(desc.mem_reg.op,desc.mem_reg.op8,new,[r_tmp0]);
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

       new:=new_reg(ctx.din.Operand[1]);
       _mov(desc.reg_mem.op,desc.reg_mem.op8,new,[r_tmp0]);
      end;
    mo_ctx_mem:
      begin
       //input:rax

       new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

       if (not (his_mov in desc.hint)) or
          (new.ARegValue[0].ASize in [os8,os16]) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq(fix_size(new),[r_thrd+i]);
       end;

       _mov(desc.reg_mem.op,desc.reg_mem.op8,new,[r_tmp0]);

       if not (his_cmp in desc.hint) then
       begin
        movq([r_thrd+i],fix_size(new));
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
       call(@uplift_jit); //in/out:rax

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
       call(@uplift_jit); //in/out:rax

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

      if (new.ARegValue[0].ASize=os32) then
      begin
       new2:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       if (not (his_mov in desc.hint)) or
          (new.ARegValue[0].ASize<>new2.ARegValue[0].ASize) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq(fix_size(new2),[r_thrd+i]);
       end;

       _mov(desc.mem_reg.op,desc.mem_reg.op8,new2,new);

       if not (his_cmp in desc.hint) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq([r_thrd+i],fix_size(new2));
       end;
      end else
      begin
       i:=GetFrameOffset(ctx.din.Operand[1]);
       _mov(desc.mem_reg.op,desc.mem_reg.op8,new,[r_thrd+i]);
      end;

     end;
   mo_reg_ctx:
     begin
      new:=new_reg(ctx.din.Operand[1]);

      i:=GetFrameOffset(ctx.din.Operand[2]);
      _mov(desc.reg_mem.op,desc.reg_mem.op8,new,[r_thrd+i]);
     end;
   mo_ctx_ctx:
     begin

      if cmp_reg(ctx.din.Operand[1].RegValue[0],
                 ctx.din.Operand[2].RegValue[0]) then
      begin
       new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       new2:=new;

       if not (his_xor in desc.hint) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq(fix_size(new),[r_thrd+i]);
       end;

      end else
      begin
       new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       new2:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

       if (not (his_mov in desc.hint)) or
          (new.ARegValue[0].ASize<>new2.ARegValue[0].ASize) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq(fix_size(new),[r_thrd+i]);
       end;

       i:=GetFrameOffset(ctx.din.Operand[2]);
       movq(fix_size(new2),[r_thrd+i]);
      end;

      _mov(desc.mem_reg.op,desc.mem_reg.op8,new,new2);

      if not (his_cmp in desc.hint) then
      begin
       i:=GetFrameOffset(ctx.din.Operand[1]);
       movq([r_thrd+i],fix_size(new));
      end;
     end;
   mo_ctx_imm:
     begin
      mem_size:=ctx.din.Operand[1].Size;
      i:=GetFrameOffset(ctx.din.Operand[1]);

      imm:=0;
      if not GetTargetOfs(ctx.din,ctx.ptr_curr,2,imm) then
      begin
       Assert(false);
      end;

      imm_size:=ctx.din.Operand[2].Size;

      if (mem_size=os32) then
      begin
       mem_size:=os64; //fix size
      end;

      if (imm_size=os8) and (mem_size<>os8) and (desc.reg_im8.index<>255) then
      begin
       _movi8(desc.reg_im8.op,desc.reg_im8.index,mem_size,[r_thrd+i],imm);
      end else
      if (classif_offset_se64(imm)=3) then
      begin
       Assert(his_mov in desc.hint);

       new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       movi64(new,imm);

       movq([r_thrd+i],fix_size(new));
      end else
      begin
       _movi(desc.reg_imm.op,desc.reg_imm.op8,desc.reg_imm.index,mem_size,[r_thrd+i],imm);
      end;

     end;
   else
    Assert(false);
  end;

end;

//

procedure op_emit_shift(var ctx:t_jit_context2;const desc:t_op_desc);
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
       if not GetTargetOfs(ctx.din,ctx.ptr_curr,2,imm) then
       begin
        Assert(false);
       end;

       imm_size:=ctx.din.Operand[2].Size;

       Assert(imm_size=os8);
       Assert(desc.reg_im8.index<>255);

       _movi8(desc.reg_im8.op,desc.reg_im8.index,mem_size,[r_tmp0],imm);
      end;
    mo_mem_cl:
      begin
       //input:rax

       _mov(desc.mem__cl.op,desc.mem__cl.op8,desc.mem__cl.index,mem_size,[r_tmp0]);
      end;
    mo_mem_one:
     begin
      //input:rax

      _mov(desc.mem_one.op,desc.mem_one.op8,desc.mem_one.index,mem_size,[r_tmp0]);
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
       call(@uplift_jit); //in/out:rax

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
      if not GetTargetOfs(ctx.din,ctx.ptr_curr,2,imm) then
      begin
       Assert(false);
      end;

      imm_size:=ctx.din.Operand[2].Size;
      Assert(imm_size=os8);

      if (mem_size=os32) then
      begin
       mem_size:=os64; //fix size
      end;

      _movi8(desc.reg_im8.op,desc.reg_im8.index,mem_size,[r_thrd+i],imm);
     end;
    mo_ctx_cl:
     begin
      mem_size:=ctx.din.Operand[1].Size;
      i:=GetFrameOffset(ctx.din.Operand[1]);

      if (mem_size=os32) then
      begin
       mem_size:=os64; //fix size
      end;

      _mov(desc.mem__cl.op,desc.mem__cl.op8,desc.mem__cl.index,mem_size,[r_thrd+i]);
     end;
    mo_ctx_one:
     begin
      mem_size:=ctx.din.Operand[1].Size;
      i:=GetFrameOffset(ctx.din.Operand[1]);

      if (mem_size=os32) then
      begin
       mem_size:=os64; //fix size
      end;

      _mov(desc.mem_one.op,desc.mem_one.op8,desc.mem_one.index,mem_size,[r_thrd+i]);
     end;

   else
    Assert(false);
  end;

end;

//

procedure op_emit_movsxd(var ctx:t_jit_context2;op,op8:Byte);
var
 i:Integer;
 memop:t_memop_type2;
 mem_size:TOperandSize;

 new,new2:t_jit_reg;

 procedure mem_in;
 begin
  with ctx.builder do
   case memop of
    mo_reg_mem:
      begin
       //input:rax

       new:=new_reg(ctx.din.Operand[1]);

       _mov(op,op8,new,[r_tmp0]);
      end;
    mo_ctx_mem:
      begin
       //input:rax

       new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

       if (new.ARegValue[0].ASize in [os8,os16]) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq(fix_size(new),[r_thrd+i]);
       end;

       _mov(op,op8,new,[r_tmp0]);

       movq([r_thrd+i],fix_size(new));
      end;
    else;
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
      //mem_size
      movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

      call(@copyin_mov); //in:rax(addr),r14:(size) out:rax

      mem_in;
     end;

   mo_ctx_reg:
     begin
      new:=new_reg(ctx.din.Operand[2]);

      new2:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

      i:=GetFrameOffset(ctx.din.Operand[1]);
      movq(fix_size(new2),[r_thrd+i]);

      mem_size:=ctx.din.Operand[1].RegValue[0].ASize; //fix size

      _mov(op,op8,new,new2,mem_size); //swaped

      movq([r_thrd+i],fix_size(new2));
     end;
   mo_reg_ctx:
     begin
      new:=new_reg(ctx.din.Operand[1]);

      new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

      i:=GetFrameOffset(ctx.din.Operand[2]);
      movq(fix_size(new2),[r_thrd+i]);

      mem_size:=ctx.din.Operand[1].RegValue[0].ASize; //fix size

      _mov(op,op8,new2,new,mem_size); //swaped
     end;
   mo_ctx_ctx:
     begin

      if cmp_reg(ctx.din.Operand[1].RegValue[0],
                 ctx.din.Operand[2].RegValue[0]) then
      begin
       new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       new2:=new;

       i:=GetFrameOffset(ctx.din.Operand[1]);
       movq(fix_size(new),[r_thrd+i]);
      end else
      begin
       new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       new2:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

       i:=GetFrameOffset(ctx.din.Operand[1]);
       movq(fix_size(new),[r_thrd+i]);

       i:=GetFrameOffset(ctx.din.Operand[2]);
       movq(fix_size(new2),[r_thrd+i]);
      end;

      mem_size:=ctx.din.Operand[1].RegValue[0].ASize; //fix size

      //fix size
      _mov(op,op8,new2,new,mem_size);  //swaped

      i:=GetFrameOffset(ctx.din.Operand[1]);
      movq([r_thrd+i],new);
     end;

   else
    Assert(false);
  end;

end;

//

procedure op_emit_movz(var ctx:t_jit_context2;op:Byte);
var
 i:Integer;
 memop:t_memop_type2;
 mem_size:TOperandSize;

 new,new2:t_jit_reg;

 procedure mem_in;
 begin
  with ctx.builder do
   case memop of
    mo_reg_mem:
      begin
       //input:rax

       new:=new_reg(ctx.din.Operand[1]);
       _movz(op,new,[r_tmp0]);
      end;
    mo_ctx_mem:
      begin
       //input:rax

       new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

       if (new.ARegValue[0].ASize in [os8,os16]) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq(fix_size(new),[r_thrd+i]);
       end;

       _movz(op,new,[r_tmp0]);

       movq([r_thrd+i],fix_size(new));
      end;
    else;
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
      //mem_size
      movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

      call(@copyin_mov); //in:rax(addr),r14:(size) out:rax

      mem_in;
     end;

   mo_ctx_reg:
     begin
      new:=new_reg(ctx.din.Operand[2]);

      new2:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

      i:=GetFrameOffset(ctx.din.Operand[1]);
      movq(fix_size(new2),[r_thrd+i]);

      _movz(op,new2,new);

      movq([r_thrd+i],fix_size(new2));
     end;
   mo_reg_ctx:
     begin
      new:=new_reg(ctx.din.Operand[1]);

      new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

      i:=GetFrameOffset(ctx.din.Operand[2]);
      movq(fix_size(new2),[r_thrd+i]);

      _movz(op,new,new2);
     end;
   mo_ctx_ctx:
     begin

      if cmp_reg(ctx.din.Operand[1].RegValue[0],
                 ctx.din.Operand[2].RegValue[0]) then
      begin
       new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       new2:=new;

       i:=GetFrameOffset(ctx.din.Operand[1]);
       movq(fix_size(new),[r_thrd+i]);
      end else
      begin
       new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       new2:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

       i:=GetFrameOffset(ctx.din.Operand[1]);
       movq(fix_size(new),[r_thrd+i]);

       i:=GetFrameOffset(ctx.din.Operand[2]);
       movq(fix_size(new2),[r_thrd+i]);
      end;

      _movz(op,new,new2);

      i:=GetFrameOffset(ctx.din.Operand[1]);
      movq([r_thrd+i],fix_size(new));
     end;

   else
    Assert(false);
  end;

end;

//

procedure op_emit_movzm(var ctx:t_jit_context2;const desc:t_op_type);
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

   _movzm(desc.op,desc.index,mem_size,[r_tmp0]);
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
      call(@uplift_jit); //in/out:rax

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

     if (mem_size=os32) then
     begin
      mem_size:=os64; //fix size
     end;

     _movzm(desc.op,desc.index,mem_size,[r_thrd+i]);
    end;

   else
    Assert(false);
  end;

end;

//

procedure op_emit_mmx2(var ctx:t_jit_context2;const desc:t_op_desc);
var
 i:Integer;
 memop:t_memop_type2;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 new:t_jit_reg;

 procedure mem_out;
 begin
  with ctx.builder do
  begin
   //input:rax

   new:=new_reg(ctx.din.Operand[2]);
   _pmov(desc.mem_reg.op,desc.mem_reg.op8,new,[r_tmp0]);
  end;
 end;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:rax

   new:=new_reg(ctx.din.Operand[1]);
   _pmov(desc.reg_mem.op,desc.reg_mem.op8,new,[r_tmp0]);
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
       call(@uplift_jit); //in/out:rax

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
       call(@uplift_jit); //in/out:rax

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

      i:=GetFrameOffset(ctx.din.Operand[1]);
      _pmov(desc.mem_reg.op,desc.mem_reg.op8,new,[r_thrd+i]);
     end;
   mo_reg_ctx:
     begin
      new:=new_reg(ctx.din.Operand[1]);

      i:=GetFrameOffset(ctx.din.Operand[2]);
      _pmov(desc.reg_mem.op,desc.reg_mem.op8,new,[r_thrd+i]);
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

 new:t_jit_reg;

 procedure mem_out;
 begin
  with ctx.builder do
  begin
   //input:rax

   new:=new_reg(ctx.din.Operand[2]);
   _vmov(desc.mem_reg.op,desc.mem_reg.op8,new,[r_tmp0]);
  end;
 end;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:rax

   new:=new_reg(ctx.din.Operand[1]);
   _vmov(desc.reg_mem.op,desc.reg_mem.op8,new,[r_tmp0]);
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
       call(@uplift_jit); //in/out:rax

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
       call(@uplift_jit); //in/out:rax

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

      i:=GetFrameOffset(ctx.din.Operand[1]);
      _vmov(desc.mem_reg.op,desc.mem_reg.op8,new,[r_thrd+i]);
     end;
   mo_reg_ctx:
     begin
      new:=new_reg(ctx.din.Operand[1]);

      i:=GetFrameOffset(ctx.din.Operand[2]);
      _vmov(desc.reg_mem.op,desc.reg_mem.op8,new,[r_thrd+i]);
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
  mem_reg:(op:$31;op8:$30;index:0);
  reg_mem:(op:$33;op8:$32;index:0);
  reg_imm:(op:$81;op8:$80;index:6);
  reg_im8:(op:$83;op8:$83;index:6);
  mem__cl:(op:$00;op8:$00;index:255);
  mem_one:(op:$00;op8:$00;index:255);
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
  mem_reg:(op:$09;op8:$08;index:0);
  reg_mem:(op:$0B;op8:$0A;index:0);
  reg_imm:(op:$81;op8:$80;index:1);
  reg_im8:(op:$83;op8:$83;index:1);
  mem__cl:(op:$00;op8:$00;index:255);
  mem_one:(op:$00;op8:$00;index:255);
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
  mem_reg:(op:$21;op8:$20;index:0);
  reg_mem:(op:$33;op8:$32;index:0);
  reg_imm:(op:$81;op8:$80;index:4);
  reg_im8:(op:$83;op8:$83;index:4);
  mem__cl:(op:$00;op8:$00;index:255);
  mem_one:(op:$00;op8:$00;index:255);
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
  mem_reg:(op:$29;op8:$28;index:0);
  reg_mem:(op:$2B;op8:$2A;index:0);
  reg_imm:(op:$81;op8:$80;index:5);
  reg_im8:(op:$83;op8:$83;index:5);
  mem__cl:(op:$00;op8:$00;index:255);
  mem_one:(op:$00;op8:$00;index:255);
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
 add_desc:t_op_desc=(
  mem_reg:(op:$01;op8:$00;index:0);
  reg_mem:(op:$03;op8:$02;index:0);
  reg_imm:(op:$81;op8:$80;index:0);
  reg_im8:(op:$83;op8:$83;index:0);
  mem__cl:(op:$00;op8:$00;index:255);
  mem_one:(op:$00;op8:$00;index:255);
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

procedure op_imul(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  case ctx.din.OperCnt of
   1:Assert(false);
   2:op_emit_movz(ctx,$AF);
   3:Assert(false);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 mov_desc:t_op_desc=(
  mem_reg:(op:$89;op8:$88;index:0);
  reg_mem:(op:$8B;op8:$8A;index:0);
  reg_imm:(op:$C7;op8:$C6;index:0);
  reg_im8:(op:$00;op8:$00;index:255);
  mem__cl:(op:$00;op8:$00;index:255);
  mem_one:(op:$00;op8:$00;index:255);
  hint:[his_mov];
 );

procedure op_mov(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit2(ctx,mov_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 CMOV_32:array[OPSc_o..OPSc_nle] of Byte=(
  $40,$41,$42,$43,$44,$45,$46,$47,
  $48,$49,$4A,$4B,$4C,$4D,$4E,$4F
 );

procedure op_cmov(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_movz(ctx,CMOV_32[ctx.din.OpCode.Suffix]);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_movzx(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  case ctx.din.Operand[2].Size of
    os8:op_emit_movz(ctx,$B6);
   os16:op_emit_movz(ctx,$B7);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_movsx(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  case ctx.din.Operand[2].Size of
    os8:op_emit_movz(ctx,$BE);
   os16:op_emit_movz(ctx,$BF);
   else
    Assert(false);
  end;
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_movsxd(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit_movsxd(ctx,$63,$63);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 vmovups_desc:t_op_desc=(
  mem_reg:(op:$11;op8:$00;index:0);
  reg_mem:(op:$10;op8:$00;index:0);
  reg_imm:(op:$00;op8:$00;index:255);
  reg_im8:(op:$00;op8:$00;index:255);
  mem__cl:(op:$00;op8:$00;index:255);
  mem_one:(op:$00;op8:$00;index:255);
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
  mem_reg:(op:$29;op8:$00;index:0);
  reg_mem:(op:$28;op8:$00;index:0);
  reg_imm:(op:$00;op8:$00;index:255);
  reg_im8:(op:$00;op8:$00;index:255);
  mem__cl:(op:$00;op8:$00;index:255);
  mem_one:(op:$00;op8:$00;index:255);
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
 stack:t_jit_reg;
 imm:Int64;
begin
 with ctx.builder do
 begin
  stack:=r_tmp1;

  i:=GetFrameOffset(rsp.ARegValue[0]);
  movq(stack,[r_thrd+i]);
  leaq(stack,[stack+(-8)]);
  movq([r_thrd+i],stack);

  imm:=Int64(ctx.ptr_next);

  if (classif_offset_se64(imm)=3) then
  begin
   if used_r_tmp0 then
   begin
    push(r_tmp0);
   end;

   movi64(r_tmp0,imm);
   movq([stack],r_tmp0);

   if used_r_tmp0 then
   begin
    pop(r_tmp0);
   end;
  end else
  begin
   movi(os64,[stack],imm);
  end;

 end;
end;

procedure op_pop_rip(var ctx:t_jit_context2);
var
 i:Integer;
 new,stack:t_jit_reg;
begin
 with ctx.builder do
 begin
  new:=r_tmp0;
  stack:=r_tmp1;

  i:=GetFrameOffset(rsp.ARegValue[0]);
  movq(stack,[r_thrd+i]);

  movq(new,[stack]);

  leaq(stack,[stack+(+8)]);
  movq([r_thrd+i],stack);
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
 new,new2:t_jit_reg;
 i:Integer;
begin
 if (ctx.din.Operand[1].RegValue[0].AType=regNone) then
 begin
  ofs:=0;
  if not GetTargetOfs(ctx.din,ctx.ptr_curr,1,ofs) then
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
  new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  if is_rsp(ctx.din.Operand[1].RegValue) then
  begin
   build_lea(ctx,1,new);
   //
   op_push_rip(ctx,true);
  end else
  begin
   op_push_rip(ctx,false);
   //
   build_lea(ctx,1,new);
  end;
  //
  op_jmp_dispatcher(ctx);
 end else
 if is_preserved(ctx.din) then
 begin
  new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  op_push_rip(ctx,false);
  //
  i:=GetFrameOffset(ctx.din.Operand[1].RegValue[0]);
  ctx.builder.movq(new,[r_thrd+i]);
  //
  op_jmp_dispatcher(ctx);
 end else
 begin
  op_push_rip(ctx,false);
  //
  new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  new2:=new_reg(ctx.din.Operand[1]);
  //
  ctx.builder.movq(new,new2);
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
 new,new2:t_jit_reg;
 i:Integer;
begin
 if (ctx.din.Operand[1].RegValue[0].AType=regNone) then
 begin
  ofs:=0;
  if not GetTargetOfs(ctx.din,ctx.ptr_curr,1,ofs) then
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
  new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  build_lea(ctx,1,new);
  //
  op_jmp_dispatcher(ctx);
 end else
 if is_preserved(ctx.din) then
 begin
  new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  //
  i:=GetFrameOffset(ctx.din.Operand[1].RegValue[0]);
  ctx.builder.movq(new,[r_thrd+i]);
  //
  op_jmp_dispatcher(ctx);
 end else
 begin
  new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
  new2:=new_reg(ctx.din.Operand[1]);
  //
  ctx.builder.movq(new,new2);
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
 if not GetTargetOfs(ctx.din,ctx.ptr_curr,1,ofs) then
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
 test_desc:t_op_desc=(
  mem_reg:(op:$85;op8:$84;index:0);
  reg_mem:(op:$00;op8:$00;index:0);
  reg_imm:(op:$F7;op8:$F6;index:0);
  reg_im8:(op:$00;op8:$00;index:0);
  mem__cl:(op:$00;op8:$00;index:255);
  mem_one:(op:$00;op8:$00;index:255);
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
  mem_reg:(op:$39;op8:$38;index:0);
  reg_mem:(op:$3B;op8:$3A;index:0);
  reg_imm:(op:$81;op8:$80;index:7);
  reg_im8:(op:$83;op8:$83;index:7);
  mem__cl:(op:$00;op8:$00;index:255);
  mem_one:(op:$00;op8:$00;index:255);
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
 shl_desc:t_op_desc=(
  mem_reg:(op:$00;op8:$00;index:255);
  reg_mem:(op:$00;op8:$00;index:255);
  reg_imm:(op:$00;op8:$00;index:255);
  reg_im8:(op:$C1;op8:$C0;index:4);
  mem__cl:(op:$D3;op8:$D2;index:4);
  mem_one:(op:$D1;op8:$D0;index:4);
  hint:[];
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
 shr_desc:t_op_desc=(
  mem_reg:(op:$00;op8:$00;index:255);
  reg_mem:(op:$00;op8:$00;index:255);
  reg_imm:(op:$00;op8:$00;index:255);
  reg_im8:(op:$C1;op8:$C0;index:5);
  mem__cl:(op:$D3;op8:$D2;index:5);
  mem_one:(op:$D1;op8:$D0;index:5);
  hint:[];
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
 sar_desc:t_op_desc=(
  mem_reg:(op:$00;op8:$00;index:255);
  reg_mem:(op:$00;op8:$00;index:255);
  reg_imm:(op:$00;op8:$00;index:255);
  reg_im8:(op:$C1;op8:$C0;index:7);
  mem__cl:(op:$D3;op8:$D2;index:7);
  mem_one:(op:$D1;op8:$D0;index:7);
  hint:[];
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
 inc_desc:t_op_type=(
  op:$FF;op8:$FE;index:0
 );

procedure op_inc(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit1(ctx,inc_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 dec_desc:t_op_type=(
  op:$FF;op8:$FE;index:1
 );

procedure op_dec(var ctx:t_jit_context2);
begin
 if is_preserved(ctx.din) or is_memory(ctx.din) then
 begin
  op_emit1(ctx,dec_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_lea(var ctx:t_jit_context2);
var
 new:t_jit_reg;
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
 stack,new:t_jit_reg;
begin
 with ctx.builder do
  if is_memory(ctx.din) then
  begin
   Assert(false);
  end else
  begin
   stack:=r_tmp0;

   if is_preserved(ctx.din) then
   begin
    new:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

    i:=GetFrameOffset(rsp.ARegValue[0]);
    movq(stack,[r_thrd+i]);
    leaq(stack,[stack+(-OPERAND_BYTES[new.ARegValue[0].ASize])]);
    movq([r_thrd+i],stack);

    i:=GetFrameOffset(ctx.din.Operand[1]);
    movq(new,[r_thrd+i]);

    movq([stack],new);
   end else
   begin
    new:=new_reg(ctx.din.Operand[1]);

    i:=GetFrameOffset(rsp.ARegValue[0]);
    movq(stack,[r_thrd+i]);
    leaq(stack,[stack+(-OPERAND_BYTES[new.ARegValue[0].ASize])]);
    movq([r_thrd+i],stack);

    movq([stack],new);
   end;

  end;
end;

procedure op_pop(var ctx:t_jit_context2);
var
 i:Integer;
 stack,new:t_jit_reg;
begin
 with ctx.builder do
  if is_memory(ctx.din) then
  begin
   Assert(false);
  end else
  begin
   stack:=r_tmp0;

   i:=GetFrameOffset(rsp.ARegValue[0]);
   movq(stack,[r_thrd+i]);

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

   leaq(stack,[stack+OPERAND_BYTES[new.ARegValue[0].ASize]]);
   movq([r_thrd+i],stack);
  end;
end;

procedure op_int(var ctx:t_jit_context2);
var
 i:Integer;
 id:Byte;
begin
 i:=ctx.din.Operand[1].ByteCount;
 Assert(i=1);
 id:=PByte(ctx.ptr_curr)[i];

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

procedure op_nop(var ctx:t_jit_context2);
begin
 //align?
end;

const
 pxor_desc:t_op_desc=(
  mem_reg:(op:$0F;op8:$EF;index:0);
  reg_mem:(op:$00;op8:$00;index:255);
  reg_imm:(op:$00;op8:$00;index:255);
  reg_im8:(op:$00;op8:$00;index:255);
  mem__cl:(op:$00;op8:$00;index:255);
  mem_one:(op:$00;op8:$00;index:255);
  hint:[his_xor];
 );

procedure op_pxor(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_mmx2(ctx,pxor_desc);
 end else
 begin
  add_orig(ctx);
 end;
end;

procedure op_vxorps(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  Assert(False);
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fnstcw_desc:t_op_type=(
  op:$D9;op8:$D9;index:7
 );

procedure op_fnstcw(var ctx:t_jit_context2);
var
 Size:TOperandSize;
begin
 if is_memory(ctx.din) then
 begin
  Size:=ctx.din.Operand[1].Size;
  ctx.din.Operand[1].Size:=os0; //no size prefix
  op_emit1(ctx,fnstcw_desc);
  ctx.din.Operand[1].Size:=Size;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 fldcw_desc:t_op_type=(
  op:$D9;op8:$D9;index:5
 );

procedure op_fldcw(var ctx:t_jit_context2);
var
 Size:TOperandSize;
begin
 if is_memory(ctx.din) then
 begin
  Size:=ctx.din.Operand[1].Size;
  ctx.din.Operand[1].Size:=os0; //no size prefix
  op_emit1(ctx,fldcw_desc);
  ctx.din.Operand[1].Size:=Size;
 end else
 begin
  add_orig(ctx);
 end;
end;

const
 dec_fxrstor:t_op_type=(
  op:$AE;op8:$AE;index:1
 );

procedure op_fxrstor(var ctx:t_jit_context2);
begin
 if is_memory(ctx.din) then
 begin
  op_emit_movzm(ctx,dec_fxrstor);
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

 jit_cbs[OPxor ,OPSnone]:=@op_xor;
 jit_cbs[OPor  ,OPSnone]:=@op_or;
 jit_cbs[OPand ,OPSnone]:=@op_and;
 jit_cbs[OPsub ,OPSnone]:=@op_sub;
 jit_cbs[OPadd ,OPSnone]:=@op_add;

 jit_cbs[OPimul,OPSnone]:=@op_imul;

 jit_cbs[OPmov ,OPSnone]:=@op_mov;
 jit_cbs[OPmovu,OPSx_ps]:=@op_vmovups;
 jit_cbs[OPmova,OPSx_ps]:=@op_vmovaps;

 jit_cbs[OPcmov__,OPSc_o  ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_no ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_b  ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_nb ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_z  ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_nz ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_be ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_nbe]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_s  ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_ns ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_p  ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_np ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_l  ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_nl ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_le ]:=@op_cmov;
 jit_cbs[OPcmov__,OPSc_nle]:=@op_cmov;

 jit_cbs[OPmovzx,OPSnone]:=@op_movzx;
 jit_cbs[OPmovsx,OPSnone]:=@op_movsx;
 jit_cbs[OPmovsx,OPSx_d ]:=@op_movsxd;

 jit_cbs[OPcall,OPSnone]:=@op_call;
 jit_cbs[OPjmp ,OPSnone]:=@op_jmp;
 jit_cbs[OPret ,OPSnone]:=@op_ret;

 jit_cbs[OPtest,OPSnone]:=@op_test;
 jit_cbs[OPcmp ,OPSnone]:=@op_cmp;

 jit_cbs[OPshl ,OPSnone]:=@op_shl;
 jit_cbs[OPshr ,OPSnone]:=@op_shr;
 jit_cbs[OPsar ,OPSnone]:=@op_sar;

 jit_cbs[OPj__,OPSc_o  ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_no ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_b  ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_nb ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_z  ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_nz ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_be ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_nbe]:=@op_jcc;
 jit_cbs[OPj__,OPSc_s  ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_ns ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_p  ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_np ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_l  ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_nl ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_le ]:=@op_jcc;
 jit_cbs[OPj__,OPSc_nle]:=@op_jcc;

 jit_cbs[OPpush,OPSnone]:=@op_push;
 jit_cbs[OPpop ,OPSnone]:=@op_pop;

 jit_cbs[OPpxor,OPSnone]:=@op_pxor;

 jit_cbs[OPemms,OPSnone]:=@add_orig;
 jit_cbs[OPvzeroall,OPSnone]:=@add_orig;
 jit_cbs[OPfninit,OPSnone]:=@add_orig;

 jit_cbs[OPxor,OPSx_ps]:=@op_vxorps;

 jit_cbs[OPlea,OPSnone]:=@op_lea;
 jit_cbs[OPint,OPSnone]:=@op_int;
 jit_cbs[OPud2,OPSnone]:=@op_ud2;

 jit_cbs[OPnop,OPSnone]:=@op_nop;

 jit_cbs[OPinc,OPSnone]:=@op_inc;
 jit_cbs[OPdec,OPSnone]:=@op_dec;

 jit_cbs[OPfnstcw,OPSnone]:=@op_fnstcw;
 jit_cbs[OPfldcw ,OPSnone]:=@op_fldcw;

 jit_cbs[OPfxrstor,OPSnone]:=@op_fxrstor;

 proc:=TDbgProcess.Create(dm64);
 adec:=TX86AsmDecoder.Create(proc);

 while (ptr<fin) do
 begin

  adec.Disassemble(ptr,ACodeBytes,ACode);

  Writeln('original------------------------':32,' ','0x',HexStr(ptr));
  Writeln(ACodeBytes:32,' ',ACode);
  Writeln('original------------------------':32,' ','');

  ctx.ptr_next:=ptr;
  ctx.ptr_curr:=ptr-adec.Disassembler.CodeIdx;

  ctx.dis:=adec.Disassembler;
  ctx.din:=adec.Instr;

  cb:=jit_cbs[adec.Instr.OpCode.Opcode,adec.Instr.OpCode.Suffix];

  if (cb=nil) then
  begin
   Writeln('Unhandled jit:',
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

