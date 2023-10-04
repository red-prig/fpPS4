unit kern_jit2_ctx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 g_node_splay,
 x86_fpdbgdisas,
 x86_jit;

{
change: rsp,rbp,rip
rax?

eflahs? temp change?

change: push/pop

thread: r15

}

type
 p_jit_frame=^jit_frame;
 jit_frame=packed record
  tf_rax:QWORD;
  tf_rsp:QWORD;
  tf_rbp:QWORD;
  tf_r14:QWORD;
  tf_r15:QWORD;
  tf_rip:QWORD;
 end;

 p_jit_context2=^t_jit_context2;
 t_jit_context2=object
  type
   p_forward_link=^t_forward_link;
   t_forward_link=object
    next    :p_forward_link;
    label_id:t_jit_i_link;
   end;

   t_forward_links=object
    root:p_forward_link;
    procedure Resolve(_label:t_jit_i_link);
   end;

   p_forward_point=^t_forward_point;
   t_forward_point=object
    pLeft :p_forward_point;
    pRight:p_forward_point;
    dst   :Pointer;
    links :t_forward_links;
    function c(n1,n2:p_forward_point):Integer; static;
   end;
   t_forward_set=specialize TNodeSplay<t_forward_point>;

   p_label=^t_label;
   t_label=object
    pLeft     :p_label;
    pRight    :p_label;
    curr      :Pointer;
    next      :Pointer;
    link_curr:t_jit_i_link;
    link_next:t_jit_i_link;
    function c(n1,n2:p_label):Integer; static;
   end;
   t_label_set=specialize TNodeSplay<t_label>;

   p_entry_point=^t_entry_point;
   t_entry_point=object
    next    :p_entry_point;
    src     :Pointer;
    label_id:t_jit_i_link;
   end;

  var
   forward_set:t_forward_set;
   label_set  :t_label_set;
   entry_list :p_entry_point;

   text_start:QWORD;
   text___end:QWORD;
   map____end:QWORD;

   max:QWORD;

   Code    :Pointer;
   ptr_curr:Pointer;
   ptr_next:Pointer;

   trim:Boolean;

   dis:TX86Disassembler;
   din:TInstruction;

   builder:t_jit_builder;

  function  is_text_addr(addr:QWORD):Boolean;
  function  is_map_addr(addr:QWORD):Boolean;
  procedure add_forward_link(node:p_forward_point;label_id:t_jit_i_link);
  function  add_forward_point(label_id:t_jit_i_link;dst:Pointer):p_forward_point;
  function  add_forward_point(dst:Pointer):p_forward_point;
  function  max_forward_point():Pointer;
  function  fetch_forward_point(var links:t_forward_links;var dst:Pointer):Boolean;
  function  add_label(curr,next:Pointer;link_curr,link_next:t_jit_i_link):p_label;
  function  get_label(src:Pointer):p_label;
  function  get_link(src:Pointer):t_jit_i_link;
  procedure add_entry_point(src:Pointer;label_id:t_jit_i_link);
  procedure Free;
 end;

const
 r_thrd:TRegValue=(AType:regGeneral;ASize:os64;AIndex:15); //r15
 r_tmp0:TRegValue=(AType:regGeneral;ASize:os64;AIndex: 0); //rax
 r_tmp1:TRegValue=(AType:regGeneral;ASize:os64;AIndex:14); //r14

 OPERAND_BYTES:array[TOperandSize] of Word=(0,1,2,4,8,6,10,16,32,64,512);

function GetFrameOffset(const RegValue:TRegValue):Integer;
function GetFrameOffset(const r:TOperand):Integer;
function GetTargetOfs(var din:TInstruction;Code:PByte;id:Byte;var ofs:Int64):Boolean;
function is_preserved(const r:TRegValue):Boolean;
function is_preserved(const r:TOperand):Boolean;
function is_preserved(const r:TInstruction):Boolean;
function is_memory(const r:TOperand):Boolean;
function is_memory(const r:TInstruction):Boolean;
function is_xmm(const r:TOperand):Boolean;
function is_xmm(const r:TInstruction):Boolean;
function is_rsp(const r:TRegValue):Boolean;
function is_rsp(const r:TRegValues):Boolean;
function is_invalid(const r:TInstruction):Boolean;

//in/out:rax uses:r14
procedure uplift_jit; assembler;

//in:rax(addr),r14b:(mem_size) out:ZF
procedure page_test; assembler;

//in:rax(addr),r14b:(size)
procedure copyout_mov; assembler;

//in:rax(addr),r14b:(size) out:rax
procedure copyin_mov; assembler;

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

 t_op_hint=Set of (his_mov,
                   his_xor,
                   his_xchg,
                   his_rax,
                   his_ro, //read only
                   his_wo, //write only
                   his_rw, //read-write
                   his_align);

 t_op_desc=packed record
  mem_reg:t_op_type; //reg_reg
  reg_mem:t_op_type; //reg_reg
  reg_imm:t_op_type; //mem_imm
  reg_im8:t_op_type; //mem_im8
  hint:t_op_hint;
 end;

 t_op_shift=packed record
  reg_im8:t_op_type; //mem_im8
  mem__cl:t_op_type; //reg__cl
  mem_one:t_op_type; //reg_one
 end;

 t_op_avx3_imm=packed record
  rmi:t_op_type;
  mri:t_op_type;
 end;

 t_lea_hint=Set Of (not_use_segment,
                    not_use_r_tmp0,
                    not_use_r_tmp1,
                    inc8_rsp,
                    code_ref);

procedure build_lea(var ctx:t_jit_context2;id:Byte;
                    reg:TRegValue;hint:t_lea_hint=[]);

function  cmp_reg(const r1,r2:TRegValue):Boolean;
function  new_reg(const Operand:TOperand):TRegValue;
function  new_reg_size(const r:TRegValue;ASize:TOperandSize):TRegValue;
function  new_reg_size(const r:TRegValue;const RegValue:TRegValues):TRegValue;
function  new_reg_size(const r:TRegValue;const Operand:TOperand):TRegValue;
function  fix_size(const r:TRegValue):TRegValue;
function  is_rep_prefix(const i:TInstruction):Boolean;
function  flags(const i:TInstruction):t_jit_reg;
function  flags(const ctx:t_jit_context2):t_jit_reg;

procedure add_orig(var ctx:t_jit_context2);
procedure op_load_rax(var ctx:t_jit_context2;reg:TRegValue);
procedure op_save_rax(var ctx:t_jit_context2;reg:TRegValue);
procedure op_emit1(var ctx:t_jit_context2;const desc:t_op_type;hint:t_op_hint);
procedure op_emit2(var ctx:t_jit_context2;const desc:t_op_desc);
procedure op_emit_shift2(var ctx:t_jit_context2;const desc:t_op_shift);
procedure op_emit_shift3(var ctx:t_jit_context2;const desc:t_op_shift);
procedure op_emit_avx2_rr(var ctx:t_jit_context2;const desc:t_op_type);
procedure op_emit_avx2(var ctx:t_jit_context2;const desc:t_op_desc);
procedure op_emit_avx3(var ctx:t_jit_context2;const desc:t_op_type);
procedure op_emit_avx3_imm8(var ctx:t_jit_context2;const desc:t_op_avx3_imm);
procedure op_emit_avx_F3(var ctx:t_jit_context2;const desc:t_op_type);
procedure op_emit_avx4(var ctx:t_jit_context2;const desc:t_op_type);
procedure op_emit_bmi_rmr(var ctx:t_jit_context2;const desc:t_op_type);
procedure op_emit_bmi_rrm(var ctx:t_jit_context2;const desc:t_op_type);

procedure print_disassemble(addr:Pointer;vsize:Integer);

implementation

uses
 vmparam,
 vm_pmap,
 systm,
 kern_thr;

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


function t_jit_context2.t_forward_point.c(n1,n2:p_forward_point):Integer;
begin
 Result:=Integer(n1^.dst>n2^.dst)-Integer(n1^.dst<n2^.dst);
end;

function t_jit_context2.t_label.c(n1,n2:p_label):Integer;
begin
 Result:=Integer(n1^.curr>n2^.curr)-Integer(n1^.curr<n2^.curr);
end;

function t_jit_context2.is_text_addr(addr:QWORD):Boolean;
begin
 Result:=(addr>=text_start) and (addr<text___end);
end;

function t_jit_context2.is_map_addr(addr:QWORD):Boolean;
begin
 Result:=(addr>=text_start) and (addr<map____end);
end;

procedure t_jit_context2.add_forward_link(node:p_forward_point;label_id:t_jit_i_link);
var
 link:p_forward_link;
begin
 if (node=nil) or (label_id=nil_link) then Exit;
 link:=builder.Alloc(Sizeof(t_forward_link));
 link^.label_id:=label_id;
 link^.next:=node^.links.root;
 node^.links.root:=link;
end;

function t_jit_context2.add_forward_point(label_id:t_jit_i_link;dst:Pointer):p_forward_point;
var
 node:t_forward_point;
begin
 if (dst=nil) then Exit;

 node.dst:=dst;
 Result:=forward_set.Find(@node);
 if (Result=nil) then
 begin
  Result:=builder.Alloc(Sizeof(t_forward_point));
  Result^.dst:=dst;
  forward_set.Insert(Result);
 end;
 add_forward_link(Result,label_id);
end;

function t_jit_context2.add_forward_point(dst:Pointer):p_forward_point;
begin
 Result:=add_forward_point(nil_link,dst);
end;

function t_jit_context2.max_forward_point():Pointer;
var
 entry:p_forward_point;
begin
 Result:=nil;
 entry:=forward_set.Max;
 if (entry=nil) then Exit;
 Result:=entry^.dst;
end;

function t_jit_context2.fetch_forward_point(var links:t_forward_links;var dst:Pointer):Boolean;
var
 min:p_forward_point;
begin
 Result:=False;
 min:=forward_set.Min;
 if (min=nil) then Exit;
 forward_set._Splay(min);
 min:=forward_set.pRoot;
 forward_set.Delete(min);
 dst  :=min^.dst;
 links:=min^.links;
 Result:=True;
end;

procedure t_jit_context2.t_forward_links.Resolve(_label:t_jit_i_link);
var
 node:p_forward_link;
begin
 node:=root;
 While (node<>nil) do
 begin
  node^.label_id._label:=_label;
  node:=node^.next;
 end;
end;

function t_jit_context2.add_label(curr,next:Pointer;link_curr,link_next:t_jit_i_link):p_label;
var
 node:t_label;
begin
 if (curr=nil) then Exit;
 node.curr:=curr;
 Result:=label_set.Find(@node);
 if (Result<>nil) then Exit;
 Result:=builder.Alloc(Sizeof(t_label));
 //
 Result^.curr     :=curr;
 Result^.next     :=next;
 Result^.link_curr:=link_curr;
 Result^.link_next:=link_next;
 //
 label_set.Insert(Result);
end;

function t_jit_context2.get_label(src:Pointer):p_label;
var
 node:t_label;
begin
 Result:=nil;
 node.curr:=src;
 Result:=label_set.Find(@node);
end;

function t_jit_context2.get_link(src:Pointer):t_jit_i_link;
var
 node:p_label;
begin
 Result:=nil_link;
 node:=get_label(src);
 if (node=nil) then Exit;
 Result:=node^.link_curr;
end;

procedure t_jit_context2.add_entry_point(src:Pointer;label_id:t_jit_i_link);
var
 node:p_entry_point;
begin
 if (src=nil) then Exit;
 node:=builder.Alloc(Sizeof(t_entry_point));
 node^.next    :=entry_list;
 node^.src     :=src;
 node^.label_id:=label_id;
 //
 entry_list:=node;
end;

procedure t_jit_context2.Free;
begin
 builder.Free;
 Self:=Default(t_jit_context2);
end;

//

function GetFrameOffset(const RegValue:TRegValue):Integer;
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

function is_xmm(const r:TOperand):Boolean; inline;
begin
 Result:=r.RegValue[0].AType=regXmm;
end;

function is_xmm(const r:TInstruction):Boolean;
var
 i:Integer;
begin
 Result:=False;
 if (r.OperCnt<>0) then
 For i:=1 to r.OperCnt do
 begin
  Result:=is_xmm(r.Operand[i]);
  if Result then Exit;
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

function cmp_reg(const r1,r2:TRegValue):Boolean; inline;
begin
 Result:=(r1.AType =r2.AType) and
         (r1.ASize =r2.ASize) and
         (r1.AIndex=r2.AIndex);
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

function is_rep_prefix(const i:TInstruction):Boolean;
begin
 Result:=[ifPrefixRep,ifPrefixRepE,ifPrefixRepNe]*i.Flags<>[];
end;

function flags(const i:TInstruction):t_jit_reg;
begin
 Result:=Default(t_jit_reg);

 if (ifPrefixLock in i.Flags) then
 begin
  Result:=Result+t_jit_builder.LOCK;
 end;
end;

function flags(const ctx:t_jit_context2):t_jit_reg; inline;
begin
 Result:=flags(ctx.din);
end;

procedure add_orig(var ctx:t_jit_context2);
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 Move(ctx.code^,ji.AData,ctx.dis.CodeIdx);

 ji.ASize:=ctx.dis.CodeIdx;

 ctx.builder._add(ji);
end;

function is_invalid(const r:TOperand):Boolean; inline;
begin
 Result:=(r.RegValue[0].AType=regInvalid) or
         (r.RegValue[1].AType=regInvalid);
end;

function is_invalid(const r:TInstruction):Boolean;
var
 i:Integer;
begin
 Result:=False;
 if (r.OperCnt<>0) then
 For i:=1 to r.OperCnt do
 begin
  Result:=is_invalid(r.Operand[i]);
  if Result then Exit;
 end;
end;

procedure sigsegv(addr:Pointer);
begin
 Writeln('sigsegv:0x',HexStr(addr));
 Assert(False);
end;

//in/out:rax uses:r14
procedure uplift_jit; assembler; nostackframe;
label
 _exit;
asm
 pushfq
 push %r14
 push %rax
 //
 //low addr (r14)
 mov %rax,%r14
 and PAGE_MASK,%r14
 //high addr (rax)
 shr PAGE_SHIFT   ,%rax
 and PAGE_MAP_MASK,%rax
 //uplift (rax)
 lea (,%rax,4),%rax
 add PAGE_MAP(%rip),%rax
 mov (%rax),%eax
 //filter (rax)
 and PAGE_OFS_MASK,%rax
 jz _exit
 //combine (rax|r14)
 shl PAGE_SHIFT,%rax
 or  %r14,%rax
 //
 pop %r14
 pop %r14
 popfq
 ret

 _exit:
 pop %rdi

 pop %r14
 popfq

 call sigsegv

 ret
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
 add PAGE_MAP(%rip),%rdi
 add PAGE_MAP(%rip),%rsi
 //
 mov (%rdi),%edi
 mov (%rsi),%esi
 //filter (rdi,rsi)
 and PAGE_OFS_MASK,%rdi
 and PAGE_OFS_MASK,%rsi
 //
 inc %rdi
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
 addr:Pointer;
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

  movq %rax,addr

  lea  data,%rax    //vaddr:=data

  mov  8(%rbp),%rdi //ret addr
  lea  2(%rdi),%rdi //jmp near

  call %rdi         //reg->data

  movq    addr,%rsi //vaddr
  lea     data,%rdi //data
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

  jmp _exit
 _simple:

  call uplift_jit

  mov  8(%rbp),%r14 //ret addr
  lea  2(%r14),%r14 //jmp near

  popfq //restore flags before call

  call %r14         //reg->data

 _exit:
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
  lea     data,%rsi //data
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

  lea data,%rax //vaddr:=data

  jmp _exit
 _simple:

  call uplift_jit

 _exit:
 //
 popfq
end;

procedure add_rip_entry(var ctx:t_jit_context2;ofs:Int64;hint:t_lea_hint);
begin
 if (ctx.max<>0) then
 if ctx.is_text_addr(ofs) then
 begin
  if (ofs<=ctx.max) then
  if ((pmap_get_raw(QWORD(ofs)) and PAGE_PROT_EXECUTE)<>0) then
  begin
   ctx.add_forward_point(Pointer(ofs));
  end;
 end;

 if (code_ref in hint) then
 if ctx.is_map_addr(ofs) then
 if ((pmap_get_raw(QWORD(ofs)) and PAGE_PROT_READ)<>0) then
 begin
  ofs:=PInt64(ofs)^;

  if ctx.is_text_addr(ofs) then
  if (ctx.max=0) or (ofs<=ctx.max) then
  if ((pmap_get_raw(QWORD(ofs)) and PAGE_PROT_EXECUTE)<>0) then
  begin
   ctx.add_forward_point(Pointer(ofs));
  end;
 end;

end;

function is_segment(const i:TInstruction):Boolean;
begin
 Result:=False;

 case i.SegmentReg of
  4:Result:=True;
  5:Result:=True;
  else;
 end;
end;

function get_segment(const i:TInstruction):Integer;
begin
 Result:=0;

 case i.SegmentReg of
  4:Result:=teb_tcb;
  5:Result:=teb_gsbase;
  else
    Assert(False);
 end;
end;

procedure optimal_swap(var RegValue:TRegValues);
var
 t:TRegValue;
begin
 if (RegValue[0].AType<>regNone) and
    (RegValue[1].AType<>regNone) and
    (not is_preserved(RegValue[0])) and
    (is_preserved(RegValue[1])) and
    (RegValue[0].AScale<=1) then
 begin
  //optimal swap
  t:=RegValue[0];
  RegValue[0]:=RegValue[1];
  RegValue[1]:=t;
 end;
end;

function lea_reg_is_used(var RegValue:TRegValues;reg:TRegValue):Boolean;
begin
 Result:=(RegValue[1].AType<>regNone) and
         (RegValue[1].AIndex=reg.AIndex);
end;

procedure build_lea(var ctx:t_jit_context2;id:Byte;
                    reg:TRegValue;hint:t_lea_hint=[]);
var
 RegValue:TRegValues;
 adr,adrr:TRegValue;
 new1,new2:TRegValue;
 ofs:Int64;
 i:Integer;
 AScale:Byte;
 save_r_tmp0:Boolean;
begin
 RegValue:=ctx.din.Operand[id].RegValue;

 adr:=new_reg_size(reg,RegValue);

 if (adr.ASize=os0) then
 begin
  adr.ASize:=os64;
 end;

 with ctx.builder do
 begin
  if (not (not_use_segment in hint)) and
     is_segment(ctx.din) then
  begin

   if (RegValue[0].AType=regNone) then //absolute offset
   begin
    ofs:=0;
    GetTargetOfs(ctx.din,ctx.code,id,ofs);

    movq(adr,[GS+get_segment(ctx.din)]);
    leaq(adr,[adr+ofs]);
   end else
   begin
    Assert(false,'TODO');
   end;

  end else
  if (RegValue[0].AType=regNone) then //absolute offset
  begin
   ofs:=0;
   GetTargetOfs(ctx.din,ctx.code,id,ofs);

   //sign extend
   movi(adr,ofs);
  end else
  if is_rip(RegValue[0]) then //rip relative
  begin
   ofs:=0;
   GetTargetOfs(ctx.din,ctx.code,id,ofs);
   ofs:=Int64(ctx.ptr_next)+ofs;

   add_rip_entry(ctx,ofs,hint);

   if (classif_offset_u64(ofs)=os64) then
   begin
    movi64(adr,ofs);
   end else
   begin
    //mov eax,imm32   this is zero extend to 64bit
    movi(new_reg_size(adr,os32),ofs);
   end;
  end else
  if is_preserved(RegValue) then
  begin

   optimal_swap(RegValue);

   save_r_tmp0:=False;
   adrr:=adr;
   if lea_reg_is_used(RegValue,adr) then
   begin
    adr:=new_reg_size(r_tmp0,adr.ASize);
    save_r_tmp0:=(not_use_r_tmp0 in hint);
   end;

   if save_r_tmp0 then
   begin
    push(r_tmp0);
   end;

   AScale:=RegValue[0].AScale;

   ofs:=0;
   GetTargetOfs(ctx.din,ctx.code,id,ofs);

   //1
   if is_preserved(RegValue[0]) then
   begin
    i:=GetFrameOffset(RegValue[0]);
    movq(adr,[r_thrd+i]);

    if (inc8_rsp in hint) and is_rsp(RegValue[0]) then
    begin

     if (AScale<=1) and
        (classif_offset_64(ofs+8)<>os64) then
     begin
      ofs:=ofs+8;
     end else
     begin
      leaq(adr,[adr+8]);
     end;

    end;

    if (AScale>1) or (ofs<>0) then
    begin
     leaq(adr,[adr*AScale+ofs]);
    end;
   end else
   begin
    new1:=RegValue[0];
    //
    //AScale in new1
    leaq(adr,[new1+ofs]);
   end;
   //1

   //2
   if (RegValue[1].AType<>regNone) then
   begin

    if is_preserved(RegValue[1]) then
    begin
     i:=GetFrameOffset(RegValue[1]);

     if (not_use_r_tmp1 in hint) then
     begin
      push(r_tmp1);
     end;

     new2:=new_reg_size(r_tmp1,adr.ASize);

     movq(new2,[r_thrd+i]);

     if (inc8_rsp in hint) and is_rsp(RegValue[1]) then
     begin
      leaq(adrr,[adr+new2+8]);
     end else
     begin
      leaq(adrr,[adr+new2]);
     end;

     if (not_use_r_tmp1 in hint) then
     begin
      pop(r_tmp1);
     end;

    end else
    begin
     new1:=RegValue[1];
     leaq(adrr,[adr+new1]);
    end;

   end;
   //2

   if save_r_tmp0 then
   begin
    pop(r_tmp0);
   end;

   //is_preserved
  end else
  begin
   //direct
   ofs:=0;
   GetTargetOfs(ctx.din,ctx.code,id,ofs);

   new1:=RegValue[0];

   if (RegValue[1].AType<>regNone) then
   begin
    new2:=RegValue[1];
    //
    //AScale in new1
    leaq(adr,[new1+new2+ofs]);
   end else
   begin
    //AScale in new1
    leaq(adr,[new1+ofs]);
   end;

  end;

 end;

end;

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

function classif_shift3(var din:TInstruction):t_memop_shift;
begin
 if (ofMemory in din.Operand[1].Flags) then
 begin
  if (din.Operand[3].ByteCount<>0) then
  begin
   Assert(din.Operand[3].Size=os8);
   Result:=mo_mem_imm8;
  end else
  if is_cl(din.Operand[3].RegValue[0]) then
  begin
   Result:=mo_mem_cl;
  end else
  begin
   Assert(false);
  end;
 end else
 if is_preserved(din.Operand[1]) then
 begin
  if (din.Operand[3].ByteCount<>0) then
  begin
   Assert(din.Operand[3].Size=os8);
   Result:=mo_ctx_imm8;
  end else
  if is_cl(din.Operand[3].RegValue[0]) then
  begin
   Result:=mo_ctx_cl;
  end else
  begin
   Assert(false);
  end;
 end else
 if (din.Operand[3].ByteCount<>0) then
 begin
  Assert(din.Operand[3].Size=os8);
  Result:=mo_reg_imm8;
 end else
 if is_cl(din.Operand[3].RegValue[0]) then
 begin
  Result:=mo_reg_cl;
 end else
 begin
  Assert(false);
 end;
end;

procedure op_load_rax(var ctx:t_jit_context2;reg:TRegValue);
var
 i:Integer;
begin
 with ctx.builder do
 begin
  i:=GetFrameOffset(rax);
  movq(reg,[r_thrd+i]);
 end;
end;

procedure op_save_rax(var ctx:t_jit_context2;reg:TRegValue);
var
 i:Integer;
begin
 with ctx.builder do
 begin
  i:=GetFrameOffset(rax);
  movq([r_thrd+i],reg);
 end;
end;

procedure op_emit1(var ctx:t_jit_context2;const desc:t_op_type;hint:t_op_hint);
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

 procedure mem_in_rax;
 begin
  with ctx.builder do
  begin
   //input:rax

   movq(r_tmp1,r_tmp0);

   op_load_rax(ctx,rax);

   _M(desc,mem_size,[flags(ctx)+r_tmp1]);

   op_save_rax(ctx,rax);
  end;
 end;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:rax

   _M(desc,mem_size,[flags(ctx)+r_tmp0]);
  end;
 end;

begin
 memop:=classif_memop1(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem:
    begin

     if (his_rax in hint) then
     begin
      //RAX:=RAX X [mem]

      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;

      if (mem_size=os8) or
         (his_rw in hint) then
      begin
       call_far(@uplift_jit); //in/out:rax uses:r14

       mem_in_rax;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call_far(@copyin_mov); //in:rax(addr),r14:(size) out:rax

       mem_in_rax;
      end;

     end else
     if (his_ro in hint) then
     begin
      //DATA:=[mem]

      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;

      if (mem_size=os8) or
         (his_rw in hint) then
      begin
       call_far(@uplift_jit); //in/out:rax uses:r14

       mem_in;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call_far(@copyin_mov); //in:rax(addr),r14:(size) out:rax

       mem_in;
      end;

     end else
     begin
      //[mem]:=DATA

      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;

      if (mem_size=os8) or
         (his_rw in hint) then
      begin

       call_far(@uplift_jit); //in/out:rax uses:r14

       mem_out;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call_far(@copyout_mov); //in:rax(addr),r14:(size)

       link_next:=jmp(nil_link,os8);

       mem_out;

       reta;

       link_next._label:=get_curr_label.after;
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

     if (his_rax in hint) then
     begin
      op_load_rax(ctx,rax);
     end;

     _M(desc,mem_size,[r_thrd+i]);

     if (his_rax in hint) then
     begin
      op_save_rax(ctx,rax);
     end;
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

       if (his_rax in desc.hint) then
       begin
        new2:=new_reg_size(rax,ctx.din.Operand[2]);

        movq(r_tmp1,r_tmp0);

        op_load_rax(ctx,fix_size(new2));

        _RM(desc.mem_reg,new1,[flags(ctx)+r_tmp1]);

        op_save_rax(ctx,fix_size(new2));
       end else
       begin
        _RM(desc.mem_reg,new1,[flags(ctx)+r_tmp0]);
       end;

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

       if (his_rax in desc.hint) then
       begin
        new1:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        i:=GetFrameOffset(ctx.din.Operand[2]);
        movq(new1,[r_thrd+i]);

        push(r15);

        op_load_rax(ctx,r15);

        xchgq(r15,rax);

        _RM(desc.mem_reg,new1,[flags(ctx)+r15]);

        pop(r15);

        op_save_rax(ctx,rax);
       end else
       begin
        new1:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        i:=GetFrameOffset(ctx.din.Operand[2]);
        movq(new1,[r_thrd+i]);

        _RM(desc.mem_reg,new1,[flags(ctx)+r_tmp0]);

        if (his_xchg in desc.hint) then
        begin
         movq([r_thrd+i],new1);
        end;
       end;

      end;
    else
     Assert(False);
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

       if (not (his_wo in desc.hint)) or
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

       if not (his_ro in desc.hint) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq([r_thrd+i],fix_size(new1));
       end;

      end;

    //read only swapped

    mo_mem_reg:
     begin
      //input:rax

      new1:=new_reg(ctx.din.Operand[2]);

      imm:=0;
      if GetTargetOfs(ctx.din,ctx.code,3,imm) then
      begin
       imm_size:=ctx.din.Operand[3].Size;
       mem_size:=ctx.din.Operand[2].Size;

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
       _RM(desc.mem_reg,new1,[flags(ctx)+r_tmp0]);
      end;

     end;

    mo_mem_ctx:
     begin
      //input:rax

      new1:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

      i:=GetFrameOffset(ctx.din.Operand[2]);
      movq(fix_size(new1),[r_thrd+i]);

      imm:=0;
      if GetTargetOfs(ctx.din,ctx.code,3,imm) then
      begin
       imm_size:=ctx.din.Operand[3].Size;
       mem_size:=ctx.din.Operand[2].Size;

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
       _RM(desc.mem_reg,new1,[flags(ctx)+r_tmp0]);
      end;

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

     end

    else
     Assert(False);
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
      if (his_ro in desc.hint) then
      begin

       if (mem_size=os8) or
          (his_rw in desc.hint) then
       begin
        call_far(@uplift_jit); //in/out:rax uses:r14

        mem_in;
       end else
       begin
        //mem_size
        movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

        call_far(@copyin_mov); //in:rax(addr),r14:(size) out:rax

        mem_in;
       end;

      end else
      if (mem_size=os8) or
         (his_rw in desc.hint) then
      begin
       call_far(@uplift_jit); //in/out:rax uses:r14

       mem_out;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call_far(@copyout_mov); //in:rax(addr),r14:(size)

       link_next:=jmp(nil_link,os8);

       mem_out;

       reta;

       link_next._label:=get_curr_label.after;
      end;
     end;

   mo_reg_mem,
   mo_ctx_mem:
     begin
      if (mem_size=os8) or
         (his_rw in desc.hint) then
      begin
       call_far(@uplift_jit); //in/out:rax uses:r14

       mem_in;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call_far(@copyin_mov); //in:rax(addr),r14:(size) out:rax

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
      if (his_rax in desc.hint) then
      begin
       new2:=new_reg_size(rax,ctx.din.Operand[1]);

       op_load_rax(ctx,fix_size(new2));

       i:=GetFrameOffset(ctx.din.Operand[1]);
       _RM(desc.mem_reg,new1,[r_thrd+i]);

       op_save_rax(ctx,fix_size(new2));
      end else
      if (new1.ASize=os32) or
         (not_impl in desc.mem_reg.opt) then
      begin
       new2:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       if (not (his_wo in desc.hint)) or
          (his_ro in desc.hint) then
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

       if not (his_ro in desc.hint) then
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

        if (not (his_wo in desc.hint)) or
           (his_ro in desc.hint) then
        begin
         i:=GetFrameOffset(ctx.din.Operand[1]);
         movq(fix_size(new1),[r_thrd+i]);
        end;

        i:=GetFrameOffset(ctx.din.Operand[2]);
        movq(fix_size(new2),[r_thrd+i]);
       end;

       mem_size:=ctx.din.Operand[1].RegValue[0].ASize;

       if (his_wo in desc.hint) and
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

        if not (his_ro in desc.hint) then
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

procedure op_emit_shift2(var ctx:t_jit_context2;const desc:t_op_shift);
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

    else
     Assert(False);
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
      if true then
      begin
       call_far(@uplift_jit); //in/out:rax uses:r14

       mem_out;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call_far(@copyout_mov); //in:rax(addr),r14:(size)

       link_next:=jmp(nil_link,os8);

       mem_out;

       reta;

       link_next._label:=get_curr_label.after;
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

procedure op_emit_shift3(var ctx:t_jit_context2;const desc:t_op_shift);
var
 i:Integer;
 memop:t_memop_shift;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 imm:Int64;
 imm_size:TOperandSize;

 new1,new2:TRegValue;

 procedure mem_out;
 begin
  with ctx.builder do
   case memop of
    mo_mem_imm8:
      begin
       //input:rax

       imm:=0;
       if not GetTargetOfs(ctx.din,ctx.code,3,imm) then
       begin
        Assert(false);
       end;

       imm_size:=ctx.din.Operand[3].Size;

       Assert(imm_size=os8);
       Assert(not (not_impl in desc.reg_im8.opt));

       if is_preserved(ctx.din.Operand[2]) then
       begin
        //mem_ctx_imm
        new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        i:=GetFrameOffset(ctx.din.Operand[2]);
        movq(new2,[r_thrd+i]);
       end else
       begin
        //mem_reg_imm
        new2:=new_reg(ctx.din.Operand[2]);
       end;

       _RMI8(desc.reg_im8,new2,[flags(ctx)+r_tmp0],imm);
      end;
    mo_mem_cl:
      begin
       //input:rax

       if is_preserved(ctx.din.Operand[2]) then
       begin
        //mem_ctx_cl
        new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        i:=GetFrameOffset(ctx.din.Operand[2]);
        movq(new2,[r_thrd+i]);
       end else
       begin
        //mem_reg_cl
        new2:=new_reg(ctx.din.Operand[2]);
       end;

       _RM(desc.mem__cl,new2,[flags(ctx)+r_tmp0]);
      end;

    else
     Assert(False);
   end;
 end;

begin
 memop:=classif_shift3(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem_imm8,
   mo_mem_cl:
     begin
      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;
     end;
   else;
  end;

 with ctx.builder do
  case memop of
   mo_mem_imm8,
   mo_mem_cl:
     begin
      if true then
      begin
       call_far(@uplift_jit); //in/out:rax uses:r14

       mem_out;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call_far(@copyout_mov); //in:rax(addr),r14:(size)

       link_next:=jmp(nil_link,os8);

       mem_out;

       reta;

       link_next._label:=get_curr_label.after;
      end;
     end;

   mo_ctx_imm8:
     begin
      mem_size:=ctx.din.Operand[1].Size;

      imm:=0;
      if not GetTargetOfs(ctx.din,ctx.code,3,imm) then
      begin
       Assert(false);
      end;

      imm_size:=ctx.din.Operand[3].Size;
      Assert(imm_size=os8);

      if is_preserved(ctx.din.Operand[2]) then
      begin
       //ctx_ctx_imm

       if cmp_reg(ctx.din.Operand[1].RegValue[0],
                  ctx.din.Operand[2].RegValue[0]) then
       begin
        new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
        new2:=new1;

        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq(new1,[r_thrd+i]);
       end else
       begin
        new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
        new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq(new1,[r_thrd+i]);

        i:=GetFrameOffset(ctx.din.Operand[2]);
        movq(new2,[r_thrd+i]);
       end;

      end else
      begin
       //ctx_reg_imm
       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
       new2:=new_reg(ctx.din.Operand[2]);

       i:=GetFrameOffset(ctx.din.Operand[1]);
       movq(new1,[r_thrd+i]);
      end;

      _RRI8(desc.reg_im8,new1,new2,imm,mem_size);

      i:=GetFrameOffset(ctx.din.Operand[1]);
      movq([r_thrd+i],fix_size(new1));
     end;
    mo_ctx_cl:
     begin
      mem_size:=ctx.din.Operand[1].Size;

      if is_preserved(ctx.din.Operand[2]) then
      begin
       //ctx_ctx_cl

       if cmp_reg(ctx.din.Operand[1].RegValue[0],
                  ctx.din.Operand[2].RegValue[0]) then
       begin
        new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
        new2:=new1;

        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq(new1,[r_thrd+i]);
       end else
       begin
        new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
        new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        i:=GetFrameOffset(ctx.din.Operand[1]);
        movq(new1,[r_thrd+i]);

        i:=GetFrameOffset(ctx.din.Operand[2]);
        movq(new2,[r_thrd+i]);
       end;

      end else
      begin
       //ctx_reg_cl
       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
       new2:=new_reg(ctx.din.Operand[2]);

       i:=GetFrameOffset(ctx.din.Operand[1]);
       movq(new1,[r_thrd+i]);
      end;

      _RR(desc.mem__cl,new1,new2,mem_size);

      i:=GetFrameOffset(ctx.din.Operand[1]);
      movq([r_thrd+i],fix_size(new1));
     end;

    mo_reg_imm8:
     begin
      //reg_ctx_imm

      mem_size:=ctx.din.Operand[1].Size;

      imm:=0;
      if not GetTargetOfs(ctx.din,ctx.code,3,imm) then
      begin
       Assert(false);
      end;

      imm_size:=ctx.din.Operand[3].Size;
      Assert(imm_size=os8);

      new1:=new_reg(ctx.din.Operand[1]);
      new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

      i:=GetFrameOffset(ctx.din.Operand[2]);
      movq(new2,[r_thrd+i]);

      _RRI8(desc.reg_im8,new1,new2,imm,mem_size);
     end;

    mo_reg_cl:
     begin
      //reg_ctx_cl

      mem_size:=ctx.din.Operand[1].Size;

      new1:=new_reg(ctx.din.Operand[1]);
      new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

      i:=GetFrameOffset(ctx.din.Operand[2]);
      movq(new2,[r_thrd+i]);

      _RR(desc.mem__cl,new1,new2,mem_size);
     end

   else
    Assert(false);
  end;

end;

//

procedure op_emit_avx2_rr(var ctx:t_jit_context2;const desc:t_op_type);
var
 i:Integer;
 new1,new2:TRegValue;
begin
 if is_preserved(ctx.din.Operand[1]) then
 begin
  with ctx.builder do
  begin
   new1:=r_tmp0;
   new2:=new_reg(ctx.din.Operand[2]);

   _VV(desc,new1,new2,new2.ASize);

   i:=GetFrameOffset(ctx.din.Operand[1]);
   movq([r_thrd+i],new1);
  end;
 end else
 begin
  Assert(False);
 end;
end;

//

procedure op_emit_avx2(var ctx:t_jit_context2;const desc:t_op_desc);
var
 i:Integer;
 memop:t_memop_type2;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 new1,new2:TRegValue;

 procedure mem_out;
 begin
  with ctx.builder do
  begin
   //input:rax

   new1:=new_reg(ctx.din.Operand[2]);
   _VM(desc.mem_reg,new1,[flags(ctx)+r_tmp0],mem_size);
  end;
 end;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:rax

   new1:=new_reg(ctx.din.Operand[1]);
   _VM(desc.reg_mem,new1,[flags(ctx)+r_tmp0],mem_size);
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
       call_far(@uplift_jit); //in/out:rax uses:r14

       mem_out;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call_far(@copyout_mov); //in:rax(addr),r14:(size)

       link_next:=jmp(nil_link,os8);

       mem_out;

       reta;

       link_next._label:=get_curr_label.after;
      end;
     end;

   mo_reg_mem:
     begin
      if (his_align in desc.hint) then
      begin
       call_far(@uplift_jit); //in/out:rax uses:r14

       mem_in;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call_far(@copyin_mov); //in:rax(addr),r14:(size) out:rax

       mem_in;
      end;
     end;

   mo_ctx_reg:
     begin
      new1:=new_reg(ctx.din.Operand[2]);

      mem_size:=ctx.din.Operand[1].Size;

      if (not_impl in desc.mem_reg.opt) then
      begin
       new2:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       _VV(desc.reg_mem,new2,new1,mem_size);

       i:=GetFrameOffset(ctx.din.Operand[1]);
       movq([r_thrd+i],new2);
      end else
      begin
       i:=GetFrameOffset(ctx.din.Operand[1]);
       _VM(desc.mem_reg,new1,[r_thrd+i],mem_size);
      end;

     end;
   mo_reg_ctx:
     begin
      new1:=new_reg(ctx.din.Operand[1]);

      mem_size:=ctx.din.Operand[2].Size;

      i:=GetFrameOffset(ctx.din.Operand[2]);
      _VM(desc.reg_mem,new1,[r_thrd+i],mem_size);
     end;

   else
    Assert(false);
  end;

end;

procedure op_emit_avx3(var ctx:t_jit_context2;const desc:t_op_type);
var
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 i:Integer;
 imm:Int64;

 new1,new2:TRegValue;

 procedure mem_out;
 begin
  with ctx.builder do
  begin
   //input:rax

   mem_size:=ctx.din.Operand[1].Size;

   new1:=new_reg(ctx.din.Operand[2]);
   new2:=new_reg(ctx.din.Operand[3]);

   imm:=0;
   if GetTargetOfs(ctx.din,ctx.code,4,imm) then
   begin
    _VVMI8(desc,new2,new1,[flags(ctx)+r_tmp0],mem_size,imm);
   end else
   begin
    _VVM(desc,new2,new1,[flags(ctx)+r_tmp0],mem_size); //[mem],arg2,arg3 -> arg3,arg2,[mem]
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
    _VVMI8(desc,new1,new2,[flags(ctx)+r_tmp0],mem_size,imm);
   end else
   begin
    _VVM(desc,new1,new2,[flags(ctx)+r_tmp0],mem_size);
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
    call_far(@uplift_jit); //in/out:rax uses:r14

    mem_in;
   end else
   begin
    //mem_size
    movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

    call_far(@copyin_mov); //in:rax(addr),r14:(size) out:rax

    mem_in;
   end;

  end;

 end else
 if is_preserved(ctx.din.Operand[3]) then
 begin
  //mo_reg_reg_ctx

  with ctx.builder do
  begin

   mem_size:=ctx.din.Operand[3].Size;

   new1:=new_reg(ctx.din.Operand[1]);
   new2:=new_reg(ctx.din.Operand[2]);

   i:=GetFrameOffset(ctx.din.Operand[3]);

   imm:=0;
   if GetTargetOfs(ctx.din,ctx.code,4,imm) then
   begin
    _VVMI8(desc,new1,new2,[r_thrd+i],mem_size,imm);
   end else
   begin
    _VVM(desc,new1,new2,[r_thrd+i],mem_size);
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
    call_far(@uplift_jit); //in/out:rax uses:r14

    mem_out;
   end else
   begin
    //mem_size
    movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

    call_far(@copyout_mov); //in:rax(addr),r14:(size)

    link_next:=jmp(nil_link,os8);

    mem_out;

    reta;

    link_next._label:=get_curr_label.after;
   end;

  end;

 end else
 begin
  Assert(False);
 end;
end;

//rri,mri
procedure op_emit_avx3_imm8(var ctx:t_jit_context2;const desc:t_op_avx3_imm);
var
 i:Integer;
 memop:t_memop_type2;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 new1,new2:TRegValue;

 imm:Int64;

 procedure mem_out;
 begin
  with ctx.builder do
  begin
   //input:rax

   //mem_reg

   new2:=new_reg(ctx.din.Operand[2]);

   imm:=0;
   GetTargetOfs(ctx.din,ctx.code,3,imm);

   _VMI8(desc.mri,new2,[flags(ctx)+r_tmp0],mem_size,imm);
  end;
 end;


 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:rax

   //reg_mem

   new1:=new_reg(ctx.din.Operand[1]);

   imm:=0;
   GetTargetOfs(ctx.din,ctx.code,3,imm);

   _VMI8(desc.rmi,new1,[flags(ctx)+r_tmp0],mem_size,imm);
  end;
 end;


begin
 memop:=classif_memop2(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem_reg:
    begin
     build_lea(ctx,1,r_tmp0);
     mem_size:=ctx.din.Operand[1].Size;

     if (mem_size=os8) then
     begin
      call_far(@uplift_jit); //in/out:rax uses:r14

      mem_out;
     end else
     begin
      //mem_size
      movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

      call_far(@copyout_mov); //in:rax(addr),r14:(size)

      link_next:=jmp(nil_link,os8);

      mem_out;

      reta;

      link_next._label:=get_curr_label.after;
     end;
    end;

   mo_reg_mem:
     begin
      build_lea(ctx,2,r_tmp0);
      mem_size:=ctx.din.Operand[2].Size;

      if (mem_size=os8) then
      begin
       call_far(@uplift_jit); //in/out:rax uses:r14

       mem_in;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call_far(@copyin_mov); //in:rax(addr),r14:(size) out:rax

       mem_in;
      end;
     end;

   mo_ctx_reg:
    begin
     mem_size:=ctx.din.Operand[1].Size;
     i:=GetFrameOffset(ctx.din.Operand[1]);

     new2:=new_reg(ctx.din.Operand[2]);

     imm:=0;
     GetTargetOfs(ctx.din,ctx.code,3,imm);

     new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

     //mri

     _VVI8(desc.mri,new2,new1,mem_size,imm);

     movq([r_thrd+i],fix_size(new1));
    end;

   mo_reg_ctx:
    begin
     mem_size:=ctx.din.Operand[2].Size;
     i:=GetFrameOffset(ctx.din.Operand[2]);

     new1:=new_reg(ctx.din.Operand[1]);

     new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

     //rmi

     movq(fix_size(new2),[r_thrd+i]);

     _VVI8(desc.rmi,new1,new2,mem_size,imm);
    end


   else
    Assert(false);
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
    else
     Assert(False);
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
       call_far(@uplift_jit); //in/out:rax uses:r14

       mem_in;
      end else
      begin
       //mem_size
       movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

       call_far(@copyin_mov); //in:rax(addr),r14:(size) out:rax

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

//rrrr,rrmr
procedure op_emit_avx4(var ctx:t_jit_context2;const desc:t_op_type);
var
 mem_size:TOperandSize;

 new1,new2,new3:TRegValue;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:rax

   new1:=new_reg(ctx.din.Operand[1]);
   new2:=new_reg(ctx.din.Operand[2]);
   new3:=new_reg(ctx.din.Operand[4]);

   _VVMV(desc,new1,new2,[flags(ctx)+r_tmp0],mem_size,new3);
  end;
 end;

begin
 with ctx.builder do
  if is_memory(ctx.din.Operand[3]) then
  begin
   build_lea(ctx,3,r_tmp0);
   mem_size:=ctx.din.Operand[3].Size;

   if false then
   begin
    call_far(@uplift_jit); //in/out:rax uses:r14

    mem_in;
   end else
   begin
    //mem_size
    movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

    call_far(@copyin_mov); //in:rax(addr),r14:(size) out:rax

    mem_in;
   end;
  end else
  begin
   Assert(false);
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
   //not need load result
  end else
  begin
   new1:=new_reg(ctx.din.Operand[1]);
  end;

  if is_memory(ctx.din.Operand[2]) then
  begin
   new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

   build_lea(ctx,2,r_tmp0);
   mem_size:=ctx.din.Operand[2].Size;

   //mem_size
   movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

   call_far(@copyin_mov); //in:rax(addr),r14:(size) out:rax

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

  _VVV(desc,new1,new3,new2,new3.ASize); //1 3 2

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
   //not need load result
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

  if is_memory(ctx.din.Operand[3]) then
  begin
   new3:=new_reg_size(r_tmp1,ctx.din.Operand[3]);

   build_lea(ctx,3,r_tmp0);
   mem_size:=ctx.din.Operand[3].Size;

   //mem_size
   movi(new_reg_size(r_tmp1,os8),OPERAND_BYTES[mem_size]);

   call_far(@copyin_mov); //in:rax(addr),r14:(size) out:rax

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

  _VVV(desc,new1,new2,new3,new3.ASize); //1 2 3

  if is_preserved(ctx.din.Operand[1]) then
  begin
   i:=GetFrameOffset(ctx.din.Operand[1]);
   movq([r_thrd+i],fix_size(new1));
  end;

 end;
end;

end.




