unit x86_jit;

{
 x86-64 minimal JIT Compiler by Red-prig
}

{$mode ObjFPC}{$H+}

interface

uses
 mqueue,
 g_node_splay,
 x86_fpdbgdisas;

type
 t_jit_lea=packed object
  ARegValue:TRegValues;
  AOffset  :Int64;
  AMemSize :TOperandSize;
  ASegment :Byte;
  ALock    :Boolean;
 end;

 t_vw_mode=(vwZero,vwOne,vwR64,vwM64);
 t_vl_mode=(vl256,vlZero,vlOne);

 t_op_opt=Set of (not_impl,not_os8,not_prefix,verif_vex_len,verif_rexw);

 t_op_type=bitpacked object
  op     :DWORD;
  index  :Byte;
  simdop :0..4;
  mm     :0..3;
  vx_len :0..3;
  rexw   :Boolean;
  vw_mode:t_vw_mode;
  vl_mode:t_vl_mode;
  opt    :t_op_opt;
 end;

 TOperandSizeSet =Set of TOperandSize;
 TRegisterTypeSet=Set of TRegisterType;

 t_jit_leas =array of t_jit_lea;

 t_jit_link_type=(lnkNone,lnkData,lnkPlt,lnkLabelBefore,lnkLabelAfter);

 p_jit_instruction=^t_jit_instruction;
 t_jit_instruction=packed object
  entry:TAILQ_ENTRY;
  AData:array[0..15] of Byte;
  AInstructionOffset:Integer;
  ASize:Byte;
  ALink:packed record
   AType  :t_jit_link_type;
   ASize  :Byte;
   AOffset:Byte;
   ALink  :Pointer;
  end;
  function  AInstructionEnd:Integer; inline;
  procedure EmitByte(b:byte); inline;
  procedure EmitWord(w:Word); inline;
  procedure EmitInt32(i:Integer); inline;
  procedure EmitInt64(i:Int64); inline;
  procedure EmitSelector(Segment:ShortInt); inline;
  procedure EmitREX(rexB,rexX,rexR,rexW:Boolean); inline;
  procedure EmitModRM(Mode,Index,RM:Byte); inline;
  procedure EmitSIB(Scale,Index,Base:Byte); inline;
  procedure EmitRvvv(rexR:Boolean;VectorIndex,VectorLength,SimdOpcode:Byte); inline;
  procedure EmitRXBm(rexB,rexX,rexR:Boolean;mm:Byte); inline;
  procedure EmitWvvv(rexW:Boolean;VectorIndex,VectorLength,SimdOpcode:Byte); inline;
 end;

const
 default_jit_instruction:t_jit_instruction=(
  AData:($90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90);
 );

type
 p_jit_data=^t_jit_data;
 t_jit_data=packed object
  entry :TAILQ_ENTRY;
  pLeft :p_jit_data;
  pRight:p_jit_data;
  pData :Pointer;
  pId   :Integer;
  function c(n1,n2:p_jit_data):Integer; static;
 end;

 t_jit_data_set=specialize TNodeSplay<t_jit_data>;

 p_jit_plt=^t_jit_plt;
 t_jit_plt=packed record
  cache:Pointer;
 end;

 p_jit_code_chunk=^t_jit_code_chunk;
 t_jit_code_chunk=object
  entry :TAILQ_ENTRY;
  pLeft :p_jit_code_chunk;
  pRight:p_jit_code_chunk;
  start:QWORD;
  __end:QWORD;
  data :QWORD;
  AInstructions:TAILQ_HEAD;
  function c(n1,n2:p_jit_code_chunk):Integer; static;
 end;

 t_jit_code_chunk_set=specialize TNodeSplay<t_jit_code_chunk>;

 t_jit_i_link=object
  //private
   AType:t_jit_link_type;
   ALink:Pointer; //variable
   procedure set_label(link:t_jit_i_link);
   function  get_label():t_jit_i_link;
  public
   function  is_valid:Boolean;
   function  offset:Integer;
   function  before:t_jit_i_link;
   function  after :t_jit_i_link;
   function  prev  :t_jit_i_link;
   function  next  :t_jit_i_link;
   function  _node :p_jit_instruction;
   property  _label:t_jit_i_link read get_label write set_label;
 end;

const
 nil_link:t_jit_i_link=(AType:lnkNone;ALink:nil);

operator = (A,B:t_jit_i_link):Boolean;

//SimdOpcode (soNone=0, so66=1, soF3=2, soF2=3);
//mm         (0F=1, 0F38=2, 0F3A=3)

type
 t_jit_builder_allocator=object
  type
   PAllocNode=^TAllocNode;
   TAllocNode=packed record
    link:PAllocNode;
    data:record end;
   end;
  var
   pHead:SLIST_HEAD;
   curr_apos:ptruint; //alloc pos in current node
   curr_size:ptruint; //useable size of current node
   used_size:ptruint; //full usable size
   full_size:ptruint; //full alloc size
  Function  Alloc(Size:ptruint):Pointer;
  Procedure Free;
 end;

 t_jit_copy_ptr=packed record
  ptr :PByte;
  size:Integer;
  pos :Integer;
 end;

 p_jit_builder=^t_jit_builder;
 t_jit_builder=object
  Const
   LOCK:t_jit_lea=(ARegValue:((AType:regNone),(AType:regNone));ALock:True);
   FS  :t_jit_lea=(ARegValue:((AType:regNone),(AType:regNone));ASegment:4);
   GS  :t_jit_lea=(ARegValue:((AType:regNone),(AType:regNone));ASegment:5);

   ah  :TRegValue=(AType:regGeneralH;ASize:os8;AIndex:0);
   ch  :TRegValue=(AType:regGeneralH;ASize:os8;AIndex:1);
   dh  :TRegValue=(AType:regGeneralH;ASize:os8;AIndex:2);
   bh  :TRegValue=(AType:regGeneralH;ASize:os8;AIndex:3);

   al  :TRegValue=(AType:regGeneral;ASize:os8;AIndex: 0);
   cl  :TRegValue=(AType:regGeneral;ASize:os8;AIndex: 1);
   dl  :TRegValue=(AType:regGeneral;ASize:os8;AIndex: 2);
   bl  :TRegValue=(AType:regGeneral;ASize:os8;AIndex: 3);
   spl :TRegValue=(AType:regGeneral;ASize:os8;AIndex: 4);
   bpl :TRegValue=(AType:regGeneral;ASize:os8;AIndex: 5);
   sil :TRegValue=(AType:regGeneral;ASize:os8;AIndex: 6);
   dil :TRegValue=(AType:regGeneral;ASize:os8;AIndex: 7);
   r8b :TRegValue=(AType:regGeneral;ASize:os8;AIndex: 8);
   r9b :TRegValue=(AType:regGeneral;ASize:os8;AIndex: 9);
   r10b:TRegValue=(AType:regGeneral;ASize:os8;AIndex:10);
   r11b:TRegValue=(AType:regGeneral;ASize:os8;AIndex:11);
   r12b:TRegValue=(AType:regGeneral;ASize:os8;AIndex:12);
   r13b:TRegValue=(AType:regGeneral;ASize:os8;AIndex:13);
   r14b:TRegValue=(AType:regGeneral;ASize:os8;AIndex:14);
   r15b:TRegValue=(AType:regGeneral;ASize:os8;AIndex:15);

   ax  :TRegValue=(AType:regGeneral;ASize:os16;AIndex: 0);
   cx  :TRegValue=(AType:regGeneral;ASize:os16;AIndex: 1);
   dx  :TRegValue=(AType:regGeneral;ASize:os16;AIndex: 2);
   bx  :TRegValue=(AType:regGeneral;ASize:os16;AIndex: 3);
   sp  :TRegValue=(AType:regGeneral;ASize:os16;AIndex: 4);
   bp  :TRegValue=(AType:regGeneral;ASize:os16;AIndex: 5);
   si  :TRegValue=(AType:regGeneral;ASize:os16;AIndex: 6);
   di  :TRegValue=(AType:regGeneral;ASize:os16;AIndex: 7);
   r8w :TRegValue=(AType:regGeneral;ASize:os16;AIndex: 8);
   r9w :TRegValue=(AType:regGeneral;ASize:os16;AIndex: 9);
   r10w:TRegValue=(AType:regGeneral;ASize:os16;AIndex:10);
   r11w:TRegValue=(AType:regGeneral;ASize:os16;AIndex:11);
   r12w:TRegValue=(AType:regGeneral;ASize:os16;AIndex:12);
   r13w:TRegValue=(AType:regGeneral;ASize:os16;AIndex:13);
   r14w:TRegValue=(AType:regGeneral;ASize:os16;AIndex:14);
   r15w:TRegValue=(AType:regGeneral;ASize:os16;AIndex:15);

   eax :TRegValue=(AType:regGeneral;ASize:os32;AIndex: 0);
   ecx :TRegValue=(AType:regGeneral;ASize:os32;AIndex: 1);
   edx :TRegValue=(AType:regGeneral;ASize:os32;AIndex: 2);
   ebx :TRegValue=(AType:regGeneral;ASize:os32;AIndex: 3);
   esp :TRegValue=(AType:regGeneral;ASize:os32;AIndex: 4);
   ebp :TRegValue=(AType:regGeneral;ASize:os32;AIndex: 5);
   esi :TRegValue=(AType:regGeneral;ASize:os32;AIndex: 6);
   edi :TRegValue=(AType:regGeneral;ASize:os32;AIndex: 7);
   r8d :TRegValue=(AType:regGeneral;ASize:os32;AIndex: 8);
   r9d :TRegValue=(AType:regGeneral;ASize:os32;AIndex: 9);
   r10d:TRegValue=(AType:regGeneral;ASize:os32;AIndex:10);
   r11d:TRegValue=(AType:regGeneral;ASize:os32;AIndex:11);
   r12d:TRegValue=(AType:regGeneral;ASize:os32;AIndex:12);
   r13d:TRegValue=(AType:regGeneral;ASize:os32;AIndex:13);
   r14d:TRegValue=(AType:regGeneral;ASize:os32;AIndex:14);
   r15d:TRegValue=(AType:regGeneral;ASize:os32;AIndex:15);

   rax:TRegValue=(AType:regGeneral;ASize:os64;AIndex: 0);
   rcx:TRegValue=(AType:regGeneral;ASize:os64;AIndex: 1);
   rdx:TRegValue=(AType:regGeneral;ASize:os64;AIndex: 2);
   rbx:TRegValue=(AType:regGeneral;ASize:os64;AIndex: 3);
   rsp:TRegValue=(AType:regGeneral;ASize:os64;AIndex: 4);
   rbp:TRegValue=(AType:regGeneral;ASize:os64;AIndex: 5);
   rsi:TRegValue=(AType:regGeneral;ASize:os64;AIndex: 6);
   rdi:TRegValue=(AType:regGeneral;ASize:os64;AIndex: 7);
   r8 :TRegValue=(AType:regGeneral;ASize:os64;AIndex: 8);
   r9 :TRegValue=(AType:regGeneral;ASize:os64;AIndex: 9);
   r10:TRegValue=(AType:regGeneral;ASize:os64;AIndex:10);
   r11:TRegValue=(AType:regGeneral;ASize:os64;AIndex:11);
   r12:TRegValue=(AType:regGeneral;ASize:os64;AIndex:12);
   r13:TRegValue=(AType:regGeneral;ASize:os64;AIndex:13);
   r14:TRegValue=(AType:regGeneral;ASize:os64;AIndex:14);
   r15:TRegValue=(AType:regGeneral;ASize:os64;AIndex:15);

   rip:TRegValue=(AType:regRip;ASize:os64);

   mm0 :TRegValue=(AType:regMm;ASize:os64;AIndex:  0);
   mm1 :TRegValue=(AType:regMm;ASize:os64;AIndex:  1);
   mm2 :TRegValue=(AType:regMm;ASize:os64;AIndex:  2);
   mm3 :TRegValue=(AType:regMm;ASize:os64;AIndex:  3);
   mm4 :TRegValue=(AType:regMm;ASize:os64;AIndex:  4);
   mm5 :TRegValue=(AType:regMm;ASize:os64;AIndex:  5);
   mm6 :TRegValue=(AType:regMm;ASize:os64;AIndex:  6);
   mm7 :TRegValue=(AType:regMm;ASize:os64;AIndex:  7);
   mm8 :TRegValue=(AType:regMm;ASize:os64;AIndex:  8);
   mm9 :TRegValue=(AType:regMm;ASize:os64;AIndex:  9);
   mm10:TRegValue=(AType:regMm;ASize:os64;AIndex: 10);
   mm11:TRegValue=(AType:regMm;ASize:os64;AIndex: 11);
   mm12:TRegValue=(AType:regMm;ASize:os64;AIndex: 12);
   mm13:TRegValue=(AType:regMm;ASize:os64;AIndex: 13);
   mm14:TRegValue=(AType:regMm;ASize:os64;AIndex: 14);
   mm15:TRegValue=(AType:regMm;ASize:os64;AIndex: 15);

   xmm0 :TRegValue=(AType:regXmm;ASize:os128;AIndex:  0);
   xmm1 :TRegValue=(AType:regXmm;ASize:os128;AIndex:  1);
   xmm2 :TRegValue=(AType:regXmm;ASize:os128;AIndex:  2);
   xmm3 :TRegValue=(AType:regXmm;ASize:os128;AIndex:  3);
   xmm4 :TRegValue=(AType:regXmm;ASize:os128;AIndex:  4);
   xmm5 :TRegValue=(AType:regXmm;ASize:os128;AIndex:  5);
   xmm6 :TRegValue=(AType:regXmm;ASize:os128;AIndex:  6);
   xmm7 :TRegValue=(AType:regXmm;ASize:os128;AIndex:  7);
   xmm8 :TRegValue=(AType:regXmm;ASize:os128;AIndex:  8);
   xmm9 :TRegValue=(AType:regXmm;ASize:os128;AIndex:  9);
   xmm10:TRegValue=(AType:regXmm;ASize:os128;AIndex: 10);
   xmm11:TRegValue=(AType:regXmm;ASize:os128;AIndex: 11);
   xmm12:TRegValue=(AType:regXmm;ASize:os128;AIndex: 12);
   xmm13:TRegValue=(AType:regXmm;ASize:os128;AIndex: 13);
   xmm14:TRegValue=(AType:regXmm;ASize:os128;AIndex: 14);
   xmm15:TRegValue=(AType:regXmm;ASize:os128;AIndex: 15);

   ymm0 :TRegValue=(AType:regXmm;ASize:os256;AIndex:  0);
   ymm1 :TRegValue=(AType:regXmm;ASize:os256;AIndex:  1);
   ymm2 :TRegValue=(AType:regXmm;ASize:os256;AIndex:  2);
   ymm3 :TRegValue=(AType:regXmm;ASize:os256;AIndex:  3);
   ymm4 :TRegValue=(AType:regXmm;ASize:os256;AIndex:  4);
   ymm5 :TRegValue=(AType:regXmm;ASize:os256;AIndex:  5);
   ymm6 :TRegValue=(AType:regXmm;ASize:os256;AIndex:  6);
   ymm7 :TRegValue=(AType:regXmm;ASize:os256;AIndex:  7);
   ymm8 :TRegValue=(AType:regXmm;ASize:os256;AIndex:  8);
   ymm9 :TRegValue=(AType:regXmm;ASize:os256;AIndex:  9);
   ymm10:TRegValue=(AType:regXmm;ASize:os256;AIndex: 10);
   ymm11:TRegValue=(AType:regXmm;ASize:os256;AIndex: 11);
   ymm12:TRegValue=(AType:regXmm;ASize:os256;AIndex: 12);
   ymm13:TRegValue=(AType:regXmm;ASize:os256;AIndex: 13);
   ymm14:TRegValue=(AType:regXmm;ASize:os256;AIndex: 14);
   ymm15:TRegValue=(AType:regXmm;ASize:os256;AIndex: 15);
  var
   ACodeChunkSet   :t_jit_code_chunk_set;
   ACodeChunkList  :TAILQ_HEAD;
   ACodeChunkCurr  :p_jit_code_chunk;
   //
   ADataSet        :t_jit_data_set;
   ADataList       :TAILQ_HEAD;
   //
   AInstructionSize:Integer;
   ADataCount      :Integer;
   APltCount       :Integer;
   //
   Allocator:t_jit_builder_allocator;
  //
  Function  Alloc(Size:ptruint):Pointer;
  Procedure Free;
  Function  _new_chunk(start:QWORD):p_jit_code_chunk;
  procedure _end_chunk(__end:QWORD);
  procedure _add(const ji:t_jit_instruction);
  Function  get_curr_label:t_jit_i_link;
  Function  _add_data(P:Pointer):p_jit_data;
  Function  _add_plt:Integer;
  function  _get_base_offset(AType:t_jit_link_type):Integer;
  //
  Function  last_instruction:p_jit_instruction;
  //
  function  call_far(P:Pointer):t_jit_i_link;
  function  jmp_far (P:Pointer):t_jit_i_link;
  //
  function  call(_label_id:t_jit_i_link):t_jit_i_link;
  function  jmp (_label_id:t_jit_i_link;size:TOperandSize=os32):t_jit_i_link;
  function  jcc (op:TOpCodeSuffix;_label_id:t_jit_i_link;size:TOperandSize=os32):t_jit_i_link;
  function  loop(op:TOpCodeSuffix;_label_id:t_jit_i_link;size:TAddressSize):t_jit_i_link;
  function  jcxz(_label_id:t_jit_i_link;size:TAddressSize):t_jit_i_link;
  function  movj(reg:TRegValue;mem:t_jit_leas;_label_id:t_jit_i_link):t_jit_i_link;
  function  leaj(reg:TRegValue;mem:t_jit_leas;_label_id:t_jit_i_link):t_jit_i_link;
  function  leap(reg:TRegValue):t_jit_i_link;
  //
  Procedure jmp(reg:TRegValue);
  Procedure call(reg:TRegValue);
  Procedure reta;
  Procedure ud2;
  //
  Function  GetInstructionsSize:Integer;
  Function  GetDataSize:Integer;
  Function  GetPltSize:Integer;
  Function  GetPltStart:Integer;
  Function  GetMemSize:Integer;
  Procedure RebuldChunkList;
  Procedure RebuldInstructionOffset;
  Procedure LinkData;
  Function  CopyChunks(var rec:t_jit_copy_ptr):Boolean;
  Function  CopyData  (var rec:t_jit_copy_ptr):Boolean;
  Function  CopyPlt   (var rec:t_jit_copy_ptr):Boolean;
  Function  SaveTo(ptr:PByte;size:Integer):Integer;
  //
  procedure _RM     (const desc:t_op_type;reg:TRegValue;mem:t_jit_leas);
  procedure _RMI    (const desc:t_op_type;reg:TRegValue;mem:t_jit_leas;imm:Int64);
  procedure _RMI8   (const desc:t_op_type;reg:TRegValue;mem:t_jit_leas;imm:Byte);
  procedure _RR     (const desc:t_op_type;reg0,reg1:TRegValue;size:TOperandSize=os0);
  procedure _RRI    (const desc:t_op_type;reg0,reg1:TRegValue;imm:Int64;size:TOperandSize=os0);
  procedure _RRI8   (const desc:t_op_type;reg0,reg1:TRegValue;imm:Byte;size:TOperandSize=os0);
  procedure _R      (const desc:t_op_type;reg:TRegValue);
  procedure _O      (op:DWORD;Size:TOperandSize=os0;opt:t_op_opt=[]);
  procedure _O      (const desc:t_op_type;reg:TRegValue);
  procedure _M      (const desc:t_op_type;mem:t_jit_leas);
  procedure _RI     (const desc:t_op_type;reg:TRegValue;imm:Int64);
  procedure _MI     (const desc:t_op_type;mem:t_jit_leas;imm:Int64);
  procedure _RI8    (const desc:t_op_type;reg:TRegValue;imm:Byte);
  procedure _MI8    (const desc:t_op_type;mem:t_jit_leas;imm:Byte);
  procedure cmov    (op:TOpCodeSuffix;reg:TRegValue;mem:t_jit_leas);
  procedure cmov    (op:TOpCodeSuffix;reg0:TRegValue;reg1:TRegValue);
  procedure _push   (op,index:Byte;mem:t_jit_leas);
  procedure _push   (op:Byte;reg:TRegValue);
  procedure _pushi  (size:TOperandSize;imm:Integer);
  procedure movq    (reg0:TRegValue ;reg1:TRegValue);
  procedure movi    (mem:t_jit_leas ;imm:Int64);
  procedure movi    (reg:TRegValue  ;imm:Int64);
  procedure movi64  (reg:TRegValue  ;imm:Int64);
  procedure movq    (reg:TRegValue  ;mem:t_jit_leas);
  procedure movq    (mem:t_jit_leas ;reg:TRegValue);
  procedure leaq    (reg:TRegValue  ;mem:t_jit_leas);
  procedure addq    (mem:t_jit_leas ;reg:TRegValue);
  procedure addq    (reg:TRegValue  ;mem:t_jit_leas);
  procedure addq    (reg0:TRegValue ;reg1:TRegValue);
  procedure addi    (reg:TRegValue  ;imm:Int64);
  procedure addi8   (reg:TRegValue  ;imm:Byte);
  procedure addi8   (mem:t_jit_leas ;imm:Byte);
  procedure subq    (mem:t_jit_leas ;reg:TRegValue);
  procedure subq    (reg:TRegValue  ;mem:t_jit_leas);
  procedure subq    (reg0:TRegValue ;reg1:TRegValue);
  procedure subi    (reg:TRegValue  ;imm:Int64);
  procedure subi8   (reg:TRegValue  ;imm:Byte);
  procedure subi8   (mem:t_jit_leas ;imm:Byte);
  procedure xorq    (reg0:TRegValue ;reg1:TRegValue);
  procedure cmpq    (mem:t_jit_leas ;reg:TRegValue);
  procedure cmpq    (reg:TRegValue  ;mem:t_jit_leas);
  procedure cmpq    (reg0:TRegValue ;reg1:TRegValue);
  procedure cmpi    (reg:TRegValue  ;imm:Int64);
  procedure cmpi    (mem:t_jit_leas ;imm:Int64);
  procedure cmpi8   (reg:TRegValue  ;imm:Byte);
  procedure cmpi8   (mem:t_jit_leas ;imm:Byte);
  procedure xchgq   (reg0:TRegValue ;reg1:TRegValue);
  procedure push    (mem:t_jit_leas);
  procedure push    (reg:TRegValue);
  procedure push8   (imm:Integer);
  procedure push16  (imm:Integer);
  procedure push32  (imm:Integer);
  procedure pop     (mem:t_jit_leas);
  procedure pop     (reg:TRegValue);
  procedure pushfq  (size:TOperandSize);
  procedure popfq   (size:TOperandSize);
  procedure _VM     (const desc:t_op_type;mem:t_jit_leas);
  procedure _VM     (const desc:t_op_type;reg:TRegValue;mem:t_jit_leas);
  procedure _VV     (const desc:t_op_type;reg0,reg1:TRegValue;size:TOperandSize=os0);
  procedure _VM_F3  (const desc:t_op_type;reg:TRegValue;mem:t_jit_leas);
  procedure _VV_F3  (const desc:t_op_type;reg0,reg1:TRegValue;size:TOperandSize=os0);
  procedure _VVM    (const desc:t_op_type;reg0,reg1:TRegValue;mem:t_jit_leas);
  procedure _VVMI8  (const desc:t_op_type;reg0,reg1:TRegValue;mem:t_jit_leas;imm8:Byte);
  procedure _VVMV   (const desc:t_op_type;reg0,reg1:TRegValue;mem:t_jit_leas;reg2:TRegValue);
  procedure _VVV    (const desc:t_op_type;reg0,reg1,reg2:TRegValue;size:TOperandSize=os0);
  procedure _VVVI8  (const desc:t_op_type;reg0,reg1,reg2:TRegValue;imm8:Byte;size:TOperandSize=os0);
  procedure _VVI8   (const desc:t_op_type;reg0,reg1:TRegValue;imm8:Byte;size:TOperandSize=os0);
  procedure _VMI8   (const desc:t_op_type;reg:TRegValue;mem:t_jit_leas;imm8:Byte);
  procedure vmovdqu (reg:TRegValue ;mem:t_jit_leas);
  procedure vmovdqu (mem:t_jit_leas;reg:TRegValue);
  procedure vmovdqa (reg:TRegValue ;mem:t_jit_leas);
  procedure vmovdqa (mem:t_jit_leas;reg:TRegValue);
  procedure vmovntdq(mem:t_jit_leas;reg:TRegValue);
  procedure vmovups (reg:TRegValue ;mem:t_jit_leas);
  procedure vmovups (mem:t_jit_leas;reg:TRegValue);
  procedure vmovdqa (reg0:TRegValue;reg1:TRegValue);
  procedure sahf;
  procedure lahf;
  procedure cli;
  procedure sti;
  procedure seto(reg:TRegValue);
  procedure int3;
  procedure testq(reg0:TRegValue;reg1:TRegValue);
  procedure bti8 (mem:t_jit_leas;imm:Byte);
 end;

operator :=(const A:TRegValue):t_jit_lea;
operator + (const A,B:t_jit_lea):t_jit_lea;
operator + (const A:t_jit_lea;const B:TRegValue):t_jit_lea;
operator + (const A:t_jit_lea;B:Integer):t_jit_lea;
operator - (const A:t_jit_lea;B:Integer):t_jit_lea;
operator + (const A:t_jit_lea;B:Int64):t_jit_lea;
operator - (const A:t_jit_lea;B:Int64):t_jit_lea;
operator + (const A:t_jit_lea;B:QWORD):t_jit_lea;
operator - (const A:t_jit_lea;B:QWORD):t_jit_lea;
operator + (const A:t_jit_lea;B:TOperandSize):t_jit_lea;
operator :=(const A:TOperandSize):t_jit_lea;
operator * (const A:t_jit_lea;B:Integer):t_jit_lea;

function Sums(mem:t_jit_leas):t_jit_lea;
function mem_size(mem:t_jit_leas):TOperandSize;

function classif_offset_32(AOffset:Integer):Byte;
function classif_offset_64(AOffset:Int64):TOperandSize;
function classif_offset_u64(AOffset:QWORD):TOperandSize;
function classif_offset_se64(AOffset:Int64):TOperandSize;

implementation

function t_jit_data.c(n1,n2:p_jit_data):Integer;
begin
 Result:=Integer(n1^.pData>n2^.pData)-Integer(n1^.pData<n2^.pData);
end;

function t_jit_code_chunk.c(n1,n2:p_jit_code_chunk):Integer;
begin
 Result:=Integer(n1^.start>n2^.start)-Integer(n1^.start<n2^.start);
end;

function is_valid_reg_type(reg:TRegValue):Boolean; inline;
begin
 Result:=(reg.AType<>regNone) and
         (reg.AType<>regInvalid);
end;

function is_valid_scale(reg:TRegValue):Boolean; inline;
begin
 case reg.AScale of
  0,
  1,
  2,
  4,
  8:Result:=True;
  else
    Result:=False;
 end;
end;

function get_scale_idx(reg:TRegValue):Byte; inline;
begin
 case reg.AScale of
  0:Result:=0;
  1:Result:=0;
  2:Result:=1;
  4:Result:=2;
  8:Result:=3;
  else
    Result:=0;
 end;
end;

function classif_offset_32(AOffset:Integer):Byte;
begin
 case AOffset of
                0:Result:=0;
  -128..-1,1..127:Result:=1;
  else
                  Result:=2;
 end;
end;

function classif_offset_64(AOffset:Int64):TOperandSize;
begin
 case AOffset of
                                  0:Result:=os0;
  -128       ..  -1,  1..127       :Result:=os8;
  -2147483648..-129,128..2147483647:Result:=os32;
  else
                                    Result:=os64;
 end;
end;

function classif_offset_u64(AOffset:QWORD):TOperandSize;
begin
 case AOffset of
                0:Result:=os0;
           1..$FF:Result:=os8;
  $100..$FFFFFFFF:Result:=os32;
  else
                  Result:=os64;
 end;
end;

function classif_offset_se64(AOffset:Int64):TOperandSize;
begin
 case AOffset of
                0:Result:=os0;
           1..127:Result:=os8;
  128..2147483647:Result:=os32;
  else
                  Result:=os64;
 end;
end;

function classif_offset_32(reg:t_jit_lea):Byte; inline;
begin
 Result:=classif_offset_32(reg.AOffset);
end;

function classif_offset_64(reg:t_jit_lea):TOperandSize; inline;
begin
 Result:=classif_offset_64(reg.AOffset);
end;

function is_not_reg(reg:t_jit_lea):Boolean; inline;
begin
 Result:=(not is_valid_reg_type(reg.ARegValue[0])) and
         (not is_valid_reg_type(reg.ARegValue[1]));
end;

function is_one_reg(reg:t_jit_lea):Boolean; inline;
begin
 Result:=(    is_valid_reg_type(reg.ARegValue[0])) and
         (not is_valid_reg_type(reg.ARegValue[1]));
end;

function is_reg_size(reg:t_jit_lea;Size:TOperandSizeSet):Boolean; inline;
begin
 Result:=(reg.ARegValue[0].ASize in Size) and
         ((reg.ARegValue[1].ASize in Size) or (reg.ARegValue[1].ASize=os0));
end;

function is_reg_type(reg:t_jit_lea;AType:TRegisterTypeSet):Boolean; inline;
begin
 Result:=(reg.ARegValue[0].AType in AType) and
         ((reg.ARegValue[1].AType in AType) or (reg.ARegValue[1].AType=regNone));
end;

function is_valid_seg(reg:t_jit_lea):Boolean; inline;
begin
 case reg.ASegment of
  0..5:Result:=True
  else
       Result:=False;
 end;
end;

function is_valid_scale(reg:t_jit_lea):Boolean; inline;
begin
 Result:=is_valid_scale(reg.ARegValue[0]) and (reg.ARegValue[1].AScale<=1)
end;

operator := (const A:TRegValue):t_jit_lea;
begin
 Result:=Default(t_jit_lea);
 Result.ARegValue[0]:=A;
end;

operator + (const A,B:t_jit_lea):t_jit_lea;
begin
 Assert(is_not_reg(A) or is_one_reg(A));
 Assert(is_not_reg(B) or is_one_reg(B));

 Result:=A;

 if is_one_reg(Result) and
    is_one_reg(B) then
 begin
  if (A.ARegValue[0].AScale>1) then
  begin
   Result.ARegValue[0]:=A.ARegValue[0];
   Result.ARegValue[1]:=B.ARegValue[0];
  end else
  begin
   Result.ARegValue[1]:=A.ARegValue[0];
   Result.ARegValue[0]:=B.ARegValue[0];
  end;
 end else
 if is_one_reg(B) then
 begin
  Result.ARegValue[0]:=B.ARegValue[0];
 end;

 if is_valid_seg(Result) then
 begin
  if is_valid_seg(B) then
  begin
   Result.ASegment:=B.ASegment;
  end;
 end else
 if is_valid_seg(B) then
 begin
  Result.ASegment:=B.ASegment;
 end;

 if (Result.AMemSize<>os0) then
 begin
  if (B.AMemSize<>os0) then
  begin
   Result.AMemSize:=B.AMemSize;
  end;
 end else
 if (B.AMemSize<>os0) then
 begin
  Result.AMemSize:=B.AMemSize;
 end;

 Result.ALock:=Result.ALock or B.ALock;

 Result.AOffset:=Result.AOffset+B.AOffset;
end;

operator + (const A:t_jit_lea;const B:TRegValue):t_jit_lea;
begin
 Assert(is_not_reg(A) or is_one_reg(A));

 Result:=A;

 if is_one_reg(Result) then
 begin
  if (A.ARegValue[0].AScale>1) then
  begin
   Result.ARegValue[0]:=A.ARegValue[0];
   Result.ARegValue[1]:=B;
  end else
  begin
   Result.ARegValue[1]:=A.ARegValue[0];
   Result.ARegValue[0]:=B;
  end;
 end else
 begin
  Result.ARegValue[0]:=B;
 end;
end;

operator + (const A:t_jit_lea;B:Integer):t_jit_lea;
begin
 Result:=A;

 Result.AOffset:=Result.AOffset+B;
end;

operator - (const A:t_jit_lea;B:Integer):t_jit_lea;
begin
 Result:=A;

 Result.AOffset:=Result.AOffset-B;
end;

operator + (const A:t_jit_lea;B:Int64):t_jit_lea;
begin
 Result:=A;

 Result.AOffset:=Result.AOffset+B;
end;

operator - (const A:t_jit_lea;B:Int64):t_jit_lea;
begin
 Result:=A;

 Result.AOffset:=Result.AOffset-B;
end;

operator + (const A:t_jit_lea;B:QWORD):t_jit_lea;
begin
 Result:=A;

 Result.AOffset:=Result.AOffset+B;
end;

operator - (const A:t_jit_lea;B:QWORD):t_jit_lea;
begin
 Result:=A;

 Result.AOffset:=Result.AOffset-B;
end;

operator + (const A:t_jit_lea;B:TOperandSize):t_jit_lea;
begin
 Result:=A;

 Result.AMemSize:=B;
end;

operator :=(const A:TOperandSize):t_jit_lea;
begin
 Result:=Default(t_jit_lea);
 Result.AMemSize:=A;
end;

operator * (const A:t_jit_lea;B:Integer):t_jit_lea;
begin
 Assert(is_one_reg(A));

 Result:=A;

 if (Result.ARegValue[0].AScale<=1) then
 begin
  Result.ARegValue[0].AScale:=B;
 end else
 begin
  Result.ARegValue[0].AScale:=Result.ARegValue[0].AScale*B;
 end;
end;

function Sums(mem:t_jit_leas):t_jit_lea;
var
 i:Integer;
begin
 if (Length(mem)=0) then
 begin
  Result:=Default(t_jit_lea);
 end else
 if (Length(mem)=1) then
 begin
  Result:=mem[0];
 end else
 begin
  Result:=mem[0];
  For i:=1 to High(mem) do
  begin
   Result:=Result+mem[i];
  end;
 end;
end;

function mem_size(mem:t_jit_leas):TOperandSize;
begin
 Result:=Sums(mem).AMemSize;
end;

function get_vex_len(mode:t_vl_mode;size:TOperandSize):Byte; inline;
begin
 case mode of
  vl256 :Result:=ord(size=os256);
  vlZero:Result:=0;
  vlOne :Result:=1;
 end;
end;

function get_vex_len(const desc:t_op_type;size:TOperandSize):Byte; inline;
begin
 Result:=get_vex_len(desc.vl_mode,size);
end;

function get_vex_len(const desc:t_op_type;const mreg:t_jit_lea):Byte; inline;
begin
 Result:=get_vex_len(desc.vl_mode,mreg.AMemSize);
end;

function get_vex_rexw(mode:t_vw_mode;rsize,msize:TOperandSize):Boolean; inline;
begin
 case mode of
  vwZero:Result:=False;
  vwOne :Result:=True;
  vwR64 :Result:=(rsize=os64);
  vwM64 :Result:=(msize=os64);
 end;
end;

function get_vex_rexw(const desc:t_op_type;rsize,msize:TOperandSize):Boolean; inline;
begin
 Result:=get_vex_rexw(desc.vw_mode,rsize,msize);
end;

////

Procedure LinkLabel(node:p_jit_instruction); forward;

procedure t_jit_i_link.set_label(link:t_jit_i_link);
begin
 if (ALink=nil) then Exit;
 p_jit_instruction(ALink)^.ALink.ALink:=link.ALink;
 p_jit_instruction(ALink)^.ALink.AType:=link.AType;
 LinkLabel(ALink);
end;

function t_jit_i_link.get_label():t_jit_i_link;
begin
 Result:=Default(t_jit_i_link);
 if (ALink=nil) then Exit;
 Result.ALink:=p_jit_instruction(ALink)^.ALink.ALink;
 Result.AType:=p_jit_instruction(ALink)^.ALink.AType;
end;

function t_jit_i_link.is_valid:Boolean;
begin
 Result:=(ALink<>nil);
end;

function _get_link_offset(AType:t_jit_link_type;ALink:Pointer):Integer;
begin
 Result:=0;
 if (ALink<>nil) then
 begin
  case AType of
   lnkData:
    begin
     Result:=p_jit_data(ALink)^.pId*SizeOf(Pointer);
    end;
   lnkPlt:
    begin
     Result:=QWORD(ALink)*SizeOf(t_jit_plt);
    end;
   lnkLabelBefore:
    begin
     Result:=p_jit_instruction(ALink)^.AInstructionOffset;
    end;
   lnkLabelAfter:
    begin
     Result:=p_jit_instruction(ALink)^.AInstructionEnd;
    end;
   else;
  end;
 end;
end;

function t_jit_i_link.offset:Integer;
begin
 Result:=_get_link_offset(AType,ALink);
end;

function t_jit_i_link.before:t_jit_i_link;
begin
 Result:=Self;
 case AType of
  lnkLabelBefore,
  lnkLabelAfter:
   begin
    Result.AType:=lnkLabelBefore;
   end;
  else;
 end;
end;

function t_jit_i_link.after:t_jit_i_link;
begin
 Result:=Self;
 case AType of
  lnkLabelBefore,
  lnkLabelAfter:
   begin
    Result.AType:=lnkLabelAfter;
   end;
  else;
 end;
end;

function t_jit_i_link.prev:t_jit_i_link;
begin
 Result:=nil_link;
 case AType of
  lnkData:
   begin
    Result.AType:=lnkData;
    Result.ALink:=TAILQ_PREV(ALink,@p_jit_data(ALink)^.entry);
   end;
  lnkPlt:
   begin
    if (ALink<>nil) then
    begin
     Dec(QWORD(ALink));
    end;
   end;
  lnkLabelBefore,
  lnkLabelAfter:
   begin
    Result.AType:=lnkLabelBefore;
    Result.ALink:=TAILQ_PREV(ALink,@p_jit_instruction(ALink)^.entry);
   end;
  else;
 end;
 if (Result.ALink=nil) and (AType<>lnkPlt) then
 begin
  Result:=nil_link;
 end;
end;

function t_jit_i_link.next:t_jit_i_link;
begin
 Result:=nil_link;
 case AType of
  lnkData:
   begin
    Result.AType:=lnkData;
    Result.ALink:=TAILQ_NEXT(ALink,@p_jit_data(ALink)^.entry);
   end;
  lnkPlt:
   begin
    if (ALink<>nil) then
    begin
     Dec(QWORD(ALink));
    end;
   end;
  lnkLabelBefore,
  lnkLabelAfter:
   begin
    Result.AType:=lnkLabelBefore;
    Result.ALink:=TAILQ_NEXT(ALink,@p_jit_instruction(ALink)^.entry);
   end;
  else;
 end;
 if (Result.ALink=nil) and (AType<>lnkPlt) then
 begin
  Result:=nil_link;
 end;
end;

function t_jit_i_link._node:p_jit_instruction;
begin
 Result:=nil;
 if (ALink<>nil) then
 begin
  case AType of
   lnkLabelBefore,
   lnkLabelAfter:
    begin
     Result:=ALink;
    end;
   else;
  end;
 end;
end;

operator = (A,B:t_jit_i_link):Boolean;
begin
 Result:=(A.ALink=B.ALink) and (A.AType=B.AType);
end;

////

function t_jit_instruction.AInstructionEnd:Integer; inline;
begin
 Result:=AInstructionOffset+ASize;
end;

procedure t_jit_instruction.EmitByte(b:byte); inline;
begin
 AData[ASize]:=b;
 Inc(ASize);
end;

procedure t_jit_instruction.EmitWord(w:Word); inline;
begin
 PWord(@AData[ASize])^:=w;
 Inc(ASize,SizeOf(Word));
end;

procedure t_jit_instruction.EmitInt32(i:Integer); inline;
begin
 PInteger(@AData[ASize])^:=i;
 Inc(ASize,SizeOf(Integer));
end;

procedure t_jit_instruction.EmitInt64(i:Int64); inline;
begin
 PInt64(@AData[ASize])^:=i;
 Inc(ASize,SizeOf(Int64));
end;

procedure t_jit_instruction.EmitSelector(Segment:ShortInt); inline;
begin
 case Segment of //Selector
  4:EmitByte($64); //fs
  5:EmitByte($65); //gs
  else;
 end;
end;

procedure t_jit_instruction.EmitREX(rexB,rexX,rexR,rexW:Boolean); inline;
var
 b:Byte;
begin
 b:=$40;
 if rexB then b:=b or 1;
 if rexX then b:=b or 2;
 if rexR then b:=b or 4;
 if rexW then b:=b or 8;
 EmitByte(b);
end;

procedure t_jit_instruction.EmitModRM(Mode,Index,RM:Byte); inline;
var
 b:Byte;
begin
 b:=RM and 7;
 b:=b or ((Index and 7) shl 3);
 b:=b or ((Mode  and 3) shl 6);
 EmitByte(b);
end;

procedure t_jit_instruction.EmitSIB(Scale,Index,Base:Byte); inline;
var
 b:Byte;
begin
 b:=Base and 7;
 b:=b or ((Index and 7) shl 3);
 b:=b or ((Scale and 3) shl 6);
 EmitByte(b);
end;

procedure t_jit_instruction.EmitRvvv(rexR:Boolean;VectorIndex,VectorLength,SimdOpcode:Byte); inline;
var
 b:Byte;
begin
 b:=SimdOpcode and 3;
 if (not rexR) then b:=b or $80;
 b:=b or (((VectorIndex xor $F) and $F) shl 3);
 b:=b or ((VectorLength and 1) shl 2);
 EmitByte(b);
end;

procedure t_jit_instruction.EmitRXBm(rexB,rexX,rexR:Boolean;mm:Byte); inline;
var
 b:Byte;
begin
 b:=mm and $1F;
 if (not rexR) then b:=b or $80;
 if (not rexX) then b:=b or $40;
 if (not rexB) then b:=b or $20;
 EmitByte(b);
end;

procedure t_jit_instruction.EmitWvvv(rexW:Boolean;VectorIndex,VectorLength,SimdOpcode:Byte); inline;
var
 b:Byte;
begin
 b:=SimdOpcode and 3;
 if rexW then b:=b or $80;
 b:=b or (((VectorIndex xor $F) and $F) shl 3);
 b:=b or ((VectorLength and 1) shl 2);
 EmitByte(b);
end;

//

Function t_jit_builder_allocator.Alloc(Size:ptruint):Pointer;
const
 asize=(2*1024*1024)-SizeOf(ptruint)*3;
var
 mem_size:ptruint;
 node:PAllocNode;

 function _alloc:Pointer;
 begin
  if (Size>asize-SizeOf(Pointer)) then
  begin
   Result:=AllocMem(Size+SizeOf(Pointer));
  end else
  begin
   Result:=AllocMem(asize);
  end;
 end;

begin
 if (pHead.slh_first=nil) or (Size>curr_size) then
 begin
  node:=_alloc;
  SLIST_INSERT_HEAD(@pHead,node,@node^.link);

  //Push_head(_alloc);
  mem_size:=MemSize(node);
  curr_apos:=0;
  curr_size:=mem_size-SizeOf(Pointer);
  Inc(full_size,mem_size);
 end;

 node:=SLIST_FIRST(@pHead);

 Result:=@PByte(@node^.data)[curr_apos];

 Inc(used_size,Size);
 Size:=Align(Size,SizeOf(ptruint));
 Inc(curr_apos,Size);
 Dec(curr_size,Size);
end;

Procedure t_jit_builder_allocator.Free;
var
 node:PAllocNode;
begin
 //node:=Pop_head;
 node:=pHead.slh_first;
 if (node<>nil) then
 begin
  pHead.slh_first:=node^.link;
 end;
 While (node<>nil) do
 begin
  FreeMem(node);
  //node:=Pop_head;
  node:=pHead.slh_first;
  if (node<>nil) then
  begin
   pHead.slh_first:=node^.link;
  end;
 end;
 Self:=Default(t_jit_builder_allocator);
end;

Function t_jit_builder.Alloc(Size:ptruint):Pointer;
begin
 Result:=Allocator.Alloc(Size);
end;

Procedure t_jit_builder.Free;
begin
 Allocator.Free;
end;

//

Function t_jit_builder._new_chunk(start:QWORD):p_jit_code_chunk;
var
 node:t_jit_code_chunk;
begin
 Result:=nil;
 node:=Default(t_jit_code_chunk);
 node.start:=start;
 Result:=ACodeChunkSet.Find(@node);
 if (Result=nil) then
 begin
  Result:=Alloc(SizeOf(t_jit_code_chunk));
  Result^.start:=start;
  TAILQ_INIT(@Result^.AInstructions);
  ACodeChunkSet.Insert(Result);
  ACodeChunkCurr:=Result;
 end else
 begin
  Result:=nil;
 end;
end;

procedure t_jit_builder._end_chunk(__end:QWORD);
begin
 if (ACodeChunkCurr<>nil) then
 begin
  ACodeChunkCurr^.__end:=__end;
  ACodeChunkCurr:=nil;
 end;
end;

procedure t_jit_builder._add(const ji:t_jit_instruction);
var
 node:p_jit_instruction;
begin
 if (ACodeChunkCurr=nil) then
 begin
  _new_chunk(0);
  Assert(ACodeChunkCurr<>nil)
 end;

 node:=Alloc(SizeOf(t_jit_instruction));
 node^:=ji;
 node^.AInstructionOffset:=AInstructionSize;

 TAILQ_INSERT_TAIL(@ACodeChunkCurr^.AInstructions,node,@node^.entry);
 Inc(AInstructionSize,ji.ASize);
end;

Function t_jit_builder.get_curr_label:t_jit_i_link;
var
 node:p_jit_instruction;
begin
 if (ACodeChunkCurr=nil) then
 begin
  _add(Default(t_jit_instruction));
 end;

 node:=TAILQ_LAST(@ACodeChunkCurr^.AInstructions);

 if (node=nil) then
 begin
  _add(Default(t_jit_instruction));
  node:=TAILQ_LAST(@ACodeChunkCurr^.AInstructions);
 end;

 Result.AType:=lnkLabelBefore;
 Result.ALink:=node;
end;

Function t_jit_builder._add_data(P:Pointer):p_jit_data;
var
 node:t_jit_data;
begin
 Result:=nil;
 node:=Default(t_jit_data);
 node.pData:=p;
 Result:=ADataSet.Find(@node);
 if (Result=nil) then
 begin
  Result:=Alloc(SizeOf(t_jit_data));
  Result^.pData:=P;
  Result^.pId:=ADataCount;
  ADataSet.Insert(Result);
  if (ADataList.tqh_first=nil) and
     (ADataList.tqh_last=nil) then
  begin
   TAILQ_INIT(@ADataList);
  end;
  TAILQ_INSERT_TAIL(@ADataList,Result,@Result^.entry);
  Inc(ADataCount);
 end;
end;

Function t_jit_builder._add_plt:Integer;
begin
 Result:=APltCount;
 Inc(APltCount);
end;

function t_jit_builder._get_base_offset(AType:t_jit_link_type):Integer;
begin
 Result:=0;
 case AType of
  lnkData:Result:=AInstructionSize;
  lnkPlt :Result:=AInstructionSize+GetDataSize;
  else;
 end;
end;

Function t_jit_builder.last_instruction:p_jit_instruction;
begin
 Result:=TAILQ_LAST(@ACodeChunkCurr^.AInstructions);
end;

Function t_jit_builder.call_far(P:Pointer):t_jit_i_link;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($FF);
 ji.EmitByte($15);

 ji.ALink.AType  :=lnkData;
 ji.ALink.ASize  :=4;
 ji.ALink.AOffset:=ji.ASize;
 ji.ALink.ALink  :=_add_data(P);

 ji.EmitInt32(0);

 _add(ji);

 Result.ALink:=last_instruction;
 Result.AType:=lnkLabelBefore;
 LinkLabel(Result.ALink);
end;

Function t_jit_builder.jmp_far(P:Pointer):t_jit_i_link;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($FF);
 ji.EmitByte($25);

 ji.ALink.AType  :=lnkData;
 ji.ALink.ASize  :=4;
 ji.ALink.AOffset:=ji.ASize;
 ji.ALink.ALink  :=_add_data(P);

 ji.EmitInt32(0);

 _add(ji);

 Result.ALink:=last_instruction;
 Result.AType:=lnkLabelBefore;
 LinkLabel(Result.ALink);
end;

function t_jit_builder.call(_label_id:t_jit_i_link):t_jit_i_link;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($E8);

 ji.ALink.AType  :=_label_id.AType;
 ji.ALink.ASize  :=4;
 ji.ALink.AOffset:=ji.ASize;
 ji.ALink.ALink  :=_label_id.ALink;

 ji.EmitInt32(0);

 _add(ji);

 Result.ALink:=last_instruction;
 Result.AType:=lnkLabelBefore;
 LinkLabel(Result.ALink);
end;

function t_jit_builder.jmp(_label_id:t_jit_i_link;size:TOperandSize=os32):t_jit_i_link;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 if (size=os8) then
 begin
  ji.EmitByte($EB);

  ji.ALink.AType  :=_label_id.AType;
  ji.ALink.ASize  :=1;
  ji.ALink.AOffset:=ji.ASize;
  ji.ALink.ALink  :=_label_id.ALink;

  ji.EmitByte(0);
 end else
 begin
  ji.EmitByte($E9);

  ji.ALink.AType  :=_label_id.AType;
  ji.ALink.ASize  :=4;
  ji.ALink.AOffset:=ji.ASize;
  ji.ALink.ALink  :=_label_id.ALink;

  ji.EmitInt32(0);
 end;

 _add(ji);

 Result.ALink:=last_instruction;
 Result.AType:=lnkLabelBefore;
 LinkLabel(Result.ALink);
end;

const
 COND_8:array[OPSc_o..OPSc_nle] of Byte=(
  $70,$71,$72,$73,$74,$75,$76,$77,
  $78,$79,$7A,$7B,$7C,$7D,$7E,$7F
 );

 COND_32:array[OPSc_o..OPSc_nle] of Byte=(
  $80,$81,$82,$83,$84,$85,$86,$87,
  $88,$89,$8A,$8B,$8C,$8D,$8E,$8F
 );

function t_jit_builder.jcc(op:TOpCodeSuffix;_label_id:t_jit_i_link;size:TOperandSize=os32):t_jit_i_link;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 case op of
  OPSc_o..OPSc_nle:;
  else
   Assert(false);
 end;

 if (size=os8) then
 begin
  ji.EmitByte(COND_8[op]);

  ji.ALink.AType  :=_label_id.AType;
  ji.ALink.ASize  :=1;
  ji.ALink.AOffset:=ji.ASize;
  ji.ALink.ALink  :=_label_id.ALink;

  ji.EmitByte(0);
 end else
 begin
  ji.EmitByte($0F);
  ji.EmitByte(COND_32[op]);

  ji.ALink.AType  :=_label_id.AType;
  ji.ALink.ASize  :=4;
  ji.ALink.AOffset:=ji.ASize;
  ji.ALink.ALink  :=_label_id.ALink;

  ji.EmitInt32(0);
 end;

 _add(ji);

 Result.ALink:=last_instruction;
 Result.AType:=lnkLabelBefore;

 LinkLabel(Result.ALink);
end;

function t_jit_builder.loop(op:TOpCodeSuffix;_label_id:t_jit_i_link;size:TAddressSize):t_jit_i_link;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 if (size=as32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 case op of
  OPSnone:ji.EmitByte($E2);
  OPSc_ne:ji.EmitByte($E0);
  OPSc_e :ji.EmitByte($E1);
  else
   Assert(false);
 end;

 ji.ALink.AType  :=_label_id.AType;
 ji.ALink.ASize  :=1;
 ji.ALink.AOffset:=ji.ASize;
 ji.ALink.ALink  :=_label_id.ALink;

 ji.EmitByte(0);

 _add(ji);

 Result.ALink:=last_instruction;
 Result.AType:=lnkLabelBefore;

 LinkLabel(Result.ALink);
end;

function t_jit_builder.jcxz(_label_id:t_jit_i_link;size:TAddressSize):t_jit_i_link;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 if (size=as32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 ji.EmitByte($E3);

 ji.ALink.AType  :=_label_id.AType;
 ji.ALink.ASize  :=1;
 ji.ALink.AOffset:=ji.ASize;
 ji.ALink.ALink  :=_label_id.ALink;

 ji.EmitByte(0);

 _add(ji);

 Result.ALink:=last_instruction;
 Result.AType:=lnkLabelBefore;

 LinkLabel(Result.ALink);
end;

function t_jit_builder.movj(reg:TRegValue;mem:t_jit_leas;_label_id:t_jit_i_link):t_jit_i_link;
begin
 movq(reg,mem);

 Result.ALink:=last_instruction;

 p_jit_instruction(Result.ALink)^.ALink.AType:=_label_id.AType;
 p_jit_instruction(Result.ALink)^.ALink.ALink:=_label_id.ALink;
 Result.AType:=lnkLabelBefore;

 LinkLabel(Result.ALink);
end;

function t_jit_builder.leaj(reg:TRegValue;mem:t_jit_leas;_label_id:t_jit_i_link):t_jit_i_link;
begin
 leaq(reg,mem);

 Result.ALink:=last_instruction;

 p_jit_instruction(Result.ALink)^.ALink.AType:=_label_id.AType;
 p_jit_instruction(Result.ALink)^.ALink.ALink:=_label_id.ALink;
 Result.AType:=lnkLabelBefore;

 LinkLabel(Result.ALink);
end;

function t_jit_builder.leap(reg:TRegValue):t_jit_i_link;
begin
 leaq(reg,[rip+$7FFFFFFF]);

 Result.ALink:=last_instruction;

 p_jit_instruction(Result.ALink)^.ALink.AType:=lnkPlt;
 p_jit_instruction(Result.ALink)^.ALink.ALink:=Pointer(_add_plt);
 Result.AType:=lnkLabelBefore;

 LinkLabel(Result.ALink);
end;

Procedure t_jit_builder.jmp(reg:TRegValue);
const
 desc:t_op_type=(op:$FF;index:4);
begin
 Assert(is_reg_size(reg,[os64]));

 _R(desc,reg);
end;

Procedure t_jit_builder.call(reg:TRegValue);
const
 desc:t_op_type=(op:$FF;index:2);
begin
 Assert(is_reg_size(reg,[os64]));

 _R(desc,reg);
end;

Procedure t_jit_builder.reta;
begin
 _O($C3);
end;

Procedure t_jit_builder.ud2;
begin
 _O($0F0B);
end;

Function t_jit_builder.GetInstructionsSize:Integer;
begin
 Result:=AInstructionSize;
end;

Function t_jit_builder.GetDataSize:Integer;
begin
 Result:=ADataCount*SizeOf(Pointer);
end;

Function t_jit_builder.GetPltSize:Integer;
begin
 Result:=APltCount*SizeOf(t_jit_plt);
end;

Function t_jit_builder.GetPltStart:Integer;
begin
 Result:=AInstructionSize+GetDataSize;
end;

Function t_jit_builder.GetMemSize:Integer;
begin
 Result:=AInstructionSize+GetDataSize+GetPltSize;
end;

Procedure t_jit_builder.RebuldChunkList;
var
 chunk:p_jit_code_chunk;
begin
 ACodeChunkCurr:=nil;

 TAILQ_INIT(@ACodeChunkList);

 chunk:=ACodeChunkSet.Min;

 while (chunk<>nil) do
 begin
  TAILQ_INSERT_TAIL(@ACodeChunkList,chunk,@chunk^.entry);

  chunk:=ACodeChunkSet.Next(chunk);
 end;
end;

Procedure t_jit_builder.RebuldInstructionOffset;
var
 chunk:p_jit_code_chunk;
 node:p_jit_instruction;
begin
 AInstructionSize:=0;

 chunk:=TAILQ_FIRST(@ACodeChunkList);

 while (chunk<>nil) do
 begin
  node:=TAILQ_FIRST(@chunk^.AInstructions);
  //
  while (node<>nil) do
  begin
   node^.AInstructionOffset:=AInstructionSize;
   Inc(AInstructionSize,node^.ASize);
   //
   node:=TAILQ_NEXT(node,@node^.entry);
  end;
  //
  chunk:=TAILQ_NEXT(chunk,@chunk^.entry);
 end;
end;

procedure _set_data(node:p_jit_instruction;d:Integer); inline;
begin
 With node^ do
  Move(d,node^.AData[ALink.AOffset],ASize);
end;

Procedure LinkLabel(node:p_jit_instruction);
var
 d:Integer;
begin
 //Pre-linking, for debugging only
 d:=0;
 if (node=nil) then Exit;
 if (node^.ALink.ALink=nil) and
    (node^.ALink.AType<>lnkPlt) then Exit;
 With node^ do
  case ALink.AType of
   lnkData:
     begin
      d:=_get_link_offset(ALink.AType,ALink.ALink);
      _set_data(node,d);
     end;
   lnkPlt:
     begin
      d:=_get_link_offset(ALink.AType,ALink.ALink);
      _set_data(node,d);
     end;
   lnkLabelBefore,
   lnkLabelAfter:
     begin
      d:=_get_link_offset(ALink.AType,ALink.ALink);
      d:=d-AInstructionEnd;
      _set_data(node,d);
     end;
   else;
  end;
end;

function is_8bit_offset(d:Integer):Boolean; inline;
begin
 case d of
  -128..127:Result:=True;
  else
              Result:=False;
 end;
end;

function classif_instr(node:p_jit_instruction):Byte; inline;
begin
 Result:=0;
 case node^.AData[0] of
       $EB:Result:=1; //jmp8
       $E9:Result:=2; //jmp32
  $70..$7F:Result:=3; //jcc8
       $0F:
           case node^.AData[1] of
            $80..$8F:Result:=4; //jcc32
            else;
           end;
  else;
 end;
end;

Procedure t_jit_builder.LinkData;
label
 _start;
var
 chunk:p_jit_code_chunk;
 node:p_jit_instruction;
 d,t:Integer;

 is_change:Boolean;
begin

 _start:
 is_change:=False;

 d:=0;

 chunk:=TAILQ_FIRST(@ACodeChunkList);

 while (chunk<>nil) do
 begin
  node:=TAILQ_FIRST(@chunk^.AInstructions);
  //
  while (node<>nil) do
  begin
   With node^ do
    case ALink.AType of
     lnkData,
     lnkPlt :
       if not is_change then
       begin
        d:=_get_link_offset(ALink.AType,ALink.ALink);
        d:=d+_get_base_offset(ALink.AType);
        d:=d-AInstructionEnd;
        _set_data(node,d);
       end;
     lnkLabelBefore,
     lnkLabelAfter:
      begin
       d:=_get_link_offset(ALink.AType,ALink.ALink);
       d:=d+_get_base_offset(ALink.AType);
       d:=d-AInstructionEnd;

       t:=classif_instr(node);

       if (t<>0) then
       begin
        if (d=0) then
        begin
         //clear instr

         ALink.AType:=lnkNone;
         ASize:=0;

         is_change:=True;
        end;
       end;

       if (ASize<>0) then
       case t of
        2:if is_8bit_offset(d) then //jmp32
          begin
           //set to jmp8

           AData[0]:=$EB;
           ASize:=2;

           ALink.ASize:=1;

           is_change:=True;
          end;
        4:if is_8bit_offset(d) then //jcc32
          begin
           t:=node^.AData[1] and $F;

           AData[0]:=$70 or t;
           ASize:=2;

           ALink.ASize:=1;
           ALink.AOffset:=1;

           is_change:=True;
          end;
        else;
       end;

       if not is_change then
       begin
        _set_data(node,d);
       end;

      end;
     else;
    end;
   //
   node:=TAILQ_NEXT(node,@node^.entry);
  end;
  //
  chunk:=TAILQ_NEXT(chunk,@chunk^.entry);
 end;

 if is_change then
 begin
  RebuldInstructionOffset;
  goto _start;
 end;

end;

Function t_jit_builder.CopyChunks(var rec:t_jit_copy_ptr):Boolean;
var
 chunk:p_jit_code_chunk;
 node_code:p_jit_instruction;
 s:Integer;
begin
 Result:=True;

 chunk:=TAILQ_FIRST(@ACodeChunkList);

 while (chunk<>nil) do
 begin
  node_code:=TAILQ_FIRST(@chunk^.AInstructions);
  //
  while (node_code<>nil) do
  begin
   s:=node_code^.ASize;
   if ((rec.pos+s)>rec.size) then
   begin
    Exit(False);
   end;
   Move(node_code^.AData,rec.ptr^,s);
   Inc(rec.pos,s);
   Inc(rec.ptr,s);
   //
   node_code:=TAILQ_NEXT(node_code,@node_code^.entry);
  end;
  //
  chunk:=TAILQ_NEXT(chunk,@chunk^.entry);
 end;
end;

Function t_jit_builder.CopyData(var rec:t_jit_copy_ptr):Boolean;
var
 node_data:p_jit_data;
 s:Integer;
begin
 Result:=True;

 node_data:=TAILQ_FIRST(@ADataList);

 while (node_data<>nil) do
 begin
  s:=SizeOf(Pointer);
  if ((rec.pos+s)>rec.size) then
  begin
   Exit(False);
  end;
  Move(node_data^.pData,rec.ptr^,s);
  Inc(rec.pos,s);
  Inc(rec.ptr,s);
  //
  node_data:=TAILQ_NEXT(node_data,@node_data^.entry);
 end;

end;

Function t_jit_builder.CopyPlt(var rec:t_jit_copy_ptr):Boolean;
var
 s:Integer;
begin
 Result:=True;

 s:=GetPltSize;
 if (s<>0) then
 begin
  if (s>rec.size) then s:=rec.size;

  FillChar(rec.ptr^,s,0);

  Inc(rec.pos,s);
  Inc(rec.ptr,s);
 end;
end;

Function t_jit_builder.SaveTo(ptr:PByte;size:Integer):Integer;
var
 rec:t_jit_copy_ptr;
begin
 RebuldChunkList;
 RebuldInstructionOffset;
 LinkData;

 Result:=0;

 rec.ptr :=ptr;
 rec.size:=size;
 rec.pos :=0;

 if not CopyChunks(rec) then
 begin
  Exit(rec.pos);
 end;

 if not CopyData(rec) then
 begin
  Exit(rec.pos);
 end;

 CopyPlt(rec);

 Exit(rec.pos);
end;

type
 t_modrm_info=object
  Mem,RH,rexF:Boolean;

  rexB,rexX,rexR:Boolean;

  ModRM:record
   Mode,Index,RM:Byte;
  end;

  SIB:record
   Scale,Index,Base:Byte;
  end;

  AOffset:Int64;

  procedure build_im(Index:Byte;const mreg:t_jit_lea);
  procedure build_rm(const reg:TRegValue;const mreg:t_jit_lea);
  procedure build_rr(const reg0,reg1:TRegValue);
  procedure build_ir(Index:Byte;const reg:TRegValue);
  procedure emit_rex(var ji:t_jit_instruction;rexW:Boolean);
  procedure emit_gop(var ji:t_jit_instruction;rexW:Boolean;op:DWORD);
  procedure emit_mrm(var ji:t_jit_instruction);
 end;

procedure t_modrm_info.build_im(Index:Byte;const mreg:t_jit_lea);
var
 ubase:Boolean;
begin
 Mem:=True;

 ModRM.Index:=Index;

 if not is_valid_reg_type(mreg.ARegValue[0]) then
 begin
  ModRM.Mode:=0;
  ModRM.RM  :=4;

  sib.Scale:=0;
  sib.Index:=4;
  sib.Base :=5;
 end else
 if (mreg.ARegValue[0].AType=regRip) then
 begin
  if is_valid_reg_type(mreg.ARegValue[1]) then
  begin
   Assert(false,'Impossible');
   Exit;
  end;

  if (mreg.ARegValue[0].AScale>1) then
  begin
   Assert(false,'Impossible');
   Exit;
  end;

  ModRM.Mode :=0;
  ModRM.RM   :=5;
 end else
 begin
  ModRM.Mode:=classif_offset_32(mreg);

  ubase:=is_valid_reg_type(mreg.ARegValue[1]);

  if (mreg.ARegValue[0].AScale>1) or ubase then
  begin
   //sib
   ModRM.RM:=4;

   SIB.Scale:=get_scale_idx(mreg.ARegValue[0]);

   SIB.Index:=mreg.ARegValue[0].AIndex;
   if (SIB.Index>=8) then
   begin
    if RH then
    begin
     Assert(false,'Impossible');
    end;
    rexX:=true;
    Dec(SIB.Index,8);
   end;

   if ubase then
   begin
    SIB.Base:=mreg.ARegValue[1].AIndex;
    if (SIB.Base>=8) then
    begin
     if RH then
     begin
      Assert(false,'Impossible');
     end;
     rexB:=true;
     Dec(SIB.Base,8);
    end;
   end else
   begin
    SIB.Base  :=5; //no base
    ModRM.Mode:=0; //exception
   end;

   if (not rexX) and (SIB.Index=4) then //exception
   begin
    //swap
    if (not rexB) and (SIB.Base=4) then
    begin
     Assert(false,'Impossible');
     Exit;
    end;

    if not ubase then
    begin
     Assert(false,'Impossible');
     Exit;
    end;

    rexX     :=rexB;
    SIB.Index:=SIB.Base;

    rexB    :=false;
    SIB.Base:=4;
   end;

   if ubase and
      (ModRM.Mode=0) and
      (SIB.Base=5) then //exception
   begin
    ModRM.Mode:=1;
   end;

  end else
  begin
   //no sib

   ModRM.RM:=mreg.ARegValue[0].AIndex;
   if (ModRM.RM>=8) then
   begin
    if RH then
    begin
     Assert(false,'Impossible');
    end;
    rexB:=true;
    Dec(ModRM.RM,8);
   end;

   if (ModRM.Mode=0) and (ModRM.RM=5) then //exception
   begin
    ModRM.Mode:=1;
   end;

   if (ModRM.RM=4) then //force sib
   begin
    SIB.Scale:=0;
    SIB.Index:=4; //no index (rexX=false)
    SIB.Base :=4; //=ModRM.RM
   end;

  end;
 end;

 AOffset:=mreg.AOffset;
end;

function get_force_rex(const reg:TRegValue):Boolean; inline;
begin
 Result:=False;
 if (reg.AType=regGeneral) then
 if (reg.ASize=os8) then
  case reg.AIndex of
   4..7:Result:=True;
   else;
  end;
end;

procedure t_modrm_info.build_rm(const reg:TRegValue;const mreg:t_jit_lea);
begin
 Mem:=True;

 ModRM.Index:=reg.AIndex;

 if (reg.AType=regGeneralH) then
 begin
  RH:=True;
  Inc(ModRM.Index,4);
 end else
 if (ModRM.Index>=8) then
 begin
  rexR:=true;
  Dec(ModRM.Index,8);
 end;

 rexF:=get_force_rex(reg);

 build_im(ModRM.Index,mreg);
end;

procedure t_modrm_info.build_rr(const reg0,reg1:TRegValue);
begin
 ModRM.Index:=reg1.AIndex;

 if (reg1.AType=regGeneralH) then
 begin
  if (reg0.AType=regGeneral) then
  begin
   Assert(reg0.AIndex<=4,'Impossible');
  end;
  RH:=True;
  Inc(ModRM.Index,4);
 end else
 if (ModRM.Index>=8) then
 begin
  rexR:=true;
  Dec(ModRM.Index,8);
 end;

 rexF:=get_force_rex(reg1);

 ModRM.RM:=reg0.AIndex;

 if (reg0.AType=regGeneralH) then
 begin
  if (reg1.AType=regGeneral) then
  begin
   Assert(reg1.AIndex<=4,'Impossible');
  end;
  if rexF or rexR then
  begin
   Assert(false,'Impossible');
  end;
  RH:=True;
  Inc(ModRM.RM,4)
 end else
 if (ModRM.RM>=8) then
 begin
  if RH then
  begin
   Assert(false,'Impossible');
  end;
  rexB:=true;
  Dec(ModRM.RM,8);
 end;

 ModRM.Mode:=3;
end;

procedure t_modrm_info.build_ir(Index:Byte;const reg:TRegValue);
begin
 ModRM.Index:=Index;

 ModRM.RM:=reg.AIndex;
 if (ModRM.RM>=8) then
 begin
  rexB:=true;
  Dec(ModRM.RM,8);
 end;

 rexF:=get_force_rex(reg);

 ModRM.Mode:=3;
end;

procedure t_modrm_info.emit_rex(var ji:t_jit_instruction;rexW:Boolean);
begin
 if rexF or rexB or rexX or rexR or rexW then
 begin
  ji.EmitREX(rexB,rexX,rexR,rexW);
 end;
end;

procedure t_modrm_info.emit_gop(var ji:t_jit_instruction;rexW:Boolean;op:DWORD);
var
 b_op:array[0..3] of Byte;
begin
 DWORD(b_op):=op;
 //
 case op of
  $00..$FF:
   begin
    emit_rex(ji,rexW);
    ji.EmitByte(b_op[0]);
   end;
  $100..$FFFF:
   begin
    emit_rex(ji,rexW);
    ji.EmitByte(b_op[1]);
    ji.EmitByte(b_op[0]);
   end;
  $10000..$FFFFFF:
   begin
    case b_op[2] of
     $66,
     $F2,
     $F3:
      begin
       ji.EmitByte(b_op[2]);
       emit_rex(ji,rexW);
       ji.EmitByte(b_op[1]);
       ji.EmitByte(b_op[0]);
      end;
     else
      begin
       emit_rex(ji,rexW);
       ji.EmitByte(b_op[2]);
       ji.EmitByte(b_op[1]);
       ji.EmitByte(b_op[0]);
      end;
    end;
   end;
  else
   begin
    case b_op[3] of
     $66,
     $F2,
     $F3:
      begin
       ji.EmitByte(b_op[3]);
       emit_rex(ji,rexW);
       ji.EmitByte(b_op[2]);
       ji.EmitByte(b_op[1]);
       ji.EmitByte(b_op[0]);
      end;
     else
      begin
       emit_rex(ji,rexW);
       ji.EmitByte(b_op[3]);
       ji.EmitByte(b_op[2]);
       ji.EmitByte(b_op[1]);
       ji.EmitByte(b_op[0]);
      end;
    end;
   end;
 end;
end;

procedure t_modrm_info.emit_mrm(var ji:t_jit_instruction);
begin
 ji.EmitModRM(ModRM.Mode,ModRM.Index,ModRM.RM);

 if Mem then
 begin
  if (ModRM.RM=4) then
  begin
   ji.EmitSIB(SIB.Scale,SIB.Index,SIB.Base);
  end;

  case ModRM.Mode of
   0:
   begin
    case ModRM.RM of
     4:if (SIB.Base=5) then
       begin
        ji.ALink.ASize:=4;
        ji.ALink.AOffset:=ji.ASize;
        ji.EmitInt32(AOffset); //4
       end;
     5:begin
        ji.ALink.ASize:=4;
        ji.ALink.AOffset:=ji.ASize;
        ji.EmitInt32(AOffset); //4
       end;
    end;
   end;
   1:begin
      ji.ALink.ASize:=1;
      ji.ALink.AOffset:=ji.ASize;
      ji.EmitByte(AOffset); //1
     end;
   2:begin
      ji.ALink.ASize:=4;
      ji.ALink.AOffset:=ji.ASize;
      ji.EmitInt32(AOffset); //4
     end;
   else;
  end;
 end;

end;

procedure t_jit_builder._RM(const desc:t_op_type;reg:TRegValue;mem:t_jit_leas);
var
 mreg:t_jit_lea;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(is_reg_size(reg,[os8,os16,os32,os64,os128]));
 Assert(is_reg_type(reg,[regGeneral,regGeneralH,regMm,regXmm]));
 Assert(reg.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 if (mreg.AMemSize=os0) then
 begin
  mreg.AMemSize:=reg.ASize;
 end;

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 op:=desc.op;
 case mreg.AMemSize of
   os8:
       if (not (not_os8 in desc.opt)) then
       begin
        Dec(op);
       end;
  os16:
       if (not (not_prefix in desc.opt)) then
       begin
        Prefix:=$66;
       end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rm(reg,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16,128)
 end;

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 modrm_info.emit_gop(ji,rexW,op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._RMI(const desc:t_op_type;reg:TRegValue;mem:t_jit_leas;imm:Int64);
var
 mreg:t_jit_lea;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(is_reg_size(reg,[os8,os16,os32,os64,os128]));
 Assert(is_reg_type(reg,[regGeneral,regGeneralH,regMm,regXmm]));
 Assert(reg.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 if (mreg.AMemSize=os0) then
 begin
  mreg.AMemSize:=reg.ASize;
 end;

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 op:=desc.op;
 case mreg.AMemSize of
   os8:
       if (not (not_os8 in desc.opt)) then
       begin
        Dec(op);
       end;
  os16:
       if (not (not_prefix in desc.opt)) then
       begin
        Prefix:=$66;
       end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rm(reg,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16,128)
 end;

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 modrm_info.emit_gop(ji,rexW,op);

 modrm_info.emit_mrm(ji);

 case reg.ASize of
   os8:ji.EmitByte (imm);
  os16:ji.EmitWord (imm);
  os32:ji.EmitInt32(imm);
  os64:ji.EmitInt32(imm);
  else;
 end;

 _add(ji);
end;

procedure t_jit_builder._RMI8(const desc:t_op_type;reg:TRegValue;mem:t_jit_leas;imm:Byte);
var
 mreg:t_jit_lea;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(is_reg_size(reg,[os8,os16,os32,os64,os128]));
 Assert(is_reg_type(reg,[regGeneral,regGeneralH,regMm,regXmm]));
 Assert(reg.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 if (mreg.AMemSize=os0) then
 begin
  mreg.AMemSize:=reg.ASize;
 end;

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 op:=desc.op;
 case mreg.AMemSize of
  os8:
      if (not (not_os8 in desc.opt)) then
      begin
       Dec(op);
      end;
  os16:
       if (not (not_prefix in desc.opt)) then
       begin
        Prefix:=$66;
       end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rm(reg,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16,128)
 end;

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 modrm_info.emit_gop(ji,rexW,op);

 modrm_info.emit_mrm(ji);

 ji.EmitByte(imm);

 _add(ji);
end;

procedure t_jit_builder._RR(const desc:t_op_type;reg0,reg1:TRegValue;size:TOperandSize=os0);
var
 modrm_info:t_modrm_info;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));

 Assert(is_reg_size(reg0,[os8,os16,os32,os64,os128]));
 Assert(is_reg_size(reg1,[os8,os16,os32,os64,os128]));

 Assert(is_reg_type(reg0,[regGeneral,regGeneralH,regMm,regXmm]));
 Assert(is_reg_type(reg1,[regGeneral,regGeneralH,regMm,regXmm]));

 Assert(reg0.AScale<=1);
 Assert(reg1.AScale<=1);

 if (size=os0) then
 begin
  Assert(reg0.ASize=reg1.ASize);
  size:=reg0.ASize;
 end;

 ji:=default_jit_instruction;

 rexW:=false;
 Prefix:=0;

 op:=desc.op;
 case size of
   os8:
       if (not (not_os8 in desc.opt)) then
       begin
        Dec(op);
       end;
  os16:
       if (not (not_prefix in desc.opt)) then
       begin
        Prefix:=$66;
       end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rr(reg0,reg1);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16,128)
 end;

 modrm_info.emit_gop(ji,rexW,op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

////

procedure t_jit_builder._RRI(const desc:t_op_type;reg0,reg1:TRegValue;imm:Int64;size:TOperandSize=os0);
var
 modrm_info:t_modrm_info;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));

 Assert(is_reg_size(reg0,[os8,os16,os32,os64,os128]));
 Assert(is_reg_size(reg1,[os8,os16,os32,os64,os128]));

 Assert(is_reg_type(reg0,[regGeneral,regGeneralH,regMm,regXmm]));
 Assert(is_reg_type(reg1,[regGeneral,regGeneralH,regMm,regXmm]));

 Assert(reg0.AScale<=1);
 Assert(reg1.AScale<=1);

 if (size=os0) then
 begin
  Assert(reg0.ASize=reg1.ASize);
  size:=reg0.ASize;
 end;

 ji:=default_jit_instruction;

 rexW:=false;
 Prefix:=0;

 op:=desc.op;
 case size of
   os8:
       if (not (not_os8 in desc.opt)) then
       begin
        Dec(op);
       end;
  os16:
       if (not (not_prefix in desc.opt)) then
       begin
        Prefix:=$66;
       end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rr(reg0,reg1);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16,128)
 end;

 modrm_info.emit_gop(ji,rexW,op);

 modrm_info.emit_mrm(ji);

 case size of
   os8:ji.EmitByte (imm);
  os16:ji.EmitWord (imm);
  os32:ji.EmitInt32(imm);
  os64:ji.EmitInt32(imm);
  else;
 end;

 _add(ji);
end;

procedure t_jit_builder._RRI8(const desc:t_op_type;reg0,reg1:TRegValue;imm:Byte;size:TOperandSize=os0);
var
 modrm_info:t_modrm_info;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));

 Assert(is_reg_size(reg0,[os8,os16,os32,os64,os128]));
 Assert(is_reg_size(reg1,[os8,os16,os32,os64,os128]));

 Assert(is_reg_type(reg0,[regGeneral,regGeneralH,regMm,regXmm]));
 Assert(is_reg_type(reg1,[regGeneral,regGeneralH,regMm,regXmm]));

 Assert(reg0.AScale<=1);
 Assert(reg1.AScale<=1);

 if (size=os0) then
 begin
  Assert(reg0.ASize=reg1.ASize);
  size:=reg0.ASize;
 end;

 ji:=default_jit_instruction;

 rexW:=false;
 Prefix:=0;

 op:=desc.op;
 case size of
  os8:
      if (not (not_os8 in desc.opt)) then
      begin
       Dec(op);
      end;
  os16:
       if (not (not_prefix in desc.opt)) then
       begin
        Prefix:=$66;
       end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rr(reg0,reg1);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16,128)
 end;

 modrm_info.emit_gop(ji,rexW,op);

 modrm_info.emit_mrm(ji);

 ji.EmitByte (imm);

 _add(ji);
end;

procedure t_jit_builder._R(const desc:t_op_type;reg:TRegValue);
var
 modrm_info:t_modrm_info;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(is_reg_size(reg,[os8,os16,os32,os64,os128]));
 Assert(is_reg_type(reg,[regGeneral,regGeneralH,regMm,regXmm]));
 Assert(reg.AScale<=1);

 ji:=default_jit_instruction;

 rexW:=false;
 Prefix:=0;

 op:=desc.op;
 case reg.ASize of
   os8:
       if (not (not_os8 in desc.opt)) then
       begin
        Dec(op);
       end;
  os16:
       if (not (not_prefix in desc.opt)) then
       begin
        Prefix:=$66;
       end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_ir(desc.index,reg);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16,128)
 end;

 modrm_info.emit_gop(ji,rexW,op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

///

procedure t_jit_builder._O(op:DWORD;Size:TOperandSize=os0;opt:t_op_opt=[]);
var
 rexW:Boolean;
 Prefix:Byte;

 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 case Size of
   os8:
       if (not (not_os8 in opt)) then
       begin
        Dec(op);
       end;
  os16:
       if (not (not_prefix in opt)) then
       begin
        Prefix:=$66;
       end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16,128)
 end;

 if rexW then
 begin
  ji.EmitREX(False,False,False,rexW);
 end;

 case op of
  $00..$FF:
   begin
    ji.EmitByte(Byte(op));
   end;
  $100..$FFFF:
   begin
    ji.EmitByte(Hi(Lo(op)));
    ji.EmitByte(Lo(Lo(op)));
   end;
  else
   begin
    ji.EmitByte(Lo(Hi(op)));
    ji.EmitByte(Hi(Lo(op)));
    ji.EmitByte(Lo(Lo(op)));
   end;
 end;

 _add(ji);
end;

procedure t_jit_builder._O(const desc:t_op_type;reg:TRegValue);
var
 op:DWORD;
 rexW,rexB:Boolean;
 Index:Byte;
 Prefix:Byte;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(is_reg_size(reg,[os16,os32,os64,os128]));
 Assert(is_reg_type(reg,[regGeneral]));
 Assert(reg.AScale<=1);

 ji:=default_jit_instruction;

 rexW:=false;
 rexB:=false;
 Prefix:=0;

 op:=desc.op;
 case reg.ASize of
   os8:
       if (not (not_os8 in desc.opt)) then
       begin
        Dec(op);
       end;
  os16:
       if (not (not_prefix in desc.opt)) then
       begin
        Prefix:=$66;
       end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 Index:=reg.AIndex;
 if (Index>=8) then
 begin
  rexB:=true;
  Dec(Index,8);
 end;

 op:=op+Index;

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16,128)
 end;

 if rexB or rexW then
 begin
  ji.EmitREX(rexB,False,False,rexW);
 end;

 case op of
  $00..$FF:
   begin
    ji.EmitByte(Byte(op));
   end;
  $100..$FFFF:
   begin
    ji.EmitByte(Hi(Lo(op)));
    ji.EmitByte(Lo(Lo(op)));
   end;
  else
   begin
    ji.EmitByte(Lo(Hi(op)));
    ji.EmitByte(Hi(Lo(op)));
    ji.EmitByte(Lo(Lo(op)));
   end;
 end;

 _add(ji);
end;

////

procedure t_jit_builder._M(const desc:t_op_type;mem:t_jit_leas);
var
 mreg:t_jit_lea;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64,os128]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 op:=desc.op;

 if (op=$0FC7) and
    (desc.index=1) then //cmpxchg8b/cmpxchg16b
 begin
  case mreg.AMemSize of
   os128:
        begin
         rexW:=True;
        end;
   else;
  end;
 end else
 begin
  case mreg.AMemSize of
    os8:
        if (not (not_os8 in desc.opt)) then
        begin
         Dec(op);
        end;
   os16:
        if (not (not_prefix in desc.opt)) then
        begin
         Prefix:=$66;
        end;
   os32:;
   os64:
        begin
         rexW:=True;
        end;
   else;
  end;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_im(desc.index,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16,128)
 end;

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 modrm_info.emit_gop(ji,rexW,op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

////

procedure t_jit_builder._RI(const desc:t_op_type;reg:TRegValue;imm:Int64);
var
 modrm_info:t_modrm_info;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(is_reg_size(reg,[os8,os16,os32,os64]));
 Assert(is_reg_type(reg,[regGeneral,regGeneralH,regMm,regXmm]));
 Assert(reg.AScale<=1);

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_ir(desc.index,reg);

 op:=desc.op;
 case reg.ASize of
   os8:
       if (not (not_os8 in desc.opt)) then
       begin
        Dec(op);
       end;
  os16:
       if (not (not_prefix in desc.opt)) then
       begin
        Prefix:=$66;
       end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16)
 end;

 modrm_info.emit_gop(ji,rexW,op);

 modrm_info.emit_mrm(ji);

 case reg.ASize of
   os8:ji.EmitByte (imm);
  os16:ji.EmitWord (imm);
  os32:ji.EmitInt32(imm);
  os64:ji.EmitInt32(imm);
  else;
 end;

 _add(ji);
end;

procedure t_jit_builder._MI(const desc:t_op_type;mem:t_jit_leas;imm:Int64);
var
 mreg:t_jit_lea;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));

 mreg:=Sums(mem);

 Assert(mreg.AMemSize in [os8,os16,os32,os64]);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 op:=desc.op;
 case mreg.AMemSize of
   os8:
       if (not (not_os8 in desc.opt)) then
       begin
        Dec(op);
       end;
  os16:
       if (not (not_prefix in desc.opt)) then
       begin
        Prefix:=$66;
       end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_im(desc.index,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16)
 end;

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 modrm_info.emit_gop(ji,rexW,op);

 modrm_info.emit_mrm(ji);

 case mreg.AMemSize of
   os8:ji.EmitByte (imm);
  os16:ji.EmitWord (imm);
  os32:ji.EmitInt32(imm);
  os64:ji.EmitInt32(imm);
  else;
 end;

 _add(ji);
end;

procedure t_jit_builder._RI8(const desc:t_op_type;reg:TRegValue;imm:Byte);
var
 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(is_reg_size(reg,[os8,os16,os32,os64]));
 Assert(is_reg_type(reg,[regGeneral,regGeneralH,regMm,regXmm]));
 Assert(reg.AScale<=1);

 ji:=default_jit_instruction;

 rexW:=false;
 Prefix:=0;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_ir(desc.index,reg);

 op:=desc.op;
 case reg.ASize of
  os16:
      if (not (not_prefix in desc.opt)) then
      begin
       Prefix:=$66;
      end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16)
 end;

 modrm_info.emit_gop(ji,rexW,op);

 modrm_info.emit_mrm(ji);

 ji.EmitByte(imm);

 _add(ji);
end;

procedure t_jit_builder._MI8(const desc:t_op_type;mem:t_jit_leas;imm:Byte);
var
 mreg:t_jit_lea;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));

 mreg:=Sums(mem);

 Assert(mreg.AMemSize in [os8,os16,os32,os64]);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 op:=desc.op;
 case mreg.AMemSize of
  os16:
      if (not (not_prefix in desc.opt)) then
      begin
       Prefix:=$66;
      end;
  os32:;
  os64:
       begin
        rexW:=True;
       end;
  else;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_im(desc.index,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16)
 end;

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 modrm_info.emit_gop(ji,rexW,op);

 modrm_info.emit_mrm(ji);

 ji.EmitByte(imm); //1

 _add(ji);
end;

///

const
 CMOV_8:array[OPSc_o..OPSc_nle] of Byte=(
  $40,$41,$42,$43,$44,$45,$46,$47,
  $48,$49,$4A,$4B,$4C,$4D,$4E,$4F
 );

procedure t_jit_builder.cmov(op:TOpCodeSuffix;reg:TRegValue;mem:t_jit_leas);
var
 desc:t_op_type;
begin
 case op of
  OPSc_o..OPSc_nle:;
  else
   Assert(false);
 end;

 desc:=Default(t_op_type);
 desc.op:=$0F00 or CMOV_8[op];

 _RM(desc,reg,mem);
end;

procedure t_jit_builder.cmov(op:TOpCodeSuffix;reg0:TRegValue;reg1:TRegValue);
var
 desc:t_op_type;
begin
 case op of
  OPSc_o..OPSc_nle:;
  else
   Assert(false);
 end;

 desc:=Default(t_op_type);
 desc.op:=$0F00 or CMOV_8[op];

 _RR(desc,reg1,reg0,os0);
end;

////

procedure t_jit_builder._push(op,index:Byte;mem:t_jit_leas);
var
 mreg:t_jit_lea;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 mreg:=Sums(mem);

 Assert(mreg.AMemSize in [os16,os64]);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_im(index,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (mreg.AMemSize=os16) then
 begin
  ji.EmitByte($66); //Operand-size override prefix (16)
 end;

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 modrm_info.emit_rex(ji,False);

 ji.EmitByte(op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._push(op:Byte;reg:TRegValue);
var
 rexB:Boolean;

 Index:Byte;

 Prefix:Byte;

 ji:t_jit_instruction;
begin
 Assert(is_reg_size(reg,[os16,os64]));
 Assert(is_reg_type(reg,[regGeneral]));
 Assert(reg.AScale<=1);

 ji:=default_jit_instruction;

 rexB:=false;
 Prefix:=0;

 Index:=reg.AIndex;
 if (Index>=8) then
 begin
  rexB:=true;
  Dec(Index,8);
 end;

 case reg.ASize of
  os16:
       begin
        Prefix:=$66;
        op:=op+Index;
       end;
  os64:
       begin
        op:=op+Index;
       end;
  else;
 end;

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16)
 end;

 if rexB then
 begin
  ji.EmitREX(rexB,False,False,False);
 end;

 ji.EmitByte(op);

 _add(ji);
end;

procedure t_jit_builder._pushi(size:TOperandSize;imm:Integer);
var
 op,Prefix:Byte;

 ji:t_jit_instruction;
begin
 Assert(size in [os8,os16,os32]);

 ji:=default_jit_instruction;

 Prefix:=0;
 op:=$68;

 case size of
  os8 :op:=$6A;
  os16:Prefix:=$66;
  os32:;
  else;
 end;

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16)
 end;

 ji.EmitByte(op);

 case size of
   os8:ji.EmitByte (imm);
  os16:ji.EmitWord (imm);
  os32:ji.EmitInt32(imm);
  else;
 end;

 _add(ji);
end;

//

procedure t_jit_builder.movq(reg0:TRegValue;reg1:TRegValue);
const
 desc:t_op_type=(op:$89;index:0);
begin
 _RR(desc,reg0,reg1,os0);
end;

procedure t_jit_builder.movi(mem:t_jit_leas;imm:Int64);
const
 desc:t_op_type=(op:$C7;index:0);
begin
 _MI(desc,mem,imm);
end;

procedure t_jit_builder.movi(reg:TRegValue;imm:Int64);
const
 desc:t_op_type=(op:$C7;index:0);
begin
 _RI(desc,reg,imm);
end;

procedure t_jit_builder.movi64(reg:TRegValue;imm:Int64);
var
 rexF,rexB,rexW:Boolean;

 Index,Prefix,op:Byte;

 ji:t_jit_instruction;
begin
 Assert(is_reg_size(reg,[os8,os16,os32,os64]));
 Assert(is_reg_type(reg,[regGeneral]));
 Assert(reg.AScale<=1);

 ji:=default_jit_instruction;

 rexF:=false;
 rexB:=false;
 rexW:=false;
 Prefix:=0;

 Index:=reg.AIndex;
 if (Index>=8) then
 begin
  rexB:=true;
  Dec(Index,8);
 end;

 rexF:=get_force_rex(reg);

 case reg.ASize of
   os8:
       begin
        op:=$B0+Index;
       end;
  os16:
       begin
        Prefix:=$66;
        op:=$B8+Index;
       end;
  os32:
       begin
        op:=$B8+Index;
       end;
  os64:
       begin
        rexW:=True;
        op:=$B8+Index;
       end;
  else;
 end;

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16)
 end;

 if rexF or rexB or rexW then
 begin
  ji.EmitREX(rexB,False,False,rexW);
 end;

 ji.EmitByte(op);

 case reg.ASize of
   os8:ji.EmitByte (imm);
  os16:ji.EmitWord (imm);
  os32:ji.EmitInt32(imm);
  os64:ji.EmitInt64(imm);
  else;
 end;

 _add(ji);
end;

procedure t_jit_builder.movq(reg:TRegValue;mem:t_jit_leas);
const
 desc:t_op_type=(op:$8B;index:0);
begin
 _RM(desc,reg,mem); //MOV r64, r/m64
end;

procedure t_jit_builder.movq(mem:t_jit_leas;reg:TRegValue);
const
 desc:t_op_type=(op:$89;index:0);
begin
 _RM(desc,reg,mem); //MOV r/m64, r64
end;

//

procedure t_jit_builder.leaq(reg:TRegValue;mem:t_jit_leas);
const
 desc:t_op_type=(op:$8D;index:0);
begin
 Assert(is_reg_size(reg,[os16,os32,os64]));

 _RM(desc,reg,mem); //LEA r64,m
end;

//

procedure t_jit_builder.addq(mem:t_jit_leas;reg:TRegValue);
const
 desc:t_op_type=(op:$01;index:0);
begin
 _RM(desc,reg,mem); //ADD r/m64, r64
end;

procedure t_jit_builder.addq(reg:TRegValue;mem:t_jit_leas);
const
 desc:t_op_type=(op:$03;index:0);
begin
 _RM(desc,reg,mem); //ADD r64, r/m64
end;

procedure t_jit_builder.addq(reg0:TRegValue;reg1:TRegValue);
const
 desc:t_op_type=(op:$01;index:0);
begin
 _RR(desc,reg0,reg1);
end;

procedure t_jit_builder.addi(reg:TRegValue;imm:Int64);
const
 desc:t_op_type=(op:$81;index:0);
begin
 _RI(desc,reg,imm);
end;

procedure t_jit_builder.addi8(reg:TRegValue;imm:Byte);
const
 desc:t_op_type=(op:$83;index:0);
begin
 _RI8(desc,reg,imm);
end;

procedure t_jit_builder.addi8(mem:t_jit_leas;imm:Byte);
const
 desc:t_op_type=(op:$83;index:0);
begin
 _MI8(desc,mem,imm);
end;

//

procedure t_jit_builder.subq(mem:t_jit_leas;reg:TRegValue);
const
 desc:t_op_type=(op:$29;index:0);
begin
 _RM(desc,reg,mem); //SUB r/m64, r64
end;

procedure t_jit_builder.subq(reg:TRegValue;mem:t_jit_leas);
const
 desc:t_op_type=(op:$2B;index:0);
begin
 _RM(desc,reg,mem); //SUB r64, r/m64
end;

procedure t_jit_builder.subq(reg0:TRegValue;reg1:TRegValue);
const
 desc:t_op_type=(op:$29;index:0);
begin
 _RR(desc,reg0,reg1);
end;

procedure t_jit_builder.subi(reg:TRegValue;imm:Int64);
const
 desc:t_op_type=(op:$81;index:5);
begin
 _RI(desc,reg,imm);
end;

procedure t_jit_builder.subi8(reg:TRegValue;imm:Byte);
const
 desc:t_op_type=(op:$83;index:5);
begin
 _RI8(desc,reg,imm);
end;

procedure t_jit_builder.subi8(mem:t_jit_leas;imm:Byte);
const
 desc:t_op_type=(op:$83;index:5);
begin
 _MI8(desc,mem,imm);
end;

///

procedure t_jit_builder.xorq(reg0:TRegValue;reg1:TRegValue);
const
 desc:t_op_type=(op:$31;index:0);
begin
 _RR(desc,reg0,reg1);
end;

///

procedure t_jit_builder.cmpq(mem:t_jit_leas;reg:TRegValue);
const
 desc:t_op_type=(op:$39;index:0);
begin
 _RM(desc,reg,mem);
end;

procedure t_jit_builder.cmpq(reg:TRegValue;mem:t_jit_leas);
const
 desc:t_op_type=(op:$3B;index:0);
begin
 _RM(desc,reg,mem);
end;

procedure t_jit_builder.cmpq(reg0:TRegValue;reg1:TRegValue);
const
 desc:t_op_type=(op:$39;index:0);
begin
 _RR(desc,reg0,reg1);
end;

procedure t_jit_builder.cmpi(reg:TRegValue;imm:Int64);
const
 desc:t_op_type=(op:$810;index:7);
begin
 _RI(desc,reg,imm);
end;

procedure t_jit_builder.cmpi(mem:t_jit_leas;imm:Int64);
const
 desc:t_op_type=(op:$81;index:7);
begin
 _MI(desc,mem,imm);
end;

procedure t_jit_builder.cmpi8(reg:TRegValue;imm:Byte);
const
 desc:t_op_type=(op:$83;index:7);
begin
 _RI8(desc,reg,imm);
end;

procedure t_jit_builder.cmpi8(mem:t_jit_leas;imm:Byte);
const
 desc:t_op_type=(op:$83;index:7);
begin
 _MI8(desc,mem,imm);
end;

procedure t_jit_builder.xchgq(reg0:TRegValue;reg1:TRegValue);
const
 desc:t_op_type=(op:$87;index:0);
begin
 _RR(desc,reg0,reg1);
end;

///

procedure t_jit_builder.push(mem:t_jit_leas);
begin
 _push($FF,6,mem);
end;

procedure t_jit_builder.push(reg:TRegValue);
begin
 _push($50,reg);
end;

procedure t_jit_builder.push8(imm:Integer);
begin
 _pushi(os8,imm);
end;

procedure t_jit_builder.push16(imm:Integer);
begin
 _pushi(os16,imm);
end;

procedure t_jit_builder.push32(imm:Integer);
begin
 _pushi(os32,imm);
end;

procedure t_jit_builder.pop(mem:t_jit_leas);
begin
 _push($8F,0,mem);
end;

procedure t_jit_builder.pop(reg:TRegValue);
begin
 _push($58,reg);
end;

procedure t_jit_builder.pushfq(size:TOperandSize);
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 if (size=os16) then
 begin
  ji.EmitByte($66);
 end;

 ji.EmitByte($9C);

 _add(ji);
end;

procedure t_jit_builder.popfq(size:TOperandSize);
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 if (size=os16) then
 begin
  ji.EmitByte($66);
 end;

 ji.EmitByte($9D);

 _add(ji);
end;

procedure t_jit_builder._VM(const desc:t_op_type;mem:t_jit_leas);
var
 mreg:t_jit_lea;

 modrm_info:t_modrm_info;

 Vex:record
  rexW  :Boolean;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm<>0);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 Vex.Length:=get_vex_len(desc,mreg);

 if (verif_vex_len in desc.opt) then
 if (Vex.Length<>desc.vx_len) then
 begin
  Assert(false,'verif_vex_len');
 end;

 Vex.rexW:=get_vex_rexw(desc,os0,mreg.AMemSize);

 if (verif_rexw in desc.opt) then
 if (Vex.rexW<>desc.rexw) then
 begin
  Assert(false,'verif_rexw');
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_im(desc.index,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 if Vex.rexW or
    modrm_info.rexB or
    modrm_info.rexX or
    (desc.mm>1) then
 begin
  ji.EmitByte($C4); //VEX3

  ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,desc.mm);
  ji.EmitWvvv(Vex.rexW,0,Vex.Length,desc.simdop);
 end else
 begin
  ji.EmitByte($C5); //VEX2

  ji.EmitRvvv(modrm_info.rexR,0,Vex.Length,desc.simdop);
 end;

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._VM(const desc:t_op_type;reg:TRegValue;mem:t_jit_leas);
var
 mreg:t_jit_lea;

 modrm_info:t_modrm_info;

 Vex:record
  rexW  :Boolean;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm<>0);

 Assert(is_reg_size(reg,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg,[regGeneral,regXmm]));
 Assert(reg.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 Vex.Length:=get_vex_len(desc,reg.ASize);

 if (verif_vex_len in desc.opt) then
 if (Vex.Length<>desc.vx_len) then
 begin
  Assert(false,'verif_vex_len');
 end;

 Vex.rexW:=get_vex_rexw(desc,reg.ASize,mreg.AMemSize);

 if (verif_rexw in desc.opt) then
 if (Vex.rexW<>desc.rexw) then
 begin
  Assert(false,'verif_rexw');
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rm(reg,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 if Vex.rexW or
    modrm_info.rexB or
    modrm_info.rexX or
    (desc.mm>1) then
 begin
  ji.EmitByte($C4); //VEX3

  ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,desc.mm);
  ji.EmitWvvv(Vex.rexW,0,Vex.Length,desc.simdop);
 end else
 begin
  ji.EmitByte($C5); //VEX2

  ji.EmitRvvv(modrm_info.rexR,0,Vex.Length,desc.simdop);
 end;

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._VV(const desc:t_op_type;reg0,reg1:TRegValue;size:TOperandSize);
var
 modrm_info:t_modrm_info;

 Vex:record
  rexW  :Boolean;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm<>0);

 Assert(is_reg_size(reg0,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg0,[regGeneral,regXmm]));
 Assert(reg0.AScale<=1);

 Assert(is_reg_size(reg1,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg1,[regGeneral,regXmm]));
 Assert(reg0.AScale<=1);

 ji:=default_jit_instruction;

 if (size=os0) then
 begin
  size:=reg0.ASize;
 end;

 Vex.Length:=get_vex_len(desc,size);

 if (verif_vex_len in desc.opt) then
 if (Vex.Length<>desc.vx_len) then
 begin
  Assert(false,'verif_vex_len');
 end;

 Vex.rexW:=get_vex_rexw(desc.vw_mode,size,os0);

 if (verif_rexw in desc.opt) then
 if (Vex.rexW<>desc.rexw) then
 begin
  Assert(false,'verif_rexw');
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rr(reg1,reg0);

 if Vex.rexW or
    modrm_info.rexB or
    modrm_info.rexX or
    (desc.mm>1) then
 begin
  ji.EmitByte($C4); //VEX3

  ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,desc.mm);
  ji.EmitWvvv(Vex.rexW,0,Vex.Length,desc.simdop);
 end else
 begin
  ji.EmitByte($C5); //VEX2

  ji.EmitRvvv(modrm_info.rexR,0,Vex.Length,desc.simdop);
 end;

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._VM_F3(const desc:t_op_type;reg:TRegValue;mem:t_jit_leas);
var
 mreg:t_jit_lea;

 modrm_info:t_modrm_info;

 Vex:record
  rexW  :Boolean;
  Index :Byte;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm=2);
 Assert(desc.op=$F3);

 Assert(is_reg_size(reg,[os32,os64,os128,os256]));
 Assert(is_reg_type(reg,[regGeneral,regXmm]));
 Assert(reg.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 Vex.Length:=get_vex_len(desc,mreg);

 if (verif_vex_len in desc.opt) then
 if (Vex.Length<>desc.vx_len) then
 begin
  Assert(false,'verif_vex_len');
 end;

 Vex.rexW:=get_vex_rexw(desc,reg.ASize,mreg.AMemSize);

 if (verif_rexw in desc.opt) then
 if (Vex.rexW<>desc.rexw) then
 begin
  Assert(false,'verif_rexw');
 end;

 Vex.Index:=reg.AIndex;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_im(desc.index,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 ji.EmitByte($C4); //VEX3

 ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,2);
 ji.EmitWvvv(Vex.rexW,Vex.Index,Vex.Length,0);

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._VV_F3(const desc:t_op_type;reg0,reg1:TRegValue;size:TOperandSize);
var
 modrm_info:t_modrm_info;

 Vex:record
  rexW  :Boolean;
  Index :Byte;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm=2);
 Assert(desc.op=$F3);

 Assert(is_reg_size(reg0,[os32,os64,os128,os256]));
 Assert(is_reg_type(reg0,[regGeneral,regXmm]));
 Assert(reg0.AScale<=1);

 Assert(is_reg_size(reg1,[os32,os64,os128,os256]));
 Assert(is_reg_type(reg1,[regGeneral,regXmm]));
 Assert(reg1.AScale<=1);

 ji:=default_jit_instruction;

 if (size=os0) then
 begin
  size:=reg0.ASize;
 end;

 Vex.Length:=get_vex_len(desc,size);

 if (verif_vex_len in desc.opt) then
 if (Vex.Length<>desc.vx_len) then
 begin
  Assert(false,'verif_vex_len');
 end;

 Vex.rexW:=get_vex_rexw(desc.vw_mode,size,os0);

 if (verif_rexw in desc.opt) then
 if (Vex.rexW<>desc.rexw) then
 begin
  Assert(false,'verif_rexw');
 end;

 Vex.Index:=reg0.AIndex;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_ir(desc.index,reg1);

 ji.EmitByte($C4); //VEX3

 ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,2);
 ji.EmitWvvv(Vex.rexW,Vex.Index,Vex.Length,0);

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._VVM(const desc:t_op_type;reg0,reg1:TRegValue;mem:t_jit_leas);
var
 mreg:t_jit_lea;

 modrm_info:t_modrm_info;

 Vex:record
  rexW  :Boolean;
  Index :Byte;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm<>0);

 Assert(is_reg_size(reg0,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg0,[regGeneral,regXmm]));
 Assert(reg0.AScale<=1);

 Assert(is_reg_size(reg1,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg1,[regGeneral,regXmm]));
 Assert(reg1.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 Vex.Length:=get_vex_len(desc,mreg);

 if (verif_vex_len in desc.opt) then
 if (Vex.Length<>desc.vx_len) then
 begin
  Assert(false,'verif_vex_len');
 end;

 Vex.rexW:=get_vex_rexw(desc,reg0.ASize,mreg.AMemSize);

 if (verif_rexw in desc.opt) then
 if (Vex.rexW<>desc.rexw) then
 begin
  Assert(false,'verif_rexw');
 end;

 Vex.Index:=reg1.AIndex;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rm(reg0,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 if Vex.rexW or
    modrm_info.rexB or
    modrm_info.rexX or
    (desc.mm>1) then
 begin
  ji.EmitByte($C4); //VEX3

  ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,desc.mm);
  ji.EmitWvvv(Vex.rexW,Vex.Index,Vex.Length,desc.simdop);
 end else
 begin
  ji.EmitByte($C5); //VEX2

  ji.EmitRvvv(modrm_info.rexR,Vex.Index,Vex.Length,desc.simdop);
 end;

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._VVMI8(const desc:t_op_type;reg0,reg1:TRegValue;mem:t_jit_leas;imm8:Byte);
var
 mreg:t_jit_lea;

 modrm_info:t_modrm_info;

 Vex:record
  rexW  :Boolean;
  Index :Byte;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm<>0);

 Assert(is_reg_size(reg0,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg0,[regGeneral,regXmm]));
 Assert(reg0.AScale<=1);

 Assert(is_reg_size(reg1,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg1,[regGeneral,regXmm]));
 Assert(reg1.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 Vex.Length:=get_vex_len(desc,mreg);

 if (verif_vex_len in desc.opt) then
 if (Vex.Length<>desc.vx_len) then
 begin
  Assert(false,'verif_vex_len');
 end;

 Vex.rexW:=get_vex_rexw(desc,reg0.ASize,mreg.AMemSize);

 if (verif_rexw in desc.opt) then
 if (Vex.rexW<>desc.rexw) then
 begin
  Assert(false,'verif_rexw');
 end;

 Vex.Index:=reg1.AIndex;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rm(reg0,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 if Vex.rexW or
    modrm_info.rexB or
    modrm_info.rexX or
    (desc.mm>1) then
 begin
  ji.EmitByte($C4); //VEX3

  ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,desc.mm);
  ji.EmitWvvv(Vex.rexW,Vex.Index,Vex.Length,desc.simdop);
 end else
 begin
  ji.EmitByte($C5); //VEX2

  ji.EmitRvvv(modrm_info.rexR,Vex.Index,Vex.Length,desc.simdop);
 end;

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 ji.EmitByte(imm8);

 _add(ji);
end;

procedure t_jit_builder._VVMV(const desc:t_op_type;reg0,reg1:TRegValue;mem:t_jit_leas;reg2:TRegValue);
begin
 _VVMI8(desc,reg0,reg1,mem,reg2.AIndex shl 4);
end;

procedure t_jit_builder._VVV(const desc:t_op_type;reg0,reg1,reg2:TRegValue;size:TOperandSize);
var
 modrm_info:t_modrm_info;

 Vex:record
  rexW  :Boolean;
  Index :Byte;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm<>0);

 Assert(is_reg_size(reg0,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg0,[regGeneral,regXmm]));
 Assert(reg0.AScale<=1);

 Assert(is_reg_size(reg1,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg1,[regGeneral,regXmm]));
 Assert(reg1.AScale<=1);

 Assert(is_reg_size(reg2,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg2,[regGeneral,regXmm]));
 Assert(reg2.AScale<=1);

 ji:=default_jit_instruction;

 if (size=os0) then
 begin
  size:=reg0.ASize;
 end;

 Vex.Length:=get_vex_len(desc,size);

 if (verif_vex_len in desc.opt) then
 if (Vex.Length<>desc.vx_len) then
 begin
  Assert(false,'verif_vex_len');
 end;

 Vex.rexW:=get_vex_rexw(desc.vw_mode,size,os0);

 if (verif_rexw in desc.opt) then
 if (Vex.rexW<>desc.rexw) then
 begin
  Assert(false,'verif_rexw');
 end;

 Vex.Index:=reg1.AIndex;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rr(reg2,reg0);

 if Vex.rexW or
    modrm_info.rexB or
    modrm_info.rexX or
    (desc.mm>1) then
 begin
  ji.EmitByte($C4); //VEX3

  ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,desc.mm);
  ji.EmitWvvv(Vex.rexW,Vex.Index,Vex.Length,desc.simdop);
 end else
 begin
  ji.EmitByte($C5); //VEX2

  ji.EmitRvvv(modrm_info.rexR,Vex.Index,Vex.Length,desc.simdop);
 end;

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._VVVI8(const desc:t_op_type;reg0,reg1,reg2:TRegValue;imm8:Byte;size:TOperandSize);
var
 modrm_info:t_modrm_info;

 Vex:record
  rexW  :Boolean;
  Index :Byte;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm<>0);

 Assert(is_reg_size(reg0,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg0,[regGeneral,regXmm]));
 Assert(reg0.AScale<=1);

 Assert(is_reg_size(reg1,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg1,[regGeneral,regXmm]));
 Assert(reg1.AScale<=1);

 Assert(is_reg_size(reg2,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg2,[regGeneral,regXmm]));
 Assert(reg2.AScale<=1);

 ji:=default_jit_instruction;

 if (size=os0) then
 begin
  size:=reg0.ASize;
 end;

 Vex.Length:=get_vex_len(desc,size);

 if (verif_vex_len in desc.opt) then
 if (Vex.Length<>desc.vx_len) then
 begin
  Assert(false,'verif_vex_len');
 end;

 Vex.rexW:=get_vex_rexw(desc.vw_mode,size,os0);

 if (verif_rexw in desc.opt) then
 if (Vex.rexW<>desc.rexw) then
 begin
  Assert(false,'verif_rexw');
 end;

 Vex.Index:=reg1.AIndex;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rr(reg2,reg0);

 if Vex.rexW or
    modrm_info.rexB or
    modrm_info.rexX or
    (desc.mm>1) then
 begin
  ji.EmitByte($C4); //VEX3

  ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,desc.mm);
  ji.EmitWvvv(Vex.rexW,Vex.Index,Vex.Length,desc.simdop);
 end else
 begin
  ji.EmitByte($C5); //VEX2

  ji.EmitRvvv(modrm_info.rexR,Vex.Index,Vex.Length,desc.simdop);
 end;

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 ji.EmitByte(imm8);

 _add(ji);
end;

procedure t_jit_builder._VVI8(const desc:t_op_type;reg0,reg1:TRegValue;imm8:Byte;size:TOperandSize);
var
 modrm_info:t_modrm_info;

 Vex:record
  rexW  :Boolean;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));

 Assert(is_reg_size(reg0,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg0,[regGeneral,regXmm]));
 Assert(reg0.AScale<=1);

 Assert(is_reg_size(reg1,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg1,[regGeneral,regXmm]));
 Assert(reg1.AScale<=1);

 ji:=default_jit_instruction;

 if (size=os0) then
 begin
  size:=reg0.ASize;
 end;

 Vex.Length:=get_vex_len(desc,size);

 if (verif_vex_len in desc.opt) then
 if (Vex.Length<>desc.vx_len) then
 begin
  Assert(false,'verif_vex_len');
 end;

 Vex.rexW:=get_vex_rexw(desc.vw_mode,size,os0);

 if (verif_rexw in desc.opt) then
 if (Vex.rexW<>desc.rexw) then
 begin
  Assert(false,'verif_rexw');
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rr(reg1,reg0);

 ji.EmitByte($C4); //VEX3

 ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,desc.mm);
 ji.EmitWvvv(Vex.rexW,0,Vex.Length,desc.simdop);

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 ji.EmitByte(imm8);

 _add(ji);
end;

procedure t_jit_builder._VMI8(const desc:t_op_type;reg:TRegValue;mem:t_jit_leas;imm8:Byte);
var
 mreg:t_jit_lea;

 modrm_info:t_modrm_info;

 Vex:record
  rexW  :Boolean;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));

 Assert(is_reg_size(reg,[os8,os16,os32,os64,os128,os256]));
 Assert(is_reg_type(reg,[regXmm]));
 Assert(reg.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 Vex.Length:=get_vex_len(desc,mreg);

 if (verif_vex_len in desc.opt) then
 if (Vex.Length<>desc.vx_len) then
 begin
  Assert(false,'verif_vex_len');
 end;

 Vex.rexW:=get_vex_rexw(desc,reg.ASize,mreg.AMemSize);

 if (verif_rexw in desc.opt) then
 if (Vex.rexW<>desc.rexw) then
 begin
  Assert(false,'verif_rexw');
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rm(reg,mreg);

 if mreg.ALock then
 begin
  ji.EmitByte($F0);
 end;

 ji.EmitSelector(mreg.ASegment);

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 ji.EmitByte($C4); //VEX3

 ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,desc.mm);
 ji.EmitWvvv(Vex.rexW,0,Vex.Length,desc.simdop);

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 ji.EmitByte(imm8);

 _add(ji);
end;

procedure t_jit_builder.vmovdqu(reg:TRegValue;mem:t_jit_leas);
const
 desc:t_op_type=(op:$6F;simdop:2;mm:1);
begin
 _VM(desc,reg,mem);
end;

procedure t_jit_builder.vmovdqu(mem:t_jit_leas;reg:TRegValue);
const
 desc:t_op_type=(op:$7F;simdop:2;mm:1);
begin
 _VM(desc,reg,mem);
end;

procedure t_jit_builder.vmovdqa(reg:TRegValue;mem:t_jit_leas);
const
 desc:t_op_type=(op:$6F;simdop:1;mm:1);
begin
 _VM(desc,reg,mem);
end;

procedure t_jit_builder.vmovdqa(mem:t_jit_leas;reg:TRegValue);
const
 desc:t_op_type=(op:$7F;simdop:1;mm:1);
begin
 _VM(desc,reg,mem);
end;

procedure t_jit_builder.vmovntdq(mem:t_jit_leas;reg:TRegValue);
const
 desc:t_op_type=(op:$E7;simdop:1;mm:1);
begin
 _VM(desc,reg,mem);
end;

procedure t_jit_builder.vmovups(reg:TRegValue;mem:t_jit_leas);
const
 desc:t_op_type=(op:$10;simdop:0;mm:1);
begin
 _VM(desc,reg,mem);
end;

procedure t_jit_builder.vmovups(mem:t_jit_leas;reg:TRegValue);
const
 desc:t_op_type=(op:$11;simdop:0;mm:1);
begin
 _VM(desc,reg,mem);
end;

procedure t_jit_builder.vmovdqa(reg0:TRegValue;reg1:TRegValue);
var
 rexB,rexX,rexR:Boolean;

 ModRM:record
  Index,RM:Byte;
 end;

 Vex:record
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(is_reg_size(reg0,[os128,os256]));
 Assert(is_reg_size(reg1,[os128,os256]));

 Assert(is_reg_type(reg0,[regXmm]));
 Assert(is_reg_type(reg1,[regXmm]));

 Assert(reg0.AScale<=1);
 Assert(reg1.AScale<=1);

 Assert(reg0.ASize=reg1.ASize);

 ji:=default_jit_instruction;

 rexB:=false;
 rexX:=false;
 rexR:=false;

 ModRM.Index:=reg0.AIndex;
 if (ModRM.Index>=8) then
 begin
  rexR:=true;
  Dec(ModRM.Index,8);
 end;

 ModRM.RM:=reg1.AIndex;
 if (ModRM.RM>=8) then
 begin
  rexB:=true;
  Dec(ModRM.RM,8);
 end;

 case reg0.ASize of
  os128:Vex.Length:=0;
  os256:Vex.Length:=1;
  else;
 end;

 if rexB or rexX then
 begin
  ji.EmitByte($C4); //VEX3

  ji.EmitRXBm(rexB,rexX,rexR,1);
  ji.EmitWvvv(False,0,Vex.Length,1);
 end else
 begin
  ji.EmitByte($C5); //VEX2

  ji.EmitRvvv(rexR,0,Vex.Length,1);
 end;

 ji.EmitByte($6F);

 ji.EmitModRM(3,ModRM.Index,ModRM.RM);

 _add(ji);
end;

procedure t_jit_builder.sahf;
begin
 _O($9E);
end;

procedure t_jit_builder.lahf;
begin
 _O($9F);
end;

procedure t_jit_builder.cli;
begin
 _O($FA);
end;

procedure t_jit_builder.sti;
begin
 _O($FB);
end;

procedure t_jit_builder.seto(reg:TRegValue);
const
 desc:t_op_type=(op:$0F90;opt:[not_prefix]);
begin
 _R(desc,reg);
end;

procedure t_jit_builder.int3;
begin
 _O($CC);
end;

procedure t_jit_builder.testq(reg0:TRegValue;reg1:TRegValue);
const
 desc:t_op_type=(op:$85;index:0);
begin
 _RR(desc,reg0,reg1,os0);
end;

procedure t_jit_builder.bti8(mem:t_jit_leas;imm:Byte);
const
 desc:t_op_type=(op:$0FBA;index:4);
begin
 _MI8(desc,mem,imm);
end;


end.



