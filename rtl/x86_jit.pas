unit x86_jit;

{
 x86-64 minimal JIT Compiler by Red-prig
}

{$mode ObjFPC}{$H+}

interface

uses
 x86_fpdbgdisas;

type
 t_jit_reg=packed object
  ARegValue:TRegValues;
  AOffset  :Int64;
  ASegment :Byte;
  ALock    :Boolean;
 end;

 t_op_type=packed object
  op:DWORD;
  index:Byte;
  mm:Byte;
  opt:Set of (not_impl,not_prefix,not_vex_len);
 end;

 TOperandSizeSet =Set of TOperandSize;
 TRegisterTypeSet=Set of TRegisterType;

 t_jit_regs =array of t_jit_reg;

 t_jit_link_type=(lnkNone,lnkData,lnkLabel);

 t_jit_instruction=object
  AData:array[0..15] of Byte;
  ASize:Byte;
  AInstructionOffset:Integer;
  ALink:record
   AType:t_jit_link_type;
   ADataOffset:Byte;
   ALinkId:Integer;
  end;
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
  ASize:0;
  AInstructionOffset:0;
  ALink:(AType:lnkNone;ADataOffset:0;ALinkId:-1);
 );

type
 t_jit_instructions=array of t_jit_instruction;
 t_jit_data        =array of Pointer;

 p_jit_builder=^t_jit_builder;

 t_jit_i_link=object
  private
   builder:p_jit_builder;
   inst_id:Integer;
   procedure set_label(id:Integer);
   function  get_label():Integer;
  public
   property  _label:Integer read get_label write set_label;
 end;

 t_jit_builder=object
  Const
   LOCK:t_jit_reg=(ARegValue:((AType:regNone),(AType:regNone));ALock:True);
   FS  :t_jit_reg=(ARegValue:((AType:regNone),(AType:regNone));ASegment:4);
   GS  :t_jit_reg=(ARegValue:((AType:regNone),(AType:regNone));ASegment:5);

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
   AInstructions:t_jit_instructions;
   AInstructionSize:Integer;
   AData:t_jit_data;
  //
  procedure _add(const ji:t_jit_instruction);
  Function  _add_data(P:Pointer):Integer;
  Function  _get_data_offset(ALinkId,AInstructionEnd:Integer):Integer;
  Function  _get_label_offset(ALinkId,AInstructionEnd:Integer):Integer;
  Function  _label:Integer;
  //
  Procedure call(P:Pointer);
  Procedure jmp (P:Pointer);
  //
  function  call(_label_id:Integer):t_jit_i_link;
  function  jmp (_label_id:Integer):t_jit_i_link;
  function  jmp8(_label_id:Integer):t_jit_i_link;
  function  jcc (op:TOpCodeSuffix;_label_id:Integer):t_jit_i_link;
  function  movj(reg:TRegValue;mem:t_jit_regs;_label_id:Integer):t_jit_i_link;
  function  leaj(reg:TRegValue;mem:t_jit_regs;_label_id:Integer):t_jit_i_link;
  //
  Procedure reta;
  //
  Function  GetInstructionsSize:Integer;
  Function  GetDataSize:Integer;
  Function  GetMemSize:Integer;
  Procedure RebuldInstructionOffset;
  Procedure LinkData;
  Function  SaveTo(ptr:PByte;size:Integer):Integer;
  //
  procedure _RM     (const desc:t_op_type;reg:TRegValue;mem:t_jit_regs);
  procedure _RMI    (const desc:t_op_type;reg:TRegValue;mem:t_jit_regs;imm:Int64);
  procedure _RMI8   (const desc:t_op_type;reg:TRegValue;mem:t_jit_regs;imm:Byte);
  procedure _RR     (const desc:t_op_type;reg0:TRegValue;reg1:TRegValue;size:TOperandSize);
  procedure _RRI    (const desc:t_op_type;reg0:TRegValue;reg1:TRegValue;imm:Int64;size:TOperandSize=os0);
  procedure _RRI8   (const desc:t_op_type;reg0:TRegValue;reg1:TRegValue;imm:Byte;size:TOperandSize=os0);
  procedure _R      (const desc:t_op_type;reg:TRegValue);
  procedure _O      (const desc:t_op_type;reg:TRegValue);
  procedure _M      (const desc:t_op_type;size:TOperandSize;mem:t_jit_regs);
  procedure _RI     (const desc:t_op_type;reg:TRegValue;imm:Int64);
  procedure _MI     (const desc:t_op_type;size:TOperandSize;mem:t_jit_regs;imm:Int64);
  procedure _RI8    (const desc:t_op_type;reg:TRegValue;imm:Byte);
  procedure _MI8    (const desc:t_op_type;size:TOperandSize;mem:t_jit_regs;imm:Byte);
  procedure cmov    (op:TOpCodeSuffix;reg:TRegValue;mem:t_jit_regs);
  procedure cmov    (op:TOpCodeSuffix;reg0:TRegValue;reg1:TRegValue);
  procedure _push   (op,index:Byte;size:TOperandSize;mem:t_jit_regs);
  procedure _push   (op:Byte;reg:TRegValue);
  procedure _pushi  (size:TOperandSize;imm:Integer);
  procedure movq    (reg0:TRegValue ;reg1:TRegValue);
  procedure movi    (size:TOperandSize;mem:t_jit_regs;imm:Int64);
  procedure movi    (reg:TRegValue  ;imm:Int64);
  procedure movi64  (reg:TRegValue  ;imm:Int64);
  procedure movq    (reg:TRegValue  ;mem:t_jit_regs);
  procedure movq    (mem:t_jit_regs ;reg:TRegValue);
  procedure leaq    (reg:TRegValue  ;mem:t_jit_regs);
  procedure addq    (mem:t_jit_regs ;reg:TRegValue);
  procedure addq    (reg:TRegValue  ;mem:t_jit_regs);
  procedure addq    (reg0:TRegValue ;reg1:TRegValue);
  procedure addi    (reg:TRegValue  ;imm:Int64);
  procedure addi8   (reg:TRegValue  ;imm:Byte);
  procedure addi8   (size:TOperandSize;mem:t_jit_regs;imm:Byte);
  procedure subq    (mem:t_jit_regs ;reg:TRegValue);
  procedure subq    (reg:TRegValue  ;mem:t_jit_regs);
  procedure subq    (reg0:TRegValue ;reg1:TRegValue);
  procedure subi    (reg:TRegValue  ;imm:Int64);
  procedure subi8   (reg:TRegValue  ;imm:Byte);
  procedure subi8   (size:TOperandSize;mem:t_jit_regs;imm:Byte);
  procedure xorq    (reg0:TRegValue ;reg1:TRegValue);
  procedure cmpq    (mem:t_jit_regs ;reg:TRegValue);
  procedure cmpq    (reg:TRegValue  ;mem:t_jit_regs);
  procedure cmpq    (reg0:TRegValue ;reg1:TRegValue);
  procedure cmpi    (reg:TRegValue  ;imm:Int64);
  procedure cmpi    (size:TOperandSize;mem:t_jit_regs;imm:Int64);
  procedure cmpi8   (reg:TRegValue;imm:Byte);
  procedure cmpi8   (size:TOperandSize;mem:t_jit_regs;imm:Byte);
  procedure push16  (mem:t_jit_regs);
  procedure push64  (mem:t_jit_regs);
  procedure push    (reg:TRegValue);
  procedure push8   (imm:Integer);
  procedure push16  (imm:Integer);
  procedure push32  (imm:Integer);
  procedure pop16   (mem:t_jit_regs);
  procedure pop64   (mem:t_jit_regs);
  procedure pop     (reg:TRegValue);
  procedure pushfq;
  procedure popfq;
  procedure _VM     (const desc:t_op_type;reg:TRegValue;mem:t_jit_regs;size:TOperandSize);
  procedure _VM_F3  (const desc:t_op_type;reg:TRegValue;mem:t_jit_regs;size:TOperandSize);
  procedure _VV_F3  (const desc:t_op_type;reg0,reg1:TRegValue;size:TOperandSize);
  procedure _VVM    (const desc:t_op_type;reg0,reg1:TRegValue;mem:t_jit_regs);
  procedure _VVV    (const desc:t_op_type;reg0,reg1,reg2:TRegValue);
  procedure _RVI8   (const desc:t_op_type;reg0,reg1:TRegValue;imm8:Byte);
  procedure _MVI8   (const desc:t_op_type;mem:t_jit_regs;reg:TRegValue;imm8:Byte);
  procedure vmovdqu (reg:TRegValue ;mem:t_jit_regs);
  procedure vmovdqu (mem:t_jit_regs;reg:TRegValue);
  procedure vmovdqa (reg:TRegValue ;mem:t_jit_regs);
  procedure vmovdqa (mem:t_jit_regs;reg:TRegValue);
  procedure vmovntdq(mem:t_jit_regs;reg:TRegValue);
  procedure vmovups (reg:TRegValue ;mem:t_jit_regs);
  procedure vmovups (mem:t_jit_regs;reg:TRegValue);
  procedure vmovdqa (reg0:TRegValue;reg1:TRegValue);
  procedure sahf;
  procedure lahf;
 end;

operator :=(const A:TRegValue):t_jit_reg;
operator + (const A,B:t_jit_reg):t_jit_reg;
operator + (const A:t_jit_reg;const B:TRegValue):t_jit_reg;
operator + (const A:t_jit_reg;B:Integer):t_jit_reg;
operator - (const A:t_jit_reg;B:Integer):t_jit_reg;
operator + (const A:t_jit_reg;B:Int64):t_jit_reg;
operator - (const A:t_jit_reg;B:Int64):t_jit_reg;
operator * (const A:t_jit_reg;B:Integer):t_jit_reg;

function classif_offset_32(AOffset:Integer):Byte;
function classif_offset_64(AOffset:Int64):TOperandSize;
function classif_offset_se64(AOffset:Int64):TOperandSize;

implementation

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

function classif_offset_32(reg:t_jit_reg):Byte; inline;
begin
 Result:=classif_offset_32(reg.AOffset);
end;

function classif_offset_64(reg:t_jit_reg):TOperandSize; inline;
begin
 Result:=classif_offset_64(reg.AOffset);
end;

function is_not_reg(reg:t_jit_reg):Boolean; inline;
begin
 Result:=(not is_valid_reg_type(reg.ARegValue[0])) and
         (not is_valid_reg_type(reg.ARegValue[1]));
end;

function is_one_reg(reg:t_jit_reg):Boolean; inline;
begin
 Result:=(    is_valid_reg_type(reg.ARegValue[0])) and
         (not is_valid_reg_type(reg.ARegValue[1]));
end;

function is_reg_size(reg:t_jit_reg;Size:TOperandSizeSet):Boolean; inline;
begin
 Result:=(reg.ARegValue[0].ASize in Size) and
         ((reg.ARegValue[1].ASize in Size) or (reg.ARegValue[1].ASize=os0));
end;

function is_reg_type(reg:t_jit_reg;AType:TRegisterTypeSet):Boolean; inline;
begin
 Result:=(reg.ARegValue[0].AType in AType) and
         ((reg.ARegValue[1].AType in AType) or (reg.ARegValue[1].AType=regNone));
end;

function is_valid_seg(reg:t_jit_reg):Boolean; inline;
begin
 case reg.ASegment of
  0..5:Result:=True
  else
       Result:=False;
 end;
end;

function is_valid_scale(reg:t_jit_reg):Boolean; inline;
begin
 Result:=is_valid_scale(reg.ARegValue[0]) and (reg.ARegValue[1].AScale<=1)
end;

operator := (const A:TRegValue):t_jit_reg;
begin
 Result:=Default(t_jit_reg);
 Result.ARegValue[0]:=A;
end;

operator + (const A,B:t_jit_reg):t_jit_reg;
begin
 Assert(is_not_reg(A) or is_one_reg(A));
 Assert(is_not_reg(B) or is_one_reg(B));

 Result:=A;

 if is_one_reg(Result) then
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
 begin
  Result.ASegment:=B.ASegment;
 end;

 Result.ALock:=Result.ALock or B.ALock;

 Result.AOffset:=Result.AOffset+B.AOffset;
end;

operator + (const A:t_jit_reg;const B:TRegValue):t_jit_reg;
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

operator + (const A:t_jit_reg;B:Integer):t_jit_reg;
begin
 Result:=A;

 Result.AOffset:=Result.AOffset+B;
end;

operator - (const A:t_jit_reg;B:Integer):t_jit_reg;
begin
 Result:=A;

 Result.AOffset:=Result.AOffset-B;
end;

operator + (const A:t_jit_reg;B:Int64):t_jit_reg;
begin
 Result:=A;

 Result.AOffset:=Result.AOffset+B;
end;

operator - (const A:t_jit_reg;B:Int64):t_jit_reg;
begin
 Result:=A;

 Result.AOffset:=Result.AOffset-B;
end;

operator * (const A:t_jit_reg;B:Integer):t_jit_reg;
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

function Sums(mem:t_jit_regs):t_jit_reg;
var
 i:Integer;
begin
 if (Length(mem)=0) then
 begin
  Result:=Default(t_jit_reg);
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

////

procedure t_jit_i_link.set_label(id:Integer);
begin
 builder^.AInstructions[inst_id].ALink.ALinkId:=id;
end;

function t_jit_i_link.get_label():Integer;
begin
 Result:=builder^.AInstructions[inst_id].ALink.ALinkId;
end;

////

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

procedure t_jit_builder._add(const ji:t_jit_instruction);
var
 i:Integer;
begin
 i:=Length(AInstructions);
 SetLength(AInstructions,i+1);
 AInstructions[i]:=ji;

 AInstructions[i].AInstructionOffset:=AInstructionSize;
 Inc(AInstructionSize,ji.ASize);
end;

Function t_jit_builder._add_data(P:Pointer):Integer;
begin
 Result:=Length(AData);
 SetLength(AData,Result+1);
 AData[Result]:=P;
end;

Function t_jit_builder._get_data_offset(ALinkId,AInstructionEnd:Integer):Integer;
begin
 Assert(ALinkId<Length(AData));
 Result:=(AInstructionSize-AInstructionEnd)+(ALinkId*SizeOf(Pointer));
end;

Function t_jit_builder._get_label_offset(ALinkId,AInstructionEnd:Integer):Integer;
var
 i:Integer;
begin
 Assert(ALinkId<=Length(AInstructions));

 if (ALinkId>=Length(AInstructions)) then
 begin
  i:=AInstructionSize;
 end else
 begin
  i:=AInstructions[ALinkId].AInstructionOffset;
 end;

 Result:=(i-AInstructionEnd);
end;

Function t_jit_builder._label:Integer;
begin
 Result:=Length(AInstructions);
end;

Procedure t_jit_builder.call(P:Pointer);
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($FF);
 ji.EmitByte($15);

 ji.ALink.AType:=lnkData;
 ji.ALink.ADataOffset:=ji.ASize;
 ji.ALink.ALinkId:=_add_data(P);

 ji.EmitInt32(0);

 _add(ji);
end;

Procedure t_jit_builder.jmp(P:Pointer);
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($FF);
 ji.EmitByte($25);

 ji.ALink.AType:=lnkData;
 ji.ALink.ADataOffset:=ji.ASize;
 ji.ALink.ALinkId:=_add_data(P);

 ji.EmitInt32(0);

 _add(ji);
end;

function t_jit_builder.call(_label_id:Integer):t_jit_i_link;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($E8);

 ji.ALink.AType:=lnkLabel;
 ji.ALink.ADataOffset:=ji.ASize;
 ji.ALink.ALinkId:=_label_id;

 ji.EmitInt32(0);

 _add(ji);

 Result.builder:=@self;
 Result.inst_id:=High(AInstructions);
end;

function t_jit_builder.jmp(_label_id:Integer):t_jit_i_link;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($E9);

 ji.ALink.AType:=lnkLabel;
 ji.ALink.ADataOffset:=ji.ASize;
 ji.ALink.ALinkId:=_label_id;

 ji.EmitInt32(0);

 _add(ji);

 Result.builder:=@self;
 Result.inst_id:=High(AInstructions);
end;

function t_jit_builder.jmp8(_label_id:Integer):t_jit_i_link;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($EB);

 ji.ALink.AType:=lnkLabel;
 ji.ALink.ADataOffset:=ji.ASize;
 ji.ALink.ALinkId:=_label_id;

 ji.EmitByte(0);

 _add(ji);

 Result.builder:=@self;
 Result.inst_id:=High(AInstructions);
end;

const
 COND_32:array[OPSc_o..OPSc_nle] of Byte=(
  $80,$81,$82,$83,$84,$85,$86,$87,
  $88,$89,$8A,$8B,$8C,$8D,$8E,$8F
 );

function t_jit_builder.jcc(op:TOpCodeSuffix;_label_id:Integer):t_jit_i_link;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 case op of
  OPSc_o..OPSc_nle:;
  else
   Assert(false);
 end;

 ji.EmitByte($0F);

 ji.EmitByte(COND_32[op]);

 ji.ALink.AType:=lnkLabel;
 ji.ALink.ADataOffset:=ji.ASize;
 ji.ALink.ALinkId:=_label_id;

 ji.EmitInt32(0);

 _add(ji);

 Result.builder:=@self;
 Result.inst_id:=High(AInstructions);
end;

function t_jit_builder.movj(reg:TRegValue;mem:t_jit_regs;_label_id:Integer):t_jit_i_link;
var
 i:Integer;
begin
 movq(reg,mem);

 i:=High(AInstructions);

 AInstructions[i].ALink.AType:=lnkLabel;
 AInstructions[i].ALink.ALinkId:=_label_id;

 Result.builder:=@self;
 Result.inst_id:=High(AInstructions);
end;

function t_jit_builder.leaj(reg:TRegValue;mem:t_jit_regs;_label_id:Integer):t_jit_i_link;
var
 i:Integer;
begin
 leaq(reg,mem);

 i:=High(AInstructions);

 AInstructions[i].ALink.AType:=lnkLabel;
 AInstructions[i].ALink.ALinkId:=_label_id;

 Result.builder:=@self;
 Result.inst_id:=High(AInstructions);
end;

Procedure t_jit_builder.reta;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($C3);

 _add(ji);
end;

Function t_jit_builder.GetInstructionsSize:Integer;
begin
 Result:=AInstructionSize;
end;

Function t_jit_builder.GetDataSize:Integer;
begin
 Result:=Length(AData)*SizeOf(QWORD);
end;

Function t_jit_builder.GetMemSize:Integer;
begin
 Result:=AInstructionSize+Length(AData)*SizeOf(QWORD);
end;

Procedure t_jit_builder.RebuldInstructionOffset;
var
 i:Integer;
begin
 AInstructionSize:=0;
 if (Length(AInstructions)<>0) then
 For i:=0 to High(AInstructions) do
 begin
  AInstructions[i].AInstructionOffset:=AInstructionSize;
  Inc(AInstructionSize,AInstructions[i].ASize);
 end;
end;

Procedure t_jit_builder.LinkData;
var
 i,d:Integer;
begin
 if (Length(AInstructions)<>0) then
 For i:=0 to High(AInstructions) do
 begin
  With AInstructions[i] do
   case ALink.AType of
    lnkData:
      begin
       d:=_get_data_offset(ALink.ALinkId,AInstructionOffset+ASize);
       PInteger(@AData[ALink.ADataOffset])^:=d;
      end;
    lnkLabel:
      begin
       d:=_get_label_offset(ALink.ALinkId,AInstructionOffset+ASize);
       PInteger(@AData[ALink.ADataOffset])^:=d;
      end;
    else;
   end;
 end;
end;

Function t_jit_builder.SaveTo(ptr:PByte;size:Integer):Integer;
var
 i,s:Integer;
begin
 LinkData;

 Result:=0;
 if (Length(AInstructions)<>0) then
 For i:=0 to High(AInstructions) do
 begin
  s:=AInstructions[i].ASize;
  if ((Result+s)>size) then
  begin
   Exit;
  end;
  Move(AInstructions[i].AData,ptr^,s);
  Inc(Result,s);
  Inc(ptr   ,s);
 end;

 if (Length(AData)<>0) then
 For i:=0 to High(AData) do
 begin
  s:=SizeOf(QWORD);
  if ((Result+s)>size) then
  begin
   Exit;
  end;
  Move(AData[i],ptr^,s);
  Inc(Result,s);
  Inc(ptr   ,s);
 end;
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

  procedure build_im(Index:Byte;const mreg:t_jit_reg);
  procedure build_rm(const reg:TRegValue;const mreg:t_jit_reg);
  procedure build_rr(const reg0,reg1:TRegValue);
  procedure build_ir(Index:Byte;const reg:TRegValue);
  procedure emit_rex(var ji:t_jit_instruction;rexW:Boolean);
  procedure emit_mrm(var ji:t_jit_instruction);
 end;

procedure t_modrm_info.build_im(Index:Byte;const mreg:t_jit_reg);
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
   Assert(false,'imposible');
   Exit;
  end;

  if (mreg.ARegValue[0].AScale>1) then
  begin
   Assert(false,'imposible');
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
     Assert(false,'imposible');
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
      Assert(false,'imposible');
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
     Assert(false,'imposible');
     Exit;
    end;

    if not ubase then
    begin
     Assert(false,'imposible');
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
     Assert(false,'imposible');
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

procedure t_modrm_info.build_rm(const reg:TRegValue;const mreg:t_jit_reg);
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
  if rexF or rexR then
  begin
   Assert(false,'imposible');
  end;
  RH:=True;
  Inc(ModRM.RM,4)
 end else
 if (ModRM.RM>=8) then
 begin
  if RH then
  begin
   Assert(false,'imposible');
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
        ji.ALink.ADataOffset:=ji.ASize;
        ji.EmitInt32(AOffset); //4
       end;
     5:begin
        ji.ALink.ADataOffset:=ji.ASize;
        ji.EmitInt32(AOffset); //4
       end;
    end;
   end;
   1:ji.EmitByte(AOffset); //1
   2:begin
      ji.ALink.ADataOffset:=ji.ASize;
      ji.EmitInt32(AOffset); //4
     end;
   else;
  end;
 end;

end;

procedure t_jit_builder._RM(const desc:t_op_type;reg:TRegValue;mem:t_jit_regs);
var
 mreg:t_jit_reg;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(is_reg_size(reg,[os8,os16,os32,os64,os128]));
 Assert(is_reg_type(reg,[regGeneral,regGeneralH]));
 Assert(reg.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 op:=desc.op;
 case reg.ASize of
   os8:
       begin
        Dec(op);
       end;
  os16,
  os128:
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

 modrm_info.emit_rex(ji,rexW);

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

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._RMI(const desc:t_op_type;reg:TRegValue;mem:t_jit_regs;imm:Int64);
var
 mreg:t_jit_reg;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(is_reg_size(reg,[os8,os16,os32,os64,os128]));
 Assert(is_reg_type(reg,[regGeneral,regGeneralH]));
 Assert(reg.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 op:=desc.op;
 case reg.ASize of
   os8:
       begin
        Dec(op);
       end;
  os16,
  os128:
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

 modrm_info.emit_rex(ji,rexW);

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

procedure t_jit_builder._RMI8(const desc:t_op_type;reg:TRegValue;mem:t_jit_regs;imm:Byte);
var
 mreg:t_jit_reg;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(is_reg_size(reg,[os8,os16,os32,os64,os128]));
 Assert(is_reg_type(reg,[regGeneral,regGeneralH]));
 Assert(reg.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 op:=desc.op;
 case reg.ASize of
   os8:
       begin
        Dec(op);
       end;
  os16,
  os128:
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

 modrm_info.emit_rex(ji,rexW);

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

 modrm_info.emit_mrm(ji);

 ji.EmitByte(imm);

 _add(ji);
end;

procedure t_jit_builder._RR(const desc:t_op_type;reg0:TRegValue;reg1:TRegValue;size:TOperandSize);
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

 Assert(is_reg_type(reg0,[regGeneral,regGeneralH]));
 Assert(is_reg_type(reg1,[regGeneral,regGeneralH]));

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
       begin
        Dec(op);
       end;
  os16,
  os128:
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

 modrm_info.emit_rex(ji,rexW);

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

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

////

procedure t_jit_builder._RRI(const desc:t_op_type;reg0:TRegValue;reg1:TRegValue;imm:Int64;size:TOperandSize=os0);
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

 Assert(is_reg_type(reg0,[regGeneral,regGeneralH]));
 Assert(is_reg_type(reg1,[regGeneral,regGeneralH]));

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
       begin
        Dec(op);
       end;
  os16,
  os128:
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

 modrm_info.emit_rex(ji,rexW);

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

procedure t_jit_builder._RRI8(const desc:t_op_type;reg0:TRegValue;reg1:TRegValue;imm:Byte;size:TOperandSize=os0);
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

 Assert(is_reg_type(reg0,[regGeneral,regGeneralH]));
 Assert(is_reg_type(reg1,[regGeneral,regGeneralH]));

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
       begin
        Dec(op);
       end;
  os16,
  os128:
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

 modrm_info.emit_rex(ji,rexW);

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
 Assert(is_reg_type(reg,[regGeneral,regGeneralH]));
 Assert(reg.AScale<=1);

 ji:=default_jit_instruction;

 rexW:=false;
 Prefix:=0;

 op:=desc.op;
 case reg.ASize of
   os8:
       begin
        Dec(op);
       end;
  os16,
  os128:
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

 modrm_info.emit_rex(ji,rexW);

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

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

///

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
  os16,
  os128:
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

procedure t_jit_builder._M(const desc:t_op_type;size:TOperandSize;mem:t_jit_regs);
var
 mreg:t_jit_reg;

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
 case size of
   os8:
       begin
        Dec(op);
       end;
  os16,
  os128:
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
  ji.EmitByte(Prefix); //Operand-size override prefix (16,128)
 end;

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 modrm_info.emit_rex(ji,rexW);

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
 Assert(is_reg_type(reg,[regGeneral,regGeneralH]));
 Assert(reg.AScale<=1);

 ji:=default_jit_instruction;

 Prefix:=0;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_ir(desc.index,reg);

 op:=desc.op;
 case reg.ASize of
   os8:
       begin
        Dec(op);
       end;
  os16:
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

 modrm_info.emit_rex(ji,rexW);

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

procedure t_jit_builder._MI(const desc:t_op_type;size:TOperandSize;mem:t_jit_regs;imm:Int64);
var
 mreg:t_jit_reg;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(size in [os8,os16,os32,os64]);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 op:=desc.op;
 case size of
   os8:
       begin
        Dec(op);
       end;
  os16:
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

 modrm_info.emit_rex(ji,rexW);

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

 modrm_info.emit_mrm(ji);

 case Size of
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
 Assert(is_reg_size(reg,[os16,os32,os64]));
 Assert(is_reg_type(reg,[regGeneral,regGeneralH]));
 Assert(reg.AScale<=1);

 ji:=default_jit_instruction;

 rexW:=false;
 Prefix:=0;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_ir(desc.index,reg);

 op:=desc.op;
 case reg.ASize of
  os16:
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

 modrm_info.emit_rex(ji,rexW);

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

 modrm_info.emit_mrm(ji);

 ji.EmitByte(imm);

 _add(ji);
end;

procedure t_jit_builder._MI8(const desc:t_op_type;size:TOperandSize;mem:t_jit_regs;imm:Byte);
var
 mreg:t_jit_reg;

 op:DWORD;
 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(size in [os16,os32,os64]);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 op:=desc.op;
 case size of
  os16:
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

 modrm_info.emit_rex(ji,rexW);

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

procedure t_jit_builder.cmov(op:TOpCodeSuffix;reg:TRegValue;mem:t_jit_regs);
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

procedure t_jit_builder._push(op,index:Byte;size:TOperandSize;mem:t_jit_regs);
var
 mreg:t_jit_reg;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 mreg:=Sums(mem);

 Assert(size in [os16,os64]);

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

 if (size=os16) then
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

procedure t_jit_builder.movi(size:TOperandSize;mem:t_jit_regs;imm:Int64);
const
 desc:t_op_type=(op:$C7;index:0);
begin
 _MI(desc,size,mem,imm);
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

procedure t_jit_builder.movq(reg:TRegValue;mem:t_jit_regs);
const
 desc:t_op_type=(op:$8B;index:0);
begin
 _RM(desc,reg,mem); //MOV r64, r/m64
end;

procedure t_jit_builder.movq(mem:t_jit_regs;reg:TRegValue);
const
 desc:t_op_type=(op:$89;index:0);
begin
 _RM(desc,reg,mem); //MOV r/m64, r64
end;

//

procedure t_jit_builder.leaq(reg:TRegValue;mem:t_jit_regs);
const
 desc:t_op_type=(op:$8D;index:0);
begin
 Assert(is_reg_size(reg,[os16,os32,os64]));

 _RM(desc,reg,mem); //LEA r64,m
end;

//

procedure t_jit_builder.addq(mem:t_jit_regs;reg:TRegValue);
const
 desc:t_op_type=(op:$01;index:0);
begin
 _RM(desc,reg,mem); //ADD r/m64, r64
end;

procedure t_jit_builder.addq(reg:TRegValue;mem:t_jit_regs);
const
 desc:t_op_type=(op:$03;index:0);
begin
 _RM(desc,reg,mem); //ADD r64, r/m64
end;

procedure t_jit_builder.addq(reg0:TRegValue;reg1:TRegValue);
const
 desc:t_op_type=(op:$01;index:0);
begin
 _RR(desc,reg0,reg1,os0);
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

procedure t_jit_builder.addi8(size:TOperandSize;mem:t_jit_regs;imm:Byte);
const
 desc:t_op_type=(op:$83;index:0);
begin
 _MI8(desc,size,mem,imm);
end;

//

procedure t_jit_builder.subq(mem:t_jit_regs;reg:TRegValue);
const
 desc:t_op_type=(op:$29;index:0);
begin
 _RM(desc,reg,mem); //SUB r/m64, r64
end;

procedure t_jit_builder.subq(reg:TRegValue;mem:t_jit_regs);
const
 desc:t_op_type=(op:$2B;index:0);
begin
 _RM(desc,reg,mem); //SUB r64, r/m64
end;

procedure t_jit_builder.subq(reg0:TRegValue;reg1:TRegValue);
const
 desc:t_op_type=(op:$29;index:0);
begin
 _RR(desc,reg0,reg1,os0);
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

procedure t_jit_builder.subi8(size:TOperandSize;mem:t_jit_regs;imm:Byte);
const
 desc:t_op_type=(op:$83;index:5);
begin
 _MI8(desc,size,mem,imm);
end;

///

procedure t_jit_builder.xorq(reg0:TRegValue;reg1:TRegValue);
const
 desc:t_op_type=(op:$31;index:0);
begin
 _RR(desc,reg0,reg1,os0);
end;

///

procedure t_jit_builder.cmpq(mem:t_jit_regs;reg:TRegValue);
const
 desc:t_op_type=(op:$39;index:0);
begin
 _RM(desc,reg,mem);
end;

procedure t_jit_builder.cmpq(reg:TRegValue;mem:t_jit_regs);
const
 desc:t_op_type=(op:$3B;index:0);
begin
 _RM(desc,reg,mem);
end;

procedure t_jit_builder.cmpq(reg0:TRegValue;reg1:TRegValue);
const
 desc:t_op_type=(op:$39;index:0);
begin
 _RR(desc,reg0,reg1,os0);
end;

procedure t_jit_builder.cmpi(reg:TRegValue;imm:Int64);
const
 desc:t_op_type=(op:$810;index:7);
begin
 _RI(desc,reg,imm);
end;

procedure t_jit_builder.cmpi(size:TOperandSize;mem:t_jit_regs;imm:Int64);
const
 desc:t_op_type=(op:$81;index:7);
begin
 _MI(desc,size,mem,imm);
end;

procedure t_jit_builder.cmpi8(reg:TRegValue;imm:Byte);
const
 desc:t_op_type=(op:$83;index:7);
begin
 _RI8(desc,reg,imm);
end;

procedure t_jit_builder.cmpi8(size:TOperandSize;mem:t_jit_regs;imm:Byte);
const
 desc:t_op_type=(op:$83;index:7);
begin
 _MI8(desc,size,mem,imm);
end;


///

procedure t_jit_builder.push16(mem:t_jit_regs);
begin
 _push($FF,6,os16,mem);
end;

procedure t_jit_builder.push64(mem:t_jit_regs);
begin
 _push($FF,6,os64,mem);
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

procedure t_jit_builder.pop16(mem:t_jit_regs);
begin
 _push($8F,0,os16,mem);
end;

procedure t_jit_builder.pop64(mem:t_jit_regs);
begin
 _push($8F,0,os64,mem);
end;

procedure t_jit_builder.pop(reg:TRegValue);
begin
 _push($58,reg);
end;

procedure t_jit_builder.pushfq;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($9C);

 _add(ji);
end;

procedure t_jit_builder.popfq;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($9D);

 _add(ji);
end;

procedure t_jit_builder._VM(const desc:t_op_type;reg:TRegValue;mem:t_jit_regs;size:TOperandSize);
var
 mreg:t_jit_reg;

 modrm_info:t_modrm_info;

 Vex:record
  rexW:Boolean;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm=1);

 Assert(is_reg_size(reg,[os32,os64,os128,os256]));
 Assert(is_reg_type(reg,[regGeneral,regXmm]));
 Assert(reg.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 Vex.Length:=0;

 if not (not_vex_len in desc.opt) then
 case reg.ASize of
  os128:Vex.Length:=0;
  os256:Vex.Length:=1;
  else;
 end;

 Vex.rexW:=False;
 if (size=os64) then
 begin
  Vex.rexW:=True;
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
    modrm_info.rexX then
 begin
  ji.EmitByte($C4); //VEX3

  ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,1);
  ji.EmitWvvv(Vex.rexW,0,Vex.Length,desc.index);
 end else
 begin
  ji.EmitByte($C5); //VEX2

  ji.EmitRvvv(modrm_info.rexR,0,Vex.Length,desc.index);
 end;

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._VM_F3(const desc:t_op_type;reg:TRegValue;mem:t_jit_regs;size:TOperandSize);
var
 mreg:t_jit_reg;

 modrm_info:t_modrm_info;

 Vex:record
  rexW:Boolean;
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

 Vex.Length:=0;

 if not (not_vex_len in desc.opt) then
 case reg.ASize of
  os128:Vex.Length:=0;
  os256:Vex.Length:=1;
  else;
 end;

 Vex.rexW:=False;
 if (size=os64) then
 begin
  Vex.rexW:=True;
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
  rexW:Boolean;
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

 Vex.Length:=0;

 Vex.rexW:=False;
 if (size=os64) then
 begin
  Vex.rexW:=True;
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

procedure t_jit_builder._VVM(const desc:t_op_type;reg0,reg1:TRegValue;mem:t_jit_regs);
var
 mreg:t_jit_reg;

 modrm_info:t_modrm_info;

 Vex:record
  rexW:Boolean;
  Index :Byte;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm<>0);

 Assert(is_reg_size(reg0,[os32,os64,os128,os256]));
 Assert(is_reg_type(reg0,[regGeneral,regXmm]));
 Assert(reg0.AScale<=1);

 Assert(is_reg_size(reg1,[os32,os64,os128,os256]));
 Assert(is_reg_type(reg1,[regGeneral,regXmm]));
 Assert(reg1.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 Vex.Length:=0;

 if not (not_vex_len in desc.opt) then
 case reg0.ASize of
  os128:Vex.Length:=0;
  os256:Vex.Length:=1;
  else;
 end;

 Vex.Index:=reg1.AIndex;

 Vex.rexW:=False;
 if (reg0.ASize=os64) then
 begin
  Vex.rexW:=True;
 end;

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
  ji.EmitWvvv(Vex.rexW,Vex.Index,Vex.Length,desc.index);
 end else
 begin
  ji.EmitByte($C5); //VEX2

  ji.EmitRvvv(modrm_info.rexR,Vex.Index,Vex.Length,desc.index);
 end;

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._VVV(const desc:t_op_type;reg0,reg1,reg2:TRegValue);
var
 modrm_info:t_modrm_info;

 Vex:record
  rexW:Boolean;
  Index :Byte;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm<>0);

 Assert(is_reg_size(reg0,[os32,os64,os128,os256]));
 Assert(is_reg_type(reg0,[regGeneral,regXmm]));
 Assert(reg0.AScale<=1);

 Assert(is_reg_size(reg1,[os32,os64,os128,os256]));
 Assert(is_reg_type(reg1,[regGeneral,regXmm]));
 Assert(reg1.AScale<=1);

 Assert(is_reg_size(reg2,[os32,os64,os128,os256]));
 Assert(is_reg_type(reg2,[regGeneral,regXmm]));
 Assert(reg2.AScale<=1);

 ji:=default_jit_instruction;

 Vex.Length:=0;

 if not (not_vex_len in desc.opt) then
 case reg0.ASize of
  os128:Vex.Length:=0;
  os256:Vex.Length:=1;
  else;
 end;

 Vex.Index:=reg1.AIndex;

 Vex.rexW:=False;
 if (reg0.ASize=os64) then
 begin
  Vex.rexW:=True;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rr(reg2,reg0);

 if Vex.rexW or
    modrm_info.rexB or
    modrm_info.rexX or
    (desc.mm>1) then
 begin
  ji.EmitByte($C4); //VEX3

  ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,desc.mm);
  ji.EmitWvvv(Vex.rexW,Vex.Index,Vex.Length,desc.index);
 end else
 begin
  ji.EmitByte($C5); //VEX2

  ji.EmitRvvv(modrm_info.rexR,Vex.Index,Vex.Length,desc.index);
 end;

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 _add(ji);
end;

procedure t_jit_builder._RVI8(const desc:t_op_type;reg0,reg1:TRegValue;imm8:Byte);
var
 modrm_info:t_modrm_info;

 Vex:record
  rexW:Boolean;
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));
 Assert(desc.mm=3);

 Assert(is_reg_size(reg0,[os8,os16,os32,os64]));
 Assert(is_reg_type(reg0,[regGeneral]));
 Assert(reg0.AScale<=1);

 Assert(is_reg_size(reg1,[os128,os256]));
 Assert(is_reg_type(reg1,[regXmm]));
 Assert(reg1.AScale<=1);

 ji:=default_jit_instruction;

 Vex.Length:=0;

 if not (not_vex_len in desc.opt) then
 case reg1.ASize of
  os128:Vex.Length:=0;
  os256:Vex.Length:=1;
  else;
 end;

 Vex.rexW:=False;
 if (reg0.ASize=os64) then
 begin
  Vex.rexW:=True;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build_rr(reg0,reg1);

 ji.EmitByte($C4); //VEX3

 ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,3);
 ji.EmitWvvv(Vex.rexW,0,Vex.Length,desc.index);

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 ji.EmitByte(imm8);

 _add(ji);
end;

procedure t_jit_builder._MVI8(const desc:t_op_type;mem:t_jit_regs;reg:TRegValue;imm8:Byte);
var
 mreg:t_jit_reg;

 modrm_info:t_modrm_info;

 Vex:record
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(not (not_impl in desc.opt));

 Assert(is_reg_size(reg,[os128,os256]));
 Assert(is_reg_type(reg,[regXmm]));
 Assert(reg.AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 Vex.Length:=0;

 if not (not_vex_len in desc.opt) then
 case reg.ASize of
  os128:Vex.Length:=0;
  os256:Vex.Length:=1;
  else;
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

 ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,3);
 ji.EmitWvvv(False,0,Vex.Length,desc.index);

 ji.EmitByte(desc.op);

 modrm_info.emit_mrm(ji);

 ji.EmitByte(imm8);

 _add(ji);
end;

procedure t_jit_builder.vmovdqu(reg:TRegValue;mem:t_jit_regs);
const
 desc:t_op_type=(op:$6F;index:2);
begin
 _VM(desc,reg,mem,os0);
end;

procedure t_jit_builder.vmovdqu(mem:t_jit_regs;reg:TRegValue);
const
 desc:t_op_type=(op:$7F;index:2);
begin
 _VM(desc,reg,mem,os0);
end;

procedure t_jit_builder.vmovdqa(reg:TRegValue;mem:t_jit_regs);
const
 desc:t_op_type=(op:$6F;index:1);
begin
 _VM(desc,reg,mem,os0);
end;

procedure t_jit_builder.vmovdqa(mem:t_jit_regs;reg:TRegValue);
const
 desc:t_op_type=(op:$7F;index:1);
begin
 _VM(desc,reg,mem,os0);
end;

procedure t_jit_builder.vmovntdq(mem:t_jit_regs;reg:TRegValue);
const
 desc:t_op_type=(op:$E7;index:1);
begin
 _VM(desc,reg,mem,os0);
end;

procedure t_jit_builder.vmovups(reg:TRegValue;mem:t_jit_regs);
const
 desc:t_op_type=(op:$10;index:0);
begin
 _VM(desc,reg,mem,os0);
end;

procedure t_jit_builder.vmovups(mem:t_jit_regs;reg:TRegValue);
const
 desc:t_op_type=(op:$11;index:0);
begin
 _VM(desc,reg,mem,os0);
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
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($9E);

 _add(ji);
end;

procedure t_jit_builder.lahf;
var
 ji:t_jit_instruction;
begin
 ji:=default_jit_instruction;

 ji.EmitByte($9F);

 _add(ji);
end;

end.



