unit x86_jit;

{
 x86-64 minimal JIT Compiler by Red-prig
}

{$mode ObjFPC}{$H+}

interface

uses
 x86_fpdbgdisas;

type
 t_jit_reg=object
  ARegValue:TRegValues;
  ASegment :ShortInt;
  AOffset  :Int64;
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
   ah  :t_jit_reg=(ARegValue:((AType:regGeneralH;ASize:os8;AIndex:0),(AType:regNone)));
   ch  :t_jit_reg=(ARegValue:((AType:regGeneralH;ASize:os8;AIndex:1),(AType:regNone)));
   dh  :t_jit_reg=(ARegValue:((AType:regGeneralH;ASize:os8;AIndex:2),(AType:regNone)));
   bh  :t_jit_reg=(ARegValue:((AType:regGeneralH;ASize:os8;AIndex:3),(AType:regNone)));

   al  :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex: 0),(AType:regNone)));
   cl  :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex: 1),(AType:regNone)));
   dl  :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex: 2),(AType:regNone)));
   bl  :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex: 3),(AType:regNone)));
   spl :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex: 4),(AType:regNone)));
   bpl :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex: 5),(AType:regNone)));
   sil :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex: 6),(AType:regNone)));
   dil :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex: 7),(AType:regNone)));
   r8b :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex: 8),(AType:regNone)));
   r9b :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex: 9),(AType:regNone)));
   r10b:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex:10),(AType:regNone)));
   r11b:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex:11),(AType:regNone)));
   r12b:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex:12),(AType:regNone)));
   r13b:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex:13),(AType:regNone)));
   r14b:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex:14),(AType:regNone)));
   r15b:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os8;AIndex:15),(AType:regNone)));

   ax  :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex: 0),(AType:regNone)));
   cx  :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex: 1),(AType:regNone)));
   dx  :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex: 2),(AType:regNone)));
   bx  :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex: 3),(AType:regNone)));
   sp  :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex: 4),(AType:regNone)));
   bp  :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex: 5),(AType:regNone)));
   si  :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex: 6),(AType:regNone)));
   di  :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex: 7),(AType:regNone)));
   r8w :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex: 8),(AType:regNone)));
   r9w :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex: 9),(AType:regNone)));
   r10w:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex:10),(AType:regNone)));
   r11w:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex:11),(AType:regNone)));
   r12w:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex:12),(AType:regNone)));
   r13w:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex:13),(AType:regNone)));
   r14w:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex:14),(AType:regNone)));
   r15w:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os16;AIndex:15),(AType:regNone)));

   eax :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex: 0),(AType:regNone)));
   ecx :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex: 1),(AType:regNone)));
   edx :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex: 2),(AType:regNone)));
   ebx :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex: 3),(AType:regNone)));
   esp :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex: 4),(AType:regNone)));
   ebp :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex: 5),(AType:regNone)));
   esi :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex: 6),(AType:regNone)));
   edi :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex: 7),(AType:regNone)));
   r8d :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex: 8),(AType:regNone)));
   r9d :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex: 9),(AType:regNone)));
   r10d:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex:10),(AType:regNone)));
   r11d:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex:11),(AType:regNone)));
   r12d:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex:12),(AType:regNone)));
   r13d:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex:13),(AType:regNone)));
   r14d:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex:14),(AType:regNone)));
   r15d:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os32;AIndex:15),(AType:regNone)));

   rax:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex: 0),(AType:regNone)));
   rcx:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex: 1),(AType:regNone)));
   rdx:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex: 2),(AType:regNone)));
   rbx:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex: 3),(AType:regNone)));
   rsp:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex: 4),(AType:regNone)));
   rbp:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex: 5),(AType:regNone)));
   rsi:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex: 6),(AType:regNone)));
   rdi:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex: 7),(AType:regNone)));
   r8 :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex: 8),(AType:regNone)));
   r9 :t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex: 9),(AType:regNone)));
   r10:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex:10),(AType:regNone)));
   r11:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex:11),(AType:regNone)));
   r12:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex:12),(AType:regNone)));
   r13:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex:13),(AType:regNone)));
   r14:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex:14),(AType:regNone)));
   r15:t_jit_reg=(ARegValue:((AType:regGeneral;ASize:os64;AIndex:15),(AType:regNone)));

   rip:t_jit_reg=(ARegValue:((AType:regRip;ASize:os64),(AType:regNone)));

   FS :t_jit_reg=(ARegValue:((AType:regNone),(AType:regNone));ASegment:4);
   GS :t_jit_reg=(ARegValue:((AType:regNone),(AType:regNone));ASegment:5);

   xmm0 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex:  0),(AType:regNone)));
   xmm1 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex:  1),(AType:regNone)));
   xmm2 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex:  2),(AType:regNone)));
   xmm3 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex:  3),(AType:regNone)));
   xmm4 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex:  4),(AType:regNone)));
   xmm5 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex:  5),(AType:regNone)));
   xmm6 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex:  6),(AType:regNone)));
   xmm7 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex:  7),(AType:regNone)));
   xmm8 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex:  8),(AType:regNone)));
   xmm9 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex:  9),(AType:regNone)));
   xmm10:t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex: 10),(AType:regNone)));
   xmm11:t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex: 11),(AType:regNone)));
   xmm12:t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex: 12),(AType:regNone)));
   xmm13:t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex: 13),(AType:regNone)));
   xmm14:t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex: 14),(AType:regNone)));
   xmm15:t_jit_reg=(ARegValue:((AType:regXmm;ASize:os128;AIndex: 15),(AType:regNone)));

   ymm0 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex:  0),(AType:regNone)));
   ymm1 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex:  1),(AType:regNone)));
   ymm2 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex:  2),(AType:regNone)));
   ymm3 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex:  3),(AType:regNone)));
   ymm4 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex:  4),(AType:regNone)));
   ymm5 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex:  5),(AType:regNone)));
   ymm6 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex:  6),(AType:regNone)));
   ymm7 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex:  7),(AType:regNone)));
   ymm8 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex:  8),(AType:regNone)));
   ymm9 :t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex:  9),(AType:regNone)));
   ymm10:t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex: 10),(AType:regNone)));
   ymm11:t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex: 11),(AType:regNone)));
   ymm12:t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex: 12),(AType:regNone)));
   ymm13:t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex: 13),(AType:regNone)));
   ymm14:t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex: 14),(AType:regNone)));
   ymm15:t_jit_reg=(ARegValue:((AType:regXmm;ASize:os256;AIndex: 15),(AType:regNone)));
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
  function  movj(reg:t_jit_reg;mem:t_jit_regs;_label_id:Integer):t_jit_i_link;
  function  leaj(reg:t_jit_reg;mem:t_jit_regs;_label_id:Integer):t_jit_i_link;
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
  procedure _mov    (op,op8:Byte;reg:t_jit_reg;mem:t_jit_regs);
  procedure _mov    (op,op8:Byte;reg0:t_jit_reg;reg1:t_jit_reg);
  procedure _movi   (op,op8,index:Byte;reg:t_jit_reg;imm:Int64);
  procedure _movi   (op,op8,index:Byte;size:TOperandSize;mem:t_jit_regs;imm:Int64);
  procedure _movi8  (op,index:Byte;reg:t_jit_reg;imm:Byte);
  procedure _movi8  (op,index:Byte;size:TOperandSize;mem:t_jit_regs;imm:Byte);
  procedure _push   (op,index:Byte;size:TOperandSize;mem:t_jit_regs);
  procedure _push   (op:Byte;reg:t_jit_reg);
  procedure _pushi  (size:TOperandSize;imm:Integer);
  procedure movq    (reg0:t_jit_reg ;reg1:t_jit_reg);
  procedure movi    (size:TOperandSize;mem:t_jit_regs;imm:Int64);
  procedure movi    (reg:t_jit_reg  ;imm:Int64);
  procedure movq    (reg:t_jit_reg  ;mem:t_jit_regs);
  procedure movq    (mem:t_jit_regs ;reg:t_jit_reg);
  procedure leaq    (reg:t_jit_reg  ;mem:t_jit_regs);
  procedure addq    (mem:t_jit_regs ;reg:t_jit_reg);
  procedure addq    (reg:t_jit_reg  ;mem:t_jit_regs);
  procedure addq    (reg0:t_jit_reg ;reg1:t_jit_reg);
  procedure addi    (reg:t_jit_reg  ;imm:Int64);
  procedure addi8   (reg:t_jit_reg  ;imm:Byte);
  procedure subq    (mem:t_jit_regs ;reg:t_jit_reg);
  procedure subq    (reg:t_jit_reg  ;mem:t_jit_regs);
  procedure subq    (reg0:t_jit_reg ;reg1:t_jit_reg);
  procedure subi    (reg:t_jit_reg  ;imm:Int64);
  procedure subi8   (reg:t_jit_reg  ;imm:Byte);
  procedure xorq    (reg0:t_jit_reg ;reg1:t_jit_reg);
  procedure cmpq    (mem:t_jit_regs ;reg:t_jit_reg);
  procedure cmpq    (reg:t_jit_reg  ;mem:t_jit_regs);
  procedure cmpq    (reg0:t_jit_reg ;reg1:t_jit_reg);
  procedure cmpi    (reg:t_jit_reg  ;imm:Int64);
  procedure cmpi    (size:TOperandSize;mem:t_jit_regs;imm:Int64);
  procedure cmpi8   (reg:t_jit_reg;imm:Byte);
  procedure cmpi8   (size:TOperandSize;mem:t_jit_regs;imm:Byte);
  procedure push16  (mem:t_jit_regs);
  procedure push64  (mem:t_jit_regs);
  procedure push    (reg:t_jit_reg);
  procedure push8   (imm:Integer);
  procedure push16  (imm:Integer);
  procedure push32  (imm:Integer);
  procedure pop16   (mem:t_jit_regs);
  procedure pop64   (mem:t_jit_regs);
  procedure pop     (reg:t_jit_reg);
  procedure _vmov   (Op,SimdOpcode:Byte;reg:t_jit_reg;mem:t_jit_regs);
  procedure vmovdqu (reg:t_jit_reg ;mem:t_jit_regs);
  procedure vmovdqu (mem:t_jit_regs;reg:t_jit_reg);
  procedure vmovdqa (reg:t_jit_reg ;mem:t_jit_regs);
  procedure vmovdqa (mem:t_jit_regs;reg:t_jit_reg);
  procedure vmovntdq(mem:t_jit_regs;reg:t_jit_reg);
  procedure vmovups (reg:t_jit_reg ;mem:t_jit_regs);
  procedure vmovups (mem:t_jit_regs;reg:t_jit_reg);
  procedure vmovdqa (reg0:t_jit_reg;reg1:t_jit_reg);
  procedure sahf;
  procedure lahf;
 end;

operator + (A,B:t_jit_reg):t_jit_reg;
operator + (A:t_jit_reg;B:Integer):t_jit_reg;
operator + (A:t_jit_reg;B:Int64):t_jit_reg;
operator * (A:t_jit_reg;B:Integer):t_jit_reg;

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

function classif_offset_32(reg:t_jit_reg):Byte; inline;
begin
 case reg.AOffset of
                0:Result:=0;
  -128..-1,1..127:Result:=1;
  else
                  Result:=2;
 end;
end;

function classif_offset_64(reg:t_jit_reg):Byte; inline;
begin
 case reg.AOffset of
                                  0:Result:=0;
  -128       ..  -1,  1..127       :Result:=1;
  -2147483648..-129,128..2147483647:Result:=2;
  else
                                    Result:=3;
 end;
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

operator + (A,B:t_jit_reg):t_jit_reg;
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

 Result.AOffset:=Result.AOffset+B.AOffset;
end;

operator + (A:t_jit_reg;B:Integer):t_jit_reg;
begin
 Result:=A;

 Result.AOffset:=Result.AOffset+B;
end;

operator + (A:t_jit_reg;B:Int64):t_jit_reg;
begin
 Result:=A;

 Result.AOffset:=Result.AOffset+B;
end;

operator * (A:t_jit_reg;B:Integer):t_jit_reg;
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
 if (not rexW) then b:=b or $80;
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

function t_jit_builder.movj(reg:t_jit_reg;mem:t_jit_regs;_label_id:Integer):t_jit_i_link;
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

function t_jit_builder.leaj(reg:t_jit_reg;mem:t_jit_regs;_label_id:Integer):t_jit_i_link;
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
  rexF,rexB,rexX,rexR:Boolean;

  ModRM:record
   Mode,Index,RM:Byte;
  end;

  SIB:record
   Scale,Index,Base:Byte;
  end;

  AOffset:Int64;

  procedure build(Index:Byte;mreg:t_jit_reg);
  procedure build(reg,mreg:t_jit_reg);
  procedure emit(var ji:t_jit_instruction);
 end;

procedure t_modrm_info.build(Index:Byte;mreg:t_jit_reg);
var
 ubase:Boolean;
begin
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
    rexX:=true;
    Dec(SIB.Index,8);
   end;

   if ubase then
   begin
    SIB.Base:=mreg.ARegValue[1].AIndex;
    if (SIB.Base>=8) then
    begin
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

function get_force_rex(const reg:t_jit_reg):Boolean; inline;
begin
 Result:=False;
 if (reg.ARegValue[0].AType=regGeneral) then
 if (reg.ARegValue[0].ASize=os8) then
  case reg.ARegValue[0].AIndex of
   4..7:Result:=True;
   else;
  end;
end;

procedure t_modrm_info.build(reg,mreg:t_jit_reg);
begin
 ModRM.Index:=reg.ARegValue[0].AIndex;

 case reg.ARegValue[0].AType of
  regGeneralH:
   begin
    ModRM.Index:=ModRM.Index+4;
   end;
  else
   begin
    if (ModRM.Index>=8) then
    begin
     rexR:=true;
     Dec(ModRM.Index,8);
    end;
   end;
 end;

 rexF:=get_force_rex(reg);

 build(ModRM.Index,mreg);
end;

procedure t_modrm_info.emit(var ji:t_jit_instruction);
begin
 ji.EmitModRM(ModRM.Mode,ModRM.Index,ModRM.RM);

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
  1:ji.EmitByte (AOffset); //1
  2:begin
     ji.ALink.ADataOffset:=ji.ASize;
     ji.EmitInt32(AOffset); //4
    end;
  else;
 end;
end;

procedure t_jit_builder._mov(op,op8:Byte;reg:t_jit_reg;mem:t_jit_regs);
var
 mreg:t_jit_reg;

 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(is_one_reg(reg));
 Assert(is_reg_size(reg,[os8,os16,os32,os64]));

 Assert(is_reg_type(reg,[regGeneral,regGeneralH]));

 Assert(reg.ARegValue[0].AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 case reg.ARegValue[0].ASize of
   os8:
       begin
        op:=op8;
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

 modrm_info.build(reg,mreg);

 ji.EmitSelector(mreg.ASegment);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16)
 end;

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 if modrm_info.rexF or modrm_info.rexB or modrm_info.rexX or modrm_info.rexR or rexW then
 begin
  ji.EmitREX(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,rexW);
 end;

 ji.EmitByte(op);

 modrm_info.emit(ji);

 _add(ji);
end;

procedure t_jit_builder._mov(op,op8:Byte;reg0:t_jit_reg;reg1:t_jit_reg);
var
 rexF,rexB,rexR,rexW:Boolean;

 ModRM:record
  Index,RM:Byte;
 end;

 Prefix:Byte;

 ji:t_jit_instruction;
begin
 Assert(is_one_reg(reg0));
 Assert(is_one_reg(reg1));

 Assert(is_reg_size(reg0,[os8,os16,os32,os64]));
 Assert(is_reg_size(reg1,[os8,os16,os32,os64]));

 Assert(is_reg_type(reg0,[regGeneral]));
 Assert(is_reg_type(reg1,[regGeneral]));

 Assert(reg0.ARegValue[0].AScale<=1);
 Assert(reg1.ARegValue[0].AScale<=1);

 Assert(reg0.ARegValue[0].ASize=reg1.ARegValue[0].ASize);

 Assert(get_force_rex(reg0)=get_force_rex(reg1));

 ji:=default_jit_instruction;

 rexF:=false;
 rexB:=false;
 rexR:=false;
 rexW:=false;
 Prefix:=0;

 case reg0.ARegValue[0].ASize of
   os8:
       begin
        op:=op8;
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

 ModRM.Index:=reg1.ARegValue[0].AIndex;
 if (ModRM.Index>=8) then
 begin
  rexR:=true;
  Dec(ModRM.Index,8);
 end;

 ModRM.RM:=reg0.ARegValue[0].AIndex;
 if (ModRM.RM>=8) then
 begin
  rexB:=true;
  Dec(ModRM.RM,8);
 end;

 rexF:=get_force_rex(reg0);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16)
 end;

 if rexF or rexB or rexR or rexW then
 begin
  ji.EmitREX(rexB,False,rexR,rexW);
 end;

 ji.EmitByte(op);

 ji.EmitModRM(3,ModRM.Index,ModRM.RM);

 _add(ji);
end;

////

procedure t_jit_builder._movi(op,op8,index:Byte;reg:t_jit_reg;imm:Int64);
var
 rexF,rexB,rexW:Boolean;

 RM,Prefix:Byte;

 ji:t_jit_instruction;
begin
 Assert(is_one_reg(reg));
 Assert(is_reg_size(reg,[os8,os16,os32,os64]));

 Assert(is_reg_type(reg,[regGeneral]));

 Assert(reg.ARegValue[0].AScale<=1);

 ji:=default_jit_instruction;

 rexF:=false;
 rexB:=false;
 rexW:=false;
 Prefix:=0;

 RM:=reg.ARegValue[0].AIndex;
 if (RM>=8) then
 begin
  rexB:=true;
  Dec(RM,8);
 end;

 rexF:=get_force_rex(reg);

 case reg.ARegValue[0].ASize of
   os8:
       begin
        op:=op8;
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

 if rexF or rexB or rexW then
 begin
  ji.EmitREX(rexB,False,False,rexW);
 end;

 ji.EmitByte(op);

 ji.EmitModRM(3,index,RM);

 case reg.ARegValue[0].ASize of
   os8:ji.EmitByte (imm);
  os16:ji.EmitWord (imm);
  os32:ji.EmitInt32(imm);
  os64:ji.EmitInt32(imm);
  else;
 end;

 _add(ji);
end;

procedure t_jit_builder._movi(op,op8,index:Byte;size:TOperandSize;mem:t_jit_regs;imm:Int64);
var
 mreg:t_jit_reg;

 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(size in [os8,os16,os32,os64]);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

 case size of
   os8:
       begin
        op:=op8;
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

 modrm_info.build(Index,mreg);

 ji.EmitSelector(mreg.ASegment);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16)
 end;

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 if modrm_info.rexF or modrm_info.rexB or modrm_info.rexX or modrm_info.rexR or rexW then
 begin
  ji.EmitREX(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,rexW);
 end;

 ji.EmitByte(op);

 modrm_info.emit(ji);

 case Size of
   os8:ji.EmitByte (imm);
  os16:ji.EmitWord (imm);
  os32:ji.EmitInt32(imm);
  os64:ji.EmitInt64(imm);
  else;
 end;

 _add(ji);
end;

procedure t_jit_builder._movi8(op,index:Byte;reg:t_jit_reg;imm:Byte);
var
 rexF,rexB,rexW:Boolean;

 RM,Prefix:Byte;

 ji:t_jit_instruction;
begin
 Assert(is_one_reg(reg));
 Assert(is_reg_size(reg,[os16,os32,os64]));

 Assert(is_reg_type(reg,[regGeneral]));

 Assert(reg.ARegValue[0].AScale<=1);

 ji:=default_jit_instruction;

 rexF:=false;
 rexB:=false;
 rexW:=false;
 Prefix:=0;

 RM:=reg.ARegValue[0].AIndex;
 if (RM>=8) then
 begin
  rexB:=true;
  Dec(RM,8);
 end;

 rexF:=get_force_rex(reg);

 case reg.ARegValue[0].ASize of
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

 if rexF or rexB or rexW then
 begin
  ji.EmitREX(rexB,False,False,rexW);
 end;

 ji.EmitByte(op);

 ji.EmitModRM(3,index,RM);

 ji.EmitByte(imm);

 _add(ji);
end;

procedure t_jit_builder._movi8(op,index:Byte;size:TOperandSize;mem:t_jit_regs;imm:Byte);
var
 mreg:t_jit_reg;

 rexW:Boolean;
 Prefix:Byte;

 modrm_info:t_modrm_info;

 ji:t_jit_instruction;
begin
 Assert(size in [os16,os32,os64]);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 rexW:=False;
 Prefix:=0;

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

 modrm_info.build(Index,mreg);

 ji.EmitSelector(mreg.ASegment);

 if (Prefix<>0) then
 begin
  ji.EmitByte(Prefix); //Operand-size override prefix (16)
 end;

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 if modrm_info.rexF or modrm_info.rexB or modrm_info.rexX or modrm_info.rexR or rexW then
 begin
  ji.EmitREX(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,rexW);
 end;

 ji.EmitByte(op);

 modrm_info.emit(ji);

 ji.EmitByte(imm); //1

 _add(ji);
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

 modrm_info.build(index,mreg);

 ji.EmitSelector(mreg.ASegment);

 if (size=os16) then
 begin
  ji.EmitByte($66); //Operand-size override prefix (16)
 end;

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 if modrm_info.rexF or modrm_info.rexB or modrm_info.rexX or modrm_info.rexR then
 begin
  ji.EmitREX(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,False);
 end;

 ji.EmitByte(op);

 modrm_info.emit(ji);

 _add(ji);
end;

procedure t_jit_builder._push(op:Byte;reg:t_jit_reg);
var
 rexB:Boolean;

 Index:Byte;

 Prefix:Byte;

 ji:t_jit_instruction;
begin
 Assert(is_one_reg(reg));

 Assert(is_reg_size(reg,[os16,os64]));

 Assert(is_reg_type(reg,[regGeneral]));

 Assert(reg.ARegValue[0].AScale<=1);

 ji:=default_jit_instruction;

 rexB:=false;
 Prefix:=0;

 Index:=reg.ARegValue[0].AIndex;
 if (Index>=8) then
 begin
  rexB:=true;
  Dec(Index,8);
 end;

 case reg.ARegValue[0].ASize of
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

procedure t_jit_builder.movq(reg0:t_jit_reg;reg1:t_jit_reg);
begin
 _mov($89,$88,reg0,reg1);
end;

procedure t_jit_builder.movi(size:TOperandSize;mem:t_jit_regs;imm:Int64);
begin
 _movi($C7,$C6,0,size,mem,imm);
end;

procedure t_jit_builder.movi(reg:t_jit_reg;imm:Int64);
var
 rexF,rexB,rexW:Boolean;

 Index,Prefix,op:Byte;

 ji:t_jit_instruction;
begin
 Assert(is_one_reg(reg));
 Assert(is_reg_size(reg,[os8,os16,os32,os64]));

 Assert(is_reg_type(reg,[regGeneral]));

 Assert(reg.ARegValue[0].AScale<=1);

 ji:=default_jit_instruction;

 rexF:=false;
 rexB:=false;
 rexW:=false;
 Prefix:=0;

 Index:=reg.ARegValue[0].AIndex;
 if (Index>=8) then
 begin
  rexB:=true;
  Dec(Index,8);
 end;

 rexF:=get_force_rex(reg);

 case reg.ARegValue[0].ASize of
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

 case reg.ARegValue[0].ASize of
   os8:ji.EmitByte (imm);
  os16:ji.EmitWord (imm);
  os32:ji.EmitInt32(imm);
  os64:ji.EmitInt32(imm);
  else;
 end;

 _add(ji);
end;

procedure t_jit_builder.movq(reg:t_jit_reg;mem:t_jit_regs);
begin
 _mov($8B,$8A,reg,mem); //MOV r64, r/m64
end;

procedure t_jit_builder.movq(mem:t_jit_regs;reg:t_jit_reg);
begin
 _mov($89,$88,reg,mem); //MOV r/m64, r64
end;

//

procedure t_jit_builder.leaq(reg:t_jit_reg;mem:t_jit_regs);
begin
 Assert(is_reg_size(reg,[os16,os32,os64]));

 _mov($8D,$8D,reg,mem); //LEA r64,m
end;

//

procedure t_jit_builder.addq(mem:t_jit_regs;reg:t_jit_reg);
begin
 _mov($01,$00,reg,mem); //ADD r/m64, r64
end;

procedure t_jit_builder.addq(reg:t_jit_reg;mem:t_jit_regs);
begin
 _mov($03,$02,reg,mem); //ADD r64, r/m64
end;

procedure t_jit_builder.addq(reg0:t_jit_reg;reg1:t_jit_reg);
begin
 _mov($01,$00,reg0,reg1);
end;

procedure t_jit_builder.addi(reg:t_jit_reg;imm:Int64);
begin
 _movi($81,$80,0,reg,imm);
end;

procedure t_jit_builder.addi8(reg:t_jit_reg;imm:Byte);
begin
 _movi8($83,0,reg,imm);
end;

//

procedure t_jit_builder.subq(mem:t_jit_regs;reg:t_jit_reg);
begin
 _mov($29,$28,reg,mem); //SUB r/m64, r64
end;

procedure t_jit_builder.subq(reg:t_jit_reg;mem:t_jit_regs);
begin
 _mov($2B,$2A,reg,mem); //SUB r64, r/m64
end;

procedure t_jit_builder.subq(reg0:t_jit_reg;reg1:t_jit_reg);
begin
 _mov($29,$28,reg0,reg1);
end;

procedure t_jit_builder.subi(reg:t_jit_reg;imm:Int64);
begin
 _movi($81,$80,5,reg,imm);
end;

procedure t_jit_builder.subi8(reg:t_jit_reg;imm:Byte);
begin
 _movi8($83,5,reg,imm);
end;

///

procedure t_jit_builder.xorq(reg0:t_jit_reg;reg1:t_jit_reg);
begin
 _mov($31,$30,reg0,reg1);
end;

///

procedure t_jit_builder.cmpq(mem:t_jit_regs;reg:t_jit_reg);
begin
 _mov($39,$38,reg,mem);
end;

procedure t_jit_builder.cmpq(reg:t_jit_reg;mem:t_jit_regs);
begin
 _mov($3B,$3A,reg,mem);
end;

procedure t_jit_builder.cmpq(reg0:t_jit_reg;reg1:t_jit_reg);
begin
 _mov($39,$38,reg0,reg1);
end;

procedure t_jit_builder.cmpi(reg:t_jit_reg;imm:Int64);
begin
 _movi($81,$80,7,reg,imm);
end;

procedure t_jit_builder.cmpi(size:TOperandSize;mem:t_jit_regs;imm:Int64);
begin
 _movi($81,$80,7,size,mem,imm);
end;

procedure t_jit_builder.cmpi8(reg:t_jit_reg;imm:Byte);
begin
 _movi8($83,7,reg,imm);
end;

procedure t_jit_builder.cmpi8(size:TOperandSize;mem:t_jit_regs;imm:Byte);
begin
 _movi8($83,7,size,mem,imm);
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

procedure t_jit_builder.push(reg:t_jit_reg);
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

procedure t_jit_builder.pop(reg:t_jit_reg);
begin
 _push($58,reg);
end;

procedure t_jit_builder._vmov(Op,SimdOpcode:Byte;reg:t_jit_reg;mem:t_jit_regs);
var
 mreg:t_jit_reg;

 modrm_info:t_modrm_info;

 Vex:record
  Length:Byte;
 end;

 ji:t_jit_instruction;
begin
 Assert(is_one_reg(reg));
 Assert(is_reg_size(reg,[os128,os256]));

 Assert(is_reg_type(reg,[regXmm]));

 Assert(reg.ARegValue[0].AScale<=1);

 mreg:=Sums(mem);

 Assert(is_reg_size(mreg,[os0,os32,os64]));
 Assert(is_reg_type(mreg,[regNone,regGeneral,regRip]));
 Assert(is_valid_scale(mreg));

 ji:=default_jit_instruction;

 case reg.ARegValue[0].ASize of
  os128:Vex.Length:=0;
  os256:Vex.Length:=1;
  else;
 end;

 modrm_info:=Default(t_modrm_info);

 modrm_info.build(reg,mreg);

 ji.EmitSelector(mreg.ASegment);

 if (mreg.ARegValue[0].ASize=os32) then
 begin
  ji.EmitByte($67); //Address-size override prefix (32)
 end;

 if modrm_info.rexB or modrm_info.rexX then
 begin
  ji.EmitByte($C4); //VEX3

  ji.EmitRXBm(modrm_info.rexB,modrm_info.rexX,modrm_info.rexR,1);
  ji.EmitWvvv(False,0,Vex.Length,SimdOpcode);
 end else
 begin
  ji.EmitByte($C5); //VEX2

  ji.EmitRvvv(modrm_info.rexR,0,Vex.Length,SimdOpcode);
 end;

 ji.EmitByte(Op);

 modrm_info.emit(ji);

 _add(ji);
end;

procedure t_jit_builder.vmovdqu(reg:t_jit_reg;mem:t_jit_regs);
begin
 _vmov($6F,2,reg,mem);
end;

procedure t_jit_builder.vmovdqu(mem:t_jit_regs;reg:t_jit_reg);
begin
 _vmov($7F,2,reg,mem);
end;

procedure t_jit_builder.vmovdqa(reg:t_jit_reg;mem:t_jit_regs);
begin
 _vmov($6F,1,reg,mem);
end;

procedure t_jit_builder.vmovdqa(mem:t_jit_regs;reg:t_jit_reg);
begin
 _vmov($7F,1,reg,mem);
end;

procedure t_jit_builder.vmovntdq(mem:t_jit_regs;reg:t_jit_reg);
begin
 _vmov($E7,1,reg,mem);
end;

procedure t_jit_builder.vmovups(reg:t_jit_reg;mem:t_jit_regs);
begin
 _vmov($10,0,reg,mem);
end;

procedure t_jit_builder.vmovups(mem:t_jit_regs;reg:t_jit_reg);
begin
 _vmov($11,0,reg,mem);
end;

procedure t_jit_builder.vmovdqa(reg0:t_jit_reg;reg1:t_jit_reg);
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
 Assert(is_one_reg(reg0));
 Assert(is_one_reg(reg1));

 Assert(is_reg_size(reg0,[os128,os256]));
 Assert(is_reg_size(reg1,[os128,os256]));

 Assert(is_reg_type(reg0,[regXmm]));
 Assert(is_reg_type(reg1,[regXmm]));

 Assert(reg0.ARegValue[0].AScale<=1);
 Assert(reg1.ARegValue[0].AScale<=1);

 Assert(reg0.ARegValue[0].ASize=reg1.ARegValue[0].ASize);

 ji:=default_jit_instruction;

 rexB:=false;
 rexX:=false;
 rexR:=false;

 ModRM.Index:=reg0.ARegValue[0].AIndex;
 if (ModRM.Index>=8) then
 begin
  rexR:=true;
  Dec(ModRM.Index,8);
 end;

 ModRM.RM:=reg1.ARegValue[0].AIndex;
 if (ModRM.RM>=8) then
 begin
  rexB:=true;
  Dec(ModRM.RM,8);
 end;

 case reg0.ARegValue[0].ASize of
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



