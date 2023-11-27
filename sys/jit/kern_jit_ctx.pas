unit kern_jit_ctx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 g_node_splay,
 x86_fpdbgdisas,
 x86_jit;

type
 t_point_type=(fpCall,fpData,fpInvalid);

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
    ptype:t_point_type;
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
  function  is_map_addr (addr:QWORD):Boolean;
  procedure add_forward_link (node:p_forward_point;label_id:t_jit_i_link);
  function  add_forward_point(ptype:t_point_type;label_id:t_jit_i_link;dst:Pointer):p_forward_point;
  function  add_forward_point(ptype:t_point_type;dst:Pointer):p_forward_point;
  Function  new_chunk(ptype:t_point_type;start:Pointer):p_jit_code_chunk;
  procedure mark_chunk(ptype:t_point_type);
  function  get_chunk_ptype():t_point_type;
  procedure end_chunk(__end:Pointer);
  function  max_forward_point():Pointer;
  function  fetch_forward_point(var links:t_forward_links;var dst:Pointer):Boolean;
  function  add_label(curr,next:Pointer;link_curr,link_next:t_jit_i_link):p_label;
  function  get_label(src:Pointer):p_label;
  function  get_link (src:Pointer):t_jit_i_link;
  procedure add_entry_point(src:Pointer;label_id:t_jit_i_link);
  procedure Free;
 end;

const
 r_thrd:TRegValue=(AType:regGeneral;ASize:os64;AIndex:13); //r13
 r_tmp0:TRegValue=(AType:regGeneral;ASize:os64;AIndex:14); //r14
 r_tmp1:TRegValue=(AType:regGeneral;ASize:os64;AIndex:15); //r15

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
function is_high(const r:TOperand):Boolean;
function is_rsp(const r:TRegValue):Boolean;
function is_rsp(const r:TRegValues):Boolean;
function is_invalid(const r:TRegValue):Boolean;
function is_invalid(const r:TInstruction):Boolean;

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
function  cmp_reg(const r1,r2:TOperand):Boolean;
function  cmp_reg_cross(const r1,r2:TRegValue):Boolean;
function  new_reg(const Operand:TOperand):TRegValue;
function  new_reg_size(const r:TRegValue;ASize:TOperandSize):TRegValue;
function  new_reg_size(const r:TRegValue;const RegValue:TRegValues):TRegValue;
function  new_reg_size(const r:TRegValue;const Operand:TOperand):TRegValue;
function  fix_size(const r:TRegValue):TRegValue;
function  is_rep_prefix(const i:TInstruction):Boolean;
function  is_segment(const r:TOperand):Boolean; inline;
function  get_segment_value(const Operand:TOperand):Byte;
function  flags(const i:TInstruction):t_jit_lea;
function  flags(const ctx:t_jit_context2):t_jit_lea;

procedure op_load_r14(var ctx:t_jit_context2;reg:TRegValue);
procedure op_save_r14(var ctx:t_jit_context2;reg:TRegValue);

procedure op_load_rsp(var ctx:t_jit_context2;reg:TRegValue);
procedure op_save_rsp(var ctx:t_jit_context2;reg:TRegValue);

procedure op_load_rbp(var ctx:t_jit_context2;reg:TRegValue);
procedure op_save_rbp(var ctx:t_jit_context2;reg:TRegValue);

procedure op_load(var ctx:t_jit_context2;reg:TRegValue;opr:Byte);
procedure op_save(var ctx:t_jit_context2;opr:Byte;reg:TRegValue);

procedure add_orig(var ctx:t_jit_context2);
procedure op_emit1(var ctx:t_jit_context2;const desc:t_op_type;hint:t_op_hint);
procedure op_emit2(var ctx:t_jit_context2;const desc:t_op_desc);
procedure op_emit_shift2(var ctx:t_jit_context2;const desc:t_op_shift);
procedure op_emit_shift3(var ctx:t_jit_context2;const desc:t_op_shift);
procedure op_emit_avx1(var ctx:t_jit_context2;const desc:t_op_type;hint:t_op_hint);
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
 vm_pmap,
 machdep,
 kern_thr,
 kern_jit_asm;

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

function t_jit_context2.add_forward_point(ptype:t_point_type;label_id:t_jit_i_link;dst:Pointer):p_forward_point;
var
 node:t_forward_point;
begin
 if (dst=nil) then Exit;

 case get_chunk_ptype of
  fpData   :if (ptype=fpCall) then ptype:=fpData;
  fpInvalid:ptype:=fpInvalid;
  else;
 end;

 node.dst:=dst;
 Result:=forward_set.Find(@node);
 if (Result=nil) then
 begin
  Result:=builder.Alloc(Sizeof(t_forward_point));
  Result^.dst:=dst;
  Result^.links.ptype:=ptype;
  forward_set.Insert(Result);
 end;
 add_forward_link(Result,label_id);
end;

function t_jit_context2.add_forward_point(ptype:t_point_type;dst:Pointer):p_forward_point;
begin
 Result:=add_forward_point(ptype,nil_link,dst);
end;

Function t_jit_context2.new_chunk(ptype:t_point_type;start:Pointer):p_jit_code_chunk;
begin
 Result:=builder._new_chunk(QWORD(start));
 if (Result<>nil) then
 begin
  Result^.data:=QWORD(ptype);
 end;
end;

procedure t_jit_context2.mark_chunk(ptype:t_point_type);
var
 node:p_jit_code_chunk;
begin
 node:=builder.ACodeChunkCurr;
 if (node<>nil) then
 if (t_point_type(node^.data)=fpData) then
 begin
  node^.data:=QWORD(ptype);
 end;
end;

function t_jit_context2.get_chunk_ptype():t_point_type;
var
 node:p_jit_code_chunk;
begin
 Result:=fpCall;
 node:=builder.ACodeChunkCurr;
 if (node<>nil) then
 begin
  Result:=t_point_type(node^.data);
 end;
end;

procedure t_jit_context2.end_chunk(__end:Pointer);
begin
 builder._end_chunk(QWORD(__end));
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
   case RegValue.AIndex of
     4:Result:=Integer(@p_jit_frame(nil)^.tf_rsp);
     5:Result:=Integer(@p_jit_frame(nil)^.tf_rbp);
    13:Result:=Integer(@p_jit_frame(nil)^.tf_r13);
    14:Result:=Integer(@p_jit_frame(nil)^.tf_r14);
    15:Result:=Integer(@p_jit_frame(nil)^.tf_r15);
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

function is_preserved(AIndex:Byte):Boolean; inline;
begin
 Result:=AIndex in [4,5,13..15];
 //  4 rsp
 //  5 rbp
 // 13 r13
 // 14 r14
 // 15 r15
end;

function is_preserved(const r:TRegValue):Boolean;
begin
 Result:=False;

 case r.AType of
  regRip    :Result:=True;
  regGeneral:Result:=is_preserved(r.AIndex);
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

function is_high(const r:TOperand):Boolean; inline;
begin
 Result:=r.RegValue[0].AType=regGeneralH;
end;

function cmp_reg(const r1,r2:TRegValue):Boolean; inline;
begin
 Result:=(r1.AType =r2.AType) and
         (r1.ASize =r2.ASize) and
         (r1.AIndex=r2.AIndex);
end;

function cmp_reg(const r1,r2:TOperand):Boolean; inline;
begin
 Result:=cmp_reg(r1.RegValue[0],r2.RegValue[0]);
end;

function cmp_reg_cross(const r1,r2:TRegValue):Boolean; inline;
begin
 Result:=(
          (r1.AType=r2.AType) or
          (
           (r1.AType in [regGeneral, regGeneralH]) and
           (r2.AType in [regGeneral, regGeneralH])
          )
         ) and
         (r1.AIndex=r2.AIndex);
end;

function cmp_reg_cross(const r1:TRegValue;const Operand:TOperand):Boolean; inline;
begin
 Result:=cmp_reg_cross(r1,Operand.RegValue[0]);
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

function fix_size8(const r:TRegValue):TRegValue; inline;
begin
 Result:=r;
 case Result.ASize of
  os8 :Result.ASize:=os16;
  os32:Result.ASize:=os64;
 end;
end;

function is_rep_prefix(const i:TInstruction):Boolean;
begin
 Result:=[ifPrefixRep,ifPrefixRepE,ifPrefixRepNe]*i.Flags<>[];
end;

function is_segment(const r:TOperand):Boolean; inline;
begin
 Result:=(r.RegValue[0].AType=regSegment) and
         (not (ofMemory in r.Flags));
end;

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

function flags(const i:TInstruction):t_jit_lea;
begin
 Result:=Default(t_jit_lea);

 if (ifPrefixLock in i.Flags) then
 begin
  Result:=Result+t_jit_builder.LOCK;
 end;
end;

function flags(const ctx:t_jit_context2):t_jit_lea; inline;
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

function is_invalid(const r:TRegValue):Boolean;
begin
 Result:=False;
 case r.AType of
  regInvalid:Result:=True;

  regGeneral:
    case r.ASize of
       os8,
      os16,
      os32,
      os64: Result:=(r.AIndex>=16);
    else
            Result:=True;
    end;

  regGeneralH:
    case r.ASize of
       os8: Result:=(r.AIndex>=4);
    else
            Result:=True;
    end;

  regMm,
  regX87: Result:=(r.AIndex>=8);

  regXmm:
    case r.ASize of
      os32,
      os64,
      os128,
      os256: Result:=(r.AIndex>=16);
    else
             Result:=True;
    end;

  regSegment: Result:=(r.AIndex>=6);

  regFlags:
   case r.ASize of
     os16,
     os32,
     os64:;
   else
     Result:=True;
   end;

  else;
 end;
end;

function is_invalid(const r:TOperand):Boolean; inline;
begin
 Result:=is_invalid(r.RegValue[0]) or
         is_invalid(r.RegValue[1]);
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

procedure add_rip_entry(var ctx:t_jit_context2;ofs:Int64;hint:t_lea_hint);
begin
 if (code_ref in hint) then
 begin
  //call [addr]
  //jmp  [addr]

  if ctx.is_map_addr(ofs) then
  if ((pmap_get_prot(QWORD(ofs)) and PAGE_PROT_READ)<>0) then
  begin
   ofs:=PInt64(ofs)^;

   if ctx.is_text_addr(ofs) then
   if (ctx.max=0) or (ofs<=ctx.max) then
   if ((pmap_get_prot(QWORD(ofs)) and PAGE_PROT_EXECUTE)<>0) then
   begin
    ctx.add_forward_point(fpCall,Pointer(ofs));
   end;
  end;

 end else
 begin
  //lea

  if (ctx.max<>0) then
  if ctx.is_text_addr(ofs) then
  begin
   if (ofs<=ctx.max) then
   if ((pmap_get_prot(QWORD(ofs)) and PAGE_PROT_EXECUTE)<>0) then
   begin
    ctx.add_forward_point(fpData,Pointer(ofs));
   end;
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
  4:Result:=teb_fsbase;
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

function lea_reg_is_used(var RegValue:TRegValues;reg:TRegValue):Boolean; inline;
begin
 Result:=(RegValue[1].AType<>regNone) and
         (RegValue[1].AIndex=reg.AIndex);
end;

procedure build_lea(var ctx:t_jit_context2;id:Byte;
                    reg:TRegValue;hint:t_lea_hint=[]);
var
 RegValue:TRegValues;
 adr:TRegValue;
 new1,new2:TRegValue;
 ofs:Int64;
 i:Integer;
 AScale:Byte;
 save_r_tmp0:Boolean;
 reg1_used:Boolean;
 scale_ofs:Boolean;
 fix_rsp:Boolean;
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
    leaq(reg,[adr+ofs]); //endpoint
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
   movi(reg,ofs); //endpoint
  end else
  if is_rip(RegValue[0]) then //rip relative
  begin
   ofs:=0;
   GetTargetOfs(ctx.din,ctx.code,id,ofs);
   ofs:=Int64(ctx.ptr_next)+ofs;

   add_rip_entry(ctx,ofs,hint);

   if (classif_offset_u64(ofs)=os64) then
   begin
    movi64(adr,ofs); //endpoint
   end else
   begin
    if (reg.ASize=os64) then
    begin
     //mov r14d,imm32   this is zero extend to 64bit
     movi(new_reg_size(reg,os32),ofs); //endpoint
    end else
    begin
     movi(reg,ofs); //endpoint
    end;
   end;
  end else
  if is_preserved(RegValue) then
  begin

   optimal_swap(RegValue);

   save_r_tmp0:=False;
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

   reg1_used:=(RegValue[1].AType<>regNone);

   //1
   if is_preserved(RegValue[0]) then
   begin
    fix_rsp:=false;

    if (inc8_rsp in hint) and is_rsp(RegValue[0]) then
    begin
     //fix rsp relative
     if (AScale<=1) and
        (classif_offset_64(ofs+8)<>os64) then
     begin
      //shift ofs
      ofs:=ofs+8;
     end else
     begin
      //fix in lea
      fix_rsp:=true;
     end;
    end;

    scale_ofs:=(AScale>1) or (ofs<>0);

    i:=GetFrameOffset(RegValue[0]);

    if reg1_used or
       fix_rsp or
       scale_ofs then
    begin
     movq(adr,[r_thrd+i]);
    end else
    begin
     movq(reg,[r_thrd+i]); //endpoint
    end;

    //fix rsp relative
    if fix_rsp then
    begin
     if reg1_used or
        scale_ofs then
     begin
      leaq(adr,[adr+8]);
     end else
     begin
      leaq(reg,[adr+8]); //endpoint
     end;
    end;

    if scale_ofs then
    begin
     if reg1_used then
     begin
      leaq(adr,[adr*AScale+ofs]);
     end else
     begin
      leaq(reg,[adr*AScale+ofs]); //endpoint
     end;
    end;
   end else
   begin
    new1:=RegValue[0];
    //
    //AScale in new1
    if reg1_used then
    begin
     leaq(adr,[new1+ofs]);
    end else
    begin
     leaq(reg,[new1+ofs]); //endpoint
    end;
   end;
   //1

   //2
   if reg1_used then
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
      //fix rsp relative
      leaq(reg,[adr+new2+8]); //endpoint
     end else
     begin
      leaq(reg,[adr+new2]); //endpoint
     end;

     if (not_use_r_tmp1 in hint) then
     begin
      pop(r_tmp1);
     end;

    end else
    begin
     new1:=RegValue[1];
     leaq(reg,[adr+new1]); //endpoint
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
    leaq(reg,[new1+new2+ofs]); //endpoint
   end else
   begin
    //AScale in new1
    leaq(reg,[new1+ofs]); //endpoint
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

//

procedure op_load_r14(var ctx:t_jit_context2;reg:TRegValue);
var
 i:Integer;
begin
 with ctx.builder do
 begin
  i:=GetFrameOffset(r14);
  movq(reg,[r_thrd+i]);
 end;
end;

procedure op_save_r14(var ctx:t_jit_context2;reg:TRegValue);
var
 i:Integer;
begin
 with ctx.builder do
 begin
  i:=GetFrameOffset(r14);
  movq([r_thrd+i],reg);
 end;
end;

//

procedure op_load_rsp(var ctx:t_jit_context2;reg:TRegValue);
var
 i:Integer;
begin
 with ctx.builder do
 begin
  i:=GetFrameOffset(rsp);
  movq(reg,[r_thrd+i]);
 end;
end;

procedure op_save_rsp(var ctx:t_jit_context2;reg:TRegValue);
var
 i:Integer;
begin
 with ctx.builder do
 begin
  i:=GetFrameOffset(rsp);
  movq([r_thrd+i],reg);
 end;
end;

//

procedure op_load_rbp(var ctx:t_jit_context2;reg:TRegValue);
var
 i:Integer;
begin
 with ctx.builder do
 begin
  i:=GetFrameOffset(rbp);
  movq(reg,[r_thrd+i]);
 end;
end;

procedure op_save_rbp(var ctx:t_jit_context2;reg:TRegValue);
var
 i:Integer;
begin
 with ctx.builder do
 begin
  i:=GetFrameOffset(rbp);
  movq([r_thrd+i],reg);
 end;
end;

//

procedure op_load(var ctx:t_jit_context2;reg:TRegValue;opr:Byte);
var
 i:Integer;
begin
 with ctx.builder do
 begin
  i:=GetFrameOffset(ctx.din.Operand[opr]);
  movq(reg,[r_thrd+i]);
 end;
end;

procedure op_save(var ctx:t_jit_context2;opr:Byte;reg:TRegValue);
var
 i:Integer;
begin
 with ctx.builder do
 begin
  i:=GetFrameOffset(ctx.din.Operand[opr]);
  movq([r_thrd+i],reg);
 end;
end;

//

procedure op_uplift(var ctx:t_jit_context2); inline;
begin
 ctx.builder.call_far(@uplift_jit); //in/out:r14
end;

procedure op_copyin(var ctx:t_jit_context2;mem_size:TOperandSize); inline;
begin
 with ctx.builder do
 begin
  call_far(copyin_mov_size[mem_size]); //in:r14(addr), out:r14
 end;
end;

procedure op_copyout(var ctx:t_jit_context2;mem_size:TOperandSize); inline;
begin
 with ctx.builder do
 begin
  call_far(copyout_mov_size[mem_size]); //in:r14(addr)
 end;
end;

//

procedure op_emit1(var ctx:t_jit_context2;const desc:t_op_type;hint:t_op_hint);
var
 i:Integer;
 memop:t_memop_type1;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 new:TRegValue;

 procedure mem_out;
 begin
  with ctx.builder do
  begin
   //input:r14

   _M(desc,[flags(ctx)+r_tmp0,mem_size]);
  end;
 end;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:r14

   _M(desc,[flags(ctx)+r_tmp0,mem_size]);
  end;
 end;

begin
 Assert(ctx.din.OperCnt=1);
 memop:=classif_memop1(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem:
    begin

     if (his_ro in hint) then
     begin
      //DATA:=[mem]

      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      if (mem_size=os8) or
         (mem_size=os4096) or
         (his_rw in hint) then
      begin
       op_uplift(ctx); //in/out:r14

       mem_in;
      end else
      begin
       op_copyin(ctx,mem_size);

       mem_in;
      end;

     end else
     begin
      //[mem]:=DATA

      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      if (mem_size=os8) or
         (mem_size=os4096) or
         (his_rw in hint) then
      begin

       op_uplift(ctx); //in/out:r14

       mem_out;
      end else
      begin
       op_copyout(ctx,mem_size);

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
     Assert(mem_size<>os0);

     if (his_ro in hint) or
        (mem_size<>os32) then
     begin
      i:=GetFrameOffset(ctx.din.Operand[1]);
      _M(desc,[r_thrd+i,mem_size]);
     end else
     begin

      new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

      if (not (his_wo in hint)) or
         (his_ro in hint) then
      begin
       op_load(ctx,new,1);
      end;

      _R(desc,new);

      if not (his_ro in hint) then
      begin
       op_save(ctx,1,fix_size(new));
      end;

     end;

    end;

   else
    Assert(false);
  end;

end;

procedure op_mi(var ctx:t_jit_context2;
                 const desc:t_op_desc;
                 mem:t_jit_leas;
                 imm:Int64;
                 imm_size:TOperandSize);
begin
 with ctx.builder do
  if (imm_size=os8) and
     (mem_size(mem)<>os8) and
     (not (not_impl in desc.reg_im8.opt)) then
  begin
   _MI8(desc.reg_im8,mem,imm);
  end else
  begin
   _MI(desc.reg_imm,mem,imm);
  end;
end;

procedure op_ri(var ctx:t_jit_context2;
                 const desc:t_op_desc;
                 reg:TRegValue;
                 imm:Int64;
                 mem_size,imm_size:TOperandSize);
begin
 with ctx.builder do
  if (imm_size=os8) and
     (mem_size<>os8) and
     (not (not_impl in desc.reg_im8.opt)) then
  begin
   _RI8(desc.reg_im8,reg,imm);
  end else
  begin
   _RI(desc.reg_imm,reg,imm);
  end;
end;

procedure op_rmi(var ctx:t_jit_context2;
                 const desc:t_op_desc;
                 reg:TRegValue;mem:t_jit_leas;
                 imm:Int64;
                 imm_size:TOperandSize);
begin
 with ctx.builder do
  if (imm_size=os8) and
    (mem_size(mem)<>os8) and
    (not (not_impl in desc.reg_im8.opt)) then
  begin
   _RMI8(desc.reg_im8,reg,mem,imm);
  end else
  begin
   _RMI(desc.reg_imm,reg,mem,imm);
  end;
end;

procedure op_rri(var ctx:t_jit_context2;
                 const desc:t_op_desc;
                 reg1,reg2:TRegValue;
                 imm:Int64;
                 mem_size,imm_size:TOperandSize);
begin
 with ctx.builder do
  if (imm_size=os8) and
     (mem_size<>os8) and
     (not (not_impl in desc.reg_im8.opt)) then
  begin
   _RRI8(desc.reg_im8,reg1,reg2,imm,mem_size);
  end else
  begin
   _RRI(desc.reg_imm,reg1,reg2,imm,mem_size);
  end;
end;

procedure op_rr(var ctx:t_jit_context2;
                const desc:t_op_desc;
                reg1,reg2:TRegValue;
                mem_size:TOperandSize);
begin
 with ctx.builder do
  if (not_impl in desc.mem_reg.opt) then
  begin
   _RR(desc.reg_mem,reg2,reg1,mem_size); //swapped
  end else
  begin
   _RR(desc.mem_reg,reg1,reg2,mem_size);
  end;
end;

const
 ax=0;
 cx=1;
 dx=2;
 bx=3;

type
 t_lo_regi=0..3;
 t_lo_regs=Set of t_lo_regi;

function get_implicit_regs(var ctx:t_jit_context2):t_lo_regs;
begin
 Result:=[];

 case ctx.din.OpCode.Opcode of
  OPcmpxchg:
    case ctx.din.OpCode.Suffix of
     OPSnone: Result:=[ax];
     else
              Result:=[ax,dx];
    end;

  OPimul:     Result:=[ax,dx];

  OPpcmpestri:Result:=[cx];
  OPpcmpistri:Result:=[cx];

  OProl:      Result:=[cx];
  OPror:      Result:=[cx];
  OPrcl:      Result:=[cx];
  OPrcr:      Result:=[cx];
  OPshl:      Result:=[cx];
  OPshr:      Result:=[cx];
  OPsar:      Result:=[cx];

  else;
 end;

end;

function alloc_tmp_lo(var ctx:t_jit_context2;ASize:TOperandSize):TRegValue;
var
 i,w:Byte;
 excl:t_lo_regs;
begin
 Result:=Default(TRegValue);
 Result.AType:=regGeneral;
 Result.ASize:=ASize;

 excl:=get_implicit_regs(ctx);

 Result.AIndex:=0;

 while (Result.AIndex<=3) do
 begin

  if (Result.AIndex in excl) then
  begin
   Inc(Result.AIndex);
   Continue;
  end;

  if (ctx.din.OperCnt<>0) then
  For i:=1 to ctx.din.OperCnt do
  For w:=0 to 1 do
  begin
   if (ctx.din.Operand[i].RegValue[w].AType in [regGeneral, regGeneralH]) then
   if (ctx.din.Operand[i].RegValue[w].AIndex=Result.AIndex) then
   begin
    Inc(Result.AIndex);
    Continue;
   end;
  end;

  Exit;
 end;

 Assert(False);
end;

function alloc_tmp_lo(var ctx:t_jit_context2;i:Byte):TRegValue; inline;
begin
 Result:=alloc_tmp_lo(ctx,ctx.din.Operand[i].RegValue[0].ASize);
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

 tmp1,tmp2:TRegValue;

 new1_load:Boolean;

 procedure override_beg1;
 begin
  with ctx.builder do
  begin
   tmp1:=Default(TRegValue);

   if is_high(ctx.din.Operand[1]) then
   begin
    tmp1:=new1;
    new1:=alloc_tmp_lo(ctx,1);

    push(fix_size8(new1));

    if (not (his_wo in desc.hint)) or
       (his_ro in desc.hint) then
    begin
     movq(new1,tmp1);
    end;
   end;
  end;
 end;

 procedure override_fin1;
 begin
  with ctx.builder do
  begin
   if (tmp1.AType<>regNone) then
   begin
    if not (his_ro in desc.hint) then
    begin
     movq(tmp1,new1);
    end;

    pop(fix_size8(new1));
   end;
  end;
 end;

 procedure override_beg2;
 begin
  with ctx.builder do
  begin
   tmp2:=Default(TRegValue);

   if is_high(ctx.din.Operand[2]) then
   begin
    tmp2:=new2;
    new2:=alloc_tmp_lo(ctx,2);

    push(fix_size8(new2));
    movq(new2,tmp2);
   end;
  end;
 end;

 procedure override_fin2;
 begin
  with ctx.builder do
  begin
   if (tmp2.AType<>regNone) then
   begin
    if (his_xchg in desc.hint) then
    begin
     movq(tmp2,new2);
    end;

    pop(fix_size8(new2));
   end;
  end;
 end;

 procedure mem_out;
 begin
  with ctx.builder do
   case memop of
    mo_mem_reg:
      begin
       //input:r14

       new2:=new_reg(ctx.din.Operand[2]);

       override_beg2;

       _RM(desc.mem_reg,new2,[flags(ctx)+r_tmp0]);

       override_fin2;

      end;
    mo_mem_imm:
      begin
       //input:r14

       imm:=0;
       GetTargetOfs(ctx.din,ctx.code,2,imm);

       imm_size:=ctx.din.Operand[2].Size;

       Assert(imm_size<>os64);

       op_mi(ctx,desc,[flags(ctx)+r_tmp0,mem_size],imm,imm_size);

      end;
    mo_mem_ctx:
      begin
       //input:r14

       new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

       op_load(ctx,new2,2);

       _RM(desc.mem_reg,new2,[flags(ctx)+r_tmp0]);

       if (his_xchg in desc.hint) then
       begin
        op_save(ctx,2,fix_size(new2));
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
       //input:r14

       new1:=new_reg(ctx.din.Operand[1]);

       override_beg1;

       imm:=0;
       if GetTargetOfs(ctx.din,ctx.code,3,imm) then
       begin
        imm_size:=ctx.din.Operand[3].Size;
        mem_size:=ctx.din.Operand[1].Size;
        Assert(mem_size<>os0);

        op_rmi(ctx,desc,new1,[flags(ctx)+r_tmp0,mem_size],imm,imm_size);

       end else
       begin
        _RM(desc.reg_mem,new1,[flags(ctx)+r_tmp0]);
       end;

       override_fin1;

      end;
    mo_ctx_mem:
      begin
       //input:r14

       new1:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

       if (not (his_wo in desc.hint)) or
          (his_ro in desc.hint) then
       begin
        op_load(ctx,new1,1);
       end;

       imm:=0;
       if GetTargetOfs(ctx.din,ctx.code,3,imm) then
       begin
        //mo_ctx_mem_imm

        imm_size:=ctx.din.Operand[3].Size;
        mem_size:=ctx.din.Operand[1].Size;
        Assert(mem_size<>os0);

        op_rmi(ctx,desc,new1,[flags(ctx)+r_tmp0,mem_size],imm,imm_size);
       end else
       begin
        _RM(desc.reg_mem,new1,[flags(ctx)+r_tmp0]);
       end;

       if not (his_ro in desc.hint) then
       begin
        op_save(ctx,1,fix_size(new1));
       end;

      end;

    //read only swapped

    mo_mem_reg:
     begin
      //input:r14

      new2:=new_reg(ctx.din.Operand[2]);

      override_beg2;

      imm:=0;
      if GetTargetOfs(ctx.din,ctx.code,3,imm) then
      begin
       imm_size:=ctx.din.Operand[3].Size;
       mem_size:=ctx.din.Operand[2].Size;
       Assert(mem_size<>os0);

       op_rmi(ctx,desc,new2,[flags(ctx)+r_tmp0,mem_size],imm,imm_size);
      end else
      begin
       _RM(desc.mem_reg,new2,[flags(ctx)+r_tmp0]);
      end;

      override_fin2;

     end;

    mo_mem_ctx:
     begin
      //input:r14

      new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

      op_load(ctx,new2,2);

      imm:=0;
      if GetTargetOfs(ctx.din,ctx.code,3,imm) then
      begin
       imm_size:=ctx.din.Operand[3].Size;
       mem_size:=ctx.din.Operand[2].Size;
       Assert(mem_size<>os0);

       op_rmi(ctx,desc,new2,[flags(ctx)+r_tmp0,mem_size],imm,imm_size);
      end else
      begin
       _RM(desc.mem_reg,new2,[flags(ctx)+r_tmp0]);
      end;

     end;

    mo_mem_imm:
     begin
      //input:r14

      imm:=0;
      GetTargetOfs(ctx.din,ctx.code,2,imm);

      imm_size:=ctx.din.Operand[2].Size;

      Assert(imm_size<>os64);

      op_mi(ctx,desc,[flags(ctx)+r_tmp0,mem_size],imm,imm_size);

     end

    else
     Assert(False);
   end;
 end;

begin
 Assert(ctx.din.OperCnt in [2,3]);
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
        op_uplift(ctx); //in/out:r14

        mem_in;
       end else
       begin
        op_copyin(ctx,mem_size);

        mem_in;
       end;

      end else
      if (mem_size=os8) or
         (his_rw in desc.hint) then
      begin
       op_uplift(ctx); //in/out:r14

       mem_out;
      end else
      begin
       op_copyout(ctx,mem_size);

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
       op_uplift(ctx); //in/out:r14

       mem_in;
      end else
      begin
       op_copyin(ctx,mem_size);

       mem_in;
      end;
     end;

   mo_ctx_reg:
     begin
      new2:=new_reg(ctx.din.Operand[2]);

      override_beg2;

      imm:=0;
      if GetTargetOfs(ctx.din,ctx.code,3,imm) then
      begin
       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       imm_size:=ctx.din.Operand[3].Size;
       mem_size:=ctx.din.Operand[1].RegValue[0].ASize;
       Assert(mem_size<>os0);

       op_rri(ctx,desc,new2,new1,imm,mem_size,imm_size); //swapped

       op_save(ctx,1,fix_size(new1));
      end else
      begin

       mem_size:=ctx.din.Operand[1].RegValue[0].ASize;
       Assert(mem_size<>os0);

       if ((his_ro in desc.hint) or (mem_size<>os32)) and
          (not (not_impl in desc.mem_reg.opt)) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        _RM(desc.mem_reg,new2,[r_thrd+i]);
       end else
       begin

        new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

        if (not (his_wo in desc.hint)) or
           (his_ro in desc.hint) then
        begin
         op_load(ctx,new1,1);
        end;

        op_rr(ctx,desc,new1,new2,mem_size);

        if not (his_ro in desc.hint) then
        begin
         op_save(ctx,1,fix_size(new1));
        end;

       end;

      end;

      override_fin2;

     end;
   mo_reg_ctx:
     begin
      new1:=new_reg(ctx.din.Operand[1]);

      override_beg1;

      imm:=0;
      if GetTargetOfs(ctx.din,ctx.code,3,imm) then
      begin
       imm_size:=ctx.din.Operand[3].Size;
       mem_size:=ctx.din.Operand[1].Size;
       Assert(mem_size<>os0);

       i:=GetFrameOffset(ctx.din.Operand[2]);
       op_rmi(ctx,desc,new1,[r_thrd+i,mem_size],imm,imm_size);

      end else
      begin

       if (not_impl in desc.reg_mem.opt) then
       begin

        new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

        op_load(ctx,new2,2);

        mem_size:=ctx.din.Operand[1].RegValue[0].ASize;
        Assert(mem_size<>os0);

        _RR(desc.mem_reg,new1,new2,mem_size);
       end else
       begin
        i:=GetFrameOffset(ctx.din.Operand[2]);
        _RM(desc.reg_mem,new1,[r_thrd+i]);
       end;

      end;

      override_fin1;

     end;
   mo_ctx_ctx:
     begin
      mem_size:=ctx.din.Operand[1].RegValue[0].ASize;
      Assert(mem_size<>os0);

      imm:=0;
      if GetTargetOfs(ctx.din,ctx.code,3,imm) then
      begin
       imm_size:=ctx.din.Operand[3].Size;

       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
       new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

       op_load(ctx,new2,2);

       op_rri(ctx,desc,new2,new1,imm,mem_size,imm_size); //swapped

       op_save(ctx,1,fix_size(new1));
      end else
      begin

       if ((his_ro in desc.hint) or (mem_size<>os32)) and
          (not (not_impl in desc.mem_reg.opt)) and
          (not cmp_reg(ctx.din.Operand[1],ctx.din.Operand[2])) then
       begin
        new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        op_load(ctx,new2,2);

        i:=GetFrameOffset(ctx.din.Operand[1]);
        _RM(desc.mem_reg,new2,[r_thrd+i]);
       end else
       begin

        new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

        new1_load:=False;

        if (not (his_wo in desc.hint)) or
           (his_ro in desc.hint) then
        begin
         op_load(ctx,new1,1);
         new1_load:=True;
        end;

        if cmp_reg(ctx.din.Operand[1],ctx.din.Operand[2]) then
        begin
         new2:=new1;

         //preload if reg1=reg2
         if not new1_load then
         begin
          op_load(ctx,new2,2);
         end;
        end else
        begin
         new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

         op_load(ctx,new2,2);
        end;

        op_rr(ctx,desc,new1,new2,mem_size);

        if not (his_ro in desc.hint) then
        begin
         op_save(ctx,1,fix_size(new1));
        end;
       end;

      end;

     end;
   mo_ctx_imm:
     begin
      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      imm:=0;
      GetTargetOfs(ctx.din,ctx.code,2,imm);

      imm_size:=ctx.din.Operand[2].Size;

      if (imm_size=os64) and
         (classif_offset_se64(imm)=os64) then
      begin
       Assert(his_mov in desc.hint);

       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       movi64(new1,imm);

       op_save(ctx,1,fix_size(new1));
      end else
      begin

       if (his_ro in desc.hint) or
          (mem_size<>os32) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        op_mi(ctx,desc,[r_thrd+i,mem_size],imm,imm_size);
       end else
       begin
        new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

        if (not (his_wo in desc.hint)) or
           (his_ro in desc.hint) then
        begin
         op_load(ctx,new1,1);
        end;

        op_ri(ctx,desc,new1,imm,mem_size,imm_size);

        if not (his_ro in desc.hint) then
        begin
         op_save(ctx,1,fix_size(new1));
        end;

       end;

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

 new:TRegValue;

 procedure mem_out;
 begin
  with ctx.builder do
   case memop of
    mo_mem_imm8:
      begin
       //input:r14

       imm:=0;
       GetTargetOfs(ctx.din,ctx.code,2,imm);

       imm_size:=ctx.din.Operand[2].Size;

       Assert(imm_size=os8);
       Assert(not (not_impl in desc.reg_im8.opt));

       _MI8(desc.reg_im8,[flags(ctx)+r_tmp0,mem_size],imm);
      end;
    mo_mem_cl:
      begin
       //input:r14

       _M(desc.mem__cl,[flags(ctx)+r_tmp0,mem_size]);
      end;
    mo_mem_one:
     begin
      //input:r14

      _M(desc.mem_one,[flags(ctx)+r_tmp0,mem_size]);
     end

    else
     Assert(False);
   end;
 end;

begin
 Assert(ctx.din.OperCnt=2);
 memop:=classif_shift2(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem_imm8,
   mo_mem_cl,
   mo_mem_one:
     begin
      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);
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
       op_uplift(ctx); //in/out:r14

       mem_out;
      end else
      begin
       op_copyout(ctx,mem_size);

       link_next:=jmp(nil_link,os8);

       mem_out;

       reta;

       link_next._label:=get_curr_label.after;
      end;
     end;

   mo_ctx_imm8:
     begin
      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      imm:=0;
      GetTargetOfs(ctx.din,ctx.code,2,imm);

      imm_size:=ctx.din.Operand[2].Size;
      Assert(imm_size=os8);

      if (mem_size<>os32) then
      begin
       i:=GetFrameOffset(ctx.din.Operand[1]);
       _MI8(desc.reg_im8,[r_thrd+i,mem_size],imm);
      end else
      begin
       new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       op_load(ctx,new,1);

       _RI8(desc.reg_im8,new,imm);

       op_save(ctx,1,fix_size(new));
      end;

     end;
    mo_ctx_cl:
     begin
      mem_size:=ctx.din.Operand[1].Size;

      if (mem_size<>os32) then
      begin
       i:=GetFrameOffset(ctx.din.Operand[1]);
       _M(desc.mem__cl,[r_thrd+i,mem_size]);
      end else
      begin
       new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       op_load(ctx,new,1);

       _R(desc.mem__cl,new);

       op_save(ctx,1,fix_size(new));
      end;

     end;
    mo_ctx_one:
     begin
      mem_size:=ctx.din.Operand[1].Size;

      if (mem_size<>os32) then
      begin
       i:=GetFrameOffset(ctx.din.Operand[1]);
       _M(desc.mem_one,[r_thrd+i,mem_size]);
      end else
      begin
       new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       op_load(ctx,new,1);

       _R(desc.mem_one,new);

       op_save(ctx,1,fix_size(new));
      end;

     end;

   else
    Assert(false);
  end;

end;

//

procedure op_emit_shift3(var ctx:t_jit_context2;const desc:t_op_shift);
var
 memop:t_memop_shift;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 imm:Int64;
 imm_size:TOperandSize;

 new1,new2:TRegValue;

 tmp1,tmp2:TRegValue;

 procedure override_beg1;
 begin
  with ctx.builder do
  begin
   tmp1:=Default(TRegValue);

   if is_high(ctx.din.Operand[1]) then
   begin
    tmp1:=new1;
    new1:=alloc_tmp_lo(ctx,1);

    push(fix_size8(new1));
    movq(new1,tmp1);
   end;
  end;
 end;

 procedure override_fin1;
 begin
  with ctx.builder do
  begin
   if (tmp1.AType<>regNone) then
   begin
    movq(tmp1,new1);

    pop(fix_size8(new1));
   end;
  end;
 end;

 procedure override_beg2;
 begin
  with ctx.builder do
  begin
   tmp2:=Default(TRegValue);

   if is_high(ctx.din.Operand[2]) then
   begin
    tmp2:=new2;
    new2:=alloc_tmp_lo(ctx,2);

    push(fix_size8(new2));
    movq(new2,tmp2);
   end;
  end;
 end;

 procedure override_fin2;
 begin
  with ctx.builder do
  begin
   if (tmp2.AType<>regNone) then
   begin
    movq(tmp2,new2);
    pop(fix_size8(new2));
   end;
  end;
 end;

 procedure mem_out;
 begin
  with ctx.builder do
   case memop of
    mo_mem_imm8:
      begin
       //input:r14

       imm:=0;
       GetTargetOfs(ctx.din,ctx.code,3,imm);

       imm_size:=ctx.din.Operand[3].Size;

       Assert(imm_size=os8);
       Assert(not (not_impl in desc.reg_im8.opt));

       if is_preserved(ctx.din.Operand[2]) then
       begin
        //mem_ctx_imm
        new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        op_load(ctx,new2,2);
       end else
       begin
        //mem_reg_imm
        new2:=new_reg(ctx.din.Operand[2]);
       end;

       override_beg2;

       _RMI8(desc.reg_im8,new2,[flags(ctx)+r_tmp0],imm);

       override_fin2;
      end;
    mo_mem_cl:
      begin
       //input:r14

       if is_preserved(ctx.din.Operand[2]) then
       begin
        //mem_ctx_cl
        new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        op_load(ctx,new2,2);
       end else
       begin
        //mem_reg_cl
        new2:=new_reg(ctx.din.Operand[2]);
       end;

       override_beg2;

       _RM(desc.mem__cl,new2,[flags(ctx)+r_tmp0]);

       override_fin2;
      end;

    else
     Assert(False);
   end;
 end;

begin
 Assert(ctx.din.OperCnt=3);
 memop:=classif_shift3(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem_imm8,
   mo_mem_cl:
     begin
      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);
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
       op_uplift(ctx); //in/out:r14

       mem_out;
      end else
      begin
       op_copyout(ctx,mem_size);

       link_next:=jmp(nil_link,os8);

       mem_out;

       reta;

       link_next._label:=get_curr_label.after;
      end;
     end;

   mo_ctx_imm8:
     begin
      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      imm:=0;
      GetTargetOfs(ctx.din,ctx.code,3,imm);

      imm_size:=ctx.din.Operand[3].Size;
      Assert(imm_size=os8);

      if is_preserved(ctx.din.Operand[2]) then
      begin
       //ctx_ctx_imm

       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       op_load(ctx,new1,1);

       if cmp_reg(ctx.din.Operand[1],ctx.din.Operand[2]) then
       begin
        new2:=new1;
       end else
       begin
        new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        op_load(ctx,new2,2);
       end;

      end else
      begin
       //ctx_reg_imm
       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
       new2:=new_reg(ctx.din.Operand[2]);

       op_load(ctx,new1,1);
      end;

      override_beg2;

      _RRI8(desc.reg_im8,new1,new2,imm,mem_size);

      override_fin2;

      op_save(ctx,1,fix_size(new1));
     end;
    mo_ctx_cl:
     begin
      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      if is_preserved(ctx.din.Operand[2]) then
      begin
       //ctx_ctx_cl

       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       op_load(ctx,new1,1);

       if cmp_reg(ctx.din.Operand[1],ctx.din.Operand[2]) then
       begin
        new2:=new1;
       end else
       begin
        new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

        op_load(ctx,new2,2);
       end;

      end else
      begin
       //ctx_reg_cl
       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
       new2:=new_reg(ctx.din.Operand[2]);

       op_load(ctx,new1,1);
      end;

      override_beg2;

      _RR(desc.mem__cl,new1,new2,mem_size);

      override_fin2;

      op_save(ctx,1,fix_size(new1));
     end;

    mo_reg_imm8:
     begin
      //reg_ctx_imm

      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      imm:=0;
      GetTargetOfs(ctx.din,ctx.code,3,imm);

      imm_size:=ctx.din.Operand[3].Size;
      Assert(imm_size=os8);

      new1:=new_reg(ctx.din.Operand[1]);
      new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

      op_load(ctx,new2,2);

      override_beg1;

      _RRI8(desc.reg_im8,new1,new2,imm,mem_size);

      override_fin1;
     end;

    mo_reg_cl:
     begin
      //reg_ctx_cl

      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      new1:=new_reg(ctx.din.Operand[1]);
      new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

      op_load(ctx,new2,2);

      override_beg1;

      _RR(desc.mem__cl,new1,new2,mem_size);

      override_fin1;
     end

   else
    Assert(false);
  end;

end;

//

procedure op_emit_avx1(var ctx:t_jit_context2;const desc:t_op_type;hint:t_op_hint);
var
 i:Integer;
 memop:t_memop_type1;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 new:TRegValue;

 procedure mem_out;
 begin
  with ctx.builder do
  begin
   //input:r14

   _VM(desc,[flags(ctx)+r_tmp0,mem_size]);
  end;
 end;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:r14

   _VM(desc,[flags(ctx)+r_tmp0,mem_size]);
  end;
 end;

begin
 Assert(ctx.din.OperCnt=1);
 memop:=classif_memop1(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem:
    begin

     if (his_ro in hint) then
     begin
      //DATA:=[mem]

      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      if (mem_size=os8) or
         (his_rw in hint) then
      begin
       op_uplift(ctx); //in/out:r14

       mem_in;
      end else
      begin
       op_copyin(ctx,mem_size);

       mem_in;
      end;

     end else
     begin
      //[mem]:=DATA

      build_lea(ctx,1,r_tmp0);
      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      if (mem_size=os8) or
         (his_rw in hint) then
      begin

       op_uplift(ctx); //in/out:r14

       mem_out;
      end else
      begin
       op_copyout(ctx,mem_size);

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
     Assert(mem_size<>os0);

     if (his_ro in hint) or
        (mem_size<>os32) then
     begin
      i:=GetFrameOffset(ctx.din.Operand[1]);
      _VM(desc,[r_thrd+i,mem_size]);
     end else
     begin

      new:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

      if (not (his_wo in hint)) or
         (his_ro in hint) then
      begin
       op_load(ctx,new,1);
      end;

      Assert(false,'_V(desc,new);');

      if not (his_ro in hint) then
      begin
       op_save(ctx,1,fix_size(new));
      end;

     end;

    end;

   else
    Assert(false);
  end;

end;

//

procedure op_emit_avx2_rr(var ctx:t_jit_context2;const desc:t_op_type);
var
 new1,new2:TRegValue;
begin
 Assert(ctx.din.OperCnt=2);
 if is_preserved(ctx.din.Operand[1]) then
 begin
  with ctx.builder do
  begin
   new1:=r_tmp0;
   new2:=new_reg(ctx.din.Operand[2]);

   _VV(desc,new1,new2,new2.ASize);

   op_save(ctx,1,fix_size(new1));
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
   //input:r14

   new2:=new_reg(ctx.din.Operand[2]);
   _VM(desc.mem_reg,new2,[flags(ctx)+r_tmp0,mem_size]);
  end;
 end;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   case memop of
    mo_reg_mem:
      begin
       //input:r14

       new1:=new_reg(ctx.din.Operand[1]);
       _VM(desc.reg_mem,new1,[flags(ctx)+r_tmp0,mem_size]);
      end;

    mo_ctx_mem:
      begin
       //input:r14

       new1:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

       if (not (his_wo in desc.hint)) or
          (his_ro in desc.hint) then
       begin
        op_load(ctx,new1,1);
       end;

       _VM(desc.reg_mem,new1,[flags(ctx)+r_tmp0,mem_size]);

       if not (his_ro in desc.hint) then
       begin
        op_save(ctx,1,fix_size(new1));
       end;

      end;
    else
     Assert(false);
    end;
  end;
 end;

begin
 Assert(ctx.din.OperCnt=2);
 memop:=classif_memop2(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem_reg,
   mo_reg_mem:
     begin
      build_lea(ctx,get_lea_id(memop),r_tmp0);
      mem_size:=ctx.din.Operand[get_lea_id(memop)].Size;
      Assert(mem_size<>os0);
     end;
   else;
  end;

 with ctx.builder do
  case memop of
   mo_mem_reg:
     begin
      if (his_align in desc.hint) then
      begin
       op_uplift(ctx); //in/out:r14

       mem_out;
      end else
      begin
       op_copyout(ctx,mem_size);

       link_next:=jmp(nil_link,os8);

       mem_out;

       reta;

       link_next._label:=get_curr_label.after;
      end;
     end;

   mo_reg_mem,
   mo_ctx_mem:
     begin
      if (his_align in desc.hint) then
      begin
       op_uplift(ctx); //in/out:r14

       mem_in;
      end else
      begin
       op_copyin(ctx,mem_size);

       mem_in;
      end;
     end;

   mo_ctx_reg:
     begin
      new1:=new_reg(ctx.din.Operand[2]);

      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      if (not_impl in desc.mem_reg.opt) then
      begin
       new2:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       _VV(desc.reg_mem,new2,new1,mem_size); //swapped

       op_save(ctx,1,fix_size(new2));
      end else
      begin
       i:=GetFrameOffset(ctx.din.Operand[1]);
       _VM(desc.mem_reg,new1,[r_thrd+i,mem_size]);
      end;

     end;
   mo_reg_ctx:
     begin
      new1:=new_reg(ctx.din.Operand[1]);

      mem_size:=ctx.din.Operand[2].Size;
      Assert(mem_size<>os0);

      i:=GetFrameOffset(ctx.din.Operand[2]);
      _VM(desc.reg_mem,new1,[r_thrd+i,mem_size]);
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
   //input:r14

   mem_size:=ctx.din.Operand[1].Size;
   Assert(mem_size<>os0);

   new1:=new_reg(ctx.din.Operand[2]);
   new2:=new_reg(ctx.din.Operand[3]);

   imm:=0;
   if GetTargetOfs(ctx.din,ctx.code,4,imm) then
   begin
    _VVMI8(desc,new2,new1,[flags(ctx)+r_tmp0,mem_size],imm);
   end else
   begin
    _VVM(desc,new2,new1,[flags(ctx)+r_tmp0,mem_size]); //[mem],arg2,arg3 -> arg3,arg2,[mem]
   end;

  end;
 end;

 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:r14

   new1:=new_reg(ctx.din.Operand[1]);
   new2:=new_reg(ctx.din.Operand[2]);

   imm:=0;
   if GetTargetOfs(ctx.din,ctx.code,4,imm) then
   begin
    _VVMI8(desc,new1,new2,[flags(ctx)+r_tmp0,mem_size],imm);
   end else
   begin
    _VVM(desc,new1,new2,[flags(ctx)+r_tmp0,mem_size]);
   end;

  end;
 end;

begin
 Assert(ctx.din.OperCnt in [3,4]);
 if (ofMemory in ctx.din.Operand[3].Flags) then
 begin
  //mo_reg_reg_mem

  build_lea(ctx,3,r_tmp0);
  mem_size:=ctx.din.Operand[3].Size;
  Assert(mem_size<>os0);

  with ctx.builder do
  begin

   if false then
   begin
    op_uplift(ctx); //in/out:r14

    mem_in;
   end else
   begin
    op_copyin(ctx,mem_size);

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
   Assert(mem_size<>os0);

   new1:=new_reg(ctx.din.Operand[1]);
   new2:=new_reg(ctx.din.Operand[2]);

   imm:=0;
   if GetTargetOfs(ctx.din,ctx.code,4,imm) then
   begin
    i:=GetFrameOffset(ctx.din.Operand[3]);
    _VVMI8(desc,new1,new2,[r_thrd+i,mem_size],imm);
   end else
   begin
    i:=GetFrameOffset(ctx.din.Operand[3]);
    _VVM(desc,new1,new2,[r_thrd+i,mem_size]);
   end;

  end;

 end else
 if (ofMemory in ctx.din.Operand[1].Flags) then
 begin
  //mo_mem_reg_reg

  build_lea(ctx,1,r_tmp0);
  mem_size:=ctx.din.Operand[1].Size;
  Assert(mem_size<>os0);

  with ctx.builder do
  begin

   if false then
   begin
    op_uplift(ctx); //in/out:r14

    mem_out;
   end else
   begin
    op_copyout(ctx,mem_size);

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
 memop:t_memop_type2;
 mem_size:TOperandSize;
 link_next:t_jit_i_link;

 new1,new2:TRegValue;

 imm:Int64;

 procedure mem_out;
 begin
  with ctx.builder do
  begin
   //input:r14

   //mem_reg

   new2:=new_reg(ctx.din.Operand[2]);

   imm:=0;
   GetTargetOfs(ctx.din,ctx.code,3,imm);

   _VMI8(desc.mri,new2,[flags(ctx)+r_tmp0,mem_size],imm);
  end;
 end;


 procedure mem_in;
 begin
  with ctx.builder do
  begin
   //input:r14

   //reg_mem

   new1:=new_reg(ctx.din.Operand[1]);

   imm:=0;
   GetTargetOfs(ctx.din,ctx.code,3,imm);

   _VMI8(desc.rmi,new1,[flags(ctx)+r_tmp0,mem_size],imm);
  end;
 end;


begin
 Assert(ctx.din.OperCnt=3);
 memop:=classif_memop2(ctx.din);

 with ctx.builder do
  case memop of
   mo_mem_reg:
    begin
     build_lea(ctx,1,r_tmp0);
     mem_size:=ctx.din.Operand[1].Size;
     Assert(mem_size<>os0);

     if (mem_size=os8) then
     begin
      op_uplift(ctx); //in/out:r14

      mem_out;
     end else
     begin
      op_copyout(ctx,mem_size);

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
      Assert(mem_size<>os0);

      if (mem_size=os8) then
      begin
       op_uplift(ctx); //in/out:r14

       mem_in;
      end else
      begin
       op_copyin(ctx,mem_size);

       mem_in;
      end;
     end;

   mo_ctx_reg:
    begin
     mem_size:=ctx.din.Operand[1].Size;
     Assert(mem_size<>os0);

     new2:=new_reg(ctx.din.Operand[2]);

     imm:=0;
     GetTargetOfs(ctx.din,ctx.code,3,imm);

     new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

     //mri

     _VVI8(desc.mri,new2,new1,imm,mem_size);

     op_save(ctx,1,fix_size(new1));
    end;

   mo_reg_ctx:
    begin
     mem_size:=ctx.din.Operand[2].Size;
     Assert(mem_size<>os0);

     new1:=new_reg(ctx.din.Operand[1]);
     new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

     //rmi

     op_load(ctx,new2,2);

     _VVI8(desc.rmi,new1,new2,imm,mem_size);
    end


   else
    Assert(false);
  end;

end;

procedure op_emit_avx_F3(var ctx:t_jit_context2;const desc:t_op_type);
var
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
      //input:r14

      new1:=new_reg(ctx.din.Operand[1]);
      _VM_F3(desc,new1,[flags(ctx)+r_tmp0,mem_size]);
     end;
    mo_ctx_mem:
     begin
      //input:r14

      //load?

      new1:=new_reg_size(r_tmp1,ctx.din.Operand[1]);

      _VM_F3(desc,new1,[flags(ctx)+r_tmp0,mem_size]);

      op_save(ctx,1,fix_size(new1));
     end;
    else
     Assert(False);
   end;
  end;
 end;

begin
 Assert(ctx.din.OperCnt=2);
 memop:=classif_memop2(ctx.din);

 with ctx.builder do
  case memop of
   mo_reg_mem,
   mo_ctx_mem:
     begin
      build_lea(ctx,get_lea_id(memop),r_tmp0);
      mem_size:=ctx.din.Operand[get_lea_id(memop)].Size;
      Assert(mem_size<>os0);
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
       op_uplift(ctx); //in/out:r14

       mem_in;
      end else
      begin
       op_copyin(ctx,mem_size);

       mem_in;
      end;
     end;

   mo_ctx_reg:
     begin
      new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
      new2:=new_reg(ctx.din.Operand[2]);

      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      //load?

      _VV_F3(desc,new1,new2,mem_size);

      op_save(ctx,1,fix_size(new1));
     end;
   mo_reg_ctx:
     begin
      new1:=new_reg(ctx.din.Operand[1]);
      new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

      mem_size:=ctx.din.Operand[2].Size;
      Assert(mem_size<>os0);

      op_load(ctx,new2,2);

      _VV_F3(desc,new1,new2,mem_size);
     end;

   mo_ctx_ctx:
     begin
      new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
      new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

      //load?

      op_load(ctx,new2,2);

      mem_size:=ctx.din.Operand[1].RegValue[0].ASize;
      Assert(mem_size<>os0);

      _VV_F3(desc,new1,new2,mem_size);

      op_save(ctx,1,fix_size(new1));
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
   //input:r14

   new1:=new_reg(ctx.din.Operand[1]);
   new2:=new_reg(ctx.din.Operand[2]);
   new3:=new_reg(ctx.din.Operand[4]);

   _VVMV(desc,new1,new2,[flags(ctx)+r_tmp0,mem_size],new3);
  end;
 end;

begin
 Assert(ctx.din.OperCnt=4);

 with ctx.builder do
  if is_memory(ctx.din.Operand[3]) then
  begin
   build_lea(ctx,3,r_tmp0);
   mem_size:=ctx.din.Operand[3].Size;
   Assert(mem_size<>os0);

   if false then
   begin
    op_uplift(ctx); //in/out:r14

    mem_in;
   end else
   begin
    op_copyin(ctx,mem_size);

    mem_in;
   end;
  end else
  begin
   Assert(false);
  end;
end;

procedure op_emit_bmi_rmr(var ctx:t_jit_context2;const desc:t_op_type);
var
 mem_size:TOperandSize;

 new1,new2,new3:TRegValue;
begin
 Assert(ctx.din.OperCnt=3);

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
   Assert(mem_size<>os0);

   op_copyin(ctx,mem_size);

   movq(new2,[r_tmp0]);
  end else
  if is_preserved(ctx.din.Operand[2]) then
  begin
   new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);
   //
   op_load(ctx,new2,2);
  end else
  begin
   new2:=new_reg(ctx.din.Operand[2]);
  end;

  if is_preserved(ctx.din.Operand[3]) then
  begin
   new3:=new_reg_size(r_tmp1,ctx.din.Operand[3]);
   //
   op_load(ctx,new3,3);
  end else
  begin
   new3:=new_reg(ctx.din.Operand[3]);
  end;

  _VVV(desc,new1,new3,new2,new3.ASize); //1 3 2

  if is_preserved(ctx.din.Operand[1]) then
  begin
   op_save(ctx,1,fix_size(new1));
  end;

 end;
end;

//

procedure op_emit_bmi_rrm(var ctx:t_jit_context2;const desc:t_op_type);
var
 mem_size:TOperandSize;

 new1,new2,new3:TRegValue;
begin
 Assert(ctx.din.OperCnt=3);

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
   op_load(ctx,new2,2);
  end else
  begin
   new2:=new_reg(ctx.din.Operand[2]);
  end;

  if is_memory(ctx.din.Operand[3]) then
  begin
   new3:=new_reg_size(r_tmp1,ctx.din.Operand[3]);

   build_lea(ctx,3,r_tmp0);
   mem_size:=ctx.din.Operand[3].Size;
   Assert(mem_size<>os0);

   op_copyin(ctx,mem_size);

   movq(new3,[r_tmp0]);
  end else
  if is_preserved(ctx.din.Operand[3]) then
  begin
   new3:=new_reg_size(r_tmp1,ctx.din.Operand[3]);
   //
   op_load(ctx,new3,3);
  end else
  begin
   new3:=new_reg(ctx.din.Operand[3]);
  end;

  _VVV(desc,new1,new2,new3,new3.ASize); //1 2 3

  if is_preserved(ctx.din.Operand[1]) then
  begin
   op_save(ctx,1,fix_size(new1));
  end;

 end;
end;

end.




