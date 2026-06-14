unit kern_jit_ctx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vmparam,
 g_node_splay,
 x86_fpdbgdisas,
 x86_jit;

const
 CAN_RESTART=1;

type
 t_point_type=(fpCall,fpData,fpInvalid);

 t_ctx_modes=Set of (cmDontScanRipRel,cmDontScanSwitchTable,cmDontScanNop,cmInternal);

 p_jit_context2=^t_jit_context2;
 t_jit_context2=object
  type
   p_forward_link=^t_forward_link;
   t_forward_link=object
    next       :p_forward_link;
    instruction:t_jit_i_link;
   end;

   t_forward_links=object
    root :p_forward_link;
    ptype:t_point_type;
   end;

   p_forward_point=^t_forward_point;
   t_forward_point=object
    pLeft :p_forward_point;
    pRight:p_forward_point;
    dst   :Pointer;
    links :t_forward_links;
    nid   :QWORD;
    function c(n1,n2:p_forward_point):Integer; static;
   end;
   t_forward_set=specialize TNodeSplay<t_forward_point>;

   p_switchtable_point=^t_switchtable_point;
   t_switchtable_point=object
    pLeft :p_switchtable_point;
    pRight:p_switchtable_point;
    //
    table:PInteger;
    curr :PInteger;
    //
    function c(n1,n2:p_switchtable_point):Integer; static;
   end;
   t_switchtable_set=specialize TNodeSplay<t_switchtable_point>;

   p_jumpslot=^t_jumpslot;
   t_jumpslot=object
    pLeft :p_jumpslot;
    pRight:p_jumpslot;
    //
    addr:Pointer;
    //
    function c(n1,n2:p_jumpslot):Integer; static;
   end;
   t_jumpslot_set=specialize TNodeSplay<t_jumpslot>;

   p_label=^t_label;
   t_label=object
    pLeft    :p_label;
    pRight   :p_label;
    curr     :Pointer;
    next     :Pointer;
    link_curr:t_jit_i_link;
    link_next:t_jit_i_link;
    flags    :Integer;
    function c(n1,n2:p_label):Integer; static;
   end;
   t_label_set=specialize TNodeSplay<t_label>;

   p_entry_point=^t_entry_point;
   t_entry_point=object
    pLeft      :p_entry_point;
    pRight     :p_entry_point;
    //
    next       :p_entry_point;
    //
    src        :Pointer;
    instruction:t_jit_i_link;
    //
    function c(n1,n2:p_entry_point):Integer; static;
   end;
   t_entry_point_set=specialize TNodeSplay<t_entry_point>;

   p_export_point=^t_export_point;
   t_export_point=object
    next  :p_export_point;
    nid   :QWORD;
    native:Pointer;
    dst   :PPointer;
   end;

   p_import_point=^t_import_point;
   t_import_point=object
    next :p_import_point;
    guest:PPointer;
    dst  :PPointer;
   end;

  var
   forward_link_cache :p_forward_link;
   forward_point_cache:p_forward_point;
   //
   forward_set:t_forward_set;
   //
   switchtable_set:t_switchtable_set;
   min_switchtable:p_switchtable_point;
   //
   jumpslot_set:t_jumpslot_set;
   //
   label_set  :t_label_set;
   entry_list :p_entry_point;
   entry_set  :t_entry_point_set;

   export_list:p_export_point;
   import_list:p_import_point;

   obj:Pointer;
   name:pchar;

   text_start:QWORD;
   text___end:QWORD;
   map____end:QWORD;

   max_reloc :QWORD;

   modes:t_ctx_modes;

   Code    :Pointer;
   ptr_curr:Pointer;
   ptr_next:Pointer;

   label_flags:Integer;
   trim:Boolean;

   dis:TX86Disassembler;
   din:TInstruction;

   builder:t_jit_builder;

  function  is_text_addr(addr:QWORD):Boolean;
  function  is_map_addr (addr:QWORD):Boolean;
  procedure add_export_point (nid:QWORD;native:Pointer;dst:PPointer);
  procedure add_import_point (guest,dst:PPointer);
  procedure add_forward_link (node:p_forward_point;instruction:t_jit_i_link);
  procedure Resolve_forwards (var links:t_forward_links;target:t_jit_i_link);
  function  add_forward_point(ptype:t_point_type;instruction:t_jit_i_link;dst:Pointer):p_forward_point;
  function  add_forward_point(ptype:t_point_type;dst:Pointer):p_forward_point;
  function  add_switchtable  (table:Pointer):p_switchtable_point;
  procedure add_jumpslot     (addr:Pointer);
  function  is_jumpslot      (addr:Pointer):Boolean;
  function  fetch_switchtable(var out_table:p_switchtable_point;var out_next:PInteger):Boolean;
  Function  new_chunk(ptype:t_point_type;start:Pointer):p_jit_code_chunk;
  procedure mark_chunk(ptype:t_point_type);
  function  get_chunk_ptype():t_point_type;
  procedure end_chunk(__end:Pointer);
  function  max_forward_point():Pointer;
  function  fetch_forward_nid(addr:Pointer):QWORD;
  function  fetch_forward_point(var links:t_forward_links;var dst:Pointer;var nid:QWORD):Boolean;
  function  add_label(curr,next:Pointer;link_curr,link_next:t_jit_i_link;flags:Integer):p_label;
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

 SCODES:array[TSimdOpcode] of Byte=(0,0,1,3,2);
 MCODES:array[0..3] of PChar=('_','0x0F','0x0F38','0x0F3A');

function GetFrameOffset(const RegValue:TRegValue):Integer;
function GetFrameOffset(const r:TOperand):Integer;
function GetTargetOfs(var din:TInstruction;Code:PByte;id:Byte;var ofs:Int64):Boolean;
function is_preserved(const r:TRegValue):Boolean;
function is_preserved(const r:TOperand):Boolean;
function is_preserved(const r:TInstruction):Boolean;
function is_rip(const r:TRegValue):Boolean;
function is_rip(const r:TRegValues):Boolean;
function is_rip(const r:TOperand):Boolean;
function is_rip(const r:TInstruction):Boolean;
function is_memory(const r:TOperand):Boolean;
function is_memory(const r:TInstruction):Boolean;
function is_xmm(const r:TOperand):Boolean;
function is_xmm(const r:TInstruction):Boolean;
function is_high(const r:TOperand):Boolean;
function is_rsp(const r:TRegValue):Boolean;
function is_rsp(const r:TRegValues):Boolean;
function is_imm(const r:TInstruction):Boolean;
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
                   his_bt,
                   his_mri8,   //mem-reg-mm8
                   his_reg,    //reg only
                   his_ro,     //read only
                   his_wo,     //write only
                   his_rw,     //read-write
                   his_align,
                   his_unbs);  //unbalanced size r/m

 t_op_desc=packed record
  mem_reg:t_op_type; //reg_reg
  reg_mem:t_op_type; //reg_reg
  reg_imm:t_op_type; //mem_imm
  reg_im8:t_op_type; //mem_im8,reg_mem_im8,(his_mri8)->mem_reg_im8
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
                    code_ref);

procedure build_lea(var ctx:t_jit_context2;id:Byte;
                    reg:TRegValue;hint:t_lea_hint=[]);

function  cmp_reg(const r1,r2:TRegValue):Boolean;
function  cmp_reg(const r1,r2:TOperand):Boolean;
function  cmp_reg_cross(const r1,r2:TRegValue):Boolean;
function  new_reg(const Operand:TOperand):TRegValue;
function  new_reg_size(const r:TRegValue;const sizeof:TOperandSize):TRegValue;
function  new_reg_size(const r:TRegValue;const sizeof:TRegValues):TRegValue;
function  new_reg_size(const r:TRegValue;const sizeof:TOperand):TRegValue;
function  fix_size(const r:TRegValue):TRegValue;
function  is_rep_prefix(const i:TInstruction):Boolean;
function  is_segment(const r:TOperand):Boolean;
function  is_segment(const i:TInstruction):Boolean;
function  get_segment_value(const Operand:TOperand):Byte;
function  flags(const i:TInstruction):t_jit_lea;
function  flags(const ctx:t_jit_context2):t_jit_lea;

procedure op_jit_interrupt(var ctx:t_jit_context2);

procedure op_set_mem_imm(var ctx:t_jit_context2;mem:t_jit_leas;imm:Int64;hint:t_lea_hint=[]);
procedure op_set_reg_imm(var ctx:t_jit_context2;reg:TRegValue;imm:Int64);
procedure op_set_r14_imm(var ctx:t_jit_context2;imm:Int64);
procedure op_set_rip_imm(var ctx:t_jit_context2;imm:Int64);

procedure op_load_r14(var ctx:t_jit_context2;reg:TRegValue);
procedure op_save_r14(var ctx:t_jit_context2;reg:TRegValue);

procedure op_load_rsp(var ctx:t_jit_context2;reg:TRegValue);
procedure op_save_rsp(var ctx:t_jit_context2;reg:TRegValue);

procedure op_load_rbp(var ctx:t_jit_context2;reg:TRegValue);
procedure op_save_rbp(var ctx:t_jit_context2;reg:TRegValue);

procedure op_load(var ctx:t_jit_context2;reg:TRegValue;opr:Byte);
procedure op_save(var ctx:t_jit_context2;opr:Byte;reg:TRegValue);

procedure op_uplift(var ctx:t_jit_context2;const dst:TRegValue;mem_size:TOperandSize;hint:t_lea_hint=[]);

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
procedure op_emit_bmi_rrm(var ctx:t_jit_context2;const desc:t_op_type);

type
 t_instruction_info=record
  code_size:Byte;
  mema_size:Byte;
 end;

function get_instruction_info(addr:Pointer):t_instruction_info;

var
 jit_relative_analize:Boolean=True;
 jit_scan_switchtable:Boolean=True;
 jit_scan_nopsequence:Boolean=True;
 jit_memory_guard    :Boolean=False;

const
 //print calls of exported library functions
 jit_trace_hle_call=False;

implementation

uses
 vm_pmap_prot,
 machdep,
 kern_thr,
 md_systm,
 systm,
 kern_jit_asm,
 subr_backtrace;

function t_jit_context2.t_forward_point.c(n1,n2:p_forward_point):Integer;
begin
 Result:=Integer(n1^.dst>n2^.dst)-Integer(n1^.dst<n2^.dst);
end;

function t_jit_context2.t_switchtable_point.c(n1,n2:p_switchtable_point):Integer;
begin
 Result:=Integer(n1^.table>n2^.table)-Integer(n1^.table<n2^.table);
end;

function t_jit_context2.t_jumpslot.c(n1,n2:p_jumpslot):Integer;
begin
 Result:=Integer(n1^.addr>n2^.addr)-Integer(n1^.addr<n2^.addr);
end;

function t_jit_context2.t_label.c(n1,n2:p_label):Integer;
begin
 Result:=Integer(n1^.curr>n2^.curr)-Integer(n1^.curr<n2^.curr);
end;

function t_jit_context2.t_entry_point.c(n1,n2:p_entry_point):Integer;
begin
 Result:=Integer(n1^.src>n2^.src)-Integer(n1^.src<n2^.src);
end;

function t_jit_context2.is_text_addr(addr:QWORD):Boolean;
begin
 Result:=(addr>=text_start) and (addr<text___end);
end;

function t_jit_context2.is_map_addr(addr:QWORD):Boolean;
begin
 Result:=(addr>=text_start) and (addr<map____end);
end;

procedure t_jit_context2.add_export_point(nid:QWORD;native:Pointer;dst:PPointer);
var
 node:p_export_point;
begin
 if (native=nil) or (dst=nil) then Exit;
 node:=builder.Alloc(Sizeof(t_export_point));
 node^.nid   :=nid;
 node^.native:=native;
 node^.dst   :=dst;
 node^.next  :=export_list;
 export_list :=node;
end;

procedure t_jit_context2.add_import_point(guest,dst:PPointer);
var
 node:p_import_point;
begin
 if (guest=nil) or (dst=nil) then Exit;
 node:=builder.Alloc(Sizeof(t_import_point));
 node^.guest:=guest;
 node^.dst  :=dst;
 node^.next :=import_list;
 import_list:=node;
end;

procedure t_jit_context2.add_forward_link(node:p_forward_point;instruction:t_jit_i_link);
var
 link:p_forward_link;
begin
 if (node=nil) or (instruction=nil_link) then Exit;
 //
 link:=forward_link_cache;
 if (link<>nil) then
 begin
  forward_link_cache:=link^.next;
  link^.next:=nil
 end else
 begin
  link:=builder.Alloc(Sizeof(t_forward_link));
 end;
 //
 link^.instruction:=instruction;
 link^.next       :=node^.links.root;
 node^.links.root:=link;
end;

procedure t_jit_context2.Resolve_forwards(var links:t_forward_links;target:t_jit_i_link);
var
 node:p_forward_link;
begin
 //init
 node:=links.root;
 //
 While (node<>nil) do
 begin
  //extract
  links.root:=node^.next;
  //set node
  node^.instruction.target:=target;
  //cache
  node^.instruction:=Default(t_jit_i_link);
  node^.next       :=forward_link_cache;
  forward_link_cache:=node;
  //next
  node:=links.root;
 end;
end;


function t_jit_context2.add_forward_point(ptype:t_point_type;instruction:t_jit_i_link;dst:Pointer):p_forward_point;
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
  Result:=forward_point_cache;
  if (Result<>nil) then
  begin
   forward_point_cache:=Result^.pLeft;
   Result^.pLeft:=nil;
  end else
  begin
   Result:=builder.Alloc(Sizeof(t_forward_point));
  end;
  //
  Result^.dst:=dst;
  Result^.links.ptype:=ptype;
  //
  forward_set.Insert(Result);
 end;
 add_forward_link(Result,instruction);
end;

function t_jit_context2.add_forward_point(ptype:t_point_type;dst:Pointer):p_forward_point;
begin
 Result:=add_forward_point(ptype,nil_link,dst);
end;

//

procedure t_jit_context2.add_jumpslot(addr:Pointer);
var
 _node:t_jumpslot;
 pnode:p_jumpslot;
begin
 if (addr=nil) then Exit;

 _node.addr:=addr;

 if (jumpslot_set.Find(@_node)=nil) then
 begin
  pnode:=builder.Alloc(Sizeof(t_jumpslot));
  pnode^.addr:=addr;

  jumpslot_set.Insert(pnode);
 end;
end;

function t_jit_context2.is_jumpslot(addr:Pointer):Boolean;
var
 node:t_jumpslot;
begin
 node.addr:=addr;

 Result:=(jumpslot_set.Find(@node)<>nil);
end;

function t_jit_context2.add_switchtable(table:Pointer):p_switchtable_point;
var
 node:t_switchtable_point;
begin
 if (table=nil) then Exit;

 node.table:=table;
 //
 Result:=switchtable_set.Find(@node);
 if (Result=nil) then
 begin
  //
  Result:=builder.Alloc(Sizeof(t_switchtable_point));
  //
  Result^.table:=table;
  Result^.curr :=table;
  //
  switchtable_set.Insert(Result);
  //
  //mark minimum address for processing
  if (min_switchtable=nil) then
  begin
   min_switchtable:=Result;
  end else
  if (min_switchtable^.table>table) then
  begin
   min_switchtable:=Result;
  end;
  //
 end;
end;

function t_jit_context2.fetch_switchtable(var out_table:p_switchtable_point;var out_next:PInteger):Boolean;
var
 node,node_next:p_switchtable_point;
begin
 Result:=False;

 //move to min
 if (min_switchtable=nil) then Exit;
 switchtable_set._Splay(min_switchtable);
 node:=switchtable_set.pRoot;
 if (node=nil) then Exit;

 repeat

  node_next:=switchtable_set.Next(node);
  if (node_next<>nil) then
  begin
   if (node^.curr=nil) or
      (node^.curr>=node_next^.table) then
   begin
    //This is complete, let's move on to the next one.
    //
    min_switchtable:=node_next;
    node           :=node_next;
   end else
   begin
    out_table:=node;
    out_next :=node_next^.table;
    //
    Exit(True);
   end;
  end else
  if (node^.curr=nil) then
  begin
   //Terminated
   min_switchtable:=nil;
   //
   out_table:=nil;
   out_next :=nil;
   //
   Exit(False);
  end else
  begin
   //Last in list
   out_table:=node;
   out_next :=nil;
   //
   Exit(True);
  end;

 until false;

end;

//

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

function t_jit_context2.fetch_forward_nid(addr:Pointer):QWORD;
var
 f:t_forward_point;
 min:p_forward_point;
begin
 Result:=0;
 f.dst:=addr;
 min:=forward_set.Find(@f);
 if (min<>nil) then
 begin
  Result:=min^.nid;
 end;
end;

function t_jit_context2.fetch_forward_point(var links:t_forward_links;var dst:Pointer;var nid:QWORD):Boolean;
var
 min:p_forward_point;
begin
 Result:=False;
 //get min
 min:=forward_set.Min;
 if (min=nil) then Exit;
 //move to min
 forward_set._Splay(min);
 min:=forward_set.pRoot;
 //extract
 forward_set.Delete(min);
 //set
 dst  :=min^.dst;
 links:=min^.links;
 nid  :=min^.nid;
 //cache
 min^:=Default(t_forward_point);
 min^.pLeft:=forward_point_cache;
 forward_point_cache:=min;
 //
 Result:=True;
end;

function t_jit_context2.add_label(curr,next:Pointer;link_curr,link_next:t_jit_i_link;flags:Integer):p_label;
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
 Result^.flags    :=flags;
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
 key :t_entry_point;
 node:p_entry_point;
begin
 if (src=nil) then Exit;
 //set key
 key:=Default(t_entry_point);
 key.src        :=src;
 key.instruction:=label_id;
 //find exists
 node:=entry_set.Find(@key);
 if (node<>nil) then Exit; //Already added
 //new
 node:=builder.Alloc(Sizeof(t_entry_point));
 node^:=key;
 //insert set
 entry_set.Insert(node);
 //insert list
 node^.next    :=entry_list;
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

function is_rip(const r:TRegValues):Boolean; inline;
begin
 Result:=is_rip(r[0]) or is_rip(r[1]);
end;

function is_rip(const r:TOperand):Boolean; inline;
begin
 Result:=is_rip(r.RegValue);
end;

function is_rip(const r:TInstruction):Boolean;
var
 i:Integer;
begin
 Result:=False;
 if (r.OperCnt<>0) then
 For i:=1 to r.OperCnt do
 begin
  Result:=is_rip(r.Operand[i]);
  if Result then Exit;
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

function is_imm(const r:TInstruction):Boolean;
begin
 Result:=(r.Operand[1].RegValue[0].AType=regNone);
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

function is_high(const r:TRegValue):Boolean; inline;
begin
 Result:=r.AType=regGeneralH;
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

function new_reg_size(const r:TRegValue;const sizeof:TOperandSize):TRegValue; inline;
begin
 Result:=r;
 Result.ASize:=sizeof;
end;

function new_reg_size(const r:TRegValue;const sizeof:TRegValues):TRegValue; inline;
begin
 Result:=new_reg_size(r,sizeof[0].ASize);
end;

function new_reg_size(const r:TRegValue;const sizeof:TOperand):TRegValue; inline;
begin
 Result:=new_reg_size(r,sizeof.RegValue[0].ASize);
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
  else;
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

 ji.AInstructionSize:=ctx.dis.CodeIdx;

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

function scan_switchtable(var ctx:t_jit_context2;start:Int64):Boolean;
var
 table:PInteger;
 rel:Integer;
begin
 Result:=False;
 //
 if (cmDontScanSwitchTable in ctx.modes) then Exit;
 //
 table:=PInteger(start);
 //
 if ctx.is_text_addr(QWORD(table)) then
 begin

  rel:=0;
  if (copyin(table,@rel,SizeOf(Integer))<>0) then
  begin
   Exit;
  end;

  if (DWORD(rel) and $FFFF0000)=$FFFF0000 then
  begin
   ctx.add_switchtable(table);
   Result:=True;
  end;

 end;
end;

procedure add_rip_entry(var ctx:t_jit_context2;ofs:Int64;hint:t_lea_hint);
var
 new_ofs:Int64;
begin

 if (code_ref in hint) then
 begin
  //call [addr]
  //jmp  [addr]

  if not (cmDontScanRipRel in ctx.modes) then

  if ctx.is_map_addr(ofs) then
  if not ctx.is_jumpslot(Pointer(ofs)) then
  if ((ppmap_get_prot(QWORD(ofs)) and PAGE_PROT_READ)<>0) then
  begin
   new_ofs:=0;

   if (copyin(Pointer(ofs),@new_ofs,SizeOf(Pointer))=0) then
   begin
    if ctx.is_text_addr(new_ofs) and (new_ofs<=ctx.max_reloc) then
    begin
     ctx.add_forward_point(fpCall,Pointer(new_ofs));
    end;
   end;

  end;

 end else
 begin
  //lea

  if not (cmDontScanRipRel in ctx.modes) then

  if scan_switchtable(ctx,ofs) then
  begin
   //
  end else
  if ctx.is_text_addr(ofs) and (ofs<=ctx.max_reloc) then
  begin
   ctx.add_forward_point(fpData,Pointer(ofs));
  end;

 end;
end;

function is_segment(const i:TInstruction):Boolean; inline;
begin
 Result:=False;

 case i.SegmentReg of
  4:Result:=True;
  5:Result:=True;
  else;
 end;
end;

function get_segment(SegmentReg:Byte):Integer; inline;
begin
 Result:=0;

 case SegmentReg of
  4:Result:=teb_fsbase;
  5:Result:=teb_gsbase;
  else
    Assert(False);
 end;
end;

function get_segment(const i:TInstruction):Integer; inline;
begin
 Result:=get_segment(i.SegmentReg);
end;

type
 t_lea_component=(
  REG_NONE   ,
  REG_DIRECT ,
  REG_LOAD   ,
  REG_SCALE  ,
  REG_OFFSET ,
  REG_SEGMENT
 );

 t_lea_action=record
  op:t_lea_component;
  case Byte of
   0:(AReg:TRegValue);
   1:(AScale:Byte);
   2:(AOfs:Int64);
   3:(ASegment:Byte);
 end;

 t_lea_builder=packed object
  actions:array[0..4] of t_lea_action;
  actions_len :Byte;
  direct_count:Byte;
  loads_count :Byte;
  is_scale    :Boolean;
  function  is_reordering:Boolean;
  function  is_ext_reg   :Boolean;
  function  is_all_clear :Boolean;
  Procedure add_reg    (Reg:TRegValue);
  Procedure add_scale  (Scale:Byte);
  Procedure add_offset (Ofs:Int64);
  Procedure add_load   (Reg:TRegValue);
  Procedure add_segment(Segment:Byte);
 end;

function t_lea_builder.is_reordering:Boolean;
begin
 Result:=(direct_count<=1) and (loads_count=0) {and (not is_scale)};
end;

function t_lea_builder.is_ext_reg:Boolean;
begin
 Result:=(ord(direct_count<>0)+loads_count)>1;
end;

function t_lea_builder.is_all_clear:Boolean;
var
 r:Integer;
begin
 Result:=True;
 for r:=0 to actions_len-1 do
 begin
  if (actions[r].op<>REG_NONE) then
  begin
   Exit(False);
  end;
 end;
end;

Procedure t_lea_builder.add_reg(Reg:TRegValue);
var
 i:Integer;
begin
 if (Reg.AType=regNone) then Exit;
 Reg.AScale:=0;

 i:=actions_len;

 actions[i].op  :=REG_DIRECT;
 actions[i].AReg:=Reg;

 Inc(actions_len);
 Inc(direct_count);
end;

Procedure t_lea_builder.add_scale(Scale:Byte);
var
 i:Integer;
begin
 if (Scale<=1) then Exit;

 i:=actions_len;

 actions[i].op    :=REG_SCALE;
 actions[i].AScale:=Scale;

 Inc(actions_len);
 is_scale:=True;
end;

Procedure t_lea_builder.add_offset(Ofs:Int64);
var
 i:Integer;
begin
 if (Ofs=0) then Exit;

 i:=actions_len;

 actions[i].op  :=REG_OFFSET;
 actions[i].AOfs:=Ofs;

 Inc(actions_len);
end;

Procedure t_lea_builder.add_load(Reg:TRegValue);
var
 i:Integer;
begin
 if (Reg.AType=regNone) then Exit;
 Reg.AScale:=0;

 if is_reordering then
 begin
  //move
  if (actions_len<>0) then
  For i:=actions_len-1 downto 0 do
  begin
   actions[i+1]:=actions[i];
  end;
  //add to first
  i:=0;
 end else
 begin
  //add to last
  i:=actions_len;
 end;

 actions[i].op  :=REG_LOAD;
 actions[i].AReg:=Reg;

 Inc(actions_len);
 Inc(loads_count);
end;

Procedure t_lea_builder.add_segment(Segment:Byte);
var
 i:Integer;
begin

 if is_reordering then
 begin
  //move
  if (actions_len<>0) then
  For i:=actions_len-1 downto 0 do
  begin
   actions[i+1]:=actions[i];
  end;
  //add to first
  i:=0;
 end else
 begin
  //add to last
  i:=actions_len;
 end;

 actions[i].op      :=REG_SEGMENT;
 actions[i].ASegment:=Segment;

 Inc(actions_len);
 Inc(loads_count);
end;

type
 t_lea_allocs=object
  regs:array[0..1] of TRegValue;
  function count:Byte;
  function alloc:TRegValue;
 end;

function t_lea_allocs.count:Byte; inline;
begin
 Result:=ord(regs[0].AType<>regNone) + ord(regs[1].AType<>regNone);
end;

function t_lea_allocs.alloc:TRegValue;
begin
 if (regs[0].AType<>regNone) then
 begin
  Result:=regs[0];
  regs[0]:=Default(TRegValue);
 end else
 if (regs[1].AType<>regNone) then
 begin
  Result:=regs[1];
  regs[1]:=Default(TRegValue);
 end else
 begin
  Assert(False,'t_lea_allocs');
 end;
end;

function get_reg_count(const addres:t_jit_lea):Byte;  inline;
begin
 Result:=ord(addres.ARegValue[0].AType<>regNone) + ord(addres.ARegValue[1].AType<>regNone);
end;

procedure build_lea(var ctx:t_jit_context2;id:Byte;
                    reg:TRegValue;hint:t_lea_hint=[]);
var
 RegValue:TRegValues;
 adr:TRegValue;
 tmp:TRegValue;
 new:TRegValue;

 ofs:Int64;

 i,r:Integer;

 saved_reg    :Boolean;

 lea_builder:t_lea_builder;
 lea_allocs :t_lea_allocs;
 addres     :t_jit_lea;
begin
 RegValue:=ctx.din.Operand[id].RegValue;

 adr:=new_reg_size(reg,RegValue);

 if (adr.ASize=os0) then
 begin
  adr.ASize:=os64;
 end;

 case adr.AIndex of
  14:hint:=hint+[not_use_r_tmp0]; //r14
  15:hint:=hint+[not_use_r_tmp1]; //r15
  else;
 end;

 //reg1*scale + reg2 + offset + [segment]

 lea_builder:=Default(t_lea_builder);

 if is_rip(RegValue[0]) then //rip relative
 begin
  ofs:=0;
  GetTargetOfs(ctx.din,ctx.code,id,ofs);
  ofs:=Int64(ctx.ptr_next)+ofs;

  add_rip_entry(ctx,ofs,hint);

  with ctx.builder do
   if (classif_offset_u64(ofs)=os64) then
   begin
    if (reg.ASize=os64) then
    begin
     movi64(adr,ofs); //endpoint
    end else
    begin
     //32 bit relative address?
     movi(new_reg_size(reg,os32),ofs); //endpoint
    end;
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

  Exit; //break
 end;

 //reg1
 if is_preserved(RegValue[0]) then
 begin
  lea_builder.add_load(RegValue[0]);
 end else
 begin
  lea_builder.add_reg(RegValue[0]);
 end;

 if (RegValue[0].AType<>regNone) and
    (RegValue[1].AType<>regNone) and
    cmp_reg(RegValue[0],RegValue[1]) then
 begin
  //2,3,5,9
  lea_builder.add_scale(RegValue[0].AScale+1);
 end else
 begin
  lea_builder.add_scale(RegValue[0].AScale);

  //reg2
  if is_preserved(RegValue[1]) then
  begin
   lea_builder.add_load(RegValue[1]);
  end else
  begin
   lea_builder.add_reg(RegValue[1]);
  end;
 end;

 ofs:=0;
 GetTargetOfs(ctx.din,ctx.code,id,ofs);

 lea_builder.add_offset(ofs);

 if (not (not_use_segment in hint)) and
    is_segment(ctx.din) then
 begin
  lea_builder.add_segment(ctx.din.SegmentReg);
 end;

 if (lea_builder.actions_len=1) then
 begin
  //simple case

  with ctx.builder do
  case lea_builder.actions[0].op of
   REG_DIRECT:
    begin
     if not cmp_reg(reg,lea_builder.actions[0].AReg) then
     begin
      movq(reg,lea_builder.actions[0].AReg); //endpoint
     end;
    end;
   REG_LOAD:
    begin
     i:=GetFrameOffset(lea_builder.actions[0].AReg);
     movq(reg,[r_thrd+i]); //endpoint
    end;
   REG_OFFSET:
    begin
     //sign extend
     movi(reg,lea_builder.actions[0].AOfs); //endpoint
    end;
   REG_SEGMENT:
    begin
     i:=get_segment(lea_builder.actions[0].ASegment);
     movq(reg,[GS+i]); //endpoint
    end;
   else;
  end;

  Exit; //break
 end;

 //prepare temp reg
 tmp:=Default(TRegValue);
 saved_reg:=False;
 if (lea_builder.is_ext_reg) then
 begin
  if ([not_use_r_tmp0,not_use_r_tmp1]*hint=[not_use_r_tmp0,not_use_r_tmp1]) then
  begin
   //use rbp
   with ctx.builder do
   begin
    tmp:=rbp;
    push(tmp);
   end;
   saved_reg:=True;
  end else
  if (not_use_r_tmp0 in hint) then
  begin
   tmp:=r_tmp1;
  end else
  begin
   tmp:=r_tmp0;
  end;
  //
  lea_allocs.regs[0]:=tmp;
  lea_allocs.regs[1]:=adr;
 end else
 begin
  lea_allocs.regs[0]:=adr;
  lea_allocs.regs[1]:=Default(TRegValue);
 end;

 addres:=Default(t_jit_lea);

 repeat //start loop

  //prepare regs
  with ctx.builder do
  for r:=0 to lea_builder.actions_len-1 do
  begin
   case lea_builder.actions[r].op of
    REG_DIRECT:Break; //stop to do lea
    REG_LOAD:
      begin
       if (lea_allocs.count=0) then Break;
       new:=lea_allocs.alloc;
       i:=GetFrameOffset(lea_builder.actions[r].AReg);
       movq(new,[r_thrd+i]);
       //override
       lea_builder.actions[r].op  :=REG_DIRECT;
       lea_builder.actions[r].AReg:=new;
      end;
    REG_SEGMENT:
      begin
       if (lea_allocs.count=0) then Break;
       new:=lea_allocs.alloc;
       i:=get_segment(lea_builder.actions[r].ASegment);
       movq(new,[GS+i]);
       //override
       lea_builder.actions[r].op  :=REG_DIRECT;
       lea_builder.actions[r].AReg:=new;
      end;
    else;
   end;
  end;

  //prepare addres arg
  with ctx.builder do
  for r:=0 to lea_builder.actions_len-1 do
  begin
   case lea_builder.actions[r].op of
    REG_DIRECT:
     begin
      i:=get_reg_count(addres);
      if (i=2) then Break;
      //custom ordering add
      addres.ARegValue[i]:=lea_builder.actions[r].AReg;
      //clear
      lea_builder.actions[r].op:=REG_NONE;
     end;
    REG_SCALE:
     begin
      i:=get_reg_count(addres);
      if (lea_builder.actions[r].AScale and 1)<>0 then
      begin
       //3,5,9
       Assert(i=1,'unexpected sequence of scale');
       addres.ARegValue[1]:=addres.ARegValue[0];
       addres.ARegValue[0].AScale:=(lea_builder.actions[r].AScale-1);
      end else
      begin
       Assert(i<>0,'unexpected sequence of scale');
       if (i=2) then
       begin
        //swap
        new:=addres.ARegValue[1];
        new.AScale:=lea_builder.actions[r].AScale;
        addres.ARegValue[1]:=addres.ARegValue[0];
        addres.ARegValue[0]:=new;
       end else
       begin
        addres.ARegValue[0].AScale:=lea_builder.actions[r].AScale;
       end;
      end;
      //clear
      lea_builder.actions[r].op:=REG_NONE;
     end;
    REG_OFFSET:
     begin
      addres.AOffset:=lea_builder.actions[r].AOfs;
      //clear
      lea_builder.actions[r].op:=REG_NONE;
     end;
   end; //case
  end; //for

  with ctx.builder do
  begin
   if lea_builder.is_all_clear then
   begin
    leaq(reg,[addres]); //endpoint

    Break; //all is clear -> Break;
   end else
   begin
    leaq(adr,[addres]);

    //new reg summ
    addres:=adr;

    //restore allocs
    lea_allocs.regs[0]:=tmp;
    lea_allocs.regs[1]:=Default(TRegValue);
   end;
  end;

 until false;

 with ctx.builder do
 if saved_reg then
 begin
  //restore temp
  pop(tmp);
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

procedure op_jit_interrupt(var ctx:t_jit_context2);
begin
 with ctx.builder do
 begin
  //[65 FF 14 25] [00 07 00 00] call gs:[$00000700]
  //call([GS+teb_jit_trp]);

  //ctx.label_flags:=ctx.label_flags or LF_JMP_INTERRUPT;
 end;
end;

//

procedure op_set_mem_imm(var ctx:t_jit_context2;mem:t_jit_leas;imm:Int64;hint:t_lea_hint=[]);
var
 mreg:t_jit_lea;
 reg:TRegValue;
begin
 with ctx.builder do
 begin
  mreg:=Sums(mem);
  Assert(mreg.AMemSize<>os0);
  if (mreg.AMemSize=os64) then
  begin
   //64
   if (classif_offset_se64(imm)=os64) then
   begin
    //64 imm
    if (not_use_r_tmp0 in hint) and
       (not_use_r_tmp1 in hint) then
    begin
     //32bit zero extend (+0) (+4)
     movi([mreg+0,os32],Lo(imm));
     movi([mreg+4,os32],Hi(imm));
     Exit;
    end else
    if (not_use_r_tmp0 in hint) then
    begin
     reg:=r_tmp1;
    end else
    begin
     reg:=r_tmp0;
    end;
    //
    if (classif_offset_u64(imm)=os64) then
    begin
     //64bit imm
     movi64(reg,imm);
     movq([mreg,os64],reg);
    end else
    begin
     //32bit zero extend
     movi(new_reg_size(reg,os32),imm);
     movq([mreg,os64],reg);
    end;
    //64 imm
   end else
   begin
    //32bit sign extend
    movi([mreg,os64],imm);
   end;
   //64
  end else
  begin
   movi(mem,imm);
  end;
 end;
end;

procedure op_set_reg_imm(var ctx:t_jit_context2;reg:TRegValue;imm:Int64);
begin
 with ctx.builder do
 begin
  if (reg.ASize=os64) then
  begin
   if (classif_offset_u64(imm)=os64) then
   begin
    if (imm and QWORD($FFFFFFFF80000000))=QWORD($FFFFFFFF80000000) then
    begin
     //32bit sign extend
     movi(reg,imm);
    end else
    begin
     //64bit imm
     movi64(reg,imm);
    end;
   end else
   begin
    //32bit zero extend
    movi(new_reg_size(reg,os32),imm);
   end;
  end else
  begin
   movi(reg,imm);
  end;
 end;
end;

procedure op_set_r14_imm(var ctx:t_jit_context2;imm:Int64);
begin
 op_set_reg_imm(ctx,r_tmp0,imm);
end;

procedure op_set_rip_imm(var ctx:t_jit_context2;imm:Int64);
begin
 op_set_r14_imm(ctx,imm);
 //
 with ctx.builder do
 begin
  movq([r_thrd+(@p_jit_frame(nil)^.tf_rip)],r_tmp0);
 end;
end;

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

// VM_MAXUSER_ADDRESS
// $80000000000 = 1 shl 43
// 64-43 = 21

procedure op_uplift(var ctx:t_jit_context2;const dst:TRegValue;mem_size:TOperandSize;hint:t_lea_hint=[]);
var
 rbits:TRegValue;
 instr:t_jit_i_link;
begin
 if not jit_memory_guard then Exit;

 case dst.AIndex of
  14:hint:=hint+[not_use_r_tmp0]; //r14
  15:hint:=hint+[not_use_r_tmp1]; //r15
  else;
 end;

 with ctx.builder do
 begin
  if (not_use_r_tmp0 in hint) and
     (not_use_r_tmp1 in hint) then
  begin
   rbits:=r13;
  end else
  if (not_use_r_tmp0 in hint) then
  begin
   rbits:=r_tmp1;
  end else
  begin
   rbits:=r_tmp0;
  end;

  xchgq(rcx,rbits);

  //addres bits
  movi(new_reg_size(rcx,os8),VM_MAX_BITS);

  shrx(rcx,dst,rcx);

  instr:=jcxz(nil_link,as64,os8);

  //error
  xchgq(rcx,rbits);
  ud2;

  //next
  instr.target:=get_curr_label.after;

  xchgq(rcx,rbits);

  {
  //zero bits
  movi(new_reg_size(rbits,os8),64-VM_MAX_BITS); //mov  $21,%bpl

  //clear hi
  shlx(dst,dst,rbits); //shlx %rbp,%r14,%r14
  shrx(dst,dst,rbits); //shrx %rbp,%r14,%r14
  }

  if (rbits.AIndex=r13.AIndex) then
  begin
   //restore jit_frame
   movq(r13,[GS +teb_thread]);
   leaq(r13,[r13+jit_frame_offset]);
  end;
 end;
end;

procedure op_copyin(var ctx:t_jit_context2;const dst:TRegValue;mem_size:TOperandSize); inline;
begin
 op_uplift(ctx,dst,mem_size); //in/out:r14

 with ctx.builder do
 begin
  //call_far(copyin_mov_size[mem_size]); //in:r14(addr), out:r14
 end;
end;

procedure op_copyout_before(var ctx:t_jit_context2;const dst:TRegValue;var link_next:t_jit_i_link;mem_size:TOperandSize); inline;
begin
 op_uplift(ctx,dst,mem_size); //in/out:r14

 with ctx.builder do
 begin
  ////call_far(copyout_mov_size[mem_size]); //in:r14(addr)
  //
  ////link_next:=jmp(nil_link,os8);
 end;
end;

procedure op_copyout_after(var ctx:t_jit_context2;var link_next:t_jit_i_link;mem_size:TOperandSize); inline;
begin
 with ctx.builder do
 begin
  ////reta;
  //
  ////link_next._label:=get_curr_label.after;
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
       op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

       mem_in;
      end else
      begin
       op_copyin(ctx,r_tmp0,mem_size);

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

       op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

       mem_out;
      end else
      begin
       op_copyout_before(ctx,r_tmp0,link_next,mem_size);

       mem_out;

       op_copyout_after(ctx,link_next,mem_size);
      end;

     end;

    end;

   mo_ctx:
    begin
     mem_size:=ctx.din.Operand[1].Size;
     Assert(mem_size<>os0);

     if (not (his_reg in hint)) and
        ((his_ro in hint) or (mem_size<>os32)) then
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
     (get_mem_size(mem)<>os8) and
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
  if (not_impl in desc.reg_imm.opt) then
  begin
   _RMI8(desc.reg_im8,reg,mem,imm);
  end else
  if (imm_size=os8) and
    (get_mem_size(mem)<>os8) and
    (not (not_impl in desc.reg_im8.opt)) then
  begin
   _RMI8(desc.reg_im8,reg,mem,imm);
  end else
  begin
   _RMI(desc.reg_imm,reg,mem,imm);
  end;
end;

procedure op_rm_or_rmi(var ctx:t_jit_context2;
                       const desc:t_op_desc;
                       reg:TRegValue;mem:t_jit_leas);
var
 mem_size:TOperandSize;

 imm:Int64;
 imm_size:TOperandSize;
begin
 imm:=0;
 if GetTargetOfs(ctx.din,ctx.code,3,imm) then
 begin
  imm_size:=ctx.din.Operand[3].Size;
  mem_size:=get_mem_size(mem);
  Assert(mem_size<>os0);

  op_rmi(ctx,desc,reg,mem,imm,imm_size);
 end else
 begin
  ctx.builder._RM(desc.reg_mem,reg,mem);
 end;
end;

procedure op_mr_or_mri(var ctx:t_jit_context2;
                       const desc:t_op_desc;
                       reg:TRegValue;mem:t_jit_leas);
var
 mem_size:TOperandSize;

 imm:Int64;
 imm_size:TOperandSize;
begin
 imm:=0;
 if GetTargetOfs(ctx.din,ctx.code,3,imm) then
 begin
  imm_size:=ctx.din.Operand[3].Size;
  mem_size:=get_mem_size(mem);
  Assert(mem_size<>os0);

  op_rmi(ctx,desc,reg,mem,imm,imm_size);
 end else
 begin
  ctx.builder._RM(desc.mem_reg,reg,mem);
 end;
end;

procedure op_rri(var ctx:t_jit_context2;
                 const desc:t_op_desc;
                 reg1,reg2:TRegValue;
                 imm:Int64;
                 mem_size,imm_size:TOperandSize);
label
 _mri8;
begin
 with ctx.builder do
  if (not_impl in desc.reg_imm.opt) then
  begin
   goto _mri8;
  end else
  if (imm_size=os8) and
     (mem_size<>os8) and
     (not (not_impl in desc.reg_im8.opt)) then
  begin
   _mri8:
   if (his_mri8 in desc.hint) then
   begin
    //mri8
    _RRI8(desc.reg_im8,reg1,reg2,imm,mem_size);
   end else
   begin
    //rmi8
    _RRI8(desc.reg_im8,reg2,reg1,imm,mem_size); //swapped
   end;
  end else
  begin
   _RRI(desc.reg_imm,reg2,reg1,imm,mem_size); //swapped?
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
   //reg mem
   _RR(desc.reg_mem,reg1,reg2,True); //swapped
  end else
  begin
   //mem reg
   _RR(desc.mem_reg,reg1,reg2,False);
  end;
end;

procedure op_rr_or_rri(var ctx:t_jit_context2;
                       const desc:t_op_desc;
                       reg1,reg2:TRegValue;
                       mem_size:TOperandSize);
var
 imm:Int64;
 imm_size:TOperandSize;
begin
 imm:=0;
 if GetTargetOfs(ctx.din,ctx.code,3,imm) then
 begin
  imm_size:=ctx.din.Operand[3].Size;

  op_rri(ctx,desc,reg1,reg2,imm,mem_size,imm_size);
 end else
 begin
  op_rr(ctx,desc,reg1,reg2,mem_size);
 end;
end;

const
 ax_id=0;
 cx_id=1;
 dx_id=2;
 bx_id=3;

type
 t_lo_regi=0..3;
 t_lo_regs=Set of t_lo_regi;

function get_implicit_regs(var ctx:t_jit_context2):t_lo_regs;
begin
 Result:=[];

 case ctx.din.OpCode.Opcode of
  OPcmpxchg:
    case ctx.din.OpCode.Suffix of
     OPSnone: Result:=[ax_id];
     else
              Result:=[ax_id,dx_id];
    end;

  OPmul:
    case ctx.din.OpCode.Suffix of
     OPSnone: Result:=[ax_id,dx_id];
     OPSx_x : Result:=[dx_id];
     else;
    end;

  OPimul:     Result:=[ax_id,dx_id];

  OPdiv:
    case ctx.din.OpCode.Suffix of
     OPSnone: Result:=[ax_id,dx_id];
     else;
    end;

  OPidiv:     Result:=[ax_id,dx_id];

  OPpcmpestri:Result:=[cx_id];
  OPpcmpistri:Result:=[cx_id];

  OProl:      Result:=[cx_id];
  OPror:      Result:=[cx_id];
  OPrcl:      Result:=[cx_id];
  OPrcr:      Result:=[cx_id];
  OPshl:      Result:=[cx_id];
  OPshr:      Result:=[cx_id];
  OPsar:      Result:=[cx_id];

  OPcbw :     Result:=[ax_id];
  OPcwde:     Result:=[ax_id];
  OPcdqe:     Result:=[ax_id];

  OPcwd:      Result:=[ax_id,dx_id];
  OPcdq:      Result:=[ax_id,dx_id];
  OPcqo:      Result:=[ax_id,dx_id];

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

 Result.AIndex:=bx_id;

 repeat

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
    if (Result.AIndex=0) then Break;
    Dec(Result.AIndex);
    Continue;
   end;
  end;

  Exit;
 until false;

 Assert(False);
end;

function alloc_tmp_lo(var ctx:t_jit_context2;i:Byte):TRegValue; inline;
begin
 Result:=alloc_tmp_lo(ctx,ctx.din.Operand[i].RegValue[0].ASize);
end;

type
 t_override_ctx=record
  original:TRegValue;
  enable  :Boolean;
  xchg    :Boolean;
 end;

procedure override_mem_in_beg(var ctx:t_jit_context2;
                              var ovr:t_override_ctx;
                              hint:t_op_hint;
                              var reg:TRegValue);
begin
 with ctx.builder do
 begin
  ovr.enable:=is_high(reg);

  if ovr.enable then
  begin
   ovr.original:=reg;

   //get override reg
   reg:=alloc_tmp_lo(ctx,1);

   ovr.xchg:=True;

   if ovr.xchg then
   begin
    //xchg hack
    xchgq(reg,ovr.original);
   end else
   begin
    //save reg
    push(fix_size8(reg));
    //
    if (not (his_wo in hint)) or
       (his_ro in hint) then
    begin
     movq(reg,ovr.original);
    end;
   end;

  end;

 end;
end;

procedure override_mem_in_fin(var ctx:t_jit_context2;
                              var ovr:t_override_ctx;
                              hint:t_op_hint;
                              var reg:TRegValue);
begin
 with ctx.builder do
 begin

  if (ovr.enable) then
  begin

   if ovr.xchg then
   begin
    //xchg hack
    xchgq(ovr.original,reg);
   end else
   begin
    if not (his_ro in hint) then
    begin
     movq(ovr.original,reg);
    end;
    //restore reg
    pop(fix_size8(reg));
   end;

   reg:=ovr.original;
  end;

 end;
end;

procedure override_mem_out_beg(var ctx:t_jit_context2;
                               var ovr:t_override_ctx;
                               hint:t_op_hint;
                               var reg:TRegValue);
begin
 with ctx.builder do
 begin
  ovr.enable:=is_high(reg);

  if ovr.enable then
  begin
   ovr.original:=reg;

   //get override reg
   reg:=alloc_tmp_lo(ctx,2);

   ovr.xchg:=True;

   if ovr.xchg then
   begin
    //xchg hack
    xchgq(reg,ovr.original);
   end else
   begin
    //save reg
    push(fix_size8(reg));
    //
    movq(reg,ovr.original);
   end;

  end;

 end;
end;

procedure override_mem_out_fin(var ctx:t_jit_context2;
                               var ovr:t_override_ctx;
                               hint:t_op_hint;
                               var reg:TRegValue);
begin
 with ctx.builder do
 begin

  if ovr.enable then
  begin

   if ovr.xchg then
   begin
    //xchg hack
    xchgq(ovr.original,reg);
   end else
   begin
    if (his_xchg in hint) then
    begin
     movq(ovr.original,reg);
    end;
    //restore reg
    pop(fix_size8(reg));
   end;

   reg:=ovr.original;
  end;

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

 cmp_opr:Boolean;

 new1_load:Boolean;

 ovr:t_override_ctx;

 procedure mem_out;
 begin
  with ctx.builder do
   case memop of
    mo_mem_reg:
      begin
       //input:r14

       new2:=new_reg(ctx.din.Operand[2]);

       override_mem_out_beg(ctx,ovr,desc.hint,new2);

       op_mr_or_mri(ctx,desc,new2,[flags(ctx)+r_tmp0,mem_size]);

       override_mem_out_fin(ctx,ovr,desc.hint,new2);

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

       op_mr_or_mri(ctx,desc,new2,[flags(ctx)+r_tmp0,mem_size]);

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

       override_mem_in_beg(ctx,ovr,desc.hint,new1);

       op_rm_or_rmi(ctx,desc,new1,[flags(ctx)+r_tmp0,mem_size]);

       override_mem_in_fin(ctx,ovr,desc.hint,new1);

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

       op_rm_or_rmi(ctx,desc,new1,[flags(ctx)+r_tmp0,mem_size]);

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

      override_mem_out_beg(ctx,ovr,desc.hint,new2);

      op_mr_or_mri(ctx,desc,new2,[flags(ctx)+r_tmp0,mem_size]);

      override_mem_out_fin(ctx,ovr,desc.hint,new2);

     end;

    mo_mem_ctx:
     begin
      //input:r14

      new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

      op_load(ctx,new2,2);

      op_mr_or_mri(ctx,desc,new2,[flags(ctx)+r_tmp0,mem_size]);

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
        op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

        mem_in;
       end else
       begin
        op_copyin(ctx,r_tmp0,mem_size);

        mem_in;
       end;

      end else
      if (mem_size=os8) or
         (his_rw in desc.hint) then
      begin
       op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

       mem_out;
      end else
      begin
       op_copyout_before(ctx,r_tmp0,link_next,mem_size);

       mem_out;

       op_copyout_after(ctx,link_next,mem_size);
      end;
     end;

   mo_reg_mem,
   mo_ctx_mem:
     begin
      if (mem_size=os8) or
         (his_rw in desc.hint) then
      begin
       op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

       mem_in;
      end else
      begin
       op_copyin(ctx,r_tmp0,mem_size);

       mem_in;
      end;
     end;

   mo_ctx_reg:
     begin
      new2:=new_reg(ctx.din.Operand[2]);

      override_mem_out_beg(ctx,ovr,desc.hint,new2);

      mem_size:=ctx.din.Operand[1].RegValue[0].ASize;
      Assert(mem_size<>os0);

      if ((his_ro in desc.hint) or (mem_size<>os32)) and
         (not (his_bt in desc.hint)) and
         (not (his_unbs in desc.hint)) and //TODO:is the second parameter always read only?
         (not (not_impl in desc.mem_reg.opt)) then
      begin
       i:=GetFrameOffset(ctx.din.Operand[1]);
       op_mr_or_mri(ctx,desc,new2,[r_thrd+i,mem_size]);
      end else
      begin

       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       if (not (his_wo in desc.hint)) or
          (his_ro in desc.hint) then
       begin
        op_load(ctx,new1,1);
       end;

       op_rr_or_rri(ctx,desc,new1,new2,mem_size);

       if not (his_ro in desc.hint) then
       begin
        op_save(ctx,1,fix_size(new1));
       end;

      end;

      override_mem_out_fin(ctx,ovr,desc.hint,new2);

     end;
   mo_reg_ctx:
     begin
      new1:=new_reg(ctx.din.Operand[1]);

      override_mem_in_beg(ctx,ovr,desc.hint,new1);

      mem_size:=ctx.din.Operand[2].RegValue[0].ASize;
      Assert(mem_size<>os0);

      if ((his_ro in desc.hint) or (mem_size<>os32)) and
         (not (his_bt in desc.hint)) and
         (not (his_unbs in desc.hint)) and
         (not (not_impl in desc.reg_mem.opt)) then
      begin
       i:=GetFrameOffset(ctx.din.Operand[2]);
       op_rm_or_rmi(ctx,desc,new1,[r_thrd+i,mem_size]);
      end else
      begin
       new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

       op_load(ctx,new2,2);

       op_rr_or_rri(ctx,desc,new1,new2,mem_size);
      end;

      override_mem_in_fin(ctx,ovr,desc.hint,new1);

     end;
   mo_ctx_ctx:
     begin
      mem_size:=ctx.din.Operand[1].RegValue[0].ASize;
      Assert(mem_size<>os0);

      cmp_opr:=cmp_reg(ctx.din.Operand[1],ctx.din.Operand[2]);

      if ((his_ro in desc.hint) or (mem_size<>os32)) and
         (not (his_bt in desc.hint)) and
         (not (his_unbs in desc.hint)) and
         (not (not_impl in desc.mem_reg.opt)) and
         (not cmp_opr) then
      begin
       new2:=new_reg_size(r_tmp1,ctx.din.Operand[2]);

       op_load(ctx,new2,2);

       i:=GetFrameOffset(ctx.din.Operand[1]);
       op_mr_or_mri(ctx,desc,new2,[r_thrd+i,mem_size]);
      end else
      begin

       new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       new1_load:=False;

       if ((his_xor in desc.hint) and cmp_opr) then
       begin
        //fake load
        new1_load:=True;
       end else
       if (not (his_wo in desc.hint)) or
          (his_ro in desc.hint) then
       begin
        op_load(ctx,new1,1);
        new1_load:=True;
       end;

       if cmp_opr then
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

       op_rr_or_rri(ctx,desc,new1,new2,mem_size);

       if not (his_ro in desc.hint) then
       begin
        op_save(ctx,1,fix_size(new1));
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

       if ((his_ro in desc.hint) or (mem_size<>os32)) and
          (not (his_bt in desc.hint)) then
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
       op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

       mem_out;
      end else
      begin
       op_copyout_before(ctx,r_tmp0,link_next,mem_size);

       mem_out;

       op_copyout_after(ctx,link_next,mem_size);
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

       Assert(not is_high(new2));

       _RMI8(desc.reg_im8,new2,[flags(ctx)+r_tmp0],imm);
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

       Assert(not is_high(new2));

       _RM(desc.mem__cl,new2,[flags(ctx)+r_tmp0]);
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
       op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

       mem_out;
      end else
      begin
       op_copyout_before(ctx,r_tmp0,link_next,mem_size);

       mem_out;

       op_copyout_after(ctx,link_next,mem_size);
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

      Assert(not is_high(new1));
      Assert(not is_high(new2));

      _RRI8(desc.reg_im8,new1,new2,imm,mem_size);

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

      Assert(not is_high(new1));
      Assert(not is_high(new2));

      _RR(desc.mem__cl,new1,new2,False);

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

      Assert(not is_high(new1));
      Assert(not is_high(new2));

      _RRI8(desc.reg_im8,new1,new2,imm,mem_size);
     end;

    mo_reg_cl:
     begin
      //reg_ctx_cl

      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      new1:=new_reg(ctx.din.Operand[1]);
      new2:=new_reg_size(r_tmp0,ctx.din.Operand[2]);

      op_load(ctx,new2,2);

      Assert(not is_high(new1));
      Assert(not is_high(new2));

      _RR(desc.mem__cl,new1,new2,False);
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
       op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

       mem_in;
      end else
      begin
       op_copyin(ctx,r_tmp0,mem_size);

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

       op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

       mem_out;
      end else
      begin
       op_copyout_before(ctx,r_tmp0,link_next,mem_size);

       mem_out;

       op_copyout_after(ctx,link_next,mem_size);
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
   mo_mem_reg:
     begin
      if (his_align in desc.hint) then
      begin
       op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

       mem_out;
      end else
      begin
       op_copyout_before(ctx,r_tmp0,link_next,mem_size);

       mem_out;

       op_copyout_after(ctx,link_next,mem_size);
      end;
     end;

   mo_reg_mem,
   mo_ctx_mem:
     begin
      if (his_align in desc.hint) then
      begin
       op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

       mem_in;
      end else
      begin
       op_copyin(ctx,r_tmp0,mem_size);

       mem_in;
      end;
     end;

   mo_ctx_reg:
     begin
      new1:=new_reg(ctx.din.Operand[2]);

      mem_size:=ctx.din.Operand[1].Size;
      Assert(mem_size<>os0);

      if (not (not_impl in desc.mem_reg.opt)) then
      begin

       if ((his_ro in desc.hint) or (mem_size<>os32)) then
       begin
        i:=GetFrameOffset(ctx.din.Operand[1]);
        _VM(desc.mem_reg,new1,[r_thrd+i,mem_size]);
       end else
       begin
        new2:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

        if (not (his_wo in desc.hint)) or
           (his_ro in desc.hint) then
        begin
         op_load(ctx,new2,1);
        end;

        _VV(desc.mem_reg,new1,new2,mem_size);

        if not (his_ro in desc.hint) then
        begin
         op_save(ctx,1,fix_size(new2));
        end;
       end;

      end else
      begin
       new2:=new_reg_size(r_tmp0,ctx.din.Operand[1]);

       if (not (his_wo in desc.hint)) or
          (his_ro in desc.hint) then
       begin
        op_load(ctx,new2,1);
       end;

       _VV(desc.reg_mem,new2,new1,mem_size); //swapped

       if not (his_ro in desc.hint) then
       begin
        op_save(ctx,1,fix_size(new2));
       end;
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
    op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

    mem_in;
   end else
   begin
    op_copyin(ctx,r_tmp0,mem_size);

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
    op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

    mem_out;
   end else
   begin
    op_copyout_before(ctx,r_tmp0,link_next,mem_size);

    mem_out;

    op_copyout_after(ctx,link_next,mem_size);
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
      op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

      mem_out;
     end else
     begin
      op_copyout_before(ctx,r_tmp0,link_next,mem_size);

      mem_out;

      op_copyout_after(ctx,link_next,mem_size);
     end;
    end;

   mo_reg_mem:
     begin
      build_lea(ctx,2,r_tmp0);
      mem_size:=ctx.din.Operand[2].Size;
      Assert(mem_size<>os0);

      if (mem_size=os8) then
      begin
       op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

       mem_in;
      end else
      begin
       op_copyin(ctx,r_tmp0,mem_size);

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

     if (not (not_impl in desc.mri.opt)) then
     begin
      _VVI8(desc.mri,new2,new1,imm,mem_size); //swapped
     end else
     begin
      _VVI8(desc.rmi,new1,new2,imm,mem_size);
     end;

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
       op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

       mem_in;
      end else
      begin
       op_copyin(ctx,r_tmp0,mem_size);

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
    op_uplift(ctx,r_tmp0,mem_size); //in/out:r14

    mem_in;
   end else
   begin
    op_copyin(ctx,r_tmp0,mem_size);

    mem_in;
   end;
  end else
  begin
   Assert(false);
  end;
end;

//

procedure op_emit_bmi_rrm(var ctx:t_jit_context2;const desc:t_op_type);
var
 mem_size:TOperandSize;

 new1,new2,new3:TRegValue;

 pr_res,pr_mem:Boolean;

 tmp_count:Byte;

 function tmp_alloc:TRegValue; inline;
 begin
  case tmp_count of
   0:Result:=r_tmp0;
   1:Result:=r_tmp1;
   2:Result:=r_thrd;
   else
    Assert(False,'tmp_alloc');
  end;
  Inc(tmp_count);
 end;

begin
 Assert(ctx.din.OperCnt=3);

 tmp_count:=0;

 with ctx.builder do
 begin

  pr_res:=is_preserved(ctx.din.Operand[1]);
  pr_mem:=is_memory   (ctx.din.Operand[3]);

  //preload addr first
  if pr_mem then
  begin
   mem_size:=ctx.din.Operand[3].Size;
   Assert(mem_size<>os0);

   new3:=tmp_alloc;

   build_lea(ctx,3,new3);

   op_copyin(ctx,new3,mem_size);
  end;

  if pr_res then
  begin
   //The value is always overwritten so it does not need to be allocated.
   new1:=new_reg_size(r_tmp0,ctx.din.Operand[1]);
   //not need load result
  end else
  begin
   new1:=new_reg(ctx.din.Operand[1]);
  end;

  if is_preserved(ctx.din.Operand[2]) then
  begin
   new2:=new_reg_size(tmp_alloc,ctx.din.Operand[2]);
   //
   op_load(ctx,new2,2);
  end else
  begin
   new2:=new_reg(ctx.din.Operand[2]);
  end;

  if pr_mem then
  begin
   _VVM(desc,new1,new2,[new3]); //1 2 3
  end else
  if is_preserved(ctx.din.Operand[3]) then
  begin
   new3:=new_reg_size(tmp_alloc,ctx.din.Operand[3]);
   //
   op_load(ctx,new3,3);
   //
   _VVV(desc,new1,new2,new3,new1.ASize); //1 2 3
  end else
  begin
   new3:=new_reg(ctx.din.Operand[3]);
   //
   _VVV(desc,new1,new2,new3,new1.ASize); //1 2 3
  end;

  if (tmp_count=3) then
  begin
   //restore jit_frame
   movq(r13,[GS +teb_thread]);
   leaq(r13,[r13+jit_frame_offset]);
  end;

  //store result
  if pr_res then
  begin
   op_save(ctx,1,fix_size(new1));
  end;

 end;
end;

//

function get_instruction_info(addr:Pointer):t_instruction_info;
type
 t_data_16=array[0..15] of Byte;
var
 ptr:Pointer;
 data:t_data_16;

 dis:TX86Disassembler;
 din:TInstruction;

 i:Byte;
begin
 if (Int64(addr)<$4000) then Exit(Default(t_instruction_info));

 dis:=Default(TX86Disassembler);
 din:=Default(TInstruction);

 data:=Default(t_data_16);
 md_copyin(addr,@data,16,nil);

 ptr:=@data;

 dis.Disassemble(dm64,ptr,din);

 Result.code_size:=(ptr-addr);
 Result.mema_size:=0;

 if (din.OperCnt<>0) then
 For i:=1 to din.OperCnt do
 if is_memory(din.Operand[i]) then
 begin
  Result.mema_size:=OPERAND_BYTES[din.Operand[i].Size];
  Exit;
 end;

 if (din.OpCode.Prefix=OPPnone) then
 begin
  case din.OpCode.Opcode of
   OPpush,
   OPpop,
   OPpushf,
   OPpopf:Result.mema_size:=OPERAND_BYTES[din.Operand[1].Size];
   //
   OPins ,
   OPouts,
   OPmovs,
   OPlods,
   OPstos,
   OPcmps,
   OPscas:
      case din.OpCode.Suffix of
       OPSx_b:begin Result.mema_size:=1; Exit; end;
       OPSx_w:begin Result.mema_size:=2; Exit; end;
       OPSx_d:begin Result.mema_size:=4; Exit; end;
       OPSx_q:begin Result.mema_size:=8; Exit; end;
       else;
      end;
   else;
  end;
 end;

 print_disassemble(@data,Result.code_size);

 Assert(false,'get_instruction_info');
end;

//

end.




