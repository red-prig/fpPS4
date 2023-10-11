unit kern_jit_dynamic;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 hamt,
 g23tree,
 murmurhash,
 x86_jit,
 kern_jit2_ctx,
 kern_rwlock,
 kern_thr;

{
  entry_point -> +----------+    +---------+
                 |code_chunk| -> |code_blob|
  entry_point -> |          |    |         |
                 +----------+    |         |
  entry_point -> +----------+    |         |
                 |code_chunk| -> |         |
  entry_point -> |          |    |         |
                 +----------+    +---------+
}

type
 p_jit_dynamic=^t_jit_dynamic;
 t_jit_dynamic=object
  type
   p_entry_point=^t_entry_point;
   t_entry_point=object
    next:p_entry_point;
    blob:p_jit_dynamic;
    src :Pointer;
    dst :Pointer;
    procedure inc_ref;
    procedure dec_ref;
   end;

   p_instr_len=^t_instr_len;
   t_instr_len=packed record
    original:Byte;
    recompil:Byte;
   end;

   p_jcode_chunk=^t_jcode_chunk;
   t_jcode_chunk=object
    next  :p_jcode_chunk;
    pLeft :p_jcode_chunk;
    pRight:p_jcode_chunk;
    blob  :p_jit_dynamic;
    start :QWORD;
    __end :QWORD;
    dest  :QWORD;
    hash  :QWORD; //MurmurHash64A(Pointer(start),__end-start,$010CA1C0DE);
    count :QWORD;
    table :record end; //p_instr_len[]
    function c(n1,n2:p_jcode_chunk):Integer; static;
    procedure inc_ref;
    procedure dec_ref;
    function  find_addr(addr:QWORD):QWORD;
   end;

   t_jcode_chunk_set=specialize T23treeSet<p_jcode_chunk,t_jcode_chunk>;

  var
   entry_list:p_entry_point;
   chunk_list:p_jcode_chunk;

   base:Pointer;
   size:ptruint;

   lock:Pointer;
   refs:Integer;

  procedure inc_ref;
  procedure dec_ref;
  procedure Free;
  function  add_entry_point(src,dst:Pointer):p_entry_point;
  function  new_chunk(count:QWORD):p_jcode_chunk;
  procedure alloc_base(_size:ptruint);
  procedure free_base;
  procedure attach_entry(node:p_entry_point);
  procedure attach_entry;
  procedure attach_chunk;
  procedure attach;
  procedure detach_entry;
  procedure detach_chunk;
  procedure detach;
 end;

 p_jctx=^t_jctx;
 t_jctx=object
  frame:jit_frame;
  cblob:p_jit_dynamic;
 end;

function new_blob(_size:ptruint):p_jit_dynamic;

var
 entry_hamt_lock:Pointer=nil;
 entry_hamt:TSTUB_HAMT64;

 entry_chunk_lock:Pointer=nil;
 entry_chunk:t_jit_dynamic.t_jcode_chunk_set;

function  fetch_entry(src:Pointer):t_jit_dynamic.p_entry_point;
function  exist_entry(src:Pointer):Boolean;
function  fetch_chunk(src:Pointer):t_jit_dynamic.p_jcode_chunk;
function  next_chunk(node:t_jit_dynamic.p_jcode_chunk):t_jit_dynamic.p_jcode_chunk;
function  preload_entry(addr:Pointer):t_jit_dynamic.p_entry_point;

procedure switch_to_jit();
function  jmp_dispatcher(addr:Pointer;is_call:Boolean):Pointer;

procedure build(var ctx:t_jit_context2);

procedure preload(addr:Pointer);

implementation

uses
 sysutils,
 vmparam,
 vm_pmap,
 trap,
 kern_jit2;

function scan_up_exc(addr:QWORD):QWORD;
begin
 addr:=(addr+PAGE_MASK) and (not PAGE_MASK);

 while is_guest_addr(addr) do
 begin
  Result:=addr;

  if ((pmap_get_raw(QWORD(addr)) and PAGE_PROT_EXECUTE)=0) then
  begin
   Break;
  end;

  addr:=addr+PAGE_SIZE;
 end;

end;

function scan_dw_exc(addr:QWORD):QWORD;
begin
 addr:=addr and (not PAGE_MASK);

 while is_guest_addr(addr) do
 begin
  Result:=addr;

  if ((pmap_get_raw(QWORD(addr)) and PAGE_PROT_EXECUTE)=0) then
  begin
   Break;
  end;

  addr:=addr-PAGE_SIZE;
 end;

end;

procedure preload(addr:Pointer);
var
 node:t_jit_dynamic.p_entry_point;
 ctx:t_jit_context2;
begin
 Writeln('unk addr:0x',HexStr(addr));

 node:=preload_entry(addr);

 if (node=nil) then
 begin
  ctx:=Default(t_jit_context2);

  ctx.text_start:=scan_dw_exc(QWORD(addr));
  ctx.text___end:=scan_up_exc(QWORD(addr));
  ctx.map____end:=ctx.text___end;
  ctx.max       :=QWORD(-1); //dont scan rip relative

  ctx.add_forward_point(addr);

  pick(ctx);
 end else
 begin
  node^.dec_ref;
 end;
end;

procedure switch_to_jit();
label
 _start;
var
 td:p_kthread;
 node:t_jit_dynamic.p_entry_point;
 jctx:p_jctx;
 jit_state:Boolean;
begin
 td:=curkthread;
 if (td=nil) then Exit;

 jit_state:=((td^.pcb_flags and PCB_IS_JIT)<>0);

 if not is_guest_addr(td^.td_frame.tf_rip) then
 begin
  Assert(False,'TODO');
 end;

 _start:

 node:=fetch_entry(Pointer(td^.td_frame.tf_rip));

 if (node=nil) then
 begin
  preload(Pointer(td^.td_frame.tf_rip));
  goto _start;
 end;

 if (td^.td_jit_ctx=nil) then
 begin
  td^.td_jit_ctx:=AllocMem(SizeOf(t_jctx));
 end;
 jctx:=td^.td_jit_ctx;

 if (jctx^.cblob<>nil) then
 begin
  jctx^.cblob^.dec_ref;
  jctx^.cblob:=nil;
 end;

 jctx^.cblob:=node^.blob;

 jctx^.frame.tf_rax:=td^.td_frame.tf_rax;
 jctx^.frame.tf_rsp:=td^.td_frame.tf_rsp;
 jctx^.frame.tf_rbp:=td^.td_frame.tf_rbp;
 jctx^.frame.tf_r14:=td^.td_frame.tf_r14;
 jctx^.frame.tf_r15:=td^.td_frame.tf_r15;

 td^.td_frame.tf_rsp:=QWORD(td^.td_kstack.stack);
 td^.td_frame.tf_rbp:=QWORD(td^.td_kstack.stack);

 td^.td_frame.tf_rip:=QWORD(node^.dst);
 td^.td_frame.tf_r15:=QWORD(jctx);

 set_pcb_flags(td,PCB_FULL_IRET or PCB_IS_JIT);
end;

function fetch_chunk(src:Pointer):t_jit_dynamic.p_jcode_chunk;
var
 i:t_jit_dynamic.t_jcode_chunk_set.Iterator;
 node:t_jit_dynamic.t_jcode_chunk;
begin
 Result:=nil;
 node:=Default(t_jit_dynamic.t_jcode_chunk);
 node.start:=QWORD(src);
 //
 rw_rlock(entry_chunk_lock);

 i:=entry_chunk.Find_le(@node);

 if (i.Item<>nil) then
 begin
  Result:=i.Item^;
 end;

 if (Result<>nil) then
 begin
  Result^.inc_ref;
 end;

 rw_runlock(entry_chunk_lock);
end;

function next_chunk(node:t_jit_dynamic.p_jcode_chunk):t_jit_dynamic.p_jcode_chunk;
var
 i:t_jit_dynamic.t_jcode_chunk_set.Iterator;
begin
 Result:=nil;
 //
 rw_rlock(entry_chunk_lock);

 i:=entry_chunk.find_be(node);

 if (i.Item<>nil) then
 begin
  i.Next;
 end;

 if (i.Item<>nil) then
 begin
  Result:=i.Item^;
 end;

 if (Result<>nil) then
 begin
  Result^.inc_ref;
 end;

 rw_runlock(entry_chunk_lock);
end;

function preload_entry(addr:Pointer):t_jit_dynamic.p_entry_point;
var
 curr,next:t_jit_dynamic.p_jcode_chunk;
 blob:p_jit_dynamic;
 dest:QWORD;
begin
 Result:=nil;

 curr:=fetch_chunk(addr);
 while (curr<>nil) do
 begin

  //Writeln(HexStr(addr),':',HexStr(curr^.start,16),'..',HexStr(curr^.__end,16));

  if (QWORD(addr)<curr^.start) then Break;

  dest:=curr^.find_addr(QWORD(addr));

  if (dest<>0) then
  begin
   Writeln('cache:',HexStr(addr),'->',HexStr(dest,16));
   //writeln;

   blob:=curr^.blob;

   rw_wlock(blob^.lock);
    Result:=blob^.add_entry_point(addr,Pointer(dest));
   rw_wunlock(blob^.lock);

   blob^.attach_entry(Result);

   Exit;
  end;

  next:=next_chunk(curr);
  curr^.dec_ref;
  curr:=next;
 end;

 if (curr<>nil) then
 begin
  curr^.dec_ref;
 end;
end;

procedure jit_call_internal; assembler; nostackframe;
asm
 //pop host call
 mov jit_frame.tf_rsp(%r15),%rax
 lea 8(%rax),%rax
 mov %rax,jit_frame.tf_rsp(%r15)

 //push internal call
 lea  -8(%rsp),%rsp

 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 //set
 movqq jit_frame.tf_rax(%r15),%rax
 //%r14 ABI preserve the registers
 //%r15 ABI preserve the registers

 //%rsp???
 //%rbp???

 call %gs:teb.jitcall

 //restore guard
 movqq %gs:teb.thread          ,%r15 //curkthread
 movqq kthread.td_jit_ctx(%r15),%r15 //jit_frame

 movqq %rax,jit_frame.tf_rax(%r15)
 //%r14 ABI preserve the registers
 //%r15 ABI preserve the registers

 //%rsp???
 //%rbp???

 //epilog
 pop  %rbp
end;

procedure jit_jmp_internal; assembler; nostackframe;
asm
 //set
 movqq jit_frame.tf_rax(%r15),%rax
 //%rsp???
 //%rbp???
 movqq jit_frame.tf_rax(%r14),%r14
 movqq jit_frame.tf_rax(%r15),%r15

 jmp %gs:teb.jitcall
end;

function jmp_dispatcher(addr:Pointer;is_call:Boolean):Pointer;
label
 _start;
var
 td:p_kthread;
 node:t_jit_dynamic.p_entry_point;
 jctx:p_jctx;
begin
 td:=curkthread;
 if (td=nil) then Exit(nil);

 //jit_state:=((td^.pcb_flags and PCB_IS_JIT)<>0);

 if not is_guest_addr(QWORD(addr)) then
 begin
  //switch to internal

  if is_call then
  begin
   td^.td_teb^.jitcall:=addr;
   Exit(@jit_call_internal);
  end else
  begin
   td^.td_teb^.jitcall:=addr;
   Exit(@jit_jmp_internal);
  end;

 end;

 _start:

 if ((pmap_get_raw(QWORD(addr)) and PAGE_PROT_EXECUTE)=0) then
 begin
  writeln('not excec:0x',HexStr(addr));
  Assert(False,'TODO');
 end;

 node:=fetch_entry(addr);

 if (node=nil) then
 begin
  preload(addr);
  goto _start;
 end;

 jctx:=td^.td_jit_ctx;

 if (jctx^.cblob<>nil) then
 begin
  jctx^.cblob^.dec_ref;
  jctx^.cblob:=nil;
 end;

 jctx^.cblob:=node^.blob;

 Result:=node^.dst;
end;

function t_jit_dynamic.t_jcode_chunk.c(n1,n2:p_jcode_chunk):Integer;
begin
 Result:=Integer(n1^.start>n2^.start)-Integer(n1^.start<n2^.start);
 if (Result<>0) then Exit;
 Result:=Integer(n1^.hash>n2^.hash)-Integer(n1^.hash<n2^.hash);
end;

procedure build(var ctx:t_jit_context2);
var
 addr:Pointer;

 blob:p_jit_dynamic;
 entry_point:t_jit_context2.p_entry_point;

 chunk:p_jit_code_chunk;

 start:QWORD;
 __end:QWORD;
 hash :QWORD;
 count:QWORD;

 original:QWORD;
 recompil:QWORD;

 jcode:t_jit_dynamic.p_jcode_chunk;
 table:t_jit_dynamic.p_instr_len;

 clabel:t_jit_context2.p_label;

 link_prev:t_jit_i_link;
 link_curr:t_jit_i_link;
 link_next:t_jit_i_link;

 prev:Pointer;
 curr:Pointer;
 next:Pointer;

 F:THandle;
begin
 if (ctx.builder.GetMemSize=0) then Exit;

 blob:=new_blob(ctx.builder.GetMemSize);

 ctx.builder.SaveTo(blob^.base,ctx.builder.GetMemSize);

 Writeln('build:0x',HexStr(ctx.text_start,16),'->0x',HexStr(blob^.base));

 //F:=FileCreate('recompile.bin');
 //FileWrite(F,blob^.base^,ctx.builder.GetMemSize);
 //FileClose(F);

 //copy entrys
 entry_point:=ctx.entry_list;
 while (entry_point<>nil) do
 begin
  addr:=blob^.base+entry_point^.label_id.offset;
  //
  blob^.add_entry_point(entry_point^.src,addr);
  //
  entry_point:=entry_point^.next;
 end;

 start:=0;
 __end:=0;
 hash :=0;
 count:=0;

 jcode:=nil;
 table:=nil;

 //copy chunks
 chunk:=TAILQ_FIRST(@ctx.builder.ACodeChunkList);

 while (chunk<>nil) do
 begin
  if (__end=chunk^.start) then
  begin
   //expand
   __end:=chunk^.__end;
  end else
  begin
   //save
   if (start<>0) then
   begin
    hash:=MurmurHash64A(Pointer(start),__end-start,$010CA1C0DE);

    count:=0;
    curr:=Pointer(start);

    //get count
    while (QWORD(curr)<__end) do
    begin
     clabel:=ctx.get_label(curr);

     if (clabel=nil) then
     begin
      Writeln('(clabel=nil) 0x',HexStr(curr));
      Assert(false);
     end;

     Inc(count);
     curr:=clabel^.next;
    end;

    clabel:=ctx.get_label(Pointer(start));

    jcode:=blob^.new_chunk(count);

    jcode^.start:=start;
    jcode^.__end:=__end;
    jcode^.dest :=QWORD(blob^.base)+clabel^.link_curr.offset;
    jcode^.hash :=hash ;

    table:=@jcode^.table;

    count:=0;
    curr:=Pointer(start);

    prev:=nil;
    link_prev:=nil_link;

    //get table
    while (QWORD(curr)<__end) do
    begin
     clabel:=ctx.get_label(curr);

     next:=clabel^.next;

     link_curr:=clabel^.link_curr;
     link_next:=clabel^.link_next;

     if (link_prev<>nil_link) then
     begin
      if (link_prev.offset<>link_curr.offset) then
      begin
       Writeln(':',HexStr(curr),'..',HexStr(next),' prev:',HexStr(prev));
       Writeln('table:',HexStr(link_prev.offset,8),'<>',HexStr(link_next.offset,8));

       print_disassemble(blob^.base+link_prev.offset,link_next.offset-link_prev.offset);

       Assert(False);
      end;
     end;

     original:=QWORD(next)-QWORD(curr);
     recompil:=link_next.offset-link_curr.offset;

     if (original>255) or (recompil>255) then
     begin
      Writeln('0x',HexStr(curr));
      Writeln(original,':',recompil);
      Assert(False);
     end;

     table[count].original:=Byte(original);
     table[count].recompil:=Byte(recompil);

     {
     writeln('|0x',HexStr(curr),'..',HexStr(next),
             ':0x',HexStr(link_curr.offset,8),'..',HexStr(link_next.offset,8),
             ':',count);
     }

     prev:=curr;
     link_prev:=link_next;

     Inc(count);
     curr:=next;
    end;

    //writeln('[0x',HexStr(start,16),':0x',HexStr(__end,16),':',count);

    jcode:=nil;
    table:=nil;
   end;
   //new
   start:=chunk^.start;
   __end:=chunk^.__end;
  end;

  //
  chunk:=TAILQ_NEXT(chunk,@chunk^.link);
 end;

 blob^.attach;
end;

function fetch_entry(src:Pointer):t_jit_dynamic.p_entry_point;
var
 data:PPointer;
begin
 Result:=nil;
 rw_rlock(entry_hamt_lock);

 data:=HAMT_search64(@entry_hamt,QWORD(src));
 if (data<>nil) then
 begin
  Result:=data^;
 end;

 if (Result<>nil) then
 begin
  Result^.inc_ref;
 end;

 rw_runlock(entry_hamt_lock);
end;

function exist_entry(src:Pointer):Boolean;
var
 entry:t_jit_dynamic.p_entry_point;
begin
 entry:=fetch_entry(src);
 if (entry<>nil) then
 begin
  entry^.dec_ref;
  Result:=True;
 end else
 begin
  Result:=False;
 end;
end;

//

function new_blob(_size:ptruint):p_jit_dynamic;
begin
 Result:=AllocMem(SizeOf(t_jit_dynamic));
 Result^.alloc_base(_size);
end;

//

procedure t_jit_dynamic.t_entry_point.inc_ref;
begin
 blob^.inc_ref;
end;

procedure t_jit_dynamic.t_entry_point.dec_ref;
begin
 blob^.dec_ref;
end;

//

procedure t_jit_dynamic.t_jcode_chunk.inc_ref;
begin
 blob^.inc_ref;
end;

procedure t_jit_dynamic.t_jcode_chunk.dec_ref;
begin
 blob^.dec_ref;
end;

//

procedure t_jit_dynamic.inc_ref;
begin
 System.InterlockedIncrement(refs);
end;

procedure t_jit_dynamic.dec_ref;
begin
 if (System.InterlockedDecrement(refs)=0) then
 begin
  Free;
 end;
end;

//

procedure t_jit_dynamic.Free;
var
 node,next:p_entry_point;
begin
 node:=entry_list;
 while (node<>nil) do
 begin
  next:=node^.next;
  FreeMem(node);
  node:=next;
 end;

 free_base;

 FreeMem(@Self);
end;

function t_jit_dynamic.add_entry_point(src,dst:Pointer):p_entry_point;
begin
 if (src=nil) or (dst=nil) then Exit;
 Result:=AllocMem(Sizeof(t_entry_point));
 Result^.next:=entry_list;
 Result^.blob:=@Self;
 Result^.src :=src;
 Result^.dst :=dst;
 //
 entry_list:=Result;
end;

function t_jit_dynamic.new_chunk(count:QWORD):p_jcode_chunk;
begin
 Result:=AllocMem(SizeOf(t_jcode_chunk)+SizeOf(t_instr_len)*count);
 Result^.count:=count;
 Result^.blob :=@Self;
 //
 Result^.next:=chunk_list;
 chunk_list:=Result;
end;

procedure t_jit_dynamic.alloc_base(_size:ptruint);
begin
 base:=md_mmap(nil,_size,MD_PROT_RWX);
 size:=_size;
end;

procedure t_jit_dynamic.free_base;
begin
 md_unmap(base,size);
 base:=nil;
 size:=0;
end;

//

function t_jit_dynamic.t_jcode_chunk.find_addr(addr:QWORD):QWORD;
var
 i,src,dst:QWORD;
 _table:p_instr_len;
begin
 Result:=0;
 if (addr>=start) and (addr<__end) then
 if (count<>0) then
 begin
  src:=start;
  dst:=dest;
  _table:=@table;
  For i:=0 to count-1 do
  begin

   if (src=addr) then
   begin
    Exit(dst);
   end else
   if (src>addr) then
   begin
    Exit(0);
   end;

   src:=src+_table[i].original;
   dst:=dst+_table[i].recompil;
  end;
 end;
end;

//

procedure t_jit_dynamic.attach_entry(node:p_entry_point);
begin
 rw_wlock(entry_hamt_lock);

 node^.inc_ref;

 HAMT_insert64(@entry_hamt,QWORD(node^.src),node);

 rw_wunlock(entry_hamt_lock);
end;

procedure t_jit_dynamic.attach_entry;
var
 node,next:p_entry_point;
begin
 rw_wlock(entry_hamt_lock);

 node:=entry_list;
 while (node<>nil) do
 begin
  next:=node^.next;

  node^.inc_ref;

  HAMT_insert64(@entry_hamt,QWORD(node^.src),node);

  node:=next;
 end;

 rw_wunlock(entry_hamt_lock);
end;

procedure t_jit_dynamic.attach_chunk;
var
 node,next:p_jcode_chunk;
begin
 rw_wlock(entry_chunk_lock);

 node:=chunk_list;
 while (node<>nil) do
 begin
  next:=node^.next;

  node^.inc_ref;

  entry_chunk.Insert(node);

  node:=next;
 end;

 rw_wunlock(entry_chunk_lock);
end;

procedure t_jit_dynamic.attach;
begin
 attach_entry;
 attach_chunk;
end;

procedure t_jit_dynamic.detach_entry;
var
 node,next:p_entry_point;
begin
 rw_wlock(entry_hamt_lock);

 node:=entry_list;
 while (node<>nil) do
 begin
  next:=node^.next;

  HAMT_delete64(@entry_hamt,QWORD(node^.src),nil);

  node^.dec_ref;

  node:=next;
 end;

 rw_wunlock(entry_hamt_lock);
end;

procedure t_jit_dynamic.detach_chunk;
var
 node,next:p_jcode_chunk;
begin
 rw_wlock(entry_chunk_lock);

 node:=chunk_list;
 while (node<>nil) do
 begin
  next:=node^.next;

  entry_chunk.Delete(node);

  node^.dec_ref;

  node:=next;
 end;

 rw_wunlock(entry_chunk_lock);
end;

procedure t_jit_dynamic.detach;
begin
 inc_ref;
 detach_entry;
 detach_chunk;
 dec_ref
end;

end.






