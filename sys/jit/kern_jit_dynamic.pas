unit kern_jit_dynamic;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 hamt,
 //g23tree,
 g_node_splay,
 murmurhash,
 x86_jit,
 kern_jit_ctx,
 kern_jit_asm,
 kern_rwlock,
 kern_thr,
 kern_stub;

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
 p_jit_dynamic_blob=^t_jit_dynamic_blob;

 p_jit_entry_point=^t_jit_entry_point;
 t_jit_entry_point=object
  next:p_jit_entry_point;
  blob:p_jit_dynamic_blob;
  src :Pointer; //<-guest
  dst :Pointer; //<-host
  procedure inc_ref(name:pchar);
  procedure dec_ref(name:pchar);
 end;

 p_jinstr_len=^t_jinstr_len;
 t_jinstr_len=packed record
  original:0..31; //5
  LF_JMP  :0..1;
  bit6    :0..1;
  bit7    :0..1;
  recompil:Byte;
 end;

 p_jcode_chunk=^t_jcode_chunk;
 t_jcode_chunk=object
  entry:TAILQ_ENTRY;    //entry_chunk_list

  next  :p_jcode_chunk; //t_jit_dynamic_blob.chunk_list
  //pLeft :p_jcode_chunk;
  //pRight:p_jcode_chunk;
  blob  :p_jit_dynamic_blob;
  tobj  :Pointer; //p_vm_track_object
  start :QWORD;   //<-guest
  __end :QWORD;   //<-guest
  dest  :QWORD;   //<-host
  d_end :QWORD;   //<-host
  hash  :QWORD;   //MurmurHash64A(Pointer(start),__end-start,$010CA1C0DE);
  count :QWORD;
  table :record end; //p_jinstr_len[]
  function  c(n1,n2:p_jcode_chunk):Integer; static;
  procedure inc_ref(name:pchar);
  procedure dec_ref(name:pchar);
  function  is_mark_del:Boolean;
  function  find_host_by_guest(addr:QWORD):QWORD;
  function  find_guest_by_host(addr:QWORD):QWORD;
  function  cross_guest(c_start,c___end:QWORD):Boolean;
  function  cross_host (c_start,c___end:QWORD):Boolean;
 end;

 //t_jcode_chunk_set=specialize T23treeSet<p_jcode_chunk,t_jcode_chunk>;

 p_jplt_cache=^t_jplt_cache;
 t_jplt_cache=object(t_jplt_cache_asm)
  pLeft :p_jplt_cache; //jpltc_curr
  pRight:p_jplt_cache; //jpltc_curr
  //
  entry:TAILQ_ENTRY;   //jpltc_attc
  function c(n1,n2:p_jplt_cache):Integer; static;
 end;

 t_jplt_cache_set=specialize TNodeSplay<t_jplt_cache>;

 t_jit_dynamic_blob=object
  var
   entry_list:p_jit_entry_point;
   chunk_list:p_jcode_chunk;

   jpltc_curr:t_jplt_cache_set;
   jpltc_attc:TAILQ_HEAD;

   mchunk:p_stub_chunk;

   base:Pointer;
   size:ptruint;

   plta:p_jit_plt;
   pltc:ptruint;

   plt_stub:t_jplt_cache_asm;

   lock:Pointer;
   refs:Integer;

   attach_count:Integer;

  procedure inc_ref(name:pchar);
  procedure dec_ref(name:pchar);
  procedure inc_attach_count;
  function  dec_attach_count:Boolean;
  function  find_guest_by_host(addr:QWORD):QWORD;
  function  cross_host(c_start,c___end:QWORD):Boolean;
  procedure Free;
  function  add_entry_point(src,dst:Pointer):p_jit_entry_point;
  procedure free_entry_point(node:p_jit_entry_point);
  procedure init_plt;
  procedure attach_plt_cache(node:p_jplt_cache);
  procedure detach_plt_cache(node:p_jplt_cache);
  procedure detach_all_attc;
  procedure detach_all_curr;
  function  add_plt_cache(plt:p_jit_plt;src,dst:Pointer;blk:p_jit_dynamic_blob):p_jplt_cache;
  function  new_chunk(count:QWORD):p_jcode_chunk;
  procedure alloc_base(_size:ptruint);
  procedure free_base;
  procedure attach_entry(node:p_jit_entry_point);
  procedure attach_entry;
  procedure attach_chunk;
  procedure attach;
  function  detach_entry(node:p_jit_entry_point):Boolean;
  procedure detach_all_entry;
  procedure detach_entry(c_start,c___end:QWORD);
  procedure detach_chunk(node:p_jcode_chunk);
  procedure detach_all_chunk;
  procedure detach;
 end;

function new_blob(_size:ptruint):p_jit_dynamic_blob;

var
 entry_hamt_lock:Pointer=nil;
 entry_hamt:TSTUB_HAMT64;

 entry_chunk_lock:Pointer=nil;

 entry_chunk_list:TAILQ_HEAD=(tqh_first:nil;tqh_last:@entry_chunk_list.tqh_first);

 //entry_chunk:t_jcode_chunk_set;

function  fetch_entry(src:Pointer):p_jit_entry_point;
function  exist_entry(src:Pointer):Boolean;

function  fetch_chunk_by_guest(src:Pointer):p_jcode_chunk;
function  fetch_blob_by_host(src:Pointer):p_jit_dynamic_blob;
function  exist_jit_host(src:Pointer;tf_tip:PQWORD):Boolean;

function  next_chunk(node:p_jcode_chunk;src:Pointer):p_jcode_chunk;
//procedure unmap_jit_cache(start,__end:QWORD);
function  preload_entry(addr:Pointer):p_jit_entry_point;

procedure jit_ctx_free(td:p_kthread);
procedure switch_to_jit(td:p_kthread);
function  jmp_dispatcher(addr:Pointer;plt:p_jit_plt;from:Pointer):Pointer;

procedure blob_track(blob:p_jit_dynamic_blob);

procedure build(var ctx:t_jit_context2);

procedure preload(addr:Pointer);

implementation

uses
 sysutils,
 vmparam,
 sys_bootparam,
 kern_proc,
 vm,
 vm_map,
 vm_pmap_prot,
 vm_pmap,
 md_map,
 vm_tracking_map,
 kern_dlsym;

//

procedure pick(var ctx:t_jit_context2;preload:Pointer); external name 'kern_jit_pick';

//

function scan_up_exc(addr:QWORD):QWORD;
begin
 addr:=(addr+PAGE_MASK) and (not PAGE_MASK);

 while is_guest_addr(addr) do
 begin
  Result:=addr;

  if ((ppmap_get_prot(QWORD(addr)) and PAGE_PROT_EXECUTE)=0) then
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

  if ((ppmap_get_prot(QWORD(addr)) and PAGE_PROT_EXECUTE)=0) then
  begin
   Break;
  end;

  addr:=addr-PAGE_SIZE;
 end;

end;

procedure preload(addr:Pointer);
var
 node:p_jit_entry_point;
 ctx:t_jit_context2;
begin
 if (p_print_jit_preload) then
 begin
  Writeln('unk addr:0x',HexStr(addr));
 end;

 node:=preload_entry(addr);

 if (node=nil) then
 begin
  ctx:=Default(t_jit_context2);

  ctx.text_start:=scan_dw_exc(QWORD(addr));
  ctx.text___end:=scan_up_exc(QWORD(addr));
  ctx.map____end:=ctx.text___end;

  ctx.modes     :=[cmDontScanRipRel]; //dont scan rip relative

  ctx.add_forward_point(fpCall,addr);

  pick(ctx,addr);
 end else
 begin
  node^.dec_ref('preload_entry');
 end;
end;

procedure jit_ctx_free(td:p_kthread); public;
begin
 td^.td_jctx.block:=nil;
end;

procedure switch_to_jit(td:p_kthread); public;
label
 _start;
var
 node:p_jit_entry_point;
 jctx:p_td_jctx;
 frame:p_jit_frame;
begin
 if (td=nil) then Exit;

 if is_guest_addr(td^.td_frame.tf_rip) then
 begin
  //host->jit
  //jit->jit
 end else
 begin
  if ((td^.pcb_flags and PCB_IS_JIT)<>0) then
  begin
   //jit->host

   if ((td^.td_pflags and TDP_KTHREAD)<>0) then
   begin
    //clear jit flag
    td^.pcb_flags:=td^.pcb_flags and (not PCB_IS_JIT);

    Exit; //internal?
   end else
   begin
    //forbidden

    Assert(false,'forbidden jump to 0x'+HexStr(td^.td_frame.tf_rip,16));
   end;

  end else
  begin
   //host->host

   Exit; //internal?
  end;
 end;

 _start:

 node:=fetch_entry(Pointer(td^.td_frame.tf_rip));

 if (node=nil) then
 begin
  preload(Pointer(td^.td_frame.tf_rip));
  goto _start;
 end;

 jctx:=@td^.td_jctx;

 frame:=@td^.td_frame.tf_r13;

 jctx^.block:=node^.blob;

 if (jctx^.rsp=nil) then
 begin
  jctx^.rsp:=td^.td_kstack.stack;
 end;

 if (jctx^.rbp=nil) then
 begin
  jctx^.rbp:=td^.td_kstack.stack;
 end;

 //tf_r14 not need to move
 //tf_r15 not need to move

 frame^.tf_r13:=td^.td_frame.tf_r13;
 frame^.tf_rsp:=td^.td_frame.tf_rsp;
 frame^.tf_rbp:=td^.td_frame.tf_rbp;

 td^.td_frame.tf_rsp:=QWORD(td^.td_kstack.stack);
 td^.td_frame.tf_rbp:=QWORD(td^.td_kstack.stack);

 td^.td_frame.tf_rip:=QWORD(node^.dst);
 td^.td_frame.tf_r13:=QWORD(frame);

 set_pcb_flags(td,PCB_FULL_IRET or PCB_IS_JIT);

 if (td^.td_teb^.jit_trp=nil) then
 begin
  td^.td_teb^.jit_trp:=@jit_interrupt_nop;
 end;

 //teb stack
 td^.td_teb^.sttop:=td^.td_kstack.sttop;
 td^.td_teb^.stack:=td^.td_kstack.stack;
 //teb stack

 //
 node^.dec_ref('fetch_entry')
end;

function fetch_chunk_by_guest(src:Pointer):p_jcode_chunk;
var
 //i:t_jcode_chunk_set.Iterator;
 //node:t_jcode_chunk;

 obj:p_vm_track_object;
begin
 Result:=nil;

 obj:=vm_map_track_next(p_proc.p_vmspace,
                        vm_offset_t(src),
                        nil,
                        H_JIT_CHUNK);

 if (obj<>nil) then
 begin
  Result:=obj^.handle;

  if (Result<>nil) then
  begin
   Result^.inc_ref('fetch_chunk_by_guest');
  end;

  vm_track_object_deallocate(obj); //<-vm_track_map_next_object
 end;

 {
 node:=Default(t_jcode_chunk);
 node.start:=QWORD(src);

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
 }
end;

function next_chunk(node:p_jcode_chunk;src:Pointer):p_jcode_chunk;
var
 //i:t_jcode_chunk_set.Iterator;

 obj:p_vm_track_object;
begin
 Result:=nil;
 //

 obj:=vm_map_track_next(p_proc.p_vmspace,
                        vm_offset_t(src),
                        node^.tobj,
                        H_JIT_CHUNK);

 if (obj<>nil) then
 begin
  Result:=obj^.handle;

  if (Result<>nil) then
  begin
   Result^.inc_ref('next_chunk');
  end;

  vm_track_object_deallocate(obj); //<-vm_track_map_next_object
 end;

 {
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
 }
end;

function fetch_blob_by_host(src:Pointer):p_jit_dynamic_blob;
var
 //i:t_jcode_chunk_set.Iterator;
 node:p_jcode_chunk;
 prev:p_jit_dynamic_blob;
 blob:p_jit_dynamic_blob;
begin
 Result:=nil;

 prev:=nil;

 //
 rw_rlock(entry_chunk_lock);

 node:=TAILQ_FIRST(@entry_chunk_list);

 //i:=entry_chunk.cbegin;

 //while (i.Item<>nil) do

 while (node<>nil) do
 begin

  //node:=i.Item^;
  //if (node<>nil) then

  if not node^.is_mark_del then
  begin
   blob:=node^.blob;
   if (blob<>nil) and (blob<>prev) then
   begin
    if blob^.cross_host(QWORD(src),QWORD(src)+1) then
    begin
     blob^.inc_ref('fetch_blob_by_host');

     rw_runlock(entry_chunk_lock);

     Exit(blob);
    end;
    prev:=blob;
   end;
  end;

  //i.Next;

  node:=TAILQ_NEXT(node,@node^.entry);
 end;

 rw_runlock(entry_chunk_lock);
end;

function exist_jit_host(src:Pointer;tf_tip:PQWORD):Boolean; public;
var
 blob:p_jit_dynamic_blob;
begin
 blob:=fetch_blob_by_host(src);
 if (blob<>nil) then
 begin
  if (tf_tip<>nil) then
  begin
   rw_rlock(blob^.lock);
   tf_tip^:=blob^.find_guest_by_host(QWORD(src));
   rw_runlock(blob^.lock);
  end;
  blob^.dec_ref('fetch_blob_by_host');
  Result:=True;
 end else
 begin
  Result:=False;
 end;
end;

{
procedure unmap_jit_cache(start,__end:QWORD); [public, alias:'kern_unmap_jit_cache'];
var
 curr,next:p_jcode_chunk;
 blob:p_jit_dynamic_blob;
begin
 curr:=fetch_chunk_by_guest(Pointer(start));
 while (curr<>nil) do
 begin

  if (curr^.start>=__end) then Break;

  if curr^.cross_guest(start,__end) then
  begin
   blob:=curr^.blob;
   rw_wlock(blob^.lock);

    blob^.detach_chunk(curr);
    blob^.detach_entry(start,__end);

   rw_wunlock(blob^.lock);
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
}

function preload_entry(addr:Pointer):p_jit_entry_point;
var
 curr,next:p_jcode_chunk;
 blob:p_jit_dynamic_blob;
 dest:QWORD;
begin
 Result:=nil;

 curr:=fetch_chunk_by_guest(addr);
 while (curr<>nil) do
 begin

  //Writeln(HexStr(addr),':',HexStr(curr^.start,16),'..',HexStr(curr^.__end,16));

  //if (QWORD(addr)<curr^.start) then Break;

  dest:=curr^.find_host_by_guest(QWORD(addr));

  if (dest<>0) then
  begin
   if (p_print_jit_preload) then
   begin
    Writeln('cache:',HexStr(addr),'->',HexStr(dest,16));
   end;

   blob:=curr^.blob;

   rw_wlock(blob^.lock);
    if not curr^.is_mark_del then
    begin
     Result:=blob^.add_entry_point(addr,Pointer(dest));
    end;
   rw_wunlock(blob^.lock);

   blob^.attach_entry(Result);

   Exit;
  end;

  next:=next_chunk(curr,addr);
  curr^.dec_ref('preload_entry');
  curr:=next;
 end;

 if (curr<>nil) then
 begin
  curr^.dec_ref('preload_entry');
 end;
end;

function jmp_dispatcher(addr:Pointer;plt:p_jit_plt;from:Pointer):Pointer; public;
label
 _start;
var
 td:p_kthread;
 node:p_jit_entry_point;
 jctx:p_td_jctx;
 curr:p_jit_dynamic_blob;
 cache:p_jplt_cache;
begin
 td:=curkthread;
 if (td=nil) then Exit(nil);

 //jit_state:=((td^.pcb_flags and PCB_IS_JIT)<>0);

 if not is_guest_addr(QWORD(addr)) then
 begin
  //switch to internal

  if ((QWORD(addr) shr 32)=$EFFFFFFE) then
  begin
   if exist_jit_host(from,@td^.td_frame.tf_rip) then
   begin
    test_unresolve_symbol(td,addr);
   end;
  end;

  writeln('not guest addr:0x',HexStr(addr));
  Assert(False,'TODO');

  //td^.td_teb^.jitcall:=addr;
  //Exit(@jit_jmp_internal);
 end;

 _start:

 if ((ppmap_get_prot(QWORD(addr)) and PAGE_PROT_EXECUTE)=0) then
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

 jctx:=@td^.td_jctx;

 curr:=jctx^.block;

 if (curr=nil) or (plt=nil) then
 begin
  jctx^.block:=node^.blob;
 end else
 begin
  cache:=curr^.add_plt_cache(plt,node^.src,node^.dst,node^.blob);

  jctx^.block:=node^.blob;

  //one element plt cache
  System.InterlockedExchange(plt^.cache,cache);
 end;

 Result:=node^.dst;

 //
 node^.dec_ref('fetch_entry')
end;

function t_jcode_chunk.c(n1,n2:p_jcode_chunk):Integer;
begin
 Result:=Integer(n1^.start>n2^.start)-Integer(n1^.start<n2^.start);
 if (Result<>0) then Exit;
 Result:=Integer(n1^.hash>n2^.hash)-Integer(n1^.hash<n2^.hash);
end;

function t_jplt_cache.c(n1,n2:p_jplt_cache):Integer;
begin
 Result:=Integer(n1^.plt>n2^.plt)-Integer(n1^.plt<n2^.plt);
 if (Result<>0) then Exit;
 Result:=Integer(n1^.src>n2^.src)-Integer(n1^.src<n2^.src);
end;

procedure build_chunk(var ctx:t_jit_context2;blob:p_jit_dynamic_blob;start,__end,count:QWORD);
var
 hash :QWORD;

 i       :QWORD;
 original:QWORD;
 recompil:QWORD;

 jcode:p_jcode_chunk;
 table:p_jinstr_len;

 clabel:t_jit_context2.p_label;

 link_prev:t_jit_i_link;
 link_curr:t_jit_i_link;
 link_next:t_jit_i_link;

 prev:Pointer;
 curr:Pointer;
 next:Pointer;
begin
 jcode:=nil;
 table:=nil;

 if (count=0) then Exit;

 hash:=MurmurHash64A(uplift(Pointer(start)),__end-start,$010CA1C0DE);

 clabel:=ctx.get_label(Pointer(start));

 jcode:=blob^.new_chunk(count);

 jcode^.start:=start;
 jcode^.__end:=__end;
 jcode^.dest :=QWORD(blob^.base)+clabel^.link_curr.offset;
 jcode^.hash :=hash ;

 table:=@jcode^.table;

 i:=0;
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
    Writeln('oaddr:',HexStr(curr),'..',HexStr(next),' prev:',HexStr(prev));
    Writeln('table:',HexStr(blob^.base+link_prev.offset),'<>',HexStr(blob^.base+link_curr.offset));

    print_disassemble(blob^.base+link_prev.offset,link_next.offset-link_prev.offset);

    Assert(False);
   end;
  end;

  original:=QWORD(next)-QWORD(curr);
  recompil:=link_next.offset-link_curr.offset;

  if (original>16) or (recompil>255) then
  begin
   Writeln('0x',HexStr(curr));
   Writeln(original,':',recompil);
   Assert(False);
  end;

  table[i].original:=Byte(original);
  table[i].recompil:=Byte(recompil);

  table[i].LF_JMP  :=ord((clabel^.flags and LF_JMP)<>0);

  {
  writeln('|0x',HexStr(curr),'..',HexStr(next),
          ':0x',HexStr(link_curr.offset,8),'..',HexStr(link_next.offset,8),
          ':',i);
  }

  prev:=curr;
  link_prev:=link_next;

  Inc(i);
  curr:=next;
 end;

 //get last addr
 //jcode^.d_end:=QWORD(blob^.base)+clabel^.link_curr.offset;

 recompil:=0;
 for i:=0 to count-1 do
 begin
  recompil:=recompil+table[i].recompil;
 end;
 jcode^.d_end:=QWORD(jcode^.dest)+recompil;

 if (p_print_jit_preload) then
 begin
  Writeln('build_chunk:0x',HexStr(jcode^.dest,16),'..',HexStr(jcode^.d_end,16),':',count);
 end;

 //writeln('[0x',HexStr(start,16),':0x',HexStr(__end,16),':',count);
end;

procedure build_blob(var ctx:t_jit_context2;
                     blob:p_jit_dynamic_blob;
                     start,__end:QWORD);
var
 count:QWORD;

 clabel:t_jit_context2.p_label;

 link_prev:t_jit_i_link;
 link_curr:t_jit_i_link;
 link_next:t_jit_i_link;

 prev:QWORD;
 curr:QWORD;
 next:QWORD;
begin
 count:=0;
 curr:=start;

 prev:=0;
 link_prev:=nil_link;

 //get count
 while (QWORD(curr)<__end) do
 begin
  clabel:=ctx.get_label(Pointer(curr));

  if (clabel=nil) then
  begin
   Writeln('(clabel=nil) 0x',HexStr(curr,16));
   Assert(false);
  end;

  //Writeln('clabel:0x',HexStr(QWORD(blob^.base)+clabel^.link_curr.offset,16));

  next:=QWORD(clabel^.next);

  link_curr:=clabel^.link_curr;
  link_next:=clabel^.link_next;

  if (link_prev<>nil_link) then
  begin
   if (link_prev.offset<>link_curr.offset) then
   begin
    //devide chunk

    build_chunk(ctx,blob,start,curr,count);

    start:=curr;
    count:=0;
   end;
  end;

  prev:=curr;
  link_prev:=link_next;

  Inc(count);
  curr:=next;
 end;

 build_chunk(ctx,blob,start,__end,count);
end;

procedure build(var ctx:t_jit_context2);
var
 addr:Pointer;

 blob:p_jit_dynamic_blob;
 entry_point:t_jit_context2.p_entry_point;

 chunk:p_jit_code_chunk;

 start:QWORD;
 __end:QWORD;

 //F:THandle;

begin
 if (ctx.builder.GetMemSize=0) then Exit;

 blob:=new_blob(ctx.builder.GetMemSize);

 ctx.builder.SaveTo(blob^.base,ctx.builder.GetMemSize);

 blob^.plta:=blob^.base+ctx.builder.GetPltStart;
 blob^.pltc:=ctx.builder.APltCount;

 blob^.init_plt;

 if (p_print_jit_preload) then
 begin
  Writeln('build:0x',HexStr(ctx.text_start,16),'->0x',HexStr(blob^.base),'..',HexStr(blob^.base+blob^.size));
 end;

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

 //copy chunks
 chunk:=TAILQ_FIRST(@ctx.builder.ACodeChunkList);

 while (chunk<>nil) do
 begin
  if (chunk^.start=chunk^.__end) then
  begin
   //no instr
  end else
  begin
   //
   {if (t_point_type(chunk^.data)=fpInvalid) then
   begin
    //skip
   end else }
   if (__end=chunk^.start) then
   begin
    //expand
    __end:=chunk^.__end;
   end else
   begin
    //save
    if (start<>0) then
    begin
     //build prev saved
     build_blob(ctx,blob,start,__end);
    end;
    //new
    start:=chunk^.start;
    __end:=chunk^.__end;
   end;
  end;
  //
  chunk:=TAILQ_NEXT(chunk,@chunk^.entry);
 end;

 if (start<>0) then
 begin
  //build prev saved
  build_blob(ctx,blob,start,__end);
 end;

 blob^.attach;
end;

function fetch_entry(src:Pointer):p_jit_entry_point;
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
  Result^.inc_ref('fetch_entry');
 end;

 rw_runlock(entry_hamt_lock);
end;

function exist_entry(src:Pointer):Boolean;
var
 entry:p_jit_entry_point;
begin
 entry:=fetch_entry(src);
 if (entry<>nil) then
 begin
  entry^.dec_ref('fetch_entry');
  Result:=True;
 end else
 begin
  Result:=False;
 end;
end;

//

function new_blob(_size:ptruint):p_jit_dynamic_blob;
begin
 Result:=AllocMem(SizeOf(t_jit_dynamic_blob));
 Result^.alloc_base(_size);

 TAILQ_INIT(@Result^.jpltc_attc);
end;

//

procedure t_jit_entry_point.inc_ref(name:pchar);
begin
 blob^.inc_ref(name);
end;

procedure t_jit_entry_point.dec_ref(name:pchar);
begin
 blob^.dec_ref(name);
end;


//

procedure t_jcode_chunk.inc_ref(name:pchar);
begin
 blob^.inc_ref(name);
end;

procedure t_jcode_chunk.dec_ref(name:pchar);
begin
 blob^.dec_ref(name);
end;

function t_jcode_chunk.is_mark_del:Boolean;
begin
 Result:=(tobj=nil);
end;

//

procedure t_jit_dynamic_blob.inc_ref(name:pchar);
begin
 //Writeln('inc_ref:0x',HexStr(@self),' ',name);

 System.InterlockedIncrement(refs);
end;

procedure t_jit_dynamic_blob.dec_ref(name:pchar);
begin
 //Writeln('dec_ref:0x',HexStr(@self),' ',name);

 if (System.InterlockedDecrement(refs)=0) then
 begin
  Free;
 end;
end;

procedure t_jit_dynamic_blob.inc_attach_count;
begin
 System.InterlockedIncrement(attach_count);
end;

function t_jit_dynamic_blob.dec_attach_count:Boolean;
begin
 Result:=(System.InterlockedDecrement(attach_count)=0);
end;

function t_jit_dynamic_blob.find_guest_by_host(addr:QWORD):QWORD;
var
 node:p_jcode_chunk;
begin
 //Writeln('_ind_guest_by_host:0x',HexStr(base),' 0x',HexStr(base+size),' 0x',HexStr(addr,16));

 Result:=0;
 node:=chunk_list;
 while (node<>nil) do
 begin
  Result:=node^.find_guest_by_host(addr);
  if (Result<>0) then Exit;

  node:=node^.next;
 end;
end;

function t_jit_dynamic_blob.cross_host(c_start,c___end:QWORD):Boolean;
begin
 Result:=(c___end>QWORD(base)) and (c_start<(QWORD(base)+size));
end;

//

procedure t_jit_dynamic_blob.Free;
var
 node,next:p_jit_entry_point;
begin
 node:=entry_list;
 while (node<>nil) do
 begin
  next:=node^.next;
  free_entry_point(node);
  node:=next;
 end;

 free_base;

 FreeMem(@Self);
end;

function t_jit_dynamic_blob.add_entry_point(src,dst:Pointer):p_jit_entry_point;
begin
 if (src=nil) or (dst=nil) then Exit;
 Result:=AllocMem(Sizeof(t_jit_entry_point));
 Result^.next:=entry_list;
 Result^.blob:=@Self;
 Result^.src :=src;
 Result^.dst :=dst;
 //
 entry_list:=Result;
end;

procedure t_jit_dynamic_blob.free_entry_point(node:p_jit_entry_point);
begin
 FreeMem(node);
end;

procedure t_jit_dynamic_blob.init_plt;
var
 i:Integer;
begin
 if (pltc<>0) then
 For i:=0 to pltc-1 do
 begin
  plta[i].cache:=@plt_stub;
 end;
end;

procedure t_jit_dynamic_blob.attach_plt_cache(node:p_jplt_cache);
begin
 rw_wlock(lock);

 TAILQ_INSERT_TAIL(@jpltc_attc,node,@node^.entry);

 rw_wunlock(lock);
end;

procedure t_jit_dynamic_blob.detach_plt_cache(node:p_jplt_cache);
begin
 rw_wlock(lock);

 if (node^.entry.tqe_prev<>nil) then
 begin
  TAILQ_REMOVE(@jpltc_attc,node,@node^.entry);

  node^.entry:=Default(TAILQ_ENTRY);
 end;

 rw_wunlock(lock);
end;

procedure _reset_plt(node:p_jplt_cache);
var
 plt:p_jit_plt;
begin
 plt:=node^.plt;
 if (plt<>nil) then
 begin
  //one element plt reset
  System.InterlockedCompareExchange(plt^.cache,nil,node);
 end;
end;

procedure t_jit_dynamic_blob.detach_all_attc;
var
 node,next:p_jplt_cache;
begin
 node:=TAILQ_FIRST(@jpltc_attc);

 while (node<>nil) do
 begin
  next:=TAILQ_NEXT(node,@node^.entry);

  TAILQ_REMOVE(@jpltc_attc,node,@node^.entry);

  node^.entry:=Default(TAILQ_ENTRY);

  _reset_plt(node);

  //force deref
  if (System.InterlockedCompareExchange(node^.blk,nil,@Self)=@Self) then
  begin
   Self.dec_ref('add_plt_cache');
  end;

  node:=next;
 end;
end;

procedure t_jit_dynamic_blob.detach_all_curr;
var
 node:p_jplt_cache;
 blk :p_jit_dynamic_blob;
begin
 node:=jpltc_curr.Min;

 while (node<>nil) do
 begin
  jpltc_curr.Delete(node);

  _reset_plt(node);

  blk:=node^.blk;

  if (blk<>nil) then
  begin
   blk^.detach_plt_cache(node);
   blk^.dec_ref('add_plt_cache');
   node^.blk:=nil;
  end;

  FreeMem(node);

  node:=jpltc_curr.Min;
 end;
end;

function t_jit_dynamic_blob.add_plt_cache(plt:p_jit_plt;src,dst:Pointer;blk:p_jit_dynamic_blob):p_jplt_cache;
var
 node:t_jplt_cache;
 dec_blk:p_jit_dynamic_blob;
 _insert:Boolean;
begin
 Assert(plt<>nil);
 Assert(blk<>nil);

 dec_blk:=nil;

 node.plt:=plt;
 node.src:=src;

 repeat

  rw_wlock(lock);
   Result:=jpltc_curr.Find(@node);
   if (Result<>nil) then
   begin
    //update
    Result^.dst:=dst;
    if (Result^.blk<>blk) then
    begin
     dec_blk:=Result^.blk;

     Result^.blk:=blk;
     //
     blk^.inc_ref('add_plt_cache');
    end;
   end;
  rw_wunlock(lock);

  if (dec_blk<>nil) then
  begin
   //del
   if (Result<>nil) then
   begin
    dec_blk^.detach_plt_cache(Result);
   end;

   dec_blk^.dec_ref('add_plt_cache');
   dec_blk:=nil;
  end;

  if (Result<>nil) then
  begin
   Break;
  end else
  begin
   Result:=AllocMem(Sizeof(t_jplt_cache));
   Result^.plt:=plt;
   Result^.src:=src;
   Result^.dst:=dst;
   Result^.blk:=blk;
   //
   rw_wlock(lock);
    _insert:=jpltc_curr.Insert(Result);
    if _insert then
    begin
     blk^.inc_ref('add_plt_cache');
    end;
   rw_wunlock(lock);
   //
   if _insert then
   begin
    //add

    Break;
   end;
  end;

 until false;

 if (Result<>nil) then
 begin
  blk^.attach_plt_cache(Result);
 end;

end;

function t_jit_dynamic_blob.new_chunk(count:QWORD):p_jcode_chunk;
begin
 Result:=AllocMem(SizeOf(t_jcode_chunk)+SizeOf(t_jinstr_len)*count);
 Result^.count:=count;
 Result^.blob :=@Self;
 //
 Result^.next:=chunk_list;
 chunk_list:=Result;
end;

procedure t_jit_dynamic_blob.alloc_base(_size:ptruint);
begin
 base:=nil;
 size:=_size;

 ///md_mmap(base,size,VM_RWX);

 mchunk:=p_alloc(nil,_size,False);
 base:=@mchunk^.body;
end;

procedure t_jit_dynamic_blob.free_base;
begin
 p_free(mchunk);

 ////md_unmap(base,size);

 base:=nil;
 size:=0;
end;

//

function t_jcode_chunk.find_host_by_guest(addr:QWORD):QWORD;
var
 i,src,dst:QWORD;
 _table:p_jinstr_len;
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

function t_jcode_chunk.find_guest_by_host(addr:QWORD):QWORD;
var
 i,src,dst:QWORD;
 _table:p_jinstr_len;
begin
 Result:=0;
 //Writeln('find_guest_by_host:0x',HexStr(dest,16),' 0x',HexStr(d_end,16),' 0x',HexStr(addr,16));
 if (addr>=dest) and (addr<=d_end) then
 if (count<>0) then
 begin
  src:=start;
  dst:=dest;
  _table:=@table;
  For i:=0 to count-1 do
  begin

   if (addr>=dst) then
   begin
    Result:=src;
   end else
   if (dst>addr) then
   begin
    Exit;
   end;

   src:=src+_table[i].original;
   dst:=dst+_table[i].recompil;
  end;
 end;
end;

function t_jcode_chunk.cross_guest(c_start,c___end:QWORD):Boolean;
begin
 Result:=(c___end>start) and (c_start<__end);
end;

function t_jcode_chunk.cross_host(c_start,c___end:QWORD):Boolean;
begin
 Result:=(c___end>dest) and (c_start<d_end);
end;

//

procedure t_jit_dynamic_blob.attach_entry(node:p_jit_entry_point);
begin
 node^.inc_ref('attach_entry');
 self.inc_attach_count;

 rw_wlock(entry_hamt_lock);
  HAMT_insert64(@entry_hamt,QWORD(node^.src),node);
 rw_wunlock(entry_hamt_lock);
end;

procedure t_jit_dynamic_blob.attach_entry;
var
 node,next:p_jit_entry_point;
begin
 node:=entry_list;
 while (node<>nil) do
 begin
  next:=node^.next;

  attach_entry(node);

  node:=next;
 end;
end;

procedure t_jit_dynamic_blob.attach_chunk;
//var
// node,next:p_jcode_chunk;
begin
 blob_track(@self);

 {
 node:=chunk_list;
 while (node<>nil) do
 begin
  next:=node^.next;

  node^.inc_ref;

  rw_wlock(entry_chunk_lock);
   entry_chunk.Insert(node);
  rw_wunlock(entry_chunk_lock);

  node:=next;
 end;
 }
end;

procedure t_jit_dynamic_blob.attach;
begin
 attach_entry;
 attach_chunk;
end;

function t_jit_dynamic_blob.detach_entry(node:p_jit_entry_point):Boolean;
begin
 rw_wlock(entry_hamt_lock);
  HAMT_delete64(@entry_hamt,QWORD(node^.src),nil);
 rw_wunlock(entry_hamt_lock);

 Result:=self.dec_attach_count;
 node^.dec_ref('attach_entry');
end;

procedure t_jit_dynamic_blob.detach_all_entry;
var
 node,next:p_jit_entry_point;
begin
 node:=entry_list;
 while (node<>nil) do
 begin
  next:=node^.next;

  detach_entry(node);

  node:=next;
 end;
end;

procedure t_jit_dynamic_blob.detach_entry(c_start,c___end:QWORD);
var
 node,next:p_jit_entry_point;
 s:QWORD;
begin
 node:=entry_list;
 while (node<>nil) do
 begin
  next:=node^.next;

  s:=QWORD(node^.src);
  if (s>=c_start) and (s<c___end) then
  begin
   detach_entry(node);
  end;

  node:=next;
 end;
end;

procedure t_jit_dynamic_blob.detach_chunk(node:p_jcode_chunk);
var
 tobj:p_vm_track_object;
begin
 tobj:=node^.tobj;
 if (tobj<>nil) then
 begin
  node^.tobj:=nil;

  _vm_map_track_delete_deferred(p_proc.p_vmspace,tobj);

  //vm_map_track_remove(p_proc.p_vmspace,tobj);
 end;

 rw_wlock(entry_chunk_lock);

  TAILQ_REMOVE(@entry_chunk_list,node,@node^.entry);

  //entry_chunk.Delete(node);
 rw_wunlock(entry_chunk_lock);

 node^.dec_ref('blob_track');
end;

procedure t_jit_dynamic_blob.detach_all_chunk;
var
 node,next:p_jcode_chunk;
begin
 node:=chunk_list;
 while (node<>nil) do
 begin
  next:=node^.next;

  detach_chunk(node);

  node:=next;
 end;
end;

procedure t_jit_dynamic_blob.detach;
begin
 inc_ref('detach');

 rw_wlock(lock);

 detach_all_entry;
 detach_all_attc;
 detach_all_curr;
 detach_all_chunk;

 rw_wunlock(lock);

 dec_ref('detach');
end;

procedure delete_jit_cache(chunk:p_jcode_chunk);
var
 blob:p_jit_dynamic_blob;
begin
 if (chunk=nil) then Exit;

 //delete full in blob
 blob:=chunk^.blob;
 blob^.detach;

 {
 blob:=chunk^.blob;
 rw_wlock(blob^.lock);

  blob^.detach_chunk(chunk);
  blob^.detach_entry(start,__end);

 rw_wunlock(blob^.lock);
 }
end;

function on_destroy(handle:Pointer):Integer;
begin
 p_jcode_chunk(handle)^.tobj:=nil; //mark delete

 delete_jit_cache(p_jcode_chunk(handle));

 Result:=DO_DELETE;
end;

function on_trigger(handle:Pointer;mode:T_TRIGGER_MODE):Integer;
begin
 case mode of
  M_CPU_WRITE :;
  M_DMEM_WRITE:;
  else
   Exit;
 end;

 //TODO
 Result:=DO_INCREMENT;
end;

procedure blob_track(blob:p_jit_dynamic_blob);
var
 node:p_jcode_chunk;
 tobj:p_vm_track_object;
begin
 node:=blob^.chunk_list;

 while (node<>nil) do
 begin
  if (node^.start<>node^.__end) then
  begin

   node^.inc_ref('blob_track');

   rw_wlock(entry_chunk_lock);

    TAILQ_INSERT_TAIL(@entry_chunk_list,node,@node^.entry);

   rw_wunlock(entry_chunk_lock);

   tobj:=vm_track_object_allocate(node,node^.start,node^.__end,H_JIT_CHUNK,PAGE_TRACK_W);
   tobj^.on_destroy:=@on_destroy;
   tobj^.on_trigger:=@on_trigger;

   node^.tobj:=tobj;

   vm_map_track_insert(p_proc.p_vmspace,tobj);

   vm_track_object_deallocate(tobj); //<-vm_track_object_allocate
  end;
  //
  node:=node^.next;
 end;

end;


end.






