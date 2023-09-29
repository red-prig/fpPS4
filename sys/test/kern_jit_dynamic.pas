unit kern_jit_dynamic;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_jit2_ctx,

 hamt,
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
  var
   entry_list:p_entry_point;

   base:Pointer;
   size:ptruint;

   refs:Integer;

  procedure inc_ref;
  procedure dec_ref;
  procedure Free;
  procedure add_entry_point(src,dst:Pointer);
  procedure alloc_base(_size:ptruint);
  procedure free_base;
  procedure attach;
  procedure detach;
 end;

 p_jit_thr_ctx=^t_jit_thr_ctx;
 t_jit_thr_ctx=object
  frame:jit_frame;
  cblob:p_jit_dynamic;
 end;

function new_blob(_size:ptruint):p_jit_dynamic;

var
 entry_lock:Pointer=nil;
 entry_hamt:TSTUB_HAMT64;

function  fetch_entry(src:Pointer):t_jit_dynamic.p_entry_point;

procedure switch_to_jit();
function  jmp_dispatcher(addr:Pointer):Pointer;

implementation

uses
 vmparam,
 vm_pmap,
 trap;

procedure switch_to_jit();
var
 td:p_kthread;
 node:t_jit_dynamic.p_entry_point;
 jctx:p_jit_thr_ctx;
begin
 td:=curkthread;
 if (td=nil) then Exit;

 if not is_guest_addr(td^.td_frame.tf_rip) then
 begin
  Assert(False,'TODO');
 end;

 node:=fetch_entry(Pointer(td^.td_frame.tf_rip));

 if (node=nil) then
 begin
  Writeln('tf_rip:0x',HexStr(td^.td_frame.tf_rip,16));
  Assert(False);
 end;

 if (td^.td_jit_ctx=nil) then
 begin
  td^.td_jit_ctx:=AllocMem(SizeOf(t_jit_thr_ctx));
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

function jmp_dispatcher(addr:Pointer):Pointer;
var
 td:p_kthread;
 node:t_jit_dynamic.p_entry_point;
 jctx:p_jit_thr_ctx;
begin
 td:=curkthread;
 if (td=nil) then Exit(nil);

 if not is_guest_addr(QWORD(addr)) then
 begin
  Writeln('addr:0x',HexStr(addr));
  Assert(False,'TODO');
 end;

 node:=fetch_entry(addr);

 if (node=nil) then
 begin
  Writeln('addr:0x',HexStr(addr));
  Assert(False);
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

function fetch_entry(src:Pointer):t_jit_dynamic.p_entry_point;
var
 data:PPointer;
begin
 Result:=nil;
 rw_rlock(entry_lock);

 data:=HAMT_search64(@entry_hamt,QWORD(src));
 if (data<>nil) then
 begin
  Result:=data^;
 end;

 if (Result<>nil) then
 begin
  Result^.inc_ref;
 end;

 rw_runlock(entry_lock);
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

procedure t_jit_dynamic.add_entry_point(src,dst:Pointer);
var
 node:p_entry_point;
begin
 if (src=nil) or (dst=nil) then Exit;
 node:=AllocMem(Sizeof(t_entry_point));
 node^.next:=entry_list;
 node^.blob:=@Self;
 node^.src :=src;
 node^.dst :=dst;
 //
 entry_list:=node;
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

procedure t_jit_dynamic.attach;
var
 node,next:p_entry_point;
begin
 rw_wlock(entry_lock);

 node:=entry_list;
 while (node<>nil) do
 begin
  next:=node^.next;

  node^.inc_ref;

  HAMT_insert64(@entry_hamt,QWORD(node^.src),node);

  node:=next;
 end;

 rw_wunlock(entry_lock);
end;

procedure t_jit_dynamic.detach;
var
 node,next:p_entry_point;
begin
 rw_wlock(entry_lock);

 node:=entry_list;
 while (node<>nil) do
 begin
  next:=node^.next;

  HAMT_delete64(@entry_hamt,QWORD(node^.src),nil);

  node^.dec_ref;

  node:=next;
 end;

 rw_wunlock(entry_lock);
end;



end.






