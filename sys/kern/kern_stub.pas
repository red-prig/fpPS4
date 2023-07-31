unit kern_stub;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue;

const
 m_header_alloc=Integer($C3C3C3C3);
 m_header_free =Integer($C3C3C390);

 segment_size  =64*1024;

type
 p_stub_chunk=^stub_chunk;
 stub_chunk=packed record
  head     :Integer;
  prev_size:Integer;
  curr_size:Integer;
  refs     :Integer;
  link     :TAILQ_ENTRY;
  body     :record end;
 end;

function  p_alloc  (vaddr:Pointer;size:Integer):p_stub_chunk;
procedure p_free   (chunk:p_stub_chunk);
procedure p_inc_ref(chunk:p_stub_chunk);
procedure p_dec_ref(chunk:p_stub_chunk);

implementation

uses
 hamt,
 kern_rwlock,
 vm,
 vm_map,
 vm_mmap,
 vm_object;

var
 chunk_alloc:TSTUB_HAMT64;
 chunk_free :TAILQ_HEAD=(tqh_first:nil;tqh_last:@chunk_free.tqh_first);

 chunk_lock :Pointer=nil;

function alloc_segment(start:Pointer):p_stub_chunk;
var
 map:vm_map_t;
 err:Integer;
begin
 Result:=nil;
 map:=@g_vmspace.vm_map;

 err:=vm_mmap2(map,
               @start,
               segment_size,
               VM_PROT_RWX,
               VM_PROT_RWX,
               MAP_ANON or MAP_PRIVATE or (16 shl MAP_ALIGNMENT_BIT),
               OBJT_DEFAULT,
               nil,
               0);

 if (err<>0) then Exit;

 Result:=start;

 Result^.head         :=m_header_free;
 Result^.prev_size    :=0;
 Result^.curr_size    :=segment_size;
 Result^.refs         :=0;
 Result^.link.tqe_next:=nil;
 Result^.link.tqe_prev:=nil;
end;

procedure free_segment(chunk:p_stub_chunk);
var
 map:vm_map_t;
begin
 map:=@g_vmspace.vm_map;

 vm_map_lock  (map);
 vm_map_delete(map, qword(chunk), qword(chunk) + segment_size);
 vm_map_unlock(map);
end;

function AlignUp(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
var
 tmp:PtrUInt;
begin
 if (alignment=0) then Exit(addr);
 tmp:=addr+PtrUInt(alignment-1);
 Result:=tmp-(tmp mod alignment)
end;

procedure split_chunk(chunk:p_stub_chunk;used_size:Integer);
var
 chunk_size:Integer;
 next:p_stub_chunk;
begin
 chunk_size:=chunk^.curr_size;

 if (AlignUp(used_size+SizeOf(stub_chunk)*2,SizeOf(Pointer))>chunk_size) then Exit;

 used_size:=AlignUp(used_size+SizeOf(stub_chunk),SizeOf(Pointer));

 next:=Pointer(chunk)+used_size;

 chunk^.curr_size:=used_size;

 next^.head         :=m_header_free;
 next^.prev_size    :=used_size;
 next^.curr_size    :=chunk_size-used_size;
 next^.refs         :=0;
 next^.link.tqe_next:=nil;
 next^.link.tqe_prev:=nil;

 TAILQ_INSERT_TAIL(@chunk_free,next,@next^.link);
end;

procedure merge_chunk(var chunk:p_stub_chunk);
var
 prev,next:p_stub_chunk;
begin

 if (chunk^.prev_size<>0) then
 begin
  prev:=Pointer(chunk)-chunk^.prev_size;
  if (prev^.head=m_header_free) then
  begin
   Assert(prev^.curr_size=chunk^.prev_size,'invalid prev chunk curr_size');
   Assert(prev^.refs=0                    ,'invalid prev chunk refs');

   prev^.curr_size:=prev^.curr_size+chunk^.curr_size;
   chunk:=prev;
  end;
 end;

 if (chunk^.curr_size<segment_size) then
 begin
  next:=Pointer(chunk)+chunk^.curr_size;
  if (next^.head=m_header_free) then
  begin
   Assert(next^.prev_size=chunk^.curr_size,'invalid next chunk prev_size');
   Assert(next^.refs=0                    ,'invalid next chunk refs');

   TAILQ_REMOVE(@chunk_free,next,@next^.link);

   chunk^.curr_size:=chunk^.curr_size+next^.curr_size;
  end;
 end;
end;

function find_free_chunk(vaddr:Pointer;size:Integer):p_stub_chunk;
var
 entry,next:p_stub_chunk;
 delta:Int64;
begin
 Result:=nil;
 entry:=TAILQ_FIRST(@chunk_free);

 while (entry<>nil) do
 begin
  next:=TAILQ_NEXT(entry,@entry^.link);
  //
  if (entry^.curr_size>=(size+SizeOf(stub_chunk))) then
  begin
   delta:=abs(Int64(vaddr)-Int64(@entry^.body));
   if (vaddr=nil) or (delta<High(Integer)) then
   begin
    TAILQ_REMOVE(@chunk_free,entry,@entry^.link);
    Exit(entry);
   end;
  end;
  //
  entry:=next;
 end;
end;

function p_alloc(vaddr:Pointer;size:Integer):p_stub_chunk;
var
 chunk:p_stub_chunk;
begin
 Assert(size<=(segment_size-SizeOf(stub_chunk)),'p_alloc to big');

 rw_wlock(chunk_lock);

 chunk:=find_free_chunk(vaddr,size);

 if (chunk=nil) then
 begin
  chunk:=alloc_segment(vaddr);
  Assert(chunk<>nil,'p_alloc NOMEM');
 end;

 split_chunk(chunk,size);

 chunk^.head:=m_header_alloc;
 HAMT_insert64(@chunk_alloc,QWORD(chunk),chunk);

 rw_wunlock(chunk_lock);

 Result:=chunk;
end;

procedure p_free(chunk:p_stub_chunk);
begin
 if (chunk=nil) then Exit;

 rw_wlock(chunk_lock);

 if (HAMT_search64(@chunk_alloc,QWORD(chunk))=nil) then
 begin
  rw_wunlock(chunk_lock);
  Exit;
 end;

 HAMT_delete64(@chunk_alloc,QWORD(chunk),nil);
 chunk^.head:=m_header_free;

 merge_chunk(chunk);

 if (chunk^.curr_size>=segment_size) then
 begin
  free_segment(chunk);
 end else
 begin
  TAILQ_INSERT_TAIL(@chunk_free,chunk,@chunk^.link);
 end;

 rw_wunlock(chunk_lock);
end;

procedure p_inc_ref(chunk:p_stub_chunk);
begin
 if (chunk=nil) then Exit;

 System.InterlockedIncrement(chunk^.refs);
end;

procedure p_dec_ref(chunk:p_stub_chunk);
begin
 if (chunk=nil) then Exit;

 if (System.InterlockedDecrement(chunk^.refs)=0) then
 begin
  p_free(chunk);
 end;
end;

end.

