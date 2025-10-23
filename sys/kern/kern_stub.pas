unit kern_stub;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

{.$DEFINE chunk_alloc}

const
 m_header=WORD($C3C3);

 m_free__chunk=1;
 m_first_chunk=2;
 m_last__chunk=4;

type
 p_stub_chunk=^stub_chunk;
 stub_chunk=packed object
  head     :WORD;
  flags    :WORD;
  prev_size:Integer;
  curr_size:Integer;
  refs     :Integer;
  pLeft    :p_stub_chunk;
  pRight   :p_stub_chunk;
  body     :record end;
  function c(n1,n2:p_stub_chunk):Integer; static;
 end;

function  is_near_valid(vaddr,body:Pointer):Boolean;
function  is_mask_valid(vaddr,body:Pointer;mask:DWORD):Boolean;

function  p_alloc  (vaddr:Pointer;size:Integer;guest:Boolean):p_stub_chunk;
function  p_alloc_m(vaddr:Pointer;size:Integer;mask:DWORD;guest:Boolean):p_stub_chunk;
procedure p_free   (chunk:p_stub_chunk);
procedure p_inc_ref(chunk:p_stub_chunk);
procedure p_dec_ref(chunk:p_stub_chunk);

implementation

uses
 g_node_splay,
 {$IFDEF chunk_alloc}
 hamt,
 {$ENDIF}
 kern_rwlock,
 vm,
 vmparam,
 vm_map,
 vm_mmap,
 md_map,
 vm_object,
 kern_proc;

function stub_chunk.c(n1,n2:p_stub_chunk):Integer;
begin
 Result:=Integer(n1^.curr_size>n2^.curr_size)-Integer(n1^.curr_size<n2^.curr_size);
end;

type
 t_chunk_free=specialize TNodeSplay<stub_chunk>;

var
 {$IFDEF chunk_alloc}
 chunk_alloc:array[Boolean] of TSTUB_HAMT64;
 {$ENDIF}

 chunk_free:array[Boolean] of t_chunk_free=
 (
  (pRoot:nil),(pRoot:nil)
 );

 chunk_lock:array[Boolean] of Pointer=(nil,nil);

function AlignUp(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
var
 tmp:PtrUInt;
begin
 if (alignment=0) then Exit(addr);
 tmp:=addr+PtrUInt(alignment-1);
 Result:=tmp-(tmp mod alignment)
end;

function is_near_valid(vaddr,body:Pointer):Boolean;
var
 delta:Int64;
begin
 delta:=abs(Int64(body)-Int64(vaddr));
 Result:=(delta<High(Integer));
end;

const
 XMASK:array[0..1] of DWORD=(
  $FF000000, //0xNN XX XX XX  4-byte instruction overlap
  $FFFF0000  //0xNN NN XX XX  3-byte instruction overlap
 );

function is_mask_valid(vaddr,body:Pointer;mask:DWORD):Boolean;
var
 delta:Int64;
 x:DWORD;
begin
 delta:=Int64(body)-Int64(vaddr);
 if not (abs(delta)<High(Integer)) then Exit(False);
 X:=XMASK[mask and 1];
 Result:=(DWORD(delta) and X)=(DWORD(mask) and X);
end;

function alloc_segment(start,size:QWORD;guest:Boolean):p_stub_chunk;
var
 map:vm_map_t;
 err:Integer;
begin
 Result:=nil;

 if guest then
 begin
  size:=AlignUp(size+SizeOf(stub_chunk),PAGE_SIZE);

  map:=p_proc.p_vmspace;

  if (start=0) then
  begin
   start:=SCE_REPLAY_EXEC_START;
  end;

  err:=vm_map_find(map, nil,
                   0, @start, size,
                   VMFS_OPTIMAL_SPACE,
                   VM_PROT_RWX, VM_PROT_RWX,
                   MAP_COW_NO_BUDGET or MAP_COW_PATCH,
                   0,
                   nil);

  if (err<>0) then Exit;

 end else
 begin
  size:=AlignUp(size+SizeOf(stub_chunk),MD_PAGE_SIZE);

  if (start=0) then
  begin
   start:=KERNEL_LOWER;
  end;

  err:=md_mmap(Pointer(start),size,VM_RWX);

  if (err<>0) then Exit;
 end;

 Result:=Pointer(start);

 Result^.head         :=m_header;
 Result^.flags        :=m_free__chunk or m_first_chunk or m_last__chunk;
 Result^.prev_size    :=0;
 Result^.curr_size    :=size;
 Result^.refs         :=0;
 Result^.pLeft        :=nil;
 Result^.pRight       :=nil;
end;

procedure free_segment(chunk:p_stub_chunk;guest:Boolean);
var
 map:vm_map_t;
begin
 if guest then
 begin
  map:=p_proc.p_vmspace;

  vm_map_remove(map, qword(chunk), qword(chunk) + chunk^.curr_size, MAP_COW_PATCH);
 end else
 begin
  md_unmap(chunk,chunk^.curr_size);
 end;
end;

procedure add_to_free_list(chunk:p_stub_chunk;guest:Boolean); inline;
begin
 chunk_free[guest].Insert(chunk);
end;

procedure del_from_free_list(chunk:p_stub_chunk;guest:Boolean); inline;
begin
 chunk_free[guest].Delete(chunk);
 chunk^.pLeft :=nil;
 chunk^.pRight:=nil;
end;

procedure fix_next_size(chunk:p_stub_chunk);
var
 next:p_stub_chunk;
begin
 if ((chunk^.flags and m_last__chunk)=0) then
 begin
  next:=Pointer(chunk)+chunk^.curr_size;
  next^.prev_size:=chunk^.curr_size;
 end;
end;

procedure split_chunk(chunk:p_stub_chunk;used_size:Integer;guest:Boolean);
var
 chunk_size:Integer;
 next:p_stub_chunk;
begin
 chunk_size:=chunk^.curr_size;

 if (AlignUp(used_size+SizeOf(stub_chunk)*2,SizeOf(Pointer))>chunk_size) then Exit;

 used_size:=AlignUp(used_size+SizeOf(stub_chunk),SizeOf(Pointer));

 next:=Pointer(chunk)+used_size;

 chunk^.curr_size:=used_size;

 next^.head         :=m_header;
 next^.flags        :=m_free__chunk;
 next^.prev_size    :=used_size;
 next^.curr_size    :=chunk_size-used_size;
 next^.refs         :=0;
 next^.pLeft        :=nil;
 next^.pRight       :=nil;

 if ((chunk^.flags and m_last__chunk)<>0) then
 begin
  chunk^.flags:=chunk^.flags and (not m_last__chunk);
  next ^.flags:=next^.flags or m_last__chunk;
 end;

 add_to_free_list(next,guest);

 fix_next_size(next);
end;

procedure merge_chunk(var chunk:p_stub_chunk;guest:Boolean);
var
 prev,next:p_stub_chunk;
begin

 if (chunk^.prev_size<>0) and
    ((chunk^.flags and m_first_chunk)=0) then
 begin
  prev:=Pointer(chunk)-chunk^.prev_size;
  if ((prev^.flags and m_free__chunk)<>0) then
  begin
   Assert(prev^.curr_size=chunk^.prev_size,'invalid prev chunk curr_size');
   Assert(prev^.refs=0                    ,'invalid prev chunk refs');

   del_from_free_list(prev,guest);

   prev^.curr_size:=prev^.curr_size+chunk^.curr_size;

   if ((chunk^.flags and m_last__chunk)<>0) then
   begin
    prev^.flags:=prev^.flags or m_last__chunk;
   end;

   chunk^:=Default(stub_chunk);
   chunk:=prev;

   fix_next_size(chunk);
  end;
 end;

 if ((chunk^.flags and m_last__chunk)=0) then
 begin
  next:=Pointer(chunk)+chunk^.curr_size;
  if ((next^.flags and m_free__chunk)<>0) then
  begin
   Assert(next^.prev_size=chunk^.curr_size,'invalid next chunk prev_size');
   Assert(next^.refs=0                    ,'invalid next chunk refs');

   del_from_free_list(next,guest);

   chunk^.curr_size:=chunk^.curr_size+next^.curr_size;

   if ((next^.flags and m_last__chunk)<>0) then
   begin
    chunk^.flags:=chunk^.flags or m_last__chunk;
   end;

   next^:=Default(stub_chunk);

   fix_next_size(chunk);
  end;
 end;
end;

function find_free_chunk(vaddr:Pointer;size:Integer;guest:Boolean):p_stub_chunk;
var
 stub:stub_chunk;
 entry:p_stub_chunk;
begin
 Result:=nil;
 size:=size+SizeOf(stub_chunk);

 stub.curr_size:=size;
 entry:=chunk_free[guest].Find_be(@stub);

 while (entry<>nil) do
 begin
  //
  if (entry^.curr_size>=size) then
  begin
   if (vaddr=nil) or is_near_valid(vaddr,@entry^.body) then
   begin
    del_from_free_list(entry,guest);
    Exit(entry);
   end;
  end;
  //
  entry:=chunk_free[guest].Next(entry);
 end;

end;

function find_free_chunk_m(vaddr:Pointer;size:Integer;mask:DWORD;guest:Boolean):p_stub_chunk;
var
 stub:stub_chunk;
 entry:p_stub_chunk;
begin
 Result:=nil;
 size:=size+SizeOf(stub_chunk);

 stub.curr_size:=size;
 entry:=chunk_free[guest].Find_be(@stub);

 while (entry<>nil) do
 begin
  //
  if (entry^.curr_size>=size) then
  begin
   if is_mask_valid(vaddr,@entry^.body,mask) then
   begin
    del_from_free_list(entry,guest);
    Exit(entry);
   end;
  end;
  //
  entry:=chunk_free[guest].Next(entry);
 end;

end;

function p_alloc(vaddr:Pointer;size:Integer;guest:Boolean):p_stub_chunk;
var
 chunk:p_stub_chunk;
begin
 rw_wlock(chunk_lock[guest]);

 chunk:=find_free_chunk(vaddr,size,guest);

 if (chunk=nil) then
 begin
  chunk:=alloc_segment(QWORD(vaddr),size,guest);
  Assert(chunk<>nil,'p_alloc NOMEM');

  if (vaddr<>nil) then
  if (not is_near_valid(vaddr,@chunk^.body)) then
  if (QWORD(vaddr)>High(Integer)) then
  begin
   free_segment(chunk,guest);

   if guest then
   begin
    chunk:=alloc_segment(AlignUp(QWORD(vaddr)-High(Integer),PAGE_SIZE),size,guest);
   end else
   begin
    chunk:=alloc_segment(AlignUp(QWORD(vaddr)-High(Integer),MD_PAGE_SIZE),size,guest);
   end;

   Assert(chunk<>nil,'p_alloc NOMEM');
  end;

  if (vaddr<>nil) then
  begin
   Assert(is_near_valid(vaddr,@chunk^.body),'p_alloc is_near_valid');
  end;
 end;

 split_chunk(chunk,size,guest);

 chunk^.flags:=chunk^.flags and (not m_free__chunk);

 {$IFDEF chunk_alloc}
 HAMT_insert64(@chunk_alloc[guest],QWORD(chunk),chunk);
 {$ENDIF}

 rw_wunlock(chunk_lock[guest]);

 Result:=chunk;
end;

//

function p_alloc_m(vaddr:Pointer;size:Integer;mask:DWORD;guest:Boolean):p_stub_chunk;
var
 chunk:p_stub_chunk;
 x:Integer;
begin
 rw_wlock(chunk_lock[guest]);

 chunk:=find_free_chunk_m(vaddr,size,mask,guest);

 if (chunk=nil) then
 begin
  x:=Integer(mask and XMASK[mask and 1]);

  if (x<0) and (QWORD(vaddr)<abs(x)) then
  begin
   Result:=nil;
   rw_wunlock(chunk_lock[guest]);
  end;

  chunk:=alloc_segment(QWORD(Int64(vaddr)+x),size,guest);
  if (chunk=nil) then
  begin
   Result:=nil;
   rw_wunlock(chunk_lock[guest]);
  end;

  if not is_mask_valid(vaddr,@chunk^.body,mask) then
  begin
   free_segment(chunk,guest);
   Result:=nil;
   rw_wunlock(chunk_lock[guest]);
  end;
 end;

 split_chunk(chunk,size,guest);

 chunk^.flags:=chunk^.flags and (not m_free__chunk);

 {$IFDEF chunk_alloc}
 HAMT_insert64(@chunk_alloc[guest],QWORD(chunk),chunk);
 {$ENDIF}

 rw_wunlock(chunk_lock[guest]);

 Result:=chunk;
end;

//

procedure p_free(chunk:p_stub_chunk);
var
 guest:Boolean;
begin
 if (chunk=nil) then Exit;

 guest:=is_guest_addr(QWORD(chunk));

 rw_wlock(chunk_lock[guest]);

 {$IFDEF chunk_alloc}
 if (HAMT_search64(@chunk_alloc[guest],QWORD(chunk))=nil) then
 begin
  rw_wunlock(chunk_lock[guest]);
  Exit;
 end;

 HAMT_delete64(@chunk_alloc[guest],QWORD(chunk),nil);

 {$ENDIF}

 chunk^.flags:=chunk^.flags or m_free__chunk;

 merge_chunk(chunk,guest);

 if ((chunk^.flags and (m_first_chunk or m_last__chunk))=(m_first_chunk or m_last__chunk)) then
 begin
  free_segment(chunk,guest);
 end else
 begin
  add_to_free_list(chunk,guest);
 end;

 rw_wunlock(chunk_lock[guest]);
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

