unit kern_blockpool;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 vmparam,
 vm_blockpool,
 vm_object,
 vm_map,
 vm_pmap,
 kern_mtx,
 kern_dmem;

function sys_blockpool_open (flags:Integer):Integer;
function sys_blockpool_map  (addr:Pointer;len:QWORD;mtype,prot,flags:DWORD):Integer;
function sys_blockpool_unmap(addr:Pointer;len:QWORD;flags:DWORD):Integer;

type
 t_dmem_bits=object
  const
   M_QWORD_COUNT=(VM_DMEM_SIZE+(M_64K*64)-1) div (M_64K*64);
   M_BACKT_BITS =(M_QWORD_COUNT+63) div 64;
   M_QWORD_ALIGN=M_BACKT_BITS*64;
   M_BACKT_COUNT=(M_QWORD_ALIGN+63) div 64;
  type
   t_qword=packed record
    val:qword;
   end;
  var
   availableBlocks:DWORD;
   //
   bits:array[0..M_QWORD_ALIGN-1] of t_qword;
   cany:array[0..M_BACKT_COUNT-1] of t_qword;
   full:array[0..M_BACKT_COUNT-1] of t_qword;
   //
   Procedure Fill(start,__end:DWORD);
   function  FindFirst:Integer;
   procedure Commit(s:DWORD);
   procedure Decommit(s:DWORD);
 end;

 p_blockpool=^t_blockpool;
 t_blockpool=object
  lock                  :mtx;
  start_dmem            :QWORD;
  __end_dmem            :QWORD;
  dmap                  :p_dmem_obj;
  budget_id             :Integer;
  refs                  :DWORD;
  //
  allocatedCachedBlocks :DWORD;
  allocatedFlushedBlocks:DWORD;
  //
  Cached                :t_dmem_bits;
  Flushed               :t_dmem_bits;
  //
  procedure flush   ();
  function  commit  (count,onion,writeback:DWORD;buf:PDWORD):Integer;
  procedure decommit(buf:PDWORD;count:DWORD);
 end;

function  blockpool_acqure (bp:p_blockpool):p_blockpool;
procedure blockpool_release(bp:p_blockpool);

function  blockpool_pager_alloc  (handle:Pointer;size:QWORD):vm_object_t;
procedure blockpool_pager_dealloc(obj:vm_object_t);

implementation

uses
 errno,
 dmem_map,
 kern_proc,
 kern_thr,
 kern_descrip,
 kern_budget,
 vfile,
 vfcntl,
 sys_conf,
 vstat;

//

Procedure t_dmem_bits.Fill(start,__end:DWORD);
var
 i:DWORD;
begin

 availableBlocks:=availableBlocks + (__end-start);

 //

 while (start<__end) do
 begin

  i:=start div 64;

  with bits[i] do
  begin

   if (val=0) then
   with cany[i div 64] do
   begin
    val:=val or (QWORD(1) shl (i mod 64));
   end;

   val:=val or (QWORD(1) shl (start mod 64));

   if (val=QWORD(-1)) then
   with full[i div 64] do
   begin
    val:=val or (QWORD(1) shl (i mod 64));
   end;

  end;

  start:=start+1;
 end;

end;

function t_dmem_bits.FindFirst:Integer;
var
 i,f,p:DWORD;
begin

 for i:=0 to High(cany) do
 begin
  f:=System.BsfQWord(cany[i].val);

  if (f<>255) then
  begin
   f:=i*64+f;

   p:=System.BsfQWord(bits[f].val);

   Assert(p<>255);

   p:=p+f*64;

   Exit(p);
  end;

 end;

 Exit(-1);
end;

procedure t_dmem_bits.Commit(s:DWORD);
var
 i:DWORD;
begin
 availableBlocks:=availableBlocks-1;

 i:=s div 64;

 with bits[i] do
 begin

  if (val=QWORD(-1)) then
  with full[i div 64] do
  begin
   val:=val and (not (QWORD(1) shl (i mod 64)));
  end;

  val:=val and (not (QWORD(1) shl (s mod 64)));

  if (val=0) then
  with cany[i div 64] do
  begin
   val:=val and (not (QWORD(1) shl (i mod 64)));
  end;

 end;

end;

procedure t_dmem_bits.Decommit(s:DWORD);
var
 i:DWORD;
begin
 availableBlocks:=availableBlocks+1;

 i:=s div 64;

 with bits[i] do
 begin

  if (val=0) then
  with cany[i div 64] do
  begin
   val:=val or (QWORD(1) shl (i mod 64));
  end;

  val:=val or (QWORD(1) shl (s mod 64));

  if (val=QWORD(-1)) then
  with full[i div 64] do
  begin
   val:=val or (QWORD(1) shl (i mod 64));
  end;

 end;

end;

//

procedure t_blockpool.flush();
var
 s:Integer;
begin
 // cached -> flushed
 repeat
  s:=Cached.FindFirst;
  if (s=-1) then Break;

  Cached.Commit(s);
  Flushed.Decommit(s);

 until false;
end;

function t_blockpool.commit(count,onion,writeback:DWORD;buf:PDWORD):Integer;
label
 _repeat;
var
 saved_count:DWORD;
 s:Integer;
begin
 Result:=0;

 if (count > (Flushed.availableBlocks + Cached.availableBlocks)) then
 begin
  Exit(ENOMEM);
 end;

 saved_count:=count;

 _repeat:
 while (count<>0) do
 begin

  if (count <= Cached.availableBlocks) and (onion<>0) then
  begin

   // cached
   while (count<>0) do
   begin
    s:=Cached.FindFirst;
    if (s=-1) then Break;

    Cached.Commit(s);

    buf[0]:=s;
    Inc(buf);
    Dec(Count);
   end;

   goto _repeat;
  end;

  if (onion<>0) then
  begin
   //count > Cached.availableBlocks

   if (Cached.availableBlocks<>0) then
   begin

    // cached
    while (count<>0) do
    begin
     s:=Cached.FindFirst;
     if (s=-1) then Break;

     Cached.Commit(s);

     buf[0]:=s;
     Inc(buf);
     Dec(Count);
    end;

   end;

   // flushed
   while (count<>0) do
   begin
    s:=Flushed.FindFirst;
    if (s=-1) then Break;

    Flushed.Commit(s);

    buf[0]:=s;
    Inc(buf);
    Dec(Count);
   end;

   goto _repeat;
  end;

  if (count <= Flushed.availableBlocks) then
  begin

   // flushed
   while (count<>0) do
   begin
    s:=Flushed.FindFirst;
    if (s=-1) then Break;

    Flushed.Commit(s);

    buf[0]:=s;
    Inc(buf);
    Dec(Count);
   end;

   goto _repeat;
  end;

  if (Flushed.availableBlocks < 32) then
  begin

   if ((count - Flushed.availableBlocks)=0) then
   begin

    // cached
    while (count<>0) do
    begin
     s:=Cached.FindFirst;
     if (s=-1) then Break;

     Cached.Commit(s);

     buf[0]:=s;
     Inc(buf);
     Dec(Count);
    end;

    goto _repeat;
   end;

   if (Flushed.availableBlocks<>0) then
   begin

    // flushed
    while (count<>0) do
    begin
     s:=Flushed.FindFirst;
     if (s=-1) then Break;

     Flushed.Commit(s);

     buf[0]:=s;
     Inc(buf);
     Dec(Count);
    end;

   end;


   // cached
   while (count<>0) do
   begin
    s:=Cached.FindFirst;
    if (s=-1) then Break;

    Cached.Commit(s);

    buf[0]:=s;
    Inc(buf);
    Dec(Count);
   end;

   goto _repeat;
  end;

  flush();
 end;

 if (writeback=0) then
 begin
  allocatedFlushedBlocks:=allocatedFlushedBlocks + saved_count;
 end else
 begin
  allocatedCachedBlocks :=allocatedCachedBlocks  + saved_count;
 end;

end;

procedure t_blockpool.decommit(buf:PDWORD;count:DWORD);
var
 i:DWORD;
 mflags:t_dmem_block;
begin
 if (count=0) then Exit;

 for i:=0 to count-1 do
 begin
  mflags:=t_dmem_block(buf[i]);
  if (mflags.valid<>0) then
  begin

   if (mflags.writeback=0) then
   begin
    allocatedFlushedBlocks:=allocatedFlushedBlocks-1;
    Flushed.Decommit(mflags.offset);
   end else
   begin
    allocatedCachedBlocks :=allocatedCachedBlocks -1;
    Cached.Decommit(mflags.offset);
   end;

   buf[i]:=0;
  end;
 end;

end;

//

procedure blockpool_free(bp:p_blockpool); forward;

function blockpool_acqure(bp:p_blockpool):p_blockpool;
begin
 System.InterlockedIncrement(bp^.refs);
 Result:=bp;
end;

procedure blockpool_release(bp:p_blockpool);
begin
 if (System.InterlockedDecrement(bp^.refs)=0) then
 begin
  blockpool_free(bp);
 end;
end;

function IDX_TO_OFF(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function OFF_TO_IDX(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function blockpool_pager_alloc(handle:Pointer;size:QWORD):vm_object_t;
var
 bp:p_blockpool;
 tlb_1gb_cnt:DWORD;
 tlb_64k_cnt:DWORD;
 tlb_1gb    :PDWORD;
 tlb_64k    :p_dmem_block;
begin

 if (p_proc.p_sdk_version > $4ffffff) then
 begin
  if (size > QWORD($7fffffffffff)) then
  begin
   Exit(nil);
  end;
 end else
 if ((size shr 16) > $3ffff) then
 begin
  Exit(nil);
 end;

 bp:=handle;

 tlb_1gb_cnt:=(size+M_1GB-1) div M_1GB;
 tlb_64k_cnt:=(size+M_64K-1) div M_64K;

 tlb_1gb:=AllocMem((tlb_1gb_cnt+tlb_64k_cnt)*SizeOf(DWORD));

 if (tlb_1gb=nil) then
 begin
  Exit(nil);
 end;

 tlb_64k:=@tlb_1gb[tlb_1gb_cnt];

 Result:=vm_object_allocate(OBJT_BLOCKPOOL,OFF_TO_IDX(size));
 if (Result=nil) then
 begin
  FreeMem(tlb_1gb);
  Exit(nil);
 end;

 mtx_lock(bp^.lock);

  if bp^.commit(tlb_1gb_cnt,1,1,tlb_1gb)<>0 then
  begin
   mtx_unlock(bp^.lock);
   vm_object_destroy(Result);
   FreeMem(tlb_1gb);
   Exit(nil);
  end;

  Result^.un_pager.bpl.tlb_1gb:=tlb_1gb;
  Result^.un_pager.bpl.tlb_64k:=tlb_64k;
  Result^.handle:=blockpool_acqure(bp);

 mtx_unlock(bp^.lock);
end;

procedure blockpool_pager_dealloc(obj:vm_object_t);
var
 bp:p_blockpool;
 i,size:QWORD;
 tlb_cnt:DWORD;
 tlb_1gb:PDWORD;
begin
 bp:=obj^.handle;

 size:=IDX_TO_OFF(obj^.size);

 tlb_cnt:=(size+M_1GB-1) div M_1GB;
 tlb_1gb:=obj^.un_pager.bpl.tlb_1gb;

 for i:=0 to tlb_cnt-1 do
 begin
  bp^.Cached.Decommit(tlb_1gb[i]);
 end;
 bp^.allocatedCachedBlocks:=bp^.allocatedCachedBlocks-tlb_cnt;

 FreeMem(tlb_1gb);

 blockpool_release(bp);

 obj^.un_pager.bpl.tlb_1gb:=nil;
 obj^.un_pager.bpl.tlb_64k:=nil;
 obj^.handle:=nil;
 obj^.otype :=OBJT_DEAD;
end;

function blockpool_obj_get_info(map  :vm_map_t;
                                obj  :vm_map_object;
                                addr :QWORD;
                                qinfo:pSceKernelVirtualQueryInfo;
                                has_sdk_version_5:Boolean):Integer; public;
var
 vm_start   :QWORD;
 start      :QWORD;
 __end      :QWORD;
 i          :DWORD;
 block_start:DWORD;
 block___end:DWORD;
 tlb_64k    :p_dmem_block;
 mflags     :t_dmem_block;
 mval       :DWORD;
 mtype      :DWORD;
begin
 vm_start:=QWORD(qinfo^.pstart);

 start:=addr;
 __end:=QWORD(qinfo^.p__end);
 //vm_blockpool_get_name(map,addr,&start,&__end,name);

 block_start:=0;
 if (vm_start <= start) then
 begin
  block_start:=(start - vm_start) div M_64K;
 end;

 block___end:=IDX_TO_OFF(obj^.size) div M_64K;
 if (((__end - vm_start) div M_64K) <= block___end) then
 begin
  block___end:=(__end - vm_start) div M_64K;
 end;

 tlb_64k:=obj^.un_pager.bpl.tlb_64k;

 //scan changes
 i:=block_start;

 mflags:=tlb_64k[i];
 mval  :=DWORD(mflags) shr 23;

 while (i<block___end) do
 begin

  if ((DWORD(tlb_64k[i]) shr 23)<>mval) then
  begin
   Break;
  end;

  Inc(i);
 end;

 block___end:=i;

 //fixup
 if has_sdk_version_5 then
 begin
  start:=QWORD(block_start)*M_64K;
  __end:=QWORD(block___end)*M_64K;
 end else
 begin
  start:=QWORD(DWORD(block_start)*M_64K);
  __end:=QWORD(DWORD(block___end)*M_64K);
 end;

 qinfo^.pstart:=Pointer(start + vm_start);
 qinfo^.p__end:=Pointer(__end + vm_start);

 qinfo^.protection:=mflags.prot;

 if (mflags.valid<>0) then
 begin
  mtype:=SCE_KERNEL_WB_GARLIC;

  if ((DWORD(mflags) and DWORD(MT_ONION_MT_WRITEBACK)) <> DWORD(MT_WRITEBACK)) then
  begin
   mtype:=SCE_KERNEL_WB_ONION;
  end;

  if ((DWORD(mflags) and DWORD(MT_ONION_MT_WRITEBACK))=0) then
  begin
   mtype:=SCE_KERNEL_WC_GARLIC;
  end;

  qinfo^.memoryType:=mtype;
 end;

 Result:=mflags.valid;
end;

function get_mflags(mtype:DWORD):t_dmem_block; inline;
begin
 if (mtype<>SCE_KERNEL_WB_GARLIC) then
 begin
  if (mtype=SCE_KERNEL_WC_GARLIC) then
  begin
   Result:=Default(t_dmem_block);
  end else
  begin
   Result:=MT_ONION_MT_WRITEBACK;
  end;
 end else
 begin
  Result:=MT_WRITEBACK;
 end;
end;

procedure blockpool_type_protect(map        :vm_map_t;
                                 obj        :vm_map_object;
                                 vm_start   :QWORD;
                                 block_start:DWORD;
                                 block___end:DWORD;
                                 mtype      :DWORD;
                                 prot       :DWORD); public;
var
 bp:p_blockpool;
 tlb_64k:p_dmem_block;
 mflags:t_dmem_block;
 mprev :t_dmem_block;
 i:DWORD;
begin
 bp:=obj^.handle;
 tlb_64k:=obj^.un_pager.bpl.tlb_64k;

 if mtype<>DWORD(-1) then
 begin
  //flags
  mflags:=get_mflags(mtype);

  //valid&prot
  mflags.prot :=prot;
  mflags.valid:=1;
 end;

 mtx_lock(bp^.lock);

  i:=block_start;
  while (i<block___end) do
  begin
   mprev:=tlb_64k[i];

   if (mprev.valid<>0) then
   begin

    if (mprev.prot<>prot) then
    begin
     vm_map_protect_internal(map,
                             nil,
                             vm_start+i*M_64K,
                             vm_start+i*M_64K+M_64K,
                             mprev.prot,
                             prot);
    end;

    if mtype=DWORD(-1) then
    begin
     //mprotect
     mprev.prot:=prot;
     tlb_64k[i]:=mprev;
    end else
    begin
     //mtypeprotect

     with bp^ do
     if (mprev.writeback<>mflags.writeback) then
     begin
      if (mflags.writeback=0) then
      begin
       allocatedFlushedBlocks:=allocatedFlushedBlocks + 1;
       allocatedCachedBlocks :=allocatedCachedBlocks  - 1;
      end else
      begin
       allocatedCachedBlocks :=allocatedCachedBlocks  + 1;
       allocatedFlushedBlocks:=allocatedFlushedBlocks - 1;
      end;
     end;

     DWORD(tlb_64k[i]):=mprev.offset or DWORD(mflags);
    end;

   end; //(mprev.valid<>0)

   Inc(i);
  end;

 mtx_unlock(bp^.lock);
end;

function kern_blockpool_map(map        :vm_map_t;
                            obj        :vm_map_object;
                            vm_start   :QWORD;
                            block_start:DWORD;
                            block___end:DWORD;
                            prot       :DWORD;
                            mtype      :DWORD):Integer;
var
 pmap:pmap_t;
 bp:p_blockpool;
 tlb_64k:p_dmem_block;
 mflags:t_dmem_block;
 i:DWORD;
begin
 pmap:=map^.pmap;

 bp:=obj^.handle;
 tlb_64k:=obj^.un_pager.bpl.tlb_64k;

 //flags
 mflags:=get_mflags(mtype);

 //valid&prot
 mflags.prot :=prot;
 mflags.valid:=1;

 mtx_lock(bp^.lock);

  //check
  i:=block_start;
  while (i<block___end) do
  begin
   if (tlb_64k[i].valid<>0) then
   begin
    mtx_unlock(bp^.lock);
    Exit(EBUSY);
   end;
   Inc(i);
  end;

  //commit
  Result:=bp^.commit(block___end-block_start,mflags.onion,mflags.writeback,@tlb_64k[block_start]);
  if (Result<>0) then
  begin
   mtx_unlock(bp^.lock);
   Exit();
  end;

  //fill flags and pmap
  i:=block_start;
  while (i<block___end) do
  begin

   pmap_enter_dmem_block(pmap,
                         DWORD(tlb_64k[i])*M_64K,
                         vm_start+       i*M_64K,
                         prot);

   DWORD(tlb_64k[i]):=DWORD(tlb_64k[i]) or DWORD(mflags);

   Inc(i);
  end;

 mtx_unlock(bp^.lock);

 Result:=0;
end;

procedure blockpool_obj_unmap(map        :vm_map_t;
                              obj        :vm_map_object;
                              vm_start   :QWORD;
                              block_start:DWORD;
                              block___end:DWORD); public;
var
 pmap:pmap_t;
 bp:p_blockpool;
 tlb_64k:p_dmem_block;
begin
 pmap:=map^.pmap;

 bp:=obj^.handle;
 tlb_64k:=obj^.un_pager.bpl.tlb_64k;

 //free region
 pmap_remove(pmap,nil,
             vm_start+block_start*M_64K,
             vm_start+block___end*M_64K
            );

 mtx_lock(bp^.lock);

  //clear
  bp^.decommit(@tlb_64k[block_start],(block___end-block_start));

 mtx_unlock(bp^.lock);
end;

function sys_blockpool_map(addr:Pointer;len:QWORD;mtype,prot,flags:DWORD):Integer;
var
 map  :vm_map_t;
 entry:vm_map_entry_t;
 obj  :vm_map_object;
 start:QWORD;
 block:DWORD;
begin
 Result:=EINVAL;

 map:=p_proc.p_vmspace;

 if (map^.header.start <= QWORD(addr)) and
    (WORD(len)=0) and
    (WORD(addr)=0) and
    (QWORD(addr) < map^.header.__end) and
    (len <= (map^.header.__end - QWORD(addr))) and
    (DWORD(mtype) < 11) and
    (($409 shr (mtype and $1f) and 1)<>0) and
    ((prot and $ffffffcc)=0) and
    (flags=0) then
 begin
  //
 end else
 begin
  Exit(EINVAL);
 end;

 if (mtype=SCE_KERNEL_WB_GARLIC) then
 begin
  if ((prot and $22)<>0) then
  begin
   Exit(EACCES);
  end;
 end;

 Result:=EINVAL;

 vm_map_lock(map);

  if vm_map_lookup_entry(map,QWORD(addr),@entry) then
  begin

   if ((entry^.eflags and MAP_ENTRY_IS_SUB_MAP)=0) then
   begin
    obj:=entry^.vm_obj;

    if (obj<>nil) then
    if (obj^.otype=OBJT_BLOCKPOOL) then
    if (len <= (entry^.__end - QWORD(addr))) then
    begin

     if (len=0) then
     begin
      Result:=0;
     end else
     begin
      start:=entry^.start;
      block:=(QWORD(addr) - start) div M_64K;

      Result:=kern_blockpool_map(map,obj,
                                 start,
                                 block,
                                 (len div M_64K) + block,
                                 prot,
                                 mtype);
     end;

    end; //obj

   end; //MAP_ENTRY_IS_SUB_MAP

  end; //vm_map_lookup_entry

 vm_map_unlock(map);

end;

function sys_blockpool_unmap(addr:Pointer;len:QWORD;flags:DWORD):Integer;
var
 map  :vm_map_t;
 entry:vm_map_entry_t;
 obj  :vm_map_object;
 start:QWORD;
 block:DWORD;
begin
 Result:=EINVAL;

 map:=p_proc.p_vmspace;


 if (map^.header.start <= QWORD(addr)) and
    (WORD(len)=0) and
    (WORD(addr)=0) and
    (QWORD(addr) < map^.header.__end) and
    (flags=0) and
    (len <= (map^.header.__end - QWORD(addr))) then
 begin
  //
 end else
 begin
  Exit(EINVAL);
 end;

 Result:=EINVAL;

 vm_map_lock(map);

  if vm_map_lookup_entry(map,QWORD(addr),@entry) then
  begin

   if ((entry^.eflags and MAP_ENTRY_IS_SUB_MAP)=0) then
   begin
    obj:=entry^.vm_obj;

    if (obj<>nil) then
    if (obj^.otype=OBJT_BLOCKPOOL) then
    if (len <= (entry^.__end - QWORD(addr))) then
    begin

     Result:=0;

     if (len<>0) then
     begin
      start:=entry^.start;
      block:=(QWORD(addr) - start) div M_64K;

      blockpool_obj_unmap(map,obj,
                          start,
                          block,
                          (len div M_64K) + block
                         );
     end;

    end; //obj

   end; //MAP_ENTRY_IS_SUB_MAP

  end; //vm_map_lookup_entry

 vm_map_unlock(map);

end;

type
 p_blockpool_expand=^t_blockpool_expand;
 t_blockpool_expand=packed record
  len  :QWORD;
  start:QWORD; // in/out
  __end:QWORD;
  flags:DWORD;
  align:Integer;
 end;

 p_blockpool_stats=^t_blockpool_stats;
 t_blockpool_stats=packed record
  availableFlushedBlocks:DWORD;
  availableCachedBlocks :DWORD;
  allocatedFlushedBlocks:DWORD;
  allocatedCachedBlocks :DWORD;
 end;

function blockpool_expand(bp:p_blockpool;data:p_blockpool_expand):Integer;
var
 len  :QWORD;
 flags:DWORD;
 start:QWORD;
 __end:QWORD;
 align:QWORD;
 addr :QWORD;
begin
 len  :=data^.len;
 flags:=data^.flags;

 if (WORD(len)<>0) or
    ((flags and $1f000000)<>flags) or
    ((flags<>0) and (flags < $10000000)) then
 begin
  Exit(EINVAL);
 end;

 if (len=0) then
 begin
  Exit(0);
 end else
 if (Int64(len) < 0) then
 begin
  Exit(ENOMEM);
 end;

 if (flags=0) then
 begin
  align:=16;
 end else
 begin
  align:=(flags shr 24);
 end;
 align:=QWORD(1) shl (align and $3f);

 if (Int64(data^.start) <= Int64(bp^.start_dmem)) then
 begin
  start:=bp^.start_dmem;
 end else
 begin
  start:=data^.start;
 end;

 if (Int64(data^.__end) >= Int64(bp^.__end_dmem)) then
 begin
   __end:=bp^.__end_dmem;
 end else
 begin
  __end:=data^.__end;
 end;

 addr:=0;
 Result:=dmem_map_alloc(bp^.dmap^.dmem,start,__end,len,align,SCE_KERNEL_WC_GARLIC,acl_blockpool,addr);

 if (Result<>0) then
 begin
  if (Result=EAGAIN) then
  begin
   Result:=ENOMEM;
  end;
  Exit();
 end;

 //////
 mtx_lock(bp^.lock);

  bp^.Flushed.Fill(
                   (addr)     div M_64K,
                   (addr+len) div M_64K
                  );

 mtx_unlock(bp^.lock);
 //////

 data^.start:=addr;
end;

function blockpool_stats(bp:p_blockpool;data:p_blockpool_stats):Integer;
begin
 data^.availableFlushedBlocks:=bp^.Flushed.availableBlocks;
 data^.availableCachedBlocks :=bp^.Cached.availableBlocks;
 data^.allocatedFlushedBlocks:=bp^.allocatedFlushedBlocks;
 data^.allocatedCachedBlocks :=bp^.allocatedCachedBlocks ;

 Result:=0;
end;

function blockpool_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
begin

 case com of
  $4010a802:
    begin
     //Writeln('sceKernelMemoryPoolGetBlockStats');
     Result:=blockpool_stats(fp^.f_data,data);
    end;
  $c020a801:
    begin
     //Writeln('sceKernelMemoryPoolExpand');
     Result:=blockpool_expand(fp^.f_data,data);
    end;
  else
    Exit(ENOTTY);
 end;

 Result:=0;
end;

function blockpool_stat(fp:p_file;sb:p_stat):Integer;
var
 bp:p_blockpool;
begin
 bp:=fp^.f_data;
 //
 sb^:=Default(t_stat);
 sb^.st_blksize:=$10000;
 sb^.st_mode   :=$b000;
 sb^.st_blocks :=(bp^.Cached.availableBlocks * 2);
 Result:=0;
end;

function blockpool_close(fp:p_file):Integer;
begin
 blockpool_release(fp^.f_data);
 Result:=0;
end;

const
 blockpool_ops:fileops=(
  fo_read    :fo_rdwr_t    (@_enxio);
  fo_write   :fo_rdwr_t    (@_enxio);
  fo_truncate:fo_truncate_t(@_enxio);
  fo_ioctl   :@blockpool_ioctl;
  fo_poll    :fo_poll_t    (@_eopnotsupp);
  fo_kqfilter:fo_kqfilter_t(@_eopnotsupp);
  fo_stat    :@blockpool_stat;
  fo_close   :@blockpool_close;
  fo_chmod   :fo_chmod_t   (@_einval);
  fo_chown   :fo_chown_t   (@_einval);
  fo_flags   :0;
 );

function sys_blockpool_open(flags:Integer):Integer;
var
 td:p_kthread;
 bp:p_blockpool;
 fp:p_file;
 pg:QWORD;
 budget_id:Integer;
 fd:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);
 //0x100000(O_CLOEXEC) | 0x400000(ASLR_FD????)
 if ((flags and $ffafffff)<>0) then Exit(EINVAL);

 //

 budget_id:=p_proc.p_budget_ptype;

 if (bp_budget_reserve(budget_id)<>0) then
 begin
  Exit(EMFILE);
 end;

 bp:=AllocMem(SizeOf(t_blockpool));

 mtx_init(bp^.lock,'bpoolfd');

 pg:=ord((p_proc.p_dmem_pool_id=0) and (budget_id>1));

 bp^.start_dmem:=(pg shl 37);
 bp^.__end_dmem:=(pg shl 38) or $1000000000;
 bp^.dmap      :=@dmem_maps[p_proc.p_dmem_pool_id];
 bp^.budget_id :=budget_id;

 ///

 flags:=flags or FWRITE;

 fd:=0;
 Result:=falloc(@fp,@fd,flags);
 if (Result<>0) then
 begin
  blockpool_free(bp);
  Exit();
 end;

 finit(fp, flags, DTYPE_BLOCKPOOL, blockpool_acqure(bp), @blockpool_ops);

 fdrop(fp);

 td^.td_retval[0]:=fd;
end;

procedure blockpool_free(bp:p_blockpool);
var
 budget_id:Integer;
begin
 budget_id:=bp^.budget_id;
 //
 //free_direct_memory
 FreeMem(bp);
 bp_budget_release(budget_id);
end;



end.

