unit kern_blockpool;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vmparam,
 sys_vm_object,
 kern_mtx,
 kern_dmem;

function sys_blockpool_open(flags:Integer):Integer;

type
 t_dmem_bits=object
  const
   M_QWORD_COUNT=(VM_DMEM_SIZE+(64*1024*64)-1) div (64*1024*64);
   M_BACKT_BITS =(M_QWORD_COUNT+63) div 64;
   M_QWORD_ALIGN=M_BACKT_BITS*64;
   M_BACKT_COUNT=(M_QWORD_ALIGN+63) div 64;
  type
   //t_byte=packed record
   // val:Byte;
   //end;
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
  procedure flush();
  function  commit(count,onion,writeback:DWORD;buf:PDWORD):Integer;
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
  Exit(12);
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
    if (s=-1) then goto _repeat;

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
     if (s=-1) then goto _repeat;

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
    if (s=-1) then goto _repeat;

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
    if (s=-1) then goto _repeat;

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
     if (s=-1) then goto _repeat;

     Cached.Commit(s);

     buf[0]:=s;
     Inc(buf);
     Dec(Count);
    end;

    goto _repeat;
   end;

   // flushed
   while (count<>0) do
   begin
    s:=Flushed.FindFirst;
    if (s=-1) then goto _repeat;

    Flushed.Commit(s);

    buf[0]:=s;
    Inc(buf);
    Dec(Count);
   end;

   // cached
   while (count<>0) do
   begin
    s:=Cached.FindFirst;
    if (s=-1) then goto _repeat;

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

const
 M_1GB=(1024*1024*1024);

function blockpool_pager_alloc(handle:Pointer;size:QWORD):vm_object_t;
var
 bp:p_blockpool;
 tlb_cnt:DWORD;
 tlb_1gb:PDWORD;
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

 tlb_cnt:=(size+M_1GB-1) div M_1GB;
 tlb_1gb:=AllocMem(tlb_cnt);

 if (tlb_1gb=nil) then
 begin
  Exit(nil);
 end;

 if bp^.commit(tlb_cnt,1,1,tlb_1gb)<>0 then
 begin
  FreeMem(tlb_1gb);
  Exit(nil);
 end;

 Result:=vm_object_allocate(OBJT_BLOCKPOOL,OFF_TO_IDX(size));
 if (Result=nil) then
 begin
  FreeMem(tlb_1gb);
  Exit(nil);
 end;

 Result^.un_pager.bpl.tlb_1gb:=tlb_1gb;
 Result^.handle:=blockpool_acqure(bp);
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
  bp^.Cached.Commit(tlb_1gb[i]);
 end;
 bp^.allocatedCachedBlocks:=bp^.allocatedCachedBlocks-tlb_cnt;

 FreeMem(tlb_1gb);

 blockpool_release(bp);

 obj^.un_pager.bpl.tlb_1gb:=nil;
 obj^.handle:=nil;
 obj^.otype :=OBJT_DEAD;
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
 Result:=dmem_map_alloc(bp^.dmap^.dmem,start,__end,len,align,SCE_KERNEL_WC_GARLIC,addr);

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
                   (addr)     div (64*1024),
                   (addr+len) div (64*1024)
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

