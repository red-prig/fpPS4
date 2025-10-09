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
 p_blockpool=^t_blockpool;
 t_blockpool=record
  lock                  :mtx;
  start_dmem            :QWORD;
  __end_dmem            :QWORD;
  dmap                  :p_dmem_obj;
  budget_id             :Integer;
  refs                  :DWORD;
  //
  availableCachedBlocks :DWORD;
  availableFlushedBlocks:DWORD;
  allocatedCachedBlocks :DWORD;
  allocatedFlushedBlocks:DWORD;
  //
  dmem_bits:array[0..(VM_DMEM_SIZE div (64*1024*8))-1] of Byte;
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

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function blockpool_pager_alloc(handle:Pointer;size:QWORD):vm_object_t;
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

 Result:=vm_object_allocate(OBJT_BLOCKPOOL,OFF_TO_IDX(size));
 if (Result=nil) then Exit;

 /////////////

 Result^.handle:=blockpool_acqure(handle);
end;

procedure blockpool_pager_dealloc(obj:vm_object_t);
begin
 /////////////

 //bp->allocatedCachedBlocks = bp->allocatedCachedBlocks + -1;
 //bp->availableCachedBlocks = bp->availableCachedBlocks + 1;

 blockpool_release(obj^.handle);

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

function blockpool_expand(bp:p_blockpool;data:p_blockpool_expand):Integer;
type
 t_byte=packed record
  val:Byte;
 end;
var
 len  :QWORD;
 flags:DWORD;
 start:QWORD;
 __end:QWORD;
 align:QWORD;
 addr :QWORD;
 blocks:DWORD;
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
 align:=1 shl (align and $3f);

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
 blocks:=(len shr 16);
 mtx_lock(bp^.lock);

  bp^.availableFlushedBlocks:=bp^.availableFlushedBlocks + blocks;

  //

  start:=addr       div (64*1024);
  __end:=(addr+len) div (64*1024);

  while (start<__end) do
  begin

   with t_byte(bp^.dmem_bits[start div 8]) do
   begin
    val:=val or (1 shl (start mod 8));
   end;

   start:=start+1;
  end;

  //

 mtx_unlock(bp^.lock);
 //////

 data^.start:=addr;
end;

function blockpool_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
begin

 case com of
  $4010a802:
    begin
     Writeln('sceKernelMemoryPoolGetBlockStats');
     Assert(False);
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
 sb^.st_blocks :=(bp^.availableCachedBlocks * 2);
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

