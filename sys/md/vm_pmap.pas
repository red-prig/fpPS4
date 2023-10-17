unit vm_pmap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 windows,
 vm,
 vmparam,
 sys_vm_object;

{$DEFINE GPU_REMAP}

const
 PAGE_MAP_COUNT   =(QWORD(VM_MAXUSER_ADDRESS) shr PAGE_SHIFT);
 PAGE_MAP_MASK    =PAGE_MAP_COUNT-1;

 PAGE_PROT_EXECUTE=DWORD($80000000);
 PAGE_PROT_WRITE  =DWORD($40000000);
 PAGE_PROT_READ   =DWORD($20000000);

 PAGE_PROT_RW     =PAGE_PROT_READ or PAGE_PROT_WRITE;

 PAGE_PROT_SHIFT  =29;

 PAGE_OFS_MASK    =(1 shl PAGE_PROT_SHIFT)-1; //Possible addressing in 8TB

 //PAGE_BUSY_FLAG   =DWORD($10000000);
 //PAGE_PATCH_FLAG  =DWORD($08000000);

var
 PAGE_MAP:PDWORD=nil;

procedure pmap_mark(start,__end,__off:vm_offset_t;flags:DWORD);
procedure pmap_unmark(start,__end:vm_offset_t);
procedure pmap_mark_flags(start,__end:vm_offset_t;flags:DWORD);
function  pmap_get_raw   (addr:vm_offset_t):DWORD;
function  pmap_get_page  (addr:vm_offset_t):DWORD;
function  pmap_test_cross(addr:vm_offset_t;h:Integer):Boolean;

function  uplift(addr:Pointer):Pointer;

type
 p_pmap=^_pmap;
 _pmap=packed object
  //
 end;

 pmap_t=p_pmap;

function  atop(x:QWORD):DWORD; inline;
function  ptoa(x:DWORD):QWORD; inline;

function  ctob(x:QWORD):QWORD; inline;
function  btoc(x:QWORD):QWORD; inline;

procedure pmap_pinit(pmap:p_pmap);

procedure pmap_align_superpage(obj   :vm_object_t;
                               offset:vm_ooffset_t;
                               addr  :p_vm_offset_t;
                               size  :vm_size_t);

procedure pmap_enter_object(pmap   :pmap_t;
                            start  :vm_offset_t;
                            __end  :vm_offset_t;
                            prot   :vm_prot_t);

procedure pmap_protect(pmap    :pmap_t;
                       start   :vm_offset_t;
                       __end   :vm_offset_t;
                       new_prot:vm_prot_t;
                       old_prot:vm_prot_t);

procedure pmap_madv_free(pmap :pmap_t;
                        start:vm_offset_t;
                        __end:vm_offset_t;
                        prot :vm_prot_t);

procedure pmap_remove(pmap :pmap_t;
                      start:vm_offset_t;
                      __end:vm_offset_t;
                      prot :vm_prot_t);

const
 ICACHE=1; //Flush the instruction cache.
 DCACHE=2; //Write back to memory and invalidate the affected valid cache lines.
 BCACHE=ICACHE or DCACHE;

procedure md_cacheflush(addr:Pointer;nbytes,cache:Integer);

const
 MD_PAGE_SIZE        = 4*1024;
 MD_ALLOC_GRANULARITY=64*1024;

 MD_PROT_NONE=PAGE_NOACCESS;
 MD_PROT_R   =PAGE_READONLY;
 MD_PROT_RW  =PAGE_READWRITE;
 MD_PROT_X   =PAGE_EXECUTE;
 MD_PROT_RX  =PAGE_EXECUTE_READ;
 MD_PROT_RWX =PAGE_EXECUTE_READWRITE;

function md_mmap (vaddr:Pointer;vlen:QWORD;prot:Integer):Pointer;
function md_unmap(vaddr:Pointer;vlen:QWORD):Integer;

implementation

function atop(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function ptoa(x:DWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function ctob(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function btoc(x:QWORD):QWORD; inline;
begin
 Result:=((x+PAGE_MASK) shr PAGE_SHIFT);
end;

function md_alloc_page(x:QWORD):QWORD; inline;
begin
 Result:=x and (not (MD_ALLOC_GRANULARITY-1));
end;

function md_up_page(x:QWORD):QWORD; inline;
begin
 Result:=(x+(MD_PAGE_SIZE-1)) and (not (MD_PAGE_SIZE-1));
end;

function md_mmap(vaddr:Pointer;vlen:QWORD;prot:Integer):Pointer;
var
 base:Pointer;
 size:QWORD;
 r:Integer;
begin
 Result:=nil;

 base:=Pointer(md_alloc_page(QWORD(vaddr)));
 size:=md_up_page(vlen);

 r:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @base,
     0,
     @size,
     MEM_COMMIT or MEM_RESERVE,
     prot
    );

 if (r=0) then
 begin
  Result:=base;
 end;
end;

function md_unmap(vaddr:Pointer;vlen:QWORD):Integer;
var
 base:Pointer;
 size:QWORD;
 r:Integer;
begin
 Result:=0;

 base:=Pointer(md_alloc_page(QWORD(vaddr)));
 size:=0;

 r:=NtFreeVirtualMemory(
     NtCurrentProcess,
     @base,
     @size,
     MEM_RELEASE
    );

 Result:=r;
end;

procedure pmap_pinit(pmap:p_pmap);
var
 base:Pointer;
 size:QWORD;
 i,r:Integer;
begin

 if Length(pmap_mem)<>0 then
 begin
  For i:=0 to High(pmap_mem) do
  begin
   base:=Pointer(md_alloc_page(pmap_mem[i].start));
   size:=trunc_page(pmap_mem[i].__end-pmap_mem[i].start);

   r:=NtAllocateVirtualMemory(
       NtCurrentProcess,
       @base,
       0,
       @size,
       MEM_RESERVE,
       PAGE_NOACCESS
      );

   if (r<>0) then
   begin
    Writeln('failed NtAllocateVirtualMemory(',HexStr(base),',',HexStr(base+size),'):',HexStr(r,8));
    //STATUS_COMMITMENT_LIMIT = $C000012D
    Assert(false,'pmap_init');
   end;

  end;
 end;

 PAGE_MAP:=md_mmap(nil,PAGE_MAP_COUNT*SizeOf(DWORD),MD_PROT_RW);

 Assert(PAGE_MAP<>nil,'pmap_init');

 //Mapping to internal memory, isolate TODO
 if Length(exclude_mem)<>0 then
 begin
  For i:=0 to High(exclude_mem) do
  begin
   pmap_mark(exclude_mem[i].start,exclude_mem[i].__end,exclude_mem[i].start,0);
  end;
 end;

end;

{
* Increase the starting virtual address of the given mapping if a
* different alignment might result in more superpage mappings.
}
procedure pmap_align_superpage(obj   :vm_object_t;
                               offset:vm_ooffset_t;
                               addr  :p_vm_offset_t;
                               size  :vm_size_t);
var
 superpage_offset:vm_offset_t;
begin
 if (size < NBPDR) then Exit;
 if (obj<>nil) then
 if ((obj^.flags and OBJ_COLORED)<>0) then
 begin
  offset:=offset+ptoa(obj^.pg_color);
 end;
 superpage_offset:=offset and PDRMASK;
 if (size - ((NBPDR - superpage_offset) and PDRMASK) < NBPDR) or
    ((addr^ and PDRMASK)=superpage_offset) then
 begin
  Exit;
 end;
 if ((addr^ and PDRMASK) < superpage_offset) then
 begin
  addr^:=(addr^ and (not PDRMASK)) + superpage_offset
 end else
 begin
  addr^:=((addr^ + PDRMASK) and (not PDRMASK)) + superpage_offset;
 end;
end;

//PAGE_MAP

function IDX_TO_OFF(x:DWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

procedure pmap_mark(start,__end,__off:vm_offset_t;flags:DWORD);
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 __off:=OFF_TO_IDX(__off);
 while (start<__end) do
 begin
  PAGE_MAP[start and PAGE_MAP_MASK]:=(__off and PAGE_OFS_MASK) or flags;
  Inc(__off);
  Inc(start);
 end;
 WriteBarrier;
end;

procedure pmap_unmark(start,__end:vm_offset_t);
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 while (start<__end) do
 begin
  PAGE_MAP[start and PAGE_MAP_MASK]:=0;
  Inc(start);
 end;
 WriteBarrier;
end;

procedure pmap_mark_flags(start,__end:vm_offset_t;flags:DWORD);
var
 i:vm_offset_t;
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 while (start<__end) do
 begin
  i:=start and PAGE_MAP_MASK;
  PAGE_MAP[i]:=PAGE_MAP[i] or flags;
  Inc(start);
 end;
 WriteBarrier;
end;

function pmap_get_raw(addr:vm_offset_t):DWORD;
begin
 addr:=OFF_TO_IDX(addr);
 addr:=addr and PAGE_MAP_MASK;
 Result:=PAGE_MAP[addr];
end;

function pmap_get_page(addr:vm_offset_t):DWORD;
begin
 addr:=OFF_TO_IDX(addr);
 addr:=addr and PAGE_MAP_MASK;
 Result:=PAGE_MAP[addr];
 Result:=Result and PAGE_OFS_MASK;
end;

function pmap_test_cross(addr:vm_offset_t;h:Integer):Boolean;
var
 page1,page2:DWORD;
begin
 page1:=OFF_TO_IDX(addr  ) and PAGE_MAP_MASK;
 page2:=OFF_TO_IDX(addr+h) and PAGE_MAP_MASK;
 if (page1=page2) then
 begin
  Result:=False;
 end else
 begin
  page1:=PAGE_MAP[page1] and PAGE_OFS_MASK;
  page2:=PAGE_MAP[page2] and PAGE_OFS_MASK;
  Result:=(page1<>page2);
 end;
end;

//rax,rdi,rsi
function uplift(addr:Pointer):Pointer; assembler; nostackframe;
label
 _exit;
asm
 //low addr (rsi)
 mov %rdi,%rsi
 and PAGE_MASK,%rsi
 //high addr (rdi)
 shr PAGE_SHIFT   ,%rdi
 and PAGE_MAP_MASK,%rdi
 //uplift (rdi)
 mov PAGE_MAP,%rax
 mov (%rax,%rdi,4),%edi
 //filter (rdi)
 and PAGE_OFS_MASK,%rdi
 jz _exit
 //combine (rdi|rsi)
 shl PAGE_SHIFT,%rdi
 or  %rsi,%rdi
 //result
 _exit:
 mov %rdi,%rax
end;

const
 VM_RWX=VM_PROT_READ or VM_PROT_WRITE or VM_PROT_EXECUTE;

 wprots:array[0..7] of Byte=(
  PAGE_NOACCESS         ,//___
  PAGE_READONLY         ,//__R
  PAGE_READWRITE        ,//_W_
  PAGE_READWRITE        ,//_WR
  PAGE_EXECUTE          ,//X__
  PAGE_EXECUTE_READ     ,//X_R
  PAGE_EXECUTE_READWRITE,//XW_
  PAGE_EXECUTE_READWRITE //XWR
 );

{
 * Maps a sequence of resident pages belonging to the same object.
 * The sequence begins with the given page m_start.  This page is
 * mapped at the given virtual address start.  Each subsequent page is
 * mapped at a virtual address that is offset from start by the same
 * amount as the page is offset from m_start within the object.  The
 * last page in the sequence is the page with the largest offset from
 * m_start that can be mapped at a virtual address less than the given
 * virtual address end.  Not every virtual page between start and end
 * is mapped; only those for which a resident page exists with the
 * corresponding offset from m_start are mapped.
}
procedure pmap_enter_object(pmap   :pmap_t;
                            start  :vm_offset_t;
                            __end  :vm_offset_t;
                            prot   :vm_prot_t);
var
 base:Pointer;
 size:QWORD;
 r:Integer;
begin
 Writeln('pmap_enter_object:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));

 base:=Pointer(trunc_page(start));
 size:=trunc_page(__end-start);

 {$IFDEF GPU_REMAP}
 if is_gpu(prot) then
 begin
  //shift
  base:=base+VM_MIN_GPU_ADDRESS;
  prot:=prot or ((prot and VM_PROT_GPU_ALL) shr 4);
  Writeln('pmap_enter_gpuobj:',HexStr(QWORD(base),11),':',HexStr(QWORD(base)+(__end-start),11),':',HexStr(prot,2));
 end;
 {$ENDIF}

 r:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @base,
     0,
     @size,
     MEM_COMMIT,
     wprots[prot and VM_RWX]
    );

 if (r<>0) then
 begin
  Writeln('failed NtAllocateVirtualMemory:',HexStr(r,8));
  Assert(false,'pmap_enter_object');
 end;

 pmap_mark(start,__end,QWORD(base),(prot and VM_RWX) shl PAGE_PROT_SHIFT);
end;

procedure pmap_move(pmap    :pmap_t;
                    start   :vm_offset_t;
                    ofs_old :vm_offset_t;
                    ofs_new :vm_offset_t;
                    size    :vm_offset_t;
                    new_prot:vm_prot_t);
var
 r,old:Integer;
begin
 old:=0;

 //pmap_mark_flags(start,start+size,PAGE_BUSY_FLAG);

 //set old to readonly
 r:=NtProtectVirtualMemory(
     NtCurrentProcess,
     @ofs_old,
     @size,
     PAGE_READONLY,
     @old
    );

 if (r<>0) then
 begin
  Writeln('failed NtProtectVirtualMemory:',HexStr(r,8));
  Assert(false,'pmap_move');
 end;

 //alloc new
 r:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @ofs_new,
     0,
     @size,
     MEM_COMMIT,
     PAGE_READWRITE
    );

 if (r<>0) then
 begin
  Writeln('failed NtAllocateVirtualMemory:',HexStr(r,8));
  Assert(false,'pmap_move');
 end;

 //move data
 Move(Pointer(ofs_old)^,Pointer(ofs_new)^,size);

 //prot new
 if (PAGE_READWRITE<>wprots[new_prot and VM_RWX]) then
 begin
  r:=NtProtectVirtualMemory(
      NtCurrentProcess,
      @ofs_new,
      @size,
      wprots[new_prot and VM_RWX],
      @old
     );

  if (r<>0) then
  begin
   Writeln('failed NtProtectVirtualMemory:',HexStr(r,8));
   Assert(false,'pmap_move');
  end;
 end;

 pmap_mark(start,start+size,ofs_new,(new_prot and VM_RWX) shl PAGE_PROT_SHIFT);

 //free old
 r:=NtFreeVirtualMemory(
     NtCurrentProcess,
     @ofs_old,
     @size,
     MEM_DECOMMIT
    );

 if (r<>0) then
 begin
  Writeln('failed NtFreeVirtualMemory:',HexStr(r,8));
  Assert(false,'pmap_move');
 end;
end;

procedure pmap_protect(pmap    :pmap_t;
                       start   :vm_offset_t;
                       __end   :vm_offset_t;
                       new_prot:vm_prot_t;
                       old_prot:vm_prot_t);
var
 base_new:Pointer;
 base_old:Pointer;
 size:QWORD;
 r,old:Integer;
begin
 Writeln('pmap_protect:',HexStr(start,11),':',HexStr(__end,11),':new:',HexStr(new_prot,2),':old:',HexStr(old_prot,2));

 base_new:=Pointer(trunc_page(start));
 base_old:=base_new;
 size:=trunc_page(__end-start);
 old:=0;

 {$IFDEF GPU_REMAP}
 if (is_gpu(new_prot)<>is_gpu(old_prot)) then
 begin
  //realloc

  if is_gpu(new_prot) then
  begin
   //shift
   base_new:=base_new+VM_MIN_GPU_ADDRESS;
   new_prot:=new_prot or ((new_prot and VM_PROT_GPU_ALL) shr 4);
  end;

  if is_gpu(old_prot) then
  begin
   //shift
   base_old:=base_old+VM_MIN_GPU_ADDRESS;
  end;

  pmap_move(pmap,start,vm_offset_t(base_old),vm_offset_t(base_new),size,new_prot);

  Exit;
 end;
 {$ENDIF}

 {$IFDEF GPU_REMAP}
 if is_gpu(new_prot) then
 begin
  //shift
  base_new:=base_new+VM_MIN_GPU_ADDRESS;
 end;
 {$ENDIF}

 r:=NtProtectVirtualMemory(
     NtCurrentProcess,
     @base_new,
     @size,
     wprots[new_prot and VM_RWX],
     @old
    );

 if (r<>0) then
 begin
  Writeln('failed NtProtectVirtualMemory:',HexStr(r,8));
  Assert(false,'pmap_protect');
 end;

 pmap_mark(start,__end,QWORD(base_new),(new_prot and VM_RWX) shl PAGE_PROT_SHIFT);
end;

procedure pmap_madv_free(pmap :pmap_t;
                        start:vm_offset_t;
                        __end:vm_offset_t;
                        prot :vm_prot_t);
var
 base:Pointer;
 size:QWORD;
 r:Integer;
begin
 Writeln('pmap_madv_free:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));

 //dont reset gpu mem
 if is_gpu(prot) then Exit;

 base:=Pointer(trunc_page(start));
 size:=trunc_page(__end-start);

 r:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @base,
     0,
     @size,
     MEM_RESET,
     wprots[prot and VM_RWX]
    );

 if (r<>0) then
 begin
  Writeln('failed NtAllocateVirtualMemory:',HexStr(r,8));
  Assert(false,'pmap_madv_free');
 end;
end;

procedure pmap_remove(pmap :pmap_t;
                      start:vm_offset_t;
                      __end:vm_offset_t;
                      prot :vm_prot_t);
var
 base:Pointer;
 size:QWORD;
 r:Integer;
begin
 Writeln('pmap_remove:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));

 base:=Pointer(trunc_page(start));
 size:=trunc_page(__end-start);

 pmap_unmark(start,__end);

 {$IFDEF GPU_REMAP}
 if is_gpu(prot) then
 begin
  //shift
  base:=base+VM_MIN_GPU_ADDRESS;
 end;
 {$ENDIF}

 r:=NtFreeVirtualMemory(
     NtCurrentProcess,
     @base,
     @size,
     MEM_DECOMMIT
    );

 if (r<>0) then
 begin
  Writeln('failed NtFreeVirtualMemory:',HexStr(r,8));
  Assert(false,'pmap_remove');
 end;
end;

procedure md_cacheflush(addr:Pointer;nbytes,cache:Integer);
begin
 if ((cache and ICACHE)<>0) then
 begin
  FlushInstructionCache(NtCurrentProcess,addr,nbytes);
 end;
 if ((cache and DCACHE)<>0) then
 begin
  FlushViewOfFile(addr,nbytes);
 end;
end;


end.

