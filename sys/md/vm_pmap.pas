unit vm_pmap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 windows,
 vm,
 vmparam,
 vm_object;

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

procedure pmap_pinit(maps:p_pmap);

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

procedure pmap_pinit(maps:p_pmap);
var
 base:Pointer;
 size:QWORD;
 i,r:Integer;
begin

 if Length(pmap_mem)<>0 then
 begin
  For i:=0 to High(pmap_mem) do
  begin
   base:=Pointer(trunc_page(pmap_mem[i].start));
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
  Exit;
 if ((addr^ and PDRMASK) < superpage_offset) then
  addr^:=(addr^ and (not PDRMASK)) + superpage_offset
 else
  addr^:=((addr^ + PDRMASK) and (not PDRMASK)) + superpage_offset;
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

 if is_gpu(prot) then
 begin
  //shift
  base:=base+VM_MIN_GPU_ADDRESS;
  prot:=prot or ((prot and VM_PROT_GPU_ALL) shr 4);
 end;

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
 Writeln('pmap_protect:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(new_prot,2),':',HexStr(old_prot,2));

 base_new:=Pointer(trunc_page(start));
 base_old:=base_new;
 size:=trunc_page(__end-start);
 old:=0;

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

  //set old to readonly
  r:=NtProtectVirtualMemory(
      NtCurrentProcess,
      @base_old,
      @size,
      PAGE_READONLY,
      @old
     );

  if (r<>0) then
  begin
   Writeln('failed NtProtectVirtualMemory:',HexStr(r,8));
   Assert(false,'pmap_protect');
  end;

  //alloc new
  r:=NtAllocateVirtualMemory(
      NtCurrentProcess,
      @base_new,
      0,
      @size,
      MEM_COMMIT,
      PAGE_READWRITE
     );

  if (r<>0) then
  begin
   Writeln('failed NtAllocateVirtualMemory:',HexStr(r,8));
   Assert(false,'pmap_protect');
  end;

  //move data
  Move(base_old^,base_new^,size);

  //prot new
  if (PAGE_READWRITE<>wprots[new_prot and VM_RWX]) then
  begin
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
  end;

  //free old
  r:=NtFreeVirtualMemory(
      NtCurrentProcess,
      @base_old,
      @size,
      MEM_DECOMMIT
     );

  if (r<>0) then
  begin
   Writeln('failed NtFreeVirtualMemory:',HexStr(r,8));
   Assert(false,'pmap_protect');
  end;

 end else
 begin
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
 end;
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

 if is_gpu(prot) then
 begin
  //shift
  base:=base+VM_MIN_GPU_ADDRESS;
 end;

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

