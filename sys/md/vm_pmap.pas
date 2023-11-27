unit vm_pmap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 vmparam,
 sys_vm_object;

{$DEFINE GPU_REMAP}

const
 PAGE_MAP_COUNT   =(QWORD(VM_MAXUSER_ADDRESS) shr PAGE_SHIFT);
 PAGE_MAP_MASK    =PAGE_MAP_COUNT-1;

 PAGE_PROT_EXECUTE=VM_PROT_EXECUTE;
 PAGE_PROT_WRITE  =VM_PROT_WRITE;
 PAGE_PROT_READ   =VM_PROT_READ;

 PAGE_PROT_RW     =PAGE_PROT_READ or PAGE_PROT_WRITE;

 //PAGE_BUSY_FLAG   =DWORD($10000000);
 //PAGE_PATCH_FLAG  =DWORD($08000000);

var
 PAGE_MAP   :PDWORD=nil;
 PAGE_PROT  :PBYTE =nil;

procedure pmap_mark      (start,__end,__off:vm_offset_t;prots:Byte);
procedure pmap_unmark    (start,__end:vm_offset_t);
function  pmap_get_prot  (addr:vm_offset_t):Byte;
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
                            obj    :vm_object_t;
                            offset :vm_ooffset_t;
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

implementation

uses
 md_map;

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

procedure pmap_pinit(pmap:p_pmap);
var
 base:Pointer;
 size:QWORD;
 prot:QWORD;
 i,r:Integer;
begin

 if Length(pmap_mem)<>0 then
 begin
  For i:=0 to High(pmap_mem) do
  begin
   base:=Pointer(pmap_mem[i].start);
   size:=pmap_mem[i].__end-pmap_mem[i].start;

   r:=md_reserve(base,size);

   if (r<>0) then
   begin
    Writeln('failed md_reserve(',HexStr(base),',',HexStr(base+size),'):',HexStr(r,8));
    //STATUS_COMMITMENT_LIMIT = $C000012D
    Assert(false,'pmap_init');
   end;

  end;
 end;

 PAGE_MAP:=nil;
 size:=PAGE_MAP_COUNT*SizeOf(DWORD);

 prot:=size;
 size:=size+PAGE_MAP_COUNT;

 r:=md_mmap(PAGE_MAP,size,MD_PROT_RW);

 if (r<>0) then
 begin
  Writeln('failed md_mmap(',HexStr(PAGE_MAP),',',HexStr(PAGE_MAP+size),'):',HexStr(r,8));
  Assert(false,'pmap_init');
 end;

 PAGE_PROT:=Pointer(PAGE_MAP)+prot;

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

procedure pmap_mark(start,__end,__off:vm_offset_t;prots:Byte);
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 __off:=OFF_TO_IDX(__off);
 while (start<__end) do
 begin
  PAGE_MAP [start and PAGE_MAP_MASK]:=__off;
  PAGE_PROT[start and PAGE_MAP_MASK]:=prots;
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
  PAGE_MAP [start and PAGE_MAP_MASK]:=0;
  PAGE_PROT[start and PAGE_MAP_MASK]:=0;
  Inc(start);
 end;
 WriteBarrier;
end;

function pmap_get_prot(addr:vm_offset_t):Byte;
begin
 addr:=OFF_TO_IDX(addr);
 addr:=addr and PAGE_MAP_MASK;
 Result:=PAGE_PROT[addr];
end;

function pmap_get_page(addr:vm_offset_t):DWORD;
begin
 addr:=OFF_TO_IDX(addr);
 addr:=addr and PAGE_MAP_MASK;
 Result:=PAGE_MAP[addr];
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
  Result:=(page1<>page2);
 end;
end;

//rax,rdi,rsi
function uplift(addr:Pointer):Pointer; assembler; nostackframe;
const
 VM_MAX_D=VM_MAXUSER_ADDRESS shr 32;
label
 _exit;
asm
 //
 mov VM_MAX_D,%rsi
 shl  $32,%rsi
 cmp %rsi,%rdi
 ja _exit
 //low addr (rsi)
 mov %rdi,%rsi
 and PAGE_MASK,%rsi
 //high addr (rdi)
 shr PAGE_SHIFT   ,%rdi
 and PAGE_MAP_MASK,%rdi
 //uplift (rdi)
 mov PAGE_MAP(%rip),%rax
 mov (%rax,%rdi,4),%edi
 //filter (rdi)
 test %rdi,%rdi
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
  MD_PROT_NONE,//___
  MD_PROT_R   ,//__R
  MD_PROT_W   ,//_W_
  MD_PROT_RW  ,//_WR
  MD_PROT_X   ,//X__
  MD_PROT_RX  ,//X_R
  MD_PROT_WX  ,//XW_
  MD_PROT_RWX  //XWR
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
                            obj    :vm_object_t;
                            offset :vm_ooffset_t;
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

 r:=md_enter(base,size,wprots[prot and VM_RWX]);

 if (r<>0) then
 begin
  Writeln('failed md_enter:',HexStr(r,8));
  Assert(false,'pmap_enter_object');
 end;

 pmap_mark(start,__end,QWORD(base),(prot and VM_RWX));
end;

procedure pmap_move(pmap    :pmap_t;
                    start   :vm_offset_t;
                    ofs_old :vm_offset_t;
                    ofs_new :vm_offset_t;
                    size    :vm_offset_t;
                    new_prot:vm_prot_t);
var
 r:Integer;
begin
 //pmap_mark_flags(start,start+size,PAGE_BUSY_FLAG);

 //set old to readonly
 r:=md_protect(Pointer(ofs_old),size,MD_PROT_R);

 if (r<>0) then
 begin
  Writeln('failed md_protect:',HexStr(r,8));
  Assert(false,'pmap_move');
 end;

 //alloc new
 r:=md_enter(Pointer(ofs_new),size,MD_PROT_RW);

 if (r<>0) then
 begin
  Writeln('failed md_enter:',HexStr(r,8));
  Assert(false,'pmap_move');
 end;

 //move data
 Move(Pointer(ofs_old)^,Pointer(ofs_new)^,size);

 //prot new
 if (MD_PROT_RW<>wprots[new_prot and VM_RWX]) then
 begin
  r:=md_protect(Pointer(ofs_new),size,wprots[new_prot and VM_RWX]);

  if (r<>0) then
  begin
   Writeln('failed md_protect:',HexStr(r,8));
   Assert(false,'pmap_move');
  end;
 end;

 pmap_mark(start,start+size,ofs_new,(new_prot and VM_RWX));

 //free old
 r:=md_remove(Pointer(ofs_old),size);

 if (r<>0) then
 begin
  Writeln('failed md_remove:',HexStr(r,8));
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
 r:Integer;
begin
 Writeln('pmap_protect:',HexStr(start,11),':',HexStr(__end,11),':new:',HexStr(new_prot,2),':old:',HexStr(old_prot,2));

 base_new:=Pointer(trunc_page(start));
 base_old:=base_new;
 size:=trunc_page(__end-start);

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

 r:=md_protect(base_new,size,wprots[new_prot and VM_RWX]);

 if (r<>0) then
 begin
  Writeln('failed md_protect:',HexStr(r,8));
  Assert(false,'pmap_protect');
 end;

 pmap_mark(start,__end,QWORD(base_new),(new_prot and VM_RWX));
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

 r:=md_reset(base,size,wprots[prot and VM_RWX]);

 if (r<>0) then
 begin
  Writeln('failed md_reset:',HexStr(r,8));
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

 r:=md_remove(base,size);

 if (r<>0) then
 begin
  Writeln('failed md_remove:',HexStr(r,8));
  Assert(false,'pmap_remove');
 end;
end;


end.

