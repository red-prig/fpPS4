unit md_map;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 windows;

const
 MD_PAGE_SIZE        = 4*1024;
 MD_ALLOC_GRANULARITY=64*1024;

 MD_PROT_NONE=PAGE_NOACCESS;
 MD_PROT_R   =PAGE_READONLY;
 MD_PROT_W   =PAGE_READWRITE;
 MD_PROT_RW  =PAGE_READWRITE;
 MD_PROT_X   =PAGE_EXECUTE;
 MD_PROT_RX  =PAGE_EXECUTE_READ;
 MD_PROT_WX  =PAGE_EXECUTE_READWRITE;
 MD_PROT_RWX =PAGE_EXECUTE_READWRITE;

function md_reserve(var base:Pointer;size:QWORD):Integer;
function md_enter  (base:Pointer;size:QWORD;prot:Integer):Integer;
function md_protect(base:Pointer;size:QWORD;prot:Integer):Integer;
function md_remove (base:Pointer;size:QWORD):Integer;
function md_reset  (base:Pointer;size:QWORD;prot:Integer):Integer;

function md_mmap   (var base:Pointer;size:QWORD;prot:Integer):Integer;
function md_unmap  (base:Pointer;size:QWORD):Integer;

function md_file_mmap (handle:THandle;var base:Pointer;offset,size:QWORD;prot:Integer):Integer;
function md_file_unmap(base:Pointer;size:QWORD):Integer;

const
 ICACHE=1; //Flush the instruction cache.
 DCACHE=2; //Write back to memory and invalidate the affected valid cache lines.
 BCACHE=ICACHE or DCACHE;

procedure md_cacheflush(addr:Pointer;nbytes,cache:Integer);

implementation

function md_alloc_page(x:Pointer):Pointer; inline;
begin
 Result:=Pointer(QWORD(x) and (not (MD_ALLOC_GRANULARITY-1)));
end;

function md_dw_page(x:QWORD):QWORD; inline;
begin
 Result:=x and (not MD_PAGE_SIZE);
end;

function md_up_page(x:QWORD):QWORD; inline;
begin
 Result:=(x+(MD_PAGE_SIZE-1)) and (not (MD_PAGE_SIZE-1));
end;

function md_reserve(var base:Pointer;size:QWORD):Integer;
begin
 base:=md_alloc_page(base);
 size:=md_up_page(size);

 Result:=NtAllocateVirtualMemory(
          NtCurrentProcess,
          @base,
          0,
          @size,
          MEM_RESERVE,
          PAGE_NOACCESS
         );
end;

function md_enter(base:Pointer;size:QWORD;prot:Integer):Integer;
begin
 Result:=NtAllocateVirtualMemory(
          NtCurrentProcess,
          @base,
          0,
          @size,
          MEM_COMMIT,
          prot
         );
end;

function md_protect(base:Pointer;size:QWORD;prot:Integer):Integer;
var
 old:Integer;
begin
 old:=0;
 Result:=NtProtectVirtualMemory(
          NtCurrentProcess,
          @base,
          @size,
          prot,
          @old
         );
end;

function md_remove(base:Pointer;size:QWORD):Integer;
begin
 Result:=NtFreeVirtualMemory(
          NtCurrentProcess,
          @base,
          @size,
          MEM_DECOMMIT
         );
end;

function md_reset(base:Pointer;size:QWORD;prot:Integer):Integer;
begin
 Result:=NtAllocateVirtualMemory(
          NtCurrentProcess,
          @base,
          0,
          @size,
          MEM_RESET,
          prot
         );
end;

function md_mmap(var base:Pointer;size:QWORD;prot:Integer):Integer;
begin
 base:=md_alloc_page(base);
 size:=md_up_page(size);

 Result:=NtAllocateVirtualMemory(
          NtCurrentProcess,
          @base,
          0,
          @size,
          MEM_COMMIT or MEM_RESERVE,
          prot
         );
end;

function md_unmap(base:Pointer;size:QWORD):Integer;
begin
 base:=md_alloc_page(base);
 size:=0;

 Result:=NtFreeVirtualMemory(
          NtCurrentProcess,
          @base,
          @size,
          MEM_RELEASE
         );
end;

function md_file_mmap(handle:THandle;var base:Pointer;offset,size:QWORD;prot:Integer):Integer;
var
 hSection:THandle;
 CommitSize:ULONG_PTR;
 SectionOffset:ULONG_PTR;
begin
 CommitSize:=0; //full size

 hSection:=0;
 Result:=NtCreateSection(
           @hSection,
           SECTION_MAP_WRITE or SECTION_MAP_READ or SECTION_MAP_EXECUTE,
           nil,
           @CommitSize,
           prot,
           SEC_COMMIT,
           handle
          );

 if (Result<>0) then Exit;

 base:=md_alloc_page(base);

 CommitSize:=size;
 SectionOffset:=offset and (not (MD_ALLOC_GRANULARITY-1));

 Result:=NtMapViewOfSection(hSection,
                            NtCurrentProcess,
                            @base,
                            0,
                            CommitSize,
                            @SectionOffset,
                            @CommitSize,
                            ViewUnmap,
                            0,
                            prot
                           );

 NtClose(hSection);
end;

function md_file_unmap(base:Pointer;size:QWORD):Integer;
begin
 base:=md_alloc_page(base);

 Result:=NtUnmapViewOfSection(NtCurrentProcess,base);
end;

//

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

