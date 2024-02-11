unit md_map;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 vm,
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

const
 VM_RWX=VM_PROT_READ or VM_PROT_WRITE or VM_PROT_EXECUTE;

 wprots:array[0..7] of Byte=(
  MD_PROT_NONE,//___
  MD_PROT_R   ,//__R
  MD_PROT_W   ,//_W_
  MD_PROT_RW  ,//_WR
  MD_PROT_R   ,//X__
  MD_PROT_R   ,//X_R
  MD_PROT_RW  ,//XW_
  MD_PROT_RW   //XWR
 );

 wprots_e:array[0..7] of Byte=(
  MD_PROT_NONE,//___
  MD_PROT_R   ,//__R
  MD_PROT_W   ,//_W_
  MD_PROT_RW  ,//_WR
  MD_PROT_X   ,//X__
  MD_PROT_RX  ,//X_R
  MD_PROT_WX  ,//XW_
  MD_PROT_RWX  //XWR
 );

function md_reserve(hProcess:THandle;var base:Pointer;size:QWORD):Integer;
function md_reserve(var base:Pointer;size:QWORD):Integer;

function md_split  (base:Pointer;size:QWORD):Integer;
function md_union  (base:Pointer;size:QWORD):Integer;

function md_memfd_create(var hMem:THandle;size:QWORD):Integer;
function md_memfd_open  (var hMem:THandle;hFile:THandle;maxprot:Byte):Integer;
function md_memfd_close (hMem:THandle):Integer;

function md_protect(hProcess:THandle;base:Pointer;size:QWORD;prot:Integer):Integer;
function md_protect(base:Pointer;size:QWORD;prot:Integer):Integer;

function md_dontneed(base:Pointer;size:QWORD):Integer;
function md_activate(base:Pointer;size:QWORD):Integer;

function md_mmap   (hProcess:THandle;var base:Pointer;size:QWORD;prot:Integer):Integer;
function md_unmap  (hProcess:THandle;base:Pointer;size:QWORD):Integer;

function md_mmap   (var base:Pointer;size:QWORD;prot:Integer):Integer;
function md_unmap  (base:Pointer;size:QWORD):Integer;

function md_file_mmap (handle:THandle;var base:Pointer;offset,size:QWORD;prot:Integer):Integer;
function md_file_unmap(base:Pointer;size:QWORD):Integer;

function md_file_mmap_ex (handle:THandle;base:Pointer;offset,size:QWORD;prot:Integer):Integer;
function md_file_unmap_ex(base:Pointer):Integer;

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
 Result:=x and (not (MD_PAGE_SIZE-1));
end;

function md_up_page(x:QWORD):QWORD; inline;
begin
 Result:=(x+(MD_PAGE_SIZE-1)) and (not (MD_PAGE_SIZE-1));
end;

function md_reserve(hProcess:THandle;var base:Pointer;size:QWORD):Integer;
var
 info:TMemoryBasicInformation;
 len:ULONG_PTR;
begin
 base:=md_alloc_page(base);
 size:=md_up_page(size);

 if (base<>nil) then
 begin
  len:=0;
  Result:=NtQueryVirtualMemory(
           hProcess,
           base,
           0,
           @info,
           sizeof(info),
           @len);

  //Writeln('NtQueryVirtualMemory:','0x',HexStr(Result,8));

  if (Result=0) then
  begin
   {
   Writeln('Query:','0x',HexStr(info.AllocationBase),'..',
                    '0x',HexStr(info.BaseAddress),'..',
                    '0x',HexStr(info.BaseAddress+info.RegionSize),':',
                    '0x',HexStr(info.RegionSize,11),' ',
                    info.State,' ',
                    info.Protect,' ',
                    info._Type
                    );
   }
   if (info.State=MEM_RESERVE) then
   begin
    if (base>=info.BaseAddress) and
       ((base+size)<=(info.BaseAddress+info.RegionSize)) then
    begin
     base:=info.AllocationBase;
     Exit(0);
    end;
   end;

  end;
 end;

 Result:=NtAllocateVirtualMemoryEx(
          hProcess,
          @base,
          @size,
          MEM_RESERVE or MEM_RESERVE_PLACEHOLDER,
          PAGE_NOACCESS,
          nil,
          0
         );

end;

function md_reserve(var base:Pointer;size:QWORD):Integer;
begin
 Result:=md_reserve(NtCurrentProcess,base,size);
end;

function md_split(base:Pointer;size:QWORD):Integer;
begin
 Result:=NtFreeVirtualMemory(
          NtCurrentProcess,
          @base,
          @size,
          MEM_RELEASE or MEM_PRESERVE_PLACEHOLDER
         );
end;

function md_union(base:Pointer;size:QWORD):Integer;
begin
 Result:=NtFreeVirtualMemory(
          NtCurrentProcess,
          @base,
          @size,
          MEM_RELEASE or MEM_COALESCE_PLACEHOLDERS
         );
end;

function md_memfd_create(var hMem:THandle;size:QWORD):Integer;
begin
 hMem:=0;
 Result:=NtCreateSection(
          @hMem,
          SECTION_ALL_ACCESS,
          //SECTION_MAP_WRITE or SECTION_MAP_READ or SECTION_MAP_EXECUTE,
          nil,
          @size,
          PAGE_READWRITE,
          SEC_COMMIT,
          THandle(0)
         );
end;

function md_memfd_open(var hMem:THandle;hFile:THandle;maxprot:Byte):Integer;
var
 Access:DWORD;
 prot:DWORD;
 size:QWORD;
begin
 Access:=SECTION_QUERY;

 if ((maxprot and VM_PROT_READ)<>0) then
 begin
  Access:=Access or SECTION_MAP_READ;
 end;

 if ((maxprot and VM_PROT_WRITE)<>0) then
 begin
  Access:=Access or SECTION_MAP_WRITE;
 end;

 if ((maxprot and VM_PROT_EXECUTE)<>0) then
 begin
  Access:=Access or SECTION_MAP_EXECUTE;
 end;

 prot:=wprots_e[maxprot and VM_RWX];

 size:=0;
 hMem:=0;
 Result:=NtCreateSection(
          @hMem,
          Access,
          nil,
          @size,
          prot,
          SEC_COMMIT,
          hFile
         );
end;

function md_memfd_close(hMem:THandle):Integer;
begin
 Result:=NtClose(hMem);
end;

function md_protect(hProcess:THandle;base:Pointer;size:QWORD;prot:Integer):Integer;
var
 old:Integer;
begin
 old:=0;
 Result:=NtProtectVirtualMemory(
          hProcess,
          @base,
          @size,
          prot,
          @old
         );
end;

function md_protect(base:Pointer;size:QWORD;prot:Integer):Integer;
begin
 Result:=md_protect(NtCurrentProcess,base,size,prot);
end;

function md_dontneed(base:Pointer;size:QWORD):Integer;
begin
 Result:=NtAllocateVirtualMemory(
          NtCurrentProcess,
          @base,
          0,
          @size,
          MEM_RESET,
          PAGE_NOACCESS
         );
end;

function md_activate(base:Pointer;size:QWORD):Integer;
begin
 Result:=NtAllocateVirtualMemory(
          NtCurrentProcess,
          @base,
          0,
          @size,
          MEM_RESET_UNDO,
          PAGE_NOACCESS
         );
end;

function md_mmap(hProcess:THandle;var base:Pointer;size:QWORD;prot:Integer):Integer;
begin
 base:=md_alloc_page(base);
 size:=md_up_page(size);

 Result:=NtAllocateVirtualMemory(
          hProcess,
          @base,
          0,
          @size,
          MEM_COMMIT or MEM_RESERVE,
          prot
         );
end;

function md_unmap(hProcess:THandle;base:Pointer;size:QWORD):Integer;
begin
 base:=md_alloc_page(base);
 size:=0;

 Result:=NtFreeVirtualMemory(
          hProcess,
          @base,
          @size,
          MEM_RELEASE
         );
end;

function md_mmap(var base:Pointer;size:QWORD;prot:Integer):Integer;
begin
 Result:=md_mmap(NtCurrentProcess,base,size,prot);
end;

function md_unmap(base:Pointer;size:QWORD):Integer;
begin
 Result:=md_unmap(NtCurrentProcess,base,size);
end;

function md_file_mmap(handle:THandle;var base:Pointer;offset,size:QWORD;prot:Integer):Integer;
var
 CommitSize:ULONG_PTR;
 SectionOffset:ULONG_PTR;
begin
 base:=md_alloc_page(base);

 CommitSize:=size;
 SectionOffset:=offset and (not (MD_ALLOC_GRANULARITY-1));

 Result:=NtMapViewOfSection(handle,
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
end;

function md_file_unmap(base:Pointer;size:QWORD):Integer;
begin
 base:=md_alloc_page(base);

 Result:=NtUnmapViewOfSection(NtCurrentProcess,base);
end;

//

function md_file_mmap_ex(handle:THandle;base:Pointer;offset,size:QWORD;prot:Integer):Integer;
begin
 Result:=NtMapViewOfSectionEx(
          handle,
          NtCurrentProcess,
          @base,
          @offset,
          @size,
          MEM_REPLACE_PLACEHOLDER,
          prot,
          nil,
          0
         );
end;

function md_file_unmap_ex(base:Pointer):Integer;
begin
 Result:=NtUnmapViewOfSectionEx(NtCurrentProcess,base,MEM_PRESERVE_PLACEHOLDER);
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

