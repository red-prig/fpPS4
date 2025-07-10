unit md_map;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 vm,
 vmparam,
 windows;

const
 MD_PAGE_SHIFT       = 12;
 MD_PAGE_SIZE        = 4*1024;
 MD_PAGE_MASK        = MD_PAGE_SIZE-1;
 MD_ALLOC_GRANULARITY= 64*1024;

 MD_MAP_FIXED      =$100;
 MD_MAP_RESERVED   =$200;

 MD_MAP_ALIGN_SHIFT=24;

function MD_MAP_ALIGN(align:QWORD):DWORD; inline;

const
 VM_RW =VM_PROT_READ or VM_PROT_WRITE;
 VM_RWX=VM_PROT_READ or VM_PROT_WRITE or VM_PROT_EXECUTE;

function md_placeholder_mmap    (var base:Pointer;size:QWORD;flags:DWORD=0;hProcess:THandle=NtCurrentProcess):Integer;
function md_placeholder_unmap   (base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer;
function md_placeholder_split   (base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer; inline;
function md_placeholder_union   (base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer; inline;
function md_placeholder_commit  (base:Pointer;size:QWORD;prot:DWORD;hProcess:THandle=NtCurrentProcess):Integer;
function md_placeholder_commit  (base:Pointer;size:QWORD;prot:DWORD;fd:THandle;offset:QWORD;hProcess:THandle=NtCurrentProcess):Integer;
function md_placeholder_decommit(base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer;

function md_memfd_create(var hMem:THandle;size:QWORD;maxprot:Byte):Integer;
function md_memfd_open  (var hMem:THandle;hFile:THandle;maxprot:Byte):Integer;
function md_memfd_close (hMem:THandle):Integer; inline;

function md_protect (base:Pointer;size:QWORD;prot:Integer;hProcess:THandle=NtCurrentProcess):Integer;
function md_dontneed(base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer; inline;
function md_activate(base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer; inline;

function md_mmap (var base:Pointer;size:QWORD;prot:DWORD;fd:THandle=0;offset:QWORD=0;hProcess:THandle=NtCurrentProcess):Integer;
function md_unmap(base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer;

function  kmem_alloc(size:QWORD;prot:DWORD):Pointer;
procedure kmem_free (base:Pointer;size:QWORD); inline;

const
 ICACHE=1; //Flush the instruction cache.
 DCACHE=2; //Write back to memory and invalidate the affected valid cache lines.
 BCACHE=ICACHE or DCACHE;

procedure md_cacheflush(addr:Pointer;nbytes,cache:Integer);

Function  md_create_swap_file(const FNAME:RawByteString;SIZE:QWORD;Var FD:THandle):DWORD;

implementation

function MD_MAP_ALIGN(align:QWORD):DWORD; inline;
begin
 Result:=BsfQWORD(align) shl MD_MAP_ALIGN_SHIFT;
end;

const
 MD_PROT_NONE=PAGE_NOACCESS;
 MD_PROT_R   =PAGE_READONLY;
 MD_PROT_W   =PAGE_READWRITE;
 MD_PROT_RW  =PAGE_READWRITE;
 MD_PROT_X   =PAGE_EXECUTE;
 MD_PROT_RX  =PAGE_EXECUTE_READ;
 MD_PROT_WX  =PAGE_EXECUTE_READWRITE;
 MD_PROT_RWX =PAGE_EXECUTE_READWRITE;

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

 waccess:array[0..7] of Byte=(
  SECTION_QUERY,                                                                //___
  SECTION_QUERY or SECTION_MAP_READ,                                            //__R
  SECTION_QUERY or SECTION_MAP_WRITE,                                           //_W_
  SECTION_QUERY or SECTION_MAP_WRITE   or SECTION_MAP_READ,                     //_WR
  SECTION_QUERY or SECTION_MAP_EXECUTE,                                         //X__
  SECTION_QUERY or SECTION_MAP_EXECUTE or SECTION_MAP_READ,                     //X_R
  SECTION_QUERY or SECTION_MAP_EXECUTE or SECTION_MAP_WRITE,                    //XW_
  SECTION_QUERY or SECTION_MAP_EXECUTE or SECTION_MAP_WRITE or SECTION_MAP_READ //XWR
 );

function md_dw_gran(x:Pointer):Pointer; inline;
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

function md_placeholder_mmap(var base:Pointer;size:QWORD;flags:DWORD=0;hProcess:THandle=NtCurrentProcess):Integer;
var
 ADDR:Pointer;
 len :ULONG_PTR;
 info:TMemoryBasicInformation;
 EXT :MEM_EXTENDED_PARAMETER;
 REQ :MEM_ADDRESS_REQUIREMENTS;
begin
 size:=md_up_page(size);

 if ((flags and MD_MAP_FIXED)<>0) then
 begin
  ADDR:=md_dw_gran(base);

  len:=0;
  Result:=NtQueryVirtualMemory(
           hProcess,
           ADDR,
           MemoryBasicInformation,
           @info,
           sizeof(TMemoryBasicInformation),
           @len);

  if (Result=0) and (info.State=MEM_RESERVE) then
  begin
   if (ADDR>=info.BaseAddress) and
      ((ADDR+size)<=(info.BaseAddress+info.RegionSize)) then
   begin
    base:=info.AllocationBase;
    Exit(0);
   end;
  end;

  REQ.LowestStartingAddress:=nil;
  REQ.Alignment            :=0;
 end else
 begin
  ADDR:=nil;
  REQ.LowestStartingAddress:=base;

  REQ.Alignment:=(flags shr MD_MAP_ALIGN_SHIFT) and $1F;

  if (REQ.Alignment<=16) then
  begin
   REQ.Alignment:=0;
  end else
  begin
   REQ.Alignment:=QWORD(1) shl REQ.Alignment;
  end;
 end;

 EXT.pType  :=MemExtendedParameterAddressRequirements;
 EXT.Pointer:=@REQ;

 REQ.HighestEndingAddress:=nil;

 Result:=NtAllocateVirtualMemoryEx(
          hProcess,
          @ADDR,
          @size,
          MEM_RESERVE or MEM_RESERVE_PLACEHOLDER,
          PAGE_NOACCESS,
          @EXT,
          1
         );

 if (Result=0) then
 begin
  base:=ADDR;
 end else
 begin
  base:=nil;
 end;

end;

function md_placeholder_unmap(base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer;
var
 pend:Pointer;

 addr,prev:Pointer;
 info:TMemoryBasicInformation;
 len:ULONG_PTR;
begin
 if (base=nil) or (size=0) then Exit(0);

 pend:=base+size;
 addr:=base;

 repeat

  len:=0;
  NtQueryVirtualMemory(
   hProcess,
   addr,
   0,
   @info,
   sizeof(TMemoryBasicInformation),
   @len);
  if (len=0) then Break;

  if (info.State<>MEM_FREE) then
  begin
   addr:=info.BaseAddress;

   case info._Type of
    MEM_MAPPED:
     begin
      //unmap
      Result:=NtUnmapViewOfSectionEx(hProcess,addr,MEM_PRESERVE_PLACEHOLDER);
     end;
    MEM_PRIVATE:
     begin
      //unmap
      len:=Info.RegionSize;
      Result:=NtFreeVirtualMemory(
               hProcess,
               @addr,
               @len,
               MEM_RELEASE or MEM_PRESERVE_PLACEHOLDER
              );
     end;
    else
     Result:=-1;
   end;

   if (Result<>0) then Exit;
  end;

  prev:=addr;
  addr:=addr+Info.RegionSize;

  if (addr>=pend) then Break;

 until (prev>=addr);

 //union all
 Result:=NtFreeVirtualMemory(
          NtCurrentProcess,
          @base,
          @size,
          MEM_RELEASE or MEM_COALESCE_PLACEHOLDERS
         );
 //ignore errors

 //free
 size:=0;
 Result:=NtFreeVirtualMemory(
          NtCurrentProcess,
          @base,
          @size,
          MEM_RELEASE
         );
end;

function md_placeholder_split(base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer; inline;
begin
 Result:=NtFreeVirtualMemory(
          hProcess,
          @base,
          @size,
          MEM_RELEASE or MEM_PRESERVE_PLACEHOLDER
         );
end;

function md_placeholder_union(base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer; inline;
begin
 Result:=NtFreeVirtualMemory(
          hProcess,
          @base,
          @size,
          MEM_RELEASE or MEM_COALESCE_PLACEHOLDERS
         );
end;

function md_placeholder_commit(base:Pointer;size:QWORD;prot:DWORD;hProcess:THandle=NtCurrentProcess):Integer;
begin
 prot:=wprots[prot and VM_RWX];

 Result:=NtAllocateVirtualMemoryEx(
          hProcess,
          @base,
          @size,
          MEM_COMMIT or MEM_RESERVE or MEM_REPLACE_PLACEHOLDER,
          prot,
          nil,
          0
         );
end;

function md_placeholder_commit(base:Pointer;size:QWORD;prot:DWORD;fd:THandle;offset:QWORD;hProcess:THandle=NtCurrentProcess):Integer;
begin
 prot:=wprots[prot and VM_RWX];

 Result:=NtMapViewOfSectionEx(
          fd,
          hProcess,
          @base,
          @offset,
          @size,
          MEM_REPLACE_PLACEHOLDER,
          prot,
          nil,
          0
         );
end;

function md_placeholder_decommit(base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer;
begin
 Result:=NtUnmapViewOfSectionEx(hProcess,base,MEM_PRESERVE_PLACEHOLDER);

 if (DWORD(Result)=$C0000019) then //STATUS_NOT_MAPPED_VIEW
 begin
  Result:=NtFreeVirtualMemory(
           hProcess,
           @base,
           @size,
           MEM_RELEASE or MEM_PRESERVE_PLACEHOLDER
          );
 end;
end;

function md_memfd_create(var hMem:THandle;size:QWORD;maxprot:Byte):Integer;
var
 Access:DWORD;
 prot:DWORD;
begin
 Access:=waccess[maxprot and VM_RWX];
 prot  :=wprots [maxprot and VM_RWX];

 hMem:=0;
 Result:=NtCreateSectionEx(
          @hMem,
          Access,
          nil,
          @size,
          prot,
          SEC_COMMIT,
          THandle(0),
          nil,
          0
         );
end;

function md_memfd_open(var hMem:THandle;hFile:THandle;maxprot:Byte):Integer;
var
 Access:DWORD;
 prot:DWORD;
 size:QWORD;
begin
 Access:=waccess[maxprot and VM_RWX];
 prot  :=wprots [maxprot and VM_RWX];

 size:=0;
 hMem:=0;
 Result:=NtCreateSectionEx(
          @hMem,
          Access,
          nil,
          @size,
          prot,
          SEC_COMMIT,
          hFile,
          nil,
          0
         );
end;

function md_memfd_close(hMem:THandle):Integer; inline;
begin
 Result:=NtClose(hMem);
end;

function md_protect(base:Pointer;size:QWORD;prot:Integer;hProcess:THandle=NtCurrentProcess):Integer;
var
 old:Integer;
begin
 prot:=wprots[prot and VM_RWX];

 old:=0;
 Result:=NtProtectVirtualMemory(
          hProcess,
          @base,
          @size,
          prot,
          @old
         );
end;

function md_dontneed(base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer; inline;
begin
 Result:=NtAllocateVirtualMemory(
          hProcess,
          @base,
          0,
          @size,
          MEM_RESET,
          PAGE_NOACCESS
         );
end;

function md_activate(base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer; inline;
begin
 Result:=NtAllocateVirtualMemory(
          hProcess,
          @base,
          0,
          @size,
          MEM_RESET_UNDO,
          PAGE_NOACCESS
         );
end;

const
 atypes:array[0..3] of DWORD=(
  MEM_COMMIT               , //___
  MEM_COMMIT or MEM_RESERVE, //__F
  MEM_RESERVE              , //_R_
  MEM_RESERVE                //_RF
 );

function md_mmap(var base:Pointer;size:QWORD;prot:DWORD;fd:THandle=0;offset:QWORD=0;hProcess:THandle=NtCurrentProcess):Integer;
var
 atype:DWORD;
 ADDR :Pointer;
 EXT  :MEM_EXTENDED_PARAMETER;
 REQ  :MEM_ADDRESS_REQUIREMENTS;
begin
 EXT.pType  :=MemExtendedParameterAddressRequirements;
 EXT.Pointer:=@REQ;

 if ((prot and MD_MAP_FIXED)<>0) then
 begin
  ADDR:=md_dw_gran(base);
  REQ.LowestStartingAddress:=nil;
  REQ.Alignment            :=0;
 end else
 begin
  ADDR:=nil;
  REQ.LowestStartingAddress:=base;

  REQ.Alignment:=(prot shr MD_MAP_ALIGN_SHIFT) and $1F;

  if (REQ.Alignment<=16) then
  begin
   REQ.Alignment:=0;
  end else
  begin
   REQ.Alignment:=QWORD(1) shl REQ.Alignment;
  end;
 end;

 REQ.HighestEndingAddress:=nil;

 atype:=atypes[(prot shr 8) and 3];

 if (prot and MD_MAP_RESERVED)<>0 then
 begin
  prot:=PAGE_NOACCESS;
  fd  :=0;
 end else
 begin
  prot:=wprots[prot and VM_RWX];
 end;

 if (fd=THandle(0)) or (fd=THandle(-1)) then
 begin
  Result:=NtAllocateVirtualMemoryEx(
           hProcess,
           @ADDR,
           @size,
           atype,
           prot,
           @EXT,
           1
          );
 end else
 begin
  Result:=NtMapViewOfSectionEx(
           fd,
           hProcess,
           @ADDR,
           @offset,
           @size,
           0,
           prot,
           @EXT,
           1
          );
 end;

 if (Result=0) then
 begin
  base:=ADDR;
 end else
 begin
  base:=nil;
 end;

end;

function md_unmap(base:Pointer;size:QWORD;hProcess:THandle=NtCurrentProcess):Integer;
var
 pend:Pointer;

 addr,prev:Pointer;
 info:TMemoryBasicInformation;
 len:ULONG_PTR;
begin
 if (base=nil) or (size=0) then Exit(0);

 pend:=base+size;
 addr:=base;

 repeat

  len:=0;
  NtQueryVirtualMemory(
   hProcess,
   addr,
   0,
   @info,
   sizeof(TMemoryBasicInformation),
   @len);
  if (len=0) then Break;

  if (info.State<>MEM_FREE) then
  begin
   addr:=info.AllocationBase;

   case info._Type of
    MEM_MAPPED:
     begin
      //unmap
      Result:=NtUnmapViewOfSectionEx(hProcess,addr,0);
     end;
    MEM_PRIVATE:
     begin
      //unmap
      len:=0;
      Result:=NtFreeVirtualMemory(
               hProcess,
               @addr,
               @len,
               MEM_RELEASE
              );
     end;
    else
     Result:=-1;
   end;

   if (Result<>0) then Exit;
  end else
  begin
   addr:=info.BaseAddress;
  end;

  prev:=addr;
  addr:=info.BaseAddress+Info.RegionSize;

  if (addr>=pend) then Break;

 until (prev>=addr);
end;

function kmem_alloc(size:QWORD;prot:DWORD):Pointer;
var
 r:Integer;
begin
 Result:=Pointer(KERNEL_LOWER); //lower
 r:=md_mmap(Result,size,prot);
 if (r<>0) then
 begin
  Writeln(stderr,'kmem_alloc(0x',HexStr(size,11),',0x',HexStr(prot,3),'):0x',HexStr(r,8));
 end;
end;

procedure kmem_free(base:Pointer;size:QWORD); inline;
begin
 md_unmap(base,size);
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

Function NtTruncate(FD:THandle;SIZE:QWORD):DWORD; inline;
var
 BLK:IO_STATUS_BLOCK;
begin
 Result:=NtSetInformationFile(
          FD,
          @BLK,
          @SIZE,
          SizeOf(Int64),
          FileEndOfFileInformation);

 if (Result<>0) then
 begin
  Result:=NtSetInformationFile(
           FD,
           @BLK,
           @SIZE,
           SizeOf(Int64),
           FileAllocationInformation);
 end;
end;

function StrLen(p: pwidechar): sizeint; external name 'FPC_PWIDECHAR_LENGTH'; overload;

Function md_create_swap_file(const FNAME:RawByteString;SIZE:QWORD;Var FD:THandle):DWORD;
var
 W:WideString;

 OBJ  :OBJECT_ATTRIBUTES;
 UPATH:UNICODE_STRING;
 BLK  :IO_STATUS_BLOCK;

begin
 W:=UTF8Decode(FNAME);
 W:='\??\'+W;

 OBJ:=Default(OBJECT_ATTRIBUTES);
 OBJ.Length    :=SizeOf(OBJECT_ATTRIBUTES);
 OBJ.ObjectName:=@UPATH;

 UPATH:=Default(UNICODE_STRING);
 UPATH.Length       :=strlen(PWideChar(w))*SizeOf(WideChar);
 UPATH.MaximumLength:=UPATH.Length+SizeOf(WideChar);
 UPATH.Buffer       :=PWideChar(w);

 BLK:=Default(IO_STATUS_BLOCK);
 FD:=0;

 Result:=NtCreateFile(@FD,
                      FILE_READ_DATA or
                      FILE_WRITE_DATA or
                      FILE_APPEND_DATA or
                      FILE_READ_ATTRIBUTES or
                      FILE_WRITE_ATTRIBUTES or
                      FILE_CAN_DELETE or
                      SYNCHRONIZE,
                      @OBJ,
                      @BLK,
                      nil,
                      FILE_ATTRIBUTE_TEMPORARY,
                      0,
                      FILE_OVERWRITE_IF,
                      FILE_SYNCHRONOUS_IO_NONALERT or
                      FILE_OPEN_REPARSE_POINT or
                      FILE_NON_DIRECTORY_FILE or
                      FILE_DELETE_ON_CLOSE,
                      nil,
                      0);

 if (Result<>0) then Exit;

 Result:=NtTruncate(FD,SIZE);

 if (Result<>0) then
 begin
  NtClose(FD);
  FD:=0;
 end;
end;

end.

