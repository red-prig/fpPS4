unit mmap;

{$mode ObjFPC}{$H+}

interface

uses
 Windows;

const
 // CPU
  SCE_KERNEL_PROT_CPU_READ =$01;
  SCE_KERNEL_PROT_CPU_WRITE=$02;
  SCE_KERNEL_PROT_CPU_RW   =(SCE_KERNEL_PROT_CPU_READ or SCE_KERNEL_PROT_CPU_WRITE);
  SCE_KERNEL_PROT_CPU_EXEC =$04;
  SCE_KERNEL_PROT_CPU_ALL  =(SCE_KERNEL_PROT_CPU_RW or SCE_KERNEL_PROT_CPU_EXEC);

 // GPU
  SCE_KERNEL_PROT_GPU_READ =$10;
  SCE_KERNEL_PROT_GPU_WRITE=$20;
  SCE_KERNEL_PROT_GPU_RW   =(SCE_KERNEL_PROT_GPU_READ or SCE_KERNEL_PROT_GPU_WRITE);
  SCE_KERNEL_PROT_GPU_ALL  =SCE_KERNEL_PROT_GPU_RW;

  SCE_KERNEL_MAP_FIXED       =$0010;
  SCE_KERNEL_MAP_NO_OVERWRITE=$0080;
  SCE_KERNEL_MAP_DMEM_COMPAT =$0400;
  SCE_KERNEL_MAP_NO_COALESCE =$400000;

  SCE_KERNEL_WB_ONION  = 0;
  SCE_KERNEL_WC_GARLIC = 3;
  SCE_KERNEL_WB_GARLIC =10;

 //mmap
  PROT_NONE      =$00; // no permissions
  PROT_READ      =$01; // pages can be read
  PROT_WRITE     =$02; // pages can be written
  PROT_EXEC      =$04; // pages can be executed
  PROT_CPU_READ  =PROT_READ;
  PROT_CPU_WRITE =PROT_WRITE;
  PROT_CPU_ALL   =$07;
  PROT_GPU_READ  =$10;
  PROT_GPU_WRITE =$20;
  PROT_GPU_ALL   =$30;


  MAP_SHARED      =$0001; // share changes
  MAP_PRIVATE     =$0002; // changes are private
  MAP_FIXED       =$0010; // map addr must be exactly as requested
  MAP_NO_OVERWRITE=$0080;
  MAP_VOID        =$0100; // reserve

  MAP_RENAME      =$0020; // Sun: rename private pages to file
  MAP_NORESERVE   =$0040; // Sun: don't reserve needed swap area
  MAP_HASSEMAPHORE=$0200; // region may contain semaphores
  MAP_STACK       =$0400; // region grows down, like a stack
  MAP_NOSYNC      =$0800; // page to but do not sync underlying file

  MAP_FILE        =$0000;    // map from file (default)
  MAP_ANON        =$1000;    // allocated from memory, swap space
  MAP_ANONYMOUS   =MAP_ANON; // For compatibility.
  MAP_SYSTEM      =$2000;
  MAP_ALLAVAILABLE=$4000;

  MAP_SELF        =$00080000; // map decryped SELF file

  MAP_ALIGNMENT_BIT =24;
  MAP_ALIGNMENT_MASK=$1f000000;
  MAP_ALIGNMENT_MUL =$01000000; //1 shl 24

  MAP_FAILED    =Pointer(-1);

function _isgpu(prot:Integer):Boolean; inline;
function __map_prot_page(prot:Integer):DWORD;
function __map_prot_file(prot:Integer):DWORD;

function _VirtualAlloc   (Addr:Pointer;dwSize:PTRUINT;prot:Integer):Integer;
function _VirtualReserve (Addr:Pointer;dwSize:PTRUINT;prot:Integer):Integer;
function _VirtualCommit  (Addr:Pointer;dwSize:PTRUINT;prot:Integer):Integer;
function _VirtualDecommit(Addr:Pointer;dwSize:PTRUINT):Integer;
function _VirtualFree    (Addr:Pointer):Integer;
function _VirtualMmap    (Addr:Pointer;len:size_t;prot,fd:Integer;offst:size_t):Integer;
function _VirtualUnmap   (addr:Pointer):Integer;
function _VirtualProtect (addr:Pointer;len:size_t;prot:Integer):Integer;

implementation

const
 FILE_MAP_EXECUTE=$0020;

function _isgpu(prot:Integer):Boolean; inline;
begin
 Result:=prot and (SCE_KERNEL_PROT_GPU_READ or SCE_KERNEL_PROT_GPU_WRITE)<>0;
end;

function __map_prot_page(prot:Integer):DWORD;
begin
 Result:=0;
 if (prot=PROT_NONE) then Exit(PAGE_NOACCESS);

 if (prot and PROT_EXEC)<>0 then
 begin
  if (prot and (PROT_WRITE or SCE_KERNEL_PROT_GPU_WRITE))<>0 then
  begin
   Result:=PAGE_EXECUTE_READWRITE;
  end else
  if (prot and (PROT_READ or SCE_KERNEL_PROT_GPU_READ))<>0 then
  begin
   Result:=PAGE_EXECUTE_READ;
  end else
  begin
   Result:=PAGE_EXECUTE;
  end;
 end else
 if (prot and (PROT_WRITE or SCE_KERNEL_PROT_GPU_WRITE))<>0 then
 begin
  Result:=PAGE_READWRITE;
 end else
 begin
  Result:=PAGE_READONLY;
 end;
end;

function __map_prot_file(prot:Integer):DWORD;
begin
 Result:= 0;
 if (prot=PROT_NONE) then Exit;
 if (prot and PROT_READ) <>0 then Result:=Result or FILE_MAP_READ;
 if (prot and PROT_WRITE)<>0 then Result:=Result or FILE_MAP_WRITE;
 if (prot and PROT_EXEC) <>0 then Result:=Result or FILE_MAP_EXECUTE;
end;

function _VirtualAlloc(Addr:Pointer;dwSize:PTRUINT;prot:Integer):Integer;
begin
 Result:=0;
 if (Addr=nil) then Exit(-1);
 Addr:=VirtualAlloc(Addr,dwSize,MEM_COMMIT or MEM_RESERVE,__map_prot_page(prot));
 if (Addr<>nil) then Exit;
 Result:=GetLastError;
end;

function _VirtualReserve(Addr:Pointer;dwSize:PTRUINT;prot:Integer):Integer;
begin
 Result:=0;
 if (Addr=nil) then Exit(-1);
 Addr:=VirtualAlloc(Addr,dwSize,MEM_RESERVE,__map_prot_page(prot));
 if (Addr<>nil) then Exit;
 Result:=GetLastError;
end;

function _VirtualCommit(Addr:Pointer;dwSize:PTRUINT;prot:Integer):Integer;
var
 new:Pointer;
begin
 Result:=0;
 if (Addr=nil) then Exit(-1);
 new:=VirtualAlloc(Addr,dwSize,MEM_COMMIT,__map_prot_page(prot));
 if (new<>nil) then
 begin
  Assert(new=Addr);
  Exit;
 end;
 Result:=GetLastError;
end;

function _VirtualDecommit(Addr:Pointer;dwSize:PTRUINT):Integer;
begin
 Result:=0;
 if (Addr=nil) then Exit(-1);
 if (dwSize=0) then Exit;
 if not VirtualFree(Addr,dwSize,MEM_DECOMMIT) then
 begin
  Result:=GetLastError;
 end;
end;

function _VirtualFree(Addr:Pointer):Integer;
begin
 Result:=0;
 if (Addr=nil) then Exit(-1);
 if not VirtualFree(Addr,0,MEM_RELEASE) then
 begin
  Result:=GetLastError;
 end;
end;

function _get_osfhandle(fd:Integer):THandle; cdecl; external 'msvcrt';

function MapViewOfFileEx(hFileMappingObject:HANDLE;
                         dwDesiredAccess:DWORD;
                         dwFileOffsetHigh:DWORD;
                         dwFileOffsetLow:DWORD;
                         dwNumberOfBytesToMap:SIZE_T;
                         lpBaseAddress:LPVOID):LPVOID; stdcall; external 'kernel32' name 'MapViewOfFileEx';

function _VirtualMmap(Addr:Pointer;len:size_t;prot,fd:Integer;offst:size_t):Integer;
Var
 fm,h:THandle;
 dwFileOffsetLow,dwFileOffsetHigh,protect,desiredAccess,dwMaxSizeLow,dwMaxSizeHigh:DWORD;
 maxSize:size_t;
begin
 if (Addr=nil) then Exit(-1);

 h:=_get_osfhandle(fd);
 if (h=INVALID_HANDLE_VALUE) then
 begin
  Exit(GetLastError);
 end;

 maxSize:=offst+len;

 dwFileOffsetLow :=DWORD(offst and $FFFFFFFF);
 dwFileOffsetHigh:=DWORD(offst shr 32);
 dwMaxSizeLow    :=DWORD(maxSize and $FFFFFFFF);
 dwMaxSizeHigh   :=DWORD(maxSize shr 32);

 protect      :=__map_prot_page(prot);
 desiredAccess:=__map_prot_file(prot);

 fm:=CreateFileMapping(h,nil,protect,dwMaxSizeHigh,dwMaxSizeLow,nil);
 if (fm=0) then
 begin
  Exit(GetLastError);
 end;

 addr:=MapViewOfFileEx(fm,desiredAccess,dwFileOffsetHigh,dwFileOffsetLow,len,addr);

 CloseHandle(fm);

 if (addr=nil) then
 begin
  Exit(GetLastError);
 end;
end;

function _VirtualUnmap(addr:Pointer):Integer;
begin
 if (Addr=nil) then Exit(-1);
 if UnmapViewOfFile(addr) then
 begin
  Result:=0;
 end else
 begin
  Result:=GetLastError;
 end;
end;

function _VirtualProtect(addr:Pointer;len:size_t;prot:Integer):Integer;
Var
 old:DWORD;
begin
 Result:=0;
 old:=0;
 if not VirtualProtect(addr,len,__map_prot_page(prot),old) then
 begin
  Result:=GetLastError;
 end;
end;

end.



