unit sys_mmap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm_mmap;

const
 PROT_NONE       =$00; // no permissions
 PROT_READ       =$01; // pages can be read
 PROT_WRITE      =$02; // pages can be written
 PROT_EXEC       =$04; // pages can be executed
 PROT_CPU_READ   =PROT_READ;
 PROT_CPU_WRITE  =PROT_WRITE;
 PROT_CPU_ALL    =$07;
 PROT_GPU_READ   =$10;
 PROT_GPU_WRITE  =$20;
 PROT_GPU_ALL    =$30;

 MAP_SHARED       =$00001;   // share changes
 MAP_PRIVATE      =$00002;   // changes are private
 MAP_FIXED        =$00010;   // map addr must be exactly as requested
 MAP_NO_OVERWRITE =$00080;   // don't overwrite memory with MAP_FIXED
 MAP_VOID         =$00100;   // reserve addr
 MAP_HASSEMAPHORE =$00200;   // region may contain semaphores
 MAP_STACK        =$00400;   // region grows down, like a stack
 MAP_NOSYNC       =$00800;   // page to but do not sync underlying file
 MAP_FILE         =$00000;   // map from file (default)
 MAP_ANON         =$01000;   // allocated from memory, swap space
 MAP_ANONYMOUS    =MAP_ANON; // For compatibility.
 MAP_SYSTEM       =$02000;
 MAP_ALLAVAILABLE =$04000;
 MAP_NOCORE       =$20000;   // dont include these pages in a coredump
 MAP_PREFAULT_READ=$40000;   // prefault mapping for reading
 MAP_SELF         =$80000;   // map decryped SELF file

 MAP_ALIGNMENT_BIT  =24;
 MAP_ALIGNMENT_SHIFT=$1f000000;
 MAP_ALIGNMENT_MASK =$ff shl MAP_ALIGNMENT_BIT;
 MAP_ALIGNED_SUPER  =$01 shl MAP_ALIGNMENT_BIT;
 //MAP_ALIGNED(n) ((n) << MAP_ALIGNMENT_SHIFT)

 MCL_CURRENT=$0001; // Lock only current memory
 MCL_FUTURE =$0002; // Lock all future memory as well

 MAP_FAILED    =Pointer(-1);

 MS_SYNC      =$0000; // msync synchronously
 MS_ASYNC     =$0001; // return immediately
 MS_INVALIDATE=$0002; // invalidate all cached data

 //Advice to madvise
 _MADV_NORMAL    =0; // no further special treatment
 _MADV_RANDOM    =1; // expect random page references
 _MADV_SEQUENTIAL=2; // expect sequential page references
 _MADV_WILLNEED  =3; // will need these pages
 _MADV_DONTNEED  =4; // dont need these pages

 MADV_NORMAL    =_MADV_NORMAL;
 MADV_RANDOM    =_MADV_RANDOM;
 MADV_SEQUENTIAL=_MADV_SEQUENTIAL;
 MADV_WILLNEED  =_MADV_WILLNEED;
 MADV_DONTNEED  =_MADV_DONTNEED;
 MADV_FREE      = 5; // dont need these pages, and junk contents
 MADV_NOSYNC    = 6; // try to avoid flushes to physical media
 MADV_AUTOSYNC  = 7; // revert to default flushing strategy
 MADV_NOCORE    = 8; // do not include these pages in a core file
 MADV_CORE      = 9; // revert to including pages in a core file
 MADV_PROTECT   =10; // protect process from pageout kill

type
 p_query_memory_prot=vm_mmap.p_query_memory_prot;
 t_query_memory_prot=vm_mmap.t_query_memory_prot;

//sce

function sceKernelSetVirtualRangeName(addr:Pointer;len:QWORD;name:PChar):Integer;

implementation

uses
 syscalls,
 errno;

//sce

function sceKernelSetVirtualRangeName(addr:Pointer;len:QWORD;name:PChar):Integer;
begin
 Result:=mname(addr,len,name);
 if (Result=-1) then
 begin
  Result:=px2sce(_get_errno);
 end;
end;


end.

