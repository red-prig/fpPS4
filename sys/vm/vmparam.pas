unit vmparam;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 t_addr_range=packed record
  start:QWORD;
  __end:QWORD;
 end;

const
 PAGE_SHIFT=14;
 PAGE_SIZE =1 shl PAGE_SHIFT; //16384
 PAGE_MASK =PAGE_SIZE-1;

 NBBY=8;

 NBPDR  =$1fffff;
 PDRMASK=$1fffff;

 MAXPAGESIZES=3; // maximum number of supported page sizes
 IOPAGES     =2; // pages of i/o permission bitmap

 pagesizes:array[0..2] of QWORD=(PAGE_SIZE,0,0);

 //Virtual memory related constants, all in bytes
 MAXTSIZ =(2048 *1024*1024); // max text size
 DFLDSIZ =(2048 *1024*1024); // initial data size limit
 MAXDSIZ =(32768*1024*1024); // max data size
 DFLSSIZ =(8    *1024*1024); // initial stack size limit
 MAXSSIZ =(2    *1024*1024); // max stack size
 SGROWSIZ=        (16*1024); // amount to grow stack

 //0x0 7FFE0000

 _PROC_AREA_START_0   =QWORD($00000400000);
 _PROC_AREA_START_1   =QWORD($00010000000); //(original:0x400000-0x80000000)
 _PROC_AREA___END     =QWORD($00070000000);

 WIN_MIN_MOVED_STACK  =QWORD($00070000000);
 WIN_MAX_MOVED_STACK  =QWORD($0007FFE0000); //255MB

 WIN_SHARED_ADDR      =QWORD($20000000000);

 SCE_REPLAY_EXEC_START=QWORD($00fc0000000);

 DL_AREA_START        =QWORD($00080000000); //(original:0x80000000-0x200000000)
 DL_AREA___END        =QWORD($00100000000);

 ET_DYN_LOAD_ADDR_USR =QWORD($00080000000);
 ET_DYN_LOAD_ADDR_SYS =QWORD($00800000000);

 USRSTACK             =QWORD($007EFFF8000);

 SCE_USR_HEAP_START   =QWORD($00200000000);
 SCE_SYS_HEAP_START   =QWORD($00880000000);

 SCE_KERNEL_GNMDRIVER =QWORD($00FE0000000);

 _VM_MINUSER_ADDRESS  =QWORD($00010000000); //(original:$000000000000)
 VM_MAXUSER_ADDRESS   =QWORD($10000000000); //(original:$800000000000) MAP_AREA_END=0xfc00000000

 VM_MIN_GPU_ADDRESS   =QWORD($10000000000);
 VM_MAX_GPU_ADDRESS   =QWORD($10180000000); //6GB

 VM_DEFAULT_MAP_BASE  =QWORD(0);

 //t_addr_range

{ --(Znullptr)--
 Process Address Space (40b)
 00`0000`0000 Unmapped
 00`0040`0000 System Managed - 0 passed to [sceKernel]Map{Flexible,Direct}Memory()
 07`FFFF`C000 System Reserved
 10`0000`0000 User Area - GB(64) Ranges returned on request
 FC`0000`0000 System Reserved
 FF`FFFF`FFFF

                               0x0 00400000  (win stack)
                               0x0 7FFE0000  (win kernel data)
                               0x1 00000000  (win image)
                            0x7FF8 xxxxxxxx  (win dll)
 -- System Managed
 SCE_KERNEL_PROC_IMAGE_AREA  = 0x0`00400000 - 0x0`80000000  (ET_SCE_DYNEXEC)
 SCE_KERNEL_DL_AREA          = 0x0`80000000 - 0x2`00000000
 SCE_KERNEL_HEAP_AREA        = 0x2`00000000 - 0x7`00000000
 SCE_KERNEL_STACK_AREA       = 0x7`E0000000 - 0x7`F0000000
 SCE_KERNEL_GBASE_AREA       = 0X7`FFFFC000 - 0x8`00000000
 SCE_KERNEL_SYSTEM_DL_AREA   = 0x8`00000000 - 0x8`40000000
 SCE_KERNEL_SYSTEM_HEAP_AREA = 0x8`80000000 - 0x9`00000000
 SCE_KERNEL_JIT_SHM_AREA     = 0x9`00000000 - 0xA`00000000
 SCE_KERMEL_JIT_SHM_AREA2    = 0xA`00000000 - 0xB`00000000
 SCE_KERNEL_RAZOR_GPU_AREA   = 0xF`00000000 - 0xE`C0000000
 SCE_KERNEL_GNMDRIVER_AREA   = 0xF`E0000000 - 0xF`F0000000 - GnmDriver maps things @ 0xF`Exxx`0000
 SCE_KERNEl_GNM_TESS_AREA    = 0xF`F0000000 - 0xF`F0040000
}

var
 pmap_mem:array[0..2] of t_addr_range=(
  (start:_PROC_AREA_START_1;__end:_PROC_AREA___END  ),
  (start:DL_AREA_START     ;__end:DL_AREA___END     ),
  (start:SCE_USR_HEAP_START;__end:VM_MAXUSER_ADDRESS)
 );

function pageablemem:QWORD;
function VM_MINUSER_ADDRESS:QWORD;
function PROC_IMAGE_AREA_START:QWORD;

function is_guest_addr(addr:QWORD):Boolean;

implementation

function pageablemem:QWORD;
begin
 Result:=VM_MAXUSER_ADDRESS-pmap_mem[0].start;
end;

function VM_MINUSER_ADDRESS:QWORD;
begin
 Result:=pmap_mem[0].start;
end;

function PROC_IMAGE_AREA_START:QWORD;
begin
 Result:=pmap_mem[0].start;
end;

function is_guest_addr(addr:QWORD):Boolean;
var
 i:Integer;
begin
 Result:=False;
 For i:=0 to High(pmap_mem) do
 begin
  if (addr>=pmap_mem[i].start) and (addr<pmap_mem[i].__end) then
  begin
   Exit(True);
  end;
 end;
end;

end.

