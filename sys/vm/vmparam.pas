unit vmparam;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 PAGE_SHIFT=14;
 PAGE_SIZE =1 shl PAGE_SHIFT; //16384
 PAGE_MASK =PAGE_SIZE-1;

 NBBY=8;

 NBPDR  =$1fffff;
 PDRMASK=$1fffff;

 //Virtual memory related constants, all in bytes
 MAXTSIZ =(128  *1024*1024); // max text size
 DFLDSIZ =(128  *1024*1024); // initial data size limit
 MAXDSIZ =(32768*1024*1024); // max data size
 DFLSSIZ =(8    *1024*1024); // initial stack size limit
 MAXSSIZ =(512  *1024*1024); // max stack size
 SGROWSIZ=       (128*1024); // amount to grow stack

 USRSTACK=$80000000;

 VM_MINUSER_ADDRESS=$00700000000;
 VM_MAXUSER_ADDRESS=$10000000000;

 VM_MIN_GPU_ADDRESS=$10000000000;
 VM_MAX_GPU_ADDRESS=$20000000000;

 pageablemem=VM_MAXUSER_ADDRESS-VM_MINUSER_ADDRESS;

{ --(Znullptr)--
 Process Address Space (40b)
 00`0000`0000 Unmapped
 00`0040`0000 System Managed - 0 passed to [sceKernel]Map{Flexible,Direct}Memory()
 07`FFFF`C000 System Reserved
 10`0000`0000 User Area - GB(64) Ranges returned on request
 FC`0000`0000 System Reserved
 FF`FFFF`FFFF

 -- System Managed
 SCE_KERNEL_PROC_IMAGE_AREA  = 0x0`00400000 - 0x0`80000000
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

implementation

end.

