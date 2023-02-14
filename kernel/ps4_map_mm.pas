unit ps4_map_mm;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  RWLock,
  sys_types,
  sys_crt,
  mmap,
  mm_adr_direct,
  mm_adr_virtual,
  mm_adr_name,
  Classes,
  SysUtils;

{
Flexible memory: elf sections, stack, calloc, cpu only

Direct memory: phisical mapped, cpu/gpu mem

Pooled memory: section of direct memory, 64 KiB blocks,

The application program can use a total of 5184 MiB (5824 MiB in NEO mode) physical memory.

If there is no specification, 448 MiB will be assigned as flexible memory.

Physical Address Space and Direct Memory Areas is guaranteed to be aligned to a 2 MiB boundary.


Unmapped area       : 0x0000 0000 0000 - 0x0000 0040 0000 Size: 0x0000 0040 0000 (4MB)

System managed area : 0x0000 0040 0000 - 0x0007 FFFF C000 Size: 0x0007 FFBF C000 (31GB)

System reserved area: 0x0007 FFFF C000 - 0x0010 0000 0000 Size: 0x0008 0000 4000 (32GB)

User area           : 0x0010 0000 0000 - 0x00FC 0000 0000 Size: 0x00EC 0000 0000 (944GB)

System reserved area: 0x00FC 0000 0000 - 0x00FF FFFF FFFF Size: 0x0003 FFFF FFFF (15GB)
}

//FFFF FF|FF FFFF FFFF
//0000 00|FF FFFF FFFF
//mirror |addr

var
 MMLock:System.TRTLCriticalSection;

 DirectManager :TDirectManager;
 VirtualManager:TVirtualManager;
 NamedManager  :TNamedManager;

Const
 //SCE_KERNEL_MAIN_DMEM_SIZE=$180000000; //6GB
 SCE_KERNEL_MAIN_DMEM_SIZE=$200000000; //8GB

type
 pSceKernelDirectMemoryQueryInfo=^SceKernelDirectMemoryQueryInfo;
 SceKernelDirectMemoryQueryInfo=packed record
  start:QWORD;
  __end:QWORD;
  mType:Integer;
  align:Integer;
 end;

const
 SCE_KERNEL_VIRTUAL_RANGE_NAME_SIZE=32;
 SCE_KERNEL_DMQ_FIND_NEXT=1;
 SCE_KERNEL_VQ_FIND_NEXT =1;

type
 pSceKernelVirtualQueryInfo=^SceKernelVirtualQueryInfo;
 SceKernelVirtualQueryInfo=packed record
  pstart:Pointer;
  pend  :Pointer;
  offset:QWORD;
  protection:Integer;
  memoryType:Integer;
  bits:bitpacked record
   isFlexibleMemory:0..1;
   isDirectMemory  :0..1;
   isStack         :0..1;
   isPooledMemory  :0..1;
   isCommitted     :0..1;
  end;
  name:array[0..SCE_KERNEL_VIRTUAL_RANGE_NAME_SIZE-1] of AnsiChar;
  align:array[0..6] of Byte;
 end;

const
 //SceKernelMapEntryOperation
 SCE_KERNEL_MAP_OP_MAP_DIRECT  =0;
 SCE_KERNEL_MAP_OP_UNMAP       =1;
 SCE_KERNEL_MAP_OP_PROTECT     =2;
 SCE_KERNEL_MAP_OP_MAP_FLEXIBLE=3;
 SCE_KERNEL_MAP_OP_TYPE_PROTECT=4;

type
 pSceKernelBatchMapEntry=^SceKernelBatchMapEntry;
 SceKernelBatchMapEntry=packed record
  start:Pointer;
  offset:QWORD;
  length:QWORD;
  protection:Byte;
  mtype:Byte;
  pad1:Word;
  operation:Integer;
 end;

function ps4_sceKernelGetDirectMemorySize:Int64; SysV_ABI_CDecl;
function ps4_getpagesize:Integer; SysV_ABI_CDecl;
function ps4_sceKernelAvailableFlexibleMemorySize(sizeOut:PQWORD):Integer; SysV_ABI_CDecl;
function ps4_sceKernelConfiguredFlexibleMemorySize(sizeOut:PQWORD):Integer; SysV_ABI_CDecl;

//direct

function ps4_sceKernelAllocateDirectMemory(
           searchStart:QWORD;
           searchEnd:QWORD;
           length:QWORD;
           alignment:QWORD;
           memoryType:Integer;
           physicalAddrDest:PQWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelAllocateMainDirectMemory(
           length:QWORD;
           alignment:QWORD;
           memoryType:Integer;
           physicalAddrDest:PQWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelAvailableDirectMemorySize(
           searchStart:QWORD;
           searchEnd:QWORD;
           alignment:QWORD;
           physAddrOut:PQWORD;
           sizeOut:PQWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelDirectMemoryQuery(
            offset:QWORD;
            flags:Integer;
            info:pSceKernelDirectMemoryQueryInfo;
            infoSize:QWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelGetDirectMemoryType(
            start:QWORD;
            memoryTypeOut:PInteger;
            regionStartOut:PQWORD;
            regionEndOut:PQWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelCheckedReleaseDirectMemory(start,len:QWORD):Integer; SysV_ABI_CDecl;
function ps4_sceKernelReleaseDirectMemory(start,len:QWORD):Integer; SysV_ABI_CDecl;

//mapping

function ps4_mmap(addr:Pointer;
                  len:size_t;
                  prot,flags,fd:Integer;
                  offset:size_t):Pointer; SysV_ABI_CDecl;

function ps4_sceKernelMmap(addr:Pointer;
                           len:size_t;
                           prot,flags,fd:Integer;
                           offset:size_t;
                           res:PPointer):Integer; SysV_ABI_CDecl;

function ps4_munmap(addr:Pointer;len:size_t):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMunmap(addr:Pointer;len:size_t):Integer; SysV_ABI_CDecl;

function ps4_sceKernelReleaseFlexibleMemory(addr:Pointer;len:size_t):Integer; SysV_ABI_CDecl;

function ps4_mprotect(addr:Pointer;len:size_t;prot:Integer):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMprotect(addr:Pointer;len:QWORD;prot:Integer):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMtypeprotect(addr:Pointer;len:size_t;mtype,prot:Integer):Integer; SysV_ABI_CDecl;

function ps4_sceKernelQueryMemoryProtection(addr:Pointer;
                                            pStart,pEnd:PPointer;
                                            pProt:PInteger):Integer; SysV_ABI_CDecl;

function ps4_sceKernelVirtualQuery(addr:Pointer;
                                   flags:Integer;
                                   info:pSceKernelVirtualQueryInfo;
                                   infoSize:QWORD):Integer; SysV_ABI_CDecl;

Function ps4_sceKernelSetVirtualRangeName(addr:Pointer;len:QWORD;pname:PChar):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMapFlexibleMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMapNamedFlexibleMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer;
           name:PChar):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMapNamedSystemFlexibleMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer;
           name:PChar):Integer; SysV_ABI_CDecl;

function ps4_sceKernelReserveVirtualRange(
           virtualAddrDest:PPointer;
           length:QWORD;
           flags:Integer;
           alignment:QWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMapDirectMemory2(
           virtualAddrDest:PPointer;
           length:QWORD;
           mtype,prots,flags:Integer;
           physicalAddr:QWORD;
           alignment:QWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMapDirectMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer;
           physicalAddr:QWORD;
           alignment:QWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMapNamedDirectMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer;
           physicalAddr:QWORD;
           alignment:QWORD;
           name:Pchar):Integer; SysV_ABI_CDecl;

//

function ps4_sceKernelBatchMap(
           entries:pSceKernelBatchMapEntry;
           numberOfEntries:Integer;
           numberOfEntriesOut:PInteger
           ):Integer; SysV_ABI_CDecl;

function ps4_sceKernelBatchMap2(
           entries:pSceKernelBatchMapEntry;
           numberOfEntries:Integer;
           numberOfEntriesOut:PInteger;
           flags:Integer):Integer; SysV_ABI_CDecl;

//

function ps4_mlock(addr:Pointer;len:qword):Integer; SysV_ABI_CDecl;
function ps4_munlock(addr:Pointer;len:qword):Integer; SysV_ABI_CDecl;
function ps4_mlockall(flags:Integer):Integer; SysV_ABI_CDecl;
function ps4_munlockall:Integer; SysV_ABI_CDecl;

function ps4_sceKernelMlock(addr:Pointer;len:qword):Integer; SysV_ABI_CDecl;
function ps4_sceKernelMunlock(addr:Pointer;len:qword):Integer; SysV_ABI_CDecl;
function ps4_sceKernelMlockall(flags:Integer):Integer; SysV_ABI_CDecl;
function ps4_sceKernelMunlockall:Integer; SysV_ABI_CDecl;

//

function ps4_msync(addr:Pointer;len:size_t;flags:Integer):Integer; SysV_ABI_CDecl;
function ps4_sceKernelMsync(addr:Pointer;len:size_t;flags:Integer):Integer; SysV_ABI_CDecl;

type
 TGpuMemAlloc=function(addr:Pointer;len:size_t):Pointer;
 TGpuMemFree =procedure(h:Pointer);

 TGpuMemBlock=record
  pAddr:Pointer;
  nSize:Int64;
  Handle:Pointer;
 end;

 TGpuMemCb=record
  Alloc:TGpuMemAlloc;
  Free :TGpuMemFree;
 end;

var
 GpuMemCb:TGpuMemCb;

Function  TryGetGpuMemBlockByAddr(addr:Pointer;var block:TGpuMemBlock):Boolean;
Procedure RegistredStack;
Procedure UnRegistredStack;

var
 SceKernelFlexibleMemorySize:QWORD=0;

Procedure _mem_init;
Procedure _mem_print;

implementation

uses
 ps4_program,
 sys_kernel,
 sys_signal;

Procedure INIT_MLOCK; inline;
begin
 System.InitCriticalSection(MMLock);
end;

Procedure MLOCK; inline;
begin
 System.EnterCriticalSection(MMLock);
end;

Procedure MUNLOCK; inline;
begin
 System.LeaveCriticalSection(MMLock);
end;

function IsPowerOfTwo(x:QWORD):Boolean; inline;
begin
 Result:=(x and (x - 1))=0;
end;

function fastIntLog2(i:QWORD):QWORD; inline;
begin
 Result:=BsfQWORD(i);
end;

function str_mem_type(memoryType:Integer):RawByteString;
begin
 Result:='';
 Case memoryType of
  SCE_KERNEL_WB_ONION :Result:='WB_ONION';
  SCE_KERNEL_WC_GARLIC:Result:='WC_GARLIC';
  SCE_KERNEL_WB_GARLIC:Result:='WB_GARLIC';
  else
                       Result:=IntToStr(memoryType);
 end;
end;

function test_KP_flags(flags:Integer):RawByteString;
begin
 Result:='';
 if (flags and SCE_KERNEL_PROT_CPU_READ) <>0 then Result:=Result+' CPU_READ';
 if (flags and SCE_KERNEL_PROT_CPU_WRITE)<>0 then Result:=Result+' CPU_WRIT';
 if (flags and SCE_KERNEL_PROT_CPU_EXEC) <>0 then Result:=Result+' CPU_EXEC';
 if (flags and SCE_KERNEL_PROT_GPU_READ) <>0 then Result:=Result+' GPU_READ';
 if (flags and SCE_KERNEL_PROT_GPU_WRITE)<>0 then Result:=Result+' GPU_WRIT';
end;

Function TryGetGpuMemBlockByAddr(addr:Pointer;var block:TGpuMemBlock):Boolean;
var
 pb:PVirtualAdrBlock;
label
 __exit;
begin
 Result:=False;

 pb:=nil;

 MLOCK;

 if VirtualManager.TryGetMapBlockByAddr(addr,pb) then
 begin
  if (not pb^.isgpu) {(pb^.F.btype<>BT_GPUM)} then goto __exit;

  if (pb^.Handle=nil) then
  begin
   if (GpuMemCb.Alloc=nil) then goto __exit;
   pb^.Handle:=GpuMemCb.Alloc(pb^.Offset,pb^.Size);
   if (pb^.Handle=nil) then goto __exit;
  end;

  block.pAddr :=pb^.Offset;
  block.nSize :=pb^.Size;
  block.Handle:=pb^.Handle;
  Result:=true;

 end;

 __exit:
 MUNLOCK;
end;

function __free_block(block:PVirtualAdrBlock):Integer;
begin
 Result:=0;
 if (block=nil) then Exit;
 //if (block^.F.btype<>BT_GPUM) then Exit;
 if (block^.Handle<>nil) then Exit;

 if (GpuMemCb.Free<>nil) then Exit;

 GpuMemCb.Free(block^.Handle);

 block^.Handle:=nil;
end;

Procedure RegistredStack;
//var
// block:PBlock;
begin
 //MLOCK;
 //block:=AllocMem(SizeOf(TBlock));
 //if (block=nil) then Exit;
 //block^.pAddr:=StackBottom;
 //block^.nSize:=StackLength;
 //block^.bType:=BT_STACK;
 //PageMM.FMapBlockSet.Insert(block);
 //MUNLOCK;
end;

Procedure UnRegistredStack;
begin
 //MLOCK;
 //PageMM._DeleteBlockByAddr(StackBottom);
 //MUNLOCK;
end;

function ps4_sceKernelGetDirectMemorySize:Int64; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_MAIN_DMEM_SIZE;
 _set_errno(0);
end;

function ps4_getpagesize:Integer; SysV_ABI_CDecl;
begin
 Result:=PHYSICAL_PAGE_SIZE;
 _set_errno(0);
end;

function ps4_sceKernelAvailableFlexibleMemorySize(sizeOut:PQWORD):Integer; SysV_ABI_CDecl;
var
 flex:QWORD;
begin
 Result:=0;
 if (sizeOut=nil) then
 begin
  _set_errno(EINVAL);
  Exit(SCE_KERNEL_ERROR_EINVAL);
 end;

 _sig_lock;
 MLOCK;

 flex:=VirtualManager.stat.flex;

 MUNLOCK;
 _sig_unlock;

 if (flex<SceKernelFlexibleMemorySize) then
 begin
  flex:=SceKernelFlexibleMemorySize-flex;
 end else
 begin
  flex:=0;
 end;

 sizeOut^:=flex;
end;

function ps4_sceKernelConfiguredFlexibleMemorySize(sizeOut:PQWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 if (sizeOut=nil) then
 begin
  _set_errno(EINVAL);
  Exit(SCE_KERNEL_ERROR_EINVAL);
 end;
 sizeOut^:=SceKernelFlexibleMemorySize;
end;

function _test_mtype(mtype:Integer):Boolean; inline;
begin
 Case mtype of
  SCE_KERNEL_WB_ONION..SCE_KERNEL_WB_GARLIC:Result:=True;
  else
   Result:=False;
 end;
end;

//direct

function _sceKernelAllocateDirectMemory(
           searchStart:QWORD;
           searchEnd:QWORD;
           length:QWORD;
           alignment:QWORD;
           memoryType:Integer;
           physicalAddrDest:PQWORD):Integer;
begin
 Result:=EINVAL;

 Writeln('srchd:',HexStr(searchStart,10),'..',HexStr(searchEnd,10),' len:',HexStr(length,10));
 Writeln('align:',HexStr(alignment,10),' ','mType:',str_mem_type(memoryType));

 if (physicalAddrDest=nil) or (length=0) or (searchEnd<=searchStart) then Exit;

 if (searchEnd>SCE_KERNEL_MAIN_DMEM_SIZE) then Exit;

 if (alignment=0) then alignment:=LOGICAL_PAGE_SIZE;

 if not IsAlign(length   ,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(alignment,LOGICAL_PAGE_SIZE) then Exit;
 if not IsPowerOfTwo(alignment)              then Exit;
 if (fastIntLog2(alignment)>31)              then Exit;

 if not _test_mtype(memoryType) then Exit;

 searchStart:=AlignUp(searchStart,LOGICAL_PAGE_SIZE);

 _sig_lock;
 MLOCK;

 Result:=DirectManager.Alloc(searchStart,searchEnd,length,alignment,Byte(memoryType),physicalAddrDest^);

 MUNLOCK;
 _sig_unlock;
end;

function _sceKernelAllocateMainDirectMemory(
           length:QWORD;
           alignment:QWORD;
           memoryType:Integer;
           physicalAddrDest:PQWORD):Integer;
begin
 Result:=EINVAL;

 Writeln('srchm: len:',HexStr(length,10));
 Writeln('align:',HexStr(alignment,10),' ','mType:',str_mem_type(memoryType));

 if (physicalAddrDest=nil) or (length=0) then Exit;

 if (alignment=0) then alignment:=LOGICAL_PAGE_SIZE;

 if not IsAlign(length   ,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(alignment,LOGICAL_PAGE_SIZE) then Exit;
 if not IsPowerOfTwo(alignment)              then Exit;
 if (fastIntLog2(alignment)>31)              then Exit;

 if not _test_mtype(memoryType) then Exit;

 _sig_lock;
 MLOCK;

 Result:=DirectManager.Alloc(length,alignment,Byte(memoryType),physicalAddrDest^);

 MUNLOCK;
 _sig_unlock;
end;

function _sceKernelAvailableDirectMemorySize(
            searchStart:QWORD;
            searchEnd:QWORD;
            alignment:QWORD;
            physAddrOut:PQWORD;
            sizeOut:PQWORD):Integer;
var
 FAdrOut,FSizeOut:QWORD;
begin
 Result:=EINVAL;

 Writeln('srchd:',HexStr(searchStart,10),'..',HexStr(searchEnd,10),' align:',HexStr(alignment,10));

 if (searchEnd<=searchStart) then Exit;

 if (searchEnd>SCE_KERNEL_MAIN_DMEM_SIZE) then Exit;

 if (alignment=0) then alignment:=LOGICAL_PAGE_SIZE;

 if not IsAlign(searchStart,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(searchEnd  ,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(alignment  ,LOGICAL_PAGE_SIZE) then Exit;
 if not IsPowerOfTwo(alignment)                then Exit;
 if (fastIntLog2(alignment)>31)                then Exit;

 searchStart:=AlignUp(searchStart,LOGICAL_PAGE_SIZE);

 FAdrOut :=0;
 FSizeOut:=0;

 _sig_lock;
 MLOCK;

 Result:=DirectManager.CheckedAvailable(searchStart,searchEnd,alignment,FAdrOut,FSizeOut);

 MUNLOCK;
 _sig_unlock;

 if (Result=0) then
 begin
  if (physAddrOut<>nil) then
  begin
   physAddrOut^:=FAdrOut;
  end;
  if (sizeOut<>nil) then
  begin
   sizeOut^:=FSizeOut;
  end;
 end;
end;

function _sceKernelDirectMemoryQuery(
            offset:QWORD;
            flags:Integer;
            info:pSceKernelDirectMemoryQueryInfo;
            infoSize:QWORD):Integer;
var
 ROut:TDirectAdrNode;
begin
 Result:=EINVAL;

 if (info=nil) or (infoSize<>SizeOf(SceKernelDirectMemoryQueryInfo)) then Exit;

 if not IsAlign(offset,LOGICAL_PAGE_SIZE) then Exit;

 ROut:=Default(TDirectAdrNode);

 _sig_lock;
 MLOCK;

 Result:=DirectManager.Query(offset,(flags=SCE_KERNEL_DMQ_FIND_NEXT),ROut);

 MUNLOCK;
 _sig_unlock;

 info^:=Default(SceKernelDirectMemoryQueryInfo);

 if (Result=0) then
 begin
  info^.start:=ROut.Offset;
  info^.__end:=ROut.Offset+ROut.Size;
  info^.mType:=ROut.F.mtype;
 end;
end;

function _sceKernelGetDirectMemoryType(
            start:QWORD;
            memoryTypeOut:PInteger;
            regionStartOut:PQWORD;
            regionEndOut:PQWORD):Integer;
var
 ROut:TDirectAdrNode;
begin
 Result:=EINVAL;

 if (memoryTypeOut=nil) then Exit;
 if (regionStartOut=nil) then Exit;
 if (regionEndOut=nil) then Exit;

 start:=AlignDw(start,PHYSICAL_PAGE_SIZE);

 ROut:=Default(TDirectAdrNode);

 _sig_lock;
 MLOCK;

 Result:=DirectManager.QueryMType(start,ROut);

 MUNLOCK;
 _sig_unlock;

 if (Result=0) then
 begin
  memoryTypeOut ^:=ROut.F.mtype;
  regionStartOut^:=ROut.Offset;
  regionEndOut  ^:=ROut.Offset+ROut.Size;
 end;
end;

function _sceKernelCheckedReleaseDirectMemory(start,len:QWORD):Integer;
begin
 Result:=EINVAL;

 if not IsAlign(start,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(len  ,LOGICAL_PAGE_SIZE) then Exit;

 _sig_lock;
 MLOCK;

 Result:=DirectManager.CheckedRelease(start,len);
 if (Result=0) then
 begin
  Result:=DirectManager.Release(start,len);
 end;

 MUNLOCK;
 _sig_unlock;
end;

function _sceKernelReleaseDirectMemory(start,len:QWORD):Integer;
begin
 Result:=EINVAL;

 if not IsAlign(start,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(len  ,LOGICAL_PAGE_SIZE) then Exit;

 _sig_lock;
 MLOCK;

 Result:=DirectManager.Release(start,len);

 MUNLOCK;
 _sig_unlock;
end;

//

function ps4_sceKernelAllocateDirectMemory(
           searchStart:QWORD;
           searchEnd:QWORD;
           length:QWORD;
           alignment:QWORD;
           memoryType:Integer;
           physicalAddrDest:PQWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelAllocateDirectMemory(
            searchStart,
            searchEnd,
            length,
            alignment,
            memoryType,
            physicalAddrDest);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelAllocateDirectMemory:',Result);
 end;
 _set_errno(Result);

 Result:=px2sce(Result);
end;

function ps4_sceKernelAllocateMainDirectMemory(
           length:QWORD;
           alignment:QWORD;
           memoryType:Integer;
           physicalAddrDest:PQWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelAllocateMainDirectMemory(
            length,
            alignment,
            memoryType,
            physicalAddrDest);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelAllocateMainDirectMemory:',Result);
 end;
 _set_errno(Result);

 Result:=px2sce(Result);
end;

function ps4_sceKernelAvailableDirectMemorySize(
            searchStart:QWORD;
            searchEnd:QWORD;
            alignment:QWORD;
            physAddrOut:PQWORD;
            sizeOut:PQWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelAvailableDirectMemorySize(
            searchStart,
            searchEnd,
            alignment,
            physAddrOut,
            sizeOut);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelAvailableDirectMemorySize:',Result);
 end;
 _set_errno(Result);

 Result:=px2sce(Result);
end;

function ps4_sceKernelDirectMemoryQuery(
            offset:QWORD;
            flags:Integer;
            info:pSceKernelDirectMemoryQueryInfo;
            infoSize:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelDirectMemoryQuery(
             offset,
             flags,
             info,
             infoSize);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelDirectMemoryQuery:',Result);
 end;
 _set_errno(Result);

 Result:=px2sce(Result);
end;

function ps4_sceKernelGetDirectMemoryType(
            start:QWORD;
            memoryTypeOut:PInteger;
            regionStartOut:PQWORD;
            regionEndOut:PQWORD):Integer; SysV_ABI_CDecl;

begin
 Result:=_sceKernelGetDirectMemoryType(
             start,
             memoryTypeOut,
             regionStartOut,
             regionEndOut);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelGetDirectMemoryType:',Result);
 end;
 _set_errno(Result);

 Result:=px2sce(Result);
end;

function ps4_sceKernelCheckedReleaseDirectMemory(start,len:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelCheckedReleaseDirectMemory(start,len);

 if (Result<>0) then
 begin
  Result:=_sceKernelCheckedReleaseDirectMemory(start,len);
  Writeln(StdWrn,'[WARN]:sceKernelCheckedReleaseDirectMemory:',Result);
 end;
 _set_errno(Result);

 Result:=px2sce(Result);
end;

function ps4_sceKernelReleaseDirectMemory(start,len:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelReleaseDirectMemory(start,len);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelReleaseDirectMemory:',Result);
 end;
 _set_errno(Result);

 Result:=px2sce(Result);
end;

//mapping

//flag:MAP_VOID   fd=-1                               //reserve
//flag:MAP_ANON   fd=-1                               //flex
//flag:MAP_SHARED fd=/dev/dmem%d offset=physicalAddr  //direct

function __mmap(addr:Pointer;len,align:size_t;prot,flags,fd:Integer;offset:size_t;var res:Pointer):Integer;
begin
 Result:=EINVAL;

 if not IsAlign(addr  ,PHYSICAL_PAGE_SIZE) then Exit;
 //if not IsAlign(len   ,PHYSICAL_PAGE_SIZE) then Exit;
 if not IsAlign(offset,PHYSICAL_PAGE_SIZE) then Exit;

 if (align<PHYSICAL_PAGE_SIZE) then align:=PHYSICAL_PAGE_SIZE;

 _sig_lock;
 MLOCK;

 if (flags and MAP_VOID)<>0 then //reserved
 begin
  Result:=VirtualManager.mmap(addr,len,align,prot,flags,-1,0,res);
 end else
 if (flags and MAP_ANON)<>0 then //flex
 begin
  Result:=VirtualManager.mmap(addr,len,align,prot,flags,-1,0,res);
 end else
 if (fd>=0) then
 begin
  if (fd=0) then //direct (psevdo dmem fd=0)
  begin
   Result:=DirectManager.CheckedMMap(offset,len);

   if (Result=0) then
   begin

    Result:=VirtualManager.mmap(addr,len,align,prot,flags,fd,offset,res);

    if (Result=0) then
    begin
     Result:=DirectManager.mmap_addr(offset,len,res);
    end;

   end;
  end else
  begin //map file
   Result:=VirtualManager.mmap(addr,len,align,prot,flags,fd,offset,res);
  end;
 end;

 MUNLOCK;
 _sig_unlock;
end;

function _mmap(addr:Pointer;
               len:size_t;
               prots,flags,fd:Integer;
               offset:size_t;
               var res:Pointer):Integer;
var
 align:size_t;
begin
 Result:=EINVAL;

 if ((prots and $ffffffc8)<>0) then Exit;

 if not IsAlign(addr,PHYSICAL_PAGE_SIZE) then Exit;

 align:=(flags and MAP_ALIGNMENT_MASK) shr MAP_ALIGNMENT_BIT;
 align:=1 shl align;

 flags:=flags and (not MAP_ALIGNMENT_MASK);

 Result:=__mmap(addr,len,align,prots,flags,fd,offset,res);
end;

function __sys_mmap_dmem(
           addr:Pointer;
           length:QWORD;
           alignment:QWORD;
           mtype,prots,flags:Integer;
           physicalAddr:QWORD;
           var res:Pointer):Integer;
begin
 Result:=0;

 _sig_lock;
 MLOCK;

 Result:=DirectManager.CheckedMMap(physicalAddr,length);

 if (Result=0) then
 begin
  Result:=VirtualManager.mmap(addr,length,alignment,prots,flags,0,physicalAddr,res);

  if (Result=0) then
  begin
   Result:=DirectManager.mmap_addr(physicalAddr,length,res,mtype);
  end;
 end;

 MUNLOCK;
 _sig_unlock;
end;

function __munmap(addr:Pointer;len:size_t):Integer;
begin
 Result:=VirtualManager.Release(addr,len,True);

 if (Result=0) then
 begin
  NamedManager.Mname(addr,len,nil);
 end;
end;

function __unmap_direct(Offset,Size:QWORD):Integer;
begin
 Result:=DirectManager.unmap_addr(Offset,Size);
end;

function __mtype_direct(Offset,Size:QWORD;mtype:Integer):Integer;
begin
 Result:=DirectManager.mmap_type(Offset,Size,mtype);
end;

function _munmap(addr:Pointer;len:size_t):Integer;
begin
 Result:=EINVAL;

 _sig_lock;
 MLOCK;

 Result:=VirtualManager.Release(addr,len,False);

 if (Result=0) then
 begin
  NamedManager.Mname(addr,len,nil);
 end;

 MUNLOCK;
 _sig_unlock;
end;

function _mprotect(addr:Pointer;len:size_t;prot:Integer):Integer;
begin
 Result:=EINVAL;

 _sig_lock;
 MLOCK;

 Result:=VirtualManager.Protect(addr,len,prot);

 MUNLOCK;
 _sig_unlock;
end;

function _sys_mtypeprotect(addr:Pointer;len:size_t;mtype,prot:Integer):Integer;
begin
 Result:=EINVAL;

 _sig_lock;
 MLOCK;

 Result:=VirtualManager.Mtypeprotect(addr,len,mtype,prot);

 MUNLOCK;
 _sig_unlock;
end;

function _sys_query_memory_protection(addr:Pointer;
                                      pStart,pEnd:PPointer;
                                      pProt:PInteger):Integer;
var
 ROut:TVirtualAdrNode;
begin
 Result:=0;

 ROut:=Default(TVirtualAdrNode);

 _sig_lock;
 MLOCK;

 Result:=VirtualManager.QueryProt(addr,ROut);

 MUNLOCK;
 _sig_unlock;

 if (Result=0) then
 begin
  if (pStart<>nil) then
  begin
   pStart^:=ROut.Offset;
  end;

  if (pEnd<>nil) then
  begin
   pEnd  ^:=ROut.Offset+ROut.Size;
  end;

  if (pProt<>nil) then
  begin
   pProt ^:=ROut.F.prot;
  end;
 end;
end;

function _sys_virtual_query(addr:Pointer;
                            flags:Integer;
                            info:pSceKernelVirtualQueryInfo;
                            infoSize:QWORD):Integer;
var
 VOut:TVirtualAdrNode;
 DOut:TDirectAdrNode;
 Name:TName;
 Committed:Boolean;
begin
 Result:=EFAULT;

 if (info=nil) then Exit;
 if (infoSize<>SizeOf(SceKernelVirtualQueryInfo)) then Exit;

 VOut:=Default(TVirtualAdrNode);
 DOut:=Default(TDirectAdrNode);
 Name:=Default(TName);

 _sig_lock;
 MLOCK;

 Result:=VirtualManager.Query(addr,(flags=SCE_KERNEL_VQ_FIND_NEXT),VOut);

 if (Result=0) and (VOut.F.direct=1) then
 begin
  Result:=DirectManager.QueryMType(VOut.addr,DOut);
 end;

 if (Result=0) then
 begin
  NamedManager.Query(addr,@name);
 end;

 MUNLOCK;
 _sig_unlock;

 info^:=Default(SceKernelVirtualQueryInfo);

 if (Result=0) then
 begin
  Committed:=(VOut.F.Free=0) and (VOut.F.reserv=0);
  info^.pstart               :=VOut.Offset;
  info^.pend                 :=VOut.Offset+VOut.Size;
  info^.offset               :=VOut.addr;
  info^.protection           :=VOut.F.prot;
  info^.memoryType           :=DOut.F.mtype;
  info^.bits.isFlexibleMemory:=Byte((VOut.F.direct=0) and Committed);
  info^.bits.isDirectMemory  :=VOut.F.direct;
  info^.bits.isStack         :=VOut.F.stack;
  info^.bits.isPooledMemory  :=VOut.F.polled;
  info^.bits.isCommitted     :=Byte(Committed);
  info^.name                 :=Name;
 end;
end;

Function _sys_mname(addr:Pointer;len:QWORD;pname:PChar):Integer;
begin
 Result:=EFAULT;

 if (pname=nil) then Exit;
 if (StrLen(pname)>32) then Exit;

 _sig_lock;
 MLOCK;

 Result:=NamedManager.Mname(addr,len,pname);

 MUNLOCK;
 _sig_unlock;
end;

function _sceKernelMapFlexibleMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer):Integer;
var
 addr:Pointer;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;

 if ((flags and $ffbfff6f)<>0) then Exit;
 if ((prots and $ffffffc8)<>0) then Exit;

 if (length<LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(length,LOGICAL_PAGE_SIZE) then Exit;

 addr:=virtualAddrDest^;
 if not IsAlign(addr,LOGICAL_PAGE_SIZE) then Exit;

 if (((flags and MAP_FIXED) <> 0) and (addr=nil)) then
 begin
  if (SDK_VERSION > $16fffff) then
  begin
   Exit(SCE_KERNEL_ERROR_EINVAL);
  end;
  flags:=flags and $ffffffef;
  Writeln(StdWrn,'[WARNING] map(addr=0, flags=MAP_FIXED)');
 end;

 if (((flags and MAP_FIXED)=0) and (addr=nil)) then
 begin
  addr:=Pointer($880000000);
 end;

 Result:=__mmap(addr,length,0,prots,flags or MAP_ANON,-1,0,addr);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Result:=px2sce(Result);
 end else
 begin
  virtualAddrDest^:=addr;
  Result:=0;
 end;
end;

function _sceKernelMapNamedSystemFlexibleMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer;
           name:PChar):Integer;
var
 addr:Pointer;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;

 if ((flags and $ffbfff6f)<>0) then Exit;
 if ((prots and $ffffffc8)<>0) then Exit;

 if (length<LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(length,LOGICAL_PAGE_SIZE) then Exit;

 addr:=virtualAddrDest^;
 if not IsAlign(addr,LOGICAL_PAGE_SIZE) then Exit;

 if (((flags and MAP_FIXED) <> 0) and (addr=nil)) then
 begin
  if (SDK_VERSION > $16fffff) then
  begin
   Exit(SCE_KERNEL_ERROR_EINVAL);
  end;
  flags:=flags and $ffffffef;
  Writeln('[WARNING] map(addr=0, flags=MAP_FIXED)');
 end;

 if (((flags and MAP_FIXED)=0) and (addr=nil)) then
 begin
  addr:=Pointer($880000000);
 end;

 Result:=__mmap(addr,length,0,prots,flags or MAP_ANON or MAP_SYSTEM,-1,0,addr);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Result:=px2sce(Result);
 end else
 begin
  _sys_mname(addr,length,name);
  virtualAddrDest^:=addr;
  Result:=0;
 end;
end;

function _sceKernelReserveVirtualRange(
           virtualAddrDest:PPointer;
           length:QWORD;
           flags:Integer;
           alignment:QWORD):Integer;
var
 addr:Pointer;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;

 if ((flags and $ffbfff6f)<>0) then Exit;

 if (length<LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(length   ,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(alignment,LOGICAL_PAGE_SIZE) then Exit;
 if not IsPowerOfTwo(alignment) then Exit;

 if (alignment<LOGICAL_PAGE_SIZE) then alignment:=LOGICAL_PAGE_SIZE;
 if (fastIntLog2(alignment)>31) then Exit;

 addr:=virtualAddrDest^;
 if not IsAlign(addr,LOGICAL_PAGE_SIZE) then Exit;

 if (((flags and MAP_FIXED)<>0) and (addr=nil)) then
 begin
  if (SDK_VERSION > $16fffff) then
  begin
   Exit(SCE_KERNEL_ERROR_EINVAL);
  end;
  flags:=flags and $ffffffef;
  Writeln('[WARNING] map(addr=0, flags=MAP_FIXED)');
 end;

 if (((flags and MAP_FIXED)=0) and (addr=nil)) then
 begin
  addr:=Pointer($880000000);
 end;

 Result:=__mmap(addr,length,alignment,0,flags or MAP_VOID or MAP_SHARED,-1,0,addr);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Result:=px2sce(Result);
 end else
 begin
  virtualAddrDest^:=addr;
  Result:=0;
 end;
end;

function _sceKernelMapDirectMemory2(
           virtualAddrDest:PPointer;
           length:QWORD;
           mtype,prots,flags:Integer;
           physicalAddr:QWORD;
           alignment:QWORD):Integer;
var
 addr:Pointer;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;

 if ((flags and $1f000000)<>0) then Exit;
 if ((prots and $ffffffc8)<>0) then Exit;

 if (length<LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(length      ,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(physicalAddr,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(alignment   ,LOGICAL_PAGE_SIZE) then Exit;
 if not IsPowerOfTwo(alignment) then Exit;

 if (alignment<LOGICAL_PAGE_SIZE) then alignment:=LOGICAL_PAGE_SIZE;
 if (fastIntLog2(alignment)>31) then Exit;

 addr:=virtualAddrDest^;
 if not IsAlign(addr,LOGICAL_PAGE_SIZE) then Exit;

 if (((flags and MAP_FIXED)=0) and (addr=nil)) then
 begin
  addr:=Pointer($880000000);
 end;

 Result:=__sys_mmap_dmem(addr,length,alignment,mtype,prots,flags,physicalAddr,addr);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Result:=px2sce(Result);
 end else
 begin
  virtualAddrDest^:=addr;
  Result:=0;
 end;
end;

function _sceKernelMapDirectMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer;
           physicalAddr:QWORD;
           alignment:QWORD):Integer;
var
 addr:Pointer;
 _flags:Integer;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;

 if ((physicalAddr < $3000000000) or (physicalAddr > $301fffffff)) and
    ((flags and SCE_KERNEL_MAP_DMEM_COMPAT)=0) and (SDK_VERSION > $24fffff) then
 begin
  Result:=_sceKernelMapDirectMemory2(virtualAddrDest,length,-1,prots,flags,physicalAddr,alignment);
  Exit;
 end;

 if ((flags and $1f000000)<>0) then Exit;
 if ((prots and $ffffffc8)<>0) then Exit;

 if (length<LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(length      ,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(physicalAddr,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(alignment   ,LOGICAL_PAGE_SIZE) then Exit;
 if not IsPowerOfTwo(alignment) then Exit;

 if (alignment<LOGICAL_PAGE_SIZE) then alignment:=LOGICAL_PAGE_SIZE;
 if (fastIntLog2(alignment)>31) then Exit;

 addr:=virtualAddrDest^;
 if not IsAlign(addr,LOGICAL_PAGE_SIZE) then Exit;

 _flags:=flags and $fffffbff;

 if (((flags and MAP_FIXED) <> 0) and (addr=nil)) then
 begin
  if (SDK_VERSION > $16fffff) then
  begin
   Exit(SCE_KERNEL_ERROR_EINVAL);
  end;
  _flags:=flags and $fffffbef;
  Writeln('[WARNING] map(addr=0, flags=MAP_FIXED)');
 end;

 if (((flags and MAP_FIXED)=0) and (addr=nil)) then
 begin
  addr:=Pointer($880000000);
 end;

 Result:=__mmap(addr,length,alignment,prots,_flags or MAP_SHARED,0,physicalAddr,addr);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Result:=px2sce(Result);
 end else
 begin
  virtualAddrDest^:=addr;
  Result:=0;
 end;
end;

////
////

function ps4_mmap(addr:Pointer;
                  len:size_t;
                  prot,flags,fd:Integer;
                  offset:size_t):Pointer; SysV_ABI_CDecl;
var
 err:Integer;
begin

 if (((flags and MAP_FIXED)=0) and (addr=nil)) then
 begin
  addr:=Pointer($880000000);
 end;

 err:=_mmap(addr,len,prot,flags,fd,offset,addr);
 _set_errno(err);

 if (err=0) then
 begin
  Result:=addr;
 end else
 begin
  Writeln(StdWrn,'[WARN]:mmap:',err);
  Result:=MAP_FAILED;
 end;
end;

function ps4_sceKernelMmap(addr:Pointer;
                           len:size_t;
                           prot,flags,fd:Integer;
                           offset:size_t;
                           res:PPointer):Integer; SysV_ABI_CDecl;
begin

 if (((flags and MAP_FIXED)=0) and (addr=nil)) then
 begin
  addr:=Pointer($880000000);
 end;

 Result:=_mmap(addr,len,prot,flags,fd,offset,addr);
 _set_errno(Result);

 if (Result=0) then
 begin
  res^:=addr;
 end else
 begin
  Writeln(StdWrn,'[WARN]:sceKernelMmap:',Result);
 end;

 Result:=px2sce(Result);
end;

function ps4_munmap(addr:Pointer;len:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=_munmap(addr,len);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:munmap:',Result);
 end;

 Result:=_set_errno(Result);
end;

function ps4_sceKernelMunmap(addr:Pointer;len:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=_munmap(addr,len);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelMunmap:',Result);
 end;

 Result:=px2sce(Result);
end;

function ps4_sceKernelReleaseFlexibleMemory(addr:Pointer;len:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=_munmap(addr,len);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelReleaseFlexibleMemory:',Result);
 end;

 Result:=px2sce(Result);
end;

function ps4_mprotect(addr:Pointer;
                      len:size_t;
                      prot:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=_mprotect(addr,len,prot);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:mprotect:',Result);
 end;

 Result:=_set_errno(Result);
end;

function ps4_sceKernelMprotect(addr:Pointer;
                               len:size_t;
                               prot:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=_mprotect(addr,len,prot);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelMprotect:',Result);
 end;

 Result:=px2sce(Result);
end;

function ps4_sceKernelMtypeprotect(addr:Pointer;
                                   len:size_t;
                                   mtype,prot:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=_sys_mtypeprotect(addr,len,mtype,prot);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelMtypeprotect:',Result);
 end;

 Result:=px2sce(Result);
end;

function ps4_sceKernelQueryMemoryProtection(addr:Pointer;
                                            pStart,pEnd:PPointer;
                                            pProt:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=_sys_query_memory_protection(addr,pStart,pEnd,pProt);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelQueryMemoryProtection:',Result);
 end;

 Result:=px2sce(Result);
end;

function ps4_sceKernelVirtualQuery(addr:Pointer;
                                   flags:Integer;
                                   info:pSceKernelVirtualQueryInfo;
                                   infoSize:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=_sys_virtual_query(addr,flags,info,infoSize);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelVirtualQuery:',Result);
 end;

 Result:=px2sce(Result);
end;

Function ps4_sceKernelSetVirtualRangeName(addr:Pointer;len:QWORD;pname:PChar):Integer; SysV_ABI_CDecl;
begin
 Result:=_sys_mname(addr,len,pname);
 _set_errno(Result);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelSetVirtualRangeName:',Result);
 end;

 Result:=px2sce(Result);
end;

function ps4_sceKernelMapFlexibleMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelMapFlexibleMemory(virtualAddrDest,length,prots,flags);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelMapFlexibleMemory:',Result);
 end;
end;

function ps4_sceKernelMapNamedFlexibleMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer;
           name:PChar):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelMapFlexibleMemory(virtualAddrDest,length,prots,flags);

 if (Result=0) then
 begin
  _sys_mname(virtualAddrDest^,length,name);
 end else
 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelMapNamedFlexibleMemory:',Result);
 end;
end;

function ps4_sceKernelMapNamedSystemFlexibleMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer;
           name:PChar):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelMapNamedSystemFlexibleMemory(virtualAddrDest,length,prots,flags,name);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelMapNamedSystemFlexibleMemory:',Result);
 end;
end;

function ps4_sceKernelReserveVirtualRange(
           virtualAddrDest:PPointer;
           length:QWORD;
           flags:Integer;
           alignment:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelReserveVirtualRange(virtualAddrDest,length,flags,alignment);

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelReserveVirtualRange:',Result);
 end;
end;

function ps4_sceKernelMapDirectMemory2(
           virtualAddrDest:PPointer;
           length:QWORD;
           mtype,prots,flags:Integer;
           physicalAddr:QWORD;
           alignment:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelMapDirectMemory2(virtualAddrDest,length,mtype,prots,flags,physicalAddr,alignment);

 if _isgpu(prots) then
 begin
  Writeln('GPU:',HexStr(virtualAddrDest^),'..',HexStr(virtualAddrDest^+length));
 end;

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelMapDirectMemory2:',Result);
 end;
end;

function ps4_sceKernelMapDirectMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer;
           physicalAddr:QWORD;
           alignment:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelMapDirectMemory(virtualAddrDest,length,prots,flags,physicalAddr,alignment);

 if _isgpu(prots) then
 begin
  Writeln('GPU:',HexStr(virtualAddrDest^),'..',HexStr(virtualAddrDest^+length));
 end;

 if (Result<>0) then
 begin
  Writeln(StdWrn,'[WARN]:sceKernelMapDirectMemory:',Result);
 end;
end;

function ps4_sceKernelMapNamedDirectMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           prots,flags:Integer;
           physicalAddr:QWORD;
           alignment:QWORD;
           name:Pchar):Integer; SysV_ABI_CDecl;
begin
 Result:=_sceKernelMapDirectMemory(virtualAddrDest,length,prots,flags,physicalAddr,alignment);

 if _isgpu(prots) then
 begin
  Writeln('GPU:',HexStr(virtualAddrDest^),'..',HexStr(virtualAddrDest^+length));
 end;

 if (Result=0) then
 begin
  _sys_mname(virtualAddrDest^,length,name);
 end else
 begin
  Writeln(StdWrn,'[WARN]:sceKernelMapNamedDirectMemory:',Result);
 end;
end;

//

function ps4_sceKernelBatchMap(
           entries:pSceKernelBatchMapEntry;
           numberOfEntries:Integer;
           numberOfEntriesOut:PInteger
           ):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_sceKernelBatchMap2(
            entries,
            numberOfEntries,
            numberOfEntriesOut,
            MAP_FIXED);
//There is a function with the same name exported from the libkernel_pre250mmap
// library, it differs only in that the flag is 0x410 (MAP_FIXED or MAP_STACK)
end;

function ps4_sceKernelBatchMap2(
           entries:pSceKernelBatchMapEntry;
           numberOfEntries:Integer;
           numberOfEntriesOut:PInteger;
           flags:Integer):Integer; SysV_ABI_CDecl;
label
 _exit,_exit_lock;
var
 i:Integer;
begin
 if (entries=nil) then
 begin
  numberOfEntries:=0;
  _set_errno(EFAULT);
  Result:=SCE_KERNEL_ERROR_EFAULT;
  goto _exit;
 end;
 if (numberOfEntries=0) then
 begin
  numberOfEntries:=0;
  _set_errno(EINVAL);
  Result:=SCE_KERNEL_ERROR_EINVAL;
  goto _exit;
 end;

 _sig_lock;
 MLOCK;

 For i:=0 to numberOfEntries-1 do
 begin
  Case entries[i].operation of
   SCE_KERNEL_MAP_OP_MAP_DIRECT:
    begin
     Result:=ps4_sceKernelMapDirectMemory(
               @entries[i].start,
               entries[i].length,
               entries[i].protection,
               flags,
               entries[i].offset,
               0);
    end;
   SCE_KERNEL_MAP_OP_UNMAP:
    begin
     Result:=ps4_sceKernelMunmap(entries[i].start,
                                 entries[i].length);
    end;
   SCE_KERNEL_MAP_OP_PROTECT:
    begin
     Result:=ps4_sceKernelMprotect(entries[i].start,
                                   entries[i].length,
                                   entries[i].protection);
    end;
   SCE_KERNEL_MAP_OP_MAP_FLEXIBLE:
    begin
     Result:=ps4_sceKernelMapFlexibleMemory(@entries[i].start,
                                            entries[i].length,
                                            entries[i].protection,
                                            flags);
    end;
   SCE_KERNEL_MAP_OP_TYPE_PROTECT:
    begin
     Result:=ps4_sceKernelMtypeprotect(entries[i].start,
                                       entries[i].length,
                                       entries[i].mtype,
                                       entries[i].protection);
    end;
   else
    begin
     numberOfEntries:=i;
     _set_errno(EINVAL);
     Result:=SCE_KERNEL_ERROR_EINVAL;
     goto _exit_lock;
    end;
  end;

  if (Result<>0) then
  begin
   numberOfEntries:=i;
   goto _exit_lock;
  end;

 end;

 _exit_lock:
  MUNLOCK;
  _sig_unlock;
 _exit:
  if (numberOfEntriesOut<>nil) then numberOfEntriesOut^:=numberOfEntries;
end;

//

function _sys_check_mmaped(addr:Pointer;len:qword):Integer;
begin
 _sig_lock;
 MLOCK;

 Result:=VirtualManager.check_mmaped(addr,len);

 MUNLOCK;
 _sig_unlock;
end;

function _sys_mlockall(flags:Integer):Integer;
const
 MNALL=not Integer(MCL_CURRENT or MCL_FUTURE);
begin
 if (flags=0) or ((flags and MNALL)<>0) then Exit(EINVAL);
 Result:=0;
end;

function ps4_mlock(addr:Pointer;len:qword):Integer; SysV_ABI_CDecl;
begin
 Result:=_set_errno(_sys_check_mmaped(addr,len));
end;

function ps4_munlock(addr:Pointer;len:qword):Integer; SysV_ABI_CDecl;
begin
 Result:=_set_errno(_sys_check_mmaped(addr,len));
end;

function ps4_mlockall(flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=_set_errno(_sys_mlockall(flags));
end;

function ps4_munlockall:Integer; SysV_ABI_CDecl;
begin
 Result:=_set_errno(0);
end;

function ps4_sceKernelMlock(addr:Pointer;len:qword):Integer; SysV_ABI_CDecl;
begin
 Result:=_sys_check_mmaped(addr,len);
 _set_errno(Result);
 Result:=px2sce(Result);
end;

function ps4_sceKernelMunlock(addr:Pointer;len:qword):Integer; SysV_ABI_CDecl;
begin
 Result:=_sys_check_mmaped(addr,len);
 _set_errno(Result);
 Result:=px2sce(Result);
end;

function ps4_sceKernelMlockall(flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=_sys_mlockall(flags);
 _set_errno(Result);
 Result:=px2sce(Result);
end;

function ps4_sceKernelMunlockall:Integer; SysV_ABI_CDecl;
begin
 _set_errno(0);
 Result:=0;
end;

////

function _sys_msync(addr:Pointer;len:size_t;flags:Integer):Integer;
begin
 if not IsAlign(addr,PHYSICAL_PAGE_SIZE) then Exit(EINVAL);

 if ((flags and MS_ASYNC)<>0) and ((flags and MS_INVALIDATE)<>0) then Exit(EINVAL);

 _sig_lock;
 MLOCK;

 Result:=VirtualManager.check_mmaped(addr,len);

 MUNLOCK;
 _sig_unlock;

 if (Result=0) then
 begin
  FlushViewOfFile(addr,len);
 end;
end;

function ps4_msync(addr:Pointer;len:size_t;flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=_set_errno(_sys_msync(addr,len,flags));
end;

function ps4_sceKernelMsync(addr:Pointer;len:size_t;flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=_sys_msync(addr,len,flags);
 _set_errno(Result);
 Result:=px2sce(Result);
end;

Procedure _mem_init;
var
 p:PQWORD;
begin
 SceKernelFlexibleMemorySize:=(448*1024*1024);
 p:=GetSceKernelFlexibleMemorySize;
 if (p<>nil) then
 begin
  SceKernelFlexibleMemorySize:=p^;
 end;
end;

Procedure _mem_print;
begin
 Writeln('---[Virtual]---');
 VirtualManager.Print;
 Writeln('---[Named]---');
 NamedManager.Print;
 Writeln('---[Direct]---');
 DirectManager.Print;
end;

initialization
 INIT_MLOCK;

 DirectManager :=TDirectManager .Create;
 DirectManager .OnMemoryUnmapCb:=@__munmap;

 VirtualManager:=TVirtualManager.Create($400000,$FFFFFFFFFF);
 VirtualManager.OnDirectUnmapCb:=@__unmap_direct;
 VirtualManager.OnDirectMtypeCb:=@__mtype_direct;
 VirtualManager.OnFreeBlockCb  :=@__free_block;
 VirtualManager._mmap_sys(Pointer($400000),$7FFBFC000);

 NamedManager  :=TNamedManager.Create($400000,$FFFFFFFFFF);

end.

