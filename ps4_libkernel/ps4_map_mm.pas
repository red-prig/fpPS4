unit ps4_map_mm;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  g23tree,
  RWLock,
  sys_types,
  Classes,
  SysUtils;

Const
 SCE_KERNEL_MAIN_DMEM_SIZE=$180000000;

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

 MAP_ANONYMOUS=1;
 MAP_ANON     =MAP_ANONYMOUS;
 MAP_SHARED   =2;
 MAP_PRIVATE  =4;
 MAP_POPULATE =8;
 MAP_NORESERVE=16;
 MAP_FIXED    =32;

 PROT_NONE     = 0;
 PROT_READ     = 1;
 PROT_WRITE    = 2;
 PROT_EXEC     = 4;

 MAP_FAILED    =Pointer(-1);

type
 pSceKernelDirectMemoryQueryInfo=^SceKernelDirectMemoryQueryInfo;
 SceKernelDirectMemoryQueryInfo=packed record
  start:QWORD;
  __end:QWORD;
  memoryType:Integer;
  __align:Integer;
 end;

const
 SCE_KERNEL_VIRTUAL_RANGE_NAME_SIZE=32;
 SCE_KERNEL_VQ_FIND_NEXT=1;

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
 end;

function ps4_sceKernelGetDirectMemorySize:Int64; SysV_ABI_CDecl;
function ps4_getpagesize:Integer; SysV_ABI_CDecl;

function ps4_sceKernelDirectMemoryQuery(
            offset:QWORD;
            flags:Integer;
            info:pSceKernelDirectMemoryQueryInfo;
            infoSize:QWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelAllocateDirectMemory(
           searchStart:QWORD;
           searchEnd:QWORD;
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

function ps4_sceKernelMapDirectMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           protections:Integer;
           flags:Integer;
           physicalAddr:QWORD;
           alignment:QWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMapNamedFlexibleMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           protections:Integer;
           flags:Integer;
           name:PChar):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMapFlexibleMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           protections:Integer;
           flags:Integer):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMunmap(addr:Pointer;len:size_t):Integer; SysV_ABI_CDecl;
function ps4_sceKernelQueryMemoryProtection(addr:Pointer;pStart,pEnd:PPointer;pProt:PInteger):Integer; SysV_ABI_CDecl;

function ps4_sceKernelVirtualQuery(addr:Pointer;
                                   flags:Integer;
                                   info:pSceKernelVirtualQueryInfo;
                                   infoSize:QWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelMprotect(addr:Pointer;len:QWORD;prot:Integer):Integer; SysV_ABI_CDecl;
function ps4_sceKernelSetVirtualRangeName(addr:Pointer;len:QWORD;name:Pchar):Integer; SysV_ABI_CDecl;
function ps4_mmap(addr:Pointer;len:size_t;prot,flags:Integer;fd:Integer;offset:size_t):Pointer; SysV_ABI_CDecl;
function ps4_munmap(addr:Pointer;len:size_t):Integer; SysV_ABI_CDecl;
function ps4_msync(addr:Pointer;len:size_t;flags:Integer):Integer; SysV_ABI_CDecl;
function ps4_mprotect(addr:Pointer;len:size_t;prot:Integer):Integer; SysV_ABI_CDecl;

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

implementation

uses
 sys_kernel,
 sys_signal;

const
 INVALID_DIRECT=QWORD(-1);

 BT_STACK     =0;
 BT_DIRECT_BIG=1;
 BT_DIRECT_64K=2;
 BT_PHYSIC_BIG=3;
 BT_PHYSIC_64K=4;

 BS_FREE   =0;
 BS_RESERVE=1;
 BS_COMMIT =2;

type
 PBlock=^TBlock;
 TBlock=object
  pAddr:Pointer;
  nSize:Int64;
  bType:SizeUint;
 end;

 pdlist=^Tdlist;

 Pdnode=^Tdnode;
 Tdnode=object
  pPrev,pNext:Pdnode;
 end;

 Tdlist=object
  pHead,pTail:Pdnode;
  function  REMOVE_HEAD:Pdnode;    inline;
  procedure INSERT_TAIL(e:Pdnode); inline;
  procedure REMOVE(e:Pdnode); inline;
 end;

 TnodeInfo=bitpacked record
  id:Byte;
  prot:Byte;
  state:Byte;
  len:Byte;
  align2:DWORD;
 end;

 PdnodeAdr=^TdnodeAdr;
 TdnodeAdr=object(Tdnode)
  direct:QWORD;
  info:TnodeInfo;
 end;

 PBlockBig=^TBlockBig;
 TBlockBig=object(TBlock)
  direct:QWORD;
  Handle:Pointer;
  prot:Byte;
 end;

 PBlock64k=^TBlock64k;
 TBlock64k=object(TBlock)
  nodes:array[0..3] of TdnodeAdr;
 end;

function IsPowerOfTwo(x:QWORD):Boolean; inline;
begin
 Result:=(x and (x - 1))=0;
end;

function _isgpu(prot:LongInt):Boolean; inline;
begin
 Result:=prot and (SCE_KERNEL_PROT_GPU_READ or SCE_KERNEL_PROT_GPU_WRITE)<>0;
end;

function __map_sce_prot_page(prot:LongInt):DWORD;
begin
 Result:=0;
 if (prot=0) then Exit(PAGE_NOACCESS);

 if (prot and SCE_KERNEL_PROT_CPU_EXEC)<>0 then
 begin
  if (prot and (SCE_KERNEL_PROT_CPU_WRITE or SCE_KERNEL_PROT_GPU_WRITE) )<>0 then
  begin
   Result:=PAGE_EXECUTE_READWRITE;
  end else
  if (prot and (SCE_KERNEL_PROT_CPU_READ or SCE_KERNEL_PROT_GPU_READ) )<>0 then
  begin
   Result:=PAGE_EXECUTE_READ;
  end else
  begin
   Result:=PAGE_EXECUTE;
  end;
 end else
 if (prot and (SCE_KERNEL_PROT_CPU_WRITE or SCE_KERNEL_PROT_GPU_WRITE) )<>0 then
 begin
  Result:=PAGE_READWRITE;
 end else
 begin
  Result:=PAGE_READONLY;
 end;
end;

function __map_mmap_prot_page(prot:LongInt):DWORD;
begin
 Result:=0;
 if (prot=PROT_NONE) then Exit(PAGE_NOACCESS);

 if (prot and PROT_EXEC)<>0 then
 begin
  if (prot and PROT_WRITE)<>0 then
  begin
   Result:=PAGE_EXECUTE_READWRITE;
  end else
  if (prot and PROT_READ)<>0 then
  begin
   Result:=PAGE_EXECUTE_READ;
  end else
  begin
   Result:=PAGE_EXECUTE;
  end;
 end else
 if (prot and PROT_WRITE)<>0 then
 begin
  Result:=PAGE_READWRITE;
 end else
 begin
  Result:=PAGE_READONLY;
 end;
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

//

function Get16kBlockCount(len:PTRUINT):PTRUINT; inline;
begin
 Result:=len div LOGICAL_PAGE_SIZE;
end;

function Get4kBlockCount(len:PTRUINT):PTRUINT; inline;
begin
 Result:=len div PHYSICAL_PAGE_SIZE;
end;

//

function VirtualAllocAlign(Addr:Pointer;dwSize,alignment:PTRUINT;flAllocationType,flProtect:DWORD):Pointer;
begin
 Result:=nil;
 if (alignment<=GRANULAR_PAGE_SIZE) or (Addr<>nil) then
 begin
  Result:=VirtualAlloc(Addr,dwSize,flAllocationType,flProtect);
  Exit;
 end;
 Addr:=Pointer($5400000);
 Addr:=AlignUp(Addr,alignment);
 repeat
  Result:=VirtualAlloc(Addr,dwSize,flAllocationType,flProtect);
  if (Result<>nil) then Exit;
  Case GetLastError of
   ERROR_INVALID_ADDRESS:;
   else
    Exit;
  end;
  Addr:=Addr+alignment;
 until false;
end;

function VirtualQueryBase(Addr:Pointer):TBlock;
var
 Info:TMemoryBasicInformation;
begin
 Result:=Default(TBlock);
 Info:=Default(TMemoryBasicInformation);
 if (VirtualQuery(addr,Info,SizeOf(TMemoryBasicInformation))<>0) then
 begin
  Case Info.State of
   MEM_FREE   :Result.bType:=BS_FREE;
   MEM_COMMIT :Result.bType:=BS_COMMIT;
   MEM_RESERVE:Result.bType:=BS_RESERVE;
  end;
  Result.pAddr:=Info.AllocationBase;
  Result.nSize:=Info.RegionSize+(ptruint(Info.BaseAddress)-ptruint(Info.AllocationBase));
 end;
end;

function VirtualIsFullReserve(Addr:Pointer;dwSize:PTRUINT):Boolean;
var
 curr:Pointer;
 q:TBlock;
begin
 Result:=True;
 curr:=Addr;
 While (curr<Addr+dwSize) do
 begin
  q:=VirtualQueryBase(curr);
  Case q.bType of
   BS_FREE:
   begin
    curr:=q.pAddr+q.nSize;
   end;
   BS_RESERVE:curr:=curr+PHYSICAL_PAGE_SIZE;
   BS_COMMIT:Exit(False);
  end;
 end;
end;

///

function Tdlist.REMOVE_HEAD:Pdnode; inline;
begin
 Result:=pHead;
 if (pHead<>nil) then
 begin
  pHead:=pHead^.pNext;
  if (pHead<>nil) then
  begin
   pHead^.pPrev:=nil;
  end else
  begin
   pTail:=nil;
  end;
  Result^.pPrev:=nil;
  Result^.pNext:=nil;
 end;
end;

procedure Tdlist.INSERT_TAIL(e:Pdnode); inline;
begin
 if (e=nil) then Exit;
 if (e^.pPrev<>nil) or (e^.pNext<>nil) then Exit;
 if (pHead=nil) then
 begin
  pHead:=e;
 end else
 begin
  pTail^.pNext:=e;
  e^.pPrev:=pTail;
 end;
 pTail:=e;
end;

procedure Tdlist.REMOVE(e:Pdnode); inline;
var
 p,n:Pdnode;
begin
 if (e=nil) then Exit;
 if (pHead=e) then
 begin
  pHead:=pHead^.pNext;
  if (pHead=nil) then pTail:=nil;
 end else
 if (pTail=e) then
 begin
  pTail:=pTail^.pPrev;
  if (pTail=nil) then pHead:=nil;
 end else
 begin
  p:=e^.pPrev;
  n:=e^.pNext;
  if (p<>nil) then p^.pNext:=n;
  if (n<>nil) then n^.pPrev:=p;
 end;
 e^.pPrev:=nil;
 e^.pNext:=nil;
end;

type
 TBlockCompare=object
  function c(const a,b:PBlock):Integer; static;
 end;

function TBlockCompare.c(const a,b:PBlock):Integer; static;
begin
 if (a^.pAddr=b^.pAddr) then
  Result:=0
 else
 if (a^.pAddr<b^.pAddr) then
  Result:=-1
 else
  Result:=1;
end;

type
 TMemDirectAdrCompare=object
  function c(const a,b:TBlock):Integer; static;
 end;

function TMemDirectAdrCompare.c(const a,b:TBlock):Integer; static;
begin
 if (a.pAddr=b.pAddr) then
  Result:=0
 else
 if (a.pAddr<b.pAddr) then
  Result:=-1
 else
  Result:=1;
end;

type
 TDirectAdrSet=specialize T23treeSet<TBlock,TMemDirectAdrCompare>;

type
 TBlockSet=specialize T23treeSet<PBlock,TBlockCompare>;

 TPageMM=object
  var
   FLock:TRWLock;

   FDirectSize:QWORD;
   FDirectAdrSet:TDirectAdrSet;

   FMapBlockSet:TBlockSet;

               //direct,num ,len
   Falign:array[Boolean,0..3,1..3] of Tdlist;

  Procedure Init;

  function  _new_big_block_d(addr:Pointer;len,alignment,direct:size_t;prot:Byte):Pointer;
  function  _new_64k_block_d(addr:Pointer;len,alignment,direct:size_t;prot:Byte):Pointer;
  function  _isfree_64k_block_d(block:PBlock64k):Boolean;
  procedure _unmap_64k_block_d(block:PBlock64k);
  procedure _map_64k_block_d(block:PBlock64k);
  function  _alloc_part_d(len,alignment,direct:size_t;prot:Byte):Pointer;
  function  _TryGetMapBlockByAddr(addr:Pointer;var _pblock:PBlock):Boolean;
  procedure _DeleteBlockByAddr(addr:Pointer);
  function  _check_fixed(addr:Pointer;len:size_t;overwrite:Boolean):Boolean;
  function  _free_fixed(addr:Pointer;len:size_t):Boolean;
  function  _commit_fixed_d(addr:Pointer;len,direct:size_t;prot:Byte):Boolean;

  function  mmap_d(addr:Pointer;len,alignment,direct:size_t;prot:Byte;overwrite:Boolean):Pointer;

  function  unmap(addr:Pointer;len:size_t):Boolean;

  function  QueryProt(addr:Pointer;pStart,pEnd:PPointer;pProt:PInteger):Boolean;
  function  ChangeProt(addr:Pointer;len:QWORD;prot:Integer):Boolean;
 end;

Procedure TPageMM.Init;
begin
 FillChar(Self,SizeOf(TPageMM),0);
 rwlock_init(FLock);
end;

function TPageMM._new_big_block_d(addr:Pointer;len,alignment,direct:size_t;prot:Byte):Pointer;
var
 base:Pointer;
 block:PBlockBig;
begin
 Result:=nil;
 base:=VirtualAllocAlign(addr,len,alignment,MEM_COMMIT or MEM_RESERVE,__map_sce_prot_page(prot));
 if (base=nil) then Exit;
 block:=AllocMem(SizeOf(TBlockBig));
 if (block=nil) then
 begin
  VirtualFree(base,0,MEM_RELEASE);
  Exit;
 end;
 block^.pAddr:=base;
 block^.nSize:=len;
 block^.bType:=BT_DIRECT_BIG;
 block^.direct:=direct;
 block^.prot:=prot;
 if _isgpu(prot) and (GpuMemCb.Alloc<>nil) then
 begin
  block^.Handle:=GpuMemCb.Alloc(base,len);
 end;
 Assert(block<>nil);
 FMapBlockSet.Insert(block);
 Result:=base;
end;

function TPageMM._new_64k_block_d(addr:Pointer;len,alignment,direct:size_t;prot:Byte):Pointer;
var
 base:Pointer;
 block:PBlock64k;
 i,c:Byte;
begin
 Result:=nil;
 base:=VirtualAllocAlign(addr,GRANULAR_PAGE_SIZE,alignment,MEM_COMMIT or MEM_RESERVE,__map_sce_prot_page(prot));
 if (len<>GRANULAR_PAGE_SIZE) then
 begin
  VirtualFree(base+len,GRANULAR_PAGE_SIZE-len,MEM_DECOMMIT);
 end;
 if (base=nil) then Exit;
 block:=AllocMem(SizeOf(TBlock64k));
 if (block=nil) then
 begin
  VirtualFree(base,0,MEM_RELEASE);
  Exit;
 end;
 block^.pAddr:=base;
 block^.nSize:=GRANULAR_PAGE_SIZE;
 block^.bType:=BT_DIRECT_64K;

 For i:=0 to 3 do
 begin
  if (direct=INVALID_DIRECT) then
  begin
   block^.nodes[i].direct:=INVALID_DIRECT;
  end else
  begin
   block^.nodes[i].direct:=direct+(i*LOGICAL_PAGE_SIZE);
  end;
  block^.nodes[i].info.id:=i;
  block^.nodes[i].info.prot:=prot;
  block^.nodes[i].info.state:=BS_COMMIT;
 end;

 c:=Get16kBlockCount(len);
 if (c<4) then
 begin
  For i:=c to 3 do
  begin
   block^.nodes[i].direct:=0;
   block^.nodes[i].info.prot:=0;
   block^.nodes[i].info.state:=BS_FREE;
  end;
  _map_64k_block_d(block);
 end;

 Assert(block<>nil);
 FMapBlockSet.Insert(block);
 Result:=base;
end;

function TPageMM._isfree_64k_block_d(block:PBlock64k):Boolean;
var
 i:Byte;
begin
 Result:=True;
 For i:=0 to 3 do
 begin
  if (block^.nodes[i].info.state=BS_COMMIT) then Exit(false);
 end;
end;

procedure TPageMM._unmap_64k_block_d(block:PBlock64k);
var
 i:Byte;
begin
 For i:=0 to 3 do
 begin
  Falign[block^.nodes[i].direct<>INVALID_DIRECT,
         i,
         block^.nodes[i].info.len].
          REMOVE(@block^.nodes[i]);
 end;
end;

procedure TPageMM._map_64k_block_d(block:PBlock64k);
var
 ip,ic:Byte;
begin
 ip:=0;
 While (ip<=3) do
 begin
  if (block^.nodes[ip].info.state<>BS_COMMIT) then
  begin
   ic:=1;
   While (ip+ic<=3) do
   begin
    if (block^.nodes[ip+ic].info.state<>BS_COMMIT) then
     Inc(ic)
    else
     Break;
   end;
   block^.nodes[ip].info.len:=ic;
   Falign[block^.nodes[ip].direct<>INVALID_DIRECT,ip,ic].INSERT_TAIL(@block^.nodes[ip]);
  end;
  Inc(ip);
 end;
end;

//  //num ,len
//  Falign_d:array[0..3,1..3] of Tdlist;

function TPageMM._alloc_part_d(len,alignment,direct:size_t;prot:Byte):Pointer;
var
 block:PBlock64k;
 node:PdnodeAdr;
 i,b,n,count:Byte;

 function _find_by_len_16(len:Byte):PdnodeAdr;
 var
  i:Byte;
 begin
  Result:=nil;
  For i:=0 to 3 do
  begin
   Result:=PdnodeAdr(Falign[direct<>INVALID_DIRECT,i,len].REMOVE_HEAD);
   if (Result<>nil) then Break;
  end;
 end;

 function _find_by_len_32(len:Byte):PdnodeAdr;
 begin
  Result:=PdnodeAdr(Falign[direct<>INVALID_DIRECT,0,len].REMOVE_HEAD);
  if (Result<>nil) then Exit;
  Result:=PdnodeAdr(Falign[direct<>INVALID_DIRECT,2,len].REMOVE_HEAD);
 end;

begin
 count:=Get16kBlockCount(len);

 node:=nil;
 if (alignment<=16*1024) then
 begin //16k
  node:=_find_by_len_16(count);
 end else
 if (alignment<=32*1024) then
 begin //32k
  node:=_find_by_len_32(count);
  if (node=nil) and (count<=2) then
  begin
   node:=PdnodeAdr(Falign[direct<>INVALID_DIRECT,1,count].REMOVE_HEAD);
   if (node<>nil) then Inc(node);
  end;
 end else
 begin //64k
  node:=PdnodeAdr(Falign[direct<>INVALID_DIRECT,0,count].REMOVE_HEAD);
 end;

 if (node=nil) then //not found
 begin
  Result:=_new_64k_block_d(nil,len,alignment,direct,prot);
 end else
 begin
  b:=node^.info.id;
  block:=Pointer(PtrUint(@node[-b])-PtrUint(@PBlock64k(nil)^.nodes));

  n:=b+count-1;

  For i:=b to n do
  begin
   if (direct=INVALID_DIRECT) then
   begin
    block^.nodes[i].direct:=INVALID_DIRECT;
   end else
   begin
    block^.nodes[i].direct:=direct+((i-b)*LOGICAL_PAGE_SIZE);
   end;
   block^.nodes[i].info.prot:=prot;
   block^.nodes[i].info.state:=BS_COMMIT;
   block^.nodes[i].info.len:=0;
  end;

  _map_64k_block_d(block);

  Result:=block^.pAddr+(b*LOGICAL_PAGE_SIZE);
  Result:=VirtualAlloc(Result,len,MEM_COMMIT,__map_sce_prot_page(prot));
 end;
end;

function TPageMM._TryGetMapBlockByAddr(addr:Pointer;var _pblock:PBlock):Boolean;
var
 It:TBlockSet.Iterator;
 i:Integer;
begin
 Result:=False;
 It:=FMapBlockSet.find_le(@addr);
 //if (It.Item=nil) then Exit;

 if (It.Item=nil) then
 begin
  Writeln('Memory dump:',HexStr(addr));
  It:=FMapBlockSet.cbegin;
  While (It.Item<>nil) do
  begin
   _pblock:=It.Item^;
   if (_pblock<>nil) then
   begin
    Case _pblock^.bType of
      BT_STACK:
       begin
        Writeln('[BT_STACK]');
        Writeln(' pAddr:',HexStr(_pblock^.pAddr));
        Writeln(' nSize:',HexStr(_pblock^.nSize,16));
       end;
      BT_DIRECT_BIG:
       begin
        Writeln('[BT_DIRECT_BIG]');
        Writeln(' pAddr:',HexStr(_pblock^.pAddr));
        Writeln(' nSize:',HexStr(_pblock^.nSize,16));
        Writeln(' direct:',HexStr(PBlockBig(_pblock)^.direct,16));
        Writeln(' Handle:',HexStr(PBlockBig(_pblock)^.Handle));
        Writeln(' prot:',test_KP_flags(PBlockBig(_pblock)^.prot));
       end;
      BT_DIRECT_64K:
       begin
        Writeln('[BT_DIRECT_64K]');
        Writeln(' pAddr:',HexStr(_pblock^.pAddr));
        Writeln(' nSize:',HexStr(_pblock^.nSize,16));

        For i:=0 to 3 do
        begin
         Writeln(' [node]:',i);
         Writeln('  direct:'    ,HexStr(PBlock64k(_pblock)^.nodes[i].direct,16));
         Writeln('  info.id:'   ,HexStr(PBlock64k(_pblock)^.nodes[i].info.id,2));
         Writeln('  info.prot:' ,test_KP_flags(PBlock64k(_pblock)^.nodes[i].info.prot));
         Writeln('  info.state:',PBlock64k(_pblock)^.nodes[i].info.state);
         Writeln('  info.len:'  ,PBlock64k(_pblock)^.nodes[i].info.len);
        end;

       end;
      BT_PHYSIC_BIG:
       begin
        Writeln('[BT_PHYSIC_BIG]');
        Writeln(' pAddr:',HexStr(_pblock^.pAddr));
        Writeln(' nSize:',HexStr(_pblock^.nSize,16));
       end;
      BT_PHYSIC_64K:
       begin
        Writeln('[BT_PHYSIC_64K]');
        Writeln(' pAddr:',HexStr(_pblock^.pAddr));
        Writeln(' nSize:',HexStr(_pblock^.nSize,16));
       end;

     else;
    end;
   end;

   It.Next;
  end;
  Writeln('------------');
  Assert(false);
  Exit;
 end;

 _pblock:=It.Item^;
 if (_pblock=nil) then Exit;
 if (_pblock^.pAddr>addr) or (_pblock^.pAddr+_pblock^.nSize<=addr) then
 begin
  _pblock:=nil;
  Exit;
 end;
 Result:=True;
end;

procedure TPageMM._DeleteBlockByAddr(addr:Pointer);
var
 It:TBlockSet.Iterator;
 _pblock:PBlock;
begin
 It:=FMapBlockSet.find(@addr);
 if (It.Item=nil) then Exit;
 _pblock:=It.Item^;
 FMapBlockSet.erase(It);
 if (_pblock<>nil) then FreeMem(_pblock);
end;

function TPageMM._check_fixed(addr:Pointer;len:size_t;overwrite:Boolean):Boolean;
var
 curr:Pointer;
 q:TBlock;
 _pblock:PBlock;
begin
 Result:=true;
 curr:=addr;
 repeat
  q:=VirtualQueryBase(curr);
  Case q.bType of
   BS_FREE:
   begin
    curr:=q.pAddr+q.nSize;
   end;
   BS_RESERVE,
   BS_COMMIT:
   begin
    if (q.bType=BS_COMMIT) and (not overwrite) then Exit(False);

    if not _TryGetMapBlockByAddr(curr,_pblock) then Exit;

    Case _pblock^.bType of
     BT_DIRECT_BIG,
     BT_DIRECT_64K:
      begin
       curr:=curr+LOGICAL_PAGE_SIZE;
      end;
     BT_PHYSIC_BIG,
     BT_PHYSIC_64K:
      begin
       curr:=curr+PHYSICAL_PAGE_SIZE;
      end;
     else
      Exit(False);
    end;

   end;
  end;

 until (curr>=addr+len);
end;

function TPageMM._free_fixed(addr:Pointer;len:size_t):Boolean;
var
 curr:Pointer;
 q:TBlock;
 _pblock:PBlock;
 i:Byte;
begin
 Result:=true;
 curr:=addr;
 repeat
  q:=VirtualQueryBase(curr);
  Case q.bType of
   BS_FREE:
   begin
    curr:=q.pAddr+q.nSize;
   end;
   BS_RESERVE,
   BS_COMMIT:
   begin
    if not _TryGetMapBlockByAddr(curr,_pblock) then Exit;

    Case _pblock^.bType of
     BT_DIRECT_BIG:
      begin
       if (curr=q.pAddr) and (_pblock^.pAddr=q.pAddr) and (_pblock^.nSize=q.nSize) then
       begin
        if _isgpu(PBlockBig(_pblock)^.prot) and (GpuMemCb.Free<>nil) then
        begin
         GpuMemCb.Free(PBlockBig(_pblock)^.Handle);
        end;
        if not VirtualFree(q.pAddr,0,MEM_RELEASE) then Exit(False);
        _DeleteBlockByAddr(q.pAddr);
        curr:=q.pAddr+q.nSize;
       end else
       begin
        VirtualFree(curr,LOGICAL_PAGE_SIZE,MEM_DECOMMIT);
        curr:=curr+LOGICAL_PAGE_SIZE;
        if VirtualIsFullReserve(q.pAddr,q.nSize) then
        begin
         if _isgpu(PBlockBig(_pblock)^.prot) and (GpuMemCb.Free<>nil) then
         begin
          GpuMemCb.Free(PBlockBig(_pblock)^.Handle);
         end;
         if not VirtualFree(q.pAddr,0,MEM_RELEASE) then Exit(False);
         _DeleteBlockByAddr(q.pAddr);
         curr:=q.pAddr+q.nSize;
        end;
       end;
      end;
     BT_PHYSIC_BIG:
      begin
       if (curr=q.pAddr) and (_pblock^.pAddr=q.pAddr) and (_pblock^.nSize=q.nSize) then
       begin
        if not VirtualFree(q.pAddr,0,MEM_RELEASE) then Exit(False);
        _DeleteBlockByAddr(q.pAddr);
        curr:=q.pAddr+q.nSize;
       end else
       begin
        VirtualFree(curr,PHYSICAL_PAGE_SIZE,MEM_DECOMMIT);
        curr:=curr+PHYSICAL_PAGE_SIZE;
        if VirtualIsFullReserve(q.pAddr,q.nSize) then
        begin
         if not VirtualFree(q.pAddr,0,MEM_RELEASE) then Exit(False);
         _DeleteBlockByAddr(q.pAddr);
         curr:=q.pAddr+q.nSize;
        end;
       end;
      end;
     BT_DIRECT_64K:
      if (q.bType=BS_COMMIT) then
      begin
       VirtualFree(curr,LOGICAL_PAGE_SIZE,MEM_DECOMMIT);
       i:=Get16kBlockCount(curr-_pblock^.pAddr);
       _unmap_64k_block_d(PBlock64k(_pblock));
       if PBlock64k(_pblock)^.nodes[i].direct<>INVALID_DIRECT then
       begin
        PBlock64k(_pblock)^.nodes[i].direct:=0;
       end;
       PBlock64k(_pblock)^.nodes[i].info.prot:=0;
       PBlock64k(_pblock)^.nodes[i].info.state:=BS_FREE;
       if _isfree_64k_block_d(PBlock64k(_pblock)) then
       begin
        if not VirtualFree(_pblock^.pAddr,0,MEM_RELEASE) then Exit(False);
        q.pAddr:=_pblock^.pAddr;
        _DeleteBlockByAddr(q.pAddr);
       end else
       begin
        _map_64k_block_d(PBlock64k(_pblock));
       end;
       curr:=curr+LOGICAL_PAGE_SIZE;
      end else
      begin
       curr:=curr+LOGICAL_PAGE_SIZE;
      end;
     BT_PHYSIC_64K:Assert(False);
     else
      Exit(False);
    end;

   end;
  end;

 until (curr>=addr+len);
end;

function TPageMM._commit_fixed_d(addr:Pointer;len,direct:size_t;prot:Byte):Boolean;
var
 base:Pointer;
 curr:Pointer;
 q:TBlock;
 _pblock:PBlock;
 i:Byte;
begin
 Result:=true;
 curr:=addr;
 repeat
  q:=VirtualQueryBase(curr);
  Case q.bType of
   BS_FREE:
   begin
    base:=curr;
    curr:=q.pAddr+q.nSize;
    if (curr>addr+len) then curr:=addr+len;
    if _new_big_block_d(base,curr-base,0,direct,prot)=nil then Exit(False);
    if (direct<>INVALID_DIRECT) then
    begin
     direct:=direct+(curr-base);
    end;
   end;
   BS_RESERVE:
   begin
    if not _TryGetMapBlockByAddr(curr,_pblock) then Exit;

    Case _pblock^.bType of
     BT_DIRECT_BIG:
      begin
       if VirtualAlloc(curr,LOGICAL_PAGE_SIZE,MEM_COMMIT,__map_sce_prot_page(prot))=nil then Exit(False);
       curr:=curr+LOGICAL_PAGE_SIZE;
       if (direct<>INVALID_DIRECT) then
       begin
        direct:=direct+LOGICAL_PAGE_SIZE;
       end;
      end;
     BT_PHYSIC_BIG:
      begin
       if VirtualAlloc(curr,PHYSICAL_PAGE_SIZE,MEM_COMMIT,__map_sce_prot_page(prot))=nil then Exit(False);
       curr:=curr+PHYSICAL_PAGE_SIZE;
       if (direct<>INVALID_DIRECT) then
       begin
        direct:=direct+PHYSICAL_PAGE_SIZE;
       end;
      end;
     BT_DIRECT_64K:
      begin
       if VirtualAlloc(curr,LOGICAL_PAGE_SIZE,MEM_COMMIT,__map_sce_prot_page(prot))=nil then Exit(False);
       i:=Get16kBlockCount(curr-_pblock^.pAddr);
       _unmap_64k_block_d(PBlock64k(_pblock));
       PBlock64k(_pblock)^.nodes[i].direct:=direct;
       PBlock64k(_pblock)^.nodes[i].info.prot:=prot;
       PBlock64k(_pblock)^.nodes[i].info.state:=BS_COMMIT;
       _map_64k_block_d(PBlock64k(_pblock));
       curr:=curr+LOGICAL_PAGE_SIZE;
       if (direct<>INVALID_DIRECT) then
       begin
        direct:=direct+LOGICAL_PAGE_SIZE;
       end;
      end;
     BT_PHYSIC_64K:Assert(False);
     else
      Exit(False);
    end;
   end;
   BS_COMMIT:Exit(False);
  end;

 until (curr>=addr+len);
end;

function TPageMM.mmap_d(addr:Pointer;len,alignment,direct:size_t;prot:Byte;overwrite:Boolean):Pointer;
begin
 Result:=nil;
 rwlock_wrlock(FLock);

 if _isgpu(prot) then
 begin
  Result:=_new_big_block_d(addr,len,alignment,direct,prot);
 end else
 if (addr<>nil) then //fixed adr
 begin
  if _check_fixed(addr,len,overwrite) then
  begin
   if _free_fixed(addr,len) then
   begin
    if _commit_fixed_d(addr,len,direct,prot) then
     Result:=addr;
   end;
  end;
 end else
 begin //any addr
  if (alignment<=GRANULAR_PAGE_SIZE) and (len<=GRANULAR_PAGE_SIZE) then //64k block
  begin
   if (len=GRANULAR_PAGE_SIZE) then //full
   begin
    Result:=_new_64k_block_d(addr,len,alignment,direct,prot);
   end else
   begin
    Result:=_alloc_part_d(len,alignment,direct,prot);
   end;
  end else
  if (len<=GRANULAR_PAGE_SIZE) then //64k but big aligned
  begin
   Result:=_new_64k_block_d(addr,len,alignment,direct,prot);
  end else
  begin //big block
   Result:=_new_big_block_d(addr,len,alignment,direct,prot);
  end;
 end;

 rwlock_unlock(FLock);
end;

function TPageMM.unmap(addr:Pointer;len:size_t):Boolean;
begin
 Result:=false;
 rwlock_wrlock(FLock);

 Result:=_check_fixed(addr,len,true);
 if Result then
 begin
  Result:=_free_fixed(addr,len);
 end;

 rwlock_unlock(FLock);
end;

function TPageMM.QueryProt(addr:Pointer;pStart,pEnd:PPointer;pProt:PInteger):Boolean;
var
 _pblock:PBlock;
 i,b,e:Byte;
begin
 Result:=False;
 rwlock_rdlock(FLock);

 if _TryGetMapBlockByAddr(addr,_pblock) then
 begin
  Result:=True;
  Case _pblock^.bType of
   BT_DIRECT_BIG,
   BT_PHYSIC_BIG:
    begin
     if (pStart<>nil) then pStart^:=_pblock^.pAddr;
     if (pEnd<>nil)   then pEnd^  :=_pblock^.pAddr+_pblock^.nSize-1;
     if (pProt<>nil)  then pProt^ :=PBlockBig(_pblock)^.prot;
    end;
   BT_DIRECT_64K:
    begin
     i:=Get16kBlockCount(addr-_pblock^.pAddr);
     b:=i;
     repeat
      if (b=0) then Break;
      if (PBlock64k(_pblock)^.nodes[b-1].info.state<>PBlock64k(_pblock)^.nodes[i].info.state) then Break;
      if (PBlock64k(_pblock)^.nodes[b-1].info.prot<>PBlock64k(_pblock)^.nodes[i].info.prot) then Break;
      Dec(b);
     until false;
     e:=i;
     repeat
      if (e=3) then Break;
      if (PBlock64k(_pblock)^.nodes[e+1].info.state<>PBlock64k(_pblock)^.nodes[i].info.state) then Break;
      if (PBlock64k(_pblock)^.nodes[e+1].info.prot<>PBlock64k(_pblock)^.nodes[i].info.prot) then Break;
      Inc(e);
     until false;
     if (pStart<>nil) then pStart^:=_pblock^.pAddr+(b*LOGICAL_PAGE_SIZE);
     if (pEnd<>nil)   then pEnd^  :=_pblock^.pAddr+(e*LOGICAL_PAGE_SIZE)-1;
     if (pProt<>nil)  then pProt^ := PBlock64k(_pblock)^.nodes[i].info.prot;
    end;
   BT_PHYSIC_64K:Assert(False);
  end;
 end;

 rwlock_unlock(FLock);
end;

function __mprotect(addr:Pointer;len:size_t;prot:Integer):Integer;
Var
 newprotect,oldprotect:DWORD;
begin
 newprotect:=__map_mmap_prot_page(prot);
 oldprotect:=0;

 if not VirtualProtect(addr,len,newprotect,oldprotect) then
 begin
  Exit(-1);
 end;

 Result:=0;
end;

function TPageMM.ChangeProt(addr:Pointer;len:QWORD;prot:Integer):Boolean;
var
 _pblock:PBlock;
begin
 Result:=False;
 rwlock_rdlock(FLock);

 repeat

  if _TryGetMapBlockByAddr(addr,_pblock) then
  begin

   if (_pblock^.nSize>len) then
   begin
    Result:=(__mprotect(addr,len,prot)=0);
    Break;
   end else
   begin
    Result:=(__mprotect(addr,_pblock^.nSize,prot)=0);
   end;

   if (len>=_pblock^.nSize) then
   begin
    len:=len-_pblock^.nSize;
    addr:=addr+_pblock^.nSize;
   end else
   begin
    Break;
   end;

  end else
  begin

   if (len>=PHYSICAL_PAGE_SIZE) then
   begin
    len:=len-PHYSICAL_PAGE_SIZE;
    addr:=addr+PHYSICAL_PAGE_SIZE;
   end else
   begin
    Break;
   end;

  end;

 until (len=0);

 rwlock_unlock(FLock);
end;

///////

Var
 PageMM:TPageMM;

Function TryGetGpuMemBlockByAddr(addr:Pointer;var block:TGpuMemBlock):Boolean;
var
 _pblock:PBlock;
begin
 Result:=False;
 rwlock_rdlock(PageMM.FLock);
 if PageMM._TryGetMapBlockByAddr(addr,_pblock) then
 begin
  Case _pblock^.bType of
   BT_DIRECT_BIG:
    if _isgpu(PBlockBig(_pblock)^.prot) then
    begin
     block.pAddr :=_pblock^.pAddr;
     block.nSize :=_pblock^.nSize;
     block.Handle:=PBlockBig(_pblock)^.Handle;
     Result:=true;
    end;
  end;
 end;
 rwlock_unlock(PageMM.FLock);
end;

Procedure RegistredStack;
var
 block:PBlock;
begin
 rwlock_wrlock(PageMM.FLock);
 block:=AllocMem(SizeOf(TBlock));
 if (block=nil) then Exit;
 block^.pAddr:=StackBottom;
 block^.nSize:=StackLength;
 block^.bType:=BT_STACK;
 PageMM.FMapBlockSet.Insert(block);
 rwlock_unlock(PageMM.FLock);
end;

Procedure UnRegistredStack;
begin
 rwlock_wrlock(PageMM.FLock);
 PageMM._DeleteBlockByAddr(StackBottom);
 rwlock_unlock(PageMM.FLock);
end;

function ps4_sceKernelGetDirectMemorySize:Int64; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_MAIN_DMEM_SIZE;
end;

function ps4_getpagesize:Integer; SysV_ABI_CDecl;
begin
 Result:=PHYSICAL_PAGE_SIZE;
end;

//function sceKernelReleaseDirectMemory(physicalAddr:Pointer;length:Int64):Int64; cdecl;

function ps4_sceKernelAllocateDirectMemory(
           searchStart:QWORD;
           searchEnd:QWORD;
           length:QWORD;
           alignment:QWORD;
           memoryType:Integer;
           physicalAddrDest:PQWORD):Integer; SysV_ABI_CDecl;
var
 It:TDirectAdrSet.Iterator;
 Adr,Tmp:TBlock;
 m1,m2:Pointer;
begin
 Writeln('srch:',HexStr(searchStart,16),'..',HexStr(searchEnd,16),' len:',HexStr(length,16));
 Writeln('align:',HexStr(alignment,16),' ','mType:',str_mem_type(memoryType));

 if (physicalAddrDest=nil) or (length=0) or (searchEnd<=searchStart) then Exit(SCE_KERNEL_ERROR_EINVAL);

 if (searchEnd>SCE_KERNEL_MAIN_DMEM_SIZE) then Exit(SCE_KERNEL_ERROR_EINVAL);

 if (alignment=0) then alignment:=LOGICAL_PAGE_SIZE;

 if not IsAlign(length   ,LOGICAL_PAGE_SIZE) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if not IsAlign(alignment,LOGICAL_PAGE_SIZE) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if not IsPowerOfTwo(alignment)              then Exit(SCE_KERNEL_ERROR_EINVAL);

 Adr.pAddr:=AlignUp(Pointer(searchStart),alignment);
 Adr.nSize:=length;
 Adr.bType:=memoryType;

 Result:=0;

 _sig_lock;
 rwlock_wrlock(PageMM.FLock);

 if ((PageMM.FDirectSize+Adr.nSize)>SCE_KERNEL_MAIN_DMEM_SIZE) then
 begin
  rwlock_unlock(PageMM.FLock);
  _sig_unlock;
  Exit(SCE_KERNEL_ERROR_EAGAIN);
 end;

 repeat

  if ((QWORD(Adr.pAddr)+Adr.nSize)>SCE_KERNEL_MAIN_DMEM_SIZE) then
  begin
   Result:=SCE_KERNEL_ERROR_EAGAIN;
   Break;
  end;

  Tmp.pAddr:=Adr.pAddr+Adr.nSize-1;
  Tmp.nSize:=0;
  Tmp.bType:=0;

  It:=PageMM.FDirectAdrSet.find_le(Tmp);
  if (It.Item=nil) then Break;

  Tmp:=It.Item^;
  m1:=Tmp.pAddr+Tmp.nSize;

  if (Adr.pAddr>=m1) then Break;

  m1:=AlignUp(m1,alignment);
  m2:=Adr.pAddr+alignment;

  if (m1>m2) then
   Adr.pAddr:=m1
  else
   Adr.pAddr:=m2;

  if (Adr.pAddr>=Pointer(searchEnd)) then
  begin
   Result:=SCE_KERNEL_ERROR_EAGAIN;
   Break;
  end;

 until false;

 if (Result=0) then
 begin
  PageMM.FDirectSize:=PageMM.FDirectSize+Adr.nSize;
  PageMM.FDirectAdrSet.Insert(Adr);
  physicalAddrDest^:=QWORD(Adr.pAddr);
 end;

 rwlock_unlock(PageMM.FLock);
 _sig_unlock;

 Result:=0;
end;

{
SCE_KERNEL_MAP_FIXED
 0x0010
 Fix map destination to *addr

SCE_KERNEL_MAP_NO_OVERWRITE
 0x0080
 Prohibit mapping when an area that is being used is included between *addr and *addr+len

SCE_KERNEL_MAP_NO_COALESCE
 0x400000
 Instruct sceKernelVirtualQuery() not to merge neighboring areas

 }

function ps4_sceKernelAvailableDirectMemorySize(
            searchStart:QWORD;
            searchEnd:QWORD;
            alignment:QWORD;
            physAddrOut:PQWORD;
            sizeOut:PQWORD):Integer; SysV_ABI_CDecl;
var
 It:TDirectAdrSet.Iterator;
 offset,size:QWORD;
 Tmp:TBlock;
begin
 if (physAddrOut=nil) or (sizeOut=nil) or (searchEnd<=searchStart) then Exit(SCE_KERNEL_ERROR_EINVAL);

 if (searchEnd>SCE_KERNEL_MAIN_DMEM_SIZE) then Exit(SCE_KERNEL_ERROR_EINVAL);

 if (alignment=0) then alignment:=LOGICAL_PAGE_SIZE;

 if not IsAlign(searchStart,LOGICAL_PAGE_SIZE) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if not IsAlign(searchEnd  ,LOGICAL_PAGE_SIZE) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if not IsAlign(alignment  ,LOGICAL_PAGE_SIZE) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if not IsPowerOfTwo(alignment)                then Exit(SCE_KERNEL_ERROR_EINVAL);

 physAddrOut^:=0;
 sizeOut^    :=0;

 offset:=0;

 Result:=0;
 _sig_lock;
 rwlock_wrlock(PageMM.FLock);

 repeat

  Tmp.pAddr:=AlignUp(Pointer(offset),alignment);
  Tmp.nSize:=0;
  Tmp.bType:=0;

  It:=PageMM.FDirectAdrSet.find_be(Tmp);

  if (It.Item=nil) then //nothing to be
  begin
   size:=searchEnd-offset;
   if (size=0) then
   begin
    Result:=SCE_KERNEL_ERROR_EAGAIN;
    Break;
   end else
   begin
    physAddrOut^:=offset;
    sizeOut^    :=size;
    Break;
   end;
  end;

  Tmp:=It.Item^;

  size:=QWORD(Tmp.pAddr)-offset;

  if (size<>0) then
  begin
   physAddrOut^:=offset;
   sizeOut^    :=size;
   Break;
  end;

  offset:=QWORD(Tmp.pAddr)+Tmp.nSize;

  if (offset>=searchEnd) then
  begin
   Result:=SCE_KERNEL_ERROR_EAGAIN;
   Break;
  end;

 until false;

 rwlock_unlock(PageMM.FLock);
 _sig_unlock;

 Result:=0;
end;

const
 SCE_KERNEL_DMQ_FIND_NEXT=1;

function ps4_sceKernelDirectMemoryQuery(
            offset:QWORD;
            flags:Integer;
            info:pSceKernelDirectMemoryQueryInfo;
            infoSize:QWORD):Integer; SysV_ABI_CDecl;
var
 It:TDirectAdrSet.Iterator;
 Tmp:TBlock;
begin
 if (info=nil) or (infoSize<>SizeOf(SceKernelDirectMemoryQueryInfo)) then Exit(SCE_KERNEL_ERROR_EINVAL);

 if not IsAlign(offset,LOGICAL_PAGE_SIZE) then Exit(SCE_KERNEL_ERROR_EINVAL);

 info^:=Default(SceKernelDirectMemoryQueryInfo);

 Tmp:=Default(TBlock);
 Tmp.pAddr:=Pointer(offset);

 Result:=0;

 _sig_lock;
 rwlock_wrlock(PageMM.FLock);

 if (flags=SCE_KERNEL_DMQ_FIND_NEXT) then
 begin
  It:=PageMM.FDirectAdrSet.find_be(Tmp);
 end else
 begin
  It:=PageMM.FDirectAdrSet.find(Tmp);
 end;

 if (It.Item=nil) then
 begin
  Result:=SCE_KERNEL_ERROR_EACCES;
 end else
 begin
  Tmp:=It.Item^;

  info^.start:=QWORD(Tmp.pAddr);
  info^.__end:=QWORD(Tmp.pAddr)+Tmp.nSize;
  info^.memoryType:=Integer(Tmp.bType);

 end;

 rwlock_unlock(PageMM.FLock);
 _sig_unlock;
end;

function ps4_sceKernelMapDirectMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           protections:Integer;
           flags:Integer;
           physicalAddr:QWORD;
           alignment:QWORD):Integer; SysV_ABI_CDecl;
var
 //flProtect:DWORD;
 R:Pointer;
begin
 if (virtualAddrDest=nil) or (length=0) then Exit(SCE_KERNEL_ERROR_EINVAL);

 //Assert(flags=0);

 //Writeln('AddrSrc:',HexStr(virtualAddrDest^));

 Writeln('length:',HexStr(length,16),' ',
         test_KP_flags(protections),' ',
         'flags:',flags);
 Writeln('length:',HexStr(length,16),' ',
         'physicalAddr:',HexStr(physicalAddr,16),' ',
         'alignment:',HexStr(alignment,16));

 if (alignment=0) then alignment:=LOGICAL_PAGE_SIZE;

 if not IsAlign(virtualAddrDest^,LOGICAL_PAGE_SIZE) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if not IsAlign(length   ,LOGICAL_PAGE_SIZE) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if not IsAlign(alignment,LOGICAL_PAGE_SIZE) then Exit(SCE_KERNEL_ERROR_EINVAL);
 if not IsPowerOfTwo(alignment) then Exit(SCE_KERNEL_ERROR_EINVAL);

 if (flags and SCE_KERNEL_MAP_FIXED)<>0 then
 begin
  R:=virtualAddrDest^;
 end else
 begin
  R:=nil;
 end;

 _sig_lock;
 R:=PageMM.mmap_d(R,length,alignment,physicalAddr,protections,(flags and SCE_KERNEL_MAP_NO_OVERWRITE)=0);
 _sig_unlock;
 //Writeln('alloc:',HexStr(R),'..',HexStr(R+length));
 virtualAddrDest^:=R;

 if (R=nil) then
 begin
  Exit(SCE_KERNEL_ERROR_ENOMEM);
 end;

 if _isgpu(protections) then
 begin
  Writeln('GPU:',HexStr(R),'..',HexStr(R+length));
 end;

 Assert(IsAlign(R,alignment),'sceKernelMapDirectMemory not aligned!');

 Result:=0;
end;

//sceKernelCheckedReleaseDirectMemory

function ps4_sceKernelMapNamedFlexibleMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           protections:Integer;
           flags:Integer;
           name:PChar):Integer; SysV_ABI_CDecl;
var
 //flProtect:DWORD;
 R:Pointer;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (virtualAddrDest=nil) or (length=0) then Exit;

 //Assert(flags=0);

 //Writeln('AddrSrc:',HexStr(virtualAddrDest^));

 Writeln('length:',HexStr(length,16),' ',
         test_KP_flags(protections),' ',
         'flags:',flags);
 Writeln('length:',HexStr(length,16),' ',
         'name:',name);

 if not IsAlign(virtualAddrDest^,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(length,LOGICAL_PAGE_SIZE) then Exit;

 if (flags and SCE_KERNEL_MAP_FIXED)<>0 then
 begin
  R:=virtualAddrDest^;
 end else
 begin
  R:=nil;
 end;

 _sig_lock;
 R:=PageMM.mmap_d(R,length,0,INVALID_DIRECT,protections,(flags and SCE_KERNEL_MAP_NO_OVERWRITE)=0);
 _sig_unlock;

 Writeln('alloc:',HexStr(R),'..',HexStr(R+length));
 virtualAddrDest^:=R;

 if (R=nil) then
 begin
  Exit(SCE_KERNEL_ERROR_ENOMEM);
 end;

 if _isgpu(protections) then
 begin
  Writeln('GPU:',HexStr(R),'..',HexStr(R+length));
 end;

 Result:=0;
end;

function ps4_sceKernelMapFlexibleMemory(
           virtualAddrDest:PPointer;
           length:QWORD;
           protections:Integer;
           flags:Integer):Integer; SysV_ABI_CDecl;
var
 //flProtect:DWORD;
 R:Pointer;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (virtualAddrDest=nil) or (length=0) then Exit;

 //Assert(flags=0);

 //Writeln('AddrSrc:',HexStr(virtualAddrDest^));

 Writeln('length:',HexStr(length,16),' ',
         test_KP_flags(protections),' ',
         'flags:',flags);
 Writeln('length:',HexStr(length,16));

 if not IsAlign(virtualAddrDest^,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(length,LOGICAL_PAGE_SIZE) then Exit;

 if (flags and SCE_KERNEL_MAP_FIXED)<>0 then
 begin
  R:=virtualAddrDest^;
 end else
 begin
  R:=nil;
 end;

 _sig_lock;
 R:=PageMM.mmap_d(R,length,0,INVALID_DIRECT,protections,(flags and SCE_KERNEL_MAP_NO_OVERWRITE)=0);
 _sig_unlock;

 //Writeln('alloc:',HexStr(R),'..',HexStr(R+length));
 virtualAddrDest^:=R;

 if (R=nil) then
 begin
  Exit(SCE_KERNEL_ERROR_ENOMEM);
 end;

 if _isgpu(protections) then
 begin
  Writeln('GPU:',HexStr(R),'..',HexStr(R+length));
 end;

 Result:=0;
end;

//MEMORY_BASIC_INFORMATION = record
//     BaseAddress       : PVOID;   addr to check
//     AllocationBase    : PVOID;   addr to begin region
//     AllocationProtect : DWORD;   R/W/E
//     RegionSize        : PTRUINT; Full size-( (addr to begin)-(addr to check)  )
//     State             : DWORD;   MEM_COMMIT or MEM_RESERVE
//     Protect           : DWORD;   R/W/E
//     _Type             : DWORD;   MEM_PRIVATE
//end;

//flex
function ps4_sceKernelMunmap(addr:Pointer;len:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (addr=nil) or (len=0) then Exit;

 if not IsAlign(addr,LOGICAL_PAGE_SIZE) then Exit;
 if not IsAlign(len,LOGICAL_PAGE_SIZE) then Exit;

 _sig_lock;
 if PageMM.unmap(addr,len) then Result:=0;
 _sig_unlock;
end;

//flex
function ps4_sceKernelQueryMemoryProtection(addr:Pointer;pStart,pEnd:PPointer;pProt:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EACCES;
 //Writeln(HexStr(addr));
 //addr:=AlignDw(addr,LOGICAL_PAGE_SIZE);
 _sig_lock;
 if PageMM.QueryProt(addr,pStart,pEnd,pProt) then Result:=0;
 _sig_unlock;
end;

function ps4_sceKernelVirtualQuery(addr:Pointer;
                                   flags:Integer;
                                   info:pSceKernelVirtualQueryInfo;
                                   infoSize:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 Assert(false,'TODO');
end;

function ps4_sceKernelMprotect(addr:Pointer;len:QWORD;prot:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;

 _sig_lock;
 if PageMM.ChangeProt(addr,len,prot) then Result:=0;
 _sig_unlock;

end;

function ps4_sceKernelSetVirtualRangeName(addr:Pointer;len:QWORD;name:Pchar):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceKernelSetVirtualRangeName:',HexStr(addr),':',len,':',name);
 Result:=0;
end;

function ps4_mmap(addr:Pointer;len:size_t;prot,flags:Integer;fd:Integer;offset:size_t):Pointer; SysV_ABI_CDecl;
Var
 map:Pointer;
 protect:DWORD;
begin
 map:=MAP_FAILED;

 if (fd<>-1) {or ((flags and MAP_ANONYMOUS)=0)} then
 begin
  SetLastError(EBADF);
  Result:=MAP_FAILED;
  Exit;
 end;

 if (not IsAlign(addr,PHYSICAL_PAGE_SIZE)) or
    (not IsAlign(len,PHYSICAL_PAGE_SIZE)) or
    (not IsAlign(offset,PHYSICAL_PAGE_SIZE)) then
 begin
  SetLastError(EINVAL);
  Result:=MAP_FAILED;
  Exit;
 end;

 protect:=__map_mmap_prot_page(prot);

 SetLastError(0);

 if (len=0) or
    // Unsupported flag combinations
    ((flags and MAP_FIXED)<>0) then
    // Usupported protection combinations
    //(prot=PROT_EXEC) then
 begin
  SetLastError(EINVAL);
  Result:=MAP_FAILED;
  Exit;
 end;

 _sig_lock;
 map:=VirtualAlloc(addr,len,MEM_COMMIT or MEM_RESERVE,Protect);
 _sig_unlock;

 if (map=nil) then
 begin
  Result:=MAP_FAILED;
  Exit;
 end;

 Result:=map;
end;

function ps4_munmap(addr:Pointer;len:size_t):Integer; SysV_ABI_CDecl;
var
 Info:TMemoryBasicInformation;
begin
 Result:=-1;
 if (addr=nil) or (len=0) then Exit;

 if not IsAlign(len,PHYSICAL_PAGE_SIZE) then Exit;

 Info:=Default(TMemoryBasicInformation);
 _sig_lock;
 if (VirtualQuery(addr,Info,len)=0) then
 begin
  _sig_unlock;
  Writeln('GetLastError:',GetLastError);
  Exit;
 end;
 _sig_unlock;
 if (Info._Type=MEM_FREE) then
 begin
  Writeln('GetLastError:',GetLastError);
  Exit;
 end;

 Assert((Info.BaseAddress=Info.AllocationBase) and (Info.RegionSize=len),'partial unmap not impliment!');

 _sig_lock;
 if not VirtualFree(addr,0,MEM_RELEASE) then
 begin
  _sig_unlock;
  Writeln('GetLastError:',GetLastError);
  Exit;
 end;
 _sig_unlock;

 Result:=0;
end;

function ps4_msync(addr:Pointer;len:size_t;flags:Integer):Integer; SysV_ABI_CDecl;
begin
 //Writeln('msync:',HexStr(addr));
 System.ReadWriteBarrier;
 Result:=0;
end;

function ps4_mprotect(addr:Pointer;len:size_t;prot:Integer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=__mprotect(addr,len,prot);
 _sig_unlock;
end;

initialization
 PageMM.init;

end.

