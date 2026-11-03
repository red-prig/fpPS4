unit vm_pmap_prot;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 vmparam,
 md_map,
 kern_mtx;

type
 PZKPTR=^TZKPTR;
 TZKPTR=packed object
  raw:DWORD;
  function  get_ptr:PBYTE;    inline;
  procedure set_ptr(p:PBYTE); inline;
  property  unzip:PBYTE read get_ptr write set_ptr;
 end;

const
 PMAPP_SHIFT=MD_PAGE_SHIFT; //12;
 PMAPP_SIZE =MD_PAGE_SIZE;  //1 shl PMAPP_SHIFT; //4*1024
 PMAPP_MASK =MD_PAGE_MASK;  //PMAPP_SIZE-1;

 PAGE_MAP_COUNT_ALL=(QWORD(VM_MAXUSER_ADDRESS) shr PMAPP_SHIFT);
 PAGE_MAP_MASK_ALL =PAGE_MAP_COUNT_ALL-1;

 PAGE_MAP_COUNT_LV1=PAGE_MAP_COUNT_ALL shr 16;
 PAGE_MAP_COUNT_LV2=1 shl 16; //64KB

 PAGE_MAP_COUNT_SZ1=PAGE_MAP_COUNT_LV1*SizeOf(TZKPTR);
 PAGE_MAP_COUNT_SZ2=PAGE_MAP_COUNT_LV2*SizeOf(Byte);

 PAGE_PROT_READ   =VM_PROT_READ;
 PAGE_PROT_WRITE  =VM_PROT_WRITE;
 PAGE_PROT_EXECUTE=VM_PROT_EXECUTE;

 PAGE_PROT_RW     =PAGE_PROT_READ or PAGE_PROT_WRITE;
 PAGE_PROT_RWX    =PAGE_PROT_READ or PAGE_PROT_WRITE or PAGE_PROT_EXECUTE;

 PAGE_TRACK_R     =$08;
 PAGE_TRACK_W     =$10;
 PAGE_TRACK_X     =$20;
 PAGE_TRACK_RWX   =PAGE_TRACK_R or PAGE_TRACK_W or PAGE_TRACK_X;
 PAGE_TRACK_SHIFT =3;

 TRACK_PROT=1; //Take tracking bits into account
 REMAP_PROT=2; //Ignore protect bit checking

var
 PAGE_PROT:PZKPTR=nil;

procedure ppmap_mark_rwx  (start,__end:vm_offset_t;prots:Byte);
procedure ppmap_unmark    (start,__end:vm_offset_t;prots:Byte);
procedure ppmap_track     (start,__end:vm_offset_t;prots:Byte);
function  ppmap_scan      (start,__end:vm_offset_t):vm_offset_t;
function  ppmap_scan_rwx  (start,__end:vm_offset_t):vm_offset_t;
function  ppmap_get_prot  (addr:vm_offset_t):Byte; inline;
function  ppmap_get_prot  (addr,size:vm_offset_t):Byte;

implementation

const
 PPMMAP_LOCK_COUNT=1024;
 PPMMAP_LOCK_MASK =PPMMAP_LOCK_COUNT-1;

var
 ppmap_mtx:array[0..PPMMAP_LOCK_COUNT-1] of mtx;

procedure ppmap_mtx_init;
var
 i:Integer;
begin
 For i:=0 to High(ppmap_mtx) do
 begin
  mtx_init(ppmap_mtx[i],'ppmap');
 end;
end;

///

function TZKPTR.get_ptr:PBYTE; inline;
begin
 if (raw=0) then
 begin
  Result:=nil;
 end else
 begin
  Result:=Pointer(KERNEL_LOWER+(QWORD(RAW) shl 16));
 end;
end;

procedure TZKPTR.set_ptr(p:PBYTE); inline;
begin
 if (p=nil) then
 begin
  raw:=0;
 end else
 begin
  raw:=(QWORD(p)-KERNEL_LOWER) shr 16;
  //Assert(get_ptr=p,HexStr(get_ptr)+'<>'+HexStr(p));
 end;
end;

//PAGE_MAP

function LV1_IDX(x:QWORD):DWORD; inline;
begin
 Result:=x shr 16;
end;

function LV2_IDX(x:QWORD):DWORD; inline;
begin
 Result:=x and (PAGE_MAP_COUNT_LV2-1);
end;

function IDX_TO_OFF(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PMAPP_SHIFT;
end;

function OFF_TO_IDX(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shr PMAPP_SHIFT;
end;

function MAX_IDX(x:QWORD):QWORD; inline;
begin
 if (x>PAGE_MAP_COUNT_ALL) then
  Result:=PAGE_MAP_COUNT_ALL
 else
  Result:=x;
end;

function ppmap_get_prot(addr:vm_offset_t):Byte; inline;
var
 idx:QWORD;
 p:TZKPTR;
begin
 idx:=OFF_TO_IDX(addr);

 if (idx>PAGE_MAP_MASK_ALL) then
 begin
  Exit(0);
 end;

 p:=PAGE_PROT[LV1_IDX(idx)];

 if (p.raw=0) then
 begin
  Exit(0);
 end;

 Result:=p.unzip[LV2_IDX(idx)];
end;

procedure prealloc(var P:TZKPTR);
var
 ptr:Pointer;
 new:TZKPTR;
begin
 repeat

  if (P.raw=0) then
  begin

   ptr:=kmem_alloc(PAGE_MAP_COUNT_SZ2,VM_RW);
   Assert((ptr<>nil),'prealloc');

   //Set cache to be enabled on demand
   md_dontneed(ptr,PAGE_MAP_COUNT_SZ2);

   new.unzip:=ptr;

   if (System.InterlockedCompareExchange(P.raw,new.raw,0)=0) then
   begin
    Exit;
   end;

   kmem_free(ptr,PAGE_MAP_COUNT_SZ2);

  end else
  begin
   Exit;
  end;

 until false;
end;

procedure _ppmap_mark(start,__end:vm_offset_t;prots,clear:Byte); inline;
var
 lv1_last,lv1,lv2_last:QWORD;
 pCur,pEnd:PByte;
 qmask,qprots:QWORD;
 b:Byte;
 end_partial:Boolean;
begin
 end_partial:=(__end and PMAPP_MASK)<>0;

 start:=OFF_TO_IDX(start);
 if (start>PAGE_MAP_MASK_ALL) then Exit;

 __end:=OFF_TO_IDX(__end);
 if end_partial then Inc(__end);
 __end:=MAX_IDX   (__end);

 lv1_last:=LV1_IDX(__end);

 b:=Byte(not clear);
 qmask :=QWORD(b)    *QWORD($0101010101010101);
 qprots:=QWORD(prots)*QWORD($0101010101010101);

 while (start < __end) do
 begin
  lv1:=LV1_IDX(start);
  prealloc(PAGE_PROT[lv1]);

  if (lv1_last=lv1) then
  begin
   lv2_last:=LV2_IDX(__end);
  end else
  begin
   lv2_last:=PAGE_MAP_COUNT_LV2;
  end;

  pCur:=@PAGE_PROT[lv1].unzip[LV2_IDX(start)];
  pEnd:=@PAGE_PROT[lv1].unzip[lv2_last];

  mtx_lock(ppmap_mtx[lv1 and PPMMAP_LOCK_MASK]);

  while (pCur<pEnd) and ((QWORD(pCur) and 7)<>0) do
  begin
   pCur^:=(pCur^ and b) or prots;
   Inc(pCur);
  end;

  while (QWORD(pEnd-pCur)>=8) do
  begin
   PQWORD(pCur)^:=(PQWORD(pCur)^ and qmask) or qprots;
   Inc(pCur,8);
  end;

  while (pCur<pEnd) do
  begin
   pCur^:=(pCur^ and b) or prots;
   Inc(pCur);
  end;

  mtx_unlock(ppmap_mtx[lv1 and PPMMAP_LOCK_MASK]);

  start:=(lv1 + 1) shl 16;
 end;
 WriteBarrier;
end;

procedure ppmap_mark_rwx(start,__end:vm_offset_t;prots:Byte);
begin
 _ppmap_mark(start,__end,prots and PAGE_PROT_RWX,PAGE_PROT_RWX);
end;

procedure ppmap_track(start,__end:vm_offset_t;prots:Byte);
begin
 _ppmap_mark(start,__end,prots and PAGE_TRACK_RWX,PAGE_TRACK_RWX);
end;

procedure ppmap_unmark(start,__end:vm_offset_t;prots:Byte);
var
 lv1_last,lv1,lv2_last:QWORD;
 pCur,pEnd:PByte;
 qmask:QWORD;
 end_partial:Boolean;
begin
 end_partial:=(__end and PMAPP_MASK)<>0;

 start:=OFF_TO_IDX(start);
 if (start>PAGE_MAP_MASK_ALL) then Exit;

 __end:=OFF_TO_IDX(__end);
 if end_partial then Inc(__end);
 __end:=MAX_IDX   (__end);

 lv1_last:=LV1_IDX(__end);

 qmask:=QWORD(Byte(not prots))*QWORD($0101010101010101);

 while (start < __end) do
 begin
  lv1:=LV1_IDX(start);
  prealloc(PAGE_PROT[lv1]);

  if (lv1_last=lv1) then
  begin
   lv2_last:=LV2_IDX(__end);
  end else
  begin
   lv2_last:=PAGE_MAP_COUNT_LV2;
  end;

  pCur:=@PAGE_PROT[lv1].unzip[LV2_IDX(start)];
  pEnd:=@PAGE_PROT[lv1].unzip[lv2_last];

  mtx_lock(ppmap_mtx[lv1 and PPMMAP_LOCK_MASK]);

  while (pCur<pEnd) and ((QWORD(pCur) and 7)<>0) do
  begin
   pCur^:=pCur^ and Byte(not prots);
   Inc(pCur);
  end;

  while (QWORD(pEnd-pCur)>=8) do
  begin
   PQWORD(pCur)^:=PQWORD(pCur)^ and qmask;
   Inc(pCur,8);
  end;

  while (pCur<pEnd) do
  begin
   pCur^:=pCur^ and Byte(not prots);
   Inc(pCur);
  end;

  mtx_unlock(ppmap_mtx[lv1 and PPMMAP_LOCK_MASK]);

  start:=(lv1 + 1) shl 16;
 end;
 WriteBarrier;
end;

function _get_prot(addr:QWORD):Byte; inline;
var
 p:TZKPTR;
begin
 if (addr>PAGE_MAP_MASK_ALL) then
 begin
  Result:=0
 end else
 begin
  p:=PAGE_PROT[LV1_IDX(addr)];

  if (p.raw=0) then
  begin
   Result:=0
  end else
  begin
   Result:=p.unzip[LV2_IDX(addr)];
  end;
 end;
end;

function ppmap_scan(start,__end:vm_offset_t):vm_offset_t;
var
 idx,endidx,lv1,chunk_end:QWORD;
 p:TZKPTR;
 b,v:Byte;
begin
 idx:=MAX_IDX(OFF_TO_IDX(start));
 endidx:=MAX_IDX(OFF_TO_IDX(__end));

 ReadBarrier;

 if (idx>=endidx) then
 begin
  Exit(IDX_TO_OFF(endidx));
 end;

 b:=_get_prot(idx);
 Inc(idx);

 while (idx<endidx) do
 begin
  lv1:=LV1_IDX(idx);
  chunk_end:=(lv1+1) shl 16;
  if (chunk_end>endidx) then
  begin
   chunk_end:=endidx;
  end;

  p:=PAGE_PROT[lv1];

  if (p.raw=0) then
  begin
   if (b<>0) then
   begin
    Exit(IDX_TO_OFF(idx));
   end;

   idx:=chunk_end;
  end else
  begin
   repeat
    v:=p.unzip[LV2_IDX(idx)];

    if (b<>v) then
    begin
     Exit(IDX_TO_OFF(idx));
    end;

    Inc(idx);
   until (idx>=chunk_end);
  end;
 end;

 Result:=IDX_TO_OFF(endidx);
end;

function ppmap_scan_rwx(start,__end:vm_offset_t):vm_offset_t;
var
 idx,endidx,lv1,chunk_end:QWORD;
 p:TZKPTR;
 b,v:Byte;
begin
 idx:=MAX_IDX(OFF_TO_IDX(start));
 endidx:=MAX_IDX(OFF_TO_IDX(__end));

 ReadBarrier;

 if (idx>=endidx) then
 begin
  Exit(IDX_TO_OFF(endidx));
 end;

 b:=_get_prot(idx) and PAGE_PROT_RWX;
 Inc(idx);

 while (idx<endidx) do
 begin
  lv1:=LV1_IDX(idx);
  chunk_end:=(lv1+1) shl 16;
  if (chunk_end>endidx) then
  begin
   chunk_end:=endidx;
  end;

  p:=PAGE_PROT[lv1];

  if (p.raw=0) then
  begin
   if (b<>0) then
   begin
    Exit(IDX_TO_OFF(idx));
   end;

   idx:=chunk_end;
  end else
  begin
   repeat
    v:=p.unzip[LV2_IDX(idx)] and PAGE_PROT_RWX;

    if (b<>v) then
    begin
     Exit(IDX_TO_OFF(idx));
    end;

    Inc(idx);
   until (idx>=chunk_end);
  end;
 end;

 Result:=IDX_TO_OFF(endidx);
end;

function ppmap_get_prot(addr,size:vm_offset_t):Byte;
var
 start_idx,end_idx:QWORD;
 pa,pb:TZKPTR;
 c,d:Byte;
begin
 start_idx:=OFF_TO_IDX(addr);

 if (start_idx>PAGE_MAP_MASK_ALL) then
 begin
  Exit(0);
 end;

 pa:=PAGE_PROT[LV1_IDX(start_idx)];
 if (pa.raw=0) then
 begin
  Exit(0);
 end;

 c:=pa.unzip[LV2_IDX(start_idx)];

 if (size<=1) then
 begin
  Exit(c);
 end;

 end_idx:=OFF_TO_IDX(addr+size-1);

 if (end_idx=start_idx) then
 begin
  Exit(c);
 end;

 if (end_idx>PAGE_MAP_MASK_ALL) then
 begin
  Exit(0);
 end;

 pb:=PAGE_PROT[LV1_IDX(end_idx)];
 if (pb.raw=0) then
 begin
  Exit(0);
 end;

 d:=pb.unzip[LV2_IDX(end_idx)];

 Result:=((c and Byte(PAGE_PROT_RWX)) and (d and Byte(PAGE_PROT_RWX))) or
         ((c and Byte(not PAGE_PROT_RWX)) or (d and Byte(not PAGE_PROT_RWX)));
end;


initialization
 ppmap_mtx_init;

end.



