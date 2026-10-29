unit vm_pmap_prot;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 atomic,
 vm,
 vmparam,
 md_map;

const
 PMAPP_SHIFT=MD_PAGE_SHIFT; //12;
 PMAPP_SIZE =MD_PAGE_SIZE;  //1 shl PMAPP_SHIFT; //4*1024
 PMAPP_MASK =MD_PAGE_MASK;  //PMAPP_SIZE-1;

 //VM_MAX_BITS          =46;
 //VM_MAXUSER_ADDRESS   =QWORD(1) shl VM_MAX_BITS; //[0..45]

 PAGE_MAP_COUNT_ALL=(QWORD(VM_MAXUSER_ADDRESS) shr PMAPP_SHIFT);
 PAGE_MAP_MASK_ALL =PAGE_MAP_COUNT_ALL-1;

 PAGE_MAP_COUNT_LV1=PAGE_MAP_COUNT_ALL shr 16;
 PAGE_MAP_COUNT_LV2=1 shl 16; //64KB

 PAGE_MAP_COUNT_SZ1=PAGE_MAP_COUNT_LV1*SizeOf(Pointer);
 PAGE_MAP_COUNT_SZ2=PAGE_MAP_COUNT_LV2*SizeOf(Byte);

 //VM_PROT_NONE      =vm_prot_t($00);
 //VM_PROT_READ      =vm_prot_t($01);
 //VM_PROT_WRITE     =vm_prot_t($02);
 //VM_PROT_EXECUTE   =vm_prot_t($04);
 //VM_PROT_COPY      =vm_prot_t($08);
 //VM_PROT_GPU_READ  =vm_prot_t($10);
 //VM_PROT_GPU_WRITE =vm_prot_t($20);
 //VM_PROT_GPU_ALL   =vm_prot_t($30);

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

 PAGE_TRACK_GC    =$40;

 TRACK_PROT=1; //Take tracking bits into account
 REMAP_PROT=2; //Ignore protect bit checking

var
 PAGE_PROT:PPBYTE=nil;

procedure ppmap_mark_rwx  (start,__end:vm_offset_t;prots:Byte);
procedure ppmap_unmark    (start,__end:vm_offset_t;prots:Byte);
procedure ppmap_track     (start,__end:vm_offset_t;prots:Byte);
procedure ppmap_mark_gc   (start,__end:vm_offset_t);
function  ppmap_scan      (start,__end:vm_offset_t):vm_offset_t;
function  ppmap_scan_rwx  (start,__end:vm_offset_t):vm_offset_t;
function  ppmap_get_prot  (addr:vm_offset_t):Byte; inline;
function  ppmap_get_prot  (addr,size:vm_offset_t):Byte;

implementation

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
 p:PByte;
begin
 idx:=OFF_TO_IDX(addr);

 if (idx>PAGE_MAP_MASK_ALL) then
 begin
  Exit(0);
 end;

 p:=PAGE_PROT[LV1_IDX(idx)];

 if (p=nil) then
 begin
  Exit(0);
 end;

 Result:=p[LV2_IDX(idx)];
end;

procedure prealloc(var P:PByte);
var
 ptr:Pointer;
begin
 repeat

  if (P=nil) then
  begin

   ptr:=kmem_alloc(PAGE_MAP_COUNT_SZ2,VM_RW);
   Assert((ptr<>nil),'prealloc');

   //Set cache to be enabled on demand
   md_dontneed(ptr,PAGE_MAP_COUNT_SZ2);

   if (System.InterlockedCompareExchange(P,ptr,nil)=nil) then
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

  pCur:=@PAGE_PROT[lv1][LV2_IDX(start)];
  pEnd:=@PAGE_PROT[lv1][lv2_last];

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

procedure ppmap_mark_gc(start,__end:vm_offset_t);
begin
 _ppmap_mark(start,__end,PAGE_TRACK_GC,0);
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

  pCur:=@PAGE_PROT[lv1][LV2_IDX(start)];
  pEnd:=@PAGE_PROT[lv1][lv2_last];

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

  start:=(lv1 + 1) shl 16;
 end;
 WriteBarrier;
end;

function _get_prot(addr:QWORD):Byte; inline;
var
 p:PByte;
begin
 if (addr>PAGE_MAP_MASK_ALL) then
 begin
  Result:=0
 end else
 begin
  p:=PAGE_PROT[LV1_IDX(addr)];

  if (p=nil) then
  begin
   Result:=0
  end else
  begin
   Result:=p[LV2_IDX(addr)];
  end;
 end;
end;

function ppmap_scan(start,__end:vm_offset_t):vm_offset_t;
var
 idx,endidx,lv1,chunk_end:QWORD;
 p:PByte;
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

  if (p=nil) then
  begin
   if (b<>0) then
   begin
    Exit(IDX_TO_OFF(idx));
   end;

   idx:=chunk_end;
  end else
  begin
   repeat
    v:=p[LV2_IDX(idx)];

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
 p:PByte;
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

  if (p=nil) then
  begin
   if (b<>0) then
   begin
    Exit(IDX_TO_OFF(idx));
   end;

   idx:=chunk_end;
  end else
  begin
   repeat
    v:=p[LV2_IDX(idx)] and PAGE_PROT_RWX;

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
 pa,pb:PBYTE;
 c,d:Byte;
begin
 start_idx:=OFF_TO_IDX(addr);

 if (start_idx>PAGE_MAP_MASK_ALL) then
 begin
  Exit(0);
 end;

 pa:=PAGE_PROT[LV1_IDX(start_idx)];
 if (pa=nil) then
 begin
  Exit(0);
 end;

 c:=pa[LV2_IDX(start_idx)];

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
 if (pb=nil) then
 begin
  Exit(0);
 end;

 d:=pb[LV2_IDX(end_idx)];

 Result:=((c and Byte(PAGE_PROT_RWX)) and (d and Byte(PAGE_PROT_RWX))) or
         ((c and Byte(not PAGE_PROT_RWX)) or (d and Byte(not PAGE_PROT_RWX)));
end;


end.



