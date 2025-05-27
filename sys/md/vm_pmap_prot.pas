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

 PAGE_MAP_COUNT_ALL=(QWORD(VM_MAXUSER_ADDRESS) shr PMAPP_SHIFT);
 PAGE_MAP_MASK_ALL =PAGE_MAP_COUNT_ALL-1;

 PAGE_MAP_COUNT_LV1=PAGE_MAP_COUNT_ALL shr 16;
 PAGE_MAP_COUNT_LV2=1 shl 16; //64KB

 PAGE_MAP_COUNT_SZ1=PAGE_MAP_COUNT_LV1*SizeOf(Pointer);
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
 PAGE_PROT:PPBYTE=nil;

procedure ppmap_mark_rwx  (start,__end:vm_offset_t;prots:Byte);
procedure ppmap_unmark    (start,__end:vm_offset_t;prots:Byte);
procedure ppmap_track     (start,__end:vm_offset_t;prots:Byte);
//procedure ppmap_untrack   (start,__end:vm_offset_t;prots:Byte);
function  ppmap_scan      (start,__end:vm_offset_t):vm_offset_t;
function  ppmap_scan_rwx  (start,__end:vm_offset_t):vm_offset_t;
function  ppmap_get_prot  (addr:vm_offset_t):Byte;
function  ppmap_get_prot  (addr,size:vm_offset_t):Byte;

implementation

//PAGE_MAP

function IDX_TO_OFF(x:DWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PMAPP_SHIFT;
end;

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PMAPP_SHIFT;
end;

function MAX_IDX(x:DWORD):DWORD; inline;
begin
 if (x>PAGE_MAP_MASK_ALL) then
  Result:=PAGE_MAP_MASK_ALL
 else
  Result:=x;
end;

function LV1_IDX(x:DWORD):DWORD; inline;
begin
 Result:=x shr 16;
end;

function LV2_IDX(x:DWORD):DWORD; inline;
begin
 Result:=x and (PAGE_MAP_COUNT_LV2-1);
end;

procedure prealloc(i:DWORD);
var
 ptr:Pointer;
 r:Integer;
begin
 repeat

  if (PAGE_PROT[LV1_IDX(i)]=nil) then
  begin

   ptr:=nil;
   r:=md_mmap(ptr,PAGE_MAP_COUNT_SZ2,VM_RW);
   Assert((r=0),'prealloc');

   if (System.InterlockedCompareExchange(PAGE_PROT[LV1_IDX(i)],ptr,nil)=nil) then
   begin
    Exit;
   end;

   md_unmap(ptr,PAGE_MAP_COUNT_SZ2);

  end else
  begin
   Exit;
  end;

 until false;
end;

procedure ppmap_mark_rwx(start,__end:vm_offset_t;prots:Byte);
var
 P:PByte;
 clear:Byte;
begin
 prots:=prots and PAGE_PROT_RWX;
 clear:=(not prots) and PAGE_PROT_RWX;
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);
 while (start<__end) do
 begin
  //
  prealloc(start);
  //
  P:=@PAGE_PROT[LV1_IDX(start)][LV2_IDX(start)];
  atomic_clear_byte(P,clear);
  atomic_set_byte  (P,prots);
  Inc(start);
 end;
 WriteBarrier;
end;

procedure ppmap_unmark(start,__end:vm_offset_t;prots:Byte);
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);
 while (start<__end) do
 begin
  if (PAGE_PROT[LV1_IDX(start)]<>nil) then
  begin
   atomic_clear_byte(@PAGE_PROT[LV1_IDX(start)][LV2_IDX(start)],prots);
  end;
  Inc(start);
 end;
 WriteBarrier;
end;

procedure ppmap_track(start,__end:vm_offset_t;prots:Byte);
var
 P:PByte;
 s_prots:Byte;
 c_prots:Byte;
begin
 s_prots:=prots and PAGE_TRACK_RWX;
 c_prots:=(not prots) and PAGE_TRACK_RWX;

 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);
 while (start<__end) do
 begin
  //
  prealloc(start);
  //
  P:=@PAGE_PROT[LV1_IDX(start)][LV2_IDX(start)];
  atomic_set_byte  (P,s_prots);
  atomic_clear_byte(P,c_prots);
  Inc(start);
 end;
 WriteBarrier;
end;

{
procedure ppmap_untrack(start,__end:vm_offset_t;prots:Byte);
begin
 prots:=prots and PAGE_TRACK_RWX;
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);
 while (start<__end) do
 begin
  atomic_clear_byte(@PAGE_PROT[start],prots);
  Inc(start);
 end;
 WriteBarrier;
end;
}

function _get_prot(addr:DWORD):Byte; inline;
begin
 if (addr>PAGE_MAP_MASK_ALL) then
 begin
  Result:=0
 end else
 if (PAGE_PROT[LV1_IDX(addr)]=nil) then
 begin
  Result:=0
 end else
 begin
  Result:=PAGE_PROT[LV1_IDX(addr)][LV2_IDX(addr)];
 end;
end;

function ppmap_scan(start,__end:vm_offset_t):vm_offset_t;
var
 b,v:Byte;
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);

 ReadBarrier;

 b:=_get_prot(start);
 Inc(start);

 while (start<__end) do
 begin
  v:=_get_prot(start);

  if (b<>v) then
  begin
   start:=IDX_TO_OFF(start);
   Exit(start);
  end;

  Inc(start);
 end;

 __end:=IDX_TO_OFF(__end);

 Result:=__end;
end;

function ppmap_scan_rwx(start,__end:vm_offset_t):vm_offset_t;
var
 b,v:Byte;
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);

 ReadBarrier;

 b:=(_get_prot(start) and PAGE_PROT_RWX);
 Inc(start);

 while (start<__end) do
 begin
  v:=(_get_prot(start) and PAGE_PROT_RWX);

  if (b<>v) then
  begin
   start:=IDX_TO_OFF(start);
   Exit(start);
  end;

  Inc(start);
 end;

 __end:=IDX_TO_OFF(__end);

 Result:=__end;
end;

function ppmap_get_prot(addr:vm_offset_t):Byte;
begin
 Result:=_get_prot(OFF_TO_IDX(addr));
end;

function ppmap_get_prot(addr,size:vm_offset_t):Byte;
begin
 Result:=ppmap_get_prot(addr) or ppmap_get_prot(addr+size);
end;


end.



