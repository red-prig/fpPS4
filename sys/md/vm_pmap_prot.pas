unit vm_pmap_prot;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 atomic,
 vm,
 vmparam;

const
 PMAPP_SHIFT=12;
 PMAPP_SIZE =1 shl PMAPP_SHIFT;
 PMAPP_MASK =PMAPP_SIZE-1;

 PAGE_MAP_COUNT   =(QWORD(VM_MAXUSER_ADDRESS) shr PMAPP_SHIFT);
 PAGE_MAP_MASK    =PAGE_MAP_COUNT-1;

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
 PAGE_PROT:PBYTE=nil;

procedure pmap_mark_rwx  (start,__end:vm_offset_t;prots:Byte);
procedure pmap_unmark    (start,__end:vm_offset_t);
procedure pmap_unmark_rwx(start,__end:vm_offset_t);
procedure pmap_track     (start,__end:vm_offset_t;prots:Byte);
procedure pmap_untrack   (start,__end:vm_offset_t;prots:Byte);
function  pmap_scan      (start,__end:vm_offset_t):vm_offset_t;
function  pmap_scan_rwx  (start,__end:vm_offset_t):vm_offset_t;
function  pmap_get_prot  (addr:vm_offset_t):Byte;
function  pmap_get_prot  (addr,size:vm_offset_t):Byte;

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
 if (x>PAGE_MAP_MASK) then
  Result:=PAGE_MAP_MASK
 else
  Result:=x;
end;

procedure pmap_mark_rwx(start,__end:vm_offset_t;prots:Byte);
var
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
  atomic_clear_byte(@PAGE_PROT[start],clear);
  atomic_set_byte  (@PAGE_PROT[start],prots);
  //PAGE_PROT[start]:=prots;
  Inc(start);
 end;
 WriteBarrier;
end;

procedure pmap_unmark(start,__end:vm_offset_t);
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);
 while (start<__end) do
 begin
  //PAGE_PROT[start]:=0;
  Inc(start);
 end;
 WriteBarrier;
end;

procedure pmap_unmark_rwx(start,__end:vm_offset_t);
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);
 while (start<__end) do
 begin
  atomic_clear_byte(@PAGE_PROT[start],PAGE_PROT_RWX);
  Inc(start);
 end;
 WriteBarrier;
end;

procedure pmap_track(start,__end:vm_offset_t;prots:Byte);
begin
 prots:=prots and PAGE_TRACK_RWX;
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);
 while (start<__end) do
 begin
  atomic_set_byte(@PAGE_PROT[start],prots);
  Inc(start);
 end;
 WriteBarrier;
end;

procedure pmap_untrack(start,__end:vm_offset_t;prots:Byte);
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

function pmap_scan(start,__end:vm_offset_t):vm_offset_t;
var
 b,v:Byte;
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);

 ReadBarrier;

 b:=PAGE_PROT[start];
 Inc(start);

 while (start<__end) do
 begin
  v:=PAGE_PROT[start];

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

function pmap_scan_rwx(start,__end:vm_offset_t):vm_offset_t;
var
 b,v:Byte;
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);

 ReadBarrier;

 b:=(PAGE_PROT[start] and PAGE_PROT_RWX);
 Inc(start);

 while (start<__end) do
 begin
  v:=(PAGE_PROT[start] and PAGE_PROT_RWX);

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

function pmap_get_prot(addr:vm_offset_t):Byte;
begin
 addr:=OFF_TO_IDX(addr);
 if (addr>PAGE_MAP_MASK) then
 begin
  Result:=0
 end else
 begin
  Result:=PAGE_PROT[addr];
 end;
end;

function pmap_get_prot(addr,size:vm_offset_t):Byte;
begin
 Result:=pmap_get_prot(addr) or pmap_get_prot(addr+size);
end;


end.



