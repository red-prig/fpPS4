unit vm_pmap_prot;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
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

 PAGE_PROT_LIFT   =$40;

 //PAGE_BUSY_FLAG   =DWORD($10000000);
 //PAGE_PATCH_FLAG  =DWORD($08000000);

var
 PAGE_PROT:PBYTE=nil;

procedure pmap_mark      (start,__end:vm_offset_t;prots:Byte);
procedure pmap_unmark    (start,__end:vm_offset_t);
function  pmap_scan_rwx  (start,__end:vm_offset_t):vm_offset_t;
function  pmap_get_prot  (addr:vm_offset_t):Byte;

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

procedure pmap_mark(start,__end:vm_offset_t;prots:Byte);
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);
 while (start<__end) do
 begin
  PAGE_PROT[start]:=prots;
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
  PAGE_PROT[start]:=0;
  Inc(start);
 end;
 WriteBarrier;
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
 addr:=MAX_IDX(addr);
 Result:=PAGE_PROT[addr];
end;



end.



