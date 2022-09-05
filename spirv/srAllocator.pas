unit srAllocator;

{$mode objfpc}{$H+}

interface

uses
 ginodes;

type
 PsrAllocNode=^TsrAllocNode;
 TsrAllocNode=packed record
  pNext:PsrAllocNode;
  data:record end;
 end;

 _TsrAllocator=specialize TNodeStack<PsrAllocNode>;
 TsrAllocator=object(_TsrAllocator)
  curr_apos:ptruint; //alloc pos in current node
  curr_size:ptruint; //useable size of current node
  used_size:ptruint; //full usable size
  full_size:ptruint; //full alloc size
  Function  Alloc(Size:ptruint):Pointer;
  Procedure Free;
 end;

implementation

Function TsrAllocator.Alloc(Size:ptruint):Pointer;
const
 asize=$FFFF-SizeOf(ptruint)*3;
var
 mem_size:ptruint;

 function _alloc:Pointer;
 begin
  if (Size>asize-SizeOf(Pointer)) then
  begin
   Result:=AllocMem(Size+SizeOf(Pointer));
  end else
  begin
   Result:=AllocMem(asize);
  end;
 end;

begin
 if (pHead=nil) or (Size>curr_size) then
 begin
  Push_head(_alloc);
  mem_size:=MemSize(pHead);
  curr_apos:=0;
  curr_size:=mem_size-SizeOf(Pointer);
  Inc(full_size,mem_size);
 end;

 Result:=@PByte(@pHead^.data)[curr_apos];

 Inc(used_size,Size);
 Size:=Align(Size,SizeOf(ptruint));
 Inc(curr_apos,Size);
 Dec(curr_size,Size);
end;

Procedure TsrAllocator.Free;
var
 node:PsrAllocNode;
begin
 node:=Pop_head;
 While (node<>nil) do
 begin
  FreeMem(node);
  node:=Pop_head;
 end;
 curr_apos:=0;
 curr_size:=0;
 used_size:=0;
 full_size:=0;
end;

end.

