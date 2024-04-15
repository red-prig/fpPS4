unit pm4_stream;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 LFQueue,
 md_map,
 bittype,
 pm4defs,
 si_ci_vi_merged_offset,
 si_ci_vi_merged_enum,
 si_ci_vi_merged_registers,
 si_ci_vi_merged_groups;

type
 t_cache_block_allocator=object
  const
   mem_size =64*1024;
   max_count=256;
  //
  var
   queue:TIntrusiveMPSCQueue;
   xlock:Pointer;
   count:QWORD;
  //
  procedure Init;
  Function  Alloc:Pointer;
  Procedure Free(node:Pointer);
 end;

 t_pm4_allocator=object
  type
   PAllocNode=^TAllocNode;
   TAllocNode=packed record
    link:PAllocNode;
    size:QWORD;
    data:record end;
   end;
  var
   pHead:SLIST_HEAD;
   curr_apos:ptruint; //alloc pos in current node
   curr_size:ptruint; //useable size of current node
   used_size:ptruint; //full usable size
   full_size:ptruint; //full alloc size
  Function  Alloc(Size:ptruint):Pointer;
  Procedure Free;
 end;

 t_pm4_node_type=(
  ntLoadConstRam,
  ntEventWrite,
  ntEventWriteEop,
  ntFastClear,
  ntResolve,
  ntDrawIndex2
 );

 p_pm4_node=^t_pm4_node;
 t_pm4_node=object
  entry:TAILQ_ENTRY;
  ntype:t_pm4_node_type;
 end;

 p_pm4_node_LoadConstRam=^t_pm4_node_LoadConstRam;
 t_pm4_node_LoadConstRam=object(t_pm4_node)
  addr  :Pointer;
  num_dw:Word;
  offset:Word;
 end;

 p_pm4_node_EventWrite=^t_pm4_node_EventWrite;
 t_pm4_node_EventWrite=object(t_pm4_node)
  eventType:Byte;
 end;

 p_pm4_node_EventWriteEop=^t_pm4_node_EventWriteEop;
 t_pm4_node_EventWriteEop=object(t_pm4_node)
  addr   :Pointer;
  data   :QWORD;
  dataSel:Byte;
  intSel :Byte;
 end;

 p_pm4_node_FastClear=^t_pm4_node_FastClear;
 t_pm4_node_FastClear=object(t_pm4_node)
  CX_REG:TCONTEXT_REG_GROUP; // 0xA000
 end;

 p_pm4_node_Resolve=^t_pm4_node_Resolve;
 t_pm4_node_Resolve=object(t_pm4_node)
  CX_REG:TCONTEXT_REG_GROUP; // 0xA000
 end;

 p_pm4_node_DrawIndex2=^t_pm4_node_DrawIndex2;
 t_pm4_node_DrawIndex2=object(t_pm4_node)
  addr  :Pointer;
  //
  SH_REG:TSH_REG_GROUP;      // 0x2C00
  CX_REG:TCONTEXT_REG_GROUP; // 0xA000
 end;

 p_pm4_stream=^t_pm4_stream;
 t_pm4_stream=object
  next_:Pointer;
  //
  allocator:t_pm4_allocator;
  //
  list:TAILQ_HEAD; //t_pm4_node
  //
  procedure Free;
  Procedure add_node(node:p_pm4_node);
  function  First:p_pm4_node;
  function  Next(node:p_pm4_node):p_pm4_node; static;
  //
  procedure LoadConstRam (addr:Pointer;num_dw,offset:Word);
  procedure EventWrite   (eventType:Byte);
  procedure EventWriteEop(addr:Pointer;data:QWORD;dataSel,intSel:Byte);
  procedure FastClear    (var CX_REG:TCONTEXT_REG_GROUP);
  procedure Resolve      (var CX_REG:TCONTEXT_REG_GROUP);
  function  ColorControl (var CX_REG:TCONTEXT_REG_GROUP):Boolean;
  procedure DrawIndex2   (addr:Pointer;var SH_REG:TSH_REG_GROUP;var CX_REG:TCONTEXT_REG_GROUP);
 end;

implementation

var
 cache_block_allocator:t_cache_block_allocator;

procedure t_pm4_stream.Free;
begin
 list:=Default(TAILQ_HEAD);
 allocator.Free;
end;

Procedure t_pm4_stream.add_node(node:p_pm4_node);
begin
 if (list.tqh_first=nil) and
    (list.tqh_last =nil) then
 begin
  TAILQ_INIT(@list);
 end;

 TAILQ_INSERT_TAIL(@list,node,@node^.entry);
end;

function t_pm4_stream.First:p_pm4_node;
begin
 Result:=TAILQ_FIRST(@list);
end;

function t_pm4_stream.Next(node:p_pm4_node):p_pm4_node;
begin
 Result:=TAILQ_NEXT(node,@node^.entry);
end;

procedure t_pm4_stream.LoadConstRam(addr:Pointer;num_dw,offset:Word);
var
 node:p_pm4_node_LoadConstRam;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_LoadConstRam));

 node^.ntype :=ntLoadConstRam;
 node^.addr  :=addr;
 node^.num_dw:=num_dw;
 node^.offset:=offset;

 add_node(node);
end;

procedure t_pm4_stream.EventWrite(eventType:Byte);
var
 node:p_pm4_node_EventWrite;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_EventWrite));

 node^.ntype :=ntEventWrite;
 node^.eventType:=eventType;

 add_node(node);
end;

procedure t_pm4_stream.EventWriteEop(addr:Pointer;data:QWORD;dataSel,intSel:Byte);
var
 node:p_pm4_node_EventWriteEop;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_EventWriteEop));

 node^.ntype  :=ntEventWriteEop;
 node^.addr   :=addr;
 node^.data   :=data;
 node^.dataSel:=dataSel;
 node^.intSel :=intSel;

 add_node(node);
end;

procedure t_pm4_stream.FastClear(var CX_REG:TCONTEXT_REG_GROUP);
var
 node:p_pm4_node_FastClear;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_FastClear));

 node^.ntype :=ntFastClear;
 node^.CX_REG:=CX_REG;

 add_node(node);
end;

procedure t_pm4_stream.Resolve(var CX_REG:TCONTEXT_REG_GROUP);
var
 node:p_pm4_node_Resolve;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_Resolve));

 node^.ntype :=ntResolve;
 node^.CX_REG:=CX_REG;

 add_node(node);
end;

function t_pm4_stream.ColorControl(var CX_REG:TCONTEXT_REG_GROUP):Boolean;
begin
 Result:=False;

 case CX_REG.CB_COLOR_CONTROL.MODE of
  CB_DISABLE             :Writeln('DISABLE');
  CB_NORMAL              :;
  CB_ELIMINATE_FAST_CLEAR:
   // Expand latest specified clear color into pixel data for the fast cleared color/depth resource.
   begin
    FastClear(CX_REG);
    Exit(True);
   end;
  CB_RESOLVE:
   // Fixed function resolve. (MSAA)
   begin
    Resolve(CX_REG);
    Exit(True);
   end;
  CB_DECOMPRESS          :Writeln('DECOMPRESS');
  CB_FMASK_DECOMPRESS    :Writeln('FMASK_DECOMPRESS'); // Fmask decompression for shader readability.
  CB_DCC_DECOMPRESS      :Writeln('DCC_DECOMPRESS');   // Indicates this color target view is for a DCC decompress
  else
   Assert(False);
 end;

end;

procedure t_pm4_stream.DrawIndex2(addr:Pointer;var SH_REG:TSH_REG_GROUP;var CX_REG:TCONTEXT_REG_GROUP);
var
 node:p_pm4_node_DrawIndex2;
begin
 if ColorControl(CX_REG) then Exit;

 node:=allocator.Alloc(SizeOf(t_pm4_node_DrawIndex2));

 node^.ntype :=ntDrawIndex2;
 node^.addr  :=addr;
 node^.SH_REG:=SH_REG;
 node^.CX_REG:=CX_REG;

 add_node(node);
end;

//

procedure t_cache_block_allocator.init;
begin
 queue.Create;
 xlock:=nil;
 count:=0;
end;

Function t_cache_block_allocator.Alloc:Pointer;
begin
 Result:=nil;
 if (XCHG(xlock,Pointer(1))=nil) then
 begin
  if queue.Pop(Result) then
  begin
   XCHG(xlock,nil);
   //
   System.InterlockedDecrement64(count);
   Exit;
  end;
  XCHG(xlock,nil);
 end;
 //
 md_mmap(Result,mem_size,MD_PROT_RW);
end;

Procedure t_cache_block_allocator.Free(node:Pointer);
begin
 if (node=nil) then Exit;
 //
 if (count+1<=max_count) then
 begin
  if (System.InterlockedIncrement64(count)<=max_count) then
  begin
   queue.Push(node);
   Exit;
  end else
  begin
   System.InterlockedDecrement64(count);
  end;
 end;
 //
 md_unmap(node,mem_size);
end;

//

Function t_pm4_allocator.Alloc(Size:ptruint):Pointer;
var
 mem_size:ptruint;
 node:PAllocNode;

begin
 if (pHead.slh_first=nil) or (Size>curr_size) then
 begin

  if (Size>(cache_block_allocator.mem_size-SizeOf(TAllocNode))) then
  begin
   mem_size:=Align(Size+SizeOf(TAllocNode),64*1024);
   //
   node:=nil;
   md_mmap(node,mem_size,MD_PROT_RW);
  end else
  begin
   mem_size:=cache_block_allocator.mem_size;
   //
   node:=cache_block_allocator.Alloc;
  end;

  node^.size:=mem_size;

  SLIST_INSERT_HEAD(@pHead,node,@node^.link);

  curr_apos:=0;
  curr_size:=mem_size-SizeOf(TAllocNode);
  Inc(full_size,mem_size);
 end;

 node:=SLIST_FIRST(@pHead);

 Result:=@PByte(@node^.data)[curr_apos];

 Inc(used_size,Size);
 Size:=Align(Size,SizeOf(ptruint));
 Inc(curr_apos,Size);
 Dec(curr_size,Size);
end;

Procedure t_pm4_allocator.Free;
var
 node:PAllocNode;
begin
 node:=pHead.slh_first;
 if (node<>nil) then
 begin
  pHead.slh_first:=node^.link;
 end;
 While (node<>nil) do
 begin

  if (node^.size=cache_block_allocator.mem_size) then
  begin
   cache_block_allocator.Free(node);
  end else
  begin
   md_unmap(node,node^.size);
  end;

  node:=pHead.slh_first;
  if (node<>nil) then
  begin
   pHead.slh_first:=node^.link;
  end;
 end;
 Self:=Default(t_pm4_allocator);
end;


initialization
 cache_block_allocator.Init;

end.

