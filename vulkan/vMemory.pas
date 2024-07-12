unit vMemory;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 mqueue,
 vmparam,
 Vulkan,
 vDevice,
 vDependence;

type
 TvMemInfo=bitpacked record
  heap_index     :0..VK_MAX_MEMORY_HEAPS-1;
  mem_type       :0..VK_MAX_MEMORY_TYPES-1;
  device_local   :Boolean;
  device_coherent:Boolean;
  host_visible   :Boolean;
  host_coherent  :Boolean;
 end;

 pp_gpu_map_entry=^p_gpu_map_entry;
 p_gpu_map_entry=^gpu_map_entry;
 gpu_map_entry=packed record
  prev    :p_gpu_map_entry; // previous entry
  next    :p_gpu_map_entry; // next entry
  left    :p_gpu_map_entry; // left child in binary search tree
  right   :p_gpu_map_entry; // right child in binary search tree
  start   :TVkDeviceSize;   // start address
  __end   :TVkDeviceSize;   // end address
  adj_free:TVkDeviceSize;   // amount of adjacent free space
  max_free:TVkDeviceSize;   // max free space in subtree
 end;

 p_gpu_map=^gpu_map;
 gpu_map=object
  header  :gpu_map_entry;   // List of entries
  root    :p_gpu_map_entry; // Root of a binary search tree
  size    :TVkDeviceSize;   // virtual size
  nentries:Integer;         // Number of entries
  property min_offset:TVkDeviceSize read header.start write header.start;
  property max_offset:TVkDeviceSize read header.__end write header.__end;
 end;

 TvDeviceMemory=class(TvDependenciesObject)
  entry:TAILQ_ENTRY;
  //
  FHandle :TVkDeviceMemory;
  FSize   :TVkDeviceSize;
  FMemInfo:TvMemInfo;
  FMap    :gpu_map;
  //
  Constructor Create(Handle:TVkDeviceMemory;Size:TVkDeviceSize;mem_type:Byte;mem_info:PVkMemoryType);
  Destructor  Destroy; override;
  Procedure   Flush;
 end;

 TvHostMemory=class(TvDeviceMemory)
  FStart:QWORD;
  F__End:QWORD;
 end;

 TvPointer=packed object
  FMemory:TvDeviceMemory;
  FOffset:TVkDeviceSize;
  function  Acquire:Boolean;
  function  Release:Boolean;
 end;

Const
 GRANULAR_DEV_BLOCK_SIZE=128*1024*1024;
 GRANULAR_MAP_BLOCK_SIZE= 16*1024*1024;

type
 PvHeap=^TvHeap;
 TvHeap=bitpacked record
  heap_size      :TVkDeviceSize;
  heap_index     :0..VK_MAX_MEMORY_HEAPS-1;
  def_mem_type   :0..VK_MAX_MEMORY_TYPES-1;
  device_local   :Boolean;
  device_coherent:Boolean;
  host_visible   :Boolean;
  host_coherent  :Boolean;
 end;

const
 V_PROP_DEVICE_LOCAL   =ord(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT       );
 V_PROP_HOST_VISIBLE   =ord(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT       );
 V_PROP_HOST_COHERENT  =ord(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT      );
 V_PROP_HOST_CACHED    =ord(VK_MEMORY_PROPERTY_HOST_CACHED_BIT        );
 V_PROP_DEVICE_COHERENT=ord(VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD);
 V_PROP_DEVICE_UNCACHED=ord(VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD);
 //
 V_PROP_BEST_FIT       =$80000000;

type
 TvMemManager=class
  public
   FProperties:TVkPhysicalDeviceMemoryProperties;

   FSparceMemoryTypes:TVkUInt32;

   FHeaps:array of TvHeap;

  private
   FDevs:TAILQ_HEAD; //TvDeviceMemory

   FBacked:TvDeviceMemory;

   FHosts:TAILQ_HEAD; //TvHostMemory

  public

   Constructor Create;

   function    findMemoryType(Filter:TVkUInt32;prop:TVkMemoryPropertyFlags;start:Integer):Integer;
   procedure   LoadMemoryHeaps;
   procedure   PrintMemoryHeaps;
   procedure   PrintMemoryType(typeFilter:TVkUInt32);

   Function    FetchMemory(const mr:TVkMemoryRequirements;prop:TVkMemoryPropertyFlags):TvPointer;

   Function    FetchMemory(Size          :TVkDeviceSize;
                           Align         :TVkDeviceSize;
                           memoryTypeBits:TVkUInt32;
                           best_fit      :Boolean):TvPointer;

   Function    FreeMemory(P:TvPointer):Boolean;

  private

   Function    _AllocDevBlock(Size:TVkDeviceSize;mtindex:Byte):TvDeviceMemory;
   procedure   _FreeDevBlock(node:TvDeviceMemory);

   Function    _shrink_dev_block(max:TVkDeviceSize;heap_index:Byte):TVkDeviceSize;
   Function    _shrink_host_map (max:TVkDeviceSize;heap_index:Byte):TVkDeviceSize;
   Function    _shrink(max:TVkDeviceSize;mtindex:Byte;mode:Byte):TVkDeviceSize;

  public

   procedure   unmap_host(start,__end:QWORD);

   Function    FetchHostMap(Addr,Size:TVkDeviceSize;mtindex:Byte):TvPointer;
   Function    FetchHostMap(Addr,Size:TVkDeviceSize;device_local:Boolean):TvPointer;

   Procedure   Flush;
 end;

var
 MemManager:TvMemManager;

const
 buf_ext:TVkExternalMemoryBufferCreateInfo=(
  sType:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO;
  pNext:nil;
  handleTypes:ord(VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT);
 );

function vkAllocMemory         (device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32):TVkDeviceMemory;
function vkAllocHostMemory     (device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32;addr:Pointer     ):TVkDeviceMemory;
function vkAllocDedicatedImage (device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32;FHandle:TVkImage ):TVkDeviceMemory;
function vkAllocDedicatedBuffer(device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32;FHandle:TVkBuffer):TVkDeviceMemory;

function GetHostMappedRequirements:TVkMemoryRequirements;
function GetSparceMemoryTypes:TVkUInt32;

implementation

uses
 kern_rwlock,
 kern_dmem;

var
 global_mem_lock:Pointer=nil;

function AlignUp(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
var
 tmp:PtrUInt;
begin
 tmp:=addr+PtrUInt(alignment-1);
 Result:=tmp-(tmp mod alignment);
end;

function AlignDw(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
begin
 Result:=addr-(addr mod alignment);
end;

function Max(a,b:Ptruint):Ptruint; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

function Min(a,b:Ptruint):Ptruint; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;


//

const
 GPU_SUCCESS        =0;
 GPU_INVALID_ADDRESS=1;
 GPU_NO_SPACE       =2;

procedure gpu_map_init(map:p_gpu_map;min,max:TVkDeviceSize);
begin
 map^.header.next:=@map^.header;
 map^.header.prev:=@map^.header;
 map^.min_offset:=min;
 map^.max_offset:=max;
 map^.header.adj_free:=(max-min);
 map^.header.max_free:=(max-min);
 map^.root:=nil;
end;

function gpu_map_entry_create(map:p_gpu_map):p_gpu_map_entry;
var
 new_entry:p_gpu_map_entry;
begin
 new_entry:=AllocMem(SizeOf(gpu_map_entry));
 Assert((new_entry<>nil),'gpu_map_entry_create: kernel resources exhausted');
 Result:=new_entry;
end;

procedure gpu_map_entry_dispose(map:p_gpu_map;entry:p_gpu_map_entry); inline;
begin
 FreeMem(entry);
end;

procedure gpu_map_entry_set_max_free(entry:p_gpu_map_entry);
begin
 entry^.max_free:=entry^.adj_free;
 if (entry^.left<>nil) then
 if (entry^.left^.max_free>entry^.max_free) then
 begin
  entry^.max_free:=entry^.left^.max_free;
 end;
 if (entry^.right<>nil) then
 if (entry^.right^.max_free>entry^.max_free) then
 begin
  entry^.max_free:=entry^.right^.max_free;
 end;
end;

function gpu_map_entry_splay(addr:TVkDeviceSize;root:p_gpu_map_entry):p_gpu_map_entry;
var
 llist,rlist:p_gpu_map_entry;
 ltree,rtree:p_gpu_map_entry;
 y          :p_gpu_map_entry;
begin
 { Special case of empty tree. }
 if (root=nil) then Exit(root);

 {
  * Pass One: Splay down the tree until we find addr or a nil
  * pointer where addr would go.  llist and rlist are the two
  * sides in reverse order (bottom-up), with llist linked by
  * the right pointer and rlist linked by the left pointer in
  * the vm_map_entry.  Wait until Pass Two to set max_free on
  * the two spines.
  }
 llist:=nil;
 rlist:=nil;
 repeat
  { root is never nil in here. }
  if (addr<root^.start) then
  begin
   y:=root^.left;
   if (y=nil) then break;
   if (addr<y^.start) and (y^.left<>nil) then
   begin
    { Rotate right and put y on rlist. }
    root^.left:=y^.right;
    y^.right:=root;
    gpu_map_entry_set_max_free(root);
    root:=y^.left;
    y^.left:=rlist;
    rlist:=y;
   end else
   begin
    { Put root on rlist. }
    root^.left:=rlist;
    rlist:=root;
    root:=y;
   end;
  end else
  if (addr>=root^.__end) then
  begin
   y:=root^.right;
   if (y=nil) then break;
   if (addr>=y^.__end) and (y^.right<>nil) then
   begin
    { Rotate left and put y on llist. }
    root^.right:=y^.left;
    y^.left:=root;
    gpu_map_entry_set_max_free(root);
    root:=y^.right;
    y^.right:=llist;
    llist:=y;
   end else
   begin
    { Put root on llist. }
    root^.right:=llist;
    llist:=root;
    root:=y;
   end;
  end else
  begin
   break;
  end;
 until false;

 {
  * Pass Two: Walk back up the two spines, flip the pointers
  * and set max_free.  The subtrees of the root go at the
  * bottom of llist and rlist.
  }
 ltree:=root^.left;
 while (llist<>nil) do
 begin
  y:=llist^.right;
  llist^.right:=ltree;
  gpu_map_entry_set_max_free(llist);
  ltree:=llist;
  llist:=y;
 end;
 rtree:=root^.right;
 while (rlist<>nil) do
 begin
  y:=rlist^.left;
  rlist^.left:=rtree;
  gpu_map_entry_set_max_free(rlist);
  rtree:=rlist;
  rlist:=y;
 end;

 {
  * Final assembly: add ltree and rtree as subtrees of root.
  }
 root^.left:=ltree;
 root^.right:=rtree;
 gpu_map_entry_set_max_free(root);

 Result:=(root);
end;

procedure gpu_map_entry_link(
           map        :p_gpu_map;
           after_where:p_gpu_map_entry;
           entry      :p_gpu_map_entry);
begin
 Inc(map^.nentries);
 entry^.prev:=after_where;
 entry^.next:=after_where^.next;
 entry^.next^.prev:=entry;
 after_where^.next:=entry;

 if (after_where<>@map^.header) then
 begin
  if (after_where<>map^.root) then
  begin
   gpu_map_entry_splay(after_where^.start, map^.root);
  end;
  entry^.right:=after_where^.right;
  entry^.left:=after_where;
  after_where^.right:=nil;
  after_where^.adj_free:=entry^.start - after_where^.__end;
  gpu_map_entry_set_max_free(after_where);
 end else
 begin
  entry^.right:=map^.root;
  entry^.left:=nil;
 end;
 if (entry^.next=@map^.header) then
 begin
  entry^.adj_free:=map^.max_offset-entry^.__end;
 end else
 begin
  entry^.adj_free:=entry^.next^.start-entry^.__end;
 end;
 gpu_map_entry_set_max_free(entry);
 map^.root:=entry;
end;

procedure gpu_map_entry_unlink(
           map  :p_gpu_map;
           entry:p_gpu_map_entry);
var
 next,prev,root:p_gpu_map_entry;
begin
 if (entry<>map^.root) then
 begin
  gpu_map_entry_splay(entry^.start, map^.root);
 end;
 if (entry^.left=nil) then
 begin
  root:=entry^.right;
 end else
 begin
  root:=gpu_map_entry_splay(entry^.start, entry^.left);
  root^.right:=entry^.right;
  if (root^.next=@map^.header) then
  begin
   root^.adj_free:=map^.max_offset-root^.__end;
  end else
  begin
   root^.adj_free:=entry^.next^.start-root^.__end;
  end;
  gpu_map_entry_set_max_free(root);
 end;
 map^.root:=root;

 prev:=entry^.prev;
 next:=entry^.next;
 next^.prev:=prev;
 prev^.next:=next;
 Dec(map^.nentries);
end;

procedure gpu_map_entry_resize_free(map:p_gpu_map;entry:p_gpu_map_entry);
begin
 if (entry<>map^.root) then
 begin
  map^.root:=gpu_map_entry_splay(entry^.start, map^.root);
 end;

 if (entry^.next=@map^.header) then
 begin
  entry^.adj_free:=map^.max_offset-entry^.__end;
 end else
 begin
  entry^.adj_free:=entry^.next^.start-entry^.__end;
 end;
 gpu_map_entry_set_max_free(entry);
end;

function gpu_map_lookup_entry(
           map    :p_gpu_map;
           address:TVkDeviceSize;
           entry  :pp_gpu_map_entry):Boolean;
var
 cur:p_gpu_map_entry;
begin
 cur:=map^.root;
 if (cur=nil) then
 begin
  entry^:=@map^.header;
 end else
 if (address>=cur^.start) and (cur^.__end>address) then
 begin
  entry^:=cur;
  Exit(TRUE);
 end else
 begin

  cur:=gpu_map_entry_splay(address,cur);
  map^.root:=cur;

  if (address>=cur^.start) then
  begin
   entry^:=cur;
   if (cur^.__end>address) then
   begin
    Exit(TRUE);
   end;
  end else
  begin
   entry^:=cur^.prev;
  end;
 end;
 Result:=(FALSE);
end;

function gpu_map_adj_free(map:p_gpu_map;
                          start:TVkDeviceSize;
                          entry:p_gpu_map_entry):TVkDeviceSize;
begin
 if (entry=@map^.header) then
 begin
  if (entry^.next=@map^.header) then
  begin
   Result:=entry^.adj_free - start;
  end else
  begin
   Result:=entry^.next^.start - start;
  end;
 end else
 begin
  Result:=entry^.adj_free - (start - entry^.__end);
 end;
end;

function gpu_map_test(
           map  :p_gpu_map;
           size :PVkDeviceSize;
           start:TVkDeviceSize;
           __end:TVkDeviceSize
          ):Integer;
var
 entry:p_gpu_map_entry;
 _size:TVkDeviceSize;
begin
 size^:=0;

 if (start<map^.min_offset) or (__end>map^.max_offset) or (start>=__end) then
 begin
  Exit(GPU_INVALID_ADDRESS);
 end;

 if gpu_map_lookup_entry(map,start,@entry) then
 begin
  Exit(GPU_NO_SPACE);
 end;

 if (entry^.next<>@map^.header) and
    (entry^.next^.start<__end) then
 begin
  Exit(GPU_NO_SPACE);
 end;

 _size:=gpu_map_adj_free(map,start,entry);

 if (entry^.next<>@map^.header) then
 if (entry^.next^.start<(start + _size)) then
 begin
  //something is wrong
  Exit(GPU_NO_SPACE);
 end;

 if (_size<(__end-start)) then
 begin
  Assert(false,'gpu_map_test');
 end;

 if ((start+_size)>map^.max_offset) then
 begin
  Assert(false,'gpu_map_test');
 end;

 size^:=_size;

 Result:=GPU_SUCCESS;
end;

function gpu_map_insert(
           map  :p_gpu_map;
           start:TVkDeviceSize;
           __end:TVkDeviceSize
          ):Integer;
var
 new_entry :p_gpu_map_entry;
 prev_entry:p_gpu_map_entry;
 temp_entry:p_gpu_map_entry;
begin

 if (start<map^.min_offset) or (__end>map^.max_offset) or (start>=__end) then
 begin
  Exit(GPU_INVALID_ADDRESS);
 end;

 if gpu_map_lookup_entry(map,start,@temp_entry) then
 begin
  Exit(GPU_NO_SPACE);
 end;

 prev_entry:=temp_entry;

 if (prev_entry^.next<>@map^.header) and
    (prev_entry^.next^.start<__end) then
 begin
  Exit(GPU_NO_SPACE);
 end;

 new_entry:=gpu_map_entry_create(map);
 new_entry^.start:=start;
 new_entry^.__end:=__end;

 gpu_map_entry_link(map, prev_entry, new_entry);
 map^.size:=map^.size+(new_entry^.__end - new_entry^.start);

 Result:=GPU_SUCCESS;
end;

function gpu_map_findspace(map   :p_gpu_map;
                           start :TVkDeviceSize;
                           length:TVkDeviceSize;
                           addr  :PVkDeviceSize):Integer;
label
 _nxt;
var
 entry:p_gpu_map_entry;
 st:TVkDeviceSize;
begin

 if (start<map^.min_offset) then
 begin
  start:=map^.min_offset;
 end;
 if (start + length>map^.max_offset) or (start + length<start) then
 begin
  Exit(1);
 end;

 if (map^.root=nil) then
 begin
  addr^:=start;
  Exit(0);
 end;

 map^.root:=gpu_map_entry_splay(start, map^.root);
 if (start + length<=map^.root^.start) then
 begin
  addr^:=start;
  Exit(0);
 end;

 if (start>map^.root^.__end) then
 begin
  st:=start;
 end else
 begin
  st:=map^.root^.__end
 end;

 if (length<=map^.root^.__end + map^.root^.adj_free - st) then
 begin
  addr^:=st;
  Exit(0);
 end;

 entry:=map^.root^.right;

 if (entry=nil) then
 begin
  Exit(1);
 end;

 if (length>entry^.max_free) then
 begin
  Exit(1);
 end;

 while (entry<>nil) do
 begin
  if (entry^.left<>nil) then
  begin
   if not (entry^.left^.max_free>=length) then goto _nxt;
   entry:=entry^.left;
  end else
  begin
   _nxt:
   if (entry^.adj_free>=length) then
   begin
    addr^:=entry^.__end;
    Exit(0);
   end else
   begin
    entry:=entry^.right;
   end;
  end;
 end;

 { Can't get here, so panic if we do. }
 Assert(false,'gpu_map_findspace: max_free corrupt');
end;

type
 t_gpu_find_mode=Set of (fmInsert,fmBestfit);

function gpu_map_find(map   :p_gpu_map;
                      addr  :PVkDeviceSize;
                      size  :PVkDeviceSize;
                      length:TVkDeviceSize;
                      align :TVkDeviceSize;
                      mode  :t_gpu_find_mode
                     ):Integer;
var
 start    :TVkDeviceSize;
 tmp      :TVkDeviceSize;
 save_addr:TVkDeviceSize;
 save_size:TVkDeviceSize;
begin
 if (addr=nil) then
 begin
  start:=0;
 end else
 begin
  start:=addr^;
 end;

 save_addr:=0;
 save_size:=0;

 repeat
  tmp:=0;
  if (gpu_map_findspace(map, start, length, @tmp)<>0) then
  begin
   Break;
  end;

  if ((tmp mod align)<>0) then
  begin
   tmp:=tmp - (tmp mod align);
   tmp:=tmp + align;
  end;

  start:=tmp;

  tmp:=0;
  Result:=gpu_map_test(map, @tmp, start, start + length);

  if (Result=GPU_SUCCESS) then
  begin
   if (fmBestfit in mode) then
   begin
    //save best fit
    if (save_size=0) or
       (save_size>tmp) then
    begin
     save_addr:=start;
     save_size:=tmp;
    end;
   end else
   begin
    //find first
    save_addr:=start;
    save_size:=tmp;
    Break;
   end;
  end;

 until (Result<>GPU_NO_SPACE);

 if (save_size=0) then
 begin
  Result:=GPU_NO_SPACE;
 end else
 if (fmInsert in mode) then
 begin
  Result:=gpu_map_insert(map, start, start + length);
 end else
 begin
  Result:=GPU_SUCCESS;
 end;

 if (addr<>nil) then
 begin
  addr^:=save_addr;
 end;

 if (size<>nil) then
 begin
  size^:=save_size;
 end;
end;

procedure gpu_map_entry_delete(map:p_gpu_map;entry:p_gpu_map_entry);
var
 size:TVkDeviceSize;
begin
 gpu_map_entry_unlink(map, entry);

 size:=entry^.__end - entry^.start;
 map^.size:=map^.size-size;

 gpu_map_entry_dispose(map,entry);
end;

function gpu_map_delete(map:p_gpu_map;start:TVkDeviceSize):Integer;
var
 entry:p_gpu_map_entry;
begin
 if (not gpu_map_lookup_entry(map, start, @entry)) then
 begin
  entry:=entry^.next;
 end;

 if (entry=@map^.header) then
 begin
  Exit(GPU_INVALID_ADDRESS);
 end;

 if (entry^.start<>start) then
 begin
  Exit(GPU_INVALID_ADDRESS);
 end;

 gpu_map_entry_delete(map, entry);

 Result:=GPU_SUCCESS;
end;

function gpu_map_add(map:p_gpu_map;start,length:TVkDeviceSize):Integer;
begin
 Result:=gpu_map_insert(map, start, start + length);
end;

function gpu_map_remove(map:p_gpu_map;start:TVkDeviceSize):Integer;
begin
 Result:=gpu_map_delete(map, start);
end;

procedure gpu_map_remove_all(map:p_gpu_map);
var
 entry,next:p_gpu_map_entry;
begin
 entry:=map^.header.next;

 while (entry<>@map^.header) do
 begin
  next:=entry^.next;

  gpu_map_entry_delete(map, entry);

  entry:=next;
 end;
end;

//


Constructor TvDeviceMemory.Create(Handle:TVkDeviceMemory;Size:TVkDeviceSize;mem_type:Byte;mem_info:PVkMemoryType);
begin
 FHandle:=Handle;
 FSize  :=Size;
 //
 FMemInfo.heap_index     :=mem_info^.heapIndex;
 FMemInfo.mem_type       :=mem_type;
 FMemInfo.device_local   :=(mem_info^.propertyFlags and ord(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT       ))<>0;
 FMemInfo.device_coherent:=(mem_info^.propertyFlags and ord(VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD))<>0;
 FMemInfo.host_visible   :=(mem_info^.propertyFlags and ord(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT       ))<>0;
 FMemInfo.host_coherent  :=(mem_info^.propertyFlags and ord(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT      ))<>0;
 //
 gpu_map_init(@FMap,0,Size);
end;

Destructor TvDeviceMemory.Destroy;
begin
 ReleaseAllDependencies(Self);
 //
 gpu_map_remove_all(@FMap);
 //
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkFreeMemory(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;
 //
 inherited;
end;

Procedure TvDeviceMemory.Flush;
var
 range:TVkMappedMemoryRange;
begin
 if (not FMemInfo.host_coherent) then
 begin
  range:=Default(TVkMappedMemoryRange);
  range.sType :=VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE;
  range.memory:=FHandle;
  range.offset:=0;
  range.size  :=FSize;
  //
  vkFlushMappedMemoryRanges(Device.FHandle,1,@range);
 end;
end;

//

function TvPointer.Acquire:Boolean;
begin
 Result:=False;
 if (FMemory=nil) then Exit;

 //
 rw_rlock(global_mem_lock);
 //

 if (FMemory<>nil) then
 begin
  Result:=FMemory.Acquire(nil);
 end;

 //
 rw_runlock(global_mem_lock);
end;

function TvPointer.Release:Boolean;
begin
 Result:=False;
 if (FMemory=nil) then Exit;

 FMemory.Release(nil);

 Result:=True;
end;

//

function GetHostMappedRequirements:TVkMemoryRequirements;
var
 cinfo:TVkBufferCreateInfo;
 r:TVkResult;
 FHandle:TVkBuffer;

begin
 Result:=Default(TVkMemoryRequirements);

 cinfo:=Default(TVkBufferCreateInfo);
 cinfo.sType      :=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
 cinfo.size       :=64*1024;
 cinfo.usage      :=ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT);
 cinfo.sharingMode:=VK_SHARING_MODE_EXCLUSIVE;

 if limits.VK_EXT_external_memory_host then
 begin
  cinfo.pNext:=@buf_ext;
 end;

 r:=vkCreateBuffer(Device.FHandle,@cinfo,nil,@FHandle);
 if (r=VK_SUCCESS) then
 begin
  vkGetBufferMemoryRequirements(Device.FHandle,FHandle,@Result);
  vkDestroyBuffer(Device.FHandle,FHandle,nil);
 end;
end;

function GetSparceMemoryTypes:TVkUInt32;
var
 cinfo:TVkBufferCreateInfo;
 mr:TVkMemoryRequirements;
 r:TVkResult;
 FHandle:TVkBuffer;
begin
 Result:=0;
 if not sparseBinding then Exit;

 mr:=Default(TVkMemoryRequirements);

 cinfo:=Default(TVkBufferCreateInfo);
 cinfo.sType      :=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
 cinfo.flags      :=ord(VK_BUFFER_CREATE_SPARSE_BINDING_BIT);
 cinfo.size       :=64*1024;
 cinfo.usage      :=ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT);
 cinfo.sharingMode:=VK_SHARING_MODE_EXCLUSIVE;
 cinfo.pNext      :=@buf_ext;

 r:=vkCreateBuffer(Device.FHandle,@cinfo,nil,@FHandle);
 if (r=VK_SUCCESS) then
 begin
  vkGetBufferMemoryRequirements(Device.FHandle,FHandle,@mr);
  vkDestroyBuffer(Device.FHandle,FHandle,nil);
  Result:=mr.memoryTypeBits;
 end;
end;

Constructor TvMemManager.Create;
var
 mr:TVkMemoryRequirements;
 s:RawByteString;
 i:Byte;
begin
 mr:=GetHostMappedRequirements;

 Writeln('[HostMappedRequirements]');
 Writeln('  Alignment=',mr.alignment);

 s:='';
 For i:=0 to VK_MAX_MEMORY_TYPES-1 do
 if ((1 shl i) and (mr.memoryTypeBits))<>0 then
 begin
  if (s='') then
  begin
   s:=IntToStr(i);
  end else
  begin
   s:=s+','+IntToStr(i);
  end;
 end;
 Writeln('  MemoryType=',S);

 FSparceMemoryTypes:=GetSparceMemoryTypes;

 s:='';
 For i:=0 to VK_MAX_MEMORY_TYPES-1 do
 if ((1 shl i) and (FSparceMemoryTypes))<>0 then
 begin
  if (s='') then
  begin
   s:=IntToStr(i);
  end else
  begin
   s:=s+','+IntToStr(i);
  end;
 end;
 Writeln('  SparceType=',s);

 FProperties:=Default(TVkPhysicalDeviceMemoryProperties);
 vkGetPhysicalDeviceMemoryProperties(VulkanApp.FPhysicalDevice,@FProperties);

 LoadMemoryHeaps;

 PrintMemoryHeaps;

 TAILQ_INIT(@FDevs );
 TAILQ_INIT(@FHosts);
end;

function TvMemManager.findMemoryType(Filter:TVkUInt32;prop:TVkMemoryPropertyFlags;start:Integer):Integer;
var
 i:Integer;
begin
 Result:=-1;
 if (start<0) or (start>=FProperties.memoryTypeCount) then Exit;
 For i:=start to FProperties.memoryTypeCount-1 do
 begin
  if  ((Filter and (1 shl i))<>0) and ((FProperties.memoryTypes[i].propertyFlags and prop)=prop) then
  begin
   Exit(i);
  end;
 end;
end;

procedure TvMemManager.LoadMemoryHeaps;
var
 i:TVkUInt32;
 mtype:Integer;

 function get_host_visible(heapIndex:TVkUInt32):Boolean; inline;
 var
  i:TVkUInt32;
 begin
  Result:=False;
  For i:=0 to FProperties.memoryTypeCount-1 do
  if (FProperties.memoryTypes[i].heapIndex=heapIndex) then
  if ((FProperties.memoryTypes[i].propertyFlags and
       ord(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))<>0) then

  begin
   Exit(True);
  end;
 end;

 function get_mem_type(heapIndex:TVkUInt32;
                       device_local,
                       host_visible,
                       device_coherent,
                       host_coherent:Boolean):Integer; inline;
 var
  i,mask:TVkUInt32;
 begin
  Result:=-1;

  mask:=(ord(device_local   )*ord(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)) or
        (ord(host_visible   )*ord(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)) or
        (ord(device_coherent)*ord(VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD)) or
        (ord(host_coherent  )*ord(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT));

  For i:=0 to FProperties.memoryTypeCount-1 do
  if (FProperties.memoryTypes[i].heapIndex=heapIndex) then
  if ((FProperties.memoryTypes[i].propertyFlags and mask)=mask) then
  begin
   Exit(i);
  end;
 end;

begin
 SetLength(FHeaps,FProperties.memoryHeapCount);

 if (FProperties.memoryHeapCount<>0) then
 For i:=0 to FProperties.memoryHeapCount-1 do
 begin
  FHeaps[i].heap_index  :=i;
  FHeaps[i].heap_size   :=FProperties.memoryHeaps[i].size;
  FHeaps[i].device_local:=(FProperties.memoryHeaps[i].flags and
                           ord(VK_MEMORY_HEAP_DEVICE_LOCAL_BIT))<>0;
  FHeaps[i].host_visible:=get_host_visible(i);

  FHeaps[i].device_coherent:=FHeaps[i].device_local;
  FHeaps[i].host_coherent  :=FHeaps[i].host_visible;

  repeat
   mtype:=get_mem_type(i,
                       FHeaps[i].device_local,
                       FHeaps[i].host_visible,
                       FHeaps[i].device_coherent,
                       FHeaps[i].host_coherent);

   if (mtype=-1) then
   begin
    if FHeaps[i].device_coherent then
    begin
     FHeaps[i].device_coherent:=False;
    end else
    if FHeaps[i].host_coherent then
    begin
     FHeaps[i].host_coherent:=False;
    end else
    begin
     Assert(false,'load memory type');
    end;

   end else
   begin
    Break;
   end;

  until false;

  FHeaps[i].def_mem_type:=mtype;
 end;

end;

procedure TvMemManager.PrintMemoryHeaps;
var
 i:TVkUInt32;

 function get_flags_str(flags:TVkUInt32):RawByteString; inline;
 begin
  Result:='';
  if (flags and ord(VK_MEMORY_HEAP_DEVICE_LOCAL_BIT))<>0 then
  begin
   Result:='DEVICE_LOCAL';
  end else
  begin
   Result:='HOST_LOCAL';
  end;
  if (flags and ord(VK_MEMORY_HEAP_MULTI_INSTANCE_BIT))<>0 then
  begin
   if (Result<>'') then Result:=Result+',';
   Result:=Result+'MULTI_INSTANCE';
  end;
 end;

 function get_types_str(heapIndex:TVkUInt32):RawByteString; inline;
 var
  i:TVkUInt32;
 begin
  Result:='';
  For i:=0 to FProperties.memoryTypeCount-1 do
  if (FProperties.memoryTypes[i].heapIndex=heapIndex) then
  begin
   if (Result<>'') then Result:=Result+',';
   Result:=Result+IntToStr(i);
  end;
 end;

begin
 For i:=0 to FProperties.memoryHeapCount-1 do
 begin
  Writeln('[Heap]:',i);
  Writeln(' size =0x',HexStr(FProperties.memoryHeaps[i].size,16));
  Writeln(' flags=',get_flags_str(FProperties.memoryHeaps[i].flags));
  Writeln(' types=',get_types_str(i));

 end;

 //

 if Length(FHeaps)<>0 then
 For i:=0 to High(FHeaps) do
 begin
  Writeln('[Heap]:',i);
  Writeln(' heap_size      =0x',HexStr(FHeaps[i].heap_size,16));
  Writeln(' heap_id        =',FHeaps[i].heap_index);
  Writeln(' def_mem_type   =',FHeaps[i].def_mem_type);
  Writeln(' device_local   =',FHeaps[i].device_local);
  Writeln(' device_coherent=',FHeaps[i].device_coherent);
  Writeln(' host_visible   =',FHeaps[i].host_visible);
  Writeln(' host_coherent  =',FHeaps[i].host_coherent);
 end;
end;

procedure TvMemManager.PrintMemoryType(typeFilter:TVkUInt32);
var
 s:RawByteString;
 i:TVkUInt32;

 procedure append(TestFlag:TVkFlags;const name:RawByteString); inline;
 begin
  if ((FProperties.memoryTypes[i].propertyFlags and TestFlag)<>0) then
  begin
   if (s='') then
   begin
    s:=s+name;
   end else
   begin
    s:=s+'|'+name;
   end;
  end;
 end;

begin

 For i:=0 to FProperties.memoryTypeCount-1 do
 begin
  if  ((typeFilter and (1 shl i))<>0) then
  begin
   s:='';

   append(ord(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT       ),'DEVICE_LOCAL');
   append(ord(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT       ),'HOST_VISIBLE');
   append(ord(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT      ),'HOST_COHERENT');
   append(ord(VK_MEMORY_PROPERTY_HOST_CACHED_BIT        ),'HOST_CACHED');
   append(ord(VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT   ),'LAZILY_ALLOCATED');
   append(ord(VK_MEMORY_PROPERTY_PROTECTED_BIT          ),'PROTECTED');
   append(ord(VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD),'DEVICE_COHERENT_AMD');
   append(ord(VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD),'DEVICE_UNCACHED_AMD');
   append(ord(VK_MEMORY_PROPERTY_RDMA_CAPABLE_BIT_NV    ),'RDMA_CAPABLE_NV');

   Write(i,':',HexStr(FProperties.memoryTypes[i].propertyFlags,8),':',s);
  end;
 end;

end;

Function TvMemManager._AllocDevBlock(Size:TVkDeviceSize;mtindex:Byte):TvDeviceMemory;
label
 _retry;
var
 FHandle:TVkDeviceMemory;
 node:TvDeviceMemory;
 tmp:TVkDeviceSize;
begin
 Result:=nil;

 node:=nil;

 if (FBacked<>nil) then
 begin
  if (FBacked.FSize>=Size) and
     (FBacked.FMemInfo.mem_type=mtindex) then
  begin
   node:=FBacked;
   FBacked:=nil;
  end;
 end;

 if (node=nil) then
 begin
  _retry:

  FHandle:=vkAllocMemory(Device.FHandle,Size,mtindex);

  if (FHandle=VK_NULL_HANDLE) then
  begin

   //try shrink
   tmp:=_shrink(Size,mtindex,0);

   if (tmp>0) then
   begin
    //fail
    Exit;
   end else
   begin
    goto _retry;
   end;

  end;

  //
  node:=TvDeviceMemory.Create(FHandle,Size,mtindex,@FProperties.memoryTypes[mtindex]);

  node.Acquire(nil); //list
 end;

 TAILQ_INSERT_HEAD(@FDevs,node,@node.entry);

 //
 Result:=node;
end;

procedure TvMemManager._FreeDevBlock(node:TvDeviceMemory);
begin
 if (node=nil) then Exit;

 TAILQ_REMOVE(@FDevs,node,@node.entry);

 if (FBacked<>nil) then
 begin
  if (node.FSize>FBacked.FSize) then
  begin
   ReleaseAndNil(FBacked); //free old
   FBacked:=node;          //set new
  end else
  begin
   ReleaseAndNil(node); //free new
  end;
 end else
 begin
  FBacked:=node; //set new
 end;

end;

//GRANULAR_DEV_BLOCK_SIZE

Function TvMemManager.FetchMemory(const mr:TVkMemoryRequirements;prop:TVkMemoryPropertyFlags):TvPointer;
var
 i:Byte;
 memoryTypeBits:TVkUInt32;
 best_fit      :Boolean;
begin
 Result:=Default(TvPointer);

 memoryTypeBits:=mr.memoryTypeBits;

 best_fit:=(prop and V_PROP_BEST_FIT)<>0;
 prop    :=prop and (not V_PROP_BEST_FIT);

 //filter by prop
 For i:=0 to VK_MAX_MEMORY_TYPES-1 do
 if (((1 shl i) and memoryTypeBits)<>0) then
 begin
  //if not contain flags
  if ((FProperties.memoryTypes[i].propertyFlags and prop)<>prop) then
  begin
   //exclude
   memoryTypeBits:=memoryTypeBits and (not (1 shl i));
  end;
 end;

 Result:=FetchMemory(mr.size,mr.alignment,memoryTypeBits,best_fit);
end;

Function TvMemManager.FetchMemory(Size          :TVkDeviceSize;
                                  Align         :TVkDeviceSize;
                                  memoryTypeBits:TVkUInt32;
                                  best_fit      :Boolean):TvPointer;
label
 _repeat;
var
 node:TvDeviceMemory;

 _addr:TVkDeviceSize;
 _size:TVkDeviceSize;

 save_addr:TVkDeviceSize;
 save_size:TVkDeviceSize;
 save_node:TvDeviceMemory;

 mem_type:Integer;

 i:Byte;

begin
 Result:=Default(TvPointer);
 if (Size=0) or (memoryTypeBits=0) then Exit;
 //
 if (Align=0) then Align:=1;
 //
 save_addr:=0;
 save_size:=0;
 save_node:=nil;
 //
 rw_wlock(global_mem_lock);
 //

 _repeat:

 node:=TvDeviceMemory(TAILQ_FIRST(@FDevs));

 while (node<>nil) do
 begin
  _addr:=0;
  _size:=0;

  if (((1 shl node.FMemInfo.mem_type) and memoryTypeBits)<>0) then
  begin
   if best_fit then
   begin
    if gpu_map_find(@node.FMap,
                    @_addr,
                    @_size,
                    Size,
                    Align,
                    [fmBestfit]
                   )=GPU_SUCCESS then
    begin
     //save best fit
     if (save_size=0) or
        (save_size>_size) then
     begin
      save_addr:=_addr;
      save_size:=_size;
      save_node:=node;
     end;
    end;
   end else
   begin
    if gpu_map_find(@node.FMap,
                    @_addr,
                    @_size,
                    Size,
                    Align,
                    [fmInsert]
                   )=GPU_SUCCESS then
    begin
     save_addr:=_addr;
     save_size:=_size;
     save_node:=node;
     Break;
    end;
   end;
  end;

  node:=TvDeviceMemory(TAILQ_NEXT(node,@node.entry));
 end;

 if (save_size=0) then
 begin
  mem_type:=-1;

  //find by default memtypes
  For i:=0 to High(FHeaps) do
  if (((1 shl FHeaps[i].def_mem_type) and memoryTypeBits)<>0) then
  begin
   mem_type:=FHeaps[i].def_mem_type;
   Break;
  end;

  if (mem_type=-1) then
  begin
   //find first bit
   mem_type:=BsfByte(memoryTypeBits);
  end;

  save_node:=_AllocDevBlock(AlignUp(Size,GRANULAR_DEV_BLOCK_SIZE),mem_type);

  if (save_node<>nil) then
  begin
   if gpu_map_add(@save_node.FMap,0,Size)=GPU_SUCCESS then
   begin
    Result.FMemory:=save_node;
    Result.FOffset:=0;
   end else
   begin
    //wtf?
    goto _repeat;
   end;
  end;

 end else
 if best_fit then
 begin
  if gpu_map_add(@save_node.FMap,save_addr,save_size)=GPU_SUCCESS then
  begin
   Result.FMemory:=save_node;
   Result.FOffset:=save_addr;
  end else
  begin
   //Something has changed
   goto _repeat;
  end;
 end else
 begin
  //Already inserted
  Result.FMemory:=save_node;
  Result.FOffset:=save_addr;
 end;
 //
 if (Result.FMemory<>nil) then
 begin
  Result.FMemory.Acquire(nil); //fetch ref
 end;
 //
 rw_wunlock(global_mem_lock);
end;

Function TvMemManager.FreeMemory(P:TvPointer):Boolean;
begin
 Result:=False;
 if (P.FMemory=nil) then Exit;
 if (P.FMemory.ClassType<>TvDeviceMemory) then Exit;
 //
 rw_wlock(global_mem_lock);
 //
 if (gpu_map_remove(@P.FMemory.FMap,P.FOffset)=GPU_SUCCESS) then
 begin
  Result:=True;
  //
  if (P.FMemory.FMap.size=0) then //is free
  begin
   _FreeDevBlock(P.FMemory);
  end;
 end;
 //
 rw_wunlock(global_mem_lock);
end;

Function TvMemManager._shrink_dev_block(max:TVkDeviceSize;heap_index:Byte):TVkDeviceSize;
var
 node,next:TvDeviceMemory;
begin
 Result:=0;

 if (FBacked<>nil) then
 begin
  Result:=Result+FBacked.FSize;
  ReleaseAndNil(FBacked);
  if (Result>=max) then Exit;
 end;

 node:=TvDeviceMemory(TAILQ_FIRST(@FDevs));

 while (node<>nil) do
 begin
  next:=TvDeviceMemory(TAILQ_NEXT(node,@node.entry));

  if (node.FMemInfo.heap_index=heap_index) then
  if (node.FRefs<=1) then
  begin
   Result:=Result+node.FSize;
   //
   TAILQ_REMOVE(@FDevs,node,@node.entry);
   ReleaseAndNil(node); //list
   //
   if (Result>=max) then Break;
  end;

  node:=next;
 end;

end;

Function TvMemManager._shrink_host_map(max:TVkDeviceSize;heap_index:Byte):TVkDeviceSize;
var
 node,prev:TvHostMemory;
begin
 Result:=0;

 node:=TvHostMemory(TAILQ_LAST(@FHosts));
 while (node<>nil) do
 begin
  prev:=TvHostMemory(TAILQ_PREV(node,@node.entry));

  if (node.FMemInfo.heap_index=heap_index) then
  if (node.FRefs<=1) then
  begin
   Result:=Result+node.FSize;
   //
   TAILQ_REMOVE(@FHosts,node,@node.entry);
   ReleaseAndNil(node); //list
   //
   if (Result>=max) then Break;
  end;

  node:=prev;
 end;

end;

Function TvMemManager._shrink(max:TVkDeviceSize;mtindex:Byte;mode:Byte):TVkDeviceSize;
var
 tmp:QWORD;
 heap_index:Byte;
begin
 heap_index:=FProperties.memoryTypes[mtindex].heapIndex;

 if (mode=0) then
 begin
  //dev->host
  tmp:=_shrink_dev_block(max,heap_index);
 end else
 begin
  //host->dev
  tmp:=_shrink_host_map(max,heap_index);
 end;

 if (tmp<max) then
 begin
  max:=max-tmp;
 end else
 begin
  max:=0;
 end;

 if (max>0) then
 begin
  //try shrink 2

  if (mode=0) then
  begin
   //dev->host
   tmp:=_shrink_host_map(max,heap_index);
  end else
  begin
   //host->dev
   tmp:=_shrink_dev_block(max,heap_index);
  end;

  if (tmp<max) then
  begin
   max:=max-tmp;
  end else
  begin
   max:=0;
  end;
 end;

 Result:=max;
end;

procedure TvMemManager.unmap_host(start,__end:QWORD);
label
 _full;
var
 node,next:TvHostMemory;
begin
 if (start=__end) then Exit;

 //
 rw_wlock(global_mem_lock);
 //

 node:=TvHostMemory(TAILQ_FIRST(@FHosts));
 while (node<>nil) do
 begin
  next:=TvHostMemory(TAILQ_NEXT(node,@node.entry));

  if (__end>node.FStart) and (start<node.F__End) then
  begin

   if (start<=node.FStart) and (__end>=node.F__End) then
   begin
    //full in
    _full:
    TAILQ_REMOVE(@FHosts,node,@node.entry);
    ReleaseAndNil(node); //list
   end else
   if rmem_map_test_lock(node.FStart,node.F__End) then
   begin
    goto _full;
   end else
   if (node.FRefs<=1) then
   begin
    //partial
    TAILQ_REMOVE(@FHosts,node,@node.entry);
    ReleaseAndNil(node); //list
   end;

  end;

  node:=next;
 end;

 //
 rw_wunlock(global_mem_lock);
end;

Function TvMemManager.FetchHostMap(Addr,Size:TVkDeviceSize;mtindex:Byte):TvPointer;
label
 _retry,
 _fail;
var
 FStart:QWORD;
 F__End:QWORD;
 FStart_align:QWORD;
 F__End_align:QWORD;
 tmp:QWORD;
 node:TvHostMemory;
 FHandle:TVkDeviceMemory;
begin
 Result:=Default(TvPointer);
 if (Addr=0) or (Size=0) then Exit;
 //
 FStart:=QWORD(Addr);
 F__End:=FStart+Size;
 //
 rw_wlock(global_mem_lock);
 //

 node:=TvHostMemory(TAILQ_FIRST(@FHosts));
 while (node<>nil) do
 begin

  if (FStart>=node.FStart) and
     (F__End<=node.F__End) then
  begin
   Break;
  end;

  node:=TvHostMemory(TAILQ_NEXT(node,@node.entry));
 end;

 if (node=nil) then
 begin
  FStart_align:=Max(AlignDw(FStart,GRANULAR_MAP_BLOCK_SIZE),VM_MIN_GPU_ADDRESS);
  F__End_align:=Min(AlignUp(F__End,GRANULAR_MAP_BLOCK_SIZE),VM_MAX_GPU_ADDRESS);

  _retry:

  tmp:=F__End_align-FStart_align;

  FHandle:=vkAllocHostMemory(Device.FHandle,tmp,mtindex,Pointer(FStart_align));

  if (FHandle=VK_NULL_HANDLE) then
  begin

   //try shrink
   tmp:=_shrink(tmp,mtindex,1);

   if (tmp>0) then
   begin
    node:=nil;
    goto _fail;
   end else
   begin
    goto _retry;
   end;

  end;

  node:=TvHostMemory.Create(FHandle,tmp,mtindex,@FProperties.memoryTypes[mtindex]);

  node.FStart:=FStart_align;
  node.F__End:=F__End_align;

  node.Acquire(nil); //map ref
  TAILQ_INSERT_HEAD(@FHosts,node,@node.entry);
 end;

 node.Acquire(nil);

 _fail:

 //
 rw_wunlock(global_mem_lock);
 //

 if (node<>nil) then
 begin
  Result.FMemory:=TvDeviceMemory(node);
  Result.FOffset:=Addr-node.FStart;

  if ((Result.FOffset+Size)>node.FSize) then
  begin
   Assert(false);
  end;

 end;
end;

Function TvMemManager.FetchHostMap(Addr,Size:TVkDeviceSize;device_local:Boolean):TvPointer;
var
 i:Byte;
begin
 Result:=Default(TvPointer);

 Assert(Length(FHeaps)<>0);

 For i:=0 to High(FHeaps) do
 if (FHeaps[i].host_visible) then
 if (FHeaps[i].device_local=device_local) then
 begin
  Exit(FetchHostMap(Addr,Size,FHeaps[i].def_mem_type));
 end;

end;

Procedure TvMemManager.Flush;
var
 node:TvHostMemory;
begin
 if (Self=nil) then Exit;

 rw_wlock(global_mem_lock);
 //

 node:=TvHostMemory(TAILQ_FIRST(@FHosts));
 while (node<>nil) do
 begin

  node.Flush;

  node:=TvHostMemory(TAILQ_NEXT(node,@node.entry));
 end;

 //
 rw_wunlock(global_mem_lock);
end;

//

function vkAllocMemory(device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32):TVkDeviceMemory;
var
 ainfo:TVkMemoryAllocateInfo;
 r:TVkResult;
begin
 ainfo:=Default(TVkMemoryAllocateInfo);
 ainfo.sType          :=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
 ainfo.allocationSize :=Size;
 ainfo.memoryTypeIndex:=mtindex;
 //
 Result:=VK_NULL_HANDLE;
 r:=vkAllocateMemory(device,@ainfo,nil,@Result);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkAllocateMemory:',r);
 end;
end;

function vkAllocHostMemory(device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32;addr:Pointer):TVkDeviceMemory;
var
 ainfo:TVkMemoryAllocateInfo;
 import:TVkImportMemoryHostPointerInfoEXT;
 r:TVkResult;
begin
 ainfo:=Default(TVkMemoryAllocateInfo);
 ainfo.sType          :=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
 ainfo.allocationSize :=Size;
 ainfo.memoryTypeIndex:=mtindex;
 ainfo.pNext:=@import;
 //
 import:=Default(TVkImportMemoryHostPointerInfoEXT);
 import.sType       :=VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT;
 import.handleType  :=VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT;
 import.pHostPointer:=addr;
 //
 Result:=VK_NULL_HANDLE;
 r:=vkAllocateMemory(device,@ainfo,nil,@Result);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkAllocateMemory:',r);
 end;
end;

function vkAllocDedicatedImage(device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32;FHandle:TVkImage):TVkDeviceMemory;
var
 ainfo:TVkMemoryAllocateInfo;
 dinfo:TVkMemoryDedicatedAllocateInfo;
 r:TVkResult;
begin
 ainfo:=Default(TVkMemoryAllocateInfo);
 ainfo.sType          :=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
 ainfo.allocationSize :=Size;
 ainfo.memoryTypeIndex:=mtindex;
 ainfo.pNext:=@dinfo;
 dinfo:=Default(TVkMemoryDedicatedAllocateInfo);
 dinfo.sType:=VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO;
 dinfo.image:=FHandle;
 Result:=VK_NULL_HANDLE;
 r:=vkAllocateMemory(device,@ainfo,nil,@Result);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkAllocateMemory:',r);
 end;
end;

function vkAllocDedicatedBuffer(device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32;FHandle:TVkBuffer):TVkDeviceMemory;
var
 ainfo:TVkMemoryAllocateInfo;
 dinfo:TVkMemoryDedicatedAllocateInfo;
 r:TVkResult;
begin
 ainfo:=Default(TVkMemoryAllocateInfo);
 ainfo.sType          :=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
 ainfo.allocationSize :=Size;
 ainfo.memoryTypeIndex:=mtindex;
 ainfo.pNext:=@dinfo;
 dinfo:=Default(TVkMemoryDedicatedAllocateInfo);
 dinfo.sType:=VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO;
 dinfo.buffer:=FHandle;
 Result:=VK_NULL_HANDLE;
 r:=vkAllocateMemory(device,@ainfo,nil,@Result);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkAllocateMemory:',r);
 end;
end;

end.

