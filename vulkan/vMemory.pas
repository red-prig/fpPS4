unit vMemory;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 mqueue,
 g23tree,
 Vulkan,
 vDevice,
 vDependence;

type
 TvMemInfo=packed record
  heap_index     :Byte;
  mem_type       :Byte;
  device_local   :Boolean;
  device_coherent:Boolean;
  host_visible   :Boolean;
  host_coherent  :Boolean;
 end;

 TvDeviceMemory=class(TvDependenciesObject)
  FHandle :TVkDeviceMemory;
  FSize   :TVkDeviceSize;
  FMemInfo:TvMemInfo;
  //
  Constructor Create(Handle:TVkDeviceMemory;Size:TVkDeviceSize;mem_type:Byte;mem_info:PVkMemoryType);
  Destructor  Destroy; override;
 end;

 TvHostMemory=class(TvDeviceMemory)
  entry:TAILQ_ENTRY;
  //
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
 TDevNode=packed record
  FSize   :TVkDeviceSize;
  FOffset :TVkDeviceSize;
  FBlockId:Word;
  FmType  :Byte;
  Fisfree :Boolean;
 end;

 //free:  [FmType]|[FSize]|[FBlockId]
 //alloc: [FBlockId]|[FOffset]

 TFreeCompare=object
  function c(const a,b:TDevNode):Integer; static;
 end;

 TAllcCompare=object
  function c(const a,b:TDevNode):Integer; static;
 end;

 TFreeDevNodeSet=specialize T23treeSet<TDevNode,TFreeCompare>;
 TAllcDevNodeSet=specialize T23treeSet<TDevNode,TAllcCompare>;

 PvHeap=^TvHeap;
 TvHeap=packed record
  heap_size      :TVkDeviceSize;
  heap_index     :Byte;
  def_mem_type   :Byte;
  device_local   :Boolean;
  device_coherent:Boolean;
  host_visible   :Boolean;
  host_coherent  :Boolean;
 end;

 TvMemManager=class
  public
   FProperties:TVkPhysicalDeviceMemoryProperties;

   FSparceMemoryTypes:TVkUInt32;

   FHeaps:array of TvHeap;

   FDevBlocks:array of TvDeviceMemory;

   FBacked:TvDeviceMemory;

  private
   FFreeSet:TFreeDevNodeSet;
   FAllcSet:TAllcDevNodeSet;

   FHosts:TAILQ_HEAD; //TvHostMemory

  public

   Constructor Create;

   function    findMemoryType(Filter:TVkUInt32;prop:TVkMemoryPropertyFlags;start:Integer):Integer;
   procedure   LoadMemoryHeaps;
   procedure   PrintMemoryHeaps;
   procedure   PrintMemoryType(typeFilter:TVkUInt32);

  private

   Function    _AllcDevBlock(Size:TVkDeviceSize;mtindex:Byte;Var R:Word):Boolean;
   Function    _FreeDevBlock(i:Word):Boolean;
   Function    _FindDevBlock(Memory:TvDeviceMemory;Var R:Word):Boolean;
   Function    _FetchFree_a(Size,Align:TVkDeviceSize;mtindex:Byte;var R:TDevNode):Boolean;
   Function    _FetchFree_l(key:TDevNode;var R:TDevNode):Boolean;
   Function    _FetchFree_b(key:TDevNode;var R:TDevNode):Boolean;
   Function    _FetchAllc(FOffset:TVkDeviceSize;FBlockId:Word;var R:TDevNode):Boolean;

  public

   Function    Alloc(const mr:TVkMemoryRequirements;pr:TVkMemoryPropertyFlags):TvPointer;
   Function    Alloc(Size,Align:TVkDeviceSize;mtindex:Byte;test_free:Boolean):TvPointer;
   Function    Free(P:TvPointer):Boolean;

  private

   Function    _shrink_dev_block(max:TVkDeviceSize;heap_index:Byte):TVkDeviceSize;
   Function    _shrink_host_map(max:TVkDeviceSize):TVkDeviceSize;

  public

   procedure   unmap_host(start,__end:QWORD);

   Function    FetchHostMap(Addr,Size:TVkDeviceSize;mtindex:Byte):TvPointer;
   Function    FetchHostMap(Addr,Size:TVkDeviceSize;device_local:Boolean):TvPointer;
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
end;

Destructor TvDeviceMemory.Destroy;
begin
 ReleaseAllDependencies(Self);
 //
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkFreeMemory(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;
 //
 inherited;
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

//free:  [FmType]|[FSize]|[FBlockId]
function TFreeCompare.c(const a,b:TDevNode):Integer;
begin
 //1 FmType
 Result:=Integer(a.FmType>b.FmType)-Integer(a.FmType<b.FmType);
 if (Result<>0) then Exit;
 //2 FSize
 Result:=Integer(a.FSize>b.FSize)-Integer(a.FSize<b.FSize);
 if (Result<>0) then Exit;
 //3 FBlockId
 Result:=Integer(a.FBlockId>b.FBlockId)-Integer(a.FBlockId<b.FBlockId);
end;

//alloc: [FBlockId]|[FOffset]
function TAllcCompare.c(const a,b:TDevNode):Integer;
begin
 //1 FBlockId
 Result:=Integer(a.FBlockId>b.FBlockId)-Integer(a.FBlockId<b.FBlockId);
 if (Result<>0) then Exit;
 //2 FOffset
 Result:=Integer(a.FOffset>b.FOffset)-Integer(a.FOffset<b.FOffset);
end;

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
 For i:=0 to 31 do
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
 For i:=0 to 31 do
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

Function TvMemManager._AllcDevBlock(Size:TVkDeviceSize;mtindex:Byte;Var R:Word):Boolean;
label
 _set;
var
 FHandle:TVkDeviceMemory;
 FDeviceMemory:TvDeviceMemory;
 i:Word;
begin
 Result:=False;

 FDeviceMemory:=nil;

 if (FBacked<>nil) then
 begin
  if (FBacked.FSize>=Size) and
     (FBacked.FMemInfo.mem_type=mtindex) then
  begin
   FDeviceMemory:=FBacked;
   FBacked:=nil;
  end;
 end;

 if (FDeviceMemory=nil) then
 begin
  FHandle:=vkAllocMemory(Device.FHandle,Size,mtindex);
  if (FHandle=VK_NULL_HANDLE) then Exit;
  //
  FDeviceMemory:=TvDeviceMemory.Create(FHandle,Size,mtindex,@FProperties.memoryTypes[mtindex]);
 end;


 //
 if Length(FDevBlocks)<>0 then
 For i:=0 to High(FDevBlocks) do
 begin
  if (FDevBlocks[i]=nil) then
  begin
   goto _set;
  end;
 end;
 //
 i:=Length(FDevBlocks);
 SetLength(FDevBlocks,i+1);
 //
 _set:
 //
 FDeviceMemory.Acquire(nil);
 FDevBlocks[i]:=FDeviceMemory;
 //
 R:=i;
 Result:=True;
end;

Function TvMemManager._FreeDevBlock(i:Word):Boolean;
var
 c:Word;
begin
 Result:=False;
 if (i>=Length(FDevBlocks)) then Exit;
 if (FDevBlocks[i]=nil) then Exit;

 if (FBacked<>nil) then
 begin
  if (FDevBlocks[i].FSize>FBacked.FSize) then
  begin
   ReleaseAndNil(FBacked);
   FBacked:=FDevBlocks[i];
   FDevBlocks[i]:=nil;
  end else
  begin
   ReleaseAndNil(FDevBlocks[i]);
  end;
 end else
 begin
  FBacked:=FDevBlocks[i];
  FDevBlocks[i]:=nil;
 end;

 //
 Result:=True;
 //shrink
 c:=Length(FDevBlocks);
 While (c<>0) do
 begin
  if (FDevBlocks[c-1]=nil) then
   Dec(c)
  else
   Break;
 end;
 SetLength(FDevBlocks,c);
end;

Function TvMemManager._FindDevBlock(Memory:TvDeviceMemory;Var R:Word):Boolean;
var
 i:Word;
begin
 Result:=False;
 if Length(FDevBlocks)<>0 then
 For i:=0 to High(FDevBlocks) do
  if (FDevBlocks[i]=Memory) then
  begin
   R:=i;
   Exit(True);
  end;
end;

//free:  [FmType]|[FSize]|[FBlockId]
Function TvMemManager._FetchFree_a(Size,Align:TVkDeviceSize;mtindex:Byte;var R:TDevNode):Boolean;
var
 It:TFreeDevNodeSet.Iterator;
 key:TDevNode;
 Offset:TVkDeviceSize;
begin
 Result:=false;
 key:=Default(TDevNode);
 key.FmType:=mtindex;
 key.FSize:=Size;
 It:=FFreeSet.find_be(key);
 if (It.Item=nil) then Exit;
 repeat
  key:=It.Item^;
  if (key.FmType<>mtindex) then Exit;
  Offset:=System.Align(key.FOffset,Align);
  if (Offset+Size)<=(key.FOffset+key.FSize) then
  begin
   R:=key;
   FAllcSet.delete(key);
   FFreeSet.erase(It);
   Exit(True);
  end;
 until not It.Next;
end;

//alloc: [FBlockId]|[FOffset]
Function TvMemManager._FetchFree_l(key:TDevNode;var R:TDevNode):Boolean;
var
 It:TAllcDevNodeSet.Iterator;
 key2:TDevNode;
begin
 Result:=false;
 It:=FAllcSet.find_le(key);
 if (It.Item=nil) then Exit;
 key2:=It.Item^;
 if (key2.FBlockId<>key.FBlockId) or
    (key2.FmType  <>key.FmType)   or
    (not key2.Fisfree) then Exit;
 R:=key2;
 FAllcSet.erase(It);
 FFreeSet.delete(key2);
 Result:=True;
end;

//alloc: [FBlockId]|[FOffset]
Function TvMemManager._FetchFree_b(key:TDevNode;var R:TDevNode):Boolean;
var
 It:TAllcDevNodeSet.Iterator;
 key2:TDevNode;
begin
 Result:=false;
 It:=FAllcSet.find_be(key);
 if (It.Item=nil) then Exit;
 key2:=It.Item^;
 if (key2.FBlockId<>key.FBlockId) or
    (key2.FmType  <>key.FmType)   or
    (not key2.Fisfree) then Exit;
 R:=key2;
 FAllcSet.erase(It);
 FFreeSet.delete(key2);
 Result:=True;
end;

//alloc: [FBlockId]|[FOffset]
Function TvMemManager._FetchAllc(FOffset:TVkDeviceSize;FBlockId:Word;var R:TDevNode):Boolean;
var
 It:TAllcDevNodeSet.Iterator;
 key:TDevNode;
begin
 Result:=False;
 key:=Default(TDevNode);
 key.FOffset :=FOffset;
 key.FBlockId:=FBlockId;
 It:=FAllcSet.find(key);
 if (It.Item=nil) then Exit;
 key:=It.Item^;
 if key.Fisfree then Exit;
 R:=key;
 FAllcSet.erase(It);
 Result:=True;
end;

//GRANULAR_DEV_BLOCK_SIZE

Function TvMemManager.Alloc(const mr:TVkMemoryRequirements;pr:TVkMemoryPropertyFlags):TvPointer;
var
 mt:Integer;
begin
 Result:=Default(TvPointer);
 mt:=-1;

 repeat

  mt:=findMemoryType(mr.memoryTypeBits,pr,mt+1);
  if (mt=-1) then Break;

  Result:=Alloc(mr.size,mr.alignment,mt,True);
  if (Result.FMemory<>nil) then Exit;

 until false;

 mt:=findMemoryType(mr.memoryTypeBits,pr,0);
 if (mt=-1) then Exit(Default(TvPointer));

 Result:=Alloc(mr.size,mr.alignment,mt,False);
end;

Function TvMemManager.Alloc(Size,Align:TVkDeviceSize;mtindex:Byte;test_free:Boolean):TvPointer;
var
 key:TDevNode;
 Offset:TVkDeviceSize;
 FSize:TVkDeviceSize;
 FEndN,FEndO:TVkDeviceSize;
begin
 Result:=Default(TvPointer);
 if (Size=0) then Exit;
 key:=Default(TDevNode);
 Size:=System.Align(Size,8);
 if (Align>GRANULAR_DEV_BLOCK_SIZE) then Align:=GRANULAR_DEV_BLOCK_SIZE;
 //
 rw_wlock(global_mem_lock);
 //
 if _FetchFree_a(Size,Align,mtindex,key) then
 begin
  Offset:=System.Align(key.FOffset,Align);
  FSize:=key.FSize;
  if (Offset<>key.FOffset) then //prev free save
  begin
   key.FSize:=Offset-key.FOffset;
   FFreeSet.Insert(key);
   FAllcSet.Insert(key);
  end;
  FEndN:=Offset+Size;
  FEndO:=key.FOffset+FSize;
  if (FEndN<>FEndO) then //next free save
  begin
   key.FOffset:=FEndN;
   key.FSize  :=FEndO-FEndN;
   FFreeSet.Insert(key);
   FAllcSet.Insert(key);
  end;
  //alloc save
  key.Fisfree:=False;
  key.FOffset:=Offset;
  key.FSize  :=Size;
  FAllcSet.Insert(key);
  Result.FMemory:=FDevBlocks[key.FBlockId];
  Result.FOffset:=key.FOffset;
 end else
 begin
  if not test_free then
  if _AllcDevBlock(System.Align(Size,GRANULAR_DEV_BLOCK_SIZE),mtindex,key.FBlockId) then
  begin
   //alloc save
   key.Fisfree:=False;
   key.FSize  :=Size;
   key.FOffset:=0;
   key.FmType :=mtindex;
   FAllcSet.Insert(key);
   Result.FMemory:=FDevBlocks[key.FBlockId];
   Result.FOffset:=0;
   //next free save
   FSize:=FDevBlocks[key.FBlockId].FSize;
   if (Size<>FSize) then
   begin
    key.Fisfree:=True;
    key.FOffset:=Size;
    key.FSize  :=FSize-Size;
    FFreeSet.Insert(key);
    FAllcSet.Insert(key);
   end;
  end;
 end;
 //
 if (Result.FMemory<>nil) then
 begin
  Result.FMemory.Acquire(nil);
 end;
 //
 rw_wunlock(global_mem_lock);
end;

Function TvMemManager.Free(P:TvPointer):Boolean;
var
 key,key2:TDevNode;
begin
 if (P.FMemory=nil) then Exit;
 key:=Default(TDevNode);
 //
 rw_wlock(global_mem_lock);
 //
 if _FindDevBlock(P.FMemory,key.FBlockId) then
 if _FetchAllc(P.FOffset,key.FBlockId,key) then
 begin
  //prev union
  repeat
   if (key.FOffset=0) then Break;
   key2:=key;
   key2.FOffset:=key2.FOffset-1;
   if not _FetchFree_l(key2,key2) then Break;
   Assert((key2.FOffset+key2.FSize)=key.FOffset);
   key.FSize  :=key.FSize+(key.FOffset-key2.FOffset);
   key.FOffset:=key2.FOffset;
  until false;
  //next union
  repeat
   key2:=key;
   key2.FOffset:=key2.FOffset+key2.FSize;
   if not _FetchFree_b(key2,key2) then Break;
   Assert((key.FOffset+key.FSize)=key2.FOffset);
   key.FSize  :=key.FSize+key2.FSize;
  until false;
  //
  if (key.FOffset=0) and (key.FSize>=FDevBlocks[key.FBlockId].FSize) then
  begin
   //free block
   _FreeDevBlock(key.FBlockId);
  end else
  begin
   //add free
   key.Fisfree:=True;
   FFreeSet.Insert(key);
   FAllcSet.Insert(key);
  end;
  Result:=True;
  //
  P.FMemory.Release(nil);
 end;
 //
 rw_wunlock(global_mem_lock);
end;

Function TvMemManager._shrink_dev_block(max:TVkDeviceSize;heap_index:Byte):TVkDeviceSize;
var
 i,c:Word;
begin
 Result:=0;

 if (FBacked<>nil) then
 begin
  Result:=Result+FBacked.FSize;
  ReleaseAndNil(FBacked);
  if (Result>=max) then Exit;
 end;

 if (Length(FDevBlocks)<>0) then
 For i:=High(FDevBlocks) to 0 do
 begin

  if (FDevBlocks[i]<>nil) then
  if (FDevBlocks[i].FMemInfo.heap_index=heap_index) then
  if (FDevBlocks[i].FRefs<=1) then
  begin
   Result:=Result+FDevBlocks[i].FSize;
   FreeAndNil(FDevBlocks[i]);
   if (Result>=max) then Break;
  end;

 end;

 //shrink
 c:=Length(FDevBlocks);
 While (c<>0) do
 begin
  if (FDevBlocks[c-1]=nil) then
   Dec(c)
  else
   Break;
 end;
 SetLength(FDevBlocks,c);
end;

Function TvMemManager._shrink_host_map(max:TVkDeviceSize):TVkDeviceSize;
var
 node,prev:TvHostMemory;
begin
 Result:=0;

 node:=TvHostMemory(TAILQ_LAST(@FHosts));
 while (node<>nil) do
 begin
  prev:=TvHostMemory(TAILQ_PREV(node,@node.entry));

  if (node.FRefs<=1) then
  begin
   TAILQ_REMOVE(@FHosts,node,@node.entry);
   Result:=Result+node.FSize;
   FreeAndNil(node);
   if (Result>=max) then Break;
  end;

  node:=prev;
 end;

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
    node.Release(nil);
    node:=nil;
   end else
   if rmem_map_test_lock(node.FStart,node.F__End) then
   begin
    goto _full;
   end else
   if (node.FRefs<=1) then
   begin
    //partial
    TAILQ_REMOVE(@FHosts,node,@node.entry);
    FreeAndNil(node);
   end;

  end;

  node:=next;
 end;

 //
 rw_wunlock(global_mem_lock);
end;

function AlignUp(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
var
 tmp:PtrUInt;
begin
 tmp:=addr+PtrUInt(alignment-1);
 Result:=tmp-(tmp mod alignment)
end;

function AlignDw(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
begin
 Result:=addr-(addr mod alignment);
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
 tmp,tmp2:QWORD;
 node:TvHostMemory;
 FHandle:TVkDeviceMemory;
 found:Boolean;
begin
 Result:=Default(TvPointer);
 if (Addr=0) or (Size=0) then Exit;
 //
 FStart:=QWORD(Addr);
 F__End:=FStart+Size;
 //
 rw_wlock(global_mem_lock);
 //

 found:=False;

 node:=TvHostMemory(TAILQ_FIRST(@FHosts));
 while (node<>nil) do
 begin

  if (FStart>=node.FStart) and
     (F__End<=node.F__End) then
  begin
   found:=True;
   Break;
  end;

  node:=TvHostMemory(TAILQ_NEXT(node,@node.entry));
 end;

 if (node=nil) then
 begin
  FStart_align:=AlignDw(FStart,GRANULAR_MAP_BLOCK_SIZE);
  F__End_align:=AlignUp(F__End,GRANULAR_MAP_BLOCK_SIZE);

  _retry:

  tmp:=F__End_align-FStart_align;

  FHandle:=vkAllocHostMemory(Device.FHandle,tmp,mtindex,Pointer(FStart_align));

  if (FHandle=VK_NULL_HANDLE) then
  begin
   //try shrink
   tmp:=size;

   tmp2:=_shrink_host_map(tmp);

   if (tmp2<tmp) then
   begin
    tmp:=tmp-tmp2;
   end else
   begin
    tmp:=0;
   end;

   if (tmp>0) then
   begin
    tmp:=tmp-tmp2;
    //try shrink 2
    tmp2:=_shrink_dev_block(tmp,FProperties.memoryTypes[mtindex].heapIndex);

    if (tmp2<tmp) then
    begin
     tmp:=tmp-tmp2;
    end else
    begin
     tmp:=0;
    end;
   end;

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

