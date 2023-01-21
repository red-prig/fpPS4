unit vMemory;

{$mode objfpc}{$H+}

interface

uses
 g23tree,
 Vulkan,
 vDevice;

type
 TvPointer=packed record
  FHandle:TVkDeviceMemory;
  FOffset:TVkDeviceSize;
 end;

Const
 GRANULAR_DEV_BLOCK_SIZE=128*1024*1024;

type
 TDevNode=packed record
  FSize   :TVkDeviceSize;
  FOffset :TVkDeviceSize;
  FBlockId:Word;
  FmType  :Byte;
  Fisfree :Boolean;
 end;

 TDevBlock=object
  FHandle:TVkDeviceMemory;
  nSize  :TVkDeviceSize;
  mType  :Byte;
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

 TvMemManager=class
  FProperties:TVkPhysicalDeviceMemoryProperties;
  FHostVisibMt:TVkUInt32;
  //FHostCacheMt:TVkUInt32;

  FSparceMemoryTypes:TVkUInt32;

  lock:Pointer;

  FDevBlocks:array of TDevBlock;
  FFreeSet:TFreeDevNodeSet;
  FAllcSet:TAllcDevNodeSet;

  Constructor Create;

  function    SparceSupportHost:Boolean;
  function    findMemoryType(Filter:TVkUInt32;prop:TVkMemoryPropertyFlags):Integer;
  procedure   PrintMemoryType(typeFilter:TVkUInt32);

  Function    _AllcDevBlock(Size:TVkDeviceSize;mtindex:Byte;Var R:Word):Boolean;
  Function    _FreeDevBlock(i:Word):Boolean;
  Function    _FindDevBlock(FHandle:TVkDeviceMemory;Var R:Word):Boolean;
  Function    _FetchFree_a(Size,Align:TVkDeviceSize;mtindex:Byte;var R:TDevNode):Boolean;
  Function    _FetchFree_l(key:TDevNode;var R:TDevNode):Boolean;
  Function    _FetchFree_b(key:TDevNode;var R:TDevNode):Boolean;
  Function    _FetchAllc(FOffset:TVkDeviceSize;FBlockId:Word;var R:TDevNode):Boolean;

  Function    Alloc(const mr:TVkMemoryRequirements;pr:TVkMemoryPropertyFlags):TvPointer;
  Function    Alloc(Size,Align:TVkDeviceSize;mtindex:Byte):TvPointer;
  Function    Free(P:TvPointer):Boolean;

 end;

var
 MemManager:TvMemManager;

function vkAllocMemory(device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32):TVkDeviceMemory;
function vkAllocHostPointer(device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32;adr:Pointer):TVkDeviceMemory;
function vkAllocDedicatedImage(device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32;FHandle:TVkImage):TVkDeviceMemory;
function vkAllocDedicatedBuffer(device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32;FHandle:TVkBuffer):TVkDeviceMemory;

Function TryGetHostPointerByAddr(addr:Pointer;var P:TvPointer;SizeOut:PQWORD=nil):Boolean;

function GetHostMappedRequirements:TVkMemoryRequirements;
function GetSparceMemoryTypes:TVkUInt32;

var
 MEMORY_BOUND_HACK:Boolean=False;

implementation

uses
 spinlock,
 ps4_map_mm;

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

const
 buf_ext:TVkExternalMemoryBufferCreateInfo=(
  sType:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO;
  pNext:nil;
  handleTypes:ord(VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT);
 );

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
 cinfo.usage      :=ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT);
 cinfo.sharingMode:=VK_SHARING_MODE_EXCLUSIVE;
 cinfo.pNext      :=@buf_ext;

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
 cinfo.usage      :=ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT);
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
 i:Byte;
begin
 mr:=GetHostMappedRequirements;

 Writeln('[HostMappedRequirements]');
 Writeln('  Alignment=',mr.alignment);

 Write('  MemoryType=');
 For i:=0 to 31 do
 if ((1 shl i) and (mr.memoryTypeBits))<>0 then
 begin
  Write(i,',');
 end;
 Writeln;

 FSparceMemoryTypes:=GetSparceMemoryTypes;
 Write('  SparceType=');
 For i:=0 to 31 do
 if ((1 shl i) and (FSparceMemoryTypes))<>0 then
 begin
  Write(i,',');
 end;
 Writeln;

 FProperties:=Default(TVkPhysicalDeviceMemoryProperties);
 vkGetPhysicalDeviceMemoryProperties(VulkanApp.FPhysicalDevice,@FProperties);

 FHostVisibMt:=findMemoryType(mr.memoryTypeBits,
                              ord(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or
                              ord(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or
                              ord(VK_MEMORY_PROPERTY_HOST_CACHED_BIT)
                             );

 if (FHostVisibMt=DWORD(-1)) then
 begin
  FHostVisibMt:=findMemoryType(mr.memoryTypeBits,
                               ord(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or
                               ord(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
                              );
 end;


 if (FHostVisibMt=DWORD(-1)) then
 begin
  FHostVisibMt:=findMemoryType(mr.memoryTypeBits,
                               ord(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)
                              );
 end;

 //We'll try it, but the driver still sucks
 if (FHostVisibMt=DWORD(-1)) then
 begin
  FHostVisibMt:=0;
 end;

 Writeln('  SelectHost=',FHostVisibMt);
end;

function TvMemManager.SparceSupportHost:Boolean;
begin
 Result:=((1 shl FHostVisibMt) and FSparceMemoryTypes)<>0;
end;

function TvMemManager.findMemoryType(Filter:TVkUInt32;prop:TVkMemoryPropertyFlags):Integer;
var
 i:TVkUInt32;
begin
 Result:=-1;
 For i:=0 to FProperties.memoryTypeCount-1 do
 begin
  if  ((Filter and (1 shl i))<>0) and ((FProperties.memoryTypes[i].propertyFlags and prop)=prop) then
  begin
   Exit(i);
  end;
 end;
end;

procedure TvMemManager.PrintMemoryType(typeFilter:TVkUInt32);
var
 i:TVkUInt32;
begin

 For i:=0 to FProperties.memoryTypeCount-1 do
 begin
  if  ((typeFilter and (1 shl i))<>0) then
  begin
   Write(i,':',HexStr(FProperties.memoryTypes[i].propertyFlags,8));

   if (FProperties.memoryTypes[i].propertyFlags and
    TVkUInt32(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT))<>0 then
       Write(' DEVICE_LOCAL');

   if (FProperties.memoryTypes[i].propertyFlags and
    TVkUInt32(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))<>0 then
       Write(' HOST_VISIBLE');

   if (FProperties.memoryTypes[i].propertyFlags and
    TVkUInt32(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))<>0 then
       Write(' HOST_COHERENT');

   if (FProperties.memoryTypes[i].propertyFlags and
    TVkUInt32(VK_MEMORY_PROPERTY_HOST_CACHED_BIT))<>0 then
       Write(' HOST_CACHED');

   if (FProperties.memoryTypes[i].propertyFlags and
    TVkUInt32(VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT))<>0 then
       Write(' LAZILY_ALLOCATED');

   if (FProperties.memoryTypes[i].propertyFlags and
    TVkUInt32(VK_MEMORY_PROPERTY_PROTECTED_BIT))<>0 then
       Write(' PROTECTED');

   if (FProperties.memoryTypes[i].propertyFlags and
    TVkUInt32(VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD))<>0 then
       Write(' DEVICE_COHERENT_AMD');

   if (FProperties.memoryTypes[i].propertyFlags and
    TVkUInt32(VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD))<>0 then
       Write(' DEVICE_UNCACHED_AMD');

   if (FProperties.memoryTypes[i].propertyFlags and
    TVkUInt32(VK_MEMORY_PROPERTY_RDMA_CAPABLE_BIT_NV))<>0 then
       Write(' RDMA_CAPABLE_NV');

   Writeln;
  end;
 end;

end;

Function TvMemManager._AllcDevBlock(Size:TVkDeviceSize;mtindex:Byte;Var R:Word):Boolean;
var
 FHandle:TVkDeviceMemory;
 i:Word;
begin
 Result:=False;
 FHandle:=vkAllocMemory(Device.FHandle,Size,mtindex);
 if (FHandle=VK_NULL_HANDLE) then Exit;
 if Length(FDevBlocks)<>0 then
 For i:=0 to High(FDevBlocks) do
  if (FDevBlocks[i].FHandle=VK_NULL_HANDLE) then
  begin
   FDevBlocks[i].FHandle:=FHandle;
   FDevBlocks[i].nSize  :=Size;
   FDevBlocks[i].mType  :=mtindex;
   R:=i;
   Exit(True);
  end;
 i:=Length(FDevBlocks);
 SetLength(FDevBlocks,i+1);
 FDevBlocks[i].FHandle:=FHandle;
 FDevBlocks[i].nSize  :=Size;
 FDevBlocks[i].mType  :=mtindex;
 R:=i;
 Result:=True;
end;

Function TvMemManager._FreeDevBlock(i:Word):Boolean;
var
 c:Word;
begin
 Result:=False;
 if (i>=Length(FDevBlocks)) then Exit;
 if (FDevBlocks[i].FHandle=VK_NULL_HANDLE) then Exit;
 vkFreeMemory(Device.FHandle,FDevBlocks[i].FHandle,nil);
 FDevBlocks[i].FHandle:=VK_NULL_HANDLE;
 FDevBlocks[i].nSize  :=0;
 FDevBlocks[i].mType  :=0;
 Result:=True;
 //shrink
 c:=Length(FDevBlocks);
 While (c<>0) do
 begin
  if (FDevBlocks[c-1].FHandle=VK_NULL_HANDLE) then
   Dec(c)
  else
   Break;
 end;
 SetLength(FDevBlocks,c);
end;

Function TvMemManager._FindDevBlock(FHandle:TVkDeviceMemory;Var R:Word):Boolean;
var
 i:Word;
begin
 Result:=False;
 if Length(FDevBlocks)<>0 then
 For i:=0 to High(FDevBlocks) do
  if (FDevBlocks[i].FHandle=FHandle) then
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
 mt:=findMemoryType(mr.memoryTypeBits,pr);
 if (mt=-1) then Exit(Default(TvPointer));
 Result:=Alloc(mr.size,mr.alignment,mt);
end;

Function TvMemManager.Alloc(Size,Align:TVkDeviceSize;mtindex:Byte):TvPointer;
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
 spin_lock(lock);
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
  Result.FHandle:=FDevBlocks[key.FBlockId].FHandle;
  Result.FOffset:=key.FOffset;
 end else
 if _AllcDevBlock(System.Align(Size,GRANULAR_DEV_BLOCK_SIZE),mtindex,key.FBlockId) then
 begin
  //alloc save
  key.Fisfree:=False;
  key.FSize  :=Size;
  key.FOffset:=0;
  key.FmType :=mtindex;
  FAllcSet.Insert(key);
  Result.FHandle:=FDevBlocks[key.FBlockId].FHandle;
  Result.FOffset:=0;
  //next free save
  FSize:=FDevBlocks[key.FBlockId].nSize;
  if (Size<>FSize) then
  begin
   key.Fisfree:=True;
   key.FOffset:=Size;
   key.FSize  :=FSize-Size;
   FFreeSet.Insert(key);
   FAllcSet.Insert(key);
  end;
 end;
 spin_unlock(lock);
end;

Function TvMemManager.Free(P:TvPointer):Boolean;
var
 key,key2:TDevNode;
begin
 if (P.FHandle=VK_NULL_HANDLE) then Exit;
 key:=Default(TDevNode);
 spin_lock(lock);
 if _FindDevBlock(P.FHandle,key.FBlockId) then
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
  if (key.FOffset=0) and (key.FSize>=FDevBlocks[key.FBlockId].nSize) then
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
 end;
 spin_unlock(lock);
end;

function vkAllocMemory(device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32):TVkDeviceMemory;
var
 ainfo:TVkMemoryAllocateInfo;
 r:TVkResult;
begin
 ainfo:=Default(TVkMemoryAllocateInfo);
 ainfo.sType          :=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
 ainfo.allocationSize :=Size;
 ainfo.memoryTypeIndex:=mtindex;
 Result:=VK_NULL_HANDLE;
 r:=vkAllocateMemory(device,@ainfo,nil,@Result);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkAllocateMemory:',r);
 end;
end;

function vkAllocHostPointer(device:TVkDevice;Size:TVkDeviceSize;mtindex:TVkUInt32;adr:Pointer):TVkDeviceMemory;
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
 import:=Default(TVkImportMemoryHostPointerInfoEXT);
 import.sType:=VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT;
 import.handleType:=VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT;
 import.pHostPointer:=adr;
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

function OnGpuMemAlloc(addr:Pointer;len:size_t):TVkDeviceMemory;
begin
 InitVulkan;

 //Some games request too much video memory, relevant for built-in iGPU
 if MEMORY_BOUND_HACK then
 begin
  if (len>1024*1024*1024) then len:=1024*1024*1024;
 end;

 Result:=vkAllocHostPointer(Device.FHandle,len,MemManager.FHostVisibMt{FHostCacheMt},addr);
 Assert(Result<>VK_NULL_HANDLE);
end;

procedure OnGpuMemFree(h:TVkDeviceMemory);
begin
 if (h=VK_NULL_HANDLE) then Exit;
 if not IsInitVulkan then Exit;
 vkFreeMemory(Device.FHandle,h,nil);
end;

Function TryGetHostPointerByAddr(addr:Pointer;var P:TvPointer;SizeOut:PQWORD=nil):Boolean;
var
 block:TGpuMemBlock;
begin
 Result:=False;
 if TryGetGpuMemBlockByAddr(addr,block) then
 begin
  P.FHandle:=TVkDeviceMemory(block.Handle);
  P.FOffset:=addr-block.pAddr;

  if (SizeOut<>nil) then
  begin
   SizeOut^:=block.nSize-P.FOffset;
  end;

  Result:=True;
 end;
end;

initialization
 GpuMemCb.Alloc:=TGpuMemAlloc(@OnGpuMemAlloc);
 GpuMemCb.Free :=TGpuMemFree (@OnGpuMemFree);

end.

