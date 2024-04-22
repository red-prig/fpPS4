unit vBuffer;

{$mode objfpc}{$H+}

interface

uses
 Vulkan,
 vDevice,
 vMemory;

type
 TvBuffer=class
  FHandle:TVkBuffer;
  FSize  :TVkDeviceSize;
  FUsage :TVkFlags;
  FBind  :TvPointer;
  Constructor Create(size:TVkDeviceSize;usage:TVkFlags;ext:Pointer=nil);
  Constructor CreateSparce(size:TVkDeviceSize;usage:TVkFlags;ext:Pointer=nil);
  Destructor  Destroy; override;
  function    GetRequirements:TVkMemoryRequirements;
  function    GetDedicatedAllocation:Boolean;
  function    BindMem(P:TvPointer):TVkResult;
  procedure   OnReleaseMem(Sender:TObject); virtual;
  //
  function    Acquire:Boolean;
  procedure   Release;
 end;

function VkBindSparseBufferMemory(queue:TVkQueue;buffer:TVkBuffer;bindCount:TVkUInt32;pBinds:PVkSparseMemoryBind):TVkResult;
function GetRequirements(sparce:boolean;size:TVkDeviceSize;usage:TVkFlags;ext:Pointer=nil):TVkMemoryRequirements;

implementation

function VkBindSparseBufferMemory(queue:TVkQueue;buffer:TVkBuffer;bindCount:TVkUInt32;pBinds:PVkSparseMemoryBind):TVkResult;
var
 finfo:TVkFenceCreateInfo;
 fence:TVkFence;

 bind:TVkSparseBufferMemoryBindInfo;
 info:TVkBindSparseInfo;
begin
 finfo:=Default(TVkFenceCreateInfo);
 finfo.sType:=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
 Result:=vkCreateFence(Device.FHandle,@finfo,nil,@fence);
 if (Result<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateFence:',Result);
  Exit;
 end;

 bind:=Default(TVkSparseBufferMemoryBindInfo);
 bind.buffer   :=buffer;
 bind.bindCount:=bindCount;
 bind.pBinds   :=pBinds;

 info:=Default(TVkBindSparseInfo);
 info.sType          :=VK_STRUCTURE_TYPE_BIND_SPARSE_INFO;
 info.bufferBindCount:=1;
 info.pBufferBinds   :=@bind;

 Result:=vkQueueBindSparse(queue,1,@info,fence);

 if (Result<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkQueueBindSparse:',Result);
  vkDestroyFence(Device.FHandle,fence,nil);
  Exit;
 end;

 Result:=vkWaitForFences(Device.FHandle,1,@fence,VK_TRUE,TVkUInt64(-1));
 if (Result<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkWaitForFences:',Result);
 end;

 vkDestroyFence(Device.FHandle,fence,nil);
end;

function GetRequirements(sparce:boolean;size:TVkDeviceSize;usage:TVkFlags;ext:Pointer=nil):TVkMemoryRequirements;
var
 Buffer:TvBuffer;
begin
 Case sparce of
  True :Buffer:=TvBuffer.CreateSparce(size,usage,ext);
  False:Buffer:=TvBuffer.Create(size,usage,ext);
 end;
 Result:=Buffer.GetRequirements;
 Buffer.Free;
end;

Constructor TvBuffer.Create(size:TVkDeviceSize;usage:TVkFlags;ext:Pointer=nil);
var
 cinfo:TVkBufferCreateInfo;
 r:TVkResult;
begin
 Assert(size<>0);
 FSize:=size;
 FUsage:=usage;
 cinfo:=Default(TVkBufferCreateInfo);
 cinfo.sType:=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
 cinfo.size :=size;
 cinfo.usage:=usage;
 cinfo.sharingMode:=VK_SHARING_MODE_EXCLUSIVE;
 cinfo.pNext:=ext;
 r:=vkCreateBuffer(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateBuffer:',r);
  Exit;
 end;
end;

Constructor TvBuffer.CreateSparce(size:TVkDeviceSize;usage:TVkFlags;ext:Pointer=nil);
var
 cinfo:TVkBufferCreateInfo;
 r:TVkResult;
begin
 Assert(size<>0);
 FSize:=size;
 FUsage:=usage;
 cinfo:=Default(TVkBufferCreateInfo);
 cinfo.sType:=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
 cinfo.flags:=ord(VK_BUFFER_CREATE_SPARSE_BINDING_BIT) or ord(VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT) or ord(VK_BUFFER_CREATE_SPARSE_ALIASED_BIT);
 cinfo.size :=size;
 cinfo.usage:=usage;
 cinfo.sharingMode:=VK_SHARING_MODE_EXCLUSIVE;
 cinfo.pNext:=ext;
 r:=vkCreateBuffer(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateBuffer:',r);
  Exit;
 end;
end;

Destructor TvBuffer.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyBuffer(Device.FHandle,FHandle,nil);
 end;
 inherited;
end;

function TvBuffer.GetRequirements:TVkMemoryRequirements;
begin
 Result:=Default(TVkMemoryRequirements);
 vkGetBufferMemoryRequirements(Device.FHandle,FHandle,@Result);
end;

function TvBuffer.GetDedicatedAllocation:Boolean;
var
 info:TVkBufferMemoryRequirementsInfo2;
 rmem:TVkMemoryRequirements2;
 rded:TVkMemoryDedicatedRequirements;
begin
 Result:=false;
 if Pointer(vkGetImageMemoryRequirements2)=nil then Exit;
 info:=Default(TVkBufferMemoryRequirementsInfo2);
 info.sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2;
 info.buffer:=FHandle;
 rmem:=Default(TVkMemoryRequirements2);
 rmem.sType:=VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2;
 rded:=Default(TVkMemoryDedicatedRequirements);
 rded.sType:=VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS;
 rmem.pNext:=@rded;
 vkGetBufferMemoryRequirements2(Device.FHandle,@info,@rmem);
 Result:=(rded.requiresDedicatedAllocation<>VK_FALSE) or
         (rded.prefersDedicatedAllocation <>VK_FALSE);
end;

function TvBuffer.BindMem(P:TvPointer):TVkResult;
begin
 if P.Acquire then
 begin
  Result:=vkBindBufferMemory(Device.FHandle,FHandle,P.FMemory.FHandle,P.FOffset);
  //
  if (Result=VK_SUCCESS) then
  begin
   FBind:=P;
   P.FMemory.AddDependence(@Self.OnReleaseMem);
  end else
  begin
   P.Release;
  end;
  //
 end else
 begin
  Result:=VK_ERROR_UNKNOWN;
 end;
end;

procedure TvBuffer.OnReleaseMem(Sender:TObject);
begin
 FBind.FMemory:=nil;
 //
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyBuffer(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;
end;

function TvBuffer.Acquire:Boolean;
begin
 Result:=FBind.Acquire;
end;

procedure TvBuffer.Release;
begin
 FBind.Release;
end;


end.

