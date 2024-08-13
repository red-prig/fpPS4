unit vBuffer;

{$mode objfpc}{$H+}

interface

uses
 Vulkan,
 vDevice,
 vMemory,
 vDependence;

type
 TvBuffer=class(TvRefsObject)
  FHandle:TVkBuffer;
  FSize  :TVkDeviceSize;
  FUsage :TVkFlags;
  FBind  :TvPointer;
  FBRefs :ptruint;
  Constructor Create(size:TVkDeviceSize;usage:TVkFlags;ext:Pointer=nil);
  Constructor CreateSparce(size:TVkDeviceSize;usage:TVkFlags;ext:Pointer=nil);
  Destructor  Destroy; override;
  function    GetRequirements:TVkMemoryRequirements;
  function    GetDedicatedAllocation:Boolean;
  function    BindMem(P:TvPointer):TVkResult;
  procedure   UnBindMem(do_free:Boolean);
  procedure   FreeHandle;
  procedure   OnReleaseMem(Sender:TObject); virtual;
  procedure   SetObjectName(const name:RawByteString);
  //
  function    _Acquire(Sender:TObject):Boolean;
  procedure   _Release(Sender:TObject);
  function    Acquire(Sender:TObject):Boolean; override;
  procedure   Release(Sender:TObject);         override;
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
 FreeHandle;
 //
 UnBindMem(True);
 //
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
 if P.Acquire then //try Acquire
 begin
  if ((P.FOffset+self.FSize)>P.FMemory.FSize) then
  begin
   Assert(False);
  end;
  //
  Result:=vkBindBufferMemory(Device.FHandle,FHandle,P.FMemory.FHandle,P.FOffset);
  //
  if (Result=VK_SUCCESS) then
  begin
   FBind:=P;
   P.FMemory.AddDependence(@Self.OnReleaseMem);
  end;
  //
  P.Release; //release Acquire
 end else
 begin
  Result:=VK_ERROR_UNKNOWN;
 end;
end;

procedure TvBuffer.UnBindMem(do_free:Boolean);
var
 B:TvPointer;
 R:ptruint;
begin
 if (FBind.FMemory<>nil) then
 begin
  B:=FBind;
  FBind.FMemory:=nil;
  //
  R:=ptruint(System.InterlockedExchange(Pointer(FBRefs),nil));
  while (R<>0) do
  begin
   B.Release;
   Dec(R);
  end;
  //
  if do_free then
  begin
   MemManager.FreeMemory(B);
  end;
 end;
end;

procedure TvBuffer.FreeHandle;
begin
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyBuffer(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;
end;

procedure TvBuffer.OnReleaseMem(Sender:TObject);
begin
 FreeHandle;
 //
 UnBindMem(False);
end;

procedure TvBuffer.SetObjectName(const name:RawByteString);
begin
 DebugReport.SetObjectName(VK_OBJECT_TYPE_BUFFER,FHandle,PChar(name));
end;

function TvBuffer._Acquire(Sender:TObject):Boolean;
begin
 Result:=inherited Acquire(Sender);
end;

procedure TvBuffer._Release(Sender:TObject);
begin
 inherited Release(Sender);
end;

function TvBuffer.Acquire(Sender:TObject):Boolean;
begin
 if (FBind.FMemory<>nil) then
 begin
  Result:=FBind.Acquire;
  if Result then
  begin
   System.InterlockedIncrement(Pointer(FBRefs));
   inherited Acquire(Sender);
  end;
 end else
 begin
  Result:=inherited Acquire(Sender);
 end;
end;

procedure TvBuffer.Release(Sender:TObject);
var
 B:TvPointer;
 R:ptruint;
begin
 while True do
 begin
  B:=FBind;
  if (B.FMemory<>nil) and (FBRefs<>0) then
  begin
   R:=FBRefs;
   if (System.InterlockedCompareExchange(Pointer(FBRefs),Pointer(R-1),Pointer(R))=Pointer(R)) then
   begin
    B.Release;
    inherited Release(Sender);
    Break;
   end;
  end else
  begin
   inherited Release(Sender);
   Break;
  end;
 end;
end;


end.

