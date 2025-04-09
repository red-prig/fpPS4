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
  FName  :RawByteString;
  Constructor Create(size:TVkDeviceSize;usage:TVkFlags;ext:Pointer=nil);
  Constructor CreateSparce(size:TVkDeviceSize;usage:TVkFlags;ext:Pointer=nil);
  Destructor  Destroy; override;
  function    GetRequirements:TVkMemoryRequirements;
  function    GetDedicatedAllocation:Boolean;
  function    BindMem(P:TvPointer):TVkResult;
  procedure   UnBindMem(do_free:Boolean);
  function    Hold(Sender:TObject):Boolean; override;
  function    Drop(Sender:TObject):Boolean; override;
  function    is_invalid:Boolean;
  procedure   FreeHandle;
  function    OnReleaseMem(Sender:TObject):Boolean; virtual;
  procedure   SetObjectName(const name:RawByteString);
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
var
 B:TvPointer;
begin
 B:=P.Acquire;
 if (B.FMemory<>nil) then //try Acquire
 begin
  if ((P.FOffset+self.FSize)>P.FMemory.FSize) then
  begin
   Assert(False);
  end;
  //
  Result:=vkBindBufferMemory(Device.FHandle,FHandle,B.FMemory.FHandle,B.FOffset);
  //
  if (Result=VK_SUCCESS) then
  begin
   B.FMemory.AddDependence(@Self.OnReleaseMem);
   FBind:=B;
  end;
  //
  B.Release; //release Acquire
 end else
 begin
  Result:=VK_ERROR_UNKNOWN;
 end;

 if (Result<>VK_SUCCESS) then
 begin
  Writeln(stderr,'Error BindMem:',Result,' To:0x',HexStr(FHandle,16));
 end;
end;

procedure TvBuffer.UnBindMem(do_free:Boolean);
var
 B:TvPointer;
 H:Integer;
begin
 B.FMemory:=TvDeviceMemory(System.InterlockedExchange(Pointer(FBind.FMemory),nil));
 B.FOffset:=FBind.FOffset;
 H:=System.InterlockedExchangeAdd(FHold,0);
 if (B.FMemory<>nil) then
 begin
  if do_free then
  begin
   B.FMemory.DelDependence(@Self.OnReleaseMem);
   while (H<>0) do
   begin
    B.Drop;
    Dec(H);
   end;
   MemManager.FreeMemory(B);
  end;
 end;
end;

function TvBuffer.Hold(Sender:TObject):Boolean;
begin
 Result:=FBind.Hold;
 if Result then
 begin
  Result:=inherited;
  if not Result then
  begin
   FBind.Drop;
  end;
 end;
end;

function TvBuffer.Drop(Sender:TObject):Boolean;
begin
 Result:=FBind.Drop;
 if Result then
 begin
  Result:=inherited;
 end;
end;

function TvBuffer.is_invalid:Boolean;
begin
 Result:=(FHandle=VK_NULL_HANDLE);
end;

procedure TvBuffer.FreeHandle;
var
 F:TVkBuffer;
begin
 F:=System.InterlockedExchange64(FHandle,VK_NULL_HANDLE);
 if (F<>VK_NULL_HANDLE) then
 begin
  vkDestroyBuffer(Device.FHandle,F,nil);
 end;
end;

Function TvBuffer.OnReleaseMem(Sender:TObject):Boolean;
begin
 FreeHandle;
 //
 UnBindMem(False);
 //
 Result:=True;
end;

procedure TvBuffer.SetObjectName(const name:RawByteString);
begin
 FName:=name;
 DebugReport.SetObjectName(VK_OBJECT_TYPE_BUFFER,FHandle,PChar(name));
end;


end.

