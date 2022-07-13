unit vBuffer;

{$mode objfpc}{$H+}

interface

uses
 vulkan,
 vDevice,
 vMemory;

type
 TvBuffer=class
  FHandle:TVkBuffer;
  FSize:TVkDeviceSize;
  FUsage:TVkFlags;
  Constructor Create(size:TVkDeviceSize;usage:TVkFlags;ext:Pointer=nil);
  Destructor  Destroy; override;
  function    GetRequirements:TVkMemoryRequirements;
  function    GetDedicatedAllocation:Boolean;
  function    BindMem(P:TvPointer):TVkResult;
 end;

implementation

Constructor TvBuffer.Create(size:TVkDeviceSize;usage:TVkFlags;ext:Pointer=nil);
var
 cinfo:TVkBufferCreateInfo;
 r:TVkResult;
begin
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

Destructor TvBuffer.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
  vkDestroyBuffer(Device.FHandle,FHandle,nil);
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
 Result:=vkBindBufferMemory(Device.FHandle,FHandle,P.FHandle,P.FOffset);
end;

end.

