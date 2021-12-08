unit vImage;

{$mode objfpc}{$H+}

interface

uses
 g23tree,
 vulkan,
 vDevice,
 vPipeline,
 vMemory;

type
 TSwapChain=class
  FSurface:TvSurface;
  FSize:TVkExtent2D;
  FHandle:TVkSwapchainKHR;
  FImage:array of TVkImage;
  FViews:array of TVkImageView;
  Constructor Create(Surface:TvSurface;mode:Integer;imageUsage:TVkImageUsageFlags);
  Destructor  Destroy; override;
 end;

 TvImageView=class
  FHandle:TVkImageView;
  Destructor  Destroy; override;
 end;

 TvImage=class
  FHandle:TVkImage;
  FFormat:TVkFormat;
  FUsage:TVkFlags;
  FExtent:TVkExtent3D;
  Constructor Create(format:TVkFormat;extent:TVkExtent3D;usage:TVkFlags;ext:Pointer=nil);
  Destructor  Destroy; override;
  function    GetRequirements:TVkMemoryRequirements;
  function    GetDedicatedAllocation:Boolean;
  function    BindMem(P:TvPointer):TVkResult;
  function    GetCInfo:TVkImageCreateInfo; virtual; abstract;
  function    GetIVCInfo:TVkImageViewCreateInfo; virtual; abstract;
  function    NewView:TvImageView;
  function    NewViewF(Format:TVkFormat):TvImageView;
 end;

 TvHostImage2D=class(TvImage)
  function    GetCInfo:TVkImageCreateInfo; override;
 end;

 TvDeviceImage2D=class(TvImage)
  function    GetIVCInfo:TVkImageViewCreateInfo; override;
  function    GetCInfo:TVkImageCreateInfo; override;
 end;

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

 _TvImageViewCompare=object
  function c(const a,b:TvImageView):Integer; static;
 end;

 _TvImageViewSet=specialize T23treeSet<TvImageView,_TvImageViewCompare>;

 TvFramebuffer=class
  FHandle:TVkFramebuffer;
  FEdit,FCompile:ptruint;
  FRenderPass:TvRenderPass;
  FSize:TVkExtent2D;
  FImages:_TvImageViewSet;
  Procedure  SetRenderPass(r:TvRenderPass);
  Procedure  SetSize(Size:TVkExtent2D);
  Procedure  AddImageView(v:TvImageView);
  Procedure  ClearImageViews;
  Procedure  FreeImageViews;
  function   IsEdit:Boolean;
  function   Compile:Boolean;
  Destructor Destroy; override;
 end;

implementation

function _TvImageViewCompare.c(const a,b:TvImageView):Integer;
begin
 Result:=CompareByte(a,b,SizeOf(TvImageView));
end;

Procedure TvFramebuffer.SetRenderPass(r:TvRenderPass);
begin
 if (r=FRenderPass) then Exit;
 FRenderPass:=r;
 Inc(FEdit);
end;

Procedure TvFramebuffer.SetSize(Size:TVkExtent2D);
begin
 if CompareByte(Size,FSize,SizeOf(TVkExtent2D))=0 then Exit;
 FSize:=Size;
 Inc(FEdit);
end;

Procedure TvFramebuffer.AddImageView(v:TvImageView);
begin
 if (v=nil) then Exit;
 if FImages.Contains(v) then Exit;
 FImages.Insert(v);
 Inc(FEdit);
end;

Procedure TvFramebuffer.ClearImageViews;
begin
 FImages.Free;
 Inc(FEdit);
end;

Procedure TvFramebuffer.FreeImageViews;
var
 It:_TvImageViewSet.Iterator;
begin
 It:=FImages.cbegin;
 if (It.Item<>nil) then
 repeat
  TvImageView(It.Item^).Free;
 until not It.Next;
 FImages.Free;
 Inc(FEdit);
end;

function TvFramebuffer.IsEdit:Boolean;
begin
 Result:=(FEdit<>FCompile);
end;

function TvFramebuffer.Compile:Boolean;
var
 i:TVkUInt32;
 It:_TvImageViewSet.Iterator;
 v:TvImageView;
 r:TVkResult;
 info:TVkFramebufferCreateInfo;
begin
 Result:=False;
 if (FHandle<>VK_NULL_HANDLE) and (not IsEdit) then Exit(true);
 if (FRenderPass=nil) then Exit;
 if (FRenderPass.FHandle=VK_NULL_HANDLE) then Exit;
 if (FSize.width=0) or (FSize.height=0) then Exit;
 info:=Default(TVkFramebufferCreateInfo);
 info.sType          :=VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
 info.renderPass     :=FRenderPass.FHandle;
 info.attachmentCount:=FImages.Size;
 info.width :=FSize.width;
 info.height:=FSize.height;
 info.layers:=1;

 if (info.attachmentCount<>0) then
 begin
  info.pAttachments:=AllocMem(info.attachmentCount*SizeOf(TVkImageView));
  i:=0;
  It:=FImages.cbegin;
  if (It.Item<>nil) then
  repeat
   v:=It.Item^;
   if (v<>nil) then
   begin
    info.pAttachments[i]:=v.FHandle;
    Inc(i);
   end;
  until not It.Next;
  info.attachmentCount:=i;
 end;

 if (info.attachmentCount=0) then
 begin
  if (info.pAttachments<>nil) then
   FreeMem(info.pAttachments);
  info.flags:=ord(VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT);
  info.pAttachments:=nil;
 end;

 r:=vkCreateFramebuffer(Device.FHandle,@info,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkCreateFramebuffer');
 end;

 if (info.pAttachments<>nil) then
  FreeMem(info.pAttachments);

 Result:=(r=VK_SUCCESS);
end;

Destructor TvFramebuffer.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
  vkDestroyFramebuffer(Device.FHandle,FHandle,nil);
 inherited;
end;

Constructor TSwapChain.Create(Surface:TvSurface;mode:Integer;imageUsage:TVkImageUsageFlags);
var
 queueFamilyIndices:array[0..1] of TVkUInt32;
 cinfo:TVkSwapchainCreateInfoKHR;
 r:TVkResult;
 i,count:TVkUInt32;
 cimg:TVkImageViewCreateInfo;
begin
 FSurface:=Surface;

 Case mode of
  1,2,3:;
  else
       mode:=1;
 end;

 FSize:=Surface.GetSize;
 if (FSize.width=0) or (FSize.height=0) then Exit;

 cinfo:=Default(TVkSwapchainCreateInfoKHR);
 cinfo.sType           :=VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
 cinfo.surface         :=FSurface.FHandle;
 cinfo.minImageCount   :=2;
 cinfo.imageFormat     :=FSurface.Fformat.format;
 cinfo.imageColorSpace :=FSurface.Fformat.colorSpace;
 cinfo.imageExtent     :=FSize;
 cinfo.imageArrayLayers:=1;
 cinfo.imageUsage      :=imageUsage or ord(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT);

 if (VulkanApp.FGFamily<>Surface.FPFamily) then
 begin
  queueFamilyIndices[0]:=VulkanApp.FGFamily;
  queueFamilyIndices[1]:=Surface.FPFamily;
  cinfo.imageSharingMode      :=VK_SHARING_MODE_CONCURRENT;
  cinfo.queueFamilyIndexCount :=2;
  cinfo.pQueueFamilyIndices   :=@queueFamilyIndices;
 end else
 begin
  cinfo.imageSharingMode      :=VK_SHARING_MODE_EXCLUSIVE;
  cinfo.queueFamilyIndexCount :=0;
  cinfo.pQueueFamilyIndices   :=nil;
 end;

 cinfo.preTransform  :=VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR;
 cinfo.compositeAlpha:=VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
 cinfo.presentMode   :=Surface.FModes[mode-1];
 cinfo.clipped       :=VK_TRUE;
 cinfo.oldSwapchain  :=VK_NULL_HANDLE;

 r:=vkCreateSwapchainKHR(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkCreateSwapchainKHR:',r);
  Exit;
 end;

 count:=1;
 Case mode of
  1,2:count:=2;
    3:count:=3;
 end;

 SetLength(FImage,count);
 SetLength(FViews,count);

 r:=vkGetSwapchainImagesKHR(Device.FHandle,FHandle,@count,@FImage[0]);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkGetSwapchainImagesKHR:',r);
  Exit;
 end;

 cimg:=Default(TVkImageViewCreateInfo);
 cimg.sType       :=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
 cimg.viewType    :=VK_IMAGE_VIEW_TYPE_2D;
 cimg.format      :=Surface.Fformat.format;
 cimg.components.r:=VK_COMPONENT_SWIZZLE_IDENTITY;
 cimg.components.g:=VK_COMPONENT_SWIZZLE_IDENTITY;
 cimg.components.b:=VK_COMPONENT_SWIZZLE_IDENTITY;
 cimg.components.a:=VK_COMPONENT_SWIZZLE_IDENTITY;
 cimg.subresourceRange.aspectMask    :=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 cimg.subresourceRange.baseMipLevel  :=0;
 cimg.subresourceRange.levelCount    :=1;
 cimg.subresourceRange.baseArrayLayer:=0;
 cimg.subresourceRange.layerCount    :=1;

 For i:=0 to count-1 do
 begin
  cimg.image:=FImage[i];
  r:=vkCreateImageView(Device.FHandle,@cimg,nil,@FViews[i]);
  if (r<>VK_SUCCESS) then
  begin
   Writeln('vkCreateImageView:',r);
   Exit;
  end;
 end;
end;

Destructor TSwapChain.Destroy;
var
 i:Integer;
begin
 For i:=0 to High(FViews) do
 begin
  vkDestroyImageView(Device.FHandle,FViews[i],nil);
 end;
 vkDestroySwapchainKHR(Device.FHandle,FHandle,nil);
end;

Constructor TvImage.Create(format:TVkFormat;extent:TVkExtent3D;usage:TVkFlags;ext:Pointer=nil);
var
 cinfo:TVkImageCreateInfo;
 r:TVkResult;
begin
 FFormat:=format;
 FUsage:=usage;
 FExtent:=extent;
 cinfo:=GetCInfo;
 cinfo.format:=format;
 cinfo.extent:=extent;
 cinfo.usage :=usage;
 cinfo.pNext:=ext;
 r:=vkCreateImage(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkCreateImage:',r);
  Exit;
 end;
end;

Destructor TvImage.Destroy;
begin
 vkDestroyImage(Device.FHandle,FHandle,nil);
end;

function TvImage.GetRequirements:TVkMemoryRequirements;
begin
 Result:=Default(TVkMemoryRequirements);
 vkGetImageMemoryRequirements(Device.FHandle,FHandle,@Result);
end;

function TvImage.GetDedicatedAllocation:Boolean;
var
 info:TVkImageMemoryRequirementsInfo2;
 rmem:TVkMemoryRequirements2;
 rded:TVkMemoryDedicatedRequirements;
begin
 Result:=false;
 if Pointer(vkGetImageMemoryRequirements2)=nil then Exit;
 info:=Default(TVkImageMemoryRequirementsInfo2);
 info.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2;
 info.image:=FHandle;
 rmem:=Default(TVkMemoryRequirements2);
 rmem.sType:=VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2;
 rded:=Default(TVkMemoryDedicatedRequirements);
 rded.sType:=VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS;
 rmem.pNext:=@rded;
 vkGetImageMemoryRequirements2(Device.FHandle,@info,@rmem);
 Result:=(rded.requiresDedicatedAllocation<>VK_FALSE) or
         (rded.prefersDedicatedAllocation <>VK_FALSE);
end;

function TvImage.BindMem(P:TvPointer):TVkResult;
begin
 Result:=vkBindImageMemory(Device.FHandle,FHandle,P.FHandle,P.FOffset);
end;

function TvImage.NewView:TvImageView;
var
 cinfo:TVkImageViewCreateInfo;
 FImg:TVkImageView;
 r:TVkResult;
begin
 Result:=nil;
 cinfo:=GetIVCInfo;
 cinfo.image:=FHandle;
 FImg:=VK_NULL_HANDLE;
 r:=vkCreateImageView(Device.FHandle,@cinfo,nil,@FImg);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkCreateImageView:',r);
  Exit;
 end;
 Result:=TvImageView.Create;
 Result.FHandle:=FImg;
end;

function TvImage.NewViewF(Format:TVkFormat):TvImageView;
var
 cinfo:TVkImageViewCreateInfo;
 FImg:TVkImageView;
 r:TVkResult;
begin
 Result:=nil;
 cinfo:=GetIVCInfo;
 cinfo.image :=FHandle;
 cinfo.format:=Format;
 FImg:=VK_NULL_HANDLE;
 r:=vkCreateImageView(Device.FHandle,@cinfo,nil,@FImg);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkCreateImageView:',r);
  Exit;
 end;
 Result:=TvImageView.Create;
 Result.FHandle:=FImg;
end;

Destructor TvImageView.Destroy;
begin
 vkDestroyImageView(Device.FHandle,FHandle,nil);
end;

function TvHostImage2D.GetCInfo:TVkImageCreateInfo;
begin
 Result:=Default(TVkImageCreateInfo);
 Result.sType        :=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
 Result.imageType    :=VK_IMAGE_TYPE_2D;
 Result.arrayLayers  :=1;
 Result.mipLevels    :=1;
 Result.initialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
 Result.samples      :=VK_SAMPLE_COUNT_1_BIT;
 Result.tiling       :=VK_IMAGE_TILING_LINEAR;
end;

//

function TvDeviceImage2D.GetCInfo:TVkImageCreateInfo;
begin
 Result:=Default(TVkImageCreateInfo);
 Result.sType        :=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
 Result.imageType    :=VK_IMAGE_TYPE_2D;
 Result.arrayLayers  :=1;
 Result.mipLevels    :=1;
 Result.initialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
 Result.samples      :=VK_SAMPLE_COUNT_1_BIT;
 Result.tiling       :=VK_IMAGE_TILING_OPTIMAL;
end;

function TvDeviceImage2D.GetIVCInfo:TVkImageViewCreateInfo;
begin
 Result:=Default(TVkImageViewCreateInfo);
 Result.sType       :=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
 Result.viewType    :=VK_IMAGE_VIEW_TYPE_2D;
 Result.format      :=FFormat;
 Result.components.r:=VK_COMPONENT_SWIZZLE_IDENTITY;
 Result.components.g:=VK_COMPONENT_SWIZZLE_IDENTITY;
 Result.components.b:=VK_COMPONENT_SWIZZLE_IDENTITY;
 Result.components.a:=VK_COMPONENT_SWIZZLE_IDENTITY;

 Case FFormat of
  VK_FORMAT_S8_UINT:
   Result.subresourceRange.aspectMask  :=ord(VK_IMAGE_ASPECT_STENCIL_BIT);

  VK_FORMAT_D16_UNORM,
  VK_FORMAT_X8_D24_UNORM_PACK32,
  VK_FORMAT_D32_SFLOAT:
   Result.subresourceRange.aspectMask  :=ord(VK_IMAGE_ASPECT_DEPTH_BIT);

  VK_FORMAT_D16_UNORM_S8_UINT,
  VK_FORMAT_D24_UNORM_S8_UINT,
  VK_FORMAT_D32_SFLOAT_S8_UINT:
   Result.subresourceRange.aspectMask  :=ord(VK_IMAGE_ASPECT_DEPTH_BIT) or ord(VK_IMAGE_ASPECT_STENCIL_BIT);

  else
   Result.subresourceRange.aspectMask  :=ord(VK_IMAGE_ASPECT_COLOR_BIT);
 end;

 Result.subresourceRange.baseMipLevel  :=0;
 Result.subresourceRange.levelCount    :=1;
 Result.subresourceRange.baseArrayLayer:=0;
 Result.subresourceRange.layerCount    :=1;
end;

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
  Writeln('vkCreateBuffer:',r);
  Exit;
 end;
end;

Destructor TvBuffer.Destroy;
begin
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

