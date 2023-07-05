unit vImage;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 Vulkan,
 vDevice,
 vPipeline,
 vMemory;

type
 PvImageBarrier=^TvImageBarrier;
 TvImageBarrier=object
  //image:TVkImage;
  //range:TVkImageSubresourceRange;
  //
  AccessMask:TVkAccessFlags;
  ImgLayout:TVkImageLayout;
  StageMask:TVkPipelineStageFlags;
  Procedure Init({_image:TVkImage;_sub:TVkImageSubresourceRange});
  function  Push(cmd:TVkCommandBuffer;
                 image:TVkImage;
                 range:TVkImageSubresourceRange;
                 dstAccessMask:TVkAccessFlags;
  	         newImageLayout:TVkImageLayout;
  	         dstStageMask:TVkPipelineStageFlags):Boolean;
 end;

 TvSwapChainImage=class
  FHandle:TVkImage;
  FView  :TVkImage;
  Barrier:TvImageBarrier;
  procedure   PushBarrier(cmd:TVkCommandBuffer;
                          range:TVkImageSubresourceRange;
                          dstAccessMask:TVkAccessFlags;
                          newImageLayout:TVkImageLayout;
                          dstStageMask:TVkPipelineStageFlags);
 end;

 TvSwapChain=class
  FSurface:TvSurface;
  FSize:TVkExtent2D;
  FHandle:TVkSwapchainKHR;
  FImages:array of TvSwapChainImage;
  Constructor Create(Surface:TvSurface;mode:Integer;imageUsage:TVkImageUsageFlags);
  Destructor  Destroy; override;
 end;

 TvImageView=class
  FHandle:TVkImageView;
  FRefs:ptruint;
  Procedure   Acquire;
  Procedure   Release;
  Destructor  Destroy; override;
 end;

 TvCustomImage=class
  FHandle:TVkImage;
  Destructor  Destroy; override;
  function    GetImageInfo:TVkImageCreateInfo; virtual; abstract;
  function    GetRequirements:TVkMemoryRequirements;
  function    GetDedicatedAllocation:Boolean;
  function    BindMem(P:TvPointer):TVkResult;
  function    Compile(ext:Pointer):Boolean;
 end;

const
 //useage image
 TM_READ =1;
 TM_WRITE=2;
 TM_CLEAR=4;

type
 TvExtent3D=packed record
  width:Word;  //(0..16383)
  height:Word; //(0..16383)
  depth:Word;  //(0..8192)
 end;

 TvDstSel=bitpacked record
  r,g,b,a:0..15; //(0..6)
 end;

 PvImageKey=^TvImageKey;
 TvImageKey=packed object
  Addr:Pointer;
  cformat:TVkFormat;
  params:packed record
   itype:Byte;         //TVkImageType 0..2
   tiling_idx:Byte;    //0..31
   extend:TvExtent3D;
   samples:Byte;       //TVkSampleCountFlagBits 1..4
   mipLevels:Byte;     //(0..15)
   arrayLayers:Word;   //(0..16383)
  end;
 end;

 PvImageViewKey=^TvImageViewKey;
 TvImageViewKey=packed record
  cformat:TVkFormat;
  vtype:Word;       //TVkImageViewType 0..6
  dstSel:TvDstSel;
  base_level:Byte;  //first mip level (0..15)
  last_level:Byte;  //last mip level (0..15)
  base_array:Word;  //first array index (0..16383)
  last_array:Word;  //texture height (0..16383)
 end;

 TvImage=class(TvCustomImage)
  FFormat:TVkFormat;
  FExtent:TVkExtent3D;
  FUsage:TVkFlags;
  Fflags:TVkImageCreateFlags;
  Barrier:TvImageBarrier;
  Constructor Create(format:TVkFormat;extent:TVkExtent3D;usage:TVkFlags;flags:TVkImageCreateFlags;ext:Pointer=nil);
  function    GetImageInfo:TVkImageCreateInfo;    override;
  function    GetViewInfo:TVkImageViewCreateInfo; virtual; abstract;
  function    NewView:TvImageView;
  function    NewViewF(Format:TVkFormat):TvImageView;
  procedure   PushBarrier(cmd:TVkCommandBuffer;
                          range:TVkImageSubresourceRange;
                          dstAccessMask:TVkAccessFlags;
                          newImageLayout:TVkImageLayout;
                          dstStageMask:TVkPipelineStageFlags);
 end;

 TvHostImage1D=class(TvImage)
  function    GetImageInfo:TVkImageCreateInfo; override;
 end;

 TvHostImage2D=class(TvImage)
  function    GetImageInfo:TVkImageCreateInfo; override;
 end;

 TvDeviceImage1D=class(TvImage)
  function    GetViewInfo:TVkImageViewCreateInfo; override;
  function    GetImageInfo:TVkImageCreateInfo;    override;
 end;

 TvDeviceImage2D=class(TvImage)
  function    GetViewInfo:TVkImageViewCreateInfo; override;
  function    GetImageInfo:TVkImageCreateInfo;    override;
 end;

 AvFramebufferImages=array[0..8] of TvImageView;
 AvImageViews=array[0..8] of TVkImageView;

 TvFramebuffer=class
  FHandle:TVkFramebuffer;
  FEdit,FCompile:ptruint;
  FRenderPass:TvRenderPass;
  FSize:TVkExtent2D;
  FImages:AvFramebufferImages;
  FImagesCount:ptruint;
  Procedure  SetRenderPass(r:TvRenderPass);
  Procedure  SetSize(Size:TVkExtent2D);
  Procedure  AddImageView(v:TvImageView);
  Procedure  FreeImageViews;
  function   IsEdit:Boolean;
  function   Compile:Boolean;
  Destructor Destroy; override;
 end;

Function GetAspectMaskByFormat(cformat:TVkFormat):DWORD;

Function getFormatSize(cformat:TVkFormat):Byte; //in bytes
function IsTexelFormat(cformat:TVkFormat):Boolean;

function vkGetFormatSupport(format:TVkFormat;tiling:TVkImageTiling;usage:TVkImageUsageFlags):Boolean;
function vkFixFormatSupport(format:TVkFormat;tiling:TVkImageTiling;usage:TVkImageUsageFlags):TVkFormat;

implementation

Function getFormatSize(cformat:TVkFormat):Byte; //in bytes
begin
 Result:=0;
 Case cformat of
  //pixel size

  VK_FORMAT_R8_UNORM             :Result:=1;
  VK_FORMAT_R8_SNORM             :Result:=1;
  VK_FORMAT_R8_UINT              :Result:=1;
  VK_FORMAT_R8_SINT              :Result:=1;
  VK_FORMAT_R8_SRGB              :Result:=1;

  VK_FORMAT_R8G8_UNORM           :Result:=2;
  VK_FORMAT_R8G8_SNORM           :Result:=2;
  VK_FORMAT_R8G8_UINT            :Result:=2;
  VK_FORMAT_R8G8_SINT            :Result:=2;

  VK_FORMAT_R8G8B8A8_UNORM       :Result:=4;
  VK_FORMAT_R8G8B8A8_SRGB        :Result:=4;
  VK_FORMAT_R8G8B8A8_SNORM       :Result:=4;
  VK_FORMAT_R8G8B8A8_UINT        :Result:=4;
  VK_FORMAT_R8G8B8A8_SINT        :Result:=4;

  VK_FORMAT_R16_UNORM            :Result:=2;
  VK_FORMAT_R16_SNORM            :Result:=2;
  VK_FORMAT_R16_UINT             :Result:=2;
  VK_FORMAT_R16_SINT             :Result:=2;
  VK_FORMAT_R16_SFLOAT           :Result:=2;

  VK_FORMAT_R16G16_UNORM         :Result:=4;
  VK_FORMAT_R16G16_SNORM         :Result:=4;
  VK_FORMAT_R16G16_UINT          :Result:=4;
  VK_FORMAT_R16G16_SINT          :Result:=4;
  VK_FORMAT_R16G16_SFLOAT        :Result:=4;

  VK_FORMAT_R16G16B16A16_UNORM   :Result:=8;
  VK_FORMAT_R16G16B16A16_SNORM   :Result:=8;
  VK_FORMAT_R16G16B16A16_UINT    :Result:=8;
  VK_FORMAT_R16G16B16A16_SINT    :Result:=8;
  VK_FORMAT_R16G16B16A16_SFLOAT  :Result:=8;

  VK_FORMAT_R32_UINT             :Result:=4;
  VK_FORMAT_R32_SINT             :Result:=4;
  VK_FORMAT_R32_SFLOAT           :Result:=4;

  VK_FORMAT_R32G32_UINT          :Result:=8;
  VK_FORMAT_R32G32_SINT          :Result:=8;
  VK_FORMAT_R32G32_SFLOAT        :Result:=8;

  VK_FORMAT_R32G32B32A32_UINT    :Result:=16;
  VK_FORMAT_R32G32B32A32_SINT    :Result:=16;
  VK_FORMAT_R32G32B32A32_SFLOAT  :Result:=16;

  VK_FORMAT_R5G6B5_UNORM_PACK16  :Result:=2;
  VK_FORMAT_R4G4B4A4_UNORM_PACK16:Result:=2;

  VK_FORMAT_A2R10G10B10_UNORM_PACK32:Result:=4;
  VK_FORMAT_A2B10G10R10_UNORM_PACK32:Result:=4;

  VK_FORMAT_B10G11R11_UFLOAT_PACK32 :Result:=4;
  VK_FORMAT_E5B9G9R9_UFLOAT_PACK32  :Result:=4;

  //stencil
  VK_FORMAT_S8_UINT              :Result:=1;
  //depth
  VK_FORMAT_D16_UNORM            :Result:=2;
  VK_FORMAT_X8_D24_UNORM_PACK32  :Result:=4;
  VK_FORMAT_D32_SFLOAT           :Result:=4;
  //depth stencil
  VK_FORMAT_D16_UNORM_S8_UINT    :Result:=3;
  VK_FORMAT_D24_UNORM_S8_UINT    :Result:=4;
  VK_FORMAT_D32_SFLOAT_S8_UINT   :Result:=5;

  //texel size
  VK_FORMAT_BC1_RGB_UNORM_BLOCK..
  VK_FORMAT_BC1_RGBA_SRGB_BLOCK,
  VK_FORMAT_BC4_UNORM_BLOCK..
  VK_FORMAT_BC4_SNORM_BLOCK      :Result:=8;

  VK_FORMAT_BC2_UNORM_BLOCK..
  VK_FORMAT_BC3_SRGB_BLOCK,
  VK_FORMAT_BC5_UNORM_BLOCK..
  VK_FORMAT_BC7_SRGB_BLOCK       :Result:=16;

  else
   Assert(false,'getFormatSize:TODO:'+IntToStr(ord(cformat)));
 end;
end;

function IsTexelFormat(cformat:TVkFormat):Boolean;
begin
 Case cformat of
  VK_FORMAT_BC1_RGB_UNORM_BLOCK..
  VK_FORMAT_BC7_SRGB_BLOCK:
   Result:=True;
  else
   Result:=False;
 end;
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
 if (FImagesCount>=Length(AvFramebufferImages)) then Exit;
 FImages[FImagesCount]:=v;
 Inc(FImagesCount);
 v.Acquire;
 Inc(FEdit);
end;

Procedure TvFramebuffer.FreeImageViews;
var
 i:Integer;
begin
 if (FImagesCount<>0) then
 For i:=0 to FImagesCount-1 do
 if (FImages[i]<>nil) then
 begin
  FImages[i].Release;
  FImages[i]:=nil;
 end;
 FImagesCount:=0;

 //It:=FImages.cbegin;
 //if (It.Item<>nil) then
 //repeat
 // TvImageView(It.Item^).Release;
 //until not It.Next;
 //FImages.Free;
 Inc(FEdit);
end;

function TvFramebuffer.IsEdit:Boolean;
begin
 Result:=(FEdit<>FCompile);
end;

function TvFramebuffer.Compile:Boolean;
var
 i:TVkUInt32;
 r:TVkResult;
 info:TVkFramebufferCreateInfo;
 FImageViews:AvImageViews;
begin
 Result:=False;
 if (not IsEdit) then Exit(true);
 if (FRenderPass=nil) then Exit;
 if (FRenderPass.FHandle=VK_NULL_HANDLE) then Exit;
 if (FSize.width=0) or (FSize.height=0) then Exit;

 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyFramebuffer(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;

 info:=Default(TVkFramebufferCreateInfo);
 info.sType          :=VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
 info.renderPass     :=FRenderPass.FHandle;
 info.attachmentCount:=FImagesCount;
 info.width :=FSize.width;
 info.height:=FSize.height;
 info.layers:=1;

 if (info.attachmentCount<>0) then
 begin
  FImageViews:=Default(AvImageViews);

  For i:=0 to FImagesCount-1 do
  if (FImages[i]<>nil) then
  begin
   FImageViews[i]:=FImages[i].FHandle;
  end;

  info.pAttachments:=@FImageViews;
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
  Writeln(StdErr,'vkCreateFramebuffer');
 end;

 Result:=(r=VK_SUCCESS);
end;

Destructor TvFramebuffer.Destroy;
begin
 FreeImageViews;
 if (FHandle<>VK_NULL_HANDLE) then
  vkDestroyFramebuffer(Device.FHandle,FHandle,nil);
 inherited;
end;

Constructor TvSwapChain.Create(Surface:TvSurface;mode:Integer;imageUsage:TVkImageUsageFlags);
var
 queueFamilyIndices:array[0..1] of TVkUInt32;
 cinfo:TVkSwapchainCreateInfoKHR;
 r:TVkResult;
 i,count:TVkUInt32;
 cimg:TVkImageViewCreateInfo;
 FImage:array of TVkImage;
 FView:TVkImageView;
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
  Writeln(StdErr,'vkCreateSwapchainKHR:',r);
  Exit;
 end;

 count:=1;
 Case mode of
  1,2:count:=2;
    3:count:=3;
 end;

 SetLength(FImage,count);
 SetLength(FImages,count);

 r:=vkGetSwapchainImagesKHR(Device.FHandle,FHandle,@count,@FImage[0]);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkGetSwapchainImagesKHR:',r);
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
  FView:=VK_NULL_HANDLE;
  r:=vkCreateImageView(Device.FHandle,@cimg,nil,@FView);
  if (r<>VK_SUCCESS) then
  begin
   Writeln(StdErr,'vkCreateImageView:',r);
   Exit;
  end;
  FImages[i]:=TvSwapChainImage.Create;
  FImages[i].FHandle:=FImage[i];
  FImages[i].FView  :=FView;
  FImages[i].Barrier.Init;
 end;
end;

Destructor TvSwapChain.Destroy;
var
 i:Integer;
begin
 For i:=0 to High(FImages) do
 begin
  if (FImages[i].FView<>VK_NULL_HANDLE) then
   vkDestroyImageView(Device.FHandle,FImages[i].FView,nil);
  FImages[i].Free;
 end;
 if (FHandle<>VK_NULL_HANDLE) then
  vkDestroySwapchainKHR(Device.FHandle,FHandle,nil);
end;

Destructor TvCustomImage.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
  vkDestroyImage(Device.FHandle,FHandle,nil);
 inherited;
end;

function TvCustomImage.GetRequirements:TVkMemoryRequirements;
begin
 Result:=Default(TVkMemoryRequirements);
 vkGetImageMemoryRequirements(Device.FHandle,FHandle,@Result);
end;

function TvCustomImage.GetDedicatedAllocation:Boolean;
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

function TvCustomImage.BindMem(P:TvPointer):TVkResult;
begin
 Result:=vkBindImageMemory(Device.FHandle,FHandle,P.FHandle,P.FOffset);
end;

procedure _test_and_set_to(var new:TVkFlags;
                           test:TVkFlags;
                           val_test:TVkImageUsageFlagBits;
                           val_sets:TVkFormatFeatureFlagBits);
begin
 if ((test and ord(val_test))<>0) then
 begin
  new:=new or ord(val_sets);
 end;
end;

function vkGetFormatSupport(format:TVkFormat;tiling:TVkImageTiling;usage:TVkImageUsageFlags):Boolean;
var
 prop:TVkFormatProperties;
 test:TVkFormatFeatureFlags;
begin
 Result:=False;

 prop:=Default(TVkFormatProperties);
 vkGetPhysicalDeviceFormatProperties(
  VulkanApp.FPhysicalDevice,
  format,
  @prop);

 test:=0;
 _test_and_set_to(test,usage,VK_IMAGE_USAGE_TRANSFER_SRC_BIT            ,VK_FORMAT_FEATURE_TRANSFER_SRC_BIT);
 _test_and_set_to(test,usage,VK_IMAGE_USAGE_TRANSFER_DST_BIT            ,VK_FORMAT_FEATURE_TRANSFER_DST_BIT);
 _test_and_set_to(test,usage,VK_IMAGE_USAGE_SAMPLED_BIT                 ,VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT);
 _test_and_set_to(test,usage,VK_IMAGE_USAGE_STORAGE_BIT                 ,VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT);
 _test_and_set_to(test,usage,VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT        ,VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT);
 _test_and_set_to(test,usage,VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT        ,VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT);
 _test_and_set_to(test,usage,VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT);
 _test_and_set_to(test,usage,VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT        ,VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT);

 Case tiling of
  VK_IMAGE_TILING_OPTIMAL:
   begin
    Result:=(prop.optimalTilingFeatures and test)=test;
   end;
  VK_IMAGE_TILING_LINEAR:
   begin
    Result:=(prop.linearTilingFeatures and test)=test;
   end;
  else;
 end;

end;

//D16_UNORM_S8_UINT   -> D24_UNORM_S8_UINT -> D32_SFLOAT_S8_UINT
//X8_D24_UNORM_PACK32 -> D32_SFLOAT

function vkFixFormatSupport(format:TVkFormat;tiling:TVkImageTiling;usage:TVkImageUsageFlags):TVkFormat;
begin
 Result:=format;

 repeat

  Case Result of

   VK_FORMAT_D16_UNORM_S8_UINT:
    begin
     if vkGetFormatSupport(Result,tiling,usage) then Break;
     Result:=VK_FORMAT_D24_UNORM_S8_UINT;
    end;

   VK_FORMAT_D24_UNORM_S8_UINT:
    begin
     if vkGetFormatSupport(Result,tiling,usage) then Break;
     Result:=VK_FORMAT_D32_SFLOAT_S8_UINT;
    end;

   VK_FORMAT_X8_D24_UNORM_PACK32:
    begin
     if vkGetFormatSupport(Result,tiling,usage) then Break;
     Result:=VK_FORMAT_D32_SFLOAT;
    end;

   else
        Break;
  end;

 until false;

end;

function TvCustomImage.Compile(ext:Pointer):Boolean;
var
 cinfo:TVkImageCreateInfo;
 r:TVkResult;
begin
 Result:=False;

 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyImage(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;

 cinfo:=GetImageInfo;
 cinfo.pNext:=ext;

 cinfo.format:=vkFixFormatSupport(cinfo.format,cinfo.tiling,cinfo.usage);

 r:=vkCreateImage(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateImage:',r);
  Exit;
 end;
 Result:=True;
end;

Constructor TvImage.Create(format:TVkFormat;extent:TVkExtent3D;usage:TVkFlags;flags:TVkImageCreateFlags;ext:Pointer=nil);
begin
 FFormat:=format;
 FExtent:=extent;
 FUsage:=usage;
 Fflags:=flags;
 Barrier.Init;
 Compile(ext);
end;

function TvImage.GetImageInfo:TVkImageCreateInfo;
begin
 Result:=Default(TVkImageCreateInfo);
 Result.format:=FFormat;
 Result.extent:=FExtent;
 Result.usage :=FUsage;
 Result.flags :=Fflags;
end;

function TvImage.NewView:TvImageView;
begin
 Result:=NewViewF(FFormat);
end;

function TvImage.NewViewF(Format:TVkFormat):TvImageView;
var
 cinfo:TVkImageViewCreateInfo;
 FImg:TVkImageView;
 r:TVkResult;
begin
 Result:=nil;
 cinfo:=GetViewInfo;
 cinfo.image :=FHandle;
 cinfo.format:=Format;
 FImg:=VK_NULL_HANDLE;
 r:=vkCreateImageView(Device.FHandle,@cinfo,nil,@FImg);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateImageView:',r);
  Exit;
 end;
 Result:=TvImageView.Create;
 Result.FHandle:=FImg;
end;

procedure TvSwapChainImage.PushBarrier(cmd:TVkCommandBuffer;
                                       range:TVkImageSubresourceRange;
                                       dstAccessMask:TVkAccessFlags;
                                       newImageLayout:TVkImageLayout;
                                       dstStageMask:TVkPipelineStageFlags);
begin
 if (cmd=VK_NULL_HANDLE) then Exit;

 Barrier.Push(cmd,
              FHandle,
              range,
              dstAccessMask,
              newImageLayout,
              dstStageMask);
end;

procedure TvImage.PushBarrier(cmd:TVkCommandBuffer;
                              range:TVkImageSubresourceRange;
                              dstAccessMask:TVkAccessFlags;
                              newImageLayout:TVkImageLayout;
                              dstStageMask:TVkPipelineStageFlags);
begin
 if (cmd=VK_NULL_HANDLE) then Exit;

 Barrier.Push(cmd,
              FHandle,
              range,
              dstAccessMask,
              newImageLayout,
              dstStageMask);
end;

Procedure TvImageView.Acquire;
begin
 System.InterlockedIncrement(Pointer(FRefs));
end;

Procedure TvImageView.Release;
begin
 if System.InterlockedDecrement(Pointer(FRefs))=nil then
 begin
  Free;
 end;
end;

Destructor TvImageView.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
  vkDestroyImageView(Device.FHandle,FHandle,nil);
end;

function TvHostImage1D.GetImageInfo:TVkImageCreateInfo;
begin
 Result:=inherited;
 Result.sType        :=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
 Result.imageType    :=VK_IMAGE_TYPE_1D;
 Result.arrayLayers  :=1;
 Result.mipLevels    :=1;
 Result.initialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
 Result.samples      :=VK_SAMPLE_COUNT_1_BIT;
 Result.tiling       :=VK_IMAGE_TILING_LINEAR;
end;

function TvHostImage2D.GetImageInfo:TVkImageCreateInfo;
begin
 Result:=inherited;
 Result.sType        :=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
 Result.imageType    :=VK_IMAGE_TYPE_2D;
 Result.arrayLayers  :=1;
 Result.mipLevels    :=1;
 Result.initialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
 Result.samples      :=VK_SAMPLE_COUNT_1_BIT;
 Result.tiling       :=VK_IMAGE_TILING_LINEAR;
end;

//

function TvDeviceImage1D.GetImageInfo:TVkImageCreateInfo;
begin
 Result:=inherited;
 Result.sType        :=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
 Result.imageType    :=VK_IMAGE_TYPE_1D;
 Result.arrayLayers  :=1;
 Result.mipLevels    :=1;
 Result.initialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
 Result.samples      :=VK_SAMPLE_COUNT_1_BIT;
 Result.tiling       :=VK_IMAGE_TILING_OPTIMAL;
end;

function TvDeviceImage1D.GetViewInfo:TVkImageViewCreateInfo;
begin
 Result:=Default(TVkImageViewCreateInfo);
 Result.sType       :=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
 Result.viewType    :=VK_IMAGE_VIEW_TYPE_1D;
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

//

function TvDeviceImage2D.GetImageInfo:TVkImageCreateInfo;
begin
 Result:=inherited;
 Result.sType        :=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
 Result.imageType    :=VK_IMAGE_TYPE_2D;
 Result.arrayLayers  :=1;
 Result.mipLevels    :=1;
 Result.initialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
 Result.samples      :=VK_SAMPLE_COUNT_1_BIT;
 Result.tiling       :=VK_IMAGE_TILING_OPTIMAL;
end;

function TvDeviceImage2D.GetViewInfo:TVkImageViewCreateInfo;
begin
 Result:=Default(TVkImageViewCreateInfo);
 Result.sType       :=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
 Result.viewType    :=VK_IMAGE_VIEW_TYPE_2D;
 Result.format      :=FFormat;
 Result.components.r:=VK_COMPONENT_SWIZZLE_IDENTITY;
 Result.components.g:=VK_COMPONENT_SWIZZLE_IDENTITY;
 Result.components.b:=VK_COMPONENT_SWIZZLE_IDENTITY;
 Result.components.a:=VK_COMPONENT_SWIZZLE_IDENTITY;

 Result.subresourceRange.aspectMask    :=GetAspectMaskByFormat(FFormat);
 Result.subresourceRange.baseMipLevel  :=0;
 Result.subresourceRange.levelCount    :=1;
 Result.subresourceRange.baseArrayLayer:=0;
 Result.subresourceRange.layerCount    :=1;
end;

Function GetAspectMaskByFormat(cformat:TVkFormat):DWORD;
begin
 Case cformat of
  VK_FORMAT_S8_UINT:
   Result  :=ord(VK_IMAGE_ASPECT_STENCIL_BIT);

  VK_FORMAT_D16_UNORM,
  VK_FORMAT_X8_D24_UNORM_PACK32,
  VK_FORMAT_D32_SFLOAT:
   Result  :=ord(VK_IMAGE_ASPECT_DEPTH_BIT);

  VK_FORMAT_D16_UNORM_S8_UINT,
  VK_FORMAT_D24_UNORM_S8_UINT,
  VK_FORMAT_D32_SFLOAT_S8_UINT:
   Result  :=ord(VK_IMAGE_ASPECT_DEPTH_BIT) or ord(VK_IMAGE_ASPECT_STENCIL_BIT);

  else
   Result  :=ord(VK_IMAGE_ASPECT_COLOR_BIT);
 end;
end;

Procedure TvImageBarrier.Init({_image:TVkImage;_sub:TVkImageSubresourceRange});
begin
 //image     :=_image;
 //range     :=_sub;
 AccessMask:=ord(VK_ACCESS_NONE_KHR);
 ImgLayout :=VK_IMAGE_LAYOUT_UNDEFINED;
 StageMask :=ord(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT);
end;

function TvImageBarrier.Push(cmd:TVkCommandBuffer;
                              image:TVkImage;
                              range:TVkImageSubresourceRange;
                              dstAccessMask:TVkAccessFlags;
	                      newImageLayout:TVkImageLayout;
	                      dstStageMask:TVkPipelineStageFlags):Boolean;
var
 info:TVkImageMemoryBarrier;
begin
 Result:=False;

 if (AccessMask<>dstAccessMask) or
    (ImgLayout <>newImageLayout) or
    (StageMask <>dstStageMask) then
 begin
  Result:=True;

  info:=Default(TVkImageMemoryBarrier);
  info.sType           :=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  info.srcAccessMask   :=AccessMask;
  info.dstAccessMask   :=dstAccessMask;
  info.oldLayout       :=ImgLayout;
  info.newLayout       :=newImageLayout;
  info.image           :=image;
  info.subresourceRange:=range;

  vkCmdPipelineBarrier(cmd,
                       StageMask,
                       dstStageMask,
                       0,
                       0, nil,
                       0, nil,
                       1, @info);

  AccessMask:=dstAccessMask;
  ImgLayout :=newImageLayout;
  StageMask :=dstStageMask;
 end;
end;

end.

