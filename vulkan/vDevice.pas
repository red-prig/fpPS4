unit vDevice;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  atomic,
  Vulkan;

type
 TExtensionNames=array of PChar;
 APhysicalDeviceProperties=array of TVkPhysicalDeviceProperties;

 t_vulkan_app_flags=Set of
   (va_debug_utils,                  //VK_EXT_debug_utils
    va_validation_layer,             //VK_LAYER_KHRONOS_validation
    //
    va_enable_gpu_assisted,          //VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT
    va_enable_gpu_assisted_reserve,  //VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT
    va_enable_best_practices,        //VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT
    va_enable_debug_printf,          //VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT
    va_enable_sync_validation,       //VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT
    //
    va_disable_shaders,              //VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT
    va_disable_thread_safety,        //VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT
    va_disable_api_params,           //VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT
    va_disable_obj_lifetimes,        //VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT
    va_disable_core_checks,          //VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT
    va_disable_unique_handles,       //VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT
    va_disable_shader_validation     //VK_VALIDATION_FEATURE_DISABLE_SHADER_VALIDATION_CACHE_EXT
   );

 TVulkanApp=class
  FInstance:TVkInstance;
  FPhysicalDevice:TVkPhysicalDevice;
  //
  FGFamily:TVkUInt32;
  FCFamily:TVkUInt32;
  FTFamily:TVkUInt32;
  //
  FGFamilyCount:TVkUInt32;
  FCFamilyCount:TVkUInt32;
  FTFamilyCount:TVkUInt32;
  //
  Constructor Create(flags:t_vulkan_app_flags);
  Destructor  Destroy; override;
  Procedure   LoadFamily; virtual;
  function    InstanceLayersIsExist(P:PChar):Boolean;
 end;

 TvDebugReport=class
  FHandle:TVkDebugUtilsMessengerEXT;
  //
  FStdOut:Text;
  //
  FCreateDebugUtilsMessenger :TvkCreateDebugUtilsMessengerEXT;
  FDestroyDebugUtilsMessenger:TvkDestroyDebugUtilsMessengerEXT;
  //
  FSetDebugUtilsObjectName   :TvkSetDebugUtilsObjectNameEXT;
  FCmdBeginDebugUtilsLabel   :TvkCmdBeginDebugUtilsLabelEXT;
  FCmdEndDebugUtilsLabel     :TvkCmdEndDebugUtilsLabelEXT;
  FCmdInsertDebugUtilsLabel  :TvkCmdInsertDebugUtilsLabelEXT;
  //
  Constructor Create;
  Destructor  Destroy; override;
  //
  procedure   SetObjectName (objectType:TVkObjectType;objectHandle:TVkUInt64;pObjectName:PVkChar);
  procedure   CmdBeginLabel (commandBuffer:TVkCommandBuffer;pLabelName:PVkChar);
  procedure   CmdEndLabel   (commandBuffer:TVkCommandBuffer);
  procedure   CmdInsertLabel(commandBuffer:TVkCommandBuffer;pLabelName:PVkChar);
  //
  procedure   ReportCallback(messageSeverity:TVkDebugUtilsMessageSeverityFlagBitsEXT;
                             messageTypes:TVkDebugUtilsMessageTypeFlagsEXT;
                             const pCallbackData:PVkDebugUtilsMessengerCallbackDataEXT
                            ); virtual;
  //
 end;

 TvSurface=class
  FHandle:TVkSurfaceKHR;
  FPFamily:TVkUInt32;
  Fformat:TVkSurfaceFormatKHR;
  FModes:array[0..2] of TVkPresentModeKHR;
  Constructor Create(Handle:THandle);
  Destructor  Destroy; override;
  Procedure   LoadFamily; virtual;
  Procedure   LoadFormat; virtual;
  Procedure   LoadModes; virtual;
  function    GetSize:TVkExtent2D; virtual;
 end;

 TSortQueueRec=record
  FIndex:TVkUInt32;
  pQueue:PVkQueue;
 end;

 PAbstractFeature=^TAbstractFeature;
 TAbstractFeature=record
  sType:TVkStructureType;
  pNext:PVkVoid;
 end;

 TvDeviceCreateInfo=class
  data:array of TSortQueueRec;
  exts:array of Pchar;
  pFeature:PVkVoid;
  procedure   add_queue(Index:TVkUInt32;Queue:PVkQueue);
  procedure   add_ext(P:Pchar);
  procedure   add_feature(P:PVkVoid);
 end;

 TvDevice=class
  FHandle:TVkDevice;
  FLock:System.TRTLCriticalSection;
  Constructor Create(Queues:TvDeviceCreateInfo);
  Destructor  Destroy; override;
  function    WaitIdle:TVkResult;
 end;

 TvQueue=class
  FHandle:TVkQueue;
  FFamily:TVkUInt32;
  FLock:System.TRTLCriticalSection;
  Constructor Create(Family:TVkUInt32);
  Destructor  Destroy; override;
  function    Submit(submitCount:TVkUInt32;const pSubmits:PVkSubmitInfo;fence:TVkFence):TVkResult;
  function    WaitIdle:TVkResult;
  function    PresentKHR(const pPresentInfo:PVkPresentInfoKHR):TVkResult;
 end;

 TvCustomCmdPool=class
  function    Alloc:TVkCommandBuffer;    virtual; abstract;
  procedure   Free(cmd:TVkCommandBuffer);virtual; abstract;
  procedure   Trim;                      virtual; abstract;
 end;

 TvCmdPool=class(TvCustomCmdPool)
  FHandle:TVkCommandPool;
  Constructor Create(FFamily:TVkUInt32);
  Destructor  Destroy;                    override;
  function    Alloc:TVkCommandBuffer;     override;
  procedure   Free(cmd:TVkCommandBuffer); override;
  procedure   Trim;                       override;
 end;

 TvFence=class
  FHandle:TVkFence;
  Constructor Create(signaled:Boolean);
  Destructor  Destroy; override;
  function    Reset:TVkResult;
  function    Wait(timeout:TVkUInt64):TVkResult;
  function    Status:TVkResult; //[VK_SUCCESS,VK_NOT_READY]
 end;

 TvSemaphore=class
  FHandle:TVkSemaphore;
  Constructor Create;
  Destructor  Destroy; override;
 end;

 TvEvent=class
  FHandle:TVkEvent;
  Constructor Create;
  Destructor  Destroy; override;
  function    SetEvent:TVkResult;
  function    ResetEvent:TVkResult;
  function    Status:TVkResult;
 end;

procedure PrintInstanceExtension;
procedure PrintDeviceExtension(physicalDevice:TVkPhysicalDevice);
procedure PrintQueueFamily(physicalDevice:TVkPhysicalDevice);

const
 SubresColor:TVkImageSubresourceRange=(
  aspectMask:TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  baseMipLevel:0;
  levelCount:1;
  baseArrayLayer:0;
  layerCount:1
 );

procedure vkImageMemoryBarrier(
	   cmdbuffer:TVkCommandBuffer;
	   image:TVkImage;
	   srcAccessMask:TVkAccessFlags;
	   dstAccessMask:TVkAccessFlags;
	   oldImageLayout:TVkImageLayout;
	   newImageLayout:TVkImageLayout;
	   srcStageMask:TVkPipelineStageFlags;
	   dstStageMask:TVkPipelineStageFlags;
	   subresourceRange:TVkImageSubresourceRange);

procedure vkBufferMemoryBarrier(
	   cmdbuffer:TVkCommandBuffer;
	   buffer:TVkBuffer;
	   srcAccessMask:TVkAccessFlags;
	   dstAccessMask:TVkAccessFlags;
           offset,size:TVkDeviceSize;
	   srcStageMask:TVkPipelineStageFlags;
	   dstStageMask:TVkPipelineStageFlags);

procedure vkMemoryBarrier(
	   cmdbuffer:TVkCommandBuffer;
	   srcAccessMask:TVkAccessFlags;
	   dstAccessMask:TVkAccessFlags;
	   srcStageMask:TVkPipelineStageFlags;
	   dstStageMask:TVkPipelineStageFlags);

{
procedure vkBarrier(
	   cmdbuffer:TVkCommandBuffer;
	   srcStageMask:TVkPipelineStageFlags;
	   dstStageMask:TVkPipelineStageFlags);
}

Procedure vkCmdBindVertexBuffer(commandBuffer:TVkCommandBuffer;
                                Binding:TVkUInt32;
                                Buffer:TVkBuffer;
                                Offset:TVkDeviceSize);

{
Procedure vkCmdBindDescriptorBuffer(commandBuffer:TVkCommandBuffer;
                                    Binding:TVkUInt32;
                                    Buffer:TVkBuffer;
                                    Offset:TVkDeviceSize);
}

Procedure vkCmdBindSB(cmd:TVkCommandBuffer;
                      point:TVkPipelineBindPoint;
                      layout:TVkPipelineLayout;
                      aSet,aBind,aElem:TVkUInt32;
                      buffer:TVkBuffer;
                      offset,range:TVkDeviceSize);

procedure vkCmdWaitEvent(commandBuffer:TVkCommandBuffer;
                         event:TVkEvent;
                         srcStageMask:TVkPipelineStageFlags;
                         dstStageMask:TVkPipelineStageFlags);


var
 VulkanDeviceGuid:TGUID;
 VulkanAppFlags  :t_vulkan_app_flags=[];

 VulkanApp:TVulkanApp;
 DebugReport:TVDebugReport;
 Device:TvDevice;

 FlipQueue:TvQueue;
 RenderQueue:TvQueue;

function  LoadVulkan:Boolean;
Procedure InitVulkan;
function  IsInitVulkan:Boolean;

function  GetPhysicalDeviceList:APhysicalDeviceProperties;

function  shaderStorageImageExtendedFormats:Boolean;
function  shaderStorageImageReadWithoutFormat:Boolean;
function  shaderStorageImageWriteWithoutFormat:Boolean;
function  shaderInt64:Boolean;
function  shaderInt16:Boolean;
function  shaderInt8:Boolean;
function  shaderFloat16:Boolean;
function  storageBuffer8Bit:Boolean;
function  uniformBuffer8Bit:Boolean;
function  storageBuffer16Bit:Boolean;
function  uniformBuffer16Bit:Boolean;
function  storageInputOutput16:Boolean;
function  sparseBinding:Boolean;
function  sparseResidencyAliased:Boolean;

var
 limits:record

  VK_KHR_swapchain                 :Boolean;
  VK_EXT_external_memory_host      :Boolean;

  VK_EXT_vertex_input_dynamic_state:Boolean;
  VK_KHR_imageless_framebuffer     :Boolean;
  VK_EXT_provoking_vertex          :Boolean;
  VK_KHR_image_format_list         :Boolean;
  VK_EXT_descriptor_indexing       :Boolean;

  VK_KHR_shader_float16_int8       :Boolean;
  VK_KHR_16bit_storage             :Boolean;
  VK_KHR_8bit_storage              :Boolean;
  VK_KHR_push_descriptor           :Boolean;
  VK_KHR_shader_non_semantic_info  :Boolean;
  VK_EXT_index_type_uint8          :Boolean;
  VK_EXT_scalar_block_layout       :Boolean;
  VK_EXT_robustness2               :Boolean;
  VK_EXT_image_view_min_lod        :Boolean;

  VK_AMD_device_coherent_memory    :Boolean;

  DeviceFeature:TVkPhysicalDeviceFeatures;

  shaderFloat16:TVkBool32;
  shaderInt8   :TVkBool32;

  storageBuffer8BitAccess          :TVkBool32;
  uniformAndStorageBuffer8BitAccess:TVkBool32;
  storagePushConstant8             :TVkBool32;

  storageBuffer16BitAccess          :TVkBool32;
  uniformAndStorageBuffer16BitAccess:TVkBool32;
  storagePushConstant16             :TVkBool32;
  storageInputOutput16              :TVkBool32;

  robustBufferAccess2:TVkBool32;
  robustImageAccess2 :TVkBool32;
  nullDescriptor     :TVkBool32;

  DescriptorIndexingFeatures:TVkPhysicalDeviceDescriptorIndexingFeatures;

  maxUniformBufferRange:TVkUInt32;
  maxStorageBufferRange:TVkUInt32;
  maxPushConstantsSize :TVkUInt32;
  maxSamplerLodBias    :TVkFloat;
  maxSamplerAnisotropy :TVkFloat;

  minMemoryMapAlignment          :TVkSize;
  minTexelBufferOffsetAlignment  :TVkDeviceSize;
  minUniformBufferOffsetAlignment:TVkDeviceSize;
  minStorageBufferOffsetAlignment:TVkDeviceSize;

  framebufferColorSampleCounts  :TVkSampleCountFlags;
  framebufferDepthSampleCounts  :TVkSampleCountFlags;
  framebufferStencilSampleCounts:TVkSampleCountFlags;

  sampledImageColorSampleCounts  :TVkSampleCountFlags;
  sampledImageIntegerSampleCounts:TVkSampleCountFlags;
  sampledImageDepthSampleCounts  :TVkSampleCountFlags;
  sampledImageStencilSampleCounts:TVkSampleCountFlags;
  storageImageSampleCounts       :TVkSampleCountFlags;

  maxComputeWorkGroupInvocations:TVkUInt32;
  maxComputeWorkGroupSize       :TVkOffset3D;

  minImportedHostPointerAlignment:TVkDeviceSize;
 end;

implementation

uses
 kern_rwlock,
 vMemory;

function shaderStorageImageExtendedFormats:Boolean;
begin
 Result:=Boolean(limits.DeviceFeature.shaderStorageImageExtendedFormats);
end;

function shaderStorageImageReadWithoutFormat:Boolean;
begin
 Result:=Boolean(limits.DeviceFeature.shaderStorageImageReadWithoutFormat);
end;

function shaderStorageImageWriteWithoutFormat:Boolean;
begin
 Result:=Boolean(limits.DeviceFeature.shaderStorageImageWriteWithoutFormat);
end;

function shaderInt64:Boolean;
begin
 Result:=Boolean(limits.DeviceFeature.shaderInt64);
end;

function shaderInt16:Boolean;
begin
 Result:=Boolean(limits.DeviceFeature.shaderInt16);
end;

function shaderInt8:Boolean;
begin
 Result:=Boolean(limits.shaderInt8);
end;

function shaderFloat16:Boolean;
begin
 Result:=Boolean(limits.shaderFloat16);
end;

function storageBuffer8Bit:Boolean;
begin
 Result:=Boolean(limits.storageBuffer8BitAccess);
end;

function uniformBuffer8Bit:Boolean;
begin
 Result:=Boolean(limits.uniformAndStorageBuffer8BitAccess);
end;

function storageBuffer16Bit:Boolean;
begin
 Result:=Boolean(limits.storageBuffer16BitAccess);
end;

function uniformBuffer16Bit:Boolean;
begin
 Result:=Boolean(limits.uniformAndStorageBuffer16BitAccess);
end;

function storageInputOutput16:Boolean;
begin
 Result:=Boolean(limits.storageInputOutput16);
end;

function sparseBinding:Boolean;
begin
 Result:=Boolean(limits.DeviceFeature.sparseBinding);
end;

function sparseResidencyAliased:Boolean;
begin
 Result:=Boolean(limits.DeviceFeature.sparseResidencyAliased);
end;

procedure FillDeviceProperties(physicalDevice:TVkPhysicalDevice);
var
 prop:TVkPhysicalDeviceProperties2;
 memh:TVkPhysicalDeviceExternalMemoryHostPropertiesEXT;
begin
 prop:=Default(TVkPhysicalDeviceProperties2);
 prop.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2;
 prop.pNext:=@memh;

 memh:=Default(TVkPhysicalDeviceExternalMemoryHostPropertiesEXT);
 memh.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT;

 vkGetPhysicalDeviceProperties2(physicalDevice,@prop);

 limits.maxUniformBufferRange          :=prop.properties.limits.maxUniformBufferRange;
 limits.maxStorageBufferRange          :=prop.properties.limits.maxStorageBufferRange;
 limits.maxPushConstantsSize           :=prop.properties.limits.maxPushConstantsSize;
 limits.maxSamplerLodBias              :=prop.properties.limits.maxSamplerLodBias;
 limits.maxSamplerAnisotropy           :=prop.properties.limits.maxSamplerAnisotropy;

 limits.minMemoryMapAlignment          :=prop.properties.limits.minMemoryMapAlignment;
 limits.minTexelBufferOffsetAlignment  :=prop.properties.limits.minTexelBufferOffsetAlignment;
 limits.minUniformBufferOffsetAlignment:=prop.properties.limits.minUniformBufferOffsetAlignment;
 limits.minStorageBufferOffsetAlignment:=prop.properties.limits.minStorageBufferOffsetAlignment;

 limits.framebufferColorSampleCounts   :=prop.properties.limits.framebufferColorSampleCounts;
 limits.framebufferDepthSampleCounts   :=prop.properties.limits.framebufferDepthSampleCounts;
 limits.framebufferStencilSampleCounts :=prop.properties.limits.framebufferStencilSampleCounts;

 limits.sampledImageColorSampleCounts  :=prop.properties.limits.sampledImageColorSampleCounts;
 limits.sampledImageIntegerSampleCounts:=prop.properties.limits.sampledImageIntegerSampleCounts;
 limits.sampledImageDepthSampleCounts  :=prop.properties.limits.sampledImageDepthSampleCounts;
 limits.sampledImageStencilSampleCounts:=prop.properties.limits.sampledImageStencilSampleCounts;
 limits.storageImageSampleCounts       :=prop.properties.limits.storageImageSampleCounts;

 limits.maxComputeWorkGroupInvocations :=prop.properties.limits.maxComputeWorkGroupInvocations;
 limits.maxComputeWorkGroupSize        :=TVkOffset3D(prop.properties.limits.maxComputeWorkGroupSize);

 limits.minImportedHostPointerAlignment:=memh.minImportedHostPointerAlignment;
end;

procedure FillDeviceExtension(physicalDevice:TVkPhysicalDevice);
var
 i,count:TVkUInt32;
 pProperties:PVkExtensionProperties;
begin
 Writeln;
 count:=0;
 vkEnumerateDeviceExtensionProperties(physicalDevice,nil,@count,nil);
 if (count<>0) then
 begin
  pProperties:=GetMem(count*SizeOf(TVkExtensionProperties));
  vkEnumerateDeviceExtensionProperties(physicalDevice,nil,@count,pProperties);
  For i:=0 to count-1 do
  begin
   Case String(pProperties[i].extensionName) of
    VK_KHR_SWAPCHAIN_EXTENSION_NAME                 :limits.VK_KHR_swapchain                 :=True;
    VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME      :limits.VK_EXT_external_memory_host      :=True;

    VK_EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME:limits.VK_EXT_vertex_input_dynamic_state:=True;
    VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME     :limits.VK_KHR_imageless_framebuffer     :=True;
    VK_EXT_PROVOKING_VERTEX_EXTENSION_NAME          :limits.VK_EXT_provoking_vertex          :=True;
    VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME         :limits.VK_KHR_image_format_list         :=True;
    VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME       :limits.VK_EXT_descriptor_indexing       :=True;

    VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME       :limits.VK_KHR_shader_float16_int8       :=True;
    VK_KHR_16BIT_STORAGE_EXTENSION_NAME             :limits.VK_KHR_16bit_storage             :=True;
    VK_KHR_8BIT_STORAGE_EXTENSION_NAME              :limits.VK_KHR_8bit_storage              :=True;
    VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME           :limits.VK_KHR_push_descriptor           :=True;
    VK_KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME  :limits.VK_KHR_shader_non_semantic_info  :=True;
    VK_EXT_INDEX_TYPE_UINT8_EXTENSION_NAME          :limits.VK_EXT_index_type_uint8          :=True;
    VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME       :limits.VK_EXT_scalar_block_layout       :=True;
    VK_EXT_ROBUSTNESS_2_EXTENSION_NAME              :limits.VK_EXT_robustness2               :=True;
    VK_EXT_IMAGE_VIEW_MIN_LOD_EXTENSION_NAME        :limits.VK_EXT_image_view_min_lod        :=True;

    VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME    :limits.VK_AMD_device_coherent_memory    :=True;
   end;
  end;
  FreeMem(pProperties);
 end;
end;

//VK_EXT_COLOR_WRITE_ENABLE_EXTENSION_NAME
//VK_KHR_MAINTENANCE_2_EXTENSION_NAME
//VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME
//VK_KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME
//VK_KHR_DYNAMIC_RENDERING_EXTENSION_NAME
//VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
//VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
//VK_EXT_DEPTH_CLIP_CONTROL_EXTENSION_NAME
//VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
//VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME
//VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
//VK_EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME

type
 TSortIndex=object
  max:Integer;
  data:array of TVkDeviceQueueCreateInfo;
  procedure add(Index:TVkUInt32);
 end;

procedure PrintInstanceExtension;
var
 i,count:TVkUInt32;
 pProperties:PVkExtensionProperties;
begin
 Writeln;
 count:=0;
 vkEnumerateInstanceExtensionProperties(nil,@count,nil);
 if (count<>0) then
 begin
  pProperties:=GetMem(count*SizeOf(TVkExtensionProperties));
  vkEnumerateInstanceExtensionProperties(nil,@count,pProperties);
  For i:=0 to count-1 do
  begin
   Writeln(pProperties[i].extensionName);
  end;
  FreeMem(pProperties);
 end;
end;

procedure PrintDeviceExtension(physicalDevice:TVkPhysicalDevice);
var
 i,count:TVkUInt32;
 pProperties:PVkExtensionProperties;
begin
 Writeln;
 count:=0;
 vkEnumerateDeviceExtensionProperties(physicalDevice,nil,@count,nil);
 if (count<>0) then
 begin
  pProperties:=GetMem(count*SizeOf(TVkExtensionProperties));
  vkEnumerateDeviceExtensionProperties(physicalDevice,nil,@count,pProperties);
  For i:=0 to count-1 do
  begin
   Writeln(pProperties[i].extensionName);
  end;
  FreeMem(pProperties);
 end;
end;

function getstr_queueFlags(queueFlags:TVkQueueFlags):RawByteString;
begin
 Result:='';
 if (queueFlags and TVkQueueFlags(VK_QUEUE_GRAPHICS_BIT))<>0 then
  Result:=Result+' GRAPHICS';
 if (queueFlags and TVkQueueFlags(VK_QUEUE_COMPUTE_BIT))<>0 then
  Result:=Result+' COMPUTE';
 if (queueFlags and TVkQueueFlags(VK_QUEUE_TRANSFER_BIT))<>0 then
  Result:=Result+' TRANSFER';
 if (queueFlags and TVkQueueFlags(VK_QUEUE_SPARSE_BINDING_BIT))<>0 then
  Result:=Result+' SPARSE_BINDING';
 if (queueFlags and TVkQueueFlags(VK_QUEUE_PROTECTED_BIT))<>0 then
  Result:=Result+' PROTECTED';
 if (queueFlags and TVkQueueFlags(VK_QUEUE_VIDEO_DECODE_BIT_KHR))<>0 then
  Result:=Result+' VIDEO_DECODE';
 if (queueFlags and TVkQueueFlags(VK_QUEUE_VIDEO_ENCODE_BIT_KHR))<>0 then
  Result:=Result+' VIDEO_ENCODE';
end;

procedure PrintQueueFamily(physicalDevice:TVkPhysicalDevice);
var
 i,count:TVkUInt32;
 pFamily:PVkQueueFamilyProperties;
begin
 count:=0;
 vkGetPhysicalDeviceQueueFamilyProperties(physicalDevice,@count,nil);
 if (count=0) then Exit;

 pFamily:=GetMem(count*SizeOf(TVkQueueFamilyProperties));
 vkGetPhysicalDeviceQueueFamilyProperties(physicalDevice,@count,pFamily);

 For i:=0 to count-1 do
 begin
  Writeln(getstr_queueFlags(pFamily[i].queueFlags),':',pFamily[i].queueCount);
 end;

 FreeMem(pFamily);
end;

function MyDebugReportCallback(messageSeverity:TVkDebugUtilsMessageSeverityFlagBitsEXT;
                               messageTypes:TVkDebugUtilsMessageTypeFlagsEXT;
                               const pCallbackData:PVkDebugUtilsMessengerCallbackDataEXT;
                               pUserData:PVkVoid):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
begin
 TVDebugReport(pUserData).ReportCallback(messageSeverity,messageTypes,pCallbackData);
 Result:=TVkBool32(False);
end;

Constructor TVDebugReport.Create;
var
 cinfo:TVkDebugUtilsMessengerCreateInfoEXT;
 r:TVkResult;
begin
 FStdOut:=StdOut;
 //
 Pointer(FCreateDebugUtilsMessenger ):=vkGetInstanceProcAddr(VulkanApp.FInstance,'vkCreateDebugUtilsMessengerEXT');
 Pointer(FDestroyDebugUtilsMessenger):=vkGetInstanceProcAddr(VulkanApp.FInstance,'vkDestroyDebugUtilsMessengerEXT');
 //
 Pointer(FSetDebugUtilsObjectName   ):=vkGetInstanceProcAddr(VulkanApp.FInstance,'vkSetDebugUtilsObjectNameEXT');
 Pointer(FCmdBeginDebugUtilsLabel   ):=vkGetInstanceProcAddr(VulkanApp.FInstance,'vkCmdBeginDebugUtilsLabelEXT');
 Pointer(FCmdEndDebugUtilsLabel     ):=vkGetInstanceProcAddr(VulkanApp.FInstance,'vkCmdEndDebugUtilsLabelEXT');
 Pointer(FCmdInsertDebugUtilsLabel  ):=vkGetInstanceProcAddr(VulkanApp.FInstance,'vkCmdInsertDebugUtilsLabelEXT');
 //
 if (FCreateDebugUtilsMessenger<>nil) then
 begin
  cinfo:=Default(TVkDebugUtilsMessengerCreateInfoEXT);
  cinfo.sType:=VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT;
  //
  cinfo.messageSeverity:=ord(VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT) or
                         ord(VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT   ) or
                         ord(VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT) or
                         ord(VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT  );
  //
  cinfo.messageType:=ord(VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT    ) or
                     ord(VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT ){ or
                     ord(VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT)};
  //
  cinfo.pfnUserCallback:=@MyDebugReportCallback;
  cinfo.pUserData:=Pointer(Self);
  //
  r:=FCreateDebugUtilsMessenger(VulkanApp.FInstance,@cinfo,nil,@FHandle);
  if (r<>VK_SUCCESS) then
  begin
   Writeln(StdErr,'CreateDebugReportCallback:',r);
   Exit;
  end;
 end;
end;

Destructor TVDebugReport.Destroy;
begin
 if (FDestroyDebugUtilsMessenger<>nil) then
 begin
  FDestroyDebugUtilsMessenger(VulkanApp.FInstance,FHandle,nil);
 end;
end;

procedure TVDebugReport.SetObjectName(objectType:TVkObjectType;objectHandle:TVkUInt64;pObjectName:PVkChar);
var
 info:TVkDebugUtilsObjectNameInfoEXT;
begin
 if (FSetDebugUtilsObjectName<>nil) and (objectHandle<>VK_NULL_HANDLE) then
 begin
  info:=Default(TVkDebugUtilsObjectNameInfoEXT);
  info.sType:=VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT;
  info.objectType  :=objectType;
  info.objectHandle:=objectHandle;
  info.pObjectName :=pObjectName;
  //
  FSetDebugUtilsObjectName(Device.FHandle,@info);
 end;
end;

procedure TVDebugReport.CmdBeginLabel(commandBuffer:TVkCommandBuffer;pLabelName:PVkChar);
var
 info:TVkDebugUtilsLabelEXT;
begin
 if (FCmdBeginDebugUtilsLabel<>nil) and (commandBuffer<>VK_NULL_HANDLE) then
 begin
  info:=Default(TVkDebugUtilsLabelEXT);
  info.sType:=VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT;
  info.pLabelName:=pLabelName;
  //
  FCmdBeginDebugUtilsLabel(commandBuffer,@info);
 end;
end;

procedure TVDebugReport.CmdEndLabel(commandBuffer:TVkCommandBuffer);
begin
 if (FCmdEndDebugUtilsLabel<>nil) and (commandBuffer<>VK_NULL_HANDLE) then
 begin
  FCmdEndDebugUtilsLabel(commandBuffer);
 end;
end;

procedure TVDebugReport.CmdInsertLabel(commandBuffer:TVkCommandBuffer;pLabelName:PVkChar);
var
 info:TVkDebugUtilsLabelEXT;
begin
 if (FCmdInsertDebugUtilsLabel<>nil) and (commandBuffer<>VK_NULL_HANDLE) then
 begin
  info:=Default(TVkDebugUtilsLabelEXT);
  info.sType:=VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT;
  info.pLabelName:=pLabelName;
  //
  FCmdInsertDebugUtilsLabel(commandBuffer,@info);
 end;
end;

procedure TVDebugReport.ReportCallback(messageSeverity:TVkDebugUtilsMessageSeverityFlagBitsEXT;
                                       messageTypes:TVkDebugUtilsMessageTypeFlagsEXT;
                                       const pCallbackData:PVkDebugUtilsMessengerCallbackDataEXT
                                      );
//var
 //i:Integer;
begin

 if Pos('not consumed by fragment shader',pCallbackData^.pMessage)<>0 then Exit;
 if Pos('fragment shader writes to output location 0 with no matching attachment',pCallbackData^.pMessage)<>0 then Exit;

 Writeln(FStdOut,pCallbackData^.pMessage);

 {
 sType:TVkStructureType;
 pNext:PVkVoid;
 flags:TVkDebugUtilsMessengerCallbackDataFlagsEXT;
 pMessageIdName:PVkChar;
 messageIdNumber:TVkInt32;
 pMessage:PVkChar;
 queueLabelCount:TVkUInt32;
 pQueueLabels:PVkDebugUtilsLabelEXT;
 cmdBufLabelCount:TVkUInt32;
 pCmdBufLabels:PVkDebugUtilsLabelEXT;
 objectCount:TVkUInt32;
 pObjects:PVkDebugUtilsObjectNameInfoEXT;
 }

 {
 Case objectType of

  VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT:
   Case DWORD(location) of
    $0609A13B:
     begin
      if Pos('not consumed by fragment shader',pMessage)<>0 then Exit;
      if Pos('fragment shader writes to output location 0 with no matching attachment',pMessage)<>0 then Exit;
     end;
   end;

  VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT:
   Case DWORD(location) of

    $A7BB8DB6:if Pos('(Float16)',pMessage)<>0 then Exit;

    $92394C89:
     begin
      i:=Pos('|',pMessage);
      if (i<>0) then
      begin
       pMessage:=@pMessage[i];
       i:=Pos('|',pMessage);
       if (i<>0) then
       begin
        pMessage:=@pMessage[i-1];
       end;
      end;
     end;

    else;
   end;
  else;
 end;

 Writeln(pMessage);
 }

end;

function vkGetPhysicalDevice4Type(pPhysicalDevices:PVkPhysicalDevice;count:TVkUInt32;deviceType:TVkPhysicalDeviceType):TVkPhysicalDevice;
var
 i:TVkUInt32;
 deviceProperties:TVkPhysicalDeviceProperties;
begin
 Result:=VK_NULL_HANDLE;
 if (count<>0) then
 For i:=0 to count-1 do
 begin
  deviceProperties:=Default(TVkPhysicalDeviceProperties);
  vkGetPhysicalDeviceProperties(pPhysicalDevices[i],@deviceProperties);
  if (deviceProperties.deviceType=deviceType) then Exit(pPhysicalDevices[i]);
 end;
end;

function vkGetPhysicalDevice4Guid(pPhysicalDevices:PVkPhysicalDevice;count:TVkUInt32;Guid:TGUID):TVkPhysicalDevice;
var
 i:TVkUInt32;
 deviceProperties:TVkPhysicalDeviceProperties;
begin
 Result:=VK_NULL_HANDLE;
 if (count<>0) then
 For i:=0 to count-1 do
 begin
  deviceProperties:=Default(TVkPhysicalDeviceProperties);
  vkGetPhysicalDeviceProperties(pPhysicalDevices[i],@deviceProperties);
  if CompareByte(deviceProperties.pipelineCacheUUID,Guid,SizeOf(TGUID))=0 then
  begin
   Exit(pPhysicalDevices[i]);
  end;
 end;
end;

function vkGetPhysicalDevice(vkInstance:TVkInstance):TVkPhysicalDevice;
var
 i,count:TVkUInt32;
 pPhysicalDevices:PVkPhysicalDevice;
 deviceProperties:TVkPhysicalDeviceProperties;
begin
 Result:=VK_NULL_HANDLE;
 count:=0;
 vkEnumeratePhysicalDevices(vkInstance,@count,nil);
 if (count=0) then Exit;
 //
 pPhysicalDevices:=GetMem(count*SizeOf(TVkPhysicalDevice));
 vkEnumeratePhysicalDevices(vkInstance,@count,pPhysicalDevices);
 //
 For i:=0 to count-1 do
 begin
  deviceProperties:=Default(TVkPhysicalDeviceProperties);
  vkGetPhysicalDeviceProperties(pPhysicalDevices[i],@deviceProperties);
  Writeln(deviceProperties.deviceName);
  Writeln('apiVersion:',VK_VERSION_MAJOR(deviceProperties.apiVersion),'.',
                        VK_VERSION_MINOR(deviceProperties.apiVersion),'.',
                        VK_VERSION_PATCH(deviceProperties.apiVersion));
  Writeln('-----------');
 end;
 //
 Result:=vkGetPhysicalDevice4Guid(pPhysicalDevices,count,VulkanDeviceGuid);
 //
 if (Result=VK_NULL_HANDLE) then
 begin
  Result:=vkGetPhysicalDevice4Type(pPhysicalDevices,count,VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU);
  //
  if (Result=VK_NULL_HANDLE) then
  begin
   Result:=vkGetPhysicalDevice4Type(pPhysicalDevices,count,VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU);
  end;
 end;
 //
 if (Result=VK_NULL_HANDLE) then
 if (count>0) then
 begin
  Result:=pPhysicalDevices[0];
 end;
 //
 FreeMem(pPhysicalDevices);

 Writeln('Select GPU:');
 deviceProperties:=Default(TVkPhysicalDeviceProperties);
 vkGetPhysicalDeviceProperties(Result,@deviceProperties);
 Writeln(' ',deviceProperties.deviceName);
 Writeln(' apiVersion:',VK_VERSION_MAJOR(deviceProperties.apiVersion),'.',
                        VK_VERSION_MINOR(deviceProperties.apiVersion),'.',
                        VK_VERSION_PATCH(deviceProperties.apiVersion));

end;

function GetInstanceExtensionNames:TExtensionNames;
begin
 Result:=nil;
 SetLength(Result,4);
 Result[0]:=VK_KHR_SURFACE_EXTENSION_NAME;
 Result[1]:=VK_KHR_WIN32_SURFACE_EXTENSION_NAME;
 Result[2]:=VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME;
 Result[3]:=VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME;
end;

function GetVkApplicationInfo:TVkApplicationInfo;
begin
 Result:=Default(TVkApplicationInfo);
 Result.sType             :=VK_STRUCTURE_TYPE_APPLICATION_INFO;
 Result.pApplicationName  :='fpPS4';
 Result.applicationVersion:=VK_MAKE_VERSION(0, 0, 1);
 Result.pEngineName       :='fpPS4';
 Result.engineVersion     :=VK_MAKE_VERSION(0, 0, 1);
 Result.apiVersion        :=VK_MAKE_API_VERSION(0, 1, 1, 0); //VK_API_VERSION_1_1
end;

function GetPhysicalDeviceList(vkInstance:TVkInstance):APhysicalDeviceProperties;
var
 i,count:TVkUInt32;
 pPhysicalDevices:PVkPhysicalDevice;
begin
 Result:=nil;
 count:=0;
 vkEnumeratePhysicalDevices(vkInstance,@count,nil);
 if (count=0) then Exit;
 //
 pPhysicalDevices:=GetMem(count*SizeOf(TVkPhysicalDevice));
 SetLength(Result,count);
 //
 vkEnumeratePhysicalDevices(vkInstance,@count,pPhysicalDevices);
 For i:=0 to count-1 do
 begin
  Result[i]:=Default(TVkPhysicalDeviceProperties);
  vkGetPhysicalDeviceProperties(pPhysicalDevices[i],@Result[i]);
 end;
 //
 FreeMem(pPhysicalDevices);
end;

function GetPhysicalDeviceList:APhysicalDeviceProperties;
var
 FInstance:TVkInstance;
 vkApp    :TVkApplicationInfo;
 vkExtList:TExtensionNames;
 vkCInfo  :TVkInstanceCreateInfo;
 r:TVkResult;
begin
 Result:=nil;

 FInstance:=VK_NULL_HANDLE;

 vkApp:=GetVkApplicationInfo;

 vkExtList:=GetInstanceExtensionNames;

 vkCInfo:=Default(TVkInstanceCreateInfo);
 vkCInfo.sType:=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
 vkCInfo.pApplicationInfo:=@vkApp;

 vkCInfo.enabledExtensionCount  :=Length(vkExtList);
 vkCInfo.ppEnabledExtensionNames:=@vkExtList[0];

 Writeln('vkCreateInstance->');
 r:=vkCreateInstance(@vkCInfo,nil,@FInstance);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateInstance:',r);
  Exit;
 end;
 Writeln('<-vkCreateInstance');

 Result:=GetPhysicalDeviceList(FInstance);

 if (FInstance<>VK_NULL_HANDLE) then
 begin
  vkDestroyInstance(FInstance,nil);
 end;

end;

type
 AVkValidationFeatureEnable =array of TVkValidationFeatureEnableEXT;
 AVkValidationFeatureDisable=array of TVkValidationFeatureDisableEXT;

function GetEnabledFeatures(flags:t_vulkan_app_flags):AVkValidationFeatureEnable;
begin
 Result:=nil;
 if (va_enable_gpu_assisted         in flags) then Insert(VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT                     ,Result,Length(Result));
 if (va_enable_gpu_assisted_reserve in flags) then Insert(VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT,Result,Length(Result));
 if (va_enable_best_practices       in flags) then Insert(VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT                   ,Result,Length(Result));
 if (va_enable_debug_printf         in flags) then Insert(VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT                     ,Result,Length(Result));
 if (va_enable_sync_validation      in flags) then Insert(VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT       ,Result,Length(Result));
end;

function GetDisabledFeatures(flags:t_vulkan_app_flags):AVkValidationFeatureDisable;
begin
 Result:=nil;
 if (va_disable_shaders           in flags) then Insert(VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT                ,Result,Length(Result));
 if (va_disable_thread_safety     in flags) then Insert(VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT          ,Result,Length(Result));
 if (va_disable_api_params        in flags) then Insert(VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT         ,Result,Length(Result));
 if (va_disable_obj_lifetimes     in flags) then Insert(VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT       ,Result,Length(Result));
 if (va_disable_core_checks       in flags) then Insert(VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT            ,Result,Length(Result));
 if (va_disable_unique_handles    in flags) then Insert(VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT         ,Result,Length(Result));
 if (va_disable_shader_validation in flags) then Insert(VK_VALIDATION_FEATURE_DISABLE_SHADER_VALIDATION_CACHE_EXT,Result,Length(Result));
end;

Constructor TVulkanApp.Create(flags:t_vulkan_app_flags);
const
 dlayer='VK_LAYER_KHRONOS_validation';
var
 vkApp     :TVkApplicationInfo;
 vkExtList :TExtensionNames;
 vkLayer   :array[0..0] of PChar;
 vkCInfo   :TVkInstanceCreateInfo;
 vkEnabled :AVkValidationFeatureEnable;
 vkDisabled:AVkValidationFeatureDisable;
 vkFeatures:TVkValidationFeaturesEXT;
 Features2 :TVkPhysicalDeviceFeatures2;
 F16_8     :TVkPhysicalDeviceShaderFloat16Int8Features;
 FSF_8     :TVkPhysicalDevice8BitStorageFeatures;
 FSF16     :TVkPhysicalDevice16BitStorageFeatures;
 FRF       :TVkPhysicalDeviceRobustness2FeaturesEXT;
 FDI       :TVkPhysicalDeviceDescriptorIndexingFeatures;
 r:TVkResult;
begin
 vkApp:=GetVkApplicationInfo;

 vkExtList:=GetInstanceExtensionNames;

 vkCInfo:=Default(TVkInstanceCreateInfo);
 vkCInfo.sType:=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
 vkCInfo.pApplicationInfo:=@vkApp;

 if (va_debug_utils in flags) then
 begin
  //VK_EXT_debug_utils
  Insert(VK_EXT_DEBUG_UTILS_EXTENSION_NAME,vkExtList,Length(vkExtList));

  if (va_validation_layer in flags) and InstanceLayersIsExist(dlayer) then
  begin
   vkLayer[0]:=dlayer;
   vkCInfo.enabledLayerCount  :=1;
   vkCInfo.ppEnabledLayerNames:=@vkLayer;
  end;
 end;

 vkCInfo.enabledExtensionCount  :=Length(vkExtList);
 vkCInfo.ppEnabledExtensionNames:=@vkExtList[0];

 if (va_debug_utils in flags) then
 begin
  vkEnabled :=GetEnabledFeatures (flags);
  vkDisabled:=GetDisabledFeatures(flags);

  if (Length(vkEnabled)<>0) or
     (Length(vkDisabled)<>0) then
  begin
   vkFeatures:=Default(TVkValidationFeaturesEXT);
   vkFeatures.sType:=VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT;

   vkFeatures.enabledValidationFeatureCount :=Length(vkEnabled);
   vkFeatures.pEnabledValidationFeatures    :=@vkEnabled[0];
   vkFeatures.disabledValidationFeatureCount:=Length(vkDisabled);
   vkFeatures.pDisabledValidationFeatures   :=@vkDisabled[0];

   vkCInfo.pNext:=@vkFeatures;
  end;
 end;

 Writeln('vkCreateInstance->');
 r:=vkCreateInstance(@vkCInfo,nil,@FInstance);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateInstance:',r);
  Exit;
 end;
 Writeln('<-vkCreateInstance');

 FPhysicalDevice:=vkGetPhysicalDevice(FInstance);
 if (FPhysicalDevice=VK_NULL_HANDLE) then
 begin
  Writeln(StdErr,'failed to choice vulkan GPU');
  halt;
 end;

 limits.DeviceFeature:=Default(TVkPhysicalDeviceFeatures);
 F16_8:=Default(TVkPhysicalDeviceShaderFloat16Int8Features);
 FSF_8:=Default(TVkPhysicalDevice8BitStorageFeatures);
 FSF16:=Default(TVkPhysicalDevice16BitStorageFeatures);
 FRF  :=Default(TVkPhysicalDeviceRobustness2FeaturesEXT);
 FDI  :=Default(TVkPhysicalDeviceDescriptorIndexingFeatures);

 if (vkGetPhysicalDeviceFeatures2<>nil) then
 begin
  Features2:=Default(TVkPhysicalDeviceFeatures2);

  Features2.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2;
  Features2.pNext:=@F16_8;

  F16_8.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES;
  F16_8.pNext:=@FSF_8;

  FSF_8.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES;
  FSF_8.pNext:=@FSF16;

  FSF16.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES;
  FSF16.pNext:=@FRF;

  FRF.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT;
  FRF.pNext:=@FDI;

  FDI.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES;

  vkGetPhysicalDeviceFeatures2(FPhysicalDevice,@Features2);

  limits.DeviceFeature:=Features2.features;

 end else
 begin
  vkGetPhysicalDeviceFeatures(FPhysicalDevice,@limits.DeviceFeature);
 end;

 limits.shaderFloat16                     :=F16_8.shaderFloat16;
 limits.shaderInt8                        :=F16_8.shaderInt8;

 limits.storageBuffer8BitAccess           :=FSF_8.storageBuffer8BitAccess;
 limits.uniformAndStorageBuffer8BitAccess :=FSF_8.uniformAndStorageBuffer8BitAccess;
 limits.storagePushConstant8              :=FSF_8.storagePushConstant8;

 limits.storageBuffer16BitAccess          :=FSF16.storageBuffer16BitAccess;
 limits.uniformAndStorageBuffer16BitAccess:=FSF16.uniformAndStorageBuffer16BitAccess;
 limits.storagePushConstant16             :=FSF16.storagePushConstant16;
 limits.storageInputOutput16              :=FSF16.storageInputOutput16;

 limits.robustBufferAccess2               :=FRF.robustBufferAccess2;
 limits.robustImageAccess2                :=FRF.robustImageAccess2;
 limits.nullDescriptor                    :=FRF.nullDescriptor;

 FDI.pNext:=nil;
 limits.DescriptorIndexingFeatures        :=FDI;

 LoadFamily;
end;

function TVulkanApp.InstanceLayersIsExist(P:PChar):Boolean;
var
 l1,l2,i,count:TVkUInt32;
 pProperties:PVkLayerProperties;
begin
 Result:=False;
 l1:=StrLen(P);
 count:=0;
 vkEnumerateInstanceLayerProperties(@count,nil);
 if (l1<>0) and (count<>0) then
 begin
  pProperties:=GetMem(count*SizeOf(TVkLayerProperties));
  vkEnumerateInstanceLayerProperties(@count,pProperties);
  For i:=0 to count-1 do
  begin
   l2:=StrLen(pProperties[i].layerName);
   if (l1=l2) and (CompareByte(pProperties[i].layerName,P^,l1)=0) then
   begin
    FreeMem(pProperties);
    Exit(true);
   end;
  end;
  FreeMem(pProperties);
 end;
end;

Destructor TVulkanApp.Destroy;
begin
 vkDestroyInstance(FInstance,nil);
end;

Procedure TVulkanApp.LoadFamily;
var
 i,count:TVkUInt32;
 pQueue:PVkQueueFamilyProperties;
 bLoaded:Set of (gLoad,cLoad,tLoad);
begin
 count:=0;
 vkGetPhysicalDeviceQueueFamilyProperties(FPhysicalDevice,@count,nil);
 if (count=0) then Exit;
 bLoaded:=[];
 pQueue:=GetMem(count*SizeOf(TVkQueueFamilyProperties));
 vkGetPhysicalDeviceQueueFamilyProperties(FPhysicalDevice,@count,pQueue);
 For i:=0 to count-1 do
 begin
  if (pQueue[i].queueFlags and ord(VK_QUEUE_GRAPHICS_BIT))<>0 then
  begin
   if not (gLoad in bLoaded) then
   begin
    FGFamily:=i;
    FGFamilyCount:=pQueue[i].queueCount;
    bLoaded:=bLoaded+[gLoad];
   end;
  end else
  if (pQueue[i].queueFlags and ord(VK_QUEUE_COMPUTE_BIT))<>0 then
  begin
   if not (cLoad in bLoaded) then
   begin
    FCFamily:=i;
    FCFamilyCount:=pQueue[i].queueCount;
    bLoaded:=bLoaded+[cLoad];
   end;
  end else
  if (pQueue[i].queueFlags and ord(VK_QUEUE_TRANSFER_BIT))<>0 then
  begin
   if not (tLoad in bLoaded) then
   begin
    FTFamily:=i;
    FTFamilyCount:=pQueue[i].queueCount;
    bLoaded:=bLoaded+[tLoad];
   end;
  end;
 end;
 if not (cLoad in bLoaded) then
 begin
  For i:=0 to count-1 do
  if (pQueue[i].queueFlags and ord(VK_QUEUE_COMPUTE_BIT))<>0 then
  begin
   FCFamily:=i;
   FCFamilyCount:=pQueue[i].queueCount;
   Break;
  end;
 end;
 if not (tLoad in bLoaded) then
 begin
  For i:=0 to count-1 do
  if (pQueue[i].queueFlags and ord(VK_QUEUE_TRANSFER_BIT))<>0 then
  begin
   FTFamily:=i;
   FTFamilyCount:=pQueue[i].queueCount;
   Break;
  end;
 end;
 FreeMem(pQueue);
end;

function vkGetQueuePresentFamily(physicalDevice:TVkPhysicalDevice;Surface:TVkSurfaceKHR;var Family:TVkUInt32):Boolean;
var
 i,count:TVkUInt32;
 presentSupport:TVkBool32;
begin
 Result:=False;
 count:=0;
 vkGetPhysicalDeviceQueueFamilyProperties(physicalDevice,@count,nil);
 if (count=0) then Exit;
 For i:=0 to count-1 do
 begin
  presentSupport:=ord(false);
  vkGetPhysicalDeviceSurfaceSupportKHR(physicalDevice,i,Surface,@presentSupport);
  if boolean(presentSupport) then
  begin
   Family:=i;
   Exit(True);
  end;
 end;
end;

Constructor TVSurface.Create(Handle:THandle);
var
 ci:TVkWin32SurfaceCreateInfoKHR;
 r:TVkResult;
begin
 ci:=Default(TVkWin32SurfaceCreateInfoKHR);
 ci.sType     :=VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
 ci.hwnd_     :=TVkHWND(Handle);
 ci.hinstance_:=System.HINSTANCE;
 r:=vkCreateWin32SurfaceKHR(VulkanApp.FInstance,@ci,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateWin32SurfaceKHR:',r);
  Exit;
 end;
 LoadFamily;
 LoadFormat;
 LoadModes;
end;

Destructor TVSurface.Destroy;
begin
 vkDestroySurfaceKHR(VulkanApp.FInstance,FHandle,nil);
end;

Procedure TVSurface.LoadFamily;
begin
 FPFamily:=0;
 if not vkGetQueuePresentFamily(VulkanApp.FPhysicalDevice,FHandle,FPFamily) then
 begin
  Writeln(StdErr,'failed to chouse QueuePresentFamily');
  Exit;
 end;
end;

Procedure TVSurface.LoadFormat;
var
 count:TVkUInt32;
 formats:PVkSurfaceFormatKHR;
 r:TVkResult;

 function TryFind(format:TVkFormat;var Fformat:TVkSurfaceFormatKHR):Boolean;
 var
  i:TVkUInt32;
 begin
  Result:=false;
  For i:=0 to count-1 do
  begin
   if (formats[i].format=format) then
   begin
    Fformat:=formats[i];
    Exit(true);
   end;
  end;
 end;

begin
 count:=0;
 r:=vkGetPhysicalDeviceSurfaceFormatsKHR(VulkanApp.FPhysicalDevice,FHandle,@count,nil);
 if (r=VK_SUCCESS) and (count<>0) then
 begin
  repeat
   formats:=AllocMem(count*SizeOf(TVkSurfaceFormatKHR));
   vkGetPhysicalDeviceSurfaceFormatsKHR(VulkanApp.FPhysicalDevice,FHandle,@count,formats);

   if TryFind(VK_FORMAT_B8G8R8A8_SRGB        ,Fformat) then Break;
   if TryFind(VK_FORMAT_B8G8R8A8_UNORM       ,Fformat) then Break;

   if TryFind(VK_FORMAT_R8G8B8A8_SRGB        ,Fformat) then Break;
   if TryFind(VK_FORMAT_R8G8B8A8_UNORM       ,Fformat) then Break;

   if TryFind(VK_FORMAT_A8B8G8R8_SRGB_PACK32 ,Fformat) then Break;
   if TryFind(VK_FORMAT_A8B8G8R8_UNORM_PACK32,Fformat) then Break;

   if (Fformat.format=VK_FORMAT_UNDEFINED) then
   begin
    Fformat:=formats[0];
   end;

  until true;
  FreeMem(formats);
  Writeln('VSurfaceFormat:',Fformat.format);
 end;
end;

Procedure TVSurface.LoadModes;
var
 i,count:TVkUInt32;
 presentModes:PVkPresentModeKHR;
 r:TVkResult;
begin
 count:=0;
 FModes[0]:=VK_PRESENT_MODE_IMMEDIATE_KHR;
 FModes[1]:=VK_PRESENT_MODE_FIFO_KHR;
 FModes[2]:=VK_PRESENT_MODE_FIFO_KHR;
 r:=vkGetPhysicalDeviceSurfacePresentModesKHR(VulkanApp.FPhysicalDevice,FHandle,@count,nil);
 if (r=VK_SUCCESS) and (count<>0) then
 begin
  presentModes:=GetMem(count*SizeOf(TVkPresentModeKHR));
  vkGetPhysicalDeviceSurfacePresentModesKHR(VulkanApp.FPhysicalDevice,FHandle,@count,presentModes);
  For i:=0 to count-1 do
  begin
   if (presentModes[i]=VK_PRESENT_MODE_MAILBOX_KHR) then
   begin
    FModes[2]:=VK_PRESENT_MODE_MAILBOX_KHR;
    Break;
   end;
  end;
  FreeMem(presentModes);
 end;
end;

function TVSurface.GetSize:TVkExtent2D;
var
 Fcap:TVkSurfaceCapabilitiesKHR;
begin
 Fcap:=Default(TVkSurfaceCapabilitiesKHR);
 vkGetPhysicalDeviceSurfaceCapabilitiesKHR(VulkanApp.FPhysicalDevice,FHandle,@Fcap);
 Result:=Fcap.currentExtent;
end;

procedure TvDeviceCreateInfo.add_queue(Index:TVkUInt32;Queue:PVkQueue);
var
 i,count:Integer;
 r:Boolean;
begin
 count:=Length(data);
 if (count<>0) then
 begin
  r:=false;
  For i:=0 to count-1 do
  begin
   if (data[i].FIndex=Index) then
   begin
    r:=true;
   end else
   if r then
   begin
    SetLength(data,count+1);
    Move(data[i],data[i+1],SizeOf(TSortQueueRec)*(count-i));
    data[i].FIndex:=Index;
    data[i].pQueue:=Queue;
    Exit;
   end;
  end;
 end;
 SetLength(data,count+1);
 data[count].FIndex:=Index;
 data[count].pQueue:=Queue;
end;

procedure TvDeviceCreateInfo.add_ext(P:Pchar);
var
 i:Integer;
begin
 i:=Length(exts);
 SetLength(exts,i+1);
 exts[i]:=P;
end;

procedure TvDeviceCreateInfo.add_feature(P:PVkVoid);
begin
 PAbstractFeature(P)^.pNext:=pFeature;
 pFeature:=P;
end;

procedure TSortIndex.add(Index:TVkUInt32);
var
 i,count:Integer;
begin
 count:=Length(data);
 if (count<>0) then
  For i:=0 to count-1 do
   if (data[i].queueFamilyIndex=Index) then
   begin
    Inc(data[i].queueCount);
    if (max<data[i].queueCount) then max:=data[i].queueCount;
    Exit;
   end;
 SetLength(data,count+1);
 data[count]:=Default(TVkDeviceQueueCreateInfo);
 data[count].sType           :=VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
 data[count].queueFamilyIndex:=Index;
 data[count].queueCount      :=1;
 if (max<1) then max:=1;
end;

Constructor TvDevice.Create(Queues:TvDeviceCreateInfo);
Var
 DeviceFeature:TVkPhysicalDeviceFeatures;
 SortIndex:TSortIndex;
 Priority:array of Single;
 DeviceInfo:TVkDeviceCreateInfo;
 r:TVkResult;
 i,p,w:Integer;
begin
 System.InitCriticalSection(FLock);

 DeviceFeature:=limits.DeviceFeature;
 DeviceFeature.robustBufferAccess:=VK_FALSE;

 DeviceInfo:=Default(TVkDeviceCreateInfo);
 DeviceInfo.sType:=VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
 DeviceInfo.pEnabledFeatures:=@DeviceFeature;
 DeviceInfo.pNext:=Queues.pFeature;

 DeviceInfo.enabledExtensionCount:=Length(Queues.exts);
 if (DeviceInfo.enabledExtensionCount<>0) then
 begin
  DeviceInfo.ppEnabledExtensionNames:=@Queues.exts[0];
 end;

 SortIndex:=Default(TSortIndex);
 For i:=0 to Length(Queues.data)-1 do
 begin
  SortIndex.add(Queues.data[i].FIndex);
 end;

 DeviceInfo.queueCreateInfoCount:=Length(SortIndex.data);
 DeviceInfo.pQueueCreateInfos:=@SortIndex.data[0];

 Priority:=nil;
 SetLength(Priority,SortIndex.max);
 For i:=0 to High(Priority) do Priority[i]:=1;

 For i:=0 to High(SortIndex.data) do
 begin
  SortIndex.data[i].pQueuePriorities:=@Priority[0];
 end;

 r:=vkCreateDevice(VulkanApp.FPhysicalDevice,@DeviceInfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateDevice:',r);
  Exit;
 end;

 p:=0;
 For i:=0 to DeviceInfo.queueCreateInfoCount-1 do
  For w:=0 to SortIndex.data[i].queueCount-1 do
  begin
   vkGetDeviceQueue(FHandle,SortIndex.data[i].queueFamilyIndex,w,Queues.data[p].pQueue);
   Inc(p);
  end;
end;

Destructor TvDevice.Destroy;
begin
 System.DoneCriticalSection(FLock);
 vkDestroyDevice(FHandle,nil);
 inherited;
end;

function TvDevice.WaitIdle:TVkResult;
begin
 System.EnterCriticalSection(FLock);
 Result:=vkDeviceWaitIdle(FHandle);
 System.LeaveCriticalSection(FLock);
end;

//

Constructor TvQueue.Create(Family:TVkUInt32);
begin
 FFamily:=Family;
 System.InitCriticalSection(FLock);
end;

Destructor TvQueue.Destroy;
begin
 System.DoneCriticalSection(FLock);
 inherited;
end;

function TvQueue.Submit(submitCount:TVkUInt32;const pSubmits:PVkSubmitInfo;fence:TVkFence):TVkResult;
begin
 System.EnterCriticalSection(FLock);
 Result:=vkQueueSubmit(FHandle,submitCount,pSubmits,fence);
 System.LeaveCriticalSection(FLock);
end;

function TvQueue.WaitIdle:TVkResult;
begin
 System.EnterCriticalSection(FLock);
 Result:=vkQueueWaitIdle(FHandle);
 System.LeaveCriticalSection(FLock);
end;

function TvQueue.PresentKHR(const pPresentInfo:PVkPresentInfoKHR):TVkResult;
begin
 System.EnterCriticalSection(FLock);
 Result:=vkQueuePresentKHR(FHandle,pPresentInfo);
 System.LeaveCriticalSection(FLock);
end;

//

Constructor TvCmdPool.Create(FFamily:TVkUInt32);
var
 cinfo:TVkCommandPoolCreateInfo;
 r:TVkResult;
begin
 cinfo:=Default(TVkCommandPoolCreateInfo);
 cinfo.sType           :=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
 cinfo.queueFamilyIndex:=FFamily;
 cinfo.flags:=ord(VK_COMMAND_POOL_CREATE_TRANSIENT_BIT){ or
              ord(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT)};
 r:=vkCreateCommandPool(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateCommandPool:',r);
  exit;
 end;
end;

Destructor TvCmdPool.Destroy;
begin
 vkDestroyCommandPool(Device.FHandle,FHandle,nil);
end;

function TvCmdPool.Alloc:TVkCommandBuffer;
var
 ainfo:TVkCommandBufferAllocateInfo;
 r:TVkResult;
begin
 ainfo:=Default(TVkCommandBufferAllocateInfo);
 ainfo.sType      :=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
 ainfo.commandPool:=FHandle;
 ainfo.level      :=VK_COMMAND_BUFFER_LEVEL_PRIMARY;
 ainfo.commandBufferCount:=1;
 Result:=VK_NULL_HANDLE;
 r:=vkAllocateCommandBuffers(Device.FHandle,@ainfo,@Result);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkAllocateCommandBuffers:',r);
  Exit;
 end;
end;

procedure TvCmdPool.Free(cmd:TVkCommandBuffer);
begin
 if (cmd=VK_NULL_HANDLE) then Exit;
 {
  It is recommended to use vkQueueWaitIdle,
   it seems that vkWaitForFences does not always ensure
   the safety of free the cmd buffer
 }
 vkFreeCommandBuffers(Device.FHandle,FHandle,1,@cmd);
end;

procedure TvCmdPool.Trim;
begin
 vkTrimCommandPool(Device.FHandle,FHandle,0);
end;

//

Constructor TvFence.Create(signaled:Boolean);
var
 cinfo:TVkFenceCreateInfo;
 r:TVkResult;
begin
 cinfo:=Default(TVkFenceCreateInfo);
 cinfo.sType:=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
 if signaled then cinfo.flags:=ord(VK_FENCE_CREATE_SIGNALED_BIT);
 r:=vkCreateFence(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateFence:',r);
  Exit;
 end;
end;

Destructor TvFence.Destroy;
begin
 vkDestroyFence(Device.FHandle,FHandle,nil);
end;

function TvFence.Reset:TVkResult;
begin
 Result:=vkResetFences(Device.FHandle,1,@FHandle);
end;

function TvFence.Wait(timeout:TVkUInt64):TVkResult;
begin
 Result:=vkWaitForFences(Device.FHandle,1,@FHandle,VK_TRUE,timeout);
end;

function TvFence.Status:TVkResult;
begin
 Result:=vkGetFenceStatus(Device.FHandle,FHandle);
end;

//

Constructor TvSemaphore.Create;
var
 cinfo:TVkSemaphoreCreateInfo;
begin
 cinfo:=Default(TVkSemaphoreCreateInfo);
 cinfo.sType:=VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
 vkCreateSemaphore(Device.FHandle,@cinfo,nil,@FHandle);
end;

Destructor TvSemaphore.Destroy;
begin
 vkDestroySemaphore(Device.FHandle,FHandle,nil);
end;

//

Constructor TvEvent.Create;
var
 cinfo:TVkEventCreateInfo;
begin
 cinfo:=Default(TVkEventCreateInfo);
 cinfo.sType:=VK_STRUCTURE_TYPE_EVENT_CREATE_INFO;
 vkCreateEvent(Device.FHandle,@cinfo,nil,@FHandle);
end;

Destructor TvEvent.Destroy;
begin
 vkDestroyEvent(Device.FHandle,FHandle,nil);
end;

function TvEvent.SetEvent:TVkResult;
begin
 Result:=vkSetEvent(Device.FHandle,FHandle);
end;

function TvEvent.ResetEvent:TVkResult;
begin
 Result:=vkResetEvent(Device.FHandle,FHandle);
end;

function TvEvent.Status:TVkResult;
begin
 Result:=vkGetEventStatus(Device.FHandle,FHandle);
end;

//

procedure vkImageMemoryBarrier(
	   cmdbuffer:TVkCommandBuffer;
	   image:TVkImage;
	   srcAccessMask:TVkAccessFlags;
	   dstAccessMask:TVkAccessFlags;
	   oldImageLayout:TVkImageLayout;
	   newImageLayout:TVkImageLayout;
	   srcStageMask:TVkPipelineStageFlags;
	   dstStageMask:TVkPipelineStageFlags;
	   subresourceRange:TVkImageSubresourceRange);
var
 info:TVkImageMemoryBarrier;
begin
 info:=Default(TVkImageMemoryBarrier);
 info.sType           :=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 info.srcAccessMask   :=srcAccessMask;
 info.dstAccessMask   :=dstAccessMask;
 info.oldLayout       :=oldImageLayout;
 info.newLayout       :=newImageLayout;
 info.image           :=image;
 info.subresourceRange:=subresourceRange;

 vkCmdPipelineBarrier(
 	cmdbuffer,
 	srcStageMask,
 	dstStageMask,
 	0,
 	0, nil,
 	0, nil,
 	1, @info);
end;

procedure vkBufferMemoryBarrier(
	   cmdbuffer:TVkCommandBuffer;
	   buffer:TVkBuffer;
	   srcAccessMask:TVkAccessFlags;
	   dstAccessMask:TVkAccessFlags;
           offset,size:TVkDeviceSize;
	   srcStageMask:TVkPipelineStageFlags;
	   dstStageMask:TVkPipelineStageFlags);
var
 info:TVkBufferMemoryBarrier;
begin
 info:=Default(TVkBufferMemoryBarrier);
 info.sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
 info.srcAccessMask:=srcAccessMask;
 info.dstAccessMask:=dstAccessMask;
 //info.srcQueueFamilyIndex:TVkUInt32;
 //info.dstQueueFamilyIndex:TVkUInt32;
 info.buffer:=buffer;
 info.offset:=offset;
 info.size:=size;

 vkCmdPipelineBarrier(
 	cmdbuffer,
 	srcStageMask,
 	dstStageMask,
 	0,
        0, nil,
 	1, @info,
 	0, nil);

end;

procedure vkMemoryBarrier(
	   cmdbuffer:TVkCommandBuffer;
	   srcAccessMask:TVkAccessFlags;
	   dstAccessMask:TVkAccessFlags;
	   srcStageMask:TVkPipelineStageFlags;
	   dstStageMask:TVkPipelineStageFlags);
var
 info:TVkMemoryBarrier;
begin
 info:=Default(TVkMemoryBarrier);
 info.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
 info.srcAccessMask:=srcAccessMask;
 info.dstAccessMask:=dstAccessMask;

 vkCmdPipelineBarrier(cmdbuffer,
  srcStageMask,
  dstStageMask,
  0,
  1,
  @info,
  0,
  nil,
  0,
  nil);
end;

procedure vkBarrier(
	   cmdbuffer:TVkCommandBuffer;
	   srcStageMask:TVkPipelineStageFlags;
	   dstStageMask:TVkPipelineStageFlags);
begin
 vkCmdPipelineBarrier(cmdbuffer,
  srcStageMask,
  dstStageMask,
  0,
  0,
  nil,
  0,
  nil,
  0,
  nil);
end;

Procedure vkCmdBindVertexBuffer(commandBuffer:TVkCommandBuffer;
                                Binding:TVkUInt32;
                                Buffer:TVkBuffer;
                                Offset:TVkDeviceSize);
begin
 vkCmdBindVertexBuffers(commandBuffer,Binding,1,@Buffer,@Offset);
end;

{
Procedure vkCmdBindDescriptorBuffer(commandBuffer:TVkCommandBuffer;
                                    Binding:TVkUInt32;
                                    Buffer:TVkBuffer;
                                    Offset:TVkDeviceSize);
var
 info:TVkWriteDescriptorSet;
begin
 info:=Default(TVkWriteDescriptorSet);
 info.sType:=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;

end;
}

Procedure vkCmdBindSB(cmd:TVkCommandBuffer;
                      point:TVkPipelineBindPoint;
                      layout:TVkPipelineLayout;
                      aSet,aBind,aElem:TVkUInt32;
                      buffer:TVkBuffer;
                      offset,range:TVkDeviceSize);
var
 dwrite:TVkWriteDescriptorSet;
 buf:TVkDescriptorBufferInfo;
begin
 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
 dwrite.descriptorCount:=1;
 dwrite.pBufferInfo    :=@buf;

 buf:=Default(TVkDescriptorBufferInfo);
 buf.buffer:=buffer;
 buf.offset:=offset;
 buf.range :=range ;

 if (vkCmdPushDescriptorSetKHR=nil) then
 begin
  TPFN_vkVoidFunction(vkCmdPushDescriptorSetKHR):=vkGetInstanceProcAddr(VulkanApp.FInstance,'vkCmdPushDescriptorSetKHR');
 end;

 Assert(vkCmdPushDescriptorSetKHR<>nil);

 vkCmdPushDescriptorSetKHR(cmd,point,layout,aSet,1,@dwrite);
end;

procedure vkCmdWaitEvent(commandBuffer:TVkCommandBuffer;
                         event:TVkEvent;
                         srcStageMask:TVkPipelineStageFlags;
                         dstStageMask:TVkPipelineStageFlags);
begin
 vkCmdWaitEvents(commandBuffer,
  1,
  @event,
  srcStageMask,
  dstStageMask,
  0,
  nil,
  0,
  nil,
  0,
  nil);
end;

var
 _lazy_init:Integer=0;
 _lazy_wait:Pointer=nil;

function IsInitVulkan:Boolean;
begin
 Result:=(load_acq_rel(_lazy_init)=2);
end;

Function TestFFF(F:TVkFormatFeatureFlags):RawByteString;
begin
 Result:='';
 if (ord(F) and ord(VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT                 ))<>0 then Result:=Result+'|SAMPLED_IMAGE';
 if (ord(F) and ord(VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT                 ))<>0 then Result:=Result+'|STORAGE_IMAGE';
 if (ord(F) and ord(VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT          ))<>0 then Result:=Result+'|STORAGE_IMAGE_ATOMIC';
 if (ord(F) and ord(VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT          ))<>0 then Result:=Result+'|UNIFORM_TEXEL_BUFFER';
 if (ord(F) and ord(VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT          ))<>0 then Result:=Result+'|STORAGE_TEXEL_BUFFER';
 if (ord(F) and ord(VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT   ))<>0 then Result:=Result+'|STORAGE_TEXEL_BUFFER_ATOMIC';
 if (ord(F) and ord(VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT                 ))<>0 then Result:=Result+'|VERTEX_BUFFER';
 if (ord(F) and ord(VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT              ))<>0 then Result:=Result+'|COLOR_ATTACHMENT';
 if (ord(F) and ord(VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT        ))<>0 then Result:=Result+'|COLOR_ATTACHMENT_BLEND';
 if (ord(F) and ord(VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT      ))<>0 then Result:=Result+'|DEPTH_STENCIL_ATTACHMENT';
 if (ord(F) and ord(VK_FORMAT_FEATURE_BLIT_SRC_BIT                      ))<>0 then Result:=Result+'|BLIT_SRC';
 if (ord(F) and ord(VK_FORMAT_FEATURE_BLIT_DST_BIT                      ))<>0 then Result:=Result+'|BLIT_DST';
 if (ord(F) and ord(VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT   ))<>0 then Result:=Result+'|SAMPLED_IMAGE_FILTER_LINEAR';
 if (ord(F) and ord(VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG))<>0 then Result:=Result+'|SAMPLED_IMAGE_FILTER_CUBIC';
 if (ord(F) and ord(VK_FORMAT_FEATURE_TRANSFER_SRC_BIT                  ))<>0 then Result:=Result+'|TRANSFER_SRC';
 if (ord(F) and ord(VK_FORMAT_FEATURE_TRANSFER_DST_BIT                  ))<>0 then Result:=Result+'|TRANSFER_DST';
end;

function LoadVulkan:Boolean;
begin
 rw_wlock(_lazy_wait);

 if (load_acq_rel(_lazy_init)<>0) then
 begin
  rw_wunlock(_lazy_wait);
  Exit(True);
 end;

 Result:=LoadVulkanLibrary;

 if Result then
 begin
  Result:=LoadVulkanGlobalCommands;
 end;

 XCHG(_lazy_init,1);

 rw_wunlock(_lazy_wait);
end;

Procedure InitVulkan;
var
 DeviceInfo:TvDeviceCreateInfo;
 //ImgProp:TVkFormatProperties;

 FVIDS:TVkPhysicalDeviceVertexInputDynamicStateFeaturesEXT;
 FILFB:TVkPhysicalDeviceImagelessFramebufferFeatures;
 FDPVF:TVkPhysicalDeviceProvokingVertexFeaturesEXT;

 F16_8:TVkPhysicalDeviceShaderFloat16Int8Features;
 FSF_8:TVkPhysicalDevice8BitStorageFeatures;
 FSF16:TVkPhysicalDevice16BitStorageFeatures;
 FRF  :TVkPhysicalDeviceRobustness2FeaturesEXT;
 FDI  :TVkPhysicalDeviceDescriptorIndexingFeatures;
 FIVML:TVkPhysicalDeviceImageViewMinLodFeaturesEXT;

 FScalar:TVkPhysicalDeviceScalarBlockLayoutFeatures;

 FCoherent:TVkPhysicalDeviceCoherentMemoryFeaturesAMD;

begin
 rw_wlock(_lazy_wait);

 if (load_acq_rel(_lazy_init)<>1) then
 begin
  rw_wunlock(_lazy_wait);
  Exit;
 end;

 VulkanApp:=TVulkanApp.Create(VulkanAppFlags);

 DebugReport:=TVDebugReport.Create;

 FillDeviceExtension (VulkanApp.FPhysicalDevice);
 FillDeviceProperties(VulkanApp.FPhysicalDevice);

 DeviceInfo:=TvDeviceCreateInfo.Create;

 if (VulkanApp.FGFamilyCount>1) then
 begin
  FlipQueue  :=TvQueue.Create(VulkanApp.FGFamily);
  RenderQueue:=TvQueue.Create(VulkanApp.FGFamily);
  DeviceInfo.add_queue(VulkanApp.FGFamily,@FlipQueue  .FHandle);
  DeviceInfo.add_queue(VulkanApp.FGFamily,@RenderQueue.FHandle);
 end else
 begin
  FlipQueue  :=TvQueue.Create(VulkanApp.FGFamily);
  RenderQueue:=FlipQueue;
  DeviceInfo.add_queue(VulkanApp.FGFamily,@FlipQueue  .FHandle);
 end;

 if limits.VK_KHR_swapchain then
 begin
  DeviceInfo.add_ext(VK_KHR_SWAPCHAIN_EXTENSION_NAME);
 end else
 begin
  Writeln(stderr,'VK_KHR_swapchain not support!');
  //raise Exception.Create('VK_KHR_swapchain not support!');
 end;

 if limits.VK_EXT_external_memory_host then
 begin
  DeviceInfo.add_ext(VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME);
  DeviceInfo.add_ext(VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME);
 end else
 begin
  Writeln(stderr,'VK_EXT_external_memory_host not support!');
  //raise Exception.Create('VK_EXT_external_memory_host not support!');
 end;

 if limits.VK_AMD_device_coherent_memory then
 begin
  DeviceInfo.add_ext(VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME);

  FCoherent:=Default(TVkPhysicalDeviceCoherentMemoryFeaturesAMD);
  FCoherent.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD;
  FCoherent.deviceCoherentMemory:=VK_TRUE;

  DeviceInfo.add_feature(@FCoherent);
 end;

 if limits.VK_EXT_vertex_input_dynamic_state then
 begin
  DeviceInfo.add_ext(VK_EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME);

  FVIDS:=Default(TVkPhysicalDeviceVertexInputDynamicStateFeaturesEXT);
  FVIDS.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_INPUT_DYNAMIC_STATE_FEATURES_EXT;
  FVIDS.vertexInputDynamicState:=VK_TRUE;

  DeviceInfo.add_feature(@FVIDS);
 end;

 if limits.VK_KHR_imageless_framebuffer then
 begin
  DeviceInfo.add_ext(VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME);

  FILFB:=Default(TVkPhysicalDeviceImagelessFramebufferFeatures);
  FILFB.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES;
  FILFB.imagelessFramebuffer:=VK_TRUE;

  DeviceInfo.add_feature(@FILFB);
 end;

 if limits.VK_EXT_provoking_vertex then
 begin
  DeviceInfo.add_ext(VK_EXT_PROVOKING_VERTEX_EXTENSION_NAME);

  FDPVF:=Default(TVkPhysicalDeviceProvokingVertexFeaturesEXT);
  FDPVF.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_FEATURES_EXT;
  FDPVF.provokingVertexLast:=VK_TRUE;

  DeviceInfo.add_feature(@FDPVF);
 end;

 if limits.VK_KHR_image_format_list then
 begin
  DeviceInfo.add_ext(VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME);
 end;

 if limits.VK_EXT_descriptor_indexing then
 begin
  DeviceInfo.add_ext(VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME);

  FDI:=limits.DescriptorIndexingFeatures;
  FDI.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES;

  DeviceInfo.add_feature(@FDI);
 end;

 //if limits.VK_KHR_push_descriptor then
 //begin
 // DeviceQueues.add_ext(VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME);
 //end;

 if limits.VK_KHR_shader_non_semantic_info then
 begin
  DeviceInfo.add_ext(VK_KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME);
 end;

 if limits.VK_EXT_scalar_block_layout then
 begin
  DeviceInfo.add_ext(VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME);

  FScalar:=Default(TVkPhysicalDeviceScalarBlockLayoutFeatures);
  FScalar.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES;
  FScalar.scalarBlockLayout:=VK_TRUE;

  DeviceInfo.add_feature(@FScalar);
 end;

 if limits.VK_KHR_shader_float16_int8 then
 begin
  DeviceInfo.add_ext(VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME);
 end;

 if (limits.shaderInt8<>0) or
    (limits.shaderFloat16<>0) then
 begin
  F16_8:=Default(TVkPhysicalDeviceShaderFloat16Int8Features);
  F16_8.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES;
  F16_8.shaderFloat16:=limits.shaderFloat16;
  F16_8.shaderInt8   :=limits.shaderInt8;

  DeviceInfo.add_feature(@F16_8);
 end;

 if limits.VK_KHR_8bit_storage then
 begin
  DeviceInfo.add_ext(VK_KHR_8BIT_STORAGE_EXTENSION_NAME);
 end;

 if (limits.storageBuffer8BitAccess<>0) or
    (limits.uniformAndStorageBuffer8BitAccess<>0) then
 begin
  FSF_8:=Default(TVkPhysicalDevice8BitStorageFeaturesKHR);
  FSF_8.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES;
  FSF_8.storageBuffer8BitAccess          :=limits.storageBuffer8BitAccess;
  FSF_8.uniformAndStorageBuffer8BitAccess:=limits.uniformAndStorageBuffer8BitAccess;
  //limits.storagePushConstant8

  DeviceInfo.add_feature(@FSF_8);
 end;

 if limits.VK_KHR_16bit_storage then
 begin
  DeviceInfo.add_ext(VK_KHR_16BIT_STORAGE_EXTENSION_NAME);
 end;

 if (limits.storageBuffer16BitAccess<>0) or
    (limits.uniformAndStorageBuffer16BitAccess<>0) or
    (limits.storageInputOutput16<>0) then
 begin
  FSF16:=Default(TVkPhysicalDevice16BitStorageFeatures);
  FSF16.sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES;
  FSF16.storageBuffer16BitAccess          :=limits.storageBuffer16BitAccess;
  FSF16.uniformAndStorageBuffer16BitAccess:=limits.uniformAndStorageBuffer16BitAccess;
  //FSF16.storagePushConstant16
  FSF16.storageInputOutput16              :=limits.storageInputOutput16;

  DeviceInfo.add_feature(@FSF16);
 end;

 if limits.VK_EXT_robustness2 then
 begin
  DeviceInfo.add_ext(VK_EXT_ROBUSTNESS_2_EXTENSION_NAME);

  FRF:=Default(TVkPhysicalDeviceRobustness2FeaturesEXT);
  FRF.sType         :=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT;
  FRF.nullDescriptor:=limits.nullDescriptor;

  DeviceInfo.add_feature(@FRF);
 end;

 if limits.VK_EXT_image_view_min_lod then
 begin
  DeviceInfo.add_ext(VK_EXT_IMAGE_VIEW_MIN_LOD_EXTENSION_NAME);

  FIVML:=Default(TVkPhysicalDeviceImageViewMinLodFeaturesEXT);
  FIVML.sType :=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_MIN_LOD_FEATURES_EXT;
  FIVML.minLod:=VK_TRUE;

  DeviceInfo.add_feature(@FIVML);
 end;

 Device:=TvDevice.Create(DeviceInfo);
 DeviceInfo.Free;

 MemManager:=TvMemManager.Create;

 XCHG(_lazy_init,2);

 //ImgProp:=Default(TVkFormatProperties);
 //vkGetPhysicalDeviceFormatProperties(VulkanApp.FPhysicalDevice,VK_FORMAT_R8G8B8A8_UNORM,@ImgProp);
 //Writeln('R8G8B8A8_UNORM:',TestFFF(ImgProp.optimalTilingFeatures));
 //writeln;
 //vkGetPhysicalDeviceFormatProperties(VulkanApp.FPhysicalDevice,VK_FORMAT_R8G8B8A8_SRGB,@ImgProp);
 //Writeln('R8G8B8A8_SRGB:',TestFFF(ImgProp.optimalTilingFeatures));
 //writeln;
 //vkGetPhysicalDeviceFormatProperties(VulkanApp.FPhysicalDevice,VK_FORMAT_B8G8R8A8_SRGB,@ImgProp);
 //Writeln('B8G8R8A8_SRGB:',TestFFF(ImgProp.optimalTilingFeatures));
 //writeln;
 //vkGetPhysicalDeviceFormatProperties(VulkanApp.FPhysicalDevice,VK_FORMAT_A8B8G8R8_SRGB_PACK32,@ImgProp);
 //Writeln('A8B8G8R8_SRGB:',TestFFF(ImgProp.optimalTilingFeatures));




 //PrintQueueFamily(VulkanApp.FPhysicalDevice);
 //writeln;
 //PrintInstanceExtension;
 //writeln;
 //PrintDeviceExtension(VulkanApp.FPhysicalDevice);
 //writeln;

 rw_wunlock(_lazy_wait);
end;

//initialization
// SetExceptionMask([exInvalidOp, exDenormalized, exPrecision, exUnderflow]);

end.

