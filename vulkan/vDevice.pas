unit vDevice;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Math,
  Vulkan;

type
 TVulkanApp=class
  FInstance:TVkInstance;
  FPhysicalDevice:TVkPhysicalDevice;
  FGFamily:TVkUInt32;
  FCFamily:TVkUInt32;
  FTFamily:TVkUInt32;
  FDeviceFeature:TVkPhysicalDeviceFeatures;
  Constructor Create(debug,validate:Boolean);
  Destructor  Destroy; override;
  Procedure   LoadFamily; virtual;
  function    InstanceLayersIsExist(P:PChar):Boolean;
 end;

 TvDebugReport=class
  FHandle:TVkDebugReportCallbackEXT;
  FCreateDebugReportCallback:TvkCreateDebugReportCallbackEXT;
  FDestroyDebugReportCallback:TvkDestroyDebugReportCallbackEXT;
  Constructor Create;
  Destructor  Destroy; override;
  procedure   ReportCallback(flags:TVkDebugReportFlagsEXT;
                             objectType:TVkDebugReportObjectTypeEXT;
                             object_:TVkUInt64;
                             location:TVkSize;
                             messageCode:TVkInt32;
                             const pLayerPrefix:PVkChar;
                             const pMessage:PVkChar); virtual;
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

 TvDeviceQueues=class
  data:array of TSortQueueRec;
  exts:array of Pchar;
  procedure   add_queue(Index:TVkUInt32;Queue:PVkQueue);
  procedure   add_ext(P:Pchar);
 end;

 TvDevice=class
  FHandle:TVkDevice;
  Constructor Create(Queues:TvDeviceQueues);
  Destructor  Destroy; override;
 end;

 TCmdPool=class
  FHandle:TVkCommandPool;
  Constructor Create(FFamily:TVkUInt32);
  Destructor  Destroy; override;
  function    Alloc:TVkCommandBuffer;
  procedure   Free(cmd:TVkCommandBuffer);
 end;

 TvFence=class
  FHandle:TVkFence;
  Constructor Create(signaled:Boolean);
  Destructor  Destroy; override;
  function    Reset:TVkResult;
  function    Wait(timeout:TVkUInt64):TVkResult;
  function    Status:TVkResult;
 end;

 TvSemaphore=class
  FHandle:TVkSemaphore;
  Constructor Create;
  Destructor  Destroy; override;
 end;

procedure PrintPhysicalDeviceProperties(physicalDevice:TVkPhysicalDevice);
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

var
 VulkanApp:TVulkanApp;
 DebugReport:TVDebugReport;
 Device:TvDevice;

 FlipQueue:TVkQueue;
 RenderQueue:TVkQueue;

Procedure InitVulkan;
function  IsInitVulkan:Boolean;

implementation

uses
 vMemory;

type
 TSortIndex=object
  max:Integer;
  data:array of TVkDeviceQueueCreateInfo;
  procedure add(Index:TVkUInt32);
 end;

procedure PrintPhysicalDeviceProperties(physicalDevice:TVkPhysicalDevice);
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

 Writeln('minImportedHostPointerAlignment=',memh.minImportedHostPointerAlignment);

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
  Write(getstr_queueFlags(pFamily[i].queueFlags));
  Writeln(':',pFamily[i].queueCount);
 end;

 FreeMem(pFamily);
end;

function MyDebugReportCallback(flags:TVkDebugReportFlagsEXT;
                               objectType:TVkDebugReportObjectTypeEXT;
                               object_:TVkUInt64;
                               location:TVkSize;
                               messageCode:TVkInt32;
                               const pLayerPrefix:PVkChar;
                               const pMessage:PVkChar;
                               pUserData:PVkVoid):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
begin
 TVDebugReport(pUserData).ReportCallback(
   flags,objectType,object_,location,messageCode,pLayerPrefix,pMessage);
 Result:=TVkBool32(False);
end;

Constructor TVDebugReport.Create;
var
 cinfo:TVkDebugReportCallbackCreateInfoEXT;
 r:TVkResult;
begin

 TPFN_vkVoidFunction(FCreateDebugReportCallback) :=vkGetInstanceProcAddr(VulkanApp.FInstance,'vkCreateDebugReportCallbackEXT');
 TPFN_vkVoidFunction(FDestroyDebugReportCallback):=vkGetInstanceProcAddr(VulkanApp.FInstance,'vkDestroyDebugReportCallbackEXT');

 if (FCreateDebugReportCallback<>nil) then
 begin
  cinfo:=Default(TVkDebugReportCallbackCreateInfoEXT);
  cinfo.sType:=VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT;
  cinfo.flags:=
                            ord(VK_DEBUG_REPORT_INFORMATION_BIT_EXT        ) or
                            ord(VK_DEBUG_REPORT_WARNING_BIT_EXT            ) or
                            ord(VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT) or
                            ord(VK_DEBUG_REPORT_ERROR_BIT_EXT              ){ or
                            ord(VK_DEBUG_REPORT_DEBUG_BIT_EXT              )};

  cinfo.pfnCallback:=@MyDebugReportCallback;
  cinfo.pUserData:=Pointer(Self);
  r:=FCreateDebugReportCallback(VulkanApp.FInstance,@cinfo,nil,@FHandle);
  if (r<>VK_SUCCESS) then
  begin
   Writeln('CreateDebugReportCallback:',r);
   Exit;
  end;
 end;
end;

Destructor TVDebugReport.Destroy;
begin
 if (FDestroyDebugReportCallback<>nil) then
 begin
  FDestroyDebugReportCallback(VulkanApp.FInstance,FHandle,nil);
 end;
end;

procedure TVDebugReport.ReportCallback(flags:TVkDebugReportFlagsEXT;
                           objectType:TVkDebugReportObjectTypeEXT;
                           object_:TVkUInt64;
                           location:TVkSize;
                           messageCode:TVkInt32;
                           const pLayerPrefix:PVkChar;
                           const pMessage:PVkChar);
begin
 if Pos('which is greater than buffer size (4)',pMessage)=0 then
  Writeln({objectType,':',pLayerPrefix,':',}pMessage);
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
 pPhysicalDevices:=GetMem(count*SizeOf(TVkPhysicalDevice));
 vkEnumeratePhysicalDevices(vkInstance,@count,pPhysicalDevices);
 For i:=0 to count-1 do
 begin
  deviceProperties:=Default(TVkPhysicalDeviceProperties);
  vkGetPhysicalDeviceProperties(pPhysicalDevices[i],@deviceProperties);
  Writeln(deviceProperties.deviceName);
  Writeln('apiVersion:',VK_VERSION_MAJOR(deviceProperties.apiVersion),'.',
                        VK_VERSION_MINOR(deviceProperties.apiVersion),'.',
                        VK_VERSION_PATCH(deviceProperties.apiVersion));
 end;
 Result:=vkGetPhysicalDevice4Type(pPhysicalDevices,count,VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU);
 if (Result=VK_NULL_HANDLE) then
 begin
  Result:=vkGetPhysicalDevice4Type(pPhysicalDevices,count,VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU);
 end;
 if (Result=VK_NULL_HANDLE) then
 begin
  Result:=pPhysicalDevices[0];
 end;
 FreeMem(pPhysicalDevices);
end;

Constructor TVulkanApp.Create(debug,validate:Boolean);
const
 dlayer='VK_LAYER_KHRONOS_validation';
var
 vkApp:TVkApplicationInfo;
 vkExtList:array[0..2] of PChar;
 vkLayer:array[0..0] of PChar;
 vkCInfo:TVkInstanceCreateInfo;
 r:TVkResult;
begin
 vkApp:=Default(TVkApplicationInfo);
 vkApp.sType             :=VK_STRUCTURE_TYPE_APPLICATION_INFO;
 vkApp.pApplicationName  :='VulkanApp';
 vkApp.applicationVersion:=VK_MAKE_VERSION(1, 0, 0);
 vkApp.pEngineName       :=nil;
 vkApp.engineVersion     :=VK_MAKE_VERSION(1, 0, 0);
 vkApp.apiVersion        :=VK_API_VERSION_1_1;

 vkExtList[0]:=VK_KHR_SURFACE_EXTENSION_NAME;
 vkExtList[1]:=VK_KHR_WIN32_SURFACE_EXTENSION_NAME;
 vkExtList[2]:=VK_EXT_DEBUG_REPORT_EXTENSION_NAME;

 vkCInfo:=Default(TVkInstanceCreateInfo);
 vkCInfo.sType:=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
 vkCInfo.pApplicationInfo:=@vkApp;
 if debug then
 begin
  vkCInfo.enabledExtensionCount:=Length(vkExtList);
  if validate and InstanceLayersIsExist(dlayer) then
  begin
   vkLayer[0]:=dlayer;
   vkCInfo.enabledLayerCount:=1;
   vkCInfo.ppEnabledLayerNames:=@vkLayer;
  end;
 end else
 begin
  vkCInfo.enabledExtensionCount:=Length(vkExtList)-1;
 end;
 vkCInfo.ppEnabledExtensionNames:=@vkExtList;

 r:=vkCreateInstance(@vkCInfo,nil,@FInstance);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkCreateInstance:',r);
  Exit;
 end;

 FPhysicalDevice:=vkGetPhysicalDevice(FInstance);
 if (FPhysicalDevice=VK_NULL_HANDLE) then
 begin
  Writeln('failed to chouse vulkan GPU');
  Exit;
 end;

 FDeviceFeature:=Default(TVkPhysicalDeviceFeatures);
 vkGetPhysicalDeviceFeatures(FPhysicalDevice,@FDeviceFeature);

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
    bLoaded:=bLoaded+[gLoad];
   end;
  end else
  if (pQueue[i].queueFlags and ord(VK_QUEUE_COMPUTE_BIT))<>0 then
  begin
   if not (cLoad in bLoaded) then
   begin
    FCFamily:=i;
    bLoaded:=bLoaded+[cLoad];
   end;
  end else
  if (pQueue[i].queueFlags and ord(VK_QUEUE_TRANSFER_BIT))<>0 then
  begin
   if not (tLoad in bLoaded) then
   begin
    FTFamily:=i;
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
   Break;
  end;
 end;
 if not (tLoad in bLoaded) then
 begin
  For i:=0 to count-1 do
  if (pQueue[i].queueFlags and ord(VK_QUEUE_TRANSFER_BIT))<>0 then
  begin
   FTFamily:=i;
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
  Writeln('vkCreateWin32SurfaceKHR:',r);
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
  Writeln('failed to chouse QueuePresentFamily');
  Exit;
 end;
end;

Procedure TVSurface.LoadFormat;
var
 count:TVkUInt32;
 formats:PVkSurfaceFormatKHR;
 r:TVkResult;

 function TryFind(format:TVkFormat):Boolean;
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
   formats:=GetMem(count*SizeOf(TVkSurfaceFormatKHR));
   vkGetPhysicalDeviceSurfaceFormatsKHR(VulkanApp.FPhysicalDevice,FHandle,@count,formats);
   if TryFind(VK_FORMAT_R8G8B8A8_SRGB)         then Break;
   if TryFind(VK_FORMAT_B8G8R8A8_SRGB)         then Break;
   if TryFind(VK_FORMAT_A8B8G8R8_SRGB_PACK32)  then Break;
   if TryFind(VK_FORMAT_R8G8B8A8_UNORM)        then Break;
   if TryFind(VK_FORMAT_B8G8R8A8_UNORM)        then Break;
   if TryFind(VK_FORMAT_A8B8G8R8_UNORM_PACK32) then Break;
   if (Fformat.format=VK_FORMAT_UNDEFINED) then
   begin
    Fformat:=formats[0];
   end;
  until true;
  FreeMem(formats);
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

procedure TvDeviceQueues.add_queue(Index:TVkUInt32;Queue:PVkQueue);
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

procedure TvDeviceQueues.add_ext(P:Pchar);
var
 i:Integer;
begin
 i:=Length(exts);
 SetLength(exts,i+1);
 exts[i]:=P;
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

Constructor TvDevice.Create(Queues:TvDeviceQueues);
Var
 DeviceFeature:TVkPhysicalDeviceFeatures;
 SortIndex:TSortIndex;
 Priority:array of Single;
 DeviceInfo:TVkDeviceCreateInfo;
 r:TVkResult;
 i,p,w:Integer;
begin
 DeviceFeature:=VulkanApp.FDeviceFeature;
 DeviceFeature.robustBufferAccess:=0;

 DeviceInfo:=Default(TVkDeviceCreateInfo);
 DeviceInfo.sType:=VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
 DeviceInfo.pEnabledFeatures:=@DeviceFeature;

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
  Writeln('vkCreateDevice:',r);
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
 vkDestroyDevice(FHandle,nil);
end;

//

Constructor TCmdPool.Create(FFamily:TVkUInt32);
var
 cinfo:TVkCommandPoolCreateInfo;
 r:TVkResult;
begin
 cinfo:=Default(TVkCommandPoolCreateInfo);
 cinfo.sType           :=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
 cinfo.queueFamilyIndex:=FFamily;
 cinfo.flags:=ord(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT);
 r:=vkCreateCommandPool(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('failed to create command pool!');
  exit;
 end;
end;

Destructor TCmdPool.Destroy;
begin
 vkDestroyCommandPool(Device.FHandle,FHandle,nil);
end;

function TCmdPool.Alloc:TVkCommandBuffer;
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
  Writeln('failed to allocate command buffers!');
  Exit;
 end;
end;

procedure TCmdPool.Free(cmd:TVkCommandBuffer);
begin
 vkFreeCommandBuffers(Device.FHandle,FHandle,1,@cmd);
end;

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
  Writeln('vkCreateFence:',r);
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
 imageMemoryBarrier:TVkImageMemoryBarrier;
begin
 imageMemoryBarrier:=Default(TVkImageMemoryBarrier);
 imageMemoryBarrier.sType           :=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 imageMemoryBarrier.srcAccessMask   :=srcAccessMask;
 imageMemoryBarrier.dstAccessMask   :=dstAccessMask;
 imageMemoryBarrier.oldLayout       :=oldImageLayout;
 imageMemoryBarrier.newLayout       :=newImageLayout;
 imageMemoryBarrier.image           :=image;
 imageMemoryBarrier.subresourceRange:=subresourceRange;

 vkCmdPipelineBarrier(
 	cmdbuffer,
 	srcStageMask,
 	dstStageMask,
 	0,
 	0, nil,
 	0, nil,
 	1, @imageMemoryBarrier);
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
 MemoryBarrier:TVkBufferMemoryBarrier;
begin
 MemoryBarrier:=Default(TVkBufferMemoryBarrier);
 MemoryBarrier.sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
 MemoryBarrier.srcAccessMask:=srcAccessMask;
 MemoryBarrier.dstAccessMask:=dstAccessMask;
 //MemoryBarrier.srcQueueFamilyIndex:TVkUInt32;
 //MemoryBarrier.dstQueueFamilyIndex:TVkUInt32;
 MemoryBarrier.buffer:=buffer;
 MemoryBarrier.offset:=offset;
 MemoryBarrier.size:=size;

 vkCmdPipelineBarrier(
 	cmdbuffer,
 	srcStageMask,
 	dstStageMask,
 	0,
        0, nil,
 	1, @MemoryBarrier,
 	0, nil);

end;

var
 _lazy_init:Integer=0;
 _lazy_wait:Integer=0;

function IsInitVulkan:Boolean;
begin
 Result:=(System.InterLockedExchangeAdd(_lazy_wait,0)<>0);
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

Procedure InitVulkan;
var
 DeviceQueues:TvDeviceQueues;
 //ImgProp:TVkFormatProperties;
begin
 if System.InterlockedExchange(_lazy_init,1)=0 then
 begin
  VulkanApp:=TVulkanApp.Create(true,true);
  DebugReport:=TVDebugReport.Create;

  MemManager:=TvMemManager.Create;

  DeviceQueues:=TvDeviceQueues.Create;
  DeviceQueues.add_queue(VulkanApp.FGFamily,@FlipQueue);
  DeviceQueues.add_queue(VulkanApp.FGFamily,@RenderQueue);
  DeviceQueues.add_ext(VK_KHR_SWAPCHAIN_EXTENSION_NAME);

  Device:=TvDevice.Create(DeviceQueues);

  System.InterLockedExchangeAdd(_lazy_wait,1);

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

 end else
 begin
  While (System.InterLockedExchangeAdd(_lazy_wait,0)=0) do System.ThreadSwitch;
 end;
end;

initialization
 if not LoadVulkanLibrary        then raise Exception.Create('LoadVulkanLibrary');
 if not LoadVulkanGlobalCommands then raise Exception.Create('LoadVulkanGlobalCommands');
 SetExceptionMask([exInvalidOp, exDenormalized, exPrecision]);

end.

