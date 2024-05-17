unit vSampler;

{$mode objfpc}{$H+}

interface

uses
 Vulkan,
 vDevice;

type
 TvSampler=class
  FHandle:TVkSampler;
  function   Compile(pInfo:PVkSamplerCreateInfo):Boolean;
  Destructor Destroy; override;
 end;

implementation

function TvSampler.Compile(pInfo:PVkSamplerCreateInfo):Boolean;
var
 r:TVkResult;
begin
 Result:=False;
 if (pInfo=nil) then Exit;

 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroySampler(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;

 r:=vkCreateSampler(Device.FHandle,pInfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateSampler:',r);
  Exit;
 end;
 Result:=True;
end;

Destructor TvSampler.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroySampler(Device.FHandle,FHandle,nil);
 end;
 inherited;
end;

end.

