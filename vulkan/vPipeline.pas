unit vPipeline;

{$mode objfpc}{$H+}

interface

uses
 Vulkan,
 vDevice,
 vShader,
 vDependence;

type
 AVkDescriptorSetLayoutBinding=array of TVkDescriptorSetLayoutBinding;

 PvSetLayoutKey=^TvSetLayoutKey;
 TvSetLayoutKey=record
  FStage:TVkShaderStageFlags;
  FFlags:TVkUInt32;
  FBinds:AVkDescriptorSetLayoutBinding;
 end;

 TvSetLayout=class
  FHandle:TVkDescriptorSetLayout;
  key:TvSetLayoutKey;
  Procedure   SetUsePushDescriptor(b:Boolean);
  function    GetUsePushDescriptor:Boolean;
  Procedure   Add(aBind:TVkUInt32;dType:TVkDescriptorType;Flags:TVkShaderStageFlags;count:TVkUInt32=1);
  Procedure   SetBinds(const A:AVkDescriptorSetLayoutBinding);
  procedure   Clear;
  function    Compile:Boolean;
  Function    IsSpace:Boolean; inline;
  Destructor  Destroy; override;
 end;

 AvSetLayout=array of TvSetLayout;
 AvPushConstantRange=array of TVkPushConstantRange;

 PvPipelineLayoutKey=^TvPipelineLayoutKey;
 TvPipelineLayoutKey=record
  FLayouts   :AvSetLayout;
  FPushConsts:AvPushConstantRange;
 end;

 TvPipelineLayout=class
  FHandle:TVkPipelineLayout;
  key:TvPipelineLayoutKey;
  //
  FBinds:TVkUInt32;
  FSets :TVkUInt32;
  FTypes:TVkUInt32;
  //
  FCounts:TvCountsGroup;
  //
  Procedure   AddLayout(F:TvSetLayout);
  Procedure   SetLayouts(const A:AvSetLayout);
  Procedure   AddPushConst(offset,size:TVkUInt32;Flags:TVkShaderStageFlags);
  Procedure   SetPushConst(const A:AvPushConstantRange);
  procedure   Clear;
  function    Compile:Boolean;
  procedure   calc_counts;
  Destructor  Destroy; override;
  Function    isSpace:Boolean;
 end;

 TvPipelineCache=class
  FHandle:TVkPipelineCache;
  Constructor Create(data:Pointer;size:TVkSize);
  Destructor  Destroy; override;
 end;

 TvPipeline=class(TvRefsObject)
  FHandle:TVkPipeline;
  FPCache:TvPipelineCache;
  Destructor  Destroy; override;
 end;

 TvRenderPass=class(TvRefsObject)
  FHandle:TVkRenderPass;
  Destructor Destroy; override;
 end;

///////

implementation

Procedure TvSetLayout.Add(aBind:TVkUInt32;dType:TVkDescriptorType;Flags:TVkShaderStageFlags;count:TVkUInt32=1);
var
 i:Integer;
begin
 i:=Length(key.FBinds);
 SetLength(key.FBinds,i+1);
 key.FBinds[i]:=Default(TVkDescriptorSetLayoutBinding);
 key.FBinds[i].binding:=aBind;
 key.FBinds[i].descriptorType:=dType;
 key.FBinds[i].descriptorCount:=count;
 key.FBinds[i].stageFlags:=Flags;
end;

Procedure TvSetLayout.SetBinds(const A:AVkDescriptorSetLayoutBinding);
begin
 key.FBinds:=A;
end;

Function TvSetLayout.IsSpace:Boolean; inline;
begin
 Result:=Length(key.FBinds)=0;
end;

Procedure TvSetLayout.Clear;
begin
 SetLength(key.FBinds,0);
 key.FFlags:=0;
end;

function TvSetLayout.Compile:Boolean;
var
 cinfo:TVkDescriptorSetLayoutCreateInfo;
 binfo:TVkDescriptorSetLayoutBindingFlagsCreateInfo;
 bflag:array of TVkDescriptorBindingFlags;
 r:TVkResult;
 i,c:Integer;
 partially:Boolean;
begin
 Result:=False;

 if (FHandle<>VK_NULL_HANDLE) then Exit(True);

 cinfo:=Default(TVkDescriptorSetLayoutCreateInfo);
 cinfo.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
 cinfo.flags:=key.FFlags;
 cinfo.bindingCount:=Length(key.FBinds);
 //
 if (cinfo.bindingCount<>0) then
 begin
  cinfo.pBindings:=@key.FBinds[0];
 end;
 //
 {
 if (limits.DescriptorIndexingFeatures.descriptorBindingPartiallyBound<>0) then
 begin
  partially:=False;
  c:=Length(key.FBinds);
  if (c<>0) then
  begin
   For i:=0 to c-1 do
   if (key.FBinds[i].descriptorCount>1) then
   begin
    partially:=True;
    Break;
   end;
  end;
  //
  if partially then
  begin
   SetLength(bflag,c);
   For i:=0 to c-1 do
   begin
    bflag[i]:=0;
    if (key.FBinds[i].descriptorCount>1) then
    begin
     bflag[i]:=ord(VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT) or
               ord(VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT);
    end;
   end;
   //
   binfo:=Default(TVkDescriptorSetLayoutBindingFlagsCreateInfo);
   binfo.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO;
   binfo.bindingCount :=c;
   binfo.pBindingFlags:=@bflag[0];
   //
   cinfo.pNext:=@binfo;
  end;
 end;
 }
 //
 r:=vkCreateDescriptorSetLayout(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateDescriptorSetLayout:',r);
  Exit;
 end;
 Result:=True;
end;

Destructor TvSetLayout.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyDescriptorSetLayout(Device.FHandle,FHandle,nil);
 end;
end;

Procedure TvSetLayout.SetUsePushDescriptor(b:Boolean);
begin
 Case b of
  True:
   if (key.FFlags<>ord(VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR)) then
   begin
    key.FFlags:=ord(VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR);
   end;
  False:
   if (key.FFlags=ord(VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR)) then
   begin
    key.FFlags:=0;
   end;
 end;
end;

function TvSetLayout.GetUsePushDescriptor:Boolean;
begin
 Result:=(key.FFlags=ord(VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR));
end;

Destructor TvPipelineLayout.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyPipelineLayout(Device.FHandle,FHandle,nil);
 end;
end;

Procedure TvPipelineLayout.AddLayout(F:TvSetLayout);
begin
 if (F=nil) then Exit;
 Insert(F,key.FLayouts,Length(key.FLayouts));
end;

Procedure TvPipelineLayout.SetLayouts(const A:AvSetLayout);
begin
 key.FLayouts:=A;
end;

Procedure TvPipelineLayout.AddPushConst(offset,size:TVkUInt32;Flags:TVkShaderStageFlags);
var
 i:Integer;
begin
 i:=Length(key.FPushConsts);
 SetLength(key.FPushConsts,i+1);
 key.FPushConsts[i].stageFlags:=Flags;
 key.FPushConsts[i].offset    :=offset;
 key.FPushConsts[i].size      :=size;
end;

Procedure TvPipelineLayout.SetPushConst(const A:AvPushConstantRange);
begin
 key.FPushConsts:=A;
end;

Function TvPipelineLayout.isSpace:Boolean;
begin
 Result:=(FBinds=0);
end;

procedure TvPipelineLayout.Clear;
begin
 SetLength(key.FLayouts   ,0);
 SetLength(key.FPushConsts,0);
 FBinds:=0;
 FSets :=0;
 FTypes:=0;
 //
 FCounts:=Default(TvCountsGroup);
end;

function TvPipelineLayout.Compile:Boolean;
var
 cinfo:TVkPipelineLayoutCreateInfo;
 r:TVkResult;
 _data_set:array of TVkDescriptorSetLayout;
 i:Integer;
begin
 Result:=false;

 if (FHandle<>VK_NULL_HANDLE) then Exit(True);

 if (Length(key.FLayouts)<>0) then
 begin
  _data_set:=nil;
  SetLength(_data_set,Length(key.FLayouts));
  For i:=0 to High(key.FLayouts) do
  begin
   Assert(key.FLayouts[i]<>nil,'key.FLayouts[i]=nil');
   _data_set[i]:=key.FLayouts[i].FHandle;
  end;
 end;

 cinfo:=Default(TVkPipelineLayoutCreateInfo);
 cinfo.sType         :=VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
 cinfo.setLayoutCount:=Length(_data_set);
 if (cinfo.setLayoutCount<>0) then
 begin
  cinfo.pSetLayouts:=@_data_set[0];
 end;
 cinfo.pushConstantRangeCount:=Length(key.FPushConsts);
 if (cinfo.pushConstantRangeCount<>0) then
 begin
  cinfo.pPushConstantRanges:=@key.FPushConsts[0];
 end;
 r:=vkCreatePipelineLayout(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreatePipelineLayout:',r);
  Exit;
 end;

 calc_counts;

 Result:=True;
end;

procedure TvPipelineLayout.calc_counts;
var
 i,b:Integer;
 t:Byte;
begin
 if Length(key.FLayouts)<>0 then
 begin
  For i:=0 to High(key.FLayouts) do
   With key.FLayouts[i] do
    if (Length(key.FBinds)<>0) then
    begin
     Inc(FBinds,Length(key.FBinds));
     Inc(FSets);
     For b:=0 to High(key.FBinds) do
      with key.FBinds[b] do
      begin
       t:=_GetIdByType(descriptorType);

       if (FCounts[t]=0) then
       begin
        Inc(FTypes);
       end;

       Inc(FCounts[t],descriptorCount);
      end;
    end;
 end;
end;

Constructor TvPipelineCache.Create(data:Pointer;size:TVkSize);
var
 info:TVkPipelineCacheCreateInfo;
 r:TVkResult;
begin
 info:=Default(TVkPipelineCacheCreateInfo);
 info.sType:=VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO;
 info.initialDataSize:=size;
 info.pInitialData   :=data;

 r:=vkCreatePipelineCache(Device.FHandle,@info,nil,@FHandle);

 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreatePipelineCache:',r);
 end;
end;

Destructor TvPipelineCache.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyPipelineCache(Device.FHandle,FHandle,nil);
 end;
end;

Destructor TvPipeline.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyPipeline(Device.FHandle,FHandle,nil);
 end;
end;

Destructor TvRenderPass.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyRenderPass(Device.FHandle,FHandle,nil);
 end;
end;


end.

