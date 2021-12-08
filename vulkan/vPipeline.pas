unit vPipeline;

{$mode objfpc}{$H+}

interface

uses
 g23tree,
 vulkan,vDevice,vShader;

type
 TvSetLayout=class
  FHandle:TVkDescriptorSetLayout;
  FEdit,FCompile:ptruint;
  FBinds:array of TVkDescriptorSetLayoutBinding;
  Destructor  Destroy; override;
  Procedure   Add(aBind:TVkUInt32;dType:TVkDescriptorType;Flags:TVkShaderStageFlags;count:TVkUInt32=1);
  procedure   Clear;
  function    Compile:Boolean;
  function    IsEdit:Boolean;
 end;

 TvPipelineLayout=class
  FHandle:TVkPipelineLayout;
  FEdit,FCompile:ptruint;
  FLayouts:array of TvSetLayout;
  FPushConsts:array of TVkPushConstantRange;
  Destructor  Destroy; override;
  Procedure   Add(F:TvSetLayout);
  Procedure   AddPushConst(offset,size:TVkUInt32;Flags:TVkShaderStageFlags);
  procedure   Clear;
  function    Compile:Boolean;
  function    IsEdit:Boolean;
 end;

 TvPipeline=class
  FHandle:TVkPipeline;
  FEdit,FCompile:ptruint;
  Destructor  Destroy; override;
 end;

 TvRenderPass=class
  FHandle:TVkRenderPass;
  Destructor Destroy; override;
 end;

 TvComputePipeline=class(TvPipeline)
  FLayout:TvPipelineLayout;
  FComputeShader:TvShader;
  procedure   SetLayout(Layout:TvPipelineLayout);
  Procedure   SetShader(Shader:TvShader);
  function    Compile:Boolean;
  function    IsEdit:Boolean;
 end;

 TvSetsPool=class;

 TvDescriptorSet=class
  FParent:TvSetsPool;
  FLayout:TvSetLayout;
  FHandle:TVkDescriptorSet;
  procedure  _AllocDesc;
  procedure  _FreeDesc;
  Destructor Destroy; override;
  Procedure  BindUB (aBind,aElem:TVkUInt32;const buf:TVkDescriptorBufferInfo);
  Procedure  BindSB (aBind,aElem:TVkUInt32;const buf:TVkDescriptorBufferInfo);
  Procedure  BindUBD(aBind,aElem:TVkUInt32;const buf:TVkDescriptorBufferInfo);
  Procedure  BindSBD(aBind,aElem:TVkUInt32;const buf:TVkDescriptorBufferInfo);
  Procedure  BindSTI(aBind,aElem:TVkUInt32;const img:TVkImageView);
 end;

 _TvSetLayoutKey=object
  Layout:TvSetLayout;
  fcount:TVkUInt32;
  function c(const a,b:_TvSetLayoutKey):Integer; static;
 end;

 _TvSetLayoutSet=specialize T23treeSet<_TvSetLayoutKey,_TvSetLayoutKey>;

 _TvDescriptorSetCompare=object
  function c(const a,b:TvDescriptorSet):Integer; static;
 end;

 _TvDescriptorSetSet=specialize T23treeSet<TvDescriptorSet,_TvDescriptorSetCompare>;

 TvSetsPool=class
  FHandle:TVkDescriptorPool;
  FEdit,FCompile:ptruint;
  FmaxSets:TVkUInt32;
  FLayouts:_TvSetLayoutSet;
  FSets:_TvDescriptorSetSet;
  Destructor  Destroy; override;
  function    _FindLayout(L:TvSetLayout):Boolean;
  procedure   ClearLayouts;
  Procedure   AddLayout(L:TvSetLayout;count:TVkUInt32=1);
  Procedure   AddFormPipelineLayout(L:TvPipelineLayout;count:TVkUInt32=1);
  function    Alloc(L:TvSetLayout):TvDescriptorSet;
  function    Compile:Boolean;
  function    IsEdit:Boolean;
 end;

implementation

function _TvSetLayoutKey.c(const a,b:_TvSetLayoutKey):Integer;
begin
 Result:=CompareByte(a.Layout,b.Layout,SizeOf(TvSetLayout));
end;

function _TvDescriptorSetCompare.c(const a,b:TvDescriptorSet):Integer;
begin
 Result:=CompareByte(a,b,SizeOf(TvDescriptorSet));
end;

Procedure TvSetLayout.Add(aBind:TVkUInt32;dType:TVkDescriptorType;Flags:TVkShaderStageFlags;count:TVkUInt32=1);
var
 i:Integer;
begin
 i:=Length(FBinds);
 SetLength(FBinds,i+1);
 FBinds[i]:=Default(TVkDescriptorSetLayoutBinding);
 FBinds[i].binding:=aBind;
 FBinds[i].descriptorType:=dType;
 FBinds[i].descriptorCount:=count;
 FBinds[i].stageFlags:=Flags;
 Inc(FEdit);
end;

Procedure TvSetLayout.Clear;
begin
 SetLength(FBinds,0);
 Inc(FEdit);
end;

function TvSetLayout.Compile:Boolean;
var
 cinfo:TVkDescriptorSetLayoutCreateInfo;
 r:TVkResult;
begin
 Result:=False;
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  if (FEdit=FCompile) then Exit(true);
  vkDestroyDescriptorSetLayout(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;
 cinfo:=Default(TVkDescriptorSetLayoutCreateInfo);
 cinfo.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
 cinfo.bindingCount:=Length(FBinds);
 if (cinfo.bindingCount<>0) then
 begin
  cinfo.pBindings:=@FBinds[0];
 end;
 r:=vkCreateDescriptorSetLayout(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkCreateDescriptorSetLayout:',r);
  Exit;
 end;
 FCompile:=FEdit;
 Result:=True;
end;

Destructor TvSetLayout.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
  vkDestroyDescriptorSetLayout(Device.FHandle,FHandle,nil);
end;

function TvSetLayout.IsEdit:Boolean;
begin
 Result:=FEdit<>FCompile;
end;

Destructor TvPipelineLayout.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
  vkDestroyPipelineLayout(Device.FHandle,FHandle,nil);
end;

Procedure TvPipelineLayout.Add(F:TvSetLayout);
var
 i:Integer;
begin
 if (F=nil) then Exit;
 i:=Length(FLayouts);
 SetLength(FLayouts,i+1);
 FLayouts[i]:=F;
 Inc(FEdit);
end;

Procedure TvPipelineLayout.AddPushConst(offset,size:TVkUInt32;Flags:TVkShaderStageFlags);
var
 i:Integer;
begin
 i:=Length(FPushConsts);
 SetLength(FPushConsts,i+1);
 FPushConsts[i].stageFlags:=Flags;
 FPushConsts[i].offset    :=offset;
 FPushConsts[i].size      :=size;
 Inc(FEdit);
end;

procedure TvPipelineLayout.Clear;
begin
 SetLength(FLayouts,0);
 SetLength(FPushConsts,0);
 Inc(FEdit);
end;

function TvPipelineLayout.Compile:Boolean;
var
 cinfo:TVkPipelineLayoutCreateInfo;
 r:TVkResult;
 _data_set:array of TVkDescriptorSetLayout;
 i:Integer;
begin
 Result:=false;
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  if (not IsEdit) then Exit(true);
  vkDestroyPipelineLayout(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;
 if (Length(FLayouts)<>0) then
 begin
  _data_set:=nil;
  SetLength(_data_set,Length(FLayouts));
  For i:=0 to High(FLayouts) do
  begin
   if not FLayouts[i].Compile then Exit;
   _data_set[i]:=FLayouts[i].FHandle;
  end;
 end;
 cinfo:=Default(TVkPipelineLayoutCreateInfo);
 cinfo.sType         :=VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
 cinfo.setLayoutCount:=Length(_data_set);
 if (cinfo.setLayoutCount<>0) then
 begin
  cinfo.pSetLayouts:=@_data_set[0];
 end;
 cinfo.pushConstantRangeCount:=Length(FPushConsts);
 if (cinfo.pushConstantRangeCount<>0) then
 begin
  cinfo.pPushConstantRanges:=@FPushConsts[0];
 end;
 r:=vkCreatePipelineLayout(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkCreatePipelineLayout:',r);
  Exit;
 end;
 FCompile:=FEdit;
 Result:=True;
end;

function TvPipelineLayout.IsEdit:Boolean;
var
 i:Integer;
begin
 Result:=FEdit<>FCompile;
 if not Result then
  if (Length(FLayouts)<>0) then
   For i:=0 to High(FLayouts) do
    if FLayouts[i].IsEdit then Exit(true);
end;

Destructor TvPipeline.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
  vkDestroyPipeline(Device.FHandle,FHandle,nil);
end;

Destructor TvRenderPass.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
  vkDestroyRenderPass(Device.FHandle,FHandle,nil);
end;

procedure TvComputePipeline.SetLayout(Layout:TvPipelineLayout);
begin
 if (FLayout<>Layout) then
 begin
  Inc(FEdit);
  FLayout:=Layout;
  Compile;
 end;
end;

Procedure TvComputePipeline.SetShader(Shader:TvShader);
begin
 if (FComputeShader<>Shader) then
 begin
  Inc(FEdit);
  if (Shader=nil) then
  begin
   FComputeShader:=nil;
  end else
  if (Shader.FStage=VK_SHADER_STAGE_COMPUTE_BIT) then
  begin
   FComputeShader:=Shader;
   Compile;
  end;
 end;
end;

function TvComputePipeline.Compile:Boolean;
var
 cinfo:TVkComputePipelineCreateInfo;
 r:TVkResult;
begin
 Result:=False;
 if (FLayout=nil) or (FComputeShader=nil) then Exit;
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  if (not IsEdit) then Exit(true);
  vkDestroyPipeline(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;
 if not FLayout.Compile then Exit;
 cinfo:=Default(TVkComputePipelineCreateInfo);
 cinfo.sType:=VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO;
 cinfo.stage.sType:=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
 cinfo.stage.stage:=VK_SHADER_STAGE_COMPUTE_BIT;
 cinfo.stage.module:=FComputeShader.FHandle;
 cinfo.stage.pName:=PChar(FComputeShader.FEntry);
 cinfo.layout:=FLayout.FHandle;
 r:=vkCreateComputePipelines(Device.FHandle,VK_NULL_HANDLE,1,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkCreateComputePipelines:',r);
  Exit;
 end;
 FCompile:=FEdit;
 Result:=True;
end;

function TvComputePipeline.IsEdit:Boolean;
begin
 Result:=(FEdit<>FCompile);
 if (not Result) and (FLayout<>nil) then
  Result:=Result or FLayout.IsEdit;
end;

Destructor TvSetsPool.Destroy;
var
 It:_TvDescriptorSetSet.Iterator;
begin
 It:=FSets.cbegin;
 if (It.Item<>nil) then
 repeat
  It.Item^.FHandle:=VK_NULL_HANDLE;
 until not It.Next;
 if (FHandle<>VK_NULL_HANDLE) then
  vkDestroyDescriptorPool(Device.FHandle,FHandle,nil);
 FSets.Free;
 FLayouts.Free;
end;

function _GetTypeById(i:Byte):TVkDescriptorType;
begin
 Result:=VK_DESCRIPTOR_TYPE_SAMPLER;
 Case i of
   0:Result:=VK_DESCRIPTOR_TYPE_SAMPLER                   ;
   1:Result:=VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER    ;
   2:Result:=VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE             ;
   3:Result:=VK_DESCRIPTOR_TYPE_STORAGE_IMAGE             ;
   4:Result:=VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER      ;
   5:Result:=VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER      ;
   6:Result:=VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER            ;
   7:Result:=VK_DESCRIPTOR_TYPE_STORAGE_BUFFER            ;
   8:Result:=VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC    ;
   9:Result:=VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC    ;
  10:Result:=VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT          ;
  11:Result:=VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT  ;
  12:Result:=VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR;
  13:Result:=VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV ;
  14:Result:=VK_DESCRIPTOR_TYPE_MUTABLE_VALVE             ;
 end;
end;

function _GetIdByType(i:TVkDescriptorType):Byte;
begin
 Result:=0;
 Case i of
  VK_DESCRIPTOR_TYPE_SAMPLER                   :Result:=0;
  VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER    :Result:=1;
  VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE             :Result:=2;
  VK_DESCRIPTOR_TYPE_STORAGE_IMAGE             :Result:=3;
  VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER      :Result:=4;
  VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER      :Result:=5;
  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER            :Result:=6;
  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER            :Result:=7;
  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC    :Result:=8;
  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC    :Result:=9;
  VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT          :Result:=10;
  VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT  :Result:=11;
  VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR:Result:=12;
  VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV :Result:=13;
  VK_DESCRIPTOR_TYPE_MUTABLE_VALVE             :Result:=14;
 end;
end;

function TvSetsPool.Compile:Boolean;
var
 i,b,L:Integer;
 It:_TvSetLayoutSet.Iterator;
 Ik:_TvSetLayoutKey;
 Id:_TvDescriptorSetSet.Iterator;
 FCounts:array[0..14] of TVkUInt32;
 FSize:array of TVkDescriptorPoolSize;
 cinfo:TVkDescriptorPoolCreateInfo;
 r:TVkResult;
begin
 Result:=False;
 if (FHandle<>VK_NULL_HANDLE) and (not IsEdit) then Exit(true);
 if (FLayouts.Size=0) then Exit;
 FSize:=nil;

 FmaxSets:=0;
 FillChar(FCounts,SizeOf(FCounts),0);
 It:=FLayouts.cbegin;
 repeat
  Ik:=It.Item^;
  if (Ik.Layout<>nil) and (Ik.fcount<>0) then
  if (Length(Ik.Layout.FBinds)<>0) then
  begin
   FmaxSets:=FmaxSets+Ik.fcount;
   For i:=0 to Ik.fcount-1 do
    For b:=0 to High(Ik.Layout.FBinds) do
     with Ik.Layout.FBinds[b] do
     begin
      Inc(FCounts[_GetIdByType(descriptorType)],descriptorCount);
     end;
  end;
 until not It.Next;

 For i:=0 to 14 do
  if (FCounts[i]<>0) then
  begin
   L:=Length(FSize);
   SetLength(FSize,L+1);
   FSize[L].type_:=_GetTypeById(i);
   FSize[L].descriptorCount:=FCounts[i];
  end;
 if (Length(FSize)=0) then Exit;

 Id:=FSets.cbegin;
 if (Id.Item<>nil) then
 repeat
  Id.Item^.FHandle:=VK_NULL_HANDLE;
 until not Id.Next;

 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyDescriptorPool(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;

 It:=FLayouts.cbegin;
 repeat
  Ik:=It.Item^;
  if (Ik.Layout<>nil) and (Ik.fcount<>0) then
  begin
   if not Ik.Layout.Compile then Exit;
  end;
 until not It.Next;

 cinfo:=Default(TVkDescriptorPoolCreateInfo);
 cinfo.sType        :=VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
 cinfo.poolSizeCount:=Length(FSize);
 cinfo.pPoolSizes   :=@FSize[0];
 cinfo.maxSets      :=FmaxSets;

 r:=vkCreateDescriptorPool(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkCreateDescriptorPool:',r);
  Exit;
 end;

 i:=0;
 Id:=FSets.cbegin;
 if (Id.Item<>nil) then
 repeat
  Id.Item^._AllocDesc;
  Inc(i);
  if (i>=FmaxSets) then Break;
 until not Id.Next;

 FCompile:=FEdit;
 Result:=True;
end;

function TvSetsPool.IsEdit:Boolean;
var
 It:_TvSetLayoutSet.Iterator;
 Ik:_TvSetLayoutKey;
begin
 Result:=False;
 if (FEdit<>FCompile) then Exit(True);
 It:=FLayouts.cbegin;
 if (It.Item=nil) then Exit;
 repeat
  Ik:=It.Item^;
  if (Ik.Layout<>nil) and (Ik.fcount<>0) then
  if Ik.Layout.IsEdit then Exit(True);
 until not It.Next;
end;

procedure TvSetsPool.ClearLayouts;
begin
 FLayouts.Free;
 Inc(FEdit);
end;

function TvSetsPool._FindLayout(L:TvSetLayout):Boolean;
var
 Ik:_TvSetLayoutKey;
begin
 Ik.Layout:=L;
 Ik.fcount:=0;
 Result:=FLayouts.Contains(Ik);
end;

Procedure TvSetsPool.AddLayout(L:TvSetLayout;count:TVkUInt32);
var
 It:_TvSetLayoutSet.Iterator;
 Ik:_TvSetLayoutKey;
begin
 if (L=nil) then Exit;
 if (count=0) then count:=1;
 Ik.Layout:=L;
 Ik.fcount:=count;
 It:=FLayouts.find(Ik);
 if (It.Item<>nil) then
 begin
  It.Item^.fcount:=It.Item^.fcount+count;
 end else
 begin
  FLayouts.Insert(Ik);
 end;
 Inc(FEdit);
end;

Procedure TvSetsPool.AddFormPipelineLayout(L:TvPipelineLayout;count:TVkUInt32);
var
 i:Integer;
begin
 if (L=nil) then Exit;
 if (Length(L.FLayouts)<>0) then
  For i:=0 to High(L.FLayouts) do
  begin
   AddLayout(L.FLayouts[i],count);
  end;
end;

function TvSetsPool.Alloc(L:TvSetLayout):TvDescriptorSet;
var
 ainfo:TVkDescriptorSetAllocateInfo;
 FResult:TVkDescriptorSet;
 r:TVkResult;
begin
 Result:=nil;
 if (L=nil) then Exit;
 if not _FindLayout(L) then Exit;
 if not Compile then Exit;
 if (FSets.Size>=FmaxSets) then Exit;
 ainfo:=Default(TVkDescriptorSetAllocateInfo);
 ainfo.sType             :=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
 ainfo.descriptorPool    :=FHandle;
 ainfo.descriptorSetCount:=1;
 ainfo.pSetLayouts:=@L.FHandle;
 r:=vkAllocateDescriptorSets(Device.FHandle,@ainfo,@FResult);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkAllocateDescriptorSets:',r);
  Exit;
 end;
 Result:=TvDescriptorSet.Create;
 Result.FParent:=Self;
 Result.FLayout:=L;
 Result.FHandle:=FResult;
 FSets.Insert(Result);
end;

procedure TvDescriptorSet._AllocDesc;
var
 ainfo:TVkDescriptorSetAllocateInfo;
 r:TVkResult;
begin
 if (FParent<>nil) and (FLayout<>nil) then
 if (FHandle=VK_NULL_HANDLE) and
    (FParent.FHandle<>VK_NULL_HANDLE) then
 begin
  ainfo:=Default(TVkDescriptorSetAllocateInfo);
  ainfo.sType             :=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
  ainfo.descriptorPool    :=FParent.FHandle;
  ainfo.descriptorSetCount:=1;
  ainfo.pSetLayouts:=@FLayout.FHandle;
  r:=vkAllocateDescriptorSets(Device.FHandle,@ainfo,@FHandle);
  if (r<>VK_SUCCESS) then
  begin
   Writeln('vkAllocateDescriptorSets:',r);
   Exit;
  end;
 end;
end;

procedure TvDescriptorSet._FreeDesc;
var
 r:TVkResult;
begin
 if (FParent<>nil) then
 if (FHandle<>VK_NULL_HANDLE) and
    (FParent.FHandle<>VK_NULL_HANDLE) then
 begin
  r:=vkFreeDescriptorSets(Device.FHandle,FParent.FHandle,1,@FHandle);
  if (r<>VK_SUCCESS) then
  begin
   Writeln('vkFreeDescriptorSets:',r);
  end;
 end;
 FHandle:=VK_NULL_HANDLE;
end;

Destructor TvDescriptorSet.Destroy;
begin
 if (FParent<>nil) then
 begin
  _FreeDesc;
  FParent.FSets.delete(Self);
 end;
 inherited;
end;

Procedure TvDescriptorSet.BindUB(aBind,aElem:TVkUInt32;const buf:TVkDescriptorBufferInfo);
var
 dwrite:TVkWriteDescriptorSet;
begin
 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
 dwrite.descriptorCount:=1;
 dwrite.pBufferInfo    :=@buf;
 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;


Procedure TvDescriptorSet.BindSB(aBind,aElem:TVkUInt32;const buf:TVkDescriptorBufferInfo);
var
 dwrite:TVkWriteDescriptorSet;
begin
 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
 dwrite.descriptorCount:=1;
 dwrite.pBufferInfo    :=@buf;
 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

Procedure TvDescriptorSet.BindUBD(aBind,aElem:TVkUInt32;const buf:TVkDescriptorBufferInfo);
var
 dwrite:TVkWriteDescriptorSet;
begin
 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC;
 dwrite.descriptorCount:=1;
 dwrite.pBufferInfo    :=@buf;
 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

Procedure TvDescriptorSet.BindSBD(aBind,aElem:TVkUInt32;const buf:TVkDescriptorBufferInfo);
var
 dwrite:TVkWriteDescriptorSet;
begin
 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC;
 dwrite.descriptorCount:=1;
 dwrite.pBufferInfo    :=@buf;
 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

Procedure TvDescriptorSet.BindSTI(aBind,aElem:TVkUInt32;const img:TVkImageView);
var
 dwrite:TVkWriteDescriptorSet;
 dimg:TVkDescriptorImageInfo;
begin
 dimg:=Default(TVkDescriptorImageInfo);
 dimg.imageView:=img;
 dimg.imageLayout:=VK_IMAGE_LAYOUT_GENERAL;
 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
 dwrite.descriptorCount:=1;
 dwrite.pImageInfo     :=@dimg;
 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

end.

