unit vPipeline;

{$mode objfpc}{$H+}

interface

uses
 g23tree,
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

 TvSetsPool=class;

 TvDescriptorSet=class
  FParent:TvSetsPool;
  FLayout:TvSetLayout;
  FHandle:TVkDescriptorSet;
  procedure  _AllocDesc;
  procedure  _FreeDesc;
  Destructor Destroy; override;
  Procedure  BindBuf(aBind,aElem:TVkUInt32;dtype:TVkDescriptorType;buffer:TVkBuffer;offset,range:TVkDeviceSize);
  Procedure  BindSTI(aBind,aElem:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
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

 AvDescriptorPoolSize=array of TVkDescriptorPoolSize;

 TvSetsPool=class
  FHandle:TVkDescriptorPool;
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
 end;

///////

 TvDescriptorSet2=object
  FHandle:TVkDescriptorSet;
  Function   IsValid:Boolean;
  Procedure  BindBuffer (aBind,aElem:TVkUInt32;dtype:TVkDescriptorType;buffer:TVkBuffer;offset,range:TVkDeviceSize);
  Procedure  BindStorage(aBind,aElem:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
  Procedure  BindImage  (aBind,aElem:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
  Procedure  BindSampler(aBind,aElem:TVkUInt32;smp:TVkSampler);
 end;

 AvDescriptorSet2=Array of TvDescriptorSet2;

 TvDescriptorGroup=class(TvRefsObject)
  //lock:Ptruint;
  FSets:AvDescriptorSet2;
  //Procedure Release;
 end;

 AvDescriptorGroup=Array of TvDescriptorGroup;

 //PvSetsPoolKey=^TvSetsPoolKey;
 //TvSetsPoolKey=record
 // FPipeline:TvPipelineLayout;
 // FNumber  :PtrUint;
 //end;

 TvSetsPool2=class
  FHandle:TVkDescriptorPool;
  //key:TvSetsPoolKey;
  FPipeline:TvPipelineLayout;
  FmaxGroup:TVkUInt32;
  FmaxSets :TVkUInt32;
  FAlcGroup:TVkUInt32;
  //FGroups  :AvDescriptorGroup;
  Constructor Create(Pipeline:TvPipelineLayout;maxGroup:TVkUInt32);
  Destructor  Destroy; override;
  function    Compile:Boolean;
  function    Alloc(L:TvSetLayout):TvDescriptorSet2;
  //function    Alloc:TvDescriptorGroup;
  function    IsFull:Boolean;
  function    Alloc:AvDescriptorSet2;
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
 r:TVkResult;
begin
 Result:=False;

 if (FHandle<>VK_NULL_HANDLE) then Exit(True);

 cinfo:=Default(TVkDescriptorSetLayoutCreateInfo);
 cinfo.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
 cinfo.flags:=key.FFlags;
 cinfo.bindingCount:=Length(key.FBinds);
 if (cinfo.bindingCount<>0) then
 begin
  cinfo.pBindings:=@key.FBinds[0];
 end;
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
var
 i:Integer;
begin
 if (F=nil) then Exit;
 i:=Length(key.FLayouts);
 SetLength(key.FLayouts,i+1);
 key.FLayouts[i]:=F;
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

//

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
 begin
  vkDestroyDescriptorPool(Device.FHandle,FHandle,nil);
 end;
 FSets.Free;
 FLayouts.Free;
end;

function TvSetsPool.Compile:Boolean;
var
 i,b,L:Integer;
 It:_TvSetLayoutSet.Iterator;
 Ik:_TvSetLayoutKey;
 Id:_TvDescriptorSetSet.Iterator;
 FCounts:TvCountsGroup;
 FSizes:AvDescriptorPoolSize;
 cinfo:TVkDescriptorPoolCreateInfo;
 r:TVkResult;
begin
 Result:=False;

 if (FHandle<>VK_NULL_HANDLE) then Exit(true);

 if (FLayouts.Size=0) then Exit;
 FSizes:=Default(AvDescriptorPoolSize);

 FmaxSets:=0;
 FCounts:=Default(TvCountsGroup);
 It:=FLayouts.cbegin;
 repeat
  Ik:=It.Item^;
  if (Ik.Layout<>nil) and (Ik.fcount<>0) then
  if (Length(Ik.Layout.key.FBinds)<>0) then
  begin
   FmaxSets:=FmaxSets+Ik.fcount;
   For i:=0 to Ik.fcount-1 do
    For b:=0 to High(Ik.Layout.key.FBinds) do
     with Ik.Layout.key.FBinds[b] do
     begin
      Inc(FCounts[_GetIdByType(descriptorType)],descriptorCount);
     end;
  end;
 until not It.Next;

 For i:=LO_DESCRIPTOR_ID to HI_DESCRIPTOR_ID do
  if (FCounts[i]<>0) then
  begin
   L:=Length(FSizes);
   SetLength(FSizes,L+1);
   FSizes[L].type_          :=_GetTypeById(i);
   FSizes[L].descriptorCount:=FCounts[i];
  end;
 if (Length(FSizes)=0) then Exit;

 Id:=FSets.cbegin;
 if (Id.Item<>nil) then
 repeat
  Id.Item^.FHandle:=VK_NULL_HANDLE;
 until not Id.Next;

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
 cinfo.flags        :=ord(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT);
 cinfo.poolSizeCount:=Length(FSizes);
 cinfo.pPoolSizes   :=@FSizes[0];
 cinfo.maxSets      :=FmaxSets;

 r:=vkCreateDescriptorPool(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateDescriptorPool:',r);
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

 Result:=True;
end;

procedure TvSetsPool.ClearLayouts;
begin
 FLayouts.Free;
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
end;

Procedure TvSetsPool.AddFormPipelineLayout(L:TvPipelineLayout;count:TVkUInt32);
var
 i:Integer;
begin
 if (L=nil) then Exit;
 if (Length(L.key.FLayouts)<>0) then
  For i:=0 to High(L.key.FLayouts) do
  begin
   AddLayout(L.key.FLayouts[i],count);
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
 if (FSets.Size>=FmaxSets)  then Exit;
 ainfo:=Default(TVkDescriptorSetAllocateInfo);
 ainfo.sType             :=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
 ainfo.descriptorPool    :=FHandle;
 ainfo.descriptorSetCount:=1;
 ainfo.pSetLayouts:=@L.FHandle;
 r:=vkAllocateDescriptorSets(Device.FHandle,@ainfo,@FResult);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkAllocateDescriptorSets:',r);
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
   Writeln(StdErr,'vkAllocateDescriptorSets:',r);
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
   Writeln(StdErr,'vkFreeDescriptorSets:',r);
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

//VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
//VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
//VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
//VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC

Procedure TvDescriptorSet.BindBuf(aBind,aElem:TVkUInt32;dtype:TVkDescriptorType;buffer:TVkBuffer;offset,range:TVkDeviceSize);
var
 dwrite:TVkWriteDescriptorSet;
 buf:TVkDescriptorBufferInfo;
begin
 buf:=Default(TVkDescriptorBufferInfo);
 buf.buffer:=buffer;
 buf.offset:=offset;
 buf.range :=range ;

 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=dtype;
 dwrite.descriptorCount:=1;
 dwrite.pBufferInfo    :=@buf;

 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

Procedure TvDescriptorSet.BindSTI(aBind,aElem:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
var
 dwrite:TVkWriteDescriptorSet;
 dimg:TVkDescriptorImageInfo;
begin
 dimg:=Default(TVkDescriptorImageInfo);
 dimg.imageView  :=img;
 dimg.imageLayout:=Layout;
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

//

Function TvDescriptorSet2.IsValid:Boolean;
begin
 Result:=FHandle<>VK_NULL_HANDLE;
end;

Procedure TvDescriptorSet2.BindBuffer(aBind,aElem:TVkUInt32;dtype:TVkDescriptorType;buffer:TVkBuffer;offset,range:TVkDeviceSize);
var
 dwrite:TVkWriteDescriptorSet;
 buf:TVkDescriptorBufferInfo;
begin
 buf:=Default(TVkDescriptorBufferInfo);
 buf.buffer:=buffer;
 buf.offset:=offset;
 buf.range :=range ;

 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=dtype;
 dwrite.descriptorCount:=1;
 dwrite.pBufferInfo    :=@buf;

 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

Procedure TvDescriptorSet2.BindStorage(aBind,aElem:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
var
 dwrite:TVkWriteDescriptorSet;
 dimg:TVkDescriptorImageInfo;
begin
 dimg:=Default(TVkDescriptorImageInfo);
 dimg.imageView  :=img;
 dimg.imageLayout:=Layout;
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

Procedure TvDescriptorSet2.BindImage(aBind,aElem:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
var
 dwrite:TVkWriteDescriptorSet;
 dimg:TVkDescriptorImageInfo;
begin
 dimg:=Default(TVkDescriptorImageInfo);
 dimg.imageView  :=img;
 dimg.imageLayout:=Layout;
 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE;
 dwrite.descriptorCount:=1;
 dwrite.pImageInfo     :=@dimg;
 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

Procedure TvDescriptorSet2.BindSampler(aBind,aElem:TVkUInt32;smp:TVkSampler);
var
 dwrite:TVkWriteDescriptorSet;
 dimg:TVkDescriptorImageInfo;
begin
 dimg:=Default(TVkDescriptorImageInfo);
 dimg.sampler:=smp;
 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_SAMPLER;
 dwrite.descriptorCount:=1;
 dwrite.pImageInfo     :=@dimg;
 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

Constructor TvSetsPool2.Create(Pipeline:TvPipelineLayout;maxGroup:TVkUInt32);
begin
 FPipeline:=Pipeline;
 FmaxGroup:=maxGroup;
end;

Destructor TvSetsPool2.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyDescriptorPool(Device.FHandle,FHandle,nil);
 end;
end;

function TvSetsPool2.Compile:Boolean;
var
 i,p:Integer;
 FSizes:AvDescriptorPoolSize;
 cinfo:TVkDescriptorPoolCreateInfo;
 r:TVkResult;
begin
 Result:=False;
 if (FHandle<>VK_NULL_HANDLE) then Exit(true);

 if (FPipeline=nil) then Exit;
 if (FmaxGroup=0) then Exit;

 if (not FPipeline.Compile) then Exit;

 FSizes:=Default(AvDescriptorPoolSize);
 SetLength(FSizes,FPipeline.FTypes);

 FmaxSets:=FPipeline.FSets*FmaxGroup;

 p:=0;
 For i:=LO_DESCRIPTOR_ID to HI_DESCRIPTOR_ID do
 begin
  if (FPipeline.FCounts[i]<>0) then
  begin
   FSizes[p].type_          :=_GetTypeById(i);
   FSizes[p].descriptorCount:=FPipeline.FCounts[i]*FmaxGroup;
   Inc(p);
  end;
 end;

 if (Length(FSizes)=0) then Exit;

 cinfo:=Default(TVkDescriptorPoolCreateInfo);
 cinfo.sType        :=VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
 //cinfo.flags        :=ord(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT);
 cinfo.poolSizeCount:=Length(FSizes);
 cinfo.pPoolSizes   :=@FSizes[0];
 cinfo.maxSets      :=FmaxSets;

 r:=vkCreateDescriptorPool(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateDescriptorPool:',r);
  Exit;
 end;

 Result:=True;
end;

function TvSetsPool2.Alloc(L:TvSetLayout):TvDescriptorSet2;
var
 ainfo:TVkDescriptorSetAllocateInfo;
 FResult:TVkDescriptorSet;
 r:TVkResult;
begin
 Result:=Default(TvDescriptorSet2);

 if (L=nil) then Exit;
 if L.IsSpace then Exit;

 if not Compile then Exit;

 ainfo:=Default(TVkDescriptorSetAllocateInfo);
 ainfo.sType             :=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
 ainfo.descriptorPool    :=FHandle;
 ainfo.descriptorSetCount:=1;
 ainfo.pSetLayouts       :=@L.FHandle;
 r:=vkAllocateDescriptorSets(Device.FHandle,@ainfo,@FResult);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkAllocateDescriptorSets:',r);
  Exit;
 end;

 Result.FHandle:=FResult;
end;

function TvSetsPool2.IsFull:Boolean;
begin
 Result:=(FAlcGroup>=FmaxGroup);
end;

function TvSetsPool2.Alloc:AvDescriptorSet2;
var
 i:Integer;
begin
 Result:=nil;
 if IsFull then Exit;
 SetLength(Result,Length(FPipeline.key.FLayouts));
 If (Length(Result)<>0) then
  For i:=0 to High(Result) do
   begin
    Result[i]:=Alloc(FPipeline.key.FLayouts[i]);
   end;
 Inc(FAlcGroup);
end;

{
Procedure TvDescriptorGroup.Release;
begin
 lock:=0;
end;
}

end.

