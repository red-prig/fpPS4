unit vCmdBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  g_node_splay,
  si_ci_vi_merged_enum,

  kern_proc,
  vm_map,
  vm_tracking_map,

  Vulkan,
  vDependence,
  vDevice,
  vMemory,
  //vShader,
  vShaderExt,
  vImage,
  vPipeline,
  vPipelineManager,
  vSetsPoolManager,
  vDescriptorSet,
  vRender;

type
 TvCmdBuffer=class;

 PvSemaphoreWait=^TvSemaphoreWait;
 TvSemaphoreWait=object
  //
  FSemaphore:TvSemaphore; //Must be the first element in memory
  FWaitStage:TVkPipelineStageFlags;
  //
  pLeft :PvSemaphoreWait;
  pRight:PvSemaphoreWait;
  //
  function c(a,b:PvSemaphoreWait):Integer; static;
 end;

 TvSemaphoreWaitSet=specialize TNodeSplay<TvSemaphoreWait>;

 p_cmd_track_deferred=^t_cmd_track_deferred;
 t_cmd_track_deferred=object
  //
  start  :QWORD;
   __end :QWORD;
  exclude:Pointer;
  //
  pLeft :p_cmd_track_deferred;
  pRight:p_cmd_track_deferred;
  //
  function c(a,b:p_cmd_track_deferred):Integer; static;
 end;

 t_cmd_track_deferred_set=specialize TNodeSplay<t_cmd_track_deferred>;

const
 BP_GRAPHICS=VK_PIPELINE_BIND_POINT_GRAPHICS;
 BP_COMPUTE =VK_PIPELINE_BIND_POINT_COMPUTE;

type
 TvCustomCmdBuffer=class(TvDependenciesObject)

  FParent:TvCustomCmdPool;
  FQueue :TvQueue;
  FCmdbuf:TVkCommandBuffer;

  cmd_count:qword;
  ret:Integer;

  submit_id:ptruint;

  FCurrPipeline:array[BP_GRAPHICS..BP_COMPUTE] of TVkPipeline;
  FCurrLayout  :array[BP_GRAPHICS..BP_COMPUTE] of TvPipelineLayout;
  FCurrBinds   :array[BP_GRAPHICS..BP_COMPUTE] of PvDescriptorCache;
  FCurrGroup   :array[BP_GRAPHICS..BP_COMPUTE] of TvDescriptorGroup;

  FDescriptorCacheSet:TvDescriptorCacheSet;

  FRenderPass:TVkRenderPass;

  FWaitSemaphores:TvSemaphoreWaitSet;
  FWaitSemaphoresCount:Integer;

  FSignalSemaphore:TvSemaphore;

  FFence:TvFence;

  FCBState:(cbFree,cbInit,cbBegin,cbEnd,cbSubmit);

  FPlannedTriggers:t_cmd_track_deferred_set;

  Constructor Create(pool:TvCustomCmdPool;Queue:TvQueue);
  Destructor  Destroy; override;

  function    AllocCmdBuffer:Boolean;
  Function    IsAllocated:Boolean;
  function    BeginCmdBuffer:Boolean;
  Procedure   EndCmdBuffer;
  Procedure   BindPipeline(BindPoint:TVkPipelineBindPoint;F:TVkPipeline);
  Function    IsRenderPass:Boolean;
  Procedure   EndRenderPass;

  function    QueueSubmit:TVkResult;
  function    Wait(timeout:TVkUInt64):TVkResult;
  function    Status:TVkResult;

  Procedure   ReleaseResource;
  Procedure   FreeAllSemaphores;
  Procedure   AddWaitSemaphore(S:TvSemaphore;W:TVkPipelineStageFlags);
  function    GetSignaledSemaphore:TvSemaphore;

  Procedure   ReleaseAllPlannedTriggers;
  Procedure   AddPlannedTrigger(start,__end:QWORD;exclude:Pointer);

  Procedure   FreeAllDescriptorCache;

  Procedure   BindLayout(BindPoint:TVkPipelineBindPoint;F:TvPipelineLayout);
  Procedure   BindSet(BindPoint:TVkPipelineBindPoint;fset:TVkUInt32;FHandle:TVkDescriptorSet);
  Procedure   PushConstant(BindPoint:TVkPipelineBindPoint;stageFlags:TVkShaderStageFlags;offset,size:TVkUInt32;const pValues:PVkVoid);
  Procedure   DispatchDirect(X,Y,Z:TVkUInt32);

  Procedure   BindVertexBuffers(const FAttrBuilder:TvAttrBuilder);
  Procedure   SetVertexInput(const FAttrBuilder:TvAttrBuilder);

  Procedure   ClearDepthStencilImage(image:TVkImage;
                                     imageLayout:TVkImageLayout;
                                     const pDepthStencil:PVkClearDepthStencilValue;
                                     rangeCount:TVkUInt32;
                                     const pRanges:PVkImageSubresourceRange);

  Procedure   ClearDepthStencilImage(image:TVkImage;
                                     imageLayout:TVkImageLayout;
                                     const pDepthStencil:PVkClearDepthStencilValue;
                                     const range:TVkImageSubresourceRange);

  Procedure   ClearColorImage(image:TVkImage;
                              imageLayout:TVkImageLayout;
                              const pColor:PVkClearColorValue;
                              rangeCount:TVkUInt32;
                              const pRanges:PVkImageSubresourceRange);

  Procedure   ResolveImage(srcImage:TVkImage;
                           srcImageLayout:TVkImageLayout;
                           dstImage:TVkImage;
                           dstImageLayout:TVkImageLayout;
                           regionCount:TVkUInt32;
                           const pRegions:PVkImageResolve);

  procedure   CopyBufferToImage(srcBuffer:TVkBuffer;
                                dstImage:TVkImage;
                                dstImageLayout:
                                TVkImageLayout;
                                regionCount:TVkUInt32;
                                const pRegions:PVkBufferImageCopy);

  procedure   CopyImageToBuffer(srcImage:TVkImage;
                                srcImageLayout:TVkImageLayout;
                                dstBuffer:TVkBuffer;
                                regionCount:TVkUInt32;
                                const pRegions:PVkBufferImageCopy);

  procedure   BufferMemoryBarrier(buffer:TVkBuffer;
                                  srcAccessMask:TVkAccessFlags;
                                  dstAccessMask:TVkAccessFlags;
                                  offset,size:TVkDeviceSize;
                                  srcStageMask:TVkPipelineStageFlags;
                                  dstStageMask:TVkPipelineStageFlags);

  Procedure   InsertLabel(pLabelName:PVkChar);
  Procedure   BeginLabel(pLabelName:PVkChar);
  Procedure   EndLabel();

  function    FetchDescriptorCache(layout:TvPipelineLayout):PvDescriptorCache;
  function    FetchDescriptorCache(BindPoint:TVkPipelineBindPoint):PvDescriptorCache;
  function    FetchDescriptorInterface(BindPoint:TVkPipelineBindPoint):TvDescriptorInterface;
  Procedure   ApplyDescriptorCache(BindPoint:TVkPipelineBindPoint);

  Procedure   BindSets(BindPoint:TVkPipelineBindPoint;F:TvDescriptorGroup);
 end;

 TvCmdBuffer=class(TvCustomCmdBuffer)

  FRender:TvRenderPassBeginInfo;

  Femulate_primtype:Integer;
  FinstanceCount:DWORD;
  FINDEX_TYPE:TVkIndexType;

  function    BeginRenderPass(RT:PvRenderPassBeginInfo;GP:TvGraphicsPipeline2):Boolean;

  function    BindCompute(CP:TvComputePipeline2):Boolean;

  Procedure   dmaData1(src,dst:Pointer;byteCount:DWORD;isBlocking:Boolean);
  Procedure   dmaData2(src:DWORD;dst:Pointer;byteCount:DWORD;isBlocking:Boolean);
  Procedure   WriteEos(eventType:Byte;dst:Pointer;value:DWORD;isBlocking:Boolean);
  Procedure   WriteEvent(eventType:Byte);

  Procedure   DrawIndexOffset2(IndexBase:Pointer;indexOffset,indexCount:DWORD);
  Procedure   DrawIndex2(IndexBase:Pointer;indexCount:DWORD);
  Procedure   DrawIndexAuto(indexCount:DWORD);
 end;

implementation

uses
 vBuffer,
 vHostBufferManager;

function TvSemaphoreWait.c(a,b:PvSemaphoreWait):Integer;
begin
 Result:=Integer(Pointer(a^.FSemaphore)>Pointer(b^.FSemaphore))-Integer(Pointer(a^.FSemaphore)<Pointer(b^.FSemaphore));
end;

function t_cmd_track_deferred.c(a,b:p_cmd_track_deferred):Integer;
begin
 //start
 Result:=Integer(a^.start>b^.start)-Integer(a^.start<b^.start);
 if (Result<>0) then Exit;
 //__end
 Result:=Integer(a^.__end>b^.__end)-Integer(a^.__end<b^.__end);
 if (Result<>0) then Exit;
 //exclude
 Result:=Integer(a^.exclude>b^.exclude)-Integer(a^.exclude<b^.exclude);
 if (Result<>0) then Exit;
end;

//

Constructor TvCustomCmdBuffer.Create(pool:TvCustomCmdPool;Queue:TvQueue);
begin
 FParent:=pool;
 FQueue:=Queue;

 FFence:=TvFence.Create(False);

 FSignalSemaphore:=TvSemaphore.Create;

 FCBState:=cbFree;
end;

Destructor TvCustomCmdBuffer.Destroy;
begin
 ReleaseResource;

 FreeAndNil(FFence);
 FreeAndNil(FSignalSemaphore);

 if (FParent<>nil) and (FCmdbuf<>VK_NULL_HANDLE) then
 begin
  FParent.Free(FCmdbuf);
 end;
 inherited;
end;

function TvCustomCmdBuffer.AllocCmdBuffer:Boolean;
begin
 if (FCmdbuf<>VK_NULL_HANDLE) then Exit(True);

 FCmdbuf:=FParent.Alloc;
 if (FCmdbuf=VK_NULL_HANDLE) then Exit(False);

 FCBState:=cbInit;

 Result:=True;
end;

Function TvCustomCmdBuffer.IsAllocated:Boolean;
begin
 Result:=False;

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 Result:=(FCmdbuf<>VK_NULL_HANDLE);
end;

function TvCustomCmdBuffer.BeginCmdBuffer:Boolean;
var
 r:TVkResult;
 Info:TVkCommandBufferBeginInfo;
begin
 Result:=False;

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (not AllocCmdBuffer) then Exit;

 case FCBState of
  cbInit:; //need start
  cbBegin:
   begin
    //is started
    Exit(True);
   end;
  else
   //idk why it called in this state
   Exit(False);
 end;

 Info:=Default(TVkCommandBufferBeginInfo);
 Info.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
 Info.flags:=ord(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT);
 Info.pInheritanceInfo:=nil;
 r:=vkBeginCommandBuffer(FCmdbuf,@Info);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkBeginCommandBuffer:',r);
  Exit;
 end;
 FCBState:=cbBegin;
 Result:=True;
end;

Procedure TvCustomCmdBuffer.EndCmdBuffer;
var
 r:TVkResult;
begin
 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (FCBState<>cbBegin) then Exit;

 EndRenderPass;

 FCurrLayout[BP_GRAPHICS]:=nil;
 FCurrLayout[BP_COMPUTE ]:=nil;

 FCurrBinds[BP_GRAPHICS]:=nil;
 FCurrBinds[BP_COMPUTE ]:=nil;

 FCurrGroup[BP_GRAPHICS]:=nil;
 FCurrGroup[BP_COMPUTE ]:=nil;

 FCurrPipeline[BP_GRAPHICS]:=VK_NULL_HANDLE;
 FCurrPipeline[BP_COMPUTE ]:=VK_NULL_HANDLE;

 r:=vkEndCommandBuffer(FCmdbuf);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkEndCommandBuffer:',r);
 end;

 FCBState:=cbEnd;
end;

Procedure TvCustomCmdBuffer.BindPipeline(BindPoint:TVkPipelineBindPoint;F:TVkPipeline);
begin
 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (FCurrPipeline[BindPoint]=F) then Exit;

 if (not BeginCmdBuffer) then Exit;

 Inc(cmd_count);

 vkCmdBindPipeline(FCmdbuf,BindPoint,F);
 FCurrPipeline[BindPoint]:=F;
end;

function TvCmdBuffer.BeginRenderPass(RT:PvRenderPassBeginInfo;GP:TvGraphicsPipeline2):Boolean;
var
 rinfo:TVkRenderPassBeginInfo;
 ainfo:TVkRenderPassAttachmentBeginInfo;
begin
 Result:=False;

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (RT=nil) then Exit;

 if (RT^.FRenderPass =nil) then Exit;
 if (RT^.FFramebuffer=nil) then Exit;
 if (GP=nil) then Exit;

 if (not BeginCmdBuffer) then Exit;

 if (FRenderPass=VK_NULL_HANDLE) or
    (CompareByte(FRender,RT^,SizeOf(TvRenderPassBeginInfo))<>0) then
 begin
  //reset render pass
  EndRenderPass;

  FRender:=RT^;

  rinfo:=FRender.GetRInfo;

  if FRender.FFramebuffer.IsImageless then
  begin
   ainfo:=FRender.GetAInfo;
   rinfo.pNext:=@ainfo;
  end;

  Inc(cmd_count);

  //start render pass
  vkCmdBeginRenderPass(FCmdbuf,@rinfo,VK_SUBPASS_CONTENTS_INLINE);
  FRenderPass:=rinfo.renderPass;
 end;

 Femulate_primtype:=GP.Key.emulate_primtype;

 BindPipeline(BP_GRAPHICS,GP.FHandle);
 BindLayout  (BP_GRAPHICS,GP.Key.FShaderGroup.FLayout);

 RefTo(FRender.FRenderPass );
 RefTo(FRender.FFramebuffer);
 RefTo(GP);

 Result:=True;
end;

Function TvCustomCmdBuffer.IsRenderPass:Boolean;
begin
 Result:=False;

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 Result:=(FRenderPass<>VK_NULL_HANDLE);
end;

Procedure TvCustomCmdBuffer.EndRenderPass;
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (FCmdbuf=VK_NULL_HANDLE) then Exit;
 if (FRenderPass<>VK_NULL_HANDLE) then
 begin
  Inc(cmd_count);
  vkCmdEndRenderPass(FCmdbuf);
  FRenderPass:=VK_NULL_HANDLE;
  //
  FCurrLayout[BP_GRAPHICS]:=nil;
  FCurrBinds [BP_GRAPHICS]:=nil;
  FCurrGroup [BP_GRAPHICS]:=nil;
  //
  FCurrPipeline[BP_GRAPHICS]:=VK_NULL_HANDLE;
 end;
end;

function TvCmdBuffer.BindCompute(CP:TvComputePipeline2):Boolean;
begin
 Result:=False;

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (CP=nil) then Exit;

 if (not BeginCmdBuffer) then Exit;

 BindPipeline(BP_COMPUTE,CP.FHandle);
 BindLayout  (BP_COMPUTE,CP.Key.FShaderGroup.FLayout);

 RefTo(CP);

 Result:=True;
end;

function TvCustomCmdBuffer.QueueSubmit:TVkResult;
var
 info:TVkSubmitInfo;

 FFenceHandle:TVkFence;

 FHandles:array of TVkSemaphore;
 FStages :array of TVkPipelineStageFlags;

 i:Integer;
 t:PvSemaphoreWait;
begin
 Result:=VK_ERROR_UNKNOWN;

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 case FCBState of
  cbFree,
  cbInit:
   begin
    //zero cmd buffer
    Exit(VK_SUCCESS);
   end;
  cbSubmit:
   begin
    //cmd buffer submitted
    Exit(VK_SUCCESS);
   end;
  else;
 end;

 if (FCmdbuf=VK_NULL_HANDLE) then Exit;

 EndCmdBuffer;

 info:=Default(TVkSubmitInfo);
 info.sType              :=VK_STRUCTURE_TYPE_SUBMIT_INFO;
 info.commandBufferCount :=1;
 info.pCommandBuffers    :=@FCmdbuf;

 if (FWaitSemaphoresCount<>0) then
 begin
  FHandles:=nil;
  FStages :=nil;
  SetLength(FHandles,FWaitSemaphoresCount);
  SetLength(FStages ,FWaitSemaphoresCount);

  i:=0;
  t:=FWaitSemaphores.Min;
  While (t<>nil) do
  begin
   FHandles[i]:=t^.FSemaphore.FHandle;
   FStages [i]:=t^.FWaitStage;
   Inc(i);
   //
   t:=FWaitSemaphores.Next(t);
  end;

  info.waitSemaphoreCount:=i;
  info.pWaitSemaphores   :=@FHandles[0];
  info.pWaitDstStageMask :=@FStages[0];

 end;

 if (FSignalSemaphore<>nil) then
 begin
  info.signalSemaphoreCount:=1;
  info.pSignalSemaphores   :=@FSignalSemaphore.FHandle;
 end;

 FFenceHandle:=VK_NULL_HANDLE;
 if (FFence<>nil) then
 begin
  FFenceHandle:=FFence.FHandle;
 end;

 Result:=FQueue.Submit(1,@info,FFenceHandle);

 ret:=Integer(Result);
 if (Result<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkQueueSubmit:',Result);
 end;

 FCBState:=cbSubmit;
end;

function TvCustomCmdBuffer.Wait(timeout:TVkUInt64):TVkResult;
begin
 Result:=VK_ERROR_UNKNOWN;

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (FFence=nil) then Exit;

 case FCBState of
  cbFree,
  cbInit:
   begin
    //zero cmd buffer
    Exit(VK_SUCCESS);
   end;
  cbSubmit:
   begin
    //cmd buffer submitted
    Result:=FFence.Wait(timeout);
   end;
  else
   //idk why it called in this state
   Exit(VK_ERROR_UNKNOWN);
 end;

end;

function TvCustomCmdBuffer.Status:TVkResult;
begin
 Result:=VK_ERROR_UNKNOWN;

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (FFence=nil) then Exit;

 case FCBState of
  cbFree,
  cbInit:
   begin
    //zero cmd buffer
    Exit(VK_SUCCESS);
   end;
  cbSubmit:
   begin
    //cmd buffer submitted
    Result:=FFence.Status;
   end;
  else
   //idk why it called in this state
   Exit(VK_ERROR_UNKNOWN);
 end;

end;

Procedure TvCustomCmdBuffer.ReleaseResource;
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 ReleaseAllDependencies(Self);

 ReleaseAllPlannedTriggers;

 FreeAllDescriptorCache;

 FreeAllSemaphores;

 cmd_count:=0;
 ret:=0;
end;

Procedure TvCustomCmdBuffer.FreeAllSemaphores;
var
 node:PvSemaphoreWait;
begin
 if IsLinearAlloc then
 begin
  FWaitSemaphores:=Default(TvSemaphoreWaitSet);
 end else
 begin
  node:=FWaitSemaphores.Min;

  while (node<>nil) do
  begin
   FWaitSemaphores.delete(node);
   OnFree(node);

   node:=FWaitSemaphores.Min;
  end;
 end;

 FWaitSemaphoresCount:=0;
end;

Procedure TvCustomCmdBuffer.AddWaitSemaphore(S:TvSemaphore;W:TVkPipelineStageFlags);
Var
 node:PvSemaphoreWait;
begin
 if (S=nil) then Exit;

 node:=FWaitSemaphores.Find(@S);

 if (node=nil) then
 begin
  node:=OnAlloc(SizeOf(TvSemaphoreWait));
  node^.FSemaphore:=S;
  node^.FWaitStage:=W;
  //
  FWaitSemaphores.Insert(node);
  //
  Inc(FWaitSemaphoresCount);
 end;

end;

function TvCustomCmdBuffer.GetSignaledSemaphore:TvSemaphore;
begin
 Result:=FSignalSemaphore;
end;

Procedure TvCustomCmdBuffer.ReleaseAllPlannedTriggers;
var
 node:p_cmd_track_deferred;
begin
 node:=FPlannedTriggers.Min;

 while (node<>nil) do
 begin

  //deffered trigger
  vm_map_track_trigger(p_proc.p_vmspace,node^.start,node^.__end,node^.exclude,M_GPU_APPLY);

  if IsLinearAlloc then
  begin
   node:=FPlannedTriggers.Next(node);
  end else
  begin
   FPlannedTriggers.delete(node);
   OnFree(node);
   //
   node:=FPlannedTriggers.Min;
  end;

 end;

 if IsLinearAlloc then
 begin
  FPlannedTriggers:=Default(t_cmd_track_deferred_set);
 end;
end;

Procedure TvCustomCmdBuffer.AddPlannedTrigger(start,__end:QWORD;exclude:Pointer);
Var
 tmp:t_cmd_track_deferred;
 node:p_cmd_track_deferred;
begin
 //planned trigger
 vm_map_track_trigger(p_proc.p_vmspace,start,__end,exclude,M_GPU_PLANNED);

 tmp:=Default(t_cmd_track_deferred);
 tmp.start  :=start;
 tmp. __end :=__end;
 tmp.exclude:=exclude;

 node:=FPlannedTriggers.Find(@tmp);

 if (node=nil) then
 begin
  node:=OnAlloc(SizeOf(t_cmd_track_deferred));
  node^:=tmp;
  //
  FPlannedTriggers.Insert(node);
 end;

end;

Procedure TvCustomCmdBuffer.FreeAllDescriptorCache;
var
 node:PvDescriptorCache;
begin
 if IsLinearAlloc then
 begin
  FDescriptorCacheSet:=Default(TvDescriptorCacheSet);
 end else
 begin
  node:=FDescriptorCacheSet.Min;

  while (node<>nil) do
  begin
   FDescriptorCacheSet.delete(node);
   OnFree(node);

   node:=FDescriptorCacheSet.Min;
  end;
 end;
end;

Procedure TvCustomCmdBuffer.BindLayout(BindPoint:TVkPipelineBindPoint;F:TvPipelineLayout);
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (F=nil) then Exit;

 if (FCurrLayout[BindPoint]<>F) then
 begin
  FCurrLayout[BindPoint]:=F;
  FCurrBinds [BindPoint]:=nil;
  FCurrGroup [BindPoint]:=nil;
 end;
end;

Procedure TvCustomCmdBuffer.BindSet(BindPoint:TVkPipelineBindPoint;fset:TVkUInt32;FHandle:TVkDescriptorSet);
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (FHandle=VK_NULL_HANDLE) then Exit;
 if (FCurrLayout[BindPoint]=nil) then Exit;

 if (not BeginCmdBuffer) then Exit;

 Inc(cmd_count);

 vkCmdBindDescriptorSets(FCmdbuf,
                         BindPoint,
                         FCurrLayout[BindPoint].FHandle,
                         fset,1,
                         @FHandle,
                         0,nil);

end;

Procedure TvCustomCmdBuffer.PushConstant(BindPoint:TVkPipelineBindPoint;stageFlags:TVkShaderStageFlags;offset,size:TVkUInt32;const pValues:PVkVoid);
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (pValues=nil) or (size=0) then Exit;
 if (FCurrLayout[BindPoint]=nil) then Exit;

 if (not BeginCmdBuffer) then Exit;

 Inc(cmd_count);

 vkCmdPushConstants(FCmdbuf,
                    FCurrLayout[BindPoint].FHandle,
                    stageFlags,
                    offset,size,
                    pValues);

end;

Procedure TvCustomCmdBuffer.DispatchDirect(X,Y,Z:TVkUInt32);
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (FCurrPipeline[BP_COMPUTE]=VK_NULL_HANDLE) then Exit;

 if (not BeginCmdBuffer) then Exit;

 ApplyDescriptorCache(BP_COMPUTE);

 Inc(cmd_count);

 vkCmdDispatch(FCmdbuf,X,Y,Z);
end;

Procedure TvCustomCmdBuffer.BindVertexBuffers(const FAttrBuilder:TvAttrBuilder);
var
 i,c:Integer;

 rb:TvHostBuffer;

 Buffers:array[0..31] of TVkBuffer;
 Offsets:array[0..31] of TVkDeviceSize;

 last_binding:TVkUInt32;
 last_size   :TVkUInt32;
begin
 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 c:=FAttrBuilder.FBindDescsCount;
 if (c=0) then Exit;

 if (not BeginCmdBuffer) then Exit;

 last_binding:=0;
 last_size   :=0;
 For i:=0 to c-1 do
 With FAttrBuilder.FBindVBufs[i] do
 begin

  rb:=FetchHostBuffer(Self,QWORD(min_addr),GetSize,ord(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT));

  if (last_binding<>binding) then
  begin
   //flush
   if (last_size<>0) then
   begin
    vkCmdBindVertexBuffers(FCmdbuf,last_binding,last_size,@Buffers,@Offsets);
    last_size:=0;
   end;
   //
   last_binding:=binding;
  end;

  Buffers[last_size]:=rb.FHandle;
  Offsets[last_size]:=QWORD(min_addr)-rb.FAddr;

  last_size:=last_size+1;
 end;

 //flush
 if (last_size<>0) then
 begin
  vkCmdBindVertexBuffers(FCmdbuf,last_binding,last_size,@Buffers,@Offsets);
 end;
end;

Procedure TvCustomCmdBuffer.SetVertexInput(const FAttrBuilder:TvAttrBuilder);
var
 input:TvVertexInputEXT;
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if not limits.VK_EXT_vertex_input_dynamic_state then Exit;

 if (not BeginCmdBuffer) then Exit;

 FAttrBuilder.Export2(input);

 if (vkCmdSetVertexInputEXT=nil) then
 begin
  TPFN_vkVoidFunction(vkCmdSetVertexInputEXT):=vkGetInstanceProcAddr(VulkanApp.FInstance,'vkCmdSetVertexInputEXT');
 end;

 vkCmdSetVertexInputEXT(FCmdbuf,
                        input.vertexBindingDescriptionCount,
                        @input.VertexBindingDescriptions[0],
                        input.vertexAttributeDescriptionCount,
                        @input.VertexAttributeDescriptions[0]);
end;

Procedure TvCustomCmdBuffer.ClearDepthStencilImage(image:TVkImage;
                                                   imageLayout:TVkImageLayout;
                                                   const pDepthStencil:PVkClearDepthStencilValue;
                                                   rangeCount:TVkUInt32;
                                                   const pRanges:PVkImageSubresourceRange);
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 EndRenderPass;
 if (not BeginCmdBuffer) then Exit;

 Inc(cmd_count);

 vkCmdClearDepthStencilImage(
   FCmdbuf,
   image,
   imageLayout,
   pDepthStencil,
   rangeCount,
   pRanges);

end;

Procedure TvCustomCmdBuffer.ClearDepthStencilImage(image:TVkImage;
                                                   imageLayout:TVkImageLayout;
                                                   const pDepthStencil:PVkClearDepthStencilValue;
                                                   const range:TVkImageSubresourceRange);
begin
 ClearDepthStencilImage(image,
                        imageLayout,
                        pDepthStencil,
                        1,
                        @range);
end;

Procedure TvCustomCmdBuffer.ClearColorImage(image:TVkImage;
                                            imageLayout:TVkImageLayout;
                                            const pColor:PVkClearColorValue;
                                            rangeCount:TVkUInt32;
                                            const pRanges:PVkImageSubresourceRange);
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 EndRenderPass;
 if (not BeginCmdBuffer) then Exit;

 Inc(cmd_count);

 vkCmdClearColorImage(
  FCmdbuf,
  image,
  imageLayout,
  pColor,
  rangeCount,
  pRanges);
end;

Procedure TvCustomCmdBuffer.ResolveImage(srcImage:TVkImage;
                                         srcImageLayout:TVkImageLayout;
                                         dstImage:TVkImage;
                                         dstImageLayout:TVkImageLayout;
                                         regionCount:TVkUInt32;
                                         const pRegions:PVkImageResolve);
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 EndRenderPass;
 if (not BeginCmdBuffer) then Exit;

 Inc(cmd_count);

 vkCmdResolveImage(
  FCmdbuf,
  srcImage,
  srcImageLayout,
  dstImage,
  dstImageLayout,
  regionCount,
  pRegions);
end;

procedure TvCustomCmdBuffer.CopyBufferToImage(srcBuffer:TVkBuffer;
                                              dstImage:TVkImage;
                                              dstImageLayout:
                                              TVkImageLayout;
                                              regionCount:TVkUInt32;
                                              const pRegions:PVkBufferImageCopy);
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 EndRenderPass;
 if (not BeginCmdBuffer) then Exit;

 Inc(cmd_count);

 vkCmdCopyBufferToImage(
  FCmdbuf,
  srcBuffer,
  dstImage,
  dstImageLayout,
  regionCount,
  pRegions);
end;

procedure TvCustomCmdBuffer.CopyImageToBuffer(srcImage:TVkImage;
                                              srcImageLayout:TVkImageLayout;
                                              dstBuffer:TVkBuffer;
                                              regionCount:TVkUInt32;
                                              const pRegions:PVkBufferImageCopy);
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 EndRenderPass;
 if (not BeginCmdBuffer) then Exit;

 Inc(cmd_count);

 vkCmdCopyImageToBuffer(
  FCmdbuf,
  srcImage,
  srcImageLayout,
  dstBuffer,
  regionCount,
  pRegions);
end;

procedure TvCustomCmdBuffer.BufferMemoryBarrier(buffer:TVkBuffer;
                                                srcAccessMask:TVkAccessFlags;
                                                dstAccessMask:TVkAccessFlags;
                                                offset,size:TVkDeviceSize;
                                                srcStageMask:TVkPipelineStageFlags;
                                                dstStageMask:TVkPipelineStageFlags);
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (not BeginCmdBuffer) then Exit;

 Inc(cmd_count);

 vkBufferMemoryBarrier(FCmdbuf,
                       buffer,
                       srcAccessMask,
                       dstAccessMask,
                       offset,size,
                       srcStageMask,
                       dstStageMask);
end;

Const
 VK_ACCESS_ANY=
  ord(VK_ACCESS_INDIRECT_COMMAND_READ_BIT         ) or
  ord(VK_ACCESS_INDEX_READ_BIT                    ) or
  ord(VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT         ) or
  ord(VK_ACCESS_UNIFORM_READ_BIT                  ) or
  ord(VK_ACCESS_INPUT_ATTACHMENT_READ_BIT         ) or
  ord(VK_ACCESS_SHADER_READ_BIT                   ) or
  ord(VK_ACCESS_SHADER_WRITE_BIT                  ) or
  ord(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT         ) or
  ord(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT        ) or
  ord(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT ) or
  ord(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT) or
  ord(VK_ACCESS_TRANSFER_READ_BIT                 ) or
  ord(VK_ACCESS_TRANSFER_WRITE_BIT                ) or
  ord(VK_ACCESS_HOST_READ_BIT                     ) or
  ord(VK_ACCESS_HOST_WRITE_BIT                    ) or
  ord(VK_ACCESS_MEMORY_READ_BIT                   ) or
  ord(VK_ACCESS_MEMORY_WRITE_BIT                  );

Procedure TvCmdBuffer.dmaData1(src,dst:Pointer;byteCount:DWORD;isBlocking:Boolean);
var
 srcb,dstb:TvHostBuffer;
 info:TVkBufferCopy;
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 EndRenderPass;
 if (not BeginCmdBuffer) then Exit;

 DebugReport.CmdBeginLabel(FCmdbuf,'dmaData');

 srcb:=FetchHostBuffer(Self,QWORD(src),byteCount,ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT));
 Assert(srcb<>nil);

 dstb:=FetchHostBuffer(Self,QWORD(dst),byteCount,ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT));
 Assert(dstb<>nil);

 info:=Default(TVkBufferCopy);
 info.srcOffset:=QWORD(src)-srcb.FAddr;
 info.dstOffset:=QWORD(dst)-dstb.FAddr;
 info.size     :=byteCount;

 Inc(cmd_count);

 vkBufferMemoryBarrier(FCmdbuf,
                       srcb.FHandle,
                       VK_ACCESS_ANY,
                       ord(VK_ACCESS_TRANSFER_READ_BIT),
                       info.srcOffset,byteCount,
                       ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT),
                       ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 Inc(cmd_count);

 vkBufferMemoryBarrier(FCmdbuf,
                       dstb.FHandle,
                       VK_ACCESS_ANY,
                       ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                       info.dstOffset,byteCount,
                       ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT),
                       ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 Inc(cmd_count);

 vkCmdCopyBuffer(FCmdbuf,
                 srcb.FHandle,
                 dstb.FHandle,
                 1,@info);

 if isBlocking then
 begin
  Inc(cmd_count);

  vkMemoryBarrier(FCmdbuf,
                  ord(VK_ACCESS_TRANSFER_WRITE_BIT),        //srcAccessMask
                  VK_ACCESS_ANY,                            //dstAccessMask
  	          ord(VK_PIPELINE_STAGE_TRANSFER_BIT),      //srcStageMask
  	          ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT)); //dstStageMask
 end;

 DebugReport.CmdEndLabel(FCmdbuf);

 AddPlannedTrigger(QWORD(dst),QWORD(dst)+byteCount,nil);
end;

Procedure TvCmdBuffer.dmaData2(src:DWORD;dst:Pointer;byteCount:DWORD;isBlocking:Boolean);
var
 dstb:TvHostBuffer;
 dstOffset:TVkDeviceSize;
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 byteCount:=byteCount and (not 3); //4 byte align

 EndRenderPass;
 if (not BeginCmdBuffer) then Exit;

 DebugReport.CmdBeginLabel(FCmdbuf,'dmaData');

 dstb:=FetchHostBuffer(Self,QWORD(dst),byteCount,ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT));
 Assert(dstb<>nil);

 dstOffset:=QWORD(dst)-dstb.FAddr;

 Inc(cmd_count);

 vkBufferMemoryBarrier(FCmdbuf,
                       dstb.FHandle,
                       VK_ACCESS_ANY,
                       ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                       dstOffset,byteCount,
                       ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT),
                       ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 Inc(cmd_count);

 vkCmdFillBuffer(FCmdbuf,
                 dstb.FHandle,
                 dstOffset,
                 byteCount,
                 src);

 if isBlocking then
 begin
  Inc(cmd_count);

  vkMemoryBarrier(FCmdbuf,
                  ord(VK_ACCESS_TRANSFER_WRITE_BIT),        //srcAccessMask
                  VK_ACCESS_ANY,                            //dstAccessMask
  	          ord(VK_PIPELINE_STAGE_TRANSFER_BIT),      //srcStageMask
  	          ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT)); //dstStageMask
 end;

 DebugReport.CmdEndLabel(FCmdbuf);

 AddPlannedTrigger(QWORD(dst),QWORD(dst)+byteCount,nil);
end;

const
 VK_ACCESS_CS=
  ord(VK_ACCESS_UNIFORM_READ_BIT) or
  ord(VK_ACCESS_SHADER_READ_BIT ) or
  ord(VK_ACCESS_SHADER_WRITE_BIT);

 VK_ACCESS_PS=
  ord(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT ) or
  ord(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);

Procedure TvCmdBuffer.WriteEos(eventType:Byte;dst:Pointer;value:DWORD;isBlocking:Boolean);
var
 rb:TvHostBuffer;
 BufOffset:TVkDeviceSize;
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 EndRenderPass;
 if (not BeginCmdBuffer) then Exit;

 DebugReport.CmdBeginLabel(FCmdbuf,'WriteEos');

 Case eventType of
  CS_DONE:
   begin
    Inc(cmd_count);

    DebugReport.CmdInsertLabel(FCmdbuf,'CS_DONE');

    vkMemoryBarrier(FCmdbuf,
                    VK_ACCESS_CS,                              //srcAccessMask
                    ord(VK_ACCESS_TRANSFER_WRITE_BIT),         //dstAccessMask
    	            ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT), //srcStageMask
    	            ord(VK_PIPELINE_STAGE_TRANSFER_BIT));      //dstStageMask
   end;
  PS_DONE:
   begin
    Inc(cmd_count);

    DebugReport.CmdInsertLabel(FCmdbuf,'PS_DONE');

    vkMemoryBarrier(FCmdbuf,
                    VK_ACCESS_PS,                                       //srcAccessMask
                    ord(VK_ACCESS_TRANSFER_WRITE_BIT),                  //dstAccessMask
    	            ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT), //srcStageMask
    	            ord(VK_PIPELINE_STAGE_TRANSFER_BIT));               //dstStageMask
   end;
  else
   Assert(false,'WriteEos.eventType');
 end;

 rb:=FetchHostBuffer(Self,QWORD(dst),4,ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT));
 Assert(rb<>nil);

 BufOffset:=QWORD(dst)-rb.FAddr;

 Inc(cmd_count);

 vkCmdFillBuffer(FCmdbuf,
                 rb.FHandle,
                 BufOffset,
                 4,value);

 if isBlocking then
 begin
  Inc(cmd_count);

  vkMemoryBarrier(FCmdbuf,
                  ord(VK_ACCESS_TRANSFER_WRITE_BIT),        //srcAccessMask
                  VK_ACCESS_ANY,                            //dstAccessMask
  	          ord(VK_PIPELINE_STAGE_TRANSFER_BIT),      //srcStageMask
  	          ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT)); //dstStageMask
 end;

 DebugReport.CmdEndLabel(FCmdbuf);

 AddPlannedTrigger(QWORD(dst),QWORD(dst)+4,nil);
end;

const
 VK_ACCESS_DB=
  ord(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT ) or
  ord(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT);

 VK_STAGE_DB=
  ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
  ord(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or
  ord(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT);

Procedure TvCmdBuffer.WriteEvent(eventType:Byte);
begin
 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 EndRenderPass;
 if (not BeginCmdBuffer) then Exit;

 Case eventType of
  FLUSH_AND_INV_DB_META: //HTILE
   begin
    Inc(cmd_count);

    DebugReport.CmdInsertLabel(FCmdbuf,'FLUSH_AND_INV_DB_META');

    vkMemoryBarrier(FCmdbuf,
                    VK_ACCESS_DB,                             //srcAccessMask
                    VK_ACCESS_ANY,                            //dstAccessMask
    	            VK_STAGE_DB,                              //srcStageMask
    	            ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT)); //dstStageMask

   end;
  FLUSH_AND_INV_CB_META: //CMASK
   begin
    Inc(cmd_count);

    DebugReport.CmdInsertLabel(FCmdbuf,'FLUSH_AND_INV_CB_META');

    vkMemoryBarrier(FCmdbuf,
                    VK_ACCESS_PS,                                       //srcAccessMask
                    ord(VK_ACCESS_TRANSFER_WRITE_BIT),                  //dstAccessMask
    	            ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT), //srcStageMask
    	            ord(VK_PIPELINE_STAGE_TRANSFER_BIT));               //dstStageMask

   end;

  else
   Assert(false,'WriteEvent.eventType');
 end;

end;

Procedure TvCustomCmdBuffer.InsertLabel(pLabelName:PVkChar);
begin
 if (Self=nil) then Exit;
 if (DebugReport.FCmdInsertDebugUtilsLabel=nil) then Exit;

 if (not BeginCmdBuffer) then Exit;

 DebugReport.CmdInsertLabel(FCmdbuf,pLabelName);
end;

Procedure TvCustomCmdBuffer.BeginLabel(pLabelName:PVkChar);
begin
 if (Self=nil) then Exit;
 if (DebugReport.FCmdBeginDebugUtilsLabel=nil) then Exit;

 if (not BeginCmdBuffer) then Exit;

 DebugReport.CmdBeginLabel(FCmdbuf,pLabelName);
end;

Procedure TvCustomCmdBuffer.EndLabel();
begin
 if (Self=nil) then Exit;

 if (not IsAllocated) then Exit;

 DebugReport.CmdEndLabel(FCmdbuf);
end;

function GET_INDEX_TYPE_SIZE(INDEX_TYPE:TVkIndexType):Byte;
begin
 Case INDEX_TYPE of
  VK_INDEX_TYPE_UINT16   :Result:=16;
  VK_INDEX_TYPE_UINT32   :Result:=32;
  VK_INDEX_TYPE_UINT8_EXT:Result:=8;
  else                    Result:=0;
 end;
end;

Procedure TvCmdBuffer.DrawIndexOffset2(IndexBase:Pointer;indexOffset,indexCount:DWORD);
var
 rb:TvHostBuffer;
 Size:TVkDeviceSize;
 BufOffset:TVkDeviceSize;
 i,h:DWORD;
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (FRenderPass=VK_NULL_HANDLE) then Exit;
 if (FCurrPipeline[BP_GRAPHICS]=VK_NULL_HANDLE) then Exit;

 if (not BeginCmdBuffer) then Exit;

 ApplyDescriptorCache(BP_GRAPHICS);

 if (FinstanceCount=0) then FinstanceCount:=1;

 Size:=(indexOffset+indexCount)*GET_INDEX_TYPE_SIZE(FINDEX_TYPE);

 rb:=FetchHostBuffer(Self,QWORD(IndexBase),Size,ord(VK_BUFFER_USAGE_INDEX_BUFFER_BIT));
 Assert(rb<>nil);

 Inc(cmd_count);

 BufOffset:=QWORD(IndexBase)-rb.FAddr;

 vkCmdBindIndexBuffer(
     Fcmdbuf,
     rb.FHandle,
     BufOffset,
     FINDEX_TYPE);

 Inc(cmd_count);

 Case Femulate_primtype of
  0:
    begin
     vkCmdDrawIndexed(
         Fcmdbuf,
         indexCount,     //indexCount
         FinstanceCount, //instanceCount
         indexOffset,    //firstIndex
         0,              //vertexOffset
         0);             //firstInstance
    end;
  DI_PT_QUADLIST:
    begin
     Assert(FinstanceCount<=1,'instance DI_PT_QUADLIST');
     Assert(indexOffset=0,'OFFSET DI_PT_QUADLIST');
     h:=indexCount div 4;
     if (h>0) then h:=h-1;
     For i:=0 to h do
     begin
      vkCmdDrawIndexed(
       Fcmdbuf,
       4,      //indexCount
       1,      //instanceCount
       i*4,    //firstIndex
       0,      //vertexOffset
       0);     //firstInstance
     end;
    end;
  else
   Assert(false,'TODO');
 end;

end;

Procedure TvCmdBuffer.DrawIndex2(IndexBase:Pointer;indexCount:DWORD);
begin
 DrawIndexOffset2(IndexBase,0,indexCount);
end;

Procedure TvCmdBuffer.DrawIndexAuto(indexCount:DWORD);
var
 i,h:DWORD;
begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (FRenderPass=VK_NULL_HANDLE) then Exit;
 if (FCurrPipeline[BP_GRAPHICS]=VK_NULL_HANDLE) then Exit;

 if (not BeginCmdBuffer) then Exit;

 ApplyDescriptorCache(BP_GRAPHICS);

 if (FinstanceCount=0) then FinstanceCount:=1;

 Case Femulate_primtype of
  0:
    begin
     vkCmdDraw(
      FCmdbuf,
      indexCount,     //vertexCount
      FinstanceCount, //instanceCount
      0,              //firstVertex
      0);             //firstInstance
    end;

  DI_PT_RECTLIST :
    begin
     Assert(FinstanceCount<=1,'instance DI_PT_RECTLIST');
     {
     VK_EXT_primitive_topology_list_restart ???

     0   3
     1   2
     }
     //0 1 2
     //0 2 3

     h:=indexCount div 3;
     if (h>0) then h:=h-1;
     For i:=0 to h do
     begin
      Inc(cmd_count);
      vkCmdDraw(
          FCmdbuf,
          4,       //vertexCount
          1,       //instanceCount
          0,       //firstVertex
          0);      //firstInstance
     end;

    end;
  //DI_PT_LINELOOP :;
  DI_PT_QUADLIST:
    begin
     Assert(FinstanceCount<=1,'instance DI_PT_QUADLIST');
     h:=indexCount div 4;
     if (h>0) then h:=h-1;
     For i:=0 to h do
     begin
      Inc(cmd_count);
      vkCmdDraw(
          FCmdbuf,
          4,       //vertexCount
          1,       //instanceCount
          i*4,     //firstVertex
          0);      //firstInstance
     end;
    end;
  //DI_PT_QUADSTRIP:;
  //DI_PT_POLYGON  :;
  else
    begin
     Assert(false,'TODO');
    end;
 end;

end;

//

function TvCustomCmdBuffer.FetchDescriptorCache(layout:TvPipelineLayout):PvDescriptorCache;
begin
 Result:=FDescriptorCacheSet.Find(@layout);

 if (Result=nil) then
 begin
  Result:=AllocDescriptorCache(Self,layout);
  FDescriptorCacheSet.Insert(Result);
 end;
end;

function TvCustomCmdBuffer.FetchDescriptorCache(BindPoint:TVkPipelineBindPoint):PvDescriptorCache;
begin
 Result:=nil;

 if (FCurrLayout[BindPoint]=nil) then Exit;

 Result:=FCurrBinds[BindPoint];

 if (Result=nil) then
 begin
  Result:=FetchDescriptorCache(FCurrLayout[BindPoint]);
  //
  FCurrBinds[BindPoint]:=Result;
  //
  Result^.SetAllChange;
 end;
end;

function TvCustomCmdBuffer.FetchDescriptorInterface(BindPoint:TVkPipelineBindPoint):TvDescriptorInterface;
begin
 Result:=Default(TvDescriptorInterface);

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 Result:=TvDescriptorInterface(FetchDescriptorCache(BindPoint));
end;

Procedure TvCustomCmdBuffer.ApplyDescriptorCache(BindPoint:TVkPipelineBindPoint);
var
 Cache:PvDescriptorCache;
 Group:TvDescriptorGroup;
begin
 if (FCurrLayout[BindPoint]=nil) then Exit;

 Cache:=FCurrBinds[BindPoint];
 if (Cache=nil) then Exit;

 if (Cache^.p_count_all=0) then Exit; //no sets

 Group:=FCurrGroup[BindPoint];

 if (Cache^.p_change_any) or
    (Group=nil) then
 begin
  Group:=FetchDescriptorGroup(Self,FCurrLayout[BindPoint]);
  //
  FCurrGroup[BindPoint]:=Group;
  //
  Group.Bind(Cache);
  //
  BindSets(BindPoint,Group);
  //
  Cache^.ClearAllChange;
 end;

 //VK_KHR_push_descriptor vkCmdPushDescriptorSetKHR TODO
end;

Procedure TvCustomCmdBuffer.BindSets(BindPoint:TVkPipelineBindPoint;F:TvDescriptorGroup);
var
 A:array[0..6] of TVkDescriptorSet;
 i,start,pos:Integer;

 procedure Flush; inline;
 begin
  Inc(cmd_count);

  vkCmdBindDescriptorSets(FCmdbuf,
                          BindPoint,
                          FCurrLayout[BindPoint].FHandle,
                          start,pos,
                          @A[0],
                          0,nil);

  pos:=0;
 end;

begin

 if (Self=nil) then
 begin
  Writeln(stderr,'Self=nil,',{$I %LINE%});
  Exit;
 end;

 if (F=nil) then Exit;
 if (FCurrLayout[BindPoint]=nil) then Exit;
 if (Length(F.FSets)=0) then Exit;

 if (not BeginCmdBuffer) then Exit;

 pos:=0;

 For i:=0 to High(F.FSets) do
 begin
  if F.FSets[i].IsValid then
  begin

   if (pos=0) then
   begin
    start:=i;
   end;

   A[pos]:=F.FSets[i].FHandle;
   Inc(pos);

   if (pos=7) then
   begin
    Flush;
   end;

  end else
  if (pos<>0) then
  begin
   Flush;
  end;
 end;

 if (pos<>0) then
 begin
  Flush;
 end;
end;

//





end.




