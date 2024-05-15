unit vCmdBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  //ps4_types,
  g23tree,
  //ps4_libSceVideoOut,
  si_ci_vi_merged_enum,
  Vulkan,
  vDependence,
  vDevice,
  vMemory,
  //vShader,
  vShaderExt,
  vImage,
  vPipeline,
  //vSetsPools,
  vRender;

type
 TvCmdBuffer=class;

 TvSemaphoreWait=record
  FSemaphore:TvSemaphore;
  FWaitStage:TVkPipelineStageFlags;
 end;

 TvSemaphoreWaitCompare=object
  function c(a,b:TvSemaphoreWait):Integer; static;
 end;

 TvSemaphoreWaitSet=specialize T23treeSet<TvSemaphoreWait,TvSemaphoreWaitCompare>;

 TvCustomCmdBuffer=class(TvDependenciesObject)
  FRefs  :ptruint;

  FParent:TvCmdPool;
  FQueue :TvQueue;
  FCmdbuf:TVkCommandBuffer;

  cmd_count:qword;
  ret:Integer;

  submit_id:ptruint;

  FCurrPipeline:array[0..1] of TVkPipeline;
  FCurrLayout  :array[0..1] of TVkPipelineLayout;

  FRenderPass:TVkRenderPass;

  FWaitSemaphores:TvSemaphoreWaitSet;

  FSignalSemaphore:TvSemaphore;

  FFence:TvFence;

  FCBState:Boolean;

  Constructor Create(pool:TvCmdPool;Queue:TvQueue);
  Destructor  Destroy; override;

  Procedure   Acquire;
  procedure   Release;
  procedure   OnReleaseCmd(Sender:TObject);

  function    BeginCmdBuffer:Boolean;
  Procedure   EndCmdBuffer;
  Procedure   BindPipeline(BindPoint:TVkPipelineBindPoint;F:TVkPipeline);
  Function    IsRenderPass:Boolean;
  Procedure   EndRenderPass;

  function    QueueSubmit:TVkResult;
  function    Wait(timeout:TVkUInt64):TVkResult;

  Procedure   ReleaseResource;
  Procedure   AddWaitSemaphore(S:TvSemaphore;W:TVkPipelineStageFlags);
  function    GetSignaledSemaphore:TvSemaphore;

  Procedure   BindLayout(BindPoint:TVkPipelineBindPoint;F:TvPipelineLayout);
  Procedure   BindSet(BindPoint:TVkPipelineBindPoint;fset:TVkUInt32;FHandle:TVkDescriptorSet);
  Procedure   PushConstant(BindPoint:TVkPipelineBindPoint;stageFlags:TVkShaderStageFlags;offset,size:TVkUInt32;const pValues:PVkVoid);
  Procedure   DispatchDirect(X,Y,Z:TVkUInt32);

  Procedure   BindVertexBuffers(const FAttrBuilder:TvAttrBuilder);
  Procedure   SetVertexInput(const FAttrBuilder:TvAttrBuilder);

  Procedure   ClearDepthStencilImage(
                         image:TVkImage;
                         imageLayout:TVkImageLayout;
                         const pDepthStencil:PVkClearDepthStencilValue;
                         rangeCount:TVkUInt32;
                         const pRanges:PVkImageSubresourceRange);

  Procedure   ClearColorImage(
                         image:TVkImage;
                         imageLayout:TVkImageLayout;
                         const pColor:PVkClearColorValue;
                         rangeCount:TVkUInt32;
                         const pRanges:PVkImageSubresourceRange);

  Procedure   ResolveImage(
                         srcImage:TVkImage;
                         srcImageLayout:TVkImageLayout;
                         dstImage:TVkImage;
                         dstImageLayout:TVkImageLayout;
                         regionCount:TVkUInt32;
                         const pRegions:PVkImageResolve);

 end;

 TvCmdBuffer=class(TvCustomCmdBuffer)

  Femulate_primtype:Integer;
  FinstanceCount:DWORD;
  FINDEX_TYPE:TVkIndexType;

  function    BeginRenderPass(RT:TvRenderTargets):Boolean;

  function    BindCompute(CP:TvComputePipeline2):Boolean;

  Procedure   BindSets(BindPoint:TVkPipelineBindPoint;F:TvDescriptorGroup);

  //Procedure   dmaData(src,dst:Pointer;byteCount:DWORD;isBlocking:Boolean);
  //Procedure   dmaData(src:DWORD;dst:Pointer;byteCount:DWORD;isBlocking:Boolean);
  //Procedure   writeAtEndOfShader(eventType:Byte;dst:Pointer;value:DWORD);

  Procedure   DrawIndexOffset2(Addr:Pointer;OFFSET,INDICES:DWORD);
  Procedure   DrawIndex2(Addr:Pointer;INDICES:DWORD);
  Procedure   DrawIndexAuto(INDICES:DWORD);
 end;

implementation

uses
 vBuffer,
 vHostBufferManager,
 kern_dmem;

function TvSemaphoreWaitCompare.c(a,b:TvSemaphoreWait):Integer;
begin
 Result:=Integer(Pointer(a.FSemaphore)>Pointer(b.FSemaphore))-Integer(Pointer(a.FSemaphore)<Pointer(b.FSemaphore));
end;

Constructor TvCustomCmdBuffer.Create(pool:TvCmdPool;Queue:TvQueue);
begin
 FParent:=pool;
 FQueue:=Queue;
 FCmdbuf:=pool.Alloc;
 if (FCmdbuf=VK_NULL_HANDLE) then Exit;

 FFence:=TvFence.Create(False);

 FSignalSemaphore:=TvSemaphore.Create;

 FCBState:=False;
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

Procedure TvCustomCmdBuffer.Acquire;
begin
 System.InterlockedIncrement(Pointer(FRefs));
end;

procedure TvCustomCmdBuffer.Release;
begin
 if System.InterlockedDecrement(Pointer(FRefs))=nil then
 begin
  Free;
 end;
end;

procedure TvCustomCmdBuffer.OnReleaseCmd(Sender:TObject);
begin
 Release;
end;

function TvCustomCmdBuffer.BeginCmdBuffer:Boolean;
var
 r:TVkResult;
 Info:TVkCommandBufferBeginInfo;
begin
 Result:=False;
 if (Self=nil) then Exit;
 if FCBState then Exit(True);
 if (FCmdbuf=VK_NULL_HANDLE) then Exit;
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
 FCBState:=True;
 Result:=True;
end;

Procedure TvCustomCmdBuffer.EndCmdBuffer;
var
 r:TVkResult;
begin
 if (Self=nil) then Exit;
 if FCBState then
 begin
  EndRenderPass;

  FCurrLayout[0]:=VK_NULL_HANDLE;
  FCurrLayout[1]:=VK_NULL_HANDLE;

  FCurrPipeline[0]:=VK_NULL_HANDLE;
  FCurrPipeline[1]:=VK_NULL_HANDLE;

  r:=vkEndCommandBuffer(FCmdbuf);
  if (r<>VK_SUCCESS) then
  begin
   Writeln(StdErr,'vkEndCommandBuffer:',r);
  end;
  FCBState:=False;
 end;
end;

Procedure TvCustomCmdBuffer.BindPipeline(BindPoint:TVkPipelineBindPoint;F:TVkPipeline);
begin
 if (Self=nil) then Exit;
 if (FCurrPipeline[ord(BindPoint)]=F) then Exit;

 if (not BeginCmdBuffer) then Exit;

 Inc(cmd_count);

 vkCmdBindPipeline(FCmdbuf,BindPoint,F);
 FCurrPipeline[ord(BindPoint)]:=F;
end;

function TvCmdBuffer.BeginRenderPass(RT:TvRenderTargets):Boolean;
var
 rinfo:TVkRenderPassBeginInfo;
 ainfo:TVkRenderPassAttachmentBeginInfo;
begin
 Result:=False;

 if (Self=nil) then Exit;

 if (RT=nil) then
 begin
  EndRenderPass;
  Exit(True);
 end;

 if (RT.FRenderPass=nil) then Exit;
 if (RT.FPipeline=nil) then Exit;
 if (RT.FFramebuffer=nil) then Exit;

 //if (not RT.FRenderPass.Compile) then Exit;
 //if (not RT.FPipeline.Compile) then Exit;
 //if (not RT.FFramebuffer.Compile) then Exit;

 if (RT.FRenderPass.FHandle=FRenderPass) then Exit(True);

 if (not BeginCmdBuffer) then Exit;

 EndRenderPass;

 rinfo:=RT.GetRInfo;

 if RT.FFramebuffer.IsImageless then
 begin
  ainfo:=RT.GetAInfo;
  rinfo.pNext:=@ainfo;
 end;

 FCurrPipeline[0]:=RT.FPipeline.FHandle;
 FCurrLayout  [0]:=RT.FPipeline.Key.FShaderGroup.FLayout.FHandle;
 Femulate_primtype:=RT.FPipeline.Key.emulate_primtype;

 Inc(cmd_count);

 vkCmdBeginRenderPass(FCmdbuf,@rinfo,VK_SUBPASS_CONTENTS_INLINE);
 vkCmdBindPipeline   (FCmdbuf,VK_PIPELINE_BIND_POINT_GRAPHICS,FCurrPipeline[0]);

 if AddDependence(@RT.Release) then
 begin
  RT.Acquire(Self);
 end;

 FRenderPass:=rinfo.renderPass;

 Result:=True;
end;

Function TvCustomCmdBuffer.IsRenderPass:Boolean;
begin
 Result:=False;
 if (Self=nil) then Exit;
 Result:=(FRenderPass<>VK_NULL_HANDLE);
end;

Procedure TvCustomCmdBuffer.EndRenderPass;
begin
 if (Self=nil) then Exit;
 if (FCmdbuf=VK_NULL_HANDLE) then Exit;
 if (FRenderPass<>VK_NULL_HANDLE) then
 begin
  Inc(cmd_count);
  vkCmdEndRenderPass(FCmdbuf);
  FRenderPass:=VK_NULL_HANDLE;
 end;
end;

function TvCmdBuffer.BindCompute(CP:TvComputePipeline2):Boolean;
begin
 Result:=False;

 if (Self=nil) or (CP=nil) then Exit;

 BindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,CP.FHandle);
 BindLayout  (VK_PIPELINE_BIND_POINT_COMPUTE,CP.FLayout);

 if AddDependence(@CP.Release) then
 begin
  CP.Acquire(Self);
 end;

 Result:=True;
end;

function TvCustomCmdBuffer.QueueSubmit:TVkResult;
var
 info:TVkSubmitInfo;

 FFenceHandle:TVkFence;

 FHandles:array of TVkSemaphore;
 FStages :array of TVkPipelineStageFlags;

 i:Integer;
 t:TvSemaphoreWaitSet.Iterator;
begin
 Result:=VK_ERROR_UNKNOWN;

 if (Self=nil) then Exit;
 if (FCmdbuf=VK_NULL_HANDLE) then Exit;

 EndCmdBuffer;

 info:=Default(TVkSubmitInfo);
 info.sType              :=VK_STRUCTURE_TYPE_SUBMIT_INFO;
 info.commandBufferCount :=1;
 info.pCommandBuffers    :=@FCmdbuf;

 if (FWaitSemaphores.Size<>0) then
 begin
  FHandles:=nil;
  FStages :=nil;
  SetLength(FHandles,FWaitSemaphores.Size);
  SetLength(FStages ,FWaitSemaphores.Size);

  i:=0;
  t:=FWaitSemaphores.cbegin;
  While (t.Item<>nil) do
  begin
   FHandles[i]:=t.Item^.FSemaphore.FHandle;
   FStages [i]:=t.Item^.FWaitStage;
   Inc(i);
   t.Next;
  end;

  info.waitSemaphoreCount:=i;
  info.pWaitSemaphores   :=@FHandles[0];
  info.pWaitDstStageMask  :=@FStages[0];

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

end;

function TvCustomCmdBuffer.Wait(timeout:TVkUInt64):TVkResult;
begin
 Result:=VK_ERROR_UNKNOWN;

 if (Self=nil) then Exit;
 if (FFence=nil) then Exit;

 Result:=FFence.Wait(timeout);
end;

Procedure TvCustomCmdBuffer.ReleaseResource;
begin
 if (Self=nil) then Exit;

 ReleaseAllDependencies(Self);

 FWaitSemaphores.Free;

 cmd_count:=0;
 ret:=0;
end;

Procedure TvCustomCmdBuffer.AddWaitSemaphore(S:TvSemaphore;W:TVkPipelineStageFlags);
Var
 I:TvSemaphoreWaitSet.Iterator;
 F:TvSemaphoreWait;
begin
 if (S=nil) then Exit;
 F:=Default(TvSemaphoreWait);
 F.FSemaphore:=S;
 F.FWaitStage:=W;

 I:=FWaitSemaphores.find(F);
 if (i.Item<>nil) then
 begin
  i.Item^.FWaitStage:=i.Item^.FWaitStage or W;
 end else
 begin
  FWaitSemaphores.Insert(F);
 end;
end;

function TvCustomCmdBuffer.GetSignaledSemaphore:TvSemaphore;
begin
 Result:=FSignalSemaphore;
end;

Procedure TvCustomCmdBuffer.BindLayout(BindPoint:TVkPipelineBindPoint;F:TvPipelineLayout);
begin
 if (Self=nil) then Exit;
 if (F=nil) then Exit;
 FCurrLayout[ord(BindPoint)]:=F.FHandle;
end;

Procedure TvCustomCmdBuffer.BindSet(BindPoint:TVkPipelineBindPoint;fset:TVkUInt32;FHandle:TVkDescriptorSet);
begin
 if (Self=nil) then Exit;
 if (FHandle=VK_NULL_HANDLE) then Exit;
 if (FCmdbuf=VK_NULL_HANDLE) then Exit;
 if (FCurrLayout[ord(BindPoint)]=VK_NULL_HANDLE) then Exit;

 Inc(cmd_count);

 vkCmdBindDescriptorSets(FCmdbuf,
                         BindPoint,
                         FCurrLayout[ord(BindPoint)],
                         fset,1,
                         @FHandle,
                         0,nil);

end;

Procedure TvCustomCmdBuffer.PushConstant(BindPoint:TVkPipelineBindPoint;stageFlags:TVkShaderStageFlags;offset,size:TVkUInt32;const pValues:PVkVoid);
begin
 if (Self=nil) then Exit;
 if (pValues=nil) or (size=0) then Exit;
 if (FCurrLayout[ord(BindPoint)]=VK_NULL_HANDLE) then Exit;

 if (not BeginCmdBuffer) then Exit;

 Inc(cmd_count);

 vkCmdPushConstants(FCmdbuf,
                    FCurrLayout[ord(BindPoint)],
                    stageFlags,
                    offset,size,
                    pValues);

end;

Procedure TvCustomCmdBuffer.DispatchDirect(X,Y,Z:TVkUInt32);
begin
 if (Self=nil) then Exit;
 if (FCurrPipeline[1]=VK_NULL_HANDLE) then Exit;

 if (not BeginCmdBuffer) then Exit;

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

 addr:QWORD;
begin
 c:=FAttrBuilder.FBindDescsCount;
 if (c=0) then Exit;

 if (Self=nil) then Exit;

 if (not BeginCmdBuffer) then Exit;

 last_binding:=0;
 last_size   :=0;
 For i:=0 to c-1 do
 With FAttrBuilder.FBindVBufs[i] do
 begin

  if not get_dmem_ptr(min_addr,@addr,nil) then
  begin
   Assert(false,'addr:0x'+HexStr(min_addr)+' not in dmem!');
  end;

  rb:=FetchHostBuffer(Self,addr,GetSize,ord(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT));

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
  Offsets[last_size]:=QWORD(addr)-rb.FAddr;

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
 if (Self=nil) then Exit;

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

Procedure TvCustomCmdBuffer.ClearDepthStencilImage(
                       image:TVkImage;
                       imageLayout:TVkImageLayout;
                       const pDepthStencil:PVkClearDepthStencilValue;
                       rangeCount:TVkUInt32;
                       const pRanges:PVkImageSubresourceRange);
begin
 if (Self=nil) then Exit;

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

Procedure TvCustomCmdBuffer.ClearColorImage(
                       image:TVkImage;
                       imageLayout:TVkImageLayout;
                       const pColor:PVkClearColorValue;
                       rangeCount:TVkUInt32;
                       const pRanges:PVkImageSubresourceRange);
begin
 if (Self=nil) then Exit;

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

Procedure TvCustomCmdBuffer.ResolveImage(
                       srcImage:TVkImage;
                       srcImageLayout:TVkImageLayout;
                       dstImage:TVkImage;
                       dstImageLayout:TVkImageLayout;
                       regionCount:TVkUInt32;
                       const pRegions:PVkImageResolve);
begin
 if (Self=nil) then Exit;

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

Procedure TvCmdBuffer.BindSets(BindPoint:TVkPipelineBindPoint;F:TvDescriptorGroup);
var
 i:Integer;
begin
 if (F=nil) then Exit;
 if (Self=nil) then Exit;
 if (FCmdbuf=VK_NULL_HANDLE) then Exit;
 if (FCurrLayout[ord(BindPoint)]=VK_NULL_HANDLE) then Exit;
 if (Length(F.FSets)=0) then Exit;
 For i:=0 to High(F.FSets) do
  if F.FSets[i].IsValid then
  begin

   Inc(cmd_count);

   vkCmdBindDescriptorSets(FCmdbuf,
                           BindPoint,
                           FCurrLayout[ord(BindPoint)],
                           i,1,
                           @F.FSets[i].FHandle,
                           0,nil);
  end;
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

{
Procedure TvCmdBuffer.dmaData(src,dst:Pointer;byteCount:DWORD;isBlocking:Boolean);
var
 srcb,dstb:TvHostBuffer;
 info:TVkBufferCopy;
begin
 if (Self=nil) then Exit;

 EndRenderPass;
 if (not BeginCmdBuffer) then Exit;

 srcb:=FetchHostBuffer(Self,src,byteCount,ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT));
 Assert(srcb<>nil);

 if (srcb.Foffset+byteCount>srcb.FSize) then
 begin
  Assert(False,'FetchHostBuffer:Insufficient buffer size!');
 end;

 dstb:=FetchHostBuffer(Self,dst,byteCount,ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT));
 Assert(dstb<>nil);

 if (dstb.Foffset+byteCount>dstb.FSize) then
 begin
  Assert(False,'FetchHostBuffer:Insufficient buffer size!');
 end;

 Inc(cmd_count);

 vkBufferMemoryBarrier(cmdbuf,
                       srcb.FHandle,
                       VK_ACCESS_ANY,
                       ord(VK_ACCESS_TRANSFER_READ_BIT),
                       srcb.Foffset,byteCount,
                       ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT),
                       ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 Inc(cmd_count);

 vkBufferMemoryBarrier(cmdbuf,
                       dstb.FHandle,
                       VK_ACCESS_ANY,
                       ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                       dstb.Foffset,byteCount,
                       ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT),
                       ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 info:=Default(TVkBufferCopy);
 info.srcOffset:=srcb.Foffset;
 info.dstOffset:=dstb.Foffset;
 info.size     :=byteCount;

 Inc(cmd_count);

 vkCmdCopyBuffer(cmdbuf,
                 srcb.FHandle,
                 dstb.FHandle,
                 1,@info);

 if isBlocking then
 begin
  Inc(cmd_count);
  vkBarrier(cmdbuf,
            ord(VK_PIPELINE_STAGE_TRANSFER_BIT),
            ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT));
 end;
end;

Procedure TvCmdBuffer.dmaData(src:DWORD;dst:Pointer;byteCount:DWORD;isBlocking:Boolean);
var
 dstb:TvHostBuffer;
begin
 if (Self=nil) then Exit;

 byteCount:=byteCount and (not 3); //4 byte align

 EndRenderPass;
 if (not BeginCmdBuffer) then Exit;

 dstb:=FetchHostBuffer(Self,dst,byteCount,ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT));
 Assert(dstb<>nil);

 Inc(cmd_count);

 vkBufferMemoryBarrier(cmdbuf,
                       dstb.FHandle,
                       VK_ACCESS_ANY,
                       ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                       dstb.Foffset,byteCount,
                       ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT),
                       ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 Inc(cmd_count);

 vkCmdFillBuffer(cmdbuf,
                 dstb.FHandle,
                 dstb.Foffset,
                 byteCount,src);

 if isBlocking then
 begin
  Inc(cmd_count);
  vkBarrier(cmdbuf,
            ord(VK_PIPELINE_STAGE_TRANSFER_BIT),
            ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT));
 end;
end;

Procedure TvCmdBuffer.writeAtEndOfShader(eventType:Byte;dst:Pointer;value:DWORD);
var
 rb:TvHostBuffer;
begin
 if (Self=nil) then Exit;

 EndRenderPass;
 if (not BeginCmdBuffer) then Exit;

 Case eventType of
  CS_DONE:
   begin
    Inc(cmd_count);
    vkBarrier(cmdbuf,
    	      ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
    	      ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

    rb:=FetchHostBuffer(Self,dst,4,ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT));
    Assert(rb<>nil);

    Inc(cmd_count);

    vkCmdFillBuffer(cmdbuf,
                    rb.FHandle,
                    rb.Foffset,
                    4,value);
   end;
  PS_DONE:
   begin
    Inc(cmd_count);
    vkBarrier(cmdbuf,
    	      ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
    	      ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

    rb:=FetchHostBuffer(Self,dst,4,ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT));
    Assert(rb<>nil);

    Inc(cmd_count);

    vkCmdFillBuffer(cmdbuf,
                    rb.FHandle,
                    rb.Foffset,
                    4,value);
   end;
  else
   Assert(False);
 end;
end;
}

function GET_INDEX_TYPE_SIZE(INDEX_TYPE:TVkIndexType):Byte;
begin
 Case INDEX_TYPE of
  VK_INDEX_TYPE_UINT16   :Result:=16;
  VK_INDEX_TYPE_UINT32   :Result:=32;
  VK_INDEX_TYPE_UINT8_EXT:Result:=8;
  else                    Result:=0;
 end;
end;

Procedure TvCmdBuffer.DrawIndexOffset2(Addr:Pointer;OFFSET,INDICES:DWORD);
var
 rb:TvHostBuffer;
 Size:TVkDeviceSize;
 BufOffset:TVkDeviceSize;
 i,h:DWORD;
begin
 if (Self=nil) then Exit;
 if (FCmdbuf=VK_NULL_HANDLE) then Exit;
 if (FRenderPass=VK_NULL_HANDLE) then Exit;
 if (FCurrPipeline[0]=VK_NULL_HANDLE) then Exit;

 if (FinstanceCount=0) then FinstanceCount:=1;

 Size:=(OFFSET+INDICES)*GET_INDEX_TYPE_SIZE(FINDEX_TYPE);

 rb:=FetchHostBuffer(Self,QWORD(Addr),Size,ord(VK_BUFFER_USAGE_INDEX_BUFFER_BIT));
 Assert(rb<>nil);

 Inc(cmd_count);

 BufOffset:=QWORD(Addr)-rb.FAddr;

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
         INDICES,        //indexCount
         FinstanceCount, //instanceCount
         OFFSET,         //firstIndex
         0,              //vertexOffset
         0);             //firstInstance
    end;
  DI_PT_QUADLIST:
    begin
     Assert(FinstanceCount<=1,'instance DI_PT_QUADLIST');
     Assert(OFFSET=0,'OFFSET DI_PT_QUADLIST');
     h:=INDICES div 4;
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

Procedure TvCmdBuffer.DrawIndex2(Addr:Pointer;INDICES:DWORD);
begin
 DrawIndexOffset2(Addr,0,INDICES);
end;

Procedure TvCmdBuffer.DrawIndexAuto(INDICES:DWORD);
var
 i,h:DWORD;
begin
 if (Self=nil) then Exit;
 if (FCmdbuf=VK_NULL_HANDLE) then Exit;
 if (FRenderPass=VK_NULL_HANDLE) then Exit;
 if (FCurrPipeline[0]=VK_NULL_HANDLE) then Exit;

 if (FinstanceCount=0) then FinstanceCount:=1;

 Case Femulate_primtype of
  0:
    begin
     vkCmdDraw(
      FCmdbuf,
      INDICES,        //vertexCount
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

     h:=INDICES div 3;
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
     h:=INDICES div 4;
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


end.

