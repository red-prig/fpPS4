unit pm4_me;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 LFQueue,

 vm_tracking_map,

 Vulkan,
 vDevice,
 vBuffer,
 vHostBufferManager,
 vImage,
 vImageManager,
 vRender,
 vRenderPassManager,
 vPipelineManager,
 vFramebufferManager,
 vShader,
 vShaderExt,
 vRegs2Vulkan,
 vCmdBuffer,
 vPipeline,
 vSetsPoolManager,
 vSampler,
 vSamplerManager,

 vImageTiling,

 renderdoc,

 sys_event,
 time,
 md_time,
 kern_thr,
 pm4defs,
 pm4_stream;

type
 t_on_submit_flip_eop=function(submit_id:QWORD):Integer;

 p_pm4_me=^t_pm4_me;
 t_pm4_me=object
  //
  queue:TIntrusiveMPSCQueue; //p_pm4_stream
  //
  stall:array[t_pm4_stream_type] of TAILQ_HEAD; //p_pm4_stream
  //
  event:PRTLEvent;
  on_idle:TProcedure;
  on_submit_flip_eop:t_on_submit_flip_eop;
  //
  started:Pointer;
  td:p_kthread;
  //
  gc_knlist:p_knlist;
  //
  rel_time:QWORD;
  //
  procedure Init(knlist:p_knlist);
  procedure start;
  procedure trigger;
  procedure knote_eventid(event_id,me_id:Byte;timestamp:QWORD;lockflags:Integer);
  procedure Push(var stream:t_pm4_stream);
  procedure add_stream (stream:p_pm4_stream;var iter:t_pm4_stream_type);
  function  get_next   (iter:t_pm4_stream_type):p_pm4_stream;
  procedure free_stream(stream:p_pm4_stream);
 end;

var
 use_renderdoc_capture:Boolean=False;

implementation

uses
 kern_proc,
 vm_map;

procedure StartFrameCapture;
begin
 if use_renderdoc_capture then
 begin
  if (renderdoc.IsFrameCapturing()=0) then
  begin
   renderdoc.StartFrameCapture(0,0);
  end;
 end;
end;

procedure EndFrameCapture;
begin
 if use_renderdoc_capture then
 begin
  if (renderdoc.IsFrameCapturing()<>0) then
  begin
   renderdoc.EndFrameCapture(0,0);
  end;
 end;
end;

procedure t_pm4_me.Init(knlist:p_knlist);
var
 i:t_pm4_stream_type;
begin
 queue.Create;

 for i:=Low(t_pm4_stream_type) to High(t_pm4_stream_type) do
 begin
  TAILQ_INIT(@stall[i]);
 end;

 gc_knlist:=knlist;
end;

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl; forward;

procedure t_pm4_me.start;
begin
 if (XCHG(started,Pointer(1))=nil) then
 begin
  event:=RTLEventCreate;
  //
  kthread_add(@pm4_me_thread,@self,@td,(8*1024*1024) div (16*1024),'[GFX_ME]');
 end;
end;

procedure t_pm4_me.trigger;
begin
 if (event<>nil) then
 begin
  RTLEventSetEvent(event);
 end;
end;

procedure t_pm4_me.knote_eventid(event_id,me_id:Byte;timestamp:QWORD;lockflags:Integer);
begin
 knote(gc_knlist, event_id or (me_id shl 8) or (timestamp shl 16), lockflags);
end;

procedure t_pm4_me.Push(var stream:t_pm4_stream);
var
 node:p_pm4_stream;
 buft:t_pm4_stream_type;
begin
 if (stream.First=nil) then Exit;
 //self alloc
 node:=stream.allocator.Alloc(SizeOf(t_pm4_stream));
 //
 node^:=stream;
 //
 buft:=stream.buft;
 stream:=Default(t_pm4_stream);
 stream.buft:=buft;
 //
 queue.Push(node);
 //
 start;
 //
 trigger;
end;

procedure t_pm4_me.add_stream(stream:p_pm4_stream;var iter:t_pm4_stream_type);
var
 i:t_pm4_stream_type;
begin
 i:=stream^.buft;
 TAILQ_INSERT_TAIL(@stall[i],stream,@stream^.next_);

 //if first
 if (stream=TAILQ_FIRST(@stall[i])) then
 if (iter>i) then
 begin
  iter:=i;
 end;
end;

function t_pm4_me.get_next(iter:t_pm4_stream_type):p_pm4_stream;
var
 i:t_pm4_stream_type;
begin
 for i:=iter to t_pm4_stream_type(ord(iter)+ord(High(t_pm4_stream_type))) do
 begin
  Result:=TAILQ_FIRST(@stall[t_pm4_stream_type(ord(i) mod Succ(ord(High(t_pm4_stream_type))))]);
  if (Result<>nil) then Break;
 end;
end;

procedure t_pm4_me.free_stream(stream:p_pm4_stream);
var
 tmp:t_pm4_stream;
 i:t_pm4_stream_type;
begin
 //pop
 i:=stream^.buft;
 TAILQ_REMOVE(@stall[i],stream,@stream^.next_);
 //
 tmp:=stream^;
 tmp.Free;
end;

//

var
 FCmdPool:TvCmdPool;

procedure Prepare_Uniforms(node:p_pm4_node;
                           var FUniformBuilder:TvUniformBuilder;
                           CmdBuffer:TvCmdBuffer);
var
 i:Integer;

 ri:TvImage2;
begin
 if (Length(FUniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FImages) do
  With FUniformBuilder.FImages[i] do
  begin

   ri:=FetchImage(CmdBuffer,
                  FImage,
                  iu_sampled
                  //TM_READ
                 );

   pm4_load_from(CmdBuffer,ri,TM_READ);

   begin

    ri.PushBarrier(CmdBuffer,
                   ord(VK_ACCESS_SHADER_READ_BIT),
                   VK_IMAGE_LAYOUT_GENERAL,
                   ord(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                   ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) );
   end;

  end;
 end;
end;

function AlignDw(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
begin
 Result:=addr-(addr mod alignment);
end;

procedure Bind_Uniforms(node:p_pm4_node;
                        var FUniformBuilder:TvUniformBuilder;
                        var FDescriptorGroup:TvDescriptorGroup;
                        ShaderGroup:TvShaderGroup;
                        CmdBuffer:TvCmdBuffer);

 procedure _init; inline;
 begin
  if (FDescriptorGroup=nil) then
  begin
   FDescriptorGroup:=FetchDescriptorGroup(CmdBuffer,ShaderGroup.FLayout);
  end;
 end;

var
 i:Integer;

 ri:TvImage2;
 iv:TvImageView2;
 sm:TvSampler;

 buf:TvHostBuffer;

 diff :TVkDeviceSize;
 align:TVkDeviceSize;
 range:TVkDeviceSize;

 resource_instance:p_pm4_resource_instance;
begin

 //images
 if (Length(FUniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FImages) do
  With FUniformBuilder.FImages[i] do
  begin

   resource_instance:=node^.scope.find_curr_image_resource(FImage);

   if (resource_instance<>nil) then
   begin
    Writeln('ri:curr:',HexStr(resource_instance^.curr_mem_usage,1),
              ' prev:',HexStr(resource_instance^.prev_mem_usage,1),
              ' next:',HexStr(resource_instance^.next_mem_usage,1)
           );
   end;

   ri:=FetchImage(CmdBuffer,
                  FImage,
                  iu_sampled
                  //TM_READ
                 );

   iv:=ri.FetchView(CmdBuffer,FView,iu_sampled);

   _init;

   FDescriptorGroup.FSets[fset].BindImg(bind,0,
                                        iv.FHandle,
                                        VK_IMAGE_LAYOUT_GENERAL);


  end;
 end;
 //images

 //samplers
 if (Length(FUniformBuilder.FSamplers)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FSamplers) do
  With FUniformBuilder.FSamplers[i] do
  begin
   sm:=FetchSampler(CmdBuffer,PS);

   _init;

   FDescriptorGroup.FSets[fset].BindSmp(bind,0,sm.FHandle);

  end;
 end;
 //samplers

 //buffers
 if (Length(FUniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FBuffers) do
  With FUniformBuilder.FBuffers[i] do
  begin

   resource_instance:=node^.scope.find_curr_buffer_resource(addr,size);

   if (resource_instance<>nil) then
   begin
    if (resource_instance^.prev_mem_usage<>0) then
    begin
     writeln;
    end;

    Writeln('rb:curr:',HexStr(resource_instance^.curr_mem_usage,1),
              ' prev:',HexStr(resource_instance^.prev_mem_usage,1),
              ' next:',HexStr(resource_instance^.next_mem_usage,1)
           );
   end;

   buf:=FetchHostBuffer(CmdBuffer,QWORD(addr),size,ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT));

   diff:=QWORD(addr)-buf.FAddr;

   align:=diff-AlignDw(diff,limits.minStorageBufferOffsetAlignment);

   if (align<>offset) then
   begin
    Assert(false,'wrong buffer align '+IntToStr(align)+'<>'+IntToStr(offset));
   end;

   diff:=AlignDw(diff,limits.minStorageBufferOffsetAlignment);

   range:=size;

   _init;

   FDescriptorGroup.FSets[fset].BindBuf(bind,0,
                                        VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                        buf.FHandle,
                                        diff,
                                        range {VK_WHOLE_SIZE});

   //TODO: check write flag
   vm_map_track_trigger(p_proc.p_vmspace,QWORD(addr),QWORD(addr)+size,nil);

  end;
 end;
 //buffers

end;

procedure pm4_ClearDepth(var rt_info:t_pm4_rt_info;
                         CmdBuffer:TvCmdBuffer);
var
 ri:TvImage2;
 cclear:array[0..1] of Boolean;
 range :TVkImageSubresourceRange;
begin
 //ClearDepthTarget

 ri:=FetchImage(CmdBuffer,
                rt_info.DB_INFO.FImageInfo,
                iu_depthstenc
                //rt_info.DB_INFO.DEPTH_USAGE
                );
 {
 ri.PushBarrier(CmdBuffer,
                ord(VK_ACCESS_TRANSFER_READ_BIT),
                VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                ord(VK_PIPELINE_STAGE_TRANSFER_BIT));
 }

 ri.PushBarrier(CmdBuffer,
                ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 cclear[0]:=((rt_info.DB_INFO.DEPTH_USAGE   and TM_CLEAR)<>0) and
            (GetDepthOnlyFormat  (ri.key.cformat)<>VK_FORMAT_UNDEFINED);

 cclear[1]:=((rt_info.DB_INFO.STENCIL_USAGE and TM_CLEAR)<>0) and
            (GetStencilOnlyFormat(ri.key.cformat)<>VK_FORMAT_UNDEFINED);

 range:=ri.GetSubresRange;

 range.aspectMask:=(ord(VK_IMAGE_ASPECT_DEPTH_BIT  )*ord(cclear[0])) or
                   (ord(VK_IMAGE_ASPECT_STENCIL_BIT)*ord(cclear[1]));

 CmdBuffer.ClearDepthStencilImage(ri.FHandle,
                                  VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                  @rt_info.DB_INFO.CLEAR_VALUE.depthStencil,
                                  range);

end;

procedure pm4_DrawPrepare(node:p_pm4_node;
                          var rt_info:t_pm4_rt_info;
                          CmdBuffer:TvCmdBuffer;
                          RenderCmd:TvRenderTargets);
var
 i:Integer;

 FAttrBuilder:TvAttrBuilder;

 FUniformBuilder:TvUniformBuilder;

 RP_KEY:TvRenderPassKey;
 RP:TvRenderPass2;

 GP_KEY:TvGraphicsPipelineKey;
 GP:TvGraphicsPipeline2;

 FB_KEY:TvFramebufferImagelessKey;
 FB_KEY2:TvFramebufferBindedKey;
 FFramebuffer:TvFramebuffer;

 ri:TvImage2;
 iv:TvImageView2;

 FDescriptorGroup:TvDescriptorGroup;

 resource_instance:p_pm4_resource_instance;
begin
 RP_KEY.Clear;

 RenderCmd.RT_COUNT:=rt_info.RT_COUNT;

 if (rt_info.RT_COUNT<>0) then
 For i:=0 to rt_info.RT_COUNT-1 do
  begin
   RenderCmd.RT_INFO[i]:=rt_info.RT_INFO[i];

   RP_KEY.AddColorAt(RenderCmd.RT_INFO[i].attachment,
                     RenderCmd.RT_INFO[i].FImageInfo.cformat,
                     RenderCmd.RT_INFO[i].IMAGE_USAGE,
                     RenderCmd.RT_INFO[i].FImageInfo.params.samples);

  end;

 //rt_info.DB_ENABLE:=False;

 RenderCmd.DB_ENABLE:=rt_info.DB_ENABLE;

 if rt_info.DB_ENABLE then
 begin
  RenderCmd.DB_INFO:=rt_info.DB_INFO;

 // RenderCmd.DB_INFO.DEPTH_USAGE:=RenderCmd.DB_INFO.DEPTH_USAGE or TM_CLEAR;

  RP_KEY.AddDepthAt(RenderCmd.RT_COUNT, //add to last attachment id
                    RenderCmd.DB_INFO.FImageInfo.cformat,
                    RenderCmd.DB_INFO.DEPTH_USAGE,
                    RenderCmd.DB_INFO.STENCIL_USAGE);

  RP_KEY.SetZorderStage(RenderCmd.DB_INFO.zorder_stage);

 end;

 RP:=FetchRenderPass(CmdBuffer,@RP_KEY);

 GP_KEY.Clear;

 GP_KEY.FRenderPass :=RP;
 GP_KEY.FShaderGroup:=rt_info.ShaderGroup;

 GP_KEY.SetBlendInfo(rt_info.BLEND_INFO.logicOp,@rt_info.BLEND_INFO.blendConstants);

 GP_KEY.SetPrimType (TVkPrimitiveTopology(rt_info.PRIM_TYPE));
 GP_KEY.SetPrimReset(rt_info.PRIM_RESET);

 if (rt_info.VP_COUNT<>0) then
 For i:=0 to rt_info.VP_COUNT-1 do
  begin
   GP_KEY.AddVPort(rt_info.VPORT[i],rt_info.SCISSOR[i]);
  end;

 if (RenderCmd.RT_COUNT<>0) then
 For i:=0 to RenderCmd.RT_COUNT-1 do
  begin
   GP_KEY.AddBlend(RenderCmd.RT_INFO[i].blend);
  end;

 FAttrBuilder:=Default(TvAttrBuilder);
 rt_info.ShaderGroup.ExportAttrBuilder(FAttrBuilder,@rt_info.USERDATA);

 if not limits.VK_EXT_vertex_input_dynamic_state then
 begin
  GP_KEY.SetVertexInput(FAttrBuilder);
 end;

 GP_KEY.rasterizer   :=rt_info.RASTERIZATION;
 GP_KEY.multisampling:=rt_info.MULTISAMPLE;

 GP_KEY.SetProvoking(TVkProvokingVertexModeEXT(rt_info.PROVOKING));

 if rt_info.DB_ENABLE then
 begin
  GP_KEY.DepthStencil:=RenderCmd.DB_INFO.ds_state;
 end;

 GP:=FetchGraphicsPipeline(CmdBuffer,@GP_KEY);

 if limits.VK_KHR_imageless_framebuffer then
 begin
  FB_KEY:=Default(TvFramebufferImagelessKey);

  FB_KEY.SetRenderPass(RP);
  FB_KEY.SetSize(rt_info.SCREEN_SIZE);

  if (RenderCmd.RT_COUNT<>0) then
  For i:=0 to RenderCmd.RT_COUNT-1 do
   begin
    FB_KEY.AddImageAt(RenderCmd.RT_INFO[i].FImageInfo);
   end;

  if rt_info.DB_ENABLE then
  begin
   FB_KEY.AddImageAt(RenderCmd.DB_INFO.FImageInfo);
  end;
 end else
 begin
  FB_KEY2:=Default(TvFramebufferBindedKey);

  FB_KEY2.SetRenderPass(RP);
  FB_KEY2.SetSize(rt_info.SCREEN_SIZE);
 end;

 RenderCmd.FRenderPass:=RP;
 RenderCmd.FPipeline  :=GP;

 RenderCmd.FRenderArea:=rt_info.SCREEN_RECT;

 if limits.VK_KHR_imageless_framebuffer then
 begin
  FFramebuffer:=FetchFramebufferImageless(CmdBuffer,@FB_KEY);
  RenderCmd.FFramebuffer:=FFramebuffer;
 end;

 if (RenderCmd.RT_COUNT<>0) then
 For i:=0 to RenderCmd.RT_COUNT-1 do
  begin

   resource_instance:=node^.scope.find_curr_image_resource(RenderCmd.RT_INFO[i].FImageInfo);

   if (resource_instance<>nil) then
   begin
    Writeln('ra:curr:',HexStr(resource_instance^.curr_mem_usage,1),
              ' prev:',HexStr(resource_instance^.prev_mem_usage,1),
              ' next:',HexStr(resource_instance^.next_mem_usage,1)
           );
   end;

   RenderCmd.AddClearColor(RenderCmd.RT_INFO[i].CLEAR_COLOR);

   ri:=FetchImage(CmdBuffer,
                  RenderCmd.RT_INFO[i].FImageInfo,
                  iu_attachment
                  //RenderCmd.RT_INFO[i].IMAGE_USAGE
                  );

   pm4_load_from(CmdBuffer,ri,RenderCmd.RT_INFO[i].IMAGE_USAGE);

   iv:=ri.FetchView(CmdBuffer,RenderCmd.RT_INFO[i].FImageView,iu_attachment);

   {
   ri.PushBarrier(CmdBuffer,
                  ord(VK_ACCESS_TRANSFER_READ_BIT),
                  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                  ord(VK_PIPELINE_STAGE_TRANSFER_BIT));
   }

   ri.PushBarrier(CmdBuffer,
                  GetColorAccessMask(RenderCmd.RT_INFO[i].IMAGE_USAGE),
                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL {VK_IMAGE_LAYOUT_GENERAL},
                  ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                  ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) );

   //
   if limits.VK_KHR_imageless_framebuffer then
   begin
    RenderCmd.AddImageView(iv);
   end else
   begin
    FB_KEY2.AddImageView(iv);
   end;
   //

  end;

 if rt_info.DB_ENABLE then
 begin

  resource_instance:=node^.scope.find_curr_image_resource(GetDepthOnly(RenderCmd.DB_INFO.FImageInfo));

  if (resource_instance<>nil) then
  begin
   Writeln('rd:curr:',HexStr(resource_instance^.curr_mem_usage,1),
             ' prev:',HexStr(resource_instance^.prev_mem_usage,1),
             ' next:',HexStr(resource_instance^.next_mem_usage,1)
          );
  end;

  resource_instance:=node^.scope.find_curr_image_resource(GetStencilOnly(RenderCmd.DB_INFO.FImageInfo));

  if (resource_instance<>nil) then
  begin
   Writeln('rs:curr:',HexStr(resource_instance^.curr_mem_usage,1),
             ' prev:',HexStr(resource_instance^.prev_mem_usage,1),
             ' next:',HexStr(resource_instance^.next_mem_usage,1)
          );
  end;

  //

  RenderCmd.AddClearColor(RenderCmd.DB_INFO.CLEAR_VALUE);

  ri:=FetchImage(CmdBuffer,
                 RenderCmd.DB_INFO.FImageInfo,
                 iu_depthstenc
                 //RenderCmd.DB_INFO.DEPTH_USAGE
                 );

  pm4_load_from(CmdBuffer,ri.DepthOnly  ,RenderCmd.DB_INFO.DEPTH_USAGE);
  pm4_load_from(CmdBuffer,ri.StencilOnly,RenderCmd.DB_INFO.STENCIL_USAGE);

  iv:=ri.FetchView(CmdBuffer,iu_depthstenc);

  {
  ri.PushBarrier(CmdBuffer,
                 ord(VK_ACCESS_TRANSFER_READ_BIT),
                 VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                 ord(VK_PIPELINE_STAGE_TRANSFER_BIT));
  }

  ri.PushBarrier(CmdBuffer,
                 GetDepthStencilAccessMask(RenderCmd.DB_INFO.DEPTH_USAGE,RenderCmd.DB_INFO.STENCIL_USAGE),
                 GetDepthStencilSendLayout(RenderCmd.DB_INFO.DEPTH_USAGE,RenderCmd.DB_INFO.STENCIL_USAGE),
                 ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                 RenderCmd.DB_INFO.zorder_stage
                 );

  //
  if limits.VK_KHR_imageless_framebuffer then
  begin
   RenderCmd.AddImageView(iv);
  end else
  begin
   FB_KEY2.AddImageView(iv);
  end;
  //

 end;

 if not limits.VK_KHR_imageless_framebuffer then
 begin
  FFramebuffer:=FetchFramebufferBinded(CmdBuffer,@FB_KEY2);
  RenderCmd.FFramebuffer:=FFramebuffer;
 end;

 ////////
 FUniformBuilder:=Default(TvUniformBuilder);
 rt_info.ShaderGroup.ExportUnifBuilder(FUniformBuilder,@rt_info.USERDATA);

 Prepare_Uniforms(node,FUniformBuilder,CmdBuffer);
 ////////

 if not CmdBuffer.BeginRenderPass(RenderCmd) then
 begin
  Writeln(stderr,'BeginRenderPass(FRenderCmd)');
  Assert (false ,'BeginRenderPass(FRenderCmd)');
 end;

 CmdBuffer.SetVertexInput   (FAttrBuilder);
 CmdBuffer.BindVertexBuffers(FAttrBuilder);

 FDescriptorGroup:=nil;

 Bind_Uniforms(node,
               FUniformBuilder,
               FDescriptorGroup,
               rt_info.ShaderGroup,
               CmdBuffer);

 if (FDescriptorGroup<>nil) then
 begin
  CmdBuffer.BindSets(VK_PIPELINE_BIND_POINT_GRAPHICS,FDescriptorGroup);
 end;

end;

procedure pm4_Writeback(CmdBuffer:TvCmdBuffer;
                        var rt_info:t_pm4_rt_info);
var
 i:Integer;

 ri:TvImage2;
begin
 //write back

 if (rt_info.RT_COUNT<>0) then
 For i:=0 to rt_info.RT_COUNT-1 do
  if (rt_info.RT_INFO[i].attachment<>VK_ATTACHMENT_UNUSED) then
  begin

   ri:=FetchImage(CmdBuffer,
                  rt_info.RT_INFO[i].FImageInfo,
                  iu_attachment
                  //RenderCmd.RT_INFO[i].IMAGE_USAGE
                  );

   pm4_write_back(CmdBuffer,ri);
  end;

 if rt_info.DB_ENABLE then
 begin

  ri:=FetchImage(CmdBuffer,
                 rt_info.DB_INFO.FImageInfo,
                 iu_depthstenc
                 //RenderCmd.DB_INFO.DEPTH_USAGE
                 );

  pm4_write_back(CmdBuffer,ri.DepthOnly  );
  pm4_write_back(CmdBuffer,ri.StencilOnly);

  //
 end;

 //write back
end;

procedure pm4_Draw(node:p_pm4_node_draw);
var
 RenderCmd:TvRenderTargets;

 CmdBuffer:TvCmdBuffer;

 r:TVkResult;
begin

 StartFrameCapture;

 //
 if (FCmdPool=nil) then
 begin
  FCmdPool:=TvCmdPool.Create(VulkanApp.FGFamily);
 end;

 CmdBuffer:=TvCmdBuffer.Create(FCmdPool,RenderQueue);
 //CmdBuffer.submit_id:=submit_id;

 //

 if (node^.ntype<>ntClearDepth) then
 begin
  RenderCmd:=TvRenderTargets.Create;

  pm4_DrawPrepare(node,
                  node^.rt_info,
                  CmdBuffer,
                  RenderCmd);

 end;

 CmdBuffer.FinstanceCount:=node^.numInstances;
 CmdBuffer.FINDEX_TYPE   :=TVkIndexType(node^.INDEX_TYPE);

 case node^.ntype of
  ntDrawIndex2:
   begin
    CmdBuffer.DrawIndex2(Pointer(node^.indexBase),node^.indexCount);
   end;
  ntDrawIndexAuto:
   begin
    CmdBuffer.DrawIndexAuto(node^.indexCount);
   end;
  ntClearDepth:
   begin
    pm4_ClearDepth(node^.rt_info,CmdBuffer);
   end;
  else;
   Assert(false);
 end;

 /////////

 CmdBuffer.EndRenderPass;

 pm4_Writeback(CmdBuffer,node^.rt_info);

 r:=CmdBuffer.QueueSubmit;

 if (r<>VK_SUCCESS) then
 begin
  Assert(false,'QueueSubmit');
 end;

 Writeln('QueueSubmit:',r);

 r:=CmdBuffer.Wait(QWORD(-1));

 Writeln('CmdBuffer:',r);

 r:=RenderQueue.WaitIdle;
 Writeln('WaitIdle:',r);

 CmdBuffer.ReleaseResource;

 CmdBuffer.Free;
end;

procedure pm4_DispatchPrepare(node:p_pm4_node_DispatchDirect;
                              CmdBuffer:TvCmdBuffer);
var
 dst:PGPU_USERDATA;

 CP_KEY:TvComputePipelineKey;
 CP:TvComputePipeline2;

 FUniformBuilder:TvUniformBuilder;

 FDescriptorGroup:TvDescriptorGroup;
begin
 CP_KEY.FShaderGroup:=node^.ShaderGroup;
 CP:=FetchComputePipeline(CmdBuffer,@CP_KEY);

 ////////

 //hack
 dst:=Pointer(@node^.USER_DATA_CS)-Ptruint(@TGPU_USERDATA(nil^).A[vShaderStageCs]);

 FUniformBuilder:=Default(TvUniformBuilder);
 CP_KEY.FShaderGroup.ExportUnifBuilder(FUniformBuilder,dst);

 Prepare_Uniforms(node,FUniformBuilder,CmdBuffer);
 ////////

 if not CmdBuffer.BindCompute(CP) then
 begin
  Writeln(stderr,'BindCompute(CP)');
  Assert(false,'BindCompute(CP)');
 end;

 FDescriptorGroup:=nil;

 Bind_Uniforms(node,
               FUniformBuilder,
               FDescriptorGroup,
               CP_KEY.FShaderGroup,
               CmdBuffer);

 if (FDescriptorGroup<>nil) then
 begin
  CmdBuffer.BindSets(VK_PIPELINE_BIND_POINT_COMPUTE,FDescriptorGroup);
 end;

end;

procedure pm4_DispatchDirect(node:p_pm4_node_DispatchDirect);
var
 CmdBuffer:TvCmdBuffer;

 r:TVkResult;
begin

 StartFrameCapture;

 //
 if (FCmdPool=nil) then
 begin
  FCmdPool:=TvCmdPool.Create(VulkanApp.FGFamily);
 end;

 CmdBuffer:=TvCmdBuffer.Create(FCmdPool,RenderQueue);
 //CmdBuffer.submit_id:=submit_id;

 //
 CmdBuffer.EndRenderPass;

 pm4_DispatchPrepare(node,
                     CmdBuffer);

 CmdBuffer.DispatchDirect(node^.DIM_X,node^.DIM_Y,node^.DIM_Z);

 /////////

 r:=CmdBuffer.QueueSubmit;

 if (r<>VK_SUCCESS) then
 begin
  Assert(false,'QueueSubmit');
 end;

 Writeln('QueueSubmit:',r);

 r:=CmdBuffer.Wait(QWORD(-1));

 Writeln('CmdBuffer:',r);

 r:=RenderQueue.WaitIdle;
 Writeln('WaitIdle:',r);

 CmdBuffer.ReleaseResource;

 CmdBuffer.Free;
end;

function mul_div_u64(m,d,v:QWORD):QWORD; sysv_abi_default; assembler; nostackframe;
asm
 movq v,%rax
 mulq m
 divq d
end;

const
 GLOBAL_CLOCK_FREQUENCY  =100*1000*1000; //100MHz
 GPU_CORE_CLOCK_FREQUENCY=800*1000*1000; //800MHz

 //neo mode & ext_gpu_timer -> 911*000*000


procedure pm4_EventWriteEop(node:p_pm4_node_EventWriteEop;me:p_pm4_me);
var
 curr,diff:QWORD;
begin
 //EndFrameCapture;

 curr:=md_rdtsc_unit;
 diff:=curr-me^.rel_time;

 if (node^.addr<>nil) then
 Case node^.dataSel of
  //
  EVENTWRITEEOP_DATA_SEL_DISCARD:;

   //32bit data
  EVENTWRITEEOP_DATA_SEL_SEND_DATA32:
   begin
    PDWORD(node^.addr)^:=node^.data;
   end;

   //64bit data
  EVENTWRITEEOP_DATA_SEL_SEND_DATA64:
   begin
    PQWORD(node^.addr)^:=node^.data;
   end;

    //system 100Mhz global clock. (relative time)
  EVENTWRITEEOP_DATA_SEL_SEND_GPU_CLOCK:
   begin
    PQWORD(node^.addr)^:=mul_div_u64(GLOBAL_CLOCK_FREQUENCY,UNIT_PER_SEC,diff);
   end;

    //GPU 800Mhz clock.           (relative time)
  EVENTWRITEEOP_DATA_SEL_SEND_CP_PERFCOUNTER:
   begin
    PQWORD(node^.addr)^:=mul_div_u64(GPU_CORE_CLOCK_FREQUENCY,UNIT_PER_SEC,diff);
   end;

  else
   Assert(false,'pm4_EventWriteEop');
 end;

 if (node^.intSel=EVENTWRITEEOP_INT_SEL_SEND_INT) or
    (node^.intSel=EVENTWRITEEOP_INT_SEL_SEND_INT_ON_CONFIRM) then
 begin
  me^.knote_eventid($40,0,curr*NSEC_PER_UNIT,0); //(absolute time) (freq???)
 end;

 if (me^.on_idle<>nil) then
 begin
  me^.on_idle();
 end;
end;

procedure pm4_SubmitFlipEop(node:p_pm4_node_SubmitFlipEop;me:p_pm4_me);
var
 curr:QWORD;
begin
 //EndFrameCapture;

 if (me^.on_submit_flip_eop<>nil) then
 begin
  me^.on_submit_flip_eop(node^.eop_value);
 end;

 curr:=md_rdtsc_unit;

 if (node^.intSel=EVENTWRITEEOP_INT_SEL_SEND_INT) or
    (node^.intSel=EVENTWRITEEOP_INT_SEL_SEND_INT_ON_CONFIRM) then
 begin
  me^.knote_eventid($40,0,curr*NSEC_PER_UNIT,0); //(absolute time) (freq???)
 end;

 if (me^.on_idle<>nil) then
 begin
  me^.on_idle();
 end;
end;

procedure pm4_EventWriteEos(node:p_pm4_node_EventWriteEos;me:p_pm4_me);
begin

 if (node^.addr<>nil) then
 Case node^.command of

   //32bit data
  EVENT_WRITE_EOS_CMD_STORE_32BIT_DATA_TO_MEMORY:PDWORD(node^.addr)^:=node^.data;

  else
   Assert(false,'pm4_EventWriteEos');
 end;

 //interrupt???

 if (me^.on_idle<>nil) then
 begin
  me^.on_idle();
 end;
end;

procedure pm4_WriteData(node:p_pm4_node_WriteData);
var
 addr:PDWORD;
begin

 case node^.dstSel of
  WRITE_DATA_DST_SEL_MEMORY_SYNC,  //writeDataInline
  WRITE_DATA_DST_SEL_TCL2,         //writeDataInlineThroughL2
  WRITE_DATA_DST_SEL_MEMORY_ASYNC:
    if (node^.dst<>0) then
    begin
     addr:=Pointer(node^.dst);
     Move(node^.src^,addr^,node^.num_dw*SizeOf(DWORD));
    end;
  else
    Assert(false,'WriteData: dstSel=0x'+HexStr(node^.dstSel,1));
 end;

end;

Function me_test_mem(node:p_pm4_node_WaitRegMem):Boolean;
var
 val,ref:DWORD;
begin
 val:=PDWORD(node^.pollAddr)^ and node^.mask;
 ref:=node^.refValue;
 Case node^.compareFunc of
  WAIT_REG_MEM_FUNC_ALWAYS       :Result:=True;
  WAIT_REG_MEM_FUNC_LESS         :Result:=(val<ref);
  WAIT_REG_MEM_FUNC_LESS_EQUAL   :Result:=(val<=ref);
  WAIT_REG_MEM_FUNC_EQUAL        :Result:=(val=ref);
  WAIT_REG_MEM_FUNC_NOT_EQUAL    :Result:=(val<>ref);
  WAIT_REG_MEM_FUNC_GREATER_EQUAL:Result:=(val>ref);
  WAIT_REG_MEM_FUNC_GREATER      :Result:=(val>=ref);
  else
   Assert(false,'me_test_mem');
 end;
end;

function pm4_WaitRegMem(node:p_pm4_node_WaitRegMem):Boolean;
begin
 Result:=me_test_mem(node);

 {
 while not me_test_mem(node) do
 begin
  sleep(1);
 end;
 }

end;

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl;
var
 stream:p_pm4_stream;
 node:p_pm4_node;
 i,start:t_pm4_stream_type;
 switch:Boolean;
begin

 if use_renderdoc_capture then
 begin
  renderdoc.LoadRenderDoc;
  renderdoc.UnloadCrashHandler;
 end;

 //reset stall iterator
 start:=Low(t_pm4_stream_type);

 repeat

  //start relative timer
  if (me^.rel_time=0) then
  begin
   me^.rel_time:=md_rdtsc_unit;
  end;
  //

  stream:=nil;
  if me^.queue.Pop(stream) then
  begin
   me^.add_stream(stream,start);
   //
   stream:=nil;
  end;

  stream:=me^.get_next(start);

  if (stream<>nil) then
  begin
   switch:=False;
   //
   node:=stream^.First;
   while (node<>nil) do
   begin
    //Writeln('+',node^.ntype);

    case node^.ntype of
     ntDrawIndex2    :pm4_Draw          (Pointer(node));
     ntDrawIndexAuto :pm4_Draw          (Pointer(node));
     ntClearDepth    :pm4_Draw          (Pointer(node));
     ntDispatchDirect:pm4_DispatchDirect(Pointer(node));
     ntEventWriteEop :pm4_EventWriteEop (Pointer(node),me);
     ntSubmitFlipEop :pm4_SubmitFlipEop (Pointer(node),me);
     ntEventWriteEos :pm4_EventWriteEos (Pointer(node),me);
     ntWriteData     :pm4_WriteData     (Pointer(node));
     ntWaitRegMem:
      begin
       if pm4_WaitRegMem(Pointer(node)) then
       begin
        //
       end else
       begin
        switch:=True;
        Break;
       end;
      end;

     else
      begin
       Writeln('me:+',node^.ntype);
      end;
    end;

    //
    node:=stream^.Next(node);
   end;

   if switch then
   begin
    i:=stream^.buft;
    if (i=High(t_pm4_stream_type)) then
    begin
     //wait
     sleep(1);
     //reset stall iterator
     start:=Low(t_pm4_stream_type);
     //
     Continue;
    end else
    begin
     //next
     start:=Succ(i);
     //
     Continue;
    end;
   end else
   begin
    //reset stall iterator
    start:=Low(t_pm4_stream_type);
   end;

   me^.free_stream(stream);

   EndFrameCapture;

   //
   Continue;
  end;

  //stall is empty!

  //reset stall iterator
  start:=Low(t_pm4_stream_type);

  me^.rel_time:=0; //reset time
  //
  if (me^.on_idle<>nil) then
  begin
   me^.on_idle();
  end;
  //
  RTLEventWaitFor(me^.event);
 until false;

end;

end.

