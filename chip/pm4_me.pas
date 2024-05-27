unit pm4_me;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 LFQueue,

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
 vShaderManager,
 vRegs2Vulkan,
 vCmdBuffer,
 vPipeline,
 vSetsPoolManager,
 vSampler,
 vSamplerManager,

 shader_dump,

 renderdoc,

 sys_event,
 time,
 md_time,
 kern_thr,
 md_sleep,
 bittype,
 pm4defs,
 pm4_stream,
 si_ci_vi_merged_offset,
 si_ci_vi_merged_enum,
 si_ci_vi_merged_registers,
 si_ci_vi_merged_groups;

type
 t_on_submit_flip_eop=function(submit_id:QWORD):Integer;

 p_pm4_me=^t_pm4_me;
 t_pm4_me=object
  //
  queue:TIntrusiveMPSCQueue; //p_pm4_stream
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
  procedure free_stream(node:p_pm4_stream); static;
 end;

var
 use_renderdoc_capture:Boolean=False;

implementation

uses
 kern_dmem;

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
begin
 queue.Create;
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
begin
 if (stream.First=nil) then Exit;
 //self alloc
 node:=stream.allocator.Alloc(SizeOf(t_pm4_stream));
 //
 node^:=stream;
 //
 stream:=Default(t_pm4_stream);
 //
 queue.Push(node);
 //
 start;
 //
 trigger;
end;

procedure t_pm4_me.free_stream(node:p_pm4_stream);
var
 tmp:t_pm4_stream;
begin
 tmp:=node^;
 tmp.Free;
end;

//

Function GetAlignWidth(format:TVkFormat;width:DWORD):DWORD;
var
 bpp,size,align_m:Ptruint;
begin
 size:=width;
 bpp:=getFormatSize(format);
 align_m:=(128 div bpp)-1;
 size:=(size+align_m) and (not align_m);
 Result:=size;
end;

Function GetLinearSize(image:TvImage2;align:Boolean):Ptruint;
var
 extend:TvExtent3D;
begin
 extend.width :=image.key.params.width;
 extend.height:=image.key.params.height;
 extend.depth :=image.key.params.depth;

 if align then
 begin
  extend.width:=GetAlignWidth(image.key.cformat,extend.width);
 end;

 if IsTexelFormat(image.key.cformat) then
 begin
  extend.width  :=(extend.width  +3) div 4;
  extend.height :=(extend.height +3) div 4;
 end;

 Result:=extend.width*
         extend.height*
         extend.depth*
         image.key.params.arrayLayers*
         getFormatSize(image.key.cformat);
end;

function AlignDw(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
begin
 Result:=addr-(addr mod alignment);
end;

var
 FCmdPool:TvCmdPool;

procedure pm4_DrawPrepare(var rt_info:t_pm4_rt_info;
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
 sm:TvSampler;

 buf:TvHostBuffer;

 diff  :TVkUInt32;
 align :TVkUInt32;

 FDescriptorGroup:TvDescriptorGroup;
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

 if rt_info.DB_ENABLE then
 begin
  RenderCmd.DB_INFO:=rt_info.DB_INFO;

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

   RenderCmd.AddClearColor(RenderCmd.RT_INFO[i].CLEAR_COLOR);

   ri:=FetchImage(CmdBuffer,
                  RenderCmd.RT_INFO[i].FImageInfo,
                  iu_attachment,
                  RenderCmd.RT_INFO[i].IMAGE_USAGE
                  );

   iv:=ri.FetchView(CmdBuffer,RenderCmd.RT_INFO[i].FImageView,iu_attachment);

   ri.PushBarrier(CmdBuffer,
                  ord(VK_ACCESS_TRANSFER_READ_BIT),
                  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                  ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

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
  RenderCmd.AddClearColor(RenderCmd.DB_INFO.CLEAR_VALUE);

  ri:=FetchImage(CmdBuffer,
                 RenderCmd.DB_INFO.FImageInfo,
                 iu_depth,
                 RenderCmd.DB_INFO.DEPTH_USAGE
                 );

  iv:=ri.FetchView(CmdBuffer,iu_depth);

  ri.PushBarrier(CmdBuffer,
                 ord(VK_ACCESS_TRANSFER_READ_BIT),
                 VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                 ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

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

 if (Length(FUniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FImages) do
  With FUniformBuilder.FImages[i] do
  begin

   ri:=FetchImage(CmdBuffer,
                  FImage,
                  iu_sampled,
                  TM_READ
                 );

   iv:=ri.FetchView(CmdBuffer,FView,iu_sampled);

   begin

    ri.PushBarrier(CmdBuffer,
                   ord(VK_ACCESS_SHADER_READ_BIT),
                   VK_IMAGE_LAYOUT_GENERAL,
                   ord(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                   ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) );
   end;

  end;
 end;
 ////////

 if not CmdBuffer.BeginRenderPass(RenderCmd) then
 begin
  Assert(false,'BeginRenderPass(FRenderCmd)');
 end;

 CmdBuffer.SetVertexInput   (FAttrBuilder);
 CmdBuffer.BindVertexBuffers(FAttrBuilder);

 FDescriptorGroup:=nil;

 //
 if (Length(FUniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FImages) do
  With FUniformBuilder.FImages[i] do
  begin

   ri:=FetchImage(CmdBuffer,
                  FImage,
                  iu_sampled,
                  TM_READ
                 );

   iv:=ri.FetchView(CmdBuffer,FView,iu_sampled);

   if (FDescriptorGroup=nil) then
   begin
    FDescriptorGroup:=FetchDescriptorGroup(CmdBuffer,rt_info.ShaderGroup.FLayout);
   end;

   FDescriptorGroup.FSets[fset].BindImg(bind,0,
                                        iv.FHandle,
                                        VK_IMAGE_LAYOUT_GENERAL);


  end;
 end;
 //

 //
 if (Length(FUniformBuilder.FSamplers)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FSamplers) do
  With FUniformBuilder.FSamplers[i] do
  begin
   sm:=FetchSampler(CmdBuffer,PS);

   if (FDescriptorGroup=nil) then
   begin
    FDescriptorGroup:=FetchDescriptorGroup(CmdBuffer,rt_info.ShaderGroup.FLayout);
   end;

   FDescriptorGroup.FSets[fset].BindSmp(bind,0,sm.FHandle);

  end;
 end;
 //

 //
 if (Length(FUniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FBuffers) do
  With FUniformBuilder.FBuffers[i] do
  begin

   if not get_dmem_ptr(addr,@addr,nil) then
   begin
    Assert(false,'addr:0x'+HexStr(addr)+' not in dmem!');
   end;

   buf:=FetchHostBuffer(CmdBuffer,QWORD(addr),size,ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT));

   diff:=QWORD(addr)-buf.FAddr;

   align:=diff-AlignDw(diff,limits.minStorageBufferOffsetAlignment);

   if (align<>offset) then
   begin
    Assert(false,'wrong buffer align '+IntToStr(align)+'<>'+IntToStr(offset));
   end;

   diff:=AlignDw(diff,limits.minStorageBufferOffsetAlignment);

   if (FDescriptorGroup=nil) then
   begin
    FDescriptorGroup:=FetchDescriptorGroup(CmdBuffer,rt_info.ShaderGroup.FLayout);
   end;

   FDescriptorGroup.FSets[fset].BindBuf(bind,0,
                                        VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                        buf.FHandle,
                                        diff,
                                        VK_WHOLE_SIZE);


  end;
 end;
 //

 if (FDescriptorGroup<>nil) then
 begin
  CmdBuffer.BindSets(VK_PIPELINE_BIND_POINT_GRAPHICS,FDescriptorGroup);
 end;

end;

procedure pm4_Writeback(CmdBuffer:TvCmdBuffer;
                        RenderCmd:TvRenderTargets);
var
 i:Integer;

 ri:TvImage2;

 buf:TvHostBuffer;
 addr,size,offset:Ptruint;

 BufferImageCopy:TVkBufferImageCopy;
begin
 //write back

 if (RenderCmd.RT_COUNT<>0) then
 For i:=0 to RenderCmd.RT_COUNT-1 do
  if (RenderCmd.RT_INFO[i].attachment<>VK_ATTACHMENT_UNUSED) then
  begin

   ri:=FetchImage(CmdBuffer,
                  RenderCmd.RT_INFO[i].FImageInfo,
                  iu_attachment,
                  RenderCmd.RT_INFO[i].IMAGE_USAGE
                  );

   ri.PushBarrier(CmdBuffer,
                  ord(VK_ACCESS_TRANSFER_READ_BIT),
                  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                  ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

   if not get_dmem_ptr(ri.key.Addr,@addr,nil) then
   begin
    Assert(false,'addr:0x'+HexStr(ri.key.Addr)+' not in dmem!');
   end;

   //Writeln('0x',HexStr(ri.key.Addr),'->0x',HexStr(addr,16));

   size:=GetLinearSize(ri,true);

   buf:=FetchHostBuffer(CmdBuffer,
                        addr,
                        size,
                        ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT));

   offset:=buf.FAddr-addr;

   vkBufferMemoryBarrier(CmdBuffer.FCmdbuf,
                         buf.FHandle,
                         ord(VK_ACCESS_MEMORY_READ_BIT),
                         ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                         offset,
                         size,
                         ord(VK_PIPELINE_STAGE_HOST_BIT),
                         ord(VK_PIPELINE_STAGE_TRANSFER_BIT)
                         );

   BufferImageCopy:=Default(TVkBufferImageCopy);

   BufferImageCopy.bufferOffset     :=offset;
   BufferImageCopy.bufferRowLength  :=0;
   BufferImageCopy.bufferImageHeight:=0;
   BufferImageCopy.imageSubresource :=ri.GetSubresLayer;
   BufferImageCopy.imageExtent.Create(ri.key.params.width,
                                      ri.key.params.height,
                                      ri.key.params.depth);

   if true {align} then
   begin
    BufferImageCopy.bufferRowLength:=GetAlignWidth(ri.key.cformat,ri.key.params.width);
   end;

   vkCmdCopyImageToBuffer(CmdBuffer.FCmdbuf,
                          ri.FHandle,
                          VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                          buf.FHandle,
                          1,
                          @BufferImageCopy);

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

 RenderCmd:=TvRenderTargets.Create;

 pm4_DrawPrepare(node^.rt_info,
                 CmdBuffer,
                 RenderCmd);

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
  else;
   Assert(false);
 end;

 /////////

 CmdBuffer.EndRenderPass;

 pm4_Writeback(CmdBuffer,RenderCmd);

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

procedure pm4_EventWriteEop(node:p_pm4_node_EventWriteEop;me:p_pm4_me);
var
 curr,diff:QWORD;
begin
 EndFrameCapture;

 curr:=md_rdtsc_unit;
 diff:=curr-me^.rel_time;

 if (node^.addr<>nil) then
 Case node^.dataSel of
  //
  EVENTWRITEEOP_DATA_SEL_DISCARD:;

   //32bit data
  EVENTWRITEEOP_DATA_SEL_SEND_DATA32:PDWORD(node^.addr)^:=node^.data;

   //64bit data
  EVENTWRITEEOP_DATA_SEL_SEND_DATA64:PQWORD(node^.addr)^:=node^.data;

    //system 100Mhz global clock. (relative time)
  EVENTWRITEEOP_DATA_SEL_SEND_GPU_CLOCK:PQWORD(node^.addr)^:=mul_div_u64(100*1000000,UNIT_PER_SEC,diff);

    //GPU 800Mhz clock.           (relative time)
  EVENTWRITEEOP_DATA_SEL_SEND_CP_PERFCOUNTER:PQWORD(node^.addr)^:=mul_div_u64(800*1000000,UNIT_PER_SEC,diff);

  else
   Assert(false,'pm4_EventWriteEop');
 end;

 if (node^.intSel<>0) then
 begin
  me^.knote_eventid($40,0,curr*NSEC_PER_UNIT,0); //(absolute time) (freq???)
 end;
end;

procedure pm4_SubmitFlipEop(node:p_pm4_node_SubmitFlipEop;me:p_pm4_me);
var
 curr:QWORD;
begin
 if (me^.on_submit_flip_eop<>nil) then
 begin
  me^.on_submit_flip_eop(node^.eop_value);
 end;

 curr:=md_rdtsc_unit;

 if (node^.intSel<>0) then
 begin
  me^.knote_eventid($40,0,curr*NSEC_PER_UNIT,0); //(absolute time) (freq???)
 end;
end;

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl;
var
 stream:p_pm4_stream;
 node:p_pm4_node;
begin

 if use_renderdoc_capture then
 begin
  renderdoc.LoadRenderDoc;
  renderdoc.UnloadCrashHandler;
 end;

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
   //
   node:=stream^.First;
   while (node<>nil) do
   begin
    Writeln('+',node^.ntype);

    case node^.ntype of
     ntDrawIndex2   :pm4_Draw         (Pointer(node));
     ntDrawIndexAuto:pm4_Draw         (Pointer(node));
     ntEventWriteEop:pm4_EventWriteEop(Pointer(node),me);
     ntSubmitFlipEop:pm4_SubmitFlipEop(Pointer(node),me);
     else
    end;

    //
    node:=stream^.Next(node);
   end;

   me^.free_stream(stream);
   //
   Continue;
  end;

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

