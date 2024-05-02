unit pm4_me;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
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

 shader_dump,

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
 p_pm4_me=^t_pm4_me;
 t_pm4_me=object
  //
  queue:TIntrusiveMPSCQueue; //p_pm4_stream
  //
  started:Pointer;
  td:p_kthread;
  //
  procedure Init;
  procedure start;
  procedure Push(var stream:t_pm4_stream);
  procedure free_stream(node:p_pm4_stream); static;
 end;

implementation

uses
 kern_dmem;

procedure t_pm4_me.Init;
begin
 queue.Create;
end;

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl; forward;

procedure t_pm4_me.start;
begin
 if (XCHG(started,Pointer(1))=nil) then
 begin
  kthread_add(@pm4_me_thread,@self,@td,(8*1024*1024) div (16*1024),'[GFX_ME]');
 end;
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

var
 FCmdPool:TvCmdPool;

procedure pm4_DrawIndex2(node:p_pm4_node_DrawIndex2);
var
 i:Integer;

 GPU_REGS:TGPU_REGS;

 RT_COUNT:Byte;
 RT_INFO:array[0..7] of TRT_INFO;
 DB_INFO:TDB_INFO;

 FVSShader:TvShaderExt;
 FPSShader:TvShaderExt;

 FShadersKey :TvShadersKey;
 FShaderGroup:TvShaderGroup;

 FAttrBuilder:TvAttrBuilder;

 RP_KEY:TvRenderPassKey;
 RP:TvRenderPass2;

 BI:TBLEND_INFO;

 GP_KEY:TvGraphicsPipelineKey;
 GP:TvGraphicsPipeline2;

 FB_KEY:TvFramebufferImagelessKey;
 FB_KEY2:TvFramebufferBindedKey;
 FFramebuffer:TvFramebuffer;

 FRenderCmd:TvRenderTargets;

 ri:TvImage2;
 iv:TvImageView2;

 CmdBuffer:TvCmdBuffer;
 r:TVkResult;

 buf:TvHostBuffer;
 addr,size,offset:Ptruint;

 BufferImageCopy:TVkBufferImageCopy;
begin
 GPU_REGS:=Default(TGPU_REGS);
 GPU_REGS.SH_REG:=@node^.SH_REG;
 GPU_REGS.CX_REG:=@node^.CX_REG;
 GPU_REGS.UC_REG:=@node^.UC_REG;

 {fdump_ps:=}DumpPS(GPU_REGS);
 {fdump_vs:=}DumpVS(GPU_REGS);

 FPSShader:=FetchShader(vShaderStagePs,0,GPU_REGS,nil{@pa});
 FVSShader:=FetchShader(vShaderStageVs,1,GPU_REGS,nil{@pa});

 FShadersKey:=Default(TvShadersKey);
 FShadersKey.SetVSShader(FVSShader);
 FShadersKey.SetPSShader(FPSShader);

 FShaderGroup:=FetchShaderGroup(@FShadersKey);
 Assert(FShaderGroup<>nil);

 RP_KEY.Clear;

 RT_COUNT:=0;

 if GPU_REGS.COMP_ENABLE then
 For i:=0 to GPU_REGS.GET_HI_RT do
  begin
   RT_INFO[RT_COUNT]:=GPU_REGS.GET_RT_INFO(i);

   //hack
   //RT_INFO[RT_COUNT].IMAGE_USAGE:=TM_CLEAR or TM_WRITE;
   //

   RP_KEY.AddColorAt(RT_INFO[RT_COUNT].attachment,
                     RT_INFO[RT_COUNT].FImageInfo.cformat,
                     RT_INFO[RT_COUNT].IMAGE_USAGE,
                     RT_INFO[RT_COUNT].FImageInfo.params.samples);

   Inc(RT_COUNT);
  end;

 if GPU_REGS.DB_ENABLE then
 begin
  DB_INFO:=GPU_REGS.GET_DB_INFO;

  RP_KEY.AddDepthAt(RT_COUNT, //add to last attachment id
                    DB_INFO.FImageInfo.cformat,
                    DB_INFO.DEPTH_USAGE,
                    DB_INFO.STENCIL_USAGE);

  RP_KEY.SetZorderStage(DB_INFO.zorder_stage);

 end;

 //
 if (FCmdPool=nil) then
 begin
  FCmdPool:=TvCmdPool.Create(VulkanApp.FGFamily);
 end;

 CmdBuffer:=TvCmdBuffer.Create(FCmdPool,RenderQueue);
 //CmdBuffer.submit_id:=submit_id;

 //


 RP:=FetchRenderPass(CmdBuffer,@RP_KEY);

 BI:=GPU_REGS.GET_BLEND_INFO;

 GP_KEY.Clear;

 GP_KEY.FRenderPass :=RP;
 GP_KEY.FShaderGroup:=FShaderGroup;

 GP_KEY.SetBlendInfo(BI.logicOp,@BI.blendConstants);

 GP_KEY.SetPrimType (GPU_REGS.GET_PRIM_TYPE);
 GP_KEY.SetPrimReset(GPU_REGS.GET_PRIM_RESET);

 For i:=0 to 15 do
  if GPU_REGS.VP_ENABLE(i) then
  begin
   GP_KEY.AddVPort(GPU_REGS.GET_VPORT(i),GPU_REGS.GET_SCISSOR(i));
  end;

 if (RT_COUNT<>0) then
 For i:=0 to RT_COUNT-1 do
  begin
   GP_KEY.AddBlend(RT_INFO[i].blend);
  end;

 FAttrBuilder:=Default(TvAttrBuilder);

 if (FVSShader<>nil) then
 begin
  FVSShader.EnumVertLayout(@FAttrBuilder.AddAttr,FVSShader.FDescSetId,GPU_REGS.get_user_data(vShaderStageVs));
 end;

 if not limits.VK_EXT_vertex_input_dynamic_state then
 begin
  GP_KEY.SetVertexInput(FAttrBuilder);
 end;

 GP_KEY.rasterizer   :=GPU_REGS.GET_RASTERIZATION;
 GP_KEY.multisampling:=GPU_REGS.GET_MULTISAMPLE;

 GP_KEY.SetProvoking(GPU_REGS.GET_PROVOKING);

 if GPU_REGS.DB_ENABLE then
 begin
  GP_KEY.DepthStencil:=DB_INFO.ds_state;
 end;

 GP:=FetchGraphicsPipeline(CmdBuffer,@GP_KEY);

 if limits.VK_KHR_imageless_framebuffer then
 begin
  FB_KEY:=Default(TvFramebufferImagelessKey);

  FB_KEY.SetRenderPass(RP);
  FB_KEY.SetSize(GPU_REGS.GET_SCREEN_SIZE);

  if (RT_COUNT<>0) then
  For i:=0 to RT_COUNT-1 do
   begin
    FB_KEY.AddImageAt(RT_INFO[i].FImageInfo);
   end;

  if GPU_REGS.DB_ENABLE then
  begin
   FB_KEY.AddImageAt(DB_INFO.FImageInfo);
  end;
 end else
 begin
  FB_KEY2:=Default(TvFramebufferBindedKey);

  FB_KEY2.SetRenderPass(RP);
  FB_KEY2.SetSize(GPU_REGS.GET_SCREEN_SIZE);
 end;

 FRenderCmd:=TvRenderTargets.Create;

 FRenderCmd.FRenderPass:=RP;
 FRenderCmd.FPipeline  :=GP;

 FRenderCmd.FRenderArea:=GPU_REGS.GET_SCREEN;

 if limits.VK_KHR_imageless_framebuffer then
 begin
  FFramebuffer:=FetchFramebufferImageless(CmdBuffer,@FB_KEY);
  FRenderCmd.FFramebuffer:=FFramebuffer;
 end;

 if (RT_COUNT<>0) then
 For i:=0 to RT_COUNT-1 do
  begin

   FRenderCmd.AddClearColor(RT_INFO[i].CLEAR_COLOR);

   ri:=FetchImage(CmdBuffer,
                  RT_INFO[i].FImageInfo,
                  iu_attachment,
                  RT_INFO[i].IMAGE_USAGE
                  );

   iv:=ri.FetchView(CmdBuffer,RT_INFO[i].FImageView,iu_attachment);

   if limits.VK_KHR_imageless_framebuffer then
   begin
    FRenderCmd.AddImageView(iv);
   end;

   ri.PushBarrier(CmdBuffer,
                  ord(VK_ACCESS_TRANSFER_READ_BIT),
                  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                  ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

   ri.PushBarrier(CmdBuffer,
                  GetColorAccessMask(RT_INFO[i].IMAGE_USAGE),
                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL {VK_IMAGE_LAYOUT_GENERAL},
                  ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                  ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) );


   if not limits.VK_KHR_imageless_framebuffer then
   begin
    FB_KEY2.AddImageView(iv);
   end;

  end;

 if not limits.VK_KHR_imageless_framebuffer then
 begin
  FFramebuffer:=FetchFramebufferBinded(CmdBuffer,@FB_KEY2);
  FRenderCmd.FFramebuffer:=FFramebuffer;
 end;

 if GPU_REGS.DB_ENABLE then
 begin
  FRenderCmd.AddClearColor(DB_INFO.CLEAR_VALUE);
  //FRenderCmd.AddImageView(iv);
 end;

 if not CmdBuffer.BeginRenderPass(FRenderCmd) then
 begin
  Assert(false,'BeginRenderPass(FRenderCmd)');
 end;

 CmdBuffer.SetVertexInput(FAttrBuilder);

 /////////
 CmdBuffer.instanceCount:=GPU_REGS.UC_REG^.VGT_NUM_INSTANCES;
 CmdBuffer.DrawIndex2(node^.addr,
                      GPU_REGS.UC_REG^.VGT_NUM_INDICES,
                      GPU_REGS.GET_INDEX_TYPE);
 /////////

 CmdBuffer.EndRenderPass;

 //write back

 if (RT_COUNT<>0) then
 For i:=0 to RT_COUNT-1 do
  if (RT_INFO[i].attachment<>VK_ATTACHMENT_UNUSED) then
  begin

   ri:=FetchImage(CmdBuffer,
                  RT_INFO[i].FImageInfo,
                  iu_attachment,
                  RT_INFO[i].IMAGE_USAGE
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

procedure pm4_EventWriteEop(node:p_pm4_node_EventWriteEop);
begin

 Case node^.dataSel of
  //
  EVENTWRITEEOP_DATA_SEL_DISCARD            :;
  EVENTWRITEEOP_DATA_SEL_SEND_DATA32        :PDWORD(node^.addr)^:=node^.data;
  EVENTWRITEEOP_DATA_SEL_SEND_DATA64        :PQWORD(node^.addr)^:=node^.data;
  EVENTWRITEEOP_DATA_SEL_SEND_GPU_CLOCK     :; //system 100Mhz global clock.
  EVENTWRITEEOP_DATA_SEL_SEND_CP_PERFCOUNTER:; //GPU 800Mhz clock.
  else;
 end;

 //node^.intSel
end;

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl;
var
 stream:p_pm4_stream;
 node:p_pm4_node;
begin

 repeat

  stream:=nil;
  if me^.queue.Pop(stream) then
  begin
   //
   node:=stream^.First;
   while (node<>nil) do
   begin
    Writeln('+',node^.ntype);

    case node^.ntype of
     ntDrawIndex2   :pm4_DrawIndex2   (Pointer(node));
     ntEventWriteEop:pm4_EventWriteEop(Pointer(node));
     else
    end;

    //
    node:=stream^.Next(node);
   end;

   me^.free_stream(stream);
  end;

   //

  msleep_td(100);
 until false;

end;

end.

