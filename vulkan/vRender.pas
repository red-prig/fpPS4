unit vRender;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  RWLock,
  //sys_types,
  g23tree,
  //ps4_libSceVideoOut,
  si_ci_vi_merged_enum,
  Vulkan,
  vDevice,
  vMemory,
  vShader,
  vShaderExt,
  vPipeline,
  //vSetsPools,
  vImage;

type
 TvComputePipeline2=class(TvComputePipeline)
  FRefs:ptruint;
  Procedure  Acquire(Sender:TObject);
  Procedure  Release(Sender:TOBject);
 end;

 {
 TvRenderPass=class(vPipeline.TvRenderPass)

  AtCount:TVkUInt32;
  ColorAt:array[0..8] of TVkAttachmentDescription;

  ColorRef:array[0..7] of TVkAttachmentReference; //subpass.colorAttachmentCount

  pDepthStencilRef:TVkAttachmentReference;

  subpass:TVkSubpassDescription;
  dependency:TVkSubpassDependency;

  Procedure  Clear;
  Procedure  SetZorderStage(s:TVkPipelineStageFlags);
  Procedure  AddColorRef(id:TVkUInt32;IMAGE_USAGE:Byte);
  Procedure  SetDepthStencilRef(id:TVkUInt32;DEPTH_USAGE,STENCIL_USAGE:Byte);
  Procedure  AddColorAt(format:TVkFormat;IMAGE_USAGE:Byte;samples:TVkSampleCountFlagBits);
  Procedure  AddDepthAt(format:TVkFormat;DEPTH_USAGE,STENCIL_USAGE:Byte);
  Function   Compile:Boolean;
 end;
 }

 TvGraphicsPipeline=class(TvPipeline)
  FRenderPass:TvRenderPass;

  Viewports:array[0..15] of TVkViewport; //viewportState.viewportCount
  Scissors :array[0..15] of TVkRect2D;   //viewportState.scissorCount

  ColorBlends:array[0..7] of TVkPipelineColorBlendAttachmentState; //colorBlending.attachmentCount

  FShaderGroup:TvShaderGroup;

  dynamicStates:array[0..1] of TVkDynamicState; //dynamicState.dynamicStateCount

  vertexInputInfo:TVkPipelineVertexInputStateCreateInfo;
  inputAssembly:TVkPipelineInputAssemblyStateCreateInfo;
  viewportState:TVkPipelineViewportStateCreateInfo;
  rasterizer:TVkPipelineRasterizationStateCreateInfo;
  multisampling:TVkPipelineMultisampleStateCreateInfo;
  colorBlending:TVkPipelineColorBlendStateCreateInfo;
  DepthStencil:TVkPipelineDepthStencilStateCreateInfo;
  dynamicState:TVkPipelineDynamicStateCreateInfo;

  emulate_primtype:Integer;

  Procedure SetPrimType(t:TVkPrimitiveTopology);
  Procedure SetPrimReset(enable:TVkBool32);
  Procedure AddVPort(const V:TVkViewport;const S:TVkRect2D);
  Procedure AddBlend(const b:TVkPipelineColorBlendAttachmentState);
  Procedure Clear;
  procedure SetBlendColors(P:PSingle);
  procedure SetRenderPass(RenderPass:TvRenderPass);
  function  Compile:Boolean;
 end;

 TvRenderTargets=class
  FRenderPass:TvRenderPass;
  FPipeline:TvGraphicsPipeline;
  FFramebuffer:TvFramebuffer;
  FRenderArea:TVkRect2D;
  FClearValuesCount:TVkUInt32;
  FClearValues:array[0..8] of TVkClearValue;
  //
  FRefs:ptruint;
  //
  Procedure  AddClearColor(clr:TVkClearValue);
  Function   GetInfo:TVkRenderPassBeginInfo;
  class function c(const a,b:TvRenderTargets):Integer;
  Destructor Destroy; override;
  Procedure  Acquire(Sender:TObject);
  Procedure  Release(Sender:TOBject);
 end;

 TvRenderTargetsSet=specialize T23treeSet<TvRenderTargets,TvRenderTargets>;

Function GetDepthStencilLayout(DEPTH_USAGE,STENCIL_USAGE:Byte):TVkImageLayout;
Function GetDepthStencilAccessMask(DEPTH_USAGE,STENCIL_USAGE:Byte):TVkAccessFlags;
Function GetColorAccessMask(IMAGE_USAGE:Byte):TVkAccessFlags;

implementation

{
const
 buf_ext:TVkExternalMemoryBufferCreateInfo=(
  sType:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO;
  pNext:nil;
  handleTypes:ord(VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT);
 );

 img_ext:TVkExternalMemoryImageCreateInfo=(
  sType:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO;
  pNext:nil;
  handleTypes:ord(VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT);
 );
}

{
Procedure TvRenderPass.Clear;
begin

 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyRenderPass(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;

 AtCount:=0;

 FillChar(ColorAt,SizeOf(ColorAt),0);
 FillChar(ColorRef,SizeOf(ColorRef),0);

 subpass:=Default(TVkSubpassDescription);
 subpass.pipelineBindPoint:=VK_PIPELINE_BIND_POINT_GRAPHICS;

 subpass.inputAttachmentCount   :=0;
 subpass.pInputAttachments      :=nil;

 subpass.colorAttachmentCount:=0;
 subpass.pColorAttachments   :=@ColorRef;

 subpass.pResolveAttachments    :=nil; //colorAttachmentCount VK_ATTACHMENT_UNUSED

 subpass.pDepthStencilAttachment:=nil; //1

 subpass.preserveAttachmentCount:=0;
 subpass.pPreserveAttachments   :=nil;

 dependency:=Default(TVkSubpassDependency);

 dependency.srcSubpass   :=VK_SUBPASS_EXTERNAL;
 dependency.dstSubpass   :=0;

 dependency.srcStageMask :=0{ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)};
 dependency.dstStageMask :=0{ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)};

 dependency.srcAccessMask:=0;
 dependency.dstAccessMask:=0{ord(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)};
end;

Procedure TvRenderPass.SetZorderStage(s:TVkPipelineStageFlags);
begin
 dependency.srcStageMask:=dependency.srcStageMask or s;
 dependency.dstStageMask:=dependency.dstStageMask or s;
end;

Procedure TvRenderPass.AddColorRef(id:TVkUInt32;IMAGE_USAGE:Byte);
var
 am:TVkAccessFlags;
begin
 if (subpass.colorAttachmentCount>7) then Exit;
 ColorRef[subpass.colorAttachmentCount].attachment:=id;
 ColorRef[subpass.colorAttachmentCount].layout    :={VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL}VK_IMAGE_LAYOUT_GENERAL;
 Inc(subpass.colorAttachmentCount);

 dependency.srcStageMask :=dependency.srcStageMask or ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT);
 dependency.dstStageMask :=dependency.dstStageMask or ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT);

 am:=GetColorAccessMask(IMAGE_USAGE);

 dependency.srcAccessMask:=dependency.srcAccessMask or am;
 dependency.dstAccessMask:=dependency.dstAccessMask or am;
end;

Procedure TvRenderPass.SetDepthStencilRef(id:TVkUInt32;DEPTH_USAGE,STENCIL_USAGE:Byte);
var
 am:TVkAccessFlags;
begin
 subpass.pDepthStencilAttachment:=@pDepthStencilRef;

 pDepthStencilRef.attachment :=id;
 pDepthStencilRef.layout     :=GetDepthStencilLayout(DEPTH_USAGE,STENCIL_USAGE);

 am:=GetDepthStencilAccessMask(DEPTH_USAGE,STENCIL_USAGE);

 dependency.srcAccessMask:=dependency.srcAccessMask or am;
 dependency.dstAccessMask:=dependency.dstAccessMask or am;
end;

Procedure TvRenderPass.AddColorAt(format:TVkFormat;IMAGE_USAGE:Byte;samples:TVkSampleCountFlagBits);
begin
 if (AtCount>8) then Exit;

 format:=vkFixFormatSupport(format,VK_IMAGE_TILING_OPTIMAL,ord(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT));

 ColorAt[AtCount]:=Default(TVkAttachmentDescription);
 ColorAt[AtCount].format        :=format;
 ColorAt[AtCount].samples       :=samples{VK_SAMPLE_COUNT_1_BIT};

 With ColorAt[AtCount] do
  if (IMAGE_USAGE and TM_CLEAR<>0) then
  begin
   loadOp:=VK_ATTACHMENT_LOAD_OP_CLEAR;
  end else
  if (IMAGE_USAGE and TM_READ<>0) then
  begin
   loadOp:=VK_ATTACHMENT_LOAD_OP_LOAD;
  end else
  begin
   loadOp:=VK_ATTACHMENT_LOAD_OP_DONT_CARE;
  end;

 With ColorAt[AtCount] do
  if (IMAGE_USAGE and TM_WRITE<>0) then
  begin
   storeOp:=VK_ATTACHMENT_STORE_OP_STORE;
  end else
  begin
   storeOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;
  end;

 ColorAt[AtCount].stencilLoadOp :=VK_ATTACHMENT_LOAD_OP_DONT_CARE;
 ColorAt[AtCount].stencilStoreOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;

 With ColorAt[AtCount] do
  if (IMAGE_USAGE and TM_READ<>0) then
  begin
   initialLayout :={VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL}VK_IMAGE_LAYOUT_GENERAL;
  end else
  begin
   initialLayout :=VK_IMAGE_LAYOUT_UNDEFINED;
  end;

 With ColorAt[AtCount] do
  finalLayout:={VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL}VK_IMAGE_LAYOUT_GENERAL;

 Inc(AtCount);
end;
}

Function GetDepthStencilLayout(DEPTH_USAGE,STENCIL_USAGE:Byte):TVkImageLayout;
begin
 if ((DEPTH_USAGE or STENCIL_USAGE) and (TM_WRITE or TM_CLEAR)<>0) then
 begin
  Result:=VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
 end else
 begin
  Result:=VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL;
 end;
end;

Function GetDepthStencilAccessMask(DEPTH_USAGE,STENCIL_USAGE:Byte):TVkAccessFlags;
begin
 Result:=(ord(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) *ord((DEPTH_USAGE or STENCIL_USAGE) and TM_READ <>0) ) or
         (ord(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT)*ord((DEPTH_USAGE or STENCIL_USAGE) and (TM_WRITE or TM_CLEAR)<>0) );
end;

Function GetColorAccessMask(IMAGE_USAGE:Byte):TVkAccessFlags;
begin
 Result:=(ord(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) *ord(IMAGE_USAGE and TM_READ<>0) ) or
         (ord(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)*ord(IMAGE_USAGE and (TM_WRITE or TM_CLEAR)<>0) );
end;

{
Procedure TvRenderPass.AddDepthAt(format:TVkFormat;DEPTH_USAGE,STENCIL_USAGE:Byte);
begin
 if (AtCount>8) then Exit;

 format:=vkFixFormatSupport(format,VK_IMAGE_TILING_OPTIMAL,ord(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT));

 ColorAt[AtCount]:=Default(TVkAttachmentDescription);
 ColorAt[AtCount].format        :=format;
 ColorAt[AtCount].samples       :=VK_SAMPLE_COUNT_1_BIT;

 With ColorAt[AtCount] do
  if (DEPTH_USAGE and TM_CLEAR<>0) then
  begin
   loadOp:=VK_ATTACHMENT_LOAD_OP_CLEAR;
  end else
  if (DEPTH_USAGE and TM_READ<>0) then
  begin
   loadOp:=VK_ATTACHMENT_LOAD_OP_LOAD;
  end else
  begin
   loadOp:=VK_ATTACHMENT_LOAD_OP_DONT_CARE;
  end;

 With ColorAt[AtCount] do
  if (DEPTH_USAGE and TM_WRITE<>0) then
  begin
   storeOp:=VK_ATTACHMENT_STORE_OP_STORE;
  end else
  begin
   storeOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;
  end;

 With ColorAt[AtCount] do
  if (STENCIL_USAGE and TM_CLEAR<>0) then
  begin
   stencilLoadOp:=VK_ATTACHMENT_LOAD_OP_CLEAR;
  end else
  if (STENCIL_USAGE and TM_READ<>0) then
  begin
   stencilLoadOp:=VK_ATTACHMENT_LOAD_OP_LOAD;
  end else
  begin
   stencilLoadOp:=VK_ATTACHMENT_LOAD_OP_DONT_CARE;
  end;

 With ColorAt[AtCount] do
  if (STENCIL_USAGE and TM_WRITE<>0) then
  begin
   stencilStoreOp:=VK_ATTACHMENT_STORE_OP_STORE;
  end else
  begin
   stencilStoreOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;
  end;

 With ColorAt[AtCount] do
  if ((DEPTH_USAGE or STENCIL_USAGE) and TM_READ<>0) then
  begin
   initialLayout :=GetDepthStencilLayout(DEPTH_USAGE,STENCIL_USAGE);
  end else
  begin
   initialLayout :=VK_IMAGE_LAYOUT_UNDEFINED;
  end;

 With ColorAt[AtCount] do
  finalLayout:=GetDepthStencilLayout(DEPTH_USAGE,STENCIL_USAGE);

 Inc(AtCount);
end;

Function TvRenderPass.Compile:Boolean;
var
 r:TVkResult;
 info:TVkRenderPassCreateInfo;
begin
 Result:=False;
 if (AtCount=0) then Exit;

 if (FHandle<>VK_NULL_HANDLE) then Exit(True);

 info:=Default(TVkRenderPassCreateInfo);
 info.sType          :=VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
 info.attachmentCount:=AtCount;
 info.pAttachments   :=@ColorAt;
 info.subpassCount   :=1;
 info.pSubpasses     :=@subpass;
 info.dependencyCount:=1;
 info.pDependencies  :=@dependency;

 r:=vkCreateRenderPass(Device.FHandle,@info,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateRenderPass:',r);
  Exit;
 end;

 Result:=True;
end;
}

//

Procedure TvGraphicsPipeline.SetPrimType(t:TVkPrimitiveTopology);
begin
 Case ord(t) of
  ord(VK_PRIMITIVE_TOPOLOGY_POINT_LIST)..ord(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY):
  begin
   inputAssembly.topology:=t;
   emulate_primtype:=0;
  end;

  DI_PT_RECTLIST:
   begin
    inputAssembly.topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP;
    emulate_primtype:=Integer(t);
   end;
  DI_PT_LINELOOP ,
  DI_PT_QUADLIST ,
  DI_PT_QUADSTRIP,
  DI_PT_POLYGON  :
  begin
   inputAssembly.topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN;
   emulate_primtype:=Integer(t);
  end;

 end;
end;

Procedure TvGraphicsPipeline.SetPrimReset(enable:TVkBool32);
begin
 inputAssembly.primitiveRestartEnable:=enable;
end;

Procedure TvGraphicsPipeline.AddVPort(const V:TVkViewport;const S:TVkRect2D);
begin
 if (s.extent.width=0) or (s.extent.height=0) then Assert(false);

 if (viewportState.viewportCount>15) then Exit;
 Viewports[viewportState.viewportCount]:=V;
 Scissors [viewportState.viewportCount]:=S;
 Inc(viewportState.viewportCount);
 viewportState.scissorCount:=viewportState.viewportCount;
end;

Procedure TvGraphicsPipeline.AddBlend(const b:TVkPipelineColorBlendAttachmentState);
begin
 if (colorBlending.attachmentCount>7) then Exit;
 ColorBlends[colorBlending.attachmentCount]:=b;
 Inc(colorBlending.attachmentCount);
end;

Procedure TvGraphicsPipeline.Clear;
begin

 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyPipeline(Device.FHandle,FHandle,nil);
  FHandle:=VK_NULL_HANDLE;
 end;

 vertexInputInfo:=Default(TVkPipelineVertexInputStateCreateInfo);
 vertexInputInfo.sType                          :=VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;

 inputAssembly:=Default(TVkPipelineInputAssemblyStateCreateInfo);
 inputAssembly.sType                 :=VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
 inputAssembly.topology              :=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 inputAssembly.primitiveRestartEnable:=VK_FALSE;  //VGT_MULTI_PRIM_IB_RESET_EN

 FillChar(Viewports  ,SizeOf(Viewports),0);
 FillChar(Scissors   ,SizeOf(Scissors) ,0);
 FillChar(ColorBlends,SizeOf(ColorBlends),0);

 FShaderGroup:=nil;

 viewportState:=Default(TVkPipelineViewportStateCreateInfo);
 viewportState.sType        :=VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
 viewportState.viewportCount:=0;
 viewportState.pViewports   :=@Viewports;
 viewportState.scissorCount :=0;
 viewportState.pScissors    :=@Scissors;

 rasterizer:=Default(TVkPipelineRasterizationStateCreateInfo);
 rasterizer.sType           :=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
 rasterizer.depthClampEnable:=VK_FALSE;
 rasterizer.rasterizerDiscardEnable:=VK_FALSE;                 //DB_SHADER_CONTROL.KILL_ENABLE  DB_RENDER_CONTROL.FORCE_COLOR_KILL
 rasterizer.polygonMode     :=VK_POLYGON_MODE_FILL;            //PA_SU_SC_MODE_CNTL.POLY_MODE  POLYMODE_FRONT_PTYPE POLYMODE_BACK_PTYPE
 rasterizer.lineWidth       :=1;                               //PA_SU_LINE_CNTL.WIDTH
 rasterizer.cullMode        :=ord(VK_CULL_MODE_NONE);          //CULL_FRONT CULL_BACK
 rasterizer.frontFace       :=VK_FRONT_FACE_COUNTER_CLOCKWISE; //FACE
 rasterizer.depthBiasEnable :=VK_FALSE;
 rasterizer.depthBiasConstantFactor:=0;
 rasterizer.depthBiasClamp         :=0;
 rasterizer.depthBiasSlopeFactor   :=0;

 multisampling:=Default(TVkPipelineMultisampleStateCreateInfo);
 multisampling.sType                :=VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
 multisampling.sampleShadingEnable  :=VK_FALSE;
 multisampling.rasterizationSamples :=VK_SAMPLE_COUNT_1_BIT;
 multisampling.minSampleShading     :=1;
 multisampling.pSampleMask          :=nil;
 multisampling.alphaToCoverageEnable:=VK_FALSE;
 multisampling.alphaToOneEnable     :=VK_FALSE;

 colorBlending:=Default(TVkPipelineColorBlendStateCreateInfo);
 colorBlending.sType            :=VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
 colorBlending.logicOpEnable    :=VK_FALSE;
 colorBlending.logicOp          :=VK_LOGIC_OP_COPY;
 colorBlending.attachmentCount  :=0;
 colorBlending.pAttachments     :=@ColorBlends;

 DepthStencil:=Default(TVkPipelineDepthStencilStateCreateInfo);
 DepthStencil.sType                :=VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
 DepthStencil.depthTestEnable      :=VK_FALSE;
 DepthStencil.depthWriteEnable     :=VK_FALSE;
 DepthStencil.depthCompareOp       :=VK_COMPARE_OP_LESS;
 DepthStencil.depthBoundsTestEnable:=VK_FALSE;
 DepthStencil.stencilTestEnable    :=VK_FALSE;
 //DepthStencil.front                :TVkStencilOpState;
 //DepthStencil.back                 :TVkStencilOpState;
 DepthStencil.minDepthBounds       :=0;
 DepthStencil.maxDepthBounds       :=0;

 dynamicState:=Default(TVkPipelineDynamicStateCreateInfo);
 dynamicState.sType            :=VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
 dynamicState.dynamicStateCount:=0{2};
 dynamicState.pDynamicStates   :=@dynamicStates;

 dynamicStates[0]:=VK_DYNAMIC_STATE_VIEWPORT;
 dynamicStates[1]:=VK_DYNAMIC_STATE_LINE_WIDTH;

end;

type
 PVec4f=^TVec4f;
 TVec4f=array[0..3] of Single;

procedure TvGraphicsPipeline.SetBlendColors(P:PSingle);
begin
 if (P=nil) then Exit;
 colorBlending.blendConstants:=PVec4f(P)^;
end;

procedure TvGraphicsPipeline.SetRenderPass(RenderPass:TvRenderPass);
begin
 if (FRenderPass<>RenderPass) then
 begin
  FRenderPass:=RenderPass;
 end;
end;

function TvGraphicsPipeline.Compile:Boolean;
type
 AVkPipelineShaderStageCreateInfo=array[0..6] of TVkPipelineShaderStageCreateInfo;
var
 r:TVkResult;
 Stages:AVkPipelineShaderStageCreateInfo; // info.stageCount
 info:TVkGraphicsPipelineCreateInfo;
begin
 Result:=False;

 if (FShaderGroup=nil) then Exit;
 if (FRenderPass=nil) then Exit;
 if (viewportState.viewportCount=0) then Exit;
 if (viewportState.scissorCount=0) then Exit;

 info:=Default(TVkGraphicsPipelineCreateInfo);

 Stages:=Default(AVkPipelineShaderStageCreateInfo);

 FShaderGroup.FKey.ExportStages(@Stages,@info.stageCount);

 if (info.stageCount=0) then Exit;

 if (not FShaderGroup.Compile) then Exit;
 //if (not FRenderPass.Compile) then Exit;

 info.sType              :=VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
 info.pStages            :=@Stages;
 info.pVertexInputState  :=@vertexInputInfo;
 info.pInputAssemblyState:=@inputAssembly;
 info.pViewportState     :=@viewportState;
 info.pRasterizationState:=@rasterizer;
 info.pMultisampleState  :=@multisampling;
 info.pDepthStencilState :=@DepthStencil;
 info.pColorBlendState   :=@colorBlending;
 info.pDynamicState      :=@dynamicState;
 info.layout             :=FShaderGroup.FLayout.FHandle;
 info.renderPass         :=FRenderPass.FHandle;
 info.subpass            :=0;
 info.basePipelineHandle :=VK_NULL_HANDLE;
 info.basePipelineIndex  :=-1;

 r:=vkCreateGraphicsPipelines(Device.FHandle,VK_NULL_HANDLE,1,@info,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateGraphicsPipelines:',r);
  exit;
 end;

 Result:=True;
end;

/////////

Procedure TvRenderTargets.AddClearColor(clr:TVkClearValue);
begin
 if (FClearValuesCount>8) then Exit;
 FClearValues[FClearValuesCount]:=clr;
 Inc(FClearValuesCount);
end;

Function TvRenderTargets.GetInfo:TVkRenderPassBeginInfo;
begin
 Result:=Default(TVkRenderPassBeginInfo);
 Result.sType          :=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
 Result.renderPass     :=FRenderPass.FHandle;
 Result.renderArea     :=FRenderArea;
 Result.clearValueCount:=FClearValuesCount;
 Result.pClearValues   :=FClearValues;
 Result.framebuffer    :=FFramebuffer.FHandle;
end;

class function TvRenderTargets.c(const a,b:TvRenderTargets):Integer;
begin
 Result:=CompareByte(a,b,SizeOf(Pointer));
end;

Destructor TvRenderTargets.Destroy;
begin
 FreeAndNil(FRenderPass);
 FreeAndNil(FPipeline);
 FreeAndNil(FFramebuffer);
 inherited;
end;

Procedure TvRenderTargets.Acquire(Sender:TObject);
begin
 System.InterlockedIncrement(Pointer(FRefs));
end;

Procedure TvRenderTargets.Release(Sender:TOBject);
begin
 if System.InterlockedDecrement(Pointer(FRefs))=nil then
 begin
  Free;
 end;
end;

Procedure TvComputePipeline2.Acquire(Sender:TObject);
begin
 System.InterlockedIncrement(Pointer(FRefs));
end;

Procedure TvComputePipeline2.Release(Sender:TOBject);
begin
 if System.InterlockedDecrement(Pointer(FRefs))=nil then
 begin
  Free;
 end;
end;

//////////////



end.

