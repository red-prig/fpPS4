unit vRender;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ps4_types,
  g23tree,
  ps4_libSceVideoOut,
  si_ci_vi_merged_enum,
  vulkan,
  vDevice,
  vMemory,
  vShader,
  vPipeline,
  vImage;

type
 TvRenderPass=class(vPipeline.TvRenderPass)

  AtCount:TVkUInt32;
  ColorAt:array[0..8] of TVkAttachmentDescription;

  ColorRef:array[0..7] of TVkAttachmentReference; //subpass.colorAttachmentCount

  pDepthStencilRef:TVkAttachmentReference;

  subpass:TVkSubpassDescription;
  dependency:TVkSubpassDependency;

  Procedure  Clear;
  Procedure  SetZorderStage(s:TVkPipelineStageFlagBits);
  Procedure  AddColorRef(id:TVkUInt32);
  Procedure  SetDepthStencilRef(id:TVkUInt32);
  Procedure  AddColorAt(format:TVkFormat;ClearColor,DrawColor:Boolean);
  Procedure  AddDepthAt(format:TVkFormat;ClearDepth,DrawDepth,ClearStencil,DrawStencil:Boolean);
  Function   Compile:Boolean;
 end;

 TvGraphicsPipeline=class(TvPipeline)
  FLayout:TvPipelineLayout;
  FRenderPass:TvRenderPass;

  Viewports:array[0..15] of TVkViewport; //viewportState.viewportCount
  Scissors :array[0..15] of TVkRect2D;   //viewportState.scissorCount

  ColorBlends:array[0..7] of TVkPipelineColorBlendAttachmentState; //colorBlending.attachmentCount

  FShaders:array[0..5] of TvShader;

  dynamicStates:array[0..1] of TVkDynamicState; //dynamicState.dynamicStateCount

  vertexInputInfo:TVkPipelineVertexInputStateCreateInfo;
  inputAssembly:TVkPipelineInputAssemblyStateCreateInfo;
  viewportState:TVkPipelineViewportStateCreateInfo;
  rasterizer:TVkPipelineRasterizationStateCreateInfo;
  multisampling:TVkPipelineMultisampleStateCreateInfo;
  colorBlending:TVkPipelineColorBlendStateCreateInfo;
  DepthStencil:TVkPipelineDepthStencilStateCreateInfo;
  dynamicState:TVkPipelineDynamicStateCreateInfo;

  emulate_primtype:TVkPrimitiveTopology;

  Procedure SetPrimType(t:TVkPrimitiveTopology);
  Procedure AddVPort(const V:TVkViewport;const S:TVkRect2D);
  Procedure AddBlend(const b:TVkPipelineColorBlendAttachmentState);
  Procedure Clear;
  Procedure SetLSShader(Shader:TvShader);
  Procedure SetHSShader(Shader:TvShader);
  Procedure SetESShader(Shader:TvShader);
  Procedure SetGSShader(Shader:TvShader);
  Procedure SetVSShader(Shader:TvShader);
  Procedure SetPSShader(Shader:TvShader);
  procedure SetLayout(Layout:TvPipelineLayout);
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
  Procedure  AddClearColor(clr:TVkClearValue);
  class function c(const a,b:TvRenderTargets):Integer;
  Destructor Destroy; override;
 end;

 TvRenderTargetsSet=specialize T23treeSet<TvRenderTargets,TvRenderTargets>;

 TvCmdBuffer=class
  cmdbuf:TVkCommandBuffer;

  FRenderTargets:TvRenderTargets;
  FRenderList:TvRenderTargetsSet;

  FWaitSemaphore:TvSemaphore;
  FSignSemaphore:TvSemaphore;
  FSignFence:TvFence;

  FCBState:Boolean;

  function  BeginCmdBuffer:Boolean;
  Procedure EndCmdBuffer;
  function  BeginRenderPass(RT:TvRenderTargets):Boolean;
  Procedure EndRenderPass;

  Procedure QueueSubmit;
  Procedure ClearRenderList;

  Procedure DrawIndex2(Addr:Pointer;INDICES:DWORD;INDEX_TYPE:TVkIndexType);
  Procedure DrawIndexAuto(INDICES:DWORD;INDEX_TYPE:TVkIndexType);
 end;

 TUnionResource=class
  Addr:Pointer;
  host:TvPointer;
 end;

 TUnionResourceBuffer=class(TUnionResource)
  FHostBuf:TvBuffer;
  Foffset:TVkDeviceSize; //offset inside buffer
  Destructor  Destroy; override;
 end;

 TUnionResourceImage=class(TUnionResource)
  FImage:TvDeviceImage2D;
  devc:TvPointer;
  Destructor  Destroy; override;
 end;

function FindHostBuffer(Addr:Pointer):TUnionResourceBuffer;
function FetchHostBuffer(Addr:Pointer;Size:TVkDeviceSize;usage:TVkFlags):TUnionResourceBuffer;

function FindUnionImage2D(Addr:Pointer):TUnionResourceImage;
function FetchUnionImage2D(Addr:Pointer;cformat:TVkFormat;extend:TVkExtent2D;usage:TVkFlags):TUnionResourceImage;

implementation

//lock res TODO

type
 TUnionResourceCompare=object
  function c(const a,b:TUnionResource):Integer; static;
 end;

 TUnionResourceSet=specialize T23treeSet<TUnionResource,TUnionResourceCompare>;

var
 FUnionBuffer:TUnionResourceSet;
 FUnionImages2D:TUnionResourceSet;

function FindHostBuffer(Addr:Pointer):TUnionResourceBuffer;
var
 i:TUnionResourceSet.Iterator;
 t:TUnionResourceBuffer;
begin
 Result:=nil;
 t:=TUnionResourceBuffer.Create;
 t.Addr:=Addr;
 i:=FUnionBuffer.find(t);
 if (i.Item<>nil) then
 begin
  Result:=TUnionResourceBuffer(i.Item^);
 end;
 FreeAndNil(t);
end;

function FetchHostBuffer(Addr:Pointer;Size:TVkDeviceSize;usage:TVkFlags):TUnionResourceBuffer;
const
 buf_ext:TVkExternalMemoryBufferCreateInfo=(
  sType:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO;
  pNext:nil;
  handleTypes:ord(VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT);
 );
var
 i:TUnionResourceSet.Iterator;
 t:TUnionResourceBuffer;
 host:TvPointer;

 procedure _init;
 var
  mr:TVkMemoryRequirements;
  p:TVkDeviceSize;
 begin
  t.host:=host;
  t.FHostBuf:=TvBuffer.Create(Size,usage,@buf_ext);
  t.Foffset:=0;

  mr:=t.FHostBuf.GetRequirements;

  if IsAlign(host.FOffset,mr.alignment) then
  begin
   t.FHostBuf.BindMem(host);
  end else
  begin
   p:=AlignDw(host.FOffset,mr.alignment);
   t.Foffset:=(host.FOffset-p);

   host.FOffset:=p;
   Size:=Size+t.Foffset;

   FreeAndNil(t.FHostBuf);
   t.FHostBuf:=TvBuffer.Create(Size,usage,@buf_ext);

   t.FHostBuf.BindMem(host);
  end;

 end;

begin
 Result:=nil;
 t:=TUnionResourceBuffer.Create;
 t.Addr:=Addr;
 i:=FUnionBuffer.find(t);
 if (i.Item=nil) then
 begin
  if not TryGetHostPointerByAddr(addr,host) then
  begin
   FreeAndNil(t);
   Exit;
  end;
  FUnionBuffer.Insert(t);
  _init;
  Result:=t;
 end else
 begin
  FreeAndNil(t);
  t:=TUnionResourceBuffer(i.Item^);
  if not TryGetHostPointerByAddr(addr,host) then
  begin
   FUnionBuffer.delete(t);
   FreeAndNil(t);
   Exit;
  end;
  if (t.host.FHandle<>host.FHandle) or
     (t.host.FOffset<>host.FOffset) or
     (t.FHostBuf.FSize<>Size) or
     (t.FHostBuf.FUsage<>usage) then
  begin
   FreeAndNil(t.FHostBuf);
   _init;
  end;
  Result:=t;
 end;
end;

function FindUnionImage2D(Addr:Pointer):TUnionResourceImage;
var
 i:TUnionResourceSet.Iterator;
 t:TUnionResourceImage;
begin
 Result:=nil;
 t:=TUnionResourceImage.Create;
 t.Addr:=Addr;
 i:=FUnionImages2D.find(t);
 if (i.Item<>nil) then
 begin
  Result:=TUnionResourceImage(i.Item^);
 end;
 FreeAndNil(t);
end;

function FetchUnionImage2D(Addr:Pointer;cformat:TVkFormat;extend:TVkExtent2D;usage:TVkFlags):TUnionResourceImage;
var
 i:TUnionResourceSet.Iterator;
 t:TUnionResourceImage;
 host:TvPointer;

 procedure _init;
 begin
  t.host:=host;
  t.FImage:=TvDeviceImage2D.Create(cformat,TVkExtent3D.Create(extend.width,extend.height,1),usage);
  t.devc:=MemManager.Alloc(
    t.FImage.GetRequirements,
    ord(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
  );
  t.FImage.BindMem(t.devc);
 end;

begin
 Result:=nil;
 t:=TUnionResourceImage.Create;
 t.Addr:=Addr;
 i:=FUnionImages2D.find(t);
 if (i.Item=nil) then
 begin
  if not TryGetHostPointerByAddr(addr,host) then
  begin
   FreeAndNil(t);
   Exit;
  end;
  FUnionImages2D.Insert(t);
  _init;
  Result:=t;
 end else
 begin
  FreeAndNil(t);
  t:=TUnionResourceImage(i.Item^);
  if not TryGetHostPointerByAddr(addr,host) then
  begin
   FUnionImages2D.delete(t);
   FreeAndNil(t);
   Exit;
  end;
  if (t.host.FHandle<>host.FHandle) or
     (t.host.FOffset<>host.FOffset) or
     (t.FImage.FFormat<>cformat) or
     (t.FImage.FUsage<>usage) or
     (t.FImage.FExtent.width<>extend.width) or
     (t.FImage.FExtent.height<>extend.height) then
  begin
   FreeAndNil(t.FImage);
   MemManager.Free(t.devc);
   _init;
  end;
  Result:=t;
 end;
end;

Procedure TvRenderPass.Clear;
begin
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

Procedure TvRenderPass.SetZorderStage(s:TVkPipelineStageFlagBits);
begin
 dependency.srcStageMask:=TVkPipelineStageFlags(ord(dependency.srcStageMask) or ord(s));
 dependency.dstStageMask:=TVkPipelineStageFlags(ord(dependency.dstStageMask) or ord(s));
end;

Procedure TvRenderPass.AddColorRef(id:TVkUInt32);
begin
 if (subpass.colorAttachmentCount>7) then Exit;
 ColorRef[subpass.colorAttachmentCount].attachment:=id;
 ColorRef[subpass.colorAttachmentCount].layout    :=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL{VK_IMAGE_LAYOUT_GENERAL};
 Inc(subpass.colorAttachmentCount);

 dependency.srcStageMask :=TVkPipelineStageFlags(ord(dependency.srcStageMask) or ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT));
 dependency.dstStageMask :=TVkPipelineStageFlags(ord(dependency.dstStageMask) or ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT));
 dependency.dstAccessMask:=TVkAccessFlags(ord(dependency.dstAccessMask) or ord(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT));
end;

Procedure TvRenderPass.SetDepthStencilRef(id:TVkUInt32);
begin
 subpass.pDepthStencilAttachment:=@pDepthStencilRef;
 pDepthStencilRef.attachment :=id;
 pDepthStencilRef.layout     :=VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;

 dependency.dstAccessMask:=TVkAccessFlags(ord(dependency.dstAccessMask) or ord(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT));
end;

Procedure TvRenderPass.AddColorAt(format:TVkFormat;ClearColor,DrawColor:Boolean);
begin
 if (AtCount>8) then Exit;

 ColorAt[AtCount]:=Default(TVkAttachmentDescription);
 ColorAt[AtCount].format        :=format;
 ColorAt[AtCount].samples       :=VK_SAMPLE_COUNT_1_BIT;

 Case ClearColor of
  True :ColorAt[AtCount].loadOp:=VK_ATTACHMENT_LOAD_OP_CLEAR;
  False:ColorAt[AtCount].loadOp:=VK_ATTACHMENT_LOAD_OP_DONT_CARE;
 end;

 Case DrawColor of
  True :ColorAt[AtCount].storeOp:=VK_ATTACHMENT_STORE_OP_STORE;
  False:ColorAt[AtCount].storeOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;
 end;

 ColorAt[AtCount].stencilLoadOp :=VK_ATTACHMENT_LOAD_OP_DONT_CARE;
 ColorAt[AtCount].stencilStoreOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;

 ColorAt[AtCount].initialLayout :=VK_IMAGE_LAYOUT_UNDEFINED;
 ColorAt[AtCount].finalLayout   :=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;

 {VK_IMAGE_LAYOUT_GENERAL}
 {VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL};
 Inc(AtCount);
end;

Procedure TvRenderPass.AddDepthAt(format:TVkFormat;ClearDepth,DrawDepth,ClearStencil,DrawStencil:Boolean);
begin
 if (AtCount>8) then Exit;

 ColorAt[AtCount]:=Default(TVkAttachmentDescription);
 ColorAt[AtCount].format        :=format;
 ColorAt[AtCount].samples       :=VK_SAMPLE_COUNT_1_BIT;

 Case ClearDepth of
  True :ColorAt[AtCount].loadOp:=VK_ATTACHMENT_LOAD_OP_CLEAR;
  False:ColorAt[AtCount].loadOp:=VK_ATTACHMENT_LOAD_OP_DONT_CARE;
 end;

 Case DrawDepth of
  True :ColorAt[AtCount].storeOp:=VK_ATTACHMENT_STORE_OP_STORE;
  False:ColorAt[AtCount].storeOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;
 end;

 Case ClearStencil of
  True :ColorAt[AtCount].stencilLoadOp:=VK_ATTACHMENT_LOAD_OP_CLEAR;
  False:ColorAt[AtCount].stencilLoadOp:=VK_ATTACHMENT_LOAD_OP_DONT_CARE;
 end;

 Case DrawStencil of
  True :ColorAt[AtCount].stencilStoreOp:=VK_ATTACHMENT_STORE_OP_STORE;
  False:ColorAt[AtCount].stencilStoreOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;
 end;

 ColorAt[AtCount].initialLayout :=VK_IMAGE_LAYOUT_UNDEFINED;
 ColorAt[AtCount].finalLayout   :=VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;

 {VK_IMAGE_LAYOUT_GENERAL}
 {VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL};
 Inc(AtCount);
end;

Function TvRenderPass.Compile:Boolean;
var
 r:TVkResult;
 info:TVkRenderPassCreateInfo;
begin
 Result:=False;
 if (AtCount=0) then Exit;

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
  Writeln('vkCreateRenderPass');
  Exit;
 end;

 Result:=True;
end;

//

Procedure TvGraphicsPipeline.SetPrimType(t:TVkPrimitiveTopology);
begin
 Case ord(t) of
  ord(VK_PRIMITIVE_TOPOLOGY_POINT_LIST)..ord(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY):
   inputAssembly.topology:=t;

  DI_PT_RECTLIST ,
  DI_PT_LINELOOP ,
  DI_PT_QUADLIST ,
  DI_PT_QUADSTRIP,
  DI_PT_POLYGON  :
  begin
   inputAssembly.topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP;
   emulate_primtype:=t;
  end;

 end;
end;

Procedure TvGraphicsPipeline.AddVPort(const V:TVkViewport;const S:TVkRect2D);
begin
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
 vertexInputInfo:=Default(TVkPipelineVertexInputStateCreateInfo);
 vertexInputInfo.sType                          :=VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;

 inputAssembly:=Default(TVkPipelineInputAssemblyStateCreateInfo);
 inputAssembly.sType                 :=VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
 inputAssembly.topology              :=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 inputAssembly.primitiveRestartEnable:=VK_FALSE;

 FillChar(Viewports  ,SizeOf(Viewports),0);
 FillChar(Scissors   ,SizeOf(Scissors) ,0);
 FillChar(ColorBlends,SizeOf(ColorBlends),0);
 FillChar(FShaders   ,SizeOf(FShaders),0);

 viewportState:=Default(TVkPipelineViewportStateCreateInfo);
 viewportState.sType        :=VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
 viewportState.viewportCount:=0;
 viewportState.pViewports   :=@Viewports;
 viewportState.scissorCount :=0;
 viewportState.pScissors    :=@Scissors;

 rasterizer:=Default(TVkPipelineRasterizationStateCreateInfo);
 rasterizer.sType           :=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
 rasterizer.depthClampEnable:=VK_FALSE;
 rasterizer.rasterizerDiscardEnable:=VK_FALSE;
 rasterizer.polygonMode     :=VK_POLYGON_MODE_FILL;
 rasterizer.lineWidth       :=1;
 rasterizer.cullMode        :=ord(VK_CULL_MODE_NONE);
 rasterizer.frontFace       :=VK_FRONT_FACE_COUNTER_CLOCKWISE;
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

//kShaderStageCs = 0x00000000,	///< Compute shader stage.
//kShaderStagePs = 0x00000001,	///< Pixel shader stage.
//kShaderStageVs = 0x00000002,	///< Vertex shader stage.
//kShaderStageGs = 0x00000003,	///< Geometry shader stage.
//kShaderStageEs = 0x00000004,	///< Export shader stage.
//kShaderStageHs = 0x00000005,	///< Hull shader stage.
//kShaderStageLs = 0x00000006,	///< LDS shader stage. = Vertex shader to share data

//kActiveShaderStagesVsPs             = 0x00000000
//kActiveShaderStagesEsGsVsPs         = 0x000000B0
//kActiveShaderStagesLsHsVsPs         = 0x00000045
//kActiveShaderStagesLsHsEsGsVsPs     = 0x000000AD
//kActiveShaderStagesDispatchDrawVsPs = 0x00000200

//Logical Pipeline ↓
//VK_SHADER_STAGE_VERTEX_BIT                  Vertex             VS	LS	LS	ES
//VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT    Hull               []	HS	HS	[]
//VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT Domain             []	VS	ES	[]
//VK_SHADER_STAGE_GEOMETRY_BIT                Geometry           []	[]	GS, VS	GS, VS
//VK_SHADER_STAGE_FRAGMENT_BIT                Pixel              PS	PS	PS	PS

//0 LS  VK_SHADER_STAGE_VERTEX_BIT
//1 HS  VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
//2 ES  VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
//3 GS  VK_SHADER_STAGE_GEOMETRY_BIT
//4 VS  VK_SHADER_STAGE_VERTEX_BIT
//5 PS  VK_SHADER_STAGE_FRAGMENT_BIT

Procedure TvGraphicsPipeline.SetLSShader(Shader:TvShader);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_VERTEX_BIT) then
  FShaders[0]:=Shader;
end;

Procedure TvGraphicsPipeline.SetHSShader(Shader:TvShader);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) then
  FShaders[1]:=Shader;
end;

Procedure TvGraphicsPipeline.SetESShader(Shader:TvShader);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) then
  FShaders[2]:=Shader;
end;

Procedure TvGraphicsPipeline.SetGSShader(Shader:TvShader);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_GEOMETRY_BIT) then
  FShaders[3]:=Shader;
end;

Procedure TvGraphicsPipeline.SetVSShader(Shader:TvShader);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_VERTEX_BIT) then
  FShaders[4]:=Shader;
end;

Procedure TvGraphicsPipeline.SetPSShader(Shader:TvShader);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_FRAGMENT_BIT) then
  FShaders[5]:=Shader;
end;

procedure TvGraphicsPipeline.SetLayout(Layout:TvPipelineLayout);
begin
 if (FLayout<>Layout) then
 begin
  FLayout:=Layout;
 end;
end;

procedure TvGraphicsPipeline.SetRenderPass(RenderPass:TvRenderPass);
begin
 if (FRenderPass<>RenderPass) then
 begin
  FRenderPass:=RenderPass;
 end;
end;

function TvGraphicsPipeline.Compile:Boolean;
var
 r:TVkResult;
 i:Integer;
 shaderStages:array[0..5] of TVkPipelineShaderStageCreateInfo; // info.stageCount
 info:TVkGraphicsPipelineCreateInfo;
begin
 Result:=False;
 if (FLayout=nil) then Exit;
 if (FRenderPass=nil) then Exit;
 if (viewportState.viewportCount=0) then Exit;
 if (viewportState.scissorCount=0) then Exit;

 info:=Default(TVkGraphicsPipelineCreateInfo);
 FillChar(shaderStages,SizeOf(shaderStages),0);
 For i:=0 to 5 do
  if (FShaders[i]<>nil) then
  begin
   shaderStages[info.stageCount].sType :=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
   shaderStages[info.stageCount].stage :=FShaders[i].FStage;
   shaderStages[info.stageCount].module:=FShaders[i].FHandle;
   shaderStages[info.stageCount].pName :=PChar(FShaders[i].FEntry);
   Inc(info.stageCount);
  end;
 if (info.stageCount=0) then Exit;

 if (not FLayout.Compile) then Exit;
 if (not FRenderPass.Compile) then Exit;

 info.sType              :=VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
 info.pStages            :=@shaderStages;
 info.pVertexInputState  :=@vertexInputInfo;
 info.pInputAssemblyState:=@inputAssembly;
 info.pViewportState     :=@viewportState;
 info.pRasterizationState:=@rasterizer;
 info.pMultisampleState  :=@multisampling;
 info.pDepthStencilState :=@DepthStencil;
 info.pColorBlendState   :=@colorBlending;
 info.pDynamicState      :=@dynamicState;
 info.layout             :=FLayout.FHandle;
 info.renderPass         :=FRenderPass.FHandle;
 info.subpass            :=0;
 info.basePipelineHandle :=VK_NULL_HANDLE;
 info.basePipelineIndex  :=-1;

 r:=vkCreateGraphicsPipelines(Device.FHandle,VK_NULL_HANDLE,1,@info,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('failed to create graphics pipeline!');
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

///////////////

function TvCmdBuffer.BeginCmdBuffer:Boolean;
var
 r:TVkResult;
 Info:TVkCommandBufferBeginInfo;
begin
 Result:=False;
 if (Self=nil) then Exit;
 if FCBState then Exit(True);
 if (cmdbuf=VK_NULL_HANDLE) then Exit;
 Info:=Default(TVkCommandBufferBeginInfo);
 Info.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
 Info.flags:=ord(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT);
 Info.pInheritanceInfo:=nil;
 r:=vkBeginCommandBuffer(cmdbuf,@Info);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkBeginCommandBuffer:',r);
  Exit;
 end;
 FCBState:=True;
 Result:=True;
end;

Procedure TvCmdBuffer.EndCmdBuffer;
var
 r:TVkResult;
begin
 if (Self=nil) then Exit;
 if FCBState then
 begin
  EndRenderPass;
  r:=vkEndCommandBuffer(cmdbuf);
  if (r<>VK_SUCCESS) then
  begin
   Writeln('vkEndCommandBuffer:',r);
  end;
  FCBState:=False;
 end;
end;

function TvCmdBuffer.BeginRenderPass(RT:TvRenderTargets):Boolean;
var
 info:TVkRenderPassBeginInfo;
begin
 Result:=False;

 if (Self=nil) then Exit;
 if (cmdbuf=VK_NULL_HANDLE) then Exit;

 if (RT=nil) then
 begin
  EndRenderPass;
  Exit(True);
 end;

 if (FRenderTargets=RT) then Exit(True);
 if (RT.FRenderPass=nil) then Exit;
 if (RT.FPipeline=nil) then Exit;
 if (RT.FFramebuffer=nil) then Exit;

 if (not RT.FRenderPass.Compile) then Exit;
 if (not RT.FPipeline.Compile) then Exit;
 if (not RT.FFramebuffer.Compile) then Exit;

 if (not BeginCmdBuffer) then Exit;

 EndRenderPass;

 info:=Default(TVkRenderPassBeginInfo);
 info.sType          :=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
 info.renderPass     :=RT.FRenderPass.FHandle;
 info.renderArea     :=RT.FRenderArea;
 info.clearValueCount:=RT.FClearValuesCount;
 info.pClearValues   :=RT.FClearValues;
 info.framebuffer    :=RT.FFramebuffer.FHandle;

 vkCmdBeginRenderPass(cmdbuf,@info,VK_SUBPASS_CONTENTS_INLINE);
 vkCmdBindPipeline   (cmdbuf,VK_PIPELINE_BIND_POINT_GRAPHICS,RT.FPipeline.FHandle);

 FRenderTargets:=RT;

 Result:=True;
end;

Procedure TvCmdBuffer.EndRenderPass;
begin
 if (Self=nil) then Exit;
 if (FRenderTargets<>nil) then
 begin
  vkCmdEndRenderPass(cmdbuf);
  FRenderList.Insert(FRenderTargets);
  FRenderTargets:=nil;
 end;
end;

Procedure TvCmdBuffer.QueueSubmit;
var
 r:TVkResult;
 info:TVkSubmitInfo;
 waitStages:TVkPipelineStageFlags;
 Fence:TVkFence;
begin
 if (Self=nil) then Exit;
 if (cmdbuf=VK_NULL_HANDLE) then Exit;
 EndCmdBuffer;

 waitStages:=ord(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT);

 info:=Default(TVkSubmitInfo);
 info.sType           :=VK_STRUCTURE_TYPE_SUBMIT_INFO;
 info.commandBufferCount :=1;
 info.pCommandBuffers    :=@cmdbuf;
 info.pWaitDstStageMask :=@waitStages;

 if (FWaitSemaphore<>nil) then
 begin
  info.waitSemaphoreCount:=1;
  info.pWaitSemaphores   :=@FWaitSemaphore.FHandle;
 end;

 if (FSignSemaphore<>nil) then
 begin
  info.signalSemaphoreCount:=1;
  info.pSignalSemaphores   :=@FSignSemaphore.FHandle;
 end;

 Fence:=VK_NULL_HANDLE;
 if (FSignFence<>nil) then
 begin
  Fence:=FSignFence.FHandle;
 end;

 r:=vkQueueSubmit(RenderQueue,1,@info,Fence);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkQueueSubmit');
  exit;
 end;
end;

Procedure TvCmdBuffer.ClearRenderList;
var
 It:TvRenderTargetsSet.Iterator;
begin
 if (Self=nil) then Exit;
 It:=FRenderList.cbegin;
 if (It.Item<>nil) then
 repeat
  TvRenderTargets(It.Item^).Free;
 until not It.Next;
 FRenderList.Free;
 FreeAndNil(FRenderTargets);
end;

function GET_INDEX_TYPE_SIZE(INDEX_TYPE:TVkIndexType):Byte;
begin
 Case INDEX_TYPE of
  VK_INDEX_TYPE_UINT16:Result:=16;
  VK_INDEX_TYPE_UINT32:Result:=32;
  VK_INDEX_TYPE_UINT8_EXT :Result:=8;
  else         Result:=0;
 end;
end;

Procedure TvCmdBuffer.DrawIndex2(Addr:Pointer;INDICES:DWORD;INDEX_TYPE:TVkIndexType);
var
 rb:TUnionResourceBuffer;
 Size:TVkDeviceSize;
begin
 if (Self=nil) then Exit;
 if (FRenderTargets=nil) then Exit;

 Size:=INDICES*GET_INDEX_TYPE_SIZE(INDEX_TYPE);
 rb:=FetchHostBuffer(Addr,Size,ord(VK_BUFFER_USAGE_INDEX_BUFFER_BIT));
 Assert(rb<>nil);

 vkCmdBindIndexBuffer(
     cmdbuf,
     rb.FHostBuf.FHandle,
     rb.Foffset,
     INDEX_TYPE);

 vkCmdDrawIndexed(
     cmdbuf,
     INDICES,
     1,0,0,0);

end;

Procedure TvCmdBuffer.DrawIndexAuto(INDICES:DWORD;INDEX_TYPE:TVkIndexType);
begin
 if (Self=nil) then Exit;
 if (FRenderTargets=nil) then Exit;

 vkCmdDraw(
     cmdbuf,
     INDICES,
     1,0,0);

end;


//

function TUnionResourceCompare.c(const a,b:TUnionResource):Integer;
begin
 Result:=CompareByte(a.Addr,b.Addr,SizeOf(Pointer));
end;

//

Destructor TUnionResourceBuffer.Destroy;
begin
 FreeAndNil(FHostBuf);
 inherited;
end;

//

Destructor TUnionResourceImage.Destroy;
begin
 FreeAndNil(FImage);
 MemManager.Free(devc);
 inherited;
end;


end.

