unit vPipelineManager;

{$mode ObjFPC}{$H+}

interface

uses
 SysUtils,
 g23tree,
 Vulkan,
 vDevice,
 vDependence,
 vPipeline,
 vShaderExt,
 si_ci_vi_merged_enum;

type
 TvViewportState=packed record
  viewportCount:TVkUInt32;
  scissorCount :TVkUInt32;
 end;

 TvColorBlendState=packed record
  logicOp        :TVkLogicOp;
  attachmentCount:TVkUInt32;
  blendConstants :array[0..3] of TVkFloat;
 end;

 TvVertexInput=packed record
  vertexBindingDescriptionCount  :Byte;
  vertexAttributeDescriptionCount:Byte;
  VertexBindingDescriptions      :AvVertexInputBindingDescription;
  VertexAttributeDescriptions    :AvVertexInputAttributeDescription;
 end;

 PvGraphicsPipelineKey=^TvGraphicsPipelineKey;
 TvGraphicsPipelineKey=packed object
  FRenderPass :TvRenderPass;
  FShaderGroup:TvShaderGroup;

  Viewports:array[0..15] of TVkViewport; //viewportState.viewportCount
  Scissors :array[0..15] of TVkRect2D;   //viewportState.scissorCount

  ColorBlends:array[0..7] of TVkPipelineColorBlendAttachmentState; //colorBlending.attachmentCount

  vertexInputInfo:TvVertexInput;
  inputAssembly  :TVkPipelineInputAssemblyStateCreateInfo;
  viewportState  :TvViewportState;
  rasterizer     :TVkPipelineRasterizationStateCreateInfo;
  multisampling  :TVkPipelineMultisampleStateCreateInfo;
  colorBlending  :TvColorBlendState;
  DepthStencil   :TVkPipelineDepthStencilStateCreateInfo;

  provokingVertexMode:TVkProvokingVertexModeEXT;

  emulate_primtype:Integer;

  Procedure Clear;
  Procedure SetVertexInput(var FAttrBuilder:TvAttrBuilder);
  Procedure SetPrimType(t:TVkPrimitiveTopology);
  Procedure SetPrimReset(enable:TVkBool32);
  Procedure SetProvoking(t:TVkProvokingVertexModeEXT);
  Procedure AddVPort(const V:TVkViewport;const S:TVkRect2D);
  Procedure AddBlend(const b:TVkPipelineColorBlendAttachmentState);
  procedure SetBlendInfo(logicOp:TVkLogicOp;P:PSingle);
 end;

 TvGraphicsPipeline2=class(TvPipeline)
  Key:TvGraphicsPipelineKey;
  //
  FRefs:ptruint;
  Function  Compile:Boolean;
  Procedure Acquire;
  procedure Release(Sender:TObject);
 end;

function FetchGraphicsPipeline(cmd:TvDependenciesObject;P:PvGraphicsPipelineKey):TvGraphicsPipeline2;

implementation

//

uses
 kern_rwlock;

type
 TvGraphicsPipelineKey2Compare=object
  function c(a,b:PvGraphicsPipelineKey):Integer; static;
 end;

 TvGraphicsPipeline2Set=specialize T23treeSet<PvGraphicsPipelineKey,TvGraphicsPipelineKey2Compare>;

var
 global_lock:Pointer=nil;

 FGlobalCache:TvPipelineCache;

 FGraphicsPipeline2Set:TvGraphicsPipeline2Set;


function TvGraphicsPipelineKey2Compare.c(a,b:PvGraphicsPipelineKey):Integer;
begin
 Result:=CompareByte(a^,b^,SizeOf(TvGraphicsPipelineKey));
end;

Procedure Global_Lock_wr;
begin
 rw_wlock(global_lock);
end;

Procedure Global_Unlock_wr;
begin
 rw_wunlock(global_lock);
end;

//

Procedure TvGraphicsPipelineKey.Clear;
begin
 Self:=Default(TvGraphicsPipelineKey);

 inputAssembly.sType                 :=VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
 inputAssembly.topology              :=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 inputAssembly.primitiveRestartEnable:=VK_FALSE;  //VGT_MULTI_PRIM_IB_RESET_EN

 rasterizer.sType                  :=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
 rasterizer.depthClampEnable       :=VK_FALSE;
 rasterizer.rasterizerDiscardEnable:=VK_FALSE;                        //DB_SHADER_CONTROL.KILL_ENABLE  DB_RENDER_CONTROL.FORCE_COLOR_KILL
 rasterizer.polygonMode            :=VK_POLYGON_MODE_FILL;            //PA_SU_SC_MODE_CNTL.POLY_MODE  POLYMODE_FRONT_PTYPE POLYMODE_BACK_PTYPE
 rasterizer.lineWidth              :=1;                               //PA_SU_LINE_CNTL.WIDTH
 rasterizer.cullMode               :=ord(VK_CULL_MODE_NONE);          //CULL_FRONT CULL_BACK
 rasterizer.frontFace              :=VK_FRONT_FACE_COUNTER_CLOCKWISE; //FACE

 multisampling.sType               :=VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
 multisampling.sampleShadingEnable :=VK_FALSE;
 multisampling.rasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
 multisampling.minSampleShading    :=1;

 colorBlending.logicOp:=VK_LOGIC_OP_COPY;

 DepthStencil.sType         :=VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
 DepthStencil.depthCompareOp:=VK_COMPARE_OP_LESS;
end;

Procedure TvGraphicsPipelineKey.SetVertexInput(var FAttrBuilder:TvAttrBuilder);
begin
 vertexInputInfo.vertexBindingDescriptionCount  :=FAttrBuilder.FBindDescsCount;
 vertexInputInfo.vertexAttributeDescriptionCount:=FAttrBuilder.FAttrDescsCount;
 vertexInputInfo.VertexBindingDescriptions      :=FAttrBuilder.FBindDescs;
 vertexInputInfo.VertexAttributeDescriptions    :=FAttrBuilder.FAttrDescs;
end;

Procedure TvGraphicsPipelineKey.SetPrimType(t:TVkPrimitiveTopology);
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

Procedure TvGraphicsPipelineKey.SetPrimReset(enable:TVkBool32);
begin
 inputAssembly.primitiveRestartEnable:=enable;
end;

Procedure TvGraphicsPipelineKey.SetProvoking(t:TVkProvokingVertexModeEXT);
begin
 provokingVertexMode:=t;
end;

Procedure TvGraphicsPipelineKey.AddVPort(const V:TVkViewport;const S:TVkRect2D);
begin
 if (s.extent.width=0) or (s.extent.height=0) then Assert(false);

 if (viewportState.viewportCount>15) then Exit;
 Viewports[viewportState.viewportCount]:=V;
 Scissors [viewportState.viewportCount]:=S;
 Inc(viewportState.viewportCount);
 viewportState.scissorCount:=viewportState.viewportCount;
end;

Procedure TvGraphicsPipelineKey.AddBlend(const b:TVkPipelineColorBlendAttachmentState);
begin
 if (colorBlending.attachmentCount>7) then Exit;
 ColorBlends[colorBlending.attachmentCount]:=b;
 Inc(colorBlending.attachmentCount);
end;

type
 PVec4f=^TVec4f;
 TVec4f=array[0..3] of Single;

procedure TvGraphicsPipelineKey.SetBlendInfo(logicOp:TVkLogicOp;P:PSingle);
begin
 colorBlending.logicOp:=logicOp;

 if (P=nil) then Exit;
 colorBlending.blendConstants:=PVec4f(P)^;
end;

///

Function TvGraphicsPipeline2.Compile:Boolean;
type
 AVkPipelineShaderStageCreateInfo=array[0..6] of TVkPipelineShaderStageCreateInfo;

var
 FCache:TVkPipelineCache;

 r:TVkResult;
 Stages:AVkPipelineShaderStageCreateInfo; // info.stageCount
 info  :TVkGraphicsPipelineCreateInfo;

 vertexInputInfo:TVkPipelineVertexInputStateCreateInfo;
 viewportState  :TVkPipelineViewportStateCreateInfo;
 colorBlending  :TVkPipelineColorBlendStateCreateInfo;
 rasterizer     :TVkPipelineRasterizationStateCreateInfo;
 ProvokingVertex:TVkPipelineRasterizationProvokingVertexStateCreateInfoEXT;
begin
 Result:=False;

 if (Key.FRenderPass =nil) then Exit;
 if (Key.FShaderGroup=nil) then Exit;
 if (Key.viewportState.viewportCount=0) then Exit;
 if (Key.viewportState.scissorCount =0) then Exit;

 info:=Default(TVkGraphicsPipelineCreateInfo);

 Stages:=Default(AVkPipelineShaderStageCreateInfo);
 Key.FShaderGroup.FKey.ExportStages(@Stages,@info.stageCount);

 if (info.stageCount=0) then Exit;

 //

 //VK_EXT_vertex_input_dynamic_state
 //VK_EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME
 // VkPhysicalDeviceVertexInputDynamicStateFeaturesEXT
 // VK_DYNAMIC_STATE_VERTEX_INPUT_EXT
 // VkVertexInputBindingDescription2EXT
 // VkVertexInputAttributeDescription2EXT

 vertexInputInfo:=Default(TVkPipelineVertexInputStateCreateInfo);
 vertexInputInfo.sType                          :=VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
 vertexInputInfo.vertexBindingDescriptionCount  :=Key.vertexInputInfo.vertexBindingDescriptionCount;
 vertexInputInfo.vertexAttributeDescriptionCount:=Key.vertexInputInfo.vertexAttributeDescriptionCount;
 vertexInputInfo.pVertexBindingDescriptions     :=@Key.vertexInputInfo.VertexBindingDescriptions;
 vertexInputInfo.pVertexAttributeDescriptions   :=@Key.vertexInputInfo.VertexAttributeDescriptions;

 viewportState:=Default(TVkPipelineViewportStateCreateInfo);
 viewportState.sType        :=VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
 viewportState.viewportCount:=Key.viewportState.viewportCount;
 viewportState.pViewports   :=@Key.Viewports;
 viewportState.scissorCount :=Key.viewportState.scissorCount;
 viewportState.pScissors    :=@Key.Scissors;

 colorBlending:=Default(TVkPipelineColorBlendStateCreateInfo);
 colorBlending.sType          :=VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
 colorBlending.logicOpEnable  :=ord(Key.colorBlending.logicOp<>VK_LOGIC_OP_COPY);
 colorBlending.logicOp        :=Key.colorBlending.logicOp;
 colorBlending.attachmentCount:=Key.colorBlending.attachmentCount;
 colorBlending.pAttachments   :=@Key.ColorBlends;

 rasterizer:=Key.rasterizer;

 if limits.VK_EXT_provoking_vertex then
 begin
  rasterizer.pNext:=@ProvokingVertex;
  //
  ProvokingVertex:=Default(TVkPipelineRasterizationProvokingVertexStateCreateInfoEXT);
  ProvokingVertex.sType              :=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_PROVOKING_VERTEX_STATE_CREATE_INFO_EXT;
  ProvokingVertex.provokingVertexMode:=Key.provokingVertexMode;
 end;

 //

 info.sType              :=VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
 info.pStages            :=@Stages;
 info.pVertexInputState  :=@vertexInputInfo;
 info.pInputAssemblyState:=@Key.inputAssembly;
 info.pViewportState     :=@viewportState;
 info.pRasterizationState:=@rasterizer;
 info.pMultisampleState  :=@Key.multisampling;
 info.pDepthStencilState :=@Key.DepthStencil;
 info.pColorBlendState   :=@colorBlending;
 info.pDynamicState      :=nil;
 info.layout             :=Key.FShaderGroup.FLayout.FHandle;
 info.renderPass         :=Key.FRenderPass.FHandle;
 info.subpass            :=0;
 info.basePipelineHandle :=VK_NULL_HANDLE;
 info.basePipelineIndex  :=-1;

 FCache:=VK_NULL_HANDLE;
 if (FPCache<>nil) then
 begin
  FCache:=FPCache.FHandle;
 end;

 r:=vkCreateGraphicsPipelines(Device.FHandle,FCache,1,@info,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateGraphicsPipelines:',r);
  Exit;
 end;

 Result:=True;
end;

Procedure TvGraphicsPipeline2.Acquire;
begin
 System.InterlockedIncrement(Pointer(FRefs));
end;

procedure TvGraphicsPipeline2.Release(Sender:TObject);
begin
 if System.InterlockedDecrement(Pointer(FRefs))=nil then
 begin
  Free;
 end;
end;

///

function _Find(P:PvGraphicsPipelineKey):TvGraphicsPipeline2;
var
 i:TvGraphicsPipeline2Set.Iterator;
begin
 Result:=nil;
 i:=FGraphicsPipeline2Set.find(P);
 if (i.Item<>nil) then
 begin
  Result:=TvGraphicsPipeline2(ptruint(i.Item^)-ptruint(@TvGraphicsPipeline2(nil).key));
 end;
end;

function _FetchGraphicsPipeline(P:PvGraphicsPipelineKey):TvGraphicsPipeline2;
var
 t:TvGraphicsPipeline2;
begin
 Result:=nil;

 t:=_Find(P);

 if (t=nil) then
 begin

  if (FGlobalCache=nil) then
  begin
   FGlobalCache:=TvPipelineCache.Create(nil,0);
  end;

  t:=TvGraphicsPipeline2.Create;
  t.FPCache:=FGlobalCache;

  t.key:=P^;

  if not t.Compile then
  begin
   FreeAndNil(t);
  end else
  begin
   t.Acquire;
   FGraphicsPipeline2Set.Insert(@t.key);
  end;
 end;

 Result:=t;
end;

function FetchGraphicsPipeline(cmd:TvDependenciesObject;P:PvGraphicsPipelineKey):TvGraphicsPipeline2;
begin
 Result:=nil;
 if (P=nil) then Exit;

 Global_Lock_wr;

 Result:=_FetchGraphicsPipeline(P);

 if (cmd<>nil) and (Result<>nil) then
 begin
  if cmd.AddDependence(@Result.Release) then
  begin
   Result.Acquire;
  end;
 end;

 Global_Unlock_wr;
end;



end.

