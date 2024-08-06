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
 vShader,
 vShaderExt,
 si_ci_vi_merged_enum;

type
 TvColorBlendState=packed record
  logicOp        :Byte;
  attachmentCount:Byte;
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

  inputAssembly  :TVkPipelineInputAssemblyStateCreateInfo;
  rasterizer     :TVkPipelineRasterizationStateCreateInfo;
  multisampling  :TVkPipelineMultisampleStateCreateInfo;
  DepthStencil   :TVkPipelineDepthStencilStateCreateInfo;

  vertexInputInfo:TvVertexInput;
  colorBlending  :TvColorBlendState;

  viewportCount   :Byte;
  provokingVertex :Byte;
  emulate_primtype:ShortInt;
  shader_primtype :ShortInt;

  Procedure Clear;
  Procedure SetVertexInput(const FAttrBuilder:TvAttrBuilder);
  Procedure SetPrimType(t,s:Integer);
  Procedure SetPrimReset(enable:TVkBool32);
  Procedure SetProvoking(t:TVkProvokingVertexModeEXT);
  Procedure AddVPort(const V:TVkViewport;const S:TVkRect2D);
  Procedure AddBlend(const b:TVkPipelineColorBlendAttachmentState);
  procedure SetBlendInfo(logicOp:TVkLogicOp;P:PSingle);
 end;

 TvGraphicsPipeline2=class(TvPipeline)
  Key:TvGraphicsPipelineKey;
  //
  Function  Compile:Boolean;
 end;

 //

 PvComputePipelineKey=^TvComputePipelineKey;
 TvComputePipelineKey=packed object
  FShaderGroup:TvShaderGroup;
 end;

 TvComputePipeline2=class(TvPipeline)
  Key:TvComputePipelineKey;
  //
  Function  Compile:Boolean;
 end;

function FetchGraphicsPipeline(cmd:TvDependenciesObject;P:PvGraphicsPipelineKey):TvGraphicsPipeline2;
function FetchComputePipeline (cmd:TvDependenciesObject;P:PvComputePipelineKey ):TvComputePipeline2;

implementation

//

uses
 kern_rwlock;

type
 TvGraphicsPipelineKey2Compare=object
  function c(a,b:PvGraphicsPipelineKey):Integer; static;
 end;

 TvComputePipelineKey2Compare=object
  function c(a,b:PvComputePipelineKey):Integer; static;
 end;

 TvGraphicsPipeline2Set=specialize T23treeSet<PvGraphicsPipelineKey,TvGraphicsPipelineKey2Compare>;

 TvComputePipeline2Set =specialize T23treeSet<PvComputePipelineKey,TvComputePipelineKey2Compare>;

var
 global_lock_rt:Pointer=nil;
 global_lock_cs:Pointer=nil;

 FGlobalCache_rt:TvPipelineCache=nil;
 FGlobalCache_cs:TvPipelineCache=nil;

 FGraphicsPipeline2Set:TvGraphicsPipeline2Set;
 FComputePipeline2Set :TvComputePipeline2Set;

//





function TvGraphicsPipelineKey2Compare.c(a,b:PvGraphicsPipelineKey):Integer;
begin
 Result:=CompareByte(a^,b^,SizeOf(TvGraphicsPipelineKey));
end;

function TvComputePipelineKey2Compare.c(a,b:PvComputePipelineKey):Integer;
begin
 Result:=Integer(Pointer(a^)>Pointer(b^))-Integer(Pointer(a^)<Pointer(b^));
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

 colorBlending.logicOp:=ord(VK_LOGIC_OP_COPY);

 DepthStencil.sType         :=VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
 DepthStencil.depthCompareOp:=VK_COMPARE_OP_LESS;
end;

Procedure TvGraphicsPipelineKey.SetVertexInput(const FAttrBuilder:TvAttrBuilder);
begin
 vertexInputInfo.vertexBindingDescriptionCount  :=FAttrBuilder.FBindDescsCount;
 vertexInputInfo.vertexAttributeDescriptionCount:=FAttrBuilder.FAttrDescsCount;
 vertexInputInfo.VertexBindingDescriptions      :=FAttrBuilder.FBindDescs;
 vertexInputInfo.VertexAttributeDescriptions    :=FAttrBuilder.FAttrDescs;
end;

Procedure TvGraphicsPipelineKey.SetPrimType(t,s:Integer);
begin
 Case ord(t) of
  ord(VK_PRIMITIVE_TOPOLOGY_POINT_LIST)..ord(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY):
  begin
   inputAssembly.topology:=TVkPrimitiveTopology(t);
   emulate_primtype:=0;
   shader_primtype :=-1;
  end;

  DI_PT_RECTLIST:
   begin
    emulate_primtype:=ord(t);
    if (s<>-1) then
    begin
     inputAssembly.topology:=TVkPrimitiveTopology(s);
     shader_primtype:=s;
    end else
    begin
     inputAssembly.topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP;
     shader_primtype :=-1;
    end;
   end;
  DI_PT_LINELOOP ,
  DI_PT_QUADLIST ,
  DI_PT_QUADSTRIP,
  DI_PT_POLYGON  :
  begin
   inputAssembly.topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN;
   emulate_primtype:=ord(t);
   shader_primtype :=-1;
  end;

 end;
end;

Procedure TvGraphicsPipelineKey.SetPrimReset(enable:TVkBool32);
begin
 inputAssembly.primitiveRestartEnable:=enable;
end;

Procedure TvGraphicsPipelineKey.SetProvoking(t:TVkProvokingVertexModeEXT);
begin
 provokingVertex:=ord(t);
end;

Procedure TvGraphicsPipelineKey.AddVPort(const V:TVkViewport;const S:TVkRect2D);
begin
 if (s.extent.width=0) or (s.extent.height=0) then Assert(false);

 if (viewportCount>15) then Exit;

 Viewports[viewportCount]:=V;
 Scissors [viewportCount]:=S;

 Inc(viewportCount);
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
 colorBlending.logicOp:=ord(logicOp);

 if (P=nil) then Exit;
 colorBlending.blendConstants:=PVec4f(P)^;
end;

///

Function TvGraphicsPipeline2.Compile:Boolean;
type
 AVkPipelineShaderStageCreateInfo=array[0..6] of TVkPipelineShaderStageCreateInfo;

var
 FLayout:TvPipelineLayout;
 FCache :TVkPipelineCache;

 r:TVkResult;
 Stages:AVkPipelineShaderStageCreateInfo; // info.stageCount
 info  :TVkGraphicsPipelineCreateInfo;

 vertexInputInfo:TVkPipelineVertexInputStateCreateInfo;
 viewportState  :TVkPipelineViewportStateCreateInfo;
 colorBlending  :TVkPipelineColorBlendStateCreateInfo;
 dynamicState   :TVkPipelineDynamicStateCreateInfo;
 rasterizer     :TVkPipelineRasterizationStateCreateInfo;
 ProvokingVertex:TVkPipelineRasterizationProvokingVertexStateCreateInfoEXT;

 dynamicStates  :array[0..0] of TVkDynamicState; //dynamicState.dynamicStateCount
begin
 Result:=False;

 if (FHandle<>VK_NULL_HANDLE) then Exit(True);

 if (Key.FRenderPass =nil) then Exit;
 if (Key.FShaderGroup=nil) then Exit;
 if (Key.viewportCount=0)  then Exit;

 if (Key.FRenderPass.FHandle=VK_NULL_HANDLE) then Exit;

 FLayout:=Key.FShaderGroup.FLayout;

 if (FLayout=nil) then Exit;
 if (FLayout.FHandle=VK_NULL_HANDLE) then Exit;

 info:=Default(TVkGraphicsPipelineCreateInfo);

 Stages:=Default(AVkPipelineShaderStageCreateInfo);
 Key.FShaderGroup.FKey.ExportStages(@Stages,@info.stageCount);

 if (info.stageCount=0) then Exit;

 //

 vertexInputInfo:=Default(TVkPipelineVertexInputStateCreateInfo);
 vertexInputInfo.sType                          :=VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
 vertexInputInfo.vertexBindingDescriptionCount  :=Key.vertexInputInfo.vertexBindingDescriptionCount;
 vertexInputInfo.vertexAttributeDescriptionCount:=Key.vertexInputInfo.vertexAttributeDescriptionCount;
 vertexInputInfo.pVertexBindingDescriptions     :=@Key.vertexInputInfo.VertexBindingDescriptions[0];
 vertexInputInfo.pVertexAttributeDescriptions   :=@Key.vertexInputInfo.VertexAttributeDescriptions[0];

 viewportState:=Default(TVkPipelineViewportStateCreateInfo);
 viewportState.sType        :=VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
 viewportState.viewportCount:=Key.viewportCount;
 viewportState.pViewports   :=@Key.Viewports;
 viewportState.scissorCount :=Key.viewportCount;
 viewportState.pScissors    :=@Key.Scissors;

 colorBlending:=Default(TVkPipelineColorBlendStateCreateInfo);
 colorBlending.sType          :=VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
 colorBlending.logicOpEnable  :=ord(Key.colorBlending.logicOp<>ord(VK_LOGIC_OP_COPY));
 colorBlending.logicOp        :=TVkLogicOp(Key.colorBlending.logicOp);
 colorBlending.attachmentCount:=Key.colorBlending.attachmentCount;
 colorBlending.pAttachments   :=@Key.ColorBlends;

 dynamicState:=Default(TVkPipelineDynamicStateCreateInfo);
 dynamicState.sType:=VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;

 if limits.VK_EXT_vertex_input_dynamic_state then
 begin
  dynamicStates[0]:=VK_DYNAMIC_STATE_VERTEX_INPUT_EXT;
  //
  dynamicState.dynamicStateCount:=1;
  dynamicState.pDynamicStates   :=@dynamicStates[0];
 end;

 rasterizer:=Key.rasterizer;

 if limits.VK_EXT_provoking_vertex then
 begin
  rasterizer.pNext:=@ProvokingVertex;
  //
  ProvokingVertex:=Default(TVkPipelineRasterizationProvokingVertexStateCreateInfoEXT);
  ProvokingVertex.sType              :=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_PROVOKING_VERTEX_STATE_CREATE_INFO_EXT;
  ProvokingVertex.provokingVertexMode:=TVkProvokingVertexModeEXT(Key.provokingVertex);
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
 info.pDynamicState      :=@dynamicState;
 info.layout             :=FLayout.FHandle;
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

///

function TvComputePipeline2.Compile:Boolean;
var
 FLayout:TvPipelineLayout;
 FCache :TVkPipelineCache;
 FShader:TvShaderExt;

 info:TVkComputePipelineCreateInfo;
 r:TVkResult;
begin
 Result:=False;

 if (FHandle<>VK_NULL_HANDLE) then Exit(True);

 if (Key.FShaderGroup=nil) then Exit;

 FLayout:=Key.FShaderGroup.FLayout;

 if (FLayout=nil) then Exit;
 if (FLayout.FHandle=VK_NULL_HANDLE) then Exit;

 FShader:=Key.FShaderGroup.FKey.FShaders[vShaderStageCs];

 if (FShader=nil) then Exit;
 if (FShader.FHandle=VK_NULL_HANDLE) then Exit;

 info:=Default(TVkComputePipelineCreateInfo);
 info.sType       :=VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO;
 info.stage.sType :=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
 info.stage.stage :=VK_SHADER_STAGE_COMPUTE_BIT;
 info.stage.module:=FShader.FHandle;
 info.stage.pName :=PChar(FShader.FEntry);
 info.layout      :=FLayout.FHandle;

 FCache:=VK_NULL_HANDLE;
 if (FPCache<>nil) then
 begin
  FCache:=FPCache.FHandle;
 end;

 r:=vkCreateComputePipelines(Device.FHandle,FCache,1,@info,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateComputePipelines:',r);
  Exit;
 end;
 Result:=True;
end;

///

function _FindGraphics(P:PvGraphicsPipelineKey):TvGraphicsPipeline2;
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

 t:=_FindGraphics(P);

 if (t=nil) then
 begin

  if (FGlobalCache_rt=nil) then
  begin
   FGlobalCache_rt:=TvPipelineCache.Create(nil,0);
  end;

  t:=TvGraphicsPipeline2.Create;
  t.FPCache:=FGlobalCache_rt;

  t.key:=P^;

  if not t.Compile then
  begin
   FreeAndNil(t);
  end else
  begin
   t.Acquire(nil); //map ref
   FGraphicsPipeline2Set.Insert(@t.key);
  end;
 end;

 Result:=t;
end;

function FetchGraphicsPipeline(cmd:TvDependenciesObject;P:PvGraphicsPipelineKey):TvGraphicsPipeline2;
begin
 Result:=nil;
 if (P=nil) then Exit;

 rw_wlock(global_lock_rt);

 Result:=_FetchGraphicsPipeline(P);

 cmd.RefTo(Result);

 rw_wunlock(global_lock_rt);
end;

//

function _FindCompute(P:PvComputePipelineKey):TvComputePipeline2;
var
 i:TvComputePipeline2Set.Iterator;
begin
 Result:=nil;
 i:=FComputePipeline2Set.find(P);
 if (i.Item<>nil) then
 begin
  Result:=TvComputePipeline2(ptruint(i.Item^)-ptruint(@TvComputePipeline2(nil).key));
 end;
end;

function _FetchComputePipeline(P:PvComputePipelineKey):TvComputePipeline2;
var
 t:TvComputePipeline2;
begin
 Result:=nil;

 t:=_FindCompute(P);

 if (t=nil) then
 begin

  if (FGlobalCache_cs=nil) then
  begin
   FGlobalCache_cs:=TvPipelineCache.Create(nil,0);
  end;

  t:=TvComputePipeline2.Create;
  t.FPCache:=FGlobalCache_cs;

  t.key:=P^;

  if not t.Compile then
  begin
   FreeAndNil(t);
  end else
  begin
   t.Acquire(nil); //map ref
   FComputePipeline2Set.Insert(@t.key);
  end;
 end;

 Result:=t;
end;

function FetchComputePipeline(cmd:TvDependenciesObject;P:PvComputePipelineKey):TvComputePipeline2;
begin
 Result:=nil;
 if (P=nil) then Exit;

 rw_wlock(global_lock_cs);

 Result:=_FetchComputePipeline(P);

 cmd.RefTo(Result);

 rw_wunlock(global_lock_cs);
end;

end.

