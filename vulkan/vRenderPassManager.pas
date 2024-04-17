unit vRenderPassManager;

{$mode ObjFPC}{$H+}

interface

uses
 SysUtils,
 g23tree,
 Vulkan,
 vDevice,
 vDependence,
 vPipeline,
 vImage{,
 vCmdBuffer};

type
 PvRenderPassKey=^TvRenderPassKey;
 TvRenderPassKey=packed object
  AtdCount:Byte;
  RefCount:Byte; //VkSubpassDescription.colorAttachmentCount
  DepCount:Byte; //DepthRef
  _align:Byte;
  ColorAtd:array[0..8] of TVkAttachmentDescription;
  ColorRef:array[0..7] of TVkAttachmentReference;
  DepthRef:TVkAttachmentReference;
  Dependency:TVkSubpassDependency;
  Procedure  Clear;
  Procedure  SetZorderStage(s:TVkPipelineStageFlags);
  Procedure  AddColorRef(id:TVkUInt32;IMAGE_USAGE:Byte);
  Procedure  SetDepthRef(id:TVkUInt32;DEPTH_USAGE,STENCIL_USAGE:Byte);
  Procedure  AddColorAt(format:TVkFormat;IMAGE_USAGE:Byte;samples:Byte);
  Procedure  AddDepthAt(format:TVkFormat;DEPTH_USAGE,STENCIL_USAGE:Byte);
 end;

 TvRenderPass2=class(TvRenderPass)
  Key:TvRenderPassKey;
  //
  FRefs:ptruint;
  Function  Compile:Boolean;
  Procedure Acquire;
  procedure Release(Sender:TObject);
 end;

function FetchRenderPass(cmd:TvDependenciesObject;P:PvRenderPassKey):TvRenderPass2;

implementation

uses
 kern_rwlock;

type
 TvRenderPassKey2Compare=object
  function c(a,b:PvRenderPassKey):Integer; static;
 end;

 _TvRenderPass2Set=specialize T23treeSet<PvRenderPassKey,TvRenderPassKey2Compare>;
 TvRenderPass2Set=object(_TvRenderPass2Set)
  lock:Pointer;
  Procedure Lock_wr;
  Procedure Unlock_wr;
 end;

var
 FRenderPass2Set:TvRenderPass2Set;

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

Procedure TvRenderPassKey.Clear;
begin
 Self:=Default(TvRenderPassKey);
 Dependency.srcSubpass:=VK_SUBPASS_EXTERNAL;
end;

Procedure TvRenderPassKey.SetZorderStage(s:TVkPipelineStageFlags);
begin
 Dependency.srcStageMask:=Dependency.srcStageMask or s;
 Dependency.dstStageMask:=Dependency.dstStageMask or s;
end;

Procedure TvRenderPassKey.AddColorRef(id:TVkUInt32;IMAGE_USAGE:Byte);
var
 am:TVkAccessFlags;
begin
 if (RefCount>7) then Exit;
 ColorRef[RefCount].attachment:=id;
 ColorRef[RefCount].layout    :=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL{VK_IMAGE_LAYOUT_GENERAL};
 Inc(RefCount);

 Dependency.srcStageMask :=Dependency.srcStageMask or ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT);
 Dependency.dstStageMask :=Dependency.dstStageMask or ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT);

 am:=GetColorAccessMask(IMAGE_USAGE);

 Dependency.srcAccessMask:=Dependency.srcAccessMask or am;
 Dependency.dstAccessMask:=Dependency.dstAccessMask or am;
end;

Procedure TvRenderPassKey.SetDepthRef(id:TVkUInt32;DEPTH_USAGE,STENCIL_USAGE:Byte);
var
 am:TVkAccessFlags;
begin
 DepCount:=1;

 DepthRef.attachment :=id;
 DepthRef.layout     :=GetDepthStencilLayout(DEPTH_USAGE,STENCIL_USAGE);

 am:=GetDepthStencilAccessMask(DEPTH_USAGE,STENCIL_USAGE);

 Dependency.srcAccessMask:=Dependency.srcAccessMask or am;
 Dependency.dstAccessMask:=Dependency.dstAccessMask or am;
end;

Procedure TvRenderPassKey.AddColorAt(format:TVkFormat;IMAGE_USAGE:Byte;samples:Byte);
begin
 if (AtdCount>8) then Exit;

 ColorAtd[AtdCount]:=Default(TVkAttachmentDescription);
 ColorAtd[AtdCount].format        :=format;
 ColorAtd[AtdCount].samples       :=TVkSampleCountFlagBits(samples);

 With ColorAtd[AtdCount] do
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

 With ColorAtd[AtdCount] do
  if (IMAGE_USAGE and TM_WRITE<>0) then
  begin
   storeOp:=VK_ATTACHMENT_STORE_OP_STORE;
  end else
  begin
   storeOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;
  end;

 ColorAtd[AtdCount].stencilLoadOp :=VK_ATTACHMENT_LOAD_OP_DONT_CARE;
 ColorAtd[AtdCount].stencilStoreOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;

 With ColorAtd[AtdCount] do
  if (IMAGE_USAGE and TM_READ<>0) then
  begin
   initialLayout :=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
  end else
  begin
   initialLayout :=VK_IMAGE_LAYOUT_UNDEFINED;
  end;

 With ColorAtd[AtdCount] do
  finalLayout:=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;

 Inc(AtdCount);
end;

Procedure TvRenderPassKey.AddDepthAt(format:TVkFormat;DEPTH_USAGE,STENCIL_USAGE:Byte);
begin
 if (AtdCount>8) then Exit;

 ColorAtd[AtdCount]:=Default(TVkAttachmentDescription);
 ColorAtd[AtdCount].format        :=format;
 ColorAtd[AtdCount].samples       :=VK_SAMPLE_COUNT_1_BIT;

 With ColorAtd[AtdCount] do
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

 With ColorAtd[AtdCount] do
  if (DEPTH_USAGE and TM_WRITE<>0) then
  begin
   storeOp:=VK_ATTACHMENT_STORE_OP_STORE;
  end else
  begin
   storeOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;
  end;

 With ColorAtd[AtdCount] do
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

 With ColorAtd[AtdCount] do
  if (STENCIL_USAGE and TM_WRITE<>0) then
  begin
   stencilStoreOp:=VK_ATTACHMENT_STORE_OP_STORE;
  end else
  begin
   stencilStoreOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;
  end;

 With ColorAtd[AtdCount] do
  if ((DEPTH_USAGE or STENCIL_USAGE) and TM_READ<>0) then
  begin
   initialLayout :=GetDepthStencilLayout(DEPTH_USAGE,STENCIL_USAGE);
  end else
  begin
   initialLayout :=VK_IMAGE_LAYOUT_UNDEFINED;
  end;

 With ColorAtd[AtdCount] do
  finalLayout:=GetDepthStencilLayout(DEPTH_USAGE,STENCIL_USAGE);

 Inc(AtdCount);
end;

///

Function TvRenderPass2.Compile:Boolean;
var
 r:TVkResult;
 subpass:TVkSubpassDescription;
 info:TVkRenderPassCreateInfo;
begin
 Result:=False;
 if (Key.AtdCount=0) then Exit;

 if (FHandle<>VK_NULL_HANDLE) then Exit(True);

 subpass:=Default(TVkSubpassDescription);
 subpass.pipelineBindPoint:=VK_PIPELINE_BIND_POINT_GRAPHICS;

 subpass.inputAttachmentCount   :=0;
 subpass.pInputAttachments      :=nil;

 subpass.colorAttachmentCount   :=Key.RefCount;
 subpass.pColorAttachments      :=@Key.ColorRef;

 subpass.pResolveAttachments    :=nil; //colorAttachmentCount VK_ATTACHMENT_UNUSED

 if (Key.DepCount<>0) then
 begin
  subpass.pDepthStencilAttachment:=@Key.DepthRef; //1
 end;

 subpass.preserveAttachmentCount:=0;
 subpass.pPreserveAttachments   :=nil;

 info:=Default(TVkRenderPassCreateInfo);
 info.sType          :=VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
 info.attachmentCount:=Key.AtdCount;
 info.pAttachments   :=@Key.ColorAtd;
 info.subpassCount   :=1;
 info.pSubpasses     :=@subpass;
 info.dependencyCount:=1;
 info.pDependencies  :=@Key.Dependency;

 r:=vkCreateRenderPass(Device.FHandle,@info,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateRenderPass:',r);
  Exit;
 end;

 Result:=True;
end;

Procedure TvRenderPass2Set.Lock_wr;
begin
 rw_wlock(lock);
end;

Procedure TvRenderPass2Set.Unlock_wr;
begin
 rw_wunlock(lock);
end;

Procedure TvRenderPass2.Acquire;
begin
 System.InterlockedIncrement(Pointer(FRefs));
end;

procedure TvRenderPass2.Release(Sender:TObject);
begin
 if System.InterlockedDecrement(Pointer(FRefs))=nil then
 begin
  Free;
 end;
end;

function TvRenderPassKey2Compare.c(a,b:PvRenderPassKey):Integer;
begin
 Result:=CompareByte(a^,b^,SizeOf(TvRenderPassKey));
end;

function _Find(P:PvRenderPassKey):TvRenderPass2;
var
 i:TvRenderPass2Set.Iterator;
begin
 Result:=nil;
 i:=FRenderPass2Set.find(P);
 if (i.Item<>nil) then
 begin
  Result:=TvRenderPass2(ptruint(i.Item^)-ptruint(@TvRenderPass2(nil).key));
 end;
end;

function _FetchRenderPass(P:PvRenderPassKey):TvRenderPass2;
var
 t:TvRenderPass2;
begin
 Result:=nil;

 t:=_Find(P);

 if (t=nil) then
 begin

  t:=TvRenderPass2.Create;
  t.key:=P^;

  if not t.Compile then
  begin
   FreeAndNil(t);
  end else
  begin
   t.Acquire;
   FRenderPass2Set.Insert(@t.key);
  end;
 end;

 Result:=t;
end;

function FetchRenderPass(cmd:TvDependenciesObject;P:PvRenderPassKey):TvRenderPass2;
begin
 Result:=nil;
 if (P=nil) then Exit;

 FRenderPass2Set.Lock_wr;

 Result:=_FetchRenderPass(P);

 if (cmd<>nil) and (Result<>nil) then
 begin
  if cmd.AddDependence(@TvRenderPass2(Result).Release) then
  begin
   TvRenderPass2(Result).Acquire;
  end;
 end;

 FRenderPass2Set.Unlock_wr;
end;



end.

