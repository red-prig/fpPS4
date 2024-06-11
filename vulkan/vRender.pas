unit vRender;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  g23tree,
  //ps4_libSceVideoOut,
  si_ci_vi_merged_enum,
  vRegs2Vulkan,
  Vulkan,
  vDevice,
  vMemory,
  vShader,
  vShaderExt,
  vPipeline,
  vPipelineManager,
  //vSetsPools,
  vImage,
  vDependence;

type
 TvRenderTargets=class(TvRefsObject)
  RT_COUNT :Byte;
  DB_ENABLE:Boolean;

  RT_INFO:array[0..7] of TRT_INFO;
  DB_INFO:TDB_INFO;
  //
  FRenderPass :TvRenderPass;
  FPipeline   :TvGraphicsPipeline2;
  FFramebuffer:TvFramebuffer;
  FRenderArea :TVkRect2D;
  //
  FClearValuesCount:TVkUInt32;
  FClearValues:array[0..8] of TVkClearValue;
  //
  FImagesCount:TVkUInt32;
  FImageViews :AvImageViews;
  //
  Procedure  AddClearColor(clr:TVkClearValue);
  Procedure  AddClearColor(clr:TVkClearColorValue);
  Procedure  AddImageView(v:TvImageView);
  Function   GetRInfo:TVkRenderPassBeginInfo;
  Function   GetAInfo:TVkRenderPassAttachmentBeginInfo;
  class function c(const a,b:TvRenderTargets):Integer;
 end;

 TvRenderTargetsSet=specialize T23treeSet<TvRenderTargets,TvRenderTargets>;

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

/////////

Procedure TvRenderTargets.AddClearColor(clr:TVkClearValue);
begin
 if (FClearValuesCount>8) then Exit;
 FClearValues[FClearValuesCount]:=clr;
 Inc(FClearValuesCount);
end;

Procedure TvRenderTargets.AddClearColor(clr:TVkClearColorValue);
begin
 AddClearColor(TVkClearValue(clr));
end;

Procedure TvRenderTargets.AddImageView(v:TvImageView);
begin
 Assert(v<>nil,'AddImageView');
 if (FImagesCount>=Length(AvImageViews)) then Exit;
 FImageViews[FImagesCount]:=v.FHandle;
 Inc(FImagesCount);
end;

Function TvRenderTargets.GetRInfo:TVkRenderPassBeginInfo;
begin
 Result:=Default(TVkRenderPassBeginInfo);
 Result.sType          :=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
 Result.renderPass     :=FRenderPass.FHandle;
 Result.renderArea     :=FRenderArea;
 Result.clearValueCount:=FClearValuesCount;
 Result.pClearValues   :=@FClearValues[0];
 Result.framebuffer    :=FFramebuffer.FHandle;
end;

Function TvRenderTargets.GetAInfo:TVkRenderPassAttachmentBeginInfo;
begin
 Result:=Default(TVkRenderPassAttachmentBeginInfo);
 Result.sType          :=VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO;
 Result.attachmentCount:=FImagesCount;
 Result.pAttachments   :=@FImageViews[0];
end;

class function TvRenderTargets.c(const a,b:TvRenderTargets):Integer;
begin
 Result:=CompareByte(a,b,SizeOf(Pointer));
end;

//////////////



end.

