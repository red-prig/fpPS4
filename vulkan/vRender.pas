unit vRender;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  //si_ci_vi_merged_enum,
  vRegs2Vulkan,
  Vulkan,
  vDevice,
  vMemory,
  vShader,
  vShaderExt,
  vPipeline,
  vPipelineManager,

  vImage,
  vDependence;

type
 AvClearValues=array[0..8] of TVkClearValue;

 PvRenderPassBeginInfo=^TvRenderPassBeginInfo;
 TvRenderPassBeginInfo=object
  FRenderPass :TvRenderPass;
  FRenderArea :TVkRect2D;
  FClearCount :TVkUInt32;
  FClearValues:AvClearValues;
  FFramebuffer:TvFramebuffer;
  //
  FImagesCount:TVkUInt32;
  FImageViews :AvImageViews;
  //
  Procedure  SetRenderPass(RP:TvRenderPass);
  Procedure  SetRenderArea(RA:TVkRect2D);
  Procedure  SetFramebuffer(FB:TvFramebuffer);
  //
  Procedure  AddClearColor(clr:TVkClearValue);
  Procedure  AddClearColor(clr:TVkClearColorValue);
  //
  Procedure  AddImageView(v:TvImageView);
  //
  Function   GetRInfo:TVkRenderPassBeginInfo;
  Function   GetAInfo:TVkRenderPassAttachmentBeginInfo;
 end;

implementation

/////////

Procedure TvRenderPassBeginInfo.SetRenderPass(RP:TvRenderPass);
begin
 FRenderPass:=RP;
end;

Procedure TvRenderPassBeginInfo.SetRenderArea(RA:TVkRect2D);
begin
 FRenderArea:=RA;
end;

Procedure TvRenderPassBeginInfo.SetFramebuffer(FB:TvFramebuffer);
begin
 FFramebuffer:=FB;
end;

Procedure TvRenderPassBeginInfo.AddClearColor(clr:TVkClearValue);
begin
 if (FClearCount>8) then Exit;
 FClearValues[FClearCount]:=clr;
 Inc(FClearCount);
end;

Procedure TvRenderPassBeginInfo.AddClearColor(clr:TVkClearColorValue);
begin
 AddClearColor(TVkClearValue(clr));
end;

//

Procedure TvRenderPassBeginInfo.AddImageView(v:TvImageView);
begin
 Assert(v<>nil,'AddImageView');
 if (FImagesCount>=Length(AvImageViews)) then Exit;
 FImageViews[FImagesCount]:=v.FHandle;
 Inc(FImagesCount);
end;

Function TvRenderPassBeginInfo.GetRInfo:TVkRenderPassBeginInfo;
begin
 Result:=Default(TVkRenderPassBeginInfo);
 Result.sType          :=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
 Result.renderPass     :=FRenderPass.FHandle;
 Result.renderArea     :=FRenderArea;
 Result.clearValueCount:=FClearCount;
 Result.pClearValues   :=@FClearValues[0];
 Result.framebuffer    :=FFramebuffer.FHandle;
end;

Function TvRenderPassBeginInfo.GetAInfo:TVkRenderPassAttachmentBeginInfo;
begin
 Result:=Default(TVkRenderPassAttachmentBeginInfo);
 Result.sType          :=VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO;
 Result.attachmentCount:=FImagesCount;
 Result.pAttachments   :=@FImageViews[0];
end;

//////////////



end.

