unit vFlip;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ps4_libSceVideoOut,
  vulkan,
  vDevice,
  vMemory,
  vShader,
  vPipeline,
  vImage,
  vRender;

type
 TFlipCfg=packed object
  gamma:array[0..3] of Single;
  Width:array[0..1] of Integer;
 end;

 Tgamma=array[0..2] of Single;

 TvFlip=class;

 PvFlipBufferCursor=^TvFlipBufferCursor;
 TvFlipBufferCursor=object
  FSet:TvDescriptorSet;
  PtrBuf  :TvPointer;
  HostBuf :TvBuffer;
  pending:PDWORD;
  procedure SetBuffer(hbuf:TvPointer);
  Procedure Init(Flip:TvFlip);
  Procedure Free;
  function  IsPrepare:Boolean;
  Procedure SetPending(f:PDWORD);
 end;

 PvFlipBuffer=^TvFlipBuffer;
 TvFlipBuffer=object

  FSet:TvDescriptorSet;

  cmdfence:TvFence; //
  cmdbuf:TVkCommandBuffer; //

  Extent:TVkExtent3D;

  PITCH,SIZE:DWORD;

  DstImgNORM:TvImage; //
  DstImgSRGB:TvImage; //

  DevcMem:TvPointer; //
  ImgViewDst:TvImageView; //

  PtrBuf:TvPointer; //
  HostBuf:TvBuffer; //

  Cursors:array[0..SCE_VIDEO_OUT_CURSOR_NUM_MAX-1] of TvFlipBufferCursor;

  ur:TUnionResourceImage;

  Procedure Init(Flip:TvFlip);
  Procedure Free(Flip:TvFlip);
  function  IsPrepare:Boolean;
 end;

 TVCursorPos=packed record
  X,Y:DWORD;
 end;

 TvFlipCursor=object
  enable:boolean;
  pos:TVCursorPos;
  PtrBuf:TvPointer;
  pending:PDWORD;
  Procedure Free;
 end;

 TvFlip=class
  FSurface:TVSurface;
  FSwapChain:TSwapChain;
  FSetLayout:TvSetLayout;
  FLayout:TvPipelineLayout;
  FPipelineFlip:TvComputePipeline;
  FPipelineCursor:TvComputePipeline;

  FSetsPool:TvSetsPool;

  FCmdPool:TCmdPool;
  Fcfg:TFlipCfg;

  Fformat,Ftmode:DWORD;
  Ffilp_shader:TvShader;

  Fcursor_shader:TvShader;

  FimageAvailableSemaphore:array[0..2] of TvSemaphore; //
  FrenderFinishedSemaphore:array[0..2] of TvSemaphore; //

  FNeoMode:Boolean;

  FcurrentFrame:Byte;
  FcurrentBuffer:Byte;
  FBuffers:array[0..15] of TvFlipBuffer;

  FCursors:array[0..SCE_VIDEO_OUT_CURSOR_NUM_MAX-1] of TvFlipCursor;

  Constructor Create(Handle:THandle);
  Destructor  Destroy; override;

  Procedure   SetCurrentBuffer(Buffer:Byte);

  Procedure   SetImageFormat(format,tmode:DWORD);
  Procedure   SetImageSize(width,height:DWORD);
  Procedure   SetHostBuffer(Addr:Pointer);
  Procedure   SetGamma(gamma:Tgamma);

  Procedure   SetCursor(index:Integer;enable:Boolean;addr:Pointer;pending:PDWORD;pos:TVCursorPos);

  procedure   recreateSwapChain;
  Procedure   FixCurrentFrame;
  Procedure   Flip;
 end;

implementation

Procedure TvFlipCursor.Free;
begin
 enable:=False;
 pos:=Default(TVCursorPos);
 pending:=nil;
 PtrBuf:=Default(TvPointer);
end;

Constructor TvFlip.Create(Handle:THandle);
var
 i:Byte;
 //P1,P2,P3:TvPointer;
begin
 InitVulkan;

 Fcfg.gamma[0]:=1;
 Fcfg.gamma[1]:=1;
 Fcfg.gamma[2]:=1;
 Fcfg.gamma[3]:=1;
 Fcfg.Width[0]:=1;
 Fcfg.Width[1]:=1;

 FSurface:=TVSurface.Create(Handle);

 FSetLayout:=TvSetLayout.Create;
 FSetLayout.Add(0,VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,ord(VK_SHADER_STAGE_COMPUTE_BIT));
 FSetLayout.Add(1,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE ,ord(VK_SHADER_STAGE_COMPUTE_BIT));

 FLayout:=TvPipelineLayout.Create;
 FLayout.Add(FSetLayout);
 FLayout.AddPushConst(0,SizeOf(TFlipCfg),ord(VK_SHADER_STAGE_COMPUTE_BIT));

 FPipelineFlip:=TvComputePipeline.Create;
 FPipelineFlip.SetLayout(FLayout);

 FSetsPool:=TvSetsPool.Create;
 FSetsPool.AddLayout(FSetLayout,16+16*2);

 FCmdPool:=TCmdPool.Create(VulkanApp.FGFamily);

 For i:=0 to 2 do
 begin
  FimageAvailableSemaphore[i]:=TvSemaphore.Create;
  FrenderFinishedSemaphore[i]:=TvSemaphore.Create;
 end;

end;

Destructor TvFlip.Destroy;
var
 i:Byte;
begin
 vkQueueWaitIdle(FlipQueue);
 FreeAndNil(Ffilp_shader);

 For i:=0 to 15 do
 begin
  FBuffers[i].Free(Self);
 end;

 For i:=0 to 2 do
 begin
  FreeAndNil(FimageAvailableSemaphore[i]);
  FreeAndNil(FrenderFinishedSemaphore[i]);
 end;

 FreeAndNil(FCmdPool);
 FreeAndNil(FSetsPool);
 FreeAndNil(FPipelineFlip);
 FreeAndNil(FLayout);
 FreeAndNil(FSetLayout);
 FreeAndNil(FSwapChain);
 FreeAndNil(FSurface);
 inherited;
end;

Procedure TvFlipBuffer.Init(Flip:TvFlip);
begin
 if (FSet=nil) then
 begin
  FSet:=Flip.FSetsPool.Alloc(Flip.FSetLayout);
 end;
 if (cmdbuf=VK_NULL_HANDLE) then
 begin
  cmdbuf:=Flip.FCmdPool.Alloc;
 end;
 if (cmdfence=nil) then
 begin
  cmdfence:=TvFence.Create(true);
 end;
end;

Procedure TvFlipBuffer.Free(Flip:TvFlip);
begin
 FreeAndNil(ImgViewDst);
 FreeAndNil(DstImgNORM);
 FreeAndNil(DstImgSRGB);
 FreeAndNil(HostBuf);
 FreeAndNil(cmdfence);
 if (cmdbuf<>VK_NULL_HANDLE) then
 begin
  Flip.FCmdPool.Free(cmdbuf);
  cmdbuf:=VK_NULL_HANDLE;
 end;
 MemManager.Free(DevcMem);
 DevcMem:=Default(TvPointer);
 Cursors[0].Free;
 Cursors[1].Free;
end;

function TvFlipBuffer.IsPrepare:Boolean;
begin
 Result:=False;
 if (FSet=nil) then Exit;
 if (cmdfence=nil) then Exit;
 if (cmdbuf=VK_NULL_HANDLE) then Exit;
 if (DstImgNORM=nil) then Exit;
 if (ImgViewDst=nil) then Exit;
 if (DevcMem.FHandle=VK_NULL_HANDLE) then Exit;
 if (Extent.width=0) or (Extent.height=0) then Exit;
 if (HostBuf=nil) then Exit;
 Result:=True;
end;

procedure TvFlipBufferCursor.SetBuffer(hbuf:TvPointer);
const
 buf_ext:TVkExternalMemoryBufferCreateInfo=(
  sType:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO;
  pNext:nil;
  handleTypes:ord(VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT);
 );
begin
 if (PtrBuf.FHandle=hbuf.FHandle) and (PtrBuf.FOffset=hbuf.FOffset) and (HostBuf<>nil) then Exit;

 PtrBuf:=hbuf;
 FreeAndNil(HostBuf);

 HostBuf:=TvBuffer.Create(16384,
                          ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                          @buf_ext);

 HostBuf.BindMem(hbuf);
end;

Procedure TvFlipBufferCursor.Init(Flip:TvFlip);
begin
 if (FSet=nil) then
 begin
  FSet:=Flip.FSetsPool.Alloc(Flip.FSetLayout);
 end;
end;

Procedure TvFlipBufferCursor.Free;
begin
 FreeAndNil(FSet);
 FreeAndNil(HostBuf);
 PtrBuf:=Default(TvPointer);
 pending:=nil;
end;

function TvFlipBufferCursor.IsPrepare:Boolean;
begin
 Result:=False;
 if (FSet=nil) then Exit;
 if (HostBuf=nil) then Exit;
 Result:=True;
end;

Procedure TvFlipBufferCursor.SetPending(f:PDWORD);
begin
 if (pending<>f) then
 begin
  if (pending<>nil) then System.InterlockedDecrement(pending^);
  pending:=f;
  if (pending<>nil) then System.InterlockedIncrement(pending^);
 end;
end;

Procedure TvFlip.SetCurrentBuffer(Buffer:Byte);
var
 buf:PvFlipBuffer;
begin
 if (Buffer>15) then Buffer:=15;
 FcurrentBuffer:=Buffer;
 buf:=@FBuffers[FcurrentBuffer];
 buf^.Init(Self);
end;

Procedure TvFlip.SetImageFormat(format,tmode:DWORD);
begin
 if (Fformat=format) and (Ftmode=tmode) and (Ffilp_shader<>nil) then Exit;

 Case tmode of
  SCE_VIDEO_OUT_TILING_MODE_LINEAR:
    Case format of
     SCE_VIDEO_OUT_PIXEL_FORMAT_A8R8G8B8_SRGB:
       begin
        Fformat:=format;
        Ftmode:=tmode;
        vkQueueWaitIdle(FlipQueue);
        FreeAndNil(Ffilp_shader);
        Ffilp_shader:=TvShader.Create;
        Ffilp_shader.LoadFromFile('shaders\FLIP_LINE_A8R8G8B8_SRGB.spv');
        FPipelineFlip.SetShader(Ffilp_shader);
       end;
     else
      Assert(false);
    end;
  SCE_VIDEO_OUT_TILING_MODE_TILE:
    Case format of
     SCE_VIDEO_OUT_PIXEL_FORMAT_A8R8G8B8_SRGB:
       begin
        Fformat:=format;
        Ftmode:=tmode;
        vkQueueWaitIdle(FlipQueue);
        FreeAndNil(Ffilp_shader);
        Ffilp_shader:=TvShader.Create;

        if FNeoMode then
        begin
         Ffilp_shader.LoadFromFile('shaders\FLIP_TILE_A8R8G8B8_SRGB_NEO.spv');
        end else
        begin
         Ffilp_shader.LoadFromFile('shaders\FLIP_TILE_A8R8G8B8_SRGB.spv');
        end;

        FPipelineFlip.SetShader(Ffilp_shader);
       end;
     else
      Assert(false);
    end;
  else
   Assert(false);
 end;

end;

Const
 SRGB_HACK=True;

Procedure TvFlip.SetImageSize(width,height:DWORD);
var
 buf:PvFlipBuffer;
 //mt:TVkUInt32;
 memr:TVkMemoryRequirements;
 cformat:TVkFormat;
 sformat:TVkFormat;
begin
 buf:=@FBuffers[FcurrentBuffer];

 if (buf^.Extent.width=width) and (buf^.Extent.height=height) and (buf^.DstImgNORM<>nil) then Exit;
 buf^.Extent.width :=width;
 buf^.Extent.height:=height;
 buf^.Extent.depth :=1;

 cformat:=VK_FORMAT_UNDEFINED;
 sformat:=VK_FORMAT_UNDEFINED;

 Case Ftmode of
  SCE_VIDEO_OUT_TILING_MODE_LINEAR:
    Case Fformat of
     SCE_VIDEO_OUT_PIXEL_FORMAT_A8R8G8B8_SRGB,
     SCE_VIDEO_OUT_PIXEL_FORMAT_A8B8G8R8_SRGB:
       begin
        buf^.PITCH:=width;
        buf^.SIZE :=width*height*4;
        cformat:=VK_FORMAT_R8G8B8A8_UNORM;
        if SRGB_HACK then
         sformat:=VK_FORMAT_R8G8B8A8_SRGB;
       end;
    end;
  SCE_VIDEO_OUT_TILING_MODE_TILE:
    Case Fformat of
     SCE_VIDEO_OUT_PIXEL_FORMAT_A8R8G8B8_SRGB,
     SCE_VIDEO_OUT_PIXEL_FORMAT_A8B8G8R8_SRGB:
       begin
        if FNeoMode then
        begin
         buf^.PITCH:=(width+127) div 128;
         buf^.SIZE :=buf^.PITCH*128*((height+127) div 128)*128*4;
        end else
        begin
         buf^.PITCH:=(width+127) div 128;
         buf^.SIZE :=buf^.PITCH*128*((height+63) div 64)*64*4;
        end;
        cformat:=VK_FORMAT_R8G8B8A8_UNORM;
        if SRGB_HACK then
         sformat:=VK_FORMAT_R8G8B8A8_SRGB;
       end;
    end;
 end;

 FreeAndNil(buf^.ImgViewDst);
 FreeAndNil(buf^.DstImgNORM);
 FreeAndNil(buf^.DstImgSRGB);

 MemManager.Free(buf^.DevcMem);
 buf^.DevcMem:=Default(TvPointer);

 //

 buf^.DstImgNORM:=TvDeviceImage2D.Create(
  cformat,
  buf^.Extent,
  ord(VK_IMAGE_USAGE_STORAGE_BIT) or
  ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
  ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT)
 );

 if (sformat<>VK_FORMAT_UNDEFINED) then
  buf^.DstImgSRGB:=TvDeviceImage2D.Create(
   sformat,
   buf^.Extent,
   ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
   ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT)
  );

 memr:=buf^.DstImgNORM.GetRequirements;

 //Writeln(buf^.DstImg.GetDedicatedAllocation);

 buf^.DevcMem:=MemManager.Alloc(memr,ord(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT));

 buf^.DstImgNORM.BindMem(buf^.DevcMem);

 if (sformat<>VK_FORMAT_UNDEFINED) then
  buf^.DstImgSRGB.BindMem(buf^.DevcMem);

 buf^.ImgViewDst:=buf^.DstImgNORM.NewView;
end;

Procedure TvFlip.SetHostBuffer(Addr:Pointer);
const
 buf_ext:TVkExternalMemoryBufferCreateInfo=(
  sType:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO;
  pNext:nil;
  handleTypes:ord(VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT);
 );
var
 r:TUnionResourceImage;
 hbuf:TvPointer;
 buf:PvFlipBuffer;
begin
 buf:=@FBuffers[FcurrentBuffer];

 r:=FindUnionImage2D(Addr);
 if (r<>nil) then
 begin
  FreeAndNil(buf^.HostBuf);
  buf^.ur:=r;
 end else
 begin
  buf^.ur:=nil;
 end;

 hbuf:=Default(TvPointer);
 if not TryGetHostPointerByAddr(addr,hbuf) then Exit;

 if (buf^.PtrBuf.FHandle=hbuf.FHandle) and (buf^.PtrBuf.FOffset=hbuf.FOffset) and (buf^.HostBuf<>nil) then Exit;
 buf^.PtrBuf:=hbuf;
 FreeAndNil(buf^.HostBuf);

 buf^.HostBuf:=TvBuffer.Create(buf^.SIZE,
                           ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                           @buf_ext);
 buf^.HostBuf.BindMem(hbuf);
end;

Procedure TvFlip.SetGamma(gamma:Tgamma);
begin
 Fcfg.gamma[0]:=gamma[0];
 Fcfg.gamma[1]:=gamma[1];
 Fcfg.gamma[2]:=gamma[2];
end;

Procedure TvFlip.SetCursor(index:Integer;enable:Boolean;addr:Pointer;pending:PDWORD;pos:TVCursorPos);
var
 i:Byte;
 hbuf:TvPointer;
begin
 If (index<0) or (index>=Length(FCursors)) then Exit;
 Case enable of
  True:
    if TryGetHostPointerByAddr(addr,hbuf) then
    begin
     if (FPipelineCursor=nil) then
     begin
      FPipelineCursor:=TvComputePipeline.Create;
      FPipelineCursor.SetLayout(FLayout);
     end;

     if (Fcursor_shader=nil) then
     begin
      Fcursor_shader:=TvShader.Create;
      Fcursor_shader.LoadFromFile('shaders\FLIP_CURSOR.spv');
      FPipelineCursor.SetShader(Fcursor_shader);
     end;

     FCursors[index].enable :=true;
     FCursors[index].pos    :=pos;
     FCursors[index].pending:=pending;
     FCursors[index].PtrBuf :=hbuf;
    end else
    begin
     FCursors[index].Free;
    end;
  False:FCursors[index].Free;
 end;
end;

procedure TvFlip.recreateSwapChain;
begin
 vkDeviceWaitIdle(Device.FHandle);
 FreeAndNil(FSwapChain);
 FSwapChain:=TSwapChain.Create(FSurface,1,ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT));
end;

Procedure TvFlip.FixCurrentFrame;
begin
 if (FSwapChain=nil) then Exit;
 if (Length(FSwapChain.FImage)=0) then Exit;
 FcurrentFrame:=FcurrentFrame mod Length(FSwapChain.FImage);
end;

Procedure TvFlip.Flip;
var
 r:TVkResult;
 imageIndex:TVkUInt32;
 SwapImage:TVkImage;

 imageAvailableSemaphore:TVkSemaphore;
 renderFinishedSemaphore:TVkSemaphore;

 buf:PvFlipBuffer;

 bufcur:PvFlipBufferCursor;

 FCurSet:TvDescriptorSet;

 FBuffer:TVkDescriptorBufferInfo;
 beginInfo:TVkCommandBufferBeginInfo;
 imgBlitRegion:TVkImageBlit;
 FLocalSize:TVkOffset3D;

 submitInfo:TVkSubmitInfo;
 wstage:TVkPipelineStageFlags;
 prInfo:TVkPresentInfoKHR;

 tmp_reg:TVkBufferImageCopy;
 tmp_buf:TUnionResourceBuffer;

 img_reg:TVkImageCopy;

begin
 if (Device=nil) then Exit;

 buf:=@FBuffers[FcurrentBuffer];

 if (Ffilp_shader=nil) then Exit;

 if not buf^.IsPrepare then Exit;

 if (FSwapChain=nil) then recreateSwapChain;
 if (FSwapChain.FHandle=VK_NULL_HANDLE) then
 begin
  recreateSwapChain;
  if (FSwapChain.FHandle=VK_NULL_HANDLE) then Exit;
 end;

 if not FSetLayout.Compile then Exit;
 if not FLayout.Compile    then Exit;
 if not FPipelineFlip.Compile  then Exit;
 if not FSetsPool.Compile  then Exit;

 repeat
  FixCurrentFrame;
  imageAvailableSemaphore:=FimageAvailableSemaphore[FcurrentFrame].FHandle;
  renderFinishedSemaphore:=FrenderFinishedSemaphore[FcurrentFrame].FHandle;

  R:=vkAcquireNextImageKHR(Device.FHandle,
                           FSwapChain.FHandle,
                           High(uInt64),
                           imageAvailableSemaphore,
                           VK_NULL_HANDLE,
                           @imageIndex);
  Case R of
   VK_SUCCESS:Break;
   VK_ERROR_OUT_OF_DATE_KHR,
   VK_SUBOPTIMAL_KHR:recreateSwapChain;
   else
    begin
     Writeln('vkAcquireNextImageKHR:',R);
     Exit;
    end;
  end;
 until false;
 SwapImage:=FSwapChain.FImage[imageIndex];

 buf^.cmdfence.Wait(High(uint64));
 buf^.cmdfence.Reset;

 buf^.Cursors[0].SetPending(nil);
 buf^.Cursors[1].SetPending(nil);


 beginInfo:=Default(TVkCommandBufferBeginInfo);
 beginInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
 beginInfo.flags:=ord(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT);
 beginInfo.pInheritanceInfo:=nil;
 r:=vkBeginCommandBuffer(buf^.cmdbuf,@beginInfo);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkBeginCommandBuffer:',r);
  Exit;
 end;

 if (buf^.ur=nil) then
 begin
  FBuffer.buffer:=buf^.HostBuf.FHandle;
  FBuffer.offset:=0;
  FBuffer.range :=buf^.SIZE;

  FCurSet:=buf^.FSet;

  FCurSet.BindSB (0,0,FBuffer);
  FCurSet.BindSTI(1,0,buf^.ImgViewDst.FHandle);

  vkImageMemoryBarrier(
        buf^.cmdbuf,
  	buf^.DstImgNORM.FHandle,
  	ord(VK_ACCESS_NONE_KHR),
  	ord(VK_ACCESS_SHADER_WRITE_BIT),
  	VK_IMAGE_LAYOUT_UNDEFINED,
  	VK_IMAGE_LAYOUT_GENERAL,
  	ord(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
  	ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
  	SubresColor);

  if (buf^.DstImgSRGB<>nil) then
  vkImageMemoryBarrier(
        buf^.cmdbuf,
  	buf^.DstImgSRGB.FHandle,
  	ord(VK_ACCESS_NONE_KHR),
  	ord(VK_ACCESS_SHADER_WRITE_BIT),
  	VK_IMAGE_LAYOUT_UNDEFINED,
  	VK_IMAGE_LAYOUT_GENERAL,
  	ord(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
  	ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
  	SubresColor);

  vkCmdBindPipeline(buf^.cmdbuf,VK_PIPELINE_BIND_POINT_COMPUTE,FPipelineFlip.FHandle);

  Fcfg.Width[0]:=1;
  Fcfg.Width[1]:=buf^.PITCH;

  vkCmdPushConstants(buf^.cmdbuf,
                     FLayout.FHandle,
                     ord(VK_SHADER_STAGE_COMPUTE_BIT),
                     0,
                     SizeOf(TFlipCfg),
                     @Fcfg);

  vkCmdBindDescriptorSets(buf^.cmdbuf,VK_PIPELINE_BIND_POINT_COMPUTE,
                          FLayout.FHandle,0,1,
                          @FCurSet.FHandle,0,nil);

  FLocalSize:=FPipelineFlip.FComputeShader.FLocalSize;
  FLocalSize.x:=(buf^.DstImgNORM.FExtent.width +(FLocalSize.x-1)) div FLocalSize.x;
  FLocalSize.y:=(buf^.DstImgNORM.FExtent.height+(FLocalSize.y-1)) div FLocalSize.y;

  vkCmdDispatch(buf^.cmdbuf,FLocalSize.x,FLocalSize.y,1);
 end else
 begin

  vkImageMemoryBarrier(
        buf^.cmdbuf,
        buf^.ur.FImage.FHandle,
  	ord(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
  	ord(VK_ACCESS_TRANSFER_READ_BIT),
  	VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
  	VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
  	ord(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
  	ord(VK_PIPELINE_STAGE_TRANSFER_BIT),
  	SubresColor);

  vkImageMemoryBarrier(
        buf^.cmdbuf,
  	buf^.DstImgNORM.FHandle,
  	ord(VK_ACCESS_NONE_KHR),
  	ord(VK_ACCESS_TRANSFER_WRITE_BIT),
  	VK_IMAGE_LAYOUT_UNDEFINED,
  	VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
  	ord(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
  	ord(VK_PIPELINE_STAGE_TRANSFER_BIT),
  	SubresColor);

  if (buf^.DstImgSRGB<>nil) then
  vkImageMemoryBarrier(
        buf^.cmdbuf,
  	buf^.DstImgSRGB.FHandle,
  	ord(VK_ACCESS_NONE_KHR),
  	ord(VK_ACCESS_TRANSFER_WRITE_BIT),
  	VK_IMAGE_LAYOUT_UNDEFINED,
  	VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
  	ord(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
  	ord(VK_PIPELINE_STAGE_TRANSFER_BIT),
  	SubresColor);


  img_reg:=Default(TVkImageCopy);
  img_reg.srcSubresource.aspectMask:=ord(VK_IMAGE_ASPECT_COLOR_BIT);
  //img_reg.srcSubresource.mipLevel:TVkUInt32;
  //img_reg.srcSubresource.baseArrayLayer:TVkUInt32;
  img_reg.srcSubresource.layerCount:=1;

  img_reg.dstSubresource.aspectMask:=ord(VK_IMAGE_ASPECT_COLOR_BIT);
  //img_reg.dstSubresource.mipLevel:TVkUInt32;
  //img_reg.dstSubresource.baseArrayLayer:TVkUInt32;
  img_reg.dstSubresource.layerCount:=1;

  //img_reg.srcOffset:TVkOffset3D;
  //img_reg.dstOffset:TVkOffset3D;
  img_reg.extent:=buf^.ur.FImage.FExtent;

  if (buf^.DstImgSRGB<>nil) then
  begin

   vkCmdCopyImage(buf^.cmdbuf,
    buf^.ur.FImage.FHandle,
    VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
    buf^.DstImgSRGB.FHandle,
    VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
    1,@img_reg);

  end else
  begin

   vkCmdCopyImage(buf^.cmdbuf,
    buf^.ur.FImage.FHandle,
    VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
    buf^.DstImgNORM.FHandle,
    VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
    1,@img_reg);

  end;

 end;

 if (FCursors[0].enable or FCursors[1].enable) then
 begin
  vkCmdBindPipeline(buf^.cmdbuf,VK_PIPELINE_BIND_POINT_COMPUTE,FPipelineCursor.FHandle);

  FLocalSize:=FPipelineCursor.FComputeShader.FLocalSize;

  bufcur:=@buf^.Cursors[0];

  if FCursors[0].enable then
  begin
   bufcur^.Init(Self);
   bufcur^.SetBuffer(FCursors[0].PtrBuf);

   if bufcur^.IsPrepare then
   begin
    FCurSet:=bufcur^.FSet;

    Fcfg.Width[0]:=FCursors[0].pos.x;
    Fcfg.Width[1]:=FCursors[0].pos.y;

    bufcur^.SetPending(FCursors[0].pending);

    FBuffer.buffer:=bufcur^.HostBuf.FHandle;
    FBuffer.offset:=0;
    FBuffer.range :=16384;

    FCurSet.BindSB (0,0,FBuffer);
    FCurSet.BindSTI(1,0,buf^.ImgViewDst.FHandle);

    vkImageMemoryBarrier(
        buf^.cmdbuf,
    	buf^.DstImgNORM.FHandle,
    	ord(VK_ACCESS_NONE_KHR),
    	ord(VK_ACCESS_SHADER_WRITE_BIT),
    	VK_IMAGE_LAYOUT_UNDEFINED,
    	VK_IMAGE_LAYOUT_GENERAL,
    	ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
    	ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
    	SubresColor);

    if (buf^.DstImgSRGB<>nil) then
    vkImageMemoryBarrier(
        buf^.cmdbuf,
    	buf^.DstImgSRGB.FHandle,
    	ord(VK_ACCESS_NONE_KHR),
    	ord(VK_ACCESS_SHADER_WRITE_BIT),
    	VK_IMAGE_LAYOUT_UNDEFINED,
    	VK_IMAGE_LAYOUT_GENERAL,
    	ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
    	ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
    	SubresColor);

    vkCmdPushConstants(buf^.cmdbuf,
                       FLayout.FHandle,
                       ord(VK_SHADER_STAGE_COMPUTE_BIT),
                       0,
                       SizeOf(TFlipCfg),
                       @Fcfg);

    vkCmdBindDescriptorSets(buf^.cmdbuf,VK_PIPELINE_BIND_POINT_COMPUTE,
                            FLayout.FHandle,0,1,
                            @FCurSet.FHandle,0,nil);

    vkCmdDispatch(buf^.cmdbuf,64 div FLocalSize.x,64 div FLocalSize.y,1);
   end else
   begin
    bufcur^.Free;
   end;
  end else
  begin
   bufcur^.Free;
  end;

  bufcur:=@buf^.Cursors[1];

  if FCursors[1].enable then
  begin
   bufcur^.Init(Self);
   bufcur^.SetBuffer(FCursors[1].PtrBuf);

   if bufcur^.IsPrepare then
   begin
    FCurSet:=bufcur^.FSet;

    Fcfg.Width[0]:=FCursors[1].pos.x;
    Fcfg.Width[1]:=FCursors[1].pos.y;

    bufcur^.SetPending(FCursors[1].pending);

    FBuffer.buffer:=bufcur^.HostBuf.FHandle;
    FBuffer.offset:=0;
    FBuffer.range :=16384;

    FCurSet.BindSB (0,0,FBuffer);
    FCurSet.BindSTI(1,0,buf^.ImgViewDst.FHandle);

    vkImageMemoryBarrier(
        buf^.cmdbuf,
    	buf^.DstImgNORM.FHandle,
    	ord(VK_ACCESS_NONE_KHR),
    	ord(VK_ACCESS_SHADER_WRITE_BIT),
    	VK_IMAGE_LAYOUT_UNDEFINED,
    	VK_IMAGE_LAYOUT_GENERAL,
    	ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
    	ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
    	SubresColor);

    if (buf^.DstImgSRGB<>nil) then
    vkImageMemoryBarrier(
        buf^.cmdbuf,
    	buf^.DstImgSRGB.FHandle,
    	ord(VK_ACCESS_NONE_KHR),
    	ord(VK_ACCESS_SHADER_WRITE_BIT),
    	VK_IMAGE_LAYOUT_UNDEFINED,
    	VK_IMAGE_LAYOUT_GENERAL,
    	ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
    	ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
    	SubresColor);

    vkCmdPushConstants(buf^.cmdbuf,
                       FLayout.FHandle,
                       ord(VK_SHADER_STAGE_COMPUTE_BIT),
                       0,
                       SizeOf(TFlipCfg),
                       @Fcfg);

    vkCmdBindDescriptorSets(buf^.cmdbuf,VK_PIPELINE_BIND_POINT_COMPUTE,
                            FLayout.FHandle,0,1,
                            @FCurSet.FHandle,0,nil);

    vkCmdDispatch(buf^.cmdbuf,64 div FLocalSize.x,64 div FLocalSize.y,1);
   end else
   begin
    bufcur^.Free;
   end;
  end else
  begin
   bufcur^.Free;
  end;

 end else
 begin
  buf^.Cursors[0].Free;
  buf^.Cursors[1].Free;
 end;

 vkImageMemoryBarrier(
 	buf^.cmdbuf,
 	buf^.DstImgNORM.FHandle,
        ord(VK_ACCESS_NONE_KHR),
 	ord(VK_ACCESS_TRANSFER_READ_BIT),
        VK_IMAGE_LAYOUT_UNDEFINED,
 	VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
 	ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
 	ord(VK_PIPELINE_STAGE_TRANSFER_BIT),
 	SubresColor);

 if (buf^.DstImgSRGB<>nil) then
 vkImageMemoryBarrier(
 	buf^.cmdbuf,
 	buf^.DstImgSRGB.FHandle,
        ord(VK_ACCESS_NONE_KHR),
 	ord(VK_ACCESS_TRANSFER_READ_BIT),
        VK_IMAGE_LAYOUT_UNDEFINED,
 	VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
 	ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
 	ord(VK_PIPELINE_STAGE_TRANSFER_BIT),
 	SubresColor);

 vkImageMemoryBarrier(
 	buf^.cmdbuf,
 	SwapImage,
 	ord(VK_ACCESS_NONE_KHR),
 	ord(VK_ACCESS_TRANSFER_WRITE_BIT),
 	VK_IMAGE_LAYOUT_UNDEFINED,
 	VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
 	ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
 	ord(VK_PIPELINE_STAGE_TRANSFER_BIT),
 	SubresColor);

 imgBlitRegion:=Default(TVkImageBlit);
 imgBlitRegion.srcSubresource.aspectMask:=ord(VK_IMAGE_ASPECT_COLOR_BIT);
 imgBlitRegion.srcSubresource.layerCount:=1;
 imgBlitRegion.dstSubresource.aspectMask:=ord(VK_IMAGE_ASPECT_COLOR_BIT);
 imgBlitRegion.dstSubresource.layerCount:=1;
 imgBlitRegion.srcOffsets[1].x:=buf^.DstImgNORM.FExtent.width;
 imgBlitRegion.srcOffsets[1].y:=buf^.DstImgNORM.FExtent.height;
 imgBlitRegion.srcOffsets[1].z:=1;
 imgBlitRegion.dstOffsets[1].x:=FSwapChain.FSize.width;
 imgBlitRegion.dstOffsets[1].y:=FSwapChain.FSize.height;
 imgBlitRegion.dstOffsets[1].z:=1;

 if (buf^.DstImgSRGB<>nil) then
 begin
  vkCmdBlitImage(
         buf^.cmdbuf,
         buf^.DstImgSRGB.FHandle, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
         SwapImage,       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
         1,@imgBlitRegion,VK_FILTER_LINEAR);
 end else
 begin
  vkCmdBlitImage(
         buf^.cmdbuf,
         buf^.DstImgNORM.FHandle, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
         SwapImage,       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
         1,@imgBlitRegion,VK_FILTER_LINEAR);
 end;



 vkImageMemoryBarrier(
 	buf^.cmdbuf,
 	SwapImage,
 	ord(VK_ACCESS_TRANSFER_WRITE_BIT),
        ord(VK_ACCESS_NONE_KHR),
 	VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
        VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
 	ord(VK_PIPELINE_STAGE_TRANSFER_BIT),
        ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
 	SubresColor);

 vkEndCommandBuffer(buf^.cmdbuf);

 wstage:=ord(VK_PIPELINE_STAGE_TRANSFER_BIT);

 submitInfo:=Default(TVkSubmitInfo);
 submitInfo.sType               :=VK_STRUCTURE_TYPE_SUBMIT_INFO;
 submitInfo.waitSemaphoreCount  :=1;
 submitInfo.pWaitSemaphores     :=@imageAvailableSemaphore;
 submitInfo.pWaitDstStageMask   :=@wstage;
 submitInfo.commandBufferCount  :=1;
 submitInfo.pCommandBuffers     :=@buf^.cmdbuf;
 submitInfo.signalSemaphoreCount:=1;
 submitInfo.pSignalSemaphores   :=@renderFinishedSemaphore;

 r:=vkQueueSubmit(FlipQueue,1,@submitInfo,buf^.cmdfence.FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkQueueSubmit:',r);
  exit;
 end;

 prInfo:=Default(TVkPresentInfoKHR);
 prInfo.sType             :=VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
 prInfo.waitSemaphoreCount:=1;
 prInfo.pWaitSemaphores   :=@renderFinishedSemaphore;
 prInfo.swapchainCount    :=1;
 prInfo.pSwapchains       :=@FSwapChain.FHandle;
 prInfo.pImageIndices     :=@imageIndex;
 prInfo.pResults:=nil;

 R:=vkQueuePresentKHR(FlipQueue,@prInfo);
 Case R of
  VK_SUCCESS:;
  VK_ERROR_OUT_OF_DATE_KHR,
  VK_SUBOPTIMAL_KHR:recreateSwapChain;
   else
    begin
     Writeln('vkQueuePresentKHR:',R);
     Exit;
    end;
 end;

 FcurrentFrame:=FcurrentFrame+1;
 FixCurrentFrame;
 //vkQueueWaitIdle(FlipQueue);
end;


end.

