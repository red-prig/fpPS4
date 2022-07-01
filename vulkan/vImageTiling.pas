unit vImageTiling;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 RWLock,
 g23tree,
 ps4_tiling,
 Vulkan,
 vDevice,
 vMemory,
 vBuffer,
 vImage,
 vHostBufferManager,
 vCmdBuffer;

Procedure LoadFromBuffer(cmd:TvCustomCmdBuffer;image:TObject); //TvImage2

implementation

uses
 vImageManager;

Function getFormatSize(cformat:TVkFormat):Byte;
begin
 Result:=0;
 Case cformat of
  VK_FORMAT_R8G8B8A8_SRGB      :Result:=4;
  VK_FORMAT_R8G8B8A8_UNORM     :Result:=4;
  VK_FORMAT_R8G8_UNORM         :Result:=2;
  VK_FORMAT_R8_UNORM           :Result:=1;
  VK_FORMAT_R8_UINT            :Result:=4;
  VK_FORMAT_R5G6B5_UNORM_PACK16:Result:=2;
  else
   Assert(false,'TODO');
 end;
end;

{
Procedure _Load_Linear(cmd:TvCustomCmdBuffer;image:TvImage2);
var
 buf:TvHostBuffer;
 BufferImageCopy:TVkBufferImageCopy;
 size:Ptruint;
begin
 size:=image.key.params.extend.width*
       image.key.params.extend.height*
       image.key.params.extend.depth*
       getFormatSize(image.key.cformat);

 image.PushBarrier(cmd,
                   ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                   VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                   ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 buf:=FetchHostBuffer(cmd,
                      image.key.Addr,
                      size,
                      ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT));

 vkBufferMemoryBarrier(cmd.cmdbuf,
                       buf.FHandle,
                       ord(VK_ACCESS_SHADER_WRITE_BIT),
                       ord(VK_ACCESS_MEMORY_READ_BIT),
                       buf.Foffset,size,
                       ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                       ord(VK_PIPELINE_STAGE_TRANSFER_BIT)
                       );

 BufferImageCopy:=Default(TVkBufferImageCopy);

 BufferImageCopy.bufferOffset:=buf.Foffset;
 BufferImageCopy.bufferRowLength:=0;
 BufferImageCopy.bufferImageHeight:=0;
 BufferImageCopy.imageSubresource:=image.GetSubresLayer;
 BufferImageCopy.imageExtent.Create(image.key.params.extend.width,
                                    image.key.params.extend.height,
                                    image.key.params.extend.depth);

  Case image.key.cformat of
   VK_FORMAT_D16_UNORM_S8_UINT,
   VK_FORMAT_D24_UNORM_S8_UINT,
   VK_FORMAT_D32_SFLOAT_S8_UINT:
    BufferImageCopy.imageSubresource.aspectMask:=ord(VK_IMAGE_ASPECT_DEPTH_BIT);
   else;
  end;

 //image.data_usage:=image.data_usage and (not TM_READ);

 vkCmdCopyBufferToImage(cmd.cmdbuf,
                        buf.FHandle,
                        image.FHandle,
                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                        1,
                        @BufferImageCopy);
end;
}

Procedure _Load_Linear(cmd:TvCustomCmdBuffer;image:TvImage2);
var
 buf:TvHostImage2;
 ImageCopy:TVkImageCopy;
begin

 if (image.key.params.samples>ord(VK_SAMPLE_COUNT_1_BIT)) then Exit;

 buf:=image.FetchHostImage(cmd,ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
                               ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT));

 Assert(buf<>nil,'FetchHostImage');

 image.PushBarrier(cmd,
                   ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                   VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                   ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 buf.PushBarrier(cmd,
                 ord(VK_ACCESS_TRANSFER_READ_BIT),
                 VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                 ord(VK_PIPELINE_STAGE_TRANSFER_BIT));


 ImageCopy:=Default(TVkImageCopy);
 ImageCopy.srcSubresource:=image.GetSubresLayer;
 ImageCopy.dstSubresource:=image.GetSubresLayer;
 ImageCopy.extent.Create(image.key.params.extend.width,
                         image.key.params.extend.height,
                         image.key.params.extend.depth);

 Case image.key.cformat of
  VK_FORMAT_D16_UNORM_S8_UINT,
  VK_FORMAT_D24_UNORM_S8_UINT,
  VK_FORMAT_D32_SFLOAT_S8_UINT:
   begin
    ImageCopy.srcSubresource.aspectMask:=ord(VK_IMAGE_ASPECT_DEPTH_BIT);
    ImageCopy.dstSubresource.aspectMask:=ord(VK_IMAGE_ASPECT_DEPTH_BIT);
   end
  else;
 end;

 vkCmdCopyImage(cmd.cmdbuf,
                buf.FHandle,
                VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                image.FHandle,
                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                1,@ImageCopy);

end;

type
 TvTempBuffer=class(TvBuffer)
  Fhost:TvPointer;
  procedure Release(Sender:TObject);
 end;

procedure TvTempBuffer.Release(Sender:TObject);
begin
 MemManager.Free(Fhost);
 Free;
end;

Procedure _Copy_Linear(cmd:TvCustomCmdBuffer;buf:TvTempBuffer;image:TvImage2);
var
 BufferImageCopy:TVkBufferImageCopy;
 size:Ptruint;
begin

 cmd.AddDependence(@buf.Release);

 size:=image.key.params.extend.width*
       image.key.params.extend.height*
       image.key.params.extend.depth*
       getFormatSize(image.key.cformat);

 image.PushBarrier(cmd,
                   ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                   VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                   ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 vkBufferMemoryBarrier(cmd.cmdbuf,
                       buf.FHandle,
                       ord(VK_ACCESS_SHADER_WRITE_BIT),
                       ord(VK_ACCESS_MEMORY_READ_BIT),
                       0,size,
                       ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                       ord(VK_PIPELINE_STAGE_TRANSFER_BIT)
                       );

 BufferImageCopy:=Default(TVkBufferImageCopy);

 BufferImageCopy.bufferOffset:=0;
 BufferImageCopy.bufferRowLength:=0;
 BufferImageCopy.bufferImageHeight:=0;
 BufferImageCopy.imageSubresource:=image.GetSubresLayer;
 BufferImageCopy.imageExtent.Create(image.key.params.extend.width,
                                    image.key.params.extend.height,
                                    image.key.params.extend.depth);

  Case image.key.cformat of
   VK_FORMAT_D16_UNORM_S8_UINT,
   VK_FORMAT_D24_UNORM_S8_UINT,
   VK_FORMAT_D32_SFLOAT_S8_UINT:
    BufferImageCopy.imageSubresource.aspectMask:=ord(VK_IMAGE_ASPECT_DEPTH_BIT);
   else;
  end;

 vkCmdCopyBufferToImage(cmd.cmdbuf,
                        buf.FHandle,
                        image.FHandle,
                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                        1,
                        @BufferImageCopy);
end;

type
 TTGAHeader=packed record
  idlength       :Byte;
  colourmaptype  :Byte;
  datatypecode   :Byte;
  colourmaporigin:Word;
  colourmaplength:Word;
  colourmapdepth :Byte;
  x_origin       :Word;
  y_origin       :Word;
  width          :Word;
  height         :Word;
  bitsperpixel   :Byte;
  imagedescriptor:Byte;
 end;

Procedure _Load_Thin_1dThin(cmd:TvCustomCmdBuffer;image:TvImage2);
var
 buf:TvTempBuffer;

 //tp:TilingParameters;
 tiler:Tiler1d;
 //mtm:Byte;
 size,i,x,y:QWORD;

 m_bitsPerElement:Word;

 //m_macroTileWidth :DWORD;
 //m_macroTileHeight:DWORD;

 pData,pSrc,pDst:Pointer;

 F:THandle;
 Header:TTGAHeader;

begin
 //tp:=Default(TilingParameters);

 //mtm:=$FF; //2 kMacroTileMode_1x1_16
 //bankWidth=1 bankHeight=1 macroTileAspect=2 numBanks=16 altBankHeight=2 altNumBanks= 8 altMacroTileAspect=1
 //computeSurfaceMacroTileMode(@mtm,13,32,1);

 //tp.m_tileMode:=image.key.params.tiling_idx;
 //tp.m_minGpuMode:=0; //PS4 NEO
 //
 //tp.m_linearWidth         :=image.key.params.extend.width;
 //tp.m_linearHeight        :=image.key.params.extend.height;
 //tp.m_linearDepth         :=image.key.params.extend.depth;
 //tp.m_numFragmentsPerPixel:=32;
 //tp.m_baseTiledPitch      :=0;
 //
 //tp.m_mipLevel            :=0;
 //tp.m_arraySlice          :=0;
 ////tp.m_surfaceFlags        :SurfaceFlags;
 //tp.m_bitsPerFragment     :=32;
 //tp.m_isBlockCompressed   :=False;
 //tp.m_tileSwizzleMask     :=0;
 //
 //tiler:=Default(Tiler2d);
 //tiler.init(tp);

 Case image.key.cformat of
  VK_FORMAT_BC1_RGB_UNORM_BLOCK,
  VK_FORMAT_BC1_RGB_SRGB_BLOCK,
  VK_FORMAT_BC1_RGBA_UNORM_BLOCK,
  VK_FORMAT_BC1_RGBA_SRGB_BLOCK,
  VK_FORMAT_BC3_UNORM_BLOCK,
  VK_FORMAT_BC3_SRGB_BLOCK:
   begin
    _Load_Linear(cmd,image);
    Exit;
   end;
  else
 end;

 tiler:=Texture2d_32;

 m_bitsPerElement:=getFormatSize(image.key.cformat)*8;

 tiler.m_bitsPerElement:=m_bitsPerElement;

 tiler.m_linearWidth :=image.key.params.extend.width;
 tiler.m_linearHeight:=image.key.params.extend.height;
 tiler.m_linearDepth :=image.key.params.extend.depth;

 tiler.m_linearSizeBytes:=tiler.m_linearWidth*tiler.m_linearHeight*tiler.m_linearDepth*(m_bitsPerElement div 8);

 tiler.m_tileBytes := (kMicroTileWidth * kMicroTileHeight * tiler.m_tileThickness * m_bitsPerElement + 7) div 8;

 Case m_bitsPerElement of
  32:begin
      tiler.m_paddedWidth :=((tiler.m_linearWidth +7) div 8)*8;
      tiler.m_paddedHeight:=((tiler.m_linearHeight+7) div 8)*8;
     end;
   8:begin
      tiler.m_paddedWidth :=((tiler.m_linearWidth +31) div 32)*32;
      tiler.m_paddedHeight:=((tiler.m_linearHeight+7) div 8)*8;
     end;
  else
   Assert(false);
 end;

 tiler.m_paddedDepth :=tiler.m_linearDepth;

 tiler.m_tiledSizeBytes:=tiler.m_paddedWidth*tiler.m_paddedHeight*tiler.m_paddedDepth*(m_bitsPerElement div 8);

 tiler.m_tilesPerRow:=tiler.m_paddedWidth div kMicroTileWidth;

 tiler.m_tilesPerSlice:= tiler.m_tilesPerRow * (tiler.m_paddedHeight div kMicroTileHeight);


 ///buf^.PITCH:=(width+127) div 128;
 ///buf^.SIZE :=buf^.PITCH*128*((height+63) div 64)*64*4;

 //m_tilesPerRow = m_paddedWidth / kMicroTileWidth;
 //m_tilesPerSlice = std::max(m_tilesPerRow * (m_paddedHeight / kMicroTileHeight), 1U);

 size:=image.key.params.extend.width*
       image.key.params.extend.height*(m_bitsPerElement div 8);

 buf:=TvTempBuffer.Create(size,ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT),nil);
 buf.Fhost:=MemManager.Alloc(buf.GetRequirements,ord(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT));
 buf.BindMem(buf.Fhost);

 pData:=nil;
 vkMapMemory(Device.FHandle,
             buf.Fhost.FHandle,
             buf.Fhost.FOffset,
             size,
             0,
             @pData);

 //pData:=AllocMem(size);

 Assert(image.key.params.extend.depth=1);

 For y:=0 to image.key.params.extend.height-1 do
  For x:=0 to image.key.params.extend.width-1 do
   begin
    i:=0;
    tiler.getTiledElementBitOffset(i,x,y,0);
    i:=i div 8;
    pSrc:=@PByte(image.key.Addr)[i];
    pDst:=@PByte(pData)[(y*image.key.params.extend.width+x)*(m_bitsPerElement div 8)];
    Move(pSrc^,pDst^,(m_bitsPerElement div 8));
    //i:=i div 4;
    //pData[y*image.key.params.extend.width+x]:={Random($FFFFFFFF);}PDWORD(image.key.Addr)[i];
   end;

 //Move(pData^,image.key.Addr^,size);
 //FreeMem(pData);

  Case m_bitsPerElement of
   8:begin
      //image.data_usage:=image.data_usage and (not TM_READ);

      //Header:=Default(TTGAHeader);
      //
      //Header.datatypecode   :=3;
      //
      //Header.width          :=image.key.params.extend.width;
      //Header.height         :=image.key.params.extend.height;
      //Header.bitsperpixel   :=8;
      //Header.imagedescriptor:=32;
      //
      //F:=FileCreate('texture.tga');
      //FileWrite(F,Header,SizeOf(TTGAHeader));
      //FileWrite(F,pData^,size);
      //FileClose(F);
     end;
 end;

 //image.data_usage:=image.data_usage and (not TM_READ);

 vkUnmapMemory(Device.FHandle,buf.Fhost.FHandle);

 _Copy_Linear(cmd,buf,image);

 //_Load_Linear(cmd,image);
 //writeln;
end;

Procedure LoadFromBuffer(cmd:TvCustomCmdBuffer;image:TObject);
begin
 if (cmd=nil) then Exit;

 Case TvImage2(image).key.params.tiling_idx of
  kTileModeDisplay_LinearAligned,
  kTileModeDisplay_LinearGeneral:
   _Load_Linear(cmd,TvImage2(image));

  kTileModeDisplay_2dThin: //render target tiling todo
   _Load_Linear(cmd,TvImage2(image));

  kTileModeDepth_2dThin_64: //depth tiling todo
   _Load_Linear(cmd,TvImage2(image));

  kTileModeThin_1dThin: //texture
   _Load_Thin_1dThin(cmd,TvImage2(image));

  kTileModeThin_2dThin:
   _Load_Linear(cmd,TvImage2(image)); //TODO

  else
   Assert(false,'TODO');
 end;

end;

end.

