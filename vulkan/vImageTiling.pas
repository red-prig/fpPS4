unit vImageTiling;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 RWLock,
 ps4_shader,
 ps4_tiling,
 Vulkan,
 vDevice,
 vMemory,
 vBuffer,
 vImage,
 vHostBufferManager,
 vCmdBuffer;

Procedure LoadFromBuffer(cmd:TvCustomCmdBuffer;image:TObject); //TvImage2
function  CheckFromBuffer(image:TObject):Boolean; //TvImage2

var
 SKIP_UNKNOW_TILING:Boolean=False;

implementation

uses
 shader_dump,
 vImageManager;

Function GetAlignWidth(format:TVkFormat;width:DWORD):DWORD;
var
 bpp,size:Ptruint;
begin
 size:=width;
 bpp:=getFormatSize(format);
 if IsTexelFormat(format) then
 begin
  size:=(size+3) div 4;
 end;
 size:=size*bpp;
 size:=(size+127) and (not 127);
 size:=size div bpp;
 if IsTexelFormat(format) then
 begin
  size:=size*4;
 end;
 Result:=size;
end;

Function GetLinearSize(image:TvImage2;align:Boolean):Ptruint;
var
 extend:TvExtent3D;
begin
 extend:=image.key.params.extend;

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

Procedure _Load_Linear(cmd:TvCustomCmdBuffer;image:TvImage2);
var
 buf:TvHostBuffer;
 BufferImageCopy:TVkBufferImageCopy;
 size:Ptruint;
begin

 if (image.key.params.samples>ord(VK_SAMPLE_COUNT_1_BIT)) then Exit;

 size:=GetLinearSize(image,(image.key.params.tiling_idx=8));

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

 if (image.key.params.tiling_idx=8) then
 begin
  BufferImageCopy.bufferRowLength:=GetAlignWidth(image.key.cformat,image.key.params.extend.width);
 end;

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

{
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
}

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

 size:=GetLinearSize(image,false);

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
 size,i,x,y,z,a:QWORD;

 m_bytePerElement:Word;
 m_bitsPerElement:Word;

 m_slice_size:DWORD;

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

 //[kDataFormatBc3UnormSrgb]
 //m_minGpuMode:0
 //m_tileMode:13
 //m_arrayMode:2
 //m_linearWidth:128
 //m_linearHeight:128
 //m_linearDepth:1
 //m_paddedWidth:128
 //m_paddedHeight:128
 //m_paddedDepth:1
 //m_bitsPerElement:128
 //m_linearSizeBytes:262144
 //m_tiledSizeBytes:262144
 //m_microTileMode:1
 //m_tileThickness:1
 //m_tileBytes:1024
 //m_tilesPerRow:16
 //m_tilesPerSlice:256

 tiler:=Texture2d_32;

 m_bytePerElement:=getFormatSize(image.key.cformat);
 m_bitsPerElement:=m_bytePerElement*8;

 tiler.m_bitsPerElement:=m_bitsPerElement;

 tiler.m_linearWidth :=image.key.params.extend.width;
 tiler.m_linearHeight:=image.key.params.extend.height;
 tiler.m_linearDepth :=image.key.params.extend.depth;

 if IsTexelFormat(image.key.cformat) then
 begin
  tiler.m_linearWidth :=(tiler.m_linearWidth +3) div 4;
  tiler.m_linearHeight:=(tiler.m_linearHeight+3) div 4;
  tiler.m_linearDepth :=(tiler.m_linearDepth +3) div 4;
 end;

 tiler.m_linearSizeBytes:=tiler.m_linearWidth*tiler.m_linearHeight*tiler.m_linearDepth*m_bytePerElement;

 tiler.m_tileBytes := (kMicroTileWidth * kMicroTileHeight * tiler.m_tileThickness * m_bitsPerElement + 7) div 8;

 if IsTexelFormat(image.key.cformat) then
 begin
  tiler.m_paddedWidth :=tiler.m_linearWidth ;
  tiler.m_paddedHeight:=tiler.m_linearHeight;
  tiler.m_paddedDepth :=tiler.m_linearDepth ;
 end else
 Case m_bitsPerElement of
  64:begin
      tiler.m_paddedWidth :=(tiler.m_linearWidth +3) and (not 3);
      tiler.m_paddedHeight:=(tiler.m_linearHeight+7) and (not 7);
      tiler.m_paddedDepth :=tiler.m_linearDepth;
     end;
  32:begin
      tiler.m_paddedWidth :=(tiler.m_linearWidth +7) and (not 7);
      tiler.m_paddedHeight:=(tiler.m_linearHeight+7) and (not 7);
      tiler.m_paddedDepth :=tiler.m_linearDepth;
     end;
  16:begin
      tiler.m_paddedWidth :=(tiler.m_linearWidth +15) and (not 15);
      tiler.m_paddedHeight:=(tiler.m_linearHeight+ 7) and (not  7);
      tiler.m_paddedDepth :=tiler.m_linearDepth;
     end;
   8:begin
      tiler.m_paddedWidth :=(tiler.m_linearWidth +31) and (not 31);
      tiler.m_paddedHeight:=(tiler.m_linearHeight+ 7) and (not  7);
      tiler.m_paddedDepth :=tiler.m_linearDepth;
     end;
  else
   Assert(false);
 end;



 tiler.m_tiledSizeBytes:=tiler.m_paddedWidth*tiler.m_paddedHeight*tiler.m_paddedDepth*m_bytePerElement;

 tiler.m_tilesPerRow:=tiler.m_paddedWidth div kMicroTileWidth;

 tiler.m_tilesPerSlice:= tiler.m_tilesPerRow * (tiler.m_paddedHeight div kMicroTileHeight);


 ///buf^.PITCH:=(width+127) div 128;
 ///buf^.SIZE :=buf^.PITCH*128*((height+63) div 64)*64*4;

 //m_tilesPerRow = m_paddedWidth / kMicroTileWidth;
 //m_tilesPerSlice = std::max(m_tilesPerRow * (m_paddedHeight / kMicroTileHeight), 1U);

 size:=tiler.m_linearSizeBytes*image.key.params.arrayLayers;

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

 m_slice_size:=(tiler.m_linearWidth*tiler.m_linearHeight);

 For a:=0 to image.key.params.arrayLayers-1 do
  For z:=0 to tiler.m_linearDepth-1 do
   For y:=0 to tiler.m_linearHeight-1 do
    For x:=0 to tiler.m_linearWidth-1 do
     begin
      i:=0;
      tiler.getTiledElementBitOffset(i,x,y,z);
      i:=i div 8;
      pSrc:=@PByte(image.key.Addr)[a*tiler.m_tiledSizeBytes+i];
      pDst:=@PByte(pData)[a*tiler.m_linearSizeBytes+(z*m_slice_size+y*tiler.m_linearWidth+x)*m_bytePerElement];
      Move(pSrc^,pDst^,m_bytePerElement);
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

  kTileModeDepth_2dThin_64 ,
  kTileModeDepth_2dThin_128,
  kTileModeDepth_2dThin_256,
  kTileModeDepth_2dThin_512,
  kTileModeDepth_2dThin_1K : //depth tiling todo
   _Load_Linear(cmd,TvImage2(image));

  kTileModeDepth_1dThin,
  kTileModeDisplay_1dThin,
  kTileModeThin_1dThin,  //texture
  $1B:
   _Load_Thin_1dThin(cmd,TvImage2(image));

  kTileModeThin_2dThin:
   _Load_Linear(cmd,TvImage2(image)); //TODO

  else
   if not SKIP_UNKNOW_TILING then
   Assert(false,'TODO tiling_idx:'+get_tiling_idx_str(TvImage2(image).key.params.tiling_idx));
 end;

end;

//FastHash(data:PByte;len:DWORD):DWORD;

function _Check_Linear(image:TvImage2):Boolean;
var
 size:Ptruint;
 cur:DWORD;
begin
 Result:=False;

 if (image.key.params.samples>ord(VK_SAMPLE_COUNT_1_BIT)) then Exit;

 size:=GetLinearSize(image,(image.key.params.tiling_idx=8));

 cur:=FastHash(image.key.Addr,size);

 if (cur<>image.hash) then
 begin
  image.hash:=cur;
  Result:=True;
 end;

end;

function _Check_Thin_1dThin(image:TvImage2):Boolean;
var
 tiler:Tiler1d;

 size:QWORD;

 m_bytePerElement:Word;
 m_bitsPerElement:Word;

 cur:DWORD;
begin
 Result:=False;

 if (image.key.params.samples>ord(VK_SAMPLE_COUNT_1_BIT)) then Exit;

 tiler:=Texture2d_32;

 m_bytePerElement:=getFormatSize(image.key.cformat);
 m_bitsPerElement:=m_bytePerElement*8;

 tiler.m_bitsPerElement:=m_bitsPerElement;

 tiler.m_linearWidth :=image.key.params.extend.width;
 tiler.m_linearHeight:=image.key.params.extend.height;
 tiler.m_linearDepth :=image.key.params.extend.depth;

 if IsTexelFormat(image.key.cformat) then
 begin
  tiler.m_linearWidth :=(tiler.m_linearWidth +3) div 4;
  tiler.m_linearHeight:=(tiler.m_linearHeight+3) div 4;
  tiler.m_linearDepth :=(tiler.m_linearDepth +3) div 4;
 end;

 if IsTexelFormat(image.key.cformat) then
 begin
  tiler.m_paddedWidth :=tiler.m_linearWidth ;
  tiler.m_paddedHeight:=tiler.m_linearHeight;
  tiler.m_paddedDepth :=tiler.m_linearDepth ;
 end else
 Case m_bitsPerElement of
  64:begin
      tiler.m_paddedWidth :=(tiler.m_linearWidth +3) and (not 3);
      tiler.m_paddedHeight:=(tiler.m_linearHeight+7) and (not 7);
      tiler.m_paddedDepth :=tiler.m_linearDepth;
     end;
  32:begin
      tiler.m_paddedWidth :=(tiler.m_linearWidth +7) and (not 7);
      tiler.m_paddedHeight:=(tiler.m_linearHeight+7) and (not 7);
      tiler.m_paddedDepth :=tiler.m_linearDepth;
     end;
  16:begin
      tiler.m_paddedWidth :=(tiler.m_linearWidth +15) and (not 15);
      tiler.m_paddedHeight:=(tiler.m_linearHeight+ 7) and (not  7);
      tiler.m_paddedDepth :=tiler.m_linearDepth;
     end;
   8:begin
      tiler.m_paddedWidth :=(tiler.m_linearWidth +31) and (not 31);
      tiler.m_paddedHeight:=(tiler.m_linearHeight+ 7) and (not  7);
      tiler.m_paddedDepth :=tiler.m_linearDepth;
     end;
  else
   Assert(false);
 end;

 size:=tiler.m_paddedWidth*
       tiler.m_paddedHeight*
       tiler.m_paddedDepth*
       m_bytePerElement;

 cur:=FastHash(image.key.Addr,size);

 if (cur<>image.hash) then
 begin
  image.hash:=cur;
  Result:=True;
 end;

end;

function CheckFromBuffer(image:TObject):Boolean;
begin
 Result:=False;

 Case TvImage2(image).key.params.tiling_idx of
  kTileModeDisplay_LinearAligned,
  kTileModeDisplay_LinearGeneral:
   Result:=_Check_Linear(TvImage2(image));

  kTileModeDisplay_2dThin: //render target tiling todo
   Result:=false;

  kTileModeDepth_2dThin_64 ,
  kTileModeDepth_2dThin_128,
  kTileModeDepth_2dThin_256,
  kTileModeDepth_2dThin_512,
  kTileModeDepth_2dThin_1K : //depth tiling todo
   Result:=false;

  kTileModeDepth_1dThin,
  kTileModeDisplay_1dThin,
  kTileModeThin_1dThin,  //texture
  $1B:
   Result:=_Check_Thin_1dThin(TvImage2(image));

  kTileModeThin_2dThin:
   Result:=_Check_Linear(TvImage2(image)); //TODO

  else
   if not SKIP_UNKNOW_TILING then
   Assert(false,'TODO tiling_idx:'+get_tiling_idx_str(TvImage2(image).key.params.tiling_idx));
 end;

end;

end.

