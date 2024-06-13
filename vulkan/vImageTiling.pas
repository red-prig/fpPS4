unit vImageTiling;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 ps4_tiling,
 Vulkan,
 vDevice,
 vMemory,
 vBuffer,
 vImage,
 vImageManager,
 vHostBufferManager,
 vCmdBuffer;

Function  GetLinearAlignWidth(bpp,width:Ptruint):Ptruint;
Function  GetLinearSize(image:TvImage2;align:Boolean):Ptruint;

procedure pm4_load_from(cmd:TvCustomCmdBuffer;ri:TvImage2;IMAGE_USAGE:Byte);

implementation

Function GetLinearAlignWidth(bpp,width:Ptruint):Ptruint; inline;
var
 align_m:Ptruint;
begin
 align_m:=(64 div bpp)-1;
 Result:=(width+align_m) and (not align_m);
end;

Function GetLinearSize(image:TvImage2;align:Boolean):Ptruint;
var
 m_bytePerElement:Ptruint;
 m_level,m_width,m_height:Ptruint;
 m_padwidth,m_padheight:Ptruint;
 m_slice:Ptruint;
begin
 m_bytePerElement:=getFormatSize(image.key.cformat);

 m_level :=image.key.params.mipLevels;
 m_width :=image.key.params.width;
 m_height:=image.key.params.height;

 Result:=0;

 while (m_level>0) do
 begin
  m_padwidth :=m_width;
  m_padheight:=m_height;

  if align then
  begin
   m_padwidth:=GetLinearAlignWidth(m_bytePerElement,m_padwidth);
  end;

  if IsTexelFormat(image.key.cformat) then
  begin
   m_padwidth :=(m_padwidth +3) shr 2;
   m_padheight:=(m_padheight+3) shr 2;
  end;

  m_slice:=m_padwidth*
           m_padheight*
           m_bytePerElement;

  //m_slice:=(m_slice+255) and (not Ptruint(255));

  Result:=Result+m_slice;

  Dec(m_level);
  m_width :=m_width  shr 1;
  m_height:=m_height shr 1;
  if (m_width =0) then m_width :=1;
  if (m_height=0) then m_height:=1;
 end;

 Result:=Result*
         image.key.params.depth*
         image.key.params.arrayLayers;
end;

Function Get1dThinAlignWidth(bpp,width:Ptruint):Ptruint; inline;
var
 align_m:Ptruint;
begin
 align_m:=(32 div bpp)-1;
 Result:=(width+align_m) and (not align_m);
end;

Function Get1dThinSize(image:TvImage2):Ptruint;
var
 m_bytePerElement:Ptruint;
 m_level,m_width,m_height:Ptruint;
 m_padwidth,m_padheight:Ptruint;
 m_slice:Ptruint;
begin
 m_bytePerElement:=getFormatSize(image.key.cformat);

 m_level :=image.key.params.mipLevels;
 m_width :=image.key.params.width;
 m_height:=image.key.params.height;

 Result:=0;

 while (m_level>0) do
 begin
  m_padwidth :=Get1dThinAlignWidth(m_bytePerElement,m_width);
  m_padheight:=(m_height+7) and (not 7);

  if IsTexelFormat(image.key.cformat) then
  begin
   m_padwidth :=(m_padwidth +3) shr 2;
   m_padheight:=(m_padheight+3) shr 2;
  end;

  m_slice:=m_padwidth*
           m_padheight*
           m_bytePerElement;

  //m_slice:=(m_slice+255) and (not Ptruint(255));

  Result:=Result+m_slice;

  Dec(m_level);
  m_width :=m_width  shr 1;
  m_height:=m_height shr 1;
  if (m_width =0) then m_width :=1;
  if (m_height=0) then m_height:=1;
 end;

 Result:=(Result+255) and (not Ptruint(255));

 Result:=Result*
         image.key.params.depth*
         image.key.params.arrayLayers;
end;

function AlignDw(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
begin
 Result:=addr-(addr mod alignment);
end;

type
 t_load_from_cb=procedure(cmd:TvCustomCmdBuffer;image:TvImage2);

type
 TvTempBuffer=class(TvBuffer)
  procedure ReleaseTmp(Sender:TObject); register;
 end;

procedure TvTempBuffer.ReleaseTmp(Sender:TObject); register;
begin
 Release(nil);
 Free;
end;

procedure load_clear(cmd:TvCustomCmdBuffer;image:TvImage2);
var
 Color:TVkClearColorValue;
 DepthStencil:TVkClearDepthStencilValue;
 Range:TVkImageSubresourceRange;
begin

 image.PushBarrier(cmd,
                   ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                   VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                   ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 case image.key.cformat of
  //stencil
  VK_FORMAT_S8_UINT,
  //depth
  VK_FORMAT_D16_UNORM,
  VK_FORMAT_X8_D24_UNORM_PACK32,
  VK_FORMAT_D32_SFLOAT,
  //depth stencil
  VK_FORMAT_D16_UNORM_S8_UINT,
  VK_FORMAT_D24_UNORM_S8_UINT,
  VK_FORMAT_D32_SFLOAT_S8_UINT:
   begin
    DepthStencil:=Default(TVkClearDepthStencilValue);
    Range:=image.GetSubresRange;

    {
    vkCmdClearDepthStencilImage(cmd.FCmdbuf,
                                image.FHandle,
                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                @DepthStencil,
                                1,
                                @Range);}

   end;
  else
   begin
    Color:=Default(TVkClearColorValue);
    Range:=image.GetSubresRange;

    {
    vkCmdClearColorImage(cmd.FCmdbuf,
                         image.FHandle,
                         VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                         @Color,
                         1,
                         @Range);}

   end;
 end;


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

Procedure SaveToTGA(const name:RawByteString;pData:Pointer;width,height,bpp:Ptruint);
var
 F:THandle;
 Header:TTGAHeader;
 slice:Ptruint;
begin
 slice:=(width*height*bpp+7) div 8;

 Header:=Default(TTGAHeader);

 Header.datatypecode   :=3;

 Header.width          :=width;
 Header.height         :=height;
 Header.bitsperpixel   :=bpp;
 Header.imagedescriptor:=32;

 F:=FileCreate(name);
 FileWrite(F,Header,SizeOf(TTGAHeader));
 FileWrite(F,pData^,slice);
 FileClose(F);
end;

Procedure copy_1dThin(var tiler:Tiler1d;src,dst:Pointer);
var
 m_bytePerElement:Ptruint;
 m_slice_size:Ptruint;
 i,x,y,z:QWORD;
 pSrc,pDst:Pointer;
begin
 m_bytePerElement:=tiler.m_bitsPerElement div 8;
 m_slice_size:=(tiler.m_linearWidth*tiler.m_linearHeight);
 //
 For z:=0 to tiler.m_linearDepth-1 do
  For y:=0 to tiler.m_linearHeight-1 do
   For x:=0 to tiler.m_linearWidth-1 do
    begin
     i:=0;
     tiler.getTiledElementByteOffset(i,x,y,z);
     pSrc:=@PByte(src)[i];
     pDst:=@PByte(dst)[(z*m_slice_size+y*tiler.m_linearWidth+x)*m_bytePerElement];
     Move(pSrc^,pDst^,m_bytePerElement);
    end;
end;

Procedure _Copy_Linear(cmd:TvCustomCmdBuffer;buf:TvTempBuffer;image:TvImage2);
var
 BufferImageCopy:TVkBufferImageCopy;
 size:Ptruint;

 BufferImageCopyA:array of TVkBufferImageCopy;

 m_bytePerElement:Ptruint;
 m_level,m_width,m_height:Ptruint;
 m_padwidth,m_padheight:Ptruint;
 m_slice :Ptruint;
 m_offset:Ptruint;

 a,d,b:Ptruint;
begin

 cmd.AddDependence(@buf.ReleaseTmp);

 m_bytePerElement:=getFormatSize(image.key.cformat);

 size:=GetLinearSize(image,false);

 image.PushBarrier(cmd,
                   ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                   VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                   ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 vkBufferMemoryBarrier(cmd.FCmdbuf,
                       buf.FHandle,
                       ord(VK_ACCESS_SHADER_WRITE_BIT),
                       ord(VK_ACCESS_MEMORY_READ_BIT),
                       0,size,
                       ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                       ord(VK_PIPELINE_STAGE_TRANSFER_BIT)
                       );

 BufferImageCopy:=Default(TVkBufferImageCopy);
 BufferImageCopy.imageSubresource:=image.GetSubresLayer;
 BufferImageCopy.imageSubresource.layerCount:=1;
 BufferImageCopy.imageExtent.depth:=1;

 BufferImageCopyA:=nil;
 SetLength(BufferImageCopyA,image.key.params.arrayLayers*image.key.params.depth*image.key.params.mipLevels);
 b:=0;

 m_offset:=0;

 for a:=0 to image.key.params.arrayLayers-1 do
 for d:=0 to image.key.params.depth-1 do
 begin
  BufferImageCopy.imageSubresource.baseArrayLayer:=a;
  BufferImageCopy.imageOffset.z:=d;

  m_level :=image.key.params.mipLevels;
  m_width :=image.key.params.width;
  m_height:=image.key.params.height;

  while (m_level>0) do
  begin
   Assert((m_offset and 3)=0,'align by 4');
   BufferImageCopy.bufferOffset:=m_offset;

   BufferImageCopy.imageSubresource.mipLevel:=image.key.params.mipLevels-m_level;

   BufferImageCopy.imageExtent.width :=m_width;
   BufferImageCopy.imageExtent.height:=m_height;

   BufferImageCopyA[b]:=BufferImageCopy;
   Inc(b);

   if IsTexelFormat(image.key.cformat) then
   begin
    m_padwidth :=(m_width +3) shr 2;
    m_padheight:=(m_height+3) shr 2;
   end else
   begin
    m_padwidth :=m_width ;
    m_padheight:=m_height;
   end;

   m_slice:=m_padwidth*m_padheight*m_bytePerElement;

   m_offset:=m_offset+m_slice;

   Dec(m_level);
   m_width :=m_width  shr 1;
   m_height:=m_height shr 1;
   if (m_width =0) then m_width :=1;
   if (m_height=0) then m_height:=1;
  end;

 end;

 vkCmdCopyBufferToImage(cmd.FCmdbuf,
                        buf.FHandle,
                        image.FHandle,
                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                        Length(BufferImageCopyA),
                        @BufferImageCopyA[0]);


end;

Procedure load_1dThin(cmd:TvCustomCmdBuffer;image:TvImage2);
var
 buf:TvTempBuffer;
 vmem:TvPointer;

 tiler:Tiler1d;

 m_bytePerElement:Ptruint;
 m_level,m_width,m_height:Ptruint;
 m_padwidth,m_padheight:Ptruint;
 //m_slice:Ptruint;

 m_full_linear_size:Ptruint;

 m_base:Pointer;

 src:Pointer;
 dst:Pointer;
begin
 Assert(image.key.params.samples<=1,'image.key.params.samples>1');

 m_full_linear_size:=GetLinearSize(image,False);
 //m_base:=GetMem(m_full_linear_size);

 buf:=TvTempBuffer.Create(m_full_linear_size,ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT),nil);

 vmem:=MemManager.FetchMemory(buf.GetRequirements,V_PROP_HOST_VISIBLE or V_PROP_DEVICE_LOCAL);

 if (vmem.FMemory=nil) then
 begin
  vmem:=MemManager.FetchMemory(buf.GetRequirements,V_PROP_HOST_VISIBLE);
 end;

 buf.BindMem(vmem);

 //Release ref in ReleaseTmp

 m_base:=nil;
 vkMapMemory(Device.FHandle,
             buf.FBind.FMemory.FHandle,
             buf.FBind.FOffset,
             m_full_linear_size,
             0,
             @m_base);

 dst:=m_base;

 m_bytePerElement:=getFormatSize(image.key.cformat);

 tiler.init_surface(m_bytePerElement*8,image.key.params.tiling.idx,image.key.params.tiling.alt);

 //TvBuffer

 m_level :=image.key.params.mipLevels;
 m_width :=image.key.params.width;
 m_height:=image.key.params.height;

 src:=image.key.Addr;

 while (m_level>0) do
 begin
  m_padwidth :=Get1dThinAlignWidth(m_bytePerElement,m_width);
  m_padheight:=(m_height+7) and (not 7);

  if IsTexelFormat(image.key.cformat) then
  begin
   m_padwidth :=(m_padwidth +3) shr 2;
   m_padheight:=(m_padheight+3) shr 2;
  end;

  {
  m_slice:=m_padwidth*
           m_padheight*
           m_bytePerElement;

  m_slice:=(m_slice+255) and (not Ptruint(255));
  }

  //
  tiler.m_linearWidth    :=m_width;
  tiler.m_linearHeight   :=m_height;
  tiler.m_linearDepth    :=1;

  if IsTexelFormat(image.key.cformat) then
  begin
   tiler.m_linearWidth :=(tiler.m_linearWidth +3) shr 2;
   tiler.m_linearHeight:=(tiler.m_linearHeight+3) shr 2;
  end;

  tiler.m_paddedWidth    :=m_padwidth;
  tiler.m_paddedHeight   :=m_padheight;
  tiler.m_paddedDepth    :=1;

  tiler.m_linearSizeBytes:=tiler.m_linearWidth*tiler.m_linearHeight*tiler.m_linearDepth*m_bytePerElement;
  tiler.m_tiledSizeBytes :=tiler.m_paddedWidth*tiler.m_paddedHeight*tiler.m_paddedDepth*m_bytePerElement;
  //tiler.m_tiledSizeBytes:=(tiler.m_tiledSizeBytes+255) and (not Ptruint(255));

  tiler.m_tilesPerRow    :=tiler.m_paddedWidth div kMicroTileWidth;
  tiler.m_tilesPerSlice  :=tiler.m_tilesPerRow * (tiler.m_paddedHeight div kMicroTileHeight);
  //

  if (ptruint(dst-m_base)+tiler.m_linearSizeBytes)>m_full_linear_size then
  begin
   Writeln(ptruint(dst-m_base)+tiler.m_linearSizeBytes,'>',m_full_linear_size);
   Assert(false);
  end;

  copy_1dThin(tiler,src,dst);

  {
  SaveToTGA('shader_dump\texture_mip'+IntToStr(m_level)+
                                  '_'+IntToStr(m_width)+
                                  'x'+IntToStr(m_height)+
                                      '.tga',
            dst,
            tiler.m_linearWidth,
            tiler.m_linearHeight,
            tiler.m_bitsPerElement);
  }

  src:=src+tiler.m_tiledSizeBytes;
  dst:=dst+tiler.m_linearSizeBytes;

  Dec(m_level);
  m_width :=m_width  shr 1;
  m_height:=m_height shr 1;
  if (m_width =0) then m_width :=1;
  if (m_height=0) then m_height:=1;
 end;

 vkUnmapMemory(Device.FHandle,buf.FBind.FMemory.FHandle);
 //FreeMem(m_base);

 _Copy_Linear(cmd,buf,image);
end;

Procedure load_Linear(cmd:TvCustomCmdBuffer;image:TvImage2);
var
 buf:TvHostBuffer;
 BufferImageCopy:TVkBufferImageCopy;
 size:Ptruint;

 addr:Pointer;

 BufferImageCopyA:array of TVkBufferImageCopy;

 m_bytePerElement:Ptruint;
 m_level,m_width,m_height:Ptruint;
 m_padwidth,m_padheight:Ptruint;
 m_slice :Ptruint;
 m_offset:Ptruint;

 a,d,b:Ptruint;
begin

 m_bytePerElement:=getFormatSize(image.key.cformat);

 size:=GetLinearSize(image,(image.key.params.tiling.idx=8));

 addr:=image.key.Addr;

 buf:=FetchHostBuffer(cmd,
                      QWORD(addr),
                      size,
                      ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT));

 image.PushBarrier(cmd,
                   ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                   VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                   ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 vkBufferMemoryBarrier(cmd.FCmdbuf,
                       buf.FHandle,
                       ord(VK_ACCESS_SHADER_WRITE_BIT),
                       ord(VK_ACCESS_MEMORY_READ_BIT),
                       0,size,
                       ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                       ord(VK_PIPELINE_STAGE_TRANSFER_BIT)
                       );

 BufferImageCopy:=Default(TVkBufferImageCopy);
 BufferImageCopy.imageSubresource:=image.GetSubresLayer;
 BufferImageCopy.imageSubresource.layerCount:=1;
 BufferImageCopy.imageExtent.depth:=1;

 BufferImageCopyA:=nil;
 SetLength(BufferImageCopyA,image.key.params.arrayLayers*image.key.params.depth*image.key.params.mipLevels);
 b:=0;

 m_offset:=buf.FAddr-QWORD(addr);

 for a:=0 to image.key.params.arrayLayers-1 do
 for d:=0 to image.key.params.depth-1 do
 begin
  BufferImageCopy.imageSubresource.baseArrayLayer:=a;
  BufferImageCopy.imageOffset.z:=d;

  m_level :=image.key.params.mipLevels;
  m_width :=image.key.params.width;
  m_height:=image.key.params.height;

  while (m_level>0) do
  begin
   Assert((m_offset and 3)=0,'align by 4');
   BufferImageCopy.bufferOffset:=m_offset;

   BufferImageCopy.imageSubresource.mipLevel:=image.key.params.mipLevels-m_level;

   BufferImageCopy.imageExtent.width :=m_width;
   BufferImageCopy.imageExtent.height:=m_height;

   if (image.key.params.tiling.idx=8) then
   begin
    BufferImageCopy.bufferRowLength:=GetLinearAlignWidth(m_bytePerElement,m_width);
   end;

   BufferImageCopyA[b]:=BufferImageCopy;
   Inc(b);

   if IsTexelFormat(image.key.cformat) then
   begin
    m_padwidth :=(m_width +3) shr 2;
    m_padheight:=(m_height+3) shr 2;
   end else
   begin
    m_padwidth :=m_width ;
    m_padheight:=m_height;
   end;

   m_slice:=m_padwidth*m_padheight*m_bytePerElement;

   m_offset:=m_offset+m_slice;

   Dec(m_level);
   m_width :=m_width  shr 1;
   m_height:=m_height shr 1;
   if (m_width =0) then m_width :=1;
   if (m_height=0) then m_height:=1;
  end;

 end;

 vkCmdCopyBufferToImage(cmd.FCmdbuf,
                        buf.FHandle,
                        image.FHandle,
                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                        Length(BufferImageCopyA),
                        @BufferImageCopyA[0]);


end;

var
 a_load_from:array[0..63] of t_load_from_cb;

procedure pm4_load_from(cmd:TvCustomCmdBuffer;ri:TvImage2;IMAGE_USAGE:Byte);
var
 cb:t_load_from_cb;
begin
 if (IMAGE_USAGE and TM_READ)=0 then Exit;

 //if (ri.data_usage and TM_READ)<>0 then Exit;
 //ri.data_usage:=ri.data_usage or TM_READ;

 //IMAGE_USAGE:=IMAGE_USAGE and (not TM_READ);

 a_load_from[10   ]:=@load_Linear;//@load_clear;
 a_load_from[10+32]:=@load_Linear;//@load_clear;

 a_load_from[2    ]:=@load_Linear;//@load_clear;
 a_load_from[2+32 ]:=@load_Linear;//@load_clear;

 a_load_from[0    ]:=@load_Linear;//@load_clear;
 a_load_from[0+32 ]:=@load_Linear;//@load_clear;

 //
 a_load_from[ 5   ]:=@load_1dThin;
 a_load_from[ 5+32]:=@load_1dThin;

 a_load_from[ 9   ]:=@load_1dThin;
 a_load_from[ 9+32]:=@load_1dThin;

 a_load_from[13   ]:=@load_1dThin;
 a_load_from[13+32]:=@load_1dThin;
 //

 a_load_from[8    ]:=@load_Linear;
 a_load_from[8+32 ]:=@load_Linear;

 cb:=a_load_from[Byte(ri.key.params.tiling)];

 if (cb=nil) then
 begin
  Writeln(stderr,'tiling:'+IntToStr(ri.key.params.tiling.idx)+' alt:'+IntToStr(ri.key.params.tiling.alt));
  Assert(false,'tiling:'+IntToStr(ri.key.params.tiling.idx)+' alt:'+IntToStr(ri.key.params.tiling.alt));
 end;

 cb(cmd,ri);
end;

{
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
}

end.

