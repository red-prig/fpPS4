unit vImageTiling;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 sys_bootparam,
 ps4_tiling,
 Vulkan,
 vDevice,
 vMemory,
 vBuffer,
 vImage,
 vImageManager,
 vHostBufferManager,
 vCmdBuffer;

procedure pm4_load_from (cmd:TvCustomCmdBuffer;image:TvCustomImage2;IMAGE_USAGE:Byte);
procedure pm4_write_back(cmd:TvCustomCmdBuffer;image:TvCustomImage2);
Function  get_image_size(const key:TvImageKey):Ptruint;

implementation

Function GetLinearAlignWidth(bpp,width:Ptruint):Ptruint; inline;
var
 align_m:Ptruint;
begin
 align_m:=(64 div bpp)-1;
 Result:=(width+align_m) and (not align_m);
end;

function Max(a,b:Ptruint):Ptruint; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

Function GetLinearSize(const key:TvImageKey;align:Boolean):Ptruint;
var
 m_bytePerElement:Ptruint;
 m_level,m_width,m_height:Ptruint;
 m_padwidth,m_padheight:Ptruint;
 m_slice:Ptruint;
begin
 Assert(key.params.samples<=1,'key.params.samples>1');

 m_bytePerElement:=getFormatSize(key.cformat);

 m_level :=key.params.mipLevels;
 m_width :=key.params.width;
 m_height:=key.params.height;

 Result:=0;

 while (m_level>0) do
 begin
  m_padwidth :=m_width;
  m_padheight:=m_height;

  if align then
  begin
   m_padwidth:=GetLinearAlignWidth(m_bytePerElement,m_padwidth);
  end;

  if IsTexelFormat(key.cformat) then
  begin
   m_padwidth :=(m_padwidth +3) shr 2;
   m_padheight:=(m_padheight+3) shr 2;
  end;

  m_slice:=m_padwidth*
           m_padheight*
           key.params.depth*
           key.params.arrayLayers*
           m_bytePerElement;

  //64?

  Result:=Result+m_slice;

  Dec(m_level);
  m_width :=Max(1,m_width  shr 1);
  m_height:=Max(1,m_height shr 1);
 end;

end;

Function GetLinearAlignSize(const key:TvImageKey):Ptruint;
begin
 Result:=GetLinearSize(key,true);
end;

function nextPowerOfTwo(x:Ptruint):Ptruint; inline;
begin
 x:=(x-1);
 x:=x or (x shr 1);
 x:=x or (x shr 2);
 x:=x or (x shr 4);
 x:=x or (x shr 8);
 x:=x or (x shr 16);
 x:=x or (x shr 32);
 Result:=(x+1);
end;

Function Get1dThinAlignWidth(bpp,width:Ptruint):Ptruint; inline;
var
 align_m:Ptruint;
begin
 align_m:=(32 div bpp)-1;
 Result:=(width+align_m) and (not align_m);
 Result:=(Result+7) and (not 7);
end;

Function Get1dThinSize(const key:TvImageKey):Ptruint;
var
 m_bytePerElement:Ptruint;
 m_level,m_width,m_height:Ptruint;
 m_padwidth   :Ptruint;
 m_padheight  :Ptruint;
 m_depth      :Ptruint;
 m_arrayLayers:Ptruint;
 m_slice:Ptruint;
begin
 Assert(key.params.samples<=1,'key.params.samples>1');

 m_bytePerElement:=getFormatSize(key.cformat);

 m_level      :=key.params.mipLevels;
 //
 m_width      :=key.params.width;
 m_height     :=key.params.height;
 m_depth      :=key.params.depth;
 m_arrayLayers:=key.params.arrayLayers;

 if (key.params.pow2pad<>0) then
 begin
  m_width      :=nextPowerOfTwo(m_width);
  m_height     :=nextPowerOfTwo(m_height);
  m_depth      :=nextPowerOfTwo(m_depth);
  m_arrayLayers:=nextPowerOfTwo(m_arrayLayers);
 end;

 Result:=0;

 while (m_level>0) do
 begin
  m_padwidth :=m_width;
  m_padheight:=m_height;

  if IsTexelFormat(key.cformat) then
  begin
   m_padwidth :=(m_padwidth +3) shr 2;
   m_padheight:=(m_padheight+3) shr 2;
  end;

  m_padwidth :=Get1dThinAlignWidth(m_bytePerElement,m_padwidth);
  m_padheight:=(m_padheight+7) and (not 7);

  if (m_level<>1) then
  begin
   m_slice:=m_padwidth*
            m_padheight*
            m_depth*
            m_arrayLayers*
            m_bytePerElement;

   m_slice:=(m_slice+255) and (not Ptruint(255));
  end else
  begin
   //Trim the last layer

   m_slice:=m_padwidth*
            m_padheight*
            key.params.depth*
            key.params.arrayLayers*
            m_bytePerElement;

  end;

  Result:=Result+m_slice;

  Dec(m_level);
  m_width :=Max(1,m_width  shr 1);
  m_height:=Max(1,m_height shr 1);
 end;

end;

function AlignDw(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
begin
 Result:=addr-(addr mod alignment);
end;

type
 TvTempBuffer=class(TvBuffer)
  procedure ReleaseTmp(Sender:TObject); virtual; register;
 end;

procedure TvTempBuffer.ReleaseTmp(Sender:TObject); register;
begin
 //force free
 Free;
end;

procedure load_clear(cmd:TvCustomCmdBuffer;image:TvCustomImage2);
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

type
 t_copy_type=(BufferToImage,ImageToBuffer);

Procedure _Copy_Linear(ctype:t_copy_type;cmd:TvCustomCmdBuffer;buf:TvTempBuffer;image:TvCustomImage2);
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

 size:=GetLinearSize(image.key,false);

 case ctype of
  BufferToImage:
   begin
    image.PushBarrier(cmd,
                      ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                      VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                      ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

    cmd.BufferMemoryBarrier(buf.FHandle,
                            ord(VK_ACCESS_HOST_READ_BIT) or ord(VK_ACCESS_HOST_WRITE_BIT),
                            ord(VK_ACCESS_TRANSFER_READ_BIT),
                            0,size,
                            ord(VK_PIPELINE_STAGE_HOST_BIT),
                            ord(VK_PIPELINE_STAGE_TRANSFER_BIT)
                           );
   end;
  ImageToBuffer:
   begin
    image.PushBarrier(cmd,
                      ord(VK_ACCESS_TRANSFER_READ_BIT),
                      VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                      ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

    cmd.BufferMemoryBarrier(buf.FHandle,
                            ord(VK_ACCESS_HOST_READ_BIT) or ord(VK_ACCESS_HOST_WRITE_BIT),
                            ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                            0,size,
                            ord(VK_PIPELINE_STAGE_HOST_BIT),
                            ord(VK_PIPELINE_STAGE_TRANSFER_BIT)
                           );
   end;
 end;

 BufferImageCopy:=Default(TVkBufferImageCopy);
 BufferImageCopy.imageSubresource:=image.GetSubresLayer;
 BufferImageCopy.imageSubresource.layerCount:=1;
 BufferImageCopy.imageExtent.depth:=1;

 BufferImageCopyA:=nil;
 SetLength(BufferImageCopyA,image.key.params.arrayLayers*image.key.params.depth*image.key.params.mipLevels);
 b:=0;

 m_offset:=0;

 //mips
 m_level :=image.key.params.mipLevels;
 m_width :=image.key.params.width;
 m_height:=image.key.params.height;

 while (m_level>0) do
 begin
  BufferImageCopy.imageSubresource.mipLevel:=image.key.params.mipLevels-m_level;

  BufferImageCopy.imageExtent.width :=m_width;
  BufferImageCopy.imageExtent.height:=m_height;

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

  //array
  for a:=0 to image.key.params.arrayLayers-1 do
  for d:=0 to image.key.params.depth-1 do
  begin
   Assert((m_offset and 3)=0,'align by 4');
   BufferImageCopy.bufferOffset:=m_offset;

   BufferImageCopy.imageSubresource.baseArrayLayer:=a;
   BufferImageCopy.imageOffset.z:=d;

   BufferImageCopyA[b]:=BufferImageCopy;
   Inc(b);

   m_offset:=m_offset+m_slice;
  end;
  //array

  Dec(m_level);
  m_width :=Max(1,m_width  shr 1);
  m_height:=Max(1,m_height shr 1);
 end;
 //mips

 case ctype of
  BufferToImage:
   begin
    cmd.CopyBufferToImage(buf.FHandle,
                          image.FHandle,
                          VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                          Length(BufferImageCopyA),
                          @BufferImageCopyA[0]);
   end;
  ImageToBuffer:
   begin
    cmd.CopyImageToBuffer(image.FHandle,
                          VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                          buf.FHandle,
                          Length(BufferImageCopyA),
                          @BufferImageCopyA[0]);
   end;
 end;

end;

Procedure copy_1dThin_to_linear(var tiler:Tiler1d;src,dst:Pointer);
var
 m_bytePerElement:Ptruint;
 m_slice_size:Ptruint;
 i,x,y,z:QWORD;
 pSrc,pDst:Pointer;
begin
 m_bytePerElement:=tiler.m_bytePerElement;
 m_slice_size:=(tiler.m_linearWidth*tiler.m_linearHeight);
 //
 For z:=0 to tiler.m_linearDepth-1 do
  For y:=0 to tiler.m_linearHeight-1 do
   For x:=0 to tiler.m_linearWidth-1 do
    begin
     i:=0;
     tiler.getTiledElementByteOffset(i,x,y,z);
     pSrc:=@PByte(src)[i];
     //
     pDst:=@PByte(dst)[(z*m_slice_size+y*tiler.m_linearWidth+x)*m_bytePerElement];
     //
     Move(pSrc^,pDst^,m_bytePerElement);
    end;
end;

Procedure copy_linear_to_1dThin(var tiler:Tiler1d;src,dst:Pointer);
var
 m_bytePerElement:Ptruint;
 m_slice_size:Ptruint;
 i,x,y,z:QWORD;
 pSrc,pDst:Pointer;
begin
 m_bytePerElement:=tiler.m_bytePerElement;
 m_slice_size:=(tiler.m_linearWidth*tiler.m_linearHeight);
 //
 For z:=0 to tiler.m_linearDepth-1 do
  For y:=0 to tiler.m_linearHeight-1 do
   For x:=0 to tiler.m_linearWidth-1 do
    begin
     pSrc:=@PByte(src)[(z*m_slice_size+y*tiler.m_linearWidth+x)*m_bytePerElement];
     //
     i:=0;
     tiler.getTiledElementByteOffset(i,x,y,z);
     pDst:=@PByte(dst)[i];
     //
     Move(pSrc^,pDst^,m_bytePerElement);
    end;
end;

Procedure load_write_1dThin(ctype:t_copy_type;
                            image:TvCustomImage2;
                            m_full_linear_size:Ptruint;
                            m_base:Pointer);
var
 tiler:Tiler1d;

 m_bytePerElement:Ptruint;
 m_level,m_width,m_height:Ptruint;

 src:Pointer;
 dst:Pointer;

 a:Ptruint;
begin

 dst:=m_base;

 m_bytePerElement:=getFormatSize(image.key.cformat);

 tiler.init_surface(m_bytePerElement,
                    ord(IsTexelFormat(image.key.cformat)),
                    image.key.params.tiling.idx,
                    image.key.params.tiling.alt);

 //TvBuffer

 //mips
 m_level :=image.key.params.mipLevels;
 //
 m_width :=image.key.params.width;
 m_height:=image.key.params.height;

 if (image.key.params.pow2pad<>0) then
 begin
  m_width :=nextPowerOfTwo(m_width);
  m_height:=nextPowerOfTwo(m_height);
 end;

 src:=image.key.addr;

 while (m_level>0) do
 begin
  tiler.init_size_2d(m_width,m_height);

  //array
  for a:=0 to image.key.params.arrayLayers-1 do
  begin

   if (ptruint(dst-m_base)+tiler.m_linearSizeBytes)>m_full_linear_size then
   begin
    Writeln(ptruint(dst-m_base)+tiler.m_linearSizeBytes,'>',m_full_linear_size);
    Assert(false);
   end;

   //x,y,z
   case ctype of
    BufferToImage:copy_1dThin_to_linear(tiler,src,dst);
    ImageToBuffer:copy_linear_to_1dThin(tiler,dst,src);
   end;
   //x,y,z

   {
   SaveToTGA('shader_dump\texture_a'+IntToStr(a)+
                                   '_mip'+IntToStr(m_level)+
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

  end;
  //array

  if (image.key.params.pow2pad<>0) then
  begin
   a:=nextPowerOfTwo(image.key.params.arrayLayers)-image.key.params.arrayLayers;

   src:=src+tiler.m_tiledSizeBytes*a;
  end;

  //Writeln('nextPowerOfTwo =',nextPowerOfTwo(image.key.params.arrayLayers));

  src:=Pointer((qword(src)+255) and (not 255));

  Dec(m_level);
  m_width :=Max(1,m_width  shr 1);
  m_height:=Max(1,m_height shr 1);
 end;
 //mips

 //Writeln('size1=',(src-image.key.addr));
 //Writeln('size2=',Get1dThinSize(image.key));

end;

Procedure load_1dThin(cmd:TvCustomCmdBuffer;image:TvCustomImage2);
var
 buf:TvTempBuffer;
 vmem:TvPointer;

 m_full_linear_size:Ptruint;

 m_base:Pointer;
begin
 Assert(image.key.params.samples<=1,'image.key.params.samples>1');

 m_full_linear_size:=GetLinearSize(image.key,False);

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

 load_write_1dThin(BufferToImage,
                   image,
                   m_full_linear_size,
                   m_base);

 vkUnmapMemory(Device.FHandle,buf.FBind.FMemory.FHandle);

 _Copy_Linear(BufferToImage,cmd,buf,image);
end;

type
 TvTempBufferWriteback=class(TvTempBuffer)
  image:TvCustomImage2;
  m_full_linear_size:Ptruint;
  procedure ReleaseTmp(Sender:TObject); override; register;
 end;

procedure TvTempBufferWriteback.ReleaseTmp(Sender:TObject); register;
var
 m_base:Pointer;
begin
 m_base:=nil;
 vkMapMemory(Device.FHandle,
             FBind.FMemory.FHandle,
             FBind.FOffset,
             m_full_linear_size,
             0,
             @m_base);

 load_write_1dThin(ImageToBuffer,
                   image,
                   m_full_linear_size,
                   m_base);

 vkUnmapMemory(Device.FHandle,FBind.FMemory.FHandle);

 image.Release(Self);
 inherited;
end;

Procedure write_1dThin(cmd:TvCustomCmdBuffer;image:TvCustomImage2);
var
 buf:TvTempBufferWriteback;
 vmem:TvPointer;

 m_full_linear_size:Ptruint;

begin
 Assert(image.key.params.samples<=1,'image.key.params.samples>1');

 m_full_linear_size:=GetLinearSize(image.key,False);

 buf:=TvTempBufferWriteback.Create(m_full_linear_size,ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT),nil);

 buf.image:=image;
 buf.m_full_linear_size:=m_full_linear_size;

 image.Acquire(buf);

 vmem:=MemManager.FetchMemory(buf.GetRequirements,V_PROP_HOST_VISIBLE or V_PROP_DEVICE_LOCAL);

 if (vmem.FMemory=nil) then
 begin
  vmem:=MemManager.FetchMemory(buf.GetRequirements,V_PROP_HOST_VISIBLE);
 end;

 buf.BindMem(vmem);

 _Copy_Linear(ImageToBuffer,cmd,buf,image);
end;

Procedure Load_Linear(cmd:TvCustomCmdBuffer;image:TvCustomImage2);
var
 buf:TvHostBuffer;
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

 m_bytePerElement:=getFormatSize(image.key.cformat);

 size:=GetLinearSize(image.key,(image.key.params.tiling.idx<>kTileModeDisplay_LinearGeneral));

 buf:=FetchHostBuffer(cmd,
                      QWORD(image.key.addr),
                      size);

 m_offset:=buf.FAddr-QWORD(image.key.addr);

 image.PushBarrier(cmd,
                   ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                   VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                   ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 cmd.BufferMemoryBarrier(buf.FHandle,
                         ord(VK_ACCESS_HOST_READ_BIT) or ord(VK_ACCESS_HOST_WRITE_BIT),
                         ord(VK_ACCESS_TRANSFER_READ_BIT),
                         m_offset,
                         size,
                         ord(VK_PIPELINE_STAGE_HOST_BIT),
                         ord(VK_PIPELINE_STAGE_TRANSFER_BIT)
                        );

 BufferImageCopy:=Default(TVkBufferImageCopy);
 BufferImageCopy.imageSubresource:=image.GetSubresLayer;
 BufferImageCopy.imageSubresource.layerCount:=1;
 BufferImageCopy.imageExtent.depth:=1;

 BufferImageCopyA:=nil;
 SetLength(BufferImageCopyA,image.key.params.arrayLayers*image.key.params.depth*image.key.params.mipLevels);
 b:=0;

 //mips
 m_level :=image.key.params.mipLevels;
 m_width :=image.key.params.width;
 m_height:=image.key.params.height;

 while (m_level>0) do
 begin
  BufferImageCopy.imageSubresource.mipLevel:=image.key.params.mipLevels-m_level;

  BufferImageCopy.imageExtent.width :=m_width;
  BufferImageCopy.imageExtent.height:=m_height;

  if (image.key.params.tiling.idx=8) then
  begin
   BufferImageCopy.bufferRowLength:=GetLinearAlignWidth(m_bytePerElement,m_width);
  end;

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

  //array
  for a:=0 to image.key.params.arrayLayers-1 do
  for d:=0 to image.key.params.depth-1 do
  begin
   Assert((m_offset and 3)=0,'align by 4');
   BufferImageCopy.bufferOffset:=m_offset;

   BufferImageCopy.imageSubresource.baseArrayLayer:=a;
   BufferImageCopy.imageOffset.z:=d;

   BufferImageCopyA[b]:=BufferImageCopy;
   Inc(b);

   m_offset:=m_offset+m_slice;
  end;
  //array

  Dec(m_level);
  m_width :=Max(1,m_width  shr 1);
  m_height:=Max(1,m_height shr 1);
 end;
 //mips

 cmd.CopyBufferToImage(buf.FHandle,
                       image.FHandle,
                       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                       Length(BufferImageCopyA),
                       @BufferImageCopyA[0]);


end;

Procedure Writeback_Linear(cmd:TvCustomCmdBuffer;image:TvCustomImage2);
var
 buf:TvHostBuffer;
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

 m_bytePerElement:=getFormatSize(image.key.cformat);

 size:=GetLinearSize(image.key,(image.key.params.tiling.idx<>kTileModeDisplay_LinearGeneral));

 buf:=FetchHostBuffer(cmd,
                      QWORD(image.key.addr),
                      size);

 m_offset:=buf.FAddr-QWORD(image.key.addr);

 image.PushBarrier(cmd,
                   ord(VK_ACCESS_TRANSFER_READ_BIT),
                   VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                   ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 cmd.BufferMemoryBarrier(buf.FHandle,
                         ord(VK_ACCESS_HOST_READ_BIT) or ord(VK_ACCESS_HOST_WRITE_BIT),
                         ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                         m_offset,
                         size,
                         ord(VK_PIPELINE_STAGE_HOST_BIT),
                         ord(VK_PIPELINE_STAGE_TRANSFER_BIT)
                        );

 BufferImageCopy:=Default(TVkBufferImageCopy);
 BufferImageCopy.imageSubresource:=image.GetSubresLayer;
 BufferImageCopy.imageSubresource.layerCount:=1;
 BufferImageCopy.imageExtent.depth:=1;

 BufferImageCopyA:=nil;
 SetLength(BufferImageCopyA,image.key.params.arrayLayers*image.key.params.depth*image.key.params.mipLevels);
 b:=0;

 //mips
 m_level :=image.key.params.mipLevels;
 m_width :=image.key.params.width;
 m_height:=image.key.params.height;

 while (m_level>0) do
 begin
  BufferImageCopy.imageSubresource.mipLevel:=image.key.params.mipLevels-m_level;

  BufferImageCopy.imageExtent.width :=m_width;
  BufferImageCopy.imageExtent.height:=m_height;

  if (image.key.params.tiling.idx=8) then
  begin
   BufferImageCopy.bufferRowLength:=GetLinearAlignWidth(m_bytePerElement,m_width);
  end;

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

  //array
  for a:=0 to image.key.params.arrayLayers-1 do
  for d:=0 to image.key.params.depth-1 do
  begin
   Assert((m_offset and 3)=0,'align by 4');
   BufferImageCopy.bufferOffset:=m_offset;

   BufferImageCopy.imageSubresource.baseArrayLayer:=a;
   BufferImageCopy.imageOffset.z:=d;

   BufferImageCopyA[b]:=BufferImageCopy;
   Inc(b);

   m_offset:=m_offset+m_slice;
  end;
  //array

  Dec(m_level);
  m_width :=Max(1,m_width  shr 1);
  m_height:=Max(1,m_height shr 1);
 end;
 //mips

 cmd.CopyImageToBuffer(image.FHandle,
                       VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                       buf.FHandle,
                       Length(BufferImageCopyA),
                       @BufferImageCopyA[0]);


end;

type
 t_load_from_cb =procedure(cmd:TvCustomCmdBuffer;image:TvCustomImage2);
 t_write_back_cb=t_load_from_cb;
 t_get_size_cb  =function(const key:TvImageKey):Ptruint;

 t_tiling_cbs=record
  load_from :t_load_from_cb;
  write_back:t_write_back_cb;
  get_size  :t_get_size_cb;
 end;

var
 a_tiling_cbs:array[0..63] of t_tiling_cbs;

function TileIdx(idx,alt:Byte):Byte; inline;
begin
 Result:=idx;
 TvTiling(Result).alt:=alt;
end;

procedure set_tiling_cbs(idx,alt:Byte;
                         l:t_load_from_cb;
                         w:t_write_back_cb;
                         g:t_get_size_cb
                        ); inline;
begin
 with a_tiling_cbs[TileIdx(idx,alt)] do
 begin
  load_from :=l;
  write_back:=w;
  get_size  :=g;
 end;
end;

procedure Init;
begin
 set_tiling_cbs(kTileModeDisplay_2dThin       ,0,@Load_Linear,@Writeback_Linear,@GetLinearAlignSize); //@load_clear;
 set_tiling_cbs(kTileModeDisplay_2dThin       ,1,@Load_Linear,@Writeback_Linear,@GetLinearAlignSize); //@load_clear;

 set_tiling_cbs(kTileModeDepth_2dThin_256     ,0,@Load_Linear,@Writeback_Linear,@GetLinearAlignSize); //@load_clear;
 set_tiling_cbs(kTileModeDepth_2dThin_256     ,1,@Load_Linear,@Writeback_Linear,@GetLinearAlignSize); //@load_clear;

 set_tiling_cbs(kTileModeDepth_2dThin_64      ,0,@Load_Linear,@Writeback_Linear,@GetLinearAlignSize); //@load_clear;
 set_tiling_cbs(kTileModeDepth_2dThin_64      ,1,@Load_Linear,@Writeback_Linear,@GetLinearAlignSize); //@load_clear;

 set_tiling_cbs(kTileModeThin_2dThin          ,0,@Load_Linear,@Writeback_Linear,@GetLinearAlignSize); //@load_clear;
 set_tiling_cbs(kTileModeThin_2dThin          ,1,@Load_Linear,@Writeback_Linear,@GetLinearAlignSize); //@load_clear;

 //
 set_tiling_cbs(kTileModeDepth_1dThin         ,0,@load_1dThin,@write_1dThin,@Get1dThinSize);
 set_tiling_cbs(kTileModeDepth_1dThin         ,1,@load_1dThin,@write_1dThin,@Get1dThinSize);

 set_tiling_cbs(kTileModeDisplay_1dThin       ,0,@load_1dThin,@write_1dThin,@Get1dThinSize);
 set_tiling_cbs(kTileModeDisplay_1dThin       ,1,@load_1dThin,@write_1dThin,@Get1dThinSize);

 set_tiling_cbs(kTileModeThin_1dThin          ,0,@load_1dThin,@write_1dThin,@Get1dThinSize);
 set_tiling_cbs(kTileModeThin_1dThin          ,1,@load_1dThin,@write_1dThin,@Get1dThinSize);
 //

 set_tiling_cbs(kTileModeDisplay_LinearAligned,0,@Load_Linear,@Writeback_Linear,@GetLinearAlignSize);
 set_tiling_cbs(kTileModeDisplay_LinearAligned,1,@Load_Linear,@Writeback_Linear,@GetLinearAlignSize);
end;

function get_tiling_name(i:Byte):RawByteString;
begin
 case i of
  // Depth modes (for depth buffers)
  kTileModeDepth_2dThin_64    :Result:='Depth_2dThin_64';
  kTileModeDepth_2dThin_128   :Result:='Depth_2dThin_128';
  kTileModeDepth_2dThin_256   :Result:='Depth_2dThin_256';
  kTileModeDepth_2dThin_512   :Result:='Depth_2dThin_512';
  kTileModeDepth_2dThin_1K    :Result:='Depth_2dThin_1K';
  kTileModeDepth_1dThin       :Result:='Depth_1dThin';
  kTileModeDepth_2dThinPrt_256:Result:='Depth_2dThinPrt_256';
  kTileModeDepth_2dThinPrt_1K :Result:='Depth_2dThinPrt_1K';
  // Display modes
  kTileModeDisplay_LinearAligned:Result:='LinearAligned';
  kTileModeDisplay_1dThin       :Result:='Display_1dThin';
  kTileModeDisplay_2dThin       :Result:='Display_2dThin';
  kTileModeDisplay_ThinPrt      :Result:='Display_ThinPrt';
  kTileModeDisplay_2dThinPrt    :Result:='Display_2dThinPrt';
  // Thin modes (for non-displayable 1D/2D/3D
  kTileModeThin_1dThin   :Result:='Thin_1dThin';
  kTileModeThin_2dThin   :Result:='Thin_2dThin';
  kTileModeThin_3dThin   :Result:='Thin_3dThin';
  kTileModeThin_ThinPrt  :Result:='Thin_ThinPrt';
  kTileModeThin_2dThinPrt:Result:='Thin_2dThinPrt';
  kTileModeThin_3dThinPrt:Result:='Thin_3dThinPrt';
  // Thick modes (for 3D textures)
  kTileModeThick_1dThick   :Result:='Thick_1dThick';
  kTileModeThick_2dThick   :Result:='Thick_2dThick';
  kTileModeThick_3dThick   :Result:='Thick_3dThick';
  kTileModeThick_ThickPrt  :Result:='Thick_ThickPrt';
  kTileModeThick_2dThickPrt:Result:='Thick_2dThickPrt';
  kTileModeThick_3dThickPrt:Result:='Thick_3dThickPrt';
  kTileModeThick_2dXThick  :Result:='Thick_2dXThick';
  kTileModeThick_3dXThick  :Result:='Thick_3dXThick';
  // Hugely inefficient linear display mode -
  kTileModeDisplay_LinearGeneral:Result:='LinearGeneral';
  else
   Result:=IntToStr(i);
 end;
end;

procedure pm4_load_from(cmd:TvCustomCmdBuffer;image:TvCustomImage2;IMAGE_USAGE:Byte);
var
 cb:t_load_from_cb;
 change_rate:t_change_rate;
begin
 if (cmd=nil) or (image=nil) then Exit;

 if (IMAGE_USAGE and TM_READ)=0 then Exit;

 if image.IsDepthAndStencil then
 begin
  pm4_load_from(cmd,image.DepthOnly  ,IMAGE_USAGE);
  pm4_load_from(cmd,image.StencilOnly,IMAGE_USAGE);
  Exit;
 end;

 change_rate:=image.get_change_rate;

 if not change_rate.need_read then Exit;

 if p_print_gpu_ops then
 begin
  Writeln('loadfrom: ',image.FName);
 end;

 cb:=a_tiling_cbs[Byte(image.key.params.tiling)].load_from;

 if (cb=nil) then
 begin
  Writeln(stderr,'tiling:'+get_tiling_name(image.key.params.tiling.idx)+' alt:'+IntToStr(image.key.params.tiling.alt));
  Assert (false ,'tiling:'+get_tiling_name(image.key.params.tiling.idx)+' alt:'+IntToStr(image.key.params.tiling.alt));
 end;

 cmd.EndRenderPass;

 image.restore_vm_track;

 cmd.BeginLabel('loadfrom');

 cb(cmd,image);

 cmd.EndLabel();

 change_rate.mark_init;

 image.apply_change_rate(change_rate);

 image.assign_vm_track;
end;

procedure pm4_write_back(cmd:TvCustomCmdBuffer;image:TvCustomImage2);
var
 cb:t_write_back_cb;
begin
 if (cmd=nil) or (image=nil) then Exit;

 if image.IsDepthAndStencil then
 begin
  pm4_write_back(cmd,image.DepthOnly  );
  pm4_write_back(cmd,image.StencilOnly);
  Exit;
 end;

 if p_print_gpu_ops then
 begin
  Writeln('writeback:',image.FName);
 end;

 cb:=a_tiling_cbs[Byte(image.key.params.tiling)].write_back;

 if (cb=nil) then
 begin
  Writeln(stderr,'tiling:'+get_tiling_name(image.key.params.tiling.idx)+' alt:'+IntToStr(image.key.params.tiling.alt));
  Assert (false ,'tiling:'+get_tiling_name(image.key.params.tiling.idx)+' alt:'+IntToStr(image.key.params.tiling.alt));
 end;

 cmd.EndRenderPass;

 cmd.BeginLabel('writeback');

 cb(cmd,image);

 cmd.EndLabel();

 image.mark_init;

 image.assign_vm_track;

 cmd.AddPlannedTrigger(QWORD(image.key.Addr),QWORD(image.key.Addr)+image.size,image.tobj);
end;

Function get_image_size(const key:TvImageKey):Ptruint; [public, alias:'tiling_get_image_size'];
var
 cb:t_get_size_cb;
begin

 cb:=a_tiling_cbs[Byte(key.params.tiling)].get_size;

 if (cb=nil) then
 begin
  Writeln(stderr,'tiling:'+get_tiling_name(key.params.tiling.idx)+' alt:'+IntToStr(key.params.tiling.alt));
  Assert (false ,'tiling:'+get_tiling_name(key.params.tiling.idx)+' alt:'+IntToStr(key.params.tiling.alt));
 end;

 Result:=cb(key);
end;


{
Procedure LoadFromBuffer(cmd:TvCustomCmdBuffer;image:TObject);
begin
 if (cmd=nil) then Exit;

 Case TvCustomImage2(image).key.params.tiling_idx of
  kTileModeDisplay_LinearAligned,
  kTileModeDisplay_LinearGeneral:
   _Load_Linear(cmd,TvCustomImage2(image));

  kTileModeDisplay_2dThin: //render target tiling todo
   _Load_Linear(cmd,TvCustomImage2(image));

  kTileModeDepth_2dThin_64 ,
  kTileModeDepth_2dThin_128,
  kTileModeDepth_2dThin_256,
  kTileModeDepth_2dThin_512,
  kTileModeDepth_2dThin_1K : //depth tiling todo
   _Load_Linear(cmd,TvCustomImage2(image));

  kTileModeDepth_1dThin,
  kTileModeDisplay_1dThin,
  kTileModeThin_1dThin,  //texture
  $1B:
   _Load_Thin_1dThin(cmd,TvCustomImage2(image));

  kTileModeThin_2dThin:
   _Load_Linear(cmd,TvCustomImage2(image)); //TODO

  else
   if not SKIP_UNKNOW_TILING then
   Assert(false,'TODO tiling_idx:'+get_tiling_idx_str(TvCustomImage2(image).key.params.tiling_idx));
 end;

end;
}

initialization
 Init;

end.

