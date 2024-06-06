unit pm4_me;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 LFQueue,

 Vulkan,
 vDevice,
 vMemory,
 vBuffer,
 vHostBufferManager,
 vImage,
 vImageManager,
 vRender,
 vRenderPassManager,
 vPipelineManager,
 vFramebufferManager,
 vShader,
 vShaderExt,
 vShaderManager,
 vRegs2Vulkan,
 vCmdBuffer,
 vPipeline,
 vSetsPoolManager,
 vSampler,
 vSamplerManager,

 shader_dump,

 ps4_Tiling,

 renderdoc,

 sys_event,
 time,
 md_time,
 kern_thr,
 md_sleep,
 bittype,
 pm4defs,
 pm4_stream,
 si_ci_vi_merged_offset,
 si_ci_vi_merged_enum,
 si_ci_vi_merged_registers,
 si_ci_vi_merged_groups;

type
 t_on_submit_flip_eop=function(submit_id:QWORD):Integer;

 p_pm4_me=^t_pm4_me;
 t_pm4_me=object
  //
  queue:TIntrusiveMPSCQueue; //p_pm4_stream
  event:PRTLEvent;
  on_idle:TProcedure;
  on_submit_flip_eop:t_on_submit_flip_eop;
  //
  started:Pointer;
  td:p_kthread;
  //
  gc_knlist:p_knlist;
  //
  rel_time:QWORD;
  //
  procedure Init(knlist:p_knlist);
  procedure start;
  procedure trigger;
  procedure knote_eventid(event_id,me_id:Byte;timestamp:QWORD;lockflags:Integer);
  procedure Push(var stream:t_pm4_stream);
  procedure free_stream(node:p_pm4_stream); static;
 end;

var
 use_renderdoc_capture:Boolean=False;

implementation

uses
 kern_dmem;

procedure StartFrameCapture;
begin
 if use_renderdoc_capture then
 begin
  if (renderdoc.IsFrameCapturing()=0) then
  begin
   renderdoc.StartFrameCapture(0,0);
  end;
 end;
end;

procedure EndFrameCapture;
begin
 if use_renderdoc_capture then
 begin
  if (renderdoc.IsFrameCapturing()<>0) then
  begin
   renderdoc.EndFrameCapture(0,0);
  end;
 end;
end;

procedure t_pm4_me.Init(knlist:p_knlist);
begin
 queue.Create;
 gc_knlist:=knlist;
end;

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl; forward;

procedure t_pm4_me.start;
begin
 if (XCHG(started,Pointer(1))=nil) then
 begin
  event:=RTLEventCreate;
  //
  kthread_add(@pm4_me_thread,@self,@td,(8*1024*1024) div (16*1024),'[GFX_ME]');
 end;
end;

procedure t_pm4_me.trigger;
begin
 if (event<>nil) then
 begin
  RTLEventSetEvent(event);
 end;
end;

procedure t_pm4_me.knote_eventid(event_id,me_id:Byte;timestamp:QWORD;lockflags:Integer);
begin
 knote(gc_knlist, event_id or (me_id shl 8) or (timestamp shl 16), lockflags);
end;

procedure t_pm4_me.Push(var stream:t_pm4_stream);
var
 node:p_pm4_stream;
begin
 if (stream.First=nil) then Exit;
 //self alloc
 node:=stream.allocator.Alloc(SizeOf(t_pm4_stream));
 //
 node^:=stream;
 //
 stream:=Default(t_pm4_stream);
 //
 queue.Push(node);
 //
 start;
 //
 trigger;
end;

procedure t_pm4_me.free_stream(node:p_pm4_stream);
var
 tmp:t_pm4_stream;
begin
 tmp:=node^;
 tmp.Free;
end;

//

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
  procedure Release(Sender:TObject); register;
 end;

procedure TvTempBuffer.Release(Sender:TObject); register;
begin
 MemManager.Free(FBind);
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

 cmd.AddDependence(@buf.Release);

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

 vmem:=MemManager.Alloc(buf.GetRequirements,ord(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or
                                            ord(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT));

 if (vmem.FMemory=nil) then
 begin
  vmem:=MemManager.Alloc(buf.GetRequirements,ord(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT));
 end;

 buf.BindMem(vmem);

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

 if not get_dmem_ptr(addr,@addr,nil) then
 begin
  Assert(false,'addr:0x'+HexStr(addr)+' not in dmem!');
 end;

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

var
 FCmdPool:TvCmdPool;

procedure Prepare_Uniforms(var FUniformBuilder:TvUniformBuilder;
                           CmdBuffer:TvCmdBuffer);
var
 i:Integer;

 ri:TvImage2;
begin
 if (Length(FUniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FImages) do
  With FUniformBuilder.FImages[i] do
  begin

   ri:=FetchImage(CmdBuffer,
                  FImage,
                  iu_sampled,
                  TM_READ
                 );

   pm4_load_from(CmdBuffer,ri,TM_READ);

   begin

    ri.PushBarrier(CmdBuffer,
                   ord(VK_ACCESS_SHADER_READ_BIT),
                   VK_IMAGE_LAYOUT_GENERAL,
                   ord(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                   ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) );
   end;

  end;
 end;
end;

procedure Bind_Uniforms(var FUniformBuilder:TvUniformBuilder;
                        var FDescriptorGroup:TvDescriptorGroup;
                        ShaderGroup:TvShaderGroup;
                        CmdBuffer:TvCmdBuffer);

 procedure _init; inline;
 begin
  if (FDescriptorGroup=nil) then
  begin
   FDescriptorGroup:=FetchDescriptorGroup(CmdBuffer,ShaderGroup.FLayout);
  end;
 end;

var
 i:Integer;

 ri:TvImage2;
 iv:TvImageView2;
 sm:TvSampler;

 addr_dst:Pointer;

 buf:TvHostBuffer;

 diff :TVkDeviceSize;
 align:TVkDeviceSize;
 range:TVkDeviceSize;
begin

 //images
 if (Length(FUniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FImages) do
  With FUniformBuilder.FImages[i] do
  begin

   ri:=FetchImage(CmdBuffer,
                  FImage,
                  iu_sampled,
                  TM_READ
                 );

   iv:=ri.FetchView(CmdBuffer,FView,iu_sampled);

   _init;

   FDescriptorGroup.FSets[fset].BindImg(bind,0,
                                        iv.FHandle,
                                        VK_IMAGE_LAYOUT_GENERAL);


  end;
 end;
 //images

 //samplers
 if (Length(FUniformBuilder.FSamplers)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FSamplers) do
  With FUniformBuilder.FSamplers[i] do
  begin
   sm:=FetchSampler(CmdBuffer,PS);

   _init;

   FDescriptorGroup.FSets[fset].BindSmp(bind,0,sm.FHandle);

  end;
 end;
 //samplers

 //buffers
 if (Length(FUniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FBuffers) do
  With FUniformBuilder.FBuffers[i] do
  begin

   addr_dst:=nil;
   if not get_dmem_ptr(addr,@addr_dst,nil) then
   begin
    Assert(false,'addr:0x'+HexStr(addr)+' not in dmem!');
   end;

   buf:=FetchHostBuffer(CmdBuffer,QWORD(addr_dst),size,ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT));

   diff:=QWORD(addr_dst)-buf.FAddr;

   align:=diff-AlignDw(diff,limits.minStorageBufferOffsetAlignment);

   if (align<>offset) then
   begin
    Assert(false,'wrong buffer align '+IntToStr(align)+'<>'+IntToStr(offset));
   end;

   diff:=AlignDw(diff,limits.minStorageBufferOffsetAlignment);

   range:=size;

   _init;

   FDescriptorGroup.FSets[fset].BindBuf(bind,0,
                                        VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                        buf.FHandle,
                                        diff,
                                        range {VK_WHOLE_SIZE});


  end;
 end;
 //buffers

end;

procedure pm4_DrawPrepare(var rt_info:t_pm4_rt_info;
                          CmdBuffer:TvCmdBuffer;
                          RenderCmd:TvRenderTargets);
var
 i:Integer;

 FAttrBuilder:TvAttrBuilder;

 FUniformBuilder:TvUniformBuilder;

 RP_KEY:TvRenderPassKey;
 RP:TvRenderPass2;

 GP_KEY:TvGraphicsPipelineKey;
 GP:TvGraphicsPipeline2;

 FB_KEY:TvFramebufferImagelessKey;
 FB_KEY2:TvFramebufferBindedKey;
 FFramebuffer:TvFramebuffer;

 ri:TvImage2;
 iv:TvImageView2;

 FDescriptorGroup:TvDescriptorGroup;
begin

 {
 if (rt_info.RT_COUNT=0) and
    (rt_info.DB_ENABLE) then
 begin
  //ClearDepthTarget

  ri:=FetchImage(CmdBuffer,
                 rt_info.DB_INFO.FImageInfo,
                 iu_depth,
                 rt_info.DB_INFO.DEPTH_USAGE
                 );

  ri.PushBarrier(CmdBuffer,
                 ord(VK_ACCESS_TRANSFER_READ_BIT),
                 VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                 ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

  ri.PushBarrier(CmdBuffer,
                 ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                 VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                 ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

  CmdBuffer.ClearDepthStencilImage(ri.FHandle,
                                   VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                   @rt_info.DB_INFO.CLEAR_VALUE.depthStencil,
                                   ri.GetSubresRange);

  Exit;
 end;
 }


 RP_KEY.Clear;

 RenderCmd.RT_COUNT:=rt_info.RT_COUNT;

 if (rt_info.RT_COUNT<>0) then
 For i:=0 to rt_info.RT_COUNT-1 do
  begin
   RenderCmd.RT_INFO[i]:=rt_info.RT_INFO[i];

   RP_KEY.AddColorAt(RenderCmd.RT_INFO[i].attachment,
                     RenderCmd.RT_INFO[i].FImageInfo.cformat,
                     RenderCmd.RT_INFO[i].IMAGE_USAGE,
                     RenderCmd.RT_INFO[i].FImageInfo.params.samples);

  end;

 //rt_info.DB_ENABLE:=False;

 RenderCmd.DB_ENABLE:=rt_info.DB_ENABLE;

 if rt_info.DB_ENABLE then
 begin
  RenderCmd.DB_INFO:=rt_info.DB_INFO;

 // RenderCmd.DB_INFO.DEPTH_USAGE:=RenderCmd.DB_INFO.DEPTH_USAGE or TM_CLEAR;

  RP_KEY.AddDepthAt(RenderCmd.RT_COUNT, //add to last attachment id
                    RenderCmd.DB_INFO.FImageInfo.cformat,
                    RenderCmd.DB_INFO.DEPTH_USAGE,
                    RenderCmd.DB_INFO.STENCIL_USAGE);

  RP_KEY.SetZorderStage(RenderCmd.DB_INFO.zorder_stage);

 end;

 RP:=FetchRenderPass(CmdBuffer,@RP_KEY);

 GP_KEY.Clear;

 GP_KEY.FRenderPass :=RP;
 GP_KEY.FShaderGroup:=rt_info.ShaderGroup;

 GP_KEY.SetBlendInfo(rt_info.BLEND_INFO.logicOp,@rt_info.BLEND_INFO.blendConstants);

 GP_KEY.SetPrimType (TVkPrimitiveTopology(rt_info.PRIM_TYPE));
 GP_KEY.SetPrimReset(rt_info.PRIM_RESET);

 if (rt_info.VP_COUNT<>0) then
 For i:=0 to rt_info.VP_COUNT-1 do
  begin
   GP_KEY.AddVPort(rt_info.VPORT[i],rt_info.SCISSOR[i]);
  end;

 if (RenderCmd.RT_COUNT<>0) then
 For i:=0 to RenderCmd.RT_COUNT-1 do
  begin
   GP_KEY.AddBlend(RenderCmd.RT_INFO[i].blend);
  end;

 FAttrBuilder:=Default(TvAttrBuilder);
 rt_info.ShaderGroup.ExportAttrBuilder(FAttrBuilder,@rt_info.USERDATA);

 if not limits.VK_EXT_vertex_input_dynamic_state then
 begin
  GP_KEY.SetVertexInput(FAttrBuilder);
 end;

 GP_KEY.rasterizer   :=rt_info.RASTERIZATION;
 GP_KEY.multisampling:=rt_info.MULTISAMPLE;

 GP_KEY.SetProvoking(TVkProvokingVertexModeEXT(rt_info.PROVOKING));

 if rt_info.DB_ENABLE then
 begin
  GP_KEY.DepthStencil:=RenderCmd.DB_INFO.ds_state;
 end;

 GP:=FetchGraphicsPipeline(CmdBuffer,@GP_KEY);

 if limits.VK_KHR_imageless_framebuffer then
 begin
  FB_KEY:=Default(TvFramebufferImagelessKey);

  FB_KEY.SetRenderPass(RP);
  FB_KEY.SetSize(rt_info.SCREEN_SIZE);

  if (RenderCmd.RT_COUNT<>0) then
  For i:=0 to RenderCmd.RT_COUNT-1 do
   begin
    FB_KEY.AddImageAt(RenderCmd.RT_INFO[i].FImageInfo);
   end;

  if rt_info.DB_ENABLE then
  begin
   FB_KEY.AddImageAt(RenderCmd.DB_INFO.FImageInfo);
  end;
 end else
 begin
  FB_KEY2:=Default(TvFramebufferBindedKey);

  FB_KEY2.SetRenderPass(RP);
  FB_KEY2.SetSize(rt_info.SCREEN_SIZE);
 end;

 RenderCmd.FRenderPass:=RP;
 RenderCmd.FPipeline  :=GP;

 RenderCmd.FRenderArea:=rt_info.SCREEN_RECT;

 if limits.VK_KHR_imageless_framebuffer then
 begin
  FFramebuffer:=FetchFramebufferImageless(CmdBuffer,@FB_KEY);
  RenderCmd.FFramebuffer:=FFramebuffer;
 end;

 if (RenderCmd.RT_COUNT<>0) then
 For i:=0 to RenderCmd.RT_COUNT-1 do
  begin

   RenderCmd.AddClearColor(RenderCmd.RT_INFO[i].CLEAR_COLOR);

   ri:=FetchImage(CmdBuffer,
                  RenderCmd.RT_INFO[i].FImageInfo,
                  iu_attachment,
                  RenderCmd.RT_INFO[i].IMAGE_USAGE
                  );

   pm4_load_from(CmdBuffer,ri,RenderCmd.RT_INFO[i].IMAGE_USAGE);

   iv:=ri.FetchView(CmdBuffer,RenderCmd.RT_INFO[i].FImageView,iu_attachment);

   ri.PushBarrier(CmdBuffer,
                  ord(VK_ACCESS_TRANSFER_READ_BIT),
                  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                  ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

   ri.PushBarrier(CmdBuffer,
                  GetColorAccessMask(RenderCmd.RT_INFO[i].IMAGE_USAGE),
                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL {VK_IMAGE_LAYOUT_GENERAL},
                  ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                  ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) );

   //
   if limits.VK_KHR_imageless_framebuffer then
   begin
    RenderCmd.AddImageView(iv);
   end else
   begin
    FB_KEY2.AddImageView(iv);
   end;
   //

  end;

 if rt_info.DB_ENABLE then
 begin
  RenderCmd.AddClearColor(RenderCmd.DB_INFO.CLEAR_VALUE);

  ri:=FetchImage(CmdBuffer,
                 RenderCmd.DB_INFO.FImageInfo,
                 iu_depth,
                 RenderCmd.DB_INFO.DEPTH_USAGE
                 );

  //pm4_load_from(CmdBuffer,ri,RenderCmd.DB_INFO.DEPTH_USAGE);

  iv:=ri.FetchView(CmdBuffer,iu_depth);

  ri.PushBarrier(CmdBuffer,
                 ord(VK_ACCESS_TRANSFER_READ_BIT),
                 VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                 ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

  ri.PushBarrier(CmdBuffer,
                 GetColorAccessMask(RenderCmd.DB_INFO.DEPTH_USAGE),
                 VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                 ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                 ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) );

  //
  if limits.VK_KHR_imageless_framebuffer then
  begin
   RenderCmd.AddImageView(iv);
  end else
  begin
   FB_KEY2.AddImageView(iv);
  end;
  //

 end;

 if not limits.VK_KHR_imageless_framebuffer then
 begin
  FFramebuffer:=FetchFramebufferBinded(CmdBuffer,@FB_KEY2);
  RenderCmd.FFramebuffer:=FFramebuffer;
 end;

 ////////
 FUniformBuilder:=Default(TvUniformBuilder);
 rt_info.ShaderGroup.ExportUnifBuilder(FUniformBuilder,@rt_info.USERDATA);

 Prepare_Uniforms(FUniformBuilder,CmdBuffer);
 ////////

 if not CmdBuffer.BeginRenderPass(RenderCmd) then
 begin
  Writeln(stderr,'BeginRenderPass(FRenderCmd)');
  Assert(false,'BeginRenderPass(FRenderCmd)');
 end;

 CmdBuffer.SetVertexInput   (FAttrBuilder);
 CmdBuffer.BindVertexBuffers(FAttrBuilder);

 FDescriptorGroup:=nil;

 Bind_Uniforms(FUniformBuilder,
               FDescriptorGroup,
               rt_info.ShaderGroup,
               CmdBuffer);

 if (FDescriptorGroup<>nil) then
 begin
  CmdBuffer.BindSets(VK_PIPELINE_BIND_POINT_GRAPHICS,FDescriptorGroup);
 end;

end;

procedure pm4_Writeback(CmdBuffer:TvCmdBuffer;
                        RenderCmd:TvRenderTargets);
var
 i:Integer;

 ri:TvImage2;

 buf:TvHostBuffer;
 addr,size,offset:Ptruint;

 BufferImageCopy:TVkBufferImageCopy;
begin
 //write back

 if (RenderCmd.RT_COUNT<>0) then
 For i:=0 to RenderCmd.RT_COUNT-1 do
  if (RenderCmd.RT_INFO[i].attachment<>VK_ATTACHMENT_UNUSED) then
  begin

   ri:=FetchImage(CmdBuffer,
                  RenderCmd.RT_INFO[i].FImageInfo,
                  iu_attachment,
                  RenderCmd.RT_INFO[i].IMAGE_USAGE
                  );

   ri.PushBarrier(CmdBuffer,
                  ord(VK_ACCESS_TRANSFER_READ_BIT),
                  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                  ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

   if not get_dmem_ptr(ri.key.Addr,@addr,nil) then
   begin
    Assert(false,'addr:0x'+HexStr(ri.key.Addr)+' not in dmem!');
   end;

   //Writeln('0x',HexStr(ri.key.Addr),'->0x',HexStr(addr,16));

   size:=GetLinearSize(ri,true);

   buf:=FetchHostBuffer(CmdBuffer,
                        addr,
                        size,
                        ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT));

   offset:=buf.FAddr-addr;

   vkBufferMemoryBarrier(CmdBuffer.FCmdbuf,
                         buf.FHandle,
                         ord(VK_ACCESS_MEMORY_READ_BIT),
                         ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                         offset,
                         size,
                         ord(VK_PIPELINE_STAGE_HOST_BIT),
                         ord(VK_PIPELINE_STAGE_TRANSFER_BIT)
                         );

   BufferImageCopy:=Default(TVkBufferImageCopy);

   BufferImageCopy.bufferOffset     :=offset;
   BufferImageCopy.bufferRowLength  :=0;
   BufferImageCopy.bufferImageHeight:=0;
   BufferImageCopy.imageSubresource :=ri.GetSubresLayer;
   BufferImageCopy.imageExtent.Create(ri.key.params.width,
                                      ri.key.params.height,
                                      ri.key.params.depth);

   if true {align} then
   begin
    BufferImageCopy.bufferRowLength:=GetLinearAlignWidth(getFormatSize(ri.key.cformat),ri.key.params.width);
   end;

   vkCmdCopyImageToBuffer(CmdBuffer.FCmdbuf,
                          ri.FHandle,
                          VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                          buf.FHandle,
                          1,
                          @BufferImageCopy);

  end;

 if RenderCmd.DB_ENABLE then
 begin

  ri:=FetchImage(CmdBuffer,
                 RenderCmd.DB_INFO.FImageInfo,
                 iu_depth,
                 RenderCmd.DB_INFO.DEPTH_USAGE
                 );

  ri.PushBarrier(CmdBuffer,
                 ord(VK_ACCESS_TRANSFER_READ_BIT),
                 VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                 ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

  //
 end;

 //write back
end;

procedure pm4_Draw(node:p_pm4_node_draw);
var
 RenderCmd:TvRenderTargets;

 CmdBuffer:TvCmdBuffer;

 r:TVkResult;
begin

 StartFrameCapture;

 //
 if (FCmdPool=nil) then
 begin
  FCmdPool:=TvCmdPool.Create(VulkanApp.FGFamily);
 end;

 CmdBuffer:=TvCmdBuffer.Create(FCmdPool,RenderQueue);
 //CmdBuffer.submit_id:=submit_id;

 //

 RenderCmd:=TvRenderTargets.Create;

 pm4_DrawPrepare(node^.rt_info,
                 CmdBuffer,
                 RenderCmd);

 CmdBuffer.FinstanceCount:=node^.numInstances;
 CmdBuffer.FINDEX_TYPE   :=TVkIndexType(node^.INDEX_TYPE);

 case node^.ntype of
  ntDrawIndex2:
   begin
    CmdBuffer.DrawIndex2(Pointer(node^.indexBase),node^.indexCount);
   end;
  ntDrawIndexAuto:
   begin
    CmdBuffer.DrawIndexAuto(node^.indexCount);
   end;
  else;
   Assert(false);
 end;

 /////////

 CmdBuffer.EndRenderPass;

 pm4_Writeback(CmdBuffer,RenderCmd);

 r:=CmdBuffer.QueueSubmit;

 if (r<>VK_SUCCESS) then
 begin
  Assert(false,'QueueSubmit');
 end;

 Writeln('QueueSubmit:',r);

 r:=CmdBuffer.Wait(QWORD(-1));

 Writeln('CmdBuffer:',r);

 r:=RenderQueue.WaitIdle;
 Writeln('WaitIdle:',r);

 CmdBuffer.ReleaseResource;

 CmdBuffer.Free;
end;

procedure pm4_DispatchPrepare(node:p_pm4_node_DispatchDirect;
                              CmdBuffer:TvCmdBuffer);
var
 dst:PGPU_USERDATA;

 CP_KEY:TvComputePipelineKey;
 CP:TvComputePipeline2;

 FUniformBuilder:TvUniformBuilder;

 FDescriptorGroup:TvDescriptorGroup;
begin
 CP_KEY.FShaderGroup:=node^.ShaderGroup;
 CP:=FetchComputePipeline(CmdBuffer,@CP_KEY);

 ////////

 //hack
 dst:=Pointer(@node^.USER_DATA_CS)-Ptruint(@TGPU_USERDATA(nil^).A[vShaderStageCs]);

 FUniformBuilder:=Default(TvUniformBuilder);
 CP_KEY.FShaderGroup.ExportUnifBuilder(FUniformBuilder,dst);

 Prepare_Uniforms(FUniformBuilder,CmdBuffer);
 ////////

 if not CmdBuffer.BindCompute(CP) then
 begin
  Writeln(stderr,'BindCompute(CP)');
  Assert(false,'BindCompute(CP)');
 end;

 FDescriptorGroup:=nil;

 Bind_Uniforms(FUniformBuilder,
               FDescriptorGroup,
               CP_KEY.FShaderGroup,
               CmdBuffer);

 if (FDescriptorGroup<>nil) then
 begin
  CmdBuffer.BindSets(VK_PIPELINE_BIND_POINT_COMPUTE,FDescriptorGroup);
 end;

end;

procedure pm4_DispatchDirect(node:p_pm4_node_DispatchDirect);
var
 CmdBuffer:TvCmdBuffer;

 r:TVkResult;
begin

 StartFrameCapture;

 //
 if (FCmdPool=nil) then
 begin
  FCmdPool:=TvCmdPool.Create(VulkanApp.FGFamily);
 end;

 CmdBuffer:=TvCmdBuffer.Create(FCmdPool,RenderQueue);
 //CmdBuffer.submit_id:=submit_id;

 //
 CmdBuffer.EndRenderPass;

 pm4_DispatchPrepare(node,
                     CmdBuffer);

 CmdBuffer.DispatchDirect(node^.DIM_X,node^.DIM_Y,node^.DIM_Z);

 /////////

 r:=CmdBuffer.QueueSubmit;

 if (r<>VK_SUCCESS) then
 begin
  Assert(false,'QueueSubmit');
 end;

 Writeln('QueueSubmit:',r);

 r:=CmdBuffer.Wait(QWORD(-1));

 Writeln('CmdBuffer:',r);

 r:=RenderQueue.WaitIdle;
 Writeln('WaitIdle:',r);

 CmdBuffer.ReleaseResource;

 CmdBuffer.Free;
end;

function mul_div_u64(m,d,v:QWORD):QWORD; sysv_abi_default; assembler; nostackframe;
asm
 movq v,%rax
 mulq m
 divq d
end;

procedure pm4_EventWriteEop(node:p_pm4_node_EventWriteEop;me:p_pm4_me);
var
 curr,diff:QWORD;
begin
 //EndFrameCapture;

 curr:=md_rdtsc_unit;
 diff:=curr-me^.rel_time;

 if (node^.addr<>nil) then
 Case node^.dataSel of
  //
  EVENTWRITEEOP_DATA_SEL_DISCARD:;

   //32bit data
  EVENTWRITEEOP_DATA_SEL_SEND_DATA32:PDWORD(node^.addr)^:=node^.data;

   //64bit data
  EVENTWRITEEOP_DATA_SEL_SEND_DATA64:PQWORD(node^.addr)^:=node^.data;

    //system 100Mhz global clock. (relative time)
  EVENTWRITEEOP_DATA_SEL_SEND_GPU_CLOCK:PQWORD(node^.addr)^:=mul_div_u64(100*1000000,UNIT_PER_SEC,diff);

    //GPU 800Mhz clock.           (relative time)
  EVENTWRITEEOP_DATA_SEL_SEND_CP_PERFCOUNTER:PQWORD(node^.addr)^:=mul_div_u64(800*1000000,UNIT_PER_SEC,diff);

  else
   Assert(false,'pm4_EventWriteEop');
 end;

 if (node^.intSel<>0) then
 begin
  me^.knote_eventid($40,0,curr*NSEC_PER_UNIT,0); //(absolute time) (freq???)
 end;
end;

procedure pm4_SubmitFlipEop(node:p_pm4_node_SubmitFlipEop;me:p_pm4_me);
var
 curr:QWORD;
begin
 //EndFrameCapture;

 if (me^.on_submit_flip_eop<>nil) then
 begin
  me^.on_submit_flip_eop(node^.eop_value);
 end;

 curr:=md_rdtsc_unit;

 if (node^.intSel<>0) then
 begin
  me^.knote_eventid($40,0,curr*NSEC_PER_UNIT,0); //(absolute time) (freq???)
 end;
end;

procedure pm4_EventWriteEos(node:p_pm4_node_EventWriteEos;me:p_pm4_me);
begin

 if (node^.addr<>nil) then
 Case node^.command of

   //32bit data
  EVENT_WRITE_EOS_CMD_STORE_32BIT_DATA_TO_MEMORY:PDWORD(node^.addr)^:=node^.data;

  else
   Assert(false,'pm4_EventWriteEos');
 end;

 //interrupt???
end;

procedure pm4_WriteData(node:p_pm4_node_WriteData);
var
 addr:PDWORD;
begin

 case node^.dstSel of
  WRITE_DATA_DST_SEL_MEMORY_SYNC,  //writeDataInline
  WRITE_DATA_DST_SEL_TCL2,         //writeDataInlineThroughL2
  WRITE_DATA_DST_SEL_MEMORY_ASYNC:
    if (node^.dst<>0) then
    begin
     addr:=Pointer(node^.dst);
     Move(node^.src^,addr^,node^.num_dw*SizeOf(DWORD));
    end;
  else
    Assert(false,'WriteData: dstSel=0x'+HexStr(node^.dstSel,1));
 end;

end;

Function me_test_mem(node:p_pm4_node_WaitRegMem):Boolean;
var
 val,ref:DWORD;
begin
 val:=PDWORD(node^.pollAddr)^ and node^.mask;
 ref:=node^.refValue;
 Case node^.compareFunc of
  WAIT_REG_MEM_FUNC_ALWAYS       :Result:=True;
  WAIT_REG_MEM_FUNC_LESS         :Result:=(val<ref);
  WAIT_REG_MEM_FUNC_LESS_EQUAL   :Result:=(val<=ref);
  WAIT_REG_MEM_FUNC_EQUAL        :Result:=(val=ref);
  WAIT_REG_MEM_FUNC_NOT_EQUAL    :Result:=(val<>ref);
  WAIT_REG_MEM_FUNC_GREATER_EQUAL:Result:=(val>ref);
  WAIT_REG_MEM_FUNC_GREATER      :Result:=(val>=ref);
  else
   Assert(false,'me_test_mem');
 end;
end;

procedure pm4_WaitRegMem(node:p_pm4_node_WaitRegMem);
begin

 while not me_test_mem(node) do
 begin
  sleep(1);
 end;

end;

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl;
var
 stream:p_pm4_stream;
 node:p_pm4_node;
begin

 if use_renderdoc_capture then
 begin
  renderdoc.LoadRenderDoc;
  renderdoc.UnloadCrashHandler;
 end;

 repeat

  //start relative timer
  if (me^.rel_time=0) then
  begin
   me^.rel_time:=md_rdtsc_unit;
  end;
  //

  stream:=nil;
  if me^.queue.Pop(stream) then
  begin
   //
   node:=stream^.First;
   while (node<>nil) do
   begin
    Writeln('+',node^.ntype);

    case node^.ntype of
     ntDrawIndex2    :pm4_Draw          (Pointer(node));
     ntDrawIndexAuto :pm4_Draw          (Pointer(node));
     ntDispatchDirect:pm4_DispatchDirect(Pointer(node));
     ntEventWriteEop :pm4_EventWriteEop (Pointer(node),me);
     ntSubmitFlipEop :pm4_SubmitFlipEop (Pointer(node),me);
     ntEventWriteEos :pm4_EventWriteEos (Pointer(node),me);
     ntWriteData     :pm4_WriteData     (Pointer(node));
     ntWaitRegMem    :pm4_WaitRegMem    (Pointer(node));
     else
    end;

    //
    node:=stream^.Next(node);
   end;

   me^.free_stream(stream);

   //EndFrameCapture;

   //
   Continue;
  end;

  EndFrameCapture;

  me^.rel_time:=0; //reset time
  //
  if (me^.on_idle<>nil) then
  begin
   me^.on_idle();
  end;
  //
  RTLEventWaitFor(me^.event);
 until false;

end;

end.

