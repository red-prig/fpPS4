unit vImageManager;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 g23tree,
 Vulkan,
 vDevice,
 vDependence,
 vMemory,
 vImage,
 vCmdBuffer,
 kern_proc,
 vm_map,
 vm_tracking_map{,
 vImageTiling};

{
 image_usage -> attachment,sampled,storage
 read_mode   -> not_need,started,keep,changed
 write_back  -> not_need,keep,started,finished

 cmd R/W -> cmd RO
  |
  v
 cmd R/W -> cmd RO
  |
  v
}

type
 TvImageView2Compare=object
  function c(a,b:PvImageViewKey):Integer; static;
 end;

 TvImage2=class;

 TvImageView2=class(TvImageView)
  Parent:TvImage2;
  key:TvImageViewKey;
  //
  Barrier:TvImageBarrier;
  //
  procedure   PushBarrier(cmd:TvCustomCmdBuffer;
                          dstAccessMask:TVkAccessFlags;
                          newImageLayout:TVkImageLayout;
                          dstStageMask:TVkPipelineStageFlags);
  Function    GetSubresRange(cformat:TVkFormat=VK_FORMAT_UNDEFINED):TVkImageSubresourceRange;
  Function    GetSubresLayer(cformat:TVkFormat=VK_FORMAT_UNDEFINED):TVkImageSubresourceLayers;
 end;

 TvImageView2Set=specialize T23treeSet<PvImageViewKey,TvImageView2Compare>;

 TvCustomImage2=class(TvCustomImage)
  //
  key :TvImageKey;
  //
  size:Ptruint;
  tobj:p_vm_track_object;
  //
  ref_trig:Ptruint;
  ref_load:Ptruint;
  //
  Parent     :TvCustomImage2;
  DepthOnly  :TvCustomImage2;
  StencilOnly:TvCustomImage2;
  //
  lock:Pointer;
  //
  Constructor Create;
  Destructor  Destroy; override;
  procedure   assign_vm_track; virtual;
  Function    GetSubresRange:TVkImageSubresourceRange;  virtual;
  Function    GetSubresLayer:TVkImageSubresourceLayers; virtual;
  function    _FetchView(cmd:TvCustomCmdBuffer;const F:TvImageViewKey;usage:TVkFlags):TvImageView2; virtual; abstract;
  function    FetchView(cmd:TvCustomCmdBuffer;const F:TvImageViewKey;usage:t_image_usage):TvImageView2;
  function    FetchView(cmd:TvCustomCmdBuffer;usage:t_image_usage):TvImageView2;
  procedure   PushBarrier(cmd:TvCustomCmdBuffer;
                          dstAccessMask:TVkAccessFlags;
                          newImageLayout:TVkImageLayout;
                          dstStageMask:TVkPipelineStageFlags); virtual; abstract;
  procedure   ForceBarrier(dstAccessMask:TVkAccessFlags;
                           newImageLayout:TVkImageLayout;
                           dstStageMask:TVkPipelineStageFlags); virtual; abstract;
 end;

 TvChildImage2=class(TvCustomImage2)
  procedure   FreeHandle; override;
  function    _FetchView(cmd:TvCustomCmdBuffer;const F:TvImageViewKey;usage:TVkFlags):TvImageView2; override;
  procedure   PushBarrier(cmd:TvCustomCmdBuffer;
                          dstAccessMask:TVkAccessFlags;
                          newImageLayout:TVkImageLayout;
                          dstStageMask:TVkPipelineStageFlags); override;
  procedure   ForceBarrier(dstAccessMask:TVkAccessFlags;
                           newImageLayout:TVkImageLayout;
                           dstStageMask:TVkPipelineStageFlags); override;
  function    Acquire(Sender:TObject):Boolean; override;
  procedure   Release(Sender:TObject);         override;
 end;

 TvImage2=class(TvCustomImage2)
  //
  FUsage:s_image_usage;
  //
  FViews:TvImageView2Set;
  //
  Barrier:TvImageBarrier;
  //
  FLastCmd:TvCustomCmdBuffer;
  FDeps:TObjectSetLock;
  //
  submit_id:ptruint;
  hash:qword;
  //
  Constructor Create;
  Destructor  Destroy; override;
  function    GetImageInfo:TVkImageCreateInfo; override;
  function    _FetchView(cmd:TvCustomCmdBuffer;const F:TvImageViewKey;usage:TVkFlags):TvImageView2; override;
  procedure   PushBarrier(cmd:TvCustomCmdBuffer;
                          dstAccessMask:TVkAccessFlags;
                          newImageLayout:TVkImageLayout;
                          dstStageMask:TVkPipelineStageFlags); override;
  procedure   ForceBarrier(dstAccessMask:TVkAccessFlags;
                           newImageLayout:TVkImageLayout;
                           dstStageMask:TVkPipelineStageFlags); override;
  function    Acquire(Sender:TObject):Boolean; override;
  procedure   Release(Sender:TObject);         override;
 end;

 TvDepthStencilImage2=class(TvImage2)
  procedure   FreeHandle; override;
  function    Compile(ext:Pointer):Boolean; override;
  procedure   assign_vm_track; override;
  Destructor  Destroy; override;
 end;

function FetchImage(cmd:TvCustomCmdBuffer;const F:TvImageKey;usage:s_image_usage):TvImage2;
function FindImage(cmd:TvCustomCmdBuffer;Addr:Pointer;cformat:TVkFormat):TvImage2;

Function get_image_size(const key:TvImageKey):Ptruint; external name 'tiling_get_image_size';

const
 img_ext:TVkExternalMemoryImageCreateInfo=(
  sType:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO;
  pNext:nil;
  handleTypes:ord(VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT);
 );

implementation

uses
 kern_rwlock;

type
 TvImageKeyCompare=object
  function c(a,b:PvImageKey):Integer; static;
 end;

 _TvImage2Set=specialize T23treeSet<PvImageKey,TvImageKeyCompare>;
 TvImage2Set=object(_TvImage2Set)
  lock:Pointer;
  Procedure Lock_wr;
  Procedure Unlock_wr;
 end;

var
 FImage2Set:TvImage2Set;

Procedure TvImage2Set.Lock_wr;
begin
 rw_wlock(lock);
end;

Procedure TvImage2Set.Unlock_wr;
begin
 rw_wunlock(lock);
end;

function TvImageKeyCompare.c(a,b:PvImageKey):Integer;
begin
 //1 Addr
 Result:=Integer(a^.Addr>b^.Addr)-Integer(a^.Addr<b^.Addr);
 if (Result<>0) then Exit;
 //1 Stencil
 Result:=Integer(a^.Addr2>b^.Addr2)-Integer(a^.Addr2<b^.Addr2);
 if (Result<>0) then Exit;
 //2 cformat
 Result:=Integer(a^.cformat>b^.cformat)-Integer(a^.cformat<b^.cformat);
 if (Result<>0) then Exit;
 //3 params
 Result:=CompareByte(a^.params,b^.params,SizeOf(TvImageKey.params));
end;

function TvImageView2Compare.c(a,b:PvImageViewKey):Integer;
begin
 Result:=CompareByte(a^,b^,SizeOf(TvImageViewKey));
end;

//

function on_destroy(handle:Pointer):Integer; SysV_ABI_CDecl;
var
 image:TvCustomImage2;
begin
 image:=TvCustomImage2(handle);
 image.tobj:=nil;

 //
 Result:=DO_DELETE;
end;

function on_trigger(handle:Pointer;start,__end:QWORD):Integer; SysV_ABI_CDecl;
var
 image:TvCustomImage2;
begin
 Result:=DO_NOTHING;

 image:=TvCustomImage2(handle);

 if (__end>QWORD(image.key.Addr)) and (start<(QWORD(image.key.Addr)+image.size)) then
 begin
  //Writeln('on_trigger image');

  System.InterlockedIncrement64(image.ref_trig);

  Result:=DO_INCREMENT;
 end;

end;

//

Constructor TvCustomImage2.Create;
begin
 inherited;
 ref_trig:=1;
end;

Destructor TvCustomImage2.Destroy;
begin
 if (tobj<>nil) then
 begin
  vm_map_track_remove(p_proc.p_vmspace,tobj);
 end;

 inherited;
end;

procedure TvCustomImage2.assign_vm_track;
var
 start,__end:QWORD;
begin
 if (tobj<>nil) then Exit;

 rw_wlock(lock);

 if (tobj=nil) then
 begin
  size:=get_image_size(key);

  start:=QWORD(key.Addr);
  __end:=start+size;

  tobj:=vm_track_object_allocate(Pointer(self),start,__end,H_GPU_IMAGE,PAGE_TRACK_W);
  tobj^.on_destroy:=@on_destroy;
  tobj^.on_trigger:=@on_trigger;

  vm_map_track_insert(p_proc.p_vmspace,tobj);

  vm_track_object_deallocate(tobj);
 end;

 rw_wunlock(lock)
end;

Function TvCustomImage2.GetSubresRange:TVkImageSubresourceRange;
begin
 Result:=Default(TVkImageSubresourceRange);
 Result.aspectMask:=GetAspectMaskByFormat(key.cformat);
 Result.levelCount:=key.params.mipLevels;
 Result.layerCount:=key.params.arrayLayers;
end;

Function TvCustomImage2.GetSubresLayer:TVkImageSubresourceLayers;
begin
 Result:=Default(TVkImageSubresourceLayers);
 Result.aspectMask    :=GetAspectMaskByFormat(key.cformat);
 Result.mipLevel      :=0;
 Result.baseArrayLayer:=0;
 Result.layerCount    :=key.params.arrayLayers;
end;

//

procedure TvChildImage2.FreeHandle;
begin
 FHandle:=VK_NULL_HANDLE;
end;

function TvChildImage2._FetchView(cmd:TvCustomCmdBuffer;const F:TvImageViewKey;usage:TVkFlags):TvImageView2;
begin
 Result:=Parent._FetchView(cmd,F,usage);
end;

procedure TvChildImage2.PushBarrier(cmd:TvCustomCmdBuffer;
                                    dstAccessMask:TVkAccessFlags;
                                    newImageLayout:TVkImageLayout;
                                    dstStageMask:TVkPipelineStageFlags);
begin
 Parent.PushBarrier(cmd,
                    dstAccessMask,
                    newImageLayout,
                    dstStageMask);
end;

procedure TvChildImage2.ForceBarrier(dstAccessMask:TVkAccessFlags;
                                     newImageLayout:TVkImageLayout;
                                     dstStageMask:TVkPipelineStageFlags);
begin
 Parent.ForceBarrier(dstAccessMask,
                     newImageLayout,
                     dstStageMask);
end;

function TvChildImage2.Acquire(Sender:TObject):Boolean;
begin
 Result:=Parent.Acquire(Sender);
end;

procedure TvChildImage2.Release(Sender:TObject);
begin
 Parent.Release(Sender);
end;

//

procedure TvImageView2.PushBarrier(cmd:TvCustomCmdBuffer;
                                   dstAccessMask:TVkAccessFlags;
                                   newImageLayout:TVkImageLayout;
                                   dstStageMask:TVkPipelineStageFlags);
begin
 if (Parent=nil) then Exit;
 Parent.PushBarrier(cmd,dstAccessMask,newImageLayout,dstStageMask);

 if (cmd=nil) then Exit;
 if (not cmd.BeginCmdBuffer) then Exit;

 if Barrier.Push(cmd.FCmdbuf,
                 Parent.FHandle,
                 GetSubresRange,
                 dstAccessMask,
                 newImageLayout,
                 dstStageMask) then
 begin
  Inc(cmd.cmd_count);
 end;

end;

Function TvImageView2.GetSubresRange(cformat:TVkFormat=VK_FORMAT_UNDEFINED):TVkImageSubresourceRange;
begin
 if (cformat=VK_FORMAT_UNDEFINED) then cformat:=key.cformat;

 Result:=Default(TVkImageSubresourceRange);
 Result.aspectMask    :=GetAspectMaskByFormat(cformat);
 Result.baseMipLevel  :=key.base_level;
 Result.levelCount    :=key.last_level-key.base_level+1;
 Result.baseArrayLayer:=key.base_array;
 Result.layerCount    :=key.last_array-key.base_array+1;
end;

Function TvImageView2.GetSubresLayer(cformat:TVkFormat=VK_FORMAT_UNDEFINED):TVkImageSubresourceLayers;
begin
 if (cformat=VK_FORMAT_UNDEFINED) then cformat:=key.cformat;

 Result:=Default(TVkImageSubresourceLayers);
 Result.aspectMask    :=GetAspectMaskByFormat(cformat);
 Result.mipLevel      :=key.base_level;
 Result.baseArrayLayer:=key.base_array;
 Result.layerCount    :=key.last_array-key.base_array+1;
end;

Constructor TvImage2.Create;
begin
 inherited;
 Barrier.Init;
 ref_trig:=1;
end;

Destructor TvImage2.Destroy;
var
 i:TvImageView2Set.Iterator;
 t:TvImageView2;
begin

 i:=FViews.cbegin;
 While (i.Item<>nil) do
 begin
  t:=TvImageView2(ptruint(i.Item^)-ptruint(@TvImageView2(nil).key));
  t.Release(Self);
  i.Next;
 end;
 FViews.Free;

 inherited;
end;

function TvImage2.GetImageInfo:TVkImageCreateInfo;
begin
 Result:=Default(TVkImageCreateInfo);
 Result.sType        :=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
 Result.flags        :=GET_VK_IMAGE_CREATE_DEFAULT(key.cformat);
 Result.imageType    :=TVkImageType(key.params.itype);
 Result.format       :=key.cformat;
 Result.extent.Create(key.params.width,key.params.height,key.params.depth);
 Result.mipLevels    :=key.params.mipLevels;
 Result.arrayLayers  :=key.params.arrayLayers;
 Result.samples      :=TVkSampleCountFlagBits(key.params.samples);
 Result.tiling       :=VK_IMAGE_TILING_OPTIMAL;
 Result.usage        :=GET_VK_IMAGE_USAGE_DEFAULT(key.cformat);
 Result.initialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
end;

function TvImage2._FetchView(cmd:TvCustomCmdBuffer;const F:TvImageViewKey;usage:TVkFlags):TvImageView2;
var
 key2:TvImageViewKey;

 i:TvImageView2Set.Iterator;
 t:TvImageView2;

 cinfo:TVkImageViewCreateInfo;
 uinfo:TVkImageViewUsageCreateInfo;

 FView:TVkImageView;
 r:TVkResult;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 if (FHandle=VK_NULL_HANDLE) then Exit;

 if (usage=0) then
 begin
  usage:=GET_VK_IMAGE_USAGE_DEFAULT(F.cformat);
 end;

 key2:=F;
 key2.fusage:=usage;

 rw_wlock(lock);

 t:=nil;
 i:=FViews.find(@key2);
 if (i.Item<>nil) then
 begin
  t:=TvImageView2(ptruint(i.Item^)-ptruint(@TvImageView2(nil).key));
 end else
 begin
  cinfo:=Default(TVkImageViewCreateInfo);
  cinfo.sType       :=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
  cinfo.image       :=FHandle;
  cinfo.viewType    :=TVkImageViewType(F.vtype);
  cinfo.format      :=F.cformat;
  cinfo.components.r:=TVkComponentSwizzle(F.dstSel.r);
  cinfo.components.g:=TVkComponentSwizzle(F.dstSel.g);
  cinfo.components.b:=TVkComponentSwizzle(F.dstSel.b);
  cinfo.components.a:=TVkComponentSwizzle(F.dstSel.a);

  cinfo.subresourceRange.aspectMask    :=GetAspectMaskByFormat(F.cformat);
  cinfo.subresourceRange.baseMipLevel  :=F.base_level;
  cinfo.subresourceRange.levelCount    :=F.last_level-F.base_level+1;
  cinfo.subresourceRange.baseArrayLayer:=F.base_array;
  cinfo.subresourceRange.layerCount    :=F.last_array-F.base_array+1;

  cinfo.format:=vkFixFormatSupport(cinfo.format,VK_IMAGE_TILING_OPTIMAL,usage);

  uinfo:=Default(TVkImageViewUsageCreateInfo);
  uinfo.sType:=VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO;
  uinfo.usage:=usage;

  cinfo.pNext:=@uinfo;

  FView:=VK_NULL_HANDLE;
  r:=vkCreateImageView(Device.FHandle,@cinfo,nil,@FView);
  if (r<>VK_SUCCESS) then
  begin
   rw_wunlock(lock);
   Writeln(StdErr,'vkCreateImageView:',r);
   Exit;
  end;

  t:=TvImageView2.Create;
  t.FHandle:=FView;
  t.Parent :=Self;
  t.key    :=key2;

  t.Acquire(Self); //map ref
  FViews.Insert(@t.key);
 end;

 cmd.RefTo(t);

 rw_wunlock(lock);

 Result:=t;
end;

function TvCustomImage2.FetchView(cmd:TvCustomCmdBuffer;const F:TvImageViewKey;usage:t_image_usage):TvImageView2;
var
 tmp:TvImageViewKey;
begin
 case usage of
  iu_storage:
   begin
    tmp:=F;
    tmp.cformat:=GET_VK_FORMAT_STORAGE(F.cformat);
    Result:=_FetchView(cmd,tmp,ord(VK_IMAGE_USAGE_STORAGE_BIT));
   end;
  else
   Result:=_FetchView(cmd,F,0);
 end;
end;

function TvCustomImage2.FetchView(cmd:TvCustomCmdBuffer;usage:t_image_usage):TvImageView2;
var
 F:TvImageViewKey;
begin
 if (Self=nil) then Exit;

 F:=Default(TvImageViewKey);
 F.cformat:=key.cformat;

 Case TVkImageType(key.params.itype) of
  VK_IMAGE_TYPE_1D:
   begin
    if (key.params.arrayLayers>1) then
     F.vtype:=ord(VK_IMAGE_VIEW_TYPE_1D_ARRAY)
    else
     F.vtype:=ord(VK_IMAGE_VIEW_TYPE_1D);
   end;
  VK_IMAGE_TYPE_2D:
   begin
    if (key.params.arrayLayers>1) then
     F.vtype:=ord(VK_IMAGE_VIEW_TYPE_2D_ARRAY)
    else
     F.vtype:=ord(VK_IMAGE_VIEW_TYPE_2D);
    //VK_IMAGE_VIEW_TYPE_CUBE
    //VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
   end;
  VK_IMAGE_TYPE_3D:F.vtype:=ord(VK_IMAGE_VIEW_TYPE_3D);
 end;

 F.last_level:=key.params.mipLevels  -1;
 F.last_array:=key.params.arrayLayers-1;

 case usage of
  iu_storage:
   begin
    F.cformat:=GET_VK_FORMAT_STORAGE(F.cformat);
    Result:=_FetchView(cmd,F,ord(VK_IMAGE_USAGE_STORAGE_BIT));
   end;
  else
   Result:=_FetchView(cmd,F,0);
 end;

end;

procedure TvImage2.PushBarrier(cmd:TvCustomCmdBuffer;
                               dstAccessMask:TVkAccessFlags;
                               newImageLayout:TVkImageLayout;
                               dstStageMask:TVkPipelineStageFlags);
begin
 if (cmd=nil) then Exit;
 if (not cmd.BeginCmdBuffer) then Exit;

 rw_wlock(lock);

 if Barrier.Push(cmd.FCmdbuf,
                 FHandle,
                 GetSubresRange,
                 dstAccessMask,
                 newImageLayout,
                 dstStageMask) then
 begin
  Inc(cmd.cmd_count);
 end;

 rw_wunlock(lock);
end;

procedure TvImage2.ForceBarrier(dstAccessMask:TVkAccessFlags;
                                newImageLayout:TVkImageLayout;
                                dstStageMask:TVkPipelineStageFlags);
begin
 rw_wlock(lock);

 Barrier.AccessMask:=dstAccessMask;
 Barrier.ImgLayout :=newImageLayout;
 Barrier.StageMask :=dstStageMask;

 rw_wunlock(lock);
end;

function TvImage2.Acquire(Sender:TObject):Boolean;
begin
 Result:=inherited Acquire(Sender);
 if Result and (Sender<>nil) then
 begin
  if FDeps.Insert(Sender) then
  begin
   if Sender.InheritsFrom(TvCustomCmdBuffer) then
   begin
    FLastCmd:=TvCustomCmdBuffer(Sender);
   end;
  end;
 end;
end;

procedure TvImage2.Release(Sender:TObject);
begin
 if (Sender<>nil) then
 begin
  FDeps.delete(Sender);
  if (FLastCmd=Sender) then
  begin
   FLastCmd:=nil;
  end;
 end;
 inherited Release(Sender);
end;

//

procedure TvDepthStencilImage2.FreeHandle;
begin
 if (DepthOnly<>nil) then
 begin
  DepthOnly.FHandle:=VK_NULL_HANDLE;
 end;

 if (StencilOnly<>nil) then
 begin
  StencilOnly.FHandle:=VK_NULL_HANDLE;
 end;

 inherited;
end;

function TvDepthStencilImage2.Compile(ext:Pointer):Boolean;
begin
 Result:=inherited Compile(ext);
 //
 if Result then
 begin
  if (DepthOnly<>nil) then
  begin
   DepthOnly.FHandle:=FHandle;
  end;

  if (StencilOnly<>nil) then
  begin
   StencilOnly.FHandle:=FHandle;
  end;
 end;
end;

procedure TvDepthStencilImage2.assign_vm_track;
begin
 if (DepthOnly<>nil) then
 begin
  DepthOnly.assign_vm_track;
 end;

 if (StencilOnly<>nil) then
 begin
  StencilOnly.assign_vm_track;
 end;
end;

Destructor TvDepthStencilImage2.Destroy;
begin
 if (DepthOnly<>nil) and
    (DepthOnly<>Self) then
 begin
  FreeAndNil(DepthOnly);
 end;

 if (StencilOnly<>nil) and
    (StencilOnly<>Self) then
 begin
  FreeAndNil(StencilOnly);
 end;

 inherited;
end;

//

function _Find(const F:TvImageKey):TvImage2;
var
 i:TvImage2Set.Iterator;
begin
 Result:=nil;
 i:=FImage2Set.find(@F);
 if (i.Item<>nil) then
 begin
  Result:=TvImage2(ptruint(i.Item^)-ptruint(@TvImage2(nil).key));
 end;
end;

procedure print_img_usage(usage:TVkFlags);
begin
 if (usage and ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT            ))<>0 then Write(' TRANSFER_SRC');
 if (usage and ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT            ))<>0 then Write(' TRANSFER_DST');
 if (usage and ord(VK_IMAGE_USAGE_SAMPLED_BIT                 ))<>0 then Write(' SAMPLED');
 if (usage and ord(VK_IMAGE_USAGE_STORAGE_BIT                 ))<>0 then Write(' STORAGE');
 if (usage and ord(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT        ))<>0 then Write(' COLOR_ATTACHMENT');
 if (usage and ord(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT))<>0 then Write(' DEPTH_STENCIL_ATTACHMENT');
 if (usage and ord(VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT    ))<>0 then Write(' TRANSIENT_ATTACHMENT');
 if (usage and ord(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT        ))<>0 then Write(' INPUT_ATTACHMENT');
end;

function _NewImage(const F:TvImageKey;usage:s_image_usage):TvImage2;
begin
 Case F.cformat of
  //stencil
  VK_FORMAT_S8_UINT:
   begin
    Result:=TvDepthStencilImage2.Create;
    Result.key   :=F;
    Result.FUsage:=usage;

    Result.StencilOnly:=TvChildImage2.Create;
    Result.StencilOnly.key   :=GetStencilOnly(F);
    Result.StencilOnly.Parent:=Result;
   end;
  //depth
  VK_FORMAT_D16_UNORM,
  VK_FORMAT_X8_D24_UNORM_PACK32,
  VK_FORMAT_D32_SFLOAT:
   begin
    Result:=TvImage2.Create;
    Result.key   :=F;
    Result.FUsage:=usage;
    //
    Result.Parent   :=Result;
    Result.DepthOnly:=Result;
   end;
  //depth stencil
  VK_FORMAT_D16_UNORM_S8_UINT,
  VK_FORMAT_D24_UNORM_S8_UINT,
  VK_FORMAT_D32_SFLOAT_S8_UINT:
   begin
    Result:=TvDepthStencilImage2.Create;
    Result.key   :=F;
    Result.FUsage:=usage;

    Result.DepthOnly:=TvChildImage2.Create;
    Result.DepthOnly.key   :=GetDepthOnly(F);
    Result.DepthOnly.Parent:=Result;

    Result.StencilOnly:=TvChildImage2.Create;
    Result.StencilOnly.key   :=GetStencilOnly(F);
    Result.StencilOnly.Parent:=Result;
   end;
  else
   begin
    Result:=TvImage2.Create;
    Result.key   :=F;
    Result.FUsage:=usage;
   end;
 end;

end;

function _FetchImage(const F:TvImageKey;usage:s_image_usage):TvImage2;
label
 _repeat;
var
 t:TvImage2;
 Fdevc:TvPointer;
begin
 Result:=nil;

 _repeat:

 t:=_Find(F);

 if (t<>nil) then
 begin
  if t.Acquire(nil) then
  begin
   t.FUsage:=t.FUsage+usage;
  end else
  begin
   //mem is deleted, free img
   FImage2Set.delete(@t.key);
   t._Release(nil); //map ref
   t:=nil;
   goto _repeat;
  end;
 end else
 begin
  t:=_NewImage(F,usage);

  if not t.Compile(nil) then
  begin
   FreeAndNil(t);
  end else
  begin

   Fdevc:=MemManager.FetchMemory(
     t.GetRequirements,
     V_PROP_DEVICE_LOCAL or V_PROP_BEST_FIT
   );

   t.BindMem(Fdevc);

   if FImage2Set.Insert(@t.key) then
   begin
    t._Acquire(nil); //map ref
   end;

  end;

 end;

 Result:=t;
end;

function _FindImage(Addr:Pointer;cformat:TVkFormat):TvImage2;
var
 i:TvImage2Set.Iterator;
 t:TvImage2;

 F:TvImageKey;
begin
 F:=Default(TvImageKey);
 F.Addr:=Addr;
 F.cformat:=cformat;

 t:=nil;
 i:=FImage2Set.find_be(@F);
 if (i.Item<>nil) then
 begin
  t:=TvImage2(ptruint(i.Item^)-ptruint(@TvImage2(nil).key));
  if (t.key.Addr<>Addr) then t:=nil;
 end;

 Result:=t;
end;

function FetchImage(cmd:TvCustomCmdBuffer;const F:TvImageKey;usage:s_image_usage):TvImage2;
begin
 FImage2Set.Lock_wr;

 Result:=_FetchImage(F,usage); // <- Acquire(nil)/FetchMemory

 cmd.RefTo(Result);

 if (Result<>nil) then
 begin
  Result.Release(nil); //<- Acquire(nil)/FetchMemory
 end;

 FImage2Set.Unlock_wr;
end;

function FindImage(cmd:TvCustomCmdBuffer;Addr:Pointer;cformat:TVkFormat):TvImage2;
begin
 FImage2Set.Lock_wr;

 Result:=_FindImage(Addr,cformat);

 cmd.RefTo(Result);

 FImage2Set.Unlock_wr;
end;


end.



