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

 t_change_rate=object
  state  :Integer;
  trigger:Integer;
  planned:Integer;
  procedure mark_init;
  function  need_read:Boolean;
 end;

 TvCustomImage2=class(TvCustomImage)
  //
  key :TvImageKey;
  //
  size:Ptruint;
  tobj:p_vm_track_object;
  //
  change_rate:t_change_rate;
  //
  Parent     :TvCustomImage2;
  DepthOnly  :TvCustomImage2;
  StencilOnly:TvCustomImage2;
  //
  lock:Pointer;
  //
  Destructor  Destroy; override;
  procedure   restore_vm_track; virtual;
  procedure   assign_vm_track;  virtual;
  procedure   mark_init;
  function    get_change_rate:t_change_rate;
  procedure   apply_change_rate(r:t_change_rate);
  Function    IsDepthAndStencil:Boolean;
  Function    GetSubresRange:TVkImageSubresourceRange;  virtual;
  Function    GetSubresLayer:TVkImageSubresourceLayers; virtual;
  function    FetchViewRaw(cmd:TvCustomCmdBuffer;const F:TvImageViewKey;usage:TVkFlags):TvImageView2; virtual; abstract;
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
  function    FetchViewRaw(cmd:TvCustomCmdBuffer;const F:TvImageViewKey;usage:TVkFlags):TvImageView2; override;
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
  function    FetchViewRaw(cmd:TvCustomCmdBuffer;const F:TvImageViewKey;usage:TVkFlags):TvImageView2; override;
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
  procedure   restore_vm_track; override;
  procedure   assign_vm_track;  override;
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
 Result:=CompareNormalized(a^,b^);
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

function on_trigger(handle:Pointer;mode:T_TRIGGER_MODE):Integer; SysV_ABI_CDecl;
var
 image:TvCustomImage2;
 i:Integer;
begin
 Result:=DO_NOTHING;

 image:=TvCustomImage2(handle);

 //Writeln('on_trigger image');

 case mode of
  M_CPU_WRITE,
  M_DMEM_WRITE:
    //direct
    begin
     System.InterlockedIncrement(image.change_rate.trigger);
    end;
  M_GPU_PLANNED://planned
    begin
     System.InterlockedIncrement(image.change_rate.planned);
    end;
  M_GPU_APPLY://differed
    begin
     i:=System.InterlockedExchangeAdd(image.change_rate.planned,0);

     System.InterlockedExchangeAdd(image.change_rate.trigger,+i);
     System.InterlockedExchangeAdd(image.change_rate.planned,-i);
    end;
  else;
 end;

 Result:=DO_INCREMENT;
end;

//

Destructor TvCustomImage2.Destroy;
begin
 if (tobj<>nil) then
 begin
  vm_map_track_remove(p_proc.p_vmspace,tobj);
 end;

 inherited;
end;

procedure TvCustomImage2.restore_vm_track;
begin
 if (tobj=nil) then Exit;

 rw_wlock(lock);

 vm_map_track_restore(p_proc.p_vmspace,tobj);

 rw_wunlock(lock)
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

procedure t_change_rate.mark_init;
begin
 state:=1;
end;

function t_change_rate.need_read:Boolean;
begin
 Result:=(state=0) or (trigger<>0) or (planned<>0);
end;

procedure TvCustomImage2.mark_init;
begin
 System.InterlockedExchange(change_rate.state,1);
end;

function TvCustomImage2.get_change_rate:t_change_rate;
begin
 Result.state  :=System.InterlockedExchangeAdd(change_rate.state  ,0);
 Result.trigger:=System.InterlockedExchangeAdd(change_rate.trigger,0);
 Result.planned:=System.InterlockedExchangeAdd(change_rate.planned,0);
end;

procedure TvCustomImage2.apply_change_rate(r:t_change_rate);
begin
 System.InterlockedExchange   (change_rate.state  ,   r.state);
 System.InterlockedExchangeAdd(change_rate.trigger,-r.trigger);
 System.InterlockedExchangeAdd(change_rate.planned,-r.planned);
end;

Function TvCustomImage2.IsDepthAndStencil:Boolean;
begin
 Result:=vImage.IsDepthAndStencil(key.cformat);
end;

Function TvCustomImage2.GetSubresRange:TVkImageSubresourceRange;
begin
 Result:=Default(TVkImageSubresourceRange);
 Result.aspectMask:=GetAspectMaskByFormat(key.cformat);
 Result.levelCount:=key.params.mipLevels;
 Result.layerCount:=key.params.layerCount;
end;

Function TvCustomImage2.GetSubresLayer:TVkImageSubresourceLayers;
begin
 Result:=Default(TVkImageSubresourceLayers);
 Result.aspectMask    :=GetAspectMaskByFormat(key.cformat);
 Result.mipLevel      :=0;
 Result.baseArrayLayer:=0;
 Result.layerCount    :=key.params.layerCount;
end;

//

procedure TvChildImage2.FreeHandle;
begin
 FHandle:=VK_NULL_HANDLE;
end;

function TvChildImage2.FetchViewRaw(cmd:TvCustomCmdBuffer;const F:TvImageViewKey;usage:TVkFlags):TvImageView2;
begin
 Result:=Parent.FetchViewRaw(cmd,F,usage);
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
                 @cmd.BeforePushBarrier,
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
 Result.baseArrayLayer:=key.baseArrayLayer;
 Result.layerCount    :=key.layerCount;
end;

Function TvImageView2.GetSubresLayer(cformat:TVkFormat=VK_FORMAT_UNDEFINED):TVkImageSubresourceLayers;
begin
 if (cformat=VK_FORMAT_UNDEFINED) then cformat:=key.cformat;

 Result:=Default(TVkImageSubresourceLayers);
 Result.aspectMask    :=GetAspectMaskByFormat(cformat);
 Result.mipLevel      :=key.base_level;
 Result.baseArrayLayer:=key.baseArrayLayer;
 Result.layerCount    :=key.layerCount;
end;

Constructor TvImage2.Create;
begin
 inherited;
 Barrier.Init;
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
 Result.arrayLayers  :=key.params.layerCount;
 Result.samples      :=TVkSampleCountFlagBits(key.params.samples);
 Result.tiling       :=VK_IMAGE_TILING_OPTIMAL;
 Result.usage        :=GET_VK_IMAGE_USAGE_DEFAULT(key.cformat);
 Result.initialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;

 if (key.params.cube<>0) then
 begin
  Result.flags:=Result.flags or ord(VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT);
 end;

 if (Result.imageType=VK_IMAGE_TYPE_3D) then
 begin
  Result.flags:=Result.flags or ord(VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT);
 end;

 //

 if (key.params.cube<>0) then
 begin
  Assert((Result.arrayLayers mod 6)=0,'CUBE: layerCount must be a multiple of 6');
 end;
end;

function TvImage2.FetchViewRaw(cmd:TvCustomCmdBuffer;const F:TvImageViewKey;usage:TVkFlags):TvImageView2;
var
 key2:TvImageViewKey;

 i:TvImageView2Set.Iterator;
 t:TvImageView2;

 cinfo:TVkImageViewCreateInfo;
 uinfo:TVkImageViewUsageCreateInfo;
 minfo:TVkImageViewMinLodCreateInfoEXT;

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
  cinfo.subresourceRange.baseArrayLayer:=F.baseArrayLayer;
  cinfo.subresourceRange.layerCount    :=F.layerCount;

  if (cinfo.subresourceRange.baseArrayLayer +
      cinfo.subresourceRange.layerCount) > self.key.params.layerCount
   then
  begin
   Assert(false);
  end;

  case cinfo.viewType of
   VK_IMAGE_VIEW_TYPE_CUBE:
    begin
     Assert(cinfo.subresourceRange.layerCount=6,'VK_IMAGE_VIEW_TYPE_CUBE: layerCount must be 6');
    end;
   VK_IMAGE_VIEW_TYPE_CUBE_ARRAY:
    begin
     Assert((cinfo.subresourceRange.layerCount mod 6)=0,'VK_IMAGE_VIEW_TYPE_CUBE_ARRAY: layerCount must be a multiple of 6');
    end;
   else;
  end;

  cinfo.format:=vkFixFormatSupport(cinfo.format,VK_IMAGE_TILING_OPTIMAL,usage);

  cinfo.pNext:=@uinfo;

  uinfo:=Default(TVkImageViewUsageCreateInfo);
  uinfo.sType:=VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO;
  uinfo.usage:=usage;

  if limits.VK_EXT_image_view_min_lod and
     (F.minLod<>0) then
  begin
   uinfo.pNext:=@minfo;
   //
   minfo:=Default(TVkImageViewMinLodCreateInfoEXT);
   minfo.sType :=VK_STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT;
   minfo.minLod:=F.minLod;
  end;

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
 fkey:TvImageViewKey;
 fusage:TVkFlags;
begin
 fkey:=F;
 //
 case usage of
  iu_attachment:
   begin
    fusage:=ord(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT);
   end;
  iu_depthstenc:
   begin
    fusage:=ord(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT);
   end;
  iu_sampled:
   begin
    fusage:=ord(VK_IMAGE_USAGE_SAMPLED_BIT);
   end;
  iu_storage:
   begin
    //Separate storage access for special cases?
    //fkey.cformat:=GET_VK_FORMAT_STORAGE(fkey.cformat);
    fusage:=ord(VK_IMAGE_USAGE_STORAGE_BIT);
   end;
  else
   fusage:=0; //default
 end;
 //
 Result:=FetchViewRaw(cmd,fkey,fusage);
end;

function TvCustomImage2.FetchView(cmd:TvCustomCmdBuffer;usage:t_image_usage):TvImageView2;
var
 fkey:TvImageViewKey;
 fusage:TVkFlags;
begin
 if (Self=nil) then Exit;

 fkey:=Default(TvImageViewKey);
 fkey.cformat:=key.cformat;

 Case TVkImageType(key.params.itype) of
  VK_IMAGE_TYPE_1D:
   begin
    if (key.params.arrayLayers>1) then
     fkey.vtype:=ord(VK_IMAGE_VIEW_TYPE_1D_ARRAY)
    else
     fkey.vtype:=ord(VK_IMAGE_VIEW_TYPE_1D);
   end;
  VK_IMAGE_TYPE_2D:
   begin
    if (key.params.cube<>0) then
    begin
     if (key.params.arrayLayers>6) then
     begin
      fkey.vtype:=ord(VK_IMAGE_VIEW_TYPE_CUBE_ARRAY);
     end else
     begin
      fkey.vtype:=ord(VK_IMAGE_VIEW_TYPE_CUBE);
     end;
    end else
    if (key.params.arrayLayers>1) then
    begin
     fkey.vtype:=ord(VK_IMAGE_VIEW_TYPE_2D_ARRAY);
    end else
    begin
     fkey.vtype:=ord(VK_IMAGE_VIEW_TYPE_2D);
    end;
   end;
  VK_IMAGE_TYPE_3D:fkey.vtype:=ord(VK_IMAGE_VIEW_TYPE_3D);
 end;

 fkey.last_level:=key.params.mipLevels  -1;
 fkey.last_array:=key.params.arrayLayers-1;

 fusage:=0;

 //
 case usage of
  iu_attachment:
   begin
    fusage:=ord(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT);
   end;
  iu_depthstenc:
   begin
    fusage:=ord(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT);
   end;
  iu_sampled:
   begin
    fusage:=ord(VK_IMAGE_USAGE_SAMPLED_BIT);
   end;
  iu_storage:
   begin
    //Separate storage access for special cases?
    //fkey.cformat:=GET_VK_FORMAT_STORAGE(fkey.cformat);
    fusage:=ord(VK_IMAGE_USAGE_STORAGE_BIT);
   end;
  else
   fusage:=0; //default
 end;
 //

 Result:=FetchViewRaw(cmd,fkey,fusage);
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
                 @cmd.BeforePushBarrier,
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

procedure TvDepthStencilImage2.restore_vm_track;
begin
 if (DepthOnly<>nil) then
 begin
  DepthOnly.restore_vm_track;
 end;

 if (StencilOnly<>nil) then
 begin
  StencilOnly.restore_vm_track;
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

function _Find(const F:TvImageKey):TvCustomImage2;
var
 i:TvImage2Set.Iterator;
begin
 Result:=nil;
 i:=FImage2Set.find(@F);
 if (i.Item<>nil) then
 begin
  Result:=TvImage2(ptruint(i.Item^)-ptruint(@TvImage2(nil).key));

  if (Result.Parent<>nil) then
  begin
   Result:=Result.Parent;
  end;
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
    //
    Result.StencilOnly:=TvChildImage2.Create;
    Result.StencilOnly.key   :=GetStencilOnly(F);
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
    //
    Result.DepthOnly:=TvChildImage2.Create;
    Result.DepthOnly.key   :=GetDepthOnly(F);
    Result.DepthOnly.Parent:=Result;
    //
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

procedure _DeleteImage(t:TvCustomImage2);
begin
 FImage2Set.delete(@t.key);
 t._Release(nil); //map ref

 if (t.DepthOnly<>nil) and
    (t.DepthOnly<>t) then
 begin
  FImage2Set.delete(@t.DepthOnly.key);
 end;

 if (t.StencilOnly<>nil) and
    (t.StencilOnly<>t) then
 begin
  FImage2Set.delete(@t.StencilOnly.key);
 end;

end;

procedure _DeleteAlias(const F:TvImageKey);
var
 t:TvCustomImage2;
begin
 t:=_Find(F);
 if (t=nil) then Exit;

 _DeleteImage(t);
end;

function _InsertImage(t:TvCustomImage2):Boolean;
begin
 if FImage2Set.Insert(@t.key) then
 begin
  t._Acquire(nil); //map ref
 end else
 begin
  Exit(False);
 end;

 if (t.DepthOnly<>nil) and
    (t.DepthOnly<>t) then
 begin
  if not FImage2Set.Insert(@t.DepthOnly.key) then
  begin
   //alias? -> delete
   _DeleteAlias(t.DepthOnly.key);
   //again
   if not FImage2Set.Insert(@t.DepthOnly.key) then
   begin
    //wtf?
    _DeleteImage(t);
    Exit(False);
   end;
  end;
 end;

 if (t.StencilOnly<>nil) and
    (t.StencilOnly<>t) then
 begin
  if not FImage2Set.Insert(@t.StencilOnly.key) then
  begin
   //alias? -> delete
   _DeleteAlias(t.StencilOnly.key);
   //again
   if not FImage2Set.Insert(@t.StencilOnly.key) then
   begin
    //wtf?
    _DeleteImage(t);
    Exit(False);
   end;
  end;
 end;

 Result:=True;
end;

procedure _SetName(t:TvCustomImage2);
var
 ch:Char;
begin

 Case t.key.cformat of
  //stencil
  VK_FORMAT_S8_UINT:
   begin
    Ch:='S';
   end;
  //depth
  VK_FORMAT_D16_UNORM,
  VK_FORMAT_X8_D24_UNORM_PACK32,
  VK_FORMAT_D32_SFLOAT:
   begin
    Ch:='D';
   end;
  //depth stencil
  VK_FORMAT_D16_UNORM_S8_UINT,
  VK_FORMAT_D24_UNORM_S8_UINT,
  VK_FORMAT_D32_SFLOAT_S8_UINT:
   begin
    Ch:='X';
   end;
  else
   begin
    if (t.key.params.cube<>0) then
    begin
     Ch:='C';
    end else
    if (t.key.params.arrayLayers>1) then
    begin
     Ch:='A';
    end else
    begin
     Ch:='I';
    end;
   end;
 end;

 t.SetObjectName(Ch+'_0x'+HexStr(QWORD(t.key.Addr),10)+
                      '_'+IntToStr(t.key.params.width)+'x'+IntToStr(t.key.params.height)+
                     '_m'+IntToStr(t.key.params.mipLevels)+
                     '_a'+IntToStr(t.key.params.arrayLayers)+
                     '_t'+IntToStr(t.key.params.tiling.idx)+'|'+IntToStr(t.key.params.tiling.alt)
                );

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

 t:=TvImage2(_Find(F));

 if (t<>nil) then
 begin
  if t.Acquire(nil) then
  begin
   t.FUsage:=t.FUsage+usage;
  end else
  begin
   //mem is deleted, free img
   _DeleteImage(t);
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

   _SetName(t);

   Fdevc:=MemManager.FetchMemory(
     t.GetRequirements,
     V_PROP_DEVICE_LOCAL or V_PROP_BEST_FIT
   );

   t.BindMem(Fdevc);

   if _InsertImage(t) then
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

 FImage2Set.Unlock_wr;

 if (Result<>nil) then
 begin
  Result.Release(nil); //<- Acquire(nil)/FetchMemory
 end;
end;

function FindImage(cmd:TvCustomCmdBuffer;Addr:Pointer;cformat:TVkFormat):TvImage2;
begin
 FImage2Set.Lock_wr;

 Result:=_FindImage(Addr,cformat);

 cmd.RefTo(Result);

 FImage2Set.Unlock_wr;
end;


end.



