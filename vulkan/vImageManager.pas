unit vImageManager;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 RWLock,
 g23tree,
 Vulkan,
 vDevice,
 vMemory,
 vImage,
 vCmdBuffer,
 vImageTiling;

type
 TvImageView2Compare=object
  function c(a,b:PvImageViewKey):Integer; static;
 end;

 TvImageView2=class(TvImageView)
  key:TvImageViewKey;
  procedure Release(Sender:TObject);
  Function  GetSubresRange:TVkImageSubresourceRange;
  Function  GetSubresLayer:TVkImageSubresourceLayers;
 end;

 TvImageView2Set=specialize T23treeSet<PvImageViewKey,TvImageView2Compare>;

 TvImage2=class;

 TvHostImage2=class(TvCustomImage)
  parent:TvImage2;
  FUsage:TVkFlags;
  function GetImageInfo:TVkImageCreateInfo; override;
 end;

 TvImage2=class(TvCustomImage)
  key:TvImageKey;
  FUsage:TVkFlags;
  FViews:TvImageView2Set;
  //
  FHostImage:TvHostImage2;
  //
  Fdevc:TvPointer;
  //
  FRefs:ptruint;
  FDeps:TObjectSetLock;
  //
  data_usage:Byte;
  Destructor  Destroy; override;
  function    GetImageInfo:TVkImageCreateInfo; override;
  Function    GetSubresRange:TVkImageSubresourceRange;
  Function    GetSubresLayer:TVkImageSubresourceLayers;
  function    FetchView(cmd:TvCustomCmdBuffer;var F:TvImageViewKey):TvImageView2;
  function    FetchView(cmd:TvCustomCmdBuffer):TvImageView2;
  function    FetchHostImage(cmd:TvCustomCmdBuffer;usage:TVkFlags):TvHostImage2;
  Procedure   Acquire(Sender:TObject);
  procedure   Release(Sender:TObject);
 end;

function FetchImage(cmd:TvCustomCmdBuffer;var F:TvImageKey;usage:TVkFlags;data_usage:Byte):TvImage2;
function FindImage(cmd:TvCustomCmdBuffer;Addr:Pointer;cformat:TVkFormat):TvImage2;

const
 img_ext:TVkExternalMemoryImageCreateInfo=(
  sType:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO;
  pNext:nil;
  handleTypes:ord(VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT);
 );

implementation

type
 TvImageKeyCompare=object
  function c(a,b:PvImageKey):Integer; static;
 end;

 _TvImage2Set=specialize T23treeSet<PvImageKey,TvImageKeyCompare>;
 TvImage2Set=object(_TvImage2Set)
  lock:TRWLock;
  Procedure Init;
  Procedure Lock_wr;
  Procedure Unlock;
 end;

var
 FImage2Set:TvImage2Set;

Procedure TvImage2Set.Init;
begin
 rwlock_init(lock);
end;

Procedure TvImage2Set.Lock_wr;
begin
 rwlock_wrlock(lock);
end;

Procedure TvImage2Set.Unlock;
begin
 rwlock_unlock(lock);
end;

function TvImageKeyCompare.c(a,b:PvImageKey):Integer;
begin
 //1 Addr
 Result:=Integer(a^.Addr>b^.Addr)-Integer(a^.Addr<b^.Addr);
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

procedure TvImageView2.Release(Sender:TObject);
begin
 inherited Release;
end;

Function TvImageView2.GetSubresRange:TVkImageSubresourceRange;
begin
 Result:=Default(TVkImageSubresourceRange);
 Result.aspectMask    :=GetAspectMaskByFormat(key.cformat);
 Result.baseMipLevel  :=key.base_level;
 Result.levelCount    :=key.last_level-key.base_level+1;
 Result.baseArrayLayer:=key.base_array;
 Result.layerCount    :=key.last_array-key.base_array+1;
end;

Function TvImageView2.GetSubresLayer:TVkImageSubresourceLayers;
begin
 Result:=Default(TVkImageSubresourceLayers);
 Result.aspectMask    :=GetAspectMaskByFormat(key.cformat);
 Result.mipLevel      :=key.base_level;
 Result.baseArrayLayer:=key.base_array;
 Result.layerCount    :=key.last_array-key.base_array+1;
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
  t.Release(nil);
  i.Next;
 end;
 FViews.Free;

 MemManager.Free(Fdevc);

 inherited;
end;

function TvImage2.GetImageInfo:TVkImageCreateInfo;
begin
 Result:=Default(TVkImageCreateInfo);
 Result.sType        :=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
 Result.imageType    :=TVkImageType(key.params.itype);
 Result.format       :=key.cformat;
 Result.extent.Create(key.params.extend.width,key.params.extend.height,key.params.extend.depth);
 Result.mipLevels    :=key.params.mipLevels;
 Result.arrayLayers  :=key.params.arrayLayers;
 Result.samples      :=TVkSampleCountFlagBits(key.params.samples);
 Result.tiling       :=VK_IMAGE_TILING_OPTIMAL;
 Result.usage        :=FUsage;
 Result.initialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
end;

function TvHostImage2.GetImageInfo:TVkImageCreateInfo;
begin
 Result:=parent.GetImageInfo;
 Result.tiling:=VK_IMAGE_TILING_LINEAR;
 Result.usage :=FUsage;
end;

Function TvImage2.GetSubresRange:TVkImageSubresourceRange;
begin
 Result:=Default(TVkImageSubresourceRange);
 Result.aspectMask:=GetAspectMaskByFormat(key.cformat);
 Result.levelCount:=key.params.mipLevels;
 Result.layerCount:=key.params.arrayLayers;
end;

Function TvImage2.GetSubresLayer:TVkImageSubresourceLayers;
begin
 Result:=Default(TVkImageSubresourceLayers);
 Result.aspectMask    :=GetAspectMaskByFormat(key.cformat);
 Result.mipLevel      :=0;
 Result.baseArrayLayer:=0;
 Result.layerCount    :=key.params.arrayLayers;
end;

function TvImage2.FetchView(cmd:TvCustomCmdBuffer;var F:TvImageViewKey):TvImageView2;
var
 i:TvImageView2Set.Iterator;
 t:TvImageView2;

 cinfo:TVkImageViewCreateInfo;
 FView:TVkImageView;
 r:TVkResult;
begin
 Result:=nil;
 if (FHandle=VK_NULL_HANDLE) then Exit;

 t:=nil;
 i:=FViews.find(@F);
 if (i.Item<>nil) then
 begin
  t:=TvImageView2(ptruint(i.Item^)-ptruint(@TvImageView2(nil).key));
 end else
 begin
  cinfo:=Default(TVkImageViewCreateInfo);
  cinfo.sType       :=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
  cinfo.image       :=FHandle;
  cinfo.viewType    :=TVkImageViewType(F.vtype);
  cinfo.format      :=key.cformat;
  cinfo.components.r:=TVkComponentSwizzle(F.dstSel.r);
  cinfo.components.g:=TVkComponentSwizzle(F.dstSel.g);
  cinfo.components.b:=TVkComponentSwizzle(F.dstSel.b);
  cinfo.components.a:=TVkComponentSwizzle(F.dstSel.a);

  cinfo.subresourceRange.aspectMask    :=GetAspectMaskByFormat(F.cformat);
  cinfo.subresourceRange.baseMipLevel  :=F.base_level;
  cinfo.subresourceRange.levelCount    :=F.last_level-F.base_level+1;
  cinfo.subresourceRange.baseArrayLayer:=F.base_array;
  cinfo.subresourceRange.layerCount    :=F.last_array-F.base_array+1;

  FView:=VK_NULL_HANDLE;
  r:=vkCreateImageView(Device.FHandle,@cinfo,nil,@FView);
  if (r<>VK_SUCCESS) then
  begin
   Writeln('vkCreateImageView:',r);
   Exit;
  end;

  t:=TvImageView2.Create;
  t.FHandle:=FView;
  t.key    :=F;

  t.Acquire;
  FViews.Insert(@t.key);
 end;

 if (cmd<>nil) and (t<>nil) then
 begin
  if cmd.AddDependence(@t.Release) then
  begin
   t.Acquire;
  end;
 end;

 Result:=t;
end;

function TvImage2.FetchView(cmd:TvCustomCmdBuffer):TvImageView2;
var
 F:TvImageViewKey;
begin
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

 F.last_level:=key.params.mipLevels-1;
 F.last_array:=key.params.arrayLayers-1;

 Result:=FetchView(cmd,F);
end;

function TvImage2.FetchHostImage(cmd:TvCustomCmdBuffer;usage:TVkFlags):TvHostImage2;
var
 t:TvHostImage2;
 Fhost:TvPointer;
begin
 Result:=nil;
 t:=FHostImage;

 if (t<>nil) then
 begin
  if ((t.FUsage and usage)<>usage) then
  begin
   Assert(false,'TODO');
  end;
  Exit(t);
 end;

 t:=TvHostImage2.Create;
 t.parent:=Self;
 t.FUsage:=usage;

 if not t.Compile(@img_ext) then
 begin
  t.Free;
  Exit;
 end;

 if TryGetHostPointerByAddr(key.Addr,Fhost) then
 begin
  if (t.BindMem(Fhost)<>VK_SUCCESS) then
  begin
   t.Free;
   Exit;
  end;
 end else
 begin
  t.Free;
  Exit;
 end;

 FHostImage:=t;
 Result:=t;

 if (cmd<>nil) and (Self<>nil) then
 begin
  if cmd.AddDependence(@Self.Release) then
  begin
   Self.Acquire(cmd);
  end;
 end;

end;

Procedure TvImage2.Acquire(Sender:TObject);
begin
 System.InterlockedIncrement(Pointer(FRefs));
 if (Sender<>nil) then
 begin
  FDeps.Insert(Sender);
 end;
end;

procedure TvImage2.Release(Sender:TObject);
begin
 if (Sender<>nil) then
 begin
  FDeps.delete(Sender);
 end;
 if System.InterlockedDecrement(Pointer(FRefs))=nil then
 begin
  Free;
 end;
end;

function _Find(var F:TvImageKey):TvImage2;
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

function _FetchImage(var F:TvImageKey;usage:TVkFlags):TvImage2;
var
 t:TvImage2;
begin
 Result:=nil;

 t:=_Find(F);

 if (t<>nil) then
 begin

  if ((t.FUsage and usage)<>usage) then
  begin
   Assert(false,'TODO');
  end;

 end else
 begin
  t:=TvImage2.Create;
  t.key   :=F;
  t.FUsage:=Usage;

  if not t.Compile(nil) then
  begin
   FreeAndNil(t);
  end else
  begin

   t.Fdevc:=MemManager.Alloc(
     t.GetRequirements,
     ord(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
   );
   t.BindMem(t.Fdevc);

   t.Acquire(nil);
   FImage2Set.Insert(@t.key);
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

function FetchImage(cmd:TvCustomCmdBuffer;var F:TvImageKey;usage:TVkFlags;data_usage:Byte):TvImage2;
begin
 FImage2Set.Lock_wr;

 Result:=_FetchImage(F,usage);

 if (cmd<>nil) and (Result<>nil) then
 begin
  if cmd.AddDependence(@Result.Release) then
  begin
   Result.Acquire(cmd);
  end;

  if not cmd.IsRenderPass then
  if ((Result.data_usage and TM_READ)=0) and
     ((data_usage and TM_READ)<>0) and
     ((data_usage and TM_CLEAR)=0) then
  begin
   Result.data_usage:=Result.data_usage or TM_READ;
   LoadFromBuffer(cmd,Result);
  end;

  Result.data_usage:=Result.data_usage or (data_usage and TM_WRITE);

 end;

 FImage2Set.Unlock;
end;

function FindImage(cmd:TvCustomCmdBuffer;Addr:Pointer;cformat:TVkFormat):TvImage2;
begin
 FImage2Set.Lock_wr;

 Result:=_FindImage(Addr,cformat);

 if (cmd<>nil) and (Result<>nil) then
 begin
  if cmd.AddDependence(@Result.Release) then
  begin
   Result.Acquire(cmd);
  end;
 end;

 FImage2Set.Unlock;
end;

{
function _FetchImageView2D(cmd:TvCustomCmdBuffer;
                           Addr:Pointer;
                           tiling_idx:Byte;
                           cformat:TVkFormat;
                           extend:TVkExtent2D;
                           usage:TVkFlags):TvImageView2;
var
 FImageKey:TvImageKey;
 FImageViewKey:TvImageViewKey;

 Image:TvImage2;

begin
 Result:=nil;

 FImageKey:=Default(TvImageKey);
 FImageKey.Addr:=Addr;
 FImageKey.cformat:=cformat;
 FImageKey.params.itype        :=ord(VK_IMAGE_TYPE_2D);
 FImageKey.params.tiling_idx   :=tiling_idx;
 FImageKey.params.extend.width :=extend.width;
 FImageKey.params.extend.height:=extend.height;
 FImageKey.params.extend.depth :=1;
 FImageKey.params.samples      :=1;
 FImageKey.params.mipLevels    :=1;
 FImageKey.params.arrayLayers  :=1;

 FImageViewKey:=Default(TvImageViewKey);
 FImageViewKey.cformat:=cformat;
 FImageViewKey.vtype  :=ord(VK_IMAGE_VIEW_TYPE_2D);
 //FImageViewKey.dstSel
 //FImageViewKey.base_level:Byte;
 //FImageViewKey.last_level:Byte;
 //FImageViewKey.base_array:Word;
 //FImageViewKey.last_array:Word;

 FImage2Set.Lock_wr;

 Image:=_FetchImage(FImageKey,usage);

 if (cmd<>nil) and (Image<>nil) then
 begin
  if cmd.AddDependence(@Image.Release) then
  begin
   Image.Acquire(cmd);
  end;
 end;

 if (Image<>nil) then
 begin
  Result:=Image.FetchView(cmd,FImageViewKey);
 end;

 FImage2Set.Unlock;
end;
}

initialization
 FImage2Set.Init;

end.



