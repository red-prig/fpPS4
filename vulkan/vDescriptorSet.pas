unit vDescriptorSet;

{$mode ObjFPC}{$H+}

interface

uses
 g_node_splay,
 Vulkan,
 vDevice,
 vPipeline,
 vDependence;

type
 PvDescriptorCache=^TvDescriptorCache;
 TvDescriptorCache=packed object
  //
  layout:TvPipelineLayout; //Must be the first element in memory
  //
  p_write:PVkWriteDescriptorSet;
  //
  p_count_all:Byte;
  //            set      bind
  p_count:array[0..6] of Byte;
  p_binds:array[0..6] of PVkWriteDescriptorSet;
  //
  p_change_any:Boolean;
  p_change:array[0..6] of Boolean;
  //
  pLeft :PvDescriptorCache;
  pRight:PvDescriptorCache;
  //
  function  c(a,b:PvDescriptorCache):Integer; static;
  procedure ClearAllChange;
  procedure SetAllChange;
  procedure BindBuffer (aSet,aBind:TVkUInt32;dtype:TVkDescriptorType;buffer:TVkBuffer;offset,range:TVkDeviceSize);
  procedure BindImage  (aSet,aBind:TVkUInt32;dtype:TVkDescriptorType;img:TVkImageView;aLayout:TVkImageLayout);
  procedure BindSampler(aSet,aBind:TVkUInt32;smp:TVkSampler);
 end;

 TvDescriptorCacheSet=specialize TNodeSplay<TvDescriptorCache>;

 TvDescriptorInterface=object
  FHandle:PvDescriptorCache;
  Procedure  BindBuffer (aSet,aBind:TVkUInt32;buffer:TVkBuffer;offset,range:TVkDeviceSize);
  Procedure  BindUniform(aSet,aBind:TVkUInt32;buffer:TVkBuffer;offset,range:TVkDeviceSize);
  Procedure  BindStorage(aSet,aBind:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
  Procedure  BindImage  (aSet,aBind:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
  Procedure  BindSampler(aSet,aBind:TVkUInt32;smp:TVkSampler);
 end;

 TvDescriptorSet2=object
  FHandle:TVkDescriptorSet;
  Function   IsValid:Boolean;
  Procedure  FillHandle (dwrite:PVkWriteDescriptorSet;count:Integer);
  Procedure  BindBuffer (aBind,aElem:TVkUInt32;buffer:TVkBuffer;offset,range:TVkDeviceSize);
  Procedure  BindUniform(aBind,aElem:TVkUInt32;buffer:TVkBuffer;offset,range:TVkDeviceSize);
  Procedure  BindStorage(aBind,aElem:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
  Procedure  BindImage  (aBind,aElem:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
  Procedure  BindSampler(aBind,aElem:TVkUInt32;smp:TVkSampler);
 end;

 AvDescriptorSet2=Array of TvDescriptorSet2;

 TvDescriptorGroup=class(TvRefsObject)
  FLayout:TvPipelineLayout;
  FSets  :AvDescriptorSet2;
  Procedure Bind(Cache:PvDescriptorCache);
 end;

function AllocDescriptorCache(obj:TvDependenciesObject;layout:TvPipelineLayout):PvDescriptorCache;

implementation

Procedure TvDescriptorGroup.Bind(Cache:PvDescriptorCache);
var
 i:Integer;
begin
 if (Cache=nil) then Exit;

 Assert(FLayout=Cache^.layout,'bind on wrong layout');

 if (Length(FSets)<>0) then
 For i:=0 to High(FSets) do
 begin
  FSets[i].FillHandle(Cache^.p_binds[i],Cache^.p_count[i]);
 end;

 if (Cache^.p_count_all<>0) then
 begin
  vkUpdateDescriptorSets(Device.FHandle,Cache^.p_count_all,Cache^.p_write,0,nil);
 end;
end;

//

Function TvDescriptorSet2.IsValid:Boolean;
begin
 Result:=FHandle<>VK_NULL_HANDLE;
end;

Procedure TvDescriptorSet2.FillHandle(dwrite:PVkWriteDescriptorSet;count:Integer);
var
 i:Integer;
begin
 if (dwrite=nil) or (count=0) then Exit;

 For i:=0 to count-1 do
 begin
  dwrite[i].dstSet:=FHandle;
 end;
end;

Procedure TvDescriptorSet2.BindBuffer(aBind,aElem:TVkUInt32;buffer:TVkBuffer;offset,range:TVkDeviceSize);
var
 dwrite:TVkWriteDescriptorSet;
 buf:TVkDescriptorBufferInfo;
begin
 buf:=Default(TVkDescriptorBufferInfo);
 buf.buffer:=buffer;
 buf.offset:=offset;
 buf.range :=range ;

 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
 dwrite.descriptorCount:=1;
 dwrite.pBufferInfo    :=@buf;

 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

Procedure TvDescriptorSet2.BindUniform(aBind,aElem:TVkUInt32;buffer:TVkBuffer;offset,range:TVkDeviceSize);
var
 dwrite:TVkWriteDescriptorSet;
 buf:TVkDescriptorBufferInfo;
begin
 buf:=Default(TVkDescriptorBufferInfo);
 buf.buffer:=buffer;
 buf.offset:=offset;
 buf.range :=range ;

 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
 dwrite.descriptorCount:=1;
 dwrite.pBufferInfo    :=@buf;

 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

Procedure TvDescriptorSet2.BindStorage(aBind,aElem:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
var
 dwrite:TVkWriteDescriptorSet;
 dimg:TVkDescriptorImageInfo;
begin
 dimg:=Default(TVkDescriptorImageInfo);
 dimg.imageView  :=img;
 dimg.imageLayout:=Layout;

 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
 dwrite.descriptorCount:=1;
 dwrite.pImageInfo     :=@dimg;
 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

Procedure TvDescriptorSet2.BindImage(aBind,aElem:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
var
 dwrite:TVkWriteDescriptorSet;
 dimg:TVkDescriptorImageInfo;
begin
 dimg:=Default(TVkDescriptorImageInfo);
 dimg.imageView  :=img;
 dimg.imageLayout:=Layout;

 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE;
 dwrite.descriptorCount:=1;
 dwrite.pImageInfo     :=@dimg;
 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

Procedure TvDescriptorSet2.BindSampler(aBind,aElem:TVkUInt32;smp:TVkSampler);
var
 dwrite:TVkWriteDescriptorSet;
 dimg:TVkDescriptorImageInfo;
begin
 dimg:=Default(TVkDescriptorImageInfo);
 dimg.sampler:=smp;

 dwrite:=Default(TVkWriteDescriptorSet);
 dwrite.sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 dwrite.dstSet         :=FHandle;
 dwrite.dstBinding     :=aBind;
 dwrite.dstArrayElement:=aElem;
 dwrite.descriptorType :=VK_DESCRIPTOR_TYPE_SAMPLER;
 dwrite.descriptorCount:=1;
 dwrite.pImageInfo     :=@dimg;
 vkUpdateDescriptorSets(Device.FHandle,1,@dwrite,0,nil);
end;

//

Procedure TvDescriptorInterface.BindBuffer(aSet,aBind:TVkUInt32;buffer:TVkBuffer;offset,range:TVkDeviceSize);
begin
 if (FHandle=nil) then Exit;

 FHandle^.BindBuffer(aSet,aBind,VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,buffer,offset,range);
end;

Procedure TvDescriptorInterface.BindUniform(aSet,aBind:TVkUInt32;buffer:TVkBuffer;offset,range:TVkDeviceSize);
begin
 if (FHandle=nil) then Exit;

 FHandle^.BindBuffer(aSet,aBind,VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,buffer,offset,range);
end;


Procedure TvDescriptorInterface.BindStorage(aSet,aBind:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
begin
 if (FHandle=nil) then Exit;

 FHandle^.BindImage(aSet,aBind,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,img,Layout);
end;

Procedure TvDescriptorInterface.BindImage(aSet,aBind:TVkUInt32;img:TVkImageView;Layout:TVkImageLayout);
begin
 if (FHandle=nil) then Exit;

 FHandle^.BindImage(aSet,aBind,VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,img,Layout);
end;

Procedure TvDescriptorInterface.BindSampler(aSet,aBind:TVkUInt32;smp:TVkSampler);
begin
 if (FHandle=nil) then Exit;

 FHandle^.BindSampler(aSet,aBind,smp);
end;


//

function AllocDescriptorCache(obj:TvDependenciesObject;layout:TvPipelineLayout):PvDescriptorCache;
var
 i,b         :Integer;
 dwrite_count:Integer;
 dimg_count  :Integer;
 dbuf_count  :Integer;
 size        :Integer;
 base        :Pointer;
 ends        :Pointer;

 p_write_base:PVkWriteDescriptorSet;
 p_write_ends:PVkWriteDescriptorSet;

 function AllocBase(size:Integer):Pointer; inline;
 begin
  Assert((base+size)<=ends,'AllocBase');
  Result:=base;
  base:=base+size;
 end;

 function AllocWrite(count:Integer):PVkWriteDescriptorSet; inline;
 begin
  Assert((p_write_base+count)<=p_write_ends,'AllocWrite');
  Result:=p_write_base;
  p_write_base:=p_write_base+count;
 end;

begin
 Assert(layout<>nil,'layout not binded');

 Assert(layout.FCounts[ord(VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER  )]=0,'TODO');
 Assert(layout.FCounts[ord(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER  )]=0,'TODO');
 Assert(layout.FCounts[ord(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC)]=0,'TODO');
 Assert(layout.FCounts[ord(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC)]=0,'TODO');
 Assert(layout.FCounts[ord(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT      )]=0,'TODO');

 dwrite_count:=layout.FBinds;
 Assert(dwrite_count<=255,'dwrite_count limit');

 dimg_count:=
  layout.FCounts[ord(VK_DESCRIPTOR_TYPE_SAMPLER)]+
  layout.FCounts[ord(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)]+
  layout.FCounts[ord(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE)]+
  layout.FCounts[ord(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)];

 dbuf_count:=
  layout.FCounts[ord(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)]+
  layout.FCounts[ord(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)];

 size:=SizeOf(TvDescriptorCache)+
       dwrite_count*SizeOf(TVkWriteDescriptorSet)+
       dimg_count  *SizeOf(TVkDescriptorImageInfo)+
       dbuf_count  *SizeOf(TVkDescriptorBufferInfo);

 base:=obj.OnAlloc(size);
 ends:=base+size;

 Result:=AllocBase(SizeOf(TvDescriptorCache));

 Result^.layout:=layout;

 p_write_base:=AllocBase(dwrite_count*SizeOf(TVkWriteDescriptorSet));
 p_write_ends:=base;

 Result^.p_write:=p_write_base;
 Result^.p_count_all:=dwrite_count;

 if Length(layout.key.FLayouts)<>0 then
 begin
  For i:=0 to High(layout.key.FLayouts) do
   With layout.key.FLayouts[i] do
    if (Length(key.FBinds)<>0) then
    begin

     Result^.p_count[i]:=Length(key.FBinds);
     Result^.p_binds[i]:=AllocWrite(Length(key.FBinds));

     For b:=0 to High(key.FBinds) do
      with key.FBinds[b] do
      begin

       Result^.p_binds[i][b].sType          :=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
       Result^.p_binds[i][b].dstBinding     :=b;
       Result^.p_binds[i][b].descriptorCount:=1;
       Result^.p_binds[i][b].descriptorType :=descriptorType;

       case descriptorType of
        VK_DESCRIPTOR_TYPE_SAMPLER,
        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
        VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,
        VK_DESCRIPTOR_TYPE_STORAGE_IMAGE:
          begin
           Result^.p_binds[i][b].pImageInfo:=AllocBase(SizeOf(TVkDescriptorImageInfo));
          end;

        VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        VK_DESCRIPTOR_TYPE_STORAGE_BUFFER:
          begin
           Result^.p_binds[i][b].pBufferInfo:=AllocBase(SizeOf(TVkDescriptorBufferInfo));
          end;

        else;
       end;

      end;
    end;
 end;

end;

///

function TvDescriptorCache.c(a,b:PvDescriptorCache):Integer;
begin
 Result:=Integer(Pointer(a^.layout)>Pointer(b^.layout))-Integer(Pointer(a^.layout)<Pointer(b^.layout));
end;

procedure TvDescriptorCache.ClearAllChange;
begin
 //hack
 PQWORD(@p_change_any)^:=0;
end;

procedure TvDescriptorCache.SetAllChange;
begin
 //hack
 PQWORD(@p_change_any)^:=$0101010101010101;
end;

procedure TvDescriptorCache.BindBuffer(aSet,aBind:TVkUInt32;dtype:TVkDescriptorType;buffer:TVkBuffer;offset,range:TVkDeviceSize);
var
 dwrite:PVkWriteDescriptorSet;
 dbuf  :PVkDescriptorBufferInfo;
 change:Boolean;
begin
 Assert(aSet<7);
 Assert(aBind<p_count[aSet]);
 Assert(p_binds[aSet]<>nil);

 dwrite:=@p_binds[aSet][aBind];

 Assert(dwrite^.descriptorType=dtype);

 dbuf:=dwrite^.pBufferInfo;

 Assert(dbuf<>nil);

 change:=False;

 change:=change or (dbuf^.buffer<>buffer);
 change:=change or (dbuf^.offset<>offset);
 change:=change or (dbuf^.range <>range );

 dbuf^.buffer:=buffer;
 dbuf^.offset:=offset;
 dbuf^.range :=range ;

 if change then
 begin
  p_change_any:=True;
  p_change[aSet]:=True;
 end;
end;

procedure TvDescriptorCache.BindImage(aSet,aBind:TVkUInt32;dtype:TVkDescriptorType;img:TVkImageView;aLayout:TVkImageLayout);
var
 dwrite:PVkWriteDescriptorSet;
 dimg  :PVkDescriptorImageInfo;
 change:Boolean;
begin
 Assert(aSet<7);
 Assert(aBind<p_count[aSet]);
 Assert(p_binds[aSet]<>nil);

 dwrite:=@p_binds[aSet][aBind];

 Assert(dwrite^.descriptorType=dtype);

 dimg:=dwrite^.pImageInfo;

 Assert(dimg<>nil);

 change:=False;

 change:=change or (dimg^.imageView  <>img    );
 change:=change or (dimg^.imageLayout<>aLayout);

 dimg^.imageView  :=img;
 dimg^.imageLayout:=aLayout;

 if change then
 begin
  p_change_any:=True;
  p_change[aSet]:=True;
 end;
end;

procedure TvDescriptorCache.BindSampler(aSet,aBind:TVkUInt32;smp:TVkSampler);
var
 dwrite:PVkWriteDescriptorSet;
 dimg  :PVkDescriptorImageInfo;
 change:Boolean;
begin
 Assert(aSet<7);
 Assert(aBind<p_count[aSet]);
 Assert(p_binds[aSet]<>nil);

 dwrite:=@p_binds[aSet][aBind];

 Assert(dwrite^.descriptorType=VK_DESCRIPTOR_TYPE_SAMPLER);

 dimg:=dwrite^.pImageInfo;

 Assert(dimg<>nil);

 change:=False;

 change:=change or (dimg^.sampler<>smp);

 dimg^.sampler:=smp;

 if change then
 begin
  p_change_any:=True;
  p_change[aSet]:=True;
 end;
end;


end.

