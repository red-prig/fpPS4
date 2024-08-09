unit vSetsPool;

{$mode ObjFPC}{$H+}

interface

uses
 Vulkan,
 vDevice,
 vShader,
 vDescriptorSet,
 vPipeline;

type
 TvSetsPool2=class
  FHandle  :TVkDescriptorPool;
  FLayout  :TvPipelineLayout;
  FmaxGroup:TVkUInt32;
  FmaxSets :TVkUInt32;
  FAlcGroup:TVkUInt32;
  Constructor Create(Layout:TvPipelineLayout;maxGroup:TVkUInt32);
  Destructor  Destroy; override;
  function    Compile:Boolean;
  function    Alloc(L:TvSetLayout):TvDescriptorSet2;
  function    IsFull:Boolean;
  function    Alloc:AvDescriptorSet2;
 end;

implementation

Constructor TvSetsPool2.Create(Layout:TvPipelineLayout;maxGroup:TVkUInt32);
begin
 FLayout  :=Layout;
 FmaxGroup:=maxGroup;
end;

Destructor TvSetsPool2.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyDescriptorPool(Device.FHandle,FHandle,nil);
 end;
end;

function TvSetsPool2.Compile:Boolean;
type
 AvDescriptorPoolSize=array[0..HI_DESCRIPTOR_ID] of TVkDescriptorPoolSize;
var
 i,p:Integer;
 FSizes:AvDescriptorPoolSize;
 cinfo:TVkDescriptorPoolCreateInfo;
 r:TVkResult;
begin
 Result:=False;
 if (FHandle<>VK_NULL_HANDLE) then Exit(true);

 if (FLayout=nil) then Exit;
 if (FmaxGroup=0) then Exit;

 if (not FLayout.Compile) then Exit;

 if (FLayout.FTypes=0) then Exit;

 FSizes:=Default(AvDescriptorPoolSize);

 FmaxSets:=FLayout.FSets*FmaxGroup;

 p:=0;
 For i:=LO_DESCRIPTOR_ID to HI_DESCRIPTOR_ID do
 begin
  if (FLayout.FCounts[i]<>0) then
  begin
   FSizes[p].type_          :=_GetTypeById(i);
   FSizes[p].descriptorCount:=FLayout.FCounts[i]*FmaxGroup;
   Inc(p);
  end;
 end;

 cinfo:=Default(TVkDescriptorPoolCreateInfo);
 cinfo.sType        :=VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
 //cinfo.flags        :=ord(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT);
 cinfo.poolSizeCount:=FLayout.FTypes;
 cinfo.pPoolSizes   :=@FSizes[0];
 cinfo.maxSets      :=FmaxSets;

 r:=vkCreateDescriptorPool(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkCreateDescriptorPool:',r);
  Exit;
 end;

 Result:=True;
end;

function TvSetsPool2.Alloc(L:TvSetLayout):TvDescriptorSet2;
var
 ainfo:TVkDescriptorSetAllocateInfo;
 FResult:TVkDescriptorSet;
 r:TVkResult;
begin
 Result:=Default(TvDescriptorSet2);

 if (L=nil) then Exit;
 if L.IsSpace then Exit;

 if not Compile then Exit;

 ainfo:=Default(TVkDescriptorSetAllocateInfo);
 ainfo.sType             :=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
 ainfo.descriptorPool    :=FHandle;
 ainfo.descriptorSetCount:=1;
 ainfo.pSetLayouts       :=@L.FHandle;
 //VkDescriptorSetVariableDescriptorCountAllocateInfo -> RuntimeArray
 r:=vkAllocateDescriptorSets(Device.FHandle,@ainfo,@FResult);
 if (r<>VK_SUCCESS) then
 begin
  Writeln(StdErr,'vkAllocateDescriptorSets:',r);
  Exit;
 end;

 Result.FHandle:=FResult;
end;

function TvSetsPool2.IsFull:Boolean;
begin
 Result:=(FAlcGroup>=FmaxGroup);
end;

function TvSetsPool2.Alloc:AvDescriptorSet2;
var
 i:Integer;
begin
 Result:=nil;
 if IsFull then Exit;
 SetLength(Result,Length(FLayout.key.FLayouts));
 If (Length(Result)<>0) then
  For i:=0 to High(Result) do
   begin
    Result[i]:=Alloc(FLayout.key.FLayouts[i]);
   end;
 Inc(FAlcGroup);
end;


end.

