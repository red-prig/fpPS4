unit vShaderExt;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ps4_shader,
  ps4_gpu_regs,
  vulkan,
  vDevice,
  vPipeline,
  vShader,
  vImage,
  vSetLayoutManager,
  vPipelineLayoutManager{,
  vSetsPoolManager};


type
 TvResourceType=(
  vtRoot,
  vtBufPtr2,
  vtFunPtr2,
  vtVSharp4,
  vtSSharp4,
  vtTSharp4,
  vtTSharp8
 );

 TvDataLayout=packed record
  rtype:TvResourceType;
  parent:DWORD;
  offset:DWORD;
 end;

 ADataLayout=array of TvDataLayout;

 TvFuncCb=procedure(addr:ADataLayout) of object;

 TvCustomLayout=packed record
  dtype:DWORD;
  bind:DWORD;
  size:DWORD;
  offset:DWORD;
  addr:ADataLayout;
 end;

 ACustomLayout=array of TvCustomLayout;

 TvCustomLayoutCb=procedure(const L:TvCustomLayout;Fset:TVkUInt32;FData:PDWORD) of object;

 TvShaderExt=class(TvShader)
  FDescSetId:Integer;

  FSetLayout:TvSetLayout;

  FDataLayouts:ADataLayout;
  FVertLayouts:ACustomLayout;
  FUnifLayouts:ACustomLayout;

  FPushConst:TvCustomLayout;

  procedure  ClearInfo; override;
  procedure  InitSetLayout;
  procedure  AddToPipeline(p:TvPipelineLayout);
  procedure  OnDescriptorSet(var Target,id:DWORD); override;
  procedure  OnSourceExtension(P:PChar); override;
  Procedure  AddDataLayout(rtype:TvResourceType;parent,offset:DWORD);
  procedure  OnDataLayout(P:PChar);
  Procedure  EnumFuncLayout(cb:TvFuncCb);
  function   GetLayoutAddr(parent:DWORD):ADataLayout;
  Procedure  AddVertLayout(parent,bind:DWORD);
  procedure  OnVertLayout(P:PChar);
  Procedure  EnumVertLayout(cb:TvCustomLayoutCb;Fset:TVkUInt32;FData:PDWORD);
  Procedure  AddBuffLayout(dtype:TVkDescriptorType;parent,bind,size,offset:DWORD);
  Procedure  SetPushConst(parent,size:DWORD);
  procedure  OnBuffLayout(P:PChar);
  Function   GetPushConstData(pUserData:Pointer):Pointer;
  Procedure  AddUnifLayout(dtype:TVkDescriptorType;parent,bind:DWORD);
  procedure  OnUnifLayout(P:PChar);
  Procedure  EnumUnifLayout(cb:TvCustomLayoutCb;Fset:TVkUInt32;FData:PDWORD);
 end;

 TvShaderStage=(
  vShaderStageLs,
  vShaderStageHs,
  vShaderStageEs,
  vShaderStageGs,
  vShaderStageVs,
  vShaderStagePs,
  vShaderStageCs
 );

 AvShaderStage=array[TvShaderStage] of TvShaderExt;

 PvShadersKey=^TvShadersKey;
 TvShadersKey=object
  FShaders:AvShaderStage;
  Procedure SetLSShader(Shader:TvShaderExt);
  Procedure SetHSShader(Shader:TvShaderExt);
  Procedure SetESShader(Shader:TvShaderExt);
  Procedure SetGSShader(Shader:TvShaderExt);
  Procedure SetVSShader(Shader:TvShaderExt);
  Procedure SetPSShader(Shader:TvShaderExt);
  Procedure SetCSShader(Shader:TvShaderExt);
  procedure ExportLayout(var A:AvSetLayout;var B:AvPushConstantRange);
  Procedure ExportStages(Stages:PVkPipelineShaderStageCreateInfo;stageCount:PVkUInt32);
 end;

 TvShaderGroup=class
  FKey:TvShadersKey;
  FLayout:TvPipelineLayout;
  Procedure Clear;
  Function  Compile:Boolean;
 end;

 TAttrBindExt=packed record
  min_addr:Pointer;
  binding:TVkUInt32;
  stride:TVkUInt32;
  count:TVkUInt32;
 end;

 TvAttrBuilder=object
  FBindDescs:array of TVkVertexInputBindingDescription;
  FAttrDescs:array of TVkVertexInputAttributeDescription;
  FBindExt:array of TAttrBindExt;
  function  NewBindDesc(binding,stride:TVkUInt32):TVkUInt32;
  procedure NewAttrDesc(location,binding,offset:TVkUInt32;format:TVkFormat);
  procedure PatchAttr(binding,offset:TVkUInt32);
  Procedure AddVSharp(PV:PVSharpResource4;location:DWord);
  procedure AddAttr(const v:TvCustomLayout;Fset:TVkUInt32;FData:PDWORD);
 end;

 TBufBindExt=packed record
  fset:TVkUInt32;
  bind:TVkUInt32;
  offset:TVkUInt32;

  addr:Pointer;
  size:TVkUInt32;
 end;

 TImageBindExt=packed record
  fset:TVkUInt32;
  bind:TVkUInt32;

  FImage:TvImageKey;
  FView:TvImageViewKey;
 end;

 TSamplerBindExt=packed record
  fset:TVkUInt32;
  bind:TVkUInt32;

  PS:PSSharpResource4;
 end;

 TvUniformBuilder=object
  FBuffers :array of TBufBindExt;
  FImages  :array of TImageBindExt;
  FSamplers:array of TSamplerBindExt;

  Procedure AddVSharp(PV:PVSharpResource4;fset,bind,offset:DWord);
  Procedure AddBufPtr(P:Pointer;fset,size,bind,offset:DWord);

  Procedure AddTSharp4(PT:PTSharpResource4;fset,bind:DWord);
  Procedure AddSSharp4(PS:PSSharpResource4;fset,bind:DWord);
  procedure AddAttr(const b:TvCustomLayout;Fset:TVkUInt32;FData:PDWORD);
 end;

 TvBufOffsetChecker=object
  FResult:Boolean;
  procedure AddAttr(const b:TvCustomLayout;Fset:TVkUInt32;FData:PDWORD);
 end;

 TvFuncLayout=object
  FList:array of ADataLayout;
  Procedure Add(addr:ADataLayout);
 end;

function GetSharpByPatch(pData:Pointer;addr:ADataLayout):Pointer;

implementation

procedure TvShaderExt.ClearInfo;
begin
 inherited;
 FSetLayout:=nil;

 FDataLayouts:=Default(ADataLayout);
 FVertLayouts:=Default(ACustomLayout);
 FUnifLayouts:=Default(ACustomLayout);

 FPushConst:=Default(TvCustomLayout);
end;

procedure TvShaderExt.InitSetLayout;
var
 i:Integer;
 A:AVkDescriptorSetLayoutBinding;
begin
 if (FSetLayout<>nil) then Exit;
 A:=Default(AVkDescriptorSetLayoutBinding);
 SetLength(A,Length(FUnifLayouts)); //++ other todo

 if (Length(FUnifLayouts)<>0) then
  For i:=0 to High(FUnifLayouts) do
  begin
   A[i]:=Default(TVkDescriptorSetLayoutBinding);
   A[i].binding        :=FUnifLayouts[i].bind;
   A[i].descriptorType :=TVkDescriptorType(FUnifLayouts[i].dtype);
   A[i].descriptorCount:=1;
   A[i].stageFlags     :=ord(FStage);
  end;

 FSetLayout:=FetchSetLayout(ord(FStage),0,A);
end;

procedure TvShaderExt.AddToPipeline(p:TvPipelineLayout);
begin
 InitSetLayout;

 p.AddLayout(FSetLayout);
 if (FPushConst.size<>0) then
 begin
  p.AddPushConst(0,FPushConst.size,ord(FStage));
 end;
end;

procedure TvShaderExt.OnDescriptorSet(var Target,id:DWORD);
begin
 if (FDescSetId>=0) then id:=FDescSetId;
end;

function _get_hex_dword(P:PChar):DWord;
var
 Error:word;
 s:string[9];
begin
 s[0]:=#9;
 s[1]:='$';
 PQWORD(@s[2])^:=PQWORD(P)^;
 Result:=0;
 Val(s,Result,Error);
end;

//0123456789ABCDEF0123456789ABCDEF012345678
//#B;PID=00000000;OFS=00000000
//VA;PID=00000004;BND=00000000
//BP;PID=00000003;BND=00000000;LEN=00000040
//UI;PID=00000001;BND=00000000
//US;PID=00000002;BND=00000001

procedure TvShaderExt.OnSourceExtension(P:PChar);
begin
 //Writeln(P);
 Case P^ of
  '#':OnDataLayout(P);
  'V':OnVertLayout(P);
  'B':OnBuffLayout(P);
  'U':OnUnifLayout(P);
 end;
end;

Procedure TvShaderExt.AddDataLayout(rtype:TvResourceType;parent,offset:DWORD);
var
 i:Integer;
begin
 i:=Length(FDataLayouts);
 SetLength(FDataLayouts,i+1);
 FDataLayouts[i].rtype :=rtype;
 FDataLayouts[i].parent:=parent;
 FDataLayouts[i].offset:=offset;
end;

procedure TvShaderExt.OnDataLayout(P:PChar);
begin
 Case P[1] of
  'R':AddDataLayout(vtRoot   ,_get_hex_dword(@P[7]),_get_hex_dword(@P[$14]));
  'B':AddDataLayout(vtBufPtr2,_get_hex_dword(@P[7]),_get_hex_dword(@P[$14]));
  'F':AddDataLayout(vtFunPtr2,_get_hex_dword(@P[7]),_get_hex_dword(@P[$14]));
  'V':AddDataLayout(vtVSharp4,_get_hex_dword(@P[7]),_get_hex_dword(@P[$14]));
  'S':AddDataLayout(vtSSharp4,_get_hex_dword(@P[7]),_get_hex_dword(@P[$14]));
  't':AddDataLayout(vtTSharp4,_get_hex_dword(@P[7]),_get_hex_dword(@P[$14]));
  'T':AddDataLayout(vtTSharp8,_get_hex_dword(@P[7]),_get_hex_dword(@P[$14]));
 end;
end;

Procedure TvShaderExt.EnumFuncLayout(cb:TvFuncCb);
var
 i:Integer;
begin
 if (cb=nil) then Exit;
 if (Length(FDataLayouts)=0) then Exit;
 For i:=0 to High(FDataLayouts) do
 if (FDataLayouts[i].rtype=vtFunPtr2) then
 begin
  cb(GetLayoutAddr(i));
 end;
end;

function TvShaderExt.GetLayoutAddr(parent:DWORD):ADataLayout;
var
 i:Integer;
begin
 Result:=Default(ADataLayout);
 repeat
  if (parent>=Length(FDataLayouts)) then
  begin
   SetLength(Result,0);
   Break;
  end;
  i:=Length(Result);
  SetLength(Result,i+1);
  Result[i]:=FDataLayouts[parent];
  if (parent=0) then Break;
  parent:=FDataLayouts[parent].parent;
 until false;
end;

Procedure AddToCustomLayout(var A:ACustomLayout;const v:TvCustomLayout);
var
 i:Integer;
begin
 i:=Length(A);
 SetLength(A,i+1);
 A[i]:=v;
end;

Procedure TvShaderExt.AddVertLayout(parent,bind:DWORD);
var
 v:TvCustomLayout;
begin
 v:=Default(TvCustomLayout);
 v.bind:=bind;
 v.addr:=GetLayoutAddr(parent);

 AddToCustomLayout(FVertLayouts,v);
end;

procedure TvShaderExt.OnVertLayout(P:PChar);
begin
 Case P[1] of
  'A':AddVertLayout(_get_hex_dword(@P[7]),_get_hex_dword(@P[$14]));
 end;
end;

Procedure TvShaderExt.EnumVertLayout(cb:TvCustomLayoutCb;Fset:TVkUInt32;FData:PDWORD);
var
 i:Integer;
begin
 if (cb=nil) then Exit;
 if (Length(FVertLayouts)=0) then Exit;
 For i:=0 to High(FVertLayouts) do
 begin
  cb(FVertLayouts[i],Fset,FData);
 end;
end;

Procedure TvShaderExt.AddBuffLayout(dtype:TVkDescriptorType;parent,bind,size,offset:DWORD);
var
 v:TvCustomLayout;
begin
 if (dtype=VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER) then
 begin
  if (size>$FFFF) then //max UBO
  begin
   dtype:=VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
  end else
  begin
   dtype:=VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
  end;
 end;

 v:=Default(TvCustomLayout);
 v.dtype:=ord(dtype);
 v.bind:=bind;
 v.size:=size;
 v.offset:=offset;
 v.addr:=GetLayoutAddr(parent);

 AddToCustomLayout(FUnifLayouts,v);
end;

Procedure TvShaderExt.SetPushConst(parent,size:DWORD);
begin
 FPushConst:=Default(TvCustomLayout);
 FPushConst.size:=size;
 FPushConst.addr:=GetLayoutAddr(parent)
end;

//BS;PID=00000002;BND=00000001;LEN=FFFFFFFF;OFS=00000000"
//0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
//0               1               2
procedure TvShaderExt.OnBuffLayout(P:PChar);
begin
 Case P[1] of
  'P':SetPushConst(_get_hex_dword(@P[7]),_get_hex_dword(@P[$21]));
  'U':AddBuffLayout(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                    _get_hex_dword(@P[7]),
                    _get_hex_dword(@P[$14]),
                    _get_hex_dword(@P[$21]),
                    _get_hex_dword(@P[$2E]));
  'S':AddBuffLayout(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                    _get_hex_dword(@P[7]),
                    _get_hex_dword(@P[$14]),
                    _get_hex_dword(@P[$21]),
                    _get_hex_dword(@P[$2E]));
 end;
end;

Function TvShaderExt.GetPushConstData(pUserData:Pointer):Pointer;
begin
 Result:=nil;
 if (pUserData=nil) then Exit;
 if (FPushConst.size=0) then Exit;
 Result:=GetSharpByPatch(pUserData,FPushConst.addr);

 if (Result=nil) then Exit;

 Case FPushConst.addr[0].rtype of
  vtVSharp4:Result:=Pointer(PVSharpResource4(Result)^.base);
  vtTSharp4,
  vtTSharp8:Result:=Pointer(PTSharpResource4(Result)^.base shl 8);
  else;
 end;

end;

Procedure TvShaderExt.AddUnifLayout(dtype:TVkDescriptorType;parent,bind:DWORD);
var
 v:TvCustomLayout;
begin
 v:=Default(TvCustomLayout);
 v.dtype:=ord(dtype);
 v.bind:=bind;
 v.addr:=GetLayoutAddr(parent);

 AddToCustomLayout(FUnifLayouts,v);
end;

procedure TvShaderExt.OnUnifLayout(P:PChar);
begin
 Case P[1] of
  'I':AddUnifLayout(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,_get_hex_dword(@P[7]),_get_hex_dword(@P[$14]));
  'S':AddUnifLayout(VK_DESCRIPTOR_TYPE_SAMPLER      ,_get_hex_dword(@P[7]),_get_hex_dword(@P[$14]));
 end;
end;

Procedure TvShaderExt.EnumUnifLayout(cb:TvCustomLayoutCb;Fset:TVkUInt32;FData:PDWORD);
var
 i:Integer;
begin
 if (cb=nil) then Exit;
 if (Length(FUnifLayouts)=0) then Exit;
 For i:=0 to High(FUnifLayouts) do
 begin
  cb(FUnifLayouts[i],Fset,FData);
 end;
end;

///

function GetSharpByPatch(pData:Pointer;addr:ADataLayout):Pointer;
var
 i:Integer;
 pSharp:Pointer;
begin
 Result:=nil;
 if (Length(addr)=0) then Exit;

 pSharp:=pData;

 For i:=High(addr) downto 0 do
 begin
  pData:=pData+addr[i].offset;

  Case addr[i].rtype of
   vtRoot:
     begin
      pSharp:=pData;
     end;
   vtBufPtr2:
     begin
      pData:=Pointer(PPtrUint(pData)^ and (not 3));
      pSharp:=pData;
     end;
   vtVSharp4:
     begin
      pSharp:=pData;
      pData:=Pointer(PVSharpResource4(pData)^.base);
     end;
   vtSSharp4:
     begin
      pSharp:=pData;
      Break;
     end;
   vtTSharp4,
   vtTSharp8:
     begin
      pSharp:=pData;
      pData:=Pointer(PTSharpResource4(pData)^.base shl 8);
     end;
   else
    Assert(false);
  end;

 end;

 Result:=pSharp;
end;

//

function TvAttrBuilder.NewBindDesc(binding,stride:TVkUInt32):TVkUInt32;
var
 i:Integer;
begin
 i:=Length(FBindDescs);
 SetLength(FBindDescs,i+1);
 FBindDescs[i]:=Default(TVkVertexInputBindingDescription);
 FBindDescs[i].binding  :=binding;
 FBindDescs[i].stride   :=stride;
 FBindDescs[i].inputRate:=VK_VERTEX_INPUT_RATE_VERTEX;
 Result:=i;
end;

procedure TvAttrBuilder.NewAttrDesc(location,binding,offset:TVkUInt32;format:TVkFormat);
var
 i:Integer;
begin
 i:=Length(FAttrDescs);
 SetLength(FAttrDescs,i+1);
 FAttrDescs[i].location:=location;
 FAttrDescs[i].binding :=binding ;
 FAttrDescs[i].format  :=format  ;
 FAttrDescs[i].offset  :=offset  ;
end;

procedure TvAttrBuilder.PatchAttr(binding,offset:TVkUInt32);
var
 i:Integer;
begin
 if Length(FAttrDescs)<>0 then
  For i:=0 to High(FAttrDescs) do
  if (FAttrDescs[i].binding=binding) then
  begin
   FAttrDescs[i].offset:=FAttrDescs[i].offset+offset;
  end;
end;

function _ptr_diff(p1,p2:Pointer):TVkUInt32;
begin
 if (p1>p2) then
  Result:=p1-p2
 else
  Result:=p2-p1;
end;

Procedure TvAttrBuilder.AddVSharp(PV:PVSharpResource4;location:DWord);
var
 i:Integer;
begin
 if (PV=nil) then Exit;

 if Length(FBindExt)<>0 then
  For i:=0 to High(FBindExt) do
   With FBindExt[i] do
    if (stride=PV^.stride) then
    if (_ptr_diff(min_addr,Pointer(PV^.base))<=stride-1) then
    begin
     if (min_addr>Pointer(PV^.base)) then
     begin
      PatchAttr(binding,min_addr-Pointer(PV^.base));
      min_addr:=Pointer(PV^.base);
     end;
     if (count<PV^.num_records) then
     begin
      count:=PV^.num_records;
     end;
     NewAttrDesc(location,binding,Pointer(PV^.base)-min_addr,_get_vsharp_cformat(PV));
     Exit;
    end;

 i:=Length(FBindExt);
 SetLength(FBindExt,i+1);
 FBindExt[i]:=Default(TAttrBindExt);

 FBindExt[i].min_addr:=Pointer(PV^.base);
 FBindExt[i].binding :=i;
 FBindExt[i].stride  :=PV^.stride;
 FBindExt[i].count   :=PV^.num_records;

 NewBindDesc(i,PV^.stride);

 NewAttrDesc(location,i,0,_get_vsharp_cformat(PV));

end;

procedure TvAttrBuilder.AddAttr(const v:TvCustomLayout;Fset:TVkUInt32;FData:PDWORD);
var
 PV:PVSharpResource4;
begin
 PV:=GetSharpByPatch(FData,v.addr);
 //print_vsharp(PV);
 AddVSharp(PV,v.bind);
end;

//

Procedure TvUniformBuilder.AddVSharp(PV:PVSharpResource4;fset,bind,offset:DWord);
var
 b:TBufBindExt;
 i,stride,num_records:Integer;
begin
 Assert(PV<>nil);
 if (PV=nil) then Exit;

 //print_vsharp(PV);

 b:=Default(TBufBindExt);
 b.fset:=fset;
 b.bind:=bind;
 b.offset:=offset;

 b.addr:=Pointer(PV^.base);

 stride:=PV^.stride;
 num_records:=PV^.num_records;
 if (stride=0) then stride:=1;
 if (num_records=0) then num_records:=1;
 b.size:=stride*num_records;

 i:=Length(FBuffers);
 SetLength(FBuffers,i+1);
 FBuffers[i]:=b;
end;

Procedure TvUniformBuilder.AddBufPtr(P:Pointer;fset,size,bind,offset:DWord);
var
 b:TBufBindExt;
 i:Integer;
begin
 Assert(P<>nil);
 if (P=nil) or (size=0) then Exit;

 b:=Default(TBufBindExt);
 b.fset:=fset;
 b.bind:=bind;
 b.offset:=offset;

 b.addr:=P;
 b.size:=size;

 i:=Length(FBuffers);
 SetLength(FBuffers,i+1);
 FBuffers[i]:=b;
end;

Procedure TvUniformBuilder.AddTSharp4(PT:PTSharpResource4;fset,bind:DWord);
var
 b:TImageBindExt;
 i:Integer;
begin
 Assert(PT<>nil);
 if (PT=nil) then Exit;

 //print_tsharp4(PT);

 b:=Default(TImageBindExt);
 b.fset:=fset;
 b.bind:=bind;

 b.FImage:=_get_tsharp4_image_info(PT);
 b.FView :=_get_tsharp4_image_view(PT);

 i:=Length(FImages);
 SetLength(FImages,i+1);
 FImages[i]:=b;
end;

procedure TvUniformBuilder.AddAttr(const b:TvCustomLayout;Fset:TVkUInt32;FData:PDWORD);
var
 P:Pointer;
begin
 P:=GetSharpByPatch(FData,b.addr);
 Assert(P<>nil);
 if (P=nil) then Exit;

 Case TVkDescriptorType(b.dtype) of
  VK_DESCRIPTOR_TYPE_SAMPLER:
    Case b.addr[0].rtype of
     vtSSharp4:AddSSharp4(P,fset,b.bind);
     else
      Assert(false);
    end;
  VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE:
    Case b.addr[0].rtype of
     vtTSharp4:AddTSharp4(P,fset,b.bind);
     vtTSharp8:
       begin
        print_tsharp8(P);
        Assert(false);
       end;
     else
      Assert(false);
    end;
  //VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER=4,
  //VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER=5,
  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER:
    Case b.addr[0].rtype of
     vtRoot,
     vtBufPtr2:AddBufPtr(P,Fset,b.size,b.bind,b.offset);
     vtVSharp4:AddVSharp(P,Fset,b.bind,b.offset);
     else
      Assert(false);
    end;

  else
   Assert(false);
 end;

 //Writeln('----');
end;

function AlignShift(addr:Pointer;alignment:PtrUInt):PtrUInt; inline;
begin
 if (alignment>1) then
 begin
  Result:=(PtrUInt(addr) mod alignment);
 end else
 begin
  Result:=0;
 end;
end;

Procedure TvUniformBuilder.AddSSharp4(PS:PSSharpResource4;fset,bind:DWord);
var
 b:TSamplerBindExt;
 i:Integer;
begin
 Assert(PS<>nil);
 if (PS=nil) then Exit;

 //print_ssharp4(PS);

 b:=Default(TSamplerBindExt);
 b.fset:=fset;
 b.bind:=bind;
 b.PS:=PS;

 i:=Length(FSamplers);
 SetLength(FSamplers,i+1);
 FSamplers[i]:=b;
end;

//

Procedure TvShadersKey.SetLSShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_VERTEX_BIT) then
  FShaders[vShaderStageLs]:=Shader;
end;

Procedure TvShadersKey.SetHSShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) then
  FShaders[vShaderStageHs]:=Shader;
end;

Procedure TvShadersKey.SetESShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) then
  FShaders[vShaderStageEs]:=Shader;
end;

Procedure TvShadersKey.SetGSShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_GEOMETRY_BIT) then
  FShaders[vShaderStageGs]:=Shader;
end;

Procedure TvShadersKey.SetVSShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_VERTEX_BIT) then
  FShaders[vShaderStageVs]:=Shader;
end;

Procedure TvShadersKey.SetPSShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_FRAGMENT_BIT) then
  FShaders[vShaderStagePs]:=Shader;
end;

Procedure TvShadersKey.SetCSShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_COMPUTE_BIT) then
  FShaders[vShaderStageCs]:=Shader;
end;

procedure TvShadersKey.ExportLayout(var A:AvSetLayout;
                                    var B:AvPushConstantRange);
var
 i:TvShaderStage;
 c,p:Integer;
begin
 c:=0;
 p:=0;

 For i:=Low(TvShaderStage) to High(TvShaderStage) do
 begin
  if (FShaders[i]<>nil) then
  begin
   FShaders[i].InitSetLayout;

   SetLength(A,c+1);
   A[c]:=FShaders[i].FSetLayout;
   Inc(c);

   if (FShaders[i].FPushConst.size<>0) then
   begin
    SetLength(B,p+1);

    B[p]:=Default(TVkPushConstantRange);
    B[p].stageFlags:=ord(FShaders[i].FStage);
    B[p].size      :=FShaders[i].FPushConst.size;

    Inc(p);
   end;

  end;
 end;
end;

Procedure TvShadersKey.ExportStages(Stages:PVkPipelineShaderStageCreateInfo;stageCount:PVkUInt32);
var
 i:TvShaderStage;
 c:Integer;
begin
 c:=0;
 For i:=Low(TvShaderStage) to High(TvShaderStage) do
  if (FShaders[i]<>nil) then
  begin
   Stages[c].sType :=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
   Stages[c].stage :=FShaders[i].FStage;
   Stages[c].module:=FShaders[i].FHandle;
   Stages[c].pName :=PChar(FShaders[i].FEntry);
   Inc(c);
  end;
 stageCount^:=c;
end;

Procedure TvShaderGroup.Clear;
begin
 FKey:=Default(TvShadersKey);
 FLayout:=nil;
end;

Function TvShaderGroup.Compile:Boolean;
var
 A:AvSetLayout;
 B:AvPushConstantRange;
begin
 Result:=True;
 if (FLayout<>nil) then Exit;

 A:=Default(AvSetLayout);
 B:=Default(AvPushConstantRange);

 FKey.ExportLayout(A,B);

 FLayout:=FetchPipelineLayout(A,B);
 Result:=(FLayout<>nil);
end;

procedure TvBufOffsetChecker.AddAttr(const b:TvCustomLayout;Fset:TVkUInt32;FData:PDWORD);
var
 P:Pointer;
 a:QWORD;
begin
 if not FResult then Exit;

 P:=GetSharpByPatch(FData,b.addr);
 if (P=nil) then Exit;

 Case TVkDescriptorType(b.dtype) of
  //VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER=4,
  //VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER=5,
  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER:
    Case b.addr[0].rtype of
     vtRoot,
     vtBufPtr2:
       begin
        a:=AlignShift(P,limits.minStorageBufferOffsetAlignment);
        if (a<>b.offset) then FResult:=False;
       end;
     vtVSharp4:
       begin
        a:=AlignShift(Pointer(PVSharpResource4(P)^.base),limits.minStorageBufferOffsetAlignment);
        if (a<>b.offset) then FResult:=False;
       end;
     else
      Assert(false);
    end;

  else;
 end;
end;

Procedure TvFuncLayout.Add(addr:ADataLayout);
var
 i:Integer;
begin
 i:=Length(FList);
 SetLength(FList,i+1);
 FList[i]:=addr;
end;

end.




