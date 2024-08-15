unit srUniform;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 spirv,
 ginodes,
 srNode,
 srRefId,
 srType,
 srTypes,
 srLayout,
 srVariable,
 srCapability,
 srDecorate,
 srConfig;

type
 String2=String[2];

 TsrRegUniform=class(TsrNode)
  private
   ID:TsrRefId;            //post id
   //
   FWriter:TsrNode;
   FVar:TsrVariable;
   Procedure SetWriter(t:TsrNode);
  public
   //
   function  _Down        :TsrNode;         override;
   Procedure _SetWriter   (w,line:TsrNode); override;
   Procedure _ResetWriter (w:TsrNode);      override;
   function  _GetPrintName:RawByteString;   override;
   function  _GetRef      :Pointer;         override;
   //
   property  pLine:TsrNode read FWriter;
   Procedure Init(pVar:TsrVariable); inline;
   function  GetPrintName:RawByteString;
 end;

 ntRegUniform=TsrRegUniform;

 TsrArrayChain=class(TsrRegUniform)
  pNext:TsrArrayChain;
  //
  idx0:TsrNode;
 end;

 TsrArrayChainList=specialize TNodeStackClass<TsrArrayChain>;

 PsrUniformKey=^TsrUniformKey;
 TsrUniformKey=record
  pLayout:TsrDataLayout;
  FType  :TsrType;
 end;

 TsrUniform=class(TsrDescriptor)
  public
   pLeft,pRight:TsrUniform;
   class function c(n1,n2:PsrUniformKey):Integer; static;
  private
   key:TsrUniformKey;
   //
   FReg:TsrRegUniform;
   FArrayChainList:TsrArrayChainList;
  public
   FMipArray:Boolean;
   //
   function  _GetStorageName:RawByteString; override;
   //
   Procedure Init(); inline;
   property  pReg:TsrRegUniform read FReg;
   function  FetchArrayChain(pLine:TsrNode;idx0:TsrNode):TsrArrayChain;
   function  chain_read:DWORD;
   function  chain_write:DWORD;
   function  GetStorageName:RawByteString;
   function  GetTypeChar:String2;
   function  GetRw:Char;
   function  GetString:RawByteString;
 end;

 ntUniform=TsrUniform;

 PsrUniformList=^TsrUniformList;
 TsrUniformList=object
  type
   TNodeTree=specialize TNodeTreeClass<TsrUniform>;
  var
   FEmit:TCustomEmit;
   FTree:TNodeTree;
  procedure Init(Emit:TCustomEmit);
  function  Fetch(s:TsrDataLayout;t:TsrType):TsrUniform;
  Function  First:TsrUniform;
  Function  Next(node:TsrUniform):TsrUniform;
  procedure AllocBinding(Var FBinding:Integer);
  procedure AllocSourceExtension;
 end;

implementation

uses
 emit_op;

function TsrRegUniform._Down:TsrNode;
begin
 Result:=FVar;
end;

Procedure TsrRegUniform._SetWriter(w,line:TsrNode);
begin
 SetWriter(w);
end;

Procedure TsrRegUniform._ResetWriter(w:TsrNode);
begin
 if (FWriter=w) then
 begin
  SetWriter(nil);
 end;
end;

function TsrRegUniform._GetPrintName:RawByteString;
begin
 Result:=GetPrintName;
end;

function TsrRegUniform._GetRef:Pointer;
begin
 Result:=@ID;
end;

//

function TsrUniform._GetStorageName:RawByteString;
begin
 Result:=GetStorageName;
end;

//

Procedure TsrRegUniform.Init(pVar:TsrVariable); inline;
begin
 FVar:=pVar;
end;

Procedure TsrRegUniform.SetWriter(t:TsrNode);
begin
 if (FWriter=t) then Exit;
 if isUsed then
 begin
        t.mark_read  (Self);
  FWriter.mark_unread(Self);
 end;
 FWriter:=t;
end;

function TsrRegUniform.GetPrintName:RawByteString;
begin
 Result:=FVar.GetStorageName;
 if (Result<>'') then
 begin
  Result:='r_'+Result;
 end else
 begin
  Assert(ID.Alloc);
  Result:='r'+IntToStr(ID.ID);
 end;
end;

//

class function TsrUniform.c(n1,n2:PsrUniformKey):Integer;
begin
 //first pLayout
 Result:=ord(n1^.pLayout.Order>n2^.pLayout.Order)-ord(n1^.pLayout.Order<n2^.pLayout.Order);
 if (Result<>0) then Exit;
 //second pType (order sort)
 Result:=ord(n1^.FType.Order>n2^.FType.Order)-ord(n1^.FType.Order<n2^.FType.Order);
end;

Procedure TsrUniform.Init(); inline;
begin
 FStorage:=StorageClass.UniformConstant;
 FBinding:=-1;
 //
 FReg    :=Emit.specialize New<TsrRegUniform>;
end;

function TsrUniform.FetchArrayChain(pLine:TsrNode;idx0:TsrNode):TsrArrayChain;
var
 iType:TsrType;
 pChain:TsrNode;
begin
 Result:=nil;

 iType:=pType.GetItem(0).specialize AsType<ntType>; //Image
 if (iType=nil) then Exit;

 //Check dublicate?
 pChain:=TEmitOp(Emit).OpAccessChainTo(iType,pVar,idx0,@pLine);

 Result:=Emit.specialize New<TsrArrayChain>;
 Result.idx0:=idx0;

 TEmitOp(Emit).OpLoad(pLine,iType,Result,pChain);

 //
 FArrayChainList.Push_head(Result);
end;

function TsrUniform.GetStorageName:RawByteString;
label
 _image_info;
var
 image_info:TsrTypeImageInfo;
 pChild:TsrType;
begin
 Result:='';
 if (FType<>nil) then
   Case FType.dtype of
    dtTypeImage:
     begin
      image_info:=FType.image_info;

      _image_info:

      if (image_info.Dim=Dim.Buffer) then
      begin
       Case image_info.Sampled of
           1:Result:='uTex'+IntToStr(FBinding);
           2:Result:='sTex'+IntToStr(FBinding);
        else Result:='rTex'+IntToStr(FBinding);
       end;
      end else
      begin
       Case image_info.Sampled of
           1:Result:='uImg'+IntToStr(FBinding);
           2:Result:='sImg'+IntToStr(FBinding);
        else Result:='rImg'+IntToStr(FBinding);
       end;
      end;
     end;
    //
    dtTypeSampler:Result:='uSmp'+IntToStr(FBinding);
    //
    dtTypeArray,
    dtTypeRuntimeArray:
     begin
      pChild:=pType.GetItem(0).specialize AsType<ntType>; //Image

      if (pChild<>nil) then
      if (pChild.dtype=dtTypeImage) then
      begin
       image_info:=pChild.image_info;

       goto _image_info;
      end;

     end;
    else;
   end;
end;

//TU
//TS
//TR

//IU
//IS
//IR

//US

//RU

function GetSampledTypeChar(Sampled:Byte):Char;
begin
 Case Sampled of
     1:Result:='U'; //VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER | VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
     2:Result:='S'; //VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER | VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
  else Result:='R'; //runtime texel buffer
 end;
end;

function TsrUniform.GetTypeChar:String2;
var
 image_info:TsrTypeImageInfo;
 pChild:TsrType;
begin
 Result:='';
 if (FType<>nil) then
   Case FType.dtype of
    dtTypeImage:
     begin
      image_info:=FType.image_info;

      if (image_info.Dim=Dim.Buffer) then
      begin
       Result:='T'+GetSampledTypeChar(image_info.Sampled);
      end else
      begin
       Result:='I'+GetSampledTypeChar(image_info.Sampled);
      end;
     end;
    //
    dtTypeSampler:Result:='US'; //VK_DESCRIPTOR_TYPE_SAMPLER
    //
    dtTypeArray,
    dtTypeRuntimeArray:
     begin
      pChild:=pType.GetItem(0).specialize AsType<ntType>; //Image

      if (pChild<>nil) then
      if (pChild.dtype=dtTypeImage) then
      begin
       image_info:=pChild.image_info;

       if (image_info.Dim=Dim.Buffer) then
       begin
        Assert(false,'GetTypeChar');
       end else
       begin
        Case FType.dtype of
         dtTypeArray       :Result:='A'+GetSampledTypeChar(image_info.Sampled);
         dtTypeRuntimeArray:Result:='R'+GetSampledTypeChar(image_info.Sampled);
         else;
        end;
       end;
      end;

     end;
    else
     Assert(false,'GetTypeChar');
   end;
end;

function TsrUniform.chain_read:DWORD;
var
 node:TsrArrayChain;
begin
 Result:=0;
 if FReg.IsUsed then
 begin
  Result:=FReg.read_count;
 end;
 //
 node:=FArrayChainList.pHead;
 While (node<>nil) do
 begin
  if node.IsUsed then
  begin
   Result:=Result+node.read_count;
  end;
  node:=node.pNext;
 end;
end;

function TsrUniform.chain_write:DWORD;
var
 node:TsrArrayChain;
begin
 Result:=0;
 if FReg.IsUsed then
 begin
  Result:=FReg.write_count;
 end;
 //
 node:=FArrayChainList.pHead;
 While (node<>nil) do
 begin
  if node.IsUsed then
  begin
   Result:=Result+node.write_count;
  end;
  node:=node.pNext;
 end;
end;

function TsrUniform.GetRw:Char;
begin
 Result:='0';
 if (chain_read<>0) then
 begin
  Result:='1';
 end;
 if (chain_write<>0) then
 begin
  Result:=Char(ord(Result) or ord('2'));
 end;
 if (FMipArray) then
 begin
  Result:=Char(ord(Result) or ord('4'));
 end;
end;

function TsrUniform.GetString:RawByteString;
var
 PID:DWORD;
begin
 PID:=0;
 if (key.pLayout<>nil) then
 begin
  PID:=key.pLayout.FID;
 end;
 Result:=GetTypeChar+
         ';PID='+HexStr(PID,8)+
         ';BND='+HexStr(FBinding,8)+
         ';MRW='+GetRw;
end;

procedure TsrUniformList.Init(Emit:TCustomEmit);
begin
 FEmit:=Emit;
end;

function TsrUniformList.Fetch(s:TsrDataLayout;t:TsrType):TsrUniform;
var
 key:TsrUniformKey;
begin
 key:=Default(TsrUniformKey);
 key.pLayout:=s;
 key.FType  :=t;
 //
 Result:=FTree.Find(@key);
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrUniform>;
  Result.Init();
  Result.key  :=key;
  Result.pType:=t;
  //
  Result.InitVar();
  Result.FReg.Init(Result.FVar);
  //
  FTree.Insert(Result);
 end;
end;

Function TsrUniformList.First:TsrUniform;
begin
 Result:=FTree.Min;
end;

Function TsrUniformList.Next(node:TsrUniform):TsrUniform;
begin
 Result:=FTree.Next(node);
end;

procedure TsrUniformList.AllocBinding(Var FBinding:Integer);
var
 pConfig:PsrConfig;
 pDecorateList:PsrDecorateList;
 pCapabilityList:PsrCapabilityList;
 //
 node:TsrUniform;
 pVar:TsrVariable;
 //
 FType:TsrType;
 image_info:TsrTypeImageInfo;
begin
 pConfig        :=FEmit.GetConfig;
 pDecorateList  :=FEmit.GetDecorateList;
 pCapabilityList:=FEmit.GetCapabilityList;

 node:=First;
 While (node<>nil) do
 begin
  pVar:=node.pVar;
  if (pVar<>nil) and node.IsUsed and (node.FBinding=-1) then
  begin
   pDecorateList^.OpDecorate(pVar,Decoration.Binding      ,FBinding);
   pDecorateList^.OpDecorate(pVar,Decoration.DescriptorSet,pConfig^.DescriptorSet);
   //
   if (node.Flags.Coherent) then
   begin
    pDecorateList^.OpDecorate(pVar,Decoration.Coherent,0);
   end;
   //
   if (node.Flags.Volatile) then
   begin
    pDecorateList^.OpDecorate(pVar,Decoration.Volatile,0);
   end;
   //
   if (node.Flags.Aliased) and (not node.Flags.Bitcast) then
   begin
    pDecorateList^.OpDecorate(pVar,Decoration.Aliased,0);
   end;
   //
   node.FBinding:=FBinding;
   Inc(FBinding);

   FType:=node.pType;
   if (FType<>nil) then
   begin
    Case FType.dtype of
     dtTypeImage:
      begin
       image_info:=FType.image_info;

       if (image_info.Sampled=2) then //storage image
       begin
        if (node.chain_read=0) then
        begin
         pDecorateList^.OpDecorate(pVar,Decoration.NonReadable,0);
        end;
        if (node.chain_write=0) then
        begin
         pDecorateList^.OpDecorate(pVar,Decoration.NonWritable,0);
        end;
       end;

       Case image_info.Dim of
        Dim.Dim1D:
         Case image_info.Sampled of
          1:pCapabilityList^.Add(Capability.Sampled1D); //sampling
          2:pCapabilityList^.Add(Capability.Image1D);   //read/write
          else;
         end;
        Dim.Buffer:
         Case image_info.Sampled of
          1:pCapabilityList^.Add(Capability.SampledBuffer); //sampling
          2:pCapabilityList^.Add(Capability.ImageBuffer);   //read/write
          else;
         end;
        else;
       end;

       if (image_info.Sampled=2) and
          (image_info.Arrayed=1) then
       begin
        pCapabilityList^.Add(Capability.ImageMSArray);
       end;

       Case image_info.Format of

        ImageFormat.Unknown:
         if (image_info.Sampled=2) then //storage image
         begin
          if (node.chain_read<>0) then
          begin
           pCapabilityList^.Add(Capability.StorageImageWriteWithoutFormat);
          end;
          if (node.chain_write<>0) then
          begin
           pCapabilityList^.Add(Capability.StorageImageWriteWithoutFormat);
          end;
         end;

        ImageFormat.Rg32f       ,
        ImageFormat.Rg16f       ,
        ImageFormat.R11fG11fB10f,
        ImageFormat.R16f        ,
        ImageFormat.Rgba16      ,
        ImageFormat.Rgb10A2     ,
        ImageFormat.Rg16        ,
        ImageFormat.Rg8         ,
        ImageFormat.R16         ,
        ImageFormat.R8          ,
        ImageFormat.Rgba16Snorm ,
        ImageFormat.Rg16Snorm   ,
        ImageFormat.Rg8Snorm    ,
        ImageFormat.R16Snorm    ,
        ImageFormat.R8Snorm     ,
        ImageFormat.Rg32i       ,
        ImageFormat.Rg16i       ,
        ImageFormat.Rg8i        ,
        ImageFormat.R16i        ,
        ImageFormat.R8i         ,
        ImageFormat.Rgb10a2ui   ,
        ImageFormat.Rg32ui      ,
        ImageFormat.Rg16ui      ,
        ImageFormat.Rg8ui       ,
        ImageFormat.R16ui       ,
        ImageFormat.R8ui        :pCapabilityList^.Add(Capability.StorageImageExtendedFormats);

        else;
       end;

      end;
     else;
    end;
   end;

  end;
  node:=Next(node);
 end;
end;

procedure TsrUniformList.AllocSourceExtension;
var
 pDebugInfoList:PsrDebugInfoList;
 node:TsrUniform;
 pVar:TsrVariable;
begin
 pDebugInfoList:=FEmit.GetDebugInfoList;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node.pVar;
  if (pVar<>nil) and node.IsUsed then
  begin
   pDebugInfoList^.OpSource(node.GetString);
  end;
  node:=Next(node);
 end;
end;

end.

