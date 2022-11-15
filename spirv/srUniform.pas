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
 ntRegUniform=class(TsrNodeVmt)
  class function  Down        (node:PsrNode):Pointer;       override;
  class Procedure SetWriter   (node,w,line:PsrNode);        override;
  class Procedure ResetWriter (node,w:PsrNode);             override;
  class Function  pwrite_count(node:PsrNode):PDWORD;        override;
  class function  GetPrintName(node:PsrNode):RawByteString; override;
  class function  GetRef      (node:PsrNode):Pointer;       override;
 end;

 ntUniform=class(ntDescriptor)
  class Function  pwrite_count  (node:PsrNode):PDWORD;        override;
  class function  GetStorageName(node:PsrNode):RawByteString; override;
 end;

 String2=String[2];

 PsrRegUniform=^TsrRegUniform;
 TsrRegUniform=packed object(TsrNode)
  private
   fwrite_count:DWORD;
   ID:TsrRefId;            //post id
   //
   FWriter:PsrNode;
   FVar:PsrVariable;
   Procedure SetWriter(t:PsrNode);
  public
   property  pLine:PsrNode read FWriter;
   Procedure Init(pVar:PsrVariable); inline;
   function  GetPrintName:RawByteString;
 end;

 PsrUniform=^TsrUniform;
 TsrUniform=object(TsrDescriptor)
  private
   pLeft,pRight:PsrUniform;
   //----
   pLayout:PsrDataLayout;
   //
   fwrite_count:DWORD;
   //
   FReg:TsrRegUniform;
   function  c(n1,n2:PsrUniform):Integer; static;
  public
   Procedure Init; inline;
   function  pReg:PsrRegUniform; inline;
   function  GetStorageName:RawByteString;
   function  GetTypeChar:String2;
   function  GetString:RawByteString;
 end;

 PsrUniformList=^TsrUniformList;
 TsrUniformList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrUniform,TsrUniform>;
  var
   FEmit:TCustomEmit;
   FNTree:TNodeFetch;
  procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(s:PsrDataLayout;t:PsrType):PsrUniform;
  Function  First:PsrUniform;
  Function  Next(node:PsrUniform):PsrUniform;
  procedure AllocBinding(Var FBinding:Integer);
  procedure AllocSourceExtension;
 end;

implementation

class function ntRegUniform.Down(node:PsrNode):Pointer;
begin
 Result:=PsrRegUniform(node)^.FVar;
end;

class Procedure ntRegUniform.SetWriter(node,w,line:PsrNode);
begin
 With PsrRegUniform(node)^ do
 begin
  SetWriter(w);
 end;
end;

class Procedure ntRegUniform.ResetWriter(node,w:PsrNode);
begin
 With PsrRegUniform(node)^ do
 if (FWriter=w) then
 begin
  SetWriter(nil);
 end;
end;

class Function ntRegUniform.pwrite_count(node:PsrNode):PDWORD;
begin
 Result:=@PsrRegUniform(node)^.fwrite_count;
end;

class function ntRegUniform.GetPrintName(node:PsrNode):RawByteString;
begin
 Result:=PsrRegUniform(node)^.GetPrintName;
end;

class function ntRegUniform.GetRef(node:PsrNode):Pointer;
begin
 Result:=@PsrRegUniform(node)^.ID;
end;

//

class Function ntUniform.pwrite_count(node:PsrNode):PDWORD;
begin
 Result:=@PsrUniform(node)^.fwrite_count;
end;

class function ntUniform.GetStorageName(node:PsrNode):RawByteString;
begin
 Result:=PsrUniform(node)^.GetStorageName;
end;

//

Procedure TsrRegUniform.Init(pVar:PsrVariable); inline;
begin
 fntype:=ntRegUniform;
 FVar:=pVar;
end;

Procedure TsrRegUniform.SetWriter(t:PsrNode);
begin
 if (FWriter=t) then Exit;
 if isUsed then
 begin
        t^.mark_read  (@Self);
  FWriter^.mark_unread(@Self);
 end;
 FWriter:=t;
end;

function TsrRegUniform.GetPrintName:RawByteString;
begin
 Result:=FVar^.GetStorageName;
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

function TsrUniform.c(n1,n2:PsrUniform):Integer;
begin
 //first pLayout
 Result:=Integer(n1^.pLayout>n2^.pLayout)-Integer(n1^.pLayout<n2^.pLayout);
 if (Result<>0) then Exit;
 //second pType
 Result:=Integer(n1^.FType>n2^.FType)-Integer(n1^.FType<n2^.FType);
end;

Procedure TsrUniform.Init; inline;
begin
 fntype  :=ntUniform;
 FStorage:=StorageClass.UniformConstant;
 FBinding:=-1;
end;

function TsrUniform.pReg:PsrRegUniform; inline;
begin
 Result:=@FReg;
end;

function TsrUniform.GetStorageName:RawByteString;
var
 image_info:TsrTypeImageInfo;
begin
 Result:='';
 if (FType<>nil) then
   Case FType^.dtype of
    dtTypeImage:
     begin
      image_info:=FType^.image_info;

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
    dtTypeSampler:Result:='uSmp'+IntToStr(FBinding);
    else;
   end;
end;


function TsrUniform.GetTypeChar:String2;
var
 image_info:TsrTypeImageInfo;
begin
 Result:='';
 if (FType<>nil) then
   Case FType^.dtype of
    dtTypeImage:
     begin
      image_info:=FType^.image_info;

      if (image_info.Dim=Dim.Buffer) then
      begin
       Case image_info.Sampled of
           1:Result:='UB'; //VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
           2:Result:='SB'; //VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
        else Result:='RB'; //runtime texel buffer
       end;
      end else
      begin
       Case image_info.Sampled of
           1:Result:='UI'; //VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
           2:Result:='SI'; //VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
        else Result:='RI'; //runtime image
       end;
      end;
     end;
    dtTypeSampler:Result:='US'; //VK_DESCRIPTOR_TYPE_SAMPLER
    else;
   end;
end;

function TsrUniform.GetString:RawByteString;
var
 PID:DWORD;
begin
 PID:=0;
 if (pLayout<>nil) then
 begin
  PID:=pLayout^.FID;
 end;
 Result:=GetTypeChar+
         ';PID='+HexStr(PID,8)+
         ';BND='+HexStr(FBinding,8);
end;

procedure TsrUniformList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrUniformList.Fetch(s:PsrDataLayout;t:PsrType):PsrUniform;
var
 node:TsrUniform;
begin
 node:=Default(TsrUniform);
 node.Init;
 node.pLayout:=s;
 node.FType  :=t;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrUniform));
  Move(node,Result^,SizeOf(TsrUniform));
  //
  Result^.InitVar(FEmit);
  Result^.FReg.Init(Result^.FVar);
  //
  FNTree.Insert(Result);
 end;
end;

Function TsrUniformList.First:PsrUniform;
begin
 Result:=FNTree.Min;
end;

Function TsrUniformList.Next(node:PsrUniform):PsrUniform;
begin
 Result:=FNTree.Next(node);
end;

procedure TsrUniformList.AllocBinding(Var FBinding:Integer);
var
 pConfig:PsrConfig;
 pDecorateList:PsrDecorateList;
 pCapabilityList:PsrCapabilityList;
 //
 node:PsrUniform;
 pVar:PsrVariable;
 //
 FType:PsrType;
 image_info:TsrTypeImageInfo;
begin
 pConfig        :=FEmit.GetConfig;
 pDecorateList  :=FEmit.GetDecorateList;
 pCapabilityList:=FEmit.GetCapabilityList;

 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) and node^.IsUsed and (node^.FBinding=-1) then
  begin
   pDecorateList^.OpDecorate(pVar,Decoration.Binding,FBinding);
   pDecorateList^.OpDecorate(pVar,Decoration.DescriptorSet,pConfig^.DescriptorSet);

   node^.FBinding:=FBinding;
   Inc(FBinding);

   FType:=node^.pType;
   if (FType<>nil) then
   begin
    Case FType^.dtype of
     dtTypeImage:
      begin
       image_info:=FType^.image_info;

       if (image_info.Sampled=2) then //storage image
       begin
        if (node^.FReg.read_count=0) then
        begin
         pDecorateList^.OpDecorate(pVar,Decoration.NonReadable,0);
        end;
        if (node^.FReg.write_count=0) then
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
         begin
          if (node^.FReg.read_count<>0) then
          begin
           pCapabilityList^.Add(Capability.StorageImageWriteWithoutFormat);
          end;
          if (node^.FReg.write_count<>0) then
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
 node:PsrUniform;
 pVar:PsrVariable;
begin
 pDebugInfoList:=FEmit.GetDebugInfoList;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) and node^.IsUsed then
  begin
   pDebugInfoList^.OpSource(node^.GetString);
  end;
  node:=Next(node);
 end;
end;

end.

