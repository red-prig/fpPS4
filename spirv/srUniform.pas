unit srUniform;

{$mode ObjFPC}{$H+}

interface

uses
  spirv,
  srNodes,
  srTypes,
  srLayout,
  srReg,
  srVariable,
  srDecorate;

type
 String2=String[2];

 PsrUniform=^TsrUniform;
 TsrUniform=object(TsrDescriptor)
  pLeft,pRight:PsrUniform;
  //----

  key:packed record
   pLayout:PsrDataLayout;
   pType:PsrType;
  end;

  pReg:PsrRegNode;

  function c(n1,n2:PsrUniform):Integer; static;
  function GetTypeChar:String2;
  function GetString:RawByteString;
 end;

 TsrUniformList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrUniform,TsrUniform>;
  var
   Alloc:TfnAlloc;
   FNTree:TNodeFetch;
  procedure Init(cb:TfnAlloc);
  function  Fetch(s:PsrDataLayout;t:PsrType):PsrUniform;
  Function  First:PsrUniform;
  Function  Next(node:PsrUniform):PsrUniform;
  procedure AllocBinding(Var FBinding:Integer;Decorates:PsrDecorateList);
  procedure AllocSourceExtension(FDebugInfo:PsrDebugInfoList);
 end;

implementation

function TsrUniform.c(n1,n2:PsrUniform):Integer;
begin
 //first pLayout
 Result:=Integer(n1^.key.pLayout>n2^.key.pLayout)-Integer(n1^.key.pLayout<n2^.key.pLayout);
 if (Result<>0) then Exit;
 //second pType
 Result:=Integer(n1^.key.pType>n2^.key.pType)-Integer(n1^.key.pType<n2^.key.pType);
end;

function TsrUniform.GetTypeChar:String2;
begin
 Result:='';
 if (key.pType<>nil) then
  With key.pType^.key do
   Case key.pType^.dtype of
    dtTypeImage:
     begin
      if (ext.image.Dim=Dim.Buffer) then
      begin
       Case ext.image.Sampled of
           1:Result:='UB'; //VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
           2:Result:='SB'; //VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
        else Result:='RB'; //runtime texel buffer
       end;
      end else
      begin
       Case ext.image.Sampled of
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
 if (key.pLayout<>nil) then
 begin
  PID:=key.pLayout^.FID;
 end;
 Result:=GetTypeChar+
         ';PID='+HexStr(PID,8)+
         ';BND='+HexStr(FBinding,8);
end;

procedure TsrUniformList.Init(cb:TfnAlloc);
begin
 Alloc:=cb;
end;

function TsrUniformList.Fetch(s:PsrDataLayout;t:PsrType):PsrUniform;
var
 node:TsrUniform;
begin
 node:=Default(TsrUniform);
 node.key.pLayout:=s;
 node.key.pType:=t;
 node.FStorage:=StorageClass.UniformConstant;
 node.FBinding:=-1;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=Alloc(SizeOf(TsrUniform));
  Move(node,Result^,SizeOf(TsrUniform));
  FNTree.Insert(Result);
 end else
 begin
  t^.mark_unread;
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

procedure TsrUniformList.AllocBinding(Var FBinding:Integer;Decorates:PsrDecorateList);
var
 node:PsrUniform;
 pVar:PsrVariable;
begin
 if (Decorates=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) then
  begin
   if (node^.FBinding=-1) then //alloc
   begin
    Decorates^.emit_decorate(ntVar,pVar,Decoration.Binding,FBinding);
    Decorates^.emit_decorate(ntVar,pVar,Decoration.DescriptorSet,Decorates^.FDescriptorSet);
    node^.FBinding:=FBinding;
    Inc(FBinding);
   end;
  end;
  node:=Next(node);
 end;
end;

procedure TsrUniformList.AllocSourceExtension(FDebugInfo:PsrDebugInfoList);
var
 node:PsrUniform;
 pVar:PsrVariable;
begin
 if (FDebugInfo=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) then
  begin
   FDebugInfo^.emit_source_extension(node^.GetString);
  end;
  node:=Next(node);
 end;
end;

end.

