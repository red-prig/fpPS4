unit srVertLayout;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils,
  spirv,
  srNodes,
  srReg,
  srOp,
  srVariable,
  srLayout,
  srDecorate;

type
 PsrVertLayout=^TsrVertLayout;
 TsrVertLayout=object(TsrDescriptor)
  pLeft,pRight:PsrVertLayout;
  //----
  pLayout:PsrDataLayout;
  pReg:PsrRegNode;
  function c(n1,n2:PsrVertLayout):Integer; static;
  function GetString:RawByteString;
  function GetName:RawByteString;
 end;

 TsrVertLayoutList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrVertLayout,TsrVertLayout>;
  var
   Alloc:TfnAlloc;
   FNTree:TNodeFetch;
  procedure Init(cb:TfnAlloc);
  function  Fetch(p:PsrDataLayout):PsrVertLayout;
  Function  First:PsrVertLayout;
  Function  Next(node:PsrVertLayout):PsrVertLayout;
  procedure AllocBinding(Decorates:PsrDecorateList);
  procedure AllocEntryPoint(EntryPoint:PSpirvOp);
  procedure AllocSourceExtension(FDebugInfo:PsrDebugInfoList);
 end;

implementation

function TsrVertLayout.c(n1,n2:PsrVertLayout):Integer;
begin
 Result:=Integer(n1^.pLayout>n2^.pLayout)-Integer(n1^.pLayout<n2^.pLayout);
 //Result:=CompareByte(n1^.key,n2^.key,SizeOf(TsrVertLayout.key));
end;

function TsrVertLayout.GetString:RawByteString;
var
 PID:DWORD;
begin
 PID:=0;
 if (pLayout<>nil) then
 begin
  PID:=pLayout^.FID;
 end;
 Result:='VA;PID='+HexStr(PID,8)+
           ';BND='+HexStr(FBinding,8);
end;

function TsrVertLayout.GetName:RawByteString;
begin
 Result:='atParam'+IntToStr(FBinding);
end;

procedure TsrVertLayoutList.Init(cb:TfnAlloc);
begin
 Alloc:=cb;
end;

function TsrVertLayoutList.Fetch(p:PsrDataLayout):PsrVertLayout;
var
 node:TsrVertLayout;
begin
 node:=Default(TsrVertLayout);
 node.pLayout:=p;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=Alloc(SizeOf(TsrVertLayout));
  Result^.pLayout:=p;
  Result^.FStorage:=StorageClass.Input;
  Result^.FBinding:=-1;
  FNTree.Insert(Result);
 end;
end;

Function TsrVertLayoutList.First:PsrVertLayout;
begin
 Result:=FNTree.Min;
end;

Function TsrVertLayoutList.Next(node:PsrVertLayout):PsrVertLayout;
begin
 Result:=FNTree.Next(node);
end;

procedure TsrVertLayoutList.AllocBinding(Decorates:PsrDecorateList);
var
 node:PsrVertLayout;
 pVar:PsrVariable;
 FBinding:Integer;
begin
 if (Decorates=nil) then Exit;
 FBinding:=0;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) then
  begin
   if (node^.FBinding=-1) then //alloc
   begin
    Decorates^.emit_decorate(ntVar,pVar,Decoration.Location,FBinding);
    node^.FBinding:=FBinding;
    Inc(FBinding);
   end;
  end;
  node:=Next(node);
 end;
end;

procedure TsrVertLayoutList.AllocEntryPoint(EntryPoint:PSpirvOp);
var
 node:PsrVertLayout;
 pVar:PsrVariable;
begin
 if (EntryPoint=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) then
  begin
   EntryPoint^.AddParam(ntVar,pVar);
  end;
  node:=Next(node);
 end;
end;

procedure TsrVertLayoutList.AllocSourceExtension(FDebugInfo:PsrDebugInfoList);
var
 node:PsrVertLayout;
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

