unit srVertLayout;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 spirv,
 ginodes,
 srNode,
 srType,
 srReg,
 srOp,
 srVariable,
 srLayout,
 srDecorate;

type
 ntVertLayout=class(ntDescriptor)
  class function  GetStorageName(node:PsrNode):RawByteString; override;
 end;

 PsrVertLayout=^TsrVertLayout;
 TsrVertLayout=object(TsrDescriptor)
  public
   pLeft,pRight:PsrVertLayout;
   function  c(n1,n2:PsrVertLayout):Integer; static;
  private
   pLayout:PsrDataLayout;
  public
   pReg:PsrRegNode;
   procedure Init(p:PsrDataLayout); inline;
   function  GetString:RawByteString;
   function  GetStorageName:RawByteString;
 end;

 PsrVertLayoutList=^TsrVertLayoutList;
 TsrVertLayoutList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrVertLayout,TsrVertLayout>;
  var
   FEmit:TCustomEmit;
   FNTree:TNodeFetch;
  procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(p:PsrDataLayout;rtype:TsrDataType):PsrVertLayout;
  Function  First:PsrVertLayout;
  Function  Next(node:PsrVertLayout):PsrVertLayout;
  procedure AllocBinding;
  procedure AllocEntryPoint(EntryPoint:PSpirvOp);
  procedure AllocSourceExtension;
 end;

implementation

class function ntVertLayout.GetStorageName(node:PsrNode):RawByteString;
begin
 Result:=PsrVertLayout(node)^.GetStorageName;
end;

//

function TsrVertLayout.c(n1,n2:PsrVertLayout):Integer;
begin
 Result:=Integer(n1^.pLayout>n2^.pLayout)-Integer(n1^.pLayout<n2^.pLayout);
end;

procedure TsrVertLayout.Init(p:PsrDataLayout); inline;
begin
 fntype  :=ntVertLayout;
 FStorage:=StorageClass.Input;
 FBinding:=-1;
 pLayout :=p;
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

function TsrVertLayout.GetStorageName:RawByteString;
begin
 Result:='atParam'+IntToStr(FBinding);
end;

procedure TsrVertLayoutList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrVertLayoutList.Fetch(p:PsrDataLayout;rtype:TsrDataType):PsrVertLayout;
var
 node:TsrVertLayout;
begin
 node:=Default(TsrVertLayout);
 node.Init(p);
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrVertLayout));
  Move(node,Result^,SizeOf(TsrVertLayout));
  //
  Result^.InitType(rtype,FEmit);
  Result^.InitVar(FEmit);
  //
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

procedure TsrVertLayoutList.AllocBinding;
var
 pDecorateList:PsrDecorateList;
 node:PsrVertLayout;
 pVar:PsrVariable;
 FBinding:Integer;
begin
 pDecorateList:=FEmit.GetDecorateList;
 FBinding:=0;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) and node^.IsUsed and (node^.FBinding=-1) then
  begin
   pDecorateList^.OpDecorate(pVar,Decoration.Location,FBinding);
   node^.FBinding:=FBinding;
   Inc(FBinding);
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
  if (pVar<>nil) and node^.IsUsed then
  begin
   EntryPoint^.AddParam(pVar);
  end;
  node:=Next(node);
 end;
end;

procedure TsrVertLayoutList.AllocSourceExtension;
var
 FDebugInfo:PsrDebugInfoList;
 node:PsrVertLayout;
 pVar:PsrVariable;
begin
 FDebugInfo:=FEmit.GetDebugInfoList;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) and node^.IsUsed then
  begin
   FDebugInfo^.OpSource(node^.GetString);
  end;
  node:=Next(node);
 end;
end;

end.

