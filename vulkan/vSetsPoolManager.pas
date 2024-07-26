unit vSetsPoolManager;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 LFQueue,
 g23tree,
 Vulkan,
 vPipeline,
 vSetsPool,
 vDescriptorSet,
 vDependence;

Function FetchDescriptorGroup(cmd:TvDependenciesObject;Layout:TvPipelineLayout):TvDescriptorGroup;

implementation

uses
 kern_rwlock;

type
 TvSetsPoolUnbound=class;

 TvDescriptorGroupNode=class(TvDescriptorGroup)
  parent:TvSetsPoolUnbound;
  pNext:Pointer;
  procedure Release(Sender:TObject); override;
 end;

 TvSetsPool2Compare=object
  function c(a,b:TvSetsPool2):Integer; static;
 end;

 TvSetsPool2Set=specialize T23treeSet<TvSetsPool2,TvSetsPool2Compare>;

 PvPipelineLayout=^TvPipelineLayout;

 TvSetsPoolUnboundCompare=object
  function c(a,b:PvPipelineLayout):Integer; static;
 end;

 TvSetsPoolUnbound=class
  FLayout:TvPipelineLayout;
  FQueue:TIntrusiveMPSCQueue;
  FPools:TvSetsPool2Set;
  FLast:TvSetsPool2;
  Constructor Create(Layout:TvPipelineLayout);
  Procedure   NewPool;
  function    Alloc:TvDescriptorGroupNode;
  procedure   PushNode(N:TvDescriptorGroupNode);
  function    PopNode:TvDescriptorGroupNode;
 end;

 _TvSetsPoolUnbounds=specialize T23treeSet<PvPipelineLayout,TvSetsPoolUnboundCompare>;
 TvSetsPoolUnbounds=object(_TvSetsPoolUnbounds)
  lock:Pointer;
  Procedure Lock_wr;
  Procedure Unlock_wr;
 end;

var
 FSetsPoolUnbounds:TvSetsPoolUnbounds;

Procedure TvSetsPoolUnbounds.Lock_wr;
begin
 rw_wlock(lock);
end;

Procedure TvSetsPoolUnbounds.Unlock_wr;
begin
 rw_wunlock(lock);
end;

function TvSetsPool2Compare.c(a,b:TvSetsPool2):Integer;
begin
 Result:=Integer(Pointer(a)>Pointer(b))-Integer(Pointer(a)<Pointer(b));
end;

function TvSetsPoolUnboundCompare.c(a,b:PvPipelineLayout):Integer;
begin
 Result:=Integer(Pointer(a^)>Pointer(b^))-Integer(Pointer(a^)<Pointer(b^));
end;

Constructor TvSetsPoolUnbound.Create(Layout:TvPipelineLayout);
begin
 FLayout:=Layout;
 FQueue.Create;
end;

Procedure TvSetsPoolUnbound.NewPool;
var
 N:TvSetsPool2;
begin
 N:=TvSetsPool2.Create(FLayout,2);
 if N.Compile then
 begin
  FPools.Insert(N);
  FLast:=N;
 end else
 begin
  Assert(False,'NewPool');
  N.Free;
 end;
end;

function TvSetsPoolUnbound.Alloc:TvDescriptorGroupNode;
begin
 Result:=PopNode;
 if (Result<>nil) then Exit;

 if (FLast=nil) then NewPool;
 if FLast.IsFull then NewPool;

 Result:=TvDescriptorGroupNode.Create;
 Result.parent :=Self;
 //
 Result.FLayout:=FLayout;
 Result.FSets  :=FLast.Alloc;
end;

procedure TvSetsPoolUnbound.PushNode(N:TvDescriptorGroupNode);
begin
 FQueue.Push(@N.pNext);
end;

function TvSetsPoolUnbound.PopNode:TvDescriptorGroupNode;
var
 Node:PPointer;
begin
 Result:=nil;
 Node:=nil;
 if FQueue.Pop(Node) then
 begin
  Result:=TvDescriptorGroupNode(ptruint(Node)-ptruint(@TvDescriptorGroupNode(nil).pNext));
 end;
end;

Procedure TvDescriptorGroupNode.Release(Sender:TObject);
begin
 if System.InterlockedDecrement(Pointer(FRefs))=nil then
 if (parent<>nil) then
 begin
  parent.PushNode(Self);
 end;
end;

function _Find(Layout:TvPipelineLayout):TvSetsPoolUnbound;
var
 i:TvSetsPoolUnbounds.Iterator;
begin
 Result:=nil;
 i:=FSetsPoolUnbounds.find(@Layout);
 if (i.Item<>nil) then
 begin
  Result:=TvSetsPoolUnbound(ptruint(i.Item^)-ptruint(@TvSetsPoolUnbound(nil).FLayout));
 end;
end;

Function _Fetch(Layout:TvPipelineLayout):TvDescriptorGroupNode;
var
 t:TvSetsPoolUnbound;
 n:TvDescriptorGroupNode;

begin
 Result:=nil;

 t:=_Find(Layout);

 if (t=nil) then
 begin
  t:=TvSetsPoolUnbound.Create(Layout);
  FSetsPoolUnbounds.Insert(@t.FLayout);
 end;

 n:=t.Alloc;

 Result:=n;
end;

Function FetchDescriptorGroup(cmd:TvDependenciesObject;Layout:TvPipelineLayout):TvDescriptorGroup;
begin
 Result:=nil;
 if (Layout=nil) then Exit;
 if Layout.isSpace then Exit;

 FSetsPoolUnbounds.Lock_wr;

 Result:=_Fetch(Layout);

 cmd.RefTo(Result);

 FSetsPoolUnbounds.Unlock_wr;
end;


end.

