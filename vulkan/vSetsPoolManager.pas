unit vSetsPoolManager;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 LFQueue,
 g23tree,
 Vulkan,
 vPipeline,
 vCmdBuffer;

Function FetchDescriptorGroup(cmd:TvCustomCmdBuffer;Pipeline:TvPipelineLayout):TvDescriptorGroup;

implementation

uses
 kern_rwlock;

type
 TvSetsPoolUnbound=class;

 TvDescriptorGroupNode=class(TvDescriptorGroup)
  parent:TvSetsPoolUnbound;
  pNext:Pointer;
  Procedure Release(Sender:TObject);
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
  FPipeline:TvPipelineLayout;
  FQueue:TIntrusiveMPSCQueue;
  FPools:TvSetsPool2Set;
  FLast:TvSetsPool2;
  Constructor Create(Pipeline:TvPipelineLayout);
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

Constructor TvSetsPoolUnbound.Create(Pipeline:TvPipelineLayout);
begin
 FPipeline:=Pipeline;
 FQueue.Create;
end;

Procedure TvSetsPoolUnbound.NewPool;
var
 N:TvSetsPool2;
begin
 N:=TvSetsPool2.Create(FPipeline,2);
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
 Result.parent:=Self;
 Result.FSets:=FLast.Alloc;
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
 if (parent<>nil) then
 begin
  parent.PushNode(Self);
 end;
end;

function _Find(Pipeline:TvPipelineLayout):TvSetsPoolUnbound;
var
 i:TvSetsPoolUnbounds.Iterator;
begin
 Result:=nil;
 i:=FSetsPoolUnbounds.find(@Pipeline);
 if (i.Item<>nil) then
 begin
  Result:=TvSetsPoolUnbound(ptruint(i.Item^)-ptruint(@TvSetsPoolUnbound(nil).FPipeline));
 end;
end;

Function _Fetch(Pipeline:TvPipelineLayout):TvDescriptorGroupNode;
var
 t:TvSetsPoolUnbound;
 n:TvDescriptorGroupNode;

begin
 Result:=nil;

 t:=_Find(Pipeline);

 if (t=nil) then
 begin
  t:=TvSetsPoolUnbound.Create(Pipeline);
  FSetsPoolUnbounds.Insert(@t.FPipeline);
 end;

 n:=t.Alloc;

 Result:=n;
end;

Function FetchDescriptorGroup(cmd:TvCustomCmdBuffer;Pipeline:TvPipelineLayout):TvDescriptorGroup;
begin
 Result:=nil;
 if (Pipeline=nil) then Exit;
 if Pipeline.isSpace then Exit;

 FSetsPoolUnbounds.Lock_wr;

 Result:=_Fetch(Pipeline);

 if (cmd<>nil) and (Result<>nil) then
 begin
  cmd.AddDependence(@TvDescriptorGroupNode(Result).Release);
 end;

 FSetsPoolUnbounds.Unlock_wr;
end;


end.

