unit vFramebufferManager;

{$mode ObjFPC}{$H+}

interface

uses
 SysUtils,
 mqueue,
 g23tree,
 vDependence,
 vImage;

type
 TvFramebufferImageless2=class(TvFramebufferImageless)
  //
 end;

 TvFramebufferBinded2=class(TvFramebufferBinded)
  FEntry:TAILQ_ENTRY;
  //
 end;

function FetchFramebufferImageless(cmd:TvDependenciesObject;P:PvFramebufferImagelessKey):TvFramebufferImageless2;
function FetchFramebufferBinded   (cmd:TvDependenciesObject;P:PvFramebufferBindedKey   ):TvFramebufferBinded2;

implementation

uses
 kern_rwlock;

type
 TvFramebufferImagelessKey2Compare=object
  function c(a,b:PvFramebufferImagelessKey):Integer; static;
 end;

 TvFramebufferImageless2Set=specialize T23treeSet<PvFramebufferImagelessKey,TvFramebufferImagelessKey2Compare>;

 TvFramebufferBinded2Set=object
  const
   MaxCount=256;
  var
   Queue:TAILQ_HEAD;
   Count:Ptruint;
 end;

var
 global_lock:Pointer=nil;

 FFramebufferImageless2Set:TvFramebufferImageless2Set;

 FFramebufferBinded:TvFramebufferBinded2Set=(
  Queue:(tqh_first:nil;tqh_last:@FFramebufferBinded.Queue.tqh_first);
  Count:0
 );

function TvFramebufferImagelessKey2Compare.c(a,b:PvFramebufferImagelessKey):Integer;
begin
 Result:=CompareByte(a^,b^,SizeOf(TvFramebufferImagelessKey));
end;

Procedure Global_Lock_wr;
begin
 rw_wlock(global_lock);
end;

Procedure Global_Unlock_wr;
begin
 rw_wunlock(global_lock);
end;

function _FindImageless(P:PvFramebufferImagelessKey):TvFramebufferImageless2;
var
 i:TvFramebufferImageless2Set.Iterator;
begin
 Result:=nil;
 i:=FFramebufferImageless2Set.find(P);
 if (i.Item<>nil) then
 begin
  Result:=TvFramebufferImageless2(ptruint(i.Item^)-ptruint(@TvFramebufferImageless2(nil).key));
 end;
end;

function _FetchImageless(P:PvFramebufferImagelessKey):TvFramebufferImageless2;
var
 t:TvFramebufferImageless2;
begin
 Result:=nil;

 t:=_FindImageless(P);

 if (t=nil) then
 begin

  t:=TvFramebufferImageless2.Create;
  t.key:=P^;

  if not t.Compile then
  begin
   FreeAndNil(t);
  end else
  begin
   t.Acquire(nil); //map ref
   FFramebufferImageless2Set.Insert(@t.key);
  end;
 end;

 Result:=t;
end;

function FetchFramebufferImageless(cmd:TvDependenciesObject;P:PvFramebufferImagelessKey):TvFramebufferImageless2;
begin
 Result:=nil;
 if (P=nil) then Exit;

 Global_Lock_wr;

 Result:=_FetchImageless(P);

 cmd.RefTo(Result);

 Global_Unlock_wr;
end;

///

function _FindBinded(P:PvFramebufferBindedKey):TvFramebufferBinded2;
var
 node:TvFramebufferBinded2;
begin
 Result:=nil;

 node:=TvFramebufferBinded2(TAILQ_FIRST(@FFramebufferBinded.Queue));

 while (node<>nil) do
 begin

  if (CompareByte(P^,node.Key,SizeOf(TvFramebufferBindedKey))=0) then
  begin
   if (node<>TvFramebufferBinded2(TAILQ_FIRST(@FFramebufferBinded.Queue))) then
   begin
    //set to top
    TAILQ_REMOVE     (@FFramebufferBinded.Queue,node,@node.FEntry);
    TAILQ_INSERT_HEAD(@FFramebufferBinded.Queue,node,@node.FEntry);
   end;
   Break;
  end;

  node:=TvFramebufferBinded2(TAILQ_NEXT(node,@node.FEntry));
 end;

end;

procedure _BoundBinded;
var
 node:TvFramebufferBinded2;
begin
 if (FFramebufferBinded.Count>FFramebufferBinded.MaxCount) then
 begin
  node:=TvFramebufferBinded2(TAILQ_LAST(@FFramebufferBinded.Queue));

  if (node<>nil) then
  begin
   TAILQ_REMOVE(@FFramebufferBinded.Queue,node,@node.FEntry);
   node.Release(nil);
  end;

  Dec(FFramebufferBinded.Count);
 end;
end;

function _FetchBinded(P:PvFramebufferBindedKey):TvFramebufferBinded2;
var
 t:TvFramebufferBinded2;
begin
 Result:=nil;

 t:=_FindBinded(P);

 if (t=nil) then
 begin

  t:=TvFramebufferBinded2.Create;
  t.key:=P^;

  if not t.Compile then
  begin
   FreeAndNil(t);
  end else
  begin
   t.Acquire(nil); //map ref
   TAILQ_INSERT_HEAD(@FFramebufferBinded.Queue,t,@t.FEntry);
   Inc(FFramebufferBinded.Count);

   _BoundBinded;
  end;
 end;

 Result:=t;
end;

function FetchFramebufferBinded(cmd:TvDependenciesObject;P:PvFramebufferBindedKey):TvFramebufferBinded2;
begin
 Result:=nil;
 if (P=nil) then Exit;

 Global_Lock_wr;

 Result:=_FetchBinded(P);

 cmd.RefTo(Result);

 Global_Unlock_wr;
end;


end.

