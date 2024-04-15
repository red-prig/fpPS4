unit vDependence;

{$mode ObjFPC}{$H+}

interface

uses
 g23tree;

type
 TvReleaseCb=procedure(Sender:TObject) of object;

 TvReleaseCompare=object
  function c(a,b:TvReleaseCb):Integer; static;
 end;

 TvRelease=specialize T23treeSet<TvReleaseCb,TvReleaseCompare>;

 TvDependenciesObject=class
  FDependencies:TvRelease;
  //
  function   AddDependence(cb:TvReleaseCb):Boolean;
  function   DelDependence(cb:TvReleaseCb):Boolean;
  Procedure  ReleaseAllDependencies(Sender:TObject);
  Destructor Destroy; override;
 end;

 TObjectCompare=object
  function c(a,b:TObject):Integer; static;
 end;

 TObjectSet=specialize T23treeSet<TObject,TObjectCompare>;

 TObjectSetLock=object(TObjectSet)
  lock:Pointer;
  Procedure Lock_rd;
  Procedure Lock_wr;
  Procedure Unlock_rd;
  Procedure Unlock_wr;
  function  Insert(Const K:TObject):Boolean;
  Function  Contains(Const K:TObject):Boolean;
  Function  delete(Const R:TObject):Boolean;
  Function  Release(Const R:TObject):Boolean;
 end;

implementation

uses
 kern_rwlock;

function TvReleaseCompare.c(a,b:TvReleaseCb):Integer;
begin
 Result:=Integer(TMethod(a).Code>TMethod(b).Code)-Integer(TMethod(a).Code<TMethod(b).Code);
 if (Result<>0) then Exit;
 Result:=Integer(TMethod(a).Data>TMethod(b).Data)-Integer(TMethod(a).Data<TMethod(b).Data);
end;

//


function TvDependenciesObject.AddDependence(cb:TvReleaseCb):Boolean;
begin
 Result:=False;
 if (cb=nil) then Exit;
 Result:=FDependencies.Insert(cb);
end;

function TvDependenciesObject.DelDependence(cb:TvReleaseCb):Boolean;
begin
 Result:=False;
 if (cb=nil) then Exit;
 Result:=FDependencies.delete(cb);
end;

Procedure TvDependenciesObject.ReleaseAllDependencies(Sender:TObject);
var
 It:TvRelease.Iterator;
begin
 while (FDependencies.size<>0) do
 begin
  It:=FDependencies.cbegin;
  if (It.Item=nil) then Break;
  FDependencies.erase(It);
  TvReleaseCb(It.Item^)(Sender);
 end;
end;

Destructor TvDependenciesObject.Destroy;
begin
 FDependencies.Free;
 inherited;
end;

//

function TObjectCompare.c(a,b:TObject):Integer;
begin
 Result:=Integer(Pointer(a)>Pointer(b))-Integer(Pointer(a)<Pointer(b));
end;

Procedure TObjectSetLock.Lock_rd;
begin
 rw_rlock(lock);
end;

Procedure TObjectSetLock.Lock_wr;
begin
 rw_wlock(lock);
end;

Procedure TObjectSetLock.Unlock_rd;
begin
 rw_runlock(lock);
end;

Procedure TObjectSetLock.Unlock_wr;
begin
 rw_wunlock(lock);
end;

function TObjectSetLock.Insert(Const K:TObject):Boolean;
begin
 Lock_wr;
 Result:=inherited;
 Unlock_wr;
end;

Function TObjectSetLock.Contains(Const K:TObject):Boolean;
begin
 Lock_rd;
 Result:=inherited;
 Unlock_rd;
end;

Function TObjectSetLock.delete(Const R:TObject):Boolean;
begin
 Lock_wr;
 Result:=inherited;
 Unlock_wr;
end;

Function TObjectSetLock.Release(Const R:TObject):Boolean;
begin
 Lock_wr;
 inherited;
 Result:=(Size=0);
 Unlock_wr;
end;

end.

