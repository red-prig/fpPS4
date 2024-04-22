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
  FDep_lock    :Pointer;
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
  function  Insert  (Const K:TObject):Boolean;
  Function  Contains(Const K:TObject):Boolean;
  Function  delete  (Const R:TObject):Boolean;
  Function  Release (Const R:TObject):Boolean;
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

 rw_wlock(FDep_lock);

 Result:=FDependencies.Insert(cb);

 rw_wunlock(FDep_lock);
end;

function TvDependenciesObject.DelDependence(cb:TvReleaseCb):Boolean;
begin
 Result:=False;
 if (cb=nil) then Exit;

 rw_wlock(FDep_lock);

 Result:=FDependencies.delete(cb);

 rw_wunlock(FDep_lock);
end;

Procedure TvDependenciesObject.ReleaseAllDependencies(Sender:TObject);
var
 It:TvRelease.Iterator;
 cb:TvReleaseCb;
begin
 rw_wlock(FDep_lock);

 while (FDependencies.size<>0) do
 begin
  It:=FDependencies.cbegin;
  if (It.Item=nil) then Break;
  cb:=It.Item^;
  FDependencies.erase(It);

  if (cb<>nil) then
  begin
   rw_wunlock(FDep_lock);

   cb(Sender);

   rw_wlock(FDep_lock);
  end;
 end;

 rw_wunlock(FDep_lock);
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

