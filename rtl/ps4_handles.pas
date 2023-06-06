unit ps4_handles;

{$mode objfpc}{$H+}

interface

uses
 hamt,
 RWLock,
 Classes, SysUtils;

type
 TClassHandle=class
  private
   FRef:Pointer;
  public
   Procedure Acqure;
   Procedure Release;
 end;
 AClassHandle=Array of TClassHandle;

 TClassHandleLock=class(TClassHandle)
  private
   FLock:TRWLock;
  public
   constructor Create;
   destructor  Destroy; override;
   Procedure   Lock;
   Procedure   UnLock;
 end;

 TIntegerHandles=class
  private
   Const
    def_max_key=$7FFFFFFF;
   var
    FStub:TSTUB_HAMT32;
    FHAMT:THAMT;
    FCount,FPos:Integer;
    FLock:TRWLock;
  public
   min_key,max_key:Integer;
   constructor Create(min:Integer);
   destructor  Destroy; override;
   function    New(H:TClassHandle;var OutKey:Integer):Boolean;
   function    Acqure(const Key:Integer):TClassHandle;
   function    Delete(const Key:Integer):Boolean;
 end;

implementation

Procedure TClassHandle.Acqure;
begin
 System.InterlockedIncrement(FRef);
end;

Procedure TClassHandle.Release;
begin
 if System.InterlockedDecrement(FRef)=nil then
 begin
  Free;
 end;
end;

constructor TClassHandleLock.Create;
begin
 inherited;
 rwlock_init(FLock);
end;

destructor  TClassHandleLock.Destroy;
begin
 rwlock_destroy(FLock);
 inherited;
end;

Procedure TClassHandleLock.Lock;
begin
 rwlock_wrlock(FLock);
end;

Procedure TClassHandleLock.UnLock;
begin
 rwlock_unlock(FLock);
end;

constructor TIntegerHandles.Create(min:Integer);
begin
 min_key:=min;
 max_key:=def_max_key;
 FPos:=min;
 FHAMT:=@FStub;
 rwlock_init(FLock);
end;

procedure _free_data_cb(data,userdata:Pointer);
begin
 if (data<>nil) then
  TClassHandle(data).Release;
end;

destructor TIntegerHandles.Destroy;
begin
 HAMT_clear32(FHAMT,@_free_data_cb,nil);
 rwlock_destroy(FLock);
 inherited;
end;

function TIntegerHandles.New(H:TClassHandle;var OutKey:Integer):Boolean;
Var
 i,m:Integer;
 data:PPointer;
Label
 _data,_exit;
begin
 Result:=False;
 if (H=nil) then Exit;
 rwlock_wrlock(FLock);
 m:=(max_key-min_key);
 if (FCount>=m+1) then Goto _exit;
 if (FPos<min_key) or (FPos>max_key) then FPos:=min_key;
 if (FCount=0) then
 begin
  OutKey:=FPos;
  Inc(FPos);
  data:=HAMT_insert32(FHAMT,OutKey,Pointer(H));
  if (data=nil) then Goto _exit;
  if (data^<>Pointer(H)) then Goto _exit;
 end else
 begin
  For i:=0 to m do
  begin
   OutKey:=FPos;
   Inc(FPos);
   if (FPos>max_key) then FPos:=min_key;
   data:=HAMT_insert32(FHAMT,OutKey,Pointer(H));
   if (data=nil) then Goto _exit;
   if (data^=Pointer(H)) then Goto _data;
  end;
  Goto _exit;
 end;
 _data:
 Inc(FCount);
 H.Acqure;
 H.Acqure;
 Result:=True;
 _exit:
 rwlock_unlock(FLock);
end;

function TIntegerHandles.Acqure(const Key:Integer):TClassHandle;
Var
 data:PPointer;
Label
 _exit;
begin
 Result:=nil;
 if (Key<min_key) or (Key>max_key) then Exit;
 rwlock_rdlock(FLock);
 data:=HAMT_search32(FHAMT,Key);
 if (data=nil) then Goto _exit;
 Pointer(Result):=data^;
 if Assigned(Result) then
 begin
  Result.Acqure;
 end;
 _exit:
 rwlock_unlock(FLock);
end;

function TIntegerHandles.Delete(const Key:Integer):Boolean;
Var
 data:TClassHandle;
begin
 Result:=False;
 if (Key<min_key) or (Key>max_key) then Exit;
 rwlock_wrlock(FLock);
 data:=nil;
 HAMT_delete32(FHAMT,Key,@data);
 if Assigned(data) then
 begin
  data.Release;
  Dec(FCount);
  Result:=True;
 end;
 rwlock_unlock(FLock);
end;

end.

