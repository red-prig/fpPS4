unit sce_pad_interface;

{$mode ObjFPC}{$H+}

interface

uses
 sce_pad_types,
 ps4_handles,
 spinlock;

type
 TScePadHandle=class(TClassHandle)
  var
   userID:Integer;
   _type :Integer;
   index :Integer;
   handle:Integer;
  function   ReadState(data:PScePadData):Integer;            virtual;
  function   SetLightBar(data:pScePadLightBarParam):Integer; virtual;
  function   ResetLightBar():Integer;                        virtual;
  destructor Destroy; override;
 end;

 TScePadInterface=class
  class function  Load:Boolean; virtual;
  class procedure Unload;       virtual;
  class function  Init:Integer; virtual;
  class function  Done:Integer; virtual;
  class function  Open(var handle:TScePadHandle):Integer; virtual;
 end;

 TAbstractScePadInterface=class of TScePadInterface;

var
 pad_handles:TIntegerHandles;
 pad_opened :array[0..15] of TScePadHandle;
 pad_lock   :Pointer;

function  FindPadByParam(userID,_type,index:Integer):TScePadHandle;
Procedure SavePadHandle(handle:TScePadHandle);

implementation

function FindPadByParam(userID,_type,index:Integer):TScePadHandle;
var
 i:Integer;
begin
 Result:=nil;
 spin_lock(pad_lock);
 For i:=Low(pad_opened) to High(pad_opened) do
  if (pad_opened[i]<>nil) then
  if (pad_opened[i].userID=userID) and
     (pad_opened[i]._type =_type ) and
     (pad_opened[i].index =index ) then
  begin
   Result:=pad_opened[i];
   Result.Acqure;
   spin_unlock(pad_lock);
   Exit;
  end;
 spin_unlock(pad_lock);
end;

Procedure SavePadHandle(handle:TScePadHandle);
var
 i:Integer;
begin
 spin_lock(pad_lock);
 For i:=Low(pad_opened) to High(pad_opened) do
  if (pad_opened[i]=nil) then
  begin
   pad_opened[i]:=handle;
   spin_unlock(pad_lock);
   Exit;
  end;
 spin_unlock(pad_lock);
end;


function TScePadHandle.ReadState(data:PScePadData):Integer;
begin
 Result:=SCE_PAD_ERROR_INVALID_HANDLE;
end;

function TScePadHandle.SetLightBar(data:pScePadLightBarParam):Integer;
begin
 Result:=0;
end;

function TScePadHandle.ResetLightBar():Integer;
begin
 Result:=0;
end;

destructor TScePadHandle.Destroy;
var
 i:Integer;
begin
 For i:=Low(pad_opened) to High(pad_opened) do
  if (pad_opened[i]=Self) then
  begin
   pad_opened[i]:=nil;
   Break;
  end;
 //
 inherited;
end;

class function TScePadInterface.Load:Boolean;
begin
 Result:=True;
end;

class procedure TScePadInterface.Unload;
begin
 //
end;


class function TScePadInterface.Init:Integer;
begin
 Result:=0;
end;

class function TScePadInterface.Done:Integer;
begin
 Result:=0;
end;

class function TScePadInterface.Open(var handle:TScePadHandle):Integer;
begin
 handle:=nil;
 Result:=SCE_PAD_ERROR_NOT_INITIALIZED;
end;

initialization
 pad_handles:=TIntegerHandles.Create(1);
 pad_handles.max_key:=16;

end.


