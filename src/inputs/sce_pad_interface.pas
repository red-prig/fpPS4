unit sce_pad_interface;

{$mode ObjFPC}{$H+}

interface

uses
 sce_pad_types,
 ps4_handles;

type
 TScePadHandle=class(TClassHandle)
  var
   handle:Integer;
   index:Integer;
  function   ReadState(data:PScePadData):Integer; virtual;
  destructor Destroy; override;
 end;

 TScePadInterface=class
  class function  Load:Boolean; virtual;
  class procedure Unload;       virtual;
  class function  Init:Integer; virtual;
  class function  Done:Integer; virtual;
  class function  Open(index:Integer;var handle:TScePadHandle):Integer; virtual;
 end;

 TAbstractScePadInterface=class of TScePadInterface;

var
 pad_handles:TIntegerHandles;
 pad_opened :array[0..15] of TScePadHandle;

implementation

function TScePadHandle.ReadState(data:PScePadData):Integer;
begin
 Result:=SCE_PAD_ERROR_INVALID_HANDLE;
end;

destructor TScePadHandle.Destroy;
begin
 if (index>=0) and (index<16) then
 begin
  pad_opened[index]:=nil;
 end;
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

class function TScePadInterface.Open(index:Integer;var handle:TScePadHandle):Integer;
begin
 handle:=nil;
 Result:=SCE_PAD_ERROR_NOT_INITIALIZED;
end;

initialization
 pad_handles:=TIntegerHandles.Create(0);
 pad_handles.max_key:=16;

end.


