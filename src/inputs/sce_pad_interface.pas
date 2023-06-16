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
  function ReadState(data:PScePadData):Integer; virtual;
 end;

 TScePadInterface=class
  class function Init:Integer; virtual;
  class function Done:Integer; virtual;
  class function Open(index:Integer;var handle:TScePadHandle):Integer; virtual;
  class function GetHandle(index:Integer):Integer; virtual;
 end;

 TAbstractScePadInterface=class of TScePadInterface;

var
 pad_handles:TIntegerHandles;

implementation

function TScePadHandle.ReadState(data:PScePadData):Integer;
begin
 Result:=SCE_PAD_ERROR_INVALID_HANDLE;
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

class function TScePadInterface.GetHandle(index:Integer):Integer;
begin
 Result:=SCE_PAD_ERROR_NO_HANDLE;
end;

initialization
 pad_handles:=TIntegerHandles.Create(0);
 pad_handles.max_key:=16;

end.


