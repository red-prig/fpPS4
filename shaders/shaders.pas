unit shaders;

{$mode ObjFPC}{$H+}

interface

uses
  LResources,
  Classes,
  SysUtils;

function GetResourceStream(const ResName:string;ResType:PChar=nil):TStream;

implementation

function GetResourceStream(const ResName:string;ResType:PChar=nil):TStream;
begin
 Result:=TLazarusResourceStream.Create(ResName,ResType);
end;

initialization
  {$I shaders.lrs}

end.

