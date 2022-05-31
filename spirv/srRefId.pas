unit srRefId;

{$mode objfpc}{$H+}

interface

type
 PsrRefId=^TsrRefId;
 TsrRefId=packed object
  ID:DWORD;
  function Alloc:Boolean; inline;
 end;

 PsrRefIdAlloc=^TsrRefIdAlloc;
 TsrRefIdAlloc=object
  FSpirvID:DWORD;
  function  FetchSpirvID:DWORD; inline;
  procedure FetchSpirvID(P:PsrRefId);
  function  GetSpirvIDBound:DWORD; inline;
 end;

implementation

function TsrRefId.Alloc:Boolean; inline;
begin
 Result:=(ID<>0);
end;

function TsrRefIdAlloc.FetchSpirvID:DWORD; inline;
begin
 Inc(FSpirvID);
 Result:=FSpirvID;
end;

procedure TsrRefIdAlloc.FetchSpirvID(P:PsrRefId);
begin
 if (P<>nil) and (not P^.Alloc) then
 begin
  P^.ID:=FetchSpirvID;
 end;
end;

function TsrRefIdAlloc.GetSpirvIDBound:DWORD; inline;
begin
 Result:=FSpirvID+1;
end;

end.

