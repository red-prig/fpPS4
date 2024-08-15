unit srRefId;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 srNode;

type
 PsrRefId=^TsrRefId;
 TsrRefId=object
  ID:DWORD;
  function  Alloc:Boolean; inline;
 end;

 TsrRefNode=class(TsrNode)
  ID:TsrRefId;
  //
  function  _GetPrintName:RawByteString;     override;
  function  _GetRef:Pointer;                 override;
  //
  function  GetPrintName:RawByteString;
 end;

 ntRefId=TsrRefNode;

 PsrRefIdAlloc=^TsrRefIdAlloc;
 TsrRefIdAlloc=object
  FSpirvID:DWORD;
  function  FetchSpirvID:DWORD; inline;
  procedure FetchSpirvID(P:PsrRefId);
  function  GetSpirvIDBound:DWORD; inline;
 end;

implementation

function TsrRefNode._GetPrintName:RawByteString;
begin
 Result:=GetPrintName;
end;

function TsrRefNode._GetRef:Pointer;
begin
 Result:=@ID;
end;

function TsrRefNode.GetPrintName:RawByteString;
begin
 Assert(ID.ID<>0);
 Result:=IntToStr(ID.ID);
end;

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

