unit srRefId;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 srNode;

type
 ntRefId=class(TsrNodeVmt)
  class function  GetPrintName(node:PsrNode):RawByteString;     override;
  class function  GetRef(node:PsrNode):Pointer;                 override;
 end;

 PsrRefId=^TsrRefId;
 TsrRefId=object
  ID:DWORD;
  function  Alloc:Boolean; inline;
 end;

 PsrRefNode=^TsrRefNode;
 TsrRefNode=object(TsrNode)
  ID:TsrRefId;
  Procedure Init; inline;
  function  GetPrintName:RawByteString;
 end;

 PsrRefIdAlloc=^TsrRefIdAlloc;
 TsrRefIdAlloc=object
  FSpirvID:DWORD;
  function  FetchSpirvID:DWORD; inline;
  procedure FetchSpirvID(P:PsrRefId);
  function  GetSpirvIDBound:DWORD; inline;
 end;

implementation

class function ntRefId.GetPrintName(node:PsrNode):RawByteString;
begin
 Result:=PsrRefNode(node)^.GetPrintName;
end;

class function ntRefId.GetRef(node:PsrNode):Pointer;
begin
 Result:=@PsrRefNode(node)^.ID;
end;

Procedure TsrRefNode.Init; inline;
begin
 fntype:=ntRefId;
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

