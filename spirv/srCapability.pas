unit srCapability;

{$mode ObjFPC}{$H+}

interface

uses
 ginodes,
 srNode;

type
 TsrCapability=class
  pLeft,pRight:TsrCapability;
  //----
  KEY:DWORD; //ID
  class function c(n1,n2:PDWORD):Integer; static;
  property ID:DWORD read KEY;
 end;

 PsrCapabilityList=^TsrCapabilityList;
 TsrCapabilityList=object
  type
   TNodeTree=specialize TNodeTreeClass<TsrCapability>;
  var
   FEmit:TCustomEmit;
   FTree:TNodeTree;
  Procedure Init(Emit:TCustomEmit);
  procedure Add(ID:DWORD);
  Function  First:TsrCapability;
  Function  Next(node:TsrCapability):TsrCapability;
 end;

implementation

class function TsrCapability.c(n1,n2:PDWORD):Integer;
begin
 Result:=ord(n1^>n2^)-ord(n1^<n2^);
end;

Procedure TsrCapabilityList.Init(Emit:TCustomEmit);
begin
 FEmit:=Emit;
end;

procedure TsrCapabilityList.Add(ID:DWORD);
var
 node:TsrCapability;
begin
 if (FTree.Find(@ID)<>nil) then Exit;
 node:=FEmit.specialize New<TsrCapability>;
 node.KEY:=ID;
 FTree.Insert(node);
end;

Function TsrCapabilityList.First:TsrCapability;
begin
 Result:=FTree.Min;
end;

Function TsrCapabilityList.Next(node:TsrCapability):TsrCapability;
begin
 Result:=FTree.Next(node);
end;

end.

