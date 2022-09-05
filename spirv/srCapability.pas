unit srCapability;

{$mode ObjFPC}{$H+}

interface

uses
 ginodes,
 srNode;

type
 PsrCapability=^TsrCapability;
 TsrCapability=object
  pLeft,pRight:PsrCapability;
  //----
  ID:DWORD;
  function c(n1,n2:PsrCapability):Integer; static;
 end;

 PsrCapabilityList=^TsrCapabilityList;
 TsrCapabilityList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrCapability,TsrCapability>;
  var
   FEmit:TCustomEmit;
   FNTree:TNodeFetch;
  Procedure Init(Emit:TCustomEmit);
  procedure Add(ID:DWORD);
  Function  First:PsrCapability;
  Function  Next(node:PsrCapability):PsrCapability;
 end;

implementation

function TsrCapability.c(n1,n2:PsrCapability):Integer;
begin
 Result:=Integer(n1^.ID>n2^.ID)-Integer(n1^.ID<n2^.ID);
end;

Procedure TsrCapabilityList.Init(Emit:TCustomEmit);
begin
 FEmit:=Emit;
end;

procedure TsrCapabilityList.Add(ID:DWORD);
var
 fnode:TsrCapability;
 pnode:PsrCapability;
begin
 fnode:=Default(TsrCapability);
 fnode.ID:=ID;
 if (FNTree.Find(@fnode)<>nil) then Exit;
 pnode:=FEmit.Alloc(SizeOf(TsrCapability));
 Move(fnode,pnode^,SizeOf(TsrCapability));
 FNTree.Insert(pnode);
end;

Function TsrCapabilityList.First:PsrCapability;
begin
 Result:=FNTree.Min;
end;

Function TsrCapabilityList.Next(node:PsrCapability):PsrCapability;
begin
 Result:=FNTree.Next(node);
end;

end.

