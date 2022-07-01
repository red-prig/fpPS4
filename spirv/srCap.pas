unit srCap;

{$mode ObjFPC}{$H+}

interface

uses
  srNodes;

type
 PSpirvCap=^TSpirvCap;
 TSpirvCap=object
  pLeft,pRight:PSpirvCap;
  //----
  ID:DWORD;
  function c(n1,n2:PSpirvCap):Integer; static;
 end;

 PsrCapList=^TsrCapList;
 TsrCapList=object
  type
   TNodeFetch=specialize TNodeFetch<PSpirvCap,TSpirvCap>;
  var
   Alloc:TfnAlloc;
   FNTree:TNodeFetch;
  procedure Add(ID:DWORD);
  Function  First:PSpirvCap;
  Function  Next(node:PSpirvCap):PSpirvCap;
 end;

implementation

function TSpirvCap.c(n1,n2:PSpirvCap):Integer;
begin
 Result:=Integer(n1^.ID>n2^.ID)-Integer(n1^.ID<n2^.ID);
end;

procedure TsrCapList.Add(ID:DWORD);
var
 fnode:TSpirvCap;
 pnode:PSpirvCap;
begin
 fnode:=Default(TSpirvCap);
 fnode.ID:=ID;
 if (FNTree.Find(@fnode)<>nil) then Exit;
 pnode:=Alloc(SizeOf(TSpirvCap));
 Move(fnode,pnode^,SizeOf(TSpirvCap));
 FNTree.Insert(pnode);
end;

Function TsrCapList.First:PSpirvCap;
begin
 Result:=FNTree.Min;
end;

Function TsrCapList.Next(node:PSpirvCap):PSpirvCap;
begin
 Result:=FNTree.Next(node);
end;

end.

