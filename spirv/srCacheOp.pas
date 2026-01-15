unit srCacheOp;

{$mode objfpc}{$H+}

interface

uses
 ginodes,
 srNode,
 srCFGParser,
 srOp;

type
 PsrCSEKey=^TsrCSEKey;
 TsrCSEKey=packed record
  pLine  :TSpirvOp;
  pParent:TSpirvOp;
 end;

 TsrCSENode=class
  public
   pLeft,pRight:TsrCSENode;
   class function c(n1,n2:PsrCSEKey):Integer; static;
  public
   key:TsrCSEKey;
  public
 end;

 PsrCacheOpList=^TsrCacheOpList;
 TsrCacheOpList=object
  type
   TCSETree=specialize TNodeTreeClass<TsrCSENode>;
  var
   FEmit:TCustomEmit;
   FCSETree:TCSETree;
  Procedure Init(Emit:TCustomEmit); inline;
  function  FindLocalCSE(node:TspirvOp):TsrCSENode;
  function  AddLocalCSE (node:TspirvOp):TsrCSENode;
 end;

function  _up_to_real(t:TsrOpBlock):TsrOpBlock;

implementation

function _up_to_real(t:TsrOpBlock):TsrOpBlock;
begin
 repeat
  if not t.IsType(ntOpBlock) then Break;
  if IsReal(t.bType) then Break;
  t:=t.Parent;
 until false;
 Result:=t;
end;

//--

function CompareParams(p1,p2:TSpirvOp):Integer;
var
 n1,n2:POpParamNode;
 r1,r2:TsrNode;
begin
 Result:=0;

 n1:=p1.ParamNode(0);
 n2:=p2.ParamNode(0);

 While (n1<>nil) do
 begin
  r1:=n1.Value;
  r2:=n2.Value;

  Result:=ord(ptruint(r1)>ptruint(r2))-ord(ptruint(r1)<ptruint(r2));
  if (Result<>0) then Exit;

  n1:=n1.Next;
  n2:=n2.Next;
 end;

end;

class function TsrCSENode.c(n1,n2:PsrCSEKey):Integer;
begin
 //pParent
 Result:=ord(ptruint(n1^.pParent)>ptruint(n2^.pParent))-ord(ptruint(n1^.pParent)<ptruint(n2^.pParent));
 if (Result<>0) then Exit;
 //OpId
 Result:=ord(n1^.pLine.OpId>n2^.pLine.OpId)-ord(n1^.pLine.OpId<n2^.pLine.OpId);
 if (Result<>0) then Exit;
 //pType
 Result:=ord(ptruint(n1^.pLine.pType)>ptruint(n2^.pLine.pType))-ord(ptruint(n1^.pLine.pType)<ptruint(n2^.pLine.pType));
 if (Result<>0) then Exit;

 Result:=CompareParams(n1^.pLine,n2^.pLine);
end;

Procedure TsrCacheOpList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrCacheOpList.FindLocalCSE(node:TspirvOp):TsrCSENode;
var
 key:TsrCSEKey;
begin
 Result:=nil;

 key.pLine  :=node;
 key.pParent:=_up_to_real(node.Parent);

 //search for dominance
 while (Result=nil) and (key.pParent<>nil) do
 begin
  Result:=FCSETree.Find(@key);
  key.pParent:=_up_to_real(key.pParent.Parent);
 end;

end;

function TsrCacheOpList.AddLocalCSE(node:TspirvOp):TsrCSENode;
var
 key:TsrCSEKey;
begin
 Result:=nil;

 key.pLine  :=node;
 key.pParent:=_up_to_real(node.Parent);

 Result:=FEmit.specialize New<TsrCSENode>;
 Result.key:=key;

 FCSETree.Insert(Result);
end;

end.

