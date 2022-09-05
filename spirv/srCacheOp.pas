unit srCacheOp;

{$mode objfpc}{$H+}

interface

uses
 ginodes,
 srNode,
 srCFGLabel,
 srType,
 srReg,
 srOp;

type
 PsrCacheOp=^TsrCacheOp;
 TsrCacheOp=object
  private
   pLeft,pRight:PsrCacheOp;
   //----
   key:packed record
    place:PsrOpBlock;
    OpId:DWORD;
    dtype:TsrDataType;
    count:DWORD;
   end;
   pData:PPsrRegNode;
   function  c(n1,n2:PsrCacheOp):Integer; static;
  public
   pDst:PsrNode;
 end;

 PsrCacheOpList=^TsrCacheOpList;
 TsrCacheOpList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrCacheOp,TsrCacheOp>;
  var
   FEmit:TCustomEmit;
   FNTree:TNodeFetch;
  Procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(place:PsrOpBlock;OpId:DWORD;rtype:TsrDataType;count:Byte;src:PPsrRegNode):PsrCacheOp;
 end;

function  _up_to_real(t:PsrOpBlock):PsrOpBlock;

implementation

function _up_to_real(t:PsrOpBlock):PsrOpBlock;
begin
 repeat
  if not t^.IsType(ntOpBlock) then Break;
  Case t^.Block.bType of
   btMain,
   btCond,
   btLoop:Break;
   else;
  end;
  t:=t^.Parent;
 until false;
 Result:=t;
end;

//--

function TsrCacheOp.c(n1,n2:PsrCacheOp):Integer;
var
 count:DWORD;
begin
 Result:=CompareByte(n1^.key,n2^.key,SizeOf(TsrCacheOp.key));
 if (Result=0) then
 begin
  count:=n1^.key.count;
  if (count<>0) then
  begin
   Result:=CompareByte(n1^.pData^,n2^.pData^,count*SizeOf(Pointer));
  end;
 end;
end;

Procedure TsrCacheOpList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrCacheOpList.Fetch(place:PsrOpBlock;OpId:DWORD;rtype:TsrDataType;count:Byte;src:PPsrRegNode):PsrCacheOp;
var
 size:ptruint;
 node:TsrCacheOp;
begin
 Assert(place<>nil);
 place:=_up_to_real(place);
 Result:=nil;
 node:=Default(TsrCacheOp);
 node.key.place:=place;
 node.key.OpId :=OpId;
 node.key.dtype:=rtype;
 node.key.count:=count;
 node.pData:=src;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrCacheOp));
  Move(node,Result^,SizeOf(TsrCacheOp));

  size:=count*SizeOf(Pointer);
  Result^.pData:=FEmit.Alloc(size);
  Move(src^,Result^.pData^,size);

  FNTree.Insert(Result);
 end;
end;

end.

