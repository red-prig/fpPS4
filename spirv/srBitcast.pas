unit srBitcast;

{$mode ObjFPC}{$H+}

interface

uses
 ginodes,
 srNode,
 srType,
 srConst,
 srReg;

type
 PsrBitcast=^TsrBitcast;
 TsrBitcast=object
  pLeft,pRight:PsrBitcast;
  //----
  key:packed record
   dtype:TsrDataType;
   src:PsrRegNode;
  end;
  dst:PsrRegNode;
  function  c(n1,n2:PsrBitcast):Integer; static;
 end;

 PsrBitcastList=^TsrBitcastList;
 TsrBitcastList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrBitcast,TsrBitcast>;
  var
   FNTree:TNodeFetch;
   rSlot:TsrRegSlot;
  procedure Init(Emit:TCustomEmit); inline;
  function  Find(dtype:TsrDataType;src:PsrRegNode):PsrBitcast;
  function  FetchRead(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
  function  FetchDstr(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
  function  FetchCast(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
 end;

implementation

function TsrBitcast.c(n1,n2:PsrBitcast):Integer;
begin
 //first dtype
 Result:=Integer(n1^.key.dtype>n2^.key.dtype)-Integer(n1^.key.dtype<n2^.key.dtype);
 if (Result<>0) then Exit;
 //second src
 Result:=Integer(n1^.key.src>n2^.key.src)-Integer(n1^.key.src<n2^.key.src);
end;

procedure TsrBitcastList.Init(Emit:TCustomEmit); inline;
begin
 rSlot.Init(Emit,'BCAST');
end;

function TsrBitcastList.Find(dtype:TsrDataType;src:PsrRegNode):PsrBitcast;
var
 node:TsrBitcast;
begin
 node:=Default(TsrBitcast);
 node.key.dtype:=dtype;
 node.key.src:=src;
 Result:=FNTree.Find(@node);
end;

function TsrBitcastList.FetchRead(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
var
 dst:PsrRegNode;
begin
 Result:=src;
 if (src=nil) then Exit;
 if (dtype=dtUnknow) or (dtype=src^.dtype) then Exit;

 dst:=rSlot.New(src^.pLine,dtype);
 dst^.pWriter:=src;

 Result:=dst;
end;

function TsrBitcastList.FetchDstr(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
var
 dst:PsrRegNode;
begin
 Result:=src;
 if (src=nil) then Exit;
 if (dtype=dtUnknow) or (dtype=src^.dtype) then Exit;

 dst:=rSlot.New(src^.pLine,dtype);
 dst^.pWriter:=src^.pWriter;

 src^.pWriter:=dst;

 Result:=dst;
end;

function TsrBitcastList.FetchCast(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
var
 pConstList:PsrConstList;
 node:PsrBitcast;
 dst:PsrRegNode;
 pConst:PsrConst;

begin
 Result:=src;
 if (src=nil) then Exit;
 if (dtype=dtUnknow) or (dtype=src^.dtype) then Exit;

 dst:=nil;

 node:=Find(dtype,src);
 if (node<>nil) then
 begin
  Result:=node^.dst;
  Exit;
 end;

 if src^.is_const then
 begin
  pConst:=src^.AsConst;

  pConstList:=rSlot.FEmit.GetConstList;
  pConst:=pConstList^.Bitcast(dtype,pConst);

  dst:=rSlot.New(src^.pLine,dtype);
  dst^.pWriter:=pConst;
 end else
 begin
  if TryBitcastType(src^.dtype,dtype) then
  begin
   dst:=rSlot.New(src^.pLine,dtype);

   rSlot.FEmit.OpCast(src^.pLine,dst,src)
  end else
  begin
   Assert(false,'bitcast');
  end;
 end;

 node:=rSlot.FEmit.Alloc(SizeOf(TsrBitcast));
 node^:=Default(TsrBitcast);
 node^.key.dtype:=dtype;
 node^.key.src:=src;
 node^.dst:=dst;
 FNTree.Insert(node);

 Result:=dst;
end;

end.

