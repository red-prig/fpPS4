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
  function  _Find(dtype:TsrDataType;src:PsrRegNode):PsrBitcast;
  function  Find(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
  Procedure Save(dtype:TsrDataType;src,dst:PsrRegNode);
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

function TsrBitcastList._Find(dtype:TsrDataType;src:PsrRegNode):PsrBitcast;
var
 node:TsrBitcast;
begin
 node:=Default(TsrBitcast);
 node.key.dtype:=dtype;
 node.key.src:=src;
 Result:=FNTree.Find(@node);
end;

function TsrBitcastList.Find(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
var
 node:PsrBitcast;
begin
 Result:=nil;
 node:=_Find(dtype,src);
 if (node<>nil) then
 begin
  Result:=node^.dst;
 end;
end;

Procedure TsrBitcastList.Save(dtype:TsrDataType;src,dst:PsrRegNode);
var
 node:PsrBitcast;
begin
 node:=rSlot.Emit.Alloc(SizeOf(TsrBitcast));
 node^:=Default(TsrBitcast);
 node^.key.dtype:=dtype;
 node^.key.src:=src;
 node^.dst:=dst;
 FNTree.Insert(node);
end;

function TsrBitcastList.FetchRead(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
var
 pConstList:PsrConstList;
 dst:PsrRegNode;
 pConst:PsrConst;

begin
 Result:=src;
 if (src=nil) then Exit;
 if (dtype=dtUnknow) or (dtype=src^.dtype) then Exit;

 Assert(TryBitcastType(src^.dtype,dtype));

 if src^.is_const then
 begin
  //only from consts, first
  dst:=Find(dtype,src);
  if (dst<>nil) then Exit(dst);

  pConst:=src^.AsConst;

  pConstList:=rSlot.Emit.GetConstList;
  pConst:=pConstList^.Bitcast(dtype,pConst);

  dst:=rSlot.New(src^.pLine,dtype);
  dst^.pWriter:=pConst;

  //
  Save(dtype,src,dst);
 end else
 begin
  dst:=rSlot.New(src^.pLine,dtype);
  dst^.pWriter:=src;
 end;

 Result:=dst;
end;

function TsrBitcastList.FetchDstr(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
var
 dst:PsrRegNode;
begin
 Result:=src;
 if (src=nil) then Exit;
 if (dtype=dtUnknow) or (dtype=src^.dtype) then Exit;

 Assert(TryBitcastType(src^.dtype,dtype));

 dst:=rSlot.New(src^.pLine,dtype);
 dst^.pWriter:=src^.pWriter;

 src^.pWriter:=dst;

 Result:=dst;
end;

function TsrBitcastList.FetchCast(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
var
 pConstList:PsrConstList;
 dst:PsrRegNode;
 pConst:PsrConst;

begin
 Result:=src;
 if (src=nil) then Exit;
 if (dtype=dtUnknow) or (dtype=src^.dtype) then Exit;

 dst:=nil;

 Assert(TryBitcastType(src^.dtype,dtype));

 //
 dst:=Find(dtype,src);
 if (dst<>nil) then Exit(dst);

 if src^.is_const then
 begin
  pConst:=src^.AsConst;

  pConstList:=rSlot.Emit.GetConstList;
  pConst:=pConstList^.Bitcast(dtype,pConst);

  dst:=rSlot.New(src^.pLine,dtype);
  dst^.pWriter:=pConst;
 end else
 begin
  if TryBitcastType(src^.dtype,dtype) then
  begin
   dst:=rSlot.New(src^.pLine,dtype);

   rSlot.Emit.OpCast(src^.pLine,dst,src)
  end else
  begin
   Writeln('bitcast:',src^.dtype,'<>',dtype);
   Assert(false,'bitcast');
  end;
 end;

 //
 Save(dtype,src,dst);

 Result:=dst;
end;

end.

