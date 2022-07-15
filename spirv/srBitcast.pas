unit srBitcast;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils,
  srNodes,
  srTypes,
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

 TsrBitcastList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrBitcast,TsrBitcast>;
  var
   pRoot:Pointer;
   FNTree:TNodeFetch;
  function Find(dtype:TsrDataType;src:PsrRegNode):PsrBitcast;
  function FetchRead(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
  function FetchDst(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
  function FetchCast(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
 end;

implementation

uses
 SprvEmit,
 emit_op;

function TsrBitcast.c(n1,n2:PsrBitcast):Integer;
begin
 Result:=CompareByte(n1^.key,n2^.key,SizeOf(TsrBitcast.key));
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
 if (dtype=dtUnknow) or (dtype=src^.dtype) then
 begin
  src^.mark_read;
  Exit;
 end;

 src^.mark_read;
 dst:=PSprvEmit(pRoot)^.NewReg(dtype);
 dst^.pLine:=src^.pLine;
 dst^.SetReg(src);

 dst^.mark_read;
 Result:=dst;
end;

function TsrBitcastList.FetchDst(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
begin
 Result:=src;
 if (src=nil) then Exit;
 if (dtype=dtUnknow) or (dtype=src^.dtype) then Exit;

 Result:=PSprvEmit(pRoot)^.NewReg(dtype);
 Result^.pLine:=src^.pLine;
 Result^.mark_read;

 Result^.pWriter:=src^.pWriter;
 src^.SetReg(Result);
end;

function TsrBitcastList.FetchCast(dtype:TsrDataType;src:PsrRegNode):PsrRegNode;
var
 node:PsrBitcast;
 dst:PsrRegNode;
 pConst:PsrConst;

begin
 Result:=src;
 if (src=nil) then Exit;
 if (dtype=dtUnknow) or (dtype=src^.dtype) then
 begin
  src^.mark_read;
  Exit;
 end;

 dst:=nil;

 node:=Find(dtype,src);
 if (node<>nil) then
 begin
  dst:=node^.dst;
  dst^.mark_read;
  Result:=dst;
  Exit;
 end;

 if src^.is_const then
 begin
  pConst:=src^.AsConst;
  pConst^.mark_unread;
  dst:=PSprvEmit(pRoot)^.NewReg(dtype);
  dst^.pLine:=src^.pLine;
  dst^.SetConst(PSprvEmit(pRoot)^.FConsts.Bitcast(dtype,pConst))
 end else
 begin
  if TryBitcastType(src^.dtype,dtype) then
  begin
   src^.mark_read;
   dst:=PSprvEmit(pRoot)^.NewReg(dtype);
   dst^.pLine:=src^.pLine;
   PEmitOp(pRoot)^.emit_OpCastExt(src^.pLine,dst,src);
  end else
  begin
   Assert(false,'bitcast');
  end;
 end;

 node:=PSprvEmit(pRoot)^.Alloc(SizeOf(TsrBitcast));
 node^:=Default(TsrBitcast);
 node^.key.dtype:=dtype;
 node^.key.src:=src;
 node^.dst:=dst;
 FNTree.Insert(node);

 dst^.mark_read;
 Result:=dst;
end;

end.

