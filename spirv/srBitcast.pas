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
 PsrBitcastKey=^TsrBitcastKey;
 TsrBitcastKey=packed record
  dtype:TsrDataType;
  src:TsrRegNode;
 end;

 TsrBitcast=class
  pLeft,pRight:TsrBitcast;
  //----
  key:TsrBitcastKey;
  dst:TsrRegNode;
  class function c(n1,n2:PsrBitcastKey):Integer; static;
 end;

 PsrBitcastList=^TsrBitcastList;
 TsrBitcastList=object
  type
   TNodeTree=specialize TNodeTreeClass<TsrBitcast>;
  var
   FTree:TNodeTree;
   rSlot:TsrRegSlot;
  procedure Init(Emit:TCustomEmit); inline;
  function  _Find(dtype:TsrDataType;src:TsrRegNode):TsrBitcast;
  function  Find(dtype:TsrDataType;src:TsrRegNode):TsrRegNode;
  Procedure Save(dtype:TsrDataType;src,dst:TsrRegNode);
  function  FetchRead(dtype:TsrDataType;src:TsrRegNode):TsrRegNode;
  function  FetchDstr(dtype:TsrDataType;src:TsrRegNode):TsrRegNode;
  function  FetchCast(dtype:TsrDataType;src:TsrRegNode):TsrRegNode;
 end;

implementation

class function TsrBitcast.c(n1,n2:PsrBitcastKey):Integer;
begin
 //first dtype
 Result:=ord(n1^.dtype>n2^.dtype)-ord(n1^.dtype<n2^.dtype);
 if (Result<>0) then Exit;
 //second src (not need order sort)
 Result:=ord(ptruint(n1^.src)>ptruint(n2^.src))-ord(ptruint(n1^.src)<ptruint(n2^.src));
end;

procedure TsrBitcastList.Init(Emit:TCustomEmit); inline;
begin
 rSlot.Init(Emit,'BCAST');
end;

function TsrBitcastList._Find(dtype:TsrDataType;src:TsrRegNode):TsrBitcast;
var
 key:TsrBitcastKey;
begin
 key:=Default(TsrBitcastKey);
 key.dtype:=dtype;
 key.src:=src;
 Result:=FTree.Find(@key);
end;

function TsrBitcastList.Find(dtype:TsrDataType;src:TsrRegNode):TsrRegNode;
var
 node:TsrBitcast;
begin
 Result:=nil;
 node:=_Find(dtype,src);
 if (node<>nil) then
 begin
  Result:=node.dst;
 end;
end;

Procedure TsrBitcastList.Save(dtype:TsrDataType;src,dst:TsrRegNode);
var
 node:TsrBitcast;
begin
 node:=rSlot.Emit.specialize New<TsrBitcast>;
 node.key:=Default(TsrBitcastKey);
 node.key.dtype:=dtype;
 node.key.src:=src;
 node.dst:=dst;
 FTree.Insert(node);
end;

function TsrBitcastList.FetchRead(dtype:TsrDataType;src:TsrRegNode):TsrRegNode;
var
 pConstList:PsrConstList;
 dst:TsrRegNode;
 pConst:TsrConst;

begin
 Result:=src;
 if (src=nil) then Exit;
 if (dtype=dtUnknow) or (dtype=src.dtype) then Exit;

 Assert(TryBitcastType(src.dtype,dtype));

 if src.is_const then
 begin
  //only from consts, first
  dst:=Find(dtype,src);
  if (dst<>nil) then Exit(dst);

  pConst:=src.AsConst;

  pConstList:=rSlot.Emit.GetConstList;
  pConst:=pConstList^.Bitcast(dtype,pConst);

  dst:=rSlot.New(src.pLine,dtype);
  dst.pWriter:=pConst;

  //
  Save(dtype,src,dst);
 end else
 begin
  dst:=rSlot.New(src.pLine,dtype);
  dst.pWriter:=src;
 end;

 Result:=dst;
end;

function TsrBitcastList.FetchDstr(dtype:TsrDataType;src:TsrRegNode):TsrRegNode;
var
 dst:TsrRegNode;
begin
 Result:=src;
 if (src=nil) then Exit;
 if (dtype=dtUnknow) or (dtype=src.dtype) then Exit;

 Assert(TryBitcastType(src.dtype,dtype));

 dst:=rSlot.New(src.pLine,dtype);
 dst.pWriter:=src.pWriter;

 src.pWriter:=dst;

 Result:=dst;
end;

function TsrBitcastList.FetchCast(dtype:TsrDataType;src:TsrRegNode):TsrRegNode;
var
 pConstList:PsrConstList;
 dst:TsrRegNode;
 pConst:TsrConst;

begin
 Result:=src;
 if (src=nil) then Exit;
 if (dtype=dtUnknow) or (dtype=src.dtype) then Exit;

 dst:=nil;

 Assert(TryBitcastType(src.dtype,dtype));

 //
 dst:=Find(dtype,src);
 if (dst<>nil) then Exit(dst);

 if src.is_const then
 begin
  pConst:=src.AsConst;

  pConstList:=rSlot.Emit.GetConstList;
  pConst:=pConstList^.Bitcast(dtype,pConst);

  dst:=rSlot.New(src.pLine,dtype);
  dst.pWriter:=pConst;
 end else
 begin
  if TryBitcastType(src.dtype,dtype) then
  begin
   dst:=rSlot.New(src.pLine,dtype);

   rSlot.Emit.OpCast(src.pLine,dst,src)
  end else
  begin
   Writeln('bitcast:',src.dtype,'<>',dtype);
   Assert(false,'bitcast');
  end;
 end;

 //
 Save(dtype,src,dst);

 Result:=dst;
end;

end.

