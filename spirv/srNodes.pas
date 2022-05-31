unit srNodes;

{$mode objfpc}{$H+}

interface

type
 generic TNodeStack<PNode>=object
  pHead:PNode;
  procedure Push_head(Node:PNode);
  function  Pop_head:PNode;
  procedure Move_from(var s:TNodeStack);
 end;

 generic TNodeQueue<PNode>=object
  pHead,pTail:PNode;
  procedure Push_head(Node:PNode);
  procedure Push_tail(Node:PNode);
  function  Pop_head:PNode;
  procedure InsertAfter(node,new:PNode);
 end;

 generic TNodeList<PNode>=object
  pHead,pTail:PNode;
  procedure Push_head(Node:PNode);
  procedure Push_tail(Node:PNode);
  function  Pop_head:PNode;
  function  Pop_tail:PNode;
  procedure InsertAfter(node,new:PNode);
  procedure InsertBefore(node,new:PNode);
  procedure Remove(node:PNode);
 end;

 generic TNodeFetch<PNode,TNode>=object
  var
   pRoot:PNode;
  function  _Splay(node:PNode):Integer;
  function  Min:PNode;
  function  Max:PNode;
  function  Next(node:PNode):PNode;
  function  Prev(node:PNode):PNode;
  function  Find(node:PNode):PNode;
  function  Find_be(node:PNode):PNode;
  function  Find_le(node:PNode):PNode;
  procedure Insert(new:PNode);
 end;

 TsrNodeType=(
  ntUnknow,
  ntLiteral,
  ntString,
  ntConst,
  ntType,
  ntLabel,
  ntBlock,
  ntFunc,
  ntVar,
  ntRefId,
  ntOp,
  ntReg,
  ntVolatile,
  ntInput,
  ntVertLayout,
  ntFragLayout,
  ntOutput,
  ntChain,
  ntUniform,
  ntBuffer
 );

 TOpParamSingle=packed object
  ntype:TsrNodeType;
  pData:Pointer;
  procedure SetParam(_ntype:TsrNodeType;_Data:Pointer);
  function  AsConst:Pointer;
  function  AsOp:Pointer;
  function  AsReg:Pointer;
  function  AsVar:Pointer;
  function  AsBlock:Pointer;
  function  AsInput:Pointer;
  function  AsRefId:Pointer;
  function  AsUniform:Pointer;
 end;

 TfnAlloc=Function(Size:ptruint):Pointer of object;

function ComparePtruint(buf1,buf2:PPtruint;count:PtrUint):Integer;

implementation

function TNodeFetch._Splay(node:PNode):Integer;
var
 aux:TNode;
 t,l,r,y:PNode;
begin
 t:=pRoot;
 l:=@aux;
 r:=@aux;
 aux.pLeft:=nil;
 aux.pRight:=nil;
 while (true) do
 begin
  Result:=TNode.c(t,node);
  if (Result=0) then Break;
  if (Result>0) then
  begin
   if (t^.pLeft=nil) then break;
   if (TNode.c(node,t^.pLeft)<0) then
   begin
    y:=t^.pLeft;                           // rotate pRight
    t^.pLeft:=y^.pRight;
    y^.pRight:=t;
    t:=y;
    if (t^.pLeft=nil) then break;
   end;
   r^.pLeft:=t;                            // link pRight
   r:=t;
   t:=t^.pLeft;
  end else
  begin
   if (t^.pRight=nil) then break;
   if (TNode.c(node,t^.pRight)>0) then
   begin
    y:=t^.pRight;                          // rotate pLeft
    t^.pRight:=y^.pLeft;
    y^.pLeft:=t;
    t:=y;
    if (t^.pRight=nil) then break;
   end;
   l^.pRight:=t;                           // link pLeft
   l:=t;
   t:=t^.pRight;
  end;
 end;
 l^.pRight:=t^.pLeft; // assemble
 r^.pLeft :=t^.pRight;
 t^.pLeft :=aux.pRight;
 t^.pRight:=aux.pLeft;
 pRoot:=t;
end;

function TNodeFetch.Min:PNode;
var
 node:PNode;
begin
 Result:=nil;
 node:=pRoot;
 While (node<>nil) do
 begin
  Result:=node;
  node:=node^.pLeft;
 end;
end;

function TNodeFetch.Max:PNode;
var
 node:PNode;
begin
 Result:=nil;
 node:=pRoot;
 While (node<>nil) do
 begin
  Result:=node;
  node:=node^.pRight;
 end;
end;

function TNodeFetch.Next(node:PNode):PNode;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;
 if (node^.pRight=nil) then
 begin
  _Splay(node);
 end;
 node:=node^.pRight;
 While (node<>nil) do
 begin
  Result:=node;
  node:=node^.pLeft;
 end;
end;

function TNodeFetch.Prev(node:PNode):PNode;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;
 if (node^.pLeft=nil) then
 begin
  _Splay(node);
 end;
 node:=node^.pLeft;
 While (node<>nil) do
 begin
  Result:=node;
  node:=node^.pRight;
 end;
end;

function TNodeFetch.Find(node:PNode):PNode;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;
 if (_Splay(node)=0) then Result:=pRoot;
end;

function TNodeFetch.Find_be(node:PNode):PNode;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;
 if (_Splay(node)<0) then
 begin
  Result:=Next(pRoot);
 end else
 begin
  Result:=pRoot;
 end;
end;

function TNodeFetch.Find_le(node:PNode):PNode;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;
 if (_Splay(node)>0) then
 begin
  Result:=Prev(pRoot);
 end else
 begin
  Result:=pRoot;
 end;
end;

procedure TNodeFetch.Insert(new:PNode);
var
 c:Integer;
begin
 if (new=nil) then Exit;
 if (pRoot=nil) then
 begin
  pRoot:=new;
 end else
 begin
  c:=TNode.c(pRoot,new);
  if (c<>0) then
  begin
   if (c<0) then
   begin
    new^.pRight:=pRoot^.pRight;
    new^.pLeft :=pRoot;
    pRoot^.pRight:=nil;
   end else
   begin
    new^.pLeft :=pRoot^.pLeft;
    new^.pRight:=pRoot;
    pRoot^.pLeft:=nil;
   end;
   pRoot:=new;
  end;
 end;
end;

//--

procedure TNodeStack.Push_head(Node:PNode);
begin
 if (pHead=nil) then
 begin
  node^.pNext:=nil;
 end else
 begin
  node^.pNext:=pHead;
 end;
 pHead:=node;
end;

function TNodeStack.Pop_head:PNode;
begin
 Result:=pHead;
 if (pHead<>nil) then
 begin
  pHead:=pHead^.pNext;
 end;
 if (Result<>nil) then
 begin
  Result^.pNext:=nil;
 end;
end;

procedure TNodeStack.Move_from(var s:TNodeStack);
var
 node:PNode;
begin
 repeat
  node:=s.Pop_head;
  if (node=nil) then Break;
  Push_head(node);
 until false;
end;

//--

procedure TNodeQueue.Push_head(Node:PNode);
begin
 if (pHead=nil) then
 begin
  pTail:=node;
  node^.pNext:=nil;
 end else
 begin
  node^.pNext:=pHead;
 end;
 pHead:=node;
end;

procedure TNodeQueue.Push_tail(Node:PNode);
begin
 if (pTail=nil) then
 begin
  pHead:=node;
  node^.pNext:=nil;
 end else
 begin
  pTail^.pNext:=node;
 end;
 pTail:=node;
end;

function TNodeQueue.Pop_head:PNode;
begin
 if (pHead=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=pHead;
  pHead:=pHead^.pNext;
  if (pHead=nil) then
  begin
   pTail:=nil;
  end;
  Result^.pNext:=nil;
 end;
end;

procedure TNodeQueue.InsertAfter(node,new:PNode);
begin
 if (node^.pNext=nil) then
 begin
  new^.pNext:=nil;
  pTail:=new;
 end else
 begin
  new^.pNext:=node^.pNext;
 end;
 node^.pNext:=new;
end;

//--

procedure TNodeList.Push_head(Node:PNode);
begin
 if (pHead=nil) then
 begin
  pTail:=node;
  node^.pNext:=nil;
 end else
 begin
  pHead^.pPrev:=node;
  node^.pNext:=pHead;
 end;
 node^.pPrev:=nil;
 pHead:=node;
end;

procedure TNodeList.Push_tail(Node:PNode);
begin
 if (pTail=nil) then
 begin
  pHead:=node;
  node^.pPrev:=nil;
 end else
 begin
  pTail^.pNext:=node;
  node^.pPrev:=pTail;
 end;
 node^.pNext:=nil;
 pTail:=node;
end;

function TNodeList.Pop_head:PNode;
begin
 if (pHead=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=pHead;
  pHead:=pHead^.pNext;
  if (pHead=nil) then
  begin
   pTail:=nil;
  end else
  begin
   pHead^.pPrev:=nil;
  end;
  Result^.pPrev:=nil;
  Result^.pNext:=nil;
 end;
end;

function TNodeList.Pop_tail:PNode;
begin
 if (pTail=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=pTail;
  pTail:=pTail^.pPrev;
  if (pTail=nil) then
  begin
   pHead:=nil;
  end else
  begin
   pTail^.pNext:=nil;
  end;
  Result^.pPrev:=nil;
  Result^.pNext:=nil;
 end;
end;

procedure TNodeList.InsertAfter(node,new:PNode);
begin
 new^.pPrev:=node;
 if (node^.pNext=nil) then
 begin
  new^.pNext:=nil;
  pTail:=new;
 end else
 begin
  new^.pNext:=node^.pNext;
  node^.pNext^.pPrev:=new;
 end;
 node^.pNext:=new;
end;

procedure TNodeList.InsertBefore(node,new:PNode);
begin
 new^.pNext:=node;
 if (node^.pPrev=nil) then
 begin
  new^.pPrev:=nil;
  pHead:=new;
 end else
 begin
  new^.pPrev:=node^.pPrev;
  node^.pPrev^.pNext:=new;
 end;
 node^.pPrev:=new;
end;

procedure TNodeList.Remove(node:PNode);
begin
 if (node^.pPrev=nil) then
 begin
  if (pHead=node) then
  begin
   pHead:=node^.pNext;
  end;
 end else
 begin
  node^.pPrev^.pNext:=node^.pNext;
 end;
 if (node^.pNext=nil) then
 begin
  if (pTail=node) then
  begin
   pTail:=node^.pPrev;
  end;
 end else
 begin
  node^.pNext^.pPrev:=node^.pPrev;
 end;
end;

procedure TOpParamSingle.SetParam(_ntype:TsrNodeType;_Data:Pointer);
begin
 ntype:=_ntype;
 pData:=_Data;
end;

function TOpParamSingle.AsConst:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (ntype<>ntConst) then Exit;
 Result:=pData;
end;

function TOpParamSingle.AsOp:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (ntype<>ntOp) then Exit;
 Result:=pData;
end;

function TOpParamSingle.AsReg:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (ntype<>ntReg) then Exit;
 Result:=pData;
end;

function TOpParamSingle.AsVar:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (ntype<>ntVar) then Exit;
 Result:=pData;
end;

function TOpParamSingle.AsBlock:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (ntype<>ntBlock) then Exit;
 Result:=pData;
end;

function TOpParamSingle.AsInput:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (ntype<>ntInput) then Exit;
 Result:=pData;
end;

function TOpParamSingle.AsRefId:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (ntype<>ntRefId) then Exit;
 Result:=pData;
end;

function TOpParamSingle.AsUniform:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (ntype<>ntUniform) then Exit;
 Result:=pData;
end;

function ComparePtruint(buf1,buf2:PPtruint;count:PtrUint):Integer;
begin
 Result:=0;
 While (count<>0) do
 begin
  Result:=Integer(buf1^>buf2^)-Integer(buf1^<buf2^);
  if (Result<>0) then Exit;
  Inc(buf1);
  Inc(buf2);
  Dec(count);
 end;
end;

end.

