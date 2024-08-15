unit ginodes;

{$mode objfpc}{$H+}

interface

type
 generic TNodeStack<PNode>=object
  pHead:PNode;
  procedure Push_head(Node:PNode);
  function  Pop_head:PNode;
  procedure Move_from(var s:TNodeStack);
 end;

 generic TNodeStackClass<TNode>=object
  pHead:TNode;
  procedure Push_head(Node:TNode);
  function  Pop_head:TNode;
  procedure Move_from(var s:TNodeStackClass);
 end;

 generic TNodeQueueClass<TNode>=object
  pHead,pTail:TNode;
  procedure Push_head(Node:TNode);
  procedure Push_tail(Node:TNode);
  function  Pop_head:TNode;
  procedure InsertAfter(node,new:TNode);
 end;

 generic TNodeListClass<TNode>=object
  pHead,pTail:TNode;
  procedure Push_head(Node:TNode);
  procedure Push_tail(Node:TNode);
  function  Pop_head:TNode;
  function  Pop_tail:TNode;
  procedure InsertAfter(node,new:TNode);
  procedure InsertBefore(node,new:TNode);
  procedure Remove(node:TNode);
 end;

 generic TNodeTreeClass<TNode>=object
  var
   pRoot:TNode;
  function  _Splay(key:Pointer):Integer;
  function  Min:TNode;
  function  Max:TNode;
  function  Next(node:TNode):TNode;
  function  Prev(node:TNode):TNode;
  function  Find(key:Pointer):TNode;
  function  Find_bg(key:Pointer):TNode;
  function  Find_be(key:Pointer):TNode;
  function  Find_ls(key:Pointer):TNode;
  function  Find_le(key:Pointer):TNode;
  function  Insert(node:TNode):Boolean;
  function  Delete(node:TNode):Boolean;
 end;

function ComparePChar(buf1,buf2:PChar):Integer;
function ComparePtruint(buf1,buf2:PPtruint;count:PtrUint):Integer;

implementation

function TNodeTreeClass._Splay(key:Pointer):Integer;
label
 _left,
 _right;
var
 llist,rlist:TNode;
 ltree,rtree:TNode;
 y          :TNode;
begin
 if (pRoot=nil) then Exit(0);

 llist:=nil;
 rlist:=nil;
 repeat
  Result:=TNode.c(key,@pRoot.key);

  if (Result<0) then
  begin
   y:=pRoot.pLeft;
   if (y=nil) then break;
   if (y.pLeft=nil) then
   begin
    _left:
    pRoot.pLeft:=rlist;
    rlist:=pRoot;
    pRoot:=y;
   end else
   if (TNode.c(key,@y.key)<0) then
   begin
    pRoot.pLeft:=y.pRight;
    y.pRight:=pRoot;
    pRoot:=y.pLeft;
    y.pLeft:=rlist;
    rlist:=y;
   end else
   begin
    goto _left;
   end;
  end else
  if (Result>0) then
  begin
   y:=pRoot.pRight;
   if (y=nil) then break;
   if (y.pRight=nil) then
   begin
    _right:
    pRoot.pRight:=llist;
    llist:=pRoot;
    pRoot:=y;
   end else
   if (TNode.c(key,@y.key)>0) then
   begin
    pRoot.pRight:=y.pLeft;
    y.pLeft:=pRoot;
    pRoot:=y.pRight;
    y.pRight:=llist;
    llist:=y;
   end else
   begin
    goto _right;
   end;
  end else
  begin
   Break;
  end;
 until false;

 ltree:=pRoot.pLeft;
 while (llist<>nil) do
 begin
  y:=llist.pRight;
  llist.pRight:=ltree;
  ltree:=llist;
  llist:=y;
 end;

 rtree:=pRoot.pRight;
 while (rlist<>nil) do
 begin
  y:=rlist.pLeft;
  rlist.pLeft:=rtree;
  rtree:=rlist;
  rlist:=y;
 end;

 pRoot.pLeft :=ltree;
 pRoot.pRight:=rtree;
 Result:=TNode.c(key,@pRoot.key);
end;

function TNodeTreeClass.Min:TNode;
var
 node:TNode;
begin
 Result:=nil;
 node:=pRoot;
 While (node<>nil) do
 begin
  Result:=node;
  node:=node.pLeft;
 end;
end;

function TNodeTreeClass.Max:TNode;
var
 node:TNode;
begin
 Result:=nil;
 node:=pRoot;
 While (node<>nil) do
 begin
  Result:=node;
  node:=node.pRight;
 end;
end;

function TNodeTreeClass.Next(node:TNode):TNode;
var
 y,r:TNode;
 c:Integer;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;

 r:=pRoot;
 y:=nil;

 if (node.pRight<>nil) then
 begin
  y:=node.pRight;
  while (y.pLeft<>nil) do y:=y.pLeft;
  Exit(y);
 end;

 while (r<>nil) do
 begin
  c:=TNode.c(@node.key,@r.key);
  if (c=0) then
  begin
   Break;
  end else
  if (c<0) then
  begin
   y:=r;
   r:=r.pLeft;
  end else
  begin
   r:=r.pRight;
  end;
 end;

 Exit(y);
end;

function TNodeTreeClass.Prev(node:TNode):TNode;
var
 y,r:TNode;
 c:Integer;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;

 r:=pRoot;
 y:=nil;

 if (node.pLeft<>nil) then
 begin
  y:=node.pLeft;
  while (y.pRight<>nil) do y:=y.pRight;
  Exit(y);
 end;

 while (r<>nil) do
 begin
  c:=TNode.c(@node.key,@r.key);
  if (c=0) then
  begin
   break;
  end else
  if (c>0) then
  begin
   y:=r;
   r:=r.pRight;
  end else
  begin
   r:=r.pLeft;
  end;
 end;

 Exit(y);
end;

function TNodeTreeClass.Find(key:Pointer):TNode;
begin
 Result:=nil;
 if (pRoot=nil) or (key=nil) then Exit;
 if (_Splay(key)=0) then Result:=pRoot;
end;

function TNodeTreeClass.Find_bg(key:Pointer):TNode;
var
 c:Integer;
begin
 Result:=nil;
 if (pRoot=nil) or (key=nil) then Exit;
 c:=_Splay(key);
 if (c=0) then
 begin
  //=
  Result:=Next(pRoot);
 end else
 if (c<0) then
 begin
  //<
  Result:=pRoot;
 end else
 begin
  //>
  Result:=Next(pRoot);
 end;
end;

function TNodeTreeClass.Find_be(key:Pointer):TNode;
var
 c:Integer;
begin
 Result:=nil;
 if (pRoot=nil) or (key=nil) then Exit;
 c:=_Splay(key);
 if (c=0) then
 begin
  //=
  Result:=pRoot;
 end else
 if (c<0) then
 begin
  //<
  Result:=pRoot;
 end else
 begin
  //>
  Result:=Next(pRoot);
 end;
end;

function TNodeTreeClass.Find_ls(key:Pointer):TNode;
var
 c:Integer;
begin
 Result:=nil;
 if (pRoot=nil) or (key=nil) then Exit;
 c:=_Splay(key);
 if (c=0) then
 begin
  //=
  Result:=Prev(pRoot);
 end else
 if (c<0) then
 begin
  //<
  Result:=Prev(pRoot);
 end else
 begin
  //>
  Result:=pRoot;
 end;
end;

function TNodeTreeClass.Find_le(key:Pointer):TNode;
var
 c:Integer;
begin
 Result:=nil;
 if (pRoot=nil) or (key=nil) then Exit;
 c:=_Splay(key);
 if (c=0) then
 begin
  //=
  Result:=pRoot;
 end else
 if (c<0) then
 begin
  //<
  Result:=Prev(pRoot);
 end else
 begin
  //>
  Result:=pRoot;
 end;
end;

function TNodeTreeClass.Insert(node:TNode):Boolean;
var
 c:Integer;
begin
 Result:=False;
 if (node=nil) then Exit;
 if (pRoot=nil) then
 begin
  pRoot:=node;
 end else
 begin
  c:=_Splay(@node.key);
  if (c=0) then Exit;
  if (c>0) then
  begin
   node.pRight:=pRoot.pRight;
   node.pLeft :=pRoot;
   pRoot.pRight:=nil;
  end else
  begin
   node.pLeft :=pRoot.pLeft;
   node.pRight:=pRoot;
   pRoot.pLeft:=nil;
  end;
  pRoot:=node;
 end;
 Result:=True;
end;

function TNodeTreeClass.Delete(node:TNode):Boolean;
var
 pLeft :TNode;
 pRight:TNode;
 pMax  :TNode;
begin
 Result:=False;

 if (pRoot=nil) or (node=nil) then Exit;
 if (_Splay(@node.key)<>0) then Exit;

 pLeft :=pRoot.pLeft;
 pRight:=pRoot.pRight;

 if (pLeft<>nil) then
 begin
  pMax:=pLeft;
  while (pMax.pRight<>nil) do
  begin
   pMax:=pMax.pRight;
  end;

  pRoot:=pLeft;
  _Splay(@pMax.key);

  pRoot.pRight:=pRight;
 end else
 begin
  pRoot:=pRight;
 end;

 Result:=True;
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

//

procedure TNodeStackClass.Push_head(Node:TNode);
begin
 if (pHead=nil) then
 begin
  node.pNext:=nil;
 end else
 begin
  node.pNext:=pHead;
 end;
 pHead:=node;
end;

function TNodeStackClass.Pop_head:TNode;
begin
 Result:=pHead;
 if (pHead<>nil) then
 begin
  pHead:=pHead.pNext;
 end;
 if (Result<>nil) then
 begin
  Result.pNext:=nil;
 end;
end;

procedure TNodeStackClass.Move_from(var s:TNodeStackClass);
var
 node:TNode;
begin
 repeat
  node:=s.Pop_head;
  if (node=nil) then Break;
  Push_head(node);
 until false;
end;

//--

procedure TNodeQueueClass.Push_head(Node:TNode);
begin
 if (pHead=nil) then
 begin
  pTail:=node;
  node.pNext:=nil;
 end else
 begin
  node.pNext:=pHead;
 end;
 pHead:=node;
end;

procedure TNodeQueueClass.Push_tail(Node:TNode);
begin
 if (pTail=nil) then
 begin
  pHead:=node;
  node.pNext:=nil;
 end else
 begin
  pTail.pNext:=node;
 end;
 pTail:=node;
end;

function TNodeQueueClass.Pop_head:TNode;
begin
 if (pHead=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=pHead;
  pHead:=pHead.pNext;
  if (pHead=nil) then
  begin
   pTail:=nil;
  end;
  Result.pNext:=nil;
 end;
end;

procedure TNodeQueueClass.InsertAfter(node,new:TNode);
begin
 if (node.pNext=nil) then
 begin
  new.pNext:=nil;
  pTail:=new;
 end else
 begin
  new.pNext:=node.pNext;
 end;
 node.pNext:=new;
end;

//--

procedure TNodeListClass.Push_head(Node:TNode);
begin
 if (pHead=nil) then
 begin
  pTail:=node;
  node.pNext:=nil;
 end else
 begin
  pHead.pPrev:=node;
  node.pNext:=pHead;
 end;
 node.pPrev:=nil;
 pHead:=node;
end;

procedure TNodeListClass.Push_tail(Node:TNode);
begin
 if (pTail=nil) then
 begin
  pHead:=node;
  node.pPrev:=nil;
 end else
 begin
  pTail.pNext:=node;
  node.pPrev:=pTail;
 end;
 node.pNext:=nil;
 pTail:=node;
end;

function TNodeListClass.Pop_head:TNode;
begin
 if (pHead=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=pHead;
  pHead:=pHead.pNext;
  if (pHead=nil) then
  begin
   pTail:=nil;
  end else
  begin
   pHead.pPrev:=nil;
  end;
  Result.pPrev:=nil;
  Result.pNext:=nil;
 end;
end;

function TNodeListClass.Pop_tail:TNode;
begin
 if (pTail=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=pTail;
  pTail:=pTail.pPrev;
  if (pTail=nil) then
  begin
   pHead:=nil;
  end else
  begin
   pTail.pNext:=nil;
  end;
  Result.pPrev:=nil;
  Result.pNext:=nil;
 end;
end;

procedure TNodeListClass.InsertAfter(node,new:TNode);
begin
 new.pPrev:=node;
 if (node.pNext=nil) then
 begin
  new.pNext:=nil;
  pTail:=new;
 end else
 begin
  new.pNext:=node.pNext;
  node.pNext.pPrev:=new;
 end;
 node.pNext:=new;
end;

procedure TNodeListClass.InsertBefore(node,new:TNode);
begin
 new.pNext:=node;
 if (node.pPrev=nil) then
 begin
  new.pPrev:=nil;
  pHead:=new;
 end else
 begin
  new.pPrev:=node.pPrev;
  TNode(node.pPrev).pNext:=new;
 end;
 node.pPrev:=new;
end;

procedure TNodeListClass.Remove(node:TNode);
begin
 if (node.pPrev=nil) then
 begin
  if (pHead=node) then
  begin
   pHead:=node.pNext;
  end;
 end else
 begin
  node.pPrev.pNext:=node.pNext;
 end;
 if (node.pNext=nil) then
 begin
  if (pTail=node) then
  begin
   pTail:=node.pPrev;
  end;
 end else
 begin
  node.pNext.pPrev:=node.pPrev;
 end;
end;

//

function ComparePChar(buf1,buf2:PChar):Integer;
begin
 Result:=0;
 if (buf1=nil) and (buf2=nil) then
 begin
  Exit;
 end else
 if (buf1=nil) then
 begin
  Result:=-Integer(buf2^);
 end else
 if (buf2=nil) then
 begin
  Result:=Integer(buf1^);
 end else
 begin
  While true do
  begin
   Result:=Integer(buf1^)-Integer(buf2^);
   if (Result<>0) or (buf1^=#0) or (buf2^=#0) then Exit;
   Inc(buf1);
   Inc(buf2);
  end;
 end;
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

