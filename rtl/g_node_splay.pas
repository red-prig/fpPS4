unit g_node_splay;

{$mode ObjFPC}{$H+}

interface

type
 generic TNodeSplay<TNode>=object
  type
   PNode=^TNode;
  var
   pRoot:PNode;
  function  _Splay(node:PNode):Integer;
  function  Min:PNode;
  function  Max:PNode;
  function  Next(node:PNode):PNode;
  function  Prev(node:PNode):PNode;
  function  Find(node:PNode):PNode;
  function  Find_bg(node:PNode):PNode;
  function  Find_be(node:PNode):PNode;
  function  Find_ls(node:PNode):PNode;
  function  Find_le(node:PNode):PNode;
  function  Insert(node:PNode):Boolean;
  function  Delete(node:PNode):Boolean;
 end;

implementation

function TNodeSplay._Splay(node:PNode):Integer;
label
 _left,
 _right;
var
 llist,rlist:PNode;
 ltree,rtree:PNode;
 y          :PNode;
begin
 if (pRoot=nil) then Exit(0);

 llist:=nil;
 rlist:=nil;
 repeat
  Result:=TNode.c(node,pRoot);

  if (Result<0) then
  begin
   y:=pRoot^.pLeft;
   if (y=nil) then break;
   if (y^.pLeft=nil) then
   begin
    _left:
    pRoot^.pLeft:=rlist;
    rlist:=pRoot;
    pRoot:=y;
   end else
   if (TNode.c(node,y)<0) then
   begin
    pRoot^.pLeft:=y^.pRight;
    y^.pRight:=pRoot;
    pRoot:=y^.pLeft;
    y^.pLeft:=rlist;
    rlist:=y;
   end else
   begin
    goto _left;
   end;
  end else
  if (Result>0) then
  begin
   y:=pRoot^.pRight;
   if (y=nil) then break;
   if (y^.pRight=nil) then
   begin
    _right:
    pRoot^.pRight:=llist;
    llist:=pRoot;
    pRoot:=y;
   end else
   if (TNode.c(node,y)>0) then
   begin
    pRoot^.pRight:=y^.pLeft;
    y^.pLeft:=pRoot;
    pRoot:=y^.pRight;
    y^.pRight:=llist;
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

 ltree:=pRoot^.pLeft;
 while (llist<>nil) do
 begin
  y:=llist^.pRight;
  llist^.pRight:=ltree;
  ltree:=llist;
  llist:=y;
 end;

 rtree:=pRoot^.pRight;
 while (rlist<>nil) do
 begin
  y:=rlist^.pLeft;
  rlist^.pLeft:=rtree;
  rtree:=rlist;
  rlist:=y;
 end;

 pRoot^.pLeft :=ltree;
 pRoot^.pRight:=rtree;
 Result:=TNode.c(node,pRoot);
end;

function TNodeSplay.Min:PNode;
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

function TNodeSplay.Max:PNode;
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

function TNodeSplay.Next(node:PNode):PNode;
var
 y,r:PNode;
 c:Integer;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;

 r:=pRoot;
 y:=nil;

 if (node^.pRight<>nil) then
 begin
  y:=node^.pRight;
  while (y^.pLeft<>nil) do y:=y^.pLeft;
  Exit(y);
 end;

 while (r<>nil) do
 begin
  c:=TNode.c(node,r);
  if (c=0) then
  begin
   Break;
  end else
  if (c<0) then
  begin
   y:=r;
   r:=r^.pLeft;
  end else
  begin
   r:=r^.pRight;
  end;
 end;

 Exit(y);
end;

function TNodeSplay.Prev(node:PNode):PNode;
var
 y,r:PNode;
 c:Integer;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;

 r:=pRoot;
 y:=nil;

 if (node^.pLeft<>nil) then
 begin
  y:=node^.pLeft;
  while (y^.pRight<>nil) do y:=y^.pRight;
  Exit(y);
 end;

 while (r<>nil) do
 begin
  c:=TNode.c(node,r);
  if (c=0) then
  begin
   break;
  end else
  if (c>0) then
  begin
   y:=r;
   r:=r^.pRight;
  end else
  begin
   r:=r^.pLeft;
  end;
 end;

 Exit(y);
end;

function TNodeSplay.Find(node:PNode):PNode;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;
 if (_Splay(node)=0) then Result:=pRoot;
end;

function TNodeSplay.Find_bg(node:PNode):PNode;
var
 c:Integer;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;
 c:=_Splay(node);
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

function TNodeSplay.Find_be(node:PNode):PNode;
var
 c:Integer;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;
 c:=_Splay(node);
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

function TNodeSplay.Find_ls(node:PNode):PNode;
var
 c:Integer;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;
 c:=_Splay(node);
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

function TNodeSplay.Find_le(node:PNode):PNode;
var
 c:Integer;
begin
 Result:=nil;
 if (pRoot=nil) or (node=nil) then Exit;
 c:=_Splay(node);
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

function TNodeSplay.Insert(node:PNode):Boolean;
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
  c:=_Splay(node);
  if (c=0) then Exit;
  if (c>0) then
  begin
   node^.pRight:=pRoot^.pRight;
   node^.pLeft :=pRoot;
   pRoot^.pRight:=nil;
  end else
  begin
   node^.pLeft :=pRoot^.pLeft;
   node^.pRight:=pRoot;
   pRoot^.pLeft:=nil;
  end;
  pRoot:=node;
 end;
 Result:=True;
end;

function TNodeSplay.Delete(node:PNode):Boolean;
var
 pLeft :PNode;
 pRight:PNode;
 pMax  :PNode;
begin
 Result:=False;

 if (pRoot=nil) or (node=nil) then Exit;
 if (_Splay(node)<>0) then Exit;

 pLeft :=pRoot^.pLeft;
 pRight:=pRoot^.pRight;

 if (pLeft<>nil) then
 begin
  pMax:=pLeft;
  while (pMax^.pRight<>nil) do
  begin
   pMax:=pMax^.pRight;
  end;

  pRoot:=pLeft;
  _Splay(pMax);

  pRoot^.pRight:=pRight;
 end else
 begin
  pRoot:=pRight;
 end;

 Result:=True;
end;

end.

