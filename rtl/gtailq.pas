unit gtailq;

{$mode ObjFPC}{$H+}

interface

type
// PTAILQ_NODE=^TAILQ_NODE;
// TAILQ_NODE=packed record
//  pNext,pPrev:Pointer;
//  //
// end;

 generic TAILQ_HEAD<PNODE>=object
  pHead:PNODE;
  procedure Insert_head(Node:PNODE);
  procedure Insert_tail(Node:PNODE);
  procedure Remove(Node:PNODE);
 end;

 generic TAILQ_ENTRY<PNODE>=object
  pHead,pTail:PNODE;
  procedure Insert_head(Node:PNODE);
  procedure Insert_tail(Node:PNODE);
  procedure Remove(Node:PNODE);
 end;

implementation

procedure TAILQ_HEAD.Insert_head(Node:PNODE);
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

procedure TAILQ_HEAD.Insert_tail(Node:PNODE);
var
 pTail:PNODE;
begin
 if (pHead=nil) then
 begin
  node^.pNext:=nil;
  pHead:=node;
 end else
 begin
  pTail:=pHead;
  repeat
   if (pTail^.pNext=nil) then Break;
   pTail:=pTail^.pNext;
  until false;

  pTail^.pNext:=node;
  node^.pPrev:=pTail;
  node^.pNext:=nil;
 end;
end;

procedure TAILQ_HEAD.Remove(Node:PNODE);
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
 if (node^.pNext<>nil) then
 begin
  node^.pNext^.pPrev:=node^.pPrev;
 end;
end;

//

procedure TAILQ_ENTRY.Insert_head(Node:PNODE);
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

procedure TAILQ_ENTRY.Insert_tail(Node:PNODE);
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

procedure TAILQ_ENTRY.Remove(Node:PNODE);
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

end.

