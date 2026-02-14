unit vm_key_instance;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 p_vm_key_instance=^t_vm_key_instance;
 t_vm_key_instance=object
  pLeft :p_vm_key_instance;
  pRight:p_vm_key_instance;
  key   :Pointer;
 end;

procedure vm_key_instance_splay (var root:p_vm_key_instance;key:Pointer);
function  vm_key_instance_find  (var root:p_vm_key_instance;key:Pointer):Boolean;
procedure vm_key_instance_insert(var root:p_vm_key_instance;node:p_vm_key_instance);
procedure vm_key_instance_delete(var root:p_vm_key_instance;node:p_vm_key_instance);
function  vm_key_instance_first (root:p_vm_key_instance):Pointer;
function  vm_key_instance_next  (root,node:p_vm_key_instance):Pointer;

implementation

procedure vm_key_instance_splay(var root:p_vm_key_instance;key:Pointer);
label
 _left,
 _right;
var
 llist,rlist:p_vm_key_instance;
 ltree,rtree:p_vm_key_instance;
 y          :p_vm_key_instance;
begin
 if (root=nil) then Exit;

 llist:=nil;
 rlist:=nil;
 repeat

  if (key<root^.key) then
  begin
   y:=root^.pLeft;
   if (y=nil) then break;
   if (y^.pLeft=nil) then
   begin
    _left:
    root^.pLeft:=rlist;
    rlist:=root;
    root:=y;
   end else
   if (key<y^.key) then
   begin
    root^.pLeft:=y^.pRight;
    y^.pRight:=root;
    root:=y^.pLeft;
    y^.pLeft:=rlist;
    rlist:=y;
   end else
   begin
    goto _left;
   end;
  end else
  if (key>root^.key) then
  begin
   y:=root^.pRight;
   if (y=nil) then break;
   if (y^.pRight=nil) then
   begin
    _right:
    root^.pRight:=llist;
    llist:=root;
    root:=y;
   end else
   if (key>y^.key) then
   begin
    root^.pRight:=y^.pLeft;
    y^.pLeft:=root;
    root:=y^.pRight;
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

 ltree:=root^.pLeft;
 while (llist<>nil) do
 begin
  y:=llist^.pRight;
  llist^.pRight:=ltree;
  ltree:=llist;
  llist:=y;
 end;

 rtree:=root^.pRight;
 while (rlist<>nil) do
 begin
  y:=rlist^.pLeft;
  rlist^.pLeft:=rtree;
  rtree:=rlist;
  rlist:=y;
 end;

 root^.pLeft :=ltree;
 root^.pRight:=rtree;
end;

function vm_key_instance_find(var root:p_vm_key_instance;key:Pointer):Boolean;
begin
 Result:=False;

 vm_key_instance_splay(root,key);

 if (root<>nil) then
 if (root^.key=key) then
 begin
  Result:=True;
 end;

end;

procedure vm_key_instance_insert(var root:p_vm_key_instance;node:p_vm_key_instance);
begin
 vm_key_instance_splay(root,node^.key);

 if (root=nil) then
 begin
  //
 end else
 if (node^.key>root^.key) then
 begin
  node^.pRight:=root^.pRight;
  node^.pLeft :=root;
  root^.pRight:=nil;
 end else
 begin
  node^.pLeft :=root^.pLeft;
  node^.pRight:=root;
  root^.pLeft :=nil;
 end;

 root:=node;
end;

procedure vm_key_instance_delete(var root:p_vm_key_instance;node:p_vm_key_instance);
var
 pLeft :p_vm_key_instance;
 pRight:p_vm_key_instance;
 pMax  :p_vm_key_instance;
begin
 vm_key_instance_splay(root,node^.key);

 if (root=node) then
 begin
  pLeft :=root^.pLeft;
  pRight:=root^.pRight;

  if (pLeft<>nil) then
  begin
   pMax:=pLeft;
   while (pMax^.pRight<>nil) do
   begin
    pMax:=pMax^.pRight;
   end;

   root:=pLeft;

   vm_key_instance_splay(root,pMax^.key);

   root^.pRight:=pRight;
  end else
  begin
   root:=pRight;
  end;
 end;

end;

function vm_key_instance_first(root:p_vm_key_instance):Pointer;
var
 node:p_vm_key_instance;
begin
 Result:=nil;
 node:=root;
 While (node<>nil) do
 begin
  Result:=node;
  node:=node^.pLeft;
 end;
end;

function vm_key_instance_next(root,node:p_vm_key_instance):Pointer;
var
 y,r:p_vm_key_instance;
begin
 Result:=nil;

 if (root=nil) or (node=nil) then Exit;

 r:=root;
 y:=nil;

 if (node^.pRight<>nil) then
 begin
  y:=node^.pRight;
  while (y^.pLeft<>nil) do y:=y^.pLeft;
  Exit(y);
 end;

 while (r<>nil) do
 begin
  if (node^.key=r^.key) then
  begin
   Break;
  end else
  if (node^.key<r^.key) then
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


end.

