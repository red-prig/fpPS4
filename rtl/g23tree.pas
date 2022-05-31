unit g23tree;

{$mode objfpc}{$H+}

{ Template implementation of 2-3 trie
  Copyright (C) 2021 Red_prig
  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:
  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.
  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.
}

interface

type
 generic T23treeSet<TKey,TCompare>=packed object
  protected
   type
    PKey=^TKey;
    P23Node=^T23Node;
    PSplitInfo=^TSplitInfo;
    T23Node=packed object
     parent:P23Node;
     childs:array[0..2] of P23Node;
     Keys:array[0..1] of TKey;
    end;
    TSplitInfo=packed record
     Key:TKey;
     Left,Right:P23Node;
     _new:Pointer;
    end;
    p_Iterator=object
     private
      Var
       N:P23Node;
       P:Shortint;
    end;
    TStuff=packed object
     Function  new_Node:P23Node; static; inline;
     procedure Free_node(P:Pointer); static; inline;
     Function  _search_min(PN:P23Node):p_Iterator; static; inline;
     Function  _search_max(PN:P23Node):p_Iterator; static; inline;
     Function  _insert2Leaf(PN:P23Node;Const K:TKey;SI:PSplitInfo):Byte; static;
     function  _insert2node(PN:P23Node;SI:PSplitInfo):Boolean; static;
     function  _find_leaf(PN:P23Node;Const K:TKey):P23Node; static;
     function  _find_node(PN:P23Node;Const K:TKey):p_Iterator; static;
     procedure _remove4leaf(PN:P23Node;first:Boolean); static; inline;
     function  _remove4parent(var PN:P23Node;SI:PSplitInfo):Boolean; static; inline;
    end;
   Const
    dump:P23Node=P23Node(High(PtrUint));
   var
    pRoot:P23Node;
    FSize:SizeUInt;
   procedure _update(PN:P23Node;SI:PSplitInfo); inline;
   procedure _fix(PN:P23Node);
  public
   type
    Iterator=object(p_Iterator)
     public
      Function  Next:Boolean;
      Function  Prev:Boolean;
      Procedure Free; inline;
      Function  Item:PKey;
      function  keyCount:Byte;
    end;
   property  Size:SizeUInt read FSize;
   Procedure Create; inline;
   Procedure Free;
   function  Insert(Const K:TKey):Boolean;
   Function  Contains(Const K:TKey):Boolean; inline;
   Function  delete(Const R:TKey):Boolean;
   Function  erase(Const it:Iterator):Boolean;
   Function  find(Const R:TKey):Iterator;
   Function  find_be(Const R:TKey):Iterator;
   Function  find_le(Const R:TKey):Iterator;
   function  cbegin:Iterator;
   function  cend:Iterator;
 end;

implementation

Function T23treeSet.TStuff.new_Node:P23Node; inline;
begin
 Result:=AllocMem(SizeOf(T23Node));
end;

Function T23treeSet.TStuff._insert2Leaf(PN:P23Node;Const K:TKey;SI:PSplitInfo):Byte;
var
 i:Integer;

 procedure _begin_split; inline;
 begin
  SI^.Left:=new_Node;
  SI^.Right:=PN;
  PN^.childs[2]:=nil; //set size 1
  SI^.Left^.childs[1]:=dump; //set size 1
 end;

begin
 Result:=0; //insert
 if (PN^.childs[1]=nil) then //0
 begin
  PN^.Keys[0]:=K;
  PN^.childs[1]:=dump; //set size 1
 end else
 if (PN^.childs[2]=nil) then //1
 begin
  i:=TCompare.c(PN^.Keys[0],K);
  if (i=0) then Exit(1); //not
  if (i>0) then
  begin
   PN^.Keys[1]:=PN^.Keys[0];
   PN^.Keys[0]:=K;
  end else
  begin
   PN^.Keys[1]:=K;
  end;
  PN^.childs[2]:=dump; //set size 2
 end else
 begin //split leaf
  i:=TCompare.c(PN^.Keys[0],K);
  if (i=0) then Exit(1); //not
  //N 0 1
  if (i>0) then
  begin
   Result:=2;
   _begin_split;

   SI^.Left^.Keys[0]:=K;
   SI^.Key:=PN^.Keys[0];
   PN^.Keys[0]:=PN^.Keys[1];

   PN^.Keys[1]:=Default(TKey);
  end else
  begin
   i:=TCompare.c(PN^.Keys[1],K);
   if (i=0) then Exit(1); //not
   //0 N 1
   if (i>0) then
   begin
    Result:=2;
    _begin_split;

    SI^.Left^.Keys[0]:=PN^.Keys[0];
    PN^.Keys[0]:=PN^.Keys[1];
    SI^.Key:=K;

    PN^.Keys[1]:=Default(TKey);
   end else
   //0 1 N
   begin
    Result:=2;
    _begin_split;

    SI^.Left^.Keys[0]:=PN^.Keys[0];
    PN^.Keys[0]:=K;
    SI^.Key:=PN^.Keys[1];

    PN^.Keys[1]:=Default(TKey);
   end;
  end;
 end;
end;

function T23treeSet.TStuff._insert2node(PN:P23Node;SI:PSplitInfo):Boolean;
Var
 SR:TSplitInfo;
begin
 Result:=true;
 if PN^.childs[1]=nil then //0
 begin
  PN^.Keys[0]  :=SI^.Key;
  PN^.childs[0]:=SI^.Left;
  PN^.childs[1]:=SI^.Right;

  PN^.childs[0]^.parent:=PN;
  PN^.childs[1]^.parent:=PN;
 end else
 if PN^.childs[2]=nil then //1
 begin
  // 0 -> N0 1
  //0 1->N0 1 2
  if PN^.childs[0]=SI^.Right then
  begin
   PN^.Keys[1]  :=PN^.Keys[0];
   PN^.childs[2]:=PN^.childs[1];
   PN^.childs[1]:=PN^.childs[0];
   PN^.Keys[0]  :=SI^.Key;
   PN^.childs[0]:=SI^.Left;
   PN^.childs[0]^.parent:=PN;
  end else
  // 0 1N
  //0 1N 2
  begin
   PN^.Keys[1]  :=SI^.Key;
   PN^.childs[1]:=SI^.Left;
   PN^.childs[2]:=SI^.Right;
   PN^.childs[1]^.parent:=PN;
  end;
 end else
 begin //2
  Result:=False;

  SR:=Default(TSplitInfo);
  if SI^._new<>nil then
  begin
   SR.Left:=SI^._new;
   SI^._new:=nil;
  end else
  begin
   SR.Left :=new_Node;
  end;

  SR.Right:=PN;
  //        ->       0K
  // N 0 1  ->   NK      1K
  //N 0 1 2 -> NN  0N  1N  2N
  if PN^.childs[0]=SI^.Right then
  begin
   SR.Key :=PN^.Keys[0];
   SR.Left^.Keys[0]  :=SI^.Key;
   SR.Left^.childs[0]:=SI^.Left;
   SR.Left^.childs[1]:=PN^.childs[0];
   PN^.Keys[0]  :=PN^.Keys[1];
   PN^.childs[0]:=PN^.childs[1];
   PN^.childs[1]:=PN^.childs[2];
  end else
  //         ->       NK
  // 0 N 1   ->   0K      1K
  //0 N 1 2  -> 0N  NN  1N  2N
  if PN^.childs[1]=SI^.Right then
  begin
   SR.Key :=SI^.Key;
   SR.Left^.Keys[0]  :=PN^.Keys[0];
   SR.Left^.childs[0]:=PN^.childs[0];
   SR.Left^.childs[1]:=SI^.Left;
   PN^.Keys[0]  :=PN^.Keys[1];
   PN^.childs[0]:=PN^.childs[1];
   PN^.childs[1]:=PN^.childs[2];
  end else
  //         ->       1K
  // 0 1 N   ->   0K      NK
  //0 1 N 2  -> 0N  1N  NN  2N
  begin
   SR.Key :=PN^.Keys[1];
   SR.Left^.Keys[0]  :=PN^.Keys[0];
   SR.Left^.childs[0]:=PN^.childs[0];
   SR.Left^.childs[1]:=PN^.childs[1];
   PN^.Keys[0]  :=SI^.Key;
   PN^.childs[0]:=SI^.Left;
   PN^.childs[1]:=SI^.Right;
   PN^.childs[0]^.parent:=PN;
  end;
  PN^.Keys[1]  :=Default(TKey);
  PN^.childs[2]:=nil;
  SR.Left^.childs[0]^.parent:=SR.Left;
  SR.Left^.childs[1]^.parent:=SR.Left;
  SI^:=SR;
 end;
end;

procedure T23treeSet.TStuff.Free_node(P:Pointer); inline;
begin
 FreeMem(P);
end;

Procedure T23treeSet.Create; inline;
begin
 pRoot:=nil;
 FSize:=0;
end;

Procedure T23treeSet.Free;
Var
 T,P:P23Node;
begin
 T:=pRoot;
 if T<>nil then
 While (T<>nil) do //up cycle
 begin

  repeat //down cycle
   if (T^.childs[0]=nil) then  //is leaf
   begin
    T^.Keys[0]:=Default(TKey);
    T^.Keys[1]:=Default(TKey);
    Break;
   end else
   if (T^.childs[1]=nil) then //zero node?
   begin
    T:=T^.childs[0];
   end else
   if (T^.childs[2]=nil) then //1
   begin
    T:=T^.childs[1];
   end else
   begin //2
    T:=T^.childs[2];
   end;
  until false;

  P:=T;
  T:=T^.parent;
  TStuff.Free_node(P);

  if Assigned(T) then
  begin
   if (T^.childs[2]=P) then
   begin
    T^.childs[2]:=nil;
   end else
   if (T^.childs[1]=P) then
   begin
    T^.childs[1]:=nil;
   end else
   begin
    T^.childs[0]:=nil;
   end;
  end;

 end;

 pRoot:=nil;
 FSize:=0;
end;

Function T23treeSet.TStuff._search_min(PN:P23Node):p_Iterator; inline;
Var
 T:P23Node;
begin
 Result:=Default(p_Iterator);
 T:=PN;
 While (T<>nil) do
 begin
  Result.N:=T;
  T:=T^.childs[0];
 end;
end;

Function T23treeSet.TStuff._search_max(PN:P23Node):p_Iterator; inline;
Var
 T:P23Node;
begin
 Result:=Default(p_Iterator);
 T:=PN;
 While (T<>nil) do
 begin
  Result.N:=T;
  if (T^.childs[0]=nil) or (T^.childs[1]=nil) then  //is leaf or zero node?
  begin
   if (T^.childs[2]<>nil) then Result.P:=1;
   Exit;
  end else
  if (T^.childs[2]=nil) then //1
  begin
   T:=T^.childs[1];
  end else
  begin //2
   T:=T^.childs[2];
  end;
 end;
end;

function T23treeSet.TStuff._find_leaf(PN:P23Node;Const K:TKey):P23Node;
Var
 i:Integer;
 T:P23Node;
begin
 Result:=nil;
 T:=PN;
 While (T<>nil) do
 begin
  Result:=T;
  if (T^.childs[0]=nil) or (T^.childs[1]=nil) then  //is leaf or zero node?
  begin
   Exit;
  end else
  begin
   i:=TCompare.c(T^.Keys[0],K);
   if (i=0) then Exit(nil);
   if (i>0) then
   begin
    T:=T^.childs[0];
   end else
   if (T^.childs[2]=nil) then //1
   begin
    T:=T^.childs[1];
   end else //2
   begin
    i:=TCompare.c(T^.Keys[1],K);
    if (i=0) then Exit(nil);
    if (i>0) then
    begin
     T:=T^.childs[1];
    end else
    begin
     T:=T^.childs[2];
    end;
   end;
  end;
 end;
end;

function T23treeSet.TStuff._find_node(PN:P23Node;Const K:TKey):p_Iterator;
Var
 i:Integer;
 T:P23Node;
begin
 Result:=Default(p_Iterator);
 T:=PN;
 While (T<>nil) do
 begin
  Result.N:=T;
  if (T^.childs[1]=nil) then Exit; //zero node or leaf?
  i:=TCompare.c(T^.Keys[0],K);
  if (i=0) then
  begin
   Result.P:=0;
   Exit;
  end;
  if (T^.childs[0]=nil) then  //is leaf
  begin
   if (i>0) then
   begin
    Result.P:=-1; //less first key
   end else
   if (T^.childs[2]=nil) then //len=1
   begin
    Result.P:=-2; //big first key
   end else
   begin //len=2 //big first key
    i:=TCompare.c(T^.Keys[1],K);
    if (i=0) then
    begin
     Result.P:=1;
    end else
    if (i>0) then
    begin
     Result.P:=-3; //less two key
    end else
    begin
     Result.P:=-4; //big two key
    end;
   end;
   Exit;
  end else //not leaf
  if (i>0) then
  begin
   T:=T^.childs[0];
  end else
  if (T^.childs[2]=nil) then //1
  begin
   T:=T^.childs[1];
  end else
  begin //2
   i:=TCompare.c(T^.Keys[1],K);
   if (i=0) then
   begin
    Result.P:=1;
    Exit;
   end else
   if (i>0) then
   begin
    T:=T^.childs[1];
   end else
   begin
    T:=T^.childs[2];
   end;
  end;
 end;
end;

procedure T23treeSet._update(PN:P23Node;SI:PSplitInfo); inline;
begin
 repeat
  PN:=PN^.parent;
  if (PN=nil) then
  begin
   pRoot:=TStuff.new_Node;
   TStuff._insert2node(pRoot,SI);
   Exit;
  end;
 until TStuff._insert2node(PN,SI);
end;

function T23treeSet.Insert(Const K:TKey):Boolean;
Var
 PN:P23Node;
 SI:TSplitInfo;
begin
 Result:=False;
 SI:=Default(TSplitInfo);

 if (pRoot=nil) then
 begin
  pRoot:=TStuff.new_Node;
  TStuff._insert2Leaf(pRoot,K,@SI);
  Inc(FSize);
  Exit(true);
 end;

 PN:=TStuff._find_leaf(pRoot,K);
 if PN=nil then Exit;

 Case TStuff._insert2Leaf(PN,K,@SI) of
  1:Exit;
  2:_update(PN,@SI);
 end;

 Inc(FSize);
 Result:=True;
end;

Function T23treeSet.Contains(Const K:TKey):Boolean; inline;
begin
 Result:=Iterator(TStuff._find_node(pRoot,K)).Item<>nil;
end;

procedure T23treeSet.TStuff._remove4leaf(PN:P23Node;first:Boolean); inline;
begin
 if (PN^.childs[1]<>nil) then
  if (PN^.childs[2]=nil) then
  begin //1
   PN^.Keys[0]  :=Default(TKey);
   PN^.childs[1]:=nil;
  end else
  begin //2
   if first then
   begin
    PN^.Keys[0] :=PN^.Keys[1];
   end;
   PN^.Keys[1]  :=Default(TKey);
   PN^.childs[2]:=nil;
  end;
end;

Function T23treeSet.erase(Const it:Iterator):Boolean;
Var
 PN,min:P23Node;
begin
 Result:=(it.Item<>nil);
 if Result then
 begin
  PN:=it.N;
  if (PN^.childs[0]=nil) then  //is leaf
  begin
   TStuff._remove4leaf(PN,(it.P=0));
  end else
  if (it.P=0) then
  begin
   min:=TStuff._search_min(PN^.childs[1]).N;
   PN^.Keys[0]:=min^.Keys[0];
   PN:=min;
   TStuff._remove4leaf(PN,true);
  end else
  begin
   min:=TStuff._search_min(PN^.childs[2]).N;
   PN^.Keys[1]:=min^.Keys[0];
   PN:=min;
   TStuff._remove4leaf(PN,true);
  end;

  _fix(PN);

  Dec(FSize);
 end;
end;

Function T23treeSet.delete(Const R:TKey):Boolean;
begin
 Result:=erase(Iterator(TStuff._find_node(pRoot,R)));
end;

Function T23treeSet.find(Const R:TKey):Iterator;
begin
 Result:=Iterator(TStuff._find_node(pRoot,R));
end;

// -1; //less first key
// -2; //big first key
// -3; //less two key
// -4; //big two key

Function T23treeSet.find_be(Const R:TKey):Iterator;
begin
 Result:=Iterator(TStuff._find_node(pRoot,R));
 Case Result.P of
  -1:Result.P:=0;
  -2:begin
      Result.P:=0;
      if not Result.Next then Result:=Default(Iterator);
     end;
  -3:Result.P:=1;
  -4:begin
      Result.P:=1;
      if not Result.Next then Result:=Default(Iterator);
     end;
 end;
end;

Function T23treeSet.find_le(Const R:TKey):Iterator;
begin
 Result:=Iterator(TStuff._find_node(pRoot,R));
 Case Result.P of
  -1:begin
      Result.P:=0;
      if not Result.Prev then Result:=Default(Iterator);
     end;
  -2:Result.P:=0;
  -3:Result.P:=0;
  -4:Result.P:=1;
 end;
end;

function T23treeSet.TStuff._remove4parent(var PN:P23Node;SI:PSplitInfo):Boolean; inline;
Var
 PR:P23Node;

 function _change_parent(O,N:P23Node):Boolean; inline;
 Var
  P:P23Node;
 begin
  P:=O^.parent;
  N^.parent:=P;
  Result:=P<>nil;
  if Result then
  begin
   if P^.childs[0]=O then
   begin
    P^.childs[0]:=N;
   end else
   if P^.childs[1]=O then
   begin
    P^.childs[1]:=N;
   end else
   begin
    P^.childs[2]:=N;
   end;
  end;
 end;

begin
 Result:=True;
 PR:=PN^.parent;
 if (PR^.childs[2]=nil) then //1
 begin
  if PR^.childs[0]=PN then
  begin
   //   1
   // / |
   //D  2
   SI^.Key:=PR^.Keys[0];
   TStuff.Free_node(PN);
   PN:=PR^.childs[1];
   Result:=_change_parent(PR,PN);
   PR^:=Default(T23Node);
   SI^._new:=PR;
  end else
  begin
   //   1
   // / |
   //0  D
   SI^.Key:=PR^.Keys[0];
   TStuff.Free_node(PN);
   PN:=PR^.childs[0];
   Result:=_change_parent(PR,PN);
   PR^:=Default(T23Node);
   SI^._new:=PR;
  end;
 end else //2
 if PR^.childs[0]=PN then
 begin
  //   1 3
  //  / | \
  // D  2  4
  SI^.Key:=PR^.Keys[0];
  PR^.Keys[0]:=PR^.Keys[1];
  PR^.Keys[1]:=Default(TKey);
  PN^:=Default(T23Node);
  SI^._new:=PN;
  PR^.childs[0]:=PR^.childs[1];
  PR^.childs[1]:=PR^.childs[2];
  PR^.childs[2]:=nil;
  PN:=PR;
 end else
 if PR^.childs[1]=PN then
 begin
  //   0 1
  //  / | \
  // 0  D  2
  SI^.Key:=PR^.Keys[1];
  PR^.Keys[1]:=Default(TKey);
  PN^:=Default(T23Node);
  SI^._new:=PN;
  PR^.childs[1]:=PR^.childs[2];
  PR^.childs[2]:=nil;
  PN:=PR;
 end else
 begin
  //   0 1
  //  / | \
  // 0  1  D
  SI^.Key:=PR^.Keys[1];
  PR^.Keys[1]:=Default(TKey);
  PN^:=Default(T23Node);
  SI^._new:=PN;
  PR^.childs[2]:=nil;
  PN:=PR;
 end;
end;

procedure T23treeSet._fix(PN:P23Node);
Var
 SI:TSplitInfo;
begin
 if (PN^.childs[1]=nil) and (PN^.parent=nil) then
 begin
  Assert(PN=pRoot);
  TStuff.Free_node(PN);
  pRoot:=nil;
  Exit;
 end else
 if (PN^.childs[1]=nil) then //0
 begin
  SI:=Default(TSplitInfo);

  if not TStuff._remove4parent(PN,@SI) then
  begin
   pRoot:=PN;
  end;

  PN:=TStuff._find_leaf(PN,SI.Key);

  if PN<>nil then
   if TStuff._insert2Leaf(PN,SI.Key,@SI)=2 then
   begin
    _update(PN,@SI);
   end;

  if SI._new<>nil then
   TStuff.Free_node(SI._new);

 end;
end;

function T23treeSet.cbegin:Iterator;
begin
 Result:=Iterator(TStuff._search_min(pRoot));
end;

function T23treeSet.cend:Iterator;
begin
 Result:=Iterator(TStuff._search_max(pRoot));
 Inc(Result.P);
end;

Function  T23treeSet.Iterator.Next:Boolean;
Var
 T:P23Node;

 function _next_leaf:Boolean; inline;
 begin
  Result:=False;
  if (N^.childs[1]<>nil) and (N^.childs[2]<>nil) then
  begin //2
   case P of
    0:begin
       Result:=True;
       Inc(P);
      end;
    1:Inc(P);
   end;
  end;
 end;

 procedure _up; inline;
 begin
  N:=T;
  T:=N^.parent;
 end;

begin
 T:=nil;
 Result:=False;
 if (N<>nil) then
 begin
  if (N^.childs[0]=nil) then //is leaf
  begin
   Result:=_next_leaf;
   if not Result then //up
   begin
    T:=N^.parent;
    While (T<>nil) do
    begin
     if T^.childs[0]=N then //0L->0K
     begin
      if T^.childs[1]=nil then
      begin
       _up;
      end else
      begin
       N:=T;
       P:=0;
       Exit(True);
      end;
     end else
     if T^.childs[1]=N then //1L->1K
     begin
      if T^.childs[2]=nil then
      begin
       _up;
      end else
      begin
       N:=T;
       P:=1;
       Exit(True);
      end;
     end else //2->up
     begin
      _up;
     end;
    end;
    if T=nil then
    begin
     if N^.childs[2]=nil then
      P:=1
     else
      P:=2;
    end;
   end;
  end else
  begin
   if (N^.childs[1]<>nil) then //<>0
   begin
    Result:=True;
    if (N^.childs[2]=nil) then //=1
    begin //0K->1L
     Self:=Iterator(TStuff._search_min(N^.childs[1]));
    end else
    begin //2
     case P of
      0:begin //0K->1L
         Self:=Iterator(TStuff._search_min(N^.childs[1]));
        end;
      1:begin //1K->2L
         Self:=Iterator(TStuff._search_min(N^.childs[2]));
        end;
      else
        Exit(False);
     end;
    end;
   end;
  end;
 end;
end;

Function  T23treeSet.Iterator.Prev:Boolean;
Var
 T:P23Node;

 function _prev_leaf:Boolean; inline;
 begin
  Result:=False;
  if (N^.childs[1]<>nil) then
   if (N^.childs[2]=nil) then //=1
   begin
    case P of
     1:begin
        Result:=True;
        Dec(P);
       end;
    end;
   end else //=2
   begin
    case P of
     1,
     2:begin
        Result:=True;
        Dec(P);
       end;
    end;
   end;
 end;

 procedure _up; inline;
 begin
  N:=T;
  T:=N^.parent;
 end;

begin
 Result:=False;
 T:=nil;
 if (N<>nil) then
 begin
  if (N^.childs[0]=nil) then //is leaf
  begin
   Result:=_prev_leaf;
   if not Result then //up
   begin
    T:=N^.parent;
    While (T<>nil) do
    begin
     if T^.childs[0]=N then //0L->up
     begin
      _up;
     end else
     if T^.childs[1]=N then //1L->0K
     begin
      N:=T;
      P:=0;
      Exit(True);
     end else //2L->1K
     begin
      N:=T;
      P:=1;
      Exit(True);
     end;
    end;
    if T=nil then
    begin
     P:=0;
    end;
   end;
  end else
  begin
   if (N^.childs[1]<>nil) then //<>0
   begin
    Result:=True;
    if (N^.childs[2]=nil) then //=1
    begin //0K->0L
     Self:=Iterator(TStuff._search_max(N^.childs[0]));
    end else
    begin //2
     case P of
      0:begin //0K->0L
         Self:=Iterator(TStuff._search_max(N^.childs[0]));
        end;
      1:begin //1K->1L
         Self:=Iterator(TStuff._search_max(N^.childs[1]));
        end;
      else
        Exit(False);
     end;
    end;
   end;
  end;
 end;
end;

Procedure T23treeSet.Iterator.Free; inline;
begin
 Self:=Default(Iterator);
end;

Function  T23treeSet.Iterator.Item:PKey;
begin
 Result:=nil;
 if (N<>nil) and (N^.childs[1]<>nil) then //<>0
  if (N^.childs[2]=nil) then //=1
  begin
   if (P=0) then
   begin
    Result:=@N^.Keys[0];
   end;
  end else //=2
  if (P>=0) and (P<2) then
  begin
   Result:=@N^.Keys[P];
  end;
end;

function T23treeSet.Iterator.keyCount:Byte;
begin
 if (N=nil) then
  Result:=0
 else
 if (N^.childs[1]=nil) then
  Result:=0
 else
 if (N^.childs[2]=nil) then
  Result:=1
 else
  Result:=2;
end;

end.

