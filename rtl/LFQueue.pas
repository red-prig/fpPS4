{ Implimentation of Dmitry Vyukov Intrusive MPSC node-based queue on free pascal

  Copyright (C) 2018-2020 Red_prig

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

unit LFQueue;

{$mode objfpc}{$H+}

interface

Const
 CacheLineSize=64;

Type
 TIntrusiveMPSCQueue=object
  protected
   type
    PQNode=^TQNode;
    TQNode=record
     next_:PQNode;
     //some data
    end;
   Var
    tail_:PQNode;
    stub_:TQNode;
    head_:PQNode;
  public
   Procedure Create;
   Function  Push(Node:Pointer):Boolean;
   Function  Pop(Var Node:Pointer):Boolean;
   Function  IsEmpty:Boolean; inline;
 end;

 TIntrusiveMPSCQueueA=object
  protected
   type
    PQNode=^TQNode;
    TQNode=record
     next_:PQNode;
     //some data
    end;
   Var
    tail_:record
     Case Byte of
      0:(Pad:Array[0..CacheLineSize-1] of Byte);
      1:(VAL:PQNode;stub:TQNode);
    end;
    head_:record
     Case Byte of
      0:(Pad:Array[0..CacheLineSize-1] of Byte);
      1:(VAL:PQNode);
    end;
  public
   Procedure Create;
   Function  Push(Node:Pointer):Boolean;
   Function  Pop(Var Node:Pointer):Boolean;
   Function  IsEmpty:Boolean; inline;
 end;

 generic TLFQueue<TItem,Allocator,back_off>=object(TIntrusiveMPSCQueue)
  type
   PNode=^TNode;
   TNode=record
    next_:PNode;
    Item:TItem;
   end;
  Function push_front(Const val:TItem):Boolean;
  Function pop_back(Var val:TItem):Boolean;
 end;

 generic TLFQueueA<TItem,Allocator,back_off>=object(TIntrusiveMPSCQueueA)
  type
   PNode=^TNode;
   TNode=record
    next_:PNode;
    Item:TItem;
   end;
  Function push_front(Const val:TItem):Boolean;
  Function pop_back(Var val:TItem):Boolean;
 end;

function  load_consume(Var addr:Pointer):Pointer; inline;
function  load_consume(Var addr:PtrUInt):PtrUInt; inline;
Procedure store_release(Var addr:Pointer;v:Pointer); inline;
function  XCHG(Var addr:Pointer;New:Pointer):Pointer; inline;

implementation

function load_consume(Var addr:Pointer):Pointer; inline;
begin
 ReadDependencyBarrier;
 Result:=addr;
end;

function load_consume(Var addr:PtrUInt):PtrUInt; inline;
begin
 ReadDependencyBarrier;
 Result:=addr;
end;

Procedure store_release(Var addr:Pointer;v:Pointer); inline;
begin
 WriteBarrier;
 addr:=v;
end;

function XCHG(Var addr:Pointer;New:Pointer):Pointer; inline;
begin
 Result:=System.InterLockedExchange(addr,New);
end;

//

Procedure TIntrusiveMPSCQueue.Create;
begin
 FillChar(Self,SizeOf(Self),0);
 head_:=@stub_;
 tail_:=@stub_;
 ReadWriteBarrier;
end;

Function TIntrusiveMPSCQueue.Push(Node:Pointer):Boolean;
Var
 prev:PQNode;
begin
 if not Assigned(Node) then Exit(False);
 store_release(PQNode(Node)^.next_,nil);
 prev:=XCHG(head_,Node);
 store_release(prev^.next_,Node);
 Result:=True;
end;

Function TIntrusiveMPSCQueue.Pop(Var Node:Pointer):Boolean;
Var
 tail,n,head:PQNode;
begin
 Node:=nil;
 Result:=False;

 tail:=tail_;
 n:=load_consume(tail^.next_);

 if tail=@stub_ then
 begin
  if n=nil then Exit;
  store_release(tail_,n);
  tail:=n;
  n:=load_consume(n^.next_);
 end;

 if n<>nil then
 begin
  store_release(tail_,n);
  Node:=tail;
  store_release(tail^.next_,nil);
  Exit(True);
 end;

 head:=head_;
 if tail<>head then Exit;

 stub_.next_:=nil;
 n:=XCHG(head_,@stub_);
 store_release(n^.next_,@stub_);

 n:=load_consume(tail^.next_);

 if n<>nil then
 begin
  store_release(tail_,n);
  Node:=tail;
  store_release(tail^.next_,nil);
  Exit(True);
 end;

end;

Function TIntrusiveMPSCQueue.IsEmpty:Boolean; inline;
begin
 Result:=head_=@stub_;
end;

//

Procedure TIntrusiveMPSCQueueA.Create;
begin
 FillChar(Self,SizeOf(Self),0);
 head_.VAL:=@tail_.stub;
 tail_.VAL:=@tail_.stub;
 ReadWriteBarrier;
end;

Function TIntrusiveMPSCQueueA.Push(Node:Pointer):Boolean;
Var
 prev:PQNode;
begin
 if not Assigned(Node) then Exit(False);
 store_release(PQNode(Node)^.next_,nil);
 prev:=XCHG(head_.VAL,Node);
 store_release(prev^.next_,Node);
 Result:=True;
end;

Function TIntrusiveMPSCQueueA.Pop(Var Node:Pointer):Boolean;
Var
 tail,n,head:PQNode;
begin
 Node:=nil;
 Result:=False;

 tail:=tail_.VAL;
 n:=load_consume(tail^.next_);

 if tail=@tail_.stub then
 begin
  if n=nil then Exit;
  store_release(tail_.VAL,n);
  tail:=n;
  n:=load_consume(n^.next_);
 end;

 if n<>nil then
 begin
  store_release(tail_.VAL,n);
  Node:=tail;
  store_release(tail^.next_,nil);
  Exit(True);
 end;

 head:=head_.VAL;
 if tail<>head then Exit;

 tail_.stub.next_:=nil;
 n:=XCHG(head_.VAL,@tail_.stub);
 store_release(n^.next_,@tail_.stub);

 n:=load_consume(tail^.next_);

 if n<>nil then
 begin
  store_release(tail_.VAL,n);
  Node:=tail;
  store_release(tail^.next_,nil);
  Exit(True);
 end;

end;

Function TIntrusiveMPSCQueueA.IsEmpty:Boolean; inline;
begin
 Result:=head_.VAL=@tail_.stub;
end;

//

Function TLFQueue.push_front(Const val:TItem):Boolean;
Var
 Node:PNode;
begin
 Node:=Allocator.AllocMem(SizeOf(TNode));
 Result:=Push(Node);
end;

Function TLFQueue.pop_back(Var val:TItem):Boolean;
Var
 tail,n,head:PQNode;
 bkoff:back_off;
begin
 Result:=False;

 bkoff.Reset;
 repeat
  tail:=XCHG(tail_,nil);

  if (tail<>nil) then
  begin
   Break;
  end else
  begin
   bkoff.Wait;
  end;

 until false;

 n:=load_consume(tail^.next_);

 if tail=@stub_ then
 begin
  if n=nil then
  begin

   if tail=nil then tail:=@stub_;
   store_release(tail_,tail); //unlock

   Exit;
  end;
  tail:=n;
  n:=load_consume(n^.next_);
 end;

 if n<>nil then
 begin

  val:=PNode(tail)^.Item;
  FreeMem(tail);

  if n=nil then n:=@stub_;
  store_release(tail_,n); //unlock

  Exit(True);
 end;

 head:=head_;
 if tail<>head then
 begin

  if tail=nil then tail:=@stub_;
  store_release(tail_,tail); //unlock

  Exit;
 end;

 stub_.next_:=nil;
 n:=XCHG(head_,@stub_);
 store_release(n^.next_,@stub_);

 n:=load_consume(tail^.next_);

 if n<>nil then
 begin

  val:=PNode(tail)^.Item;
  FreeMem(tail);

  if n=nil then n:=@stub_;
  store_release(tail_,n); //unlock

  Exit(True);
 end;

 if tail=nil then tail:=@stub_;
 store_release(tail_,tail); //unlock
end;

//

Function TLFQueueA.push_front(Const val:TItem):Boolean;
Var
 Node:PNode;
begin
 Node:=Allocator.AllocMem(SizeOf(TNode));
 Result:=Push(Node);
end;

Function TLFQueueA.pop_back(Var val:TItem):Boolean;
Var
 tail,n,head:PQNode;
 bkoff:back_off;
begin
 Result:=False;

 bkoff.Reset;
 repeat
  tail:=XCHG(tail_.VAL,nil);

  if (tail<>nil) then
  begin
   Break;
  end else
  begin
   bkoff.Wait;
  end;

 until false;

 n:=load_consume(tail^.next_);

 if tail=@tail_.stub then
 begin
  if n=nil then
  begin

   if tail=nil then tail:=@tail_.stub;
   store_release(tail_.VAL,tail); //unlock

   Exit;
  end;
  tail:=n;
  n:=load_consume(n^.next_);
 end;

 if n<>nil then
 begin

  val:=PNode(tail)^.Item;
  FreeMem(tail);

  if n=nil then n:=@tail_.stub;
  store_release(tail_.VAL,n); //unlock

  Exit(True);
 end;

 head:=head_.VAL;
 if tail<>head then
 begin

  if tail=nil then tail:=@tail_.stub;
  store_release(tail_.VAL,tail); //unlock

  Exit;
 end;

 tail_.stub.next_:=nil;
 n:=XCHG(head_.VAL,@tail_.stub);
 store_release(n^.next_,@tail_.stub);

 n:=load_consume(tail^.next_);

 if n<>nil then
 begin

  val:=PNode(tail)^.Item;
  FreeMem(tail);

  if n=nil then n:=@tail_.stub;
  store_release(tail_.VAL,n); //unlock

  Exit(True);
 end;

 if tail=nil then tail:=@tail_.stub;
 store_release(tail_.VAL,tail); //unlock
end;

end.



