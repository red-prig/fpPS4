{ Implimentation of Dmitry Vyukov Bounded MPMC queue on free pascal

  Copyright (C) 2022 Red_prig

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

unit mpmc_queue;

{$mode objfpc}{$H+}

interface

Const
 cacheline_size=64;

Type
 cacheline_pad_t=array[0..cacheline_size-1] of Byte;

 generic mpmc_bounded_queue<TItem>=object
  type
   p_cell_t=^cell_t;
   cell_t=record
    sequence_:PtrUInt;
    data_:TItem;
   end;
  var
   pad0_:cacheline_pad_t;
   buffer_:p_cell_t;
   buffer_mask_:PtrUInt;
   pad1_:cacheline_pad_t;
   enqueue_pos_:PtrUInt;
   pad2_:cacheline_pad_t;
   dequeue_pos_:PtrUInt;
   pad3_:cacheline_pad_t;
  Procedure Create(buffer_size:PtrUInt);
  Procedure Free;
  function  enqueue(var data:TItem):Boolean;
  function  dequeue(var data:TItem):Boolean;
 end;

function  load_consume(Var addr:PtrUInt):PtrUInt; inline;
function  load_acquire(Var addr:PtrUInt):PtrUInt; inline;
Procedure store_release(Var addr:PtrUInt;v:PtrUInt); inline;
function  CAS(Var addr:PtrUInt;Comp,New:PtrUInt):Boolean; inline;

implementation

function load_consume(Var addr:PtrUInt):PtrUInt; inline;
begin
 ReadDependencyBarrier;
 Result:=addr;
end;

function load_acquire(Var addr:PtrUInt):PtrUInt; inline;
begin
 ReadBarrier;
 Result:=addr;
end;

Procedure store_release(Var addr:PtrUInt;v:PtrUInt); inline;
begin
 WriteBarrier;
 addr:=v;
end;

function CAS(Var addr:PtrUInt;Comp,New:PtrUInt):Boolean; inline;
begin
 Result:=system.InterlockedCompareExchange(Pointer(addr),Pointer(New),Pointer(Comp))=Pointer(Comp);
end;

//

Procedure mpmc_bounded_queue.Create(buffer_size:PtrUInt);
var
 i:PtrUInt;
begin
 Assert((buffer_size >= 2) and ((buffer_size and (buffer_size-1))=0));

 buffer_:=AllocMem(buffer_size*SizeOf(cell_t));
 buffer_mask_:=buffer_size-1;

 For i:=0 to buffer_mask_ do
 begin
  buffer_[i].sequence_:=i;
 end;

 enqueue_pos_:=0;
 dequeue_pos_:=0;
end;

Procedure mpmc_bounded_queue.Free;
begin
 FreeMem(buffer_);
end;

function mpmc_bounded_queue.enqueue(var data:TItem):Boolean;
var
 cell:p_cell_t;
 pos:PtrUInt;
 seq:PtrUInt;
 dif:PtrInt;
begin
 pos:=load_consume(enqueue_pos_);

 repeat
  cell:=@buffer_[pos and buffer_mask_];

  seq:=load_acquire(cell^.sequence_);

  dif:=PtrInt(seq)-PtrInt(pos);

  if (dif=0) then
  begin
   if CAS(enqueue_pos_,pos,pos+1) then
   begin
    Break;
   end;
  end else
  if (dif<0) then
  begin
   Exit(False);
  end else
  begin
   pos:=load_consume(enqueue_pos_);
  end;

 until false;

 cell^.data_:=data;

 store_release(cell^.sequence_,pos+1);

 Result:=True;
end;

function mpmc_bounded_queue.dequeue(var data:TItem):Boolean;
var
 cell:p_cell_t;
 pos:PtrUInt;
 seq:PtrUInt;
 dif:PtrInt;
begin
 pos:=load_consume(dequeue_pos_);

 repeat
  cell:=@buffer_[pos and buffer_mask_];

  seq:=load_acquire(cell^.sequence_);

  dif:=PtrInt(seq)-PtrInt(pos+1);

  if (dif=0) then
  begin
   if CAS(dequeue_pos_,pos,pos+1) then
   begin
    Break;
   end;
  end else
  if (dif<0) then
  begin
   Exit(False);
  end else
  begin
   pos:=load_consume(dequeue_pos_);
  end;

 until false;

 data:=cell^.data_;

 store_release(cell^.sequence_,pos+buffer_mask_+1);

 Result:=True;
end;

end.




