{ IO event buffer

  Copyright (C) 2018-2023 Red_prig

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

unit evbuffer;

{$mode ObjFPC}{$H+}

interface

uses
 atomic;

type
 TfuncFree=Function(p:pointer):SizeUInt; register;

 Peviovec=^Teviovec;
 Teviovec=object
  private
   next_:Peviovec;
  public
   base:Pointer;
   len :SizeUInt;
   pos :SizeUInt;
   buf_free:TfuncFree;
   vec_free:TfuncFree;
 end;

 Pevbuffer=^Tevbuffer;
 Tevbuffer=object
  private
   Var
    len :SizeUInt;
    tail_,head_:Peviovec;
    stub_:Pointer;
  public
 end;

Procedure eviovec_free(P:Peviovec);
function  eviovec_next(buf:Pevbuffer;vec:Peviovec):Peviovec;
function  eviovec_getdata(vec:Peviovec):Pointer;
function  eviovec_getlen(vec:Peviovec):SizeUInt;

procedure evbuffer_init(buf:Pevbuffer);
function  evbuffer_new:Pevbuffer;
procedure evbuffer_free(buf:Pevbuffer);
procedure evbuffer_clear(buf:Pevbuffer);
Function  evbuffer_isempty(buf:Pevbuffer):Boolean;

function  evbuffer_push(buf:Pevbuffer;Node:Peviovec):Boolean;
function  evbuffer_pop(buf:Pevbuffer):Peviovec;
function  evbuffer_peek(buf:Pevbuffer):Peviovec;
function  evbuffer_add_ref(buf:Pevbuffer;data:pointer;datapos,datalen:SizeUInt;ff:TfuncFree):Boolean;
function  evbuffer_remove_ref(buf:Pevbuffer;var data:pointer;var datapos,datalen:SizeUInt;var ff:TfuncFree):Boolean;
function  evbuffer_add(buf:Pevbuffer;data:pointer;datalen:SizeUInt):Boolean;
function  evbuffer_remove(buf:Pevbuffer;data:pointer;datalen:SizeUInt):SizeUInt;
function  evbuffer_copy(buf:Pevbuffer;data:pointer;datalen:SizeUInt):SizeUInt;
function  evbuffer_drain(buf:Pevbuffer;datalen:SizeUInt):SizeUInt;
function  evbuffer_get_length(buf:Pevbuffer):SizeUInt;
function  evbuffer_get_contiguous_space(buf:Pevbuffer):SizeUInt;
function  evbuffer_get_atmost_size(buf:Pevbuffer;size:SizeUint):SizeUInt;
function  evbuffer_get_atless_size(buf:Pevbuffer;size:SizeUint):SizeUInt;
function  evbuffer_move(Src,Dst:Pevbuffer):SizeUInt;
function  evbuffer_move_length(Src,Dst:Pevbuffer;length:SizeUInt):SizeUInt;

function Freemem_ptr:TfuncFree;

implementation

function Freemem_ptr:TfuncFree;
Var
 MemMgr:TMemoryManager;
begin
 MemMgr:=Default(TMemoryManager);
 GetMemoryManager(MemMgr);
 Result:=MemMgr.Freemem;
end;

//

Procedure eviovec_free(P:Peviovec);
begin
 if not Assigned(P) then Exit;
 if Assigned(P^.buf_free) then
 begin
  P^.buf_free(P^.base);
 end;
 if Assigned(P^.vec_free) then
 begin
  P^.vec_free(P);
 end;
end;

function eviovec_next(buf:Pevbuffer;vec:Peviovec):Peviovec;
Var
 tail,n:Peviovec;
begin
 Result:=nil;
 if (not Assigned(buf)) or
    (not Assigned(vec)) then Exit;
 With vec^ do
 begin
  tail:=vec^.next_;
  if not Assigned(tail) then Exit;
  n:=load_consume(tail^.next_);
  if tail=@buf^.stub_ then
  begin
   if n=nil then Exit;
   tail:=n;
  end;
  Result:=tail;
 end;
end;

function eviovec_getdata(vec:Peviovec):Pointer;
begin
 Result:=nil;
 if not Assigned(vec) then Exit;
 Result:=@PByte(vec^.base)[vec^.pos];
end;

function eviovec_getlen(vec:Peviovec):SizeUInt;
begin
 Result:=0;
 if not Assigned(vec) then Exit;
 Result:=vec^.len;
end;

//--evbuffer--

procedure evbuffer_init(buf:Pevbuffer);
begin
 if not Assigned(buf) then Exit;
 buf^:=Default(Tevbuffer);
 With buf^ do
 begin
  head_:=Peviovec(@stub_);
  tail_:=Peviovec(@stub_);
 end;
 ReadWriteBarrier;
end;

function evbuffer_new:Pevbuffer;
begin
 Result:=GetMem(SizeOf(Tevbuffer));
 evbuffer_init(Result);
end;

procedure evbuffer_free(buf:Pevbuffer);
begin
 if not Assigned(buf) then Exit;
 evbuffer_clear(buf);
 FreeMem(buf);
end;

procedure evbuffer_clear(buf:Pevbuffer);
Var
 Node:Peviovec;
begin
 if not Assigned(buf) then Exit;
 repeat
  Node:=evbuffer_pop(buf);
  eviovec_free(Node);
 until (Node=nil);
end;

Function evbuffer_isempty(buf:Pevbuffer):Boolean;
begin
 if not Assigned(buf) then Exit(true);
 Result:=(load_acquire(buf^.head_)=@buf^.stub_);
end;

function evbuffer_push(buf:Pevbuffer;Node:Peviovec):Boolean;
Var
 prev:Peviovec;
begin
 if (not Assigned(buf)) or (not Assigned(Node)) then Exit(False);
 With buf^ do
 begin
  store_release(Node^.next_,nil);
  prev:=XCHG(head_,Node);
  store_release(prev^.next_,Node);
  fetch_add(len,Node^.len);
 end;
 Result:=True;
end;

function evbuffer_pop(buf:Pevbuffer):Peviovec;
Var
 tail,n,head:Peviovec;
begin
 Result:=nil;
 if not Assigned(buf) then Exit;
 With buf^ do
 begin
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
   Result:=tail;
   store_release(tail^.next_,nil);
   fetch_sub(len,Result^.len);
   Exit;
  end;

  head:=head_;
  if tail<>head then Exit;

  stub_:=nil;
  n:=XCHG(head_,@stub_);
  store_release(n^.next_,@stub_);

  n:=load_consume(tail^.next_);

  if n<>nil then
  begin
   store_release(tail_,n);
   Result:=tail;
   store_release(tail^.next_,nil);
   fetch_sub(len,Result^.len);
   Exit;
  end;
 end;
end;

function evbuffer_peek(buf:Pevbuffer):Peviovec;
Var
 tail,n:Peviovec;
begin
 Result:=nil;
 if not Assigned(buf) then Exit;
 With buf^ do
 begin
  tail:=tail_;
  if not Assigned(tail) then Exit;
  n:=load_consume(tail^.next_);
  if tail=@stub_ then
  begin
   if not Assigned(n) then Exit;
   tail:=n;
  end;
  Result:=tail;
 end;
end;

function evbuffer_get_atmost_size(buf:Pevbuffer;size:SizeUint):SizeUInt;
Var
 vec:Peviovec;
begin
 Result:=0;
 if size=0 then Exit;
 vec:=evbuffer_peek(buf);
 if not Assigned(vec) then Exit;
 Result:=vec^.len;
 if Result>=size then
 begin
  Result:=size;
 end else
 begin
  repeat
   vec:=eviovec_next(buf,vec);
   if not Assigned(vec) then Break;
   if Result+vec^.len>size then Break;
   Result:=Result+vec^.len;
  until false;
 end;
end;

function evbuffer_get_atless_size(buf:Pevbuffer;size:SizeUint):SizeUInt;
Var
 vec:Peviovec;
begin
 Result:=0;
 if size=0 then Exit;
 vec:=evbuffer_peek(buf);
 if not Assigned(vec) then Exit;
 Result:=vec^.len;
 if Result<size then
 begin
  repeat
   vec:=eviovec_next(buf,vec);
   if not Assigned(vec) then Break;
   Result:=Result+vec^.len;
   if Result>=size then Break;
  until false;
 end;
end;

function evbuffer_move(Src,Dst:Pevbuffer):SizeUInt;
Var
 vec:Peviovec;
begin
 Result:=0;
 if Assigned(Dst) then
 repeat
  vec:=evbuffer_pop(Src);
  if vec=nil then Exit;
  Result:=Result+vec^.len;
  evbuffer_push(Dst,vec);
 until false;
end;

function evbuffer_move_length(Src,Dst:Pevbuffer;length:SizeUInt):SizeUInt;
var
 i:SizeUInt;
 vec:Peviovec;
begin
 Result:=0;
 if Assigned(Dst) then
 repeat
  vec:=evbuffer_peek(Src);
  if vec=nil then Exit;
  i:=Result+vec^.len;
  if i>length then
  begin
   i:=length-Result;
   evbuffer_add(Dst,eviovec_getdata(vec),i);
   evbuffer_drain(Src,i);
   Result:=length;
   Exit;
  end else
  begin
   evbuffer_push(Dst,evbuffer_pop(Src));
   Result:=i;
   if length=Result then Exit;
  end;
 until false;
end;

Var
 cache_peviovec:Peviovec=nil;

Function get_peviovec:peviovec;
begin
 Result:=XCHG(cache_peviovec,nil);
 if Result=nil then
 begin
  Result:=GetMem(SizeOf(Teviovec));
 end;
end;

Function free_peviovec(p:pointer):SizeUInt;
begin
 Result:=FreeMem(XCHG(cache_peviovec,p));
end;

function evbuffer_add_ref(buf:Pevbuffer;data:pointer;datapos,datalen:SizeUInt;ff:TfuncFree):Boolean;
Var
 Node:Peviovec;
begin
 Result:=False;
 if (not Assigned(buf)) or
    (not Assigned(data)) or
    (datalen=0) then Exit;
 //Node:=GetMem(SizeOf(Teviovec));
 Node:=get_peviovec;
 if Node=nil then Exit;
 With Node^ do
 begin
  base:=data;
  len:=datalen;
  pos:=datapos;
  buf_free:=ff;
  //vec_free:=Freemem_ptr;
  vec_free:=@free_peviovec;
 end;
 Result:=evbuffer_push(buf,Node);
end;

function evbuffer_remove_ref(buf:Pevbuffer;var data:pointer;var datapos,datalen:SizeUInt;var ff:TfuncFree):Boolean;
Var
 Node:Peviovec;
begin
 Node:=evbuffer_pop(buf);
 Result:=Assigned(Node);
 if Result then
 begin
  data   :=Node^.base;
  datapos:=Node^.pos;
  datalen:=Node^.len;
  ff     :=Node^.buf_free;
  if Assigned(Node^.vec_free) then
  begin
   Node^.vec_free(Node);
  end;
 end;
end;

function _evbuffer_add_opt(buf:Pevbuffer;data:pointer;datalen:SizeUInt):Boolean;
Var
 Node:Peviovec;
begin
 Result:=False;
 Node:=GetMem(datalen+SizeOf(Teviovec));
 if Node=nil then Exit;
 With Node^ do
 begin
  base:=@PByte(Node)[SizeOf(Teviovec)];
  len:=datalen;
  pos:=0;
  buf_free:=nil;
  vec_free:=Freemem_ptr;
 end;
 Move(data^,Node^.base^,datalen);
 Result:=evbuffer_push(buf,Node);
end;

function evbuffer_add(buf:Pevbuffer;data:pointer;datalen:SizeUInt):Boolean;
Const
 optimal_size=4*1024-SizeOf(Teviovec)-2*SizeOf(Pointer);
Var
 base:Pointer;
begin
 Result:=False;
 if (not Assigned(buf)) or
    (not Assigned(data)) or
    (datalen=0) then Exit;

 if (datalen<=optimal_size) then
 begin
  Result:=_evbuffer_add_opt(buf,data,datalen);
 end else
 begin
  base:=GetMem(datalen);
  Move(data^,base^,datalen);
  Result:=evbuffer_add_ref(buf,base,0,datalen,Freemem_ptr);
 end;

end;

function evbuffer_remove(buf:Pevbuffer;data:pointer;datalen:SizeUInt):SizeUInt;
Var
 vec:Peviovec;
begin
 Result:=0;
 if not Assigned(data) then Exit;
 While (datalen<>0) do
 begin
  vec:=evbuffer_peek(buf);
  if not Assigned(vec) then Break;
  With vec^ do
  begin
   if (len>datalen) then
   begin
    Move(PByte(base)[pos],data^,datalen);
    pos:=pos+datalen;
    len:=len-datalen;
    Result:=Result+datalen;
    fetch_sub(buf^.len,datalen);
    Break;
   end else
   begin
    Move(PByte(base)[pos],data^,len);
    datalen:=datalen-len;
    Result:=Result+len;
    data:=@PByte(data)[len];
    eviovec_free(evbuffer_pop(buf));
   end;
  end;
 end;
end;

function evbuffer_copy(buf:Pevbuffer;data:pointer;datalen:SizeUInt):SizeUInt;
Var
 vec:Peviovec;
begin
 Result:=0;
 if not Assigned(data) then Exit;
 vec:=evbuffer_peek(buf);
 While (datalen<>0) and Assigned(vec) do
 begin
  With vec^ do
  begin
   if (len>datalen) then
   begin
    Move(PByte(base)[pos],data^,datalen);
    Result:=Result+datalen;
    Break;
   end else
   begin
    Move(PByte(base)[pos],data^,len);
    datalen:=datalen-len;
    Result:=Result+len;
    data:=@PByte(data)[len];
    vec:=eviovec_next(buf,vec);
   end;
  end;
 end;
end;

function evbuffer_drain(buf:Pevbuffer;datalen:SizeUInt):SizeUInt;
Var
 vec:Peviovec;
begin
 Result:=0;
 While (datalen<>0) do
 begin
  vec:=evbuffer_peek(buf);
  if not Assigned(vec) then Break;
  With vec^ do
  begin
   if (len>datalen) then
   begin
    pos:=pos+datalen;
    len:=len-datalen;
    Result:=Result+datalen;
    fetch_sub(buf^.len,datalen);
    Break;
   end else
   begin
    datalen:=datalen-len;
    Result:=Result+len;
    eviovec_free(evbuffer_pop(buf));
   end;
  end;
 end;
end;

function evbuffer_get_length(buf:Pevbuffer):SizeUInt;
begin
 Result:=0;
 if not Assigned(buf) then Exit;
 Result:=buf^.len;
end;

function evbuffer_get_contiguous_space(buf:Pevbuffer):SizeUInt;
Var
 vec:Peviovec;
begin
 Result:=0;
 vec:=evbuffer_peek(buf);
 if Assigned(vec) then
 begin
  Result:=vec^.len;
 end;
end;

end.

