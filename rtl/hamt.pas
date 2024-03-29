{ Simplified implementation of HAMT (Hash Array Mapped Trie) with 32bit/64bit hash key.
  Specific hash functions and collision resolution are outside the scope of
   this implementation and can be implemented on top of it.

  Copyright (C) 2023 Red_prig
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

unit hamt;

{$mode objfpc}{$H+}

interface

type
 THAMT=type Pointer;
 Tfree_data_cb=procedure(data,userdata:Pointer);

function  HAMT_create32:THAMT;
function  HAMT_clear32(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
function  HAMT_destroy32(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
function  HAMT_search32(hamt:THAMT;key:DWORD):PPointer;              //mutable link to data
function  HAMT_insert32(hamt:THAMT;key:DWORD;data:Pointer):PPointer; //mutable link to data
function  HAMT_delete32(hamt:THAMT;key:DWORD;old:PPointer):Boolean;  //data
function  HAMT_traverse32(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;

function  HAMT_create64:THAMT;
function  HAMT_clear64(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
function  HAMT_destroy64(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
function  HAMT_search64(hamt:THAMT;key:QWORD):PPointer;              //mutable link to data
function  HAMT_insert64(hamt:THAMT;key:QWORD;data:Pointer):PPointer; //mutable link to data
function  HAMT_delete64(hamt:THAMT;key:QWORD;old:PPointer):Boolean;  //data
function  HAMT_traverse64(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;

type
 // [7] [5]*5 =32
 HAMT32=object
  type
   TBitKey=DWORD;
  const
   node_size=SizeOf(TBitKey)*TBitKey(8);
   node_mask=TBitKey(node_size)-TBitKey(1);
   node_bits=PopCnt(TBitKey(node_mask));
   root_bits=(TBitKey(node_size) mod TBitKey(node_bits))+TBitKey(node_bits);
   root_size=TBitKey(1) shl TBitKey(root_bits);
   root_mask=TBitKey(root_size)-TBitKey(1);
   const_one=TBitKey(1);
   const_max=not TBitKey(0);
   stack_max=(TBitKey(node_size) div TBitKey(node_bits));
 end;

 //[10] [6]*9 =64
 HAMT64=object
  type
   TBitKey=QWORD;
  const
   node_size=SizeOf(TBitKey)*TBitKey(8);
   node_mask=TBitKey(node_size)-TBitKey(1);
   node_bits=PopCnt(TBitKey(node_mask));
   root_bits=(TBitKey(node_size) mod TBitKey(node_bits))+TBitKey(node_bits);
   root_size=TBitKey(1) shl TBitKey(root_bits);
   root_mask=TBitKey(root_size)-TBitKey(1);
   const_one=TBitKey(1);
   const_max=not TBitKey(0);
   stack_max=(TBitKey(node_size) div TBitKey(node_bits));
 end;

type
 PHAMTNode32=^THAMTNode32;
 THAMTNode32=packed record
  BitMapKey:DWORD;
  BaseValue:Pointer;
 end;

 PHAMTNode64=^THAMTNode64;
 THAMTNode64=packed record
  BitMapKey:QWORD;
  BaseValue:Pointer;
 end;

 TSTUB_HAMT32=array[0..HAMT32.root_size-1] of THAMTNode32;
 TSTUB_HAMT64=array[0..HAMT64.root_size-1] of THAMTNode64;

 //iterators

 PHAMT_Iterator32=^THAMT_Iterator32;
 THAMT_Iterator32=packed object
  type
   PStackNode=^TStackNode;
   TStackNode=packed record
    bnode,cnode,enode:PHAMTNode32;
   end;
  var
   cpos:Ptruint;
   data:array[0..HAMT32.stack_max+1] of TStackNode;
 end;

 PHAMT_Iterator64=^THAMT_Iterator64;
 THAMT_Iterator64=packed object
  type
   PStackNode=^TStackNode;
   TStackNode=packed record
    bnode,cnode,enode:PHAMTNode64;
   end;
  var
   cpos:Ptruint;
   data:array[0..HAMT64.stack_max+1] of TStackNode;
 end;

//32

function HAMT_first32(hamt:THAMT;i:PHAMT_Iterator32):Boolean;
function HAMT_last32(hamt:THAMT;i:PHAMT_Iterator32):Boolean;

function HAMT_next32(i:PHAMT_Iterator32):Boolean;
function HAMT_prev32(i:PHAMT_Iterator32):Boolean;

function HAMT_get_value32(i:PHAMT_Iterator32;v:PPointer):Boolean;

//64

function HAMT_first64(hamt:THAMT;i:PHAMT_Iterator64):Boolean;
function HAMT_last64(hamt:THAMT;i:PHAMT_Iterator64):Boolean;

function HAMT_next64(i:PHAMT_Iterator64):Boolean;
function HAMT_prev64(i:PHAMT_Iterator64):Boolean;

function HAMT_get_value64(i:PHAMT_Iterator64;v:PPointer):Boolean;

implementation

function IsSubTrie32(n:PHAMTNode32):Boolean; inline;
begin
 Result:=(PtrUint(n^.BaseValue) and 1)<>0;
end;

function IsSubTrie64(n:PHAMTNode64):Boolean; inline;
begin
 Result:=(PtrUint(n^.BaseValue) and 1)<>0;
end;

procedure SetSubTrie32(n:PHAMTNode32;v:PHAMTNode32); inline;
begin
 Assert((PtrUint(v) and 1)=0);
 PtrUint(n^.BaseValue):=PtrUint(v) or 1;
end;

procedure SetSubTrie64(n:PHAMTNode64;v:PHAMTNode64); inline;
begin
 Assert((PtrUint(v) and 1)=0);
 PtrUint(n^.BaseValue):=PtrUint(v) or 1;
end;

procedure SetValue32(n:PHAMTNode32;v:Pointer); inline;
begin
 Assert((PtrUint(v) and 1)=0);
 n^.BaseValue:=v;
end;

procedure SetValue64(n:PHAMTNode64;v:Pointer); inline;
begin
 Assert((PtrUint(v) and 1)=0);
 n^.BaseValue:=v;
end;

function GetSubTrie32(n:PHAMTNode32):PHAMTNode32; inline;
begin
 PtrUint(Result):=(PtrUint(n^.BaseValue) or 1) xor 1;
end;

function GetSubTrie64(n:PHAMTNode64):PHAMTNode64; inline;
begin
 PtrUint(Result):=(PtrUint(n^.BaseValue) or 1) xor 1;
end;

function GetValue32(n:PHAMTNode32):Pointer; inline;
begin
 Result:=n^.BaseValue;
end;

function GetValue64(n:PHAMTNode64):Pointer; inline;
begin
 Result:=n^.BaseValue;
end;

function GetMutableValue32(n:PHAMTNode32):PPointer; inline;
begin
 Result:=@n^.BaseValue;
end;

function GetMutableValue64(n:PHAMTNode64):PPointer; inline;
begin
 Result:=@n^.BaseValue;
end;

function GetBitMapSize32(BitKey:DWORD):DWORD; inline;
begin
 if (BitKey=0) then Exit(0);
 Result:=PopCnt(BitKey);
end;

function GetBitMapSize64(BitKey:QWORD):QWORD; inline;
begin
 if (BitKey=0) then Exit(0);
 Result:=PopCnt(BitKey);
end;

function BitIsNotSet32(BitKey,keypart:DWORD):Boolean; inline;
begin
 Result:=(BitKey and (HAMT32.const_one shl keypart))=0;
end;

function BitIsNotSet64(BitKey,keypart:QWORD):Boolean; inline;
begin
 Result:=(BitKey and (HAMT64.const_one shl keypart))=0;
end;

function SetBitInSet32(BitKey,keypart:DWORD):DWORD; inline;
begin
 Result:=BitKey or (HAMT32.const_one shl keypart);
end;

function SetBitInSet64(BitKey,keypart:QWORD):QWORD; inline;
begin
 Result:=BitKey or (HAMT64.const_one shl keypart);
end;

function UnSetBitInSet32(BitKey,keypart:DWORD):DWORD; inline;
begin
 Result:=BitKey and (not (HAMT32.const_one shl keypart));
end;

function UnSetBitInSet64(BitKey,keypart:QWORD):QWORD; inline;
begin
 Result:=BitKey and (not (HAMT64.const_one shl keypart));
end;

function GetMapPos32(BitKey,keypart:DWORD):DWORD; inline;
var
 k:DWORD;
begin
 k:=BitKey and (not DWORD(HAMT32.const_max shl keypart));
 if (k=0) then Exit(0);
 Result:=PopCnt(k);
end;

function GetMapPos64(BitKey,keypart:QWORD):QWORD; inline;
var
 k:QWORD;
begin
 k:=BitKey and (not QWORD(HAMT64.const_max shl keypart));
 if (k=0) then Exit(0);
 Result:=PopCnt(k);
end;

function GetRootKeyMask32(key:DWORD):DWORD; inline;
begin
 Result:=key and HAMT32.root_mask;
end;

function GetNodeKeyMask32(key,bits:DWORD):DWORD; inline;
begin
 Result:=(key shr bits) and HAMT32.node_mask;
end;

function GetRootKeyMask64(key:QWORD):QWORD; inline;
begin
 Result:=key and HAMT64.root_mask;
end;

function GetNodeKeyMask64(key,bits:QWORD):QWORD; inline;
begin
 Result:=(key shr bits) and HAMT64.node_mask;
end;

function HAMT_create32:THAMT;
begin
 Result:=AllocMem(HAMT32.root_size*SizeOf(THAMTNode32));
end;

function HAMT_create64:THAMT;
begin
 Result:=AllocMem(HAMT64.root_size*SizeOf(THAMTNode64));
end;

procedure HAMT_delete_trie32(node:PHAMTNode32;cb:Tfree_data_cb;userdata:Pointer); inline;
type
 PStackNode=^TStackNode;
 TStackNode=packed record
  bnode,cnode,enode:PHAMTNode32;
 end;
var
 curr:PStackNode;
 data:array[0..HAMT32.stack_max] of TStackNode;
 Size:DWORD;
begin
 if (node^.BaseValue=nil) then Exit;
 if IsSubTrie32(node) then
 begin
  curr:=@data;
  Size:=GetBitMapSize32(node^.BitMapKey);
  With curr^ do
  begin
   bnode:=GetSubTrie32(node);
   cnode:=bnode;
   enode:=@bnode[Size];
  end;
  repeat
   if (curr^.cnode>=curr^.enode) then
   begin
    FreeMem(curr^.bnode);
    if (curr=@data) then Break;
    Dec(curr);
    Inc(curr^.cnode);
    Continue;
   end;
   if IsSubTrie32(curr^.cnode) then
   begin
    node:=curr^.cnode;
    Inc(curr);
    Size:=GetBitMapSize32(node^.BitMapKey);
    With curr^ do
    begin
     bnode:=GetSubTrie32(node);
     cnode:=bnode;
     enode:=@bnode[Size];
    end;
   end else
   begin
    if (cb<>nil) then
     cb(GetValue32(curr^.cnode),userdata);
    Inc(curr^.cnode);
   end;
  until false;
 end else
 begin
  if (cb<>nil) then
   cb(GetValue32(node),userdata);
 end;
end;

procedure HAMT_delete_trie64(node:PHAMTNode64;cb:Tfree_data_cb;userdata:Pointer); inline;
type
 PStackNode=^TStackNode;
 TStackNode=packed record
  bnode,cnode,enode:PHAMTNode64;
 end;
var
 curr:PStackNode;
 data:array[0..HAMT64.stack_max] of TStackNode;
 Size:QWORD;
begin
 if (node^.BaseValue=nil) then Exit;
 if IsSubTrie64(node) then
 begin
  curr:=@data;
  Size:=GetBitMapSize64(node^.BitMapKey);
  With curr^ do
  begin
   bnode:=GetSubTrie64(node);
   cnode:=bnode;
   enode:=@bnode[Size];
  end;
  repeat
   if (curr^.cnode>=curr^.enode) then
   begin
    FreeMem(curr^.bnode);
    if (curr=@data) then Break;
    Dec(curr);
    Inc(curr^.cnode);
    Continue;
   end;
   if IsSubTrie64(curr^.cnode) then
   begin
    node:=curr^.cnode;
    Inc(curr);
    Size:=GetBitMapSize64(node^.BitMapKey);
    With curr^ do
    begin
     bnode:=GetSubTrie64(node);
     cnode:=bnode;
     enode:=@bnode[Size];
    end;
   end else
   begin
    if (cb<>nil) then
     cb(GetValue64(curr^.cnode),userdata);
    Inc(curr^.cnode);
   end;
  until false;
 end else
 begin
  if (cb<>nil) then
   cb(GetValue64(node),userdata);
 end;
end;

function HAMT_clear32(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
var
 i:Integer;
begin
 if (hamt=nil) then Exit(False);
 For i:=0 to HAMT32.root_mask do
 begin
  HAMT_delete_trie32(@PHAMTNode32(hamt)[i],cb,userdata);
 end;
 FillChar(hamt^,HAMT32.root_size*SizeOf(THAMTNode32),0);
 Result:=True;
end;

function HAMT_clear64(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
var
 i:Integer;
begin
 if (hamt=nil) then Exit(False);
 For i:=0 to HAMT64.root_mask do
 begin
  HAMT_delete_trie64(@PHAMTNode64(hamt)[i],cb,userdata);
 end;
 FillChar(hamt^,HAMT64.root_size*SizeOf(THAMTNode64),0);
 Result:=True;
end;

procedure HAMT_traverse_trie32(node:PHAMTNode32;cb:Tfree_data_cb;userdata:Pointer); inline;
type
 PStackNode=^TStackNode;
 TStackNode=packed record
  cnode,enode:PHAMTNode32;
 end;
var
 curr:PStackNode;
 data:array[0..HAMT32.stack_max] of TStackNode;
 Size:QWORD;
begin
 if (node^.BaseValue=nil) or (cb=nil) then Exit;
 if IsSubTrie32(node) then
 begin
  curr:=@data;
  Size:=GetBitMapSize32(node^.BitMapKey);
  With curr^ do
  begin
   cnode:=GetSubTrie32(node);
   enode:=@cnode[Size];
  end;
  repeat
   if (curr^.cnode>=curr^.enode) then
   begin
    if (curr=@data) then Break;
    Dec(curr);
    Inc(curr^.cnode);
    Continue;
   end;
   if IsSubTrie32(curr^.cnode) then
   begin
    node:=curr^.cnode;
    Inc(curr);
    Size:=GetBitMapSize32(node^.BitMapKey);
    With curr^ do
    begin
     cnode:=GetSubTrie32(node);
     enode:=@cnode[Size];
    end;
   end else
   begin
    cb(GetValue32(curr^.cnode),userdata);
    Inc(curr^.cnode);
   end;
  until false;
 end else
 begin
  cb(GetValue32(node),userdata);
 end;
end;

procedure HAMT_traverse_trie64(node:PHAMTNode64;cb:Tfree_data_cb;userdata:Pointer); inline;
type
 PStackNode=^TStackNode;
 TStackNode=packed record
  cnode,enode:PHAMTNode64;
 end;
var
 curr:PStackNode;
 data:array[0..HAMT64.stack_max] of TStackNode;
 Size:QWORD;
begin
 if (node^.BaseValue=nil) or (cb=nil) then Exit;
 if IsSubTrie64(node) then
 begin
  curr:=@data;
  Size:=GetBitMapSize64(node^.BitMapKey);
  With curr^ do
  begin
   cnode:=GetSubTrie64(node);
   enode:=@cnode[Size];
  end;
  repeat
   if (curr^.cnode>=curr^.enode) then
   begin
    if (curr=@data) then Break;
    Dec(curr);
    Inc(curr^.cnode);
    Continue;
   end;
   if IsSubTrie64(curr^.cnode) then
   begin
    node:=curr^.cnode;
    Inc(curr);
    Size:=GetBitMapSize64(node^.BitMapKey);
    With curr^ do
    begin
     cnode:=GetSubTrie64(node);
     enode:=@cnode[Size];
    end;
   end else
   begin
    cb(GetValue64(curr^.cnode),userdata);
    Inc(curr^.cnode);
   end;
  until false;
 end else
 begin
  cb(GetValue64(node),userdata);
 end;
end;

function HAMT_traverse32(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
var
 i:Integer;
begin
 if (hamt=nil) then Exit(False);
 For i:=0 to HAMT32.root_mask do
 begin
  HAMT_traverse_trie32(@PHAMTNode32(hamt)[i],cb,userdata);
 end;
 Result:=True;
end;

function HAMT_traverse64(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
var
 i:Integer;
begin
 if (hamt=nil) then Exit(False);
 For i:=0 to HAMT64.root_mask do
 begin
  HAMT_traverse_trie64(@PHAMTNode64(hamt)[i],cb,userdata);
 end;
 Result:=True;
end;

function HAMT_destroy32(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
begin
 Result:=HAMT_clear32(hamt,cb,userdata);
 FreeMem(hamt);
end;

function HAMT_destroy64(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
begin
 Result:=HAMT_clear64(hamt,cb,userdata);
 FreeMem(hamt);
end;

function HAMT_search32(hamt:THAMT;key:DWORD):PPointer;
var
 node:PHAMTNode32;
 keypart,Map:DWORD;
 keypartbits:DWORD;
begin
 if (hamt=nil) then Exit(nil);

 keypartbits:=HAMT32.root_bits;

 keypart:=GetRootKeyMask32(key);

 node:=@PHAMTNode32(hamt)[keypart];

 if (node^.BaseValue=nil) then Exit(nil);

 repeat
  if not IsSubTrie32(node) then
  begin
   if (node^.BitMapKey=key) then
   begin
    Exit(GetMutableValue32(node));
   end else
    Exit(nil);
  end;
  //Subtree: look up in bitmap
  Assert(keypartbits<HAMT32.node_size);

  keypart:=GetNodeKeyMask32(key,keypartbits);

  if BitIsNotSet32(node^.BitMapKey,keypart) then
   Exit(nil); // bit is 0 in bitmap -> no match

  Map:=GetMapPos32(node^.BitMapKey,keypart);

  // Go down a level */
  node:=@GetSubTrie32(node)[Map];

  keypartbits:=keypartbits+HAMT32.node_bits;
 until false;
end;

function HAMT_search64(hamt:THAMT;key:QWORD):PPointer;
var
 node:PHAMTNode64;
 keypart,Map:QWORD;
 keypartbits:QWORD;
begin
 if (hamt=nil) then Exit(nil);

 keypartbits:=HAMT64.root_bits;

 keypart:=GetRootKeyMask64(key);

 node:=@PHAMTNode64(hamt)[keypart];

 if (node^.BaseValue=nil) then Exit(nil);

 repeat
  if not IsSubTrie64(node) then
  begin
   if (node^.BitMapKey=key) then
   begin
    Exit(GetMutableValue64(node));
   end else
    Exit(nil);
  end;
  //Subtree: look up in bitmap
  Assert(keypartbits<HAMT64.node_size);

  keypart:=GetNodeKeyMask64(key,keypartbits);

  if BitIsNotSet64(node^.BitMapKey,keypart) then
   Exit(nil); // bit is 0 in bitmap -> no match

  Map:=GetMapPos64(node^.BitMapKey,keypart);

  // Go down a level */
  node:=@GetSubTrie64(node)[Map];

  keypartbits:=keypartbits+HAMT64.node_bits;
 until false;
end;

function HAMT_insert32(hamt:THAMT;key:DWORD;data:Pointer):PPointer;
var
 node,oldnodes,newnodes:PHAMTNode32;
 key2,keypart,keypart2,Map,Size:DWORD;
 keypartbits:DWORD;
begin
 if (hamt=nil) then Exit(nil);

 Assert(data<>nil);
 Assert((PtrUint(data) and 1)=0);

 keypartbits:=HAMT32.root_bits;

 keypart:=GetRootKeyMask32(key);

 node:=@PHAMTNode32(hamt)[keypart];

 if (node^.BaseValue=nil) then
 begin
  node^.BitMapKey:=key;
  SetValue32(node,data);
  Assert(not IsSubTrie32(node));
  Exit(GetMutableValue32(node));
 end;

 repeat
  if not IsSubTrie32(node) then
  begin
   if (node^.BitMapKey=key) then
   begin
    Exit(GetMutableValue32(node));
   end else
   begin
    key2:=node^.BitMapKey;
    //build tree downward until keys differ
    repeat
     Assert(keypartbits<HAMT32.node_size);

     keypart :=GetNodeKeyMask32(key ,keypartbits);
     keypart2:=GetNodeKeyMask32(key2,keypartbits);

     if (keypart=keypart2) then
     begin
      newnodes:=AllocMem(SizeOf(THAMTNode32));
      Assert((PtrUint(newnodes) and 1)=0);
      newnodes[0].BitMapKey:=key2;
      newnodes[0].BaseValue:=node^.BaseValue;
      node^.BitMapKey:=SetBitInSet32(0,keypart);
      SetSubTrie32(node,newnodes);
      node:=@newnodes[0];
     end else
     begin
      newnodes:=AllocMem(2*SizeOf(THAMTNode32));
      Assert((PtrUint(newnodes) and 1)=0);

      if (keypart2<keypart) then
      begin
       newnodes[0].BitMapKey:=key2;
       newnodes[0].BaseValue:=node^.BaseValue;
       newnodes[1].BitMapKey:=key;
       SetValue32(@newnodes[1],data);
       Result:=GetMutableValue32(@newnodes[1]);
      end else
      begin
       newnodes[0].BitMapKey:=key;
       SetValue32(@newnodes[0],data);
       Result:=GetMutableValue32(@newnodes[0]);
       newnodes[1].BitMapKey:=key2;
       newnodes[1].BaseValue:=node^.BaseValue;
      end;

      node^.BitMapKey:=(HAMT32.const_one shl keypart) or
                       (HAMT32.const_one shl keypart2);

      SetSubTrie32(node,newnodes);
      Exit;
     end;

     keypartbits:=keypartbits+HAMT32.node_bits;
    until false;
   end;
  end; //if not IsSubTrie(node) then

  Assert(keypartbits<HAMT32.node_size);

  keypart:=GetNodeKeyMask32(key,keypartbits);

  if BitIsNotSet32(node^.BitMapKey,keypart) then
  begin
   // bit is 0 in bitmap -> add node to table

   node^.BitMapKey:=SetBitInSet32(node^.BitMapKey,keypart);
   Size:=GetBitMapSize32(node^.BitMapKey);

   Map:=GetMapPos32(node^.BitMapKey,keypart);

   oldnodes:=GetSubTrie32(node);
   if (MemSize(oldnodes)>=(Size*SizeOf(THAMTNode32))) then
   begin
    newnodes:=oldnodes;
    Move(oldnodes[Map],newnodes[Map+1],(Size-Map-1)*SizeOf(THAMTNode32));
   end else
   begin
    newnodes:=AllocMem(Size*SizeOf(THAMTNode32));
    Assert((PtrUint(newnodes) and 1)=0);
    Move(oldnodes[0]  ,newnodes[0]    ,         Map*SizeOf(THAMTNode32));
    Move(oldnodes[Map],newnodes[Map+1],(Size-Map-1)*SizeOf(THAMTNode32));
    FreeMem(oldnodes);
    SetSubTrie32(node,newnodes);
   end;

   // Set up new node
   newnodes[Map].BitMapKey:=key;
   SetValue32(@newnodes[Map],data);

   Exit(GetMutableValue32(@newnodes[Map]));
  end;

  Map:=GetMapPos32(node^.BitMapKey,keypart);
  // Go down a level */
  node:=@GetSubTrie32(node)[Map];

  keypartbits:=keypartbits+HAMT32.node_bits;
 until false;
end;

function HAMT_insert64(hamt:THAMT;key:QWORD;data:Pointer):PPointer;
var
 node,oldnodes,newnodes:PHAMTNode64;
 key2,keypart,keypart2,Map,Size:QWORD;
 keypartbits:QWORD;
begin
 if (hamt=nil) then Exit(nil);

 Assert(data<>nil);
 Assert((PtrUint(data) and 1)=0);

 keypartbits:=HAMT64.root_bits;

 keypart:=GetRootKeyMask64(key);
 node:=@PHAMTNode64(hamt)[keypart];

 if (node^.BaseValue=nil) then
 begin
  node^.BitMapKey:=key;
  SetValue64(node,data);
  Assert(not IsSubTrie64(node));
  Exit(GetMutableValue64(node));
 end;

 repeat
  if not IsSubTrie64(node) then
  begin
   if (node^.BitMapKey=key) then
   begin
    Exit(GetMutableValue64(node));
   end else
   begin
    key2:=node^.BitMapKey;
    //build tree downward until keys differ
    repeat
     Assert(keypartbits<HAMT64.node_size);

     keypart :=GetNodeKeyMask64(key ,keypartbits);
     keypart2:=GetNodeKeyMask64(key2,keypartbits);

     if (keypart=keypart2) then
     begin
      newnodes:=AllocMem(SizeOf(THAMTNode64));
      Assert((PtrUint(newnodes) and 1)=0);
      newnodes[0].BitMapKey:=key2;
      newnodes[0].BaseValue:=node^.BaseValue;
      node^.BitMapKey:=SetBitInSet64(0,keypart);
      SetSubTrie64(node,newnodes);
      node:=@newnodes[0];
     end else
     begin
      newnodes:=AllocMem(2*SizeOf(THAMTNode64));
      Assert((PtrUint(newnodes) and 1)=0);

      if (keypart2<keypart) then
      begin
       newnodes[0].BitMapKey:=key2;
       newnodes[0].BaseValue:=node^.BaseValue;
       newnodes[1].BitMapKey:=key;
       SetValue64(@newnodes[1],data);
       Result:=GetMutableValue64(@newnodes[1]);
      end else
      begin
       newnodes[0].BitMapKey:=key;
       SetValue64(@newnodes[0],data);
       Result:=GetMutableValue64(@newnodes[0]);
       newnodes[1].BitMapKey:=key2;
       newnodes[1].BaseValue:=node^.BaseValue;
      end;

      node^.BitMapKey:=(HAMT64.const_one shl keypart) or
                       (HAMT64.const_one shl keypart2);

      SetSubTrie64(node,newnodes);
      Exit;
     end;

     keypartbits:=keypartbits+HAMT64.node_bits;
    until false;
   end;
  end; //if not IsSubTrie(node) then

  Assert(keypartbits<HAMT64.node_size);

  keypart:=GetNodeKeyMask64(key,keypartbits);

  if BitIsNotSet64(node^.BitMapKey,keypart) then
  begin
   // bit is 0 in bitmap -> add node to table

   node^.BitMapKey:=SetBitInSet64(node^.BitMapKey,keypart);
   Size:=GetBitMapSize64(node^.BitMapKey);

   Map:=GetMapPos64(node^.BitMapKey,keypart);

   oldnodes:=GetSubTrie64(node);
   if (MemSize(oldnodes)>=(Size*SizeOf(THAMTNode64))) then
   begin
    newnodes:=oldnodes;
    Move(oldnodes[Map],newnodes[Map+1],(Size-Map-1)*SizeOf(THAMTNode64));
   end else
   begin
    newnodes:=AllocMem(Size*SizeOf(THAMTNode64));
    Assert((PtrUint(newnodes) and 1)=0);
    Move(oldnodes[0]  ,newnodes[0]    ,         Map*SizeOf(THAMTNode64));
    Move(oldnodes[Map],newnodes[Map+1],(Size-Map-1)*SizeOf(THAMTNode64));
    FreeMem(oldnodes);
    SetSubTrie64(node,newnodes);
   end;

   // Set up new node
   newnodes[Map].BitMapKey:=key;
   SetValue64(@newnodes[Map],data);

   Exit(GetMutableValue64(@newnodes[Map]));
  end;

  Map:=GetMapPos64(node^.BitMapKey,keypart);
  // Go down a level */
  node:=@GetSubTrie64(node)[Map];

  keypartbits:=keypartbits+HAMT64.node_bits;
 until false;
end;

function HAMT_delete32(hamt:THAMT;key:DWORD;old:PPointer):Boolean;
var
 node,oldnodes:PHAMTNode32;
 keypart,Map,Size:DWORD;
 keypartbits:DWORD;

 curr:^PHAMTNode32;
 data:array[0..HAMT32.stack_max] of PHAMTNode32;

 function copyup:Boolean;
 var
  tmp:PHAMTNode32;
 begin
  if (Map=0) then
  begin
   tmp:=@oldnodes[1];
  end else
  begin
   tmp:=@oldnodes[0];
  end;

  Result:=not IsSubTrie32(tmp);

  if Result then
  begin
   //copy up
   node^:=tmp^;
   FreeMem(oldnodes);
  end;
 end;

 procedure shrink;
 var
  newnodes:PHAMTNode32;
 begin
  if ((2*Size*SizeOf(THAMTNode32))<=MemSize(oldnodes)) then //shrink mem?
  begin
   newnodes:=AllocMem(Size*SizeOf(THAMTNode32));
   Assert((PtrUint(newnodes) and 1)=0);
   Move(oldnodes[0]    ,newnodes[0]  ,Map*SizeOf(THAMTNode32));
   Move(oldnodes[Map+1],newnodes[Map],(Size-Map)*SizeOf(THAMTNode32));
   FreeMem(oldnodes);
   SetSubTrie32(node,newnodes);
   oldnodes:=newnodes;
  end else
  begin
   Move(oldnodes[Map+1],oldnodes[Map],(Size-Map)*SizeOf(THAMTNode32));
  end;
 end;

begin
 Result:=False;
 if (hamt=nil) then Exit;

 curr:=@data;

 keypartbits:=HAMT32.root_bits;

 Map:=0;

 keypart:=GetRootKeyMask32(key);

 node:=@PHAMTNode32(hamt)[keypart];

 if (node^.BaseValue=nil) then Exit;

 repeat
  if not IsSubTrie32(node) then
  begin
   if (node^.BitMapKey=key) then
   begin
    Result:=True;

    if (old<>nil) then
    begin
     //save value
     old^:=GetValue32(node);
    end;

    //clear
    node^:=Default(THAMTNode32);

    if (curr=@data) then Exit; //not in stack

    //up
    Dec(curr);
    node:=curr^;

    keypartbits:=keypartbits-HAMT32.node_bits;

    node^.BitMapKey:=UnSetBitInSet32(node^.BitMapKey,keypart);

    Size:=GetBitMapSize32(node^.BitMapKey);
    oldnodes:=GetSubTrie32(node);

    if (Size=0) then
    begin
     repeat
      //free
      node^:=Default(THAMTNode32);
      FreeMem(oldnodes);

      if (curr=@data) then Exit; //not in stack

      //up
      Dec(curr);
      node:=curr^;

      keypartbits:=keypartbits-HAMT32.node_bits;

      keypart:=GetNodeKeyMask32(key,keypartbits);
      Map:=GetMapPos32(node^.BitMapKey,keypart);

      node^.BitMapKey:=UnSetBitInSet32(node^.BitMapKey,keypart);

      Size:=GetBitMapSize32(node^.BitMapKey);
      oldnodes:=GetSubTrie32(node);

      shrink;

     until (Size<>0);
    end else
    if (Size=1) then
    begin
     if not copyup then
     begin
      shrink;
     end;
    end else
    begin
     shrink;
    end;

    Exit;
   end else
   begin
    //non exists?
    Exit;
   end;
  end;

  //Subtree: look up in bitmap
  Assert(keypartbits<HAMT32.node_size);

  keypart:=GetNodeKeyMask32(key,keypartbits);

  if BitIsNotSet32(node^.BitMapKey,keypart) then
   Exit; // bit is 0 in bitmap -> no match

  Map:=GetMapPos32(node^.BitMapKey,keypart);

  // Go down a level
  curr^:=node;
  Inc(curr);

  node:=@GetSubTrie32(node)[Map];

  keypartbits:=keypartbits+HAMT32.node_bits;
 until false;
end;

function HAMT_delete64(hamt:THAMT;key:QWORD;old:PPointer):Boolean;
var
 node,oldnodes:PHAMTNode64;
 keypart,Map,Size:QWORD;
 keypartbits:QWORD;

 curr:^PHAMTNode64;
 data:array[0..HAMT64.stack_max] of PHAMTNode64;

 function copyup:Boolean;
 var
  tmp:PHAMTNode64;
 begin
  if (Map=0) then
  begin
   tmp:=@oldnodes[1];
  end else
  begin
   tmp:=@oldnodes[0];
  end;

  Result:=not IsSubTrie64(tmp);

  if Result then
  begin
   //copy up
   node^:=tmp^;
   FreeMem(oldnodes);
  end;
 end;

 procedure shrink;
 var
  newnodes:PHAMTNode64;
 begin
  if ((2*Size*SizeOf(THAMTNode64))<=MemSize(oldnodes)) then //shrink mem?
  begin
   newnodes:=AllocMem(Size*SizeOf(THAMTNode64));
   Assert((PtrUint(newnodes) and 1)=0);
   Move(oldnodes[0]    ,newnodes[0]  ,Map*SizeOf(THAMTNode64));
   Move(oldnodes[Map+1],newnodes[Map],(Size-Map)*SizeOf(THAMTNode64));
   FreeMem(oldnodes);
   SetSubTrie64(node,newnodes);
   oldnodes:=newnodes;
  end else
  begin
   Move(oldnodes[Map+1],oldnodes[Map],(Size-Map)*SizeOf(THAMTNode64));
  end;
 end;

begin
 Result:=False;
 if (hamt=nil) then Exit;

 curr:=@data;

 keypartbits:=HAMT64.root_bits;

 Map:=0;

 keypart:=GetRootKeyMask64(key);

 node:=@PHAMTNode64(hamt)[keypart];

 if (node^.BaseValue=nil) then Exit;

 repeat
  if not IsSubTrie64(node) then
  begin
   if (node^.BitMapKey=key) then
   begin
    Result:=True;

    if (old<>nil) then
    begin
     //save value
     old^:=GetValue64(node);
    end;

    //clear
    node^:=Default(THAMTNode64);

    if (curr=@data) then Exit; //not in stack

    //up
    Dec(curr);
    node:=curr^;

    keypartbits:=keypartbits-HAMT64.node_bits;

    node^.BitMapKey:=UnSetBitInSet64(node^.BitMapKey,keypart);

    Size:=GetBitMapSize64(node^.BitMapKey);
    oldnodes:=GetSubTrie64(node);

    if (Size=0) then
    begin
     repeat
      //free
      node^:=Default(THAMTNode64);
      FreeMem(oldnodes);

      if (curr=@data) then Exit; //not in stack

      //up
      Dec(curr);
      node:=curr^;

      keypartbits:=keypartbits-HAMT64.node_bits;

      keypart:=GetNodeKeyMask64(key,keypartbits);
      Map:=GetMapPos64(node^.BitMapKey,keypart);

      node^.BitMapKey:=UnSetBitInSet64(node^.BitMapKey,keypart);

      Size:=GetBitMapSize64(node^.BitMapKey);
      oldnodes:=GetSubTrie64(node);

      shrink;

     until (Size<>0);
    end else
    if (Size=1) then
    begin
     if not copyup then
     begin
      shrink;
     end;
    end else
    begin
     shrink;
    end;

    Exit;
   end else
   begin
    //non exists?
    Exit;
   end;
  end;

  //Subtree: look up in bitmap
  Assert(keypartbits<HAMT64.node_size);

  keypart:=GetNodeKeyMask64(key,keypartbits);

  if BitIsNotSet64(node^.BitMapKey,keypart) then
   Exit; // bit is 0 in bitmap -> no match

  Map:=GetMapPos64(node^.BitMapKey,keypart);

  // Go down a level
  curr^:=node;
  Inc(curr);

  node:=@GetSubTrie64(node)[Map];

  keypartbits:=keypartbits+HAMT64.node_bits;
 until false;
end;

//iterator32

function _HAMT_next32(i:PHAMT_Iterator32):Boolean;
var
 curr:THAMT_Iterator32.PStackNode;
 node:PHAMTNode32;
 Size:QWORD;
begin
 Result:=False;

 curr:=@i^.data[i^.cpos];

 repeat
  if (curr^.cnode>=curr^.enode) then
  begin
   //up
   if (i^.cpos=0) then Exit;
   Dec(i^.cpos);
   Dec(curr);
   //next
   Inc(curr^.cnode);
   Continue;
  end;
  if IsSubTrie32(curr^.cnode) then
  begin
   //down
   node:=curr^.cnode;
   Inc(i^.cpos);
   Inc(curr);
   Size:=GetBitMapSize32(node^.BitMapKey);
   With curr^ do
   begin
    //new
    bnode:=GetSubTrie32(node);
    cnode:=bnode;
    enode:=@bnode[Size];
   end;
  end else
  begin
   Exit(True);
  end;
 until false;
end;

//

function _HAMT_prev32(i:PHAMT_Iterator32):Boolean;
var
 curr:THAMT_Iterator32.PStackNode;
 node:PHAMTNode32;
 Size:QWORD;
begin
 Result:=False;

 curr:=@i^.data[i^.cpos];

 repeat
  if (curr^.cnode<curr^.bnode) then
  begin
   //up
   if (i^.cpos=0) then Exit;
   Dec(i^.cpos);
   Dec(curr);
   //prev
   Dec(curr^.cnode);
   Continue;
  end;
  if IsSubTrie32(curr^.cnode) then
  begin
   //down
   node:=curr^.cnode;
   Inc(i^.cpos);
   Inc(curr);
   Size:=GetBitMapSize32(node^.BitMapKey);
   With curr^ do
   begin
    //new
    bnode:=GetSubTrie32(node);
    enode:=@bnode[Size];
    cnode:=enode;
    Dec(cnode);
   end;
  end else
  begin
   Exit(True);
  end;
 until false;
end;


function HAMT_first32(hamt:THAMT;i:PHAMT_Iterator32):Boolean;
var
 node:THAMT_Iterator32.TStackNode;
begin
 if (hamt=nil) then Exit(False);

 node.bnode:=@PHAMTNode32(hamt)[0];
 node.cnode:=node.bnode;
 node.enode:=@node.bnode[HAMT32.root_mask+1];

 i^:=Default(THAMT_Iterator32);
 i^.data[0]:=node;

 Result:=_HAMT_next32(i);
end;

function HAMT_last32(hamt:THAMT;i:PHAMT_Iterator32):Boolean;
var
 node:THAMT_Iterator32.TStackNode;
begin
 if (hamt=nil) then Exit(False);

 node.bnode:=@PHAMTNode32(hamt)[0];
 node.enode:=@node.bnode[HAMT32.root_mask+1];
 node.cnode:=node.enode;
 Dec(node.cnode);

 i^:=Default(THAMT_Iterator32);
 i^.data[0]:=node;

 Result:=_HAMT_prev32(i);
end;

function HAMT_next32(i:PHAMT_Iterator32):Boolean;
var
 curr:THAMT_Iterator32.PStackNode;
begin
 Result:=False;
 if (i=nil) then Exit;
 if (i^.cpos>=Length(i^.data)) then Exit;

 curr:=@i^.data[i^.cpos];

 if (curr^.cnode<curr^.enode) then
 begin
  //next
  Inc(curr^.cnode);
 end;

 Result:=_HAMT_next32(i);
end;


function HAMT_prev32(i:PHAMT_Iterator32):Boolean;
var
 curr:THAMT_Iterator32.PStackNode;
begin
 Result:=False;
 if (i=nil) then Exit;
 if (i^.cpos>=Length(i^.data)) then Exit;

 curr:=@i^.data[i^.cpos];

 if (curr^.cnode>=curr^.bnode) then
 begin
  //prev
  Dec(curr^.cnode);
 end;

 Result:=_HAMT_prev32(i);
end;

function HAMT_get_value32(i:PHAMT_Iterator32;v:PPointer):Boolean;
var
 curr:THAMT_Iterator32.PStackNode;
begin
 Result:=False;
 if (i=nil) or (v=nil) then Exit;
 if (i^.cpos>=Length(i^.data)) then Exit;

 curr:=@i^.data[i^.cpos];

 if IsSubTrie32(curr^.cnode) then Exit;

 v^:=GetValue32(curr^.cnode);
 Result:=True;
end;

//iterator64

function _HAMT_next64(i:PHAMT_Iterator64):Boolean;
var
 curr:THAMT_Iterator64.PStackNode;
 node:PHAMTNode64;
 Size:QWORD;
begin
 Result:=False;

 curr:=@i^.data[i^.cpos];

 repeat
  if (curr^.cnode>=curr^.enode) then
  begin
   //up
   if (i^.cpos=0) then Exit;
   Dec(i^.cpos);
   Dec(curr);
   //next
   Inc(curr^.cnode);
   Continue;
  end;
  if (curr^.cnode^.BaseValue=nil) then //space
  begin
   Inc(curr^.cnode);
   Continue;
  end;
  if IsSubTrie64(curr^.cnode) then
  begin
   //down
   node:=curr^.cnode;
   Inc(i^.cpos);
   Inc(curr);
   Size:=GetBitMapSize64(node^.BitMapKey);
   With curr^ do
   begin
    //new
    bnode:=GetSubTrie64(node);
    cnode:=bnode;
    enode:=@bnode[Size];
   end;
  end else
  begin
   Exit(True);
  end;
 until false;
end;

//

function _HAMT_prev64(i:PHAMT_Iterator64):Boolean;
var
 curr:THAMT_Iterator64.PStackNode;
 node:PHAMTNode64;
 Size:QWORD;
begin
 Result:=False;

 curr:=@i^.data[i^.cpos];

 repeat
  if (curr^.cnode<curr^.bnode) then
  begin
   //up
   if (i^.cpos=0) then Exit;
   Dec(i^.cpos);
   Dec(curr);
   //prev
   Dec(curr^.cnode);
   Continue;
  end;
  if (curr^.cnode^.BaseValue=nil) then //space
  begin
   Dec(curr^.cnode);
   Continue;
  end;
  if IsSubTrie64(curr^.cnode) then
  begin
   //down
   node:=curr^.cnode;
   Inc(i^.cpos);
   Inc(curr);
   Size:=GetBitMapSize64(node^.BitMapKey);
   With curr^ do
   begin
    //new
    bnode:=GetSubTrie64(node);
    enode:=@bnode[Size];
    cnode:=enode;
    Dec(cnode);
   end;
  end else
  begin
   Exit(True);
  end;
 until false;
end;


function HAMT_first64(hamt:THAMT;i:PHAMT_Iterator64):Boolean;
var
 node:THAMT_Iterator64.TStackNode;
begin
 if (hamt=nil) then Exit(False);

 node.bnode:=@PHAMTNode64(hamt)[0];
 node.cnode:=node.bnode;
 node.enode:=@node.bnode[HAMT64.root_mask+1];

 i^:=Default(THAMT_Iterator64);
 i^.data[0]:=node;

 Result:=_HAMT_next64(i);
end;

function HAMT_last64(hamt:THAMT;i:PHAMT_Iterator64):Boolean;
var
 node:THAMT_Iterator64.TStackNode;
begin
 if (hamt=nil) then Exit(False);

 node.bnode:=@PHAMTNode64(hamt)[0];
 node.enode:=@node.bnode[HAMT64.root_mask+1];
 node.cnode:=node.enode;
 Dec(node.cnode);

 i^:=Default(THAMT_Iterator64);
 i^.data[0]:=node;

 Result:=_HAMT_prev64(i);
end;

function HAMT_next64(i:PHAMT_Iterator64):Boolean;
var
 curr:THAMT_Iterator64.PStackNode;
begin
 Result:=False;
 if (i=nil) then Exit;
 if (i^.cpos>=Length(i^.data)) then Exit;

 curr:=@i^.data[i^.cpos];

 if (curr^.cnode<curr^.enode) then
 begin
  //next
  Inc(curr^.cnode);
 end;

 Result:=_HAMT_next64(i);
end;


function HAMT_prev64(i:PHAMT_Iterator64):Boolean;
var
 curr:THAMT_Iterator64.PStackNode;
begin
 Result:=False;
 if (i=nil) then Exit;
 if (i^.cpos>=Length(i^.data)) then Exit;

 curr:=@i^.data[i^.cpos];

 if (curr^.cnode>=curr^.bnode) then
 begin
  //prev
  Dec(curr^.cnode);
 end;

 Result:=_HAMT_prev64(i);
end;

function HAMT_get_value64(i:PHAMT_Iterator64;v:PPointer):Boolean;
var
 curr:THAMT_Iterator64.PStackNode;
begin
 Result:=False;
 if (i=nil) or (v=nil) then Exit;
 if (i^.cpos>=Length(i^.data)) then Exit;

 curr:=@i^.data[i^.cpos];

 if IsSubTrie64(curr^.cnode) then Exit;

 v^:=GetValue64(curr^.cnode);
 Result:=True;
end;

//


end.

