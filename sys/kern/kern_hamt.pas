{ Simplified implementation of HAMT (Hash Array Mapped Trie) with 64bit hash key.
  Specific hash functions and collision resolution are outside the scope of
   this implementation and can be implemented on top of it.

  Copyright (C) 2025 Red_prig
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

unit kern_hamt;

{$mode objfpc}{$H+}

{$OPTIMIZATION USELOADMODIFYSTORE,AUTOINLINE,DEADVALUES}

interface

type
 THAMT=type Pointer;
 Tfree_data_cb=procedure(data,userdata:Pointer);

function  HAMT_create64:THAMT;
function  HAMT_clear64   (hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
function  HAMT_destroy64 (hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
function  HAMT_search64  (hamt:THAMT;key:QWORD):PPointer;              //mutable link to data
function  HAMT_insert64  (hamt:THAMT;key:QWORD;data:Pointer):PPointer; //mutable link to data
function  HAMT_delete64  (hamt:THAMT;key:QWORD;old:PPointer):Boolean;  //data
function  HAMT_traverse64(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;

function  HAMT_clear32   (hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
function  HAMT_search32  (hamt:THAMT;key:DWORD):PPointer;              //mutable link to data
function  HAMT_insert32  (hamt:THAMT;key:DWORD;data:Pointer):PPointer; //mutable link to data
function  HAMT_delete32  (hamt:THAMT;key:DWORD;old:PPointer):Boolean;  //data
function  HAMT_traverse32(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;

type
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

 //[8] + [6]*4 =32
 HAMT32=object
  type
   TBitKey=QWORD;
  const
   root_bits=8;
   root_size=TBitKey(1) shl TBitKey(root_bits);
   root_mask=TBitKey(root_size)-TBitKey(1);
 end;

type
 PHAMTNode64=^THAMTNode64;
 THAMTNode64=packed record
  BitMapKey:QWORD;
  BaseValue:Pointer;
 end;

 TSTUB_HAMT64=array[0..HAMT64.root_mask] of THAMTNode64;
 TSTUB_HAMT32=array[0..HAMT32.root_mask] of THAMTNode64;

 //iterators

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

function HAMT_first64(hamt:THAMT;i:PHAMT_Iterator64):Boolean;
function HAMT_last64 (hamt:THAMT;i:PHAMT_Iterator64):Boolean;

function HAMT_next64(i:PHAMT_Iterator64):Boolean;
function HAMT_prev64(i:PHAMT_Iterator64):Boolean;

function HAMT_get_value64(i:PHAMT_Iterator64;v:PPointer):Boolean;

//internal
procedure HAMT_delete_trie64  (node:PHAMTNode64;cb:Tfree_data_cb;userdata:Pointer);
procedure HAMT_traverse_trie64(node:PHAMTNode64;cb:Tfree_data_cb;userdata:Pointer);
function  _HAMT_search64      (node:PHAMTNode64;key,keypartbits:QWORD):PPointer; sysv_abi_cdecl;
function  _HAMT_insert64      (node:PHAMTNode64;key,keypartbits:QWORD;data:Pointer):PPointer;
function  _HAMT_delete64      (node:PHAMTNode64;key,keypartbits:QWORD;old:PPointer):Boolean;

implementation

procedure Move64f(src,dst:Pointer;count:QWORD); inline;
begin
 while (count<>0) do
 begin
  PHAMTNode64(dst)^:=PHAMTNode64(src)^;
  //
  Inc(PHAMTNode64(dst));
  Inc(PHAMTNode64(src));
  Dec(count);
 end;
end;

procedure Move64b(src,dst:Pointer;count:QWORD); inline;
begin
 while (count<>0) do
 begin
  Dec(count);
  //
  PHAMTNode64(dst)[count]:=PHAMTNode64(src)[count];
 end;
end;

function IsSubTrie64(n:PHAMTNode64):Boolean; inline;
begin
 Result:=(PtrUint(n^.BaseValue) and 1)<>0;
end;

procedure SetSubTrie64(n:PHAMTNode64;v:PHAMTNode64); inline;
begin
 Assert((PtrUint(v) and 1)=0);
 PtrUint(n^.BaseValue):=PtrUint(v) or 1;
end;

procedure SetValue64(n:PHAMTNode64;v:Pointer); inline;
begin
 Assert((PtrUint(v) and 1)=0);
 n^.BaseValue:=v;
end;

function GetSubTrie64(n:PHAMTNode64):PHAMTNode64; inline;
begin
 PtrUint(Result):=PtrUint(n^.BaseValue) xor 1;
end;

function GetValue64(n:PHAMTNode64):Pointer; inline;
begin
 Result:=n^.BaseValue;
end;

function GetMutableValue64(n:PHAMTNode64):PPointer; inline;
begin
 Result:=@n^.BaseValue;
end;

function GetBitMapSize64(BitKey:QWORD):QWORD; inline;
begin
 if (BitKey=0) then Exit(0);
 Result:=PopCnt(BitKey);
end;

function BitIsNotSet64(BitKey,keypart:QWORD):Boolean; inline;
begin
 Result:=(BitKey and (HAMT64.const_one shl keypart))=0;
end;

function SetBitInSet64(BitKey,keypart:QWORD):QWORD; inline;
begin
 Result:=BitKey or (HAMT64.const_one shl keypart);
end;

function UnSetBitInSet64(BitKey,keypart:QWORD):QWORD; inline;
begin
 Result:=BitKey and (not (HAMT64.const_one shl keypart));
end;

function GetMapPos64(BitKey,keypart:QWORD):QWORD; inline;
var
 k:QWORD;
begin
 k:=BitKey and (QWORD(HAMT64.const_one shl keypart)-1);
 if (k=0) then Exit(0);
 Result:=PopCnt(k);
end;

function GetRootKeyMask64(key:QWORD):QWORD; inline;
begin
 Result:=key and HAMT64.root_mask;
end;

function GetRootKeyMask32(key:DWORD):DWORD; inline;
begin
 Result:=key and HAMT32.root_mask;
end;

function GetNodeKeyMask64(key,bits:QWORD):QWORD; inline;
begin
 Result:=(key shr bits) and HAMT64.node_mask;
end;

function HAMT_create64:THAMT;
begin
 Result:=AllocMem(SizeOf(TSTUB_HAMT64));
end;

procedure HAMT_delete_trie64(node:PHAMTNode64;cb:Tfree_data_cb;userdata:Pointer);
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

function HAMT_clear64(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
var
 i:Integer;
begin
 if (hamt=nil) then Exit(False);
 For i:=0 to HAMT64.root_mask do
 begin
  HAMT_delete_trie64(@PHAMTNode64(hamt)[i],cb,userdata);
 end;
 FillChar(hamt^,SizeOf(TSTUB_HAMT64),0);
 Result:=True;
end;

function  HAMT_clear32(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
var
 i:Integer;
begin
 if (hamt=nil) then Exit(False);
 For i:=0 to HAMT32.root_mask do
 begin
  HAMT_delete_trie64(@PHAMTNode64(hamt)[i],cb,userdata);
 end;
 FillChar(hamt^,SizeOf(TSTUB_HAMT32),0);
 Result:=True;
end;

procedure HAMT_traverse_trie64(node:PHAMTNode64;cb:Tfree_data_cb;userdata:Pointer);
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

function HAMT_traverse32(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
var
 i:Integer;
begin
 if (hamt=nil) then Exit(False);
 For i:=0 to HAMT32.root_mask do
 begin
  HAMT_traverse_trie64(@PHAMTNode64(hamt)[i],cb,userdata);
 end;
 Result:=True;
end;

function HAMT_destroy64(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
begin
 Result:=HAMT_clear64(hamt,cb,userdata);
 FreeMem(hamt);
end;

//rdi:node, rsi:key, rdx:keypartbits, rcx,  r8, r9.
function _HAMT_search64(node:PHAMTNode64;key,keypartbits:QWORD):PPointer; assembler; nostackframe; sysv_abi_cdecl;
label
 _exit,
 _not_found,
 _start,
 _sub_node;
asm
 mov THAMTNode64.BaseValue(%rdi),%r8
 test %r8,%r8
 jnz _start

  //not found
  _not_found:
  xor %eax,%eax
  ret
 _start:
  //start

 test $1,%r8 //if not IsSubTrie64(node) then
 jnz _sub_node
  //value

 cmp THAMTNode64.BitMapKey(%rdi),%rsi //if (node^.BitMapKey=key) then
 jne _not_found //Exit(nil);

  lea THAMTNode64.BaseValue(%rdi),%rax //Exit(GetMutableValue64(node));
  ret

 _sub_node:
  //subnode

  mov %edx,%ecx //cl:=keypartbits

  mov %rsi,%rax
  shr  %cl,%rax
  and  $63,%eax //GetNodeKeyMask64

  mov  %eax,%ecx //keypart

  mov THAMTNode64.BitMapKey(%rdi),%r9 //key2

  mov   $1,%eax
  shl  %cl,%rax
  and  %r9,%rax   //BitIsNotSet64(key2,keypart)
  jz   _not_found //Exit(nil);

  lea    -1(%rax),%rax
  and         %r9,%rax
  popcnt     %rax,%rax //Map:=GetMapPos64(key2,keypart);

  xor $1,%r8        //GetSubTrie64

  shl $4,%rax
  lea (%rax,%r8),%rdi  //node:=@GetSubTrie64(node)[Map];

  lea 6(%edx),%edx //keypartbits:=keypartbits+HAMT64.node_bits;

  mov THAMTNode64.BaseValue(%rdi),%r8
  jmp _start

 _exit:
end;

function HAMT_search64(hamt:THAMT;key:QWORD):PPointer;
var
 node:PHAMTNode64;
 keypart:QWORD;
begin
 if (hamt=nil) then Exit(nil);

 keypart:=GetRootKeyMask64(key);
 node:=@PHAMTNode64(hamt)[keypart];

 Exit(_HAMT_search64(node,key,HAMT64.root_bits));
end;

function HAMT_search32(hamt:THAMT;key:DWORD):PPointer;
var
 node:PHAMTNode64;
 keypart:QWORD;
begin
 if (hamt=nil) then Exit(nil);

 keypart:=GetRootKeyMask32(key);
 node:=@PHAMTNode64(hamt)[keypart];

 Exit(_HAMT_search64(node,key,HAMT32.root_bits));
end;

function _HAMT_insert64(node:PHAMTNode64;key,keypartbits:QWORD;data:Pointer):PPointer;
var
 oldnodes,newnodes:PHAMTNode64;
 key2,keypart,keypart2,Map:QWORD;
 old_size,new_size:QWORD;
begin

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
      newnodes:=GetMem(SizeOf(THAMTNode64));
      Assert((PtrUint(newnodes) and 1)=0);
      newnodes[0].BitMapKey:=key2;
      newnodes[0].BaseValue:=node^.BaseValue;
      node^.BitMapKey:=SetBitInSet64(0,keypart);
      SetSubTrie64(node,newnodes);
      node:=@newnodes[0];
     end else
     begin
      newnodes:=GetMem(2*SizeOf(THAMTNode64));
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

  key2:=node^.BitMapKey;

  if BitIsNotSet64(key2,keypart) then
  begin
   // bit is 0 in bitmap -> add node to table

   old_size:=GetBitMapSize64(key2);

   key2:=SetBitInSet64(key2,keypart);

   node^.BitMapKey:=key2;
   new_size:=GetBitMapSize64(key2);

   Map:=GetMapPos64(key2,keypart);

   oldnodes:=GetSubTrie64(node);
   if (MemSize(oldnodes)>=(new_size*SizeOf(THAMTNode64))) then
   begin
    newnodes:=oldnodes;
    Move64b(@oldnodes[Map],@newnodes[Map+1],(new_size-Map-1));
   end else
   begin
    newnodes:=GetMem(new_size*SizeOf(THAMTNode64));
    Assert((PtrUint(newnodes) and 1)=0);
    Move64f(@oldnodes[0]  ,@newnodes[0]    ,             Map);
    Move64f(@oldnodes[Map],@newnodes[Map+1],(new_size-Map-1));
    FreeMem(oldnodes);
    SetSubTrie64(node,newnodes);
   end;

   // Set up new node
   newnodes[Map].BitMapKey:=key;
   SetValue64(@newnodes[Map],data);

   Exit(GetMutableValue64(@newnodes[Map]));
  end;

  Map:=GetMapPos64(key2,keypart);
  // Go down a level
  node:=@GetSubTrie64(node)[Map];

  keypartbits:=keypartbits+HAMT64.node_bits;
 until false;
end;

function HAMT_insert64(hamt:THAMT;key:QWORD;data:Pointer):PPointer;
var
 node:PHAMTNode64;
 keypart:QWORD;
begin
 if (hamt=nil) then Exit(nil);

 Assert(data<>nil);
 Assert((PtrUint(data) and 1)=0);

 keypart:=GetRootKeyMask64(key);
 node:=@PHAMTNode64(hamt)[keypart];

 Exit(_HAMT_insert64(node,key,HAMT64.root_bits,data));
end;

function HAMT_insert32(hamt:THAMT;key:DWORD;data:Pointer):PPointer;
var
 node:PHAMTNode64;
 keypart:QWORD;
begin
 if (hamt=nil) then Exit(nil);

 Assert(data<>nil);
 Assert((PtrUint(data) and 1)=0);

 keypart:=GetRootKeyMask32(key);
 node:=@PHAMTNode64(hamt)[keypart];

 Exit(_HAMT_insert64(node,key,HAMT32.root_bits,data));
end;

function _HAMT_delete64(node:PHAMTNode64;key,keypartbits:QWORD;old:PPointer):Boolean;
var
 oldnodes,tmp:PHAMTNode64;
 key2,keypart,Map:QWORD;
 old_size,new_size:QWORD;

 curr:^PHAMTNode64;
 data:array[0..HAMT64.stack_max] of PHAMTNode64;

 procedure shrink;
 var
  newnodes:PHAMTNode64;
 begin
  if ((2*new_size*SizeOf(THAMTNode64))<=MemSize(oldnodes)) then //shrink mem?
  begin
   newnodes:=GetMem(new_size*SizeOf(THAMTNode64));
   Assert((PtrUint(newnodes) and 1)=0);
   Move64f(@oldnodes[0]    ,@newnodes[0]  ,           Map);
   Move64f(@oldnodes[Map+1],@newnodes[Map],(new_size-Map));
   FreeMem(oldnodes);
   SetSubTrie64(node,newnodes);
   oldnodes:=newnodes;
  end else
  begin
   Move64f(@oldnodes[Map+1],@oldnodes[Map],(new_size-Map));
  end;
 end;

begin
 Result:=False;

 if (node^.BaseValue=nil) then Exit;

 curr:=@data;

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

    keypart:=GetNodeKeyMask64(key,keypartbits);

    key2:=node^.BitMapKey;
    old_size:=GetBitMapSize64(key2);

    key2:=UnSetBitInSet64(key2,keypart);
    node^.BitMapKey:=key2;

    new_size:=GetBitMapSize64(key2);
    oldnodes:=GetSubTrie64(node);

    if (new_size=0) then
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

      key2:=node^.BitMapKey;
      old_size:=GetBitMapSize64(key2);

      Map:=GetMapPos64(key2,keypart);

      key2:=UnSetBitInSet64(key2,keypart);
      node^.BitMapKey:=key2;

      new_size:=GetBitMapSize64(key2);
      oldnodes:=GetSubTrie64(node);

      shrink;

     until (new_size<>0);
    end else
    if (new_size=1) then
    begin
     tmp:=@oldnodes[(Map+1) and 1];

     if not IsSubTrie64(tmp) then
     begin
      //copy up
      node^:=tmp^;
      FreeMem(oldnodes);
      Exit;
     end else
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

  key2:=node^.BitMapKey;

  if BitIsNotSet64(key2,keypart) then
  begin
   Exit; // bit is 0 in bitmap -> no match
  end;

  Map:=GetMapPos64(key2,keypart);

  // Go down a level
  curr^:=node;
  Inc(curr);

  node:=@GetSubTrie64(node)[Map];

  keypartbits:=keypartbits+HAMT64.node_bits;
 until false;
end;

function HAMT_delete64(hamt:THAMT;key:QWORD;old:PPointer):Boolean;
var
 node:PHAMTNode64;
 keypart:QWORD;
begin
 Result:=False;
 if (hamt=nil) then Exit;

 keypart:=GetRootKeyMask64(key);
 node:=@PHAMTNode64(hamt)[keypart];

 Exit(_HAMT_delete64(node,key,HAMT64.root_bits,old));
end;

function HAMT_delete32(hamt:THAMT;key:DWORD;old:PPointer):Boolean;
var
 node:PHAMTNode64;
 keypart:QWORD;
begin
 Result:=False;
 if (hamt=nil) then Exit;

 keypart:=GetRootKeyMask32(key);
 node:=@PHAMTNode64(hamt)[keypart];

 Exit(_HAMT_delete64(node,key,HAMT32.root_bits,old));
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

