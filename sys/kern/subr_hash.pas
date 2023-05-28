unit subr_hash;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue;

function  hashinit(elements:Integer;hashmask:PQWORD):Pointer;
procedure hashdestroy(vhashtbl:Pointer;hashmask:QWORD);
function  phashinit(elements:Integer;nentries:PQWORD):Pointer;

implementation

{
 * General routine to allocate a hash table
 }
function hashinit(elements:Integer;hashmask:PQWORD):Pointer;
var
 hashsize:QWORD;
 hashtbl:P_LIST_HEAD;
 i:Integer;
begin
 Assert((elements > 0),'%s: bad elements');

 hashsize:=1;
 while (hashsize <= elements) do hashsize:=hashsize shl 1;
 hashsize:=hashsize shr 1;

 hashtbl:=AllocMem(hashsize*sizeof(LIST_HEAD));

 if (hashtbl<>nil) then
 begin
  For i:=0 to hashsize-1 do
  begin
   LIST_INIT(@hashtbl[i]);
  end;
  hashmask^:=hashsize - 1;
 end;

 Exit(hashtbl);
end;

procedure hashdestroy(vhashtbl:Pointer;hashmask:QWORD);
var
 hashtbl,hp:P_LIST_HEAD;
begin
 hashtbl:=vhashtbl;

 hp:=hashtbl;
 while (hp <= @hashtbl[hashmask]) do
 begin
  Assert(LIST_EMPTY(hp),'%s: hash not empty');
  Inc(hp);
 end;
 FreeMem(hashtbl);
end;

const
 NPRIMES=27;
 primes:array[0..NPRIMES-1] of Integer=(
  1, 13, 31, 61, 127, 251, 509, 761, 1021, 1531,
  2039, 2557, 3067, 3583, 4093, 4603, 5119, 5623, 6143,
  6653, 7159, 7673, 8191, 12281, 16381, 24571, 32749
 );

{
 * General routine to allocate a prime number sized hash table.
 }
function phashinit(elements:Integer;nentries:PQWORD):Pointer;
var
 hashsize:QWORD;
 hashtbl:P_LIST_HEAD;
 i:Integer;
begin
 Assert(elements > 0,'%s: bad elements');

 i:=1;
 hashsize:=primes[1];
 while (hashsize <= elements) do
 begin
  Inc(i);
  if (i=NPRIMES) then
   break;
  hashsize:=primes[i];
 end;
 hashsize:=primes[i - 1];

 hashtbl:=AllocMem(hashsize*sizeof(LIST_HEAD));

 if (hashtbl<>nil) then
 begin
  For i:=0 to hashsize-1 do
  begin
   LIST_INIT(@hashtbl[i]);
  end;
  nentries^:=hashsize;
 end;

 Exit(hashtbl);
end;


end.

