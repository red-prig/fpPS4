unit kern_mtxpool;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_mtx;

const
 { Pool sizes must be a power of two }
 MTX_POOL_LOCKBUILDER_SIZE=128;
 MTX_POOL_SLEEP_SIZE      =128;

type
 mtxpool_header=packed record
  mtxpool_size :Integer;
  mtxpool_mask :Integer;
  mtxpool_shift:Integer;
  mtxpool_next :Integer;
 end;

 pp_mtx_pool=^p_mtx_pool;
 p_mtx_pool=^mtx_pool;
 mtx_pool=packed object
  mtx_pool_header:mtxpool_header;
  mtx_pool_ary   :array[0..0] of mtx;
  property mtx_pool_size :Integer read mtx_pool_header.mtxpool_size  write mtx_pool_header.mtxpool_size ;
  property mtx_pool_mask :Integer read mtx_pool_header.mtxpool_mask  write mtx_pool_header.mtxpool_mask ;
  property mtx_pool_shift:Integer read mtx_pool_header.mtxpool_shift write mtx_pool_header.mtxpool_shift;
  property mtx_pool_next :Integer read mtx_pool_header.mtxpool_next  write mtx_pool_header.mtxpool_next ;
 end;

 mtx_pool_lockbuilder=packed record
  mtx_pool_header:mtxpool_header;
  mtx_pool_ary   :array[0..MTX_POOL_LOCKBUILDER_SIZE-1] of mtx;
 end;

function  mtx_pool_find(pool:p_mtx_pool;ptr:Pointer):p_mtx;
procedure mtx_pool_initialize(pool:p_mtx_pool;mtx_name:PChar;pool_size:Integer);
function  mtx_pool_create(mtx_name:PChar;pool_size:Integer):p_mtx_pool;
procedure mtx_pool_destroy(poolp:pp_mtx_pool);
function  mtx_pool_alloc(pool:p_mtx_pool):p_mtx;

implementation

const
 POINTER_BITS   =64;
 HASH_MULTIPLIER=11400714819323198485; { (2^64)*(sqrt(5)-1)/2 }

{
 * Exitthe (shared) pool mutex associated with the specified address.
 * The returned mutex is a leaf level mutex, meaning that if you obtain it
 * you cannot obtain any other mutexes until you release it.  You can
 * legally msleep() on the mutex.
 }
function mtx_pool_find(pool:p_mtx_pool;ptr:Pointer):p_mtx;
var
 p:Integer;
begin
 Assert(pool<>nil, ('_mtx_pool_find(): nil pool'));
 {
  * Fibonacci hash, see Knuth's
  * _Art of Computer Programming, Volume 3 / Sorting and Searching_
  }
 p:=((HASH_MULTIPLIER * ptruint(ptr)) shr pool^.mtx_pool_shift) and pool^.mtx_pool_mask;
 Exit(@pool^.mtx_pool_ary[p]);
end;

procedure mtx_pool_initialize(pool:p_mtx_pool;mtx_name:PChar;pool_size:Integer);
var
 i,maskbits:Integer;
begin
 pool^.mtx_pool_size:=pool_size;
 pool^.mtx_pool_mask:=pool_size - 1;
 i:=1;
 maskbits:=0;
 while ((i and pool_size)=0) do
 begin
  Inc(maskbits);
  i:=i shl 1;
 end;
 pool^.mtx_pool_shift:=POINTER_BITS - maskbits;
 pool^.mtx_pool_next:=0;
 For i:=0 to pool_size-1 do
  mtx_init(pool^.mtx_pool_ary[i], mtx_name);
end;

function powerof2(i:DWord):Boolean; inline;
begin
 Result:=popcnt(i)=1;
end;

function mtx_pool_create(mtx_name:PChar;pool_size:Integer):p_mtx_pool;
var
 pool:p_mtx_pool;
begin
 if (pool_size <= 0) or (not powerof2(pool_size)) then
 begin
  Writeln('WARNING: %s pool size is not a power of 2.', mtx_name);
  pool_size:=128;
 end;
 pool:=AllocMem(sizeof(mtx_pool)+
                ((pool_size - 1) * sizeof(mtx))
       );
 mtx_pool_initialize(pool, mtx_name, pool_size);
 Exit(pool);
end;

procedure mtx_pool_destroy(poolp:pp_mtx_pool);
var
 i:Integer;
 pool:p_mtx_pool;
begin
 pool:=poolp^;

 For i:=(pool^.mtx_pool_size-1) downto 0 do
 begin
  mtx_destroy(pool^.mtx_pool_ary[i]);
 end;

 FreeMem(pool);
 poolp^:=nil;
end;

{
 * Obtain a (shared) mutex from the pool.  The returned mutex is a leaf
 * level mutex, meaning that if you obtain it you cannot obtain any other
 * mutexes until you release it.  You can legally msleep() on the mutex.
 }
function mtx_pool_alloc(pool:p_mtx_pool):p_mtx;
var
 i:Integer;
begin
 Assert(pool<>nil, ('mtx_pool_alloc(): nil pool'));
 {
  * mtx_pool_next is unprotected against multiple accesses,
  * but simultaneous access by two CPUs should not be very
  * harmful.
  }
 i:=pool^.mtx_pool_next;
 pool^.mtx_pool_next:=(i + 1) and pool^.mtx_pool_mask;
 Exit(@pool^.mtx_pool_ary[i]);
end;



end.

