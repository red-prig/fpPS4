unit subr_sleepqueue;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 gtailq,
 hamt,
 kern_mtx;

const
 SLEEPQ_TYPE         =$ff;  // Mask of sleep queue types.
 SLEEPQ_SLEEP        =$00;  // Used by sleep/wakeup.
 SLEEPQ_CONDVAR      =$01;  // Used for a cv.
 SLEEPQ_PAUSE        =$02;  // Used by pause.
 SLEEPQ_SX           =$03;  // Used by an sx lock.
 SLEEPQ_LK           =$04;  // Used by a lockmgr.
 SLEEPQ_INTERRUPTIBLE=$100; // Sleep is interruptible.
 SLEEPQ_STOP_ON_BDRY =$200; // Stop sleeping thread

 SC_TABLESIZE=128;
 SC_MASK     =(SC_TABLESIZE-1);
 SC_SHIFT    =8;

 NR_SLEEPQS=2;

type
 kthread_list=packed record
  pFirst:Pointer;
  pLast :PPointer;
 end;

 p_sleepqueue=^sleepqueue;
 sleepqueue=packed record
  sq_blocked   :array[0..NR_SLEEPQS-1] of kthread_list;
  sq_blockedcnt:array[0..NR_SLEEPQS-1] of DWORD;
  sq_hash      :TSTUB_HAMT64;
  sq_free      :p_sleepqueue;
  sq_wchan     :Pointer;
  sq_type      :Integer;
 end;

 p_sleepqueue_chain=^sleepqueue_chain;
 sleepqueue_chain=packed record
  sc_queues:p_sleepqueue;
  sc_lock  :mtx;
 end;

function  sleepq_alloc:p_sleepqueue; inline;
procedure sleepq_free(sq:p_sleepqueue); inline;
procedure sleepq_lock(wchan:Pointer);
procedure sleepq_release(wchan:Pointer);

implementation

uses
 kern_thread;

var
 sleepq_chains:array[0..SC_MASK] of sleepqueue_chain;

procedure init_sleepqueues;
var
 i:Integer;
begin
 For i:=0 to SC_MASK do
 begin
  LIST_INIT(@sleepq_chains[i].sc_queues);
  mtx_init ( sleepq_chains[i].sc_lock);
 end;
end;

function sleepq_alloc:p_sleepqueue; inline;
begin
 Result:=AllocMem(SizeOf(sleepqueue));
end;

procedure sleepq_free(sq:p_sleepqueue); inline;
begin
 FreeMem(sq);
end;

function SC_HASH(wc:Pointer):DWORD; inline;
begin
 Result:=(ptruint(wc) shr SC_SHIFT) and SC_MASK;
end;

function SC_LOOKUP(wc:Pointer):p_sleepqueue_chain; inline;
begin
 Result:=@sleepq_chains[SC_HASH(wc)];
end;

procedure sleepq_lock(wchan:Pointer);
var
 sc:p_sleepqueue_chain;
begin
 sc:=SC_LOOKUP(wchan);
 mtx_lock(sc^.sc_lock);
end;

procedure sleepq_release(wchan:Pointer);
var
 sc:p_sleepqueue_chain;
begin
 sc:=SC_LOOKUP(wchan);
 mtx_unlock(sc^.sc_lock);
end;



initialization
 init_sleepqueues;

end.

