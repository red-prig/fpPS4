unit kern_rangelock;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_thr,
 kern_mtx;

const
 RL_LOCK_READ     =$0001;
 RL_LOCK_WRITE    =$0002;
 RL_LOCK_TYPE_MASK=$0003;
 RL_LOCK_GRANTED  =$0004;

type
 off_t=Int64;

 p_rl_q_entry=^rl_q_entry;
 rl_q_entry=record
  rl_q_link :TAILQ_ENTRY; //rl_q_entry
  rl_q_start:off_t;
  rl_q_end  :off_t;
  rl_q_flags:Integer;
 end;

 p_rangelock=^rangelock;
 rangelock=record
  rl_waiters:TAILQ_HEAD; //rl_q_entry
  rl_currdep:p_rl_q_entry;
 end;

procedure rangelock_init(lock:p_rangelock);
procedure rangelock_destroy(lock:p_rangelock);
procedure rangelock_unlock(lock:p_rangelock;cookie:Pointer;ilk:p_mtx);
function  rangelock_unlock_range(lock:p_rangelock;cookie:Pointer;start,__end:off_t;ilk:p_mtx):Pointer;
function  rangelock_rlock(lock:p_rangelock;start,__end:off_t;ilk:p_mtx):Pointer;
function  rangelock_wlock(lock:p_rangelock;start,__end:off_t;ilk:p_mtx):Pointer;
procedure rlqentry_free(rleq:p_rl_q_entry);

implementation

//

function  msleep(ident   :Pointer;
                 lock    :p_mtx;
                 priority:Integer;
                 wmesg   :PChar;
                 timo    :Int64):Integer; external;

procedure wakeup(ident:Pointer); external;

//

function rlqentry_alloc():p_rl_q_entry; inline;
begin
 Result:=AllocMem(SizeOf(rl_q_entry));
end;

procedure rlqentry_free(rleq:p_rl_q_entry); inline;
begin
 if (rleq<>nil) then
 begin
  FreeMem(rleq);
 end;
end;

procedure rangelock_init(lock:p_rangelock);
begin
 TAILQ_INIT(@lock^.rl_waiters);
 lock^.rl_currdep:=nil;
end;

procedure rangelock_destroy(lock:p_rangelock);
begin
 Assert(TAILQ_EMPTY(@lock^.rl_waiters), 'Dangling waiters');
end;

{
 * Verifies the supplied rl_q_entries for compatibility.  Returns true
 * if the rangelock queue entries are not compatible, false if they are.
 *
 * Two entries are compatible if their ranges do not overlap, or both
 * entries are for read.
}
function rangelock_incompatible(e1,e2:p_rl_q_entry):Integer;
begin
 if ((e1^.rl_q_flags and RL_LOCK_TYPE_MASK)=RL_LOCK_READ) and
    ((e2^.rl_q_flags and RL_LOCK_TYPE_MASK)=RL_LOCK_READ) then
 begin
  Exit(0);
 end;
 if (e1^.rl_q_start < e2^.rl_q_end) and (e1^.rl_q_end > e2^.rl_q_start) then
 begin
  Exit(1);
 end;
 Exit(0);
end;

{
 * Recalculate the lock^.rl_currdep after an unlock.
}
procedure rangelock_calc_block(lock:p_rangelock);
label
 _out;
var
 entry,entry1,whead:p_rl_q_entry;
begin

 if (lock^.rl_currdep=TAILQ_FIRST(@lock^.rl_waiters)) and
    (lock^.rl_currdep<>nil) then
 begin
  lock^.rl_currdep:=TAILQ_NEXT(lock^.rl_currdep, @lock^.rl_currdep^.rl_q_link);
 end;

 entry:=lock^.rl_currdep;

 while (entry<>nil) do
 begin
  entry1:=TAILQ_FIRST(@lock^.rl_waiters);

  while (entry1<>nil) do
  begin
   if (rangelock_incompatible(entry, entry1)<>0) then
   begin
    goto _out;
   end;
   if (entry1=entry) then
   begin
    break;
   end;
   //
   entry1:=TAILQ_NEXT(entry1, @entry1^.rl_q_link)
  end;

  //next
  entry:=TAILQ_NEXT(entry, @entry^.rl_q_link);
 end;

_out:
 lock^.rl_currdep:=entry;

 whead:=TAILQ_FIRST(@lock^.rl_waiters);

 while (whead<>nil) do
 begin
  if (whead=lock^.rl_currdep) then
  begin
   break;
  end;
  if ((whead^.rl_q_flags and RL_LOCK_GRANTED)=0) then
  begin
   whead^.rl_q_flags:=whead^.rl_q_flags or RL_LOCK_GRANTED;
   wakeup(whead);
  end;
  //
  whead:=TAILQ_NEXT(whead, @whead^.rl_q_link)
 end;
end;

procedure rangelock_unlock_locked(lock:p_rangelock;entry:p_rl_q_entry;ilk:p_mtx);
begin
 Assert((lock<>nil) and (entry<>nil) and (ilk<>nil));
 mtx_assert(ilk^);
 Assert(entry<>lock^.rl_currdep, 'stuck currdep');

 TAILQ_REMOVE(@lock^.rl_waiters, entry, @entry^.rl_q_link);
 rangelock_calc_block(lock);
 mtx_unlock(ilk^);

 if (curkthread^.td_rlqe=nil) then
  curkthread^.td_rlqe:=entry
 else
  rlqentry_free(entry);
end;

procedure rangelock_unlock(lock:p_rangelock;cookie:Pointer;ilk:p_mtx);
begin
 Assert((lock<>nil) and (cookie<>nil) and (ilk<>nil));

 mtx_lock(ilk^);
 rangelock_unlock_locked(lock, cookie, ilk);
end;

{
 * Unlock the sub-range of granted lock.
}
function rangelock_unlock_range(lock:p_rangelock;cookie:Pointer;start,__end:off_t;ilk:p_mtx):Pointer;
var
 entry:p_rl_q_entry;
begin
 Assert((lock<>nil) and (cookie<>nil) and (ilk<>nil));

 entry:=cookie;

 Assert((entry^.rl_q_flags and RL_LOCK_GRANTED)<>0,'Unlocking non-granted lock');
 Assert(entry^.rl_q_start=start, 'wrong start');
 Assert(entry^.rl_q_end >= __end, 'wrong end');

 mtx_lock(ilk^);

 if (entry^.rl_q_end=__end) then
 begin
  rangelock_unlock_locked(lock, cookie, ilk);
  Exit(nil);
 end;

 entry^.rl_q_end:=__end;
 rangelock_calc_block(lock);
 mtx_unlock(ilk^);
 Exit(cookie);
end;

{
 * Add the lock request to the queue of the pending requests for
 * rangelock.  Sleep until the request can be granted.
}
function rangelock_enqueue(lock:p_rangelock;start,__end:off_t;mode:Integer;ilk:p_mtx):Pointer;
var
 entry:p_rl_q_entry;
 td:p_kthread;
begin
 Assert((lock<>nil) and (ilk<>nil));

 td:=curkthread;
 if (td^.td_rlqe<>nil) then
 begin
  entry:=td^.td_rlqe;
  td^.td_rlqe:=nil;
 end else
 begin
  entry:=rlqentry_alloc();
 end;

 Assert(entry<>nil);
 entry^.rl_q_flags:=mode;
 entry^.rl_q_start:=start;
 entry^.rl_q_end:=__end;

 mtx_lock(ilk^);
 {
  * XXXKIB TODO. Check that a thread does not try to enqueue a
  * lock that is incompatible with another request from the same
  * thread.
 }

 TAILQ_INSERT_TAIL(@lock^.rl_waiters, entry, @entry^.rl_q_link);

 if (lock^.rl_currdep=nil) then
 begin
  lock^.rl_currdep:=entry;
 end;

 rangelock_calc_block(lock);

 while ((entry^.rl_q_flags and RL_LOCK_GRANTED)=0) do
 begin
  msleep(entry, ilk, 0, 'range', 0);
 end;

 mtx_unlock(ilk^);
 Exit(entry);
end;

function rangelock_rlock(lock:p_rangelock;start,__end:off_t;ilk:p_mtx):Pointer;
begin
 Result:=rangelock_enqueue(lock, start, __end, RL_LOCK_READ, ilk);
end;

function rangelock_wlock(lock:p_rangelock;start,__end:off_t;ilk:p_mtx):Pointer;
begin
 Result:=rangelock_enqueue(lock, start, __end, RL_LOCK_WRITE, ilk);
end;



end.




