unit kern_rangelock;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_thr,
 systm,
 kern_mtx;

const
 RL_LOCK_READ     =$0001;
 RL_LOCK_WRITE    =$0002;
 RL_LOCK_TYPE_MASK=$0003;
 RL_LOCK_GRANTED  =$0004;
 RL_LOCK_TRYLOCK  =$0008;

type
 off_t=Int64;

 p_rl_q_entry=^rl_q_entry;
 rl_q_entry=record
  rl_q_link :TAILQ_ENTRY; //rl_q_entry
  rl_q_start:off_t;
  rl_q___end:off_t;
  rl_q_flags:Integer;
  rl_q_count:Integer;
 end;

 p_rangelock=^rangelock;
 rangelock=record
  rl_waiters:TAILQ_HEAD; //rl_q_entry
  rl_currdep:p_rl_q_entry;
 end;

procedure rangelock_init        (lock:p_rangelock);
procedure rangelock_destroy     (lock:p_rangelock);

procedure rangelock_unlock      (lock:p_rangelock;cookie:Pointer;ilk:p_mtx);
function  rangelock_unlock_range(lock:p_rangelock;cookie:Pointer;start,__end:off_t;ilk:p_mtx):Pointer;

function  rangelock_enqueue     (lock:p_rangelock;start,__end:off_t;mode:Integer;ilk:p_mtx):Pointer;

function  rangelock_update      (lock:p_rangelock;start,__end:off_t;mode:Integer;ilk:p_mtx;cookie:Pointer):Boolean;

function  rangelock_rlock       (lock:p_rangelock;start,__end:off_t;ilk:p_mtx):Pointer;
function  rangelock_tryrlock    (lock:p_rangelock;start,__end:off_t;ilk:p_mtx):Pointer;
function  rangelock_wlock       (lock:p_rangelock;start,__end:off_t;ilk:p_mtx):Pointer;
function  rangelock_trywlock    (lock:p_rangelock;start,__end:off_t;ilk:p_mtx):Pointer;

procedure rlqentry_free         (rleq:p_rl_q_entry);

implementation

var
 global_rlqe:Pointer=nil;

function rlqentry_alloc():p_rl_q_entry; inline;
begin
 Result:=System.InterlockedExchange(global_rlqe,nil);
 if (Result=nil) then
 begin
  Result:=AllocMem(SizeOf(rl_q_entry));
 end;
end;

procedure rlqentry_free(rleq:p_rl_q_entry); inline;
begin
 if (rleq<>nil) then
 begin
  rleq:=System.InterlockedExchange(global_rlqe,rleq);
  if (rleq<>nil) then
  begin
   FreeMem(rleq);
  end;
 end;
end;

procedure rlqentry_rel(entry:p_rl_q_entry); inline;
begin
 with curkthread^ do
 if (td_rlqe=nil) then
 begin
  td_rlqe:=entry;
 end else
 begin
  rlqentry_free(entry);
 end;
end;

function rlqentry_new():p_rl_q_entry; inline;
begin
 with curkthread^ do
  if (td_rlqe<>nil) then
  begin
   Result:=td_rlqe;
   td_rlqe:=nil;
   Result^:=Default(rl_q_entry);
  end else
  begin
   Result:=rlqentry_alloc();
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
 * Two entries are compatible if their ranges do not overlap, or both
 * entries are for read.
}
function ranges_overlap(e1,e2:p_rl_q_entry):Boolean;
begin
 Result:=(e1^.rl_q_start < e2^.rl_q___end) and (e1^.rl_q___end > e2^.rl_q_start);
end;

{
 * Recalculate the lock^.rl_currdep after an unlock.
}
procedure rangelock_calc_block(lock:p_rangelock);
label
 _out;
var
 entry,nextentry,entry1:p_rl_q_entry;
begin

 //for
 entry:=lock^.rl_currdep;
 while (entry<>nil) do
 begin
  nextentry:=TAILQ_NEXT(entry, @entry^.rl_q_link);

  if (entry^.rl_q_flags and RL_LOCK_READ)<>0 then
  begin
   // Reads must not overlap with granted writes.

   //for
   entry1:=TAILQ_FIRST(@lock^.rl_waiters);
   while ((entry1^.rl_q_flags and RL_LOCK_READ)=0) do
   begin
    if ranges_overlap(entry, entry1) then
    begin
     goto _out;
    end;
    //
    entry1:=TAILQ_NEXT(entry1, @entry1^.rl_q_link);
   end;
   //for

  end else
  begin
   // Write must not overlap with any granted locks.

   //for
   entry1:=TAILQ_FIRST(@lock^.rl_waiters);
   while (entry1<>entry) do
   begin
    if ranges_overlap(entry, entry1) then
    begin
     {
     Writeln('ranges_overlap:',
              HexStr(entry),',',
              HexStr(entry^.rl_q_start,11),',',
              HexStr(entry^.rl_q_end,11),
              ' to ',
              HexStr(entry1),',',
              HexStr(entry1^.rl_q_start,11),',',
              HexStr(entry1^.rl_q_end,11));
     }
     goto _out;
    end;
    //
    entry1:=TAILQ_NEXT(entry1, @entry1^.rl_q_link);
   end;
   //for

   // Move grantable write locks to the front.
   TAILQ_REMOVE     (@lock^.rl_waiters, entry, @entry^.rl_q_link);
   TAILQ_INSERT_HEAD(@lock^.rl_waiters, entry, @entry^.rl_q_link);
  end;

  // Grant this lock.
  entry^.rl_q_flags:=entry^.rl_q_flags or RL_LOCK_GRANTED;
  wakeup(entry);
  //Writeln('rl_wakeup:',HexStr(entry));

  //
  entry:=nextentry;
 end;
 //for

 _out:
  lock^.rl_currdep:=entry;
end;

procedure rangelock_unlock_locked(lock:p_rangelock;entry:p_rl_q_entry;ilk:p_mtx;do_calc_block:Boolean);
begin
 Assert((lock<>nil) and (entry<>nil) and (ilk<>nil));
 mtx_assert(ilk^);

 if (not do_calc_block) then
 begin
  {
   * This is the case where rangelock_enqueue() has been called
   * with trylock=true and just inserted this entry in the
   * queue.
   * If rl_currdep is this entry, rl_currdep needs to
   * be set to the next entry in the rl_waiters list.
   * However, since this entry is the last entry in the
   * list, the next entry is NULL.
  }

  if (lock^.rl_currdep=entry) then
  begin
   Assert(TAILQ_NEXT(lock^.rl_currdep, @lock^.rl_currdep^.rl_q_link)=nil, 'rangelock_enqueue: next entry not NULL');

   lock^.rl_currdep:=nil;
  end;
 end else
 begin
  Assert(entry<>lock^.rl_currdep, 'stuck currdep');
 end;

 TAILQ_REMOVE(@lock^.rl_waiters, entry, @entry^.rl_q_link);
 entry^.rl_q_link:=Default(TAILQ_ENTRY);

 if (do_calc_block) then
 begin
  rangelock_calc_block(lock);
 end;

 mtx_unlock(ilk^);

 rlqentry_rel(entry);
end;

procedure rangelock_unlock(lock:p_rangelock;cookie:Pointer;ilk:p_mtx);
begin
 Assert((lock<>nil) and (cookie<>nil) and (ilk<>nil));

 mtx_lock(ilk^);
 rangelock_unlock_locked(lock, cookie, ilk, true);
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
 Assert(entry^.rl_q_start=start , 'wrong start');
 Assert(entry^.rl_q___end>=__end, 'wrong end');

 mtx_lock(ilk^);

 if (entry^.rl_q___end=__end) then
 begin
  rangelock_unlock_locked(lock, cookie, ilk, true);
  Exit(nil);
 end;

 entry^.rl_q___end:=__end;
 rangelock_calc_block(lock);
 mtx_unlock(ilk^);
 Exit(cookie);
end;

{
* Add the lock request to the queue of the pending requests for
* rangelock.  Sleep until the request can be granted unless trylock=true.
}
function rangelock_enqueue(lock:p_rangelock;start,__end:off_t;mode:Integer;ilk:p_mtx):Pointer;
var
 entry:p_rl_q_entry;
begin
 Assert((lock<>nil) and (ilk<>nil));

 entry:=rlqentry_new();
 //
 Assert(entry<>nil);
 entry^.rl_q_flags:=mode;
 entry^.rl_q_start:=start;
 entry^.rl_q___end:=__end;

 mtx_lock(ilk^);

 {
  * XXXKIB TODO. Check that a thread does not try to enqueue a
  * lock that is incompatible with another request from the same
  * thread.
 }

 TAILQ_INSERT_TAIL(@lock^.rl_waiters, entry, @entry^.rl_q_link);

 {
  * If rl_currdep=NULL, there is no entry waiting for a conflicting
  * range to be resolved, so set rl_currdep to this entry.  If there is
  * no conflicting entry for this entry, rl_currdep will be set back to
  * NULL by rangelock_calc_block().
 }

 if (lock^.rl_currdep=nil) then
 begin
  lock^.rl_currdep:=entry;
 end;

 rangelock_calc_block(lock);

 while ((entry^.rl_q_flags and RL_LOCK_GRANTED)=0) do
 begin

  if (mode and RL_LOCK_TRYLOCK)<>0 then
  begin
   {
    * For this case, the range is not actually locked
    * yet, but removal from the list requires the same
    * steps, except for not doing a rangelock_calc_block()
    * call, since rangelock_calc_block() was called above.
   }
   rangelock_unlock_locked(lock, entry, ilk, false);
   Exit(nil);
  end;

  //Writeln('rl_msleep:',HexStr(entry));
  msleep(entry, ilk, 0, 'range', 0);
 end;

 mtx_unlock(ilk^);
 Exit(entry);
end;

function get_flags_str(flags:Integer):RawByteString;
const
 _R:array[0..1] of AnsiChar='_R';
 _W:array[0..1] of AnsiChar='_W';
 _T:array[0..1] of AnsiChar='_T';
 _G:array[0..1] of AnsiChar='_G';
begin
 Result:=_R[ord((RL_LOCK_READ      and flags)<>0)]+
         _W[ord((RL_LOCK_WRITE     and flags)<>0)]+
         _G[ord((RL_LOCK_GRANTED   and flags)<>0)]+
         _T[ord((RL_LOCK_TRYLOCK   and flags)<>0)];
end;

function rangelock_update(lock:p_rangelock;start,__end:off_t;mode:Integer;ilk:p_mtx;cookie:Pointer):Boolean;
var
 entry:p_rl_q_entry;

 prev_start:off_t;
 prev___end:off_t;
 prev_flags:Integer;
begin
 Assert((lock<>nil) and (ilk<>nil) and (cookie<>nil));

 entry:=cookie;

 mtx_lock(ilk^);

 //save
 prev_start:=entry^.rl_q_start;
 prev___end:=entry^.rl_q___end;
 prev_flags:=entry^.rl_q_flags;

 {
 Writeln('rangelock_update:',HexStr(prev_start,16),',',
                             HexStr(prev___end,16),',',
                             get_flags_str(prev_flags),'->',
                             HexStr(start,16),',',
                             HexStr(__end,16),',',
                             get_flags_str(mode)
        );
 }

 //remove range
 TAILQ_REMOVE(@lock^.rl_waiters, entry, @entry^.rl_q_link);
 entry^.rl_q_link:=Default(TAILQ_ENTRY);

 //set new info
 entry^.rl_q_start:=start;
 entry^.rl_q___end:=__end;
 entry^.rl_q_flags:=mode;

 //insert
 TAILQ_INSERT_TAIL(@lock^.rl_waiters, entry, @entry^.rl_q_link);

 {
  * If rl_currdep=NULL, there is no entry waiting for a conflicting
  * range to be resolved, so set rl_currdep to this entry.  If there is
  * no conflicting entry for this entry, rl_currdep will be set back to
  * NULL by rangelock_calc_block().
 }

 if (lock^.rl_currdep=nil) then
 begin
  lock^.rl_currdep:=entry;
 end;

 rangelock_calc_block(lock);

 while ((entry^.rl_q_flags and RL_LOCK_GRANTED)=0) do
 begin

  if (mode and RL_LOCK_TRYLOCK)<>0 then
  begin

   if (lock^.rl_currdep=entry) then
   begin
    lock^.rl_currdep:=nil;
   end;

   //remove range
   TAILQ_REMOVE(@lock^.rl_waiters, entry, @entry^.rl_q_link);
   entry^.rl_q_link:=Default(TAILQ_ENTRY);

   //restore
   entry^.rl_q_start:=prev_start;
   entry^.rl_q___end:=prev___end;
   entry^.rl_q_flags:=prev_flags;

   //insert
   TAILQ_INSERT_TAIL(@lock^.rl_waiters, entry, @entry^.rl_q_link);

   mtx_unlock(ilk^);
   Exit(False);
  end;

  //Writeln('rl_msleep:',HexStr(entry));
  msleep(entry, ilk, 0, 'range', 0);
 end;

 mtx_unlock(ilk^);
 Exit(True);
end;

function rangelock_rlock(lock:p_rangelock;start,__end:off_t;ilk:p_mtx):Pointer;
begin
 Result:=rangelock_enqueue(lock, start, __end, RL_LOCK_READ, ilk);
end;

function rangelock_tryrlock(lock:p_rangelock;start,__end:off_t;ilk:p_mtx):Pointer;
begin
 Result:=rangelock_enqueue(lock, start, __end, RL_LOCK_READ or RL_LOCK_TRYLOCK, ilk);
end;

function rangelock_wlock(lock:p_rangelock;start,__end:off_t;ilk:p_mtx):Pointer;
begin
 Result:=rangelock_enqueue(lock, start, __end, RL_LOCK_WRITE, ilk);
end;

function rangelock_trywlock(lock:p_rangelock;start,__end:off_t;ilk:p_mtx):Pointer;
begin
 Result:=rangelock_enqueue(lock, start, __end, RL_LOCK_WRITE or RL_LOCK_TRYLOCK, ilk);
end;


end.




