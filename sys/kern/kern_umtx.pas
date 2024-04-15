unit kern_umtx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 atomic,
 mqueue,
 kern_mtx,
 time,
 kern_thr,
 _umtx,
 rtprio;

procedure _umutex_init(mtx:p_umutex); inline;

procedure umtx_thread_init(td:p_kthread);
procedure umtx_thread_exit(td:p_kthread);
procedure umtx_thread_fini(td:p_kthread);

function  sys__umtx_lock(mtx:Pointer):Integer;
function  sys__umtx_unlock(mtx:Pointer):Integer;
function  sys__umtx_op(obj:Pointer;op:Integer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;

//

function  kern_umtx_wake(td:p_kthread;umtx:p_umtx;n_wake,priv:Integer):Integer;

function  umtx_copyin_timeout(addr:Pointer;tsp:p_timespec):Integer;

procedure umtxq_sysinit; //SYSINIT(umtx, SI_SUB_EVENTHANDLER+1, SI_ORDER_MIDDLE, umtxq_sysinit, NULL);

const
 _UMUTEX_TRY =1;
 _UMUTEX_WAIT=2;

 UQF_UMTXQ=$0001;

 UMTX_SHARED_QUEUE   =0;
 UMTX_EXCLUSIVE_QUEUE=1;

 GOLDEN_RATIO_PRIME=2654404609;
 UMTX_CHAINS       =512;
 UMTX_SHIFTS       =(32 - 9);

 BUSY_SPINS        =200;

type
 // Priority inheritance mutex info.
 p_umtx_pi=^umtx_pi;
 umtx_pi=record
  // Owner thread
  pi_owner:p_kthread;

  // Reference count
  pi_refcount:Integer;

  // List entry to link umtx holding by thread
  pi_link:TAILQ_ENTRY; //umtx_pi

  // List entry in hash
  pi_hashlink:TAILQ_ENTRY; //umtx_pi

  // List for waiters
  pi_blocked:TAILQ_HEAD; //umtx_q

  // Identify a userland lock object
  pi_key:umtx_key;
 end;

 p_umtxq_queue=^umtxq_queue;

 // A userland synchronous object user.
 pp_umtx_q=^p_umtx_q;
 p_umtx_q=^umtx_q;
 umtx_q=record
  // Linked list for the hash.
  uq_link:TAILQ_ENTRY; //umtx_q

  // Umtx key.
  uq_key:umtx_key;

  // Umtx flags.
  uq_flags:Integer;

  // Inherited priority from PP mutex
  uq_inherited_pri:Integer;

  // The thread waits on.
  uq_thread:p_kthread;

  {
   Blocked on PI mutex. read can use chain lock
   or umtx_lock, write must have both chain lock and
   umtx_lock being hold.
  }

  uq_pi_blocked:p_umtx_pi;

  // On blocked list
  uq_lockq:TAILQ_ENTRY; //umtx_q

  // Thread contending with us
  uq_pi_contested:TAILQ_HEAD; //umtx_pi

  // Spare queue ready to be reused
  uq_spare_queue:p_umtxq_queue;

  // The queue we on
  uq_cur_queue:p_umtxq_queue;
 end;

 umtxq_head=TAILQ_HEAD; //umtx_q

 // Per-key wait-queue
 umtxq_queue=record
  head  :umtxq_head;
  key   :umtx_key;
  link  :LIST_ENTRY; //umtxq_queue
  length:Integer;
 end;

 umtxq_list=LIST_HEAD; //umtxq_queue

 // Userland lock object's wait-queue chain
 p_umtxq_chain=^umtxq_chain;
 umtxq_chain=record
  // Lock for this chain.
  uc_lock:mtx;

  // List of sleep queues.
  uc_queue:array[0..1] of umtxq_list;

  uc_spare_queue:LIST_HEAD; //umtxq_queue

  // Busy flag
  uc_busy:Integer;

  // Chain lock waiters
  uc_waiters:Integer;

  // All PI in the list
  uc_pi_list:TAILQ_HEAD; //umtx_pi
 end;

var
 umtxq_chains:array[0..1,0..UMTX_CHAINS-1] of umtxq_chain;
 umtx_pi_allocated:Integer=0;

 size_of_umtx_q:Integer=sizeof(umtx_q)+sizeof(umtxq_queue); public;

implementation

uses
 errno,
 systm,
 md_time,
 kern_proc,
 kern_synch,
 kern_param,
 sched_ule,
 vm,
 vm_map,
 sys_vm_object;

procedure UMTXQ_LOCKED_ASSERT(uc:p_umtxq_chain); inline;
begin
 mtx_assert(uc^.uc_lock);
end;

procedure UMTXQ_BUSY_ASSERT(uc:p_umtxq_chain); inline;
begin
 Assert(uc^.uc_busy<>0,'umtx chain is not busy');
end;

function UPRI(td:p_kthread):Integer; inline;
begin
 if (td^.td_user_pri>=PRI_MIN_TIMESHARE) and
    (td^.td_user_pri<=PRI_MAX_TIMESHARE) then
 begin
  Result:=PRI_MAX_TIMESHARE;
 end else
 begin
  Result:=td^.td_user_pri;
 end;
end;

function GET_SHARE(flags:Integer):Integer; inline;
begin
 if ((flags and USYNC_PROCESS_SHARED)=0) then
 begin
  Result:=THREAD_SHARE;
 end else
 begin
  Result:=PROCESS_SHARE;
 end;
end;

function GET_PRIV_SHARE(priv:Integer):Integer; inline;
begin
 if (priv<>0) then
 begin
  Result:=THREAD_SHARE;
 end else
 begin
  Result:=AUTO_SHARE;
 end;
end;

//

procedure umtxq_insert_queue(uq:p_umtx_q;q:Integer); forward;
procedure umtxq_remove_queue(uq:p_umtx_q;q:Integer); forward;
function  umtxq_signal_queue(key:p_umtx_key;n_wake,q:Integer):Integer; forward;
procedure umtx_thread_cleanup(td:p_kthread); forward;
procedure umtx_pi_clearowner(pi:p_umtx_pi); forward;

//

function umtxq_signal(key:p_umtx_key;n_wake:Integer):Integer; inline;
begin
 Result:=umtxq_signal_queue(key,n_wake,UMTX_SHARED_QUEUE);
end;

procedure umtxq_insert(uq:p_umtx_q); inline;
begin
 umtxq_insert_queue(uq,UMTX_SHARED_QUEUE);
end;

procedure umtxq_remove(uq:p_umtx_q); inline;
begin
 umtxq_remove_queue(uq,UMTX_SHARED_QUEUE);
end;

var
 umtx_lock:mtx;

procedure umtxq_sysinit;
var
 i,j:Integer;
begin
 For i:=0 to 1 do
 begin
  For j:=0 to UMTX_CHAINS-1 do
  begin
   mtx_init  ( umtxq_chains[i][j].uc_lock, 'umtxql');
   LIST_INIT (@umtxq_chains[i][j].uc_queue[0]);
   LIST_INIT (@umtxq_chains[i][j].uc_queue[1]);
   LIST_INIT (@umtxq_chains[i][j].uc_spare_queue);
   TAILQ_INIT(@umtxq_chains[i][j].uc_pi_list);
   umtxq_chains[i][j].uc_busy   :=0;
   umtxq_chains[i][j].uc_waiters:=0;
  end;
 end;

 mtx_init(umtx_lock, 'umtx lock');
 //EVENTHANDLER_REGISTER(process_exec, umtx_exec_hook, NULL, EVENTHANDLER_PRI_ANY);
end;

procedure umtxq_alloc(uq:p_umtx_q);
begin
 Assert(uq<>nil);
 uq^.uq_spare_queue:=Pointer(uq)+sizeof(umtx_q);
 //uq:=AllocMem(sizeof(umtx_q));
 //uq^.uq_spare_queue:=AllocMem(sizeof(umtxq_queue));
 //
 TAILQ_INIT(@uq^.uq_spare_queue^.head);
 TAILQ_INIT(@uq^.uq_pi_contested);
 uq^.uq_inherited_pri:=PRI_MAX;
end;

procedure umtxq_free(uq:p_umtx_q);
begin
 Assert(uq^.uq_spare_queue<>nil);
 //FreeMem(uq^.uq_spare_queue);
 //FreeMem(uq);
end;

procedure umtx_thread_init(td:p_kthread); public;
var
 uq:p_umtx_q;
begin
 uq:=td^.td_umtxq;
 umtxq_alloc(uq);
 uq^.uq_thread:=td;
end;

procedure umtx_thread_exit(td:p_kthread); public;
begin
 umtx_thread_cleanup(td);
end;

procedure umtx_thread_fini(td:p_kthread); public;
begin
 umtxq_free(td^.td_umtxq);
end;

procedure umtx_thread_cleanup(td:p_kthread);
var
 uq:p_umtx_q;
 pi:p_umtx_pi;
begin
 uq:=td^.td_umtxq;

 if (uq=nil) then Exit;

 mtx_lock(umtx_lock);

 uq^.uq_inherited_pri:=PRI_MAX;

 pi:=TAILQ_FIRST(@uq^.uq_pi_contested);
 while (pi<>nil) do
 begin
  umtx_pi_clearowner(pi);
  //
  TAILQ_REMOVE(@uq^.uq_pi_contested, pi, @pi^.pi_link);
  //
  pi:=TAILQ_FIRST(@uq^.uq_pi_contested);
 end;

 mtx_unlock(umtx_lock);

 thread_lock(td);
 sched_lend_user_prio(td, PRI_MAX);
 thread_unlock(td);
end;

procedure umtxq_hash(key:p_umtx_key);
var
 n:QWORD;
begin
 n:=QWORD(key^.info.both.a) + key^.info.both.b;
 key^.hash:=((n * GOLDEN_RATIO_PRIME) shr UMTX_SHIFTS) mod UMTX_CHAINS;
end;

function umtxq_getchain(key:p_umtx_key):p_umtxq_chain;
begin
 if (key^._type <= TYPE_SEM) then
 begin
  Exit(@umtxq_chains[1][key^.hash]);
 end;
 Exit(@umtxq_chains[0][key^.hash]);
end;

procedure umtxq_lock(key:p_umtx_key); inline;
var
 uc:p_umtxq_chain;
begin
 uc:=umtxq_getchain(key);
 mtx_lock(uc^.uc_lock);
end;

procedure umtxq_unlock(key:p_umtx_key); inline;
var
 uc:p_umtxq_chain;
begin
 uc:=umtxq_getchain(key);
 mtx_unlock(uc^.uc_lock);
end;

procedure umtxq_busy(key:p_umtx_key);
var
 uc:p_umtxq_chain;
 count:Integer;
begin
 uc:=umtxq_getchain(key);
 mtx_assert(uc^.uc_lock);
 if (uc^.uc_busy<>0) then
 begin

  //if (smp_cpus > 1) then
  begin
   count:=BUSY_SPINS;
   if (count > 0) then
   begin
    umtxq_unlock(key);
    while (uc^.uc_busy<>0) and (count > 0) do
    begin
     Dec(count);
     spin_pause;
    end;
    umtxq_lock(key);
   end;
  end;

  while (uc^.uc_busy<>0) do
  begin
   Inc(uc^.uc_waiters);
   msleep(uc, @uc^.uc_lock, 0, 'umtxqb', 0);
   Dec(uc^.uc_waiters);
  end;
 end;
 uc^.uc_busy:=1;
end;

procedure umtxq_unbusy(key:p_umtx_key);
var
 uc:p_umtxq_chain;
begin
 uc:=umtxq_getchain(key);
 mtx_assert(uc^.uc_lock);
 Assert(uc^.uc_busy<>0, 'not busy');
 uc^.uc_busy:=0;
 if (uc^.uc_waiters<>0) then
 begin
  wakeup_one(uc);
 end;
end;

function umtxq_queue_lookup(key:p_umtx_key;q:Integer):p_umtxq_queue;
var
 uh:p_umtxq_queue;
 uc:p_umtxq_chain;
begin
 uc:=umtxq_getchain(key);
 UMTXQ_LOCKED_ASSERT(uc);

 uh:=LIST_FIRST(@uc^.uc_queue[q]);
 while (uh<>nil) do
 begin
  if ((umtx_key_match(@uh^.key, key))<>0) then
  begin
   Exit(uh);
  end;
  uh:=LIST_NEXT(uh,@uh^.link);
 end;

 Exit(nil);
end;

procedure umtxq_insert_queue(uq:p_umtx_q;q:Integer);
var
 uh:p_umtxq_queue;
 uc:p_umtxq_chain;
begin
 uc:=umtxq_getchain(@uq^.uq_key);
 UMTXQ_LOCKED_ASSERT(uc);
 Assert((uq^.uq_flags and UQF_UMTXQ)=0, 'umtx_q is already on queue');
 uh:=umtxq_queue_lookup(@uq^.uq_key, q);
 if (uh<>nil) then
 begin
  LIST_INSERT_HEAD(@uc^.uc_spare_queue, uq^.uq_spare_queue, @uq^.uq_spare_queue^.link);
 end else
 begin
  uh:=uq^.uq_spare_queue;
  uh^.key:=uq^.uq_key;
  LIST_INSERT_HEAD(@uc^.uc_queue[q], uh, @uh^.link);
 end;
 uq^.uq_spare_queue:=nil;

 TAILQ_INSERT_TAIL(@uh^.head, uq, @uq^.uq_link);
 Inc(uh^.length);

 uq^.uq_flags:=uq^.uq_flags or UQF_UMTXQ;
 uq^.uq_cur_queue:=uh;
end;

procedure umtxq_remove_queue(uq:p_umtx_q;q:Integer);
var
 uc:p_umtxq_chain;
 uh:p_umtxq_queue;
begin
 uc:=umtxq_getchain(@uq^.uq_key);
 UMTXQ_LOCKED_ASSERT(uc);
 if ((uq^.uq_flags and UQF_UMTXQ)<>0) then
 begin
  uh:=uq^.uq_cur_queue;
  TAILQ_REMOVE(@uh^.head, uq, @uq^.uq_link);
  Dec(uh^.length);

  uq^.uq_flags:=uq^.uq_flags and (not UQF_UMTXQ);
  if (TAILQ_EMPTY(@uh^.head)) then
  begin
   Assert(uh^.length=0,'inconsistent umtxq_queue length');
   LIST_REMOVE(uh,@uh^.link);
  end else
  begin
   uh:=LIST_FIRST(@uc^.uc_spare_queue);
   Assert(uh<>nil,'uc_spare_queue is empty');
   LIST_REMOVE(uh,@uh^.link);
  end;
  uq^.uq_spare_queue:=uh;
  uq^.uq_cur_queue  :=nil;
 end;
end;

function umtxq_count(key:p_umtx_key):Integer;
var
 uc:p_umtxq_chain;
 uh:p_umtxq_queue;
begin
 uc:=umtxq_getchain(key);
 UMTXQ_LOCKED_ASSERT(uc);
 uh:=umtxq_queue_lookup(key, UMTX_SHARED_QUEUE);
 if (uh<>nil) then
 begin
  Exit(uh^.length);
 end;
 Exit(0);
end;

function umtxq_count_pi(key:p_umtx_key;first:pp_umtx_q):Integer;
var
 uc:p_umtxq_chain;
 uh:p_umtxq_queue;
begin
 first^:=nil;
 uc:=umtxq_getchain(key);
 UMTXQ_LOCKED_ASSERT(uc);
 uh:=umtxq_queue_lookup(key, UMTX_SHARED_QUEUE);
 if (uh<>nil) then
 begin
  first^:=TAILQ_FIRST(@uh^.head);
  Exit(uh^.length);
 end;
 Exit(0);
end;

function umtxq_check_susp(td:p_kthread):Integer;
begin
 Exit(0);
end;

function umtxq_signal_queue(key:p_umtx_key;n_wake,q:Integer):Integer;
var
 uc:p_umtxq_chain;
 uh:p_umtxq_queue;
 uq:p_umtx_q;
 ret:Integer;
begin
 ret:=0;
 uc:=umtxq_getchain(key);
 UMTXQ_LOCKED_ASSERT(uc);
 uh:=umtxq_queue_lookup(key, q);
 if (uh<>nil) then
 begin
  uq:=TAILQ_FIRST(@uh^.head);
  while (uq<>nil) do
  begin
   umtxq_remove_queue(uq, q);
   wakeup(uq);
   Inc(ret);
   if (ret >= n_wake) then
   begin
    Exit(ret);
   end;
   //
   uq:=TAILQ_FIRST(@uh^.head);
  end;
 end;
 Exit(ret);
end;

procedure umtxq_signal_thread(uq:p_umtx_q);
var
 uc:p_umtxq_chain;
begin
 uc:=umtxq_getchain(@uq^.uq_key);
 UMTXQ_LOCKED_ASSERT(uc);
 umtxq_remove(uq);
 wakeup(uq);
end;

function umtxq_sleep(uq:p_umtx_q;wmesg:pchar;timo:Int64):Integer;
var
 uc:p_umtxq_chain;
 error:Integer;
begin
 uc:=umtxq_getchain(@uq^.uq_key);
 UMTXQ_LOCKED_ASSERT(uc);
 if ((uq^.uq_flags and UQF_UMTXQ)=0) then
 begin
  Exit(0);
 end;
 error:=msleep(uq, @uc^.uc_lock, PCATCH, wmesg, timo);
 if (error=EWOULDBLOCK) then
 begin
  error:=ETIMEDOUT;
 end;
 Exit(error);
end;

function umtx_key_get(addr:Pointer;_type,share:Integer;key:p_umtx_key):Integer;
var
 td:p_kthread;
 map:vm_map_t;
 entry:vm_map_entry_t;
 pindex:vm_pindex_t;
 prot:vm_prot_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (ptrint(addr)<$1000) then Exit(EFAULT);

 key^._type:=_type;

 if (share=THREAD_SHARE) then
 begin
  key^.shared:=0;
  key^.info.private.vs  :=p_proc.p_vmspace;
  key^.info.private.addr:=QWORD(addr);
 end else
 begin
  Assert((share=PROCESS_SHARE) or (share=AUTO_SHARE));
  map:=@p_vmspace(p_proc.p_vmspace)^.vm_map;
  if (vm_map_lookup(@map, vm_offset_t(addr), VM_PROT_WRITE,
       @entry, @key^.info.shared.vm_obj, @pindex, @prot
       )<>KERN_SUCCESS) then
  begin
   Exit(EFAULT);
  end;

  if (share=PROCESS_SHARE) or
     ((share=AUTO_SHARE) and
      (VM_INHERIT_SHARE=entry^.inheritance)) then
  begin
   key^.shared:=1;
   key^.info.shared.offset:=entry^.offset + entry^.start - vm_offset_t(addr);
   vm_object_reference(key^.info.shared.vm_obj);
  end else
  begin
   key^.shared:=0;
   key^.info.private.vs  :=p_proc.p_vmspace;
   key^.info.private.addr:=QWORD(addr);
  end;
  vm_map_lookup_done(map, entry);
 end;

 umtxq_hash(key);
 Exit(0);
end;

procedure umtx_key_release(key:p_umtx_key); inline;
begin
 if (key^.shared<>0) then
 begin
  vm_object_deallocate(key^.info.shared.vm_obj);
 end;
end;

//

function _do_lock_umtx(td:p_kthread;umtx:p_umtx;id:QWORD;timo:Int64):Integer;
var
 uq:p_umtx_q;
 owner,old:QWORD;
begin
 Result:=0;

 uq:=td^.td_umtxq;

 repeat
  owner:=fuword64(umtx^.u_owner);

  if (owner=QWORD(-1)) then
  begin
   Exit(EFAULT);
  end;

  owner:=casuword64(umtx^.u_owner,UMTX_UNOWNED,id);

  if (owner=UMTX_UNOWNED) then
  begin
   Exit(0);
  end;

  if (owner=QWORD(-1)) then
  begin
   Exit(EFAULT);
  end;

  if (owner=UMTX_CONTESTED) then
  begin
   owner:=casuword64(umtx^.u_owner,UMTX_CONTESTED,id or UMTX_CONTESTED);

   if (owner=UMTX_CONTESTED) then
   begin
    Exit(0);
   end;

   if (owner=QWORD(-1)) then
   begin
    Exit(EFAULT);
   end;

   Continue;
  end;

  if (Result<>0) then Exit;

  Result:=umtx_key_get(umtx, TYPE_SIMPLE_LOCK, AUTO_SHARE, @uq^.uq_key);
  if (Result<>0) then Exit;

  umtxq_lock  (@uq^.uq_key);
  umtxq_busy  (@uq^.uq_key);
  umtxq_insert(uq);
  umtxq_unbusy(@uq^.uq_key);
  umtxq_unlock(@uq^.uq_key);

  old:=casuword64(umtx^.u_owner,owner,owner or UMTX_CONTESTED);

  if (old=QWORD(-1)) then
  begin
   umtxq_lock  (@uq^.uq_key);
   umtxq_remove(uq);
   umtxq_unlock(@uq^.uq_key);

   umtx_key_release(@uq^.uq_key);
   Exit(EFAULT);
  end;

  umtxq_lock(@uq^.uq_key);

  if (old=owner) then
  begin
   Result:=umtxq_sleep(uq,'umtx',timo);
  end;

  umtxq_remove(uq);
  umtxq_unlock(@uq^.uq_key);

  umtx_key_release(@uq^.uq_key);
 until false;
end;

function do_lock_umtx(td:p_kthread;umtx:p_umtx;id:QWORD;timeout:p_timespec):Integer;
var
 tv,ts,ts2:Int64;
begin
 Result:=0;

 if (timeout=nil) then
 begin
  Result:=_do_lock_umtx(td,umtx,id,0);
  if (Result=EINTR) then
  begin
   Result:=ERESTART;
  end;
 end else
 begin
  tv:=TIMESPEC_TO_UNIT(timeout);
  ts:=get_unit_uptime;
  ts:=ts+tv;

  repeat
   Result:=_do_lock_umtx(td,umtx,id,tvtohz(tv));
   if (Result<>ETIMEDOUT) then Break;

   ts2:=get_unit_uptime;
   if (ts2>=ts) then
   begin
    Result:=ETIMEDOUT;
    Break;
   end;

   tv:=ts-ts2;
  until false;

  if (Result=ERESTART) then
  begin
   Result:=EINTR;
  end;
 end;
end;

function do_unlock_umtx(td:p_kthread;umtx:p_umtx;id:QWORD):Integer;
var
 key:umtx_key;
 owner,old,t:QWORD;
 count:ptrint;
begin
 Result:=0;

 owner:=fuword64(umtx^.u_owner);

 if (owner=QWORD(-1)) then
 begin
  Exit(EFAULT);
 end;

 if ((owner and (not UMTX_CONTESTED))<>id) then
 begin
  Exit(EPERM);
 end;

 if ((owner and UMTX_CONTESTED)=0) then
 begin
  old:=casuword64(umtx^.u_owner,owner,UMTX_UNOWNED);

  if (old=QWORD(-1)) then
  begin
   Exit(EFAULT);
  end;

  if (old=owner) then
  begin
   Exit(0);
  end;

  owner:=old;
 end;

 Result:=umtx_key_get(umtx, TYPE_SIMPLE_LOCK, AUTO_SHARE, @key);
 if (Result<>0) then Exit;

 umtxq_lock(@key);
 umtxq_busy(@key);
 count:=umtxq_count(@key);
 umtxq_unlock(@key);

 if (count<=1) then
 begin
  t:=UMTX_UNOWNED;
 end else
 begin
  t:=UMTX_CONTESTED;
 end;

 old:=casuword64(umtx^.u_owner,owner,t);

 umtxq_lock  (@key);
 umtxq_signal(@key,1);
 umtxq_unbusy(@key);
 umtxq_unlock(@key);

 umtx_key_release(@key);

 if (old=QWORD(-1)) then
 begin
  Exit(EFAULT);
 end;

 if (old<>owner) then
 begin
  Exit(EINVAL);
 end;
end;

function do_wait(td      :p_kthread;
                 addr    :Pointer;
                 id      :QWORD;
                 timeout :p_timespec;
                 compat32:Integer;
                 priv    :Integer
                ):Integer;
var
 uq:p_umtx_q;
 tv,ts,ts2:Int64;
 tmp:QWORD;
begin
 Result:=0;

 uq:=td^.td_umtxq;

 Result:=umtx_key_get(addr, TYPE_SIMPLE_WAIT, GET_PRIV_SHARE(priv), @uq^.uq_key);
 if (Result<>0) then Exit;

 umtxq_lock  (@uq^.uq_key);
 umtxq_insert(uq);
 umtxq_unlock(@uq^.uq_key);

 if (compat32=0) then
 begin
  tmp:=fuword64(PQWORD(addr)^);
 end else
 begin
  tmp:=fuword32(PDWORD(addr)^);
 end;

 if (tmp<>id) then
 begin
  umtxq_lock  (@uq^.uq_key);
  umtxq_remove(uq);
  umtxq_unlock(@uq^.uq_key);
 end else
 if (timeout=nil) then
 begin
  umtxq_lock(@uq^.uq_key);
  Result:=umtxq_sleep(uq, 'uwait', 0);
  umtxq_remove(uq);
  umtxq_unlock(@uq^.uq_key);
 end else
 begin
  tv:=TIMESPEC_TO_UNIT(timeout);
  ts:=get_unit_uptime;
  ts:=ts+tv;

  umtxq_lock(@uq^.uq_key);
  repeat
   Result:=umtxq_sleep(uq,'uwait',tvtohz(tv));

   if ((uq^.uq_flags and UQF_UMTXQ)=0) then
   begin
    Result:=0;
    Break;
   end;

   if (Result<>ETIMEDOUT) then Break;

   umtxq_unlock(@uq^.uq_key);

   ts2:=get_unit_uptime;
   if (ts2>=ts) then
   begin
    Result:=ETIMEDOUT;
    umtxq_lock(@uq^.uq_key);
    Break;
   end;

   tv:=ts-ts2;
  until false;

  umtxq_remove(uq);
  umtxq_unlock(@uq^.uq_key);
 end;

 umtx_key_release(@uq^.uq_key);

 if (Result=ERESTART) then
 begin
  Result:=EINTR;
 end;
end;

function kern_umtx_wake(td:p_kthread;umtx:p_umtx;n_wake,priv:Integer):Integer; public;
var
 key:umtx_key;
begin
 Result:=0;

 Result:=umtx_key_get(umtx, TYPE_SIMPLE_WAIT, GET_PRIV_SHARE(priv), @key);
 if (Result<>0) then Exit;

 umtxq_lock(@key);

 Result:=umtxq_signal(@key, n_wake);

 umtxq_unlock(@key);

 umtx_key_release(@key);
 Exit(0);
end;

//

function _do_lock_normal(td:p_kthread;m:p_umutex;flags:Integer;timo:Int64;mode:Integer):Integer;
var
 uq:p_umtx_q;
 id,owner,old:DWORD;
begin
 Result:=0;

 id:=td^.td_tid;
 uq:=td^.td_umtxq;

 repeat
  owner:=fuword32(m^.m_owner);

  if (owner=DWORD(-1)) then
  begin
   Exit(EFAULT);
  end;

  if (mode=_UMUTEX_WAIT) then
  begin
   if (owner=UMUTEX_UNOWNED) or (owner=UMUTEX_CONTESTED) then
   begin
    Exit(0);
   end;
  end else
  begin
   owner:=casuword32(m^.m_owner,UMUTEX_UNOWNED,id);

   if (owner=UMUTEX_UNOWNED) then
   begin
    Exit(0);
   end;

   if (owner=DWORD(-1)) then
   begin
    Exit(EFAULT);
   end;

   if (owner=UMUTEX_CONTESTED) then
   begin
    owner:=casuword32(m^.m_owner,UMUTEX_CONTESTED,id or UMUTEX_CONTESTED);

    if (owner=UMUTEX_CONTESTED) then
    begin
     Exit(0);
    end;

    if (owner=DWORD(-1)) then
    begin
     Exit(EFAULT);
    end;

    Continue;
   end;

   if ((flags and UMUTEX_ERROR_CHECK)<>0) and
      ((owner and (not UMUTEX_CONTESTED))=id) then
   begin
    Exit(EDEADLK);
   end;

   if (mode=_UMUTEX_TRY) then
   begin
    Exit(EBUSY);
   end;

   if (Result<>0) then Exit;

   Result:=umtx_key_get(m, TYPE_NORMAL_UMUTEX, GET_SHARE(flags), @uq^.uq_key);
   if (Result<>0) then Exit;

   umtxq_lock  (@uq^.uq_key);
   umtxq_busy  (@uq^.uq_key);
   umtxq_insert(uq);
   umtxq_unlock(@uq^.uq_key);

   old:=casuword32(m^.m_owner,owner,owner or UMUTEX_CONTESTED);

   if (old=DWORD(-1)) then
   begin
    umtxq_lock  (@uq^.uq_key);
    umtxq_remove(uq);
    umtxq_unbusy(@uq^.uq_key);
    umtxq_unlock(@uq^.uq_key);

    umtx_key_release(@uq^.uq_key);

    Exit(EFAULT);
   end;

   umtxq_lock  (@uq^.uq_key);
   umtxq_unbusy(@uq^.uq_key);

   if (old=owner) then
   begin
    Result:=umtxq_sleep(uq,'umtxn',timo);
   end;

   umtxq_remove(uq);
   umtxq_unlock(@uq^.uq_key);

   umtx_key_release(@uq^.uq_key);
  end;

 until false;
end;

function do_unlock_normal(td:p_kthread;m:p_umutex;flags:Integer):Integer;
var
 key:umtx_key;
 id,owner,old,t:DWORD;
 count:ptrint;
begin
 Result:=0;

 id:=td^.td_tid;

 owner:=fuword32(m^.m_owner);

 if (owner=DWORD(-1)) then
 begin
  Exit(EFAULT);
 end;

 if ((owner and (not UMUTEX_CONTESTED))<>id) then
 begin
  Exit(EPERM);
 end;

 if ((owner and UMUTEX_CONTESTED)=0) then
 begin
  old:=casuword32(m^.m_owner,owner,UMUTEX_UNOWNED);

  if (old=DWORD(-1)) then
  begin
   Exit(EFAULT);
  end;

  if (old=owner) then
  begin
   Exit(0);
  end;

  owner:=old;
 end;

 Result:=umtx_key_get(m, TYPE_NORMAL_UMUTEX, GET_SHARE(flags), @key);
 if (Result<>0) then Exit;

 umtxq_lock(@key);
 umtxq_busy(@key);
 count:=umtxq_count(@key);
 umtxq_unlock(@key);

 if (count<=1) then
 begin
  t:=UMUTEX_UNOWNED;
 end else
 begin
  t:=UMUTEX_CONTESTED;
 end;

 old:=casuword32(m^.m_owner,owner,t);

 umtxq_lock  (@key);
 umtxq_signal(@key,1);
 umtxq_unbusy(@key);
 umtxq_unlock(@key);

 umtx_key_release(@key);

 if (old=DWORD(-1)) then
 begin
  Exit(EFAULT);
 end;

 if (old<>owner) then
 begin
  Exit(EINVAL);
 end;
end;

function do_wake_umutex(td:p_kthread;m:p_umutex):Integer;
var
 key:umtx_key;
 owner:DWORD;
 count:ptrint;
 flags:DWORD;
begin
 Result:=0;

 owner:=fuword32(m^.m_owner);

 if (owner=DWORD(-1)) then
 begin
  Exit(EFAULT);
 end;

 if ((owner and (not UMUTEX_CONTESTED))<>0) then
 begin
  Exit(0);
 end;

 flags:=fuword32(m^.m_flags);

 Result:=umtx_key_get(m, TYPE_NORMAL_UMUTEX, GET_SHARE(flags), @key);
 if (Result<>0) then Exit;

 umtxq_lock(@key);
 umtxq_busy(@key);
 count:=umtxq_count(@key);
 umtxq_unlock(@key);

 if (count>=1) then
 begin
  owner:=casuword32(m^.m_owner,UMUTEX_CONTESTED,UMUTEX_UNOWNED);
 end;

 umtxq_lock(@key);
 if (count<>0) and ((owner and (not UMUTEX_CONTESTED))=0) then
 begin
  umtxq_signal(@key, 1);
 end;
 umtxq_unbusy(@key);
 umtxq_unlock(@key);

 umtx_key_release(@key);
end;

function do_wake2_umutex(td:p_kthread;m:p_umutex;flags:DWORD):Integer;
var
 key:umtx_key;
 ktype:Integer;
 owner,old:DWORD;
 count:ptrint;
begin
 Result:=0;

 Case (flags and (UMUTEX_PRIO_INHERIT or UMUTEX_PRIO_PROTECT)) of
                    0:ktype:=TYPE_NORMAL_UMUTEX;
  UMUTEX_PRIO_INHERIT:ktype:=TYPE_PI_UMUTEX;
  UMUTEX_PRIO_PROTECT:ktype:=TYPE_PP_UMUTEX;
  else
   Exit(EINVAL);
 end;

 Result:=umtx_key_get(m, ktype, GET_SHARE(flags), @key);
 if (Result<>0) then Exit;

 owner:=0;

 umtxq_lock(@key);
 umtxq_busy(@key);
 count:=umtxq_count(@key);
 umtxq_unlock(@key);

 if (count>1) then
 begin
  owner:=fuword32(m^.m_owner);
  While ((owner and UMUTEX_CONTESTED)=0) do
  begin
   old:=casuword32(m^.m_owner,owner,owner or UMUTEX_CONTESTED);
   if (old=owner) then Break;
   owner:=old;
   if (old=DWORD(-1)) then Break;
  end;
 end else
 if (count=1) then
 begin
  owner:=fuword32(m^.m_owner);
  While ((owner and (not UMUTEX_CONTESTED))<>0) and
        ((owner and UMUTEX_CONTESTED)=0) do
  begin
   old:=casuword32(m^.m_owner,owner,owner or UMUTEX_CONTESTED);
   if (old=owner) then Break;
   owner:=old;
   if (old=DWORD(-1)) then Break;
  end;
 end;

 umtxq_lock(@key);

 if (owner=DWORD(-1)) then
 begin
  Result:=EFAULT;
  umtxq_signal(@key, High(Integer));
 end else
 if (count<>0) and ((owner and (not UMUTEX_CONTESTED))=0) then
 begin
  umtxq_signal(@key, 1);
 end;

 umtxq_unbusy(@key);
 umtxq_unlock(@key);

 umtx_key_release(@key);
end;

//

function umtx_pi_alloc():p_umtx_pi; inline;
var
 pi:p_umtx_pi;
begin
 pi:=AllocMem(sizeof(umtx_pi));
 TAILQ_INIT(@pi^.pi_blocked);
 fetch_add(umtx_pi_allocated, 1);
 Exit(pi);
end;

procedure umtx_pi_free(pi:p_umtx_pi); inline;
begin
 FreeMem(pi);
 fetch_add(umtx_pi_allocated, -1);
end;

function umtx_pi_adjust_thread(pi:p_umtx_pi;td:p_kthread):Integer;
var
 uq,uq1,uq2:p_umtx_q;
 td1:p_kthread;
begin
 mtx_assert(umtx_lock);
 if (pi=nil) then Exit(0);

 uq:=td^.td_umtxq;

 uq1:=TAILQ_PREV(uq, @uq^.uq_lockq);
 uq2:=TAILQ_NEXT(uq, @uq^.uq_lockq);

 if ((uq1<>nil) and (UPRI(td) < UPRI(uq1^.uq_thread))) or
    ((uq2<>nil) and (UPRI(td) > UPRI(uq2^.uq_thread))) then
 begin
  TAILQ_REMOVE(@pi^.pi_blocked, uq, @uq^.uq_lockq);

  uq1:=TAILQ_FIRST(@pi^.pi_blocked);
  while (uq1<>nil) do
  begin
   td1:=uq1^.uq_thread;
   //Assert(td1^.td_proc^.p_magic=P_MAGIC);
   if (UPRI(td1) > UPRI(td)) then Break;
   //
   uq1:=TAILQ_NEXT(uq1,@uq1^.uq_lockq);
  end;

  if (uq1=nil) then
  begin
   TAILQ_INSERT_TAIL(@pi^.pi_blocked, uq, @uq^.uq_lockq);
  end else
  begin
   TAILQ_INSERT_BEFORE(uq1, uq, @uq^.uq_lockq);
  end;
 end;
 Exit(1);
end;

procedure umtx_propagate_priority(td:p_kthread);
var
 uq:p_umtx_q;
 pi:p_umtx_pi;
 pri:Integer;
begin
 mtx_assert(umtx_lock);

 pri:=UPRI(td);
 uq:=td^.td_umtxq;
 pi:=uq^.uq_pi_blocked;
 if (pi=nil) then Exit;

 while (true) do
 begin
  td:=pi^.pi_owner;
  if (td=nil) or (td=curkthread) then Exit;

  //Assert(td^.td_proc<>nil);
  //Assert(td^.td_proc^.p_magic=P_MAGIC);

  thread_lock(td);
  if (td^.td_lend_user_pri > pri) then
  begin
   sched_lend_user_prio(td, pri);
  end else
  begin
   thread_unlock(td);
   Break;
  end;
  thread_unlock(td);

  uq:=td^.td_umtxq;
  pi:=uq^.uq_pi_blocked;
  if (pi=nil) then Break;

  umtx_pi_adjust_thread(pi, td);
 end;
end;

procedure umtx_repropagate_priority(pi:p_umtx_pi);
var
 uq,uq_owner:p_umtx_q;
 pi2:p_umtx_pi;
 pri:Integer;
begin
 mtx_assert(umtx_lock);

 while (pi<>nil) and (pi^.pi_owner<>nil) do
 begin
  pri:=PRI_MAX;
  uq_owner:=pi^.pi_owner^.td_umtxq;

  pi2:=TAILQ_FIRST(@uq_owner^.uq_pi_contested);
  while (pi2<>nil) do
  begin
   uq:=TAILQ_FIRST(@pi2^.pi_blocked);
   if (uq<>nil) then
   begin
    if (pri > UPRI(uq^.uq_thread)) then
    begin
     pri:=UPRI(uq^.uq_thread);
    end;
   end;
   //
   pi2:=TAILQ_NEXT(pi2,@pi2^.pi_link);
  end;

  if (pri > uq_owner^.uq_inherited_pri) then
  begin
   pri:=uq_owner^.uq_inherited_pri;
  end;

  thread_lock(pi^.pi_owner);
  sched_lend_user_prio(pi^.pi_owner, pri);
  thread_unlock(pi^.pi_owner);

  pi:=uq_owner^.uq_pi_blocked;
  if (pi<>nil) then
  begin
   umtx_pi_adjust_thread(pi, uq_owner^.uq_thread);
  end;
 end;
end;

procedure umtx_pi_setowner(pi:p_umtx_pi;owner:p_kthread);
var
 uq_owner:p_umtx_q;
begin
 uq_owner:=owner^.td_umtxq;
 mtx_assert(umtx_lock);
 if (pi^.pi_owner<>nil) then
 begin
  Assert(false,'pi_owner<>nil');
 end;
 thread_inc_ref(owner);
 pi^.pi_owner:=owner;
 TAILQ_INSERT_TAIL(@uq_owner^.uq_pi_contested, pi, @pi^.pi_link);
end;

procedure umtx_pi_clearowner(pi:p_umtx_pi);
var
 owner:p_kthread;
begin
 if (pi^.pi_owner<>nil) then
 begin
  owner:=pi^.pi_owner;
  pi^.pi_owner:=nil;
  thread_dec_ref(owner);
 end;
end;

function umtx_pi_claim(pi:p_umtx_pi;owner:p_kthread):Integer;
var
 uq:p_umtx_q;
 pri:Integer;
begin
 mtx_lock(umtx_lock);

 if (pi^.pi_owner=owner) then
 begin
  mtx_unlock(umtx_lock);
  Exit (0);
 end;

 if (pi^.pi_owner<>nil) then
 begin
  mtx_unlock(umtx_lock);
  Exit(EPERM);
 end;

 umtx_pi_setowner(pi, owner);
 uq:=TAILQ_FIRST(@pi^.pi_blocked);

 if (uq<>nil) then
 begin
  pri:=UPRI(uq^.uq_thread);
  thread_lock(owner);
  if (pri < UPRI(owner)) then
  begin
   sched_lend_user_prio(owner, pri);
  end;
  thread_unlock(owner);
 end;
 mtx_unlock(umtx_lock);
 Exit(0);
end;

procedure umtx_pi_adjust(td:p_kthread;oldpri:Integer);
var
 uq:p_umtx_q;
 pi:p_umtx_pi;
begin
 uq:=td^.td_umtxq;
 mtx_lock(umtx_lock);

 pi:=uq^.uq_pi_blocked;
 if (pi<>nil) then
 begin
  umtx_pi_adjust_thread(pi, td);
  umtx_repropagate_priority(pi);
 end;
 mtx_unlock(umtx_lock);
end;

function umtxq_sleep_pi(uq:p_umtx_q;pi:p_umtx_pi;
                        owner:DWORD;wmesg:pchar;timo:Int64):Integer;
var
 uc:p_umtxq_chain;
 td,td1:p_kthread;
 uq1:p_umtx_q;
 pri:Integer;
begin
 td:=uq^.uq_thread;
 Assert(td=curkthread,'inconsistent uq_thread');

 uc:=umtxq_getchain(@uq^.uq_key);
 UMTXQ_LOCKED_ASSERT(uc);
 UMTXQ_BUSY_ASSERT(uc);
 umtxq_insert(uq);

 mtx_lock(umtx_lock);
 if (pi^.pi_owner=nil) then
 begin
  mtx_unlock(umtx_lock);

  td1:=tdfind(owner);

  mtx_lock(umtx_lock);
  if (td1<>nil) then
  begin
   if (pi^.pi_owner=nil) then
   begin
    umtx_pi_setowner(pi, td1);
   end;
   //
   thread_dec_ref(td1);
  end;
 end;

 uq1:=TAILQ_FIRST(@pi^.pi_blocked);
 while (uq1<>nil) do
 begin
  pri:=UPRI(uq1^.uq_thread);
  if (pri > UPRI(td)) then Break;
  //
  uq1:=TAILQ_NEXT(uq1,@uq1^.uq_lockq);
 end;

 if (uq1<>nil) then
 begin
  TAILQ_INSERT_BEFORE(uq1, uq, @uq^.uq_lockq);
 end else
 begin
  TAILQ_INSERT_TAIL(@pi^.pi_blocked, uq, @uq^.uq_lockq);
 end;

 uq^.uq_pi_blocked:=pi;

 thread_lock(td);
 td^.td_flags:=td^.td_flags or TDF_UPIBLOCKED;
 thread_unlock(td);

 umtx_propagate_priority(td);
 mtx_unlock(umtx_lock);
 umtxq_unbusy(@uq^.uq_key);

 if ((uq^.uq_flags and UQF_UMTXQ)<>0) then
 begin
  Result:=msleep(uq, @uc^.uc_lock, PCATCH, wmesg, timo);
  if (Result=EWOULDBLOCK) then
  begin
   Result:=ETIMEDOUT;
  end;
  if ((uq^.uq_flags and UQF_UMTXQ)<>0) then
  begin
   umtxq_remove(uq);
  end;
 end;
 mtx_lock(umtx_lock);

 uq^.uq_pi_blocked:=nil;

 thread_lock(td);
 td^.td_flags:=td^.td_flags and (not TDF_UPIBLOCKED);
 thread_unlock(td);

 TAILQ_REMOVE(@pi^.pi_blocked, uq, @uq^.uq_lockq);
 umtx_repropagate_priority(pi);

 mtx_unlock(umtx_lock);
 umtxq_unlock(@uq^.uq_key);
end;

procedure umtx_pi_ref(pi:p_umtx_pi);
var
 uc:p_umtxq_chain;
begin
 uc:=umtxq_getchain(@pi^.pi_key);
 UMTXQ_LOCKED_ASSERT(uc);
 Inc(pi^.pi_refcount);
end;

procedure umtx_pi_unref(pi:p_umtx_pi);
var
 uq:p_umtx_q;
 uc:p_umtxq_chain;
begin
 uc:=umtxq_getchain(@pi^.pi_key);

 UMTXQ_LOCKED_ASSERT(uc);
 Assert(pi^.pi_refcount > 0,'invalid reference count');

 Dec(pi^.pi_refcount);
 if (pi^.pi_refcount=0) then
 begin
  mtx_lock(umtx_lock);

  if (pi^.pi_owner<>nil) then
  begin
   uq:=pi^.pi_owner^.td_umtxq;
   TAILQ_REMOVE(@uq^.uq_pi_contested, pi, @pi^.pi_link);
   umtx_pi_clearowner(pi);
  end;

  Assert(TAILQ_EMPTY(@pi^.pi_blocked),'blocked queue not empty');
  mtx_unlock(umtx_lock);

  TAILQ_REMOVE(@uc^.uc_pi_list, pi, @pi^.pi_hashlink);

  umtx_pi_free(pi);
 end;
end;

function umtx_pi_lookup(key:p_umtx_key):p_umtx_pi;
var
 uc:p_umtxq_chain;
 pi:p_umtx_pi;
begin
 uc:=umtxq_getchain(key);
 UMTXQ_LOCKED_ASSERT(uc);

 pi:=TAILQ_FIRST(@uc^.uc_pi_list);
 while (pi<>nil) do
 begin
  if (umtx_key_match(@pi^.pi_key, key)<>0) then
  begin
   Exit(pi);
  end;
  //
  pi:=TAILQ_NEXT(pi,@pi^.pi_hashlink);
 end;

 Exit(nil);
end;

procedure umtx_pi_insert(pi:p_umtx_pi);
var
 uc:p_umtxq_chain;
begin
 uc:=umtxq_getchain(@pi^.pi_key);
 UMTXQ_LOCKED_ASSERT(uc);
 TAILQ_INSERT_TAIL(@uc^.uc_pi_list, pi, @pi^.pi_hashlink);
end;

function _do_lock_pi(td:p_kthread;m:p_umutex;flags:Integer;timo:Int64;mode:Integer):Integer;
var
 uq:p_umtx_q;
 pi,new_pi:p_umtx_pi;
 id,owner,old:DWORD;
begin
 Result:=0;

 id:=td^.td_tid;
 uq:=td^.td_umtxq;

 Result:=umtx_key_get(m, TYPE_PI_UMUTEX, GET_SHARE(flags), @uq^.uq_key);
 if (Result<>0) then Exit;

 umtxq_lock(@uq^.uq_key);
 pi:=umtx_pi_lookup(@uq^.uq_key);

 if (pi=nil) then
 begin
  new_pi:=umtx_pi_alloc();

  if (new_pi=nil) then
  begin
   umtxq_unlock(@uq^.uq_key);
   new_pi:=umtx_pi_alloc();
   umtxq_lock(@uq^.uq_key);

   pi:=umtx_pi_lookup(@uq^.uq_key);
   if (pi<>nil) then
   begin
    umtx_pi_free(new_pi);
    new_pi:=nil;
   end;
  end;

  if (new_pi<>nil) then
  begin
   new_pi^.pi_key:=uq^.uq_key;
   umtx_pi_insert(new_pi);
   pi:=new_pi;
  end;
 end;

 umtx_pi_ref(pi);
 umtxq_unlock(@uq^.uq_key);

 while (true) do
 begin
  owner:=casuword32(m^.m_owner, UMUTEX_UNOWNED, id);

  if (owner=UMUTEX_UNOWNED) then
  begin
   Result:=0;
   Break;
  end;

  if (owner=DWORD(-1)) then
  begin
   Result:=EFAULT;
   Break;
  end;

  if (owner=UMUTEX_CONTESTED) then
  begin
   owner:=casuword32(m^.m_owner, UMUTEX_CONTESTED, id or UMUTEX_CONTESTED);

   if (owner=UMUTEX_CONTESTED) then
   begin
    umtxq_lock(@uq^.uq_key);
    umtxq_busy(@uq^.uq_key);
    Result:=umtx_pi_claim(pi, td);
    umtxq_unbusy(@uq^.uq_key);
    umtxq_unlock(@uq^.uq_key);
    Break;
   end;

   if (owner=DWORD(-1)) then
   begin
    Result:=EFAULT;
    Break;
   end;

   continue;
  end;

  if ((flags and UMUTEX_ERROR_CHECK)<>0) and
     ((owner and (not UMUTEX_CONTESTED))=id) then
  begin
   Result:=EDEADLK;
   Break;
  end;

  if (mode<>0) then
  begin
   Result:=EBUSY;
   Break;
  end;

  if (Result<>0) then Break;

  umtxq_lock  (@uq^.uq_key);
  umtxq_busy  (@uq^.uq_key);
  umtxq_unlock(@uq^.uq_key);

  old:=casuword32(m^.m_owner, owner, owner or UMUTEX_CONTESTED);

  if (old=DWORD(-1)) then
  begin
   umtxq_lock  (@uq^.uq_key);
   umtxq_unbusy(@uq^.uq_key);
   umtxq_unlock(@uq^.uq_key);
   Result:=EFAULT;
   Break;
  end;

  umtxq_lock(@uq^.uq_key);

  if (old=owner) then
  begin
   Result:=umtxq_sleep_pi(uq, pi, owner and (not UMUTEX_CONTESTED),'umtxpi', timo);
  end else
  begin
   umtxq_unbusy(@uq^.uq_key);
   umtxq_unlock(@uq^.uq_key);
  end;

  Result:=umtxq_check_susp(td);
  if (Result<>0) then Break;
 end;

 umtxq_lock   (@uq^.uq_key);
 umtx_pi_unref(pi);
 umtxq_unlock (@uq^.uq_key);

 umtx_key_release(@uq^.uq_key);
end;

function do_unlock_pi(td:p_kthread;m:p_umutex;flags:Integer):Integer;
var
 cur:p_kthread;
 key:umtx_key;
 uq_first,uq_first2,uq_me:p_umtx_q;
 pi,pi2:p_umtx_pi;
 owner,old,id,t:DWORD;
 count,pri:Integer;
begin
 Result:=0;

 cur:=curkthread;
 if (cur=nil) then Exit(-1);

 id:=td^.td_tid;

 owner:=fuword32(m^.m_owner);

 if (owner=DWORD(-1)) then
 begin
  Exit(EFAULT);
 end;

 if ((owner and (not UMUTEX_CONTESTED))<>id) then
 begin
  Exit(EPERM);
 end;

 if ((owner and UMUTEX_CONTESTED)=0) then
 begin
  old:=casuword32(m^.m_owner, owner, UMUTEX_UNOWNED);

  if (old=DWORD(-1)) then
  begin
   Exit(EFAULT);
  end;

  if (old=owner) then
  begin
   Exit(0);
  end;

  owner:=old;
 end;

 Result:=umtx_key_get(m, TYPE_PI_UMUTEX, GET_SHARE(flags), @key);
 if (Result<>0) then Exit;

 umtxq_lock(@key);
 umtxq_busy(@key);
 count:=umtxq_count_pi(@key, @uq_first);

 if (uq_first<>nil) then
 begin
  mtx_lock(umtx_lock);
  pi:=uq_first^.uq_pi_blocked;
  Assert(pi<>nil,'pi=nil?');
  if (pi^.pi_owner<>cur) then
  begin
   mtx_unlock(umtx_lock);
   umtxq_unbusy(@key);
   umtxq_unlock(@key);
   umtx_key_release(@key);

   Exit(EPERM);
  end;
  uq_me:=cur^.td_umtxq;

  umtx_pi_clearowner(pi);

  TAILQ_REMOVE(@uq_me^.uq_pi_contested, pi, @pi^.pi_link);

  uq_first:=TAILQ_FIRST(@pi^.pi_blocked);
  while (uq_first<>nil) and
        ((uq_first^.uq_flags and UQF_UMTXQ)=0) do
  begin
   uq_first:=TAILQ_NEXT(uq_first, @uq_first^.uq_lockq);
  end;

  pri:=PRI_MAX;

  pi2:=TAILQ_FIRST(@uq_me^.uq_pi_contested);
  while (pi2<>nil) do
  begin
   uq_first2:=TAILQ_FIRST(@pi2^.pi_blocked);
   if (uq_first2<>nil) then
   begin
    if (pri > UPRI(uq_first2^.uq_thread)) then
    begin
     pri:=UPRI(uq_first2^.uq_thread);
    end;
   end;
   //
   pi2:=TAILQ_NEXT(pi2,@pi2^.pi_link);
  end;

  thread_lock(cur);
  sched_lend_user_prio(cur, pri);
  thread_unlock(cur);

  mtx_unlock(umtx_lock);
  if (uq_first<>nil) then
  begin
   umtxq_signal_thread(uq_first);
  end;
 end;
 umtxq_unlock(@key);

 if (count<=1) then
 begin
  t:=UMUTEX_UNOWNED;
 end else
 begin
  t:=UMUTEX_CONTESTED;
 end;

 old:=casuword32(m^.m_owner, owner, t);

 umtxq_lock  (@key);
 umtxq_unbusy(@key);
 umtxq_unlock(@key);

 umtx_key_release(@key);

 if (old=DWORD(-1)) then
 begin
  Exit(EFAULT);
 end;

 if (old<>owner) then
 begin
  Exit(EINVAL);
 end;

 Exit(0);
end;

//

function _do_lock_pp(td:p_kthread;m:p_umutex;flags:Integer;timo:Int64;mode:Integer):Integer;
label
 _out;
var
 uq,uq2:p_umtx_q;
 pi:p_umtx_pi;
 ceiling,owner,id:DWORD;
 pri,old_inherited_pri,su:Integer;
begin
 Result:=0;

 id:=td^.td_tid;
 uq:=td^.td_umtxq;

 Result:=umtx_key_get(m, TYPE_PP_UMUTEX, GET_SHARE(flags), @uq^.uq_key);
 if (Result<>0) then Exit;

 su:=1;
 //su:=(priv_check(td, PRIV_SCHED_RTPRIO)=0);

 while (true) do
 begin
  old_inherited_pri:=uq^.uq_inherited_pri;
  umtxq_lock(@uq^.uq_key);
  umtxq_busy(@uq^.uq_key);
  umtxq_unlock(@uq^.uq_key);

  ceiling:=RTP_PRIO_MAX - fuword32(m^.m_ceilings[0]);
  if (ceiling > RTP_PRIO_MAX) then
  begin
   Result:=EINVAL;
   goto _out;
  end;

  mtx_lock(umtx_lock);

  if (UPRI(td) < (PRI_MIN_REALTIME + ceiling)) then
  begin
   mtx_unlock(umtx_lock);
   Result:=EINVAL;
   goto _out;
  end;

  if (su<>0) and ((PRI_MIN_REALTIME + ceiling) < uq^.uq_inherited_pri) then
  begin
   uq^.uq_inherited_pri:=PRI_MIN_REALTIME + ceiling;
   thread_lock(td);
   if (uq^.uq_inherited_pri < UPRI(td)) then
   begin
    sched_lend_user_prio(td, uq^.uq_inherited_pri);
   end;
   thread_unlock(td);
  end;
  mtx_unlock(umtx_lock);

  owner:=casuword32(m^.m_owner, UMUTEX_CONTESTED, id or UMUTEX_CONTESTED);

  if (owner=UMUTEX_CONTESTED) then
  begin
   Result:=0;
   Break;
  end;

  if (owner=DWORD(-1)) then
  begin
   Result:=EFAULT;
   Break;
  end;

  if ((flags and UMUTEX_ERROR_CHECK)<>0) and
     ((owner and (not UMUTEX_CONTESTED))=id) then
  begin
   Result:=EDEADLK;
   Break;
  end;

  if (mode<>0) then
  begin
   Result:=EBUSY;
   Break;
  end;

  if (Result<>0) then Break;

  umtxq_lock  (@uq^.uq_key);
  umtxq_insert(uq);
  umtxq_unbusy(@uq^.uq_key);
  Result:=umtxq_sleep(uq, 'umtxpp', timo);
  umtxq_remove(uq);
  umtxq_unlock(@uq^.uq_key);

  mtx_lock(umtx_lock);
  uq^.uq_inherited_pri:=old_inherited_pri;
  pri:=PRI_MAX;

  pi:=TAILQ_FIRST(@uq^.uq_pi_contested);
  while (pi<>nil) do
  begin
   uq2:=TAILQ_FIRST(@pi^.pi_blocked);
   if (uq2<>nil) then
   begin
    if (pri > UPRI(uq2^.uq_thread)) then
    begin
     pri:=UPRI(uq2^.uq_thread);
    end;
   end;
   //
   pi:=TAILQ_NEXT(pi,@pi^.pi_link);
  end;

  if (pri > uq^.uq_inherited_pri) then
  begin
   pri:=uq^.uq_inherited_pri;
  end;

  thread_lock(td);
  sched_lend_user_prio(td, pri);
  thread_unlock(td);

  mtx_unlock(umtx_lock);
 end;

 if (Result<>0) then
 begin
  mtx_lock(umtx_lock);

  uq^.uq_inherited_pri:=old_inherited_pri;
  pri:=PRI_MAX;

  pi:=TAILQ_FIRST(@uq^.uq_pi_contested);
  while (pi<>nil) do
  begin
   uq2:=TAILQ_FIRST(@pi^.pi_blocked);
   if (uq2<>nil) then
   begin
    if (pri > UPRI(uq2^.uq_thread)) then
    begin
     pri:=UPRI(uq2^.uq_thread);
    end;
   end;
   //
   pi:=TAILQ_NEXT(pi,@pi^.pi_link);
  end;

  if (pri > uq^.uq_inherited_pri) then
  begin
   pri:=uq^.uq_inherited_pri;
  end;

  thread_lock(td);
  sched_lend_user_prio(td, pri);
  thread_unlock(td);

  mtx_unlock(umtx_lock);
 end;

_out:
 umtxq_lock  (@uq^.uq_key);
 umtxq_unbusy(@uq^.uq_key);
 umtxq_unlock(@uq^.uq_key);

 umtx_key_release(@uq^.uq_key);
end;

function do_unlock_pp(td:p_kthread;m:p_umutex;flags:Integer):Integer;
var
 key:umtx_key;
 uq,uq2:p_umtx_q;
 pi:p_umtx_pi;
 owner,id,rceiling:DWORD;
 pri,new_inherited_pri,su:Integer;
begin
 Result:=0;

 id:=td^.td_tid;
 uq:=td^.td_umtxq;

 su:=1;
 //su:=(priv_check(td, PRIV_SCHED_RTPRIO)=0);

 owner:=fuword32(m^.m_owner);

 if (owner=DWORD(-1)) then
 begin
  Exit(EFAULT);
 end;

 if ((owner and (not UMUTEX_CONTESTED))<>id) then
 begin
  Exit(EPERM);
 end;

 Result:=copyin(@m^.m_ceilings[1], @rceiling, sizeof(DWORD));
 if (Result<>0) then Exit;

 if (rceiling=-1) then
 begin
  new_inherited_pri:=PRI_MAX;
 end else
 begin
  rceiling:=RTP_PRIO_MAX - rceiling;
  if (rceiling > RTP_PRIO_MAX) then
  begin
   Exit (EINVAL);
  end;
  new_inherited_pri:=PRI_MIN_REALTIME + rceiling;
 end;

 Result:=umtx_key_get(m, TYPE_PP_UMUTEX, GET_SHARE(flags), @key);
 if (Result<>0) then Exit;

 umtxq_lock  (@key);
 umtxq_busy  (@key);
 umtxq_unlock(@key);

 Result:=suword32(m^.m_owner, UMUTEX_CONTESTED);

 umtxq_lock(@key);
 if (Result=0) then
 begin
  umtxq_signal(@key, 1);
 end;
 umtxq_unbusy(@key);
 umtxq_unlock(@key);

 if (Result=-1) then
 begin
  Result:=EFAULT;
 end else
 begin
  mtx_lock(umtx_lock);
  if (su<>0) then
  begin
   uq^.uq_inherited_pri:=new_inherited_pri;
  end;

  pri:=PRI_MAX;

  pi:=TAILQ_FIRST(@uq^.uq_pi_contested);
  while (pi<>nil) do
  begin
   uq2:=TAILQ_FIRST(@pi^.pi_blocked);
   if (uq2<>nil) then
   begin
    if (pri > UPRI(uq2^.uq_thread)) then
    begin
     pri:=UPRI(uq2^.uq_thread);
    end;
   end;
   //
   pi:=TAILQ_NEXT(pi,@pi^.pi_link);
  end;

  if (pri > uq^.uq_inherited_pri) then
  begin
   pri:=uq^.uq_inherited_pri;
  end;

  thread_lock(td);
  sched_lend_user_prio(td, pri);
  thread_unlock(td);

  mtx_unlock(umtx_lock);
 end;

 umtx_key_release(@key);
end;

function do_set_ceiling(td:p_kthread;m:p_umutex;ceiling:DWORD;old_ceiling:PDWORD):Integer;
var
 uq:p_umtx_q;
 save_ceiling:DWORD;
 id,owner:DWORD;
 flags:DWORD;
begin
 Result:=0;

 flags:=fuword32(m^.m_flags);

 if ((flags and UMUTEX_PRIO_PROTECT)=0) then Exit(EINVAL);
 if (ceiling>PRI_MAX) then Exit(EINVAL);

 id:=td^.td_tid;
 uq:=td^.td_umtxq;

 Result:=umtx_key_get(m, TYPE_PP_UMUTEX, GET_SHARE(flags), @uq^.uq_key);
 if (Result<>0) then Exit;

 repeat
  umtxq_lock  (@uq^.uq_key);
  umtxq_busy  (@uq^.uq_key);
  umtxq_unlock(@uq^.uq_key);

  save_ceiling:=fuword32(m^.m_ceilings[0]);

  owner:=fuword32(m^.m_owner);

  if (owner=DWORD(-1)) then
  begin
   Result:=EFAULT;
   Break;
  end;

  owner:=casuword32(m^.m_owner,UMUTEX_CONTESTED,id or UMUTEX_CONTESTED);

  if (owner=DWORD(-1)) then
  begin
   Exit(EFAULT);
  end;

  if (owner=UMUTEX_CONTESTED) then
  begin
   suword32(m^.m_ceilings[0],ceiling);
   suword32(m^.m_owner,UMUTEX_CONTESTED);
   Result:=0;
   Break;
  end;

  if ((owner and (not UMUTEX_CONTESTED))=id) then
  begin
   suword32(m^.m_ceilings[0],ceiling);
   Result:=0;
   Break;
  end;

  if (Result<>0) then Break;

  umtxq_lock  (@uq^.uq_key);
  umtxq_insert(uq);
  umtxq_unbusy(@uq^.uq_key);

  Result:=umtxq_sleep(uq, 'umtxpp', 0);

  umtxq_remove(uq);
  umtxq_unlock(@uq^.uq_key);
 until false;

 umtxq_lock(@uq^.uq_key);

 if (Result=0) then
 begin
  umtxq_signal(@uq^.uq_key,High(Integer));
 end;

 umtxq_unbusy(@uq^.uq_key);
 umtxq_unlock(@uq^.uq_key);

 umtx_key_release(@uq^.uq_key);

 if (Result=0) and (old_ceiling<>nil) then
 begin
  suword32(old_ceiling^,save_ceiling);
 end;
end;

//////

function _do_lock_umutex(td:p_kthread;m:p_umutex;flags:Integer;timo:Int64;mode:Integer):Integer;
begin
 Case (flags and (UMUTEX_PRIO_INHERIT or UMUTEX_PRIO_PROTECT)) of
                    0:Result:=_do_lock_normal(td,m,flags,timo,mode);
  UMUTEX_PRIO_INHERIT:Result:=_do_lock_pi(td,m,flags,timo,mode);
  UMUTEX_PRIO_PROTECT:Result:=_do_lock_pp(td,m,flags,timo,mode);
  else
   Exit(EINVAL);
 end;
end;

function do_lock_umutex(td:p_kthread;m:p_umutex;timeout:p_timespec;mode:Integer):Integer;
var
 flags:DWORD;
 tv,ts,ts2:Int64;
begin
 Result:=0;

 flags:=fuword32(m^.m_flags);

 if (timeout=nil) then
 begin
  Result:=_do_lock_umutex(td,m,flags,0,mode);
  if (Result=EINTR) and (mode<>_UMUTEX_WAIT) then
  begin
   Result:=ERESTART;
  end;
 end else
 begin
  tv:=TIMESPEC_TO_UNIT(timeout);
  ts:=get_unit_uptime;
  ts:=ts+tv;

  repeat
   Result:=_do_lock_umutex(td,m,flags,tvtohz(tv),mode);
   if (Result<>ETIMEDOUT) then Break;

   ts2:=get_unit_uptime;
   if (ts2>=ts) then
   begin
    Result:=ETIMEDOUT;
    Break;
   end;

   tv:=ts-ts2;
  until false;

  if (Result=ERESTART) then
  begin
   Result:=EINTR;
  end;
 end;
end;

function do_unlock_umutex(td:p_kthread;m:p_umutex):Integer;
var
 flags:DWORD;
begin
 Result:=0;

 flags:=fuword32(m^.m_flags);

 if (flags=DWORD(-1)) then
 begin
  Exit(EFAULT);
 end;

 Case (flags and (UMUTEX_PRIO_INHERIT or UMUTEX_PRIO_PROTECT)) of
                    0:Result:=do_unlock_normal(td,m,flags);
  UMUTEX_PRIO_INHERIT:Result:=do_unlock_pi(td,m,flags);
  UMUTEX_PRIO_PROTECT:Result:=do_unlock_pp(td,m,flags);
  else
   Exit(EINVAL);
 end;
end;

////

function do_cv_wait(td:p_kthread;cv:p_ucond;m:p_umutex;timeout:p_timespec;wflags:QWORD):Integer;
var
 uq:p_umtx_q;
 clockid:Integer;
 oldlen:Integer;
 flags:Integer;
 cts,ets,tts,tv:Int64;
begin
 Result:=0;

 uq:=td^.td_umtxq;

 flags:=fuword32(cv^.c_flags);

 Result:=umtx_key_get(cv, TYPE_CV, GET_SHARE(flags), @uq^.uq_key);
 if (Result<>0) then Exit;

 if ((wflags and CVWAIT_CLOCKID)<>0) then
 begin
  clockid:=fuword32(cv^.c_clockid);
  if (clockid<CLOCK_REALTIME) or
     (clockid>=CLOCK_THREAD_CPUTIME_ID) then
  begin
   umtx_key_release(@uq^.uq_key);
   Exit(EINVAL);
  end;
 end else
 begin
  clockid:=CLOCK_REALTIME;
 end;

 umtxq_lock  (@uq^.uq_key);
 umtxq_busy  (@uq^.uq_key);
 umtxq_insert(uq);
 umtxq_unlock(@uq^.uq_key);

 if (fuword32(cv^.c_has_waiters)=0) then
 begin
  suword32(cv^.c_has_waiters,1);
 end;

 umtxq_lock  (@uq^.uq_key);
 umtxq_unbusy(@uq^.uq_key);
 umtxq_unlock(@uq^.uq_key);

 Result:=do_unlock_umutex(td,m);

 umtxq_lock(@uq^.uq_key);

 if (Result=0) then
 begin
  if (timeout=nil) then
  begin
   Result:=umtxq_sleep(uq, 'ucond', 0);
  end else
  begin
   if ((wflags and CVWAIT_ABSTIME)=0) then
   begin
    ets:=0;
    kern_clock_gettime_unit(clockid,@ets);

    tts:=TIMESPEC_TO_UNIT(timeout);
    ets:=ets+tts;
   end else
   begin
    ets:=TIMESPEC_TO_UNIT(timeout);
    tts:=ets;

    cts:=0;
    kern_clock_gettime_unit(clockid,@cts);

    tts:=tts-cts;
   end;

   tv:=tts;
   repeat
    Result:=umtxq_sleep(uq, 'ucond', tvtohz(tv));
    if (Result<>ETIMEDOUT) then Break;

    kern_clock_gettime_unit(clockid,@cts);

    if (cts>=ets) then
    begin
     Result:=ETIMEDOUT;
     Break;
    end;

    tts:=ets;
    tts:=tts-cts;
    tv :=tts;
   until false;
  end;
 end;

 if ((uq^.uq_flags and UQF_UMTXQ)=0) then
 begin
  Result:=0;
 end else
 begin
  umtxq_busy(@uq^.uq_key);

  if ((uq^.uq_flags and UQF_UMTXQ)<>0) then
  begin
   oldlen:=uq^.uq_cur_queue^.length;
   umtxq_remove(uq);

   if (oldlen=1) then
   begin
    umtxq_unlock(@uq^.uq_key);
    suword32(cv^.c_has_waiters,0);
    umtxq_lock(@uq^.uq_key);
   end;
  end;

  umtxq_unbusy(@uq^.uq_key);

  if (Result=ERESTART) then
  begin
   Result:=EINTR;
  end;
 end;

 umtxq_unlock(@uq^.uq_key);

 umtx_key_release(@uq^.uq_key);
end;

function do_cv_signal(td:p_kthread;cv:p_ucond):Integer;
var
 key:umtx_key;
 count,nwake,flags:Integer;
begin
 Result:=0;

 flags:=fuword32(cv^.c_flags);

 Result:=umtx_key_get(cv, TYPE_CV, GET_SHARE(flags), @key);
 if (Result<>0) then Exit;

 umtxq_lock(@key);
 umtxq_busy(@key);

 count:=umtxq_count (@key);
 nwake:=umtxq_signal(@key, 1);

 if (count<=nwake) then
 begin
  umtxq_unlock(@key);
  Result:=suword32(cv^.c_has_waiters,0);
  umtxq_lock(@key);
  if (Result<>0) then Result:=EFAULT;
 end;

 umtxq_unbusy(@key);
 umtxq_unlock(@key);

 umtx_key_release(@key);
end;

function do_cv_broadcast(td:p_kthread;cv:p_ucond):Integer;
var
 key:umtx_key;
 flags:Integer;
begin
 Result:=0;

 flags:=fuword32(cv^.c_flags);

 Result:=umtx_key_get(cv, TYPE_CV, GET_SHARE(flags), @key);
 if (Result<>0) then Exit;

 umtxq_lock  (@key);
 umtxq_busy  (@key);
 umtxq_signal(@key, High(Integer));
 umtxq_unlock(@key);

 Result:=suword32(cv^.c_has_waiters,0);
 if (Result<>0) then Result:=EFAULT;

 umtxq_lock  (@key);
 umtxq_unbusy(@key);
 umtxq_unlock(@key);

 umtx_key_release(@key);
end;

////

function do_rw_rdlock(td:p_kthread;rwlock:p_urwlock;fflag:QWORD;timo:Int64):Integer;
label
 _sleep;
var
 uq:p_umtx_q;
 flags,wrflags:DWORD;
 state,oldstate:Integer;
 blocked_readers:Integer;
begin
 Result:=0;

 uq:=td^.td_umtxq;

 flags:=fuword32(rwlock^.rw_flags);

 Result:=umtx_key_get(rwlock, TYPE_RWLOCK, GET_SHARE(flags), @uq^.uq_key);
 if (Result<>0) then Exit;

 wrflags:=URWLOCK_WRITE_OWNER;

 if ((fflag and URWLOCK_PREFER_READER)=0) and ((flags and URWLOCK_PREFER_READER)=0) then
 begin
  wrflags:=wrflags or URWLOCK_WRITE_WAITERS;
 end;

 repeat
  state:=fuword32(rwlock^.rw_state);

  While ((state and wrflags)=0) do
  begin
   if (URWLOCK_READER_COUNT(state)=URWLOCK_MAX_READERS) then
   begin
    umtx_key_release(@uq^.uq_key);
    Exit(EAGAIN);
   end;

   oldstate:=casuword32(rwlock^.rw_state,state,state+1);

   if (oldstate=-1) then
   begin
    umtx_key_release(@uq^.uq_key);
    Exit(EFAULT);
   end;

   if (oldstate=state) then
   begin
    umtx_key_release(@uq^.uq_key);
    Exit(0);
   end;

   state:=oldstate;
  end;

  if (Result<>0) then Break;

  umtxq_lock  (@uq^.uq_key);
  umtxq_busy  (@uq^.uq_key);
  umtxq_unlock(@uq^.uq_key);

  state:=fuword32(rwlock^.rw_state);

  While ((state and wrflags)<>0) and ((state and URWLOCK_READ_WAITERS)=0) do
  begin
   oldstate:=casuword32(rwlock^.rw_state,state,state or URWLOCK_READ_WAITERS);

   if (oldstate=-1) then
   begin
    Result:=EFAULT;
    Break;
   end;

   if (oldstate=state) then
   begin
    goto _sleep;
   end;

   state:=oldstate;
  end;

  if (Result<>0) then
  begin
   umtxq_lock  (@uq^.uq_key);
   umtxq_unbusy(@uq^.uq_key);
   umtxq_unlock(@uq^.uq_key);
   Break;
  end;

  if ((state and wrflags)=0) then
  begin
   umtxq_lock  (@uq^.uq_key);
   umtxq_unbusy(@uq^.uq_key);
   umtxq_unlock(@uq^.uq_key);

   Continue;
  end;

  _sleep:

  blocked_readers:=fuword32(rwlock^.rw_blocked_readers);
  suword32(rwlock^.rw_blocked_readers,blocked_readers+1);

  While ((state and wrflags)<>0) do
  begin
   umtxq_lock  (@uq^.uq_key);
   umtxq_insert(uq);
   umtxq_unbusy(@uq^.uq_key);

   Result:= umtxq_sleep(uq, 'urdlck', timo);

   umtxq_busy  (@uq^.uq_key);
   umtxq_remove(uq);
   umtxq_unlock(@uq^.uq_key);

   if (Result<>0) then Break;

   state:=fuword32(rwlock^.rw_state);
  end;

  blocked_readers:=fuword32(rwlock^.rw_blocked_readers);
  suword32(rwlock^.rw_blocked_readers,blocked_readers-1);

  if (blocked_readers=1) then
  begin
   state:=fuword32(rwlock^.rw_state);
   repeat
    oldstate:=casuword32(rwlock^.rw_state,state,state and (not URWLOCK_READ_WAITERS));

    if (oldstate=-1) then
    begin
     Result:=EFAULT;
     Break;
    end;

    if (oldstate=state) then
    begin
     Break;
    end;

    state:=oldstate;
   until false;
  end;

  umtxq_lock  (@uq^.uq_key);
  umtxq_unbusy(@uq^.uq_key);
  umtxq_unlock(@uq^.uq_key);

 until (Result<>0);

 umtx_key_release(@uq^.uq_key);
end;

function do_rw_rdlock2(td:p_kthread;rwlock:p_urwlock;fflag:QWORD;timeout:p_timespec):Integer;
var
 ts,ts2,tv:Int64;
begin
 Result:=0;

 ts:=get_unit_uptime;
 ts:=ts+TIMESPEC_TO_UNIT(timeout);
 tv:=ts;

 repeat
  Result:=do_rw_rdlock(td,rwlock,fflag,tvtohz(tv));

  if (Result<>ETIMEDOUT) then Break;

  ts2:=get_unit_uptime;

  if (ts2>=ts) then
  begin
   Result:=ETIMEDOUT;
   Break;
  end;

  tv:=ts-ts2;
 until false;

 if (Result=ERESTART) then
 begin
  Result:=EINTR;
 end;
end;

function do_rw_wrlock(td:p_kthread;rwlock:p_urwlock;fflag:QWORD;timo:Int64):Integer;
label
 _sleep;
var
 uq:p_umtx_q;
 flags:Integer;
 state,oldstate:Integer;
 blocked_writers:Integer;
 blocked_readers:Integer;
begin
 Result:=0;

 uq:=td^.td_umtxq;

 flags:=fuword32(rwlock^.rw_flags);

 Result:=umtx_key_get(rwlock, TYPE_RWLOCK, GET_SHARE(flags), @uq^.uq_key);
 if (Result<>0) then Exit;

 blocked_readers:=0;

 repeat
  state:=fuword32(rwlock^.rw_state);

  while ((state and URWLOCK_WRITE_OWNER)=0) and (URWLOCK_READER_COUNT(state)=0) do
  begin
   oldstate:=casuword32(rwlock^.rw_state,state,state or URWLOCK_WRITE_OWNER);

   if (oldstate=-1) then
   begin
    umtx_key_release(@uq^.uq_key);
    Exit(EFAULT);
   end;

   if (oldstate=state) then
   begin
    umtx_key_release(@uq^.uq_key);
    Exit(0);
   end;

   state:=oldstate;
  end;

  if (Result<>0) then
  begin
   if ((state and (URWLOCK_WRITE_OWNER or URWLOCK_WRITE_WAITERS))=0) and
      (blocked_readers<>0) then
   begin
    umtxq_lock(@uq^.uq_key);
    umtxq_busy(@uq^.uq_key);
    umtxq_signal_queue(@uq^.uq_key, High(Integer), UMTX_SHARED_QUEUE);
    umtxq_unbusy(@uq^.uq_key);
    umtxq_unlock(@uq^.uq_key);
   end;
   Break;
  end;

  umtxq_lock  (@uq^.uq_key);
  umtxq_busy  (@uq^.uq_key);
  umtxq_unlock(@uq^.uq_key);

  state:=fuword32(rwlock^.rw_state);

  while (((state and URWLOCK_WRITE_OWNER)<>0) or (URWLOCK_READER_COUNT(state)<>0)) and
        ((state and URWLOCK_WRITE_WAITERS)=0) do
  begin
   oldstate:=casuword32(rwlock^.rw_state,state,state or URWLOCK_WRITE_WAITERS);

   if (oldstate=-1) then
   begin
    Result:=EFAULT;
    Break;
   end;

   if (oldstate=state) then
   begin
    goto _sleep;
   end;

   state:=oldstate;
  end;

  if (Result<>0) then
  begin
   umtxq_lock  (@uq^.uq_key);
   umtxq_unbusy(@uq^.uq_key);
   umtxq_unlock(@uq^.uq_key);
   Break;
  end;

  if ((state and URWLOCK_WRITE_OWNER)=0) and (URWLOCK_READER_COUNT(state)=0) then
  begin
   umtxq_lock  (@uq^.uq_key);
   umtxq_unbusy(@uq^.uq_key);
   umtxq_unlock(@uq^.uq_key);

   Continue;
  end;

  _sleep:

  blocked_writers:=fuword32(rwlock^.rw_blocked_writers);
  suword32(rwlock^.rw_blocked_writers,blocked_writers+1);

  While ((state and URWLOCK_WRITE_OWNER) or URWLOCK_READER_COUNT(state)<>0) do
  begin
   umtxq_lock(@uq^.uq_key);
   umtxq_insert_queue(uq, UMTX_EXCLUSIVE_QUEUE);
   umtxq_unbusy(@uq^.uq_key);

   Result:=umtxq_sleep(uq, 'uwrlck', timo);

   umtxq_busy(@uq^.uq_key);
   umtxq_remove_queue(uq, UMTX_EXCLUSIVE_QUEUE);
   umtxq_unlock(@uq^.uq_key);

   if (Result<>0) then Break;

   state:=fuword32(rwlock^.rw_state);
  end;

  blocked_writers:=fuword32(rwlock^.rw_blocked_writers);
  suword32(rwlock^.rw_blocked_writers,blocked_writers-1);

  if (blocked_writers=1) then
  begin
   state:=fuword32(rwlock^.rw_state);

   repeat
    oldstate:=casuword32(rwlock^.rw_state,state,state and (not URWLOCK_WRITE_WAITERS));

    if (oldstate=-1) then
    begin
     Result:=EFAULT;
     Break;
    end;

    if (oldstate=state) then
    begin
     Break;
    end;

    state:=oldstate;
   until (Result<>0);

   blocked_readers:=fuword32(rwlock^.rw_blocked_readers);
  end else
  begin
   blocked_readers:=0;
  end;

  umtxq_lock  (@uq^.uq_key);
  umtxq_unbusy(@uq^.uq_key);
  umtxq_unlock(@uq^.uq_key);

 until false;

 umtx_key_release(@uq^.uq_key);
end;

function do_rw_wrlock2(td:p_kthread;rwlock:p_urwlock;fflag:QWORD;timeout:p_timespec):Integer;
var
 ts,ts2,tv:Int64;
begin
 Result:=0;

 ts:=get_unit_uptime;
 ts:=ts+TIMESPEC_TO_UNIT(timeout);
 tv:=ts;

 repeat
  Result:=do_rw_wrlock(td,rwlock,fflag,tvtohz(tv));

  if (Result<>ETIMEDOUT) then Break;

  ts2:=get_unit_uptime;

  if (ts2>=ts) then
  begin
   Result:=ETIMEDOUT;
   Break;
  end;

  tv:=ts-ts2;
 until false;

 if (Result=ERESTART) then
 begin
  Result:=EINTR;
 end;
end;

function do_rw_unlock(td:p_kthread;rwlock:p_urwlock):Integer;
label
 _out;
var
 uq:p_umtx_q;
 flags:DWORD;
 state,oldstate:Integer;
 q,count:Integer;
begin
 Result:=0;

 uq:=td^.td_umtxq;

 flags:=fuword32(rwlock^.rw_flags);

 Result:=umtx_key_get(rwlock, TYPE_RWLOCK, GET_SHARE(flags), @uq^.uq_key);
 if (Result<>0) then Exit;

 state:=fuword32(rwlock^.rw_state);

 if ((state and URWLOCK_WRITE_OWNER)<>0) then
 begin
  repeat
   oldstate:=casuword32(rwlock^.rw_state,state,state and (not URWLOCK_WRITE_OWNER));

   if (oldstate=-1) then
   begin
    Result:=EFAULT;
    goto _out;
   end;

   if (oldstate<>state) then
   begin
    state:=oldstate;
    if ((oldstate and URWLOCK_WRITE_OWNER)=0) then
    begin
     Result:=EPERM;
     goto _out;
    end;
   end else
   begin
    Break;
   end;

  until false;
 end else
 if (URWLOCK_READER_COUNT(state)<>0) then
 begin
  repeat
   oldstate:=casuword32(rwlock^.rw_state,state,state-1);

   if (oldstate=-1) then
   begin
    Result:=EFAULT;
    goto _out;
   end;

   if (oldstate<>state) then
   begin
    state:=oldstate;
    if (URWLOCK_READER_COUNT(oldstate)=0) then
    begin
     Result:=EPERM;
     goto _out;
    end;
   end else
   begin
    Break;
   end;

  until false;
 end else
 begin
  Result:=EPERM;
  goto _out;
 end;

 count:=0;

 if ((flags and URWLOCK_PREFER_READER)=0) then
 begin
  if (state and URWLOCK_WRITE_WAITERS)<>0 then
  begin
   count:=1;
   q:=UMTX_EXCLUSIVE_QUEUE;
  end else
  if (state and URWLOCK_READ_WAITERS)<>0 then
  begin
   count:=High(Integer);
   q:=UMTX_SHARED_QUEUE;
  end;
 end else
 begin
  if ((state and URWLOCK_READ_WAITERS)<>0) then
  begin
   count:=High(Integer);
   q:=UMTX_SHARED_QUEUE;
  end else
  if ((state and URWLOCK_WRITE_WAITERS)<>0) then
  begin
   count:=1;
   q:=UMTX_EXCLUSIVE_QUEUE;
  end;
 end;

 if (count<>0) then
 begin
  umtxq_lock(@uq^.uq_key);
  umtxq_busy(@uq^.uq_key);
  umtxq_signal_queue(@uq^.uq_key, count, q);
  umtxq_unbusy(@uq^.uq_key);
  umtxq_unlock(@uq^.uq_key);
 end;

 _out:
  umtx_key_release(@uq^.uq_key);
end;

////

function do_sem_wait(td:p_kthread;sem:p__usem;timeout:p_timespec):Integer;
var
 uq:p_umtx_q;
 flags:Integer;
 count:DWORD;
 cts,ets,tv:Int64;
begin
 Result:=0;

 uq:=td^.td_umtxq;

 flags:=fuword32(sem^._flags);

 Result:=umtx_key_get(sem, TYPE_SEM, GET_SHARE(flags), @uq^.uq_key);
 if (Result<>0) then Exit;

 umtxq_lock  (@uq^.uq_key);
 umtxq_busy  (@uq^.uq_key);
 umtxq_insert(uq);
 umtxq_unlock(@uq^.uq_key);

 if ((fuword32(sem^._has_waiters))=0) then
 begin
  casuword32(sem^._has_waiters,0,1);
 end;

 count:=fuword32(sem^._count);

 if (count<>0) then
 begin
  umtxq_lock  (@uq^.uq_key);
  umtxq_unbusy(@uq^.uq_key);
  umtxq_remove(uq);
  umtxq_unlock(@uq^.uq_key);

  umtx_key_release(@uq^.uq_key);
  Exit(0);
 end;

 umtxq_lock  (@uq^.uq_key);
 umtxq_unbusy(@uq^.uq_key);
 umtxq_unlock(@uq^.uq_key);

 umtxq_lock(@uq^.uq_key);
 if (timeout=nil) then
 begin
  Result:=umtxq_sleep(uq, 'usem', 0);
 end else
 begin
  ets:=get_unit_uptime;
  tv:=TIMESPEC_TO_UNIT(timeout);
  ets:=ets+tv;

  repeat
   Result:=umtxq_sleep(uq, 'usem', tvtohz(tv));
   if (Result<>ETIMEDOUT) then Break;

   cts:=get_unit_uptime;

   if (cts>=ets) then
   begin
    Result:=ETIMEDOUT;
    Break;
   end;

   tv:=ets-cts;
  until false;
 end;

 if ((uq^.uq_flags and UQF_UMTXQ)=0) then
 begin
  Result:=0;
 end else
 begin
  umtxq_remove(uq);

  if (Result=ERESTART) and (timeout<>nil) then
  begin
   Result:=EINTR;
  end;
 end;

 umtxq_unlock(@uq^.uq_key);

 umtx_key_release(@uq^.uq_key);
end;

function do_sem_wake(td:p_kthread;sem:p__usem):Integer;
var
 key:umtx_key;
 flags:Integer;
 count,nwake:Integer;
begin
 Result:=0;

 flags:=fuword32(sem^._flags);

 Result:=umtx_key_get(sem, TYPE_SEM, GET_SHARE(flags), @key);
 if (Result<>0) then Exit;

 umtxq_lock(@key);
 umtxq_busy(@key);
 count:=umtxq_count (@key);
 nwake:=umtxq_signal(@key, 1);

 if (count<=nwake) then
 begin
  umtxq_unlock(@key);
  Result:=suword32(sem^._has_waiters,0);
  if (Result<>0) then Result:=EFAULT;
  umtxq_lock(@key);
 end;

 umtxq_unbusy(@key);
 umtxq_unlock(@key);

 umtx_key_release(@key);
end;

////

function umtx_copyin_timeout(addr:Pointer;tsp:p_timespec):Integer; public;
begin
 Result:=copyin(addr,tsp,SizeOf(timespec));
 if (Result=0) then
 begin
  if (tsp^.tv_sec < 0) or
     (tsp^.tv_nsec>=1000000000) or
     (tsp^.tv_nsec< 0) then
  begin
   Result:=EINVAL;
  end;
 end else
 begin
  Result:=EFAULT;
 end;
end;

function __umtx_op_lock_umtx(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 ts:p_timespec;
 timeout:timespec;
begin
 ts:=nil;
 if (uaddr2<>nil) then
 begin
  Result:=umtx_copyin_timeout(uaddr2,@timeout);
  if (Result<>0) then Exit;
  ts:=@timeout;
 end;
 Result:=do_lock_umtx(td,obj,val,ts);
end;

function __umtx_op_unlock_umtx(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; inline;
begin
 Result:=do_unlock_umtx(td,obj,val);
end;

function __umtx_op_wait(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 ts:p_timespec;
 timeout:timespec;
begin
 ts:=nil;
 if (uaddr2<>nil) then
 begin
  Result:=umtx_copyin_timeout(uaddr2,@timeout);
  if (Result<>0) then Exit;
  ts:=@timeout;
 end;
 Result:=do_wait(td,obj,val,ts,0,0);
end;

function __umtx_op_wait_uint(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 ts:p_timespec;
 timeout:timespec;
begin
 ts:=nil;
 if (uaddr2<>nil) then
 begin
  Result:=umtx_copyin_timeout(uaddr2,@timeout);
  if (Result<>0) then Exit;
  ts:=@timeout;
 end;
 Result:=do_wait(td,obj,val,ts,1,0);
end;

function __umtx_op_wait_uint_private(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 ts:p_timespec;
 timeout:timespec;
begin
 ts:=nil;
 if (uaddr2<>nil) then
 begin
  Result:=umtx_copyin_timeout(uaddr2,@timeout);
  if (Result<>0) then Exit;
  ts:=@timeout;
 end;
 Result:=do_wait(td,obj,val,ts,1,1);
end;

function __umtx_op_wake(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; inline;
begin
 Result:=kern_umtx_wake(td,obj,val,0);
end;

function __umtx_op_nwake_private(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
const
 BATCH_SIZE=128;
var
 count:Integer;
 tocopy:Integer;
 i,pos:Integer;
 uaddrs:array[0..BATCH_SIZE-1] of Pointer;
 upp:PPByte;
begin
 Result:=0;
 count :=val;
 upp   :=obj;
 i     :=0;
 pos   :=0;

 While (count>0) do
 begin
  tocopy:=count;
  if (tocopy>BATCH_SIZE) then
  begin
   tocopy:=BATCH_SIZE;
  end;

  Result:=copyin(upp+pos,@uaddrs,tocopy);
  if (Result<>0) then Exit;

  For i:=0 to tocopy-1 do
  begin
   kern_umtx_wake(td,uaddrs[i],High(Integer),1);
  end;

  count:=count-tocopy;
  pos  :=pos  +tocopy;
 end;
end;

function __umtx_op_wake_private(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; inline;
begin
 Result:=kern_umtx_wake(td,obj,val,1);
end;

function __umtx_op_lock_umutex(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 ts:p_timespec;
 timeout:timespec;
begin
 ts:=nil;
 if (uaddr2<>nil) then
 begin
  Result:=umtx_copyin_timeout(uaddr2,@timeout);
  if (Result<>0) then Exit;
  ts:=@timeout;
 end;
 Result:=do_lock_umutex(td,obj,ts,0);
end;

function __umtx_op_trylock_umutex(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; inline;
begin
 Result:=do_lock_umutex(td,obj,nil,_UMUTEX_TRY);
end;

function __umtx_op_wait_umutex(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 ts:p_timespec;
 timeout:timespec;
begin
 ts:=nil;
 if (uaddr2<>nil) then
 begin
  Result:=umtx_copyin_timeout(uaddr2,@timeout);
  if (Result<>0) then Exit;
  ts:=@timeout;
 end;
 Result:=do_lock_umutex(td,obj,ts,_UMUTEX_WAIT);
end;

function __umtx_op_wake_umutex(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; inline;
begin
 Result:=do_wake_umutex(td,obj);
end;

function __umtx_op_wake2_umutex(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; inline;
begin
 Result:=do_wake2_umutex(td,obj,val);
end;

function __umtx_op_unlock_umutex(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; inline;
begin
 Result:=do_unlock_umutex(td,obj);
end;

function __umtx_op_set_ceiling(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; inline;
begin
 Result:=do_set_ceiling(td,obj,val,uaddr1);
end;

function __umtx_op_cv_wait(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 ts:p_timespec;
 timeout:timespec;
begin
 ts:=nil;
 if (uaddr2<>nil) then
 begin
  Result:=umtx_copyin_timeout(uaddr2,@timeout);
  if (Result<>0) then Exit;
  ts:=@timeout;
 end;
 Result:=do_cv_wait(td,obj,uaddr1,ts,val);
end;

function __umtx_op_cv_signal(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; inline;
begin
 Result:=do_cv_signal(td,obj);
end;

function __umtx_op_cv_broadcast(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; inline;
begin
 Result:=do_cv_broadcast(td,obj);
end;

function __umtx_op_rw_rdlock(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 timeout:timespec;
begin

 if (uaddr2=nil) then
 begin
  Result:=do_rw_rdlock(td,obj,val,0);
 end else
 begin
  Result:=umtx_copyin_timeout(uaddr2,@timeout);
  if (Result<>0) then Exit;
  Result:=do_rw_rdlock2(td,obj,val,@timeout);
 end;
end;

function __umtx_op_rw_wrlock(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 timeout:timespec;
begin

 if (uaddr2=nil) then
 begin
  Result:=do_rw_wrlock(td,obj,val,0);
 end else
 begin
  Result:=umtx_copyin_timeout(uaddr2,@timeout);
  if (Result<>0) then Exit;
  Result:=do_rw_wrlock2(td,obj,val,@timeout);
 end;
end;

function __umtx_op_rw_unlock(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; inline;
begin
 Result:=do_rw_unlock(td,obj);
end;

function sys__umtx_lock(mtx:Pointer):Integer;
var
 td:p_kthread;
begin
 if (mtx=nil) then Exit(EINVAL);
 td:=curkthread;
 if (td=nil) then Exit(EFAULT);
 Result:=_do_lock_umtx(td,mtx,td^.td_tid,0);
end;

function sys__umtx_unlock(mtx:Pointer):Integer;
var
 td:p_kthread;
begin
 if (mtx=nil) then Exit(EINVAL);
 td:=curkthread;
 if (td=nil) then Exit(EFAULT);
 Result:=do_unlock_umtx(td,mtx,td^.td_tid);
end;

function __umtx_op_sem_wait(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 ts:p_timespec;
 timeout:timespec;
begin

 ts:=nil;
 if (uaddr2<>nil) then
 begin
  Result:=umtx_copyin_timeout(uaddr2,@timeout);
  if (Result<>0) then Exit;
  ts:=@timeout;
 end;
 Result:=do_sem_wait(td,obj,ts);
end;

function __umtx_op_sem_wake(td:p_kthread;obj:Pointer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; inline;
begin
 Result:=do_sem_wake(td,obj)
end;

function sys__umtx_op(obj:Pointer;op:Integer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 td:p_kthread;
begin
 if (obj=nil) then Exit(EINVAL);
 td:=curkthread;
 if (td=nil) then Exit(EFAULT);
 Case op of
  UMTX_OP_LOCK             :Result:=__umtx_op_lock_umtx        (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_UNLOCK           :Result:=__umtx_op_unlock_umtx      (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_WAIT             :Result:=__umtx_op_wait             (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_WAKE             :Result:=__umtx_op_wake             (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_MUTEX_TRYLOCK    :Result:=__umtx_op_trylock_umutex   (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_MUTEX_LOCK       :Result:=__umtx_op_lock_umutex      (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_MUTEX_UNLOCK     :Result:=__umtx_op_unlock_umutex    (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_SET_CEILING      :Result:=__umtx_op_set_ceiling      (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_CV_WAIT          :Result:=__umtx_op_cv_wait          (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_CV_SIGNAL        :Result:=__umtx_op_cv_signal        (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_CV_BROADCAST     :Result:=__umtx_op_cv_broadcast     (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_WAIT_UINT        :Result:=__umtx_op_wait_uint        (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_RW_RDLOCK        :Result:=__umtx_op_rw_rdlock        (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_RW_WRLOCK        :Result:=__umtx_op_rw_wrlock        (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_RW_UNLOCK        :Result:=__umtx_op_rw_unlock        (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_WAIT_UINT_PRIVATE:Result:=__umtx_op_wait_uint_private(td,obj,val,uaddr1,uaddr2);
  UMTX_OP_WAKE_PRIVATE     :Result:=__umtx_op_wake_private     (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_MUTEX_WAIT       :Result:=__umtx_op_wait_umutex      (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_MUTEX_WAKE       :Result:=__umtx_op_wake_umutex      (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_SEM_WAIT         :Result:=__umtx_op_sem_wait         (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_SEM_WAKE         :Result:=__umtx_op_sem_wake         (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_NWAKE_PRIVATE    :Result:=__umtx_op_nwake_private    (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_MUTEX_WAKE2      :Result:=__umtx_op_wake2_umutex     (td,obj,val,uaddr1,uaddr2);
  else
   Exit(EINVAL);
 end;

end;

procedure _umutex_init(mtx:p_umutex); inline;
begin
 mtx^:=Default(umutex);
end;


end.

