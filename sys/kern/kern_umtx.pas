unit kern_umtx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_rwlock,
 time,
 kern_time,
 kern_thr,
 _umtx,
 rtprio;

procedure _umutex_init(mtx:p_umutex); inline;

procedure umtx_thread_init(td:p_kthread);
procedure umtx_thread_exit(td:p_kthread);
procedure umtx_thread_fini(td:p_kthread);

function  sys__umtx_lock(mtx:p_umtx):Integer;
function  sys__umtx_unlock(mtx:p_umtx):Integer;
function  sys__umtx_op(obj:Pointer;op:Integer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;

//

function  kern_umtx_wake(td:p_kthread;umtx:p_umtx;n_wake,priv:Integer):Integer;

function  umtx_copyin_timeout(addr:Pointer;tsp:ptimespec):Integer;

procedure umtxq_sysinit; //SYSINIT

implementation

uses
 HAMT,
 errno,
 systm,
 trap,
 vm_machdep,
 kern_thread,
 sched_ule;

const
 _UMUTEX_TRY =1;
 _UMUTEX_WAIT=2;

 UQF_UMTXQ=$0001;

 UMTX_SHARED_QUEUE   =0;
 UMTX_EXCLUSIVE_QUEUE=1;

 BUSY_SPINS        =200;

type
 p_umtx_q     =^umtx_q;
 p_umtxq_queue=^umtxq_queue;
 p_umtxq_chain=^umtxq_chain;

 umtx_key     =p_umtxq_chain;

 //per thread local
 umtx_q=packed record
  uq_link:packed record
   pNext:p_umtx_q;
   pPrev:PPointer;
  end;
  //
  uq_key          :umtx_key;
  uq_flags        :Integer;
  uq_inherited_pri:Integer;
  uq_thread       :p_kthread;
  uq_handle       :THandle; //nt event
  uq_cur_queue    :p_umtxq_queue;
 end;

 //per mutex addr
 umtxq_queue=packed record
  head:packed record
   pFirst:p_umtx_q;
   pLast :PPointer;
  end;
  length  :Integer;
  align   :Integer;
 end;

 umtxq_chain=packed record
  uc_lock :Pointer;
  uc_owner:p_kthread;
  uc_queue:array[0..1] of umtxq_queue;
  uc_obj  :Pointer;
  uc_type :Integer;
  uc_refs :Integer;
 end;

 p_umtxq_hamt=^umtxq_hamt;
 umtxq_hamt=packed record
  hamt:TSTUB_HAMT64;
  lock:Pointer;
 end;

var
 umtxq_chains:array[0..8] of umtxq_hamt;

procedure umtxq_sysinit;
begin
 FillChar(umtxq_chains,SizeOf(umtxq_chains),0);
end;

function umtx_key_get(m:Pointer;ktype:Integer):umtx_key;
Var
 umtxq:p_umtxq_hamt;
 data:PPointer;
 new:umtx_key;
begin
 Result:=nil;
 umtxq:=@umtxq_chains[ktype];

 rw_rlock(umtxq^.lock);
  data:=HAMT_search64(@umtxq^.hamt,QWORD(m));
  if (data<>nil) then //EXIST
  begin
   Result:=data^;
   if (Result<>nil) then
   begin
    System.InterlockedIncrement(Result^.uc_refs); //GET
   end;
  end;
 rw_runlock(umtxq^.lock);

 if (Result<>nil) then Exit; //EXIST

 new:=AllocMem(SizeOf(umtxq_chain)); //NEW

 TAILQ_INIT(@new^.uc_queue[0].head);
 TAILQ_INIT(@new^.uc_queue[1].head);

 new^.uc_obj :=m;
 new^.uc_type:=ktype;

 rw_wlock(umtxq^.lock);
  data:=HAMT_insert64(@umtxq^.hamt,QWORD(m),new);
  if (data<>nil) then //NOMEM
  begin
   Result:=data^;
   if (Result<>nil) then
   begin
    System.InterlockedIncrement(Result^.uc_refs); //GET
   end;
  end;
 rw_wunlock(umtxq^.lock);

 if (data=nil) then //NOMEM
 begin
  FreeMem(new);
  Exit(nil);
 end;

 if (Result<>new) then //ANOTHER INSERT
 begin
  FreeMem(new);
 end;
end;

function umtx_key_remove(key:umtx_key):Boolean;
var
 m:Pointer;
 umtxq:p_umtxq_hamt;
 old:umtx_key;
begin
 Result:=False;

 m:=System.InterlockedExchange(key^.uc_obj,nil);
 if (m=nil) then Exit;

 umtxq:=@umtxq_chains[key^.uc_type];
 rw_wlock(umtxq^.lock);

  if (key^.uc_refs=0) and
     (key^.uc_owner=nil) and
     (key^.uc_queue[0].length=0) and
     (key^.uc_queue[1].length=0) then
  begin
   old:=nil;
   HAMT_delete64(@umtxq^.hamt,QWORD(m),@old);
   Result:=(old=key);
  end;

 rw_wunlock(umtxq^.lock);
end;

procedure umtx_key_release(var key:umtx_key);
var
 old:umtx_key;
begin
 old:=System.InterlockedExchange(key,nil);
 if (old=nil) then Exit;
 if (System.InterlockedDecrement(old^.uc_refs)=0) then
 if (old^.uc_owner=nil) then
 if umtx_key_remove(old) then
 begin
  FreeMem(old);
 end;
end;

function umtxq_queue_lookup(key:umtx_key;q:Integer):p_umtxq_queue; inline;
begin
 Result:=@key^.uc_queue[q];
end;

procedure umtxq_lock(key:umtx_key); inline;
begin
 rw_wlock(key^.uc_lock);
end;

procedure umtxq_unlock(key:umtx_key); inline;
begin
 rw_wunlock(key^.uc_lock);
end;

function umtxq_count(key:umtx_key):ptrint;
var
 uh:p_umtxq_queue;
begin
 Result:=0;
 uh:=umtxq_queue_lookup(key,UMTX_SHARED_QUEUE);
 if (uh<>nil) then Result:=uh^.length;
end;

function umtxq_count_pi(key:umtx_key;var first:p_umtx_q):ptrint;
var
 uh:p_umtxq_queue;
begin
 Result:=0;
 if (key=nil) then Exit;
 uh:=umtxq_queue_lookup(key,UMTX_SHARED_QUEUE);
 if (uh<>nil) then
 begin
  first :=TAILQ_FIRST(@uh^.head);
  Result:=uh^.length;
 end;
end;

procedure umtxq_set_owner(key:umtx_key;td:p_kthread);
var
 old:p_kthread;
begin
 old:=System.InterlockedExchange(key^.uc_owner,td);
 if (old<>nil) then
 begin
  thread_dec_ref(old);
 end;
 if (td<>nil) then
 begin
  thread_inc_ref(td);
 end;
end;

//int umtxq_check_susp(struct thread *td)

procedure umtxq_insert(uq:p_umtx_q;q:Integer=UMTX_SHARED_QUEUE);
var
 uh:p_umtxq_queue;
begin
 Assert((uq^.uq_flags and UQF_UMTXQ)=0,'umtx_q is already on queue');

 uh:=umtxq_queue_lookup(uq^.uq_key,q);

 TAILQ_INSERT_TAIL(@uh^.head,uq,@uq^.uq_link);
 Inc(uh^.length);

 uq^.uq_flags:=uq^.uq_flags or UQF_UMTXQ;
 uq^.uq_cur_queue:=uh;
end;

procedure umtxq_remove(uq:p_umtx_q;q:Integer=UMTX_SHARED_QUEUE);
var
 uh:p_umtxq_queue;
begin
 if (uq^.uq_flags and UQF_UMTXQ)<>0 then
 begin
  uh:=uq^.uq_cur_queue;

  TAILQ_REMOVE(@uh^.head,uq,@uq^.uq_link);
  Dec(uh^.length);

  uq^.uq_flags:=uq^.uq_flags and (not UQF_UMTXQ);

  if TAILQ_EMPTY(@uh^.head) then
  begin
   Assert(uh^.length=0,'inconsistent umtxq_queue length');
  end;

  uq^.uq_cur_queue:=nil;
 end;
end;

function umtxq_alloc:p_umtx_q;
begin
 Result:=AllocMem(SizeOf(umtx_q));
 Result^.uq_inherited_pri:=PRI_MAX;
 Result^.uq_handle:=_umtxq_alloc;
end;

procedure umtxq_free(uq:p_umtx_q);
begin
 if (uq=nil) then Exit;
 _umtxq_free(uq^.uq_handle);
 FreeMem(uq);
end;

procedure umtx_thread_init(td:p_kthread);
var
 uq:p_umtx_q;
begin
 uq:=umtxq_alloc;
 uq^.uq_thread:=td;

 td^.td_umtxq :=uq;
end;

procedure umtx_thread_exit(td:p_kthread);
begin
 thread_lock(td);
 sched_lend_user_prio(td,PRI_MAX);
 thread_unlock(td);
end;

procedure umtx_thread_fini(td:p_kthread);
begin
 umtxq_free(td^.td_umtxq);
end;

function msleep(uq:p_umtx_q;timo:Int64):Integer; inline;
begin
 Result:=msleep_umtxq(uq^.uq_handle,timo);
end;

function wakeup(uq:p_umtx_q):Integer; inline;
begin
 Result:=wakeup_umtxq(uq^.uq_handle);
end;

function umtxq_signal(key:umtx_key;n_wake:Integer;q:Integer=UMTX_SHARED_QUEUE):ptrint;
var
 uh:p_umtxq_queue;
 uq:p_umtx_q;
begin
 Result:=0;
 uh:=umtxq_queue_lookup(key,q);

 if (uh<>nil) then
 begin
  repeat
   uq:=TAILQ_FIRST(@uh^.head);
   if (uq=nil) then Break;

   umtxq_remove(uq);
   wakeup(uq);

   Inc(Result);
   if (Result>=n_wake) then Exit;
  until false;
 end;
end;

procedure umtxq_signal_thread(uq:p_umtx_q);
begin
 umtxq_remove(uq);
 wakeup(uq);
end;

function umtxq_sleep(uq:p_umtx_q;timo:Int64):Integer;
begin
 if ((uq^.uq_flags and UQF_UMTXQ)=0) then Exit(0);
 Result:=msleep(uq,timo);
end;

function UPRI(td:p_kthread):Integer;
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

procedure umtx_set_prio(td:p_kthread;pri:Integer);
begin
 if (td=nil) then Exit;
 if (pri<UPRI(td)) then
 begin
  thread_lock(td);
  sched_lend_user_prio(td,pri);
  thread_unlock(td);
 end;
end;

procedure umtx_reset_prio(td:p_kthread);
begin
 if (td=nil) then Exit;
 thread_lock(td);
 sched_lend_user_prio(td,td^.td_priority);
 thread_unlock(td);
end;

function umtxq_sleep_pi(uq:p_umtx_q;timo:Int64):Integer;
begin
 if ((uq^.uq_flags and UQF_UMTXQ)=0) then Exit(0);

 umtxq_lock(uq^.uq_key);
 umtx_set_prio(uq^.uq_key^.uc_owner,UPRI(uq^.uq_thread));
 umtxq_unlock(uq^.uq_key);

 Result:=msleep(uq,timo);

 if (Result<>0) then
 begin
  umtxq_lock(uq^.uq_key);
  umtx_reset_prio(uq^.uq_thread);
  umtxq_unlock(uq^.uq_key);
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

  if (owner=QWORD(-1)) then Exit(EFAULT);

  owner:=casuword64(umtx^.u_owner,UMTX_UNOWNED,id);

  if (owner=UMTX_UNOWNED) then Exit(0);

  if (owner=UMTX_CONTESTED) then
  begin
   owner:=casuword64(umtx^.u_owner,UMTX_CONTESTED,id or UMTX_CONTESTED);

   if (owner=UMTX_CONTESTED) then Exit(0);

   Continue;
  end;

  if (Result<>0) then Exit;

  uq^.uq_key:=umtx_key_get(umtx,TYPE_SIMPLE_LOCK);
  if (uq^.uq_key=nil) then Exit(EFAULT);

  umtxq_lock(uq^.uq_key);
  umtxq_insert(uq);
  umtxq_unlock(uq^.uq_key);

  old:=casuword64(umtx^.u_owner,owner,owner or UMTX_CONTESTED);

  if (old=owner) then
  begin
   Result:=umtxq_sleep(uq,timo);
  end;

  umtxq_lock(uq^.uq_key);
  umtxq_remove(uq);
  umtxq_unlock(uq^.uq_key);

  umtx_key_release(uq^.uq_key);
 until false;
end;

function do_lock_umtx(td:p_kthread;umtx:p_umtx;id:QWORD;timeout:ptimespec):Integer;
var
 tv,ts,ts2:Int64;
begin
 Result:=0;
 if (ptrint(umtx)<$1000) then Exit(EFAULT);

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

 if (owner=QWORD(-1)) then Exit(EFAULT);

 if ((owner and (not UMTX_CONTESTED))<>id) then
 begin
  Exit(EPERM);
 end;

 if ((owner and UMTX_CONTESTED)=0) then
 begin
  old:=casuword64(umtx^.u_owner,owner,UMTX_UNOWNED);

  if (old=owner) then Exit(0);
  owner:=old;
 end;

 key:=umtx_key_get(umtx,TYPE_SIMPLE_LOCK);
 if (key=nil) then Exit(EFAULT);

 umtxq_lock(key);
 count:=umtxq_count(key);
 umtxq_unlock(key);

 if (count<=1) then
 begin
  t:=UMTX_UNOWNED;
 end else
 begin
  t:=UMTX_CONTESTED;
 end;

 old:=casuword64(umtx^.u_owner,owner,t);

 umtxq_lock(key);
 umtxq_signal(key,1);
 umtxq_unlock(key);

 umtx_key_release(key);

 if (old<>owner) then Exit(EINVAL);
end;


function do_wait(td      :p_kthread;
                 addr    :Pointer;
                 id      :QWORD;
                 timeout :ptimespec;
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

 uq^.uq_key:=umtx_key_get(addr,TYPE_SIMPLE_WAIT);
 if (uq^.uq_key=nil) then Exit(EFAULT);

 umtxq_lock(uq^.uq_key);
 umtxq_insert(uq);
 umtxq_unlock(uq^.uq_key);

 if (compat32=0) then
 begin
  tmp:=fuword64(PQWORD(addr)^);
 end else
 begin
  tmp:=fuword32(PDWORD(addr)^);
 end;

 if (tmp<>id) then
 begin
  umtxq_lock(uq^.uq_key);
  umtxq_remove(uq);
  umtxq_unlock(uq^.uq_key);
 end else
 if (timeout=nil) then
 begin
  umtxq_sleep(uq,0);

  umtxq_lock(uq^.uq_key);
  umtxq_remove(uq);
  umtxq_unlock(uq^.uq_key);
 end else
 begin
  tv:=TIMESPEC_TO_UNIT(timeout);
  ts:=get_unit_uptime;
  ts:=ts+tv;

  repeat
   Result:=umtxq_sleep(uq,tvtohz(tv));

   if ((uq^.uq_flags and UQF_UMTXQ)=0) then
   begin
    Result:=0;
    Break;
   end;

   if (Result<>ETIMEDOUT) then Break;

   ts2:=get_unit_uptime;
   if (ts2>=ts) then
   begin
    Result:=ETIMEDOUT;
    Break;
   end;

   tv:=ts-ts2;
  until false;

  umtxq_lock(uq^.uq_key);
  umtxq_remove(uq);
  umtxq_unlock(uq^.uq_key);
 end;

 umtx_key_release(uq^.uq_key);

 if (Result=ERESTART) then
 begin
  Result:=EINTR;
 end;
end;

function kern_umtx_wake(td:p_kthread;umtx:p_umtx;n_wake,priv:Integer):Integer;
var
 key:umtx_key;
begin
 Result:=0;

 key:=umtx_key_get(umtx,TYPE_SIMPLE_WAIT);
 if (key=nil) then Exit(EFAULT);

 umtxq_lock(key);
 Result:=umtxq_signal(key,n_wake);
 umtxq_unlock(key);

 umtx_key_release(key);
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

  if (owner=DWORD(-1)) then Exit(EFAULT);

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

   if (owner=UMUTEX_CONTESTED) then
   begin
    owner:=casuword32(m^.m_owner,UMUTEX_CONTESTED,id or UMUTEX_CONTESTED);

    if (owner=UMUTEX_CONTESTED) then
    begin
     Exit(0);
    end;

    Continue;
   end;

   if ((flags and UMUTEX_ERROR_CHECK)<>0) and
      ((owner and (not UMUTEX_CONTESTED))=id) then
   begin
    Exit(EDEADLK);
   end;

   if (mode<>0) then
   begin
    Exit(EBUSY);
   end;

   if (Result<>0) then Exit;

   uq^.uq_key:=umtx_key_get(m,TYPE_NORMAL_UMUTEX);
   if (uq^.uq_key=nil) then Exit(EFAULT);

   umtxq_lock(uq^.uq_key);
   umtxq_insert(uq);
   umtxq_unlock(uq^.uq_key);

   old:=casuword32(m^.m_owner,owner,owner or UMUTEX_CONTESTED);

   if (old=owner) then
   begin
    Result:=umtxq_sleep(uq,timo);
   end;

   umtxq_lock(uq^.uq_key);
   umtxq_remove(uq);
   umtxq_unlock(uq^.uq_key);

   umtx_key_release(uq^.uq_key)
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

 if (owner=DWORD(-1)) then Exit(EFAULT);

 if ((owner and (not UMUTEX_CONTESTED))<>id) then
 begin
  Exit(EPERM);
 end;

 if ((owner and UMUTEX_CONTESTED)=0) then
 begin
  old:=casuword32(m^.m_owner,owner,UMUTEX_UNOWNED);

  if (old=owner) then Exit(0);
  owner:=old;
 end;

 key:=umtx_key_get(m,TYPE_NORMAL_UMUTEX);
 if (key=nil) then Exit(EFAULT);

 umtxq_lock(key);
 count:=umtxq_count(key);
 umtxq_unlock(key);

 if (count<=1) then
 begin
  t:=UMUTEX_UNOWNED;
 end else
 begin
  t:=UMUTEX_CONTESTED;
 end;

 old:=casuword32(m^.m_owner,owner,t);

 umtxq_lock(key);
 umtxq_signal(key,1);
 umtxq_unlock(key);

 umtx_key_release(key);

 if (old<>owner) then Exit(EINVAL);
end;

function do_wake_umutex(td:p_kthread;m:p_umutex):Integer;
var
 key:umtx_key;
 owner:DWORD;
 count:ptrint;
begin
 Result:=0;
 if (ptrint(m)<$1000) then Exit(EFAULT);

 owner:=fuword32(m^.m_owner);

 if (owner=DWORD(-1)) then Exit(EFAULT);

 if ((owner and (not UMUTEX_CONTESTED))<>0) then
 begin
  Exit(0);
 end;

 key:=umtx_key_get(m,TYPE_NORMAL_UMUTEX);
 if (key=nil) then Exit(EFAULT);

 umtxq_lock(key);
 count:=umtxq_count(key);
 umtxq_unlock(key);

 if (count>=1) then
 begin
  owner:=casuword32(m^.m_owner,UMUTEX_CONTESTED,UMUTEX_UNOWNED);
 end;

 if (count<>0) and ((owner and (not UMUTEX_CONTESTED))=0) then
 begin
  umtxq_lock(key);
  umtxq_signal(key,1);
  umtxq_unlock(key);
 end;

 umtx_key_release(key);
end;

function do_wake2_umutex(td:p_kthread;m:p_umutex;flags:DWORD):Integer;
label
 _exit;
var
 key:umtx_key;
 ktype:Integer;
 owner,old:DWORD;
 count:ptrint;
begin
 Result:=0;
 if (ptrint(m)<$1000) then Exit(EFAULT);

 Case (flags and (UMUTEX_PRIO_INHERIT or UMUTEX_PRIO_PROTECT)) of
                    0:ktype:=TYPE_NORMAL_UMUTEX;
  UMUTEX_PRIO_INHERIT:ktype:=TYPE_PI_UMUTEX;
  UMUTEX_PRIO_PROTECT:ktype:=TYPE_PP_UMUTEX;
  else
   Exit(EINVAL);
 end;

 key:=umtx_key_get(m,ktype);
 if (key=nil) then Exit(EFAULT);

 owner:=0;

 umtxq_lock(key);
 count:=umtxq_count(key);
 umtxq_unlock(key);

 if (count>1) then
 begin
  owner:=fuword32(m^.m_owner);

  if (owner=DWORD(-1)) then
  begin
   Result:=EFAULT;
   goto _exit;
  end;

  While ((owner and UMUTEX_CONTESTED)=0) do
  begin
   old:=casuword32(m^.m_owner,owner,owner or UMUTEX_CONTESTED);
   if (old=owner) then Break;
   owner:=old;
  end;
 end else
 if (count=1) then
 begin
  owner:=fuword32(m^.m_owner);

  if (owner=DWORD(-1)) then
  begin
   Result:=EFAULT;
   goto _exit;
  end;

  While ((owner and (not UMUTEX_CONTESTED))<>0) and
        ((owner and UMUTEX_CONTESTED)=0) do
  begin
   old:=casuword32(m^.m_owner,owner,owner or UMUTEX_CONTESTED);
   if (old=owner) then Break;
   owner:=old;
  end;

 end;

 if (count<>0) and ((owner and (not UMUTEX_CONTESTED))=0) then
 begin
  umtxq_lock(key);
  umtxq_signal(key,1);
  umtxq_unlock(key);
 end;

 _exit:
  umtx_key_release(key);
end;

//

function _do_lock_pi(td:p_kthread;m:p_umutex;flags:Integer;timo:Int64;mode:Integer):Integer;
label
 _exit;
var
 uq:p_umtx_q;
 id,owner,old:DWORD;
begin
 Result:=0;

 id:=td^.td_tid;
 uq:=td^.td_umtxq;

 uq^.uq_key:=umtx_key_get(m,TYPE_PI_UMUTEX);
 if (uq^.uq_key=nil) then Exit(EFAULT);

 owner:=fuword32(m^.m_owner);
 if (owner=DWORD(-1)) then Exit(EFAULT);

 repeat
  owner:=casuword32(m^.m_owner,UMUTEX_UNOWNED,id);

  if (owner=UMUTEX_UNOWNED) then
  begin
   Result:=0;
   goto _exit;
  end;

  if (owner=UMUTEX_CONTESTED) then
  begin
   owner:=casuword32(m^.m_owner,UMUTEX_CONTESTED,id or UMUTEX_CONTESTED);

   if (owner=UMUTEX_CONTESTED) then
   begin
    Result:=0;
    goto _exit;
   end;

   Continue;
  end;

  if ((flags and UMUTEX_ERROR_CHECK)<>0) and
     ((owner and (not UMUTEX_CONTESTED))=id) then
  begin
   Result:=EDEADLK;
   goto _exit;
  end;

  if (mode<>0) then
  begin
   Result:=EBUSY;
   goto _exit;
  end;

  if (Result<>0) then goto _exit;

  umtxq_lock(uq^.uq_key);
  umtxq_insert(uq);
  umtxq_unlock(uq^.uq_key);

  old:=casuword32(m^.m_owner,owner,owner or UMUTEX_CONTESTED);

  if (old=owner) then
  begin
   Result:=umtxq_sleep_pi(uq,timo);
  end;

  umtxq_lock(uq^.uq_key);
  umtxq_remove(uq);
  umtxq_unlock(uq^.uq_key);
 until false;

 _exit:
  if (Result=0) then
  begin
   umtxq_lock(uq^.uq_key);
   umtxq_set_owner(uq^.uq_key,td);
   umtxq_unlock(uq^.uq_key);
  end;

  umtx_key_release(uq^.uq_key);
end;

function do_unlock_pi(td:p_kthread;m:p_umutex;flags:Integer):Integer;
var
 uq_first,uq_max:p_umtx_q;
 key:umtx_key;
 id,owner,old,t:DWORD;
 pri:Integer;
 count:ptrint;
begin
 Result:=0;

 uq_first:=nil;
 id:=td^.td_tid;

 owner:=fuword32(m^.m_owner);

 if (owner=DWORD(-1)) then Exit(EFAULT);

 if ((owner and (not UMUTEX_CONTESTED))<>id) then
 begin
  Exit(EPERM);
 end;

 if ((owner and UMUTEX_CONTESTED)=0) then
 begin
  old:=casuword32(m^.m_owner,owner,UMUTEX_UNOWNED);

  if (old=owner) then Exit(0);
  owner:=old;
 end;

 key:=umtx_key_get(m,TYPE_PI_UMUTEX);
 if (key=nil) then Exit(EFAULT);

 umtxq_lock(key);
 count:=umtxq_count_pi(key,uq_first);

 if (uq_first<>nil) then
 begin
  if (key^.uc_owner<>td) then
  begin
   umtxq_unlock(key);
   umtx_key_release(key);
   Exit(EPERM);
  end;

  pri:=PRI_MAX;
  uq_max:=uq_first;

  While (uq_first<>nil) do
  begin
   if (UPRI(uq_first^.uq_thread)<UPRI(uq_max^.uq_thread)) then
   begin
    uq_max:=uq_first;
   end;

   if (pri>UPRI(uq_first^.uq_thread)) then
   begin
    pri:=UPRI(uq_first^.uq_thread);
   end;

   uq_first:=TAILQ_NEXT(uq_first,@uq_first^.uq_link);
  end;

  thread_lock(td);
  sched_lend_user_prio(td,pri);
  thread_unlock(td);

  if (uq_max<>nil) then
  begin
   umtxq_signal_thread(uq_max);
   umtxq_set_owner(key,nil);
  end;
 end;

 umtxq_unlock(key);

 if (count<=1) then
 begin
  t:=UMUTEX_UNOWNED;
 end else
 begin
  t:=UMUTEX_CONTESTED;
 end;

 old:=casuword32(m^.m_owner,owner,t);

 umtx_key_release(key);

 if (old<>owner) then Exit(EINVAL);
end;

//

function _do_lock_pp(td:p_kthread;m:p_umutex;flags:Integer;timo:Int64;mode:Integer):Integer;
label
 _exit;
var
 uq,uq2:p_umtx_q;
 id,owner,ceiling:DWORD;
 old_inherited_pri:Integer;
 pri:Integer;
begin
 Result:=0;

 uq2:=nil;

 id:=td^.td_tid;
 uq:=td^.td_umtxq;
 uq^.uq_key:=umtx_key_get(m,TYPE_PP_UMUTEX);
 if (uq^.uq_key=nil) then Exit(EFAULT);

 repeat
  old_inherited_pri:=uq^.uq_inherited_pri;

  ceiling:=fuword32(m^.m_ceilings[0]);
  if (ceiling>PRI_MAX) then
  begin
   Result:=EINVAL;
   goto _exit;
  end;

  umtxq_lock(uq^.uq_key);

  if (UPRI(td)<ceiling) then
  begin
   umtxq_unlock(uq^.uq_key);
   Result:=EINVAL;
   goto _exit;
  end;

  if (ceiling<uq^.uq_inherited_pri) then
  begin
   thread_lock(td);
   uq^.uq_inherited_pri:=ceiling;
   if (uq^.uq_inherited_pri<UPRI(td)) then
   begin
    sched_lend_user_prio(td,uq^.uq_inherited_pri);
   end;
   thread_unlock(td);
  end;

  umtxq_unlock(uq^.uq_key);

  owner:=casuword32(m^.m_owner,UMUTEX_UNOWNED,id);

  if (owner=UMUTEX_UNOWNED) then
  begin
   Result:=0;
   goto _exit;
  end;

  if (owner=UMUTEX_CONTESTED) then
  begin
   owner:=casuword32(m^.m_owner,UMUTEX_CONTESTED,id or UMUTEX_CONTESTED);

   if (owner=UMUTEX_CONTESTED) then
   begin
    Result:=0;
    goto _exit;
   end;

   Continue;
  end;

  if ((flags and UMUTEX_ERROR_CHECK)<>0) and
     ((owner and (not UMUTEX_CONTESTED))=id) then
  begin
   Result:=EDEADLK;
   break;
  end;

  if (mode<>0) then
  begin
   Result:=EBUSY;
   break;
  end;

  if (Result<>0) then goto _exit;

  umtxq_lock(uq^.uq_key);
  umtxq_insert(uq);
  umtxq_unlock(uq^.uq_key);

  Result:=umtxq_sleep_pi(uq,timo);

  umtxq_lock(uq^.uq_key);
  umtxq_remove(uq);

  uq^.uq_inherited_pri:=old_inherited_pri;

  pri:=PRI_MAX;
  umtxq_count_pi(uq^.uq_key,uq2);

  While (uq2<>nil) do
  begin
   if (pri>UPRI(uq2^.uq_thread)) then
   begin
    pri:=UPRI(uq2^.uq_thread);
   end;
   uq2:=TAILQ_NEXT(uq2,@uq2^.uq_link);
  end;

  if (pri>uq^.uq_inherited_pri) then
  begin
   pri:=uq^.uq_inherited_pri;
  end;

  thread_lock(td);
  sched_lend_user_prio(td,pri);
  thread_unlock(td);

  umtxq_unlock(uq^.uq_key);
 until false;

 //

 if (Result<>0) then
 begin
  umtxq_lock(uq^.uq_key);
  uq^.uq_inherited_pri:=old_inherited_pri;

  pri:=PRI_MAX;
  umtxq_count_pi(uq^.uq_key,uq2);

  While (uq2<>nil) do
  begin
   if (pri>UPRI(uq2^.uq_thread)) then
   begin
    pri:=UPRI(uq2^.uq_thread);
   end;
   uq2:=TAILQ_NEXT(uq2,@uq2^.uq_link);
  end;

  if (pri>uq^.uq_inherited_pri) then
  begin
   pri:=uq^.uq_inherited_pri;
  end;

  thread_lock(td);
  sched_lend_user_prio(td,pri);
  thread_unlock(td);

  umtxq_unlock(uq^.uq_key);
 end;

 _exit:
  umtx_key_release(uq^.uq_key);
end;

function do_unlock_pp(td:p_kthread;m:p_umutex;flags:Integer):Integer;
var
 uq,uq2:p_umtx_q;
 key:umtx_key;
 id,owner:DWORD;
 rceiling:Integer;
 new_inherited_pri:Integer;
 pri:Integer;
begin
 Result:=0;

 uq2:=nil;
 id:=td^.td_tid;
 uq:=td^.td_umtxq;

 owner:=fuword32(m^.m_owner);

 if (owner=DWORD(-1)) then Exit(EFAULT);

 if ((owner and (not UMUTEX_CONTESTED))<>id) then
 begin
  Exit(EPERM);
 end;

 Result:=copyin(@m^.m_ceilings[1],@rceiling,SizeOf(Integer));
 if (Result<>0) then Exit(EFAULT);

 if (rceiling=-1) then
 begin
  new_inherited_pri:=PRI_MAX;
 end else
 begin
  if (rceiling>PRI_MAX) then Exit(EINVAL);
  new_inherited_pri:=rceiling;
 end;

 key:=umtx_key_get(m,TYPE_PP_UMUTEX);
 if (key=nil) then Exit(EFAULT);

 Result:=suword32(m^.m_owner,UMUTEX_CONTESTED);

 if (Result<>0) then
 begin
  umtx_key_release(key);
  Exit(EFAULT);
 end;

 umtxq_lock(key);
 umtxq_signal(key,1);

 uq^.uq_inherited_pri:=new_inherited_pri;

 pri:=PRI_MAX;
 umtxq_count_pi(uq^.uq_key,uq2);

 While (uq2<>nil) do
 begin
  if (pri>UPRI(uq2^.uq_thread)) then
  begin
   pri:=UPRI(uq2^.uq_thread);
  end;
  uq2:=TAILQ_NEXT(uq2,@uq2^.uq_link);
 end;

 if (pri>uq^.uq_inherited_pri) then
 begin
  pri:=uq^.uq_inherited_pri;
 end;

 umtxq_unlock(key);

 thread_lock(td);
 sched_lend_user_prio(td,pri);
 thread_unlock(td);

 umtx_key_release(key);
end;

function do_set_ceiling(td:p_kthread;m:p_umutex;ceiling:DWORD;old_ceiling:PDWORD):Integer;
var
 uq:p_umtx_q;
 save_ceiling:DWORD;
 id,owner:DWORD;
 flags:DWORD;
begin
 Result:=0;
 if (ptrint(m)<$1000) then Exit(EFAULT);

 flags:=fuword32(m^.m_flags);

 if ((flags and UMUTEX_PRIO_PROTECT)=0) then Exit(EINVAL);
 if (ceiling>PRI_MAX) then Exit(EINVAL);

 id:=td^.td_tid;
 uq:=td^.td_umtxq;
 uq^.uq_key:=umtx_key_get(m,TYPE_PP_UMUTEX);

 if (uq^.uq_key=nil) then Exit(EFAULT);

 repeat
  save_ceiling:=fuword32(m^.m_ceilings[0]);

  owner:=fuword32(m^.m_owner);

  if (owner=DWORD(-1)) then
  begin
   umtx_key_release(uq^.uq_key);
   Exit(EFAULT);
  end;

  owner:=casuword32(m^.m_owner,UMUTEX_CONTESTED,id or UMUTEX_CONTESTED);

  if (owner=UMUTEX_CONTESTED) then
  begin
   suword32(m^.m_ceilings[0],ceiling);
   suword32(m^.m_owner,UMUTEX_CONTESTED);
   Result:=0;
   break;
  end;

  if ((owner and (not UMUTEX_CONTESTED))=id) then
  begin
   suword32(m^.m_ceilings[0],ceiling);
   Result:=0;
   break;
  end;

  if (Result<>0) then Break;

  umtxq_lock(uq^.uq_key);
  umtxq_insert(uq);
  umtxq_unlock(uq^.uq_key);

  Result:=umtxq_sleep(uq,0);

  umtxq_lock(uq^.uq_key);
  umtxq_remove(uq);
  umtxq_unlock(uq^.uq_key);
 until false;

 umtxq_lock(uq^.uq_key);
 if (Result=0) then
 begin
  umtxq_signal(uq^.uq_key,High(Integer));
 end;
 umtxq_unlock(uq^.uq_key);

 umtx_key_release(uq^.uq_key);

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

function do_lock_umutex(td:p_kthread;m:p_umutex;timeout:ptimespec;mode:Integer):Integer;
var
 flags:DWORD;
 tv,ts,ts2:Int64;
begin
 Result:=0;
 if (ptrint(m)<$1000) then Exit(EFAULT);

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
 if (ptrint(m)<$1000) then Exit(EFAULT);

 flags:=fuword32(m^.m_flags);

 Case (flags and (UMUTEX_PRIO_INHERIT or UMUTEX_PRIO_PROTECT)) of
                    0:Result:=do_unlock_normal(td,m,flags);
  UMUTEX_PRIO_INHERIT:Result:=do_unlock_pi(td,m,flags);
  UMUTEX_PRIO_PROTECT:Result:=do_unlock_pp(td,m,flags);
  else
   Exit(EINVAL);
 end;

end;

////

function do_cv_wait(td:p_kthread;cv:p_ucond;m:p_umutex;timeout:ptimespec;wflags:QWORD):Integer;
label
 _exit;
var
 uq:p_umtx_q;
 clockid:Integer;
 oldlen:Integer;
 cts,ets,tts,tv:Int64;
begin
 Result:=0;
 if (ptrint(cv)<$1000) then Exit(EFAULT);
 if (ptrint(m) <$1000) then Exit(EFAULT);

 uq:=td^.td_umtxq;

 uq^.uq_key:=umtx_key_get(cv,TYPE_CV);
 if (uq^.uq_key=nil) then Exit(EFAULT);

 if ((wflags and CVWAIT_CLOCKID)<>0) then
 begin
  clockid:=fuword32(cv^.c_clockid);
  if (clockid<CLOCK_REALTIME) or
     (clockid>=CLOCK_THREAD_CPUTIME_ID) then
  begin
   Result:=EINVAL;
   goto _exit;
  end;
 end else
 begin
  clockid:=CLOCK_REALTIME;
 end;

 umtxq_lock(uq^.uq_key);
 umtxq_insert(uq);
 umtxq_unlock(uq^.uq_key);

 if (fuword32(cv^.c_has_waiters)=0) then
 begin
  suword32(cv^.c_has_waiters,1);
 end;

 Result:=do_unlock_umutex(td,m);

 if (Result=0) then
 begin
  if (timeout=nil) then
  begin
   Result:=umtxq_sleep(uq,0);
  end else
  begin
   if ((wflags and CVWAIT_ABSTIME)=0) then
   begin
    ets:=0;
    Result:=kern_clock_gettime_unit(clockid,@ets);
    if (Result<>0) then goto _exit;

    tts:=TIMESPEC_TO_UNIT(timeout);
    ets:=ets+tts;
   end else
   begin
    ets:=TIMESPEC_TO_UNIT(timeout);

    cts:=0;
    Result:=kern_clock_gettime_unit(clockid,@cts);
    if (Result<>0) then goto _exit;

    tts:=tts-cts;
   end;

   tv:=tts;
   repeat
    Result:=umtxq_sleep(uq,tvtohz(tv));
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

 umtxq_lock(uq^.uq_key);

 if ((uq^.uq_flags and UQF_UMTXQ)=0) then
 begin
  umtxq_unlock(uq^.uq_key);
  Result:=0;
 end else
 begin
  if ((uq^.uq_flags and UQF_UMTXQ)<>0) then
  begin
   oldlen:=uq^.uq_cur_queue^.length;
   umtxq_remove(uq);
   umtxq_unlock(uq^.uq_key);
   if (oldlen=1) then
   begin
    suword32(cv^.c_has_waiters,0);
   end;
  end else
  begin
   umtxq_unlock(uq^.uq_key);
  end;
  if (Result=ERESTART) then
  begin
   Result:=EINTR;
  end;
 end;

 _exit:
  umtx_key_release(uq^.uq_key);
end;

function do_cv_signal(td:p_kthread;cv:p_ucond):Integer;
var
 key:umtx_key;
 count,nwake:Integer;
begin
 Result:=0;
 if (ptrint(cv)<$1000) then Exit(EFAULT);

 key:=umtx_key_get(cv,TYPE_CV);
 if (key=nil) then Exit(EFAULT);

 umtxq_lock(key);
 count:=umtxq_count(key);
 nwake:=umtxq_signal(key,1);
 umtxq_unlock(key);

 if (count<=nwake) then
 begin
  Result:=suword32(cv^.c_has_waiters,0);
  if (Result<>0) then Result:=EFAULT;
 end;

 umtx_key_release(key);
end;

function do_cv_broadcast(td:p_kthread;cv:p_ucond):Integer;
var
 key:umtx_key;
begin
 Result:=0;
 if (ptrint(cv)<$1000) then Exit(EFAULT);

 key:=umtx_key_get(cv,TYPE_CV);
 if (key=nil) then Exit(EFAULT);

 umtxq_lock(key);
 umtxq_signal(key,High(Integer));
 umtxq_unlock(key);

 Result:=suword32(cv^.c_has_waiters,0);
 if (Result<>0) then Result:=EFAULT;

 umtx_key_release(key);
end;

////

function do_rw_rdlock(td:p_kthread;rwlock:p_urwlock;fflag:QWORD;timo:Int64):Integer;
label
 _exit,_sleep;
var
 uq:p_umtx_q;
 flags,wrflags:DWORD;
 state,oldstate:Integer;
 blocked_readers:Integer;
begin
 Result:=0;

 uq:=td^.td_umtxq;

 uq^.uq_key:=umtx_key_get(rwlock,TYPE_RWLOCK);
 if (uq^.uq_key=nil) then Exit(EFAULT);

 flags:=fuword32(rwlock^.rw_flags);

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
    Result:=EAGAIN;
    goto _exit;
   end;

   oldstate:=casuword32(rwlock^.rw_state,state,state+1);

   if (oldstate=-1) then
   begin
    Result:=EFAULT;
    goto _exit;
   end;
   if (oldstate=state) then
   begin
    Result:=0;
    goto _exit;
   end;

   state:=oldstate;
  end;

  if (Result<>0) then goto _exit;

  state:=fuword32(rwlock^.rw_state);

  While ((state and wrflags)<>0) and ((state and URWLOCK_READ_WAITERS)=0) do
  begin
   oldstate:=casuword32(rwlock^.rw_state,state,state or URWLOCK_READ_WAITERS);

   if (oldstate=-1) then
   begin
    Result:=EFAULT;
    goto _exit;
   end;

   if (oldstate=state) then
   begin
    goto _sleep;
   end;

   state:=oldstate;
  end;

  if ((state and wrflags)=0) then
  begin
   Continue;
  end;

  _sleep:

  blocked_readers:=fuword32(rwlock^.rw_blocked_readers);
  suword32(rwlock^.rw_blocked_readers,blocked_readers+1);

  While ((state and wrflags)<>0) do
  begin
   umtxq_lock(uq^.uq_key);
   umtxq_insert(uq);
   umtxq_unlock(uq^.uq_key);

   Result:=umtxq_sleep(uq,timo);

   umtxq_lock(uq^.uq_key);
   umtxq_remove(uq);
   umtxq_unlock(uq^.uq_key);

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

 until (Result<>0);

 _exit:
  umtx_key_release(uq^.uq_key);
end;

function do_rw_rdlock2(td:p_kthread;rwlock:p_urwlock;fflag:QWORD;timeout:ptimespec):Integer;
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
 _exit,_sleep;
var
 uq:p_umtx_q;
 state,oldstate:Integer;
 blocked_writers:Integer;
 blocked_readers:Integer;
begin
 Result:=0;

 uq:=td^.td_umtxq;

 uq^.uq_key:=umtx_key_get(rwlock,TYPE_RWLOCK);
 if (uq^.uq_key=nil) then Exit(EFAULT);

 blocked_readers:=0;

 repeat
  state:=fuword32(rwlock^.rw_state);

  while ((state and URWLOCK_WRITE_OWNER)=0) and (URWLOCK_READER_COUNT(state)=0) do
  begin
   oldstate:=casuword32(rwlock^.rw_state,state,state or URWLOCK_WRITE_OWNER);

   if (oldstate=-1) then
   begin
    Result:=EFAULT;
    goto _exit;
   end;
   if (oldstate=state) then
   begin
    Result:=0;
    goto _exit;
   end;

   state:=oldstate;
  end;

  if (Result<>0) then
  begin
   if ((state and (URWLOCK_WRITE_OWNER or URWLOCK_WRITE_WAITERS))=0) and
      (blocked_readers<>0) then
   begin
    umtxq_lock(uq^.uq_key);
    umtxq_signal(uq^.uq_key,High(Integer));
    umtxq_unlock(uq^.uq_key);
   end;
   break;
  end;

  state:=fuword32(rwlock^.rw_state);

  while (((state and URWLOCK_WRITE_OWNER)<>0) or (URWLOCK_READER_COUNT(state)<>0)) and
        ((state and URWLOCK_WRITE_WAITERS)=0) do
  begin
   oldstate:=casuword32(rwlock^.rw_state,state,state or URWLOCK_WRITE_WAITERS);

   if (oldstate=-1) then
   begin
    Result:=EFAULT;
    goto _exit;
   end;
   if (oldstate=state) then
   begin
    goto _sleep;
   end;

   state:=oldstate;
  end;

  if (Result<>0) then goto _exit;

  if ((state and URWLOCK_WRITE_OWNER)=0) and (URWLOCK_READER_COUNT(state)=0) then
  begin
   Continue;
  end;

  _sleep:

  blocked_writers:=fuword32(rwlock^.rw_blocked_writers);
  suword32(rwlock^.rw_blocked_writers,blocked_writers+1);

  While ((state and URWLOCK_WRITE_OWNER) or URWLOCK_READER_COUNT(state)<>0) do
  begin
   umtxq_lock(uq^.uq_key);
   umtxq_insert(uq,UMTX_EXCLUSIVE_QUEUE);
   umtxq_unlock(uq^.uq_key);

   Result:=umtxq_sleep(uq,timo);

   umtxq_lock(uq^.uq_key);
   umtxq_remove(uq,UMTX_EXCLUSIVE_QUEUE);
   umtxq_unlock(uq^.uq_key);

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
     goto _exit;
    end;
    if (oldstate=state) then
    begin
     Break;
    end;

    state:=oldstate;
   until false;

   blocked_readers:=fuword32(rwlock^.rw_blocked_readers);
  end else
  begin
   blocked_readers:=0;
  end;

 until false;

 _exit:
  umtx_key_release(uq^.uq_key);
end;

function do_rw_wrlock2(td:p_kthread;rwlock:p_urwlock;fflag:QWORD;timeout:ptimespec):Integer;
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
 _exit;
var
 uq:p_umtx_q;
 flags:DWORD;
 state,oldstate:Integer;
 q,count:Integer;
begin
 Result:=0;

 uq:=td^.td_umtxq;

 flags:=fuword32(rwlock^.rw_flags);

 uq^.uq_key:=umtx_key_get(rwlock,TYPE_RWLOCK);
 if (uq^.uq_key=nil) then Exit(EFAULT);

 state:=fuword32(rwlock^.rw_state);

 if ((state and URWLOCK_WRITE_OWNER)<>0) then
 begin
  repeat
   oldstate:=casuword32(rwlock^.rw_state,state,state and (not URWLOCK_WRITE_OWNER));

   if (oldstate=-1) then
   begin
    Result:=EFAULT;
    goto _exit;
   end;
   if (oldstate<>state) then
   begin
    state:=oldstate;
    if ((oldstate and URWLOCK_WRITE_OWNER)=0) then
    begin
     Result:=EPERM;
     goto _exit;
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
    goto _exit;
   end;

   if (oldstate<>state) then
   begin
    state:=oldstate;
    if (URWLOCK_READER_COUNT(oldstate)=0) then
    begin
     Result:=EPERM;
     goto _exit;
    end;
   end else
   begin
    Break;
   end;

  until false;
 end else
 begin
  Result:=EPERM;
  goto _exit;
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
  umtxq_lock(uq^.uq_key);
  umtxq_signal(uq^.uq_key,count,q);
  umtxq_unlock(uq^.uq_key);
 end;

 _exit:
  umtx_key_release(uq^.uq_key);
end;

////

function do_sem_wait(td:p_kthread;sem:p__usem;timeout:ptimespec):Integer;
var
 uq:p_umtx_q;
 count:DWORD;
 cts,ets,tv:Int64;
begin
 Result:=0;

 uq:=td^.td_umtxq;

 uq^.uq_key:=umtx_key_get(sem,TYPE_SEM);
 if (uq^.uq_key=nil) then Exit(EFAULT);

 umtxq_lock(uq^.uq_key);
 umtxq_insert(uq);
 umtxq_unlock(uq^.uq_key);

 if ((fuword32(sem^._has_waiters))=0) then
 begin
  casuword32(sem^._has_waiters,0,1);
 end;

 count:=fuword32(sem^._count);

 if (count<>0) then
 begin
  umtxq_lock(uq^.uq_key);
  umtxq_remove(uq);
  umtxq_unlock(uq^.uq_key);
  umtx_key_release(uq^.uq_key);
  Exit(0);
 end;

 if (timeout=nil) then
 begin
  Result:=umtxq_sleep(uq,0);
 end else
 begin
  ets:=get_unit_uptime;
  tv:=TIMESPEC_TO_UNIT(timeout);
  ets:=ets+tv;

  repeat
   Result:=umtxq_sleep(uq,tvtohz(tv));
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
  umtxq_lock(uq^.uq_key);
  umtxq_remove(uq);
  umtxq_unlock(uq^.uq_key);

  if (Result=ERESTART) and (timeout<>nil) then
  begin
   Result:=EINTR;
  end;
 end;

 umtx_key_release(uq^.uq_key);
end;

function do_sem_wake(td:p_kthread;sem:p__usem):Integer;
var
 key:umtx_key;
 count,nwake:Integer;
begin
 Result:=0;

 key:=umtx_key_get(sem,TYPE_SEM);
 if (key=nil) then Exit(EFAULT);

 umtxq_lock(key);
 count:=umtxq_count(key);
 nwake:=umtxq_signal(key,1);
 umtxq_unlock(key);

 if (count<=nwake) then
 begin
  Result:=suword32(sem^._has_waiters,0);
  if (Result<>0) then Result:=EFAULT;
 end;

 umtx_key_release(key);
end;

////

function umtx_copyin_timeout(addr:Pointer;tsp:ptimespec):Integer;
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
 ts:ptimespec;
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
 ts:ptimespec;
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
 ts:ptimespec;
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
 ts:ptimespec;
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
 ts:ptimespec;
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
 ts:ptimespec;
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
 ts:ptimespec;
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
 if (ptrint(obj)<$1000) then Exit(EFAULT);

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
 if (ptrint(obj)<$1000) then Exit(EFAULT);

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

function sys__umtx_lock(mtx:p_umtx):Integer;
var
 td:p_kthread;
begin
 if (mtx=nil) then Exit(EINVAL);
 td:=curkthread;
 if (td=nil) then Exit(EFAULT);
 Result:=_do_lock_umtx(td,mtx,td^.td_tid,0);
end;

function sys__umtx_unlock(mtx:p_umtx):Integer;
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
 ts:ptimespec;
 timeout:timespec;
begin
 if (ptrint(obj)<$1000) then Exit(EFAULT);

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

initialization
 umtxq_sysinit;

end.

