unit kern_umtx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 gtailq,
 kern_rwlock,
 time,
 kern_time,

 kern_thread,
 _umtx,
 sys_kernel;

procedure _umutex_init(mtx:p_umutex); inline;
procedure _umtx_obj_done(mtx:Pointer);

procedure umtx_thread_init(td:p_kthread);
procedure umtx_thread_exit(td:p_kthread);
procedure umtx_thread_fini(td:p_kthread);

function  _sys_umtx_lock(mtx:p_umtx):Integer;
function  _sys_umtx_unlock(mtx:p_umtx):Integer;
function  _sys_umtx_op(obj:Pointer;op:Integer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;

implementation

uses
 HAMT,
 systm,

 windows,
 ntapi;

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

 umtx_key     =p_umtxq_queue;

 //per thread local
 umtx_q=packed record
  pNext,pPrev:p_umtx_q; //uq_link
  //
  uq_key          :umtx_key;
  uq_flags        :Integer;
  uq_inherited_pri:Integer;
  uq_thread       :p_kthread;
  uq_handle       :THandle; //nt event
  uq_cur_queue    :p_umtxq_queue;
 end;

 umtxq_head=specialize TAILQ_HEAD<p_umtx_q>;

 //per mutex addr
 umtxq_queue=packed record
  head    :umtxq_head;
  length  :Integer;
  refs    :Integer;
  lock    :Pointer;
  owner   :p_kthread;
 end;

var
 umtx_hamt:TSTUB_HAMT64;
 umtx_hamt_lock:Pointer=nil;

 //TYPE_SIMPLE_WAIT  =0;
 //TYPE_CV           =1;
 //TYPE_SEM          =2;
 //TYPE_SIMPLE_LOCK  =3;
 //TYPE_NORMAL_UMUTEX=4;
 //TYPE_PI_UMUTEX    =5;
 //TYPE_PP_UMUTEX    =6;
 //TYPE_RWLOCK       =7;

procedure umtxq_sysinit; inline;
begin
 FillChar(umtx_hamt,SizeOf(umtx_hamt),0);
end;

function umtx_key_get(m:Pointer;ktype:Integer):umtx_key;
Var
 data:PPointer;
 new:umtx_key;
begin
 Result:=nil;

 rw_rlock(umtx_hamt_lock);
  data:=HAMT_search64(@umtx_hamt,QWORD(m));
  if (data<>nil) then //EXIST
  begin
   Result:=data^;
   if (Result<>nil) then
   begin
    System.InterlockedIncrement(Result^.refs); //GET
   end;
  end;
 rw_runlock(umtx_hamt_lock);

 if (Result<>nil) then Exit; //EXIST

 new:=AllocMem(SizeOf(umtxq_queue)); //NEW
 new^.refs:=1; //INSERT

 rw_wlock(umtx_hamt_lock);
  data:=HAMT_insert64(@umtx_hamt,QWORD(m),new);
  if (data<>nil) then //NOMEM
  begin
   Result:=data^;
   if (Result<>nil) then
   begin
    System.InterlockedIncrement(Result^.refs); //GET
   end;
  end;
 rw_wunlock(umtx_hamt_lock);

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

function umtx_key_remove(m:Pointer):umtx_key;
begin
 if (m=nil) then Exit(nil);
 rw_wlock(umtx_hamt_lock);
  Result:=HAMT_delete64(@umtx_hamt,QWORD(m));
 rw_wunlock(umtx_hamt_lock);
end;

procedure umtxq_set_owner(key:umtx_key;td:p_kthread); forward;

procedure umtx_key_release(var key:umtx_key);
var
 old:umtx_key;
begin
 old:=System.InterlockedExchange(key,nil);
 if (old=nil) then Exit;
 if (System.InterlockedDecrement(old^.refs)=0) then
 begin
  umtxq_set_owner(old,nil);
  FreeMem(old);
 end;
end;

function umtxq_queue_lookup(key:umtx_key):p_umtxq_queue; inline;
begin
 Result:=key;
end;

procedure umtxq_lock(key:umtx_key); inline;
begin
 rw_wlock(key^.lock);
end;

procedure umtxq_unlock(key:umtx_key); inline;
begin
 rw_wunlock(key^.lock);
end;

function umtxq_count(key:umtx_key):ptrint;
var
 uh:p_umtxq_queue;
begin
 Result:=0;
 uh:=umtxq_queue_lookup(key);
 if (uh<>nil) then Result:=uh^.length;
end;

function umtxq_count_pi(key:umtx_key;var first:p_umtx_q):ptrint;
var
 uh:p_umtxq_queue;
begin
 Result:=0;
 uh:=umtxq_queue_lookup(key);
 if (uh<>nil) then
 begin
  first:=uh^.head.pHead; //TAILQ_FIRST
  Result:=uh^.length;
 end;
end;

procedure umtxq_set_owner(key:umtx_key;td:p_kthread);
var
 old:p_kthread;
begin
 old:=System.InterlockedExchange(key^.owner,td);
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

procedure umtxq_insert(uq:p_umtx_q);
var
 uh:p_umtxq_queue;
begin
 Assert((uq^.uq_flags and UQF_UMTXQ)=0,'umtx_q is already on queue');

 uh:=umtxq_queue_lookup(uq^.uq_key);

 uh^.head.Insert_tail(uq);
 Inc(uh^.length);

 uq^.uq_flags:=uq^.uq_flags or UQF_UMTXQ;
 uq^.uq_cur_queue:=uh;
end;

procedure umtxq_remove(uq:p_umtx_q);
var
 uh:p_umtxq_queue;
begin
 if (uq^.uq_flags and UQF_UMTXQ)<>0 then
 begin
  uh:=uq^.uq_cur_queue;

  uh^.head.Remove(uq); //uq_link
  Dec(uh^.length);

  uq^.uq_flags:=uq^.uq_flags and (not UQF_UMTXQ);

  if (uh^.head.pHead=nil) then //TAILQ_EMPTY
  begin
   Assert(uh^.length=0,'inconsistent umtxq_queue length');
  end;

  uq^.uq_cur_queue:=nil;
 end;
end;

function ntw2px(n:Integer):Integer;
begin
 Case DWORD(n) of
  STATUS_SUCCESS         :Result:=0;
  STATUS_ABANDONED       :Result:=EPERM;
  STATUS_ALERTED         :Result:=EINTR;
  STATUS_USER_APC        :Result:=EINTR;
  STATUS_TIMEOUT         :Result:=ETIMEDOUT;
  STATUS_ACCESS_VIOLATION:Result:=EFAULT;
  else
                   Result:=EINVAL;
 end;
end;

function umtxq_alloc:p_umtx_q;
var
 n:Integer;
begin
 Result:=AllocMem(SizeOf(umtx_q));
 Result^.uq_inherited_pri:=PRI_MAX;

 n:=NtCreateEvent(@Result^.uq_handle,EVENT_ALL_ACCESS,nil,SynchronizationEvent,False);
 Assert(n=0);
end;

procedure umtxq_free(uq:p_umtx_q);
begin
 if (uq=nil) then Exit;

 NtClose(uq^.uq_handle);

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
 Result:=ntw2px(NtWaitForSingleObject(uq^.uq_handle,True,@timo));
end;

function wakeup(uq:p_umtx_q):Integer; inline;
begin
 Result:=ntw2px(NtSetEvent(uq^.uq_handle,nil));
end;

function umtxq_signal(key:umtx_key;n_wake:Integer):ptrint;
var
 uh:p_umtxq_queue;
 uq:p_umtx_q;
begin
 Result:=0;
 uh:=umtxq_queue_lookup(key);

 if (uh<>nil) then
 begin
  repeat
   uq:=uh^.head.pHead; //TAILQ_FIRST
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
 umtx_set_prio(uq^.uq_key^.owner,UPRI(uq^.uq_thread));
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
  Result:=_do_lock_umtx(td,umtx,id,NT_INFINITE);
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
   Result:=_do_lock_umtx(td,umtx,id,-tv);
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
  umtxq_sleep(uq,NT_INFINITE);

  umtxq_lock(uq^.uq_key);
  umtxq_remove(uq);
  umtxq_unlock(uq^.uq_key);
 end else
 begin
  tv:=TIMESPEC_TO_UNIT(timeout);
  ts:=get_unit_uptime;
  ts:=ts+tv;

  repeat
   Result:=umtxq_sleep(uq,-tv);

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
  if (key^.owner<>td) then
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

   uq_first:=uq_first^.pNext;
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
   uq2:=uq2^.pNext;
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
   uq2:=uq2^.pNext;
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
  uq2:=uq2^.pNext;
 end;

 if (pri>uq^.uq_inherited_pri) then
 begin
  pri:=uq^.uq_inherited_pri;
 end;

 thread_lock(td);
 sched_lend_user_prio(td,pri);
 thread_unlock(td);

 umtxq_unlock(key);

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
  Result:=_do_lock_umutex(td,m,flags,NT_INFINITE,mode);
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
   Result:=_do_lock_umutex(td,m,flags,-tv,mode);
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
   Result:=umtxq_sleep(uq,NT_INFINITE);
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
    Result:=umtxq_sleep(uq,-tv);
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
  Result:=0;
 end else
 begin
  if ((uq^.uq_flags and UQF_UMTXQ)<>0) then
  begin
   oldlen:=uq^.uq_cur_queue^.length;
   umtxq_remove(uq);
   if (oldlen=1) then
   begin
    umtxq_unlock(uq^.uq_key);
    suword32(cv^.c_has_waiters,0);
    umtxq_lock(uq^.uq_key);
   end;
  end;
  if (Result=ERESTART) then
  begin
   Result:=EINTR;
  end;
 end;

 umtxq_unlock(uq^.uq_key);

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
 if (count<=nwake) then
 begin
  umtxq_unlock(&key);
  Result:=suword32(cv^.c_has_waiters,0);
  umtxq_lock(&key);
 end;

 umtxq_unlock(key);

 umtx_key_release(key);

 if (Result<>0) then Result:=EFAULT;
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

 umtx_key_release(key);

 if (Result<>0) then Result:=EFAULT;
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

function _sys_umtx_lock(mtx:p_umtx):Integer;
var
 td:p_kthread;
begin
 if (mtx=nil) then Exit(EINVAL);
 td:=curkthread;
 if (td=nil) then Exit(EINVAL);
 Result:=_do_lock_umtx(td,mtx,td^.td_tid,0);
end;

function _sys_umtx_unlock(mtx:p_umtx):Integer;
var
 td:p_kthread;
begin
 if (mtx=nil) then Exit(EINVAL);
 td:=curkthread;
 if (td=nil) then Exit(EINVAL);
 Result:=do_unlock_umtx(td,mtx,td^.td_tid);
end;

function _sys_umtx_op(obj:Pointer;op:Integer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 td:p_kthread;
begin
 if (obj=nil) then Exit(EINVAL);
 td:=curkthread;
 if (td=nil) then Exit(EINVAL);
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
  //UMTX_OP_RW_RDLOCK
  //UMTX_OP_RW_WRLOCK
  //UMTX_OP_RW_UNLOCK
  UMTX_OP_WAIT_UINT_PRIVATE:Result:=__umtx_op_wait_uint_private(td,obj,val,uaddr1,uaddr2);
  UMTX_OP_WAKE_PRIVATE     :Result:=__umtx_op_wake_private     (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_MUTEX_WAIT       :Result:=__umtx_op_wait_umutex      (td,obj,val,uaddr1,uaddr2);
  UMTX_OP_MUTEX_WAKE       :Result:=__umtx_op_wake_umutex      (td,obj,val,uaddr1,uaddr2);
  //UMTX_OP_SEM_WAIT
  //UMTX_OP_SEM_WAKE
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

procedure _umtx_obj_done(mtx:Pointer);
var
 key:umtx_key;
begin
 key:=umtx_key_remove(mtx);
 if (key<>nil) then
 begin
  umtx_key_release(key);
 end;
end;

initialization
 umtxq_sysinit;

end.

