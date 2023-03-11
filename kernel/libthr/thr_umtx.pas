unit thr_umtx;

{$mode ObjFPC}{$H+}

interface

uses
 _umtx,
 sys_umtx,
 time,
 _sys_time,
 thr_private,
 pthread_md;

type
 p_umtx=_umtx.p_umtx;
 umtx  =_umtx.umtx;

 p_umutex=_umtx.p_umutex;
 umutex  =_umtx.umutex;

 p_ucond=_umtx.p_ucond;
 ucond  =_umtx.ucond;

 p_urwlock=_umtx.p_urwlock;
 urwlock  =_umtx.urwlock;

 p__usem=_umtx.p__usem;
 _usem  =_umtx._usem;

const
 DEFAULT_UMUTEX :umutex =(m_owner:0;m_flags:0;m_ceilings:(0,0);m_spare:(0,0));
 DEFAULT_URWLOCK:urwlock=(rw_state:0;rw_flags:0;
                          rw_blocked_readers:0;
                          rw_blocked_writers:0;
                          rw_spare:(0,0,0,0));

function _thr_umutex_trylock(mtx:p_umutex;id:DWORD):Integer;
function _thr_umutex_trylock2(mtx:p_umutex;id:DWORD):Integer;
function _thr_umutex_lock(mtx:p_umutex;id:DWORD):Integer;
function _thr_umutex_lock_spin(mtx:p_umutex;id:DWORD):Integer;
function _thr_umutex_timedlock(mtx:p_umutex;id:DWORD;timeout:ptimespec):Integer;
function _thr_umutex_unlock(mtx:p_umutex;id:DWORD):Integer;
function _thr_rwlock_tryrdlock(rwlock:p_urwlock;flags:Integer):Integer;
function _thr_rwlock_trywrlock(rwlock:p_urwlock):Integer;
function _thr_rwlock_rdlock(rwlock:p_urwlock;flags:Integer;tsp:ptimespec):Integer;
function _thr_rwlock_wrlock(rwlock:p_urwlock;tsp:ptimespec):Integer;
function _thr_rwlock_unlock(rwlock:p_urwlock):Integer;

procedure _thr_umutex_init(mtx:p_umutex);
procedure _thr_urwlock_init(rwl:p_urwlock);
function  __thr_umutex_lock(mtx:p_umutex;id:DWORD):Integer;
function  __thr_umutex_lock_spin(mtx:p_umutex;id:DWORD):Integer;
function  __thr_umutex_timedlock(mtx:p_umutex;id:DWORD;ets:ptimespec):Integer;
function  __thr_umutex_unlock(mtx:p_umutex;id:DWORD):Integer;
function  __thr_umutex_trylock(mtx:p_umutex):Integer;
function  __thr_umutex_set_ceiling(mtx:p_umutex;ceiling:DWORD;oldceiling:PDWORD):Integer;
function  _thr_umtx_wait(mtx:Pointer;id:QWORD;timeout:ptimespec):Integer;
function  _thr_umtx_wait_uint(mtx:Pointer;id:DWORD;timeout:ptimespec;shared:Integer):Integer;
function  _thr_umtx_timedwait_uint(mtx:Pointer;id:DWORD;clockid:Integer;abstime:ptimespec;shared:Integer):Integer;
function  _thr_umtx_wake(mtx:Pointer;nr_wakeup,shared:Integer):Integer;
procedure _thr_ucond_init(cv:p_ucond);
function  _thr_ucond_wait(cv:p_ucond;m:p_umutex;timeout:ptimespec;flags:Integer):Integer;
function  _thr_ucond_signal(cv:p_ucond):Integer;
function  _thr_ucond_broadcast(cv:p_ucond):Integer;
function  __thr_rwlock_rdlock(rwlock:p_urwlock;flags:Integer;tsp:ptimespec):Integer;
function  __thr_rwlock_wrlock(rwlock:p_urwlock;tsp:ptimespec):Integer;
function  __thr_rwlock_unlock(rwlock:p_urwlock):Integer;
procedure _thr_rwl_rdlock(rwlock:p_urwlock);
procedure _thr_rwl_wrlock(rwlock:p_urwlock);
procedure _thr_rwl_unlock(rwlock:p_urwlock);

implementation

uses
 errno,
 thr_init;

function atomic_cmpset_acq_32(p:PDWORD;cmpval,newval:DWORD):DWORD; inline;
begin
 Result:=System.InterlockedCompareExchange(p^,newval,cmpval);
end;

function atomic_cmpset_rel_32(p:PDWORD;cmpval,newval:DWORD):DWORD; inline;
begin
 Result:=System.InterlockedCompareExchange(p^,newval,cmpval);
end;

function _thr_umutex_trylock(mtx:p_umutex;id:DWORD):Integer;
begin
 if (atomic_cmpset_acq_32(@mtx^.m_owner,UMUTEX_UNOWNED,id)<>0) then
  Exit(0);
 if ((mtx^.m_flags and UMUTEX_PRIO_PROTECT)=0) then
  Exit(EBUSY);
 Exit(__thr_umutex_trylock(mtx));
end;

function _thr_umutex_trylock2(mtx:p_umutex;id:DWORD):Integer;
begin
 if (atomic_cmpset_acq_32(@mtx^.m_owner,UMUTEX_UNOWNED,id)<>0) then
  Exit(0);
 if (mtx^.m_owner=UMUTEX_CONTESTED) and
    ((mtx^.m_flags and (UMUTEX_PRIO_PROTECT or UMUTEX_PRIO_INHERIT))=0) then
  if (atomic_cmpset_acq_32(@mtx^.m_owner,UMUTEX_CONTESTED,id or UMUTEX_CONTESTED)<>0) then
   Exit(0);
 Exit(EBUSY);
end;

function _thr_umutex_lock(mtx:p_umutex;id:DWORD):Integer;
begin
 if (_thr_umutex_trylock2(mtx,id)=0) then
  Exit(0);
 Exit(__thr_umutex_lock(mtx,id));
end;

function _thr_umutex_lock_spin(mtx:p_umutex;id:DWORD):Integer;
begin
 if (_thr_umutex_trylock2(mtx, id)=0) then
  Exit(0);
 Exit(__thr_umutex_lock_spin(mtx,id));
end;

function _thr_umutex_timedlock(mtx:p_umutex;id:DWORD;timeout:ptimespec):Integer;
begin
 if (_thr_umutex_trylock2(mtx,id)=0) then
  Exit(0);
 Exit(__thr_umutex_timedlock(mtx,id,timeout));
end;

function _thr_umutex_unlock(mtx:p_umutex;id:DWORD):Integer;
begin
 if (atomic_cmpset_rel_32(@mtx^.m_owner,id,UMUTEX_UNOWNED)<>0) then
  Exit(0);
 Exit(__thr_umutex_unlock(mtx,id));
end;

function _thr_rwlock_tryrdlock(rwlock:p_urwlock;flags:Integer):Integer;
var
 state  :Integer;
 wrflags:DWORD;
begin
 if ((flags and URWLOCK_PREFER_READER)<>0) or ((rwlock^.rw_flags and URWLOCK_PREFER_READER)<>0) then
  wrflags:=URWLOCK_WRITE_OWNER
 else
  wrflags:=URWLOCK_WRITE_OWNER or URWLOCK_WRITE_WAITERS;

 state:=rwlock^.rw_state;
 while ((state and wrflags)=0) do
 begin
  if (URWLOCK_READER_COUNT(state)=URWLOCK_MAX_READERS) then
   Exit(EAGAIN);
  if (atomic_cmpset_acq_32(@rwlock^.rw_state,state,state+1)<>0) then
   Exit(0);
  state:=rwlock^.rw_state;
 end;

 Exit(EBUSY);
end;

function _thr_rwlock_trywrlock(rwlock:p_urwlock):Integer;
var
 state:Integer;
begin
 state:=rwlock^.rw_state;
 while ((state and URWLOCK_WRITE_OWNER)=0) and (URWLOCK_READER_COUNT(state)=0) do
 begin
  if (atomic_cmpset_acq_32(@rwlock^.rw_state,state,state or URWLOCK_WRITE_OWNER)<>0) then
   Exit(0);
  state:=rwlock^.rw_state;
 end;

 Exit(EBUSY);
end;

function _thr_rwlock_rdlock(rwlock:p_urwlock;flags:Integer;tsp:ptimespec):Integer;
begin
 if (_thr_rwlock_tryrdlock(rwlock,flags)=0) then
  Exit(0);
 Exit(__thr_rwlock_rdlock(rwlock,flags,tsp));
end;

function _thr_rwlock_wrlock(rwlock:p_urwlock;tsp:ptimespec):Integer;
begin
 if (_thr_rwlock_trywrlock(rwlock)=0) then
  Exit(0);
 Exit(__thr_rwlock_wrlock(rwlock, tsp));
end;

function _thr_rwlock_unlock(rwlock:p_urwlock):Integer;
var
 state:Integer;
begin
 state:=rwlock^.rw_state;
 if ((state and URWLOCK_WRITE_OWNER)<>0) then
 begin
  if (atomic_cmpset_rel_32(@rwlock^.rw_state,URWLOCK_WRITE_OWNER,0)<>0) then
   Exit(0);
 end else
 begin
  repeat
   if (URWLOCK_READER_COUNT(state)=0) then
    Exit(EPERM);
   if ((state and (URWLOCK_WRITE_WAITERS or URWLOCK_READ_WAITERS))=0) and
      (URWLOCK_READER_COUNT(state)=1) then
   begin
    if (atomic_cmpset_rel_32(@rwlock^.rw_state,state,state-1)<>0) then
     Exit(0);
    state:=rwlock^.rw_state;
   end else
   begin
    break;
   end;
  until false;
 end;
 Exit(__thr_rwlock_unlock(rwlock));
end;

////

procedure _thr_umutex_init(mtx:p_umutex);
begin
 mtx^:=DEFAULT_UMUTEX;
end;

procedure _thr_urwlock_init(rwl:p_urwlock);
begin
 rwl^:=DEFAULT_URWLOCK;
end;

function __thr_umutex_lock(mtx:p_umutex;id:DWORD):Integer;
var
 owner:DWORD;
begin
 if ((mtx^.m_flags and (UMUTEX_PRIO_PROTECT or UMUTEX_PRIO_INHERIT))=0) then
 begin
  repeat
   // wait in kernel
   _umtx_op_err(mtx,UMTX_OP_MUTEX_WAIT,0,nil,nil);

   owner:=mtx^.m_owner;
   if ((owner and (not UMUTEX_CONTESTED))=0) and
      (atomic_cmpset_acq_32(@mtx^.m_owner,owner,id or owner)<>0) then
    Exit(0);
  until false;
 end;

 Exit(_umtx_op_err(mtx,UMTX_OP_MUTEX_LOCK,0,nil,nil));
end;

const
 SPINLOOPS=100;

function __thr_umutex_lock_spin(mtx:p_umutex;id:DWORD):Integer;
var
 owner:DWORD;
 count:Integer;
begin
 if (_thr_is_smp=0) then
  Exit(__thr_umutex_lock(mtx,id));

 if ((mtx^.m_flags and (UMUTEX_PRIO_PROTECT or UMUTEX_PRIO_INHERIT))=0) then
 begin
  repeat
   count:=SPINLOOPS;
   while (count<>0) do
   begin
    Dec(count);
    owner:=mtx^.m_owner;
    if ((owner and (not UMUTEX_CONTESTED))=0) then
    begin
     if (atomic_cmpset_acq_32(
         @mtx^.m_owner,
         owner,id or owner)<>0) then
     begin
      Exit(0);
     end;
    end;
    CPU_SPINWAIT;
   end;

   // wait in kernel */
   _umtx_op_err(mtx,UMTX_OP_MUTEX_WAIT,0,nil,nil);
  until false;
 end;

 Exit(_umtx_op_err(mtx,UMTX_OP_MUTEX_LOCK,0,nil,nil));
end;

function __thr_umutex_timedlock(mtx:p_umutex;id:DWORD;ets:ptimespec):Integer;
var
 timo,cts:timespec;
 owner:DWORD;
begin
 //int ret;

 clock_gettime(CLOCK_REALTIME,@cts);
 TIMESPEC_SUB(@timo,ets,@cts);

 if (timo.tv_sec<0) then
  Exit(ETIMEDOUT);

 repeat
  if ((mtx^.m_flags and (UMUTEX_PRIO_PROTECT or UMUTEX_PRIO_INHERIT))=0) then
  begin
   // wait in kernel
   Result:=_umtx_op_err(mtx,UMTX_OP_MUTEX_WAIT,0,nil,@timo);

   // now try to lock it
   owner:=mtx^.m_owner;
   if ((owner and (not UMUTEX_CONTESTED))=0) and
      (atomic_cmpset_acq_32(@mtx^.m_owner,owner,id or owner)<>0) then
    Exit(0);
  end else
  begin
   Result:=_umtx_op_err(mtx,UMTX_OP_MUTEX_LOCK,0,nil,@timo);
   if (Result=0) then
    break;
  end;
  if (Result=ETIMEDOUT) then
   break;
  clock_gettime(CLOCK_REALTIME,@cts);
  TIMESPEC_SUB(@timo,ets,@cts);
  if (timo.tv_sec<0) or ((timo.tv_sec=0) and (timo.tv_nsec=0)) then
  begin
   Result:=ETIMEDOUT;
   break;
  end;
 until false;

 Exit;
end;

function __thr_umutex_unlock(mtx:p_umutex;id:DWORD):Integer;
label
 unlock;
var
 wake2_avail:Integer;
 flags:DWORD;
 owner:DWORD;
 test:umutex;
begin
 wake2_avail:=0;

 if (wake2_avail=0) then
 begin
  test:=DEFAULT_UMUTEX;

  if (_umtx_op(@test, UMTX_OP_MUTEX_WAKE2, test.m_flags,nil,nil)=-1) then
   wake2_avail:=-1
  else
   wake2_avail:=1;
 end;

 if (wake2_avail<>1) then
  goto unlock;

 flags:=mtx^.m_flags;

 if ((flags and (UMUTEX_PRIO_PROTECT or UMUTEX_PRIO_INHERIT))=0) then
 begin
  repeat
   owner:=mtx^.m_owner;
   if ((owner and (not UMUTEX_CONTESTED))<>id) then
    Exit(EPERM);
  until (atomic_cmpset_rel_32(@mtx^.m_owner,owner,UMUTEX_UNOWNED)<>0);
  if ((owner and UMUTEX_CONTESTED)<>0) then
   _umtx_op_err(mtx, UMTX_OP_MUTEX_WAKE2,flags,nil,nil);
  Exit (0);
 end;
unlock:
 Exit(_umtx_op_err(mtx, UMTX_OP_MUTEX_UNLOCK,0,nil,nil));
end;

function __thr_umutex_trylock(mtx:p_umutex):Integer; inline;
begin
 Exit(_umtx_op_err(mtx, UMTX_OP_MUTEX_TRYLOCK,0,nil,nil));
end;

function __thr_umutex_set_ceiling(mtx:p_umutex;ceiling:DWORD;oldceiling:PDWORD):Integer; inline;
begin
 Exit(_umtx_op_err(mtx,UMTX_OP_SET_CEILING,ceiling,oldceiling,nil));
end;

function _thr_umtx_wait(mtx:Pointer;id:QWORD;timeout:ptimespec):Integer;
begin
 if (timeout<>nil) and
    ((timeout^.tv_sec<0) or ((timeout^.tv_sec=0) and (timeout^.tv_nsec<=0))) then
  Exit(ETIMEDOUT);
 Exit(_umtx_op_err(mtx,UMTX_OP_WAIT,id,nil,timeout));
end;

function _thr_umtx_wait_uint(mtx:Pointer;id:DWORD;timeout:ptimespec;shared:Integer):Integer;
begin
 if (timeout<>nil) and
    ((timeout^.tv_sec<0) or ((timeout^.tv_sec=0) and (timeout^.tv_nsec<=0))) then
  Exit(ETIMEDOUT);

 if (shared<>0) then
  Exit(_umtx_op_err(mtx,UMTX_OP_WAIT_UINT,id,nil,timeout))
 else
  Exit(_umtx_op_err(mtx,UMTX_OP_WAIT_UINT_PRIVATE,id,nil,timeout));
end;

function _thr_umtx_timedwait_uint(mtx:Pointer;id:DWORD;clockid:Integer;abstime:ptimespec;shared:Integer):Integer;
var
 ts,ts2:timespec;
 tsp:ptimespec;
begin
 if (abstime<>nil) then
 begin
  clock_gettime(clockid,@ts);
  TIMESPEC_SUB(@ts2,abstime,@ts);
  if (ts2.tv_sec<0) or ((ts2.tv_sec=0) and (ts2.tv_nsec<=0)) then
   Exit(ETIMEDOUT);
  tsp:=@ts2;
 end else
 begin
  tsp:=nil;
 end;
 if (shared<>0) then
  Exit(_umtx_op_err(mtx,UMTX_OP_WAIT_UINT,id,nil,tsp))
 else
  Exit(_umtx_op_err(mtx,UMTX_OP_WAIT_UINT_PRIVATE,id,nil,tsp));
end;

function _thr_umtx_wake(mtx:Pointer;nr_wakeup,shared:Integer):Integer;
begin
 if (shared<>0) then
  Exit(_umtx_op_err(mtx,UMTX_OP_WAKE,nr_wakeup,nil,nil))
 else
  Exit(_umtx_op_err(mtx,UMTX_OP_WAKE_PRIVATE,nr_wakeup,nil,nil));
end;

procedure _thr_ucond_init(cv:p_ucond); inline;
begin
 FillChar(cv^,SizeOf(ucond),0);
end;

function _thr_ucond_wait(cv:p_ucond;m:p_umutex;timeout:ptimespec;flags:Integer):Integer;
var
 curthread:p_pthread;
begin
 if (timeout<>nil) and
    ((timeout^.tv_sec<0) or ((timeout^.tv_sec=0) and (timeout^.tv_nsec<=0))) then
 begin
  curthread:=_get_curthread();
  _thr_umutex_unlock(m,TID(curthread));
  Exit(ETIMEDOUT);
 end;
 Exit(_umtx_op_err(cv,UMTX_OP_CV_WAIT, flags,m,timeout));
end;

function _thr_ucond_signal(cv:p_ucond):Integer; inline;
begin
 if (cv^.c_has_waiters=0) then
  Exit(0);
 Exit(_umtx_op_err(cv,UMTX_OP_CV_SIGNAL,0,nil, nil));
end;

function _thr_ucond_broadcast(cv:p_ucond):Integer; inline;
begin
 if (cv^.c_has_waiters=0) then
  Exit(0);
 Exit(_umtx_op_err(cv,UMTX_OP_CV_BROADCAST,0,nil,nil));
end;

function __thr_rwlock_rdlock(rwlock:p_urwlock;flags:Integer;tsp:ptimespec):Integer; inline;
begin
 Exit(_umtx_op_err(rwlock,UMTX_OP_RW_RDLOCK,flags,nil,tsp));
end;

function __thr_rwlock_wrlock(rwlock:p_urwlock;tsp:ptimespec):Integer; inline;
begin
 Exit(_umtx_op_err(rwlock,UMTX_OP_RW_WRLOCK,0,nil,tsp));
end;

function __thr_rwlock_unlock(rwlock:p_urwlock):Integer; inline;
begin
 Exit(_umtx_op_err(rwlock,UMTX_OP_RW_UNLOCK,0,nil,nil));
end;

procedure _thr_rwl_rdlock(rwlock:p_urwlock);
var
 ret:Integer;
begin
 repeat
  if (_thr_rwlock_tryrdlock(rwlock,URWLOCK_PREFER_READER)=0) then
   Exit;
  ret:=__thr_rwlock_rdlock(rwlock,URWLOCK_PREFER_READER,nil);
  if (ret=0) then
   Exit;
  if (ret<>EINTR) then
   Writeln('rdlock error');
 until false;
end;

procedure _thr_rwl_wrlock(rwlock:p_urwlock);
var
 ret:Integer;
begin
 repeat
  if (_thr_rwlock_trywrlock(rwlock)=0) then
   Exit;
  ret:=__thr_rwlock_wrlock(rwlock,nil);
  if (ret=0) then
   Exit;
  if (ret<>EINTR) then
   Writeln('wrlock error');
 until false;
end;

procedure _thr_rwl_unlock(rwlock:p_urwlock);
begin
 if (_thr_rwlock_unlock(rwlock)<>0) then
  Writeln('unlock error');
end;


end.

