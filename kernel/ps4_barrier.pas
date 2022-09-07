unit ps4_barrier;

{$mode objfpc}{$H+}

interface

uses
 windows,
 ps4_mutex,
 ps4_cond;

type
 p_pthread_barrierattr_t=^pthread_barrierattr_t;
 pthread_barrierattr_t=^pthread_barrier_attr;
 pthread_barrier_attr=record
  pshared:Integer;
 end;

 p_pthread_barrier_t=^pthread_barrier_t;
 pthread_barrier_t=^pthread_barrier;
 pthread_barrier=record
  valid:DWORD;
  mlock:pthread_mutex;
  mcond:pthread_cond;
  cycle:Integer;
  count:Integer;
  waiters:Integer;
  refcount:Integer;
  destroying:Integer;
 end;

const
 PTHREAD_BARRIER_SERIAL_THREAD=-1;

function ps4_pthread_barrier_init(bar:p_pthread_barrier_t;attr:p_pthread_barrierattr_t;count:DWORD):Integer; SysV_ABI_CDecl;
function ps4_pthread_barrier_destroy(bar:p_pthread_barrier_t):Integer; SysV_ABI_CDecl;
function ps4_pthread_barrier_wait(bar:p_pthread_barrier_t):Integer; SysV_ABI_CDecl;

implementation

uses
 atomic,
 sys_kernel,
 sys_signal;

const
 LIFE_BAR=$BAB1F00D;
 DEAD_BAR=$DEADBEEF;

function bar_init(m,mi:p_pthread_barrier_t;count:DWORD):Integer;
var
 new:pthread_barrier_t;
begin
 new:=AllocMem(SizeOf(pthread_barrier));
 if (new=nil) then Exit(ENOMEM);

 new^.valid:=LIFE_BAR;
 new^.count:=count;

 if (ps4_pthread_mutex_init(@new^.mlock,nil)<>0) then
 begin
  FreeMem(new);
  Exit(EAGAIN);
 end;

 if (ps4_pthread_cond_init(@new^.mcond,nil)<>0) then
 begin
  FreeMem(new);
  Exit(EAGAIN);
 end;

 if CAS(m^,mi^,new) then
 begin
  mi^:=new;
 end else
 begin
  FreeMem(new);
  mi^:=m^;
 end;

 Result:=0;
end;

function _pthread_barrier_init(bar:p_pthread_barrier_t;attr:p_pthread_barrierattr_t;count:DWORD):Integer;
var
 new:pthread_barrier_t;
begin
 if (bar=nil) or (count=0) or (count>High(Integer)) then Exit(EINVAL);

 if (attr<>nil) then
 if (attr^<>nil) then
 if (attr^^.pshared<>0) then //<>PTHREAD_PROCESS_PRIVATE
 begin
  Assert(false,'shared barrier not support!');
  Exit(ENOSYS);
 end;

 _sig_lock;
 Result:=bar_init(bar,@new,count);
 _sig_unlock;
end;

function bar_enter(bar,bvp:p_pthread_barrier_t):Integer;
var
 bv:pthread_barrier_t;
begin
 if (bar=nil) then Exit(EINVAL);
 bv:=bar^;
 if (bv=nil) then Exit(EINVAL);
 if not safe_test(bv^.valid,LIFE_BAR) then Exit(ESRCH);

 ps4_pthread_mutex_lock(@bv^.mlock);

 bvp^:=bv;
 Result:=0;
end;

function _pthread_barrier_destroy(bar:p_pthread_barrier_t):Integer;
var
 bv:pthread_barrier_t;
begin
 Result:=bar_enter(bar,@bv);
 if (Result<>0) then Exit;

 if (bv^.destroying<>0) then //is destroying
 begin
  ps4_pthread_mutex_unlock(@bv^.mlock);
  Exit(EBUSY);
 end;

 bv^.destroying:=1; //mark destroy
 repeat
  if (bv^.waiters>0) then
  begin
   bv^.destroying:=0; //unmark
   ps4_pthread_mutex_unlock(@bv^.mlock);
   Exit(EBUSY);
  end;
  if (bv^.refcount<>0) then
  begin
   ps4_pthread_cond_wait (@bv^.mcond,@bv^.mlock);
   ps4_pthread_mutex_lock(@bv^.mlock);
  end else
  begin
   Break;
  end;
 until false;

 bar^:=nil;
 bv^.valid:=DEAD_BAR; //mark free
 bv^.destroying:=0;   //unmark

 ps4_pthread_mutex_unlock(@bv^.mlock);

 ps4_pthread_mutex_destroy(@bv^.mlock);
 ps4_pthread_cond_destroy (@bv^.mcond);

 SwFreeMem(bv);
 Result:=0;
end;

function _pthread_barrier_wait(bar:p_pthread_barrier_t):Integer;
var
 bv:pthread_barrier_t;
 cycle:Integer;
begin
 Result:=bar_enter(bar,@bv);
 if (Result<>0) then Exit;

 if (System.InterlockedIncrement(bv^.waiters)=bv^.count) then
 begin
  // Current thread is lastest thread
  bv^.waiters:=0;
  System.InterlockedIncrement(bv^.cycle);
  ps4_pthread_cond_broadcast(@bv^.mcond);
  ps4_pthread_mutex_unlock  (@bv^.mlock);
  Result:=PTHREAD_BARRIER_SERIAL_THREAD;
 end else
 begin
  cycle:=bv^.cycle;
  System.InterlockedIncrement(bv^.refcount);

  repeat
   ps4_pthread_cond_wait (@bv^.mcond,@bv^.mlock);
   ps4_pthread_mutex_lock(@bv^.mlock);
   // test cycle to avoid bogus wakeup
  until (cycle<>bv^.cycle);

  if (System.InterlockedDecrement(bv^.refcount)=0) and (bv^.destroying<>0) then
  begin
   ps4_pthread_cond_broadcast(@bv^.mcond);
  end;

  ps4_pthread_mutex_unlock(@bv^.mlock);
  Result:=0;
 end;
end;

function ps4_pthread_barrier_init(bar:p_pthread_barrier_t;attr:p_pthread_barrierattr_t;count:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=_pthread_barrier_init(bar,attr,count);
end;

function ps4_pthread_barrier_destroy(bar:p_pthread_barrier_t):Integer; SysV_ABI_CDecl;
begin
 Result:=_pthread_barrier_destroy(bar);
end;

function ps4_pthread_barrier_wait(bar:p_pthread_barrier_t):Integer; SysV_ABI_CDecl;
begin
 Result:=_pthread_barrier_wait(bar);
end;

end.

