unit thr_init;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 time,
 pthread,
 pthread_md,
 thr_umtx,
 thr_private,
 thr;

var
 _usrstack             :Pointer=Pointer($FFFFFFEFFF); //USRSTACK kern.usrstack
 _thr_initial          :p_pthread;
 _thread_event_mask    :Integer;
 _thread_last_event    :p_pthread;
 _thread_list          :pthreadlist=(tqh_first:nil;tqh_last:@_thread_list.tqh_first);
 _thread_gc_list       :pthreadlist=(tqh_first:nil;tqh_last:@_thread_gc_list.tqh_first);
 _thread_active_threads:Integer=0;
 _thr_atfork_list      :atfork_head=(tqh_first:nil;tqh_last:@_thr_atfork_list.tqh_first);
 _thr_atfork_lock      :urwlock=(rw_state:0;rw_flags:0;
                                 rw_blocked_readers:0;
                                 rw_blocked_writers:0;
                                 rw_spare:(0,0,0,0));

const
 _thr_priorities       :array[0..2] of pthread_prio=(
  (pri_min:256;pri_max:767;pri_default:700), //FIFO
  (pri_min:768;pri_max:959;pri_default:900), //OTHER
  (pri_min:256;pri_max:767;pri_default:700)  //RR
 );

var
 _pthread_attr_default :pthread_attr=(
  sched_policy  :SCHED_FIFO;
  sched_inherit :PTHREAD_INHERIT_SCHED;
  prio          :700;
  suspend       :THR_CREATE_RUNNING;
  flags         :PTHREAD_SCOPE_SYSTEM;
  _align        :0;
  stackaddr_attr:nil;
  stacksize_attr:$10000;
  guardsize_attr:0;
  cpuset        :0;
  cpusetsize    :0
 );

 _pthread_mutexattr_default:pthread_mutex_attr=(
  m_type    :PTHREAD_MUTEX_DEFAULT;
  m_protocol:PTHREAD_PRIO_NONE;
  m_ceiling :0
 );

 _pthread_mutexattr_adaptive_default:pthread_mutex_attr=(
  m_type    :PTHREAD_MUTEX_ADAPTIVE;
  m_protocol:PTHREAD_PRIO_NONE;
  m_ceiling :0
 );

 _pthread_condattr_default:pthread_cond_attr=(
  c_pshared:PTHREAD_PROCESS_PRIVATE;
  c_clockid:CLOCK_REALTIME;
 );

 _thr_pid:Integer;

 _thr_is_smp        :Integer=1; //kern.smp.cpus
 _thr_guard_default :Integer;
 _thr_stack_default :QWORD  =THR_STACK_DEFAULT;
 _thr_stack_initial :QWORD  =THR_STACK_INITIAL;
 _thr_page_size     :Integer=4*1024; //getpagesize

 _thr_spinloops :Integer;
 _thr_yieldloops:Integer;
 _gc_count      :Integer;

 _mutex_static_lock :umutex= (m_owner:0;m_flags:0;m_ceilings:(0,0);m_spare:(0,0));
 _cond_static_lock  :umutex= (m_owner:0;m_flags:0;m_ceilings:(0,0);m_spare:(0,0));
 _rwlock_static_lock:umutex= (m_owner:0;m_flags:0;m_ceilings:(0,0);m_spare:(0,0));
 _keytable_lock     :umutex= (m_owner:0;m_flags:0;m_ceilings:(0,0);m_spare:(0,0));
 _thr_list_lock     :urwlock=(rw_state:0;rw_flags:0;
                              rw_blocked_readers:0;
                              rw_blocked_writers:0;
                              rw_spare:(0,0,0,0));
 _thr_event_lock    :umutex= (m_owner:0;m_flags:0;m_ceilings:(0,0);m_spare:(0,0));

 init_once:Integer=0;

procedure _libpthread_init(curthread:p_pthread);

implementation

uses
 sys_mmap;

const
 g_user_stacksize=$10000;

procedure init_private;
begin
 _thr_umutex_init(@_mutex_static_lock);
 _thr_umutex_init(@_cond_static_lock);
 _thr_umutex_init(@_rwlock_static_lock);
 _thr_umutex_init(@_keytable_lock);
 _thr_urwlock_init(@_thr_atfork_lock);
 _thr_umutex_init(@_thr_event_lock);
 //_thr_once_init();
 //_thr_spinlock_init();
 //_thr_list_init();
 //_thr_wake_addr_init();
 //_sleepq_init();
 if (init_once=0) then
 begin
  //mib[0]:=1;
  //mib[1]:=33;
  //len:=8;
  //ret:=sysctl(mib,2,@_usrstack,@len,nil,0);
  //if (ret=-1) then
  //begin
  // Assert(false,'Cannot get kern.usrstack from sysctl');
  //end;
  //len:=4;
  //sysctlbyname('kern.smp.cpus',@_thr_is_smp,@len,0,0);
  //_thr_is_smp:=(1<_thr_is_smp);
  //_thr_page_size:=getpagesize();
  _pthread_attr_default.guardsize_attr:=_thr_page_size;
  _thr_guard_default:=_pthread_attr_default.guardsize_attr;
  //ret:=IsSystemProcess(getpid());
  _thr_atfork_list.tqh_first:=nil;
  _pthread_attr_default.stacksize_attr:=$200000;
  //if (ret<>1) then
  begin
   _pthread_attr_default.stacksize_attr:=g_user_stacksize;
  end;
 end;
 init_once:=1;
end;

procedure init_main_thread(thread:p_pthread);
var
 ret:Pointer;
 param:sched_param;
begin
 thr_self(@thread^.tid);

 thread^.attr:=_pthread_attr_default;

 ret:=mmap(_usrstack-_thr_guard_default-_thr_stack_initial,_thr_guard_default,0,MAP_ANON,-1,0);

 if (ret=MAP_FAILED) then
 begin
  Assert(false,'Cannot allocate red zone for initial thread');
  Exit;
 end;
 thread^.attr.stackaddr_attr:=(_usrstack-_thr_stack_initial);
 thread^.attr.stacksize_attr:=_thr_stack_initial;
 thread^.attr.guardsize_attr:=_thr_guard_default;
 thread^.attr.flags:=thread^.attr.flags or THR_STACK_USER;

 thread^.magic:=THR_MAGIC;

 thread^.cancel_enable:=1;
 thread^.cancel_async:=0;

 TAILQ_INIT(@thread^.mutexq);
 TAILQ_INIT(@thread^.pp_mutexq);

 thread^.state:=PS_RUNNING;

 //_thr_getscheduler(thread^.tid,@thread^.attr.sched_policy,@param);

 thread^.attr.prio:=param.sched_priority;
 thread^.unwind_stackend:=_usrstack;
end;

procedure _libpthread_init(curthread:p_pthread);
var
 first:Integer;
begin
 if (_thr_initial<>nil) and (curthread=nil) then Exit;

 _thr_pid:=getpid();

 init_private;

 first:=0;

 if (curthread=nil) then
 begin
  //curthread:=_thr_alloc(nil);
  if (curthread=nil) then
  begin
   Assert(false,'Can''t allocate initial thread');
  end;
  init_main_thread(curthread);
  first:=1;
 end;

 THR_LIST_ADD(curthread);
 _thread_active_threads:=1;

 if (first<>0) then
 begin
  _thr_initial:=curthread;
  _tcb_set(curthread^.tcb);
 end;
end;


end.







