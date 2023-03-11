unit thr_init;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 pthread,
 thr_umtx,
 thr_private;

var
 _usrstack             :Pointer;
 _thr_initial          :p_pthread;
 _thread_event_mask    :Integer;
 _thread_last_event    :p_pthread;
 _thread_list          :pthreadlist=(tqh_first:nil;tqh_last:@_thread_list.tqh_first);
 _thread_gc_list       :pthreadlist=(tqh_first:nil;tqh_last:@_thread_gc_list.tqh_first);
 _thread_active_threads:Integer=1;
 _thr_atfork_list      :atfork_head=(tqh_first:nil;tqh_last:@_thr_atfork_list.tqh_first);
 _thr_atfork_lock      :urwlock=(rw_state:0;rw_flags:0;
                                 rw_blocked_readers:0;
                                 rw_blocked_writers:0;
                                 rw_spare:(0,0,0,0));

 _thr_priorities       :array[0..2] of pthread_prio=(
  (pri_min:256;pri_max:767;pri_default:700), //FIFO
  (pri_min:768;pri_max:959;pri_default:900), //OTHER
  (pri_min:256;pri_max:767;pri_default:700)  //RR
 );

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

 _thr_pid:Integer;

const
 _thr_is_smp:Integer=1;
 _thr_guard_default =THR_STACK_DEFAULT;
 _thr_stack_default =THR_STACK_DEFAULT;
 _thr_stack_initial =THR_STACK_INITIAL;
 _thr_page_size     =4*1024;

var
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

implementation

end.







