unit thr_init;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
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

implementation

end.

