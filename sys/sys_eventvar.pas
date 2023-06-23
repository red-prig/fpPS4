unit sys_eventvar;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 sys_event,
 kern_mtx,
 vselinfo,
 vfiledesc;

const
 KQ_NEVENTS  =  8;  // minimize copy{in,out} calls
 KQEXTENT    =256;  // linear growth by this amount

 KQ_SEL      =$01;
 KQ_SLEEP    =$02;
 KQ_FLUXWAIT =$04;  // waiting for a in flux kn
 KQ_ASYNC    =$08;
 KQ_CLOSING  =$10;
 KQ_TASKSCHED=$20;  // task scheduled
 KQ_TASKDRAIN=$40;  // waiting for task to drain

type
 pp_kqueue=^p_kqueue;
 p_kqueue=^t_kqueue;
 t_kqueue=record
  kq_lock      :mtx;
  kq_refcnt    :Integer;
  kq_list      :TAILQ_ENTRY; // (kqueue)
  kq_head      :TAILQ_HEAD;  // (knote) list of pending event
  kq_count     :Integer;     // number of pending events
  kq_sel       :t_selinfo;
  //kq_sigio     :p_sigio; (sigio)
  //kq_fdp       :p_filedesc;
  kq_state     :Integer;
  kq_knlistsize:Integer;     // size of knlist
  kq_knlist    :p_klist;     // list of knotes
  kq_knhashmask:QWORD;       // size of knhash
  kq_knhash    :p_klist;     // hash table for knotes
  //kq_task:task;
 end;


implementation

end.

