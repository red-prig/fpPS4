unit kern_event;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 systm,
 time,
 sys_event,
 sys_eventvar,
 kern_thr,
 kern_mtx,
 kern_synch,
 kern_descrip,
 vuio,
 vfile,
 vfiledesc,
 vfcntl,
 vcapability,
 vselinfo,
 vpoll,
 vstat;

//TASKQUEUE_DEFINE_THREAD(kqueue);

function  kevent_copyout   (arg:Pointer;kevp:p_kevent;count:Integer):Integer;
function  kevent_copyin    (arg:Pointer;kevp:p_kevent;count:Integer):Integer;
function  kqueue_register  (kq:p_kqueue;kev:p_kevent):Integer;
function  kqueue_acquire   (fp:p_file;kqp:pp_kqueue):Integer;
procedure kqueue_release   (kq:p_kqueue;locked:Integer);
function  kqueue_expand    (kq:p_kqueue;fops:p_filterops;ident:ptruint):Integer;
procedure kqueue_task      (arg:Pointer;pending:Integer);
function  kqueue_scan      (kq:p_kqueue;
                            maxevents:Integer;
                            k_ops:p_kevent_copyops;
                            tsp:ptimespec;
                            keva:p_kevent):Integer;
procedure kqueue_wakeup    (kq:p_kqueue);
function  kqueue_fo_find   (filt:Integer):p_filterops;
procedure kqueue_fo_release(filt:Integer);

function  kqueue_read    (fp:p_file;uio:p_uio;flags:Integer):Integer;
function  kqueue_write   (fp:p_file;uio:p_uio;flags:Integer):Integer;
function  kqueue_truncate(fp:p_file;length:Int64):Integer;
function  kqueue_ioctl   (fp:p_file;cmd:QWORD;data:Pointer):Integer;
function  kqueue_poll    (fp:p_file;events:Integer):Integer;
function  kqueue_kqfilter(fp:p_file;kn:p_knote):Integer;
function  kqueue_stat    (fp:p_file;st:p_stat):Integer;
function  kqueue_close   (fp:p_file):Integer;

const
 kqueueops:fileops=(
  fo_read    :@kqueue_read;
  fo_write   :@kqueue_write;
  fo_truncate:@kqueue_truncate;
  fo_ioctl   :@kqueue_ioctl;
  fo_poll    :@kqueue_poll;
  fo_kqfilter:@kqueue_kqfilter;
  fo_stat    :@kqueue_stat;
  fo_close   :@kqueue_close;
  fo_chmod   :@invfo_chmod;
  fo_chown   :@invfo_chown;
 );

function  knote_attach (kn:p_knote;kq:p_kqueue):Integer;
procedure knote_drop   (kn:p_knote);
procedure knote_enqueue(kn:p_knote);
procedure knote_dequeue(kn:p_knote);
procedure knote_init   (); //SYSINIT(knote, SI_SUB_PSEUDO, SI_ORDER_ANY, knote_init, nil);
function  knote_alloc  ():p_knote;
procedure knote_free   (kn:p_knote);

procedure knlist_add     (knl:p_knlist;kn:p_knote;islocked:Integer);
procedure knlist_remove  (knl:p_knlist;kn:p_knote;islocked:Integer);
function  knlist_empty   (knl:p_knlist):Integer;
procedure knlist_init    (knl:p_knlist;lock,kl_lock,kl_unlock,kl_assert_locked,kl_assert_unlocked:Pointer);
procedure knlist_init_mtx(knl:p_knlist;lock:p_mtx);
procedure knlist_destroy (knl:p_knlist);

procedure knlist_clear   (knl:p_knlist;islocked:Integer);
procedure knlist_delete  (knl:p_knlist;islocked:Integer);
procedure knote_fdclose(fd:Integer);

procedure knote(list:p_knlist;hint:QWORD;lockflags:Integer);

procedure KNOTE_LOCKED  (list:p_knlist;hint:QWORD);
procedure KNOTE_UNLOCKED(list:p_knlist;hint:QWORD);
function  M_KNLIST_EMPTY(list:p_knlist):Boolean;

function  sys_kqueue():Integer;
function  sys_kevent(fd:Integer;
                     changelist:Pointer;
                     nchanges:Integer;
                     eventlist:Pointer;
                     nevents:Integer;
                     timeout:Pointer):Integer;

var
 g_p_klist:t_knlist; //Knotes attached to this proc

implementation

uses
 errno,
 md_proc,
 md_time,
 kern_sig,
 kern_sx,
 vfs_subr,
 subr_hash,
 vsys_generic;

//static MALLOC_DEFINE(M_KQUEUE, 'kqueue', 'memory for kqueue system');

{
 * This lock is used if multiple kq locks are required.  This possibly
 * should be made into a per proc lock.
 }
var
 kq_global     :mtx; //MTX_SYSINIT(kq_global, @kq_global, 'kqueue order', MTX_DEF);
 filterops_lock:mtx; //MTX_SYSINIT(kqueue_filterops, @filterops_lock, 'protect sysfilt_ops', MTX_DEF);
 knlist_lock   :mtx; //MTX_SYSINIT(knlist_lock, @knlist_lock, 'knlist lock for lockless objects', MTX_DEF);

 kq_ncallouts :Integer=0;
 kq_calloutmax:Integer=(4 * 1024);

procedure KNOTE_LOCKED(list:p_knlist;hint:QWORD); inline;
begin
 knote(list, hint, KNF_LISTLOCKED);
end;

procedure KNOTE_UNLOCKED(list:p_knlist;hint:QWORD); inline;
begin
 knote(list, hint, 0);
end;

function M_KNLIST_EMPTY(list:p_knlist):Boolean; inline;
begin
 Result:=SLIST_EMPTY(@list^.kl_list);
end;

procedure KQ_GLOBAL_LOCK(lck:p_mtx;var haslck:Integer); inline;
begin
 if (haslck=0) then
  mtx_lock(lck^);
 haslck:=1;
end;

procedure KQ_GLOBAL_UNLOCK(lck:p_mtx;var haslck:Integer); inline;
begin
 if (haslck<>0) then
  mtx_unlock(lck^);
 haslck:=0;
end;

procedure KQ_LOCK(kq:p_kqueue); inline;
begin
 mtx_lock(kq^.kq_lock);
end;

procedure KQ_FLUX_WAKEUP(kq:p_kqueue); inline;
begin
 if ((kq^.kq_state and KQ_FLUXWAIT)=KQ_FLUXWAIT) then
 begin
  kq^.kq_state:=kq^.kq_state and (not KQ_FLUXWAIT);
  wakeup(kq);
 end;
end;

procedure KQ_UNLOCK_FLUX(kq:p_kqueue); inline;
begin
 KQ_FLUX_WAKEUP(kq);
 mtx_unlock(kq^.kq_lock);
end;

procedure KQ_UNLOCK(kq:p_kqueue); inline;
begin
 mtx_unlock(kq^.kq_lock);
end;

procedure KQ_OWNED(kq:p_kqueue); inline;
begin
 mtx_assert(kq^.kq_lock);
end;

procedure KQ_NOTOWNED(kq:p_kqueue); inline;
begin
 //
end;

{ XXX - ensure not KN_INFLUX?? }
procedure KNOTE_ACTIVATE(kn:p_knote;islock:Integer);
begin
 if (islock<>0) then
  mtx_assert(p_kqueue(kn^.kn_kq)^.kq_lock)
 else
  KQ_LOCK(kn^.kn_kq);

 kn^.kn_status:=kn^.kn_status or KN_ACTIVE;
 if ((kn^.kn_status and (KN_QUEUED or KN_DISABLED))=0) then
  knote_enqueue(kn);

 if (islock=0) then
  KQ_UNLOCK(kn^.kn_kq);
end;

procedure KN_LIST_LOCK(kn:p_knote); inline;
begin
 if (kn^.kn_knlist<>nil) then
  kn^.kn_knlist^.kl_lock(kn^.kn_knlist^.kl_lockarg);
end;

procedure KN_LIST_UNLOCK(kn:p_knote); inline;
begin
 if (kn^.kn_knlist<>nil) then
  kn^.kn_knlist^.kl_unlock(kn^.kn_knlist^.kl_lockarg);
end;

procedure KNL_ASSERT_LOCKED(knl:p_knlist); inline;
begin
 knl^.kl_assert_locked(knl^.kl_lockarg);
end;

procedure KNL_ASSERT_UNLOCKED(knl:p_knlist); inline;
begin
 knl^.kl_assert_unlocked(knl^.kl_lockarg);
end;

procedure KNL_ASSERT_LOCK(knl:p_knlist;islocked:Integer); inline;
begin
 if (islocked<>0) then
  KNL_ASSERT_LOCKED(knl)
 else
  KNL_ASSERT_UNLOCKED(knl);
end;

const
 KN_HASHSIZE=64; { XXX should be tunable }

function KN_HASH(val,mask:QWORD):QWORD; inline;
begin
 Result:=(val xor (val shr 8)) and mask;
end;

procedure filt_kqdetach   (kn:p_knote);                          forward;
function  filt_kqueue     (kn:p_knote;hint:QWORD):Integer;       forward;
function  filt_procattach (kn:p_knote):Integer;                  forward;
procedure filt_procdetach (kn:p_knote);                          forward;
function  filt_proc       (kn:p_knote;hint:QWORD):Integer;       forward;
function  filt_fileattach (kn:p_knote):Integer;                  forward;
procedure filt_timerexpire(knx:Pointer);                         forward;
function  filt_timerattach(kn:p_knote):Integer;                  forward;
procedure filt_timerdetach(kn:p_knote);                          forward;
function  filt_timer      (kn:p_knote;hint:QWORD):Integer;       forward;
function  filt_userattach (kn:p_knote):Integer;                  forward;
procedure filt_userdetach (kn:p_knote);                          forward;
function  filt_user       (kn:p_knote;hint:QWORD):Integer;       forward;
procedure filt_usertouch  (kn:p_knote;kev:p_kevent;_type:QWORD); forward;

const
 file_filtops:t_filterops=(
  f_isfd  :1;
  _align  :0;
  f_attach:@filt_fileattach;
 );

 kqread_filtops:t_filterops=(
  f_isfd  :1;
  _align  :0;
  f_attach:nil;
  f_detach:@filt_kqdetach;
  f_event :@filt_kqueue;
 );

 //XXX - move to kern_proc.c?
 proc_filtops:t_filterops=(
  f_isfd  :0;
  _align  :0;
  f_attach:@filt_procattach;
  f_detach:@filt_procdetach;
  f_event :@filt_proc;
 );

 timer_filtops:t_filterops=(
  f_isfd  :0;
  _align  :0;
  f_attach:@filt_timerattach;
  f_detach:@filt_timerdetach;
  f_event :@filt_timer;
 );

 user_filtops:t_filterops=(
  f_isfd  :0;
  _align  :0;
  f_attach:@filt_userattach;
  f_detach:@filt_userdetach;
  f_event :@filt_user;
  f_touch :@filt_usertouch;
 );


function filt_nullattach(kn:p_knote):Integer;
begin
 Exit(ENXIO);
end;

const
 null_filtops:t_filterops=(
  f_isfd  :0;
  _align  :0;
  f_attach:@filt_nullattach;
 );

type
 t_sysfilt_ops=record
  fop:p_filterops;
  ref:Integer;
 end;

{
 * Table for for all system-defined filters.
 }
var
 sysfilt_ops:array[0..EVFILT_SYSCOUNT-1] of t_sysfilt_ops=(
  (fop:@file_filtops ;ref:0),   { EVFILT_READ                 }
  (fop:@file_filtops ;ref:0),   { EVFILT_WRITE                }
  (fop:@null_filtops ;ref:0),   { EVFILT_AIO                  }
  (fop:@file_filtops ;ref:0),   { EVFILT_VNODE                }
  (fop:@proc_filtops ;ref:0),   { EVFILT_PROC                 }
  (fop:@sig_filtops  ;ref:0),   { EVFILT_SIGNAL               }
  (fop:@timer_filtops;ref:0),   { EVFILT_TIMER                }
  (fop:@null_filtops ;ref:0),   { former EVFILT_NETDEV        }
  (fop:@fs_filtops   ;ref:0),   { EVFILT_FS                   }
  (fop:@null_filtops ;ref:0),   { EVFILT_LIO                  }
  (fop:@user_filtops ;ref:0),   { EVFILT_USER                 }
  (fop:nil           ;ref:0),   { EVFILT_POLLING              } //SCE
  (fop:nil           ;ref:0),   { EVFILT_DISPLAY              } //SCE
  (fop:nil           ;ref:0),   { EVFILT_GRAPHICS_CORE        } //SCE
  (fop:nil           ;ref:0),   { EVFILT_HRTIMER              } //SCE
  (fop:nil           ;ref:0),   { EVFILT_UVD_TRAP             } //SCE
  (fop:nil           ;ref:0),   { EVFILT_VCE_TRAP             } //SCE
  (fop:nil           ;ref:0),   { EVFILT_SDMA_TRAP            } //SCE
  (fop:nil           ;ref:0),   { EVFILT_REG_EV               } //SCE
  (fop:nil           ;ref:0),   { EVFILT_GPU_EXCEPTION        } //SCE
  (fop:nil           ;ref:0),   { EVFILT_GPU_SYSTEM_EXCEPTION } //SCE
  (fop:nil           ;ref:0),   { EVFILT_GPU_DBGGC_EV         } //SCE
  (fop:nil           ;ref:0)    { EVFILT_CPUMODE              } //SCE
 );

{
 * Simple redirection for all cdevsw style objects to call their fo_kqfilter
 * method.
 }
function filt_fileattach(kn:p_knote):Integer;
begin
 Exit(fo_kqfilter(kn^.kn_fp, kn));
end;

{ARGSUSED}
function kqueue_kqfilter(fp:p_file;kn:p_knote):Integer;
var
 kq:p_kqueue;
begin
 kq:=p_file(kn^.kn_fp)^.f_data;

 if (kn^.kn_filter<>EVFILT_READ) then
  Exit(EINVAL);

 kn^.kn_status:=kn^.kn_status or KN_KQUEUE;
 kn^.kn_fop   :=@kqread_filtops;

 knlist_add(@kq^.kq_sel.si_note, kn, 0);

 Exit(0);
end;

procedure filt_kqdetach(kn:p_knote);
var
 kq:p_kqueue;
begin
 kq:=p_file(kn^.kn_fp)^.f_data;

 knlist_remove(@kq^.kq_sel.si_note, kn, 0);
end;

{ARGSUSED}
function filt_kqueue(kn:p_knote;hint:QWORD):Integer;
var
 kq:p_kqueue;
begin
 kq:=p_file(kn^.kn_fp)^.f_data;

 kn^.kn_data:=kq^.kq_count;
 Exit(ord(kn^.kn_data>0));
end;

{ XXX - move to kern_proc.c?  }
function filt_procattach(kn:p_knote):Integer;
var
 immediate:Integer;
begin
 immediate:=0;

 if (Integer(kn^.kn_id)<>g_pid) then Exit(ESRCH);

 if ((p_leader.p_flag and P_WEXIT)<>0) then
 begin
  immediate:=1;
 end;

 kn^.kn_ptr.p_proc:=nil;
 kn^.kn_flags:=kn^.kn_flags or EV_CLEAR;  { automatically set }

 {
  * internal flag indicating registration done by kernel
  }
 if ((kn^.kn_flags and EV_FLAG1)<>0) then
 begin
  kn^.kn_data  :=kn^.kn_sdata;  { ppid }
  kn^.kn_fflags:=NOTE_CHILD;
  kn^.kn_flags :=kn^.kn_flags and (not EV_FLAG1);
 end;

 if (immediate=0) then
 begin
  knlist_add(@g_p_klist, kn, 1);
 end;

 {
  * Immediately activate any exit notes if the target process is a
  * zombie.  This is necessary to handle the case where the target
  * process, e.g. a child, dies before the kevent is registered.
  }
 if (immediate<>0) and (filt_proc(kn, NOTE_EXIT)<>0) then
 begin
  KNOTE_ACTIVATE(kn, 0);
 end;

 PROC_UNLOCK();

 Exit(0);
end;

{
 * The knote may be attached to a different process, which may exit,
 * leaving nothing for the knote to be attached to.  So when the process
 * exits, the knote is marked as DETACHED and also flagged as ONESHOT so
 * it will be deleted when read out.  However, as part of the knote deletion,
 * this routine is called, so a check is needed to avoid actually performing
 * a detach, because the original process does not exist any more.
 }
{ XXX - move to kern_proc.c?  }
procedure filt_procdetach (kn:p_knote);
begin
 knlist_remove(@g_p_klist, kn, 0);
end;

procedure knlist_remove_inevent(knl:p_knlist;kn:p_knote); forward;

{ XXX - move to kern_proc.c?  }
function filt_proc(kn:p_knote;hint:QWORD):Integer;
var
 event:DWORD;
begin

 {
  * mask off extra data
  }
 event:=DWORD(hint) and NOTE_PCTRLMASK;

 {
  * if the user is interested in this event, record it.
  }
 if ((kn^.kn_sfflags and event)<>0) then
 begin
  kn^.kn_fflags:=kn^.kn_fflags or event;
 end;

 {
  * process is gone, so flag the event as finished.
  }
 if (event=NOTE_EXIT) then
 begin
  if ((kn^.kn_status and KN_DETACHED)=0) then
  begin
   knlist_remove_inevent(@g_p_klist, kn);
  end;
  kn^.kn_flags:=kn^.kn_flags or (EV_EOF or EV_ONESHOT);
  kn^.kn_ptr.p_proc:=nil;
  if ((kn^.kn_fflags and NOTE_EXIT)<>0) then
  begin
   kn^.kn_data:=0;
   //kn^.kn_data:=p^.p_xstat;
  end;
  if (kn^.kn_fflags=0) then
  begin
   kn^.kn_flags:=kn^.kn_flags or EV_DROP;
  end;
  Exit(1);
 end;

 Exit(ord(kn^.kn_fflags<>0));
end;

{
 * Called when the process forked. It mostly does the same as the
 * knote(), activating all knotes registered to be activated when the
 * process forked. Additionally, for each knote attached to the
 * parent, check whether user wants to track the new process. If so
 * attach a new knote to it, and immediately report an event with the
 * child's pid.
 }
procedure knote_fork(list:p_knlist;pid:Integer);
label
 _continue;
var
 kq:p_kqueue;
 kn:p_knote;
 kev:t_kevent;
 error:Integer;
begin
 if (list=nil) then Exit;

 list^.kl_lock(list^.kl_lockarg);

 kn:=SLIST_FIRST(@list^.kl_list);
 while (kn<>nil) do //SLIST_FOREACH
 begin
  if ((kn^.kn_status and KN_INFLUX)=KN_INFLUX) then
  begin
   goto _continue;
  end;

  kq:=kn^.kn_kq;
  KQ_LOCK(kq);
  if ((kn^.kn_status and (KN_INFLUX or KN_SCAN))=KN_INFLUX) then
  begin
   KQ_UNLOCK(kq);
   goto _continue;
  end;

  {
   * The same as knote(), activate the event.
   }
  if ((kn^.kn_sfflags and NOTE_TRACK)=0) then
  begin
   kn^.kn_status:=kn^.kn_status or KN_HASKQLOCK;
   if ((kn^.kn_fop^.f_event(kn, NOTE_FORK))<>0) then
   begin
    KNOTE_ACTIVATE(kn, 1);
   end;
   kn^.kn_status:=kn^.kn_status and (not KN_HASKQLOCK);
   KQ_UNLOCK(kq);
   goto _continue;
  end;

  {
   * The NOTE_TRACK case. In addition to the activation
   * of the event, we need to register new event to
   * track the child. Drop the locks in preparation for
   * the call to kqueue_register().
   }
  kn^.kn_status:=kn^.kn_status or KN_INFLUX;
  KQ_UNLOCK(kq);
  list^.kl_unlock(list^.kl_lockarg);

  {
   * Activate existing knote and register a knote with
   * new process.
   }
  kev.ident :=pid;
  kev.filter:=kn^.kn_filter;
  kev.flags :=kn^.kn_flags or EV_ADD or EV_ENABLE or EV_FLAG1;
  kev.fflags:=kn^.kn_sfflags;
  kev.data  :=kn^.kn_id;  { parent }
  kev.udata :=kn^.kn_kevent.udata;{ preserve udata }

  error:=kqueue_register(kq, @kev);
  if (error<>0) then
  begin
   kn^.kn_fflags:=kn^.kn_fflags or NOTE_TRACKERR;
  end;
  if ((kn^.kn_fop^.f_event(kn, NOTE_FORK))<>0) then
  begin
   KNOTE_ACTIVATE(kn, 0);
  end;
  KQ_LOCK(kq);
  kn^.kn_status:=kn^.kn_status and (not KN_INFLUX);
  KQ_UNLOCK_FLUX(kq);
  list^.kl_lock(list^.kl_lockarg);
  //
  _continue:
  kn:=SLIST_NEXT(kn,@kn^.kn_selnext);
 end; //SLIST_FOREACH
 list^.kl_unlock(list^.kl_lockarg);
end;

{
 * XXX: EVFILT_TIMER should perhaps live in kern_time.c beside the
 * interval timer support code.
 }
function timertoticks(data:Int64):Integer;
begin
 Exit(tvtohz(data));
end;

procedure filt_timerexpire(knx:Pointer);
var
 kn:p_knote;
begin
 kn:=knx;
 //struct callout *calloutp;

 Inc(kn^.kn_kevent.data);
 KNOTE_ACTIVATE(kn, 0); { XXX - handle locking }

 {
  * timertoticks() uses tvtohz() which always adds 1 to allow
  * for the time until the next clock interrupt being strictly
  * less than 1 clock tick.  We don't want that here since we
  * want to appear to be in sync with the clock interrupt even
  * when we're delayed.
  }
 if ((kn^.kn_flags and EV_ONESHOT)<>EV_ONESHOT) then
 begin
  //calloutp:=kn^.kn_hook;
  //callout_reset_curcpu(calloutp, timertoticks(kn^.kn_sdata) - 1, @filt_timerexpire, kn);
 end;
end;

{
 * data contains amount of time to sleep, in milliseconds
 }
function filt_timerattach(kn:p_knote):Integer;
begin
 //struct callout *calloutp;

 System.InterlockedIncrement(kq_ncallouts);

 if (kq_ncallouts >= kq_calloutmax) then
 begin
  System.InterlockedDecrement(kq_ncallouts);
  Exit(ENOMEM);
 end;

 kn^.kn_flags:=kn^.kn_flags or EV_CLEAR;  { automatically set }
 kn^.kn_status:=kn^.kn_status and (not KN_DETACHED);  { knlist_add usually sets it }

 //calloutp:=malloc(sizeof(calloutp), M_KQUEUE, M_WAITOK);
 //callout_init(calloutp, CALLOUT_MPSAFE);
 //kn^.kn_hook:=calloutp;
 //callout_reset_curcpu(calloutp, timertoticks(kn^.kn_sdata), @filt_timerexpire, kn);

 Exit(0);
end;

procedure filt_timerdetach(kn:p_knote);
begin
 //struct callout *calloutp;

 //calloutp:=(struct callout *)kn^.kn_hook;
 //callout_drain(calloutp);
 //free(calloutp, M_KQUEUE);

 System.InterlockedDecrement(kq_ncallouts);
 kn^.kn_status:=kn^.kn_status or KN_DETACHED; { knlist_remove usually clears it }
end;

function filt_timer(kn:p_knote;hint:QWORD):Integer;
begin
 Exit(ord(kn^.kn_data<>0));
end;

function filt_userattach(kn:p_knote):Integer;
begin
 {
  * EVFILT_USER knotes are not attached to anything in the kernel.
  }
 kn^.kn_hook:=nil;

 if ((kn^.kn_fflags and NOTE_TRIGGER)<>0) then
  kn^.kn_hookid:=1
 else
  kn^.kn_hookid:=0;

 Exit(0);
end;

procedure filt_userdetach(kn:p_knote);
begin
 {
  * EVFILT_USER knotes are not attached to anything in the kernel.
  }
end;

function filt_user(kn:p_knote;hint:QWORD):Integer;
begin
 Exit(kn^.kn_hookid);
end;

procedure filt_usertouch(kn:p_knote;kev:p_kevent;_type:QWORD);
var
 ffctrl:DWORD;
begin
 case (_type) of
  EVENT_REGISTER:
   begin
    if ((kev^.fflags and NOTE_TRIGGER)<>0) then
    begin
     kn^.kn_hookid:=1;
    end;

    ffctrl:=kev^.fflags and NOTE_FFCTRLMASK;
    kev^.fflags:=kev^.fflags and NOTE_FFLAGSMASK;

    case (ffctrl) of
     NOTE_FFNOP :;
     NOTE_FFAND :kn^.kn_sfflags:=kn^.kn_sfflags and kev^.fflags;
     NOTE_FFOR  :kn^.kn_sfflags:=kn^.kn_sfflags or kev^.fflags;
     NOTE_FFCOPY:kn^.kn_sfflags:=kev^.fflags;
     else;
      { XXX Exit error? }
    end;

    kn^.kn_sdata:=kev^.data;
    if ((kev^.flags and EV_CLEAR)<>0) then
    begin
     kn^.kn_hookid:=0;
     kn^.kn_data  :=0;
     kn^.kn_fflags:=0;
    end;
   end;

  EVENT_PROCESS:
   begin
    kev^:=kn^.kn_kevent;
    kev^.fflags:=kn^.kn_sfflags;
    kev^.data:=kn^.kn_sdata;
    if ((kn^.kn_flags and EV_CLEAR)<>0) then
    begin
     kn^.kn_hookid:=0;
     kn^.kn_data  :=0;
     kn^.kn_fflags:=0;
    end;
   end;

  else
   Assert(false,'filt_usertouch() - invalid type ('+IntToStr(_type)+')');
 end;
end;

function sys_kqueue():Integer;
label
 done2;
var
 td:p_kthread;
 kq:p_kqueue;
 fp:p_file;
 fd,error:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 //fdp:=td^.td_proc^.p_fd;
 error:=falloc(@fp, @fd, 0);
 if (error<>0) then goto done2;

 { An extra reference on `nfp' has been held for us by falloc(). }
 kq:=AllocMem(SizeOf(t_kqueue));

 mtx_init(kq^.kq_lock, 'kqueue');
 TAILQ_INIT(@kq^.kq_head);
 //kq^.kq_fdp:=fdp;
 knlist_init_mtx(@kq^.kq_sel.si_note, @kq^.kq_lock);
 //TASK_INIT(@kq^.kq_task, 0, kqueue_task, kq);

 FILEDESC_XLOCK(@fd_table);
 TAILQ_INSERT_HEAD(@fd_table.fd_kqlist, kq, @kq^.kq_list);
 FILEDESC_XUNLOCK(@fd_table);

 finit(fp, FREAD or FWRITE, DTYPE_KQUEUE, kq, @kqueueops);
 fdrop(fp);

 td^.td_retval[0]:=fd;
done2:
 Exit(error);
end;

type
 p_kevent_args=^t_kevent_args;
 t_kevent_args=record
  fd        :Integer;
  changelist:p_kevent;
  nchanges  :Integer;
  eventlist :p_kevent;
  nevents   :Integer;
  timeout   :ptimespec;
 end;

{
 * Copy 'count' items into the destination list pointed to by uap^.eventlist.
 }
function kevent_copyout(arg:Pointer;kevp:p_kevent;count:Integer):Integer;
var
 uap:p_kevent_args;
 error:Integer;
begin
 Assert(count <= KQ_NEVENTS, Format('count (%d) > KQ_NEVENTS', [count]));
 uap:=arg;

 error:=copyout(kevp, uap^.eventlist, count*sizeof(t_kevent));
 if (error=0) then
 begin
  Inc(uap^.eventlist,count);
 end;
 Exit(error);
end;

{
 * Copy 'count' items from the list pointed to by uap^.changelist.
 }
function kevent_copyin(arg:Pointer;kevp:p_kevent;count:Integer):Integer;
var
 uap:p_kevent_args;
 error:Integer;
begin
 Assert(count <= KQ_NEVENTS, Format('count (%d) > KQ_NEVENTS', [count]));
 uap:=arg;

 error:=copyin(uap^.changelist, kevp, count*sizeof(t_kevent));
 if (error=0) then
 begin
  Inc(uap^.changelist,count);
 end;
 Exit(error);
end;

function kern_kevent(fd:Integer;
                     nchanges:Integer;
                     nevents:Integer;
                     k_ops:p_kevent_copyops;
                     timeout:ptimespec):Integer;
label
 done_norel,
 done;
var
 td:p_kthread;
 keva:array[0..KQ_NEVENTS-1] of t_kevent;
 kevp,changes:p_kevent;
 kq:p_kqueue;
 fp:p_file;
 i,n,nerrors,error:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 error:=fget(fd, CAP_POST_EVENT, @fp);
 if (error<>0) then Exit(error);

 error:=kqueue_acquire(fp, @kq);
 if (error<>0) then goto done_norel;

 nerrors:=0;

 while (nchanges > 0) do
 begin
  if (nchanges > KQ_NEVENTS) then
   n:=KQ_NEVENTS
  else
   n:=nchanges;

  error:=k_ops^.k_copyin(k_ops^.arg, keva, n);
  if (error<>0) then goto done;
  changes:=keva;
  For i:=0 to n-1 do
  begin
   kevp:=@changes[i];
   if (kevp^.filter=0) then continue;
   kevp^.flags:=kevp^.flags and (not EV_SYSFLAGS);
   error:=kqueue_register(kq, kevp);
   if (error<>0) or ((kevp^.flags and EV_RECEIPT)<>0) then
   begin
    if (nevents<>0) then
    begin
     kevp^.flags:=EV_ERROR;
     kevp^.data :=error;
     k_ops^.k_copyout(k_ops^.arg, kevp, 1);
     Dec(nevents);
     Inc(nerrors);
    end else
    begin
     goto done;
    end;
   end;
  end;
  Dec(nchanges,n);
 end;
 if (nerrors<>0) then
 begin
  td^.td_retval[0]:=nerrors;
  error:=0;
  goto done;
 end;

 error:=kqueue_scan(kq, nevents, k_ops, timeout, keva);
done:
 kqueue_release(kq, 0);
done_norel:
 fdrop(fp);
 Exit(error);
end;

function sys_kevent(fd:Integer;
                    changelist:Pointer;
                    nchanges:Integer;
                    eventlist:Pointer;
                    nevents:Integer;
                    timeout:Pointer):Integer;
var
 ts:timespec;
 tsp:Ptimespec;
 error:Integer;
 uap:t_kevent_args;
 k_ops:t_kevent_copyops;
begin
 if (timeout<>nil) then
 begin
  error:=copyin(timeout, @ts, sizeof(ts));
  if (error<>0) then Exit(error);
  tsp:=@ts;
 end else
 begin
  tsp:=nil;
 end;

 uap.fd        :=fd        ;
 uap.changelist:=changelist;
 uap.nchanges  :=nchanges  ;
 uap.eventlist :=eventlist ;
 uap.nevents   :=nevents   ;
 uap.timeout   :=timeout   ;

 k_ops.arg      :=@uap;
 k_ops.k_copyout:=@kevent_copyout;
 k_ops.k_copyin :=@kevent_copyin;

 error:=kern_kevent(fd, nchanges, nevents, @k_ops, tsp);

 Exit(error);
end;

function kqueue_add_filteropts(filt:Integer;filtops:p_filterops):Integer;
var
 error:Integer;
begin
 error:=0;
 if (filt > 0) or (filt + EVFILT_SYSCOUNT < 0) then
 begin
  Writeln('trying to add a filterop that is out of range: ',(not filt),' is beyond ',EVFILT_SYSCOUNT);
  Exit(EINVAL);
 end;
 mtx_lock(filterops_lock);
 if (sysfilt_ops[not filt].fop<>@null_filtops) and
    (sysfilt_ops[not filt].fop<>nil) then
 begin
  error:=EEXIST;
 end else
 begin
  sysfilt_ops[not filt].fop:=filtops;
  sysfilt_ops[not filt].ref:=0;
 end;
 mtx_unlock(filterops_lock);

 Exit(error);
end;

function kqueue_del_filteropts(filt:Integer):Integer;
var
 error:Integer;
begin
 error:=0;
 if (filt > 0) or (filt + EVFILT_SYSCOUNT < 0) then
  Exit(EINVAL);

 mtx_lock(filterops_lock);
 if (sysfilt_ops[not filt].fop=@null_filtops) or
    (sysfilt_ops[not filt].fop=nil) then
 begin
  error:=EINVAL;
 end else
 if (sysfilt_ops[not filt].ref<>0) then
 begin
  error:=EBUSY;
 end else
 begin
  sysfilt_ops[not filt].fop:=@null_filtops;
  sysfilt_ops[not filt].ref:=0;
 end;
 mtx_unlock(filterops_lock);

 Exit(error);
end;

function kqueue_fo_find(filt:Integer):p_filterops;
begin
 if (filt > 0) or (filt + EVFILT_SYSCOUNT < 0) then
  Exit(nil);

 mtx_lock(filterops_lock);
 Inc(sysfilt_ops[not filt].ref);
 if (sysfilt_ops[not filt].fop=nil) then
 begin
  sysfilt_ops[not filt].fop:=@null_filtops;
 end;
 mtx_unlock(filterops_lock);

 Exit(sysfilt_ops[not filt].fop);
end;

procedure kqueue_fo_release(filt:Integer);
begin
 if (filt > 0) or (filt + EVFILT_SYSCOUNT < 0) then
  Exit;

 mtx_lock(filterops_lock);
 Assert(sysfilt_ops[not filt].ref>0,'filter object refcount not valid on release');
 Dec(sysfilt_ops[not filt].ref);
 mtx_unlock(filterops_lock);
end;

{
 * A ref to kq (obtained via kqueue_acquire) must be held.  waitok will
 * influence if memory allocation should wait.  Make sure it is 0 if you
 * hold any mutexes.
 }
function kqueue_register(kq:p_kqueue;kev:p_kevent):Integer;
label
 findkn,
 done,
 done_ev_add;
var
 fops:p_filterops;
 fp:p_file;
 kn,tkn:p_knote;
 error,filt,event:Integer;
 haskqglobal,filedesc_unlock:Integer;
 list:p_klist;
begin
 fp:=nil;
 kn:=nil;
 error:=0;
 haskqglobal:=0;
 filedesc_unlock:=0;

 filt:=kev^.filter;
 fops:=kqueue_fo_find(filt);

 if (fops=nil) then Exit(EINVAL);

 tkn:=knote_alloc();  { prevent waiting with locks }

findkn:
 if (fops^.f_isfd<>0) then
 begin
  if (kev^.ident > High(Integer)) then
   error:=EBADF
  else
   error:=fget(kev^.ident, CAP_POLL_EVENT, @fp);

  if (error<>0) then goto done;

  if ((kev^.flags and EV_ADD)=EV_ADD) and (kqueue_expand(kq, fops, kev^.ident)<>0) then
  begin
   { try again }
   fdrop(fp);
   fp:=nil;
   error:=kqueue_expand(kq, fops, kev^.ident);
   if (error<>0) then goto done;
   goto findkn;
  end;

  if (fp^.f_type=DTYPE_KQUEUE) then
  begin
   {
    * if we add some inteligence about what we are doing,
    * we should be able to support events on ourselves.
    * We need to know when we are doing this to prevent
    * getting both the knlist lock and the kq lock since
    * they are the same thing.
    }
   if (fp^.f_data=kq) then
   begin
    error:=EINVAL;
    goto done;
   end;

   {
    * Pre-lock the filedesc before the global
    * lock mutex, see the comment in
    * kqueue_close().
    }
   FILEDESC_XLOCK(@fd_table);
   filedesc_unlock:=1;
   KQ_GLOBAL_LOCK(@kq_global, haskqglobal);
  end;

  KQ_LOCK(kq);
  if (kev^.ident < kq^.kq_knlistsize) then
  begin
   kn:=SLIST_FIRST(@kq^.kq_knlist[kev^.ident]);
   while (kn<>nil) do
   begin
    if (kev^.filter=kn^.kn_filter) then break;
    kn:=SLIST_NEXT(kn,@kn^.kn_link);
   end;
  end;
 end else
 begin
  if ((kev^.flags and EV_ADD)=EV_ADD) then
  begin
   kqueue_expand(kq, fops, kev^.ident);
  end;

  KQ_LOCK(kq);
  if (kq^.kq_knhashmask<>0) then
  begin
   list:=@kq^.kq_knhash[KN_HASH(kev^.ident, kq^.kq_knhashmask)];
   kn:=SLIST_FIRST(list);
   while (kn<>nil) do
   begin
    if (kev^.ident=kn^.kn_id) and
       (kev^.filter=kn^.kn_filter) then
     break;
    kn:=SLIST_NEXT(kn,@kn^.kn_link);
   end;
  end;
 end;

 { knote is in the process of changing, wait for it to stablize. }
 if (kn<>nil) and ((kn^.kn_status and KN_INFLUX)=KN_INFLUX) then
 begin
  KQ_GLOBAL_UNLOCK(@kq_global, haskqglobal);
  if (filedesc_unlock<>0) then
  begin
   FILEDESC_XUNLOCK(@fd_table);
   filedesc_unlock:=0;
  end;
  kq^.kq_state:=kq^.kq_state or KQ_FLUXWAIT;
  msleep(kq, @kq^.kq_lock, PSOCK or PDROP, 'kqflxwt', 0);
  if (fp<>nil) then
  begin
   fdrop(fp);
   fp:=nil;
  end;
  goto findkn;
 end;

 {
  * kn now contains the matching knote, or nil if no match
  }
 if (kn=nil) then
 begin
  if ((kev^.flags and EV_ADD)<>0) then
  begin
   kn:=tkn;
   tkn:=nil;
   if (kn=nil) then
   begin
    KQ_UNLOCK(kq);
    error:=ENOMEM;
    goto done;
   end;
   kn^.kn_fp:=fp;
   kn^.kn_kq:=kq;
   kn^.kn_fop:=fops;
   {
    * apply reference counts to knote structure, and
    * do not release it at the end of this routine.
    }
   fops:=nil;
   fp:=nil;

   kn^.kn_sfflags:=kev^.fflags;
   kn^.kn_sdata:=kev^.data;
   kev^.fflags:=0;
   kev^.data:=0;
   kn^.kn_kevent:=kev^;
   kn^.kn_kevent.flags:=kn^.kn_kevent.flags and (not (EV_ADD or EV_DELETE or EV_ENABLE or EV_DISABLE));
   kn^.kn_status:=KN_INFLUX or KN_DETACHED;

   error:=knote_attach(kn, kq);
   KQ_UNLOCK(kq);
   if (error<>0) then
   begin
    tkn:=kn;
    goto done;
   end;

   error:=kn^.kn_fop^.f_attach(kn);
   if (error<>0) then
   begin
    knote_drop(kn);
    goto done;
   end;
   KN_LIST_LOCK(kn);
   goto done_ev_add;
  end else
  begin
   { No matching knote and the EV_ADD flag is not set. }
   KQ_UNLOCK(kq);
   error:=ENOENT;
   goto done;
  end;
 end;

 if ((kev^.flags and EV_DELETE)<>0) then
 begin
  kn^.kn_status:=kn^.kn_status or KN_INFLUX;
  KQ_UNLOCK(kq);
  if ((kn^.kn_status and KN_DETACHED)=0) then
  begin
   kn^.kn_fop^.f_detach(kn);
  end;
  knote_drop(kn);
  goto done;
 end;

 {
  * The user may change some filter values after the initial EV_ADD,
  * but doing so will not reset any filter which has already been
  * triggered.
  }
 kn^.kn_status:=kn^.kn_status or KN_INFLUX or KN_SCAN;
 KQ_UNLOCK(kq);
 KN_LIST_LOCK(kn);
 kn^.kn_kevent.udata:=kev^.udata;
 if (fops^.f_isfd=0) and (fops^.f_touch<>nil) then
 begin
  fops^.f_touch(kn, kev, EVENT_REGISTER);
 end else
 begin
  kn^.kn_sfflags:=kev^.fflags;
  kn^.kn_sdata  :=kev^.data;
 end;

 {
  * We can get here with kn^.kn_knlist=nil.  This can happen when
  * the initial attach event decides that the event is 'completed'
  * already.  i.e. filt_procattach is called on a zombie process.  It
  * will call filt_proc which will remove it from the list, and nil
  * kn_knlist.
  }
done_ev_add:
 event:=kn^.kn_fop^.f_event(kn, 0);
 KQ_LOCK(kq);
 if (event<>0) then
 begin
  KNOTE_ACTIVATE(kn, 1);
 end;
 kn^.kn_status:=kn^.kn_status and (not (KN_INFLUX or KN_SCAN));
 KN_LIST_UNLOCK(kn);

 if ((kev^.flags and EV_DISABLE)<>0) and
    ((kn^.kn_status and KN_DISABLED)=0) then
 begin
  kn^.kn_status:=kn^.kn_status or KN_DISABLED;
 end;

 if ((kev^.flags and EV_ENABLE)<>0) and ((kn^.kn_status and KN_DISABLED)<>0) then
 begin
  kn^.kn_status:=kn^.kn_status and (not KN_DISABLED);
  if ((kn^.kn_status and KN_ACTIVE)<>0) and
     ((kn^.kn_status and KN_QUEUED)=0) then
  begin
   knote_enqueue(kn);
  end;
 end;
 KQ_UNLOCK_FLUX(kq);

done:
 KQ_GLOBAL_UNLOCK(@kq_global, haskqglobal);
 if (filedesc_unlock<>0) then
 begin
  FILEDESC_XUNLOCK(@fd_table);
 end;
 if (fp<>nil) then
 begin
  fdrop(fp);
 end;
 if (tkn<>nil) then
 begin
  knote_free(tkn);
 end;
 if (fops<>nil) then
 begin
  kqueue_fo_release(filt);
 end;
 Exit(error);
end;

function kqueue_acquire(fp:p_file;kqp:pp_kqueue):Integer;
var
 error:Integer;
 kq:p_kqueue;
begin
 error:=0;

 kq:=fp^.f_data;
 if (fp^.f_type<>DTYPE_KQUEUE) or (kq=nil) then
 begin
  Exit(EBADF);
 end;
 kqp^:=kq;
 KQ_LOCK(kq);
 if ((kq^.kq_state and KQ_CLOSING)=KQ_CLOSING) then
 begin
  KQ_UNLOCK(kq);
  Exit(EBADF);
 end;
 Inc(kq^.kq_refcnt);
 KQ_UNLOCK(kq);

 Exit(error);
end;

procedure kqueue_release(kq:p_kqueue;locked:Integer);
begin
 if (locked<>0) then
  KQ_OWNED(kq)
 else
  KQ_LOCK(kq);

 Dec(kq^.kq_refcnt);
 if (kq^.kq_refcnt=1) then
 begin
  wakeup(@kq^.kq_refcnt);
 end;
 if (locked=0) then
 begin
  KQ_UNLOCK(kq);
 end;
end;

procedure kqueue_schedtask(kq:p_kqueue);
begin
 KQ_OWNED(kq);
 Assert((kq^.kq_state and KQ_TASKDRAIN)<>KQ_TASKDRAIN,'scheduling kqueue task while draining');

 if ((kq^.kq_state and KQ_TASKSCHED)<>KQ_TASKSCHED) then
 begin
  //taskqueue_enqueue(taskqueue_kqueue, @kq^.kq_task);
  kq^.kq_state:=kq^.kq_state or KQ_TASKSCHED;
 end;
end;

{
 * Expand the kq to make sure we have storage for fops/ident pair.
 *
 * Return 0 on success (or no work necessary), return errno on failure.
 *
 * Not calling hashinit w/ waitok (proper malloc flag) should be safe.
 * If kqueue_register is called from a non-fd context, there usually/should
 * be no locks held.
 }
function kqueue_expand(kq:p_kqueue;fops:p_filterops;ident:ptruint):Integer;
var
 list,tmp_knhash,to_free:p_klist;
 tmp_knhashmask:QWORD;
 size:Integer;
 fd:Integer;
begin
 KQ_NOTOWNED(kq);

 to_free:=nil;
 if (fops^.f_isfd<>0) then
 begin
  fd:=ident;
  if (kq^.kq_knlistsize <= fd) then
  begin
   size:=kq^.kq_knlistsize;
   while (size <= fd) do
   begin
    Inc(size, KQEXTENT);
   end;
   list:=AllocMem(size*SizeOf(t_klist));
   if (list=nil) then Exit(ENOMEM);
   KQ_LOCK(kq);
   if (kq^.kq_knlistsize > fd) then
   begin
    to_free:=list;
    list:=nil;
   end else
   begin
    if (kq^.kq_knlist<>nil) then
    begin
     Move(kq^.kq_knlist^,list^,kq^.kq_knlistsize*SizeOf(t_klist));
     to_free:=kq^.kq_knlist;
     kq^.kq_knlist:=nil;
    end;
    FillChar((Pointer(list) + kq^.kq_knlistsize*SizeOf(t_klist))^, (size - kq^.kq_knlistsize)*SizeOf(t_klist),0);
    kq^.kq_knlistsize:=size;
    kq^.kq_knlist    :=list;
   end;
   KQ_UNLOCK(kq);
  end;
 end else
 begin
  if (kq^.kq_knhashmask=0) then
  begin
   tmp_knhash:=hashinit(KN_HASHSIZE, @tmp_knhashmask);
   if (tmp_knhash=nil) then
   begin
    Exit(ENOMEM);
   end;
   KQ_LOCK(kq);
   if (kq^.kq_knhashmask=0) then
   begin
    kq^.kq_knhash    :=tmp_knhash;
    kq^.kq_knhashmask:=tmp_knhashmask;
   end else
   begin
    to_free:=tmp_knhash;
   end;
   KQ_UNLOCK(kq);
  end;
 end;
 FreeMem(to_free);

 KQ_NOTOWNED(kq);
 Exit(0);
end;

procedure kqueue_task(arg:Pointer;pending:Integer);
var
 kq:p_kqueue;
 haskqglobal:Integer;
begin
 haskqglobal:=0;
 kq:=arg;

 KQ_GLOBAL_LOCK(@kq_global, haskqglobal);
 KQ_LOCK(kq);

 KNOTE_LOCKED(@kq^.kq_sel.si_note, 0);

 kq^.kq_state:=kq^.kq_state and (not KQ_TASKSCHED);
 if ((kq^.kq_state and KQ_TASKDRAIN)=KQ_TASKDRAIN) then
 begin
  wakeup(@kq^.kq_state);
 end;
 KQ_UNLOCK(kq);
 KQ_GLOBAL_UNLOCK(@kq_global, haskqglobal);
end;

{
 * Scan, update kn_data (if not ONESHOT), and copyout triggered events.
 * We treat KN_MARKER knotes as if they are INFLUX.
 }
function kqueue_scan(kq:p_kqueue;
                     maxevents:Integer;
                     k_ops:p_kevent_copyops;
                     tsp:ptimespec;
                     keva:p_kevent):Integer;
label
 done,
 done_nl,
 retry,
 start;
var
 td:p_kthread;
 kevp:p_kevent;
 atv,rtv,ttv:Int64;
 timeout:Int64;
 kn,marker:p_knote;
 count,nkev,error,influx:Integer;
 haskqglobal,touch:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 count:=maxevents;
 nkev:=0;
 error:=0;
 haskqglobal:=0;

 if (maxevents=0) then goto done_nl;

 if (tsp<>nil) then
 begin
  atv:=TIMESPEC_TO_UNIT(tsp);

  if (atv=0) then
   timeout:=-1
  else
  if (atv>24*60*60*hz) then
   timeout:=24*60*60*hz
  else
   timeout:=tvtohz(atv);

  rtv:=get_unit_uptime;
  atv:=atv+rtv;
 end else
 begin
  atv:=0;
  timeout:=0;
 end;
 marker:=knote_alloc();
 if (marker=nil) then
 begin
  error:=ENOMEM;
  goto done_nl;
 end;
 marker^.kn_status:=KN_MARKER;
 KQ_LOCK(kq);
 goto start;

retry:
 if (atv<>0) then
 begin
  rtv:=get_unit_uptime;
  if (rtv>=atv) then goto done;
  ttv:=atv-rtv;

  if (ttv>24*60*60*hz) then
   timeout:=24*60*60*hz
  else
   timeout:=tvtohz(ttv);
 end;

start:
 kevp:=keva;
 if (kq^.kq_count=0) then
 begin
  if (timeout < 0) then
  begin
   error:=EWOULDBLOCK;
  end else
  begin
   kq^.kq_state:=kq^.kq_state or KQ_SLEEP;
   error:=msleep(kq, @kq^.kq_lock, PSOCK or PCATCH, 'kqread', timeout);
  end;
  if (error=0) then goto retry;
  { don't restart after signals... }
  if (error=ERESTART) then
  begin
   error:=EINTR;
  end else
  if (error=EWOULDBLOCK) then
  begin
   error:=0;
  end;
  goto done;
 end;

 TAILQ_INSERT_TAIL(@kq^.kq_head,marker,@marker^.kn_tqe);
 influx:=0;
 while (count<>0) do
 begin
  KQ_OWNED(kq);
  kn:=TAILQ_FIRST(@kq^.kq_head);

  if ((kn^.kn_status=KN_MARKER) and (kn<>marker)) or
     ((kn^.kn_status and KN_INFLUX)=KN_INFLUX) then
  begin
   if (influx<>0) then
   begin
    influx:=0;
    KQ_FLUX_WAKEUP(kq);
   end;
   kq^.kq_state:=kq^.kq_state or KQ_FLUXWAIT;
   error:=msleep(kq, @kq^.kq_lock, PSOCK, 'kqflxwt', 0);
   continue;
  end;

  TAILQ_REMOVE(@kq^.kq_head,kn,@kn^.kn_tqe);
  if ((kn^.kn_status and KN_DISABLED)=KN_DISABLED) then
  begin
   kn^.kn_status:=kn^.kn_status and (not KN_QUEUED);
   Dec(kq^.kq_count);
   continue;
  end;
  if (kn=marker) then
  begin
   KQ_FLUX_WAKEUP(kq);
   if (count=maxevents) then goto retry;
   goto done;
  end;
  Assert((kn^.kn_status and KN_INFLUX)=0,'KN_INFLUX set when not suppose to be');

  if ((kn^.kn_flags and EV_DROP)=EV_DROP) then
  begin
   kn^.kn_status:=kn^.kn_status and (not KN_QUEUED);
   kn^.kn_status:=kn^.kn_status or KN_INFLUX;
   Dec(kq^.kq_count);
   KQ_UNLOCK(kq);
   {
    * We don't need to lock the list since we've marked
    * it _INFLUX.
    }
   if ((kn^.kn_status and KN_DETACHED)=0) then
   begin
    kn^.kn_fop^.f_detach(kn);
   end;
   knote_drop(kn);
   KQ_LOCK(kq);
   continue;
  end else
  if ((kn^.kn_flags and EV_ONESHOT)=EV_ONESHOT) then
  begin
   kn^.kn_status:=kn^.kn_status and (not KN_QUEUED);
   kn^.kn_status:=kn^.kn_status or KN_INFLUX;
   Dec(kq^.kq_count);
   KQ_UNLOCK(kq);
   {
    * We don't need to lock the list since we've marked
    * it _INFLUX.
    }
   kevp^:=kn^.kn_kevent;
   if ((kn^.kn_status and KN_DETACHED)=0) then
   begin
    kn^.kn_fop^.f_detach(kn);
   end;
   knote_drop(kn);
   KQ_LOCK(kq);
   kn:=nil;
  end else
  begin
   kn^.kn_status:=kn^.kn_status or KN_INFLUX or KN_SCAN;
   KQ_UNLOCK(kq);
   if ((kn^.kn_status and KN_KQUEUE)=KN_KQUEUE) then
   begin
    KQ_GLOBAL_LOCK(@kq_global, haskqglobal);
   end;
   KN_LIST_LOCK(kn);
   if (kn^.kn_fop^.f_event(kn, 0)=0) then
   begin
    KQ_LOCK(kq);
    KQ_GLOBAL_UNLOCK(@kq_global, haskqglobal);
    kn^.kn_status:=kn^.kn_status and (not (KN_QUEUED or KN_ACTIVE or KN_INFLUX or KN_SCAN));
    Dec(kq^.kq_count);
    KN_LIST_UNLOCK(kn);
    influx:=1;
    continue;
   end;
   touch:=ord((kn^.kn_fop^.f_isfd=0) and (kn^.kn_fop^.f_touch<>nil));

   if (touch<>0) then
    kn^.kn_fop^.f_touch(kn, kevp, EVENT_PROCESS)
   else
    kevp^:=kn^.kn_kevent;

   KQ_LOCK(kq);

   KQ_GLOBAL_UNLOCK(@kq_global, haskqglobal);
   if ((kn^.kn_flags and (EV_CLEAR or EV_DISPATCH))<>0) then
   begin
    {
     * Manually clear knotes who weren't
     * 'touch'ed.
     }
    if (touch=0) and ((kn^.kn_flags and EV_CLEAR)<>0) then
    begin
     kn^.kn_data  :=0;
     kn^.kn_fflags:=0;
    end;
    if ((kn^.kn_flags and EV_DISPATCH)<>0) then
    begin
     kn^.kn_status:=kn^.kn_status or KN_DISABLED;
    end;
    kn^.kn_status:=kn^.kn_status and (not (KN_QUEUED or KN_ACTIVE));
    Dec(kq^.kq_count);
   end else
   begin
    TAILQ_INSERT_TAIL(@kq^.kq_head,kn,@kn^.kn_tqe);
   end;

   kn^.kn_status:=kn^.kn_status and (not (KN_INFLUX or KN_SCAN));
   KN_LIST_UNLOCK(kn);
   influx:=1;
  end;

  { we are returning a copy to the user }
  Inc(kevp);
  Inc(nkev);
  Dec(count);

  if (nkev=KQ_NEVENTS) then
  begin
   influx:=0;
   KQ_UNLOCK_FLUX(kq);
   error:=k_ops^.k_copyout(k_ops^.arg, keva, nkev);
   nkev:=0;
   kevp:=keva;
   KQ_LOCK(kq);
   if (error<>0) then break;
  end;
 end;
 TAILQ_REMOVE(@kq^.kq_head,marker,@marker^.kn_tqe);
done:
 KQ_OWNED(kq);
 KQ_UNLOCK_FLUX(kq);
 knote_free(marker);
done_nl:
 KQ_NOTOWNED(kq);
 if (nkev<>0) then
 begin
  error:=k_ops^.k_copyout(k_ops^.arg, keva, nkev);
 end;
 td^.td_retval[0]:=maxevents - count;
 Exit(error);
end;

{
 * XXX
 * This could be expanded to call kqueue_scan, if desired.
 }
{ARGSUSED}
function kqueue_read(fp:p_file;uio:p_uio;flags:Integer):Integer;
begin
 Exit(ENXIO);
end;

{ARGSUSED}
function kqueue_write(fp:p_file;uio:p_uio;flags:Integer):Integer;
begin
 Exit(ENXIO);
end;

{ARGSUSED}
function kqueue_truncate(fp:p_file;length:Int64):Integer;
begin
 Exit(EINVAL);
end;

{ARGSUSED}
function kqueue_ioctl(fp:p_file;cmd:QWORD;data:Pointer):Integer;
begin
 Exit(ENOTTY);
end;

{ARGSUSED}
function kqueue_poll(fp:p_file;events:Integer):Integer;
var
 kq:p_kqueue;
 revents:Integer;
 error:Integer;
begin
 revents:=0;

 error:=kqueue_acquire(fp, @kq);
 if (error<>0) then
 begin
  Exit(POLLERR);
 end;

 KQ_LOCK(kq);
 if ((events and (POLLIN or POLLRDNORM))<>0) then
 begin
  if (kq^.kq_count<>0) then
  begin
   revents:=revents or events and (POLLIN or POLLRDNORM);
  end else
  begin
   selrecord(curkthread, @kq^.kq_sel);
   if (SEL_WAITING(@kq^.kq_sel)) then
   begin
    kq^.kq_state:=kq^.kq_state or KQ_SEL;
   end;
  end;
 end;
 kqueue_release(kq, 1);
 KQ_UNLOCK(kq);
 Exit(revents);
end;

{ARGSUSED}
function kqueue_stat(fp:p_file;st:p_stat):Integer;
begin
 st^:=Default(t_stat);
 {
  * We no longer Exit kq_count because the unlocked value is useless.
  * If you spent all this time getting the count, why not spend your
  * syscall better by calling kevent?
  *
  * XXX - This is needed for libc_r.
  }
 st^.st_mode:=S_IFIFO;
 Exit(0);
end;

{ARGSUSED}
function kqueue_close(fp:p_file):Integer;
var
 kq:p_kqueue;
 kn:p_knote;
 i:Integer;
 error:Integer;
 filedesc_unlock:Integer;
begin
 kq:=fp^.f_data;

 error:=kqueue_acquire(fp, @kq);
 if (error<>0) then Exit(error);

 filedesc_unlock:=0;
 KQ_LOCK(kq);

 Assert((kq^.kq_state and KQ_CLOSING)<>KQ_CLOSING,'kqueue already closing');

 kq^.kq_state:=kq^.kq_state or KQ_CLOSING;

 if (kq^.kq_refcnt > 1) then
 begin
  msleep(@kq^.kq_refcnt, @kq^.kq_lock, PSOCK, 'kqclose', 0);
 end;

 Assert(kq^.kq_refcnt=1,'other refs are out there!');

 Assert(knlist_empty(@kq^.kq_sel.si_note)<>0,'kqueue knlist not empty');

 For i:=0 to kq^.kq_knlistsize-1 do
 begin
  kn:=SLIST_FIRST(@kq^.kq_knlist[i]);
  while (kn<>nil) do
  begin
   if ((kn^.kn_status and KN_INFLUX)=KN_INFLUX) then
   begin
    kq^.kq_state:=kq^.kq_state or KQ_FLUXWAIT;
    msleep(kq, @kq^.kq_lock, PSOCK, 'kqclo1', 0);
    //
    kn:=SLIST_FIRST(@kq^.kq_knlist[i]);
    continue;
   end;
   kn^.kn_status:=kn^.kn_status or KN_INFLUX;
   KQ_UNLOCK(kq);
   if ((kn^.kn_status and KN_DETACHED)=0) then
   begin
    kn^.kn_fop^.f_detach(kn);
   end;
   knote_drop(kn);
   KQ_LOCK(kq);
   //
   kn:=SLIST_FIRST(@kq^.kq_knlist[i]);
  end;
 end;

 if (kq^.kq_knhashmask<>0) then
 begin
  For i:=0 to kq^.kq_knhashmask do
  begin
   kn:=SLIST_FIRST(@kq^.kq_knhash[i]);
   while (kn<>nil) do
   begin
    if ((kn^.kn_status and KN_INFLUX)=KN_INFLUX) then
    begin
     kq^.kq_state:=kq^.kq_state or KQ_FLUXWAIT;
     msleep(kq, @kq^.kq_lock, PSOCK, 'kqclo2', 0);
     //
     kn:=SLIST_FIRST(@kq^.kq_knhash[i]);
     continue;
    end;
    kn^.kn_status:=kn^.kn_status or KN_INFLUX;
    KQ_UNLOCK(kq);
    if ((kn^.kn_status and KN_DETACHED)=0) then
    begin
     kn^.kn_fop^.f_detach(kn);
    end;
    knote_drop(kn);
    KQ_LOCK(kq);
    //
    kn:=SLIST_FIRST(@kq^.kq_knhash[i]);
   end;
  end;
 end;

 if ((kq^.kq_state and KQ_TASKSCHED)=KQ_TASKSCHED) then
 begin
  kq^.kq_state:=kq^.kq_state or KQ_TASKDRAIN;
  msleep(@kq^.kq_state, @kq^.kq_lock, PSOCK, 'kqtqdr', 0);
 end;

 if ((kq^.kq_state and KQ_SEL)=KQ_SEL) then
 begin
  selwakeuppri(@kq^.kq_sel, PSOCK);
  if (not SEL_WAITING(@kq^.kq_sel)) then
  begin
   kq^.kq_state:=kq^.kq_state and (not KQ_SEL);
  end
 end;

 KQ_UNLOCK(kq);

 {
  * We could be called due to the knote_drop() doing fdrop(),
  * called from kqueue_register().  In this case the global
  * lock is owned, and filedesc sx is locked before, to not
  * take the sleepable lock after non-sleepable.
  }
 if (not sx_xlocked(FILEDESC_LOCK(@fd_table))) then
 begin
  FILEDESC_XLOCK(@fd_table);
  filedesc_unlock:=1;
 end else
 begin
  filedesc_unlock:=0;
 end;

 TAILQ_REMOVE(@fd_table.fd_kqlist,kq,@kq^.kq_list);

 if (filedesc_unlock<>0) then
 begin
  FILEDESC_XUNLOCK(@fd_table);
 end;

 seldrain(@kq^.kq_sel);
 knlist_destroy(@kq^.kq_sel.si_note);
 mtx_destroy(kq^.kq_lock);
 //kq^.kq_fdp:=nil;

 if (kq^.kq_knhash<>nil) then
 begin
  FreeMem(kq^.kq_knhash);
 end;
 if (kq^.kq_knlist<>nil) then
 begin
  FreeMem(kq^.kq_knlist);
 end;

 //funsetown(@kq^.kq_sigio);
 FreeMem(kq);
 fp^.f_data:=nil;

 Exit(0);
end;

procedure kqueue_wakeup(kq:p_kqueue);
begin
 KQ_OWNED(kq);

 if ((kq^.kq_state and KQ_SLEEP)=KQ_SLEEP) then
 begin
  kq^.kq_state:=kq^.kq_state and (not KQ_SLEEP);
  wakeup(kq);
 end;
 if ((kq^.kq_state and KQ_SEL)=KQ_SEL) then
 begin
  selwakeuppri(@kq^.kq_sel, PSOCK);
  if (not SEL_WAITING(@kq^.kq_sel)) then
  begin
   kq^.kq_state:=kq^.kq_state and (not KQ_SEL);
  end;
 end;
 if (knlist_empty(@kq^.kq_sel.si_note)=0) then
 begin
  kqueue_schedtask(kq);
 end;
 if ((kq^.kq_state and KQ_ASYNC)=KQ_ASYNC) then
 begin
  //pgsigio(@kq^.kq_sigio, SIGIO, 0);
 end;
end;

{
 * Walk down a list of knotes, activating them if their event has triggered.
 *
 * There is a possibility to optimize in the case of one kq watching another.
 * Instead of scheduling a task to wake it up, you could pass enough state
 * down the chain to make up the parent kqueue.  Make this code functional
 * first.
 }
procedure knote(list:p_knlist;hint:QWORD;lockflags:Integer);
var
 kq:p_kqueue;
 kn:p_knote;
 error:Integer;
begin
 if (list=nil) then Exit;

 KNL_ASSERT_LOCK(list, lockflags and KNF_LISTLOCKED);

 if ((lockflags and KNF_LISTLOCKED)=0) then
 begin
  list^.kl_lock(list^.kl_lockarg);
 end;

 {
  * If we unlock the list lock (and set KN_INFLUX), we can eliminate
  * the kqueue scheduling, but this will introduce four
  * lock/unlock's for each knote to test.  If we do, continue to use
  * SLIST_FOREACH, SLIST_FOREACH_SAFE is not safe in our case, it is
  * only safe if you want to remove the current item, which we are
  * not doing.
  }
 kn:=SLIST_FIRST(@list^.kl_list);
 while (kn<>nil) do
 begin
  kq:=kn^.kn_kq;
  KQ_LOCK(kq);
  if ((kn^.kn_status and (KN_INFLUX or KN_SCAN))=KN_INFLUX) then
  begin
   {
    * Do not process the influx notes, except for
    * the influx coming from the kq unlock in the
    * kqueue_scan().  In the later case, we do
    * not interfere with the scan, since the code
    * fragment in kqueue_scan() locks the knlist,
    * and cannot proceed until we finished.
    }
   KQ_UNLOCK(kq);
  end else
  if ((lockflags and KNF_NOKQLOCK)<>0) then
  begin
   kn^.kn_status:=kn^.kn_status or KN_INFLUX;
   KQ_UNLOCK(kq);
   error:=kn^.kn_fop^.f_event(kn, hint);
   KQ_LOCK(kq);
   kn^.kn_status:=kn^.kn_status and (not KN_INFLUX);
   if (error<>0) then
   begin
    KNOTE_ACTIVATE(kn, 1);
   end;
   KQ_UNLOCK_FLUX(kq);
  end else
  begin
   kn^.kn_status:=kn^.kn_status or KN_HASKQLOCK;
   if (kn^.kn_fop^.f_event(kn, hint)<>0) then
   begin
    KNOTE_ACTIVATE(kn, 1);
   end;
   kn^.kn_status:=kn^.kn_status and (not KN_HASKQLOCK);
   KQ_UNLOCK(kq);
  end;
  //
  kn:=SLIST_NEXT(kn,@kn^.kn_selnext);
 end;
 if ((lockflags and KNF_LISTLOCKED)=0) then
 begin
  list^.kl_unlock(list^.kl_lockarg);
 end;
end;

{
 * add a knote to a knlist
 }
procedure knlist_add(knl:p_knlist;kn:p_knote;islocked:Integer);
begin
 KNL_ASSERT_LOCK(knl, islocked);
 KQ_NOTOWNED(kn^.kn_kq);
 Assert((kn^.kn_status and (KN_INFLUX or KN_DETACHED))=(KN_INFLUX or KN_DETACHED),'knote not KN_INFLUX and KN_DETACHED');
 if (islocked=0) then
 begin
  knl^.kl_lock(knl^.kl_lockarg);
 end;
 SLIST_INSERT_HEAD(@knl^.kl_list,kn,@kn^.kn_selnext);
 if (islocked=0) then
 begin
  knl^.kl_unlock(knl^.kl_lockarg);
 end;
 KQ_LOCK(kn^.kn_kq);
 kn^.kn_knlist:=knl;
 kn^.kn_status:=kn^.kn_status and (not KN_DETACHED);
 KQ_UNLOCK(kn^.kn_kq);
end;

procedure knlist_remove_kq(knl:p_knlist;kn:p_knote;knlislocked,kqislocked:Integer);
begin
 Assert(not ((kqislocked<>0) and (knlislocked=0)),'kq locked w/o knl locked');
 KNL_ASSERT_LOCK(knl, knlislocked);

 if (kqislocked<>0) then
 begin
  mtx_assert(p_kqueue(kn^.kn_kq)^.kq_lock);
 end;

 if (kqislocked=0) then
 begin
  Assert((kn^.kn_status and (KN_INFLUX or KN_DETACHED))=KN_INFLUX,'knlist_remove called w/o knote being KN_INFLUX or already removed');
 end;

 if (knlislocked=0) then
 begin
  knl^.kl_lock(knl^.kl_lockarg);
 end;

 SLIST_REMOVE(@knl^.kl_list,kn,@kn^.kn_selnext);
 kn^.kn_knlist:=nil;

 if (knlislocked=0) then
 begin
  knl^.kl_unlock(knl^.kl_lockarg);
 end;

 if (kqislocked=0) then
 begin
  KQ_LOCK(kn^.kn_kq);
 end;

 kn^.kn_status:=kn^.kn_status or KN_DETACHED;

 if (kqislocked=0) then
 begin
  KQ_UNLOCK(kn^.kn_kq);
 end;
end;

{
 * remove all knotes from a specified klist
 }
procedure knlist_remove(knl:p_knlist;kn:p_knote;islocked:Integer);
begin
 knlist_remove_kq(knl, kn, islocked, 0);
end;

{
 * remove knote from a specified klist while in f_event handler.
 }
procedure knlist_remove_inevent(knl:p_knlist;kn:p_knote);
begin
 knlist_remove_kq(knl, kn, 1, ord((kn^.kn_status and KN_HASKQLOCK)=KN_HASKQLOCK));
end;

function knlist_empty(knl:p_knlist):Integer;
begin
 KNL_ASSERT_LOCKED(knl);
 Exit(ord(SLIST_EMPTY(@knl^.kl_list)));
end;

procedure knlist_mtx_lock(arg:Pointer);
begin
 mtx_lock(p_mtx(arg)^);
end;

procedure knlist_mtx_unlock(arg:Pointer);
begin
 mtx_unlock(p_mtx(arg)^);
end;

procedure knlist_mtx_assert_locked(arg:Pointer);
begin
 mtx_assert(p_mtx(arg)^);
end;

procedure knlist_mtx_assert_unlocked(arg:Pointer);
begin
 //
end;

procedure knlist_init(knl:p_knlist;lock,kl_lock,kl_unlock,kl_assert_locked,kl_assert_unlocked:Pointer);
begin
 if (lock=nil) then
  knl^.kl_lockarg:=@knlist_lock
 else
  knl^.kl_lockarg:=lock;

 if (kl_lock=nil) then
  knl^.kl_lock:=@knlist_mtx_lock
 else
  Pointer(knl^.kl_lock):=kl_lock;

 if (kl_unlock=nil) then
  knl^.kl_unlock:=@knlist_mtx_unlock
 else
  Pointer(knl^.kl_unlock):=kl_unlock;

 if (kl_assert_locked=nil) then
  knl^.kl_assert_locked:=@knlist_mtx_assert_locked
 else
  Pointer(knl^.kl_assert_locked):=kl_assert_locked;

 if (kl_assert_unlocked=nil) then
  knl^.kl_assert_unlocked:=@knlist_mtx_assert_unlocked
 else
  Pointer(knl^.kl_assert_unlocked):=kl_assert_unlocked;

 SLIST_INIT(@knl^.kl_list);
end;

procedure knlist_init_mtx(knl:p_knlist;lock:p_mtx);
begin
 knlist_init(knl, lock, nil, nil, nil, nil);
end;

procedure knlist_destroy(knl:p_knlist);
begin
 {
  * if we run across this error, we need to find the offending
  * driver and have it call knlist_clear.
  }
 if (not SLIST_EMPTY(@knl^.kl_list)) then
 begin
  Writeln('WARNING: destroying knlist w/ knotes on it!');
 end;

 knl^.kl_lockarg:=nil;
 knl^.kl_lock   :=nil;
 knl^.kl_unlock :=nil;
 SLIST_INIT(@knl^.kl_list);
end;

{
 * Even if we are locked, we may need to drop the lock to allow any influx
 * knotes time to 'settle'.
 }
procedure knlist_cleardel(knl:p_knlist;islocked,killkn:Integer);
label
 again;
var
 kn,kn2:p_knote;
 kq:p_kqueue;
begin
 if (islocked<>0) then
 begin
  KNL_ASSERT_LOCKED(knl);
 end else
 begin
  KNL_ASSERT_UNLOCKED(knl);
  again:  { need to reacquire lock since we have dropped it }
  knl^.kl_lock(knl^.kl_lockarg);
 end;

 kn:=SLIST_FIRST(@knl^.kl_list);
 while (kn<>nil) do
 begin
  kn2:=SLIST_NEXT(kn,@kn^.kn_selnext);
  //
  kq:=kn^.kn_kq;
  KQ_LOCK(kq);
  if ((kn^.kn_status and KN_INFLUX)<>0) then
  begin
   KQ_UNLOCK(kq);
   //
   kn:=kn2;
   continue;
  end;
  knlist_remove_kq(knl, kn, 1, 1);
  if (killkn<>0) then
  begin
   kn^.kn_status:=kn^.kn_status or KN_INFLUX or KN_DETACHED;
   KQ_UNLOCK(kq);
   knote_drop(kn);
  end else
  begin
   { Make sure cleared knotes disappear soon }
   kn^.kn_flags:=kn^.kn_flags or (EV_EOF or EV_ONESHOT);
   KQ_UNLOCK(kq);
  end;
  kq:=nil;
  //
  kn:=kn2;
 end;

 if (not SLIST_EMPTY(@knl^.kl_list)) then
 begin
  { there are still KN_INFLUX remaining }
  kn:=SLIST_FIRST(@knl^.kl_list);
  kq:=kn^.kn_kq;
  KQ_LOCK(kq);
  Assert((kn^.kn_status and KN_INFLUX)<>0,'knote removed w/o list lock');
  knl^.kl_unlock(knl^.kl_lockarg);
  kq^.kq_state:=kq^.kq_state or KQ_FLUXWAIT;
  msleep(kq, @kq^.kq_lock, PSOCK or PDROP, 'kqkclr', 0);
  kq:=nil;
  goto again;
 end;

 if (islocked<>0) then
 begin
  KNL_ASSERT_LOCKED(knl);
 end else
 begin
  knl^.kl_unlock(knl^.kl_lockarg);
  KNL_ASSERT_UNLOCKED(knl);
 end;
end;

procedure knlist_clear(knl:p_knlist;islocked:Integer);
begin
 knlist_cleardel(knl, islocked, 0)
end;

procedure knlist_delete(knl:p_knlist;islocked:Integer);
begin
 knlist_cleardel(knl, islocked, 1)
end;

{
 * Remove all knotes referencing a specified fd must be called with FILEDESC
 * lock.  This prevents a race where a new fd comes along and occupies the
 * entry and we attach a knote to the fd.
 }
procedure knote_fdclose(fd:Integer);
label
 again;
var
 kq:p_kqueue;
 kn:p_knote;
 influx:Integer;
begin
 //FILEDESC_XLOCK_ASSERT(@fd_table);

 {
  * We shouldn't have to worry about new kevents appearing on fd
  * since filedesc is locked.
  }
 kq:=TAILQ_FIRST(@fd_table.fd_kqlist);
 while (kq<>nil) do
 begin
  KQ_LOCK(kq);

  again:
  influx:=0;
  while (kq^.kq_knlistsize > fd) do
  begin
   kn:=SLIST_FIRST(@kq^.kq_knlist[fd]);
   if (kn=nil) then Break;

   if ((kn^.kn_status and KN_INFLUX)<>0) then
   begin
    { someone else might be waiting on our knote }
    if (influx<>0) then
    begin
     wakeup(kq);
    end;
    kq^.kq_state:=kq^.kq_state or KQ_FLUXWAIT;
    msleep(kq, @kq^.kq_lock, PSOCK, 'kqflxwt', 0);
    goto again;
   end;
   kn^.kn_status:=kn^.kn_status or KN_INFLUX;
   KQ_UNLOCK(kq);
   if ((kn^.kn_status and KN_DETACHED)=0) then
   begin
    kn^.kn_fop^.f_detach(kn);
   end;
   knote_drop(kn);
   influx:=1;
   KQ_LOCK(kq);
  end;
  KQ_UNLOCK_FLUX(kq);
  //
  kq:=TAILQ_NEXT(kq,@kq^.kq_list);
 end;
end;

function knote_attach(kn:p_knote;kq:p_kqueue):Integer;
var
 list:p_klist;
begin
 Assert((kn^.kn_status and KN_INFLUX)<>0,'knote not marked INFLUX');
 KQ_OWNED(kq);

 if (kn^.kn_fop^.f_isfd<>0) then
 begin
  if (kn^.kn_id >= kq^.kq_knlistsize) then
  begin
   Exit(ENOMEM);
  end;
  list:=@kq^.kq_knlist[kn^.kn_id];
 end else
 begin
  if (kq^.kq_knhash=nil) then
  begin
   Exit(ENOMEM);
  end;
  list:=@kq^.kq_knhash[KN_HASH(kn^.kn_id, kq^.kq_knhashmask)];
 end;

 SLIST_INSERT_HEAD(list,kn,@kn^.kn_link);

 Exit(0);
end;

{
 * knote must already have been detached using the f_detach method.
 * no lock need to be held, it is assumed that the KN_INFLUX flag is set
 * to prevent other removal.
 }
procedure knote_drop(kn:p_knote);
var
 kq:p_kqueue;
 list:p_klist;
begin
 kq:=kn^.kn_kq;

 KQ_NOTOWNED(kq);
 Assert((kn^.kn_status and KN_INFLUX)=KN_INFLUX,'knote_drop called without KN_INFLUX set in kn_status');

 KQ_LOCK(kq
 );
 if (kn^.kn_fop^.f_isfd<>0) then
  list:=@kq^.kq_knlist[kn^.kn_id]
 else
  list:=@kq^.kq_knhash[KN_HASH(kn^.kn_id, kq^.kq_knhashmask)];

 if (not SLIST_EMPTY(list)) then
 begin
  SLIST_REMOVE(list,kn,@kn^.kn_link);
 end;

 if ((kn^.kn_status and KN_QUEUED)<>0) then
 begin
  knote_dequeue(kn);
 end;

 KQ_UNLOCK_FLUX(kq);

 if (kn^.kn_fop^.f_isfd<>0) then
 begin
  fdrop(kn^.kn_fp);
  kn^.kn_fp:=nil;
 end;
 kqueue_fo_release(kn^.kn_kevent.filter);
 kn^.kn_fop:=nil;
 knote_free(kn);
end;

procedure knote_enqueue(kn:p_knote);
var
 kq:p_kqueue;
begin
 kq:=kn^.kn_kq;

 KQ_OWNED(kn^.kn_kq);
 Assert((kn^.kn_status and KN_QUEUED)=0,'knote already queued');

 TAILQ_INSERT_TAIL(@kq^.kq_head,kn,@kn^.kn_tqe);
 kn^.kn_status:=kn^.kn_status or KN_QUEUED;
 Inc(kq^.kq_count);
 kqueue_wakeup(kq);
end;

procedure knote_dequeue(kn:p_knote);
var
 kq:p_kqueue;
begin
 kq:=kn^.kn_kq;

 KQ_OWNED(kn^.kn_kq);
 Assert((kn^.kn_status and KN_QUEUED)<>0,'knote not queued');

 TAILQ_REMOVE(@kq^.kq_head,kn,@kn^.kn_tqe);
 kn^.kn_status:=kn^.kn_status and (not KN_QUEUED);
 Dec(kq^.kq_count);
end;

procedure knote_init(); //SYSINIT(knote, SI_SUB_PSEUDO, SI_ORDER_ANY, knote_init, nil);
begin
 mtx_init(kq_global     ,'kqueue order');
 mtx_init(filterops_lock,'protect sysfilt_ops');
 mtx_init(knlist_lock   ,'knlist lock for lockless objects');
 //
 knlist_init_mtx(@g_p_klist,@proc_mtx);
end;

function knote_alloc():p_knote;
begin
 Result:=AllocMem(SizeOf(t_knote));
end;

procedure knote_free(kn:p_knote);
begin
 if (kn<>nil) then
 begin
  FreeMem(kn);
 end;
end;

{
 * Register the kev w/ the kq specified by fd.
 }
function kqfd_register(fd:Integer;kev:p_kevent):Integer;
label
 noacquire;
var
 kq:p_kqueue;
 fp:p_file;
 error:Integer;
begin
 error:=fget(fd, CAP_POST_EVENT, @fp);
 if (error<>0) then Exit(error);

 error:=kqueue_acquire(fp, @kq);

 if (error<>0) then
 begin
  goto noacquire;
 end;

 error:=kqueue_register(kq, kev);

 kqueue_release(kq, 0);

noacquire:
 fdrop(fp);

 Exit(error);
end;


end.

