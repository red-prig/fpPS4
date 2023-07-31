unit sys_event;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue;

const
 EVFILT_READ                =(-1);
 EVFILT_WRITE               =(-2);
 EVFILT_AIO                 =(-3);   // attached to aio requests
 EVFILT_VNODE               =(-4);   // attached to vnodes
 EVFILT_PROC                =(-5);   // attached to struct proc
 EVFILT_SIGNAL              =(-6);   // attached to struct proc
 EVFILT_TIMER               =(-7);   // timers
 //EVFILT_NETDEV            =(-8);      no longer supported
 EVFILT_FS                  =(-9);   // filesystem events
 EVFILT_LIO                 =(-10);  // attached to lio requests
 EVFILT_USER                =(-11);  // User events
 EVFILT_POLLING             =(-12);
 EVFILT_DISPLAY             =(-13);
 EVFILT_GRAPHICS_CORE       =(-14);
 EVFILT_HRTIMER             =(-15);  // High resolution timers
 EVFILT_UVD_TRAP            =(-16);
 EVFILT_VCE_TRAP            =(-17);
 EVFILT_SDMA_TRAP           =(-18);
 EVFILT_REG_EV              =(-19);
 EVFILT_GPU_EXCEPTION       =(-20);
 EVFILT_GPU_SYSTEM_EXCEPTION=(-21);
 EVFILT_GPU_DBGGC_EV        =(-22);
 EVFILT_CPUMODE             =(-23);
 EVFILT_SYSCOUNT            =(23);

// actions
 EV_ADD     =$0001;  // add event to kq (implies enable)
 EV_DELETE  =$0002;  // delete event from kq
 EV_ENABLE  =$0004;  // enable event
 EV_DISABLE =$0008;  // disable event (not reported)

// flags
 EV_ONESHOT  =$0010;  // only report one occurrence
 EV_CLEAR    =$0020;  // clear event state after reporting
 EV_RECEIPT  =$0040;  // force EV_ERROR on success, data=0
 EV_DISPATCH =$0080;  // disable event after reporting

 EV_SYSFLAGS =$F000;  // reserved by system
 EV_DROP     =$1000;  // note should be dropped
 EV_FLAG1    =$2000;  // filter-specific flag

// returned values
 EV_EOF   =$8000;  // EOF detected
 EV_ERROR =$4000;  // error, data contains errno

//data/hint flags/masks for EVFILT_USER, shared with userspace
 NOTE_FFNOP     =$00000000; // ignore input fflags
 NOTE_FFAND     =$40000000; // AND fflags
 NOTE_FFOR      =$80000000; // OR fflags
 NOTE_FFCOPY    =$c0000000; // copy fflags
 NOTE_FFCTRLMASK=$c0000000; // masks for operations
 NOTE_FFLAGSMASK=$00ffffff;

 NOTE_TRIGGER   =$01000000; // Cause the event to be triggered for output.

//data/hint flags for EVFILT_{READ|WRITE}, shared with userspace
 NOTE_LOWAT  =$0001;   // low water mark

 NOTE_DELETE =$0001;   // vnode was removed
 NOTE_WRITE  =$0002;   // data contents changed
 NOTE_EXTEND =$0004;   // size increased
 NOTE_ATTRIB =$0008;   // attributes changed
 NOTE_LINK   =$0010;   // link count changed
 NOTE_RENAME =$0020;   // vnode was renamed
 NOTE_REVOKE =$0040;   // vnode access was revoked

//data/hint flags for EVFILT_PROC, shared with userspace
 NOTE_EXIT     =$80000000; // process exited
 NOTE_FORK     =$40000000; // process forked
 NOTE_EXEC     =$20000000; // process exec'd
 NOTE_PCTRLMASK=$f0000000; // mask for hint bits
 NOTE_PDATAMASK=$000fffff; // mask for pid

// additional flags for EVFILT_PROC
 NOTE_TRACK   =$00000001; // follow across forks
 NOTE_TRACKERR=$00000002; // could not track child
 NOTE_CHILD   =$00000004; // am a child process

// flags for EVFILT_POLLING
 NOTE_POLLING_EQUAL=$00000001; // trigger when var == data
 NOTE_POLLING_LESS =$00000002; // trigger when var < data
 NOTE_POLLING_NOT  =$00000004; // negate trigger condition
 NOTE_POLLING_COND =$00000007;
 NOTE_POLLING_64BIT=$00000008; // watch a 64-bit variable

//Flags for knote call
 KNF_LISTLOCKED=$0001; // knlist is locked
 KNF_NOKQLOCK  =$0002; // do not keep KQ_LOCK

 {
 * Flag indicating hint is a signal.  Used by EVFILT_SIGNAL, and also
 * shared by EVFILT_PROC  (all knotes attached to p->p_klist)
 }
 NOTE_SIGNAL=$08000000;

 //Hint values for the optional f_touch event filter
 EVENT_REGISTER=1;
 EVENT_PROCESS =2;

//kn_status
 KN_ACTIVE   =$001; // event has been triggered
 KN_QUEUED   =$002; // event is on queue
 KN_DISABLED =$004; // event is disabled
 KN_DETACHED =$008; // knote is detached
 KN_INFLUX   =$010; // knote is in flux
 KN_MARKER   =$020; // ignore this knote
 KN_KQUEUE   =$040; // this knote belongs to a kq
 KN_HASKQLOCK=$080; // for _inevent
 KN_SCAN     =$100; // flux set in kqueue_scan()

//sce
 SCE_KERNEL_EVFILT_TIMER     =EVFILT_TIMER        ;
 SCE_KERNEL_EVFILT_READ      =EVFILT_READ         ;
 SCE_KERNEL_EVFILT_WRITE     =EVFILT_WRITE        ;
 SCE_KERNEL_EVFILT_USER      =EVFILT_USER         ;
 SCE_KERNEL_EVFILT_FILE      =EVFILT_VNODE        ;
 SCE_KERNEL_EVFILT_GNM       =EVFILT_GRAPHICS_CORE;
 SCE_KERNEL_EVFILT_VIDEO_OUT =EVFILT_DISPLAY      ;
 SCE_KERNEL_EVFILT_HRTIMER   =EVFILT_HRTIMER      ;

 SCE_KERNEL_EVNOTE_DELETE    =NOTE_DELETE         ;
 SCE_KERNEL_EVNOTE_WRITE     =NOTE_WRITE          ;
 SCE_KERNEL_EVNOTE_EXTEND    =NOTE_EXTEND         ;
 SCE_KERNEL_EVNOTE_ATTRIB    =NOTE_ATTRIB         ;
 SCE_KERNEL_EVNOTE_RENAME    =NOTE_RENAME         ;
 SCE_KERNEL_EVNOTE_REVOKE    =NOTE_REVOKE         ;

 SCE_KERNEL_EVFLAG_EOF       =EV_EOF              ;
 SCE_KERNEL_EVFLAG_ERROR     =EV_ERROR            ;

 SCE_VIDEO_OUT_EVENT_FLIP            =0; //Flip completion event
 SCE_VIDEO_OUT_EVENT_VBLANK          =1; //Vblank event
 SCE_VIDEO_OUT_EVENT_PRE_VBLANK_START=2; //PreVblankStart event

type
 p_kevent=^t_kevent;
 t_kevent=packed record
  ident :PtrUint;  // identifier for this event
  filter:SmallInt; // filter for event
  flags :Word;     // action flags for kqueue
  fflags:DWORD;    // filter flag value
  data  :Ptrint;   // filter data value
  udata :Pointer;  // opaque user data identifier
 end;
 {$IF sizeof(t_kevent)<>32}{$STOP sizeof(t_kevent)<>32}{$ENDIF}

 pSceKernelEvent=^SceKernelEvent;
 SceKernelEvent=t_kevent;

 p_knote=^t_knote;

 p_klist=^t_klist;
 t_klist =SLIST_HEAD; //knote

 p_kqlist=^t_kqlist;
 t_kqlist=SLIST_HEAD; //kqueue

 p_knlist=^t_knlist;
 t_knlist=packed record
  kl_list           :t_klist;
  kl_lock           :procedure(arg:Pointer); // lock function
  kl_unlock         :procedure(arg:Pointer);
  kl_assert_locked  :procedure(arg:Pointer);
  kl_assert_unlocked:procedure(arg:Pointer);
  kl_lockarg        :Pointer;  // argument passed to kl_lockf()
 end;
 {$IF sizeof(t_knlist)<>48}{$STOP sizeof(t_knlist)<>48}{$ENDIF}

 p_filterops=^t_filterops;
 t_filterops=packed record
  f_isfd  :Integer;  // true if ident == filedescriptor
  _align  :Integer;
  f_attach:function (kn:p_knote):Integer;
  f_detach:procedure(kn:p_knote);
  f_event :function (kn:p_knote;hint:QWORD):Integer;
  f_touch :procedure(kn:p_knote;kev:p_kevent;_type:QWORD);
 end;
 {$IF sizeof(t_filterops)<>40}{$STOP sizeof(t_filterops)<>40}{$ENDIF}

 t_knote=packed object
  kn_link   :SLIST_ENTRY; // (knote) for kq
  kn_selnext:SLIST_ENTRY; // (knote) for struct selinfo
  kn_knlist :p_knlist;    // f_attach populated
  kn_tqe    :TAILQ_ENTRY; // (knote)
  kn_kq     :Pointer;     // (kqueue) which queue we are on
  kn_kevent :t_kevent;
  kn_status :Integer;     // protected by kq lock
  kn_sfflags:Integer;     // saved filter flags
  kn_sdata  :Ptrint;      // saved data field
  kn_ptr    :packed record
   case Byte of
    0:(p_fp  :Pointer); // (file)      file data pointer
    1:(p_proc:Pointer); // (proc)      proc pointer
    2:(p_aio :Pointer); // (aiocblist) AIO job pointer
    3:(p_lio :Pointer); // (aioliojob) LIO job pointer
  end;
  kn_fop    :p_filterops;
  kn_hook   :Pointer;
  kn_hookid :Integer;
  _align    :Integer;
  //
  property kn_id    :PtrUint  read kn_kevent.ident  write kn_kevent.ident ;
  property kn_filter:SmallInt read kn_kevent.filter write kn_kevent.filter;
  property kn_flags :Word     read kn_kevent.flags  write kn_kevent.flags ;
  property kn_fflags:DWORD    read kn_kevent.fflags write kn_kevent.fflags;
  property kn_data  :Ptrint   read kn_kevent.data   write kn_kevent.data  ;
  property kn_fp    :Pointer  read kn_ptr.p_fp      write kn_ptr.p_fp     ;
 end;
 {$IF sizeof(t_knote)<>128}{$STOP sizeof(t_knote)<>128}{$ENDIF}

 p_kevent_copyops=^t_kevent_copyops;
 t_kevent_copyops=packed record
  arg      :Pointer;
  k_copyout:function(arg:Pointer;kevp:p_kevent;count:Integer):Integer;
  k_copyin :function(arg:Pointer;kevp:p_kevent;count:Integer):Integer;
 end;
 {$IF sizeof(t_kevent_copyops)<>24}{$STOP sizeof(t_kevent_copyops)<>24}{$ENDIF}

procedure EV_SET(kevp:p_kevent;a:PtrUint;b:SmallInt;c:Word;d:DWORD;e:Ptrint;f:Pointer);

implementation

procedure EV_SET(kevp:p_kevent;a:PtrUint;b:SmallInt;c:Word;d:DWORD;e:Ptrint;f:Pointer);
begin
 (kevp)^.ident :=(a);
 (kevp)^.filter:=(b);
 (kevp)^.flags :=(c);
 (kevp)^.fflags:=(d);
 (kevp)^.data  :=(e);
 (kevp)^.udata :=(f);
end;


end.

