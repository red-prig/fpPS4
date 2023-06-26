unit kern_callout;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 LFQueue;

type
 t_callout_func=procedure(arg:Pointer);

 p_callout=^t_callout;
 t_callout=record
  c_queue:Pointer;
  c_links:TAILQ_ENTRY;
  c_time :Int64;
  c_arg  :Pointer;
  c_func :t_callout_func;
  c_lock :Pointer; //mtx/rwlock
  c_flags:QWORD;
  c_timer:THandle;
 end;

const
 CALLOUT_LOCAL_ALLOC   =$0001; // was allocated from callfree
 CALLOUT_ACTIVE        =$0002; // callout is currently active
 CALLOUT_PENDING       =$0004; // callout is waiting for timeout
 CALLOUT_MPSAFE        =$0008; // callout handler is mp safe
 CALLOUT_RETURNUNLOCKED=$0010; // handler returns with mtx unlocked
 CALLOUT_SHAREDLOCK    =$0020; // callout lock held in shared mode
 CALLOUT_RWLOCK        =$0040; // callout used rw lock

implementation

end.

