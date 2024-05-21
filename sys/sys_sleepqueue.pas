unit sys_sleepqueue;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_thr;

const
 SLEEPQ_TYPE         =$ff;  // Mask of sleep queue types.
 SLEEPQ_SLEEP        =$00;  // Used by sleep/wakeup.
 SLEEPQ_CONDVAR      =$01;  // Used for a cv.
 SLEEPQ_PAUSE        =$02;  // Used by pause.
 SLEEPQ_SX           =$03;  // Used by an sx lock.
 SLEEPQ_LK           =$04;  // Used by a lockmgr.
 SLEEPQ_INTERRUPTIBLE=$100; // Sleep is interruptible.
 SLEEPQ_STOP_ON_BDRY =$200; // Stop sleeping thread

function  sleepq_alloc:Pointer; external;
procedure sleepq_free(sq:Pointer); external;
procedure sleepq_lock(wchan:Pointer); external;
procedure sleepq_release(wchan:Pointer); external;
function  sleepq_lookup(wchan:Pointer):Pointer; external;
procedure sleepq_add(wchan,lock,wmesg:Pointer;flags,queue:Integer); external;
procedure sleepq_set_timeout(wchan:Pointer;time:Int64); external;
function  sleepq_sleepcnt(wchan,lock:Pointer;flags,queue:Integer):DWORD; external;
procedure sleepq_wait(wchan:Pointer;pri:Integer); external;
function  sleepq_wait_sig(wchan:Pointer;pri:Integer):Integer; external;
function  sleepq_timedwait(wchan:Pointer;pri:Integer):Integer; external;
function  sleepq_timedwait_sig(wchan:Pointer;pri:Integer):Integer; external;
function  sleepq_get_type(wchan:Pointer;pri:Integer):Integer; external;
function  sleepq_signal(wchan:Pointer;flags,pri,queue:Integer):Integer; external;
function  sleepq_signalto(wchan:Pointer;flags,pri,queue:Integer;std:p_kthread):Integer; external;
function  sleepq_broadcast(wchan:Pointer;flags,pri,queue:Integer):Integer; external;
procedure sleepq_remove(td:p_kthread;wchan:Pointer); external;
function  sleepq_abort(td:p_kthread;intrval:Integer):Integer; external;

implementation

end.

