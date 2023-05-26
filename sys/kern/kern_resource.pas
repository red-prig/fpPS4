unit kern_resource;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 time,
 vmparam,
 vfile;

const
 RLIMIT_CPU    = 0;  // maximum cpu time in seconds
 RLIMIT_FSIZE  = 1;  // maximum file size
 RLIMIT_DATA   = 2;  // data size
 RLIMIT_STACK  = 3;  // stack size
 RLIMIT_CORE   = 4;  // core file size
 RLIMIT_RSS    = 5;  // resident set size
 RLIMIT_MEMLOCK= 6;  // locked-in-memory address space
 RLIMIT_NPROC  = 7;  // number of processes
 RLIMIT_NOFILE = 8;  // number of open files
 RLIMIT_SBSIZE = 9;  // maximum size of all socket buffers
 RLIMIT_VMEM   =10;  // virtual process size (incl. mmap)
 RLIMIT_AS     =RLIMIT_VMEM; // standard name for RLIMIT_VMEM
 RLIMIT_NPTS   =11;  // pseudo-terminals
 RLIMIT_SWAP   =12;  // swap used

function lim_max(which:Integer):QWORD;
function lim_cur(which:Integer):QWORD;

const
 RUSAGE_SELF    = 0;
 RUSAGE_CHILDREN=-1;
 RUSAGE_THREAD  = 1;

type
 p_rusage=^t_rusage;
 t_rusage=packed record
  ru_utime   :timeval; // user time used
  ru_stime   :timeval; // system time used
  ru_maxrss  :DWORD;   // max resident set size
  ru_ixrss   :DWORD;   // integral shared memory size *
  ru_idrss   :DWORD;   // integral unshared data
  ru_isrss   :DWORD;   // integral unshared stack
  ru_minflt  :DWORD;   // page reclaims
  ru_majflt  :DWORD;   // page faults
  ru_nswap   :DWORD;   // swaps
  ru_inblock :DWORD;   // block input operations
  ru_oublock :DWORD;   // block output operations
  ru_msgsnd  :DWORD;   // messages sent
  ru_msgrcv  :DWORD;   // messages received
  ru_nsignals:DWORD;   // signals received
  ru_nvcsw   :DWORD;   // voluntary context switches
  ru_nivcsw  :DWORD;   // involuntary
 end;

const
 //Process priority specifications to get/setpriority.
 PRIO_MIN=-20;
 PRIO_MAX= 20;

 PRIO_PROCESS=0;
 PRIO_PGRP   =1;
 PRIO_USER   =2;

function sys_getpriority(which,who:Integer):Integer;
function sys_setpriority(which,who,prio:Integer):Integer;

implementation

uses
 errno,
 kern_thr,
 md_proc;

function lim_max(which:Integer):QWORD;
begin
 Result:=0;
 Case which of
  RLIMIT_DATA   :Result:=MAXDSIZ;
  RLIMIT_STACK  :Result:=MAXSSIZ;
  RLIMIT_MEMLOCK:Result:=pageablemem;
  RLIMIT_VMEM   :Result:=pageablemem;
  RLIMIT_NOFILE :Result:=maxfilesperproc;
  else;
 end;
end;

function lim_cur(which:Integer):QWORD;
begin
 Result:=0;
 Case which of
  RLIMIT_DATA   :Result:=MAXDSIZ;
  RLIMIT_STACK  :Result:=MAXSSIZ;
  RLIMIT_MEMLOCK:Result:=pageablemem;
  RLIMIT_VMEM   :Result:=pageablemem;
  RLIMIT_NOFILE :Result:=maxfilesperproc;
  else;
 end;
end;

function cur_proc_get_nice():Integer; inline;
begin
 Result:=get_proc_prio;
end;

function cur_proc_donice(n:Integer):Integer;
begin
 if (n > PRIO_MAX) then n:=PRIO_MAX;
 if (n < PRIO_MIN) then n:=PRIO_MIN;

 //if (n < cur_proc_get_nice) and (priv_check(td, PRIV_SCHED_SETPRIORITY) <> 0) then
 // Exit(EACCES);

 Result:=set_proc_prio(n);
 if (Result<>0) then Result:=EPERM;
end;

{
 * Resource controls and accounting.
 }
function sys_getpriority(which,who:Integer):Integer;
var
 td:p_kthread;
 error,low:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 error:=0;
 low:=PRIO_MAX+1;

 case (which) of

  PRIO_PROCESS:
   begin
    if (who=0) or (who=g_pid)  then
    begin
     low:=cur_proc_get_nice;
    end;
   end;

  PRIO_PGRP:
   begin
    if (who=0) then
    begin
     low:=cur_proc_get_nice;
    end;
   end;

  PRIO_USER:
   begin
    if (who=0) or (who=g_pid)  then
    begin
     low:=cur_proc_get_nice;
    end;
   end;

  else
   error:=EINVAL;
 end;

 if (low=PRIO_MAX+1) and (error=0) then
  error:=ESRCH;

 td^.td_retval[0]:=low;
 Exit(error);
end;

function sys_setpriority(which,who,prio:Integer):Integer;
var
 found,error:Integer;
begin
 found:=0;
 error:=0;

 case (which) of

  PRIO_PROCESS:
   begin
    if (who=0) or (who=g_pid)  then
    begin
     PROC_LOCK();
     error:=cur_proc_donice(prio);
     PROC_UNLOCK();
     Inc(found);
    end;
   end;

  PRIO_PGRP:
   begin
    if (who=0) then
    begin
     PROC_LOCK();
     error:=cur_proc_donice(prio);
     PROC_UNLOCK();
     Inc(found);
    end;
   end;

  PRIO_USER:
   begin
    if (who=0) or (who=g_pid)  then
    begin
     PROC_LOCK();
     error:=cur_proc_donice(prio);
     PROC_UNLOCK();
     Inc(found);
    end;
   end;

  else
   error:=EINVAL;
 end;

 if (found=0) and (error=0) then
  error:=ESRCH;

 Exit(error);
end;



end.

