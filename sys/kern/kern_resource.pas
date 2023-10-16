unit kern_resource;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vmparam,
 kern_param,
 _resource;

function  lim_max(which:Integer):QWORD;
function  lim_cur(which:Integer):QWORD;
procedure lim_rlimit(which:Integer;rlp:p_rlimit);

function  sys_getrlimit(which:Integer;rlp:Pointer):Integer;
function  sys_setrlimit(which:Integer;rlp:Pointer):Integer;

function  sys_getpriority(which,who:Integer):Integer;
function  sys_setpriority(which,who,prio:Integer):Integer;

function  sys_getrusage(who:Integer;rusage:Pointer):Integer;

implementation

uses
 errno,
 systm,
 kern_thr,
 kern_proc,
 md_proc,
 md_resource;

function lim_max(which:Integer):QWORD;
begin
 Result:=RLIM_INFINITY;
 Case which of
  RLIMIT_DATA   :Result:=MAXDSIZ;
  RLIMIT_STACK  :Result:=MAXSSIZ;
  RLIMIT_MEMLOCK:Result:=pageablemem;
  RLIMIT_VMEM   :Result:=pageablemem;
  RLIMIT_NPROC  :Result:=maxprocperuid;
  RLIMIT_NOFILE :Result:=maxfilesperproc;
  else;
 end;
end;

function lim_cur(which:Integer):QWORD;
begin
 Result:=RLIM_INFINITY;
 Case which of
  RLIMIT_DATA   :Result:=MAXDSIZ;
  RLIMIT_STACK  :Result:=MAXSSIZ;
  RLIMIT_MEMLOCK:Result:=pageablemem;
  RLIMIT_VMEM   :Result:=pageablemem;
  RLIMIT_NPROC  :Result:=maxprocperuid;
  RLIMIT_NOFILE :Result:=maxfilesperproc;
  else;
 end;
end;

procedure lim_rlimit(which:Integer;rlp:p_rlimit);
begin
 rlp^.rlim_cur:=lim_max(which);
 rlp^.rlim_max:=lim_cur(which);
end;

function sys_getrlimit(which:Integer;rlp:Pointer):Integer;
var
 rlim:t_rlimit;
begin
 if (which >= RLIM_NLIMITS) then
  Exit(EINVAL);

 lim_rlimit(which, @rlim);

 Result:=copyout(@rlim, rlp, sizeof(t_rlimit));
end;

function sys_setrlimit(which:Integer;rlp:Pointer):Integer;
var
 alim:t_rlimit;
begin
 Result:=copyin(rlp, @alim, sizeof(t_rlimit));
 if (Result<>0) then Exit;

 //error:=kern_setrlimit(td, uap^.which, @alim);
 Exit(0);
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

function sys_getrusage(who:Integer;rusage:Pointer):Integer;
var
 ru:t_rusage;
begin
 Result:=kern_getrusage(who, @ru);
 if (Result=0) then
 begin
  Result:=copyout(@ru, rusage, sizeof(t_rusage));
 end;
end;


end.


