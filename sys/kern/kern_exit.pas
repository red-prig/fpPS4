unit kern_exit;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 _resource;

const
 WCOREFLAG=&0200;
 _WSTOPPED=&0177; { _WSTATUS if process is stopped }

 {
 * Option bits for the third argument of wait4.  WNOHANG causes the
 * wait to not hang if there are no stopped or terminated processes, rather
 * returning an error indication in this case (pid==0).  WUNTRACED
 * indicates that the caller should receive status about untraced children
 * which stop due to signals.  If children are stopped and a wait without
 * this option is done, it is as though they were still running... nothing
 * about them is returned. WNOWAIT only request information about zombie,
 * leaving the proc around, available for later waits.
 }
 WNOHANG   =1;  { Don't hang in wait. }
 WUNTRACED =2;  { Tell about stopped, untraced children. }
 WSTOPPED  =WUNTRACED; { SUS compatibility }
 WCONTINUED=4;  { Report a job control continued process. }
 WNOWAIT   =8;  { Poll only. Don't delete the proc entry. }
 WEXITED   =16; { Wait for exited processes. }
 WTRAPPED  =32; { Wait for a process to hit a trap or a breakpoint. }

 WLINUXCLONE=$80000000; { Wait for kthread spawned from linux_clone. }

 {
 * Tokens for special values of the "pid" parameter to wait4.
 * Extended struct __wrusage to collect rusage for both the target
 * process and its children within one wait6() call.
 }
 WAIT_ANY   =(-1); { any process }
 WAIT_MYPGRP=0;    { any process in my process group }

type
 _W_INT=Integer;

function _WSTATUS(x:Integer):Integer; inline;
function WIFSTOPPED(x:Integer):Boolean; inline;
function WSTOPSIG(x:Integer):Integer; inline;
function WIFSIGNALED(x:Integer):Boolean; inline;
function WTERMSIG(x:Integer):Integer; inline;
function WIFEXITED(x:Integer):Boolean; inline;
function WEXITSTATUS(x:Integer):Integer; inline;
function WIFCONTINUED(x:Integer):Boolean; inline; { 0x13=SIGCONT }
function WCOREDUMP(x:Integer):Boolean; inline;
function W_STOPCODE(sig:Integer):Integer; inline;
function W_EXITCODE(ret,sig:Integer):Integer; inline;

procedure exit1(rv:Integer);

procedure sys_sys_exit(rval:Integer);
function  sys_wait4(pid:Integer;status:PInteger;options:Integer;rusage:p_rusage):Integer;

implementation

uses
 errno,
 systm;

function _WSTATUS(x:Integer):Integer; inline;
begin
 Result:=(_W_INT(x) and &0177);
end;

function WIFSTOPPED(x:Integer):Boolean; inline;
begin
 Result:=(_WSTATUS(x)=_WSTOPPED);
end;

function WSTOPSIG(x:Integer):Integer; inline;
begin
 Result:=(_W_INT(x) shr 8) ;
end;

function WIFSIGNALED(x:Integer):Boolean; inline;
begin
 Result:=(_WSTATUS(x)<>_WSTOPPED) and (_WSTATUS(x)<>0);
end;

function WTERMSIG(x:Integer):Integer; inline;
begin
 Result:=(_WSTATUS(x));
end;

function WIFEXITED(x:Integer):Boolean; inline;
begin
 Result:=(_WSTATUS(x)=0);
end;

function WEXITSTATUS(x:Integer):Integer; inline;
begin
 Result:=(_W_INT(x) shr 8);
end;

function WIFCONTINUED(x:Integer):Boolean; inline; { 0x13=SIGCONT }
begin
 Result:=(x=$13);
end;

function WCOREDUMP(x:Integer):Boolean; inline;
begin
 Result:=(_W_INT(x) and WCOREFLAG)<>0;
end;

function W_STOPCODE(sig:Integer):Integer; inline;
begin
 Result:=(sig shl 8) or _WSTOPPED;
end;

function W_EXITCODE(ret,sig:Integer):Integer; inline;
begin
 Result:=(ret shl 8) or sig;
end;

procedure exit1(rv:Integer);
begin
 Halt(rv);
end;

procedure sys_sys_exit(rval:Integer);
begin
 exit1(W_EXITCODE(rval, 0));
 // NOTREACHED
end;

function kern_wait(pid:Integer;status:PInteger;options:Integer;rusage:p_rusage):Integer;
begin
 options:=options or WEXITED or WTRAPPED;

 { If we don't know the option, just return. }
 if ((options and (not (WUNTRACED or WNOHANG or WCONTINUED or WNOWAIT or
                        WEXITED or WTRAPPED or WLINUXCLONE)))<>0) then
 begin
  Exit(EINVAL);
 end;

 if ((options and (WEXITED or WUNTRACED or WCONTINUED or WTRAPPED))=0) then
 begin
  {
   * We will be unable to find any matching processes,
   * because there are no known events to look for.
   * Prefer to Exiterror instead of blocking
   * indefinitely.
   }
  Exit(EINVAL);
 end;

 Exit(ECHILD);
end;

{
 * The dirty work is handled by kern_wait().
 }
function sys_wait4(pid:Integer;status:PInteger;options:Integer;rusage:p_rusage):Integer;
var
 ru:t_rusage;
 rup:p_rusage;
 error,_status:Integer;
begin
 if (rusage<>nil) then
  rup:=@ru
 else
  rup:=nil;

 error:=kern_wait(pid, @_status, options, rup);

 if (status<>nil) and (error=0) then
  error:=copyout(@_status, status, sizeof(_status));

 if (rusage<>nil) and (error=0) then
  error:=copyout(@ru, rusage, sizeof(t_rusage));

 Exit(error);
end;



end.

