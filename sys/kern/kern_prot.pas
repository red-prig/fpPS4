unit kern_prot;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 MAXLOGNAME =17; { max login name length (incl. NUL) }

function sys_getpid():Integer;
function sys_getppid():Integer;
function sys_getpgrp():Integer;
function sys_getpgid():Integer;
function sys_getsid():Integer;
function sys_getuid():Integer;
function sys_geteuid():Integer;
function sys_getgid():Integer;
function sys_getegid():Integer;
function sys_getgroups(gidsetsize:DWORD;gidset:PInteger):Integer;
function sys_setsid():Integer;
function sys_setpgid(pid,pgid:Integer):Integer;
function sys_setuid(uid:Integer):Integer;
function sys_seteuid(euid:Integer):Integer;
function sys_setgid(gid:Integer):Integer;
function sys_setegid(egid:Integer):Integer;
function sys_setgroups(gidsetsize:DWORD;gidset:PInteger):Integer;
function sys_setreuid(ruid,euid:Integer):Integer;
function sys_setregid(rgid,egid:Integer):Integer;
function sys_setresuid(ruid,euid,suid:Integer):Integer;
function sys_setresgid(rgid,egid,sgid:Integer):Integer;
function sys_getresuid(ruid,euid,suid:PInteger):Integer;
function sys_getresgid(rgid,egid,sgid:PInteger):Integer;
function sys_issetugid():Integer;
function sys_getlogin(namebuf:PChar;namelen:DWORD):Integer;
function sys_setlogin(namebuf:PChar):Integer;

function p_cansignal(signum:Integer):Integer;

implementation

uses
 errno,
 systm,
 signal,
 kern_thr,
 md_proc;

{
 * System calls related to processes and protection
 }

function sys_getpid():Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);
 td^.td_retval[0]:=g_pid;
 Exit(0);
end;

function sys_getppid():Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);
 td^.td_retval[0]:=1; //psevodo parent id
 Exit(0);
end;

{
 * Get process group ID; note that POSIX getpgrp takes no parameter.
 }
function sys_getpgrp():Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);
 td^.td_retval[0]:=0; //psevodo group id
 Exit(0);
end;

{ Get an arbitary pid's process group id }
function sys_getpgid():Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);
 td^.td_retval[0]:=0; //psevodo group id
 Exit(0);
end;

{
 * Get an arbitary pid's session id.
 }
function sys_getsid():Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);
 td^.td_retval[0]:=0; //psevodo session id
 Exit(0);
end;

function sys_getuid():Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);
 td^.td_retval[0]:=0; //psevodo user id
 Exit(0);
end;

function sys_geteuid():Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);
 td^.td_retval[0]:=0; //psevodo user id
 Exit(0);
end;

function sys_getgid():Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);
 td^.td_retval[0]:=0; //psevodo group id
 Exit(0);
end;

{
 * Get effective group ID.  The 'egid' is groups[0], and could be obtained
 * via getgroups.  This syscall exists because it is somewhat painful to do
 * correctly in a library function.
 }
function sys_getegid():Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);
 td^.td_retval[0]:=0; //psevodo group id
 Exit(0);
end;

function sys_getgroups(gidsetsize:DWORD;gidset:PInteger):Integer;
const
 cr_ngroups=1;
var
 td:p_kthread;
 ngrp:Integer;
 groups:array[0..0] of Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (gidsetsize < cr_ngroups) then
 begin
  if (gidsetsize=0) then
   ngrp:=0
  else
   Exit(EINVAL);
 end else
  ngrp:=cr_ngroups;

 groups[0]:=0;

 if (gidsetsize > 0) then
  Result:=copyout(@groups, gidset, ngrp * sizeof(Integer));

 if (Result=0) then
  td^.td_retval[0]:=ngrp;
end;

function sys_setsid():Integer;
begin
 Exit(EPERM);
end;

{
 * set process group (setpgid/old setpgrp)
 *
 * caller does setpgid(targpid, targpgid)
 *
 * pid must be caller or child of caller (ESRCH)
 * if a child
 * pid must be in same session (EPERM)
 * pid can't have done an exec (EACCES)
 * if pgid<>pid
 *  there must exist some pid in same session having pgid (EPERM)
 * pid must not be session leader (EPERM)
 }
function sys_setpgid(pid,pgid:Integer):Integer;
begin
 if (pid<>0) and (pid<>g_pid) then
 begin
  Exit(ESRCH);
 end;

 Exit(EPERM);
end;

{
 * Use the clause in B.4.2.2 that allows setuid/setgid to be 4.2/4.3BSD
 * compatible.  It says that setting the uid/gid to euid/egid is a special
 * case of 'appropriate privilege'.  Once the rules are expanded out, this
 * basically means that setuid(nnn) sets all three id's, in all permitted
 * cases unless _POSIX_SAVED_IDS is enabled.  In that case, setuid(getuid())
 * does not set the saved id - this is dangerous for traditional BSD
 * programs.  For this reason, we *really* do not want to set
 * _POSIX_SAVED_IDS and do not want to clear POSIX_APPENDIX_B_4_2_2.
 }
function sys_setuid(uid:Integer):Integer;
begin
 Result:=0;
 if (uid<>0) then //not psevodo user id
 begin
  Exit(EPERM);
 end;
end;

function sys_seteuid(euid:Integer):Integer;
begin
 Result:=0;
 if (euid<>0) then //not psevodo user id
 begin
  Exit(EPERM);
 end;
end;

function sys_setgid(gid:Integer):Integer;
begin
 Result:=0;
 if (gid<>0) then //not group user id
 begin
  Exit(EPERM);
 end;
end;

function sys_setegid(egid:Integer):Integer;
begin
 Result:=0;
 if (egid<>0) then //not group user id
 begin
  Exit(EPERM);
 end;
end;

function sys_setgroups(gidsetsize:DWORD;gidset:PInteger):Integer;
const
 ngroups_max=1023;
var
 groups:array[0..0] of Integer;
begin
 Result:=0;

 if (gidsetsize > ngroups_max + 1) then
  Exit(EINVAL);

 Result:=copyin(gidset, @groups, 1 * sizeof(Integer));
 if (Result<>0) then Exit;

 if (gidsetsize<>1) then Exit(EPERM);

 if (groups[0]<>0) then //not group user id
 begin
  Exit(EPERM);
 end;
end;

function sys_setreuid(ruid,euid:Integer):Integer;
begin
 Result:=0;
 if (ruid<>0) and (ruid<>-1) then //not psevodo user id
 begin
  Exit(EPERM);
 end;
 if (euid<>0) and (euid<>-1) then //not psevodo user id
 begin
  Exit(EPERM);
 end;
end;

function sys_setregid(rgid,egid:Integer):Integer;
begin
 Result:=0;
 if (rgid<>0) and (rgid<>-1) then //not psevodo group id
 begin
  Exit(EPERM);
 end;
 if (egid<>0) and (egid<>-1) then //not psevodo group id
 begin
  Exit(EPERM);
 end;
end;

{
 * setresuid(ruid, euid, suid) is like setreuid except control over the saved
 * uid is explicit.
 }
function sys_setresuid(ruid,euid,suid:Integer):Integer;
begin
 Result:=0;
 if (ruid<>0) and (ruid<>-1) then //not psevodo user id
 begin
  Exit(EPERM);
 end;
 if (euid<>0) and (euid<>-1) then //not psevodo user id
 begin
  Exit(EPERM);
 end;
 if (suid<>0) and (suid<>-1) then //not psevodo session id
 begin
  Exit(EPERM);
 end;
end;

{
 * setresgid(rgid, egid, sgid) is like setregid except control over the saved
 * gid is explicit.
 }
function sys_setresgid(rgid,egid,sgid:Integer):Integer;
begin
 Result:=0;
 if (rgid<>0) and (rgid<>-1) then //not psevodo group id
 begin
  Exit(EPERM);
 end;
 if (egid<>0) and (egid<>-1) then //not psevodo group id
 begin
  Exit(EPERM);
 end;
 if (sgid<>0) and (sgid<>-1) then //not psevodo group id
 begin
  Exit(EPERM);
 end;
end;

function sys_getresuid(ruid,euid,suid:PInteger):Integer;
var
 cr_uid:Integer;
 error1,error2,error3:Integer;
begin
 cr_uid:=0; //psevodo user id

 error1:=0;
 error2:=0;
 error3:=0;

 if (ruid<>nil) then
  error1:=copyout(@cr_uid, ruid, sizeof(Integer));

 if (euid<>nil) then
  error2:=copyout(@cr_uid, euid, sizeof(Integer));

 if (suid<>nil) then
  error3:=copyout(@cr_uid, suid, sizeof(Integer));

 if (error1<>0) then
 begin
  Exit(error1);
 end else
 if (error2<>0) then
 begin
  Exit(error2);
 end else
 begin
  Exit(error3);
 end;
end;

function sys_getresgid(rgid,egid,sgid:PInteger):Integer;
var
 cr_gid:Integer;
 error1,error2,error3:Integer;
begin
 cr_gid:=0; //psevodo group id

 error1:=0;
 error2:=0;
 error3:=0;

 if (rgid<>nil) then
  error1:=copyout(@cr_gid, rgid, sizeof(Integer));

 if (egid<>nil) then
  error2:=copyout(@cr_gid, egid, sizeof(Integer));

 if (sgid<>nil) then
  error3:=copyout(@cr_gid, sgid, sizeof(Integer));

 if (error1<>0) then
 begin
  Exit(error1);
 end else
 if (error2<>0) then
 begin
  Exit(error2);
 end else
 begin
  Exit(error3);
 end;
end;

function sys_issetugid():Integer;
begin
 Exit(0);
end;

{
 * Get login name, if available.
 }
function sys_getlogin(namebuf:PChar;namelen:DWORD):Integer;
var
 login:array[0..MAXLOGNAME-1] of AnsiChar;
 len:ptrint;
begin
 if (namelen > MAXLOGNAME) then
  namelen:=MAXLOGNAME;

 login:='user';
 len:=strlen(@login);

 if (len > namelen) then
  Exit(ERANGE);

 Exit(copyout(@login, namebuf, len));
end;

{
 * Set login name.
 }
function sys_setlogin(namebuf:PChar):Integer;
begin
 //error:=priv_check(td, PRIV_PROC_SETLOGIN);
 Exit(EPERM);
end;


{-
 * Determine whether td may deliver the specified signal to p.
 * Returns: 0 for permitted, an errno value otherwise
 * Locks: Sufficient locks to protect various components of td and p
 *        must be held.  td must be curthread, and a lock must be
 *        held for p.
 * References: td and p must be valid for the lifetime of the call
 }
function p_cansignal(signum:Integer):Integer;
begin
 if (curkthread=nil) then Exit(-1);

 {
  * UNIX signalling semantics require that processes in the same
  * session always be able to deliver SIGCONT to one another,
  * overriding the remaining protections.
  }
 { XXX: This will require an additional lock of some sort. }
 if (signum=SIGCONT) {and (td^.td_proc^.p_session=p^.p_session)} then
  Exit(0);
 {
  * Some compat layers use SIGTHR and higher signals for
  * communication between different kernel threads of the same
  * process, so that they expect that it's always possible to
  * deliver them, even for suid applications where cr_cansignal() can
  * deny such ability for security consideration.  It should be
  * pretty safe to do since the only way to create two processes
  * with the same p_leader is via rfork(2).
  }
 if (signum >= SIGTHR) and
    (signum < SIGTHR + 4) then
  Exit(0);

 //Exit(cr_cansignal(td^.td_ucred, p, signum));
 Exit(0);
end;


end.

