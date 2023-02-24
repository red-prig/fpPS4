unit kern_sig;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 gtailq,
 sys_kernel,
 time,
 signal,
 signalvar,
 kern_thread;

const
 SA_KILL    =$01; // terminates process by default
 SA_CORE    =$02; // ditto and coredumps
 SA_STOP    =$04; // suspend process
 SA_TTYSTOP =$08; // ditto, from tty
 SA_IGNORE  =$10; // ignore by default
 SA_CONT    =$20; // continue if suspended
 SA_CANTMASK=$40; // non-maskable, catchable
 SA_PROC    =$80; // deliverable to any thread

 sigproptbl:array[0..30] of Integer=(
  SA_KILL   or SA_PROC,               // SIGHUP
  SA_KILL   or SA_PROC,               // SIGINT
  SA_KILL   or SA_CORE or SA_PROC,    // SIGQUIT
  SA_KILL   or SA_CORE,               // SIGILL
  SA_KILL   or SA_CORE,               // SIGTRAP
  SA_KILL   or SA_CORE,               // SIGABRT
  SA_KILL   or SA_CORE or SA_PROC,    // SIGEMT
  SA_KILL   or SA_CORE,               // SIGFPE
  SA_KILL   or SA_PROC,               // SIGKILL
  SA_KILL   or SA_CORE,               // SIGBUS
  SA_KILL   or SA_CORE,               // SIGSEGV
  SA_KILL   or SA_CORE,               // SIGSYS
  SA_KILL   or SA_PROC,               // SIGPIPE
  SA_KILL   or SA_PROC,               // SIGALRM
  SA_KILL   or SA_PROC,               // SIGTERM
  SA_IGNORE or SA_PROC,               // SIGURG
  SA_STOP   or SA_PROC,               // SIGSTOP
  SA_STOP   or SA_TTYSTOP or SA_PROC, // SIGTSTP
  SA_IGNORE or SA_CONT    or SA_PROC, // SIGCONT
  SA_IGNORE or SA_PROC,               // SIGCHLD
  SA_STOP   or SA_TTYSTOP or SA_PROC, // SIGTTIN
  SA_STOP   or SA_TTYSTOP or SA_PROC, // SIGTTOU
  SA_IGNORE or SA_PROC,               // SIGIO
  SA_KILL,                            // SIGXCPU
  SA_KILL,                            // SIGXFSZ
  SA_KILL   or SA_PROC,               // SIGVTALRM
  SA_KILL   or SA_PROC,               // SIGPROF
  SA_IGNORE or SA_PROC,               // SIGWINCH
  SA_IGNORE or SA_PROC,               // SIGINFO
  SA_KILL   or SA_PROC,               // SIGUSR1
  SA_KILL   or SA_PROC                // SIGUSR2
);

Function  sys_sigaction(sig:Integer;
                        act,oact:p_sigaction_t;
                        flags:Integer):Integer;

Function  sys_sigprocmask(how:Integer;
                          _set:p_sigset_t;
                          oset:p_sigset_t;
                          flags:Integer):Integer;

Function  sys_sigpending(oset:p_sigset_t):Integer;

Function  sys_sigwait(oset:p_sigset_t;sig:PInteger):Integer;
Function  sys_sigtimedwait(oset:p_sigset_t;info:p_siginfo_t;timeout:ptimespec):Integer;
Function  sys_sigwaitinfo(oset:p_sigset_t;info:p_siginfo_t):Integer;

Function  sys_sigsuspend(sigmask:p_sigset_t):Integer;

Function  sys_sigaltstack(ss:p_stack_t;oss:p_stack_t):Integer;

Function  sigonstack(sp:size_t):Integer;
procedure sigqueue_init(list:p_sigqueue);
procedure tdsigcleanup(td:p_kthread);

procedure tdsignal(td:p_kthread;sig:Integer);
procedure tdksignal(td:p_kthread;sig:Integer;ksi:p_ksiginfo);
procedure sigexit(td:p_kthread;sig:Integer);

Function  kern_sigprocmask(td:p_kthread;
                           how:Integer;
                           _set:p_sigset_t;
                           oset:p_sigset_t;
                           flags:Integer
                          ):Integer;

procedure ast;

function  ps_mtx_lock:Integer;
function  ps_mtx_unlock:Integer;

implementation

uses
 ntapi,
 systm,
 kern_mtx,
 kern_time,
 vm_machdep,
 machdep,
 trap;

const
 max_pending_per_proc=128;

 kern_forcesigexit=1;

var
 g_p_sigqueue     :sigqueue_t;

 p_pendingcnt     :Integer=0;
 signal_overflow  :Integer=0;
 signal_alloc_fail:Integer=0;

function ksiginfo_alloc():p_ksiginfo;
begin
 Result:=AllocMem(SizeOf(ksiginfo_t));
end;

procedure ksiginfo_free(ksi:p_ksiginfo);
begin
 FreeMem(ksi);
end;

Function ksiginfo_tryfree(ksi:p_ksiginfo):Boolean;
begin
 Result:=False;
 if ((ksi^.ksi_flags and KSI_EXT)=0) then
 begin
  FreeMem(ksi);
  Result:=True;
 end;
end;

procedure sigqueue_init(list:p_sigqueue);
begin
 SIGEMPTYSET(@list^.sq_signals);
 SIGEMPTYSET(@list^.sq_kill);
 TAILQ_INIT(@list^.sq_list);
 list^.sq_flags:=SQ_INIT;
end;

Function sigqueue_get(sq:p_sigqueue;signo:Integer;si:p_ksiginfo):Integer;
var
 ksi,next:p_ksiginfo;
 count:Integer;
begin
 count:=0;

 Assert((sq^.sq_flags and SQ_INIT)<>0,'sigqueue not inited');

 if not SIGISMEMBER(@sq^.sq_signals,signo) then Exit(0);

 if SIGISMEMBER(@sq^.sq_kill,signo) then
 begin
  Inc(count);
  SIGDELSET(@sq^.sq_kill,signo);
 end;

 ksi:=TAILQ_FIRST(@sq^.sq_list);
 While (ksi<>nil) do
 begin
  next:=TAILQ_NEXT(ksi,@ksi^.ksi_link);
  if (ksi^.ksi_info.si_signo=signo) then
  begin
   if (count=0) then
   begin
    TAILQ_REMOVE(@sq^.sq_list,ksi,@ksi^.ksi_link);
    ksi^.ksi_sigq:=nil;
    ksiginfo_copy(ksi,si);
    if ksiginfo_tryfree(ksi) then
    begin
     Dec(p_pendingcnt);
    end;
   end;
   if (count>1) then
   begin
    Inc(count);
    Break;
   end else
   begin
    Inc(count);
   end;
  end;
  ksi:=next;
 end;

 if (count<=1) then
 begin
  SIGDELSET(@sq^.sq_signals,signo);
 end;

 si^.ksi_info.si_signo:=signo;
 Result:=signo;
end;

procedure sigqueue_take(ksi:p_ksiginfo);
var
 kp:p_ksiginfo;
 sq:p_sigqueue;
begin
 if (ksi=nil) or (ksi^.ksi_sigq=nil) then Exit;
 sq:=ksi^.ksi_sigq;

 TAILQ_REMOVE(@sq^.sq_list,ksi,@ksi^.ksi_link);

 ksi^.ksi_sigq:=nil;
 if ((ksi^.ksi_flags and KSI_EXT)=0)then
 begin
  Dec(p_pendingcnt);
 end;

 kp:=TAILQ_FIRST(@sq^.sq_list);
 While (kp<>nil) do
 begin
  if (kp^.ksi_info.si_signo=ksi^.ksi_info.si_signo) then Break;
  kp:=TAILQ_NEXT(kp,@kp^.ksi_link);
 end;

 if (kp=nil) and (not SIGISMEMBER(@sq^.sq_kill,ksi^.ksi_info.si_signo)) then
 begin
  SIGDELSET(@sq^.sq_signals,ksi^.ksi_info.si_signo);
 end;
end;

Function sigqueue_add(sq:p_sigqueue;signo:Integer;si:p_ksiginfo):Integer;
label
 out_set_bit;
var
 ksi:p_ksiginfo;
begin
 Result:=0;

 Assert((sq^.sq_flags and SQ_INIT)<>0,'sigqueue not inited');

 if (signo=SIGKILL) or (signo=SIGSTOP) or (si=nil) then
 begin
  SIGADDSET(@sq^.sq_kill,signo);
  goto out_set_bit;
 end;

 if ((si^.ksi_flags and KSI_INS)<>0) then
 begin
  if ((si^.ksi_flags and KSI_HEAD)<>0) then
   TAILQ_INSERT_HEAD(@sq^.sq_list,si,@si^.ksi_link)
  else
   TAILQ_INSERT_TAIL(@sq^.sq_list,si,@si^.ksi_link);
  si^.ksi_sigq:=sq;
  goto out_set_bit;
 end;

 if (p_pendingcnt>=max_pending_per_proc) then
 begin
  Inc(signal_overflow);
  Result:=EAGAIN;
 end else
 begin
  ksi:=ksiginfo_alloc;
  if (ksi=nil) then
  begin
   Inc(signal_alloc_fail);
   Result:=EAGAIN;
  end else
  begin
   Inc(p_pendingcnt);
   ksiginfo_copy(si,ksi);
   ksi^.ksi_info.si_signo:=signo;
   if ((si^.ksi_flags and KSI_HEAD)<>0) then
    TAILQ_INSERT_HEAD(@sq^.sq_list,ksi,@ksi^.ksi_link)
   else
    TAILQ_INSERT_TAIL(@sq^.sq_list,ksi,@ksi^.ksi_link);
   ksi^.ksi_sigq:=sq;
  end;
 end;

 if ((si^.ksi_flags and KSI_TRAP)<>0) or
    ((si^.ksi_flags and KSI_SIGQ) =0) then
 begin
  if (Result<>0) then
  begin
   SIGADDSET(@sq^.sq_kill,signo);
  end;
  Result:=0;
  goto out_set_bit;
 end;

 if (Result<>0) then Exit;

 out_set_bit:
  SIGADDSET(@sq^.sq_signals,signo);
end;

procedure sigqueue_flush(sq:p_sigqueue);
var
 ksi,next:p_ksiginfo;
begin
 Assert((sq^.sq_flags and SQ_INIT)<>0,'sigqueue not inited');

 ksi:=TAILQ_FIRST(@sq^.sq_list);
 while (ksi<>nil) do
 begin
  next:=TAILQ_NEXT(ksi,@ksi^.ksi_link);
  TAILQ_REMOVE(@sq^.sq_list,ksi,@ksi^.ksi_link);
  ksi^.ksi_sigq:=nil;
  if ksiginfo_tryfree(ksi) then
  begin
   Dec(p_pendingcnt);
  end;
  ksi:=next;
 end;

 SIGEMPTYSET(@sq^.sq_signals);
 SIGEMPTYSET(@sq^.sq_kill);
end;

procedure sigqueue_move_set(src,dst:p_sigqueue;_set:p_sigset_t);
var
 tmp:sigset_t;
 ksi,next:p_ksiginfo;
begin
 Assert((src^.sq_flags and SQ_INIT)<>0,'sigqueue not inited');
 Assert((dst^.sq_flags and SQ_INIT)<>0,'sigqueue not inited');

 ksi:=TAILQ_FIRST(@src^.sq_list);
 while (ksi<>nil) do
 begin
  next:=TAILQ_NEXT(ksi,@ksi^.ksi_link);

  if SIGISMEMBER(_set,ksi^.ksi_info.si_signo) then
  begin
   TAILQ_REMOVE(@src^.sq_list,ksi,@ksi^.ksi_link);
   TAILQ_INSERT_TAIL(@dst^.sq_list,ksi,@ksi^.ksi_link);
   ksi^.ksi_sigq:=dst;
  end;

  ksi:=next;
 end;

 tmp:=src^.sq_kill;
 SIGSETAND(@tmp,_set);
 SIGSETOR(@dst^.sq_kill,@tmp);
 SIGSETNAND(@src^.sq_kill,@tmp);

 tmp:=src^.sq_signals;
 SIGSETAND(@tmp,_set);
 SIGSETOR(@dst^.sq_signals,@tmp);
 SIGSETNAND(@src^.sq_signals,@tmp);
end;

procedure sigqueue_delete_set(sq:p_sigqueue;_set:p_sigset_t);
var
 ksi,next:p_ksiginfo;
begin
 Assert((sq^.sq_flags and SQ_INIT)<>0,'sigqueue not inited');

 ksi:=TAILQ_FIRST(@sq^.sq_list);
 while (ksi<>nil) do
 begin
  next:=TAILQ_NEXT(ksi,@ksi^.ksi_link);

  if SIGISMEMBER(_set,ksi^.ksi_info.si_signo) then
  begin
   TAILQ_REMOVE(@sq^.sq_list,ksi,@ksi^.ksi_link);
   ksi^.ksi_sigq:=nil;
   if ksiginfo_tryfree(ksi) then
   begin
    Dec(p_pendingcnt)
   end;
  end;

  ksi:=next;
 end;

 SIGSETNAND(@sq^.sq_kill,_set);
 SIGSETNAND(@sq^.sq_signals,_set);
end;

procedure sigqueue_delete(sq:p_sigqueue;signo:Integer);
var
 _set:sigset_t;
begin
 SIGEMPTYSET(@_set);
 SIGADDSET(@_set,signo);
 sigqueue_delete_set(sq,@_set);
end;

type
 p_t_sdsp=^_t_sdsp;
 _t_sdsp=record
   worklist:p_sigqueue;
       _set:p_sigset_t
 end;

procedure _for_sdsp(td:p_kthread;data:p_t_sdsp); register; //Tfree_data_cb
begin
 sigqueue_move_set(@td^.td_sigqueue,data^.worklist,data^._set);
end;

procedure sigqueue_delete_set_proc(_set:p_sigset_t);
var
 worklist:sigqueue_t;
 data:_t_sdsp;
begin
 sigqueue_init(@worklist);
 sigqueue_move_set(@g_p_sigqueue,@worklist,_set);

 data.worklist:=@worklist;
 data.    _set:=_set;

 FOREACH_THREAD_IN_PROC(@_for_sdsp,@data);

 sigqueue_flush(@worklist);
end;

procedure sigqueue_delete_proc(signo:Integer);
var
 _set:sigset_t;
begin
 SIGEMPTYSET(@_set);
 SIGADDSET(@_set,signo);
 sigqueue_delete_set_proc(@_set);
end;

procedure sigqueue_delete_stopmask_proc;
var
 _set:sigset_t;
begin
 SIGEMPTYSET(@_set);
 SIGADDSET(@_set,SIGSTOP);
 SIGADDSET(@_set,SIGTSTP);
 SIGADDSET(@_set,SIGTTIN);
 SIGADDSET(@_set,SIGTTOU);
 sigqueue_delete_set_proc(@_set);
end;

///

Function issignal(td:p_kthread;stop_allowed:Integer):Integer; forward;

Function cursig(td:p_kthread;stop_allowed:Integer):Integer;
begin
 Assert((stop_allowed=SIG_STOP_ALLOWED) or (stop_allowed=SIG_STOP_NOT_ALLOWED),'cursig: stop_allowed');

 if SIGPENDING(td) then
 begin
  Result:=issignal(td, stop_allowed);
 end else
 begin
  Result:=0;
 end;
end;

procedure signotify(td:p_kthread);
begin
 if SIGPENDING(td) then
 begin
  thread_lock(td);
  td^.td_flags:=td^.td_flags or TDF_NEEDSIGCHK or TDF_ASTPENDING;
  thread_unlock(td);
 end;
end;

Function sigonstack(sp:size_t):Integer;
var
 td:p_kthread;
begin
 Result:=0;

 td:=curkthread;
 if (td=nil) then Exit;

 if ((td^.td_pflags and TDP_ALTSTACK)<>0) then
 begin
  Result:=ord((sp-size_t(td^.td_sigstk.ss_sp))<td^.td_sigstk.ss_size);
 end;
end;

Function sigprop(sig:Integer):Integer;
begin
 Result:=0;
 if (sig>0) and (sig<NSIG) then
 begin
  Result:=sigproptbl[_SIG_IDX(sig)];
 end;
end;

function ps_mtx_lock:Integer;
begin
 Result:=mtx_lock(@p_sigacts.ps_mtx);
end;

function ps_mtx_unlock:Integer;
begin
 Result:=mtx_unlock(@p_sigacts.ps_mtx);
end;

Function kern_sigaction(sig:Integer;
                        act,oact:p_sigaction_t):Integer;
begin
 if (not _SIG_VALID(sig)) then Exit(EINVAL);

 PROC_LOCK;
 ps_mtx_lock;

 if (oact<>nil) then
 begin
  oact^.sa_mask:=p_sigacts.ps_catchmask[_SIG_IDX(sig)];
  oact^.sa_flags:=0;
  if (SIGISMEMBER(@p_sigacts.ps_sigonstack,sig)) then
   oact^.sa_flags:=oact^.sa_flags or SA_ONSTACK;
  if (not SIGISMEMBER(@p_sigacts.ps_sigintr,sig)) then
   oact^.sa_flags:=oact^.sa_flags or SA_RESTART;
  if (SIGISMEMBER(@p_sigacts.ps_sigreset,sig)) then
   oact^.sa_flags:=oact^.sa_flags or SA_RESETHAND;
  if (SIGISMEMBER(@p_sigacts.ps_signodefer,sig)) then
   oact^.sa_flags:=oact^.sa_flags or SA_NODEFER;
  if (SIGISMEMBER(@p_sigacts.ps_siginfo,sig)) then
  begin
   oact^.sa_flags:=oact^.sa_flags or SA_SIGINFO;
   oact^.u.sa_handler:=p_sigacts.ps_sigact[_SIG_IDX(sig)];
  end else
  begin
   oact^.u.sa_handler:=p_sigacts.ps_sigact[_SIG_IDX(sig)];
  end;
  if (sig=SIGCHLD and p_sigacts.ps_flag and PS_NOCLDSTOP) then
   oact^.sa_flags:=oact^.sa_flags or SA_NOCLDSTOP;
  if (sig=SIGCHLD and p_sigacts.ps_flag and PS_NOCLDWAIT) then
   oact^.sa_flags:=oact^.sa_flags or SA_NOCLDWAIT;
 end;
 if (act<>nil) then
 begin
  if ((sig=SIGKILL) or (sig=SIGSTOP)) and
      (act^.u.code<>SIG_DFL) then
  begin
   ps_mtx_unlock;
   PROC_UNLOCK;
   Result:=EINVAL;
  end;

  p_sigacts.ps_catchmask[_SIG_IDX(sig)]:=act^.sa_mask;
  SIG_CANTMASK(@p_sigacts.ps_catchmask[_SIG_IDX(sig)]);
  if ((act^.sa_flags and SA_SIGINFO)<>0) then
  begin
   p_sigacts.ps_sigact[_SIG_IDX(sig)]:=act^.u.sa_handler;
   SIGADDSET(@p_sigacts.ps_siginfo,sig);
  end else
  begin
   p_sigacts.ps_sigact[_SIG_IDX(sig)]:=act^.u.sa_handler;
   SIGDELSET(@p_sigacts.ps_siginfo,sig);
  end;
  if ((act^.sa_flags and SA_RESTART)=0) then
   SIGADDSET(@p_sigacts.ps_sigintr,sig)
  else
   SIGDELSET(@p_sigacts.ps_sigintr,sig);
  if ((act^.sa_flags and SA_ONSTACK)<>0) then
   SIGADDSET(@p_sigacts.ps_sigonstack,sig)
  else
   SIGDELSET(@p_sigacts.ps_sigonstack,sig);
  if ((act^.sa_flags and SA_RESETHAND)<>0) then
   SIGADDSET(@p_sigacts.ps_sigreset,sig)
  else
   SIGDELSET(@p_sigacts.ps_sigreset,sig);
  if ((act^.sa_flags and SA_NODEFER)<>0) then
   SIGADDSET(@p_sigacts.ps_signodefer,sig)
  else
   SIGDELSET(@p_sigacts.ps_signodefer,sig);
  if (sig=SIGCHLD) then
  begin
   if ((act^.sa_flags and SA_NOCLDSTOP)<>0) then
    p_sigacts.ps_flag:=p_sigacts.ps_flag or PS_NOCLDSTOP
   else
    p_sigacts.ps_flag:=p_sigacts.ps_flag and (not PS_NOCLDSTOP);
   if ((act^.sa_flags and SA_NOCLDWAIT)<>0) then
   begin
    p_sigacts.ps_flag:=p_sigacts.ps_flag or PS_NOCLDWAIT;
   end else
    p_sigacts.ps_flag:=p_sigacts.ps_flag and (not PS_NOCLDWAIT);
   if (p_sigacts.ps_sigact[_SIG_IDX(SIGCHLD)]=sig_t(SIG_IGN)) then
    p_sigacts.ps_flag:=p_sigacts.ps_flag or PS_CLDSIGIGN
   else
    p_sigacts.ps_flag:=p_sigacts.ps_flag and (not PS_CLDSIGIGN);
  end;

  if (p_sigacts.ps_sigact[_SIG_IDX(sig)]=sig_t(SIG_IGN)) or
      (((sigprop(sig) and SA_IGNORE)<>0) and
       (p_sigacts.ps_sigact[_SIG_IDX(sig)]=sig_t(SIG_DFL))) then
  begin
   sigqueue_delete_proc(sig);
   if (sig<>SIGCONT) then
   begin
    SIGADDSET(@p_sigacts.ps_sigignore,sig);
   end;
   SIGDELSET(@p_sigacts.ps_sigcatch,sig);
  end else
  begin
   SIGDELSET(@p_sigacts.ps_sigignore,sig);
   if (p_sigacts.ps_sigact[_SIG_IDX(sig)]=sig_t(SIG_DFL)) then
    SIGDELSET(@p_sigacts.ps_sigcatch,sig)
   else
    SIGADDSET(@p_sigacts.ps_sigcatch,sig);
  end;
 end;

 ps_mtx_unlock;
 PROC_UNLOCK;
 Result:=0;
end;

Function sys_sigaction(sig:Integer;
                       act,oact:p_sigaction_t;
                       flags:Integer):Integer;
var
 _act,_oact:sigaction_t;
 actp,oactp:p_sigaction_t;
begin
 actp :=nil;
 oactp:=nil;

 if (oact<>nil) then oactp:=@_oact;

 if (act<>nil) then
 begin
  actp:=@_act;
  Result:=copyin(act,actp,sizeof(sigaction_t));
  if (Result<>0) then Exit;
 end;

 Result:=kern_sigaction(sig,actp,oactp);

 if (oact<>nil) and (Result=0) then
 begin
  Result:=copyout(oactp,oact,sizeof(sigaction_t));
 end;
end;

procedure siginit;
var
 i:Integer;
begin
 mtx_init(@p_sigacts.ps_mtx);

 For i:=1 to NSIG do
 begin
  if ((sigprop(i) and SA_IGNORE)<>0) and (i<>SIGCONT) then
  begin
   SIGADDSET(@p_sigacts.ps_sigignore,i);
  end;
 end;

 sigqueue_init(@g_p_sigqueue);
end;

procedure reschedule_signals(block:sigset_t;flags:Integer); forward;

Function kern_sigprocmask(td:p_kthread;
                          how:Integer;
                          _set:p_sigset_t;
                          oset:p_sigset_t;
                          flags:Integer
                         ):Integer;
label
 _out;
var
 new_block,oset1:sigset_t;
begin
 Result:=0;

 if ((flags and SIGPROCMASK_PROC_LOCKED)=0) then
 begin
  PROC_LOCK;
 end;

 if (oset<>nil) then
 begin
  oset^:=td^.td_sigmask;
 end;

 if (_set<>nil) then
 begin

  Case how of
  SIG_BLOCK:
   begin
    SIG_CANTMASK(_set);
    oset1:=td^.td_sigmask;
    SIGSETOR(@td^.td_sigmask, _set);
    new_block:=td^.td_sigmask;
    SIGSETNAND(@new_block, @oset1);
   end;
   SIG_UNBLOCK:
   begin
    SIGSETNAND(@td^.td_sigmask, _set);
    signotify(td);
    goto _out;
   end;
   SIG_SETMASK:
   begin
    SIG_CANTMASK(_set);
    oset1:=td^.td_sigmask;
    if ((flags and SIGPROCMASK_OLD)<>0) then
     SIGSETLO(@td^.td_sigmask, _set)
    else
     td^.td_sigmask:=_set^;
    new_block:=td^.td_sigmask;
    SIGSETNAND(@new_block, @oset1);
    signotify(td);
   end;
   else
    begin
     Result:=EINVAL;
     goto _out;
    end;
   end;

  reschedule_signals(new_block, flags);
 end;

 _out:
  if ((flags and SIGPROCMASK_PROC_LOCKED)=0) then
  begin
   PROC_UNLOCK;
  end;
end;

Function sys_sigprocmask(how:Integer;
                         _set:p_sigset_t;
                         oset:p_sigset_t;
                         flags:Integer):Integer;
var
 td:p_kthread;

 __set,_oset:sigset_t;
 setp,osetp:p_sigset_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 setp :=nil;
 osetp:=nil;

 if (oset<>nil) then osetp:=@_oset;

 if (_set<>nil) then
 begin
  setp:=@__set;
  Result:=copyin(_set,setp,sizeof(sigset_t));
  if (Result<>0) then Exit;
 end;

 Result:=kern_sigprocmask(td,how,setp,osetp,0);

 if (oset<>nil) and (Result=0) then
 begin
  Result:=copyout(osetp,oset,sizeof(sigset_t));
 end;
end;

Function sys_sigpending(oset:p_sigset_t):Integer;
var
 td:p_kthread;

 pending:sigset_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 PROC_LOCK;
 pending:=g_p_sigqueue.sq_signals;
 SIGSETOR(@pending,@td^.td_sigqueue.sq_signals);
 PROC_UNLOCK;

 Result:=copyout(@pending,oset,sizeof(sigset_t));
end;

function ntw2px(n:Integer):Integer;
begin
 Case DWORD(n) of
  STATUS_SUCCESS         :Result:=0;
  STATUS_ABANDONED       :Result:=EPERM;
  STATUS_ALERTED         :Result:=EINTR;
  STATUS_USER_APC        :Result:=EINTR;
  STATUS_TIMEOUT         :Result:=ETIMEDOUT;
  STATUS_ACCESS_VIOLATION:Result:=EFAULT;
  else
                   Result:=EINVAL;
 end;
end;

function msleep(timo:Int64):Integer; inline;
begin
 sig_set_alterable;
 Result:=ntw2px(NtDelayExecution(True,@timo));
 sig_reset_alterable;
end;

Function kern_sigtimedwait(td:p_kthread;
                           waitset:sigset_t;
                           ksi:p_ksiginfo;
                           timeout:ptimespec
                          ):Integer;
var
 saved_mask,new_block:sigset_t;
 rts,ets,tv:Int64;
 sig,timevalid:Integer;
begin
 Result:=0;

 timevalid:=0;
 ets:=0;

 if (timeout<>nil) then
 begin
  if (timeout^.tv_nsec >= 0) and (timeout^.tv_nsec < 1000000000) then
  begin
   timevalid:=1;
   rts:=get_unit_uptime;
   ets:=rts;
   ets:=ets+TIMESPEC_TO_UNIT(timeout);
  end;
 end;

 ksiginfo_init(ksi);

 SIG_CANTMASK(@waitset);

 PROC_LOCK;
 saved_mask:=td^.td_sigmask;
 SIGSETNAND(@td^.td_sigmask,@waitset);

 repeat
  ps_mtx_lock;
  sig:=cursig(td,SIG_STOP_ALLOWED);
  ps_mtx_unlock;

  if (sig<>0) and SIGISMEMBER(@waitset,sig) then
  begin
   if (sigqueue_get(@td^.td_sigqueue,sig,ksi)<>0) or
      (sigqueue_get(@g_p_sigqueue,sig,ksi)<>0) then
   begin
    Result:=0;
    break;
   end;
  end;

  if (Result<>0) then Break;

  if (timeout<>nil) then
  begin
   if (timevalid=0) then
   begin
    Result:=EINVAL;
    break;
   end;
   rts:=get_unit_uptime;
   if (rts>=ets) then
   begin
    Result:=EAGAIN;
    break;
   end;
   tv:=ets-rts;
  end else
  begin
   tv:=NT_INFINITE;
  end;

  PROC_UNLOCK; //
  Result:=msleep(tvtohz(tv));
  PROC_LOCK;  //

  if (timeout<>nil) then
  begin
   if (Result=ERESTART) then
   begin
    Result:=EINTR;
   end else
   if (Result=EAGAIN) then
   begin
    Result:=0;
   end;
  end;

 until false;

 new_block:=saved_mask;
 SIGSETNAND(@new_block,@td^.td_sigmask);
 td^.td_sigmask:=saved_mask;

 reschedule_signals(new_block,0);

 //if (ksi^.ksi_code == SI_TIMER)
 // itimer_accept(p, ksi^.ksi_timerid, ksi);

 if (sig=SIGKILL) then
 begin
  sigexit(td,sig);
 end;

 PROC_UNLOCK;
end;

Function sys_sigwait(oset:p_sigset_t;sig:PInteger):Integer;
var
 td:p_kthread;
 ksi:ksiginfo_t;
 __set:sigset_t;
begin
 td:=curkthread;
 if (td=nil) then
 begin
  td^.td_retval[0]:=EFAULT;
  Exit(0);
 end;

 Result:=copyin(oset,@__set,sizeof(sigset_t));
 if (Result<>0) then
 begin
  td^.td_retval[0]:=EFAULT;
  Exit(0);
 end;

 Result:=kern_sigtimedwait(td,__set,@ksi,nil);
 if (Result<>0) then
 begin
  if (Result=EINTR) then
  begin
   Result:=ERESTART;
  end;
  td^.td_retval[0]:=Result;
  Exit(0);
 end;

 Result:=copyout(@ksi.ksi_info.si_signo,sig,sizeof(Integer));
 td^.td_retval[0]:=Result;
 Result:=0;
end;

Function sys_sigtimedwait(oset:p_sigset_t;info:p_siginfo_t;timeout:ptimespec):Integer;
var
 td:p_kthread;
 ts:timespec;
 ksi:ksiginfo_t;
 __set:sigset_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (timeout<>nil) then
 begin
  Result:=copyin(timeout,@ts,sizeof(timespec));
  if (Result<>0) then Exit;
  timeout:=@ts;
 end;

 Result:=copyin(oset,@__set,sizeof(sigset_t));
 if (Result<>0) then Exit;

 Result:=kern_sigtimedwait(td,__set,@ksi,timeout);
 if (Result<>0) then Exit;

 if (info<>nil) then
 begin
  Result:=copyout(@ksi.ksi_info,info,sizeof(siginfo_t));
  if (Result<>0) then Exit;
 end;

 td^.td_retval[0]:=ksi.ksi_info.si_signo;
end;

Function sys_sigwaitinfo(oset:p_sigset_t;info:p_siginfo_t):Integer;
var
 td:p_kthread;
 ksi:ksiginfo_t;
 __set:sigset_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 Result:=copyin(oset,@__set,sizeof(sigset_t));
 if (Result<>0) then Exit;

 Result:=kern_sigtimedwait(td,__set,@ksi,nil);
 if (Result<>0) then Exit;

 if (info<>nil) then
 begin
  Result:=copyout(@ksi.ksi_info,info,sizeof(siginfo_t));
  if (Result<>0) then Exit;
 end;

 td^.td_retval[0]:=ksi.ksi_info.si_signo;
end;

function postsig(sig:Integer):Integer; forward;

Function kern_sigsuspend(td:p_kthread;mask:sigset_t):Integer;
var
 has_sig,sig:Integer;
begin
 PROC_LOCK;
 kern_sigprocmask(td,SIG_SETMASK,@mask,@td^.td_oldsigmask,SIGPROCMASK_PROC_LOCKED);
 td^.td_pflags:=td^.td_pflags or TDP_OLDMASK;

 cpu_set_syscall_retval(td,EINTR);

 has_sig:=0;
 While (has_sig=0) do
 begin
  PROC_UNLOCK; //
  while (msleep(T_INFINITE)=0) do;
  PROC_LOCK;   //

  //thread_suspend_check(0);

  ps_mtx_lock;

  repeat
   sig:=cursig(td,SIG_STOP_ALLOWED);
   if (sig=0) then Break;
   has_sig:=has_sig+postsig(sig);
  until false;

  ps_mtx_unlock;
 end;

 PROC_UNLOCK;
 td^.td_errno:=EINTR;
 td^.td_pflags:=td^.td_pflags or TDP_NERRNO;
 Result:=EJUSTRETURN;
end;

Function sys_sigsuspend(sigmask:p_sigset_t):Integer;
var
 td:p_kthread;
 mask:sigset_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 Result:=copyin(sigmask,@mask,sizeof(sigset_t));
 if (Result<>0) then Exit;

 Result:=kern_sigsuspend(td,mask);
end;

Function kern_sigaltstack(td:p_kthread;ss:p_stack_t;oss:p_stack_t):Integer;
var
 oonstack:Integer;
begin
 oonstack:=sigonstack(cpu_getstack(td));

 if (oss<>nil) then
 begin
  oss^:=td^.td_sigstk;

  if ((td^.td_pflags and TDP_ALTSTACK)<>0) then
  begin
   if (oonstack<>0) then
   begin
    oss^.ss_flags:=SS_ONSTACK;
   end else
   begin
    oss^.ss_flags:=0;
   end;
  end else
  begin
   oss^.ss_flags:=SS_DISABLE;
  end;
 end;

 if (ss<>nil) then
 begin
  if (oonstack<>0) then Exit(EPERM);

  if ((ss^.ss_flags and (not SS_DISABLE))<>0) then Exit(EINVAL);

  if ((ss^.ss_flags and SS_DISABLE)=0) then
  begin
   if (ss^.ss_size<MINSIGSTKSZ) then Exit(ENOMEM);

   td^.td_sigstk:=ss^;
   td^.td_pflags:=td^.td_pflags or TDP_ALTSTACK;
  end else
  begin
   td^.td_pflags:=td^.td_pflags and (not TDP_ALTSTACK);
  end;
 end;

 Result:=0;
end;

Function sys_sigaltstack(ss:p_stack_t;oss:p_stack_t):Integer;
var
 td:p_kthread;
 _ss,_oss:stack_t;
 p_oss:p_stack_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (ss<>nil) then
 begin
  Result:=copyin(ss,@_ss,sizeof(stack_t));
  if (Result<>0) then Exit;
  ss:=@_ss;
 end;

 p_oss:=nil;
 if (oss<>nil) then
 begin
  p_oss:=@_oss;
 end;

 Result:=kern_sigaltstack(td,ss,p_oss);
 if (Result<>0) then Exit;

 if (oss<>nil) then
 begin
  Result:=copyout(p_oss,oss,sizeof(stack_t));
 end;
end;

procedure postsig_done(sig:Integer;td:p_kthread);
var
 mask:sigset_t;
begin
 //td->td_ru.ru_nsignals++;

 mask:=p_sigacts.ps_catchmask[_SIG_IDX(sig)];

 if (not SIGISMEMBER(@p_sigacts.ps_signodefer,sig)) then
 begin
  SIGADDSET(@mask,sig);
 end;

 kern_sigprocmask(td,SIG_BLOCK,@mask,nil,SIGPROCMASK_PROC_LOCKED or SIGPROCMASK_PS_LOCKED);

 if (SIGISMEMBER(@p_sigacts.ps_sigreset,sig)) then
 begin
  SIGDELSET(@p_sigacts.ps_sigcatch,sig);
  if (sig<>SIGCONT) and
     ((sigprop(sig) and SA_IGNORE)<>0) then
  begin
   SIGADDSET(@p_sigacts.ps_sigignore,sig);
  end;
  p_sigacts.ps_sigact[_SIG_IDX(sig)]:=sig_t(SIG_DFL);
 end;
end;

function tdsendsignal(td:p_kthread;sig:Integer;ksi:p_ksiginfo):Integer; forward;

procedure trapsignal(td:p_kthread;ksi:p_ksiginfo);
var
 sig,code:Integer;
begin
 sig :=ksi^.ksi_info.si_signo;
 code:=ksi^.ksi_info.si_code;

 Assert(_SIG_VALID(sig),'invalid signal');

 PROC_LOCK;
 ps_mtx_lock;

 if SIGISMEMBER(@p_sigacts.ps_sigcatch,sig) and
    (not SIGISMEMBER(@td^.td_sigmask,sig)) then
 begin
  sendsig(p_sigacts.ps_sigact[_SIG_IDX(sig)],ksi,@td^.td_sigmask);
  postsig_done(sig,td);
  ps_mtx_unlock;
 end else
 begin
  //ignoring?

  if (kern_forcesigexit<>0) and
     (SIGISMEMBER(@td^.td_sigmask,sig) or
      (p_sigacts.ps_sigact[_SIG_IDX(sig)]=sig_t(SIG_IGN))) then
  begin
   SIGDELSET(@td^.td_sigmask,sig);
   SIGDELSET(@p_sigacts.ps_sigcatch,sig);
   SIGDELSET(@p_sigacts.ps_sigignore,sig);
   p_sigacts.ps_sigact[_SIG_IDX(sig)]:=sig_t(SIG_DFL);
  end;

  ps_mtx_unlock;

  tdsendsignal(td,sig,ksi);
 end;

 PROC_UNLOCK;
end;

type
 p_t_sigtd=^_t_sigtd;
 _t_sigtd=record
   first_td:p_kthread;
  signal_td:p_kthread;
        sig:Integer
 end;

procedure _for_sigtd(td:p_kthread;data:p_t_sigtd); register; //Tfree_data_cb
begin
 if (td=nil) or (data=nil) then Exit;

 if (data^.first_td=nil) then
 begin
  data^.first_td:=td;
 end;

 if (data^.signal_td=nil) then
 if (not SIGISMEMBER(@td^.td_sigmask,data^.sig)) then
 begin
  data^.signal_td:=td;
 end;
end;

function sigtd(sig,prop:Integer):p_kthread;
var
 td:p_kthread;
 data:_t_sigtd;
begin
 td:=curkthread;

 if (td<>nil) then
 if not SIGISMEMBER(@td^.td_sigmask,sig) then
 begin
  Exit(td);
 end;

 data:=Default(_t_sigtd);
 data.sig:=sig;

 FOREACH_THREAD_IN_PROC(Pointer(@_for_sigtd),@data);

 if (data.signal_td=nil) then
 begin
  Result:=data.first_td;
 end else
 begin
  Result:=data.signal_td;
 end;
end;

procedure kern_psignal(sig:Integer);
var
 ksi:ksiginfo_t;
begin
 ksiginfo_init(@ksi);
 ksi.ksi_info.si_signo:=sig;
 ksi.ksi_info.si_code :=SI_KERNEL;
 tdsendsignal(nil,sig,@ksi);
end;

function pksignal(sig:Integer;ksi:p_ksiginfo):Integer;
begin
 Result:=tdsendsignal(nil,sig,ksi);
end;

procedure tdsigwakeup(td:p_kthread;sig:Integer;action:sig_t;intrval:Integer); forward;

function tdsendsignal(td:p_kthread;sig:Integer;ksi:p_ksiginfo):Integer;
var
 sigqueue:p_sigqueue;
 action:sig_t;
 intrval:Integer;
 prop:Integer;
begin
 Result:=0;

 prop:=sigprop(sig);

 if (td=nil) then
 begin
  td:=sigtd(sig,prop);
  sigqueue:=@g_p_sigqueue;
 end else
 begin
  sigqueue:=@td^.td_sigqueue;
 end;

 ps_mtx_lock;

 if (SIGISMEMBER(@p_sigacts.ps_sigignore,sig)) then
 begin
  ps_mtx_unlock;

  if (ksi<>nil) then
  if (ksi^.ksi_flags and KSI_INS)<>0 then
  begin
   ksiginfo_tryfree(ksi);
  end;

  Exit;
 end;

 if (SIGISMEMBER(@td^.td_sigmask,sig)) then
  action:=sig_t(SIG_HOLD)
 else if (SIGISMEMBER(@p_sigacts.ps_sigcatch,sig)) then
  action:=sig_t(SIG_CATCH)
 else
  action:=sig_t(SIG_DFL);

 if (SIGISMEMBER(@p_sigacts.ps_sigintr,sig)) then
  intrval:=EINTR
 else
  intrval:=ERESTART;

 ps_mtx_unlock;

 if ((prop and SA_CONT)<>0) then
 begin
  sigqueue_delete_stopmask_proc
 end else
 if ((prop and SA_STOP)<>0) then
 begin
  sigqueue_delete_proc(SIGCONT);
 end;

 Result:=sigqueue_add(sigqueue,sig,ksi);
 if (Result<>0) then Exit;
 signotify(td);

 if (action=sig_t(SIG_HOLD)) and ((prop and SA_CONT)=0) then Exit;

 tdsigwakeup(td,sig,action,intrval);
end;

procedure tdsignal(td:p_kthread;sig:Integer);
var
 ksi:ksiginfo_t;
begin
 ksiginfo_init(@ksi);
 ksi.ksi_info.si_signo:=sig;
 ksi.ksi_info.si_code :=SI_KERNEL;
 tdsendsignal(td,sig,@ksi);
end;

procedure tdksignal(td:p_kthread;sig:Integer;ksi:p_ksiginfo);
begin
 tdsendsignal(td,sig,ksi);
end;

procedure forward_signal(td:p_kthread);
begin
 if (td=curkthread) then Exit;
 ipi_send_cpu(td);
end;

procedure tdsigwakeup(td:p_kthread;sig:Integer;action:sig_t;intrval:Integer);
label
 _out;
var
 prop:Integer;
 wakeup_swapper:Integer;
begin
 wakeup_swapper:=0;
 prop:=sigprop(sig);

 PROC_LOCK;
 thread_lock(td);

 if (action=sig_t(SIG_DFL)) and ((prop and SA_KILL)<>0) and (td^.td_priority>PUSER) then
 begin
  sched_prio(td,PUSER);
 end;

 //TD_ON_SLEEPQ(td) ((td)->td_wchan != NULL)

 if false{TD_ON_SLEEPQ(td)}  then
 begin

  if ((td^.td_flags and TDF_SINTR)=0) then
  begin
   goto _out;
  end;

  if ((prop and SA_CONT)<>0) and (action=sig_t(SIG_DFL)) then
  begin
   thread_unlock(td);
   PROC_UNLOCK;
   sigqueue_delete(@g_p_sigqueue,sig);
   sigqueue_delete(@td^.td_sigqueue,sig);
   Exit;
  end;

  if ((prop and SA_STOP)<>0) and ((td^.td_flags and TDF_SBDRY)<>0) then
  begin
   goto _out;
  end;

  if (td^.td_priority>PUSER) then
  begin
   sched_prio(td,PUSER);
  end;

  //wakeup_swapper:=sleepq_abort(td,intrval);
 end else
 begin
  if TD_IS_RUNNING(td) and (td<>curkthread) then
  begin
   forward_signal(td);
  end;
 end;

 _out:
  PROC_UNLOCK;
  thread_unlock(td);
  //if (wakeup_swapper)
  // kick_proc0();
end;

procedure reschedule_signals(block:sigset_t;flags:Integer);
var
 td:p_kthread;
 sig:Integer;
begin
 if (SIGISEMPTY(@g_p_sigqueue.sq_signals)) then Exit;

 SIGSETAND(@block,@g_p_sigqueue.sq_signals);

 repeat
  sig:=sig_ffs(@block);
  if (sig=0) then Exit;

  SIGDELSET(@block,sig);
  td:=sigtd(sig,0);
  signotify(td);

  if ((flags and SIGPROCMASK_PS_LOCKED)=0) then
  begin
   ps_mtx_lock;
  end;

  if SIGISMEMBER(@p_sigacts.ps_sigcatch,sig) then
  begin
   if SIGISMEMBER(@p_sigacts.ps_sigintr,sig) then
   begin
    tdsigwakeup(td,sig,sig_t(SIG_CATCH),EINTR);
   end else
   begin
    tdsigwakeup(td,sig,sig_t(SIG_CATCH),ERESTART);
   end;
  end;

  if ((flags and SIGPROCMASK_PS_LOCKED)=0) then
  begin
   ps_mtx_unlock;
  end;
 until false;
end;

procedure tdsigcleanup(td:p_kthread);
var
 unblocked:sigset_t;
begin
 sigqueue_flush(@td^.td_sigqueue);

 SIGFILLSET(@unblocked);
 SIGSETNAND(@unblocked,@td^.td_sigmask);
 SIGFILLSET(@td^.td_sigmask);
 reschedule_signals(unblocked,0);
end;

function sigdeferstop:Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td^.td_flags and TDF_SBDRY)<>0 then Exit(0);
 thread_lock(td);
 td^.td_flags:=td^.td_flags or TDF_SBDRY;
 thread_unlock(td);
 Result:=1;
end;

procedure sigallowstop;
var
 td:p_kthread;
begin
 td:=curkthread;
 thread_lock(td);
 td^.td_flags:=td^.td_flags and (not TDF_SBDRY);
 thread_unlock(td);
end;

Function issignal(td:p_kthread;stop_allowed:Integer):Integer;
var
 sigpending:sigset_t;
 sig,prop:Integer;
begin
 repeat

  sigpending:=td^.td_sigqueue.sq_signals;
  SIGSETOR(@sigpending,@g_p_sigqueue.sq_signals);
  SIGSETNAND(@sigpending,@td^.td_sigmask);

  if (td^.td_flags and TDF_SBDRY)<>0 then
  begin
   SIG_STOPSIGMASK(@sigpending);
  end;
  if (SIGISEMPTY(@sigpending)) then
  begin
   Exit(0);
  end;
  sig:=sig_ffs(@sigpending);

  if SIGISMEMBER(@p_sigacts.ps_sigignore,sig) then
  begin
   sigqueue_delete(@td^.td_sigqueue,sig);
   sigqueue_delete(@g_p_sigqueue,sig);
   continue;
  end;

  prop:=sigprop(sig);

  Case ptrint(p_sigacts.ps_sigact[_SIG_IDX(sig)]) of

   SIG_DFL:
    begin
     if ((prop and SA_STOP)<>0) then
     begin
      ps_mtx_unlock;
      PROC_LOCK;
      //sig_suspend_threads(td, p, 0);
      //thread_suspend_switch(td);
      PROC_UNLOCK;
      ps_mtx_lock;
     end else
     if ((prop and SA_IGNORE)<>0) then
     begin
      //ignore
     end else
     begin
      Exit(sig);
     end;
    end;

   SIG_IGN:; //ignore

   else
    Exit(sig);

  end;

  sigqueue_delete(@td^.td_sigqueue,sig);
  sigqueue_delete(@g_p_sigqueue,sig);
 until false;
end;

function postsig(sig:Integer):Integer;
var
 td:p_kthread;
 action:sig_t;
 ksi:ksiginfo_t;
 returnmask:sigset_t;
begin
 td:=curkthread;

 Assert(sig<>0,'postsig');

 ksiginfo_init(@ksi);

 if (sigqueue_get(@td^.td_sigqueue,sig,@ksi)=0) and
    (sigqueue_get(@g_p_sigqueue,sig,@ksi)=0) then
 begin
  Exit(0);
 end;

 ksi.ksi_info.si_signo:=sig;

 //if (ksi.ksi_code=SI_TIMER)
 // itimer_accept(p, ksi.ksi_timerid, &ksi);

 action:=p_sigacts.ps_sigact[_SIG_IDX(sig)];

 if (action=sig_t(SIG_DFL)) then
 begin
  ps_mtx_unlock;
  sigexit(td,sig);
  // NOTREACHED
 end else
 begin
  Assert((action<>sig_t(SIG_IGN)) and (not SIGISMEMBER(@td^.td_sigmask,sig)),'postsig action');

  if ((td^.td_pflags and TDP_OLDMASK)<>0) then
  begin
   returnmask:=td^.td_oldsigmask;
   td^.td_pflags:=td^.td_pflags and (not TDP_OLDMASK);
  end else
  begin
   returnmask:=td^.td_sigmask;
  end;

  sendsig(action,@ksi,@returnmask);
  postsig_done(sig,td);
 end;
 Result:=1;
end;

function W_EXITCODE(ret,sig:Integer):Integer; inline;
begin
 Result:=(ret shl 8) or sig;
end;

procedure sigexit(td:p_kthread;sig:Integer);
begin
 Halt(W_EXITCODE(0,sig));
 // NOTREACHED
end;

//

procedure ast;
var
 td:p_kthread;
 flags,sig:Integer;
begin
 td:=curkthread;

 thread_lock(td);
 flags:=td^.td_flags;

 td^.td_flags:=td^.td_flags and (not (TDF_ASTPENDING or TDF_NEEDSIGCHK or TDF_NEEDSUSPCHK or
     TDF_NEEDRESCHED or TDF_ALRMPEND or TDF_PROFPEND or TDF_MACPEND));

 thread_unlock(td);

 NtTestAlert();

 if ((flags and TDF_ALRMPEND)<>0) then
 begin
  PROC_LOCK;
  kern_psignal(SIGVTALRM);
  PROC_UNLOCK;
 end;

 if ((flags and TDF_PROFPEND)<>0) then
 begin
  PROC_LOCK;
  kern_psignal(SIGPROF);
  PROC_UNLOCK;
 end;

 if ((flags and TDF_NEEDRESCHED)<>0) then
 begin
  thread_lock(td);
  sched_prio(td,td^.td_user_pri);
  //mi_switch(SW_INVOL or SWT_NEEDRESCHED, NULL);
  thread_unlock(td);
 end;

 if ((flags and TDF_NEEDSIGCHK)<>0) or
    (p_pendingcnt > 0) or
    (not SIGISEMPTY(@g_p_sigqueue.sq_list)) then
 begin
  PROC_LOCK;
  ps_mtx_lock;

  repeat
   sig:=cursig(td,SIG_STOP_ALLOWED);
   if (sig=0) then Exit;
   postsig(sig);
  until false;

  ps_mtx_unlock;
  PROC_UNLOCK;
 end;

 if ((flags and TDF_NEEDSUSPCHK)<>0) then
 begin
  PROC_LOCK;
  //thread_suspend_check(0);
  PROC_UNLOCK;
 end;

 if ((td^.td_pflags and TDP_OLDMASK)<>0) then
 begin
  td^.td_pflags:=td^.td_pflags and (not TDP_OLDMASK);
  kern_sigprocmask(td,SIG_SETMASK,@td^.td_oldsigmask,nil,0);
 end;
end;

initialization
 siginit;

end.

