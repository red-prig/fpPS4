unit kern_sig;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 gtailq,
 sys_kernel,
 time,
 signal,
 signalvar;

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

Function sys_sigaction(sig:Integer;
                       act,oact:p_sigaction_t;
                       flags:Integer):Integer;

Function sys_sigprocmask(how:Integer;
                         _set:p_sigset_t;
                         oset:p_sigset_t;
                         flags:Integer):Integer;

Function sys_sigpending(oset:p_sigset_t):Integer;

Function sys_sigwait(oset:p_sigset_t;sig:PInteger):Integer;
Function sys_sigtimedwait(oset:p_sigset_t;info:p_siginfo_t;timeout:ptimespec):Integer;
Function sys_sigwaitinfo(oset:p_sigset_t;info:p_siginfo_t):Integer;

Function sys_sigsuspend(sigmask:p_sigset_t):Integer;

Function sys_sigaltstack(ss:p_stack_t;oss:p_stack_t):Integer;

implementation

uses
 ntapi,
 systm,
 _umtx,
 kern_umtx,
 kern_time,
 kern_thread,
 vm_machdep,
 machdep;

const
 max_pending_per_proc=128;

 kern_forcesigexit=1;

var
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
 ksi:p_ksiginfo;
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
  ksi:=TAILQ_NEXT(ksi,@ksi^.ksi_link);
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
 ksi:p_ksiginfo;
begin
 Assert((sq^.sq_flags and SQ_INIT)<>0,'sigqueue not inited');

 ksi:=TAILQ_FIRST(@sq^.sq_list);
 while (ksi<>nil) do
 begin
  TAILQ_REMOVE(@sq^.sq_list,ksi,@ksi^.ksi_link);
  ksi^.ksi_sigq:=nil;
  if ksiginfo_tryfree(ksi) then
  begin
   Dec(p_pendingcnt);
  end;
  ksi:=TAILQ_NEXT(ksi,@ksi^.ksi_link);
 end;

 SIGEMPTYSET(@sq^.sq_signals);
 SIGEMPTYSET(@sq^.sq_kill);
end;

procedure sigqueue_move_set(src,dst:p_sigqueue;_set:p_sigset_t);
var
 tmp:sigset_t;
 ksi:p_ksiginfo;
begin
 Assert((src^.sq_flags and SQ_INIT)<>0,'sigqueue not inited');
 Assert((dst^.sq_flags and SQ_INIT)<>0,'sigqueue not inited');

 ksi:=TAILQ_FIRST(@src^.sq_list);
 while (ksi<>nil) do
 begin
  if SIGISMEMBER(_set,ksi^.ksi_info.si_signo) then
  begin
   TAILQ_REMOVE(@src^.sq_list,ksi,@ksi^.ksi_link);
   TAILQ_INSERT_TAIL(@dst^.sq_list,ksi,@ksi^.ksi_link);
   ksi^.ksi_sigq:=dst;
  end;

  ksi:=TAILQ_NEXT(ksi,@ksi^.ksi_link);
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
 ksi:p_ksiginfo;
begin
 Assert((sq^.sq_flags and SQ_INIT)<>0,'sigqueue not inited');

 ksi:=TAILQ_FIRST(@sq^.sq_list);
 while (ksi<>nil) do
 begin
  if SIGISMEMBER(_set,ksi^.ksi_info.si_signo) then
  begin
   TAILQ_REMOVE(@sq^.sq_list,ksi,@ksi^.ksi_link);
   ksi^.ksi_sigq:=nil;
   if ksiginfo_tryfree(ksi) then
   begin
    Dec(p_pendingcnt)
   end;
  end;

  ksi:=TAILQ_NEXT(ksi,@ksi^.ksi_link);
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
  Result:=ord((sp - size_t(td^.td_sigstk.ss_sp)) < td^.td_sigstk.ss_size);
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

function mtx_lock(m:p_umtx):Integer; inline;
begin
 Result:=_sys_umtx_lock(m);
end;

function mtx_unlock(m:p_umtx):Integer; inline;
begin
 Result:=_sys_umtx_unlock(m);
end;

Function kern_sigaction(sig:Integer;
                        act,oact:p_sigaction_t):Integer;
begin
 if (not _SIG_VALID(sig)) then Exit(EINVAL);

 PROC_LOCK;
 mtx_lock(@p_sigacts.ps_mtx);

 if (oact<>nil) then
 begin
  oact^.sa_mask := p_sigacts.ps_catchmask[_SIG_IDX(sig)];
  oact^.sa_flags := 0;
  if (SIGISMEMBER(@p_sigacts.ps_sigonstack, sig)) then
   oact^.sa_flags := oact^.sa_flags or SA_ONSTACK;
  if (not SIGISMEMBER(@p_sigacts.ps_sigintr, sig)) then
   oact^.sa_flags := oact^.sa_flags or SA_RESTART;
  if (SIGISMEMBER(@p_sigacts.ps_sigreset, sig)) then
   oact^.sa_flags := oact^.sa_flags or SA_RESETHAND;
  if (SIGISMEMBER(@p_sigacts.ps_signodefer, sig)) then
   oact^.sa_flags := oact^.sa_flags or SA_NODEFER;
  if (SIGISMEMBER(@p_sigacts.ps_siginfo, sig)) then
  begin
   oact^.sa_flags := oact^.sa_flags or SA_SIGINFO;
   oact^.u.sa_handler := p_sigacts.ps_sigact[_SIG_IDX(sig)];
  end else
  begin
   oact^.u.sa_handler := p_sigacts.ps_sigact[_SIG_IDX(sig)];
  end;
  if (sig = SIGCHLD and p_sigacts.ps_flag and PS_NOCLDSTOP) then
   oact^.sa_flags := oact^.sa_flags or SA_NOCLDSTOP;
  if (sig = SIGCHLD and p_sigacts.ps_flag and PS_NOCLDWAIT) then
   oact^.sa_flags := oact^.sa_flags or SA_NOCLDWAIT;
 end;
 if (act<>nil) then
 begin
  if ((sig = SIGKILL) or (sig = SIGSTOP)) and
      (act^.u.code <> SIG_DFL) then
  begin
   mtx_unlock(@p_sigacts.ps_mtx);
   PROC_UNLOCK;
   Result:=EINVAL;
  end;

  p_sigacts.ps_catchmask[_SIG_IDX(sig)] := act^.sa_mask;
  SIG_CANTMASK(@p_sigacts.ps_catchmask[_SIG_IDX(sig)]);
  if ((act^.sa_flags and SA_SIGINFO)<>0) then
  begin
   p_sigacts.ps_sigact[_SIG_IDX(sig)] := act^.u.sa_handler;
   SIGADDSET(@p_sigacts.ps_siginfo, sig);
  end else
  begin
   p_sigacts.ps_sigact[_SIG_IDX(sig)] := act^.u.sa_handler;
   SIGDELSET(@p_sigacts.ps_siginfo, sig);
  end;
  if ((act^.sa_flags and SA_RESTART)=0) then
   SIGADDSET(@p_sigacts.ps_sigintr, sig)
  else
   SIGDELSET(@p_sigacts.ps_sigintr, sig);
  if ((act^.sa_flags and SA_ONSTACK)<>0) then
   SIGADDSET(@p_sigacts.ps_sigonstack, sig)
  else
   SIGDELSET(@p_sigacts.ps_sigonstack, sig);
  if ((act^.sa_flags and SA_RESETHAND)<>0) then
   SIGADDSET(@p_sigacts.ps_sigreset, sig)
  else
   SIGDELSET(@p_sigacts.ps_sigreset, sig);
  if ((act^.sa_flags and SA_NODEFER)<>0) then
   SIGADDSET(@p_sigacts.ps_signodefer, sig)
  else
   SIGDELSET(@p_sigacts.ps_signodefer, sig);
  if (sig = SIGCHLD) then
  begin
   if ((act^.sa_flags and SA_NOCLDSTOP)<>0) then
    p_sigacts.ps_flag := p_sigacts.ps_flag or PS_NOCLDSTOP
   else
    p_sigacts.ps_flag := p_sigacts.ps_flag and (not PS_NOCLDSTOP);
   if ((act^.sa_flags and SA_NOCLDWAIT)<>0) then
   begin
    p_sigacts.ps_flag := p_sigacts.ps_flag or PS_NOCLDWAIT;
   end else
    p_sigacts.ps_flag := p_sigacts.ps_flag and (not PS_NOCLDWAIT);
   if (p_sigacts.ps_sigact[_SIG_IDX(SIGCHLD)] = sig_t(SIG_IGN)) then
    p_sigacts.ps_flag := p_sigacts.ps_flag or PS_CLDSIGIGN
   else
    p_sigacts.ps_flag := p_sigacts.ps_flag and (not PS_CLDSIGIGN);
  end;

  if (p_sigacts.ps_sigact[_SIG_IDX(sig)] = sig_t(SIG_IGN)) or
      (((sigprop(sig) and SA_IGNORE)<>0) and
       (p_sigacts.ps_sigact[_SIG_IDX(sig)] = sig_t(SIG_DFL))) then
  begin
   //sigqueue_delete_proc(p, sig);
   if (sig <> SIGCONT) then
   begin
    SIGADDSET(@p_sigacts.ps_sigignore, sig);
   end;
   SIGDELSET(@p_sigacts.ps_sigcatch, sig);
  end else
  begin
   SIGDELSET(@p_sigacts.ps_sigignore, sig);
   if (p_sigacts.ps_sigact[_SIG_IDX(sig)] = sig_t(SIG_DFL)) then
    SIGDELSET(@p_sigacts.ps_sigcatch, sig)
   else
    SIGADDSET(@p_sigacts.ps_sigcatch, sig);
  end;
 end;

 mtx_unlock(@p_sigacts.ps_mtx);
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
 PROC_LOCK;

 For i:=1 to NSIG do
 begin
  if ((sigprop(i) and SA_IGNORE)<>0) and (i <> SIGCONT) then
  begin
   SIGADDSET(@p_sigacts.ps_sigignore, i);
  end;
 end;

 PROC_UNLOCK;
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

 if (_set <> nil) then
 begin

  Case how of
  SIG_BLOCK:
   begin
    SIG_CANTMASK(_set);
    oset1 := td^.td_sigmask;
    SIGSETOR(@td^.td_sigmask, _set);
    new_block := td^.td_sigmask;
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
    oset1 := td^.td_sigmask;
    if ((flags and SIGPROCMASK_OLD)<>0) then
     SIGSETLO(@td^.td_sigmask, _set)
    else
     td^.td_sigmask := _set^;
    new_block := td^.td_sigmask;
    SIGSETNAND(@new_block, @oset1);
    signotify(td);
   end;
   else
    begin
     Result := EINVAL;
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
 pending:=td^.td_sigqueue.sq_signals;
 PROC_UNLOCK;

 Result:=copyout(@pending,oset,sizeof(sigset_t));
end;

procedure sigexit(td:p_kthread;sig:Integer); forward;

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
  mtx_lock(@p_sigacts.ps_mtx);
  sig:=cursig(td,SIG_STOP_ALLOWED);
  mtx_unlock(@p_sigacts.ps_mtx);

  if (sig<>0) and SIGISMEMBER(@waitset,sig) then
  begin
   if (sigqueue_get(@td^.td_sigqueue,sig,ksi)<>0) then
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

  //Result:=msleep(ps, &p^.p_mtx, PPAUSE|PCATCH, "sigwait", -tv);

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
  //while (msleep(&p->p_sigacts, &p->p_mtx, PPAUSE|PCATCH, "pause",0) == 0)

  //thread_suspend_check(0);

  mtx_lock(@p_sigacts.ps_mtx);

  repeat
   sig:=cursig(td,SIG_STOP_ALLOWED);
   if (sig=0) then Break;
   has_sig:=has_sig+postsig(sig);
  until false;

  mtx_unlock(@p_sigacts.ps_mtx);
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

 if (oss <> nil) then
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

procedure trapsignal(td:p_kthread;ksi:p_ksiginfo);
var
 sig,code:Integer;
begin
 sig :=ksi^.ksi_info.si_signo;
 code:=ksi^.ksi_info.si_code;

 Assert(_SIG_VALID(sig),'invalid signal');

 PROC_LOCK;
 mtx_lock(@p_sigacts.ps_mtx);

 if SIGISMEMBER(@p_sigacts.ps_sigcatch,sig) and
    (not SIGISMEMBER(@td^.td_sigmask,sig)) then
 begin
  sendsig(p_sigacts.ps_sigact[_SIG_IDX(sig)],ksi,@td^.td_sigmask);
  postsig_done(sig,td);
  mtx_unlock(@p_sigacts.ps_mtx);
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

  mtx_unlock(@p_sigacts.ps_mtx);

  //tdsendsignal(p, td, sig, ksi);
 end;

 PROC_UNLOCK;
end;

function postsig(sig:Integer):Integer;
begin
 //TODO
end;

procedure sigexit(td:p_kthread;sig:Integer);
begin
 //TODO
end;

Function issignal(td:p_kthread;stop_allowed:Integer):Integer;
begin
 //TODO
end;

procedure reschedule_signals(block:sigset_t;flags:Integer);
begin
 //TODO
end;

initialization
 siginit;

end.

