unit kern_sig;

{$mode ObjFPC}{$H+}

interface

uses
 gtailq,
 sys_kernel,
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


implementation

const
 max_pending_per_proc=128;

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




end.

