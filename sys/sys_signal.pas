unit sys_signal;

{$mode ObjFPC}{$H+}

interface

uses
 Windows,
 sys_types,
 atomic,
 LFQueue,
 sys_context;

{$I signal.inc}

type
 p_ksiginfo_t=^ksiginfo_t;
 ksiginfo_t=packed record
  pNext:p_ksiginfo_t;
  //
  ksi_info:siginfo_t;
 end;

 ksiginfo_list=object
  pHead,pTail:p_ksiginfo_t;
  procedure Insert(Node:p_ksiginfo_t);
  procedure Remove(pPrev,node:p_ksiginfo_t);
 end;

 p_sigqueue_t=^sigqueue_t;
 sigqueue_t=packed record
  _mask:sigset_t;

  _lock:Integer; //signal blocking
  _flag:DWORD;   //signal flags

  _queue_lock:DWORD;
  _event_lock:DWORD; //context send lock
  _event:DWORD;      //context event

  _signals:sigset_t;

  _queue:ksiginfo_list;

  _contexts:TIntrusiveMPSCQueue;
 end;

 p_ksig_context=^ksig_context;
 ksig_context=packed record
  next:p_ksig_context;
  ectx:TCONTEXT_EXTENDED;
  _rsp:QWORD;
 end;

Const
 SL_ALERTABLE=1;
 SL_NOINTRRUP=2;

function _SIG_IDX(sig:Integer):DWORD;        inline;
function _SIG_VALID(sig:Integer):Boolean;    inline;
function _SIG_VALID_32(sig:Integer):Boolean; inline;

Procedure sigqueue_init(sq:p_sigqueue_t);

function  _sig_pending(sq:p_sigqueue_t):DWORD; inline;
function  _sig_is_pending(sq:p_sigqueue_t):Boolean; inline;

function  _sigaddset(_set:p_sigset_t;signum:Integer):Integer;
function  _sigdelset(_set:p_sigset_t;signum:Integer):Integer;
function  _sigismember(_set:p_sigset_t;signum:Integer):Integer;
function  __sigprocmask(how:Integer;_set,oldset:p_sigset_t):Integer;

function  __sigaction(signum:Integer;act,oldact:p_sigaction_t):Integer;

procedure _sig_lock(flags:integer=0);
procedure __sig_lock(pt:Pointer;flags:integer=0);
procedure _sig_unlock(flags:integer=0);
procedure __sig_unlock(pt:Pointer;flags:integer=0);

function  _pthread_kill(t:Pointer;sig:Integer):Integer;

procedure _copy_ctx_from_sys(src:PCONTEXT;dst:p_ucontext_t);
procedure _copy_ctx_to_sys(src:p_ucontext_t;dst:PCONTEXT);

implementation

uses
 ntapi,
 spinlock,
 sys_kernel,
 sys_pthread;

Var
 ps_sigact:array[0..31] of sigaction_t;

 pf_deadlock:QWORD;


function _SIG_IDX(sig:Integer):DWORD; inline;
begin
 Result:=sig-1;
end;

function _SIG_VALID(sig:Integer):Boolean; inline;
begin
 Result:=(sig<=_SIG_MAXSIG) and (sig>0);
end;

function _SIG_VALID_32(sig:Integer):Boolean; inline;
begin
 Result:=(sig<=32) and (sig>0);
end;

procedure ksiginfo_list.Insert(Node:p_ksiginfo_t);
begin
 if (pTail=nil) then
 begin
  pHead:=node;
  node^.pNext:=nil;
 end else
 begin
  pTail^.pNext:=node;
 end;
 pTail:=node;
end;

procedure ksiginfo_list.Remove(pPrev,node:p_ksiginfo_t);
begin
 if (pPrev=nil) then
 begin
  if (pHead=node) then
  begin
   pHead:=node^.pNext;
  end;
 end else
 begin
  pPrev^.pNext:=node^.pNext;
 end;
 if (node^.pNext=nil) then
 begin
  if (pTail=node) then
  begin
   pTail:=pPrev;
  end;
 end;
end;

Procedure sigqueue_init(sq:p_sigqueue_t);
var
 i:Byte;
begin
 if (sq=nil) then Exit;
 sq^:=Default(sigqueue_t);
 sq^._contexts.Create;
end;

function sigqueue_add(sq:p_sigqueue_t;si:p_siginfo_t):Integer;
var
 node:p_ksiginfo_t;
begin
 Result:=0;
 if (sq=nil) or (si=nil) then Exit;

 if (si^.si_signo=SIGKILL) or
    (si^.si_signo=SIGSTOP) then
 begin
  _sigaddset(@sq^._signals,si^.si_signo);
  Exit;
 end;

 node:=AllocMem(SizeOf(ksiginfo_t));
 if (node=nil) then
 begin
  _sigaddset(@sq^._signals,si^.si_signo);
  Exit(EAGAIN);
 end;

 node^.ksi_info:=si^;

 spin_lock(sq^._queue_lock);

   sq^._queue.Insert(node);
   _sigaddset(@sq^._signals,si^.si_signo);

 spin_unlock(sq^._queue_lock);

end;

function sigqueue_get(sq:p_sigqueue_t;signo:Integer;si:p_siginfo_t):Integer;
var
 node:p_ksiginfo_t;
 prev:p_ksiginfo_t;
 item:p_ksiginfo_t;
 count:DWORD;
begin
 if (sq=nil) or (si=nil) then Exit(0);

 if (_sigismember(@sq^._signals,signo)<>1) then Exit(0);

 count:=0;
 node:=nil;
 prev:=nil;

 spin_lock(sq^._queue_lock);

   item:=sq^._queue.pHead;
   While (item<>nil) do
   begin
    if (item^.ksi_info.si_signo=signo) then
    begin
     if (node=nil) then
     begin
      node:=item;
      sq^._queue.Remove(prev,item);
     end else
     begin
      Inc(count);
     end;
    end;

    prev:=item;
    item:=item^.pNext;
   end;

   if (node=nil) or (count=0) then
   begin
    _sigdelset(@sq^._signals,signo);
   end;

 spin_unlock(sq^._queue_lock);

 if (node=nil) then Exit(0);

 si^:=node^.ksi_info;
 FreeMem(node);

 Result:=signo;
end;

function sigqueue_push_ctx(sq:p_sigqueue_t;ectx:PCONTEXT_EXTENDED;_rsp:QWORD):Integer;
var
 node:p_ksig_context;
begin
 Result:=0;
 if (sq=nil) or (ectx=nil) then Exit;

 node:=AllocMem(SizeOf(ksig_context));
 if (node=nil) then Exit(EAGAIN);

 node^._rsp:=_rsp;

 if not CopyContextExtended(ectx,@node^.ectx) then
 begin
  FreeMem(node);
  Exit(EINVAL);
 end;

 sq^._contexts.Push(node);
end;

function sigqueue_pop_ctx(sq:p_sigqueue_t;ectx:PCONTEXT_EXTENDED;_rsp:QWORD):Integer;
var
 node:p_ksig_context;
begin
 Result:=0;
 if (sq=nil) or (ectx=nil) then Exit;

 node:=nil;
 sq^._contexts.Pop(node);
 if (node=nil) then Exit(EAGAIN);

 if (node^._rsp<>_rsp) then
 begin
  Result:=EFAULT;
 end;

 if not CopyContextExtended(@node^.ectx,ectx) then
 begin
  Result:=EINVAL;
 end;
 FreeMem(node);
end;

function _sig_pending(sq:p_sigqueue_t):DWORD; inline;
begin
 Result:=( load_acq_rel(sq^._signals.bits[0]) and (not load_acq_rel(sq^._mask.bits[0])) );
end;

function _sig_is_pending(sq:p_sigqueue_t):Boolean; inline;
begin
 Result:=_sig_pending(sq)<>0;
end;

function _sigaddset(_set:p_sigset_t;signum:Integer):Integer;
begin
 if (_set=nil) or
    (not _SIG_VALID(signum)) then Exit(EINVAL);
 signum:=signum-1;
 fetch_or(_set^.qwords[signum shr 6],1 shl (signum and 63));
 Result:=0;
end;

function _sigdelset(_set:p_sigset_t;signum:Integer):Integer;
begin
 if (_set=nil) or
    (not _SIG_VALID(signum)) then Exit(EINVAL);
 signum:=signum-1;
 fetch_and(_set^.qwords[signum shr 6],not(1 shl (signum and 63)));
 Result:=0;
end;

function _sigismember(_set:p_sigset_t;signum:Integer):Integer;
begin
 if (_set=nil) or
    (not _SIG_VALID(signum)) then Exit(EINVAL);
 signum:=signum-1;
 Result:=Integer((_set^.qwords[signum shr 6] and (1 shl (signum and 63)))<>0);
end;

function __sigprocmask(how:Integer;_set,oldset:p_sigset_t):Integer;
var
 t:pthread;
begin
 t:=tcb_thread;
 if (t=nil) then Exit(EINVAL);

 if (_set=nil) then
 begin
  if (oldset<>nil) then oldset^:=t^.sig._mask;
  Exit(0);
 end;

 Case how of
  SIG_BLOCK:
    begin
     if (oldset<>nil) then oldset^:=t^.sig._mask;
     t^.sig._mask.qwords[0]:=t^.sig._mask.qwords[0] or _set^.qwords[0];
     t^.sig._mask.qwords[1]:=t^.sig._mask.qwords[1] or _set^.qwords[1];
    end;
  SIG_UNBLOCK:
    begin
     if (oldset<>nil) then oldset^:=t^.sig._mask;
     t^.sig._mask.qwords[0]:=t^.sig._mask.qwords[0] and (not _set^.qwords[0]);
     t^.sig._mask.qwords[1]:=t^.sig._mask.qwords[1] and (not _set^.qwords[1]);
    end;
  SIG_SETMASK:
    begin
     if (oldset<>nil) then oldset^:=t^.sig._mask;
     t^.sig._mask:=_set^;
    end;
  else
    Exit(EINVAL);
 end;

 Result:=0;
end;

function __sigaction(signum:Integer;act,oldact:p_sigaction_t):Integer;
var
 idx:DWORD;
 tmp:sigaction_t;
begin
 if not _SIG_VALID_32(signum) then Exit(EINVAL);

 if (act=nil) then
 begin
  tmp:=Default(sigaction_t);
 end else
 begin
  tmp:=act^;
 end;

 Case signum of
  SIGKILL,
  SIGSTOP:
    begin
     case tmp.__sigaction_u.__code of
      SIG_DFL:;
      else
       Exit(EINVAL);
     end;
    end;
  else;
 end;

 idx:=_SIG_IDX(signum);

 if (oldact<>nil) then
 begin
  oldact^:=ps_sigact[idx];
 end;

 ps_sigact[idx]:=tmp;

 Result:=0;
end;

function _sig_dfl(sig:Integer):Integer;
begin
 Case sig of
  SIGURG  ,
  SIGCHLD ,
  SIGWINCH,
  SIGINFO :Result:=SIG_IGN;
  else     Result:=SIG_ERR;
 end;
end;

function _sig_act_type(sig:Integer;var act:sigaction_t):Integer;
begin
 Result:=0;
 Case act.__sigaction_u.__code  of
  SIG_DFL:Case _sig_dfl(sig) of
           SIG_IGN:Result:=SIG_IGN;
           SIG_ERR:Result:=SIG_ERR;
          end;
  SIG_IGN  :Result:=SIG_IGN;
  SIG_ERR  :Result:=SIG_ERR;
  SIG_CATCH:Result:=SIG_ERR;
  SIG_HOLD :Result:=SIG_ERR;
 end;
end;

const
 ALERTABLE_FLAG=1;
 SPIN_WAIT_FLAG=2;

function __sig_self_interrupt(t:pthread):Integer; forward;

procedure _sig_lock(flags:integer=0);
begin
 __sig_lock(tcb_thread,flags);
end;

procedure __sig_lock(pt:Pointer;flags:integer=0);
label
 tryagain;
var
 t:pthread;
 i:Integer;
 pc:QWORD;
begin
 if (pt=nil) then Exit;
 t:=pt;

 if ((flags and SL_ALERTABLE)<>0) then
 begin
  fetch_or(t^.sig._flag,ALERTABLE_FLAG);
 end;

 i:=fetch_add(t^.sig._lock,1);

 //need to interrupt
 if ((flags and SL_NOINTRRUP)=0) and ((i=0) or ((flags and SL_ALERTABLE)<>0)) then
 begin
  tryagain:

  fetch_or(t^.sig._flag,SPIN_WAIT_FLAG);

   pc:=0;
   While (_sig_is_pending(@t^.sig)) do
   begin
    spin_pause;
    Inc(pc);
    if (pc>pf_deadlock) then
    begin
     //deadlock
     __sig_self_interrupt(t);
    end;
   end;

  fetch_and(t^.sig._flag,DWORD(not SPIN_WAIT_FLAG));

  if (_sig_is_pending(@t^.sig)) then goto tryagain;

 end;

end;

procedure _sig_unlock(flags:integer=0);
begin
 __sig_unlock(tcb_thread,flags);
end;

procedure __sig_unlock(pt:Pointer;flags:integer=0);
label
 tryagain;
var
 t:pthread;
 i:Integer;
 Alertable:Boolean;
 pc:QWORD;
begin
 if (pt=nil) then Exit;
 t:=pt;

 i:=load_acq_rel(t^.sig._lock);
 Alertable:=((t^.sig._flag and ALERTABLE_FLAG)<>0);

 i:=fetch_sub(t^.sig._lock,1);

 //need to interrupt
 if ((flags and SL_NOINTRRUP)=0) and ((i=1) or Alertable) then
 begin
  tryagain:

  fetch_or(t^.sig._flag,SPIN_WAIT_FLAG);

   pc:=0;
   While (_sig_is_pending(@t^.sig)) do
   begin
    spin_pause;
    Inc(pc);
    if (pc>pf_deadlock) then
    begin
     //deadlock
     __sig_self_interrupt(t);
    end;
   end;

  fetch_and(t^.sig._flag,DWORD(not (ALERTABLE_FLAG or SPIN_WAIT_FLAG)));

  if (_sig_is_pending(@t^.sig)) then goto tryagain;

 end;

 if ((flags and SL_NOINTRRUP)=0) then
 begin
  fetch_and(t^.sig._flag,DWORD(not ALERTABLE_FLAG));
 end;
end;

//var
// _test_counter:DWORD;

procedure __sig_test(t:pthread;ectx:PCONTEXT_EXTENDED); MS_ABI_Default;
var
 signo:Integer;
 sact:sigaction_t;
 info:siginfo_t;
 ucontext:_ucontext_t;
 old:sigset_t;
 errno:QWORD;
 //_rsp:QWORD;
 _rbp:QWORD;
 sigpending:DWORD;
 win_err:DWORD;
 _lock:Integer;
 setmask:Boolean;
begin
 if (t=nil) then Exit;

 win_err:=GetLastError;
 errno:=t^.errno;

 While true do
 begin

  sigpending:=_sig_pending(@t^.sig);
  if (sigpending=0) then Break;

  signo:=BsfDWord(sigpending)+1;

  While (sigqueue_get(@t^.sig,signo,@info)<>0) do
  begin

   //Writeln('>__sig_test:'{,system.InterlockedIncrement(_test_counter)},':',t^.ThreadId);

   sact:=ps_sigact[_SIG_IDX(signo)];

   if ((sact.sa_flags and SA_RESETHAND)<>0) then
   begin
    ps_sigact[_SIG_IDX(signo)]:=Default(sigaction_t);
   end;

   Case _sig_act_type(info.si_signo,sact) of
    SIG_IGN:;
    SIG_ERR:;
    else
     begin
      //errno:=t^.errno;
      setmask:=((sact.sa_flags and SA_NODEFER)=0);

      if setmask then
      begin
       old:=t^.sig._mask;
       t^.sig._mask:=sact.sa_mask;
      end;

      ucontext:=Default(_ucontext_t);
      _copy_ctx_from_sys(ectx^.CONTEXT,@ucontext);
      ucontext.uc_sigmask:=t^.sig._mask;
      ucontext.uc_mcontext.mc_err:=errno;

      //asm
      // //Mov %rsp,_rsp
      // Mov %rbp,_rbp
      //end;

      //link break from???
      //ucontext.uc_mcontext.mc_lbrfrom :=_rsp;
      //link break to???
      //ucontext.uc_mcontext.mc_lbrto   :=_rbp; //bottom in GC_push_all_stack

      //ucontext.uc_mcontext.mc_lbrfrom :=ucontext.uc_mcontext.mc_rsp;
      //ucontext.uc_mcontext.mc_lbrto   :=ucontext.uc_mcontext.mc_rsp;

      _lock:=t^.sig._lock;

      if ((sact.sa_flags and SA_SIGINFO)=0) then
      begin
       //sa_handler
       sact.__sigaction_u.__sa_handler(info.si_signo,info.si_code,@ucontext);
      end else
      begin
       //sa_sigaction
       sact.__sigaction_u.__sa_sigaction(info.si_signo,@info,@ucontext);
      end;

      if (_lock<>t^.sig._lock) then
      begin
       Assert(false);
      end;

      if setmask then
      begin
       t^.sig._mask:=old;
      end;
      //t^.errno:=errno;

     end;
   end;

   //Writeln('<__sig_test:'{,_test_counter,':'},t^.ThreadId);

  end;

 end;

 t^.errno:=errno;
 SetLastError(win_err);
end;

//compiler i hate you
procedure __sig_test_align(t:pthread;ectx:PCONTEXT_EXTENDED); MS_ABI_Default; assembler; nostackframe;
asm
 pushq %rbp
 movq %rsp, %rbp
 andq $-32, %rsp //align stack by AVX
 call __sig_test
 lea  0x0(%rbp),%rsp
 popq %rbp
 ret
end;

procedure __sig_interrupt(t:pthread); MS_ABI_Default;
var
 ctx:TCONTEXT_EXTENDED;
 rsp:QWORD;
begin
 if (t=nil) then Exit;

 asm
  Movq %rbp,rsp
 end;

 if (sigqueue_pop_ctx(@t^.sig,@ctx,rsp+8)<>0) then
 begin
  asm
   ud2
  end;
 end;

 //Writeln('>__sig_interrupt:',t^.ThreadId,' ',t^.sig._lock);

 repeat
  __sig_test_align(t,@ctx);
 until event_try_disable(t^.sig._event);

 NtContinue(ctx.CONTEXT,False);
 asm
  ud2
 end;
end;

procedure _update_ctx(ctx:PCONTEXT); MS_ABI_Default; assembler; nostackframe;
asm
 movq %rbx,TCONTEXT.rbx(%rcx)
 movq %rbp,TCONTEXT.rbp(%rcx)
 movq %r12,TCONTEXT.r12(%rcx)
 movq %r13,TCONTEXT.r13(%rcx)
 movq %r14,TCONTEXT.r14(%rcx)
 movq %r15,TCONTEXT.r15(%rcx)
 movq %rsi,TCONTEXT.rsi(%rcx)
 movq %rdi,TCONTEXT.rdi(%rcx)
 leaq 8(%rsp),%rax
 movq %rax,TCONTEXT.rsp(%rcx)
end;

function __sig_self_interrupt(t:pthread):Integer;
var
 ctx:TCONTEXT_EXTENDED;
 rsp:QWORD;
label
 eoi;
begin

 event_try_enable(t^.sig._event); //mark change

 //Writeln('__sig_self_interrupt');

 if not InitializeContextExtended(@ctx) then Exit(ESRCH);
 if (NtGetContextThread(t^.handle,ctx.CONTEXT)<>STATUS_SUCCESS) then Exit(ESRCH);

 _update_ctx(ctx.CONTEXT);
 ctx.CONTEXT^.Rip:=qword(@eoi);

 rsp:=AlignDw(ctx.CONTEXT^.Rsp-SizeOf(Pointer),SizeOf(Pointer));

 Result:=sigqueue_push_ctx(@t^.sig,@ctx,rsp);
 if (Result<>0) then Exit;

 ctx.CONTEXT^.Rip:=qword(@__sig_interrupt);
 ctx.CONTEXT^.Rcx:=qword(t);
 ctx.CONTEXT^.Rsp:=rsp;

 //Writeln('beg Sptr=',HexStr(Sptr));

 //Writeln('>NtContinue:',HexStr(ctx.CONTEXT^.Rip,16));
 NtContinue(ctx.CONTEXT,False);

 eoi:

 //Writeln('end Sptr=',HexStr(Sptr));

 Result:=0;
end;

procedure _apc_null(dwParam:PTRUINT); stdcall;
begin
end;

//var
// _send_counter:DWORD;

function nt_send_interrupt(t:pthread;signo:Integer):Integer;
label
 tryagain;
var
 ctx:TCONTEXT_EXTENDED;
 rsp:QWORD;
 w:LARGE_INTEGER;
begin
 Result:=0;
 tryagain:

 if (NtSuspendThread(t^.handle,nil)<>STATUS_SUCCESS) then
 begin
  Exit(ESRCH);
 end;

 w.QuadPart:=0;
 if (NtWaitForSingleObject(t^.handle,False,@w)<>STATUS_TIMEOUT) then
 begin
  NtResumeThread(t^.handle,nil);
  Exit(ESRCH);
 end;

 if (_sigismember(@t^.sig._signals,signo)<>1) then
 begin
  //signal is gone
  NtResumeThread(t^.handle,nil);
  Exit(0);
 end;

 if (t^.sig._lock=0) or ((t^.sig._flag and SPIN_WAIT_FLAG)<>0) then //(not locked) or wait
 begin

  if not InitializeContextExtended(@ctx) then
  begin
   NtResumeThread(t^.handle,nil);
   Exit(ESRCH);
  end;

  if (NtGetContextThread(t^.handle,ctx.CONTEXT)<>STATUS_SUCCESS) then
  begin
   NtResumeThread(t^.handle,nil);
   Exit(ESRCH);
  end;

  if IS_SYSCALL(ctx.CONTEXT^.Rip) then //system call in code without blocking
  begin
   //skip
   //Writeln('Warn syscall:0x',HexStr(ctx.CONTEXT^.Rax,4));

   //store_release(t^.sig._wait,1);

   if ((ps_sigact[_SIG_IDX(signo)].sa_flags and SA_RESTART)<>0) then
   begin
    NtResumeThread(t^.handle,nil);
    w.QuadPart:=-10000;
    SwDelayExecution(False,@w); //100ms
    goto tryagain;
   end else
   begin
    NtResumeThread(t^.handle,nil);
    Exit(EAGAIN);
   end;

  end else
  begin

   rsp:=AlignDw(ctx.CONTEXT^.Rsp-SizeOf(Pointer),SizeOf(Pointer));

   Result:=sigqueue_push_ctx(@t^.sig,@ctx,rsp);
   if (Result<>0) then
   begin
    NtResumeThread(t^.handle,nil);
    Exit;
   end;

   ctx.CONTEXT^.Rip:=qword(@__sig_interrupt);
   ctx.CONTEXT^.Rcx:=qword(t);
   ctx.CONTEXT^.Rsp:=rsp;

   if (NtSetContextThread(t^.handle,ctx.CONTEXT)<>STATUS_SUCCESS) then
   begin
    NtResumeThread(t^.handle,nil);
    Exit(ESRCH);
   end;

  end;

  NtResumeThread(t^.handle,nil);
 end else
 if ((t^.sig._flag and ALERTABLE_FLAG)<>0) then //Alertable
 begin
  NtQueueApcThread(t^.handle,@_apc_null,0,nil,0);

  NtResumeThread(t^.handle,nil);

  if ((ps_sigact[_SIG_IDX(signo)].sa_flags and SA_RESTART)<>0) then
  begin
   w.QuadPart:=-10000;
   SwDelayExecution(False,@w); //100ms
   goto tryagain;
  end else
  begin
   Exit(EAGAIN);
  end;

 end else
 begin
  //locked

  NtResumeThread(t^.handle,nil);

  if ((ps_sigact[_SIG_IDX(signo)].sa_flags and SA_RESTART)<>0) then
  begin
   w.QuadPart:=-10000;
   SwDelayExecution(False,@w); //100ms
   goto tryagain;
  end else
  begin
   Exit(EAGAIN);
  end;

 end;
end;

function __sig_send_interrupt(t:pthread;signo:Integer):Integer;
begin
 Result:=0;

 Writeln('__sig_send_interrupt:'{,system.InterlockedIncrement(_send_counter),':'},t^.ThreadId);

 event_try_enable(t^.sig._event); //mark change

 if spin_trylock(t^.sig._event_lock) then
 begin
  Result:=nt_send_interrupt(t,signo);
  spin_unlock(t^.sig._event_lock);
 end;

end;

function _pthread_kill(t:Pointer;sig:Integer):Integer;
var
 idx:DWORD;
 act:sigaction_t;
 sinfo:siginfo_t;
begin
 if (t=nil) then Exit(EINVAL);

 Writeln('_pthread_kill:',sig,':',pthread(t)^.ThreadId);

 if (sig=0) then Exit(0); //check pthread

 if not _SIG_VALID_32(sig) then Exit(EINVAL);

 idx:=_SIG_IDX(sig);

 act:=ps_sigact[idx];

 Case _sig_act_type(sig,act) of
  SIG_IGN:
   begin
    Exit(0);
   end;
  SIG_ERR:
    begin
     DebugBreak;
     Halt(0);
    end;
  else;
 end;

 sinfo:=Default(siginfo_t);
 sinfo.si_signo:=sig;
 sinfo.si_code :=SI_USER;
 sinfo.si_pid  :=GetCurrentProcessId;

 Result:=sigqueue_add(@pthread(t)^.sig,@sinfo);
 if (Result<>0) then Exit;

 if (t=tcb_thread) then
 begin
  _sig_lock;
  Result:=__sig_self_interrupt(t);
  _sig_unlock;
 end else
 begin
  _sig_lock;
  Result:=__sig_send_interrupt(t,sig);
  _sig_unlock;
 end;
end;

procedure _copy_ctx_from_sys(src:PCONTEXT;dst:p_ucontext_t);
var
 flags:DWORD;
 context_ex:PCONTEXT_EX;
 xs:PXSTATE;
 uc_xsave:PXmmSaveArea;
 uc_xstate:PXSTATE;
begin
 if (src=nil) or (dst=nil) then Exit;

 flags:=src^.ContextFlags and (not CONTEXT_AMD64);

 context_ex:=PCONTEXT_EX(src+1);
 xs:=PXSTATE(PByte(context_ex)+context_ex^.XState.Offset);

 uc_xsave:=PXmmSaveArea(@dst^.uc_mcontext.mc_fpstate);
 uc_xstate:=PXSTATE(uc_xsave+1);

 if ((flags and CONTEXT_INTEGER)<>0) then
 begin
  dst^.uc_flags:=dst^.uc_flags or _UC_CPU;

  dst^.uc_mcontext.mc_rax:=src^.Rax;
  dst^.uc_mcontext.mc_rbx:=src^.Rbx;
  dst^.uc_mcontext.mc_rcx:=src^.Rcx;
  dst^.uc_mcontext.mc_rdx:=src^.Rdx;
  dst^.uc_mcontext.mc_rsi:=src^.Rsi;
  dst^.uc_mcontext.mc_rdi:=src^.Rdi;
  dst^.uc_mcontext.mc_r8 :=src^.R8;
  dst^.uc_mcontext.mc_r9 :=src^.R9;
  dst^.uc_mcontext.mc_r10:=src^.R10;
  dst^.uc_mcontext.mc_r11:=src^.R11;
  dst^.uc_mcontext.mc_r12:=src^.R12;
  dst^.uc_mcontext.mc_r13:=src^.R13;
  dst^.uc_mcontext.mc_r14:=src^.R14;
  dst^.uc_mcontext.mc_r15:=src^.R15;
 end;

 if ((flags and CONTEXT_CONTROL)<>0) then
 begin
  dst^.uc_flags:=dst^.uc_flags or _UC_CPU;

  dst^.uc_mcontext.mc_rsp   :=src^.Rsp;
  dst^.uc_mcontext.mc_rbp   :=src^.Rbp;
  dst^.uc_mcontext.mc_rip   :=src^.Rip;
  dst^.uc_mcontext.mc_rflags:=src^.EFlags;
  dst^.uc_mcontext.mc_cs    :=src^.Segcs;
  dst^.uc_mcontext.mc_ss    :=src^.Segss;
 end;

 if ((flags and CONTEXT_SEGMENTS)<>0) then
 begin
  dst^.uc_flags:=dst^.uc_flags or _UC_CPU;

  dst^.uc_mcontext.mc_ds:=src^.SegDs;
  dst^.uc_mcontext.mc_es:=src^.SegEs;
  dst^.uc_mcontext.mc_fs:=src^.SegFs;
  dst^.uc_mcontext.mc_gs:=src^.SegGs;
 end;

 if ((flags and CONTEXT_FLOATING_POINT)<>0) then
 begin
  dst^.uc_flags:=dst^.uc_flags or _UC_FPU;

  uc_xsave^:=src^.FltSave;
  uc_xstate^.Mask:=uc_xstate^.Mask or XSTATE_MASK_LEGACY;
 end;

 if ((flags and CONTEXT_XSTATE)<>0) then
 begin
  if ((xs^.Mask and XSTATE_MASK_AVX)<>0) then
  begin
   uc_xstate^.Mask:=uc_xstate^.Mask or XSTATE_MASK_AVX;
   Move(xs^.YmmContext,uc_xstate^.YmmContext,SizeOf(TYMMCONTEXT));
  end;
 end;

 if ((flags and CONTEXT_DEBUG_REGISTERS)<>0) then
 begin
  //save it somewhere
  dst^.uc_mcontext.mc_spare[0]:=src^.Dr0;
  dst^.uc_mcontext.mc_spare[1]:=src^.Dr1;
  dst^.uc_mcontext.mc_spare[2]:=src^.Dr2;
  dst^.uc_mcontext.mc_spare[3]:=src^.Dr3;
  dst^.uc_mcontext.mc_spare[4]:=src^.Dr6;
  dst^.uc_mcontext.mc_spare[5]:=src^.Dr7;
 end;

 //fix me
 dst^.uc_mcontext.mc_lbrfrom:=src^.Rsp; //LastBranchFromRip
 dst^.uc_mcontext.mc_lbrto  :=src^.Rbp; //LastBranchToRip

 dst^.uc_flags:=dst^.uc_flags or (flags shl 8); //set as extended

 dst^.uc_mcontext.mc_addr    :=dst^.uc_mcontext.mc_rip;
 dst^.uc_mcontext.mc_len     :=SizeOf(mcontext_t);
 dst^.uc_mcontext.mc_flags   :=_MC_HASSEGS;
 dst^.uc_mcontext.mc_fpformat:=_MC_FPFMT_XMM;
 dst^.uc_mcontext.mc_ownedfp :=_MC_FPOWNED_FPU;
end;

procedure _copy_ctx_to_sys(src:p_ucontext_t;dst:PCONTEXT);
var
 flags:DWORD;
 context_ex:PCONTEXT_EX;
 xs:PXSTATE;
 uc_xsave:PXmmSaveArea;
 uc_xstate:PXSTATE;
begin
 if (src=nil) or (dst=nil) then Exit;

 context_ex:=PCONTEXT_EX(src+1);
 xs:=PXSTATE(PByte(context_ex)+context_ex^.XState.Offset);

 uc_xsave:=PXmmSaveArea(@src^.uc_mcontext.mc_fpstate);
 uc_xstate:=PXSTATE(uc_xsave+1);

 flags:=(src^.uc_flags shr 8); //get extended

 if (flags=0) then //nop, get by other
 begin
  if ((src^.uc_flags and _UC_CPU)<>0) then
  begin
   flags:=flags or CONTEXT_INTEGER or CONTEXT_CONTROL or CONTEXT_SEGMENTS;
  end;
  if ((src^.uc_flags and _UC_FPU)<>0) then
  begin
   flags:=flags or CONTEXT_FLOATING_POINT;
  end;
  if ((uc_xstate^.Mask and XSTATE_MASK_AVX)<>0) then
  begin
   flags:=flags or CONTEXT_XSTATE;
  end;
  flags:=flags and (not CONTEXT_AMD64);
 end;

 flags:=flags and dst^.ContextFlags; //filter
 dst^.ContextFlags:=flags or CONTEXT_AMD64; //update

 if ((flags and CONTEXT_INTEGER)<>0) then
 begin
  dst^.Rax:=src^.uc_mcontext.mc_rax;
  dst^.Rbx:=src^.uc_mcontext.mc_rbx;
  dst^.Rcx:=src^.uc_mcontext.mc_rcx;
  dst^.Rdx:=src^.uc_mcontext.mc_rdx;
  dst^.Rsi:=src^.uc_mcontext.mc_rsi;
  dst^.Rdi:=src^.uc_mcontext.mc_rdi;
  dst^.R8 :=src^.uc_mcontext.mc_r8;
  dst^.R9 :=src^.uc_mcontext.mc_r9;
  dst^.R10:=src^.uc_mcontext.mc_r10;
  dst^.R11:=src^.uc_mcontext.mc_r11;
  dst^.R12:=src^.uc_mcontext.mc_r12;
  dst^.R13:=src^.uc_mcontext.mc_r13;
  dst^.R14:=src^.uc_mcontext.mc_r14;
  dst^.R15:=src^.uc_mcontext.mc_r15;
 end;

 if ((flags and CONTEXT_CONTROL)<>0) then
 begin
  dst^.Rsp   :=src^.uc_mcontext.mc_rsp;
  dst^.Rbp   :=src^.uc_mcontext.mc_rbp;
  dst^.Rip   :=src^.uc_mcontext.mc_rip;
  dst^.EFlags:=src^.uc_mcontext.mc_rflags;
  dst^.Segcs :=src^.uc_mcontext.mc_cs;
  dst^.Segss :=src^.uc_mcontext.mc_ss;
 end;

 if ((flags and CONTEXT_SEGMENTS)<>0) then
 begin
  dst^.SegDs:=src^.uc_mcontext.mc_ds;
  dst^.SegEs:=src^.uc_mcontext.mc_es;
  dst^.SegFs:=src^.uc_mcontext.mc_fs;
  dst^.SegGs:=src^.uc_mcontext.mc_gs;
 end;

 if ((flags and CONTEXT_FLOATING_POINT)<>0) then
 begin

  if ((uc_xstate^.Mask and XSTATE_MASK_LEGACY_FLOATING_POINT)<>0) then
  begin
   Move(uc_xsave^,dst^.FltSave,ptruint(@PXmmSaveArea(nil)^.MxCsr));
   Move(uc_xsave^.FloatRegisters,dst^.FltSave.FloatRegisters,SizeOf(TXmmSaveArea.FloatRegisters));
  end else
  begin
   FillChar(dst^.FltSave,ptruint(@PXmmSaveArea(nil)^.MxCsr),0);
   FillChar(dst^.FltSave.FloatRegisters,SizeOf(TXmmSaveArea.FloatRegisters),0);
   dst^.FltSave.ControlWord:=$37f;
  end;

  if ((uc_xstate^.Mask and XSTATE_MASK_LEGACY_SSE)<>0) then
  begin
   Move(uc_xsave^.XmmRegisters,dst^.FltSave.XmmRegisters,SizeOf(TXmmSaveArea.XmmRegisters));
   dst^.FltSave.MxCsr     :=uc_xsave^.MxCsr;
   dst^.FltSave.MxCsr_Mask:=uc_xsave^.MxCsr_Mask;
  end else
  begin
   FillChar(dst^.FltSave.XmmRegisters,SizeOf(TXmmSaveArea.XmmRegisters),0);
   dst^.FltSave.MxCsr     :=$1f80;
   dst^.FltSave.MxCsr_Mask:=$2ffff;
  end;

  dst^.MxCsr:=dst^.FltSave.MxCsr;
 end;

 if ((flags and CONTEXT_XSTATE)<>0) then
 begin
  xs^.Mask:=uc_xstate^.Mask;
  if ((uc_xstate^.Mask and XSTATE_MASK_AVX)<>0) then
  begin
   Move(uc_xstate^.YmmContext,xs^.YmmContext,SizeOf(TYMMCONTEXT));
  end;
 end;

 if ((flags and CONTEXT_DEBUG_REGISTERS)<>0) then
 begin
  //save it somewhere
  dst^.Dr0:=src^.uc_mcontext.mc_spare[0];
  dst^.Dr1:=src^.uc_mcontext.mc_spare[1];
  dst^.Dr2:=src^.uc_mcontext.mc_spare[2];
  dst^.Dr3:=src^.uc_mcontext.mc_spare[3];
  dst^.Dr6:=src^.uc_mcontext.mc_spare[4];
  dst^.Dr7:=src^.uc_mcontext.mc_spare[5];
 end;

end;

Procedure Init;
var
 pc:QWORD;
begin
 pf_deadlock:=1;
 NtQueryPerformanceCounter(@pc,@pf_deadlock);
 pf_deadlock:=pf_deadlock*2;
end;

initialization
 Init;

end.

