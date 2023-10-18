unit kern_thread;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 kern_param,
 kern_thr,
 ucontext,
 signal,
 signalvar,
 time,
 rtprio,
 hamt;

type
 p_kthread_iterator=^kthread_iterator;
 kthread_iterator=THAMT_Iterator32;

procedure thread_reap();

function  thread_alloc:p_kthread;
procedure thread_free(td:p_kthread);

function  sys_thr_new     (_param:Pointer;_size:Integer):Integer;
function  sys_thr_create  (ctx:Pointer;id:PDWORD;flags:Integer):Integer;
function  sys_thr_self    (id:PDWORD):Integer;
procedure sys_thr_exit    (state:PQWORD);
function  sys_thr_kill    (id,sig:Integer):Integer;
function  sys_thr_kill2   (pid,id,sig:Integer):Integer;
function  sys_thr_suspend (timeout:Pointer):Integer;
function  sys_thr_wake    (id:DWORD):Integer;
function  sys_thr_set_name(id:DWORD;pname:PChar):Integer;
function  sys_thr_get_name(id:DWORD;pname:PChar):Integer;

function  sys_amd64_set_fsbase(base:Pointer):Integer;
function  sys_amd64_get_fsbase(base:PPointer):Integer;
function  sys_amd64_set_gsbase(base:Pointer):Integer;
function  sys_amd64_get_gsbase(base:PPointer):Integer;

procedure thread_inc_ref(td:p_kthread);
procedure thread_dec_ref(td:p_kthread);
procedure thread_lock   (td:p_kthread);
procedure thread_unlock (td:p_kthread);
function  tdfind(tid:DWORD):p_kthread;

function  FOREACH_THREAD_START (i:p_kthread_iterator):Boolean;
procedure FOREACH_THREAD_FINISH();
function  THREAD_NEXT(i:p_kthread_iterator):Boolean;
function  THREAD_GET (i:p_kthread_iterator):p_kthread;

function  SIGPENDING(td:p_kthread):Boolean;

procedure threadinit; //SYSINIT

function  kthread_add (func,arg:Pointer;newtdp:pp_kthread;name:PChar):Integer;
procedure kthread_exit();

implementation

uses
 errno,
 systm,
 kern_mtx,
 md_sleep,
 md_context,
 machdep,
 md_proc,
 md_thread,
 kern_rwlock,
 kern_sig,
 kern_proc,
 sched_ule;

//

procedure umtx_thread_init(td:p_kthread); external;
procedure umtx_thread_exit(td:p_kthread); external;
procedure umtx_thread_fini(td:p_kthread); external;
function  kern_umtx_wake(td:p_kthread;umtx:Pointer;n_wake,priv:Integer):Integer; external;
function  umtx_copyin_timeout(addr:Pointer;tsp:p_timespec):Integer; external;

function  sleepq_alloc:Pointer;    external;
procedure sleepq_free(sq:Pointer); external;

procedure switch_to_jit(td:p_kthread); external;

function  msleep(ident   :Pointer;
                 lock    :p_mtx;
                 priority:Integer;
                 wmesg   :PChar;
                 timo    :Int64):Integer; external;

//

var
 tidhashtbl:TSTUB_HAMT32;
 tidhash_lock:Pointer=nil;

 zombie_threads:TAILQ_HEAD=(tqh_first:nil;tqh_last:@zombie_threads.tqh_first);
 zombie_lock:Pointer=nil;

 p_numthreads:Integer=0;

const
 max_threads_per_proc=1500;

function SIGPENDING(td:p_kthread):Boolean;
begin
 Result:=SIGNOTEMPTY(@td^.td_sigqueue.sq_signals) and
         sigsetmasked(@td^.td_sigqueue.sq_signals,@td^.td_sigmask);
end;

//

function _thread_null(parameter:pointer):ptrint; register;
begin
 Result:=0;
end;

procedure threadinit;
begin
 FillChar(tidhashtbl,SizeOf(tidhashtbl),0);
 //init internals
 BeginThread(@_thread_null);
end;

{
 * Place an unused thread on the zombie list.
 * Use the slpq as that must be unused by now.
 }
procedure thread_zombie(td:p_kthread);
begin
 rw_wlock(zombie_lock);
 TAILQ_INSERT_HEAD(@zombie_threads,td,@td^.td_slpq);
 rw_wunlock(zombie_lock);
end;

{
 * Reap zombie resources.
 }
procedure thread_reap();
var
 td_first,td_next:p_kthread;
begin
 if rw_try_wlock(zombie_lock) then
 begin
  if (not TAILQ_EMPTY(@zombie_threads)) then
  begin
   td_first:=TAILQ_FIRST(@zombie_threads);

   while (td_first<>nil) do
   begin
    td_next:=TAILQ_NEXT(td_first,@td_first^.td_slpq);

    if cpu_thread_finished(td_first) then
    begin
     TAILQ_REMOVE(@zombie_threads,@td_first,@td_first^.td_slpq);
     thread_free(td_first);
    end else
    begin
     Break;
    end;

    td_first:=td_next;
   end;
  end;

  rw_wunlock(zombie_lock);
 end;
end;

function thread_alloc:p_kthread;
begin
 thread_reap();

 Result:=cpu_thread_alloc();

 Result^.td_state:=TDS_INACTIVE;
 Result^.td_lend_user_pri:=PRI_MAX;

 Result^.td_sleepqueue:=sleepq_alloc();
 umtx_thread_init(Result);
end;

procedure thread_free(td:p_kthread);

begin
 sleepq_free(td^.td_sleepqueue);
 umtx_thread_fini(td);
 cpu_thread_free(td);
end;

procedure thread_inc_ref(td:p_kthread);
begin
 System.InterlockedIncrement(td^.td_ref);
end;

procedure thread_dec_ref(td:p_kthread);
begin
 if (System.InterlockedDecrement(td^.td_ref)=0) then
 begin
  thread_zombie(td);
 end;
end;

procedure thread_lock(td:p_kthread); public;
begin
 rw_wlock(td^.td_lock);
end;

procedure thread_unlock(td:p_kthread); public;
begin
 rw_wunlock(td^.td_lock);
end;

procedure thread_link(td:p_kthread);
begin
 td^.td_state:=TDS_INACTIVE;
 td^.td_flags:=TDF_INMEM;

 sigqueue_init(@td^.td_sigqueue);

 System.InterlockedIncrement(p_numthreads);
end;

procedure thread_unlink(td:p_kthread);
begin
 System.InterlockedDecrement(p_numthreads)
end;

function tdfind(tid:DWORD):p_kthread;
Var
 data:PPointer;
begin
 Result:=nil;
 rw_rlock(tidhash_lock);

 data:=HAMT_search32(@tidhashtbl,tid);

 if (data<>nil) then
 begin
  Result:=data^;
 end;

 if (Result<>nil) then
 begin
  thread_inc_ref(Result);
 end;

 rw_runlock(tidhash_lock);
end;

procedure tidhash_add(td:p_kthread);
var
 data:PPointer;
begin
 rw_wlock(tidhash_lock);

 data:=HAMT_insert32(@tidhashtbl,td^.td_tid,td);

 if (data<>nil) then
 begin
  if (data^=td) then
  begin
   thread_inc_ref(td);
  end;
 end;

 rw_wunlock(tidhash_lock);
end;

procedure tidhash_remove(td:p_kthread);
var
 data:Pointer;
begin
 data:=nil;
 rw_wlock(tidhash_lock);

 HAMT_delete32(@tidhashtbl,td^.td_tid,@data);

 rw_wunlock(tidhash_lock);

 if (data=td) then
 begin
  thread_dec_ref(td);
 end;
end;

function FOREACH_THREAD_START(i:p_kthread_iterator):Boolean;
begin
 rw_rlock(tidhash_lock);

 Result:=HAMT_first32(@tidhashtbl,i);

 if not Result then //space
 begin
  rw_runlock(tidhash_lock);
 end;
end;

procedure FOREACH_THREAD_FINISH();
begin
 rw_runlock(tidhash_lock);
end;

function THREAD_NEXT(i:p_kthread_iterator):Boolean;
begin
 Result:=HAMT_next32(i);
end;

function THREAD_GET(i:p_kthread_iterator):p_kthread;
begin
 Result:=nil;
 HAMT_get_value32(i,@Result);
end;

procedure KernSetThreadDebugName(newtd:p_kthread;prefix:PChar);
var
 td:p_kthread;
 backup:array[0..1] of QWORD;
 name:shortstring;
begin
 name:=shortstring(prefix)+shortstring(newtd^.td_name);

 td:=curkthread;
 if (td<>nil) then
 begin
  //prevent bullshit in ntdll:__chkstk
  backup[0]:=PQWORD(td^.td_kstack.stack)[-1];
  backup[1]:=PQWORD(td^.td_kstack.stack)[-2];
  PQWORD(td^.td_kstack.stack)[-1]:=0;
  PQWORD(td^.td_kstack.stack)[-2]:=0;
 end;

 SetThreadDebugName(newtd^.td_tid,name);

 if (td<>nil) then
 begin
  //restore stack
  PQWORD(td^.td_kstack.stack)[-1]:=backup[0];
  PQWORD(td^.td_kstack.stack)[-2]:=backup[1];
 end;
end;

procedure before_start(td:p_kthread);
begin
 //init this
 ipi_sigreturn; //switch
end;

procedure thread0_param(td:p_kthread);
begin
 td^.td_base_user_pri:=700;
 td^.td_lend_user_pri:=1023;
 td^.td_base_pri     :=68;
 td^.td_priority     :=68;
 td^.td_pri_class    :=10;
 td^.td_user_pri     :=700;
end;

function create_thread(td        :p_kthread; //calling thread
                       ctx       :p_mcontext_t;
                       start_func:Pointer;
                       arg       :Pointer;
                       stack_base:Pointer;
                       stack_size:QWORD;
                       tls_base  :Pointer;
                       child_tid :PDWORD;
                       parent_tid:PDWORD;
                       rtp       :p_rtprio;
                       name      :PChar
                      ):Integer;
var
 newtd:p_kthread;
 stack:stack_t;

 n:Integer;
begin
 Result:=0;
 if (p_numthreads>=max_threads_per_proc) then
 begin
  Exit(EPROCLIM);
 end;

 thread_reap();

 if (rtp<>nil) then
 begin
  Case (rtp^._type and $fff7) of //RTP_PRIO_BASE
   RTP_PRIO_IDLE:
     begin
      if (rtp^._prio<>960) then Exit(EINVAL);
     end;
   RTP_PRIO_NORMAL:
     begin
      if (rtp^._prio>959) then Exit(EINVAL);
     end;
   PRI_REALTIME:
     begin
      if (rtp^._prio>767) then Exit(EINVAL);
     end;
   else
     Exit(EINVAL)
  end;
 end;

 if (stack_size<$1000) then
 begin
  if (ctx<>nil) then
  begin
   stack_size:=$1000;
  end else
  begin
   Exit(EINVAL);
  end;
 end;

 if (ptruint(stack_base)<$1000) then
 begin
  if (ctx<>nil) then
  begin
   stack_base:=Pointer(ctx^.mc_rsp-stack_size);

   //re check
   if (ptruint(stack_base)<$1000) then
   begin
    Exit(EINVAL);
   end;
  end else
  begin
   Exit(EINVAL);
  end;
 end;

 newtd:=thread_alloc;
 if (newtd=nil) then Exit(ENOMEM);

 thread0_param(newtd);

 //user stack
 newtd^.td_ustack.stack:=stack_base+stack_size;
 newtd^.td_ustack.sttop:=stack_base;
 //user stack

 n:=cpu_thread_create(newtd,
                      stack_base,
                      stack_size,
                      @before_start,
                      Pointer(newtd));

 if (n<>0) then
 begin
  thread_free(newtd);
  Exit(EINVAL);
 end;

 if (child_tid<>nil) then
 begin
  n:=suword32(child_tid^,newtd^.td_tid);
  if (n<>0) then
  begin
   cpu_thread_terminate(newtd);
   thread_free(newtd);
   Exit(EFAULT);
  end;
 end;

 if (parent_tid<>nil) then
 begin
  n:=suword32(parent_tid^,newtd^.td_tid);
  if (n<>0) then
  begin
   cpu_thread_terminate(newtd);
   thread_free(newtd);
   Exit(EFAULT);
  end;
 end;

 if (ctx<>nil) then
 begin
  // old way to set user context
  n:=set_mcontext(newtd,ctx);
  if (n<>0) then
  begin
   cpu_thread_terminate(newtd);
   thread_free(newtd);
   Exit(n);
  end;
 end else
 begin
  // Set up our machine context.
  stack.ss_sp  :=stack_base;
  stack.ss_size:=stack_size;
  // Set upcall address to user thread entry function.
  cpu_set_upcall_kse(newtd,start_func,arg,@stack);
  // Setup user TLS address and TLS pointer register.
  cpu_set_user_tls(newtd,tls_base);
 end;

 //jit wrapper
 switch_to_jit(newtd);
 //jit wrapper

 //seh wrapper
 seh_wrapper(newtd);
 //seh wrapper

 if (td<>nil) then
 begin
  newtd^.td_sigmask:=td^.td_sigmask;
 end;

 thread_link(newtd);

 if (name<>nil) then
 begin
  Move(name^,newtd^.td_name,SizeOf(t_td_name));
 end;
 KernSetThreadDebugName(newtd,'ps4:');

 sched_fork_thread(td,newtd);

 tidhash_add(newtd);

 if (rtp<>nil) then
 begin
  if (td=nil) then
  begin
   rtp_to_pri(rtp,newtd);
   sched_prio(newtd,newtd^.td_user_pri);
  end else
  if (td^.td_pri_class<>PRI_TIMESHARE) then
  begin
   rtp_to_pri(rtp,newtd);
   sched_prio(newtd,newtd^.td_user_pri);
  end;
 end;

 n:=cpu_sched_add(newtd);
 if (n<>0) then
 begin
  cpu_thread_terminate(newtd);
  thread_free(newtd);
  Exit(EFAULT);
 end;
end;

function kthread_add(func,arg:Pointer;newtdp:pp_kthread;name:PChar):Integer;
var
 td:p_kthread;
 newtd:p_kthread;
 stack:stack_t;

 n:Integer;
begin
 Result:=0;

 if (func=nil) or
    (newtdp=nil) then
 begin
  Exit(EINVAL);
 end;

 thread_reap();

 td:=curkthread;

 newtd:=thread_alloc;
 if (newtd=nil) then Exit(ENOMEM);

 thread0_param(newtd);

 stack.ss_sp  :=newtd^.td_kstack.sttop;
 stack.ss_size:=(ptruint(newtd^.td_kstack.stack)-ptruint(newtd^.td_kstack.sttop));

 //user stack
 newtd^.td_ustack.stack:=newtd^.td_kstack.stack;
 newtd^.td_ustack.sttop:=newtd^.td_kstack.sttop;
 //user stack

 n:=cpu_thread_create(newtd,
                      stack.ss_sp,
                      stack.ss_size,
                      func,
                      arg);

 if (n<>0) then
 begin
  thread_free(newtd);
  Exit(EINVAL);
 end;

 cpu_set_upcall_kse(newtd,func,arg,@stack);

 //jit wrapper
 switch_to_jit(newtd);
 //jit wrapper

 //seh wrapper
 seh_wrapper(newtd);
 //seh wrapper

 if (td<>nil) then
 begin
  newtd^.td_sigmask:=td^.td_sigmask;
 end;

 thread_link(newtd);

 newtd^.td_pflags:=newtd^.td_pflags or TDP_KTHREAD;

 if (name<>nil) then
 begin
  Move(name^,newtd^.td_name,SizeOf(t_td_name));
 end;
 KernSetThreadDebugName(newtd,'kern:');

 sched_fork_thread(td,newtd);

 tidhash_add(newtd);

 thread_inc_ref(newtd);

 n:=cpu_sched_add(newtd);
 if (n<>0) then
 begin
  cpu_thread_terminate(newtd);
  thread_dec_ref(newtd);
  thread_free(newtd);
  Exit(EFAULT);
 end;

 newtdp^:=newtd;
end;

function kern_thr_new(td:p_kthread;param:p_thr_param):Integer;
var
 rtp:t_rtprio;
 rtpp:p_rtprio;
 name:t_td_name;
begin
 Result:=0;
 rtpp:=nil;

 if (param^.rtp<>nil) then
 begin
  Result:=copyin(param^.rtp,@rtp,Sizeof(t_rtprio));
  if (Result<>0) then Exit;
  rtpp:=@rtp;
 end;

 name:=Default(t_td_name);

 if (param^.name<>nil) then
 begin
  Result:=copyinstr(param^.name,@name,32,nil);
  if (Result<>0) then Exit;
 end;

 Result:=create_thread(td,
                       nil,
                       param^.start_func,
                       param^.arg,
                       param^.stack_base,
                       param^.stack_size,
                       param^.tls_base,
                       param^.child_tid,
                       param^.parent_tid,
                       rtpp,
                       @name);
end;

function sys_thr_new(_param:Pointer;_size:Integer):Integer;
var
 param:thr_param;
begin
 if (_size<0) or (_size>Sizeof(thr_param)) then Exit(EINVAL);

 param:=Default(thr_param);

 Result:=copyin(_param,@param,_size);
 if (Result<>0) then Exit;

 Result:=kern_thr_new(curkthread,@param);
end;

function sys_thr_create(ctx:Pointer;id:PDWORD;flags:Integer):Integer;
var
 _ctx:ucontext_t;
begin
 Result:=copyin(ctx,@_ctx,sizeof(ucontext_t));
 if (Result<>0) then Exit;

 //flags ignored
 Result:=create_thread(curkthread,
                       @_ctx.uc_mcontext,
                       nil,
                       nil,
                       nil,
                       0,
                       nil,
                       id,
                       nil,
                       nil,
                       nil);

end;

procedure thread_exit;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit;

 ASSERT(TAILQ_EMPTY(@td^.td_sigqueue.sq_list),'signal pending');

 td^.td_state:=TDS_INACTIVE;

 tidhash_remove(td);
 thread_unlink(td);

 umtx_thread_exit(td);

 FreeMem(td^.td_jit_ctx);
 td^.td_jit_ctx:=nil;

 //free
 thread_dec_ref(td);

 cpu_sched_throw;
end;

function sys_thr_self(id:PDWORD):Integer;
var
 td:p_kthread;
begin
 if (id=nil) then Exit(EINVAL);

 td:=curkthread;
 if (td=nil) then Exit(EFAULT);

 Result:=suword32(id^,td^.td_tid);
 if (Result<>0) then Exit(EFAULT);

 Result:=0;
end;

procedure sys_thr_exit(state:PQWORD);
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit;

 if (state<>nil) then
 begin
  kern_umtx_wake(td,Pointer(state),High(Integer),0);
 end;

 tdsigcleanup(td);
 //thread_stopped(p);
 thread_reap();
 thread_exit();
 // NOTREACHED
end;

procedure kthread_exit();
begin
 thread_reap();
 thread_exit();
 // NOTREACHED
end;

function sys_thr_kill(id,sig:Integer):Integer;
var
 td,ttd:p_kthread;
 ksi:ksiginfo_t;
 i:kthread_iterator;
begin
 td:=curkthread;

 thread_reap();

 ksiginfo_init(@ksi);
 ksi.ksi_info.si_signo:=sig;
 ksi.ksi_info.si_code :=SI_LWP;
 ksi.ksi_info.si_pid  :=p_proc.p_pid;

 if (id=-1) then //all
 begin
  if (sig<>0) and (not _SIG_VALID(sig)) then
  begin
   Result:=EINVAL;
  end else
  begin
   Result:=ESRCH;
   PROC_LOCK;

   if FOREACH_THREAD_START(@i) then
   begin
    repeat
     ttd:=THREAD_GET(@i);
     if (ttd<>td) then
     begin
      Result:=0;
      if (sig=0) then Break;
      tdksignal(ttd,sig,@ksi);
     end;
    until not THREAD_NEXT(@i);
    FOREACH_THREAD_FINISH();
   end;

   PROC_UNLOCK;
  end;
 end else
 begin
  Result:=0;

  td:=tdfind(DWORD(id));
  if (td=nil) then Exit(ESRCH);

  if (sig=0) then
  begin
   //
  end else
  if (not _SIG_VALID(sig)) then
   Result:=EINVAL
  else
  begin
   PROC_LOCK;
   tdksignal(td,sig,@ksi);
   PROC_UNLOCK;
  end;

  thread_dec_ref(td);
 end;
end;

function sys_thr_kill2(pid,id,sig:Integer):Integer;
begin
 if (pid<>0) and (pid<>p_proc.p_pid) then
 begin
  Exit(ESRCH);
 end;

 Result:=sys_thr_kill(id,sig);
end;

function kern_thr_suspend(td:p_kthread;tsp:p_timespec):Integer;
var
 tv:Int64;
begin
 Result:=0;

 thread_reap();

 if ((td^.td_pflags and TDP_WAKEUP)<>0) then
 begin
  td^.td_pflags:=td^.td_pflags and (not TDP_WAKEUP);
  Exit(0);
 end;

 tv:=0;
 if (tsp<>nil) then
 begin
  if (tsp^.tv_sec=0) and (tsp^.tv_nsec=0) then
  begin
   Result:=EWOULDBLOCK;
  end else
  begin
   tv:=TIMESPEC_TO_UNIT(tsp);
   tv:=tvtohz(tv);
  end;
 end;

 PROC_LOCK;

 if (Result=0) and ((td^.td_flags and TDF_THRWAKEUP)=0) then
 begin
  Result:=msleep(td,@p_proc.p_mtx,PCATCH,'lthr',tv);
 end;

 if ((td^.td_flags and TDF_THRWAKEUP)<>0) then
 begin
  thread_lock(td);
  td^.td_flags:=td^.td_flags and (not TDF_THRWAKEUP);
  thread_unlock(td);
  PROC_UNLOCK;
  Exit(0);
 end;

 PROC_UNLOCK;

 if (Result=EWOULDBLOCK) then
 begin
  Result:=ETIMEDOUT;
 end else
 if (Result=ERESTART) then
 begin
  if (tv<>0) then
  begin
   Result:=EINTR;
  end;
 end;
end;

function sys_thr_suspend(timeout:Pointer):Integer;
var
 td:p_kthread;
 ts:timespec;
 tsp:p_timespec;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 tsp:=nil;
 if (timeout<>nil) then
 begin
  Result:=umtx_copyin_timeout(timeout,@ts);
  if (Result<>0) then Exit;
  tsp:=@ts;
 end;

 Result:=kern_thr_suspend(td,tsp);
end;

function sys_thr_wake(id:DWORD):Integer;
var
 td:p_kthread;
begin
 Result:=0;
 td:=curkthread;

 thread_reap();

 if (td<>nil) then
 if (id=td^.td_tid) then
 begin
  td^.td_pflags:=td^.td_pflags or TDP_WAKEUP;
  Exit(0);
 end;

 td:=tdfind(DWORD(id));
 if (td=nil) then Exit(ESRCH);

 thread_lock(td);
 td^.td_flags:=td^.td_flags or TDF_THRWAKEUP;
 thread_unlock(td);

 wakeup_td(td); //broadcast

 thread_dec_ref(td);
end;

function sys_thr_set_name(id:DWORD;pname:PChar):Integer;
var
 td:p_kthread;
 name:t_td_name;
begin
 Result:=0;

 thread_reap();

 name:=Default(t_td_name);
 if (name<>nil) then
 begin
  Result:=copyinstr(pname,@name,32,nil);
  if (Result<>0) then Exit;
 end;

 if (Integer(id)=-1) then
 begin
  //TODO SetProcName
  Exit;
 end;

 td:=tdfind(DWORD(id));
 if (td=nil) then Exit(ESRCH);

 thread_lock(td);

 td^.td_name:=name;
 KernSetThreadDebugName(td,'ps4:');

 thread_unlock(td);

 thread_dec_ref(td);
end;

function strnlen(s:PChar;maxlen:ptrint):ptrint;
var
 len:size_t;
begin
 For len:=0 to maxlen-1 do
 begin
  if (s^=#0) then Break;
  Inc(s);
 end;
 Exit(len);
end;

function sys_thr_get_name(id:DWORD;pname:PChar):Integer;
var
 td:p_kthread;
 name:t_td_name;
 len:ptrint;
begin
 Result:=0;

 thread_reap();

 td:=tdfind(DWORD(id));
 if (td=nil) then Exit(ESRCH);

 thread_lock(td);

 name:=td^.td_name;

 thread_unlock(td);

 len:=strnlen(name,31);
 Result:=copyout(@name,pname,len+1);

 thread_dec_ref(td);
end;

function sys_amd64_set_fsbase(base:Pointer):Integer;
var
 td:p_kthread;
begin
 Result:=0;
 td:=curkthread;
 if (td=nil) then Exit(-1);
 cpu_set_user_tls(td,base);
end;

function sys_amd64_get_fsbase(base:PPointer):Integer;
var
 td:p_kthread;
begin
 Result:=0;
 td:=curkthread;
 if (td=nil) or (base=nil) then Exit(-1);
 base^:=td^.pcb_fsbase;
end;

function sys_amd64_set_gsbase(base:Pointer):Integer;
var
 td:p_kthread;
begin
 Result:=0;
 td:=curkthread;
 if (td=nil) then Exit(-1);
 cpu_set_gsbase(td,base);
end;

function sys_amd64_get_gsbase(base:PPointer):Integer;
var
 td:p_kthread;
begin
 Result:=0;
 td:=curkthread;
 if (td=nil) or (base=nil) then Exit(-1);
 base^:=td^.pcb_gsbase;
end;

end.

