unit kern_thread;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 kern_thr,
 ntapi,
 windows,
 ucontext,
 signal,
 signalvar,
 time,
 rtprio,
 kern_rtprio,
 hamt;

function  thread_alloc:p_kthread;
procedure thread_free(td:p_kthread);

function  sys_thr_new(_param:p_thr_param;_size:Integer):Integer;
function  sys_thr_self(id:PDWORD):Integer;
procedure sys_thr_exit(state:PQWORD);
function  sys_thr_kill(id:DWORD;sig:Integer):Integer;
function  sys_thr_suspend(timeout:ptimespec):Integer;
function  sys_thr_wake(id:DWORD):Integer;
function  sys_thr_set_name(id:DWORD;pname:PChar):Integer;
function  sys_thr_get_name(id:DWORD;pname:PChar):Integer;

function  sys_amd64_set_fsbase(base:Pointer):Integer;

procedure thread_inc_ref(td:p_kthread);
procedure thread_dec_ref(td:p_kthread);
procedure thread_lock(td:p_kthread);
procedure thread_unlock(td:p_kthread);
function  tdfind(tid:DWORD):p_kthread;
procedure FOREACH_THREAD_IN_PROC(cb,userdata:Pointer);

function  SIGPENDING(td:p_kthread):Boolean;

procedure threadinit; //SYSINIT

implementation

{
64 48 A1 [0000000000000000] mov rax,fs:[$0000000000000000] -> 65 48 A1 [0807000000000000] mov rax,gs:[$0000000000000708]
64 48 8B 04 25 [00000000]   mov rax,fs:[$00000000]         -> 65 48 8B 04 25 [08070000]   mov rax,gs:[$00000708]
64 48 8B 0C 25 [00000000]   mov rcx,fs:[$00000000]         -> 65 48 8B 0C 25 [08070000]   mov rcx,gs:[$00000708]
64 48 8B 14 25 [00000000]   mov rdx,fs:[$00000000]         -> 65 48 8B 14 25 [08070000]   mov rdx,gs:[$00000708]
64 48 8B 1C 25 [00000000]   mov rbx,fs:[$00000000]         -> 65 48 8B 1C 25 [08070000]   mov rbx,gs:[$00000708]
64 48 8B 24 25 [00000000]   mov rsp,fs:[$00000000]         -> 65 48 8B 24 25 [08070000]   mov rsp,gs:[$00000708]
64 48 8B 2C 25 [00000000]   mov rbp,fs:[$00000000]         -> 65 48 8B 2C 25 [08070000]   mov rbp,gs:[$00000708]
64 48 8B 34 25 [00000000]   mov rsi,fs:[$00000000]         -> 65 48 8B 34 25 [08070000]   mov rsi,gs:[$00000708]
64 48 8B 3C 25 [00000000]   mov rdi,fs:[$00000000]         -> 65 48 8B 3C 25 [08070000]   mov rdi,gs:[$00000708]
64 4C 8B 04 25 [00000000]   mov r8 ,fs:[$00000000]         -> 65 4C 8B 04 25 [08070000]   mov r8 ,gs:[$00000708]
64 4C 8B 0C 25 [00000000]   mov r9 ,fs:[$00000000]         -> 65 4C 8B 0C 25 [08070000]   mov r9 ,gs:[$00000708]
64 4C 8B 14 25 [00000000]   mov r10,fs:[$00000000]         -> 65 4C 8B 14 25 [08070000]   mov r10,gs:[$00000708]
64 4C 8B 1C 25 [00000000]   mov r11,fs:[$00000000]         -> 65 4C 8B 1C 25 [08070000]   mov r11,gs:[$00000708]
64 4C 8B 24 25 [00000000]   mov r12,fs:[$00000000]         -> 65 4C 8B 24 25 [08070000]   mov r12,gs:[$00000708]
64 4C 8B 2C 25 [00000000]   mov r13,fs:[$00000000]         -> 65 4C 8B 2C 25 [08070000]   mov r13,gs:[$00000708]
64 4C 8B 34 25 [00000000]   mov r14,fs:[$00000000]         -> 65 4C 8B 34 25 [08070000]   mov r14,gs:[$00000708]
64 4C 8B 3C 25 [00000000]   mov r15,fs:[$00000000]         -> 65 4C 8B 3C 25 [08070000]   mov r15,gs:[$00000708]
}

uses
 errno,
 systm,
 vm_machdep,
 md_thread,
 kern_rwlock,
 kern_mtx,
 kern_umtx,
 kern_sig,
 sched_ule,
 subr_sleepqueue;

var
 tidhashtbl:TSTUB_HAMT32;
 tidhash_lock:Pointer=nil;

 p_numthreads:Integer=0;

 const
  max_threads_per_proc=1500;

function SIGPENDING(td:p_kthread):Boolean;
begin
 Result:=SIGNOTEMPTY(@td^.td_sigqueue.sq_signals) and
         sigsetmasked(@td^.td_sigqueue.sq_signals,@td^.td_sigmask);
end;

//

procedure threadinit;
begin
 FillChar(tidhashtbl,SizeOf(tidhashtbl),0);
end;

function thread_alloc:p_kthread;
var
 data:Pointer;
begin
 data:=AllocMem(SizeOf(kthread)+SizeOf(trapframe));

 Result:=data;

 data:=data+SizeOf(kthread);
 Result^.td_frame:=data;

 cpu_thread_alloc(Result);

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
 //
 FreeMem(td);
end;

procedure thread_inc_ref(td:p_kthread);
begin
 System.InterlockedIncrement(td^.td_ref);
end;

procedure thread_dec_ref(td:p_kthread);
begin
 if (System.InterlockedDecrement(td^.td_ref)=0) then
 begin
  thread_free(td);
 end;
end;

procedure thread_lock(td:p_kthread);
begin
 rw_wlock(td^.td_lock);
end;

procedure thread_unlock(td:p_kthread);
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

procedure FOREACH_THREAD_IN_PROC(cb,userdata:Pointer);
begin
 rw_wlock(tidhash_lock);

 HAMT_traverse32(@tidhashtbl,Tfree_data_cb(cb),userdata);

 rw_wunlock(tidhash_lock);
end;

function BaseQueryInfo(td:p_kthread):Integer;
var
 TBI:THREAD_BASIC_INFORMATION;
begin
 TBI:=Default(THREAD_BASIC_INFORMATION);

 Result:=NtQueryInformationThread(
           td^.td_handle,
           ThreadBasicInformation,
           @TBI,
           SizeOf(THREAD_BASIC_INFORMATION),
           nil);
 if (Result<>0) then Exit;

 td^.td_teb   :=TBI.TebBaseAddress;
 td^.td_cpuset:=TBI.AffinityMask;

 td^.td_teb^.thread:=td; //self
end;

procedure BaseInitializeStack(InitialTeb  :PINITIAL_TEB;
                              StackAddress:Pointer;
                              StackSize   :Ptruint); inline;
begin
 InitialTeb^.PreviousStackBase :=nil;
 InitialTeb^.PreviousStackLimit:=nil;
 InitialTeb^.StackBase         :=StackAddress+StackSize;  //start addr
 InitialTeb^.StackLimit        :=StackAddress;            //lo addr
 InitialTeb^.AllocatedStackBase:=StackAddress;            //lo addr
end;

procedure BaseInitializeContext(Context     :PCONTEXT;
                                Parameter   :Pointer;
                                StartAddress:Pointer;
                                StackAddress:Pointer); inline;
begin
 Context^:=Default(TCONTEXT);

 Context^.Rsp:=ptruint(StackAddress);
 Context^.Rbp:=ptruint(StackAddress);
 Context^.Rdi:=ptruint(Parameter);
 Context^.Rip:=ptruint(StartAddress);

 Context^.SegGs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegEs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegDs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegCs:=KGDT64_R3_CODE  or RPL_MASK;
 Context^.SegSs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegFs:=KGDT64_R3_CMTEB or RPL_MASK;

 Context^.EFlags:=$3000 or EFLAGS_INTERRUPT_MASK;

 Context^.MxCsr:=INITIAL_MXCSR;

 Context^.ContextFlags:=CONTEXT_THREAD;
end;

function create_thread(td        :p_kthread; //calling thread
                       ctx       :Pointer;
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
label
 _term;
var
 newtd:p_kthread;

 _ClientId  :array[0..SizeOf(TCLIENT_ID  )+14] of Byte;
 _InitialTeb:array[0..SizeOf(TINITIAL_TEB)+14] of Byte;
 _Context   :array[0..SizeOf(TCONTEXT    )+14] of Byte;

 ClientId  :PCLIENT_ID;
 InitialTeb:PINITIAL_TEB;
 Context   :PCONTEXT;

 Stack:Pointer;

 n:Integer;
begin

 if (p_numthreads>=max_threads_per_proc) then
 begin
  Exit(EPROCLIM);
 end;

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

 if (ctx<>nil) then Exit(EINVAL);

 if (ptruint(stack_base)<$1000) or (stack_size<$1000) then Exit(EINVAL);

 newtd:=thread_alloc;
 if (newtd=nil) then Exit(ENOMEM);

 ClientId  :=Align(@_ClientId  ,16);
 InitialTeb:=Align(@_InitialTeb,16);
 Context   :=Align(@_Context   ,16);

 ClientId^.UniqueProcess:=NtCurrentProcess;
 ClientId^.UniqueThread :=NtCurrentThread;

 BaseInitializeStack(InitialTeb,stack_base,stack_size);

 Stack:=InitialTeb^.StackBase;
 Stack:=Pointer((ptruint(Stack) and (not $F)){-Sizeof(Pointer)});

 BaseInitializeContext(Context,
                       arg,
                       start_func,
                       Stack);

 n:=NtCreateThread(
           @newtd^.td_handle,
           THREAD_ALL_ACCESS,
           nil,
           NtCurrentProcess,
           ClientId,
           Context,
           InitialTeb,
           True);

 if (n<>0) then
 begin
  thread_free(newtd);
  Exit(EINVAL);
 end;

 newtd^.td_tid:=DWORD(ClientId^.UniqueThread);

 if (BaseQueryInfo(newtd)<>0) then
 begin
  _term:
  NtTerminateThread(newtd^.td_handle,n);
  NtClose(newtd^.td_handle);

  thread_free(newtd);
  Exit(EFAULT);
 end;

 cpu_set_user_tls(newtd,tls_base);

 if (child_tid<>nil) then
 begin
  n:=suword32(child_tid^,newtd^.td_tid);
  if (n<>0) then Goto _term;
 end;

 if (parent_tid<>nil) then
 begin
  n:=suword32(parent_tid^,newtd^.td_tid);
  if (n<>0) then Goto _term;
 end;

 if (td<>nil) then
 begin
  newtd^.td_sigmask:=td^.td_sigmask;
 end;

 thread_link(newtd);

 if (name<>nil) then
 begin
  Move(name^,newtd^.td_name,SizeOf(t_td_name));
 end;
 SetThreadDebugName(newtd^.td_handle,'ps4:'+newtd^.td_name);

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

 newtd^.td_state:=TDS_RUNNING;
 NtResumeThread(newtd^.td_handle,nil);
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

function sys_thr_new(_param:p_thr_param;_size:Integer):Integer;
var
 param:thr_param;
begin
 if (_size<0) or (_size>Sizeof(thr_param)) then Exit(EINVAL);

 param:=Default(thr_param);

 Result:=copyin(_param,@param,_size);
 if (Result<>0) then Exit;

 Result:=kern_thr_new(curkthread,@param);
end;

procedure thread_exit;
var
 td:p_kthread;
 rsp:QWORD;
begin
 td:=curkthread;
 if (td=nil) then Exit;

 ASSERT(TAILQ_EMPTY(@td^.td_sigqueue.sq_list),'signal pending');

 td^.td_state:=TDS_INACTIVE;

 thread_inc_ref(td);
 tidhash_remove(td);
 thread_unlink(td);

 NtClose(td^.td_handle);

 umtx_thread_exit(td);

 //switch to userstack
 rsp:=td^.td_frame^.tf_rsp;
 if (rsp<>0) then
 asm
  mov rsp,%rsp
 end;

 //free
 thread_dec_ref(td);

 RtlExitUserThread(0);
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
 thread_exit();
 // NOTREACHED
end;

type
 p_t_stk=^_t_stk;
 _t_stk=record
   error:Integer;
   sig:Integer;
   td:p_kthread;
   ksi:ksiginfo_t;
 end;

procedure _for_stk(td:p_kthread;data:p_t_stk); register; //Tfree_data_cb
begin
 if (td<>data^.td) then
 begin
  data^.error:=0;
  if (data^.sig=0) then Exit;
  tdksignal(td,data^.sig,@data^.ksi);
 end;
end;

function sys_thr_kill(id:DWORD;sig:Integer):Integer;
var
 data:_t_stk;
begin
 data.td:=curkthread;

 ksiginfo_init(@data.ksi);
 data.ksi.ksi_info.si_signo:=sig;
 data.ksi.ksi_info.si_code :=SI_LWP;

 if (Integer(id)=-1) then
 begin
  if (sig<>0) and (not _SIG_VALID(sig)) then
  begin
   Result:=EINVAL;
  end else
  begin
   data.error:=ESRCH;
   data.sig:=0;
   PROC_LOCK;
   FOREACH_THREAD_IN_PROC(@_for_stk,@data);
   PROC_UNLOCK;
   Result:=data.error;
  end;
 end else
 begin
  Result:=0;

  data.td:=tdfind(DWORD(id));
  if (data.td=nil) then Exit(ESRCH);

  if (sig=0) then
  begin
   //
  end else
  if (not _SIG_VALID(sig)) then
   Result:=EINVAL
  else
   tdksignal(data.td,sig,@data.ksi);

  thread_dec_ref(data.td);
 end;
end;

function kern_thr_suspend(td:p_kthread;tsp:ptimespec):Integer;
var
 tv:Int64;
begin
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
  PROC_UNLOCK; //
  Result:=msleep_td(tv);
  PROC_LOCK;   //
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

function sys_thr_suspend(timeout:ptimespec):Integer;
var
 td:p_kthread;
 ts:timespec;
 tsp:ptimespec;
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
 SetThreadDebugName(td^.td_handle,'ps4:'+name);

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
 if (td=nil) then Exit(EFAULT);
 cpu_set_user_tls(td,base);
end;

end.

