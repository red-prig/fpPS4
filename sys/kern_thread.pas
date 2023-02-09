unit kern_thread;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 windows,
 sys_kernel;

const
 PRI_ITHD     =1; // Interrupt thread.
 PRI_REALTIME =2; // Real time process.
 PRI_TIMESHARE=3; // Time sharing process.
 PRI_IDLE     =4; // Idle process.
 PRI_FIFO     =10;

 PRI_MIN=0;
 PRI_MAX=960;

 PRI_MIN_TIMESHARE=256;
 PRI_MAX_TIMESHARE=767;

 PUSER=PRI_MIN_TIMESHARE;

 RTP_PRIO_REALTIME =PRI_REALTIME;
 RTP_PRIO_NORMAL   =PRI_TIMESHARE;
 RTP_PRIO_IDLE     =PRI_IDLE;
 RTP_PRIO_FIFO     =PRI_FIFO;

 THR_SUSPENDED=$0001;

type
 p_kthread=^kthread;
 kthread=packed record
  td_umtxq        :Pointer; //p_umtx_q
  td_handle       :THandle; //nt thread
  td_tid          :DWORD;
  td_ref          :Integer;
  td_lock         :Integer;
  td_priority     :Word;
  td_pri_class    :Word;
  td_base_pri     :Word;
  td_base_user_pri:Word;
  td_lend_user_pri:Word;
  td_user_pri     :Word;
  td_name         :array[0..31] of AnsiChar;
  //
  td_fsbase       :Pointer;
  td_cpuset       :Ptruint;
 end;

 p_rtprio=^rtprio;
 rtprio=packed record
  _type:Word;
  _prio:Word;
 end;

 p_thr_param=^thr_param;
 thr_param=packed record
  start_func:Pointer;
  arg       :Pointer;
  stack_base:Pointer;
  stack_size:Ptruint;
  tls_base  :Pointer;
  tls_size  :Ptruint;
  child_tid :PDWORD;
  parent_tid:PDWORD;
  flags     :Integer;
  align     :Integer;
  rtp       :p_rtprio;
  name      :PChar;
  spare     :array[0..1] of Pointer;
 end;

procedure sched_fork_thread(td,childtd:p_kthread);
procedure sched_class(td:p_kthread;_class:Integer);
function  sched_priority(td:p_kthread;prio:Integer):Integer;
procedure sched_prio(td:p_kthread;prio:Integer);
procedure sched_user_prio(td:p_kthread;prio:Integer);
procedure sched_lend_user_prio(td:p_kthread;prio:Integer);

function  rtp_to_pri(rtp:p_rtprio;td:p_kthread):Integer;
procedure pri_to_rtp(td:p_kthread;rtp:p_rtprio);

function  create_thread(td        :p_kthread; //calling thread
                        ctx       :Pointer;
                        start_func:Pointer;
                        arg       :Pointer;
                        stack_base:Pointer;
                        stack_size:QWORD;
                        tls_base  :Pointer;
                        child_tid :PDWORD;
                        parent_tid:PDWORD;
                        flags     :Integer;
                        rtp       :p_rtprio;
                        name      :PChar
                       ):Integer;

function  sys_thr_new(td:p_kthread;_param:p_thr_param;_size:Integer):Integer;

procedure thread_exit;

procedure thread_inc_ref(td:p_kthread);
procedure thread_dec_ref(td:p_kthread);
procedure thread_lock(td:p_kthread);
procedure thread_unlock(td:p_kthread);
function  tdfind(tid:DWORD):p_kthread;

function  curkthread:p_kthread; assembler;
procedure set_curkthread(td:p_kthread); assembler;

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
 hamt,
 systm,
 kern_lock,
 kern_umtx;

var
 tidhashtbl:TSTUB_HAMT32;
 tidhash_lock:Integer=0;

 p_numthreads:Integer=0;

 const
  max_threads_per_proc=1500;

function curkthread:p_kthread; assembler; nostackframe;
asm
 movqq %gs:(0x700),Result
end;

procedure set_curkthread(td:p_kthread); assembler; nostackframe;
asm
 movqq td,%gs:(0x700)
end;

procedure threadinit; inline;
begin
 FillChar(tidhashtbl,SizeOf(tidhashtbl),0);
end;

function thread_alloc:p_kthread;
begin
 Result:=AllocMem(SizeOf(kthread));

 //Result^.td_state:=TDS_INACTIVE;
 Result^.td_lend_user_pri:=PRI_MAX;

 //
 umtx_thread_init(Result);
end;

procedure thread_free(td:p_kthread); inline;
begin
 umtx_thread_fini(td);
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
 klock(td^.td_lock);
end;

procedure thread_unlock(td:p_kthread);
begin
 kunlock(td^.td_lock);
end;

procedure thread_link(td:p_kthread);
begin
 //td^.td_state:=TDS_INACTIVE;
 //sigqueue_init(&td->td_sigqueue, p);

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
 klock(tidhash_lock); //rdlock

 data:=HAMT_search32(@tidhashtbl,tid);

 if (data<>nil) then
 begin
  Result:=data^;
 end;

 if (Result<>nil) then
 begin
  thread_inc_ref(Result);
 end;

 kunlock(tidhash_lock);
end;

procedure tidhash_add(td:p_kthread);
var
 data:PPointer;
begin
 klock(tidhash_lock); //wlock

 data:=HAMT_insert32(@tidhashtbl,td^.td_tid,td);

 if (data<>nil) then
 begin
  if (data^=td) then
  begin
   thread_inc_ref(td);
  end;
 end;

 kunlock(tidhash_lock);
end;

procedure tidhash_remove(td:p_kthread);
var
 data:Pointer;
begin
 klock(tidhash_lock); //wlock

 data:=HAMT_delete32(@tidhashtbl,td^.td_tid);

 kunlock(tidhash_lock);

 if (data=td) then
 begin
  thread_dec_ref(td);
 end;
end;

function cpuset_setaffinity(td:p_kthread;new:Ptruint):Integer;
begin
 td^.td_cpuset:=new;
 Result:=NtSetInformationThread(td^.td_handle,ThreadAffinityMask,@new,SizeOf(Ptruint));
end;

function cpu_set_user_tls(td:p_kthread;base:Pointer):Integer;
var
 TBI:THREAD_BASIC_INFORMATION;
 pcur:PPointer;
 ptls:PPointer;
begin
 td^.td_fsbase:=base;

 TBI:=Default(THREAD_BASIC_INFORMATION);

 Result:=NtQueryInformationThread(
           td^.td_handle,
           ThreadBasicInformation,
           @TBI,
           SizeOf(THREAD_BASIC_INFORMATION),
           nil);
 if (Result<>0) then Exit;

 td^.td_cpuset:=TBI.AffinityMask;

 pcur:=TBI.TebBaseAddress+$700;
 ptls:=TBI.TebBaseAddress+$708;

 pcur^:=td;
 ptls^:=base;
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
                       flags     :Integer;
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

 if (cpu_set_user_tls(newtd,tls_base)<>0) then
 begin
  _term:
  NtTerminateThread(newtd^.td_handle,n);
  NtClose(newtd^.td_handle);

  thread_free(newtd);
  Exit(EFAULT);
 end;

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

 //newtd->td_sigmask = td->td_sigmask;
 thread_link(newtd);

 if (name<>nil) then
 begin
  Move(name^,newtd^.td_name,SizeOf(newtd^.td_name));
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

 if ((flags and THR_SUSPENDED)=0) then
 begin
  NtResumeThread(newtd^.td_handle,nil);
 end;

end;

function kern_thr_new(td:p_kthread;param:p_thr_param):Integer;
var
 rtp:rtprio;
 rtpp:p_rtprio;
 name:array[0..31] of AnsiChar;
begin
 Result:=0;
 rtpp:=nil;

 if (param^.rtp<>nil) then
 begin
  Result:=copyin(param^.rtp,@rtp,Sizeof(rtprio));
  if (Result<>0) then Exit(EFAULT);
  rtpp:=@rtp;
 end;

 name[0]:=#0;

 if (param^.name<>nil) then
 begin
  Result:=copyinstr(param^.name,@name,32,nil);
  if (Result<>0) then Exit(EFAULT);
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
                       param^.flags,
                       rtpp,
                       @name);
end;

function sys_thr_new(td:p_kthread;_param:p_thr_param;_size:Integer):Integer;
var
 param:thr_param;
begin
 if (_size<0) or (_size>Sizeof(thr_param)) then Exit(EINVAL);

 param:=Default(thr_param);

 Result:=copyin(_param,@param,_size);
 if (Result<>0) then Exit(EFAULT);

 Result:=kern_thr_new(td,@param);
end;

procedure thread_exit;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit;

 //KASSERT(TAILQ_EMPTY(&td->td_sigqueue.sq_list), ("signal pending"));

 //td^.td_state:=TDS_INACTIVE;

 umtx_thread_exit(td);

 thread_dec_ref(td);

 RtlExitUserThread(0);
end;







function rtp_to_pri(rtp:p_rtprio;td:p_kthread):Integer;
var
 newpri:Integer;
begin

 Case (rtp^._type and $fff7) of //RTP_PRIO_BASE
  RTP_PRIO_IDLE:
    begin
     newpri:=960;
     if (rtp^._prio<>960) then Exit(EINVAL);
    end;
  RTP_PRIO_NORMAL:
    begin
     newpri:=rtp^._prio;
     if (newpri>959) then Exit(EINVAL);
    end;
  PRI_REALTIME:
    begin
     newpri:=rtp^._prio;
     if (newpri>767) then Exit(EINVAL);
    end;
  else
    Exit(EINVAL)
 end;

 thread_lock(td);
 sched_class(td,rtp^._type);
 sched_user_prio(td, newpri);

 if (td=curkthread) then
 begin
  sched_prio(td,td^.td_user_pri);
 end;

 thread_unlock(td);
end;

procedure pri_to_rtp(td:p_kthread;rtp:p_rtprio);
begin
 thread_lock(td);

 case (td^.td_pri_class and $fff7) of //PRI_BASE
  PRI_REALTIME,
  RTP_PRIO_NORMAL,
  PRI_IDLE:
    begin
     rtp^._prio:=td^.td_base_user_pri;
    end;
  else;
 end;
 rtp^._type:=td^.td_pri_class;

 thread_unlock(td);
end;

procedure sched_fork_thread(td,childtd:p_kthread);
begin
 if (td<>nil) then
 begin
  cpuset_setaffinity(childtd,td^.td_cpuset);
  sched_priority(td,td^.td_base_pri);
 end;
end;

procedure sched_class(td:p_kthread;_class:Integer); inline;
begin
 td^.td_pri_class:=_class;
end;

function sched_priority(td:p_kthread;prio:Integer):Integer;
begin
 td^.td_priority:=prio;

 Case prio of
    0..255:prio:= 16;
  256..496:prio:= 2;
  497..526:prio:= 1;
  527..556:prio:= 0;
  557..586:prio:=-1;
  587..767:prio:=-2;
  else
           prio:=-16;
 end;

 Result:=NtSetInformationThread(td^.td_handle,ThreadBasePriority,@prio,SizeOf(Integer));
end;

procedure sched_prio(td:p_kthread;prio:Integer); inline;
begin
 td^.td_base_pri:=prio;
 sched_priority(td, prio);
end;

procedure sched_user_prio(td:p_kthread;prio:Integer); inline;
begin
 td^.td_base_user_pri:=prio;
 if (td^.td_lend_user_pri<=prio) then Exit;
 td^.td_user_pri:=prio;
end;

function min(a,b:Integer):Integer; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

procedure sched_lend_user_prio(td:p_kthread;prio:Integer);
begin
 td^.td_lend_user_pri:=prio;
 td^.td_user_pri:=min(prio,td^.td_base_user_pri);
 if (td^.td_priority>td^.td_user_pri) then
 begin
  sched_prio(td,td^.td_user_pri);
 end;
end;






initialization
 threadinit;

end.

