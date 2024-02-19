unit md_thread;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 windows,
 kern_thr,
 sysutils;

Const
 SYS_STACK_RSRV=64*1024;
 SYS_STACK_SIZE=16*1024;

function  cpu_thread_alloc():p_kthread;
function  cpu_thread_free(td:p_kthread):Integer;

function  BaseQueryInfo(td:p_kthread):Integer;

function  cpu_thread_create(td:p_kthread;
                            stack_base:Pointer;
                            stack_size:QWORD;
                            start_func:Pointer;
                            arg       :Pointer):Integer;
procedure cpu_thread_terminate(td:p_kthread);
function  cpu_sched_add(td:p_kthread):Integer;
procedure cpu_sched_throw;
function  cpu_thread_finished(td:p_kthread):Boolean;

function  cpuset_setaffinity(td:p_kthread;new:Ptruint):Integer;
function  cpu_set_priority(td:p_kthread;prio:Integer):Integer;

function  cpu_thread_set_name(td:p_kthread;const name:shortstring):Integer;

function  md_suspend(td:p_kthread):Integer;
function  md_resume (td:p_kthread):Integer;

procedure seh_wrapper_before(td:p_kthread;var func:Pointer);
procedure seh_wrapper_after (td:p_kthread;func:Pointer);

implementation

//

var
 size_of_umtx_q:Integer; external;

//

function cpu_thread_alloc():p_kthread;
var
 td:p_kthread;
 data:Pointer;
 size:ULONG_PTR;
 R:DWORD;
begin
 Result:=nil;

 data:=nil;
 size:=SYS_STACK_RSRV;

 R:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @data,
     0,
     @size,
     MEM_RESERVE,
     PAGE_READWRITE
    );
 if (R<>0) then Exit;

 //header
 size:=SizeOf(kthread)+size_of_umtx_q;
 size:=System.Align(size,4*1024);

 R:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @data,
     0,
     @size,
     MEM_COMMIT,
     PAGE_READWRITE
    );
 if (R<>0) then Exit;

 td:=data;
 td^.td_umtxq:=Pointer(td+1);

 //footer
 data:=data+SYS_STACK_RSRV-SYS_STACK_SIZE;
 size:=SYS_STACK_SIZE;

 R:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @data,
     0,
     @size,
     MEM_COMMIT,
     PAGE_READWRITE
    );
 if (R<>0) then Exit;

 td^.td_kstack.sttop:=data;

 data:=data+SYS_STACK_SIZE;
 td^.td_kstack.stack:=data;

 Result:=td;
end;

function cpu_thread_free(td:p_kthread):Integer;
var
 data:Pointer;
 size:ULONG_PTR;
begin
 if (td=nil) then Exit(0);

 data:=td;
 size:=0;

 Result:=NtFreeVirtualMemory(
           NtCurrentProcess,
           @data,
           @size,
           MEM_RELEASE
          );
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

function cpu_thread_create(td:p_kthread;
                           stack_base:Pointer;
                           stack_size:QWORD;
                           start_func:Pointer;
                           arg       :Pointer):Integer;
var
 _ClientId  :array[0..SizeOf(TCLIENT_ID  )+14] of Byte;
 _InitialTeb:array[0..SizeOf(TINITIAL_TEB)+14] of Byte;
 _Context   :array[0..SizeOf(TCONTEXT    )+14] of Byte;

 ClientId  :PCLIENT_ID;
 InitialTeb:PINITIAL_TEB;
 Context   :PCONTEXT;

 Stack:Pointer;
begin
 if (td=nil) then Exit(-1);

 ClientId  :=Align(@_ClientId  ,16);
 InitialTeb:=Align(@_InitialTeb,16);
 Context   :=Align(@_Context   ,16);

 ClientId^.UniqueProcess:=NtCurrentProcess;
 ClientId^.UniqueThread :=NtCurrentThread;

 BaseInitializeStack(InitialTeb,stack_base,stack_size);

 //use kernel stack to init
 Stack:=td^.td_kstack.stack;
 Stack:=Pointer((ptruint(Stack) and (not $F)));

 BaseInitializeContext(Context,
                       arg,
                       start_func,
                       Stack);

 Result:=NtCreateThread(
          @td^.td_handle,
          THREAD_ALL_ACCESS,
          nil,
          NtCurrentProcess,
          ClientId,
          Context,
          InitialTeb,
          True);

 if (Result=0) then
 begin
  td^.td_tid:=DWORD(ClientId^.UniqueThread);

  Result:=BaseQueryInfo(td);

  if (Result<>0) then
  begin
   cpu_thread_terminate(td);
  end;
 end;
end;

procedure cpu_thread_terminate(td:p_kthread);
begin
 if (td=nil) then Exit;
 if (td^.td_handle=0) or (td^.td_handle=THandle(-1)) then Exit;
 NtTerminateThread(td^.td_handle,0);
 NtClose(td^.td_handle);
 td^.td_handle:=0;
 td^.td_tid:=0;
end;

function cpu_sched_add(td:p_kthread):Integer;
begin
 if (td=nil) then Exit(-1);
 if (td^.td_handle=0) or (td^.td_handle=THandle(-1)) then Exit(-1);

 td^.td_state:=TDS_RUNNING;
 Result:=NtResumeThread(td^.td_handle,nil);

 if (Result<>0) then
 begin
  td^.td_state:=TDS_INACTIVE;
 end;
end;

procedure cpu_sched_throw;
begin
 RtlExitUserThread(0);
end;

function cpu_thread_finished(td:p_kthread):Boolean;
var
 R:DWORD;
 T:QWORD;
begin
 Result:=True;
 if (td=nil) then Exit;
 if (td^.td_handle=0) or (td^.td_handle=THandle(-1)) then Exit;

 T:=0;
 R:=NtWaitForSingleObject(td^.td_handle,False,@T);

 Result:=(R=STATUS_WAIT_0);

 if Result then
 begin
  NtClose(td^.td_handle);
  td^.td_handle:=0;
  td^.td_tid:=0;
 end;
end;

function cpuset_setaffinity(td:p_kthread;new:Ptruint):Integer;
begin
 if (td=nil) then Exit;
 if (td^.td_handle=0) or (td^.td_handle=THandle(-1)) then Exit(-1);

 td^.td_cpuset:=new;
 Result:=NtSetInformationThread(td^.td_handle,ThreadAffinityMask,@new,SizeOf(Ptruint));
end;

function cpu_set_priority(td:p_kthread;prio:Integer):Integer;
begin
 if (td=nil) then Exit;
 if (td^.td_handle=0) or (td^.td_handle=THandle(-1)) then Exit(-1);

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

function cpu_thread_set_name(td:p_kthread;const name:shortstring):Integer;
var
 W:array[0..255] of WideChar;
 UNAME:UNICODE_STRING;
 L:DWORD;
begin
 Result:=0;
 if (td=nil) then Exit;
 if (td^.td_handle=0) or (td^.td_handle=THandle(-1)) then Exit;

 L:=Utf8ToUnicode(@W,length(W),@name[1],length(name));

 W:=UTF8Decode(name);

 UNAME.Length       :=L*SizeOf(WideChar);
 UNAME.MaximumLength:=UNAME.Length;
 UNAME.Buffer       :=PWideChar(W);

 Result:=NtSetInformationThread(td^.td_handle,ThreadNameInformation,@UNAME,SizeOf(UNAME));
end;

function md_suspend(td:p_kthread):Integer;
var
 count:ULONG;
begin
 Result:=0;
 if (td=nil) then Exit;
 if (td^.td_handle=0) or (td^.td_handle=THandle(-1)) then Exit;

 count:=0;
 NtSuspendThread(td^.td_handle,@count);

 Result:=count;
end;

function md_resume(td:p_kthread):Integer;
var
 count:ULONG;
begin
 Result:=0;
 if (td=nil) then Exit;
 if (td^.td_handle=0) or (td^.td_handle=THandle(-1)) then Exit;

 count:=0;
 NtResumeThread(td^.td_handle,@count);

 Result:=count;
end;

procedure main_wrapper; assembler; nostackframe;
asm
 subq   $40, %rsp
.seh_stackalloc 40
.seh_endprologue

 call %gs:teb.jitcall

 nop
 addq   $40, %rsp
.seh_handler __FPC_default_handler,@except,@unwind
end;

procedure seh_wrapper_before(td:p_kthread;var func:Pointer);
begin
 func:=@main_wrapper;
end;

procedure seh_wrapper_after(td:p_kthread;func:Pointer);
begin
 td^.td_teb^.jitcall:=func;
end;

end.

