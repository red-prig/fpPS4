unit md_thread;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

{$DEFINE NT_THREAD}
{$DEFINE CSRSRV}

interface

{$IFDEF NT_THREAD}
 //
{$ELSE}
 {$UNDEF CSRSRV}
{$ENDIF}

uses
 ntapi,
 windows,
 kern_thr,
 sysutils;

Const
 SYS_STACK_RSRV=64*1024;
 SYS_STACK_SIZE=16*1024;
 SYS_GUARD_SIZE= 4*1024;
 SYS_CACHE_SIZE=64*1024;

function  cpu_thread_alloc(pages:Word):p_kthread;
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
function  cpu_set_priority  (td:p_kthread;prio:Integer):Integer;

function  cpu_thread_set_name(td:p_kthread;const name:shortstring):Integer;

function  md_suspend(td:p_kthread):Integer;
function  md_resume (td:p_kthread):Integer;

procedure seh_wrapper_before(td:p_kthread;var func:Pointer);
procedure seh_wrapper_after (td:p_kthread;func:Pointer);

implementation

uses
 md_context,
 vmparam;

//

var
 size_of_umtx_q:Integer; external;

//

function cpu_thread_alloc(pages:Word):p_kthread;
var
 td        :p_kthread;
 stack_size:ULONG_PTR;
 headr_size:ULONG_PTR;
 padding   :ULONG_PTR;
 data      :Pointer;
 size      :ULONG_PTR;
 R         :DWORD;
begin
 Result:=nil;

 stack_size:=ULONG_PTR(pages)*PAGE_SIZE;
 if (stack_size<SYS_STACK_SIZE) then
 begin
  stack_size:=SYS_STACK_SIZE;
 end;

 headr_size:=SizeOf(kthread)+size_of_umtx_q;
 size      :=System.Align(headr_size,4*1024);
 padding   :=size-headr_size;
 headr_size:=size;

 size:=SYS_CACHE_SIZE+headr_size+SYS_GUARD_SIZE+stack_size;
 size:=System.Align(size,64*1024);

 data:=nil;

 //reserve
 R:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @data,
     0,
     @size,
     MEM_RESERVE,
     PAGE_READWRITE
    );
 if (R<>0) then Exit;

 //cache
 data:=data+SYS_CACHE_SIZE;
 size:=size-SYS_CACHE_SIZE;

 //header
 R:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @data,
     0,
     @headr_size,
     MEM_COMMIT,
     PAGE_READWRITE
    );
 if (R<>0) then Exit;

 td:=data;
 td^.td_umtxq:=Pointer(td+1);

 //footer
 data:=data+size-stack_size;

 //kernel stack
 R:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @data,
     0,
     @stack_size,
     MEM_COMMIT,
     PAGE_READWRITE
    );
 if (R<>0) then Exit;

 td^.td_kstack.sttop:=data;
 td^.td_kstack.stack:=data+stack_size;

 td^.td_padding.addr:=Pointer(td)+(headr_size-padding);
 td^.td_padding.size:=padding;

 Result:=td;
end;

function cpu_thread_free(td:p_kthread):Integer;
var
 data:Pointer;
 size:ULONG_PTR;
begin
 if (td=nil) then Exit(0);

 //cache
 data:=Pointer(td)-SYS_CACHE_SIZE;
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
 data:array[0..SizeOf(THREAD_BASIC_INFORMATION)-1+7] of Byte;
 P_TBI:PTHREAD_BASIC_INFORMATION;
begin
 P_TBI:=Align(@data,8);
 P_TBI^:=Default(THREAD_BASIC_INFORMATION);

 Result:=NtQueryInformationThread(
           td^.td_handle,
           ThreadBasicInformation,
           P_TBI,
           SizeOf(THREAD_BASIC_INFORMATION),
           nil);
 if (Result<>0) then Exit;

 if (P_TBI^.TebBaseAddress=nil) then Exit(-1);

 td^.td_teb   :=P_TBI^.TebBaseAddress;
 td^.td_cpuset:=P_TBI^.AffinityMask;

 td^.td_teb^.thread:=td; //self
 td^.td_teb^.DeallocationStack:=nil; //dont free memory
end;

procedure BaseInitializeStack(InitialTeb  :PINITIAL_TEB;
                              StackAddress:Pointer;
                              StackSize   :Ptruint); inline;
begin
 InitialTeb^.PreviousStackBase :=nil;
 InitialTeb^.PreviousStackLimit:=nil;
 InitialTeb^.StackBase         :=StackAddress+StackSize;  //start addr
 InitialTeb^.StackLimit        :=StackAddress;            //lo addr
 InitialTeb^.AllocatedStackBase:=StackAddress;            //DeallocationStack
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

 Context^.MxCsr:=__INITIAL_MXCSR__;

 Context^.FltSave.ControlWord:=__INITIAL_FPUCW__;
 //Context^.FltSave.StatusWord: WORD;
 Context^.FltSave.MxCsr      :=__INITIAL_MXCSR__;
 Context^.FltSave.MxCsr_Mask :=__INITIAL_MXCSR_MASK__;

 Context^.ContextFlags:=CONTEXT_THREAD;
end;

{$IFDEF CSRSRV}
type
 t_GetProcAddressForCaller=function(hModule   :HINST;
                                    lpProcName:LPCSTR;
                                    Param3    :Pointer):Pointer; //KernelBase.dll

 t_CsrCreateRemoteThread=function(hThread :THANDLE;
                                  ClientId:PCLIENT_ID):DWORD; //csrsrv

function _CsrCreateRemoteThread(hThread:THANDLE;ClientId:PCLIENT_ID):DWORD;
var
 GetProcAddressForCaller:t_GetProcAddressForCaller;
 CsrCreateRemoteThread  :t_CsrCreateRemoteThread;
begin
 Result:=0;

 Pointer(GetProcAddressForCaller):=GetProcAddress(GetModuleHandle('kernelbase.dll'),'GetProcAddressForCaller');
 CsrCreateRemoteThread  :=nil;

 //Writeln('csrsrv.dll:0x',HexStr(GetModuleHandle('csrsrv.dll'),16));
 //Writeln('csrsrv:0x',HexStr(GetModuleHandle('csrsrv'),16));

 if (GetProcAddressForCaller<>nil) then
 begin
  Pointer(CsrCreateRemoteThread):=GetProcAddressForCaller(GetModuleHandle('csrsrv.dll'),'CsrCreateRemoteThread',nil);
 end;

 if (CsrCreateRemoteThread=nil) then
 begin
  Pointer(CsrCreateRemoteThread):=GetProcAddress(GetModuleHandle('csrsrv.dll'),'CsrCreateRemoteThread');
 end;

 if (CsrCreateRemoteThread<>nil) then
 begin
  Result:=CsrCreateRemoteThread(hThread,ClientId);
 end;

 //Writeln('GetProcAddressForCaller:0x',HexStr(GetProcAddressForCaller));
 //Writeln('CsrCreateRemoteThread  :0x',HexStr(CsrCreateRemoteThread));
end;
{$ENDIF}

function cpu_thread_create(td:p_kthread;
                           stack_base:Pointer;
                           stack_size:QWORD;
                           start_func:Pointer;
                           arg       :Pointer):Integer;
{$IFDEF NT_THREAD}
var
 _ClientId  :array[0..SizeOf(TCLIENT_ID  )+14] of Byte;
 _InitialTeb:array[0..SizeOf(TINITIAL_TEB)+14] of Byte;
 _Context   :array[0..SizeOf(TCONTEXT    )+14] of Byte;

 ClientId  :PCLIENT_ID;
 InitialTeb:PINITIAL_TEB;
 Context   :PCONTEXT;

 Stack     :Pointer;
{$ENDIF}
begin
 if (td=nil) then Exit(-1);

 {$IFDEF NT_THREAD}
  ClientId  :=Align(@_ClientId  ,16);
  InitialTeb:=Align(@_InitialTeb,16);
  Context   :=Align(@_Context   ,16);

  ClientId^.UniqueProcess:=NtCurrentProcess;
  ClientId^.UniqueThread :=NtCurrentThread;

  BaseInitializeStack(InitialTeb,stack_base,stack_size);

  Stack:=stack_base+stack_size;
  Stack:=Pointer((ptruint(Stack) and (not $F)));

  BaseInitializeContext(Context,
                        arg,
                        start_func,
                        Stack);
 {$ENDIF}

 {$IFDEF NT_THREAD}
  //Writeln('NtCreateThread');
  Result:=NtCreateThread(
           @td^.td_handle,
           THREAD_ALL_ACCESS,
           nil,
           NtCurrentProcess,
           ClientId,
           Context,
           InitialTeb,
           True);
 {$ELSE}
  //Writeln('CreateThread');
  td^.td_handle:=CreateThread(nil,4*1024,start_func,arg,CREATE_SUSPENDED,PDWORD(@td^.td_tid)^);

  if (td^.td_handle<>0) then
  begin
   Result:=0;
  end else
  begin
   Result:=GetLastError;
  end;
 {$ENDIF}

 if (Result=0) then
 begin
  {$IFDEF NT_THREAD}
  td^.td_tid:=DWORD(ClientId^.UniqueThread);
  {$ENDIF}

  //CSRSRV
  {$IFDEF CSRSRV}
  Result:=_CsrCreateRemoteThread(td^.td_handle,ClientId);
  {$ENDIF}
  //CSRSRV

  if (Result=0) then
  begin
   Result:=BaseQueryInfo(td);
  end;

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
var
 info:SYSTEM_INFO;
 i,m,t,n:Integer;
 data:array[0..SizeOf(Ptruint)-1+7] of Byte;
 p_mask:PPtruint;
begin
 if (td=nil) then Exit;
 if (td^.td_handle=0) or (td^.td_handle=THandle(-1)) then Exit(-1);

 new:=new and $FF;

 info.dwNumberOfProcessors:=1;
 GetSystemInfo(info);

 if (info.dwNumberOfProcessors<8) then
 begin
  //remap
  m:=0;
  for i:=0 to 7 do
  begin
   t:=(new shr i) and 1;
   n:=(i mod info.dwNumberOfProcessors);
   m:=m or (t shl n);
  end;
  new:=m;
 end;

 td^.td_cpuset:=new;

 p_mask:=Align(@data,8);
 p_mask^:=new;

 Result:=NtSetInformationThread(td^.td_handle,ThreadAffinityMask,p_mask,SizeOf(Ptruint));
end;

function cpu_set_priority(td:p_kthread;prio:Integer):Integer;
var
 mmpr,base:Integer;
begin
 if (td=nil) then Exit;
 if (td^.td_handle=0) or (td^.td_handle=THandle(-1)) then Exit(-1);

 Case prio of
    0..255:mmpr:=5;
  256..767:mmpr:=4;
  768..960:mmpr:=3;
  else
           mmpr:=2;
 end;

 Case prio of
    0..255:base:=15;
  256..305:base:=14;
  306..355:base:=13;
  356..405:base:=12;
  406..455:base:=11;
  456..505:base:=10;
  506..555:base:= 9;
  556..605:base:= 8;
  606..655:base:= 7;
  656..705:base:= 6;
  706..756:base:= 5;
  757..807:base:= 4;
  808..858:base:= 3;
  859..909:base:= 2;
  910..960:base:= 1;
  else
           base:= 1;
 end;

 Result:=NtSetInformationThread(td^.td_handle,ThreadMemoryPriority    ,@mmpr,SizeOf(Integer));
 Result:=NtSetInformationThread(td^.td_handle,ThreadActualBasePriority,@base,SizeOf(Integer));
end;

function cpu_thread_set_name(td:p_kthread;const name:shortstring):Integer;
Const
 MAX_LEN=256;
var
 W:array[0..MAX_LEN-1+7] of WideChar;
 P_W:PWideChar;
 data:array[0..SizeOf(UNICODE_STRING)-1+7] of Byte;
 P_UNAME:PUNICODE_STRING;
 L:DWORD;
begin
 Result:=0;
 if (td=nil) then Exit;
 if (td^.td_handle=0) or (td^.td_handle=THandle(-1)) then Exit;

 P_W:=Align(@W,8);

 FillWord(P_W^,MAX_LEN,0);
 L:=Utf8ToUnicode(P_W,MAX_LEN,@name[1],length(name));

 P_UNAME:=Align(@data,8);

 P_UNAME^.Length       :=L*SizeOf(WideChar);
 P_UNAME^.MaximumLength:=P_UNAME^.Length;
 P_UNAME^._Align       :=0;
 P_UNAME^.Buffer       :=P_W;

 Result:=NtSetInformationThread(td^.td_handle,ThreadNameInformation,P_UNAME,SizeOf(UNICODE_STRING));
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

