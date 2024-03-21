unit md_systm;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 windows,
 ntapi;

type
 t_fork_cb=procedure(data:Pointer;size:QWORD); SysV_ABI_CDecl;

 p_fork_proc=^t_fork_proc;
 t_fork_proc=record
  hInput :THandle;  //in
  hOutput:THandle;  //in
  hError :THandle;  //in

  proc:Pointer;     //in
  data:Pointer;     //in
  size:QWORD;       //in

  hProcess:THandle; //out
  fork_pid:Integer; //out
 end;

function  md_copyin (hProcess:THandle;udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
function  md_copyout(hProcess:THandle;kaddr,udaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;

function  md_copyin (udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
function  md_copyout(kaddr,udaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;

function  md_getppid:DWORD;
function  md_pidfd_getfd(pidfd,targetfd:THandle):THandle;
function  md_pidfd_open (pid:DWORD):THandle;

procedure md_run_forked;
procedure md_fork_unshare;
function  md_fork_process(var info:t_fork_proc):Integer;

implementation

uses
 vmparam,
 kern_thr,
 sys_crt,
 errno,
 md_map;

var
 ppid:DWORD=0;

function md_copyin(hProcess:THandle;udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
var
 num:DWORD;
begin
 num:=0;
 if (NtReadVirtualMemory(hProcess,udaddr,kaddr,len,@num)=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=EFAULT;
 end;
 if (lencopied<>nil) then
 begin
  lencopied^:=num;
 end;
end;

function md_copyout(hProcess:THandle;kaddr,udaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
var
 num:DWORD;
begin
 num:=0;
 if (NtWriteVirtualMemory(hProcess,udaddr,kaddr,len,@num)=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=EFAULT;
 end;
 if (lencopied<>nil) then
 begin
  lencopied^:=num;
 end;
end;

function md_copyin(udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
begin
 Result:=md_copyin(NtCurrentProcess,udaddr,kaddr,len,lencopied);
end;

function md_copyout(kaddr,udaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
begin
 Result:=md_copyout(NtCurrentProcess,kaddr,udaddr,len,lencopied);
end;

function md_getppid:DWORD;
begin
 Result:=ppid;
end;

function md_pidfd_getfd(pidfd,targetfd:THandle):THandle;
begin
 Result:=0;
 NtDuplicateObject(
  pidfd,
  targetfd,
  NtCurrentProcess,
  @Result,
  0,
  0,
  DUPLICATE_SAME_ACCESS
 );
end;

function md_dup_to_pidfd(pidfd,targetfd:THandle):THandle;
begin
 Result:=0;
 NtDuplicateObject(
  NtCurrentProcess,
  targetfd,
  pidfd,
  @Result,
  0,
  0,
  DUPLICATE_SAME_ACCESS
 );
end;

function md_pidfd_open(pid:DWORD):THandle;
var
 ClientId:TCLIENT_ID;
 OATTR:OBJECT_ATTRIBUTES;
 R:DWORD;
begin
 Result:=0;

 ClientId.UniqueProcess:=pid;
 ClientId.UniqueThread :=0;

 OATTR:=Default(OBJECT_ATTRIBUTES);
 OATTR.Length:=SizeOf(OBJECT_ATTRIBUTES);

 R:=NtOpenProcess(@Result,PROCESS_DUP_HANDLE,@OATTR,@ClientId);
end;

const
 JobObjectExtendedLimitInformation=9;
 JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE=$00002000;

type
 JOBOBJECT_BASIC_LIMIT_INFORMATION = record
  PerProcessUserTimeLimit: LARGE_INTEGER;
  PerJobUserTimeLimit: LARGE_INTEGER;
  LimitFlags: DWORD;
  MinimumWorkingSetSize: SIZE_T;
  MaximumWorkingSetSize: SIZE_T;
  ActiveProcessLimit: DWORD;
  Affinity: ULONG_PTR;
  PriorityClass: DWORD;
  SchedulingClass: DWORD;
 end;

 PJOBOBJECT_EXTENDED_LIMIT_INFORMATION = ^JOBOBJECT_EXTENDED_LIMIT_INFORMATION;
 JOBOBJECT_EXTENDED_LIMIT_INFORMATION = record
  BasicLimitInformation: JOBOBJECT_BASIC_LIMIT_INFORMATION;
  IoInfo: IO_COUNTERS;
  ProcessMemoryLimit: SIZE_T;
  JobMemoryLimit: SIZE_T;
  PeakProcessMemoryUsed: SIZE_T;
  PeakJobMemoryUsed: SIZE_T;
 end;

function CreateJobObjectA(lpJobAttributes:LPSECURITY_ATTRIBUTES;
                          lpName:LPCTSTR):THandle; stdcall; external kernel32;

function SetInformationJobObject(hJob:HANDLE;
                                 JobObjectInformationClass:DWORD;
                                 lpJobObjectInformation:LPVOID;
                                 cbJobObjectInformationLength:DWORD):BOOL; stdcall; external kernel32;

function AssignProcessToJobObject(hJob,hProcess:THandle):BOOL; stdcall; external kernel32;

function NtQueryTeb(td_handle:THandle;var teb:p_teb):Integer;
var
 TBI:THREAD_BASIC_INFORMATION;
begin
 Result:=0;
 teb:=nil;
 TBI:=Default(THREAD_BASIC_INFORMATION);

 Result:=NtQueryInformationThread(
          td_handle,
          ThreadBasicInformation,
          @TBI,
          SizeOf(THREAD_BASIC_INFORMATION),
          nil);
 if (Result<>0) then Exit;

 teb:=TBI.TebBaseAddress;
end;

procedure NtGetVirtualInfo(hProcess:THandle;var base:Pointer;var size:QWORD);
var
 addr:Pointer;
 prev:Pointer;
 info:TMemoryBasicInformation;
 len:ULONG_PTR;
begin
 size:=0;

 len:=0;
 NtQueryVirtualMemory(
  hProcess,
  base,
  0,
  @info,
  sizeof(info),
  @len);
 if (len=0) then Exit;

 //allocated?
 if (info.State=MEM_FREE) then Exit;

 addr:=info.AllocationBase;

 base:=addr;

 repeat
  len:=0;
  NtQueryVirtualMemory(
   hProcess,
   addr,
   0,
   @info,
   sizeof(info),
   @len);
  if (len=0) then Exit;

  if (base<>info.AllocationBase) then Break;

  size:=size+Info.RegionSize;

  prev:=addr;
  addr:=addr+Info.RegionSize;

 until (prev>=addr);
end;

function NtMoveStack(hProcess,hThread:THandle):Integer;
var
 _Context:array[0..SizeOf(TCONTEXT)+15] of Byte;
 Context :PCONTEXT;
 teb   :p_teb;
 kstack:t_td_stack;
 addr  :Pointer;
 delta :QWORD;
 size  :QWORD;
 err   :DWORD;
begin
 Result:=0;

 Context:=Align(@_Context,16);

 Context^:=Default(TCONTEXT);
 Context^.ContextFlags:=CONTEXT_ALL;

 err:=NtGetContextThread(hThread,Context);
 if (err<>0) then Exit(err);

 err:=NtQueryTeb(hThread,teb);
 if (err<>0) then Exit(err);

 kstack:=Default(t_td_stack);
 err:=md_copyin(hProcess,@teb^.stack,@kstack,SizeOf(t_td_stack),nil);
 if (err<>0) then Exit(err);

 delta:=QWORD(kstack.stack)-Context^.Rsp;

 addr:=kstack.sttop;
 size:=0;
 NtGetVirtualInfo(hProcess,addr,size);

 err:=md_unmap(hProcess,addr,size);
 if (err<>0) then Exit(err);

 addr:=Pointer(WIN_MIN_MOVED_STACK);

 if (size>(WIN_MAX_MOVED_STACK-WIN_MIN_MOVED_STACK)) then
 begin
  size:=(WIN_MAX_MOVED_STACK-WIN_MIN_MOVED_STACK);
 end;

 err:=md_mmap(hProcess,addr,size,PAGE_READWRITE);
 if (err<>0) then Exit(err);

 kstack.sttop:=addr;
 kstack.stack:=addr+size;

 err:=md_copyout(hProcess,@kstack,@teb^.stack,SizeOf(t_td_stack),nil);
 if (err<>0) then Exit(err);

 Context^.Rsp:=QWORD(kstack.stack)-delta;

 err:=NtSetContextThread(hThread,Context);

 Exit(err);
end;

function NtReserve(hProcess:THandle):Integer;
var
 base:Pointer;
 size:QWORD;
 i,r:Integer;

 addr,prev:Pointer;
 info:TMemoryBasicInformation;
 len:ULONG_PTR;
begin
 if Length(pmap_mem)<>0 then
 begin
  //fixup
  pmap_mem[0].start:=_PROC_AREA_START_0;
  //
  For i:=0 to High(pmap_mem) do
  begin
   base:=Pointer(pmap_mem[i].start);
   size:=pmap_mem[i].__end-pmap_mem[i].start;

   r:=md_reserve(hProcess,base,size);
   if (r<>0) then Exit(r);
  end;
 end;

 //dmem mirror
 base:=Pointer(VM_MIN_GPU_ADDRESS);
 size:=VM_MAX_GPU_ADDRESS-VM_MIN_GPU_ADDRESS;

 r:=md_reserve(hProcess,base,size);
 if (r<>0) then Exit(r);

 addr:=Pointer(pmap_mem[0].start);

 repeat

  len:=0;
  NtQueryVirtualMemory(
   hProcess,
   addr,
   0,
   @info,
   sizeof(info),
   @len);
  if (len=0) then Break;

  if (info.State=MEM_FREE) then
  begin
   r:=md_reserve(hProcess,info.BaseAddress,info.RegionSize);
  end;

  prev:=addr;
  addr:=addr+Info.RegionSize;

  if (addr>=Pointer(VM_MAXUSER_ADDRESS)) then Break;

 until (prev>=addr);
end;

type
 p_shared_info=^t_shared_info;
 t_shared_info=record
  ppid      :QWORD;
  hStdInput :THandle;
  hStdOutput:THandle;
  hStdError :THandle;
  proc      :Pointer;
  size      :QWORD;
  data      :record end;
 end;

procedure md_run_forked;
var
 base:p_shared_info;
 info:TMemoryBasicInformation;
 len:ULONG_PTR;

 proc:Pointer;
begin
 base:=Pointer(WIN_SHARED_ADDR);

 len:=0;
 NtQueryVirtualMemory(
  NtCurrentProcess,
  base,
  0,
  @info,
  sizeof(info),
  @len);
 if (len=0) then Exit;

 if (info.State=MEM_FREE) then Exit;

 ppid:=base^.ppid;

 SetStdHandle(STD_INPUT_HANDLE ,base^.hStdInput );
 SetStdHandle(STD_ERROR_HANDLE ,base^.hStdOutput);
 SetStdHandle(STD_OUTPUT_HANDLE,base^.hStdError );

 proc:=base^.proc;

 if (proc=nil) then Exit;

 t_fork_cb(proc)(@base^.data,base^.size);

 NtTerminateProcess(NtCurrentProcess, 0);
end;

procedure md_fork_unshare;
var
 base:Pointer;
begin
 base:=Pointer(WIN_SHARED_ADDR);
 md_unmap(base,0);
end;

var
 hProcJob:Thandle=0;

function NtFetchJob:THandle;
var
 info:JOBOBJECT_EXTENDED_LIMIT_INFORMATION;
begin
 if (hProcJob<>0) then Exit(hProcJob);

 hProcJob:=CreateJobObjectA(nil,nil);

 info:=Default(JOBOBJECT_EXTENDED_LIMIT_INFORMATION);
 info.BasicLimitInformation.LimitFlags:=JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;

 SetInformationJobObject(hProcJob,
                         JobObjectExtendedLimitInformation,
                         @info,
                         SizeOf(info));

 Exit(hProcJob);
end;

function NtCreateShared(hProcess:THandle;var info:t_fork_proc):Integer;
var
 base:p_shared_info;
 full:QWORD;
 shared_info:t_shared_info;
begin
 base:=Pointer(WIN_SHARED_ADDR);

 full:=SizeOf(shared_info)+info.size;
 full:=(info.size+(MD_PAGE_SIZE-1)) and (not (MD_PAGE_SIZE-1));

 Result:=md_mmap(hProcess,base,full,MD_PROT_RW);
 if (Result<>0) then Exit;

 shared_info:=Default(t_shared_info);

 shared_info.ppid      :=GetCurrentProcessId;

 shared_info.hStdInput :=md_dup_to_pidfd(hProcess,info.hInput );
 shared_info.hStdOutput:=md_dup_to_pidfd(hProcess,info.hOutput);
 shared_info.hStdError :=md_dup_to_pidfd(hProcess,info.hError );

 shared_info.proc:=info.proc;
 shared_info.size:=info.size;

 Result:=md_copyout(hProcess,@shared_info,base,SizeOf(shared_info),nil);
 if (Result<>0) then Exit;

 if (info.data<>nil) and (info.size<>0) then
 begin
  Result:=md_copyout(hProcess,info.data,@base^.data,info.size,nil);
 end;
end;

function md_fork_process(var info:t_fork_proc):Integer;
type
 TBUF_PROC_INFO=packed record
  UNAME:UNICODE_STRING;
  DATA :array[0..MAX_PATH*2] of WideChar;
 end;
var
 si:TSTARTUPINFO;
 pi:PROCESS_INFORMATION;
 BUF:TBUF_PROC_INFO;
 LEN:ULONG;
 b:BOOL;
begin
 Result:=0;

 BUF:=Default(TBUF_PROC_INFO);
 LEN:=SizeOf(BUF);

 Result:=NtQueryInformationProcess(NtCurrentProcess,
                                   ProcessImageFileNameWin32,
                                   @BUF,
                                   LEN,
                                   @LEN);
 if (Result<>0) then Exit;

 si:=Default(TSTARTUPINFO);
 pi:=Default(PROCESS_INFORMATION);

 si.cb:=SizeOf(si);

 b:=CreateProcessW(PWideChar(@BUF.DATA),nil,nil,nil,False,CREATE_SUSPENDED,nil,nil,@si,@pi);
 if not b then Exit(-1);

 b:=AssignProcessToJobObject(NtFetchJob, pi.hProcess);
 if not b then Exit(-1);

 Result:=NtMoveStack(pi.hProcess,pi.hThread);
 if (Result<>0) then Exit;

 Result:=NtReserve(pi.hProcess);
 if (Result<>0) then Exit;

 Result:=NtCreateShared(pi.hProcess,info);
 if (Result<>0) then Exit;

 Result:=NtResumeProcess(pi.hProcess);
 if (Result<>0) then Exit;

 NtClose(pi.hThread);

 info.hProcess:=pi.hProcess;
 info.fork_pid:=pi.dwProcessId;
end;

end.


