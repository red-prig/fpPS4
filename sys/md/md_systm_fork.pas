unit md_systm_fork;

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

function  md_getppid:DWORD;

procedure md_run_forked;
function  md_fork_process(var info:t_fork_proc):Integer;

implementation

uses
 vmparam,
 kern_thr,
 md_systm,
 md_systm_reserve,
 md_map;

function md_getppid:DWORD;
var
 data:array[0..SizeOf(PROCESS_BASIC_INFORMATION)-1+7] of Byte;
 p_info:PPROCESS_BASIC_INFORMATION;
 R:DWORD;
begin
 Result:=0;
 p_info:=Align(@data,8);

 R:=NtQueryInformationProcess(NtCurrentProcess,
                              ProcessBasicInformation,
                              p_info,
                              SizeOf(PROCESS_BASIC_INFORMATION),
                              nil);
 if (R=0) then
 begin
  Result:=p_info^.InheritedFromUPI;
 end;
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
 data:array[0..SizeOf(THREAD_BASIC_INFORMATION)-1+7] of Byte;
 P_TBI:PTHREAD_BASIC_INFORMATION;
begin
 Result:=0;
 teb:=nil;
 P_TBI:=Align(@data,8);
 P_TBI^:=Default(THREAD_BASIC_INFORMATION);

 Result:=NtQueryInformationThread(
          td_handle,
          ThreadBasicInformation,
          P_TBI,
          SizeOf(THREAD_BASIC_INFORMATION),
          nil);
 if (Result<>0) then Exit;

 teb:=P_TBI^.TebBaseAddress;
end;

function NtQueryPeb(hProcess:THandle;var peb:PPEB):Integer;
var
 data:array[0..SizeOf(PROCESS_BASIC_INFORMATION)-1+7] of Byte;
 p_info:PPROCESS_BASIC_INFORMATION;
begin
 p_info:=Align(@data,8);

 Result:=NtQueryInformationProcess(hProcess,
                                   ProcessBasicInformation,
                                   p_info,
                                   SizeOf(PROCESS_BASIC_INFORMATION),
                                   nil);
 if (Result=0) then
 begin
  peb:=p_info^.PebBaseAddress;
 end;
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

function fast_aslr():DWORD; inline;
var
 x:QWORD;
begin
 x:=GetTickCount64;
 x:=x xor (x shl 13);
 x:=x xor (x shr  7);
 x:=x xor (x shl 17);
 Result:=DWORD(x shl 16);
end;

function NtMoveStack(hProcess,hThread:THandle;var rip:QWORD):Integer;
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

 //get main thread context
 err:=NtGetContextThread(hThread,Context);
 if (err<>0) then Exit(err);

 rip:=Context^.Rip;

 //RIP -> RtlUserThreadStart
 //RCX -> entry             (_WinMainCRTStartup)
 //RDX -> lpThreadParameter

 //get teb
 err:=NtQueryTeb(hThread,teb);
 if (err<>0) then Exit(err);

 //get stack bound
 kstack:=Default(t_td_stack);
 err:=md_copyin(@teb^.stack,@kstack,SizeOf(t_td_stack),nil,hProcess);
 if (err<>0) then Exit(err);

 delta:=QWORD(kstack.stack)-Context^.Rsp;

 //get full bound of stack
 addr:=kstack.sttop;
 size:=0;
 NtGetVirtualInfo(hProcess,addr,size);

 //unmap old
 err:=md_unmap(addr,size,hProcess);
 if (err<>0) then Exit(err);

 //map new
 addr:=Pointer(KERNEL_LOWER + fast_aslr());
 err:=md_mmap(addr,size,VM_RW,0,0,hProcess);
 if (err<>0) then Exit(err);

 kstack.sttop:=addr;
 kstack.stack:=addr+size;

 //save new
 err:=md_copyout(@kstack,@teb^.stack,SizeOf(t_td_stack),nil,hProcess);
 if (err<>0) then Exit(err);

 Context^.Rsp:=QWORD(kstack.stack)-delta;

 //save context
 err:=NtSetContextThread(hThread,Context);

 Exit(err);
end;

type
 TForkUserParams=record
  temp:PRTL_USER_PROCESS_PARAMETERS;
  usrc:Pointer;
  udst:Pointer;
  size:QWORD;
 end;

Procedure PATCH_UNICODE_STRING(var UserParams:TForkUserParams;var str:UNICODE_STRING);
var
 ofs:Integer;
begin
 if (str.Buffer=nil) then Exit;
 ofs:=str.Buffer-Pointer(UserParams.usrc);
 Assert((ofs>0) and (ofs<UserParams.size));
 str.Buffer:=UserParams.udst+ofs;
end;

Procedure PATCH_POINTER(var UserParams:TForkUserParams;var ptr:Pointer);
var
 ofs:Integer;
begin
 if (ptr=nil) then Exit;
 ofs:=ptr-Pointer(UserParams.usrc);
 Assert((ofs>0) and (ofs<UserParams.size));
 ptr:=UserParams.udst+ofs;
end;

Procedure PATCH_USER_PROCESS_PARAMETERS(var UserParams:TForkUserParams);
begin
 PATCH_UNICODE_STRING(UserParams,UserParams.temp^.CurrentDirectory.DosPath);
 PATCH_UNICODE_STRING(UserParams,UserParams.temp^.DllPath                 );
 PATCH_UNICODE_STRING(UserParams,UserParams.temp^.ImagePathName           );
 PATCH_UNICODE_STRING(UserParams,UserParams.temp^.CommandLine             );
 PATCH_POINTER       (UserParams,UserParams.temp^.Environment             );
 PATCH_UNICODE_STRING(UserParams,UserParams.temp^.WindowTitle             );
 PATCH_UNICODE_STRING(UserParams,UserParams.temp^.DesktopInfo             );
 PATCH_UNICODE_STRING(UserParams,UserParams.temp^.ShellInfo               );
 PATCH_UNICODE_STRING(UserParams,UserParams.temp^.RuntimeData             );
end;

function NtMoveProcessParameters(hProcess:THandle):Integer;
var
 err       :DWORD;
 u_peb     :PPEB;
 u_upp     :PRTL_USER_PROCESS_PARAMETERS;
 UserParams:TForkUserParams;
begin
 Result:=0;

 //get peb ptr
 u_peb:=nil;
 err:=NtQueryPeb(hProcess,u_peb);
 if (err<>0) then Exit(err);

 //get params ptr
 u_upp:=nil;
 err:=md_copyin(@u_peb^.ProcessParameters,@u_upp,SizeOf(Pointer),nil,hProcess);
 if (err<>0) then Exit(err);

 //get full bound of stack
 UserParams.usrc:=u_upp;
 UserParams.size:=0;
 NtGetVirtualInfo(hProcess,UserParams.usrc,UserParams.size);

 //alloc temp
 UserParams.temp:=kmem_alloc(UserParams.size,VM_RW);
 if (UserParams.temp=nil) then Exit(-1);

 //get params
 err:=md_copyin(u_upp,UserParams.temp,UserParams.size,nil,hProcess);
 if (err<>0) then Exit(err);

 //map new
 UserParams.udst:=Pointer(KERNEL_LOWER + fast_aslr());
 err:=md_mmap(UserParams.udst,UserParams.size,VM_RW,0,0,hProcess);
 if (err<>0) then Exit(err);

 PATCH_USER_PROCESS_PARAMETERS(UserParams);

 //save new
 err:=md_copyout(UserParams.temp,UserParams.udst,UserParams.size,nil,hProcess);
 if (err<>0) then Exit(err);

 //set params ptr
 err:=md_copyout(@UserParams.udst,@u_peb^.ProcessParameters,SizeOf(Pointer),nil,hProcess);
 if (err<>0) then Exit(err);

 //unmap old
 err:=md_unmap(u_upp,UserParams.size,hProcess);
 if (err<>0) then Exit(err);

 //free temp
 kmem_free(UserParams.temp,UserParams.size);
end;

function NtReserve(hProcess:THandle;rip:QWORD):Integer;
var
 addr,prev:Pointer;
 info:TMemoryBasicInformation;
 len:ULONG_PTR;

 m:t_md_map_reserve_result;
begin
 Result:=0;

 m:=md_map_reserve(hProcess,rip);
 if (m.error<>0) then
 begin
  Writeln(stderr,'md_placeholder_mmap(0x',HexStr(m.base),',0x',HexStr(m.size,16),'):0x',HexStr(m.error,8));
  Exit(m.error);
 end;

 //fill corners

 addr:=Pointer($10000 {guest_pmap_mem[0].start});

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
   md_placeholder_mmap(info.BaseAddress,info.RegionSize,MD_MAP_FIXED,hProcess);
  end;

  prev:=addr;
  addr:=addr+Info.RegionSize;

  if (addr>=Pointer(VM_MAXGUEST_ADDRESS)) then Break;

 until (prev>=addr);
end;

type
 p_shared_info=^t_shared_info;
 t_shared_info=record
  hStdInput :THandle;
  hStdOutput:THandle;
  hStdError :THandle;
  proc      :Pointer;
  size      :QWORD;
  data      :record end;
 end;

function get_cur_peb:PPEB; assembler; nostackframe;
asm
 movqq %gs:teb.PEB,Result
end;

procedure md_run_forked;
var
 base :p_shared_info;
 size :QWORD;
 data :Pointer;
 proc :Pointer;
begin
 base:=System.InterlockedExchange(get_cur_peb^.SubSystemData,nil);
 if (base=nil) then Exit;

 SetStdHandle(STD_INPUT_HANDLE ,base^.hStdInput );
 SetStdHandle(STD_ERROR_HANDLE ,base^.hStdOutput);
 SetStdHandle(STD_OUTPUT_HANDLE,base^.hStdError );

 proc:=base^.proc;

 if (proc=nil) then
 begin
  md_unmap(base,0);
  Exit;
 end;

 //clone data
 size:=base^.size;
 data:=AllocMem(size);
 Move(base^.data,data^,size);

 //free page
 md_unmap(base,0);

 t_fork_cb(proc)(data,size);

 NtTerminateProcess(NtCurrentProcess, 0);
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
 err        :DWORD;
 u_peb      :PPEB;
 base       :p_shared_info;
 full       :QWORD;
 shared_info:t_shared_info;
begin
 Result:=0;

 //get peb ptr
 u_peb:=nil;
 err:=NtQueryPeb(hProcess,u_peb);
 if (err<>0) then Exit(err);

 //calc full size
 full:=SizeOf(shared_info)+info.size;
 full:=(info.size+(MD_PAGE_SIZE-1)) and (not (MD_PAGE_SIZE-1));

 //alloc page
 base:=Pointer(KERNEL_LOWER + fast_aslr());
 err:=md_mmap(base,full,VM_RW,0,0,hProcess);
 if (err<>0) then Exit(err);

 //save base to peb
 err:=md_copyout(@base,@u_peb^.SubSystemData,SizeOf(Pointer),nil,hProcess);
 if (err<>0) then Exit(err);

 shared_info:=Default(t_shared_info);

 shared_info.hStdInput :=md_dup_to_pidfd(hProcess,info.hInput );
 shared_info.hStdOutput:=md_dup_to_pidfd(hProcess,info.hOutput);
 shared_info.hStdError :=md_dup_to_pidfd(hProcess,info.hError );

 shared_info.proc:=info.proc;
 shared_info.size:=info.size;

 err:=md_copyout(@shared_info,base,SizeOf(shared_info),nil,hProcess);
 if (err<>0) then Exit(err);

 if (info.data<>nil) and (info.size<>0) then
 begin
  Result:=md_copyout(info.data,@base^.data,info.size,nil,hProcess);
 end;
end;

function md_fork_process(var info:t_fork_proc):Integer;
type
 PBUF_PROC_INFO=^TBUF_PROC_INFO;
 TBUF_PROC_INFO=packed record
  UNAME:UNICODE_STRING;
  DATA :array[0..MAX_PATH*2] of WideChar;
 end;
var
 si:TSTARTUPINFO;
 pi:PROCESS_INFORMATION;
 data:array[0..SizeOf(TBUF_PROC_INFO)-1+7] of Byte;
 P_BUF:PBUF_PROC_INFO;
 LEN:ULONG;
 rip:QWORD;
 b:BOOL;
begin
 Result:=0;

 P_BUF:=Align(@data,8);

 P_BUF^:=Default(TBUF_PROC_INFO);
 LEN:=SizeOf(TBUF_PROC_INFO);

 Result:=NtQueryInformationProcess(NtCurrentProcess,
                                   ProcessImageFileNameWin32,
                                   P_BUF,
                                   LEN,
                                   @LEN);
 if (Result<>0) then Exit;

 si:=Default(TSTARTUPINFO);
 pi:=Default(PROCESS_INFORMATION);

 si.cb:=SizeOf(si);

 b:=CreateProcessW(PWideChar(@P_BUF^.DATA),nil,nil,nil,False,CREATE_SUSPENDED,nil,nil,@si,@pi);
 if not b then Exit(-1);

 b:=AssignProcessToJobObject(NtFetchJob, pi.hProcess);
 if not b then Exit(-1);

 rip:=0;
 Result:=NtMoveStack(pi.hProcess,pi.hThread,rip);
 if (Result<>0) then
 begin
  Writeln(stderr,'NtMoveStack:0x',HexStr(Result,8));
  Exit;
 end;

 Result:=NtMoveProcessParameters(pi.hProcess);
 if (Result<>0) then
 begin
  Writeln(stderr,'NtMoveProcessParameters:0x',HexStr(Result,8));
  Exit;
 end;

 Result:=NtReserve(pi.hProcess,rip);
 if (Result<>0) then
 begin
  Writeln(stderr,'NtReserve:0x',HexStr(Result,8));
  Exit;
 end;

 Result:=NtCreateShared(pi.hProcess,info);
 if (Result<>0) then
 begin
  Writeln(stderr,'NtCreateShared:0x',HexStr(Result,8));
  Exit;
 end;

 Result:=NtResumeProcess(pi.hProcess);
 if (Result<>0) then
 begin
  Writeln(stderr,'NtResumeProcess:0x',HexStr(Result,8));
  Exit;
 end;

 NtClose(pi.hThread);

 info.hProcess:=pi.hProcess;
 info.fork_pid:=pi.dwProcessId;
end;


end.

