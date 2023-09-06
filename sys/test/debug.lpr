
uses
 hamt,
 Windows,
 ntapi,
 kern_thr;

const
 ProcessName='project1.exe';

var
 si:TSTARTUPINFO;
 pi:PROCESS_INFORMATION;

 b:Bool;

 devent:DEBUG_EVENT;
 dwContinueStatus:DWORD;

const
 FPC_EXCEPTION_CODE=$E0465043;
 FPC_SET_EH_HANDLER=$E0465044;

type
 PTHREADNAME_INFO=^THREADNAME_INFO;
 THREADNAME_INFO=record
  dwType    :DWord;     // Must be 0x1000.
  szName    :PAnsiChar; // Pointer to name (in user addr space).
  dwThreadID:DWord;     // Thread ID (-1=caller thread).
  dwFlags   :DWord;     // Reserved for future use, must be zero.
 end;

function md_copyin(udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
var
 num:DWORD;
begin
 num:=0;
 if (NtReadVirtualMemory(pi.hProcess,udaddr,kaddr,len,@num)=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=-1;
 end;
 if (lencopied<>nil) then
 begin
  lencopied^:=num;
 end;
end;

function md_copyout(kaddr,udaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
var
 num:DWORD;
begin
 num:=0;
 if (NtWriteVirtualMemory(pi.hProcess,udaddr,kaddr,len,@num)=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=-1;
 end;
 if (lencopied<>nil) then
 begin
  lencopied^:=num;
 end;
end;

var
 EhHandler:Pointer=nil;

type
 PEH_HANDLER_INFO=^EH_HANDLER_INFO;
 EH_HANDLER_INFO=record
  ptr:Pointer;
 end;

procedure ProcessSetEhHandler(p:PEXCEPTION_DEBUG_INFO);
var
 info:PEH_HANDLER_INFO;
begin
 info:=@p^.ExceptionRecord.ExceptionInformation;
 EhHandler:=info^.ptr;

 Writeln('FPC_SET_EH_HANDLER:0x',HexStr(EhHandler));
end;

procedure ProcessSetThreadName(p:PEXCEPTION_DEBUG_INFO);
var
 info:PTHREADNAME_INFO;
 name:array[0..255] of AnsiChar;
 len:ptruint;
begin
 info:=@p^.ExceptionRecord.ExceptionInformation;

 Fillchar(name,SizeOf(name),0);

 md_copyin(info^.szName,@name,SizeOf(name),@len);

 Writeln('EXCEPTION_SET_THREADNAME:',name,':',info^.dwThreadID);

 //writeln;
end;

function NtQueryTeb(td_handle:THandle):p_teb;
var
 TBI:THREAD_BASIC_INFORMATION;
 err:Integer;
begin
 Result:=nil;
 TBI:=Default(THREAD_BASIC_INFORMATION);

 err:=NtQueryInformationThread(
       td_handle,
       ThreadBasicInformation,
       @TBI,
       SizeOf(THREAD_BASIC_INFORMATION),
       nil);
 if (err<>0) then Exit;

 Result:=TBI.TebBaseAddress;
end;

Function NtQueryThread(dwProcessId,dwThreadId:DWORD):THandle;
var
 hThread:THandle;
 ClientId:TCLIENT_ID;
 OATTR:OBJECT_ATTRIBUTES;
begin
 ClientId.UniqueProcess:=dwProcessId;
 ClientId.UniqueThread :=dwThreadId;

 OATTR:=Default(OBJECT_ATTRIBUTES);
 OATTR.Length:=SizeOf(OBJECT_ATTRIBUTES);

 hThread:=0;
 NtOpenThread(@hThread,THREAD_ALL_ACCESS,@OATTR,@ClientId);

 Result:=hThread;
end;

function IS_SYSTEM_STACK(kstack:t_td_stack;rsp:qword):Boolean; inline;
begin
 Result:=(rsp<=QWORD(kstack.stack)) and (rsp>QWORD(kstack.sttop));
end;

type
 p_debug_thread=^t_debug_thread;
 t_debug_thread=packed record
  ProcessId:DWORD;
  ThreadId :DWORD;
  hThread  :THandle;
  u_teb    :p_teb;
  u_td     :p_kthread;
  kstack   :t_td_stack;
 end;

var
 debug_threads:TSTUB_HAMT32;

function load_debug_thread(dwProcessId,dwThreadId:DWORD):p_debug_thread;
var
 data:PPointer;
 thread:p_debug_thread;

 hThread:THandle;
 u_teb:p_teb;
 u_td:p_kthread;
 kstack:t_td_stack;
begin
 Result:=nil;
 data:=HAMT_search32(@debug_threads,dwThreadId);

 if (data<>nil) then Exit(data^);

 hThread:=NtQueryThread(dwProcessId,dwThreadId);
 if (hThread=0) then Exit;

 u_teb:=NtQueryTeb(hThread);
 if (u_teb=nil) then
 begin
  NtClose(hThread);
  Exit;
 end;

 u_td:=nil;
 md_copyin(@u_teb^.thread,@u_td,SizeOf(Pointer),nil);

 kstack:=Default(t_td_stack);
 md_copyin(@u_td^.td_kstack,@kstack,SizeOf(t_td_stack),nil);

 thread:=AllocMem(SizeOf(t_debug_thread));

 thread^.ProcessId:=dwProcessId;
 thread^.ThreadId :=dwThreadId;
 thread^.hThread  :=hThread;
 thread^.u_teb    :=u_teb;
 thread^.u_td     :=u_td;
 thread^.kstack   :=kstack;

 HAMT_insert32(@debug_threads,dwThreadId,thread);

 Exit(thread);
end;

type
 PDispatchContext=^TDispatchContext;
 TDispatchContext=packed record
  ExceptionContext :TCONTEXT;
  ExceptionRecord  :EXCEPTION_RECORD;
  ExceptionPointers:TExceptionPointers;
 end;

procedure ProcessAV(p:PEXCEPTION_DEBUG_INFO);
var
 thread:p_debug_thread;

 _Context:array[0..SizeOf(TDispatchContext)+15] of Byte;
 Context :PDispatchContext;

 u_ctx:PDispatchContext;
 u_exp:PExceptionPointers;

 rsp:QWORD;

 err:DWORD;
begin
 thread:=load_debug_thread(devent.dwProcessId,devent.dwThreadId);
 if (thread=nil) then Exit;

 if (thread^.u_td=nil) then Exit;

 Context:=Align(@_Context,16);

 Context^:=Default(TDispatchContext);
 Context^.ExceptionContext.ContextFlags:=CONTEXT_ALL;

 err:=NtGetContextThread(thread^.hThread,@Context^.ExceptionContext);
 //Writeln('NtGetContextThread:',HexStr(err,16));

 //Writeln('td_kstack.stack:',HexStr(kstack.stack));
 //Writeln('td_kstack.sttop:',HexStr(kstack.sttop));

 rsp:=Context^.ExceptionContext.Rsp;
 //Writeln('Rsp:',HexStr(rsp,16));

 if IS_SYSTEM_STACK(thread^.kstack,rsp) then
 begin
  rsp:=rsp-128;
 end else
 begin
  rsp:=QWORD(thread^.kstack.stack);
 end;

 rsp:=rsp-SizeOf(TDispatchContext);
 rsp:=rsp and QWORD(-32); //avx/context align

 u_ctx:=Pointer(rsp);
 u_exp:=@u_ctx^.ExceptionPointers;

 //stack frame
 rsp:=rsp-SizeOf(QWORD);

 Context^.ExceptionRecord:=p^.ExceptionRecord;

 Context^.ExceptionPointers.ExceptionRecord:=@u_ctx^.ExceptionRecord;
 Context^.ExceptionPointers.ContextRecord  :=@u_ctx^.ExceptionContext;

 err:=md_copyout(Context,u_ctx,SizeOf(TDispatchContext),nil);
 //Writeln('md_copyout:',err);

 //Writeln('start rsp:',HexStr(rsp,16));

 Context^.ExceptionContext.Rsp:=rsp;
 Context^.ExceptionContext.Rbp:=rsp;

 Context^.ExceptionContext.Rcx:=QWORD(u_exp);
 Context^.ExceptionContext.Rip:=QWORD(EhHandler);

 err:=NtSetContextThread(thread^.hThread,@Context^.ExceptionContext);
 //Writeln('NtSetContextThread:',err);

end;

procedure ProcessException(p:PEXCEPTION_DEBUG_INFO);
begin

 case p^.ExceptionRecord.ExceptionCode of
  FPC_EXCEPTION_CODE        :Exit;
  FPC_SET_EH_HANDLER        :ProcessSetEhHandler(p);
  EXCEPTION_BREAKPOINT      :Exit;
  EXCEPTION_SET_THREADNAME  :ProcessSetThreadName(p);
  EXCEPTION_ACCESS_VIOLATION:ProcessAV(p);
  else
   Writeln('EXCEPTION_DEBUG_EVENT:0x',HexStr(p^.ExceptionRecord.ExceptionCode,8));
 end;

 //writeln;
end;

procedure ProcessDebugEvent;
begin
 case devent.dwDebugEventCode of
  EXCEPTION_DEBUG_EVENT     :ProcessException(@devent.Exception);
  CREATE_THREAD_DEBUG_EVENT :Writeln('CREATE_THREAD_DEBUG_EVENT ');
  CREATE_PROCESS_DEBUG_EVENT:Writeln('CREATE_PROCESS_DEBUG_EVENT');
  EXIT_THREAD_DEBUG_EVENT   :Writeln('EXIT_THREAD_DEBUG_EVENT   ');
  EXIT_PROCESS_DEBUG_EVENT  :Writeln('EXIT_PROCESS_DEBUG_EVENT  ');
  LOAD_DLL_DEBUG_EVENT      :Writeln('LOAD_DLL_DEBUG_EVENT      ');
  OUTPUT_DEBUG_STRING_EVENT :Writeln('OUTPUT_DEBUG_STRING_EVENT ');
  UNLOAD_DLL_DEBUG_EVENT    :Writeln('UNLOAD_DLL_DEBUG_EVENT    ');
  RIP_EVENT                 :Writeln('RIP_EVENT                 ');
  else;
 end;
end;

begin
 si:=Default(TSTARTUPINFO);
 pi:=Default(PROCESS_INFORMATION);

 si.cb:=SizeOf(si);

 b:=CreateProcessW(ProcessName,nil,nil,nil,False,DEBUG_ONLY_THIS_PROCESS,nil,nil,@si,@pi);

 devent:=Default(Windows.DEBUG_EVENT);

 repeat
  b:=WaitForDebugEvent(@devent,INFINITE);
  if not b then Break;

  dwContinueStatus:=DBG_CONTINUE;

  ProcessDebugEvent();

  ContinueDebugEvent(devent.dwProcessId,
                     devent.dwThreadId,
                     dwContinueStatus);
 until false;

end.

