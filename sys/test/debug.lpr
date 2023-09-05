
uses
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

procedure ProcessAV(p:PEXCEPTION_DEBUG_INFO);
var
 hThread:THandle;
 u_teb:p_teb;
 k_teb:teb;
 u_td:p_kthread;
 _Context:array[0..SizeOf(TCONTEXT)+15] of Byte;
 Context :PCONTEXT;

 k_exp:TExceptionPointers;
 u_exp:PExceptionPointers;

 kstack:t_td_stack;

 rsp:QWORD;

 err:DWORD;
begin
 //dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED;

 hThread:=NtQueryThread(devent.dwProcessId,devent.dwThreadId);

 u_teb:=NtQueryTeb(hThread);

 u_td:=nil;

 if (u_teb<>nil) then
 begin
  k_teb:=Default(teb);
  md_copyin(u_teb,@k_teb,SizeOf(teb),nil);
  u_td:=k_teb.thread;
 end;

 if (u_td<>nil) then
 begin
  Context:=Align(@_Context,16);

  Context^:=Default(TCONTEXT);
  Context^.ContextFlags:=CONTEXT_ALL;

  err:=NtGetContextThread(hThread,Context);
  //Writeln('NtGetContextThread:',HexStr(err,16));

  kstack:=Default(t_td_stack);
  err:=md_copyin(@u_td^.td_kstack,@kstack,SizeOf(t_td_stack),nil);
  //Writeln('md_copyin:',err);

  //Writeln('td_kstack.stack:',HexStr(kstack.stack));
  //Writeln('td_kstack.sttop:',HexStr(kstack.sttop));

  rsp:=Context^.Rsp;
  //Writeln('Rsp:',HexStr(rsp,16));

  if IS_SYSTEM_STACK(kstack,rsp) then
  begin
   rsp:=rsp-128;
  end else
  begin
   rsp:=QWORD(kstack.stack);
  end;

  rsp:=rsp-SizeOf(EXCEPTION_RECORD);
  k_exp.ExceptionRecord:=Pointer(rsp);

  rsp:=rsp-SizeOf(TCONTEXT);
  rsp:=rsp and QWORD(-16); //context align

  k_exp.ContextRecord:=Pointer(rsp);

  rsp:=rsp-SizeOf(EXCEPTION_POINTERS);

  u_exp:=Pointer(rsp);

  rsp:=rsp-SizeOf(QWORD);
  rsp:=rsp and QWORD(-32); //avx align
  rsp:=rsp-SizeOf(QWORD);

  err:=md_copyout(@k_exp,u_exp,SizeOf(EXCEPTION_POINTERS),nil); //pointers
  //Writeln('md_copyout:',err);

  err:=md_copyout(Context,k_exp.ContextRecord,SizeOf(TCONTEXT),nil); //context
  //Writeln('md_copyout:',err);

  err:=md_copyout(@p^.ExceptionRecord,k_exp.ExceptionRecord,SizeOf(EXCEPTION_RECORD),nil); //record
  //Writeln('md_copyout:',err);

  //Writeln('start rsp:',HexStr(rsp,16));

  Context^.Rsp:=rsp;
  Context^.Rbp:=rsp;

  Context^.Rcx:=QWORD(u_exp);
  Context^.Rip:=QWORD(EhHandler);

  err:=NtSetContextThread(hThread,Context);
  //Writeln('NtSetContextThread:',err);

 end;

 if (hThread<>0) then
 begin
  NtClose(hThread);
 end;

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

