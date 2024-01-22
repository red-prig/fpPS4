
uses
 hamt,
 Windows,
 ntapi,
 kern_thr,
 vmparam;

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

//function VirtualQueryEx(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: PTRUINT): PTRUINT; external 'kernel32' name 'VirtualQueryEx';

function _get_prot_str(p:DWORD):RawByteString;
begin
 case (p and 127) of
  0,
  PAGE_NOACCESS         :Result:='___';
  PAGE_READONLY         :Result:='R__';
  PAGE_READWRITE        :Result:='RW_';
  PAGE_WRITECOPY        :Result:='CW_';
  PAGE_EXECUTE          :Result:='__E';
  PAGE_EXECUTE_READ     :Result:='R_E';
  PAGE_EXECUTE_READWRITE:Result:='RWE';
  PAGE_EXECUTE_WRITECOPY:Result:='CWE';
  else
   begin
    Result:='x'+HexStr(p,2);
   end;
 end;

 if ((p and PAGE_GUARD)<>0) then
 begin
  Result:=Result+'G';
 end else
 begin
  Result:=Result+'_';
 end;
end;

function _get_state_str(s:DWORD):RawByteString;
begin
 case s of
  MEM_COMMIT :Result:='COMMIT ';
  MEM_FREE   :Result:='FREE   ';
  MEM_RESERVE:Result:='RESERVE';
  else
   Result:='0x'+HexStr(s,5);
 end;
end;

function _get_type_str(t:DWORD):RawByteString;
begin
 case t of
  0          :Result:='FREE   ';
  MEM_IMAGE  :Result:='IMAGE  ';
  MEM_MAPPED :Result:='MAPPED ';
  MEM_PRIVATE:Result:='PRIVATE';
  else
   Result:='0x'+HexStr(t,5);
 end;
end;

procedure get_virtual_info(hProcess:THandle;var base:Pointer;var size:QWORD);
var
 addr:Pointer;
 prev:Pointer;
 info:TMemoryBasicInformation;
 i:integer;
begin
 size:=0;

 i:=VirtualQueryEx(hProcess,base,@info,SizeOf(info));
 if (i=0) then Exit;

 //allocated?
 if (info.State=MEM_FREE) then Exit;

 addr:=info.AllocationBase;

 base:=addr;

 repeat
  i:=VirtualQueryEx(hProcess,addr,@info,SizeOf(info));
  if (i=0) then Break;

  if (base<>info.AllocationBase) then Break;

  size:=size+Info.RegionSize;

  prev:=addr;
  addr:=addr+Info.RegionSize;

 until (prev>=addr);
end;

function md_reserve(hProcess:THandle;base:Pointer;size:QWORD):Integer;
begin
 Result:=NtAllocateVirtualMemoryEx(
          hProcess,
          @base,
          @size,
          MEM_RESERVE or MEM_RESERVE_PLACEHOLDER,
          PAGE_NOACCESS,
          nil,
          0
         );
end;

function md_mmap(hProcess:THandle;var base:Pointer;size:QWORD;prot:Integer):Integer;
begin
 Result:=NtAllocateVirtualMemory(
          hProcess,
          @base,
          0,
          @size,
          MEM_COMMIT or MEM_RESERVE,
          prot
         );
end;

function md_unmap(hProcess:THandle;base:Pointer;size:QWORD):Integer;
begin
 size:=0;
 Result:=NtFreeVirtualMemory(
          hProcess,
          @base,
          @size,
          MEM_RELEASE
         );
end;

procedure pmap_pinit(hProcess:THandle);
var
 base:Pointer;
 size:QWORD;
 prot:QWORD;
 i,r:Integer;
begin
 //fixup
 pmap_mem[0].start:=_PROC_AREA_START_0;

 if Length(pmap_mem)<>0 then
 begin
  For i:=0 to High(pmap_mem) do
  begin
   base:=Pointer(pmap_mem[i].start);
   size:=pmap_mem[i].__end-pmap_mem[i].start;

   r:=md_reserve(hProcess,base,size);

   if (r<>0) then
   begin
    Writeln('failed md_reserve(',HexStr(base),',',HexStr(base+size),'):0x',HexStr(r,8));
    //STATUS_COMMITMENT_LIMIT = $C000012D
    //Assert(false,'pmap_init');
   end;

   Writeln('md_reserve(',HexStr(base),',',HexStr(base+size),')');
  end;
 end;

end;

procedure print_virtual_proc(hProcess:THandle);
var
 addr,prev:Pointer;
 info:TMemoryBasicInformation;
 size:ptruint;
 i:integer;
begin
 addr:=Pointer($400000);

 get_virtual_info(hProcess,addr,size);
 Writeln('0x',HexStr(addr),'..','0x',HexStr(addr+size),':','0x',HexStr(size,16));

 addr:=nil;

 repeat

  i:=VirtualQueryEx(hProcess,addr,@info,SizeOf(info));

  if (i=0) then
  begin
   Break;
  end;

  Writeln('0x',HexStr(info.BaseAddress)   ,'..',
          '0x',HexStr(info.BaseAddress+info.RegionSize),':',
          '0x',HexStr(info.RegionSize,16) ,' ',
          _get_prot_str(info.Protect),' ',
          _get_state_str(info.State),' ',
          _get_type_str(info._Type),' '
          );

  prev:=addr;
  addr:=addr+Info.RegionSize;

  //if (addr>=Pointer($7FFE0000)) then Break;

 until (prev>=addr);
end;

procedure move_stack(hProcess,hThread:THandle);
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
 Context:=Align(@_Context,16);

 Context^:=Default(TCONTEXT);
 Context^.ContextFlags:=CONTEXT_ALL;

 err:=NtGetContextThread(hThread,Context);
 Writeln('NtGetContextThread:',HexStr(err,16));

 teb:=NtQueryTeb(hThread);

 kstack:=Default(t_td_stack);
 md_copyin(@teb^.stack,@kstack,SizeOf(t_td_stack),nil);

 Writeln('kstack.stack:',HexStr(kstack.stack));
 Writeln('kstack.sttop:',HexStr(kstack.sttop));

 Writeln('Rsp         :',HexStr(Context^.Rsp,16));
 Writeln('Rip         :',HexStr(Context^.Rip,16));

 delta:=QWORD(kstack.stack)-Context^.Rsp;

 addr:=kstack.sttop;
 get_virtual_info(hProcess,addr,size);

 Writeln('addr        :',HexStr(addr));
 Writeln('size        :',HexStr(size,16));

 err:=md_unmap(hProcess,addr,size);
 Writeln('md_unmap    :',HexStr(err,16));

 addr:=Pointer(WIN_MIN_MOVED_STACK);

 if (size>(WIN_MAX_MOVED_STACK-WIN_MIN_MOVED_STACK)) then
 begin
  size:=(WIN_MAX_MOVED_STACK-WIN_MIN_MOVED_STACK);
 end;

 Writeln('addr        :',HexStr(addr));
 Writeln('size        :',HexStr(size,16));

 err:=md_mmap(hProcess,addr,size,PAGE_READWRITE);
 Writeln('md_mmap     :',HexStr(err,16));

 kstack.sttop:=addr;
 kstack.stack:=addr+size;

 Writeln('kstack.stack:',HexStr(kstack.stack));
 Writeln('kstack.sttop:',HexStr(kstack.sttop));

 md_copyout(@kstack,@teb^.stack,SizeOf(t_td_stack),nil);

 Context^.Rsp:=QWORD(kstack.stack)-delta;

 Writeln('Rsp         :',HexStr(Context^.Rsp,16));
 Writeln('Rip         :',HexStr(Context^.Rip,16));

 err:=NtSetContextThread(hThread,Context);
 Writeln('NtSetContextThread:',err);
end;

procedure reserve_holes(hProcess:THandle);
var
 addr,prev:Pointer;
 info:TMemoryBasicInformation;
 i,r:integer;
begin
 Writeln('[reserve_holes]');

 addr:=Pointer(pmap_mem[0].start);

 repeat

  i:=VirtualQueryEx(hProcess,addr,@info,SizeOf(info));

  if (i=0) then
  begin
   Break;
  end;

  if (info.State=MEM_FREE) then
  begin
   r:=md_reserve(hProcess,info.BaseAddress,info.RegionSize);

   if (r<>0) then
   begin
    Writeln('failed md_reserve(',HexStr(info.BaseAddress),',',HexStr(info.BaseAddress+info.RegionSize),'):0x',HexStr(r,8));
    //Assert(false,'pmap_init');
   end;

   Writeln('md_reserve(',HexStr(info.BaseAddress),',',HexStr(info.BaseAddress+info.RegionSize),')');
  end;

  prev:=addr;
  addr:=addr+Info.RegionSize;

  if (addr>=Pointer(VM_MAXUSER_ADDRESS)) then Break;

 until (prev>=addr);
end;

begin
 si:=Default(TSTARTUPINFO);
 pi:=Default(PROCESS_INFORMATION);

 si.cb:=SizeOf(si);

 b:=CreateProcessW(ProcessName,nil,nil,nil,False,DEBUG_ONLY_THIS_PROCESS or CREATE_SUSPENDED,nil,nil,@si,@pi);

 print_virtual_proc(pi.hProcess);

 move_stack(pi.hProcess,pi.hThread);

 pmap_pinit(pi.hProcess);

 reserve_holes(pi.hProcess);

 readln;

 NtResumeProcess(pi.hProcess);

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

