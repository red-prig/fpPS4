unit md_exception;

{$mode objfpc}{$H+}

interface

implementation

uses
  Windows,
  sysutils,
  ntapi,
  machdep,
  md_context,
  md_proc,
  sys_bootparam,
  kern_exit,
  kern_thr,
  subr_backtrace,
  trap,
  signal,
  ucontext,
  vm,
  vm_map,
  vm_pmap,
  vm_pmap_prot,
  kern_proc,
  kern_jit_dynamic;

const
 EXCEPTION_SET_THREADNAME  = $406D1388;
 DBG_PRINTEXCEPTION_C      = $40010006;
 DBG_PRINTEXCEPTION_WIDE_C = $4001000A;

type
 LPTOP_LEVEL_EXCEPTION_FILTER=function(excep:PEXCEPTION_POINTERS):longint; stdcall;

function SetUnhandledExceptionFilter(lpTopLevelExceptionFilter:Pointer):Pointer;          stdcall; external kernel32 name 'SetUnhandledExceptionFilter';
function AddVectoredExceptionHandler(FirstHandler:DWORD;VectoredHandler:Pointer):Pointer; stdcall; external kernel32 name 'AddVectoredExceptionHandler';
function RemoveVectoredExceptionHandler(VectoredHandlerHandle:Pointer): ULONG;            stdcall; external kernel32 name 'RemoveVectoredExceptionHandler';
function GetModuleHandleEx(dwFlags:DWORD;lpModuleName:Pointer;var hModule:THandle):BOOL;  stdcall; external kernel32 name 'GetModuleHandleExA';
function IsDebuggerPresent: BOOL; stdcall; external kernel32;

function RunErrorCode(const rec: TExceptionRecord): longint;
begin
 { negative result means 'FPU reset required' }
 case rec.ExceptionCode of
   STATUS_INTEGER_DIVIDE_BY_ZERO:      result := 200;    { reDivByZero }
   STATUS_FLOAT_DIVIDE_BY_ZERO:        result := -208;   { !!reZeroDivide }
   STATUS_ARRAY_BOUNDS_EXCEEDED:       result := 201;    { reRangeError }
   STATUS_STACK_OVERFLOW:              result := 202;    { reStackOverflow }
   STATUS_FLOAT_OVERFLOW:              result := -205;   { reOverflow }
   STATUS_FLOAT_DENORMAL_OPERAND,
   STATUS_FLOAT_UNDERFLOW:             result := -206;   { reUnderflow }
   STATUS_FLOAT_INEXACT_RESULT,
   STATUS_FLOAT_INVALID_OPERATION,
   STATUS_FLOAT_STACK_CHECK:           result := -207;   { reInvalidOp }
   STATUS_INTEGER_OVERFLOW:            result := 215;    { reIntOverflow }
   STATUS_ILLEGAL_INSTRUCTION:         result := -216;
   STATUS_ACCESS_VIOLATION:            result := 216;    { reAccessViolation }
   STATUS_CONTROL_C_EXIT:              result := 217;    { reControlBreak }
   STATUS_PRIVILEGED_INSTRUCTION:      result := 218;    { rePrivilegedInstruction }
   STATUS_FLOAT_MULTIPLE_TRAPS,
   STATUS_FLOAT_MULTIPLE_FAULTS:       result := -255;   { indicate FPU reset }
 else
   result := 255;                                        { reExternalException }
 end;
end;

procedure TranslateMxcsr(mxcsr: longword; var code: longint);
begin
 { we can return only one value, further one's are lost }
 { InvalidOp }
 if (mxcsr and 1)<>0 then
   code:=-207
 { Denormal }
 else if (mxcsr and 2)<>0 then
   code:=-206
 { !!reZeroDivide }
 else if (mxcsr and 4)<>0 then
   code:=-208
 { reOverflow }
 else if (mxcsr and 8)<>0 then
   code:=-205
 { Underflow }
 else if (mxcsr and 16)<>0 then
   code:=-206
 { Precision }
 else if (mxcsr and 32)<>0 then
   code:=-207
 else { this should not happen }
   code:=-255
end;

function RunErrorCodeSEH(const rec:TExceptionRecord;const context:TContext):Longint;
begin
 Result:=RunErrorCode(rec);
 if (Result=-255) then
 begin
  TranslateMxcsr(context.MxCsr,result);
 end;
end;

const
 FPC_EXCEPTION_CODE=$E0465043;
 FPC_SET_EH_HANDLER=$E0465044;

function translate_pageflt_err(v:QWORD):Byte; inline;
begin
 Result:=VM_PROT_NONE;
 case v of
  0:Result:=VM_PROT_READ;
  2:Result:=VM_PROT_WRITE;
  8:Result:=VM_PROT_EXECUTE;
 end;
end;

function get_pageflt_err(p:PExceptionPointers):Byte; inline;
begin
 Result:=translate_pageflt_err(p^.ExceptionRecord^.ExceptionInformation[0]);
end;

function get_pageflt_addr(p:PExceptionPointers):QWORD; inline;
begin
 Result:=p^.ExceptionRecord^.ExceptionInformation[1];
end;

function get_exception(p:PExceptionPointers):DWORD; inline;
begin
 Result:=p^.ExceptionRecord^.ExceptionCode;
end;

procedure jit_save_to_sys_save(td:p_kthread); external;
procedure sys_save_to_jit_save(td:p_kthread); external;

function ProcessException3(td:p_kthread;p:PExceptionPointers):longint; SysV_ABI_CDecl;
var
 tf_addr:QWORD;
 rv:Integer;
 is_jit:Boolean;
begin
 Result:=1;
 if (td=nil) then Exit;

 thread_suspend_all(td);

 tf_addr:=0;
 is_jit:=exist_jit_host(p^.ExceptionRecord^.ExceptionAddress,@tf_addr);

 Writeln('cr_rip:0x',HexStr(p^.ContextRecord^.Rip,16));
 Writeln('cr_rsp:0x',HexStr(p^.ContextRecord^.Rsp,16));
 Writeln('cr_rbp:0x',HexStr(p^.ContextRecord^.Rbp,16));

 Writeln('jitcall:0x',HexStr(td^.td_teb^.jitcall));
 print_frame(stderr,td^.td_teb^.jitcall);

 Writeln('tf_rip:0x',HexStr(tf_addr,16));

 _get_frame(p^.ContextRecord,@td^.td_frame,{@td^.td_fpstate}nil,is_jit);

 if (is_jit) then
 begin
  jit_save_to_sys_save(td);
  td^.td_frame.tf_rip:=tf_addr;
 end;

 Writeln('tf_rsp:0x',HexStr(td^.td_frame.tf_rsp,16));
 Writeln('tf_rbp:0x',HexStr(td^.td_frame.tf_rbp,16));

 print_backtrace_td(stderr);

 td^.td_frame.tf_trapno:=0;

 rv:=-1;

 case get_exception(p) of
  STATUS_ACCESS_VIOLATION:
    begin
     tf_addr:=get_pageflt_addr(p);

     Writeln('tf_addr:0x',HexStr(tf_addr,16));

     //Writeln(HexStr(p^.ContextRecord^.Rip,16));
     //Writeln(HexStr(Get_pc_addr));

     //_get_frame(p^.ContextRecord,@td^.td_frame,{@td^.td_fpstate}nil);

     td^.td_frame.tf_trapno:=T_PAGEFLT;
     td^.td_frame.tf_err   :=get_pageflt_err(p);
     td^.td_frame.tf_addr  :=tf_addr;

     rv:=trap.trap(@td^.td_frame,is_jit);
    end;

  else;
 end;

 if (is_jit) then
 begin
  sys_save_to_jit_save(td);
 end;

 if (rv=0) then
 begin
  //_set_frame(p^.ContextRecord,@td^.td_frame,{@td^.td_fpstate}nil);
  Result:=0;
 end;

 thread_resume_all(td);

 //simple ret
 //td^.pcb_flags:=td^.pcb_flags and (not PCB_FULL_IRET);

 //td^.td_frame:=backup;
end;

type
 TExceptObjProc=function(code: Longint; const rec: TExceptionRecord): Pointer; { Exception }
 TExceptClsProc=function(code: Longint): Pointer; { ExceptClass }

function IS_SYSTEM_STACK(td:p_kthread;rsp:qword):Boolean; inline;
begin
 Result:=(rsp<=QWORD(td^.td_kstack.stack)) and (rsp>(QWORD(td^.td_kstack.sttop)));
end;

function IsDefaultExceptions(ExceptionCode:DWORD):Boolean; inline;
begin
 case ExceptionCode of
  EXCEPTION_ACCESS_VIOLATION,
  EXCEPTION_BREAKPOINT,
  EXCEPTION_DATATYPE_MISALIGNMENT,
  EXCEPTION_SINGLE_STEP,
  EXCEPTION_ARRAY_BOUNDS_EXCEEDED,
  EXCEPTION_FLT_DENORMAL_OPERAND,
  EXCEPTION_FLT_DIVIDE_BY_ZERO,
  EXCEPTION_FLT_INEXACT_RESULT,
  EXCEPTION_FLT_INVALID_OPERATION,
  EXCEPTION_FLT_OVERFLOW,
  EXCEPTION_FLT_STACK_CHECK,
  EXCEPTION_FLT_UNDERFLOW,
  EXCEPTION_INT_DIVIDE_BY_ZERO,
  EXCEPTION_INT_OVERFLOW,
  EXCEPTION_INVALID_HANDLE,
  EXCEPTION_PRIV_INSTRUCTION,
  EXCEPTION_NONCONTINUABLE_EXCEPTION,
  EXCEPTION_STACK_OVERFLOW,
  EXCEPTION_INVALID_DISPOSITION,
  EXCEPTION_IN_PAGE_ERROR,
  EXCEPTION_ILLEGAL_INSTRUCTION,
  EXCEPTION_POSSIBLE_DEADLOCK:
    Result:=True;
  else
    Result:=False;
 end;
end;

function ProcessException(p:PExceptionPointers):longint; stdcall;
begin
 Result:=EXCEPTION_CONTINUE_SEARCH;
 if (curkthread=nil) then Exit;

 case get_exception(p) of
  FPC_EXCEPTION_CODE       :Exit;
  FPC_SET_EH_HANDLER       :Exit(EXCEPTION_CONTINUE_EXECUTION);
  EXCEPTION_BREAKPOINT     :Exit;
  EXCEPTION_SINGLE_STEP    :Exit;
  EXCEPTION_SET_THREADNAME :Exit;
  DBG_PRINTEXCEPTION_C     :Exit(EXCEPTION_CONTINUE_EXECUTION);
  DBG_PRINTEXCEPTION_WIDE_C:Exit(EXCEPTION_CONTINUE_EXECUTION); //RenderDoc issuse

  STATUS_ACCESS_VIOLATION:
    begin

     if pmap_danger_zone(vm_map_t(p_proc.p_vmspace)^.pmap,
                         get_pageflt_addr(p),
                         256) then
     begin
      Exit(EXCEPTION_CONTINUE_EXECUTION);
     end;

     case get_pageflt_err(p) of
      VM_PROT_READ:
        begin
         if ((pmap_get_prot(get_pageflt_addr(p),256) and VM_PROT_READ)<>0) then
         begin
          Writeln(stderr,'Unhandled VM_PROT_READ');
         end;
        end;
      VM_PROT_WRITE:
        begin
         if ((pmap_get_prot(get_pageflt_addr(p),256) and VM_PROT_WRITE)<>0) then
         begin
          Writeln(stderr,'Unhandled VM_PROT_WRITE');
         end;
        end;
      else;
     end;

    end;

  else
   if not IsDefaultExceptions(get_exception(p)) then
   begin
    Writeln(stderr,'Unknow ExceptionCode:0x',HexStr(get_exception(p),8));
    Exit;
   end;
 end;

 Result:=ProcessException3(curkthread,p);

 if (Result=0) then
 begin
  Result:=EXCEPTION_CONTINUE_EXECUTION;
 end else
 begin
  Writeln(stderr,'ExceptionCode:0x',HexStr(get_exception(p),8));
  Result:=EXCEPTION_CONTINUE_SEARCH;
 end;
end;

function UnhandledException(p:PExceptionPointers):longint; stdcall;
var
 rec:PExceptionRecord;
 code: Longint;
 ExObj:Exception;
begin
 Result:=EXCEPTION_CONTINUE_SEARCH;

 case get_exception(p) of
  FPC_EXCEPTION_CODE       :Exit;
  FPC_SET_EH_HANDLER       :Exit(EXCEPTION_CONTINUE_EXECUTION);
  EXCEPTION_BREAKPOINT     :Exit;
  EXCEPTION_SET_THREADNAME :Exit;
  DBG_PRINTEXCEPTION_C     :Exit(EXCEPTION_CONTINUE_EXECUTION);
  DBG_PRINTEXCEPTION_WIDE_C:Exit(EXCEPTION_CONTINUE_EXECUTION); //RenderDoc issuse
 end;

 if (curkthread=nil) then Exit;

 rec:=p^.ExceptionRecord;

 code:=abs(RunErrorCodeSEH(rec^,p^.ContextRecord^));

 ExObj:=nil;

 if (rec^.ExceptionCode=FPC_EXCEPTION_CODE) then
 begin
  ExObj:=Exception(rec^.ExceptionInformation[1])
 end else
 if Assigned(ExceptObjProc) then
 begin
  ExObj:=Exception(TExceptObjProc(ExceptObjProc)(abs(code),rec^));
 end;

 if (curkthread<>nil) then
 begin
  Writeln('curkthread^.td_name:',curkthread^.td_name);
 end;

 if (ExObj=nil) then
 begin
  Writeln(stderr,'Runtime error ',code,' at $',hexstr(rec^.ExceptionAddress));
 end else
 begin
  Writeln(stderr,'An unhandled exception occurred at $',hexstr(rec^.ExceptionAddress));
  Writeln(stderr,ExObj.ClassName,': ',ExObj.Message);
 end;

 print_backtrace(stderr,
                 Pointer(p^.ContextRecord^.Rip),
                 Pointer(p^.ContextRecord^.Rbp),0);

 ErrorCode:=word(code);
 md_halt(code);
end;

procedure ExceptionDispatcher(p:PExceptionPointers); stdcall;
begin
 if (ProcessException(p)=EXCEPTION_CONTINUE_EXECUTION) then
 begin
  NtContinue(p^.ContextRecord,False);
 end else
 begin
  UnhandledException(p);
 end;
end;

function _get_msg(Const Msg:Shortstring):Shortstring; inline;
begin
 Result:=Msg;
 if (Result='') then
 begin
  Result:='Assertion failed';
 end;
end;

var
 prev_assert:TAssertErrorProc;

Procedure _Assert(Const Msg,FName:Shortstring;LineNo:Longint;ErrorAddr:Pointer);
begin
 if (curkthread=nil) then
 begin
  prev_assert(Msg,FName,LineNo,ErrorAddr);
  Exit;
 end;

 Writeln(stderr,_get_msg(Msg),' (',FName,', line ',LineNo,').');
 p_host_ipc.error(_get_msg(Msg)+' ('+FName+', line '+IntToStr(LineNo)+').');

 print_backtrace(stderr,Get_pc_addr,get_frame,2);

 if IsDebuggerPresent then
 asm
  int3
 end;

 kern_exit.exit1(217);
end;

var
 VEHandler:Pointer=nil;
 UEHandler:Pointer=nil;
 V2Handler:Pointer=nil;

type
 PEH_HANDLER_INFO=^EH_HANDLER_INFO;
 EH_HANDLER_INFO=record
  ptr:Pointer;
 end;

procedure RaiseException(dwExceptionCode:DWORD;
                         dwExceptionFlags:DWORD;
                         nNumberOfArguments:DWORD;
                         lpArguments:Pointer); external 'kernel32';

procedure InstallExceptionHandler;
var
 eh:EH_HANDLER_INFO;
begin
 prev_assert:=AssertErrorProc;
 AssertErrorProc:=@_Assert;
 //UEHandler:=SetUnhandledExceptionFilter(@UnhandledException);
 VEHandler:=AddVectoredExceptionHandler(1,@ProcessException);
 V2Handler:=AddVectoredExceptionHandler(0,@UnhandledException);
 eh.ptr:=@ExceptionDispatcher;
 RaiseException(FPC_SET_EH_HANDLER,0,2,@eh);
end;

procedure UninstallExceptionHandler;
begin
 //SetUnhandledExceptionFilter(UEHandler);
 RemoveVectoredExceptionHandler(VEHandler);
 RemoveVectoredExceptionHandler(V2Handler);
end;

initialization
 InstallExceptionHandler;

finalization
 UninstallExceptionHandler;

end.
