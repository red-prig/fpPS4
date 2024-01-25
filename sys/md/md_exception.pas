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
  kern_thr,
  subr_backtrace,
  trap,
  signal,
  ucontext,
  vm;

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

function translate_pageflt_err(v:QWORD):QWORD; inline;
begin
 Result:=VM_PROT_NONE;
 case v of
  0:Result:=VM_PROT_READ;
  2:Result:=VM_PROT_WRITE;
  8:Result:=VM_PROT_EXECUTE;
 end;
end;

function ProcessException3(p:PExceptionPointers):longint; SysV_ABI_CDecl;
var
 ExceptionCode:DWORD;
 td:p_kthread;
 tf_addr:QWORD;
 //backup:trapframe;
 rv:Integer;
begin
 Result:=1;
 td:=curkthread;

 //Context backup to return correctly
 //backup:=td^.td_frame;

 td^.td_frame.tf_trapno:=0;

 ExceptionCode:=p^.ExceptionRecord^.ExceptionCode;

 rv:=-1;

 case ExceptionCode of
  STATUS_ACCESS_VIOLATION:
    begin
     tf_addr:=p^.ExceptionRecord^.ExceptionInformation[1];

     //Writeln(HexStr(p^.ContextRecord^.Rip,16));
     //Writeln(HexStr(Get_pc_addr));

     _get_frame(p^.ContextRecord,@td^.td_frame,{@td^.td_fpstate}nil);

     td^.td_frame.tf_trapno:=T_PAGEFLT;
     td^.td_frame.tf_err   :=translate_pageflt_err(p^.ExceptionRecord^.ExceptionInformation[0]);
     td^.td_frame.tf_addr  :=tf_addr;

     rv:=trap.trap(@td^.td_frame);
    end;

  else;
 end;

 if (rv=0) then
 begin
  _set_frame(p^.ContextRecord,@td^.td_frame,{@td^.td_fpstate}nil);
  Result:=0;
 end;

 //simple ret
 //td^.pcb_flags:=td^.pcb_flags and (not PCB_FULL_IRET);

 //td^.td_frame:=backup;
end;

{
//rdi,rsi
function ProcessException2(p:PExceptionPointers;rsp:Pointer):longint; assembler; nostackframe; SysV_ABI_CDecl;
asm
 //prolog (debugger)
 //pushq %rbp
 //movq  %rsp,%rbp

 xchg  %rsi,%rsp
 andq  $-32,%rsp //align stack

 pushq %rsi
 pushq %rbp

 call  ProcessException3

 popq  %rbp
 popq  %rsp

 //movq ProcessException3,%rax
 ///call fast_syscall

 //epilog (debugger)
 //popq  %rbp
end;
}

type
 TExceptObjProc=function(code: Longint; const rec: TExceptionRecord): Pointer; { Exception }
 TExceptClsProc=function(code: Longint): Pointer; { ExceptClass }

function IS_SYSTEM_STACK(td:p_kthread;rsp:qword):Boolean; inline;
begin
 Result:=(rsp<=QWORD(td^.td_kstack.stack)) and (rsp>(QWORD(td^.td_kstack.sttop)));
end;

function ProcessException(p:PExceptionPointers):longint; stdcall;
var
 td:p_kthread;
begin
 Result:=EXCEPTION_CONTINUE_SEARCH;

 //Writeln('  p_ctx:0x',HexStr(p));
 //Writeln('rsp_ctx:0x',HexStr(p^.ContextRecord^.Rsp,16));
 //Writeln('rsp_frm:0x',HexStr(get_frame));

 case p^.ExceptionRecord^.ExceptionCode of
  FPC_EXCEPTION_CODE      :Exit;
  FPC_SET_EH_HANDLER      :Exit(EXCEPTION_CONTINUE_EXECUTION);
  EXCEPTION_BREAKPOINT    :Exit;
  EXCEPTION_SET_THREADNAME:Exit;
 end;

 //readln;

 td:=curkthread;

 if (td=nil) then Exit;

 {
 if (td^.td_teb^.jit_rsp<>nil) then //debugger switch
 begin
  Writeln('td_kstack:0x',HexStr(td^.td_kstack.stack));
  Writeln('jit_rsp:0x',HexStr(td^.td_teb^.jit_rsp));
  p^.ContextRecord^.Rsp:=qword(td^.td_teb^.jit_rsp);
  td^.td_teb^.jit_rsp:=nil;
 end;
 }

 Result:=ProcessException3(p);

 {
 if IS_SYSTEM_STACK(curkthread,p^.ContextRecord^.Rsp) then
 begin
  Writeln(IS_SYSTEM_STACK(curkthread,p^.ContextRecord^.Rsp),' ',IS_SYSTEM_STACK(curkthread,qword(get_frame)));
  //
  Exit;
 end else
 begin
  //It looks like there is a small stack inside the exception, so you need to switch the context
  Result:=ProcessException2(p,curkthread^.td_kstack.stack);
 end;
 }

 if (Result=0) then
 begin
  Result:=EXCEPTION_CONTINUE_EXECUTION;
 end else
 begin
  Result:=EXCEPTION_CONTINUE_SEARCH;
 end;
end;

function UnhandledException(p:PExceptionPointers):longint; stdcall;
var
 td:p_kthread;
 rec:PExceptionRecord;
 code: Longint;
 ExObj:Exception;
begin
 Result:=EXCEPTION_CONTINUE_SEARCH;

 case p^.ExceptionRecord^.ExceptionCode of
  FPC_EXCEPTION_CODE      :Exit;
  FPC_SET_EH_HANDLER      :Exit(EXCEPTION_CONTINUE_EXECUTION);
  EXCEPTION_BREAKPOINT    :Exit;
  EXCEPTION_SET_THREADNAME:Exit;
 end;

 td:=curkthread;
 if (td=nil) then Exit;

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

 if curkthread<>nil then
  Writeln('curkthread^.td_name:',curkthread^.td_name);

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

Procedure _Assert(Const Msg,FName:Shortstring;LineNo:Longint;ErrorAddr:Pointer);
begin
 Writeln(stderr,_get_msg(Msg),' (',FName,', line ',LineNo,').');
 print_backtrace(stderr,Get_pc_addr,get_frame,2);

 sleep(-1);

 if IsDebuggerPresent then
  Raise EAssertionFailed.
         Createfmt('%s (%s, line %d).',
         [_get_msg(Msg),FName,LineNo])
         at get_caller_addr (ErrorAddr),
            get_caller_frame(ErrorAddr);

 //asm
 // int3
 //end;

 md_halt(217);
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
 AssertErrorProc:=@_Assert;
 UEHandler:=SetUnhandledExceptionFilter(@UnhandledException);
 VEHandler:=AddVectoredExceptionHandler(1,@ProcessException);
 V2Handler:=AddVectoredExceptionHandler(0,@UnhandledException);
 eh.ptr:=@ExceptionDispatcher;
 RaiseException(FPC_SET_EH_HANDLER,0,2,@eh);
end;

procedure UninstallExceptionHandler;
begin
 SetUnhandledExceptionFilter(UEHandler);
 RemoveVectoredExceptionHandler(VEHandler);
 RemoveVectoredExceptionHandler(V2Handler);
end;

initialization
 InstallExceptionHandler;

finalization
 UninstallExceptionHandler;

end.
