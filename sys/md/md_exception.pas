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
  trap,
  signal,
  ucontext,
  vm,
  vmparam;

type
 LPTOP_LEVEL_EXCEPTION_FILTER=function(excep:PEXCEPTION_POINTERS):longint; stdcall;

function SetUnhandledExceptionFilter(lpTopLevelExceptionFilter:Pointer):Pointer;          stdcall; external 'kernel32' name 'SetUnhandledExceptionFilter';
function AddVectoredExceptionHandler(FirstHandler:DWORD;VectoredHandler:Pointer):Pointer; stdcall; external 'kernel32' name 'AddVectoredExceptionHandler';
function RemoveVectoredExceptionHandler(VectoredHandlerHandle:Pointer): ULONG;            stdcall; external 'kernel32' name 'RemoveVectoredExceptionHandler';
function GetModuleHandleEx(dwFlags:DWORD;lpModuleName:Pointer;var hModule:THandle):BOOL;  stdcall; external 'kernel32' name 'GetModuleHandleExA';

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
 backup:trapframe;
 rv:Integer;
begin
 Result:=1;
 td:=curkthread;

 //Context backup to return correctly
 backup:=td^.td_frame;
 td^.td_frame.tf_trapno:=0;

 ExceptionCode:=p^.ExceptionRecord^.ExceptionCode;

 rv:=-1;

 case ExceptionCode of
  STATUS_ACCESS_VIOLATION:
    begin
     tf_addr:=p^.ExceptionRecord^.ExceptionInformation[1];

     //Writeln(HexStr(p^.ContextRecord^.Rip,16));
     //Writeln(HexStr(Get_pc_addr));

     _get_frame(p^.ContextRecord,@td^.td_frame,@td^.td_fpstate);

     td^.td_frame.tf_trapno:=T_PAGEFLT;
     td^.td_frame.tf_err   :=translate_pageflt_err(p^.ExceptionRecord^.ExceptionInformation[0]);
     td^.td_frame.tf_addr  :=tf_addr;

     rv:=trap.trap(@td^.td_frame);
    end;

  else;
 end;

 if (rv=0) then
 begin
  _set_frame(p^.ContextRecord,@td^.td_frame,@td^.td_fpstate);
  Result:=0;
 end;

 //simple ret
 td^.pcb_flags:=td^.pcb_flags and (not PCB_FULL_IRET);

 td^.td_frame:=backup;
end;

function ProcessException2(p:PExceptionPointers):longint; assembler; nostackframe; SysV_ABI_CDecl;
asm
 //prolog (debugger)
 pushq %rbp
 movq  %rsp,%rbp

 movq ProcessException3,%rax
 call fast_syscall

 //epilog (debugger)
 popq  %rbp
end;

type
 TExceptObjProc=function(code: Longint; const rec: TExceptionRecord): Pointer; { Exception }
 TExceptClsProc=function(code: Longint): Pointer; { ExceptClass }

function ProcessException(p:PExceptionPointers):longint; stdcall;
begin
 Result:=EXCEPTION_CONTINUE_SEARCH;

 //Writeln('rsp:0x',HexStr(p^.ContextRecord^.Rsp,16));
 //Writeln('rsp:0x',HexStr(get_frame));

 case p^.ExceptionRecord^.ExceptionCode of
  FPC_EXCEPTION_CODE      :Exit;
  EXCEPTION_BREAKPOINT    :Exit;
  EXCEPTION_SET_THREADNAME:Exit;
 end;

 if (curkthread=nil) then Exit;

 //It looks like there is a small stack inside the exception, so you need to switch the context
 Result:=ProcessException2(p);

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

Procedure _Assert(Const Msg,FName:Shortstring;LineNo:Longint;ErrorAddr:Pointer);
begin
 If Length(msg)=0 then
   write(stderr,'Assertion failed')
 else
   write(stderr,msg);
 Writeln(stderr,' (',FName,', line ',LineNo,').');
 print_backtrace(stderr,Get_pc_addr,get_frame,0);
 asm
  int3
 end;
 md_halt(217);
end;

var
 VEHandler:Pointer=nil;
 UEHandler:Pointer=nil;
 V2Handler:Pointer=nil;

procedure InstallExceptionHandler;
begin
 AssertErrorProc:=@_Assert;
 UEHandler:=SetUnhandledExceptionFilter(@UnhandledException);
 VEHandler:=AddVectoredExceptionHandler(1,@ProcessException);
 V2Handler:=AddVectoredExceptionHandler(0,@UnhandledException);
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
