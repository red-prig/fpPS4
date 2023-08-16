unit md_exception;

{$mode objfpc}{$H+}

interface

implementation

uses
  Windows,
  ntapi,
  machdep,
  md_context,
  kern_thr,
  trap,
  signal,
  ucontext,
  vm,
  vmparam;

function AddVectoredExceptionHandler(FirstHandler: DWORD; VectoredHandler: pointer): pointer; stdcall;
  external 'kernel32.dll' name 'AddVectoredExceptionHandler';
function RemoveVectoredExceptionHandler(VectoredHandlerHandle: pointer): ULONG; stdcall;
  external 'kernel32.dll' name 'RemoveVectoredExceptionHandler';  
function GetModuleHandleEx(dwFlags: DWORD; lpModuleName: pointer; var hModule: THandle): BOOL; stdcall;
  external 'kernel32.dll' name 'GetModuleHandleExA';

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

function RunErrorCodex64(const rec: TExceptionRecord; const context: TContext): Longint;
begin
 Result:=RunErrorCode(rec);
 if (Result=-255) then
   TranslateMxcsr(context.MxCsr,result);
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

function ProcessException(p:PExceptionPointers):longint; stdcall;
var
 ExceptionCode:DWORD;
 td:p_kthread;
 tf_addr:QWORD;
 uc:ucontext_t;
 rv:Integer;
begin
 Result:=EXCEPTION_CONTINUE_SEARCH;

 td:=curkthread;
 if (td=nil) then Exit;

 if not is_guest_addr(QWORD(p^.ExceptionRecord^.ExceptionAddress)) then Exit;

 td^.td_frame.tf_trapno:=0;

 ExceptionCode:=p^.ExceptionRecord^.ExceptionCode;

 rv:=-1;

 case ExceptionCode of
  FPC_EXCEPTION_CODE:;

  STATUS_ACCESS_VIOLATION:
    begin
     tf_addr:=p^.ExceptionRecord^.ExceptionInformation[1];

     uc:=Default(ucontext_t);
     _get_ucontext(p^.ContextRecord,@uc);
     set_mcontext(td, @uc.uc_mcontext);

     td^.td_frame.tf_trapno:=T_PAGEFLT;
     td^.td_frame.tf_err   :=translate_pageflt_err(p^.ExceptionRecord^.ExceptionInformation[0]);
     td^.td_frame.tf_addr  :=tf_addr;

     rv:=trap.trap(@td^.td_frame);
    end;

  else;
 end;

 if (rv=0) then
 begin
  get_mcontext(td, @uc.uc_mcontext, TF_HASFPXSTATE);
  _set_ucontext(p^.ContextRecord,@uc);
  Exit(EXCEPTION_CONTINUE_EXECUTION);
 end;

end;

var
 VEHandler:pointer=nil;

procedure InstallExceptionHandler;
begin
 //SetUnhandledExceptionFilter
 VEHandler:=AddVectoredExceptionHandler(1,@ProcessException);
end;

procedure UninstallExceptionHandler;
begin
 if Assigned(VEHandler) then
 begin
  RemoveVectoredExceptionHandler(VEHandler);
  VEHandler:=nil;
 end;
end;

initialization
 InstallExceptionHandler;

finalization
 UninstallExceptionHandler;

end.
