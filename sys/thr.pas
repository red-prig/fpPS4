unit thr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ucontext,
 kern_thr,
 kern_thread;

type
 p_thr_param=kern_thr.p_thr_param;
 thr_param  =kern_thr.thr_param;

function  thr_new(param:p_thr_param;param_size:Integer):Integer;
function  thr_create(ctx:p_ucontext_t;id:PDWORD;flags:Integer):Integer;

function  amd64_set_fsbase(addr:Pointer):Integer;

implementation

uses
 syscalls,
 sys_machdep,
 trap,
 thr_error;

function thr_new(param:p_thr_param;param_size:Integer):Integer;
begin
 if (curkthread=nil) then
 begin
  Result:=sys_thr_new(param,param_size);
 end else
 asm
  movq  $455,%rax
  call  fast_syscall
  jmp   cerror
 end;
end;

function thr_create(ctx:p_ucontext_t;id:PDWORD;flags:Integer):Integer;
begin
 if (curkthread=nil) then
 begin
  Result:=sys_thr_create(ctx,id,flags);
 end else
 asm
  movq  $455,%rax
  call  fast_syscall
  jmp   cerror
 end;
end;

function amd64_set_fsbase(addr:Pointer):Integer;
begin
 Result:=sysarch(sys_machdep.AMD64_SET_FSBASE,@addr);
end;

end.



