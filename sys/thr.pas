unit thr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 time,
 kern_thr,
 kern_thread;

type
 p_thr_param=kern_thr.p_thr_param;
 thr_param  =kern_thr.thr_param;

function  getpid:Integer;

function  thr_new(param:p_thr_param;param_size:Integer):Integer;
function  thr_self(id:PQWORD):Integer;
procedure thr_exit(state:PQWORD);
function  thr_kill(id:QWORD;sig:Integer):Integer;
function  thr_suspend(timeout:ptimespec):Integer;
function  thr_wake(id:QWORD):Integer;
function  thr_set_name(id:QWORD;name:PChar):Integer;

//int  thr_create(ucontext_t *ctx, long *id, int flags);
//int  thr_kill2(pid_t pid, long id, int sig);

function  amd64_set_fsbase(base:Pointer):Integer;

implementation

uses
 trap,
 thr_error,
 vm_machdep;

function getpid:Integer;
begin
 Result:=g_pid;
end;

function thr_new(param:p_thr_param;param_size:Integer):Integer;
begin
 if (curkthread=nil) then
 begin
  Result:=sys_thr_new(param,param_size);
 end else
 asm
  movq  sys_thr_new,%rax
  call  fast_syscall
  jmp   cerror
 end;
end;

function thr_self(id:PQWORD):Integer; assembler; nostackframe;
asm
 movq  sys_thr_self,%rax
 call  fast_syscall
 jmp   cerror
end;

procedure thr_exit(state:PQWORD); assembler; nostackframe;
asm
 movq  sys_thr_exit,%rax
 call  fast_syscall
 jmp   cerror
end;

function thr_kill(id:QWORD;sig:Integer):Integer; assembler; nostackframe;
asm
 movq  sys_thr_kill,%rax
 call  fast_syscall
 jmp   cerror
end;

function thr_suspend(timeout:ptimespec):Integer; assembler; nostackframe;
asm
 movq  sys_thr_suspend,%rax
 call  fast_syscall
 jmp   cerror
end;

function thr_wake(id:QWORD):Integer; assembler; nostackframe;
asm
 movq  sys_thr_wake,%rax
 call  fast_syscall
 jmp   cerror
end;

function thr_set_name(id:QWORD;name:PChar):Integer; assembler; nostackframe;
asm
 movq  sys_thr_set_name,%rax
 call  fast_syscall
end;

function amd64_set_fsbase(base:Pointer):Integer; assembler; nostackframe;
asm
 movq  sys_amd64_set_fsbase,%rax
 call  fast_syscall
 jmp   cerror
end;

end.



