unit thr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 time,
 kern_thread;

type
 p_thr_param=kern_thread.p_thr_param;
 thr_param  =kern_thread.thr_param;

function  thr_new(param:p_thr_param;param_size:Integer):Integer;
function  thr_self(id:PQWORD):Integer;
procedure thr_exit(state:PQWORD);
function  thr_kill(id:QWORD;sig:Integer):Integer;
function  thr_suspend(timeout:ptimespec):Integer;

//int  thr_create(ucontext_t *ctx, long *id, int flags);

//int  thr_kill2(pid_t pid, long id, int sig);
//int  thr_wake(long id);
//int  thr_set_name(long id, const char *name);

implementation

uses
 trap;

function thr_new(param:p_thr_param;param_size:Integer):Integer;
begin
 if (curkthread=nil) then
 begin
  Result:=sys_thr_new(param,param_size);
 end else
 asm
  movq  sys_thr_new,%rax
  call  fast_syscall
 end;
end;

function thr_self(id:PQWORD):Integer; assembler; nostackframe;
asm
 movq  sys_thr_self,%rax
 call  fast_syscall
end;

procedure thr_exit(state:PQWORD); assembler; nostackframe;
asm
 movq  sys_thr_exit,%rax
 call  fast_syscall
end;

function thr_kill(id:QWORD;sig:Integer):Integer; assembler; nostackframe;
asm
 movq  sys_thr_kill,%rax
 call  fast_syscall
end;

function thr_suspend(timeout:ptimespec):Integer; assembler; nostackframe;
asm
 movq  sys_thr_suspend,%rax
 call  fast_syscall
end;


end.



