unit sys_sig;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 time,
 signal,
 kern_sig;

Function  sigaction(sig:Integer;
                    act,oact:p_sigaction_t;
                    flags:Integer):Integer;

Function  sigprocmask(how:Integer;
                      _set:p_sigset_t;
                      oset:p_sigset_t;
                      flags:Integer):Integer;

Function  sigpending(oset:p_sigset_t):Integer;

Function  sigwait(oset:p_sigset_t;sig:PInteger):Integer;
Function  sigtimedwait(oset:p_sigset_t;info:p_siginfo_t;timeout:ptimespec):Integer;
Function  sigwaitinfo(oset:p_sigset_t;info:p_siginfo_t):Integer;

Function  sigsuspend(sigmask:p_sigset_t):Integer;

Function  sigaltstack(ss:p_stack_t;oss:p_stack_t):Integer;

implementation

uses
 trap;

Function sigaction(sig:Integer;
                   act,oact:p_sigaction_t;
                   flags:Integer):Integer; assembler; nostackframe;
asm
 movq  sys_sigaction,%rax
 call  fast_syscall
end;

Function sigprocmask(how:Integer;
                     _set:p_sigset_t;
                     oset:p_sigset_t;
                     flags:Integer):Integer; assembler; nostackframe;
asm
 movq  sys_sigprocmask,%rax
 call  fast_syscall
end;

Function sigpending(oset:p_sigset_t):Integer; assembler; nostackframe;
asm
 movq  sys_sigpending,%rax
 call  fast_syscall
end;

Function sigwait(oset:p_sigset_t;sig:PInteger):Integer; assembler; nostackframe;
asm
 movq  sys_sigwait,%rax
 call  fast_syscall
end;

Function sigtimedwait(oset:p_sigset_t;info:p_siginfo_t;timeout:ptimespec):Integer; assembler; nostackframe;
asm
 movq  sys_sigtimedwait,%rax
 call  fast_syscall
end;

Function sigwaitinfo(oset:p_sigset_t;info:p_siginfo_t):Integer; assembler; nostackframe;
asm
 movq  sys_sigwaitinfo,%rax
 call  fast_syscall
end;

Function sigsuspend(sigmask:p_sigset_t):Integer; assembler; nostackframe;
asm
 movq  sys_sigsuspend,%rax
 call  fast_syscall
end;

Function sigaltstack(ss:p_stack_t;oss:p_stack_t):Integer; assembler; nostackframe;
asm
 movq  sys_sigaltstack,%rax
 call  fast_syscall
end;


end.


