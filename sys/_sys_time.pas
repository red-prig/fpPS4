unit _sys_time;

{$mode ObjFPC}{$H+}

interface

uses
 time;

function clock_gettime(clock_id:Integer;tp:Ptimespec):Integer;
function clock_getres(clock_id:Integer;tp:Ptimespec):Integer;

implementation

uses
 trap,
 thr_error,
 kern_time;

function clock_gettime(clock_id:Integer;tp:Ptimespec):Integer; assembler; nostackframe;
asm
 movq  sys_clock_gettime,%rax
 call  fast_syscall
 jmp   cerror
end;

function clock_getres(clock_id:Integer;tp:Ptimespec):Integer; assembler; nostackframe;
asm
 movq  sys_clock_getres,%rax
 call  fast_syscall
 jmp   cerror
end;

end.

