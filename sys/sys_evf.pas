unit sys_evf;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function evf_create(name:PChar;attr:DWORD;initPattern:QWORD):Integer;
function _evf_delete_err(key:Integer):Integer;
function _evf_cancel_err(key:Integer;setPattern:QWORD;pNumWait:PInteger):Integer;
function _evf_clear_err(key:Integer;bitPattern:QWORD):Integer;
function _evf_set_err(key:Integer;bitPattern:QWORD):Integer;
function _evf_trywait_err(key:Integer;bitPattern:QWORD;waitMode:DWORD;pRes:PQWORD):Integer;
function _evf_wait_err(key:Integer;bitPattern:QWORD;waitMode:DWORD;pRes:PQWORD;pTimeout:PDWORD):Integer;

implementation

uses
 kern_evf,
 trap,
 thr_error;

function evf_create(name:PChar;attr:DWORD;initPattern:QWORD):Integer; assembler; nostackframe;
asm
 movq  sys_evf_create,%rax
 call  fast_syscall
 call  cerror
end;

function _evf_delete_err(key:Integer):Integer; assembler; nostackframe;
asm
 movq  sys_evf_delete,%rax
 call  fast_syscall
end;

function _evf_cancel_err(key:Integer;setPattern:QWORD;pNumWait:PInteger):Integer; assembler; nostackframe;
asm
 movq  sys_evf_cancel,%rax
 call  fast_syscall
end;

function _evf_clear_err(key:Integer;bitPattern:QWORD):Integer; assembler; nostackframe;
asm
 movq  sys_evf_clear,%rax
 call  fast_syscall
end;

function _evf_set_err(key:Integer;bitPattern:QWORD):Integer; assembler; nostackframe;
asm
 movq  sys_evf_set,%rax
 call  fast_syscall
end;

function _evf_trywait_err(key:Integer;bitPattern:QWORD;waitMode:DWORD;pRes:PQWORD):Integer; assembler; nostackframe;
asm
 movq  sys_evf_trywait,%rax
 call  fast_syscall
end;

function _evf_wait_err(key:Integer;bitPattern:QWORD;waitMode:DWORD;pRes:PQWORD;pTimeout:PDWORD):Integer; assembler; nostackframe;
asm
 movq  sys_evf_wait,%rax
 call  fast_syscall
end;

end.

