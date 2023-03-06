unit sys_osem;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function _osem_create_err(name:PChar;attr:DWORD;initCount,maxCount:Integer):Integer;
function _osem_delete_err(key:Integer):Integer;
function _osem_cancel_err(key,setCount:Integer;pNumWait:PInteger):Integer;
function _osem_post_err(key,signalCount:Integer):Integer;
function _osem_trywait_err(key,needCount:Integer):Integer;
function _osem_wait_err(key,needCount:Integer;pTimeout:PDWORD):Integer;

implementation

uses
 kern_osem,
 trap,
 thr_error;

function _osem_create_err(name:PChar;attr:DWORD;initCount,maxCount:Integer):Integer; assembler; nostackframe;
asm
 movq  sys_osem_create,%rax
 call  fast_syscall
end;

function _osem_delete_err(key:Integer):Integer; assembler; nostackframe;
asm
 movq  sys_osem_delete,%rax
 call  fast_syscall
end;

function _osem_cancel_err(key,setCount:Integer;pNumWait:PInteger):Integer; assembler; nostackframe;
asm
 movq  sys_osem_cancel,%rax
 call  fast_syscall
end;

function _osem_post_err(key,signalCount:Integer):Integer; assembler; nostackframe;
asm
 movq  sys_osem_post,%rax
 call  fast_syscall
end;

function _osem_trywait_err(key,needCount:Integer):Integer; assembler; nostackframe;
asm
 movq  sys_osem_trywait,%rax
 call  fast_syscall
end;

function _osem_wait_err(key,needCount:Integer;pTimeout:PDWORD):Integer; assembler; nostackframe;
asm
 movq  sys_osem_wait,%rax
 call  fast_syscall
end;

end.

