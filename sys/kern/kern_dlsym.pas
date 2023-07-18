unit kern_dlsym;

{$mode ObjFPC}{$H+}

interface

uses
 mqueue,
 elf64,
 kern_thr,
 kern_rtld,
 subr_dynlib;

function do_dlsym(obj:p_lib_info;symbol,libname:pchar;flags:DWORD):Pointer;
function find_symdef(symnum:QWORD;refobj:p_lib_info;var defobj_out:p_lib_info;flags:DWORD;cache:p_SymCache):p_elf64_sym;

implementation

function do_dlsym(obj:p_lib_info;symbol,libname:pchar;flags:DWORD):Pointer;
begin
 Result:=nil;
 ////
end;

function find_symdef(symnum:QWORD;refobj:p_lib_info;var defobj_out:p_lib_info;flags:DWORD;cache:p_SymCache):p_elf64_sym;
begin
 Result:=nil;
 ////
end;


end.

