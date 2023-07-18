unit kern_reloc;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 elf64,
 kern_thr,
 kern_rtld,
 subr_dynlib;

function relocate_one_object(obj:p_lib_info;jmpslots:Integer):Integer;

implementation

function reloc_non_plt(obj:p_lib_info):Integer;
begin
 Result:=0;
 //////
end;

function reloc_jmplots(obj:p_lib_info):Integer;
begin
 Result:=0;
 //////
end;

function relocate_one_object(obj:p_lib_info;jmpslots:Integer):Integer;
begin
 Result:=reloc_non_plt(obj);
 if (Result<>0) then
 begin
  Writeln(StdErr,'relocate_one_object:','reloc_non_plt() failed. obj=',obj^.lib_path,' rv=',Result);
  Exit;
 end;

 Result:=reloc_jmplots(obj);
 if (Result<>0) then
 begin
  Writeln(StdErr,'relocate_one_object:','reloc_jmplots() failed. obj=',obj^.lib_path,' rv=',Result);
  Exit;
 end;
end;

end.

