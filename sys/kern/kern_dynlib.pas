unit kern_dynlib;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 elf64,
 kern_thr,
 kern_rtld,
 subr_dynlib,
 kern_dlsym,
 kern_reloc;

function sys_dynlib_dlsym(handle:Integer;symbol:pchar;addrp:ppointer):Integer;
function sys_dynlib_process_needed_and_relocate():Integer;
function sys_dynlib_do_copy_relocations():Integer;
function sys_dynlib_load_prx(moduleFileName:pchar;flags:DWORD;pRes:PInteger):Integer;

implementation

uses
 errno,
 systm;

function not_dynamic:Boolean; inline;
begin
 Result:=True;
 if (dynlibs_info.libprogram=nil) then Exit;
 if (dynlibs_info.libprogram^.rel_data=nil) then Exit;
 Result:=False;
end;

function sys_dynlib_dlsym(handle:Integer;symbol:pchar;addrp:ppointer):Integer;
label
 _exit;
var
 lib:p_lib_info;
 flags:Integer;
 ptr:Pointer;

 len:ptruint;
 fsym:array[0..2560-1] of char;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'sys_dynlib_dlsym:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 len:=0;
 Result:=copyinstr(symbol,@fsym,sizeof(fsym),@len);
 if (Result<>0) then Exit;

 dynlibs_lock;

 lib:=find_obj_by_handle(handle);
 if (lib=nil) then
 begin
  Result:=ESRCH;
  goto _exit;
 end;

 flags:=0;
 if (StrLComp(@fsym,'BaOKcng8g88',Length('BaOKcng8g88'))=0) or
    (StrLComp(@fsym,'KpDMrPHvt3Q',Length('KpDMrPHvt3Q'))=0) then
 begin
  flags:=SYMLOOK_BASE64;
 end;

 ptr:=do_dlsym(lib,@fsym,nil,flags);

 if (ptr=nil) then
 begin
  Result:=ESRCH;
  goto _exit;
 end;

 Result:=copyout(@ptr,addrp,SizeOf(Pointer));

 _exit:
  dynlibs_unlock;
end;

function sys_dynlib_process_needed_and_relocate():Integer;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'sys_dynlib_process_needed_and_relocate:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 dynlibs_lock;

 Result:=dynlib_load_needed_shared_objects();
 if (Result=0) then
 begin
  Result:=dynlib_load_relocate();
 end;

 dynlibs_unlock;
end;

function sys_dynlib_do_copy_relocations():Integer;
begin
 dynlibs_lock;

 Result:=check_copy_relocations(dynlibs_info.libprogram);

 dynlibs_unlock;
end;

function sys_dynlib_load_prx(moduleFileName:pchar;flags:DWORD;pRes:PInteger):Integer;
label
 _exit;
var
 len:ptruint;
 fname:array[0..1024-1] of char;

 lib:p_lib_info;
 key:Integer;
 allocs:Boolean;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'sys_dynlib_load_prx:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 //0x10000 //priv libs?
 //0x20000 //reset jmpslots_done?
 //0x40000 //reset on_fini_list?
 if ((flags and $fff8ffff)<>0) then Exit(EINVAL);

 len:=0;
 Result:=copyinstr(moduleFileName,@fname,sizeof(fname),@len);
 if (Result<>0) then Exit;

 dynlibs_lock;

 lib:=nil;
 Result:=load_prx(@fname,flags or ord(budget_ptype_caller=0),lib);
 if (Result=0) then
 begin
  allocs:=(lib^.id<=0);

  if (lib^.ref_count < 2) then
  begin
   if not alloc_obj_id(lib) then
   begin
    //unload_prx(lib);
    Result:=EAGAIN;
    goto _exit;
   end;
  end;
  key:=lib^.id;

  Result:=copyout(@key,pRes,SizeOf(Integer));
  if (Result<>0) then
  begin
   if allocs then
   begin
    free_obj_id(lib^.id);
    lib^.id:=0;
   end;
   //unload_prx(lib);
   Result:=EFAULT;
  end;
 end;

 _exit:
  dynlibs_unlock;

 //dynlib_notify_event(td,resid,0x40);
end;

function sys_dynlib_load_prx(handle:Integer):Integer;
var
 lib:p_lib_info;
 ref,id:Integer;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'sys_dynlib_load_prx:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 dynlibs_lock;

 lib:=find_obj_id(handle);
 if (lib=nil) then
 begin
  Result:=ESRCH;
 end else
 begin
  ref:=lib^.ref_count;
  id :=lib^.id;
  //Result:=unload_prx(lib);
  //
  if (ref=1) then
  begin
   free_obj_id(id);
  end;
 end;

 dynlibs_unlock;
end;




end.

