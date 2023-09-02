unit kern_dynlib;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 kern_rtld,
 subr_dynlib,
 kern_dlsym,
 kern_reloc;

const
 SCE_DBG_MAX_NAME_LENGTH = 256;
 SCE_DBG_MAX_SEGMENTS    = 4;
 SCE_DBG_NUM_FINGERPRINT = 20;

type
 SceKernelModuleSegmentInfo=packed record
  addr:Pointer;
  size:DWORD;
  prot:Integer;  //PF_
 end;

 SceDynlibName=array[0..SCE_DBG_MAX_NAME_LENGTH-1] of AnsiChar;

 pSceKernelModuleInfo=^SceKernelModuleInfo;
 SceKernelModuleInfo=packed record
  size        :QWORD;  //Size of this structure
  name        :SceDynlibName; //name.prx
  segmentInfo :array[0..SCE_DBG_MAX_SEGMENTS-1] of SceKernelModuleSegmentInfo;
  segmentCount:DWORD;
  fingerprint :array[0..SCE_DBG_NUM_FINGERPRINT-1] of Byte;
 end;
 {$IF sizeof(SceKernelModuleInfo)<>352}{$STOP sizeof(SceKernelModuleInfo)<>352}{$ENDIF}

 pSceKernelModuleInfoEx=^SceKernelModuleInfoEx;
 SceKernelModuleInfoEx=packed record
  st_size          :QWORD; //424
  name             :SceDynlibName; //name.prx
  id               :Integer;
  tls_index        :DWORD;
  tls_init_addr    :Pointer;
  tls_init_size    :DWORD;
  tls_size         :DWORD;
  tls_offset       :DWORD;
  tls_align        :DWORD;
  init_proc_addr   :Pointer;
  fini_proc_addr   :Pointer;
  reserved1        :QWORD;
  reserved2        :QWORD;
  eh_frame_hdr_addr:Pointer;
  eh_frame_addr    :Pointer;
  eh_frame_hdr_size:DWORD;
  eh_frame_size    :DWORD;
  segments         :array[0..SCE_DBG_MAX_SEGMENTS-1] of SceKernelModuleSegmentInfo;
  segment_count    :DWORD;
  ref_count        :DWORD;
 end;
 {$IF sizeof(SceKernelModuleInfoEx)<>424}{$STOP sizeof(SceKernelModuleInfoEx)<>424}{$ENDIF}

 pSceModuleInfoForUnwind=^SceModuleInfoForUnwind;
 SceModuleInfoForUnwind=packed record
  st_size          :qword; //304
  name             :SceDynlibName; //name.prx
  eh_frame_hdr_addr:Pointer;
  eh_frame_addr    :Pointer;
  eh_frame_size    :qword;
  seg0_addr        :Pointer;
  seg0_size        :qword;
 end;

function sys_dynlib_dlsym(handle:Integer;symbol:pchar;addrp:ppointer):Integer;
function sys_dynlib_process_needed_and_relocate():Integer;
function sys_dynlib_do_copy_relocations():Integer;
function sys_dynlib_load_prx(moduleFileName:pchar;flags:DWORD;pRes:PInteger;unused:Pointer):Integer;
function sys_dynlib_unload_prx(handle:Integer;args:QWORD;argp:Pointer):Integer;
function sys_dynlib_get_info(handle:Integer;info:Pointer):Integer;
function sys_dynlib_get_info2(handle:Integer;info:Pointer):Integer;
function sys_dynlib_get_info_ex(handle,flags:Integer;info:Pointer):Integer;
function sys_dynlib_get_info_for_libdbg(handle:Integer;info:Pointer):Integer;
function sys_dynlib_get_list(pArray:PInteger;numArray:QWORD;pActualNum:PQWORD):Integer;
function sys_dynlib_get_list2(pArray:PInteger;numArray:QWORD;pActualNum:PQWORD):Integer;
function sys_dynlib_get_list_for_libdbg(pArray:PInteger;numArray:QWORD;pActualNum:PQWORD):Integer;
function sys_dynlib_get_obj_member(handle:Integer;num:Byte;pout:PPointer):Integer;
function sys_dynlib_get_proc_param(pout:PPointer;psize:PQWORD):Integer;
function sys_dl_get_info(pid,handle:Integer;pout:PPointer):Integer;
function sys_dl_get_list(pid:Integer;pArray:PInteger;numArray:Integer;pActualNum:PInteger):Integer;
function sys_dl_get_metadata(pid,handle:Integer;pout:Pointer;size:Integer;pactual_size:PInteger):Integer;

implementation

uses
 errno,
 systm,
 vm;

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
 obj:p_lib_info;
 flags:Integer;
 ptr:Pointer;

 fsym:array[0..2560-1] of char;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'sys_dynlib_dlsym:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 Result:=copyinstr(symbol,@fsym,sizeof(fsym),nil);
 if (Result<>0) then Exit;

 Writeln('sys_dynlib_dlsym:',fsym);

 dynlibs_lock;

 obj:=find_obj_by_handle(handle);
 if (obj=nil) then
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

 ptr:=do_dlsym(obj,@fsym,nil,flags);

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

function sys_dynlib_load_prx(moduleFileName:pchar;flags:DWORD;pRes:PInteger;unused:Pointer):Integer;
label
 _exit;
var
 len:ptruint;
 fname:array[0..1024-1] of char;

 obj:p_lib_info;
 key:Integer;
 allocs:Boolean;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'sys_dynlib_load_prx:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 //0x10000 //priv libs?
 //0x20000 //set jmpslots_done?
 //0x40000 //set not_get_proc?
 if ((flags and $fff8ffff)<>0) then Exit(EINVAL);

 len:=0;
 Result:=copyinstr(moduleFileName,@fname,sizeof(fname),@len);
 if (Result<>0) then Exit;

 dynlibs_lock;

 obj:=nil;
 Result:=load_prx(@fname,flags or ord(budget_ptype_caller=0),obj);
 if (Result=0) then
 begin
  allocs:=(obj^.id<=0);

  if (obj^.ref_count < 2) then
  begin
   if not alloc_obj_id(obj) then
   begin
    unload_prx(obj);
    Result:=EAGAIN;
    goto _exit;
   end;
  end;
  key:=obj^.id;

  Result:=copyout(@key,pRes,SizeOf(Integer));
  if (Result<>0) then
  begin
   if allocs then
   begin
    free_obj_id(obj^.id);
    obj^.id:=0;
   end;
   unload_prx(obj);
   Result:=EFAULT;
  end;
 end;

 _exit:
  dynlibs_unlock;

 //dynlib_notify_event(td,resid,0x40);
end;

function sys_dynlib_unload_prx(handle:Integer;args:QWORD;argp:Pointer):Integer;
var
 obj:p_lib_info;
 ref,id:Integer;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'sys_dynlib_unload_prx:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 dynlibs_lock;

 obj:=find_obj_id(handle);
 if (obj=nil) then
 begin
  Result:=ESRCH;
 end else
 begin
  ref:=obj^.ref_count;
  id :=obj^.id;
  //
  Result:=unload_prx(obj);
  //
  if (ref=1) then
  begin
   free_obj_id(id);
  end;
 end;

 dynlibs_unlock;
end;

function kern_dynlib_get_info(handle,flags:Integer;dst:pSceKernelModuleInfo):Integer;
var
 obj:p_lib_info;
 lib_path:pchar;
 lib_name:pchar;
begin
 Result:=ESRCH;

 dynlibs_lock;

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  if (obj^.id=handle) then
  begin
   dst^:=Default(SceKernelModuleInfo);

   if ((flags and 1)=0) and (obj^.is_system<>0) then
   begin
    Result:=EPERM;
    Break;
   end;

   lib_path:=obj^.lib_path;
   lib_name:=dynlib_basename(lib_path);

   strlcopy(dst^.name,lib_name,SCE_DBG_MAX_NAME_LENGTH);

   //if not is_webkit then
   begin
    dst^.segmentCount:=2;
    dst^.segmentInfo[0].addr:=obj^.map_base;
    dst^.segmentInfo[0].size:=obj^.text_size;
    dst^.segmentInfo[0].prot:=VM_PROT_READ or VM_PROT_EXECUTE;
    dst^.segmentInfo[1].addr:=obj^.data_addr;
    dst^.segmentInfo[1].size:=obj^.data_size;
    dst^.segmentInfo[1].prot:=VM_PROT_RW;
    if (obj^.relro_addr<>nil) and (obj^.relro_size<>0) then
    begin
     dst^.segmentInfo[2].addr:=obj^.relro_addr;
     dst^.segmentInfo[2].size:=obj^.relro_size;
     dst^.segmentInfo[2].prot:=VM_PROT_READ;
     dst^.segmentCount:=3;
    end;
   end;

   Move(obj^.fingerprint,dst^.fingerprint,SCE_DBG_NUM_FINGERPRINT);

   Result:=0;
   Break;
  end;
  //
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;

 dynlibs_unlock;
end;

function sys_dynlib_get_info(handle:Integer;info:Pointer):Integer;
var
 size:QWORD;
 dst:SceKernelModuleInfo;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'sys_dynlib_get_info:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 size:=0;
 Result:=copyin(info,@size,SizeOf(QWORD));
 if (Result<>0) then
 begin
  Exit(EFAULT);
 end;

 if (size<>SizeOf(SceKernelModuleInfo)) then
 begin
  Exit(EINVAL);
 end;

 Result:=kern_dynlib_get_info(handle,1,@dst);
 if (Result<>0) then
 begin
  Exit(ESRCH);
 end;

 Result:=copyout(@dst,info,SizeOf(SceKernelModuleInfo));
end;

function sys_dynlib_get_info2(handle:Integer;info:Pointer):Integer;
var
 size:QWORD;
 dst:SceKernelModuleInfo;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'sys_dynlib_get_info2:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 size:=0;
 Result:=copyin(info,@size,SizeOf(QWORD));
 if (Result<>0) then
 begin
  Exit(EFAULT);
 end;

 if (size<>SizeOf(SceKernelModuleInfo)) then
 begin
  Exit(EINVAL);
 end;

 Result:=kern_dynlib_get_info(handle,0,@dst);
 if (Result<>0) then
 begin
  Exit(ESRCH);
 end;

 Result:=copyout(@dst,info,SizeOf(SceKernelModuleInfo));
end;

function kern_dynlib_get_info_ex(handle,flags:Integer;dst:pSceKernelModuleInfoEx):Integer;
var
 obj:p_lib_info;
 lib_path:pchar;
 lib_name:pchar;
 tls_index:Integer;
begin
 Result:=ESRCH;

 dynlibs_lock;

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  if (obj^.id=handle) then
  begin
   dst^:=Default(SceKernelModuleInfoEx);

   lib_path:=obj^.lib_path;
   lib_name:=dynlib_basename(lib_path);

   strlcopy(dst^.name,lib_name,SCE_DBG_MAX_NAME_LENGTH);

   dst^.id:=obj^.id;

   tls_index:=obj^.tls_index;
   dst^.tls_index:=tls_index;

   if ((flags and 1)<>0) then
   begin
    tls_index:=((ord(obj^.is_system<>0) + ord(obj^.mainprog<>0)*2) * $10000) or tls_index;
    dst^.tls_index:=tls_index;
   end;

   if ((flags and 2)<>0) then
   begin
    if (obj^.is_system<>0) {or (is_webkit)} then
    begin
     FillChar(dst^.name,SCE_DBG_MAX_NAME_LENGTH,0);
    end;
   end;

   dst^.tls_init_addr    :=obj^.tls_init_addr;
   dst^.tls_init_size    :=obj^.tls_init_size;
   dst^.tls_size         :=obj^.tls_size;
   dst^.tls_offset       :=obj^.tls_offset;
   dst^.tls_align        :=obj^.tls_align;
   dst^.reserved1        :=0;
   dst^.reserved2        :=0;
   dst^.eh_frame_hdr_addr:=obj^.eh_frame_hdr_addr;
   dst^.eh_frame_addr    :=obj^.eh_frame_addr;
   dst^.eh_frame_hdr_size:=obj^.eh_frame_hdr_size;
   dst^.eh_frame_size    :=obj^.eh_frame_size;
   dst^.ref_count        :=obj^.ref_count;

   //if not webkit then
   begin
    if (obj^.not_get_proc=0) then
    begin
     dst^.init_proc_addr:=obj^.init_proc_addr;
     dst^.fini_proc_addr:=obj^.fini_proc_addr;
    end;

    dst^.segments[0].addr:=obj^.map_base;
    dst^.segments[0].size:=obj^.text_size;
    dst^.segments[0].prot:=VM_PROT_READ or VM_PROT_EXECUTE;
    dst^.segments[1].addr:=obj^.data_addr;
    dst^.segments[1].size:=obj^.data_size;
    dst^.segments[1].prot:=VM_PROT_RW;
    dst^.segment_count:=2;
   end;

   Result:=0;
   Break;
  end;
  //
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;

 dynlibs_unlock;
end;

function sys_dynlib_get_info_ex(handle,flags:Integer;info:Pointer):Integer;
var
 size:QWORD;
 dst:SceKernelModuleInfoEx;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'sys_dynlib_get_info_ex:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 size:=0;
 Result:=copyin(info,@size,SizeOf(QWORD));
 if (Result<>0) then
 begin
  Exit(EFAULT);
 end;

 if (size<>SizeOf(SceKernelModuleInfoEx)) then
 begin
  Exit(EINVAL);
 end;

 Result:=kern_dynlib_get_info_ex(handle,flags,@dst);
 if (Result<>0) then
 begin
  Exit(ESRCH);
 end;

 Result:=copyout(@dst,info,SizeOf(SceKernelModuleInfoEx));
end;

function sys_dynlib_get_info_for_libdbg(handle:Integer;info:Pointer):Integer;
begin
 Exit(EPERM);  //sceKernelIsDevelopmentMode
end;

function copyout_module_handle_list(pArray:PInteger;numArray:QWORD;flags:DWORD;pActualNum:PQWORD):Integer;
var
 i,count:QWORD;
 src:PInteger;
 obj:p_lib_info;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'copyout_module_handle_list:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 dynlibs_lock;

 i:=0;
 count:=dynlibs_info.obj_count;

 if (((flags and 1)<>0) and (count > numArray)) then
 begin
  dynlibs_unlock;
  Exit(ENOMEM);
 end;

 src:=AllocMem(count*SizeOf(Integer));

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) and (i<count) do
 begin
  if ((flags and 1)<>0) or (obj^.is_system=0) then
  begin
   if (numArray<=i) then
   begin
    dynlibs_unlock;
    FreeMem(src);
    Exit(ENOMEM);
   end;
   src[i]:=obj^.id;
  end;
  //
  Inc(i);
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;

 dynlibs_unlock;

 if (i<>count) and ((flags and 1)<>0) then
 begin
  Writeln(StdErr,'copyout_module_handle_list:','WARNING: num<>dp^.obj_count');
 end;

 Result:=copyout(src,pArray,i*SizeOf(Integer));

 if (Result=0) then
 begin
  Result:=copyout(@i,pActualNum,8);
 end;

 FreeMem(src);
end;

function sys_dynlib_get_list(pArray:PInteger;numArray:QWORD;pActualNum:PQWORD):Integer;
begin
 Result:=copyout_module_handle_list(pArray,numArray,1,pActualNum);
end;

function sys_dynlib_get_list2(pArray:PInteger;numArray:QWORD;pActualNum:PQWORD):Integer;
begin
 Result:=copyout_module_handle_list(pArray,numArray,0,pActualNum);
end;

function sys_dynlib_get_list_for_libdbg(pArray:PInteger;numArray:QWORD;pActualNum:PQWORD):Integer;
begin
 Exit(EPERM);  //sceKernelIsDevelopmentMode
end;

function sys_dynlib_get_obj_member(handle:Integer;num:Byte;pout:PPointer):Integer;
var
 obj:p_lib_info;
 dst:Pointer;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'sys_dynlib_get_obj_member:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 dynlibs_lock;

 Result:=ESRCH;

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  if (obj^.id=handle) then
  begin
   dst:=nil;

   case num of
    1:if (obj^.not_get_proc=0) then
      begin
       dst:=obj^.init_proc_addr;
      end;
    2:if (obj^.not_get_proc=0) then
      begin
       dst:=obj^.fini_proc_addr;
      end;
    3:begin
       dst:=obj^.eh_frame_hdr_addr;
      end;
    4:begin
       dst:=obj^.eh_frame_addr;
      end;
    7:begin
       dst:=obj^.tls_init_addr;
      end;
    8:begin
       dst:=obj^.module_param;
      end;
    else
      begin
       dynlibs_unlock;
       Exit(EINVAL);
      end;
   end;

   Result:=copyout(@dst,pout,SizeOf(Pointer));

   Break;
  end;
  //
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;

 dynlibs_unlock;
end;

function sys_dynlib_get_proc_param(pout:PPointer;psize:PQWORD):Integer;
begin
 if not_dynamic then
 begin
  Writeln(StdErr,'sys_dynlib_get_proc_param:','this is not dynamic linked program.');
  Exit(EPERM);
 end;

 Result:=copyout(@dynlibs_info.proc_param_addr,pout,SizeOf(Pointer));
 if (Result=0) then
 begin
  Result:=copyout(@dynlibs_info.proc_param_size,psize,SizeOf(QWORD));
 end;
end;

function sys_dl_get_info(pid,handle:Integer;pout:PPointer):Integer;
begin
 Exit(EPERM);  //sceSblACMgrIsDebuggerProcess || sceSblACMgrIsCoredumpProcess || sceSblACMgrIsSyscoreProcess
end;

function sys_dl_get_list(pid:Integer;pArray:PInteger;numArray:Integer;pActualNum:PInteger):Integer;
begin
 Exit(EPERM);  //sceSblACMgrIsDebuggerProcess || sceSblACMgrIsCoredumpProcess || sceSblACMgrIsSyscoreProcess
end;

function sys_dl_get_metadata(pid,handle:Integer;pout:Pointer;size:Integer;pactual_size:PInteger):Integer;
begin
 //sce_comment_addr
 //sce_comment_size
 Exit(EPERM);  //sceSblACMgrIsDebuggerProcess || sceSblACMgrIsCoredumpProcess || sceSblACMgrIsSyscoreProcess
end;


end.





