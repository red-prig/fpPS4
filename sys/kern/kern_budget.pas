unit kern_budget;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_budget_create(name:pchar;ptype:DWORD;unk_ptr1:Pointer;unk_count:DWORD;unk_ptr2:Pointer):Integer;
function sys_budget_delete(key:Integer):Integer;
function sys_budget_get(key:Integer;ptr:Pointer;psize:PInteger):Integer;
function sys_budget_set(key:Integer):Integer;
function sys_budget_get_ptype(pid:Integer):Integer;
function sys_budget_get_ptype_of_budget(key:Integer):Integer;
function sys_budget_getid():Integer;

function sys_get_proc_type_info(dst:Pointer):Integer;

implementation

uses
 errno,
 systm,
 kern_thr,
 md_proc,
 kern_rtld;

function sys_budget_create(name:pchar;ptype:DWORD;unk_ptr1:Pointer;unk_count:DWORD;unk_ptr2:Pointer):Integer;
begin
 //name  != null
 //ptype -> [0..3]
 //count -> [0..11]
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_delete(key:Integer):Integer;
begin
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_get(key:Integer;ptr:Pointer;psize:PInteger):Integer;
begin
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_set(key:Integer):Integer;
begin
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_get_ptype(pid:Integer):Integer;
var
 td:p_kthread;
begin
 //sceKernelGetProcessType

 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (pid<>-1) and
    (pid<>g_pid) then
 begin
  Exit(ENOSYS);
 end;

 td^.td_retval[0]:=1; //ptype
 Result:=0;
end;

function sys_budget_get_ptype_of_budget(key:Integer):Integer;
begin
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_getid():Integer;
begin
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

type
 p_proc_type_info=^t_proc_type_info;
 t_proc_type_info=packed record
  size  :QWORD;
  bptype:DWORD;
  pflags:DWORD;
 end;
 {$IF sizeof(t_proc_type_info)<>16}{$STOP sizeof(t_proc_type_info)<>16}{$ENDIF}

function sys_get_proc_type_info(dst:Pointer):Integer;
var
 info:t_proc_type_info;
begin
 info:=Default(t_proc_type_info);

 Result:=copyin(dst,@info.size,SizeOf(QWORD));
 if (Result<>0) then Exit;

 if (Result<>SizeOf(t_proc_type_info)) then Exit(EINVAL);

 info.bptype:=budget_ptype_caller;
 info.pflags:=0;

 //sceSblACMgrIsJitCompilerProcess()         -> | 0x01
 //sceSblACMgrIsJitApplicationProcess()      -> | 0x02
 //sceSblACMgrIsVideoplayerProcess()         -> | 0x04
 //sceSblACMgrIsDiskplayeruiProcess()        -> | 0x08
 //sceSblACMgrHasUseVideoServiceCapability() -> | 0x10
 //sceSblACMgrIsWebcoreProcess()             -> | 0x20
 //is_libkernel_sys()                        -> | 0x40
 //sceSblACMgrHasSceProgramAttribute()       -> | 0x80

 Result:=copyout(@info,dst,SizeOf(t_proc_type_info));
end;


end.



