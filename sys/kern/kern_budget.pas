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

implementation

uses
 errno,
 kern_thr,
 kern_proc;

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
    (pid<>p_proc.p_pid) then
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


end.



