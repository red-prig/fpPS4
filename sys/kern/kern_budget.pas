unit kern_budget;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 sys_vm_object;

procedure budget_enter_object(obj :vm_object_t;
                              len :vm_ooffset_t);

procedure budget_remove(obj :vm_object_t;
                        len :vm_ooffset_t);

function  get_mlock_avail():QWORD;
function  get_mlock_total():QWORD;

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

var
 budget_total :QWORD=(448*1024*1024);
 budget_malloc:QWORD=0;
 budget_dmem  :QWORD=0;

procedure budget_enter_object(obj :vm_object_t;
                              len :vm_ooffset_t);
label
 _inc_malloc;
begin
 if (obj=nil) then
 begin
  _inc_malloc:
   System.InterlockedExchangeAdd64(budget_malloc,len);
 end else
 if (obj^.otype in [OBJT_DEFAULT,OBJT_SWAP,OBJT_VNODE,OBJT_SELF]) and
    ((obj^.flags and OBJ_DMEM_EXT2)=0) then
 begin
  goto _inc_malloc;
 end;

 if (obj<>nil) then
 if (obj^.otype=OBJT_PHYSHM) then
 begin
  System.InterlockedExchangeAdd64(budget_dmem,len);
 end;
end;

procedure budget_remove(obj :vm_object_t;
                        len :vm_ooffset_t);
label
 _dec_malloc;
begin
 if (obj=nil) then
 begin
  _dec_malloc:
   System.InterlockedExchangeAdd64(budget_malloc,-len);
 end else
 if (obj^.otype in [OBJT_DEFAULT,OBJT_SWAP,OBJT_VNODE,OBJT_SELF]) and
    ((obj^.flags and OBJ_DMEM_EXT2)=0) then
 begin
  goto _dec_malloc;
 end;

 if (obj<>nil) then
 if (obj^.otype=OBJT_PHYSHM) then
 begin
  System.InterlockedExchangeAdd64(budget_dmem,-len);
 end;
end;

function get_mlock_avail():QWORD;
begin
 Result:=0;
 if (budget_total>budget_malloc) then
 begin
  Result:=budget_total-budget_malloc;
 end;
end;

function get_mlock_total():QWORD;
begin
 Result:=budget_total;
end;

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



