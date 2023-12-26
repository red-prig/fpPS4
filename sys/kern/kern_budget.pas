unit kern_budget;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 sys_vm_object;

type
 p_budget_resource=^t_budget_resource;
 t_budget_resource=packed record
  resid:Integer;
  attr :Integer;
  limit:QWORD;   //cpuset
  used :QWORD;
 end;

//budget resid
const
 SCE_KERNEL_BUDGET_MEMORY_INVALID=0;
 SCE_KERNEL_BUDGET_MEMORY_DMEM   =1;
 SCE_KERNEL_BUDGET_MEMORY_VMEM   =2;
 SCE_KERNEL_BUDGET_MEMORY_MLOCK  =3;
 SCE_KERNEL_BUDGET_CPU_SET       =4;
 SCE_KERNEL_BUDGET_FD_FILE       =5;
 SCE_KERNEL_BUDGET_FD_SOCKET     =6;
 SCE_KERNEL_BUDGET_FD_EQUEUE     =7;
 SCE_KERNEL_BUDGET_FD_PIPE       =8;
 SCE_KERNEL_BUDGET_FD_DEVICE     =9;
 SCE_KERNEL_BUDGET_THREADS       =10;
 SCE_KERNEL_BUDGET_FD_IPCSOCKET  =11;

//budget proc_type
 PTYPE_BIG_APP          = 0;
 PTYPE_MINI_APP         = 1;
 PTYPE_SYSTEM           = 2;
 PTYPE_NONGAME_MINI_APP = 3;

procedure budget_reserve(obj :vm_object_t;
                         len :vm_ooffset_t);

procedure budget_release(obj :vm_object_t;
                         len :vm_ooffset_t);

function  get_mlock_avail():QWORD;
function  get_mlock_total():QWORD;

function sys_budget_create(name:pchar;ptype:DWORD;new:Pointer;count:DWORD;prev:Pointer):Integer;
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

procedure budget_reserve(obj :vm_object_t;
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
end;

procedure budget_release(obj :vm_object_t;
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
end;

function get_mlock_avail():QWORD;
var
 m:QWORD;
begin
 Result:=0;
 m:=budget_malloc;
 if (budget_total>m) then
 begin
  Result:=budget_total-m;
 end;
end;

function get_mlock_total():QWORD;
begin
 Result:=budget_total;
end;

function sys_budget_create(name:pchar;ptype:DWORD;new:Pointer;count:DWORD;prev:Pointer):Integer;
begin
 //name  != null
 //ptype -> [0..3]  (proc_type)
 //new   -> p_budget_resource
 //count -> [0..11] (new/prev)
 //prev  -> p_budget_resource
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_delete(key:Integer):Integer;
begin
 //key -> id_table
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_get(key:Integer;ptr:Pointer;psize:PInteger):Integer;
begin
 //key   -> [-2..-5] (budget limits [0..3]) else -> id_table
 //ptr   -> p_budget_resource
 //psize -> in/out size
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_set(key:Integer):Integer;
begin
 //key -> id_table
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

 td^.td_retval[0]:=p_proc.p_budget_ptype;

 Result:=0;
end;

function sys_budget_get_ptype_of_budget(key:Integer):Integer;
begin
 //key -> id_table
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_getid():Integer;
begin
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;


end.



