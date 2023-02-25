unit kern_cpuset;

{$mode ObjFPC}{$H+}

interface

uses
 trap;

type
 p_cpuset_t=^cpuset_t;
 cpuset_t  =array[0..1] of QWORD;

const
 CPU_LEVEL_WHICH =3; // Actual mask/id for which.

 CPU_WHICH_TID   =1; // Specifies a thread id.
 CPU_WHICH_PID   =2; // Specifies a process id.

function sys_cpuset_getaffinity(level,which:Integer;id,cpusetsize:QWORD;mask:p_cpuset_t):Integer;
function sys_cpuset_setaffinity(level,which:Integer;id,cpusetsize:QWORD;mask:p_cpuset_t):Integer;

implementation

uses
 systm,
 sys_kernel,
 kern_thread,
 vm_machdep;

function sys_cpuset_getaffinity(level,which:Integer;id,cpusetsize:QWORD;mask:p_cpuset_t):Integer;
var
 td:p_kthread;
begin
 if (cpusetsize<SizeOf(cpuset_t)) then Exit(ERANGE);

 if (level<>CPU_LEVEL_WHICH) then Exit(EINVAL);
 if (level<>CPU_WHICH_TID)   then Exit(EINVAL);

 if (int64(id)=-1) then
 begin
  td:=curkthread;
  thread_inc_ref(td);
 end else
 begin
  td:=tdfind(id);
 end;

 if (td=nil) then Exit(ESRCH);

 Result:=copyout(@td^.td_cpuset,mask,SizeOf(QWORD));
 if (Result<>0) then Result:=EFAULT;

 thread_dec_ref(td);
end;

function sys_cpuset_setaffinity(level,which:Integer;id,cpusetsize:QWORD;mask:p_cpuset_t):Integer;
var
 td:p_kthread;
 new:QWORD;
begin
 if (cpusetsize<SizeOf(cpuset_t)) then Exit(ERANGE);

 if (level<>CPU_LEVEL_WHICH) then Exit(EINVAL);
 if (level<>CPU_WHICH_TID)   then Exit(EINVAL);

 if (int64(id)=-1) then
 begin
  td:=curkthread;
  thread_inc_ref(td);
 end else
 begin
  td:=tdfind(id);
 end;

 if (td=nil) then Exit(ESRCH);

 Result:=copyin(mask,@new,SizeOf(QWORD));

 if (Result<>0) then
 begin
  Result:=EFAULT;
 end else
 begin
  Result:=cpuset_setaffinity(td,new);
  if (Result<>0) then Result:=ESRCH;
 end;

 thread_dec_ref(td);
end;

end.

