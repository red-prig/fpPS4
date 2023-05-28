unit kern_cpuset;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 p_cpuset_t=^cpuset_t;
 cpuset_t  =array[0..1] of QWORD;

const
 CPU_LEVEL_WHICH =3; // Actual mask/id for which.

 CPU_WHICH_TID   =1; // Specifies a thread id.
 CPU_WHICH_PID   =2; // Specifies a process id.

function sys_cpuset_getaffinity(level,which,id:Integer;cpusetsize:QWORD;mask:p_cpuset_t):Integer;
function sys_cpuset_setaffinity(level,which,id:Integer;cpusetsize:QWORD;mask:p_cpuset_t):Integer;

implementation

uses
 errno,
 systm,
 kern_thr,
 kern_thread,
 md_thread,
 md_proc;

function sys_cpuset_getaffinity(level,which,id:Integer;cpusetsize:QWORD;mask:p_cpuset_t):Integer;
var
 td:p_kthread;
 old:QWORD;
begin
 if (cpusetsize<SizeOf(cpuset_t)) then Exit(ERANGE);

 if (level<>CPU_LEVEL_WHICH) then Exit(EINVAL);

 Case which of
  CPU_WHICH_TID:
    begin
     if (id=-1) then
     begin
      td:=curkthread;
      thread_inc_ref(td);
     end else
     begin
      td:=tdfind(id);
     end;

     if (td=nil) then Exit(ESRCH);

     old:=td^.td_cpuset;

     thread_dec_ref(td);
    end;
  CPU_WHICH_PID:
    begin
     if (id=-1) or (id=g_pid) then
     begin
      Result:=cpuset_getproc(old);
      if (Result<>0) then Exit(ESRCH);
     end else
     begin
      Exit(ESRCH);
     end;
    end;
  else
    Exit(EINVAL);
 end;

 Result:=copyout(@old,mask,SizeOf(QWORD));
end;

function sys_cpuset_setaffinity(level,which,id:Integer;cpusetsize:QWORD;mask:p_cpuset_t):Integer;
var
 td:p_kthread;
 new:QWORD;
begin
 if (cpusetsize<SizeOf(cpuset_t)) then Exit(ERANGE);

 if (level<>CPU_LEVEL_WHICH) then Exit(EINVAL);

 Result:=copyin(mask,@new,SizeOf(QWORD));
 if (Result<>0) then Exit;

 Case which of
  CPU_WHICH_TID:
    begin
     if (id=-1) then
     begin
      td:=curkthread;
      thread_inc_ref(td);
     end else
     begin
      td:=tdfind(id);
     end;

     if (td=nil) then Exit(ESRCH);

     Result:=cpuset_setaffinity(td,new);
     if (Result<>0) then Result:=ESRCH;

     thread_dec_ref(td);
    end;
  CPU_WHICH_PID:
    begin
     begin
      if (id=-1) or (id=g_pid) then
      begin
       Result:=cpuset_setproc(new);
       if (Result<>0) then Result:=ESRCH;
      end else
      begin
       Exit(ESRCH);
      end;
     end;
    end
  else
    Exit(EINVAL);
 end;

end;

end.

