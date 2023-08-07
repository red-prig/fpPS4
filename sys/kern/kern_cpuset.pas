unit kern_cpuset;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 p_cpuset_t=^cpuset_t;
 cpuset_t  =array[0..0] of QWORD;

const
 CPU_LEVEL_ROOT   =1; // All system cpus.
 CPU_LEVEL_CPUSET =2; // Available cpus for which.
 CPU_LEVEL_WHICH  =3; // Actual mask/id for which.

 CPU_WHICH_TID   =1; // Specifies a thread id.
 CPU_WHICH_PID   =2; // Specifies a process id.
 CPU_WHICH_CPUSET=3; // Specifies a set id.
 CPU_WHICH_IRQ   =4; // Specifies an irq #.
 CPU_WHICH_JAIL  =5; // Specifies a jail id.

function sys_cpuset_getaffinity(level,which,id:Integer;cpusetsize:QWORD;mask:Pointer):Integer;
function sys_cpuset_setaffinity(level,which,id:Integer;cpusetsize:QWORD;mask:Pointer):Integer;

function sys_cpuset(setid:PInteger):Integer;
function sys_cpuset_setid(which,id,setid:Integer):Integer;
function sys_cpuset_getid(level,which,id:Integer;setid:PInteger):Integer;

implementation

uses
 errno,
 systm,
 kern_thr,
 kern_thread,
 md_thread,
 md_proc;

function sys_cpuset_getaffinity(level,which,id:Integer;cpusetsize:QWORD;mask:Pointer):Integer;
var
 td:p_kthread;
 old:QWORD;
begin
 if (cpusetsize<SizeOf(cpuset_t)) then Exit(ERANGE);

 case level of
  CPU_LEVEL_ROOT  :;
  CPU_LEVEL_CPUSET:;
  CPU_LEVEL_WHICH :;
  else
   Exit(EINVAL);
 end;

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
  CPU_WHICH_CPUSET,
  CPU_WHICH_JAIL:
    begin
     if (id=-1) or (id=0) then
     begin
      td:=curkthread;
      old:=td^.td_cpuset;
     end else
     begin
      Exit(ESRCH);
     end;
    end;
  CPU_WHICH_IRQ:
    begin
     Exit(ESRCH);
    end
  else
    Exit(EINVAL);
 end;

 Result:=copyout(@old,mask,SizeOf(QWORD));
end;

function sys_cpuset_setaffinity(level,which,id:Integer;cpusetsize:QWORD;mask:Pointer):Integer;
var
 td:p_kthread;
 new:QWORD;
begin
 if (cpusetsize<SizeOf(cpuset_t)) then Exit(ERANGE);

 case level of
  CPU_LEVEL_ROOT  :;
  CPU_LEVEL_CPUSET:;
  CPU_LEVEL_WHICH :;
  else
   Exit(EINVAL);
 end;

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

     thread_lock(td);
     Result:=cpuset_setaffinity(td,new);
     thread_unlock(td);

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
    end;
  CPU_WHICH_CPUSET,
  CPU_WHICH_JAIL:
    begin
     if (id=-1) or (id=0) then
     begin
      td:=curkthread;

      thread_lock(td);
      Result:=cpuset_setaffinity(td,new);
      thread_unlock(td);

      if (Result<>0) then Result:=ESRCH;
     end else
     begin
      Exit(ESRCH);
     end;
    end;
  CPU_WHICH_IRQ:
    begin
     Exit(ESRCH);
    end
  else
    Exit(EINVAL);
 end;

end;

function sys_cpuset(setid:PInteger):Integer;
begin
 Exit(EPERM); //sceSblACMgrIsSystemUcred
end;

function sys_cpuset_setid(which,id,setid:Integer):Integer;
begin
 Exit(EPERM); //sceSblACMgrIsSystemUcred
end;

function sys_cpuset_getid(level,which,id:Integer;setid:PInteger):Integer;
var
 td:p_kthread;
begin
 if (level=CPU_LEVEL_WHICH) and (which<>CPU_WHICH_CPUSET) then
  Exit(EINVAL);

 case level of
  CPU_LEVEL_ROOT  :;
  CPU_LEVEL_CPUSET:;
  CPU_LEVEL_WHICH :;
  else
   Exit(EINVAL);
 end;

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

     thread_dec_ref(td);
    end;
  CPU_WHICH_PID:
    begin
     if (id=-1) or (id=g_pid) then
     begin
      //
     end else
     begin
      Exit(ESRCH);
     end;
    end;
  CPU_WHICH_CPUSET,
  CPU_WHICH_JAIL:
    begin
     if (id=-1) or (id=0) then
     begin
      //
     end else
     begin
      Exit(ESRCH);
     end;
    end;
  else
    Exit(EINVAL);
 end;

 id:=0; //psevdo cpuset id
 Result:=copyout(@id, setid, sizeof(id));
end;




end.

