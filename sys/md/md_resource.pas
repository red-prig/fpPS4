unit md_resource;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 _resource;

function kern_getrusage(who:Integer;rup:p_rusage):Integer;

implementation

uses
 ntapi,
 errno,
 time,
 md_proc,
 md_time,
 kern_thr,
 vmparam,
 vm_map;

function pgtok(x:QWORD):QWORD; inline;
begin
 Result:=x*(PAGE_SIZE div 1024)
end;

function kern_getrusage(who:Integer;rup:p_rusage):Integer;
var
 td:p_kthread;

 user,syst:Int64;

 IO:IO_COUNTERS;
 VM:VM_COUNTERS;
 R:DWORD;
begin
 Result:=0;

 rup^:=Default(t_rusage);

 user:=0;
 syst:=0;

 Case who of
  RUSAGE_SELF:
    begin
     rup^.ru_nsignals:=p_proc.p_nsignals;
     rup^.ru_nvcsw   :=p_proc.p_nvcsw;
     rup^.ru_nivcsw  :=p_proc.p_nivcsw;

     rup^.ru_ixrss   :=pgtok(g_vmspace.vm_tsize);
     rup^.ru_idrss   :=pgtok(g_vmspace.vm_dsize);
     rup^.ru_isrss   :=pgtok(g_vmspace.vm_ssize);

     IO:=Default(IO_COUNTERS);
     R:=NtQueryInformationProcess(NtCurrentProcess,ProcessIoCounters,@IO,SizeOf(IO),nil);

     if (R=0) then
     begin
      rup^.ru_inblock:=IO.ReadOperationCount;
      rup^.ru_oublock:=IO.WriteOperationCount;
     end;

     VM:=Default(VM_COUNTERS);
     R:=NtQueryInformationProcess(NtCurrentProcess,ProcessVmCounters,@VM,SizeOf(VM),nil);

     if (R=0) then
     begin
      rup^.ru_maxrss:=VM.PeakWorkingSetSize div 1024;
      rup^.ru_majflt:=VM.PageFaultCount;
     end;

     calcru_proc(@user,@syst);
    end;
  RUSAGE_THREAD:
    begin
     td:=curkthread;
     if (td=nil) then Exit(-1);

     rup^.ru_nsignals:=td^.td_ru.ru_nsignals;
     rup^.ru_nvcsw   :=td^.td_ru.ru_nvcsw;
     rup^.ru_nivcsw  :=td^.td_ru.ru_nivcsw;

     rup^.ru_inblock :=td^.td_ru.ru_inblock;
     rup^.ru_oublock :=td^.td_ru.ru_oublock;

     calcru_thread(@user,@syst);
    end;
   RUSAGE_CHILDREN:
    begin
     Exit(0);
    end
  else;
    Exit(EINVAL);
 end;

 UNIT_TO_TIMEVAL(@rup^.ru_utime,user);
 UNIT_TO_TIMEVAL(@rup^.ru_stime,syst);

 //ru_minflt
 //ru_nswap
 //ru_msgsnd
 //ru_msgrcv
end;

end.

