unit md_proc;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 windows;

function  cpuset_setproc(new:Ptruint):Integer;
function  cpuset_getproc(var old:Ptruint):Integer;

function  get_proc_prio():Integer;
function  set_proc_prio(n:Integer):Integer;

Procedure md_halt(errnum:TExitCode); noreturn;

implementation

uses
 kern_proc;

function cpuset_setproc(new:Ptruint):Integer;
begin
 Result:=NtSetInformationProcess(NtCurrentProcess,
                                 ProcessAffinityMask,
                                 @new,
                                 SizeOf(QWORD));
end;

function cpuset_getproc(var old:Ptruint):Integer;
var
 info:PROCESS_BASIC_INFORMATION;
begin
 Result:=NtQueryInformationProcess(NtCurrentProcess,
                                   ProcessBasicInformation,
                                   @info,
                                   SizeOf(info),
                                   nil);
 if (Result=0) then
 begin
  old:=info.AffinityMask;
 end;
end;

function get_proc_prio():Integer;
var
 info:PROCESS_PRIORITY_CLASS;
begin
 Result:=NtQueryInformationProcess(NtCurrentProcess,
                                   ProcessPriorityClass,
                                   @info,
                                   SizeOf(info),
                                   nil);
 if (Result=0) then
 begin
  Result:=0;

  case info.PriorityClass of
   PROCESS_PRIORITY_CLASS_IDLE        :Result:=-20;
   PROCESS_PRIORITY_CLASS_BELOW_NORMAL:Result:=-10;
   PROCESS_PRIORITY_CLASS_NORMAL      :Result:=0;
   PROCESS_PRIORITY_CLASS_ABOVE_NORMAL:Result:=10;
   PROCESS_PRIORITY_CLASS_HIGH        :Result:=20;
   else;
  end;

 end else
 begin
  Result:=0;
 end;
end;

function set_proc_prio(n:Integer):Integer;
var
 info:PROCESS_PRIORITY_CLASS;
begin
 info.Foreground   :=False;
 info.PriorityClass:=PROCESS_PRIORITY_CLASS_NORMAL;

 case n of
  -20..-14:info.PriorityClass:=PROCESS_PRIORITY_CLASS_IDLE;
  -13.. -7:info.PriorityClass:=PROCESS_PRIORITY_CLASS_BELOW_NORMAL;
   -6..  6:info.PriorityClass:=PROCESS_PRIORITY_CLASS_NORMAL;
    7.. 13:info.PriorityClass:=PROCESS_PRIORITY_CLASS_ABOVE_NORMAL;
   14.. 20:info.PriorityClass:=PROCESS_PRIORITY_CLASS_HIGH;
  else;
 end;

 Result:=NtSetInformationProcess(NtCurrentProcess,
                                 ProcessPriorityClass,
                                 @info,
                                 SizeOf(info));
end;

Procedure md_halt(errnum:TExitCode); noreturn;
begin
 NtTerminateProcess(NtCurrentProcess, errnum);
end;

initialization
 p_proc.p_pid:=GetCurrentProcessId;

end.

