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

Procedure md_halt(errnum:DWORD); noreturn;

implementation

uses
 md_thread;

function cpuset_setproc(new:Ptruint):Integer;
var
 i:Integer;
 mask:QWORD;
begin
 if (new=0) then Exit(-1);

 //remap
 mask:=0;
 for i:=0 to 7 do
 if (new and (1 shl i))<>0 then
 begin
  mask:=mask or (1 shl cpuid_g2h[i]);
 end;

 Result:=NtSetInformationProcess(NtCurrentProcess,
                                 ProcessAffinityMask,
                                 @mask,
                                 SizeOf(QWORD));
end;

function cpuset_getproc(var old:Ptruint):Integer;
var
 pbi:PROCESS_BASIC_INFORMATION;
begin
 Result:=NtQueryInformationProcess(NtCurrentProcess,
                                   ProcessBasicInformation,
                                   @pbi,
                                   SizeOf(PROCESS_BASIC_INFORMATION),
                                   nil);
 if (Result=0) then
 begin
  old:=pbi.AffinityMask;
 end;
end;

function get_proc_prio():Integer;
var
 pclass:PROCESS_PRIORITY_CLASS;
begin
 Result:=NtQueryInformationProcess(NtCurrentProcess,
                                   ProcessPriorityClass,
                                   @pclass,
                                   SizeOf(PROCESS_PRIORITY_CLASS),
                                   nil);
 if (Result=0) then
 begin
  Result:=0;

  case pclass.PriorityClass of
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
 pclass:PROCESS_PRIORITY_CLASS;
begin
 pclass.Foreground   :=False;
 pclass.PriorityClass:=PROCESS_PRIORITY_CLASS_NORMAL;

 case n of
  -20..-14:pclass.PriorityClass:=PROCESS_PRIORITY_CLASS_IDLE;
  -13.. -7:pclass.PriorityClass:=PROCESS_PRIORITY_CLASS_BELOW_NORMAL;
   -6..  6:pclass.PriorityClass:=PROCESS_PRIORITY_CLASS_NORMAL;
    7.. 13:pclass.PriorityClass:=PROCESS_PRIORITY_CLASS_ABOVE_NORMAL;
   14.. 20:pclass.PriorityClass:=PROCESS_PRIORITY_CLASS_HIGH;
  else;
 end;

 Result:=NtSetInformationProcess(NtCurrentProcess,
                                 ProcessPriorityClass,
                                 @pclass,
                                 SizeOf(PROCESS_PRIORITY_CLASS));
end;

Procedure md_halt(errnum:DWORD); noreturn;
begin
 NtTerminateProcess(NtCurrentProcess, errnum);
end;


end.

