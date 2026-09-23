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
 md_thread,
 kern_proc;

function cpuset_setproc(new:Ptruint):Integer;
var
 i:Integer;
 data:array[0..SizeOf(Ptruint)-1+7] of Byte;
 mask:QWORD;
 p_mask:PQWORD;
begin
 if (new=0) then Exit(-1);

 //remap
 mask:=0;
 for i:=0 to 7 do
 if (new and (1 shl i))<>0 then
 begin
  mask:=mask or (1 shl cpuid_g2h[i]);
 end;

 p_mask:=Align(@data,8);
 p_mask^:=mask;

 Result:=NtSetInformationProcess(NtCurrentProcess,
                                 ProcessAffinityMask,
                                 p_mask,
                                 SizeOf(QWORD));
end;

function cpuset_getproc(var old:Ptruint):Integer;
var
 data:array[0..SizeOf(PROCESS_BASIC_INFORMATION)-1+7] of Byte;
 p_info:PPROCESS_BASIC_INFORMATION;
begin
 p_info:=Align(@data,8);

 Result:=NtQueryInformationProcess(NtCurrentProcess,
                                   ProcessBasicInformation,
                                   p_info,
                                   SizeOf(PROCESS_BASIC_INFORMATION),
                                   nil);
 if (Result=0) then
 begin
  old:=p_info^.AffinityMask;
 end;
end;

function get_proc_prio():Integer;
var
 data:array[0..SizeOf(PROCESS_PRIORITY_CLASS)-1+7] of Byte;
 p_info:PPROCESS_PRIORITY_CLASS;
begin
 p_info:=Align(@data,8);

 Result:=NtQueryInformationProcess(NtCurrentProcess,
                                   ProcessPriorityClass,
                                   p_info,
                                   SizeOf(PROCESS_PRIORITY_CLASS),
                                   nil);
 if (Result=0) then
 begin
  Result:=0;

  case p_info^.PriorityClass of
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
 data:array[0..SizeOf(PROCESS_PRIORITY_CLASS)-1+7] of Byte;
 p_info:PPROCESS_PRIORITY_CLASS;
begin
 p_info:=Align(@data,8);

 p_info^.Foreground   :=False;
 p_info^.PriorityClass:=PROCESS_PRIORITY_CLASS_NORMAL;

 case n of
  -20..-14:p_info^.PriorityClass:=PROCESS_PRIORITY_CLASS_IDLE;
  -13.. -7:p_info^.PriorityClass:=PROCESS_PRIORITY_CLASS_BELOW_NORMAL;
   -6..  6:p_info^.PriorityClass:=PROCESS_PRIORITY_CLASS_NORMAL;
    7.. 13:p_info^.PriorityClass:=PROCESS_PRIORITY_CLASS_ABOVE_NORMAL;
   14.. 20:p_info^.PriorityClass:=PROCESS_PRIORITY_CLASS_HIGH;
  else;
 end;

 Result:=NtSetInformationProcess(NtCurrentProcess,
                                 ProcessPriorityClass,
                                 p_info,
                                 SizeOf(PROCESS_PRIORITY_CLASS));
end;

Procedure md_halt(errnum:DWORD); noreturn;
begin
 NtTerminateProcess(NtCurrentProcess, errnum);
end;


end.

