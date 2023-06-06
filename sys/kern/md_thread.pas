unit md_thread;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 windows,
 kern_thr;

Const
 SYS_STACK_RSRV=64*1024;
 SYS_STACK_SIZE=16*1024;

function  cpu_thread_alloc(td:p_kthread):Integer;
function  cpu_thread_free(td:p_kthread):Integer;
function  cpuset_setaffinity(td:p_kthread;new:Ptruint):Integer;
procedure cpu_set_user_tls(td:p_kthread;base:Pointer);
function  cpu_set_priority(td:p_kthread;prio:Integer):Integer;
function  cpu_getstack(td:p_kthread):QWORD; inline;

implementation

function cpu_thread_alloc(td:p_kthread):Integer;
var
 data:Pointer;
 size:ULONG_PTR;
begin
 data:=nil;
 size:=SYS_STACK_RSRV;

 Result:=NtAllocateVirtualMemory(
           NtCurrentProcess,
           @data,
           0,
           @size,
           MEM_RESERVE,
           PAGE_READWRITE
          );
 if (Result<>0) then Exit;

 data:=data+SYS_STACK_RSRV-SYS_STACK_SIZE;
 size:=SYS_STACK_SIZE;

 Result:=NtAllocateVirtualMemory(
           NtCurrentProcess,
           @data,
           0,
           @size,
           MEM_COMMIT,
           PAGE_READWRITE
          );

 td^.td_ksttop:=data;

 data:=data+SYS_STACK_SIZE;
 td^.td_kstack:=data;
end;

function cpu_thread_free(td:p_kthread):Integer;
var
 data:Pointer;
 size:ULONG_PTR;
begin
 data:=td^.td_kstack;
 data:=data-SYS_STACK_RSRV;
 size:=0;

 Result:=NtFreeVirtualMemory(
           NtCurrentProcess,
           @data,
           @size,
           MEM_RELEASE
          );
end;

function cpuset_setaffinity(td:p_kthread;new:Ptruint):Integer;
begin
 td^.td_cpuset:=new;
 Result:=NtSetInformationThread(td^.td_handle,ThreadAffinityMask,@new,SizeOf(Ptruint));
end;

procedure cpu_set_user_tls(td:p_kthread;base:Pointer); inline;
begin
 td^.pcb_fsbase:=base;
 td^.td_teb^.tcb:=base;
end;

function cpu_set_priority(td:p_kthread;prio:Integer):Integer;
begin
 td^.td_priority:=prio;

 Case prio of
    0..255:prio:= 16;
  256..496:prio:= 2;
  497..526:prio:= 1;
  527..556:prio:= 0;
  557..586:prio:=-1;
  587..767:prio:=-2;
  else
           prio:=-16;
 end;

 Result:=NtSetInformationThread(td^.td_handle,ThreadBasePriority,@prio,SizeOf(Integer));
end;

function cpu_getstack(td:p_kthread):QWORD; inline;
begin
 Result:=td^.td_frame^.tf_rsp;
end;



end.

