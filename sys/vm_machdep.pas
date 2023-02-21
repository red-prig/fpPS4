unit vm_machdep;

{$mode ObjFPC}{$H+}

interface

uses
 ntapi,
 windows,
 md_psl,
 sys_kernel,
 kern_thread;

function  cpu_thread_alloc(td:p_kthread):Integer;
function  cpu_thread_free(td:p_kthread):Integer;
procedure cpu_set_syscall_retval(td:p_kthread;error:Integer);
function  cpuset_setaffinity(td:p_kthread;new:Ptruint):Integer;
procedure cpu_set_user_tls(td:p_kthread;base:Pointer);
function  cpu_set_priority(td:p_kthread;prio:Integer):Integer;
function  cpu_getstack(td:p_kthread):QWORD;

implementation

function cpu_thread_alloc(td:p_kthread):Integer;
var
 data:Pointer;
 size:ULONG_PTR;
begin
 data:=nil;
 size:=16*1024;

 Result:=NtAllocateVirtualMemory(
           NtCurrentProcess,
           @data,
           0,
           @size,
           MEM_COMMIT or MEM_RESERVE,
           PAGE_READWRITE
          );

 data:=data+16*1024;
 td^.td_kstack:=data;
end;

function cpu_thread_free(td:p_kthread):Integer;
var
 data:Pointer;
 size:ULONG_PTR;
begin
 data:=td^.td_kstack;
 data:=data-16*1024;
 size:=0;

 Result:=NtFreeVirtualMemory(
           NtCurrentProcess,
           @data,
           @size,
           MEM_RELEASE
          );
end;

procedure cpu_set_syscall_retval(td:p_kthread;error:Integer);
begin
 Case error of
  0:With td^.td_frame^ do
    begin
     tf_rax:=td^.td_retval[0];
     tf_rdx:=td^.td_retval[1];
     tf_rflags:=tf_rflags and (not PSL_C);
    end;
  ERESTART:
    With td^.td_frame^ do
    begin
     //tf_err = size of syscall cmd
     tf_rip:=tf_rip-td^.td_frame^.tf_err;
     tf_r10:=tf_rcx;
     //set_pcb_flags(td->td_pcb, PCB_FULL_IRET);
    end;
  EJUSTRETURN:;
  else
    With td^.td_frame^ do
    begin
     tf_rax:=error;
     tf_rflags:=tf_rflags or PSL_C;
    end;
 end;
end;

function cpuset_setaffinity(td:p_kthread;new:Ptruint):Integer;
begin
 td^.td_cpuset:=new;
 Result:=NtSetInformationThread(td^.td_handle,ThreadAffinityMask,@new,SizeOf(Ptruint));
end;

procedure cpu_set_user_tls(td:p_kthread;base:Pointer);
var
 ptls:PPointer;
begin
 ptls:=td^.td_teb+$708;

 ptls^:=base;
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

function cpu_getstack(td:p_kthread):QWORD;
begin
 Result:=td^.td_frame^.tf_rsp;
end;


end.

