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

function  cpu_thread_alloc():p_kthread;
function  cpu_thread_free(td:p_kthread):Integer;
function  cpu_thread_finished(td:p_kthread):Boolean;
function  cpuset_setaffinity(td:p_kthread;new:Ptruint):Integer;
procedure cpu_set_user_tls(td:p_kthread;base:Pointer);
function  cpu_set_priority(td:p_kthread;prio:Integer):Integer;
function  cpu_getstack(td:p_kthread):QWORD; inline;

implementation

uses
 ucontext,
 kern_umtx;

function cpu_thread_alloc():p_kthread;
var
 td:p_kthread;
 data:Pointer;
 size:ULONG_PTR;
 R:DWORD;
begin
 Result:=nil;

 data:=nil;
 size:=SYS_STACK_RSRV;

 R:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @data,
     0,
     @size,
     MEM_RESERVE,
     PAGE_READWRITE
    );
 if (R<>0) then Exit;

 //header
 size:=SizeOf(kthread)+SizeOf(trapframe)+SizeOf(umtx_q);
 size:=System.Align(size,4*1024);

 R:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @data,
     0,
     @size,
     MEM_COMMIT,
     PAGE_READWRITE
    );
 if (R<>0) then Exit;

 td:=data;
 td^.td_frame:=Pointer(td+1);
 td^.td_umtxq:=Pointer(td^.td_frame+1);

 //footer
 data:=data+SYS_STACK_RSRV-SYS_STACK_SIZE;
 size:=SYS_STACK_SIZE;

 R:=NtAllocateVirtualMemory(
     NtCurrentProcess,
     @data,
     0,
     @size,
     MEM_COMMIT,
     PAGE_READWRITE
    );
 if (R<>0) then Exit;

 td^.td_ksttop:=data;

 data:=data+SYS_STACK_SIZE;
 td^.td_kstack:=data;

 Result:=td;
end;

function cpu_thread_free(td:p_kthread):Integer;
var
 data:Pointer;
 size:ULONG_PTR;
begin
 data:=td;
 size:=0;

 Result:=NtFreeVirtualMemory(
           NtCurrentProcess,
           @data,
           @size,
           MEM_RELEASE
          );
end;

function cpu_thread_finished(td:p_kthread):Boolean;
var
 R:DWORD;
 T:QWORD;
begin
 Result:=True;
 if (td=nil) then Exit;
 if (td^.td_handle=0) or (td^.td_handle=THandle(-1)) then Exit;

 T:=0;
 R:=NtWaitForSingleObject(td^.td_handle,False,@T);

 Result:=(R=STATUS_WAIT_0);

 if Result then
 begin
  NtClose(td^.td_handle);
  td^.td_handle:=0;
 end;
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

