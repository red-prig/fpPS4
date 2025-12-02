unit kern_daemon;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 LFQueue,
 systm,
 time,
 kern_thr,
 sched_ule;

type
 t_daemon_cbs=procedure;

 p_daemon_node=^t_daemon_node;
 t_daemon_node=record
  next:p_daemon_node;
  icbs:t_daemon_cbs;
 end;

procedure sys_daemon_init;
procedure sys_daemon_add_cbs(node:p_daemon_node;cbs:t_daemon_cbs);

implementation

var
 daemon_thr:p_kthread;
 daemon_new:TIntrusiveMPSCQueue=(tail_:@daemon_new.stub_;stub_:(next_:nil);head_:@daemon_new.stub_);

procedure sys_daemon_add_cbs(node:p_daemon_node;cbs:t_daemon_cbs);
begin
 if (node=nil) or (cbs=nil) then Exit;
 node^.icbs:=cbs;
 daemon_new.Push(node);
end;

//Daemon for a separate thread
procedure sys_daemon(arg:Pointer);
var
 node:p_daemon_node;
begin
 sched_prio(curkthread,1000);
 repeat

  node:=Pointer(daemon_new.tail_^.next_);
  while (node<>nil) do
  begin
   node^.icbs();
   //
   node:=node^.next;
  end;

  pause('sys_daemon',hz);
 until false;
end;

procedure sys_daemon_init;
var
 n:Integer;
begin
 n:=kthread_add(@sys_daemon,nil,@daemon_thr,0,'sys_daemon');
 Assert(n=0,'sys_daemon');
end;

end.

