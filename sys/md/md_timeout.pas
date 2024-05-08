unit md_timeout;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 //
 LFQueue,
 md_sleep,
 sched_ule,
 kern_synch,
 kern_thr,
 kern_callout;

procedure md_start_softclock();
procedure md_callout_new_inserted(c:p_callout;cc:p_callout_cpu);
procedure md_callout_reset(c:p_callout);
procedure md_callout_stop(c:p_callout);
procedure md_callout_done(c:p_callout);

implementation

uses
 kern_timeout;

var
 timeout_thr:p_kthread=nil;
 timeout_new:TIntrusiveMPSCQueue;

procedure softclock(arg:Pointer); forward;

procedure md_start_softclock();
var
 r:Integer;
begin
 timeout_new.Create;

 r:=kthread_add(@softclock,nil,@timeout_thr,0,'softclock');
 Assert(r=0,'softclock');
end;

procedure wt_event(arg:Pointer;dwTimerLowValue,dwTimerHighValue:DWORD); stdcall;
var
 c:p_callout;
 cc:p_callout_cpu;
begin
 if (arg=nil) then Exit;
 c:=arg;
 cc:=callout_lock();
 softclock_call_cc(c,cc);
 CC_UNLOCK(cc);
end;

Procedure wt_timer_add(c:p_callout;cc:p_callout_cpu);
var
 f:Int64;
 n:Integer;
begin
 f:=-c^.c_time;
 n:=-1;

 if ((c^.c_flags and CALLOUT_PENDING)<>0) then
 begin
  n:=NtSetTimer(c^.c_timer,
                @f,
                @wt_event,
                c,
                false,
                0,
                nil);
 end;

 if (n<>0) then
 begin
  if (cc^.cc_waiting<>0) then
  begin
   cc^.cc_waiting:=0;
   CC_UNLOCK(cc);
   wakeup(@cc^.cc_waiting);
   CC_LOCK(cc);
  end;

  Assert(((c^.c_flags and CALLOUT_LOCAL_ALLOC)=0) or (c^.c_flags=CALLOUT_LOCAL_ALLOC),'corrupted callout');

  if ((c^.c_flags and CALLOUT_LOCAL_ALLOC)<>0) then
  begin
   callout_cc_del(c, cc);
  end;
 end;
end;

procedure md_callout_new_inserted(c:p_callout;cc:p_callout_cpu);
var
 n:Integer;
begin
 if (timeout_thr=curkthread) then
 begin
  wt_timer_add(c,cc);
 end else
 begin
  timeout_new.Push(c);
  n:=wakeup_td(timeout_thr);
  Assert(n=0,'md_callout_new_inserted');
 end;
end;

procedure softclock(arg:Pointer);
var
 c:p_callout;
 cc:p_callout_cpu;
begin
 sched_prio(curkthread,64);
 repeat
  cc:=nil;
  c:=nil;

  while timeout_new.Pop(c) do
  begin
   cc:=callout_lock();
   wt_timer_add(c,cc);
   CC_UNLOCK(cc);
  end;

  msleep_td(0);
 until false;
end;

procedure md_callout_reset(c:p_callout);
var
 n:Integer;
begin
 if (c^.c_timer=0) then
 begin
  n:=NtCreateTimer(
      @c^.c_timer,
      EVENT_ALL_ACCESS,
      nil,
      0);
  Assert(n=0,'NtCreateTimer');
 end else
 begin
  NtCancelTimer(c^.c_timer,nil);
 end;
end;

procedure md_callout_stop(c:p_callout);
begin
 if (c^.c_timer<>0) then
 begin
  NtCancelTimer(c^.c_timer,nil);
 end;
end;

procedure md_callout_done(c:p_callout);
begin
 if (c^.c_timer<>0) then
 begin
  NtClose(c^.c_timer);
  c^.c_timer:=0;
 end;
end;


end.

