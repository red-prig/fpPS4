unit md_sleep;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 windows,
 kern_thr;

function  _umtxq_alloc:THandle;
procedure _umtxq_free(h:THandle);
function  msleep_umtxq(h:THandle;timo:Int64):Integer; inline;
function  wakeup_umtxq(h:THandle):Integer; inline;

function  msleep_td(timo:Int64):Integer; inline;
function  wakeup_td(td:p_kthread):Integer; inline;
procedure md_yield; inline;

implementation

uses
 errno,
 trap;

function ntw2px(n:Integer):Integer; inline;
begin
 Case DWORD(n) of
  STATUS_SUCCESS         :Result:=0;
  STATUS_ABANDONED       :Result:=EPERM;
  STATUS_ALERTED         :Result:=EINTR;
  STATUS_USER_APC        :Result:=EINTR;
  STATUS_TIMEOUT         :Result:=ETIMEDOUT;
  STATUS_ACCESS_VIOLATION:Result:=EFAULT;
  else
                          Result:=EINVAL;
 end;
end;

function ntd2px(n:Integer):Integer; inline;
begin
 Case DWORD(n) of
  STATUS_SUCCESS         :Result:=ETIMEDOUT;
  STATUS_ABANDONED       :Result:=EPERM;
  STATUS_ALERTED         :Result:=EINTR;
  STATUS_USER_APC        :Result:=EINTR;
  STATUS_ACCESS_VIOLATION:Result:=EFAULT;
  else
                          Result:=EINVAL;
 end;
end;

function _umtxq_alloc:THandle;
var
 n:Integer;
begin
 Result:=0;
 n:=NtCreateEvent(@Result,EVENT_ALL_ACCESS,nil,SynchronizationEvent,False);
 Assert(n=0);
end;

procedure _umtxq_free(h:THandle);
begin
 NtClose(h);
end;

function msleep_umtxq(h:THandle;timo:Int64):Integer; inline;
begin
 if (timo=0) then
 begin
  timo:=NT_INFINITE;
 end else
 begin
  timo:=-timo;
 end;
 sig_sta;
 Result:=ntw2px(NtWaitForSingleObject(h,True,@timo));
 sig_cla;
end;

function wakeup_umtxq(h:THandle):Integer; inline;
begin
 Result:=ntw2px(NtSetEvent(h,nil));
end;

function msleep_td(timo:Int64):Integer; inline;
begin
 if (timo=0) then
 begin
  timo:=NT_INFINITE;
 end else
 begin
  timo:=-timo;
 end;
 sig_sta;
 Result:=ntd2px(NtDelayExecution(True,@timo));
 sig_cla;
end;

procedure _apc_null(dwParam:PTRUINT); stdcall;
begin
end;

function wakeup_td(td:p_kthread):Integer; inline;
begin
 Result:=ntw2px(NtQueueApcThread(td^.td_handle,@_apc_null,nil,nil,0));
end;

procedure md_yield; inline;
begin
 NtYieldExecution;
end;

end.



