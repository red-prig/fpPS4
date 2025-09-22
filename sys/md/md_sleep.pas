unit md_sleep;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 windows,
 kern_thr;

function  msleep_td(timo:Int64):Integer;
function  wakeup_td(td:p_kthread):Integer;
procedure md_reset_wakeup;
procedure md_yield;

implementation

uses
 errno;

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

function msleep_td(timo:Int64):Integer;
begin
 if (timo=0) then
 begin
  timo:=NT_INFINITE;
 end else
 begin
  timo:=-timo;
 end;
 Result:=ntd2px(NtDelayExecution(True,@timo));
end;

procedure _apc_null(dwParam:PTRUINT); stdcall;
begin
end;

function wakeup_td(td:p_kthread):Integer;
begin
 Result:=ntw2px(NtQueueApcThread(td^.td_handle,@_apc_null,nil,nil,0));
end;

procedure md_reset_wakeup;
begin
 NtTestAlert();
end;

procedure md_yield;
var
 timo:Int64;
begin
 timo:=-100*10;
 NtDelayExecution(True,@timo);

 //NtYieldExecution;
end;

end.



