unit sys_umtx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sys_kernel,
 time,
 _umtx,
 kern_umtx;

procedure umtx_init(var umtx:umtx); inline;
function  umtx_owner(var umtx:umtx):QWORD; inline;
function  umtx_lock(var umtx:umtx;id:QWORD):Integer; inline;
function  umtx_trylock(var umtx:umtx;id:QWORD):Integer; inline;
function  umtx_timedlock(var umtx:umtx;id:QWORD;timeout:ptimespec):Integer; inline;
function  umtx_unlock(var umtx:umtx;id:QWORD):Integer; inline;
function  umtx_wait(var umtx:umtx;id:QWORD;timeout:ptimespec):Integer; inline;
function  umtx_wake(var umtx:umtx;nr_wakeup:Integer):Integer; inline;

function  sys_umtx_op(obj:Pointer;op:Integer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;

implementation

procedure umtx_init(var umtx:umtx); inline;
begin
 umtx.u_owner:=UMTX_UNOWNED;
end;

function umtx_owner(var umtx:umtx):QWORD; inline;
begin
 Result:=umtx.u_owner and (not UMTX_CONTESTED)
end;

function umtx_lock(var umtx:umtx;id:QWORD):Integer; inline;
begin
 Result:=0;
 if (System.InterlockedCompareExchange64(umtx.u_owner,id,UMTX_UNOWNED)=0) then
 begin
  Result:=_sys_umtx_lock(@umtx);
 end;
end;

function umtx_trylock(var umtx:umtx;id:QWORD):Integer; inline;
begin
 Result:=0;
 if (System.InterlockedCompareExchange64(umtx.u_owner,id,UMTX_UNOWNED)=0) then
 begin
  Result:=EBUSY;
 end;
end;

function umtx_timedlock(var umtx:umtx;id:QWORD;timeout:ptimespec):Integer; inline;
begin
 Result:=0;
 if (System.InterlockedCompareExchange64(umtx.u_owner,id,UMTX_UNOWNED)=0) then
 begin
  Result:=_sys_umtx_op(@umtx,UMTX_OP_LOCK,id,nil,timeout)
 end;
end;

function umtx_unlock(var umtx:umtx;id:QWORD):Integer; inline;
begin
 Result:=0;
 if (System.InterlockedCompareExchange64(umtx.u_owner,UMTX_UNOWNED,id)=0) then
 begin
  Result:=_sys_umtx_unlock(@umtx);
 end;
end;

function umtx_wait(var umtx:umtx;id:QWORD;timeout:ptimespec):Integer; inline;
begin
 Result:=_sys_umtx_op(@umtx,UMTX_OP_WAIT,id,nil,timeout)
end;

function umtx_wake(var umtx:umtx;nr_wakeup:Integer):Integer; inline;
begin
 Result:=_sys_umtx_op(@umtx,UMTX_OP_WAKE,nr_wakeup,nil,nil);
end;

function sys_umtx_op(obj:Pointer;op:Integer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
begin
 Result:=_sys_umtx_op(obj,op,val,uaddr1,uaddr2);
end;

end.

