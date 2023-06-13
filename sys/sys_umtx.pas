unit sys_umtx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 time,
 _umtx;

procedure umtx_init(var umtx:umtx); inline;
function  umtx_owner(var umtx:umtx):QWORD; inline;
function  umtx_lock(var umtx:umtx;id:QWORD):Integer; inline;
function  umtx_trylock(var umtx:umtx;id:QWORD):Integer; inline;
function  umtx_timedlock(var umtx:umtx;id:QWORD;timeout:ptimespec):Integer; inline;
function  umtx_unlock(var umtx:umtx;id:QWORD):Integer; inline;
function  umtx_wait(var umtx:umtx;id:QWORD;timeout:ptimespec):Integer; inline;
function  umtx_wake(var umtx:umtx;nr_wakeup:Integer):Integer; inline;

function  _umtx_op_err(obj:Pointer;op:Integer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;

implementation

uses
 errno,
 syscalls,
 kern_umtx,
 trap,
 thr_error;

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
  Result:=_umtx_lock(@umtx);
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
  Result:=_umtx_op(@umtx,UMTX_OP_LOCK,id,nil,timeout)
 end;
end;

function umtx_unlock(var umtx:umtx;id:QWORD):Integer; inline;
begin
 Result:=0;
 if (System.InterlockedCompareExchange64(umtx.u_owner,UMTX_UNOWNED,id)=0) then
 begin
  Result:=_umtx_unlock(@umtx);
 end;
end;

function umtx_wait(var umtx:umtx;id:QWORD;timeout:ptimespec):Integer; inline;
begin
 Result:=_umtx_op(@umtx,UMTX_OP_WAIT,id,nil,timeout)
end;

function umtx_wake(var umtx:umtx;nr_wakeup:Integer):Integer; inline;
begin
 Result:=_umtx_op(@umtx,UMTX_OP_WAKE,nr_wakeup,nil,nil);
end;

function _umtx_op_err(obj:Pointer;op:Integer;val:QWORD;uaddr1,uaddr2:Pointer):Integer; assembler; nostackframe;
asm
 movq  sys__umtx_op,%rax
 call  fast_syscall
end;

end.

