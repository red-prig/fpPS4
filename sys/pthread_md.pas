unit pthread_md;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 thr,
 kern_thread,
 thr_private;

const
 DTV_OFFSET=8;

type
 p_tcb=^tcb;
 tcb=packed record
  tcb_self  :p_tcb;
  tcb_dtv   :Pointer;
  tcb_thread:p_pthread;
  tcb_spare :Pointer;
 end;

procedure _tcb_set(tcb:p_tcb);
function  _tcb_get:p_tcb;
function  _get_curthread:p_pthread;

function  _tcb_ctor(thread:Pointer;initial:Integer):p_tcb;
procedure _tcb_dtor(tcb:p_tcb);

implementation

function TCB_GET64:p_tcb; assembler; nostackframe;
asm
 movqq %gs:teb.tcb,Result
end;

procedure _tcb_set(tcb:p_tcb); inline;
begin
 amd64_set_fsbase(tcb);
end;

function _tcb_get:p_tcb; inline;
begin
 Result:=TCB_GET64;
end;

function _get_curthread:p_pthread; inline;
begin
 Result:=TCB_GET64^.tcb_thread;
end;

function _tcb_ctor(thread:Pointer;initial:Integer):p_tcb;
begin
 if (initial<>0) then
 begin
  Result:=TCB_GET64;
 end else
 begin
  //Result:=_rtld_allocate_tls(nil,sizeof(tcb),16);
 end;
 if (Result<>nil) then
 begin
  Result^.tcb_thread:=thread;
 end;
end;

procedure _tcb_dtor(tcb:p_tcb);
begin
 //_rtld_free_tls(tcb,sizeof(tcb),16);
end;


end.

