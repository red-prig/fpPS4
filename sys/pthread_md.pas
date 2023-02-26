unit pthread_md;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_thread;

const
 KSE_STACKSIZE=16384;
 DTV_OFFSET   =8;

type
 p_kcb=^kcb;
 p_tcb=^tcb;

 kcb=packed record
  kcb_curtcb:p_tcb;
  kcb_self  :p_kcb;
  kcb_kse   :Pointer; //kse
  //kcb_kmbx:kse_mailbox;
 end;

 tcb=packed record
  tcb_self  :Pointer;
  tcb_dtv   :Pointer;
  tcb_thread:Pointer;
  tcb_spare :Pointer;
  //tcb_tmbx:kse_thr_mailbox
 end;

function _get_curthread:Pointer;

implementation

function _kcb_curtcb:p_tcb; assembler; nostackframe;
asm
 movqq %gs:teb.tcb,Result
end;

function _get_curthread:Pointer; inline;
var
 tcb:p_tcb;
begin
 tcb:=_kcb_curtcb;
 if (tcb<>nil) then
 begin
  Result:=tcb^.tcb_thread;
 end else
 begin
  Result:=nil;
 end;
end;



end.

