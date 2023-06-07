unit kern_context;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ucontext;

function sys_getcontext(ucp:p_ucontext_t):Integer;
function sys_setcontext(ucp:p_ucontext_t):Integer;
function sys_swapcontext(oucp,ucp:p_ucontext_t):Integer;

implementation

uses
 errno,
 systm,
 kern_thr,
 signal,
 kern_sig,
 machdep;

{
 * The first two fields of a ucontext_t are the signal mask and the machine
 * context.  The next field is uc_link; we want to avoid destroying the link
 * when copying out contexts.
 }
const
 UC_COPY_SIZE=ptrint(@ucontext_t(nil^).uc_link);
 {$IF UC_COPY_SIZE<>1216}{$STOP UC_COPY_SIZE<>1216}{$ENDIF}

function sys_getcontext(ucp:p_ucontext_t):Integer;
var
 td:p_kthread;
 uc:ucontext_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (ucp=nil) then Exit(EINVAL);

 get_mcontext(td, @uc.uc_mcontext, GET_MC_CLEAR_RET);

 PROC_LOCK();
 uc.uc_sigmask:=td^.td_sigmask;
 PROC_UNLOCK();

 bzero(@uc.__spare, sizeof(uc.__spare));
 Result:=copyout(@uc, ucp, UC_COPY_SIZE);
end;

function sys_setcontext(ucp:p_ucontext_t):Integer;
var
 td:p_kthread;
 uc:ucontext_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (ucp=nil) then Exit(EINVAL);

 Result:=copyin(ucp, @uc, UC_COPY_SIZE);
 if (Result=0) then
 begin
  Result:=set_mcontext(td, @uc.uc_mcontext);
  if (Result=0) then
  begin
   kern_sigprocmask(td, SIG_SETMASK, @uc.uc_sigmask, nil, 0);
  end;
 end;

 if (Result=0) then Exit(EJUSTRETURN);
end;

function sys_swapcontext(oucp,ucp:p_ucontext_t):Integer;
var
 td:p_kthread;
 uc:ucontext_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (oucp=nil) or (ucp=nil) then Exit(EINVAL);

 get_mcontext(td, @uc.uc_mcontext, GET_MC_CLEAR_RET);
 bzero(@uc.__spare, sizeof(uc.__spare));

 PROC_LOCK();
 uc.uc_sigmask:=td^.td_sigmask;
 PROC_UNLOCK();

 Result:=copyout(@uc, oucp, UC_COPY_SIZE);
 if (Result=0) then
 begin
  Result:=copyin(ucp, @uc, UC_COPY_SIZE);
  if (Result=0) then
  begin
   Result:=set_mcontext(td, @uc.uc_mcontext);
   if (Result=0) then
   begin
    kern_sigprocmask(td, SIG_SETMASK, @uc.uc_sigmask, nil, 0);
   end;
  end;
 end;

 if (Result=0) then Exit(EJUSTRETURN);
end;



end.

