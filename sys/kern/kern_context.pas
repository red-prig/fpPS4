unit kern_context;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ucontext;

function sys_getcontext(ucp:Pointer):Integer;
function sys_setcontext(ucp:Pointer):Integer;
function sys_swapcontext(oucp,ucp:Pointer):Integer;

function sys_thr_get_ucontext(tid:Integer;ucp:Pointer):Integer;
function sys_thr_suspend_ucontext(tid:Integer):Integer;
function sys_thr_resume_ucontext(tid:Integer):Integer;

implementation

uses
 errno,
 systm,
 kern_thr,
 kern_proc,
 signal,
 kern_sig,
 kern_synch,
 sched_ule,
 machdep;

{
 * The first two fields of a ucontext_t are the signal mask and the machine
 * context.  The next field is uc_link; we want to avoid destroying the link
 * when copying out contexts.
 }
const
 UC_COPY_SIZE=ptrint(@ucontext_t(nil^).uc_link);
 {$IF UC_COPY_SIZE<>1216}{$STOP UC_COPY_SIZE<>1216}{$ENDIF}

function sys_getcontext(ucp:Pointer):Integer;
var
 td:p_kthread;
 uc:ucontext_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (ucp=nil) then Exit(EINVAL);

 bzero(@uc,SizeOf(ucontext_t));

 get_mcontext(td, @uc.uc_mcontext, GET_MC_CLEAR_RET);

 PROC_LOCK();
 uc.uc_sigmask:=td^.td_sigmask;
 PROC_UNLOCK();

 Result:=copyout(@uc, ucp, UC_COPY_SIZE);
end;

function sys_setcontext(ucp:Pointer):Integer;
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

function sys_swapcontext(oucp,ucp:Pointer):Integer;
var
 td:p_kthread;
 uc:ucontext_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (oucp=nil) or (ucp=nil) then Exit(EINVAL);

 bzero(@uc,SizeOf(ucontext_t));

 get_mcontext(td, @uc.uc_mcontext, GET_MC_CLEAR_RET);

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

function sys_thr_get_ucontext(tid:Integer;ucp:Pointer):Integer;
var
 tdf:p_kthread;
 uc:ucontext_t;
begin
 tdf:=tdfind(tid);

 repeat
  if (tdf=nil) then Exit(ESRCH);

  PROC_LOCK;
  thread_lock(tdf);

  if ((tdf^.td_inhibitors and TDI_SUSP_CTX)=0) and
     ((tdf^.td_flags and TDF_SUSP_CTX)=0) then
  begin
   thread_unlock(tdf);
   thread_dec_ref(tdf);
   PROC_UNLOCK;
   Exit(EINVAL);
  end;

  case tdf^.td_state of
   TDS_INACTIVE :; //try again?
   TDS_INHIBITED:
     begin
      bzero(@uc,SizeOf(ucontext_t));

      get_mcontext2(tdf, @uc.uc_mcontext, 0);

      thread_unlock(tdf);
      thread_dec_ref(tdf);
      PROC_UNLOCK;

      Result:=copyout(@uc,ucp,UC_COPY_SIZE);
      Exit;
     end
   else
     begin
      Writeln('get_ucontext: an illegal state ',tdf^.td_state);
      thread_unlock(tdf);
      thread_dec_ref(tdf);
      PROC_UNLOCK;
      Exit(ESRCH);
     end;
  end;

  thread_unlock(tdf);
  thread_dec_ref(tdf);
  PROC_UNLOCK;

  pause('ususp',1);
  tdf:=tdfind(tid);
 until false;
end;

function sys_thr_suspend_ucontext(tid:Integer):Integer;
label
 _exit;
var
 td,tdf,tdf2:p_kthread;
begin
 Result:=0;

 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (tid=td^.td_tid) then Exit(EDEADLK);

 tdf:=tdfind(tid);

 if (tdf=nil) then Exit(ESRCH);

 PROC_LOCK;
 thread_lock(tdf);

 if ((tdf^.td_inhibitors and TDI_SUSP_CTX)=0) and
    ((tdf^.td_flags and TDF_SUSP_CTX)=0) then
 begin
  if ((tdf^.td_inhibitors and TDI_SLEEPING)=0) or
     ((tdf^.td_flags and TDF_SINTR)=0) then
  begin
   tdf^.td_flags:=tdf^.td_flags or (TDF_ASTPENDING or TDF_SUSP_CTX);

   if TD_IS_RUNNING(tdf) then
   begin
    forward_signal(tdf);
   end;

   repeat
    if (tdf^.td_state<>TDS_RUNNING) and
       ((tdf^.td_inhibitors and TDI_SLEEPING)=0) then
    begin
     Result:=0;
     goto _exit;
    end;

    thread_unlock(tdf);
    thread_dec_ref(tdf);
    PROC_UNLOCK;

    pause('ususp',1);
    pause('ususp',1);

    tdf2:=tdfind(tid);

    if (tdf2=nil) then Exit(ESRCH);

    if (tdf<>tdf2) then
    begin
     thread_dec_ref(tdf2);
     Exit(ESRCH);
    end;

    PROC_LOCK;
    thread_lock(tdf);

   until not (((tdf^.td_inhibitors and TDI_SLEEPING)=0) or
              ((tdf^.td_flags and TDF_SINTR)=0));

   tdf^.td_flags:=tdf^.td_flags and (not (TDF_ASTPENDING or TDF_SUSP_CTX));
   tdf^.td_state:=TDS_INHIBITED;
   tdf^.td_inhibitors:=tdf^.td_inhibitors or TDI_SUSP_CTX;

   Result:=0;
  end else
  begin
   tdf^.td_state:=TDS_INHIBITED;
   tdf^.td_inhibitors:=tdf^.td_inhibitors or TDI_SUSP_CTX;

   Result:=0;
  end;
 end else
 begin
  Result:=EINVAL;
 end;

_exit:
 thread_unlock(tdf);
 thread_dec_ref(tdf);
 PROC_UNLOCK;
end;

function sys_thr_resume_ucontext(tid:Integer):Integer;
label
 _exit;
var
 td,tdf:p_kthread;
begin
 Result:=0;

 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (tid=td^.td_tid) then Exit(EINVAL);

 tdf:=tdfind(tid);

 if (tdf=nil) then Exit(ESRCH);

 PROC_LOCK;
 thread_lock(tdf);

 if ((tdf^.td_inhibitors and TDI_SUSP_CTX)=0) then
 begin
  if ((tdf^.td_flags and TDF_SUSP_CTX)=0) then
  begin
   Result:=EINVAL;
   goto _exit;
  end;
  tdf^.td_flags:=tdf^.td_flags and (not TDF_SUSP_CTX);
 end else
 begin
  if ((tdf^.td_flags and TDF_SUSP_CTX)<>0) then
  begin
   tdf^.td_flags:=tdf^.td_flags and (not TDF_SUSP_CTX);
   Result:=0;
   goto _exit;
  end;

  tdf^.td_inhibitors:=tdf^.td_inhibitors and (not TDI_SUSP_CTX);

  if (tdf^.td_inhibitors=0) then
  begin
   tdf^.td_state:=TDS_CAN_RUN;
  end;

  setrunnable(tdf);
 end;

 Result:=0;

_exit:
 thread_unlock(tdf);
 thread_dec_ref(tdf);
 PROC_UNLOCK;
end;


end.

