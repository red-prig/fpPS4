unit kern_mdbg;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_mdbg_service(op:Integer;arg1,arg2:Pointer):Integer;

implementation

uses
 sysutils,
 errno,
 systm,
 vuio,
 md_tty,
 sys_tty,
 kern_mtx,
 kern_condvar,
 kern_thr,
 kern_proc,
 subr_backtrace;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

type
 t_SetProcessProperty=packed record //0x48
  f_0:QWORD;
  f_1:QWORD;
  f_2:QWORD;
  f_3:QWORD;
  f_4:QWORD;
  name:array[0..31] of AnsiChar;
 end;

 t_coredump_ret=packed record //0x28
  ret  :Integer;
  flags:Integer;
  data :array[0..3] of QWORD;
 end;

 t_pevt_state=(pevt_none,pevt_inited,pevt_wait,pevt_finish);

 p_mdbg_pevt=^t_mdbg_pevt;
 t_mdbg_pevt=record
  cv :t_cv;
  mtx:mtx;

  td_tid     :Integer;
  td_cancel  :Integer;
  td_ret     :Integer;
  td_flags   :Integer;
  td_state   :t_pevt_state;
  crash_state:Integer;

  //
  Sce:record
   Flags             :QWORD;
   PSM_AppName       :array[0..3] of QWORD;
   PSM_SdbInfo       :array[0..1] of QWORD;
   PSM_LogArea       :array[0..3] of QWORD;
   Debug_UserObjArray:array[0..1] of QWORD;
   Debug_Gnm         :array[0..1] of QWORD;
  end;
  //

 end;

var
 g_mdbg_pevt:p_mdbg_pevt=nil;

function init_mdbg_pevt:p_mdbg_pevt;
var
 props:p_mdbg_pevt;
begin
 mtx_lock(p_proc.p_mtx);

  props:=g_mdbg_pevt;

  if (props<>nil) then
  begin
   mtx_unlock(p_proc.p_mtx);
   Exit(props);
  end;

  props:=AllocMem(SizeOf(t_mdbg_pevt));

  mtx_init(props^.mtx,'mdbg_pevt_MTX');
  cv_init (@props^.cv,'mDBG Debug event');

  props^.td_state:=pevt_inited;

  g_mdbg_pevt:=props;

 mtx_unlock(p_proc.p_mtx);

 LOG_INFO('init_mdbg_pevt');

 Exit(props);
end;

function SetProcessProperty(arg1:Pointer):Integer;
var
 data :t_SetProcessProperty;
 props:p_mdbg_pevt;
begin
 Result:=copyin(arg1,@data,SizeOf(t_SetProcessProperty));
 if (Result<>0) then Exit;

 props:=init_mdbg_pevt;

 Result:=0;

 mtx_lock(p_proc.p_mtx);

 case String(data.name) of
  'Sce.PSM:AppName':        //1
   begin
    props^.Sce.Flags         :=props^.Sce.Flags or $02;
    props^.Sce.PSM_AppName[0]:=data.f_1;
    props^.Sce.PSM_AppName[1]:=data.f_2;
    props^.Sce.PSM_AppName[2]:=data.f_3;
    props^.Sce.PSM_AppName[3]:=data.f_4;
   end;
  'Sce.PSM:SdbInfo':        //2
   begin
    props^.Sce.Flags         :=props^.Sce.Flags or $04;
    props^.Sce.PSM_SdbInfo[0]:=data.f_1;
    props^.Sce.PSM_SdbInfo[1]:=data.f_2;
   end;
  'Sce.PSM:LogArea':        //3
   begin
    props^.Sce.Flags         :=props^.Sce.Flags or $08;
    props^.Sce.PSM_LogArea[0]:=data.f_1;
    props^.Sce.PSM_LogArea[1]:=data.f_2;
    props^.Sce.PSM_LogArea[2]:=data.f_3;
    props^.Sce.PSM_LogArea[3]:=data.f_4;
   end;
  'Sce.Debug:UserObjArray': //4
   begin
    props^.Sce.Flags                :=props^.Sce.Flags or $10;
    props^.Sce.Debug_UserObjArray[0]:=data.f_1;
    props^.Sce.Debug_UserObjArray[1]:=data.f_2;
   end;
  'Sce.Debug:Gnm':          //5
   begin
    props^.Sce.Flags       :=props^.Sce.Flags or $20;
    props^.Sce.Debug_Gnm[0]:=data.f_1;
    props^.Sce.Debug_Gnm[1]:=data.f_2;
   end;
  else
   Result:=EINVAL;
 end;

 mtx_unlock(p_proc.p_mtx);

 if (Result<>0) then Exit;

 LOG_INFO('SetProcessProperty("',data.name,'",0x',
                                HexStr(data.f_1,16),',0x',
                                HexStr(data.f_2,16),',0x',
                                HexStr(data.f_3,16),',0x',
                                HexStr(data.f_4,16),')');

end;

procedure mdbg_out(p:PChar);
var
 buf :PChar;
 len :ptruint;
 auio:t_uio;
 aiov:iovec;
begin
 buf:=AllocMem($1000);
 len:=0;
 if copyinstr(p,buf,$1000,@len)=0 then
 begin
  aiov.iov_base  :=buf;
  aiov.iov_len   :=len;
  auio.uio_iov   :=@aiov;
  auio.uio_iovcnt:=1;
  auio.uio_offset:=0;
  auio.uio_resid :=len;
  auio.uio_segflg:=UIO_SYSSPACE;
  auio.uio_rw    :=UIO_WRITE;
  //
  md_tty_write(@debug_tty,debug_tty.t_priv,@auio,0);
 end;
 FreeMem(buf);
end;

function sys_mdbg_service(op:Integer;arg1,arg2:Pointer):Integer;
var
 props:p_mdbg_pevt;
 ret,size:Integer;

 coredump_ret:t_coredump_ret;
begin
 Result:=0;

 case op of

  1: //SetProcessProperty
    begin
     Result:=SetProcessProperty(arg1);
    end;

  3: //sceKernelDebugRaiseException
    begin
     print_error_td('sceKernelDebugRaiseException:0x'+HexStr(DWORD(arg1),8));
     Assert(False);
     Result:=EINVAL;
    end;

  4: //sceKernelDebugRaiseExceptionOnReleaseMode
    begin
     LOG_ERROR('sceKernelDebugRaiseExceptionOnReleaseMode:0x',HexStr(DWORD(arg1),8));
     print_backtrace_td(stderr);
     Result:=0;
    end;

  7: //sceKernelDebugOutText
    begin
     mdbg_out(arg1);
    end;

  8:
    begin
     //signal coredump thread start and wait

     props:=init_mdbg_pevt;
     //td->td_dbgflags = td->td_dbgflags | 0x100;

     mtx_lock(props^.mtx);

      props^.td_tid:=curkthread^.td_tid;

      Result:=0;

      if (props^.td_cancel=0) then
      begin
       props^.td_state:=pevt_wait;
       cv_signal(@props^.cv);
       ret:=_cv_wait_sig(@props^.cv,@props^.mtx);
       Result:=0;
       props^.td_state:=pevt_inited;
       if (ret<>0) and (props^.td_cancel=0) then
       begin
        Result:=EINTR;
       end;
      end;

      props^.td_cancel:=0;

      if (props^.crash_state=0) then
      begin
       //
      end else
      begin
       Result:=EINVAL;
      end;

      if (Result=0) then
      begin
       coredump_ret:=Default(t_coredump_ret);
       coredump_ret.ret  :=props^.td_ret;
       coredump_ret.flags:=props^.td_flags;

       mtx_unlock(props^.mtx);

        size:=Integer(arg2);

        if (size < 0) then
        begin
         Result:=EFAULT;
        end else
        begin

         if (DWORD(size) < $28) then
         begin
          //
         end else
         begin
          size:=$28;
         end;

         Result:=copyout(@coredump_ret,arg1,size);

         if (Result=0) then
         begin
          LOG_INFO('mDBG: Debughandler starting(',curkthread^.td_tid,':',curkthread^.td_name,')');
         end;

        end;

        Exit;
      end;

     mtx_unlock(props^.mtx);
    end;

  9: //sceCoredumpUnregisterCoredumpHandler
    begin
     //cancel coredump thread and wait exit

     mtx_lock(p_proc.p_mtx);

      props:=g_mdbg_pevt;

      if (props<>nil) then
      begin
       mtx_lock(props^.mtx);

        props^.td_flags :=props^.td_flags or 1;
        props^.td_ret   :=Integer(arg1);
        props^.td_cancel:=1;

        if (props^.td_state=pevt_wait) then
        begin
         cv_signal(@props^.cv);
        end;

       mtx_unlock(props^.mtx);

       mtx_unlock(p_proc.p_mtx);

       //TODO: wait thread exit

       Exit;
      end else
      begin
       Result:=ESRCH;
      end;

     mtx_unlock(p_proc.p_mtx);
    end;

  10: //sceCoredumpAttach*
    begin
     //only in coredump thread
     //(td->td_dbgflags | 0x100)=0 && not sceSblACMgrIsSystemUcred
     Result:=EPERM;
    end;

  11:
    begin
     init_mdbg_pevt;
     Result:=0;
    end;

  12:
    begin
     //wait coredump thread start

     mtx_lock(p_proc.p_mtx);
      props:=g_mdbg_pevt;
     mtx_unlock(p_proc.p_mtx);

     if (props=nil) then
     begin
      Exit(EINVAL);
     end;

     mtx_lock(props^.mtx);

     if (props^.td_state=pevt_inited) then
     begin
      ret:=_cv_wait_sig(@props^.cv,@props^.mtx);
      if (ret<>-1) then
      begin
       Result:=ret;
      end else
      begin
       Result:=EINTR;
      end;
     end else
     if (props^.td_state<>pevt_wait) then
     begin
      if (props^.td_state=pevt_finish) then
      begin
       Result:=ESRCH;
      end else
      begin
       Result:=EAGAIN;
      end;
     end;

     mtx_unlock(props^.mtx);
    end;

  13:
    begin
     //coredump thread reset

     mtx_lock(p_proc.p_mtx);

      //if (((td->td_dbgflags & 0x100) != 0) && (proc->debug_event == 0)) {
      //  td->td_dbgflags = td->td_dbgflags & (~0x100);
      //}

      props:=g_mdbg_pevt;

      if (props<>nil) then
      begin
       mtx_lock(props^.mtx);

        props^.td_flags:=props^.td_flags and (not 1);

        if (arg1<>nil) then
        begin
         props^.td_state:=pevt_finish;
        end;

       mtx_unlock(props^.mtx);
      end;

     mtx_unlock(p_proc.p_mtx);
    end;

  20:
    begin
     //td->td_dbgflags = td->td_dbgflags | 0x800;
     Result:=0;
    end;

  else
    begin
     print_error_td('sys_mdbg_service('+IntToStr(op)+')');
     Assert(False);
     Result:=EINVAL;
    end;
 end;

end;



end.

