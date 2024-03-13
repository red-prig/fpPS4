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
 md_sleep,
 subr_backtrace;

type
 t_SetProcessProperty=packed record //0x48
  f_0:QWORD;
  f_1:Pointer;
  f_2:Pointer;
  f_3:Pointer;
  f_4:Pointer;
  name:array[0..31] of AnsiChar;
 end;

function SetProcessProperty(arg1:Pointer):Integer;
var
 data:t_SetProcessProperty;
begin
 Result:=copyin(arg1,@data,SizeOf(t_SetProcessProperty));
 if (Result<>0) then Exit;

 case String(data.name) of
  'Sce.PSM:AppName':;        //1
  'Sce.PSM:SdbInfo':;        //2
  'Sce.PSM:LogArea':;        //3
  'Sce.Debug:UserObjArray':; //4
  'Sce.Debug:Gnm':;          //5
  else
   Exit(EINVAL);
 end;

 Writeln('SetProcessProperty("',data.name,'",0x',
                                HexStr(data.f_1),',0x',
                                HexStr(data.f_2),',0x',
                                HexStr(data.f_3),',0x',
                                HexStr(data.f_4),')');

end;

function sys_mdbg_service(op:Integer;arg1,arg2:Pointer):Integer;
begin
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
     Writeln('sceKernelDebugRaiseExceptionOnReleaseMode:0x',HexStr(DWORD(arg1),8));
     print_backtrace_td(stderr);
     Result:=0;
    end;

  8:
    begin
     //init_mdbg_pevt(proc);
     //td->td_dbgflags = td->td_dbgflags | 0x100;

     //signal thread start and wait
     msleep_td(0);
    end;

  10: //sceCoredumpAttach*
    begin
     //(td->td_dbgflags | 0x100)=0 && not sceSblACMgrIsSystemUcred
     Result:=EPERM;
    end;

  11:
    begin
     //init_mdbg_pevt
     Result:=0;
    end;

  12:
    begin
     //wait thread start
     Result:=0;
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

