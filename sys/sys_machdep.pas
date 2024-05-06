unit sys_machdep;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 I386_GET_IOPERM    =  3;
 I386_SET_IOPERM    =  4;
 I386_GET_FSBASE    =  7;
 I386_SET_FSBASE    =  8;
 I386_GET_GSBASE    =  9;
 I386_SET_GSBASE    = 10;

 AMD64_GET_FSBASE   =128;
 AMD64_SET_FSBASE   =129;
 AMD64_GET_GSBASE   =130;
 AMD64_SET_GSBASE   =131;

type
 p_i386_ioperm_args=^t_i386_ioperm_args;
 t_i386_ioperm_args=packed record
  start :DWORD;
  length:DWORD;
  enable:Integer;
 end;

function sys_sysarch(op:Integer;parms:Pointer):Integer;

implementation

uses
 errno,
 systm,
 vmparam,
 kern_thread;

function amd64_get_ioperm(uap:p_i386_ioperm_args):Integer;
begin
 if (uap^.start >= (IOPAGES * PAGE_SIZE * NBBY)) then
  Exit(EINVAL);

 uap^.length:=0;
 Exit(0);
end;

function amd64_set_ioperm(uap:p_i386_ioperm_args):Integer;
begin
 Exit(EPERM); //priv_check(td, PRIV_IO)
end;

function sys_sysarch(op:Integer;parms:Pointer):Integer;
var
 i386base:DWORD;
 a64base :QWORD;
 iargs   :t_i386_ioperm_args;
begin
 Result:=0;
 i386base:=0;
 a64base :=0;

 //read
 case op of
  I386_GET_IOPERM,
  I386_SET_IOPERM:
    begin
     Result:=copyin(parms, @iargs, sizeof(t_i386_ioperm_args));
    end;
  I386_SET_FSBASE,
  I386_SET_GSBASE:
    begin
     Result:=copyin(parms, @i386base, sizeof(i386base));
    end;
  AMD64_SET_FSBASE,
  AMD64_SET_GSBASE:
    begin
     Result:=copyin(parms, @a64base, sizeof(a64base));
    end;
  else;
 end;

 if (Result<>0) then Exit;

 //operation
 case op of
  I386_GET_IOPERM:
    begin
     Result:=amd64_get_ioperm(@iargs);
    end;
  I386_SET_IOPERM:
    begin
     Result:=amd64_set_ioperm(@iargs);
    end;
  I386_GET_FSBASE:
    begin
     Result:=sys_amd64_get_fsbase(@a64base);
     i386base:=a64base;
    end;
  I386_SET_FSBASE:
    begin
     a64base:=i386base;
     Result:=sys_amd64_set_fsbase(Pointer(a64base));
    end;
  I386_GET_GSBASE:
    begin
     Result:=sys_amd64_get_gsbase(@a64base);
     i386base:=a64base;
    end;
  I386_SET_GSBASE:
    begin
     a64base:=i386base;
     Result:=sys_amd64_set_gsbase(Pointer(a64base));
    end;
  AMD64_GET_FSBASE:
    begin
     Result:=sys_amd64_get_fsbase(@a64base);
    end;
  AMD64_SET_FSBASE:
    begin
     Result:=sys_amd64_set_fsbase(Pointer(a64base));
     Writeln('set_fsbase=0x',HexStr(a64base,16));
    end;
  AMD64_GET_GSBASE:
    begin
     Result:=sys_amd64_get_gsbase(@a64base);
    end;
  AMD64_SET_GSBASE:
    begin
     Result:=sys_amd64_set_gsbase(Pointer(a64base));
    end;
  else
    Exit(EINVAL);
 end;

 if (Result<>0) then Exit;

 //write
 case op of
  I386_GET_IOPERM:
    begin
     Result:=copyout(@iargs, parms, sizeof(t_i386_ioperm_args));
    end;
  I386_GET_FSBASE,
  I386_GET_GSBASE:
    begin
     Result:=copyout(@i386base, parms, sizeof(i386base));
    end;
  AMD64_GET_FSBASE,
  AMD64_GET_GSBASE:
    begin
     Result:=copyout(@a64base, parms, sizeof(a64base));
    end;
  else;
 end;

end;

end.

