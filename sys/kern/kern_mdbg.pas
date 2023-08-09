unit kern_mdbg;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_mdbg_service(op:Integer;arg1,arg2:Pointer):Integer;

implementation

uses
 errno,
 systm,
 trap;

type
 t_SetProcessProperty=packed record //0x48
  f_0:Pointer;
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

  else
    begin
     print_backtrace_c(stderr);
     Assert(False);
     Result:=EINVAL;
    end;
 end;

end;



end.

