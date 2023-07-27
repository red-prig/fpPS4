unit kern_authinfo;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_rtld;

var
 g_authinfo:t_authinfo;

function sys_get_authinfo(pid:Integer;info:Pointer):Integer;

implementation

uses
 errno,
 systm,
 md_proc;

function sys_get_authinfo(pid:Integer;info:Pointer):Integer;
var
 data:t_authinfo;
 x,y:QWORD;
begin
 Result:=0;

 if (pid<>0) and
    (pid<>g_pid) then
 begin
  Exit(ESRCH);
 end;

 data:=Default(t_authinfo);

 //if (priv_check(td,$2ae)=0) then
 //begin
 // data:=g_authinfo;
 //end else
 begin
  x:=g_authinfo.app_type_id;
  y:=x+QWORD($c7ffffffeffffffc);

  if (y < $f) and (((QWORD($6001) shr (y and $3f)) and 1)<>0) then
  begin
   data.app_type_id:=x;
  end;

  data.app_flags:=g_authinfo.app_flags and QWORD($7000000000000000);
 end;

 if (info<>nil) then
 begin
  Result:=copyout(@data,info,SizeOf(t_authinfo));
 end;
end;



end.

