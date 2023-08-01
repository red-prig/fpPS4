unit kern_dmem;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_dmem_container(d_pool_id:Integer):Integer;

implementation

uses
 errno,
 kern_thr;

const
 default_pool_id=1;

function sys_dmem_container(d_pool_id:Integer):Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 td^.td_retval[0]:=default_pool_id;
 Result:=0;

 if (d_pool_id<>-1) then
 begin
  //Result:=priv_check(td,0x2ad);
  //(param < 3)
  Exit(EPERM);
 end;
end;



end.

