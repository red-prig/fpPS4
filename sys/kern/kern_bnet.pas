unit kern_bnet;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_netcontrol  (fd,op:Integer;buf:Pointer;nbuf:DWORD):Integer;
function sys_netgetiflist(param1:Pointer;param2,param3:Integer):Integer;

implementation

uses
 errno,
 systm,
 md_arc4random;

function bnet_get_secure_seed():Integer;
begin
 Result:=0;
 arc4rand(@Result,SizeOf(Integer),0);
end;

type
 p_bnet_stat_info=^t_bnet_stat_info;
 t_bnet_stat_info=packed record
  kernel_mem_free_min :Integer;
  kernel_mem_free_size:Integer;
  reserved:QWORD;
 end;

function sys_netcontrol(fd,op:Integer;buf:Pointer;nbuf:DWORD):Integer;
var
 _kbuf:array[0..159] of Byte;
 kaddr:Pointer;
begin
 Result:=0;

 if (buf=nil) then
 begin
  kaddr:=nil;
 end else
 begin
  if (160 < nbuf) then
  begin
   Exit(EINVAL);
  end else
  begin
   kaddr:=@_kbuf;
  end;

  if ((op and QWORD($30000000))<>0) then
  begin
   Result:=copyin(buf,kaddr,nbuf);
   if (Result<>0) then Exit;
  end;
 end;

 case op of
    1: //sceNetGetStatisticsInfo
      begin
       if (kaddr=nil) or (nbuf<16) then
       begin
        Exit(EINVAL);
       end;
       //
       p_bnet_stat_info(kaddr)^:=Default(t_bnet_stat_info);
      end;

  $14: //bnet_get_secure_seed
      begin
       if (kaddr=nil) or (nbuf<4) then
       begin
        Exit(EINVAL);
       end;
       //
       PInteger(kaddr)^:=bnet_get_secure_seed();
      end

  else
   begin
    Writeln(StdErr,'Unhandled netcontrol:',op);
    Assert(False);
    Result:=EINVAL;
   end;
 end;

 if (buf<>nil) and (Result=0) and ((op and QWORD($30000000))<>QWORD($20000000)) then
 begin
  Result:=copyout(kaddr,buf,nbuf);
 end;
end;

function sys_netgetiflist(param1:Pointer;param2,param3:Integer):Integer;
begin
 Writeln('TODO:sys_netgetiflist(0x',HexStr(QWORD(param1),11),',',param2,',',param3,')');
 Result:=0;
end;


end.

