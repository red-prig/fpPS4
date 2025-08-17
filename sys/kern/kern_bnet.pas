unit kern_bnet;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_netcontrol  (fd,op:Integer;buf:Pointer;nbuf:DWORD):Integer;
function sys_netgetiflist(buf:Pointer;max,unused:Integer):Integer;

implementation

uses
 errno,
 systm,
 kern_thr,
 subr_backtrace,
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

function sys_netgetiflist(buf:Pointer;max,unused:Integer):Integer;
var
 info:array[0..$1e0-1] of Byte;
 td:p_kthread;
begin
 Writeln('TODO:sys_netgetiflist(0x',HexStr(QWORD(buf),11),',',max,')');
 print_backtrace_td(StdErr);

 FillChar(info,sizeof(info),0);
 copyout(@info,buf,sizeof(info));

 td:=curkthread;
 td^.td_retval[0]:=1;

 Result:=0;
end;


end.

