unit kern_bnet;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_netcontrol(fd,op:Integer;buf:Pointer;nbuf:DWORD):Integer;

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
  $14: //bnet_get_secure_seed
      begin
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




end.

