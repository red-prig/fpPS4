unit kern_regmgr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_regmgr_call(op,key:DWORD;presult,pvalue:Pointer;vlen:QWORD):Integer;

implementation

uses
 errno,
 systm;

type
 t_encoded_id=packed record
  data    :array[0..3] of Byte;
  table   :Byte;
  index   :Byte;
  checksum:Word;
 end;

function sys_regmgr_call(op,key:DWORD;presult,pvalue:Pointer;vlen:QWORD):Integer;
label
 _err;
var
 kret:DWORD;

 data:packed record
  enc :t_encoded_id;
  val1:DWORD;
  val2:DWORD;
 end;

begin
 kret:=0;

 if (presult=nil) then
 begin
  kret:=$800d0202;
  goto _err;
 end;

 case op of
  $19:begin
       Result:=copyin(pvalue,@data,16);
       if (Result<>0) then
       begin
        kret:=$800d020f;
        goto _err;
       end;

       Writeln(' enc:0x',HexStr(qword(data.enc),16),' val1:0x',HexStr(data.val1,8),' val12:0x',HexStr(data.val2,8));

       data.val2:=0;

       Result:=copyout(@data,pvalue,vlen);
       if (Result<>0) then
       begin
        kret:=$800d0210;
        goto _err;
       end;
      end;
  else
      begin
       Writeln('Unhandled regmgr op:0x',HexStr(op,4));
       Assert(False);
      end;
 end;


_err:
 Result:=copyout(@kret,presult,SizeOf(DWORD));
end;



end.

