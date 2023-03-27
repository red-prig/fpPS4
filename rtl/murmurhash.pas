unit murmurhash;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

Function MurmurHash64A(key:Pointer;len,seed:QWORD):QWORD;

implementation

Function MurmurHash64A(key:Pointer;len,seed:QWORD):QWORD;
const
 m:QWORD=QWORD($c6a4a7935bd1e995);
 r=47;
var
 h,k:QWORD;
 data:PQWORD;
 _end:PQWORD;
 data2:PByte;
begin
 h:=seed xor (len*m);

 data:=key;
 _end:=data+(len shr 3);

 while (data<>_end) do
 begin
  k:=data^;
  Inc(data);

  k:=k*m;
  k:=k xor (k shr r);
  k:=k*m;

  h:=h xor k;
  h:=h*m;
 end;

 data2:=Pointer(data);

 len:=(len and 7);

 if (len<>0) then
 begin
  while (len<>0) do
  begin
   case len of
    7: h:=h xor (QWORD(data2[6]) shl 48);
    6: h:=h xor (QWORD(data2[5]) shl 40);
    5: h:=h xor (QWORD(data2[4]) shl 32);
    4: h:=h xor (QWORD(data2[3]) shl 24);
    3: h:=h xor (QWORD(data2[2]) shl 16);
    2: h:=h xor (QWORD(data2[1]) shl  8);
    1: h:=h xor (QWORD(data2[0]));
   end;
   Dec(len);
  end;
  h:=h*m;
 end;

 h:=h xor (h shr r);
 h:=h*m;
 h:=h xor (h shr r);

 Result:=h;
end;

end.

