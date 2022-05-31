unit Half16;

{$mode objfpc}{$H+}

interface

type
 PHalf16=^THalf16;
 THalf16=bitpacked record
  man:0..1023;
  exp:0..31;
  sgn:0..1;
 end;

operator := (i:THalf16):Single;
operator := (i:Single):THalf16;

implementation

operator := (i:THalf16):Single;
var
 t1,t2,t3:DWORD;
begin
 t1:=Word(i) and $7fff; // Non-sign bits
 t2:=Word(i) and $8000; // Sign bit
 t3:=Word(i) and $7c00; // Exponent

 t1:=t1 shl 13; // Align mantissa on MSB
 t2:=t2 shl 16; // Shift sign bit into position

 t1:=t1+$38000000; // Adjust bias

 if (t3=0) then t1:=0; // Denormals-as-zero

 t1:=t1 or t2; // Re-insert sign bit

 PDWORD(@Result)^:=t1;
end;

operator := (i:Single):THalf16;
var
 t1,t2,t3:DWORD;
begin
 t1:=PDWORD(@i)^ and $7fffffff; // Non-sign bits
 t2:=PDWORD(@i)^ and $80000000; // Sign bit
 t3:=PDWORD(@i)^ and $7f800000; // Exponent

 t1:=t1 shr 13; // Align mantissa on MSB
 t2:=t2 shr 16; // Shift sign bit into position

 t1:=t1-$1c000; // Adjust bias

 if (t3<$38800000) then t1:=0;     // Flush-to-zero
 if (t3>$47000000) then t1:=$7bff; // Clamp-to-max

 t1:=t1 or t2; // Re-insert sign bit

 Word(Result):=Word(t1);
end;

end.

