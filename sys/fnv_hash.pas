unit fnv_hash;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

{
 * Fowler / Noll / Vo Hash (FNV Hash)
 * http://www.isthe.com/chongo/tech/comp/fnv/
 *
 * This is an implementation of the algorithms posted above.
 * This file is placed in the public domain by Peter Wemm.
 *
 * $FreeBSD$
}

type
 Fnv32_t=DWORD;
 Fnv64_t=QWORD;

const
 FNV1_32_INIT:Fnv32_t=Fnv32_t(33554467);
 FNV1_64_INIT:Fnv64_t=Fnv64_t($cbf29ce484222325);

 FNV_32_PRIME:Fnv32_t=Fnv32_t($01000193);
 FNV_64_PRIME:Fnv64_t=Fnv64_t($100000001b3);

function fnv_32_str(str:pchar;hval:Fnv32_t):Fnv32_t;

implementation

function fnv_32_str(str:pchar;hval:Fnv32_t):Fnv32_t;
var
 s:PByte;
 c:Fnv32_t;
begin
 s:=PByte(str);

 c:=s^;
 while (c <> 0) do
 begin
  s:=s+1;
  //
  hval:=hval * FNV_32_PRIME;
  hval:=hval xor c;
  //
  c:=s^;
 end;

 Result:=hval;
end;

end.

