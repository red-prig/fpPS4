unit md_arc4random;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

procedure arc4rand(ptr:Pointer;len,reseed:Integer);

implementation

const
 BCRYPT_USE_SYSTEM_PREFERRED_RNG=2;

function  BCryptGenRandom(hAlgorithm:Pointer;
                          pbBuffer:PByte;
                          cbBuffer:DWORD;
                          dwFlags:DWORD):DWORD; stdcall; external 'Bcrypt';

procedure arc4rand(ptr:Pointer;len,reseed:Integer);
begin
 BCryptGenRandom(nil,ptr,len,BCRYPT_USE_SYSTEM_PREFERRED_RNG);
end;


end.

