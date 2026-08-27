unit md_arc4random;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

procedure arc4rand(ptr:Pointer;len,reseed:Integer);
function  arc4random():DWORD;

implementation

type
 t_BCryptGenRandom=function(hAlgorithm:Pointer;
                            pbBuffer:PByte;
                            cbBuffer:DWORD;
                            dwFlags:DWORD):DWORD; stdcall;

var
 Bcrypt:THandle=NilHandle;
 BCryptGenRandom:t_BCryptGenRandom=nil;

const
 BCRYPT_USE_SYSTEM_PREFERRED_RNG=2;

procedure arc4rand(ptr:Pointer;len,reseed:Integer);
begin
 if (BCryptGenRandom=nil) then
 begin
  Bcrypt:=LoadLibrary('Bcrypt');
  Pointer(BCryptGenRandom):=GetProcedureAddress(Bcrypt,'BCryptGenRandom');
  if (BCryptGenRandom=nil) then
  begin
   Ptruint(BCryptGenRandom):=1;
  end;
 end;

 if (Ptruint(BCryptGenRandom)<>1) then
 begin
  BCryptGenRandom(nil,ptr,len,BCRYPT_USE_SYSTEM_PREFERRED_RNG);
 end;
end;

function arc4random():DWORD;
begin
 arc4rand(@Result, sizeof(Result), 0);
end;


end.

