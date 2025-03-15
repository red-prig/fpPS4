unit srOpInternal;

{$mode ObjFPC}{$H+}

interface

uses
 spirv;

const
 OpIAddExt=DWORD(-1);
 OpISubExt=DWORD(-2);
 OpAbsDiff=DWORD(-3);
 OpWQM32  =DWORD(-4);

 OpBFE_32 =DWORD(-5);
 OpBFIB32 =DWORD(-6);

 OpPackAnc=DWORD(-7);
 OpPackOfs=DWORD(-8);
 OpMakeExp=DWORD(-9);
 OpMakeVec=DWORD(-10);
 OpMakeCub=DWORD(-11);

 OpCUBEID =DWORD(-12);
 OpCUBESC =DWORD(-13);
 OpCUBETC =DWORD(-14);
 OpCUBEMA =DWORD(-15);

function OpGetStrInternal(OpId:DWORD):RawByteString;

implementation

function OpGetStrInternal(OpId:DWORD):RawByteString;
begin
 case OpId of

  OpIAddExt:Result:='[OpIAddExt]';
  OpISubExt:Result:='[OpISubExt]';
  OpAbsDiff:Result:='[OpAbsDiff]';
  OpWQM32  :Result:='[OpWQM32]';

  OpBFE_32 :Result:='[OpBFE_32]';
  OpBFIB32 :Result:='[OpBFIB32]';

  OpPackAnc:Result:='[OpPackAnc]';
  OpPackOfs:Result:='[OpPackOfs]';
  OpMakeExp:Result:='[OpMakeExp]';
  OpMakeVec:Result:='[OpMakeVec]';
  OpMakeCub:Result:='[OpMakeCub]';

  OpCUBEID :Result:='[OpCUBEID]';
  OpCUBESC :Result:='[OpCUBESC]';
  OpCUBETC :Result:='[OpCUBETC]';
  OpCUBEMA :Result:='[OpCUBEMA]';

  else
   Result:=Op.GetStr(OpId);
 end;
end;

end.

